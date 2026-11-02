use crate::{text, Decimal, HResult, Locale, VarType, Variant};

const OVERFLOW: HResult = HResult::DISP_E_OVERFLOW;
const MISMATCH: HResult = HResult::DISP_E_TYPEMISMATCH;

pub(crate) fn change_type(
    src: &Variant,
    to: VarType,
    locale: Locale,
    flags: u16,
) -> Result<Variant, HResult> {
    match (src, to) {
        _ if src.vt() == to as u16 => Ok(src.clone()),
        (Variant::Bstr(s), _) => text::from_bstr(s, to, locale),
        (Variant::BstrNull, _) => text::from_bstr("", to, locale),
        (_, VarType::Bstr) => text::to_bstr(src, locale, flags).map(Variant::Bstr),
        (Variant::Dispatch(Some(_)) | Variant::Unknown(Some(_)), _) => Err(HResult::E_NOTIMPL),
        _ => numeric(src, to),
    }
}

#[derive(Clone, Copy)]
enum Num {
    Int(i64),
    Bool(bool),
    R4(f32),

    F64(f64),
    Cy(i64),
    Dec(Decimal),
}

impl Num {
    fn of(v: &Variant) -> Option<Num> {
        Some(match v {
            Variant::Empty => Num::Int(0),
            Variant::I2(x) => Num::Int(i64::from(*x)),
            Variant::I4(x) => Num::Int(i64::from(*x)),
            Variant::Ui1(x) => Num::Int(i64::from(*x)),
            Variant::I8(x) => Num::Int(*x),
            Variant::Bool(b) => Num::Bool(*b),
            Variant::R4(x) => Num::R4(*x),
            Variant::R8(x) | Variant::Date(x) => Num::F64(*x),
            Variant::Cy(x) => Num::Cy(*x),
            Variant::Decimal(d) => Num::Dec(*d),
            _ => return None,
        })
    }
}

fn numeric(src: &Variant, to: VarType) -> Result<Variant, HResult> {
    let Some(n) = Num::of(src) else {
        let pointer = matches!(src, Variant::Dispatch(None) | Variant::Unknown(None));
        return match to {
            VarType::Empty | VarType::Null if pointer => Ok(empty_or_null(to)),
            VarType::Dispatch if pointer => Ok(Variant::Dispatch(None)),
            VarType::Unknown if pointer => Ok(Variant::Unknown(None)),
            _ => Err(MISMATCH),
        };
    };
    match to {
        VarType::Empty | VarType::Null => Ok(empty_or_null(to)),
        VarType::R8 => to_r8(n).map(Variant::R8),
        VarType::I4 => to_i4(n).map(Variant::I4),
        VarType::Bool => to_bool(n).map(Variant::Bool),
        VarType::Cy => to_cy(n).map(Variant::Cy),
        VarType::Date => to_date(n).map(Variant::Date),
        VarType::I2 => {
            to_small_int(n, i64::from(i16::MIN), i64::from(i16::MAX)).map(|v| Variant::I2(v as i16))
        }
        VarType::Ui1 => to_ui1(n).map(Variant::Ui1),
        VarType::I8 => to_i8(n).map(Variant::I8),
        VarType::R4 => to_r4(n).map(Variant::R4),
        VarType::Decimal => to_decimal(n).map(Variant::Decimal),
        VarType::Dispatch | VarType::Unknown | VarType::Error | VarType::Variant => Err(MISMATCH),
        VarType::Bstr => Err(HResult::E_NOTIMPL),
    }
}

fn empty_or_null(to: VarType) -> Variant {
    if to == VarType::Empty {
        Variant::Empty
    } else {
        Variant::Null
    }
}

const POW10: [f64; 29] = [
    1e0, 1e1, 1e2, 1e3, 1e4, 1e5, 1e6, 1e7, 1e8, 1e9, 1e10, 1e11, 1e12, 1e13, 1e14, 1e15, 1e16,
    1e17, 1e18, 1e19, 1e20, 1e21, 1e22, 1e23, 1e24, 1e25, 1e26, 1e27, 1e28,
];

const TWO_64: f64 = 18446744073709551616.0;

fn check_decimal(d: &Decimal) -> Result<(), HResult> {
    if d.scale > 28 {
        Err(HResult::E_INVALIDARG)
    } else {
        Ok(())
    }
}

fn dec_to_f64(d: &Decimal) -> Result<f64, HResult> {
    check_decimal(d)?;
    let high = f64::from((d.mantissa >> 64) as u32) * TWO_64;
    let low = d.mantissa as u64;
    let sum = if low < 1 << 63 {
        high + low as i64 as f64
    } else {
        (low as i64 as f64 + TWO_64) + high
    };
    let x = sum / POW10[usize::from(d.scale)];
    Ok(if d.negative { -x } else { x })
}

fn to_r8(n: Num) -> Result<f64, HResult> {
    Ok(match n {
        Num::Int(i) => i as f64,
        Num::Bool(b) => {
            if b {
                -1.0
            } else {
                0.0
            }
        }
        Num::R4(f) => f64::from(f),
        Num::F64(d) => d,
        Num::Cy(c) => c as f64 / 10000.0,
        Num::Dec(d) => dec_to_f64(&d)?,
    })
}

fn round_to_int(x: f64, lo: i64, hi: i64) -> Result<i64, HResult> {
    let r = x.round_ties_even();
    if r >= lo as f64 && r < hi as f64 + 1.0 {
        Ok(r as i64)
    } else {
        Err(OVERFLOW)
    }
}

fn div_round_even(q: i128, scale: u32) -> i128 {
    let d = 10i128.pow(scale);
    let (whole, rest) = (q / d, (q % d).abs());
    let up = rest * 2 > d || (rest * 2 == d && whole % 2 != 0);
    whole + if up { q.signum() } else { 0 }
}

fn dec_to_int(d: &Decimal, lo: i64, hi: i64, float: (i64, i64)) -> Result<i64, HResult> {
    if d.scale != 0 {
        return round_to_int(dec_to_f64(d)?, float.0, float.1);
    }
    let m = d.mantissa as i128;
    i64::try_from(if d.negative { -m } else { m })
        .ok()
        .filter(|v| (lo..=hi).contains(v))
        .ok_or(OVERFLOW)
}

fn cy_to_int(c: i64, lo: i64, hi: i64) -> Result<i64, HResult> {
    let v = div_round_even(i128::from(c), 4) as i64;
    if (lo..=hi).contains(&v) {
        Ok(v)
    } else {
        Err(OVERFLOW)
    }
}

fn to_int(n: Num, lo: i64, hi: i64, float: (i64, i64)) -> Result<i64, HResult> {
    match n {
        Num::Int(i) if (lo..=hi).contains(&i) => Ok(i),
        Num::Int(_) => Err(OVERFLOW),
        Num::Bool(b) => Ok(-i64::from(b)),
        Num::R4(f) => round_to_int(f64::from(f), float.0, float.1),
        Num::F64(d) => round_to_int(d, float.0, float.1),
        Num::Cy(c) => cy_to_int(c, lo, hi),
        Num::Dec(d) => dec_to_int(&d, lo, hi, float),
    }
}

fn to_small_int(n: Num, lo: i64, hi: i64) -> Result<i64, HResult> {
    to_int(n, lo, hi, (lo, hi))
}

fn to_i4(n: Num) -> Result<i32, HResult> {
    to_small_int(n, i64::from(i32::MIN), i64::from(i32::MAX)).map(|v| v as i32)
}

fn to_ui1(n: Num) -> Result<u8, HResult> {
    match n {
        Num::Bool(b) => Ok(if b { 255 } else { 0 }),
        _ => to_small_int(n, 0, 255).map(|v| v as u8),
    }
}

fn r4_from_f64(x: f64) -> Result<f32, HResult> {
    if (-3.4028235677973362e38..=3.4028235677973362e38).contains(&x) {
        Ok(x as f32)
    } else {
        Err(OVERFLOW)
    }
}

fn to_r4(n: Num) -> Result<f32, HResult> {
    match n {
        Num::Int(i) => Ok(i as f32),
        _ => r4_from_f64(to_r8(n)?),
    }
}

fn i8_from_cy(c: i64) -> i64 {
    let whole = c / 10000;
    let low = i64::from(c as u32);
    let remainder = low - whole * 10000;
    let size = if remainder >= 0 {
        remainder
    } else {
        whole * 10000 - low
    };
    if size + (whole & 1) <= 5000 {
        whole
    } else if c >= 0 {
        whole + 1
    } else {
        whole - 1
    }
}

fn to_i8(n: Num) -> Result<i64, HResult> {
    match n {
        Num::Cy(c) => Ok(i8_from_cy(c)),
        _ => to_int(n, i64::MIN, i64::MAX, (-(1 << 62), (1 << 62) - 1)),
    }
}

fn decimal_exponent(e: i32) -> i32 {
    (e * 0x4D10) >> 16
}

struct FloatDigits {
    digits: i32,
    low: f64,
    high: f64,
    steps: &'static [i32],
}

const DOUBLE_DIGITS: FloatDigits = FloatDigits {
    digits: 14,
    low: 1e14,
    high: 1e15,
    steps: &[8, 4, 2, 1],
};

const SINGLE_DIGITS: FloatDigits = FloatDigits {
    digits: 6,
    low: 1e6,
    high: 1e7,
    steps: &[4, 2, 1],
};

impl FloatDigits {
    fn decimal(&self, x: f64, exponent: i32, negative: bool) -> Result<Decimal, HResult> {
        let zero = Decimal {
            negative: false,
            scale: 0,
            mantissa: 0,
        };
        let mut x = x.abs();
        let mut scale = self.digits - decimal_exponent(exponent);
        if scale < 0 {
            if scale == -1 && x < self.high {
                scale = 0;
            } else {
                x /= POW10[(-scale) as usize];
            }
        } else {
            scale = scale.min(28);
            x *= POW10[scale as usize];
        }
        if self.low > x && scale < 28 {
            x *= 10.0;
            scale += 1;
        }
        let whole = x as i64;
        let fraction = x - whole as f64;
        let mut mantissa = whole + i64::from(fraction > 0.5 || (fraction == 0.5 && whole & 1 == 1));
        if mantissa == 0 {
            return Ok(zero);
        }
        if scale < 0 {
            let big = mantissa as u128 * 10u128.pow((-scale) as u32);
            return if big < 1 << 96 {
                Ok(Decimal {
                    negative,
                    scale: 0,
                    mantissa: big,
                })
            } else {
                Err(OVERFLOW)
            };
        }
        let mut room = scale.min(self.digits);
        for &step in self.steps {
            let divisor = 10i64.pow(step as u32);
            if step <= room && mantissa % divisor == 0 {
                mantissa /= divisor;
                scale -= step;
                room -= step;
            }
        }
        Ok(Decimal {
            negative,
            scale: scale as u8,
            mantissa: mantissa as u128,
        })
    }
}

fn dec_from_r8(x: f64) -> Result<Decimal, HResult> {
    let bits = x.to_bits();
    let exponent = (bits >> 52 & 0x7FF) as i32;
    match exponent {
        ..=0x39F => Ok(Decimal {
            negative: false,
            scale: 0,
            mantissa: 0,
        }),
        0x3A0..=0x45E => DOUBLE_DIGITS.decimal(x, exponent - 1022, bits >> 63 == 1),
        _ => Err(OVERFLOW),
    }
}

fn dec_from_r4(f: f32) -> Result<Decimal, HResult> {
    let bits = f.to_bits();
    let exponent = (bits >> 23 & 0xFF) as i32;
    match exponent {
        ..=0x1F => Ok(Decimal {
            negative: false,
            scale: 0,
            mantissa: 0,
        }),
        0x20..=0xDE => SINGLE_DIGITS.decimal(f64::from(f), exponent - 126, bits >> 31 == 1),
        _ => Err(OVERFLOW),
    }
}

fn to_decimal(n: Num) -> Result<Decimal, HResult> {
    let integer = |negative, scale, mantissa| {
        Ok(Decimal {
            negative,
            scale,
            mantissa,
        })
    };
    match n {
        Num::Int(i) => integer(i < 0, 0, u128::from(i.unsigned_abs())),
        Num::Bool(b) => integer(b, 0, u128::from(b)),
        Num::R4(f) => dec_from_r4(f),
        Num::F64(d) => dec_from_r8(d),
        Num::Cy(c) => integer(c < 0, 4, u128::from(c.unsigned_abs())),
        Num::Dec(d) => Ok(d),
    }
}

fn to_bool(n: Num) -> Result<bool, HResult> {
    Ok(match n {
        Num::Int(i) | Num::Cy(i) => i != 0,
        Num::Bool(b) => b,
        Num::R4(f) => f != 0.0,
        Num::F64(d) => d != 0.0,
        Num::Dec(d) => {
            check_decimal(&d)?;
            d.mantissa != 0
        }
    })
}

fn truncate32(x: f64) -> i32 {
    if x > -2147483649.0 && x < 2147483648.0 {
        x as i32
    } else {
        i32::MIN
    }
}

fn truncate64(x: f64) -> i64 {
    if (-9223372036854775808.0..9223372036854775808.0).contains(&x) {
        x as i64
    } else {
        i64::MIN
    }
}

fn cy_from_f64(x: f64) -> Result<i64, HResult> {
    if x >= 922337203685477.6 || x <= -922337203685477.6 {
        return Err(OVERFLOW);
    }
    let negative = x < 0.0;
    let a = if negative { -x } else { x };
    let big = f64::from(a as f32);
    let small = a - big;
    let big = big * 10000.0;
    let small = small * 10000.0;
    let high = truncate32((small + big) * 2.3283064365386963e-10);
    let rest = big - f64::from(high) * 4294967296.0;
    let low = truncate64(small + rest) as u64;
    let remainder = (rest - (f64::from(low as u32) + 0.5)) + small;
    let mut v = (high as u32 as u64) << 32 | low as u32 as u64;
    if remainder > 0.0 || (remainder == 0.0 && low & 1 == 1) {
        v = v.wrapping_add(1);
    }
    Ok(if negative { v.wrapping_neg() } else { v } as i64)
}

fn to_cy(n: Num) -> Result<i64, HResult> {
    match n {
        Num::Int(i) if i.unsigned_abs() <= 922337203685476 => Ok(i * 10000),
        Num::Int(_) => Err(OVERFLOW),
        Num::Bool(b) => Ok(-10000 * i64::from(b)),
        Num::R4(f) => cy_from_f64(f64::from(f)),
        Num::F64(d) => cy_from_f64(d),
        Num::Cy(c) => Ok(c),
        Num::Dec(d) => {
            check_decimal(&d)?;
            let m = d.mantissa as i128;
            let v = if d.scale <= 4 {
                m * 10i128.pow(4 - u32::from(d.scale))
            } else {
                div_round_even(m, u32::from(d.scale) - 4)
            };
            i64::try_from(if d.negative { -v } else { v }).map_err(|_| OVERFLOW)
        }
    }
}

fn to_date(n: Num) -> Result<f64, HResult> {
    let x = to_r8(n)?;
    if x >= 2958466.0 || x <= -657435.0 {
        Err(OVERFLOW)
    } else {
        Ok(x)
    }
}

#[cfg(all(test, windows))]
mod tests {
    use super::*;
    use crate::oleaut::{differences, Rng};
    use crate::{flags, SafeArray};

    const SETTINGS: [(Locale, u16); 8] = [
        (Locale::EN_US, 0),
        (Locale::EN_NZ, 0),
        (Locale::EN_US, flags::NOVALUEPROP),
        (Locale::EN_US, flags::NOUSEROVERRIDE),
        (Locale::EN_US, flags::ALPHABOOL),
        (Locale::EN_US, flags::LOCALBOOL),
        (Locale::EN_NZ, flags::ALPHABOOL | flags::LOCALBOOL),
        (Locale::EN_NZ, 0x17),
    ];

    fn agree_in(settings: &[(Locale, u16)], cases: &[Variant], to: VarType) {
        let mut found = Vec::new();
        for (locale, flags) in settings {
            found.extend(differences(cases.iter().cloned(), to, *locale, *flags));
        }
        assert!(
            found.is_empty(),
            "{} differences, the first ones:
{}",
            found.len(),
            found[..found.len().min(20)].join(
                "
"
            )
        );
    }

    fn agree(cases: &[Variant], to: VarType) {
        agree_in(&SETTINGS, cases, to);
    }

    fn neighbours(xs: &[f64]) -> Vec<f64> {
        let mut out = Vec::new();
        for &x in xs {
            for y in [x, -x] {
                out.extend([y.next_down(), y, y.next_up()]);
            }
        }
        out
    }

    fn edge_doubles() -> Vec<f64> {
        let mut xs = vec![
            0.0,
            5e-324,
            1e-310,
            f64::MIN_POSITIVE,
            0.49999999999999994,
            f64::INFINITY,
            f64::from(f32::MAX),
            3.4028235677973366e38,
            3.402823567797337e38,
            f64::from(f32::MIN_POSITIVE),
            1.401298464324817e-45,
            7.006492321624085e-46,
            922337203685477.6,
            922337203685477.5,
            2958466.0,
            2958465.999999,
            657434.0,
            657435.0,
            0.00005,
            0.00015,
            1.00005,
            2147483647.00005,
        ];
        for k in 0..110 {
            let p = 2f64.powi(k);
            xs.extend([p, p + 0.5, p - 0.5, p + 1.0, p - 1.0]);
        }
        xs.extend((-110..0).map(|k| 2f64.powi(k)));
        for k in 0..400 {
            let k = f64::from(k);
            xs.extend([k / 2.0, k + 0.00005, k + 0.00015]);
        }
        for k in [
            126,
            127,
            128,
            254,
            255,
            256,
            32766,
            32767,
            32768,
            65534,
            65535,
            65536,
            2147483646,
            2147483647,
            2147483648_u64,
            4294967294,
            4294967295,
            4294967296,
        ] {
            let k = k as f64;
            xs.extend([k + 0.5, k - 0.5, k]);
        }
        let mut out = neighbours(&xs);
        out.extend(
            [
                0x7ff8_0000_0000_0000_u64,
                0xfff8_0000_0000_0000,
                0x7ff0_0000_0000_0001,
                0xfff0_0000_0000_0001,
                0x7fff_ffff_ffff_ffff,
                0xffff_ffff_ffff_ffff,
                0x7ff8_0000_dead_beef,
            ]
            .map(f64::from_bits),
        );
        out
    }

    fn random_doubles(rng: &mut Rng, n: usize) -> Vec<f64> {
        (0..n)
            .map(|i| match i % 12 {
                0 => rng.double(-60, 70),
                1 => rng.double(-8, 12),
                2 => rng.double(0, 33),
                3 => rng.double(40, 52),
                4 => rng.double(44, 54),
                5 => rng.double(-3, 40).trunc() + [0.5, -0.5, 0.25, 0.0][rng.below(4) as usize],
                6 => rng.double(-1100, 1100),
                7 => rng.double(85, 100),
                8 => rng.double(-100, -90),
                9 => (rng.below(1_000_000) as f64) / 10f64.powi(rng.below(12) as i32),
                10 => (rng.below(1_000_000) as f64) * 10f64.powi(rng.below(25) as i32),
                _ => rng.any_double(),
            })
            .collect()
    }

    fn r8_cases(rng: &mut Rng, n: usize) -> Vec<Variant> {
        let mut xs = edge_doubles();
        xs.extend(random_doubles(rng, n));
        xs.into_iter().map(Variant::R8).collect()
    }

    fn date_cases(rng: &mut Rng, n: usize) -> Vec<Variant> {
        let mut xs = edge_doubles();
        xs.extend(random_doubles(rng, n));
        xs.extend((0..n).map(|_| rng.double(14, 22)));
        xs.into_iter().map(Variant::Date).collect()
    }

    fn r4_cases(rng: &mut Rng, n: usize) -> Vec<Variant> {
        let mut xs = vec![0.0, -0.0, f32::MAX, f32::MIN, f32::MIN_POSITIVE, 1e-45];
        xs.extend([f32::INFINITY, f32::NEG_INFINITY, f32::NAN, -f32::NAN]);
        xs.extend([f32::from_bits(0x7f80_0001), f32::from_bits(0xff80_0001)]);
        xs.extend([
            0.1,
            0.2,
            0.3,
            1.0 / 3.0,
            16777216.0,
            16777217.0,
            1e-5,
            123456.7,
        ]);
        for x in edge_doubles() {
            let f = x as f32;
            xs.extend([f.next_down(), f, f.next_up()]);
        }
        for i in 0..n {
            xs.push(match i % 3 {
                0 => f32::from_bits(rng.next() as u32),
                1 => rng.double(-30, 40) as f32,
                _ => (rng.double(-8, 24) as f32 * 1000.0).round() / 1000.0,
            });
        }
        xs.into_iter().map(Variant::R4).collect()
    }

    fn random_int(rng: &mut Rng) -> i64 {
        let bits = 1 + rng.below(64) as u32;
        let magnitude = (rng.next() >> (64 - bits)) as i64;
        if rng.below(2) == 0 {
            magnitude
        } else {
            magnitude.wrapping_neg()
        }
    }

    fn cy_cases(rng: &mut Rng, n: usize) -> Vec<Variant> {
        let mut xs = vec![i64::MIN, i64::MIN + 1, i64::MAX, i64::MAX - 1, 0, 1, -1];
        for k in [
            0i64,
            1,
            2,
            3,
            126,
            127,
            128,
            254,
            255,
            256,
            32767,
            32768,
            2147483647,
            2147483648,
            4294967295,
            92233720368547,
            92233720368547,
        ] {
            for frac in [0, 1, 4999, 5000, 5001, 9999] {
                for sign in [1, -1] {
                    let c = sign * (k * 10000 + frac);
                    xs.extend([c, c.wrapping_add(1), c.wrapping_sub(1)]);
                }
            }
        }
        xs.extend((0..n).map(|_| random_int(rng)));
        xs.into_iter().map(Variant::Cy).collect()
    }

    fn dec_cases(rng: &mut Rng, n: usize) -> Vec<Variant> {
        let dec = |negative, scale, mantissa| {
            Variant::Decimal(Decimal {
                negative,
                scale,
                mantissa,
            })
        };
        let mut out = Vec::new();
        for scale in 0..=28u8 {
            for negative in [false, true] {
                for mantissa in [
                    0u128,
                    1,
                    4,
                    5,
                    6,
                    9,
                    10,
                    15,
                    25,
                    35,
                    (1 << 96) - 1,
                    1 << 95,
                    1 << 64,
                    (1 << 64) - 1,
                    1 << 63,
                    (1 << 63) - 1,
                ] {
                    out.push(dec(negative, scale, mantissa));
                }
                for k in 0..=28u32 {
                    let p = 10u128.pow(k);
                    let ties = [p, p - 1, p + 1, 5 * p, 5 * p / 10, 15 * p / 10, 25 * p / 10];
                    for m in ties.into_iter().chain([5 * p - 1, 5 * p + 1]) {
                        if m < 1 << 96 {
                            out.push(dec(negative, scale, m));
                        }
                    }
                }
            }
        }
        for i in 0..n {
            let bits = 1 + rng.below(96) as u32;
            let wide = (rng.next() as u128) << 64 | rng.next() as u128;
            let scale = if i % 2 == 0 {
                rng.below(29) as u8
            } else {
                (bits as u8 / 4).min(28)
            };
            out.push(dec(rng.below(2) == 0, scale, wide >> (128 - bits)));
        }
        out
    }

    fn int_cases(rng: &mut Rng, n: usize) -> Vec<Variant> {
        let mut out: Vec<Variant> = (i16::MIN..=i16::MAX).map(Variant::I2).collect();
        out.extend((0..=u8::MAX).map(Variant::Ui1));
        let mut xs = vec![i64::MIN, i64::MAX, 0];
        for k in 0..63 {
            let p = 1i64 << k;
            for d in -2..=2 {
                xs.extend([p + d, -p + d]);
            }
        }
        for k in [
            657434,
            657435,
            657436,
            2958465,
            2958466,
            2958467,
            922337203685477,
            922337203685478,
        ] {
            xs.extend([k, -k]);
        }
        xs.extend((-300..300).map(i64::from));

        for k in 25..63 {
            for d in -2..=2 {
                let x = (1i64 << k) + (1i64 << (k - 24)) + d;
                xs.extend([x, -x]);
            }
        }
        xs.extend((0..n).map(|_| random_int(rng)));
        out.extend(xs.iter().map(|&x| Variant::I8(x)));
        out.extend(
            xs.iter()
                .filter_map(|&x| i32::try_from(x).ok())
                .map(Variant::I4),
        );
        out.extend([Variant::Empty, Variant::Bool(true), Variant::Bool(false)]);
        out
    }

    fn numeric_sources(n: usize) -> Vec<Vec<Variant>> {
        let mut rng = Rng(0x5eed);
        vec![
            r8_cases(&mut rng, n),
            date_cases(&mut rng, n),
            r4_cases(&mut rng, n),
            cy_cases(&mut rng, n),
            dec_cases(&mut rng, n),
            int_cases(&mut rng, n),
        ]
    }

    fn all_numeric_sources_to(to: VarType) {
        for cases in numeric_sources(100_000) {
            agree_in(&SETTINGS[..1], &cases, to);
        }
        for cases in numeric_sources(1000) {
            agree(&cases, to);
        }
    }

    #[test]
    fn same_type_is_a_copy() {
        let mut rng = Rng(7);
        let cases: Vec<Variant> = (0..1000).map(|_| Variant::R8(rng.any_double())).collect();
        let none: Vec<String> = vec![];
        assert_eq!(differences(cases, VarType::R8, Locale::EN_US, 0), none);
    }

    #[test]
    fn to_r8() {
        all_numeric_sources_to(VarType::R8);
    }

    #[test]
    fn to_i4() {
        all_numeric_sources_to(VarType::I4);
    }

    #[test]
    fn to_bool() {
        all_numeric_sources_to(VarType::Bool);
    }

    #[test]
    fn to_cy() {
        all_numeric_sources_to(VarType::Cy);
    }

    #[test]
    fn to_date() {
        all_numeric_sources_to(VarType::Date);
    }

    #[test]
    fn to_i2() {
        all_numeric_sources_to(VarType::I2);
    }

    #[test]
    fn to_r4() {
        all_numeric_sources_to(VarType::R4);
    }

    #[test]
    fn to_ui1() {
        all_numeric_sources_to(VarType::Ui1);
    }

    #[test]
    fn to_i8() {
        all_numeric_sources_to(VarType::I8);
    }

    #[test]
    fn to_decimal() {
        all_numeric_sources_to(VarType::Decimal);
    }

    const TARGETS: [VarType; 16] = [
        VarType::Decimal,
        VarType::I2,
        VarType::R4,
        VarType::Ui1,
        VarType::I8,
        VarType::Empty,
        VarType::Null,
        VarType::R8,
        VarType::I4,
        VarType::Bool,
        VarType::Cy,
        VarType::Date,
        VarType::Dispatch,
        VarType::Error,
        VarType::Variant,
        VarType::Unknown,
    ];

    #[test]
    fn sources_without_a_number() {
        let array = |bounds: Vec<(i32, u32)>, data| {
            Variant::Array(Box::new(SafeArray {
                elem: VarType::Variant,
                bounds,
                data,
            }))
        };
        let cases = [
            Variant::Null,
            Variant::Error(0x800A_07FA_u32 as i32),
            Variant::Error(HResult::DISP_E_PARAMNOTFOUND.0),
            Variant::Error(0),
            Variant::Error(-1),
            Variant::Dispatch(None),
            Variant::Unknown(None),
            array(vec![(1, 1)], vec![Variant::I4(1)]),
            array(vec![(0, 0)], vec![]),
            array(
                vec![(1, 2), (1, 2)],
                vec![
                    Variant::R8(1.5),
                    Variant::Null,
                    Variant::Empty,
                    Variant::Bool(true),
                ],
            ),
        ];
        for to in TARGETS {
            agree(&cases, to);
        }
    }

    #[test]
    fn numbers_to_types_without_numbers() {
        for cases in numeric_sources(1000) {
            for to in [
                VarType::Empty,
                VarType::Null,
                VarType::Dispatch,
                VarType::Error,
                VarType::Variant,
                VarType::Unknown,
            ] {
                agree(&cases, to);
            }
        }
    }

    #[test]
    fn invalid_decimals() {
        let mut rng = Rng(11);
        let cases: Vec<Variant> = (0..2000)
            .map(|_| {
                Variant::Decimal(Decimal {
                    negative: rng.below(2) == 0,
                    scale: 29 + rng.below(227) as u8,
                    mantissa: [0, 1, rng.next() as u128, (1 << 96) - 1][rng.below(4) as usize],
                })
            })
            .collect();
        for to in TARGETS {
            agree(&cases, to);
        }
    }

    #[test]
    fn documented_examples() {
        use crate::oleaut::same;
        use Variant::{
            Bool, Cy, Date, Dispatch, Empty, Error, Null, Ui1, Unknown, I2, I4, I8, R4, R8,
        };

        let dec = |negative, scale, mantissa| {
            Variant::Decimal(Decimal {
                negative,
                scale,
                mantissa,
            })
        };
        let nan = f64::from_bits(0x7ff8_0000_dead_beef);
        let cases: Vec<(Variant, VarType, Result<Variant, HResult>)> = vec![
            (R8(nan), VarType::R8, Ok(R8(nan))),
            (R8(-0.0), VarType::R8, Ok(R8(-0.0))),
            (dec(true, 5, 100), VarType::Decimal, Ok(dec(true, 5, 100))),
            (Empty, VarType::Bool, Ok(Bool(false))),
            (Empty, VarType::Decimal, Ok(dec(false, 0, 0))),
            (I4(7), VarType::Null, Ok(Null)),
            (Dispatch(None), VarType::Empty, Ok(Empty)),
            (Null, VarType::Empty, Err(MISMATCH)),
            (Error(0x800A_07FA_u32 as i32), VarType::I4, Err(MISMATCH)),
            (Dispatch(None), VarType::Unknown, Ok(Unknown(None))),
            (Dispatch(None), VarType::I4, Err(MISMATCH)),
            (I4(1), VarType::Error, Err(MISMATCH)),
            (I4(1), VarType::Dispatch, Err(MISMATCH)),
            (dec(false, 29, 1), VarType::R8, Err(HResult::E_INVALIDARG)),
            (dec(false, 29, 1), VarType::Empty, Ok(Empty)),
            (Bool(true), VarType::I4, Ok(I4(-1))),
            (Bool(true), VarType::R8, Ok(R8(-1.0))),
            (Bool(true), VarType::Decimal, Ok(dec(true, 0, 1))),
            (Bool(true), VarType::Ui1, Ok(Ui1(255))),
            (R8(-0.0), VarType::Bool, Ok(Bool(false))),
            (R8(5e-324), VarType::Bool, Ok(Bool(true))),
            (R8(f64::NAN), VarType::Bool, Ok(Bool(true))),
            (dec(true, 28, 0), VarType::Bool, Ok(Bool(false))),
            (dec(false, 28, 1), VarType::Bool, Ok(Bool(true))),
            (I2(-5), VarType::Ui1, Err(OVERFLOW)),
            (I4(70000), VarType::I2, Err(OVERFLOW)),
            (R8(2.5), VarType::I4, Ok(I4(2))),
            (R8(3.5), VarType::I4, Ok(I4(4))),
            (R8(-2.5), VarType::I4, Ok(I4(-2))),
            (R8(-2147483648.5), VarType::I4, Ok(I4(i32::MIN))),
            (R8(2147483647.5), VarType::I4, Err(OVERFLOW)),
            (R8(-32768.5), VarType::I2, Ok(I2(i16::MIN))),
            (R8(32767.5), VarType::I2, Err(OVERFLOW)),
            (R8(-0.5), VarType::Ui1, Ok(Ui1(0))),
            (R8(255.5), VarType::Ui1, Err(OVERFLOW)),
            (R8(f64::NAN), VarType::I4, Err(OVERFLOW)),
            (R8(f64::INFINITY), VarType::I2, Err(OVERFLOW)),
            (R8(4611686018427387904.0), VarType::I8, Err(OVERFLOW)),
            (R8(-4611686018427387904.0), VarType::I8, Ok(I8(-(1 << 62)))),
            (Cy(25000), VarType::I4, Ok(I4(2))),
            (Cy(35000), VarType::I4, Ok(I4(4))),
            (Cy(-25000), VarType::I4, Ok(I4(-2))),
            (Cy(-21474836485000), VarType::I4, Ok(I4(i32::MIN))),
            (Cy(-10000), VarType::I8, Ok(I8(-2))),
            (Cy(-1), VarType::I8, Ok(I8(-1))),
            (Cy(4294970000), VarType::I8, Ok(I8(429498))),
            (Cy(25000), VarType::I8, Ok(I8(2))),
            (Cy(-25000), VarType::I8, Ok(I8(-3))),
            (dec(false, 17, 50000000000000001), VarType::I4, Ok(I4(0))),
            (
                dec(false, 24, 2500000000000000000000000),
                VarType::I4,
                Ok(I4(3)),
            ),
            (
                dec(false, 1, 50000000000000000000),
                VarType::I8,
                Err(OVERFLOW),
            ),
            (I8((1 << 53) + 1), VarType::R8, Ok(R8(9007199254740992.0))),
            (R4(0.1), VarType::R8, Ok(R8(0.10000000149011612))),
            (Cy(12345), VarType::R8, Ok(R8(1.2345))),
            (
                I8((1 << 62) + (1 << 38) + 1),
                VarType::R4,
                Ok(R4(4.6116866e18)),
            ),
            (R8(1e39), VarType::R4, Err(OVERFLOW)),
            (R8(f64::NAN), VarType::R4, Err(OVERFLOW)),
            (R8(3.4028235677973362e38), VarType::R4, Ok(R4(f32::MAX))),
            (
                R8(3.4028235677973362e38_f64.next_up()),
                VarType::R4,
                Err(OVERFLOW),
            ),
            (R8(1e-50), VarType::R4, Ok(R4(0.0))),
            (
                dec(false, 2, 1162002898094173229013531282),
                VarType::R8,
                Ok(R8(1.162002898094173e25)),
            ),
            (dec(true, 3, 0), VarType::R8, Ok(R8(-0.0))),
            (R8(2958466.0), VarType::Date, Err(OVERFLOW)),
            (
                R8(2958466f64.next_down()),
                VarType::Date,
                Ok(Date(2958466f64.next_down())),
            ),
            (R8(-657435.0), VarType::Date, Err(OVERFLOW)),
            (I8(2958466), VarType::Date, Err(OVERFLOW)),
            (I8(2958465), VarType::Date, Ok(Date(2958465.0))),
            (R8(nan), VarType::Date, Ok(Date(nan))),
            (
                I8(922337203685476),
                VarType::Cy,
                Ok(Cy(9223372036854760000)),
            ),
            (I8(922337203685477), VarType::Cy, Err(OVERFLOW)),
            (R8(922337203685477.6), VarType::Cy, Err(OVERFLOW)),
            (R8(f64::NAN), VarType::Cy, Ok(Cy(i64::MIN))),
            (
                R8(-1629888469945.9783),
                VarType::Cy,
                Ok(Cy(-16298884699459783)),
            ),
            (R8(0.00005), VarType::Cy, Ok(Cy(1))),
            (dec(false, 5, 25), VarType::Cy, Ok(Cy(2))),
            (I4(0), VarType::Decimal, Ok(dec(false, 0, 0))),
            (Cy(10000), VarType::Decimal, Ok(dec(false, 4, 10000))),
            (Cy(0), VarType::Decimal, Ok(dec(false, 4, 0))),
            (Cy(-5), VarType::Decimal, Ok(dec(true, 4, 5))),
            (R8(0.1 + 0.2), VarType::Decimal, Ok(dec(false, 1, 3))),
            (
                R8(1.0 / 3.0),
                VarType::Decimal,
                Ok(dec(false, 15, 333333333333333)),
            ),
            (
                R8(1e15),
                VarType::Decimal,
                Ok(dec(false, 0, 1_000_000_000_000_000)),
            ),
            (
                R8(123456789012345678.0),
                VarType::Decimal,
                Ok(dec(false, 0, 123456789012346000)),
            ),
            (R8(1e-30), VarType::Decimal, Ok(dec(false, 0, 0))),
            (R8(-0.0), VarType::Decimal, Ok(dec(false, 0, 0))),
            (
                R8(1e28),
                VarType::Decimal,
                Ok(dec(false, 0, 10u128.pow(28))),
            ),
            (R8(1e29), VarType::Decimal, Err(OVERFLOW)),
            (R4(0.1), VarType::Decimal, Ok(dec(false, 1, 1))),
            (
                R4(16777216.0),
                VarType::Decimal,
                Ok(dec(false, 0, 16777220)),
            ),
            (R4(1.0 / 3.0), VarType::Decimal, Ok(dec(false, 7, 3333333))),
        ];
        for (src, to, want) in cases {
            let ours = crate::change_type(&src, to, Locale::EN_US, 0);
            let theirs = crate::oleaut::change_type(&src, to, Locale::EN_US, 0);
            let agree = |a: &Result<Variant, HResult>, b: &Result<Variant, HResult>| match (a, b) {
                (Ok(a), Ok(b)) => same(a, b),
                (a, b) => a == b,
            };
            assert!(
                agree(&theirs, &want),
                "{src:?} -> {to:?}: oleaut32 {theirs:?}, documented {want:?}"
            );
            assert!(
                agree(&ours, &want),
                "{src:?} -> {to:?}: ours {ours:?}, documented {want:?}"
            );
        }
    }
}
