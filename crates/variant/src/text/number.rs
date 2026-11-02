use super::parse::Parsed;
use crate::{Decimal, HResult};

const OVERFLOW: HResult = HResult::DISP_E_OVERFLOW;

fn radix_bits(p: &Parsed) -> Result<u64, HResult> {
    let bits = p.digits.len() as u32 * p.shift;
    if bits > 66 || bits == 66 && p.digits[0] > 1 {
        return Err(OVERFLOW);
    }
    Ok(p.digits
        .iter()
        .fold(0, |v: u64, &d| v << p.shift | u64::from(d)))
}

fn radix_signed(v: u64) -> (bool, u64) {
    if v >= 1 << 63 {
        (true, v.wrapping_neg())
    } else if (1 << 31..1 << 32).contains(&v) {
        (true, (1 << 32) - v)
    } else {
        (false, v)
    }
}

fn fold(digits: &[u8]) -> u128 {
    digits.iter().fold(0, |v: u128, &d| v * 10 + u128::from(d))
}

fn round_div(d: u128, k: u32, inexact: bool) -> u128 {
    if k == 0 {
        return d;
    }
    let div = 10u128.pow(k);
    let (q, r) = (d / div, d % div);
    let up = match (2 * r).cmp(&div) {
        std::cmp::Ordering::Greater => true,
        std::cmp::Ordering::Equal => q & 1 == 1 || inexact,
        std::cmp::Ordering::Less => false,
    };
    q + u128::from(up)
}

fn integer(p: &Parsed) -> Option<u128> {
    let int_digits = p.pwr10 + p.digits.len() as i64;
    if int_digits > 20 {
        return None;
    }
    if p.pwr10 >= 0 {
        return Some(fold(&p.digits) * 10u128.pow(p.pwr10 as u32));
    }
    let k = -p.pwr10;
    if k > p.digits.len() as i64 {
        return Some(0);
    }
    Some(round_div(fold(&p.digits), k as u32, p.inexact))
}

fn signed(p: &Parsed, max: u128) -> Result<i128, HResult> {
    let v = integer(p).ok_or(OVERFLOW)?;
    if p.neg && v <= max + 1 {
        Ok(-(v as i128))
    } else if !p.neg && v <= max {
        Ok(v as i128)
    } else {
        Err(OVERFLOW)
    }
}

pub(super) fn i2(p: &Parsed) -> Result<i16, HResult> {
    if p.shift != 0 {
        let v = radix_bits(p)?;
        return if !(0x1_0000..=0xFFFF_7FFF).contains(&v) {
            Ok(v as i16)
        } else {
            Err(OVERFLOW)
        };
    }
    signed(p, 0x7FFF).map(|v| v as i16)
}

pub(super) fn i4(p: &Parsed) -> Result<i32, HResult> {
    if p.shift != 0 {
        let bits = p.digits.len() as u32 * p.shift;
        if bits > 33 || bits == 33 && p.digits[0] > 3 {
            return Err(OVERFLOW);
        }
        let v = p
            .digits
            .iter()
            .fold(0u32, |v, &d| v << p.shift | u32::from(d));
        return Ok(v as i32);
    }
    let int_digits = p.pwr10 + p.digits.len() as i64;
    if int_digits > 10 || int_digits == 10 && p.digits[0] > 2 {
        return Err(OVERFLOW);
    }
    signed(p, i32::MAX as u128).map(|v| v as i32)
}

pub(super) fn i8(p: &Parsed) -> Result<i64, HResult> {
    if p.shift != 0 {
        if p.digits.len() as u32 * p.shift > 65 {
            return Err(OVERFLOW);
        }
        let v = p
            .digits
            .iter()
            .fold(0u64, |v, &d| v << p.shift | u64::from(d));
        return Ok(v as i64);
    }
    if p.pwr10 + p.digits.len() as i64 > 19 {
        return Err(OVERFLOW);
    }
    signed(p, i64::MAX as u128).map(|v| v as i64)
}

pub(super) fn ui1(p: &Parsed) -> Result<u8, HResult> {
    let v = i2(p)? as u16;
    u8::try_from(v).map_err(|_| OVERFLOW)
}

pub(super) fn cy(p: &Parsed) -> Result<i64, HResult> {
    if p.pwr10 + p.digits.len() as i64 > 15 {
        return Err(OVERFLOW);
    }
    let (neg, m) = if p.shift != 0 {
        let v = radix_bits(p)?;
        if v >= 1 << 63 {
            return Err(OVERFLOW);
        }
        let (neg, m) = radix_signed(v);
        (neg, u128::from(m) * 10_000)
    } else {
        let d = fold(&p.digits);
        let e = p.pwr10 + 4;
        let m = if e >= 0 {
            d * 10u128.pow(e as u32)
        } else {
            round_div(d, (-e).min(38) as u32, p.inexact)
        };
        (p.neg, m)
    };
    match (neg, i64::try_from(m)) {
        (false, Ok(v)) => Ok(v),
        (true, _) if m <= 1 << 63 => Ok((m as i64).wrapping_neg()),
        _ => Err(OVERFLOW),
    }
}

const DEC_MAX: u128 = (1 << 96) - 1;

pub(super) fn decimal(p: &Parsed) -> Result<Decimal, HResult> {
    let dec = |negative, scale, mantissa| Decimal {
        negative,
        scale,
        mantissa,
    };
    if p.shift != 0 {
        let (neg, m) = radix_signed(radix_bits(p)?);
        return Ok(dec(neg, 0, u128::from(m)));
    }
    let int_digits = p.pwr10 + p.digits.len() as i64;
    if int_digits > 29 {
        return Err(OVERFLOW);
    }
    if int_digits < -28 {
        return Ok(dec(p.neg, 0, 0));
    }
    let d = fold(&p.digits);
    if p.pwr10 >= 0 {
        let k = p.pwr10 as u32;
        return match 10u128.checked_pow(k).and_then(|f| d.checked_mul(f)) {
            Some(m) if m <= DEC_MAX => Ok(dec(p.neg, 0, m)),
            _ => Err(OVERFLOW),
        };
    }
    let decimals = (-p.pwr10) as u32;
    let mut scale = decimals.min(28);
    loop {
        let m = round_div(d, decimals - scale, p.inexact);
        if m <= DEC_MAX {
            return Ok(dec(p.neg, scale as u8, m));
        }
        if scale == 0 {
            return Err(OVERFLOW);
        }
        scale -= 1;
    }
}

const POW10: [f64; 23] = [
    1e0, 1e1, 1e2, 1e3, 1e4, 1e5, 1e6, 1e7, 1e8, 1e9, 1e10, 1e11, 1e12, 1e13, 1e14, 1e15, 1e16,
    1e17, 1e18, 1e19, 1e20, 1e21, 1e22,
];

const DP_POW10: [(u64, u64); 16] = [
    (0x4341_c379_37e0_8000, 0),
    (0x4693_b8b5_b505_6e16, 0x4346_77c0_8000_0000),
    (0x49e5_e531_a0a1_c872, 0x4697_5a59_c204_adfd),
    (0x4d38_4f03_e93f_f9f4, 0x49eb_54f2_fc2d_c71d),
    (0x508a_fcef_51f0_fb5e, 0x4d3f_ef70_cc11_7b25),
    (0x53dd_f675_62d8_b362, 0x5082_8b17_3e25_3bcf),
    (0x5730_a1f5_b813_2466, 0x53d4_f01f_147b_5e30),
    (0x5a82_7748_f930_1d31, 0x5733_7f19_bc0d_b0da),
    (0x5dd4_8057_38b5_1a74, 0x5a8b_9d7c_5801_b271),
    (0x6126_c2d4_256f_fcc2, 0x5dde_a95d_ee21_acc5),
    (0x6479_4514_5230_b377, 0x612e_4df5_ee3d_d40f),
    (0x67cc_0e1e_f1a7_24ea, 0x6475_aa16_ee09_4fd1),
    (0x6b1f_25c1_86a6_f04c, 0x67a4_5a77_08a5_6ccd),
    (0x6e71_4a52_dffc_6799, 0x6b02_f82b_d0b7_0d99),
    (0x71c3_3234_de7a_d7e2, 0x6e7d_96b3_2836_8782),
    (0x6515_4fdd_7f73_bf3b, 0x61ca_3776_ee00_6e63),
];

fn dp_pow10(k: usize) -> (f64, f64) {
    let (h, l) = DP_POW10[k - 1];
    (f64::from_bits(h), f64::from_bits(l))
}

fn split(x: f64) -> (f64, f64) {
    let h = f64::from_bits(x.to_bits() & 0xFFFF_FFFF_F800_0000);
    (h, x - h)
}

fn dp_mul(a: (f64, f64), b: (f64, f64)) -> (f64, f64) {
    let (ah, al) = split(a.0);
    let (bh, bl) = split(b.0);
    let hh = bh * ah;
    let c = bh * al + bl * ah;
    let s = (c + hh) - hh;
    let r = c - s;
    let lo = (((bl * al + b.1 * ah) + bh * a.1) + (bl * a.1 + b.1 * al)) + r;
    (s + hh, lo)
}

fn shift_exponent(x: f64, delta: i64) -> f64 {
    let b = x.to_bits();
    let e = ((b >> 52 & 0x7FF) as i64 + delta) as u64 & 0x7FF;
    f64::from_bits(b & !(0x7FF << 52) | e << 52)
}

fn mul_power10(hi: f64, lo: f64, n: i64) -> Result<f64, HResult> {
    let m = n.unsigned_abs();
    if lo == 0.0 && m <= 22 {
        return Ok(if n >= 0 {
            hi * POW10[m as usize]
        } else {
            hi / POW10[m as usize]
        });
    }
    let mut p = (POW10[(m & 15) as usize], 0.0);
    if (m >> 4) & 15 != 0 {
        p = dp_mul(p, dp_pow10(((m >> 4) & 15) as usize));
    }
    let big = m >= 256;
    if big {
        if n < -350 {
            return Ok(0.0);
        }
        if n >= 309 {
            return Err(OVERFLOW);
        }
        p = dp_mul(p, dp_pow10(16));
    }
    if n >= 0 {
        let (xh, xl) = dp_mul((hi, lo), p);
        let r = xl + xh;
        if !big {
            return Ok(r);
        }
        if (r.to_bits() >> 52 & 0x7FF) + 0x100 >= 0x7FF {
            return Err(OVERFLOW);
        }
        return Ok(shift_exponent(r, 0x100));
    }
    let (mut xh, mut xl) = (hi, lo);
    if big {
        xh = shift_exponent(xh, -0x100);
        if xl != 0.0 {
            xl = shift_exponent(xl, -0x100);
        }
    }
    let q = xh / p.0;
    let t = dp_mul((q, 0.0), p);
    Ok(((xl - t.1) + (xh - t.0)) / (p.1 + p.0) + q)
}

pub(super) fn r8(p: &Parsed) -> Result<f64, HResult> {
    if p.shift != 0 {
        let (neg, m) = radix_signed(radix_bits(p)?);
        let x = m as f64;
        return Ok(if neg { -x } else { x });
    }
    let x = if p.digits.len() <= 15 {
        mul_power10(fold(&p.digits) as f64, 0.0, p.pwr10)?
    } else {
        let (head, tail) = p.digits.split_at(15);
        let hi = fold(head) as f64;
        let lo = fold(tail) as f64 / POW10[tail.len()];
        mul_power10(hi, lo, p.pwr10 + tail.len() as i64)?
    };
    Ok(if p.neg { -x } else { x })
}

const R4_MAX: f64 = f64::from_bits(0x47EF_FFFF_EFFF_FFFF);

pub(super) fn r4(p: &Parsed) -> Result<f32, HResult> {
    let x = r8(p)?;
    if x.abs() > R4_MAX {
        return Err(OVERFLOW);
    }
    Ok(x as f32)
}
