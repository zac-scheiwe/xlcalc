use std::cmp::Ordering;
use std::ops::{Add, Div, Mul, Neg, Sub};

const EMAX: i32 = 16383;

const EMIN: i32 = -16382;

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
enum Class {
    Zero,
    Finite,
    Inf,
    Nan,
}

#[derive(Clone, Copy, Debug)]
pub struct F80 {
    class: Class,
    neg: bool,
    m: u64,
    e: i32,
}

fn make(neg: bool, n: u128, mut e: i32, sticky: bool) -> F80 {
    if n == 0 {
        return F80::zero(neg);
    }
    let len = 128 - n.leading_zeros();
    let q = if len > 64 {
        let shift = len - 64;
        let drop = n & ((1u128 << shift) - 1);
        let half = 1u128 << (shift - 1);
        let mut q = n >> shift;
        e += shift as i32;
        if drop > half || (drop == half && (sticky || q & 1 == 1)) {
            q += 1;
            if q >> 64 != 0 {
                q >>= 1;
                e += 1;
            }
        }
        q as u64
    } else {
        e -= (64 - len) as i32;
        (n << (64 - len)) as u64
    };
    if e + 63 > EMAX {
        return F80::inf(neg);
    }
    if e + 63 < EMIN {
        return F80::zero(neg);
    }
    F80 {
        class: Class::Finite,
        neg,
        m: q,
        e,
    }
}

fn store(m: u64, e: i32, bits: u32, emin: i32, emax: i32, tiny: i32) -> Option<(u64, i32)> {
    let top = e + 63;

    let grid = if top >= emin {
        top - bits as i32 + 1
    } else {
        tiny
    };
    let shift = grid - e;
    let (q, g) = if shift <= 0 {
        (m, e)
    } else if shift >= 66 {
        (0, grid)
    } else {
        let n = m as u128;
        let drop = n & ((1u128 << shift) - 1);
        let half = 1u128 << (shift - 1);
        let q = n >> shift;
        let q = q + (drop > half || (drop == half && q & 1 == 1)) as u128;
        (q as u64, grid)
    };

    let len = 64 - q.leading_zeros() as i32;
    if q != 0 && g + len - 1 > emax {
        return None;
    }
    Some((q, g))
}

fn pack(q: u64, g: i32, frac: u32, tiny: i32, bias: i32) -> u64 {
    if q == 0 {
        return 0;
    }
    if g == tiny {
        return q;
    }
    let len = 64 - q.leading_zeros();
    let top = g + len as i32 - 1;

    let norm = if len > frac + 1 {
        q >> (len - frac - 1)
    } else {
        q << (frac + 1 - len)
    };
    (((top + bias) as u64) << frac) | (norm & ((1u64 << frac) - 1))
}

impl F80 {
    const fn zero(neg: bool) -> F80 {
        F80 {
            class: Class::Zero,
            neg,
            m: 0,
            e: 0,
        }
    }

    const fn inf(neg: bool) -> F80 {
        F80 {
            class: Class::Inf,
            neg,
            m: 0,
            e: 0,
        }
    }

    const NAN: F80 = F80 {
        class: Class::Nan,
        neg: true,
        m: 0,
        e: 0,
    };

    pub fn from_f64(x: f64) -> F80 {
        if x.is_nan() {
            return F80::NAN;
        }
        let neg = x.is_sign_negative();
        if x == 0.0 {
            return F80::zero(neg);
        }
        if x.is_infinite() {
            return F80::inf(neg);
        }
        let bits = x.to_bits();
        let be = ((bits >> 52) & 0x7ff) as i32;
        let frac = bits & ((1u64 << 52) - 1);
        let (m, e) = if be == 0 {
            (frac, -1074)
        } else {
            (frac | (1u64 << 52), be - 1075)
        };
        let lz = m.leading_zeros() as i32;
        F80 {
            class: Class::Finite,
            neg,
            m: m << lz,
            e: e - lz,
        }
    }

    pub fn from_f32(x: f32) -> F80 {
        F80::from_f64(f64::from(x))
    }

    pub fn from_i64(i: i64) -> F80 {
        if i == 0 {
            return F80::zero(false);
        }
        let a = i.unsigned_abs();
        let lz = a.leading_zeros() as i32;
        F80 {
            class: Class::Finite,
            neg: i < 0,
            m: a << lz,
            e: -lz,
        }
    }

    pub fn is_nan(self) -> bool {
        self.class == Class::Nan
    }

    pub fn is_infinite(self) -> bool {
        self.class == Class::Inf
    }

    pub fn is_zero(self) -> bool {
        self.class == Class::Zero
    }

    fn fchs(self) -> F80 {
        F80 {
            neg: !self.neg,
            ..self
        }
    }

    fn fadd(self, o: F80) -> F80 {
        match (self.class, o.class) {
            (Class::Nan, _) | (_, Class::Nan) => F80::NAN,
            (Class::Inf, Class::Inf) if self.neg != o.neg => F80::NAN,
            (Class::Inf, _) => self,
            (_, Class::Inf) => o,
            (Class::Zero, Class::Zero) => F80::zero(self.neg && o.neg),
            (Class::Zero, _) => o,
            (_, Class::Zero) => self,
            (Class::Finite, Class::Finite) => {
                let (a, b) = if (self.e, self.m) >= (o.e, o.m) {
                    (self, o)
                } else {
                    (o, self)
                };
                let d = (a.e - b.e) as u32;
                let big = (a.m as u128) << 62;
                let small = (b.m as u128) << 62;
                let (mut s, sticky) = if d >= 127 {
                    (0, true)
                } else {
                    (small >> d, small & ((1u128 << d) - 1) != 0)
                };

                s |= sticky as u128;
                let n = if a.neg == b.neg { big + s } else { big - s };
                if n == 0 {
                    return F80::zero(false);
                }
                make(a.neg, n, a.e - 62, false)
            }
        }
    }

    fn fmul(self, o: F80) -> F80 {
        let neg = self.neg != o.neg;
        match (self.class, o.class) {
            (Class::Nan, _) | (_, Class::Nan) => F80::NAN,
            (Class::Inf, Class::Zero) | (Class::Zero, Class::Inf) => F80::NAN,
            (Class::Inf, _) | (_, Class::Inf) => F80::inf(neg),
            (Class::Zero, _) | (_, Class::Zero) => F80::zero(neg),
            (Class::Finite, Class::Finite) => {
                make(neg, self.m as u128 * o.m as u128, self.e + o.e, false)
            }
        }
    }

    fn fdiv(self, o: F80) -> F80 {
        let neg = self.neg != o.neg;
        match (self.class, o.class) {
            (Class::Nan, _) | (_, Class::Nan) => F80::NAN,
            (Class::Inf, Class::Inf) | (Class::Zero, Class::Zero) => F80::NAN,
            (Class::Inf, _) | (_, Class::Zero) => F80::inf(neg),
            (Class::Zero, _) | (_, Class::Inf) => F80::zero(neg),
            (Class::Finite, Class::Finite) => {
                let d = o.m as u128;
                let n = (self.m as u128) << 64;
                let (q, r) = (n / d, n % d);
                let r2 = r << 1;
                let (q, r) = if r2 >= d {
                    ((q << 1) | 1, r2 - d)
                } else {
                    (q << 1, r2)
                };
                make(neg, q, self.e - o.e - 65, r != 0)
            }
        }
    }

    pub fn compare(self, o: F80) -> Option<Ordering> {
        if self.is_nan() || o.is_nan() {
            return None;
        }

        let key = |v: F80| -> (i8, (u8, i32, u64)) {
            let mag = match v.class {
                Class::Zero => (0, 0, 0),
                Class::Finite => (1, v.e, v.m),
                _ => (2, 0, 0),
            };
            let s = if v.class == Class::Zero {
                0
            } else if v.neg {
                -1
            } else {
                1
            };
            (s, mag)
        };
        let (sa, ma) = key(self);
        let (sb, mb) = key(o);
        Some(match sa.cmp(&sb) {
            Ordering::Equal if sa < 0 => mb.cmp(&ma),
            Ordering::Equal => ma.cmp(&mb),
            other => other,
        })
    }

    pub fn to_f64(self) -> Option<f64> {
        match self.class {
            Class::Nan => Some(f64::NAN),
            Class::Inf => Some(if self.neg {
                f64::NEG_INFINITY
            } else {
                f64::INFINITY
            }),
            Class::Zero => Some(if self.neg { -0.0 } else { 0.0 }),
            Class::Finite => {
                let (q, g) = store(self.m, self.e, 53, -1022, 1023, -1074)?;
                let bits = pack(q, g, 52, -1074, 1023) | (u64::from(self.neg) << 63);
                Some(f64::from_bits(bits))
            }
        }
    }

    pub fn to_f32(self) -> Option<f32> {
        match self.class {
            Class::Nan => Some(f32::NAN),
            Class::Inf => Some(if self.neg {
                f32::NEG_INFINITY
            } else {
                f32::INFINITY
            }),
            Class::Zero => Some(if self.neg { -0.0 } else { 0.0 }),
            Class::Finite => {
                let (q, g) = store(self.m, self.e, 24, -126, 127, -149)?;
                let bits = pack(q, g, 23, -149, 127) as u32 | (u32::from(self.neg) << 31);
                Some(f32::from_bits(bits))
            }
        }
    }

    pub fn round_to_i64(self) -> Option<i64> {
        match self.class {
            Class::Zero => Some(0),
            Class::Nan | Class::Inf => None,
            Class::Finite => {
                if self.e >= 0 {
                    return (self.neg && self.e == 0 && self.m == 1 << 63).then_some(i64::MIN);
                }
                let shift = (-self.e) as u32;
                if shift >= 65 {
                    return Some(0);
                }
                let n = self.m as u128;
                let drop = n & ((1u128 << shift) - 1);
                let half = 1u128 << (shift - 1);
                let q = n >> shift;
                let q = q + (drop > half || (drop == half && q & 1 == 1)) as u128;
                if self.neg {
                    (q <= 1 << 63).then(|| (q as i64).wrapping_neg())
                } else {
                    i64::try_from(q).ok()
                }
            }
        }
    }
}

impl Neg for F80 {
    type Output = F80;

    fn neg(self) -> F80 {
        self.fchs()
    }
}

impl Add for F80 {
    type Output = F80;

    fn add(self, o: F80) -> F80 {
        self.fadd(o)
    }
}

impl Sub for F80 {
    type Output = F80;

    fn sub(self, o: F80) -> F80 {
        self.fadd(o.fchs())
    }
}

impl Mul for F80 {
    type Output = F80;

    fn mul(self, o: F80) -> F80 {
        self.fmul(o)
    }
}

impl Div for F80 {
    type Output = F80;

    fn div(self, o: F80) -> F80 {
        self.fdiv(o)
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    fn rand(st: &mut u64) -> u64 {
        *st ^= *st << 13;
        *st ^= *st >> 7;
        *st ^= *st << 17;
        *st
    }

    fn rand_dbl(st: &mut u64, lo: i32, hi: i32) -> f64 {
        let m = rand(st) >> 12;
        let e = lo + (rand(st) % (hi - lo) as u64) as i32;
        let x = f64::from_bits((1023u64 << 52) | m) * 2f64.powi(e);
        if rand(st) & 1 == 0 {
            -x
        } else {
            x
        }
    }

    fn d(x: f64) -> F80 {
        F80::from_f64(x)
    }

    #[test]
    fn measured_vba_cases() {
        let e = 2f64.powi(-60);
        assert_eq!(d(1.0).fadd(d(e)).fadd(d(-1.0)).to_f64(), Some(e));

        let big = d(1e308).fmul(d(10.0));
        assert_eq!(big.fdiv(d(10.0)).to_f64(), Some(1e308));
        assert_eq!(big.to_f64(), None);

        let a = 1.0000000000000002;
        assert_eq!(
            d(a).fmul(d(a)).fadd(d(-1.0)).to_f64(),
            Some(4.440892098500626e-16)
        );
    }

    #[test]
    fn stores() {
        assert_eq!(d(0.1).to_f32(), Some(0.1f32));
        assert_eq!(d(f64::MAX).to_f32(), None);
        assert_eq!(d(5e-324).to_f64(), Some(5e-324));
        let tiny = d(f64::MIN_POSITIVE).fmul(d(0.75));
        assert_eq!(tiny.to_f64(), Some(f64::MIN_POSITIVE * 0.75));
        assert_eq!(d(2.5).round_to_i64(), Some(2));
        assert_eq!(d(-3.5).round_to_i64(), Some(-4));
        assert_eq!(d(0.5).round_to_i64(), Some(0));
        assert_eq!(d(-9.223_372_036_854_776e18).round_to_i64(), Some(i64::MIN));
        assert_eq!(d(9.223_372_036_854_776e18).round_to_i64(), None);
        assert_eq!(F80::from_i64(i64::MIN).round_to_i64(), Some(i64::MIN));
        assert_eq!(F80::from_i64(-7).to_f64(), Some(-7.0));
        assert!(d(1.0).fdiv(d(0.0)).is_infinite());
        assert!(d(0.0).fdiv(d(0.0)).is_nan());
        assert_eq!(d(-0.0).compare(d(0.0)), Some(Ordering::Equal));
        assert_eq!(d(-2.0).compare(d(-1.0)), Some(Ordering::Less));
    }

    #[test]
    fn single_ops_match_x87_model() {
        let mut st = 0x9e37_79b9_7f4a_7c15u64;
        for _ in 0..100_000 {
            let a = rand_dbl(&mut st, -60, 60);
            let b = rand_dbl(&mut st, -60, 60);
            assert_eq!(
                d(a).fadd(d(b)).to_f64().unwrap().to_bits(),
                crate::x87::add(a, b).to_bits()
            );
            assert_eq!(
                d(a).fmul(d(b)).to_f64().unwrap().to_bits(),
                crate::x87::mul(a, b).to_bits()
            );
            assert_eq!(
                d(a).fdiv(d(b)).to_f64().unwrap().to_bits(),
                crate::x87::div(a, b).to_bits()
            );
        }
    }

    #[cfg(target_arch = "x86_64")]
    mod native {
        use super::super::*;
        use std::arch::asm;

        static CW: u16 = 0x037F;

        pub fn to_bits80(v: F80) -> [u8; 10] {
            let (m, be) = match v.class {
                Class::Zero => (0u64, 0u16),
                Class::Finite => (v.m, (v.e + 63 + 16383) as u16),
                Class::Inf => (1 << 63, 0x7fff),
                Class::Nan => (0xc000_0000_0000_0000, 0x7fff),
            };
            let mut out = [0u8; 10];
            out[..8].copy_from_slice(&m.to_le_bytes());
            out[8..].copy_from_slice(&(be | (u16::from(v.neg) << 15)).to_le_bytes());
            out
        }

        pub fn op(op: u8, a: F80, b: F80) -> [u8; 10] {
            let (a, b) = (to_bits80(a), to_bits80(b));
            let mut r = [0u8; 10];
            let mut env = [0u32; 7];
            macro_rules! run {
                ($ins:literal) => {

                    unsafe {
                        asm!(
                            "fnstenv [{env}]",
                            "fldcw word ptr [{cw}]",
                            "fld tbyte ptr [{a}]",
                            "fld tbyte ptr [{b}]",
                            $ins,
                            "fstp tbyte ptr [{r}]",
                            "fldenv [{env}]",
                            cw = in(reg) &CW,
                            env = in(reg) &mut env,
                            a = in(reg) &a,
                            b = in(reg) &b,
                            r = in(reg) &mut r,
                            out("st(0)") _, out("st(1)") _, out("st(2)") _, out("st(3)") _,
                            out("st(4)") _, out("st(5)") _, out("st(6)") _, out("st(7)") _,
                            options(nostack)
                        )
                    }
                };
            }
            match op {
                b'+' => run!("faddp st(1), st(0)"),
                b'*' => run!("fmulp st(1), st(0)"),
                _ => run!("fdivp st(1), st(0)"),
            }
            r
        }

        #[test]
        fn matches_native() {
            let mut st = 0x2545_f491_4f6c_dd1du64;
            let mut acc = Vec::new();
            for i in 0..200_000 {
                let a = super::rand_dbl(&mut st, -40, 40);
                let b = super::rand_dbl(&mut st, -40, 40);

                let x = if i % 3 == 0 {
                    F80::from_f64(a).fmul(F80::from_f64(b))
                } else {
                    F80::from_f64(a)
                };
                let y = if i % 5 == 0 {
                    x.fchs().fadd(F80::from_f64(b * 1e-17))
                } else if i % 7 == 0 {
                    F80::from_f64(a).fdiv(F80::from_f64(b))
                } else {
                    F80::from_f64(b)
                };
                for o in *b"+*/" {
                    let ours = match o {
                        b'+' => x.fadd(y),
                        b'*' => x.fmul(y),
                        _ => x.fdiv(y),
                    };
                    assert_eq!(to_bits80(ours), op(o, x, y), "{a} {} {b}", o as char);
                }
                acc.push(x);
            }
            assert!(!acc.is_empty());
        }
    }
}
