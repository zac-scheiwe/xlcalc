use crate::functions::statdist::dd::DD;

const NATIVE: bool = cfg!(all(target_arch = "x86_64", not(feature = "x87-emulate")));

const TRIG_MAX: f64 = 134217728.0;

#[cfg(target_arch = "x86_64")]
#[path = "x87/native.rs"]
mod native;

#[cfg(not(target_arch = "x86_64"))]
mod native {
    pub fn none(_: f64) -> f64 {
        unreachable!("the x87 instructions need x86_64")
    }
    pub use none as atan_abs;
    pub use none as cos_abs;
    pub use none as exp;
    pub use none as ln;
    pub use none as log10;
    pub use none as sin_abs;
    pub use none as tan_abs;
}

fn round2(neg: bool, n: u128, mut e: i32, sticky: bool) -> Option<f64> {
    if n == 0 {
        return Some(if neg { -0.0 } else { 0.0 });
    }
    let q = round_to(n, &mut e, sticky, 64);
    let sign = (neg as u64) << 63;
    if e + 63 < -1022 {
        let shift = (-1074 - e) as u32;
        let m = if shift >= 66 {
            0
        } else {
            let drop = q & ((1u128 << shift) - 1);
            let half = 1u128 << (shift - 1);
            let m = q >> shift;
            m + (drop > half || (drop == half && m & 1 == 1)) as u128
        };
        return Some(f64::from_bits(sign | m as u64));
    }
    let q = round_to(q, &mut e, false, 53);
    let biased = e + 52 + 1023;
    if !(1..=2046).contains(&biased) {
        return None;
    }
    let bits = sign | ((biased as u64) << 52) | ((q as u64) & ((1u64 << 52) - 1));
    Some(f64::from_bits(bits))
}

fn round_to(n: u128, e: &mut i32, sticky: bool, width: u32) -> u128 {
    let len = 128 - n.leading_zeros();
    if len <= width {
        *e -= (width - len) as i32;
        return n << (width - len);
    }
    let shift = len - width;
    let drop = n & ((1u128 << shift) - 1);
    let half = 1u128 << (shift - 1);
    let mut q = n >> shift;
    *e += shift as i32;
    if drop > half || (drop == half && (sticky || q & 1 == 1)) {
        q += 1;
        if q >> width != 0 {
            q >>= 1;
            *e += 1;
        }
    }
    q
}

fn parts(x: f64) -> Option<(bool, u128, i32)> {
    let bits = x.to_bits();
    let e = ((bits >> 52) & 0x7ff) as i32;
    if e == 0x7ff || bits << 1 == 0 {
        return None;
    }
    if e == 0 {
        let m = bits & ((1u64 << 52) - 1);
        let shift = m.leading_zeros() as i32 - 11;
        return Some((bits >> 63 == 1, (m << shift) as u128, -1074 - shift));
    }
    Some((
        bits >> 63 == 1,
        ((bits & ((1u64 << 52) - 1)) | (1u64 << 52)) as u128,
        e - 1075,
    ))
}

fn dd_int(v: DD) -> Option<(bool, u128, i32, bool)> {
    let (neg, mh, eh) = parts(v.0)?;
    let base = eh - 64;
    let mut n = mh << 64;
    let mut sticky = false;
    if v.1 != 0.0 {
        let (nl, ml, el) = parts(v.1)?;
        let d = el - base;
        let s = if d >= 0 {
            ml << d
        } else if -d < 128 {
            sticky = ml & ((1u128 << -d) - 1) != 0;
            ml >> -d
        } else {
            sticky = true;
            0
        };
        if nl == neg {
            n += s;
        } else {
            n -= s;
            if sticky {
                n -= 1;
            }
        }
    }
    Some((neg, n, base, sticky))
}

fn round_dd(v: DD) -> Option<f64> {
    let (neg, n, e, sticky) = dd_int(v)?;
    round2(neg, n, e, sticky)
}

pub fn add(a: f64, b: f64) -> f64 {
    let s = a + b;
    if a == 0.0 || b == 0.0 || !s.is_finite() {
        return s;
    }
    let bb = s - a;
    let err = (a - (s - bb)) + (b - bb);
    if s == 0.0 {
        return s;
    }
    round_dd(DD(s, err)).unwrap_or(s)
}

pub fn sub(a: f64, b: f64) -> f64 {
    add(a, -b)
}

fn ftz(v: f64) -> f64 {
    if v != 0.0 && v.abs() < f64::MIN_POSITIVE {
        0.0f64.copysign(v)
    } else {
        v
    }
}

pub fn mul(a: f64, b: f64) -> f64 {
    ftz(match (parts(a), parts(b)) {
        (Some((sa, ma, ea)), Some((sb, mb, eb))) => {
            round2(sa != sb, ma * mb, ea + eb, false).unwrap_or(a * b)
        }
        _ => a * b,
    })
}

pub fn div(a: f64, b: f64) -> f64 {
    ftz(div_raw(a, b))
}

fn div_raw(a: f64, b: f64) -> f64 {
    match (parts(a), parts(b)) {
        (Some((sa, ma, ea)), Some((sb, mb, eb))) => {
            let n = ma << 74;
            let (q, r) = (n / mb, n % mb);
            round2(sa != sb, q, ea - eb - 74, r != 0).unwrap_or(a / b)
        }
        _ => a / b,
    }
}

pub fn sqrt(x: f64) -> f64 {
    let Some((false, m, e)) = parts(x) else {
        return x.sqrt();
    };
    let s = if (e - 74) % 2 == 0 { 74 } else { 75 };
    let n = m << s;
    let q = n.isqrt();
    let r = n - q * q;

    let n = (q << 2) | (((r > q) as u128) << 1) | (r != 0) as u128;
    round2(false, n, (e - s) / 2 - 2, false).unwrap_or(x.sqrt())
}

fn atan_dd(a: DD) -> DD {
    let one = DD::new(1.0);
    let mut b = a;
    for _ in 0..2 {
        let w = one.add(b.mul(b));
        let s0 = w.0.sqrt();
        let s = DD::new(s0).add(w.sub(DD::new(s0).mul_f(s0)).mul_f(0.5 / s0));
        b = b.div(one.add(s));
    }
    let b2 = b.mul(b);
    let mut term = b;
    let mut sum = b;
    for k in 1..40 {
        term = term.mul(b2).neg();
        let t = term.div(DD::new((2 * k + 1) as f64));
        sum = sum.add(t);
        if t.0.abs() < 1e-34 * sum.0 {
            break;
        }
    }
    sum.mul_f(4.0)
}

fn round64_dd(v: DD) -> DD {
    let Some((neg, n, base, sticky)) = dd_int(v) else {
        return v;
    };
    let mut e = base;
    let q = round_to(n, &mut e, sticky, 64);
    let hi = (q >> 11) << 11;
    let sign = if neg { -1.0 } else { 1.0 };
    let f = |m: u128, e: i32| sign * (m as f64) * 2f64.powi(e);
    DD(f(hi, e), f(q - hi, e))
}

pub fn atan(x: f64) -> f64 {
    let a = x.abs();

    if a < f64::MIN_POSITIVE {
        return x;
    }
    if NATIVE && a.is_finite() {
        return native::atan_abs(a).copysign(x);
    }
    atan_emu(x)
}

fn atan_emu(x: f64) -> f64 {
    let a = x.abs();
    if !(f64::MIN_POSITIVE..=f64::MAX).contains(&a) {
        return x.atan();
    }
    let r = if a < 1.0 {
        round_dd(atan_dd(DD::new(a)))
    } else if a == 1.0 {
        Some(std::f64::consts::FRAC_PI_4)
    } else {
        let t = round64_dd(atan_dd(DD::new(1.0).div(DD::new(a))));
        let half_pi = round64_dd(PI_DD.mul_f(0.5));
        round_dd(round64_dd(half_pi.sub(t)))
    };
    let r = r.unwrap_or(a.atan());
    if x < 0.0 {
        -r
    } else {
        r
    }
}

const PI_DD: DD = DD(std::f64::consts::PI, 1.2246467991473532e-16);
const LOG2E_DD: DD = DD(std::f64::consts::LOG2_E, 2.0355273740931033e-17);
const LOG10_2_DD: DD = DD(std::f64::consts::LOG10_2, -2.8037281277851704e-18);

fn expm1_dd(y: DD) -> DD {
    let mut term = y;
    let mut sum = y;
    for k in 2..40 {
        term = term.mul(y).div(DD::new(k as f64));
        sum = sum.add(term);
        if term.0.abs() < 1e-34 * sum.0.abs() {
            break;
        }
    }
    sum
}

fn tan_dd(a: DD) -> DD {
    let a2 = a.mul(a);
    let (mut sn, mut cs) = (a, DD::new(1.0));
    let (mut ts, mut tc) = (a, DD::new(1.0));
    for k in 1..30 {
        ts = ts
            .mul(a2)
            .div(DD::new(((2 * k) * (2 * k + 1)) as f64))
            .neg();
        tc = tc
            .mul(a2)
            .div(DD::new(((2 * k - 1) * (2 * k)) as f64))
            .neg();
        sn = sn.add(ts);
        cs = cs.add(tc);
        if tc.0.abs() < 1e-34 {
            break;
        }
    }
    sn.div(cs)
}

fn sqrt64(v: DD) -> DD {
    let s0 = v.0.sqrt();
    let s = DD::new(s0).add(v.sub(DD::new(s0).mul_f(s0)).mul_f(0.5 / s0));
    round64_dd(s)
}

pub fn exp(x: f64) -> f64 {
    if NATIVE && x.is_finite() {
        return native::exp(x);
    }
    exp_emu(x)
}

fn exp_emu(x: f64) -> f64 {
    if x == 0.0 || !(-750.0..=710.0).contains(&x) {
        return x.exp();
    }
    let t = round64_dd(round64_dd(LOG2E_DD).mul_f(x));
    let mut n = t.0.round_ties_even();
    let frac = t.0 - n;
    if frac.abs() == 0.5 && t.1 != 0.0 {
        n = if t.1 > 0.0 { t.0 + 0.5 } else { t.0 - 0.5 };
    }
    let f = t.add_f(-n);
    let fa = if f.0 < 0.0 { f.neg() } else { f };
    let g = round64_dd(expm1_dd(fa.mul(dd_ln2())));
    let mut h = round64_dd(g.add_f(1.0));
    if f.0 < 0.0 {
        h = round64_dd(DD::new(1.0).div(h));
    }

    dd_int(h)
        .and_then(|(neg, m, e, sticky)| round2(neg, m, e + n as i32, sticky))
        .unwrap_or(x.exp())
}

fn dd_ln2() -> DD {
    crate::functions::statdist::dd::LN2
}

fn ln_dd(x: f64) -> DD {
    let d = x - 1.0;
    if d.abs() >= 0.0625 {
        return DD::new(x).ln();
    }
    let u = DD::new(d).div(DD::new(2.0).add_f(d));
    let u2 = u.mul(u);
    let mut term = u;
    let mut sum = u;
    let mut k = 3.0;
    while k < 60.0 {
        term = term.mul(u2);
        let t = term.div(DD::new(k));
        if t.hi().abs() <= sum.hi().abs() * 1e-34 {
            break;
        }
        sum = sum.add(t);
        k += 2.0;
    }
    sum.mul_f(2.0)
}

fn yl2x(x: f64, c: DD) -> f64 {
    let l2 = ln_dd(x).div(dd_ln2());
    round_dd(round64_dd(c).mul(l2)).unwrap_or(f64::NAN)
}

pub fn ln(x: f64) -> f64 {
    if NATIVE && x > 0.0 && x.is_finite() {
        return native::ln(x);
    }
    ln_emu(x)
}

fn ln_emu(x: f64) -> f64 {
    if x == 1.0 {
        return 0.0;
    }
    if parts(x).is_none() || x < 0.0 {
        return x.ln();
    }
    let r = yl2x(x, dd_ln2());
    if r.is_nan() {
        x.ln()
    } else {
        r
    }
}

pub fn log10(x: f64) -> f64 {
    if NATIVE && x > 0.0 && x.is_finite() {
        return native::log10(x);
    }
    log10_emu(x)
}

fn log10_emu(x: f64) -> f64 {
    if x == 1.0 {
        return 0.0;
    }
    if parts(x).is_none() || x < 0.0 {
        return x.log10();
    }
    let r = yl2x(x, LOG10_2_DD);
    if r.is_nan() {
        x.log10()
    } else {
        r
    }
}

fn trig_core(a: f64) -> (DD, DD, u64) {
    let p = round64_dd(PI_DD.mul_f(0.25));
    let mut q = (DD::new(a).div(p)).0.floor();
    let mut r = DD::new(a).sub(p.mul_f(q));
    if r.0 < 0.0 {
        q -= 1.0;
        r = r.add(p);
    } else if r.sub(p).0 >= 0.0 {
        q += 1.0;
        r = r.sub(p);
    }
    let q = q as u64;
    let red = if q & 1 == 1 { round64_dd(p.sub(r)) } else { r };
    let t = round64_dd(tan_dd(red));
    let one = DD::new(1.0);
    let (a_, b_) = if ((q >> 1) ^ q) & 1 == 0 {
        (one, t)
    } else {
        (t, one)
    };
    (a_, b_, q)
}

fn sin_cos(x: f64, cos: bool) -> f64 {
    let a = x.abs();
    if a <= f64::from_bits(0x3E4F_FFFF_FFFF_FFFF) {
        return if cos { 1.0 } else { x };
    }
    if NATIVE && a < TRIG_MAX {
        return if cos {
            native::cos_abs(a)
        } else {
            flip(native::sin_abs(a), x)
        };
    }
    sin_cos_emu(x, cos)
}

fn sin_cos_emu(x: f64, cos: bool) -> f64 {
    let a = x.abs();
    if a <= f64::from_bits(0x3E4F_FFFF_FFFF_FFFF) {
        return if cos { 1.0 } else { x };
    }
    let (ca, cb, q) = trig_core(a);
    let a2 = round64_dd(ca.mul(ca));
    let b2 = round64_dd(cb.mul(cb));
    let sum = round64_dd(a2.add(b2));
    let d = round64_dd(if cos { a2 } else { b2 }.div(sum));
    let v = round_dd(sqrt64(d)).unwrap_or(f64::NAN);
    let neg = if cos {
        ((q >> 2) ^ (q >> 1)) & 1 == 1
    } else {
        ((q >> 2) & 1 == 1) != (x < 0.0)
    };
    if neg {
        -v
    } else {
        v
    }
}

fn flip(v: f64, x: f64) -> f64 {
    if x.is_sign_negative() {
        -v
    } else {
        v
    }
}

pub fn sin(x: f64) -> f64 {
    sin_cos(x, false)
}

pub fn cos(x: f64) -> f64 {
    sin_cos(x, true)
}

pub fn tan(x: f64) -> f64 {
    let a = x.abs();
    if NATIVE && a < TRIG_MAX {
        return flip(native::tan_abs(a), x);
    }
    tan_emu(x)
}

fn tan_emu(x: f64) -> f64 {
    let a = x.abs();
    if !(f64::MIN_POSITIVE..=f64::MAX).contains(&a) {
        return x.tan();
    }
    let (ca, cb, q) = trig_core(a);
    let ca = if (q >> 1) & 1 == 1 { ca.neg() } else { ca };
    let v = round_dd(cb.div(ca)).unwrap_or(x.tan());
    if x < 0.0 {
        -v
    } else {
        v
    }
}

pub fn expm1(x: f64) -> f64 {
    let u = exp(x);
    if u == 1.0 {
        return x;
    }
    let em1 = add(u, -1.0);
    if em1 == -1.0 {
        return -1.0;
    }
    div(mul(x, em1), ln(u))
}

#[derive(Default)]
pub struct MpFlag(pub u8);

impl MpFlag {
    fn ovf(&mut self, v: f64) -> f64 {
        if v.is_finite() {
            v
        } else {
            self.0 = 1;
            0.0
        }
    }
    pub fn add(&mut self, a: f64, b: f64) -> f64 {
        let v = add(a, b);
        self.ovf(v)
    }
    pub fn sub(&mut self, a: f64, b: f64) -> f64 {
        let v = sub(a, b);
        self.ovf(v)
    }

    pub fn exp(&mut self, x: f64) -> f64 {
        let v = exp(x);
        self.ovf(v)
    }
    pub fn mul(&mut self, a: f64, b: f64) -> f64 {
        let v = mul(a, b);
        self.ovf(v)
    }
    pub fn div(&mut self, a: f64, b: f64) -> f64 {
        if b == 0.0 || b.abs() < f64::MIN_POSITIVE {
            self.0 = 4;
            return 0.0;
        }
        let v = div(a, b);
        self.ovf(v)
    }
    pub fn sqrt(&mut self, x: f64) -> f64 {
        let v = sqrt(x);
        if !v.is_finite() {
            self.0 = 8;
        }
        v
    }

    pub fn result(&self, v: f64) -> Result<f64, crate::value::XlError> {
        use crate::value::XlError;
        match self.0 {
            0 => Ok(v),
            4 => Err(XlError::DIV0),
            _ => Err(XlError::NUM),
        }
    }
}

pub fn chk(v: f64) -> Result<f64, crate::value::XlError> {
    if v.is_finite() {
        Ok(v)
    } else {
        Err(crate::value::XlError::NUM)
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn sqrt_double_rounds() {
        assert_eq!(sqrt(0.9999999999999999), 1.0);
        assert_eq!(sqrt(4.0), 2.0);
        assert_eq!(sqrt(2.0), 2f64.sqrt());
    }

    #[test]
    fn sin_cos_of_pi() {
        let b = |x: f64| x.to_bits();
        assert_eq!(b(sin(std::f64::consts::PI)), 0x3CA1A80000000000);
        assert_eq!(cos(std::f64::consts::PI), -1.0);
    }

    #[test]
    fn subnormals_as_hardware() {
        let b = |x: f64| x.to_bits();
        assert_eq!(b(exp(-711.5296988064689)), 0x0000b27b4e8ea40f);
        assert_eq!(b(exp(-710.2668257538389)), 0x000277089331d2d3);
        assert_eq!(b(exp(-745.1)), 1);
        assert_eq!(exp(-745.2), 0.0);
        let x = f64::from_bits(0x000e7eeb2194d1b9);
        assert_eq!(b(ln(x)), 0xc08623f61134afe6);
        let x = f64::from_bits(0x000df8a14c5dac1f);
        assert_eq!(b(sqrt(x)), 0x1ffde7080fa310a2);
    }

    #[test]
    fn ln_fyl2x_rounding() {
        let r = ln(174347.1506034721).to_bits();
        if NATIVE {
            assert!(r == 0x4028233a3d600375 || r == 0x4028233a3d600376);
        } else {
            assert_eq!(r, 0x4028233a3d600376);
        }
    }

    #[cfg(target_arch = "x86_64")]
    fn rand_dbl(state: &mut u64, lo: i32, hi: i32) -> f64 {
        let mut next = || {
            *state = state.wrapping_add(0x9E37_79B9_7F4A_7C15);
            let mut z = *state;
            z = (z ^ (z >> 30)).wrapping_mul(0xBF58_476D_1CE4_E5B9);
            z = (z ^ (z >> 27)).wrapping_mul(0x94D0_49BB_1331_11EB);
            z ^ (z >> 31)
        };
        let e = lo + (next() % ((hi - lo + 1) as u64)) as i32;
        f64::from_bits((((e + 1023) as u64) << 52) | (next() & ((1u64 << 52) - 1)))
    }

    #[cfg(target_arch = "x86_64")]
    #[test]
    fn arith_is_native() {
        let mut st = 1u64;
        for _ in 0..200_000 {
            let a = rand_dbl(&mut st, -60, 60);
            let b = rand_dbl(&mut st, -60, 60);
            let b = if st & 1 == 0 { -b } else { b };
            assert_eq!(add(a, b).to_bits(), native::arith(b'+', a, b).to_bits());
            assert_eq!(mul(a, b).to_bits(), native::arith(b'*', a, b).to_bits());
            assert_eq!(div(a, b).to_bits(), native::arith(b'/', a, b).to_bits());
            assert_eq!(sqrt(a).to_bits(), native::arith(b'q', a, 0.0).to_bits());
        }
    }

    #[cfg(target_arch = "x86_64")]
    #[test]
    fn native_agrees_with_emulation() {
        fn cw_sw() -> (u16, u16) {
            let (mut cw, mut sw) = (0u16, 0u16);

            unsafe {
                std::arch::asm!("fnstcw word ptr [{c}]", "fnstsw word ptr [{s}]",
                    c = in(reg) &mut cw, s = in(reg) &mut sw, options(nostack));
            }
            (cw, sw)
        }
        let before = cw_sw();
        let mut st = 7u64;
        let mut diff = 0u32;
        let n = 100_000;
        for _ in 0..n {
            let x = rand_dbl(&mut st, -20, 20);
            let pairs = [
                (native::ln(x), ln_emu(x)),
                (native::log10(x), log10_emu(x)),
                (native::exp(x.min(700.0)), exp_emu(x.min(700.0))),
                (native::atan_abs(x), atan_emu(x)),
                (flip(native::sin_abs(x), x), sin_cos_emu(x, false)),
                (native::cos_abs(x), sin_cos_emu(x, true)),
                (native::tan_abs(x), tan_emu(x)),
            ];
            for (h, e) in pairs {
                assert!((h - e).abs() <= e.abs() * 2.3e-16, "{x:e}: {h:e} vs {e:e}");
                diff += (h != e) as u32;
            }
        }
        assert!(diff < 7 * n / 1000, "{diff}");
        assert_eq!(cw_sw(), before);
    }

    #[test]
    fn ops_match_ieee_mostly() {
        let mut diff = 0;
        let mut x = 0.1234567f64;
        for _ in 0..10000 {
            x = (x * 3.7 + 0.31).fract() + 0.5;
            let y = (x * 7.1).fract() + 0.25;
            for (a, b) in [(add(x, y), x + y), (mul(x, y), x * y), (div(x, y), x / y)] {
                assert!((a - b).abs() <= b.abs() * 2.3e-16);
                diff += (a != b) as u32;
            }
            assert!((atan(x) - x.atan()).abs() <= x.atan() * 2.3e-16);
        }
        assert!(diff < 100, "{diff}");
    }
}
