use crate::functions::statdist::dd::DD;

fn round2(neg: bool, n: u128, mut e: i32, sticky: bool) -> Option<f64> {
    if n == 0 {
        return Some(if neg { -0.0 } else { 0.0 });
    }
    let q = round_to(n, &mut e, sticky, 64);
    let q = round_to(q, &mut e, false, 53);
    let biased = e + 52 + 1023;
    if !(1..=2046).contains(&biased) {
        return None;
    }
    let bits = ((neg as u64) << 63) | ((biased as u64) << 52) | ((q as u64) & ((1u64 << 52) - 1));
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
    if e == 0 || e == 0x7ff {
        return None;
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
    if a == 0.0 || !a.is_finite() || parts(a).is_none() {
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

fn scale(v: DD, n: i32) -> Option<DD> {
    let (n1, n2) = (n / 2, n - n / 2);
    let (s1, s2) = (2f64.powi(n1), 2f64.powi(n2));
    let r = DD(v.0 * s1 * s2, v.1 * s1 * s2);
    (r.0.abs() >= f64::MIN_POSITIVE && r.0.is_finite()).then_some(r)
}

pub fn exp(x: f64) -> f64 {
    if x == 0.0 || !x.is_finite() || x.abs() > 709.0 {
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
    scale(h, n as i32).and_then(round_dd).unwrap_or(x.exp())
}

fn dd_ln2() -> DD {
    crate::functions::statdist::dd::LN2
}

fn yl2x(x: f64, c: DD) -> f64 {
    let l2 = DD::new(x).ln().div(dd_ln2());
    round_dd(round64_dd(c).mul(l2)).unwrap_or(f64::NAN)
}

pub fn ln(x: f64) -> f64 {
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

pub fn sin(x: f64) -> f64 {
    sin_cos(x, false)
}

pub fn cos(x: f64) -> f64 {
    sin_cos(x, true)
}

pub fn tan(x: f64) -> f64 {
    let a = x.abs();
    if a == 0.0 || parts(a).is_none() {
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
