#[derive(Clone, Copy, Debug)]
struct DD {
    hi: f64,
    lo: f64,
}

#[inline]
fn two_sum(a: f64, b: f64) -> DD {
    let s = a + b;
    let bb = s - a;
    let e = (a - (s - bb)) + (b - bb);
    DD { hi: s, lo: e }
}

#[inline]
fn quick_two_sum(a: f64, b: f64) -> DD {
    let s = a + b;
    DD {
        hi: s,
        lo: b - (s - a),
    }
}

#[inline]
fn two_prod(a: f64, b: f64) -> DD {
    let p = a * b;
    DD {
        hi: p,
        lo: a.mul_add(b, -p),
    }
}

impl DD {
    #[inline]
    fn from(x: f64) -> DD {
        DD { hi: x, lo: 0.0 }
    }
    #[inline]
    fn add(self, o: DD) -> DD {
        let s = two_sum(self.hi, o.hi);
        let t = two_sum(self.lo, o.lo);
        let s = quick_two_sum(s.hi, s.lo + t.hi);
        quick_two_sum(s.hi, s.lo + t.lo)
    }
    #[inline]
    fn neg(self) -> DD {
        DD {
            hi: -self.hi,
            lo: -self.lo,
        }
    }
    #[inline]
    fn sub(self, o: DD) -> DD {
        self.add(o.neg())
    }
    #[inline]
    fn mul(self, o: DD) -> DD {
        let p = two_prod(self.hi, o.hi);
        quick_two_sum(p.hi, p.lo + (self.hi * o.lo + self.lo * o.hi))
    }
    #[inline]
    fn mul_f(self, f: f64) -> DD {
        let p = two_prod(self.hi, f);
        quick_two_sum(p.hi, p.lo + self.lo * f)
    }
    fn div(self, o: DD) -> DD {
        let q1 = self.hi / o.hi;
        let r = self.sub(o.mul_f(q1));
        let q2 = r.hi / o.hi;
        let r = r.sub(o.mul_f(q2));
        let q3 = r.hi / o.hi;
        quick_two_sum(q1, q2).add(DD::from(q3))
    }
    #[inline]
    fn scale(self, k: f64) -> DD {
        DD {
            hi: self.hi * k,
            lo: self.lo * k,
        }
    }
}

const LN2: DD = DD {
    hi: std::f64::consts::LN_2,
    lo: 2.3190468138462996e-17,
};

fn ln_dd(x: f64) -> DD {
    let bits = x.to_bits();
    let mut e = ((bits >> 52) & 0x7ff) as i64;
    let mut m;
    if e == 0 {
        let y = x * (1u64 << 54) as f64;
        let b = y.to_bits();
        e = ((b >> 52) & 0x7ff) as i64 - 54;
        m = f64::from_bits((b & 0x000f_ffff_ffff_ffff) | 0x3ff0_0000_0000_0000);
    } else {
        m = f64::from_bits((bits & 0x000f_ffff_ffff_ffff) | 0x3ff0_0000_0000_0000);
    }
    e -= 1023;
    if m >= 1.5 {
        m *= 0.5;
        e += 1;
    }

    let num = DD::from(m - 1.0);
    let den = two_sum(m, 1.0);
    let s = num.div(den);
    let s2 = s.mul(s);
    let mut term = s;
    let mut sum = s;
    let mut k = 3.0;
    loop {
        term = term.mul(s2);
        let t = term.div(DD::from(k));
        sum = sum.add(t);
        if t.hi.abs() <= 1e-34 * sum.hi.abs() || k > 400.0 {
            break;
        }
        k += 2.0;
    }
    let lm = sum.scale(2.0);
    LN2.mul_f(e as f64).add(lm)
}

pub fn ln(x: f64) -> f64 {
    if x.is_nan() || x < 0.0 {
        return f64::NAN;
    }
    if x == 0.0 {
        return f64::NEG_INFINITY;
    }
    if x.is_infinite() {
        return x;
    }
    if x == 1.0 {
        return 0.0;
    }
    let r = ln_dd(x);
    r.hi + r.lo
}

fn exp_small_dd(r: DD) -> DD {
    let rr = r.scale(1.0 / 1024.0);
    let mut sum = DD::from(1.0);
    let mut term = DD::from(1.0);
    let mut i = 1.0;
    loop {
        term = term.mul(rr).div(DD::from(i));
        sum = sum.add(term);
        if term.hi.abs() < 1e-36 || i > 200.0 {
            break;
        }
        i += 1.0;
    }
    for _ in 0..10 {
        sum = sum.mul(sum);
    }
    sum
}

fn exp_dd(x: f64) -> (DD, i32) {
    let k = (x / LN2.hi).round();

    let r = DD::from(x).sub(LN2.mul_f(k));
    (exp_small_dd(r), k as i32)
}

fn ldexp(x: f64, k: i32) -> f64 {
    let mut x = x;
    let mut k = k;
    while k > 1000 {
        x *= f64::from_bits(((1023 + 1000) as u64) << 52);
        k -= 1000;
    }
    while k < -1000 {
        x *= f64::from_bits(((1023 - 1000) as u64) << 52);
        k += 1000;
    }
    x * f64::from_bits(((1023 + k) as u64) << 52)
}

pub fn exp(x: f64) -> f64 {
    if x.is_nan() {
        return x;
    }
    if x > 709.8 {
        return f64::INFINITY;
    }
    if x < -745.2 {
        return 0.0;
    }
    if x == 0.0 {
        return 1.0;
    }
    let (d, k) = exp_dd(x);
    if k >= -1021 {
        ldexp(d.hi + d.lo, k)
    } else {
        ldexp(d.hi, k) + ldexp(d.lo, k)
    }
}

pub fn powi(mut a: f64, mut n: u64) -> f64 {
    let mut r = 1.0;
    while n != 0 {
        if n & 1 == 1 {
            r *= a;
        }
        n >>= 1;
        if n != 0 {
            a *= a;
        }
    }
    r
}

pub fn pow(a: f64, b: f64) -> f64 {
    if b == b.trunc() && b.abs() < 1.8e19 {
        let n = b.abs() as u64;
        let p = powi(a, n);
        return if b < 0.0 { 1.0 / p } else { p };
    }
    if a < 0.0 {
        return f64::NAN;
    }
    if a == 0.0 {
        return if b > 0.0 { 0.0 } else { f64::INFINITY };
    }
    if b < 0.0 {
        1.0 / exp(-b * ln(a))
    } else {
        exp(b * ln(a))
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn integral_powers() {
        assert_eq!(pow(2.0, 10.0), 1024.0);
        assert_eq!(pow(2.0, -2.0), 0.25);
        assert_eq!(pow(1.5, 0.0), 1.0);
        assert!(pow(-8.0, 0.5).is_nan());
        assert_eq!(pow(-2.0, 3.0), -8.0);
    }

    #[test]
    fn correctly_rounded() {
        assert_eq!(ln(1.05), 0.04879016416943205);
        assert_eq!(ln(1.112), 0.10616019582839072);
        assert_eq!(ln(0.917), -0.08664780672567217);
        assert_eq!(ln(2.0), std::f64::consts::LN_2);
        assert_eq!(exp(1.0), std::f64::consts::E);
        assert_eq!(exp(0.0), 1.0);
        assert!(exp(-745.0) > 0.0);
    }
}
