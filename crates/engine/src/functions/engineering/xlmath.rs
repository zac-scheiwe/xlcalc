const PIO2_HI: f64 = f64::from_bits(0x3FF921FB54442D18);
const PIO2_LO: f64 = f64::from_bits(0x3C91A80000000000);

pub const TRIG_LIMIT: f64 = 134_217_728.0;

fn reduce(x: f64) -> (i64, f64, f64) {
    if x.abs() < 0.785_398_163_397_448_3 {
        return (0, x, 0.0);
    }
    let k = (x / PIO2_HI).round();
    let p = k * PIO2_HI;
    let e = k.mul_add(PIO2_HI, -p);
    let s = x - p;
    let t = e + k * PIO2_LO;
    let hi = s - t;
    let lo = (s - hi) - t;
    (k as i64, hi, lo)
}

#[derive(Clone, Copy)]
struct Dd(f64, f64);

fn two_sum(a: f64, b: f64) -> Dd {
    let s = a + b;
    let bb = s - a;
    Dd(s, (a - (s - bb)) + (b - bb))
}

impl Dd {
    fn add(self, o: Dd) -> Dd {
        let s = two_sum(self.0, o.0);
        let t = s.1 + self.1 + o.1;
        two_sum(s.0, t)
    }

    fn mul(self, o: Dd) -> Dd {
        let p = self.0 * o.0;
        let e = self.0.mul_add(o.0, -p) + (self.0 * o.1 + self.1 * o.0);
        two_sum(p, e)
    }

    fn scale(self, f: f64) -> Dd {
        let p = self.0 * f;
        let e = self.0.mul_add(f, -p) + self.1 * f;
        two_sum(p, e)
    }

    fn neg(self) -> Dd {
        Dd(-self.0, -self.1)
    }

    fn div(self, o: Dd) -> Dd {
        let q1 = self.0 / o.0;
        let r = self.add(o.scale(-q1));
        let q2 = r.0 / o.0;
        let r2 = r.add(o.scale(-q2));
        two_sum(q1, q2).add(Dd(r2.0 / o.0, 0.0))
    }

    fn sqrt(self) -> Dd {
        let s = self.0.sqrt();
        let r = Dd(s, 0.0);

        r.add(self.add(r.mul(r).neg()).scale(0.5 / s))
    }
}

const PIO2_DD: Dd = Dd(1.5707963267948966, 6.123233995736766e-17);

fn atan_small(t: Dd) -> Dd {
    let one = Dd(1.0, 0.0);
    let mut t = t;
    for _ in 0..3 {
        t = t.div(one.add(one.add(t.mul(t)).sqrt()));
    }
    let t2 = t.mul(t);
    let (mut term, mut sum) = (t, t);
    for k in 1..20 {
        term = term.mul(t2).neg();
        sum = sum.add(term.div(Dd((2 * k + 1) as f64, 0.0)));
    }
    sum.scale(8.0)
}

pub fn atan(x: f64) -> f64 {
    if !x.is_finite() || x == 0.0 {
        return x.atan();
    }
    let a = x.abs();
    let r = if a > 1e300 {
        PIO2_DD.add(Dd(-1.0 / a, 0.0))
    } else if a > 1.0 {
        PIO2_DD.add(atan_small(Dd(1.0, 0.0).div(Dd(a, 0.0))).neg())
    } else {
        atan_small(Dd(a, 0.0))
    };
    (r.0 + r.1).copysign(x)
}

fn sincos_dd(hi: f64, lo: f64) -> (f64, f64) {
    let (s, c) = sincos_dd2(hi, lo);
    (s.0 + s.1, c.0 + c.1)
}

fn sincos_dd2(hi: f64, lo: f64) -> (Dd, Dd) {
    let r = Dd(hi, lo);
    let r2 = r.mul(r);

    let (mut st, mut ct) = (Dd(1.0, 0.0), Dd(1.0, 0.0));
    let (mut s, mut c) = (Dd(1.0, 0.0), Dd(1.0, 0.0));
    for k in 1..16 {
        let k = k as f64;

        st = st.mul(r2).div(Dd(-((2.0 * k) * (2.0 * k + 1.0)), 0.0));
        ct = ct.mul(r2).div(Dd(-((2.0 * k - 1.0) * (2.0 * k)), 0.0));
        s = s.add(st);
        c = c.add(ct);
    }
    (s.mul(r), c)
}

fn dd_div(a: Dd, b: Dd) -> f64 {
    let q1 = a.0 / b.0;
    let r = a.add(b.scale(-q1));
    let q2 = r.0 / b.0;
    let r2 = r.add(b.scale(-q2));
    let q3 = r2.0 / b.0;
    two_sum(q1, q2).add(Dd(q3, 0.0)).0
}

pub fn sin(x: f64) -> f64 {
    let (k, hi, lo) = reduce(x);
    let (s, c) = sincos_dd(hi, lo);
    match k.rem_euclid(4) {
        0 => s,
        1 => c,
        2 => -s,
        _ => -c,
    }
}

pub fn cos(x: f64) -> f64 {
    let (k, hi, lo) = reduce(x);
    let (s, c) = sincos_dd(hi, lo);
    match k.rem_euclid(4) {
        0 => c,
        1 => -s,
        2 => -c,
        _ => s,
    }
}

pub fn tan(x: f64) -> f64 {
    let (k, hi, lo) = reduce(x);
    let (s, c) = sincos_dd2(hi, lo);
    if k.rem_euclid(2) == 0 {
        dd_div(s, c)
    } else {
        -dd_div(c, s)
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn x87_reduction() {
        assert_eq!(sin(std::f64::consts::PI).to_bits(), 0x3CA1A80000000000);
        assert_eq!(
            sin(2.0 * std::f64::consts::PI).to_bits(),
            0xBCB1A80000000000
        );
        assert_eq!(
            cos(std::f64::consts::FRAC_PI_2).to_bits(),
            0x3C91A80000000000
        );
        assert_eq!(cos(std::f64::consts::PI), -1.0);
        assert_eq!(sin(std::f64::consts::FRAC_PI_2), 1.0);
    }
}
