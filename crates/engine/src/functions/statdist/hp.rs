use super::dd::DD;
use super::special::{igam, igamc, incbet2, lgamma_dd, lgamma_xl, MACHEP};

const ONE: DD = DD(1.0, 0.0);
const MAX_ITER: usize = 200_000;
const TOL: f64 = 1e-33;

fn gamma_prefactor(a: f64, x: f64) -> DD {
    DD::new(x)
        .ln()
        .mul_f(a)
        .sub(DD::new(x))
        .sub(lgamma_dd(a))
        .exp()
}

fn gamma_series(a: f64, x: f64) -> Option<DD> {
    let mut sum = ONE;
    let mut term = ONE;
    let mut ap = DD::new(a);
    for _ in 0..MAX_ITER {
        ap = ap.add_f(1.0);
        term = term.mul_f(x).div(ap);
        sum = sum.add(term);
        if term.0.abs() <= TOL * sum.0.abs() {
            return Some(sum);
        }
    }
    None
}

fn gamma_cf(a: f64, x: f64) -> Option<DD> {
    let tiny = DD::new(1e-300);
    let mut b = DD::new(x).add_f(1.0).sub(DD::new(a));
    let mut c = DD::new(1e300);
    let mut d = b.recip();
    let mut h = d;
    for i in 1..MAX_ITER {
        let fi = i as f64;
        let an = DD::new(fi).sub(DD::new(a)).mul_f(-fi);
        b = b.add_f(2.0);
        d = an.mul(d).add(b);
        if d.0.abs() < 1e-300 {
            d = tiny;
        }
        c = b.add(an.div(c));
        if c.0.abs() < 1e-300 {
            c = tiny;
        }
        d = d.recip();
        let del = d.mul(c);
        h = h.mul(del);
        if del.add_f(-1.0).0.abs() <= TOL {
            return Some(h);
        }
    }
    None
}

pub fn gamma_pq(a: f64, x: f64) -> (f64, f64) {
    if a == 0.5 && x > 0.0 && x.is_finite() {
        let s = x.sqrt();
        return (super::special::erf(s), super::special::erfc(s));
    }
    let (p, q) = gamma_pq_raw(a, x, true);
    (
        crate::number::flush_subnormal(p),
        crate::number::flush_subnormal(q),
    )
}

pub fn xl_gamma_prefactor(a: f64, x: f64) -> f64 {
    DD::new(a * x.ln())
        .sub(DD::new(x))
        .round64()
        .sub(DD::new(lgamma_xl(a + 1.0)))
        .round64()
        .exp()
        .hi()
}

fn xl_series_p(a: f64, x: f64) -> Option<f64> {
    if a == 0.5 || a == 1.0 || !(a > 0.0 && a <= 30.0) || !(x <= 1.0) {
        return None;
    }
    let (mut r, mut c, mut ans) = (a, 1.0, 1.0);
    loop {
        r += 1.0;
        c *= x / r;
        ans += c;
        if !(c / ans > MACHEP) {
            break;
        }
    }
    let e = xl_gamma_prefactor(a, x);
    if !(e > 0.0 && e.is_finite()) {
        return None;
    }
    let p = e * ans;
    if p.is_finite() && p > 1e-300 {
        Some(p)
    } else {
        None
    }
}

fn gamma_pq_raw(a: f64, x: f64, xl: bool) -> (f64, f64) {
    if x <= 0.0 {
        return (0.0, 1.0);
    }
    if x.is_infinite() {
        return (1.0, 0.0);
    }
    if a > 1e7 {
        return (igam(a, x), igamc(a, x));
    }
    if x < a + 1.0 {
        if let Some(p) = xl.then(|| xl_series_p(a, x)).flatten() {
            let q = gamma_pq_raw(a, x, false).1;
            return (p, q);
        }
        if let Some(s) = gamma_series(a, x) {
            let p = gamma_prefactor(a, x).mul(s).div(DD::new(a));
            return (p.hi(), ONE.sub(p).hi());
        }
    } else if let Some(h) = gamma_cf(a, x) {
        let q = gamma_prefactor(a, x).mul(h);
        return (ONE.sub(q).hi(), q.hi());
    }
    (igam(a, x), igamc(a, x))
}

pub fn gamma_p_cr(a: f64, x: f64) -> f64 {
    crate::number::flush_subnormal(gamma_pq_raw(a, x, false).0)
}

pub fn gamma_q_cr(a: f64, x: f64) -> f64 {
    crate::number::flush_subnormal(gamma_pq_raw(a, x, false).1)
}

fn beta_cf(a: f64, b: f64, x: DD) -> Option<DD> {
    let tiny = DD::new(1e-300);
    let qab = DD::new(a).add(DD::new(b));
    let qap = DD::new(a).add_f(1.0);
    let qam = DD::new(a).add_f(-1.0);
    let mut c = ONE;
    let mut d = ONE.sub(qab.mul(x).div(qap));
    if d.0.abs() < 1e-300 {
        d = tiny;
    }
    d = d.recip();
    let mut h = d;
    for m in 1..MAX_ITER {
        let fm = m as f64;
        let m2 = 2.0 * fm;

        let num = DD::new(b).add_f(-fm).mul_f(fm).mul(x);
        let den = qam.add_f(m2).mul(DD::new(a).add_f(m2));
        let aa = num.div(den);
        d = ONE.add(aa.mul(d));
        if d.0.abs() < 1e-300 {
            d = tiny;
        }
        c = ONE.add(aa.div(c));
        if c.0.abs() < 1e-300 {
            c = tiny;
        }
        d = d.recip();
        h = h.mul(d).mul(c);

        let num = DD::new(a).add_f(fm).mul(qab.add_f(fm)).mul(x).neg();
        let den = DD::new(a).add_f(m2).mul(qap.add_f(m2));
        let aa = num.div(den);
        d = ONE.add(aa.mul(d));
        if d.0.abs() < 1e-300 {
            d = tiny;
        }
        c = ONE.add(aa.div(c));
        if c.0.abs() < 1e-300 {
            c = tiny;
        }
        d = d.recip();
        let del = d.mul(c);
        h = h.mul(del);
        if del.add_f(-1.0).0.abs() <= TOL {
            return Some(h);
        }
    }
    None
}

pub fn beta_inc(a: f64, b: f64, x: f64, xc_hint: f64) -> f64 {
    if let Some((w, _)) = super::ibeta::xl_ibeta(x, crate::x87::sub(1.0, x), a, b) {
        return crate::number::flush_subnormal(w);
    }
    beta_inc_dd(a, b, x, xc_hint)
}

pub fn beta_inc_xy(a: f64, b: f64, x: f64, y: f64) -> (f64, f64) {
    use crate::number::flush_subnormal as fl;
    if let Some((w, w1)) = super::ibeta::xl_ibeta(x, y, a, b) {
        return (fl(w), fl(w1));
    }
    (beta_inc_dd(a, b, x, y), beta_inc_dd(b, a, y, x))
}

fn beta_inc_dd(a: f64, b: f64, x: f64, xc_hint: f64) -> f64 {
    if !(x > 0.0) {
        return 0.0;
    }
    if x >= 1.0 {
        return 1.0;
    }
    if a > 1e7 || b > 1e7 {
        return incbet2(a, b, x, xc_hint);
    }
    let xd = DD::new(x);
    let xc = ONE.sub(xd);
    let lbeta = lgamma_dd(a + b).sub(lgamma_dd(a)).sub(lgamma_dd(b));
    let pre = xd.ln().mul_f(a).add(xc.ln().mul_f(b)).add(lbeta).exp();
    if x < (a + 1.0) / (a + b + 2.0) {
        if let Some(h) = beta_cf(a, b, xd) {
            return pre.mul(h).div(DD::new(a)).hi();
        }
    } else if let Some(h) = beta_cf(b, a, xc) {
        return ONE.sub(pre.mul(h).div(DD::new(b))).hi();
    }
    incbet2(a, b, x, xc_hint)
}
