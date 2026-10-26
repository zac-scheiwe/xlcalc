use std::sync::Arc;

use super::hp;
use super::special::{self as sp, lgamma, norm_cdf, norm_pdf};
use super::*;
use crate::value::Array;
use crate::x87;

fn flag(a: &[Value], i: usize) -> R<bool> {
    boolean(&a[i])
}

fn coerce_all(ctx: &EvalCtx, a: &[Value], spec: &str) -> R<()> {
    for (v, k) in a.iter().zip(spec.bytes()) {
        match k {
            b'f' => {
                boolean(v)?;
            }
            _ => {
                num(ctx, v)?;
            }
        }
    }
    Ok(())
}

fn int(ctx: &EvalCtx, a: &[Value], i: usize) -> R<f64> {
    Ok(num(ctx, &a[i])?.trunc())
}

fn flush(x: f64) -> f64 {
    crate::number::flush_subnormal(x)
}

fn atp_scalar(ctx: &EvalCtx, v: &Value) -> R<f64> {
    match v {
        Value::Number(n) => Ok(*n),
        Value::Blank => Ok(0.0),
        Value::Text(_) => ctx.to_number(v),
        Value::Error(e) => Err(*e),
        _ => Err(XlError::Value),
    }
}

enum AtpArg {
    One(R<f64>),
    Many(Arc<Array>),
}

fn atp_arg(ctx: &mut EvalCtx, op: &Operand) -> AtpArg {
    match op {
        Operand::Missing => AtpArg::One(Err(XlError::NA)),
        Operand::Val(Value::Array(a)) => AtpArg::Many(a.clone()),
        Operand::Val(v) => AtpArg::One(atp_scalar(ctx, v)),
        Operand::Ref(r) => match r.single() {
            Some(a) if a.rect.is_cell() => {
                let a = *a;
                let v = ctx.cell_value(a.sheet, a.rect.r0, a.rect.c0);
                AtpArg::One(atp_scalar(ctx, &v))
            }
            _ => AtpArg::One(Err(XlError::Value)),
        },
    }
}

fn atp_lift(ctx: &mut EvalCtx, ops: &[Operand], f: impl Fn(&[f64]) -> R<f64>) -> Operand {
    let args: Vec<AtpArg> = ops.iter().map(|o| atp_arg(ctx, o)).collect();
    let mut shape: Option<(u32, u32)> = None;
    for a in &args {
        if let AtpArg::Many(arr) = a {
            let (r, c) = shape.unwrap_or((1, 1));
            shape = Some((r.max(arr.rows), c.max(arr.cols)));
        }
    }
    let eval = |vals: Vec<R<f64>>| -> Value {
        let mut xs = Vec::with_capacity(vals.len());
        for v in vals {
            match v {
                Ok(x) => xs.push(x),
                Err(e) => return Value::Error(e),
            }
        }
        fin(f(&xs))
    };
    match shape {
        None => {
            let vals = args
                .iter()
                .map(|a| {
                    if let AtpArg::One(v) = a {
                        *v
                    } else {
                        unreachable!()
                    }
                })
                .collect();
            Operand::Val(eval(vals))
        }
        Some((rows, cols)) => {
            let out = Array::from_fn(rows, cols, |r, c| {
                let vals = args
                    .iter()
                    .map(|a| match a {
                        AtpArg::One(v) => *v,
                        AtpArg::Many(arr) => atp_scalar(ctx, &arr.broadcast_get(r, c)),
                    })
                    .collect();
                eval(vals)
            });
            Operand::Val(out.into_value())
        }
    }
}

pub fn erf_fn(ctx: &mut EvalCtx, o: &[Operand]) -> Operand {
    atp_lift(ctx, o, |x| {
        Ok(if x.len() == 2 {
            sp::erf(x[1]) - sp::erf(x[0])
        } else {
            sp::erf(x[0])
        })
    })
}

pub fn erf_precise(ctx: &mut EvalCtx, o: &[Operand]) -> Operand {
    atp_lift(ctx, o, |x| Ok(sp::erf(x[0])))
}

pub fn erfc_fn(ctx: &mut EvalCtx, o: &[Operand]) -> Operand {
    atp_lift(ctx, o, |x| Ok(sp::erfc(x[0])))
}

pub fn gamma_fn(ctx: &mut EvalCtx, a: &[Value]) -> Value {
    fin((|| {
        let x = num(ctx, &a[0])?;
        let g = sp::gamma_xl(x);
        if !g.is_finite() {
            return Err(XlError::Num);
        }
        Ok(flush(g))
    })())
}

pub fn gammaln(ctx: &mut EvalCtx, a: &[Value]) -> Value {
    fin((|| {
        let x = num(ctx, &a[0])?;
        if x <= 0.0 {
            return Err(XlError::Num);
        }
        Ok(sp::lgamma_xl(x))
    })())
}

fn norm_pdf_checked(z: f64) -> R<f64> {
    if (z * z).is_infinite() {
        return Err(XlError::Num);
    }
    Ok(flush(norm_pdf(z)))
}

pub fn phi(ctx: &mut EvalCtx, a: &[Value]) -> Value {
    fin(num(ctx, &a[0]).and_then(|z| {
        norm_pdf_checked(z)?;
        Ok(norm_pdf_xl(z, 0.0, 1.0))
    }))
}

pub fn gauss(ctx: &mut EvalCtx, a: &[Value]) -> Value {
    fin(num(ctx, &a[0]).map(|x| {
        if x.abs() <= 1e-15 {
            x87::mul(sp::erf(x87::mul(-std::f64::consts::FRAC_1_SQRT_2, x)), -0.5)
        } else {
            norm_cdf(x) - 0.5
        }
    }))
}

pub fn norm_dist(ctx: &mut EvalCtx, a: &[Value]) -> Value {
    fin((|| {
        let x = num(ctx, &a[0])?;
        let m = num(ctx, &a[1])?;
        let s = num(ctx, &a[2])?;
        let cum = flag(a, 3)?;
        if s <= 0.0 {
            return Err(XlError::Num);
        }
        let z = (x - m) / s;
        if cum {
            return Ok(norm_cdf(z));
        }
        norm_pdf_checked(z)?;
        Ok(norm_pdf_xl(x, m, s))
    })())
}

fn norm_pdf_xl(x: f64, m: f64, s: f64) -> f64 {
    let z = x87::div(x87::sub(x, m), s);
    let e = x87::exp(x87::mul(x87::mul(z, z), -0.5));
    flush(x87::mul(x87::div(e, s), sp::INV_SQRT_2PI))
}

pub fn norm_s_dist(ctx: &mut EvalCtx, a: &[Value]) -> Value {
    fin((|| {
        let z = num(ctx, &a[0])?;
        let cum = flag(a, 1)?;
        Ok(if cum {
            norm_cdf(z)
        } else {
            norm_pdf_checked(z)?;
            norm_pdf_xl(z, 0.0, 1.0)
        })
    })())
}

pub fn normsdist(ctx: &mut EvalCtx, a: &[Value]) -> Value {
    fin(num(ctx, &a[0]).map(norm_cdf))
}

fn prob_open(p: f64) -> R<f64> {
    if p <= 0.0 || p >= 1.0 {
        Err(XlError::Num)
    } else {
        Ok(p)
    }
}

pub fn norm_s_inv(ctx: &mut EvalCtx, a: &[Value]) -> Value {
    fin((|| Ok(norm_s_inv_xl(prob_open(num(ctx, &a[0])?)?)))())
}

fn norm_s_inv_xl(p: f64) -> f64 {
    super::invnorm::norm_s_inv(p)
}

pub fn norm_inv(ctx: &mut EvalCtx, a: &[Value]) -> Value {
    fin((|| {
        let p = num(ctx, &a[0])?;
        let m = num(ctx, &a[1])?;
        let s = num(ctx, &a[2])?;
        prob_open(p)?;
        if s <= 0.0 {
            return Err(XlError::Num);
        }
        Ok(m + s * norm_s_inv_xl(p))
    })())
}

pub fn lognorm_dist(ctx: &mut EvalCtx, a: &[Value]) -> Value {
    fin((|| {
        let x = num(ctx, &a[0])?;
        let m = num(ctx, &a[1])?;
        let s = num(ctx, &a[2])?;
        let cum = flag(a, 3)?;
        lognorm(x, m, s, cum)
    })())
}

fn lognorm(x: f64, m: f64, s: f64, cum: bool) -> R<f64> {
    if x <= 0.0 || s <= 0.0 {
        return Err(XlError::Num);
    }
    let z = (x.ln() - m) / s;
    Ok(if cum {
        norm_cdf(z)
    } else {
        let lx = x87::ln(x);
        let d = x87::sub(lx, m);

        let den = x87::mul(2.0, x87::mul(s, s));
        if den == 0.0 {
            return Err(XlError::Div0);
        }
        let q = x87::div(x87::mul(d, d), den);
        let mut t = XlLogSum::new(x87::ln(s));
        t.add(q);
        t.add(LN_SQRT_2PI);
        t.add(lx);
        flush(x87::mul(x87::exp(t.err), x87::exp(-t.s)))
    })
}

pub fn lognormdist(ctx: &mut EvalCtx, a: &[Value]) -> Value {
    fin((|| {
        let x = num(ctx, &a[0])?;
        let m = num(ctx, &a[1])?;
        let s = num(ctx, &a[2])?;
        lognorm(x, m, s, true)
    })())
}

pub fn lognorm_inv(ctx: &mut EvalCtx, a: &[Value]) -> Value {
    fin((|| {
        let p = num(ctx, &a[0])?;
        let m = num(ctx, &a[1])?;
        let s = num(ctx, &a[2])?;
        prob_open(p)?;
        if s <= 0.0 {
            return Err(XlError::Num);
        }

        Ok(x87::exp(x87::add(m, x87::mul(norm_s_inv_xl(p), s))))
    })())
}

fn t_df(ctx: &EvalCtx, a: &[Value], i: usize) -> R<f64> {
    let df = int(ctx, a, i)?;
    if !(1.0..=1e10).contains(&df) {
        return Err(XlError::Num);
    }
    Ok(df)
}

pub(super) fn t_2t(t: f64, df: f64) -> f64 {
    if t == 0.0 {
        return 1.0;
    }
    let t2 = t * t;
    let s = df + t2;
    if df > 1e7 && t2 < df {
        return 1.0 - hp::beta_inc(0.5, df / 2.0, t2 / s, df / s);
    }
    hp::beta_inc(df / 2.0, 0.5, df / s, t2 / s)
}

fn t_xl(t: f64, df: f64) -> f64 {
    let t2 = x87::mul(t, t);
    let s = x87::add(t2, df);
    hp::beta_inc_xy(x87::div(df, 2.0), 0.5, x87::div(df, s), x87::div(t2, s)).0
}

fn t_tails_xl(t: f64, df: f64) -> (f64, f64) {
    let h = x87::div(t_xl(t, df), 2.0);
    let o = x87::sub(1.0, h);
    if t < 0.0 {
        (h, o)
    } else {
        (o, h)
    }
}

fn t_pdf(t: f64, df: f64) -> f64 {
    let t2 = x87::mul(t, t);
    if df == 1.0 {
        return x87::div(1.0, x87::mul(std::f64::consts::PI, x87::add(1.0, t2)));
    }

    if df == 2.0 {
        return x87::exp(x87::mul(-1.5, x87::ln(x87::add(t2, 2.0))));
    }
    let h1 = x87::div(x87::add(df, 1.0), 2.0);
    let h0 = x87::div(df, 2.0);
    let e = x87::sub(x87::sub(stirlerr(h1), stirlerr(h0)), bd0(h0, h1));
    let x = x87::div(t2, df);
    let u = if x >= 1.0 {
        x87::mul(h0, xl_log1p(x))
    } else {
        let np = x87::div(x87::add(t2, df), 2.0);
        x87::sub(x87::div(t2, 2.0), bd0(h0, np))
    };
    let c = x87::div(0.3989422804014327, x87::sqrt(x87::add(x, 1.0)));
    x87::mul(c, x87::exp(x87::sub(e, u)))
}

fn check_t(t: f64) -> R<f64> {
    if !(t * t).is_finite() {
        Err(XlError::Num)
    } else {
        Ok(t)
    }
}

pub fn t_dist(ctx: &mut EvalCtx, a: &[Value]) -> Value {
    fin((|| {
        coerce_all(ctx, a, "nnf")?;
        let t = check_t(num(ctx, &a[0])?)?;
        let raw_df = int(ctx, a, 1)?;
        let cum = flag(a, 2)?;

        if !cum && raw_df < 1.0 {
            let lim = if t == 0.0 { 0.0 } else { -1.0 };
            return Err(if raw_df < lim {
                XlError::Num
            } else {
                XlError::Div0
            });
        }
        let df = if cum { t_df(ctx, a, 1)? } else { raw_df };
        Ok(if cum {
            t_tails_xl(t, df).0
        } else {
            t_pdf(t, df)
        })
    })())
}

pub fn t_dist_2t(ctx: &mut EvalCtx, a: &[Value]) -> Value {
    fin((|| {
        coerce_all(ctx, a, "nn")?;
        let t = check_t(num(ctx, &a[0])?)?;
        let df = t_df(ctx, a, 1)?;
        if t < 0.0 {
            return Err(XlError::Num);
        }
        Ok(t_xl(t, df))
    })())
}

pub fn t_dist_rt(ctx: &mut EvalCtx, a: &[Value]) -> Value {
    fin((|| {
        coerce_all(ctx, a, "nn")?;
        let t = check_t(num(ctx, &a[0])?)?;
        let df = t_df(ctx, a, 1)?;
        Ok(t_tails_xl(t, df).1)
    })())
}

pub fn tdist(ctx: &mut EvalCtx, a: &[Value]) -> Value {
    fin((|| {
        coerce_all(ctx, a, "nnn")?;
        let t = check_t(num(ctx, &a[0])?)?;
        let df = t_df(ctx, a, 1)?;
        let tails = crate::functions::args::sfix(num(ctx, &a[2])?) as f64;
        if t < 0.0 || !(tails == 1.0 || tails == 2.0) {
            return Err(XlError::Num);
        }
        Ok(if tails == 2.0 {
            t_xl(t, df)
        } else {
            t_tails_xl(t, df).1
        })
    })())
}

fn t_inv_xl(p: f64, df: f64) -> Result<f64, XlError> {
    if p == 0.5 {
        return Ok(0.0);
    }
    let s = x87::add(x87::sub(0.5, p), 0.5);
    let m = if p < s { p } else { s };
    let t = if df == 1.0 {
        x87::div(1.0, x87::tan(x87::mul(m, std::f64::consts::PI)))
    } else {
        let two = x87::mul(2.0, m);
        if two > 0.9 {
            let x = inv_ibeta_xy(two, true, 0.5, x87::mul(df, 0.5))?.0;
            x87::sqrt(x87::div(x87::mul(x, df), x87::sub(1.0, x)))
        } else {
            let x = inv_ibeta_xy(two, false, x87::mul(df, 0.5), 0.5)?.0;
            x87::sqrt(x87::mul(x87::sub(x87::div(1.0, x), 1.0), df))
        }
    };
    Ok(if p < 0.5 { -t } else { t })
}

pub fn t_inv(ctx: &mut EvalCtx, a: &[Value]) -> Value {
    fin((|| {
        coerce_all(ctx, a, "nn")?;
        let p = num(ctx, &a[0])?;
        let df = t_df(ctx, a, 1)?;
        if p == 0.0 && df == 1.0 {
            return Err(XlError::Div0);
        }
        let p = prob_open(p)?;
        if df == 1.0 && p.min(1.0 - p) < f64::MIN_POSITIVE {
            return Err(XlError::Div0);
        }
        t_inv_xl(p, df)
    })())
}

pub fn t_inv_2t(ctx: &mut EvalCtx, a: &[Value]) -> Value {
    fin((|| {
        coerce_all(ctx, a, "nn")?;
        let p = num(ctx, &a[0])?;
        let df = t_df(ctx, a, 1)?;

        if !(0.0..2.0).contains(&p) {
            return Err(XlError::Num);
        }
        if p == 0.0 && df != 1.0 {
            return Err(XlError::Num);
        }
        if df == 1.0 && p.min(2.0 - p) < f64::MIN_POSITIVE {
            return Err(XlError::Div0);
        }
        Ok(-t_inv_xl(x87::div(p, 2.0), df)?)
    })())
}

fn chi_df(ctx: &EvalCtx, a: &[Value], i: usize) -> R<f64> {
    let df = int(ctx, a, i)?;
    if !(1.0..=1e10).contains(&df) {
        return Err(XlError::Num);
    }
    Ok(df)
}

fn gamma_pdf(x: f64, k: f64, theta: f64) -> R<f64> {
    if x == 0.0 {
        return if k < 1.0 || k == 1.0 {
            Err(XlError::Num)
        } else {
            Ok(0.0)
        };
    }
    let y = x87::div(x, theta);
    if y > 0.0 && y.is_finite() && k.is_finite() {
        return Ok(x87::mul(poisson_pmf(k, y), x87::div(k, x)));
    }
    if let Some(p) = xl_gamma_pdf(y, k) {
        return Ok(p / theta);
    }
    Ok(((k - 1.0) * y.ln() - y - lgamma(k)).exp() / theta)
}

fn xl_gamma_pdf(y: f64, a: f64) -> Option<f64> {
    if !(y > 0.0 && y.is_finite() && a > 0.0 && a.is_finite()) {
        return None;
    }
    let e = hp::xl_gamma_prefactor(a, y);
    let r = e * (a / y);
    (e.is_finite() && e >= f64::MIN_POSITIVE && r.is_finite()).then_some(r)
}

pub fn chisq_dist(ctx: &mut EvalCtx, a: &[Value]) -> Value {
    fin((|| {
        coerce_all(ctx, a, "nnf")?;
        let x = num(ctx, &a[0])?;
        let df = chi_df(ctx, a, 1)?;
        let cum = flag(a, 2)?;
        if x < 0.0 {
            return Err(XlError::Num);
        }
        if cum {
            return Ok(igamma_pq(df / 2.0, x / 2.0).0);
        }
        if x == 0.0 {
            return if df == 1.0 {
                Err(XlError::Num)
            } else if df == 2.0 {
                Ok(0.5)
            } else {
                Ok(0.0)
            };
        }
        gamma_pdf(x, df / 2.0, 2.0)
    })())
}

pub fn chisq_dist_rt(ctx: &mut EvalCtx, a: &[Value]) -> Value {
    fin((|| {
        coerce_all(ctx, a, "nn")?;
        let x = num(ctx, &a[0])?;
        let df = chi_df(ctx, a, 1)?;
        if x < 0.0 {
            return Err(XlError::Num);
        }
        Ok(igamma_pq(df / 2.0, x / 2.0).1)
    })())
}

fn bracket(start: f64, target: f64, f: &impl Fn(f64) -> f64) -> f64 {
    let mut hi = start.max(1.0);
    while f(hi) < target && hi < 1e300 {
        hi *= 2.0;
    }
    hi
}

fn inv_status(x: f64) -> R<f64> {
    if x.is_nan() {
        Err(XlError::Num)
    } else if x == f64::NEG_INFINITY {
        Err(XlError::Div0)
    } else {
        Ok(x)
    }
}

fn gamma_inv_unit(p: f64, k: f64) -> f64 {
    if k == 0.5 {
        let t = super::invnorm::inverse_erf(p);
        return x87::mul(t, t);
    }
    if k == 1.0 {
        return -xl_log1p(-p);
    }
    gamma_inv_xl(p, false, k)
}

fn gamma_inv_xl(t: f64, upper: bool, a: f64) -> f64 {
    let lnp = if upper { xl_log1p(-t) } else { x87::ln(t) };
    let x = if x87::mul(lnp, -0.62) > a {
        x87::exp(x87::div(
            x87::add(x87::add(lnp, sp::lgamma_xl(a)), x87::ln(a)),
            a,
        ))
    } else {
        let z = if upper {
            -super::invnorm::norm_s_inv(t)
        } else {
            super::invnorm::norm_s_inv(t)
        };
        if a < 1.0 {
            let v = x87::div(1.0, x87::mul(a, 9.0));
            let c = x87::add(x87::mul(z, x87::sqrt(v)), x87::sub(1.0, v));
            return gamma_inv_halley(t, upper, a, x87::mul(a, xl_pow(c, 3.0)));
        }
        let u = x87::div(0.5, a);
        let w = x87::mul(z, x87::sqrt(u));

        const C: [[f64; 3]; 7] = [
            [0.001565326, -0.001450117, 9.780499e-05],
            [0.001060438, 0.00253001, -0.0008426812],
            [-0.006950356, 0.005169654, 0.00312558],
            [-0.01323293, -0.01153761, -0.008553069],
            [0.02277679, 0.01128186, 0.0001348028],
            [-0.008986007, 0.02607083, 0.4713941],
            [-0.01513904, -0.2237368, 1.0000886],
        ];
        let ck = |c: &[f64; 3]| x87::add(x87::mul(x87::add(x87::mul(c[0], u), c[1]), u), c[2]);
        let c = C[1..]
            .iter()
            .fold(ck(&C[0]), |acc, k| x87::add(x87::mul(acc, w), ck(k)));
        x87::mul(x87::mul(x87::mul(c, c), c), a)
    };
    gamma_inv_halley(t, upper, a, x)
}

fn gamma_inv_halley(t: f64, upper: bool, a: f64, mut x: f64) -> f64 {
    if x == 0.0 {
        return 0.0;
    }
    if x < 0.0 || x.is_nan() {
        return f64::NAN;
    }
    if x < f64::MIN_POSITIVE {
        return f64::NEG_INFINITY;
    }

    let (t, upper) = if t > 0.9999 {
        (x87::sub(1.0, t), !upper)
    } else {
        (t, upper)
    };
    let am1 = x87::sub(a, 1.0);
    let mut best = (f64::INFINITY, x);
    for _ in 0..106 {
        let (pp, qq) = igamma_pq(a, x);
        let r = x87::sub(if upper { qq } else { pp }, t);
        if r.abs() < best.0 {
            best = (r.abs(), x);
        }
        let f = x87::mul(poisson_pmf(a, x), x87::div(a, x));
        let f = if upper { -f } else { f };
        if f == 0.0 {
            return x;
        }
        let c = x87::sub(x87::div(am1, x), 1.0);
        let den = x87::sub(x87::mul(2.0, f), x87::mul(r, c));
        let xn = x87::sub(x, x87::div(x87::mul(2.0, r), den));
        if xn == 0.0 {
            return f64::NEG_INFINITY;
        }
        if !(xn > 0.0) {
            return f64::NAN;
        }
        if x87::sub(xn, x).abs() <= x87::mul(x, f64::EPSILON) {
            return x;
        }
        x = xn;
    }

    if x87::div(best.0, t) <= 1e-12 {
        best.1
    } else {
        f64::NAN
    }
}

fn gamma_inv_upper(q: f64, k: f64) -> f64 {
    if k == 0.5 {
        let t = super::invnorm::inverse_erf(x87::add(0.5, x87::sub(0.5, q)));
        return x87::mul(t, t);
    }
    if k != 1.0 {
        return gamma_inv_xl(q, true, k);
    }

    if q > 0.5 {
        let y = x87::mul(x87::sub(1.0, q), -1.0);
        return x87::mul(xl_log1p(y), -1.0);
    }
    if q <= 0.5 {
        let f = |x: f64| -hp::gamma_q_cr(k, x);
        let hi = bracket(k, -q, &f);
        return sp::solve_increasing(0.0, hi, -q, f);
    }
    let p = 1.0 - q;
    let f = |x: f64| hp::gamma_p_cr(k, x);
    let hi = bracket(k, p, &f);
    sp::solve_increasing(0.0, hi, p, f)
}

pub fn chisq_inv(ctx: &mut EvalCtx, a: &[Value]) -> Value {
    fin((|| {
        coerce_all(ctx, a, "nn")?;
        let p = num(ctx, &a[0])?;
        let df = chi_df(ctx, a, 1)?;
        if !(0.0..1.0).contains(&p) {
            return Err(XlError::Num);
        }
        if p == 0.0 {
            return Ok(0.0);
        }
        Ok(2.0 * inv_status(gamma_inv_unit(p, df / 2.0))?)
    })())
}

pub fn chisq_inv_rt(ctx: &mut EvalCtx, a: &[Value]) -> Value {
    fin((|| {
        coerce_all(ctx, a, "nn")?;
        let p = num(ctx, &a[0])?;
        let df = chi_df(ctx, a, 1)?;
        if p <= 0.0 || p > 1.0 {
            return Err(XlError::Num);
        }
        if p == 1.0 {
            return Ok(0.0);
        }
        Ok(2.0 * inv_status(gamma_inv_upper(p, df / 2.0))?)
    })())
}

fn f_dfs(ctx: &EvalCtx, a: &[Value], i: usize) -> R<(f64, f64)> {
    let d1 = int(ctx, a, i)?;
    let d2 = int(ctx, a, i + 1)?;
    if !(1.0..=1e10).contains(&d1) || !(1.0..=1e10).contains(&d2) {
        return Err(XlError::Num);
    }
    Ok((d1, d2))
}

fn f_tails_xl(x: f64, d1: f64, d2: f64) -> (f64, f64) {
    if x == 0.0 {
        return (0.0, 1.0);
    }
    let v = x87::mul(d1, x);
    let (h1, h2) = (x87::mul(d1, 0.5), x87::mul(d2, 0.5));
    let up = v > d2;
    let r = if up { x87::div(v, d2) } else { x87::div(d2, v) };
    let s = x87::add(r, 1.0);
    let (x0, y0) = (x87::div(1.0, s), x87::div(r, s));
    if up {
        let (w, w1) = hp::beta_inc_xy(h2, h1, x0, y0);
        (w1, w)
    } else {
        hp::beta_inc_xy(h1, h2, x0, y0)
    }
}

fn f_sf(x: f64, d1: f64, d2: f64) -> f64 {
    if x == 0.0 {
        return 1.0;
    }
    let y = d1 * x;
    hp::beta_inc(d2 / 2.0, d1 / 2.0, d2 / (d2 + y), y / (d2 + y))
}

pub fn f_dist(ctx: &mut EvalCtx, a: &[Value]) -> Value {
    fin((|| {
        coerce_all(ctx, a, "nnnf")?;
        let x = num(ctx, &a[0])?;
        let (d1, d2) = f_dfs(ctx, a, 1)?;
        let cum = flag(a, 3)?;
        if x < 0.0 {
            return Err(XlError::Num);
        }
        if cum {
            return Ok(f_tails_xl(x, d1, d2).0);
        }
        if x == 0.0 {
            return if d1 == 1.0 {
                Err(XlError::Num)
            } else if d1 == 2.0 {
                Ok(1.0)
            } else {
                Ok(0.0)
            };
        }
        Ok(f_pdf_xl(x, d1, d2))
    })())
}

const LN_SQRT_4PI: f64 = 1.2655121234846454;

fn f_pdf_xl(x: f64, d1: f64, d2: f64) -> f64 {
    let (h1, h2) = (x87::mul(d1, 0.5), x87::mul(d2, 0.5));
    let hs = x87::mul(x87::add(d1, d2), 0.5);
    let e1 = stirlerr(h1);
    let e2 = stirlerr(h2);
    let es = x87::mul(stirlerr(hs), -1.0);
    let l1 = xl_log1p(x87::div(d1, d2));

    let u = if d1 > d2 {
        x87::sub(x87::ln(d2), xl_log1p(x87::div(d2, d1)))
    } else {
        x87::sub(x87::ln(d1), l1)
    };
    let u = x87::mul(-0.5, u);
    let d1x = x87::mul(d1, x);
    let v = if d1x > d2 {
        let w = x87::mul(0.5, x87::mul(d2, x87::ln(x)));
        let l2 = xl_log1p(x87::div(d2, d1x));
        let l1 = xl_log1p(x87::div(d2, d1));
        x87::add(
            w,
            x87::mul(0.5, x87::mul(x87::add(d1, d2), x87::sub(l2, l1))),
        )
    } else {
        let w = x87::mul(-0.5, x87::mul(d1, x87::ln(x)));
        let l2 = xl_log1p(x87::div(d1x, d2));
        x87::add(
            w,
            x87::mul(0.5, x87::mul(x87::add(d1, d2), x87::sub(l2, l1))),
        )
    };
    let mut t = XlLogSum::new(e1);
    t.add(e2);
    t.add(es);
    t.add(LN_SQRT_4PI);
    t.add(u);
    t.add(v);
    flush(x87::mul(x87::div(x87::exp(-t.err), x), x87::exp(-t.s)))
}

pub fn f_dist_rt(ctx: &mut EvalCtx, a: &[Value]) -> Value {
    fin((|| {
        coerce_all(ctx, a, "nnn")?;
        let x = num(ctx, &a[0])?;
        let (d1, d2) = f_dfs(ctx, a, 1)?;
        if x < 0.0 {
            return Err(XlError::Num);
        }
        Ok(f_tails_xl(x, d1, d2).1)
    })())
}

fn f_inv_xl(p: f64, upper: bool, d1: f64, d2: f64) -> Result<f64, XlError> {
    let l2 = x87::ln(d2);
    let (x, y) = inv_ibeta_xy(p, upper, x87::mul(d1, 0.5), x87::mul(d2, 0.5))?;
    let (lx, ly) = if y < x {
        (xl_log1p(-y), x87::ln(y))
    } else {
        (x87::ln(x), xl_log1p(-x))
    };
    let mut s = XlLogSum::new(l2);
    s.add(lx);
    s.add(-x87::ln(d1));
    s.add(-ly);
    Ok(s.exp())
}

pub fn f_inv(ctx: &mut EvalCtx, a: &[Value]) -> Value {
    fin((|| {
        coerce_all(ctx, a, "nnn")?;
        let p = num(ctx, &a[0])?;
        let (d1, d2) = f_dfs(ctx, a, 1)?;
        if !(0.0..1.0).contains(&p) {
            return Err(XlError::Num);
        }
        if p == 0.0 {
            return Ok(0.0);
        }
        f_inv_xl(p, false, d1, d2)
    })())
}

pub fn f_inv_rt(ctx: &mut EvalCtx, a: &[Value]) -> Value {
    fin((|| {
        coerce_all(ctx, a, "nnn")?;
        let p = num(ctx, &a[0])?;
        let (d1, d2) = f_dfs(ctx, a, 1)?;
        if p <= 0.0 || p > 1.0 {
            return Err(XlError::Num);
        }
        if p == 1.0 {
            return Ok(0.0);
        }
        f_inv_xl(p, true, d1, d2)
    })())
}

fn opt_bound(ctx: &EvalCtx, a: &[Value], i: usize, default: f64) -> R<f64> {
    opt_num_or_blank(ctx, a, i, default)
}

fn beta_core(x: f64, al: f64, be: f64, cum: bool, lo: f64, hi: f64) -> R<f64> {
    if al <= 0.0 || be <= 0.0 || lo >= hi || x < lo || x > hi {
        return Err(XlError::Num);
    }
    let z = x87::div(x87::sub(x, lo), x87::sub(hi, lo));
    if cum {
        return Ok(hp::beta_inc(al, be, z, (hi - x) / (hi - lo)));
    }
    if z == 0.0 {
        return if al <= 1.0 {
            Err(XlError::Num)
        } else {
            Ok(0.0)
        };
    }
    if z == 1.0 {
        return if be <= 1.0 {
            Err(XlError::Num)
        } else {
            Ok(0.0)
        };
    }
    Ok(x87::div(xl_beta_pdf(z, al, be), x87::sub(hi, lo)))
}

pub(super) fn xl_beta_pdf(z: f64, al: f64, be: f64) -> f64 {
    if al > 1.0 && be > 1.0 {
        let mut s = xl_dbinom_log(x87::sub(al, 1.0), x87::sub(x87::add(al, be), 2.0), z);
        s.add(x87::ln(x87::sub(x87::add(al, be), 1.0)));
        s.exp()
    } else {
        let mut s = XlLogSum::new(x87::mul(x87::sub(be, 1.0), xl_log1p(-z)));
        s.add(x87::mul(x87::sub(al, 1.0), x87::ln(z)));
        s.add(-lbeta_xl(al, be));
        s.exp()
    }
}

pub fn beta_dist(ctx: &mut EvalCtx, a: &[Value]) -> Value {
    fin((|| {
        let x = num(ctx, &a[0])?;
        let al = num(ctx, &a[1])?;
        let be = num(ctx, &a[2])?;
        let cum = flag(a, 3)?;
        let lo = opt_bound(ctx, a, 4, 0.0)?;
        let hi = opt_bound(ctx, a, 5, 1.0)?;
        beta_core(x, al, be, cum, lo, hi)
    })())
}

pub fn betadist(ctx: &mut EvalCtx, a: &[Value]) -> Value {
    fin((|| {
        let x = num(ctx, &a[0])?;
        let al = num(ctx, &a[1])?;
        let be = num(ctx, &a[2])?;
        let lo = opt_bound(ctx, a, 3, 0.0)?;
        let hi = opt_bound(ctx, a, 4, 1.0)?;
        beta_core(x, al, be, true, lo, hi)
    })())
}

pub(super) fn xl_inv_ibeta(prob: f64, upper: bool, a: f64, b: f64) -> Result<(f64, bool), XlError> {
    let other = x87::add(0.5, x87::sub(0.5, prob));
    let (lo, up) = if upper { (other, prob) } else { (prob, other) };
    let swap = if upper { prob <= 0.5 } else { prob > 0.5 };
    let (al, pp, qq) = if swap { (up, b, a) } else { (lo, a, b) };

    let x = inv_ibeta_start(al, pp, qq);
    let x = if !(x < 1.0) {
        0.999999
    } else if x <= 0.0 {
        1e-6
    } else {
        x
    };
    Ok((inv_ibeta_iter(al, pp, qq, x)?, swap))
}

pub(super) fn inv_ibeta_xy(prob: f64, upper: bool, a: f64, b: f64) -> Result<(f64, f64), XlError> {
    let (v, swap) = xl_inv_ibeta(prob, upper, a, b)?;
    let c = x87::add(0.5, x87::sub(0.5, v));
    Ok(if swap { (c, v) } else { (v, c) })
}

fn inv_ibeta_start(al: f64, pp: f64, qq: f64) -> f64 {
    let y = x87::sub(0.0, super::invnorm::norm_s_inv(al));
    let r = x87::mul(qq, 2.0);
    if pp > 1.0 && qq > 1.0 {
        let lam = x87::div(x87::sub(x87::mul(y, y), 3.0), 6.0);
        let s = x87::div(1.0, x87::sub(x87::mul(pp, 2.0), 1.0));
        let t = x87::div(1.0, x87::sub(r, 1.0));
        let h = x87::div(2.0, x87::add(t, s));
        let d = x87::sub(t, s);
        let k = x87::sub(
            x87::add(lam, 0.8333333333333334),
            x87::div(2.0, x87::mul(3.0, h)),
        );
        let w = x87::sub(
            x87::div(x87::mul(y, x87::sqrt(x87::add(lam, h))), h),
            x87::mul(d, k),
        );
        let e = x87::mul(qq, x87::exp(x87::mul(2.0, w)));
        return x87::div(pp, x87::add(pp, e));
    }
    let t = x87::div(1.0, x87::mul(qq, 9.0));
    let c = x87::add(x87::sub(1.0, t), x87::mul(y, x87::sqrt(t)));

    let c3 = if c < 0.0 {
        -xl_pow(-c, 3.0)
    } else {
        xl_pow(c, 3.0)
    };
    if !c3.is_finite() {
        return 0.5;
    }
    let t = x87::mul(r, c3);
    let num = x87::sub(x87::add(x87::mul(pp, 4.0), r), 2.0);
    if t <= 0.0 {
        let l = x87::add(
            xl_log1p(x87::mul(al, -1.0)),
            x87::add(x87::ln(qq), lbeta_xl(pp, qq)),
        );
        x87::mul(xl_expm1(x87::div(l, qq)), -1.0)
    } else if num <= t {
        let l = x87::add(x87::ln(al), x87::add(x87::ln(pp), lbeta_xl(pp, qq)));
        x87::exp(x87::div(l, pp))
    } else {
        let t = x87::div(num, t);
        x87::sub(1.0, x87::div(2.0, x87::add(t, 1.0)))
    }
}

fn inv_ibeta_iter(al: f64, pp: f64, qq: f64, mut x: f64) -> Result<f64, XlError> {
    let e = x87::sub(
        x87::add(-13.0, x87::div(-5.0, x87::mul(pp, pp))),
        xl_pow(al, -0.2),
    );
    let acu = xl_pow(10.0, x87::mul(2.0, e).max(-300.0)).min(4.930380657631324e-32);
    let (mut yprev, mut sq, mut prev) = (0.0, 1.0f64, 1.0);
    let mut best = (f64::INFINITY, x);
    let mut trials = 0;
    let mut evals = 0;
    let mut limited = false;
    loop {
        if evals >= 501 {
            limited = true;
            break;
        }
        evals += 1;
        let i = match super::ibeta::xl_ibeta(x, x87::sub(1.0, x), pp, qq) {
            Some((w, _)) => w,
            None => hp::beta_inc(pp, qq, x, 0.0),
        };
        let r = x87::sub(i, al);
        if r.abs() < best.0 {
            best = (r.abs(), x);
        }
        let pdf = xl_beta_pdf(x, pp, qq);
        if pdf == 0.0 {
            break;
        }

        if pdf.is_infinite() {
            return Err(XlError::Num);
        }
        if x87::div(x87::mul(r, yprev), pdf) <= 0.0 {
            prev = if sq == 0.0 { f64::MIN_POSITIVE } else { sq };
        }
        if prev <= acu {
            break;
        }
        let y2 = x87::div(x87::mul(r, r), x87::mul(pdf, pdf));
        if y2 <= acu {
            if y2 != 0.0 || r == 0.0 {
                break;
            }
            let ly = x87::mul(x87::sub(x87::ln(r.abs()), x87::ln(pdf.abs())), 2.0);
            if !(x87::ln(acu) < ly) {
                break;
            }
        }

        let rf = if r.abs() < f64::MIN_POSITIVE { 0.0 } else { r };
        let mut g = 1.0;
        trials += 1;
        if trials >= 700 {
            limited = true;
            break;
        }
        let tx = loop {
            let adj = x87::div(x87::mul(g, rf), pdf);
            sq = x87::mul(adj, adj);
            if sq < prev {
                let tx = x87::sub(x, adj);
                if tx > 0.0 && tx < 1.0 {
                    break Some(tx);
                }
            }
            g = x87::div(g, 3.0);
            trials += 1;
            if trials >= 700 {
                break None;
            }
        };
        let Some(tx) = tx else {
            limited = true;
            break;
        };
        if tx == x {
            break;
        }
        yprev = x87::div(r, pdf);
        let d = x87::sub(tx, x).abs();
        if d <= x87::mul(x, 1.1102230246251565e-16).abs() {
            break;
        }
        x = tx;
    }
    if (limited || evals > 500) && x87::div(best.0, al).abs() > 1e-12 {
        return Err(XlError::Num);
    }
    Ok(best.1)
}

pub fn beta_inv(ctx: &mut EvalCtx, a: &[Value]) -> Value {
    fin((|| {
        let p = num(ctx, &a[0])?;
        let al = num(ctx, &a[1])?;
        let be = num(ctx, &a[2])?;
        let lo = opt_bound(ctx, a, 3, 0.0)?;
        let hi = opt_bound(ctx, a, 4, 1.0)?;
        prob_open(p)?;
        if al <= 0.0 || be <= 0.0 || lo >= hi {
            return Err(XlError::Num);
        }
        let z = inv_ibeta_xy(p, false, al, be)?.0;

        Ok(x87::add(lo, x87::mul(z, x87::sub(hi, lo))))
    })())
}

pub fn gamma_dist(ctx: &mut EvalCtx, a: &[Value]) -> Value {
    fin((|| {
        let x = num(ctx, &a[0])?;
        let al = num(ctx, &a[1])?;
        let be = num(ctx, &a[2])?;
        let cum = flag(a, 3)?;
        if x < 0.0 || al <= 0.0 || be <= 0.0 {
            return Err(XlError::Num);
        }
        if cum {
            Ok(igamma_pq(al, x87::div(x, be)).0)
        } else {
            gamma_pdf(x, al, be)
        }
    })())
}

pub fn gamma_inv(ctx: &mut EvalCtx, a: &[Value]) -> Value {
    fin((|| {
        let p = num(ctx, &a[0])?;
        let al = num(ctx, &a[1])?;
        let be = num(ctx, &a[2])?;
        if !(0.0..1.0).contains(&p) || al <= 0.0 || be <= 0.0 {
            return Err(XlError::Num);
        }
        if p == 0.0 {
            return Ok(0.0);
        }
        Ok(x87::mul(inv_status(gamma_inv_unit(p, al))?, be))
    })())
}

pub fn expon_dist(ctx: &mut EvalCtx, a: &[Value]) -> Value {
    fin((|| {
        let x = num(ctx, &a[0])?;
        let l = num(ctx, &a[1])?;
        let cum = flag(a, 2)?;
        if x < 0.0 || l <= 0.0 {
            return Err(XlError::Num);
        }

        Ok(if cum {
            x87::sub(0.0, xl_expm1(-x87::mul(l, x)))
        } else {
            l * (-l * x).exp()
        })
    })())
}

pub fn weibull_dist(ctx: &mut EvalCtx, a: &[Value]) -> Value {
    fin((|| {
        let x = num(ctx, &a[0])?;
        let al = num(ctx, &a[1])?;
        let be = num(ctx, &a[2])?;
        let cum = flag(a, 3)?;
        if x < 0.0 || al <= 0.0 || be <= 0.0 {
            return Err(XlError::Num);
        }

        let u = if x == 0.0 {
            0.0
        } else {
            xl_pow(x87::div(x, be), al)
        };
        if !u.is_finite() {
            return Err(XlError::Num);
        }
        if cum {
            return Ok(x87::sub(0.0, xl_expm1(-u)));
        }
        if x == 0.0 {
            return Ok(0.0);
        }
        let e = x87::exp(-u);

        let (px, pb) = (xl_pow(x, x87::sub(al, 1.0)), xl_pow(be, al));
        if !px.is_finite() || !pb.is_finite() {
            return Err(XlError::Num);
        }
        if pb.to_bits() & 0xFFFF_0000_0000_0000 == 0 {
            return Err(XlError::Div0);
        }
        let r = x87::mul(px, x87::div(al, pb));
        Ok(x87::mul(e, r))
    })())
}

pub(super) fn binom_pmf(k: f64, n: f64, p: f64) -> f64 {
    if p == 0.0 {
        return if k == 0.0 { 1.0 } else { 0.0 };
    }
    if p == 1.0 {
        return if k == n { 1.0 } else { 0.0 };
    }

    if k == 0.0 {
        return x87::exp(x87::mul(n, xl_log1p(-p)));
    }
    if k == n {
        return x87::exp(x87::mul(n, x87::ln(p)));
    }
    xl_dbinom_log(k, n, p).exp()
}

fn binom_cdf(k: f64, n: f64, p: f64) -> f64 {
    if k >= n {
        return 1.0;
    }
    if p == 0.0 {
        return 1.0;
    }
    if p == 1.0 {
        return 0.0;
    }

    if k == 0.0 {
        return x87::exp(x87::mul(xl_log1p(-p), n));
    }
    match super::ibeta::xl_ibeta(p, x87::add(1.0, -p), k + 1.0, n - k) {
        Some((_, c)) => c,
        None => hp::beta_inc(n - k, k + 1.0, 1.0 - p, p),
    }
}

const MAX_TRIALS: f64 = 2147483647.0;

pub fn binom_dist(ctx: &mut EvalCtx, a: &[Value]) -> Value {
    fin((|| {
        let k = num(ctx, &a[0])?;
        let n = num(ctx, &a[1])?;
        if k < 0.0 || n < 0.0 {
            return Err(XlError::Num);
        }
        let (k, n) = (k.trunc(), n.trunc());
        let p = num(ctx, &a[2])?;
        let cum = flag(a, 3)?;
        if k < 0.0 || n < 0.0 || k > n || !(0.0..=1.0).contains(&p) || n >= MAX_TRIALS {
            return Err(XlError::Num);
        }
        if !cum && k == 0.0 && p == 1.0 && n > 0.0 {
            return Err(XlError::Num);
        }
        Ok(if cum {
            binom_cdf(k, n, p)
        } else {
            binom_pmf(k, n, p)
        })
    })())
}

pub fn binom_dist_range(ctx: &mut EvalCtx, a: &[Value]) -> Value {
    fin((|| {
        let n = int(ctx, a, 0)?;
        let p = num(ctx, &a[1])?;
        let s = int(ctx, a, 2)?;
        let s2 = match a.get(3) {
            None | Some(Value::Blank) => s,
            Some(v) => num(ctx, v)?.trunc(),
        };
        if n < 0.0 || !(0.0..=1.0).contains(&p) || s < 0.0 || s > n || s2 < s || s2 > n {
            return Err(XlError::Num);
        }
        if s == s2 {
            return Ok(binom_pmf(s, n, p));
        }
        Ok(binom_range(n, p, s, s2))
    })())
}

fn comp_sum(hi: f64, lo: f64) -> f64 {
    let t = x87::mul(-1.0, lo);
    let sum = x87::add(hi, t);
    let bv = x87::sub(sum, hi);
    let av = x87::sub(sum, bv);
    let br = x87::sub(t, bv);
    let err = x87::add(br, x87::sub(hi, av));
    x87::add(sum, err)
}

fn binom_range(n: f64, p: f64, s: f64, s2: f64) -> f64 {
    if s == 0.0 {
        return binom_cdf(s2, n, p);
    }
    let lo_cdf = binom_cdf(x87::sub(s, 1.0), n, p);
    let (hi, lo) = if lo_cdf > 0.9 {
        let t = x87::mul(-1.0, p);
        let q0 = x87::add(1.0, t);
        let bv = x87::sub(q0, 1.0);
        let av = x87::sub(q0, bv);
        let br = x87::sub(t, bv);
        let err = x87::add(br, x87::sub(1.0, av));
        let q = x87::add(x87::add(q0, err), err);
        let upper = |a: f64, b: f64| match super::ibeta::xl_ibeta(q, p, a, b) {
            Some((_, c)) => c,
            None => hp::beta_inc(b, a, p, q),
        };
        let from_s = || upper(x87::add(1.0, x87::sub(n, s)), s);
        if s2 == n {
            return from_s();
        }
        let above = upper(x87::sub(n, s2), x87::add(1.0, s2));
        (from_s(), above)
    } else {
        (binom_cdf(s2, n, p), lo_cdf)
    };
    let r = comp_sum(hi, lo);
    let ratio = if lo != 0.0 { x87::div(r, lo) } else { 1.0 };
    if ratio > 0.001 {
        return r;
    }
    let (mut k, mut acc) = (s, 0.0);
    let mut i = 0;
    while k <= s2 {
        if i > 100 {
            return r;
        }
        let next = x87::add(acc, binom_pmf(k, n, p));
        if next == acc {
            break;
        }
        acc = next;
        k = x87::add(k, 1.0);
        i += 1;
    }
    if i > 100 {
        r
    } else {
        acc
    }
}

pub fn binom_inv(ctx: &mut EvalCtx, a: &[Value]) -> Value {
    fin((|| {
        let n = int(ctx, a, 0)?;
        let p = num(ctx, &a[1])?;
        let alpha = num(ctx, &a[2])?;
        if n < 0.0 || n >= MAX_TRIALS || p <= 0.0 || p >= 1.0 || alpha <= 0.0 || alpha >= 1.0 {
            return Err(XlError::Num);
        }

        let (mut lo, mut hi) = (0.0f64, n);
        if binom_cdf(lo, n, p) > alpha {
            return Ok(0.0);
        }
        while hi - lo > 1.0 {
            let mid = ((lo + hi) / 2.0).floor();
            if binom_cdf(mid, n, p) > alpha {
                hi = mid;
            } else {
                lo = mid;
            }
        }
        Ok(hi)
    })())
}

pub fn negbinom_dist(ctx: &mut EvalCtx, a: &[Value]) -> Value {
    fin((|| {
        let f = int(ctx, a, 0)?;
        let s = int(ctx, a, 1)?;
        let p = num(ctx, &a[2])?;
        let cum = flag(a, 3)?;
        negbinom(f, s, p, cum)
    })())
}

pub fn negbinomdist(ctx: &mut EvalCtx, a: &[Value]) -> Value {
    fin((|| {
        let f = int(ctx, a, 0)?;
        let s = int(ctx, a, 1)?;
        let p = num(ctx, &a[2])?;
        negbinom(f, s, p, false)
    })())
}

fn negbinom_pmf(f: f64, s: f64, p: f64) -> f64 {
    if f == 0.0 {
        return x87::exp(x87::mul(s, x87::ln(p)));
    }
    let b = x87::add(f, 1.0);
    x87::mul(xl_beta_pdf(p, s, b), x87::div(p, x87::add(f, s)))
}

fn negbinom(f: f64, s: f64, p: f64, cum: bool) -> R<f64> {
    if f < 0.0 || s < 1.0 || p <= 0.0 || p >= 1.0 {
        return Err(XlError::Num);
    }
    if !cum {
        return Ok(negbinom_pmf(f, s, p));
    }
    Ok(hp::beta_inc(s, f + 1.0, p, 1.0 - p))
}

fn hypgeom_pmf(k: f64, n: f64, kk: f64, nn: f64) -> f64 {
    let nk = x87::sub(n, k);
    let p = x87::div(nk.max(k), nn);
    let r = x87::div(binom_pmf(k, kk, p), binom_pmf(n, nn, p));
    x87::mul(r, binom_pmf(nk, x87::sub(nn, kk), p))
}

fn hypgeom_cdf_sum(k: f64, n: f64, kk: f64, nn: f64) -> f64 {
    let c = x87::sub(x87::sub(nn, n), kk);
    let num = x87::mul(k, x87::add(c, k));
    let den = x87::mul(
        x87::add(x87::sub(kk, k), 1.0),
        x87::add(x87::sub(n, k), 1.0),
    );
    let mut t = x87::div(num, den);
    let mut s = t;
    let mut j = x87::sub(k, 2.0);
    while j >= 0.0 {
        let a = x87::add(x87::add(x87::sub(x87::add(-kk, nn), n), j), 1.0);
        let num = x87::mul(x87::add(j, 1.0), a);
        let den = x87::mul(x87::sub(kk, j), x87::sub(n, j));
        t = x87::mul(t, x87::div(num, den));
        s = x87::add(s, t);
        j = x87::sub(j, 1.0);
    }
    x87::mul(x87::add(s, 1.0), hypgeom_pmf(k, n, kk, nn))
}

fn hypgeom(k: f64, n: f64, kk: f64, nn: f64, cum: bool) -> R<f64> {
    if [k, n, kk, nn].iter().any(|v| *v >= 2147483647.0) {
        return Err(XlError::Num);
    }
    if k < 0.0 || n < 0.0 || kk < 0.0 || nn <= 0.0 || n > nn || kk > nn {
        return Err(XlError::Num);
    }
    let lo = (n + kk - nn).max(0.0);
    let hi = n.min(kk);
    if !cum {
        if k < lo || k > hi {
            return Ok(0.0);
        }
        return Ok(hypgeom_pmf(k, n, kk, nn));
    }
    if k >= hi {
        return Ok(1.0);
    }
    if k < lo {
        return Ok(0.0);
    }

    if k > x87::mul(n, x87::div(kk, nn)) {
        let k2 = x87::sub(x87::sub(n, k), 1.0);
        let kk2 = x87::sub(nn, kk);
        if k2 < (n + kk2 - nn).max(0.0) {
            return Ok(1.0);
        }
        return Ok(x87::sub(1.0, hypgeom_cdf_sum(k2, n, kk2, nn)));
    }
    Ok(hypgeom_cdf_sum(k, n, kk, nn))
}

pub fn hypgeom_dist(ctx: &mut EvalCtx, a: &[Value]) -> Value {
    fin((|| {
        let k = int(ctx, a, 0)?;
        let n = int(ctx, a, 1)?;
        let kk = int(ctx, a, 2)?;
        let nn = int(ctx, a, 3)?;
        let cum = flag(a, 4)?;
        hypgeom(k, n, kk, nn, cum)
    })())
}

pub fn hypgeomdist(ctx: &mut EvalCtx, a: &[Value]) -> Value {
    fin((|| {
        let k = int(ctx, a, 0)?;
        let n = int(ctx, a, 1)?;
        let kk = int(ctx, a, 2)?;
        let nn = int(ctx, a, 3)?;
        hypgeom(k, n, kk, nn, false)
    })())
}

const XL_SFERR: [f64; 6] = [
    0.0810614667953272,
    0.0413406959554092,
    0.0276779256849983,
    0.020790672103765,
    0.0166446911898211,
    0.0138761288230707,
];

const XL_STIRL: [f64; 10] = [
    -1.3924322169059,
    0.17964437236883057,
    -0.029550653594771242,
    0.00641025641025641,
    -0.0019175269175269176,
    0.0008417508417508417,
    -0.0005952380952380953,
    0.0007936507936507937,
    -0.002777777777777778,
    0.08333333333333333,
];

fn xl_atanh(v: f64) -> f64 {
    let w = x87::mul(v, v);
    let (mut term, mut odd, mut sum) = (v, 1.0, 0.0);
    for _k in 0..1000 {
        let next = odd + 2.0;
        term = x87::mul(x87::mul(x87::div(odd, next), w), term);
        sum = x87::add(sum, term);
        let thr = x87::mul(sum, f64::EPSILON);
        odd = next;
        if !(term.abs() > thr.abs()) {
            break;
        }
    }
    x87::add(sum, v)
}

pub(crate) fn xl_log1p(y: f64) -> f64 {
    if y.abs() < 0.375 {
        x87::mul(xl_atanh(x87::div(y, x87::add(y, 2.0))), 2.0)
    } else {
        x87::ln(x87::add(1.0, y))
    }
}

pub(super) fn stirlerr(n: f64) -> f64 {
    if n == n.floor() && (1.0..=6.0).contains(&n) {
        return XL_SFERR[n as usize - 1];
    }
    let (mut n, mut acc) = (n, 0.0);
    while n < 6.0 && n > 0.0 {
        let l = xl_log1p(x87::div(1.0, n));
        acc = x87::add(acc, x87::sub(x87::mul(x87::add(n, 0.5), l), 1.0));
        n = x87::add(n, 1.0);
    }
    let x = x87::div(1.0, n);
    let s = x87::mul(sp::horner_x87(&XL_STIRL, x87::mul(x, x)), x);
    x87::add(s, acc)
}

fn bd0(x: f64, np: f64) -> f64 {
    let d = x87::add(x, np);
    let diff = x87::sub(x, np);
    let v = x87::div(diff, d);

    let t = 0.2 * d;
    if diff.abs() < t || (diff.abs() == t && 0.2f64.mul_add(d, -t) > 0.0) {
        let x2 = x87::mul(x, 2.0);
        let c = x87::sub(x87::mul(xl_atanh(v), x2), x87::mul(x2, v));
        return x87::add(x87::mul(v, diff), c);
    }
    x87::sub(x87::mul(x87::ln(x87::div(x, np)), x), diff)
}

pub(super) fn lbeta_xl(a: f64, b: f64) -> f64 {
    let (p, q) = if a < b { (a, b) } else { (b, a) };
    if p == 1.0 || q == 1.0 {
        return -x87::ln(if p == 1.0 { q } else { p });
    }
    let apb = x87::add(p, q);
    if q <= 8.0 {
        let t = x87::add(-sp::lgamma_xl(apb), sp::lgamma_xl(q));
        return x87::add(sp::lgamma_xl(p), t);
    }
    let l = xl_log1p(x87::div(p, q));
    let d = x87::sub(stirlerr(q), stirlerr(apb));
    if p > 8.0 {
        let a = x87::mul(x87::mul(xl_log1p(x87::div(q, p)), p), -1.0);
        let mut t = x87::sub(a, x87::mul(q, l));
        t = x87::add(t, LN_SQRT_2PI);
        let h = x87::mul(x87::ln(x87::div(apb, x87::mul(p, q))), 0.5);
        t = x87::add(t, h);
        t = x87::add(t, stirlerr(p));
        return x87::add(t, d);
    }
    let mut t = x87::add(sp::lgamma_xl(p), d);
    t = x87::add(t, x87::mul(l, 0.5));
    t = x87::add(t, p);
    t = x87::sub(t, x87::mul(x87::ln(apb), p));
    x87::sub(t, x87::mul(l, q))
}

pub(super) struct XlLogSum {
    pub(super) s: f64,
    pub(super) err: f64,
}

impl XlLogSum {
    pub(super) fn new(s: f64) -> Self {
        XlLogSum { s, err: 0.0 }
    }
    pub(super) fn add(&mut self, b: f64) {
        let a = self.s;
        let s = x87::add(a, b);
        let bb = x87::sub(s, a);
        let e = x87::add(x87::sub(a, x87::sub(s, bb)), x87::sub(b, bb));
        self.err = x87::add(self.err, e);
        self.s = s;
    }
    pub(super) fn exp(&self) -> f64 {
        x87::mul(x87::exp(self.err), x87::exp(self.s))
    }
}

const LN_SQRT_2PI: f64 = 0.9189385332046728;

fn xl_dbinom_log(x: f64, n: f64, p: f64) -> XlLogSum {
    let nx = x87::sub(n, x);
    let mut lc = x87::sub(stirlerr(n), stirlerr(x));
    lc = x87::sub(lc, bd0(x, x87::mul(n, p)));
    lc = x87::sub(lc, stirlerr(nx));
    let q = x87::sub(1.0, p);
    lc = x87::sub(lc, bd0(nx, x87::mul(n, q)));
    let mut s = XlLogSum::new(lc);
    s.add(x87::mul(x87::ln(n), 0.5));
    s.add(x87::mul(x87::ln(x), -0.5));
    s.add(x87::mul(x87::ln(nx), -0.5));
    s.add(-LN_SQRT_2PI);
    s
}

fn poisson_pmf(k: f64, l: f64) -> f64 {
    if l == 0.0 {
        return if k == 0.0 { 1.0 } else { 0.0 };
    }
    if k == 0.0 {
        return x87::exp(-l);
    }
    if k <= 1.1 || l <= 1.1 {
        let mut s = XlLogSum::new(x87::mul(k, x87::ln(l)));
        s.add(-l);
        let e1 = s.err;
        s.err = 0.0;
        s.add(-sp::lgamma_xl(x87::add(k, 1.0)));
        return x87::mul(x87::mul(x87::exp(e1), x87::exp(s.s)), x87::exp(s.err));
    }
    let e = x87::exp(-x87::add(stirlerr(k), bd0(k, l)));
    x87::div(e, x87::sqrt(x87::mul(6.283185307179586, k)))
}

pub(super) fn xl_pow(x: f64, y: f64) -> f64 {
    x87::exp(x87::mul(x87::ln(x), y))
}

pub(crate) fn xl_expm1(v: f64) -> f64 {
    if v.abs() < 1.0 {
        x87::expm1(v)
    } else {
        x87::add(x87::exp(v), -1.0)
    }
}

const FRAC_1_SQRT_PI: f64 = 0.5641895835477563;

pub(super) fn xl_igamma(a: f64, y: f64) -> Option<(f64, f64)> {
    if !(a > 0.0 && a.is_finite() && y > 0.0 && y.is_finite()) || a == 0.5 {
        return None;
    }
    let flip = |v: f64| x87::add(x87::sub(0.5, v), 0.5);
    if a == 1.0 {
        let p = x87::sub(0.0, xl_expm1(-y));
        return Some((p, x87::exp(-y)));
    }

    if a > 20.0 && y < 1.1 {
        let p = x87::mul(xl_igamma_lower_cf(a, y), poisson_pmf(a, y));
        return Some((p, flip(p)));
    }
    if a > 20.0 {
        let mu = x87::sub(x87::div(y, a), 1.0).abs();
        if !(mu > 0.3) {
            let v = xl_temme(a, y);
            return Some(if y > a { (flip(v), v) } else { (v, flip(v)) });
        }
        if y > a {
            let mut k = x87::sub(a, 1.0);
            let mut term = x87::div(k, y);
            let mut sum = x87::add(term, 1.0);
            for _ in 0..100000 {
                k = x87::sub(k, 1.0);
                term = x87::mul(x87::div(k, y), term);
                sum = x87::add(sum, term);
                if x87::mul(sum, f64::EPSILON).abs() > term.abs() {
                    break;
                }
            }
            let q = x87::mul(x87::mul(x87::div(a, y), sum), poisson_pmf(a, y));
            return Some((flip(q), q));
        }
    }

    let int = a == a.floor() && y <= 30.0;
    let half = (a - 0.5) == (a - 0.5).floor() && y <= 30.0;
    if a < 1.0 && y < 1.1 {
        if y < 0.5 && a > x87::div(-std::f64::consts::LN_2, x87::ln(y))
            || y > 0.5 && a > x87::add(y, 0.25)
        {
            let p = x87::mul(xl_igamma_lower_cf(a, y), poisson_pmf(a, y));
            return Some((p, flip(p)));
        }
        let q = xl_igamma_smallx_q(a, y);
        return Some((flip(q), q));
    }

    if a > 1.0 && !(y > a) {
        let mut r = x87::add(a, 1.0);
        let mut term = x87::div(y, r);
        let mut sum = x87::add(term, 1.0);
        for _ in 0..10000 {
            r = x87::add(r, 1.0);
            term = x87::mul(term, x87::div(y, r));
            sum = x87::add(sum, term);
            if !(term > x87::mul(sum, f64::EPSILON)) {
                break;
            }
        }
        let p = x87::mul(sum, poisson_pmf(a, y));
        return Some((p, flip(p)));
    }
    let q = if int {
        let n = a as usize;
        let c = &XL_INV_FACT[..n];
        let mut p = c[n - 1];
        for k in (0..n - 1).rev() {
            p = x87::add(x87::mul(p, y), c[k]);
        }
        x87::mul(x87::exp(-y), p)
    } else if half {
        let n = (a - 0.5) as usize;
        let c = &XL_HALF_COEF[..=n];
        let mut p = c[n];
        for k in (0..n).rev() {
            p = x87::add(x87::mul(p, y), c[k]);
        }
        let g = x87::add(y, x87::mul(x87::ln(y), 0.5));
        let t = x87::mul(x87::mul(x87::exp(-g), FRAC_1_SQRT_PI), p);
        x87::add(sp::erfc(x87::sqrt(y)), t)
    } else {
        let dp = poisson_pmf(a, y);
        let (mut f, mut c, mut d) = (1e-30, 1e-30, 0.0);
        let (mut an, mut b) = (a, y);
        for i in 1..20000u32 {
            d = x87::div(1.0, x87::add(b, x87::mul(an, d)));
            c = x87::add(b, x87::div(an, c));
            let delta = x87::mul(c, d);
            f = x87::mul(f, delta);
            if delta == 1.0 {
                break;
            }
            let k = ((i + 1) / 2) as f64;
            (an, b) = if i % 2 == 1 {
                (x87::sub(k, a), 1.0)
            } else {
                (k, y)
            };
        }
        x87::mul(f, dp)
    };
    Some((flip(q), q))
}

const XL_INV_FACT: [f64; 20] = [
    1.0,
    1.0,
    0.5,
    0.16666666666666666,
    0.041666666666666664,
    0.008333333333333333,
    0.001388888888888889,
    0.0001984126984126984,
    2.48015873015873e-05,
    2.7557319223985893e-06,
    2.755731922398589e-07,
    2.505210838544172e-08,
    2.08767569878681e-09,
    1.6059043836821613e-10,
    1.1470745597729725e-11,
    7.647163731819816e-13,
    4.779477332387385e-14,
    2.8114572543455206e-15,
    1.5619206968586225e-16,
    8.22063524662433e-18,
];

const XL_HALF_COEF: [f64; 20] = [
    0.0,
    2.0,
    1.3333333333333333,
    0.5333333333333333,
    0.1523809523809524,
    0.033862433862433865,
    0.0061568061568061565,
    0.0009472009472009472,
    0.00012629345962679296,
    1.4858054073740349e-05,
    1.5640056919726682e-06,
    1.4895292304501602e-07,
    1.2952428090870958e-08,
    1.0361942472696766e-09,
    7.675512942738345e-11,
    5.293457201888515e-12,
    3.4151336786377513e-13,
    2.0697779870531825e-14,
    1.1827302783161044e-15,
    6.39313663954651e-17,
];

pub(super) fn xl_log1pmx(x: f64) -> f64 {
    if x == 0.0 {
        return 0.0;
    }
    if !(x > -0.99 && x < 1.0) {
        return x87::sub(x87::ln(x87::add(x, 1.0)), x);
    }
    let r = x87::div(x, x87::add(x, 2.0));
    let y = x87::mul(r, r);
    let mut t = x87::div(x87::mul(y, 2.0), 3.0);
    let (mut sum, mut k) = (t, 3.0);
    loop {
        let k2 = x87::add(k, 2.0);
        t = x87::mul(x87::div(x87::mul(y, k), k2), t);
        sum = x87::add(sum, t);
        k = k2;
        if !(t > x87::mul(sum, f64::EPSILON)) {
            break;
        }
    }
    x87::mul(x87::sub(sum, x), r)
}

const XL_TEMME: [f64; 30] = [
    -4.770037049820485e-18,
    1.1686939738559576e-17,
    2.5305430097478883e-18,
    -1.6522531216398162e-16,
    8.099521156704561e-16,
    -1.9752288294349442e-15,
    -5.139111834242572e-16,
    2.8534893807047445e-14,
    -1.392388722418162e-13,
    3.371763262400985e-13,
    1.1004392031956135e-13,
    -5.0276692801141755e-12,
    2.4361948020667415e-11,
    -5.830772132550426e-11,
    -2.5514193994946248e-11,
    9.14769958223679e-10,
    -4.382036018453353e-09,
    1.0261809784240309e-08,
    6.707853543401498e-09,
    -1.7665952736826078e-07,
    8.296711340953087e-07,
    -1.85406221071516e-06,
    -2.185448510679992e-06,
    3.919263178522438e-05,
    -0.0001787551440329218,
    0.0003527336860670194,
    0.0011574074074074073,
    -0.014814814814814815,
    0.08333333333333333,
    -0.3333333333333333,
];

fn xl_temme(a: f64, y: f64) -> f64 {
    let mu = x87::div(x87::sub(y, a), a);
    let l = xl_log1pmx(mu);
    let mut eta = x87::sqrt(x87::mul(l, -2.0));
    if y < a {
        eta = -eta;
    }
    let u = -x87::mul(a, l);
    let mut b = XL_TEMME;
    let mut k = 29.0;
    for i in 2..30 {
        b[i] = x87::add(x87::div(x87::mul(b[i - 2], k), a), XL_TEMME[i]);
        k = x87::sub(k, 1.0);
    }
    let s = b[1..]
        .iter()
        .fold(b[0], |acc, &c| x87::add(x87::mul(acc, eta), c));
    let s = x87::div(s, x87::exp(stirlerr(a)));
    let t = x87::div(x87::mul(x87::exp(-u), 0.3989422804014327), x87::sqrt(a));
    let mut r = x87::mul(t, s);
    if !(y > a) {
        r = -r;
    }
    x87::add(r, x87::mul(0.5, sp::erfc(x87::sqrt(u))))
}

fn xl_gam1(a: f64) -> f64 {
    const P: [f64; 7] = [
        5.89597428611429e-4,
        -5.14889771323592e-3,
        7.66968181649490e-3,
        5.97275330452234e-2,
        -2.30975380857675e-1,
        -4.09078193005776e-1,
        5.77215664901533e-1,
    ];
    const Q: [f64; 5] = [
        4.23244297896961e-3,
        2.61132021441447e-2,
        1.58451672430138e-1,
        4.27569613095214e-1,
        1.0,
    ];
    const R: [f64; 9] = [
        -1.32674909766242e-4,
        2.66505979058923e-4,
        2.23047661158249e-3,
        -1.18290993445146e-2,
        9.30357293360349e-4,
        1.18378989872749e-1,
        -2.44757765222226e-1,
        -7.71330383816272e-1,
        -4.22784335098468e-1,
    ];
    const S: [f64; 3] = [5.59398236957378e-2, 2.73076135303957e-1, 1.0];

    if a < 0.5 {
        return x87::div(x87::mul(a, sp::horner_x87(&P, a)), sp::horner_x87(&Q, a));
    }
    let t = x87::sub(a, 1.0);
    let bot = sp::horner_x87(&S, t);
    let top = sp::horner_x87(&R, t);
    x87::div(x87::mul(x87::div(t, a), top), bot)
}

fn xl_igamma_smallx_q(a: f64, y: f64) -> f64 {
    let my = x87::mul(y, -1.0);
    let mut c = x87::mul(a, y);
    let mut s = x87::div(c, x87::add(a, 1.0));
    let mut n = 2.0;
    for _ in 0..10000 {
        c = x87::mul(c, x87::div(my, n));
        let term = x87::div(c, x87::add(a, n));
        s = x87::add(s, term);
        let thr = x87::mul(s, f64::EPSILON);
        n = x87::add(n, 1.0);
        if !(term.abs() > thr.abs()) {
            break;
        }
    }
    let z = x87::mul(a, x87::ln(y));
    let w = x87::exp(z);
    let l = x87::expm1(z);
    let t = x87::sub(x87::mul(s, w), l);
    let g = x87::mul(a, x87::exp(sp::lgamma_xl(a)));
    x87::sub(x87::div(t, g), xl_gam1(a))
}

fn xl_igamma_lower_cf(a: f64, y: f64) -> f64 {
    let my = x87::sub(0.0, y);
    let (mut f, mut c, mut d) = (1e-30, 1e-30, 0.0);
    let (mut an, mut b, mut n) = (a, a, 0.0);
    for _ in 0..20000 {
        d = x87::div(1.0, x87::add(b, x87::mul(an, d)));
        c = x87::add(b, x87::div(an, c));
        let delta = x87::mul(c, d);
        f = x87::mul(f, delta);
        if delta == 1.0 {
            break;
        }
        an = x87::mul(my, x87::add(a, n));
        n = x87::add(n, 1.0);
        b = x87::add(n, x87::add(a, y));
    }
    f
}

fn igamma_pq(a: f64, y: f64) -> (f64, f64) {
    match xl_igamma(a, y) {
        Some((p, q)) => (
            crate::number::flush_subnormal(p),
            crate::number::flush_subnormal(q),
        ),
        None => hp::gamma_pq(a, y),
    }
}

pub fn poisson_dist(ctx: &mut EvalCtx, a: &[Value]) -> Value {
    fin((|| {
        let k = int(ctx, a, 0)?;
        let l = num(ctx, &a[1])?;
        let cum = flag(a, 2)?;
        if k == 0.0 && l < 0.0 {
            return Ok((-l).exp());
        }
        if k < 0.0 || l < 0.0 {
            return Err(XlError::Num);
        }
        if cum {
            if l == 0.0 {
                return Ok(1.0);
            }
            return Ok(igamma_pq(k + 1.0, l).1);
        }
        Ok(poisson_pmf(k, l))
    })())
}

pub(super) fn t_inv_two_tailed(p: f64, df: f64) -> Result<f64, XlError> {
    Ok(-t_inv_xl(x87::div(p, 2.0), df)?)
}

pub(super) fn f_upper(x: f64, d1: f64, d2: f64) -> f64 {
    f_sf(x, d1, d2)
}

pub(super) fn chisq_upper(x: f64, df: f64) -> f64 {
    igamma_pq(df / 2.0, x / 2.0).1
}
