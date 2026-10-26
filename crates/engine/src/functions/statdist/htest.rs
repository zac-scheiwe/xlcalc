use super::desc::pairs;
use super::dist::{chisq_upper, f_upper, t_2t, t_inv_two_tailed};
use super::special::norm_cdf;
use super::*;
use crate::x87;

fn sample_var(v: &[f64]) -> (f64, f64) {
    let m = mean(v);
    let mut ss = 0.0;
    for &x in v {
        ss += (x - m) * (x - m);
    }
    (m, ss / (v.len() as f64 - 1.0))
}

fn xl_sample_var(v: &[f64]) -> f64 {
    let sum = v.iter().fold(0.0, |s, &x| x87::add(s, x));
    let m = x87::div(sum, v.len() as f64);
    let ss = v.iter().fold(0.0, |s, &x| {
        let d = x87::sub(x, m);
        x87::add(s, x87::mul(d, d))
    });
    x87::div(ss, v.len() as f64 - 1.0)
}

pub fn confidence_norm(ctx: &mut EvalCtx, a: &[Value]) -> Value {
    fin((|| {
        let alpha = num(ctx, &a[0])?;
        let sd = num(ctx, &a[1])?;
        let n = num(ctx, &a[2])?.trunc();
        if alpha <= 0.0 || alpha >= 1.0 || sd <= 0.0 || n < 1.0 {
            return Err(XlError::Num);
        }

        let z = super::invnorm::norm_s_inv(x87::sub(1.0, x87::div(alpha, 2.0)));
        Ok(x87::mul(z, x87::div(sd, x87::sqrt(n))))
    })())
}

pub fn confidence_t(ctx: &mut EvalCtx, a: &[Value]) -> Value {
    fin((|| {
        let alpha = num(ctx, &a[0])?;
        let sd = num(ctx, &a[1])?;
        let n = num(ctx, &a[2])?.trunc();
        if alpha <= 0.0 || alpha >= 1.0 || sd <= 0.0 || n < 1.0 {
            return Err(XlError::Num);
        }
        if n == 1.0 {
            return Err(XlError::Div0);
        }

        Ok(x87::mul(
            x87::div(sd, x87::sqrt(n)),
            t_inv_two_tailed(alpha, n - 1.0)?,
        ))
    })())
}

pub fn z_test(ctx: &mut EvalCtx, o: &[Operand]) -> Operand {
    opfin((|| {
        if matches!(&o[0], Operand::Val(v @ Value::Text(_)) if ctx.to_number(v).is_err()) {
            return Err(XlError::Value);
        }
        let x = opnum(ctx, &o[1])?;
        let sigma = match o.get(2) {
            None => None,

            Some(op) => Some(opnum(ctx, op)?),
        };
        let v = collect_numbers(ctx, &o[..1], SUM_POLICY)?;
        if v.is_empty() {
            return Err(XlError::NA);
        }
        let n = v.len() as f64;
        if n < 2.0 {
            return Err(XlError::Div0);
        }
        if let Some(s) = sigma {
            if s <= 0.0 {
                return Err(XlError::Num);
            }
        }

        let m = x87::div(v.iter().fold(0.0, |s, &x| x87::add(s, x)), n);
        let s = match sigma {
            Some(s) => s,
            None => x87::sqrt(xl_sample_var(&v)),
        };
        if s == 0.0 {
            return Err(XlError::Div0);
        }
        let z = x87::div(x87::mul(x87::sub(m, x), x87::sqrt(n)), s);
        Ok(norm_cdf(-z))
    })())
}

fn int_op(ctx: &EvalCtx, o: &Operand) -> R<f64> {
    Ok(crate::functions::args::sfix(opnum(ctx, o)?) as f64)
}

pub fn t_test(ctx: &mut EvalCtx, o: &[Operand]) -> Operand {
    opfin((|| {
        check_array_args(ctx, &[&o[0], &o[1]])?;
        let tails = int_op(ctx, &o[2])?;
        let kind = int_op(ctx, &o[3])?;
        if !(tails == 1.0 || tails == 2.0) || !(1.0..=3.0).contains(&kind) {
            return Err(XlError::Num);
        }
        let (t, df) = if kind == 1.0 {
            let (x, y) = pairs(ctx, &o[0], &o[1])?;
            let n = x.len() as f64;
            if n < 2.0 {
                return Err(XlError::Div0);
            }

            let sum = |v: &[f64]| v.iter().fold(0.0, |s, &a| x87::add(s, a));
            let (mx, my) = (x87::div(sum(&x), n), x87::div(sum(&y), n));
            let (mut sxy, mut sxx, mut syy) = (0.0, 0.0, 0.0);
            for (&a, &b) in x.iter().zip(&y) {
                let (dx, dy) = (x87::sub(a, mx), x87::sub(b, my));
                sxy = x87::add(sxy, x87::mul(dy, dx));
                sxx = x87::add(sxx, x87::mul(dx, dx));
                syy = x87::add(syy, x87::mul(dy, dy));
            }
            let n1 = x87::sub(n, 1.0);
            let cxy = x87::mul(2.0, x87::div(sxy, n1));
            let var = x87::add(x87::sub(x87::div(sxx, n1), cxy), x87::div(syy, n1));
            if var == 0.0 {
                return Err(XlError::Div0);
            }
            (x87::div(x87::sub(mx, my), x87::sqrt(x87::div(var, n))), n1)
        } else {
            let x = numbers_only(ctx, &o[0])?;
            let y = numbers_only(ctx, &o[1])?;
            let (n1, n2) = (x.len() as f64, y.len() as f64);
            if n1 < 2.0 || n2 < 2.0 {
                return Err(XlError::Div0);
            }
            let (m1, v1) = sample_var(&x);
            let (m2, v2) = sample_var(&y);
            if kind == 2.0 {
                let df = n1 + n2 - 2.0;
                let sp2 = ((n1 - 1.0) * v1 + (n2 - 1.0) * v2) / df;
                let se = (sp2 * (1.0 / n1 + 1.0 / n2)).sqrt();
                if se == 0.0 {
                    return Err(XlError::Div0);
                }
                ((m1 - m2) / se, df)
            } else {
                let a = v1 / n1;
                let b = v2 / n2;
                if a + b == 0.0 {
                    return Err(XlError::Div0);
                }
                let df = (a + b) * (a + b) / (a * a / (n1 - 1.0) + b * b / (n2 - 1.0));
                ((m1 - m2) / (a + b).sqrt(), df)
            }
        };

        let t2 = x87::mul(t, t);
        let bx = x87::div(df, x87::add(t2, df));
        let p = match super::ibeta::xl_ibeta(bx, x87::add(1.0, -bx), x87::div(df, 2.0), 0.5) {
            Some((w, _)) if t != 0.0 => w,
            _ => t_2t(t.abs(), df),
        };
        Ok(if tails == 2.0 { p } else { x87::div(p, 2.0) })
    })())
}

pub fn f_test(ctx: &mut EvalCtx, o: &[Operand]) -> Operand {
    opfin((|| {
        check_array_args(ctx, &[&o[0], &o[1]])?;
        let x = numbers_only(ctx, &o[0])?;
        let y = numbers_only(ctx, &o[1])?;
        if x.len() < 2 || y.len() < 2 {
            return Err(XlError::Div0);
        }
        let v1 = xl_sample_var(&x);
        let v2 = xl_sample_var(&y);
        if v1 == 0.0 || v2 == 0.0 {
            return Err(XlError::Div0);
        }
        let (d1, d2) = (x.len() as f64 - 1.0, y.len() as f64 - 1.0);

        let (f, dn, dd) = if v1 > v2 {
            (x87::div(v1, v2), d1, d2)
        } else {
            (x87::div(v2, v1), d2, d1)
        };
        let t = x87::mul(dn, f);
        let s = x87::add(t, dd);
        let (bx, by) = (x87::div(dd, s), x87::div(t, s));
        let w = match super::ibeta::xl_ibeta(bx, by, x87::div(dd, 2.0), x87::div(dn, 2.0)) {
            Some((w, _)) => w,
            None => f_upper(f, dn, dd),
        };
        let p = x87::mul(w, 2.0);
        Ok(if p > 1.0 { x87::sub(2.0, p) } else { p })
    })())
}

pub fn chisq_test(ctx: &mut EvalCtx, o: &[Operand]) -> Operand {
    opfin((|| {
        check_array_args(ctx, &[&o[0], &o[1]])?;
        let (rows, cols, act) = grid_values(ctx, &o[0])?;
        let exp = flat_values(ctx, &o[1])?;
        if act.len() != exp.len() {
            return Err(XlError::NA);
        }
        let mut chi = 0.0;
        for (a, e) in act.iter().zip(exp.iter()) {
            if let Value::Error(err) = a {
                return Err(*err);
            }
            if let Value::Error(err) = e {
                return Err(*err);
            }
            if let (Value::Number(a), Value::Number(e)) = (a, e) {
                if *e < 0.0 {
                    return Err(XlError::Num);
                }
                if *e == 0.0 {
                    return Err(XlError::Div0);
                }
                chi += (a - e) * (a - e) / e;
            }
        }
        let df = if rows > 1 && cols > 1 {
            (rows as f64 - 1.0) * (cols as f64 - 1.0)
        } else {
            (rows * cols) as f64 - 1.0
        };
        if df < 1.0 {
            return Err(XlError::NA);
        }
        Ok(chisq_upper(chi, df))
    })())
}
