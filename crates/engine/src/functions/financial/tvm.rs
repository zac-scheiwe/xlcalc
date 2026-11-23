use super::atp::{self, Arg};
use crate::eval::EvalCtx;
use crate::functions::args::{num, opt_num, val, R};
use crate::functions::statdist::{xl_expm1, xl_log1p};
use crate::value::{Value, XlError};
use crate::x87;
use crate::xmath;

fn finite(x: f64) -> R<f64> {
    if x.is_finite() {
        Ok(x)
    } else {
        Err(XlError::NUM)
    }
}

struct A<'a, 'b> {
    ctx: &'a EvalCtx<'b>,
    a: &'a [Value],
}

impl A<'_, '_> {
    fn n(&self, i: usize) -> R<f64> {
        num(self.ctx, &self.a[i])
    }
    fn o(&self, i: usize, default: f64) -> R<f64> {
        opt_num(self.ctx, self.a, i, default)
    }

    fn t(&self, i: usize) -> R<bool> {
        Ok(self.o(i, 0.0)? != 0.0)
    }
}

fn negate_nonzero(x: f64) -> f64 {
    if x.to_bits() >> 48 == 0 {
        x
    } else {
        -x
    }
}

fn pmt_parts(r: f64, n: f64, pv: f64, fv: f64, t: bool) -> R<(f64, f64, f64)> {
    let l = xl_log1p(r);
    let e = xl_expm1(x87::mul(l, x87::sub(0.0, n)));

    if e.is_infinite() {
        return Err(XlError::NUM);
    }
    let q = negate_nonzero(x87::div(x87::add(pv, fv), e));
    let rho = if t {
        x87::div(1.0, x87::add(x87::div(1.0, r), 1.0))
    } else {
        r
    };
    Ok((q, l, rho))
}

fn periods_left(per: f64, n: f64) -> f64 {
    x87::sub(per, x87::add(n, 1.0))
}

fn growth_check(r: f64, n: f64) -> R<()> {
    let r1 = x87::add(1.0, r);
    if r1 == 0.0 && n == 0.0 {
        return Err(XlError::NUM);
    }
    if n < 0.0 && crate::xmath::pow(r1, -n) == 0.0 {
        return Err(XlError::DIV0);
    }
    Ok(())
}

fn annuity(r: f64, n: f64, t: bool) -> (f64, f64) {
    let r1 = x87::add(1.0, r);
    let g = crate::xmath::pow(r1, n);
    let mut ann = x87::div(x87::sub(g, 1.0), r);
    if t {
        ann = x87::mul(ann, r1);
    }
    (g, ann)
}

fn pmt_check(r: f64, n: f64) -> R<()> {
    if n == 0.0 || r <= -1.0 {
        return Err(XlError::NUM);
    }
    Ok(())
}

pub fn pmt_core(r: f64, n: f64, pv: f64, fv: f64, t: bool) -> R<f64> {
    pmt_check(r, n)?;
    if r == 0.0 {
        return finite(x87::div(x87::add(pv, fv), x87::sub(0.0, n)));
    }
    let (q, _, rho) = pmt_parts(r, n, pv, fv, t)?;
    finite(x87::mul(x87::add(-q, fv), rho))
}

pub fn pmt(ctx: &mut EvalCtx, a: &[Value]) -> Value {
    let a = A { ctx, a };
    val((|| {
        let (r, n, pv, fv, t) = (a.n(0)?, a.n(1)?, a.n(2)?, a.o(3, 0.0)?, a.t(4)?);
        pmt_core(r, n, pv, fv, t).map(Value::num)
    })())
}

pub fn fv_core(r: f64, n: f64, pmt: f64, pv: f64, t: bool) -> R<f64> {
    if r == 0.0 {
        return Ok(negate_nonzero(x87::add(x87::mul(n, pmt), pv)));
    }
    growth_check(r, n)?;
    let (g, ann) = annuity(r, n, t);
    if g.is_nan() {
        return Err(XlError::NUM);
    }
    finite(negate_nonzero(x87::add(
        x87::mul(pmt, ann),
        x87::mul(pv, g),
    )))
}

pub fn fv(ctx: &mut EvalCtx, a: &[Value]) -> Value {
    let a = A { ctx, a };
    val((|| {
        let (r, n, pmt, pv, t) = (a.n(0)?, a.n(1)?, a.n(2)?, a.o(3, 0.0)?, a.t(4)?);
        fv_core(r, n, pmt, pv, t).map(Value::num)
    })())
}

pub fn pv_core(r: f64, n: f64, pmt: f64, fv: f64, t: bool) -> R<f64> {
    if r == 0.0 {
        return Ok(negate_nonzero(x87::add(x87::mul(n, pmt), fv)));
    }
    growth_check(r, n)?;
    let (g, ann) = annuity(r, n, t);
    if g.is_nan() || g.is_infinite() {
        return Err(XlError::NUM);
    }
    if g == 0.0 {
        return Err(XlError::DIV0);
    }
    finite(negate_nonzero(x87::div(
        x87::add(x87::mul(pmt, ann), fv),
        g,
    )))
}

pub fn pv(ctx: &mut EvalCtx, a: &[Value]) -> Value {
    let a = A { ctx, a };
    val((|| {
        let (r, n, pmt, fv, t) = (a.n(0)?, a.n(1)?, a.n(2)?, a.o(3, 0.0)?, a.t(4)?);
        pv_core(r, n, pmt, fv, t).map(Value::num)
    })())
}

pub fn nper(ctx: &mut EvalCtx, a: &[Value]) -> Value {
    let a = A { ctx, a };
    val((|| {
        let (r, pmt, pv, fv, t) = (a.n(0)?, a.n(1)?, a.n(2)?, a.o(3, 0.0)?, a.t(4)?);
        if r == 0.0 {
            if pmt == 0.0 {
                return Err(XlError::DIV0);
            }

            return Ok(Value::num(negate_nonzero(x87::div(x87::add(pv, fv), pmt))));
        }
        if r <= -1.0 {
            return Err(XlError::NUM);
        }

        let r1 = x87::add(1.0, r);
        let pt = if t { x87::mul(pmt, r1) } else { pmt };

        let l = x87::ln(r1);
        let x = x87::div(x87::sub(pt, x87::mul(fv, r)), x87::add(pt, x87::mul(pv, r)));
        if l == 0.0 {
            return Err(XlError::DIV0);
        }
        if x <= 0.0 || !x.is_finite() {
            return Err(XlError::NUM);
        }
        finite(x87::div(x87::ln(x), l)).map(Value::num)
    })())
}

fn per_check(per: f64, n: f64) -> R<()> {
    if per < 1.0 || x87::sub(per, 1.0) >= n || n <= 0.0 {
        return Err(XlError::NUM);
    }
    Ok(())
}

pub fn ipmt(ctx: &mut EvalCtx, a: &[Value]) -> Value {
    let a = A { ctx, a };
    val((|| {
        let (r, per, n, pv, fv, t) = (a.n(0)?, a.n(1)?, a.n(2)?, a.n(3)?, a.o(4, 0.0)?, a.t(5)?);
        per_check(per, n)?;

        if t && per == 1.0 {
            return Ok(Value::num(0.0));
        }
        if r == 0.0 {
            return Ok(Value::num(0.0));
        }
        pmt_check(r, n)?;

        let (q, l, rho) = pmt_parts(r, n, pv, fv, t)?;
        let em = xl_expm1(x87::mul(l, periods_left(per, n)));
        finite(x87::mul(x87::add(fv, x87::mul(q, em)), rho)).map(Value::num)
    })())
}

pub fn ppmt(ctx: &mut EvalCtx, a: &[Value]) -> Value {
    let a = A { ctx, a };
    val((|| {
        let (r, per, n, pv, fv, t) = (a.n(0)?, a.n(1)?, a.n(2)?, a.n(3)?, a.o(4, 0.0)?, a.t(5)?);
        per_check(per, n)?;
        if r == 0.0 || (t && per == 1.0) {
            return pmt_core(r, n, pv, fv, t).map(Value::num);
        }
        pmt_check(r, n)?;

        let (q, l, rho) = pmt_parts(r, n, pv, fv, t)?;
        let c = x87::exp(x87::mul(l, periods_left(per, n)));
        finite(x87::mul(-x87::mul(q, c), rho)).map(Value::num)
    })())
}

const RATE_NEAR_M1: f64 = 5e-8;

pub fn rate(ctx: &mut EvalCtx, a: &[Value]) -> Value {
    let a = A { ctx, a };
    val((|| {
        let (n, pmt, pv, fv, t, guess) = (
            a.n(0)?,
            a.n(1)?,
            a.n(2)?,
            a.o(3, 0.0)?,
            a.t(4)?,
            a.o(5, 0.1)?,
        );

        let start = super::cash::secant_start(guess)?;
        if n == 0.0 {
            return Err(XlError::NUM);
        }

        let f = |r: f64| {
            if r == 0.0 {
                return -(pv + pmt * n + fv);
            }
            let (g, ann) = annuity(r, n, t);
            x87::sub(-x87::add(x87::mul(pmt, ann), x87::mul(pv, g)), fv)
        };
        let r = super::cash::secant_solve(start, f).ok_or(XlError::NUM)?;
        if r <= -1.0 + RATE_NEAR_M1 {
            return Err(XlError::NUM);
        }
        Ok(Value::num(r))
    })())
}

pub fn ispmt(ctx: &mut EvalCtx, a: &[Value]) -> Value {
    let a = A { ctx, a };
    val((|| {
        let (r, per, n, pv) = (a.n(0)?, a.n(1)?, a.n(2)?, a.n(3)?);

        let mut f = x87::MathFlag::default();
        let t = f.sub(n, per);
        let t = f.mul(t, pv);
        let t = f.mul(t, r);
        let t = f.div(t, n);
        f.result(if t == 0.0 { t } else { -t }).map(Value::num)
    })())
}

pub fn pduration(ctx: &mut EvalCtx, a: &[Value]) -> Value {
    let a = A { ctx, a };
    val((|| {
        let (r, pv, fv) = (a.n(0)?, a.n(1)?, a.n(2)?);
        if r <= 0.0 || pv <= 0.0 || fv <= 0.0 {
            return Err(XlError::NUM);
        }

        let l = x87::ln(x87::add(r, 1.0));
        if l == 0.0 {
            return Err(XlError::DIV0);
        }
        finite(x87::div(x87::sub(x87::ln(fv), x87::ln(pv)), l)).map(Value::num)
    })())
}

pub fn rri(ctx: &mut EvalCtx, a: &[Value]) -> Value {
    let a = A { ctx, a };
    val((|| {
        let (n, pv, fv) = (a.n(0)?, a.n(1)?, a.n(2)?);
        if n > 0.0 && pv == 0.0 && fv == 0.0 {
            return Ok(Value::num(0.0));
        }
        if n <= 0.0 || pv == 0.0 {
            return Err(XlError::NUM);
        }

        let q = x87::div(fv, pv);
        if q < 0.0 {
            return Err(XlError::NUM);
        }
        if n == 1.0 {
            return finite(x87::sub(q, 1.0)).map(Value::num);
        }
        if q == 0.0 {
            return Ok(Value::num(-1.0));
        }
        let p = x87::exp(x87::mul(x87::ln(q), x87::div(1.0, n)));
        finite(x87::sub(p, 1.0)).map(Value::num)
    })())
}

fn cum(ctx: &mut EvalCtx, a: &[Arg], principal: bool) -> Value {
    atp::out((|| {
        let r = atp::req(ctx, a, 0)?;
        let n = atp::req(ctx, a, 1)?;
        let pv = atp::req(ctx, a, 2)?;
        let start = atp::req(ctx, a, 3)?;
        let end = atp::req(ctx, a, 4)?;
        let ty = atp::req(ctx, a, 5)?;
        if r <= 0.0 || n <= 0.0 || pv <= 0.0 {
            return Err(XlError::NUM);
        }
        if start < 1.0 || end < start || end > n {
            return Err(XlError::NUM);
        }
        if ty != 0.0 && ty != 1.0 {
            return Err(XlError::NUM);
        }
        let t = ty == 1.0;

        let s = start.ceil();
        let e = end.trunc();
        if e < s {
            return Ok(0.0);
        }

        pmt_check(r, n)?;

        let bad = std::cell::Cell::new(false);
        let ck = |v: f64| {
            if !v.is_finite() {
                bad.set(true);
            }
            v
        };
        let rho = if t {
            ck(x87::div(1.0, ck(x87::add(ck(x87::div(1.0, r)), 1.0))))
        } else {
            r
        };
        let l = ck(xl_log1p(r));
        let den = ck(xl_expm1(ck(x87::mul(l, -n))));
        if den == 0.0 || den.abs() < f64::MIN_POSITIVE {
            return Err(XlError::NUM);
        }
        let a = ck(x87::div(pv, den));
        let p = ck(x87::mul(-a, rho));
        let first = t && s == 1.0;
        let s1 = if first { 2.0 } else { s };
        let k = ck(x87::sub(s1, ck(x87::add(e, 1.0))));
        let g = ck(xl_expm1(ck(x87::mul(l, k))));
        let c = ck(x87::exp(ck(x87::mul(
            l,
            ck(x87::sub(ck(x87::sub(e, n)), 1.0)),
        ))));
        let mut prin = ck(x87::mul(-a, ck(x87::mul(g, c))));
        if !t {
            prin = ck(x87::mul(prin, ck(x87::add(1.0, r))));
        }
        let out = if !principal {
            ck(x87::sub(ck(x87::mul(p, k)), prin))
        } else {
            if first {
                prin = ck(x87::sub(prin, p));
            }
            prin
        };
        if bad.get() {
            return Err(XlError::NUM);
        }
        Ok(out)
    })())
}

pub fn cumipmt(ctx: &mut EvalCtx, a: &[Arg]) -> Value {
    cum(ctx, a, false)
}

pub fn cumprinc(ctx: &mut EvalCtx, a: &[Arg]) -> Value {
    cum(ctx, a, true)
}

pub fn effect(ctx: &mut EvalCtx, a: &[Arg]) -> Value {
    atp::out((|| {
        let nominal = atp::req(ctx, a, 0)?;
        let p = atp::req(ctx, a, 1)?.trunc();
        if nominal <= 0.0 || p < 1.0 {
            return Err(XlError::NUM);
        }

        let base = x87::add(x87::div(nominal, p), 1.0);
        if p >= 4_294_967_295.0 {
            return finite(x87::sub(crate::xmath::pow(base, p), 1.0));
        }
        let (mut r, mut a, mut n) = (1.0, base, p as u64);
        while n != 0 {
            if n & 1 == 1 {
                r = x87::mul(r, a);
            }
            n >>= 1;
            if n != 0 {
                a = x87::mul(a, a);
            }
        }
        finite(x87::sub(r, 1.0))
    })())
}

pub fn nominal(ctx: &mut EvalCtx, a: &[Arg]) -> Value {
    atp::out((|| {
        let eff = atp::req(ctx, a, 0)?;
        let p = atp::req(ctx, a, 1)?.trunc();
        if eff <= 0.0 || p < 1.0 {
            return Err(XlError::NUM);
        }

        finite(x87::mul(
            x87::sub(xmath::pow(x87::add(1.0, eff), x87::div(1.0, p)), 1.0),
            p,
        ))
    })())
}

pub fn fvschedule(ctx: &mut EvalCtx, a: &[Arg]) -> Value {
    atp::out((|| {
        let mut acc = atp::req(ctx, a, 0)?;
        let rates: Vec<Value> = match a.get(1) {
            Some(Some(Value::Array(arr))) => arr.data.clone(),
            Some(Some(v)) => vec![v.clone()],
            _ => return Err(XlError::NA),
        };
        for v in &rates {
            let r = match v {
                Value::Number(x) => *x,
                Value::Blank => 0.0,
                Value::Error(e) => return Err(*e),
                Value::Text(_) => match a.get(1) {
                    Some(Some(Value::Text(_))) => atp::num_of(ctx, v)?,
                    _ => return Err(XlError::VALUE),
                },
                _ => return Err(XlError::VALUE),
            };

            acc = crate::x87::mul(crate::x87::add(r, 1.0), acc);
            if acc.is_infinite() {
                acc = 0.0;
            }
        }
        finite(acc)
    })())
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn helpers_follow_the_routine_branches() {
        let t = 2f64.powi(-52);
        assert_eq!(xl_expm1(t), t);
        assert_eq!(xl_log1p(0.0), 0.0);
        assert_eq!(xl_log1p(1e-20), 1e-20);

        let e = xl_expm1(1.0);
        assert!((e - (std::f64::consts::E - 1.0)).abs() < 1e-15);
    }

    #[test]
    fn zero_rate_arms_flip_sign_like_the_routines() {
        assert_eq!(negate_nonzero(0.0).to_bits(), 0);
        assert_eq!(negate_nonzero(2.0), -2.0);
        assert_eq!(fv_core(0.0, 3.0, 0.0, 0.0, false).unwrap().to_bits(), 0);
        assert_eq!(pv_core(0.0, 3.0, 2.0, 1.0, false).unwrap(), -7.0);
        assert_eq!(pmt_core(0.0, 4.0, 100.0, 0.0, false).unwrap(), -25.0);
    }
}
