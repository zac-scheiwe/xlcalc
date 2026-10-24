use super::atp::{self, Arg};
use super::xmath;
use crate::eval::{EvalCtx, Operand};
use crate::functions::args::{collect_numbers, num, opval, Grid, R, SUM_POLICY};
use crate::value::{Value, XlError};

fn finite(x: f64) -> R<f64> {
    if x.is_finite() {
        Ok(x)
    } else {
        Err(XlError::Num)
    }
}

fn flow_values(ctx: &mut EvalCtx, op: &Operand) -> R<Vec<f64>> {
    let g = Grid::from_operand(op)?;
    let mut out = Vec::new();
    for r in 0..g.rows() {
        for c in 0..g.cols() {
            match g.get(ctx, r, c) {
                Value::Number(n) => out.push(n),
                Value::Error(e) => return Err(e),
                _ => {}
            }
        }
    }
    Ok(out)
}

fn scalar_num(ctx: &EvalCtx, op: &Operand, default: f64) -> R<f64> {
    match op {
        Operand::Val(v) => num(ctx, v),
        Operand::Missing => Ok(default),
        Operand::Ref(_) => Err(XlError::Value),
    }
}

pub fn npv_of(rate: f64, values: &[f64]) -> R<f64> {
    let b = 1.0 + rate;
    if b == 0.0 {
        return Err(XlError::Div0);
    }
    let mut acc = 0.0;
    let mut d = 1.0;
    for v in values {
        acc += v / d;
        d *= b;
    }
    finite(acc / b)
}

pub fn npv(ctx: &mut EvalCtx, ops: &[Operand]) -> Operand {
    opval((|| {
        let rate = scalar_num(ctx, &ops[0], 0.0)?;
        let values = collect_numbers(ctx, &ops[1..], SUM_POLICY)?;
        npv_of(rate, &values).map(Value::num)
    })())
}

fn irr_secant(values: &[f64], guess: f64) -> Option<f64> {
    use crate::x87;
    let npv = |w: f64| {
        let b = x87::div(1.0, x87::add(w, 1.0));
        let mut d = 1.0;
        let mut acc = values[0];
        for v in &values[1..] {
            d = x87::mul(d, b);
            acc = x87::add(acc, x87::div(*v, d));
        }
        acc
    };
    let w = fn_iter(x87::div(guess, x87::sub(-1.0, guess)), npv)?;
    let r = x87::sub(x87::div(1.0, x87::add(w, 1.0)), 1.0);
    r.is_finite().then_some(r)
}

pub(super) fn fn_iter(x0: f64, f: impl Fn(f64) -> f64) -> Option<f64> {
    use crate::x87;
    let npv = f;
    let mut x0 = x0;
    let mut f0 = npv(x0);
    let mut x1 = if f0 > 0.0 {
        x87::add(x0, 0.001)
    } else {
        x87::sub(x0, 0.001)
    };
    let mut f1 = npv(x1);
    let mut step = f64::INFINITY;
    for _ in 0..200 {
        if !f1.is_finite() {
            return None;
        }
        if f1 == f0 {
            x0 = if x87::sub(x0, x1) > 0.0 {
                x87::sub(x0, 0.001)
            } else {
                x87::add(x0, 0.001)
            };
            f0 = npv(x0);
        }
        let q = x87::div(x87::mul(x87::sub(x1, x0), f1), x87::sub(f1, f0));
        let x2 = x87::sub(x1, q);
        if !x2.is_finite() {
            return None;
        }
        let f2 = npv(x2);
        step = (x2 - x1).abs();
        if f2.abs() < 1e-7 && step < 1e-7 {
            return Some(x2);
        }
        (x0, f0, x1, f1) = (x1, f1, x2, f2);
    }
    if f1.abs() < 1e-4 && step < 1e-4 {
        return Some(x1);
    }
    None
}

fn irr_of(values: &[f64], guess: Option<f64>) -> R<f64> {
    if !values.iter().any(|&v| v > 0.0) || !values.iter().any(|&v| v < 0.0) {
        return Err(XlError::Num);
    }
    let restarts = [0.1, -0.09090909090909091, 2.0];
    let first = guess.as_ref().map_or(&[][..], std::slice::from_ref);
    let runs = first.iter().chain(&restarts);

    runs.filter_map(|&g| irr_secant(values, g))
        .next()
        .ok_or(XlError::Num)
}

pub fn irr(ctx: &mut EvalCtx, ops: &[Operand]) -> Operand {
    opval((|| {
        if let Operand::Val(Value::Error(e)) = &ops[0] {
            return Err(*e);
        }
        let values = flow_values(ctx, &ops[0]).map_err(|_| XlError::Value)?;
        let guess = match ops.get(1) {
            None => None,

            Some(op) => Some(scalar_num(ctx, op, 0.0)?),
        };
        if guess.is_some_and(|g| g <= -1.0) {
            return Err(XlError::Value);
        }
        irr_of(&values, guess).map(Value::num)
    })())
}

pub fn mirr(ctx: &mut EvalCtx, ops: &[Operand]) -> Operand {
    opval((|| {
        if matches!(ops[0], Operand::Missing) {
            return Err(XlError::Value);
        }
        let values = flow_values(ctx, &ops[0])?;
        let fr = scalar_num(ctx, &ops[1], 0.0)?;
        let rr = scalar_num(ctx, &ops[2], 0.0)?;

        use crate::x87;
        let n = values.len() as f64;
        let (rr1, fr1) = (x87::add(1.0, rr), x87::add(1.0, fr));
        let (mut prr, mut pfr) = (1.0, 1.0);
        let mut pos = 0.0;
        let mut neg = 0.0;
        for &v in &values {
            if v > 0.0 {
                pos = x87::add(pos, x87::div(v, prr));
            } else if v < 0.0 {
                if pfr == 0.0 {
                    return Err(XlError::Div0);
                }
                neg = x87::add(neg, x87::div(v, pfr));
            }
            prr = x87::mul(prr, rr1);
            pfr = x87::mul(pfr, fr1);
        }
        if neg == 0.0 || n <= 1.0 || rr1 == 0.0 {
            return Err(XlError::Div0);
        }
        let fv = x87::div(x87::mul(pos, prr), rr1);
        let ratio = x87::div(fv, -neg);
        let y = x87::div(1.0, n - 1.0);

        let x = if ratio > 0.0 && y.fract() != 0.0 {
            x87::exp(x87::mul(x87::ln(ratio), y))
        } else {
            xmath::pow(ratio, y)
        };
        finite(x87::sub(x, 1.0)).map(Value::num)
    })())
}

fn cells(v: &Arg) -> R<Vec<Value>> {
    match v {
        Some(Value::Array(a)) => Ok(a.data.clone()),
        Some(v) => Ok(vec![v.clone()]),
        None => Err(XlError::NA),
    }
}

fn xflows(ctx: &EvalCtx, a: &[Arg], vi: usize, di: usize, xirr: bool) -> R<(Vec<f64>, Vec<f64>)> {
    let text_err = |first_date: bool| {
        if xirr || first_date {
            XlError::Value
        } else {
            XlError::Num
        }
    };
    let vals = cells(&a[vi])?;
    let dates = cells(&a[di])?;
    let mut v = Vec::with_capacity(vals.len());
    for x in &vals {
        match x {
            Value::Number(n) => v.push(*n),
            Value::Error(e) => return Err(*e),
            Value::Text(_) | Value::Bool(_) => return Err(text_err(false)),
            _ => return Err(XlError::Num),
        }
    }
    let mut d = Vec::with_capacity(dates.len());
    for (i, x) in dates.iter().enumerate() {
        match x {
            Value::Number(n) => d.push(atp::check_date(ctx, *n)? as f64),
            Value::Error(e) => return Err(*e),
            Value::Text(_) | Value::Bool(_) => return Err(text_err(i == 0)),
            _ => return Err(XlError::Num),
        }
    }
    if v.len() != d.len() {
        return Err(XlError::Num);
    }
    for &x in &d[1..] {
        if x < d[0] {
            return Err(XlError::Num);
        }
    }
    Ok((v, d))
}

fn xnpv_of(rate: f64, v: &[f64], d: &[f64]) -> f64 {
    let b = 1.0 + rate;
    let mut acc = 0.0;
    for (x, t) in v.iter().zip(d) {
        acc += x / xmath::pow(b, (t - d[0]) / 365.0);
    }
    acc
}

pub fn xnpv(ctx: &mut EvalCtx, a: &[Arg]) -> Value {
    atp::out((|| {
        let rate = atp::req(ctx, a, 0)?;
        let (v, d) = xflows(ctx, a, 1, 2, false)?;
        if rate <= 0.0 {
            return Err(XlError::Num);
        }
        finite(xnpv_of(rate, &v, &d))
    })())
}

fn xirr_bisect(v: &[f64], d: &[f64], guess: f64) -> R<f64> {
    let sign = if v[0] < 0.0 { 1.0 } else { -1.0 };
    let h = |x: f64| -> R<f64> {
        let b = 1.0 + x;
        if b <= 0.0 {
            return Err(XlError::Num);
        }
        let mut acc = 0.0;
        for (a, t) in v.iter().zip(d) {
            acc += a / xmath::pow(b, (t - d[0]) / 365.0);
        }
        finite(acc * sign)
    };
    let g = if guess == 0.0 { 1e-5 } else { guess };
    let h0 = h(0.0)?;
    let (mut lo, mut hi);
    let up = h0 > 0.0;
    if up {
        lo = 0.0;
        hi = g;
        let mut n = 0;
        while h(hi)? > 0.0 {
            lo = hi;
            hi *= 2.0;
            n += 1;
            if n > 2000 {
                return Err(XlError::Num);
            }
        }
    } else if h0 < 0.0 {
        hi = 0.0;
        lo = -g.abs();
        if h(lo)? <= 0.0 {
            hi = lo;
            lo = -1.0;
        }
    } else if g > 0.0 {
        lo = 0.0;
        hi = g;
    } else {
        lo = g;
        hi = 0.0;
    }
    for _ in 0..2000 {
        let mid = hi - (hi - lo) / 2.0;
        let hm = h(mid)?;

        if (hi - lo).abs() <= 1e-8 * (1.0 + 2.0 * mid.abs()) && (up || hm.abs() < 2e-8 * v[0].abs())
        {
            return Ok(mid);
        }
        if hm > 0.0 {
            lo = mid;
        } else {
            hi = mid;
        }
    }
    Err(XlError::Num)
}

pub fn xirr(ctx: &mut EvalCtx, a: &[Arg]) -> Value {
    atp::out((|| {
        let (v, d) = xflows(ctx, a, 0, 1, true)?;
        let guess = atp::opt(ctx, a, 2, 0.1)?;
        if !v.iter().any(|&x| x > 0.0) || !v.iter().any(|&x| x < 0.0) {
            return Err(XlError::Num);
        }
        finite(xirr_bisect(&v, &d, guess)?)
    })())
}
