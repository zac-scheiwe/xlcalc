use super::atp::{self, Arg};
use crate::eval::{EvalCtx, Operand, Reference};
use crate::functions::args::{collect_numbers, num, opval, Grid, R, SUM_POLICY};
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

fn mirr_values(ctx: &mut EvalCtx, op: &Operand) -> R<Vec<f64>> {
    match op {
        Operand::Ref(r @ (Reference::Multi(_) | Reference::Span(_))) => {
            let mut out = Vec::new();
            for area in r.areas() {
                out.extend(flow_values(ctx, &Operand::Ref(Reference::Area(*area)))?);
            }
            Ok(out)
        }
        _ => flow_values(ctx, op),
    }
}

fn scalar_num(ctx: &EvalCtx, op: &Operand, default: f64) -> R<f64> {
    match op {
        Operand::Val(v) => num(ctx, v),
        Operand::Missing => Ok(default),
        Operand::Ref(_) => Err(XlError::VALUE),
    }
}

pub fn npv_of(rate: f64, values: &[f64]) -> R<f64> {
    use crate::x87;

    let mut mp = x87::MathFlag::default();
    let b = mp.add(rate, 1.0);
    let mut acc = values.first().copied().unwrap_or(0.0);
    let mut d = 1.0;
    for &v in values.iter().skip(1) {
        d = mp.mul(d, b);
        let q = mp.div(v, d);
        acc = mp.add(q, acc);
    }
    let r = mp.div(acc, b);
    mp.result(r)
}

pub fn npv(ctx: &mut EvalCtx, ops: &[Operand]) -> Operand {
    opval((|| {
        let rate = scalar_num(ctx, &ops[0], 0.0)?;
        let values = collect_numbers(ctx, &ops[1..], SUM_POLICY)?;
        npv_of(rate, &values).map(Value::num)
    })())
}

fn irr_secant(values: &[f64], guess: f64) -> R<Option<f64>> {
    use crate::x87;
    let div0 = std::cell::Cell::new(false);
    let npv = |w: f64| {
        let mut mp = x87::MathFlag::default();
        let b = mp.div(1.0, x87::add(w, 1.0));
        let mut d = 1.0;
        let mut acc = values[0];
        for v in &values[1..] {
            d = mp.mul(d, b);
            let t = mp.div(*v, d);
            acc = mp.add(acc, t);
        }
        match mp.0 {
            0 => acc,
            4 => {
                div0.set(true);
                f64::NAN
            }
            _ => f64::NAN,
        }
    };
    let w = secant_solve(x87::div(guess, x87::sub(-1.0, guess)), npv);
    if div0.get() {
        return Err(XlError::DIV0);
    }
    let Some((w, tol)) = w else { return Ok(None) };

    if !fn_iter_above_m1(w, tol) {
        return Ok(None);
    }
    let r = x87::sub(x87::div(1.0, x87::add(w, 1.0)), 1.0);
    Ok(r.is_finite().then_some(r))
}

pub(super) fn secant_start(x0: f64) -> R<f64> {
    let s = crate::x87::add(x0, 1.0);
    let b = s.to_bits();
    if b >> 48 == 0 || !s.is_finite() || (b as i64) <= 0 {
        return Err(XlError::VALUE);
    }
    Ok(x0)
}

pub(super) fn fn_iter_above_m1(x: f64, tol: f64) -> bool {
    crate::x87::sub(x, tol) > -1.0
}

pub(super) fn secant_solve(x0: f64, f: impl Fn(f64) -> f64) -> Option<(f64, f64)> {
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
            return Some((x2, 1e-7));
        }
        (x0, f0, x1, f1) = (x1, f1, x2, f2);
    }
    if f1.abs() < 1e-4 && step < 1e-4 {
        return Some((x1, 1e-4));
    }
    None
}

fn irr_of(values: &[f64], guess: Option<f64>) -> R<f64> {
    if values.is_empty() {
        return Err(XlError::NUM);
    }
    let restarts = [0.1, -0.09090909090909091, 2.0];
    let first = guess.as_slice();
    let runs = first.iter().chain(&restarts);

    for &g in runs {
        if let Some(r) = irr_secant(values, g)? {
            return Ok(r);
        }
    }
    Err(XlError::NUM)
}

pub fn irr(ctx: &mut EvalCtx, ops: &[Operand]) -> Operand {
    opval((|| {
        if matches!(ops[0], Operand::Missing) {
            return Err(XlError::VALUE);
        }
        if let Operand::Val(Value::Error(e)) = &ops[0] {
            return Err(*e);
        }
        let guess = match ops.get(1) {
            None => None,

            Some(op) => Some(scalar_num(ctx, op, 0.0)?),
        };

        let values = flow_values(ctx, &ops[0]).map_err(|_| XlError::VALUE)?;
        if let Some(g) = guess {
            secant_start(crate::x87::div(g, crate::x87::sub(-1.0, g)))?;
        }

        let lead = values.iter().take_while(|v| v.to_bits() >> 48 == 0).count();
        irr_of(&values[lead..], guess).map(Value::num)
    })())
}

fn power_signed(x: f64, y: f64) -> R<f64> {
    use crate::x87;
    if x >= 0.0 || y.fract() == 0.0 || y == 0.5 {
        let p = crate::xmath::pow(x, y);
        return if p.is_nan() { Err(XlError::NUM) } else { Ok(p) };
    }
    let k: f64 = format!("{:.14e}", x87::div(1.0, y))
        .parse()
        .unwrap_or(f64::NAN);
    if k.fract() != 0.0 || k.abs() >= 2147483647.0 || (k as i64) & 1 == 0 {
        return Err(XlError::NUM);
    }
    Ok(-x87::exp(x87::mul(x87::ln(-x), y)))
}

pub fn mirr(ctx: &mut EvalCtx, ops: &[Operand]) -> Operand {
    opval((|| {
        if matches!(ops[0], Operand::Missing) {
            return Err(XlError::VALUE);
        }

        if let Operand::Val(Value::Error(e)) = &ops[0] {
            return Err(*e);
        }
        let fr = scalar_num(ctx, &ops[1], 0.0)?;
        let rr = scalar_num(ctx, &ops[2], 0.0)?;
        let values = mirr_values(ctx, &ops[0])?;

        let n = values.len() as f64;
        let mut f = x87::MathFlag::default();
        let (rr1, fr1) = (f.add(1.0, rr), f.add(1.0, fr));
        let (mut prr, mut pfr) = (1.0, 1.0);
        let mut pos = 0.0;
        let mut neg = 0.0;
        for &v in &values {
            if v.to_bits() >> 63 == 1 {
                let t = f.div(v, pfr);
                neg = f.add(neg, t);
            } else {
                let t = f.div(v, prr);
                pos = f.add(pos, t);
            }
            prr = f.mul(prr, rr1);
            pfr = f.mul(pfr, fr1);
        }
        if f.0 != 0 {
            return Err(if f.0 & 4 != 0 {
                XlError::DIV0
            } else {
                XlError::NUM
            });
        }
        if n <= 1.0 {
            return Err(XlError::DIV0);
        }
        let scaled = f.mul(pos, prr);
        let fv = f.div(scaled, rr1);

        let fv = if fv.to_bits() >> 48 != 0 { -fv } else { fv };
        let ratio = f.div(fv, neg);
        let y = f.div(1.0, n - 1.0);

        let x = match power_signed(ratio, y) {
            Ok(p) if p.is_finite() => p,
            Ok(_) => {
                f.0 = 1;
                0.0
            }
            Err(_) => {
                f.0 = 8;
                0.0
            }
        };
        if f.0 != 0 {
            return Err(if f.0 & 4 != 0 {
                XlError::DIV0
            } else {
                XlError::NUM
            });
        }
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
            XlError::VALUE
        } else {
            XlError::NUM
        }
    };
    let vals = cells(&a[vi])?;
    let dates = cells(&a[di])?;
    let is_arr = |i: usize| matches!(a[i], Some(Value::Array(_)));
    let (v_arr, d_arr) = (is_arr(vi), is_arr(di));

    let classify_v = |i: usize, x: &Value| match x {
        Value::Number(n) => Ok(*n),
        Value::Error(e) => Err(*e),

        Value::Text(_) if xirr => ctx.to_number_atp(x).map_err(|_| XlError::VALUE),
        Value::Text(_) | Value::Bool(_) if !xirr && i == 0 => Err(XlError::VALUE),
        Value::Text(_) | Value::Bool(_) => Err(text_err(false)),
        Value::Blank if xirr || !v_arr => Ok(0.0),
        Value::Blank if i == 0 => Err(XlError::VALUE),
        _ => Err(XlError::NUM),
    };
    let classify_d = |i: usize, x: &Value| match x {
        Value::Text(_) if xirr => ctx
            .to_number_atp(x)
            .map_err(|_| XlError::VALUE)
            .and_then(|n| atp::check_date(ctx, n))
            .map(|d| d as f64),

        Value::Number(n) if xirr => {
            atp::check_date(ctx, *n).map(|d| (d + atp::kernel_offset(ctx)) as f64)
        }
        Value::Blank if xirr => Ok(atp::kernel_offset(ctx) as f64),

        Value::Number(n) => atp::check_date(ctx, *n).map(|d| d as f64).and_then(|d| {
            if !xirr && d == 0.0 {
                Err(XlError::NUM)
            } else {
                Ok(d)
            }
        }),
        Value::Error(e) => Err(*e),
        Value::Text(_) | Value::Bool(_) => Err(text_err(i == 0)),
        Value::Blank if d_arr && i == 0 => Err(XlError::VALUE),
        _ => Err(XlError::NUM),
    };

    if !v_arr {
        if let Some(x) = a[vi]
            .as_ref()
            .filter(|x| !matches!(x, Value::Blank) && (xirr || !matches!(x, Value::Text(_))))
        {
            classify_v(0, x)?;
        }
    }
    if !d_arr {
        if let Some(x) = a[di].as_ref().filter(|x| !matches!(x, Value::Blank)) {
            classify_d(0, x)?;
        }
    }

    if xirr && atp::opt(ctx, a, 2, 0.1)? <= -1.0 {
        return Err(XlError::NUM);
    }

    if vals.len() != dates.len() {
        return Err(XlError::NUM);
    }

    if xirr && vals.len() == 1 {
        return Err(XlError::NA);
    }

    let mut v = Vec::with_capacity(vals.len());
    let mut d = Vec::with_capacity(dates.len());
    if !xirr && !dates.is_empty() {
        d.push(classify_d(0, &dates[0])?);
    }
    for i in 0..vals.len().max(dates.len()) {
        if xirr && i < dates.len() {
            d.push(classify_d(i, &dates[i])?);
        }
        if !xirr {
            if let Some(Value::Error(e)) = vals.get(i) {
                return Err(*e);
            }
            if i >= 1 && i < dates.len() {
                d.push(classify_d(i, &dates[i])?);
            }
        }
        if i < vals.len() {
            v.push(classify_v(i, &vals[i])?);
        }
    }
    if v.len() != d.len() {
        return Err(XlError::NUM);
    }

    for &x in d.iter().skip(1) {
        if !xirr && x < d[0] {
            return Err(XlError::NUM);
        }
    }
    Ok((v, d))
}

fn xnpv_of(rate: f64, v: &[f64], d: &[f64]) -> R<f64> {
    let b = x87::add(1.0, rate);
    let mut acc = 0.0;
    for (x, t) in v.iter().zip(d) {
        let e = x87::div(x87::sub(*t, d[0]), 365.0);
        let p = xmath::pow(b, e);

        if !p.is_finite() {
            return Err(XlError::NUM);
        }
        acc = x87::add(acc, x87::div(*x, p));
    }
    Ok(acc)
}

pub fn xnpv(ctx: &mut EvalCtx, a: &[Arg]) -> Value {
    atp::out((|| {
        let rate = atp::req(ctx, a, 0)?;
        let (v, d) = xflows(ctx, a, 1, 2, false)?;
        if rate <= 0.0 {
            return Err(XlError::NUM);
        }
        finite(xnpv_of(rate, &v, &d)?)
    })())
}

fn xirr_bisect(v: &[f64], d: &[f64], guess: f64) -> R<f64> {
    let v0 = v[0];
    let later = &v[1..];
    let exps: Vec<f64> = d[1..]
        .iter()
        .map(|t| x87::div(x87::sub(*t, d[0]), 365.0))
        .collect();

    let npv = |r: f64| -> R<f64> {
        let b = x87::add(r, 1.0);
        if b <= 0.0 {
            return Err(XlError::NUM);
        }
        let mut acc = 0.0;
        for (x, e) in later.iter().zip(&exps) {
            let p = xmath::pow(b, *e);
            if !p.is_finite() || p == 0.0 {
                return Err(XlError::NUM);
            }
            acc = x87::add(x87::div(*x, p), acc);
        }
        finite(acc)
    };

    let opposite = |h: f64| (v0 > 0.0 && h < 0.0) || (v0 < 0.0 && h > 0.0);
    let half_way = |a: f64, b: f64| x87::add(x87::div(x87::sub(a, b), 2.0), b);
    let narrow = |a: f64, b: f64| {
        let tol = x87::mul(x87::add(x87::add(b, a).abs(), 1.0), 1e-8);
        x87::sub(a, b).abs() < tol
    };
    const STEPS: usize = 100;

    let mut rate = if guess == 0.0 { 1e-5 } else { guess };
    let sum = later.iter().fold(0.0, |s, &x| x87::add(x, s));

    if sum.abs() < v0.abs() && rate > 0.0 && rate.to_bits() & 0xffff_0000_0000_0000 != 0 {
        rate = -rate;
    }
    let mut n = npv(rate)?;
    if rate < 0.0 {
        let (mut a, mut b) = (-1.0, 0.0);
        for _ in 0..STEPS {
            let h = x87::add(v0, n);
            let htol = x87::mul(x87::add(n.abs(), v0.abs()), 1e-8);
            if h.abs() < htol && narrow(a, b) {
                return Ok(rate);
            }
            if opposite(h) {
                a = rate;
            } else {
                b = rate;
            }
            rate = half_way(a, b);
            n = npv(rate)?;
        }
    } else {
        let (mut a, mut b) = (0.0, 0.0);
        let mut bracketed = false;
        for _ in 0..STEPS {
            if bracketed && narrow(a, b) {
                return Ok(rate);
            }
            let h = x87::add(v0, n);
            if !opposite(h) {
                a = rate;
                rate = if bracketed {
                    half_way(a, b)
                } else {
                    bracketed = true;
                    b = 0.0;
                    x87::div(rate, 2.0)
                };
            } else if bracketed {
                b = rate;
                rate = half_way(a, b);
            } else {
                rate = x87::mul(rate, 2.0);
            }
            n = npv(rate)?;
        }
    }
    Err(XlError::NUM)
}

pub fn xirr(ctx: &mut EvalCtx, a: &[Arg]) -> Value {
    atp::out((|| {
        let guess = atp::opt(ctx, a, 2, 0.1)?;
        let (v, d) = xflows(ctx, a, 0, 1, true)?;
        if !v.iter().any(|&x| x > 0.0) || !v.iter().any(|&x| x < 0.0) {
            return Err(XlError::NUM);
        }
        finite(xirr_bisect(&v, &d, guess)?)
    })())
}

#[cfg(test)]
mod tests {
    use super::*;

    const D: [f64; 3] = [43831.0, 43831.0, 44196.0];

    #[test]
    fn npv_flag_codes() {
        assert_eq!(npv_of(-1.0, &[1.0, 2.0]), Err(XlError::DIV0));
        assert_eq!(npv_of(-1.0, &[1.0]), Err(XlError::DIV0));

        let mut v = vec![1e-100; 30];
        v[0] = 1.0;
        assert_eq!(npv_of(-1.0 + 1e-16, &v), Err(XlError::DIV0));

        assert_eq!(npv_of(-1.0 + 1e-16, &[1e300, 1.0]), Err(XlError::NUM));

        assert_eq!(npv_of(1e200, &[1.0, 1.0, 1.0]), Err(XlError::DIV0));

        assert_eq!(npv_of(0.0, &[f64::MAX, f64::MAX]), Err(XlError::NUM));
        assert_eq!(npv_of(0.0, &[1.0, 2.0, 3.0]), Ok(6.0));
    }

    #[test]
    fn zero_first_flow_halves_the_guess_to_zero() {
        let v = [0.0, -1000.0, 1100.0];
        assert_eq!(xirr_bisect(&v, &D, 0.1), Ok(0.1 / 33554432.0));
        assert_eq!(xirr_bisect(&v, &D, 0.0), Ok(4.8828125e-9));

        assert_eq!(
            xirr_bisect(&[0.0, 1000.0, -1100.0], &D, 0.1),
            Ok(0.1 / 33554432.0)
        );
    }

    #[test]
    fn zero_first_flow_with_negative_guess_never_converges() {
        let v = [0.0, -1000.0, 900.0];
        assert_eq!(xirr_bisect(&v, &D, -0.2), Err(XlError::NUM));
    }

    #[test]
    fn small_later_sum_negates_the_guess() {
        let d = [43831.0, 44196.0];
        let r = xirr_bisect(&[-1000.0, 900.0], &d, 0.5).unwrap();
        assert!((r + 0.1).abs() < 1e-6, "{r}");

        assert_eq!(
            xirr_bisect(&[-1000.0, 1000.0], &d, 0.1),
            Ok(0.1 / 33554432.0)
        );
    }

    #[test]
    fn constant_npv_hits_the_step_cap() {
        let d = [43831.0, 43831.0];
        assert_eq!(xirr_bisect(&[-1000.0, 1100.0], &d, 0.1), Err(XlError::NUM));
    }

    #[test]
    fn positive_root_from_a_positive_guess() {
        let d = [43831.0, 44196.0];
        let r = xirr_bisect(&[-1000.0, 1100.0], &d, 0.1).unwrap();
        assert!((r - 0.1).abs() < 1e-7, "{r}");
    }
}
