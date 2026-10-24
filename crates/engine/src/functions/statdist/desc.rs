use super::*;
use crate::functions::math::{round_decimal, RoundMode};
use crate::number;
use crate::value::Array;
use crate::x87;

pub(super) fn pairs(ctx: &mut EvalCtx, a: &Operand, b: &Operand) -> R<(Vec<f64>, Vec<f64>)> {
    check_array_args(ctx, &[a, b])?;
    let va = flat_values(ctx, a)?;
    let vb = flat_values(ctx, b)?;
    if va.len() != vb.len() {
        return Err(XlError::NA);
    }
    let mut xs = Vec::with_capacity(va.len());
    let mut ys = Vec::with_capacity(va.len());
    for (x, y) in va.iter().zip(vb.iter()) {
        if let Value::Error(e) = x {
            return Err(*e);
        }
        if let Value::Error(e) = y {
            return Err(*e);
        }
        if let (Value::Number(x), Value::Number(y)) = (x, y) {
            xs.push(*x);
            ys.push(*y);
        }
    }
    Ok((xs, ys))
}

struct Moments {
    n: f64,
    sxx: f64,
    syy: f64,
    sxy: f64,
    mx: f64,
    my: f64,
}

fn moments(x: &[f64], y: &[f64]) -> Moments {
    let n = x.len() as f64;
    let mx = mean(x);
    let my = mean(y);
    let (mut sxx, mut syy, mut sxy) = (0.0, 0.0, 0.0);
    for (a, b) in x.iter().zip(y) {
        let dx = a - mx;
        let dy = b - my;
        sxx += dx * dx;
        syy += dy * dy;
        sxy += dx * dy;
    }
    Moments {
        n,
        sxx,
        syy,
        sxy,
        mx,
        my,
    }
}

fn pearson_xl(x: &[f64], y: &[f64], d: f64) -> R<f64> {
    let n = x.len() as f64;
    let (mut sx, mut sy) = (0.0, 0.0);
    for (a, b) in x.iter().zip(y) {
        sx = x87::add(sx, *a);
        sy = x87::add(sy, *b);
    }
    let (mx, my) = (x87::div(sx, n), x87::div(sy, n));
    let (mut sxx, mut syy, mut sxy) = (0.0, 0.0, 0.0);
    for (a, b) in x.iter().zip(y) {
        let dx = x87::sub(*a, mx);
        let dy = x87::sub(*b, my);
        sxy = x87::add(x87::mul(dx, dy), sxy);
        sxx = x87::add(x87::mul(dx, dx), sxx);
        syy = x87::add(x87::mul(dy, dy), syy);
    }
    if sxx == 0.0 || syy == 0.0 {
        return Err(XlError::Div0);
    }
    let sd = x87::mul(x87::sqrt(x87::div(sxx, d)), x87::sqrt(x87::div(syy, d)));
    Ok(x87::div(x87::div(sxy, d), sd))
}

pub fn correl(ctx: &mut EvalCtx, o: &[Operand]) -> Operand {
    opfin((|| {
        let (x, y) = pairs(ctx, &o[0], &o[1])?;
        if x.is_empty() {
            return Err(XlError::Div0);
        }
        pearson_xl(&x, &y, x.len() as f64)
    })())
}

pub fn rsq(ctx: &mut EvalCtx, o: &[Operand]) -> Operand {
    opfin((|| {
        let (x, y) = pairs(ctx, &o[0], &o[1])?;
        if x.is_empty() {
            return Err(XlError::Div0);
        }
        let r = pearson_xl(&x, &y, x.len() as f64 - 1.0)?;
        Ok(x87::mul(r, r))
    })())
}

fn line(ctx: &mut EvalCtx, y: &Operand, x: &Operand) -> R<(f64, f64, Moments)> {
    let (ys, xs) = pairs(ctx, y, x)?;
    if xs.is_empty() {
        return Err(XlError::Div0);
    }
    let m = moments(&xs, &ys);
    if m.sxx == 0.0 {
        return Err(XlError::Div0);
    }
    let b = m.sxy / m.sxx;
    let a = m.my - b * m.mx;
    Ok((b, a, m))
}

pub fn slope(ctx: &mut EvalCtx, o: &[Operand]) -> Operand {
    opfin(line(ctx, &o[0], &o[1]).map(|t| t.0))
}

pub fn intercept(ctx: &mut EvalCtx, o: &[Operand]) -> Operand {
    opfin(line(ctx, &o[0], &o[1]).map(|t| t.1))
}

pub fn steyx(ctx: &mut EvalCtx, o: &[Operand]) -> Operand {
    opfin((|| {
        let (ys, xs) = pairs(ctx, &o[0], &o[1])?;
        if xs.len() < 3 {
            return Err(XlError::Div0);
        }
        let m = moments(&xs, &ys);
        if m.sxx == 0.0 {
            return Err(XlError::Div0);
        }
        let r = (m.syy - m.sxy * m.sxy / m.sxx) / (m.n - 2.0);
        Ok(if r < 0.0 { 0.0 } else { r.sqrt() })
    })())
}

pub fn forecast(ctx: &mut EvalCtx, o: &[Operand]) -> Operand {
    opfin((|| {
        let x = opnum(ctx, &o[0])?;
        let (b, a, _) = line(ctx, &o[1], &o[2])?;
        Ok(a + b * x)
    })())
}

pub fn covar(ctx: &mut EvalCtx, o: &[Operand], sample: bool) -> Operand {
    opfin((|| {
        let (x, y) = pairs(ctx, &o[0], &o[1])?;
        let n = x.len() as f64;
        if n == 0.0 || (sample && n < 2.0) {
            return Err(XlError::Div0);
        }
        let m = moments(&x, &y);
        Ok(m.sxy / if sample { n - 1.0 } else { n })
    })())
}

fn sorted_numbers(ctx: &mut EvalCtx, op: &Operand) -> R<Vec<f64>> {
    let mut v = collect_numbers(ctx, std::slice::from_ref(op), SUM_POLICY)?;
    v.sort_by(|a, b| a.partial_cmp(b).unwrap());
    Ok(v)
}

fn percentile_of(v: &[f64], k: f64, exclusive: bool) -> R<f64> {
    let n = v.len();
    if n == 0 {
        return Err(XlError::Num);
    }
    if exclusive {
        if !(k > 0.0 && k < 1.0) {
            return Err(XlError::Num);
        }
        let r = k * (n as f64 + 1.0);
        if r < 1.0 || r > n as f64 {
            return Err(XlError::Num);
        }
        let i = r.floor();
        let f = r - i;
        let i = i as usize;
        if i >= n {
            return Ok(v[n - 1]);
        }
        Ok(v[i - 1] + f * (v[i] - v[i - 1]))
    } else {
        if !(0.0..=1.0).contains(&k) {
            return Err(XlError::Num);
        }

        let r = k * (n as f64 - 1.0) + 1.0;
        let i = r.floor();
        let f = r - i;
        let i = i as usize;
        if i >= n {
            return Ok(v[n - 1]);
        }
        Ok(v[i - 1] + f * (v[i] - v[i - 1]))
    }
}

pub fn percentile(ctx: &mut EvalCtx, o: &[Operand], exclusive: bool) -> Operand {
    opfin((|| {
        let k = opnum(ctx, &o[1])?;
        let v = sorted_numbers(ctx, &o[0])?;
        percentile_of(&v, k, exclusive)
    })())
}

pub fn quartile(ctx: &mut EvalCtx, o: &[Operand], exclusive: bool) -> Operand {
    opfin((|| {
        let q = opnum(ctx, &o[1])?.trunc();
        let v = sorted_numbers(ctx, &o[0])?;
        if exclusive {
            if !(1.0..=3.0).contains(&q) {
                return Err(XlError::Num);
            }
        } else if !(0.0..=4.0).contains(&q) {
            return Err(XlError::Num);
        }
        percentile_of(&v, q / 4.0, exclusive)
    })())
}

pub fn percentrank(ctx: &mut EvalCtx, o: &[Operand], exclusive: bool) -> Operand {
    opfin((|| {
        let x = opnum(ctx, &o[1])?;
        let sig = match o.get(2) {
            None | Some(Operand::Missing) | Some(Operand::Val(Value::Blank)) => 3.0,
            Some(op) => opnum(ctx, op)?.trunc(),
        };
        let v = sorted_numbers(ctx, &o[0])?;
        let n = v.len();
        if n == 0 || x < v[0] || x > v[n - 1] {
            return Err(XlError::NA);
        }

        if n == 1 {
            return Ok(1.0);
        }
        if sig < 1.0 {
            return Err(XlError::Num);
        }
        let nf = n as f64;
        let rank = {
            let less = v.iter().filter(|&&a| a < x).count();
            let pos = if v[less] == x {
                less as f64
            } else {
                let lo = v[less - 1];
                let hi = v[less];
                (less - 1) as f64 + (x - lo) / (hi - lo)
            };
            if exclusive {
                (pos + 1.0) / (nf + 1.0)
            } else {
                pos / (nf - 1.0)
            }
        };
        let digits = sig.min(400.0) as i32;
        Ok(round_decimal(rank, digits, RoundMode::Down))
    })())
}

fn ref_numbers(ctx: &mut EvalCtx, op: &Operand) -> R<Vec<f64>> {
    match op {
        Operand::Ref(r) => {
            let mut out = Vec::new();
            let mut failure = None;
            for area in r.areas() {
                ctx.visit_area(area, |_, _, v| match v {
                    Value::Number(n) => out.push(*n),
                    Value::Error(e) if failure.is_none() => failure = Some(*e),
                    _ => {}
                });
            }
            match failure {
                Some(e) => Err(e),
                None => Ok(out),
            }
        }
        _ => numbers_only(ctx, op),
    }
}

pub fn rank(ctx: &mut EvalCtx, o: &[Operand], avg: bool) -> Operand {
    opfin((|| {
        let x = opnum(ctx, &o[0])?;
        let ascending = match o.get(2) {
            None | Some(Operand::Missing) => false,

            Some(Operand::Val(Value::Text(t))) => match t.to_ascii_lowercase().as_str() {
                "true" => true,
                "false" => false,
                _ => return Err(XlError::Value),
            },
            Some(op) => opnum(ctx, op)? != 0.0,
        };
        let v = ref_numbers(ctx, &o[1])?;
        let mut before = 0usize;
        let mut equal = 0usize;
        for &a in &v {
            if a == x {
                equal += 1;
            } else if (a > x) != ascending {
                before += 1;
            }
        }
        if equal == 0 {
            return Err(XlError::NA);
        }
        Ok(if avg {
            before as f64 + (equal as f64 + 1.0) / 2.0
        } else {
            before as f64 + 1.0
        })
    })())
}

fn mode_values(ctx: &mut EvalCtx, ops: &[Operand]) -> R<Vec<f64>> {
    for op in ops {
        match op {
            Operand::Missing => return Err(XlError::Value),
            Operand::Val(Value::Error(e)) => return Err(*e),

            Operand::Ref(r) if r.single().is_some_and(|a| a.rect.is_cell()) => {
                let area = *r.single().unwrap();
                match ctx.area_array(&area) {
                    Value::Blank => return Err(XlError::Value),
                    Value::Error(e) => return Err(e),
                    _ => {}
                }
            }
            _ => {}
        }
    }
    let mut out = Vec::new();
    for op in ops {
        match op {
            Operand::Val(Value::Number(n)) => out.push(*n),
            Operand::Val(Value::Array(_)) | Operand::Ref(_) => out.extend(ref_numbers(ctx, op)?),
            _ => {}
        }
    }
    Ok(out)
}

pub fn mode(ctx: &mut EvalCtx, o: &[Operand], multi: bool) -> Operand {
    let v = match mode_values(ctx, o) {
        Ok(v) => v,
        Err(e) => return Operand::err(e),
    };

    let mut distinct: Vec<(f64, usize)> = Vec::new();
    for &x in &v {
        match distinct.iter_mut().find(|(d, _)| *d == x) {
            Some(e) => e.1 += 1,
            None => distinct.push((x, 1)),
        }
    }
    let best = distinct.iter().map(|d| d.1).max().unwrap_or(0);
    if best < 2 {
        return Operand::err(XlError::NA);
    }
    let modes: Vec<Value> = distinct
        .iter()
        .filter(|d| d.1 == best)
        .map(|d| Value::Number(d.0))
        .collect();
    if !multi || modes.len() == 1 {
        return Operand::Val(modes[0].clone());
    }
    Operand::Val(Array::column(modes).into_value())
}

pub fn trimmean(ctx: &mut EvalCtx, o: &[Operand]) -> Operand {
    opfin((|| {
        let p = opnum(ctx, &o[1])?;
        let v = sorted_numbers(ctx, &o[0])?;
        if !(0.0..1.0).contains(&p) || v.is_empty() {
            return Err(XlError::Num);
        }
        let k = (v.len() as f64 * p / 2.0).floor() as usize;
        let rest = &v[k..v.len() - k];
        if rest.is_empty() {
            return Err(XlError::Num);
        }
        Ok(mean(rest))
    })())
}

fn flat_areas(ctx: &mut EvalCtx, op: &Operand) -> R<Vec<Value>> {
    match op {
        Operand::Ref(r) if r.single().is_none() => {
            let mut out = Vec::new();
            for a in r.areas() {
                out.extend(flat_values(
                    ctx,
                    &Operand::Ref(crate::eval::Reference::Area(*a)),
                )?);
            }
            Ok(out)
        }
        op => flat_values(ctx, op),
    }
}

pub fn frequency(ctx: &mut EvalCtx, o: &[Operand]) -> Operand {
    let data = match &o[0] {
        Operand::Ref(r) if r.single().is_none() => flat_areas(ctx, &o[0]).and_then(|v| {
            let mut out = Vec::new();
            for x in v {
                match x {
                    Value::Number(n) => out.push(n),
                    Value::Error(e) => return Err(e),
                    _ => {}
                }
            }
            Ok(out)
        }),
        Operand::Val(Value::Number(n)) => Ok(vec![*n]),
        Operand::Val(Value::Error(e)) => Err(*e),
        Operand::Val(Value::Array(_)) | Operand::Ref(_) => numbers_only(ctx, &o[0]),
        _ => Ok(Vec::new()),
    };
    let data = match data {
        Ok(d) => d,
        Err(e) => return Operand::err(e),
    };
    let bins_vals = match &o[1] {
        Operand::Val(Value::Error(_)) => Vec::new(),
        op => match flat_areas(ctx, op) {
            Ok(v) => v,
            Err(e) => return Operand::err(e),
        },
    };
    let mut bins: Vec<f64> = bins_vals
        .iter()
        .filter_map(|v| {
            if let Value::Number(n) = v {
                Some(*n)
            } else {
                None
            }
        })
        .collect();
    if bins.is_empty() {
        bins.push(0.0);
    }

    let mut order: Vec<usize> = (0..bins.len()).collect();
    order.sort_by(|&a, &b| bins[a].partial_cmp(&bins[b]).unwrap());
    let mut counts = vec![0.0; bins.len() + 1];
    for &x in &data {
        match order.iter().find(|&&i| x <= bins[i]) {
            Some(&i) => counts[i] += 1.0,
            None => counts[bins.len()] += 1.0,
        }
    }
    Operand::Val(Array::column(counts.into_iter().map(Value::Number).collect()).into_value())
}

fn moment_base(v: &[f64], population: bool) -> R<(f64, f64, f64)> {
    let n = v.len() as f64;
    let mut sum = 0.0;
    for &x in v {
        sum = x87::add(sum, x);
    }
    let m = x87::div(sum, n);
    let mut ss = 0.0;
    for &x in v {
        let d = x87::add(x, -m);
        ss = x87::add(ss, x87::mul(d, d));
    }
    let s = x87::sqrt(x87::div(ss, if population { n } else { n - 1.0 }));
    if s == 0.0 {
        return Err(XlError::Div0);
    }
    Ok((m, s, n))
}

fn moment_sum(v: &[f64], m: f64, s: f64, fourth: bool) -> f64 {
    let mut acc = 0.0;
    for &x in v {
        let t = x87::div(x87::add(x, -m), s);
        let t2 = x87::mul(t, t);
        acc = x87::add(acc, x87::mul(t2, if fourth { t2 } else { t }));
    }
    acc
}

pub fn kurt(ctx: &mut EvalCtx, o: &[Operand]) -> Operand {
    opfin((|| {
        let v = collect_numbers(ctx, o, SUM_POLICY)?;
        if v.len() < 4 {
            return Err(XlError::Div0);
        }
        let (m, s, n) = moment_base(&v, false)?;
        let acc = moment_sum(&v, m, s, true);

        let (a, b, c) = (n - 1.0, n - 2.0, n - 3.0);
        let bc = x87::mul(b, c);
        let g = x87::div(x87::mul(n, n + 1.0), x87::mul(bc, a));
        let h = x87::div(x87::mul(x87::mul(3.0, a), a), bc);
        Ok(x87::sub(x87::mul(g, acc), h))
    })())
}

pub fn skew(ctx: &mut EvalCtx, o: &[Operand], population: bool) -> Operand {
    opfin((|| {
        let v = collect_numbers(ctx, o, SUM_POLICY)?;
        if v.len() < 3 {
            return Err(XlError::Div0);
        }
        let (m, s, n) = moment_base(&v, population)?;
        let acc = moment_sum(&v, m, s, false);

        Ok(if population {
            x87::div(acc, n)
        } else {
            x87::mul(acc, x87::div(n, x87::mul(n - 1.0, n - 2.0)))
        })
    })())
}

pub fn standardize(ctx: &mut EvalCtx, a: &[Value]) -> Value {
    fin((|| {
        let x = num(ctx, &a[0])?;
        let m = num(ctx, &a[1])?;
        let s = num(ctx, &a[2])?;
        if s <= 0.0 {
            return Err(XlError::Num);
        }
        Ok((x - m) / s)
    })())
}

pub fn fisher(ctx: &mut EvalCtx, a: &[Value]) -> Value {
    fin((|| {
        let x = num(ctx, &a[0])?;
        if x <= -1.0 || x >= 1.0 {
            return Err(XlError::Num);
        }
        Ok(0.5 * ((1.0 + x) / (1.0 - x)).ln())
    })())
}

pub fn fisherinv(ctx: &mut EvalCtx, a: &[Value]) -> Value {
    fin((|| {
        let y = num(ctx, &a[0])?;
        let e = (2.0 * y).exp();
        if e.is_infinite() {
            return Ok(1.0);
        }
        Ok((e - 1.0) / (e + 1.0))
    })())
}

pub fn prob(ctx: &mut EvalCtx, o: &[Operand]) -> Operand {
    opfin((|| {
        check_array_args(ctx, &[&o[0], &o[1]])?;
        let lower = opnum(ctx, &o[2])?;
        let upper = match o.get(3) {
            None | Some(Operand::Missing) => lower,
            Some(op) => opnum(ctx, op)?,
        };
        let xs = flat_values(ctx, &o[0])?;
        let ps = flat_values(ctx, &o[1])?;
        if xs.len() != ps.len() {
            return Err(XlError::NA);
        }
        let mut total = 0.0;
        let mut acc = 0.0;
        let mut counted = 0;
        for (x, p) in xs.iter().zip(ps.iter()) {
            if let Value::Error(e) = x {
                return Err(*e);
            }
            if let Value::Error(e) = p {
                return Err(*e);
            }

            let (&Value::Number(x), &Value::Number(p)) = (x, p) else {
                continue;
            };
            counted += 1;
            if !(0.0..=1.0).contains(&p) {
                return Err(XlError::Num);
            }
            total += p;
            if x >= lower && x <= upper {
                acc += p;
            }
        }
        if counted == 0 {
            return Err(XlError::Div0);
        }
        if number::cmp_numbers(total, 1.0) != std::cmp::Ordering::Equal {
            return Err(XlError::Num);
        }
        Ok(acc)
    })())
}
