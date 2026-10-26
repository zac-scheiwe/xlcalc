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

    if va
        .iter()
        .chain(vb.iter())
        .any(|v| matches!(v, Value::Error(XlError::Circ)))
    {
        return Err(XlError::Circ);
    }
    let mut xs = Vec::with_capacity(va.len());
    let mut ys = Vec::with_capacity(va.len());
    for (x, y) in va.iter().zip(vb.iter()) {
        for v in [x, y] {
            if let Value::Error(e) = v {
                return Err(if *e == XlError::Spill {
                    XlError::Value
                } else {
                    *e
                });
            }
        }
        if let (Value::Number(x), Value::Number(y)) = (x, y) {
            xs.push(*x);
            ys.push(*y);
        }
    }
    Ok((xs, ys))
}

fn co_moment(x: &[f64], y: &[f64]) -> f64 {
    let n = x.len() as f64;
    let (mut sx, mut sy) = (0.0, 0.0);
    for (a, b) in x.iter().zip(y) {
        sx = x87::add(sx, *a);
        sy = x87::add(sy, *b);
    }
    let (mx, my) = (x87::div(sx, n), x87::div(sy, n));
    let mut sxy = 0.0;
    for (a, b) in x.iter().zip(y) {
        sxy = x87::add(sxy, x87::mul(x87::sub(*a, mx), x87::sub(*b, my)));
    }
    sxy
}

struct Centred {
    mx: f64,
    my: f64,
    sxx: f64,
    syy: f64,
    sxy: f64,
}

fn centred(x: &[f64], y: &[f64], yy: bool) -> R<Centred> {
    use x87::chk;
    let n = x.len() as f64;
    let (mut sx, mut sy) = (0.0, 0.0);
    for (a, b) in x.iter().zip(y) {
        sx = chk(x87::add(sx, *a))?;
        sy = chk(x87::add(sy, *b))?;
    }
    let (mx, my) = (x87::div(sx, n), x87::div(sy, n));
    let (mut sxx, mut syy, mut sxy) = (0.0, 0.0, 0.0);
    for (a, b) in x.iter().zip(y) {
        let dx = chk(x87::sub(*a, mx))?;
        let dy = chk(x87::sub(*b, my))?;
        sxy = chk(x87::add(chk(x87::mul(dx, dy))?, sxy))?;
        sxx = chk(x87::add(chk(x87::mul(dx, dx))?, sxx))?;
        if yy {
            syy = chk(x87::add(chk(x87::mul(dy, dy))?, syy))?;
        }
    }
    Ok(Centred {
        mx,
        my,
        sxx,
        syy,
        sxy,
    })
}

fn pearson_xl(x: &[f64], y: &[f64], d: f64) -> R<f64> {
    use x87::chk;
    let c = centred(x, y, true)?;
    if c.sxx == 0.0 || c.syy == 0.0 {
        return Err(XlError::Div0);
    }
    let sd = chk(x87::mul(
        x87::sqrt(x87::div(c.sxx, d)),
        x87::sqrt(x87::div(c.syy, d)),
    ))?;
    chk(x87::div(x87::div(c.sxy, d), sd))
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

fn line(ctx: &mut EvalCtx, y: &Operand, x: &Operand) -> R<(f64, f64)> {
    let (ys, xs) = pairs(ctx, y, x)?;
    if xs.is_empty() {
        return Err(XlError::Div0);
    }
    let c = centred(&xs, &ys, false)?;
    if c.sxx == 0.0 {
        return Err(XlError::Div0);
    }
    let b = x87::chk(x87::div(c.sxy, c.sxx))?;
    let a = x87::chk(x87::add(-x87::chk(x87::mul(c.mx, b))?, c.my))?;
    Ok((b, a))
}

pub fn slope(ctx: &mut EvalCtx, o: &[Operand]) -> Operand {
    opfin(line(ctx, &o[0], &o[1]).map(|t| t.0))
}

pub fn intercept(ctx: &mut EvalCtx, o: &[Operand]) -> Operand {
    opfin(line(ctx, &o[0], &o[1]).map(|t| t.1))
}

pub fn steyx(ctx: &mut EvalCtx, o: &[Operand]) -> Operand {
    opfin((|| {
        use x87::chk;
        let (ys, xs) = pairs(ctx, &o[0], &o[1])?;

        let c = centred(&xs, &ys, true)?;
        if xs.len() < 3 {
            return Err(XlError::Div0);
        }
        if c.sxx == 0.0 {
            return Err(XlError::Div0);
        }

        let n2 = xs.len() as f64 - 2.0;
        let q = chk(x87::div(chk(x87::mul(c.sxy, c.sxy))?, c.sxx))?;
        let r = x87::div(chk(x87::add(c.syy, -q))?, n2);

        if r.is_nan() || r < 0.0 {
            return Err(XlError::Num);
        }
        Ok(x87::sqrt(r))
    })())
}

pub fn forecast(ctx: &mut EvalCtx, o: &[Operand]) -> Operand {
    opfin((|| {
        let x = opnum(ctx, &o[0])?;
        let (b, a) = line(ctx, &o[1], &o[2])?;
        x87::chk(x87::add(x87::chk(x87::mul(b, x))?, a))
    })())
}

pub fn covar(ctx: &mut EvalCtx, o: &[Operand], sample: bool) -> Operand {
    opfin((|| {
        let (x, y) = pairs(ctx, &o[0], &o[1])?;
        let n = x.len() as f64;
        if n == 0.0 || (sample && n < 2.0) {
            return Err(XlError::Div0);
        }

        Ok(x87::div(
            co_moment(&x, &y),
            if sample { n - 1.0 } else { n },
        ))
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

        if sig >= 309.0 {
            return Err(XlError::Div0);
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

        let v = match &o[1] {
            Operand::Ref(_) => ref_numbers(ctx, &o[1])?,
            Operand::Val(Value::Error(e)) => return Err(*e),
            _ => return Err(XlError::Value),
        };
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
        let data = collect_numbers(ctx, std::slice::from_ref(&o[0]), SUM_POLICY)?;
        let mut v = data.clone();
        v.sort_by(|a, b| a.partial_cmp(b).unwrap());
        if !(0.0..1.0).contains(&p) || v.is_empty() {
            return Err(XlError::Num);
        }

        let n = v.len();
        let k = x87::div(x87::mul(n as f64, p), 2.0).trunc() as usize;
        if 2 * k >= n {
            return Err(XlError::Num);
        }
        let (lo, hi) = (v[k], v[n - 1 - k]);
        let (mut klo, mut khi) = (k, k);
        let mut acc = 0.0;
        for &x in &data {
            if x < lo {
                klo -= 1;
            } else if x > hi {
                khi -= 1;
            } else {
                acc = x87::add(acc, x);
            }
        }
        for _ in 0..klo {
            acc = x87::sub(acc, lo);
        }
        for _ in 0..khi {
            acc = x87::sub(acc, hi);
        }
        Ok(x87::div(acc, (n - 2 * k) as f64))
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

fn moment_base(v: &[f64], population: bool, f: &mut x87::MpFlag) -> (f64, f64, f64) {
    let n = v.len() as f64;
    let mut sum = 0.0;
    for &x in v {
        sum = f.add(sum, x);
    }
    let m = f.div(sum, n);
    let mut ss = 0.0;
    for &x in v {
        let d = f.add(x, -m);
        let d2 = f.mul(d, d);
        ss = f.add(ss, d2);
    }
    let q = f.div(ss, if population { n } else { n - 1.0 });
    let s = f.sqrt(q);
    (m, s, n)
}

fn moment_sum(v: &[f64], m: f64, s: f64, fourth: bool, f: &mut x87::MpFlag) -> f64 {
    let mut acc = 0.0;
    for &x in v {
        let d = f.add(x, -m);
        let t = f.div(d, s);
        let t2 = f.mul(t, t);
        let p = f.mul(t2, if fourth { t2 } else { t });
        acc = f.add(acc, p);
    }
    acc
}

pub fn kurt(ctx: &mut EvalCtx, o: &[Operand]) -> Operand {
    opfin((|| {
        let v = collect_numbers(ctx, o, SUM_POLICY)?;
        if v.len() < 4 {
            return Err(XlError::Div0);
        }

        let mut f = x87::MpFlag::default();
        let (m, s, n) = moment_base(&v, false, &mut f);
        let acc = moment_sum(&v, m, s, true, &mut f);

        let (a, b, c) = (n - 1.0, n - 2.0, n - 3.0);
        let bc = x87::mul(b, c);
        let g = x87::div(x87::mul(n, n + 1.0), x87::mul(bc, a));
        let h = x87::div(x87::mul(x87::mul(3.0, a), a), bc);
        let r = f.mul(g, acc);
        let r = f.add(r, -h);
        f.result(r)
    })())
}

pub fn skew(ctx: &mut EvalCtx, o: &[Operand], population: bool) -> Operand {
    opfin((|| {
        let v = collect_numbers(ctx, o, SUM_POLICY)?;
        if v.len() < 3 {
            return Err(XlError::Div0);
        }
        let mut f = x87::MpFlag::default();
        let (m, s, n) = moment_base(&v, population, &mut f);
        let acc = moment_sum(&v, m, s, false, &mut f);

        let r = if population {
            f.div(acc, n)
        } else {
            f.mul(acc, x87::div(n, x87::mul(n - 1.0, n - 2.0)))
        };
        f.result(r)
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
