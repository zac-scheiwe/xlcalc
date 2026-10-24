use super::*;
use crate::eval::call::call_lifted;
use crate::value::Array;
use crate::x87;

fn numeric_grid(ctx: &mut EvalCtx, op: &Operand) -> R<(usize, usize, Vec<f64>)> {
    let (r, c, vals) = grid_values(ctx, op)?;
    let mut out = Vec::with_capacity(vals.len());
    for v in vals {
        match v {
            Value::Number(n) => out.push(n),
            Value::Error(e) => return Err(e),
            _ => return Err(XlError::Value),
        }
    }
    Ok((r as usize, c as usize, out))
}

struct Design {
    row: bool,
    y: Vec<f64>,

    x: Vec<Vec<f64>>,
}

fn present(o: Option<&Operand>) -> Option<&Operand> {
    match o {
        None | Some(Operand::Missing) => None,
        Some(op) => Some(op),
    }
}

fn to_numbers(vals: Vec<Value>) -> R<Vec<f64>> {
    vals.into_iter()
        .map(|v| match v {
            Value::Number(n) => Ok(n),
            _ => Err(XlError::Value),
        })
        .collect()
}

fn to_numbers_e(vals: Vec<Value>) -> R<Vec<f64>> {
    let mut out = Vec::with_capacity(vals.len());
    for v in vals {
        match v {
            Value::Number(n) => out.push(n),
            Value::Error(e) => return Err(e),
            _ => return Err(XlError::Value),
        }
    }
    Ok(out)
}

fn design(
    ctx: &mut EvalCtx,
    yo: &Operand,
    xo: Option<&Operand>,
    flags: Option<XlError>,
) -> R<Design> {
    let (ry, cy, yv) = grid_values(ctx, yo)?;
    let (ry, cy) = (ry as usize, cy as usize);
    let n = yv.len();
    let row = ry == 1 && cy > 1;
    let Some(xo) = xo else {
        if let Some(e) = flags {
            return Err(e);
        }
        let y = to_numbers(yv)?;
        return Ok(Design {
            row,
            y,
            x: vec![(1..=n).map(|i| i as f64).collect()],
        });
    };
    let (rx, cx, xv) = grid_values(ctx, xo)?;

    if let Some(e) = flags {
        return Err(e);
    }
    let (rx, cx) = (rx as usize, cx as usize);
    let same = rx == ry && cx == cy;
    let by_col = cy == 1 && rx == n;
    let by_row = ry == 1 && cx == n;
    if !(same || by_col || by_row) {
        return Err(XlError::Ref);
    }

    let row = row || (n == 1 && !same && by_row && rx > 1);
    let y = to_numbers(yv)?;
    let xv = to_numbers(xv)?;
    let x = if same {
        vec![xv]
    } else if by_col {
        (0..cx)
            .map(|j| (0..n).map(|i| xv[i * cx + j]).collect())
            .collect()
    } else {
        (0..rx)
            .map(|j| (0..n).map(|i| xv[j * cx + i]).collect())
            .collect()
    };
    Ok(Design { row, y, x })
}

struct Fit {
    m: Vec<f64>,
    b: f64,

    se_m: Vec<f64>,
    se_b: Option<f64>,
    r2: f64,
    se_y: f64,
    f: f64,
    df: f64,
    ss_reg: f64,
    ss_resid: f64,
}

const COLLINEAR_EPS: f64 = 4.547473508864641e-13;

fn dot87(a: &[f64], b: &[f64]) -> f64 {
    let mut s = 0.0;
    for (&x, &y) in a.iter().zip(b) {
        s = x87::add(s, x87::mul(x, y));
    }
    s
}

fn fit(d: &Design, constant: bool) -> R<Fit> {
    let n = d.y.len();
    let k = d.x.len();
    let nf = n as f64;
    let mean = |v: &[f64]| {
        let mut s = 0.0;
        for &t in v {
            s = x87::add(s, t);
        }
        x87::div(s, nf)
    };
    let xbar: Vec<f64> = d.x.iter().map(|c| mean(c)).collect();
    let ybar = mean(&d.y);

    let mut cols: Vec<Vec<f64>> =
        d.x.iter()
            .zip(&xbar)
            .map(|(c, &m)| {
                let m = if constant { m } else { 0.0 };
                c.iter().map(|&t| x87::sub(t, m)).collect()
            })
            .collect();
    let mut orig: Vec<f64> = cols.iter().map(|c| dot87(c, c)).collect();
    if constant {
        cols.push(vec![1.0; n]);
        orig.push(nf);
    }
    let yc = if constant { ybar } else { 0.0 };
    let mut qty: Vec<f64> = d.y.iter().map(|&t| x87::sub(t, yc)).collect();
    let ss_total = dot87(&qty, &qty);
    let mut down = orig.clone();

    let ncols = cols.len();
    let mut perm: Vec<usize> = (0..ncols).collect();
    let mut rdiag: Vec<f64> = Vec::new();
    let mut steps = 0usize;
    while steps < ncols && steps < n {
        let kk = steps;
        let pos = if constant && kk == 0 {
            ncols - 1
        } else {
            let mut best = kk;
            for p in kk + 1..ncols {
                if x87::sub(down[perm[p]], down[perm[best]]) > 0.0 {
                    best = p;
                }
            }
            let j = perm[best];
            if down[j] < x87::mul(orig[j], COLLINEAR_EPS) {
                break;
            }
            best
        };
        perm.swap(kk, pos);
        let pj = perm[kk];
        let piv = &cols[pj];
        let sk = dot87(&piv[kk..], &piv[kk..]);
        let sk1 = dot87(&piv[kk + 1..], &piv[kk + 1..]);
        let norm = x87::sqrt(sk);
        let a = piv[kk];
        let rest: Vec<usize> = perm[kk + 1..].to_vec();
        if norm == 0.0 {
            rdiag.push(0.0);
        } else {
            let vk = if a > 0.0 {
                x87::add(a, norm)
            } else {
                x87::sub(a, norm)
            };
            rdiag.push(if a > 0.0 { -norm } else { norm });
            let vtv = x87::add(x87::mul(vk, vk), sk1);
            let v: Vec<f64> = std::iter::once(vk)
                .chain(piv[kk + 1..].iter().copied())
                .collect();
            let reflect = |c: &mut [f64]| {
                let s = dot87(&v, &c[kk..]);
                let f = x87::div(x87::mul(2.0, s), vtv);
                for (t, &vi) in c[kk..].iter_mut().zip(&v) {
                    *t = x87::sub(*t, x87::mul(f, vi));
                }
            };
            for &j in &rest {
                reflect(&mut cols[j][..]);
            }
            reflect(&mut qty[..]);
        }
        for &j in &rest {
            let r = cols[j][kk];
            down[j] = x87::sub(down[j], x87::mul(r, r));
        }
        steps += 1;
    }
    let r = steps;

    let rr = |i: usize, jj: usize| {
        if i == jj {
            rdiag[i]
        } else {
            cols[perm[jj]][i]
        }
    };

    let mut beta = vec![0.0; r];
    for i in (0..r).rev() {
        let mut s = 0.0;
        for jj in (i + 1..r).rev() {
            s = x87::add(s, x87::mul(rr(i, jj), beta[jj]));
        }
        let t = x87::sub(qty[i], s);
        beta[i] = if rdiag[i] == 0.0 {
            0.0
        } else {
            x87::div(t, rdiag[i])
        };
    }

    let mut coef = vec![0.0; ncols];
    let mut at: Vec<Option<usize>> = vec![None; ncols];
    for i in 0..r {
        coef[perm[i]] = beta[i];
        at[perm[i]] = Some(i);
    }
    let mut ss_resid = 0.0;
    for &t in &qty[r.min(n)..] {
        ss_resid = x87::add(ss_resid, x87::mul(t, t));
    }
    let ss_reg = x87::sub(ss_total, ss_resid);
    let p = r;
    let off = usize::from(constant);
    let df = nf - p as f64;
    let df_reg = (p - off.min(p)) as f64;
    let ms_resid = if df > 0.0 {
        x87::div(ss_resid, df)
    } else {
        0.0
    };

    let mut rinv = vec![vec![0.0; r]; r];
    for i in (0..r).rev() {
        for c in 0..r {
            let mut s = if i == c { 1.0 } else { 0.0 };
            for jj in i + 1..r {
                s = x87::sub(s, x87::mul(rr(i, jj), rinv[jj][c]));
            }
            rinv[i][c] = if rdiag[i] == 0.0 {
                0.0
            } else {
                x87::div(s, rdiag[i])
            };
        }
    }
    let cov = |a: usize, b: usize| {
        if df <= 0.0 {
            return 0.0;
        }
        let mut s = 0.0;
        for t in 0..r {
            s = x87::add(s, x87::mul(rinv[a][t], rinv[b][t]));
        }
        x87::div(x87::mul(s, ss_resid), df)
    };
    let cov_cols = |a: usize, b: usize| match (at[a], at[b]) {
        (Some(i), Some(j)) => cov(i, j),
        _ => 0.0,
    };
    let se_m: Vec<f64> = (0..k).map(|j| x87::sqrt(cov_cols(j, j))).collect();
    let (b, se_b) = if constant {
        let mut b = ybar;
        for j in 0..k {
            b = x87::sub(b, x87::mul(xbar[j], coef[j]));
        }
        let o = k;
        let mut v = cov_cols(o, o);
        for j in 0..k {
            v = x87::add(v, x87::mul(x87::mul(xbar[j], xbar[j]), cov_cols(j, j)));
        }
        for j in 0..k {
            for l in j + 1..k {
                let t = x87::mul(x87::mul(2.0 * xbar[j], xbar[l]), cov_cols(j, l));
                v = x87::add(v, t);
            }
        }
        for j in 0..k {
            v = x87::add(v, x87::mul(-2.0 * xbar[j], cov_cols(o, j)));
        }
        (b, Some(x87::sqrt(v)))
    } else {
        (0.0, None)
    };
    let r2 = if ss_total == 0.0 {
        1.0
    } else {
        x87::div(ss_reg, ss_total)
    };
    let f = if df > 0.0 && ss_resid > 0.0 {
        x87::div(x87::div(ss_reg, df_reg), ms_resid)
    } else {
        f64::NAN
    };
    Ok(Fit {
        m: coef[..k].to_vec(),
        b,
        se_m,
        se_b,
        r2,
        se_y: x87::sqrt(ms_resid),
        f,
        df,
        ss_reg,
        ss_resid,
    })
}

pub fn fit_line(xs: &[f64], ys: &[f64], log: bool) -> Option<(f64, f64)> {
    let mut y = ys.to_vec();
    if log {
        for v in y.iter_mut() {
            if *v <= 0.0 {
                return None;
            }
            *v = x87::ln(*v);
        }
    }
    let d = Design {
        row: false,
        y,
        x: vec![xs.to_vec()],
    };
    let f = fit(&d, true).ok()?;
    Some((f.m[0], f.b))
}

fn opt_flag(o: Option<&Operand>, default: bool) -> R<bool> {
    match o {
        None | Some(Operand::Missing) => Ok(default),
        Some(Operand::Val(v)) => boolean(v),
        Some(Operand::Ref(_)) => Err(XlError::Value),
    }
}

fn log_y(d: &mut Design) -> R<()> {
    for v in d.y.iter_mut() {
        if *v <= 0.0 {
            return Err(XlError::Num);
        }
        *v = x87::ln(*v);
    }
    Ok(())
}

const LIFT_LINEST: FnDef = args("LINEST", 1, 4, "RRVV", |_, _| Operand::Missing);
const LIFT_TREND: FnDef = args("TREND", 1, 4, "RRRV", |_, _| Operand::Missing);

fn flags_first_cell(ctx: &mut EvalCtx, ops: &[Operand], flags: &[usize]) -> Vec<Operand> {
    let mut ops = ops.to_vec();
    for &i in flags {
        if let Some(Operand::Ref(r)) = ops.get(i) {
            if let Some(a) = r.single() {
                let v = ctx.cell_value(a.sheet, a.rect.r0, a.rect.c0);
                ops[i] = Operand::Val(v);
            }
        }
    }
    ops
}

pub fn linest_lifted(ctx: &mut EvalCtx, o: &[Operand], log: bool) -> Operand {
    let ops = flags_first_cell(ctx, o, &[2, 3]);
    call_lifted(ctx, &LIFT_LINEST, ops, &mut |c, o| linest(c, o, log))
}

pub fn trend_lifted(ctx: &mut EvalCtx, o: &[Operand], log: bool) -> Operand {
    let ops = flags_first_cell(ctx, o, &[3]);
    call_lifted(ctx, &LIFT_TREND, ops, &mut |c, o| trend(c, o, log))
}

pub fn linest(ctx: &mut EvalCtx, o: &[Operand], log: bool) -> Operand {
    let r = (|| {
        let (constant, stats) = (opt_flag(o.get(2), true), opt_flag(o.get(3), false));

        let fe = constant.as_ref().err().or(stats.as_ref().err()).copied();
        let mut d = design(ctx, &o[0], present(o.get(1)), fe)?;
        let (constant, stats) = (constant?, stats?);
        if log {
            log_y(&mut d)?;
        }
        let f = fit(&d, constant)?;
        let k = f.m.len();
        let cols = k + 1;
        let tr = |x: f64| if log { x87::exp(x) } else { x };
        let mut first: Vec<Value> = f.m.iter().rev().map(|&m| Value::num(tr(m))).collect();
        first.push(Value::num(if constant {
            tr(f.b)
        } else if log {
            1.0
        } else {
            0.0
        }));
        if !stats {
            return Ok(Array::new(1, cols as u32, first).into_value());
        }
        let na = || Value::Error(XlError::NA);
        let mut data = first;
        data.extend(f.se_m.iter().rev().map(|&s| Value::num(s)));
        data.push(match f.se_b {
            Some(s) => Value::num(s),
            None => na(),
        });
        let mut row3 = vec![Value::num(f.r2), Value::num(f.se_y)];
        let mut row4 = vec![Value::num(f.f), Value::num(f.df)];
        let mut row5 = vec![Value::num(f.ss_reg), Value::num(f.ss_resid)];
        for row in [&mut row3, &mut row4, &mut row5] {
            row.truncate(cols);
            while row.len() < cols {
                row.push(na());
            }
        }
        data.extend(row3);
        data.extend(row4);
        data.extend(row5);
        Ok(Array::new(5, cols as u32, data).into_value())
    })();
    Operand::Val(val(r))
}

pub fn trend(ctx: &mut EvalCtx, o: &[Operand], log: bool) -> Operand {
    let r = (|| {
        let constant = opt_flag(o.get(3), true);
        let xo = present(o.get(1));

        let new_x_err = match o.get(2) {
            Some(Operand::Val(Value::Error(e))) => Some(*e),
            _ => None,
        };
        let flag_err = new_x_err.or(constant.as_ref().err().copied());
        let mut d = design(ctx, &o[0], xo, flag_err)?;
        let constant = constant?;
        if log {
            log_y(&mut d)?;
        }
        let f = fit(&d, constant)?;
        let k = f.m.len();

        let (rows, cols, nx) = match present(o.get(2)) {
            Some(op) => {
                let (r, c, vals) = grid_values(ctx, op)?;
                let (r, c) = (r as usize, c as usize);
                if k > 1 && (if d.row { r != k } else { c != k }) {
                    return Err(XlError::Ref);
                }
                (r, c, to_numbers_e(vals)?)
            }
            None => match xo {
                Some(op) => numeric_grid(ctx, op)?,
                None => {
                    let n = d.y.len();
                    let v: Vec<f64> = (1..=n).map(|i| i as f64).collect();
                    if d.row {
                        (1, n, v)
                    } else {
                        (n, 1, v)
                    }
                }
            },
        };

        let predict = |xs: &[f64]| -> Value {
            if log {
                let mut p = 1.0;
                for (j, &x) in xs.iter().enumerate() {
                    p = x87::mul(p, x87::exp(x87::mul(f.m[j], x)));
                }
                return Value::num(x87::mul(p, x87::exp(f.b)));
            }
            let mut s = 0.0;
            for (j, &x) in xs.iter().enumerate() {
                s = x87::add(s, x87::mul(f.m[j], x));
            }
            Value::num(x87::add(f.b, s))
        };
        if k == 1 {
            let out: Vec<Value> = nx.iter().map(|&x| predict(&[x])).collect();
            return Ok(array_or_scalar(rows as u32, cols as u32, |r, c| {
                out[r as usize * cols + c as usize].clone()
            }));
        }

        if d.row {
            if rows != k {
                return Err(XlError::Ref);
            }
            let out: Vec<Value> = (0..cols)
                .map(|i| predict(&(0..k).map(|j| nx[j * cols + i]).collect::<Vec<_>>()))
                .collect();
            Ok(array_or_scalar(1, cols as u32, |_, c| {
                out[c as usize].clone()
            }))
        } else {
            if cols != k {
                return Err(XlError::Ref);
            }
            let out: Vec<Value> = (0..rows)
                .map(|i| predict(&nx[i * cols..(i + 1) * cols]))
                .collect();
            Ok(array_or_scalar(rows as u32, 1, |r, _| {
                out[r as usize].clone()
            }))
        }
    })();
    Operand::Val(val(r))
}
