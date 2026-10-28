use std::sync::Arc;

use super::args::*;
use super::{args, lazy, volatile, FnDef, MANY254};
use crate::coerce;
use crate::eval::lambda::{invoke, LambdaValue};
use crate::eval::{EvalCtx, Operand};
use crate::formula::Node;
use crate::value::{Array, Value, XlError};
use crate::workbook::MAX_ARRAY_CELLS;

pub const DEFS: &[FnDef] = &[
    args("SEQUENCE", 1, 4, "VVVV", sequence),
    volatile(args("RANDARRAY", 0, 5, "VVVVV", randarray)),
    lazy("ISOMITTED", 1, 1, isomitted),
    args("MAP", 2, MANY254, "R", map),
    args("REDUCE", 3, 3, "R", |c, o| reduce_scan(c, o, false)),
    args("SCAN", 3, 3, "R", |c, o| reduce_scan(c, o, true)),
    args("MAKEARRAY", 3, 3, "VVR", makearray),
    args("BYROW", 1, 2, "R", |c, o| by_line(c, o, true)),
    args("BYCOL", 1, 2, "R", |c, o| by_line(c, o, false)),
    args("FILTER", 2, 3, "MMR", filter),
    args("VSTACK", 1, MANY254, "M", |c, o| stack(c, o, true)),
    args("HSTACK", 1, MANY254, "M", |c, o| stack(c, o, false)),
];

fn n_arg(ctx: &EvalCtx, ops: &[Operand], i: usize, d: f64) -> R<f64> {
    match ops.get(i) {
        None | Some(Operand::Missing) => Ok(d),
        Some(Operand::Val(v)) => ctx.to_number(v),
        Some(_) => Err(XlError::Value),
    }
}

fn check_size(ctx: &mut EvalCtx, rows: f64, cols: f64) -> R<(u32, u32)> {
    if rows < 0.0 || cols < 0.0 {
        return Err(XlError::Value);
    }

    if rows > 1048576.0 || cols > 1048576.0 {
        return Err(XlError::Value);
    }
    if rows < 1.0 || cols < 1.0 {
        return Err(XlError::Calc);
    }
    if ctx.too_big(rows * cols) || rows * cols > MAX_ARRAY_CELLS as f64 {
        return Err(XlError::Num);
    }
    Ok((rows as u32, cols as u32))
}

fn sequence(ctx: &mut EvalCtx, ops: &[Operand]) -> Operand {
    opval((|| {
        let rows = sfix(n_arg(ctx, ops, 0, 1.0)?) as f64;
        let cols = sfix(n_arg(ctx, ops, 1, 1.0)?) as f64;
        let start = n_arg(ctx, ops, 2, 1.0)?;
        let step = n_arg(ctx, ops, 3, 1.0)?;
        let (rows, cols) = check_size(ctx, rows, cols)?;

        let mut next = start;
        Ok(array_always(rows, cols, |_, _| {
            let v = Value::num(next);
            next += step;
            v
        }))
    })())
}

fn randarray(ctx: &mut EvalCtx, ops: &[Operand]) -> Operand {
    opval((|| {
        let rows = sfix(n_arg(ctx, ops, 0, 1.0)?) as f64;
        let cols = sfix(n_arg(ctx, ops, 1, 1.0)?) as f64;
        let lo = n_arg(ctx, ops, 2, 0.0)?;
        let hi = n_arg(ctx, ops, 3, 1.0)?;
        let whole = match ops.get(4) {
            None | Some(Operand::Missing) => false,
            Some(Operand::Val(v)) => boolean(v)?,
            Some(_) => return Err(XlError::Value),
        };
        let (rows, cols) = check_size(ctx, rows, cols)?;
        if lo > hi || (whole && (lo.fract() != 0.0 || hi.fract() != 0.0)) {
            return Err(XlError::Value);
        }
        let wb = ctx.wb;
        Ok(array_always(rows, cols, |_, _| {
            let r = wb.random();
            Value::num(if whole {
                (lo.ceil() + (r * (hi.floor() - lo.ceil() + 1.0)).floor()).min(hi.floor())
            } else {
                lo + r * (hi - lo)
            })
        }))
    })())
}

fn isomitted(ctx: &mut EvalCtx, args: &[Node]) -> Operand {
    match args[0].unwrap_ws() {
        Node::Local { slot, .. } => Operand::Val(Value::Bool(matches!(
            ctx.env.get(*slot as usize),
            Some(Operand::Missing)
        ))),
        Node::Missing => Operand::Val(Value::Bool(true)),
        _ => Operand::Val(Value::Bool(false)),
    }
}

fn elem_arg(op: &Operand, arr: &Array, idx: usize) -> Operand {
    if let Operand::Ref(r) = op {
        if let Some(a) = r.single() {
            if a.rect.r1 - a.rect.r0 + 1 == arr.rows
                && a.rect.c1 - a.rect.c0 + 1 == arr.cols
                && arr.cols > 0
            {
                let (dr, dc) = (idx as u32 / arr.cols, idx as u32 % arr.cols);
                let (r, c) = (a.rect.r0 + dr, a.rect.c0 + dc);
                return Operand::Ref(crate::eval::Reference::Area(crate::eval::Area {
                    sheet: a.sheet,
                    rect: crate::address::Rect::new(r, c, r, c),
                }));
            }
        }
    }
    Operand::Val(arr.data[idx].clone())
}

fn lambda_arg(op: &Operand, arity: usize) -> R<Arc<LambdaValue>> {
    match op {
        Operand::Val(Value::Lambda(l)) if l.arity() == arity => Ok(l.clone()),
        Operand::Val(Value::Lambda(_)) => Err(XlError::Value),
        Operand::Val(Value::Error(e)) => Err(*e),
        _ => Err(XlError::Value),
    }
}

fn blank_in_used(ctx: &EvalCtx, out: &Operand) -> bool {
    match out {
        Operand::Ref(r) => r.single().is_some_and(|a| {
            a.rect.is_cell()
                && ctx
                    .wb
                    .used_range(a.sheet)
                    .is_some_and(|u| u.contains(a.rect.r0, a.rect.c0))
        }),
        _ => false,
    }
}

fn call_scalar(
    ctx: &mut EvalCtx,
    f: &LambdaValue,
    args: Vec<Operand>,
    hard: &mut bool,
) -> R<Value> {
    let out = invoke(ctx, f, args);
    let in_used = blank_in_used(ctx, &out);
    let v = match ctx.deref(out) {
        Value::Array(a) if a.rows == 1 && a.cols == 1 => a.data[0].clone(),
        Value::Array(_) => return Err(XlError::Calc),
        v => v,
    };

    if matches!(v, Value::Blank) && !in_used {
        *hard = true;
    }
    if matches!(v, Value::Blank) {
        let value_making = match &f.def.body {
            crate::formula::Node::Unary(..) => true,
            crate::formula::Node::Func { id, .. } => matches!(
                crate::functions::def(*id).name,
                "IFERROR" | "IFNA" | "LOOKUP" | "FILTER" | "CHOOSEROWS" | "UNIQUE" | "VSTACK"
            ),
            _ => false,
        };
        if value_making {
            return Ok(Value::Number(0.0));
        }
    }
    Ok(v)
}

fn arrays(ctx: &mut EvalCtx, ops: &[Operand]) -> R<Vec<Arc<Array>>> {
    ops.iter()
        .map(|op| match op {
            Operand::Val(Value::Error(e)) => Err(*e),
            _ => Grid::from_operand(op).map(|g| g.to_array(ctx)),
        })
        .collect()
}

fn map(ctx: &mut EvalCtx, ops: &[Operand]) -> Operand {
    opval((|| {
        let (arrs, f) = ops.split_at(ops.len() - 1);
        let n_arrays = arrs.len();
        let op_list: Vec<Operand> = arrs.to_vec();

        if let Operand::Val(Value::Error(e)) = &arrs[0] {
            return Err(*e);
        }
        if let Operand::Val(Value::Error(e)) = &f[0] {
            return Err(*e);
        }
        if arrs[1..]
            .iter()
            .any(|o| matches!(o, Operand::Val(Value::Error(_))))
        {
            return Err(XlError::Value);
        }

        for (i, op) in arrs.iter().enumerate() {
            match op {
                Operand::Missing => return Err(XlError::Value),
                Operand::Val(Value::Array(_)) | Operand::Ref(_) | Operand::Val(Value::Error(_)) => {
                }
                Operand::Val(_) if i > 0 => return Err(XlError::Value),
                _ => {}
            }
        }

        if let Operand::Val(Value::Lambda(l)) = &f[0] {
            if l.arity() != n_arrays {
                let shape = |o: &Operand| match o {
                    Operand::Ref(r) => r
                        .areas()
                        .first()
                        .map_or((1, 1), |a| (a.rect.rows(), a.rect.cols())),
                    Operand::Val(Value::Array(a)) => (a.rows, a.cols),
                    _ => (1, 1),
                };
                let rows = op_list.iter().map(|o| shape(o).0).max().unwrap();
                let cols = op_list.iter().map(|o| shape(o).1).max().unwrap();
                return Ok(Array::filled(rows, cols, Value::Error(XlError::NA)).into_value());
            }
        }
        let arrs = arrays(ctx, arrs)?;
        let rows = arrs.iter().map(|a| a.rows).max().unwrap();
        let cols = arrs.iter().map(|a| a.cols).max().unwrap();
        let f = lambda_arg(&f[0], n_arrays)?;
        let mut hard = false;
        let mut data = Vec::with_capacity(rows as usize * cols as usize);
        for r in 0..rows {
            for c in 0..cols {
                if arrs.iter().any(|a| r >= a.rows || c >= a.cols) {
                    data.push(Value::Error(XlError::NA));
                    continue;
                }

                let lin = (r * cols + c) as usize;
                let args = arrs
                    .iter()
                    .zip(op_list.iter())
                    .map(|(a, o)| elem_arg(o, a, lin.min(a.data.len() - 1)))
                    .collect();
                data.push(call_scalar(ctx, &f, args, &mut hard)?);
                if ctx.interrupted() {
                    return Err(XlError::Calc);
                }
            }
        }
        Ok(Array::new(rows, cols, data).with_lam(hard).into_value())
    })())
}

fn reduce_scan(ctx: &mut EvalCtx, ops: &[Operand], scan: bool) -> Operand {
    let r = (|| {
        let omitted = matches!(ops[0], Operand::Missing);
        let arr = arrays(ctx, &ops[1..2])?.pop().unwrap();

        if let Operand::Val(Value::Lambda(l)) = &ops[2] {
            if l.arity() != 2 {
                if !scan {
                    return Err(XlError::NA);
                }
                let mut data = vec![Value::Error(XlError::NA); arr.len()];
                if omitted {
                    data[0] = arr.data[0].clone();
                }
                return Ok(Operand::Val(
                    Array::new(arr.rows, arr.cols, data).into_value(),
                ));
            }
        }
        let f = lambda_arg(&ops[2], 2)?;

        let (mut acc, skip) = if omitted {
            (Operand::Val(arr.data[0].clone()), 1)
        } else if scan {
            (Operand::Val(ctx.deref(ops[0].clone())), 0)
        } else {
            (ops[0].clone(), 0)
        };
        let mut out = Vec::with_capacity(if scan { arr.len() } else { 0 });
        let mut hard = false;
        if scan && omitted {
            out.push(arr.data[0].clone());
        }
        for i in skip..arr.data.len() {
            let res = invoke(ctx, &f, vec![acc, elem_arg(&ops[1], &arr, i)]);
            let in_used = blank_in_used(ctx, &res);
            acc = if scan || !matches!(res, Operand::Ref(_)) {
                Operand::Val(ctx.deref(res))
            } else {
                res
            };
            hard |= matches!(acc, Operand::Val(Value::Blank)) && !in_used;
            if ctx.interrupted() {
                return Err(XlError::Calc);
            }
            if scan {
                out.push(match &acc {
                    Operand::Val(Value::Array(a)) if a.data.len() == 1 => a.data[0].clone(),
                    Operand::Val(Value::Array(_)) => return Err(XlError::Calc),
                    Operand::Val(v) => v.clone(),
                    _ => Value::Blank,
                });
            }
        }
        Ok(if scan {
            Operand::Val(
                Array::new(arr.rows, arr.cols, out)
                    .with_lam(hard || omitted)
                    .into_value(),
            )
        } else {
            match acc {
                Operand::Missing => Operand::Val(Value::Number(0.0)),
                acc => acc,
            }
        })
    })();
    r.unwrap_or_else(Operand::err)
}

fn makearray(ctx: &mut EvalCtx, ops: &[Operand]) -> Operand {
    opval((|| {
        let rows = n_arg(ctx, ops, 0, 1.0)?;
        let cols = n_arg(ctx, ops, 1, 1.0)?;
        let wrong_arity = matches!(&ops[2], Operand::Val(Value::Lambda(l)) if l.arity() != 2);

        if rows.fract() != 0.0
            || cols.fract() != 0.0
            || rows < 1.0
            || cols < 1.0
            || rows >= 4294967295.0
            || cols >= 4294967295.0
        {
            return Err(XlError::Value);
        }
        if ctx.too_big(rows * cols) {
            return Err(XlError::Num);
        }
        if rows > 1048576.0
            || cols > 16384.0
            || rows * cols > crate::workbook::MAX_ARRAY_CELLS as f64
        {
            return Err(XlError::Spill);
        }
        let (rows, cols) = (rows as u32, cols as u32);
        if wrong_arity {
            return Ok(Array::filled(rows, cols, Value::Error(XlError::NA)).into_value());
        }
        let f = lambda_arg(&ops[2], 2)?;
        let mut hard = false;
        let mut data = Vec::with_capacity(rows as usize * cols as usize);
        for r in 0..rows {
            for c in 0..cols {
                let args = vec![
                    Operand::Val(Value::Number(r as f64 + 1.0)),
                    Operand::Val(Value::Number(c as f64 + 1.0)),
                ];
                data.push(call_scalar(ctx, &f, args, &mut hard)?);
                if ctx.interrupted() {
                    return Err(XlError::Calc);
                }
            }
        }
        Ok(Array::new(rows, cols, data).with_lam(hard).into_value())
    })())
}

fn by_line(ctx: &mut EvalCtx, ops: &[Operand], by_row: bool) -> Operand {
    if ops.len() == 1 {
        return Operand::err(XlError::Calc);
    }
    opval((|| {
        let arr = arrays(ctx, &ops[..1])?.pop().unwrap();
        let n = if by_row { arr.rows } else { arr.cols };
        if let Operand::Val(Value::Lambda(l)) = &ops[1] {
            if l.arity() != 1 {
                let data = vec![Value::Error(XlError::NA); n as usize];
                return Ok(if n == 1 {
                    data[0].clone()
                } else if by_row {
                    Array::column(data).into_value()
                } else {
                    Array::row(data).into_value()
                });
            }
        }
        let f = lambda_arg(&ops[1], 1)?;

        let src_area = match &ops[0] {
            Operand::Ref(r) => r.single().copied(),
            _ => None,
        };
        let mut hard = false;
        let mut data = Vec::with_capacity(n as usize);
        for i in 0..n {
            let line = if by_row {
                Array::from_fn(1, arr.cols, |_, c| arr.get(i, c).clone())
            } else {
                Array::from_fn(arr.rows, 1, |r, _| arr.get(r, i).clone())
            };
            let arg = match src_area {
                Some(a) => {
                    let rect = if by_row {
                        crate::address::Rect::new(
                            a.rect.r0 + i,
                            a.rect.c0,
                            a.rect.r0 + i,
                            a.rect.c1,
                        )
                    } else {
                        crate::address::Rect::new(
                            a.rect.r0,
                            a.rect.c0 + i,
                            a.rect.r1,
                            a.rect.c0 + i,
                        )
                    };
                    Operand::Ref(crate::eval::Reference::Area(crate::eval::Area {
                        sheet: a.sheet,
                        rect,
                    }))
                }
                _ => Operand::Val(line.into_value()),
            };
            data.push(call_scalar(ctx, &f, vec![arg], &mut hard)?);
            if ctx.interrupted() {
                return Err(XlError::Calc);
            }
        }
        Ok(if by_row {
            Array::column(data)
        } else {
            Array::row(data)
        }
        .with_lam(hard)
        .into_value())
    })())
}

fn filter(ctx: &mut EvalCtx, ops: &[Operand]) -> Operand {
    opval((|| {
        let arr = arrays(ctx, &ops[..1])?.pop().unwrap();
        let inc = arrays(ctx, &ops[1..2])?.pop().unwrap();
        let keep = |v: &Value| -> R<bool> {
            match v {
                Value::Error(e) => Err(*e),
                Value::Blank => Ok(false),
                v => coerce::to_bool(v),
            }
        };
        let picked: Vec<u32>;
        let by_rows;
        if inc.cols == 1 && inc.rows == arr.rows {
            by_rows = true;
            picked = (0..arr.rows)
                .filter_map(|r| keep(inc.get(r, 0)).map(|k| k.then_some(r)).transpose())
                .collect::<R<_>>()?;
        } else if inc.rows == 1 && inc.cols == arr.cols {
            by_rows = false;
            picked = (0..arr.cols)
                .filter_map(|c| keep(inc.get(0, c)).map(|k| k.then_some(c)).transpose())
                .collect::<R<_>>()?;
        } else {
            return Err(XlError::Value);
        }
        if picked.is_empty() {
            return match ops.get(2) {
                None | Some(Operand::Missing) => Err(XlError::Calc),
                Some(Operand::Ref(r)) if r.single().is_none() => Err(XlError::Value),
                Some(op) => Ok(ctx.deref(op.clone())),
            };
        }
        let n = picked.len() as u32;
        Ok(if by_rows {
            array_always(n, arr.cols, |r, c| arr.get(picked[r as usize], c).clone())
        } else {
            array_always(arr.rows, n, |r, c| arr.get(r, picked[c as usize]).clone())
        })
    })())
}

pub fn split_union(ops: &[Operand]) -> Vec<Operand> {
    let mut out = Vec::with_capacity(ops.len());
    for op in ops {
        match op {
            Operand::Ref(
                r @ (crate::eval::Reference::Multi(_) | crate::eval::Reference::Span(_)),
            ) => {
                out.extend(
                    r.areas()
                        .iter()
                        .map(|a| Operand::Ref(crate::eval::Reference::Area(*a))),
                );
            }
            other => out.push(other.clone()),
        }
    }
    out
}

pub fn stack(ctx: &mut EvalCtx, ops: &[Operand], vertical: bool) -> Operand {
    opval((|| {
        let ops = &split_union(ops)[..];
        let arrs = arrays(ctx, ops)?;
        let (rows, cols) = if vertical {
            (
                arrs.iter().map(|a| a.rows).sum::<u32>(),
                arrs.iter().map(|a| a.cols).max().unwrap(),
            )
        } else {
            (
                arrs.iter().map(|a| a.rows).max().unwrap(),
                arrs.iter().map(|a| a.cols).sum::<u32>(),
            )
        };
        check_size(ctx, rows as f64, cols as f64)?;
        let mut data = vec![Value::Error(XlError::NA); rows as usize * cols as usize];
        let mut offset = 0;
        for a in &arrs {
            for r in 0..a.rows {
                for c in 0..a.cols {
                    let (rr, cc) = if vertical {
                        (offset + r, c)
                    } else {
                        (r, offset + c)
                    };

                    data[(rr * cols + cc) as usize] = a.get(r, c).clone();
                }
            }
            offset += if vertical { a.rows } else { a.cols };
        }
        Ok(Array::new(rows, cols, data).into_value())
    })())
}
