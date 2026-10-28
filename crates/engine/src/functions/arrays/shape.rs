use std::cmp::Ordering;
use std::collections::HashMap;
use std::sync::Arc;

use super::super::args::R;
use super::{
    arg_error, arg_error_typed, array_value, check_result_size, collate, opt_flag, opt_number,
    xl_cmp, Src,
};
use crate::address::MAX_ROWS;
use crate::eval::{EvalCtx, Operand};
use crate::value::{Array, Value, XlError};

fn finish(r: R<Value>) -> Operand {
    match r {
        Ok(v) => Operand::Val(v),
        Err(e) => Operand::err(e),
    }
}

fn numbers_of(ctx: &mut EvalCtx, op: &Operand) -> R<Vec<f64>> {
    match op {
        Operand::Val(Value::Error(e)) => Err(*e),
        Operand::Val(Value::Array(a)) => a.data.iter().map(|v| element_number(ctx, v)).collect(),
        Operand::Val(v) => Ok(vec![ctx.to_number(v)?]),
        Operand::Ref(_) => {
            let src = Src::from_operand_3d(op)?;
            let a = src.to_array(ctx)?;
            if a.len() == 1 {
                return Ok(vec![ctx.to_number(&a.data[0])?]);
            }
            a.data.iter().map(|v| element_number(ctx, v)).collect()
        }
        Operand::Missing => Err(XlError::Value),
    }
}

fn element_number(ctx: &EvalCtx, v: &Value) -> R<f64> {
    match v {
        Value::Number(n) => Ok(*n),
        Value::Error(_) => Err(XlError::Value),
        other => ctx.to_number(other).map_err(|_| XlError::Value),
    }
}

fn order_of(n: f64) -> R<bool> {
    let n = super::super::args::sfix(n) as f64;
    if n == 1.0 {
        Ok(false)
    } else if n == -1.0 {
        Ok(true)
    } else {
        Err(XlError::Value)
    }
}

fn sort_cmp(a: &Value, b: &Value, desc: bool) -> Ordering {
    match (a.is_blank(), b.is_blank()) {
        (true, true) => Ordering::Equal,
        (true, false) => Ordering::Greater,
        (false, true) => Ordering::Less,
        _ => {
            let o = xl_cmp(a, b);
            if desc {
                o.reverse()
            } else {
                o
            }
        }
    }
}

fn reorder(a: &Array, by_col: bool, order: &[u32]) -> Value {
    let (rows, cols) = (a.rows, a.cols);
    let mut data = Vec::with_capacity(a.len());
    for r in 0..rows {
        for c in 0..cols {
            let v = if by_col {
                a.get(r, order[c as usize])
            } else {
                a.get(order[r as usize], c)
            };
            data.push(v.clone());
        }
    }
    array_value(rows, cols, data)
}

pub fn sort(ctx: &mut EvalCtx, ops: &[Operand]) -> Operand {
    let mut ops = ops.to_vec();
    if let Some(r) = lift_flags(ctx, &ops, &[3], sort_one, true) {
        return r;
    }

    if let Some(Operand::Ref(r)) = ops.get(3) {
        if r.single().is_none_or(|a| a.rect.is_cell()) {
            let v = ctx.deref(ops[3].clone());
            ops[3] = Operand::Val(v);
        }
    }
    sort_one(ctx, &ops)
}

fn sort_one(ctx: &mut EvalCtx, ops: &[Operand]) -> Operand {
    finish((|| {
        arg_error(&ops[1..])?;
        let src = Src::from_operand_3d(&ops[0])?;
        let by_col = opt_flag(ops.get(3))?;
        let indexes = match ops.get(1) {
            None | Some(Operand::Missing) => vec![1.0],
            Some(op) => numbers_of(ctx, op)?,
        };
        let orders = match ops.get(2) {
            None | Some(Operand::Missing) => vec![1.0],
            Some(op) => numbers_of(ctx, op)?,
        };
        let a = src.to_array(ctx)?;
        let width = if by_col { a.rows } else { a.cols };
        if indexes.len() > width as usize || (orders.len() != 1 && orders.len() != indexes.len()) {
            return Err(XlError::Value);
        }
        let mut keys = Vec::with_capacity(indexes.len());
        for (k, ix) in indexes.iter().enumerate() {
            let ix = super::super::args::sfix(*ix) as f64;
            if ix < 1.0 || ix > width as f64 {
                return Err(XlError::Value);
            }
            let desc = order_of(orders[if orders.len() == 1 { 0 } else { k }])?;
            keys.push((ix as u32 - 1, desc));
        }
        let n = if by_col { a.cols } else { a.rows };
        let mut order: Vec<u32> = (0..n).collect();
        let at = |line: u32, key: u32| {
            if by_col {
                a.get(key, line)
            } else {
                a.get(line, key)
            }
        };

        if (0..n).any(|l| {
            keys.iter()
                .any(|&(k, _)| matches!(at(l, k), Value::Error(XlError::Circ)))
        }) {
            ctx.circ_used();
        }
        order.sort_by(|&x, &y| {
            for &(key, desc) in &keys {
                let o = sort_cmp(at(x, key), at(y, key), desc);
                if o != Ordering::Equal {
                    return o;
                }
            }
            Ordering::Equal
        });
        Ok(reorder(&a, by_col, &order))
    })())
}

fn lift_flags(
    ctx: &mut EvalCtx,
    ops: &[Operand],
    at: &[usize],
    f: fn(&mut EvalCtx, &[Operand]) -> Operand,
    first_ok: bool,
) -> Option<Operand> {
    let mut grids: Vec<(usize, Arc<Array>)> = Vec::new();
    for &i in at {
        match ops.get(i) {
            Some(Operand::Val(Value::Array(a))) => grids.push((i, a.clone())),
            Some(o @ Operand::Ref(_)) => {
                if let Ok(a) = Src::from_operand(o).and_then(|s| s.to_array(ctx)) {
                    if a.data.len() > 1 {
                        grids.push((i, a));
                    }
                }
            }
            _ => {}
        }
    }
    if grids.is_empty() {
        return None;
    }
    let rows = grids.iter().map(|g| g.1.rows).max().unwrap();
    let cols = grids.iter().map(|g| g.1.cols).max().unwrap();
    let mut data = Vec::new();
    let mut done = false;
    for r in 0..rows {
        for c in 0..cols {
            let mut call = ops.to_vec();
            for (i, a) in &grids {
                let miss = (a.rows > 1 && r >= a.rows) || (a.cols > 1 && c >= a.cols);
                call[*i] = Operand::Val(if miss {
                    Value::Error(XlError::NA)
                } else {
                    a.get(
                        if a.rows > 1 { r } else { 0 },
                        if a.cols > 1 { c } else { 0 },
                    )
                    .clone()
                });
            }
            let res = match f(ctx, &call) {
                Operand::Val(v) => v,
                other => ctx.deref(other),
            };
            if first_ok {
                data.push(match res {
                    Value::Error(e) => Value::Error(e),
                    _ if done => Value::Error(XlError::Value),
                    Value::Array(a) => {
                        done = true;
                        a.data.first().cloned().unwrap_or(Value::Blank)
                    }
                    v => {
                        done = true;
                        v
                    }
                });
                continue;
            }
            data.push(match res {
                Value::Error(XlError::Calc) if (r, c) != (0, 0) => Value::Number(0.0),
                Value::Error(e) => Value::Error(e),
                Value::Array(a) => {
                    if (r, c) == (0, 0) {
                        a.data.first().cloned().unwrap_or(Value::Blank)
                    } else {
                        Value::Number(0.0)
                    }
                }
                v => {
                    if (r, c) == (0, 0) {
                        v
                    } else {
                        Value::Number(0.0)
                    }
                }
            });
        }
    }
    Some(Operand::Val(array_value(rows, cols, data)))
}

pub fn sortby(ctx: &mut EvalCtx, ops: &[Operand]) -> Operand {
    let orders: Vec<usize> = (2..ops.len()).step_by(2).collect();
    if let Some(r) = lift_flags(ctx, ops, &orders, sortby_one, false) {
        return r;
    }
    sortby_one(ctx, ops)
}

fn sortby_one(ctx: &mut EvalCtx, ops: &[Operand]) -> Operand {
    finish((|| {
        let src = Src::from_operand_3d(&ops[0])?;
        let a = src.to_array(ctx)?;

        if let (Src::Area(_), 1, Value::Error(e)) = (&src, a.len(), &a.data[0]) {
            return Err(*e);
        }
        let mut keys: Vec<(Arc<Array>, bool)> = Vec::new();
        let mut by_col: Option<bool> = None;

        let mut shape: Option<(u32, u32)> = None;
        let mut i = 1;
        while i < ops.len() {
            let by = Src::from_operand_3d(&ops[i])?.to_array(ctx)?;
            let desc = match ops.get(i + 1) {
                None | Some(Operand::Missing) => false,
                Some(Operand::Val(Value::Error(e))) => return Err(*e),
                Some(Operand::Val(Value::Array(_))) => return Err(XlError::Value),
                Some(Operand::Val(v)) => order_of(ctx.to_number(v)?)?,
                Some(op @ Operand::Ref(_)) => {
                    let v = Src::from_operand_3d(op)?.to_array(ctx)?;
                    if v.len() != 1 {
                        return Err(XlError::Value);
                    }
                    order_of(ctx.to_number(&v.data[0])?)?
                }
            };

            if let (Operand::Ref(_), 1, Value::Error(e)) = (&ops[i], by.len(), &by.data[0]) {
                return Err(*e);
            }
            if *shape.get_or_insert((by.rows, by.cols)) != (by.rows, by.cols) {
                return Err(XlError::Value);
            }

            let this_by_col = if by.rows == 1 && by.cols == 1 {
                None
            } else if by.cols == 1 {
                if by.rows != a.rows {
                    return Err(XlError::Value);
                }
                Some(false)
            } else if by.rows == 1 {
                if by.cols != a.cols {
                    return Err(XlError::Value);
                }
                Some(true)
            } else {
                return Err(XlError::Value);
            };
            if let Some(b) = this_by_col {
                if by_col.is_some_and(|x| x != b) {
                    return Err(XlError::Value);
                }
                by_col = Some(b);
            }
            keys.push((by, desc));
            i += 2;
        }

        if by_col.is_none() && a.rows > 1 && a.cols > 1 {
            return Err(XlError::Value);
        }
        let by_col = by_col.unwrap_or(false);
        if keys
            .iter()
            .any(|(by, _)| crate::eval::contains_circ(&Value::Array(by.clone())))
        {
            ctx.circ_used();
        }
        let n = if by_col { a.cols } else { a.rows };
        let mut order: Vec<u32> = (0..n).collect();
        order.sort_by(|&x, &y| {
            for (by, desc) in &keys {
                if by.len() == 1 {
                    continue;
                }
                let o = sort_cmp(&by.data[x as usize], &by.data[y as usize], *desc);
                if o != Ordering::Equal {
                    return o;
                }
            }
            Ordering::Equal
        });
        Ok(reorder(&a, by_col, &order))
    })())
}

#[derive(Hash, PartialEq, Eq)]
enum Key {
    Num(u64),
    Text(Vec<u32>),
    Bool(bool),
    Err(XlError),
    Blank,
    Other,
}

fn key_of(v: &Value) -> Key {
    match v {
        Value::Number(n) => Key::Num(if *n == 0.0 { 0 } else { n.to_bits() }),
        Value::Text(s) => Key::Text(collate::key(s)),
        Value::Bool(b) => Key::Bool(*b),
        Value::Error(e) => Key::Err(*e),
        Value::Blank => Key::Blank,
        _ => Key::Other,
    }
}

pub fn unique(ctx: &mut EvalCtx, ops: &[Operand]) -> Operand {
    if let Some(r) = lift_flags(ctx, ops, &[1, 2], unique_one, false) {
        return r;
    }

    let ops: Vec<Operand> = ops
        .iter()
        .enumerate()
        .map(|(i, o)| match o {
            Operand::Ref(_) if i > 0 => Operand::Val(ctx.deref(o.clone())),
            Operand::Val(Value::Array(a)) if i > 0 && a.data.len() == 1 => {
                Operand::Val(a.data[0].clone())
            }
            o => o.clone(),
        })
        .collect();
    unique_one(ctx, &ops)
}

fn unique_one(ctx: &mut EvalCtx, ops: &[Operand]) -> Operand {
    finish((|| {
        let src = Src::from_operand_3d(&ops[0])?;
        let by_col = opt_flag(ops.get(1))?;
        let once = opt_flag(ops.get(2))?;
        let a = src.to_array(ctx)?;
        let (n, w) = if by_col {
            (a.cols, a.rows)
        } else {
            (a.rows, a.cols)
        };
        let at = |line: u32, k: u32| {
            if by_col {
                a.get(k, line)
            } else {
                a.get(line, k)
            }
        };
        let mut first: Vec<u32> = Vec::new();
        let mut counts: HashMap<Vec<Key>, (u32, u32)> = HashMap::new();
        if ctx.has_placeholders() && crate::eval::contains_circ(&Value::Array(a.clone())) {
            let mut seen: Vec<(u32, u32)> = Vec::new();
            let mut touched = false;
            for line in 0..n {
                let mut hit = None;
                for (i, &(f, _)) in seen.iter().enumerate() {
                    let mut same = true;
                    for k in 0..w {
                        let (x, y) = (at(line, k), at(f, k));
                        if matches!(x, Value::Error(XlError::Circ))
                            || matches!(y, Value::Error(XlError::Circ))
                        {
                            touched = true;
                        }
                        if key_of(x) != key_of(y) {
                            same = false;
                            break;
                        }
                    }
                    if same {
                        hit = Some(i);
                        break;
                    }
                }
                match hit {
                    Some(i) => seen[i].1 += 1,
                    None => seen.push((line, 1)),
                }
            }
            if touched {
                ctx.circ_used();
            } else {
                ctx.circ_lifted = true;
            }
            let keep: Vec<u32> = seen
                .iter()
                .filter(|s| !once || s.1 == 1)
                .map(|s| s.0)
                .collect();
            if keep.is_empty() {
                return Err(XlError::Calc);
            }
            let m = keep.len() as u32;
            let (rows, cols) = if by_col { (a.rows, m) } else { (m, a.cols) };
            let mut data = Vec::with_capacity((rows * cols) as usize);
            for r in 0..rows {
                for c in 0..cols {
                    data.push(
                        if by_col {
                            a.get(r, keep[c as usize])
                        } else {
                            a.get(keep[r as usize], c)
                        }
                        .clone(),
                    );
                }
            }
            return Ok(array_value(rows, cols, data));
        }
        for line in 0..n {
            let key: Vec<Key> = (0..w).map(|k| key_of(at(line, k))).collect();
            let e = counts.entry(key).or_insert((line, 0));
            if e.1 == 0 {
                first.push(line);
            }
            e.1 += 1;
        }
        let keep: Vec<u32> = if once {
            first
                .into_iter()
                .filter(|&line| {
                    let key: Vec<Key> = (0..w).map(|k| key_of(at(line, k))).collect();
                    counts[&key].1 == 1
                })
                .collect()
        } else {
            first
        };
        if keep.is_empty() {
            return Err(XlError::Calc);
        }
        let m = keep.len() as u32;
        let (rows, cols) = if by_col { (a.rows, m) } else { (m, a.cols) };
        let mut data = Vec::with_capacity((rows * cols) as usize);
        for r in 0..rows {
            for c in 0..cols {
                data.push(
                    if by_col {
                        a.get(r, keep[c as usize])
                    } else {
                        a.get(keep[r as usize], c)
                    }
                    .clone(),
                );
            }
        }
        Ok(array_value(rows, cols, data))
    })())
}

fn take_range(n: u32, k: Option<f64>, take: bool) -> R<(u32, u32)> {
    let Some(k) = k else { return Ok((0, n - 1)) };
    let k = k.trunc();
    let nn = n as f64;
    if take {
        if k == 0.0 {
            return Err(XlError::Calc);
        }
        if k > 0.0 {
            Ok((0, (k.min(nn) as u32) - 1))
        } else {
            Ok((n - (-k).min(nn) as u32, n - 1))
        }
    } else {
        if k.abs() >= nn {
            return Err(XlError::Calc);
        }
        if k >= 0.0 {
            Ok((k as u32, n - 1))
        } else {
            Ok((0, n - 1 - (-k) as u32))
        }
    }
}

pub fn take_drop(ctx: &mut EvalCtx, ops: &[Operand], take: bool) -> Operand {
    if matches!(ops[0], Operand::Missing) {
        return Operand::err(XlError::Value);
    }

    let grid = |ctx: &mut EvalCtx, op: Option<&Operand>| -> Option<Arc<Array>> {
        match op {
            Some(Operand::Val(Value::Array(a))) => Some(a.clone()),
            Some(o @ Operand::Ref(_)) => {
                let a = Src::from_operand(o).ok()?.to_array(ctx).ok()?;
                (a.data.len() > 1).then_some(a)
            }
            _ => None,
        }
    };
    let (g1, g2) = (grid(ctx, ops.get(1)), grid(ctx, ops.get(2)));
    if g1.is_some() || g2.is_some() {
        let (r1, c1) = g1.as_ref().map_or((1, 1), |a| (a.rows, a.cols));
        let (r2, c2) = g2.as_ref().map_or((1, 1), |a| (a.rows, a.cols));
        let (rows, cols) = (r1.max(r2), c1.max(c2));
        let range_src = matches!(ops[0], Operand::Ref(_));

        let area_src = matches!(ops[0], Operand::Ref(crate::eval::Reference::Area(_)));
        let mut items = Vec::new();
        let mut data = Vec::new();
        for r in 0..rows {
            for c in 0..cols {
                let pick = |g: &Option<Arc<Array>>, op: Option<&Operand>| -> Operand {
                    match g {
                        Some(a) => {
                            let miss = (a.rows > 1 && r >= a.rows) || (a.cols > 1 && c >= a.cols);
                            Operand::Val(if miss {
                                Value::Error(XlError::NA)
                            } else {
                                a.get(
                                    if a.rows > 1 { r } else { 0 },
                                    if a.cols > 1 { c } else { 0 },
                                )
                                .clone()
                            })
                        }
                        None => op.cloned().unwrap_or(Operand::Missing),
                    }
                };
                let call = [ops[0].clone(), pick(&g1, ops.get(1)), pick(&g2, ops.get(2))];
                let res = take_drop(ctx, &call, take);
                if area_src {
                    items.push(match res {
                        Operand::Ref(crate::eval::Reference::Area(a)) => Ok(a),
                        Operand::Val(Value::Error(e)) => Err(e),
                        _ => Err(XlError::Value),
                    });
                    continue;
                }
                let v = match res {
                    Operand::Val(Value::Array(a)) => {
                        a.data.first().cloned().unwrap_or(Value::Blank)
                    }
                    Operand::Val(v) => v,

                    other if range_src && !matches!(&other, Operand::Val(Value::Error(_))) => {
                        Value::Error(XlError::Value)
                    }
                    other => ctx.deref(other),
                };
                data.push(v);
            }
        }
        if area_src {
            return Operand::Ref(crate::eval::Reference::Grid(Arc::new(
                crate::eval::RefGrid::new(rows, cols, items),
            )));
        }
        return Operand::Val(array_value(rows, cols, data));
    }
    let r = (|| -> R<Operand> {
        let src = Src::from_operand(&ops[0])?;
        let mut scalar = |op: Option<&Operand>| -> Option<Operand> {
            op.map(|o| match o {
                Operand::Ref(_) => Operand::Val(ctx.deref(o.clone())),
                Operand::Val(Value::Array(a)) if a.data.len() == 1 => {
                    Operand::Val(a.data[0].clone())
                }
                o => o.clone(),
            })
        };
        let (rows_op, cols_op) = (scalar(ops.get(1)), scalar(ops.get(2)));
        let rows = opt_number(ctx, rows_op.as_ref())?;
        let cols = opt_number(ctx, cols_op.as_ref())?;
        let (r0, r1) = take_range(src.rows(), rows, take)?;
        let (c0, c1) = take_range(src.cols(), cols, take)?;
        Ok(match src.slice(r0, r1, c0, c1) {
            Operand::Val(v) if !matches!(v, Value::Array(_)) => {
                Operand::Val(Array::new(1, 1, vec![v]).into_value())
            }
            o => o,
        })
    })();
    r.unwrap_or_else(Operand::err)
}

pub fn choose_lines(ctx: &mut EvalCtx, ops: &[Operand], rows: bool) -> Operand {
    finish((|| {
        let src = Src::from_operand(&ops[0])?;
        let n = if rows { src.rows() } else { src.cols() };

        enum Idx {
            One(f64),
            Many(Arc<Array>),
        }
        let mut args = Vec::with_capacity(ops.len() - 1);
        for op in &ops[1..] {
            args.push(match op {
                Operand::Missing => return Err(XlError::Value),
                Operand::Val(Value::Array(a)) => Idx::Many(a.clone()),
                Operand::Val(v) => Idx::One(ctx.to_number(v)?),
                Operand::Ref(r) if r.single().is_some_and(|a| a.rect.is_cell()) => {
                    let v = ctx.deref(op.clone());
                    Idx::One(ctx.to_number(&v)?)
                }
                Operand::Ref(_) => Idx::Many(Src::from_operand_3d(op)?.to_array(ctx)?),
            });
        }
        let pick = |k: f64| -> R<u32> {
            let k = k.trunc();
            if k == 0.0 || k.abs() > n as f64 {
                return Err(XlError::Value);
            }
            Ok(if k > 0.0 {
                k as u32 - 1
            } else {
                n - (-k) as u32
            })
        };
        let mut idx = Vec::new();
        for a in &args {
            match a {
                Idx::One(k) => idx.push(pick(*k)?),
                Idx::Many(a) => {
                    if a.rows > 1 && a.cols > 1 {
                        return Err(XlError::Value);
                    }
                    for v in &a.data {
                        match v {
                            Value::Number(k) => idx.push(pick(*k)?),
                            Value::Error(e) => return Err(*e),
                            _ => return Err(XlError::Value),
                        }
                    }
                }
            }
        }
        let a = src.to_array(ctx)?;
        let m = idx.len() as u32;
        let (out_r, out_c) = if rows { (m, a.cols) } else { (a.rows, m) };
        check_result_size(ctx, out_r as u64, out_c as u64)?;
        let mut data = Vec::with_capacity((out_r * out_c) as usize);
        for r in 0..out_r {
            for c in 0..out_c {
                data.push(
                    if rows {
                        a.get(idx[r as usize], c)
                    } else {
                        a.get(r, idx[c as usize])
                    }
                    .clone(),
                );
            }
        }
        Ok(array_value(out_r, out_c, data))
    })())
}

fn pad_value(ctx: &mut EvalCtx, op: Option<&Operand>, omitted: Value) -> R<Value> {
    Ok(match op {
        None => Value::Error(XlError::NA),
        Some(Operand::Missing) => omitted,
        Some(Operand::Val(Value::Array(_))) => return Err(XlError::Value),
        Some(Operand::Val(v)) => v.clone(),

        Some(Operand::Ref(r)) if r.is_span() && r.single().is_none() => Value::Error(XlError::Ref),
        Some(Operand::Ref(r)) if r.single().is_none() => Value::Error(XlError::Value),
        Some(op @ Operand::Ref(_)) => {
            let a = Src::from_operand_3d(op)?.to_array(ctx)?;
            if a.len() != 1 {
                return Err(XlError::Value);
            }
            a.data[0].clone()
        }
    })
}

pub fn expand(ctx: &mut EvalCtx, ops: &[Operand]) -> Operand {
    finish((|| {
        let src = Src::from_operand_3d(&ops[0])?;
        let (sr, sc) = (src.rows(), src.cols());
        let dim = |ctx: &EvalCtx, i: usize, cur: u32| -> R<u64> {
            match opt_number(ctx, ops.get(i))? {
                None => Ok(cur as u64),
                Some(n) => {
                    let n = n.trunc();
                    if n < cur as f64 {
                        Err(XlError::Value)
                    } else if n >= 2147483648.0 {
                        Err(XlError::Value)
                    } else {
                        Ok(n as u64)
                    }
                }
            }
        };

        if let Operand::Ref(r) = &ops[0] {
            if let Some(a) = r.single().filter(|a| a.rect.is_cell()) {
                if let Value::Error(e) = ctx.area_array(a) {
                    return Err(e);
                }
            }
        }

        let (nr, nc) = (opt_number(ctx, ops.get(1)), opt_number(ctx, ops.get(2)));
        if let (Err(e), _) | (_, Err(e)) = (&nr, &nc) {
            return Err(*e);
        }
        let rows = dim(ctx, 1, sr)?;
        let cols = dim(ctx, 2, sc)?;

        if rows > MAX_ROWS as u64 || cols > MAX_ROWS as u64 {
            return Err(XlError::Num);
        }
        check_result_size(ctx, rows, cols)?;
        let pad = pad_value(ctx, ops.get(3), Value::Blank)?;
        let a = src.to_array(ctx)?;

        if a.rows == 1 && a.cols == 1 && matches!(ops[0], Operand::Ref(_)) {
            if let Value::Error(e) = a.get(0, 0) {
                return Err(*e);
            }
        }
        let (rows, cols) = (rows as u32, cols as u32);
        let mut data = Vec::with_capacity((rows * cols) as usize);
        for r in 0..rows {
            for c in 0..cols {
                data.push(if r < a.rows && c < a.cols {
                    a.get(r, c).clone()
                } else {
                    pad.clone()
                });
            }
        }
        Ok(array_value(rows, cols, data))
    })())
}

pub fn to_line(ctx: &mut EvalCtx, ops: &[Operand], col: bool) -> Operand {
    if let Operand::Ref(r @ (crate::eval::Reference::Multi(_) | crate::eval::Reference::Span(_))) =
        &ops[0]
    {
        if r.areas().len() > 1 {
            let mut parts = Vec::new();
            for a in r.areas() {
                let mut o = ops.to_vec();
                o[0] = Operand::Ref(crate::eval::Reference::Area(*a));
                parts.push(to_line(ctx, &o, col));
            }
            return crate::functions::dynamic::stack(ctx, &parts, col);
        }
    }

    if let Operand::Ref(crate::eval::Reference::Grid(g)) = &ops[0] {
        let g = g.clone();
        let mut o = ops.to_vec();

        let marked: Vec<Value> = g
            .items
            .iter()
            .enumerate()
            .map(|(k, i)| match i {
                Ok(_) => Value::Number(k as f64),
                Err(e) => Value::Error(*e),
            })
            .collect();
        o[0] = Operand::Val(Array::new(g.rows, g.cols, marked).into_value());
        let back = |v: &Value| match v {
            Value::Number(k) => g.items[*k as usize],
            Value::Error(e) => Err(*e),
            _ => Err(XlError::Value),
        };
        let grid = |rows: u32, cols: u32, items| {
            Operand::Ref(crate::eval::Reference::Grid(Arc::new(
                crate::eval::RefGrid::new(rows, cols, items),
            )))
        };
        return match to_line(ctx, &o, col) {
            Operand::Val(Value::Array(a)) => {
                grid(a.rows, a.cols, a.data.iter().map(back).collect())
            }
            Operand::Val(v @ Value::Number(_)) => grid(1, 1, vec![back(&v)]),
            r => r,
        };
    }
    finish((|| {
        arg_error_typed(ctx, &ops[1..], "nb")?;
        let src = Src::from_operand(&ops[0])?;
        let ignore = match opt_number(ctx, ops.get(1))? {
            None => 0,
            Some(n) => {
                let n = n.trunc();
                if !(0.0..=3.0).contains(&n) {
                    return Err(XlError::Value);
                }
                n as u32
            }
        };
        let by_col = opt_flag(ops.get(2))?;

        let a = src.to_array(ctx)?;
        let mut data = Vec::new();
        let mut visit = |v: &Value| {
            let blank = v.is_blank();
            let skip = (ignore & 1 != 0 && blank) || (ignore & 2 != 0 && v.is_error());
            if !skip {
                data.push(v.clone());
            }
        };
        if by_col {
            for c in 0..a.cols {
                for r in 0..a.rows {
                    visit(a.get(r, c));
                }
            }
        } else {
            a.data.iter().for_each(&mut visit);
        }
        if data.is_empty() {
            return Err(XlError::Calc);
        }
        let n = data.len() as u32;
        Ok(if col {
            array_value(n, 1, data)
        } else {
            array_value(1, n, data)
        })
    })())
}

pub fn wrap(ctx: &mut EvalCtx, ops: &[Operand], rows: bool) -> Operand {
    finish((|| {
        if let Operand::Ref(r) = &ops[0] {
            if let Some(a) = r.single().filter(|a| a.rect.is_cell()) {
                match ctx.area_array(a) {
                    Value::Blank => return Err(XlError::Value),
                    Value::Error(e) => return Err(e),
                    _ => {}
                }
            }
        }
        arg_error_typed(ctx, &ops[1..2], "n")?;
        let src = Src::from_operand_3d(&ops[0])?;
        if src.rows() > 1 && src.cols() > 1 {
            return Err(XlError::Value);
        }
        let k = match ops.get(1) {
            Some(Operand::Val(v)) => ctx.to_number(v)?.trunc(),
            _ => 0.0,
        };
        if k < 1.0 || k >= 2147483648.0 {
            return Err(XlError::Num);
        }
        let pad = pad_value(ctx, ops.get(2), Value::Error(XlError::NA))?;
        let a = src.to_array(ctx)?;
        let n = a.len() as u64;
        let width = (k as u64).min(n);
        let lines = n.div_ceil(width);
        let (out_r, out_c) = if rows { (lines, width) } else { (width, lines) };
        check_result_size(ctx, out_r, out_c)?;
        let (out_r, out_c) = (out_r as u32, out_c as u32);
        let mut data = vec![pad; (out_r * out_c) as usize];
        for (i, v) in a.data.iter().enumerate() {
            let (line, pos) = (i as u32 / width as u32, i as u32 % width as u32);
            let (r, c) = if rows { (line, pos) } else { (pos, line) };
            data[(r * out_c + c) as usize] = v.clone();
        }
        Ok(array_value(out_r, out_c, data))
    })())
}
