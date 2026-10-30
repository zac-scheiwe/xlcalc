use std::cmp::Ordering;

use super::args::*;
use super::{args, lazy, volatile, FnDef, MANY};
use crate::address::{col_to_letters, parse_a1_rect, Rect, MAX_COLS, MAX_ROWS};
use crate::eval::call::call_lifted;
use crate::eval::ops::compare;
use crate::eval::{Area, EvalCtx, Operand, Reference};
use crate::formula::Node;
use crate::text::Wildcard;
use crate::value::{Array, Value, XlError};

pub const DEFS: &[FnDef] = &[
    args("ROW", 0, 1, "Q", |c, o| row_col(c, o, true)),
    args("COLUMN", 0, 1, "Q", |c, o| row_col(c, o, false)),
    args("ROWS", 1, 1, "R", |c, o| rows_cols(c, o, true)),
    args("COLUMNS", 1, 1, "R", |c, o| rows_cols(c, o, false)),
    args("AREAS", 1, 1, "Q", |_, o| match &o[0] {
        Operand::Ref(r) => Operand::Val(Value::Number(r.areas().len() as f64)),
        Operand::Val(Value::Error(e)) => Operand::err(*e),
        _ => Operand::err(XlError::VALUE),
    }),
    args("INDEX", 2, 4, "RVVV", index),
    args("MATCH", 2, 3, "RRR", match_outer),
    args("VLOOKUP", 3, 4, "RRRR", |c, o| vhlookup_outer(c, o, true)),
    args("HLOOKUP", 3, 4, "RRRR", |c, o| vhlookup_outer(c, o, false)),
    lazy("CHOOSE", 2, MANY, choose),
    volatile(args("OFFSET", 3, 5, "QRRRR", offset)),
    volatile(args("INDIRECT", 1, 2, "RR", indirect)),
    args("ADDRESS", 2, 5, "VVVVV", address),
    args("TRANSPOSE", 1, 1, "R", transpose),
];

fn row_col(ctx: &mut EvalCtx, ops: &[Operand], row: bool) -> Operand {
    let rect = match ops.first() {
        None | Some(Operand::Missing) => ctx
            .wb
            .host_block(ctx.host)
            .unwrap_or(Rect::cell(ctx.host.row, ctx.host.col)),
        Some(Operand::Ref(r)) => match r.single() {
            Some(a) => a.rect,
            None => return Operand::err(XlError::REF),
        },
        Some(Operand::Val(Value::Error(e))) => return Operand::err(*e),
        Some(_) => return Operand::err(XlError::VALUE),
    };
    let (start, n) = if row {
        (rect.r0, rect.rows())
    } else {
        (rect.c0, rect.cols())
    };

    if n == 1 && (matches!(ctx.arr_ctx, 0 | 3) || crate::legacy::host_may_array(ctx.wb, ctx.host)) {
        return Operand::Val(Value::Number(start as f64 + 1.0));
    }
    let data: Vec<Value> = (0..n)
        .map(|i| Value::Number((start + i) as f64 + 1.0))
        .collect();
    Operand::Val(
        if row {
            Array::column(data)
        } else {
            Array::row(data)
        }
        .into_value(),
    )
}

fn rows_cols(_ctx: &mut EvalCtx, ops: &[Operand], rows: bool) -> Operand {
    let (r, c) = match &ops[0] {
        Operand::Ref(Reference::Grid(g)) => (g.rows, g.cols),
        Operand::Ref(r) => match r.single() {
            Some(a) => (a.rect.rows(), a.rect.cols()),
            None => return Operand::err(XlError::REF),
        },
        Operand::Val(Value::Array(a)) => (a.rows, a.cols),
        Operand::Val(Value::Error(e)) => return Operand::err(*e),
        _ => (1, 1),
    };
    Operand::Val(Value::Number(if rows { r } else { c } as f64))
}

fn index(ctx: &mut EvalCtx, ops: &[Operand]) -> Operand {
    if let Some(Operand::Val(Value::Error(e))) = ops.first() {
        return Operand::err(*e);
    }
    let num_arg = |ctx: &EvalCtx, o: Option<&Operand>| -> R<Option<f64>> {
        match o {
            None | Some(Operand::Missing) => Ok(None),
            Some(Operand::Val(v)) => ctx
                .to_number(v)
                .map(|n| Some(if n < 0.0 { ffuzz(n) } else { ifuzz(n) })),
            Some(Operand::Ref(_)) => Err(XlError::VALUE),
        }
    };
    let (row, col, area_n) = match (|| {
        Ok((
            num_arg(ctx, ops.get(1))?,
            num_arg(ctx, ops.get(2))?,
            num_arg(ctx, ops.get(3))?,
        ))
    })() {
        Ok(t) => t,
        Err(e) => return Operand::err(e),
    };
    let explicit_col = ops.len() > 2;

    if (ops.len() > 3 && area_n.is_none())
        || area_n.is_some_and(|k| k < 1.0)
        || row.is_some_and(|x| x < 0.0)
        || col.is_some_and(|x| x < 0.0)
    {
        return Operand::err(XlError::VALUE);
    }
    match &ops[0] {
        Operand::Ref(r) if r.is_span() => Operand::err(XlError::VALUE),

        Operand::Ref(Reference::Grid(g)) => match pick(g.rows, g.cols, row, col, explicit_col) {
            Ok((r0, r1, c0, c1)) if r0 == r1 && c0 == c1 && (explicit_col || g.rows == 1) => {
                g.broadcast_get(r0, c0)
            }
            Ok((r0, r1, c0, c1)) => {
                let items = (r0..=r1)
                    .flat_map(|r| (c0..=c1).map(move |c| (r, c)))
                    .map(|(r, c)| g.items[(r * g.cols + c) as usize])
                    .collect();
                Operand::Ref(Reference::Grid(std::sync::Arc::new(
                    crate::eval::RefGrid::new(r1 - r0 + 1, c1 - c0 + 1, items),
                )))
            }
            Err(e) => Operand::err(e),
        },
        Operand::Ref(r) => {
            let k = area_n.unwrap_or(1.0);
            if k as usize > r.areas().len() {
                return Operand::err(XlError::REF);
            }
            let a = r.areas()[k as usize - 1];

            if !explicit_col && a.rect.rows() > 1 && a.rect.cols() > 1 {
                return Operand::err(XlError::REF);
            }
            match pick(a.rect.rows(), a.rect.cols(), row, col, explicit_col) {
                Ok((r0, r1, c0, c1)) => Operand::Ref(Reference::Area(Area {
                    sheet: a.sheet,
                    rect: Rect::new(
                        a.rect.r0 + r0,
                        a.rect.c0 + c0,
                        a.rect.r0 + r1,
                        a.rect.c0 + c1,
                    ),
                })),
                Err(e) => Operand::err(e),
            }
        }
        Operand::Val(v) => {
            let arr = match v {
                Value::Array(a) => a.clone(),
                Value::Error(e) => return Operand::err(*e),
                other => std::sync::Arc::new(Array::new(1, 1, vec![other.clone()])),
            };
            if area_n.is_some_and(|k| k > 1.0) {
                return Operand::err(XlError::REF);
            }
            match pick(arr.rows, arr.cols, row, col, explicit_col) {
                Ok((r0, r1, c0, c1)) => {
                    let slice = if explicit_col {
                        row.is_none_or(|x| x == 0.0) || col.is_none_or(|x| x == 0.0)
                    } else {
                        arr.rows > 1 || row.is_none_or(|x| x == 0.0)
                    };
                    let n = (r1 - r0 + 1, c1 - c0 + 1);
                    let get = |r: u32, c: u32| arr.get(r0 + r, c0 + c).clone();
                    Operand::Val(if slice {
                        array_always(n.0, n.1, get)
                    } else {
                        array_or_scalar(n.0, n.1, get)
                    })
                }
                Err(e) => Operand::err(e),
            }
        }
        Operand::Missing => Operand::err(XlError::VALUE),
    }
}

fn pick(
    rows: u32,
    cols: u32,
    row: Option<f64>,
    col: Option<f64>,
    explicit_col: bool,
) -> R<(u32, u32, u32, u32)> {
    let mut row = row.unwrap_or(0.0);
    let mut col = col.unwrap_or(0.0);
    if row < 0.0 || col < 0.0 {
        return Err(XlError::VALUE);
    }

    if rows == 1 && !explicit_col && cols > 1 {
        col = row;
        row = 0.0;
    }
    if row > rows as f64 || col > cols as f64 {
        return Err(XlError::REF);
    }
    let (r0, r1) = if row == 0.0 {
        (0, rows - 1)
    } else {
        (row as u32 - 1, row as u32 - 1)
    };
    let (c0, c1) = if col == 0.0 {
        (0, cols - 1)
    } else {
        (col as u32 - 1, col as u32 - 1)
    };
    Ok((r0, r1, c0, c1))
}

struct Line<'g> {
    grid: &'g Grid,
    vertical: bool,
    fixed: u32,
    len: u32,
}

impl Line<'_> {
    fn get(&self, ctx: &mut EvalCtx, i: u32) -> Value {
        if self.vertical {
            self.grid.get(ctx, i, self.fixed)
        } else {
            self.grid.get(ctx, self.fixed, i)
        }
    }
}

fn same_kind(a: &Value, b: &Value) -> bool {
    matches!(
        (a, b),
        (Value::Number(_), Value::Number(_))
            | (Value::Text(_), Value::Text(_))
            | (Value::Bool(_), Value::Bool(_))
    )
}

fn line_values(ctx: &mut EvalCtx, line: &Line) -> Vec<Value> {
    match line.grid {
        Grid::Area(a) => {
            let rect = if line.vertical {
                Rect::new(
                    a.rect.r0,
                    a.rect.c0 + line.fixed,
                    a.rect.r0 + line.len - 1,
                    a.rect.c0 + line.fixed,
                )
            } else {
                Rect::new(
                    a.rect.r0 + line.fixed,
                    a.rect.c0,
                    a.rect.r0 + line.fixed,
                    a.rect.c0 + line.len - 1,
                )
            };
            let used = ctx.wb.sheet(a.sheet).and_then(|s| s.bounded(&rect));
            let Some(used) = used else { return Vec::new() };

            let rect = Rect::new(rect.r0, rect.c0, used.r1, used.c1);
            let mut out = vec![
                Value::Blank;
                if line.vertical {
                    rect.rows()
                } else {
                    rect.cols()
                } as usize
            ];
            let area = Area {
                sheet: a.sheet,
                rect,
            };
            ctx.visit_area(&area, |r, c, v| {
                out[if line.vertical { r } else { c } as usize] = v.clone()
            });
            out
        }
        _ => (0..line.len).map(|i| line.get(ctx, i)).collect(),
    }
}

fn exact_find(ctx: &mut EvalCtx, line: &Line, target: &Value, wildcards: bool) -> Option<u32> {
    let pattern = match target {
        Value::Text(s) if wildcards && Wildcard::has_wildcards(s) => Some(Wildcard::new(s)),
        _ => None,
    };
    let is_hit = |v: &Value| match (&pattern, v) {
        (Some(p), Value::Text(s)) => p.matches(s),
        (Some(_), _) => false,
        (None, Value::Text(s)) => {
            matches!(target, Value::Text(t) if crate::text::eq_ordinal_ci(t, s))
        }

        (None, Value::Number(n)) => matches!(target, Value::Number(t) if t == n),
        _ => same_kind(target, v) && compare(target, v) == Ordering::Equal,
    };

    if let Grid::Area(a) = line.grid {
        let rect = if line.vertical {
            Rect::new(
                a.rect.r0,
                a.rect.c0 + line.fixed,
                a.rect.r0 + line.len - 1,
                a.rect.c0 + line.fixed,
            )
        } else {
            Rect::new(
                a.rect.r0 + line.fixed,
                a.rect.c0,
                a.rect.r0 + line.fixed,
                a.rect.c0 + line.len - 1,
            )
        };
        let mut found = None;
        let area = Area {
            sheet: a.sheet,
            rect,
        };
        ctx.visit_area_until(&area, |r, c, v| {
            if is_hit(v) {
                found = Some(if line.vertical { r } else { c });
                return false;
            }
            true
        });
        return if ctx.interrupted() { None } else { found };
    }
    for i in 0..line.len {
        let v = line.get(ctx, i);
        if matches!(v, Value::Error(XlError::CIRC)) {
            ctx.circ_used();
            return None;
        }
        if is_hit(&v) {
            return Some(i);
        }
    }
    None
}

fn approx_cmp(a: &Value, b: &Value) -> Ordering {
    match (a, b) {
        (Value::Number(x), Value::Number(y)) => x.partial_cmp(y).unwrap_or(Ordering::Equal),

        (Value::Text(x), Value::Text(y)) => {
            crate::text::compare_ci(crate::text::cut255(x), crate::text::cut255(y))
        }
        (Value::Bool(x), Value::Bool(y)) => x.cmp(y),
        (Value::Error(x), Value::Error(y)) => x.cmp(y),
        _ => Ordering::Equal,
    }
}

fn approx_find(ctx: &mut EvalCtx, line: &Line, target: &Value, descending: bool) -> Option<u32> {
    if line.len == 0 {
        return None;
    }
    let values = line_values(ctx, line);
    if descending {
        let mut best = None;
        for (i, v) in values.iter().enumerate() {
            if !same_kind(target, v) {
                continue;
            }
            match approx_cmp(v, target) {
                Ordering::Equal => return Some(i as u32),
                Ordering::Greater => best = Some(i as u32),
                Ordering::Less => break,
            }
        }
        return best;
    }
    let mat = values.len() as i64;
    let (mut lo, mut hi) = (0i64, line.len as i64 - 1);
    let mut best: Option<u32> = None;
    while lo <= hi {
        let mid = (lo + hi) / 2;
        let mut i = mid;
        while i <= hi && i < mat && !same_kind(target, &values[i as usize]) {
            i += 1;
        }
        if i > hi || i >= mat {
            hi = mid - 1;
            continue;
        }
        let ord = approx_cmp(&values[i as usize], target);
        match ord {
            Ordering::Equal => {
                let mut j = i;
                while j + 1 < mat
                    && same_kind(target, &values[j as usize + 1])
                    && approx_cmp(&values[j as usize + 1], target) == Ordering::Equal
                {
                    j += 1;
                }
                return Some(j as u32);
            }
            Ordering::Less => {
                best = Some(i as u32);
                lo = i + 1;
            }
            Ordering::Greater => hi = i - 1,
        }
    }
    best
}

const LIFT_MATCH: FnDef = args("MATCH", 2, 3, "VRV", |_, _| Operand::Missing);
const LIFT_VHLOOKUP: FnDef = args("VLOOKUP", 3, 4, "VRRV", |_, _| Operand::Missing);

fn match_outer(ctx: &mut EvalCtx, ops: &[Operand]) -> Operand {
    let mut ops = ops.to_vec();

    if text_table(ctx, &ops[1]) || matches!(&ops[0], Operand::Val(Value::Error(_))) {
        let shape = match ops.get(2) {
            Some(Operand::Val(Value::Array(a))) => Some((a.rows, a.cols)),
            _ => None,
        };
        if ops.len() > 2 {
            ops[2] = Operand::Missing;
        }
        let r = call_lifted(ctx, &LIFT_MATCH, ops, &mut |ctx, o| match_scalar(ctx, o));
        return match (shape, r) {
            (Some((rows, cols)), Operand::Val(v)) if !matches!(v, Value::Array(_)) => {
                Operand::Val(Array::filled(rows, cols, v).into_value())
            }
            (_, r) => r,
        };
    }

    if let Some(mt) = ops.get(2).cloned() {
        if let Operand::Ref(_) = mt {
            if is_array_arg(&mt) {
                let first = match &mt {
                    Operand::Ref(r) if r.single().is_some() && is_array_arg(&ops[0]) => {
                        match ctx.deref(mt.clone()) {
                            Value::Array(a) => Some(a.data[0].clone()),
                            v => Some(v),
                        }
                    }
                    _ => None,
                };
                if let Some(v) = first {
                    ops[2] = Operand::Val(v);
                } else {
                    match super::super::eval::refs::implicit_intersection(ctx, mt) {
                        Operand::Ref(r) => ops[2] = Operand::Val(ctx.deref(Operand::Ref(r))),
                        _ => return Operand::err(XlError::REF),
                    }
                }
            } else {
                match ctx.deref(mt.clone()) {
                    Value::Error(_) => return Operand::err(XlError::REF),
                    v @ Value::Text(_) if ctx.to_number(&v).is_err() => {
                        return Operand::err(XlError::REF)
                    }
                    _ => {}
                }
            }
        }
    }
    call_lifted(ctx, &LIFT_MATCH, ops, &mut |ctx, o| match_scalar(ctx, o))
}

fn text_table(ctx: &EvalCtx, op: &Operand) -> bool {
    matches!(op, Operand::Val(v @ Value::Text(_)) if ctx.to_number(v).is_err())
}

fn index_elem(ctx: &EvalCtx, v: &Value) -> Value {
    match v {
        Value::Blank => Value::Number(0.0),
        v if v.is_cell_empty() => Value::Number(0.0),
        v => Value::Number(ctx.to_number(v).map_or(f64::INFINITY, f64::trunc)),
    }
}

fn index_array_error(a: &Array, width: u32) -> Option<XlError> {
    a.data.iter().find_map(|v| match v {
        Value::Number(n) if sfix(*n) == i32::MIN as i64 => Some(XlError::REF),
        Value::Number(n) if sfix(*n) < 1 => Some(XlError::VALUE),
        Value::Number(n) if sfix(*n) > width as i64 => Some(XlError::REF),
        _ => None,
    })
}

fn is_array_arg(op: &Operand) -> bool {
    match op {
        Operand::Val(Value::Array(_)) => true,
        Operand::Ref(r) => r.single().is_none_or(|a| !a.rect.is_cell()),
        _ => false,
    }
}

fn vhlookup_outer(ctx: &mut EvalCtx, ops: &[Operand], vertical: bool) -> Operand {
    let mut ops = ops.to_vec();

    let shaped = is_array_arg(&ops[0]) || ops.get(3).is_some_and(is_array_arg);
    let idx = std::mem::replace(&mut ops[2], Operand::Missing);
    ops[2] = match idx {
        Operand::Missing => Operand::Missing,

        op @ Operand::Ref(_) if matches!(&op, Operand::Ref(r) if r.single().is_none()) => op,
        op => {
            let from_ref = matches!(op, Operand::Ref(_));
            match ctx.deref(op) {
                Value::Array(a) => {
                    let conv: Vec<Value> = a.data.iter().map(|v| index_elem(ctx, v)).collect();
                    if shaped || conv.len() == 1 {
                        Operand::Val(conv[0].clone())
                    } else {
                        Operand::Val(Array::new(a.rows, a.cols, conv).into_value())
                    }
                }
                v if from_ref => Operand::Val(index_elem(ctx, &v)),
                v => Operand::Val(v),
            }
        }
    };
    call_lifted(ctx, &LIFT_VHLOOKUP, ops, &mut |ctx, o| {
        vhlookup_scalar(ctx, o, vertical)
    })
}

fn match_scalar(ctx: &mut EvalCtx, ops: &[Operand]) -> Operand {
    let target = match &ops[0] {
        Operand::Val(Value::Error(e)) => return Operand::err(*e),

        Operand::Val(Value::Text(t)) if crate::text::utf16_len(t) > 255 => {
            return Operand::err(XlError::VALUE)
        }
        Operand::Val(Value::Blank) | Operand::Missing => Value::Number(0.0),
        Operand::Val(v) => v.clone(),
        _ => Value::Number(0.0),
    };
    if let Operand::Val(Value::Error(e)) = &ops[1] {
        return Operand::err(*e);
    }
    let mode = match ops.get(2) {
        None => 1.0,

        Some(Operand::Missing) => 0.0,
        Some(Operand::Val(v)) => match ctx.to_number(v) {
            Ok(n) => {
                if n > 0.0 {
                    1.0
                } else if n < 0.0 {
                    -1.0
                } else {
                    0.0
                }
            }
            Err(e) => return Operand::err(e),
        },
        Some(_) => return Operand::err(XlError::VALUE),
    };
    if let Some(e) = scalar_table_error(ctx, &ops[1]) {
        return Operand::err(e);
    }
    let grid = match &ops[1] {
        Operand::Val(Value::Error(e)) => return Operand::err(*e),
        op => match Grid::from_operand(op) {
            Ok(g) => g,
            Err(e) => return Operand::err(e),
        },
    };
    if grid.rows() > 1 && grid.cols() > 1 {
        return Operand::err(XlError::NA);
    }
    let vertical = grid.cols() == 1;
    let len = if vertical { grid.rows() } else { grid.cols() };
    let line = Line {
        grid: &grid,
        vertical,
        fixed: 0,
        len,
    };
    let pos = if mode == 0.0 {
        exact_find(ctx, &line, &target, true)
    } else {
        if let Grid::Array(a) = &grid {
            if crate::eval::contains_circ(&Value::Array(a.clone())) {
                ctx.circ_used();
            }
        }
        approx_find(ctx, &line, &target, mode < 0.0)
    };
    match pos {
        Some(i) => Operand::Val(Value::Number(i as f64 + 1.0)),
        None => Operand::err(XlError::NA),
    }
}

fn vhlookup_scalar(ctx: &mut EvalCtx, ops: &[Operand], vertical: bool) -> Operand {
    let target = match &ops[0] {
        Operand::Val(Value::Error(e)) => return Operand::err(*e),

        Operand::Val(Value::Text(t)) if crate::text::utf16_len(t) > 255 => {
            return Operand::err(XlError::VALUE)
        }
        Operand::Val(Value::Blank) | Operand::Missing => Value::Number(0.0),
        Operand::Val(v) => v.clone(),
        _ => Value::Number(0.0),
    };
    if let Operand::Val(Value::Error(e)) = &ops[1] {
        return Operand::err(*e);
    }
    if text_table(ctx, &ops[1]) {
        return Operand::err(XlError::VALUE);
    }

    let idx = match &ops[2] {
        Operand::Val(Value::Array(a)) => Err(a.clone()),

        Operand::Val(Value::Lambda(_)) => Ok(f64::INFINITY),
        Operand::Val(v) => match ctx.to_number(v) {
            Ok(n) => Ok(sfix(n) as f64),
            Err(e) => return Operand::err(e),
        },
        Operand::Ref(_) => Ok(f64::INFINITY),
        _ => Ok(f64::NAN),
    };
    let approx = match ops.get(3) {
        None => true,
        Some(Operand::Missing) => false,
        Some(Operand::Val(v)) => match boolean(v) {
            Ok(b) => b,
            Err(e) => return Operand::err(e),
        },
        Some(_) => return Operand::err(XlError::VALUE),
    };
    if let Some(e) = scalar_table_error(ctx, &ops[1]) {
        return Operand::err(e);
    }
    let grid = match Grid::from_operand(&ops[1]) {
        Ok(g) => g,
        Err(e) => return Operand::err(e),
    };
    let (len, width) = if vertical {
        (grid.rows(), grid.cols())
    } else {
        (grid.cols(), grid.rows())
    };
    let line = Line {
        grid: &grid,
        vertical,
        fixed: 0,
        len,
    };
    let pos = if approx {
        approx_find(ctx, &line, &target, false)
    } else {
        exact_find(ctx, &line, &target, true)
    };

    let Some(i) = pos else {
        return Operand::err(XlError::NA);
    };
    let pick = |ctx: &mut EvalCtx, idx: f64| {
        if idx == i32::MIN as f64 {
            return Value::Error(XlError::REF);
        }
        if idx.is_nan() || idx < 1.0 {
            return Value::Error(XlError::VALUE);
        }
        if idx > width as f64 {
            return Value::Error(XlError::REF);
        }
        let k = idx as u32 - 1;
        if vertical {
            grid.get(ctx, i, k)
        } else {
            grid.get(ctx, k, i)
        }
    };
    match idx {
        Ok(idx) => Operand::Val(pick(ctx, idx)),

        Err(arr) => {
            if let Some(e) = index_array_error(&arr, width) {
                return Operand::err(e);
            }
            let data = arr
                .data
                .iter()
                .map(|v| {
                    let n = ctx.to_number(v).map_or(f64::NAN, |x| sfix(x) as f64);
                    pick(ctx, n)
                })
                .collect();
            Operand::Val(Array::new(arr.rows, arr.cols, data).into_value())
        }
    }
}

fn choose(ctx: &mut EvalCtx, args: &[Node]) -> Operand {
    let i = ctx.eval(&args[0]);
    let i = ctx.deref(i);
    let n = args.len() - 1;
    let pick = |ctx: &EvalCtx, v: &Value| -> R<usize> {
        let k = ctx.to_number(v)?.trunc();
        if k < 1.0 || k > n as f64 {
            Err(XlError::VALUE)
        } else {
            Ok(k as usize)
        }
    };
    match i {
        Value::Array(a) => {
            let mut choices: Vec<Value> = Vec::with_capacity(n + 1);
            choices.push(Value::Blank);
            let (mut rows, mut cols) = (a.rows, a.cols);
            for node in &args[1..] {
                let x = match node {
                    Node::Missing => Operand::Val(Value::Number(0.0)),
                    node => ctx.eval(node),
                };
                let v = ctx.deref(x);
                if let Value::Array(b) = &v {
                    rows = rows.max(b.rows);
                    cols = cols.max(b.cols);
                }
                choices.push(v);
            }
            let mut out = Vec::with_capacity(rows as usize * cols as usize);
            for r in 0..rows {
                for c in 0..cols {
                    let v = a.broadcast_get(r, c);
                    out.push(match pick(ctx, &v) {
                        Ok(k) => match &choices[k] {
                            Value::Array(b) => match b.broadcast_get(r, c) {
                                Value::Blank => Value::Number(0.0),
                                x => x,
                            },
                            Value::Blank => Value::Number(0.0),
                            other => other.clone(),
                        },
                        Err(e) => Value::Error(match v {
                            Value::Error(ev) => ev,
                            _ => e,
                        }),
                    });
                }
            }
            Operand::Val(Array::new(rows, cols, out).into_value())
        }
        v => match pick(ctx, &v) {
            Ok(k) => match &args[k] {
                Node::Missing => Operand::Missing,
                node => ctx.eval(node),
            },
            Err(e) => Operand::err(e),
        },
    }
}

fn offset(ctx: &mut EvalCtx, ops: &[Operand]) -> Operand {
    let mut args = ops.to_vec();
    let mut shape: Option<(u32, u32)> = None;
    for op in args.iter_mut().skip(1) {
        if let Operand::Ref(_) = op {
            let r = std::mem::replace(op, Operand::Missing);
            *op = Operand::Val(ctx.deref(r));
        }
        if let Operand::Val(Value::Array(a)) = op {
            let (r, c) = shape.unwrap_or((1, 1));
            shape = Some((r.max(a.rows), c.max(a.cols)));
        }
    }
    if ctx.interrupted() {
        return Operand::err(XlError::CALC);
    }
    let Some((rows, cols)) = shape else {
        return offset_one(ctx, &args);
    };
    let mut items = Vec::with_capacity(rows as usize * cols as usize);
    let mut elem = args.clone();
    for r in 0..rows {
        for c in 0..cols {
            for (i, op) in args.iter().enumerate().skip(1) {
                if let Operand::Val(Value::Array(a)) = op {
                    elem[i] = Operand::Val(a.broadcast_get(r, c));
                }
            }
            items.push(match offset_one(ctx, &elem) {
                Operand::Ref(Reference::Area(a)) => Ok(a),
                Operand::Val(Value::Error(e)) => Err(e),
                _ => Err(XlError::VALUE),
            });
        }
    }
    Operand::Ref(Reference::Grid(std::sync::Arc::new(
        crate::eval::RefGrid::new(rows, cols, items),
    )))
}

fn offset_one(ctx: &mut EvalCtx, ops: &[Operand]) -> Operand {
    let n = |ctx: &EvalCtx, o: Option<&Operand>, d: f64| -> R<(f64, bool, bool)> {
        match o {
            None | Some(Operand::Missing) => Ok((d, false, d > 0.0)),
            Some(Operand::Val(v)) => ctx.to_number(v).map(|x| (x.trunc(), x == 0.0, x > 0.0)),
            Some(_) => Err(XlError::VALUE),
        }
    };
    let base = match &ops[0] {
        Operand::Ref(r) => match r.single() {
            Some(a) => *a,
            None => {
                for i in 1..ops.len().min(5) {
                    if let Err(e) = n(ctx, ops.get(i), 0.0) {
                        return Operand::err(e);
                    }
                }
                return Operand::err(XlError::VALUE);
            }
        },
        Operand::Val(Value::Error(e)) => return Operand::err(*e),
        _ => return Operand::err(XlError::VALUE),
    };

    let r = (|| {
        let dr = n(ctx, ops.get(1), 0.0)?.0;
        let dc = n(ctx, ops.get(2), 0.0)?.0;
        let (h, hz, hp) = n(ctx, ops.get(3), base.rect.rows() as f64)?;
        let (w, wz, wp) = n(ctx, ops.get(4), base.rect.cols() as f64)?;

        if hz || wz {
            return Err(XlError::REF);
        }
        let r0 = base.rect.r0 as f64 + dr;
        let c0 = base.rect.c0 as f64 + dc;
        let (r0, r1) = if h > 0.0 || hp {
            (r0, r0 + h - 1.0)
        } else {
            (r0 + h + 1.0, r0)
        };
        let (c0, c1) = if w > 0.0 || wp {
            (c0, c0 + w - 1.0)
        } else {
            (c0 + w + 1.0, c0)
        };
        let (r0, r1) = if r0 > r1 { (r1, r0) } else { (r0, r1) };
        let (c0, c1) = if c0 > c1 { (c1, c0) } else { (c0, c1) };
        if r0 < 0.0 || c0 < 0.0 || r1 >= MAX_ROWS as f64 || c1 >= MAX_COLS as f64 {
            return Err(XlError::REF);
        }
        Ok(Area {
            sheet: base.sheet,
            rect: Rect::new(r0 as u32, c0 as u32, r1 as u32, c1 as u32),
        })
    })();
    match r {
        Ok(a) => Operand::Ref(Reference::Area(a)),
        Err(e) => Operand::err(e),
    }
}

fn parse_r1c1(s: &str, host_row: u32, host_col: u32) -> Option<Rect> {
    fn part(s: &str, letter: u8, host: u32, max: u32) -> Option<(u32, usize)> {
        let b = s.as_bytes();
        if b.first()?.to_ascii_uppercase() != letter {
            return None;
        }
        if b.get(1) == Some(&b'[') {
            let end = s.find(']')?;
            let body = &s[2..end];
            let digits = body.strip_prefix(['+', '-']).unwrap_or(body);
            if digits.is_empty() || !digits.bytes().all(|c| c.is_ascii_digit()) {
                return None;
            }
            let off: i64 = body.parse().ok()?;
            if off.abs() >= max as i64 {
                return None;
            }
            return Some(((host as i64 + off).rem_euclid(max as i64) as u32, end + 1));
        }
        let mut i = 1;
        while i < b.len() && b[i].is_ascii_digit() {
            i += 1;
        }
        if i == 1 {
            return Some((host, 1));
        }
        let n: u32 = s[1..i].parse().ok()?;
        (1..=max).contains(&n).then_some((n - 1, i))
    }

    let one = |t: &str| -> Option<(Option<u32>, Option<u32>)> {
        match t.as_bytes().first()?.to_ascii_uppercase() {
            b'R' => {
                let (r, i) = part(t, b'R', host_row, MAX_ROWS)?;
                if i == t.len() {
                    return Some((Some(r), None));
                }
                let (c, j) = part(&t[i..], b'C', host_col, MAX_COLS)?;
                (i + j == t.len()).then_some((Some(r), Some(c)))
            }
            b'C' => {
                let (c, i) = part(t, b'C', host_col, MAX_COLS)?;
                (i == t.len()).then_some((None, Some(c)))
            }
            _ => None,
        }
    };
    let rect = |a: (Option<u32>, Option<u32>), b: (Option<u32>, Option<u32>)| -> Option<Rect> {
        match (a, b) {
            ((Some(r0), Some(c0)), (Some(r1), Some(c1))) => Some(Rect::new(r0, c0, r1, c1)),
            ((Some(r0), None), (Some(r1), None)) => Some(Rect::new(r0, 0, r1, MAX_COLS - 1)),
            ((None, Some(c0)), (None, Some(c1))) => Some(Rect::new(0, c0, MAX_ROWS - 1, c1)),
            _ => None,
        }
    };
    match s.split_once(':') {
        Some((a, b)) => rect(
            one(a.trim_end_matches(INDIRECT_BLANKS))?,
            one(b.trim_start_matches(INDIRECT_BLANKS))?,
        ),
        None => {
            let t = one(s)?;
            rect(t, t)
        }
    }
}

const INDIRECT_BLANKS: [char; 2] = [' ', '\n'];

fn indirect(ctx: &mut EvalCtx, ops: &[Operand]) -> Operand {
    let first = match &ops[0] {
        Operand::Ref(_) => Operand::Val(ctx.deref(ops[0].clone())),
        o => o.clone(),
    };
    if let Operand::Val(Value::Array(a)) = &first {
        let mut elem = ops.to_vec();
        let mut items = Vec::with_capacity(a.data.len());
        for e in a.data.iter() {
            elem[0] = Operand::Val(e.clone());
            items.push(match indirect_one(ctx, &elem) {
                Operand::Ref(Reference::Area(x)) => Ok(x),
                Operand::Val(Value::Error(x)) => Err(x),
                _ => Err(XlError::VALUE),
            });
        }
        return Operand::Ref(Reference::Grid(std::sync::Arc::new(
            crate::eval::RefGrid::new(a.rows, a.cols, items),
        )));
    }
    let mut ops = ops.to_vec();
    ops[0] = first;
    indirect_one(ctx, &ops)
}

fn indirect_one(ctx: &mut EvalCtx, ops: &[Operand]) -> Operand {
    let s = match &ops[0] {
        Operand::Val(v) => match text(v) {
            Ok(s) => s,
            Err(e) => return Operand::err(e),
        },
        _ => return Operand::err(XlError::REF),
    };
    let a1 = match ops.get(1) {
        None => true,

        Some(Operand::Missing) => false,

        Some(op) => match ctx.deref(op.clone()) {
            Value::Array(a) => {
                let data = a
                    .data
                    .iter()
                    .map(|e| match e {
                        Value::Error(e) => Value::Error(*e),
                        e => match indirect_one(ctx, &[ops[0].clone(), Operand::Val(e.clone())]) {
                            Operand::Val(Value::Error(x)) => Value::Error(x),
                            _ => Value::Error(XlError::VALUE),
                        },
                    })
                    .collect();
                return Operand::Val(Array::new(a.rows, a.cols, data).into_value());
            }
            v => match boolean(&v) {
                Ok(b) => b,
                Err(e) => return Operand::err(e),
            },
        },
    };
    let s = s.trim_end_matches(INDIRECT_BLANKS);
    let (sheet, body) = match s.rfind('!') {
        Some(i) => {
            let raw = &s[..i];
            let name = match raw.strip_prefix('\'').and_then(|r| r.strip_suffix('\'')) {
                Some(inner) if !inner.replace("''", "").contains('\'') => {
                    Some(inner.replace("''", "'"))
                }
                Some(_) => None,
                None => crate::parser::print::bare_sheet_name_ok(raw).then(|| raw.to_string()),
            };
            let Some(name) = name else {
                return Operand::err(XlError::REF);
            };
            match ctx.wb.sheet_by_name(&name) {
                Some(sh) => (sh.id, s[i + 1..].trim_start_matches(INDIRECT_BLANKS)),
                None => return Operand::err(XlError::REF),
            }
        }
        None => (ctx.host.sheet, s),
    };

    if let Some(cell) = body
        .strip_suffix('#')
        .map(|c| c.trim_end_matches(INDIRECT_BLANKS))
    {
        let rect = if a1 {
            parse_a1_rect(&cell.replace('$', ""))
        } else {
            parse_r1c1(cell, ctx.host.row, ctx.host.col)
        };
        return match rect {
            Some(r) if r.rows() == 1 && r.cols() == 1 => {
                let out = crate::eval::refs::spill_ref(
                    ctx,
                    Operand::Ref(Reference::Area(Area { sheet, rect: r })),
                );

                let plain = match (
                    &out,
                    ctx.wb.formula_at(crate::CellAddr::new(sheet, r.r0, r.c0)),
                ) {
                    (Operand::Ref(_), Some(fid)) => {
                        let f = ctx.wb.formula(fid);
                        f.spill.is_none()
                            && !yields_array(&f.formula.root)
                            && !matches!(f.value, Value::Error(_) | Value::Array(_))
                    }
                    _ => false,
                };
                if plain {
                    Operand::err(XlError::REF)
                } else {
                    out
                }
            }
            _ => Operand::err(XlError::REF),
        };
    }
    let rect = if a1 {
        match body.split_once(':') {
            Some((x, y)) => parse_a1_rect(
                &format!(
                    "{}:{}",
                    x.trim_end_matches(INDIRECT_BLANKS),
                    y.trim_start_matches(INDIRECT_BLANKS)
                )
                .replace('$', ""),
            ),
            None => parse_a1_rect(&body.replace('$', "")),
        }
    } else {
        parse_r1c1(body, ctx.host.row, ctx.host.col)
    };
    match rect {
        Some(rect) => Operand::Ref(Reference::Area(Area { sheet, rect })),
        None => {
            let key = body.to_uppercase();
            if let Some(def) = ctx.wb.names.lookup(&key, ctx.host.sheet) {
                let mut root = &def.formula.root;
                while let crate::formula::Node::Ws { node, .. }
                | crate::formula::Node::Paren(node) = root
                {
                    root = node;
                }
                if !matches!(
                    root,
                    crate::formula::Node::Ref(..)
                        | crate::formula::Node::StructRef { .. }
                        | crate::formula::Node::DeadRef(..)
                ) {
                    return Operand::err(XlError::REF);
                }

                let host = ctx.host;
                ctx.host = crate::CellAddr::new(host.sheet, 0, 0);
                let v = crate::eval::refs::eval_name(ctx, &key, None);
                ctx.host = host;

                if let Operand::Ref(r) = &v {
                    if r.is_span() {
                        return Operand::err(XlError::REF);
                    }
                    return v;
                }
            }

            let (tname, inner) = match body.find('[') {
                Some(i) if body.ends_with(']') => (&body[..i], Some(&body[i + 1..body.len() - 1])),
                _ => (body, None),
            };
            if let Some(t) = ctx.wb.table_by_name(tname.trim()) {
                let spec = match inner {
                    None => Some(crate::workbook::tables::Spec {
                        items: crate::workbook::tables::Items::DATA,
                        ..Default::default()
                    }),
                    Some(i) => crate::workbook::tables::parse_spec(i).ok(),
                };
                let mut spec = spec;
                let cols = spec.as_ref().and_then(|s| s.cols.clone());
                if let (Some(sp), Some((a, b))) = (spec.as_mut(), cols) {
                    let canon = |c: &str| {
                        t.columns
                            .iter()
                            .find(|x| crate::text::eq_ordinal_ci(x, c))
                            .cloned()
                    };
                    match (canon(&a), canon(&b)) {
                        (Some(a), Some(b)) => sp.cols = Some((a, b)),
                        _ => spec = None,
                    }
                }
                if let Some(sp) = spec {
                    let tn = t.name.clone();
                    return match crate::eval::refs::struct_rect(ctx.wb, &tn, &sp, ctx.host) {
                        Ok(a) => Operand::Ref(Reference::Area(a)),
                        Err(e) => Operand::err(e),
                    };
                }
            }
            Operand::err(XlError::REF)
        }
    }
}

fn address(ctx: &mut EvalCtx, ops: &[Operand]) -> Operand {
    let get = |ctx: &EvalCtx, i: usize, d: f64| -> R<f64> {
        match ops.get(i) {
            None | Some(Operand::Missing) => Ok(d),

            Some(Operand::Val(v)) => ctx.to_number(v).map(|x| sfix(x) as f64),
            Some(_) => Err(XlError::VALUE),
        }
    };
    opval((|| {
        let row = get(ctx, 0, 0.0)?;
        let col = get(ctx, 1, 0.0)?;
        let abs = get(ctx, 2, 1.0)?;
        let a1 = match ops.get(3) {
            None | Some(Operand::Missing) => true,
            Some(Operand::Val(v)) => boolean(v)?,
            Some(_) => return Err(XlError::VALUE),
        };
        if !(1.0..=4.0).contains(&abs) {
            return Err(XlError::VALUE);
        }
        let (ra, ca) = match abs as u32 {
            1 => (true, true),
            2 => (true, false),
            3 => (false, true),
            _ => (false, false),
        };

        let (mr, mc) = (MAX_ROWS as f64, MAX_COLS as f64);
        let row_ok = if a1 || ra {
            (1.0..=mr).contains(&row)
        } else {
            row > -mr && row < mr
        };
        let col_ok = if a1 || ca {
            (1.0..=mc).contains(&col)
        } else {
            col > -mc && col < mc
        };
        if !row_ok || !col_ok {
            return Err(XlError::VALUE);
        }
        let body = if a1 {
            format!(
                "{}{}{}{}",
                if ca { "$" } else { "" },
                col_to_letters(col as u32 - 1),
                if ra { "$" } else { "" },
                row as u32
            )
        } else {
            let r = if ra {
                format!("R{}", row)
            } else if row == 0.0 {
                "R".to_string()
            } else {
                format!("R[{}]", row)
            };
            let c = if ca {
                format!("C{}", col)
            } else if col == 0.0 {
                "C".to_string()
            } else {
                format!("C[{}]", col)
            };
            format!("{r}{c}")
        };
        let sheet = match ops.get(4) {
            None | Some(Operand::Missing) => None,
            Some(Operand::Val(v)) => Some(text(v)?),
            Some(_) => return Err(XlError::VALUE),
        };
        Ok(Value::from(match sheet {
            Some(s)
                if s.len() > 2
                    && s.starts_with('[')
                    && s.ends_with(']')
                    && !s[1..s.len() - 1].contains(['[', ']']) =>
            {
                format!("{s}!{body}")
            }
            Some(s) if !s.is_empty() => {
                format!("{}!{}", crate::parser::print::sheet_name_text(&s), body)
            }
            Some(_) => format!("!{body}"),
            None => body,
        }))
    })())
}

fn transpose(ctx: &mut EvalCtx, ops: &[Operand]) -> Operand {
    if let Operand::Ref(Reference::Grid(g)) = &ops[0] {
        let (rows, cols) = (g.cols, g.rows);
        let items = (0..rows * cols)
            .map(|i| g.items[((i % cols) * g.cols + i / cols) as usize])
            .collect();
        return Operand::Ref(Reference::Grid(std::sync::Arc::new(
            crate::eval::RefGrid::new(rows, cols, items),
        )));
    }
    let grid = match Grid::from_operand_3d(&ops[0]) {
        Ok(g) => g,
        Err(e) => return Operand::err(e),
    };
    let a = grid.to_array(ctx);
    let keep = matches!(&ops[0], Operand::Val(Value::Array(_)));
    let (rows, cols) = (a.cols, a.rows);

    let get = |r: u32, c: u32| match a.get(c, r) {
        Value::Blank if a.len() == 1 => Value::Number(0.0),
        v => v.clone(),
    };
    if keep {
        let data = (0..rows * cols).map(|i| get(i / cols, i % cols)).collect();
        return Operand::Val(crate::value::Array::new(rows, cols, data).into_value());
    }
    Operand::Val(array_or_scalar(rows, cols, get))
}

fn scalar_table_error(ctx: &EvalCtx, op: &Operand) -> Option<XlError> {
    match op {
        Operand::Missing => Some(XlError::NA),
        Operand::Ref(r) if r.single().is_none() => Some(XlError::NA),
        Operand::Val(Value::Error(e)) => Some(*e),
        Operand::Val(v @ Value::Text(_)) => Some(if ctx.to_number(v).is_ok() {
            XlError::NA
        } else {
            XlError::VALUE
        }),
        Operand::Val(Value::Number(_) | Value::Bool(_)) => Some(XlError::NA),
        _ => None,
    }
}

fn yields_array(n: &crate::formula::Node) -> bool {
    crate::deep(|| {
        use crate::formula::Node;
        match n {
            Node::Lit(Value::Array(_))
            | Node::Spill(_)
            | Node::Ref(_, crate::formula::CRef::Area { .. }) => true,
            Node::Let { body, .. } => yields_array(body),
            Node::Ws { node, .. } => yields_array(node),
            Node::Paren(x) | Node::Unary(_, x) | Node::Percent(x) => yields_array(x),
            Node::Binary(crate::parser::ast::BinOp::Range, ..) => true,
            Node::Binary(_, a, b) => yields_array(a) || yields_array(b),
            Node::Func { id, args } => {
                const ARR: &[&str] = &[
                    "TRANSPOSE",
                    "UNIQUE",
                    "SORT",
                    "SORTBY",
                    "SEQUENCE",
                    "FILTER",
                    "TAKE",
                    "DROP",
                    "VSTACK",
                    "HSTACK",
                    "MAP",
                    "SCAN",
                    "BYROW",
                    "BYCOL",
                    "TOCOL",
                    "TOROW",
                    "CHOOSECOLS",
                    "CHOOSEROWS",
                    "WRAPROWS",
                    "WRAPCOLS",
                    "EXPAND",
                    "RANDARRAY",
                    "MAKEARRAY",
                    "INDIRECT",
                    "TEXTSPLIT",
                    "MMULT",
                    "MUNIT",
                    "MINVERSE",
                ];
                let name = crate::functions::def(*id).name;
                ARR.contains(&name)
                    || (matches!(name, "IF" | "CHOOSE") && args.iter().skip(1).any(yields_array))
            }
            _ => false,
        }
    })
}
