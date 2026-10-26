use rustc_hash::FxHashMap;

use super::stats;
use crate::eval::refs::implicit_intersection;
use crate::eval::{EvalCtx, Operand, Reference};
use crate::formula::{walk, Node};
use crate::functions;
use crate::value::{Array, Value, XlError};
use crate::workbook::{Entry, FormulaId, Workbook};

type R<T> = Result<T, XlError>;

#[derive(Clone, Copy, PartialEq, Eq, Debug)]
enum Func {
    Average,
    Count,
    CountA,
    Max,
    Min,
    Product,
    StDevS,
    StDevP,
    Sum,
    VarS,
    VarP,
    Median,
    Mode,
    Large,
    Small,
    PctInc,
    QuartInc,
    PctExc,
    QuartExc,
}

impl Func {
    fn from_index(n: f64) -> Option<Func> {
        use Func::*;
        Some(match n as i64 {
            1 => Average,
            2 => Count,
            3 => CountA,
            4 => Max,
            5 => Min,
            6 => Product,
            7 => StDevS,
            8 => StDevP,
            9 => Sum,
            10 => VarS,
            11 => VarP,
            12 => Median,
            13 => Mode,
            14 => Large,
            15 => Small,
            16 => PctInc,
            17 => QuartInc,
            18 => PctExc,
            19 => QuartExc,
            _ => return None,
        })
    }
}

fn scalar_num(ctx: &EvalCtx, op: &Operand) -> R<f64> {
    match op {
        Operand::Val(v) => ctx.to_number(v).map(f64::trunc),
        Operand::Missing => Err(XlError::Value),
        Operand::Ref(_) => Err(XlError::Value),
    }
}

pub(crate) fn is_subtotal_formula(wb: &Workbook, fid: FormulaId) -> bool {
    let mut found = false;
    walk(&wb.formula(fid).formula.root, &mut |n| {
        if let Node::Func { id, .. } = n {
            let name = functions::def(*id).name;
            if name == "SUBTOTAL" || name == "AGGREGATE" {
                found = true;
            }
        }
    });
    found
}

pub(crate) fn is_bare_subtotal_cell(wb: &Workbook, addr: crate::address::CellAddr) -> bool {
    let Some(Entry::Formula(fid)) = wb
        .sheet(addr.sheet)
        .and_then(|s| s.cells.get(addr.row, addr.col))
    else {
        return false;
    };
    matches!(&wb.formula(*fid).formula.root, Node::Func { id, .. } if functions::def(*id).name == "SUBTOTAL")
}

#[derive(Clone, Copy, PartialEq, Eq)]
enum Hide {
    No,

    Always,

    IfFiltered,
}

fn gather_ref(
    ctx: &mut EvalCtx,
    r: &Reference,
    skip_nested: bool,
    skip_errors: bool,
    hide: Hide,
    out: &mut Vec<Value>,
) -> R<()> {
    let areas = r.areas();
    if areas.iter().any(|a| a.sheet != areas[0].sheet) {
        return Err(XlError::Value);
    }
    let wb = ctx.wb;
    let mut cache: FxHashMap<FormulaId, bool> = FxHashMap::default();
    for area in areas {
        let sh = wb.sheet(area.sheet);
        let (r0, c0) = (area.rect.r0, area.rect.c0);
        let skip_hidden = match hide {
            Hide::No => false,
            Hide::Always => true,
            Hide::IfFiltered => wb.filter_mode(area.sheet),
        };
        let sheet_id = area.sheet;
        ctx.visit_area_lazy(
            area,
            |r, _| !(skip_hidden && wb.row_hidden(sheet_id, r0 + r)),
            |r, c, v| {
                if skip_errors && v.is_error() {
                    return true;
                }
                if skip_nested {
                    if let Some(sh) = sh {
                        let (ar, ac) = (r0 + r, c0 + c);
                        let fid = match sh.cells.get(ar, ac) {
                            Some(Entry::Formula(f)) => Some(*f),
                            Some(Entry::Const(_)) => None,
                            None => sh.spills.owner_of(ar, ac),
                        };
                        if let Some(fid) = fid {
                            if *cache
                                .entry(fid)
                                .or_insert_with(|| is_subtotal_formula(wb, fid))
                            {
                                return true;
                            }
                        }
                    }
                }
                out.push(v.clone());
                true
            },
        );
    }
    Ok(())
}

fn numbers(vals: &[Value]) -> R<Vec<f64>> {
    let mut v = Vec::with_capacity(vals.len());
    for x in vals {
        match x {
            Value::Number(n) => v.push(*n),
            Value::Error(e) => return Err(*e),
            _ => {}
        }
    }
    Ok(v)
}

fn apply(f: Func, vals: &[Value], k: f64) -> R<Value> {
    use Func::*;
    match f {
        Count => {
            return Ok(Value::Number(
                vals.iter()
                    .filter(|v| matches!(v, Value::Number(_)))
                    .count() as f64,
            ))
        }
        CountA => {
            return Ok(Value::Number(
                vals.iter().filter(|v| !v.is_blank()).count() as f64
            ))
        }
        _ => {}
    }
    let v = numbers(vals)?;
    let r = match f {
        Sum => stats::snapped_sum(&v),
        Average => {
            if v.is_empty() {
                return Err(XlError::Div0);
            }
            stats::snapped_sum(&v) / v.len() as f64
        }
        Max => stats::max(&v),
        Min => stats::min(&v),
        Product => {
            if v.is_empty() {
                0.0
            } else {
                v.iter().fold(1.0, |a, x| a * x)
            }
        }
        StDevS => stats::stdev(&v, true)?,
        StDevP => stats::stdev(&v, false)?,
        VarS => stats::var(&v, true)?,
        VarP => stats::var(&v, false)?,
        Median => stats::median(&v)?,
        Mode => stats::mode(&v)?,
        Large => stats::kth(&v, k, true)?,
        Small => stats::kth(&v, k, false)?,
        PctInc => stats::percentile_inc(&v, k)?,
        QuartInc => stats::quartile_inc(&v, k)?,
        PctExc => stats::percentile_exc(&v, k)?,
        QuartExc => stats::quartile_exc(&v, k)?,
        Count | CountA => unreachable!(),
    };
    Ok(Value::num(r))
}

pub fn subtotal(ctx: &mut EvalCtx, ops: &[Operand]) -> Operand {
    let r = (|| {
        let n = scalar_num(ctx, &ops[0])?;
        let f = match n as i64 {
            1..=11 => Func::from_index(n),
            101..=111 => Func::from_index(n - 100.0),
            _ => None,
        }
        .ok_or(XlError::Value)?;
        let mut vals = Vec::new();
        let hide = if n >= 100.0 {
            Hide::Always
        } else {
            Hide::IfFiltered
        };
        for op in &ops[1..] {
            match op {
                Operand::Ref(r) => gather_ref(ctx, r, true, false, hide, &mut vals)?,
                Operand::Val(Value::Error(e)) => return Err(*e),
                _ => return Err(XlError::Value),
            }
        }
        if ctx.interrupted() {
            return Err(XlError::Calc);
        }
        apply(f, &vals, 0.0)
    })();
    Operand::Val(r.unwrap_or_else(Value::Error))
}

pub fn aggregate(ctx: &mut EvalCtx, ops: &[Operand]) -> Operand {
    let mut ops = ops.to_vec();
    let mut grids: Vec<(usize, std::sync::Arc<Array>)> = Vec::new();
    for i in [0usize, 1, 3] {
        if i >= ops.len() {
            continue;
        }
        if i < 3 {
            if let Operand::Ref(_) = &ops[i] {
                let v = ctx.deref(ops[i].clone());
                ops[i] = Operand::Val(v);
            }
        }
        if let Operand::Val(Value::Array(a)) = &ops[i] {
            grids.push((i, a.clone()));
        }
    }
    if grids.is_empty() {
        return aggregate_one(ctx, &ops);
    }
    let rows = grids.iter().map(|g| g.1.rows).max().unwrap();
    let cols = grids.iter().map(|g| g.1.cols).max().unwrap();
    let mut data = Vec::with_capacity(rows as usize * cols as usize);
    for r in 0..rows {
        for c in 0..cols {
            let mut call = ops.clone();
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
            let v = match aggregate_one(ctx, &call) {
                Operand::Val(Value::Array(a)) => a.data[0].clone(),
                Operand::Val(v) => v,
                other => ctx.deref(other),
            };
            data.push(v);
        }
    }
    Operand::Val(Array::new(rows, cols, data).into_value())
}

fn aggregate_one(ctx: &mut EvalCtx, ops: &[Operand]) -> Operand {
    let r = (|| {
        let n = scalar_num(ctx, &ops[0])?;
        let f = if (1.0..20.0).contains(&n) {
            Func::from_index(n)
        } else {
            None
        };
        let Some(f) = f else {
            if let Some(Operand::Val(Value::Array(a))) = ops.get(3).filter(|_| ops.len() == 4) {
                return Ok(Array::new(
                    a.rows,
                    a.cols,
                    vec![Value::Error(XlError::Value); a.data.len()],
                )
                .into_value());
            }
            return Err(XlError::Value);
        };
        let opt = match &ops[1] {
            Operand::Missing => 0.0,
            op => scalar_num(ctx, op)?,
        };
        if !(0.0..=7.0).contains(&opt) {
            return Err(XlError::Value);
        }
        let opt = opt as u32;
        let skip_nested = opt <= 3;
        let skip_errors = matches!(opt, 2 | 3 | 6 | 7);
        let hide = if opt % 2 == 1 { Hide::Always } else { Hide::No };
        let mut vals = Vec::new();
        if n < 14.0 {
            if let Some(Operand::Val(Value::Array(a))) = ops.get(3).filter(|_| ops.len() == 4) {
                return Ok(Array::new(
                    a.rows,
                    a.cols,
                    vec![Value::Error(XlError::Value); a.data.len()],
                )
                .into_value());
            }
            for op in &ops[2..] {
                match op {
                    Operand::Ref(r) => {
                        gather_ref(ctx, r, skip_nested, skip_errors, hide, &mut vals)?
                    }
                    Operand::Val(Value::Error(e)) => return Err(*e),
                    _ => return Err(XlError::Value),
                }
            }
            if ctx.interrupted() {
                return Err(XlError::Calc);
            }
            return apply(f, &vals, 0.0);
        }
        if ops.len() != 4 {
            return Err(XlError::Value);
        }
        match &ops[2] {
            Operand::Ref(r) => gather_ref(ctx, r, skip_nested, skip_errors, hide, &mut vals)?,
            Operand::Val(Value::Array(a)) => {
                vals.extend(
                    a.data
                        .iter()
                        .filter(|v| !(skip_errors && v.is_error()))
                        .cloned(),
                );
            }
            Operand::Val(Value::Number(x)) => vals.push(Value::Number(*x)),

            Operand::Val(Value::Error(e)) => return Err(*e),
            Operand::Val(_) => {}
            Operand::Missing => return Err(XlError::Value),
        }

        let k = match &ops[3] {
            Operand::Missing => Value::Blank,
            Operand::Ref(_) => {
                let op = implicit_intersection(ctx, ops[3].clone());
                ctx.deref(op)
            }
            Operand::Val(v) => v.clone(),
        };
        if ctx.interrupted() {
            return Err(XlError::Calc);
        }
        let one = |ctx: &EvalCtx, k: &Value| -> Value {
            match ctx.to_number(k) {
                Ok(k) => apply(f, &vals, k).unwrap_or_else(Value::Error),
                Err(e) => Value::Error(e),
            }
        };
        Ok(match &k {
            Value::Array(a) => {
                Array::new(a.rows, a.cols, a.data.iter().map(|x| one(ctx, x)).collect())
                    .into_value()
            }
            k => one(ctx, k),
        })
    })();

    let text_head = ops.iter().take(2).any(|o| match o {
        Operand::Val(v @ Value::Text(_)) => ctx.to_number(v).is_err(),
        _ => false,
    });
    if let (Err(XlError::Value), false) = (&r, text_head) {
        for (i, op) in ops.iter().enumerate().skip(1) {
            let e = match op {
                Operand::Val(Value::Error(e)) => Some(*e),
                Operand::Ref(rf) if i == 1 && rf.single().is_some() => {
                    match ctx.deref(op.clone()) {
                        Value::Error(e) => Some(e),
                        _ => None,
                    }
                }
                _ => None,
            };
            if let Some(e) = e {
                return Operand::Val(Value::Error(e));
            }
        }
    }
    Operand::Val(r.unwrap_or_else(Value::Error))
}
