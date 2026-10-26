use crate::datetime::DateSystem;
use crate::eval::{EvalCtx, Operand};
use crate::functions::args::R;
use crate::value::{Array, Value, XlError};

pub type Arg = Option<Value>;

pub type Imp = fn(&mut EvalCtx, &[Arg]) -> Value;

pub fn call(
    ctx: &mut EvalCtx,
    ops: &[Operand],
    array_params: &[usize],
    quirk: bool,
    f: Imp,
) -> Operand {
    call_ex(ctx, ops, array_params, quirk, false, f)
}

pub fn call_ex(
    ctx: &mut EvalCtx,
    ops: &[Operand],
    array_params: &[usize],
    quirk: bool,
    late: bool,
    f: Imp,
) -> Operand {
    let mut vals: Vec<Arg> = Vec::with_capacity(ops.len());
    let mut multi: Vec<(usize, Value, Value)> = Vec::new();
    for (i, op) in ops.iter().enumerate() {
        let is_array_param = array_params.contains(&i);
        let v = match op {
            Operand::Missing => None,
            Operand::Ref(crate::eval::Reference::Grid(g)) => Some(ctx.grid_cells(g)),
            Operand::Ref(r) => {
                let Some(area) = r.single() else {
                    return Operand::err(XlError::Value);
                };
                if !is_array_param && !area.rect.is_cell() {
                    if quirk {
                        let first = ctx.cell_value(area.sheet, area.rect.r0, area.rect.c0);
                        let last = ctx.cell_value(area.sheet, area.rect.r1, area.rect.c1);
                        multi.push((i, first, last));
                    } else {
                        multi.push((
                            i,
                            Value::Error(XlError::Value),
                            Value::Error(XlError::Value),
                        ));
                    }
                    vals.push(Some(Value::Error(XlError::Value)));
                    continue;
                }
                Some(ctx.deref(op.clone()))
            }
            Operand::Val(v) => Some(v.clone()),
        };
        vals.push(v);
    }
    if ctx.interrupted() {
        return Operand::err(XlError::Calc);
    }
    let mut shape: Option<(u32, u32)> = None;
    let mut lifted = Vec::new();
    for (i, v) in vals.iter().enumerate() {
        if array_params.contains(&i) {
            continue;
        }
        if let Some(Value::Array(a)) = v {
            let (r, c) = shape.unwrap_or((1, 1));
            shape = Some((r.max(a.rows), c.max(a.cols)));
            lifted.push(i);
        }
    }
    let Some((rows, cols)) = shape else {
        if !multi.is_empty() && !late {
            return Operand::err(XlError::Value);
        }
        return Operand::Val(f(ctx, &vals));
    };
    let mut elem = vals.clone();
    let mut data = Vec::with_capacity(rows as usize * cols as usize);
    for r in 0..rows {
        for c in 0..cols {
            for &i in &lifted {
                if let Some(Value::Array(a)) = &vals[i] {
                    elem[i] = Some(a.broadcast_get(r, c));
                }
            }
            for (i, first, last) in &multi {
                elem[*i] = Some(match r * cols + c {
                    0 => Value::Error(XlError::Value),
                    1 => first.clone(),
                    _ => last.clone(),
                });
            }
            data.push(f(ctx, &elem));
        }
    }
    Operand::Val(Array::new(rows, cols, data).into_value())
}

pub fn num_of(ctx: &EvalCtx, v: &Value) -> R<f64> {
    match v {
        Value::Number(n) => Ok(*n),
        Value::Blank => Ok(0.0),
        Value::Text(_) => ctx.to_number_atp(v),
        Value::Error(e) => Err(*e),
        Value::Bool(_) | Value::Array(_) => Err(XlError::Value),
        Value::Lambda(_) => Err(XlError::Calc),
    }
}

pub fn req(ctx: &EvalCtx, a: &[Arg], i: usize) -> R<f64> {
    match a.get(i) {
        Some(Some(v)) => num_of(ctx, v),
        _ => Err(XlError::NA),
    }
}

pub fn opt(ctx: &EvalCtx, a: &[Arg], i: usize, default: f64) -> R<f64> {
    match a.get(i) {
        Some(Some(v)) => num_of(ctx, v),
        _ => Ok(default),
    }
}

pub fn date_system(ctx: &EvalCtx) -> DateSystem {
    ctx.wb.settings.date_system
}

pub fn check_date(ctx: &EvalCtx, x: f64) -> R<i64> {
    if x < 0.0 || x >= date_system(ctx).max_serial() + 1.0 {
        return Err(XlError::Num);
    }
    Ok(x.floor() as i64)
}

pub fn date(ctx: &EvalCtx, a: &[Arg], i: usize) -> R<i64> {
    let x = req(ctx, a, i)?;
    let shift = a
        .get(i)
        .and_then(|v| v.as_ref())
        .map_or(0.0, |v| ctx.atp_text_shift(v));
    check_date(ctx, x + shift).map(|d| d - shift as i64)
}

pub fn basis(ctx: &EvalCtx, a: &[Arg], i: usize) -> R<i32> {
    let b = opt(ctx, a, i, 0.0)?.trunc();
    if !(0.0..=4.0).contains(&b) {
        return Err(XlError::Num);
    }
    Ok(b as i32)
}

pub fn frequency(x: f64) -> R<i32> {
    let f = x.trunc();
    if f == 1.0 || f == 2.0 || f == 4.0 {
        Ok(f as i32)
    } else {
        Err(XlError::Num)
    }
}

pub fn out(r: R<f64>) -> Value {
    match r {
        Ok(x) => Value::num(x),
        Err(e) => Value::Error(e),
    }
}
