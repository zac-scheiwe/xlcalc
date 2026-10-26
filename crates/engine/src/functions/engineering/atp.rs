use crate::eval::{EvalCtx, Operand};
use crate::number;
use crate::value::{Array, Text, Value, XlError};

pub type R<T> = Result<T, XlError>;

pub fn call(
    ctx: &mut EvalCtx,
    ops: &[Operand],
    spec: &str,
    mut f: impl FnMut(&mut EvalCtx, &[Option<Value>], &[Operand]) -> Value,
) -> Operand {
    let spec = spec.as_bytes();
    let mut vals: Vec<Option<Value>> = Vec::with_capacity(ops.len());
    let mut lifted = Vec::new();
    let mut shape: Option<(u32, u32)> = None;

    let mut multi: Vec<(usize, Value, Value)> = Vec::new();

    for (i, op) in ops.iter().enumerate() {
        let class = spec[i.min(spec.len() - 1)];
        if class == b'r' {
            vals.push(None);
            continue;
        }
        let v = match op {
            Operand::Missing => None,
            Operand::Val(v) => Some(v.clone()),
            Operand::Ref(crate::eval::Reference::Grid(g)) => Some(ctx.grid_cells(g)),
            Operand::Ref(r) => {
                let Some(area) = r.single() else {
                    vals.push(Some(Value::Error(XlError::Value)));
                    continue;
                };
                if (class == b'a' || class == b'b') && !area.rect.is_cell() {
                    if class == b'b' {
                        let first = ctx.cell_value(area.sheet, area.rect.r0, area.rect.c0);
                        let last = ctx.cell_value(area.sheet, area.rect.r1, area.rect.c1);
                        multi.push((i, first, last));
                    }
                    vals.push(Some(Value::Error(XlError::Value)));
                    continue;
                }
                let area = *area;
                Some(ctx.area_array(&area))
            }
        };
        if let Some(Value::Array(a)) = &v {
            let (r, c) = shape.unwrap_or((1, 1));
            shape = Some((r.max(a.rows), c.max(a.cols)));
            lifted.push(i);
        }
        vals.push(v);
    }
    if ctx.interrupted() {
        return Operand::err(XlError::Calc);
    }
    let Some((rows, cols)) = shape else {
        return Operand::Val(f(ctx, &vals, ops));
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
            data.push(f(ctx, &elem, ops));
        }
    }
    Operand::Val(Array::new(rows, cols, data).into_value())
}

pub fn num(ctx: &EvalCtx, v: &Value) -> R<f64> {
    match v {
        Value::Bool(_) => Err(XlError::Value),
        other => ctx.to_number_atp(other),
    }
}

pub fn req_num(ctx: &EvalCtx, a: &[Option<Value>], i: usize) -> R<f64> {
    match a.get(i) {
        Some(Some(v)) => num(ctx, v),
        _ => Err(XlError::NA),
    }
}

pub fn opt_num(ctx: &EvalCtx, a: &[Option<Value>], i: usize) -> R<Option<f64>> {
    match a.get(i) {
        Some(Some(v)) => num(ctx, v).map(Some),
        _ => Ok(None),
    }
}

pub fn text(v: &Value) -> R<Text> {
    match v {
        Value::Bool(_) => Err(XlError::Value),
        Value::Number(n) => Ok(number::format_general(*n).into()),
        other => crate::coerce::to_text(other),
    }
}

pub fn req_text(a: &[Option<Value>], i: usize) -> R<Text> {
    match a.get(i) {
        Some(Some(v)) => text(v),
        _ => Err(XlError::NA),
    }
}

pub fn out(r: R<f64>) -> Value {
    match r {
        Ok(x) => Value::num(x),
        Err(e) => Value::Error(e),
    }
}

pub fn visit_list(
    ctx: &mut EvalCtx,
    op: &Operand,
    mut f: impl FnMut(&mut EvalCtx, &Value) -> R<()>,
) -> R<()> {
    match op {
        Operand::Missing => Ok(()),
        Operand::Val(Value::Array(a)) => {
            for v in &a.data {
                f(ctx, v)?;
            }
            Ok(())
        }
        Operand::Val(v) => f(ctx, v),
        Operand::Ref(r) => {
            for area in r.areas().to_vec() {
                let v = ctx.area_array(&area);
                if ctx.interrupted() {
                    return Err(XlError::Calc);
                }
                match v {
                    Value::Array(a) => {
                        for x in &a.data {
                            f(ctx, x)?;
                        }
                    }
                    other => f(ctx, &other)?,
                }
            }
            Ok(())
        }
    }
}
