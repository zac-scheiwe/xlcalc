use crate::coerce;
use crate::eval::{Area, EvalCtx, Operand};
use crate::value::{Array, Text, Value, XlError};

pub type R<T> = Result<T, XlError>;

#[inline]
pub fn err(e: XlError) -> Value {
    Value::Error(e)
}

pub fn first_cell_or_lift(ctx: &mut EvalCtx, op: &Operand, f: fn(&Value) -> Value) -> Operand {
    let v = match op {
        Operand::Ref(r) => match r.areas().first() {
            Some(a) => ctx.cell_value(a.sheet, a.rect.r0, a.rect.c0),
            None => Value::Error(XlError::Ref),
        },
        Operand::Val(v) => v.clone(),
        Operand::Missing => Value::Blank,
    };
    Operand::Val(match v {
        Value::Array(a) => {
            ctx.circ_lifted |=
                ctx.has_placeholders() && crate::eval::contains_circ(&Value::Array(a.clone()));
            Array::new(a.rows, a.cols, a.data.iter().map(f).collect()).into_value()
        }
        v => f(&v),
    })
}

#[inline]
pub fn val(r: R<Value>) -> Value {
    r.unwrap_or_else(Value::Error)
}

#[inline]
pub fn opval(r: R<Value>) -> Operand {
    Operand::Val(val(r))
}

pub fn num(ctx: &EvalCtx, v: &Value) -> R<f64> {
    ctx.to_number(v)
}

pub fn opt_num(ctx: &EvalCtx, args: &[Value], i: usize, default: f64) -> R<f64> {
    match args.get(i) {
        None => Ok(default),
        Some(v) => ctx.to_number(v),
    }
}

pub fn opt_num_or_blank(ctx: &EvalCtx, args: &[Value], i: usize, default: f64) -> R<f64> {
    match args.get(i) {
        None | Some(Value::Blank) => Ok(default),
        Some(v) => ctx.to_number(v),
    }
}

pub fn text(v: &Value) -> R<Text> {
    coerce::to_text(v)
}

pub fn boolean(v: &Value) -> R<bool> {
    coerce::to_bool(v)
}

pub fn opt_bool(args: &[Value], i: usize, default: bool) -> R<bool> {
    match args.get(i) {
        None => Ok(default),
        Some(v) => coerce::to_bool(v),
    }
}

#[inline]
pub fn sfix(x: f64) -> i64 {
    if x.is_nan() {
        return i32::MIN as i64;
    }
    let s = crate::x87::add(x, 2147483648.0);
    if s < 0.0 {
        return i32::MIN as i64;
    }
    let f = s.floor();
    if f >= 4294967296.0 {
        return i32::MAX as i64;
    }
    f as i64 - 2147483648
}

#[inline]
pub fn lfix(x: f64) -> i64 {
    let t = x.trunc();
    if t >= -2147483648.0 && t < 2147483648.0 {
        t as i64
    } else {
        i32::MIN as i64
    }
}

#[inline]
pub fn ifuzz(x: f64) -> f64 {
    if x < 0.0 {
        -(sfix(-x) as f64)
    } else {
        sfix(x) as f64
    }
}

#[inline]
pub fn ffuzz(x: f64) -> f64 {
    sfix(x) as f64
}

#[inline]
pub fn trunc(n: f64) -> f64 {
    n.trunc()
}

#[derive(Clone, Copy, Debug)]
pub struct AggPolicy {
    pub propagate_errors: bool,

    pub direct_text_error: bool,

    pub a_variant: bool,

    pub array_text_zero: bool,
}

pub const SUM_POLICY: AggPolicy = AggPolicy {
    propagate_errors: true,
    direct_text_error: true,
    a_variant: false,
    array_text_zero: false,
};
pub const COUNT_POLICY: AggPolicy = AggPolicy {
    propagate_errors: false,
    direct_text_error: false,
    a_variant: false,
    array_text_zero: false,
};
pub const A_POLICY: AggPolicy = AggPolicy {
    propagate_errors: true,
    direct_text_error: true,
    a_variant: true,
    array_text_zero: false,
};
pub const AVGA_POLICY: AggPolicy = AggPolicy {
    propagate_errors: true,
    direct_text_error: true,
    a_variant: true,
    array_text_zero: true,
};

pub fn visit_numbers(
    ctx: &mut EvalCtx,
    ops: &[Operand],
    p: AggPolicy,
    mut f: impl FnMut(f64),
) -> R<()> {
    for op in ops {
        match op {
            Operand::Val(Value::Error(e)) if p.propagate_errors => return Err(*e),
            Operand::Val(Value::Lambda(_)) if p.propagate_errors => return Err(XlError::Value),
            Operand::Val(v @ Value::Text(_)) if p.direct_text_error => {
                ctx.to_number(v)?;
            }
            _ => {}
        }
    }
    for op in ops {
        match op {
            Operand::Missing => f(0.0),
            Operand::Val(Value::Array(a)) => visit_array_numbers(a, p, &mut f)?,
            Operand::Val(v) => match v {
                Value::Number(n) => f(*n),
                Value::Bool(b) => f(if *b { 1.0 } else { 0.0 }),
                Value::Blank => f(0.0),
                Value::Text(_) => match ctx.to_number(v) {
                    Ok(n) => f(n),
                    Err(e) => {
                        if p.direct_text_error {
                            return Err(e);
                        }
                    }
                },
                Value::Error(e) => {
                    if p.propagate_errors {
                        return Err(*e);
                    }
                }
                Value::Lambda(_) => {
                    if p.propagate_errors {
                        return Err(XlError::Value);
                    }
                }
                Value::Array(_) => unreachable!(),
            },

            Operand::Ref(crate::eval::Reference::Grid(_))
                if !p.propagate_errors && !p.a_variant => {}

            Operand::Ref(crate::eval::Reference::Grid(g)) if p.propagate_errors => {
                for item in &g.items {
                    match item {
                        Err(e) => return Err(*e),
                        Ok(a) => match ctx.cell_value(a.sheet, a.rect.r0, a.rect.c0) {
                            Value::Number(n) => f(n),
                            Value::Error(e) => return Err(e),
                            Value::Text(_) if p.a_variant => f(0.0),
                            Value::Bool(b) if p.a_variant => f(if b { 1.0 } else { 0.0 }),
                            _ => {}
                        },
                    }
                }
            }
            Operand::Ref(r) => {
                let mut failure = None;
                for area in r.areas() {
                    ctx.visit_area_until(area, |_, _, v| {
                        match v {
                            Value::Number(n) => f(*n),
                            Value::Error(e) if p.propagate_errors => failure = Some(*e),
                            Value::Text(_) if p.a_variant => f(0.0),
                            Value::Bool(b) if p.a_variant => f(if *b { 1.0 } else { 0.0 }),
                            _ => {}
                        }
                        failure.is_none()
                    });
                    if let Some(e) = failure {
                        return Err(e);
                    }
                }
            }
        }
    }
    Ok(())
}

fn visit_array_numbers(a: &Array, p: AggPolicy, f: &mut impl FnMut(f64)) -> R<()> {
    for v in &a.data {
        match v {
            Value::Number(n) => f(*n),
            Value::Error(e) if p.propagate_errors => return Err(*e),
            Value::Text(_) if p.array_text_zero => f(0.0),
            _ => {}
        }
    }
    Ok(())
}

pub fn collect_numbers(ctx: &mut EvalCtx, ops: &[Operand], p: AggPolicy) -> R<Vec<f64>> {
    let mut v = Vec::new();
    visit_numbers(ctx, ops, p, |n| v.push(n))?;
    Ok(v)
}

pub enum Grid {
    Array(std::sync::Arc<Array>),
    Area(Area),
    Scalar(Value),
}

impl Grid {
    pub fn from_operand_3d(op: &Operand) -> R<Grid> {
        match op {
            Operand::Ref(r) if r.is_span() && r.single().is_none() => Err(XlError::Ref),
            _ => Grid::from_operand(op),
        }
    }

    pub fn from_operand(op: &Operand) -> R<Grid> {
        Ok(match op {
            Operand::Ref(crate::eval::Reference::Grid(g)) => Grid::Array(g.value_array()),
            Operand::Ref(r) => Grid::Area(*r.single().ok_or(XlError::Value)?),
            Operand::Val(Value::Array(a)) => Grid::Array(a.clone()),
            Operand::Val(v) => Grid::Scalar(v.clone()),
            Operand::Missing => Grid::Scalar(Value::Blank),
        })
    }

    pub fn rows(&self) -> u32 {
        match self {
            Grid::Array(a) => a.rows,
            Grid::Area(a) => a.rect.rows(),
            Grid::Scalar(_) => 1,
        }
    }

    pub fn cols(&self) -> u32 {
        match self {
            Grid::Array(a) => a.cols,
            Grid::Area(a) => a.rect.cols(),
            Grid::Scalar(_) => 1,
        }
    }

    pub fn get(&self, ctx: &mut EvalCtx, r: u32, c: u32) -> Value {
        match self {
            Grid::Array(a) => a.get(r, c).clone(),
            Grid::Area(a) => ctx.cell_value(a.sheet, a.rect.r0 + r, a.rect.c0 + c),
            Grid::Scalar(v) => v.clone(),
        }
    }

    pub fn to_value(&self, ctx: &mut EvalCtx) -> Value {
        match self {
            Grid::Array(a) => Value::Array(a.clone()),
            Grid::Area(a) => ctx.area_array(a),
            Grid::Scalar(v) => v.clone(),
        }
    }

    pub fn to_array(&self, ctx: &mut EvalCtx) -> std::sync::Arc<Array> {
        match self.to_value(ctx) {
            Value::Array(a) => a,
            v => std::sync::Arc::new(Array::new(1, 1, vec![v])),
        }
    }
}

pub fn array_always(rows: u32, cols: u32, f: impl FnMut(u32, u32) -> Value) -> Value {
    Array::from_fn(rows, cols, f).into_value()
}

pub fn array_or_scalar(rows: u32, cols: u32, f: impl FnMut(u32, u32) -> Value) -> Value {
    let a = Array::from_fn(rows, cols, f);
    if rows == 1 && cols == 1 {
        a.data.into_iter().next().unwrap()
    } else {
        a.into_value()
    }
}

pub fn check_array_args(ctx: &mut EvalCtx, ops: &[&Operand]) -> R<()> {
    for op in ops {
        match op {
            Operand::Missing | Operand::Val(Value::Blank) => return Err(XlError::Value),
            Operand::Val(Value::Error(e)) => return Err(*e),
            Operand::Ref(r) => {
                if let Some(a) = r.single() {
                    if a.rect.rows() == 1 && a.rect.cols() == 1 {
                        match ctx.cell_value(a.sheet, a.rect.r0, a.rect.c0) {
                            Value::Blank => return Err(XlError::Value),
                            Value::Error(e) => return Err(e),
                            _ => {}
                        }
                    }
                }
            }
            _ => {}
        }
    }
    Ok(())
}
