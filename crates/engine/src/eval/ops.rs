use std::cmp::Ordering;

use super::EvalCtx;
use crate::coerce;
use crate::number;
use crate::parser::ast::{BinOp, UnaryOp};
use crate::text;
use crate::value::{Array, Value, XlError};

pub fn map1(v: Value, mut f: impl FnMut(&Value) -> Value) -> Value {
    match v {
        Value::Array(a) => {
            Array::new(a.rows, a.cols, a.data.iter().map(&mut f).collect()).into_value()
        }
        other => f(&other),
    }
}

pub fn map2(a: Value, b: Value, mut f: impl FnMut(&Value, &Value) -> Value) -> Value {
    match (&a, &b) {
        (Value::Array(x), Value::Array(y)) => {
            let rows = x.rows.max(y.rows);
            let cols = x.cols.max(y.cols);

            let inside = |a: &Array, r: u32, c: u32| {
                (a.rows == 1 || r < a.rows) && (a.cols == 1 || c < a.cols)
            };
            Array::from_fn(rows, cols, |r, c| {
                if !inside(x, r, c) || !inside(y, r, c) {
                    return Value::Error(XlError::NA);
                }
                f(&x.broadcast_get(r, c), &y.broadcast_get(r, c))
            })
            .into_value()
        }
        (Value::Array(x), _) => {
            Array::new(x.rows, x.cols, x.data.iter().map(|p| f(p, &b)).collect()).into_value()
        }
        (_, Value::Array(y)) => {
            Array::new(y.rows, y.cols, y.data.iter().map(|q| f(&a, q)).collect()).into_value()
        }
        _ => f(&a, &b),
    }
}

pub fn unary(ctx: &EvalCtx, op: UnaryOp, v: Value) -> Value {
    if op == UnaryOp::Plus {
        return v;
    }
    map1(v, |x| match ctx.to_number(x) {
        Ok(n) => Value::Number(number::normalize(-n)),
        Err(e) => Value::Error(e),
    })
}

pub fn percent(ctx: &EvalCtx, v: Value) -> Value {
    map1(v, |x| match ctx.to_number(x) {
        Ok(n) => Value::num(n / 100.0),
        Err(e) => Value::Error(e),
    })
}

fn arith(op: BinOp, a: f64, b: f64) -> Value {
    let r = match op {
        BinOp::Add => a + b,
        BinOp::Sub => a - b,
        BinOp::Mul => number::flush_subnormal(a * b),
        BinOp::Div => {
            if b == 0.0 {
                return Value::Error(XlError::Div0);
            }
            number::flush_subnormal(a / b)
        }
        BinOp::Pow => return power(a, b),
        _ => unreachable!(),
    };
    Value::num(r)
}

pub fn power(a: f64, b: f64) -> Value {
    if a == 0.0 {
        if b == 0.0 {
            return Value::Error(XlError::Num);
        }
        if b < 0.0 {
            return Value::Error(XlError::Div0);
        }
        return Value::Number(0.0);
    }
    if a < 0.0 && b.fract() != 0.0 {
        let inv = 1.0 / b;
        let k = inv.round();
        if (inv - k).abs() < 1e-10 && k % 2.0 != 0.0 {
            return Value::num(-crate::xmath::pow(-a, b));
        }
        return Value::Error(XlError::Num);
    }

    if a < 0.0 && b.abs() >= 4294967295.0 {
        return Value::Error(XlError::Num);
    }
    if b < 0.0 {
        let den = crate::xmath::pow(a, -b);
        if den.is_finite() && number::flush_subnormal(den) == 0.0 {
            return Value::Error(XlError::Div0);
        }
    }
    let r = crate::xmath::pow(a, b);
    if r.is_finite() {
        Value::Number(number::normalize(number::flush_subnormal(r)))
    } else {
        Value::Error(XlError::Num)
    }
}

fn scalar_binary(ctx: &EvalCtx, op: BinOp, a: &Value, b: &Value, snap: bool) -> Value {
    match op {
        BinOp::Add | BinOp::Sub | BinOp::Mul | BinOp::Div | BinOp::Pow => {
            let x = match ctx.to_number(a) {
                Ok(x) => x,
                Err(e) => return Value::Error(e),
            };
            let y = match ctx.to_number(b) {
                Ok(y) => y,
                Err(e) => return Value::Error(e),
            };
            let v = arith(op, x, y);
            if snap {
                if let Value::Number(r) = v {
                    let y2 = if op == BinOp::Sub { -y } else { y };
                    return Value::Number(number::normalize(number::snap_add(x, y2, r)));
                }
            }
            v
        }
        BinOp::Concat => {
            let x = match coerce::to_text(a) {
                Ok(x) => x,
                Err(e) => return Value::Error(e),
            };
            let y = match coerce::to_text(b) {
                Ok(y) => y,
                Err(e) => return Value::Error(e),
            };
            let mut s = String::with_capacity(x.len() + y.len());
            s.push_str(&x);
            s.push_str(&y);

            if text::utf16_len(&s) > crate::value::MAX_TEXT_LEN {
                s = text::utf16_slice(&s, 0, crate::value::MAX_TEXT_LEN);
            }
            if s.is_empty() && (a.is_cell_empty() || b.is_cell_empty()) {
                return Value::Text(crate::value::cell_empty());
            }
            Value::from(s)
        }
        BinOp::Eq | BinOp::Ne | BinOp::Lt | BinOp::Le | BinOp::Gt | BinOp::Ge => {
            if let Value::Error(e) = a {
                return Value::Error(*e);
            }
            if let Value::Error(e) = b {
                return Value::Error(*e);
            }
            if matches!(a, Value::Lambda(_)) || matches!(b, Value::Lambda(_)) {
                return Value::Error(XlError::Value);
            }
            let ord = compare(a, b);
            Value::Bool(match op {
                BinOp::Eq => ord == Ordering::Equal,
                BinOp::Ne => ord != Ordering::Equal,
                BinOp::Lt => ord == Ordering::Less,
                BinOp::Le => ord != Ordering::Greater,
                BinOp::Gt => ord == Ordering::Greater,
                BinOp::Ge => ord != Ordering::Less,
                _ => unreachable!(),
            })
        }
        BinOp::Range | BinOp::Intersect | BinOp::Union => Value::Error(XlError::Value),
    }
}

pub fn binary(ctx: &EvalCtx, op: BinOp, a: Value, b: Value) -> Value {
    map2(a, b, |x, y| scalar_binary(ctx, op, x, y, false))
}

pub fn binary_snapped(ctx: &EvalCtx, op: BinOp, a: Value, b: Value) -> Value {
    map2(a, b, |x, y| scalar_binary(ctx, op, x, y, true))
}

fn type_rank(v: &Value) -> u8 {
    match v {
        Value::Number(_) => 0,
        Value::Text(_) => 1,
        Value::Bool(_) => 2,
        _ => 3,
    }
}

pub fn compare(a: &Value, b: &Value) -> Ordering {
    let blank_like = |other: &Value| match other {
        Value::Number(_) | Value::Blank => Value::Number(0.0),
        Value::Text(_) => Value::empty_text(),
        Value::Bool(_) => Value::Bool(false),
        _ => Value::Number(0.0),
    };
    let (a, b) = match (a, b) {
        (Value::Blank, Value::Blank) => return Ordering::Equal,
        (Value::Blank, _) => (blank_like(b), b.clone()),
        (_, Value::Blank) => (a.clone(), blank_like(a)),
        _ => (a.clone(), b.clone()),
    };
    match (&a, &b) {
        (Value::Number(x), Value::Number(y)) => number::cmp_numbers(*x, *y),
        (Value::Text(x), Value::Text(y)) => text::compare_ci(x, y),
        (Value::Bool(x), Value::Bool(y)) => x.cmp(y),
        _ => type_rank(&a).cmp(&type_rank(&b)),
    }
}
