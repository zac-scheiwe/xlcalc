use std::cmp::Ordering;

use super::args::*;
use super::{args, checked, lazy, scalar, FnDef, MANY, MANY254};
use crate::coerce;
use crate::eval::ops::compare;
use crate::eval::{EvalCtx, Operand};
use crate::formula::Node;
use crate::parser::ast::Expr;
use crate::value::{Array, Value, XlError};

pub const DEFS: &[FnDef] = &[
    lazy("IF", 2, 3, if_fn),
    checked(lazy("IFS", 2, MANY, ifs), ifs_pairs),
    lazy("IFERROR", 2, 2, |c, a| iferror(c, a, |_| true)),
    lazy("IFNA", 2, 2, |c, a| iferror(c, a, |e| e == XlError::NA)),
    lazy("SWITCH", 3, MANY254, switch),
    args("AND", 1, MANY, "R", |c, o| logical_fold(c, o, Fold::And)),
    args("OR", 1, MANY, "R", |c, o| logical_fold(c, o, Fold::Or)),
    args("XOR", 1, MANY254, "R", |c, o| logical_fold(c, o, Fold::Xor)),
    scalar("NOT", 1, 1, |_, a| {
        val(boolean(&a[0]).map(|b| Value::Bool(!b)))
    }),
    scalar("TRUE", 0, 0, |_, _| Value::Bool(true)),
    scalar("FALSE", 0, 0, |_, _| Value::Bool(false)),
];

fn branch(ctx: &mut EvalCtx, args: &[Node], i: usize, absent: Value) -> Operand {
    match args.get(i) {
        None => Operand::Val(absent),
        Some(Node::Missing) => Operand::Val(Value::Number(0.0)),
        Some(n) => ctx.eval(n),
    }
}

fn branch_blank(ctx: &mut EvalCtx, args: &[Node], i: usize) -> Operand {
    match args.get(i) {
        Some(Node::Missing) => Operand::Missing,
        _ => branch(ctx, args, i, Value::Bool(false)),
    }
}

fn select3(cond: &Array, t: &Value, f: &Value) -> Value {
    let dims = |v: &Value| match v {
        Value::Array(a) => (a.rows, a.cols),
        _ => (1, 1),
    };
    let (tr, tc) = dims(t);
    let (fr, fc) = dims(f);
    let rows = cond.rows.max(tr).max(fr);
    let cols = cond.cols.max(tc).max(fc);

    let get = |v: &Value, r: u32, c: u32| match v {
        Value::Array(a) => a.broadcast_get(r, c),
        other => other.clone(),
    };
    let get = move |v: &Value, r: u32, c: u32| match get(v, r, c) {
        Value::Blank => Value::Number(0.0),
        other => other,
    };
    Array::from_fn(rows, cols, |r, c| {
        let cv = cond.broadcast_get(r, c);
        match coerce::to_bool(&cv) {
            Ok(true) => get(t, r, c),
            Ok(false) => get(f, r, c),
            Err(e) => Value::Error(e),
        }
    })
    .into_value()
}

fn if_fn(ctx: &mut EvalCtx, args: &[Node]) -> Operand {
    let c = ctx.eval(&args[0]);
    let c = ctx.deref(c);
    match c {
        Value::Array(a) => {
            let any_true = a.data.iter().any(|v| coerce::to_bool(v) == Ok(true));
            let any_false = a.data.iter().any(|v| coerce::to_bool(v) == Ok(false));
            let t = if any_true {
                branch(ctx, args, 1, Value::Bool(false))
            } else {
                let o = branch(ctx, args, 1, Value::Blank);
                match ctx.deref(o) {
                    Value::Array(a) => Operand::Val(
                        Array::new(a.rows, a.cols, vec![Value::Blank; a.data.len()]).into_value(),
                    ),
                    _ => Operand::Val(Value::Blank),
                }
            };
            let t = ctx.deref(t);
            let f = if any_false {
                branch(ctx, args, 2, Value::Bool(false))
            } else {
                let o = branch(ctx, args, 2, Value::Blank);
                match ctx.deref(o) {
                    Value::Array(a) => Operand::Val(
                        Array::new(a.rows, a.cols, vec![Value::Blank; a.data.len()]).into_value(),
                    ),
                    _ => Operand::Val(Value::Blank),
                }
            };
            let f = ctx.deref(f);
            Operand::Val(select3(&a, &t, &f))
        }
        v => match coerce::to_bool(&v) {
            Ok(true) => branch_blank(ctx, args, 1),
            Ok(false) => branch_blank(ctx, args, 2),
            Err(e) => Operand::err(e),
        },
    }
}

fn ifs_pairs(a: &[Expr]) -> Result<(), &'static str> {
    if a.len() % 2 == 0 {
        Ok(())
    } else {
        Err("needs condition/value pairs")
    }
}

fn dims_of(v: &Value) -> (u32, u32) {
    match v {
        Value::Array(a) => (a.rows, a.cols),
        _ => (1, 1),
    }
}

fn elem(v: &Value, r: u32, c: u32) -> Value {
    match v {
        Value::Array(a) => match a.broadcast_get(r, c) {
            Value::Blank => Value::Number(0.0),
            x => x,
        },
        Value::Blank => Value::Number(0.0),
        other => other.clone(),
    }
}

fn ifs(ctx: &mut EvalCtx, args: &[Node]) -> Operand {
    let mut lifted: Vec<(Value, Value)> = Vec::new();
    let mut tail: Option<Value> = None;
    let mut tail_err: Option<XlError> = None;

    let mut shapes: Vec<Value> = Vec::new();
    let mut stop = args.len().div_ceil(2);
    for (pi, pair) in args.chunks(2).enumerate() {
        let c = ctx.eval(&pair[0]);
        let c = ctx.deref(c);
        match &c {
            Value::Array(_) => {}

            Value::Text(_) if coerce::to_bool(&c).is_err() => {}
            Value::Error(e) => {
                tail_err = Some(*e);
                stop = pi + 1;
                if !lifted.is_empty() {
                    let v = branch(ctx, pair, 1, Value::Number(0.0));
                    shapes.push(ctx.deref(v));
                }
                break;
            }
            v => match coerce::to_bool(v) {
                Ok(true) => {
                    let o = branch(ctx, pair, 1, Value::Number(0.0));
                    if lifted.is_empty() {
                        return o;
                    }
                    tail = Some(ctx.deref(o));
                    stop = pi + 1;
                    break;
                }
                Ok(false) => {
                    if !lifted.is_empty() {
                        let v = branch(ctx, pair, 1, Value::Number(0.0));
                        shapes.push(ctx.deref(v));
                    }
                    continue;
                }
                Err(e) => {
                    tail_err = Some(e);
                    stop = pi + 1;
                    break;
                }
            },
        }
        let v = branch(ctx, pair, 1, Value::Number(0.0));
        let v = ctx.deref(v);
        lifted.push((c, v));
    }
    if lifted.is_empty() {
        return Operand::err(tail_err.unwrap_or(XlError::NA));
    }

    for pair in args.chunks(2).skip(stop) {
        for node in pair {
            let o = ctx.eval(node);
            shapes.push(ctx.deref(o));
        }
    }
    let (mut rows, mut cols) = (1, 1);
    for (c, v) in &lifted {
        for x in [c, v] {
            let (r, k) = dims_of(x);
            rows = rows.max(r);
            cols = cols.max(k);
        }
    }
    for t in tail.iter().chain(shapes.iter()) {
        let (r, k) = dims_of(t);
        rows = rows.max(r);
        cols = cols.max(k);
    }
    Operand::Val(
        Array::from_fn(rows, cols, |r, c| {
            for (cv, vv) in &lifted {
                match coerce::to_bool(&elem(cv, r, c)) {
                    Ok(true) => return elem(vv, r, c),
                    Ok(false) => {}
                    Err(e) => return Value::Error(e),
                }
            }
            match (&tail, tail_err) {
                (Some(t), _) => elem(t, r, c),
                (None, Some(e)) => Value::Error(e),
                (None, None) => Value::Error(XlError::NA),
            }
        })
        .into_value(),
    )
}

fn iferror(ctx: &mut EvalCtx, args: &[Node], catches: fn(XlError) -> bool) -> Operand {
    let catches = |e: XlError| e != XlError::Circ && catches(e);
    let v = branch(ctx, args, 0, Value::Number(0.0));
    let v = ctx.deref(v);
    match v {
        Value::Error(e) if catches(e) => {
            let alt = branch_blank(ctx, args, 1);
            if alt.is_missing() {
                return alt;
            }
            let alt = ctx.deref(alt);
            Operand::Val(alt)
        }
        Value::Array(a) => {
            let alt = branch(ctx, args, 1, Value::Number(0.0));
            let alt = ctx.deref(alt);
            let alt_get = |r: u32, c: u32| match &alt {
                Value::Array(b) => match b.broadcast_get(r, c) {
                    Value::Blank => Value::Number(0.0),
                    x => x,
                },
                Value::Blank => Value::Number(0.0),
                other => other.clone(),
            };
            let (rows, cols) = match &alt {
                Value::Array(b) => (a.rows.max(b.rows), a.cols.max(b.cols)),
                _ => (a.rows, a.cols),
            };
            Operand::Val(
                Array::from_fn(rows, cols, |r, c| match a.broadcast_get(r, c) {
                    Value::Error(e) if catches(e) => alt_get(r, c),
                    Value::Blank => Value::Number(0.0),
                    other => other,
                })
                .into_value(),
            )
        }
        other => Operand::Val(other),
    }
}

fn switch_equal(a: &Value, b: &Value) -> bool {
    let z = Value::Number(0.0);
    let a = if a.is_blank() { &z } else { a };
    let b = if b.is_blank() { &z } else { b };
    compare(a, b) == Ordering::Equal
}

fn switch(ctx: &mut EvalCtx, args: &[Node]) -> Operand {
    let target = ctx.eval(&args[0]);
    let target = ctx.deref(target);
    if let Value::Error(e) = target {
        return Operand::err(e);
    }
    let rest = &args[1..];
    let pairs = rest.len() / 2;
    let has_default = rest.len() % 2 == 1;
    let mut lifted: Vec<(Value, Value)> = Vec::new();
    let mut tail: Option<Value> = None;
    let target_is_array = matches!(target, Value::Array(_));
    let mut matched = false;
    let mut shapes: Vec<Value> = Vec::new();
    let mut stop = pairs;
    for i in 0..pairs {
        let v = ctx.eval(&rest[2 * i]);
        let v = ctx.deref(v);
        let case_is_array = matches!(v, Value::Array(_));
        if !target_is_array && !case_is_array {
            if let Value::Error(e) = v {
                if lifted.is_empty() {
                    return Operand::err(e);
                }
                let o = branch(ctx, rest, 2 * i + 1, Value::Number(0.0));
                shapes.push(ctx.deref(o));
                tail = Some(Value::Error(e));
                matched = true;
                stop = i + 1;
                break;
            }
            if !switch_equal(&target, &v) {
                let o = branch(ctx, rest, 2 * i + 1, Value::Number(0.0));
                shapes.push(ctx.deref(o));
                continue;
            }
            let o = branch(ctx, rest, 2 * i + 1, Value::Number(0.0));
            if lifted.is_empty() {
                return o;
            }
            tail = Some(ctx.deref(o));
            matched = true;
            stop = i + 1;
            break;
        }
        let r = branch(ctx, rest, 2 * i + 1, Value::Number(0.0));
        let r = ctx.deref(r);
        lifted.push((v, r));
    }
    if !lifted.is_empty() {
        for i in stop..pairs {
            for k in [2 * i, 2 * i + 1] {
                let o = branch(ctx, rest, k, Value::Number(0.0));
                shapes.push(ctx.deref(o));
            }
        }
        if matched && has_default {
            let o = branch(ctx, rest, rest.len() - 1, Value::Number(0.0));
            shapes.push(ctx.deref(o));
        }
    }
    if lifted.is_empty() {
        if has_default {
            return branch(ctx, rest, rest.len() - 1, Value::Number(0.0));
        }
        return Operand::err(XlError::NA);
    }
    if !matched && has_default {
        let o = branch(ctx, rest, rest.len() - 1, Value::Number(0.0));
        tail = Some(ctx.deref(o));
    }
    let (mut rows, mut cols) = dims_of(&target);
    for (c, v) in &lifted {
        for x in [c, v] {
            let (r, k) = dims_of(x);
            rows = rows.max(r);
            cols = cols.max(k);
        }
    }
    for t in tail.iter().chain(shapes.iter()) {
        let (r, k) = dims_of(t);
        rows = rows.max(r);
        cols = cols.max(k);
    }
    Operand::Val(
        Array::from_fn(rows, cols, |r, c| {
            let t = elem(&target, r, c);
            if let Value::Error(e) = t {
                return Value::Error(e);
            }
            for (cv, vv) in &lifted {
                let cv = elem(cv, r, c);
                if let Value::Error(e) = cv {
                    return Value::Error(e);
                }
                if switch_equal(&t, &cv) {
                    return elem(vv, r, c);
                }
            }
            match &tail {
                Some(t) => elem(t, r, c),
                None => Value::Error(XlError::NA),
            }
        })
        .into_value(),
    )
}

#[derive(Clone, Copy)]
enum Fold {
    And,
    Or,
    Xor,
}

fn logical_fold(ctx: &mut EvalCtx, ops: &[Operand], fold: Fold) -> Operand {
    let mut seen = false;
    let mut trues = 0usize;
    let mut falses = 0usize;
    let mut failure: Option<XlError> = None;
    let mut take = |v: &Value, direct: bool, failure: &mut Option<XlError>| {
        if failure.is_some() {
            return;
        }
        match v {
            Value::Bool(b) => {
                seen = true;
                if *b {
                    trues += 1
                } else {
                    falses += 1
                }
            }
            Value::Number(n) => {
                seen = true;
                if *n != 0.0 {
                    trues += 1
                } else {
                    falses += 1
                }
            }
            Value::Error(e) => *failure = Some(*e),
            Value::Text(s) if direct => match coerce::to_bool(&Value::Text(s.clone())) {
                Ok(b) => {
                    seen = true;
                    if b {
                        trues += 1
                    } else {
                        falses += 1
                    }
                }

                Err(_) => {}
            },
            _ => {}
        }
    };

    for op in ops {
        if let Operand::Val(Value::Error(e)) = op {
            return Operand::err(*e);
        }
    }
    for op in ops {
        match op {
            Operand::Missing => take(&Value::Bool(false), true, &mut failure),
            Operand::Val(Value::Array(a)) => {
                for v in &a.data {
                    take(v, false, &mut failure);
                }
            }
            Operand::Val(v) => take(v, true, &mut failure),
            Operand::Ref(r) => {
                for area in r.areas() {
                    ctx.visit_area(area, |_, _, v| take(v, false, &mut failure));
                }
            }
        }
        if failure.is_some() {
            break;
        }
    }
    if let Some(e) = failure {
        return Operand::err(e);
    }
    if !seen {
        return Operand::err(XlError::Value);
    }
    Operand::Val(Value::Bool(match fold {
        Fold::And => falses == 0,
        Fold::Or => trues > 0,
        Fold::Xor => trues % 2 == 1,
    }))
}
