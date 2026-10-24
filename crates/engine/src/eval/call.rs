use super::{EvalCtx, Operand, Reference};
use crate::formula::Node;
use crate::functions::{self, FnDef, FuncId, Imp, ParamClass};
use crate::value::{Array, Value, XlError};

pub fn call_builtin(ctx: &mut EvalCtx, id: FuncId, args: &[Node]) -> Operand {
    let def = functions::def(id);
    let live = ctx.circ_live;
    let out = call_builtin_inner(ctx, def, args);
    if ctx.has_placeholders() {
        track_circ(ctx, def, live, &out);
    }
    out
}

const SELECTING: &[&str] = &[
    "INDEX",
    "ROWS",
    "COLUMNS",
    "TAKE",
    "DROP",
    "TRANSPOSE",
    "EXPAND",
    "SORT",
    "SORTBY",
    "IF",
    "IFS",
    "CHOOSE",
    "SWITCH",
    "LET",
];

fn track_circ(ctx: &mut EvalCtx, def: &FnDef, live: u32, out: &Operand) {
    let kept = match out {
        Operand::Val(v) => super::contains_circ(v),
        _ => false,
    };
    let consumed = ctx.circ_live > live || ctx.circ_args;
    let lazy = ctx.circ_lifted
        || SELECTING.contains(&def.name)
        || matches!(def.name, "IFERROR" | "IFNA" | "MATCH" | "BYROW" | "BYCOL");
    ctx.circ_args = false;
    ctx.circ_lifted = false;
    let early_error = matches!(out, Operand::Val(Value::Error(e)) if *e != XlError::Circ);
    if consumed && !lazy && !early_error {
        ctx.circ_used();
    }
    ctx.circ_live = live + kept as u32;
}

const PLAIN_EMPTY: &[&str] = &[
    "SORT",
    "VSTACK",
    "HSTACK",
    "UNIQUE",
    "FILTER",
    "CHOOSEROWS",
    "CHOOSECOLS",
    "MAP",
    "TOCOL",
    "TOROW",
];

fn call_builtin_inner(ctx: &mut EvalCtx, def: &'static FnDef, args: &[Node]) -> Operand {
    match def.imp {
        Imp::Lazy(f) => {
            let out = f(ctx, args);

            if matches!(def.name, "IF" | "IFS" | "CHOOSE" | "SWITCH") && span_multi(&out) {
                return Operand::err(XlError::Value);
            }
            out
        }
        Imp::Scalar(f) => {
            let mut ops = Vec::with_capacity(args.len());
            for a in args {
                let v = ctx.eval(a);
                ops.push(v);
            }
            if ctx.interrupted() {
                return Operand::err(XlError::Calc);
            }
            note_circ_args(ctx, &ops);
            call_lifted(ctx, def, ops, &mut |ctx, ops| {
                if long_text(ctx, def.name, ops) {
                    return Operand::err(XlError::Value);
                }
                let vals: Vec<Value> = ops
                    .iter()
                    .map(|o| match o {
                        Operand::Val(v) => v.clone(),
                        _ => Value::Blank,
                    })
                    .collect();
                Operand::Val(f(ctx, &vals))
            })
        }
        Imp::Args(f) => {
            let mut ops = Vec::with_capacity(args.len());
            for a in args {
                let v = ctx.eval(a);
                ops.push(v);
            }
            if ctx.interrupted() {
                return Operand::err(XlError::Calc);
            }
            note_circ_args(ctx, &ops);
            if let Some(e) = union_reject(def.name, &ops) {
                return Operand::err(e);
            }
            if let Some(r) = span_reject(def.name, &ops) {
                return r;
            }
            if let Some(r) = lambda_array_arg(ctx, def.name, &ops) {
                return r;
            }
            if lam_blank_reject(def.name, &ops) {
                return Operand::err(XlError::NA);
            }
            let plain =
                PLAIN_EMPTY.contains(&def.name) && ops.iter().any(|o| matches!(o, Operand::Ref(_)));
            let out = call_lifted(ctx, def, ops, &mut |ctx, ops| {
                if let Some(e) = lead_error(ctx, def, ops) {
                    return Operand::err(e);
                }
                if long_text(ctx, def.name, ops) {
                    return Operand::err(XlError::Value);
                }
                f(ctx, ops)
            });
            match out {
                Operand::Val(v) if plain => Operand::Val(v.plain_empty()),
                out => out,
            }
        }
    }
}

fn long_text(ctx: &mut EvalCtx, name: &str, ops: &[Operand]) -> bool {
    let idx: &[usize] = match name {
        "CODE" | "UNICODE" | "ASC" | "DBCS" | "VALUE" | "NUMBERVALUE" | "REPT" | "SEARCH"
        | "SEARCHB" | "N" | "T" | "MATCH" | "VLOOKUP" | "HLOOKUP" => &[0],
        _ => return false,
    };
    idx.iter().any(|&i| match ops.get(i) {
        Some(Operand::Val(Value::Text(t))) => crate::text::utf16_len(t) > 255,
        Some(Operand::Ref(r)) => match r.single() {
            Some(a) if a.rect.is_cell() => matches!(
                ctx.cell_value(a.sheet, a.rect.r0, a.rect.c0),
                Value::Text(t) if crate::text::utf16_len(&t) > 255
            ),
            _ => false,
        },
        _ => false,
    })
}

fn lambda_array_arg(ctx: &mut EvalCtx, name: &str, ops: &[Operand]) -> Option<Operand> {
    let arr_at = match name {
        "BYROW" | "BYCOL" | "MAP" => 0,
        "REDUCE" | "SCAN" => 1,
        _ => return None,
    };
    if matches!(ops.get(arr_at), Some(Operand::Missing)) {
        return Some(Operand::err(XlError::Value));
    }
    if name == "MAP"
        && ops.len() >= 2
        && ops.last().is_some_and(|l| match l {
            Operand::Val(Value::Array(_)) => true,
            Operand::Ref(r) => matches!(r.single(), Some(a) if !a.rect.is_cell()),
            _ => false,
        })
    {
        return Some(Operand::err(XlError::Value));
    }
    let last = ops.last()?;
    let multi = match last {
        Operand::Val(Value::Array(_)) => true,
        Operand::Ref(r) => matches!(r.single(), Some(a) if !a.rect.is_cell()),
        _ => false,
    };
    if !multi || ops.len() < 2 {
        return None;
    }
    let Value::Array(a) = ctx.deref(last.clone()) else {
        return None;
    };
    let data = a
        .data
        .iter()
        .map(|v| match v {
            Value::Error(e) => Value::Error(*e),
            _ => Value::Error(XlError::Value),
        })
        .collect();
    Some(Operand::Val(Array::new(a.rows, a.cols, data).into_value()))
}

fn span_multi(op: &Operand) -> bool {
    matches!(op, Operand::Ref(r) if r.is_span()
        && r.areas().first().is_some_and(|a| !a.rect.is_cell()))
}

fn span_reject(name: &str, ops: &[Operand]) -> Option<Operand> {
    if !ops.iter().any(span_multi) {
        return None;
    }
    let at = |i: usize| ops.get(i).is_some_and(span_multi);
    let e = match name {
        "ISREF" => return Some(Operand::Val(Value::Bool(false))),
        "CELL" if at(0) => XlError::Ref,
        "ROW" | "COLUMN" | "ROWS" | "COLUMNS" | "AREAS" | "CELL" | "ISFORMULA" | "FORMULATEXT"
        | "PHONETIC" | "N" | "T" | "IMSUM" | "IMPRODUCT" | "LINEST" | "LOGEST" | "TREND"
        | "GROWTH" | "MAP" => XlError::Value,

        "MATCH" | "XMATCH" | "VLOOKUP" | "HLOOKUP" | "LOOKUP" | "XLOOKUP" => {
            let arrays: &[usize] = match name {
                "LOOKUP" | "XLOOKUP" => &[1, 2],
                "MATCH" | "VLOOKUP" | "HLOOKUP" => &[1, 2],
                _ => &[1],
            };
            if arrays.iter().any(|&i| at(i)) {
                XlError::Value
            } else if name == "XLOOKUP" && at(3) && !(0..ops.len()).any(|i| i != 3 && at(i)) {
                return None;
            } else {
                XlError::Ref
            }
        }
        "SUMX2MY2" | "SUMX2PY2" | "SUMXMY2" | "MDETERM" | "MINVERSE" | "MMULT" => XlError::Ref,
        "MODE" | "MODE.SNGL" | "MODE.MULT" => XlError::Ref,
        "BYROW" | "BYCOL" | "REDUCE" | "SCAN" => {
            if at(ops.len() - 1) {
                XlError::Ref
            } else {
                XlError::Value
            }
        }
        _ => return None,
    };
    Some(Operand::err(e))
}

fn is_union(op: &Operand) -> bool {
    matches!(op, Operand::Ref(r @ Reference::Multi(_)) if r.single().is_none())
}

fn union_reject(name: &str, ops: &[Operand]) -> Option<XlError> {
    let any = ops.iter().any(is_union);
    if !any {
        return None;
    }
    match name {
        "CONCAT" | "TEXTJOIN" | "GCD" | "LCM" | "MULTINOMIAL" | "SERIESSUM" | "IMSUM"
        | "IMPRODUCT" => Some(XlError::Value),

        "CHOOSEROWS" | "CHOOSECOLS"
            if ops[1..]
                .iter()
                .any(|o| is_union(o) || matches!(o, Operand::Val(Value::Text(_)))) =>
        {
            Some(XlError::Value)
        }

        "FILTER" if !ops[..2].iter().any(is_union) => None,
        "CHOOSEROWS" | "CHOOSECOLS" | "FILTER" => Some(XlError::Ref),

        "TREND" | "GROWTH" => Some(if ops.iter().take(3).any(is_union) {
            XlError::Ref
        } else {
            XlError::Value
        }),
        "LINEST" | "LOGEST" => Some(if ops.iter().take(2).any(is_union) {
            XlError::Ref
        } else {
            XlError::Value
        }),
        "MODE" | "MODE.SNGL" | "MODE.MULT" => Some(XlError::Value),
        _ => None,
    }
}

fn lam_blank_reject(name: &str, ops: &[Operand]) -> bool {
    let flagged = || {
        ops.iter()
            .any(|o| matches!(o, Operand::Val(Value::Array(a)) if a.lam_blank))
    };
    match name {
        "SUM" | "MIN" | "MAX" | "AVERAGE" | "PRODUCT" | "LARGE" | "SMALL" | "MEDIAN" | "STDEV"
        | "STDEV.S" | "STDEV.P" | "STDEVP" | "VAR" | "VAR.S" | "VAR.P" | "VARP" | "SUMSQ"
        | "MODE" | "MODE.SNGL" | "GEOMEAN" | "HARMEAN" | "DEVSQ" | "AVEDEV" | "KURT" | "SKEW"
        | "MINA" | "MAXA" | "AVERAGEA" | "XOR" => flagged(),
        "AND" | "OR" => {
            let want = name == "OR";
            for o in ops {
                let items: &[Value] = match o {
                    Operand::Val(Value::Array(a)) => &a.data,
                    Operand::Val(v) => std::slice::from_ref(v),
                    _ => continue,
                };
                let lam = matches!(o, Operand::Val(Value::Array(a)) if a.lam_blank);
                for v in items {
                    match v {
                        Value::Blank if lam => return true,
                        Value::Number(n) if (*n != 0.0) == want => return false,
                        Value::Bool(b) if *b == want => return false,
                        Value::Error(_) => return false,
                        _ => {}
                    }
                }
            }
            false
        }
        _ => false,
    }
}

fn note_circ_args(ctx: &mut EvalCtx, ops: &[Operand]) {
    if ctx.has_placeholders() {
        ctx.circ_args = ops
            .iter()
            .any(|o| matches!(o, Operand::Val(v) if super::contains_circ(v)));
    }
}

fn blank_as_zero(v: Value) -> Value {
    match v {
        Value::Blank => Value::Number(0.0),
        Value::Array(a) if a.data.iter().any(|e| matches!(e, Value::Blank)) => {
            let data = a
                .data
                .iter()
                .map(|e| match e {
                    Value::Blank => Value::Number(0.0),
                    e => e.clone(),
                })
                .collect();
            Array::new(a.rows, a.cols, data).into_value()
        }
        v => v,
    }
}

pub fn call_lifted(
    ctx: &mut EvalCtx,
    def: &FnDef,
    mut ops: Vec<Operand>,
    body: &mut dyn FnMut(&mut EvalCtx, &[Operand]) -> Operand,
) -> Operand {
    let mut shape: Option<(u32, u32)> = None;
    let mut lifted: Vec<usize> = Vec::new();
    for i in 0..ops.len() {
        let class = def.param_class(i);
        if class == ParamClass::Reference {
            if let Operand::Val(Value::Array(a)) = &ops[i] {
                let (r, c) = shape.unwrap_or((1, 1));
                shape = Some((r.max(a.rows), c.max(a.cols)));
                lifted.push(i);
            }

            if let Operand::Ref(Reference::Grid(g)) = &ops[i] {
                let (r, c) = shape.unwrap_or((1, 1));
                shape = Some((r.max(g.rows), c.max(g.cols)));
                lifted.push(i);
            }
            continue;
        }
        if class != ParamClass::Value {
            continue;
        }
        if ops[i].is_missing() {
            continue;
        }
        let op = std::mem::replace(&mut ops[i], Operand::Missing);
        let v = ctx.deref(op);
        let v = if def.param_letter(i) == b'B' {
            blank_as_zero(v)
        } else {
            v
        };
        if let Value::Array(a) = &v {
            let (r, c) = shape.unwrap_or((1, 1));
            shape = Some((r.max(a.rows), c.max(a.cols)));
            lifted.push(i);
        }
        ops[i] = Operand::Val(v);
    }
    if ctx.interrupted() {
        return Operand::err(XlError::Calc);
    }
    let Some((rows, cols)) = shape else {
        return body(ctx, &ops);
    };
    let circ = ctx.has_placeholders()
        && lifted
            .iter()
            .any(|&i| matches!(&ops[i], Operand::Val(v) if super::contains_circ(v)));
    ctx.circ_lifted |= circ;
    let mut data = Vec::with_capacity(rows as usize * cols as usize);
    let mut elem = ops.clone();
    for r in 0..rows {
        for c in 0..cols {
            for &i in &lifted {
                match &ops[i] {
                    Operand::Val(Value::Array(a)) => elem[i] = Operand::Val(a.broadcast_get(r, c)),
                    Operand::Ref(Reference::Grid(g)) => elem[i] = g.broadcast_get(r, c),
                    _ => {}
                }
            }
            if circ
                && lifted
                    .iter()
                    .any(|&i| matches!(&elem[i], Operand::Val(Value::Error(XlError::Circ))))
            {
                data.push(Value::Error(XlError::Circ));
                continue;
            }
            let out = body(ctx, &elem);
            let mut v = match ctx.deref_marked(out) {
                Value::Array(a) => a.data[0].clone(),
                other => other,
            };

            if matches!(&v, Value::Text(t) if t.is_empty())
                && lifted
                    .iter()
                    .any(|&i| matches!(&elem[i], Operand::Val(e) if e.is_cell_empty()))
            {
                v = Value::Text(crate::value::cell_empty());
            }
            data.push(v);
            if ctx.interrupted() {
                return Operand::err(XlError::Calc);
            }
        }
    }
    Operand::Val(Array::new(rows, cols, data).into_value())
}

fn lead_error(ctx: &EvalCtx, def: &FnDef, ops: &[Operand]) -> Option<XlError> {
    if !def.params.bytes().any(|b| matches!(b, b'E' | b'X' | b'M')) {
        return None;
    }
    for (i, op) in ops.iter().enumerate() {
        let letter = def.param_letter(i);
        if !matches!(letter, b'E' | b'X' | b'M') {
            continue;
        }
        match op {
            Operand::Val(Value::Error(e)) => return Some(*e),
            Operand::Missing if letter == b'M' => return Some(XlError::Value),
            Operand::Val(v @ Value::Text(_)) if letter == b'X' && ctx.to_number(v).is_err() => {
                return Some(XlError::Value)
            }
            _ => {}
        }
    }
    None
}
