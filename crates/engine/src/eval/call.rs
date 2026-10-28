use super::{EvalCtx, Operand, Reference};
use crate::formula::Node;
use crate::functions::{self, FnDef, FuncId, Imp, ParamClass};
use crate::value::{Array, Value, XlError};

pub fn call_builtin(ctx: &mut EvalCtx, id: FuncId, args: &[Node]) -> Operand {
    let def = functions::def(id);
    let live = ctx.circ_live;
    let saved = ctx.arr_ctx;
    match def.name {
        "ROWS" | "COLUMNS" => {
            ctx.arr_ctx = if saved == 2 { 2 } else { 1 };
            ctx.arr_direct = true;
        }
        n if ARR_BREAK.contains(&n) => ctx.arr_ctx = 0,
        _ => {}
    }
    let out = call_builtin_inner(ctx, def, args);
    ctx.arr_ctx = saved;
    if ctx.has_placeholders() {
        track_circ(ctx, def, live, &out);
    }
    out
}

const ARR_BREAK: &[&str] = &[
    "IF",
    "IFS",
    "IFERROR",
    "IFNA",
    "CHOOSE",
    "SWITCH",
    "VLOOKUP",
    "HLOOKUP",
    "OFFSET",
    "TRANSPOSE",
];

fn arr_class(ctx: &EvalCtx, name: &str, i: usize) -> u8 {
    match (name, i) {
        ("INDEX", 1..) | ("XLOOKUP", 0) => 0,
        _ => ctx.arr_ctx,
    }
}

fn array_slot(name: &str, i: usize) -> bool {
    match name {
        "VSTACK" | "HSTACK" => true,
        "MAP" => true,
        "TAKE" | "DROP" | "EXPAND" | "CHOOSEROWS" | "CHOOSECOLS" | "TOCOL" | "TOROW"
        | "WRAPROWS" | "WRAPCOLS" | "SORT" | "SORTBY" | "UNIQUE" | "FILTER" => i == 0,
        _ => false,
    }
}

fn eval_arg(ctx: &mut EvalCtx, name: &str, i: usize, a: &Node) -> Operand {
    if ctx.arr_ctx == 0 {
        if !array_slot(name, i) {
            return ctx.eval(a);
        }
        ctx.arr_ctx = 3;
        let v = ctx.eval(a);
        ctx.arr_ctx = 0;
        return v;
    }
    let direct = ctx.arr_direct;
    let class = arr_class(ctx, name, i);
    let saved = std::mem::replace(&mut ctx.arr_ctx, class);
    ctx.arr_direct = direct;
    let v = ctx.eval(a);
    ctx.arr_ctx = saved;
    v
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
            for (i, a) in args.iter().enumerate() {
                let v = eval_arg(ctx, def.name, i, a);
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
            if let Some(r) = ghost_ref_call(ctx, def.name, args) {
                return r;
            }
            let mut ops = Vec::with_capacity(args.len());
            for (i, a) in args.iter().enumerate() {
                let v = eval_arg(ctx, def.name, i, a);
                ops.push(v);
            }
            if ctx.interrupted() {
                return Operand::err(XlError::Calc);
            }
            note_circ_args(ctx, &ops);

            if def.name == "SUBTOTAL" {
                if let Some(Operand::Val(Value::Error(e))) = ops
                    .iter()
                    .skip(1)
                    .find(|o| matches!(o, Operand::Val(Value::Error(_))))
                {
                    let e = *e;
                    let one = |ctx: &EvalCtx, v: &Value| match ctx.to_number(v) {
                        Ok(_) => Value::Error(e),
                        Err(x) => Value::Error(x),
                    };
                    let f0 = match &ops[0] {
                        Operand::Ref(_) => ctx.deref(ops[0].clone()),
                        Operand::Val(v) => v.clone(),
                        Operand::Missing => Value::Blank,
                    };
                    return Operand::Val(match &f0 {
                        Value::Array(a) => {
                            let data = a.data.iter().map(|v| one(ctx, v)).collect();
                            Array::new(a.rows, a.cols, data).into_value()
                        }
                        v => one(ctx, v),
                    });
                }
            }
            if let Some(r) = grid_args(ctx, def.name, &mut ops) {
                return r;
            }
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
            if blank_value_reject(def.name, &ops) {
                if def.name == "INDEX" {
                    let (mut r, mut c) = (0, 0);
                    for o in &ops[1..] {
                        if let Operand::Val(Value::Array(a)) = o {
                            r = r.max(a.rows);
                            c = c.max(a.cols);
                        }
                    }
                    if r > 0 {
                        return Operand::Val(
                            Array::filled(r, c, Value::Error(XlError::Value)).into_value(),
                        );
                    }
                }
                return Operand::err(XlError::Value);
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
        | "SEARCHB" | "N" | "T" | "MATCH" | "VLOOKUP" | "HLOOKUP" | "LOOKUP" => &[0],
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

fn ghost_ref_call(ctx: &mut EvalCtx, name: &str, args: &[Node]) -> Option<Operand> {
    let at = if name == "CELL" { 1 } else { 0 };
    let Some(Node::Ref(crate::formula::SheetRef::Missing(_), cref)) = args.get(at) else {
        return None;
    };
    let rect = super::refs::resolve_rect(cref, ctx.host, ctx.wrap)?;
    let v = match name {
        "ROW" | "COLUMN" => {
            let row = name == "ROW";
            let (lo, n) = if row {
                (rect.r0, rect.rows())
            } else {
                (rect.c0, rect.cols())
            };
            if n == 1 {
                Value::Number(lo as f64 + 1.0)
            } else {
                let data = (0..n)
                    .map(|i| Value::Number((lo + i) as f64 + 1.0))
                    .collect();
                let (r, c) = if row { (n, 1) } else { (1, n) };
                Array::new(r, c, data).into_value()
            }
        }
        "AREAS" => Value::Number(1.0),
        "OFFSET" => Value::Error(XlError::Value),
        "CELL" => Value::Error(XlError::NA),
        _ => return None,
    };
    Some(Operand::Val(v))
}

fn span_multi(op: &Operand) -> bool {
    matches!(op, Operand::Ref(r) if r.is_span())
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

fn grid_top_left(ctx: &mut EvalCtx, g: &super::RefGrid) -> Value {
    let data = g
        .items
        .iter()
        .map(|i| match i {
            Ok(a) => ctx.cell_value(a.sheet, a.rect.r0, a.rect.c0),
            Err(e) => Value::Error(*e),
        })
        .collect();
    Array::new(g.rows, g.cols, data).into_value()
}

fn grid_args(ctx: &mut EvalCtx, name: &str, ops: &mut [Operand]) -> Option<Operand> {
    let is_grid = |o: &Operand| matches!(o, Operand::Ref(Reference::Grid(_)));
    if !ops.iter().any(is_grid) {
        return None;
    }
    for o in ops.iter_mut() {
        if let Operand::Ref(Reference::Grid(g)) = o {
            if g.all_errors() {
                *o = Operand::Val(Value::Array(g.value_array()));
            }
        }
    }
    let at = |ops: &[Operand], idx: &[usize]| idx.iter().any(|&i| ops.get(i).is_some_and(is_grid));
    let any = ops.iter().any(is_grid);
    let e = match name {
        "AND" | "OR" | "XOR" | "MEDIAN" | "MODE" | "MODE.SNGL" | "MODE.MULT" if any => {
            match ops.iter().find_map(|o| match o {
                Operand::Val(Value::Error(e)) => Some(*e),
                _ => None,
            }) {
                Some(e) => e,
                None => XlError::NA,
            }
        }

        "LARGE" | "SMALL" | "PERCENTILE" | "PERCENTILE.INC" | "PERCENTILE.EXC" | "QUARTILE"
        | "QUARTILE.INC" | "QUARTILE.EXC"
            if at(ops, &[0]) =>
        {
            let k = match ops.get(1) {
                Some(o @ Operand::Ref(_)) => ctx.deref(o.clone()),
                Some(Operand::Val(v)) => v.clone(),
                _ => Value::Blank,
            };
            if let Value::Array(a) = k {
                return Some(Operand::Val(
                    Array::new(
                        a.rows,
                        a.cols,
                        vec![Value::Error(XlError::NA); a.data.len()],
                    )
                    .into_value(),
                ));
            }
            XlError::NA
        }
        "CONCAT" | "MMULT" if any => XlError::Value,
        "TEXTJOIN" if ops.iter().skip(2).any(is_grid) => XlError::Value,
        "ARRAYTOTEXT" if at(ops, &[0]) => XlError::Calc,
        "CORREL" if any => XlError::Num,

        "MATCH" | "XMATCH" | "XLOOKUP" | "LOOKUP" | "VLOOKUP" | "HLOOKUP" => {
            for o in ops.iter_mut() {
                if let Operand::Ref(Reference::Grid(g)) = o {
                    *o = Operand::Val(Value::Array(g.value_array()));
                }
            }
            return None;
        }
        "FREQUENCY" | "N" | "T" | "MAP" | "SCAN" | "REDUCE" => {
            let idx: Vec<usize> = match name {
                "FREQUENCY" | "N" | "T" => vec![0],
                "SCAN" | "REDUCE" => vec![1],
                _ => (0..ops.len().saturating_sub(1)).collect(),
            };
            for i in idx {
                let Some(Operand::Ref(Reference::Grid(g))) = ops.get(i) else {
                    continue;
                };
                let g = g.clone();
                if !matches!(name, "FREQUENCY" | "N" | "T")
                    && g.items
                        .iter()
                        .any(|a| matches!(a, Ok(a) if !a.rect.is_cell()))
                {
                    return Some(Operand::err(XlError::Calc));
                }
                ops[i] = Operand::Val(grid_top_left(ctx, &g));
            }
            return None;
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

fn blank_value_reject(name: &str, ops: &[Operand]) -> bool {
    let at: &[usize] = match name {
        "TOROW" | "TOCOL" | "TAKE" | "DROP" | "VSTACK" | "HSTACK" | "CHOOSEROWS" | "CHOOSECOLS"
        | "WRAPROWS" | "WRAPCOLS" | "ROWS" | "COLUMNS" | "ARRAYTOTEXT" | "TEXTSPLIT" | "MMULT"
        | "INDEX" | "FILTER" | "MAP" | "BYROW" | "BYCOL" => &[0],
        "FREQUENCY" => &[0, 1],

        "LOOKUP" => &[1],
        _ => return false,
    };
    at.iter()
        .any(|&i| matches!(ops.get(i), Some(Operand::Val(Value::Blank))))
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

            if def.name == "MAKEARRAY" {
                for &i in &lifted {
                    if let Operand::Val(Value::Number(n)) = elem[i] {
                        if n.fract() == 0.0 && n >= 1.0 && n < 4294967295.0 {
                            elem[i] = Operand::Val(Value::Number(1.0));
                        }
                    }
                }
            }
            let out = body(ctx, &elem);

            if def.name == "INDEX"
                && matches!(out, Operand::Ref(_))
                && matches!(ops.first(), Some(Operand::Ref(Reference::Grid(_))))
            {
                data.push(Value::Error(XlError::Value));
                continue;
            }
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
