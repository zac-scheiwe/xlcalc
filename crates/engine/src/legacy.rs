use crate::address::CellAddr;
use crate::calc::RecalcReport;
use crate::functions;
use crate::input::{self, Entered};
use crate::parser::ast::{BinOp, Expr, RefKind, UnaryOp};
use crate::parser::{parse, print::print};
use crate::workbook::{EditError, Workbook};

#[derive(Clone, Copy, PartialEq)]
enum Ctx {
    Value,
    Ref,
    Array,
}

const ARRAY_ARGS: &[&str] = &[
    "SUMPRODUCT",
    "FILTER",
    "SORT",
    "SORTBY",
    "UNIQUE",
    "MMULT",
    "MDETERM",
    "MINVERSE",
    "FREQUENCY",
    "LINEST",
    "LOGEST",
    "TREND",
    "GROWTH",
];

const ARRAY_RESULT: &[&str] = &[
    "MAP",
    "BYROW",
    "BYCOL",
    "SCAN",
    "REDUCE",
    "MAKEARRAY",
    "INDEX",
    "OFFSET",
    "INDIRECT",
    "SEQUENCE",
    "RANDARRAY",
    "FILTER",
    "SORT",
    "SORTBY",
    "UNIQUE",
    "TRANSPOSE",
    "MMULT",
    "MINVERSE",
    "MUNIT",
    "FREQUENCY",
    "LINEST",
    "LOGEST",
    "TREND",
    "GROWTH",
    "FIELDVALUE",
    "FILTERXML",
    "MODE.MULT",
    "STOCKHISTORY",
    "IFS",
    "SWITCH",
    "CELL",
    "TAKE",
    "DROP",
    "CHOOSECOLS",
    "CHOOSEROWS",
    "VSTACK",
    "HSTACK",
    "TOCOL",
    "TOROW",
    "WRAPROWS",
    "WRAPCOLS",
    "EXPAND",
    "TEXTSPLIT",
];

const PASS_THROUGH: &[&str] = &["IF", "IFS", "CHOOSE", "SWITCH", "IFERROR", "IFNA", "LET"];

const NO_LIFT: &[&str] = &[
    "LET",
    "LAMBDA",
    "MAP",
    "SCAN",
    "REDUCE",
    "BYROW",
    "BYCOL",
    "MAKEARRAY",
    "CELL",
    "CHOOSE",
    "CHOOSECOLS",
    "CHOOSEROWS",
    "DROP",
    "EXPAND",
    "FIELDVALUE",
    "FILTER",
    "FILTERXML",
    "HLOOKUP",
    "HYPERLINK",
    "IF",
    "IFERROR",
    "IFNA",
    "IFS",
    "INDEX",
    "MUNIT",
    "RANDARRAY",
    "SEQUENCE",
    "SORT",
    "STOCKHISTORY",
    "SWITCH",
    "TAKE",
    "TEXTSPLIT",
    "TOCOL",
    "TOROW",
    "TRANSPOSE",
    "UNIQUE",
    "VLOOKUP",
    "WRAPCOLS",
    "WRAPROWS",
    "XLOOKUP",
];

fn legacy_params(name: &str) -> Option<&'static str> {
    Some(match name {
        "MATCH" => "VRV",
        "VLOOKUP" | "HLOOKUP" => "VRVV",
        "LOOKUP" => "VRR",
        "TRANSPOSE" => "V",
        "INDIRECT" => "VR",
        "IFERROR" | "IFNA" => "VV",
        _ => return None,
    })
}

thread_local! {

    static EVAL_FORM: std::cell::Cell<bool> = const { std::cell::Cell::new(false) };

    static LEGACY_ENTRY: std::cell::Cell<bool> = const { std::cell::Cell::new(false) };
    static MULTI_NAMES: std::cell::RefCell<Vec<String>> = const { std::cell::RefCell::new(Vec::new()) };

    static RANGE_NAMES: std::cell::RefCell<Vec<String>> = const { std::cell::RefCell::new(Vec::new()) };
    static COMPUTED_NAMES: std::cell::RefCell<Vec<String>> = const { std::cell::RefCell::new(Vec::new()) };

    static SCALAR_NAMES: std::cell::RefCell<Vec<String>> = const { std::cell::RefCell::new(Vec::new()) };

    static ONE_CELL_NAMES: std::cell::RefCell<Vec<String>> = const { std::cell::RefCell::new(Vec::new()) };

    static KNOWN_NAMES: std::cell::RefCell<Option<Vec<String>>> = const { std::cell::RefCell::new(None) };

    static IN_AGG: std::cell::Cell<bool> = const { std::cell::Cell::new(false) };
}

fn walk_agg(e: Expr, c: Ctx) -> Expr {
    if c == Ctx::Value {
        return walk(e, c);
    }
    let saved = IN_AGG.with(|f| f.replace(true));
    let out = walk(e, c);
    IN_AGG.with(|f| f.set(saved));
    out
}

fn in_agg() -> bool {
    IN_AGG.with(|f| f.get())
}

fn undefined_name(name: &str) -> bool {
    let up = name.to_uppercase();
    KNOWN_NAMES.with(|k| k.borrow().as_ref().is_some_and(|v| !v.contains(&up)))
}

fn bound_names(e: &Expr, out: &mut Vec<String>) {
    crate::deep(|| {
        let mut add = |x: &Expr| {
            if let Expr::Name { name, .. } = x.unwrap_ws() {
                out.push(name.to_uppercase());
            }
        };
        if let Expr::Func { name, args } = e {
            let up = name.to_uppercase();
            let bare = up.strip_prefix("_XLFN.").unwrap_or(&up);
            if bare == "LET" {
                for a in args.iter().step_by(2).take(args.len() / 2) {
                    add(a);
                }
            } else if bare == "LAMBDA" {
                for a in args.iter().take(args.len().saturating_sub(1)) {
                    add(a);
                }
            }
        }
        match e {
            Expr::Func { args, .. } => args.iter().for_each(|a| bound_names(a, out)),
            Expr::Call { callee, args } => {
                bound_names(callee, out);
                args.iter().for_each(|a| bound_names(a, out));
            }
            Expr::Unary { expr, .. } | Expr::Ws { expr, .. } => bound_names(expr, out),
            Expr::Percent(x) | Expr::Spill(x) | Expr::Paren(x) => bound_names(x, out),
            Expr::Field { expr, .. } => bound_names(expr, out),
            Expr::Binary { left, right, .. } => {
                bound_names(left, out);
                bound_names(right, out);
            }
            _ => {}
        }
    })
}

fn is_multi_ref(e: &Expr) -> bool {
    match e {
        Expr::Name { name, .. } => {
            let up = name.to_uppercase();
            MULTI_NAMES.with(|m| m.borrow().contains(&up))
        }

        Expr::Ref { kind, .. } => !matches!(kind, RefKind::Cell { .. }),
        Expr::Binary {
            op: BinOp::Range, ..
        } => true,

        Expr::Spill(_) => true,
        _ => false,
    }
}

fn is_native_array_call(e: &Expr) -> bool {
    match e {
        Expr::Func { name, .. } => {
            let up = name.to_uppercase();
            ARRAY_RESULT.contains(&up.as_str())
                && !matches!(
                    up.as_str(),
                    "INDEX" | "OFFSET" | "INDIRECT" | "CELL" | "FILTERXML"
                )
        }
        _ => false,
    }
}

fn takes_ref(e: &Expr) -> bool {
    match e {
        Expr::Func { name, args } => {
            let up = name.to_uppercase();
            let bare = up.strip_prefix("_XLFN.").unwrap_or(&up);
            matches!(bare, "TAKE" | "DROP")
                && args.first().is_some_and(|a| match a.unwrap_ws() {
                    Expr::Func { name, .. } => ref_call(name),
                    x => is_multi_ref(x) && !is_computed_name(x),
                })
        }
        _ => false,
    }
}

fn ref_call(name: &str) -> bool {
    name.eq_ignore_ascii_case("OFFSET") || name.eq_ignore_ascii_case("INDIRECT")
}

fn index_of_ref(e: &Expr) -> bool {
    crate::deep(|| match e.unwrap_ws() {
        Expr::Func { name, args } if name.eq_ignore_ascii_case("INDEX") => {
            args.first().is_some_and(|a| match a.unwrap_ws() {
                Expr::Ref { .. } | Expr::Spill(_) => true,
                Expr::Binary {
                    op: BinOp::Range, ..
                } => true,
                n @ Expr::Name { .. } => {
                    !is_computed_name(n) && (is_multi_ref(n) || is_scalar_name(n))
                }
                Expr::Func { name, .. } if ref_call(name) => true,
                f => index_of_ref(f),
            })
        }
        _ => false,
    })
}

fn at(e: Expr) -> Expr {
    Expr::Unary {
        op: UnaryOp::Implicit,
        expr: Box::new(e),
    }
}

fn array_capable(e: &Expr) -> bool {
    crate::deep(|| match e.unwrap_ws() {
        Expr::Paren(x) => array_capable(x),
        Expr::Unary {
            op: UnaryOp::Implicit,
            ..
        } => false,
        Expr::Array(rows) => rows.len() > 1 || rows.first().is_some_and(|r| r.len() > 1),
        Expr::Func { name, args } => {
            let upper = name.to_uppercase();
            let bare = upper.strip_prefix("_XLFN.").unwrap_or(&upper);
            returns_array(bare, args)
                || (matches!(bare, "IF" | "CHOOSE") && args.iter().skip(1).any(array_capable))
        }
        e => is_multi_ref(e),
    })
}

fn returns_array(name: &str, args: &[Expr]) -> bool {
    if name == "LET" && args.len() % 2 == 1 {
        return args.last().is_some_and(|r| match r.unwrap_ws() {
            Expr::Name { name, .. } => {
                MULTI_NAMES.with(|m| m.borrow().contains(&name.to_uppercase()))
            }
            Expr::Call { .. } => true,
            Expr::Func { name, .. } if functions::lookup(name).is_none() => true,
            x => array_capable(x),
        });
    }
    if functions::lookup(name).is_none() && undefined_name(name) {
        return true;
    }

    let zero_or_missing = |e: &Expr| {
        matches!(
            e.unwrap_ws(),
            Expr::Missing | Expr::Number(0.0) | Expr::Name { .. }
        )
    };

    fn arrayish(e: &Expr) -> bool {
        crate::deep(|| match e.unwrap_ws() {
            Expr::Binary { left, right, .. } => arrayish(left) || arrayish(right),

            Expr::Unary {
                op: UnaryOp::Implicit,
                ..
            } => false,
            Expr::Unary { expr, .. } => arrayish(expr),
            x => is_multi_ref(x) || array_capable(x),
        })
    }
    let multi = arrayish;
    fn has_arr(e: &Expr) -> bool {
        match e.unwrap_ws() {
            Expr::Func { args, .. } => args.iter().any(has_arr) || array_capable(e),
            x => array_capable(x) && !is_multi_ref(x),
        }
    }
    match name {
        "INDEX" if args.first().is_some_and(is_computed_name) => {
            let nonzero = args
                .iter()
                .skip(1)
                .filter(|a| matches!(a.unwrap_ws(), Expr::Number(n) if *n != 0.0))
                .count();
            let zero = args
                .get(1)
                .is_some_and(|a| matches!(a.unwrap_ws(), Expr::Number(n) if *n == 0.0));
            !(nonzero >= 2 || (zero && is_range_name(&args[0])))
        }

        "INDEX"
            if args.first().is_some_and(|a| {
                is_scalar_name(a)
                    || matches!(
                        a.unwrap_ws(),
                        Expr::Ref {
                            kind: RefKind::Cell { .. },
                            ..
                        }
                    )
            }) && !args[1..].iter().any(has_arr) =>
        {
            false
        }
        "INDEX" => {
            let first = args.first().map(|a| a.unwrap_ws());
            let computed = first.is_some_and(|a| match a {
                Expr::Func { name, .. } if ref_call(name) || index_of_ref(a) => false,
                Expr::Func { name, args } => {
                    array_capable(a)
                        || (matches!(
                            name.to_uppercase().as_str(),
                            "ABS" | "INT" | "ROUND" | "SQRT" | "SIGN"
                        ) && args
                            .iter()
                            .any(|x| is_multi_ref(x.unwrap_ws()) || arrayish(x)))
                }
                Expr::Binary { op, .. }
                    if !matches!(op, BinOp::Range | BinOp::Intersect | BinOp::Union) =>
                {
                    arrayish(a)
                }
                Expr::Unary { op, .. } if *op != UnaryOp::Implicit => arrayish(a),
                Expr::Call { .. } => true,
                _ => false,
            });

            let literal = |e: &Expr| match e.unwrap_ws() {
                Expr::Number(n) => *n != 0.0,
                f @ Expr::Func { .. } => !array_capable(f),
                _ => false,
            };
            let ranged = first.is_some_and(|f| {
                is_multi_ref(f)
                    || matches!(f, Expr::Func { name, .. } if (ref_call(name) || index_of_ref(f)) && array_capable(f))
            }) && !args[1..].iter().all(literal);
            computed
                || ranged
                || args
                    .iter()
                    .skip(1)
                    .any(|a| zero_or_missing(a) || has_arr(a))
        }
        "OFFSET" => {
            let one = |a: &Expr| matches!(a.unwrap_ws(), Expr::Number(n) if *n == 1.0);

            let sized = args.len() == 5 && args[3..].iter().all(one);
            args.iter().skip(1).take(2).any(has_arr)
                || args.iter().skip(3).take(2).any(|a| !one(a))
                || args
                    .first()
                    .is_some_and(|a| is_computed_name(a) || (is_multi_ref(a.unwrap_ws()) && !sized))
        }
        "IFERROR" | "IFNA" => args.get(1).is_some_and(array_capable),
        "ROW" | "COLUMN" | "ISFORMULA" | "FORMULATEXT" => args.first().is_some_and(multi),

        "XLOOKUP" => {
            let ret = |a: &Expr| match a.unwrap_ws() {
                n @ Expr::Name { .. } => is_computed_name(n) && !is_one_cell_name(n),
                Expr::Ref {
                    kind: RefKind::Area { r0, c0, r1, c1 },
                    ..
                } => r0.index != r1.index && c0.index != c1.index,
                Expr::Ref {
                    kind: RefKind::Cols { c0, c1 },
                    ..
                } => c0.index != c1.index,
                Expr::Ref {
                    kind: RefKind::Rows { r0, r1 },
                    ..
                } => r0.index != r1.index,
                Expr::Ref { .. } | Expr::Spill(_) => false,
                Expr::Binary {
                    op: BinOp::Range, ..
                } => false,
                x => multi(x),
            };
            args.get(3).is_some_and(array_capable)
                || (args.get(2).is_some_and(multi) && !args.get(1).is_some_and(multi))
                || args.get(2).is_some_and(ret)
        }
        _ => ARRAY_RESULT.contains(&name),
    }
}

fn walk(e: Expr, ctx: Ctx) -> Expr {
    crate::deep(|| walk_inner(e, ctx))
}

fn walk_inner(e: Expr, ctx: Ctx) -> Expr {
    if ctx == Ctx::Array {
        return match e {
            Expr::Func { name, args } => {
                let upper = name.to_uppercase();
                let bare = upper.strip_prefix("_XLFN.").unwrap_or(&upper);
                if NO_LIFT.contains(&bare) && !EVAL_FORM.with(|f| f.get()) {
                    return walk_inner(Expr::Func { name, args }, Ctx::Ref);
                }
                Expr::Func {
                    name,
                    args: args.into_iter().map(|a| walk(a, Ctx::Array)).collect(),
                }
            }
            c @ Expr::Call { .. } => walk_inner(c, Ctx::Ref),
            Expr::Ws {
                before,
                after,
                expr,
            } => Expr::Ws {
                before,
                after,
                expr: Box::new(walk(*expr, ctx)),
            },
            Expr::Paren(x) => Expr::Paren(Box::new(walk(*x, ctx))),
            Expr::Binary { op, left, right } => Expr::Binary {
                op,
                left: Box::new(walk(*left, ctx)),
                right: Box::new(walk(*right, ctx)),
            },
            Expr::Unary { op, expr } => Expr::Unary {
                op,
                expr: Box::new(walk(*expr, ctx)),
            },
            e => e,
        };
    }
    match e {
        Expr::Ws {
            before,
            after,
            expr,
        } => Expr::Ws {
            before,
            after,
            expr: Box::new(walk(*expr, ctx)),
        },
        Expr::Paren(x) => Expr::Paren(Box::new(walk(*x, ctx))),
        e if is_multi_ref(&e) => {
            if ctx == Ctx::Value {
                at(e)
            } else {
                e
            }
        }
        Expr::Name {
            ref sheet,
            ref name,
        } if sheet.is_none() && undefined_name(name) => {
            if ctx == Ctx::Value {
                at(e)
            } else {
                e
            }
        }
        Expr::Array(rows) => {
            let multi = rows.len() > 1 || rows.first().is_some_and(|r| r.len() > 1);
            if ctx == Ctx::Value && multi && !in_agg() {
                at(Expr::Array(rows))
            } else {
                Expr::Array(rows)
            }
        }
        Expr::Unary { op, expr } if op != UnaryOp::Implicit => Expr::Unary {
            op,
            expr: Box::new(walk(*expr, Ctx::Value)),
        },

        Expr::Unary {
            op: UnaryOp::Implicit,
            expr,
        } if matches!(expr.unwrap_ws(), Expr::Binary { op, .. }
                if !matches!(op, BinOp::Range | BinOp::Intersect | BinOp::Union))
            || matches!(expr.unwrap_ws(), Expr::Unary { op, .. } if *op != UnaryOp::Implicit)
            || matches!(expr.unwrap_ws(), Expr::Percent(_)) =>
        {
            Expr::Unary {
                op: UnaryOp::Implicit,
                expr: Box::new(walk(*expr, Ctx::Value)),
            }
        }
        Expr::Percent(x) => Expr::Percent(Box::new(walk(*x, Ctx::Value))),
        Expr::Binary { op, left, right }
            if !matches!(op, BinOp::Range | BinOp::Intersect | BinOp::Union) =>
        {
            Expr::Binary {
                op,
                left: Box::new(walk(*left, Ctx::Value)),
                right: Box::new(walk(*right, Ctx::Value)),
            }
        }
        Expr::Call { callee, args } => {
            let callee = walk(*callee, Ctx::Ref);
            let args = args.into_iter().map(|a| walk(a, Ctx::Ref)).collect();
            let call = Expr::Call {
                callee: Box::new(callee),
                args,
            };
            if ctx == Ctx::Value && !in_agg() {
                at(call)
            } else {
                call
            }
        }
        Expr::Func { name, args } => {
            let upper = name.to_uppercase();
            let bare = upper.strip_prefix("_XLFN.").unwrap_or(&upper).to_string();

            let mut args = args;
            let mut outer_arr = false;
            let mut inner = Vec::new();
            let measured = crate::legacy_classes::legacy_array_args(&bare)
                .or_else(|| crate::legacy_classes::legacy_classes(&bare).map(|_| "0000"));
            if let Some(t) = measured {
                let t = t.as_bytes();

                let col = if ctx == Ctx::Value && (!in_agg() || bare == "CHOOSE") {
                    0
                } else {
                    2
                };
                for (i, a) in args.iter_mut().enumerate() {
                    let multi = matches!(a.unwrap_ws(), Expr::Array(rows)
                        if rows.len() > 1 || rows.first().is_some_and(|r| r.len() > 1))
                        || (bare == "AGGREGATE"
                            && i == 3
                            && matches!(a.unwrap_ws(), Expr::Array(_)));

                    let refused = functions::lookup(&bare).is_some_and(|id| {
                        functions::def(id).param_class(i) == functions::ParamClass::Reference
                    });

                    let refused = refused || (bare == "AGGREGATE" && i >= 4);

                    let ref_arg = crate::legacy_classes::legacy_classes(&bare).is_some_and(|p| {
                        p.as_bytes().get(i.min(p.len().saturating_sub(1))) == Some(&b'R')
                    });
                    let native = is_native_array_call(a.unwrap_ws())
                        && !(ref_arg && takes_ref(a.unwrap_ws()));
                    if (multi || native) && !refused {
                        let k = (i * 4).min(t.len() - 4) + col;
                        let arr = std::mem::replace(a, Expr::Number(0.0));
                        let arr = if native { walk(arr, Ctx::Ref) } else { arr };
                        inner.push((i, arr, t[k] == b'1'));
                        outer_arr |= t[k + 1] == b'1';
                    }
                }
            }
            let mut args = func_args(&bare, args);
            for (i, arr, at_it) in inner {
                args[i] = if at_it { at(arr) } else { arr };
            }

            let rowcol = matches!(
                bare.as_str(),
                "ROW" | "COLUMN" | "ISFORMULA" | "FORMULATEXT"
            );

            let hoisted = bare == "LET"
                && ctx == Ctx::Value
                && args.len() % 2 == 1
                && args.last_mut().is_some_and(|l| hoist_leftmost(l, true));
            let call = Expr::Func { name, args };
            if hoisted {
                return at(call);
            }

            let native = in_agg()
                && ARRAY_RESULT.contains(&bare.as_str())
                && !matches!(
                    bare.as_str(),
                    "INDEX" | "OFFSET" | "INDIRECT" | "CELL" | "FILTERXML"
                );
            if outer_arr || ((ctx == Ctx::Value || rowcol) && !native && array_capable(&call)) {
                at(call)
            } else {
                call
            }
        }
        e => e,
    }
}

fn hoist_leftmost(e: &mut Expr, top: bool) -> bool {
    match e {
        Expr::Unary {
            op: UnaryOp::Implicit,
            expr,
        } => {
            let native = match expr.unwrap_ws() {
                Expr::Func { name, .. } => {
                    let up = name.to_uppercase();
                    ARRAY_RESULT.contains(&up.as_str())
                        && !matches!(
                            up.as_str(),
                            "INDEX" | "OFFSET" | "INDIRECT" | "CELL" | "FILTERXML"
                        )
                }
                Expr::Array(_) => true,
                _ => false,
            };
            let whole = top
                && match expr.unwrap_ws() {
                    Expr::Func { name, .. } => !matches!(
                        name.to_uppercase().as_str(),
                        "ROW" | "COLUMN" | "ISFORMULA" | "FORMULATEXT" | "CELL"
                    ),
                    Expr::Call { .. } | Expr::Ref { .. } => true,
                    Expr::Binary {
                        op: BinOp::Range, ..
                    } => true,
                    _ => false,
                };
            if native || whole {
                let inner = std::mem::replace(&mut **expr, Expr::Number(0.0));
                *e = inner;
                true
            } else {
                false
            }
        }
        Expr::Unary { expr, .. } | Expr::Paren(expr) | Expr::Ws { expr, .. } => {
            hoist_leftmost(expr, false)
        }
        Expr::Binary { op, left, right }
            if !matches!(op, BinOp::Range | BinOp::Intersect | BinOp::Union) =>
        {
            let l = hoist_leftmost(left, false);
            let r = hoist_leftmost(right, false);
            l || r
        }
        _ => false,
    }
}

fn func_args(name: &str, args: Vec<Expr>) -> Vec<Expr> {
    if name == "LAMBDA" && !args.is_empty() {
        let n = args.len();
        let params: Vec<String> = args[..n - 1]
            .iter()
            .filter_map(|a| match a.unwrap_ws() {
                Expr::Name { name, .. } => Some(name.to_uppercase()),
                _ => None,
            })
            .collect();
        let own = params.clone();
        MULTI_NAMES.with(|m| m.borrow_mut().extend(params));
        let mut args = args;
        let body = args.pop().unwrap();

        fn bare_param(e: &Expr, own: &[String]) -> bool {
            match e.unwrap_ws() {
                Expr::Paren(x) => bare_param(x, own),
                Expr::Name { sheet: None, name } => own.contains(&name.to_uppercase()),
                _ => false,
            }
        }
        args.push(if bare_param(&body, &own) {
            body
        } else {
            walk(body, Ctx::Value)
        });
        return args;
    }

    if name == "LET" && args.len() % 2 == 1 {
        let n = args.len();
        let mut out = Vec::with_capacity(n);
        let mut added = Vec::new();
        let mut it = args.into_iter();
        while out.len() + 1 < n {
            let var = it.next().unwrap();
            let val = it.next().unwrap();
            if is_multi_ref(val.unwrap_ws()) || is_native_array_call(val.unwrap_ws()) {
                if let Expr::Name { name, .. } = var.unwrap_ws() {
                    added.push(name.to_uppercase());
                }
            }
            out.push(var);
            out.push(walk(val, Ctx::Ref));
            MULTI_NAMES.with(|m| m.borrow_mut().extend(added.drain(..)));
        }
        let last = it.next().unwrap();

        out.push(if matches!(last.unwrap_ws(), Expr::Name { .. }) {
            last
        } else if in_agg() && matches!(last.unwrap_ws(), Expr::Func { .. }) {
            walk(last, Ctx::Ref)
        } else {
            walk(last, Ctx::Value)
        });
        return out;
    }

    if let Some(classes) =
        crate::legacy_classes::legacy_classes(name).filter(|_| !EVAL_FORM.with(|f| f.get()))
    {
        return args
            .into_iter()
            .enumerate()
            .map(|(i, a)| {
                let c = match crate::legacy_classes::class_at(name, classes, i) {

                    b'V' if matches!(name, "HLOOKUP" | "VLOOKUP") && i == 2 && in_agg() => Ctx::Ref,
                    b'V' => Ctx::Value,

                    b'A' if name != "SUMPRODUCT"
                        && matches!(a.unwrap_ws(), Expr::Func { name: f, .. }
                            if f.eq_ignore_ascii_case("OFFSET") || f.eq_ignore_ascii_case("INDIRECT")) =>
                    {
                        Ctx::Ref
                    }
                    b'A' => Ctx::Array,
                    _ => Ctx::Ref,
                };

                if PASS_THROUGH.contains(&name) {
                    walk(a, c)
                } else {
                    walk_agg(a, c)
                }
            })
            .collect();
    }
    if ARRAY_ARGS.contains(&name) {
        return args.into_iter().map(|a| walk_agg(a, Ctx::Array)).collect();
    }

    if matches!(name, "IF" | "CHOOSE") {
        return args
            .into_iter()
            .enumerate()
            .map(|(i, a)| walk(a, if i == 0 { Ctx::Value } else { Ctx::Ref }))
            .collect();
    }
    let params = match legacy_params(name) {
        Some(p) => p,
        None => {
            let Some(id) = functions::lookup(name) else {
                return args;
            };
            let def = functions::def(id);
            if matches!(def.imp, functions::Imp::Lazy(_)) {
                return args;
            }
            def.params
        }
    };
    args.into_iter()
        .enumerate()
        .map(|(i, a)| {
            let b = params.as_bytes();
            let letter = if b.is_empty() {
                b'V'
            } else {
                b[i.min(b.len() - 1)]
            };
            let c = if letter == b'V' { Ctx::Value } else { Ctx::Ref };
            if PASS_THROUGH.contains(&name) {
                walk(a, c)
            } else {
                walk_agg(a, c)
            }
        })
        .collect()
}

fn strip_at(e: Expr) -> Expr {
    rewrite_at(e, false)
}

fn rewrite_at(e: Expr, quiet: bool) -> Expr {
    crate::deep(|| rewrite_at_inner(e, quiet))
}

fn rewrite_at_inner(e: Expr, quiet: bool) -> Expr {
    let strip_at = |e: Expr| rewrite_at(e, quiet);
    match e {
        Expr::Unary {
            op: UnaryOp::Implicit,
            expr,
        } if quiet => Expr::Unary {
            op: UnaryOp::ImplicitQuiet,
            expr: Box::new(strip_at(*expr)),
        },
        Expr::Unary {
            op: UnaryOp::Implicit | UnaryOp::ImplicitQuiet,
            expr,
        } if !quiet => strip_at(*expr),
        Expr::Unary { op, expr } => Expr::Unary {
            op,
            expr: Box::new(strip_at(*expr)),
        },
        Expr::Ws {
            before,
            after,
            expr,
        } => Expr::Ws {
            before,
            after,
            expr: Box::new(strip_at(*expr)),
        },
        Expr::Paren(x) => Expr::Paren(Box::new(strip_at(*x))),
        Expr::Percent(x) => Expr::Percent(Box::new(strip_at(*x))),
        Expr::Spill(x) => Expr::Spill(Box::new(strip_at(*x))),
        Expr::Binary { op, left, right } => Expr::Binary {
            op,
            left: Box::new(strip_at(*left)),
            right: Box::new(strip_at(*right)),
        },
        Expr::Func { name, args } => Expr::Func {
            name,
            args: args.into_iter().map(strip_at).collect(),
        },
        Expr::Call { callee, args } => Expr::Call {
            callee: Box::new(strip_at(*callee)),
            args: args.into_iter().map(strip_at).collect(),
        },
        e => e,
    }
}

pub(crate) fn legacy_to_dynamic_in(wb: &Workbook, text: &str) -> String {
    let text = input::auto_close_call(text);
    let Ok(e) = parse(&text) else {
        return format!("={text}");
    };
    let mut known: Vec<String> = wb.names.iter().map(|n| n.name.to_uppercase()).collect();
    bound_names(&e, &mut known);
    KNOWN_NAMES.with(|k| *k.borrow_mut() = Some(known));
    let out = format!("={}", print(&walk(e, Ctx::Value)));
    KNOWN_NAMES.with(|k| *k.borrow_mut() = None);
    out
}

impl Workbook {
    pub fn set_legacy_input(
        &mut self,
        addr: CellAddr,
        input: &str,
    ) -> Result<RecalcReport, EditError> {
        match input::interpret(input, &self.input_env()) {
            Entered::Formula(text) => {
                let multi = multi_names(self);
                MULTI_NAMES.with(|m| *m.borrow_mut() = multi);
                let da = legacy_to_dynamic_in(self, &text);
                MULTI_NAMES.with(|m| m.borrow_mut().clear());
                LEGACY_ENTRY.with(|l| l.set(true));
                let r = self.set_input(addr, &da);
                LEGACY_ENTRY.with(|l| l.set(false));
                r
            }
            _ => self.set_input(addr, input),
        }
    }

    pub fn legacy_formula_text(&self, addr: CellAddr) -> Option<String> {
        let t = self.formula_text(addr)?;

        if t.contains("!#REF!") && !t.contains('@') {
            return Some(t);
        }

        if t.contains("@#REF!")
            || t.contains("@INDEX(#REF!")
            || t.contains("@OFFSET(#REF!")
            || (t.contains("@CHOOSE(") && t.contains(",#REF!,#REF!)"))
        {
            return Some(t);
        }
        match parse(&t) {
            Ok(e) => Some(format!("={}", print(&strip_at_keep_cell(e)))),
            Err(_) => Some(t),
        }
    }
}

fn strip_at_keep_cell(e: Expr) -> Expr {
    crate::deep(|| match e {
        Expr::Unary {
            op: UnaryOp::Implicit,
            expr,
        } if matches!(
            expr.unwrap_ws(),
            Expr::Ref {
                kind: RefKind::Cell { .. },
                ..
            }
        ) =>
        {
            Expr::Unary {
                op: UnaryOp::Implicit,
                expr,
            }
        }
        Expr::Func { name, args } => {
            let upper = name.to_uppercase();
            let bare = upper.strip_prefix("_XLFN.").unwrap_or(&upper);
            let classes = crate::legacy_classes::legacy_classes(bare).map(str::as_bytes);
            let args = args
                .into_iter()
                .enumerate()
                .map(|(i, a)| {
                    let q = classes.is_some_and(|b| !b.is_empty() && b[i.min(b.len() - 1)] == b'Q');
                    match a {
                        Expr::Unary {
                            op: UnaryOp::Implicit,
                            expr,
                        } if q => Expr::Unary {
                            op: UnaryOp::Implicit,
                            expr: Box::new(strip_at_keep_cell(*expr)),
                        },
                        a => strip_at_keep_cell(a),
                    }
                })
                .collect();
            Expr::Func { name, args }
        }
        Expr::Unary {
            op: UnaryOp::Implicit | UnaryOp::ImplicitQuiet,
            expr,
        } => strip_at_keep_cell(*expr),
        Expr::Unary { op, expr } => Expr::Unary {
            op,
            expr: Box::new(strip_at_keep_cell(*expr)),
        },
        Expr::Ws {
            before,
            after,
            expr,
        } => Expr::Ws {
            before,
            after,
            expr: Box::new(strip_at_keep_cell(*expr)),
        },
        Expr::Paren(x) => Expr::Paren(Box::new(strip_at_keep_cell(*x))),
        Expr::Percent(x) => Expr::Percent(Box::new(strip_at_keep_cell(*x))),
        Expr::Spill(x) => Expr::Spill(Box::new(strip_at_keep_cell(*x))),
        Expr::Binary { op, left, right } => Expr::Binary {
            op,
            left: Box::new(strip_at_keep_cell(*left)),
            right: Box::new(strip_at_keep_cell(*right)),
        },
        e => strip_at(e),
    })
}

pub(crate) fn legacy_quiet_expr(text: &str) -> Option<Expr> {
    let e = parse(text.trim_start_matches('=')).ok()?;
    Some(rewrite_at(walk(e, Ctx::Value), true))
}

thread_local! {
    static UNDEFINED: std::cell::Cell<bool> = const { std::cell::Cell::new(false) };
}

const SCALAR_RESULT: &[&str] = &[
    "ABS",
    "AND",
    "AVERAGE",
    "AVERAGEA",
    "AVERAGEIF",
    "AVERAGEIFS",
    "CHAR",
    "CHOOSE",
    "CODE",
    "COLUMN",
    "COLUMNS",
    "CONCAT",
    "CONCATENATE",
    "COUNT",
    "COUNTA",
    "COUNTBLANK",
    "COUNTIF",
    "COUNTIFS",
    "DATE",
    "DAY",
    "EXACT",
    "EXP",
    "FALSE",
    "FIND",
    "HLOOKUP",
    "HOUR",
    "IF",
    "IFERROR",
    "IFNA",
    "INDEX",
    "INT",
    "ISBLANK",
    "ISERR",
    "ISERROR",
    "ISEVEN",
    "ISLOGICAL",
    "ISNA",
    "ISNONTEXT",
    "ISNUMBER",
    "ISODD",
    "ISREF",
    "ISTEXT",
    "LARGE",
    "LEFT",
    "LEN",
    "LN",
    "LOG",
    "LOG10",
    "LOWER",
    "MATCH",
    "MAX",
    "MAXA",
    "MEDIAN",
    "MID",
    "MIN",
    "MINA",
    "MINUTE",
    "MOD",
    "MONTH",
    "N",
    "NA",
    "NOT",
    "NOW",
    "OR",
    "PI",
    "POWER",
    "PRODUCT",
    "PROPER",
    "RAND",
    "RANK",
    "REPT",
    "RIGHT",
    "ROUND",
    "ROUNDDOWN",
    "ROUNDUP",
    "ROW",
    "ROWS",
    "SEARCH",
    "SECOND",
    "SIGN",
    "SMALL",
    "SQRT",
    "STDEV",
    "SUBSTITUTE",
    "SUBTOTAL",
    "SUM",
    "SUMIF",
    "SUMIFS",
    "SUMPRODUCT",
    "SUMSQ",
    "T",
    "TEXT",
    "TEXTJOIN",
    "TODAY",
    "TRIM",
    "TRUE",
    "TRUNC",
    "TYPE",
    "UPPER",
    "VALUE",
    "VAR",
    "VLOOKUP",
    "XOR",
    "YEAR",
    "LET",
];

fn scalar_definition(refers_to: &str) -> bool {
    if refers_to.contains("!#REF!") {
        return false;
    }
    match parse(refers_to.trim_start_matches('=')) {
        Ok(e) => matches!(
            e.unwrap_ws(),
            Expr::Ref {
                kind: RefKind::Cell { .. },
                ..
            } | Expr::Number(_)
                | Expr::Text(_)
                | Expr::Bool(_)
                | Expr::Error(_)
        ),
        Err(_) => false,
    }
}

fn may_array(
    e: &Expr,
    wb: &Workbook,
    sheet: crate::address::SheetId,
    locals: &mut Vec<(String, bool)>,
) -> bool {
    crate::deep(|| match e {
        Expr::Ws { expr, .. } | Expr::Paren(expr) | Expr::Percent(expr) => {
            may_array(expr, wb, sheet, locals)
        }
        Expr::Number(_) | Expr::Text(_) | Expr::Bool(_) | Expr::Error(_) | Expr::Missing => false,

        Expr::Ref {
            sheet: Some(crate::parser::ast::SheetSpec::Range(a, _)),
            ..
        } if wb.sheet_by_name(a).is_none()
            && locals.iter().any(|(n, _)| *n == a.to_uppercase()) =>
        {
            true
        }
        Expr::Ref { kind, .. } => !matches!(kind, RefKind::Cell { .. }),
        Expr::Name { sheet: q, name } => {
            if q.is_none() {
                let up = name.to_uppercase();
                if let Some((_, a)) = locals.iter().rev().find(|(n, _)| *n == up) {
                    return *a;
                }
            }
            let key = name.to_uppercase();
            let def = match q {
                Some(crate::parser::ast::SheetSpec::Single(s)) => {
                    let id = wb.sheet_by_name(s).map(|s| s.id);
                    id.and_then(|id| wb.names.get(&key, Some(id)))
                        .or_else(|| wb.names.get(&key, None))
                }
                Some(_) => None,
                None => wb.names.lookup(&key, sheet),
            };
            if def.is_none() {
                UNDEFINED.with(|u| u.set(true));
            }
            !def.is_some_and(|d| scalar_definition(&d.refers_to))
        }
        Expr::Unary { op, expr } => {
            !matches!(op, UnaryOp::Implicit | UnaryOp::ImplicitQuiet)
                && may_array(expr, wb, sheet, locals)
        }
        Expr::Binary { op, left, right } => {
            matches!(op, BinOp::Range | BinOp::Intersect | BinOp::Union)
                || may_array(left, wb, sheet, locals)
                || may_array(right, wb, sheet, locals)
        }
        Expr::Func { name, args } => {
            let upper = name.to_uppercase();
            let bare = upper.strip_prefix("_XLFN.").unwrap_or(&upper);
            if bare == "LET" && args.len() % 2 == 1 {
                let n = locals.len();
                for pair in args[..args.len() - 1].chunks(2) {
                    let a = may_array(&pair[1], wb, sheet, locals);

                    if a {
                        locals.truncate(n);
                        return true;
                    }
                    if let Expr::Name { name, .. } = pair[0].unwrap_ws() {
                        locals.push((name.to_uppercase(), a));
                    }
                }
                let r = may_array(&args[args.len() - 1], wb, sheet, locals);
                locals.truncate(n);
                return r;
            }

            if matches!(bare, "ROW" | "COLUMN" | "OFFSET")
                && args
                    .first()
                    .is_some_and(|a| may_array(a, wb, sheet, locals))
            {
                return true;
            }
            if returns_array(bare, args) || !SCALAR_RESULT.contains(&bare) {
                return true;
            }
            let params = match legacy_params(bare) {
                Some(p) => p,
                None => {
                    let Some(id) = functions::lookup(bare) else {
                        return true;
                    };
                    let def = functions::def(id);
                    if matches!(def.imp, functions::Imp::Lazy(_)) || bare == "LAMBDA" {
                        return bare == "LAMBDA"
                            || args.iter().any(|a| may_array(a, wb, sheet, locals));
                    }
                    def.params
                }
            };
            let b = params.as_bytes();

            let reference = |a: &Expr| match a.unwrap_ws() {
                Expr::Ref { .. } | Expr::Name { .. } | Expr::Spill(_) => true,
                Expr::Binary { op, .. } => {
                    matches!(op, BinOp::Range | BinOp::Intersect | BinOp::Union)
                }
                _ => false,
            };
            args.iter().enumerate().any(|(i, a)| {
                let letter = if b.is_empty() {
                    b'V'
                } else {
                    b[i.min(b.len() - 1)]
                };

                let entered = !LEGACY_ENTRY.with(|l| l.get());
                let lifting = letter == b'Q' && entered && is_ref_array_name(a, wb, sheet);

                let values = entered && is_value_array_name(a, wb, sheet);
                (letter == b'V' || lifting || values || !reference(a))
                    && may_array(a, wb, sheet, locals)
            })
        }
        _ => true,
    })
}

fn is_value_array_name(e: &Expr, wb: &Workbook, sheet: crate::address::SheetId) -> bool {
    let Expr::Name { sheet: None, name } = e.unwrap_ws() else {
        return false;
    };
    let Some(def) = wb.names.lookup(&name.to_uppercase(), sheet) else {
        return false;
    };
    let Ok(d) = parse(def.refers_to.trim_start_matches('=')) else {
        return false;
    };
    let value_expr = match d.unwrap_ws() {
        Expr::Array(_) => true,
        Expr::Unary { .. } | Expr::Percent(_) => true,
        Expr::Binary { op, .. } => !matches!(op, BinOp::Range | BinOp::Intersect | BinOp::Union),
        Expr::Func { name, args } => {
            let up = name.to_uppercase();
            let bare = up.strip_prefix("_XLFN.").unwrap_or(&up);
            !matches!(
                bare,
                "OFFSET"
                    | "INDEX"
                    | "INDIRECT"
                    | "CHOOSE"
                    | "IF"
                    | "IFS"
                    | "SWITCH"
                    | "LET"
                    | "XLOOKUP"
                    | "TAKE"
                    | "DROP"
                    | "CHOOSECOLS"
                    | "CHOOSEROWS"
                    | "LOOKUP"
                    | "VLOOKUP"
                    | "HLOOKUP"
            ) || (matches!(bare, "CHOOSE" | "IF")
                && args
                    .first()
                    .is_some_and(|a| may_array(a, wb, sheet, &mut Vec::new())))
        }
        _ => false,
    };
    value_expr && may_array(&d, wb, sheet, &mut Vec::new())
}

fn is_ref_array_name(e: &Expr, wb: &Workbook, sheet: crate::address::SheetId) -> bool {
    let Expr::Name { sheet: None, name } = e.unwrap_ws() else {
        return false;
    };
    let Some(def) = wb.names.lookup(&name.to_uppercase(), sheet) else {
        return false;
    };
    let Ok(Expr::Func { name: f, args }) = parse(def.refers_to.trim_start_matches('=')) else {
        return false;
    };
    let f = f.to_uppercase();
    let skip = match f.as_str() {
        "OFFSET" | "INDEX" => 1,
        "INDIRECT" => 0,
        _ => return false,
    };
    args.iter()
        .skip(skip)
        .any(|a| may_array(a, wb, sheet, &mut Vec::new()))
}

fn uses_name(e: &Expr, wb: &Workbook) -> Option<bool> {
    let mut names = Vec::new();
    collect_names(e, &mut names);
    if names.iter().any(|n| wb.table_by_name(n).is_some()) {
        return None;
    }

    let mut vars = Vec::new();
    let_vars(e, &mut vars);
    let defined = |n: &str| {
        let k = n.to_uppercase();
        wb.names.iter().any(|d| d.name.to_uppercase() == k) || vars.contains(&k)
    };
    if !names.iter().all(|n| defined(n)) {
        return None;
    }
    Some(!names.is_empty())
}

fn let_vars(e: &Expr, out: &mut Vec<String>) {
    crate::deep(|| match e {
        Expr::Func { name, args } => {
            if name.eq_ignore_ascii_case("LET") || name.eq_ignore_ascii_case("_xlfn.LET") {
                for a in args.iter().step_by(2).take(args.len() / 2) {
                    if let Expr::Name { name, .. } = a.unwrap_ws() {
                        out.push(name.to_uppercase());
                    }
                }
            }
            args.iter().for_each(|a| let_vars(a, out));
        }
        Expr::Ws { expr, .. }
        | Expr::Paren(expr)
        | Expr::Percent(expr)
        | Expr::Spill(expr)
        | Expr::Unary { expr, .. }
        | Expr::Field { expr, .. } => let_vars(expr, out),
        Expr::Binary { left, right, .. } => {
            let_vars(left, out);
            let_vars(right, out);
        }
        _ => {}
    })
}

fn collect_names<'a>(e: &'a Expr, out: &mut Vec<&'a str>) {
    crate::deep(|| match e {
        Expr::Name { name, .. } => out.push(name),
        Expr::Ws { expr, .. }
        | Expr::Paren(expr)
        | Expr::Percent(expr)
        | Expr::Spill(expr)
        | Expr::Unary { expr, .. }
        | Expr::Field { expr, .. } => collect_names(expr, out),
        Expr::Binary { left, right, .. } => {
            collect_names(left, out);
            collect_names(right, out);
        }
        Expr::Func { args, .. } => args.iter().for_each(|a| collect_names(a, out)),
        Expr::Call { callee, args } => {
            collect_names(callee, out);
            args.iter().for_each(|a| collect_names(a, out));
        }
        _ => {}
    })
}

fn is_range_name(e: &Expr) -> bool {
    match e.unwrap_ws() {
        Expr::Name { name, .. } => RANGE_NAMES.with(|c| c.borrow().contains(&name.to_uppercase())),
        _ => false,
    }
}

fn is_computed_name(e: &Expr) -> bool {
    match e.unwrap_ws() {
        Expr::Name { name, .. } => {
            COMPUTED_NAMES.with(|c| c.borrow().contains(&name.to_uppercase()))
        }
        _ => false,
    }
}

fn computed_name(e: &Expr) -> bool {
    matches!(
        e.unwrap_ws(),
        Expr::Paren(_)
            | Expr::Array(_)
            | Expr::Func { .. }
            | Expr::Unary { .. }
            | Expr::Percent(_)
            | Expr::Binary { .. }
    )
}

fn scalar_name_def(e: &Expr) -> bool {
    matches!(
        e.unwrap_ws(),
        Expr::Number(_)
            | Expr::Text(_)
            | Expr::Bool(_)
            | Expr::Error(_)
            | Expr::Ref {
                kind: RefKind::Cell { .. },
                ..
            }
    )
}

fn one_cell_index(e: &Expr) -> bool {
    let first_nonzero = |a: &Expr| match a.unwrap_ws() {
        Expr::Number(n) => *n != 0.0,
        Expr::Array(rows) => rows
            .first()
            .and_then(|r| r.first())
            .is_some_and(|x| matches!(x, crate::parser::ast::ArrayItem::Number(n) if *n != 0.0)),
        _ => false,
    };
    match e.unwrap_ws() {
        Expr::Func { name, args } if name.eq_ignore_ascii_case("INDEX") && args.len() >= 2 => {
            matches!(
                args[0].unwrap_ws(),
                Expr::Ref { .. }
                    | Expr::Binary {
                        op: BinOp::Range,
                        ..
                    }
            ) && args[1..].iter().all(first_nonzero)
        }
        _ => false,
    }
}

fn is_one_cell_name(e: &Expr) -> bool {
    match e.unwrap_ws() {
        Expr::Name { name, .. } => {
            ONE_CELL_NAMES.with(|c| c.borrow().contains(&name.to_uppercase()))
        }
        _ => false,
    }
}

fn is_scalar_name(e: &Expr) -> bool {
    match e.unwrap_ws() {
        Expr::Name { name, .. } => SCALAR_NAMES.with(|c| c.borrow().contains(&name.to_uppercase())),
        _ => false,
    }
}

fn multi_names(wb: &Workbook) -> Vec<String> {
    RANGE_NAMES.with(|c| {
        *c.borrow_mut() = wb
            .names
            .iter()
            .filter(|n| {
                parse(n.refers_to.trim_start_matches('=')).is_ok_and(|e| {
                    matches!(
                        e.unwrap_ws(),
                        Expr::Binary {
                            op: BinOp::Range,
                            ..
                        }
                    )
                })
            })
            .map(|n| n.name.to_uppercase())
            .collect()
    });
    COMPUTED_NAMES.with(|c| {
        *c.borrow_mut() = wb
            .names
            .iter()
            .filter(|n| parse(n.refers_to.trim_start_matches('=')).is_ok_and(|e| computed_name(&e)))
            .map(|n| n.name.to_uppercase())
            .collect()
    });
    SCALAR_NAMES.with(|c| {
        *c.borrow_mut() = wb
            .names
            .iter()
            .filter(|n| {
                parse(n.refers_to.trim_start_matches('=')).is_ok_and(|e| scalar_name_def(&e))
            })
            .map(|n| n.name.to_uppercase())
            .collect()
    });
    ONE_CELL_NAMES.with(|c| {
        *c.borrow_mut() = wb
            .names
            .iter()
            .filter(|n| {
                parse(n.refers_to.trim_start_matches('=')).is_ok_and(|e| one_cell_index(&e))
            })
            .map(|n| n.name.to_uppercase())
            .collect()
    });
    wb.names
        .iter()
        .filter(|n| {
            parse(n.refers_to.trim_start_matches('='))
                .is_ok_and(|e| is_multi_ref(&e) || array_capable(&e) || computed_name(&e))
        })
        .map(|n| n.name.to_uppercase())
        .collect()
}

pub(crate) fn scalar_entry_expr(wb: &Workbook, body: &str, host: CellAddr) -> Option<Expr> {
    let legacy = LEGACY_ENTRY.with(|l| l.get());
    if (body.contains('@') && !legacy)
        || body.contains('[')
        || body.contains("#REF!")
        || body.contains("Book1!")
    {
        return None;
    }
    let e = parse(body).ok()?;
    UNDEFINED.with(|u| u.set(false));

    if uses_name(&e, wb) != Some(true)
        || (may_array(&e, wb, host.sheet, &mut Vec::new()) && !legacy)
        || UNDEFINED.with(|u| u.get())
    {
        return None;
    }
    legacy_eval_expr(wb, body)
}

pub(crate) fn legacy_eval_expr(wb: &Workbook, body: &str) -> Option<Expr> {
    let shown = parse(body).ok()?;
    MULTI_NAMES.with(|m| *m.borrow_mut() = multi_names(wb));
    EVAL_FORM.with(|f| f.set(true));
    let w = walk(strip_at(shown.clone()), Ctx::Value);
    EVAL_FORM.with(|f| f.set(false));
    MULTI_NAMES.with(|m| m.borrow_mut().clear());
    Some(Expr::Unary {
        op: UnaryOp::ImplicitQuiet,
        expr: Box::new(merge_at(w, &shown)),
    })
}

fn merge_at(eval: Expr, shown: &Expr) -> Expr {
    crate::deep(|| merge_at_inner(eval, shown))
}

fn merge_at_inner(eval: Expr, shown: &Expr) -> Expr {
    let boxed = |e: Box<Expr>, s: &Expr| Box::new(merge_at(*e, s));
    match (eval, shown) {
        (
            Expr::Unary {
                op: UnaryOp::Implicit,
                expr,
            },
            Expr::Unary {
                op: UnaryOp::Implicit,
                expr: s,
            },
        ) => Expr::Unary {
            op: UnaryOp::Implicit,
            expr: boxed(expr, s),
        },
        (
            Expr::Unary {
                op: UnaryOp::Implicit,
                expr,
            },
            s,
        ) => Expr::Unary {
            op: UnaryOp::ImplicitQuiet,
            expr: boxed(expr, s),
        },

        (
            e,
            Expr::Unary {
                op: UnaryOp::Implicit,
                expr: s,
            },
        ) => Expr::Unary {
            op: UnaryOp::Implicit,
            expr: Box::new(merge_at(e, s)),
        },
        (Expr::Unary { op, expr }, Expr::Unary { op: o, expr: s }) if op == *o => Expr::Unary {
            op,
            expr: boxed(expr, s),
        },
        (Expr::Func { name, args }, Expr::Func { args: sa, .. }) if args.len() == sa.len() => {
            Expr::Func {
                name,
                args: args
                    .into_iter()
                    .zip(sa)
                    .map(|(a, s)| merge_at(a, s))
                    .collect(),
            }
        }
        (
            Expr::Call { callee, args },
            Expr::Call {
                callee: sc,
                args: sa,
            },
        ) if args.len() == sa.len() => Expr::Call {
            callee: boxed(callee, sc),
            args: args
                .into_iter()
                .zip(sa)
                .map(|(a, s)| merge_at(a, s))
                .collect(),
        },
        (
            Expr::Binary { op, left, right },
            Expr::Binary {
                left: l, right: r, ..
            },
        ) => Expr::Binary {
            op,
            left: boxed(left, l),
            right: boxed(right, r),
        },
        (Expr::Paren(x), Expr::Paren(s)) => Expr::Paren(boxed(x, s)),
        (Expr::Percent(x), Expr::Percent(s)) => Expr::Percent(boxed(x, s)),
        (Expr::Spill(x), Expr::Spill(s)) => Expr::Spill(boxed(x, s)),
        (
            Expr::Ws {
                before,
                after,
                expr,
            },
            Expr::Ws { expr: s, .. },
        ) => Expr::Ws {
            before,
            after,
            expr: boxed(expr, s),
        },
        (e, _) => rewrite_at(e, true),
    }
}

impl Workbook {
    pub(crate) fn refresh_scalar_entry(&mut self, fid: crate::workbook::FormulaId) {
        let (root, host) = {
            let c = self.formula(fid);
            (c.formula.root.clone(), c.addr)
        };
        if !matches!(root, crate::formula::Node::Unary(UnaryOp::ImplicitQuiet, _)) {
            return;
        }
        let text = crate::formula_print::print_node(self, &root, host);
        let body = text.trim_start_matches('=');

        if body.contains("#REF!") || body.contains("Book1!") || body.contains('[') {
            return;
        }
        let Some(e) = legacy_eval_expr(self, body) else {
            return;
        };
        if let Ok((f, _)) = self.compile_expr_normalized(&e, host) {
            self.formula_mut(fid).formula = std::sync::Arc::new(f);
        }
    }
}

pub(crate) fn host_may_array(wb: &Workbook, host: CellAddr) -> bool {
    let Some(fid) = wb.formula_at(host) else {
        return false;
    };
    let root = wb.formula(fid).formula.root.clone();
    let text = crate::formula_print::print_node(wb, &root, host);
    let Ok(e) = parse(text.trim_start_matches('=')) else {
        return false;
    };
    value_array(&e, true)
}

fn value_array(e: &Expr, value: bool) -> bool {
    crate::deep(|| match e {
        Expr::Ws { expr, .. } | Expr::Paren(expr) | Expr::Percent(expr) => value_array(expr, value),
        Expr::Unary { op, expr } => {
            !matches!(op, UnaryOp::Implicit | UnaryOp::ImplicitQuiet) && value_array(expr, true)
        }
        Expr::Array(_) | Expr::Spill(_) => value,
        Expr::Ref { kind, .. } => value && !matches!(kind, RefKind::Cell { .. }),
        Expr::Binary { op, left, right } => {
            if matches!(op, BinOp::Range | BinOp::Intersect | BinOp::Union) {
                return value;
            }
            value_array(left, true) || value_array(right, true)
        }
        Expr::Func { name, args } => {
            let upper = name.to_uppercase();
            let bare = upper.strip_prefix("_XLFN.").unwrap_or(&upper);
            if value && bare != "XLOOKUP" && returns_array(bare, args) {
                return true;
            }

            let params = (bare == "XLOOKUP")
                .then_some("VRV")
                .or_else(|| legacy_params(bare))
                .or_else(|| {
                    let def = functions::def(functions::lookup(bare)?);
                    Some(if matches!(def.imp, functions::Imp::Lazy(_)) {
                        "V"
                    } else {
                        def.params
                    })
                });
            let b = params.unwrap_or("V").as_bytes();
            args.iter().enumerate().any(|(i, a)| {
                let letter = if b.is_empty() {
                    b'V'
                } else {
                    b[i.min(b.len() - 1)]
                };

                let written_ref = matches!(a.unwrap_ws(), Expr::Ref { .. } | Expr::Name { .. })
                    || matches!(
                        a.unwrap_ws(),
                        Expr::Binary {
                            op: BinOp::Range | BinOp::Intersect | BinOp::Union,
                            ..
                        }
                    );
                let v = letter == b'V' || (value && bare == "INDEX" && i == 0 && !written_ref);
                value_array(a, v)
            })
        }
        Expr::Call { callee, args } => {
            value_array(callee, false) || args.iter().any(|a| value_array(a, true))
        }
        _ => false,
    })
}

pub(crate) fn text_may_array(wb: &Workbook, body: &str, sheet: crate::address::SheetId) -> bool {
    let Ok(e) = parse(body) else {
        return false;
    };
    UNDEFINED.with(|u| u.set(false));
    may_array(&e, wb, sheet, &mut Vec::new()) || UNDEFINED.with(|u| u.get())
}
