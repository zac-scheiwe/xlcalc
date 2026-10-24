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
    "XLOOKUP",
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

fn legacy_params(name: &str) -> Option<&'static str> {
    Some(match name {
        "MATCH" => "VRV",
        "VLOOKUP" | "HLOOKUP" => "VRVV",
        "TRANSPOSE" => "V",
        "IFERROR" | "IFNA" => "VV",
        _ => return None,
    })
}

thread_local! {

    static MULTI_NAMES: std::cell::RefCell<Vec<String>> = const { std::cell::RefCell::new(Vec::new()) };
}

fn is_multi_ref(e: &Expr) -> bool {
    match e {
        Expr::Name { name, .. } => {
            let up = name.to_uppercase();
            MULTI_NAMES.with(|m| m.borrow().contains(&up))
        }
        Expr::Ref { kind, .. } => match kind {
            RefKind::Cell { .. } => false,

            _ => true,
        },
        Expr::Binary {
            op: BinOp::Range, ..
        } => true,

        Expr::Spill(_) => true,
        _ => false,
    }
}

fn at(e: Expr) -> Expr {
    Expr::Unary {
        op: UnaryOp::Implicit,
        expr: Box::new(e),
    }
}

fn array_capable(e: &Expr) -> bool {
    match e.unwrap_ws() {
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
    }
}

fn returns_array(name: &str, args: &[Expr]) -> bool {
    let zero_or_missing = |e: &Expr| matches!(e.unwrap_ws(), Expr::Missing | Expr::Number(0.0));
    match name {
        "INDEX" => args.iter().skip(1).any(zero_or_missing),
        "OFFSET" => args.len() > 3 || args.first().is_some_and(|a| is_multi_ref(a.unwrap_ws())),
        "ROW" | "COLUMN" => args.first().is_some_and(|a| is_multi_ref(a.unwrap_ws())),
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
                let c = if bare == "TRANSPOSE" {
                    Ctx::Value
                } else {
                    Ctx::Array
                };
                Expr::Func {
                    name,
                    args: args.into_iter().map(|a| walk(a, c)).collect(),
                }
            }
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
        Expr::Array(rows) => {
            let multi = rows.len() > 1 || rows.first().is_some_and(|r| r.len() > 1);
            if ctx == Ctx::Value && multi {
                at(Expr::Array(rows))
            } else {
                Expr::Array(rows)
            }
        }
        Expr::Unary { op, expr } if op != UnaryOp::Implicit => Expr::Unary {
            op,
            expr: Box::new(walk(*expr, Ctx::Value)),
        },
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
        Expr::Func { name, args } => {
            let upper = name.to_uppercase();
            let bare = upper.strip_prefix("_XLFN.").unwrap_or(&upper).to_string();
            let args = func_args(&bare, args);

            let rowcol = matches!(bare.as_str(), "ROW" | "COLUMN");
            let call = Expr::Func { name, args };
            if (ctx == Ctx::Value || rowcol) && array_capable(&call) {
                at(call)
            } else {
                call
            }
        }
        e => e,
    }
}

fn func_args(name: &str, args: Vec<Expr>) -> Vec<Expr> {
    if ARRAY_ARGS.contains(&name) {
        return args.into_iter().map(|a| walk(a, Ctx::Array)).collect();
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
            walk(a, c)
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

pub fn legacy_to_dynamic(text: &str) -> String {
    match parse(text) {
        Ok(e) => format!("={}", print(&walk(e, Ctx::Value))),
        Err(_) => format!("={text}"),
    }
}

impl Workbook {
    pub fn set_legacy_input(
        &mut self,
        addr: CellAddr,
        input: &str,
    ) -> Result<RecalcReport, EditError> {
        match input::interpret(input, &self.input_env()) {
            Entered::Formula(text) => {
                let multi: Vec<String> = self
                    .names
                    .iter()
                    .filter(|n| {
                        parse(n.refers_to.trim_start_matches('=')).is_ok_and(|e| is_multi_ref(&e))
                    })
                    .map(|n| n.name.to_uppercase())
                    .collect();
                MULTI_NAMES.with(|m| *m.borrow_mut() = multi);
                let da = legacy_to_dynamic(&text);
                MULTI_NAMES.with(|m| m.borrow_mut().clear());
                self.set_input(addr, &da)
            }
            _ => self.set_input(addr, input),
        }
    }

    pub fn legacy_formula_text(&self, addr: CellAddr) -> Option<String> {
        let t = self.formula_text(addr)?;

        if t.contains("!#REF!") && !t.contains('@') {
            return Some(t);
        }
        match parse(&t) {
            Ok(e) => Some(format!("={}", print(&strip_at(e)))),
            Err(_) => Some(t),
        }
    }
}

pub(crate) fn legacy_quiet_expr(text: &str) -> Option<Expr> {
    let e = parse(text.trim_start_matches('=')).ok()?;
    Some(rewrite_at(walk(e, Ctx::Value), true))
}
