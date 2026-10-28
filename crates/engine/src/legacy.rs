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
        "LOOKUP" => "VRR",
        "TRANSPOSE" => "V",
        "IFERROR" | "IFNA" => "VV",
        _ => return None,
    })
}

thread_local! {

    static MULTI_NAMES: std::cell::RefCell<Vec<String>> = const { std::cell::RefCell::new(Vec::new()) };

    static KNOWN_NAMES: std::cell::RefCell<Option<Vec<String>>> = const { std::cell::RefCell::new(None) };
}

fn undefined_name(name: &str) -> bool {
    let up = name.to_uppercase();
    KNOWN_NAMES.with(|k| k.borrow().as_ref().is_some_and(|v| !v.contains(&up)))
}

fn bound_names(e: &Expr, out: &mut Vec<String>) {
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
    if functions::lookup(name).is_none() && undefined_name(name) {
        return true;
    }

    let zero_or_missing = |e: &Expr| {
        matches!(
            e.unwrap_ws(),
            Expr::Missing | Expr::Number(0.0) | Expr::Name { .. }
        )
    };
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
    if name == "LET" && args.len() % 2 == 1 {
        let n = args.len();
        let mut out = Vec::with_capacity(n);
        let mut added = Vec::new();
        let mut it = args.into_iter();
        while out.len() + 1 < n {
            let var = it.next().unwrap();
            let val = it.next().unwrap();
            if is_multi_ref(val.unwrap_ws()) {
                if let Expr::Name { name, .. } = var.unwrap_ws() {
                    added.push(name.to_uppercase());
                }
            }
            out.push(var);
            out.push(walk(val, Ctx::Ref));
            MULTI_NAMES.with(|m| m.borrow_mut().extend(added.drain(..)));
        }
        out.push(walk(it.next().unwrap(), Ctx::Value));
        return out;
    }
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
                let multi: Vec<String> = self
                    .names
                    .iter()
                    .filter(|n| {
                        parse(n.refers_to.trim_start_matches('=')).is_ok_and(|e| is_multi_ref(&e))
                    })
                    .map(|n| n.name.to_uppercase())
                    .collect();
                MULTI_NAMES.with(|m| *m.borrow_mut() = multi);
                let da = legacy_to_dynamic_in(self, &text);
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

        if t.contains("@#REF!")
            || t.contains("@INDEX(#REF!")
            || t.contains("@OFFSET(#REF!")
            || (t.contains("@CHOOSE(") && t.contains(",#REF!,#REF!)"))
        {
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
    match e {
        Expr::Ws { expr, .. } | Expr::Paren(expr) | Expr::Percent(expr) => {
            may_array(expr, wb, sheet, locals)
        }
        Expr::Number(_) | Expr::Text(_) | Expr::Bool(_) | Expr::Error(_) | Expr::Missing => false,
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
                (letter == b'V' || !reference(a)) && may_array(a, wb, sheet, locals)
            })
        }
        _ => true,
    }
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
    match e {
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
    }
}

fn collect_names<'a>(e: &'a Expr, out: &mut Vec<&'a str>) {
    match e {
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
    }
}

fn multi_names(wb: &Workbook) -> Vec<String> {
    wb.names
        .iter()
        .filter(|n| parse(n.refers_to.trim_start_matches('=')).is_ok_and(|e| is_multi_ref(&e)))
        .map(|n| n.name.to_uppercase())
        .collect()
}

pub(crate) fn scalar_entry_expr(wb: &Workbook, body: &str, host: CellAddr) -> Option<Expr> {
    if body.contains('@') || body.contains('[') || body.contains("#REF!") || body.contains("Book1!")
    {
        return None;
    }
    let e = parse(body).ok()?;
    UNDEFINED.with(|u| u.set(false));

    if uses_name(&e, wb) != Some(true)
        || may_array(&e, wb, host.sheet, &mut Vec::new())
        || UNDEFINED.with(|u| u.get())
    {
        return None;
    }
    legacy_eval_expr(wb, body)
}

pub(crate) fn legacy_eval_expr(wb: &Workbook, body: &str) -> Option<Expr> {
    let e = parse(body).ok()?;
    MULTI_NAMES.with(|m| *m.borrow_mut() = multi_names(wb));
    let w = walk(e, Ctx::Value);
    MULTI_NAMES.with(|m| m.borrow_mut().clear());
    Some(Expr::Unary {
        op: UnaryOp::ImplicitQuiet,
        expr: Box::new(rewrite_at(w, true)),
    })
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
    match e {
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
    }
}

pub(crate) fn text_may_array(wb: &Workbook, body: &str, sheet: crate::address::SheetId) -> bool {
    let Ok(e) = parse(body) else {
        return false;
    };
    UNDEFINED.with(|u| u.set(false));
    may_array(&e, wb, sheet, &mut Vec::new()) || UNDEFINED.with(|u| u.get())
}
