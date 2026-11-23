use crate::address::CellAddr;
use crate::calc::RecalcReport;
use crate::eval::ptgclass;
use crate::functions;
use crate::input::{self, Entered};
use crate::parser::ast::{BinOp, Expr, RefKind, SheetSpec, UnaryOp};
use crate::parser::{parse, print::print};
use crate::workbook::{notice, Command};
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

    static NAME_DEFS: std::cell::RefCell<Vec<(String, u8, bool)>> = const { std::cell::RefCell::new(Vec::new()) };

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

fn is_span3d(e: &Expr) -> bool {
    matches!(e, Expr::Ref { sheet: Some(SheetSpec::Range(a, b)), .. } if !a.eq_ignore_ascii_case(b))
}

enum StructKind {
    Single,

    Column(String),

    Multi,
}

fn struct_kind(spec: &str) -> StructKind {
    let spec = spec.trim();

    let items: Vec<&str> = if spec.starts_with('[') {
        spec.split(',').map(str::trim).collect()
    } else {
        vec![spec]
    };
    let inner = |s: &str| -> String {
        let s = s.trim();
        s.strip_prefix('[')
            .and_then(|x| x.strip_suffix(']'))
            .unwrap_or(s)
            .to_string()
    };
    if let Some(rest) = spec.strip_prefix('@') {
        let rest = rest.trim();
        return if rest.is_empty() || rest.contains("]:[") || rest.contains("]:") {
            StructKind::Multi
        } else {
            StructKind::Single
        };
    }
    let mut cols = Vec::new();
    let mut line = false;
    for it in &items {
        let name = inner(it);
        let low = name.to_lowercase();
        if matches!(low.as_str(), "#headers" | "#totals" | "#this row") {
            line = true;
        } else if low.starts_with('#') {
            return StructKind::Multi;
        } else {
            cols.push(it.to_string());
        }
    }
    let one_column = cols.len() == 1 && !cols[0].contains("]:[") && !inner(&cols[0]).contains(':');
    match (line, one_column, cols.len()) {
        (true, true, _) => StructKind::Single,
        (false, true, 1) if items.len() == 1 => StructKind::Column(inner(&cols[0])),
        _ => StructKind::Multi,
    }
}

fn is_single_cell(e: &Expr) -> bool {
    match e.unwrap_ws() {
        Expr::StructRef { spec, .. } => matches!(struct_kind(spec), StructKind::Single),

        x @ Expr::Name { .. } => defined_name(x).is_some_and(|(i, _)| i & 3 == 0),
        x => {
            matches!(
                x,
                Expr::Ref {
                    kind: RefKind::Cell { .. },
                    ..
                }
            ) && !is_span3d(x)
        }
    }
}

fn is_undefined_name(e: &Expr) -> bool {
    matches!(e.unwrap_ws(), Expr::Name { sheet: None, name } if undefined_name(name))
}

fn is_multi_ref(e: &Expr) -> bool {
    match e {
        Expr::StructRef { spec, .. } => !matches!(struct_kind(spec), StructKind::Single),
        Expr::Name { name, sheet } => {
            if let Some((info, _)) = defined_name(e) {
                return info & 3 != 0;
            }
            let up = name.to_uppercase();

            let book = match sheet {
                Some(SheetSpec::Single(b)) => Some(format!("{}!{up}", b.to_uppercase())),
                _ => None,
            };
            MULTI_NAMES.with(|m| {
                let m = m.borrow();
                m.contains(&up) || book.is_some_and(|k| m.contains(&k))
            })
        }

        Expr::Ref { kind, .. } => !matches!(kind, RefKind::Cell { .. }) || is_span3d(e),

        Expr::Binary {
            op: BinOp::Intersect,
            left,
            right,
        } => !is_single_cell(left) && !is_single_cell(right),
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

fn walk_ws(before: String, after: String, expr: Expr, ctx: Ctx) -> Expr {
    let typed = matches!(
        expr,
        Expr::Unary {
            op: UnaryOp::Implicit,
            ..
        }
    );
    match walk(expr, ctx) {
        Expr::Unary {
            op: UnaryOp::Implicit,
            expr: inner,
        } if !typed => at(Expr::Ws {
            before,
            after,
            expr: inner,
        }),
        walked => Expr::Ws {
            before,
            after,
            expr: Box::new(walked),
        },
    }
}

fn modelled(bare: &str) -> Option<&'static crate::legacy_ftab::Ftab> {
    if matches!(bare, "LET" | "LAMBDA") {
        return None;
    }
    if !crate::legacy_classes::is_known(bare) {
        return None;
    }
    crate::legacy_single::ftab(bare)
}

fn one_dimensional(e: &Expr) -> bool {
    match e.unwrap_ws() {
        n @ Expr::Name { .. } => defined_name(n).is_some_and(|(_, d)| d),
        x => area_1d(x),
    }
}

fn lifted_operand(e: &Expr) -> bool {
    match e.unwrap_ws() {
        Expr::Array(rows) => rows.len() > 1 || rows.first().is_some_and(|r| r.len() > 1),
        f @ Expr::Func { name, .. } => {
            let upper = name.to_uppercase();
            let bare = upper.strip_prefix("_XLFN.").unwrap_or(&upper);
            is_native_array_call(f)
                && modelled(bare).is_some_and(|ft| ft.flags & 0x40 != 0 && ft.flags & 0x8 == 0)
                && info_of(f, true) & 1 != 0
                && can_return_multi(f)
        }
        _ => false,
    }
}

fn can_return_multi(e: &Expr) -> bool {
    match e.unwrap_ws() {
        Expr::Paren(x) => can_return_multi(x),
        Expr::Func { name, .. } => {
            let upper = name.to_uppercase();
            let bare = upper.strip_prefix("_XLFN.").unwrap_or(&upper);
            modelled(bare).is_none_or(crate::legacy_single::can_return_multi)
        }
        Expr::Unary { .. } | Expr::Percent(_) | Expr::Number(_) => false,
        Expr::Binary { op, .. } => matches!(op, BinOp::Range | BinOp::Intersect | BinOp::Union),
        _ => true,
    }
}

fn value_info(e: &Expr) -> u8 {
    let i = info_of(e, true);
    if i & 3 != 0 && can_return_multi(e) {
        0
    } else {
        i
    }
}

fn operand_info(e: &Expr, vctx: bool) -> u8 {
    if vctx {
        value_info(e)
    } else {
        info_of(e, false)
    }
}

fn call_info(name: &str, ft: &crate::legacy_ftab::Ftab, args: &[Expr], vctx: bool) -> u8 {
    let argc = args.len();

    let own = ft.flags & 0x48 != 0 && !PASS_THROUGH.contains(&name);
    let a: Vec<u8> = args
        .iter()
        .enumerate()
        .map(|(i, x)| {
            let class = crate::legacy_classes::class_of_n(name, i, argc);
            let g = crate::legacy_classes::grbit_at(name, i).unwrap_or(0);

            let value = if lifted_operand(x) || is_native_array_call(x.unwrap_ws()) {
                g & 0x40 == 0
            } else {
                class == Some(b'V')
            } && (vctx || own);
            if value {
                value_info(x)
            } else {
                info_of(x, false)
            }
        })
        .collect();
    let one_d = argc > 2 && one_dimensional(&args[1]) && one_dimensional(&args[2]);
    crate::legacy_single::call_flags(ft, &a, one_d)
}

fn info_of(e: &Expr, vctx: bool) -> u8 {
    use crate::legacy_single as ls;
    crate::deep(|| match e.unwrap_ws() {
        Expr::Paren(x) => info_of(x, vctx),
        Expr::Unary {
            op: UnaryOp::Implicit,
            ..
        } => 0,
        Expr::Number(n) => ls::int_flags(*n),
        Expr::Array(_) => 1,
        Expr::Unary { expr, .. } | Expr::Percent(expr) => {
            ls::operator_flags(&[operand_info(expr, vctx)])
        }
        Expr::Binary { op, left, right } => match op {
            BinOp::Range | BinOp::Union => {
                ls::range_flags(info_of(left, false), info_of(right, false), false)
            }
            BinOp::Intersect => ls::range_flags(info_of(left, false), info_of(right, false), true),
            _ => ls::operator_flags(&[operand_info(left, vctx), operand_info(right, vctx)]),
        },
        Expr::Spill(x) => ls::call_flags(
            ls::ftab("ANCHORARRAY").unwrap(),
            &[info_of(x, false)],
            false,
        ),
        x @ Expr::Func { name, args } => {
            let upper = name.to_uppercase();
            let bare = upper.strip_prefix("_XLFN.").unwrap_or(&upper);
            match modelled(bare) {
                Some(ft) => call_info(bare, ft, args, vctx),
                None if !matches!(bare, "LET" | "LAMBDA")
                    && functions::lookup(bare).is_none()
                    && undefined_name(bare) =>
                {
                    3
                }
                None => {
                    if array_capable(x) {
                        3
                    } else {
                        0
                    }
                }
            }
        }
        Expr::Call { .. } => 3,
        x @ Expr::Name { .. } => {
            if let Some((info, _)) = defined_name(x) {
                info
            } else if is_multi_ref(x) {
                2
            } else if is_undefined_name(x) {
                3
            } else {
                0
            }
        }
        x @ Expr::Ref { kind, .. } => match kind {
            RefKind::Cell { .. } if is_span3d(x) => 3,
            RefKind::Cell { .. } => 0,
            _ => 2,
        },
        x if is_multi_ref(x) => 2,
        _ => 0,
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
    if let Some(ft) = modelled(name) {
        return call_info(name, ft, args, true) & 3 != 0
            && crate::legacy_single::can_return_multi(ft);
    }
    if name == "LET" && args.len() % 2 == 1 {
        return args.last().is_some_and(|r| match r.unwrap_ws() {
            n @ Expr::Name { .. } => is_multi_ref(n),
            Expr::Call { .. } => true,
            Expr::Func { name, .. } if functions::lookup(name).is_none() => true,
            x => array_capable(x),
        });
    }

    if name == "LAMBDA" {
        return false;
    }
    if functions::lookup(name).is_none() && undefined_name(name) {
        return true;
    }
    ARRAY_RESULT.contains(&name)
}

fn walk_root(e: Expr) -> Expr {
    if matches!(
        e.unwrap_ws(),
        Expr::Binary {
            op: BinOp::Intersect,
            left,
            right,
        } if is_single_cell(left) || is_single_cell(right)
    ) {
        let undefined = matches!(e.unwrap_ws(), Expr::Binary { left, right, .. }
            if is_undefined_name(left) || is_undefined_name(right));
        return if undefined { at(e) } else { e };
    }
    walk(e, Ctx::Value)
}

thread_local! {

    static CW_ANC: std::cell::RefCell<Vec<ptgclass::Anc>> = const { std::cell::RefCell::new(Vec::new()) };
    static CW_SLOT: std::cell::Cell<Option<u32>> = const { std::cell::Cell::new(None) };
    static CW_CLS: std::cell::Cell<ptgclass::Class> = const { std::cell::Cell::new(ptgclass::Class::Val) };
}

struct CwRestore(Option<Vec<ptgclass::Anc>>);

impl Drop for CwRestore {
    fn drop(&mut self) {
        if let Some(a) = self.0.take() {
            CW_ANC.with(|c| *c.borrow_mut() = a);
        }
    }
}

fn slot_arg(name: &str, i: usize) {
    slot(ptgclass::arg_mask(ptgclass::ftab_of_name(name), i));
}

fn slot(mask: u32) {
    CW_SLOT.with(|s| s.set(Some(mask)));
}

fn cw_kind(e: &Expr) -> ptgclass::Kind {
    use ptgclass::Kind;
    match e {
        Expr::Func { name, .. } => {
            let upper = name.to_uppercase();
            let bare = upper.strip_prefix("_XLFN.").unwrap_or(&upper);

            Kind::Call(ptgclass::ftab_of_name(bare).or_else(ptgclass::ftab_call))
        }
        Expr::Unary {
            op: UnaryOp::Implicit,
            ..
        } => Kind::Call(ptgclass::ftab_of_name("SINGLE")),
        Expr::Spill(_) => Kind::Call(ptgclass::ftab_of_name("ANCHORARRAY")),
        Expr::Call { .. } => Kind::Call(ptgclass::ftab_call()),
        Expr::Name { .. } | Expr::Ref { .. } | Expr::StructRef { .. } => {
            Kind::RefToken { array_const: false }
        }

        Expr::Binary {
            op: BinOp::Range | BinOp::Intersect | BinOp::Union,
            ..
        } => Kind::RefToken { array_const: false },
        Expr::Array(_) => Kind::RefToken { array_const: true },
        _ => Kind::Other,
    }
}

fn cw_on() -> bool {
    !EVAL_FORM.with(|f| f.get())
}

fn walk(e: Expr, ctx: Ctx) -> Expr {
    let mask = CW_SLOT
        .with(|s| s.take())
        .unwrap_or(ptgclass::MASK_OPERATOR);
    if matches!(e, Expr::Paren(_) | Expr::Ws { .. }) {
        slot(mask);
        return crate::deep(|| walk_inner(e, ctx));
    }
    let kind = cw_kind(&e);
    let cls = CW_ANC.with(|a| ptgclass::classify(kind, mask, &a.borrow(), false));

    let cls = if mask == ptgclass::MASK_RANGE && cls == ptgclass::Class::Val {
        ptgclass::Class::Ref
    } else {
        cls
    };
    CW_ANC.with(|a| a.borrow_mut().push(ptgclass::Anc::of(kind, mask)));
    let saved = CW_CLS.with(|c| c.replace(cls));
    let ctx = if cw_on() {
        if cls == ptgclass::Class::Val {
            Ctx::Value
        } else {
            Ctx::Ref
        }
    } else {
        ctx
    };
    let out = crate::deep(|| walk_inner(e, ctx));
    CW_CLS.with(|c| c.set(saved));
    CW_ANC.with(|a| a.borrow_mut().pop());
    out
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
            } => walk_ws(before, after, *expr, ctx),
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
        } => walk_ws(before, after, *expr, ctx),
        Expr::Paren(x) => Expr::Paren(Box::new(walk(*x, ctx))),
        Expr::StructRef { table, spec } if ctx == Ctx::Value => match struct_kind(&spec) {
            StructKind::Single => Expr::StructRef { table, spec },

            StructKind::Column(name) => {
                let simple =
                    !name.is_empty() && name.chars().all(|c| c.is_alphanumeric() || c == '_');
                Expr::StructRef {
                    table,
                    spec: if simple {
                        format!("@{name}")
                    } else {
                        format!("@[{name}]")
                    },
                }
            }
            StructKind::Multi => at(Expr::StructRef { table, spec }),
        },

        Expr::Binary {
            op: BinOp::Intersect,
            left,
            right,
        } if matches!(
            right.unwrap_ws(),
            Expr::Unary {
                op: UnaryOp::Implicit,
                ..
            }
        ) && ctx == Ctx::Value =>
        {
            Expr::Binary {
                op: BinOp::Intersect,
                left,
                right: Box::new(walk(*right, Ctx::Value)),
            }
        }

        Expr::Binary {
            op: op @ (BinOp::Range | BinOp::Intersect | BinOp::Union),
            left,
            right,
        } if index_with_array_index(&left) || index_with_array_index(&right) => {
            let fix = |x: Box<Expr>| {
                if index_with_array_index(&x) {
                    Box::new(walk(*x, Ctx::Ref))
                } else {
                    x
                }
            };
            let e = Expr::Binary {
                op,
                left: fix(left),
                right: fix(right),
            };
            if ctx == Ctx::Value {
                at(e)
            } else {
                e
            }
        }
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
            || matches!(expr.unwrap_ws(), Expr::Paren(x) if matches!(x.unwrap_ws(), Expr::Binary { op, .. }
                if !matches!(op, BinOp::Range | BinOp::Intersect | BinOp::Union)))
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
            slot(ptgclass::callee_mask());
            let callee = walk(*callee, Ctx::Ref);
            let args = args
                .into_iter()
                .enumerate()
                .map(|(i, a)| {
                    slot(ptgclass::arg_mask(ptgclass::ftab_call(), i));
                    walk(a, Ctx::Ref)
                })
                .collect();
            let call = Expr::Call {
                callee: Box::new(callee),
                args,
            };
            let wrap = if cw_on() {
                CW_CLS.with(|c| c.get()) == ptgclass::Class::Val
            } else {
                ctx == Ctx::Value && !in_agg()
            };
            if wrap {
                at(call)
            } else {
                call
            }
        }
        Expr::Func { name, args } => {
            let upper = name.to_uppercase();
            let bare = upper.strip_prefix("_XLFN.").unwrap_or(&upper).to_string();
            let model = modelled(&bare);

            let lookup = matches!(bare.as_str(), "VLOOKUP" | "HLOOKUP");
            let own_class = model.is_some_and(|ft| ft.flags & 0x48 != 0)
                && !PASS_THROUGH.contains(&bare.as_str())
                && !lookup;
            let saved_agg = IN_AGG.with(|f| f.get());
            if own_class {
                IN_AGG.with(|f| f.set(false));
            }

            let typed: Vec<bool> = args
                .iter()
                .map(|a| {
                    matches!(
                        a.unwrap_ws(),
                        Expr::Unary {
                            op: UnaryOp::Implicit,
                            ..
                        }
                    )
                })
                .collect();
            let args = func_args(&bare, args);
            let mut args = args;
            let agg_here = saved_agg && !own_class && !lookup;
            IN_AGG.with(|f| f.set(saved_agg));

            if model.is_some() {
                let args_len = args.len();
                for (i, a) in args.iter_mut().enumerate() {
                    let g = crate::legacy_classes::grbit_at(&bare, i).unwrap_or(0);

                    let refused = functions::lookup(&bare).is_some_and(|id| {
                        functions::def(id).param_class(i) == functions::ParamClass::Reference
                    }) || (bare == "AGGREGATE" && i >= 4);
                    if refused || typed.get(i).copied().unwrap_or(false) {
                        continue;
                    }

                    if agg_here {
                        continue;
                    }
                    let inner = match &*a {
                        Expr::Unary {
                            op: UnaryOp::Implicit,
                            expr,
                        } => expr.as_ref(),
                        x => x,
                    };

                    let lifted = lifted_operand(inner);

                    if !lifted && g & 0x40 != 0 && is_native_array_call(inner.unwrap_ws()) {
                        if let Expr::Unary {
                            op: UnaryOp::Implicit,
                            ..
                        } = &*a
                        {
                            if let Expr::Unary { expr, .. } =
                                std::mem::replace(a, Expr::Number(0.0))
                            {
                                *a = *expr;
                            }
                        }
                        continue;
                    }
                    if lifted {
                        let x = match std::mem::replace(a, Expr::Number(0.0)) {
                            Expr::Unary { expr, .. } => *expr,
                            x => x,
                        };

                        let value = if cw_on() {
                            let mask = crate::legacy_classes::mask_n(&bare, i, args_len)
                                .unwrap_or_else(|| {
                                    ptgclass::arg_mask(ptgclass::ftab_of_name(&bare), i)
                                });
                            CW_ANC.with(|anc| {
                                ptgclass::classify(
                                    cw_kind(x.unwrap_ws()),
                                    mask,
                                    &anc.borrow(),
                                    false,
                                )
                            }) == ptgclass::Class::Val
                        } else {
                            g & 0x40 == 0
                        };
                        *a = if value { at(x) } else { x };
                        continue;
                    }
                    let one_elem = (bare == "INDEX" && i >= 1) || (bare == "AGGREGATE" && i == 3);
                    if one_elem && matches!(a, Expr::Array(_)) {
                        let arr = std::mem::replace(a, Expr::Number(0.0));
                        *a = at(arr);
                    }
                }
            }

            let rowcol = matches!(
                bare.as_str(),
                "ROW" | "COLUMN" | "ISFORMULA" | "FORMULATEXT"
            );

            let hoisted = bare == "LET"
                && !cw_on()
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

            let mut lifted_call = false;
            let cw = cw_on();
            let needs = match model {
                Some(ft) if cw => {
                    let Expr::Func { args, .. } = &call else {
                        unreachable!()
                    };
                    let a: Vec<u8> = args.iter().map(|x| info_of(x, false)).collect();
                    let one_d =
                        args.len() > 2 && one_dimensional(&args[1]) && one_dimensional(&args[2]);
                    let info = crate::legacy_single::call_flags(ft, &a, one_d);
                    info & 3 != 0 && crate::legacy_single::can_return_multi(ft)
                }
                Some(ft) => {
                    let Expr::Func { args, .. } = &call else {
                        unreachable!()
                    };
                    {
                        let info = call_info(&bare, ft, args, ctx == Ctx::Value && !agg_here);

                        lifted_call =
                            info & 1 != 0 && ft.flags & 0x7 != 0 && ft.flags & 0x8 == 0 && !native;
                        info & 3 != 0 && crate::legacy_single::can_return_multi(ft)
                    }
                }

                None if cw && bare == "LET" => {
                    let Expr::Func { args, .. } = &call else {
                        unreachable!()
                    };
                    args.len() % 2 == 1
                        && args.last().is_some_and(|l| match l.unwrap_ws() {
                            Expr::Func { name, .. }
                                if functions::lookup(name).is_none()
                                    && !matches!(
                                        name.to_uppercase().as_str(),
                                        "LET" | "LAMBDA"
                                    ) =>
                            {
                                true
                            }
                            l => info_of(l, false) & 3 != 0,
                        })
                }
                None => array_capable(&call),
            };
            let is_val = CW_CLS.with(|c| c.get()) == ptgclass::Class::Val;
            if if cw && (model.is_some() || bare == "LET") {
                is_val && needs
            } else {
                (ctx == Ctx::Value || rowcol || lifted_call) && !native && needs
            } {
                match call {
                    Expr::Func { name, mut args } if lookup && args.len() > 2 => {
                        let x = std::mem::replace(&mut args[2], Expr::Number(0.0));
                        args[2] = at(x);
                        Expr::Func { name, args }
                    }
                    call => at(call),
                }
            } else {
                call
            }
        }
        e => e,
    }
}

fn index_with_array_index(e: &Expr) -> bool {
    matches!(e.unwrap_ws(), Expr::Func { name, args }
        if name.eq_ignore_ascii_case("INDEX")
            && args.iter().skip(1).any(|a| matches!(a.unwrap_ws(), Expr::Array(_))))
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

        let outer = CW_ANC.with(|a| std::mem::take(&mut *a.borrow_mut()));
        let _restore = CwRestore(Some(outer));

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
            slot_arg("LET", out.len());
            out.push(walk(val, Ctx::Ref));
            MULTI_NAMES.with(|m| m.borrow_mut().extend(added.drain(..)));
        }
        let last = it.next().unwrap();

        out.push(if matches!(last.unwrap_ws(), Expr::Name { .. }) {
            last
        } else if in_agg() && matches!(last.unwrap_ws(), Expr::Func { .. } | Expr::Ref { .. }) {
            slot_arg("LET", 0);
            walk(last, Ctx::Ref)
        } else {
            slot_arg("LET", 0);
            walk(last, Ctx::Value)
        });
        return out;
    }

    if crate::legacy_classes::is_known(name) && !EVAL_FORM.with(|f| f.get()) {
        let argc = args.len();
        return args
            .into_iter()
            .enumerate()
            .map(|(i, a)| {
                let c = match crate::legacy_classes::class_of_n(name, i, argc).unwrap_or(b'R') {

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

                if let Some(m) = crate::legacy_classes::mask_n(name, i, argc) {
                    slot(m);
                } else {
                    slot_arg(name, i);
                }
                if PASS_THROUGH.contains(&name) {
                    walk(a, c)
                } else {
                    walk_agg(a, c)
                }
            })
            .collect();
    }
    if ARRAY_ARGS.contains(&name) {
        return args
            .into_iter()
            .enumerate()
            .map(|(i, a)| {
                slot_arg(name, i);
                walk_agg(a, Ctx::Array)
            })
            .collect();
    }

    if matches!(name, "IF" | "CHOOSE") {
        return args
            .into_iter()
            .enumerate()
            .map(|(i, a)| {
                slot_arg(name, i);
                walk(a, if i == 0 { Ctx::Value } else { Ctx::Ref })
            })
            .collect();
    }
    let params = match legacy_params(name) {
        Some(p) => p,
        None => {
            let Some(id) = functions::lookup(name) else {
                if functions::xlm::XLM_ONLY.contains(&name) {
                    return args;
                }

                return args
                    .into_iter()
                    .map(|a| {
                        slot(ptgclass::MASK_REF_ARRAY);
                        walk_agg(a, Ctx::Ref)
                    })
                    .collect();
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
            slot_arg(name, i);
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

fn strip_known_prefixes(e: Expr) -> Expr {
    let go = |x: Box<Expr>| Box::new(strip_known_prefixes(*x));
    match e {
        Expr::Func { name, args } => {
            let bare = crate::formula::strip_prefixes(&name);
            let u = bare.to_uppercase();
            let known = matches!(u.as_str(), "LET" | "LAMBDA") || functions::lookup(&u).is_some();
            Expr::Func {
                name: if known { bare.to_string() } else { name },
                args: args.into_iter().map(strip_known_prefixes).collect(),
            }
        }
        Expr::Unary { op, expr } => Expr::Unary { op, expr: go(expr) },
        Expr::Ws {
            before,
            after,
            expr,
        } => Expr::Ws {
            before,
            after,
            expr: go(expr),
        },
        Expr::Paren(x) => Expr::Paren(go(x)),
        Expr::Percent(x) => Expr::Percent(go(x)),
        Expr::Spill(x) => Expr::Spill(go(x)),
        Expr::Binary { op, left, right } => Expr::Binary {
            op,
            left: go(left),
            right: go(right),
        },
        Expr::Call { callee, args } => Expr::Call {
            callee: go(callee),
            args: args.into_iter().map(strip_known_prefixes).collect(),
        },
        e => e,
    }
}

pub(crate) fn legacy_to_dynamic_in(wb: &Workbook, text: &str) -> String {
    let text = input::auto_close_call(text);
    let Ok(e) = parse(&text) else {
        return format!("={text}");
    };
    let e = strip_known_prefixes(e);

    let mut known: Vec<String> = wb
        .names
        .iter()
        .map(|n| n.name.to_uppercase())
        .filter(|k| {
            wb.names.macro_type(k, None).is_none()
                && wb
                    .sheets()
                    .all(|s| wb.names.macro_type(k, Some(s.id)).is_none())
        })
        .collect();
    bound_names(&e, &mut known);
    KNOWN_NAMES.with(|k| *k.borrow_mut() = Some(known));
    let out = format!("={}", print(&walk_root(e)));
    KNOWN_NAMES.with(|k| *k.borrow_mut() = None);
    out
}

fn mark_quoted_names(t: &str) -> String {
    let b: Vec<char> = t.chars().collect();
    let mut out = String::new();
    let (mut i, mut in_str) = (0, false);
    while i < b.len() {
        if b[i] == '"' {
            in_str = !in_str;
        }
        if !in_str && b[i] == '\'' {
            if let Some(len) = b[i + 1..].iter().position(|&c| c == '\'') {
                let w: String = b[i + 1..i + 1 + len].iter().collect();
                let after = b.get(i + len + 2);
                if crate::parser::r1c1::forced_name(&w)
                    && !matches!(after, Some('!' | ':'))
                    && !(i > 0 && matches!(b[i - 1], '!' | ':'))
                {
                    out.push('\u{1}');
                    out.push_str(&w);
                    i += len + 2;
                    continue;
                }
            }
        }
        out.push(b[i]);
        i += 1;
    }
    out
}

fn marks_to_quotes(t: &str) -> String {
    let mut out = String::new();
    let mut word = false;
    for c in t.chars() {
        if c == '\u{1}' {
            out.push('\'');
            word = true;
        } else if word && !(c.is_alphanumeric() || matches!(c, '_' | '.' | '\\' | '?')) {
            out.push('\'');
            out.push(c);
            word = false;
        } else {
            out.push(c);
        }
    }
    if word {
        out.push('\'');
    }
    out
}

fn legacy_loses_marks(wb: &Workbook, legacy: &str, shown: &str) -> bool {
    let Some(body) = legacy.strip_prefix('=') else {
        return false;
    };
    MULTI_NAMES.with(|m| *m.borrow_mut() = multi_names(wb));
    let da = legacy_to_dynamic_in(wb, &mark_quoted_names(body));
    clear_names();
    let marks = |t: &str| t.matches('@').count();
    marks(&da) < marks(shown)
}

pub(crate) fn legacy_reads_back(wb: &Workbook, legacy: &str, shown: &str) -> bool {
    let Some(body) = legacy.strip_prefix('=') else {
        return false;
    };
    MULTI_NAMES.with(|m| *m.borrow_mut() = multi_names(wb));
    let da = legacy_to_dynamic_in(wb, body);
    clear_names();
    da == shown
}

pub(crate) fn legacy_entry_active() -> bool {
    LEGACY_ENTRY.with(|l| l.get())
}

pub(crate) fn legacy_entry_is_scalar(wb: &Workbook, shown: &str, host: CellAddr) -> bool {
    let Some(body) = shown.strip_prefix('=') else {
        return false;
    };
    LEGACY_ENTRY.with(|l| l.set(true));
    let r = scalar_entry_expr(wb, body, host).is_some();
    LEGACY_ENTRY.with(|l| l.set(false));
    r
}

impl Workbook {
    pub fn set_legacy_input(
        &mut self,
        addr: CellAddr,
        input: &str,
    ) -> Result<RecalcReport, EditError> {
        self.command(Command::Entry, |wb| {
            wb.note_changed(addr.sheet, &[notice::cell(addr)]);
            wb.set_legacy_input_run(addr, input)
        })
    }

    fn set_legacy_input_run(
        &mut self,
        addr: CellAddr,
        input: &str,
    ) -> Result<RecalcReport, EditError> {
        match input::interpret(input, &self.input_env()) {
            Entered::Formula(text)
                if text.starts_with('=') || text.trim_matches(' ').is_empty() =>
            {
                Err(EditError::Parse(crate::parser::ParseError {
                    pos: 0,
                    message: "empty or doubled equals sign".into(),
                }))
            }
            Entered::Formula(text) => {
                let text = crate::parser::r1c1::rewrite(&text, addr.row, addr.col).unwrap_or(text);
                let multi = multi_names(self);
                MULTI_NAMES.with(|m| *m.borrow_mut() = multi);
                let da = legacy_to_dynamic_in(self, &text);
                clear_names();
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

        if self.array_at(addr).is_some() {
            return Some(t);
        }

        if t.contains("!#REF!") && !t.contains('@') {
            return Some(t);
        }

        if t.replace("@#REF!:", "")
            .replace("@#REF!#", "")
            .contains("@#REF!")
            || t.contains("@INDEX(#REF!")
            || t.contains("@OFFSET(#REF!")
            || (t.contains("@CHOOSE(") && t.contains(",#REF!,#REF!)"))
        {
            return Some(t);
        }
        match parse(&mark_quoted_names(&t)) {
            Ok(e) => {
                KEPT_AT.with(|k| k.set(false));
                let out = marks_to_quotes(&format!("={}", print(&strip_at_keep_cell(e))));

                if KEPT_AT.with(|k| k.replace(false))
                    || (out != t && legacy_loses_marks(self, &out, &t))
                {
                    Some(t)
                } else {
                    Some(out)
                }
            }
            Err(_) => Some(t),
        }
    }

    pub fn legacy_formula_text_r1c1(&self, addr: CellAddr) -> Option<String> {
        let r1c1 = self.formula_text_r1c1(addr)?;
        let shown = self.formula_text(addr)?;
        let legacy = self.legacy_formula_text(addr)?;

        let marks = |t: &str| -> Vec<usize> {
            let (mut quote, mut out) = (None::<char>, Vec::new());
            for (i, c) in t.char_indices() {
                match (quote, c) {
                    (Some(q), c) if c == q => quote = None,
                    (Some(_), _) => {}
                    (None, '"' | '\'') => quote = Some(c),
                    (None, '@') => out.push(i),
                    _ => {}
                }
            }
            out
        };
        let (ms, ml, mr) = (marks(&shown), marks(&legacy), marks(&r1c1));
        if ms.len() != mr.len() || ml.len() > ms.len() {
            return Some(r1c1);
        }

        let (sb, lb) = (shown.as_bytes(), legacy.as_bytes());
        let (mut i, mut j, mut k) = (0, 0, 0);
        let mut drop = Vec::new();
        while i < sb.len() {
            if ms.get(k) == Some(&i) {
                if j < lb.len() && lb[j] == b'@' && ml.contains(&j) {
                    j += 1;
                } else {
                    drop.push(mr[k]);
                }
                k += 1;
                i += 1;
            } else if j < lb.len() && sb[i] == lb[j] {
                i += 1;
                j += 1;
            } else {
                return Some(r1c1);
            }
        }
        let mut out = r1c1;
        for at in drop.into_iter().rev() {
            out.remove(at);
        }
        Some(out)
    }
}

fn contains_implicit(e: &Expr) -> bool {
    match e {
        Expr::Unary {
            op: UnaryOp::Implicit,
            ..
        } => true,
        Expr::Binary { left, right, .. } => contains_implicit(left) || contains_implicit(right),
        Expr::Ws { expr, .. } => contains_implicit(expr),
        _ => false,
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
        ) && !is_span3d(expr.unwrap_ws()) =>
        {
            KEPT_AT.with(|k| k.set(true));
            Expr::Unary {
                op: UnaryOp::Implicit,
                expr,
            }
        }

        Expr::Unary {
            op: UnaryOp::Implicit,
            expr,
        } if matches!(
            expr.unwrap_ws(),
            Expr::Number(_) | Expr::Text(_) | Expr::Bool(_) | Expr::Error(_)
        ) || matches!(
            expr.unwrap_ws(),
            Expr::Unary {
                op: UnaryOp::Implicit,
                ..
            }
        ) || matches!(expr.unwrap_ws(), Expr::Unary { op: UnaryOp::Neg | UnaryOp::Plus, expr: x }
                if matches!(x.unwrap_ws(), Expr::Number(_) | Expr::Unary { op: UnaryOp::Implicit, .. }))
            || matches!(expr.unwrap_ws(), Expr::Paren(_))
            || matches!(expr.unwrap_ws(), Expr::Func { name, args }
                if !returns_array(name, args)
                    && functions::lookup(&name.to_uppercase()).is_some()
                    && args.iter().all(|a| matches!(a.unwrap_ws(),
                        Expr::Number(_) | Expr::Text(_) | Expr::Bool(_) | Expr::Error(_) | Expr::Missing))) =>
        {
            KEPT_AT.with(|k| k.set(true));
            Expr::Unary {
                op: UnaryOp::Implicit,
                expr,
            }
        }
        Expr::Func { name, args } => {
            let upper = name.to_uppercase();
            let bare = upper.strip_prefix("_XLFN.").unwrap_or(&upper);
            let argc = args.len();
            let args = args
                .into_iter()
                .enumerate()
                .map(|(i, a)| {
                    let class = crate::legacy_classes::class_of_n(bare, i, argc);

                    let q = class == Some(b'Q') || matches!(class, Some(b'R' | b'A'));
                    match a {
                        Expr::Unary {
                            op: UnaryOp::Implicit,
                            expr,
                        } if q
                            && (class == Some(b'Q')
                                || !matches!(
                                    expr.unwrap_ws(),
                                    Expr::Func { .. } | Expr::Call { .. }
                                )) =>
                        {
                            Expr::Unary {
                                op: UnaryOp::Implicit,
                                expr: Box::new(strip_at_keep_cell(*expr)),
                            }
                        }
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

        Expr::Paren(x)
            if matches!(
                &*x,
                Expr::Binary {
                    op: BinOp::Union,
                    ..
                }
            ) && contains_implicit(&x) =>
        {
            KEPT_AT.with(|k| k.set(true));
            Expr::Paren(x)
        }
        Expr::Paren(x) => Expr::Paren(Box::new(strip_at_keep_cell(*x))),

        e @ Expr::Binary {
            op: BinOp::Intersect,
            ..
        } if matches!(&e, Expr::Binary { right, .. }
            if matches!(right.unwrap_ws(), Expr::Unary { op: UnaryOp::Implicit, .. })) =>
        {
            KEPT_AT.with(|k| k.set(true));
            e
        }
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
    Some(rewrite_at(walk_root(e), true))
}

thread_local! {

    static KEPT_AT: std::cell::Cell<bool> = const { std::cell::Cell::new(false) };
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
            if let Some(crate::parser::ast::SheetSpec::Single(b)) = q {
                if wb.sheet_by_name(b).is_none() {
                    if let Some(scalar) = wb.ext_name_scalar(b, name) {
                        return !scalar;
                    }
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
    if names.iter().any(|(_, n)| wb.table_by_name(n).is_some()) {
        return None;
    }

    let mut vars = Vec::new();
    let_vars(e, &mut vars);
    let defined = |q: &Option<SheetSpec>, n: &str| {
        let k = n.to_uppercase();

        if let Some(SheetSpec::Single(b)) = q {
            if wb.sheet_by_name(b).is_none() && wb.ext_name_scalar(b, n).is_some() {
                return true;
            }
        }
        wb.names.iter().any(|d| d.name.to_uppercase() == k) || vars.contains(&k)
    };
    if !names.iter().all(|(q, n)| defined(q, n)) {
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

fn collect_names<'a>(e: &'a Expr, out: &mut Vec<(&'a Option<SheetSpec>, &'a str)>) {
    crate::deep(|| match e {
        Expr::Name { name, sheet } => out.push((sheet, name)),
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

fn first_ptg(e: &Expr) -> &Expr {
    match e {
        Expr::Ws { expr, .. }
        | Expr::Paren(expr)
        | Expr::Percent(expr)
        | Expr::Spill(expr)
        | Expr::Unary { expr, .. }
        | Expr::Field { expr, .. } => first_ptg(expr),

        Expr::Binary {
            op: BinOp::Range | BinOp::Intersect | BinOp::Union,
            ..
        } => e,
        Expr::Binary { left, .. } => first_ptg(left),
        Expr::Func { args, .. } => args.first().map_or(e, first_ptg),
        Expr::Call { callee, .. } => first_ptg(callee),
        e => e,
    }
}

fn name_def_info(def: &Expr) -> u8 {
    match def.unwrap_ws() {
        Expr::Number(n) => crate::legacy_single::int_flags(*n),
        Expr::Text(_) | Expr::Bool(_) | Expr::Error(_) | Expr::DeadRef(_) => 0,
        x @ Expr::Ref { kind, .. } => match kind {
            RefKind::Cell { .. } if is_span3d(x) => 3,
            RefKind::Cell { .. } => 0,
            _ => 2,
        },
        Expr::Array(_) => 1,
        _ => 3,
    }
}

fn name_def_1d(def: &Expr) -> bool {
    area_1d(first_ptg(def))
}

fn area_1d(e: &Expr) -> bool {
    let same = |a: &crate::parser::ast::Coord, b: &crate::parser::ast::Coord| a.abs == b.abs;
    match e {
        Expr::Ref {
            kind: RefKind::Area { r0, c0, r1, c1 },
            ..
        } => {
            same(r0, r1)
                && same(c0, c1)
                && ((c0.index == c1.index && r0.index != r1.index)
                    || (r0.index == r1.index && c0.index != c1.index))
        }
        Expr::Ref {
            kind: RefKind::Cols { c0, c1 },
            ..
        } => same(c0, c1) && c0.index == c1.index,
        Expr::Ref {
            kind: RefKind::Rows { r0, r1 },
            ..
        } => same(r0, r1) && r0.index == r1.index,
        _ => false,
    }
}

fn clear_names() {
    MULTI_NAMES.with(|m| m.borrow_mut().clear());
    NAME_DEFS.with(|c| c.borrow_mut().clear());
}

fn defined_name(e: &Expr) -> Option<(u8, bool)> {
    match e.unwrap_ws() {
        Expr::Name { name, sheet: None } => {
            let up = name.to_uppercase();
            NAME_DEFS.with(|c| {
                c.borrow()
                    .iter()
                    .find(|(n, ..)| *n == up)
                    .map(|(_, i, d)| (*i, *d))
            })
        }
        _ => None,
    }
}

fn multi_names(wb: &Workbook) -> Vec<String> {
    NAME_DEFS.with(|c| {
        let mut defs: Vec<(String, u8, bool)> = Vec::new();
        for n in wb.names.iter() {
            let Ok(def) = parse(n.refers_to.trim_start_matches('=')) else {
                continue;
            };
            let (key, info, d1) = (
                n.name.to_uppercase(),
                name_def_info(&def),
                name_def_1d(&def),
            );
            match defs.iter_mut().find(|(k, ..)| *k == key) {
                Some((_, i, d)) => {
                    *i |= info;
                    *d |= d1;
                }
                None => defs.push((key, info, d1)),
            }
        }
        *c.borrow_mut() = defs;
    });

    wb.tables()
        .iter()
        .map(|t| t.name.to_uppercase())
        .chain(wb.ext_multi_names())
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

    if uses_name(&e, wb) != Some(true) {
        return None;
    }
    MULTI_NAMES.with(|m| *m.borrow_mut() = multi_names(wb));
    let dynamic =
        (may_array(&e, wb, host.sheet, &mut Vec::new()) && !legacy) || UNDEFINED.with(|u| u.get());
    clear_names();
    if dynamic {
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
    clear_names();
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

pub(crate) fn with_info_env<T>(wb: &Workbook, bound: &[String], f: impl FnOnce() -> T) -> T {
    MULTI_NAMES.with(|m| *m.borrow_mut() = multi_names(wb));
    let mut known: Vec<String> = wb.names.iter().map(|n| n.name.to_uppercase()).collect();
    known.extend(bound.iter().cloned());
    let saved = KNOWN_NAMES.with(|k| k.replace(Some(known)));
    let out = f();
    KNOWN_NAMES.with(|k| *k.borrow_mut() = saved);
    clear_names();
    out
}

pub(crate) fn leaf_info(e: &Expr) -> u8 {
    info_of(e, false)
}

pub(crate) fn expr_one_dimensional(e: &Expr) -> bool {
    one_dimensional(e)
}

pub(crate) fn text_may_array(wb: &Workbook, body: &str, sheet: crate::address::SheetId) -> bool {
    let Ok(e) = parse(body) else {
        return false;
    };
    UNDEFINED.with(|u| u.set(false));
    MULTI_NAMES.with(|m| *m.borrow_mut() = multi_names(wb));
    let r = may_array(&e, wb, sheet, &mut Vec::new()) || UNDEFINED.with(|u| u.get());
    clear_names();
    r
}

#[cfg(test)]
mod tests {
    use super::*;

    fn dyn_of(t: &str) -> String {
        legacy_to_dynamic_in(&Workbook::new(), t)
    }

    fn def(t: &str) -> Expr {
        parse(t).unwrap()
    }

    #[test]
    fn array_class_call_is_not_wrapped() {
        assert_eq!(
            dyn_of("TYPE(LEN(MMULT(A1,{1})))"),
            "=TYPE(LEN(MMULT(A1,{1})))"
        );
        assert_eq!(
            dyn_of("TYPE(TEXT(MMULT(LEN(LAMBDA(p,ROW(p)/0)(A1)),{1}),\"0\"))"),
            "=TYPE(TEXT(MMULT(LEN(LAMBDA(p,@ROW(p)/0)(A1)),{1}),\"0\"))"
        );
        assert_eq!(dyn_of("N({1;2;3})"), "=N(@{1;2;3})");
    }

    #[test]
    fn let_result_is_not_an_undefined_name() {
        assert_eq!(
            dyn_of("INDEX(LET(x,A1:A3,x*2),2)"),
            "=INDEX(LET(x,A1:A3,@x*2),2)"
        );
    }

    #[test]
    fn name_ptg_info() {
        assert_eq!(name_def_info(&def("Sheet1!#REF!")), 0);
        assert_eq!(name_def_info(&def("5")), 4);
        assert_eq!(name_def_info(&def("1")), 0xc);
        assert_eq!(name_def_info(&def("0")), 0);
        assert_eq!(name_def_info(&def("1.5")), 0);
        assert_eq!(name_def_info(&def("70000")), 0);
        assert_eq!(name_def_info(&def("\"a\"")), 0);
        assert_eq!(name_def_info(&def("Sheet2!$A$2")), 0);
        assert_eq!(name_def_info(&def("Sheet1:Sheet3!$A$2")), 3);
        assert_eq!(name_def_info(&def("Sheet2!$A$2:$A$4")), 2);
        assert_eq!(name_def_info(&def("{5}")), 1);

        assert_eq!(name_def_info(&def("(Sheet2!$A$2)")), 3);
        assert_eq!(name_def_info(&def("Sheet2!$A$2:$A$4*1")), 3);
        assert_eq!(name_def_info(&def("NOW()")), 3);
        assert_eq!(name_def_info(&def("nRng")), 3);
    }

    #[test]
    fn name_first_ptg_one_dimensional() {
        assert!(name_def_1d(&def("Sheet2!$A$2:$A$4")));
        assert!(name_def_1d(&def("Sheet2!$A$2:$C$2")));
        assert!(name_def_1d(&def("Sheet2!$A:$A")));

        assert!(name_def_1d(&def("INDEX(Sheet2!$B$2:$B$11,{1;2;3})")));
        assert!(name_def_1d(&def("Sheet2!$A$2:$A$4*1")));
        assert!(!name_def_1d(&def("Sheet2!$A$2:$B$4")));
        assert!(!name_def_1d(&def("Sheet2!$A$2:$A$2")));
        assert!(!name_def_1d(&def("Sheet2!$A$2")));

        assert!(!name_def_1d(&def("Sheet2!$A2:A$4")));

        assert!(!name_def_1d(&def("(Sheet2!$A$2:$A$4,Sheet2!$B$2:$B$4)")));
        assert!(!name_def_1d(&def("nRng")));
    }

    #[test]
    fn array_constant_in_value_parameter() {
        assert_eq!(dyn_of("DISC({1,2},A5,A5,A5)"), "=DISC(@{1,2},A5,A5,A5)");
        assert_eq!(dyn_of("INDEX(A1:B3,1,1,A1:A3)"), "=INDEX(A1:B3,1,1,@A1:A3)");
        assert_eq!(dyn_of("INDEX(A1:B3,1,1,A:A)"), "=INDEX(A1:B3,1,1,@A:A)");
        assert_eq!(
            dyn_of("INDEX(A1:B3,1,1,{1;2;3})"),
            "=INDEX(A1:B3,1,1,@{1;2;3})"
        );
        assert_eq!(
            dyn_of("INDEX(A1:B3,1,1,OFFSET(A1,0,0,3,1))"),
            "=INDEX(A1:B3,1,1,@OFFSET(A1,0,0,3,1))"
        );
        assert_eq!(dyn_of("INDEX(A1:A3,B1)"), "=@INDEX(A1:A3,B1)");
    }
}
