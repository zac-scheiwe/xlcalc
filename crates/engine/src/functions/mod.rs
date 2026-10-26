pub mod args;
pub mod arrays;
pub mod calendar;
mod casemap;
pub mod criteria;
pub mod datetime;
pub mod dynamic;
pub mod engineering;
pub mod financial;
pub mod info;
pub mod logical;
pub mod lookup;
pub mod math;
pub mod statdist;
pub mod stats;
pub mod text;
pub mod textfmt;
pub mod xlm;
pub mod xml;

use std::sync::LazyLock;

use rustc_hash::FxHashMap;

use crate::eval::{EvalCtx, Operand};
use crate::formula::Node;
use crate::value::Value;

pub type FuncId = u16;

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum ParamClass {
    Value,

    Raw,

    Reference,
}

#[derive(Clone, Copy)]
pub enum Imp {
    Scalar(fn(&mut EvalCtx, &[Value]) -> Value),

    Args(fn(&mut EvalCtx, &[Operand]) -> Operand),

    Lazy(fn(&mut EvalCtx, &[Node]) -> Operand),
}

#[derive(Clone, Copy)]
pub struct FnDef {
    pub name: &'static str,
    pub min_args: u8,
    pub max_args: u8,

    pub params: &'static str,
    pub volatile: bool,
    pub imp: Imp,

    pub check: Option<fn(&[crate::parser::ast::Expr]) -> Result<(), &'static str>>,
}

impl FnDef {
    pub fn param_letter(&self, i: usize) -> u8 {
        let b = self.params.as_bytes();
        if b.is_empty() {
            b'V'
        } else {
            b[i.min(b.len() - 1)]
        }
    }

    pub fn param_class(&self, i: usize) -> ParamClass {
        let b = self.params.as_bytes();
        if b.is_empty() {
            return ParamClass::Value;
        }
        match b[i.min(b.len() - 1)] {
            b'R' | b'E' | b'X' | b'M' => ParamClass::Raw,
            b'Q' => ParamClass::Reference,
            _ => ParamClass::Value,
        }
    }
}

pub const fn checked(
    mut d: FnDef,
    f: fn(&[crate::parser::ast::Expr]) -> Result<(), &'static str>,
) -> FnDef {
    d.check = Some(f);
    d
}

const REFERENCE_FUNCTIONS: &[&str] = &[
    "INDEX", "OFFSET", "INDIRECT", "IF", "IFS", "CHOOSE", "SWITCH", "XLOOKUP", "LET", "TAKE",
    "DROP",
];

pub fn can_be_called(e: &crate::parser::ast::Expr) -> bool {
    use crate::parser::ast::Expr;
    match e.unwrap_ws() {
        Expr::Number(_)
        | Expr::Text(_)
        | Expr::Bool(_)
        | Expr::Array(_)
        | Expr::Unary {
            op: crate::parser::ast::UnaryOp::Neg | crate::parser::ast::UnaryOp::Plus,
            ..
        }
        | Expr::Percent(_) => false,
        Expr::Error(err) => *err == crate::value::XlError::Ref,
        Expr::Binary { op, .. } => matches!(
            op,
            crate::parser::ast::BinOp::Range
                | crate::parser::ast::BinOp::Intersect
                | crate::parser::ast::BinOp::Union
        ),
        Expr::Paren(x) => can_be_called(x),
        Expr::Func { name, .. } => {
            let up = crate::formula::strip_prefixes(name).to_uppercase();
            up == "LAMBDA"
                || up == "LET"
                || REFERENCE_FUNCTIONS.contains(&up.as_str())
                || lookup(&up).is_none()
        }
        _ => true,
    }
}

pub fn is_reference_expr(e: &crate::parser::ast::Expr) -> bool {
    use crate::parser::ast::{BinOp, Expr, UnaryOp};
    match e.unwrap_ws() {
        Expr::Ref { .. }
        | Expr::Name { .. }
        | Expr::StructRef { .. }
        | Expr::Spill(_)
        | Expr::Call { .. } => true,
        Expr::Unary {
            op: UnaryOp::Implicit,
            ..
        } => true,
        Expr::Error(crate::value::XlError::Ref) => true,

        Expr::Field { expr, .. } => {
            let mut b = expr.unwrap_ws();
            while let Expr::Field { expr, .. } | Expr::Unary { expr, .. } = b {
                b = expr.unwrap_ws();
            }
            matches!(b, Expr::Ref { .. })
        }
        Expr::Paren(x) => is_reference_expr(x),
        Expr::Binary {
            op: BinOp::Range | BinOp::Intersect | BinOp::Union,
            ..
        } => true,
        Expr::Func { name, .. } => {
            let up = crate::formula::strip_prefixes(name).to_uppercase();
            REFERENCE_FUNCTIONS.contains(&up.as_str()) || lookup(&up).is_none()
        }
        _ => false,
    }
}

pub const MANY: u8 = 255;

pub const MANY254: u8 = 254;

pub const fn scalar_p(
    name: &'static str,
    min: u8,
    max: u8,
    params: &'static str,
    f: fn(&mut EvalCtx, &[Value]) -> Value,
) -> FnDef {
    FnDef {
        params,
        ..scalar(name, min, max, f)
    }
}

pub const fn scalar(
    name: &'static str,
    min: u8,
    max: u8,
    f: fn(&mut EvalCtx, &[Value]) -> Value,
) -> FnDef {
    FnDef {
        name,
        min_args: min,
        max_args: max,
        params: "",
        volatile: false,
        imp: Imp::Scalar(f),
        check: None,
    }
}

pub const fn args(
    name: &'static str,
    min: u8,
    max: u8,
    params: &'static str,
    f: fn(&mut EvalCtx, &[Operand]) -> Operand,
) -> FnDef {
    FnDef {
        name,
        min_args: min,
        max_args: max,
        params,
        volatile: false,
        imp: Imp::Args(f),
        check: None,
    }
}

pub const fn lazy(
    name: &'static str,
    min: u8,
    max: u8,
    f: fn(&mut EvalCtx, &[Node]) -> Operand,
) -> FnDef {
    FnDef {
        name,
        min_args: min,
        max_args: max,
        params: "R",
        volatile: false,
        imp: Imp::Lazy(f),
        check: None,
    }
}

pub const fn volatile(mut d: FnDef) -> FnDef {
    d.volatile = true;
    d
}

struct Registry {
    defs: Vec<FnDef>,
    by_name: FxHashMap<&'static str, FuncId>,
}

static REGISTRY: LazyLock<Registry> = LazyLock::new(|| {
    let mut defs = Vec::new();
    for group in [
        math::DEFS,
        stats::DEFS,
        logical::DEFS,
        info::DEFS,
        text::DEFS,
        lookup::DEFS,
        datetime::DEFS,
        dynamic::DEFS,
        criteria::DEFS,
        arrays::DEFS,
        textfmt::DEFS,
        financial::DEFS,
        statdist::DEFS,
        engineering::DEFS,
        calendar::DEFS,
        xml::DEFS,
    ] {
        defs.extend_from_slice(group);
    }
    let mut by_name = FxHashMap::default();
    for (i, d) in defs.iter().enumerate() {
        let prev = by_name.insert(d.name, i as FuncId);
        assert!(prev.is_none(), "function {} registered twice", d.name);
    }
    Registry { defs, by_name }
});

pub fn lookup(upper_name: &str) -> Option<FuncId> {
    REGISTRY.by_name.get(upper_name).copied()
}

pub fn def(id: FuncId) -> &'static FnDef {
    &REGISTRY.defs[id as usize]
}

pub fn names() -> Vec<&'static str> {
    let mut v: Vec<_> = REGISTRY.defs.iter().map(|d| d.name).collect();
    v.sort_unstable();
    v
}
