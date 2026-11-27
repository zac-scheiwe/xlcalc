use crate::eval::ptgclass::{self, Anc, Class, Kind};
use crate::parser::ast::Expr;
use std::collections::HashMap;

#[derive(Clone, Copy, PartialEq, Eq, Debug)]
pub enum FormulaContext {
    Cell,

    Array,

    Name,

    ConditionalFormat,

    Validation,

    Reference,
}

impl FormulaContext {
    fn root(self) -> (u32, bool) {
        match self {
            FormulaContext::Cell | FormulaContext::Validation => (ptgclass::MASK_OPERATOR, false),
            FormulaContext::Array | FormulaContext::ConditionalFormat => {
                (ptgclass::MASK_OPERATOR, true)
            }
            FormulaContext::Name => (ptgclass::MASK_NAME, true),
            FormulaContext::Reference => (0x20a, false),
        }
    }
}

#[derive(Clone, Copy, PartialEq, Eq, Debug)]
pub enum TokenClass {
    Reference,
    Value,
    Array,
}

pub struct TokenClasses(HashMap<*const Expr, TokenClass>);

fn token(mut e: &Expr) -> &Expr {
    loop {
        match e {
            Expr::Paren(x) | Expr::Ws { expr: x, .. } => e = x,
            _ => return e,
        }
    }
}

impl TokenClasses {
    pub fn of(&self, e: &Expr) -> TokenClass {
        self.0
            .get(&std::ptr::from_ref(token(e)))
            .copied()
            .unwrap_or(TokenClass::Reference)
    }
}

fn walk(
    e: &Expr,
    mask: u32,
    array_entry: bool,
    chain: &mut Vec<Anc>,
    out: &mut HashMap<*const Expr, TokenClass>,
) {
    let e = token(e);
    let (mut kind, kids) = crate::legacy_pass::operands(e);

    let dead = matches!(e, Expr::DeadRef(_) | Expr::DeadTable { .. })
        || matches!(e, Expr::Error(x) if x.as_str().eq_ignore_ascii_case("#REF!"));
    if dead {
        kind = Kind::RefToken { array_const: false };
    }
    let class = match ptgclass::classify(kind, mask, chain, array_entry) {
        Class::Val if mask == ptgclass::MASK_RANGE => TokenClass::Reference,
        Class::Ref => TokenClass::Reference,
        Class::Val => TokenClass::Value,
        Class::Arr => TokenClass::Array,
    };
    out.insert(std::ptr::from_ref(e), class);
    chain.push(Anc::of(kind, mask));
    for (k, m) in kids {
        walk(k, m, array_entry, chain, out);
    }
    chain.pop();
}

pub fn token_classes(formula: &Expr, context: FormulaContext) -> TokenClasses {
    let (mask, array_entry) = context.root();
    crate::deep(|| {
        let mut out = HashMap::new();
        walk(formula, mask, array_entry, &mut Vec::new(), &mut out);
        TokenClasses(out)
    })
}
