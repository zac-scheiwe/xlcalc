use crate::eval::ptgclass::{self, Anc, Class, Kind};
use crate::legacy_single as ls;
use crate::parser::ast::{BinOp, Expr, UnaryOp};
use std::collections::HashMap;

#[derive(Clone)]
enum Shape {
    Number(f64),

    Leaf(Expr),

    Array,

    Scalar,

    Operator,

    RangeOp(BinOp),

    Call(&'static crate::legacy_ftab::Ftab),

    NameCall,

    Plain,

    Single,

    Let(Vec<String>),

    Lambda(Vec<String>),
}

struct It {
    kind: Kind,
    mask: u32,
    parent: Option<usize>,
    kids: Vec<usize>,
    shape: Shape,

    one_d: Option<crate::legacy::OneDim>,
}

struct Pass {
    array_entry: bool,
    items: Vec<It>,
    wrap: Vec<bool>,
    locals: HashMap<String, u8>,
}

fn bare_name(name: &str) -> String {
    let up = name.to_uppercase();
    let up = up.strip_prefix("_XLFN.").unwrap_or(&up);
    up.strip_prefix("_XLWS.").unwrap_or(up).to_string()
}

pub(crate) fn operands(e: &Expr) -> (Kind, Vec<(&Expr, u32)>) {
    let (kind, _, kids) = describe(e);
    (kind, kids)
}

fn describe(e: &Expr) -> (Kind, Shape, Vec<(&Expr, u32)>) {
    let single = ptgclass::ftab_of_name("SINGLE");
    match e {
        Expr::Number(n) => (Kind::Other, Shape::Number(*n), vec![]),
        Expr::Array(_) => (Kind::RefToken { array_const: true }, Shape::Array, vec![]),
        Expr::Ref { .. } | Expr::Name { .. } | Expr::StructRef { .. } => (
            Kind::RefToken { array_const: false },
            Shape::Leaf(e.clone()),
            vec![],
        ),
        Expr::Unary {
            op: UnaryOp::Implicit,
            expr,
        } => (
            Kind::Call(single),
            Shape::Single,
            vec![(expr, ptgclass::arg_mask(single, 0))],
        ),
        Expr::Unary { expr, .. } | Expr::Percent(expr) => (
            Kind::Other,
            Shape::Operator,
            vec![(expr, ptgclass::MASK_OPERATOR)],
        ),
        Expr::Spill(x) => {
            let f = ptgclass::ftab_of_name("ANCHORARRAY");
            (
                Kind::Call(f),
                f.map_or(Shape::NameCall, Shape::Call),
                vec![(x, ptgclass::arg_mask(f, 0))],
            )
        }
        Expr::Binary { op, left, right } => {
            if matches!(op, BinOp::Range | BinOp::Intersect | BinOp::Union) {
                (
                    Kind::RefToken { array_const: false },
                    Shape::RangeOp(*op),
                    vec![(left, ptgclass::MASK_RANGE), (right, ptgclass::MASK_RANGE)],
                )
            } else {
                (
                    Kind::Other,
                    Shape::Operator,
                    vec![
                        (left, ptgclass::MASK_OPERATOR),
                        (right, ptgclass::MASK_OPERATOR),
                    ],
                )
            }
        }
        Expr::Call { callee, args } => {
            let f = ptgclass::ftab_call();
            let mut k = vec![(&**callee, ptgclass::callee_mask())];
            k.extend(
                args.iter()
                    .enumerate()
                    .map(|(i, a)| (a, ptgclass::arg_mask(f, i))),
            );
            (Kind::Call(f), Shape::NameCall, k)
        }
        Expr::Func { name, args } => {
            let bare = bare_name(name);
            let n = args.len();
            let name_of = |x: &Expr| match x.unwrap_ws() {
                Expr::Name { name, .. } => name.to_uppercase(),
                _ => String::new(),
            };
            if bare == "LET" && n % 2 == 1 && n >= 3 {
                let f = ptgclass::ftab_of_name("LET");
                let m = ptgclass::arg_mask(f, 1);
                let vars: Vec<String> = args.iter().step_by(2).take(n / 2).map(name_of).collect();
                let mut k: Vec<(&Expr, u32)> = args
                    .iter()
                    .skip(1)
                    .step_by(2)
                    .take(n / 2)
                    .map(|v| (v, m))
                    .collect();
                k.push((&args[n - 1], m));
                return (Kind::Call(f), Shape::Let(vars), k);
            }
            if bare == "LAMBDA" && n >= 1 {
                let f = ptgclass::ftab_of_name("LAMBDA");
                let params: Vec<String> = args[..n - 1].iter().map(name_of).collect();
                return (
                    Kind::Call(f),
                    Shape::Lambda(params),
                    vec![(&args[n - 1], ptgclass::arg_mask(f, n - 1))],
                );
            }

            if bare == "ANCHORARRAY" && n == 1 {
                let f = ptgclass::ftab_of_name(&bare);
                return (
                    Kind::Call(f),
                    f.map_or(Shape::NameCall, Shape::Call),
                    vec![(&args[0], ptgclass::arg_mask(f, 0))],
                );
            }
            let macro_fn = crate::functions::xlm::XLM_ONLY.contains(&bare.as_str())
                && (crate::functions::xlm::NAME_ONLY.contains(&bare.as_str())
                    || crate::functions::lookup(&bare).is_none());
            if macro_fn {
                let kids = args.iter().map(|a| (a, ptgclass::MASK_OPERATOR)).collect();
                return (Kind::Call(None), Shape::Plain, kids);
            }
            match ptgclass::ftab_of_name(&bare) {
                Some(f) if f.ifunc != 255 => {
                    let kids = args
                        .iter()
                        .enumerate()
                        .map(|(i, a)| {
                            let m = crate::legacy_classes::mask_n(&bare, i, n)
                                .unwrap_or_else(|| ptgclass::arg_mask(Some(f), i));
                            (a, m)
                        })
                        .collect();
                    (Kind::Call(Some(f)), Shape::Call(f), kids)
                }
                _ => {
                    let f = ptgclass::ftab_call();
                    let kids = args
                        .iter()
                        .enumerate()
                        .map(|(i, a)| {
                            let m = crate::legacy_classes::mask_n(&bare, i, n)
                                .unwrap_or_else(|| ptgclass::arg_mask(f, i));
                            (a, m)
                        })
                        .collect();
                    (Kind::Call(f), Shape::NameCall, kids)
                }
            }
        }
        _ => (Kind::Other, Shape::Scalar, vec![]),
    }
}

impl Pass {
    fn add(&mut self, e: &Expr, mask: u32, parent: Option<usize>) -> usize {
        match e {
            Expr::Paren(x) => return self.add(x, mask, parent),
            Expr::Ws { expr, .. } => return self.add(expr, mask, parent),
            _ => {}
        }
        let (kind, shape, kids) = describe(e);
        let at = self.items.len();
        self.items.push(It {
            kind,
            mask,
            parent,
            kids: Vec::new(),
            shape,
            one_d: None,
        });
        let xlookup = matches!(&self.items[at].shape, Shape::Call(f) if f.ifunc == 0x275);
        for (j, (k, m)) in kids.into_iter().enumerate() {
            let c = self.add(k, m, Some(at));
            if xlookup && (j == 1 || j == 2) {
                self.items[c].one_d = crate::legacy::expr_one_dimensional(k);
            }
            self.items[at].kids.push(c);
        }
        at
    }

    fn class_of(&self, n: usize) -> Class {
        let mut chain = Vec::new();
        let mut p = self.items[n].parent;
        while let Some(q) = p {
            chain.push(Anc::of(self.items[q].kind, self.items[q].mask));
            p = self.items[q].parent;
        }
        chain.reverse();
        let it = &self.items[n];
        let cls = ptgclass::classify(it.kind, it.mask, &chain, self.array_entry);

        if it.mask == ptgclass::MASK_RANGE && cls == Class::Val {
            Class::Ref
        } else {
            cls
        }
    }

    fn can_multi(&self, n: usize) -> bool {
        match &self.items[n].shape {
            Shape::Call(f) => ls::can_return_multi(f),
            Shape::NameCall => true,
            Shape::Single | Shape::Operator | Shape::Number(_) | Shape::Scalar | Shape::Plain => {
                false
            }
            Shape::Lambda(_) => false,

            Shape::RangeOp(BinOp::Union) => false,
            Shape::RangeOp(_) | Shape::Leaf(_) | Shape::Array | Shape::Let(_) => true,
        }
    }

    fn info(&mut self, n: usize) -> u8 {
        let framed = matches!(self.items[n].shape, Shape::Let(_) | Shape::Lambda(_));
        let outer = framed.then(|| self.locals.clone());
        let info = self.info_in_scope(n);
        if let Some(outer) = outer {
            self.locals = outer;
        }
        info
    }

    fn info_in_scope(&mut self, n: usize) -> u8 {
        let kids = self.items[n].kids.clone();
        let shape = self.items[n].shape.clone();
        let mut ki = Vec::with_capacity(kids.len());
        if let Shape::Lambda(params) = &shape {
            for v in params {
                self.locals.insert(v.clone(), 3);
            }
        }
        for (j, &k) in kids.iter().enumerate() {
            let i = self.info(k);
            if let Shape::Let(vars) = &shape {
                if let Some(v) = vars.get(j) {
                    self.locals.insert(v.clone(), i);
                }
            }
            ki.push(i);
        }
        if matches!(shape, Shape::Plain) {
            let mut stack = kids.clone();
            while let Some(k) = stack.pop() {
                self.wrap[k] = false;
                stack.extend(self.items[k].kids.iter().copied());
            }
        }
        let raw = match &shape {
            Shape::Number(x) => ls::int_flags(*x),
            Shape::Array => 1,
            Shape::Scalar => 0,
            Shape::Leaf(e) => {
                let local = match e {
                    Expr::Name { sheet: None, name } => self.locals.get(&name.to_uppercase()),
                    _ => None,
                };
                local
                    .copied()
                    .unwrap_or_else(|| crate::legacy::leaf_info(e))
            }
            Shape::Operator => ls::operator_flags(&ki),
            Shape::RangeOp(BinOp::Intersect) => ls::range_flags(ki[0], ki[1], true),
            Shape::RangeOp(_) => ls::range_flags(ki[0], ki[1], false),
            Shape::Single | Shape::Plain => 0,
            Shape::NameCall => 3,
            Shape::Call(f) => {
                let one_d = f.ifunc == 0x275
                    && kids.len() > 2
                    && self.items[kids[1]].one_d.is_some()
                    && self.items[kids[1]].one_d == self.items[kids[2]].one_d;
                ls::call_flags(f, &ki, one_d)
            }
            Shape::Let(_) => ki.last().copied().unwrap_or(0),
            Shape::Lambda(_) => 0,
        };
        let needs = self.class_of(n) == Class::Val && raw & 3 != 0 && self.can_multi(n);
        if !needs {
            return raw;
        }
        match &shape {
            Shape::Call(f) if matches!(f.ifunc, 0x65 | 0x66) && kids.len() > 2 => {
                self.wrap[kids[2]] = true;
                2
            }
            _ => {
                if raw & 0x10 != 0 {
                    if let Some(&k) = kids.first() {
                        self.wrap[k] = true;
                    }
                }
                self.wrap[n] = true;
                0
            }
        }
    }
}

fn kids_mut(e: &mut Expr) -> Vec<&mut Expr> {
    match e {
        Expr::Unary { expr, .. } | Expr::Percent(expr) | Expr::Spill(expr) => vec![&mut **expr],
        Expr::Binary { left, right, .. } => vec![&mut **left, &mut **right],
        Expr::Call { callee, args } => {
            let mut k = vec![&mut **callee];
            k.extend(args.iter_mut());
            k
        }
        Expr::Func { name, args } => {
            let bare = bare_name(name);
            let n = args.len();
            if bare == "LET" && n % 2 == 1 && n >= 3 {
                let mut k: Vec<&mut Expr> = Vec::new();
                let mut last = None;
                for (i, a) in args.iter_mut().enumerate() {
                    if i == n - 1 {
                        last = Some(a);
                    } else if i % 2 == 1 {
                        k.push(a);
                    }
                }
                k.extend(last);
                k
            } else if bare == "LAMBDA" && n >= 1 {
                args.iter_mut().skip(n - 1).collect()
            } else {
                args.iter_mut().collect()
            }
        }
        _ => vec![],
    }
}

fn ws_over_paren(e: &Expr) -> bool {
    match e {
        Expr::Ws { expr, .. } => ws_over_paren(expr),
        Expr::Paren(_) => true,
        _ => false,
    }
}

fn slot(e: &mut Expr) -> &mut Expr {
    if matches!(e, Expr::Paren(_)) || (matches!(e, Expr::Ws { .. }) && ws_over_paren(e)) {
        match e {
            Expr::Paren(x) => slot(x),
            Expr::Ws { expr, .. } => slot(expr),
            _ => unreachable!(),
        }
    } else {
        e
    }
}

fn core(e: &mut Expr) -> &mut Expr {
    match e {
        Expr::Paren(x) => core(x),
        Expr::Ws { expr, .. } => core(expr),
        e => e,
    }
}

fn apply(e: &mut Expr, idx: &mut usize, wrap: &[bool]) {
    let me = *idx;
    *idx += 1;
    let s = slot(e);
    for k in kids_mut(core(s)) {
        apply(k, idx, wrap);
    }
    if wrap[me] {
        let inner = std::mem::replace(s, Expr::Missing);
        *s = wrap_with_at(inner);
    }
}

fn wrap_with_at(e: Expr) -> Expr {
    use crate::legacy::{struct_kind, StructKind};
    let (before, after, x) = match e {
        Expr::Ws {
            before,
            after,
            expr,
        } => (before, after, *expr),
        x => (String::new(), String::new(), x),
    };
    let x = match x {
        Expr::StructRef { table, spec } => match struct_kind(&spec) {
            StructKind::Single => {
                return rewrap(before, after, Expr::StructRef { table, spec }, false)
            }

            StructKind::Column(name) if crate::legacy::table_has_many_rows(table.as_deref()) => {
                let simple =
                    !name.is_empty() && name.chars().all(|c| c.is_alphanumeric() || c == '_');
                let spec = if simple {
                    format!("@{name}")
                } else {
                    format!("@[{name}]")
                };
                return rewrap(before, after, Expr::StructRef { table, spec }, false);
            }
            StructKind::Multi | StructKind::Column(_) => Expr::StructRef { table, spec },
        },
        x => x,
    };
    rewrap(before, after, x, true)
}

fn rewrap(before: String, after: String, x: Expr, at: bool) -> Expr {
    let inner = if before.is_empty() && after.is_empty() {
        x
    } else {
        Expr::Ws {
            before,
            after,
            expr: Box::new(x),
        }
    };
    if at {
        crate::legacy::at(inner)
    } else {
        inner
    }
}

pub(crate) fn annotate(e: Expr) -> Expr {
    annotate_in(e, false)
}

pub(crate) fn annotate_in(mut e: Expr, array_entry: bool) -> Expr {
    crate::deep(|| {
        let mut p = Pass {
            array_entry,
            items: Vec::new(),
            wrap: Vec::new(),
            locals: HashMap::new(),
        };
        let root = p.add(&e, ptgclass::MASK_OPERATOR, None);
        p.wrap = vec![false; p.items.len()];
        p.info(root);
        let mut idx = 0;
        apply(&mut e, &mut idx, &p.wrap);
        debug_assert_eq!(idx, p.items.len());
        e
    })
}

#[cfg(test)]
mod tests {
    use super::*;

    fn run(t: &str) -> String {
        let e = crate::parser::parse(t).unwrap();
        crate::parser::print::print(&annotate(e))
    }

    #[test]
    fn places_at_from_the_postfix_machine() {
        for (t, want) in [
            ("SUM(A1:A3)", "SUM(A1:A3)"),
            ("A1:A3+1", "@A1:A3+1"),
            (
                "--Tb[[a]:[b c]]/ABS(($A$1))",
                "--@Tb[[a]:[b c]]/ABS(($A$1))",
            ),
            ("-(B2,D4)", "-(B2,D4)"),
            ("MAP(A1:A3,LAMBDA(a,a*2))", "@MAP(A1:A3,LAMBDA(a,@a*2))"),
        ] {
            assert_eq!(run(t), want, "{t}");
        }
    }
}
