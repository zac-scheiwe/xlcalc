use super::ptgclass::{self, Anc, Class, Kind};
use crate::address::CellAddr;
use crate::formula::Node;
use crate::legacy_single as ls;
use crate::parser::ast::{BinOp, Expr, UnaryOp};
use crate::value::Value;
use crate::workbook::Workbook;
use std::collections::HashMap;

struct It<'a> {
    node: &'a Node,
    kind: Kind,
    mask: u32,
    parent: Option<usize>,
    kids: Vec<usize>,
}

struct Tree<'a> {
    items: Vec<It<'a>>,

    bound: Vec<String>,

    quiet: bool,
}

impl<'a> Tree<'a> {
    fn add(&mut self, node: &'a Node, mask: u32, parent: Option<usize>) -> usize {
        match node {
            Node::Paren(x) => return self.add(x, mask, parent),
            Node::Ws { node, .. } => return self.add(node, mask, parent),
            _ => {}
        }
        let single = ptgclass::ftab_of_name("SINGLE");
        let (kind, kids): (Kind, Vec<(&'a Node, u32)>) = match node {
            Node::Lit(Value::Array(_)) => (Kind::RefToken { array_const: true }, vec![]),
            Node::Name { .. }
            | Node::Ref(..)
            | Node::StructRef { .. }
            | Node::DeadRef(_)
            | Node::Unsupported { .. } => (Kind::RefToken { array_const: false }, vec![]),
            Node::Unary(op @ (UnaryOp::Implicit | UnaryOp::ImplicitQuiet), x) => {
                self.quiet |= *op == UnaryOp::ImplicitQuiet;
                (
                    Kind::Call(single),
                    vec![(&**x, ptgclass::arg_mask(single, 0))],
                )
            }
            Node::Unary(_, x) | Node::Percent(x) => {
                (Kind::Other, vec![(&**x, ptgclass::MASK_OPERATOR)])
            }
            Node::Spill(x) => {
                let f = ptgclass::ftab_of_name("ANCHORARRAY");
                (Kind::Call(f), vec![(&**x, ptgclass::arg_mask(f, 0))])
            }
            Node::Binary(op, a, b) => {
                let m = if matches!(op, BinOp::Range | BinOp::Intersect | BinOp::Union) {
                    ptgclass::MASK_RANGE
                } else {
                    ptgclass::MASK_OPERATOR
                };
                (Kind::Other, vec![(&**a, m), (&**b, m)])
            }
            Node::Func { id, args } => {
                let f = ptgclass::ftab_of(*id);
                (
                    Kind::Call(f),
                    args.iter()
                        .enumerate()
                        .map(|(i, a)| (a, ptgclass::arg_mask(f, i)))
                        .collect(),
                )
            }
            Node::NamedCall { args, .. } => {
                let f = ptgclass::ftab_call();
                (
                    Kind::Call(f),
                    args.iter()
                        .enumerate()
                        .map(|(i, a)| (a, ptgclass::arg_mask(f, i)))
                        .collect(),
                )
            }
            Node::Call { callee, args } => {
                let f = ptgclass::ftab_call();
                let mut k = vec![(&**callee, ptgclass::callee_mask())];
                k.extend(
                    args.iter()
                        .enumerate()
                        .map(|(i, a)| (a, ptgclass::arg_mask(f, i))),
                );
                (Kind::Call(f), k)
            }
            Node::Let {
                names,
                values,
                body,
                ..
            } => {
                self.bound.extend(names.iter().map(|n| n.to_uppercase()));
                let f = ptgclass::ftab_of_name("LET");
                let m = ptgclass::arg_mask(f, 1);
                let mut k: Vec<(&Node, u32)> = values.iter().map(|v| (v, m)).collect();
                k.push((&**body, m));
                (Kind::Call(f), k)
            }
            Node::Lambda(def) => {
                self.bound
                    .extend(def.params.iter().map(|n| n.to_uppercase()));
                let f = ptgclass::ftab_of_name("LAMBDA");
                (
                    Kind::Call(f),
                    vec![(&def.body, ptgclass::arg_mask(f, def.params.len()))],
                )
            }
            _ => (Kind::Other, vec![]),
        };
        let at = self.items.len();
        self.items.push(It {
            node,
            kind,
            mask,
            parent,
            kids: Vec::new(),
        });
        for (k, m) in kids {
            let c = self.add(k, m, Some(at));
            self.items[at].kids.push(c);
        }
        at
    }

    fn anc_of(&self, i: usize) -> Anc {
        let it = &self.items[i];
        Anc::of(it.kind, it.mask)
    }

    fn class_at(&self, n: usize, top: usize) -> Class {
        let mut chain = Vec::new();
        let mut p = if n == top { None } else { self.items[n].parent };
        while let Some(q) = p {
            chain.push(self.anc_of(q));
            p = if q == top { None } else { self.items[q].parent };
        }
        chain.reverse();
        let it = &self.items[n];
        ptgclass::classify(it.kind, it.mask, &chain, false)
    }
}

fn check_ifunc(i: u16) -> bool {
    matches!(
        i,
        1 | 8 | 9 | 0x1d | 0x40 | 0x4e | 100..=102 | 0x167 | 0x1e0 | 0x24c..=0x24e | 0x275 | 0x27f
    )
}

fn expr_of(wb: &Workbook, host: CellAddr, node: &Node) -> Option<Expr> {
    let text = crate::formula_print::print_node(wb, node, host);
    crate::parser::parse(text.trim_start_matches('=')).ok()
}

struct Machine<'a, 'b> {
    t: &'b Tree<'a>,
    wb: &'b Workbook,
    host: CellAddr,
    top: usize,
    locals: HashMap<u32, u8>,
    need: bool,
}

impl Machine<'_, '_> {
    fn leaf(&self, n: usize) -> u8 {
        match self.t.items[n].node {
            Node::Local { slot, .. } => self.locals.get(slot).copied().unwrap_or(0),
            Node::Lit(Value::Number(x)) => ls::int_flags(*x),
            Node::Lit(Value::Array(_)) => 1,
            Node::Lit(_) | Node::Missing | Node::Unwritten | Node::DeadRef(_) => 0,
            node => expr_of(self.wb, self.host, node)
                .map(|e| crate::legacy::leaf_info(&e))
                .unwrap_or(0),
        }
    }

    fn can_multi(&self, n: usize) -> bool {
        let it = &self.t.items[n];
        match (it.kind, it.node) {
            (Kind::Call(Some(f)), _) => ls::can_return_multi(f),
            (Kind::Call(None), _) => true,
            (Kind::Other, Node::Binary(op, ..)) => {
                matches!(op, BinOp::Range | BinOp::Intersect | BinOp::Union)
            }
            (Kind::Other, Node::Unary(..) | Node::Percent(_) | Node::Lit(_)) => false,
            (Kind::Other, _) => false,
            _ => true,
        }
    }

    fn info(&mut self, n: usize) -> u8 {
        let kids: Vec<usize> = self.t.items[n].kids.clone();
        let ki: Vec<u8> = kids.iter().map(|&k| self.info(k)).collect();
        let it = &self.t.items[n];
        let raw = match (it.kind, it.node) {
            (Kind::Call(f), Node::Unary(..)) if f.is_some_and(|f| f.ifunc == 0x261) => 0,
            (_, Node::Let { values, base, .. }) => {
                for k in 0..values.len() {
                    self.locals
                        .insert(base + k as u32, ki.get(k).copied().unwrap_or(0));
                }
                ki.last().copied().unwrap_or(0)
            }
            (Kind::Call(_), Node::Lambda(_)) => 0,
            (Kind::Call(_), Node::NamedCall { .. } | Node::Call { .. }) => 3,
            (Kind::Call(Some(f)), _) => {
                let one_d = f.ifunc == 0x275
                    && kids.len() > 2
                    && [1, 2].iter().all(|&j| {
                        expr_of(self.wb, self.host, self.t.items[kids[j]].node)
                            .is_some_and(|e| crate::legacy::expr_one_dimensional(&e))
                    });
                ls::call_flags(f, &ki, one_d)
            }
            (Kind::Call(None), _) => 3,
            (_, Node::Binary(op, ..)) => match op {
                BinOp::Range | BinOp::Union => ls::range_flags(ki[0], ki[1], false),
                BinOp::Intersect => ls::range_flags(ki[0], ki[1], true),
                _ => ls::operator_flags(&ki),
            },
            (_, Node::Unary(..) | Node::Percent(_)) => ls::operator_flags(&ki),
            _ => self.leaf(n),
        };
        let cls = self.t.class_at(n, self.top);
        if cls == Class::Val && raw & 3 != 0 && self.can_multi(n) {
            self.need = true;
            0
        } else {
            raw
        }
    }
}

fn simple(t: &Tree, wb: &Workbook, host: CellAddr, n: usize) -> bool {
    let it = &t.items[n];
    if t.class_at(n, n) != Class::Val {
        return false;
    }
    match (it.kind, it.node) {
        (Kind::Call(Some(f)), _) => match f.ifunc {
            0x261 => false,
            0xff | 0x264 => true,
            _ => f.flags & 0x60648 != 0,
        },
        (Kind::Call(None), _) => true,
        (Kind::Other, _) => false,
        (_, node) => {
            let m = Machine {
                t,
                wb,
                host,
                top: n,
                locals: HashMap::new(),
                need: false,
            };
            match node {
                Node::Lit(Value::Array(_)) => true,
                _ => m.leaf(n) & 3 != 0,
            }
        }
    }
}

fn determine(t: &Tree, wb: &Workbook, host: CellAddr, c: usize) -> bool {
    let it = &t.items[c];
    if it
        .parent
        .is_some_and(|p| matches!(t.items[p].kind, Kind::Call(Some(f)) if f.ifunc == 0x261))
    {
        return false;
    }
    if let Kind::Call(Some(f)) = it.kind {
        if check_ifunc(f.ifunc) {
            if matches!(f.ifunc, 8 | 9 | 0x24e | 0x1e0) && it.mask & 7 == 0 {
                return false;
            }
            return machine(t, wb, host, c);
        }
    }
    simple(t, wb, host, c)
}

fn machine(t: &Tree, wb: &Workbook, host: CellAddr, c: usize) -> bool {
    let mut m = Machine {
        t,
        wb,
        host,
        top: c,
        locals: HashMap::new(),
        need: false,
    };
    m.info(c);
    m.need
}

fn post(t: &Tree, n: usize, out: &mut Vec<usize>) {
    for &k in &t.items[n].kids {
        post(t, k, out);
    }
    out.push(n);
}

pub(crate) fn f_dynamic(wb: &Workbook, host: CellAddr) -> bool {
    let Some(fid) = wb.formula_at(host) else {
        return false;
    };

    if wb.formula(fid).legacy_entry || wb.formula(fid).array.is_some() {
        return false;
    }
    let root = &wb.formula(fid).formula.root;
    crate::deep(|| {
        let mut t = Tree {
            items: Vec::new(),
            bound: Vec::new(),
            quiet: false,
        };
        let r = t.add(root, ptgclass::MASK_OPERATOR, None);

        if t.quiet {
            return false;
        }
        let mut order = Vec::new();
        post(&t, r, &mut order);
        let bound = t.bound.clone();
        crate::legacy::with_info_env(wb, &bound, || {
            order.iter().any(|&c| determine(&t, wb, host, c)) || machine(&t, wb, host, r)
        })
    })
}

pub(crate) fn cached(wb: &Workbook, host: CellAddr) -> bool {
    use std::sync::atomic::Ordering;
    let Some(fid) = wb.formula_at(host) else {
        return false;
    };
    let cell = wb.formula(fid);
    let key = std::sync::Arc::as_ptr(&cell.formula) as usize;
    let seen = cell.fdyn.load(Ordering::Relaxed);
    if seen != 0 && seen & !3 == key as u64 {
        return seen & 3 == 2;
    }
    let v = f_dynamic(wb, host);
    cell.fdyn
        .store(key as u64 | if v { 2 } else { 1 }, Ordering::Relaxed);
    v
}

#[cfg(test)]
mod tests {
    use crate::address::CellAddr;
    use crate::Workbook;

    #[test]
    fn formula_entry_never_flips() {
        let mut w = Workbook::new();
        let s = w.sheet_at(0).id;
        let (d, l) = (CellAddr::new(s, 5, 5), CellAddr::new(s, 6, 5));
        w.set_input(d, "=VLOOKUP(1,A1:B3,A1:A2)").unwrap();
        w.set_legacy_input(l, "=VLOOKUP(1,A1:B3,A1:A2)").unwrap();
        assert!(super::cached(&w, d));
        assert!(!super::cached(&w, l));
    }
}
