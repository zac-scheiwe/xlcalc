use super::ptgclass::{self, Anc, Class, Kind};
use crate::address::CellAddr;
use crate::formula::{Formula, Node};
use crate::legacy_single as ls;
use crate::parser::ast::{BinOp, Expr, UnaryOp};
use crate::value::Value;
use crate::workbook::Workbook;
use std::collections::HashMap;
use std::sync::{Arc, Weak};

struct It<'a> {
    node: &'a Node,
    kind: Kind,
    mask: u32,
    parent: Option<usize>,
    kids: Vec<usize>,
}

struct Tree<'a> {
    items: Vec<It<'a>>,
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
            Node::Unary(UnaryOp::Implicit, x) => (
                Kind::Call(single),
                vec![(&**x, ptgclass::arg_mask(single, 0))],
            ),
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
            Node::Let { values, body, .. } => {
                let f = ptgclass::ftab_of_name("LET");
                let m = ptgclass::arg_mask(f, 1);
                let mut k: Vec<(&Node, u32)> = values.iter().map(|v| (v, m)).collect();
                k.push((&**body, m));
                (Kind::Call(f), k)
            }
            Node::Lambda(def) => {
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
        self.class_in(n, top, false)
    }

    fn class_in(&self, n: usize, top: usize, array_entry: bool) -> Class {
        let mut chain = Vec::new();
        let mut p = if n == top { None } else { self.items[n].parent };
        while let Some(q) = p {
            chain.push(self.anc_of(q));
            p = if q == top { None } else { self.items[q].parent };
        }
        chain.reverse();
        let it = &self.items[n];
        ptgclass::classify(it.kind, it.mask, &chain, array_entry)
    }
}

pub(crate) type ClassTable = HashMap<usize, Class>;

pub(crate) fn body_classes(root: &Node, mask: u32, array_entry: bool) -> ClassTable {
    crate::deep(|| {
        let mut t = Tree { items: Vec::new() };
        let r = t.add(root, mask, None);
        (0..t.items.len())
            .map(|n| {
                (
                    t.items[n].node as *const Node as usize,
                    t.class_in(n, r, array_entry),
                )
            })
            .collect()
    })
}

pub(crate) struct CachedClasses {
    formula: Weak<Formula>,
    array_entry: bool,
    table: Arc<ClassTable>,
}

pub(crate) fn stored_classes(
    wb: &Workbook,
    host: CellAddr,
    array_entry: bool,
) -> Option<Arc<ClassTable>> {
    let fid = wb.formula_at(host)?;
    let cell = wb.formula(fid);
    if let Ok(g) = cell.cls_store.lock() {
        if let Some(c) = &*g {
            if c.array_entry == array_entry && c.formula.as_ptr() == Arc::as_ptr(&cell.formula) {
                return Some(c.table.clone());
            }
        }
    }
    let table = body_classes(&cell.formula.root, ptgclass::MASK_OPERATOR, array_entry);
    let table = Arc::new(table);
    if let Ok(mut g) = cell.cls_store.lock() {
        *g = Some(CachedClasses {
            formula: Arc::downgrade(&cell.formula),
            array_entry,
            table: table.clone(),
        });
    }
    Some(table)
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

    let_slot: bool,
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

            (Kind::Other, Node::Local { .. }) => true,
            (Kind::Other, Node::Unary(..) | Node::Percent(_) | Node::Lit(_)) => false,
            (Kind::Other, _) => false,
            _ => true,
        }
    }

    fn info(&mut self, n: usize) -> u8 {
        let kids: Vec<usize> = self.t.items[n].kids.clone();
        let mut ki: Vec<u8> = Vec::new();
        for (j, &k) in kids.iter().enumerate() {
            if let Node::Let { base, .. } = self.t.items[n].node {
                if j > 0 {
                    self.locals.insert(base + (j - 1) as u32, ki[j - 1]);
                }
            }
            let let_slot = matches!(self.t.items[n].node, Node::Let { .. })
                && matches!(self.t.items[k].node, Node::Local { .. });
            let saved = std::mem::replace(&mut self.let_slot, let_slot);
            let i = self.info(k);
            self.let_slot = saved;
            ki.push(i);
        }
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
                let one_d = f.ifunc == 0x275 && kids.len() > 2 && {
                    let [a, b] = [1, 2].map(|j| {
                        expr_of(self.wb, self.host, self.t.items[kids[j]].node)
                            .and_then(|e| crate::legacy::expr_one_dimensional(&e))
                    });
                    a.is_some() && a == b
                };
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

        let own = !(self.let_slot && matches!(it.node, Node::Local { .. }));
        if own
            && cls == Class::Val
            && raw & 3 != 0
            && (self.can_multi(n) || matches!(it.kind, Kind::Call(Some(_))))
        {
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
        (Kind::Call(_), Node::Let { .. }) => {
            let mut m = Machine {
                t,
                wb,
                host,
                top: n,
                locals: HashMap::new(),
                need: false,
                let_slot: false,
            };
            m.info(n) & 3 != 0
        }
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
                let_slot: false,
            };
            match node {
                Node::Lit(Value::Array(_)) => true,

                _ => match expr_of(wb, host, node)
                    .and_then(|e| crate::legacy::name_root_can_multi(&e))
                {
                    Some(multi) => multi,
                    None => m.leaf(n) & 3 != 0,
                },
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
        let_slot: false,
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
    root_flips(wb, host, &wb.formula(fid).formula.root)
}

pub(crate) fn root_flips(wb: &Workbook, host: CellAddr, root: &Node) -> bool {
    crate::deep(|| {
        let mut t = Tree { items: Vec::new() };
        let r = t.add(root, ptgclass::MASK_OPERATOR, None);
        let mut order = Vec::new();
        post(&t, r, &mut order);
        crate::legacy::with_info_env(wb, host, || {
            order.iter().any(|&c| determine(&t, wb, host, c)) || machine(&t, wb, host, r)
        })
    })
}

pub(crate) fn cached(wb: &Workbook, host: CellAddr) -> bool {
    let Some(fid) = wb.formula_at(host) else {
        return false;
    };
    let cell = wb.formula(fid);
    if let Some(v) = cell.formula.dynamic.get() {
        return v;
    }
    let v = f_dynamic(wb, host);
    cell.formula.dynamic.set(v);
    v
}

#[cfg(test)]
mod tests {
    use crate::address::CellAddr;
    use crate::Workbook;

    #[test]
    fn class_table_follows_a_replaced_formula() {
        let mut w = Workbook::new();
        let s = w.sheet_at(0).id;
        let host = CellAddr::new(s, 4, 13);
        w.set_legacy_input(host, "=IFERROR($C$4:$E$9,\"none\")")
            .unwrap();
        let fid = w.formula_at(host).unwrap();
        for k in 0..100_000 {
            let (f, _) = w
                .compile_normalized("IFERROR($C$4:$E$9,\"none\")", host)
                .unwrap();
            w.formula_mut(fid).formula = std::sync::Arc::new(f);

            if k % 3 != 0 {
                continue;
            }
            let got = super::stored_classes(&w, host, false).unwrap();
            let root = &w.formula(fid).formula.root;
            let want = super::body_classes(root, super::ptgclass::MASK_OPERATOR, false);
            assert!(*got == want, "stale class table at replacement {k}");
        }
    }

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

    #[test]
    fn name_body_flips_at_definition() {
        let mut w = Workbook::new();
        let s1 = w.sheet_at(0).id;
        for (r, v) in [(0u32, "1"), (1, "2"), (2, "3")] {
            w.set_input(CellAddr::new(s1, r, 7), v).unwrap();
        }
        w.define_name("kcc", "=1/3", None).unwrap();
        w.define_name("kff", "=Sheet1!$H$1:$H$3*2", None).unwrap();
        w.define_name("kcf", "=kcc+kff", None).unwrap();
        let (f, l) = (CellAddr::new(s1, 1, 9), CellAddr::new(s1, 2, 9));
        w.set_input(f, "=SUM(kcf)").unwrap();
        w.set_legacy_input(l, "=SUM(kcf)").unwrap();
        for a in [f, l] {
            assert!(matches!(w.value(a), crate::Value::Number(x) if (x - 13.0).abs() < 1e-9));
        }
    }

    #[test]
    fn name_lift_follows_the_dynamic_mode() {
        let mut w = Workbook::new();
        let s = w.sheet_at(0).id;
        for (r, v) in [(1u32, "10"), (2, "20"), (3, "30"), (4, "40")] {
            w.set_input(CellAddr::new(s, r, 0), v).unwrap();
        }
        w.define_name("rowsb", "=OFFSET($A$1,ROW($A$2:$A$4)-1,0)", None)
            .unwrap();
        w.define_name("nB", "=OFFSET($A$1,{1;2;3},0)", None)
            .unwrap();
        w.define_name("nE", "=INDEX($A$1:$A$4,{2;3;4})", None)
            .unwrap();
        let mut run = |row: u32, f: &str, legacy: bool| {
            let a = CellAddr::new(s, row, 3);
            if legacy {
                w.set_legacy_input(a, f).unwrap();
            } else {
                w.set_formula(a, f).unwrap();
            }
            crate::testing::recalc(&mut w);
            (super::cached(&w, a), w.value(a))
        };
        let f = "=MMULT(TRANSPOSE(SUBTOTAL(9,rowsb)),{1;1;1})";
        let (d, v) = run(20, f, false);
        assert!(d && matches!(v, crate::Value::Number(x) if x == 60.0));
        assert!(matches!(run(21, f, true).1, crate::Value::Error(_)));
        let f = "=SUMPRODUCT(SUBTOTAL(9,rowsb)*1)";
        let (d, v) = run(22, f, false);
        assert!(!d && matches!(v, crate::Value::Number(x) if x == 10.0));
        let f = "=ROWS(LET(x,UNIQUE(nB),x))";
        let (d, v) = run(23, f, false);
        assert!(!d && matches!(v, crate::Value::Number(x) if x == 1.0));
        let f = "=LET(f,LAMBDA(x,IF(x>1,x,0)),SUM(f(A2:A4)))";
        assert!(matches!(run(24, f, false).1, crate::Value::Number(x) if x == 60.0));

        for (k, (f, flip)) in [
            ("=SUBTOTAL(9,nB)", true),
            ("=SUBTOTAL(9,rowsb)", true),
            ("=SUBTOTAL(9,OFFSET($A$1,{1;2;3},0))", true),
            ("=COUNTIF(nB,\">0\")", true),
            ("=SUM(nB)", false),
            ("=SUBTOTAL(9,A2:A4)", false),
        ]
        .into_iter()
        .enumerate()
        {
            assert_eq!(run(50 + k as u32, f, false).0, flip, "{f}");
        }

        let (d, v) = run(25, "=ROW(nE)", false);
        assert!(d && matches!(v, crate::Value::Error(_)));
    }
}
