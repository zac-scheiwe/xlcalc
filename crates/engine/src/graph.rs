use std::collections::BTreeMap;

use rstar::primitives::{GeomWithData, Rectangle};
use rstar::{RTree, AABB};
use rustc_hash::{FxHashMap, FxHashSet};
use smallvec::SmallVec;

use crate::address::{CellAddr, Rect, SheetId};
use crate::eval::refs::resolve_rect;
use crate::formula::{walk, Formula, Node, SheetRef};
use crate::value::Text;
use crate::workbook::{FormulaId, Workbook};

#[derive(Clone, Debug, PartialEq)]
pub enum Prec {
    Cell(SheetId, u32, u32),
    Range(SheetId, Rect),
    Name(Text),
}

type RangeEntry = GeomWithData<Rectangle<[i64; 2]>, FormulaId>;

fn rect_geom(r: &Rect) -> Rectangle<[i64; 2]> {
    Rectangle::from_corners([r.r0 as i64, r.c0 as i64], [r.r1 as i64, r.c1 as i64])
}

#[derive(Default)]
pub struct DepGraph {
    points: FxHashMap<SheetId, BTreeMap<(u32, u32), SmallVec<[FormulaId; 2]>>>,
    ranges: FxHashMap<SheetId, RTree<RangeEntry>>,
    names: FxHashMap<Text, FxHashSet<FormulaId>>,
}

impl DepGraph {
    pub fn add(&mut self, fid: FormulaId, precs: &[Prec]) {
        for p in precs {
            match p {
                Prec::Cell(s, r, c) => {
                    let v = self
                        .points
                        .entry(*s)
                        .or_default()
                        .entry((*r, *c))
                        .or_default();
                    if !v.contains(&fid) {
                        v.push(fid);
                    }
                }
                Prec::Range(s, rect) => {
                    self.ranges
                        .entry(*s)
                        .or_default()
                        .insert(GeomWithData::new(rect_geom(rect), fid));
                }
                Prec::Name(k) => {
                    self.names.entry(k.clone()).or_default().insert(fid);
                }
            }
        }
    }

    pub fn remove(&mut self, fid: FormulaId, precs: &[Prec]) {
        for p in precs {
            match p {
                Prec::Cell(s, r, c) => {
                    if let Some(m) = self.points.get_mut(s) {
                        if let Some(v) = m.get_mut(&(*r, *c)) {
                            v.retain(|f| *f != fid);
                            if v.is_empty() {
                                m.remove(&(*r, *c));
                            }
                        }
                    }
                }
                Prec::Range(s, rect) => {
                    if let Some(t) = self.ranges.get_mut(s) {
                        t.remove(&GeomWithData::new(rect_geom(rect), fid));
                    }
                }
                Prec::Name(k) => {
                    if let Some(set) = self.names.get_mut(k) {
                        set.remove(&fid);
                    }
                }
            }
        }
    }

    pub fn dependents_of_cell(&self, sheet: SheetId, row: u32, col: u32, out: &mut Vec<FormulaId>) {
        if let Some(m) = self.points.get(&sheet) {
            if let Some(v) = m.get(&(row, col)) {
                out.extend_from_slice(v);
            }
        }
        if let Some(t) = self.ranges.get(&sheet) {
            let p = AABB::from_point([row as i64, col as i64]);
            out.extend(t.locate_in_envelope_intersecting(&p).map(|e| e.data));
        }
    }

    pub fn dependents_of_rect(&self, sheet: SheetId, rect: &Rect, out: &mut Vec<FormulaId>) {
        if let Some(m) = self.points.get(&sheet) {
            for (&(_, c), v) in m.range((rect.r0, 0)..=(rect.r1, u32::MAX)) {
                if c >= rect.c0 && c <= rect.c1 {
                    out.extend_from_slice(v);
                }
            }
        }
        if let Some(t) = self.ranges.get(&sheet) {
            let env = AABB::from_corners(
                [rect.r0 as i64, rect.c0 as i64],
                [rect.r1 as i64, rect.c1 as i64],
            );
            out.extend(t.locate_in_envelope_intersecting(&env).map(|e| e.data));
        }
    }

    pub fn dependents_of_name(&self, key: &str, out: &mut Vec<FormulaId>) {
        if let Some(set) = self.names.get(key) {
            out.extend(set.iter().copied());
        }
    }
}

pub fn collect_precedents(wb: &Workbook, formula: &Formula, host: CellAddr) -> Vec<Prec> {
    let mut out = Vec::new();
    let mut seen_names: SmallVec<[Text; 4]> = SmallVec::new();

    if crate::datatable::is_table_formula(formula) {
        if let Node::Func { args, .. } = &formula.root {
            for a in args.iter().skip(2) {
                collect_node(wb, a, host, false, &mut out, &mut seen_names);
            }
            return out;
        }
    }
    collect_node(wb, &formula.root, host, false, &mut out, &mut seen_names);
    out
}

fn sheets_of(wb: &Workbook, sheet: &SheetRef, host: CellAddr) -> SmallVec<[SheetId; 1]> {
    match sheet {
        SheetRef::Current => smallvec::smallvec![host.sheet],
        SheetRef::Sheet(id) => smallvec::smallvec![*id],
        SheetRef::Span(a, b) => match (wb.sheet_position(*a), wb.sheet_position(*b)) {
            (Some(x), Some(y)) => (x.min(y)..=x.max(y)).map(|i| wb.sheet_at(i).id).collect(),
            _ => SmallVec::new(),
        },
        SheetRef::Missing(_) | SheetRef::External(_) => SmallVec::new(),
    }
}

fn collect_node(
    wb: &Workbook,
    node: &Node,
    host: CellAddr,
    wrap: bool,
    out: &mut Vec<Prec>,
    seen: &mut SmallVec<[Text; 4]>,
) {
    walk(node, &mut |n| match n {
        Node::Ref(sheet, cref) => {
            if let Some(rect) = resolve_rect(cref, host, wrap) {
                for s in sheets_of(wb, sheet, host) {
                    out.push(if rect.is_cell() {
                        Prec::Cell(s, rect.r0, rect.c0)
                    } else {
                        Prec::Range(s, rect)
                    });
                }
            }
        }

        Node::Binary(crate::parser::ast::BinOp::Range, a, b) => {
            if let (Node::Ref(sa, ca), Node::Ref(sb, cb)) = (a.unwrap_ws(), b.unwrap_ws()) {
                if let (Some(ra), Some(rb)) =
                    (resolve_rect(ca, host, wrap), resolve_rect(cb, host, wrap))
                {
                    let (xa, xb) = (sheets_of(wb, sa, host), sheets_of(wb, sb, host));
                    if xa == xb {
                        let rect = ra.union_bounds(&rb);
                        for s in xa {
                            out.push(Prec::Range(s, rect));
                        }
                    }
                }
            }
        }
        Node::Name { key, sheet, .. } => {
            name_precedents(wb, key, sheet.as_ref(), host, out, seen);
        }
        Node::NamedCall { key, .. } => {
            name_precedents(wb, key, None, host, out, seen);
        }

        Node::Func { id, args }
            if args.len() == 3
                && matches!(crate::functions::def(*id).name, "SUMIF" | "AVERAGEIF") =>
        {
            if let (Node::Ref(s0, c0), Node::Ref(s2, c2)) =
                (args[0].unwrap_ws(), args[2].unwrap_ws())
            {
                if let (Some(r0), Some(r2)) =
                    (resolve_rect(c0, host, wrap), resolve_rect(c2, host, wrap))
                {
                    let rect = Rect::new(
                        r2.r0,
                        r2.c0,
                        (r2.r0 + r0.rows() - 1).min(crate::address::MAX_ROWS - 1),
                        (r2.c0 + r0.cols() - 1).min(crate::address::MAX_COLS - 1),
                    );
                    let _ = s0;
                    for s in sheets_of(wb, s2, host) {
                        out.push(Prec::Range(s, rect));
                    }
                }
            }
        }
        Node::StructRef { table, spec, .. } => {
            out.push(Prec::Name(crate::workbook::tables::table_dep_key(table)));
            if let Ok(a) = crate::eval::refs::struct_rect(wb, table, spec, host) {
                out.push(if a.rect.is_cell() {
                    Prec::Cell(a.sheet, a.rect.r0, a.rect.c0)
                } else {
                    Prec::Range(a.sheet, a.rect)
                });
            }
        }
        _ => {}
    });
}

fn name_precedents(
    wb: &Workbook,
    key: &Text,
    sheet: Option<&SheetRef>,
    host: CellAddr,
    out: &mut Vec<Prec>,
    seen: &mut SmallVec<[Text; 4]>,
) {
    if !out.iter().any(|p| matches!(p, Prec::Name(k) if k == key)) {
        out.push(Prec::Name(key.clone()));
    }
    if seen.contains(key) || seen.len() > 32 {
        return;
    }
    let def = match sheet {
        Some(s) => {
            let ids = sheets_of(wb, s, host);
            ids.first()
                .and_then(|id| wb.names.get(key, Some(*id)))
                .or_else(|| wb.names.get(key, None))
        }
        None => wb.names.lookup(key, host.sheet),
    };
    if let Some(def) = def {
        seen.push(key.clone());
        let f = def.formula.clone();
        collect_node(wb, &f.root, host, true, out, seen);
        seen.pop();
    }
}
