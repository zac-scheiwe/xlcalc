use rustc_hash::FxHashSet;

use crate::address::{CellAddr, Rect, SheetId};
use crate::graph::{collect_precedents_scan, Prec};
use crate::workbook::store::Entry;
use crate::workbook::{FormulaId, Workbook};

fn mergeable(a: &Rect, b: &Rect) -> bool {
    let (ar0, ar1, ac0, ac1) = (
        i64::from(a.r0),
        i64::from(a.r1),
        i64::from(a.c0),
        i64::from(a.c1),
    );
    let (br0, br1, bc0, bc1) = (
        i64::from(b.r0),
        i64::from(b.r1),
        i64::from(b.c0),
        i64::from(b.c1),
    );
    let same_rows = ar0 == br0 && ar1 == br1 && ac0 <= bc1 + 1 && bc0 - 1 <= ac1;
    let same_cols = ac0 == bc0 && ac1 == bc1 && ar0 <= br1 + 1 && br0 - 1 <= ar1;
    let a_in_b = br0 <= ar0 && ar1 <= br1 && bc0 <= ac0 && ac1 <= bc1;
    let b_in_a = ar0 <= br0 && br1 <= ar1 && ac0 <= bc0 && bc1 <= ac1;
    same_rows || same_cols || a_in_b || b_in_a
}

pub(crate) fn minimize(list: &mut Vec<Rect>) {
    let mut cur = list.len() as isize - 1;
    while cur >= 0 {
        let mut j = cur - 1;
        while j >= 0 {
            if j != cur && mergeable(&list[j as usize], &list[cur as usize]) {
                let u = list[j as usize].union_bounds(&list[cur as usize]);
                list[cur as usize] = u;
                list.remove(j as usize);
                if j < cur {
                    cur -= 1;
                }
                j = list.len() as isize - 1;
            } else {
                j -= 1;
            }
        }
        cur -= 1;
    }
}

pub(crate) fn coalesce(mut list: Vec<Rect>) -> Vec<Rect> {
    minimize(&mut list);
    list
}

struct RefList {
    items: Vec<Rect>,
    limit: usize,
}

impl RefList {
    fn new() -> Self {
        RefList {
            items: Vec::new(),
            limit: 0x2000,
        }
    }

    fn push(&mut self, r: Rect) {
        if self.limit <= self.items.len() {
            minimize(&mut self.items);
            self.limit = self.limit.max(self.items.len() + self.items.len() / 5);
        }
        self.items.push(r);
    }
}

impl Workbook {
    fn formula_refs(&self, fid: FormulaId) -> Vec<(SheetId, Rect)> {
        let f = self.formula(fid);
        let spilled = self.spill_ref_cells(fid);
        collect_precedents_scan(self, &f.formula, f.addr)
            .into_iter()
            .filter_map(|p| match p {
                Prec::Cell(s, r, c) => Some((s, self.spill_extent(&spilled, s, r, c))),
                Prec::Range(s, rect) => Some((s, rect)),
                Prec::Name(_) => None,
            })
            .collect()
    }

    fn spill_ref_cells(&self, fid: FormulaId) -> Vec<(SheetId, u32, u32)> {
        let f = self.formula(fid);
        let mut inner: Vec<crate::formula::Formula> = Vec::new();
        crate::formula::walk(&f.formula.root, &mut |n| {
            if let crate::formula::Node::Spill(x) = n {
                inner.push(crate::formula::Formula {
                    root: (**x).clone(),
                    volatile: false,
                });
            }
        });
        inner
            .iter()
            .flat_map(|i| collect_precedents_scan(self, i, f.addr))
            .filter_map(|p| match p {
                Prec::Cell(s, r, c) => Some((s, r, c)),
                _ => None,
            })
            .collect()
    }

    fn spill_extent(&self, spilled: &[(SheetId, u32, u32)], s: SheetId, r: u32, c: u32) -> Rect {
        if spilled.contains(&(s, r, c)) {
            let active = self
                .formula_at(CellAddr::new(s, r, c))
                .and_then(|a| self.sheet(s)?.spills.active_rect(a));
            if let Some(rect) = active {
                return rect;
            }
        }
        Rect::cell(r, c)
    }

    fn formulas_in(&self, sheet: SheetId, rect: Rect) -> Vec<FormulaId> {
        let Some(sh) = self.sheet(sheet) else {
            return Vec::new();
        };
        let mut out: Vec<FormulaId> = sh
            .cells
            .iter_rect(rect)
            .filter_map(|(_, _, e)| match e {
                Entry::Formula(f) => Some(*f),
                Entry::Const(_) => None,
            })
            .collect();

        for (f, block) in self.array_blocks(sheet) {
            if block.intersects(&rect) && !out.contains(&f) {
                out.push(f);
            }
        }
        out
    }

    fn formula_area(&self, f: FormulaId) -> Rect {
        self.array_block(f).unwrap_or_else(|| {
            let at: CellAddr = self.formula(f).addr;
            Rect::cell(at.row, at.col)
        })
    }

    pub fn trace_precedents(&self, sheet: SheetId, areas: &[Rect], direct: bool) -> Vec<Rect> {
        let mut seen: FxHashSet<FormulaId> = FxHashSet::default();
        let mut cur: Vec<FormulaId> = Vec::new();
        for a in areas {
            for f in self.formulas_in(sheet, *a) {
                if seen.insert(f) {
                    cur.push(f);
                }
            }
        }
        let mut raw: Vec<Rect> = Vec::new();
        while !cur.is_empty() {
            let mut found = RefList::new();
            for (_, r) in cur
                .iter()
                .flat_map(|f| self.formula_refs(*f))
                .filter(|(s, _)| *s == sheet)
            {
                found.push(r);
            }
            let level = found.items;
            raw.extend(level.iter().copied());
            if direct {
                break;
            }
            cur = Vec::new();
            for r in &level {
                for f in self.formulas_in(sheet, *r) {
                    if seen.insert(f) {
                        cur.push(f);
                    }
                }
            }
        }
        coalesce(raw)
    }

    fn direct_dependents(&self, sheet: SheetId, areas: &[Rect]) -> Vec<FormulaId> {
        let mut found: Vec<FormulaId> = Vec::new();
        for a in areas {
            self.graph.dependents_of_rect(sheet, a, &mut found);
        }

        found.retain(|&f| {
            let mut isect = false;
            crate::formula::walk(&self.formula(f).formula.root, &mut |n| {
                isect |= matches!(
                    n,
                    crate::formula::Node::Binary(crate::parser::ast::BinOp::Intersect, ..)
                );
            });
            !isect
                || self
                    .formula_refs(f)
                    .iter()
                    .any(|(s, r)| *s == sheet && areas.iter().any(|a| a.intersects(r)))
        });

        for f in self.formula_ids() {
            let spilled = self.spill_ref_cells(f);
            if !spilled.is_empty()
                && self
                    .formula_refs(f)
                    .iter()
                    .any(|(s, r)| *s == sheet && areas.iter().any(|a| a.intersects(r)))
            {
                found.push(f);
            }
        }
        let mut cells: Vec<(u32, u32, FormulaId)> = found
            .into_iter()
            .filter_map(|f| {
                let at = self.formula(f).addr;
                (at.sheet == sheet).then_some((at.row, at.col, f))
            })
            .collect();
        cells.sort_unstable();
        cells.dedup();
        cells.into_iter().map(|c| c.2).collect()
    }

    pub fn trace_dependents(&self, sheet: SheetId, areas: &[Rect], direct: bool) -> Vec<Rect> {
        let mut seen: FxHashSet<FormulaId> = FxHashSet::default();
        let mut cur = self.direct_dependents(sheet, areas);
        seen.extend(cur.iter().copied());
        let mut raw: Vec<Rect> = Vec::new();
        while !cur.is_empty() {
            let mut found = RefList::new();
            for f in &cur {
                found.push(self.formula_area(*f));
            }
            let level = found.items;
            raw.extend(level.iter().copied());
            if direct {
                break;
            }
            cur = self
                .direct_dependents(sheet, &level)
                .into_iter()
                .filter(|f| seen.insert(*f))
                .collect();
        }
        coalesce(raw)
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    fn rect(a: &str) -> Rect {
        let (r, c, _, _) = crate::address::parse_a1_cell(a).unwrap();
        Rect::cell(r, c)
    }

    fn show(list: &[Rect]) -> String {
        list.iter().map(Rect::a1).collect::<Vec<_>>().join(" ")
    }

    #[test]
    fn coalesce_follows_the_measured_orders() {
        let b2c3 = Rect::new(1, 1, 2, 2);
        let c1c3 = Rect::new(0, 2, 2, 2);
        let a1d1 = Rect::new(0, 0, 0, 3);
        let a1b2 = Rect::new(0, 0, 1, 1);
        let a2a4 = Rect::new(1, 0, 3, 0);
        assert_eq!(
            show(&coalesce(vec![rect("A3"), rect("B4"), rect("A3")],)),
            "B4 A3"
        );
        assert_eq!(
            show(&coalesce(vec![rect("A2"), rect("B2"), rect("B3")],)),
            "A2 B2:B3"
        );
        assert_eq!(
            show(&coalesce(vec![rect("D4"), a1d1, rect("C4"), rect("B1")],)),
            "C4:D4 A1:D1"
        );
        assert_eq!(
            show(&coalesce(vec![rect("C1"), rect("B1"), rect("D2"), a1b2],)),
            "C1 D2 A1:B2"
        );
        assert_eq!(
            show(&coalesce(vec![b2c3, rect("D4"), c1c3, rect("C3")],)),
            "B2:C3 D4 C1:C3"
        );
        assert_eq!(
            show(&coalesce(vec![rect("A1"), rect("A3"), rect("B3"), a2a4],)),
            "B3 A1:A4"
        );
    }

    #[test]
    fn space_operator_lists_the_overlap_or_the_operands() {
        let mut wb = Workbook::new();
        let sh = wb.sheet_at(0).id;
        let host = |r, c| CellAddr::new(sh, r, c);
        for (r, f) in [
            "=SUM(A1:A3 A2:A4)",
            "=SUM(A1:A2 A4:A5)",
            "=SUM(A1:A3 A2:A4 A5:A6)",
            "=SUM((A1:A3,A5:A6) A2:A5)",
        ]
        .iter()
        .enumerate()
        {
            wb.set_formula(host(r as u32, 4), f).unwrap();
        }
        let direct = |r: u32| show(&wb.trace_precedents(sh, &[Rect::cell(r, 4)], true));
        assert_eq!(direct(0), "A2:A3");
        assert_eq!(direct(1), "A1:A2 A4:A5");
        assert_eq!(direct(2), "A1:A6");
        assert_eq!(direct(3), "A2:A3 A5");

        let deps = wb.trace_dependents(sh, &[Rect::cell(0, 0)], true);
        assert_eq!(show(&deps), "E2:E3");
    }
}
