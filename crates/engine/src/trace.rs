use rustc_hash::FxHashSet;

use crate::address::{CellAddr, Rect, SheetId};
use crate::graph::{collect_precedents, Prec};
use crate::workbook::store::Entry;
use crate::workbook::{FormulaId, Workbook};

fn joins(a: &Rect, b: &Rect) -> bool {
    let u = a.union_bounds(b);
    let inter = a.intersection(b).map_or(0, |i| i.cell_count());
    u.cell_count() == a.cell_count() + b.cell_count() - inter
}

fn single(r: &Rect) -> bool {
    r.cell_count() == 1
}

fn contains(a: &Rect, b: &Rect) -> bool {
    a.intersection(b).is_some_and(|i| i == *b)
}

fn merge(list: &mut Vec<Rect>, j: usize, i: usize, early: bool) -> usize {
    let u = list[j].union_bounds(&list[i]);
    let (a, b) = (list[j], list[i]);
    if early && !single(&a) && !single(&b) && !contains(&a, &b) && !contains(&b, &a) {
        list[j] = u;
        list.remove(i);
        j
    } else {
        list[i] = u;
        list.remove(j);
        i - 1
    }
}

pub(crate) fn coalesce(mut list: Vec<Rect>, early: bool) -> Vec<Rect> {
    let mut c: isize = list.len() as isize - 1;
    let mut changed: Option<usize> = None;
    loop {
        if let Some(p) = changed.take() {
            let k = ((c + 1) as usize..list.len())
                .rev()
                .find(|&k| joins(&list[k], &list[p]));
            if let Some(k) = k {
                changed = Some(merge(&mut list, p, k, early));
                c = k as isize - 1;
                continue;
            }
        }
        if c < 0 {
            break;
        }
        let i = c as usize;
        match (0..i).rev().find(|&j| joins(&list[j], &list[i])) {
            None => c -= 1,
            Some(j) => {
                changed = Some(merge(&mut list, j, i, early));
                c = i as isize - 1;
            }
        }
    }
    list
}

impl Workbook {
    fn formula_refs(&self, fid: FormulaId) -> Vec<(SheetId, Rect)> {
        let f = self.formula(fid);
        collect_precedents(self, &f.formula, f.addr)
            .into_iter()
            .filter_map(|p| match p {
                Prec::Cell(s, r, c) => Some((s, Rect::cell(r, c))),
                Prec::Range(s, rect) => Some((s, rect)),
                Prec::Name(_) => None,
            })
            .collect()
    }

    fn formulas_in(&self, sheet: SheetId, rect: Rect) -> Vec<FormulaId> {
        let Some(sh) = self.sheet(sheet) else {
            return Vec::new();
        };
        sh.cells
            .iter_rect(rect)
            .filter_map(|(_, _, e)| match e {
                Entry::Formula(f) => Some(*f),
                Entry::Const(_) => None,
            })
            .collect()
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
            let level: Vec<Rect> = cur
                .iter()
                .flat_map(|f| self.formula_refs(*f))
                .filter(|(s, _)| *s == sheet)
                .map(|(_, r)| r)
                .collect();
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
        coalesce(raw, false)
    }

    fn direct_dependents(&self, sheet: SheetId, areas: &[Rect]) -> Vec<FormulaId> {
        let mut found: Vec<FormulaId> = Vec::new();
        for a in areas {
            self.graph.dependents_of_rect(sheet, a, &mut found);
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
        let cell = |f: FormulaId| {
            let at: CellAddr = self.formula(f).addr;
            Rect::cell(at.row, at.col)
        };
        let mut seen: FxHashSet<FormulaId> = FxHashSet::default();
        let mut cur = self.direct_dependents(sheet, areas);
        seen.extend(cur.iter().copied());
        let mut raw: Vec<Rect> = Vec::new();
        while !cur.is_empty() {
            raw.extend(cur.iter().map(|f| cell(*f)));
            if direct {
                break;
            }
            let reads: Vec<Rect> = cur.iter().map(|f| cell(*f)).collect();
            cur = self
                .direct_dependents(sheet, &reads)
                .into_iter()
                .filter(|f| seen.insert(*f))
                .collect();
        }
        coalesce(raw, true)
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
            show(&coalesce(vec![rect("A3"), rect("B4"), rect("A3")], true)),
            "B4 A3"
        );
        assert_eq!(
            show(&coalesce(vec![rect("A2"), rect("B2"), rect("B3")], true)),
            "A2 B2:B3"
        );
        assert_eq!(
            show(&coalesce(
                vec![rect("D4"), a1d1, rect("C4"), rect("B1")],
                true
            )),
            "C4:D4 A1:D1"
        );
        assert_eq!(
            show(&coalesce(
                vec![rect("C1"), rect("B1"), rect("D2"), a1b2],
                true
            )),
            "C1 D2 A1:B2"
        );
        assert_eq!(
            show(&coalesce(vec![b2c3, rect("D4"), c1c3, rect("C3")], true)),
            "B2:C3 D4 C1:C3"
        );
        assert_eq!(
            show(&coalesce(
                vec![rect("A1"), rect("A3"), rect("B3"), a2a4],
                true
            )),
            "B3 A1:A4"
        );
    }
}
