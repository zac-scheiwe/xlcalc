use rustc_hash::FxHashMap;

use crate::address::{CellAddr, Rect, SheetId};
use crate::datatable::is_table_formula;
use crate::formula::Node;
use crate::formula_print::print_node;
use crate::structure::{edit_refs, RefEdit};
use crate::workbook::{Entry, Workbook};

#[derive(Default)]
pub(crate) struct SharePlan {
    cells: FxHashMap<CellAddr, (Option<Node>, Option<u32>)>,
}

#[derive(Clone, Copy, PartialEq, Eq, Hash)]
enum Key {
    Share(u32),
    Cell(CellAddr),
}

impl Workbook {
    pub(crate) fn copied_root(
        &self,
        root: &Node,
        from: CellAddr,
        to: CellAddr,
        fill: bool,
    ) -> (Node, bool) {
        let mut dead = false;
        let root = edit_refs(root, &mut |sh, cref| {
            let e = self.copy_ref_edit(sh, cref, from, to);
            dead |= matches!(e, RefEdit::Dead(_));
            e
        });
        (self.copy_struct_cols(&root, from, to, !fill), dead)
    }

    pub(crate) fn plan_shares(&self, pairs: &[(CellAddr, CellAddr)], fill: bool) -> SharePlan {
        let mut pairs: Vec<(CellAddr, CellAddr)> = pairs
            .iter()
            .copied()
            .filter(|(s, _)| {
                self.formula_at(*s).is_some_and(|fid| {
                    let f = self.formula(fid);
                    f.array.is_none() && !is_table_formula(&f.formula)
                })
            })
            .collect();
        if pairs.is_empty() {
            return SharePlan::default();
        }
        pairs.sort_by_key(|(_, d)| (d.sheet, d.row, d.col));
        let used: Vec<u32> = pairs
            .iter()
            .filter_map(|(s, _)| self.formula(self.formula_at(*s)?).share)
            .collect();
        let mut members: FxHashMap<u32, usize> = used.iter().map(|&g| (g, 0)).collect();
        for f in self.formulas.iter().flatten() {
            if let Some(n) = f.share.and_then(|g| members.get_mut(&g)) {
                *n += 1;
            }
        }
        let mut next = self.next_share;
        let mut cache: FxHashMap<Key, (Node, CellAddr, u32)> = FxHashMap::default();
        let mut plan = SharePlan::default();
        for (src, dst) in pairs {
            let f = self.formula(self.formula_at(src).unwrap());
            let shared = f.share.filter(|g| members.get(g).copied().unwrap_or(0) > 1);
            let key = shared.map_or(Key::Cell(src), Key::Share);
            let (normal, _) = self.copied_root(&f.formula.root, src, dst, fill);
            if let Some((root, at, id)) = cache.get(&key) {
                let (cand, dead) = self.copied_root(root, *at, dst, fill);
                let same = match key {
                    Key::Share(_) => !dead,
                    Key::Cell(_) => print_node(self, &cand, dst) == print_node(self, &normal, dst),
                };
                if same {
                    let over = matches!(key, Key::Share(_)).then_some(cand);
                    plan.cells.insert(dst, (over, Some(*id)));
                    continue;
                }
            }
            next += 1;
            cache.insert(key, (normal, dst, next));
            plan.cells.insert(dst, (None, Some(next)));
        }

        let mut count: FxHashMap<u32, usize> = FxHashMap::default();
        for (_, id) in plan.cells.values() {
            *count.entry(id.unwrap()).or_default() += 1;
        }
        for (_, id) in plan.cells.values_mut() {
            if count[&id.unwrap()] < 2 {
                *id = None;
            }
        }
        plan
    }

    pub(crate) fn share_pairs(
        &self,
        sheet: SheetId,
        src: Rect,
        tiles: &[CellAddr],
    ) -> Vec<(CellAddr, CellAddr)> {
        let Some(sh) = self.sheet(sheet) else {
            return Vec::new();
        };
        let cells: Vec<(u32, u32)> = sh
            .cells
            .iter_rect(src)
            .filter(|(_, _, e)| matches!(e, Entry::Formula(_)))
            .map(|(r, c, _)| (r, c))
            .collect();
        tiles
            .iter()
            .flat_map(|t| {
                cells.iter().map(move |&(r, c)| {
                    (
                        CellAddr::new(sheet, r, c),
                        CellAddr::new(t.sheet, t.row + r - src.r0, t.col + c - src.c0),
                    )
                })
            })
            .collect()
    }

    pub(crate) fn with_shares<T>(
        &mut self,
        pairs: &[(CellAddr, CellAddr)],
        fill: bool,
        body: impl FnOnce(&mut Workbook) -> T,
    ) -> T {
        if self.share_plan.is_some() {
            return body(self);
        }
        let plan = self.plan_shares(pairs, fill);
        self.next_share += plan.cells.len() as u32;
        self.share_plan = Some(plan);
        let r = body(self);
        self.share_plan = None;
        r
    }

    pub(crate) fn shared_root(&self, to: CellAddr) -> Option<Node> {
        self.share_plan.as_ref()?.cells.get(&to)?.0.clone()
    }

    pub(crate) fn note_share(&mut self, to: CellAddr) {
        let id = self
            .share_plan
            .as_ref()
            .and_then(|p| p.cells.get(&to))
            .and_then(|c| c.1);
        if let (Some(id), Some(fid)) = (id, self.formula_at(to)) {
            self.formula_mut(fid).share = Some(id);
        }
    }
}
