use rustc_hash::FxHashMap;

use crate::address::Rect;
use crate::workbook::FormulaId;

#[derive(Default, Debug)]
pub struct SpillIndex {
    active: FxHashMap<FormulaId, Rect>,

    blocked: FxHashMap<FormulaId, Rect>,
}

impl SpillIndex {
    pub fn set_active(&mut self, fid: FormulaId, rect: Rect) {
        self.blocked.remove(&fid);
        self.active.insert(fid, rect);
    }

    pub fn set_blocked(&mut self, fid: FormulaId, rect: Rect) {
        self.active.remove(&fid);
        self.blocked.insert(fid, rect);
    }

    pub fn remove(&mut self, fid: FormulaId) -> Option<Rect> {
        self.blocked.remove(&fid);
        self.active.remove(&fid)
    }

    pub fn blocked_rect(&self, fid: FormulaId) -> Option<Rect> {
        self.blocked.get(&fid).copied()
    }

    pub fn active_overlapping(&self, rect: Rect) -> bool {
        self.active.values().any(|r| r.intersects(&rect))
    }

    pub fn is_blocked(&self, fid: FormulaId) -> bool {
        self.blocked.contains_key(&fid)
    }

    pub fn active_ids(&self) -> Vec<FormulaId> {
        self.active.keys().copied().collect()
    }

    pub fn active_rect(&self, fid: FormulaId) -> Option<Rect> {
        self.active.get(&fid).copied()
    }

    pub fn owner_of(&self, row: u32, col: u32) -> Option<FormulaId> {
        self.active
            .iter()
            .find(|(_, r)| r.contains(row, col))
            .map(|(f, _)| *f)
    }

    pub fn intersecting(&self, rect: &Rect) -> Vec<(FormulaId, Rect)> {
        let mut v: Vec<_> = self
            .active
            .iter()
            .filter(|(_, r)| r.intersects(rect))
            .map(|(f, r)| (*f, *r))
            .collect();
        v.sort_unstable_by_key(|(f, _)| *f);
        v
    }

    pub fn anchors_wanting(&self, row: u32, col: u32) -> Vec<FormulaId> {
        self.active
            .iter()
            .chain(self.blocked.iter())
            .filter(|(_, r)| r.contains(row, col))
            .map(|(f, _)| *f)
            .collect()
    }

    pub fn anchors_wanting_rect(&self, rect: &Rect) -> Vec<FormulaId> {
        self.active
            .iter()
            .chain(self.blocked.iter())
            .filter(|(_, r)| r.intersects(rect))
            .map(|(f, _)| *f)
            .collect()
    }

    pub fn bounds(&self) -> Option<Rect> {
        self.active
            .values()
            .copied()
            .reduce(|a, b| a.union_bounds(&b))
    }

    #[allow(dead_code)]
    pub fn is_empty(&self) -> bool {
        self.active.is_empty()
    }
}
