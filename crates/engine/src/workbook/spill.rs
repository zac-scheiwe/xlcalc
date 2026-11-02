use rustc_hash::FxHashMap;

use crate::address::Rect;
use crate::value::Value;
use crate::workbook::FormulaId;

#[derive(Default, Debug)]
pub struct SpillIndex {
    active: FxHashMap<FormulaId, Rect>,

    blocked: FxHashMap<FormulaId, Rect>,

    holed: FxHashMap<FormulaId, Holed>,

    cut_kept: FxHashMap<FormulaId, u32>,

    stale: FxHashMap<FormulaId, u32>,

    rw: u32,
}

#[derive(Debug, Clone)]
struct Holed {
    live: Vec<(u32, u32)>,
    fresh: Vec<(u32, u32)>,

    rw: u32,

    revived: bool,

    snap: FxHashMap<(u32, u32), Value>,

    frozen: FxHashMap<(u32, u32), Value>,
}

fn block_end(row: u32) -> u32 {
    row | 15
}

impl Holed {
    fn revive(&mut self, rect: Rect, write: bool) {
        self.revived = true;
        let end = block_end(self.rw).min(rect.r1);
        for r in rect.r0..=end {
            for c in rect.c0..=rect.c1 {
                if !self.live.contains(&(r, c)) && !self.fresh.contains(&(r, c)) {
                    self.live.push((r, c));
                    if let (false, Some(v)) = (write, self.snap.get(&(r, c))) {
                        self.frozen.insert((r, c), v.clone());
                    }
                }
            }
        }
        self.live.sort_unstable();
    }

    fn covers(&self, rect: Rect) -> bool {
        (rect.r0..=rect.r1).all(|r| {
            (rect.c0..=rect.c1).all(|c| self.live.contains(&(r, c)) || self.fresh.contains(&(r, c)))
        })
    }
}

#[derive(Debug, Clone)]
pub enum SpillSource {
    At(u32, u32),

    Zero,

    Value(Value),
}

impl SpillIndex {
    pub fn set_active(&mut self, fid: FormulaId, rect: Rect) {
        self.blocked.remove(&fid);
        if let Some(rw) = self.cut_kept.remove(&fid) {
            self.layout(fid, rect, rw);
        } else if let (Some(h), Some(&prev)) = (self.holed.get_mut(&fid), self.active.get(&fid)) {
            if prev == rect {
                self.active.insert(fid, rect);
                return;
            }
            h.frozen.clear();

            let fresh = std::mem::take(&mut h.fresh);
            h.live.extend(fresh);
            h.live.sort_unstable();
            if h.covers(prev) {
                self.holed.remove(&fid);
            } else {
                let grew =
                    (rect.r0..=rect.r1).any(|r| (rect.c0..=rect.c1).any(|c| !prev.contains(r, c)));

                let kept = Rect::new(
                    rect.r0.max(prev.r0),
                    rect.c0.max(prev.c0),
                    rect.r1.min(prev.r1),
                    rect.c1.min(prev.c1),
                );
                if (grew || h.revived) && kept.r0 <= kept.r1 && kept.c0 <= kept.c1 {
                    h.revive(kept, true);
                }
                h.live.retain(|&(r, c)| rect.contains(r, c));
                for r in rect.r0..=rect.r1 {
                    for c in rect.c0..=rect.c1 {
                        if !prev.contains(r, c) {
                            h.fresh.push((r, c));
                        }
                    }
                }
                if h.covers(rect) && h.fresh.is_empty() {
                    self.holed.remove(&fid);
                }
            }
        }
        self.active.insert(fid, rect);
        let last = match self.holed.get(&fid) {
            Some(h) => h.live.iter().chain(&h.fresh).map(|x| x.0).max(),
            None => Some(rect.r1),
        };
        if let Some(last) = last {
            self.note_rows(Some(last));
        }
    }

    fn layout(&mut self, fid: FormulaId, rect: Rect, rw: u32) {
        let rw = rw.max(rect.r0);
        if rect.r1 <= rw {
            self.holed.remove(&fid);
            return;
        }
        let mut live = Vec::new();
        for r in rect.r0..=rw {
            for c in rect.c0..=rect.c1 {
                live.push((r, c));
            }
        }
        self.holed.insert(
            fid,
            Holed {
                live,
                fresh: Vec::new(),
                rw,
                revived: false,
                snap: FxHashMap::default(),
                frozen: FxHashMap::default(),
            },
        );
    }

    pub fn set_blocked(&mut self, fid: FormulaId, rect: Rect) {
        self.active.remove(&fid);
        self.holed.remove(&fid);
        self.blocked.insert(fid, rect);
    }

    pub fn remove(&mut self, fid: FormulaId) -> Option<Rect> {
        self.blocked.remove(&fid);
        self.holed.remove(&fid);
        self.cut_kept.remove(&fid);
        self.stale.remove(&fid);
        self.active.remove(&fid)
    }

    pub fn reset(&mut self) {
        *self = Self {
            cut_kept: std::mem::take(&mut self.cut_kept),
            stale: std::mem::take(&mut self.stale),
            rw: self.rw,
            ..Self::default()
        };
    }

    pub fn note_cut_anchor(&mut self, fid: FormulaId, last_row: u32, circular: bool) {
        let rw = self.rw.max(last_row);
        if circular {
            self.cut_kept.insert(fid, rw);
        }
        self.stale.insert(fid, rw);
    }

    pub fn touch(&mut self) {
        self.stale.clear();
    }

    pub fn relayout(&mut self) {
        let stale: Vec<(FormulaId, u32)> = self.stale.iter().map(|(&f, &r)| (f, r)).collect();
        for (fid, floor) in stale {
            if let Some(&rect) = self.active.get(&fid) {
                self.layout(fid, rect, floor);
            }
        }
    }

    pub fn note_rows(&mut self, last: Option<u32>) {
        let Some(last) = last else {
            return;
        };
        if last <= self.rw {
            return;
        }
        self.rw = last;
        for (fid, h) in self.holed.iter_mut() {
            if !h.revived && last > h.rw {
                if let Some(&rect) = self.active.get(fid) {
                    h.revive(rect, false);
                }
            }
        }
    }

    pub fn snapshot_cells(&self, fid: FormulaId) -> Vec<(u32, u32)> {
        let (Some(h), Some(rect)) = (self.holed.get(&fid), self.active.get(&fid)) else {
            return Vec::new();
        };
        if h.revived || !h.snap.is_empty() {
            return Vec::new();
        }
        let end = block_end(h.rw).min(rect.r1);
        let mut out = Vec::new();
        for r in rect.r0..=end {
            for c in rect.c0..=rect.c1 {
                if !h.live.contains(&(r, c)) {
                    out.push((r, c));
                }
            }
        }
        out
    }

    pub fn set_snapshot(&mut self, fid: FormulaId, values: Vec<((u32, u32), Value)>) {
        if let Some(h) = self.holed.get_mut(&fid) {
            h.snap = values.into_iter().collect();
        }
    }

    pub fn is_hole(&self, fid: FormulaId, row: u32, col: u32) -> bool {
        self.holed
            .get(&fid)
            .is_some_and(|h| !h.live.contains(&(row, col)) && !h.fresh.contains(&(row, col)))
    }

    pub fn source(&self, fid: FormulaId, row: u32, col: u32) -> Option<SpillSource> {
        let h = self.holed.get(&fid)?;
        let rect = self.active.get(&fid)?;
        if let Some(v) = h.frozen.get(&(row, col)) {
            return Some(SpillSource::Value(v.clone()));
        }
        let w = rect.cols();
        let at = |k: u32| SpillSource::At(rect.r0 + k / w, rect.c0 + k % w);
        if let Some(k) = h.live.iter().position(|&x| x == (row, col)) {
            return Some(at(k as u32));
        }
        if h.fresh.contains(&(row, col)) {
            let p = (row - rect.r0) * w + (col - rect.c0);
            return Some(if (p as usize) < h.live.len() {
                at(p)
            } else {
                SpillSource::Zero
            });
        }
        None
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
            .find(|(f, r)| r.contains(row, col) && !self.is_hole(**f, row, col))
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
}
