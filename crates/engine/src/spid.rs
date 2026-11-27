use crate::{SheetId, Workbook};
use rustc_hash::FxHashMap;
use std::collections::hash_map::Entry;

const CLUSTER: u32 = 1024;

const MAX_CLUSTER: u32 = 0x1_0000;

#[derive(Clone, Copy)]
struct Cluster {
    owner: Option<SheetId>,

    used: u32,
}

#[derive(Clone, Default)]
pub(crate) struct ShapeIds {
    clusters: Vec<Cluster>,

    drawings: FxHashMap<SheetId, u32>,
}

impl ShapeIds {
    fn ensure(&mut self, sheet: SheetId) {
        if let Entry::Vacant(e) = self.drawings.entry(sheet) {
            e.insert(CLUSTER - 1);
            self.create(sheet);
        }
    }

    fn create(&mut self, sheet: SheetId) -> u32 {
        let last = self.drawings.get(&sheet).copied().unwrap_or(CLUSTER - 1);
        let mut k = (last / CLUSTER) as usize;
        let foreign = k
            .checked_sub(1)
            .and_then(|i| self.clusters.get(i))
            .is_none_or(|c| c.owner.is_some_and(|o| o != sheet));
        if foreign || last % CLUSTER == CLUSTER - 1 {
            self.clusters.push(Cluster {
                owner: Some(sheet),
                used: 0,
            });
            k = self.clusters.len();
        }
        let c = &mut self.clusters[k - 1];
        let spid = k as u32 * CLUSTER + c.used;
        c.used += 1;
        self.drawings.insert(sheet, last.max(spid));
        spid
    }

    fn shape(&mut self, sheet: SheetId) -> u32 {
        self.ensure(sheet);
        self.create(sheet)
    }

    fn loaded(&mut self, sheet: SheetId, spid: u32) {
        let k = spid / CLUSTER;
        if k == 0 || k > MAX_CLUSTER {
            return;
        }
        let i = k as usize - 1;
        if self.clusters.len() <= i {
            let free = Cluster {
                owner: None,
                used: 0,
            };
            self.clusters.resize(i + 1, free);
        }
        let c = &mut self.clusters[i];
        if *c.owner.get_or_insert(sheet) == sheet {
            c.used = c.used.max(spid % CLUSTER + 1);
            let last = self.drawings.entry(sheet).or_insert(spid);
            *last = (*last).max(spid);
        }
    }

    fn number(&self, sheet: SheetId, spid: u32) -> u32 {
        let k = (spid / CLUSTER) as usize;
        let others = self
            .clusters
            .iter()
            .take(k.saturating_sub(1))
            .filter(|c| c.owner != Some(sheet))
            .count();
        spid.saturating_sub((others as u32 + 1) * CLUSTER)
    }

    pub(crate) fn drop_sheet(&mut self, sheet: SheetId) {
        self.drawings.remove(&sheet);
        for c in &mut self.clusters {
            if c.owner == Some(sheet) {
                c.owner = None;
            }
        }
    }

    fn discard(&mut self, sheet: SheetId) {
        self.drawings.remove(&sheet);
        while self.clusters.last().is_some_and(|c| c.owner == Some(sheet)) {
            self.clusters.pop();
        }
    }
}

impl Workbook {
    pub fn legacy_shape_new(&mut self, sheet: SheetId) -> u32 {
        self.shape_ids.shape(sheet)
    }

    pub fn legacy_drawing_ensure(&mut self, sheet: SheetId) {
        self.shape_ids.ensure(sheet);
    }

    pub fn legacy_shape_loaded(&mut self, sheet: SheetId, spid: u32) {
        self.shape_ids.loaded(sheet, spid);
    }

    pub fn legacy_shape_number(&self, sheet: SheetId, spid: u32) -> u32 {
        self.shape_ids.number(sheet, spid)
    }

    pub fn legacy_number_copy(&mut self, sheet: SheetId, controls: usize) -> Vec<u32> {
        self.shape_ids.discard(sheet);
        let ids = (0..controls).map(|_| self.shape_ids.shape(sheet)).collect();
        if let Some(m) = self.cell_notes.get_mut(&sheet) {
            for n in m.values_mut() {
                n.spid = self.shape_ids.shape(sheet);
            }
        }
        ids
    }
}

pub fn clusters(ids: impl IntoIterator<Item = u32>) -> Vec<u32> {
    let mut v: Vec<u32> = ids.into_iter().map(|i| i / CLUSTER).collect();
    v.sort_unstable();
    v.dedup();
    v
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn clusters_go_to_drawings_as_they_come() {
        let (a, b, c) = (SheetId(1), SheetId(2), SheetId(3));
        let mut s = ShapeIds::default();
        assert_eq!(s.shape(a), 1025);
        s.ensure(c);
        assert_eq!(s.shape(b), 3073);
        assert_eq!(s.shape(a), 1026);
        assert_eq!(s.shape(c), 2049);
        assert_eq!(s.number(b, 3073), 1);

        s.drop_sheet(c);
        assert_eq!(s.shape(c), 4097);
    }

    #[test]
    fn a_full_cluster_is_followed_by_a_new_one() {
        let (a, b) = (SheetId(1), SheetId(2));
        let mut s = ShapeIds::default();
        s.ensure(a);
        s.ensure(b);
        for _ in 1..CLUSTER {
            s.shape(a);
        }
        let next = s.shape(a);
        assert_eq!(next, 3 * CLUSTER);

        assert_eq!(s.number(a, next), CLUSTER);
    }

    #[test]
    fn a_read_drawing_goes_on_after_its_highest_id() {
        let (a, b) = (SheetId(1), SheetId(2));
        let mut s = ShapeIds::default();
        s.loaded(a, 3077);
        s.loaded(a, 3074);
        assert_eq!(s.shape(a), 3078);
        assert_eq!(s.number(a, 3078), 6);

        assert_eq!(s.shape(b), 4097);
        s.loaded(b, 3080);
        assert_eq!(s.shape(a), 3079);
    }
}
