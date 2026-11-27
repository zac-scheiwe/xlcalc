use crate::address::{CellAddr, Rect, SheetId};
use crate::cfv::{CfRule, SheetCfv};
use crate::cfv_hosed::{minimize, R4};
use crate::dfrtree::{Bounds, DfRtree, Search, ANY_KEY};
use crate::formula::{Node, SheetRef};
use crate::workbook::Workbook;
use std::collections::HashMap;

#[derive(Clone, Debug, Default, PartialEq)]
pub struct CfBlock {
    pub areas: Vec<Rect>,
    pub rules: Vec<u32>,

    pub uid: u32,
}

const CF_KIND: u32 = 8;

fn bounds(r: &Rect) -> Bounds {
    Bounds::new(r.r0 as i32, r.r1 as i32, r.c0 as i32, r.c1 as i32)
}

fn bounding(v: &[Rect]) -> Bounds {
    let Some(first) = v.first() else {
        return bounds(&Rect {
            r0: 0,
            c0: 0,
            r1: 0,
            c1: 0,
        });
    };
    let mut all = *first;
    for r in v {
        all.r0 = all.r0.min(r.r0);
        all.c0 = all.c0.min(r.c0);
        all.r1 = all.r1.max(r.r1);
        all.c1 = all.c1.max(r.c1);
    }
    bounds(&all)
}

fn add_item(list: &mut Vec<(i32, u32)>, prio: i32, id: u32) {
    let mut at = list.len();
    if list.last().is_some_and(|last| last.0 >= prio) {
        let (mut low, mut high) = (0, list.len());
        at = 0;
        while low != high {
            at = (high - low) / 2 + low;
            match list[at].0.cmp(&prio) {
                std::cmp::Ordering::Equal => break,
                std::cmp::Ordering::Less => {
                    at += 1;
                    low = at;
                }
                std::cmp::Ordering::Greater => high = at,
            }
        }
    }
    list.insert(at, (prio, id));
}

struct Pending {
    block: usize,

    own: bool,
}

#[derive(Clone, Debug, PartialEq)]
pub struct Placed {
    pub rule: CfRule,
    pub areas: Vec<Rect>,

    pub block: usize,

    pub seq: u32,
}

impl Placed {
    pub fn anchor(&self) -> (u32, u32) {
        crate::cfv_edit::bbox_tl(&self.areas)
    }
}

pub(crate) fn minimized(v: &[Rect]) -> Vec<Rect> {
    let mut r4: Vec<R4> = v
        .iter()
        .map(|r| [r.r0.into(), r.r1.into(), r.c0.into(), r.c1.into()])
        .collect();
    minimize(&mut r4);
    r4.iter()
        .map(|x| Rect::new(x[0] as u32, x[2] as u32, x[1] as u32, x[3] as u32))
        .collect()
}

fn subtract_ref(areas: &mut Vec<Rect>, hole: &Rect) {
    let mut i = 0;
    let mut left = areas.len();
    while left > 0 {
        left -= 1;
        let mut pieces = areas[i].subtract(hole).into_iter();
        match pieces.next() {
            Some(first) => {
                areas[i] = first;
                areas.extend(pieces);
                i += 1;
            }
            None => {
                areas.remove(i);
            }
        }
    }
}

fn remove_overlap(a: &Rect, cur: &Rect) -> Vec<Rect> {
    let (r0, r1) = (cur.r0.max(a.r0), cur.r1.min(a.r1));
    let (c0, c1) = (cur.c0.max(a.c0), cur.c1.min(a.c1));
    let mut pieces = Vec::new();
    if cur.c0 < c0 {
        pieces.push(Rect::new(cur.r0, cur.c0, cur.r1, c0 - 1));
    }
    if c1 < cur.c1 {
        pieces.push(Rect::new(cur.r0, c1 + 1, cur.r1, cur.c1));
    }
    if r0 == cur.r0 {
        if r1 < cur.r1 {
            pieces.insert(0, Rect::new(r1 + 1, c0, cur.r1, c1));
        }
        return pieces;
    }
    if r1 == cur.r1 {
        pieces.insert(0, Rect::new(cur.r0, c0, r0 - 1, c1));
        return pieces;
    }
    let above = Rect::new(cur.r0, cur.c0, r0 - 1, cur.c1);
    let below = Rect::new(r1 + 1, cur.c0, cur.r1, cur.c1);
    match (c0 == cur.c0, c1 == cur.c1) {
        (true, true) => vec![above, below],
        (true, false) => vec![Rect::new(r0, c1 + 1, r1, cur.c1), above, below],
        (false, true) => vec![Rect::new(r0, cur.c0, r1, c0 - 1), above, below],
        (false, false) => Vec::new(),
    }
}

fn unique_cells(areas: &[Rect]) -> Vec<Rect> {
    let mut work = areas.to_vec();
    let mut out: Vec<Rect> = Vec::new();
    let mut i = 0;
    while i < work.len() {
        let cur = work[i];
        match out.iter().find(|a| a.intersects(&cur)) {
            Some(a) => {
                let pieces = remove_overlap(a, &cur);
                if pieces.is_empty() {
                    i += 1;
                } else {
                    work.splice(i..=i, pieces);
                }
            }
            None => {
                out.push(cur);
                i += 1;
            }
        }
    }
    out
}

fn same_cells(areas: &[Rect], scope: &[Rect]) -> bool {
    let unique = unique_cells(scope);
    areas == unique || (areas.len() < unique.len() && areas == minimized(&unique))
}

impl SheetCfv {
    pub fn block_of(&self, id: u32) -> Option<usize> {
        self.blocks.iter().position(|b| b.rules.contains(&id))
    }

    pub fn areas(&self, id: u32) -> &[Rect] {
        self.block_of(id)
            .map_or(&[], |b| self.blocks[b].areas.as_slice())
    }

    pub fn anchor(&self, id: u32) -> (u32, u32) {
        crate::cfv_edit::bbox_tl(self.areas(id))
    }

    pub fn covers(&self, id: u32, row: u32, col: u32) -> bool {
        self.areas(id).iter().any(|r| r.contains(row, col))
    }

    pub fn placed(&self) -> Vec<Placed> {
        let stored: Vec<(u32, usize, &[Rect])> = self
            .blocks
            .iter()
            .enumerate()
            .flat_map(|(k, b)| b.rules.iter().map(move |&id| (id, k, b.areas.as_slice())))
            .collect();
        self.rules
            .iter()
            .map(|r| {
                let at = stored.iter().position(|s| s.0 == r.id);
                Placed {
                    rule: r.clone(),
                    areas: at.map_or_else(Vec::new, |k| stored[k].2.to_vec()),
                    block: at.map_or(0, |k| stored[k].1),
                    seq: at.map_or(0, |k| k as u32 + 1),
                }
            })
            .collect()
    }

    pub fn set_placed(&mut self, placed: Vec<Placed>) {
        self.group_placed(placed);
        self.reindex();
    }

    pub(crate) fn append_placed(&mut self, placed: Vec<Placed>, kept: usize) {
        let uids: Vec<u32> = self.blocks.iter().take(kept).map(|b| b.uid).collect();
        self.group_placed(placed);
        for k in 0..self.blocks.len() {
            match uids.get(k) {
                Some(&uid) => self.blocks[k].uid = uid,
                None => {
                    self.blocks[k].uid = self.fresh_uid();
                    self.index_block(k);
                }
            }
        }
    }

    fn group_placed(&mut self, mut placed: Vec<Placed>) {
        placed.retain(|p| !p.areas.is_empty());
        self.rules = placed.iter().map(|p| p.rule.clone()).collect();
        placed.sort_by_key(|p| p.seq);
        self.blocks.clear();
        let mut of: Vec<usize> = Vec::new();
        for p in placed {
            let held = (0..self.blocks.len())
                .find(|&k| of[k] == p.block && self.blocks[k].areas == p.areas);
            match held {
                Some(k) => self.blocks[k].rules.push(p.rule.id),
                None => {
                    of.push(p.block);
                    self.blocks.push(CfBlock {
                        areas: p.areas,
                        rules: vec![p.rule.id],
                        uid: 0,
                    });
                }
            }
        }
    }

    pub fn add_rule_at(&mut self, rule: CfRule, prio: i32, areas: Vec<Rect>) -> u32 {
        let id = self.new_rule(rule, prio);
        match self.blocks.iter_mut().find(|b| b.areas == areas) {
            Some(b) => b.rules.push(id),
            None => self.push_block(areas, vec![id]),
        }
        id
    }

    fn fresh_uid(&mut self) -> u32 {
        self.next_uid += 1;
        self.next_uid
    }

    fn index_block(&mut self, b: usize) {
        let block = &self.blocks[b];
        for a in &block.areas {
            self.tree.insert(bounds(a), CF_KIND, ANY_KEY, block.uid);
        }
    }

    fn unindex_block(&mut self, b: usize) {
        let block = &self.blocks[b];
        for a in &block.areas {
            self.tree.remove(&bounds(a), block.uid);
        }
    }

    pub(crate) fn reindex(&mut self) {
        self.tree = DfRtree::default();
        for b in 0..self.blocks.len() {
            self.blocks[b].uid = self.fresh_uid();
            self.index_block(b);
        }
    }

    fn push_block(&mut self, areas: Vec<Rect>, rules: Vec<u32>) {
        let uid = self.fresh_uid();
        self.blocks.push(CfBlock { areas, rules, uid });
        self.index_block(self.blocks.len() - 1);
    }

    fn replace_areas(&mut self, b: usize, areas: Vec<Rect>) {
        self.unindex_block(b);
        self.blocks[b].areas = areas;
        self.index_block(b);
    }

    pub(crate) fn reenter(&mut self, b: usize) {
        self.unindex_block(b);
        self.index_block(b);
    }

    fn identical_block(&self, range: &[Rect]) -> Option<usize> {
        let found = self
            .tree
            .search(Some(&bounding(range)), Search::Inside, CF_KIND, ANY_KEY);
        found.iter().find_map(|&(_, uid)| {
            self.blocks
                .iter()
                .position(|b| b.uid == uid && b.areas == range)
        })
    }

    pub fn listing(&self, range: &[Rect]) -> Vec<u32> {
        let all = bounding(range);
        let in_all = |a: &Rect| {
            let a = bounds(a);
            a.row0 <= all.row1 && all.row0 <= a.row1 && a.col0 <= all.col1 && all.col0 <= a.col1
        };
        let mut list = Vec::new();
        for (at, uid) in self
            .tree
            .search(Some(&all), Search::Overlaps, CF_KIND, ANY_KEY)
        {
            let Some(b) = self.blocks.iter().position(|b| b.uid == uid) else {
                continue;
            };
            let block = &self.blocks[b];
            let first = block.areas.iter().find(|a| in_all(a)).map(bounds);
            if !self.meets(b, range) || (block.areas.len() > 1 && first != Some(at)) {
                continue;
            }
            for &id in &block.rules {
                if let Some(i) = self.index_of(id) {
                    add_item(&mut list, self.rules[i].prio, id);
                }
            }
        }
        list.into_iter().map(|x| x.1).collect()
    }

    pub(crate) fn fresh_id(&mut self) -> u32 {
        self.next_id += 1;
        self.next_id
    }

    fn grow_block(&mut self, id: u32, more: &[Rect]) {
        if let Some(b) = self.block_of(id) {
            let mut areas = self.blocks[b].areas.clone();
            areas.extend_from_slice(more);
            self.replace_areas(b, minimized(&areas));
        }
    }

    fn new_rule(&mut self, mut rule: CfRule, prio: i32) -> u32 {
        self.next_id += 1;
        rule.id = self.next_id;
        rule.prio = prio;
        self.rules.push(rule);
        self.next_id
    }

    fn drop_empty_blocks(&mut self) {
        let cleared: Vec<u32> = self
            .blocks
            .iter()
            .filter(|b| b.areas.is_empty())
            .flat_map(|b| b.rules.clone())
            .collect();
        let (gone, kept): (Vec<_>, Vec<_>) = std::mem::take(&mut self.rules)
            .into_iter()
            .partition(|r| cleared.contains(&r.id));
        self.rules = kept;
        self.retired.extend(gone);
        self.blocks
            .retain(|b| !b.areas.is_empty() && !b.rules.is_empty());
    }

    fn subtract_from_block(&mut self, b: usize, range: &[Rect]) {
        let mut areas = self.blocks[b].areas.clone();
        for q in range {
            subtract_ref(&mut areas, q);
        }
        self.replace_areas(b, areas);
        let joined = minimized(&self.blocks[b].areas);
        if joined != self.blocks[b].areas {
            self.replace_areas(b, joined);
        }
    }

    pub fn next_priority(&self) -> i32 {
        let positive = self.rules.iter().map(|r| r.prio).filter(|&p| p > 0);
        positive.max().unwrap_or(0).wrapping_add(1)
    }

    pub fn add_on_range(&mut self, rule: CfRule, range: &[Rect]) -> u32 {
        let old = self.identical_block(range);
        let prio = self.next_priority();
        let id = self.new_rule(rule, prio);
        match old {
            Some(b) => {
                self.blocks[b].rules.push(id);
                self.reenter(b);
                self.reenter(b);
            }
            None => self.push_block(minimized(&unique_cells(range)), vec![id]),
        }
        id
    }

    fn meets(&self, b: usize, range: &[Rect]) -> bool {
        self.blocks[b]
            .areas
            .iter()
            .any(|a| range.iter().any(|q| a.intersects(q)))
    }

    fn put_on_scope(&mut self, id: u32, scope: &[Rect], old: Option<&CfRule>) -> Option<Pending> {
        let b = self.block_of(id)?;
        if !self.meets(b, scope) {
            return None;
        }
        if same_cells(&self.blocks[b].areas, scope) {
            self.keep_on_scope(b, scope);
            return Some(Pending {
                block: b,
                own: true,
            });
        }
        let areas = self.blocks[b].areas.clone();
        let rules = self.blocks[b].rules.clone();
        self.subtract_from_block(b, scope);
        if self.blocks[b].areas.is_empty() {
            self.blocks.remove(b);
        } else {
            let mut copies = Vec::new();
            for &rule in &rules {
                let Some(i) = self.index_of(rule) else {
                    continue;
                };
                let mut copy = match old {
                    Some(o) if o.id == rule => o.clone(),
                    _ => self.rules[i].clone(),
                };
                copy.id = self.fresh_id();
                copies.push(copy.id);
                self.rules.insert(i + 1, copy);
            }
            self.blocks[b].rules = copies;
        }
        Some(self.pending_block(minimized(&areas), rules))
    }

    fn keep_on_scope(&mut self, b: usize, scope: &[Rect]) {
        self.unindex_block(b);
        self.blocks[b].areas = minimized(&unique_cells(scope));
    }

    fn pending_block(&mut self, areas: Vec<Rect>, rules: Vec<u32>) -> Pending {
        let uid = self.fresh_uid();
        self.blocks.push(CfBlock { areas, rules, uid });
        Pending {
            block: self.blocks.len() - 1,
            own: false,
        }
    }

    fn settle(&mut self, p: &Pending) {
        self.index_block(p.block);
        if p.own {
            self.reenter(p.block);
        }
    }

    fn merge_blocks(&mut self, from: usize, to: usize, scope: &[Rect]) {
        let gone = self.blocks.remove(from);
        let to = if from < to { to - 1 } else { to };
        let mut areas = self.blocks[to].areas.clone();
        areas.extend_from_slice(scope);
        self.replace_areas(to, minimized(&areas));
        let old = std::mem::replace(&mut self.blocks[to].rules, gone.rules);
        self.rules.retain(|r| !old.contains(&r.id));
    }

    pub fn clear_range(&mut self, range: &[Rect]) {
        for b in (0..self.blocks.len()).rev() {
            let areas = &self.blocks[b].areas;
            let common: Vec<Rect> = range
                .iter()
                .rev()
                .flat_map(|q| areas.iter().rev().filter_map(|a| a.intersection(q)))
                .collect();
            if !common.is_empty() {
                self.subtract_from_block(b, &common);
            }
        }
        self.drop_empty_blocks();
    }

    fn delete_rule_at(&mut self, id: u32, scope: &[Rect]) -> Option<Pending> {
        let b = self.block_of(id)?;
        if self.blocks[b].rules.len() == 1 {
            self.subtract_from_block(b, scope);
            self.drop_empty_blocks();
            return None;
        }
        if !self.meets(b, scope) {
            return None;
        }
        if same_cells(&self.blocks[b].areas, scope) {
            self.keep_on_scope(b, scope);
            self.blocks[b].rules.retain(|&x| x != id);
            self.rules.retain(|r| r.id != id);
            return Some(Pending {
                block: b,
                own: true,
            });
        }
        let areas = self.blocks[b].areas.clone();
        let mut others = self.blocks[b].rules.clone();
        others.retain(|&x| x != id);
        self.subtract_from_block(b, scope);
        if self.blocks[b].areas.is_empty() {
            self.blocks.remove(b);
            self.rules.retain(|r| r.id != id);
        } else {
            self.blocks[b].rules = vec![id];
        }
        Some(self.pending_block(minimized(&areas), others))
    }

    pub fn set_applies_to(&mut self, id: u32, range: &[Rect]) {
        let Some(source) = self.block_of(id) else {
            return;
        };
        match self.identical_block(range) {
            Some(t) if t == source => return,
            Some(t) => {
                self.blocks[source].rules.retain(|&x| x != id);
                self.blocks[t].rules.push(id);
            }
            None if self.blocks[source].rules.len() == 1 => {
                self.replace_areas(source, range.to_vec());
            }
            None => {
                self.push_block(range.to_vec(), vec![id]);
                self.blocks[source].rules.retain(|&x| x != id);
            }
        }
        if self.blocks[source].rules.is_empty() {
            self.unindex_block(source);
        }
        self.drop_empty_blocks();
    }
}

impl Workbook {
    fn cfv_reanchored(
        &mut self,
        sheet: SheetId,
        source: Option<u32>,
        change: impl FnOnce(&mut SheetCfv),
    ) {
        let sh = self.cfv_sheet_mut(sheet);
        let before: HashMap<u32, (u32, u32)> =
            sh.rules.iter().map(|r| (r.id, sh.anchor(r.id))).collect();
        let of_source = source.and_then(|id| before.get(&id).copied());
        change(sh);
        let at = |c: (u32, u32)| CellAddr::new(sheet, c.0, c.1);
        let moved: Vec<(usize, CellAddr, CellAddr)> = sh
            .rules
            .iter()
            .enumerate()
            .filter_map(|(i, r)| {
                let from = before.get(&r.id).copied().or(of_source)?;
                let to = sh.anchor(r.id);
                (from != to).then(|| (i, at(from), at(to)))
            })
            .collect();
        for (i, from, to) in moved {
            let rule = &self.cfv_sheet_mut(sheet).rules[i];
            let f = [rule.f1.clone(), rule.f2.clone()].map(|f| {
                f.map(|t| match t.starts_with('=') {
                    true => self.cfv_retarget(&t, from, to).unwrap_or(t),
                    false => t,
                })
            });
            let rule = &mut self.cfv_sheet_mut(sheet).rules[i];
            [rule.f1, rule.f2] = f;
        }
    }

    pub fn cfv_clear_range(&mut self, sheet: SheetId, range: &[Rect]) {
        self.cfv_reanchored(sheet, None, |sh| sh.clear_range(range));
    }

    pub fn cfv_retired_names(&self) -> Vec<(String, String)> {
        let mut out = Vec::new();
        for (&sheet, sh) in &self.cfv {
            let texts = sh.retired.iter().flat_map(|r| [&r.f1, &r.f2]);
            for text in texts.filter_map(|t| t.as_deref()?.strip_prefix('=')) {
                let Ok((f, _)) = self.compile_normalized(text, CellAddr::new(sheet, 0, 0)) else {
                    continue;
                };
                crate::formula::walk(&f.root, &mut |n| {
                    let (key, on) = match n {
                        Node::Name {
                            key,
                            sheet: Some(SheetRef::Sheet(on)),
                            ..
                        } => (key, *on),
                        Node::Name { key, .. } => (key, sheet),
                        _ => return,
                    };
                    if let Some(s) = self.sheet(on) {
                        out.push((s.name.to_string(), key.to_string()));
                    }
                });
            }
        }
        out
    }

    pub(crate) fn cfv_grow_block(&mut self, sheet: SheetId, id: u32, more: &[Rect]) {
        self.cfv_reanchored(sheet, None, |sh| sh.grow_block(id, more));
    }

    pub fn cfv_delete_rule_at(&mut self, sheet: SheetId, id: u32, scope: &[Rect]) {
        let mut pending = None;
        self.cfv_reanchored(sheet, Some(id), |sh| {
            pending = sh.delete_rule_at(id, scope);
        });
        if let Some(p) = pending {
            self.cfv_settle_merged(sheet, &p, scope);
        }
    }

    fn cfv_settle_merged(&mut self, sheet: SheetId, p: &Pending, scope: &[Rect]) {
        let from = p.block;
        let sh = self.cfv_sheet_mut(sheet);
        let placed = sh.placed();
        let of = |b: usize| -> Vec<Placed> {
            let rules = &sh.blocks[b].rules;
            rules
                .iter()
                .filter_map(|&x| placed.iter().find(|p| p.rule.id == x).cloned())
                .collect()
        };
        let mine = of(from);
        let others: Vec<(usize, Vec<Placed>)> = (0..sh.blocks.len())
            .filter(|&b| b != from)
            .map(|b| (b, of(b)))
            .collect();
        let to = others.into_iter().find(|(_, theirs)| {
            theirs.len() == mine.len()
                && theirs
                    .iter()
                    .zip(&mine)
                    .all(|(a, b)| a.rule.prio == b.rule.prio && self.cf_same_rule(sheet, a, b))
        });
        match to {
            Some((to, _)) => {
                self.cfv_reanchored(sheet, None, |sh| sh.merge_blocks(from, to, scope));
            }
            None => self.cfv_sheet_mut(sheet).settle(p),
        }
    }

    pub fn cfv_put_on_scope(
        &mut self,
        sheet: SheetId,
        id: u32,
        scope: &[Rect],
        old: Option<&CfRule>,
        change: impl FnOnce(&mut SheetCfv),
    ) {
        let mut pending = None;
        self.cfv_reanchored(sheet, Some(id), |sh| {
            pending = sh.put_on_scope(id, scope, old);
        });
        change(self.cfv_sheet_mut(sheet));
        if let Some(p) = pending {
            self.cfv_settle_merged(sheet, &p, scope);
        }
    }

    pub fn cfv_put_format(&mut self, sheet: SheetId, id: u32) {
        let scope = self.cfv_sheet_mut(sheet).areas(id).to_vec();
        self.cfv_put_on_scope(sheet, id, &scope, None, |_| {});
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::cfv::{CfKind, Dxf};

    fn rule(f: &str) -> CfRule {
        CfRule {
            id: 0,
            kind: CfKind::Expression,
            stop: true,
            prio: 0,
            f1: Some(f.into()),
            f2: None,
            dxf: Dxf::default(),
        }
    }

    fn r(r0: u32, c0: u32, r1: u32, c1: u32) -> Rect {
        Rect::new(r0, c0, r1, c1)
    }

    fn put(sh: &mut SheetCfv, id: u32, scope: &[Rect]) {
        if let Some(p) = sh.put_on_scope(id, scope, None) {
            sh.settle(&p);
        }
    }

    fn delete(sh: &mut SheetCfv, id: u32, scope: &[Rect]) {
        if let Some(p) = sh.delete_rule_at(id, scope) {
            sh.settle(&p);
        }
    }

    fn blocks(sh: &SheetCfv) -> Vec<(Vec<Rect>, Vec<u32>)> {
        sh.blocks
            .iter()
            .map(|b| (b.areas.clone(), b.rules.clone()))
            .collect()
    }

    #[test]
    fn a_block_with_an_equal_rule_array_takes_the_other() {
        let mut wb = Workbook::new();
        let s = wb.sheet_at(0).id;
        let sh = wb.cfv_sheet_mut(s);
        let a = sh.add_on_range(rule("=1"), &[r(0, 0, 3, 0)]);
        let b = sh.add_on_range(rule("=2"), &[r(0, 0, 3, 0)]);
        wb.cfv_put_on_scope(s, a, &[r(1, 0, 1, 0)], None, |_| {});

        let sh = wb.cfv_sheet_mut(s);
        assert_eq!(blocks(sh), [(vec![r(0, 0, 3, 0)], vec![a, b])]);
        assert_eq!(sh.rules.len(), 2);

        wb.cfv_put_on_scope(s, a, &[r(1, 0, 1, 0)], None, |sh| sh.rules[0].stop = false);
        assert_eq!(wb.cfv_sheet_mut(s).blocks.len(), 2);
    }

    #[test]
    fn clearing_takes_the_common_cells_from_the_last_area_back() {
        let mut sh = SheetCfv::default();
        sh.add_on_range(rule("=1"), &[r(0, 0, 5, 0)]);
        sh.clear_range(&[r(1, 0, 1, 0), r(3, 0, 3, 0)]);
        assert_eq!(
            sh.blocks[0].areas,
            [r(0, 0, 0, 0), r(4, 0, 5, 0), r(2, 0, 2, 0)]
        );
    }

    #[test]
    fn a_put_on_its_own_cells_joins_the_areas_of_a_block() {
        let mut sh = SheetCfv::default();
        let a = sh.add_on_range(rule("=1"), &[r(0, 0, 0, 0)]);
        sh.set_applies_to(a, &[r(10, 0, 10, 0), r(9, 0, 9, 0)]);
        let scope = sh.areas(a).to_vec();
        put(&mut sh, a, &scope);
        assert_eq!(sh.areas(a), [r(9, 0, 10, 0)]);
    }

    #[test]
    fn the_next_priority_passes_over_those_that_wrapped() {
        let mut sh = SheetCfv::default();
        assert_eq!(sh.next_priority(), 1);
        sh.add_on_range(rule("=1"), &[r(0, 0, 0, 0)]);
        sh.rules[0].prio = i32::MAX;
        assert_eq!(sh.next_priority(), i32::MIN);
        sh.add_on_range(rule("=2"), &[r(1, 0, 1, 0)]);
        assert_eq!(sh.next_priority(), i32::MIN);
    }

    #[test]
    fn subtract_puts_further_pieces_last() {
        let mut v = vec![r(0, 0, 2, 2), r(5, 0, 5, 0)];
        subtract_ref(&mut v, &r(1, 1, 1, 1));
        assert_eq!(
            v,
            [
                r(0, 0, 0, 2),
                r(5, 0, 5, 0),
                r(2, 0, 2, 2),
                r(1, 0, 1, 0),
                r(1, 2, 1, 2)
            ]
        );
    }

    #[test]
    fn add_joins_the_block_on_the_same_areas_and_joins_new_areas() {
        let mut sh = SheetCfv::default();
        let a = sh.add_on_range(rule("=1"), &[r(0, 0, 0, 0), r(1, 0, 1, 0)]);
        assert_eq!(blocks(&sh), [(vec![r(0, 0, 1, 0)], vec![a])]);
        let b = sh.add_on_range(rule("=2"), &[r(0, 1, 0, 1)]);
        let c = sh.add_on_range(rule("=3"), &[r(0, 0, 1, 0)]);
        assert_eq!(
            blocks(&sh),
            [
                (vec![r(0, 0, 1, 0)], vec![a, c]),
                (vec![r(0, 1, 0, 1)], vec![b])
            ]
        );
    }

    #[test]
    fn range_change_moves_a_rule_between_blocks() {
        let mut sh = SheetCfv::default();
        let a = sh.add_on_range(rule("=1"), &[r(0, 0, 0, 0)]);
        let b = sh.add_on_range(rule("=2"), &[r(0, 0, 0, 0)]);
        sh.set_applies_to(a, &[r(2, 0, 2, 0)]);
        assert_eq!(
            blocks(&sh),
            [
                (vec![r(0, 0, 0, 0)], vec![b]),
                (vec![r(2, 0, 2, 0)], vec![a])
            ]
        );
        sh.set_applies_to(b, &[r(2, 0, 2, 0)]);
        assert_eq!(blocks(&sh), [(vec![r(2, 0, 2, 0)], vec![a, b])]);
    }

    #[test]
    fn deleting_one_rule_of_a_block_leaves_the_others_their_areas() {
        let mut sh = SheetCfv::default();
        let a = sh.add_on_range(rule("=1"), &[r(0, 0, 0, 1)]);
        let b = sh.add_on_range(rule("=2"), &[r(0, 0, 0, 1)]);
        delete(&mut sh, a, &[r(0, 0, 0, 0)]);
        assert_eq!(
            blocks(&sh),
            [
                (vec![r(0, 1, 0, 1)], vec![a]),
                (vec![r(0, 0, 0, 1)], vec![b])
            ]
        );

        delete(&mut sh, b, &[r(0, 0, 0, 1)]);
        assert_eq!(blocks(&sh), [(vec![r(0, 1, 0, 1)], vec![a])]);
        assert_eq!(sh.rules.len(), 1);
    }

    #[test]
    fn clear_drops_a_block_left_without_areas() {
        let mut sh = SheetCfv::default();
        let a = sh.add_on_range(rule("=1"), &[r(0, 0, 0, 0)]);
        let b = sh.add_on_range(rule("=2"), &[r(0, 0, 0, 1)]);
        sh.clear_range(&[r(0, 0, 0, 0)]);
        assert_eq!(blocks(&sh), [(vec![r(0, 1, 0, 1)], vec![b])]);
        assert_eq!(sh.rules.len(), 1);

        assert_eq!(sh.retired.iter().map(|r| r.id).collect::<Vec<_>>(), [a]);
    }

    #[test]
    fn a_cleared_formats_names_stay_in_use() {
        let mut wb = Workbook::new();
        let sheet = wb.sheets().next().map(|s| s.id).unwrap();
        let at = [r(0, 0, 0, 0)];
        wb.cfv_sheet_mut(sheet).add_on_range(rule("=used>0"), &at);
        assert_eq!(wb.cfv_retired_names(), []);
        wb.cfv_clear_range(sheet, &at);
        let name = wb.sheet(sheet).map(|s| s.name.to_string()).unwrap();
        assert_eq!(wb.cfv_retired_names(), [(name, "USED".to_string())]);
    }

    #[test]
    fn overlaps_are_cut_by_the_first_earlier_area() {
        let a = r(2, 2, 4, 4);

        assert_eq!(
            remove_overlap(&a, &r(2, 1, 6, 5)),
            [r(5, 2, 6, 4), r(2, 1, 6, 1), r(2, 5, 6, 5)]
        );
        assert_eq!(remove_overlap(&a, &r(0, 3, 3, 3)), [r(0, 3, 1, 3)]);
        assert_eq!(
            remove_overlap(&a, &r(3, 3, 5, 5)),
            [r(5, 3, 5, 4), r(3, 5, 5, 5)]
        );

        assert_eq!(
            remove_overlap(&a, &r(0, 2, 6, 6)),
            [r(2, 5, 4, 6), r(0, 2, 1, 6), r(5, 2, 6, 6)]
        );
        assert_eq!(
            remove_overlap(&a, &r(0, 0, 6, 4)),
            [r(2, 0, 4, 1), r(0, 0, 1, 4), r(5, 0, 6, 4)]
        );
        assert_eq!(
            remove_overlap(&a, &r(0, 3, 6, 3)),
            [r(0, 3, 1, 3), r(5, 3, 6, 3)]
        );
        assert_eq!(remove_overlap(&a, &r(0, 0, 6, 6)), []);
        assert_eq!(
            unique_cells(&[a, r(0, 0, 6, 6), r(0, 0, 0, 0)]),
            [a, r(0, 0, 0, 0)]
        );
        assert_eq!(
            unique_cells(&[r(0, 0, 1, 0), r(0, 0, 1, 1), r(1, 1, 1, 2)]),
            [r(0, 0, 1, 0), r(0, 1, 1, 1), r(1, 2, 1, 2)]
        );
    }

    #[test]
    fn a_put_through_part_of_a_block_leaves_the_old_rules_there() {
        let mut sh = SheetCfv::default();
        let a = sh.add_on_range(rule("=1"), &[r(0, 0, 0, 1)]);
        let b = sh.add_on_range(rule("=2"), &[r(0, 0, 0, 1)]);
        let c = sh.add_on_range(rule("=3"), &[r(1, 0, 1, 0)]);
        put(&mut sh, b, &[r(0, 0, 0, 1)]);
        assert_eq!(blocks(&sh)[0], (vec![r(0, 0, 0, 1)], vec![a, b]));
        put(&mut sh, b, &[r(0, 0, 0, 0)]);
        let (a2, b2) = (c + 1, c + 2);
        assert_eq!(
            blocks(&sh),
            [
                (vec![r(0, 1, 0, 1)], vec![a2, b2]),
                (vec![r(1, 0, 1, 0)], vec![c]),
                (vec![r(0, 0, 0, 1)], vec![a, b])
            ]
        );
        let listed: Vec<u32> = sh.rules.iter().map(|x| x.id).collect();
        assert_eq!(listed, [a, a2, b, b2, c]);

        put(&mut sh, c, &[r(1, 0, 2, 0)]);
        assert_eq!(blocks(&sh)[2], (vec![r(1, 0, 1, 0)], vec![c]));
    }

    #[test]
    fn rules_of_one_priority_list_as_the_binary_insert_leaves_them() {
        let mut sh = SheetCfv::default();
        let all = [r(0, 0, 0, 0)];
        let a = sh.add_rule_at(rule("=1"), 1, all.to_vec());
        let b = sh.add_rule_at(rule("=2"), 1, all.to_vec());
        assert_eq!(sh.listing(&all), [b, a]);
        let c = sh.add_rule_at(rule("=3"), 1, all.to_vec());
        assert_eq!(sh.listing(&all), [b, c, a]);
    }

    #[test]
    fn add_misses_an_identical_block_once_the_tree_has_levels() {
        let mut sh = SheetCfv::default();
        let cell = |i: u32| [r(i % 10, i / 10, i % 10, i / 10)];
        for i in 0..200 {
            sh.add_on_range(rule("=1"), &cell(i));
        }
        let missed = (0..200).find(|&i| {
            let before = sh.blocks.len();
            sh.add_on_range(rule("=2"), &cell(i));
            sh.blocks.len() > before
        });
        let missed = missed.expect("every block was found");

        assert_eq!(sh.listing(&cell(missed)).len(), 2);
    }
}
