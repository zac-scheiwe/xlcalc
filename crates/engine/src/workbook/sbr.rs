use std::collections::BTreeSet;

use rustc_hash::{FxHashMap, FxHashSet};

use crate::address::CellAddr;
use crate::formula::{Formula, Node};
use crate::workbook::notice::Command;
use crate::workbook::{FormulaId, Workbook};

const SLOTS: usize = 100;

const WHOLE_ROW: u16 = 0xffff;

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub(crate) struct Slot {
    col: u16,
    row: u32,
}

#[derive(Clone, Debug, Default)]
pub(crate) struct Sbr {
    slots: Vec<Option<Slot>>,

    cmd: Option<Command>,

    entry: bool,

    spared: Option<FxHashSet<FormulaId>>,

    edits: u32,
}

impl Sbr {
    fn forget_slots(&mut self) {
        self.slots.clear();
    }

    fn rec(&mut self, recs: &[Option<Slot>]) {
        if recs.len() > SLOTS {
            self.forget_slots();
            return;
        }
        if self.slots.len() < recs.len() {
            self.slots.resize(recs.len(), None);
        }
        self.slots[..recs.len()].copy_from_slice(recs);
    }

    fn slot(&self, i: usize) -> Option<Slot> {
        self.slots.get(i).copied().flatten()
    }
}

fn list_of(wb: &Workbook, table: &str, spec: &crate::workbook::tables::Spec) -> Option<u16> {
    let t = wb.table_by_name(table)?;
    let it = spec.items;
    let part = if it.this_row && !it.headers && !it.totals {
        1
    } else if it.data && !it.headers && !it.totals {
        0
    } else if it.headers && !it.data && !it.totals {
        2
    } else {
        return None;
    };
    let k = match &spec.cols {
        None if part == 0 => 0,
        None => return None,
        Some((a, b)) => {
            let pos = |n: &str| t.columns.iter().position(|c| c.eq_ignore_ascii_case(n));
            let (a, b) = (pos(a)?, pos(b)?);
            if a != b {
                if part == 2 {
                    return None;
                }
                0
            } else {
                a + 1
            }
        }
    };
    u16::try_from(5 * k + part).ok()
}

fn lists_of_formula(wb: &Workbook, f: &Formula, name: &str) -> Result<Vec<u16>, ()> {
    let mut out = Vec::new();
    let mut bad = false;
    crate::formula::walk(&f.root, &mut |n| {
        if let Node::StructRef { table, spec, .. } = n {
            if table.to_lowercase() == name {
                match list_of(wb, table, spec) {
                    Some(l) => out.push(l),
                    None => bad = true,
                }
            }
        }
    });
    if bad {
        Err(())
    } else {
        Ok(out)
    }
}

impl Workbook {
    pub(crate) fn sbr_command(&mut self, cmd: Command) {
        let sbr = &mut self.sbr;
        sbr.edits = 0;
        sbr.cmd = Some(cmd);
        sbr.entry = cmd == Command::Entry;
        match cmd {
            Command::Entry
            | Command::Setting
            | Command::View
            | Command::ActivateSheet
            | Command::Format
            | Command::ClearFormats => {}

            Command::TableAdd | Command::Table => sbr.forget_slots(),

            Command::TableResize => sbr.spared = None,
            _ => {
                sbr.forget_slots();
                self.sbr_forget_lists();
            }
        }
    }

    pub(crate) fn sbr_command_end(&mut self) {
        if self.sbr.cmd == Some(Command::TableResize) {
            self.sbr.forget_slots();
            if self.sbr.spared.take().is_none() {
                self.sbr_forget_lists();
            }
        }
        self.sbr.cmd = None;
        self.sbr.entry = false;
    }

    fn sbr_forget_lists(&mut self) {
        for t in &mut self.tables {
            t.sbr_lists = None;
        }
    }

    pub(crate) fn sbr_in_command(&self, cmd: Command) -> bool {
        self.sbr.cmd == Some(cmd)
    }

    pub(crate) fn sbr_spared(&self) -> FxHashSet<FormulaId> {
        self.sbr.spared.clone().unwrap_or_default()
    }

    pub(crate) fn sbr_table_added(&mut self, ti: usize) {
        let known = self.sbr.cmd == Some(Command::TableAdd);
        self.tables[ti].sbr_lists = known.then(BTreeSet::new);
    }

    fn sbr_table_lists(&self, ti: usize) -> Option<FxHashMap<u16, Vec<FormulaId>>> {
        let name = self.tables[ti].name.to_lowercase();
        self.tables[ti].sbr_lists.as_ref()?;
        let mut out: FxHashMap<u16, Vec<FormulaId>> = FxHashMap::default();
        for fid in self.formula_ids() {
            for l in lists_of_formula(self, &self.formula(fid).formula, &name).ok()? {
                let v = out.entry(l).or_default();
                if v.last() != Some(&fid) {
                    v.push(fid);
                }
            }
        }
        Some(out)
    }

    fn sbr_note_lists(&mut self, addr: CellAddr) {
        let Some(fid) = self.formula_at(addr) else {
            return;
        };
        let f = self.formula(fid).formula.clone();
        for ti in 0..self.tables.len() {
            let got = lists_of_formula(self, &f, &self.tables[ti].name.to_lowercase());
            let entry = &mut self.tables[ti].sbr_lists;
            match (got, entry.as_mut()) {
                (Ok(ls), Some(set)) => set.extend(ls),
                (Err(()), _) => *entry = None,
                _ => {}
            }
        }
    }

    fn sbr_has_dependents(&self, fid: FormulaId) -> bool {
        let f = self.formula(fid);
        let mut deps = Vec::new();
        match f.spill {
            Some(rect) => self
                .graph
                .dependents_of_rect(f.addr.sheet, &rect, &mut deps),
            None => self
                .graph
                .dependents_of_cell(f.addr.sheet, f.addr.row, f.addr.col, &mut deps),
        }
        deps.iter().any(|&d| d != fid)
    }

    fn sbr_records(&self, cells: &[CellAddr]) -> Option<Vec<Option<Slot>>> {
        let mut recs = Vec::new();
        let mut lists: Vec<Option<Slot>> = Vec::new();
        let mut have_of: FxHashMap<usize, FxHashMap<u16, Vec<FormulaId>>> = FxHashMap::default();
        for &a in cells {
            let ti = self.tables.iter().position(|t| {
                let (d0, d1) = t.data_rows();
                t.sheet == a.sheet
                    && (d0..=d1).contains(&a.row)
                    && (t.rect.c0..=t.rect.c1).contains(&a.col)
            });
            let Some(ti) = ti else {
                match self.formula_at(a) {
                    Some(fid) if !self.sbr_has_dependents(fid) => continue,
                    None => {
                        let mut deps = Vec::new();
                        self.graph
                            .dependents_of_cell(a.sheet, a.row, a.col, &mut deps);
                        if deps.is_empty() {
                            continue;
                        }
                        return None;
                    }
                    _ => return None,
                }
            };
            let col = u16::try_from(a.col).ok()?;
            recs.push(Some(Slot { col, row: a.row }));
            if let std::collections::hash_map::Entry::Vacant(e) = have_of.entry(ti) {
                e.insert(self.sbr_table_lists(ti)?);
            }
            let have = &have_of[&ti];
            let k = (a.col - self.tables[ti].rect.c0 + 1) as u16;

            if [2, 3, 4]
                .iter()
                .any(|p| have.contains_key(&(5 * k + p)) || have.contains_key(p))
            {
                return None;
            }
            for (l, part) in [(5 * k, 0), (0, 0), (5 * k + 1, 1), (1, 1)] {
                if have.contains_key(&l) {
                    let col = if part == 1 { WHOLE_ROW } else { col };
                    lists.push(Some(Slot { col, row: a.row }));
                }
            }

            let mut deps = Vec::new();
            self.graph
                .dependents_of_cell(a.sheet, a.row, a.col, &mut deps);
            for d in deps {
                let at = self.formula(d).addr;
                if cells.contains(&at) {
                    continue;
                }
                if self.sbr_has_dependents(d) || self.table_at(at).is_some() {
                    return None;
                }
            }
        }

        recs.extend(lists);
        Some(recs)
    }

    fn sbr_rec(&mut self, cells: &[CellAddr]) {
        match self.sbr_records(cells) {
            Some(recs) => self.sbr.rec(&recs),
            None => self.sbr.forget_slots(),
        }
    }

    pub(crate) fn sbr_unknown(&mut self) {
        self.sbr.edits += 1;
        self.sbr.forget_slots();
    }

    pub(crate) fn sbr_edit(&mut self, addr: CellAddr, tables_before: &[crate::address::Rect]) {
        self.sbr.edits += 1;
        let same = self.tables.len() == tables_before.len()
            && self
                .tables
                .iter()
                .zip(tables_before)
                .all(|(t, r)| t.rect == *r);
        if !same {
            self.sbr.forget_slots();
            self.sbr_forget_lists();
            return;
        }
        self.sbr_note_lists(addr);
        if self.sbr.entry && self.sbr.edits == 1 {
            self.sbr_rec(&[addr]);
        } else {
            self.sbr.forget_slots();
        }
    }

    pub(crate) fn sbr_fill(
        &mut self,
        sheet: crate::address::SheetId,
        col: u32,
        rows: std::ops::RangeInclusive<u32>,
        all: bool,
    ) {
        if !self.sbr.entry || self.sbr.edits != 1 || !all {
            self.sbr.forget_slots();
            return;
        }
        let cells: Vec<CellAddr> = rows.map(|r| CellAddr::new(sheet, r, col)).collect();
        self.sbr_rec(&cells);
    }

    pub(crate) fn sbr_grow_spared(&mut self, ti: usize, fresh: &[u16]) -> FxHashSet<FormulaId> {
        if let Some(s) = &self.sbr.spared {
            return s.clone();
        }
        let mut spared = FxHashSet::default();
        if let (Some(have), Some(made)) =
            (self.sbr_table_lists(ti), self.tables[ti].sbr_lists.clone())
        {
            if have.keys().all(|l| made.contains(l)) {
                let mut dirty: FxHashSet<FormulaId> = FxHashSet::default();
                let mut readers: FxHashSet<FormulaId> = FxHashSet::default();
                for (i, l) in made.iter().enumerate() {
                    let Some(fs) = have.get(l) else { continue };
                    readers.extend(fs.iter().copied());
                    let row = match self.sbr.slot(i) {
                        Some(s) if s.col == WHOLE_ROW && !fresh.contains(l) => Some(s.row),
                        _ => None,
                    };
                    for &f in fs {
                        if row.is_none_or(|r| self.formula(f).addr.row == r) {
                            dirty.insert(f);
                        }
                    }
                }
                spared = readers.difference(&dirty).copied().collect();
            }
        }
        self.sbr.forget_slots();
        self.sbr.spared = Some(spared.clone());
        spared
    }
}
