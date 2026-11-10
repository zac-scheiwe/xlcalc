use crate::structure::{Axis, Shift};
use crate::{CellAddr, Rect, SheetId, Workbook};
use std::collections::BTreeMap;

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct Note {
    pub id: u32,
    pub text: String,
    pub author: String,

    pub visible: bool,
}

pub type NoteMap = BTreeMap<(u32, u32), Note>;

impl Workbook {
    pub fn notes(&self, sheet: SheetId) -> impl Iterator<Item = ((u32, u32), &Note)> {
        self.cell_notes
            .get(&sheet)
            .into_iter()
            .flat_map(|m| m.iter().map(|(k, n)| (*k, n)))
    }

    pub fn note_at(&self, sheet: SheetId, row: u32, col: u32) -> Option<&Note> {
        self.cell_notes.get(&sheet)?.get(&(row, col))
    }

    pub fn note_by_id(&self, sheet: SheetId, id: u32) -> Option<((u32, u32), &Note)> {
        self.notes(sheet).find(|(_, n)| n.id == id)
    }

    pub fn note_edit(&mut self, sheet: SheetId, id: u32, f: impl FnOnce(&mut Note)) {
        if let Some(n) = self
            .cell_notes
            .get_mut(&sheet)
            .and_then(|m| m.values_mut().find(|n| n.id == id))
        {
            f(n);
        }
    }

    pub fn note_add(
        &mut self,
        at: CellAddr,
        text: &str,
        author: &str,
        visible: bool,
    ) -> Option<u32> {
        let map = self.cell_notes.entry(at.sheet).or_default();
        if map.contains_key(&(at.row, at.col)) {
            return None;
        }
        self.note_next += 1;
        let id = self.note_next;
        map.insert(
            (at.row, at.col),
            Note {
                id,
                text: text.to_string(),
                author: author.to_string(),
                visible,
            },
        );
        Some(id)
    }

    pub fn note_remove(&mut self, sheet: SheetId, id: u32) -> bool {
        let Some(m) = self.cell_notes.get_mut(&sheet) else {
            return false;
        };
        let n = m.len();
        m.retain(|_, v| v.id != id);
        m.len() != n
    }

    pub fn notes_clear(&mut self, sheet: SheetId, r: &Rect) {
        if let Some(m) = self.cell_notes.get_mut(&sheet) {
            m.retain(|&(row, col), _| !r.contains(row, col));
        }
    }

    pub fn notes_paste(&mut self, ss: SheetId, src: Rect, ds: SheetId, pasted: Rect, cut: bool) {
        let inside: Vec<((u32, u32), Note)> = self
            .notes(ss)
            .filter(|((r, c), _)| src.contains(*r, *c))
            .map(|(k, n)| (k, n.clone()))
            .collect();
        if cut {
            self.notes_clear(ss, &src);
        }
        self.notes_clear(ds, &pasted);
        let (rows, cols) = (src.rows(), src.cols());
        let mut r = pasted.r0;
        while r + rows - 1 <= pasted.r1 {
            let mut c = pasted.c0;
            while c + cols - 1 <= pasted.c1 {
                for ((nr, nc), n) in &inside {
                    let mut n = n.clone();
                    if !cut {
                        self.note_next += 1;
                        n.id = self.note_next;
                    }
                    self.cell_notes
                        .entry(ds)
                        .or_default()
                        .insert((nr - src.r0 + r, nc - src.c0 + c), n);
                }
                c += cols;
            }
            r += rows;
        }
    }

    pub(crate) fn notes_shift(&mut self, s: &Shift) {
        let rows = s.axis == Axis::Rows;
        let Some(m) = self.cell_notes.get_mut(&s.sheet) else {
            return;
        };
        let old = std::mem::take(m);
        for ((r, c), n) in old {
            let (along, across) = if rows { (r, c) } else { (c, r) };
            let outside = s.limit.is_some_and(|(p0, p1)| across < p0 || across > p1);
            let to = if outside {
                Some(along)
            } else {
                s.map_span(along, along).map(|(a, _)| a)
            };
            if let Some(a) = to {
                m.insert(if rows { (a, c) } else { (r, a) }, n);
            }
        }
    }

    pub(crate) fn notes_sort_pairs(&mut self, pairs: &[(CellAddr, CellAddr)]) {
        let Some(first) = pairs.first() else {
            return;
        };
        let Some(m) = self.cell_notes.get_mut(&first.0.sheet) else {
            return;
        };
        if m.is_empty() {
            return;
        }
        let moved: Vec<(CellAddr, Option<Note>)> = pairs
            .iter()
            .map(|(from, to)| (*to, m.get(&(from.row, from.col)).cloned()))
            .collect();
        for (_, to) in pairs {
            m.remove(&(to.row, to.col));
        }
        for (to, n) in moved {
            if let Some(n) = n {
                m.insert((to.row, to.col), n);
            }
        }
    }

    pub(crate) fn notes_drop_sheet(&mut self, id: SheetId) {
        self.cell_notes.remove(&id);
    }

    pub(crate) fn notes_copy_sheet(&mut self, src: SheetId, new: SheetId) {
        if let Some(mut m) = self.cell_notes.get(&src).cloned() {
            for n in m.values_mut() {
                self.note_next += 1;
                n.id = self.note_next;
            }
            self.cell_notes.insert(new, m);
        }
    }
}
