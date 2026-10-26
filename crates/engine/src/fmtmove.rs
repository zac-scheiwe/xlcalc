use std::sync::Arc;

use rustc_hash::FxHashMap;

use crate::address::{CellAddr, Rect, SheetId, MAX_COLS, MAX_ROWS};
use crate::workbook::Workbook;

type Line = (u64, Arc<str>);

const CELL_LIMIT: u64 = 1 << 20;

impl Workbook {
    fn fmt_at(&self, a: CellAddr) -> Option<Arc<str>> {
        self.cellfmt.get(a).cloned()
    }

    pub(crate) fn fmt_copy_pairs(&mut self, pairs: &[(CellAddr, CellAddr)]) {
        if !self.cellfmt.any() {
            return;
        }
        let snap: Vec<Option<Arc<str>>> = pairs.iter().map(|(f, _)| self.fmt_at(*f)).collect();
        for ((_, to), f) in pairs.iter().zip(snap) {
            self.store_format(*to, f.as_deref().unwrap_or("General"));
        }
    }

    pub(crate) fn fmt_move_pairs(&mut self, pairs: &[(CellAddr, CellAddr)]) {
        if !self.cellfmt.any() {
            return;
        }
        let snap: Vec<Option<Arc<str>>> = pairs.iter().map(|(f, _)| self.fmt_at(*f)).collect();
        for ((_, to), f) in pairs.iter().zip(snap) {
            let key = (to.sheet, to.row, to.col);
            let line = self.cellfmt.line(*to).cloned();
            match f {
                Some(f) if line.as_deref() != Some(&*f) && !f.eq_ignore_ascii_case("general") => {
                    self.cellfmt.map.insert(key, f);
                }
                f => self.store_format(*to, f.as_deref().unwrap_or("General")),
            }
        }
    }

    pub(crate) fn fmt_move_block(&mut self, ss: SheetId, src: Rect, ds: SheetId, dst: (u32, u32)) {
        if !self.cellfmt.any() {
            return;
        }
        if src.is_full_columns() || src.is_full_rows() {
            let cols = src.is_full_columns();
            let (a0, a1, d0) = if cols {
                (src.c0, src.c1, dst.1)
            } else {
                (src.r0, src.r1, dst.0)
            };
            self.fmt_lines(ss, ds, cols, a0, a1, d0, true);
            return;
        }
        if src.rows() as u64 * src.cols() as u64 > CELL_LIMIT {
            return;
        }
        let mut pairs = Vec::new();
        for r in src.r0..=src.r1 {
            for c in src.c0..=src.c1 {
                pairs.push((
                    CellAddr::new(ss, r, c),
                    CellAddr::new(ds, dst.0 + r - src.r0, dst.1 + c - src.c0),
                ));
            }
        }
        let snap: Vec<Option<Arc<str>>> = pairs.iter().map(|(f, _)| self.fmt_at(*f)).collect();
        for (f, _) in &pairs {
            self.cellfmt.map.remove(&(f.sheet, f.row, f.col));
        }
        for ((_, to), f) in pairs.iter().zip(snap) {
            self.store_format(*to, f.as_deref().unwrap_or("General"));
        }
    }

    pub(crate) fn fmt_copy_block(&mut self, ss: SheetId, src: Rect, dst: CellAddr) {
        if !self.cellfmt.any() {
            return;
        }
        if src.is_full_columns() || src.is_full_rows() {
            let cols = src.is_full_columns();
            let (a0, a1, d0) = if cols {
                (src.c0, src.c1, dst.col)
            } else {
                (src.r0, src.r1, dst.row)
            };
            self.fmt_lines(ss, dst.sheet, cols, a0, a1, d0, false);
            return;
        }
        if src.rows() as u64 * src.cols() as u64 > CELL_LIMIT {
            return;
        }
        let mut pairs = Vec::new();
        for r in src.r0..=src.r1 {
            for c in src.c0..=src.c1 {
                pairs.push((
                    CellAddr::new(ss, r, c),
                    CellAddr::new(dst.sheet, dst.row + r - src.r0, dst.col + c - src.c0),
                ));
            }
        }
        self.fmt_copy_pairs(&pairs);
    }

    #[allow(clippy::too_many_arguments)]
    fn fmt_lines(
        &mut self,
        ss: SheetId,
        ds: SheetId,
        cols: bool,
        a0: u32,
        a1: u32,
        d0: u32,
        mv: bool,
    ) {
        let n = a1 - a0 + 1;
        let f = &mut self.cellfmt;
        let line_map = |f: &mut crate::cellfmt::CellFormats| {
            if cols {
                std::mem::take(&mut f.cols)
            } else {
                std::mem::take(&mut f.rows)
            }
        };
        let mut lines = line_map(f);
        let snap_lines: Vec<Option<Line>> =
            (0..n).map(|i| lines.get(&(ss, a0 + i)).cloned()).collect();
        let key = |k: &(SheetId, u32, u32)| if cols { k.2 } else { k.1 };
        let snap_cells: Vec<((SheetId, u32, u32), Arc<str>)> = f
            .map
            .iter()
            .filter(|(k, _)| k.0 == ss && (a0..=a1).contains(&key(k)))
            .map(|(k, v)| (*k, v.clone()))
            .collect();
        if mv {
            for i in 0..n {
                lines.remove(&(ss, a0 + i));
            }
            f.map
                .retain(|k, _| !(k.0 == ss && (a0..=a1).contains(&key(k))));
        }
        for i in 0..n {
            lines.remove(&(ds, d0 + i));
        }
        f.map
            .retain(|k, _| !(k.0 == ds && (d0..d0 + n).contains(&key(k))));
        for (i, l) in snap_lines.into_iter().enumerate() {
            if let Some(l) = l {
                lines.insert((ds, d0 + i as u32), l);
            }
        }
        for (k, v) in snap_cells {
            let off = key(&k) - a0;
            let nk = if cols {
                (ds, k.1, d0 + off)
            } else {
                (ds, d0 + off, k.2)
            };
            f.map.insert(nk, v);
        }
        if cols {
            f.cols = lines;
        } else {
            f.rows = lines;
        }
    }

    pub(crate) fn fmt_copy_sheet(&mut self, src: SheetId, new: SheetId) {
        let f = &mut self.cellfmt;
        let cells: Vec<_> = f
            .map
            .iter()
            .filter(|(k, _)| k.0 == src)
            .map(|(k, v)| ((new, k.1, k.2), v.clone()))
            .collect();
        f.map.extend(cells);
        let rows: Vec<_> = f
            .rows
            .iter()
            .filter(|(k, _)| k.0 == src)
            .map(|(k, v)| ((new, k.1), v.clone()))
            .collect();
        f.rows.extend(rows);
        let cols: Vec<_> = f
            .cols
            .iter()
            .filter(|(k, _)| k.0 == src)
            .map(|(k, v)| ((new, k.1), v.clone()))
            .collect();
        f.cols.extend(cols);
        if let Some(l) = f.sheets.get(&src).cloned() {
            f.sheets.insert(new, l);
        }
    }

    pub(crate) fn fmt_shift(
        &mut self,
        sheet: SheetId,
        rows: bool,
        at: u32,
        count: u32,
        insert: bool,
        limit: Option<(u32, u32)>,
    ) {
        if !self.cellfmt.any() {
            return;
        }
        let max = if rows { MAX_ROWS } else { MAX_COLS };

        let map = |i: u32| -> Option<u32> {
            if i < at {
                Some(i)
            } else if insert {
                (i + count < max).then_some(i + count)
            } else if i < at + count {
                None
            } else {
                Some(i - count)
            }
        };
        let along = |k: &(SheetId, u32, u32)| if rows { k.1 } else { k.2 };
        let place = |k: &(SheetId, u32, u32), i: u32| {
            if rows {
                (k.0, i, k.2)
            } else {
                (k.0, k.1, i)
            }
        };
        match limit {
            None => {
                let f = &mut self.cellfmt;
                let old = std::mem::take(&mut f.map);
                for (k, v) in old {
                    if k.0 != sheet {
                        f.map.insert(k, v);
                    } else if let Some(i) = map(along(&k)) {
                        f.map.insert(place(&k, i), v);
                    }
                }
                let lines = if rows { &mut f.rows } else { &mut f.cols };
                let old = std::mem::take(lines);
                for (k, v) in old {
                    if k.0 != sheet {
                        lines.insert(k, v);
                    } else if let Some(i) = map(k.1) {
                        lines.insert((k.0, i), v);
                    }
                }
                if insert && at > 0 {
                    let above: Vec<_> = f
                        .map
                        .iter()
                        .filter(|(k, _)| k.0 == sheet && along(k) == at - 1)
                        .map(|(k, v)| (*k, v.clone()))
                        .collect();
                    let line = (if rows { &f.rows } else { &f.cols })
                        .get(&(sheet, at - 1))
                        .cloned();
                    for j in 0..count.min(max - at) {
                        for (k, v) in &above {
                            f.map.insert(place(k, at + j), v.clone());
                        }
                        if let Some(l) = &line {
                            let lines = if rows { &mut f.rows } else { &mut f.cols };
                            lines.insert((sheet, at + j), l.clone());
                        }
                    }
                }
            }
            Some((l0, l1)) => self.fmt_shift_cells(sheet, rows, at, count, insert, (l0, l1)),
        }
    }

    fn fmt_shift_cells(
        &mut self,
        sheet: SheetId,
        rows: bool,
        at: u32,
        count: u32,
        insert: bool,
        (l0, l1): (u32, u32),
    ) {
        let max = if rows { MAX_ROWS } else { MAX_COLS };
        let cell = |i: u32, x: u32| {
            if rows {
                CellAddr::new(sheet, i, x)
            } else {
                CellAddr::new(sheet, x, i)
            }
        };

        let f = &self.cellfmt;
        let mut last = f
            .map
            .keys()
            .filter(|k| k.0 == sheet && (l0..=l1).contains(&if rows { k.2 } else { k.1 }))
            .map(|k| if rows { k.1 } else { k.2 })
            .max();
        let along_last = (if rows { &f.rows } else { &f.cols })
            .keys()
            .filter(|k| k.0 == sheet)
            .map(|k| k.1)
            .max();
        last = last.max(along_last);
        let Some(last) = last else { return };
        if last < at && !insert {
            return;
        }
        let span = (last.max(at) - at + 1) as u64 * (l1 - l0 + 1) as u64;
        if span > CELL_LIMIT {
            return;
        }

        let end = if insert {
            last.max(at)
        } else {
            (last.max(at) + count).min(max - 1)
        };

        let mut snap: FxHashMap<(u32, u32), Option<Arc<str>>> = FxHashMap::default();
        for i in at..=end {
            for x in l0..=l1 {
                snap.insert((i, x), self.fmt_at(cell(i, x)));
            }
        }
        let above: Vec<Option<Arc<str>>> = (l0..=l1)
            .map(|x| {
                (insert && at > 0)
                    .then(|| self.fmt_at(cell(at - 1, x)))
                    .flatten()
            })
            .collect();

        let hi = if insert {
            (end + count).min(max - 1)
        } else {
            end
        };
        for i in at..=hi {
            for x in l0..=l1 {
                let a = cell(i, x);
                self.cellfmt.map.remove(&(a.sheet, a.row, a.col));
            }
        }
        for i in at..=end {
            let target = if insert {
                (i + count < max).then_some(i + count)
            } else if i < at + count {
                None
            } else {
                Some(i - count)
            };
            let Some(t) = target else { continue };
            for x in l0..=l1 {
                let f = snap[&(i, x)].clone();
                self.store_format(cell(t, x), f.as_deref().unwrap_or("General"));
            }
        }
        if insert {
            for j in 0..count.min(max - at) {
                for (k, x) in (l0..=l1).enumerate() {
                    let f = above[k].clone();
                    self.store_format(cell(at + j, x), f.as_deref().unwrap_or("General"));
                }
            }
        }
    }
}
