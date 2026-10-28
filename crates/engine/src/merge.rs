use crate::address::{CellAddr, Rect, SheetId};
use crate::calc::{self, RecalcReport};
use crate::cse::{inside, paste_dest, ArrayPaste};
use crate::structure::{Axis, Shift};
use crate::workbook::{CalcMode, EditError, Op, Workbook};

pub const MERGED_CELL: &str = "We can't do that to a merged cell.";

pub(crate) fn merged_cell() -> EditError {
    EditError::Compile(MERGED_CELL.into())
}

#[derive(Default, Clone, Debug)]
pub struct Merges {
    areas: Vec<(SheetId, Rect)>,
}

pub(crate) struct PasteBlocks {
    arrays: ArrayPaste,
}

impl Workbook {
    pub fn merges(&self, sheet: SheetId) -> Vec<Rect> {
        self.merges
            .areas
            .iter()
            .filter(|(s, _)| *s == sheet)
            .map(|(_, r)| *r)
            .collect()
    }

    pub fn merge_at(&self, addr: CellAddr) -> Option<Rect> {
        self.merges
            .areas
            .iter()
            .find(|(s, r)| *s == addr.sheet && r.contains(addr.row, addr.col))
            .map(|(_, r)| *r)
    }

    pub fn merged_hidden(&self, addr: CellAddr) -> bool {
        !self.merges.areas.is_empty()
            && self
                .merge_at(addr)
                .is_some_and(|r| (r.r0, r.c0) != (addr.row, addr.col))
    }

    pub(crate) fn merge_intersects(&self, sheet: SheetId, rect: &Rect) -> bool {
        self.merges
            .areas
            .iter()
            .any(|(s, r)| *s == sheet && r.intersects(rect))
    }

    pub fn merge_cells(
        &mut self,
        sheet: SheetId,
        rect: Rect,
        across: bool,
    ) -> Result<RecalcReport, EditError> {
        if self.sheet(sheet).is_none() {
            return Err(EditError::NoSuchSheet);
        }

        let mut rect = rect;
        if across {
            loop {
                let grown = self
                    .merges(sheet)
                    .into_iter()
                    .filter(|m| m.intersects(&rect))
                    .fold(rect, |x, m| x.union_bounds(&m));
                if grown == rect {
                    break;
                }
                rect = grown;
            }
        }
        let rows: Vec<Rect> = if across && rect.rows() > 1 {
            (rect.r0..=rect.r1)
                .map(|r| Rect::new(r, rect.c0, r, rect.c1))
                .collect()
        } else {
            vec![rect]
        };

        let mut areas = Vec::new();
        let mut wiped = Vec::new();
        for mut a in rows {
            while !across {
                let grown = self
                    .merges(sheet)
                    .into_iter()
                    .filter(|m| m.intersects(&a))
                    .fold(a, |x, m| x.union_bounds(&m));
                if grown == a {
                    break;
                }
                a = grown;
            }
            if !across {
                let ms: Vec<Rect> = self
                    .merges(sheet)
                    .into_iter()
                    .filter(|m| m.intersects(&a))
                    .collect();
                let staggered = ms.iter().any(|l| {
                    l.c0 == a.c0
                        && ms
                            .iter()
                            .any(|r| r.r0 == a.r0 && r.c0 > l.c1 && r.r0 < l.r0 && l.r0 <= r.r1)
                });
                if staggered {
                    wiped.push(a);
                }
            }
            areas.push(a);
        }

        areas.retain(|a| !a.is_cell());
        if areas.is_empty() {
            return Ok(self.recalc_after_block_edit());
        }
        let blocks = self.array_blocks(sheet);
        for a in &areas {
            if blocks.iter().any(|(_, b)| b.intersects(a)) {
                return Err(crate::cse::part_of_array());
            }
            if self
                .data_tables
                .iter()
                .any(|t| t.sheet == sheet && t.interior().intersects(a))
            {
                return Err(EditError::Compile(crate::datatable::PART_ERROR.into()));
            }
            if self
                .tables
                .iter()
                .any(|t| t.sheet == sheet && t.rect.intersects(a))
            {
                return Err(EditError::Compile("0x800A03EC".into()));
            }
        }

        for a in &wiped {
            let cells: Vec<(u32, u32)> = self
                .sheet(sheet)
                .unwrap()
                .cells
                .iter_rect(*a)
                .map(|(r, c, _)| (r, c))
                .collect();
            for (r, c) in cells {
                let x = CellAddr::new(sheet, r, c);
                if self.remove_cell(x) {
                    calc::cell_changed(self, x);
                }
            }
            if a.rows() as u64 * a.cols() as u64 <= (1 << 16) {
                for r in a.r0..=a.r1 {
                    for c in a.c0..=a.c1 {
                        self.store_format(CellAddr::new(sheet, r, c), "General");
                    }
                }
            }
        }
        let hull = areas.iter().fold(areas[0], |x, a| x.union_bounds(a));
        self.merges
            .areas
            .retain(|(s, m)| !(*s == sheet && m.intersects(&hull)));
        let mut stored: Vec<(u32, u32)> = self
            .sheet(sheet)
            .unwrap()
            .cells
            .iter_rect(hull)
            .map(|(r, c, _)| (r, c))
            .collect();
        stored.sort_unstable();
        for a in areas {
            let lo = stored.partition_point(|&p| p < (a.r0, 0));
            let hi = stored.partition_point(|&p| p <= (a.r1, u32::MAX));
            let inside: Vec<(u32, u32)> = stored[lo..hi]
                .iter()
                .copied()
                .filter(|&(_, c)| c >= a.c0 && c <= a.c1)
                .collect();
            if let Some(&(r, c)) = inside.first() {
                for &(r2, c2) in &inside[1..] {
                    let x = CellAddr::new(sheet, r2, c2);
                    if self.remove_cell(x) {
                        calc::cell_changed(self, x);
                    }
                }
                if (r, c) != (a.r0, a.c0) {
                    let mode = self.settings.mode;
                    self.settings.mode = CalcMode::Manual;
                    let moved =
                        self.move_range(sheet, Rect::cell(r, c), CellAddr::new(sheet, a.r0, a.c0));
                    self.settings.mode = mode;
                    moved?;
                }
            }

            if a.rows() as u64 * a.cols() as u64 <= (1 << 16) {
                let fmt = self
                    .number_format(CellAddr::new(sheet, a.r0, a.c0))
                    .to_string();
                for r in a.r0..=a.r1 {
                    for c in a.c0..=a.c1 {
                        self.store_format(CellAddr::new(sheet, r, c), &fmt);
                    }
                }
            }
            self.merges.areas.push((sheet, a));
            calc::rect_changed(self, sheet, a);
        }
        Ok(self.recalc_after_block_edit())
    }

    pub fn unmerge_cells(&mut self, sheet: SheetId, rect: Rect) -> RecalcReport {
        let mut rect = rect;
        loop {
            let grown = self
                .merges(sheet)
                .into_iter()
                .filter(|m| m.intersects(&rect))
                .fold(rect, |x, m| x.union_bounds(&m));
            if grown == rect {
                break;
            }
            rect = grown;
        }
        let gone: Vec<Rect> = self
            .merges(sheet)
            .into_iter()
            .filter(|m| m.intersects(&rect))
            .collect();
        self.merges
            .areas
            .retain(|(s, m)| !(*s == sheet && m.intersects(&rect)));
        for m in gone {
            calc::rect_changed(self, sheet, m);
        }
        self.recalc_after_block_edit()
    }

    fn recalc_after_block_edit(&mut self) -> RecalcReport {
        if self.settings.mode == CalcMode::Automatic {
            calc::recalc(self)
        } else {
            RecalcReport::default()
        }
    }

    pub(crate) fn copy_merges(&mut self, from: SheetId, to: SheetId) {
        let add: Vec<_> = self
            .merges
            .areas
            .iter()
            .filter(|(s, _)| *s == from)
            .map(|(_, m)| (to, *m))
            .collect();
        self.merges.areas.extend(add);
    }

    pub(crate) fn guard_merges_rect(&self, sheet: SheetId, rect: Rect) -> Result<(), EditError> {
        if self
            .merges(sheet)
            .iter()
            .any(|m| m.intersects(&rect) && !inside(m, &rect))
        {
            return Err(merged_cell());
        }
        Ok(())
    }

    pub(crate) fn guard_merge_array(&self, sheet: SheetId, rect: Rect) -> Result<(), EditError> {
        if !rect.is_cell() && self.merge_intersects(sheet, &rect) {
            return Err(merged_cell());
        }
        Ok(())
    }

    pub(crate) fn merges_in_shift(&self, s: &Shift) -> bool {
        self.merges.areas.iter().any(|&(sheet, m)| {
            let (q, u, v) = match s.axis {
                Axis::Rows => (m.r1, m.c0, m.c1),
                Axis::Cols => (m.c1, m.r0, m.r1),
            };
            let (l0, l1) = s.limit.unwrap_or((0, u32::MAX));
            sheet == s.sheet && !(v < l0 || u > l1 || q < s.at)
        })
    }

    pub(crate) fn paste_blocks_plan(
        &mut self,
        ss: SheetId,
        src: Rect,
        dst: CellAddr,
    ) -> Result<PasteBlocks, EditError> {
        let dest = paste_dest(src, dst);
        let ds = dst.sheet;
        let arrays = self.paste_array_plan(ss, src, dst)?;
        self.remove_arrays_in(ds, dest);
        Ok(PasteBlocks { arrays })
    }

    pub fn copy_to_range(
        &mut self,
        ss: SheetId,
        src: Rect,
        ds: SheetId,
        dr: Rect,
    ) -> Result<RecalcReport, EditError> {
        self.copy_to_range_body(ss, src, ds, dr)
    }

    fn copy_to_range_body(
        &mut self,
        ss: SheetId,
        src: Rect,
        ds: SheetId,
        dr: Rect,
    ) -> Result<RecalcReport, EditError> {
        let keep = Rect::new(
            dr.r0,
            dr.c0,
            dr.r1.max(dr.r0 + src.rows() - 1),
            dr.c1.max(dr.c0 + src.cols() - 1),
        );
        self.guard_filtered(ss, src, true, true)?;
        self.guard_filtered(ds, keep, true, true)?;
        if self.merge_intersects(ss, &src) || self.merge_intersects(ds, &keep) {
            return Err(EditError::OutOfScope("copy over merged cells"));
        }
        let pending = self.totals_pending(ds, keep);
        let r = self.with_paste_block((ds, keep), |this| this.copy_to_range_raw(ss, src, ds, dr));
        if pending {
            self.restore_totals(ds, keep, Some(keep));
        }
        r
    }

    fn copy_to_range_raw(
        &mut self,
        ss: SheetId,
        src: Rect,
        ds: SheetId,
        dr: Rect,
    ) -> Result<RecalcReport, EditError> {
        let (nr, nc) = if dr.rows() % src.rows() == 0 && dr.cols() % src.cols() == 0 {
            (dr.rows() / src.rows(), dr.cols() / src.cols())
        } else {
            (1, 1)
        };
        if nr * nc > 1 {
            let whole = Rect::new(
                dr.r0,
                dr.c0,
                dr.r0 + nr * src.rows() - 1,
                dr.c0 + nc * src.cols() - 1,
            );
            self.guard_arrays_rect(ds, whole)?;
            if ss == ds && whole.intersects(&src) {
                self.guard_arrays_rect(ss, src)?;
            }
            self.remove_arrays_in(ds, whole);
        }
        let mut report = RecalcReport::default();
        for ti in 0..nr {
            for tj in 0..nc {
                let to = CellAddr::new(ds, dr.r0 + ti * src.rows(), dr.c0 + tj * src.cols());
                let r = self.with_op(Op::DeferPasteRecalc, ti + 1 < nr || tj + 1 < nc, |this| {
                    this.copy_range(ss, src, to)
                });
                let r = r?;
                report.changed.extend(r.changed);
                report.evaluations += r.evaluations;
            }
        }
        Ok(report)
    }

    pub(crate) fn paste_blocks(&mut self, plan: PasteBlocks) {
        self.paste_arrays(plan.arrays);
    }
}
