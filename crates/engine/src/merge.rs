use crate::address::{CellAddr, Rect, SheetId};
use crate::calc::{self, RecalcReport};
use crate::cse::{inside, paste_dest, ArrayPaste};
use crate::structure::{Axis, Shift};
use crate::workbook::{CalcMode, EditError, Workbook};

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
    dest_sheet: SheetId,
    dest: Rect,

    merges: Vec<Rect>,
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

    pub(crate) fn unmerge_for_table(&mut self, sheet: SheetId, rect: Rect) {
        self.merges
            .areas
            .retain(|(s, m)| !(*s == sheet && m.intersects(&rect)));
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

    pub(crate) fn guard_merges_touch(&self, sheet: SheetId, rect: Rect) -> Result<(), EditError> {
        if self.merge_intersects(sheet, &rect) {
            return Err(EditError::Compile(
                "To do this, all the merged cells need to be the same size.".into(),
            ));
        }
        Ok(())
    }

    pub(crate) fn guard_paste_merges(
        &self,
        sheet: SheetId,
        target: Rect,
        dst: Rect,
        keep_one: bool,
        spec: &crate::pastespecial::PasteSpecial,
    ) -> Result<(Vec<Rect>, Vec<Rect>), EditError> {
        use crate::pastespecial::PasteWhat;
        if spec.what == PasteWhat::Values {
            self.guard_merges_touch(sheet, target)?;
            return Ok((vec![], vec![]));
        }
        if keep_one {
            return Ok((vec![], vec![]));
        }
        let region = if spec.what == PasteWhat::Formats && (dst.rows() > 1 || dst.cols() > 1) {
            dst
        } else {
            Rect::new(
                target.r0.min(dst.r0),
                target.c0.min(dst.c0),
                target.r1.max(dst.r1),
                target.c1.max(dst.c1),
            )
        };
        let hit: Vec<Rect> = self
            .merges(sheet)
            .into_iter()
            .filter(|m| m.intersects(&region))
            .collect();
        if hit.iter().any(|m| !inside(m, &region)) {
            return Err(merged_cell());
        }
        Ok(if spec.what == PasteWhat::Formulas {
            (vec![], hit)
        } else {
            (hit, vec![])
        })
    }

    pub(crate) fn guard_merge_array(&self, sheet: SheetId, rect: Rect) -> Result<(), EditError> {
        if !rect.is_cell() && self.merge_intersects(sheet, &rect) {
            return Err(merged_cell());
        }
        Ok(())
    }

    pub(crate) fn guard_merges_sort(&self, sheet: SheetId, rect: Rect) -> Result<(), EditError> {
        let ms: Vec<Rect> = self
            .merges(sheet)
            .into_iter()
            .filter(|m| m.intersects(&rect))
            .filter(|m| rect.rows() > 1 || inside(m, &rect))
            .collect();
        if ms.is_empty() {
            return Ok(());
        }
        let same = ms
            .iter()
            .all(|m| inside(m, &rect) && m.rows() == ms[0].rows() && m.cols() == ms[0].cols());
        let covered: u64 = ms.iter().map(|m| m.cell_count()).sum();
        if !same || covered != rect.cell_count() {
            return Err(EditError::Compile(
                "To do this, all the merged cells need to be the same size.".into(),
            ));
        }
        Ok(())
    }

    pub(crate) fn shift_merges(&mut self, s: &Shift) {
        if self.merges.areas.is_empty() {
            return;
        }
        let mut out = Vec::new();
        for &(sheet, m) in &self.merges.areas {
            if sheet != s.sheet {
                out.push((sheet, m));
                continue;
            }
            let (p, q, u, v) = match s.axis {
                Axis::Rows => (m.r0, m.r1, m.c0, m.c1),
                Axis::Cols => (m.c0, m.c1, m.r0, m.r1),
            };
            let (l0, l1) = s.limit.unwrap_or((0, u32::MAX));
            if v < l0 || u > l1 || q < s.at {
                out.push((sheet, m));
                continue;
            }
            if !(l0 <= u && v <= l1) {
                continue;
            }
            let span = if s.insert {
                let n = |i: u32| if i >= s.at { i + s.count } else { i };

                Some((if p >= s.at { n(p) } else { p }, n(q)))
            } else {
                let end = s.at + s.count - 1;
                let keep: Vec<u32> = (p..=q).filter(|&i| i < s.at || i > end).collect();
                keep.first().map(|&a| {
                    let f = |i: u32| if i > end { i - s.count } else { i };
                    (f(a), f(*keep.last().unwrap()))
                })
            };
            let Some((np, nq)) = span else { continue };
            let max = match s.axis {
                Axis::Rows => crate::address::MAX_ROWS,
                Axis::Cols => crate::address::MAX_COLS,
            };
            if nq >= max {
                continue;
            }
            let r = match s.axis {
                Axis::Rows => Rect::new(np, m.c0, nq, m.c1),
                Axis::Cols => Rect::new(m.r0, np, m.r1, nq),
            };
            if !r.is_cell() {
                out.push((sheet, r));
            }
        }

        let unmerged = out.len() < self.merges.areas.len();
        self.merges.areas = out;
        if unmerged {
            self.extra_recalc();
        }
    }

    pub(crate) fn unmerge_cut_sideways(&mut self, s: &Shift) {
        let Some((l0, l1)) = s.limit else { return };
        self.merges.areas.retain(|&(sheet, m)| {
            let (q, u, v) = match s.axis {
                Axis::Rows => (m.r1, m.c0, m.c1),
                Axis::Cols => (m.c1, m.r0, m.r1),
            };
            sheet != s.sheet || v < l0 || u > l1 || q < s.at || (l0 <= u && v <= l1)
        });
    }

    pub(crate) fn move_merges(
        &mut self,
        ss: SheetId,
        src: Rect,
        ds: SheetId,
        dest: Rect,
    ) -> Result<(), EditError> {
        if self.merges.areas.is_empty() {
            return Ok(());
        }
        self.guard_merges_rect(ss, src)?;
        let (dr, dc) = (
            dest.r0 as i64 - src.r0 as i64,
            dest.c0 as i64 - src.c0 as i64,
        );
        let mut out = Vec::new();
        let mut moved = Vec::new();
        for &(sheet, m) in &self.merges.areas {
            if sheet == ss && inside(&m, &src) {
                moved.push((ds, offset(&m, dr, dc)));
            } else if sheet == ds && m.intersects(&dest) {
            } else {
                out.push((sheet, m));
            }
        }
        out.extend(moved);
        self.merges.areas = out;
        Ok(())
    }

    pub(crate) fn paste_blocks_plan(
        &mut self,
        ss: SheetId,
        src: Rect,
        dst: CellAddr,
    ) -> Result<PasteBlocks, EditError> {
        let dest = paste_dest(src, dst);
        let ds = dst.sheet;
        self.guard_merges_rect(ds, dest)?;
        let arrays = self.paste_array_plan(ss, src, dst)?;
        let (dr, dc) = (
            dst.row as i64 - src.r0 as i64,
            dst.col as i64 - src.c0 as i64,
        );
        let merges: Vec<Rect> = self
            .merges(ss)
            .into_iter()
            .filter(|m| src.contains(m.r0, m.c0))
            .map(|m| Rect::new(m.r0, m.c0, m.r1.min(src.r1), m.c1.min(src.c1)))
            .filter(|m| !m.is_cell())
            .map(|m| offset(&m, dr, dc))
            .collect();
        self.remove_arrays_in(ds, dest);
        self.merges
            .areas
            .retain(|(s, m)| !(*s == ds && m.intersects(&dest)));
        Ok(PasteBlocks {
            arrays,
            dest_sheet: ds,
            dest,
            merges,
        })
    }

    pub fn copy_to_range(
        &mut self,
        ss: SheetId,
        src: Rect,
        ds: SheetId,
        dr: Rect,
    ) -> Result<RecalcReport, EditError> {
        let r = self.copy_to_range_body(ss, src, ds, dr);
        if r.is_ok() {
            self.view_touch_paste(ds, src, dr);
        }
        r
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
        let pending = self.totals_pending(ss, src) || self.totals_pending(ds, keep);
        let outer = self.calc.paste_block.replace((ds, keep));
        let r = self.copy_to_range_raw(ss, src, ds, dr);
        self.select_paste(ds, dr.r0, dr.c0);
        self.calc.paste_block = outer;
        if pending {
            self.restore_totals(ss, src, None);
            self.restore_totals(ds, keep, Some(keep));
        }

        if r.is_ok() && self.tables.iter().any(|t| t.sheet == ds) {
            self.extra_recalc();
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
        if let Some(plan) = self.view_copy_plan(ss, src, ds, dr) {
            let (steps, whole, from_filtered) = plan?;
            self.guard_merges_rect(ds, whole)?;
            self.guard_arrays_rect(ds, whole)?;
            self.remove_arrays_in(ds, whole);
            let mut report = RecalcReport::default();
            let last = steps.len();
            for (i, (run, to)) in steps.into_iter().enumerate() {
                self.defer_paste_recalc = i + 1 < last;
                let r = self.copy_range(ss, run, to);
                self.defer_paste_recalc = false;
                let r = r?;
                report.changed.extend(r.changed);
                report.evaluations += r.evaluations;

                if from_filtered {
                    let d = Rect::new(
                        to.row,
                        to.col,
                        to.row + run.rows() - 1,
                        to.col + run.cols() - 1,
                    );
                    let spec = crate::pastespecial::PasteSpecial {
                        what: crate::pastespecial::PasteWhat::Values,
                        ..Default::default()
                    };
                    self.defer_paste_recalc = i + 1 < last;
                    let r = self.paste_special_one(ss, run, ds, d, &spec);
                    self.defer_paste_recalc = false;
                    r?;
                }
            }
            return Ok(report);
        }
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
            self.guard_merges_rect(ds, whole)?;
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
                self.defer_paste_recalc = ti + 1 < nr || tj + 1 < nc;
                let r = self.copy_range(ss, src, to);
                self.defer_paste_recalc = false;
                let r = r?;
                report.changed.extend(r.changed);
                report.evaluations += r.evaluations;
            }
        }
        Ok(report)
    }

    pub(crate) fn suspend_merges(&mut self, sheet: SheetId, ms: &[Rect], off: bool) {
        if off {
            self.merges
                .areas
                .retain(|(s, x)| !(*s == sheet && ms.contains(x)));
        } else {
            self.merges.areas.extend(ms.iter().map(|m| (sheet, *m)));
        }
    }

    pub(crate) fn remove_merge(&mut self, sheet: SheetId, r: Rect) {
        self.merges.areas.retain(|(s, x)| !(*s == sheet && *x == r));
        calc::rect_changed(self, sheet, r);
    }

    pub(crate) fn put_merge(&mut self, sheet: SheetId, r: Rect) {
        self.merges
            .areas
            .retain(|(s, x)| !(*s == sheet && x.intersects(&r)));
        self.merges.areas.push((sheet, r));
        calc::rect_changed(self, sheet, r);
    }

    pub(crate) fn paste_blocks(&mut self, plan: PasteBlocks) {
        self.paste_arrays(plan.arrays);
        for m in plan.merges {
            debug_assert!(inside(&m, &plan.dest));
            self.merges.areas.push((plan.dest_sheet, m));
            calc::rect_changed(self, plan.dest_sheet, m);
        }
    }
}

fn offset(r: &Rect, dr: i64, dc: i64) -> Rect {
    Rect::new(
        (r.r0 as i64 + dr) as u32,
        (r.c0 as i64 + dc) as u32,
        (r.r1 as i64 + dr) as u32,
        (r.c1 as i64 + dc) as u32,
    )
}
