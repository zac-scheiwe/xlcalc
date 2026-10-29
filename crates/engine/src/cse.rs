use std::sync::Arc;

use crate::address::{CellAddr, Rect, SheetId};
use crate::calc::{self, RecalcReport};
use crate::value::{Array, Value};
use crate::workbook::{EditError, Entry, FormulaId, Op, Workbook};

pub const PART_OF_ARRAY: &str = "You can't change part of an array.";

pub(crate) fn part_of_array() -> EditError {
    EditError::Compile(PART_OF_ARRAY.into())
}

pub fn fit(value: Value, rows: u32, cols: u32) -> Value {
    let a = match value {
        Value::Array(a) => a,
        v => {
            if rows == 1 && cols == 1 {
                return v;
            }
            return Array::filled(rows, cols, v).into_value();
        }
    };
    if rows == 1 && cols == 1 {
        return a.data[0].clone();
    }
    if a.rows == rows && a.cols == cols {
        return Value::Array(a);
    }
    Array::from_fn(rows, cols, |r, c| a.broadcast_get(r, c)).into_value()
}

impl Workbook {
    pub fn array_block(&self, fid: FormulaId) -> Option<Rect> {
        let f = self.formula(fid);
        let (rows, cols) = f.array?;
        Some(Rect::new(
            f.addr.row,
            f.addr.col,
            f.addr.row + rows - 1,
            f.addr.col + cols - 1,
        ))
    }

    pub fn array_at(&self, addr: CellAddr) -> Option<(FormulaId, Rect)> {
        let sh = self.sheet(addr.sheet)?;
        let fid = match sh.cells.get(addr.row, addr.col) {
            Some(Entry::Formula(f)) => *f,
            Some(_) => return None,
            None => sh.spills.owner_of(addr.row, addr.col)?,
        };
        let block = self.array_block(fid)?;
        block.contains(addr.row, addr.col).then_some((fid, block))
    }

    fn copied_formula(&self, fid: FormulaId, to: CellAddr) -> Arc<crate::formula::Formula> {
        let f = &self.formula(fid).formula;
        let root = crate::structure::map_refs(&f.root, &mut |_, cref| {
            Some(
                crate::formula_print::cref_text(cref, to)
                    .map(|_| crate::structure::normalize_cref(cref, to)),
            )
        });
        Arc::new(crate::formula::Formula {
            root,
            volatile: f.volatile,
        })
    }

    pub fn current_array(&self, addr: CellAddr) -> Option<Rect> {
        if let Some(t) = self.data_tables.iter().find(|t| t.contains_interior(addr)) {
            return Some(t.rect);
        }
        let (_, b) = self.array_at(addr)?;
        if b.is_cell() {
            if let Some(m) = self.merge_at(addr) {
                return Some(m);
            }
        }
        Some(b)
    }

    pub(crate) fn host_block(&self, host: CellAddr) -> Option<Rect> {
        let fid = self.formula_at(host)?;
        self.array_block(fid).filter(|b| !b.is_cell())
    }

    pub fn in_multi_cell_array(&self, addr: CellAddr) -> bool {
        self.array_at(addr).is_some_and(|(_, b)| !b.is_cell())
    }

    pub(crate) fn has_arrays(&self) -> bool {
        self.arrays_seen
            && self
                .formulas
                .iter()
                .flatten()
                .any(|f| f.array.is_some_and(|(r, c)| r > 1 || c > 1))
    }

    pub fn array_blocks(&self, sheet: SheetId) -> Vec<(FormulaId, Rect)> {
        self.formula_ids()
            .filter(|&f| self.formula(f).addr.sheet == sheet)
            .filter_map(|f| self.array_block(f).map(|b| (f, b)))
            .filter(|(_, b)| !b.is_cell())
            .collect()
    }

    pub(crate) fn guard_arrays_rect(&self, sheet: SheetId, rect: Rect) -> Result<(), EditError> {
        if !self.has_arrays() {
            return Ok(());
        }
        for (_, b) in self.array_blocks(sheet) {
            if b.intersects(&rect) && b.intersection(&rect) != Some(b) {
                return Err(part_of_array());
            }
        }
        Ok(())
    }

    pub fn set_array_formula(
        &mut self,
        sheet: SheetId,
        rect: Rect,
        text: &str,
    ) -> Result<RecalcReport, EditError> {
        if self.sheet(sheet).is_none() {
            return Err(EditError::NoSuchSheet);
        }

        let rect = if rect.is_cell() {
            self.array_at(CellAddr::new(sheet, rect.r0, rect.c0))
                .map(|(_, b)| b)
                .filter(|b| !b.is_cell())
                .unwrap_or(rect)
        } else {
            rect
        };

        if self.guard_arrays_rect(sheet, rect).is_err() {
            return Err(EditError::Compile(
                "Unable to set the FormulaArray property of the Range class".into(),
            ));
        }
        self.guard_merge_array(sheet, rect)?;
        let anchor = CellAddr::new(sheet, rect.r0, rect.c0);
        if !rect.is_cell()
            && self
                .tables
                .iter()
                .any(|t| t.sheet == sheet && t.rect.intersects(&rect))
        {
            return Err(EditError::Compile(
                "Multi-cell array formulas aren't allowed in tables".into(),
            ));
        }
        if rect.rows() as u64 * rect.cols() as u64 > crate::workbook::MAX_ARRAY_CELLS {
            return Err(EditError::Compile(
                "Unable to set the FormulaArray property of the Range class".into(),
            ));
        }
        let body = text.strip_prefix('=').unwrap_or(text);
        let (formula, body) = self.compile_normalized(body, anchor)?;
        let formula = Arc::new(formula);

        if self.data_tables_overwritten(sheet, rect).is_err() {
            return Err(EditError::Compile(
                "Unable to set the FormulaArray property of the Range class".into(),
            ));
        }

        for (r, c) in self.cells_to_visit(sheet, rect) {
            let a = CellAddr::new(sheet, r, c);
            if self.remove_cell(a) {
                calc::cell_changed(self, a);
            }
        }
        self.set_compiled_formula(anchor, formula, body);
        let fid = self.formula_at(anchor).unwrap();
        self.formula_mut(fid).array = Some((rect.rows(), rect.cols()));
        self.arrays_seen |= !rect.is_cell();

        self.formula_mut(fid).value = fit(Value::Number(0.0), rect.rows(), rect.cols());
        if !rect.is_cell() {
            self.sheet_mut(sheet).unwrap().spills.set_active(fid, rect);
            self.formula_mut(fid).spill = Some(rect);
            calc::rect_changed(self, sheet, rect);
        }

        let mut table_edit = false;
        if rect.is_cell() && !self.tables.is_empty() {
            let grows = self
                .table_expansion(anchor, true)
                .is_some_and(|(_, _, col)| col);
            self.table_auto_expand(anchor, true);
            self.header_edited(anchor);
            self.table_formula_under(anchor);
            if grows {
                self.formula_mut(fid).array = None;
            }
            table_edit = true;
        }
        let mut report = if self.settings.mode == crate::workbook::CalcMode::Automatic {
            calc::recalc(self)
        } else {
            calc::recalc_only(self, &[anchor])
        };
        report.changed.insert(0, anchor);
        if table_edit {
            self.table_calc_fill(anchor);
        }
        Ok(calc::finish_report(report))
    }

    pub(crate) fn reserve_array_blocks(&mut self) {
        if !self.has_arrays() {
            return;
        }
        let ids: Vec<FormulaId> = self.formula_ids().collect();
        for fid in ids {
            if let Some(b) = self.array_block(fid).filter(|b| !b.is_cell()) {
                let sheet = self.formula(fid).addr.sheet;
                if let Some(sh) = self.sheet_mut(sheet) {
                    sh.spills.set_active(fid, b);
                }
                let f = self.formula_mut(fid);
                f.spill = Some(b);
                if !matches!(f.value, Value::Array(_)) {
                    f.value = fit(f.value.clone(), b.rows(), b.cols());
                }
            }
        }
    }

    pub fn clear_range(&mut self, sheet: SheetId, rect: Rect) -> Result<RecalcReport, EditError> {
        self.guard_filtered(sheet, rect, true, true)?;
        let pending = self.totals_pending(sheet, rect);
        let r = self.clear_range_raw(sheet, rect);
        if pending {
            self.restore_totals(sheet, rect, Some(rect));
        }
        r
    }

    fn clear_range_raw(&mut self, sheet: SheetId, rect: Rect) -> Result<RecalcReport, EditError> {
        let rect = Rect::new(
            rect.r0,
            rect.c0,
            rect.r1.min(crate::address::MAX_ROWS - 1),
            rect.c1.min(crate::address::MAX_COLS - 1),
        );
        if rect.r0 > rect.r1 || rect.c0 > rect.c1 {
            return Ok(RecalcReport::default());
        }
        self.guard_arrays_rect(sheet, rect)?;
        self.guard_merges_rect(sheet, rect)?;

        self.data_tables_overwritten(sheet, rect)?;
        let mut report = RecalcReport::default();

        self.remove_arrays_in(sheet, rect);
        let visit = self.cells_to_visit(sheet, rect);

        if self.settings.mode == crate::workbook::CalcMode::Automatic && visit.len() > 512 {
            let mut edited = Vec::new();
            for (r, c) in visit {
                let a = CellAddr::new(sheet, r, c);
                if self.clear_core(a) {
                    edited.push(a);
                }
            }
            if !edited.is_empty() {
                let r = self.after_edit(edited);
                report.changed.extend(r.changed);
                report.evaluations += r.evaluations;
            }
            return Ok(report);
        }
        let n = visit.len();
        for (i, (r, c)) in visit.into_iter().enumerate() {
            let r = self.with_op(Op::HoldIter, i + 1 < n, |this| {
                this.clear(CellAddr::new(sheet, r, c))
            });
            report.changed.extend(r.changed);
            report.evaluations += r.evaluations;
        }
        Ok(report)
    }

    fn cells_to_visit(&self, sheet: SheetId, rect: Rect) -> Vec<(u32, u32)> {
        if rect.rows() as u64 * rect.cols() as u64 <= 1 << 20 {
            let mut v = Vec::with_capacity((rect.rows() * rect.cols()) as usize);
            for r in rect.r0..=rect.r1 {
                for c in rect.c0..=rect.c1 {
                    v.push((r, c));
                }
            }
            return v;
        }
        let mut v: Vec<(u32, u32)> = self
            .sheet(sheet)
            .map(|sh| sh.cells.iter_rect(rect).map(|(r, c, _)| (r, c)).collect())
            .unwrap_or_default();
        v.sort_unstable();
        v
    }

    pub(crate) fn guard_arrays_shift(&self, s: &crate::structure::Shift) -> Result<(), EditError> {
        if !self.has_arrays() {
            return Ok(());
        }
        for (_, b) in self.array_blocks(s.sheet) {
            if !shift_keeps_block(s, &b) {
                return Err(part_of_array());
            }
        }
        Ok(())
    }

    pub(crate) fn guard_arrays_move(
        &self,
        ss: SheetId,
        src: Rect,
        ds: SheetId,
        dest: Rect,
    ) -> Result<(), EditError> {
        if !self.has_arrays() {
            return Ok(());
        }
        self.guard_arrays_rect(ss, src)?;
        for (_, b) in self.array_blocks(ds) {
            let moving = ds == ss && inside(&b, &src);
            if !moving && b.intersects(&dest) && !inside(&b, &dest) {
                return Err(part_of_array());
            }
        }
        Ok(())
    }

    pub(crate) fn paste_array_plan(
        &self,
        ss: SheetId,
        src: Rect,
        dst: CellAddr,
    ) -> Result<ArrayPaste, EditError> {
        if !self.has_arrays() {
            return Ok(vec![]);
        }
        let dest = paste_dest(src, dst);
        let ds = dst.sheet;
        for (_, b) in self.array_blocks(ds) {
            if b.intersects(&dest) && !inside(&b, &dest) {
                return Err(part_of_array());
            }
            if ss == ds && b.intersects(&dest) && b.intersects(&src) && !inside(&b, &src) {
                return Err(part_of_array());
            }
        }
        self.array_copies(ss, src, dst)
    }

    fn array_copies(&self, ss: SheetId, src: Rect, dst: CellAddr) -> Result<ArrayPaste, EditError> {
        let ds = dst.sheet;
        let (dr, dc) = (
            dst.row as i64 - src.r0 as i64,
            dst.col as i64 - src.c0 as i64,
        );
        let mut plan = Vec::new();
        for (fid, b) in self.array_blocks(ss) {
            if let Some(i) = b.intersection(&src) {
                let at = CellAddr::new(ds, (i.r0 as i64 + dr) as u32, (i.c0 as i64 + dc) as u32);
                plan.push((at, self.copied_formula(fid, at), (i.rows(), i.cols())));
            }
        }

        for fid in self.formula_ids() {
            let f = self.formula(fid);
            if f.array == Some((1, 1)) && f.addr.sheet == ss && src.contains(f.addr.row, f.addr.col)
            {
                let at = CellAddr::new(
                    ds,
                    (f.addr.row as i64 + dr) as u32,
                    (f.addr.col as i64 + dc) as u32,
                );
                plan.push((at, self.copied_formula(fid, at), (1, 1)));
            }
        }
        Ok(plan)
    }

    pub(crate) fn remove_arrays_in(&mut self, sheet: SheetId, dest: Rect) {
        if !self.has_arrays() {
            return;
        }
        for (fid, b) in self.array_blocks(sheet) {
            if inside(&b, &dest) {
                let a = self.formula(fid).addr;
                if self.remove_cell(a) {
                    crate::calc::cell_changed(self, a);
                }
            }
        }
    }

    pub(crate) fn paste_arrays(&mut self, plan: ArrayPaste) {
        for (at, formula, (rows, cols)) in plan {
            for r in at.row..at.row + rows {
                for c in at.col..at.col + cols {
                    let a = CellAddr::new(at.sheet, r, c);
                    if self.remove_cell(a) {
                        crate::calc::cell_changed(self, a);
                    }
                }
            }
            let text = crate::formula_print::print_node(self, &formula.root, at);
            self.set_compiled_formula(at, formula.clone(), text);
            let nf = self.formula_at(at).unwrap();
            self.formula_mut(nf).array = Some((rows, cols));
            self.arrays_seen |= rows * cols > 1;
            self.formula_mut(nf).value = fit(Value::Number(0.0), rows, cols);
            if rows * cols > 1 {
                let block = Rect::new(at.row, at.col, at.row + rows - 1, at.col + cols - 1);
                self.sheet_mut(at.sheet)
                    .unwrap()
                    .spills
                    .set_active(nf, block);
                self.formula_mut(nf).spill = Some(block);
                crate::calc::rect_changed(self, at.sheet, block);
            }
        }
    }

    pub(crate) fn paste_special_arrays(
        &mut self,
        ss: SheetId,
        src: Rect,
        ds: SheetId,
        target: Rect,
        spec: &crate::pastespecial::PasteSpecial,
    ) -> Result<ArrayPaste, EditError> {
        use crate::pastespecial::{PasteOp, PasteWhat};
        if !self.has_arrays() {
            return Ok(vec![]);
        }
        for (_, b) in self.array_blocks(ds) {
            if b.intersects(&target) && (!inside(&b, &target) || spec.op != PasteOp::None) {
                return Err(part_of_array());
            }
        }
        let plain = spec.op == PasteOp::None && spec.what != PasteWhat::Values && !spec.transpose;

        let mut plan = vec![];
        if plain
            && ss == ds
            && target.intersects(&src)
            && self
                .array_blocks(ss)
                .iter()
                .any(|(_, b)| b.intersects(&target) && b.intersects(&src) && !inside(b, &src))
        {
            return Err(part_of_array());
        }
        if plain
            && target.rows().is_multiple_of(src.rows())
            && target.cols().is_multiple_of(src.cols())
        {
            for tr in 0..target.rows() / src.rows() {
                for tc in 0..target.cols() / src.cols() {
                    let at =
                        CellAddr::new(ds, target.r0 + tr * src.rows(), target.c0 + tc * src.cols());
                    plan.extend(self.array_copies(ss, src, at)?);
                }
            }
        }
        self.remove_arrays_in(ds, target);
        Ok(plan)
    }
}

pub(crate) fn settle_array(
    wb: &mut Workbook,
    fid: FormulaId,
    first: Value,
    report: &mut RecalcReport,
) -> Option<Value> {
    let (rows, cols) = wb.formula(fid).array?;
    let (saved, saved_spill) = {
        let f = wb.formula(fid);
        (f.value.clone(), f.spill)
    };
    let block = wb.array_block(fid);
    let mut out = None;
    if reads_own_cell(wb, fid) {
        let f = wb.formula_mut(fid);
        f.value = saved;
        f.spill = saved_spill;
        return None;
    }

    for seed in [fit(first, rows, cols), fit(Value::Number(1.0), rows, cols)] {
        let mut prev = seed;
        let mut res = None;
        for _ in 0..rows * cols + 1 {
            {
                let f = wb.formula_mut(fid);
                f.value = prev.clone();
                f.spill = block.filter(|b| !b.is_cell());
            }
            let (v, interrupt) = {
                let f = wb.formula(fid);
                let formula = f.formula.clone();
                let mut ctx = crate::eval::EvalCtx::new(wb, f.addr);
                ctx.own = Some(fid);
                let v = crate::eval::eval_formula(&mut ctx, &formula.root);
                (v, ctx.interrupt)
            };
            report.evaluations += 1;
            if interrupt.is_some() {
                break;
            }
            let next = fit(v.clone(), rows, cols);
            if next.same_as(&prev) {
                res = Some((v, next));
                break;
            }
            prev = next;
        }
        match (&out, res) {
            (_, None) => {
                out = None;
                break;
            }
            (None, Some(r)) => out = Some(r),
            (Some((_, a)), Some((_, b))) => {
                if !a.same_as(&b) {
                    out = None;
                    break;
                }
            }
        }
    }
    let f = wb.formula_mut(fid);
    f.value = saved;
    f.spill = saved_spill;
    out.map(|(v, _)| v)
}

pub(crate) type ArrayPaste = Vec<(CellAddr, Arc<crate::formula::Formula>, (u32, u32))>;

pub(crate) fn paste_dest(src: Rect, dst: CellAddr) -> Rect {
    Rect::new(
        dst.row,
        dst.col,
        dst.row + src.rows() - 1,
        dst.col + src.cols() - 1,
    )
}

pub(crate) fn inside(a: &Rect, b: &Rect) -> bool {
    b.r0 <= a.r0 && a.r1 <= b.r1 && b.c0 <= a.c0 && a.c1 <= b.c1
}

pub(crate) fn shift_keeps_block(s: &crate::structure::Shift, b: &Rect) -> bool {
    use crate::structure::Axis;
    let (p, q, u, v) = match s.axis {
        Axis::Rows => (b.r0, b.r1, b.c0, b.c1),
        Axis::Cols => (b.c0, b.c1, b.r0, b.r1),
    };
    let (l0, l1) = s.limit.unwrap_or((0, u32::MAX));
    if v < l0 || u > l1 || q < s.at {
        return true;
    }
    let across = l0 <= u && v <= l1;
    if s.insert {
        p >= s.at && across
    } else {
        let end = s.at + s.count - 1;
        across && (p > end || (s.at <= p && q <= end))
    }
}

pub(crate) fn reads_own_cell(wb: &Workbook, fid: FormulaId) -> bool {
    use crate::formula::{CRef, Node, SheetRef};
    let Some(block) = wb.array_block(fid) else {
        return false;
    };
    let host = wb.formula(fid).addr;
    let mut hit = false;
    crate::formula::walk(&wb.formula(fid).formula.root, &mut |n| {
        if let Node::Ref(sr, CRef::Cell { row, col }) = n {
            if matches!(sr, SheetRef::Current) || *sr == SheetRef::Sheet(host.sheet) {
                if let (Some(r), Some(c)) = (
                    row.resolve(host.row, 1 << 20, false),
                    col.resolve(host.col, 1 << 14, false),
                ) {
                    hit |= block.contains(r, c);
                }
            }
        }
    });
    hit
}
