use std::sync::Arc;

use crate::address::{CellAddr, Rect, SheetId, MAX_COLS, MAX_ROWS};
use crate::calc::{self, RecalcReport};
use crate::datatable::{is_table_formula, DataTable, PART_ERROR};
use crate::formula::{CRef, Formula, LambdaDef, Node, Rc, SheetRef};
use crate::formula_print::{cref_text, print_node};
use crate::graph::DepGraph;
use crate::value::Value;
use crate::workbook::{EditError, Entry, FormulaId, Workbook};

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum Axis {
    Rows,

    Cols,
}

#[derive(Clone, Copy, Debug)]
pub struct Shift {
    pub sheet: SheetId,

    pub axis: Axis,

    pub at: u32,

    pub count: u32,

    pub insert: bool,

    pub limit: Option<(u32, u32)>,

    pub cells: bool,
}

pub(crate) const MAX_PASTE_CELLS: u64 = 1 << 23;

pub(crate) const MAX_RANGE_OP_CELLS: u64 = 1 << 25;

pub(crate) fn range_too_large(r: &Rect) -> Result<(), EditError> {
    if r.rows() as u64 * r.cols() as u64 > MAX_RANGE_OP_CELLS {
        Err(EditError::Compile("The range is too large.".into()))
    } else {
        Ok(())
    }
}

impl Shift {
    fn max(&self) -> u32 {
        match self.axis {
            Axis::Rows => MAX_ROWS,
            Axis::Cols => MAX_COLS,
        }
    }

    pub fn map_index(&self, i: u32) -> Option<u32> {
        if self.insert {
            if i >= self.at {
                let n = i as u64 + self.count as u64;
                (n < self.max() as u64).then_some(n as u32)
            } else {
                Some(i)
            }
        } else if i < self.at {
            Some(i)
        } else if i < self.at + self.count {
            None
        } else {
            Some(i - self.count)
        }
    }

    pub fn map_span(&self, a: u32, b: u32) -> Option<(u32, u32)> {
        let last = self.max() - 1;
        if self.insert {
            if a >= self.at {
                let na = a as u64 + self.count as u64;
                if na > last as u64 {
                    return None;
                }
                Some((
                    na as u32,
                    (b as u64 + self.count as u64).min(last as u64) as u32,
                ))
            } else if b >= self.at {
                Some((a, (b as u64 + self.count as u64).min(last as u64) as u32))
            } else {
                Some((a, b))
            }
        } else {
            let (at, end) = (self.at, self.at + self.count);
            if b < at {
                Some((a, b))
            } else if a >= end {
                Some((a - self.count, b - self.count))
            } else if a >= at && b < end {
                None
            } else {
                let na = if a < at { a } else { at };
                let nb = if b >= end { b - self.count } else { at - 1 };
                Some((na, nb))
            }
        }
    }
}

fn resolve(rc: Rc, host: u32, max: u32) -> Option<u32> {
    rc.resolve(host, max, false)
}

fn encode(abs_index: u32, abs: bool, host: u32) -> Rc {
    if abs {
        Rc {
            v: abs_index as i32,
            abs: true,
        }
    } else {
        Rc {
            v: abs_index as i32 - host as i32,
            abs: false,
        }
    }
}

fn shift_cref(cref: &CRef, old: CellAddr, new: CellAddr, s: &Shift) -> Result<CRef, ()> {
    let (rmax, cmax) = (MAX_ROWS, MAX_COLS);
    let rows = s.axis == Axis::Rows;
    let one = |rc: Rc, along: bool, oh: u32, nh: u32, max: u32| -> Result<Rc, ()> {
        let i = resolve(rc, oh, max).ok_or(())?;
        let j = if along { s.map_index(i).ok_or(())? } else { i };
        Ok(encode(j, rc.abs, nh))
    };
    let span = |a: Rc, b: Rc, along: bool, oh: u32, nh: u32, max: u32| -> Result<(Rc, Rc), ()> {
        let i = resolve(a, oh, max).ok_or(())?;
        let j = resolve(b, oh, max).ok_or(())?;
        let (i, j) = if along {
            let (lo, hi) = (i.min(j), i.max(j));
            if lo == 0 && hi == max - 1 {
                return Ok((encode(i, a.abs, nh), encode(j, b.abs, nh)));
            }
            let (na, nb) = s.map_span(lo, hi).ok_or(())?;

            let nb = if !s.insert && hi == max - 1 {
                max - 1
            } else {
                nb
            };
            (na, nb)
        } else {
            (i, j)
        };
        Ok((encode(i, a.abs, nh), encode(j, b.abs, nh)))
    };
    Ok(match *cref {
        CRef::Cell { row, col } => CRef::Cell {
            row: one(row, rows, old.row, new.row, rmax)?,
            col: one(col, !rows, old.col, new.col, cmax)?,
        },
        CRef::Area { r0, c0, r1, c1 } => {
            let (r0, r1) = span(r0, r1, rows, old.row, new.row, rmax)?;
            let (c0, c1) = span(c0, c1, !rows, old.col, new.col, cmax)?;
            CRef::Area { r0, c0, r1, c1 }
        }
        CRef::Cols { c0, c1 } => {
            let (c0, c1) = span(c0, c1, !rows, old.col, new.col, cmax)?;
            CRef::Cols { c0, c1 }
        }
        CRef::Rows { r0, r1 } => {
            let (r0, r1) = span(r0, r1, rows, old.row, new.row, rmax)?;
            CRef::Rows { r0, r1 }
        }
    })
}

pub(crate) fn normalize_cref(cref: &CRef, host: CellAddr) -> CRef {
    let swap = |a: Rc, b: Rc, h: u32, max: u32| -> (Rc, Rc) {
        match (a.resolve(h, max, false), b.resolve(h, max, false)) {
            (Some(i), Some(j)) if i > j => (b, a),
            _ => (a, b),
        }
    };
    match *cref {
        CRef::Area { r0, c0, r1, c1 } => {
            let (r0, r1) = swap(r0, r1, host.row, MAX_ROWS);
            let (c0, c1) = swap(c0, c1, host.col, MAX_COLS);
            CRef::Area { r0, c0, r1, c1 }
        }
        CRef::Cols { c0, c1 } => {
            let (c0, c1) = swap(c0, c1, host.col, MAX_COLS);
            CRef::Cols { c0, c1 }
        }
        CRef::Rows { r0, r1 } => {
            let (r0, r1) = swap(r0, r1, host.row, MAX_ROWS);
            CRef::Rows { r0, r1 }
        }
        c => c,
    }
}

impl Workbook {
    pub(crate) fn copy_ref_edit(
        &self,
        sh: &SheetRef,
        cref: &CRef,
        from: CellAddr,
        to: CellAddr,
    ) -> RefEdit {
        let dead = match sh {
            SheetRef::Sheet(id) => self.sheet(*id).is_none(),
            SheetRef::Span(a, b) => self.sheet(*a).is_none() || self.sheet(*b).is_none(),
            _ => false,
        };
        if dead {
            return match crate::moves::rebase(cref, from, to) {
                Some(c) => RefEdit::Set(sh.clone(), c),
                None => RefEdit::Keep,
            };
        }
        match cref_text(cref, to) {
            Some(_) => RefEdit::Set(sh.clone(), normalize_cref(cref, to)),
            None => RefEdit::Dead(sh.clone()),
        }
    }
}

pub fn map_refs(node: &Node, f: &mut dyn FnMut(&SheetRef, &CRef) -> Option<Option<CRef>>) -> Node {
    edit_refs(node, &mut |sheet, cref| match f(sheet, cref) {
        None => RefEdit::Keep,
        Some(Some(c)) => RefEdit::Set(sheet.clone(), c),
        Some(None) => RefEdit::Dead(sheet.clone()),
    })
}

pub enum RefEdit {
    Keep,

    Set(SheetRef, CRef),

    Dead(SheetRef),
}

pub fn edit_refs(node: &Node, f: &mut dyn FnMut(&SheetRef, &CRef) -> RefEdit) -> Node {
    edit_refs_at(node, &mut |s, c, _| f(s, c), false)
}

pub fn edit_refs_at(
    node: &Node,
    f: &mut dyn FnMut(&SheetRef, &CRef, bool) -> RefEdit,
    implicit: bool,
) -> Node {
    crate::deep(|| edit_refs_at_inner(node, f, implicit))
}

fn edit_refs_at_inner(
    node: &Node,
    f: &mut dyn FnMut(&SheetRef, &CRef, bool) -> RefEdit,
    implicit: bool,
) -> Node {
    let mut m = |n: &Node| edit_refs_at(n, f, false);
    match node {
        Node::Ref(sheet, cref) => match f(sheet, cref, implicit) {
            RefEdit::Keep => node.clone(),
            RefEdit::Set(s, c) => Node::Ref(s, c),
            RefEdit::Dead(s) => Node::DeadRef(s),
        },
        Node::Unary(op, x) => Node::Unary(
            *op,
            Box::new(edit_refs_at(
                x,
                f,
                matches!(
                    *op,
                    crate::parser::ast::UnaryOp::Implicit
                        | crate::parser::ast::UnaryOp::ImplicitQuiet
                ),
            )),
        ),
        Node::Percent(x) => Node::Percent(Box::new(m(x))),
        Node::Spill(x) => Node::Spill(Box::new(m(x))),
        Node::Paren(x) => Node::Paren(Box::new(m(x))),
        Node::Binary(op, a, b) => Node::Binary(*op, Box::new(m(a)), Box::new(m(b))),
        Node::Func { id, args } => Node::Func {
            id: *id,
            args: args.iter().map(&mut m).collect(),
        },
        Node::NamedCall { key, display, args } => Node::NamedCall {
            key: key.clone(),
            display: display.clone(),
            args: args.iter().map(&mut m).collect(),
        },
        Node::Call { callee, args } => Node::Call {
            callee: Box::new(m(callee)),
            args: args.iter().map(&mut m).collect(),
        },
        Node::Let {
            names,
            values,
            body,
            base,
        } => Node::Let {
            names: names.clone(),
            values: values.iter().map(&mut m).collect(),
            body: Box::new(m(body)),
            base: *base,
        },
        Node::Lambda(def) => Node::Lambda(Arc::new(LambdaDef {
            params: def.params.clone(),
            base: def.base,
            body: m(&def.body),
        })),
        Node::Ws {
            before,
            after,
            node,
        } => Node::Ws {
            before: before.clone(),
            after: after.clone(),
            node: Box::new(m(node)),
        },
        Node::Lit(_)
        | Node::Name { .. }
        | Node::Local { .. }
        | Node::Missing
        | Node::Unsupported { .. }
        | Node::DeadRef(_)
        | Node::StructRef { .. } => node.clone(),
    }
}

fn within_limit(cref: &CRef, host: CellAddr, s: &Shift) -> bool {
    let Some((l0, l1)) = s.limit else {
        return true;
    };
    let col = |rc: Rc| resolve(rc, host.col, MAX_COLS).is_some_and(|c| (l0..=l1).contains(&c));
    let row = |rc: Rc| resolve(rc, host.row, MAX_ROWS).is_some_and(|r| (l0..=l1).contains(&r));
    match (cref, s.axis) {
        (CRef::Cell { col: c, .. }, Axis::Rows) => col(*c),
        (CRef::Area { c0, c1, .. }, Axis::Rows) => col(*c0) && col(*c1),
        (CRef::Cell { row: r, .. }, Axis::Cols) => row(*r),
        (CRef::Area { r0, r1, .. }, Axis::Cols) => row(*r0) && row(*r1),
        _ => false,
    }
}

impl Shift {
    fn in_band(&self, r: u32, c: u32) -> bool {
        let p = if self.axis == Axis::Rows { c } else { r };
        self.limit.is_none_or(|(l0, l1)| (l0..=l1).contains(&p))
    }

    fn rect_in_band(&self, r: &Rect) -> bool {
        let (a, b) = if self.axis == Axis::Rows {
            (r.c0, r.c1)
        } else {
            (r.r0, r.r1)
        };
        self.limit.is_none_or(|(l0, l1)| l0 <= a && b <= l1)
    }

    fn deleted_rect(&self) -> Option<Rect> {
        let (l0, l1) = self.limit?;
        let (a, b) = (self.at, self.at + self.count - 1);
        Some(if self.axis == Axis::Rows {
            Rect::new(a, l0, b, l1)
        } else {
            Rect::new(l0, a, l1, b)
        })
    }

    fn straddle(&self, cref: &CRef, old: CellAddr, new: CellAddr) -> Option<CRef> {
        if !self.cells || self.insert {
            return None;
        }
        let x = self.deleted_rect()?;
        let r = crate::moves::cref_rect(cref, old)?;
        match crate::moves::remove_band(&r, &x) {
            crate::moves::Adj::To(to) => Some(crate::moves::encode_rect(cref, to, new)),
            _ => None,
        }
    }
}

fn abs_flags(cref: &CRef) -> (Option<bool>, Option<bool>) {
    match *cref {
        CRef::Cell { row, col } => (Some(row.abs), Some(col.abs)),
        CRef::Area { r0, c0, r1, c1 } => (Some(r0.abs && r1.abs), Some(c0.abs && c1.abs)),
        CRef::Cols { c0, c1 } => (None, Some(c0.abs && c1.abs)),
        CRef::Rows { r0, r1 } => (Some(r0.abs && r1.abs), None),
    }
}

fn targets_sheet(sheet: &SheetRef, host_sheet: SheetId, s: SheetId) -> bool {
    match sheet {
        SheetRef::Current => host_sheet == s,
        SheetRef::Sheet(id) => *id == s,
        _ => false,
    }
}

impl Workbook {
    pub fn insert_rows(
        &mut self,
        sheet: SheetId,
        at: u32,
        count: u32,
    ) -> Result<RecalcReport, EditError> {
        let inside: Vec<(String, Vec<(u32, FormulaId, Arc<Formula>)>)> = self
            .tables
            .iter()
            .filter(|t| t.sheet == sheet && t.rect.r0 < at && at <= t.rect.r1)
            .map(|t| (t.name.clone(), self.calc_column_sources(t, at)))
            .collect();
        let sh = Shift {
            sheet,
            axis: Axis::Rows,
            at,
            count,
            insert: true,
            limit: None,
            cells: false,
        };
        let report = self.shift(sh)?;
        for (name, sources) in inside {
            if let Some(ti) = self.tables.iter().position(|t| t.name == name) {
                let sources = self.calc_sources_shifted(ti, sources, &sh);
                self.table_fill_new_rows(ti, (at, at + count - 1), &sources);
            }
        }
        Ok(report)
    }

    fn calc_sources_shifted(
        &self,
        ti: usize,
        sources: Vec<(u32, FormulaId, Arc<Formula>)>,
        s: &Shift,
    ) -> Vec<(u32, FormulaId, Arc<Formula>)> {
        let t = &self.tables[ti];
        let d0 = t.data_start();
        sources
            .into_iter()
            .map(|(col, fid, f)| {
                let host = CellAddr::new(t.sheet, d0, col);
                let to = if s.axis == Axis::Rows && s.insert && d0 >= s.at {
                    CellAddr::new(t.sheet, d0 + s.count, col)
                } else {
                    host
                };
                let root = map_refs(&f.root, &mut |sheet, cref| {
                    if !targets_sheet(sheet, host.sheet, s.sheet) {
                        (host != to)
                            .then(|| shift_cref(cref, host, to, &Shift { count: 0, ..*s }).ok())
                    } else if within_limit(cref, host, s) {
                        Some(shift_cref(cref, host, to, s).ok())
                    } else {
                        s.straddle(cref, host, to).map(Some)
                    }
                });
                let nf = Arc::new(Formula {
                    root,
                    volatile: f.volatile,
                });
                (col, fid, nf)
            })
            .collect()
    }

    pub fn delete_rows(
        &mut self,
        sheet: SheetId,
        at: u32,
        count: u32,
    ) -> Result<RecalcReport, EditError> {
        self.shift(Shift {
            sheet,
            axis: Axis::Rows,
            at,
            count,
            insert: false,
            limit: None,
            cells: false,
        })
    }

    pub fn insert_cols(
        &mut self,
        sheet: SheetId,
        at: u32,
        count: u32,
    ) -> Result<RecalcReport, EditError> {
        self.shift(Shift {
            sheet,
            axis: Axis::Cols,
            at,
            count,
            insert: true,
            limit: None,
            cells: false,
        })
    }

    pub fn delete_cols(
        &mut self,
        sheet: SheetId,
        at: u32,
        count: u32,
    ) -> Result<RecalcReport, EditError> {
        self.shift(Shift {
            sheet,
            axis: Axis::Cols,
            at,
            count,
            insert: false,
            limit: None,
            cells: false,
        })
    }

    pub fn insert_cells(
        &mut self,
        sheet: SheetId,
        rect: Rect,
        down: bool,
    ) -> Result<RecalcReport, EditError> {
        self.shift_cells(sheet, rect, down, true)
    }

    pub fn delete_cells(
        &mut self,
        sheet: SheetId,
        rect: Rect,
        up: bool,
    ) -> Result<RecalcReport, EditError> {
        self.shift_cells(sheet, rect, up, false)
    }

    fn shift_cells(
        &mut self,
        sheet: SheetId,
        rect: Rect,
        rows: bool,
        insert: bool,
    ) -> Result<RecalcReport, EditError> {
        if self.sheet(sheet).is_none() {
            return Err(EditError::NoSuchSheet);
        }
        if rect.r0 > rect.r1 || rect.c0 > rect.c1 || rect.r1 >= MAX_ROWS || rect.c1 >= MAX_COLS {
            return Err(EditError::Compile(
                "the range is outside the worksheet".into(),
            ));
        }
        let (axis, at, count, mut lim) = if rows {
            (Axis::Rows, rect.r0, rect.rows(), (rect.c0, rect.c1))
        } else {
            (Axis::Cols, rect.c0, rect.cols(), (rect.r0, rect.r1))
        };

        let full = if rows {
            rect.is_full_columns()
        } else {
            rect.is_full_rows()
        };
        if full {
            return match (rows, insert) {
                (true, true) => self.insert_rows(sheet, at, count),
                (true, false) => self.delete_rows(sheet, at, count),
                (false, true) => self.insert_cols(sheet, at, count),
                (false, false) => self.delete_cols(sheet, at, count),
            };
        }
        let moves_table =
            || EditError::Compile("This won't work because it would move cells in a table".into());
        let end = at + count - 1;
        let mut grown: Vec<String> = Vec::new();
        for t in self.tables.iter().filter(|t| t.sheet == sheet) {
            let (ta, tb, p0, p1) = if rows {
                (t.rect.r0, t.rect.r1, t.rect.c0, t.rect.c1)
            } else {
                (t.rect.c0, t.rect.c1, t.rect.r0, t.rect.r1)
            };
            if lim.1 < p0 || lim.0 > p1 || at > tb {
                continue;
            }
            let covers = lim.0 <= p0 && p1 <= lim.1;
            if insert {
                if at <= ta {
                    if !covers {
                        return Err(moves_table());
                    }
                } else if lim == (p0, p1) {
                    if rows {
                        grown.push(t.name.clone());
                    }
                } else {
                    return Err(moves_table());
                }
                continue;
            }
            if end < ta {
                if !covers {
                    return Err(moves_table());
                }
                continue;
            }

            let (bh, th, bo, to) = if rows {
                ((at, end), (ta, tb), lim, (p0, p1))
            } else {
                (lim, (p0, p1), (at, end), (ta, tb))
            };
            let covers_h = bh.0 <= th.0 && th.1 <= bh.1;
            let exact_h = bh == th;
            let data_h = bh.0 > th.0 && bh.1 <= th.1;
            let covers_o = bo.0 <= to.0 && to.1 <= bo.1;
            let within_o = to.0 <= bo.0 && bo.1 <= to.1;
            let allowed = if covers_h {
                covers_o || exact_h && within_o
            } else {
                data_h && within_o
            };
            if !allowed {
                return Err(EditError::Compile(
                    "Unable to get the Delete property of the Range class".into(),
                ));
            }

            lim = (lim.0.min(p0), lim.1.max(p1));
        }
        let sources: Vec<(String, Vec<(u32, FormulaId, Arc<Formula>)>)> = grown
            .iter()
            .filter_map(|n| self.tables.iter().find(|t| &t.name == n))
            .map(|t| (t.name.clone(), self.calc_column_sources(t, at)))
            .collect();
        let sh = Shift {
            sheet,
            axis,
            at,
            count,
            insert,
            limit: Some(lim),
            cells: true,
        };
        let report = self.shift(sh)?;
        for (name, sources) in sources {
            if let Some(ti) = self.tables.iter().position(|t| t.name == name) {
                let sources = self.calc_sources_shifted(ti, sources, &sh);
                self.table_fill_new_rows(ti, (at, at + count - 1), &sources);
            }
        }
        Ok(report)
    }

    pub(crate) fn shift(&mut self, s: Shift) -> Result<RecalcReport, EditError> {
        let mut s = s;
        if self.sheet(s.sheet).is_none() {
            return Err(EditError::NoSuchSheet);
        }

        if s.at >= s.max() || s.count > s.max() {
            return Err(EditError::Compile(
                "the position is outside the worksheet".into(),
            ));
        }
        if !s.insert {
            s.count = s.count.min(s.max() - s.at);
        }
        self.guard_arrays_shift(&s)?;

        if s.limit.is_none() {
            let span = |t: &crate::workbook::tables::Table| match s.axis {
                Axis::Rows => (t.rect.r0, t.rect.r1),
                Axis::Cols => (t.rect.c0, t.rect.c1),
            };
            let on: Vec<(u32, u32)> = self
                .tables
                .iter()
                .filter(|t| t.sheet == s.sheet)
                .map(span)
                .collect();
            let blocked = if s.insert {
                let hit: Vec<_> = on
                    .iter()
                    .filter(|(a, b)| *a <= s.at && s.at <= *b)
                    .collect();
                hit.len() >= 2 && hit.iter().any(|(a, _)| *a < s.at)
            } else {
                on.iter()
                    .filter(|(a, b)| *a < s.at + s.count && s.at <= *b)
                    .count()
                    >= 2
            };
            if blocked {
                return Err(EditError::Compile(
                    "cannot change rows or columns shared by two tables".into(),
                ));
            }
        }
        if s.axis == Axis::Rows && !s.insert && (s.limit.is_none() || s.cells) {
            let s0 = s;
            let (lo, hi) = (s.at, s.at + s.count);
            let mut clear_first: Option<(Rect, bool)> = None;
            for t in self
                .tables
                .iter()
                .filter(|t| t.sheet == s0.sheet && s0.rect_in_band(&t.rect))
            {
                let (h, last) = (t.rect.r0, t.rect.r1);
                if t.headers && lo <= h && h < hi && hi <= last {
                    return Err(EditError::Compile(
                        "cannot delete the header row of a table".into(),
                    ));
                }
                let (d0, d1) = t.data_rows();
                let hg = if t.headers { h } else { d0.saturating_sub(1) };
                if lo > hg && lo <= d0 && hi > d1 {
                    clear_first = Some((
                        Rect::new(d0, t.rect.c0, d0, t.rect.c1),
                        (t.totals && hi <= last) || s0.cells,
                    ));
                    s.at = d0 + 1;
                    s.count = hi - (d0 + 1);
                }
            }
            if let Some((rect, totals_kept)) = clear_first {
                if totals_kept {
                    for c in rect.c0..=rect.c1 {
                        self.clear(CellAddr::new(s.sheet, rect.r0, c));
                    }
                    return self.shift_core(s);
                }

                let r = self.shift_core(s)?;
                for c in rect.c0..=rect.c1 {
                    self.remove_cell(CellAddr::new(s.sheet, rect.r0, c));
                }
                self.calc.stale_tables = true;
                return Ok(r);
            }
        }
        self.shift_core(s)
    }

    fn shift_core(&mut self, s: Shift) -> Result<RecalcReport, EditError> {
        if s.count == 0 {
            return Ok(RecalcReport::default());
        }
        let along = |r: u32, c: u32| if s.axis == Axis::Rows { r } else { c };

        if s.insert {
            let limit = s.max() - s.count;
            let sh = self.sheet(s.sheet).unwrap();
            if sh
                .cells
                .iter()
                .any(|(r, c, _)| along(r, c) >= limit && s.in_band(r, c))
            {
                return Err(EditError::Compile(
                    "cannot shift non-blank cells off the worksheet".into(),
                ));
            }
        }

        let data_tables = self.plan_data_table_shift(&s)?;

        self.shift_merges(&s);

        let old_cells: Vec<(u32, u32, Entry)> = {
            let sh = self.sheet(s.sheet).unwrap();
            sh.cells.iter().map(|(r, c, e)| (r, c, e.clone())).collect()
        };
        let mut moved: Vec<(FormulaId, CellAddr, CellAddr)> = Vec::new();
        let mut deleted: Vec<FormulaId> = Vec::new();
        let spilling: rustc_hash::FxHashSet<FormulaId> = self
            .sheets
            .iter()
            .flat_map(|sh| sh.spills.active_ids())
            .collect();
        self.calc.spilling = spilling;
        {
            let sh = self.sheet_mut(s.sheet).unwrap();
            sh.cells = Default::default();
            sh.spills = Default::default();
            sh.dirty = Default::default();
        }
        for (r, c, e) in old_cells {
            let (nr, nc) = match s.axis {
                _ if !s.in_band(r, c) => (Some(r), Some(c)),
                Axis::Rows => (s.map_index(r), Some(c)),
                Axis::Cols => (Some(r), s.map_index(c)),
            };
            match (nr, nc) {
                (Some(nr), Some(nc)) => {
                    if let Entry::Formula(fid) = &e {
                        moved.push((
                            *fid,
                            CellAddr::new(s.sheet, r, c),
                            CellAddr::new(s.sheet, nr, nc),
                        ));
                    }
                    self.sheet_mut(s.sheet).unwrap().cells.set(nr, nc, e);
                }
                _ => {
                    if let Entry::Formula(fid) = e {
                        deleted.push(fid);
                    }
                }
            }
        }
        for fid in deleted {
            self.free_formula(fid);
        }
        self.fmt_shift(
            s.sheet,
            s.axis == Axis::Rows,
            s.at,
            s.count,
            s.insert,
            s.limit,
        );
        let new_addr: rustc_hash::FxHashMap<FormulaId, CellAddr> =
            moved.iter().map(|(f, _, n)| (*f, *n)).collect();

        let expand: Vec<(u32, u32, u32, u32)> = if s.axis == Axis::Rows && s.insert {
            self.tables
                .iter()
                .filter(|t| t.sheet == s.sheet)
                .filter_map(|t| {
                    let (d0, d1) = t.data_rows();
                    (s.at == d0 || (t.totals && s.at == d1 + 1))
                        .then_some((d0, d1, t.rect.c0, t.rect.c1))
                })
                .collect()
        } else {
            Vec::new()
        };
        let ids: Vec<FormulaId> = self.formula_ids().collect();
        for fid in ids {
            let (old, formula) = {
                let f = self.formula(fid);
                (f.addr, f.formula.clone())
            };
            let new = new_addr.get(&fid).copied().unwrap_or(old);
            let root = map_refs(&formula.root, &mut |sheet, cref| {
                if targets_sheet(sheet, old.sheet, s.sheet) && within_limit(cref, old, &s) {
                    let mut shifted = shift_cref(cref, old, new, &s);
                    if let (
                        Ok(CRef::Area {
                            r0: nr0,
                            c0: nc0,
                            r1: nr1,
                            c1: nc1,
                        }),
                        CRef::Area { r0, c0, r1, c1 },
                    ) = (&mut shifted, cref)
                    {
                        let rows = (
                            resolve(*r0, old.row, MAX_ROWS),
                            resolve(*r1, old.row, MAX_ROWS),
                        );
                        let cols = (
                            resolve(*c0, old.col, MAX_COLS),
                            resolve(*c1, old.col, MAX_COLS),
                        );
                        if let ((Some(a), Some(b)), (Some(x), Some(y))) = (rows, cols) {
                            let (x, y) = (x.min(y), x.max(y));
                            if expand
                                .iter()
                                .any(|&(d0, d1, c0, c1)| a <= d0 && b == d1 && x >= c0 && y <= c1)
                            {
                                *nr0 = encode(a, r0.abs, new.row);
                                *nr1 = encode(b + s.count, r1.abs, new.row);
                                let _ = (nc0, nc1, c0, c1);
                            }
                        }
                    }
                    Some(shifted.ok())
                } else if let Some(c) = targets_sheet(sheet, old.sheet, s.sheet)
                    .then(|| s.straddle(cref, old, new))
                    .flatten()
                {
                    Some(Some(c))
                } else if old != new {
                    Some(shift_cref(cref, old, new, &Shift { count: 0, ..s }).ok())
                } else {
                    None
                }
            });
            let text = print_node(self, &root, new);

            if old.sheet != s.sheet && text != self.formula(fid).text {
                self.calc.dt_seed.push(fid);
            }
            let text = print_node(self, &root, new);
            let f = self.formula_mut(fid);
            f.addr = new;
            f.formula = Arc::new(Formula {
                root,
                volatile: formula.volatile,
            });
            f.text = text;
            f.spill = None;
        }

        let mut kept = Vec::new();

        let mut gone: Vec<(String, Vec<String>)> = Vec::new();

        let mut headers: Vec<(SheetId, u32, u32, String)> = Vec::new();

        let mut removed: Vec<String> = Vec::new();
        for mut t in std::mem::take(&mut self.tables) {
            if t.sheet != s.sheet || (s.limit.is_some() && !(s.cells && s.rect_in_band(&t.rect))) {
                kept.push(t);
                continue;
            }
            let (a, b) = match s.axis {
                Axis::Rows => (t.rect.r0, t.rect.r1),
                Axis::Cols => (t.rect.c0, t.rect.c1),
            };
            let Some((na, nb)) = s.map_span(a, b) else {
                removed.push(t.name.clone());
                continue;
            };
            match s.axis {
                Axis::Rows => {
                    if t.totals && !s.insert && s.map_index(b).is_none() {
                        t.totals = false;
                    }
                    t.rect.r0 = na;
                    t.rect.r1 = nb;
                }
                Axis::Cols => {
                    if s.insert && s.at > a && s.at <= b {
                        let pos = (s.at - a) as usize;
                        for k in 0..s.count as usize {
                            let left = t.columns[pos - 1].clone();
                            let name = if let Some(n) =
                                crate::workbook::tables::next_header_name(&left, &t.columns)
                            {
                                n
                            } else {
                                let mut n = 1;
                                while t.column_index(&format!("Column{n}")).is_some() {
                                    n += 1;
                                }
                                format!("Column{n}")
                            };
                            if t.headers {
                                headers.push((t.sheet, t.rect.r0, s.at + k as u32, name.clone()));
                            }
                            t.columns.insert(pos + k, name);
                            t.calc.resize(t.columns.len() - 1, false);
                            t.calc.insert(pos + k, false);
                        }
                    } else if !s.insert {
                        let (lo, hi) = (s.at, s.at + s.count);
                        let mut i = 0;
                        let mut removed = Vec::new();
                        t.calc.resize(t.columns.len(), false);
                        let calc_old = std::mem::take(&mut t.calc);
                        t.calc = calc_old
                            .iter()
                            .enumerate()
                            .filter(|(j, _)| {
                                let c = a + *j as u32;
                                !(c >= lo && c < hi)
                            })
                            .map(|(_, v)| *v)
                            .collect();
                        t.columns.retain(|name| {
                            let c = a + i;
                            i += 1;
                            let keep = !(c >= lo && c < hi);
                            if !keep {
                                removed.push(name.clone());
                            }
                            keep
                        });
                        if !removed.is_empty() {
                            gone.push((t.name.clone(), removed));
                        }
                    }
                    t.rect.c0 = na;
                    t.rect.c1 = nb;
                }
            }
            kept.push(t);
        }
        self.tables = kept;
        for (sheet, r, c, name) in headers {
            if let Some(sh) = self.sheet_mut(sheet) {
                sh.cells.set(r, c, Entry::Const(Value::from(name.as_str())));
            }
        }
        if !gone.is_empty() || !removed.is_empty() {
            let ids: Vec<FormulaId> = self.formula_ids().collect();
            for fid in ids {
                let formula = self.formula(fid).formula.clone();
                let mut changed = false;
                let root = crate::formula::map_node(&formula.root, &mut |n| match n {
                    Node::StructRef { table, .. }
                        if removed.iter().any(|r| crate::text::eq_ordinal_ci(r, table)) =>
                    {
                        changed = true;
                        Some(Node::DeadRef(SheetRef::Current))
                    }
                    Node::StructRef { table, spec, .. } => {
                        let (a, b) = spec.cols.as_ref()?;
                        let dead = gone.iter().any(|(t, cols)| {
                            crate::text::eq_ordinal_ci(t, table)
                                && cols.iter().any(|c| {
                                    crate::text::eq_ordinal_ci(c, a)
                                        || crate::text::eq_ordinal_ci(c, b)
                                })
                        });
                        changed |= dead;
                        dead.then(|| Node::DeadRef(SheetRef::Current))
                    }
                    _ => None,
                });
                if changed {
                    let host = self.formula(fid).addr;
                    let text = print_node(self, &root, host);
                    let f = self.formula_mut(fid);
                    f.formula = Arc::new(Formula {
                        root,
                        volatile: formula.volatile,
                    });
                    f.text = text;
                }
            }
        }

        let names: Vec<_> = self.names.iter().cloned().collect();
        for mut def in names {
            let host = def.host();
            let root = map_refs(&def.formula.root, &mut |sheet, cref| {
                let (rows, cols) = abs_flags(cref);
                let along = if s.axis == Axis::Rows { rows } else { cols };
                let fixed = if s.cells {
                    rows == Some(true) && cols == Some(true)
                } else {
                    along != Some(false)
                };
                if !(s.limit.is_none() || s.cells)
                    || !targets_sheet(sheet, host.sheet, s.sheet)
                    || !fixed
                {
                    None
                } else if within_limit(cref, host, &s) {
                    Some(shift_cref(cref, host, host, &s).ok())
                } else {
                    s.straddle(cref, host, host).map(Some)
                }
            });
            def.refers_to = print_node(self, &root, host);
            def.formula = Arc::new(Formula {
                root,
                volatile: def.formula.volatile,
            });
            self.names.insert(def);
        }
        self.data_tables = data_tables;
        Ok(self.relink_and_recalc())
    }

    fn plan_data_table_shift(&self, s: &Shift) -> Result<Vec<DataTable>, EditError> {
        let mut out = Vec::new();
        for t in &self.data_tables {
            if t.sheet != s.sheet {
                out.push(t.clone());
                continue;
            }
            let map = |r: u32, c: u32| -> Option<(u32, u32)> {
                if !s.in_band(r, c) {
                    return Some((r, c));
                }
                match s.axis {
                    Axis::Rows => s.map_index(r).map(|n| (n, c)),
                    Axis::Cols => s.map_index(c).map(|n| (r, n)),
                }
            };
            let int = t.interior();
            let (mut gone, mut edge_gone, mut mixed) = (0u64, false, false);
            let mut delta: Option<(i64, i64)> = None;
            for r in t.rect.r0..=t.rect.r1 {
                for c in t.rect.c0..=t.rect.c1 {
                    match map(r, c) {
                        None if int.contains(r, c) => gone += 1,
                        None => edge_gone = true,
                        Some((nr, nc)) => {
                            let d = (nr as i64 - r as i64, nc as i64 - c as i64);
                            mixed |= *delta.get_or_insert(d) != d;
                        }
                    }
                }
            }
            if gone == int.cell_count() {
                continue;
            }
            if gone > 0 || edge_gone || mixed {
                return Err(EditError::Compile(PART_ERROR.into()));
            }
            let (dr, dc) = delta.unwrap_or((0, 0));
            let mut nt = t.clone();
            nt.rect = Rect::new(
                (t.rect.r0 as i64 + dr) as u32,
                (t.rect.c0 as i64 + dc) as u32,
                (t.rect.r1 as i64 + dr) as u32,
                (t.rect.c1 as i64 + dc) as u32,
            );
            nt.dirty = true;
            out.push(nt);
        }
        Ok(out)
    }

    pub(crate) fn relink_and_recalc(&mut self) -> RecalcReport {
        let manual = self.settings.mode == crate::workbook::CalcMode::Manual;
        let before: Vec<(FormulaId, Value, Option<Rect>)> = if manual {
            self.formula_ids()
                .map(|f| {
                    let a = self.formula(f).addr;

                    let blocked = self.sheet(a.sheet).and_then(|s| {
                        s.spills.blocked_rect(f).filter(|r| {
                            s.cells
                                .iter_rect(*r)
                                .any(|(rr, cc, _)| (rr, cc) != (a.row, a.col))
                                || s.spills.active_overlapping(*r)
                        })
                    });
                    (f, self.formula(f).value.clone(), blocked)
                })
                .collect()
        } else {
            Vec::new()
        };
        self.graph = DepGraph::default();
        self.calc.volatile.clear();
        self.calc.circular.clear();
        self.calc.dirty_list.clear();
        let spilling = std::mem::take(&mut self.calc.spilling);
        for sh in &mut self.sheets {
            sh.spills = Default::default();
            sh.dirty = Default::default();
        }
        self.reserve_array_blocks();
        let mut ids: Vec<FormulaId> = self.formula_ids().collect();

        ids.sort_by_key(|&f| (!spilling.contains(&f), self.formula(f).born));
        for (rank, &fid) in ids.iter().enumerate() {
            self.formula_mut(fid).seq = rank as i64 + 1;
        }
        self.calc.next_done_seq = ids.len() as i64 + 1;
        for &fid in &ids {
            let f = self.formula_mut(fid);
            f.dirty = false;
            f.on_stack = false;
            f.active = false;
            f.precedents.clear();
            self.link_formula(fid);
        }
        for fid in std::mem::take(&mut self.calc.dt_seed) {
            if self.formulas.get(fid as usize).is_some_and(|f| f.is_some()) {
                calc::mark_dirty(self, fid);
            }
        }

        self.calc.dt_block = true;
        for fid in ids {
            calc::mark_dirty(self, fid);
        }
        self.calc.dt_block = false;
        let report = calc::recalc(self);
        if !manual {
            let stuck: Vec<FormulaId> = self
                .calc
                .circular
                .iter()
                .copied()
                .filter(|&f| self.formulas.get(f as usize).is_some_and(|c| c.is_some()))
                .collect();
            for fid in stuck {
                let f = self.formula(fid);
                if !f.dirty || f.spill.is_some() {
                    continue;
                }
                let addr = f.addr;
                let Value::Array(a) = &f.value else { continue };
                if a.rows < 2 && a.cols < 2 {
                    continue;
                }
                let rect = Rect::new(
                    addr.row,
                    addr.col,
                    addr.row + a.rows - 1,
                    addr.col + a.cols - 1,
                );
                if let Some(sh) = self.sheet_mut(addr.sheet) {
                    sh.spills.set_active(fid, rect);
                }
                self.formula_mut(fid).spill = Some(rect);
            }
        }
        for (fid, old, was_blocked) in before {
            let addr = self.formula(fid).addr;
            let blocked = self
                .sheet(addr.sheet)
                .is_some_and(|s| s.spills.is_blocked(fid));

            let unblocked = match (was_blocked, self.formula(fid).spill) {
                (Some(w), Some(n)) => w.rows() == n.rows() && w.cols() == n.cols(),
                _ => false,
            };
            let anchor_moved = blocked || unblocked;
            if self.formulas.get(fid as usize).is_some_and(|f| f.is_some())
                && (!self.formula(fid).value.same_as(&old)
                    || (self.formula(fid).dirty && self.formula(fid).spill.is_none()))
                && !anchor_moved
            {
                let (addr, was_spilling) =
                    (self.formula(fid).addr, self.formula(fid).spill.is_some());
                let rect = match &old {
                    Value::Array(a) if a.rows > 1 || a.cols > 1 => Some(Rect::new(
                        addr.row,
                        addr.col,
                        addr.row + a.rows as u32 - 1,
                        addr.col + a.cols as u32 - 1,
                    )),
                    _ => None,
                };
                if rect.is_some() || was_spilling {
                    if let Some(sh) = self.sheet_mut(addr.sheet) {
                        sh.spills.remove(fid);
                        if let Some(r) = rect {
                            sh.spills.set_active(fid, r);
                        }
                    }
                }
                self.formula_mut(fid).spill = rect;
                self.formula_mut(fid).value = old;
                self.set_dirty(fid, true);
                self.calc.dirty_list.push(fid);
            }
        }
        report
    }

    pub fn copy_range(
        &mut self,
        src_sheet: SheetId,
        src: Rect,
        dst: CellAddr,
    ) -> Result<RecalcReport, EditError> {
        if src.r0 > src.r1 || src.c0 > src.c1 || src.r1 >= MAX_ROWS || src.c1 >= MAX_COLS {
            return Err(EditError::Compile(
                "the range is outside the worksheet".into(),
            ));
        }
        let dr = dst.row as i64 - src.r0 as i64;
        let dc = dst.col as i64 - src.c0 as i64;
        if dst.row as u64 + src.rows() as u64 > MAX_ROWS as u64
            || dst.col as u64 + src.cols() as u64 > MAX_COLS as u64
        {
            return Err(EditError::Compile(
                "paste area does not fit on the sheet".into(),
            ));
        }
        self.sheet(src_sheet).ok_or(EditError::NoSuchSheet)?;
        self.sheet(dst.sheet).ok_or(EditError::NoSuchSheet)?;
        if src.rows() as u64 * src.cols() as u64 > MAX_PASTE_CELLS {
            return Err(EditError::Compile("The paste area is too large.".into()));
        }
        self.names_follow(src_sheet, src, dst.sheet, true);
        let blocks = self.paste_blocks_plan(src_sheet, src, dst)?;

        self.data_tables_overwritten(
            dst.sheet,
            Rect::new(
                dst.row,
                dst.col,
                dst.row + src.rows() - 1,
                dst.col + src.cols() - 1,
            ),
        )?;
        let entries: Vec<(u32, u32, Entry)> = {
            let sh = self.sheet(src_sheet).ok_or(EditError::NoSuchSheet)?;
            sh.cells
                .iter_rect(src)
                .map(|(r, c, e)| (r, c, e.clone()))
                .collect()
        };
        let by_pos: rustc_hash::FxHashMap<(u32, u32), usize> = entries
            .iter()
            .enumerate()
            .map(|(i, e)| ((e.0, e.1), i))
            .collect();
        let mut plan: Vec<(CellAddr, Result<Value, (Arc<Formula>, String, Value)>)> = Vec::new();
        for r in src.r0..=src.r1 {
            for c in src.c0..=src.c1 {
                let to = CellAddr::new(dst.sheet, (r as i64 + dr) as u32, (c as i64 + dc) as u32);
                match by_pos.get(&(r, c)).map(|&i| &entries[i].2) {
                    None => {
                        let in_header = src.rows() == 1
                            && entries.is_empty()
                            && self.tables.iter().any(|t| {
                                t.sheet == to.sheet
                                    && t.header_row() == Some(to.row)
                                    && (t.rect.c0..=t.rect.c1).contains(&to.col)
                            });
                        if !in_header {
                            plan.push((to, Ok(Value::Blank)));
                        }
                    }
                    Some(Entry::Const(v)) => plan.push((to, Ok(v.clone()))),

                    Some(Entry::Formula(fid)) if is_table_formula(&self.formula(*fid).formula) => {
                        plan.push((to, Ok(self.formula(*fid).value.clone())))
                    }
                    Some(Entry::Formula(fid)) => {
                        let f = self.formula(*fid);
                        let from = f.addr;
                        let root = edit_refs(&f.formula.root, &mut |sh, cref| {
                            self.copy_ref_edit(sh, cref, from, to)
                        });
                        let text = print_node(self, &root, to);
                        plan.push((
                            to,
                            Err((
                                Arc::new(Formula {
                                    root,
                                    volatile: f.formula.volatile,
                                }),
                                text,
                                f.value_at(f.addr.row, f.addr.col),
                            )),
                        ));
                    }
                }
            }
        }
        self.fmt_copy_block(src_sheet, src, dst);
        let mut report = RecalcReport::default();
        let mode = self.settings.mode;
        self.settings.mode = crate::workbook::CalcMode::Manual;
        for (to, what) in plan {
            match what {
                Ok(v) => {
                    self.set_value_exact(to, v);
                }
                Err((formula, text, shown)) => {
                    self.set_compiled_formula(to, formula, text);
                    {
                        if let Some(fid) = self.formula_at(to) {
                            self.formula_mut(fid).value = shown;
                        }
                    }
                }
            }
            report.changed.push(to);
        }
        self.paste_blocks(blocks);
        self.settings.mode = mode;
        if mode == crate::workbook::CalcMode::Automatic {
            let r = calc::recalc(self);
            report.evaluations = r.evaluations;
            report.circular = r.circular;
            report.changed.extend(r.changed);
        }

        let headers: Vec<CellAddr> = report
            .changed
            .iter()
            .copied()
            .filter(|a| {
                self.tables.iter().any(|t| {
                    t.sheet == a.sheet
                        && t.header_row() == Some(a.row)
                        && (t.rect.c0..=t.rect.c1).contains(&a.col)
                }) && matches!(
                    self.sheet(a.sheet).and_then(|s| s.cells.get(a.row, a.col)),
                    Some(Entry::Formula(_))
                )
            })
            .collect();
        for a in headers {
            let text = crate::coerce::to_text(&self.value(a))
                .map(|t| t.to_string())
                .unwrap_or_else(|e| e.to_string());
            self.set_value(a, Value::from(text.as_str()));
        }

        let dest = Rect::new(
            dst.row,
            dst.col,
            dst.row + src.rows() - 1,
            dst.col + src.cols() - 1,
        );
        if let Some((ti, rect, rows)) = self.paste_growth(dst.sheet, dest, None) {
            self.grow_table_by_paste(ti, rect, rows);
        }
        Ok(calc::finish_report(report))
    }

    pub fn fill(
        &mut self,
        sheet: SheetId,
        src: Rect,
        dst: Rect,
    ) -> Result<RecalcReport, EditError> {
        if dst.rows() as u64 * dst.cols() as u64 > MAX_PASTE_CELLS {
            return Err(EditError::Compile("The fill range is too large.".into()));
        }
        let mut report = RecalcReport::default();
        let mut r = dst.r0;
        while r <= dst.r1 {
            let mut c = dst.c0;
            while c <= dst.c1 {
                let h = src.rows().min(dst.r1 - r + 1);
                let w = src.cols().min(dst.c1 - c + 1);
                let part = Rect::new(src.r0, src.c0, src.r0 + h - 1, src.c0 + w - 1);
                let rep = self.copy_range(sheet, part, CellAddr::new(sheet, r, c))?;
                report.evaluations += rep.evaluations;
                report.changed.extend(rep.changed);
                c += src.cols();
            }
            r += src.rows();
        }
        Ok(calc::finish_report(report))
    }
}
