use std::sync::Arc;

use crate::address::{CellAddr, Rect, SheetId, MAX_COLS, MAX_ROWS};
use crate::calc::RecalcReport;
use crate::formula::{CRef, Formula, Rc, SheetRef};
use crate::formula_print::print_node;
use crate::structure::{edit_refs, edit_refs_at, RefEdit};
use crate::workbook::{EditError, Entry, FormulaId, Workbook};

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub(crate) enum Adj {
    Keep,

    To(Rect),

    Moved(Rect),
    Dead,
}

fn inside(a: &Rect, b: &Rect) -> bool {
    b.r0 <= a.r0 && a.r1 <= b.r1 && b.c0 <= a.c0 && a.c1 <= b.c1
}

fn offset(r: &Rect, dr: i64, dc: i64) -> Rect {
    Rect::new(
        (r.r0 as i64 + dr) as u32,
        (r.c0 as i64 + dc) as u32,
        (r.r1 as i64 + dr) as u32,
        (r.c1 as i64 + dc) as u32,
    )
}

#[allow(clippy::too_many_arguments)]
fn band(
    (a, b): (i64, i64),
    (s0, s1): (i64, i64),
    d: i64,
    (p0, p1): (i64, i64),
    (q0, q1): (i64, i64),
    dperp: i64,
) -> Option<Option<(i64, i64)>> {
    if !(q0 <= p0 && p1 <= q1) || dperp != 0 {
        return None;
    }
    let (d0, d1) = (s0 + d, s1 + d);
    if s0 > a && s0 <= b && b <= s1 {
        let nb = b + d;
        if d0 <= a && a <= d1 {
            return Some(Some((a.min(s0 + d), nb.max(s0 - 1))));
        }
        if nb < a {
            return Some(None);
        }
        if s1 > b {
            return Some(Some((a, nb)));
        }
        return Some(Some((a.min(s0 + d), nb.max(s0 - 1))));
    }
    if s1 < b && s0 <= a && a <= s1 {
        let na = a + d;
        if d0 <= b && b <= d1 {
            return Some(Some((na.min(s1 + 1), b.max(s1 + d))));
        }
        if na > b {
            return Some(None);
        }
        if s0 < a {
            return Some(Some((na, b)));
        }
        return Some(Some((na.min(s1 + 1), b.max(s1 + d))));
    }
    None
}

fn dest_side(r: &Rect, d: &Rect) -> Adj {
    if inside(r, d) {
        return Adj::Dead;
    }
    if d.c0 <= r.c0 && r.c1 <= d.c1 {
        if d.r0 < r.r0 && r.r0 <= d.r1 && d.r1 < r.r1 {
            return Adj::To(Rect::new(d.r1 + 1, r.c0, r.r1, r.c1));
        }
        if r.r0 < d.r0 && d.r0 <= r.r1 && r.r1 < d.r1 {
            return Adj::To(Rect::new(r.r0, r.c0, d.r0 - 1, r.c1));
        }
    }
    if d.r0 <= r.r0 && r.r1 <= d.r1 {
        if d.c0 < r.c0 && r.c0 <= d.c1 && d.c1 < r.c1 {
            return Adj::To(Rect::new(r.r0, d.c1 + 1, r.r1, r.c1));
        }
        if r.c0 < d.c0 && d.c0 <= r.c1 && r.c1 < d.c1 {
            return Adj::To(Rect::new(r.r0, r.c0, r.r1, d.c0 - 1));
        }
    }
    Adj::Keep
}

pub(crate) fn remove_band(r: &Rect, s: &Rect) -> Adj {
    if s.c0 <= r.c0 && r.c1 <= s.c1 {
        if s.r0 <= r.r0 && r.r0 <= s.r1 && s.r1 < r.r1 {
            return Adj::To(Rect::new(s.r1 + 1, r.c0, r.r1, r.c1));
        }
        if r.r0 < s.r0 && s.r0 <= r.r1 && r.r1 <= s.r1 {
            return Adj::To(Rect::new(r.r0, r.c0, s.r0 - 1, r.c1));
        }
    }
    if s.r0 <= r.r0 && r.r1 <= s.r1 {
        if s.c0 <= r.c0 && r.c0 <= s.c1 && s.c1 < r.c1 {
            return Adj::To(Rect::new(r.r0, s.c1 + 1, r.r1, r.c1));
        }
        if r.c0 < s.c0 && s.c0 <= r.c1 && r.c1 <= s.c1 {
            return Adj::To(Rect::new(r.r0, r.c0, r.r1, s.c0 - 1));
        }
    }
    Adj::Keep
}

pub(crate) fn move_area(r: &Rect, s: &Rect, dr: i64, dc: i64) -> Adj {
    if inside(r, s) {
        return Adj::Moved(offset(r, dr, dc));
    }
    let d = offset(s, dr, dc);
    let i = |x: u32| x as i64;
    if let Some(x) = band(
        (i(r.r0), i(r.r1)),
        (i(s.r0), i(s.r1)),
        dr,
        (i(r.c0), i(r.c1)),
        (i(s.c0), i(s.c1)),
        dc,
    ) {
        return match x {
            None => Adj::Keep,
            Some((a, b)) => Adj::To(Rect::new(a as u32, r.c0, b as u32, r.c1)),
        };
    }
    if let Some(x) = band(
        (i(r.c0), i(r.c1)),
        (i(s.c0), i(s.c1)),
        dc,
        (i(r.r0), i(r.r1)),
        (i(s.r0), i(s.r1)),
        dr,
    ) {
        return match x {
            None => Adj::Keep,
            Some((a, b)) => Adj::To(Rect::new(r.r0, a as u32, r.r1, b as u32)),
        };
    }
    corners(r, s, &d, dr, dc)
}

#[derive(Clone, Copy, PartialEq, Eq)]
enum Fate {
    Src,

    Dst,

    Stay,
}

fn corners(r: &Rect, s: &Rect, d: &Rect, dr: i64, dc: i64) -> Adj {
    let fate = |row: u32, col: u32| {
        if s.contains(row, col) {
            Fate::Src
        } else if d.contains(row, col) {
            Fate::Dst
        } else {
            Fate::Stay
        }
    };
    let pts = [(r.r0, r.c0), (r.r0, r.c1), (r.r1, r.c0), (r.r1, r.c1)];
    let [tl, tr, bl, br] = pts.map(|(y, x)| fate(y, x));
    let moved = r.intersection(s).map(|i| offset(&i, dr, dc));
    let edge = |f: Fate| {
        (tl == f && tr == f) || (bl == f && br == f) || (tl == f && bl == f) || (tr == f && br == f)
    };
    if inside(r, d) {
        return match moved {
            Some(m) if edge(Fate::Src) => Adj::To(m),
            _ => Adj::Dead,
        };
    }
    if !edge(Fate::Dst) {
        return Adj::Keep;
    }
    let pos = |(y, x): (u32, u32), f: Fate| -> Option<(i64, i64)> {
        match f {
            Fate::Src => Some((y as i64 + dr, x as i64 + dc)),
            Fate::Stay => Some((y as i64, x as i64)),
            Fate::Dst => None,
        }
    };
    let [ptl, ptr, pbl, pbr] = [
        pos(pts[0], tl),
        pos(pts[1], tr),
        pos(pts[2], bl),
        pos(pts[3], br),
    ];
    let i = |x: u32| x as i64;

    let fallback = match moved {
        Some(m) if tl == Fate::Src || br == Fate::Src => (i(m.r0), i(m.c0), i(m.r1), i(m.c1)),
        _ => (
            if d.r0 < r.r0 { i(d.r1) + 1 } else { i(r.r0) },
            if d.c0 < r.c0 { i(d.c1) + 1 } else { i(r.c0) },
            if d.r1 > r.r1 { i(d.r0) - 1 } else { i(r.r1) },
            if d.c1 > r.c1 { i(d.c0) - 1 } else { i(r.c1) },
        ),
    };
    let pick = |a: Option<(i64, i64)>, b: Option<(i64, i64)>, row: bool, fb: i64| {
        a.or(b).map_or(fb, |p| if row { p.0 } else { p.1 })
    };
    let top = pick(ptl, ptr, true, fallback.0);
    let left = pick(ptl, pbl, false, fallback.1);
    let bottom = pick(pbr, pbl, true, fallback.2);
    let right = pick(pbr, ptr, false, fallback.3);
    let to = Rect::new(top as u32, left as u32, bottom as u32, right as u32);
    if to == *r {
        Adj::Keep
    } else {
        Adj::To(to)
    }
}

pub(crate) fn cref_rect(cref: &CRef, host: CellAddr) -> Option<Rect> {
    let r = |rc: Rc| rc.resolve(host.row, MAX_ROWS, false);
    let c = |rc: Rc| rc.resolve(host.col, MAX_COLS, false);
    match *cref {
        CRef::Cell { row, col } => {
            let (y, x) = (r(row)?, c(col)?);
            Some(Rect::new(y, x, y, x))
        }
        CRef::Area { r0, c0, r1, c1 } => {
            let (a, b, x, y) = (r(r0)?, r(r1)?, c(c0)?, c(c1)?);
            Some(Rect::new(a.min(b), x.min(y), a.max(b), x.max(y)))
        }
        _ => None,
    }
}

fn enc(abs_index: u32, abs: bool, host: u32) -> Rc {
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

pub(crate) fn encode_rect(cref: &CRef, to: Rect, host: CellAddr) -> CRef {
    match *cref {
        CRef::Cell { row, col } => CRef::Cell {
            row: enc(to.r0, row.abs, host.row),
            col: enc(to.c0, col.abs, host.col),
        },
        CRef::Area { r0, c0, r1, c1 } => CRef::Area {
            r0: enc(to.r0, r0.abs, host.row),
            c0: enc(to.c0, c0.abs, host.col),
            r1: enc(to.r1, r1.abs, host.row),
            c1: enc(to.c1, c1.abs, host.col),
        },
        c => c,
    }
}

pub(crate) fn rebase(cref: &CRef, old: CellAddr, new: CellAddr) -> Option<CRef> {
    let re = |rc: Rc, oh: u32, nh: u32, max: u32| -> Option<Rc> {
        Some(enc(rc.resolve(oh, max, false)?, rc.abs, nh))
    };
    Some(match *cref {
        CRef::Cell { row, col } => CRef::Cell {
            row: re(row, old.row, new.row, MAX_ROWS)?,
            col: re(col, old.col, new.col, MAX_COLS)?,
        },
        CRef::Area { r0, c0, r1, c1 } => CRef::Area {
            r0: re(r0, old.row, new.row, MAX_ROWS)?,
            c0: re(c0, old.col, new.col, MAX_COLS)?,
            r1: re(r1, old.row, new.row, MAX_ROWS)?,
            c1: re(c1, old.col, new.col, MAX_COLS)?,
        },
        CRef::Cols { c0, c1 } => CRef::Cols {
            c0: re(c0, old.col, new.col, MAX_COLS)?,
            c1: re(c1, old.col, new.col, MAX_COLS)?,
        },
        CRef::Rows { r0, r1 } => CRef::Rows {
            r0: re(r0, old.row, new.row, MAX_ROWS)?,
            r1: re(r1, old.row, new.row, MAX_ROWS)?,
        },
    })
}

#[derive(Clone, Copy, Debug)]
pub(crate) struct Move {
    pub ss: SheetId,
    pub src: Rect,
    pub ds: SheetId,
    pub dr: i64,
    pub dc: i64,
}

impl Move {
    pub fn dest(&self) -> Rect {
        offset(&self.src, self.dr, self.dc)
    }

    pub fn adjust(&self, t: SheetId, r: &Rect) -> Adj {
        if self.ss == self.ds {
            if t != self.ss {
                return Adj::Keep;
            }
            return move_area(r, &self.src, self.dr, self.dc);
        }
        if t == self.ss {
            if inside(r, &self.src) {
                return Adj::Moved(offset(r, self.dr, self.dc));
            }
            return remove_band(r, &self.src);
        }
        if t == self.ds {
            return dest_side(r, &self.dest());
        }
        Adj::Keep
    }

    fn edit(&self, sheet: &SheetRef, cref: &CRef, old: CellAddr, new: CellAddr) -> RefEdit {
        let target = match sheet {
            SheetRef::Current => Some(old.sheet),
            SheetRef::Sheet(id) => Some(*id),
            _ => None,
        };

        let stay = |sheet: &SheetRef| stay_ref(sheet, cref, old, new);
        let (Some(t), Some(rect)) = (target, cref_rect(cref, old)) else {
            return stay(sheet);
        };
        let single = matches!(cref, CRef::Cell { .. });
        let adj = if single {
            let (r, c) = (rect.r0, rect.c0);
            if t == self.ss && self.src.contains(r, c) {
                Adj::Moved(offset(&rect, self.dr, self.dc))
            } else if t == self.ds && self.dest().contains(r, c) {
                Adj::Dead
            } else {
                Adj::Keep
            }
        } else {
            match self.adjust(t, &rect) {
                Adj::To(_) if new.sheet != old.sheet && t == self.ss => {
                    match rect.intersection(&self.src) {
                        Some(i) => Adj::Moved(offset(&i, self.dr, self.dc)),
                        None => Adj::Keep,
                    }
                }
                a => a,
            }
        };
        match adj {
            Adj::Keep => stay(sheet),
            Adj::Dead => RefEdit::Dead(sheet.clone()),
            Adj::To(to) => {
                let s = match sheet {
                    SheetRef::Current if new.sheet != old.sheet => SheetRef::Sheet(old.sheet),
                    s => s.clone(),
                };
                RefEdit::Set(s, encode_rect(cref, to, new))
            }
            Adj::Moved(to) => {
                let s = match sheet {
                    SheetRef::Current if new.sheet == self.ds => SheetRef::Current,
                    _ => SheetRef::Sheet(self.ds),
                };
                RefEdit::Set(s, encode_rect(cref, to, new))
            }
        }
    }
}

fn stay_ref(sheet: &SheetRef, cref: &CRef, old: CellAddr, new: CellAddr) -> RefEdit {
    let s = match sheet {
        SheetRef::Current if new.sheet != old.sheet => SheetRef::Sheet(old.sheet),
        s => s.clone(),
    };
    if old == new && s == *sheet {
        return RefEdit::Keep;
    }
    match rebase(cref, old, new) {
        Some(c) => RefEdit::Set(s, c),
        None => RefEdit::Keep,
    }
}

fn misses_host(r: &Rect, host: CellAddr) -> bool {
    let in_rows = (r.r0..=r.r1).contains(&host.row);
    let in_cols = (r.c0..=r.c1).contains(&host.col);
    match (r.rows() == 1, r.cols() == 1) {
        (true, true) => false,
        (false, true) => !in_rows,
        (true, false) => !in_cols,
        (false, false) => !(in_rows && in_cols),
    }
}

fn all_abs(cref: &CRef) -> bool {
    match *cref {
        CRef::Cell { row, col } => row.abs && col.abs,
        CRef::Area { r0, c0, r1, c1 } => r0.abs && c0.abs && r1.abs && c1.abs,
        _ => false,
    }
}

#[derive(Clone, Copy)]
pub(crate) struct PasteInfo {
    pub block: Rect,
    pub carries_fmt: bool,
    pub protect_blanks: bool,
}

impl Workbook {
    pub fn move_range(
        &mut self,
        src_sheet: SheetId,
        src: Rect,
        dst: CellAddr,
    ) -> Result<RecalcReport, EditError> {
        let r = self.move_range_body(src_sheet, src, dst);
        if r.is_ok() {
            self.view_touch_row_pub(src_sheet, src.r1);
            self.view_touch_row_pub(dst.sheet, dst.row.saturating_add(src.rows() - 1));
        }
        r
    }

    fn move_range_body(
        &mut self,
        src_sheet: SheetId,
        src: Rect,
        dst: CellAddr,
    ) -> Result<RecalcReport, EditError> {
        let d = Rect::new(
            dst.row,
            dst.col,
            dst.row.saturating_add(src.rows() - 1),
            dst.col.saturating_add(src.cols() - 1),
        );
        let pending = self.totals_pending(src_sheet, src) || self.totals_pending(dst.sheet, d);
        let r = self.move_range_raw(src_sheet, src, dst);
        self.select_paste(dst.sheet, dst.row, dst.col);
        if pending {
            self.restore_totals(src_sheet, src, Some(d));
            self.restore_totals(dst.sheet, d, Some(d));
        }

        if r.is_ok()
            && self
                .tables
                .iter()
                .any(|t| t.sheet == src_sheet || t.sheet == dst.sheet)
        {
            self.extra_recalc();
            self.extra_recalc();
        }
        r
    }

    fn move_range_raw(
        &mut self,
        src_sheet: SheetId,
        src: Rect,
        dst: CellAddr,
    ) -> Result<RecalcReport, EditError> {
        if self.sheet(src_sheet).is_none() || self.sheet(dst.sheet).is_none() {
            return Err(EditError::NoSuchSheet);
        }
        if dst.row as u64 + src.rows() as u64 > MAX_ROWS as u64
            || dst.col as u64 + src.cols() as u64 > MAX_COLS as u64
        {
            return Err(EditError::Compile(
                "paste area does not fit on the sheet".into(),
            ));
        }
        let mv = Move {
            ss: src_sheet,
            src,
            ds: dst.sheet,
            dr: dst.row as i64 - src.r0 as i64,
            dc: dst.col as i64 - src.c0 as i64,
        };
        if mv.ss == mv.ds && mv.dr == 0 && mv.dc == 0 {
            return Ok(RecalcReport::default());
        }
        self.names_follow(src_sheet, src, dst.sheet, false);
        let dest = mv.dest();

        let mut dt_moving: Vec<usize> = Vec::new();
        for (i, t) in self.data_tables.iter().enumerate() {
            if t.sheet == src_sheet && t.rect.intersects(&src) {
                let corner = src.intersection(&t.rect) == Some(Rect::cell(t.rect.r0, t.rect.c0));
                if corner {
                    continue;
                }
                if !inside(&t.rect, &src) {
                    let doomed: Vec<Rect> = self
                        .merges(dst.sheet)
                        .into_iter()
                        .filter(|m| m.intersects(&dest))
                        .collect();
                    for m in doomed {
                        self.remove_merge(dst.sheet, m);
                    }
                    return Err(EditError::Compile(crate::datatable::PART_ERROR.into()));
                }
                dt_moving.push(i);
            }
        }
        let mut dt_doomed: Vec<usize> = Vec::new();
        for (i, t) in self.data_tables.iter().enumerate() {
            let int = t.interior();
            if dt_moving.contains(&i) || t.sheet != dst.sheet || !int.intersects(&dest) {
                continue;
            }
            if !inside(&int, &dest) {
                return Err(EditError::Compile(crate::datatable::PART_ERROR.into()));
            }
            dt_doomed.push(i);
        }
        self.guard_arrays_move(src_sheet, src, dst.sheet, dest)?;
        self.move_merges(src_sheet, src, dst.sheet, dest)?;
        self.calc.spilling = self
            .sheets
            .iter()
            .flat_map(|sh| sh.spills.active_ids())
            .collect();
        for i in 0..self.data_tables.len() {
            if dt_moving.contains(&i) {
                let t = &mut self.data_tables[i];
                t.rect = offset(&t.rect, mv.dr, mv.dc);
                t.sheet = dst.sheet;
            }
        }
        let mut i = 0;
        self.data_tables.retain(|_| {
            i += 1;
            !dt_doomed.contains(&(i - 1))
        });
        self.fmt_move_block(src_sheet, src, dst.sheet, (dst.row, dst.col));

        let moved_blocked: Vec<FormulaId> = match self.sheet(src_sheet) {
            Some(sh) => sh
                .cells
                .iter_rect(src)
                .filter_map(|(_, _, e)| match e {
                    Entry::Formula(f) if sh.spills.is_blocked(*f) => Some(*f),
                    _ => None,
                })
                .collect(),
            None => Vec::new(),
        };

        let moving: Vec<(u32, u32, Entry)> = {
            let sh = self.sheet_mut(src_sheet).unwrap();
            let cells: Vec<(u32, u32)> = sh.cells.iter_rect(src).map(|(r, c, _)| (r, c)).collect();
            cells
                .into_iter()
                .filter_map(|(r, c)| sh.cells.remove(r, c).map(|e| (r, c, e)))
                .collect()
        };
        let overwritten: Vec<Entry> = {
            let sh = self.sheet_mut(dst.sheet).unwrap();
            let cells: Vec<(u32, u32)> = sh.cells.iter_rect(dest).map(|(r, c, _)| (r, c)).collect();
            cells
                .into_iter()
                .filter_map(|(r, c)| sh.cells.remove(r, c))
                .collect()
        };
        for e in overwritten {
            if let Entry::Formula(fid) = e {
                self.free_formula(fid);
            }
        }
        let mut new_addr: rustc_hash::FxHashMap<FormulaId, CellAddr> = Default::default();
        for (r, c, e) in moving {
            let to = CellAddr::new(
                dst.sheet,
                (r as i64 + mv.dr) as u32,
                (c as i64 + mv.dc) as u32,
            );
            if let Entry::Formula(fid) = &e {
                new_addr.insert(*fid, to);
            }
            self.sheet_mut(dst.sheet)
                .unwrap()
                .cells
                .set(to.row, to.col, e);
        }

        self.calc.spilling.retain(|f| !new_addr.contains_key(f));

        let ids: Vec<FormulaId> = self.formula_ids().collect();
        for fid in ids {
            let (old, formula) = {
                let f = self.formula(fid);
                (f.addr, f.formula.clone())
            };
            let new = new_addr.get(&fid).copied().unwrap_or(old);

            let mut plain = Vec::new();
            edit_refs_at(
                &formula.root,
                &mut |sheet, cref, at| {
                    if !at {
                        plain.push((sheet.clone(), cref_rect(cref, old)));
                    }
                    RefEdit::Keep
                },
                false,
            );
            let root = edit_refs_at(
                &formula.root,
                &mut |sheet, cref, at| {
                    if at
                        && !matches!(cref, CRef::Cell { .. })
                        && cref_rect(cref, old).is_some_and(|r| {
                            misses_host(&r, old) && !plain.contains(&(sheet.clone(), Some(r)))
                        })
                    {
                        return stay_ref(sheet, cref, old, new);
                    }
                    mv.edit(sheet, cref, old, new)
                },
                false,
            );
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

        let names: Vec<_> = self.names.iter().cloned().collect();
        for mut def in names {
            let host = def.host();
            let mut changed = false;
            let root = edit_refs(&def.formula.root, &mut |sheet, cref| {
                if !all_abs(cref) || matches!(sheet, SheetRef::Current) {
                    return RefEdit::Keep;
                }
                let e = mv.edit(sheet, cref, host, host);
                changed |= !matches!(e, RefEdit::Keep);
                e
            });
            if changed {
                def.refers_to = print_node(self, &root, host);
                def.formula = Arc::new(Formula {
                    root,
                    volatile: def.formula.volatile,
                });
                self.names.insert(def);
            }
        }

        let mut headers: Vec<CellAddr> = Vec::new();
        for t in self.tables.iter_mut() {
            if t.sheet == src_sheet && inside(&t.rect, &src) {
                t.rect = offset(&t.rect, mv.dr, mv.dc);
                t.sheet = dst.sheet;
                continue;
            }

            if !t.headers {
                continue;
            }
            for c in t.rect.c0..=t.rect.c1 {
                let a = CellAddr::new(t.sheet, t.rect.r0, c);
                if (t.sheet == src_sheet && src.contains(a.row, a.col))
                    || (t.sheet == dst.sheet && dest.contains(a.row, a.col))
                {
                    headers.push(a);
                }
            }
        }

        for t in self.tables.iter_mut() {
            if t.sheet != dst.sheet
                || !t.headers
                || !dest.contains(t.rect.r0, dest.c0.max(t.rect.c0))
            {
                continue;
            }
            for c in dest.c0.max(t.rect.c0)..=dest.c1.min(t.rect.c1) {
                if let Some(name) = t.columns.get((c - t.rect.c0) as usize) {
                    let k: String = name.chars().map(crate::text::fold_upper).collect();
                    t.dxf.retain(|(n, _)| *n != k);
                }
            }
        }
        self.note_pasted_col_formats(dst.sheet, dest);
        let grow = self.paste_growth(
            dst.sheet,
            dest,
            Some((src_sheet, src)),
            None,
            None,
            self.sheet_count() == 1,
        );
        let mut report = self.relink_and_recalc();
        if self.settings.mode == crate::workbook::CalcMode::Manual {
            for fid in moved_blocked {
                let (addr, spill) = {
                    let f = self.formula(fid);
                    (f.addr, f.spill)
                };
                if let Some(rect) = spill {
                    if let Some(sh) = self.sheet_mut(addr.sheet) {
                        sh.spills.set_blocked(fid, rect);
                    }
                    let f = self.formula_mut(fid);
                    f.spill = None;
                    f.value = crate::value::Value::Error(crate::value::XlError::Spill);
                    self.set_dirty(fid, true);
                    self.calc.dirty_list.push(fid);
                }
            }
        }
        if !headers.is_empty() {
            headers.sort_by_key(|a| (a.sheet.0, a.row, a.col));
            for a in headers {
                let pasted = (a.sheet == dst.sheet && dest.contains(a.row, a.col))
                    || self.tables.iter().any(|t| {
                        t.totals
                            && t.sheet == a.sheet
                            && t.header_row() == Some(a.row)
                            && (t.rect.c0..=t.rect.c1).contains(&a.col)
                    });
                if pasted && matches!(self.value(a), crate::value::Value::Blank) {
                    let next = self.tables.iter().find_map(|t| {
                        (t.sheet == a.sheet
                            && t.header_row() == Some(a.row)
                            && t.rect.c0 < a.col
                            && a.col <= t.rect.c1)
                            .then(|| {
                                let i = (a.col - t.rect.c0) as usize;
                                let others: Vec<String> = t
                                    .columns
                                    .iter()
                                    .enumerate()
                                    .filter(|(j, _)| *j != i)
                                    .map(|(_, c)| c.clone())
                                    .collect();
                                crate::workbook::tables::next_header_name(
                                    &t.columns[i - 1],
                                    &others,
                                )
                            })
                            .flatten()
                    });
                    if let Some(n) = next {
                        self.sheet_mut(a.sheet).unwrap().cells.set(
                            a.row,
                            a.col,
                            crate::workbook::Entry::Const(crate::value::Value::from(n.as_str())),
                        );
                        crate::calc::cell_changed(self, a);
                    }
                }
                self.header_edited_in_cut(a);
            }
            report = self.relink_and_recalc();
        }
        if let Some((ti, rect, added_rows, _)) = grow {
            report = self.grow_table_by_paste(ti, rect, added_rows, None);
        }
        Ok(report)
    }

    pub(crate) fn paste_growth(
        &self,
        sheet: SheetId,
        d: Rect,
        cut_from: Option<(SheetId, Rect)>,
        orig: Option<&[Rect]>,
        copied_from: Option<(SheetId, Rect)>,
        active: bool,
    ) -> Option<(usize, Rect, bool, bool)> {
        self.tables.iter().enumerate().find_map(|(i, t)| {
            if t.sheet != sheet {
                return None;
            }
            let r = t.rect;

            let r_old = orig.and_then(|o| o.get(i)).copied().unwrap_or(r);
            let mut rect = r;
            let rows = !t.totals
                && r.c0 <= d.c0
                && d.c1 <= r.c1
                && d.r0 > r.r0
                && d.r0 <= r.r1 + 1
                && d.r1 > r.r1;
            let cols =
                r.r0 <= d.r0 && d.r1 <= r.r1 && d.c0 > r.c0 && d.c0 <= r.c1 + 1 && d.c1 > r.c1;
            if rows {
                rect.r1 = d.r1;
            } else if cols {
                rect.c1 = d.c1;
            } else {
                return None;
            }
            let clash = self
                .tables
                .iter()
                .enumerate()
                .any(|(j, o)| j != i && o.sheet == sheet && o.rect.intersects(&rect));
            if clash {
                return None;
            }

            let added = if rows {
                Rect::new(r.r1 + 1, r.c0, rect.r1, r.c1)
            } else {
                Rect::new(r.r0, r.c1 + 1, r.r1, rect.c1)
            };
            let sh = self.sheet(sheet)?;

            let past = if rows {
                Rect::new(r_old.r1 + 1, d.c0, d.r1, d.c1)
            } else {
                Rect::new(d.r0, r_old.c1 + 1, d.r1, d.c1)
            };
            let edge_nonblank = sh.cells.iter_rect(past).next().is_some();

            let from_table = cut_from.is_some_and(|(s, c)| s == sheet && c.intersects(&r));

            let inside = if rows { d.r0 <= r.r1 } else { d.c0 <= r.c1 };

            let outside_src = !copied_from.is_some_and(|(s, c)| s == sheet && c.intersects(&r));
            if !from_table && !(inside && rows && outside_src && active) {
                sh.cells.iter_rect(past).next()?;
            }

            let busy = !(from_table && inside)
                && sh.cells.iter_rect(added).any(|(y, x, _)| !d.contains(y, x));
            (!busy).then_some((i, rect, rows, edge_nonblank))
        })
    }

    pub(crate) fn table_selected(&self, ti: usize) -> bool {
        let t = &self.tables[ti];
        let cell = if t.sheet == self.active {
            Some(self.selection.get(&t.sheet).copied().unwrap_or((0, 0)))
        } else {
            self.fill_selected.get(&t.sheet).copied()
        };
        cell.is_some_and(|(r, c)| t.rect.contains(r, c))
    }

    fn first_busy_row(&self, sheet: SheetId, area: Rect) -> Option<u32> {
        let mut first = self
            .sheet(sheet)
            .and_then(|sh| sh.cells.iter_rect(area).map(|(y, _, _)| y).min());
        let ids: Vec<FormulaId> = self.formula_ids().collect();
        for fid in ids {
            let f = self.formula(fid);
            let host = f.addr;
            edit_refs(&f.formula.root, &mut |s, cref| {
                let t = match s {
                    SheetRef::Current => host.sheet,
                    SheetRef::Sheet(id) => *id,
                    _ => return RefEdit::Keep,
                };
                if t == sheet {
                    let rect = match *cref {
                        CRef::Cols { c0, c1 } => {
                            let c = |rc: Rc| rc.resolve(host.col, MAX_COLS, false);
                            c(c0)
                                .zip(c(c1))
                                .map(|(a, b)| Rect::new(0, a.min(b), MAX_ROWS - 1, a.max(b)))
                        }
                        CRef::Rows { r0, r1 } => {
                            let r = |rc: Rc| rc.resolve(host.row, MAX_ROWS, false);
                            r(r0)
                                .zip(r(r1))
                                .map(|(a, b)| Rect::new(a.min(b), 0, a.max(b), MAX_COLS - 1))
                        }
                        _ => cref_rect(cref, host),
                    };
                    if let Some(r) = rect {
                        if r.intersects(&area) {
                            let y = r.r0.max(area.r0);
                            first = Some(first.map_or(y, |f: u32| f.min(y)));
                        }
                    }
                }
                RefEdit::Keep
            });
        }
        first
    }

    pub(crate) fn fill_grow_tables(
        &mut self,
        sheet: SheetId,
        start: u32,
        range: Rect,
        written: &[Rect],
    ) {
        let reaches =
            |r: Rect, w: Rect| w.r0 <= r.r1 + 1 && w.r1 > r.r1 && w.c0 <= r.c1 && w.c1 >= r.c0;
        let hits: Vec<usize> = self
            .tables
            .iter()
            .enumerate()
            .filter(|(_, t)| {
                t.sheet == sheet && !t.totals && start < t.rect.r1 && reaches(t.rect, range)
            })
            .map(|(i, _)| i)
            .collect();
        for ti in hits {
            let old = self.tables[ti].rect;
            let active = self.table_selected(ti);
            if active {
                let area = Rect::new(old.r1 + 1, old.c0, range.r1, old.c1);
                if let Some(first) = self.first_busy_row(sheet, area) {
                    let add = range.r1 - first + 1;
                    let ins = Rect::new(old.r1 + 1, old.c0, old.r1 + add, old.c1);
                    if self.insert_cells(sheet, ins, true).is_err() {
                        continue;
                    }
                }
            }
            let grow = written
                .iter()
                .filter(|w| reaches(old, **w))
                .map(|w| w.r1 - old.r1)
                .max();
            if let Some(g) = grow {
                if !active {
                    let added = Rect::new(old.r1 + 1, old.c0, old.r1 + g, old.c1);
                    let blocked = self.sheet(sheet).is_some_and(|sh| {
                        sh.cells
                            .iter_rect(added)
                            .any(|(_, x, _)| !written.iter().any(|w| w.c0 <= x && x <= w.c1))
                    });
                    if blocked {
                        continue;
                    }
                }
                let grown = Rect::new(old.r0, old.c0, old.r1 + g, old.c1);
                self.grow_table(ti, grown, true, true, None);
            }
        }
    }

    pub(crate) fn grow_exact_refs(
        &mut self,
        ti: usize,
        old: Rect,
        add: u32,
        header_too: bool,
    ) -> bool {
        let mut any = false;

        let sheet = self.tables[ti].sheet;
        let (d0, d1) = (self.tables[ti].data_start(), old.r1);
        let ids: Vec<FormulaId> = self.formula_ids().collect();
        for fid in ids {
            let (host, formula) = {
                let f = self.formula(fid);
                (f.addr, f.formula.clone())
            };
            let mut changed = false;
            let root = edit_refs(&formula.root, &mut |s, cref| {
                let t = match s {
                    SheetRef::Current => host.sheet,
                    SheetRef::Sheet(id) => *id,
                    _ => return RefEdit::Keep,
                };
                if t != sheet || !matches!(cref, CRef::Area { .. }) {
                    return RefEdit::Keep;
                }
                match cref_rect(cref, host) {
                    Some(r)
                        if (r.r0 == d0 || (header_too && r.r0 == old.r0))
                            && r.r1 == d1
                            && old.c0 <= r.c0
                            && r.c1 <= old.c1 =>
                    {
                        changed = true;
                        RefEdit::Set(
                            s.clone(),
                            encode_rect(cref, Rect::new(r.r0, r.c0, r.r1 + add, r.c1), host),
                        )
                    }
                    _ => RefEdit::Keep,
                }
            });
            if changed {
                any = true;
                let text = print_node(self, &root, host);
                let f = self.formula_mut(fid);
                f.formula = Arc::new(Formula {
                    root,
                    volatile: formula.volatile,
                });
                f.text = text;
            }
        }
        any
    }

    pub(crate) fn grow_table_by_paste(
        &mut self,
        ti: usize,
        rect: Rect,
        rows: bool,
        pasted: Option<PasteInfo>,
    ) -> RecalcReport {
        if rows {
            let t = &self.tables[ti];
            let (sheet, old) = (t.sheet, t.rect);
            let mut grown = old;
            for r in old.r1 + 1..=rect.r1 {
                grown.r1 = r;
                self.tables[ti].rect = grown;
                self.extend_row_formats(sheet, grown);
            }
            self.tables[ti].rect = old;
        }
        self.grow_table(ti, rect, rows, false, pasted)
    }

    pub(crate) fn grow_table(
        &mut self,
        ti: usize,
        rect: Rect,
        rows: bool,
        header_too: bool,
        pasted: Option<PasteInfo>,
    ) -> RecalcReport {
        let old = self.tables[ti].rect;
        let sheet = self.tables[ti].sheet;
        let name = self.tables[ti].name.clone();
        self.tables[ti].rect = rect;
        if rows {
            let _ = self.grow_exact_refs(ti, old, rect.r1 - old.r1, header_too);

            let mut filled: Vec<CellAddr> = Vec::new();
            let t = self.tables[ti].clone();
            let (d0, _) = t.data_rows();
            for (idx, &is_calc) in t.calc.iter().enumerate() {
                if !is_calc {
                    continue;
                }
                let col = t.rect.c0 + idx as u32;
                let order: Vec<u32> = (d0..=old.r1).collect();
                let Some(sr) = self.calc_source_row(&t, idx, &order) else {
                    if let Some(snap) = self.calc_snapshot(&t, idx) {
                        for r in old.r1 + 1..=rect.r1 {
                            let to = CellAddr::new(sheet, r, col);
                            if !pasted.is_some_and(|p| p.protect_blanks && p.block.contains(r, col))
                                && self
                                    .sheet(sheet)
                                    .is_some_and(|s| s.cells.get(r, col).is_none())
                            {
                                let text = print_node(self, &snap.root, to);
                                self.set_compiled_formula(to, snap.clone(), text);
                                filled.push(to);
                            }
                        }
                    }
                    continue;
                };
                let Some(sf) = self.formula_at(CellAddr::new(sheet, sr, col)) else {
                    continue;
                };
                let from = Rect::new(sr, col, sr, col);
                for r in old.r1 + 1..=rect.r1 {
                    let to = CellAddr::new(sheet, r, col);
                    let empty = self
                        .sheet(sheet)
                        .is_some_and(|s| s.cells.get(r, col).is_none());

                    if let Some(p) = pasted.filter(|p| p.block.contains(r, col)) {
                        if !empty || p.protect_blanks {
                            if !p.carries_fmt {
                                self.fmt_copy_pairs(&[(CellAddr::new(sheet, sr, col), to)]);
                            }
                            continue;
                        }
                    }
                    if empty && self.fill_fits(sf, sheet, col, r..=r) {
                        let _ = self.copy_range(sheet, from, to);
                        filled.push(to);
                    }
                }
            }
            let mut report = self.relink_and_recalc();

            if self.settings.mode != crate::workbook::CalcMode::Automatic && !filled.is_empty() {
                let r = crate::calc::recalc_only(self, &filled);
                report.evaluations += r.evaluations;
                report.changed.extend(r.changed);
            }
            return report;
        }
        for c in old.c1 + 1..=rect.c1 {
            let hdr = CellAddr::new(sheet, rect.r0, c);

            let next = match self.value(hdr) {
                crate::value::Value::Blank => self.tables[ti].columns.last().and_then(|prev| {
                    crate::workbook::tables::next_header_name(prev, &self.tables[ti].columns)
                }),
                _ => None,
            };
            self.tables[ti].columns.push(String::new());
            self.tables[ti].calc.push(false);
            if let Some(n) = next {
                self.set_value(hdr, crate::value::Value::from(n.as_str()));
            }
            self.header_edited(hdr);

            let first = (rect.r0 + 1..=rect.r1).find_map(|r| {
                let a = CellAddr::new(sheet, r, c);
                self.formula_at(a).map(|_| a)
            });
            if let Some(a) = first {
                self.table_calc_fill(a);
            }
        }
        self.table_changed(&name)
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    fn col(a: u32, b: u32) -> Rect {
        Rect::new(a, 23, b, 23)
    }

    #[test]
    fn band_moves() {
        let s = col(6, 7);

        assert_eq!(move_area(&col(0, 7), &s, -6, 0), Adj::To(col(0, 5)));

        assert_eq!(move_area(&col(2, 7), &s, -6, 0), Adj::Keep);

        assert_eq!(move_area(&col(7, 11), &s, 3, 0), Adj::To(col(10, 11)));

        assert_eq!(move_area(&col(5, 6), &s, -1, 0), Adj::To(col(5, 5)));
    }

    #[test]
    fn corner_rule() {
        let a = |x: &str| crate::address::parse_a1_rect(x).unwrap();

        assert_eq!(
            move_area(&a("G4:H5"), &a("E5:G6"), -2, 1),
            Adj::To(a("H5:H5"))
        );

        assert_eq!(
            move_area(&a("F3:H5"), &a("E5:G6"), -2, 1),
            Adj::To(a("G3:H5"))
        );

        assert_eq!(
            move_area(&a("D1:E5"), &a("E5:G7"), -4, -2),
            Adj::To(a("C1:D1"))
        );
        assert_eq!(
            move_area(&a("C2:E5"), &a("E5:G7"), -4, -2),
            Adj::To(a("C1:C1"))
        );

        assert_eq!(move_area(&a("E2:F6"), &a("E6:G6"), -2, -2), Adj::Keep);

        assert_eq!(move_area(&a("C3:E5"), &a("E5:G7"), -2, -2), Adj::Dead);
        assert_eq!(
            move_area(&a("C5:E5"), &a("E5:G7"), -2, -2),
            Adj::To(a("C3:C3"))
        );
    }
}

#[cfg(test)]
mod grid {
    use super::*;

    fn parse(f: &str) -> Option<Rect> {
        let body = f.strip_prefix("=SUM(")?.strip_suffix(')')?;
        if body.contains("#REF!") {
            return None;
        }
        Some(crate::address::parse_a1_rect(body).unwrap())
    }

    fn engine(r: &Rect, s: &Rect, dr: i64, dc: i64) -> Option<Rect> {
        let single = r.rows() == 1 && r.cols() == 1;
        let d = offset(s, dr, dc);
        let adj = if single {
            if s.contains(r.r0, r.c0) {
                Adj::Moved(offset(r, dr, dc))
            } else if d.contains(r.r0, r.c0) {
                Adj::Dead
            } else {
                Adj::Keep
            }
        } else {
            move_area(r, s, dr, dc)
        };
        match adj {
            Adj::Keep => Some(*r),
            Adj::Dead => None,
            Adj::To(x) | Adj::Moved(x) => Some(x),
        }
    }

    fn show(r: Option<Rect>) -> String {
        match r {
            None => "#REF!".into(),
            Some(r) => format!(
                "{}{}:{}{}",
                crate::address::col_to_letters(r.c0),
                r.r0 + 1,
                crate::address::col_to_letters(r.c1),
                r.r1 + 1
            ),
        }
    }

    #[test]
    #[ignore]
    fn cut_grid() {
        let Ok(path) = std::env::var("CUTGRID") else {
            return;
        };
        let text = std::fs::read_to_string(path).unwrap();
        let mut lines = text.lines();
        let head: Vec<u32> = lines
            .next()
            .unwrap()
            .split(' ')
            .skip(2)
            .take(4)
            .map(|x| x.parse().unwrap())
            .collect();
        let s = Rect::new(head[0] - 1, head[1] - 1, head[2] - 1, head[3] - 1);
        let (mut ok, mut bad) = (0, 0);
        let mut shown = 0;
        for l in lines {
            let p: Vec<&str> = l.splitn(7, ' ').collect();
            let n: Vec<i64> = p[..6].iter().map(|x| x.parse().unwrap()).collect();
            let r = Rect::new(
                n[2] as u32 - 1,
                n[3] as u32 - 1,
                n[4] as u32 - 1,
                n[5] as u32 - 1,
            );
            let want = parse(p[6]);
            let got = engine(&r, &s, n[0], n[1]);
            if want == got {
                ok += 1;
            } else {
                bad += 1;
                if shown < 100000 {
                    shown += 1;
                    println!(
                        "d=({},{}) S={} R={} want {} got {}",
                        n[0],
                        n[1],
                        show(Some(s)),
                        show(Some(r)),
                        show(want),
                        show(got)
                    );
                }
            }
        }
        println!("ok {ok} bad {bad}");
    }
}
