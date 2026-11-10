use xlcalc::{paste_area, CellAddr, Notice, Rect, Selection, SheetId};
use xlcalc_variant::Variant;

use crate::app::{unsupported, Application, BookId, Mode};
use crate::conv::Args;
use crate::error::OmError;
use crate::objects::edit_error;
use crate::typelib::Member;

#[derive(Clone, Copy)]
pub(crate) enum At {
    First,

    BeforeChange,

    BeforeAfterCalc,

    Last,
}

impl Application<'_> {
    pub(crate) fn notice_mark(&self, b: BookId) -> usize {
        self.book(b).map_or(0, |bk| bk.wb.notices().len())
    }

    pub(crate) fn note_selection(
        &mut self,
        b: BookId,
        mark: usize,
        at: At,
        s: SheetId,
        areas: &[Rect],
    ) {
        let Ok(bk) = self.book_mut(b) else {
            return;
        };
        let log = bk.wb.notices_mut();
        let mark = mark.min(log.len());
        let after_calc = |log: &[Notice]| {
            log[mark..]
                .iter()
                .rposition(|n| matches!(n, Notice::AfterCalculate))
                .map_or(log.len(), |i| mark + i)
        };
        let i = match at {
            At::First => mark,
            At::BeforeChange => log[mark..]
                .iter()
                .position(|n| matches!(n, Notice::Changed { .. } | Notice::ChangedAsOne { .. }))
                .map_or_else(|| after_calc(log), |i| mark + i),
            At::BeforeAfterCalc => after_calc(log),
            At::Last => log.len(),
        };
        log.insert(
            i,
            Notice::SelectionChanged {
                sheet: s,
                areas: areas.to_vec(),
            },
        );
    }

    pub(crate) fn select(&mut self, b: BookId, s: SheetId, sel: Selection) -> Result<(), OmError> {
        let wb = &mut self.book_mut(b)?.wb;
        let changed = wb.selection_of(s).areas != sel.areas;
        let areas = sel.areas.clone();
        wb.set_selection(s, sel).map_err(edit_error)?;
        if changed {
            let mark = self.notice_mark(b);
            self.note_selection(b, mark, At::Last, s, &areas);
        }
        Ok(())
    }

    fn is_shown(&self, b: BookId, s: SheetId) -> bool {
        self.active == Some(b) && self.book(b).is_ok_and(|bk| bk.wb.active_sheet() == s)
    }

    pub(crate) fn range_select(
        &mut self,
        b: BookId,
        s: SheetId,
        rects: &[Rect],
        m: &'static Member,
    ) -> Result<Variant, OmError> {
        if !self.is_shown(b, s) {
            return Err(OmError::app_defined_with(&format!(
                "{} method of Range class failed",
                m.name
            )));
        }
        let first = rects
            .first()
            .copied()
            .ok_or_else(OmError::object_required)?;
        let mut sel = self.book(b)?.wb.selection_of(s);
        if m.name == "Activate" && sel.contains(first.r0, first.c0) {
            sel.active = (first.r0, first.c0);
            self.book_mut(b)?
                .wb
                .set_selection(s, sel)
                .map_err(edit_error)?;
        } else {
            self.select(b, s, Selection::of(rects))?;
        }
        Ok(Variant::Bool(true))
    }

    pub(crate) fn app_selection(&mut self) -> Result<Variant, OmError> {
        let b = self.active_book()?;
        let (id, sheet) = (b.id, b.wb.active_sheet());
        let sel = b.wb.selection_of(sheet);
        let areas = match sel.areas.as_slice() {
            [one] if one.cell_count() == 1 => {
                vec![b
                    .wb
                    .merge_at(CellAddr::new(sheet, one.r0, one.c0))
                    .unwrap_or(*one)]
            }
            _ => sel.areas,
        };
        self.new_multi(id, sheet, &areas, Mode::Cells)
    }

    pub(crate) fn app_goto(&mut self, a: &Args) -> Result<Variant, OmError> {
        let Some(v @ Variant::Dispatch(Some(_))) = a.get(0) else {
            return Err(OmError::unsupported("Application.Goto without a Range"));
        };
        let (b, s, rects) = self.range_areas(Some(v))?;
        self.select(b, s, Selection::of(&rects))?;
        self.book_mut(b)?
            .wb
            .set_active_sheet(s)
            .map_err(edit_error)?;
        self.active = Some(b);
        Ok(Variant::Bool(true))
    }

    pub(crate) fn with_sheet_shown<R>(
        &mut self,
        b: BookId,
        s: SheetId,
        stay_on_err: bool,
        f: impl FnOnce(&mut Self) -> Result<R, OmError>,
    ) -> Result<R, OmError> {
        let was = self.book(b)?.wb.active_sheet();
        if was == s {
            return f(self);
        }
        self.book_mut(b)?
            .wb
            .set_active_sheet(s)
            .map_err(edit_error)?;
        let r = f(self);

        if stay_on_err && r.is_err() {
            return r;
        }
        if let Ok(bk) = self.book_mut(b) {
            if bk.wb.sheet(was).is_some() {
                let _ = bk.wb.set_active_sheet(was);
            }
        }
        r
    }

    pub(crate) fn sheet_paste(
        &mut self,
        b: BookId,
        s: SheetId,
        m: &'static Member,
        a: &Args,
    ) -> Result<Variant, OmError> {
        let Some((cb, ss, src)) = self.clip else {
            return Err(OmError::app_defined_with(
                "Paste method of Worksheet class failed",
            ));
        };
        if cb != b || a.bool(1)?.unwrap_or(false) {
            return Err(unsupported("Worksheet", m));
        }
        let dest = match a.get(0) {
            Some(v @ Variant::Dispatch(Some(_))) => {
                let (db, ds, rects) = self.range_areas(Some(v))?;
                match rects.as_slice() {
                    [one] if db == b => Some((ds, *one)),
                    _ => return Err(unsupported("Worksheet", m)),
                }
            }
            _ => None,
        };
        self.with_sheet_shown(b, s, false, |app| {
            let (ds, dr) = match dest {
                Some(d) => d,
                None => match app.book(b)?.wb.selection_of(s).areas.as_slice() {
                    [one] => (s, *one),
                    _ => return Err(unsupported("Worksheet", m)),
                },
            };
            if let Err(e) = app.paste_copy(b, (ss, src), (ds, dr), false) {
                if dest.is_none() {
                    app.book_mut(b)?
                        .wb
                        .set_selection(ds, Selection::of(&[paste_area(src, dr)]))
                        .map_err(edit_error)?;
                }
                return Err(e);
            }
            if dest.is_none() {
                let pasted = paste_area(src, dr);
                app.book_mut(b)?
                    .wb
                    .set_selection(ds, Selection::of(&[pasted]))
                    .map_err(edit_error)?;
            }
            Ok(Variant::Bool(true))
        })
    }

    pub(crate) fn paste_copy(
        &mut self,
        b: BookId,
        (ss, src): (SheetId, Rect),
        (ds, dr): (SheetId, Rect),
        cut: bool,
    ) -> Result<(), OmError> {
        let mark = self.notice_mark(b);
        let area = if cut {
            Rect::new(dr.r0, dr.c0, dr.r0 + src.rows() - 1, dr.c0 + src.cols() - 1)
        } else {
            paste_area(src, dr)
        };
        self.paste_guard(b, (ss, src), (ds, area), cut)?;

        if cut && dr.rows() * dr.cols() > 1 {
            let wb = &self.book(b)?.wb;
            if wb.filter_mode(ds) && wb.hidden_rows(ds).iter().any(|r| *r >= 1) {
                let vis: Vec<u32> = (dr.r0..=dr.r1).filter(|r| !wb.row_hidden(ds, *r)).collect();
                if vis.len() > 1 && vis.windows(2).any(|w| w[1] != w[0] + 1) {
                    return Err(OmError::app_defined_with(
                        "This can't be done on a multiple range selection.\n\nSelect a single range and try again.",
                    ));
                }
                if vis.len() as u32 != src.rows() || dr.cols() != src.cols() {
                    return Err(OmError::app_defined_with(
                        "You can't paste this here because the Cut area and paste area aren't the same size.\n\nSelect just one cell in the paste area or an area that's the same size, and try pasting again.",
                    ));
                }
            }
        }

        let kept = if cut {
            self.locked_snapshot(b, ss, src)
        } else {
            self.locked_snapshot(b, ds, area)
        };
        let wb = &mut self.book_mut(b)?.wb;
        let pasted = if cut {
            wb.move_range(ss, src, CellAddr::new(ds, dr.r0, dr.c0))
                .map_err(edit_error)?;
            Rect::new(dr.r0, dr.c0, dr.r0 + src.rows() - 1, dr.c0 + src.cols() - 1)
        } else {
            wb.copy_to_range(ss, src, ds, dr).map_err(edit_error)?;
            paste_area(src, dr)
        };
        self.book_mut(b)?.wb.hl_paste(ss, src, ds, pasted, cut);
        self.book_mut(b)?.wb.notes_paste(ss, src, ds, pasted, cut);
        self.restore_locked(b, &kept);
        if pasted != dr {
            self.note_selection(b, mark, At::BeforeAfterCalc, ds, &[pasted]);
        }
        Ok(())
    }
}
