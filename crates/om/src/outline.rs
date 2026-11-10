use xlcalc::{Rect, SheetId};
use xlcalc_variant::Variant;

use crate::addr::{MAX_COLS, MAX_ROWS};
use crate::app::{unsupported, Application, BookId, Mode, Object, RangeRef};
use crate::conv::Args;
use crate::error::OmError;
use crate::typelib::{InvokeKind, Member};

const CREATOR: i32 = 0x5843_454C;
const XL_LEFT: i32 = -4131;
const XL_RIGHT: i32 = -4152;

pub(crate) fn group_axis(rect: Rect, mode: Mode) -> bool {
    let rows = rect.c0 == 0 && rect.c1 == MAX_COLS - 1;
    let cols = rect.r0 == 0 && rect.r1 == MAX_ROWS - 1;
    match (mode, rows, cols) {
        (Mode::Columns, _, true) => false,
        (_, true, _) => true,
        _ => false,
    }
}

fn level_arg(a: &Args, i: usize) -> Result<u32, OmError> {
    let n = match a.get(i) {
        None => 0,
        Some(Variant::Bool(b)) => i32::from(*b),
        Some(_) => a.f64(i)?.map_or(0, |x| x.trunc() as i32),
    };
    u32::try_from(n)
        .ok()
        .filter(|n| *n <= 8)
        .ok_or_else(OmError::app_defined)
}

impl Application<'_> {
    pub(crate) fn outline_member(
        &mut self,
        b: BookId,
        s: SheetId,
        m: &'static Member,
        a: &Args,
    ) -> Result<Variant, OmError> {
        use InvokeKind::*;
        let (above, left, styles) = self.book(b)?.wb.outline_settings(s);
        match (m.name, m.kind) {
            ("SummaryRow", Get) => Ok(Variant::I4(i32::from(!above))),
            ("SummaryColumn", Get) => Ok(Variant::I4(if left { XL_LEFT } else { XL_RIGHT })),
            ("AutomaticStyles", Get) => Ok(Variant::Bool(styles)),
            ("SummaryRow" | "SummaryColumn", Put) => {
                if matches!(a.put, Some(Variant::Bstr(_))) {
                    return Err(OmError::unable_to_set(m.name, "Outline"));
                }
                let x = self.put_f64(a)?;
                let wb = &mut self.book_mut(b)?.wb;
                if m.name == "SummaryRow" {
                    wb.set_outline_settings(s, x == 0.0, left, styles);
                } else {
                    wb.set_outline_settings(s, above, x == 0.0 || x == f64::from(XL_LEFT), styles);
                }
                Ok(Variant::Empty)
            }
            ("AutomaticStyles", Put) => {
                let on = Application::put_value(a)
                    .bool(0)
                    .map_err(|_| OmError::unable_to_set("AutomaticStyles", "Outline"))?
                    .unwrap_or(false);
                self.book_mut(b)?
                    .wb
                    .set_outline_settings(s, above, left, on);
                Ok(Variant::Empty)
            }
            ("ShowLevels", Func) => {
                if a.get(0).is_none() && a.get(1).is_none() {
                    return Err(OmError::app_defined());
                }
                let rows = level_arg(a, 0)?;
                let cols = level_arg(a, 1)?;
                self.book_mut(b)?.wb.outline_show_levels(s, rows, cols);
                Ok(Variant::Bool(true))
            }
            ("Application", Get) => Ok(Variant::Dispatch(Some(self.handle()))),
            ("Creator", Get) => Ok(Variant::I4(CREATOR)),
            ("Parent", Get) => Ok(self.alloc(Object::Worksheet(b, s))),
            _ => Err(unsupported("Outline", m)),
        }
    }

    pub(crate) fn range_outline_member(
        &mut self,
        r: &RangeRef,
        sheet: SheetId,
        rect: Rect,
        m: &'static Member,
        a: &Args,
    ) -> Result<Variant, OmError> {
        use InvokeKind::*;
        let book = r.book;
        if m.name == "ShowDetail" && rect.cell_count() == 1 && r.mode == Mode::Cells {
            let addr = xlcalc::CellAddr::new(sheet, rect.r0, rect.c0);
            let wb = &self.book(book)?.wb;
            if let Some(rows) = wb.pivot_detail_at(addr) {
                if m.kind == Get || !self.put_bool(a)? {
                    return Err(OmError::app_defined());
                }
                return self.pivot_drill_down(book, rows);
            }
            if m.kind == Get && wb.pivot_label_at(addr).is_some() {
                return Ok(Variant::Bool(true));
            }
        }
        match (m.name, m.kind) {
            ("Group" | "Ungroup", Func) => {
                if a.get(1).is_some() {
                    return Err(OmError::app_defined());
                }
                let rows = group_axis(rect, r.mode);
                let (at, count) = if rows {
                    (rect.r0, rect.rows())
                } else {
                    (rect.c0, rect.cols())
                };
                self.book_mut(book)?
                    .wb
                    .group_axis(sheet, rows, at, count, m.name == "Group")
                    .map_err(|_| OmError::app_defined())?;
                Ok(Variant::Bool(true))
            }
            ("ShowDetail", _) => {
                let rows = group_axis(rect, r.mode);
                let single = if rows { rect.rows() } else { rect.cols() } == 1;
                let whole = if rows {
                    rect.c0 == 0 && rect.c1 == MAX_COLS - 1
                } else {
                    rect.r0 == 0 && rect.r1 == MAX_ROWS - 1
                };
                if !single || !whole {
                    return Err(OmError::app_defined());
                }
                let idx = if rows { rect.r0 } else { rect.c0 };
                if m.kind == Get {
                    return Ok(Variant::Bool(
                        self.book(book)?.wb.detail_shown(sheet, rows, idx),
                    ));
                }
                let show = self.put_bool(a)?;
                self.book_mut(book)?
                    .wb
                    .set_detail_shown(sheet, rows, idx, show);
                Ok(Variant::Empty)
            }
            ("OutlineLevel", _) => {
                let whole_rows = rect.c0 == 0 && rect.c1 == MAX_COLS - 1;
                let whole_cols = rect.r0 == 0 && rect.r1 == MAX_ROWS - 1;
                let rows = match r.mode {
                    Mode::Rows => true,
                    Mode::Columns => false,
                    _ if whole_rows => true,
                    _ if whole_cols => false,
                    _ => return Err(OmError::app_defined()),
                };
                let (lo, hi) = if rows {
                    (rect.r0, rect.r1)
                } else {
                    (rect.c0, rect.c1)
                };
                if m.kind == Get {
                    let wb = &self.book(book)?.wb;
                    let level = |i: u32| {
                        if rows {
                            wb.row_outline_level(sheet, i)
                        } else {
                            wb.col_outline_level(sheet, i)
                        }
                    };
                    let first = level(lo);
                    return Ok(if (lo..=hi).all(|i| level(i) == first) {
                        Variant::R8(f64::from(first))
                    } else {
                        Variant::Null
                    });
                }
                let x = self.put_f64(a)?.trunc();
                if !(1.0..=8.0).contains(&x) {
                    return Err(OmError::app_defined());
                }
                self.book_mut(book)?
                    .wb
                    .set_outline_levels(sheet, rows, lo, hi - lo + 1, x as u32);
                Ok(Variant::Empty)
            }
            _ => Err(unsupported("Range", m)),
        }
    }
}

impl Application<'_> {
    fn pivot_drill_down(
        &mut self,
        b: BookId,
        rows: Vec<Vec<xlcalc::Value>>,
    ) -> Result<Variant, OmError> {
        let book = self.book_mut(b)?;
        let mut name = format!("Sheet{}", book.next_sheet);
        book.next_sheet += 1;
        while book.wb.sheet_by_name(&name).is_some() {
            name = format!("Sheet{}", book.next_sheet);
            book.next_sheet += 1;
        }
        let first = book.wb.sheet_at(0).id;
        let id = book
            .wb
            .add_sheet_before(&name, Some(first))
            .map_err(crate::objects::edit_error)?;
        for (i, row) in rows.into_iter().enumerate() {
            for (j, v) in row.into_iter().enumerate() {
                book.wb
                    .set_value(xlcalc::CellAddr::new(id, i as u32, j as u32), v);
            }
        }
        book.wb
            .set_active_sheet(id)
            .map_err(crate::objects::edit_error)?;
        Ok(Variant::Empty)
    }
}
