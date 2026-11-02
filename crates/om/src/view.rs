use xlcalc::{FilterSpec, Rect, SheetId, Table};
use xlcalc_variant::Variant;

use crate::addr::{MAX_COLS, MAX_ROWS};
use crate::app::{unsupported, Application, BookId, Mode, Object, RangeRef};
use crate::conv::Args;
use crate::error::OmError;
use crate::objects::edit_error;
use crate::typelib::{InvokeKind, Member};

fn axis(rect: Rect, mode: Mode) -> Option<bool> {
    let rows = rect.c0 == 0 && rect.c1 == MAX_COLS - 1;
    let cols = rect.r0 == 0 && rect.r1 == MAX_ROWS - 1;
    match (mode, rows, cols) {
        (Mode::Columns, _, true) => Some(false),
        (_, true, _) => Some(true),
        (_, _, true) => Some(false),
        _ => None,
    }
}

fn tri(total: u64, yes: u64) -> Variant {
    if yes == total {
        Variant::Bool(true)
    } else if yes == 0 {
        Variant::Bool(false)
    } else {
        Variant::Null
    }
}

impl Application<'_> {
    pub(crate) fn range_view_member(
        &mut self,
        r: &RangeRef,
        sheet: SheetId,
        rect: Rect,
        m: &'static Member,
        a: &Args,
    ) -> Result<Variant, OmError> {
        use InvokeKind::*;
        let book = r.book;
        match (m.name, m.kind) {
            ("Hidden", Get) => {
                let rows = axis(rect, r.mode).ok_or_else(OmError::app_defined)?;
                let wb = &self.book(book)?.wb;
                let (list, lo, hi) = if rows {
                    (wb.hidden_rows(sheet), rect.r0, rect.r1)
                } else {
                    (wb.hidden_cols(sheet), rect.c0, rect.c1)
                };
                let yes = list.iter().filter(|&&i| (lo..=hi).contains(&i)).count();
                Ok(tri(u64::from(hi - lo + 1), yes as u64))
            }
            ("Hidden", Put) => {
                let hide = self.put_bool(a)?;
                let rows = axis(rect, r.mode).ok_or_else(OmError::app_defined)?;
                let wb = &mut self.book_mut(book)?.wb;
                if rows {
                    wb.set_rows_hidden(sheet, rect.r0, rect.rows(), hide)
                        .map_err(edit_error)?;
                } else {
                    wb.set_cols_hidden(sheet, rect.c0, rect.cols(), hide);
                }
                Ok(Variant::Empty)
            }
            ("OutlineLevel", Get) => {
                let rows = axis(rect, r.mode).ok_or_else(OmError::app_defined)?;
                let wb = &self.book(book)?.wb;

                let level = |i: u32| {
                    if rows {
                        wb.row_outline_level(sheet, i)
                    } else {
                        1
                    }
                };
                let (lo, hi) = if rows {
                    (rect.r0, rect.r1)
                } else {
                    (rect.c0, rect.c1)
                };
                let first = level(lo);
                let same = (lo..=hi).all(|i| level(i) == first);
                Ok(if same {
                    Variant::I4(first as i32)
                } else {
                    Variant::Null
                })
            }
            ("Merge", Func) => {
                let across = a.bool(0)?.unwrap_or(false);
                self.book_mut(book)?
                    .wb
                    .merge_cells(sheet, rect, across)
                    .map_err(edit_error)?;
                Ok(Variant::Empty)
            }
            ("MergeCells", Put) => {
                let on = self.put_bool(a)?;
                let wb = &mut self.book_mut(book)?.wb;
                if on {
                    wb.merge_cells(sheet, rect, false).map_err(edit_error)?;
                } else {
                    wb.unmerge_cells(sheet, rect);
                }
                Ok(Variant::Empty)
            }
            ("UnMerge", Func) => {
                self.book_mut(book)?.wb.unmerge_cells(sheet, rect);
                Ok(Variant::Empty)
            }
            ("AutoFilter", Func) => self.range_autofilter(book, sheet, rect, m, a),

            ("ColumnWidth", Get) => {
                let wb = &self.book(book)?.wb;
                let w = wb.column_width(sheet, rect.c0);
                Ok(
                    if (rect.c0..=rect.c1).all(|c| wb.column_width(sheet, c) == w) {
                        Variant::R8(w)
                    } else {
                        Variant::Null
                    },
                )
            }
            ("Width", Get) => {
                let wb = &self.book(book)?.wb;
                Ok(Variant::R8(
                    (rect.c0..=rect.c1)
                        .map(|c| wb.column_points(sheet, c))
                        .sum(),
                ))
            }

            ("RowHeight", Get) => {
                let wb = &self.book(book)?.wb;
                let h = wb.row_height(sheet, rect.r0);
                Ok(
                    if (rect.r0..=rect.r1).all(|r| wb.row_height(sheet, r) == h) {
                        Variant::R8(h)
                    } else {
                        Variant::Null
                    },
                )
            }
            ("Height", Get) => {
                let wb = &self.book(book)?.wb;
                Ok(Variant::R8(
                    (rect.r0..=rect.r1).map(|r| wb.row_height(sheet, r)).sum(),
                ))
            }
            ("RowHeight", Put) => {
                let h = self.put_f64(a)?;
                self.book_mut(book)?
                    .wb
                    .set_row_height(sheet, rect.r0, rect.r1, h)
                    .ok_or_else(|| OmError::unable_to_set("RowHeight", "Range"))?;
                Ok(Variant::Empty)
            }

            ("AutoFit", Func) => {
                let full_cols = rect.r0 == 0 && rect.r1 + 1 >= MAX_ROWS;
                let full_rows = rect.c0 == 0 && rect.c1 + 1 >= MAX_COLS;
                if matches!(r.mode, Mode::Columns) || full_cols {
                    self.book_mut(book)?.wb.autofit_columns(sheet, rect);
                } else if full_rows {
                    self.book_mut(book)?
                        .wb
                        .autofit_rows(sheet, rect.r0, rect.r1);
                } else {
                    return Err(OmError::app_defined_with(
                        "AutoFit method of Range class failed",
                    ));
                }
                Ok(Variant::Bool(true))
            }
            ("ColumnWidth", Put) => {
                let cw = self.put_f64(a)?;
                if !(0.0..=255.0).contains(&cw) {
                    return Err(OmError::unable_to_set("ColumnWidth", "Range"));
                }
                self.book_mut(book)?
                    .wb
                    .set_column_width(sheet, rect.c0, rect.c1, cw);
                Ok(Variant::Empty)
            }
            _ => Err(unsupported("Range", m)),
        }
    }

    fn range_autofilter(
        &mut self,
        book: BookId,
        sheet: SheetId,
        rect: Rect,
        m: &'static Member,
        a: &Args,
    ) -> Result<Variant, OmError> {
        let table: Option<String> = self
            .book(book)?
            .wb
            .tables()
            .iter()
            .find(|t| t.sheet == sheet && t.rect == rect)
            .map(|t| t.name.clone());
        if a.get(4).is_some() || a.get(5).is_some() {
            return Err(unsupported("Range", m));
        }
        let Some(field) = a.i32(0)? else {
            if table.is_some() {
                return Err(OmError::unsupported(
                    "Range.AutoFilter switching a table's filter buttons",
                ));
            }
            self.book_mut(book)?
                .wb
                .autofilter_toggle(sheet, rect)
                .map_err(edit_error)?;
            return Ok(Variant::Empty);
        };
        let criteria1: Option<String> = match a.get(1) {
            None => None,
            Some(Variant::Bstr(s)) => Some(s.clone()),
            Some(Variant::Array(arr)) => {
                let mut items = Vec::new();
                for v in &arr.data {
                    match v {
                        Variant::Bstr(s) => items.push(s.as_str()),
                        _ => return Err(OmError::type_mismatch()),
                    }
                }
                Some(items.join(&xlcalc::testing::VALUES_SEP.to_string()))
            }
            Some(Variant::R8(x)) => Some(x.to_string()),
            Some(Variant::I4(n)) => Some(n.to_string()),
            Some(Variant::I2(n)) => Some(n.to_string()),
            Some(_) => return Err(OmError::type_mismatch()),
        };
        let operator = match a.i32(2)? {
            None | Some(1) => "and",
            Some(2) => "or",
            Some(3) => "top10",
            Some(4) => "bottom10",
            Some(5) => "top10pct",
            Some(6) => "bottom10pct",
            Some(7) => "values",
            Some(11) => "dynamic",
            Some(_) => return Err(OmError::app_defined()),
        };
        let criteria2 = a.string(3)?;
        let field = u32::try_from(field).unwrap_or(0);
        self.book_mut(book)?
            .wb
            .autofilter(
                sheet,
                rect,
                table.as_deref(),
                FilterSpec {
                    field,
                    criteria1: criteria1.as_deref(),
                    operator: Some(operator),
                    criteria2: criteria2.as_deref(),
                },
            )
            .map_err(edit_error)?;
        Ok(Variant::Empty)
    }

    pub(crate) fn sheet_view_member(
        &mut self,
        b: BookId,
        s: SheetId,
        m: &'static Member,
        a: &Args,
    ) -> Result<Variant, OmError> {
        use InvokeKind::*;
        match (m.name, m.kind) {
            ("AutoFilter", Get) => {
                if self.book(b)?.wb.sheet_autofilter_missing(s) {
                    Ok(Variant::Dispatch(None))
                } else {
                    Ok(self.alloc(Object::AutoFilter(b, s)))
                }
            }
            ("AutoFilterMode", Get) => Ok(Variant::Bool(self.book(b)?.wb.autofilter_mode(s))),
            ("AutoFilterMode", Put) => {
                let on = self.put_bool(a)?;
                let wb = &mut self.book_mut(b)?.wb;
                if on {
                    if wb.autofilter_mode(s) {
                        return Ok(Variant::Empty);
                    }
                    return Err(unsupported("Worksheet", m));
                }
                wb.autofilter_off(s, None).map_err(edit_error)?;
                Ok(Variant::Empty)
            }
            ("FilterMode", Get) => Ok(Variant::Bool(self.book(b)?.wb.filter_mode(s))),
            ("StandardWidth", Get) => Ok(Variant::R8(self.book(b)?.wb.standard_width())),
            ("StandardHeight", Get) => Ok(Variant::R8(self.book(b)?.wb.standard_height(s))),
            ("ShowAllData", Func) => {
                self.book_mut(b)?.wb.show_all_data(s).map_err(edit_error)?;
                Ok(Variant::Empty)
            }
            _ => Err(unsupported("Worksheet", m)),
        }
    }

    pub(crate) fn table_view_member(
        &mut self,
        b: BookId,
        t: &Table,
        m: &'static Member,
        a: &Args,
    ) -> Result<Variant, OmError> {
        use InvokeKind::*;
        match (m.name, m.kind) {
            ("AutoFilter", Get) => Ok(self.alloc(Object::AutoFilter(b, t.sheet))),
            ("ShowAutoFilter", Get) => Ok(Variant::Bool(t.show_filter)),
            ("ShowAutoFilter", Put) => {
                let on = self.put_bool(a)?;
                if on {
                    if !t.show_filter {
                        return Err(unsupported("ListObject", m));
                    }
                } else {
                    self.book_mut(b)?
                        .wb
                        .autofilter_off(t.sheet, Some(&t.name))
                        .map_err(edit_error)?;
                }
                Ok(Variant::Empty)
            }
            _ => Err(unsupported("ListObject", m)),
        }
    }

    pub(crate) fn autofilter_member(
        &mut self,
        b: BookId,
        s: SheetId,
        m: &'static Member,
    ) -> Result<Variant, OmError> {
        use InvokeKind::*;
        match (m.name, m.kind) {
            ("ApplyFilter", Func) => {
                self.book_mut(b)?.wb.reapply_filter(s).map_err(edit_error)?;
                Ok(Variant::Empty)
            }
            ("FilterMode", Get) => Ok(Variant::Bool(self.book(b)?.wb.filter_mode(s))),
            _ => Err(unsupported("AutoFilter", m)),
        }
    }
}
