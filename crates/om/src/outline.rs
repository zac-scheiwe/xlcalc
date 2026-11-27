use xlcalc::{Rect, SheetId};
use xlcalc_variant::Variant;

use crate::addr::{max_cols, max_rows};
use crate::app::{unsupported, Application, BookId, Mode, Object, RangeRef};
use crate::cmdargs::Reader;
use crate::conv::{command_int, Args};
use crate::error::OmError;
use crate::typelib::{InvokeKind, Member};

const CREATOR: i32 = 0x5843_454C;
const XL_LEFT: i32 = -4131;
const XL_RIGHT: i32 = -4152;

fn outline_rows(rect: Rect, mode: Mode, start: Option<i32>) -> Result<bool, OmError> {
    let rows = rect.c0 == 0 && rect.c1 == max_cols() - 1;
    let cols = rect.r0 == 0 && rect.r1 == max_rows() - 1;
    if rows != cols {
        return Ok(rows);
    }
    match start {
        None => Ok(mode == Mode::Rows),
        Some(1) => Ok(true),
        Some(2) => Ok(false),
        Some(_) => Err(OmError::app_defined()),
    }
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

                let operands = self
                    .member_operands("Outline", m.dispid, a)
                    .map_err(|_| OmError::app_defined())?;
                let refused = OmError::app_defined();
                self.run_command(b, a.lcid, operands, &refused, |app, o| {
                    let reader = Reader {
                        wb: &app.book(b)?.wb,
                        lcid: a.lcid,
                    };
                    let level = |i: usize| {
                        let n = reader.int(o.get(i)).map_err(|_| OmError::app_defined())?;
                        u32::try_from(n.unwrap_or(0))
                            .ok()
                            .filter(|n| *n <= 8)
                            .ok_or_else(OmError::app_defined)
                    };
                    let (rows, cols) = (level(0)?, level(1)?);
                    app.book_mut(b)?.wb.outline_show_levels(s, rows, cols);
                    Ok(Variant::Bool(true))
                })
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
        if m.name == "ShowDetail" && rect.cell_count() == 1 && r.eff_mode() == Mode::Cells {
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

        if matches!(m.name, "ShowDetail" | "OutlineLevel") && r.eff_mode() == Mode::Cells {
            return Err(OmError::app_defined());
        }
        match (m.name, m.kind) {
            ("Group" | "Ungroup", Func) => {
                if (1..4).any(|i| a.get(i).is_some()) {
                    return Err(OmError::app_defined());
                }
                let operands = self
                    .range_operands(m.dispid, a)
                    .map_err(|_| OmError::app_defined())?;
                let refused = OmError::app_defined();
                let mode = r.eff_mode();
                self.run_command(book, a.lcid, operands, &refused, |app, o| {
                    let reader = Reader {
                        wb: &app.book(book)?.wb,
                        lcid: a.lcid,
                    };
                    let start = reader.int(o.get(0)).map_err(|_| OmError::app_defined())?;
                    let rows = outline_rows(rect, mode, start)?;
                    let (at, count) = if rows {
                        (rect.r0, rect.rows())
                    } else {
                        (rect.c0, rect.cols())
                    };
                    app.book_mut(book)?
                        .wb
                        .group_axis(sheet, rows, at, count, m.name == "Group")
                        .map_err(|_| OmError::app_defined())?;
                    Ok(Variant::Bool(true))
                })
            }
            ("ShowDetail", _) => {
                let rows = r.eff_mode() == Mode::Rows;
                if (if rows { rect.rows() } else { rect.cols() }) != 1 {
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
                let whole_rows = rect.c0 == 0 && rect.c1 == max_cols() - 1;
                let whole_cols = rect.r0 == 0 && rect.r1 == max_rows() - 1;
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

                let wb = &self.book(book)?.wb;
                let level = command_int(a.put.as_ref(), a.lcid, wb)
                    .ok()
                    .flatten()
                    .and_then(|x| u32::try_from(x).ok())
                    .filter(|x| (1..=8).contains(x))
                    .ok_or_else(OmError::app_defined)?;
                self.book_mut(book)?
                    .wb
                    .set_outline_levels(sheet, rows, lo, hi - lo + 1, level);
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

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn group_start_picks_the_axis_of_a_block_only() {
        let rows = Rect::new(1, 0, 2, max_cols() - 1);
        let block = Rect::new(1, 1, 2, 2);
        assert!(outline_rows(rows, Mode::Rows, Some(7)).unwrap());
        assert!(!outline_rows(block, Mode::Cells, None).unwrap());
        assert!(outline_rows(block, Mode::Rows, None).unwrap());
        assert!(outline_rows(block, Mode::Cells, Some(1)).unwrap());
        assert!(outline_rows(block, Mode::Cells, Some(0)).is_err());
    }
}
