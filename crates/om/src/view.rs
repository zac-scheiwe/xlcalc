use xlcalc::{FilterSpec, Rect, SheetId, Table};
use xlcalc_variant::Variant;

use crate::addr::{MAX_COLS, MAX_ROWS};
use crate::app::{unsupported, Application, BookId, Mode, Object, RangeRef, SortOwner};
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

        (Mode::Rows, _, _) => Some(true),
        (Mode::Columns, _, _) => Some(false),
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
            ("Merge", Func) => {
                let across = a.bool(0)?.unwrap_or(false);
                self.book_mut(book)?
                    .wb
                    .merge_cells(sheet, rect, across)
                    .map_err(edit_error)?;
                Ok(Variant::Empty)
            }
            ("MergeCells", Put) => {
                let on = match &a.put {
                    None | Some(Variant::Empty | Variant::Null) => false,
                    _ => self
                        .put_bool(a)
                        .map_err(|_| OmError::unable_to_set("MergeCells", "Range"))?,
                };
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
            ("Left", Get) => {
                let wb = &self.book(book)?.wb;
                Ok(Variant::R8(
                    (0..rect.c0).map(|c| wb.column_points(sheet, c)).sum(),
                ))
            }
            ("Top", Get) => {
                let wb = &self.book(book)?.wb;
                Ok(Variant::R8(
                    (0..rect.r0).map(|r| wb.row_height(sheet, r)).sum(),
                ))
            }
            ("RowHeight", Put) => {
                let h = self.put_f64_or_flag(a)?;
                self.book_mut(book)?
                    .wb
                    .set_row_height(sheet, rect.r0, rect.r1, h)
                    .ok_or_else(|| OmError::unable_to_set("RowHeight", "Range"))?;
                Ok(Variant::Empty)
            }

            ("AutoFit", Func) => {
                if matches!(
                    (r.mode, r.axis),
                    (Mode::Columns, _) | (_, Some(Mode::Columns))
                ) {
                    self.book_mut(book)?.wb.autofit_columns(sheet, rect);
                } else if matches!((r.mode, r.axis), (Mode::Rows, _) | (_, Some(Mode::Rows))) {
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
                let cw = self.put_f64_or_flag(a)?;
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

    pub(crate) fn use_standard_member(
        &mut self,
        book: BookId,
        sheet: SheetId,
        rects: &[Rect],
        m: &'static Member,
        a: &Args,
    ) -> Result<Variant, OmError> {
        use InvokeKind::*;
        if m.name == "Summary" {
            return Err(OmError::app_defined_with(
                "Unable to get the Summary property of the Range class",
            ));
        }
        let rows = m.name == "UseStandardHeight";
        let wb = &self.book(book)?.wb;
        let standard = |i: u32| {
            if rows {
                !wb.row_hidden(sheet, i) && wb.row_custom_h8(sheet, i).is_none()
            } else {
                wb.cell_width(sheet, i).1
            }
        };
        let span = |r: &Rect| if rows { (r.r0, r.r1) } else { (r.c0, r.c1) };
        if m.kind == Get {
            let mut all = true;
            let mut none = true;
            for r in rects {
                let (lo, hi) = span(r);
                for i in lo..=hi {
                    if standard(i) {
                        none = false;
                    } else {
                        all = false;
                    }
                }
            }
            return Ok(if all {
                Variant::Bool(true)
            } else if none {
                Variant::Bool(false)
            } else {
                Variant::Null
            });
        }
        let on = self
            .put_bool(a)
            .map_err(|_| OmError::unable_to_set(m.name, "Range"))?;
        let wb = &mut self.book_mut(book)?.wb;
        for r in rects {
            let (lo, hi) = span(r);
            if rows {
                if on {
                    wb.autofit_rows(sheet, lo, hi);
                } else {
                    for i in lo..=hi {
                        if !wb.row_hidden(sheet, i) && wb.row_custom_h8(sheet, i).is_none() {
                            let h8 = wb.row_h8(sheet, i);
                            wb.set_row_h8(sheet, i, h8);
                        }
                    }
                }
            } else if on {
                let std = wb.sheet_standard_width(sheet);
                wb.set_column_width(sheet, lo, hi, std);
            }
        }
        Ok(Variant::Empty)
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
            return Ok(Variant::Bool(true));
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

        let op_code = match a.i32(2)? {
            Some(1) if a.get(3).is_none() => 0,
            o => o.unwrap_or(0),
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

        let each: Vec<String> = match a.get(1) {
            Some(Variant::Array(arr)) if op_code != 7 => arr
                .data
                .iter()
                .filter_map(|v| match v {
                    Variant::Bstr(s) => Some(s.clone()),
                    _ => None,
                })
                .collect(),
            _ => Vec::new(),
        };
        let runs: Vec<Option<&str>> = if each.is_empty() {
            vec![criteria1.as_deref()]
        } else {
            each.iter().map(|s| Some(s.as_str())).collect()
        };
        for criteria1 in runs {
            self.book_mut(book)?
                .wb
                .autofilter(
                    sheet,
                    rect,
                    table.as_deref(),
                    FilterSpec {
                        field,
                        criteria1,
                        operator: Some(operator),
                        criteria2: criteria2.as_deref(),
                        op_code,
                    },
                )
                .map_err(edit_error)?;
        }

        if op_code == 7 {
            return Ok(Variant::R8(f64::from(field)));
        }
        if let Some(Variant::Array(arr)) = a.get(1) {
            return Ok(Variant::Array(Box::new(xlcalc_variant::SafeArray {
                elem: xlcalc_variant::VarType::Variant,
                bounds: vec![(1, arr.data.len() as u32)],
                data: vec![Variant::Bool(true); arr.data.len()],
            })));
        }
        Ok(Variant::Bool(true))
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
                let on = self.put_bool(a).map_err(|_| OmError::app_defined())?;
                let wb = &mut self.book_mut(b)?.wb;
                if on {
                    if wb.autofilter_mode(s) {
                        return Ok(Variant::Empty);
                    }
                    return Err(OmError::app_defined());
                }
                wb.autofilter_off(s, None).map_err(edit_error)?;
                Ok(Variant::Empty)
            }
            ("FilterMode", Get) => Ok(Variant::Bool(self.book(b)?.wb.filter_mode(s))),
            ("StandardWidth", Get) => Ok(Variant::R8(self.book(b)?.wb.sheet_standard_width(s))),
            ("StandardWidth", Put) => {
                let w = self.put_f64(a).map_err(|_| OmError::app_defined())?;
                if !(0.0..=255.0).contains(&w) {
                    return Err(OmError::unable_to_set("StandardWidth", "Worksheet"));
                }
                self.book_mut(b)?.wb.set_sheet_standard_width(s, w);
                Ok(Variant::Empty)
            }
            ("StandardHeight", Get) => Ok(Variant::R8(self.book(b)?.wb.standard_height(s))),
            ("ShowAllData", Func) => {
                self.book_mut(b)?.wb.show_all_data(s).map_err(edit_error)?;
                Ok(Variant::Bool(true))
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
                    self.book_mut(b)?.wb.table_filter_on(&t.name);
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
            ("Range", Get) => {
                let v = self
                    .book(b)?
                    .wb
                    .autofilter_view(s)
                    .ok_or_else(OmError::app_defined)?;
                self.new_range(b, s, v.rect, Mode::Cells)
            }
            ("Filters", Get) => Ok(self.alloc(Object::Filters(b, s))),
            ("Sort", Get) => Ok(self.alloc(Object::Sort(SortOwner::Sheet(b, s)))),
            ("Parent", Get) => Ok(self.alloc(Object::Worksheet(b, s))),
            _ => Err(unsupported("AutoFilter", m)),
        }
    }

    pub(crate) fn autofilter_cols_member(
        &mut self,
        b: BookId,
        s: SheetId,
        m: &'static Member,
        a: &Args,
    ) -> Result<Variant, OmError> {
        let n = self
            .book(b)?
            .wb
            .autofilter_view(s)
            .map_or(0, |v| v.fields.len());
        match m.name {
            "Count" => Ok(Variant::I4(n as i32)),
            "Item" | "_Default" => {
                let cols: Vec<usize> = (0..n).collect();
                let i = self.item_index(a.get(0), a.lcid, &cols, |_| "")?;
                Ok(self.alloc(Object::Filter(b, s, i)))
            }
            "_NewEnum" => {
                let items = (0..n)
                    .map(|i| self.alloc(Object::Filter(b, s, i)))
                    .collect();
                Ok(self.alloc_enumerator(items))
            }
            _ => Err(unsupported("Filters", m)),
        }
    }

    pub(crate) fn autofilter_col_member(
        &mut self,
        b: BookId,
        s: SheetId,
        i: usize,
        m: &'static Member,
    ) -> Result<Variant, OmError> {
        let view = self
            .book(b)?
            .wb
            .autofilter_view(s)
            .ok_or_else(OmError::app_defined)?;
        let col = view.fields.get(i).ok_or_else(OmError::app_defined)?;
        match m.name {
            "On" => Ok(Variant::Bool(col.is_some())),
            "Criteria1" => {
                let c = col.as_ref().ok_or_else(OmError::app_defined)?;
                if c.values.len() > 1 {
                    let data: Vec<Variant> = c.values.iter().cloned().map(Variant::Bstr).collect();
                    return Ok(Variant::Array(Box::new(xlcalc_variant::SafeArray {
                        elem: xlcalc_variant::VarType::Variant,
                        bounds: vec![(1, data.len() as u32)],
                        data,
                    })));
                }
                Ok(Variant::Bstr(c.c1.clone()))
            }
            "Criteria2" => match col.as_ref().and_then(|c| c.c2.clone()) {
                Some(t) => Ok(Variant::Bstr(t)),
                None => Err(OmError::app_defined()),
            },
            "Operator" => Ok(Variant::I4(col.as_ref().map_or(0, |c| c.op))),
            _ => Err(unsupported("Filter", m)),
        }
    }
}

impl Application<'_> {
    fn put_f64_or_flag(&self, a: &Args) -> Result<f64, OmError> {
        match a.put {
            Some(Variant::Bool(x)) => Ok(f64::from(u8::from(x))),
            _ => self.put_f64(a),
        }
    }
}
