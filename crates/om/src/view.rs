use crate::sheetprot::ProtObject;
use xlcalc::{CellAddr, FilterSpec, Rect, SheetId, Table};
use xlcalc_variant::Variant;

use crate::addr::{max_cols, max_rows};
use crate::app::{unsupported, Application, BookId, Mode, Object, RangeRef, SortOwner};
use crate::conv::Args;
use crate::error::OmError;
use crate::objects::edit_error;
use crate::typelib::{InvokeKind, Member};

fn axis(rect: Rect, mode: Mode) -> Option<bool> {
    let rows = rect.c0 == 0 && rect.c1 == max_cols() - 1;
    let cols = rect.r0 == 0 && rect.r1 == max_rows() - 1;
    match (mode, rows, cols) {
        (Mode::Cells, _, _) => None,
        (Mode::Columns, _, true) => Some(false),
        (_, true, _) => Some(true),
        (_, _, true) => Some(false),

        (Mode::Rows, _, _) => Some(true),
        (Mode::Columns, _, _) => Some(false),
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
                let rows = axis(rect, r.eff_mode()).ok_or_else(OmError::app_defined)?;
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
                let rows = axis(rect, r.eff_mode()).ok_or_else(OmError::app_defined)?;
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
                let across = self.merge_across(a)?;
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
            ("AutoFilter", Func) => self.range_autofilter(book, sheet, rect, a),

            ("ColumnWidth", Get) => {
                let wb = &self.book(book)?.wb;
                let w = wb.column_width(sheet, rect.c0);

                Ok(
                    if crate::range::used_area_differs(wb, sheet, rect, |c| {
                        wb.column_width(sheet, c)
                    }) {
                        Variant::Null
                    } else {
                        Variant::R8(w)
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
                let pop = wb.populated_lines(sheet, true);
                let mut hs = pop
                    .range(rect.r0..=rect.r1)
                    .map(|r| wb.row_height(sheet, *r));

                let differs = hs.next().is_some_and(|f| hs.any(|x| x != f))
                    || wb.row_custom_heights(sheet).iter().any(|(r, _)| {
                        (rect.r0..=rect.r1).contains(r) && wb.row_height(sheet, *r) != h
                    });
                Ok(if differs {
                    Variant::Null
                } else {
                    Variant::R8(h)
                })
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
                    (0..rect.c0)
                        .map(|c| wb.column_points(sheet, c))
                        .fold(0.0, |x, y| x + y),
                ))
            }
            ("Top", Get) => {
                let wb = &self.book(book)?.wb;
                Ok(Variant::R8(
                    (0..rect.r0)
                        .map(|r| wb.row_height(sheet, r))
                        .fold(0.0, |x, y| x + y),
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
                let wb = &mut self.book_mut(book)?.wb;

                if cw > 0.0 && rect.is_full_rows() {
                    wb.set_sheet_standard_width(sheet, cw);
                    wb.use_standard_width(sheet, rect.c0, rect.c1);
                } else {
                    wb.set_column_width(sheet, rect.c0, rect.c1, cw);
                }
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
                wb.col_standard(sheet, i)
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
                wb.use_standard_width(sheet, lo, hi);
            }
        }
        Ok(Variant::Empty)
    }

    pub(crate) fn merge_across(&self, a: &Args) -> Result<bool, OmError> {
        if let Some(Variant::Dispatch(Some(id))) = a.get(0) {
            let Some(Object::Range(r)) = self.arena.get(*id) else {
                return Err(OmError::app_defined());
            };
            let (sheet, rect) = self.resolve(r)?;
            let value = self
                .book(r.book)?
                .wb
                .value(CellAddr::new(sheet, rect.r0, rect.c0));
            return match value {
                _ if !rect.is_cell() => Err(OmError::app_defined()),
                xlcalc::Value::Blank => Ok(false),
                xlcalc::Value::Bool(b) => Ok(b),
                xlcalc::Value::Number(n) => Ok(n != 0.0),
                _ => Err(OmError::app_defined()),
            };
        }
        Ok(a.bool(0)
            .map_err(|_| OmError::app_defined())?
            .unwrap_or(false))
    }

    fn range_autofilter(
        &mut self,
        book: BookId,
        sheet: SheetId,
        rect: Rect,
        a: &Args,
    ) -> Result<Variant, OmError> {
        let table = self
            .book(book)?
            .wb
            .tables()
            .iter()
            .find(|t| t.sheet == sheet && t.rect == rect)
            .map(|t| (t.id, t.name.clone()));
        let Some(_) = a.get(0) else {
            if a.get(1).is_some() {
                return Err(OmError::app_defined());
            }
            if let Some((id, name)) = table {
                let on = self
                    .book(book)?
                    .wb
                    .tables()
                    .iter()
                    .any(|t| t.name == name && t.show_filter);
                let wb = &mut self.book_mut(book)?.wb;
                if on {
                    wb.autofilter_off(sheet, Some(&name)).map_err(edit_error)?;
                } else {
                    wb.table_filter_on(id);
                }
                return Ok(Variant::Bool(true));
            }
            self.book_mut(book)?
                .wb
                .autofilter_toggle(sheet, rect)
                .map_err(edit_error)?;
            if !self.book(book)?.wb.autofilter_mode(sheet) {
                self.drop_filter_sort(book, sheet);
            }
            return Ok(Variant::Bool(true));
        };

        let args = match self.af_args(a) {
            Ok(args) => args,
            Err(f) => {
                if f.arrows && table.is_none() {
                    let wb = &mut self.book_mut(book)?.wb;
                    if !wb.autofilter_mode(sheet) {
                        wb.autofilter_toggle(sheet, rect).ok();
                    }
                }
                return Err(f.err);
            }
        };
        let values_op = args.operator == "values";

        let operator = match (values_op, &args.values) {
            (true, None) => "and",
            _ => args.operator,
        };
        let joined = args
            .values
            .as_ref()
            .map(|v| v.join(&xlcalc::testing::VALUES_SEP.to_string()));
        let first_field = args.calls.first().map_or(0, |c| c.field);

        let none = matches!(&args.values, Some(v) if v.is_empty());

        let wb = &mut self.book_mut(book)?.wb;
        let locale = crate::conv::input_locale(a.lcid, wb.locale());
        for call in &args.calls {
            let op_code = match (values_op, &args.values) {
                (true, Some(_)) => 7,
                (true, None) => i32::from(call.c2.is_some()),
                _ if matches!(args.op_code, 1 | 2) && call.c2.is_none() => 0,
                _ => args.op_code,
            };
            let spec = FilterSpec {
                field: u32::try_from(call.field).unwrap_or(0),
                criteria1: none
                    .then_some(xlcalc::testing::VALUES_NONE)
                    .or(joined.as_deref())
                    .or(call.c1.as_deref())
                    .or((operator == "icon").then_some(""))
                    .or((values_op && call.c2.is_some()).then_some("=")),
                operator: Some(operator),
                criteria2: call.c2.as_deref().filter(|_| !none),
                op_code,
                rich: args.rich,
            };
            wb.with_input_locale(&locale, |wb| {
                wb.autofilter(sheet, rect, table.as_ref().map(|(_, n)| n.as_str()), spec)
            })
            .map_err(edit_error)?;
        }

        if values_op || operator == "icon" {
            return Ok(Variant::R8(f64::from(first_field)));
        }
        let total = args
            .shape
            .as_ref()
            .map_or(1, |b| b.iter().map(|&(_, n)| n as usize).product());
        Ok(Self::af_answer(&args.shape, total))
    }

    fn drop_filter_sort(&mut self, book: BookId, sheet: SheetId) {
        self.sorts.remove(&SortOwner::Filter(book, sheet));
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
            ("AutoFilter" | "_AutoFilter", Get) => {
                if self.book(b)?.wb.sheet_autofilter_missing(s) {
                    Ok(Variant::Dispatch(None))
                } else {
                    Ok(self.alloc(Object::AutoFilter(b, s, None)))
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
                self.drop_filter_sort(b, s);
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
            ("AutoFilter" | "_AutoFilter", Get) if !t.show_filter => Ok(Variant::Dispatch(None)),
            ("AutoFilter" | "_AutoFilter", Get) => {
                Ok(self.alloc(Object::AutoFilter(b, t.sheet, Some(t.name.clone()))))
            }
            ("ShowAutoFilter", Get) => Ok(Variant::Bool(t.show_filter)),

            ("ShowAutoFilter", Put) => {
                let on = self.put_bool(a)?;
                if on != t.show_filter {
                    self.object_guard(b, t.sheet, ProtObject::TableFilter, None)?;
                }
                if on {
                    self.book_mut(b)?.wb.table_filter_on(t.id);
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
        table: Option<&str>,
        m: &'static Member,
    ) -> Result<Variant, OmError> {
        use InvokeKind::*;
        match (m.name, m.kind) {
            ("ApplyFilter", Func) => {
                self.book_mut(b)?.wb.reapply_filter(s).map_err(edit_error)?;
                Ok(Variant::Empty)
            }
            ("FilterMode", Get) => Ok(Variant::Bool(
                self.book(b)?.wb.autofilter_object_mode(s, table),
            )),

            ("ShowAllData", Func) => {
                self.book_mut(b)?.wb.show_all_data(s).ok();
                Ok(Variant::Empty)
            }
            ("Range", Get) => {
                let v = self
                    .book(b)?
                    .wb
                    .autofilter_view(s, table)
                    .ok_or_else(OmError::app_defined)?;
                self.new_range(b, s, v.rect, Mode::Cells)
            }
            ("Filters", Get) => Ok(self.alloc(Object::Filters(b, s, table.map(str::to_string)))),

            ("Sort", Get) => {
                let object = match table {
                    Some(name) => {
                        let id = self
                            .book(b)?
                            .wb
                            .table_by_name(name)
                            .map(|t| t.id)
                            .ok_or_else(OmError::app_defined)?;
                        Object::TableFilterSort(b, id)
                    }
                    None => Object::Sort(SortOwner::Filter(b, s)),
                };
                Ok(self.alloc(object))
            }
            ("Parent", Get) => Ok(self.alloc(Object::Worksheet(b, s))),
            _ => Err(unsupported("AutoFilter", m)),
        }
    }

    pub(crate) fn autofilter_cols_member(
        &mut self,
        b: BookId,
        s: SheetId,
        table: Option<&str>,
        m: &'static Member,
        a: &Args,
    ) -> Result<Variant, OmError> {
        let name = || table.map(str::to_string);
        let n = self
            .book(b)?
            .wb
            .autofilter_view(s, table)
            .map_or(0, |v| v.fields.len());
        match m.name {
            "Count" => Ok(Variant::I4(n as i32)),
            "Item" | "_Default" => {
                let cols: Vec<usize> = (0..n).collect();
                let i = self.item_index(a.get(0), a.lcid, &cols, |_| "")?;
                Ok(self.alloc(Object::Filter(b, s, name(), i)))
            }
            "_NewEnum" => {
                let items = (0..n)
                    .map(|i| self.alloc(Object::Filter(b, s, name(), i)))
                    .collect();
                Ok(self.alloc_enumerator(items))
            }
            _ => Err(unsupported("Filters", m)),
        }
    }

    pub(crate) fn filter_color_member(
        &mut self,
        b: BookId,
        s: SheetId,
        table: Option<&str>,
        i: usize,
        m: &'static Member,
    ) -> Result<Variant, OmError> {
        let col = self
            .book(b)?
            .wb
            .autofilter_view(s, table)
            .and_then(|v| v.fields.get(i).cloned().flatten())
            .ok_or_else(OmError::app_defined)?;
        match (m.name, m.kind) {
            ("Color", InvokeKind::Get) => match col.c1.parse::<f64>() {
                Ok(n) => Ok(Variant::R8(n)),
                Err(_) => Err(OmError::app_defined()),
            },
            _ => Err(unsupported("Interior", m)),
        }
    }

    pub(crate) fn autofilter_col_member(
        &mut self,
        b: BookId,
        s: SheetId,
        table: Option<&str>,
        i: usize,
        m: &'static Member,
    ) -> Result<Variant, OmError> {
        let view = self
            .book(b)?
            .wb
            .autofilter_view(s, table)
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

                if c.op == 14 {
                    return Ok(Variant::Empty);
                }

                if c.op == 0 && c.c1.is_empty() {
                    return Err(OmError::app_defined());
                }
                if matches!(c.op, 8 | 12) {
                    return Ok(self.alloc(Object::FilterColor(
                        b,
                        s,
                        table.map(str::to_string),
                        i,
                        false,
                    )));
                }

                if matches!(c.op, 9 | 11 | 13) {
                    if let Ok(n) = c.c1.parse::<i32>() {
                        return Ok(Variant::I4(n));
                    }
                }
                Ok(Variant::Bstr(c.c1.clone()))
            }
            "Criteria2" => match col.as_ref().and_then(|c| c.c2.clone()) {
                Some(t) => Ok(Variant::Bstr(t)),
                None => Err(OmError::app_defined()),
            },

            "Operator" => Ok(Variant::I4(
                col.as_ref().ok_or_else(OmError::app_defined)?.op,
            )),
            "Count" => {
                let c = col.as_ref().ok_or_else(OmError::app_defined)?;
                Ok(Variant::I4(if c.op == 0 && c.c1.is_empty() {
                    0
                } else if c.values.len() > 1 {
                    c.values.len() as i32
                } else {
                    1 + i32::from(c.c2.is_some())
                }))
            }
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
