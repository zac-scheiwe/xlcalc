use xlcalc::{ColumnId, Rect, SheetId, Table, TableId};
use xlcalc_variant::Variant;

use crate::addr::parse_areas_text;
use crate::app::{unsupported, Application, BookId, Mode, Object, SortOwner};
use crate::conv::{is_missing, Args};
use crate::error::{OmError, DISP_E_BADPARAMCOUNT, DISP_E_PARAMNOTOPTIONAL, DISP_E_TYPEMISMATCH};
use crate::objects::edit_error;
use crate::typelib::{InvokeKind, Member};

pub(crate) fn body(t: &Table) -> Option<Rect> {
    let mut r = t.rect;
    if t.headers {
        r.r0 += 1;
    }
    if t.totals_row().is_some() {
        r.r1 = r.r1.checked_sub(1)?;
    }
    (r.r0 <= r.r1 && !t.emptied).then_some(r)
}

fn lenient_pos(v: &Variant, lcid: u32) -> Result<i64, OmError> {
    match v {
        Variant::Null | Variant::Empty => Ok(0),
        Variant::Bool(b) => Ok(if *b { -1 } else { 0 }),
        Variant::Bstr(t) => t
            .trim()
            .parse::<f64>()
            .map(|f| f.round_ties_even() as i64)
            .map_err(|_| OmError::bad_index()),
        v => Ok(crate::controls::num_arg(v, lcid)?.round_ties_even() as i64),
    }
}

impl Application<'_> {
    pub(crate) fn table_of(&self, b: BookId, id: TableId) -> Result<Table, OmError> {
        self.book(b)?
            .wb
            .table(id)
            .cloned()
            .ok_or_else(OmError::object_required)
    }

    fn tables_on(&self, b: BookId, s: SheetId) -> Result<Vec<Table>, OmError> {
        Ok(self
            .book(b)?
            .wb
            .tables()
            .iter()
            .filter(|t| t.sheet == s)
            .cloned()
            .collect())
    }

    pub(crate) fn range_arg(&self, v: Option<&Variant>) -> Result<(SheetId, Rect), OmError> {
        match v {
            Some(Variant::Dispatch(Some(id))) => match self.arena.get(*id) {
                Some(Object::Range(r)) => self.resolve(r),
                _ => Err(OmError::app_defined()),
            },
            _ => Err(OmError::app_defined()),
        }
    }

    fn optional_range(
        &mut self,
        b: BookId,
        s: SheetId,
        r: Option<Rect>,
    ) -> Result<Variant, OmError> {
        match r {
            Some(rect) => self.new_range(b, s, rect, Mode::Cells),
            None => Ok(Variant::Dispatch(None)),
        }
    }

    pub(crate) fn item_index<T>(
        &self,
        v: Option<&Variant>,
        lcid: u32,
        items: &[T],
        name_of: impl Fn(&T) -> &str,
    ) -> Result<usize, OmError> {
        match v {
            Some(Variant::Bstr(n)) => items
                .iter()
                .position(|t| name_of(t).eq_ignore_ascii_case(n))
                .ok_or_else(OmError::bad_index),
            Some(v) => {
                let n = Args {
                    values: vec![Some(v.clone())],
                    put: None,
                    lcid,
                }
                .i32(0)?
                .unwrap_or(0);
                usize::try_from(n - 1)
                    .ok()
                    .filter(|&i| i < items.len())
                    .ok_or_else(OmError::bad_index)
            }
            None => Err(OmError::bad_index()),
        }
    }

    pub(crate) fn list_objects_member(
        &mut self,
        b: BookId,
        s: SheetId,
        m: &'static Member,
        a: &Args,
    ) -> Result<Variant, OmError> {
        let list = self.tables_on(b, s)?;
        match m.name {
            "Count" => Ok(Variant::I4(list.len() as i32)),
            "Item" | "_Default" => {
                let i = self.item_index(a.get(0), a.lcid, &list, |t| t.name.as_str())?;
                Ok(self.alloc(Object::Table(b, list[i].id, true)))
            }
            "_NewEnum" => {
                let items = list
                    .iter()
                    .map(|t| self.alloc(Object::Table(b, t.id, true)))
                    .collect();
                Ok(self.alloc_enumerator(items))
            }
            "Add" | "_Add" => self.list_objects_add(b, s, &list, a),
            _ => Err(unsupported("ListObjects", m)),
        }
    }

    fn list_objects_add(
        &mut self,
        b: BookId,
        s: SheetId,
        list: &[Table],
        a: &Args,
    ) -> Result<Variant, OmError> {
        let source_type = a.i32(0)?.unwrap_or(1);
        if source_type == 2 {
            return Err(invalid_arg());
        }
        self.table_guard(b, s, true)?;
        match source_type {
            1 => {}
            0 | 3 | 4 => return Err(self.external_source_error(s, source_type == 4, a)),
            _ => return Err(invalid_arg()),
        }

        let has_headers = a.i32(3)?.unwrap_or(0);
        let rect = self.list_source(b, s, a.get(1))?;
        if list.iter().any(|t| t.rect.intersects(&rect)) {
            self.book_mut(b)?.wb.table_create_off_sheet(s);
            return Err(OmError::app_defined_with(
                "A table can't overlap another table. ",
            ));
        }
        let wb = &self.book(b)?.wb;

        let guess = || {
            rect.rows() > 1
                && (rect.c0..=rect.c1).all(|c| {
                    matches!(
                        wb.value(xlcalc::CellAddr::new(s, rect.r0, c)),
                        xlcalc::Value::Text(_)
                    )
                })
        };
        let headed = match has_headers {
            0 => guess(),
            1 => true,
            2 => false,
            _ => a.get(1).is_none() && guess(),
        };

        if wb.list_add_meets_filter(s, rect, headed) {
            self.book_mut(b)?.wb.table_create_off_sheet(s);
            return Err(OmError::app_defined_with(
                "This will change a filtered range on your worksheet. To complete this task, please remove AutoFilters.",
            ));
        }

        let style = match a.get(5) {
            None => None,
            Some(v) => {
                let name = match v {
                    Variant::Bstr(n) => Some(n.as_str()),
                    Variant::Dispatch(Some(id)) => match self.arena.get(*id) {
                        Some(Object::TableStyle(n)) => Some(n.as_str()),
                        _ => None,
                    },
                    _ => None,
                };
                Some(name.and_then(structured_style).ok_or_else(invalid_arg)?.0)
            }
        };
        let id = self.list_create(b, s, rect, headed)?;
        if let Some(style) = style {
            self.book_mut(b)?
                .wb
                .set_table_look(id, |t| t.style = Some(style));
        }
        Ok(self.alloc(Object::Table(b, id, false)))
    }

    fn external_source_error(&self, s: SheetId, model: bool, a: &Args) -> OmError {
        let connect = || OmError::unsupported("ListObjects.Add of an external source");

        match self.range_arg(a.get(4)) {
            Ok((sheet, r)) if r.is_cell() => {
                if sheet != s {
                    return OmError::app_defined();
                }
            }
            _ => return invalid_arg(),
        }
        let prefixed = |text: &str, prefixes: &[&str]| {
            prefixes.iter().any(|p| {
                text.get(..p.len())
                    .is_some_and(|head| head.eq_ignore_ascii_case(p))
            })
        };
        let array = match a.get(1) {
            Some(Variant::Array(array)) if !model => array,

            Some(Variant::Bstr(text)) if !model => {
                return if prefixed(text, &["FINDER;", "OLEDB;", "ODBC;", "URL;", "TEXT;"]) {
                    connect()
                } else {
                    OmError::app_defined()
                };
            }

            Some(Variant::Dispatch(_)) if model => return connect(),
            _ => return invalid_arg(),
        };

        let [(lower, len)] = array.bounds[..] else {
            return invalid_arg();
        };
        if lower < 0 {
            return invalid_arg();
        }
        let upper = i64::from(lower) + i64::from(len) - 1;
        let element = |i: i32| {
            let at = usize::try_from(i - lower).ok()?;
            array.data.get(at).map(|v| a.variant_string(v))
        };
        let address = match element(0) {
            None => return OmError::bad_index(),
            Some(Ok(Some(text))) => text,
            Some(_) => return connect(),
        };
        if prefixed(&address, &["ODBC;", "OLEDB;", "FINDER;"]) {
            return connect();
        }

        if prefixed(&address, &["URL;", "TEXT;"]) || upper > 2 || a.bool(2).is_err() {
            return invalid_arg();
        }
        match element(1) {
            None => OmError::bad_index(),
            Some(_) => connect(),
        }
    }

    fn list_source(
        &self,
        b: BookId,
        s: SheetId,
        source: Option<&Variant>,
    ) -> Result<Rect, OmError> {
        let wb = &self.book(b)?.wb;
        let (sheet, rect) = match source {
            None => {
                let sel = wb.selection_of(s);
                let rect = match sel.areas.as_slice() {
                    [one] if !one.is_cell() => *one,
                    _ => {
                        let (r, c) = sel.active;
                        let cell = Rect {
                            r0: r,
                            r1: r,
                            c0: c,
                            c1: c,
                        };
                        wb.current_region(s, cell)
                    }
                };
                return Ok(rect);
            }
            Some(Variant::Dispatch(Some(id))) => match self.arena.get(*id) {
                Some(Object::Range(r)) => self.resolve(r)?,
                Some(Object::Multi(_)) => return Err(OmError::app_defined()),
                _ => return Err(invalid_arg()),
            },
            Some(Variant::Bstr(text)) if text.encode_utf16().count() <= 255 => {
                let text = text.strip_prefix('=').unwrap_or(text);
                match parse_areas_text(wb, s, text) {
                    Some((sheet, areas)) if sheet == s => match areas.as_slice() {
                        [one] => (sheet, *one),
                        _ => return Err(OmError::app_defined()),
                    },
                    _ => return Err(invalid_arg()),
                }
            }
            _ => return Err(invalid_arg()),
        };
        if sheet != s {
            return Err(OmError::app_defined());
        }
        Ok(rect)
    }

    fn list_create(
        &mut self,
        b: BookId,
        sheet: SheetId,
        mut rect: Rect,
        headed: bool,
    ) -> Result<TableId, OmError> {
        let wb = &mut self.book_mut(b)?.wb;
        let mut emptied = false;
        if headed {
            if rect.rows() < 2 {
                rect.r1 += 1;
                emptied = true;
            }
        } else {
            let first = Rect {
                r1: rect.r0,
                ..rect
            };
            wb.insert_cells(sheet, first, true).map_err(edit_error)?;
            rect.r1 += 1;
        }

        let mut k = wb.tables_made() + 1;
        while wb.table_by_name(&format!("Table{k}")).is_some()
            || wb.display_name_in_use(&format!("Table{k}"))
        {
            k += 1;
        }
        let name = format!("Table{k}");
        wb.add_table(&name, sheet, rect).map_err(edit_error)?;

        let id = wb
            .table_by_name(&name)
            .map(|t| t.id)
            .ok_or_else(OmError::object_required)?;
        if emptied {
            wb.set_table_look(id, |t| t.emptied = true);
        }

        if wb.selection_of(sheet).areas != [rect] {
            let mark = self.notice_mark(b);
            self.note_selection(b, mark, crate::selection::At::Last, sheet, &[rect]);
        }
        Ok(id)
    }

    pub(crate) fn list_object_member(
        &mut self,
        b: BookId,
        id: TableId,
        m: &'static Member,
        a: &Args,
    ) -> Result<Variant, OmError> {
        use InvokeKind::*;
        let t = self.table_of(b, id)?;

        if m.kind == Put && m.name != "TableStyle" && a.put.as_ref().is_some_and(is_missing) {
            return Err(OmError::hresult(DISP_E_PARAMNOTOPTIONAL));
        }
        match (m.name, m.kind) {
            ("DisplayName", Get) => Ok(Variant::Bstr(t.disp.clone())),
            ("Name" | "_Default", Get) => Ok(Variant::Bstr(t.name.clone())),

            ("Name" | "DisplayName", Put) => {
                let new = self.put_string_alpha(a)?;
                if m.name == "Name" && new == t.name {
                    return Ok(Variant::Empty);
                }
                self.table_guard(b, t.sheet, false)?;
                if new.encode_utf16().count() > 255 {
                    return Err(invalid_arg());
                }
                if m.name == "DisplayName" && !display_name_ok(&new) {
                    return Err(OmError::app_defined());
                }
                let wb = &mut self.book_mut(b)?.wb;
                if m.name == "DisplayName" {
                    wb.set_table_display_name(id, &new)
                } else {
                    wb.rename_table(id, &new)
                }
                .map_err(edit_error)?;
                Ok(Variant::Empty)
            }
            ("ShowTotals", Get) => Ok(Variant::Bool(t.totals)),

            ("ShowTotals", Put) => {
                let on = self.put_bool(a)?;
                if on != t.totals {
                    self.table_guard(b, t.sheet, false)?;
                    if on && t.rect.r1 + 1 >= crate::addr::XLSX_ROWS {
                        return Err(OmError::app_defined());
                    }
                }
                self.book_mut(b)?
                    .wb
                    .set_table_totals(id, on)
                    .map_err(edit_error)?;
                Ok(Variant::Empty)
            }
            ("ShowHeaders", Get) => Ok(Variant::Bool(t.headers)),

            ("ShowHeaders", Put) => {
                let on = self.put_bool(a)?;
                if self.table_guard(b, t.sheet, false).is_err() {
                    return Ok(Variant::Empty);
                }
                self.book_mut(b)?
                    .wb
                    .set_table_headers(id, on)
                    .map_err(edit_error)?;
                Ok(Variant::Empty)
            }
            ("Range", Get) => self.new_range(b, t.sheet, t.rect, Mode::Cells),
            ("DataBodyRange", Get) => self.optional_range(b, t.sheet, body(&t)),
            ("HeaderRowRange", Get) => {
                let r = t.headers.then_some(Rect {
                    r1: t.rect.r0,
                    ..t.rect
                });
                self.optional_range(b, t.sheet, r)
            }
            ("TotalsRowRange", Get) => {
                let r = t.totals_row().map(|row| Rect { r0: row, ..t.rect });
                self.optional_range(b, t.sheet, r)
            }
            ("ListColumns", Get) => Ok(self.alloc(Object::ListColumns(b, id))),
            ("ListRows", Get) => Ok(self.alloc(Object::ListRows(b, id))),
            ("SourceType", Get) => Ok(Variant::I4(1)),
            ("DisplayRightToLeft", Get) => Ok(Variant::Bool(false)),

            ("Active", Get) => Ok(Variant::Bool(t.list_sel)),
            ("InsertRowRange" | "XmlMap", Get) => Ok(Variant::Dispatch(None)),
            ("TableObject" | "SharePointURL" | "QueryTable", Get) => Err(OmError::app_defined()),
            ("AlternativeText" | "Summary", Get) => {
                let key = format!("tbl{}:{id:?}:{}", b.0, m.name);
                Ok(self.opts.get(&key).cloned().unwrap_or(Variant::BstrNull))
            }

            ("AlternativeText" | "Summary", Put) => {
                self.table_guard(b, t.sheet, false)?;
                let text = self.put_string_alpha(a)?;
                let max = if m.name == "Summary" { 50_000 } else { 25_000 };
                if text.encode_utf16().count() > max {
                    return Err(invalid_arg());
                }
                self.opts
                    .insert(format!("tbl{}:{id:?}:{}", b.0, m.name), Variant::Bstr(text));
                Ok(Variant::Empty)
            }
            ("TableStyle", Get) => Ok(match &t.style {
                Some(s) => self.alloc(Object::TableStyle(s.clone())),
                None => Variant::Bstr("\u{0}".to_string()),
            }),
            ("TableStyle", Put) => {
                let style = self.table_style_put(b, &t, a)?;
                self.book_mut(b)?.wb.set_table_look(id, |t| t.style = style);
                Ok(Variant::Empty)
            }
            (
                "ShowTableStyleRowStripes"
                | "ShowTableStyleColumnStripes"
                | "ShowTableStyleFirstColumn"
                | "ShowTableStyleLastColumn",
                k,
            ) => {
                let idx = match m.name {
                    "ShowTableStyleRowStripes" => 0,
                    "ShowTableStyleColumnStripes" => 1,
                    "ShowTableStyleFirstColumn" => 2,
                    _ => 3,
                };
                if k == Get {
                    return Ok(Variant::Bool(t.stripes[idx]));
                }
                let on = self.put_bool(a)?;
                self.book_mut(b)?
                    .wb
                    .set_table_look(id, |t| t.stripes[idx] = on);
                Ok(Variant::Empty)
            }
            ("Comment", Get) => Ok(Variant::Bstr(t.comment.clone())),
            ("Comment", Put) => {
                let text = self.put_string_alpha(a)?;
                if text.encode_utf16().count() > 255 {
                    return Err(invalid_arg());
                }
                self.book_mut(b)?
                    .wb
                    .set_table_look(id, |t| t.comment = text);
                Ok(Variant::Empty)
            }

            ("ShowAutoFilterDropDown", Get) => Ok(Variant::Bool(
                t.show_filter
                    && t.headers
                    && !matches!(
                        self.opts.get(&format!("tbl{}:{id:?}:dd", b.0)),
                        Some(Variant::Bool(false))
                    ),
            )),
            ("ShowAutoFilterDropDown", Put) => {
                let on = self.put_bool(a)?;
                if !t.show_filter || !t.headers {
                    return Err(OmError::app_defined());
                }
                self.opts
                    .insert(format!("tbl{}:{id:?}:dd", b.0), Variant::Bool(on));
                Ok(Variant::Empty)
            }
            ("AutoFilter" | "_AutoFilter" | "ShowAutoFilter", _) => {
                self.table_view_member(b, &t, m, a)
            }
            ("Sort", Get) => Ok(self.alloc(Object::Sort(SortOwner::Table(b, id)))),
            ("Parent", Get) => Ok(self.alloc(Object::Worksheet(b, t.sheet))),
            ("Unlist", Func) => {
                self.book_mut(b)?.wb.unlist_table(id).map_err(edit_error)?;
                self.sorts.remove(&SortOwner::Table(b, id));
                Ok(Variant::Empty)
            }
            ("Delete", Func) => {
                self.book_mut(b)?.wb.delete_table(id).map_err(edit_error)?;
                self.sorts.remove(&SortOwner::Table(b, id));
                Ok(Variant::Empty)
            }

            ("Resize", Func) => {
                let typed = matches!(a.get(0), Some(Variant::Dispatch(Some(id)))
                    if matches!(self.arena.get(*id), Some(Object::Range(_) | Object::Multi(_))));
                if !typed {
                    return Err(OmError::arg(DISP_E_TYPEMISMATCH, 0));
                }
                self.table_guard(b, t.sheet, false)?;
                let (_, rect) = self.range_arg(a.get(0))?;

                self.book_mut(b)?
                    .wb
                    .resize_table(id, rect)
                    .map_err(edit_error)?;
                Ok(Variant::Empty)
            }
            _ => Err(unsupported("ListObject", m)),
        }
    }

    fn table_style_put(&self, b: BookId, t: &Table, a: &Args) -> Result<Option<String>, OmError> {
        let refused = || OmError::hresult(DISP_E_BADPARAMCOUNT);
        self.table_guard(b, t.sheet, false).map_err(|_| refused())?;
        let name = match a.put.as_ref() {
            Some(Variant::Dispatch(Some(id))) => match self.arena.get(*id) {
                Some(Object::TableStyle(n)) => n.clone(),
                Some(Object::Worksheet(..)) => return Ok(None),
                _ => return Err(OmError::type_mismatch()),
            },
            Some(Variant::Array(_)) => return Err(refused()),
            Some(v) if is_missing(v) => return Ok(None),
            Some(Variant::Null) => return Ok(None),
            _ => self.put_string_alpha(a)?,
        };
        if name.is_empty() || name.encode_utf16().count() > 255 {
            return Ok(None);
        }
        match structured_style(&name) {
            Some((style, false)) => Ok(Some(style)),
            _ => Err(refused()),
        }
    }

    pub(crate) fn pivot_style_put(&self, a: &Args) -> Result<String, OmError> {
        let name = match a.put.as_ref() {
            Some(Variant::Dispatch(Some(id))) => match self.arena.get(*id) {
                Some(Object::TableStyle(n)) => n.clone(),

                _ => return Err(OmError::unsupported("TableStyle2 of another object")),
            },
            _ => self.put_string_alpha(a).unwrap_or_default(),
        };
        match structured_style(&name) {
            Some((style, false)) => Ok(style),
            Some((_, true)) => Err(OmError::hresult(DISP_E_BADPARAMCOUNT)),
            None => Ok(String::new()),
        }
    }

    pub(crate) fn list_columns_member(
        &mut self,
        b: BookId,
        id: TableId,
        m: &'static Member,
        a: &Args,
    ) -> Result<Variant, OmError> {
        let t = self.table_of(b, id)?;
        match m.name {
            "Count" => Ok(Variant::I4(t.columns.len() as i32)),
            "Item" | "_Default" => {
                let first = Variant::I4(1);
                let i =
                    self.item_index(Some(a.get(0).unwrap_or(&first)), a.lcid, &t.columns, |c| {
                        c.as_str()
                    })?;
                Ok(self.alloc(Object::ListColumn(b, id, t.column_ids[i], true)))
            }
            "_NewEnum" => {
                let items = t
                    .column_ids
                    .iter()
                    .map(|&c| self.alloc(Object::ListColumn(b, id, c, true)))
                    .collect();
                Ok(self.alloc_enumerator(items))
            }
            "Add" | "_Add" => {
                let n = t.columns.len();
                let pos = match a.get(0) {
                    Some(p) => usize::try_from(lenient_pos(p, a.lcid)? - 1)
                        .ok()
                        .filter(|&p| p <= n)
                        .ok_or_else(OmError::bad_index)?,
                    None => n,
                };
                let wb = &mut self.book_mut(b)?.wb;
                if pos == n {
                    let c = t.rect.c1 + 1;
                    if c > 16_383 {
                        return Err(OmError::app_defined());
                    }
                    let cells = Rect {
                        c0: c,
                        c1: c,
                        ..t.rect
                    };
                    wb.insert_cells(t.sheet, cells, false).map_err(edit_error)?;
                    let rect = Rect {
                        c1: t.rect.c1 + 1,
                        ..t.rect
                    };
                    wb.resize_table(id, rect).map_err(edit_error)?;
                } else {
                    let col = t.rect.c0 + pos as u32;
                    let cells = Rect {
                        c0: col,
                        c1: col,
                        ..t.rect
                    };
                    wb.insert_cells(t.sheet, cells, false).map_err(edit_error)?;

                    if pos == 0 {
                        let rect = Rect {
                            c1: t.rect.c1 + 1,
                            ..t.rect
                        };
                        wb.resize_table(id, rect).map_err(edit_error)?;
                    }
                }
                let column = self.table_of(b, id)?.column_ids[pos];
                Ok(self.alloc(Object::ListColumn(b, id, column, false)))
            }
            _ => Err(unsupported("ListColumns", m)),
        }
    }

    pub(crate) fn list_column_member(
        &mut self,
        b: BookId,
        id: TableId,
        column: ColumnId,
        m: &'static Member,
        a: &Args,
    ) -> Result<Variant, OmError> {
        use InvokeKind::*;
        let t = self.table_of(b, id)?;

        let i = t
            .column_ids
            .iter()
            .position(|&c| c == column)
            .ok_or_else(OmError::object_required)?;
        let col = t.rect.c0 + i as u32;
        let narrow = |r: Rect| Rect {
            c0: col,
            c1: col,
            ..r
        };
        match (m.name, m.kind) {
            ("Parent", Get) => Ok(self.alloc(Object::Table(b, id, true))),
            ("Name" | "_Default", Get) => Ok(Variant::Bstr(t.columns[i].clone())),
            ("Name", Put) => {
                let new = self.put_string(a)?;
                if !t.headers {
                    return Err(OmError::app_defined());
                }

                if t.columns
                    .iter()
                    .enumerate()
                    .any(|(j, c)| j != i && c.eq_ignore_ascii_case(&new))
                {
                    return Ok(Variant::Empty);
                }
                let addr = xlcalc::CellAddr::new(t.sheet, t.rect.r0, col);
                self.book_mut(b)?
                    .wb
                    .set_input(addr, &new)
                    .map_err(edit_error)?;
                Ok(Variant::Empty)
            }
            ("Index", Get) => Ok(Variant::I4(i as i32 + 1)),
            ("SharePointFormula", Get) => Ok(Variant::BstrNull),
            ("Range", Get) => self.new_range(b, t.sheet, narrow(t.rect), Mode::Cells),
            ("DataBodyRange", Get) => self.optional_range(b, t.sheet, body(&t).map(narrow)),
            ("Total", Get) => {
                let r = t.totals_row().map(|row| Rect {
                    r0: row,
                    r1: row,
                    ..narrow(t.rect)
                });
                self.optional_range(b, t.sheet, r)
            }
            ("Delete", Func) => {
                if t.columns.len() < 2 {
                    return Err(OmError::app_defined());
                }
                self.book_mut(b)?
                    .wb
                    .delete_cells(t.sheet, narrow(t.rect), false)
                    .map_err(edit_error)?;
                Ok(Variant::Empty)
            }
            ("TotalsCalculation", Get) => {
                let wb = &self.book(b)?.wb;
                Ok(Variant::I4(wb.table_totals_calc(id, i)))
            }
            ("TotalsCalculation", Put) => {
                let code = Application::put_value(a)
                    .i32(0)?
                    .ok_or_else(OmError::type_mismatch)?;
                if !(0..=9).contains(&code) {
                    return Err(OmError::exception(0x8007_0057_u32 as i32, None));
                }
                let wb = &mut self.book_mut(b)?.wb;
                wb.set_table_totals_calc(id, i, code).map_err(edit_error)?;
                Ok(Variant::Empty)
            }
            _ => Err(unsupported("ListColumn", m)),
        }
    }

    pub(crate) fn style_object(&mut self, name: &str) -> Variant {
        let name = structured_style(name).map_or_else(|| name.to_string(), |(n, _)| n);
        self.alloc(Object::TableStyle(name))
    }

    pub(crate) fn table_style_member(
        &mut self,
        name: &str,
        m: &'static Member,
        a: &Args,
    ) -> Result<Variant, OmError> {
        match m.name {
            "Name" | "_Default" => Ok(Variant::Bstr(name.to_string())),
            "NameLocal" => Ok(Variant::Bstr(style_local_name(name))),
            _ => {
                let slot = format!("TableStyle:{name}");
                match self.bag_member(crate::bag::Store::App, "TableStyle", &slot, m, a) {
                    Some(res) => res,
                    None => Err(unsupported("TableStyle", m)),
                }
            }
        }
    }

    pub(crate) fn list_rows_member(
        &mut self,
        b: BookId,
        id: TableId,
        m: &'static Member,
        a: &Args,
    ) -> Result<Variant, OmError> {
        let t = self.table_of(b, id)?;
        let n = if t.emptied {
            0
        } else {
            body(&t).map_or(0, |r| r.rows() as usize)
        };
        match m.name {
            "Count" => Ok(Variant::I4(n as i32)),
            "Item" | "_Default" => {
                let rows: Vec<usize> = (0..n).collect();
                let first = Variant::I4(1);
                let i = self.item_index(Some(a.get(0).unwrap_or(&first)), a.lcid, &rows, |_| "")?;
                Ok(self.alloc(Object::ListRow(b, id, i)))
            }
            "Add" | "_Add" => {
                let pos = match a.get(0) {
                    Some(p) => lenient_pos(p, a.lcid)?,
                    None => n as i64 + 1,
                };
                if pos < 1 || pos > n as i64 + 1 {
                    return Err(OmError::bad_index());
                }
                let pos = pos as usize - 1;
                if n == 0 {
                    return Err(OmError::app_defined());
                }
                let first = body(&t).ok_or_else(OmError::object_required)?;
                let row = first.r0 + pos as u32;

                if row + 1 >= crate::addr::XLSX_ROWS {
                    return Err(OmError::app_defined());
                }
                let always = match a.get(1) {
                    Some(Variant::Bstr(_)) => {
                        return Err(OmError::exception(0x8007_0057_u32 as i32, None))
                    }
                    Some(Variant::Bool(f)) => *f,
                    _ => true,
                };
                let wb = &mut self.book_mut(b)?.wb;

                if !always && pos == n && wb.list_row_append_free(id) {
                    let rect = Rect {
                        r1: t.rect.r1 + 1,
                        ..t.rect
                    };
                    wb.resize_table(id, rect).map_err(edit_error)?;
                    return Ok(self.alloc(Object::ListRow(b, id, pos)));
                }
                let cells = Rect {
                    r0: row,
                    r1: row,
                    ..t.rect
                };
                wb.insert_cells(t.sheet, cells, true).map_err(edit_error)?;

                let grown = wb.table(id).map(|x| x.rect);
                if grown == Some(t.rect) {
                    let rect = Rect {
                        r1: t.rect.r1 + 1,
                        ..t.rect
                    };
                    wb.resize_table(id, rect).map_err(edit_error)?;
                }
                Ok(self.alloc(Object::ListRow(b, id, pos)))
            }
            "_NewEnum" => {
                let items = (0..n)
                    .map(|i| self.alloc(Object::ListRow(b, id, i)))
                    .collect();
                Ok(self.alloc_enumerator(items))
            }
            _ => Err(unsupported("ListRows", m)),
        }
    }

    pub(crate) fn list_row_member(
        &mut self,
        b: BookId,
        id: TableId,
        i: usize,
        m: &'static Member,
    ) -> Result<Variant, OmError> {
        let t = self.table_of(b, id)?;
        let body = body(&t).ok_or_else(OmError::object_required)?;
        let row = body.r0 + i as u32;
        match m.name {
            "Index" => Ok(Variant::I4(i as i32 + 1)),
            "Range" => self.new_range(
                b,
                t.sheet,
                Rect {
                    r0: row,
                    r1: row,
                    ..t.rect
                },
                Mode::Cells,
            ),
            "Delete" => {
                let cells = Rect {
                    r0: row,
                    r1: row,
                    ..t.rect
                };
                self.book_mut(b)?
                    .wb
                    .delete_cells(t.sheet, cells, true)
                    .map_err(edit_error)?;
                Ok(Variant::Empty)
            }
            _ => Err(unsupported("ListRow", m)),
        }
    }
}

pub(crate) fn structured_style(name: &str) -> Option<(String, bool)> {
    const FAMILIES: [(&str, u32, bool); 11] = [
        ("TableStyleLight", 21, false),
        ("TableStyleMedium", 28, false),
        ("TableStyleDark", 11, false),
        ("PivotStyleLight", 28, false),
        ("PivotStyleMedium", 28, false),
        ("PivotStyleDark", 28, false),
        ("SlicerStyleLight", 6, true),
        ("SlicerStyleDark", 6, true),
        ("SlicerStyleOther", 2, true),
        ("TimeSlicerStyleLight", 6, true),
        ("TimeSlicerStyleDark", 6, true),
    ];
    FAMILIES.iter().find_map(|&(family, count, slicer)| {
        let head = name.get(..family.len())?;
        let digits = name.get(family.len()..)?;
        if !head.eq_ignore_ascii_case(family) || digits.starts_with(['0', '+']) {
            return None;
        }
        let n = digits.parse::<u32>().ok()?;
        (1..=count)
            .contains(&n)
            .then(|| (format!("{family}{n}"), slicer))
    })
}

pub(crate) fn style_local_name(name: &str) -> String {
    const GALLERIES: [(&str, &str); 4] = [
        ("TimeSlicerStyle", "Timeline Style"),
        ("SlicerStyle", "Slicer Style"),
        ("PivotStyle", "Pivot Style"),
        ("TableStyle", "Table Style"),
    ];
    let Some((style, _)) = structured_style(name) else {
        return name.to_string();
    };
    GALLERIES
        .iter()
        .find_map(|(family, gallery)| {
            let rest = style.strip_prefix(family)?;
            let (shade, number) = rest.split_at(rest.find(|c: char| c.is_ascii_digit())?);
            Some(format!("{gallery} {shade} {number}"))
        })
        .unwrap_or(style)
}

fn invalid_arg() -> OmError {
    OmError::exception(0x8007_0057_u32 as i32, None)
}

fn display_name_ok(s: &str) -> bool {
    xlcalc::is_valid_name(s)
}
