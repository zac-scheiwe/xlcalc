use xlcalc::{Rect, SheetId, Table};
use xlcalc_variant::Variant;

use crate::addr::parse_range_text;
use crate::app::{unsupported, Application, BookId, Mode, Object, SortOwner};
use crate::conv::Args;
use crate::error::OmError;
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

impl Application<'_> {
    pub(crate) fn table_of(&self, b: BookId, name: &str) -> Result<Table, OmError> {
        self.book(b)?
            .wb
            .table_by_name(name)
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
                Ok(self.alloc(Object::Table(b, list[i].name.clone())))
            }
            "_NewEnum" => {
                let items = list
                    .iter()
                    .map(|t| self.alloc(Object::Table(b, t.name.clone())))
                    .collect();
                Ok(self.alloc_enumerator(items))
            }
            "Add" | "_Add" => {
                if a.i32(0)?.unwrap_or(1) != 1 {
                    return Err(OmError::unsupported(
                        "ListObjects.Add of a source other than xlSrcRange",
                    ));
                }
                let has_headers = a.i32(3)?.unwrap_or(0);
                if !(0..=2).contains(&has_headers) {
                    return Err(OmError::app_defined());
                }
                let (sheet, mut rect) = match a.get(1) {
                    Some(Variant::Bstr(text)) => {
                        let wb = &self.book(b)?.wb;
                        parse_range_text(wb, s, text).ok_or_else(OmError::app_defined)?
                    }
                    other => self.range_arg(other)?,
                };
                if sheet != s {
                    return Err(OmError::app_defined());
                }

                if rect.is_cell() {
                    rect = self.book(b)?.wb.current_region(sheet, rect);
                }
                if list.iter().any(|t| t.rect.intersects(&rect)) {
                    return Err(OmError::app_defined_with(
                        "A table can't overlap another table. ",
                    ));
                }
                let wb = &self.book(b)?.wb;

                let headed = match has_headers {
                    1 => true,
                    2 => false,
                    _ => {
                        rect.rows() > 1
                            && (rect.c0..=rect.c1).all(|c| {
                                matches!(
                                    wb.value(xlcalc::CellAddr::new(sheet, rect.r0, c)),
                                    xlcalc::Value::Text(_)
                                )
                            })
                    }
                };
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
                while wb.table_by_name(&format!("Table{k}")).is_some() {
                    k += 1;
                }
                let name = format!("Table{k}");
                wb.add_table(&name, sheet, rect).map_err(edit_error)?;
                if emptied {
                    wb.set_table_look(&name, |t| t.emptied = true);
                }

                if wb.selection_of(sheet).areas != [rect] {
                    let mark = self.notice_mark(b);
                    self.note_selection(b, mark, crate::selection::At::Last, sheet, &[rect]);
                }
                Ok(self.alloc(Object::Table(b, name)))
            }
            _ => Err(unsupported("ListObjects", m)),
        }
    }

    fn table_renamed(&mut self, b: BookId, old: &str, new: &str) {
        let same = |n: &str| n.eq_ignore_ascii_case(old);
        self.arena.for_each_mut(|o| match o {
            Object::Table(ob, n)
            | Object::ListColumns(ob, n)
            | Object::ListColumn(ob, n, _)
            | Object::ListRows(ob, n)
            | Object::ListRow(ob, n, _)
                if *ob == b && same(n) =>
            {
                *n = new.to_string();
            }
            Object::Sort(SortOwner::Table(ob, n))
            | Object::SortFields(SortOwner::Table(ob, n))
            | Object::SortField(SortOwner::Table(ob, n), _)
                if *ob == b && same(n) =>
            {
                *n = new.to_string();
            }
            _ => {}
        });
        if let Some(st) = self
            .sorts
            .remove(&SortOwner::Table(b, old.to_string()).key())
        {
            self.sorts
                .insert(SortOwner::Table(b, new.to_string()).key(), st);
        }
    }

    pub(crate) fn list_object_member(
        &mut self,
        b: BookId,
        name: &str,
        m: &'static Member,
        a: &Args,
    ) -> Result<Variant, OmError> {
        use InvokeKind::*;
        let t = self.table_of(b, name)?;
        let handle = name.to_string();
        match (m.name, m.kind) {
            ("Name" | "DisplayName" | "_Default", Get) => Ok(Variant::Bstr(t.name.clone())),
            ("Name" | "DisplayName", Put) => {
                let new = self.put_string(a)?;
                if m.name == "DisplayName" {
                    if new.chars().count() > 255 {
                        return Err(OmError::exception(0x8007_0057_u32 as i32, None));
                    }
                    if !display_name_ok(&new) {
                        return Err(OmError::app_defined());
                    }
                }
                self.book_mut(b)?
                    .wb
                    .rename_table(&t.name, &new)
                    .map_err(edit_error)?;
                self.table_renamed(b, &handle, &new);
                Ok(Variant::Empty)
            }
            ("ShowTotals", Get) => Ok(Variant::Bool(t.totals)),
            ("ShowTotals", Put) => {
                let on = self.put_bool(a)?;
                self.book_mut(b)?
                    .wb
                    .set_table_totals(&t.name, on)
                    .map_err(edit_error)?;
                Ok(Variant::Empty)
            }
            ("ShowHeaders", Get) => Ok(Variant::Bool(t.headers)),
            ("ShowHeaders", Put) => {
                let on = self.put_bool(a)?;
                self.book_mut(b)?
                    .wb
                    .set_table_headers(&t.name, on)
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
            ("ListColumns", Get) => Ok(self.alloc(Object::ListColumns(b, handle))),
            ("ListRows", Get) => Ok(self.alloc(Object::ListRows(b, handle))),
            ("SourceType", Get) => Ok(Variant::I4(1)),
            ("Active" | "DisplayRightToLeft", Get) => Ok(Variant::Bool(false)),
            ("InsertRowRange" | "XmlMap", Get) => Ok(Variant::Dispatch(None)),
            ("TableObject" | "SharePointURL", Get) => Err(OmError::app_defined()),
            ("AlternativeText" | "Summary", Get) => {
                let key = format!("tbl{}:{}:{}", b.0, t.name, m.name);
                Ok(self.opts.get(&key).cloned().unwrap_or(Variant::BstrNull))
            }
            ("AlternativeText" | "Summary", Put) => {
                let text = self.put_string(a)?;
                self.opts.insert(
                    format!("tbl{}:{}:{}", b.0, t.name, m.name),
                    Variant::Bstr(text),
                );
                Ok(Variant::Empty)
            }
            ("TableStyle", Get) => Ok(match &t.style {
                Some(s) => self.alloc(Object::TableStyle(s.clone())),
                None => Variant::Bstr("\u{0}".to_string()),
            }),
            ("TableStyle", Put) => {
                let style = match a.put.as_ref() {
                    Some(Variant::Dispatch(Some(id))) => match self.arena.get(*id) {
                        Some(Object::TableStyle(n)) => n.clone(),
                        _ => return Err(OmError::type_mismatch()),
                    },
                    _ => self.put_string(a)?,
                };
                let valid = style.is_empty() || table_style_exists(&style);
                if !valid {
                    return Err(OmError::hresult(crate::error::DISP_E_BADPARAMCOUNT));
                }
                self.book_mut(b)?.wb.set_table_look(&t.name, |t| {
                    t.style = (!style.is_empty()).then_some(style);
                });
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
                    .set_table_look(&t.name, |t| t.stripes[idx] = on);
                Ok(Variant::Empty)
            }
            ("Comment", Get) => Ok(Variant::Bstr(t.comment.clone())),
            ("Comment", Put) => {
                let text = self.put_string(a)?;
                if text.chars().count() > 255 {
                    return Err(OmError::exception(0x8007_0057_u32 as i32, None));
                }
                self.book_mut(b)?
                    .wb
                    .set_table_look(&t.name, |t| t.comment = text);
                Ok(Variant::Empty)
            }

            ("ShowAutoFilterDropDown", Get) => Ok(Variant::Bool(
                t.show_filter
                    && !matches!(
                        self.opts.get(&format!("tbl{}:{}:dd", b.0, t.name)),
                        Some(Variant::Bool(false))
                    ),
            )),
            ("ShowAutoFilterDropDown", Put) => {
                let on = self.put_bool(a)?;
                if !t.show_filter {
                    return Err(OmError::app_defined());
                }
                self.opts
                    .insert(format!("tbl{}:{}:dd", b.0, t.name), Variant::Bool(on));
                Ok(Variant::Empty)
            }
            ("AutoFilter" | "ShowAutoFilter", _) => self.table_view_member(b, &t, m, a),
            ("Sort", Get) => Ok(self.alloc(Object::Sort(SortOwner::Table(b, handle)))),
            ("Parent", Get) => Ok(self.alloc(Object::Worksheet(b, t.sheet))),
            ("Unlist", Func) => {
                self.book_mut(b)?
                    .wb
                    .unlist_table(&t.name)
                    .map_err(edit_error)?;
                self.sorts
                    .remove(&SortOwner::Table(b, t.name.clone()).key());
                Ok(Variant::Empty)
            }
            ("Delete", Func) => {
                self.book_mut(b)?
                    .wb
                    .delete_table(&t.name)
                    .map_err(edit_error)?;
                self.sorts
                    .remove(&SortOwner::Table(b, t.name.clone()).key());
                Ok(Variant::Empty)
            }
            ("Resize", Func) => {
                let (_, rect) = self.range_arg(a.get(0))?;

                self.book_mut(b)?
                    .wb
                    .resize_table(&t.name, rect)
                    .map_err(edit_error)?;
                Ok(Variant::Empty)
            }
            _ => Err(unsupported("ListObject", m)),
        }
    }

    pub(crate) fn list_columns_member(
        &mut self,
        b: BookId,
        name: &str,
        m: &'static Member,
        a: &Args,
    ) -> Result<Variant, OmError> {
        let t = self.table_of(b, name)?;
        match m.name {
            "Count" => Ok(Variant::I4(t.columns.len() as i32)),
            "Item" | "_Default" => {
                let i = self.item_index(a.get(0), a.lcid, &t.columns, |c| c.as_str())?;
                Ok(self.alloc(Object::ListColumn(b, name.to_string(), i)))
            }
            "_NewEnum" => {
                let items = (0..t.columns.len())
                    .map(|i| self.alloc(Object::ListColumn(b, name.to_string(), i)))
                    .collect();
                Ok(self.alloc_enumerator(items))
            }
            "Add" | "_Add" => {
                let n = t.columns.len();
                let pos = match a.i32(0)? {
                    Some(p) => usize::try_from(p - 1)
                        .ok()
                        .filter(|&p| p <= n)
                        .ok_or_else(OmError::bad_index)?,
                    None => n,
                };
                let wb = &mut self.book_mut(b)?.wb;
                if pos == n {
                    let rect = Rect {
                        c1: t.rect.c1 + 1,
                        ..t.rect
                    };
                    wb.resize_table(&t.name, rect).map_err(edit_error)?;
                } else {
                    let col = t.rect.c0 + pos as u32;
                    let cells = Rect {
                        c0: col,
                        c1: col,
                        ..t.rect
                    };
                    wb.insert_cells(t.sheet, cells, false).map_err(edit_error)?;
                }
                Ok(self.alloc(Object::ListColumn(b, name.to_string(), pos)))
            }
            _ => Err(unsupported("ListColumns", m)),
        }
    }

    pub(crate) fn list_column_member(
        &mut self,
        b: BookId,
        name: &str,
        i: usize,
        m: &'static Member,
        a: &Args,
    ) -> Result<Variant, OmError> {
        use InvokeKind::*;
        let t = self.table_of(b, name)?;
        let col = t.rect.c0 + i as u32;
        let narrow = |r: Rect| Rect {
            c0: col,
            c1: col,
            ..r
        };
        match (m.name, m.kind) {
            ("Name" | "_Default", Get) => Ok(Variant::Bstr(
                t.columns
                    .get(i)
                    .cloned()
                    .ok_or_else(OmError::object_required)?,
            )),
            ("Name", Put) => {
                let new = self.put_string(a)?;
                if !t.headers {
                    return Err(OmError::app_defined());
                }
                let addr = xlcalc::CellAddr::new(t.sheet, t.rect.r0, col);
                self.book_mut(b)?
                    .wb
                    .set_input(addr, &new)
                    .map_err(edit_error)?;
                Ok(Variant::Empty)
            }
            ("Index", Get) => Ok(Variant::I4(i as i32 + 1)),
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
                let Some(row) = t.totals_row() else {
                    return Ok(Variant::I4(0));
                };
                let wb = &self.book(b)?.wb;
                let text = wb.formula_text(xlcalc::CellAddr::new(t.sheet, row, col));
                Ok(Variant::I4(match text {
                    None => 0,
                    Some(f) => totals_code(&f),
                }))
            }
            ("TotalsCalculation", Put) => {
                let code = Application::put_value(a)
                    .i32(0)?
                    .ok_or_else(OmError::type_mismatch)?;
                if !(0..=9).contains(&code) {
                    return Err(OmError::exception(0x8007_0057_u32 as i32, None));
                }
                let Some(row) = t.totals_row() else {
                    return Ok(Variant::Empty);
                };
                let addr = xlcalc::CellAddr::new(t.sheet, row, col);
                let wb = &mut self.book_mut(b)?.wb;
                match code {
                    0 => {
                        wb.clear(addr);
                    }
                    1..=8 => {
                        let f = [109, 101, 103, 102, 105, 104, 107, 110][code as usize - 1];
                        let colname = xlcalc::testing::escape_column(&t.columns[i]);
                        let text = format!("=SUBTOTAL({f},{}[{colname}])", t.name);
                        wb.set_formula(addr, &text).map_err(edit_error)?;
                    }
                    _ => {}
                }
                Ok(Variant::Empty)
            }
            _ => Err(unsupported("ListColumn", m)),
        }
    }

    pub(crate) fn table_style_member(
        &mut self,
        name: &str,
        m: &'static Member,
    ) -> Result<Variant, OmError> {
        match m.name {
            "Name" | "_Default" => Ok(Variant::Bstr(name.to_string())),
            _ => Err(unsupported("TableStyle", m)),
        }
    }

    pub(crate) fn list_rows_member(
        &mut self,
        b: BookId,
        name: &str,
        m: &'static Member,
        a: &Args,
    ) -> Result<Variant, OmError> {
        let t = self.table_of(b, name)?;
        let n = if t.emptied {
            0
        } else {
            body(&t).map_or(0, |r| r.rows() as usize)
        };
        match m.name {
            "Count" => Ok(Variant::I4(n as i32)),
            "Item" | "_Default" => {
                let rows: Vec<usize> = (0..n).collect();
                let i = self.item_index(a.get(0), a.lcid, &rows, |_| "")?;
                Ok(self.alloc(Object::ListRow(b, name.to_string(), i)))
            }
            "_NewEnum" => {
                let items = (0..n)
                    .map(|i| self.alloc(Object::ListRow(b, name.to_string(), i)))
                    .collect();
                Ok(self.alloc_enumerator(items))
            }
            _ => Err(unsupported("ListRows", m)),
        }
    }

    pub(crate) fn list_row_member(
        &mut self,
        b: BookId,
        name: &str,
        i: usize,
        m: &'static Member,
    ) -> Result<Variant, OmError> {
        let t = self.table_of(b, name)?;
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
            _ => Err(unsupported("ListRow", m)),
        }
    }
}

fn totals_code(f: &str) -> i32 {
    let up = f.to_ascii_uppercase();
    let Some(rest) = up.strip_prefix("=SUBTOTAL(") else {
        return 9;
    };
    match rest.split(',').next().map(str::trim) {
        Some("109") => 1,
        Some("101") => 2,
        Some("103") => 3,
        Some("102") => 4,
        Some("105") => 5,
        Some("104") => 6,
        Some("107") => 7,
        Some("110") => 8,
        _ => 9,
    }
}

fn table_style_exists(name: &str) -> bool {
    let (kind, max) = if let Some(n) = name.strip_prefix("TableStyleLight") {
        (n, 21)
    } else if let Some(n) = name.strip_prefix("TableStyleMedium") {
        (n, 28)
    } else if let Some(n) = name.strip_prefix("TableStyleDark") {
        (n, 11)
    } else {
        return false;
    };
    kind.parse::<u32>().is_ok_and(|n| (1..=max).contains(&n)) && !kind.starts_with('0')
}

const BACKSLASH: char = '\\';

fn display_name_ok(s: &str) -> bool {
    let mut cs = s.chars();
    let Some(first) = cs.next() else {
        return false;
    };
    if !(first.is_alphabetic() || first == '_' || first == BACKSLASH) {
        return false;
    }
    if !cs.all(|c| c.is_alphanumeric() || matches!(c, '_' | '.' | BACKSLASH | '?')) {
        return false;
    }
    let letters = s.chars().take_while(|c| c.is_ascii_alphabetic()).count();
    let rest = &s[letters..];
    let a1 =
        (1..=3).contains(&letters) && !rest.is_empty() && rest.chars().all(|c| c.is_ascii_digit());
    let up = s.to_ascii_uppercase();
    let r1c1 = up == "R"
        || up == "C"
        || up
            .strip_prefix('R')
            .is_some_and(|t| !t.is_empty() && t.chars().all(|c| c.is_ascii_digit() || c == 'C'));
    !(a1 || r1c1)
}
