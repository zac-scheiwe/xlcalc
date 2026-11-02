use xlcalc::{Rect, SheetId, Table};
use xlcalc_variant::Variant;

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
    (r.r0 <= r.r1).then_some(r)
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
                if a.i32(3)?.unwrap_or(0) != 1 {
                    return Err(OmError::unsupported(
                        "ListObjects.Add without HasHeaders:=xlYes",
                    ));
                }
                let (sheet, rect) = self.range_arg(a.get(1))?;
                if sheet != s {
                    return Err(OmError::app_defined());
                }

                let wb = &mut self.book_mut(b)?.wb;
                let mut k = wb.tables().len() + 1;
                while wb.table_by_name(&format!("Table{k}")).is_some() {
                    k += 1;
                }
                let name = format!("Table{k}");
                wb.add_table(&name, sheet, rect).map_err(edit_error)?;

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
            Object::Table(ob, n) | Object::ListColumns(ob, n) | Object::ListColumn(ob, n, _)
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
            ("Name" | "DisplayName", Get) => Ok(Variant::Bstr(t.name.clone())),
            ("Name" | "DisplayName", Put) => {
                let new = self.put_string(a)?;
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
            _ => Err(unsupported("ListColumns", m)),
        }
    }

    pub(crate) fn list_column_member(
        &mut self,
        b: BookId,
        name: &str,
        i: usize,
        m: &'static Member,
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
            ("Index", Get) => Ok(Variant::I4(i as i32 + 1)),
            ("Range", Get) => self.new_range(b, t.sheet, narrow(t.rect), Mode::Cells),
            ("DataBodyRange", Get) => self.optional_range(b, t.sheet, body(&t).map(narrow)),
            _ => Err(unsupported("ListColumn", m)),
        }
    }
}
