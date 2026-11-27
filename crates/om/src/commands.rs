use xlcalc::{
    paste_area, DateUnit, FillType, PasteOp, PasteSpecial, PasteWhat, Rect, ReplaceSpec, Selection,
    SeriesSpec, SeriesType, SheetId, SortKey, SortSpec, TableId,
};
use xlcalc_variant::Variant;

use crate::app::{
    unsupported, Application, BookId, FieldExtra, Mode, Object, RangeRef, SortOwner, SortState,
};
use crate::cmdargs::{Oper, Operands, Reader, Refused};
use crate::conv::Args;
use crate::error::OmError;
use crate::objects::edit_error;
use crate::selection::At;
use crate::typelib::Member;

fn paste_type(oper: &Oper, r: Reader) -> Result<Option<i32>, Refused> {
    const BUTTONS: [i32; 12] = [-4104, -4123, -4163, -4122, -4144, 6, 13, 7, 8, 11, 12, 14];
    let Some(n) = r.int(oper)? else {
        return Ok(Some(BUTTONS[0]));
    };
    let whole = matches!(oper, Oper::Number(x) if x.fract() == 0.0);
    let button = match n {
        7 | 8 if whole => n + 1,
        11 | 12 if whole => n - 1,
        13 if whole => 7,
        14 if whole => 12,
        1..=6 => n,
        _ if whole => 0,
        _ => n,
    };
    Ok(usize::try_from(button - 1)
        .ok()
        .and_then(|i| BUTTONS.get(i).copied()))
}

impl Application<'_> {
    fn sort_selects(&mut self, book: BookId, mark: usize, st: &SortState, sorted: bool) {
        let Some((sheet, rect)) = st.range else {
            return;
        };
        let Ok(bk) = self.book_mut(book) else {
            return;
        };
        if bk.wb.selection_of(sheet).areas != [rect] {
            let data = match (st.header, st.orientation) {
                (1, 1) if rect.r1 > rect.r0 => Rect::new(rect.r0 + 1, rect.c0, rect.r1, rect.c1),
                _ => rect,
            };
            if bk.wb.active_sheet() != sheet {
                let _ = bk.wb.set_selection(sheet, Selection::of(&[data]));
            }
            if sorted {
                self.note_selection(book, mark, At::First, sheet, &[data]);
            }
        }
    }

    fn apply_sort(
        &mut self,
        book: BookId,
        st: &SortState,
        filter: bool,
        lcid: u32,
    ) -> Result<(), OmError> {
        let Some((sheet, rect)) = st.range else {
            return Err(OmError::app_defined());
        };
        let (orientation, header, match_case) = (st.orientation, st.header, st.match_case);
        let by_columns = orientation == 2;
        let mut keys = Vec::new();
        for (i, &(ks, kr, order)) in st.fields.iter().enumerate() {
            let (lo, first, last) = if by_columns {
                (rect.r0, kr.r0, rect.r1)
            } else {
                (rect.c0, kr.c0, rect.c1)
            };
            if ks != sheet || first < lo || first > last {
                return Err(OmError::app_defined());
            }
            keys.push(SortKey {
                index: first - lo,
                descending: order == 2,
                color: st.extra.get(i).and_then(FieldExtra::sort_color),
            });
        }
        let spec = SortSpec {
            keys,
            header: header == 1,
            by_columns,
            match_case,
            custom: st.custom.clone(),
            text_as_number: (0..st.fields.len())
                .map(|i| st.extra.get(i).is_some_and(|e| e.data_option == 1))
                .collect(),
        };
        let wb = &mut self.book_mut(book)?.wb;

        let locale = crate::conv::input_locale(lcid, wb.locale());
        wb.with_input_locale(&locale, |wb| {
            if filter {
                wb.sort_filter(sheet, rect, &spec)
            } else {
                wb.sort_range(sheet, rect, &spec)
            }
        })
        .map_err(edit_error)?;
        Ok(())
    }

    fn range_sort(
        &mut self,
        r: &RangeRef,
        sheet: SheetId,
        rect: Rect,
        a: &Args,
    ) -> Result<(), OmError> {
        for i in [1, 4, 6] {
            match a.get(i) {
                Some(Variant::Null | Variant::Empty) => return Err(OmError::app_defined()),
                Some(Variant::Bstr(_)) => {
                    a.i32(i).map_err(|_| OmError::app_defined())?;
                }
                _ => {}
            }
        }
        if matches!(a.get(3), Some(Variant::Bstr(_) | Variant::Bool(_))) {
            return Err(OmError::app_defined());
        }
        if matches!(a.get(7), Some(Variant::Bstr(_))) {
            a.i32(7).map_err(|_| OmError::app_defined())?;
        }

        let num = |i: usize, lo: i32, hi: i32| -> Result<(), OmError> {
            match a.get(i) {
                Some(Variant::Bool(_) | Variant::Null | Variant::Empty) | None => Ok(()),
                _ => match a.i32(i) {
                    Ok(Some(v)) if !(lo..=hi).contains(&v) => Err(OmError::app_defined()),
                    _ => Ok(()),
                },
            }
        };
        for i in [1, 4, 6] {
            num(i, 1, 2)?;
        }
        num(7, 0, 2)?;
        num(10, 1, 2)?;

        if !matches!(a.get(3), None | Some(Variant::Null | Variant::Empty)) {
            return Err(OmError::app_defined().with_detail("Reference isn't valid."));
        }
        let header_arg = match a.get(7) {
            Some(Variant::Null | Variant::Empty) => None,
            _ => a.i32(7)?,
        };
        let mut fields = Vec::new();
        let mut extra = Vec::new();
        for (key, order, dopt) in [(0, 1, 12), (2, 4, 13), (5, 6, 14)] {
            if a.get(key).is_some()
                && !(key != 0 && matches!(a.get(key), Some(Variant::Null | Variant::Empty)))
            {
                let (ks, kr) = self.range_arg(a.get(key))?;
                let order = match a.get(order) {
                    Some(Variant::Bool(_)) => 1,
                    _ => a.i32(order)?.unwrap_or(1),
                };
                fields.push((ks, kr, order));
                extra.push(FieldExtra {
                    data_option: a.i32(dopt).ok().flatten().unwrap_or(0),
                    ..FieldExtra::default()
                });
            }
        }

        let owner = SortOwner::Sheet(r.book, sheet);
        let orientation = match a.i32(10)? {
            Some(2) => 2,
            Some(_) => 1,
            None => self.sort_state(&owner).orientation,
        };
        self.sort_state(&owner).orientation = orientation;
        let header = header_arg.unwrap_or(2);
        let match_case = match a.bool(9)? {
            Some(mc) => mc,
            None => self.sort_state(&owner).match_case,
        };

        if let Some(mc) = a.bool(9)? {
            self.sort_state(&owner).match_case = mc;
        }

        let autosel = rect.is_cell();
        let mut rect = self
            .book(r.book)?
            .wb
            .sort_auto_select(sheet, rect)
            .ok_or_else(|| {
                OmError::app_defined().with_detail("Sort method of Range class failed")
            })?;
        if self.book(r.book)?.wb.sort_straddles_table(sheet, rect) {
            return Err(OmError::app_defined().with_detail("Sort method of Range class failed"));
        }

        let header = if header == 0 {
            match self
                .book(r.book)?
                .wb
                .guess_list_header(sheet, rect, autosel)
            {
                Some(h) => {
                    rect.r0 = h;
                    1
                }
                None => 2,
            }
        } else {
            header
        };

        let oc = match a.i32(8)? {
            Some(n) => {
                self.sort_state(&owner).order_custom = n;
                n
            }
            None => self.sort_state(&owner).order_custom,
        };
        let list = self.custom_list(oc - 1).unwrap_or_default();
        let order_custom = self.sort_state(&owner).order_custom;
        let st = SortState {
            order_custom,

            custom: vec![list],
            fields,
            extra,
            range: Some((sheet, rect)),
            orientation,
            header,
            match_case,
            ..SortState::default()
        };
        self.apply_sort(r.book, &st, false, a.lcid)
    }

    fn key_arg(&self, v: Option<&Variant>, single: bool) -> Result<(SheetId, Rect), OmError> {
        if let Some(Variant::Dispatch(Some(id))) = v {
            if let Some(Object::Multi(areas)) = self.arena.get(*id) {
                return match (areas.first(), single) {
                    (Some(first), false) => self.resolve(first),
                    _ => Err(OmError::app_defined()),
                };
            }
        }

        if single && matches!(v, Some(x) if !matches!(x, Variant::Dispatch(_))) {
            return Err(OmError::arg(crate::error::DISP_E_TYPEMISMATCH, 0));
        }
        self.range_arg(v)
    }

    fn custom_list_count(&self) -> i32 {
        (0..)
            .take_while(|&n| self.custom_list(n + 1).is_some())
            .count() as i32
    }

    fn field_extra(&mut self, o: &SortOwner, i: usize) -> &mut FieldExtra {
        let st = self.sort_state(o);
        if st.extra.len() <= i {
            st.extra.resize(i + 1, FieldExtra::default());
        }
        &mut st.extra[i]
    }

    fn table_header_value(&self, o: &SortOwner) -> Result<i32, OmError> {
        let SortOwner::Table(b, id) = o else {
            return Err(OmError::app_defined());
        };
        Ok(if self.table_of(*b, *id)?.headers {
            1
        } else {
            2
        })
    }

    fn sort_state(&mut self, o: &SortOwner) -> &mut SortState {
        self.sorts.entry(o.clone()).or_default()
    }

    pub(crate) fn sort_member(
        &mut self,
        o: &SortOwner,
        m: &'static Member,
        a: &Args,
    ) -> Result<Variant, OmError> {
        use crate::typelib::InvokeKind::*;
        if let SortOwner::Table(b, id) = o {
            self.table_of(*b, *id)?;
        }
        let sheet_owned = matches!(o, SortOwner::Sheet(..));
        match (m.name, m.kind) {
            ("SortFields", Get) => Ok(self.alloc(Object::SortFields(o.clone()))),

            ("Header", Get) if matches!(o, SortOwner::Filter(..)) => Ok(Variant::I4(1)),
            ("Header", Get) if !sheet_owned => Ok(Variant::I4(self.table_header_value(o)?)),
            ("Header", Get) => Ok(Variant::I4(self.sort_state(o).header)),

            ("Header", Put) => {
                self.coerce_wb_put(m, a)?;
                let v = self.put_i32(a)?;
                if sheet_owned {
                    if !(0..=2).contains(&v) {
                        return Err(invalid_arg());
                    }
                    self.sort_state(o).header = v;
                } else if v != 0 && v != self.table_header_value(o)? {
                    return Err(invalid_arg());
                }
                Ok(Variant::Empty)
            }
            ("SortMethod", Get) => Ok(Variant::I4(self.sort_state(o).method)),

            ("SortMethod", Put) => {
                self.coerce_wb_put(m, a)?;
                let v = self.put_i32(a)?;
                if !(1..=2).contains(&v) {
                    return Err(invalid_arg());
                }
                self.sort_state(o).method = v;
                Ok(Variant::Empty)
            }
            ("Orientation", Get) if !sheet_owned => Ok(Variant::I4(1)),
            ("Orientation", Put) if !sheet_owned => {
                self.coerce_wb_put(m, a)?;
                if self.put_i32(a)? != 1 {
                    return Err(invalid_arg());
                }
                Ok(Variant::Empty)
            }
            ("Rng", Get) => {
                if let SortOwner::Table(b, id) = o {
                    let t = self.table_of(*b, *id)?;

                    let rect = Rect {
                        r1: if t.totals { t.rect.r1 - 1 } else { t.rect.r1 },
                        ..t.rect
                    };
                    return self.new_range(*b, t.sheet, rect, Mode::Cells);
                }
                let SortOwner::Sheet(b, _) = o else {
                    return Err(OmError::app_defined());
                };
                let (sheet, rect) = self.sort_state(o).range.ok_or_else(OmError::app_defined)?;
                self.new_range(*b, sheet, rect, Mode::Cells)
            }
            ("MatchCase", Get) => Ok(Variant::Bool(self.sort_state(o).match_case)),

            ("MatchCase", Put) => {
                let v = self.put_bool(a)?;
                self.sort_state(o).match_case = v;
                Ok(Variant::Empty)
            }

            ("SetRange", Func) if sheet_owned => {
                let target = self.key_arg(a.get(0), true).map_err(|e| match a.get(0) {
                    Some(Variant::Dispatch(Some(id)))
                        if matches!(self.arena.get(*id), Some(Object::Multi(_))) =>
                    {
                        invalid_arg()
                    }
                    _ => e,
                })?;
                self.sort_state(o).range = Some(target);
                Ok(Variant::Empty)
            }
            ("Orientation", Get) if sheet_owned => Ok(Variant::I4(self.sort_state(o).orientation)),

            ("Orientation", Put) if sheet_owned => {
                self.coerce_wb_put(m, a)?;
                let v = self.put_i32(a)?;
                if !(1..=2).contains(&v) {
                    return Err(invalid_arg());
                }
                self.sort_state(o).orientation = v;
                Ok(Variant::Empty)
            }
            ("Apply", Func) => {
                let st = self.sort_state(o).clone();

                if st.extra.iter().any(|e| e.sort_on == 3) {
                    return Err(OmError::unsupported("Sort.Apply with SortOn icon"));
                }
                match o {
                    SortOwner::Sheet(book, _) => {
                        if st.fields.is_empty() {
                            return if st.range.is_some() {
                                Ok(Variant::Empty)
                            } else {
                                Err(OmError::app_defined())
                            };
                        }

                        if st.fields.iter().any(|f| {
                            if st.orientation == 2 {
                                f.1.r0 != f.1.r1
                            } else {
                                f.1.c0 != f.1.c1
                            }
                        }) {
                            return Err(OmError::app_defined_with(
                                "The sort reference is not valid. Make sure that it's within the                                  data you want to sort, and the first Sort By box isn't the same                                  or blank.",
                            ));
                        }
                        let mark = self.notice_mark(*book);
                        let r = self.apply_sort(*book, &st, false, a.lcid);
                        self.sort_selects(*book, mark, &st, r.is_ok());
                        r?;

                        if let (Some((sheet, rect)), 1) = (st.range, st.orientation) {
                            let wb = &self.book(*book)?.wb;
                            let head = u32::from(st.header == 1 && rect.r1 > rect.r0);
                            let first = (rect.r0 + head..=rect.r1)
                                .find(|&r| !wb.row_hidden(sheet, r))
                                .unwrap_or(rect.r1);
                            let r1 = (first..=rect.r1)
                                .rev()
                                .find(|&r| !wb.row_hidden(sheet, r))
                                .unwrap_or(first);
                            let r0 = first.saturating_sub(head).max(rect.r0);
                            self.sort_state(o).range = Some((sheet, Rect { r0, r1, ..rect }));
                        }
                    }
                    SortOwner::Filter(book, sheet) => {
                        let rect = self.range_filter_rect(*book, *sheet)?;
                        self.sort_state(o).range = Some((*sheet, rect));
                        self.sort_range_filter(*book, *sheet, a.lcid)?;
                    }
                    SortOwner::Table(b, id) => {
                        let t = self.table_of(*b, *id)?;
                        let spec = SortSpec {
                            keys: st
                                .fields
                                .iter()
                                .enumerate()
                                .map(|(i, &(_, kr, order))| SortKey {
                                    index: kr.c0 - t.rect.c0,
                                    descending: order == 2,
                                    color: st.extra.get(i).and_then(FieldExtra::sort_color),
                                })
                                .collect(),
                            header: false,
                            by_columns: false,
                            match_case: st.match_case,
                            custom: st.custom.clone(),
                            text_as_number: (0..st.fields.len())
                                .map(|i| st.extra.get(i).is_some_and(|e| e.data_option == 1))
                                .collect(),
                        };
                        self.book_mut(*b)?
                            .wb
                            .sort_table(*id, &spec)
                            .map_err(edit_error)?;

                        let wb = &mut self.book_mut(*b)?.wb;
                        if wb.active_sheet() != t.sheet && t.rect.r1 > t.rect.r0 {
                            let data = Rect::new(t.rect.r0 + 1, t.rect.c0, t.rect.r1, t.rect.c1);
                            let _ = wb.set_selection(t.sheet, Selection::of(&[data]));
                        }
                    }
                }
                Ok(Variant::Empty)
            }
            _ => Err(unsupported("Sort", m)),
        }
    }

    pub(crate) fn table_filter_sort_member(
        &mut self,
        b: BookId,
        id: TableId,
        m: &'static Member,
        a: &Args,
    ) -> Result<Variant, OmError> {
        if (m.name, m.kind) != ("Apply", crate::typelib::InvokeKind::Func) {
            return self.sort_member(&SortOwner::Table(b, id), m, a);
        }
        let sheet = self.table_of(b, id)?.sheet;

        self.range_filter_rect(b, sheet)?;
        if self
            .sort_state(&SortOwner::Filter(b, sheet))
            .range
            .is_some()
        {
            self.sort_range_filter(b, sheet, a.lcid)?;
        }
        Ok(Variant::Empty)
    }

    fn range_filter_rect(&self, book: BookId, sheet: SheetId) -> Result<Rect, OmError> {
        self.book(book)?
            .wb
            .autofilter_view(sheet, None)
            .map(|v| v.rect)
            .ok_or_else(OmError::app_defined)
    }

    fn sort_range_filter(
        &mut self,
        book: BookId,
        sheet: SheetId,
        lcid: u32,
    ) -> Result<(), OmError> {
        let rect = self.range_filter_rect(book, sheet)?;
        let st = self.sort_state(&SortOwner::Filter(book, sheet)).clone();
        if st.fields.is_empty() {
            return Ok(());
        }
        let st = SortState {
            range: Some((sheet, rect)),
            header: 1,
            ..st
        };
        self.apply_sort(book, &st, true, lcid)
    }

    pub(crate) fn sort_fields_member(
        &mut self,
        o: &SortOwner,
        m: &'static Member,
        a: &Args,
    ) -> Result<Variant, OmError> {
        if let SortOwner::Table(b, id) = o {
            self.table_of(*b, *id)?;
        }
        match m.name {
            "Count" => Ok(Variant::I4(self.sort_state(o).fields.len() as i32)),
            "Clear" => {
                let st = self.sort_state(o);
                st.fields.clear();
                st.custom.clear();
                st.extra.clear();

                if matches!(o, SortOwner::Sheet(..)) {
                    st.orientation = 1;
                    st.match_case = false;
                    st.range = None;
                }
                Ok(Variant::Empty)
            }
            "Item" | "_Default" => {
                let items: Vec<usize> = (0..self.sort_state(o).fields.len()).collect();

                let i = self
                    .item_index(a.get(0), a.lcid, &items, |_| "")
                    .map_err(|e| match a.get(0) {
                        Some(Variant::Bstr(_)) => {
                            OmError::exception(crate::error::SCODE_TYPE_MISMATCH, None)
                        }
                        Some(_) if matches!(a.i32(0), Ok(Some(n)) if n <= 0) => {
                            OmError::app_defined()
                        }
                        _ => e,
                    })?;
                Ok(self.alloc(Object::SortField(o.clone(), i)))
            }
            "Add" | "Add2" => {
                let missing = |i: usize| a.get(i).is_none();
                if !missing(3) && !missing(5) {
                    return Err(invalid_arg());
                }
                let (ks, kr) = self.key_arg(a.get(0), false)?;
                let sort_on = a.i32(1)?.unwrap_or(0);
                if !(0..=3).contains(&sort_on) {
                    return Err(OmError::app_defined());
                }

                if a.get(2).is_some() && !matches!(a.i32(2)?, Some(1 | 2)) {
                    return Err(OmError::app_defined());
                }

                let (list, custom_order) = match a.get(3) {
                    None => (Vec::new(), None),
                    Some(Variant::Bstr(s)) => (
                        s.split(',').map(|e| e.trim().to_string()).collect(),
                        Some(Variant::Bstr(s.clone())),
                    ),

                    Some(_) => {
                        let n = a.i32(3)?.unwrap_or(0);
                        if n < 0 || n > self.custom_list_count() {
                            return Err(OmError::app_defined());
                        }
                        (
                            self.custom_list(n).unwrap_or_default(),
                            Some(Variant::Int(n)),
                        )
                    }
                };
                let data_option = a.i32(4)?.unwrap_or(0);
                if !(0..=1).contains(&data_option) {
                    return Err(OmError::app_defined());
                }
                let sub_field = match a.get(5) {
                    None => None,
                    Some(Variant::Bstr(s)) => Some(s.clone()),
                    Some(_) => return Err(invalid_arg()),
                };
                if let SortOwner::Table(b, id) = o {
                    let t = self.table_of(*b, *id)?;
                    if ks != t.sheet || kr.c0 < t.rect.c0 || kr.c1 > t.rect.c1 || kr.c0 != kr.c1 {
                        return Err(OmError::app_defined());
                    }
                }
                let order = a.i32(2)?.unwrap_or(1);
                let st = self.sort_state(o);
                st.fields.push((ks, kr, order));
                st.custom.push(list);
                st.extra.push(FieldExtra {
                    sort_on,
                    data_option,
                    custom_order,
                    sub_field,
                    ..FieldExtra::default()
                });
                let i = st.fields.len() - 1;
                Ok(self.alloc(Object::SortField(o.clone(), i)))
            }
            _ => Err(unsupported("SortFields", m)),
        }
    }

    pub(crate) fn sort_field_member(
        &mut self,
        o: &SortOwner,
        i: usize,
        m: &'static Member,
        a: &Args,
    ) -> Result<Variant, OmError> {
        use crate::typelib::InvokeKind::*;
        let order = self
            .sorts
            .get(&o.clone())
            .and_then(|s| s.fields.get(i))
            .map(|f| f.2)
            .ok_or_else(OmError::object_required)?;
        match (m.name, m.kind) {
            ("Order", Get) => Ok(Variant::I4(order)),
            ("Key", Get) => {
                let book = match o {
                    SortOwner::Sheet(b, _) | SortOwner::Filter(b, _) | SortOwner::Table(b, _) => *b,
                };
                let (sheet, rect) = self
                    .sort_state(o)
                    .fields
                    .get(i)
                    .map(|f| (f.0, f.1))
                    .ok_or_else(OmError::object_required)?;
                self.new_range(book, sheet, rect, Mode::Cells)
            }
            ("SortOnValue", Get) => {
                let on = self.field_extra(o, i).sort_on;
                if !(1..=2).contains(&on) {
                    return Err(OmError::app_defined());
                }
                Ok(self.alloc(Object::SortOnValue(o.clone(), i, on == 2)))
            }
            ("SortOn", Get) => Ok(Variant::I4(self.field_extra(o, i).sort_on)),

            ("SortOn", Put) => {
                self.coerce_wb_put(m, a)?;
                let v = self.put_i32(a)?;
                if !(0..=3).contains(&v) {
                    return Err(invalid_arg());
                }
                self.field_extra(o, i).sort_on = v;
                Ok(Variant::Empty)
            }
            ("DataOption", Get) => Ok(Variant::I4(self.field_extra(o, i).data_option)),
            ("DataOption", Put) => {
                self.coerce_wb_put(m, a)?;
                let v = self.put_i32(a)?;
                if !(0..=2).contains(&v) {
                    return Err(invalid_arg());
                }
                self.field_extra(o, i).data_option = v;
                Ok(Variant::Empty)
            }
            ("Priority", Get) => Ok(Variant::I4(i as i32 + 1)),

            ("Priority", Put) => {
                self.coerce_wb_put(m, a)?;
                let v = self.put_i32(a)?;
                let st = self.sort_state(o);
                if v < 1 || v as usize > st.fields.len() {
                    return Err(invalid_arg());
                }
                let to = v as usize - 1;
                st.extra.resize(st.fields.len(), FieldExtra::default());
                st.custom.resize(st.fields.len(), Vec::new());
                let f = st.fields.remove(i);
                st.fields.insert(to, f);
                let e = st.extra.remove(i);
                st.extra.insert(to, e);
                let c = st.custom.remove(i);
                st.custom.insert(to, c);
                Ok(Variant::Empty)
            }

            ("Delete", Func) => {
                let st = self.sort_state(o);
                st.fields.remove(i);
                if i < st.extra.len() {
                    st.extra.remove(i);
                }
                if i < st.custom.len() {
                    st.custom.remove(i);
                }
                Ok(Variant::Empty)
            }

            ("ModifyKey", Func) => {
                let key = self.key_arg(a.get(0), true)?;
                if let Some(f) = self.sort_state(o).fields.get_mut(i) {
                    *f = (key.0, key.1, f.2);
                }
                Ok(Variant::Empty)
            }
            ("SubField", Get) => Ok(match self.field_extra(o, i).sub_field.clone() {
                Some(s) => Variant::Bstr(s),
                None => Variant::Empty,
            }),
            ("SubField", Put) => match Application::put_value(a).get(0) {
                Some(Variant::Bstr(s)) => {
                    self.field_extra(o, i).sub_field = Some(s.clone());
                    Ok(Variant::Empty)
                }
                _ => Err(invalid_arg()),
            },
            ("CustomOrder", Get) => Ok(self
                .field_extra(o, i)
                .custom_order
                .clone()
                .unwrap_or(Variant::Int(0))),
            ("CustomOrder", Put) => {
                let put = Application::put_value(a);
                let stored = match put.get(0) {
                    Some(Variant::Bstr(s)) => Some(Variant::Bstr(s.clone())),
                    Some(Variant::Bool(_)) => return Err(invalid_arg()),
                    Some(Variant::Empty | Variant::Null) | None => None,
                    Some(_) => put.i32(0)?.map(Variant::Int),
                };
                self.field_extra(o, i).custom_order = stored;
                Ok(Variant::Empty)
            }

            ("Order", Put) => {
                self.coerce_wb_put(m, a)?;
                let v = self.put_i32(a)?;
                if !(1..=2).contains(&v) {
                    return Err(invalid_arg());
                }
                if let Some(f) = self.sort_state(o).fields.get_mut(i) {
                    f.2 = v;
                }
                Ok(Variant::Empty)
            }
            _ => Err(unsupported("SortField", m)),
        }
    }

    pub(crate) fn sort_on_value_member(
        &mut self,
        o: &SortOwner,
        i: usize,
        m: &'static Member,
        a: &Args,
    ) -> Result<Variant, OmError> {
        use crate::typelib::InvokeKind::*;
        match (m.name, m.kind) {
            ("Color", Get) => Ok(Variant::R8(f64::from(
                self.field_extra(o, i).color.unwrap_or(-4142),
            ))),
            ("Color", Put) => {
                self.coerce_wb_put(m, a)?;
                let v = self.put_i32(a)?;
                self.field_extra(o, i).color = Some(v);
                Ok(Variant::Empty)
            }
            _ => Err(unsupported("SortOnValue", m)),
        }
    }

    fn range_autofill(
        &mut self,
        book: BookId,
        sheet: SheetId,
        fill: (Rect, Rect),
        o: &Operands,
        lcid: u32,
    ) -> Result<Variant, OmError> {
        let (rect, dst) = fill;
        let reader = Reader {
            wb: &self.book(book)?.wb,
            lcid,
        };
        let ty_arg = reader.int(o.get(1)).map_err(|_| OmError::type_mismatch())?;
        let ty = match ty_arg.unwrap_or(0) {
            0 => FillType::Default,
            1 => FillType::Copy,
            2 => FillType::Series,
            3 => FillType::Formats,
            4 => FillType::Values,
            5 => FillType::Days,
            6 => FillType::Weekdays,
            7 => FillType::Months,
            8 => FillType::Years,
            9 => FillType::Linear,
            10 => FillType::Growth,

            12 => FillType::Copy,
            _ => return Err(OmError::app_defined()),
        };

        let mark = self.notice_mark(book);
        self.book_mut(book)?
            .wb
            .autofill(sheet, rect, dst, ty)
            .map_err(edit_error)?;

        if let Some(rest) = extension(rect, dst) {
            self.carry_objects(book, (sheet, rect), (sheet, rest), false)?;
        }
        self.note_selection(book, mark, At::BeforeChange, sheet, &[dst]);
        Ok(Variant::Bool(true))
    }

    fn range_data_series(
        &mut self,
        book: BookId,
        at: (SheetId, Rect),
        o: &Operands,
        lcid: u32,
    ) -> Result<Variant, OmError> {
        let (sheet, rect) = at;
        let r = Reader {
            wb: &self.book(book)?.wb,
            lcid,
        };
        let num = |i: usize| r.number(o.get(i)).map_err(|_| OmError::app_defined());
        let int = |i: usize| r.int(o.get(i)).map_err(|_| OmError::app_defined());
        let spec = SeriesSpec {
            rows: match int(0)? {
                None => None,
                Some(1) => Some(true),
                Some(2) => Some(false),
                Some(_) => return Err(OmError::app_defined()),
            },
            ty: match int(1)? {
                None => None,
                Some(1) => Some(SeriesType::Linear),
                Some(2) => Some(SeriesType::Growth),
                Some(3) => Some(SeriesType::Date),
                Some(4) => Some(SeriesType::AutoFill),
                Some(_) => return Err(OmError::app_defined()),
            },
            unit: match int(2)? {
                None => None,
                Some(1) => Some(DateUnit::Day),
                Some(2) => Some(DateUnit::Weekday),
                Some(3) => Some(DateUnit::Month),
                Some(4) => Some(DateUnit::Year),
                Some(_) => return Err(OmError::app_defined()),
            },
            step: num(3)?,
            stop: num(4)?,
            trend: r
                .flag(o.get(5))
                .map_err(|_| OmError::app_defined())?
                .unwrap_or(false),
        };
        self.book_mut(book)?
            .wb
            .data_series(sheet, rect, &spec)
            .map_err(edit_error)?;
        Ok(Variant::Bool(true))
    }

    fn range_paste(
        &mut self,
        book: BookId,
        at: (SheetId, Rect),
        clip: (SheetId, Rect),
        o: &Operands,
        lcid: u32,
    ) -> Result<Variant, OmError> {
        let (sheet, rect) = at;
        let (ss, src) = clip;

        let r = Reader {
            wb: &self.book(book)?.wb,
            lcid,
        };
        let code = paste_type(o.get(0), r).map_err(|_| OmError::type_mismatch())?;
        let op = r.int(o.get(1)).map_err(|_| OmError::type_mismatch())?;
        let flag = |i: usize| r.flag(o.get(i)).map_err(|_| OmError::type_mismatch());
        let (skip_blanks, transpose) = (flag(2)?, flag(3)?);

        let what = code.map(|code| match code {
            -4163 | 12 => PasteWhat::Values,
            -4123 | 11 => PasteWhat::Formulas,
            -4122 => PasteWhat::Formats,
            6 => PasteWhat::Validation,
            _ => PasteWhat::All,
        });
        let code = code.unwrap_or(0);
        let op = match op.unwrap_or(1) {
            1 => PasteOp::None,
            2 => PasteOp::Add,
            3 => PasteOp::Sub,
            4 => PasteOp::Mul,
            5 => PasteOp::Div,
            _ => return Err(OmError::app_defined()),
        };
        let spec = PasteSpecial {
            what: what.unwrap_or(PasteWhat::All),
            op,
            skip_blanks: skip_blanks.unwrap_or(false),
            transpose: transpose.unwrap_or(false),
        };

        self.with_sheet_shown(book, sheet, true, |app| {
            let mark = app.notice_mark(book);
            let wb = &mut app.book_mut(book)?.wb;
            let old = wb.selection_of(sheet).areas;
            wb.set_selection(sheet, Selection::of(&[rect]))
                .map_err(edit_error)?;
            if what.is_none() {
                if old != [rect] {
                    app.note_selection(book, mark, At::First, sheet, &[rect]);
                }
                return Err(OmError::app_defined_with(
                    "PasteSpecial method of Range class failed",
                ));
            }
            let shape = if spec.transpose {
                Rect::new(0, 0, src.cols() - 1, src.rows() - 1)
            } else {
                src
            };
            let pasted = paste_area(shape, rect);
            if let Err(e) = app.paste_guard(book, (ss, src), (sheet, pasted), false) {
                app.book_mut(book)?
                    .wb
                    .set_selection(sheet, Selection::of(&[pasted]))
                    .map_err(edit_error)?;
                return Err(e);
            }
            let kept = app.locked_snapshot(book, sheet, pasted);
            let wb = &mut app.book_mut(book)?.wb;
            if code == 8 {
                let n = if rect.cols().is_multiple_of(src.cols()) {
                    rect.cols()
                } else {
                    src.cols()
                };
                for k in 0..n.min(crate::addr::max_cols() - rect.c0) {
                    wb.paste_column_width(ss, src.c0 + k % src.cols(), sheet, rect.c0 + k);
                }
            } else if code == 7 {
                wb.paste_special_keep_borders(ss, src, sheet, rect, &spec)
                    .map_err(edit_error)?;
            } else if code != -4144 {
                wb.paste_special(ss, src, sheet, rect, &spec)
                    .map_err(edit_error)?;
            }
            if matches!(code, 11 | 12) {
                wb.paste_number_formats(ss, src, sheet, rect, spec.transpose);
            }

            if matches!(code, -4104 | -4144) && !spec.transpose {
                wb.notes_paste(ss, src, sheet, pasted, false);
            }
            app.restore_locked(book, &kept);
            let wb = &mut app.book_mut(book)?.wb;
            wb.set_selection(sheet, Selection::of(&[pasted]))
                .map_err(edit_error)?;
            if old != [rect] {
                app.note_selection(book, mark, At::First, sheet, &[rect]);
            }
            Ok(Variant::Bool(true))
        })
    }

    pub(crate) fn command_member(
        &mut self,
        r: &RangeRef,
        sheet: SheetId,
        rect: Rect,
        m: &'static Member,
        a: &Args,
    ) -> Result<Variant, OmError> {
        let book = r.book;
        match m.name {
            "Sort" => {
                self.range_sort(r, sheet, rect, a)?;
                Ok(Variant::Bool(true))
            }
            "AutoFill" => {
                let (_, dst) = self.range_arg(a.get(0))?;
                let operands = self
                    .range_operands(m.dispid, a)
                    .map_err(|_| OmError::type_mismatch())?;
                let refused = OmError::type_mismatch();

                let mut from = rect;
                self.run_command(book, a.lcid, operands, &refused, |app, o| {
                    let filled = app.range_autofill(book, sheet, (from, dst), o, a.lcid)?;
                    from = dst;
                    Ok(filled)
                })
            }
            "DataSeries" => {
                let operands = self
                    .range_operands(m.dispid, a)
                    .map_err(|_| OmError::app_defined())?;
                let refused = OmError::app_defined();
                self.run_command(book, a.lcid, operands, &refused, |app, o| {
                    app.range_data_series(book, (sheet, rect), o, a.lcid)
                })
            }
            "PasteSpecial" => {
                let clip = self.clip.filter(|_| !self.clip_cut);
                let Some((cb, ss, src)) = clip else {
                    self.book_mut(book)?
                        .wb
                        .set_active_sheet(sheet)
                        .map_err(edit_error)?;
                    return Err(OmError::app_defined_with(
                        "PasteSpecial method of Range class failed",
                    ));
                };
                if cb != book {
                    return Err(unsupported("Range", m));
                }

                if !self.clip_more.0.is_empty() {
                    if a.bool(3)?.unwrap_or(false) {
                        return Err(unsupported("Range", m));
                    }
                    let (more, stacked) = self.clip_more.clone();
                    let mut areas = vec![src];
                    areas.extend(more.iter().copied());
                    self.clip_more.0.clear();
                    let mut off = 0;
                    let mut res = Ok(Variant::Bool(true));
                    for part in &areas {
                        let at = if stacked {
                            Rect::new(rect.r0 + off, rect.c0, rect.r0 + off, rect.c0)
                        } else {
                            Rect::new(rect.r0, rect.c0 + off, rect.r0, rect.c0 + off)
                        };
                        self.clip = Some((cb, ss, *part));
                        res = self.command_member(r, sheet, at, m, a);
                        if res.is_err() {
                            break;
                        }
                        off += if stacked { part.rows() } else { part.cols() };
                    }
                    self.clip = Some((cb, ss, src));
                    self.clip_more = (more, stacked);
                    return res;
                }

                let operands = self
                    .range_operands(m.dispid, a)
                    .map_err(|_| OmError::type_mismatch())?;
                let refused = OmError::type_mismatch();
                self.run_command(book, a.lcid, operands, &refused, |app, o| {
                    app.range_paste(book, (sheet, rect), (ss, src), o, a.lcid)
                })
            }
            _ => {
                let mut st = self.find.clone();

                let null_what = matches!(a.get(0), Some(Variant::Null));
                let what = if null_what {
                    String::new()
                } else {
                    self.search_text_arg(book, a, 0)?
                        .unwrap_or_else(|| st.what.clone().unwrap_or_default())
                };
                let with = if matches!(a.get(1), Some(Variant::Null)) {
                    String::new()
                } else {
                    self.search_text_arg(book, a, 1)?.unwrap_or_default()
                };
                st.take_replace(a)?;
                self.find = st.clone();
                let spec = ReplaceSpec {
                    what,
                    with,
                    whole: st.whole || null_what,
                    by_columns: st.by_columns,
                    match_case: st.match_case,
                };
                self.book_mut(book)?
                    .wb
                    .replace_range(sheet, rect, &spec)
                    .map_err(edit_error)?;
                Ok(Variant::Bool(true))
            }
        }
    }
}

fn extension(src: Rect, dst: Rect) -> Option<Rect> {
    if dst.r1 > src.r1 {
        Some(Rect::new(src.r1 + 1, dst.c0, dst.r1, dst.c1))
    } else if dst.r0 < src.r0 {
        Some(Rect::new(dst.r0, dst.c0, src.r0 - 1, dst.c1))
    } else if dst.c1 > src.c1 {
        Some(Rect::new(dst.r0, src.c1 + 1, dst.r1, dst.c1))
    } else if dst.c0 < src.c0 {
        Some(Rect::new(dst.r0, dst.c0, dst.r1, src.c0 - 1))
    } else {
        None
    }
}

fn invalid_arg() -> OmError {
    OmError::exception(0x8007_0057_u32 as i32, None)
}

#[cfg(test)]
mod tests {
    use super::paste_type;
    use crate::cmdargs::{Oper, Reader};
    use xlcalc::Workbook;

    #[test]
    fn paste_numbers_follow_the_dialog_buttons() {
        let wb = Workbook::new();
        let reader = Reader { wb: &wb, lcid: 0 };
        let paste = |x: f64| paste_type(&Oper::Number(x), reader);
        assert_eq!(paste(1.0), Ok(Some(-4104)));
        assert_eq!(paste(2.0), Ok(Some(-4123)));
        assert_eq!(paste(7.0), Ok(Some(7)));
        assert_eq!(paste(7.5), Ok(Some(13)));
        assert_eq!(paste(14.0), Ok(Some(14)));
        assert_eq!(paste(9.0), Ok(None));
        assert_eq!(paste(0.0), Ok(None));
    }
}
