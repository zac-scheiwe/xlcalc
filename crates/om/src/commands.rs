use xlcalc::{
    paste_area, DateUnit, FillType, PasteOp, PasteSpecial, PasteWhat, Rect, ReplaceSpec, Selection,
    SeriesSpec, SeriesType, SheetId, SortKey, SortSpec,
};
use xlcalc_variant::Variant;

use crate::app::{
    unsupported, Application, BookId, FieldExtra, Mode, Object, RangeRef, SortOwner, SortState,
};
use crate::conv::Args;
use crate::error::OmError;
use crate::objects::edit_error;
use crate::selection::At;
use crate::typelib::Member;

impl Application<'_> {
    fn sort_selects(&mut self, book: BookId, mark: usize, st: &SortState, sorted: bool) {
        let Some((sheet, rect)) = st.range else {
            return;
        };
        let Ok(bk) = self.book_mut(book) else {
            return;
        };
        if bk.wb.selection_of(sheet).areas != [rect] {
            if bk.wb.active_sheet() != sheet {
                let data = match (st.header, st.orientation) {
                    (1, 1) if rect.r1 > rect.r0 => {
                        Rect::new(rect.r0 + 1, rect.c0, rect.r1, rect.c1)
                    }
                    _ => rect,
                };
                let _ = bk.wb.set_selection(sheet, Selection::of(&[data]));
            }
            if sorted {
                self.note_selection(book, mark, At::First, sheet, &[rect]);
            }
        }
    }

    fn apply_sort(&mut self, book: BookId, st: &SortState) -> Result<(), OmError> {
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
        };
        self.book_mut(book)?
            .wb
            .sort_range(sheet, rect, &spec)
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
        let mut fields = Vec::new();
        for (key, order) in [(0, 1), (2, 4), (5, 6)] {
            if a.get(key).is_some() {
                let (ks, kr) = self.range_arg(a.get(key))?;
                fields.push((ks, kr, a.i32(order)?.unwrap_or(1)));
            }
        }

        let owner = SortOwner::Sheet(r.book, sheet);
        let orientation = match a.i32(10)? {
            Some(2) => 2,
            Some(_) => 1,
            None => self.sort_state(&owner).orientation,
        };
        self.sort_state(&owner).orientation = orientation;
        let header = a.i32(7)?.unwrap_or(2);
        let match_case = a.bool(9)?.unwrap_or(false);

        let rect = if rect.is_cell() {
            self.book(r.book)?.wb.current_region(sheet, rect)
        } else {
            rect
        };

        let header = if header == 0 {
            if self.guess_header(r.book, sheet, rect) {
                1
            } else {
                2
            }
        } else {
            header
        };

        let list = a
            .i32(8)?
            .and_then(|n| self.custom_list(n - 1))
            .unwrap_or_default();
        let st = SortState {
            custom: vec![list; fields.len()],
            fields,
            range: Some((sheet, rect)),
            orientation,
            header,
            match_case,
            ..SortState::default()
        };
        self.apply_sort(r.book, &st)
    }

    fn field_extra(&mut self, o: &SortOwner, i: usize) -> &mut FieldExtra {
        let st = self.sort_state(o);
        if st.extra.len() <= i {
            st.extra.resize(i + 1, FieldExtra::default());
        }
        &mut st.extra[i]
    }

    fn table_header_value(&self, o: &SortOwner) -> Result<i32, OmError> {
        let SortOwner::Table(b, name) = o else {
            return Err(OmError::app_defined());
        };
        Ok(if self.table_of(*b, name)?.headers {
            1
        } else {
            2
        })
    }

    fn sort_state(&mut self, o: &SortOwner) -> &mut SortState {
        self.sorts.entry(o.key()).or_default()
    }

    pub(crate) fn sort_member(
        &mut self,
        o: &SortOwner,
        m: &'static Member,
        a: &Args,
    ) -> Result<Variant, OmError> {
        use crate::typelib::InvokeKind::*;
        if let SortOwner::Table(b, name) = o {
            self.table_of(*b, name)?;
        }
        let sheet_owned = matches!(o, SortOwner::Sheet(..));
        match (m.name, m.kind) {
            ("SortFields", Get) => Ok(self.alloc(Object::SortFields(o.clone()))),

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
                if let SortOwner::Table(b, name) = o {
                    let t = self.table_of(*b, name)?;

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
                let target = self.range_arg(a.get(0))?;
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
                match o {
                    SortOwner::Sheet(book, _) => {
                        if st.fields.is_empty() {
                            return Err(OmError::app_defined());
                        }
                        let mark = self.notice_mark(*book);
                        let r = self.apply_sort(*book, &st);
                        self.sort_selects(*book, mark, &st, r.is_ok());
                        r?;
                    }
                    SortOwner::Table(b, name) => {
                        let t = self.table_of(*b, name)?;
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
                        };
                        self.book_mut(*b)?
                            .wb
                            .sort_table(&t.name, &spec)
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

    pub(crate) fn sort_fields_member(
        &mut self,
        o: &SortOwner,
        m: &'static Member,
        a: &Args,
    ) -> Result<Variant, OmError> {
        if let SortOwner::Table(b, name) = o {
            self.table_of(*b, name)?;
        }
        match m.name {
            "Count" => Ok(Variant::I4(self.sort_state(o).fields.len() as i32)),
            "Clear" => {
                let st = self.sort_state(o);
                st.fields.clear();
                st.custom.clear();
                st.extra.clear();
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
            "Add" => {
                let (ks, kr) = self.range_arg(a.get(0))?;
                let sort_on = a.i32(1)?.unwrap_or(0);
                if !(0..=2).contains(&sort_on) {
                    return Err(OmError::unsupported("SortFields.Add with SortOn icon"));
                }
                if a.i32(4)?.unwrap_or(0) != 0 {
                    return Err(OmError::unsupported("SortFields.Add with DataOption"));
                }

                let list = match a.get(3) {
                    None => Vec::new(),
                    Some(Variant::Bstr(_)) => a
                        .string(3)?
                        .unwrap_or_default()
                        .split(',')
                        .map(|e| e.trim().to_string())
                        .collect(),

                    Some(_) => a
                        .i32(3)?
                        .and_then(|n| self.custom_list(n))
                        .unwrap_or_default(),
                };
                if let SortOwner::Table(b, name) = o {
                    let t = self.table_of(*b, name)?;
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
            .get(&o.key())
            .and_then(|s| s.fields.get(i))
            .map(|f| f.2)
            .ok_or_else(OmError::object_required)?;
        match (m.name, m.kind) {
            ("Order", Get) => Ok(Variant::I4(order)),
            ("Key", Get) => {
                let book = match o {
                    SortOwner::Sheet(b, _) | SortOwner::Table(b, _) => *b,
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
                if !(0..=2).contains(&v) {
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
                if v != i as i32 + 1 {
                    return Err(invalid_arg());
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
            ("CustomOrder", Get) => Ok(match self.field_extra(o, i).custom_order.clone() {
                Some(s) => Variant::Bstr(s),
                None => Variant::I4(0),
            }),
            ("CustomOrder", Put) => match Application::put_value(a).get(0) {
                Some(Variant::Bstr(s)) => {
                    self.field_extra(o, i).custom_order = Some(s.clone());
                    Ok(Variant::Empty)
                }
                Some(Variant::Bool(_)) => Err(invalid_arg()),
                _ => {
                    self.field_extra(o, i).custom_order = None;
                    Ok(Variant::Empty)
                }
            },
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
                let ty = match a.i32(1)?.unwrap_or(0) {
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
                self.note_selection(book, mark, At::BeforeChange, sheet, &[dst]);
                Ok(Variant::Bool(true))
            }
            "DataSeries" => {
                let num = |i: usize| -> Result<Option<f64>, OmError> {
                    match a.get(i) {
                        None => Ok(None),
                        Some(Variant::R8(x)) => Ok(Some(*x)),
                        Some(Variant::I4(x)) => Ok(Some(f64::from(*x))),
                        Some(_) => Err(OmError::app_defined()),
                    }
                };
                let spec = SeriesSpec {
                    rows: match a.i32(0)? {
                        None => None,
                        Some(1) => Some(true),
                        Some(2) => Some(false),
                        Some(_) => return Err(OmError::app_defined()),
                    },
                    ty: match a.i32(1)? {
                        None => None,
                        Some(1 | -4132) => Some(SeriesType::Linear),
                        Some(2) => Some(SeriesType::Growth),
                        Some(3) => Some(SeriesType::Date),
                        Some(4) => Some(SeriesType::AutoFill),
                        Some(_) => return Err(OmError::app_defined()),
                    },
                    unit: match a.i32(2)? {
                        None => None,
                        Some(1) => Some(DateUnit::Day),
                        Some(2) => Some(DateUnit::Weekday),
                        Some(3) => Some(DateUnit::Month),
                        Some(4) => Some(DateUnit::Year),
                        Some(_) => return Err(OmError::app_defined()),
                    },
                    step: num(3)?,
                    stop: num(4)?,
                    trend: a.bool(5)?.unwrap_or(false),
                };
                self.book_mut(book)?
                    .wb
                    .data_series(sheet, rect, &spec)
                    .map_err(edit_error)?;
                Ok(Variant::Bool(true))
            }
            "PasteSpecial" => {
                let Some((cb, ss, src)) = self.clip else {
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
                let code = a.i32(0)?.unwrap_or(-4104);
                let what = match code {
                    -4104 | -4144 => PasteWhat::All,
                    -4163 => PasteWhat::Values,
                    -4123 => PasteWhat::Formulas,
                    -4122 => PasteWhat::Formats,
                    -4114 => return Err(OmError::app_defined()),
                    6 => PasteWhat::Validation,
                    _ => {
                        return Err(OmError::app_defined_with(
                            "PasteSpecial method of Range class failed",
                        ))
                    }
                };
                let op = match a.i32(1)?.unwrap_or(-4142) {
                    -4142 | 1 => PasteOp::None,
                    2 => PasteOp::Add,
                    3 => PasteOp::Sub,
                    4 => PasteOp::Mul,
                    5 => PasteOp::Div,
                    _ => return Err(OmError::app_defined()),
                };
                let spec = PasteSpecial {
                    what,
                    op,
                    skip_blanks: a.bool(2)?.unwrap_or(false),
                    transpose: a.bool(3)?.unwrap_or(false),
                };

                self.with_sheet_shown(book, sheet, true, |app| {
                    let mark = app.notice_mark(book);
                    let wb = &mut app.book_mut(book)?.wb;
                    let old = wb.selection_of(sheet).areas;
                    wb.set_selection(sheet, Selection::of(&[rect]))
                        .map_err(edit_error)?;
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
                    if code != -4144 {
                        wb.paste_special(ss, src, sheet, rect, &spec)
                            .map_err(edit_error)?;
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
            _ => {
                let mut st = self.find.clone();
                st.take_replace(a)?;
                self.find = st.clone();
                let spec = ReplaceSpec {
                    what: a.string(0)?.unwrap_or_default(),
                    with: a.string(1)?.unwrap_or_default(),
                    whole: st.whole,
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

fn invalid_arg() -> OmError {
    OmError::exception(0x8007_0057_u32 as i32, None)
}
