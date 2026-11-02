use xlcalc::{
    paste_area, DateUnit, FillType, PasteOp, PasteSpecial, PasteWhat, Rect, ReplaceSpec, Selection,
    SeriesSpec, SeriesType, SheetId, SortKey, SortSpec,
};
use xlcalc_variant::Variant;

use crate::app::{unsupported, Application, BookId, Object, RangeRef, SortOwner, SortState};
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
        for &(ks, kr, order) in &st.fields {
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
            });
        }
        let spec = SortSpec {
            keys,
            header: header == 1,
            by_columns,
            match_case,
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

        let orientation = if a.i32(10)?.unwrap_or(2) == 1 { 2 } else { 1 };
        let header = a.i32(7)?.unwrap_or(2);
        let match_case = a.bool(9)?.unwrap_or(false);

        let rect = if rect.is_cell() {
            self.book(r.book)?.wb.current_region(sheet, rect)
        } else {
            rect
        };
        let st = SortState {
            fields,
            range: Some((sheet, rect)),
            orientation,
            header,
            match_case,
        };
        self.apply_sort(r.book, &st)
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
            ("Header", Get) => Ok(Variant::I4(self.sort_state(o).header)),
            ("Header", Put) => {
                let v = self.put_i32(a)?;
                self.sort_state(o).header = v;
                Ok(Variant::Empty)
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
                let v = self.put_i32(a)?;
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

                        if st.header != 1 {
                            return Err(OmError::unsupported(
                                "ListObject.Sort.Apply without Header:=xlYes",
                            ));
                        }
                        let spec = SortSpec {
                            keys: st
                                .fields
                                .iter()
                                .map(|&(_, kr, order)| SortKey {
                                    index: kr.c0 - t.rect.c0,
                                    descending: order == 2,
                                })
                                .collect(),
                            header: false,
                            by_columns: false,
                            match_case: st.match_case,
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
                self.sort_state(o).fields.clear();
                Ok(Variant::Empty)
            }
            "Item" | "_Default" => {
                let items: Vec<usize> = (0..self.sort_state(o).fields.len()).collect();
                let i = self.item_index(a.get(0), a.lcid, &items, |_| "")?;
                Ok(self.alloc(Object::SortField(o.clone(), i)))
            }
            "Add" => {
                let (ks, kr) = self.range_arg(a.get(0))?;
                if a.i32(1)?.unwrap_or(0) != 0 {
                    return Err(OmError::unsupported(
                        "SortFields.Add with SortOn other than values",
                    ));
                }
                if a.get(3).is_some() || a.i32(4)?.unwrap_or(0) != 0 {
                    return Err(OmError::unsupported(
                        "SortFields.Add with CustomOrder or DataOption",
                    ));
                }
                if let SortOwner::Table(b, name) = o {
                    let t = self.table_of(*b, name)?;
                    if ks != t.sheet || kr.c0 < t.rect.c0 || kr.c1 > t.rect.c1 || kr.c0 != kr.c1 {
                        return Err(OmError::app_defined());
                    }
                }
                let order = a.i32(2)?.unwrap_or(1);
                let st = self.sort_state(o);
                st.fields.push((ks, kr, order));
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
            ("Order", Put) => {
                let v = self.put_i32(a)?;
                if let Some(f) = self.sort_state(o).fields.get_mut(i) {
                    f.2 = v;
                }
                Ok(Variant::Empty)
            }
            _ => Err(unsupported("SortField", m)),
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
                        Some(_) => Err(OmError::type_mismatch()),
                    }
                };
                let spec = SeriesSpec {
                    rows: a.i32(0)?.map(|v| v == 1),
                    ty: match a.i32(1)? {
                        None => None,
                        Some(-4132) => Some(SeriesType::Linear),
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
                let what = match a.i32(0)?.unwrap_or(-4104) {
                    -4104 => PasteWhat::All,
                    -4163 => PasteWhat::Values,
                    -4123 => PasteWhat::Formulas,
                    -4122 => PasteWhat::Formats,
                    _ => return Err(unsupported("Range", m)),
                };
                let op = match a.i32(1)?.unwrap_or(-4142) {
                    -4142 => PasteOp::None,
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

                self.with_sheet_shown(book, sheet, |app| {
                    let mark = app.notice_mark(book);
                    let wb = &mut app.book_mut(book)?.wb;
                    let old = wb.selection_of(sheet).areas;
                    wb.set_selection(sheet, Selection::of(&[rect]))
                        .map_err(edit_error)?;
                    wb.paste_special(ss, src, sheet, rect, &spec)
                        .map_err(edit_error)?;
                    let shape = if spec.transpose {
                        Rect::new(0, 0, src.cols() - 1, src.rows() - 1)
                    } else {
                        src
                    };
                    let pasted = paste_area(shape, rect);
                    wb.set_selection(sheet, Selection::of(&[pasted]))
                        .map_err(edit_error)?;
                    if old != [rect] {
                        app.note_selection(book, mark, At::First, sheet, &[rect]);
                    }
                    Ok(Variant::Bool(true))
                })
            }
            _ => {
                let spec = ReplaceSpec {
                    what: a.string(0)?.unwrap_or_default(),
                    with: a.string(1)?.unwrap_or_default(),
                    whole: a.i32(2)?.unwrap_or(2) == 1,
                    by_columns: a.i32(3)?.unwrap_or(1) == 2,
                    match_case: a.bool(4)?.unwrap_or(false),
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
