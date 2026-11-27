use xlcalc::{EndDir, FindSpec, LookIn, Rect, SheetId, SpecialKind};
use xlcalc_variant::{HResult, Variant};

use crate::app::{Application, BookId, Mode};
use crate::cmdargs::Reader;
use crate::conv::Args;
use crate::error::OmError;
use crate::typelib::Member;

#[derive(Clone, Debug)]
pub(crate) struct FindState {
    pub what: Option<String>,
    pub look_in: LookIn,
    pub whole: bool,
    pub by_columns: bool,
    pub match_case: bool,
}

impl Default for FindState {
    fn default() -> Self {
        FindState {
            what: Some(String::new()),
            look_in: LookIn::Formulas,
            whole: false,
            by_columns: false,
            match_case: false,
        }
    }
}

impl FindState {
    fn take(
        &mut self,
        a: &Args,
        look_in: usize,
        look_at: usize,
        order: usize,
        case: usize,
    ) -> Result<(), OmError> {
        if let Some(v) = a.i32(look_in)? {
            self.look_in = match v {
                -4123 | -4185 => LookIn::Formulas,
                -4163 => LookIn::Values,
                -4144 | -4184 => LookIn::Comments,
                _ => return Err(OmError::bad_index()),
            };
        }
        if let Some(Variant::Bool(b)) = a.get(look_at) {
            self.whole = *b;
        } else if let Some(v) = a.i32(look_at)? {
            self.whole = match v {
                1 => true,
                2 => false,
                _ => return Err(OmError::bad_index()),
            };
        }
        if let Some(v) = a.i32(order)? {
            self.by_columns = match v {
                1 => false,
                2 => true,
                _ => return Err(OmError::bad_index()),
            };
        }
        if let Some(v) = a.bool(case)? {
            self.match_case = v;
        }
        Ok(())
    }

    pub(crate) fn take_replace(&mut self, a: &Args) -> Result<(), OmError> {
        self.take(a, usize::MAX, 2, 3, usize::MAX)?;
        if a.get(8).is_some_and(|v| !matches!(v, Variant::Null))
            && !matches!(a.i32(8)?, Some(0 | 1))
        {
            return Err(OmError::bad_index());
        }
        self.match_case = a.bool(4)?.unwrap_or(false);
        a.bool(5)?;
        format_arg(a, 6)?;
        format_arg(a, 7)
    }
}

fn format_arg(a: &Args, i: usize) -> Result<(), OmError> {
    match a.get(i) {
        None | Some(Variant::Bool(_)) => Ok(()),
        Some(_) => Err(OmError::type_mismatch()),
    }
}

const SEARCH_TEXT_MAX: usize = 255;

impl Application<'_> {
    pub(crate) fn search_text_arg(
        &self,
        book: BookId,
        a: &Args,
        i: usize,
    ) -> Result<Option<String>, OmError> {
        let text = match a.get(i) {
            None => return Ok(None),
            Some(Variant::Bool(b)) => (if *b { "TRUE" } else { "FALSE" }).to_string(),
            Some(Variant::Dispatch(Some(_))) => {
                let (sheet, rect) = self
                    .range_arg(a.get(i))
                    .map_err(|_| OmError::type_mismatch())?;
                let value = self
                    .book(book)?
                    .wb
                    .value(xlcalc::CellAddr::new(sheet, rect.r0, rect.c0));
                let v = crate::conv::cell_variant(
                    &value,
                    "General",
                    crate::conv::Flavour::Value2,
                    false,
                );
                let one = Args {
                    values: vec![Some(match v {
                        Variant::Bool(b) => {
                            Variant::Bstr((if b { "TRUE" } else { "FALSE" }).into())
                        }
                        v => v,
                    })],
                    put: None,
                    lcid: a.lcid,
                };
                one.string(0)?.unwrap_or_default()
            }
            Some(_) => a.string(i)?.unwrap_or_default(),
        };
        if text.encode_utf16().count() > SEARCH_TEXT_MAX {
            return Err(OmError::type_mismatch());
        }
        Ok(Some(text))
    }

    pub(crate) fn range_search(
        &mut self,
        book: BookId,
        sheet: SheetId,
        rects: &[Rect],
        m: &'static Member,
        a: &Args,
    ) -> Result<Variant, OmError> {
        let first = rects[0];
        match m.name {
            "End" => {
                let dir = match a
                    .i32(0)
                    .map_err(|_| OmError::arg(HResult::DISP_E_TYPEMISMATCH, 0))?
                {
                    Some(-4162) => EndDir::Up,
                    Some(-4121) => EndDir::Down,
                    Some(-4159) => EndDir::Left,
                    Some(-4161) => EndDir::Right,
                    _ => return Err(OmError::app_defined()),
                };
                let (r, c) = self
                    .book(book)?
                    .wb
                    .range_end(sheet, first.r0, first.c0, dir);
                self.new_range(book, sheet, Rect::cell(r, c), Mode::Cells)
            }
            "Find" | "FindNext" | "FindPrevious" => {
                let find = m.name == "Find";
                let mut st = self.find.clone();
                if find {
                    if matches!(a.get(0), Some(Variant::Null)) {
                        st.what = None;
                    } else if a.get(0).is_some() {
                        st.what = self.search_text_arg(book, a, 0)?;
                    }
                }

                let after = match a.get(usize::from(find)) {
                    None => (first.r0, first.c0),
                    Some(v) => {
                        let (_, r) = self.range_arg(Some(v)).map_err(|e| {
                            if find {
                                OmError::type_mismatch()
                            } else {
                                e
                            }
                        })?;
                        let inside = r.is_cell() && rects.iter().any(|x| x.contains(r.r0, r.c0));
                        if !inside {
                            return Err(if find {
                                OmError::type_mismatch()
                            } else {
                                OmError::app_defined_with(&format!(
                                    "Unable to get the {} property of the Range class",
                                    m.name
                                ))
                            });
                        }
                        (r.r0, r.c0)
                    }
                };
                if find {
                    st.take(a, 2, 3, 4, 6)?;
                    if a.get(6).is_none() {
                        st.match_case = false;
                    }
                    a.bool(7)?;
                    format_arg(a, 8)?;
                    self.find = st.clone();
                }
                let Some(what) = st.what.clone() else {
                    return Ok(Variant::Dispatch(None));
                };
                let backward =
                    matches!((m.name, a.i32(5)?), ("FindPrevious", _) | ("Find", Some(2)));
                let spec = FindSpec {
                    what,
                    look_in: st.look_in,
                    whole: st.whole,
                    by_columns: st.by_columns,
                    backward,
                    match_case: st.match_case,
                    in_macro: self.in_vba,
                    cell_only: !find,
                };
                let found = self.book(book)?.wb.find_cell(sheet, rects, after, &spec);
                match found {
                    Some((r, c)) => self.new_range(book, sheet, Rect::cell(r, c), Mode::Cells),
                    None => Ok(Variant::Dispatch(None)),
                }
            }
            _ => Err(crate::app::unsupported("Range", m)),
        }
    }
}

impl Application<'_> {
    pub(crate) fn special_cells_member(
        &mut self,
        book: BookId,
        sheet: SheetId,
        rects: &[Rect],
        m: &'static Member,
        a: &Args,
    ) -> Result<Variant, OmError> {
        let mut operands = self
            .range_operands(m.dispid, a)
            .map_err(|_| OmError::app_defined())?;
        let lcid = a.lcid;
        if operands.given == 1 {
            let wb = &self.book(book)?.wb;
            operands.read_by_executor(Reader { wb, lcid }, 0);
        }
        let filtered = a.get(1).is_some_and(|v| !matches!(v, Variant::Empty));
        let array = operands.has_array(lcid)?;
        let mut found = rects.to_vec();
        self.each_call(lcid, operands, |app, mut operands| {
            let reader = Reader {
                wb: &app.book(book)?.wb,
                lcid,
            };
            operands
                .coerce(reader)
                .map_err(|_| OmError::app_defined())?;
            let int = |i: usize| {
                reader
                    .int(operands.get(i))
                    .map_err(|_| OmError::app_defined())
            };
            let ty = int(0)?.ok_or_else(OmError::app_defined)?;

            if array && ty < 1 {
                return Err(OmError::app_defined());
            }
            let value = if filtered { int(1)? } else { None };

            if (-4175..=-4172).contains(&ty) && filtered {
                return Err(OmError::app_defined());
            }
            found = app.special_found(book, sheet, &found, ty, value)?;
            Ok(Variant::Empty)
        })?;
        self.new_multi(book, sheet, &found, Mode::Cells)
    }

    fn special_found(
        &mut self,
        book: BookId,
        sheet: SheetId,
        rects: &[Rect],
        ty: i32,
        value: Option<i32>,
    ) -> Result<Vec<Rect>, OmError> {
        let p = self.sheet_prot(book, sheet);
        if matches!(ty, -4175 | -4174 | 15) && p.contents && !p.ui_only {
            let mut e = OmError::app_defined_with(crate::sheetprot::PROTECTED_COMMAND);
            e.error_info = true;
            return Err(e);
        }
        let wb = &self.book(book)?.wb;
        let mask = value.map_or(23, |v| v as u32);
        let search = |kind| wb.special_cells(sheet, rects, kind);
        let found = match ty {
            -4175..=-4172 => wb.cfv_special_cells(sheet, rects, ty),
            14 => wb.cfv_special_cells(sheet, rects, -4172),
            15 => wb.cfv_special_cells(sheet, rects, -4174),
            5 => rects
                .first()
                .map(|f| vec![wb.current_region(sheet, Rect::cell(f.r0, f.c0))])
                .unwrap_or_default(),
            9 => wb.trace_precedents(sheet, rects, true),
            10 => wb.trace_dependents(sheet, rects, true),
            13 => {
                let mut e = OmError::app_defined_with("No objects found.");
                e.error_info = true;
                return Err(e);
            }
            2 => search(SpecialKind::Constants(mask)),
            -4123 | 3 => search(SpecialKind::Formulas(mask)),
            4 => search(SpecialKind::Blanks),
            11 => search(SpecialKind::LastCell),
            12 => search(SpecialKind::Visible),
            -4144 | 1 => search(SpecialKind::Comments),
            7 => search(SpecialKind::Differences { by_col: false }),
            8 => search(SpecialKind::Differences { by_col: true }),
            1..=16 | -4175..=-4123 => Vec::new(),
            _ => {
                return Err(OmError::app_defined_with(
                    "Unable to get the SpecialCells property of the Range class",
                ))
            }
        };
        if found.is_empty() {
            return Err(no_cells());
        }
        Ok(found)
    }
}

pub(crate) fn no_cells() -> OmError {
    let mut e = OmError::app_defined_with("No cells were found.");
    e.error_info = true;
    e
}
