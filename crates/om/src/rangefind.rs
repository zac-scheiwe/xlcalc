use xlcalc::{EndDir, FindSpec, LookIn, Rect, SheetId, SpecialKind};
use xlcalc_variant::{HResult, Variant};

use crate::app::{Application, BookId, Mode};
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
                -4123 => LookIn::Formulas,
                -4163 => LookIn::Values,
                -4144 | -4184 => LookIn::Comments,
                _ => return Err(OmError::bad_index()),
            };
        }
        if let Some(v) = a.i32(look_at)? {
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
        self.take(a, usize::MAX, 2, 3, 4)
    }
}

impl Application<'_> {
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
                    st.what = Some(a.string(0)?.ok_or_else(OmError::type_mismatch)?);
                    st.take(a, 2, 3, 4, 6)?;
                }
                let after = match a.get(usize::from(find)) {
                    None => (first.r0, first.c0),
                    Some(v) => {
                        let (s, r) = self.range_arg(Some(v))?;
                        let inside = s == sheet
                            && r.is_cell()
                            && (rects.iter().any(|x| x.contains(r.r0, r.c0))
                                || (rects.len() == 1 && first.is_cell()));
                        if !inside {
                            return Err(if find {
                                OmError::type_mismatch()
                            } else {
                                OmError::app_defined()
                            });
                        }
                        (r.r0, r.c0)
                    }
                };
                if find {
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
                };
                let found = self.book(book)?.wb.find_cell(sheet, rects, after, &spec);
                match found {
                    Some((r, c)) => self.new_range(book, sheet, Rect::cell(r, c), Mode::Cells),
                    None => Ok(Variant::Dispatch(None)),
                }
            }
            _ => {
                let mask = a.i32(1)?.map_or(23, |v| v as u32);
                let kind = match a.i32(0)?.ok_or_else(OmError::app_defined)? {
                    2 => SpecialKind::Constants(mask),
                    -4123 => SpecialKind::Formulas(mask),
                    4 => SpecialKind::Blanks,
                    11 => SpecialKind::LastCell,
                    12 => SpecialKind::Visible,
                    -4144 | 1 => SpecialKind::Comments,
                    3 => SpecialKind::Formulas(mask),
                    7 => SpecialKind::Differences { by_col: false },
                    8 => SpecialKind::Differences { by_col: true },
                    _ => return Err(no_cells()),
                };
                let found = self.book(book)?.wb.special_cells(sheet, rects, kind);
                if found.is_empty() {
                    return Err(no_cells());
                }
                self.new_multi(book, sheet, &found, Mode::Cells)
            }
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
        let ty = a.i32(0)?.ok_or_else(OmError::app_defined)?;
        let wb = &self.book(book)?.wb;
        let found = match ty {
            -4175..=-4172 => wb.cfv_special_cells(sheet, rects, ty),
            14 => wb.cfv_special_cells(sheet, rects, -4172),
            15 => wb.cfv_special_cells(sheet, rects, -4174),
            16 => Vec::new(),
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
            6 => Vec::new(),
            _ => return self.range_search(book, sheet, rects, m, a),
        };
        if found.is_empty() {
            return Err(no_cells());
        }
        self.new_multi(book, sheet, &found, Mode::Cells)
    }
}

fn no_cells() -> OmError {
    let mut e = OmError::app_defined_with("No cells were found.");
    e.error_info = true;
    e
}
