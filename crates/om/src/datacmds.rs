use xlcalc::{
    AdvancedFilterSpec, CellAddr, DedupSpec, Rect, SheetId, SubFunc, SubtotalSpec, TtcSpec,
    Workbook,
};
use xlcalc_variant::{change_type, SafeArray, VarType, Variant};

use crate::app::{unsupported, Application, BookId, Object};
use crate::cmdargs::{answers, Oper, Operands, Reader};
use crate::conv::{command_int, is_missing, variant_locale, Args};
use crate::error::OmError;
use crate::objects::edit_error;
use crate::typelib::Member;

const SEEK_FAILED: &str = "GoalSeek method of Range class failed";

fn total_list(v: &Variant, lcid: u32, wb: &Workbook) -> Result<Vec<u32>, OmError> {
    let items = match v {
        Variant::Array(sa) => match sa.data.as_slice() {
            [Variant::Array(row)] => row.data.iter().collect(),
            data if matches!(sa.bounds.as_slice(), [(_, rows), _] if *rows != 1)
                || data.iter().any(|e| matches!(e, Variant::Array(_))) =>
            {
                return Err(OmError::app_defined());
            }
            data => data.iter().collect::<Vec<_>>(),
        },
        other => vec![other],
    };
    items
        .into_iter()
        .map(|e| match command_int(Some(e), lcid, wb) {
            Ok(Some(n)) => Ok(u32::try_from(n).unwrap_or(0)),
            _ => Err(OmError::app_defined()),
        })
        .collect()
}

fn field_info(sa: &SafeArray, lcid: u32, wb: &Workbook) -> Option<Vec<(u32, u32)>> {
    let int = |e: &Variant| command_int(Some(e), lcid, wb).ok().flatten();
    let pair = |a: &Variant, b: &Variant| {
        let at = u32::try_from(int(a)?).ok().filter(|&n| n <= 0x7fff)?;
        let ty = u32::try_from(int(b)?)
            .ok()
            .filter(|n| (1..=9).contains(n))?;
        Some((at, ty))
    };
    if !sa.data.is_empty() && sa.data.iter().all(|e| matches!(e, Variant::Array(_))) {
        return sa
            .data
            .iter()
            .map(|e| match e {
                Variant::Array(row) => match row.data.as_slice() {
                    [a, b] => pair(a, b),
                    _ => None,
                },
                _ => None,
            })
            .collect();
    }
    match sa.bounds.as_slice() {
        [(_, 2)] => Some(vec![pair(&sa.data[0], &sa.data[1])?]),

        [(_, rows), (_, 2)] => {
            let n = *rows as usize;
            (0..n).map(|i| pair(&sa.data[i], &sa.data[n + i])).collect()
        }
        _ => None,
    }
}

fn ttc_failed() -> OmError {
    OmError::app_defined_with("TextToColumns method of Range class failed")
}

fn sub_func(code: i32) -> Result<SubFunc, OmError> {
    Ok(match code {
        1 => SubFunc::Sum,
        2 => SubFunc::Count,
        3 => SubFunc::Average,
        4 => SubFunc::Max,
        5 => SubFunc::Min,
        6 => SubFunc::Product,
        7 => SubFunc::CountNums,
        8 => SubFunc::StDev,
        9 => SubFunc::StDevP,
        10 => SubFunc::Var,
        11 => SubFunc::VarP,
        _ => return Err(OmError::app_defined()),
    })
}

#[derive(Clone, Copy, Debug, Default)]
pub(crate) struct TextDelims {
    consecutive: bool,
    tab: bool,
    semicolon: bool,
    comma: bool,
    space: bool,
    use_other: bool,
    other: Option<char>,

    qualifier: i32,
}

fn ttc_spec(o: &Operands, r: Reader, delims: &mut TextDelims) -> Result<Option<TtcSpec>, ()> {
    let int = |i: usize| r.int(o.get(i)).map_err(|_| ());
    let flag = |i: usize| r.flag(o.get(i)).map_err(|_| ());
    let text = |i: usize| match o.get(i) {
        Oper::Text(s) | Oper::Dated(_, s) => Some(s.clone()),
        _ => None,
    };
    let datatype = int(1)?;
    let qualifier = int(2)?;
    let (consecutive, tab, semicolon) = (flag(3)?, flag(4)?, flag(5)?);
    let (comma, space, use_other) = (flag(6)?, flag(7)?, flag(8)?);
    let other = text(9);
    let decimal = text(11);
    let thousands = text(12);
    let trailing_minus = flag(13)?.unwrap_or(false);

    if datatype.is_some_and(|n| !(1..=2).contains(&n)) {
        return Ok(None);
    }
    let fieldinfo = match o.get(10) {
        Oper::Missing => Vec::new(),
        Oper::Array(sa) => match field_info(sa, r.lcid, r.wb) {
            Some(f) => f,
            None => return Ok(None),
        },
        _ => return Ok(None),
    };
    for (to, v) in [
        (&mut delims.consecutive, consecutive),
        (&mut delims.tab, tab),
        (&mut delims.comma, comma),
        (&mut delims.use_other, use_other),
        (&mut delims.space, space),
        (&mut delims.semicolon, semicolon),
    ] {
        if let Some(v) = v {
            *to = v;
        }
    }
    if let Some(c) = other.and_then(|t| t.chars().next()) {
        delims.other = Some(c);
    }
    if let Some(q) = qualifier {
        if !(1..=3).contains(&q) {
            return Ok(None);
        }
        delims.qualifier = q;
    }

    let separator = |t: Option<String>| match t {
        None => Some(None),
        Some(t) if t.chars().count() == 1 => Some(t.chars().next()),
        Some(_) => None,
    };
    let (Some(decimal), Some(thousands)) = (separator(decimal), separator(thousands)) else {
        return Ok(None);
    };
    Ok(Some(TtcSpec {
        fixed: datatype == Some(2),
        qualifier: match delims.qualifier {
            2 => Some('\''),
            3 => None,
            _ => Some('"'),
        },
        consecutive: delims.consecutive,
        tab: delims.tab,
        semicolon: delims.semicolon,
        comma: delims.comma,
        space: delims.space,
        other: delims.other.filter(|_| delims.use_other),
        fieldinfo,
        decimal,
        thousands,
        trailing_minus,
        dest: None,
        refused: false,
    }))
}

impl Application<'_> {
    fn range_subtotal(
        &mut self,
        book: BookId,
        at: (SheetId, Rect),
        o: &Operands,
        a: &Args,
    ) -> Result<Variant, OmError> {
        let (sheet, rect) = at;
        let wb = &self.book(book)?.wb;
        let r = Reader { wb, lcid: a.lcid };
        let int = |i: usize| r.int(o.get(i)).map_err(|_| OmError::app_defined());
        let flag = |i: usize| r.flag(o.get(i)).map_err(|_| OmError::app_defined());
        let totals = match a.get(2) {
            Some(Variant::Null) => Vec::new(),
            Some(v) => total_list(v, a.lcid, wb)?,
            None => return Err(OmError::app_defined()),
        };
        let spec = SubtotalSpec {
            group: u32::try_from(int(0)?.unwrap_or(1)).unwrap_or(0),
            function: sub_func(int(1)?.ok_or_else(OmError::app_defined)?)?,
            totals,
            replace: flag(3)?.unwrap_or(true),
            page_breaks: flag(4)?.unwrap_or(false),
            below: flag(5)?.unwrap_or(true),
        };
        self.book_mut(book)?
            .wb
            .subtotal(sheet, rect, &spec)
            .map_err(edit_error)?;
        Ok(Variant::Bool(true))
    }

    pub(crate) fn guess_header(&self, book: BookId, sheet: SheetId, rect: Rect) -> bool {
        self.book(book)
            .is_ok_and(|b| b.wb.guess_list_header(sheet, rect, false) == Some(rect.r0))
    }

    fn data_range_arg(&self, a: &Args, i: usize) -> Result<Option<(SheetId, Rect)>, OmError> {
        match a.get(i) {
            None | Some(Variant::Empty) => Ok(None),
            Some(Variant::Dispatch(Some(id))) => match self.arena.get(*id) {
                Some(Object::Range(r)) => Ok(Some(self.resolve(&r.clone())?)),
                _ => Err(OmError::app_defined()),
            },
            Some(_) => Err(OmError::app_defined()),
        }
    }

    fn dedup_columns(&self, rect: Rect, a: &Args) -> Result<Vec<u32>, OmError> {
        let invalid = || OmError::exception(0x8007_0057_u32 as i32, None);
        let width = i64::from(rect.cols());
        let long = |v: &Variant| -> Result<i32, OmError> {
            if is_missing(v) {
                return Ok(0);
            }
            match change_type(v, VarType::I4, variant_locale(a.lcid), 0) {
                Ok(Variant::I4(n)) => Ok(n),
                _ => Err(OmError::type_mismatch()),
            }
        };
        match a.get(0) {
            None => Ok(Vec::new()),
            Some(Variant::Array(sa)) => {
                let [(lower, len)] = sa.bounds[..] else {
                    return Err(invalid());
                };
                let upper = i64::from(lower) + i64::from(len) - 1;
                if lower < 0 || upper >= width {
                    return Err(invalid());
                }

                if lower > 0 {
                    return Err(OmError::bad_index());
                }
                sa.data
                    .iter()
                    .map(|v| {
                        let n = long(v)?;
                        u32::try_from(n)
                            .ok()
                            .filter(|&n| n >= 1 && i64::from(n) <= width)
                            .ok_or_else(invalid)
                    })
                    .collect()
            }
            Some(v) => match long(v)? {
                0 => Ok(Vec::new()),
                n => match u32::try_from(n) {
                    Ok(n) if rect.is_cell() || i64::from(n) <= width => Ok(vec![n]),
                    _ => Err(OmError::app_defined()),
                },
            },
        }
    }

    pub(crate) fn data_member(
        &mut self,
        book: BookId,
        (sheet, rect): (SheetId, Rect),
        m: &'static Member,
        a: &Args,
    ) -> Result<Variant, OmError> {
        match m.name {
            "RemoveDuplicates" => {
                let guessed = a.i32(1)?.unwrap_or(2) == 0;
                let header = match a.i32(1)?.unwrap_or(2) {
                    1 => true,
                    2 => false,
                    0 => self.guess_header(book, sheet, rect),
                    _ => return Err(OmError::app_defined()),
                };
                let columns = self.dedup_columns(rect, a)?;
                let header_above = guessed
                    && !header
                    && self
                        .book(book)
                        .is_ok_and(|b| b.wb.guess_header_above(sheet, rect));
                let spec = DedupSpec {
                    columns,
                    header,
                    header_above,
                };
                self.book_mut(book)?
                    .wb
                    .remove_duplicates(sheet, rect, &spec)
                    .map_err(edit_error)?;
                Ok(Variant::Empty)
            }
            "TextToColumns" => {
                let dest = self
                    .data_range_arg(a, 0)?
                    .map(|(s, r)| CellAddr::new(s, r.r0, r.c0));
                let operands = self.range_operands(m.dispid, a).map_err(|_| ttc_failed())?;

                let (shape, done) = self.each_call(a.lcid, operands, |app, mut o| {
                    let mut delims = app.text_delims;
                    let wb = &app.book(book)?.wb;
                    let r = Reader { wb, lcid: a.lcid };
                    o.coerce(r).map_err(|_| ttc_failed())?;
                    let spec = ttc_spec(&o, r, &mut delims).map_err(|()| ttc_failed())?;
                    app.text_delims = delims;
                    let spec = TtcSpec {
                        dest,
                        ..spec.unwrap_or_else(|| TtcSpec {
                            refused: true,
                            ..TtcSpec::default()
                        })
                    };
                    app.book_mut(book)?
                        .wb
                        .text_to_columns(sheet, rect, &spec)
                        .map_err(edit_error)?;
                    Ok(Variant::Bool(true))
                })?;
                if let Some((rows, cols)) = shape {
                    return Ok(answers(rows, cols, done));
                }

                Ok(match a.get(10) {
                    Some(Variant::Array(sa)) if sa.bounds.len() == 2 => {
                        Variant::Array(Box::new(SafeArray {
                            elem: VarType::Variant,
                            bounds: sa.bounds.clone(),
                            data: vec![Variant::Bool(true); sa.data.len()],
                        }))
                    }
                    _ => Variant::Bool(true),
                })
            }
            "Subtotal" => {
                let operands = self
                    .range_operands(m.dispid, a)
                    .map_err(|_| OmError::app_defined())?;
                let refused = OmError::app_defined();
                self.run_command(book, a.lcid, operands, &refused, |app, o| {
                    app.range_subtotal(book, (sheet, rect), o, a)
                })
            }
            "AdvancedFilter" => {
                let copy = match a.i32(0)? {
                    Some(1) => false,
                    Some(2) => true,
                    _ => return Err(OmError::app_defined()),
                };
                let criteria = self.data_range_arg(a, 1)?;
                let copy_to = self.data_range_arg(a, 2)?;

                if copy && copy_to.is_none() {
                    return Err(OmError::app_defined());
                }
                let spec = AdvancedFilterSpec {
                    list: (sheet, rect),
                    criteria,
                    copy_to: copy_to.filter(|_| copy),
                    unique: a.bool(3)?.unwrap_or(false),
                };
                let (_, copied) = self
                    .book_mut(book)?
                    .wb
                    .advanced_filter(&spec)
                    .map_err(edit_error)?;
                self.carry_extract(book, &copied)?;
                Ok(Variant::Bool(true))
            }
            "Consolidate" => self.consolidate(book, sheet, rect, a),
            "RemoveSubtotal" => {
                self.book_mut(book)?
                    .wb
                    .remove_subtotal(sheet, rect)
                    .map_err(edit_error)?;
                Ok(Variant::Bool(true))
            }
            "Table" => {
                let mut cells = [None, None];
                for (i, slot) in cells.iter_mut().enumerate() {
                    if let Some((s, r)) = self.data_range_arg(a, i)? {
                        if !r.is_cell() {
                            return Err(OmError::app_defined()
                                .with_detail("Input cell reference is not valid."));
                        }
                        *slot = Some(CellAddr::new(s, r.r0, r.c0));
                    }
                }
                self.book_mut(book)?
                    .wb
                    .add_data_table(sheet, rect, cells[0], cells[1])
                    .map_err(edit_error)?;

                Ok(Variant::Bool(true))
            }
            "GoalSeek" => {
                let goal = match a.get(0) {
                    Some(v) => match change_type(v, VarType::R8, variant_locale(a.lcid), 0) {
                        Ok(Variant::R8(n)) => n,

                        _ => return Err(OmError::app_defined_with(SEEK_FAILED)),
                    },
                    None => return Err(OmError::app_defined()),
                };
                let (cs, cr) = self
                    .data_range_arg(a, 1)?
                    .ok_or_else(OmError::app_defined)?;

                if !cr.is_cell() {
                    return Err(OmError::app_defined_with("Reference isn't valid."));
                }
                let target = CellAddr::new(sheet, rect.r0, rect.c0);
                let changing = CellAddr::new(cs, cr.r0, cr.c0);
                let got = self
                    .book_mut(book)?
                    .wb
                    .goal_seek(target, goal, changing)
                    .map_err(edit_error)?;
                Ok(Variant::Bool(got))
            }
            _ => Err(unsupported("Range", m)),
        }
    }
}
