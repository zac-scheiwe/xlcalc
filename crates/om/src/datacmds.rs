use xlcalc::{CellAddr, DedupSpec, Rect, SheetId, SubFunc, SubtotalSpec, TtcSpec};
use xlcalc_variant::{change_type, VarType, Variant};

use crate::app::{unsupported, Application, BookId, Object};
use crate::conv::{variant_locale, Args};
use crate::error::OmError;
use crate::objects::edit_error;
use crate::typelib::Member;

fn int_list(v: &Variant) -> Result<Vec<i32>, OmError> {
    match v {
        Variant::Array(sa) => sa.data.iter().map(int_of).collect(),
        other => Ok(vec![int_of(other)?]),
    }
}

fn int_of(v: &Variant) -> Result<i32, OmError> {
    match v {
        Variant::I2(n) => Ok(i32::from(*n)),
        Variant::I4(n) => Ok(*n),
        Variant::Ui1(n) => Ok(i32::from(*n)),
        Variant::R8(n) if n.fract() == 0.0 => Ok(*n as i32),
        _ => Err(OmError::type_mismatch()),
    }
}

fn field_info(v: &Variant) -> Result<Vec<(u32, u32)>, OmError> {
    let Variant::Array(sa) = v else {
        return Err(OmError::type_mismatch());
    };
    let pair = |a: i32, b: i32| (a.max(0) as u32, b.max(0) as u32);
    if !sa.data.is_empty() && sa.data.iter().all(|e| matches!(e, Variant::Array(_))) {
        return sa
            .data
            .iter()
            .map(|e| match int_list(e)?.as_slice() {
                [a, b] => Ok(pair(*a, *b)),
                _ => Err(OmError::type_mismatch()),
            })
            .collect();
    }
    let nums = int_list(v)?;
    match sa.bounds.as_slice() {
        [(_, rows), (_, 2)] => {
            let n = *rows as usize;
            Ok((0..n).map(|i| pair(nums[i], nums[n + i])).collect())
        }
        _ => Err(OmError::type_mismatch()),
    }
}

fn sub_func(code: i32) -> Result<SubFunc, OmError> {
    Ok(match code {
        -4157 => SubFunc::Sum,
        -4112 => SubFunc::Count,
        -4106 => SubFunc::Average,
        -4136 => SubFunc::Max,
        -4139 => SubFunc::Min,
        -4149 => SubFunc::Product,
        -4113 => SubFunc::CountNums,
        -4155 => SubFunc::StDev,
        -4156 => SubFunc::StDevP,
        -4164 => SubFunc::Var,
        -4165 => SubFunc::VarP,
        _ => return Err(OmError::app_defined()),
    })
}

fn first_char(s: Option<String>) -> Option<char> {
    s.and_then(|t| t.chars().next())
}

impl Application<'_> {
    fn data_range_arg(&self, a: &Args, i: usize) -> Result<Option<(SheetId, Rect)>, OmError> {
        match a.get(i) {
            None => Ok(None),
            Some(Variant::Dispatch(Some(id))) => match self.arena.get(*id) {
                Some(Object::Range(r)) => Ok(Some(self.resolve(&r.clone())?)),
                _ => Err(OmError::app_defined()),
            },
            Some(_) => Err(OmError::app_defined()),
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
                let header = match a.i32(1)? {
                    Some(1) => true,
                    Some(2) => false,
                    _ => return Err(unsupported("Range", m)),
                };
                let columns = match a.get(0) {
                    Some(v) => int_list(v)?.into_iter().map(|c| c.max(0) as u32).collect(),
                    None => return Err(unsupported("Range", m)),
                };
                let spec = DedupSpec { columns, header };
                self.book_mut(book)?
                    .wb
                    .remove_duplicates(sheet, rect, &spec)
                    .map_err(edit_error)?;
                Ok(Variant::Empty)
            }
            "TextToColumns" => {
                let other = if a.bool(8)?.unwrap_or(false) {
                    first_char(a.string(9)?)
                } else {
                    None
                };
                let dest = self
                    .data_range_arg(a, 0)?
                    .map(|(s, r)| CellAddr::new(s, r.r0, r.c0));
                let spec = TtcSpec {
                    fixed: a.i32(1)? == Some(2),
                    qualifier: match a.i32(2)? {
                        Some(2) => Some('\''),
                        Some(-4142) => None,
                        _ => Some('"'),
                    },
                    consecutive: a.bool(3)?.unwrap_or(false),
                    tab: a.bool(4)?.unwrap_or(false),
                    semicolon: a.bool(5)?.unwrap_or(false),
                    comma: a.bool(6)?.unwrap_or(false),
                    space: a.bool(7)?.unwrap_or(false),
                    other,
                    fieldinfo: match a.get(10) {
                        Some(v) => field_info(v)?,
                        None => Vec::new(),
                    },
                    decimal: first_char(a.string(11)?),
                    thousands: first_char(a.string(12)?),
                    trailing_minus: a.bool(13)?.unwrap_or(false),
                    dest,
                };
                self.book_mut(book)?
                    .wb
                    .text_to_columns(sheet, rect, &spec)
                    .map_err(edit_error)?;
                Ok(Variant::Empty)
            }
            "Subtotal" => {
                let totals = match a.get(2) {
                    Some(v) => int_list(v)?.into_iter().map(|c| c.max(0) as u32).collect(),
                    None => return Err(OmError::app_defined()),
                };

                let below = match a.i32(5)? {
                    None | Some(1) => true,
                    Some(0) => false,
                    Some(_) => return Err(OmError::app_defined()),
                };
                let spec = SubtotalSpec {
                    group: a.i32(0)?.ok_or_else(OmError::app_defined)?.max(0) as u32,
                    function: sub_func(a.i32(1)?.ok_or_else(OmError::app_defined)?)?,
                    totals,
                    replace: a.bool(3)?.unwrap_or(true),
                    below,
                };
                self.book_mut(book)?
                    .wb
                    .subtotal(sheet, rect, &spec)
                    .map_err(edit_error)?;
                Ok(Variant::Empty)
            }
            "RemoveSubtotal" => {
                self.book_mut(book)?
                    .wb
                    .remove_subtotal(sheet, rect)
                    .map_err(edit_error)?;
                Ok(Variant::Empty)
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
                Ok(Variant::Empty)
            }
            "GoalSeek" => {
                let goal = match a.get(0) {
                    Some(v) => match change_type(v, VarType::R8, variant_locale(a.lcid), 0) {
                        Ok(Variant::R8(n)) => n,
                        _ => return Err(OmError::type_mismatch()),
                    },
                    None => return Err(OmError::app_defined()),
                };
                let (cs, cr) = self
                    .data_range_arg(a, 1)?
                    .ok_or_else(OmError::app_defined)?;
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
