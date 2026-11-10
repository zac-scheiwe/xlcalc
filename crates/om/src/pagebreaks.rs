use xlcalc::Rect;
use xlcalc_variant::Variant;

use crate::app::{unsupported, Application, BookId, Object};
use crate::conv::Args;
use crate::error::OmError;
use crate::typelib::{InvokeKind, Member};
use xlcalc::SheetId;

const MANUAL: i32 = -4135;
const FULL: i32 = 1;

impl Application<'_> {
    fn breaks_of(&self, b: BookId, s: SheetId, rows: bool) -> Result<Vec<u32>, OmError> {
        Ok(self.book(b)?.wb.page_breaks(s, rows))
    }

    pub(crate) fn page_breaks_member(
        &mut self,
        rows: bool,
        owner: Option<(BookId, SheetId)>,
        m: &'static Member,
        a: &Args,
    ) -> Result<Variant, OmError> {
        let Some((b, s)) = owner else {
            return match m.name {
                "Count" => Ok(Variant::I4(0)),
                "Item" | "_Default" => Err(OmError::bad_index()),
                _ => Err(unsupported("PageBreaks", m)),
            };
        };
        let list = self.breaks_of(b, s, rows)?;
        match (m.name, m.kind) {
            ("Count", InvokeKind::Get) => Ok(Variant::I4(list.len() as i32)),
            ("Item" | "_Default", InvokeKind::Get) => {
                let i = a.i32(0)?.ok_or_else(OmError::bad_index)?;
                let at = usize::try_from(i - 1)
                    .ok()
                    .and_then(|i| list.get(i))
                    .ok_or_else(OmError::bad_index)?;
                Ok(self.alloc(Object::PageBreak(rows, b, s, *at)))
            }
            ("_NewEnum", InvokeKind::Get) => {
                let items = list
                    .into_iter()
                    .map(|at| self.alloc(Object::PageBreak(rows, b, s, at)))
                    .collect();
                Ok(self.alloc_enumerator(items))
            }
            ("Add", InvokeKind::Func) => {
                let before = match a.get(0) {
                    Some(Variant::Dispatch(Some(id))) => match self.arena.get(*id) {
                        Some(Object::Range(r)) => self.resolve(&r.clone())?,
                        _ => return Err(OmError::app_defined()),
                    },
                    _ => return Err(OmError::app_defined()),
                };
                if before.0 != s {
                    return Err(OmError::app_defined());
                }
                let at = if rows { before.1.r0 } else { before.1.c0 };

                if at == 0 {
                    return Err(OmError::app_defined());
                }
                self.book_mut(b)?.wb.add_page_break(s, rows, at);
                Ok(self.alloc(Object::PageBreak(rows, b, s, at)))
            }
            ("Application", InvokeKind::Get) => Ok(Variant::Dispatch(Some(self.handle()))),
            ("Parent", InvokeKind::Get) => Ok(self.sheet_object(b, s)),
            ("Creator", InvokeKind::Get) => Ok(Variant::I4(0x5843_454C_u32 as i32)),
            _ => Err(unsupported(
                if rows { "HPageBreaks" } else { "VPageBreaks" },
                m,
            )),
        }
    }

    pub(crate) fn page_break_member(
        &mut self,
        (rows, b, s, at): (bool, BookId, SheetId, u32),
        m: &'static Member,
        _a: &Args,
    ) -> Result<Variant, OmError> {
        match (m.name, m.kind) {
            ("Type", InvokeKind::Get) => Ok(Variant::I4(MANUAL)),
            ("Extent", InvokeKind::Get) => Ok(Variant::I4(FULL)),
            ("Location", InvokeKind::Get) => {
                let rect = if rows {
                    Rect::new(at, 0, at, 0)
                } else {
                    Rect::new(0, at, 0, at)
                };
                self.new_range(b, s, rect, crate::app::Mode::Cells)
            }
            ("Delete", InvokeKind::Func) => {
                self.book_mut(b)?.wb.delete_page_break(s, rows, at);
                Ok(Variant::Empty)
            }
            ("Application", InvokeKind::Get) => Ok(Variant::Dispatch(Some(self.handle()))),
            ("Parent", InvokeKind::Get) => Ok(self.sheet_object(b, s)),
            ("Creator", InvokeKind::Get) => Ok(Variant::I4(0x5843_454C_u32 as i32)),
            _ => Err(unsupported(
                if rows { "HPageBreak" } else { "VPageBreak" },
                m,
            )),
        }
    }
}
