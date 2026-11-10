use xlcalc::{CellAddr, Rect, SheetId};
use xlcalc_variant::{HResult, Variant};

use crate::app::{unsupported, Application, BookId, Mode, Object};
use crate::conv::Args;
use crate::error::OmError;
use crate::typelib::{InvokeKind, Member};

const CREATOR: i32 = 0x5843_454C;

pub(crate) fn user_name() -> String {
    std::env::var("USERNAME")
        .or_else(|_| std::env::var("USER"))
        .unwrap_or_default()
}

fn invalid_arg() -> OmError {
    OmError::exception(HResult::E_INVALIDARG.0, None)
}

fn splice(text: &str, new: &str, start: usize, len: Option<usize>) -> String {
    let chars: Vec<char> = text.chars().collect();
    let from = (start - 1).min(chars.len());
    let to = len.map_or(chars.len(), |l| (from + l).min(chars.len()));
    let mut out: String = chars[..from].iter().collect();
    out.push_str(new);
    out.extend(&chars[to..]);
    out
}

impl Application<'_> {
    pub(crate) fn range_comment(
        &mut self,
        book: BookId,
        sheet: SheetId,
        rect: Rect,
    ) -> Result<Variant, OmError> {
        if !rect.is_cell() {
            return Ok(Variant::Dispatch(None));
        }
        let id = self
            .book(book)?
            .wb
            .note_at(sheet, rect.r0, rect.c0)
            .map(|n| n.id);
        Ok(match id {
            Some(id) => self.alloc(Object::Comment(book, sheet, id)),
            None => Variant::Dispatch(None),
        })
    }

    pub(crate) fn range_add_comment(
        &mut self,
        book: BookId,
        sheet: SheetId,
        rect: Rect,
        a: &Args,
    ) -> Result<Variant, OmError> {
        if !rect.is_cell() {
            return Err(invalid_arg());
        }
        let author = user_name();
        let text = match a.get(0) {
            None | Some(Variant::Empty) => String::new(),
            Some(_) => a.string(0)?.unwrap_or_default(),
        };

        let text = if text.is_empty() {
            format!("{author}:\n")
        } else {
            text
        };
        let at = CellAddr::new(sheet, rect.r0, rect.c0);
        let id = self
            .book_mut(book)?
            .wb
            .note_add(at, &text, &author, false)
            .ok_or_else(OmError::app_defined)?;
        Ok(self.alloc(Object::Comment(book, sheet, id)))
    }

    pub(crate) fn range_note_text(
        &mut self,
        book: BookId,
        sheet: SheetId,
        rect: Rect,
        a: &Args,
    ) -> Result<Variant, OmError> {
        let at = CellAddr::new(sheet, rect.r0, rect.c0);
        let start = a.i32(1)?;
        let len = a.i32(2)?;
        if start.is_some_and(|s| s < 1) || len.is_some_and(|l| l < 0) {
            return Err(OmError::app_defined());
        }
        let old = self
            .book(book)?
            .wb
            .note_at(sheet, at.row, at.col)
            .map(|n| n.text.clone());
        if a.get(0).is_none_or(|v| matches!(v, Variant::Empty)) {
            let chars: Vec<char> = old.unwrap_or_default().chars().collect();
            let from = (start.unwrap_or(1) as usize - 1).min(chars.len());
            let to = len.map_or(chars.len(), |l| (from + l as usize).min(chars.len()));
            return Ok(Variant::Bstr(chars[from..to].iter().collect()));
        }
        let new = a.string(0)?.unwrap_or_default();
        let text = match start {
            None => new,
            Some(s) => splice(
                &old.clone().unwrap_or_default(),
                &new,
                s as usize,
                len.map(|l| l as usize),
            ),
        };
        let n = text.chars().count();
        let wb = &mut self.book_mut(book)?.wb;
        match old {
            Some(_) => {
                if let Some(id) = wb.note_at(sheet, at.row, at.col).map(|n| n.id) {
                    wb.note_edit(sheet, id, |note| note.text = text);
                }
            }
            None => {
                wb.note_add(at, &text, &user_name(), false);
            }
        }
        Ok(Variant::R8(n as f64))
    }

    pub(crate) fn range_clear_notes(
        &mut self,
        book: BookId,
        sheet: SheetId,
        rects: &[Rect],
        m: &'static Member,
    ) -> Result<Variant, OmError> {
        let wb = &mut self.book_mut(book)?.wb;
        for r in rects {
            wb.notes_clear(sheet, r);
        }

        Ok(if m.name == "ClearNotes" {
            Variant::Bool(true)
        } else {
            Variant::Empty
        })
    }

    pub(crate) fn comments_member(
        &mut self,
        book: BookId,
        sheet: SheetId,
        m: &'static Member,
        a: &Args,
    ) -> Result<Variant, OmError> {
        use InvokeKind::*;
        let ids: Vec<u32> = self
            .book(book)?
            .wb
            .notes(sheet)
            .map(|(_, n)| n.id)
            .collect();
        match (m.name, m.kind) {
            ("Count", Get) => Ok(Variant::I4(ids.len() as i32)),
            ("Application", Get) => Ok(Variant::Dispatch(Some(self.handle()))),
            ("Creator", Get) => Ok(Variant::I4(CREATOR)),
            ("Parent", Get) => Ok(self.alloc(Object::Worksheet(book, sheet))),
            ("Item" | "_Default", _) => {
                let i = a.i32(0)?.unwrap_or(0);
                match usize::try_from(i - 1).ok().and_then(|i| ids.get(i)) {
                    Some(id) => Ok(self.alloc(Object::Comment(book, sheet, *id))),
                    None => Err(OmError::app_defined()),
                }
            }
            ("_NewEnum", Get) => {
                let items = ids
                    .into_iter()
                    .map(|id| self.alloc(Object::Comment(book, sheet, id)))
                    .collect();
                Ok(self.alloc_enumerator(items))
            }
            _ => Err(unsupported("Comments", m)),
        }
    }

    pub(crate) fn comment_member(
        &mut self,
        book: BookId,
        sheet: SheetId,
        id: u32,
        m: &'static Member,
        a: &Args,
    ) -> Result<Variant, OmError> {
        use InvokeKind::*;
        let ((row, col), note) = self
            .book(book)?
            .wb
            .note_by_id(sheet, id)
            .map(|(k, n)| (k, n.clone()))
            .ok_or_else(OmError::object_required)?;
        match (m.name, m.kind) {
            ("Application", Get) => Ok(Variant::Dispatch(Some(self.handle()))),
            ("Creator", Get) => Ok(Variant::I4(CREATOR)),
            ("Author", Get) => Ok(Variant::Bstr(note.author)),
            ("Visible", Get) => Ok(Variant::Bool(note.visible)),
            ("Visible", Put) => {
                let v = crate::chartparts::put_bool(a)?;
                self.book_mut(book)?
                    .wb
                    .note_edit(sheet, id, |n| n.visible = v);
                Ok(Variant::Empty)
            }
            ("Parent", Get) => self.new_range(book, sheet, Rect::cell(row, col), Mode::Cells),
            ("Delete", Func) => {
                self.book_mut(book)?.wb.note_remove(sheet, id);
                Ok(Variant::Empty)
            }
            ("Next" | "Previous", Func) => {
                let ids: Vec<u32> = self
                    .book(book)?
                    .wb
                    .notes(sheet)
                    .map(|(_, n)| n.id)
                    .collect();
                let at = ids.iter().position(|&i| i == id).unwrap_or(0);
                let other = if m.name == "Next" {
                    ids.get(at + 1)
                } else {
                    at.checked_sub(1).and_then(|p| ids.get(p))
                };
                Ok(match other {
                    Some(o) => self.alloc(Object::Comment(book, sheet, *o)),
                    None => Variant::Dispatch(None),
                })
            }
            ("Text", Func) => {
                let overwrite = a.get(2).filter(|v| !matches!(v, Variant::Empty));
                if overwrite.is_some() && a.bool(2)? == Some(true) {
                    return Err(invalid_arg());
                }
                let new = match a.get(0).filter(|v| !matches!(v, Variant::Empty)) {
                    None => return Ok(Variant::Bstr(note.text)),
                    Some(Variant::Bstr(s)) => s.clone(),
                    Some(_) => return Err(OmError::app_defined()),
                };
                let start = a.i32(1)?;
                if start.is_some_and(|s| s < 1) {
                    return Err(invalid_arg());
                }
                let text = match (start, overwrite.is_some()) {
                    (None, false) => new,
                    (s, true) => splice(&note.text, &new, s.unwrap_or(1) as usize, Some(0)),
                    (Some(s), false) => splice(&note.text, &new, s as usize, None),
                };
                self.book_mut(book)?
                    .wb
                    .note_edit(sheet, id, |n| n.text = text.clone());
                Ok(Variant::Bstr(text))
            }
            ("Shape", Get) => Ok(self.alloc(Object::CommentShape(book, sheet, id))),
            _ => Err(unsupported("Comment", m)),
        }
    }
}
