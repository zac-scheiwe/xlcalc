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

const NOTE_TEXT_MAX: usize = 0x7fff;

fn set_note_text(old: &str, new: &str, start: usize, overwrite: Option<usize>) -> String {
    let new: Vec<char> = new.chars().take(NOTE_TEXT_MAX).collect();
    let mut cur = old.to_string();
    let mut at = start;
    let mut count = overwrite;
    for piece in new.chunks(255) {
        let piece_text: String = piece.iter().collect();
        let cand = splice(&cur, &piece_text, at, count);
        if cand.chars().count() <= NOTE_TEXT_MAX {
            cur = cand;
        }
        at += piece.len();
        count = Some(0);
    }
    cur
}

pub(crate) struct Thread {
    resolved: bool,
    date: f64,
    replies: u32,
    text: String,
}

impl Application<'_> {
    pub(crate) fn range_thread(
        &mut self,
        book: BookId,
        sheet: SheetId,
        rect: Rect,
    ) -> Result<Variant, OmError> {
        if !rect.is_cell() {
            return Err(invalid_arg());
        }
        let key = (sheet, rect.r0, rect.c0);
        let has = self.book(book)?.threads.iter().any(|(k, _)| *k == key);
        Ok(if has {
            self.alloc(Object::CommentThreaded(book, sheet, rect.r0, rect.c0))
        } else {
            Variant::Dispatch(None)
        })
    }

    pub(crate) fn range_add_thread(
        &mut self,
        book: BookId,
        sheet: SheetId,
        rect: Rect,
        a: &Args,
    ) -> Result<Variant, OmError> {
        if !rect.is_cell() {
            return Err(invalid_arg());
        }
        let text = a.string(0)?.unwrap_or_default();
        let key = (sheet, rect.r0, rect.c0);
        let bk = self.book(book)?;
        if bk.threads.iter().any(|(k, _)| *k == key)
            || bk.wb.note_at(sheet, rect.r0, rect.c0).is_some()
        {
            return Err(OmError::app_defined());
        }
        let date = match crate::fsobj::ole_date(Ok(std::time::SystemTime::now())) {
            Variant::Date(d) => d,
            _ => 0.0,
        };
        self.book_mut(book)?.threads.push((
            key,
            Thread {
                resolved: false,
                date,
                replies: 0,
                text,
            },
        ));
        Ok(self.alloc(Object::CommentThreaded(book, sheet, rect.r0, rect.c0)))
    }

    pub(crate) fn thread_member(
        &mut self,
        book: BookId,
        sheet: SheetId,
        row: u32,
        col: u32,
        m: &'static Member,
        a: &Args,
    ) -> Result<Variant, OmError> {
        use InvokeKind::*;
        let key = (sheet, row, col);
        let at = self
            .book(book)?
            .threads
            .iter()
            .position(|(k, _)| *k == key)
            .ok_or_else(OmError::object_required)?;
        match (m.name, m.kind) {
            ("Application", Get) => Ok(Variant::Dispatch(Some(self.handle()))),
            ("Creator", Get) => Ok(Variant::I4(CREATOR)),
            ("Parent", Get) => self.new_range(book, sheet, Rect::cell(row, col), Mode::Cells),
            ("Text", Func | Get) => Ok(Variant::Bstr(self.book(book)?.threads[at].1.text.clone())),
            ("Date", Get) => Ok(Variant::Date(self.book(book)?.threads[at].1.date)),
            ("Resolved", Get) => Ok(Variant::Bool(self.book(book)?.threads[at].1.resolved)),
            ("Resolved", Put) => {
                let v = match a.put.as_ref() {
                    Some(Variant::Bool(x)) => *x,
                    Some(Variant::I4(x)) => *x != 0,
                    Some(Variant::R8(x)) => *x != 0.0,
                    _ => return Err(OmError::type_mismatch()),
                };
                self.book_mut(book)?.threads[at].1.resolved = v;
                Ok(Variant::Empty)
            }
            ("Delete", Func) => {
                self.book_mut(book)?.threads.remove(at);
                Ok(Variant::Empty)
            }
            ("Next" | "Previous", Func) => Ok(Variant::Dispatch(None)),
            ("Replies", Get) => Ok(self.alloc(Object::ThreadList(book, sheet, Some((row, col))))),
            ("AddReply", Func) => {
                self.book_mut(book)?.threads[at].1.replies += 1;
                Ok(Variant::Dispatch(None))
            }
            ("Author", Get) => Ok(self.alloc(Object::Stub("Author"))),
            _ => Err(unsupported("CommentThreaded", m)),
        }
    }

    pub(crate) fn thread_list_member(
        &mut self,
        book: BookId,
        sheet: SheetId,
        cell: Option<(u32, u32)>,
        m: &'static Member,
    ) -> Result<Variant, OmError> {
        if m.name != "Count" {
            return Err(unsupported("CommentsThreaded", m));
        }
        let bk = self.book(book)?;
        let n = match cell {
            None => bk.threads.iter().filter(|(k, _)| k.0 == sheet).count() as u32,
            Some((r, c)) => bk
                .threads
                .iter()
                .find(|(k, _)| *k == (sheet, r, c))
                .map_or(0, |(_, t)| t.replies),
        };
        Ok(Variant::I4(n as i32))
    }

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

        let cell = CellAddr::new(sheet, rect.r0, rect.c0);
        let anchor = self
            .book(book)?
            .wb
            .merge_at(cell)
            .map_or((rect.r0, rect.c0), |m| (m.r0, m.c0));
        let key = (sheet, anchor.0, anchor.1);
        if self.book(book)?.threads.iter().any(|(k, _)| *k == key) {
            return Err(OmError::app_defined());
        }
        let author = user_name();
        let text = match a.get(0) {
            None => String::new(),

            Some(Variant::Empty | Variant::Null) => return Err(OmError::app_defined()),
            Some(v) if !matches!(v, Variant::Bstr(_) | Variant::Error(_)) => {
                return Err(OmError::app_defined())
            }
            Some(_) => a.string(0)?.unwrap_or_default(),
        };

        let existing = self
            .book(book)?
            .wb
            .note_at(sheet, anchor.0, anchor.1)
            .map(|n| (n.id, n.text.clone()));
        if let Some((id, old)) = existing {
            if anchor == (rect.r0, rect.c0) {
                return Err(OmError::app_defined());
            }
            if !text.is_empty() {
                let new = set_note_text(&old, &text, 1, None);
                self.book_mut(book)?
                    .wb
                    .note_edit(sheet, id, |n| n.text = new.clone());
            }
            return Ok(self.alloc(Object::Comment(book, sheet, id)));
        }

        let text = if text.is_empty() && self.running.is_none() {
            format!("{author}:\n")
        } else {
            set_note_text("", &text, 1, None)
        };
        let at = CellAddr::new(sheet, anchor.0, anchor.1);
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

                if i < 1 {
                    return Err(OmError::exception(crate::error::SCODE_BAD_INDEX, None));
                }
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
                let count = match a.get(2) {
                    None => None,
                    Some(Variant::Bool(true)) => return Err(invalid_arg()),

                    Some(Variant::Bool(false) | Variant::Null | Variant::Empty) => Some(0),

                    Some(_) => match a.i32(2) {
                        Ok(Some(c)) if (0..i32::MAX).contains(&c) => Some(c),
                        _ => return Err(invalid_arg()),
                    },
                };
                let new = match a.get(0) {
                    None => return Ok(Variant::Bstr(note.text)),

                    Some(Variant::Bstr(s)) if s.is_empty() => return Ok(Variant::Bstr(note.text)),
                    Some(Variant::Bstr(s)) => s.clone(),
                    Some(_) => return Err(OmError::app_defined()),
                };

                let start = match a.i32(1) {
                    Ok(Some(s)) if !(1..i32::MAX).contains(&s) => return Err(invalid_arg()),
                    Ok(s) => s,
                    Err(_) => return Err(invalid_arg()),
                };
                let text = set_note_text(
                    &note.text,
                    &new,
                    start.map_or(1, |s| s as usize),
                    count.map(|c| c as usize),
                );
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
