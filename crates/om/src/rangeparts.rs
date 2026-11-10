use xlcalc::hyperlink::{normalize_address, split_fragment, Hyperlink};
use xlcalc::{CellAddr, Rect, SheetId, Value};
use xlcalc_variant::Variant;

use crate::app::{unsupported, Application, BookId, Mode, Object, RangeRef};
use crate::conv::Args;
use crate::error::{OmError, DISP_E_TYPEMISMATCH};
use crate::style::StyleOwner;
use crate::typelib::{InvokeKind, Member};

const CREATOR: i32 = 0x5843_454C;

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub(crate) struct CharSpan {
    pub(crate) start: Option<i32>,
    pub(crate) len: Option<i32>,
}

impl Application<'_> {
    fn characters_source(&self, r: &RangeRef) -> Result<Option<String>, OmError> {
        let (sheet, rect) = self.resolve(r)?;
        if !rect.is_cell() {
            return Ok(None);
        }
        let wb = &self.book(r.book)?.wb;
        let at = CellAddr::new(sheet, rect.r0, rect.c0);
        if wb.formula_text(at).is_some() {
            return Ok(None);
        }
        Ok(match wb.value(at) {
            Value::Text(t) => Some(t.to_string()),
            _ => None,
        })
    }

    fn is_blank_cell(&self, r: &RangeRef) -> Result<bool, OmError> {
        let (sheet, rect) = self.resolve(r)?;
        let wb = &self.book(r.book)?.wb;
        let at = CellAddr::new(sheet, rect.r0, rect.c0);
        Ok(rect.is_cell() && wb.formula_text(at).is_none() && matches!(wb.value(at), Value::Blank))
    }

    pub(crate) fn hyperlinks_member(
        &mut self,
        book: BookId,
        sheet: SheetId,
        rect: Rect,
        m: &'static Member,
        a: &Args,
    ) -> Result<Variant, OmError> {
        use InvokeKind::*;
        let ids = |this: &Application| -> Result<Vec<u32>, OmError> {
            Ok(this
                .book(book)?
                .wb
                .hyperlinks(sheet)
                .iter()
                .filter(|h| h.rect.intersection(&rect).is_some())
                .map(|h| h.id)
                .collect())
        };
        match (m.name, m.kind) {
            ("Count", Get) => Ok(Variant::I4(ids(self)?.len() as i32)),
            ("Application", Get) => Ok(Variant::Dispatch(Some(self.handle()))),
            ("Creator", Get) => Ok(Variant::I4(CREATOR)),
            ("Parent", Get) => Ok(self.alloc(Object::Worksheet(book, sheet))),
            ("Item" | "_Default", _) => {
                let ids = ids(self)?;
                let i = a.i32(0).map_err(|_| OmError::bad_index())?.unwrap_or(0);
                match usize::try_from(i - 1).ok().and_then(|i| ids.get(i)) {
                    Some(id) => Ok(self.alloc(Object::Hyperlink(book, sheet, *id))),
                    None => Err(OmError::bad_index()),
                }
            }
            ("_NewEnum", Get) => {
                let items = ids(self)?
                    .into_iter()
                    .map(|id| self.alloc(Object::Hyperlink(book, sheet, id)))
                    .collect();
                Ok(self.alloc_enumerator(items))
            }
            ("Delete", Func) => {
                for id in ids(self)? {
                    self.book_mut(book)?.wb.hyperlink_remove(sheet, id);
                }
                Ok(Variant::Empty)
            }
            ("Add", Func) => self.hyperlinks_add(book, a),
            _ => Err(unsupported("Hyperlinks", m)),
        }
    }

    fn hyperlinks_add(&mut self, book: BookId, a: &Args) -> Result<Variant, OmError> {
        let Some(Variant::Dispatch(Some(_))) = a.get(0) else {
            let given = a
                .values
                .iter()
                .rposition(Option::is_some)
                .map_or(0, |i| i + 1);
            let last = given.saturating_sub(1) as u32;
            return Err(OmError::arg(DISP_E_TYPEMISMATCH, last));
        };

        let (anchor_book, sheet, rects) = self.range_areas(a.get(0))?;
        let raw = a.string(1)?.unwrap_or_default();
        let (mut address, hashed) = normalize_address(&raw);
        let mut sub = a.string(2)?.unwrap_or_default();
        if hashed {
            if sub.is_empty() {
                sub = address;
            }
            address = String::new();
        } else if let (a, Some(frag)) = split_fragment(&address) {
            address = a;
            if sub.is_empty() {
                sub = frag;
            }
        }
        let tip = a.string(3)?.filter(|t| !t.is_empty());
        let ttd = a.string(4)?.filter(|t| !t.is_empty());
        let mut first = None;
        for (k, rect) in rects.iter().enumerate() {
            let mut link = Hyperlink {
                id: 0,
                rect: *rect,
                address: address.clone(),
                sub_address: sub.clone(),
                screen_tip: tip.clone(),
                name: None,
            };
            let shown = ttd.clone().or_else(|| link.display_name());
            link.name = ttd.clone();
            let at = CellAddr::new(sheet, rect.r0, rect.c0);
            let wb = &mut self.book_mut(anchor_book)?.wb;
            if let Some(t) = shown.filter(|_| k == 0) {
                let constant = wb.formula_text(at).is_none();
                let blank = constant && matches!(wb.value(at), Value::Blank);
                let text = constant && matches!(wb.value(at), Value::Text(_));
                if blank || (text && ttd.is_some()) {
                    wb.set_value(at, Value::Text(t.as_str().into()));
                }
            }
            let id = wb.hyperlink_add(sheet, link);
            first.get_or_insert(id);
            self.hyperlink_style(anchor_book, sheet, *rect)?;
        }
        let id = first.ok_or_else(OmError::app_defined)?;
        Ok(self.alloc(Object::Hyperlink(book, sheet, id)))
    }

    pub(crate) fn hyperlink_member(
        &mut self,
        book: BookId,
        sheet: SheetId,
        id: u32,
        m: &'static Member,
        a: &Args,
    ) -> Result<Variant, OmError> {
        use InvokeKind::*;
        let link = self
            .book(book)?
            .wb
            .hyperlinks(sheet)
            .iter()
            .find(|h| h.id == id)
            .cloned()
            .ok_or_else(OmError::object_required)?;
        let null_if_empty = |s: &str| {
            if s.is_empty() {
                Variant::BstrNull
            } else {
                Variant::Bstr(s.to_string())
            }
        };
        let e_fail = || OmError::exception(0x8000_4005_u32 as i32, None);

        let too_long = |t: &str| {
            (t.encode_utf16().count() > 254)
                .then(|| OmError::exception(0x8007_000E_u32 as i32, None))
        };
        let edit = |this: &mut Application, f: &dyn Fn(&mut Hyperlink)| -> Result<(), OmError> {
            if let Some(h) = this.book_mut(book)?.wb.hyperlink_mut(sheet, id) {
                f(h);
            }
            Ok(())
        };
        let at = CellAddr::new(sheet, link.rect.r0, link.rect.c0);
        match (m.name, m.kind) {
            ("Address", Get) => Ok(null_if_empty(&link.address)),
            ("Address", Put) => {
                let text = self.put_string(a)?;
                if let Some(e) = too_long(&text) {
                    return Err(e);
                }
                let (addr, hashed) = normalize_address(&text);
                let (addr, frag) = if hashed {
                    (addr, None)
                } else {
                    split_fragment(&addr)
                };
                edit(self, &|h| {
                    if hashed {
                        h.sub_address = addr.clone();
                        h.address.clear();
                    } else {
                        h.address = addr.clone();
                        if let Some(f) = &frag {
                            h.sub_address = f.clone();
                        }
                    }
                })?;
                Ok(Variant::Empty)
            }
            ("SubAddress", Get) => Ok(null_if_empty(&link.sub_address)),

            ("SubAddress", Put) => {
                let v = self.put_string(a)?;
                if let Some(e) = too_long(&v) {
                    return Err(e);
                }
                edit(self, &|h| {
                    h.sub_address = v.clone();
                    h.address.clear();
                })?;
                Ok(Variant::Empty)
            }
            ("ScreenTip", Get) => Ok(match &link.screen_tip {
                Some(t) => Variant::Bstr(t.clone()),
                None => Variant::BstrNull,
            }),
            ("ScreenTip", Put) => {
                let v = self.put_string(a)?;
                if let Some(e) = too_long(&v) {
                    return Err(e);
                }
                edit(self, &|h| h.screen_tip = Some(v.clone()))?;
                Ok(Variant::Empty)
            }
            ("TextToDisplay", Get) => {
                let wb = &self.book(book)?.wb;
                Ok(match wb.value(at) {
                    Value::Text(t) if wb.formula_text(at).is_none() && !t.is_empty() => {
                        Variant::Bstr(t.chars().take(255).collect())
                    }
                    _ => Variant::BstrNull,
                })
            }
            ("TextToDisplay", Put) => {
                let v = self.put_string(a)?;
                if v.encode_utf16().count() > 32767 {
                    return Err(OmError::exception(
                        0x8007_0057_u32 as i32,
                        Some("Value does not fall within the expected range."),
                    ));
                }
                edit(self, &|h| h.name = Some(v.clone()))?;

                if !v.is_empty() {
                    let wb = &mut self.book_mut(book)?.wb;
                    crate::range::enter_text(wb, at, &v, false, a.lcid)?;
                }
                Ok(Variant::Empty)
            }
            ("EmailSubject", Put) => {
                let t = self.put_string(a)?;
                if let Some(e) = too_long(&t) {
                    return Err(e);
                }
                if !link.address.to_ascii_lowercase().starts_with("mailto:") {
                    return Err(e_fail());
                }
                let base = link.address.split('?').next().unwrap_or("").to_string();
                let new = if t.is_empty() {
                    base
                } else {
                    format!("{base}?subject={t}")
                };
                edit(self, &|h| h.address = new.clone())?;
                Ok(Variant::Empty)
            }
            ("Shape", Get) => Err(OmError::app_defined()),
            ("Name", Get) => link.display_name().map(Variant::Bstr).ok_or_else(e_fail),
            ("Type", Get) => Ok(Variant::I4(0)),
            ("EmailSubject", Get) => {
                if link.address.is_empty() {
                    return Ok(Variant::Bstr(String::new()));
                }
                if !link.address.to_ascii_lowercase().starts_with("mailto:") {
                    return Err(e_fail());
                }
                let subject = link
                    .address
                    .split_once('?')
                    .map(|(_, q)| q)
                    .and_then(|q| {
                        q.split('&').find_map(|kv| {
                            let (k, v) = kv.split_once('=')?;
                            k.eq_ignore_ascii_case("subject").then(|| v.to_string())
                        })
                    })
                    .unwrap_or_default();
                Ok(Variant::Bstr(subject))
            }
            ("Range" | "Parent", Get) => self.new_range(book, sheet, link.rect, Mode::Cells),
            ("Application", Get) => Ok(Variant::Dispatch(Some(self.handle()))),
            ("Creator", Get) => Ok(Variant::I4(CREATOR)),
            ("Delete", Func) => {
                self.book_mut(book)?.wb.hyperlink_remove(sheet, id);
                Ok(Variant::Empty)
            }
            _ => Err(unsupported("Hyperlink", m)),
        }
    }

    pub(crate) fn characters_member(
        &mut self,
        r: &RangeRef,
        span: CharSpan,
        m: &'static Member,
        a: &Args,
    ) -> Result<Variant, OmError> {
        use InvokeKind::*;
        let source = self.characters_source(r)?;

        let usable = source.is_some() || self.is_blank_cell(r)?;
        let fail = |what: &str| OmError::app_defined_with(what);
        let text = || -> String {
            let Some(s) = &source else {
                return String::new();
            };
            let from = usize::try_from(span.start.unwrap_or(1).max(1) - 1).unwrap_or(0);
            let rest = s.chars().skip(from);
            match span.len {
                None => rest.collect(),
                Some(n) => rest.take(usize::try_from(n).unwrap_or(0)).collect(),
            }
        };
        match (m.name, m.kind) {
            ("Text" | "Caption", Get) if !usable => Err(fail(&format!(
                "Unable to get the {} property of the Characters class",
                m.name
            ))),
            ("Text" | "Caption", Put) if !usable => Err(fail(&format!(
                "Unable to set the {} property of the Characters class",
                m.name
            ))),
            ("Delete", Func) if !usable => Err(fail("Delete method of Characters class failed")),
            ("Insert", Func) if !usable => Err(fail("Insert method of Characters class failed")),
            ("Text" | "Caption", Put) => {
                let t = match &a.put {
                    Some(Variant::Bool(b)) => (if *b { "TRUE" } else { "FALSE" }).to_string(),
                    _ => self.put_string(a)?,
                };

                if t.chars().count() > 255 {
                    return Err(fail(&format!(
                        "Unable to set the {} property of the Characters class",
                        m.name
                    )));
                }
                self.characters_replace(r, span, source.as_deref().unwrap_or(""), &t)?;
                Ok(Variant::Empty)
            }
            ("Insert", Func) => {
                let t = a.string(0)?.unwrap_or_default();
                self.characters_replace(r, span, source.as_deref().unwrap_or(""), &t)?;
                Ok(Variant::Bstr(t))
            }
            ("Delete", Func) => {
                self.characters_replace(r, span, source.as_deref().unwrap_or(""), "")?;
                Ok(Variant::Null)
            }
            ("Text" | "Caption", Get) => Ok(Variant::Bstr(text())),

            ("Count", Get) => Ok(match (span.start, span.len, &source) {
                (Some(start), Some(len), _) if len != 0 => Variant::I4(start),
                (_, _, Some(s)) => {
                    Variant::I4(i32::try_from(s.chars().count()).unwrap_or(i32::MAX))
                }
                _ if self.is_blank_cell(r)? => Variant::I4(0),
                _ => Variant::Empty,
            }),

            ("PhoneticCharacters", Get) => Ok(Variant::BstrNull),
            ("PhoneticCharacters", Put | PutRef) => Ok(Variant::Empty),
            ("Application", Get) => Ok(Variant::Dispatch(Some(self.handle()))),
            ("Creator", Get) => Ok(Variant::I4(CREATOR)),
            ("Parent", Get) => Ok(self.alloc(Object::Range(*r))),
            ("Font", Get) => Ok(self.alloc(Object::Font(StyleOwner::Range(*r)))),
            _ => Err(unsupported("Characters", m)),
        }
    }

    fn characters_replace(
        &mut self,
        r: &RangeRef,
        span: CharSpan,
        old: &str,
        new: &str,
    ) -> Result<(), OmError> {
        let (sheet, rect) = self.resolve(r)?;
        let chars: Vec<char> = old.chars().collect();
        let from = usize::try_from(span.start.unwrap_or(1).max(1) - 1)
            .unwrap_or(0)
            .min(chars.len());
        let to = match span.len {
            None => chars.len(),
            Some(n) => (from + usize::try_from(n).unwrap_or(0)).min(chars.len()),
        };
        let mut out: String = chars[..from].iter().collect();
        out.push_str(new);
        out.extend(&chars[to..]);
        let at = CellAddr::new(sheet, rect.r0, rect.c0);
        self.book_mut(r.book)?
            .wb
            .set_value(at, Value::Text(out.as_str().into()));
        Ok(())
    }

    pub(crate) fn phonetic_member(
        &mut self,
        r: &RangeRef,
        m: &'static Member,
    ) -> Result<Variant, OmError> {
        use InvokeKind::*;
        match (m.name, m.kind) {
            ("Text", Get) => Ok(Variant::BstrNull),
            ("Visible", Get) => Ok(Variant::Bool(false)),
            ("Alignment" | "CharacterType", Get) => Ok(Variant::I4(0)),
            (_, Put | PutRef) => Ok(Variant::Empty),
            ("Application", Get) => Ok(Variant::Dispatch(Some(self.handle()))),
            ("Creator", Get) => Ok(Variant::I4(CREATOR)),
            ("Parent", Get) => Ok(self.alloc(Object::Range(*r))),
            ("Font", Get) => Ok(self.alloc(Object::Font(StyleOwner::Range(*r)))),
            _ => Err(unsupported("Phonetic", m)),
        }
    }
}
