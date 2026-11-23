use crate::sheetprot::ProtObject;
use std::collections::HashMap;

use xlcalc::hyperlink::{normalize_address, split_fragment, Hyperlink};
use xlcalc::{CellAddr, Rect, Selection, SheetId, Value};
use xlcalc_variant::Variant;

use crate::addr::parse_range_text;
use crate::app::{unsupported, Application, BookId, Mode, Object, RangeRef};
use crate::conv::Args;
use crate::error::{OmError, DISP_E_TYPEMISMATCH};
use crate::objects::edit_error;
use crate::style::StyleOwner;
use crate::typelib::{InvokeKind, Member};

const CREATOR: i32 = 0x5843_454C;

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub(crate) struct CharSpan {
    pub(crate) start: Option<i32>,
    pub(crate) len: Option<i32>,
}

impl CharSpan {
    pub(crate) const WHOLE: CharSpan = CharSpan {
        start: None,
        len: None,
    };

    fn bounds(self, n: usize) -> (usize, usize) {
        let from = usize::try_from(self.start.unwrap_or(1).max(1) - 1).unwrap_or(0);
        let to = self
            .len
            .filter(|l| *l > 0)
            .map_or(n, |l| from + usize::try_from(l).unwrap_or(0))
            .min(n);
        (from, to)
    }
}

type Run = (usize, usize, Vec<(&'static str, Variant)>);

type CellKey = (BookId, SheetId, u32, u32);

type CellRuns = (String, Vec<Run>);

#[derive(Default)]
pub(crate) struct CharRuns(HashMap<CellKey, CellRuns>);

impl Application<'_> {
    fn live_runs(&self, r: &RangeRef) -> Option<(String, Vec<Run>)> {
        let key = self.cell_key(r)?;
        let (text, runs) = self.char_runs.0.get(&key)?;
        (self.characters_source(r).ok()?.as_deref() == Some(text.as_str()))
            .then(|| (text.clone(), runs.clone()))
    }

    fn cell_key(&self, r: &RangeRef) -> Option<(BookId, SheetId, u32, u32)> {
        let (sheet, rect) = self.resolve(r).ok()?;
        rect.is_cell().then_some((r.book, sheet, rect.r0, rect.c0))
    }

    pub(crate) fn char_font_member(
        &mut self,
        r: &RangeRef,
        span: CharSpan,
        m: &'static Member,
        a: &Args,
    ) -> Result<Variant, OmError> {
        let owner = StyleOwner::Range(*r);
        let member = FONT_PARTS.iter().find(|n| **n == m.name).copied();
        if matches!(m.kind, InvokeKind::Put | InvokeKind::PutRef) {
            let text = if span == CharSpan::WHOLE {
                None
            } else {
                self.characters_source(r)?
            };
            let text_cell = text.is_some();
            let (Some(text), Some(member), Some(v), Some(key)) =
                (text, member, a.put.clone(), self.cell_key(r))
            else {
                if let Some((_, runs)) = self.cell_key(r).and_then(|k| self.char_runs.0.get_mut(&k))
                {
                    for run in runs.iter_mut() {
                        run.2.retain(|(n, _)| *n != m.name);
                    }
                }

                let keep =
                    if span.start.is_some_and(|s| s > 1) && !text_cell && !self.is_blank_cell(r)? {
                        self.resolve(r)
                            .ok()
                            .filter(|(_, rect)| rect.is_cell())
                            .and_then(|(sheet, rect)| {
                                let at = CellAddr::new(sheet, rect.r0, rect.c0);
                                let wb = &self.book(r.book).ok()?.wb;
                                Some((sheet, rect, wb.cell_style(at).clone()))
                            })
                    } else {
                        None
                    };
                self.chars_font_put = span != CharSpan::WHOLE;
                let put = self.font_member(&owner, m, a);
                self.chars_font_put = false;
                if let Some((sheet, rect, style)) = keep {
                    self.book_mut(r.book)?
                        .wb
                        .set_style_with(sheet, rect, &|_| style.clone());
                }
                return put;
            };
            let (from, to) = span.bounds(text.chars().count());
            let mut runs = self.live_runs(r).map(|x| x.1).unwrap_or_default();

            let parts: Vec<(&'static str, Variant)> = match member {
                "ColorIndex" | "ThemeColor" | "Color" => {
                    vec![("Color", Variant::R8(f64::from(self.run_color(r, m, a)?)))]
                }
                "FontStyle" => {
                    let t = match &v {
                        Variant::Bstr(t) => t.to_ascii_lowercase(),
                        _ => String::new(),
                    };
                    let words: Vec<&str> = t.split_whitespace().collect();
                    let known = ["regular", "bold", "italic"];
                    if words.is_empty() || !words.iter().all(|w| known.contains(w)) {
                        return Ok(Variant::Empty);
                    }
                    vec![
                        ("Bold", Variant::Bool(words.contains(&"bold"))),
                        ("Italic", Variant::Bool(words.contains(&"italic"))),
                    ]
                }

                "Size" => match v {
                    Variant::I4(n) => vec![(member, Variant::R8(f64::from(n)))],
                    v => vec![(member, v)],
                },
                _ => vec![(member, v)],
            };
            if from < to {
                runs.push((from, to, parts));
            }
            self.char_runs.0.insert(key, (text, runs));
            return Ok(Variant::Empty);
        }

        if span != CharSpan::WHOLE
            && m.name != "ThemeFont"
            && !self.resolve(r).is_ok_and(|(_, rect)| rect.is_cell())
        {
            return Err(OmError::app_defined_with(&format!(
                "Unable to get the {} property of the Font class",
                m.name
            )));
        }

        if span != CharSpan::WHOLE && m.name == "Background" {
            return Ok(Variant::I4(0));
        }
        let base = self.font_member(&owner, m, a)?;
        let Some((text, runs)) = self.live_runs(r) else {
            return Ok(base);
        };
        let style_base = match &base {
            Variant::Bstr(t) if m.name == "FontStyle" => {
                Some((t.contains("Bold"), t.contains("Italic")))
            }
            _ => None,
        };
        let names: &[&str] = match m.name {
            "FontStyle" => &["Bold", "Italic"],
            "ColorIndex" => &["Color"],

            "Superscript" | "Subscript" => &["Superscript", "Subscript"],
            _ => &[m.name],
        };
        let color_index = m.name == "ColorIndex";
        let base_index = match &base {
            Variant::I4(-4105) => 1,
            Variant::I4(n) => *n,
            _ => 1,
        };
        let (from, to) = span.bounds(text.chars().count());
        if from >= to {
            return Ok(base);
        }

        let at = |i: usize| -> Vec<Variant> {
            names
                .iter()
                .map(|nm| {
                    let over = runs
                        .iter()
                        .rev()
                        .filter(|run| run.0 <= i && i < run.1)
                        .find_map(|run| run.2.iter().rev().find(|(k, _)| k == nm).map(|p| &p.1));
                    match (over, style_base) {
                        (Some(Variant::R8(c)), _) if color_index => {
                            let bgr = *c as u32;
                            let rgb = ((bgr & 0xFF) << 16) | (bgr & 0xFF00) | ((bgr >> 16) & 0xFF);
                            Variant::I4(i32::from(xlcalc::style::nearest_color_index(rgb)))
                        }
                        (None, _) if color_index => Variant::I4(base_index),
                        (None, _) if matches!(*nm, "Superscript" | "Subscript") => {
                            Variant::Bool(false)
                        }
                        (Some(v), _) => v.clone(),
                        (None, Some((bold, _))) if *nm == "Bold" => Variant::Bool(bold),
                        (None, Some((_, italic))) => Variant::Bool(italic),
                        (None, None) => base.clone(),
                    }
                })
                .collect()
        };
        let first = at(from);
        if (from..to).any(|i| at(i) != first) {
            return Ok(Variant::Null);
        }
        Ok(match (m.name, first.as_slice()) {
            ("FontStyle", [Variant::Bool(b), Variant::Bool(i)]) => Variant::Bstr(
                match (b, i) {
                    (false, false) => "Regular",
                    (true, false) => "Bold",
                    (false, true) => "Italic",
                    (true, true) => "Bold Italic",
                }
                .into(),
            ),
            ("Superscript", [v, _]) => v.clone(),
            ("Subscript", [_, v]) => v.clone(),
            (_, [v]) => v.clone(),
            _ => base,
        })
    }

    fn run_color(&mut self, r: &RangeRef, m: &'static Member, a: &Args) -> Result<u32, OmError> {
        let (sheet, rect) = self.resolve(r)?;
        let at = CellAddr::new(sheet, rect.r0, rect.c0);
        let saved = self.book(r.book)?.wb.cell_style(at).clone();
        let put = self.font_member(&StyleOwner::Range(*r), m, a);
        let wb = &mut self.book_mut(r.book)?.wb;
        let color = wb.cell_style(at).font.color;
        let rgb = wb.style_table().theme.rgb_cv(color, 0);
        let bgr = ((rgb & 0xFF) << 16) | (rgb & 0xFF00) | ((rgb >> 16) & 0xFF);
        wb.set_style_with(sheet, rect, &|_| saved.clone());
        put?;
        Ok(bgr)
    }

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

        let whole = rect == crate::objects::whole_sheet();
        let ids = |this: &Application| -> Result<Vec<u32>, OmError> {
            let wb = &this.book(book)?.wb;
            let links = wb.hyperlinks(sheet);
            let cells = links
                .iter()
                .filter(|h| h.shape.is_none() && h.rect.intersection(&rect).is_some());

            let mut shapes: Vec<(usize, u32)> = links
                .iter()
                .filter(|_| whole)
                .filter_map(|h| {
                    let z = wb.drawing(sheet)?.index_of(h.shape?)?;
                    Some((z, h.id))
                })
                .collect();
            shapes.sort_unstable();
            Ok(cells
                .map(|h| h.id)
                .chain(shapes.into_iter().map(|(_, id)| id))
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
        let shape = match a.get(0) {
            Some(Variant::Dispatch(Some(id))) => match self.arena.get(*id) {
                Some(Object::Draw(d)) if d.kind == crate::shapes::DrawKind::Shape => {
                    Some((d.book, d.sheet, d.id))
                }
                _ => None,
            },
            _ => None,
        };
        if let Some((sb, ss, sid)) = shape {
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
            let link = Hyperlink {
                id: 0,
                rect: Rect::cell(0, 0),
                address,
                sub_address: sub,
                screen_tip: Some(a.string(3)?.unwrap_or_default()),
                name: None,
                shape: Some(sid),
            };
            let id = self.book_mut(sb)?.wb.hyperlink_add(ss, link);
            return Ok(self.alloc(Object::Hyperlink(book, ss, id)));
        }
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
        for rect in &rects {
            self.object_guard(anchor_book, sheet, ProtObject::Hyperlink, Some(*rect))?;
        }
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

        let ttd_given = a.string(4)?.is_some();
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
                shape: None,
            };
            let shown = ttd.clone().or_else(|| link.display_name());
            link.name = ttd.clone();
            let at = CellAddr::new(sheet, rect.r0, rect.c0);
            let wb = &mut self.book_mut(anchor_book)?.wb;
            if let Some(t) = shown.filter(|_| k == 0) {
                let constant = wb.formula_text(at).is_none();
                let blank = constant && matches!(wb.value(at), Value::Blank);
                let text = constant && matches!(wb.value(at), Value::Text(_));
                if blank || (text && ttd_given) {
                    crate::range::enter_text(wb, at, &t, false, a.lcid)?;
                }
            }
            let id = wb.hyperlink_add(sheet, link);
            first.get_or_insert(id);
            self.hyperlink_style(anchor_book, sheet, *rect)?;
        }
        let id = first.ok_or_else(OmError::app_defined)?;
        Ok(self.alloc(Object::Hyperlink(book, sheet, id)))
    }

    pub(crate) fn follow_target(
        &mut self,
        book: BookId,
        sheet: SheetId,
        address: &str,
        sub_address: &str,
    ) -> Result<(), OmError> {
        let (mut book, mut sheet) = (book, sheet);
        let ext = address
            .rsplit_once('.')
            .map(|(_, e)| e.to_ascii_lowercase())
            .unwrap_or_default();
        if !address.is_empty() {
            if !["xlsx", "xlsm", "xlsb", "xltx", "xltm", "xlam", "xls"].contains(&ext.as_str()) {
                return Ok(());
            }
            let mut file = address.replace('/', "\\");

            let own = self.book(book)?.path.clone();
            if let Some((dir, _)) = own.rsplit_once('\\') {
                let joined = format!("{dir}\\{file}");
                if self.fs.exists(&joined) {
                    file = joined;
                }
            }
            let args = Args {
                values: vec![Some(Variant::Bstr(file))],
                put: None,
                lcid: 1033,
            };
            if self.open_with_events(&args).is_err() {
                return Err(OmError::exception(
                    0x8004_01EA_u32 as i32,
                    Some("Cannot open the specified file."),
                ));
            }
            let Some(opened) = self.active else {
                return Ok(());
            };
            book = opened;
            sheet = self.book(book)?.wb.active_sheet();
        }
        let wb = &self.book(book)?.wb;
        let sub = sub_address.trim();
        let named = wb.names().lookup(&sub.to_uppercase(), sheet).and_then(|d| {
            let t = d.refers_to.trim_start_matches('=').replace('$', "");
            parse_range_text(wb, sheet, &t)
        });
        let target = parse_range_text(wb, sheet, sub).or(named);
        if let Some((s, rect)) = target {
            self.select(book, s, Selection::of(&[rect]))?;
            self.book_mut(book)?
                .wb
                .set_active_sheet(s)
                .map_err(edit_error)?;
            self.active = Some(book);
        }
        Ok(())
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
                let v = self.put_string_alpha(a)?;
                if let Some(e) = too_long(&v) {
                    return Err(e);
                }
                edit(self, &|h| h.screen_tip = Some(v.clone()))?;
                Ok(Variant::Empty)
            }
            ("TextToDisplay", Get | Put) if link.shape.is_some() => Err(e_fail()),
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
                if matches!(a.put, None | Some(Variant::Null | Variant::Empty)) {
                    return Ok(Variant::Empty);
                }
                let mut v = self.put_string(a)?;
                if v.is_empty() {
                    v = if link.address.is_empty() {
                        link.sub_address.clone()
                    } else {
                        link.address.clone()
                    };
                }
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
            ("Shape" | "Parent", Get) if link.shape.is_some() => {
                let id = link.shape.unwrap_or_default();
                Ok(self.draw_obj(book, sheet, id, crate::shapes::DrawKind::Shape))
            }
            ("Range", Get) if link.shape.is_some() => {
                Err(OmError::exception(0x800A_03EC_u32 as i32, None))
            }
            ("Shape", Get) => Err(OmError::app_defined()),
            ("Name", Get) => link.display_name().map(Variant::Bstr).ok_or_else(e_fail),
            ("Type", Get) => Ok(Variant::I4(i32::from(link.shape.is_some()))),

            ("Follow", Func) => {
                self.follow_target(book, sheet, &link.address, &link.sub_address)?;
                if link.shape.is_none() {
                    self.raise_follow_hyperlink(book, sheet, id);
                }
                Ok(Variant::Empty)
            }
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
                let p = self.sheet_prot(book, sheet);
                if link.shape.is_none()
                    && p.contents
                    && !p.ui_only
                    && self.any_locked(book, sheet, link.rect)
                {
                    return Err(OmError::app_defined_with(crate::sheetprot::LOCKED_CELL));
                }
                self.book_mut(book)?.wb.hyperlink_remove(sheet, id);
                if link.shape.is_none() {
                    self.unhyperlink_style(book, sheet, link.rect)?;
                }
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

                Some(n) if n < 0 => rest.collect(),
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
            ("Font", Get) => Ok(self.alloc(Object::CharFont(*r, span))),
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

const FONT_PARTS: [&str; 14] = [
    "Shadow",
    "OutlineFont",
    "FontStyle",
    "ThemeColor",
    "ColorIndex",
    "Name",
    "Size",
    "Bold",
    "Italic",
    "Underline",
    "Strikethrough",
    "Superscript",
    "Subscript",
    "Color",
];
