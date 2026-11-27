use crate::sheetprot::ProtObject;
use std::sync::Arc;

use xlcalc::hyperlink::{normalize_address, split_fragment, Hyperlink};
use xlcalc::rich::{RichText, Run, RunValue};
use xlcalc::{CellAddr, Rect, Selection, SheetId, Value};
use xlcalc_variant::Variant;

use crate::addr::parse_range_text;
use crate::app::{unsupported, Application, BookId, Mode, Object, RangeRef, Track};
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

    fn bounds(self, text: &[u16]) -> (usize, usize) {
        let n = text.len();
        let from = usize::try_from(self.start.unwrap_or(1).max(1) - 1).unwrap_or(0);
        let to = self
            .len
            .filter(|l| *l > 0)
            .map_or(n, |l| from + usize::try_from(l).unwrap_or(0))
            .min(n);
        (snap(text, from.min(n)), snap(text, to))
    }
}

fn snap(text: &[u16], i: usize) -> usize {
    let inside = i > 0
        && matches!(text.get(i - 1), Some(0xD800..=0xDBFF))
        && matches!(text.get(i), Some(0xDC00..=0xDFFF));
    i - usize::from(inside)
}

fn run_value(runs: &[Run], i: usize, name: &str) -> Option<Variant> {
    runs.iter()
        .rev()
        .filter(|run| run.from <= i && i < run.to)
        .find_map(|run| run.parts.iter().rev().find(|(k, _)| *k == name))
        .map(|(_, v)| match v {
            RunValue::Bool(b) => Variant::Bool(*b),
            RunValue::Int(n) => Variant::I4(*n),
            RunValue::Num(n) => Variant::R8(*n),
            RunValue::Text(t) => Variant::Bstr(t.to_string()),
        })
}

fn kept_value(v: &Variant) -> Option<RunValue> {
    Some(match v {
        Variant::Bool(b) => RunValue::Bool(*b),
        Variant::I2(n) => RunValue::Int(i32::from(*n)),
        Variant::I4(n) => RunValue::Int(*n),
        Variant::R8(n) => RunValue::Num(*n),
        Variant::Bstr(t) => RunValue::Text(t.to_string().into()),
        _ => return None,
    })
}

fn part_names(member: &'static str) -> Vec<&'static str> {
    match member {
        "FontStyle" => vec!["Bold", "Italic"],
        "ColorIndex" | "ThemeColor" => vec!["Color"],

        "Superscript" | "Subscript" => vec!["Superscript", "Subscript"],
        _ => vec![member],
    }
}

impl Application<'_> {
    fn cell_at(&self, r: &RangeRef) -> Option<CellAddr> {
        let (sheet, rect) = self.resolve(r).ok()?;
        rect.is_cell()
            .then(|| CellAddr::new(sheet, rect.r0, rect.c0))
    }

    fn live_runs(&self, r: &RangeRef) -> Option<(Arc<str>, Vec<Run>)> {
        let at = self.cell_at(r)?;
        let rich = self.book(r.book).ok()?.wb.rich_text(at)?;

        (!rich.runs.is_empty()).then(|| (rich.text.clone(), rich.runs.clone()))
    }

    fn unset_runs(&mut self, r: &RangeRef, member: &'static str) {
        let Some(at) = self.cell_at(r) else {
            return;
        };
        if let Ok(bk) = self.book_mut(r.book) {
            let mut names = part_names(member);
            names.push(member);
            bk.wb.rich_unset(at, &names);
        }
    }

    fn run_cells(&self, areas: &[RangeRef]) -> Vec<RangeRef> {
        let mut out = Vec::new();
        for r in areas {
            let Ok((sheet, rect)) = self.resolve(r) else {
                continue;
            };
            let Ok(bk) = self.book(r.book) else {
                continue;
            };
            out.extend(
                bk.wb
                    .rich_cells(sheet, rect)
                    .into_iter()
                    .map(|(row, col)| RangeRef {
                        track: Track::Fixed(sheet, Rect::cell(row, col)),
                        ..*r
                    }),
            );
        }
        out
    }

    pub(crate) fn range_font_member(
        &mut self,
        owner: &StyleOwner,
        areas: &[RangeRef],
        m: &'static Member,
        a: &Args,
    ) -> Result<Variant, OmError> {
        let cells = self.run_cells(areas);
        if matches!(m.kind, InvokeKind::Put | InvokeKind::PutRef) {
            let result = self.font_member(owner, m, a)?;
            for c in &cells {
                self.unset_runs(c, m.name);
            }
            return Ok(result);
        }
        let base = self.font_member(owner, m, a)?;
        if base == Variant::Null {
            return Ok(base);
        }
        let names = part_names(m.name);
        for c in cells {
            let touched = self.live_runs(&c).is_some_and(|(_, runs)| {
                runs.iter()
                    .any(|run| run.parts.iter().any(|(n, _)| names.contains(n)))
            });
            if touched && self.char_font_member(&c, CharSpan::WHOLE, m, a)? != base {
                return Ok(Variant::Null);
            }
        }
        Ok(base)
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
            let (Some(text), Some(member), Some(v), Some(at)) =
                (text, member, a.put.clone(), self.cell_at(r))
            else {
                self.unset_runs(r, m.name);

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
            let units: Vec<u16> = xlcalc::wide::units(&text).collect();
            let (from, to) = span.bounds(&units);
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
                    Variant::I2(n) => vec![(member, Variant::R8(f64::from(n)))],
                    Variant::I4(n) => vec![(member, Variant::R8(f64::from(n)))],
                    v => vec![(member, v)],
                },
                _ => vec![(member, v)],
            };
            let n = xlcalc::wide::units(&text).count();
            if from < to {
                let parts = parts
                    .iter()
                    .filter_map(|(k, v)| kept_value(v).map(|v| (*k, v)))
                    .collect();
                runs.push(Run {
                    from,
                    to,
                    parts,
                    file: None,
                });
            }

            let whole = !parts.is_empty()
                && parts.iter().all(|(name, v)| {
                    (0..n).all(|i| run_value(&runs, i, name).is_some_and(|x| x == *v))
                });
            if whole {
                self.chars_font_put = true;
                let put = self.font_member(&owner, m, a);
                self.chars_font_put = false;
                put?;
                for run in &mut runs {
                    run.parts.retain(|(k, _)| !parts.iter().any(|p| p.0 == *k));
                }
            }
            let text = Arc::from(text.as_str());
            let wb = &mut self.book_mut(r.book)?.wb;
            let phonetic = wb.rich_text(at).and_then(|x| x.phonetic.clone());
            wb.rich_set(
                at,
                RichText {
                    text,
                    runs,
                    phonetic,
                },
            );
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
        let names = part_names(m.name);
        let color_index = m.name == "ColorIndex";
        let theme = self.book(r.book)?.wb.style_table().theme.clone();
        let base_index = match &base {
            Variant::I4(-4105) => 1,
            Variant::I4(n) => *n,
            _ => 1,
        };
        let units: Vec<u16> = xlcalc::wide::units(&text).collect();
        let (mut from, mut to) = span.bounds(&units);
        if from >= to {
            if units.is_empty() {
                return Ok(base);
            }

            from = if from < units.len() {
                from.saturating_sub(1)
            } else {
                0
            };
            to = from + 1;
        }

        let at = |i: usize| -> Vec<Variant> {
            let i = i + usize::from(i + 1 < units.len() && snap(&units, i + 1) == i);
            names
                .iter()
                .map(|nm| {
                    let over = run_value(&runs, i, nm);
                    match (over, style_base) {
                        (Some(Variant::R8(c)), _) if color_index => {
                            let bgr = c as u32;
                            let rgb = ((bgr & 0xFF) << 16) | (bgr & 0xFF00) | ((bgr >> 16) & 0xFF);
                            Variant::I4(theme.color_index(rgb))
                        }
                        (None, _) if color_index => Variant::I4(base_index),
                        (None, _) if matches!(*nm, "Superscript" | "Subscript") => {
                            Variant::Bool(false)
                        }
                        (Some(v), _) => v,
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
        if let (Some(Variant::I4(i)), "ColorIndex") = (a.put.as_ref(), m.name) {
            if let Some(&rgb) = usize::try_from(i - 1)
                .ok()
                .and_then(|k| xlcalc::style::PALETTE.get(k))
            {
                return Ok(((rgb & 0xFF) << 16) | (rgb & 0xFF00) | ((rgb >> 16) & 0xFF));
            }
        }
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

    pub(crate) fn host_text(v: &Variant) -> Variant {
        match v {
            Variant::Bool(b) => Variant::Bstr(if *b { "True" } else { "False" }.to_string()),
            Variant::Date(d) if d.fract() == 0.0 && *d >= 61.0 => {
                let z = *d as i64 - 25569 + 719_468;
                let era = z.div_euclid(146_097);
                let doe = z.rem_euclid(146_097);
                let yoe = (doe - doe / 1460 + doe / 36_524 - doe / 146_096) / 365;
                let doy = doe - (365 * yoe + yoe / 4 - yoe / 100);
                let mp = (5 * doy + 2) / 153;
                let day = doy - (153 * mp + 2) / 5 + 1;
                let month = if mp < 10 { mp + 3 } else { mp - 9 };
                let year = yoe + era * 400 + i64::from(month <= 2);
                Variant::Bstr(format!("{day}/{month:02}/{year}"))
            }
            v => v.clone(),
        }
    }

    fn hyperlink_text_args(a: &Args) -> Result<(), OmError> {
        for i in 2..=4 {
            match a.get(i) {
                None | Some(Variant::Bstr(_) | Variant::BstrNull) => {}
                Some(v) if crate::conv::is_missing(v) => {}
                Some(_) => return Err(OmError::exception(0x8007_0057_u32 as i32, None)),
            }
        }
        Ok(())
    }

    fn hyperlinks_add(&mut self, book: BookId, a: &Args) -> Result<Variant, OmError> {
        Self::hyperlink_text_args(a)?;

        let ranged;
        let a = match a.get(1) {
            Some(v @ Variant::Dispatch(Some(_))) => {
                let (rs, r) = self
                    .range_arg(Some(v))
                    .map_err(|_| OmError::arg(DISP_E_TYPEMISMATCH, 0))?;
                let text = match self.book(book)?.wb.value(CellAddr::new(rs, r.r0, r.c0)) {
                    Value::Text(t) => t.to_string(),
                    Value::Number(n) => a.variant_string(&Variant::R8(n))?.unwrap_or_default(),
                    Value::Blank => String::new(),
                    _ => return Err(OmError::arg(DISP_E_TYPEMISMATCH, 0)),
                };
                let mut values = a.values.clone();
                values[1] = Some(Variant::Bstr(text));
                ranged = Args {
                    values,
                    put: a.put.clone(),
                    lcid: a.lcid,
                };
                &ranged
            }
            _ => a,
        };
        if a.get(1).is_none() && matches!(a.get(0), Some(Variant::Dispatch(_))) {
            return Err(OmError::hresult(crate::error::DISP_E_BADPARAMCOUNT));
        }
        let host_args = Args {
            values: a
                .values
                .iter()
                .enumerate()
                .map(|(i, v)| match v {
                    Some(v) if i > 0 => Some(Self::host_text(v)),
                    v => v.clone(),
                })
                .collect(),
            put: a.put.clone(),
            lcid: crate::conv::HOST_LCID,
        };
        let a = &host_args;
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
            Self::hyperlink_text_args(a)?;
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

        Self::hyperlink_text_args(a)?;
        let (anchor_book, sheet, rects) = self.range_areas(a.get(0))?;
        for rect in &rects {
            self.object_guard(anchor_book, sheet, ProtObject::Hyperlink, Some(*rect))?;
        }
        let raw = a.string(1)?.unwrap_or_default();
        if xlcalc::hyperlink::add_refused(
            &raw,
            &a.string(2)?.unwrap_or_default(),
            &a.string(3)?.unwrap_or_default(),
            &a.string(4)?.unwrap_or_default(),
        ) {
            return Err(OmError::exception(0x800A_03EC_u32 as i32, None));
        }
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

        let host_args;
        let a = if m.kind == Put {
            if a.put.as_ref().is_some_and(crate::conv::is_missing) {
                return Err(OmError::hresult(crate::error::DISP_E_PARAMNOTOPTIONAL));
            }
            host_args = Args {
                values: a.values.clone(),
                put: a.put.as_ref().map(Self::host_text),
                lcid: crate::conv::HOST_LCID,
            };
            &host_args
        } else {
            a
        };
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
            (xlcalc::wide::units(t).count() > 254)
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
                if matches!(a.put, None | Some(Variant::Null)) {
                    return Ok(Variant::Empty);
                }

                if matches!(a.put, Some(Variant::Empty)) {
                    let wb = &mut self.book_mut(book)?.wb;
                    let constant = wb.formula_text(at).is_none();
                    let again = matches!(wb.value(at), Value::Text(_) | Value::Bool(_));
                    if let Some(t) = link.display_name().filter(|_| constant && again) {
                        crate::range::enter_text(wb, at, &t, false, a.lcid)?;
                    }
                    return Ok(Variant::Empty);
                }
                let mut v = self.put_string_alpha(a)?;

                if v.is_empty() {
                    if !link.address.is_empty() {
                        return Ok(Variant::Empty);
                    }
                    v = link.sub_address.clone();
                }

                if xlcalc::wide::units(&v).count() > 0x824 {
                    return Err(OmError::exception(0x8007_0057_u32 as i32, None));
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
            let units: Vec<u16> = xlcalc::wide::units(s).collect();
            let from = usize::try_from(span.start.unwrap_or(1).max(1) - 1).unwrap_or(0);

            let len = span
                .len
                .map_or(usize::MAX, |n| usize::try_from(n).unwrap_or(usize::MAX));
            let to = snap(&units, from.saturating_add(len).min(units.len()));
            let from = snap(&units, from.min(units.len()));
            xlcalc::wide::from_units(&units[from..to.max(from)])
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

                if xlcalc::wide::units(&t).count() > 255 {
                    return Err(fail(&format!(
                        "Unable to set the {} property of the Characters class",
                        m.name
                    )));
                }

                let old = source.as_deref().unwrap_or("");
                if xlcalc::wide::units(&characters_spliced(old, span, &t)).count() > 255 {
                    return Ok(Variant::Empty);
                }
                self.characters_replace(r, span, old, &t)?;
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

            ("Text" | "Caption", Get) => {
                let t = text();
                if xlcalc::wide::units(&t).count() > 256 {
                    return Err(fail(&format!(
                        "Unable to get the {} property of the Characters class",
                        m.name
                    )));
                }
                Ok(Variant::Bstr(t))
            }

            ("Count", Get) => match (span.start, span.len, &source) {
                (Some(start), Some(len), _) if len > 0 => Ok(Variant::I4(start)),
                (_, len, _) if !usable && len.is_none_or(|n| n <= 0) => Err(fail(
                    "Unable to get the Count property of the Characters class",
                )),
                (_, _, Some(s)) => Ok(Variant::I4(
                    i32::try_from(xlcalc::wide::units(s).count()).unwrap_or(i32::MAX),
                )),
                _ if self.is_blank_cell(r)? => Ok(Variant::I4(0)),
                _ => Ok(Variant::Empty),
            },

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
        let out = characters_spliced(old, span, new);
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

fn characters_spliced(old: &str, span: CharSpan, new: &str) -> String {
    let mut out: Vec<u16> = xlcalc::wide::units(old).collect();
    let n = out.len();
    let from = usize::try_from(span.start.unwrap_or(1).max(1) - 1)
        .unwrap_or(0)
        .min(n);
    let to = match span.len {
        None => n,
        Some(len) => (from + usize::try_from(len).unwrap_or(0)).min(n),
    };
    let (from, to) = (snap(&out, from), snap(&out, to));
    out.splice(from..to, xlcalc::wide::units(new));
    xlcalc::wide::from_units(&out)
}
