use xlcalc::drawing::{Shape, ShapeKind};
use xlcalc::drawing_text::{CharFmt, FontColor, TextFrame};
use xlcalc::style::{nearest_color_index, PALETTE};
use xlcalc_variant::{HResult, Variant};

use super::{put_f64, put_i32, tri, DrawKind, DrawRef};
use crate::app::unsupported;
use crate::bag::Store;
use crate::conv::{is_missing, Args};
use crate::error::OmError;
use crate::typelib::{InvokeKind, Member};

const TO_END: u32 = u32::MAX;
const DEFAULT_FONT: &str = "Aptos Narrow";

fn invalid_arg() -> OmError {
    OmError::exception(HResult::E_INVALIDARG.0, None)
}

fn e_fail() -> OmError {
    OmError::exception(0x8000_4005_u32 as i32, None)
}

fn not_found() -> OmError {
    OmError::exception(0x8002_0003_u32 as i32, None)
}

fn utf16_len(s: &str) -> usize {
    s.encode_utf16().count()
}

fn swap_rb(c: u32) -> u32 {
    ((c & 255) << 16) | (c & 0xFF00) | ((c >> 16) & 255)
}

fn put_bool(a: &Args) -> Result<bool, OmError> {
    Ok(match a.put.as_ref() {
        Some(Variant::Bool(b)) => *b,
        _ => put_f64(a)? != 0.0,
    })
}

fn range_args(a: &Args, text_len: usize, excel: bool) -> Result<(u32, u32), OmError> {
    let arg = |i: usize| -> Result<Option<f64>, OmError> {
        match a.get(i) {
            Some(v) if !is_missing(v) => crate::controls::arg_num(a, i).map(Some),
            _ => Ok(None),
        }
    };
    let start = arg(0)?;
    let len = arg(1)?;
    if !excel && start.is_some_and(|v| v < 1.0) {
        return Err(invalid_arg());
    }
    let s = start.filter(|v| *v > 0.0).map_or(1, |v| v as usize);
    if s > text_len.max(1) {
        return Err(if excel {
            OmError::app_defined()
        } else {
            invalid_arg()
        });
    }
    let l = len
        .filter(|v| excel && *v > 0.0 || !excel && *v != 0.0)
        .map_or(if excel || len.is_none() { TO_END } else { 0 }, |v| {
            u32::try_from(v.abs() as usize).unwrap_or(TO_END)
        });
    if !excel && l != TO_END && s - 1 + l as usize > text_len {
        return Err(invalid_arg());
    }
    Ok((u32::try_from(s - 1).unwrap_or(0), l))
}

fn span(text_len: usize, start: u32, len: u32) -> (usize, usize) {
    let s = (start as usize).min(text_len);
    let e = if len == TO_END {
        text_len
    } else {
        (s + len as usize).min(text_len)
    };
    (s, e)
}

fn kind_range(k: DrawKind) -> (u32, u32) {
    match k {
        DrawKind::Chars(s, l)
        | DrawKind::TextRange2(s, l)
        | DrawKind::Font(s, l)
        | DrawKind::Font2(s, l)
        | DrawKind::ParaFmt(s, l)
        | DrawKind::TextFill(s, l)
        | DrawKind::TextColor(s, l) => (s, l),
        _ => (0, TO_END),
    }
}

fn default_color(sh: &Shape) -> u32 {
    if sh.kind == ShapeKind::TextBox {
        0
    } else {
        0xFF_FFFF
    }
}

fn color_of(sh: &Shape, c: FontColor) -> u32 {
    match c {
        FontColor::Default => default_color(sh),
        FontColor::Rgb(v) => v,
        FontColor::NoFill => 0xFF_FFFF,
    }
}

fn color_index(sh: &Shape, c: FontColor) -> i32 {
    match c {
        FontColor::Default => {
            if sh.kind == ShapeKind::TextBox {
                -4105
            } else {
                2
            }
        }
        FontColor::NoFill => -4142,
        FontColor::Rgb(0) => -4105,
        FontColor::Rgb(v) => i32::from(nearest_color_index(swap_rb(v))),
    }
}

fn underline_excel(u: u8) -> i32 {
    match u {
        0 => -4142,
        3 => -4119,
        _ => 2,
    }
}

fn tri_of(vals: &[&CharFmt], f: impl Fn(&CharFmt) -> bool) -> i32 {
    let first = vals.first().is_some_and(|c| f(c));
    if vals.iter().any(|c| f(c) != first) {
        -2
    } else if first {
        -1
    } else {
        0
    }
}

fn horizontal_excel(align: u8) -> i32 {
    match align {
        2 => -4108,
        3 => -4152,
        4 => -4130,
        5..=7 => -4117,
        _ => -4131,
    }
}

impl super::Application<'_> {
    fn with_frame<R>(
        &mut self,
        r: &DrawRef,
        f: impl FnOnce(&mut Shape) -> R,
    ) -> Result<R, OmError> {
        let x = self
            .draw_wb(r.book)?
            .drawing_mut(r.sheet)
            .get_mut(r.id)
            .ok_or_else(OmError::object_required)?;
        Ok(f(x))
    }

    fn replace_text(
        &mut self,
        r: &DrawRef,
        start: u32,
        len: u32,
        new: &str,
    ) -> Result<(), OmError> {
        self.with_frame(r, |x| {
            let mut units: Vec<u16> = x.text.encode_utf16().collect();
            let (s, e) = span(units.len(), start, len);
            let new_units: Vec<u16> = new.encode_utf16().collect();
            let old_len = units.len();
            x.props.tf.splice(old_len, s, e - s, new_units.len());
            units.splice(s..e, new_units);
            x.text = String::from_utf16_lossy(&units);
        })
    }

    pub(crate) fn text_member(
        &mut self,
        r: &DrawRef,
        m: &'static Member,
        a: &Args,
    ) -> Result<Variant, OmError> {
        use InvokeKind::*;
        let put = m.kind == Put || m.kind == PutRef;
        let sh = self.shape_clone(r)?;
        let text_len = utf16_len(&sh.text);
        let capable = matches!(sh.kind, ShapeKind::Auto(_) | ShapeKind::TextBox);
        let (rs, rl) = kind_range(r.kind);
        let book = r.book;
        let obj = |app: &mut Self, k: DrawKind| Ok(app.draw_obj(book, r.sheet, r.id, k));
        let name = m.name;
        match (r.kind, name, put) {
            (DrawKind::Fill, "Visible", false) if sh.kind == ShapeKind::Line => Ok(Variant::I4(0)),
            (DrawKind::Fill | DrawKind::LineFmt, "Visible", false) => Ok(Variant::I4(-1)),
            (DrawKind::TextFrame2, "HasText", false) => Ok(tri(text_len > 0)),
            (DrawKind::TextFrame2, "DeleteText", false) => {
                if capable {
                    self.with_frame(r, |x| {
                        x.text.clear();
                        x.props.tf.clear_text();
                    })?;
                }
                Ok(Variant::Empty)
            }
            (DrawKind::TextFrame, "AutoSize" | "Characters", _) if !capable => Err(not_found()),
            (DrawKind::TextFrame, _, _) if !capable => Err(invalid_arg()),
            (DrawKind::TextFrame2, "Column" | "ThreeD" | "WordArtformat", _) if !capable => {
                match self.bag_member(Store::Shape(r), "TextFrame2", "TextFrame2", m, a) {
                    Some(res) => res,
                    None => Err(invalid_arg()),
                }
            }
            (DrawKind::TextFrame2, _, _) if !capable => Err(invalid_arg()),
            (DrawKind::TextFrame, "Characters", _) => {
                let (s, l) = range_args(a, text_len, true)?;
                obj(self, DrawKind::Chars(s, l))
            }
            (DrawKind::TextFrame2, "TextRange", false) => {
                obj(self, DrawKind::TextRange2(0, TO_END))
            }
            (DrawKind::TextFrame, _, _) => self.frame1_member(r, m, a, &sh),
            (DrawKind::TextFrame2, _, _) => self.frame2_member(r, m, a, &sh),
            (DrawKind::TextRange2(..), "Characters", false) => {
                let (s, l) = range_args(a, text_len, false)?;
                obj(self, DrawKind::TextRange2(s, l))
            }
            (DrawKind::TextRange2(..) | DrawKind::Paras, "Paragraphs", false) => {
                match a.get(0).filter(|v| !is_missing(v)) {
                    None => obj(self, DrawKind::Paras),
                    Some(_) => {
                        let n = crate::controls::arg_num(a, 0)?;
                        if n == 1.0 || n == -1.0 {
                            obj(self, DrawKind::TextRange2(0, TO_END))
                        } else {
                            Err(invalid_arg())
                        }
                    }
                }
            }
            (DrawKind::Paras, "Count", false) => Ok(Variant::I4(1)),
            (DrawKind::Paras, "Item" | "_Default", false) => {
                let n = crate::controls::arg_num(a, 0)?;
                if n == 1.0 {
                    obj(self, DrawKind::TextRange2(0, TO_END))
                } else {
                    Err(invalid_arg())
                }
            }
            (DrawKind::Paras, _, _) => self.text_member(
                &DrawRef {
                    kind: DrawKind::TextRange2(0, TO_END),
                    ..*r
                },
                m,
                a,
            ),
            (DrawKind::Chars(..), "Insert", false) => {
                let t = match a.get(0) {
                    Some(Variant::Bstr(t)) => t.clone(),
                    _ => return Err(OmError::app_defined()),
                };
                self.replace_text(r, rs, rl, &t)?;
                Ok(Variant::Bstr(t))
            }
            (DrawKind::Chars(..), "Delete", false) => {
                self.replace_text(r, rs, rl, "")?;
                Ok(Variant::Null)
            }
            (DrawKind::Chars(..) | DrawKind::TextRange2(..), "Count" | "Length", false) => {
                let (s, e) = span(text_len, rs, rl);
                Ok(Variant::I4(i32::try_from(e - s).unwrap_or(i32::MAX)))
            }
            (DrawKind::TextRange2(..), "Start", false) => Ok(Variant::I4(rs as i32 + 1)),
            (DrawKind::TextRange2(..), "Font", false) => obj(self, DrawKind::Font2(rs, rl)),
            (DrawKind::TextRange2(..), "ParagraphFormat", false) => {
                obj(self, DrawKind::ParaFmt(rs, rl))
            }
            (DrawKind::Chars(..), "Font", false) => obj(self, DrawKind::Font(rs, rl)),
            (DrawKind::Chars(..) | DrawKind::TextRange2(..), "Text" | "_Default", false) => {
                let units: Vec<u16> = sh.text.encode_utf16().collect();
                let (s, e) = span(units.len(), rs, rl);
                Ok(Variant::Bstr(String::from_utf16_lossy(&units[s..e])))
            }
            (DrawKind::Chars(..) | DrawKind::TextRange2(..), "Text", true) => {
                let t = crate::controls::put_text(a)?;
                self.replace_text(r, rs, rl, &t)?;
                Ok(Variant::Empty)
            }
            (DrawKind::ParaFmt(..), "Alignment", false) => {
                Ok(Variant::I4(i32::from(sh.props.tf.align)))
            }
            (DrawKind::ParaFmt(..), "Alignment", true) => {
                let v = put_i32(a)?;
                if !(1..=7).contains(&v) {
                    return Err(invalid_arg());
                }
                self.with_frame(r, |x| x.props.tf.align = v as u8)?;
                Ok(Variant::Empty)
            }
            (DrawKind::Font(..), _, _) => self.font1_member(r, m, a, &sh, (rs, rl)),
            (DrawKind::Font2(..), "Fill", false) => obj(self, DrawKind::TextFill(rs, rl)),
            (DrawKind::Font2(..), _, _) => self.font2_member(r, m, a, &sh, (rs, rl)),
            (DrawKind::TextFill(..), "ForeColor", false) => obj(self, DrawKind::TextColor(rs, rl)),
            (DrawKind::TextColor(..), "RGB", false) => {
                let tf = &sh.props.tf;
                let vals = tf.range(text_len, rs as usize, span_len(text_len, rs, rl));
                let c = vals.first().map_or(FontColor::Default, |c| c.color);
                if vals.iter().any(|v| v.color != c) {
                    return Ok(Variant::I4(i32::MIN));
                }
                Ok(Variant::I4(color_of(&sh, c) as i32))
            }
            (DrawKind::TextColor(..), "RGB", true) => {
                let v = put_f64(a)? as i64 as u32 & 0xFF_FFFF;
                self.modify_chars(r, text_len, rs, rl, move |c| c.color = FontColor::Rgb(v))?;
                Ok(Variant::Empty)
            }
            _ => {
                let key = match r.kind {
                    DrawKind::Fill => Some("Fill"),
                    DrawKind::LineFmt => Some("Line"),
                    _ => None,
                };
                match key.and_then(|k| self.bag_member(Store::Shape(r), k, k, m, a)) {
                    Some(res) => res,
                    None => Err(unsupported(r.kind.interface_name(), m)),
                }
            }
        }
    }

    fn modify_chars(
        &mut self,
        r: &DrawRef,
        text_len: usize,
        start: u32,
        len: u32,
        f: impl Fn(&mut CharFmt),
    ) -> Result<(), OmError> {
        let (s, e) = span(text_len, start, len);
        self.with_frame(r, |x| x.props.tf.modify(text_len, s, e - s, f))
    }

    fn frame1_member(
        &mut self,
        r: &DrawRef,
        m: &'static Member,
        a: &Args,
        sh: &Shape,
    ) -> Result<Variant, OmError> {
        let put = m.kind == InvokeKind::Put || m.kind == InvokeKind::PutRef;
        let tf = &sh.props.tf;
        let text_box = sh.kind == ShapeKind::TextBox;
        match (m.name, put) {
            ("AutoMargins", _) => Err(OmError::app_defined()),
            ("AutoSize", false) | ("AutoSize", true) if !text_box && sh.text.is_empty() => {
                Err(OmError::app_defined())
            }
            ("AutoSize", false) => Ok(Variant::Bool(tf.autosize == 1)),
            ("AutoSize", true) => {
                let on = put_bool(a)?;
                self.with_frame(r, |x| {
                    x.props.tf.autosize = u8::from(on);
                    if text_box {
                        x.props.tf.wrap = !on;
                    }
                })?;
                Ok(Variant::Empty)
            }
            ("HorizontalAlignment", false) => Ok(Variant::I4(horizontal_excel(tf.align))),
            ("HorizontalAlignment", true) => {
                let v = put_i32(a)?;
                let al = match v {
                    0 => return Err(OmError::app_defined()),
                    -4131 | 1 => 1,
                    -4108 | 2 => 2,
                    -4152 | 3 => 3,
                    -4130 | 4 => 4,
                    -4117 | 5 => 7,
                    _ => 2,
                };
                self.with_frame(r, |x| x.props.tf.align = al)?;
                Ok(Variant::Empty)
            }
            ("VerticalAlignment", false) => Ok(Variant::I4(match tf.anchor {
                1 => -4160,
                4 => -4107,
                _ => -4108,
            })),
            ("VerticalAlignment", true) => {
                let v = put_i32(a)?;
                let an = match v {
                    -4160 | 1 => 1,
                    -4107 | 3 => 4,
                    -4108 | -4130 | -4117 | 2 | 4 | 5 => 3,
                    _ => return Err(OmError::app_defined()),
                };
                self.with_frame(r, |x| x.props.tf.anchor = an)?;
                Ok(Variant::Empty)
            }
            ("MarginLeft" | "MarginTop" | "MarginRight" | "MarginBottom", _) => {
                self.margin(r, m.name, put, a, tf)
            }
            ("Orientation", false) => Ok(Variant::I4(i32::from(tf.orient))),
            ("Orientation", true) => self.put_orientation(r, a),
            ("HorizontalOverflow", false) => Ok(Variant::I4(i32::from(tf.h_over))),
            ("HorizontalOverflow", true) => {
                let v = put_i32(a)?;
                if !(0..=1).contains(&v) {
                    return Err(e_fail());
                }
                self.with_frame(r, |x| x.props.tf.h_over = v as u8)?;
                Ok(Variant::Empty)
            }
            ("VerticalOverflow", false) => Ok(Variant::I4(i32::from(tf.v_over))),
            ("VerticalOverflow", true) => {
                let v = put_i32(a)?;
                if !(0..=2).contains(&v) {
                    return Err(e_fail());
                }
                self.with_frame(r, |x| x.props.tf.v_over = v as u8)?;
                Ok(Variant::Empty)
            }
            ("ReadingOrder", false) => Ok(Variant::I4(match tf.reading {
                1 => -5003,
                2 => -5004,
                _ => -2,
            })),
            ("ReadingOrder", true) => {
                let rd = match put_i32(a)? {
                    -5003 | 2 => 1,
                    -5004 => 2,
                    _ => return Err(OmError::app_defined()),
                };
                self.with_frame(r, |x| x.props.tf.reading = rd)?;
                Ok(Variant::Empty)
            }
            _ => Err(unsupported("TextFrame", m)),
        }
    }

    fn margin(
        &mut self,
        r: &DrawRef,
        name: &str,
        put: bool,
        a: &Args,
        tf: &TextFrame,
    ) -> Result<Variant, OmError> {
        let i = match name {
            "MarginLeft" => 0,
            "MarginTop" => 1,
            "MarginRight" => 2,
            _ => 3,
        };
        if !put {
            return Ok(Variant::R4(tf.margins[i]));
        }
        let v = put_f64(a)?;
        if v < 0.0 {
            return Err(invalid_arg());
        }

        let emu = (v as f32) * 12700.0_f32;
        self.with_frame(r, |x| x.props.tf.margins[i] = emu / 12700.0_f32)?;
        Ok(Variant::Empty)
    }

    fn put_orientation(&mut self, r: &DrawRef, a: &Args) -> Result<Variant, OmError> {
        let v = put_i32(a)?;
        if !(1..=6).contains(&v) {
            return Err(invalid_arg());
        }
        self.with_frame(r, |x| x.props.tf.orient = v as u8)?;
        Ok(Variant::Empty)
    }

    fn frame2_member(
        &mut self,
        r: &DrawRef,
        m: &'static Member,
        a: &Args,
        sh: &Shape,
    ) -> Result<Variant, OmError> {
        let put = m.kind == InvokeKind::Put || m.kind == InvokeKind::PutRef;
        let tf = &sh.props.tf;
        match (m.name, put) {
            ("AutoSize", false) => Ok(Variant::I4(i32::from(tf.autosize))),
            ("AutoSize", true) => {
                let v = put_i32(a)?;
                if !(0..=2).contains(&v) {
                    return Err(invalid_arg());
                }
                self.with_frame(r, |x| x.props.tf.autosize = v as u8)?;
                Ok(Variant::Empty)
            }
            ("WordWrap", false) => Ok(tri(tf.wrap)),
            ("WordWrap", true) => {
                let on = put_bool(a)?;
                self.with_frame(r, |x| x.props.tf.wrap = on)?;
                Ok(Variant::Empty)
            }
            ("HorizontalAnchor", false) => Ok(Variant::I4(if tf.anchor_ctr { 2 } else { 1 })),
            ("HorizontalAnchor", true) => {
                let v = put_i32(a)?;
                if !(1..=2).contains(&v) {
                    return Err(invalid_arg());
                }
                self.with_frame(r, |x| x.props.tf.anchor_ctr = v == 2)?;
                Ok(Variant::Empty)
            }
            ("VerticalAnchor", false) => Ok(Variant::I4(i32::from(tf.anchor))),
            ("VerticalAnchor", true) => {
                let v = put_i32(a)?;
                if !matches!(v, 1 | 3 | 4) {
                    return Err(invalid_arg());
                }
                self.with_frame(r, |x| x.props.tf.anchor = v as u8)?;
                Ok(Variant::Empty)
            }
            ("MarginLeft" | "MarginTop" | "MarginRight" | "MarginBottom", _) => {
                self.margin(r, m.name, put, a, tf)
            }
            ("Orientation", false) => Ok(Variant::I4(i32::from(tf.orient))),
            ("Orientation", true) => self.put_orientation(r, a),
            ("NoTextRotation", false) => Ok(tri(tf.upright)),
            ("NoTextRotation", true) => {
                let on = put_bool(a)?;
                self.with_frame(r, |x| x.props.tf.upright = on)?;
                Ok(Variant::Empty)
            }
            _ => match self.bag_member(Store::Shape(r), "TextFrame2", "TextFrame2", m, a) {
                Some(res) => res,
                None => Err(unsupported("TextFrame2", m)),
            },
        }
    }

    fn font1_member(
        &mut self,
        r: &DrawRef,
        m: &'static Member,
        a: &Args,
        sh: &Shape,
        (rs, rl): (u32, u32),
    ) -> Result<Variant, OmError> {
        let put = m.kind == InvokeKind::Put || m.kind == InvokeKind::PutRef;
        let text_len = utf16_len(&sh.text);
        let vals = sh
            .props
            .tf
            .range(text_len, rs as usize, span_len(text_len, rs, rl));
        let first = vals.first().map_or_else(CharFmt::default, |c| (*c).clone());
        let mixed_color = vals.iter().any(|c| c.color != first.color);
        let all = |f: &dyn Fn(&CharFmt) -> bool| vals.iter().all(|c| f(c));
        let set = |app: &mut Self, f: &dyn Fn(&mut CharFmt)| -> Result<Variant, OmError> {
            app.modify_chars(r, text_len, rs, rl, f)?;
            Ok(Variant::Empty)
        };
        match (m.name, put) {
            ("Name", false) => Ok(Variant::Bstr(
                if vals.iter().any(|c| c.name != first.name) {
                    String::new()
                } else {
                    first.name.clone().unwrap_or_else(|| DEFAULT_FONT.into())
                },
            )),
            ("Name", true) => {
                let t = crate::controls::put_text(a)?;
                set(self, &|c| c.name = Some(t.clone()))
            }
            ("Size", false) => Ok(Variant::R8(f64::from(first.size))),
            ("Size", true) => {
                let v = put_f64(a)?;
                if (1.0..=410.0).contains(&v) {
                    set(self, &|c| c.size = v as f32)
                } else {
                    Ok(Variant::Empty)
                }
            }
            ("Bold", false) => Ok(Variant::Bool(all(&|c| c.bold))),
            ("Bold", true) => {
                let v = put_bool(a)?;
                set(self, &|c| c.bold = v)
            }
            ("Italic", false) => Ok(Variant::Bool(all(&|c| c.italic))),
            ("Italic", true) => {
                let v = put_bool(a)?;
                set(self, &|c| c.italic = v)
            }
            ("Color", false) => Ok(Variant::I4(if mixed_color {
                i32::MIN
            } else {
                color_of(sh, first.color) as i32
            })),
            ("Color", true) => {
                let v = put_f64(a)? as i64 as u32 & 0xFF_FFFF;
                set(self, &|c| c.color = FontColor::Rgb(v))
            }
            ("ColorIndex", false) if mixed_color => Err(e_fail()),
            ("ColorIndex", false) => Ok(Variant::I4(color_index(sh, first.color))),
            ("ColorIndex", true) => {
                let c = match put_i32(a)? {
                    0 | -4105 => FontColor::Default,
                    -4142 => FontColor::NoFill,
                    i @ 1..=56 => {
                        let rgb = PALETTE[(i - 1) as usize];
                        FontColor::Rgb(swap_rb(rgb))
                    }
                    _ => return Err(OmError::bad_index()),
                };
                set(self, &|f| f.color = c)
            }
            ("FontStyle", false) => Ok(Variant::Bstr(
                match (all(&|c| c.bold), all(&|c| c.italic)) {
                    (false, false) => "Regular",
                    (true, false) => "Bold",
                    (false, true) => "Italic",
                    (true, true) => "Bold Italic",
                }
                .into(),
            )),
            ("FontStyle", true) => {
                let t = crate::controls::put_text(a)?.to_lowercase();
                let (b, i) = (t.contains("bold"), t.contains("italic"));
                set(self, &|c| {
                    c.bold = b;
                    c.italic = i;
                })
            }
            ("Underline", false) => Ok(Variant::I4(underline_excel(first.underline))),
            ("Underline", true) => {
                let u = match put_i32(a)? {
                    -4142 | 1 => 0,
                    2 => 1,
                    -4119 | 3 => 3,
                    _ => return Err(OmError::app_defined()),
                };
                set(self, &|c| c.underline = u)
            }
            ("Strikethrough", false) => Ok(Variant::Bool(first.strike == 1)),
            ("Strikethrough", true) => {
                let v = u8::from(put_bool(a)?);
                set(self, &|c| c.strike = v)
            }
            ("Superscript", false) => Ok(Variant::Bool(first.baseline > 0.0)),
            ("Superscript", true) => {
                let v = if put_bool(a)? { 0.3 } else { 0.0 };
                set(self, &|c| c.baseline = v)
            }
            ("Subscript", false) => Ok(Variant::Bool(first.baseline < 0.0)),
            ("Subscript", true) => {
                let v = if put_bool(a)? { -0.25 } else { 0.0 };
                set(self, &|c| c.baseline = v)
            }
            ("OutlineFont", false) => Ok(Variant::I4(19)),
            ("Shadow", false) => Ok(Variant::Bool(false)),
            _ => Err(unsupported("Font", m)),
        }
    }

    fn font2_member(
        &mut self,
        r: &DrawRef,
        m: &'static Member,
        a: &Args,
        sh: &Shape,
        (rs, rl): (u32, u32),
    ) -> Result<Variant, OmError> {
        let put = m.kind == InvokeKind::Put || m.kind == InvokeKind::PutRef;
        let text_len = utf16_len(&sh.text);
        let vals = sh
            .props
            .tf
            .range(text_len, rs as usize, span_len(text_len, rs, rl));
        let first = vals.first().map_or_else(CharFmt::default, |c| (*c).clone());
        let set = |app: &mut Self, f: &dyn Fn(&mut CharFmt)| -> Result<Variant, OmError> {
            app.modify_chars(r, text_len, rs, rl, f)?;
            Ok(Variant::Empty)
        };
        let mixed = |f: &dyn Fn(&CharFmt) -> i32| -> Variant { Variant::I4(f(&first)) };
        match (m.name, put) {
            ("Name", false) if vals.iter().any(|c| c.name != first.name) => Ok(Variant::BstrNull),
            ("Name", false) => Ok(Variant::Bstr(
                first.name.clone().unwrap_or_else(|| DEFAULT_FONT.into()),
            )),
            ("Name", true) => {
                let t = crate::controls::put_text(a)?;
                set(self, &|c| c.name = Some(t.clone()))
            }
            ("Size", false) => Ok(Variant::R4(first.size)),
            ("Size", true) => {
                let v = put_f64(a)?;
                if v <= 0.0 {
                    return Err(invalid_arg());
                }
                if !(1.0..=410.0).contains(&v) {
                    return Err(e_fail());
                }
                set(self, &|c| c.size = v as f32)
            }
            ("Bold", false) => Ok(Variant::I4(tri_of(&vals, |c| c.bold))),
            ("Bold", true) => {
                let v = put_bool(a)?;
                set(self, &|c| c.bold = v)
            }
            ("Italic", false) => Ok(Variant::I4(tri_of(&vals, |c| c.italic))),
            ("Italic", true) => {
                let v = put_bool(a)?;
                set(self, &|c| c.italic = v)
            }
            ("Strike", false) => Ok(mixed(&|c| i32::from(c.strike))),
            ("Strike", true) => {
                let v = put_i32(a)?;
                if !(0..=2).contains(&v) {
                    return Err(invalid_arg());
                }
                set(self, &|c| c.strike = v as u8)
            }
            ("StrikeThrough", false) => Ok(Variant::I4(tri_of(&vals, |c| c.strike == 1))),
            ("DoubleStrikeThrough", false) => Ok(Variant::I4(tri_of(&vals, |c| c.strike == 2))),
            ("Allcaps", false) => Ok(Variant::I4(tri_of(&vals, |c| c.caps == 1))),
            ("Allcaps", true) => {
                let on = put_bool(a)?;
                set(self, &|c| {
                    c.caps = if on {
                        1
                    } else if c.caps == 1 {
                        0
                    } else {
                        c.caps
                    }
                })
            }
            ("Smallcaps", false) => Ok(Variant::I4(tri_of(&vals, |c| c.caps == 2))),
            ("Smallcaps", true) => {
                let on = put_bool(a)?;
                set(self, &|c| {
                    c.caps = if on {
                        2
                    } else if c.caps == 2 {
                        0
                    } else {
                        c.caps
                    }
                })
            }
            ("Spacing", false) => Ok(Variant::R4(first.spacing)),
            ("Spacing", true) => {
                let v = put_f64(a)? as f32;
                set(self, &|c| c.spacing = v)
            }
            ("Kerning", false) => Ok(Variant::R4(first.kerning)),
            ("Kerning", true) => {
                let v = put_f64(a)? as f32;
                set(self, &|c| c.kerning = v)
            }
            ("UnderlineStyle", false) => Ok(mixed(&|c| i32::from(c.underline))),
            ("UnderlineStyle", true) => {
                let v = put_i32(a)?;
                if !(0..=17).contains(&v) {
                    return Err(invalid_arg());
                }
                set(self, &|c| c.underline = v as u8)
            }
            ("BaselineOffset", false) => Ok(Variant::R4(first.baseline)),
            ("BaselineOffset", true) => {
                let v = put_f64(a)? as f32;
                set(self, &|c| c.baseline = v)
            }
            ("Superscript", false) => Ok(Variant::I4(tri_of(&vals, |c| c.baseline > 0.0))),
            ("Subscript", false) => Ok(Variant::I4(tri_of(&vals, |c| c.baseline < 0.0))),
            _ => Err(unsupported("Font2", m)),
        }
    }
}

fn span_len(text_len: usize, start: u32, len: u32) -> usize {
    let (s, e) = span(text_len, start, len);
    e - s
}
