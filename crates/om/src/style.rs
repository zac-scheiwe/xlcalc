use std::sync::Arc;

use xlcalc::style::{
    nearest_color_index, stored_tint, Border, BorderLine, CellColor, Edge, Fill, FontScheme,
    HAlign, NamedStyle, Pattern, Style, Theme, Underline, VAlign,
};
use xlcalc::{CellAddr, Rect, SheetId, Workbook};

use crate::addr::{MAX_COLS, MAX_ROWS};
use xlcalc_variant::{HResult, Variant};

use crate::app::{unsupported, Application, BookId, Object, RangeRef};
use crate::conv::Args;
use crate::error::{OmError, SCODE_BAD_INDEX};
use crate::typelib::{InvokeKind, Member};

#[derive(Clone, Debug)]
pub(crate) enum StyleOwner {
    Range(RangeRef),
    Named(BookId, Arc<str>),

    Display(RangeRef),

    Cond(BookId, SheetId, u32),
}

const NONE: i32 = -4142;
const AUTOMATIC: i32 = -4105;
const WHITE: u32 = 0xFFFFFF;

fn swap_rb(c: u32) -> u32 {
    ((c & 0xFF) << 16) | (c & 0xFF00) | ((c >> 16) & 0xFF)
}

fn color_variant(rgb: u32) -> Variant {
    Variant::R8(f64::from(swap_rb(rgb)))
}

fn invalid_arg() -> OmError {
    OmError::exception(HResult::E_INVALIDARG.0, None)
}

fn tint_arg(a: &Args) -> Result<f64, OmError> {
    let t = match a.put {
        Some(Variant::R8(x)) => x,
        Some(Variant::R4(x)) => f64::from(x),
        Some(Variant::I4(x)) => f64::from(x),
        Some(Variant::I2(x)) => f64::from(x),
        Some(Variant::Ui1(x)) => f64::from(x),
        _ => return Err(invalid_arg()),
    };
    if (-1.0..=1.0).contains(&t) {
        Ok(t)
    } else {
        Err(invalid_arg())
    }
}

fn i4(n: i32) -> Variant {
    Variant::I4(n)
}

fn or_null<T>(v: Option<T>, f: impl FnOnce(T) -> Variant) -> Variant {
    v.map_or(Variant::Null, f)
}

fn color_from_index(n: i32) -> Result<CellColor, OmError> {
    match n {
        AUTOMATIC => Ok(CellColor::Auto),
        1..=56 => Ok(CellColor::Indexed(n as u8 + 7)),
        _ => Err(OmError::app_defined()),
    }
}

fn index_of(theme: &Theme, c: CellColor, auto: u32) -> i32 {
    match c {
        CellColor::Auto => AUTOMATIC,
        CellColor::Indexed(i) if (8..64).contains(&i) => i32::from(i) - 7,
        c => i32::from(nearest_color_index(theme.rgb(c, auto))),
    }
}

fn theme_index(c: CellColor) -> Option<i32> {
    match c {
        CellColor::Theme { index, .. } => Some(i32::from(index) + 1),
        _ => None,
    }
}

fn theme_color(n: i32, tint: f64) -> Result<CellColor, OmError> {
    match n {
        1..=12 => Ok(CellColor::theme(n as u8 - 1, tint)),
        _ => Err(OmError::app_defined()),
    }
}

fn with_tint(c: CellColor, t: f64) -> CellColor {
    match c {
        CellColor::Theme { index, .. } => CellColor::theme(index, stored_tint(t)),
        c => c,
    }
}

const PATTERNS: [i32; 19] = [
    NONE, 1, -4125, -4126, -4124, -4128, -4166, -4121, -4162, 9, 10, 11, 12, 13, 14, 15, 16, 17, 18,
];

fn pattern_const(p: Pattern) -> i32 {
    PATTERNS[p as usize]
}

fn pattern_from(n: i32) -> Option<Pattern> {
    if n == AUTOMATIC || n == -1 {
        return Some(Pattern::Solid);
    }

    if (0..PATTERNS.len() as i32).contains(&n) {
        return Some(Pattern::ALL[n as usize]);
    }
    PATTERNS
        .iter()
        .position(|&c| c == n)
        .map(|i| Pattern::ALL[i])
}

const H_ALIGN: [i32; 8] = [1, -4131, -4108, -4152, 5, -4130, 7, -4117];
const V_ALIGN: [i32; 5] = [-4160, -4108, -4107, -4130, -4117];

fn underline_const(u: Underline) -> i32 {
    match u {
        Underline::None => NONE,
        Underline::Single => 2,
        Underline::Double => -4119,
        Underline::SingleAccounting => 4,
        Underline::DoubleAccounting => 5,
    }
}

fn underline_from(n: i32) -> Option<Underline> {
    Some(match n {
        NONE => Underline::None,
        2 => Underline::Single,
        -4119 => Underline::Double,
        4 => Underline::SingleAccounting,
        5 => Underline::DoubleAccounting,
        _ => return None,
    })
}

fn orientation_const(rotation: u8) -> i32 {
    match rotation {
        0 => -4128,
        90 => -4171,
        180 => -4170,
        255 => -4166,
        r @ 1..=89 => i32::from(r),
        r => 90 - i32::from(r),
    }
}

fn rotation_from(n: i32) -> Option<u8> {
    Some(match n {
        -4128 => 0,
        -4171 => 90,
        -4170 => 180,
        -4166 | 255 => 255,
        0..=90 => n as u8,
        -90..=-1 => (90 - n) as u8,
        _ => return None,
    })
}

const CONTINUOUS: i32 = 1;
const DASH: i32 = -4115;
const DASH_DOT: i32 = 4;
const DASH_DOT_DOT: i32 = 5;
const DOT: i32 = -4118;
const DOUBLE: i32 = -4119;
const SLANT: i32 = 13;
const HAIR: i32 = 1;
const THIN: i32 = 2;
const MEDIUM: i32 = -4138;
const THICK: i32 = 4;

fn line_parts(l: BorderLine) -> (i32, i32) {
    use BorderLine::*;
    match l {
        None => (NONE, THIN),
        Thin => (CONTINUOUS, THIN),
        Medium => (CONTINUOUS, MEDIUM),
        Dashed => (DASH, THIN),
        Dotted => (DOT, THIN),
        Thick => (CONTINUOUS, THICK),
        Double => (DOUBLE, THICK),
        Hair => (CONTINUOUS, HAIR),
        MediumDashed => (DASH, MEDIUM),
        DashDot => (DASH_DOT, THIN),
        MediumDashDot => (DASH_DOT, MEDIUM),
        DashDotDot => (DASH_DOT_DOT, THIN),
        MediumDashDotDot => (DASH_DOT_DOT, MEDIUM),
        SlantDashDot => (SLANT, MEDIUM),
    }
}

fn line_from(style: i32, weight: i32) -> Option<BorderLine> {
    use BorderLine::*;
    let plain = || {
        Some(match weight {
            HAIR => Hair,
            THIN => Thin,
            MEDIUM => Medium,
            THICK => Thick,
            _ => return Option::None,
        })
    };
    match (style, weight) {
        (NONE, _) => Some(None),
        (DOUBLE, _) => Some(Double),
        (SLANT, MEDIUM) => Some(SlantDashDot),
        (DASH, THIN) => Some(Dashed),
        (DASH, MEDIUM) => Some(MediumDashed),
        (DASH_DOT, THIN) => Some(DashDot),
        (DASH_DOT, MEDIUM) => Some(MediumDashDot),
        (DASH_DOT_DOT, THIN) => Some(DashDotDot),
        (DASH_DOT_DOT, MEDIUM) => Some(MediumDashDotDot),
        (DOT, THIN) => Some(Dotted),
        (CONTINUOUS | DASH | DASH_DOT | DASH_DOT_DOT | DOT | SLANT, _) => plain(),
        _ => Option::None,
    }
}

fn agree<T: PartialEq>(mut it: impl Iterator<Item = T>) -> Option<T> {
    let first = it.next()?;
    for v in it {
        if v != first {
            return None;
        }
    }
    Some(first)
}

#[derive(Clone, Copy, PartialEq, Eq, Debug)]
enum Side {
    Edge(Edge),
    DiagonalDown,
    DiagonalUp,
}

fn border_cells(rect: Rect, index: i32) -> Result<Option<(Rect, Side)>, OmError> {
    let (r0, c0, r1, c1) = (rect.r0, rect.c0, rect.r1, rect.c1);
    Ok(match index {
        7 | -4131 => Some((Rect::new(r0, c0, r1, c0), Side::Edge(Edge::Left))),
        8 | -4160 => Some((Rect::new(r0, c0, r0, c1), Side::Edge(Edge::Top))),
        9 | -4107 => Some((Rect::new(r1, c0, r1, c1), Side::Edge(Edge::Bottom))),
        10 | -4152 => Some((Rect::new(r0, c1, r1, c1), Side::Edge(Edge::Right))),
        11 => (c1 > c0).then(|| (Rect::new(r0, c0, r1, c1 - 1), Side::Edge(Edge::Right))),
        12 => (r1 > r0).then(|| (Rect::new(r0, c0, r1 - 1, c1), Side::Edge(Edge::Bottom))),
        5 => Some((rect, Side::DiagonalDown)),
        6 => Some((rect, Side::DiagonalUp)),
        _ => return Err(OmError::app_defined()),
    })
}

fn side_of(s: &Style, side: Side) -> Border {
    match side {
        Side::Edge(e) => s.borders.edge(e),
        Side::DiagonalDown if s.borders.diagonal_down => s.borders.diagonal,
        Side::DiagonalUp if s.borders.diagonal_up => s.borders.diagonal,

        Side::DiagonalDown | Side::DiagonalUp
            if !s.borders.diagonal_down && !s.borders.diagonal_up =>
        {
            Border {
                line: s.borders.diagonal.line,
                color: CellColor::Auto,
            }
        }
        _ => Border::default(),
    }
}

fn shown_side(wb: &Workbook, a: CellAddr, side: Side) -> Border {
    match side {
        Side::Edge(e) => wb.shown_border(a, e),
        _ => side_of(wb.cell_style(a), side),
    }
}

fn cells(sheet: SheetId, rect: Rect) -> impl Iterator<Item = CellAddr> {
    (rect.r0..=rect.r1)
        .flat_map(move |r| (rect.c0..=rect.c1).map(move |c| CellAddr::new(sheet, r, c)))
}

fn border_prop(theme: &Theme, b: Option<Border>, prop: &str) -> Result<Variant, OmError> {
    let Some(b) = b else {
        return Ok(Variant::Null);
    };
    let none = b.line == BorderLine::None;
    let (style, weight) = line_parts(b.line);
    Ok(match prop {
        "LineStyle" => i4(style),
        "Weight" => i4(weight),
        "Color" => color_variant(theme.rgb(b.color, 0)),
        "ColorIndex" if none => i4(NONE),
        "ColorIndex" => i4(index_of(theme, b.color, 0)),
        "ThemeColor" | "TintAndShade" if none => Variant::Null,
        "ThemeColor" => i4(theme_index(b.color).ok_or_else(invalid_arg)?),
        "TintAndShade" => Variant::R8(b.color.tint()),
        _ => return Err(OmError::app_defined()),
    })
}

struct FontFields {
    name: Option<Arc<str>>,
    size: Option<u32>,
    bold: Option<bool>,
    italic: Option<bool>,
    underline: Option<Underline>,
    strike: Option<bool>,
    sup: Option<bool>,
    sub: Option<bool>,
    color: Option<CellColor>,
    scheme: Option<FontScheme>,
}

fn font_prop(theme: &Theme, f: &FontFields, m: &Member) -> Result<Variant, OmError> {
    let b = |v: Option<bool>| or_null(v, Variant::Bool);
    Ok(match m.name {
        "Name" => or_null(f.name.clone(), |n| Variant::Bstr(n.to_string())),
        "Size" => or_null(f.size, |t| Variant::R8(f64::from(t) / 20.0)),
        "Bold" => b(f.bold),
        "Italic" => b(f.italic),
        "Strikethrough" => b(f.strike),
        "Superscript" => b(f.sup),
        "Subscript" => b(f.sub),
        "OutlineFont" | "Shadow" => Variant::Bool(false),
        "Underline" => or_null(f.underline, |u| i4(underline_const(u))),
        "Color" => or_null(f.color, |c| color_variant(theme.rgb(c, 0))),
        "ColorIndex" => or_null(f.color, |c| i4(index_of(theme, c, 0))),
        "ThemeColor" => match f.color {
            None => Variant::Null,
            Some(c) => i4(theme_index(c).ok_or_else(invalid_arg)?),
        },
        "TintAndShade" => or_null(f.color, |c| Variant::R8(c.tint())),
        "FontStyle" => match (f.bold, f.italic) {
            (Some(b), Some(i)) => Variant::Bstr(
                match (b, i) {
                    (false, false) => "Regular",
                    (true, false) => "Bold",
                    (false, true) => "Italic",
                    (true, true) => "Bold Italic",
                }
                .into(),
            ),
            _ => Variant::Null,
        },
        "ThemeFont" => i4(match f.scheme {
            Some(FontScheme::Major) => 1,
            Some(FontScheme::Minor) => 2,
            _ => 0,
        }),
        _ => return Err(unsupported("Font", m)),
    })
}

struct FillFields {
    pattern: Option<Pattern>,
    color: Option<CellColor>,
    pattern_color: Option<CellColor>,
}

fn interior_prop(theme: &Theme, f: &FillFields, m: &Member) -> Result<Variant, OmError> {
    let no_fill = f.pattern == Some(Pattern::None);
    Ok(match m.name {
        "Pattern" => or_null(f.pattern, |p| i4(pattern_const(p))),
        "Color" if no_fill => color_variant(WHITE),
        "Color" => f
            .color
            .map_or(Variant::R8(0.0), |c| color_variant(theme.rgb(c, WHITE))),
        "ColorIndex" | "ThemeColor" | "PatternColorIndex" | "PatternThemeColor" if no_fill => {
            i4(NONE)
        }
        "ColorIndex" => or_null(f.color, |c| i4(index_of(theme, c, WHITE))),
        "ThemeColor" => or_null(f.color, |c| i4(theme_index(c).unwrap_or(0))),
        "TintAndShade" => or_null(f.color, |c| Variant::R8(c.tint())),
        "PatternColor" => or_null(f.pattern_color, |c| i4(swap_rb(theme.rgb(c, 0)) as i32)),
        "PatternColorIndex" => or_null(f.pattern_color, |c| i4(index_of(theme, c, 0))),
        "PatternThemeColor" => or_null(f.pattern_color, |c| i4(theme_index(c).unwrap_or(0))),
        "PatternTintAndShade" => or_null(f.pattern_color, |c| Variant::R8(c.tint())),
        _ => return Err(unsupported("Interior", m)),
    })
}

type Field<'a> = &'a dyn Fn(&Style) -> Variant;

fn format_prop(get: &dyn Fn(Field) -> Variant, prop: &str) -> Option<Variant> {
    Some(match prop {
        "HorizontalAlignment" => get(&|s| i4(H_ALIGN[s.alignment.horizontal as usize])),
        "VerticalAlignment" => get(&|s| i4(V_ALIGN[s.alignment.vertical as usize])),
        "WrapText" => get(&|s| Variant::Bool(s.alignment.wrap)),
        "ShrinkToFit" => get(&|s| Variant::Bool(s.alignment.shrink)),
        "IndentLevel" => get(&|s| i4(i32::from(s.alignment.indent))),
        "Orientation" => get(&|s| i4(orientation_const(s.alignment.rotation))),
        "AddIndent" => get(&|s| Variant::Bool(s.alignment.add_indent)),
        "ReadingOrder" => get(&|s| i4(-5002 - i32::from(s.alignment.reading_order))),
        "Locked" => get(&|s| Variant::Bool(s.locked)),
        "FormulaHidden" => get(&|s| Variant::Bool(s.formula_hidden)),
        _ => return None,
    })
}

fn format_put(prop: &str, v: &Args, app: &Application) -> Result<Option<StyleEdit>, OmError> {
    let n = || app.put_i32(v);
    let b = || app.put_bool(v);
    Ok(Some(match prop {
        "HorizontalAlignment" => {
            let i = H_ALIGN
                .iter()
                .position(|&c| c == enum_arg(v, n().unwrap_or(0)))
                .ok_or_else(OmError::app_defined)?;
            let h = HAlign::ALL[i];
            Box::new(move |s: &mut Style| s.alignment.horizontal = h)
        }
        "VerticalAlignment" => {
            let x = enum_arg(v, n().unwrap_or(0));
            let i = V_ALIGN
                .iter()
                .position(|&c| c == x)
                .or_else(|| usize::try_from(x - 1).ok().filter(|&i| i < V_ALIGN.len()))
                .ok_or_else(OmError::app_defined)?;
            let va = VAlign::ALL[i];
            Box::new(move |s: &mut Style| s.alignment.vertical = va)
        }
        "WrapText" => {
            let x = b()?;
            Box::new(move |s: &mut Style| s.alignment.wrap = x)
        }
        "ShrinkToFit" => {
            let x = b()?;
            Box::new(move |s: &mut Style| s.alignment.shrink = x)
        }
        "Locked" => {
            let x = b()?;
            Box::new(move |s: &mut Style| s.locked = x)
        }
        "FormulaHidden" => {
            let x = b()?;
            Box::new(move |s: &mut Style| s.formula_hidden = x)
        }
        "IndentLevel" => {
            let x = match &v.put {
                Some(Variant::Null | Variant::Empty) => 0,
                _ => enum_arg(v, n()?),
            };
            let x = u8::try_from(x)
                .ok()
                .filter(|&x| x <= 250)
                .ok_or_else(OmError::app_defined)?;

            Box::new(move |s: &mut Style| {
                s.alignment.indent = x;
                if x > 0
                    && !matches!(
                        s.alignment.horizontal,
                        HAlign::Left | HAlign::Right | HAlign::Distributed
                    )
                {
                    s.alignment.horizontal = HAlign::Left;
                }
            })
        }
        "AddIndent" => {
            let x = b()?;
            Box::new(move |s: &mut Style| s.alignment.add_indent = x)
        }
        "ReadingOrder" => {
            let r = match enum_arg(v, n()?) {
                -5002 | 1 => 0u8,
                -5003 | 2 => 1,
                -5004 | 3 => 2,
                _ => return Err(OmError::app_defined()),
            };
            Box::new(move |s: &mut Style| s.alignment.reading_order = r)
        }
        "Orientation" => {
            let x = match &v.put {
                Some(Variant::Null | Variant::Empty) => 0,
                _ => enum_arg(v, n()?),
            };
            let r = rotation_from(x).ok_or_else(OmError::app_defined)?;
            Box::new(move |s: &mut Style| s.alignment.rotation = r)
        }
        _ => return Ok(None),
    }))
}

fn enum_arg(v: &Args, n: i32) -> i32 {
    match v.put {
        Some(Variant::Bool(x)) => i32::from(x),
        Some(Variant::R8(f)) if f.abs() < 2e9 => f as i32,
        Some(Variant::R4(f)) if f.abs() < 2e9 => f64::from(f) as i32,
        _ => n,
    }
}

type StyleEdit = Box<dyn Fn(&mut Style)>;

fn bgr_arg(app: &Application, a: &Args) -> Result<u32, OmError> {
    let x = app.put_f64(a)?.round_ties_even();
    if !(0.0..=16_777_215.0).contains(&x) {
        return Err(OmError::app_defined());
    }
    Ok(swap_rb(x as u32))
}

fn font_put(app: &Application, prop: &str, a: &Args, theme: &Theme) -> Result<StyleEdit, OmError> {
    let n = || app.put_i32(a);

    let flag = || match a.put {
        Some(Variant::Bstr(_)) => Err(OmError::unable_to_set(prop, "Font")),
        _ => app.put_bool(a),
    };
    Ok(match prop {
        "Name" => {
            let text = match &a.put {
                Some(Variant::Bool(x)) => (if *x { "TRUE" } else { "FALSE" }).to_string(),
                None | Some(Variant::Empty | Variant::Null) => {
                    return Ok(Box::new(|_: &mut Style| {}))
                }
                Some(v @ (Variant::I2(_) | Variant::I4(_) | Variant::R4(_) | Variant::R8(_))) => {
                    match xlcalc_variant::change_type(
                        v,
                        xlcalc_variant::VarType::Bstr,
                        crate::conv::variant_locale(a.lcid),
                        0,
                    ) {
                        Ok(Variant::Bstr(t)) => t,
                        _ => return Err(OmError::type_mismatch()),
                    }
                }
                _ => app.put_string(a)?,
            };
            let name: Arc<str> = Arc::from(text);
            Box::new(move |s: &mut Style| {
                s.font.name = name.clone();
                s.font.scheme = FontScheme::None;
            })
        }
        "Size" => {
            match a.put {
                Some(Variant::Bstr(_) | Variant::Empty | Variant::Null) | None => {
                    return Err(OmError::unable_to_set(prop, "Font"))
                }

                Some(Variant::Bool(_)) => return Ok(Box::new(|_: &mut Style| {})),
                _ => {}
            }
            let x = app.put_f64(a)?;
            if !(1.0..=409.0).contains(&x) {
                return Err(OmError::app_defined());
            }

            let t = (x * 20.0).round() as u32;
            Box::new(move |s: &mut Style| s.font.size_twips = t)
        }
        "Bold" => {
            let x = flag()?;
            Box::new(move |s: &mut Style| s.font.bold = x)
        }
        "Italic" => {
            let x = flag()?;
            Box::new(move |s: &mut Style| s.font.italic = x)
        }
        "Strikethrough" => {
            let x = flag()?;
            Box::new(move |s: &mut Style| s.font.strikethrough = x)
        }
        "Superscript" => {
            let x = flag()?;
            Box::new(move |s: &mut Style| {
                s.font.superscript = x;
                if x {
                    s.font.subscript = false;
                }
            })
        }
        "Subscript" => {
            let x = flag()?;
            Box::new(move |s: &mut Style| {
                s.font.subscript = x;
                if x {
                    s.font.superscript = false;
                }
            })
        }
        "Underline" => {
            let u = match &a.put {
                Some(Variant::Bool(true)) => Underline::Single,
                Some(Variant::Bool(false)) => Underline::None,
                Some(Variant::Bstr(_) | Variant::Empty | Variant::Null) | None => {
                    return Err(OmError::unable_to_set(prop, "Font"))
                }
                Some(Variant::R8(x)) => underline_from(x.trunc() as i32).unwrap_or(Underline::None),
                _ => underline_from(n()?).unwrap_or(Underline::None),
            };
            Box::new(move |s: &mut Style| s.font.underline = u)
        }
        "FontStyle" => {
            let t = app.put_string(a)?.to_lowercase();
            let (bold, italic) = (t.contains("bold"), t.contains("italic"));
            Box::new(move |s: &mut Style| {
                s.font.bold = bold;
                s.font.italic = italic;
            })
        }
        "Color" => {
            let c = CellColor::Rgb(bgr_arg(app, a)?);
            Box::new(move |s: &mut Style| s.font.color = c)
        }
        "ColorIndex" => {
            let c = match &a.put {
                Some(Variant::Bool(_)) => return Ok(Box::new(|_: &mut Style| {})),
                None | Some(Variant::Empty | Variant::Null) => CellColor::Auto,
                _ => color_from_index(n()?)?,
            };
            Box::new(move |s: &mut Style| s.font.color = c)
        }
        "ThemeColor" => {
            if matches!(
                a.put,
                None | Some(Variant::Bool(_) | Variant::Empty | Variant::Null)
            ) {
                return Err(OmError::bad_index());
            }
            let c = theme_color(n()?, 0.0)?;
            Box::new(move |s: &mut Style| s.font.color = c)
        }
        "TintAndShade" => {
            let t = tint_arg(a)?;
            Box::new(move |s: &mut Style| s.font.color = with_tint(s.font.color, t))
        }
        "ThemeFont" => {
            let (scheme, name) = match n()? {
                0 => (FontScheme::None, None),
                1 => (FontScheme::Major, Some(theme.major_font.clone())),
                2 => (FontScheme::Minor, Some(theme.minor_font.clone())),
                _ => return Err(OmError::app_defined()),
            };
            Box::new(move |s: &mut Style| {
                s.font.scheme = scheme;
                if let Some(n) = &name {
                    s.font.name = n.clone();
                }
            })
        }
        _ => return Err(OmError::app_defined()),
    })
}

fn solidify(s: &mut Style) {
    if s.fill.pattern == Pattern::None {
        s.fill.pattern = Pattern::Solid;
    }
    s.fill.cleared = false;
}

fn interior_put(app: &Application, prop: &str, a: &Args) -> Result<StyleEdit, OmError> {
    let n = || app.put_i32(a);

    let colour = |c: CellColor| -> StyleEdit {
        Box::new(move |s: &mut Style| {
            solidify(s);
            s.fill.color = c;
        })
    };

    let pattern_colour = |c: CellColor| -> StyleEdit {
        Box::new(move |s: &mut Style| {
            solidify(s);
            s.fill.pattern_color = c;
        })
    };

    let ignored = matches!(a.put, Some(Variant::Bool(_)));
    let nothing = matches!(a.put, None | Some(Variant::Empty | Variant::Null));
    let noop = || -> StyleEdit { Box::new(|_: &mut Style| {}) };
    if ignored
        && matches!(
            prop,
            "Color" | "PatternColor" | "ThemeColor" | "PatternThemeColor"
        )
    {
        return Ok(noop());
    }
    if nothing && matches!(prop, "ThemeColor" | "PatternThemeColor") {
        return Err(OmError::bad_index());
    }
    if ignored && prop == "ColorIndex" {
        return Ok(colour(CellColor::Auto));
    }
    if ignored && prop == "PatternColorIndex" {
        return Ok(pattern_colour(CellColor::Auto));
    }
    Ok(match prop {
        "Color" => colour(CellColor::Rgb(bgr_arg(app, a)?)),
        "ColorIndex" if nothing => Box::new(|s: &mut Style| s.fill = Fill::default()),
        "ColorIndex" => match n()? {
            NONE => Box::new(|s: &mut Style| s.fill = Fill::default()),
            i => colour(color_from_index(i)?),
        },
        "ThemeColor" => colour(theme_color(n()?, 0.0)?),
        "TintAndShade" => {
            let t = tint_arg(a)?;
            Box::new(move |s: &mut Style| s.fill.color = with_tint(s.fill.color, t))
        }
        "Pattern" => match n()? {
            NONE => Box::new(|s: &mut Style| {
                s.fill = Fill {
                    cleared: true,
                    ..Fill::default()
                }
            }),
            i => {
                let p = pattern_from(i).ok_or_else(|| OmError::exception(SCODE_BAD_INDEX, None))?;
                Box::new(move |s: &mut Style| {
                    s.fill.pattern = p;
                    s.fill.cleared = false;
                })
            }
        },
        "PatternColor" => {
            let c = CellColor::Rgb(bgr_arg(app, a)?);
            pattern_colour(c)
        }
        "PatternColorIndex" if nothing => Box::new(|s: &mut Style| s.fill = Fill::default()),
        "PatternColorIndex" => {
            let c = color_from_index(n()?)?;
            pattern_colour(c)
        }
        "PatternThemeColor" => {
            let c = theme_color(n()?, 0.0)?;
            pattern_colour(c)
        }
        "PatternTintAndShade" => {
            let t = tint_arg(a)?;
            Box::new(move |s: &mut Style| {
                solidify(s);
                s.fill.pattern_color = with_tint(s.fill.pattern_color, t)
            })
        }
        _ => return Err(OmError::app_defined()),
    })
}

fn border_int(app: &Application, a: &Args, prop: &str) -> Result<Option<i32>, OmError> {
    let line = matches!(prop, "LineStyle" | "Weight");
    match &a.put {
        Some(Variant::Bstr(_)) if line => Err(OmError::unable_to_set(prop, "Border")),
        Some(Variant::Bool(_)) if prop == "ThemeColor" => Err(OmError::bad_index()),
        Some(Variant::Bool(_)) => Ok(None),
        None | Some(Variant::Empty | Variant::Null) => match prop {
            "LineStyle" => Ok(Some(NONE)),
            "ColorIndex" => Ok(Some(AUTOMATIC)),
            "Weight" => Err(OmError::unable_to_set(prop, "Border")),
            _ => Err(OmError::bad_index()),
        },
        Some(Variant::R8(x)) if line => Ok(Some(x.trunc() as i32)),
        _ => app.put_i32(a).map(Some),
    }
}

fn border_put(
    app: &Application,
    prop: &str,
    a: &Args,
) -> Result<Box<dyn Fn(Border) -> Border>, OmError> {
    let colour = |c: CellColor| -> Box<dyn Fn(Border) -> Border> {
        Box::new(move |b: Border| Border {
            line: if b.line == BorderLine::None {
                BorderLine::Thin
            } else {
                b.line
            },
            color: c,
        })
    };
    Ok(match prop {
        "LineStyle" => {
            let Some(style) = border_int(app, a, prop)? else {
                return Ok(Box::new(|b| b));
            };
            line_from(style, THIN).ok_or_else(OmError::app_defined)?;
            Box::new(move |b: Border| {
                let weight = match style {
                    DOT => THIN,
                    SLANT => MEDIUM,
                    _ => line_parts(b.line).1,
                };
                let line = line_from(style, weight).unwrap_or(BorderLine::Thin);
                Border {
                    line,
                    color: if line == BorderLine::None {
                        CellColor::Auto
                    } else {
                        b.color
                    },
                }
            })
        }
        "Weight" => {
            let Some(weight) = border_int(app, a, prop)? else {
                return Ok(Box::new(|b| b));
            };
            line_from(CONTINUOUS, weight).ok_or_else(OmError::app_defined)?;
            Box::new(move |b: Border| {
                let style = match line_parts(b.line).0 {
                    NONE | DOUBLE => CONTINUOUS,
                    s => s,
                };
                Border {
                    line: line_from(style, weight).unwrap_or(BorderLine::Thin),
                    color: b.color,
                }
            })
        }
        "Color" => colour(CellColor::Rgb(bgr_arg(app, a)?)),
        "ColorIndex" => match border_int(app, a, prop)? {
            None => Box::new(|b| b),
            Some(NONE) => Box::new(|_| Border::default()),
            Some(i) => colour(color_from_index(i)?),
        },
        "ThemeColor" => match border_int(app, a, prop)? {
            None => Box::new(|b| b),
            Some(i) => colour(theme_color(i, 0.0)?),
        },
        "TintAndShade" => {
            let t = tint_arg(a)?;
            Box::new(move |b: Border| Border {
                color: with_tint(b.color, t),
                ..b
            })
        }
        _ => return Err(OmError::app_defined()),
    })
}

impl Application<'_> {
    pub(crate) fn hyperlink_style(
        &mut self,
        book: BookId,
        sheet: SheetId,
        rect: Rect,
    ) -> Result<(), OmError> {
        let wb = &mut self.book_mut(book)?.wb;
        if &*wb.cell_style(CellAddr::new(sheet, rect.r0, rect.c0)).named != "Normal" {
            return Ok(());
        }
        wb.style_table_mut().ensure_hyperlink();
        let ns = wb
            .style_table()
            .get("Hyperlink")
            .cloned()
            .ok_or_else(OmError::app_defined)?;
        wb.set_style_with(sheet, rect, &|s: &Style| {
            let mut s = s.clone();
            apply_named(&mut s, &ns);
            s.named = ns.name.clone();
            s
        });
        Ok(())
    }
}

fn apply_named(s: &mut Style, n: &NamedStyle) {
    let from = &n.style;
    if n.include_number {
        s.number_format = from.number_format.clone();
    }
    if n.include_font {
        s.font = from.font.clone();
    }
    if n.include_alignment {
        s.alignment = from.alignment.clone();
    }
    if n.include_border {
        s.borders = from.borders.clone();
    }
    if n.include_patterns {
        s.fill = from.fill.clone();
    }
    if n.include_protection {
        s.locked = from.locked;
        s.formula_hidden = from.formula_hidden;
    }
    s.named = n.name.clone();
}

impl Application<'_> {
    fn owner_book(&self, o: &StyleOwner) -> BookId {
        match o {
            StyleOwner::Range(r) => r.book,
            StyleOwner::Named(b, _) => *b,
            StyleOwner::Display(r) => r.book,
            StyleOwner::Cond(b, ..) => *b,
        }
    }

    fn named_style(&self, b: BookId, name: &str) -> Result<&NamedStyle, OmError> {
        self.book(b)?
            .wb
            .style_table()
            .get(name)
            .ok_or_else(|| OmError::exception(SCODE_BAD_INDEX, None))
    }

    pub(crate) fn agree_on<T: PartialEq>(
        &self,
        o: &StyleOwner,
        f: impl Fn(&Style) -> T,
    ) -> Result<Option<T>, OmError> {
        match o {
            StyleOwner::Range(r) => {
                let (sheet, rect) = self.resolve(r)?;
                let wb = &self.book(r.book)?.wb;
                Ok(agree(cells(sheet, rect).map(|a| f(wb.cell_style(a)))))
            }
            StyleOwner::Named(b, n) => Ok(Some(f(&self.named_style(*b, n)?.style))),
            StyleOwner::Display(r) => {
                let (sheet, rect) = self.resolve(r)?;
                let wb = &self.book(r.book)?.wb;
                Ok(agree(cells(sheet, rect).map(|a| f(&wb.display_style(a)))))
            }
            StyleOwner::Cond(b, s, id) => {
                let wb = &self.book(*b)?.wb;
                let rule = wb
                    .cfv_sheet(*s)
                    .and_then(|sh| sh.index_of(*id).map(|i| &sh.rules[i]))
                    .ok_or_else(OmError::app_defined)?;
                Ok(Some(f(&rule.dxf.style)))
            }
        }
    }

    fn cond_mask(&self, o: &StyleOwner) -> Option<xlcalc::cfv::DxfMask> {
        let StyleOwner::Cond(b, s, id) = o else {
            return None;
        };
        let sh = self.book(*b).ok()?.wb.cfv_sheet(*s)?;
        sh.index_of(*id).map(|i| sh.rules[i].dxf.mask)
    }

    fn edit_style(&mut self, o: &StyleOwner, f: &dyn Fn(&mut Style)) -> Result<(), OmError> {
        match o {
            StyleOwner::Range(r) => {
                let (sheet, rect) = self.resolve(r)?;
                let wb = &mut self.book_mut(r.book)?.wb;
                wb.set_style_with(sheet, rect, &|s: &Style| {
                    let mut s = s.clone();
                    f(&mut s);
                    s
                });
            }
            StyleOwner::Display(_) => return Err(OmError::app_defined()),
            StyleOwner::Cond(b, s, id) => {
                let wb = &mut self.book_mut(*b)?.wb;
                let sh = wb.cfv_sheet_mut(*s);
                let i = sh.index_of(*id).ok_or_else(OmError::app_defined)?;
                let dxf = &mut sh.rules[i].dxf;
                let (a, b2) = dxf_probe(f);
                merge_dxf(dxf, &a, &b2);
            }
            StyleOwner::Named(b, n) => {
                let mut ns = self.named_style(*b, n)?.clone();
                f(&mut ns.style);
                let wb = &mut self.book_mut(*b)?.wb;

                if ns.name.eq_ignore_ascii_case("Normal") {
                    wb.set_base_style(ns.style.clone());
                }
                wb.style_table_mut().insert(ns);
            }
        }
        Ok(())
    }

    pub(crate) fn font_member(
        &mut self,
        o: &StyleOwner,
        m: &'static Member,
        a: &Args,
    ) -> Result<Variant, OmError> {
        let theme = self
            .book(self.owner_book(o))?
            .wb
            .style_table()
            .theme
            .clone();

        let put = matches!(m.kind, InvokeKind::Put | InvokeKind::PutRef);
        let zero_name;
        let mut a = a;
        if matches!(o, StyleOwner::Named(..)) {
            if m.name == "FontStyle" || (m.name == "Background" && put) {
                return Err(OmError::app_defined());
            }
            if m.name == "Name"
                && put
                && matches!(a.put, None | Some(Variant::Empty | Variant::Null))
            {
                zero_name = Args {
                    values: Vec::new(),
                    put: Some(Variant::Bstr("0".into())),
                    lcid: a.lcid,
                };
                a = &zero_name;
            }
        }
        if matches!(o, StyleOwner::Display(_)) || m.name == "Background" {
            if let Some(r) = display_font_special(m, a) {
                return r;
            }
        }
        match m.kind {
            InvokeKind::Get => {
                let mut f = FontFields {
                    name: self.agree_on(o, |s| s.font.name.clone())?,
                    size: self.agree_on(o, |s| s.font.size_twips)?,
                    bold: self.agree_on(o, |s| s.font.bold)?,
                    italic: self.agree_on(o, |s| s.font.italic)?,
                    underline: self.agree_on(o, |s| s.font.underline)?,
                    strike: self.agree_on(o, |s| s.font.strikethrough)?,
                    sup: self.agree_on(o, |s| s.font.superscript)?,
                    sub: self.agree_on(o, |s| s.font.subscript)?,
                    color: self.agree_on(o, |s| s.font.color)?,
                    scheme: self.agree_on(o, |s| s.font.scheme)?,
                };

                if let Some(mk) = self.cond_mask(o) {
                    let style = mk.bold || mk.italic;
                    f.name = None;
                    f.size = None;
                    f.scheme = None;
                    f.sup = None;
                    f.sub = None;
                    f.bold = f.bold.filter(|_| style);
                    f.italic = f.italic.filter(|_| style);
                    f.underline = f.underline.filter(|_| mk.underline);
                    f.strike = f.strike.filter(|_| mk.strike);
                    f.color = f.color.filter(|_| mk.font_color);
                }
                font_prop(&theme, &f, m)
            }
            _ => {
                self.style_guard(o, "Font", m.name)?;
                let edit = font_put(self, m.name, a, &theme)?;
                self.edit_style(o, &*edit)?;
                Ok(Variant::Empty)
            }
        }
    }

    pub(crate) fn interior_member(
        &mut self,
        o: &StyleOwner,
        m: &'static Member,
        a: &Args,
    ) -> Result<Variant, OmError> {
        let theme = self
            .book(self.owner_book(o))?
            .wb
            .style_table()
            .theme
            .clone();
        if m.name == "InvertIfNegative" {
            return Err(OmError::app_defined());
        }

        if (m.name, m.kind) == ("Gradient", InvokeKind::Get) {
            return Ok(Variant::Dispatch(None));
        }
        if matches!(o, StyleOwner::Display(_))
            && matches!(m.kind, InvokeKind::Put | InvokeKind::PutRef)
        {
            return Err(OmError::app_defined());
        }
        match m.kind {
            InvokeKind::Get => {
                let mut f = FillFields {
                    pattern: self.agree_on(o, |s| s.fill.pattern)?,
                    color: self.agree_on(o, |s| s.fill.color)?,
                    pattern_color: self.agree_on(o, |s| s.fill.pattern_color)?,
                };
                if let Some(mk) = self.cond_mask(o) {
                    f.pattern = f.pattern.filter(|_| mk.pattern_read);
                    f.color = f.color.filter(|_| mk.fill_color);
                    f.pattern_color = f.pattern_color.filter(|_| mk.pattern_color);
                }
                interior_prop(&theme, &f, m)
            }
            _ => {
                self.style_guard(o, "Interior", m.name)?;
                let edit = interior_put(self, m.name, a)?;
                self.edit_style(o, &*edit)?;
                if let StyleOwner::Cond(b, s, id) = o {
                    if matches!(m.name, "Pattern" | "ThemeColor") {
                        let sh = self.book_mut(*b)?.wb.cfv_sheet_mut(*s);
                        if let Some(i) = sh.index_of(*id) {
                            sh.rules[i].dxf.mask.pattern_read = true;
                        }
                    }
                }
                Ok(Variant::Empty)
            }
        }
    }

    pub(crate) fn borders_member(
        &mut self,
        o: &StyleOwner,
        m: &'static Member,
        a: &Args,
    ) -> Result<Variant, OmError> {
        use InvokeKind::*;
        match (m.name, m.kind) {
            ("Item" | "_Default", Get) => {
                let i = a
                    .i32(0)
                    .map_err(|_| OmError::arg(crate::error::DISP_E_TYPEMISMATCH, 0))?
                    .ok_or_else(OmError::app_defined)?;

                if !(5..=12).contains(&i) && !matches!(i, -4131 | -4152 | -4160 | -4107) {
                    return Err(OmError::app_defined());
                }
                Ok(self.alloc(Object::Border(o.clone(), i)))
            }
            ("Count", Get) => Ok(i4(if matches!(o, StyleOwner::Cond(..)) {
                4
            } else {
                6
            })),
            (prop, Get) => {
                let prop = if prop == "Value" { "LineStyle" } else { prop };

                let mut seen: Option<Variant> = None;
                for i in [7, 8, 9, 10, 11, 12] {
                    if i > 10 && matches!(o, StyleOwner::Named(..)) {
                        break;
                    }

                    let at = match i {
                        7 if matches!(o, StyleOwner::Named(..)) => -4131,
                        8 if matches!(o, StyleOwner::Named(..)) => -4160,
                        9 if matches!(o, StyleOwner::Named(..)) => -4107,
                        10 if matches!(o, StyleOwner::Named(..)) => -4152,
                        _ => i,
                    };
                    let v = self.border_get(o, at, prop)?;
                    if v == Variant::Null && matches!(i, 11 | 12) {
                        continue;
                    }
                    match &seen {
                        None => seen = Some(v),
                        Some(p) if *p == v => {}
                        Some(_) => return Ok(Variant::Null),
                    }
                }
                Ok(seen.unwrap_or(Variant::Null))
            }
            (prop, _) if matches!(o, StyleOwner::Display(_)) => Err(match prop {
                "LineStyle" | "Value" | "Weight" => OmError::unable_to_set(prop, "Borders"),
                _ => OmError::app_defined(),
            }),
            (prop, _) => {
                let prop = if prop == "Value" { "LineStyle" } else { prop };

                let cleared;
                let a = if prop == "ColorIndex" && matches!(a.put, Some(Variant::Bool(_))) {
                    cleared = Args {
                        values: Vec::new(),
                        put: Some(Variant::I4(NONE)),
                        lcid: a.lcid,
                    };
                    &cleared
                } else {
                    a
                };
                for i in [7, 8, 9, 10, 11, 12] {
                    if i > 10 && matches!(o, StyleOwner::Named(..)) {
                        break;
                    }
                    self.border_set(o, i, prop, a)?;
                }

                if matches!(prop, "LineStyle" | "Weight") && matches!(o, StyleOwner::Range(_)) {
                    self.diagonal_line_set(o, prop, a)?;
                }
                Ok(Variant::Empty)
            }
        }
    }

    pub(crate) fn border_member(
        &mut self,
        o: &StyleOwner,
        index: i32,
        m: &'static Member,
        a: &Args,
    ) -> Result<Variant, OmError> {
        if matches!(o, StyleOwner::Cond(..))
            && m.name == "LineStyle"
            && !matches!(index, -4131 | -4152 | -4160 | -4107)
        {
            return Err(OmError::app_defined());
        }
        match m.kind {
            InvokeKind::Get => self.border_get(o, index, m.name),
            _ => {
                self.border_set(o, index, m.name, a)?;
                Ok(Variant::Empty)
            }
        }
    }

    fn border_get(&self, o: &StyleOwner, index: i32, prop: &str) -> Result<Variant, OmError> {
        let b = self.owner_book(o);
        let theme = &self.book(b)?.wb.style_table().theme;
        match o {
            StyleOwner::Display(r) => {
                let (sheet, rect) = self.resolve(r)?;
                let Some((cells_at, side)) = border_cells(rect, index)? else {
                    return Ok(Variant::Null);
                };
                let wb = &self.book(r.book)?.wb;
                let first = CellAddr::new(sheet, cells_at.r0, cells_at.c0);
                border_prop(theme, Some(side_of(&wb.display_style(first), side)), prop)
            }
            StyleOwner::Named(..) | StyleOwner::Cond(..) => {
                let named = matches!(o, StyleOwner::Named(..));
                let side = match index {
                    -4131 => Side::Edge(Edge::Left),
                    -4152 => Side::Edge(Edge::Right),
                    -4160 => Side::Edge(Edge::Top),
                    -4107 => Side::Edge(Edge::Bottom),
                    7 if !named => Side::Edge(Edge::Left),
                    10 if !named => Side::Edge(Edge::Right),
                    8 if !named => Side::Edge(Edge::Top),
                    9 if !named => Side::Edge(Edge::Bottom),
                    5 => Side::DiagonalDown,
                    6 => Side::DiagonalUp,

                    _ => {
                        return Ok(if prop == "Color" {
                            Variant::R8(0.0)
                        } else {
                            Variant::Null
                        })
                    }
                };
                let line = self.agree_on(o, |s| side_of(s, side))?;
                border_prop(theme, line, prop)
            }
            StyleOwner::Range(r) => {
                let (sheet, rect) = self.resolve(r)?;
                let Some((cells_at, side)) = border_cells(rect, index)? else {
                    return Ok(Variant::Null);
                };

                let wb = &self.book(r.book)?.wb;
                let first = CellAddr::new(sheet, cells_at.r0, cells_at.c0);

                if !matches!(side, Side::Edge(_)) {
                    let s = wb.cell_style(first);
                    if rect != Rect::cell(rect.r0, rect.c0)
                        || (!s.borders.diagonal_down && !s.borders.diagonal_up)
                    {
                        match prop {
                            "ColorIndex" => return Ok(i4(NONE)),
                            "ThemeColor" | "TintAndShade" => return Ok(Variant::Null),
                            _ => {}
                        }
                    }
                }
                border_prop(theme, Some(shown_side(wb, first, side)), prop)
            }
        }
    }

    fn border_set(
        &mut self,
        o: &StyleOwner,
        index: i32,
        prop: &str,
        a: &Args,
    ) -> Result<(), OmError> {
        self.style_guard(o, "Border", prop)?;
        let edit = border_put(self, prop, a)?;
        let r = match o {
            StyleOwner::Display(_) => return Err(OmError::app_defined()),
            StyleOwner::Named(..) | StyleOwner::Cond(..) => {
                let side = match index {
                    -4131 | 7 => Edge::Left,
                    -4152 | 10 => Edge::Right,
                    -4160 | 8 => Edge::Top,
                    -4107 | 9 => Edge::Bottom,
                    _ => return Err(OmError::app_defined()),
                };
                return self.edit_style(o, &|s: &mut Style| {
                    let b = s.borders.edge(side);
                    *s.borders.edge_mut(side) = edit(b);
                });
            }
            StyleOwner::Range(r) => *r,
        };
        let (sheet, rect) = self.resolve(&r)?;
        let Some((at, side)) = border_cells(rect, index)? else {
            return Ok(());
        };
        let wb = &mut self.book_mut(r.book)?.wb;
        let first = CellAddr::new(sheet, at.r0, at.c0);
        let shown = shown_side(wb, first, side);

        let multi = rect != Rect::cell(rect.r0, rect.c0);
        if !matches!(side, Side::Edge(_))
            && (multi || shown.line == BorderLine::None)
            && matches!(prop, "Color" | "ColorIndex" | "ThemeColor" | "TintAndShade")
        {
            return Ok(());
        }
        let line = edit(shown);
        match side {
            Side::Edge(e) => {
                wb.set_style_with(sheet, at, &|s: &Style| {
                    let mut s = s.clone();
                    *s.borders.edge_mut(e) = line;
                    s
                });
                if let Some(n) = neighbour(at, e) {
                    wb.set_style_with(sheet, n, &|s: &Style| {
                        let mut s = s.clone();
                        *s.borders.edge_mut(e.opposite()) = Border::default();
                        s
                    });
                }
            }
            Side::DiagonalDown | Side::DiagonalUp => {
                let down = side == Side::DiagonalDown;
                wb.set_style_with(sheet, at, &|s: &Style| {
                    let mut s = s.clone();
                    let on = line.line != BorderLine::None;
                    if multi {
                        if !s.borders.diagonal_down && !s.borders.diagonal_up {
                            s.borders.diagonal = line;
                        }
                        return s;
                    }
                    if down {
                        s.borders.diagonal_down = on;
                    } else {
                        s.borders.diagonal_up = on;
                    }
                    if on {
                        s.borders.diagonal = line;
                    } else if !s.borders.diagonal_down && !s.borders.diagonal_up {
                        s.borders.diagonal = Border::default();
                    }
                    s
                });
            }
        }
        Ok(())
    }

    pub(crate) fn range_style_member(
        &mut self,
        r: &RangeRef,
        sheet: SheetId,
        rect: Rect,
        m: &'static Member,
        a: &Args,
    ) -> Option<Result<Variant, OmError>> {
        use InvokeKind::*;
        let owner = StyleOwner::Range(*r);
        Some(match (m.name, m.kind) {
            ("Font", Get) => Ok(self.alloc(Object::Font(owner))),
            ("Interior", Get) => Ok(self.alloc(Object::Interior(owner))),
            ("Borders", Get) => match a.get(0) {
                Some(_) => a
                    .i32(0)
                    .and_then(|i| i.ok_or_else(OmError::app_defined))
                    .map(|i| self.alloc(Object::Border(owner, i))),
                None => Ok(self.alloc(Object::Borders(owner))),
            },
            ("Style", Get) => self.agree_on(&owner, |s| s.named.clone()).map(|n| match n {
                Some(n) => self.alloc(Object::Style(r.book, n)),
                None => Variant::Dispatch(None),
            }),
            ("Style", Put | PutRef) => (|| {
                let name = match a.put.as_ref() {
                    Some(Variant::Dispatch(Some(id))) => match self.arena.get(*id) {
                        Some(Object::Style(_, n)) => n.to_string(),
                        _ => return Err(OmError::app_defined()),
                    },
                    _ => self.put_string(a)?,
                };
                let ns = self
                    .book(r.book)?
                    .wb
                    .style_table()
                    .get(&name)
                    .cloned()
                    .ok_or_else(|| OmError::hresult(crate::error::DISP_E_BADPARAMCOUNT))?;
                let wb = &mut self.book_mut(r.book)?.wb;
                wb.set_style_with(sheet, rect, &|s: &Style| {
                    let mut s = s.clone();
                    apply_named(&mut s, &ns);
                    s
                });
                if ns.include_number {
                    wb.set_number_format(sheet, rect, &ns.style.number_format);
                }
                Ok(Variant::Empty)
            })(),
            (prop, Get) => {
                let wb = match self.book(r.book) {
                    Ok(b) => &b.wb,
                    Err(e) => return Some(Err(e)),
                };
                let get = |f: &dyn Fn(&Style) -> Variant| {
                    agree(cells(sheet, rect).map(|x| f(wb.cell_style(x)))).unwrap_or(Variant::Null)
                };
                return format_prop(&get, prop).map(Ok);
            }
            (prop, _) => {
                let edit = match format_put(prop, a, self) {
                    Ok(Some(e)) => e,
                    Ok(None) => return None,
                    Err(e) => return Some(Err(e)),
                };
                self.edit_style(&owner, &*edit).map(|()| Variant::Empty)
            }
        })
    }

    pub(crate) fn styles_member(
        &mut self,
        b: BookId,
        m: &'static Member,
        a: &Args,
    ) -> Result<Variant, OmError> {
        let table = self.book(b)?.wb.style_table();
        match m.name {
            "Count" => Ok(i4(table.named.len() as i32)),
            "Item" | "_Default" => {
                let found = match a.get(0) {
                    Some(Variant::Bstr(t)) => table.get(t).map(|s| s.name.clone()),
                    Some(_) => {
                        let i = a.i32(0)?.unwrap_or(0);
                        usize::try_from(i - 1)
                            .ok()
                            .and_then(|i| table.named.get(i))
                            .map(|s| s.name.clone())
                    }
                    None => None,
                };
                let name = found.ok_or_else(|| OmError::exception(SCODE_BAD_INDEX, None))?;
                Ok(self.alloc(Object::Style(b, name)))
            }
            "Add" => {
                let name = a.string(0)?.ok_or_else(OmError::app_defined)?;
                if name.chars().count() > 255 || table.get(&name).is_some() {
                    return Err(OmError::app_defined());
                }

                let mut style = match a.get(1) {
                    Some(Variant::Dispatch(_)) => {
                        let (sheet, rect) = self.range_arg(a.get(1))?;
                        self.book(b)?
                            .wb
                            .cell_style(CellAddr::new(sheet, rect.r0, rect.c0))
                            .clone()
                    }
                    _ => table
                        .get("Normal")
                        .map(|n| n.style.clone())
                        .unwrap_or_default(),
                };
                style.named = Arc::from(name.as_str());
                self.book_mut(b)?.wb.style_table_mut().insert(NamedStyle {
                    name: Arc::from(name.as_str()),
                    builtin_id: None,
                    style,
                    include_number: true,
                    include_font: true,
                    include_alignment: true,
                    include_border: true,
                    include_patterns: true,
                    include_protection: true,
                });
                Ok(self.alloc(Object::Style(b, Arc::from(name.as_str()))))
            }
            _ => Err(unsupported("Styles", m)),
        }
    }

    pub(crate) fn style_member(
        &mut self,
        b: BookId,
        name: &Arc<str>,
        m: &'static Member,
        a: &Args,
    ) -> Result<Variant, OmError> {
        use InvokeKind::*;
        let owner = StyleOwner::Named(b, name.clone());
        let ns = self.named_style(b, name)?.clone();
        let flag = |x: bool| Ok(Variant::Bool(x));
        match (m.name, m.kind) {
            ("Name" | "NameLocal" | "Value" | "_Default", Get) => {
                Ok(Variant::Bstr(ns.name.to_string()))
            }

            ("IndentLevel", Get) => Ok(Variant::Null),
            ("MergeCells", Get) => Err(OmError::app_defined()),
            ("BuiltIn", Get) => flag(ns.builtin_id.is_some()),
            ("Delete", Func) if ns.builtin_id.is_none() => {
                let wb = &mut self.book_mut(b)?.wb;
                wb.style_table_mut()
                    .named
                    .retain(|s| !s.name.eq_ignore_ascii_case(name));
                Ok(Variant::Bool(true))
            }
            ("NumberFormat", Get) => Ok(Variant::Bstr(ns.style.number_format.to_string())),

            ("NumberFormatLocal", Get) => {
                let wb = &self.book(b)?.wb;
                Ok(Variant::Bstr(crate::range::engine_format(
                    wb,
                    &ns.style.number_format,
                )))
            }
            ("IncludeNumber", Get) => flag(ns.include_number),
            ("IncludeFont", Get) => flag(ns.include_font),
            ("IncludeAlignment", Get) => flag(ns.include_alignment),
            ("IncludeBorder", Get) => flag(ns.include_border),
            ("IncludePatterns", Get) => flag(ns.include_patterns),
            ("IncludeProtection", Get) => flag(ns.include_protection),
            ("Font", Get) => Ok(self.alloc(Object::Font(owner))),
            ("Interior", Get) => Ok(self.alloc(Object::Interior(owner))),
            ("Borders", Get) => Ok(self.alloc(Object::Borders(owner))),
            (prop, Get) => {
                let get = |f: &dyn Fn(&Style) -> Variant| f(&ns.style);
                format_prop(&get, prop).ok_or_else(|| unsupported("Style", m))
            }
            (prop, _) => match format_put(prop, a, self)? {
                Some(edit) => {
                    self.edit_style(&owner, &*edit)?;
                    Ok(Variant::Empty)
                }
                None => Err(unsupported("Style", m)),
            },
        }
    }
}

fn neighbour(at: Rect, e: Edge) -> Option<Rect> {
    Some(match e {
        Edge::Left => Rect::new(at.r0, at.c0.checked_sub(1)?, at.r1, at.c0.checked_sub(1)?),
        Edge::Right if at.c1 + 1 < MAX_COLS => Rect::new(at.r0, at.c1 + 1, at.r1, at.c1 + 1),
        Edge::Top => Rect::new(at.r0.checked_sub(1)?, at.c0, at.r0.checked_sub(1)?, at.c1),
        Edge::Bottom if at.r1 + 1 < MAX_ROWS => Rect::new(at.r1 + 1, at.c0, at.r1 + 1, at.c1),
        _ => return None,
    })
}

fn poison_style() -> Style {
    let mut s = Style {
        number_format: Arc::from("0.0000_);(@)"),
        ..Style::default()
    };
    s.font.bold = true;
    s.font.italic = true;
    s.font.underline = Underline::Single;
    s.font.strikethrough = true;
    s.font.color = CellColor::Rgb(0x123456);
    s.fill.pattern = Pattern::MediumGray;
    s.fill.color = CellColor::Rgb(0xABCDEF);
    s.fill.pattern_color = CellColor::Rgb(0x654321);
    let b = Border {
        line: BorderLine::Thick,
        color: CellColor::Rgb(0x112233),
    };
    s.borders.left = b;
    s.borders.right = b;
    s.borders.top = b;
    s.borders.bottom = b;
    s
}

fn dxf_probe(f: &dyn Fn(&mut Style)) -> (Style, Style) {
    let mut a = Style::default();
    f(&mut a);
    let mut b = poison_style();
    f(&mut b);
    (a, b)
}

fn merge_dxf(dxf: &mut xlcalc::cfv::Dxf, a: &Style, b: &Style) {
    let (m, s) = (&mut dxf.mask, &mut dxf.style);
    if a.number_format == b.number_format {
        m.number_format = true;
        s.number_format = a.number_format.clone();
    }
    if a.font.bold == b.font.bold {
        m.bold = true;
        s.font.bold = a.font.bold;
    }
    if a.font.italic == b.font.italic {
        m.italic = true;
        s.font.italic = a.font.italic;
    }
    if a.font.underline == b.font.underline {
        m.underline = true;
        s.font.underline = a.font.underline;
    }
    if a.font.strikethrough == b.font.strikethrough {
        m.strike = true;
        s.font.strikethrough = a.font.strikethrough;
    }
    if a.font.color == b.font.color {
        m.font_color = true;
        s.font.color = a.font.color;
    }
    if a.fill.pattern == b.fill.pattern {
        m.pattern = true;
        s.fill.pattern = a.fill.pattern;
    }
    if a.fill.color == b.fill.color
        && a.fill.pattern != b.fill.pattern
        && a.fill.pattern == Pattern::Solid
    {
        m.pattern = true;
        s.fill.pattern = Pattern::Solid;
    }
    if a.fill.color == b.fill.color {
        m.fill_color = true;
        s.fill.color = a.fill.color;
    }
    if a.fill.pattern_color == b.fill.pattern_color {
        m.pattern_color = true;
        s.fill.pattern_color = a.fill.pattern_color;
    }
    if a.borders.left == b.borders.left {
        m.left = true;
        s.borders.left = a.borders.left;
    }
    if a.borders.right == b.borders.right {
        m.right = true;
        s.borders.right = a.borders.right;
    }
    if a.borders.top == b.borders.top {
        m.top = true;
        s.borders.top = a.borders.top;
    }
    if a.borders.bottom == b.borders.bottom {
        m.bottom = true;
        s.borders.bottom = a.borders.bottom;
    }
}

fn display_font_special(m: &'static Member, a: &Args) -> Option<Result<Variant, OmError>> {
    let put = matches!(m.kind, InvokeKind::Put | InvokeKind::PutRef);
    if !put {
        return (m.name == "Background").then_some(Ok(Variant::Null));
    }
    let text = matches!(a.put.as_ref(), Some(Variant::Bstr(t)) if t.trim().parse::<f64>().is_err());
    let nothing = matches!(a.put.as_ref(), None | Some(Variant::Null | Variant::Empty));
    Some(match m.name {
        "Color" | "ColorIndex" | "ThemeColor" | "TintAndShade" | "ThemeFont" => {
            Err(OmError::exception(0x8000_4001_u32 as i32, None))
        }
        "Bold" | "Italic" | "Strikethrough" | "Subscript" | "Superscript" if text => {
            Err(OmError::unable_to_set(m.name, "Font"))
        }
        "Background" | "Size" | "Underline" if text || nothing => {
            Err(OmError::unable_to_set(m.name, "Font"))
        }
        _ => Ok(Variant::Empty),
    })
}

impl Application<'_> {
    fn diagonal_line_set(&mut self, o: &StyleOwner, prop: &str, a: &Args) -> Result<(), OmError> {
        let StyleOwner::Range(r) = o else {
            return Ok(());
        };
        let edit = border_put(self, prop, a)?;
        let (sheet, rect) = self.resolve(r)?;
        let wb = &mut self.book_mut(r.book)?.wb;
        wb.set_style_with(sheet, rect, &|s: &Style| {
            let mut s = s.clone();
            if !s.borders.diagonal_down && !s.borders.diagonal_up {
                s.borders.diagonal = edit(Border {
                    line: s.borders.diagonal.line,
                    color: CellColor::Auto,
                });
            }
            s
        });
        Ok(())
    }
}
