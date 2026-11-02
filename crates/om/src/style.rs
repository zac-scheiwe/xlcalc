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
        -4166 => 255,
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
                .position(|&c| c == n().unwrap_or(0))
                .ok_or_else(OmError::app_defined)?;
            let h = HAlign::ALL[i];
            Box::new(move |s: &mut Style| s.alignment.horizontal = h)
        }
        "VerticalAlignment" => {
            let i = V_ALIGN
                .iter()
                .position(|&c| c == n().unwrap_or(0))
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
            let x = n()?;
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
        "Orientation" => {
            let r = rotation_from(n()?).ok_or_else(OmError::app_defined)?;
            Box::new(move |s: &mut Style| s.alignment.rotation = r)
        }
        _ => return Ok(None),
    }))
}

type StyleEdit = Box<dyn Fn(&mut Style)>;

fn bgr_arg(app: &Application, a: &Args) -> Result<u32, OmError> {
    let x = app.put_f64(a)?;
    if !(0.0..=16_777_215.0).contains(&x) {
        return Err(OmError::app_defined());
    }
    Ok(swap_rb(x as u32))
}

fn font_put(app: &Application, prop: &str, a: &Args, theme: &Theme) -> Result<StyleEdit, OmError> {
    let n = || app.put_i32(a);
    let b = || app.put_bool(a);
    Ok(match prop {
        "Name" => {
            let name: Arc<str> = Arc::from(app.put_string(a)?);
            Box::new(move |s: &mut Style| {
                s.font.name = name.clone();
                s.font.scheme = FontScheme::None;
            })
        }
        "Size" => {
            let x = app.put_f64(a)?;
            if !(1.0..=409.0).contains(&x) {
                return Err(OmError::app_defined());
            }

            let t = (x * 20.0).round() as u32;
            Box::new(move |s: &mut Style| s.font.size_twips = t)
        }
        "Bold" => {
            let x = b()?;
            Box::new(move |s: &mut Style| s.font.bold = x)
        }
        "Italic" => {
            let x = b()?;
            Box::new(move |s: &mut Style| s.font.italic = x)
        }
        "Strikethrough" => {
            let x = b()?;
            Box::new(move |s: &mut Style| s.font.strikethrough = x)
        }
        "Superscript" => {
            let x = b()?;
            Box::new(move |s: &mut Style| {
                s.font.superscript = x;
                if x {
                    s.font.subscript = false;
                }
            })
        }
        "Subscript" => {
            let x = b()?;
            Box::new(move |s: &mut Style| {
                s.font.subscript = x;
                if x {
                    s.font.superscript = false;
                }
            })
        }
        "Underline" => {
            let u = underline_from(n()?).ok_or_else(OmError::app_defined)?;
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
            let c = color_from_index(n()?)?;
            Box::new(move |s: &mut Style| s.font.color = c)
        }
        "ThemeColor" => {
            let c = theme_color(n()?, 0.0)?;
            Box::new(move |s: &mut Style| s.font.color = c)
        }
        "TintAndShade" => {
            let t = app.put_f64(a)?;
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
    Ok(match prop {
        "Color" => colour(CellColor::Rgb(bgr_arg(app, a)?)),
        "ColorIndex" => match n()? {
            NONE => Box::new(|s: &mut Style| s.fill = Fill::default()),
            i => colour(color_from_index(i)?),
        },
        "ThemeColor" => colour(theme_color(n()?, 0.0)?),
        "TintAndShade" => {
            let t = app.put_f64(a)?;
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
        "PatternColorIndex" => {
            let c = color_from_index(n()?)?;
            pattern_colour(c)
        }
        "PatternThemeColor" => {
            let c = theme_color(n()?, 0.0)?;
            pattern_colour(c)
        }
        "PatternTintAndShade" => {
            let t = app.put_f64(a)?;
            Box::new(move |s: &mut Style| {
                solidify(s);
                s.fill.pattern_color = with_tint(s.fill.pattern_color, t)
            })
        }
        _ => return Err(OmError::app_defined()),
    })
}

fn border_put(
    app: &Application,
    prop: &str,
    a: &Args,
) -> Result<Box<dyn Fn(Border) -> Border>, OmError> {
    let n = || app.put_i32(a);

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
            let style = n()?;
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
            let weight = n()?;
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
        "ColorIndex" => match n()? {
            NONE => Box::new(|_| Border::default()),
            i => colour(color_from_index(i)?),
        },
        "ThemeColor" => colour(theme_color(n()?, 0.0)?),
        "TintAndShade" => {
            let t = app.put_f64(a)?;
            Box::new(move |b: Border| Border {
                color: with_tint(b.color, t),
                ..b
            })
        }
        _ => return Err(OmError::app_defined()),
    })
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
        }
    }

    fn named_style(&self, b: BookId, name: &str) -> Result<&NamedStyle, OmError> {
        self.book(b)?
            .wb
            .style_table()
            .get(name)
            .ok_or_else(|| OmError::exception(SCODE_BAD_INDEX, None))
    }

    fn agree_on<T: PartialEq>(
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
        }
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
        match m.kind {
            InvokeKind::Get => {
                let f = FontFields {
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
                font_prop(&theme, &f, m)
            }
            _ => {
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
        match m.kind {
            InvokeKind::Get => {
                let f = FillFields {
                    pattern: self.agree_on(o, |s| s.fill.pattern)?,
                    color: self.agree_on(o, |s| s.fill.color)?,
                    pattern_color: self.agree_on(o, |s| s.fill.pattern_color)?,
                };
                interior_prop(&theme, &f, m)
            }
            _ => {
                let edit = interior_put(self, m.name, a)?;
                self.edit_style(o, &*edit)?;
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
                let i = a.i32(0)?.ok_or_else(OmError::app_defined)?;
                Ok(self.alloc(Object::Border(o.clone(), i)))
            }
            ("Count", Get) => Ok(i4(6)),
            (prop, Get) => {
                let mut seen: Option<Variant> = None;
                for i in [7, 8, 9, 10, 11, 12] {
                    let v = self.border_get(o, i, prop)?;
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
            (prop, _) => {
                for i in [7, 8, 9, 10, 11, 12] {
                    self.border_set(o, i, prop, a)?;
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
            StyleOwner::Named(..) => {
                let side = match index {
                    -4131 => Side::Edge(Edge::Left),
                    -4152 => Side::Edge(Edge::Right),
                    -4160 => Side::Edge(Edge::Top),
                    -4107 => Side::Edge(Edge::Bottom),
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
        let edit = border_put(self, prop, a)?;
        let r = match o {
            StyleOwner::Named(..) => {
                let side = match index {
                    -4131 => Edge::Left,
                    -4152 => Edge::Right,
                    -4160 => Edge::Top,
                    -4107 => Edge::Bottom,
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
        let line = edit(shown_side(wb, first, side));
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
                    if down {
                        s.borders.diagonal_down = on;
                    } else {
                        s.borders.diagonal_up = on;
                    }
                    if on {
                        s.borders.diagonal = line;
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
            ("Style", Get) => self
                .agree_on(&owner, |s| s.named.clone())
                .map(|n| or_null(n, |n| self.alloc(Object::Style(r.book, n)))),
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
            ("BuiltIn", Get) => flag(ns.builtin_id.is_some()),
            ("NumberFormat" | "NumberFormatLocal", Get) => {
                Ok(Variant::Bstr(ns.style.number_format.to_string()))
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
