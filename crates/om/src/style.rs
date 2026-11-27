use std::sync::Arc;

use xlcalc::style::{
    stored_tint, Border, BorderLine, CellColor, Edge, Fill, FontScheme, HAlign, NamedStyle,
    Pattern, Style, Theme, Underline, VAlign,
};
use xlcalc::{CellAddr, Rect, SheetId, Workbook};

use crate::addr::{max_cols, max_rows};
use xlcalc_variant::{HResult, Variant};

use crate::app::{unsupported, Application, BookId, Object, RangeRef};
use crate::conv::Args;
use crate::error::{OmError, SCODE_BAD_INDEX};
use crate::typelib::{InvokeKind, Member};

#[derive(Clone, Debug)]
pub(crate) enum StyleOwner {
    Range(RangeRef),

    Areas(Vec<RangeRef>),
    Named(BookId, Arc<str>),

    Display(RangeRef),

    Cond(BookId, SheetId, u32, Vec<Rect>),
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
        AUTOMATIC | 0 => Ok(CellColor::Auto),
        1..=56 => Ok(CellColor::Indexed(n as u8 + 7)),
        _ => Err(OmError::bad_index()),
    }
}

fn index_of(theme: &Theme, c: CellColor, auto: u32) -> i32 {
    match c {
        CellColor::Auto | CellColor::Indexed(64) | CellColor::AutoTint { .. } => AUTOMATIC,
        CellColor::Indexed(i) | CellColor::IndexedTint { index: i, .. } if i < 64 => {
            i32::from(i) - 7
        }
        c => theme.color_index(theme.rgb(c, auto)),
    }
}

fn font_index_of(theme: &Theme, c: CellColor) -> i32 {
    match c {
        CellColor::Auto | CellColor::AutoTint { .. } => AUTOMATIC,
        CellColor::Indexed(i) | CellColor::IndexedTint { index: i, .. } => i32::from(i) - 7,
        c => theme.color_index(theme.rgb(c, 0)),
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
        _ => Err(OmError::bad_index()),
    }
}

fn font_tint(c: CellColor, t: f64) -> CellColor {
    match c {
        CellColor::Indexed(index) | CellColor::IndexedTint { index, .. } if index < 64 => {
            let tint = stored_tint(t);
            if tint == 0.0 {
                CellColor::Indexed(index)
            } else {
                CellColor::IndexedTint {
                    index,
                    tint: tint.to_bits(),
                }
            }
        }
        c => with_tint(c, t),
    }
}

fn with_tint(c: CellColor, t: f64) -> CellColor {
    match c {
        CellColor::Theme { index, .. } => CellColor::theme(index, stored_tint(t)),
        CellColor::Rgb(rgb) | CellColor::RgbTint { rgb, .. } => {
            let tint = stored_tint(t);
            if tint == 0.0 {
                CellColor::Rgb(rgb)
            } else {
                CellColor::RgbTint {
                    rgb,
                    tint: tint.to_bits(),
                }
            }
        }
        c => c,
    }
}

fn auto_tint(c: CellColor, t: f64) -> CellColor {
    match c {
        CellColor::Auto | CellColor::AutoTint { .. } => {
            let tint = stored_tint(t);
            if tint == 0.0 {
                CellColor::Auto
            } else {
                CellColor::AutoTint {
                    tint: tint.to_bits(),
                }
            }
        }
        c => with_tint(c, t),
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

fn underline_put(n: i32, prop: &str) -> Result<Underline, OmError> {
    match n {
        1 => Ok(Underline::None),
        3 => Ok(Underline::Double),
        _ => underline_from(n).ok_or_else(|| OmError::unable_to_set(prop, "Font")),
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
const DOT: i32 = -4118;
const DOUBLE: i32 = -4119;
const THIN: i32 = 2;
const MEDIUM: i32 = -4138;

fn line_parts(l: BorderLine) -> (i32, i32) {
    (
        line_code_const(LINE_CODE[l as usize]),
        weight_const(LINE_WEIGHT[l as usize]),
    )
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

fn cell_side_index(index: i32) -> Option<i32> {
    match index {
        -4131 => Some(7),
        -4160 => Some(8),
        -4107 => Some(9),
        -4152 => Some(10),
        _ => None,
    }
}

fn cond_plain_index(index: i32) -> i32 {
    match index {
        7 => -4131,
        8 => -4160,
        9 => -4107,
        10 => -4152,
        i => i,
    }
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

fn odd_edge_cell(rect: Rect, index: i32, multi: bool) -> Option<(u32, u32)> {
    let bottom = match index {
        9 | -4107 => true,
        10 | -4152 => false,
        _ => return None,
    };
    let whole_col = rect.r0 == 0 && rect.r1 == max_rows() - 1;
    let whole_row = rect.c0 == 0 && rect.c1 == max_cols() - 1;
    if multi || (!whole_row && !whole_col && rect != Rect::cell(rect.r0, rect.c0)) {
        return Some((rect.r1, rect.c1));
    }
    match (whole_row, whole_col) {
        (false, true) => Some(if bottom {
            (rect.r0, rect.c0)
        } else {
            (rect.r1, rect.c1)
        }),
        (true, false) => Some(if bottom {
            (rect.r1, rect.c1)
        } else {
            (rect.r0, rect.c0)
        }),
        _ => None,
    }
}

fn side_of(s: &Style, side: Side) -> Border {
    match side {
        Side::Edge(e) => s.borders.edge(e),
        Side::DiagonalDown if s.borders.diagonal_down => s.borders.diagonal,
        Side::DiagonalUp if s.borders.diagonal_up => s.borders.diagonal,
        _ => Border::default(),
    }
}

fn diagonal_base(s: &Style, side: Side) -> Border {
    if !s.borders.diagonal_down && !s.borders.diagonal_up {
        Border {
            line: s.borders.diagonal.line,
            color: CellColor::Auto,
        }
    } else {
        side_of(s, side)
    }
}

fn whole_line(rect: Rect) -> Option<bool> {
    match (rect.is_full_rows(), rect.is_full_columns()) {
        (true, false) => Some(true),
        (false, true) => Some(false),
        _ => None,
    }
}

fn reduced(rect: Rect, kind: Option<bool>) -> Rect {
    match kind {
        Some(true) => Rect::new(rect.r0, 0, rect.r1, 0),
        Some(false) => Rect::new(0, rect.c0, 0, rect.c1),
        None => rect,
    }
}

fn unit(kind: Option<bool>, c: CellAddr) -> Rect {
    match kind {
        Some(true) => Rect::new(c.row, 0, c.row, max_cols() - 1),
        Some(false) => Rect::new(0, c.col, max_rows() - 1, c.col),
        None => Rect::cell(c.row, c.col),
    }
}

fn vstyle(wb: &Workbook, kind: Option<bool>, a: CellAddr) -> &Style {
    match kind {
        Some(row) => wb
            .own_style(a)
            .unwrap_or_else(|| wb.line_style(a.sheet, row, if row { a.row } else { a.col })),
        None => wb.cell_style(a),
    }
}

fn vshown(wb: &Workbook, kind: Option<bool>, a: CellAddr, e: Edge) -> Border {
    let Some(row) = kind else {
        return wb.shown_border(a, e);
    };
    let own = vstyle(wb, kind, a).borders.edge(e);
    let (r, c) = (i64::from(a.row), i64::from(a.col));
    let (nr, nc) = match (row, e) {
        (false, Edge::Left) => (r, c - 1),
        (false, Edge::Right) => (r, c + 1),
        (true, Edge::Top) => (r - 1, c),
        (true, Edge::Bottom) => (r + 1, c),
        _ => return own,
    };
    let (Ok(nr), Ok(nc)) = (u32::try_from(nr), u32::try_from(nc)) else {
        return own;
    };
    if nr >= max_rows() || nc >= max_cols() {
        return own;
    }
    let other = vstyle(wb, kind, CellAddr::new(a.sheet, nr, nc))
        .borders
        .edge(e.opposite());
    wb.merge_sides(own, other)
}

fn cells(sheet: SheetId, rect: Rect) -> impl Iterator<Item = CellAddr> {
    (rect.r0..=rect.r1)
        .flat_map(move |r| (rect.c0..=rect.c1).map(move |c| CellAddr::new(sheet, r, c)))
}

fn line_range_styles(wb: &Workbook, sheet: SheetId, rect: Rect, row: bool) -> Vec<&Style> {
    let (lo, hi) = if row {
        (rect.r0, rect.r1)
    } else {
        (rect.c0, rect.c1)
    };
    let mine: Vec<u32> = wb
        .line_format_indices(sheet, row)
        .into_iter()
        .filter(|i| (lo..=hi).contains(i))
        .collect();
    let mut v = vec![wb.line_style(sheet, row, lo)];
    v.extend(mine.iter().map(|i| wb.line_style(sheet, row, *i)));
    for (cr, cc) in wb.formatted_cells(sheet) {
        let i = if row { cr } else { cc };
        if (lo..=hi).contains(&i) {
            v.push(wb.cell_style(CellAddr::new(sheet, cr, cc)));
        }
    }
    let mut reps = vec![lo];
    reps.extend(mine.iter().take(256));
    for j in wb.line_format_indices(sheet, !row) {
        for &i in &reps {
            let a = if row {
                CellAddr::new(sheet, i, j)
            } else {
                CellAddr::new(sheet, j, i)
            };
            v.push(wb.cell_style(a));
        }
    }
    v
}

fn side_uniform_none(
    wb: &Workbook,
    sheet: SheetId,
    rect: Rect,
    kind: Option<bool>,
    e: Edge,
    shown: bool,
) -> bool {
    match kind {
        Some(row) => line_range_styles(wb, sheet, rect, row)
            .iter()
            .all(|s| s.borders.edge(e).line == BorderLine::None),
        None => cells(sheet, rect).all(|c| {
            let b = if shown {
                wb.shown_border(c, e)
            } else {
                wb.cell_style(c).borders.edge(e)
            };
            b.line == BorderLine::None
        }),
    }
}

fn edge_slot(e: Edge) -> usize {
    match e {
        Edge::Left => 0,
        Edge::Right => 1,
        Edge::Top => 2,
        Edge::Bottom => 3,
    }
}

fn is_colour_prop(p: &str) -> bool {
    matches!(
        p,
        "Color" | "ColorAuto" | "ColorIndex" | "ThemeColor" | "TintAndShade"
    )
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
    outline: Option<bool>,
    shadow: Option<bool>,
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
        "OutlineFont" => b(f.outline),
        "Shadow" => b(f.shadow),
        "Underline" => or_null(f.underline, |u| i4(underline_const(u))),
        "Color" => or_null(f.color, |c| color_variant(theme.rgb(c, 0))),
        "ColorIndex" => or_null(f.color, |c| i4(font_index_of(theme, c))),
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
        "PatternThemeColor" => f
            .pattern_color
            .map_or(i4(-1), |c| i4(theme_index(c).unwrap_or(0))),
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
    if matches!(v.put, Some(Variant::Null))
        && matches!(
            prop,
            "HorizontalAlignment"
                | "VerticalAlignment"
                | "WrapText"
                | "ShrinkToFit"
                | "Locked"
                | "FormulaHidden"
                | "AddIndent"
                | "IndentLevel"
                | "ReadingOrder"
        )
    {
        return Ok(Some(Box::new(|_: &mut Style| {})));
    }
    let worded;
    let v = if matches!(
        prop,
        "WrapText" | "ShrinkToFit" | "Locked" | "FormulaHidden" | "AddIndent"
    ) {
        match &v.put {
            Some(Variant::Bstr(t)) => {
                let b = oper_bool_word(t).ok_or_else(|| OmError::unable_to_set(prop, "Range"))?;
                worded = Args {
                    values: Vec::new(),
                    put: Some(Variant::Bool(b)),
                    lcid: v.lcid,
                };
                &worded
            }
            _ => v,
        }
    } else {
        v
    };
    let n = || app.put_i32(v);

    if let Some(Variant::Bstr(t)) = &v.put {
        if matches!(prop, "HorizontalAlignment" | "VerticalAlignment")
            && !t.trim().parse::<f64>().is_ok_and(|x| x >= 0.0)
        {
            return Err(OmError::unable_to_set(prop, "Range"));
        }
    }
    let b = || app.put_bool(v);
    Ok(Some(match prop {
        "HorizontalAlignment" => {
            let x = enum_arg(v, n().unwrap_or(0));
            let i = H_ALIGN
                .iter()
                .position(|&c| c == x)
                .or_else(|| usize::try_from(x - 1).ok().filter(|&i| i < H_ALIGN.len()))
                .ok_or_else(|| OmError::unable_to_set(prop, "Range"))?;
            let h = HAlign::ALL[i];

            Box::new(move |s: &mut Style| {
                s.alignment.horizontal = h;
                if !matches!(h, HAlign::Left | HAlign::Right | HAlign::Distributed) {
                    s.alignment.indent = 0;
                }
            })
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

                Some(Variant::R8(f)) if f.abs() < 2e9 => f.floor() as i32,
                _ => enum_arg(v, n()?),
            };

            if x == -1 {
                return Ok(Some(Box::new(|_: &mut Style| {})));
            }
            let x = u8::try_from(x)
                .ok()
                .filter(|&x| x <= 250)
                .ok_or_else(OmError::app_defined)?;

            Box::new(move |s: &mut Style| {
                s.alignment.indent = x;

                if x > 0 {
                    s.alignment.rotation = 0;
                }
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
            let r = rotation_from(x).ok_or_else(|| OmError::unable_to_set(prop, "Range"))?;

            Box::new(move |s: &mut Style| {
                if s.alignment.indent == 0 {
                    s.alignment.rotation = r;
                }
            })
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
    if matches!(a.put, Some(Variant::Bool(_))) {
        return Err(OmError::app_defined());
    }
    let x = app.put_f64(a)?.round_ties_even();
    Ok(swap_rb((x as i64 & 0xFF_FFFF) as u32))
}

fn variant_to_int(a: &Args) -> Result<i32, OmError> {
    match variant_i4(a)? {
        i32::MAX => Err(OmError::app_defined()),
        n => Ok(n),
    }
}

fn variant_i4(a: &Args) -> Result<i32, OmError> {
    let v = a.put.clone().unwrap_or(Variant::Empty);
    match xlcalc_variant::change_type(
        &v,
        xlcalc_variant::VarType::I4,
        crate::conv::variant_locale(a.lcid),
        0,
    ) {
        Ok(Variant::I4(n)) => Ok(n),
        Ok(_) => Err(OmError::type_mismatch()),
        Err(hr) => Err(OmError::exception(hr.0, None)),
    }
}

fn colour_put_value(a: &Args, prop: &str) -> Result<Option<Variant>, OmError> {
    if matches!(a.put, Some(Variant::Null)) {
        return Ok(None);
    }
    let n = match (prop, &a.put) {
        ("Color", Some(Variant::Bool(true))) => return Err(OmError::app_defined()),
        ("Color", Some(Variant::Bool(false))) => -1,
        ("Color", _) => variant_to_int(a)?,
        ("ColorIndex", _) => match variant_to_int(a)? {
            NONE | -1 => NONE,
            AUTOMATIC | 0 => AUTOMATIC,
            n @ 1..=56 => n,
            _ => return Err(OmError::bad_index()),
        },
        (_, _) => match variant_to_int(a)? {
            n @ 1..=12 => n,
            _ => return Err(OmError::bad_index()),
        },
    };
    Ok(Some(Variant::I4(n)))
}

fn colour_put_args(o: &StyleOwner, prop: &str, a: &Args) -> Result<Option<Args>, OmError> {
    if !matches!(prop, "Color" | "ColorIndex" | "ThemeColor")
        || !matches!(
            o,
            StyleOwner::Range(_) | StyleOwner::Areas(_) | StyleOwner::Named(..)
        )
    {
        return Ok(Some(a.clone()));
    }
    Ok(colour_put_value(a, prop)?.map(|v| Args {
        values: Vec::new(),
        put: Some(v),
        lcid: a.lcid,
    }))
}

fn oper_bool_word(t: &str) -> Option<bool> {
    if t.eq_ignore_ascii_case("TRUE") {
        Some(true)
    } else if t.eq_ignore_ascii_case("FALSE") {
        Some(false)
    } else {
        None
    }
}

fn font_put(app: &Application, prop: &str, a: &Args, theme: &Theme) -> Result<StyleEdit, OmError> {
    let n = || app.put_i32(a);

    let flag = || match &a.put {
        Some(Variant::Bstr(t)) => {
            oper_bool_word(t).ok_or_else(|| OmError::unable_to_set(prop, "Font"))
        }
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

            if text.is_empty() {
                return Ok(Box::new(|_: &mut Style| {}));
            }
            if text.chars().count() > 31 {
                return Err(OmError::unable_to_set(prop, "Font"));
            }
            let name: Arc<str> = Arc::from(text);
            Box::new(move |s: &mut Style| {
                s.font.name = name.clone();
                s.font.scheme = FontScheme::None;
            })
        }
        "Size" => {
            let x = match &a.put {
                Some(Variant::Empty | Variant::Null) | None => {
                    return Err(OmError::unable_to_set(prop, "Font"))
                }
                Some(Variant::Bstr(t)) => t
                    .trim()
                    .parse::<f64>()
                    .map_err(|_| OmError::unable_to_set(prop, "Font"))?,
                Some(Variant::Bool(b)) => f64::from(u8::from(*b)),
                _ => app.put_f64(a)?,
            };

            let t = (x * 20.0 + 0.5).trunc();
            if !(20.0..=8190.0).contains(&t) {
                return Err(OmError::unable_to_set(prop, "Font"));
            }
            let t = t as u32;
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
        "OutlineFont" => {
            let x = flag()?;
            Box::new(move |s: &mut Style| s.font.outline = x)
        }
        "Shadow" => {
            let x = flag()?;
            Box::new(move |s: &mut Style| s.font.shadow = x)
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

                Some(Variant::Bstr(t)) => match t.trim().parse::<f64>() {
                    Ok(x) if x >= 0.0 => underline_put(x.trunc() as i32, prop)?,
                    _ => return Err(OmError::unable_to_set(prop, "Font")),
                },
                Some(Variant::Empty | Variant::Null) | None => {
                    return Err(OmError::unable_to_set(prop, "Font"))
                }
                Some(Variant::R8(x)) => underline_put(x.trunc() as i32, prop)?,
                _ => underline_put(n()?, prop)?,
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
            let c = if !matches!(a.put, Some(Variant::Bool(_)))
                && app.put_f64(a).is_ok_and(|x| x == -1.0)
            {
                CellColor::Auto
            } else {
                CellColor::Rgb(bgr_arg(app, a)?)
            };
            Box::new(move |s: &mut Style| s.font.color = c)
        }
        "ColorIndex" => {
            if n()? == NONE {
                return Ok(Box::new(|_: &mut Style| {}));
            }
            let c = color_from_index(n()?)?;
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
            Box::new(move |s: &mut Style| s.font.color = font_tint(s.font.color, t))
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

const MPFLSVBA: [(i32, u8); 11] = [
    (NONE, 0),
    (AUTOMATIC, 1),
    (-4121, 7),
    (-4124, 4),
    (-4125, 2),
    (-4126, 3),
    (-4128, 5),
    (-4162, 8),
    (-4166, 6),
    (4000, 40),
    (4001, 40),
];

enum FillColour {
    Set(CellColor),

    Clear,
}

fn fill_colour(a: &Args, icv: bool, theme: bool) -> Result<FillColour, OmError> {
    let boolean = matches!(a.put, Some(Variant::Bool(_)));
    let mut n = variant_i4(a)?;
    let mut index = icv;
    if n == NONE {
        index = true;
        n = 0;
    } else if icv && n == AUTOMATIC {
        n = -1;
    }
    let value = |n: i32| -> Result<FillColour, OmError> {
        Ok(FillColour::Set(if theme && (1..=12).contains(&n) {
            theme_color(n, 0.0)?
        } else if !index && !theme {
            CellColor::Rgb(swap_rb(n as u32 & 0xFF_FFFF))
        } else if index && (1..=56).contains(&n) {
            color_from_index(n)?
        } else {
            return Err(OmError::bad_index());
        }))
    };
    if boolean && !index {
        return value(i32::from(n != 0));
    }
    if !index {
        return value(n);
    }
    match n {
        0 => Ok(FillColour::Clear),
        -1 => Ok(FillColour::Set(CellColor::Auto)),
        _ if boolean => Ok(FillColour::Set(CellColor::Auto)),
        n => value(n),
    }
}

fn sync_ifill(s: &mut Style) {
    if s.fill.pattern == Pattern::None && s.fill.raw.is_none() {
        s.fill.pattern_color = CellColor::Auto;
        s.fill.color = CellColor::Auto;
    }
}

fn no_fill(s: &mut Style) {
    s.fill.pattern = Pattern::None;
    s.fill.raw = None;
    s.fill.cleared = true;
    sync_ifill(s);
}

fn interior_range_put(prop: &str, a: &Args) -> Result<Option<StyleEdit>, OmError> {
    Ok(interior_range_edit(prop, a)?.map(|edit| -> StyleEdit {
        Box::new(move |s: &mut Style| {
            edit(s);
            sync_ifill(s);
        })
    }))
}

fn fill_colour_put_clears(prop: &str, a: &Args) -> Option<bool> {
    let null = matches!(a.put, Some(Variant::Null));
    let pattern = matches!(
        prop,
        "PatternColor" | "PatternColorIndex" | "PatternThemeColor"
    );
    if !(pattern || matches!(prop, "Color" | "ColorIndex" | "ThemeColor")) || (null && pattern) {
        return None;
    }
    if null {
        return Some(true);
    }
    let icv = matches!(prop, "ColorIndex" | "PatternColorIndex");
    let theme = matches!(prop, "ThemeColor" | "PatternThemeColor");
    Some(matches!(fill_colour(a, icv, theme), Ok(FillColour::Clear)))
}

fn interior_range_edit(prop: &str, a: &Args) -> Result<Option<StyleEdit>, OmError> {
    let null = matches!(a.put, Some(Variant::Null));
    let zero;
    let a = if null && matches!(prop, "Color" | "ColorIndex" | "ThemeColor") {
        zero = Args {
            values: Vec::new(),
            put: Some(Variant::I4(0)),
            lcid: a.lcid,
        };
        &zero
    } else {
        a
    };
    let pattern_member = matches!(
        prop,
        "PatternColor" | "PatternColorIndex" | "PatternThemeColor"
    );
    Ok(Some(match prop {
        "Color" | "ColorIndex" | "ThemeColor" | "PatternColor" | "PatternColorIndex"
        | "PatternThemeColor" => {
            if matches!(a.put, Some(Variant::Null)) {
                return Ok(None);
            }
            let icv = matches!(prop, "ColorIndex" | "PatternColorIndex");
            let theme = matches!(prop, "ThemeColor" | "PatternThemeColor");
            match fill_colour(a, icv, theme)? {
                FillColour::Clear => Box::new(|s: &mut Style| s.fill = Fill::default()),
                FillColour::Set(c) => Box::new(move |s: &mut Style| {
                    if s.fill.pattern == Pattern::None {
                        s.fill.pattern = Pattern::Solid;
                        s.fill.cleared = false;
                    }
                    if pattern_member {
                        s.fill.pattern_color = c;
                    } else {
                        s.fill.color = c;
                    }
                }),
            }
        }

        "Pattern" => {
            if null {
                return Ok(None);
            }
            let n = variant_i4(a)?;
            let id = if n == -1 {
                1
            } else {
                MPFLSVBA
                    .iter()
                    .find(|(c, _)| *c == n)
                    .map_or(n, |(_, f)| i32::from(*f))
            };
            if id == 40 {
                return Err(OmError::app_defined());
            }
            let p = usize::try_from(id)
                .ok()
                .and_then(|i| Pattern::ALL.get(i).copied())
                .ok_or_else(OmError::bad_index)?;
            Box::new(move |s: &mut Style| {
                if p == Pattern::None {
                    no_fill(s);
                } else {
                    s.fill.pattern = p;
                    s.fill.cleared = false;
                }
            })
        }
        _ => return Err(OmError::app_defined()),
    }))
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

    if matches!(a.put, Some(Variant::Bool(false)))
        && matches!(prop, "ThemeColor" | "PatternThemeColor")
    {
        return Err(OmError::bad_index());
    }
    if let Some(Variant::R8(x)) = &a.put {
        if x.abs() > f64::from(i32::MAX)
            && matches!(prop, "Pattern" | "ThemeColor" | "PatternThemeColor")
        {
            return Err(OmError::exception(HResult::DISP_E_OVERFLOW.0, None));
        }
    }

    if ignored && matches!(prop, "Color" | "PatternColor") {
        let c = CellColor::Rgb(swap_rb(1));
        return Ok(if prop == "Color" {
            colour(c)
        } else {
            pattern_colour(c)
        });
    }
    if ignored && matches!(prop, "ThemeColor" | "PatternThemeColor") {
        let c = theme_color(1, 0.0)?;
        return Ok(if prop == "ThemeColor" {
            colour(c)
        } else {
            pattern_colour(c)
        });
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
        "Color" => colour(CellColor::Rgb(swap_rb(
            (app.put_f64(a)?.round_ties_even() as i64 & 0xFF_FFFF) as u32,
        ))),
        "ColorIndex" if nothing => Box::new(|s: &mut Style| s.fill = Fill::default()),
        "ColorIndex" => match n()? {
            NONE | 0 => Box::new(|s: &mut Style| s.fill = Fill::default()),
            i => colour(color_from_index(i).map_err(|_| OmError::bad_index())?),
        },
        "ThemeColor" => colour(theme_color(n()?, 0.0)?),
        "TintAndShade" => {
            let t = tint_arg(a)?;

            Box::new(move |s: &mut Style| {
                s.fill.color = auto_tint(s.fill.color, t);
                sync_ifill(s);
            })
        }

        "Pattern" => match n()? {
            NONE | 0 => Box::new(|s: &mut Style| {
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
        "PatternColorIndex" if matches!(n(), Ok(NONE | 0)) => {
            Box::new(|s: &mut Style| s.fill = Fill::default())
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
            let t = tint_arg(a)?;
            Box::new(move |s: &mut Style| {
                s.fill.pattern_color = auto_tint(s.fill.pattern_color, t);
                sync_ifill(s);
            })
        }
        _ => return Err(OmError::app_defined()),
    })
}

fn border_int(app: &Application, a: &Args, prop: &str) -> Result<Option<i32>, OmError> {
    match &a.put {
        Some(Variant::Bool(_)) if prop == "ThemeColor" => Err(OmError::bad_index()),
        Some(Variant::Bool(_)) => Ok(None),
        None | Some(Variant::Empty | Variant::Null) => match prop {
            "ColorIndex" => Ok(Some(AUTOMATIC)),
            _ => Err(OmError::bad_index()),
        },
        Some(Variant::R8(x)) if x.abs() > f64::from(i32::MAX) => {
            Err(OmError::exception(HResult::DISP_E_OVERFLOW.0, None))
        }
        _ => app.put_i32(a).map(Some),
    }
}

const APPLYLS: [u8; 104] = [
    1, 3, 4, 9, 11, 6, 13, 1, 3, 4, 9, 11, 6, 13, 2, 8, 4, 10, 12, 6, 13, 1, 3, 4, 9, 11, 6, 13, 1,
    3, 4, 9, 11, 6, 13, 5, 3, 4, 9, 11, 6, 13, 5, 3, 4, 9, 11, 6, 13, 7, 3, 4, 9, 11, 6, 13, 2, 8,
    4, 10, 12, 6, 13, 1, 3, 4, 9, 11, 6, 13, 2, 8, 4, 10, 12, 6, 13, 1, 3, 4, 9, 11, 6, 13, 2, 8,
    4, 10, 12, 6, 13, 2, 8, 4, 10, 12, 6, 13, 0, 0, 7, 1, 2, 5,
];

const WEIGHT_LINE: [[u8; 4]; 14] = [
    [7, 1, 2, 5],
    [7, 1, 2, 5],
    [7, 1, 2, 5],
    [7, 3, 8, 5],
    [7, 4, 2, 5],
    [7, 1, 2, 5],
    [7, 1, 2, 6],
    [7, 1, 2, 5],
    [7, 3, 8, 5],
    [7, 9, 10, 5],
    [7, 9, 10, 5],
    [7, 11, 12, 5],
    [7, 11, 12, 5],
    [7, 1, 13, 5],
];

const LINE_CODE: [i32; 14] = [0, 1, 1, 2, 3, 1, 9, 1, 2, 4, 4, 5, 5, 13];

const LINE_WEIGHT: [i32; 14] = [2, 2, 3, 2, 2, 4, 4, 1, 3, 2, 3, 2, 3, 3];

fn line_code_const(code: i32) -> i32 {
    match code {
        0 => NONE,
        2 => DASH,
        3 => DOT,
        9 => DOUBLE,
        c => c,
    }
}

fn weight_const(w: i32) -> i32 {
    if w == 3 {
        MEDIUM
    } else {
        w
    }
}

fn line_arg(a: &Args, prop: &str) -> Result<i32, OmError> {
    let refused = || OmError::app_defined();
    let n = match &a.put {
        Some(Variant::Bstr(t)) => {
            let x = t
                .trim()
                .parse::<f64>()
                .ok()
                .filter(|x| x.is_finite())
                .ok_or_else(|| OmError::unable_to_set(prop, "Border"))?;
            let n = x.floor().clamp(f64::from(i32::MIN), f64::from(i32::MAX)) as i32;
            let ok = if prop == "LineStyle" {
                (0..=13).contains(&n)
            } else {
                (1..=4).contains(&n)
            };
            return if ok { Ok(n) } else { Err(refused()) };
        }
        Some(Variant::Bool(x)) => i32::from(*x),
        None | Some(Variant::Empty | Variant::Null) => 0,
        Some(v) => {
            let x = match v {
                Variant::R8(x) => *x,
                Variant::R4(x) => f64::from(*x),
                Variant::I4(x) => f64::from(*x),
                Variant::I2(x) => f64::from(*x),
                Variant::Ui1(x) => f64::from(*x),
                _ => return Err(OmError::type_mismatch()),
            };
            x.floor().clamp(f64::from(i32::MIN), f64::from(i32::MAX)) as i32
        }
    };
    if prop == "LineStyle" {
        let code = match n {
            NONE => 0,
            DASH => 2,
            DOT => 3,
            DOUBLE => 9,
            n => n,
        };
        if (0..=13).contains(&code) {
            Ok(code)
        } else {
            Err(refused())
        }
    } else {
        let w = if n == MEDIUM { 3 } else { n };
        if (1..=4).contains(&w) {
            Ok(w)
        } else {
            Err(refused())
        }
    }
}

fn line_style_put(cur: BorderLine, code: i32) -> BorderLine {
    let column = match code {
        1 => 0,
        2 => 1,
        3 => 2,
        4 => 3,
        5 => 4,
        9 => 5,
        13 => 6,
        c => c as usize,
    };
    BorderLine::ALL[usize::from(APPLYLS[cur as usize * 7 + column])]
}

fn weight_put(cur: BorderLine, w: i32) -> BorderLine {
    BorderLine::ALL[usize::from(WEIGHT_LINE[cur as usize][w as usize - 1])]
}

#[derive(Clone, Copy)]
struct EdgePut {
    clear: bool,
    whole: Option<[CellColor; 4]>,
    cur: Option<BorderLine>,
}

impl EdgePut {
    fn single() -> Self {
        EdgePut {
            clear: true,
            whole: None,
            cur: None,
        }
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
            let code = line_arg(a, prop)?;

            Box::new(move |b: Border| {
                if code == 0 {
                    Border::default()
                } else {
                    Border {
                        line: line_style_put(b.line, code),
                        color: b.color,
                    }
                }
            })
        }
        "Weight" => {
            let w = line_arg(a, prop)?;
            Box::new(move |b: Border| Border {
                line: weight_put(b.line, w),
                color: b.color,
            })
        }

        "Color"
            if !matches!(a.put, Some(Variant::Bool(_)))
                && app.put_f64(a).is_ok_and(|x| x == -1.0) =>
        {
            Box::new(|b| b)
        }
        "Color" => colour(CellColor::Rgb(bgr_arg(app, a)?)),
        "ColorIndex" => match border_int(app, a, prop)? {
            None => Box::new(|b| b),

            Some(NONE) => Box::new(|b| b),

            Some(AUTOMATIC) => colour(CellColor::Indexed(64)),
            Some(i) => colour(color_from_index(i)?),
        },
        "ThemeColor" => match border_int(app, a, prop)? {
            None => Box::new(|b| b),
            Some(i) => colour(theme_color(i, 0.0)?),
        },

        "ColorNone" => Box::new(|_| Border::default()),

        "ColorAuto" => Box::new(|b: Border| {
            if b.line == BorderLine::None {
                Border {
                    line: BorderLine::Thin,
                    color: CellColor::Indexed(64),
                }
            } else {
                b
            }
        }),
        "TintAndShade" => {
            let t = tint_arg(a)?;

            Box::new(move |b: Border| {
                if b.line == BorderLine::None {
                    Border {
                        line: BorderLine::Thin,
                        color: auto_tint(CellColor::Auto, t),
                    }
                } else {
                    Border {
                        color: with_tint(b.color, t),
                        ..b
                    }
                }
            })
        }
        _ => return Err(OmError::app_defined()),
    })
}

impl Application<'_> {
    fn is_builtin_style(&self, o: &StyleOwner) -> bool {
        let StyleOwner::Named(b, n) = o else {
            return false;
        };
        self.book(*b).is_ok_and(|bk| {
            bk.wb
                .style_table()
                .get(n)
                .is_some_and(|s| s.builtin_id.is_some())
        })
    }
}

impl Application<'_> {
    pub(crate) fn tab_member(
        &mut self,
        book: BookId,
        sheet: SheetId,
        m: &'static Member,
        a: &Args,
    ) -> Result<Variant, OmError> {
        let key = format!("ws{}:{}:TabRgb", book.0, sheet.0);
        let cur = match self.opts.get(&key) {
            Some(Variant::I4(n)) => Some(*n as u32),
            _ => None,
        };
        let theme = self.book(book)?.wb.style_table().theme.clone();
        let tkey = format!("ws{}:{}:TabTheme", book.0, sheet.0);
        let skey = format!("ws{}:{}:TabTint", book.0, sheet.0);
        let tint = match self.opts.get(&skey) {
            Some(Variant::R8(t)) => *t,
            _ => 0.0,
        };
        if m.kind != InvokeKind::Put {
            if m.name == "ThemeColor" {
                return Ok(match (self.opts.get(&tkey), cur) {
                    (Some(Variant::I4(t)), Some(_)) => Variant::I4(*t),
                    _ => Variant::I4(0),
                });
            }
            return Ok(match (m.name, cur) {
                ("Color" | "TintAndShade", None) => Variant::Bool(false),
                ("Color", Some(c)) => Variant::I4(swap_rb(c) as i32),
                ("ColorIndex", None) => i4(NONE),
                ("ColorIndex", Some(c)) => i4(i32::from(theme.pcv_color_index(c))),
                ("TintAndShade", Some(_)) => Variant::R8(tint),
                _ => Variant::Empty,
            });
        }
        let new = match m.name {
            "Color" => match a.put {
                Some(Variant::Bool(_)) => None,
                _ => Some(bgr_arg(self, a)?),
            },
            "ColorIndex" => {
                if matches!(a.put, Some(Variant::Null | Variant::Empty) | None) {
                    return Err(OmError::bad_index());
                }
                let n = Application::put_value(a).i32(0).map_err(|e| {
                    if e.scode == crate::error::SCODE_TYPE_MISMATCH {
                        OmError::arg(crate::error::DISP_E_TYPEMISMATCH, 0)
                    } else {
                        e
                    }
                })?;
                match n.unwrap_or(0) {
                    n if n == NONE => None,
                    n => Some(theme.rgb(color_from_index(n)?, 0)),
                }
            }
            "ThemeColor" => Some(theme.rgb_cv(theme_color(self.put_i32(a)?, tint)?, 0)),
            "TintAndShade" => {
                if matches!(a.put, Some(Variant::Null | Variant::Empty) | None) {
                    return Err(OmError::exception(0x8007_0057_u32 as i32, None));
                }

                let t = self.put_f64(a)?;
                let count = ((t * 32767.0).trunc() as i64) as i16;
                let t = f64::from(count) / 32767.0;
                self.opts.insert(skey, Variant::R8(t));
                if let (Some(Variant::I4(th)), Some(_)) = (self.opts.get(&tkey).cloned(), cur) {
                    let c = theme.rgb_cv(theme_color(th, t)?, 0);
                    self.opts.insert(key, Variant::I4(c as i32));
                }
                return Ok(Variant::Empty);
            }
            _ => return Ok(Variant::Empty),
        };
        if m.name != "ThemeColor" {
            self.opts.remove(&skey);
        }
        if m.name == "ThemeColor" {
            let t = self.put_i32(a)?;
            self.opts.insert(tkey, Variant::I4(t));
        } else {
            self.opts.remove(&tkey);
        }
        match new {
            Some(c) => self.opts.insert(key, Variant::I4(c as i32)),
            None => self.opts.remove(&key),
        };
        Ok(Variant::Empty)
    }

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

    pub(crate) fn unhyperlink_style(
        &mut self,
        book: BookId,
        sheet: SheetId,
        rect: Rect,
    ) -> Result<(), OmError> {
        let wb = &mut self.book_mut(book)?.wb;
        let Some(ns) = wb.style_table().get("Normal").cloned() else {
            return Ok(());
        };
        for r in rect.r0..=rect.r1 {
            for c in rect.c0..=rect.c1 {
                if &*wb.cell_style(CellAddr::new(sheet, r, c)).named != "Hyperlink" {
                    continue;
                }
                wb.set_style_with(sheet, Rect::new(r, c, r, c), &|s: &Style| {
                    let mut s = s.clone();
                    apply_named(&mut s, &ns);
                    s.named = ns.name.clone();
                    s
                });
            }
        }
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
            StyleOwner::Areas(rs) => rs.first().map_or(BookId(0), |r| r.book),
            StyleOwner::Named(b, _) => *b,
            StyleOwner::Display(r) => r.book,
            StyleOwner::Cond(b, ..) => *b,
        }
    }

    fn named_style(&self, b: BookId, name: &str) -> Result<&NamedStyle, OmError> {
        let t = self.book(b)?.wb.style_table();
        t.position(name)
            .map(|i| &t.named[i])
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
                Ok(agree(wb.range_styles(sheet, rect).into_iter().map(f)))
            }
            StyleOwner::Areas(rs) => {
                let mut all = Vec::new();
                for r in rs {
                    let (sheet, rect) = self.resolve(r)?;
                    let wb = &self.book(r.book)?.wb;
                    all.extend(wb.range_styles(sheet, rect).into_iter().map(&f));
                }
                Ok(agree(all.into_iter()))
            }
            StyleOwner::Named(b, n) => Ok(Some(f(&self.named_style(*b, n)?.style))),
            StyleOwner::Display(r) => {
                let (sheet, rect) = self.resolve(r)?;
                let wb = &self.book(r.book)?.wb;
                Ok(agree(cells(sheet, rect).map(|a| f(&wb.display_style(a)))))
            }
            StyleOwner::Cond(b, s, id, _) => {
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
        let StyleOwner::Cond(b, s, id, _) = o else {
            return None;
        };
        let sh = self.book(*b).ok()?.wb.cfv_sheet(*s)?;
        sh.index_of(*id).map(|i| sh.rules[i].dxf.mask)
    }

    fn edit_cond_dxf(
        &mut self,
        o: &StyleOwner,
        f: impl FnOnce(&mut xlcalc::cfv::Dxf),
    ) -> Result<(), OmError> {
        let StyleOwner::Cond(b, s, id, _) = o else {
            return Err(OmError::app_defined());
        };
        let sh = self.book_mut(*b)?.wb.cfv_sheet_mut(*s);
        let i = sh.index_of(*id).ok_or_else(OmError::app_defined)?;
        f(&mut sh.rules[i].dxf);
        Ok(())
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
            StyleOwner::Areas(rs) => {
                for r in rs {
                    self.edit_style(&StyleOwner::Range(*r), f)?;
                }
            }
            StyleOwner::Display(_) => return Err(OmError::app_defined()),
            StyleOwner::Cond(b, s, id, _) => {
                let wb = &mut self.book_mut(*b)?.wb;
                let sh = wb.cfv_sheet_mut(*s);
                let i = sh.index_of(*id).ok_or_else(OmError::app_defined)?;
                let dxf = &mut sh.rules[i].dxf;
                let (a, b2) = dxf_probe(f);
                merge_dxf(dxf, &a, &b2);
                wb.cfv_format_made(*s, *id);
            }
            StyleOwner::Named(b, n) => {
                let mut ns = self.named_style(*b, n)?.clone();
                let old = ns.style.clone();
                f(&mut ns.style);
                let book = self.book_mut(*b)?;
                let (wb, views) = (&mut book.wb, &book.window.views);

                wb.restyle_named(&ns.name, &old, &ns.style);

                if ns.name.eq_ignore_ascii_case("Normal") {
                    wb.set_normal_style(ns.style.clone(), &|id| {
                        views.get(&id).map_or(100.0, |v| v.zoom)
                    });
                }
                wb.style_table_mut().replace(ns);
            }
        }
        Ok(())
    }

    fn style_border_owner(&mut self, o: &StyleOwner, member: &str) -> Result<StyleOwner, OmError> {
        let target = self.font_put_owner(o, member)?;
        if let (StyleOwner::Named(from, name), StyleOwner::Named(to, _)) = (o, &target) {
            if from != to {
                let borders = self.named_style(*from, name)?.style.borders.clone();
                self.edit_style(&target, &|s: &mut Style| s.borders = borders.clone())?;
            }
        }
        Ok(target)
    }

    fn font_put_owner(&mut self, o: &StyleOwner, member: &str) -> Result<StyleOwner, OmError> {
        let StyleOwner::Named(b, name) = o else {
            return Ok(o.clone());
        };
        let colour = matches!(
            member,
            "Color" | "ColorIndex" | "ThemeColor" | "TintAndShade" | "ThemeFont"
        );
        let Some(active) = self.active.filter(|active| !colour && active != b) else {
            return Ok(o.clone());
        };
        let builtin_id = self.named_style(*b, name)?.builtin_id;
        let table = self.book_mut(active)?.wb.style_table_mut();
        let found = table.position(name).map(|i| table.named[i].name.clone());
        let name = match found {
            Some(found) => found,
            None => {
                let mut style = table
                    .get("Normal")
                    .map(|n| n.style.clone())
                    .unwrap_or_default();
                style.named = name.clone();
                table.insert(NamedStyle {
                    name: name.clone(),
                    builtin_id,
                    style,
                    include_number: true,
                    include_font: true,
                    include_alignment: true,
                    include_border: true,
                    include_patterns: true,
                    include_protection: true,
                });
                name.clone()
            }
        };
        Ok(StyleOwner::Named(active, name))
    }

    fn font_put_widen(&mut self, o: &StyleOwner) -> Result<(), OmError> {
        if self.chars_font_put {
            return Ok(());
        }
        let areas = match o {
            StyleOwner::Range(r) => vec![*r],
            StyleOwner::Areas(rs) => rs.clone(),
            _ => Vec::new(),
        };
        for r in areas {
            let (sheet, rect) = self.resolve(&r)?;
            let wb = &mut self.book_mut(r.book)?.wb;
            wb.widen_for_font(sheet, rect);
            let fitted = wb.widen_for_indent(sheet, rect);
            self.widen_kept
                .extend(fitted.into_iter().map(|(r, t)| (sheet, r, t)));
        }
        Ok(())
    }

    fn cond_put(
        &mut self,
        o: &StyleOwner,
        m: &'static Member,
        own: bool,
        run: impl FnOnce(&mut Self) -> Result<Variant, OmError>,
    ) -> Result<Variant, OmError> {
        let put = matches!(m.kind, InvokeKind::Put | InvokeKind::PutRef);
        let before = match (o, put) {
            (StyleOwner::Cond(b, s, id, _), true) => self
                .book(*b)?
                .wb
                .cfv_sheet(*s)
                .and_then(|sh| sh.index_of(*id).map(|i| sh.rules[i].clone())),
            _ => None,
        };
        let out = run(self)?;
        if let (StyleOwner::Cond(b, s, id, scope), Some(old)) = (o, before) {
            let wb = &mut self.book_mut(*b)?.wb;
            if own {
                wb.cfv_put_format(*s, *id);
            } else {
                wb.cfv_put_on_scope(*s, *id, scope, Some(&old), |_| {});
            }
        }
        Ok(out)
    }

    pub(crate) fn font_member(
        &mut self,
        o: &StyleOwner,
        m: &'static Member,
        a: &Args,
    ) -> Result<Variant, OmError> {
        let own = is_colour_prop(m.name) || m.name == "ThemeFont";
        self.cond_put(o, m, own, |app| app.font_member_core(o, m, a))
    }

    fn font_member_core(
        &mut self,
        o: &StyleOwner,
        m: &'static Member,
        a: &Args,
    ) -> Result<Variant, OmError> {
        let mut theme = self
            .book(self.owner_book(o))?
            .wb
            .style_table()
            .theme
            .clone();

        if m.name == "ColorIndex" && matches!(o, StyleOwner::Cond(..)) {
            theme.palette = xlcalc::style::PALETTE;
        }

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

        let canon;

        if put
            && matches!(o, StyleOwner::Cond(..))
            && is_colour_prop(m.name)
            && m.name != "TintAndShade"
        {
            let c = colour_put_value(a, m.name)?;
            let cleared = match &c {
                None => true,
                Some(Variant::I4(NONE)) => m.name == "ColorIndex",
                Some(_) => false,
            };
            if cleared {
                if let StyleOwner::Cond(b, s, id, _) = o {
                    let sh = self.book_mut(*b)?.wb.cfv_sheet_mut(*s);
                    let i = sh.index_of(*id).ok_or_else(OmError::app_defined)?;
                    sh.rules[i].dxf.mask.font_color = false;
                    self.book_mut(*b)?.wb.cfv_format_made(*s, *id);
                }
                return Ok(Variant::Empty);
            }
            if let Some(v) = c {
                canon = Args {
                    values: Vec::new(),
                    put: Some(v),
                    lcid: a.lcid,
                };
                a = &canon;
            }
        } else if put {
            match colour_put_args(o, m.name, a)? {
                None => {
                    if let StyleOwner::Named(b, n) = o {
                        self.edit_style(o, &|s: &mut Style| s.font.color = CellColor::Rgb(0))?;
                        self.mixed_style_color.remove(&(*b, n.clone()));
                    }
                    return Ok(Variant::Empty);
                }
                Some(c) => {
                    canon = c;
                    a = &canon;
                }
            }
        }

        if put && matches!(o, StyleOwner::Cond(..)) && m.name == "TintAndShade" {
            let t = tint_arg(a)?;
            self.edit_cond_dxf(o, |d| {
                if d.mask.font_color {
                    d.style.font.color = font_tint(d.style.font.color, t);
                }
            })?;
            return Ok(Variant::Empty);
        }

        if put
            && matches!(o, StyleOwner::Cond(..))
            && matches!(
                m.name,
                "Name"
                    | "Size"
                    | "OutlineFont"
                    | "Shadow"
                    | "Superscript"
                    | "Subscript"
                    | "Background"
            )
        {
            return Err(OmError::unable_to_set(m.name, "Font"));
        }

        let listed;
        if put && matches!(o, StyleOwner::Cond(..)) && m.name == "Underline" {
            if matches!(a.put, Some(Variant::Null)) {
                return Ok(Variant::Empty);
            }
            let code = match &a.put {
                Some(Variant::Bool(b)) => Some(if *b { 2.0 } else { 1.0 }),
                Some(Variant::Bstr(t)) => t.trim().parse::<f64>().ok(),
                Some(Variant::R8(x)) => Some(match *x {
                    -4142.0 => 1.0,
                    -4119.0 => 3.0,
                    v => v,
                }),
                Some(Variant::I4(x)) => Some(match *x {
                    -4142 => 1.0,
                    -4119 => 3.0,
                    v => f64::from(v),
                }),
                _ => None,
            };
            if let Some(x) = code {
                let n = x.trunc();
                if !(1.0..=3.0).contains(&n) {
                    return Err(OmError::unable_to_set(m.name, "Font"));
                }
                listed = Args {
                    values: Vec::new(),
                    put: Some(Variant::I4(match n as i32 {
                        1 => -4142,
                        3 => -4119,
                        k => k,
                    })),
                    lcid: a.lcid,
                };
                a = &listed;
            }
        }

        let worded;
        if put && matches!(o, StyleOwner::Cond(..)) && matches!(m.name, "Bold" | "Italic") {
            if let Some(Variant::Bstr(t)) = &a.put {
                let Some(b) = oper_bool_word(t) else {
                    self.fc_poisoned = true;
                    return Err(OmError::unable_to_set(m.name, "Font"));
                };
                worded = Args {
                    values: Vec::new(),
                    put: Some(Variant::Bool(b)),
                    lcid: a.lcid,
                };
                a = &worded;
            }
        }
        if matches!(o, StyleOwner::Display(_)) || m.name == "Background" {
            if let Some(r) = display_font_special(m, a) {
                if put && r.is_ok() {
                    self.font_put_widen(o)?;
                }
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
                    outline: self.agree_on(o, |s| s.font.outline)?,
                    shadow: self.agree_on(o, |s| s.font.shadow)?,
                    color: self.agree_on(o, |s| s.font.color)?,
                    scheme: self.agree_on(o, |s| s.font.scheme)?,
                };

                if f.sup.is_none() || f.sub.is_none() {
                    f.sup = None;
                    f.sub = None;
                }

                if let Some(mk) = self.cond_mask(o) {
                    let style = mk.bold || mk.italic;

                    f.name = None;
                    f.size = None;
                    f.outline = None;
                    f.shadow = None;
                    f.scheme = f.scheme.filter(|_| mk.theme_font);
                    f.sup = None;
                    f.sub = None;
                    f.bold = f.bold.filter(|_| style);
                    f.italic = f.italic.filter(|_| style);
                    f.underline = f.underline.filter(|_| mk.underline);
                    f.strike = f.strike.filter(|_| mk.strike);
                    f.color = f.color.filter(|_| mk.font_color);
                }
                if let StyleOwner::Named(b, n) = o {
                    if self.mixed_style_color.contains(&(*b, n.clone())) {
                        f.color = None;
                    }
                }

                if self.is_builtin_style(o) {
                    f.outline = f.outline.map(|_| false);
                    f.shadow = f.shadow.map(|_| false);
                }

                if let StyleOwner::Named(b, n) = o {
                    if !self.named_style(*b, n)?.include_font {
                        f.sup = f.sup.map(|_| false);
                        f.sub = f.sub.map(|_| false);
                    }
                }
                font_prop(&theme, &f, m)
            }
            _ => {
                self.style_guard(o, "Font", m.name)?;

                let named_none = matches!(o, StyleOwner::Named(..))
                    && m.name == "ColorIndex"
                    && matches!(a.put, Some(Variant::I4(NONE)));
                let edit: Box<dyn Fn(&mut Style)> = if named_none {
                    Box::new(|s: &mut Style| s.font.color = CellColor::Auto)
                } else {
                    font_put(self, m.name, a, &theme)?
                };
                let target = self.font_put_owner(o, m.name)?;
                let o = &target;
                self.edit_style(o, &*edit)?;
                if !matches!(m.name, "Bold" | "Italic" | "Color" | "ColorIndex") {
                    self.font_put_widen(o)?;
                }
                if let StyleOwner::Named(b, n) = o {
                    let key = (*b, n.clone());
                    if named_none {
                        self.mixed_style_color.insert(key);
                    } else if matches!(m.name, "Color" | "ColorIndex" | "ThemeColor") {
                        self.mixed_style_color.remove(&key);
                    }
                }
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
        self.cond_put(o, m, true, |app| app.interior_member_core(o, m, a))
    }

    fn interior_member_core(
        &mut self,
        o: &StyleOwner,
        m: &'static Member,
        a: &Args,
    ) -> Result<Variant, OmError> {
        let mut theme = self
            .book(self.owner_book(o))?
            .wb
            .style_table()
            .theme
            .clone();

        if m.name == "ColorIndex" && matches!(o, StyleOwner::Cond(..)) {
            theme.palette = xlcalc::style::PALETTE;
        }
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

                    let none_read = (mk.fill_none
                        && f.color.is_some()
                        && matches!(m.name, "ColorIndex" | "ThemeColor"))
                        || (mk.pattern_color_none
                            && f.pattern_color.is_some()
                            && matches!(m.name, "PatternColorIndex" | "PatternThemeColor"));
                    if none_read {
                        return Ok(i4(NONE));
                    }
                }
                interior_prop(&theme, &f, m)
            }
            _ => {
                self.style_guard(o, "Interior", m.name)?;

                let range_member = matches!(
                    m.name,
                    "Color"
                        | "ColorIndex"
                        | "ThemeColor"
                        | "Pattern"
                        | "PatternColor"
                        | "PatternColorIndex"
                        | "PatternThemeColor"
                );
                let cond = matches!(o, StyleOwner::Cond(..));

                if cond && matches!(m.name, "TintAndShade" | "PatternTintAndShade") {
                    let t = tint_arg(a)?;
                    let pattern = m.name == "PatternTintAndShade";
                    self.edit_cond_dxf(o, |d| {
                        if pattern {
                            d.style.fill.pattern_color = auto_tint(d.style.fill.pattern_color, t);
                            d.mask.pattern_color = true;
                        } else {
                            d.style.fill.color = auto_tint(d.style.fill.color, t);
                            d.mask.fill_color = true;
                        }
                    })?;
                    return Ok(Variant::Empty);
                }
                if range_member
                    && (cond
                        || matches!(
                            o,
                            StyleOwner::Range(_) | StyleOwner::Areas(_) | StyleOwner::Named(..)
                        ))
                {
                    if let Some(edit) = interior_range_put(m.name, a)? {
                        let keep_none = m.name == "PatternColorIndex"
                            && matches!(o, StyleOwner::Named(..))
                            && !matches!(a.put, Some(Variant::Null));
                        if keep_none {
                            self.edit_style(o, &|s: &mut Style| {
                                let none = s.fill.pattern == Pattern::None && !s.fill.cleared;
                                edit(s);
                                if none {
                                    s.fill.pattern = Pattern::None;
                                    sync_ifill(s);
                                }
                            })?;
                        } else if cond
                            && m.name.starts_with("Pattern")
                            && fill_colour_put_clears(m.name, a) == Some(true)
                        {
                            self.edit_cond_dxf(o, |d| {
                                d.style.fill.pattern_color = CellColor::Auto;
                                d.mask.pattern_color = true;
                            })?;
                        } else {
                            self.edit_style(o, &*edit)?;
                        }
                    }
                    if let StyleOwner::Cond(b, s, id, _) = o {
                        if let Some(none) = fill_colour_put_clears(m.name, a) {
                            let sh = self.book_mut(*b)?.wb.cfv_sheet_mut(*s);
                            if let Some(i) = sh.index_of(*id) {
                                let mk = &mut sh.rules[i].dxf.mask;
                                if m.name.starts_with("Pattern") {
                                    mk.pattern_color_none = none;
                                } else {
                                    mk.fill_none = none;
                                }
                            }
                        }

                        let null_pattern =
                            m.name == "Pattern" && matches!(a.put, Some(Variant::Null));
                        if matches!(m.name, "Pattern" | "ThemeColor") && !null_pattern {
                            let sh = self.book_mut(*b)?.wb.cfv_sheet_mut(*s);
                            if let Some(i) = sh.index_of(*id) {
                                sh.rules[i].dxf.mask.pattern_read = true;
                            }
                        }
                    }
                    return Ok(Variant::Empty);
                }
                let edit = interior_put(self, m.name, a)?;
                self.edit_style(o, &*edit)?;
                if let StyleOwner::Cond(b, s, id, _) = o {
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
        let own = is_colour_prop(m.name);
        self.cond_put(o, m, own, |app| app.borders_member_core(o, m, a))
    }

    fn borders_member_core(
        &mut self,
        o: &StyleOwner,
        m: &'static Member,
        a: &Args,
    ) -> Result<Variant, OmError> {
        use InvokeKind::*;
        if let (StyleOwner::Areas(rs), true) = (o, !matches!(m.kind, Get)) {
            for r in rs {
                self.borders_member(&StyleOwner::Range(*r), m, a)?;
            }
            return Ok(Variant::Empty);
        }
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
            ("Count", Get) => Ok(i4(match o {
                StyleOwner::Cond(b, sh, id, _) => {
                    let top10 = self.book(*b)?.wb.cfv_sheet(*sh).is_some_and(|c| {
                        c.index_of(*id).is_some_and(|i| {
                            matches!(c.rules[i].kind, xlcalc::cfv::CfKind::Top10 { .. })
                        })
                    });
                    if top10 {
                        6
                    } else {
                        4
                    }
                }
                _ => 6,
            })),
            ("_NewEnum", Get) => {
                let owner = o.clone();
                let items = [7, 8, 9, 10, 11, 12]
                    .into_iter()
                    .map(|i| self.alloc(Object::Border(owner.clone(), i)))
                    .collect();
                Ok(self.alloc_enumerator(items))
            }
            (prop, Get) if matches!(o, StyleOwner::Range(_) | StyleOwner::Areas(_)) => {
                let prop = if prop == "Value" { "LineStyle" } else { prop };
                self.borders_collection_get(o, prop)
            }
            (prop, Get) => {
                let prop = if prop == "Value" { "LineStyle" } else { prop };

                let mut seen: Option<Variant> = None;
                for i in [7, 8, 9, 10, 11, 12] {
                    if i > 10 && matches!(o, StyleOwner::Named(..) | StyleOwner::Cond(..)) {
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
                let numeric;
                let a = match (&a.put, prop) {
                    (Some(Variant::Bstr(t)), "Value") if t.trim().parse::<f64>().is_ok() => {
                        numeric = Args {
                            values: Vec::new(),
                            put: Some(Variant::R8(t.trim().parse::<f64>().unwrap_or(0.0))),
                            lcid: a.lcid,
                        };
                        &numeric
                    }
                    _ => a,
                };
                let prop = if prop == "Value" { "LineStyle" } else { prop };
                let redirected;
                let o = if matches!(o, StyleOwner::Named(..))
                    && matches!(prop, "LineStyle" | "Weight")
                {
                    line_arg(a, prop)?;
                    redirected = self.style_border_owner(o, prop)?;
                    &redirected
                } else {
                    o
                };

                let Some(canon) = colour_put_args(o, prop, a)? else {
                    return Ok(Variant::Empty);
                };
                let a = &canon;
                let auto_colour;
                let prop = if prop == "Color"
                    && !matches!(a.put, Some(Variant::Bool(_)))
                    && self.put_f64(a).is_ok_and(|x| x == -1.0)
                {
                    auto_colour = "ColorAuto";
                    auto_colour
                } else {
                    prop
                };

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

                let none_line;
                let (prop, a) = if prop == "ColorIndex" && app_put_is_none(a) {
                    none_line = Args {
                        values: Vec::new(),
                        put: Some(Variant::I4(NONE)),
                        lcid: a.lcid,
                    };
                    ("LineStyle", &none_line)
                } else {
                    (prop, a)
                };

                if let StyleOwner::Range(r) = o {
                    let (sheet, rect) = self.resolve(r)?;
                    if rect.is_full_rows() {
                        self.style_guard(o, "Border", prop)?;
                        let put = border_put(self, prop, a)?;
                        let draws = matches!(prop, "LineStyle" | "Weight");

                        let cur = if draws {
                            let mut c = [None; 4];
                            for (k, i) in [7, 10, 8, 9].into_iter().enumerate() {
                                c[k] = Some(self.edge_line_read(o, i, true)?);
                            }
                            c
                        } else {
                            [None; 4]
                        };
                        let diagonal = self.edge_line_read(o, 5, true)?;
                        let edit = |k: usize, b: Border| match cur[k] {
                            Some(line) => put(Border { line, ..b }),
                            None => put(b),
                        };
                        let wb = &mut self.book_mut(r.book)?.wb;

                        let fresh = [Edge::Left, Edge::Right, Edge::Top, Edge::Bottom].map(|e| {
                            if side_uniform_none(wb, sheet, rect, Some(true), e, false) {
                                CellColor::Indexed(64)
                            } else {
                                CellColor::Auto
                            }
                        });

                        let row_was = wb.line_style(sheet, true, rect.r0).borders.clone();
                        let mut row_now = [None; 4];
                        for (k, e) in [Edge::Left, Edge::Right, Edge::Top, Edge::Bottom]
                            .into_iter()
                            .enumerate()
                        {
                            row_now[k] =
                                Some(edit(k, row_was.edge(e))).filter(|n| *n != row_was.edge(e));
                        }
                        wb.set_style_with(sheet, rect, &|s: &Style| {
                            let mut s = s.clone();
                            for (k, e) in [Edge::Left, Edge::Right, Edge::Top, Edge::Bottom]
                                .into_iter()
                                .enumerate()
                            {
                                let was = s.borders.edge(e);
                                let mut now = match row_now[k] {
                                    Some(n) if prop == "ColorAuto" => n,
                                    _ => edit(k, was),
                                };
                                if draws
                                    && was.line == BorderLine::None
                                    && now.line != BorderLine::None
                                {
                                    now.color = fresh[k];
                                }
                                *s.borders.edge_mut(e) = now;
                            }
                            s
                        });
                        if matches!(prop, "LineStyle") {
                            self.diagonal_line_set(o, prop, a, diagonal)?;
                        }
                        return Ok(Variant::Empty);
                    }
                }
                let pre = self.collection_colours(o);

                let draws = matches!(prop, "LineStyle" | "Weight")
                    && matches!(o, StyleOwner::Range(_) | StyleOwner::Areas(_));
                let mut cur = [None; 6];
                if draws {
                    for (k, i) in (7..=12).enumerate() {
                        cur[k] = Some(self.edge_line_read(o, i, true)?);
                    }
                }
                let diagonal = self.edge_line_read(o, 5, true)?;

                if let (StyleOwner::Cond(..), "LineStyle" | "Weight") = (o, prop) {
                    if let Ok(c) = line_arg(a, prop) {
                        if c >= if prop == "LineStyle" { 9 } else { 3 } {
                            return Err(OmError::app_defined());
                        }
                    }
                }
                for (k, i) in (7..=12).enumerate() {
                    if i > 10 && matches!(o, StyleOwner::Named(..) | StyleOwner::Cond(..)) {
                        break;
                    }
                    let i = if matches!(o, StyleOwner::Cond(..)) {
                        cond_plain_index(i)
                    } else {
                        i
                    };
                    self.border_set(
                        o,
                        i,
                        prop,
                        a,
                        EdgePut {
                            clear: i > 10,
                            whole: pre,
                            cur: cur[k],
                        },
                    )?;
                    if let StyleOwner::Cond(b, sh, id, _) = o {
                        self.cf_edge_touched.insert((*b, *sh, *id, i));
                    }
                }

                if matches!(prop, "LineStyle") && matches!(o, StyleOwner::Range(_)) {
                    self.diagonal_line_set(o, prop, a, diagonal)?;
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
        let own = is_colour_prop(m.name);
        self.cond_put(o, m, own, |app| app.border_member_core(o, index, m, a))
    }

    fn border_member_core(
        &mut self,
        o: &StyleOwner,
        index: i32,
        m: &'static Member,
        a: &Args,
    ) -> Result<Variant, OmError> {
        if let (StyleOwner::Areas(rs), true) = (o, !matches!(m.kind, InvokeKind::Get)) {
            for r in rs {
                self.border_member(&StyleOwner::Range(*r), index, m, a)?;
            }
            return Ok(Variant::Empty);
        }

        if matches!(o, StyleOwner::Cond(..))
            && m.name == "LineStyle"
            && !matches!(index, -4131 | -4152 | -4160 | -4107)
        {
            return Err(OmError::app_defined());
        }

        if let StyleOwner::Cond(..) = o {
            let plain = matches!(index, -4131 | -4152 | -4160 | -4107);
            if matches!(m.name, "LineStyle" | "Weight") && !plain {
                return Err(OmError::app_defined());
            }
            if plain
                && matches!(m.name, "LineStyle" | "Weight")
                && !matches!(m.kind, InvokeKind::Get)
            {
                if let Ok(c) = line_arg(a, m.name) {
                    if c >= if m.name == "LineStyle" { 9 } else { 3 } {
                        return Err(OmError::app_defined());
                    }
                }
            }
        }

        let index = match (o, m.kind) {
            (StyleOwner::Cond(..), kind) if !matches!(kind, InvokeKind::Get) => {
                if matches!(index, 11 | 12) {
                    return Ok(Variant::Empty);
                }
                cond_plain_index(index)
            }
            _ => index,
        };

        if matches!(o, StyleOwner::Named(..))
            && (7..=10).contains(&index)
            && matches!(
                m.name,
                "ThemeColor" | "TintAndShade" | "Color" | "ColorIndex"
            )
            && !matches!(m.kind, InvokeKind::Get)
        {
            return Ok(Variant::Empty);
        }
        match m.kind {
            InvokeKind::Get => self.border_get(o, index, m.name),
            _ => {
                let draws = matches!(m.name, "LineStyle" | "Weight");
                let redirected;
                let o = if draws && matches!(o, StyleOwner::Named(..)) {
                    line_arg(a, m.name)?;
                    redirected = self.style_border_owner(o, m.name)?;
                    &redirected
                } else {
                    o
                };

                let Some(a) = (match o {
                    StyleOwner::Cond(..)
                        if matches!(m.name, "Color" | "ColorIndex" | "ThemeColor") =>
                    {
                        colour_put_value(a, m.name)?.map(|v| Args {
                            values: Vec::new(),
                            put: Some(v),
                            lcid: a.lcid,
                        })
                    }
                    _ => colour_put_args(o, m.name, a)?,
                }) else {
                    return Ok(Variant::Empty);
                };
                let a = &a;
                let clears = matches!(o, StyleOwner::Cond(..))
                    && matches!(
                        (m.name, &a.put),
                        ("Color", Some(Variant::I4(-1))) | ("ColorIndex", Some(Variant::I4(NONE)))
                    );
                let prop = if clears { "ColorNone" } else { m.name };

                let opens_left = match o {
                    StyleOwner::Named(b, n) if draws && index == 7 && line_arg(a, prop)? != 0 => {
                        let left = self.named_style(*b, n)?.style.borders.left;
                        (left.line == BorderLine::None).then(|| (*b, n.clone()))
                    }
                    _ => None,
                };
                if !(matches!(o, StyleOwner::Named(..)) && (7..=10).contains(&index)) {
                    self.border_set(o, index, prop, a, EdgePut::single())?;
                }
                if let StyleOwner::Cond(b, s, id, _) = o {
                    self.cf_edge_touched.insert((*b, *s, *id, index));
                }
                if let Some(key) = opens_left {
                    self.style_edge_touched.insert(key);
                }
                Ok(Variant::Empty)
            }
        }
    }

    fn borders_collection_get(&self, o: &StyleOwner, prop: &str) -> Result<Variant, OmError> {
        let areas = match o {
            StyleOwner::Range(r) => vec![*r],
            StyleOwner::Areas(rs) => rs.clone(),
            _ => return Err(OmError::app_defined()),
        };
        let theme = &self.book(self.owner_book(o))?.wb.style_table().theme;
        let mut first: Option<Border> = None;
        let mut line: Option<BorderLine> = None;
        let mut style: Option<i32> = None;
        let (mut same_line, mut same_style) = (true, true);
        let mut conflict = false;
        let mut kinds = Vec::new();
        let placed = areas
            .iter()
            .map(|r| {
                let (sheet, rect) = self.resolve(r)?;
                let kind = whole_line(rect);
                kinds.push(kind);
                Ok((sheet, reduced(rect, kind)))
            })
            .collect::<Result<Vec<_>, OmError>>()?;
        for ((r, (sheet, rect)), kind) in areas.iter().zip(&placed).zip(kinds.iter().copied()) {
            let wb = &self.book(r.book)?.wb;
            for a in cells(*sheet, *rect) {
                let s = vstyle(wb, kind, a);
                for e in [Edge::Left, Edge::Top, Edge::Bottom, Edge::Right] {
                    let inside = neighbour(Rect::cell(a.row, a.col), e).is_some_and(|n| {
                        placed
                            .iter()
                            .any(|(sh, rc)| *sh == *sheet && rc.contains(n.r0, n.c0))
                    });
                    if inside && matches!(e, Edge::Right | Edge::Bottom) {
                        if let Some(n) = neighbour(Rect::cell(a.row, a.col), e) {
                            let other = vstyle(wb, kind, CellAddr::new(*sheet, n.r0, n.c0))
                                .borders
                                .edge(e.opposite());
                            let mine = s.borders.edge(e);

                            conflict |= mine.line != BorderLine::None
                                && other.line != BorderLine::None
                                && mine.color != other.color;
                        }
                    }

                    let b = if inside {
                        vshown(wb, kind, a, e)
                    } else {
                        s.borders.edge(e)
                    };

                    if matches!(e, Edge::Left | Edge::Top | Edge::Bottom | Edge::Right) {
                        for step in [Edge::Right, Edge::Bottom] {
                            if let Some(n) = neighbour(Rect::cell(a.row, a.col), step) {
                                if placed
                                    .iter()
                                    .any(|(sh, rc)| *sh == *sheet && rc.contains(n.r0, n.c0))
                                {
                                    let side_of = |c: CellAddr| {
                                        let inner = neighbour(Rect::cell(c.row, c.col), e)
                                            .is_some_and(|x| {
                                                placed.iter().any(|(sh, rc)| {
                                                    *sh == *sheet && rc.contains(x.r0, x.c0)
                                                })
                                            });
                                        if inner {
                                            vshown(wb, kind, c, e)
                                        } else {
                                            vstyle(wb, kind, c).borders.edge(e)
                                        }
                                    };
                                    let o = side_of(CellAddr::new(*sheet, n.r0, n.c0));
                                    let m = side_of(a);
                                    conflict |= m.line != o.line
                                        || (m.line != BorderLine::None && m.color != o.color);
                                }
                            }
                        }
                    }
                    first.get_or_insert(b);

                    let st = if b.line == BorderLine::Hair {
                        i32::MIN
                    } else {
                        line_parts(b.line).0
                    };
                    same_line &= *line.get_or_insert(b.line) == b.line;
                    same_style &= *style.get_or_insert(st) == st;
                }
            }
        }
        first.ok_or_else(OmError::object_required)?;

        for ((r, (sheet, rect)), kind) in areas.iter().zip(&placed).zip(kinds.iter().copied()) {
            let Some(row) = kind else { continue };
            let wb = &self.book(r.book)?.wb;
            for (cr, cc) in wb.formatted_cells(*sheet) {
                let (idx, inside) = if row {
                    (cr, (rect.r0..=rect.r1).contains(&cr))
                } else {
                    (cc, (rect.c0..=rect.c1).contains(&cc))
                };
                if !inside {
                    continue;
                }
                let fmt = wb.line_style(*sheet, row, idx);
                let c = CellAddr::new(*sheet, cr, cc);
                for e in [Edge::Left, Edge::Top, Edge::Bottom, Edge::Right] {
                    conflict |= wb.cell_style(c).borders.edge(e) != fmt.borders.edge(e);
                }
            }

            let (lo, hi) = if row {
                (rect.r0, rect.r1)
            } else {
                (rect.c0, rect.c1)
            };
            let mut reps = vec![lo];
            reps.extend(
                wb.line_format_indices(*sheet, row)
                    .into_iter()
                    .filter(|i| (lo..=hi).contains(i))
                    .take(256),
            );
            for j in wb.line_format_indices(*sheet, !row) {
                for &i in &reps {
                    let c = if row {
                        CellAddr::new(*sheet, i, j)
                    } else {
                        CellAddr::new(*sheet, j, i)
                    };
                    let fmt = wb.line_style(*sheet, row, i);
                    for e in [Edge::Left, Edge::Top, Edge::Bottom, Edge::Right] {
                        conflict |= wb.cell_style(c).borders.edge(e) != fmt.borders.edge(e);
                    }
                }
            }
        }
        match prop {
            "LineStyle" => Ok(if same_style && same_line && !conflict {
                i4(style
                    .filter(|s| *s != i32::MIN)
                    .unwrap_or(if line.is_some() { CONTINUOUS } else { NONE }))
            } else {
                Variant::Null
            }),
            "Weight" => Ok(match line {
                Some(l) if same_line && !conflict => i4(line_parts(l).1),
                _ => Variant::Null,
            }),
            _ => {
                let (sheet, rect) = placed.first().ok_or_else(OmError::object_required)?;
                let wb = &self.book(areas[0].book)?.wb;
                let s = vstyle(wb, kinds[0], CellAddr::new(*sheet, rect.r0, rect.c0));
                let sides =
                    [Edge::Left, Edge::Top, Edge::Bottom, Edge::Right].map(|e| s.borders.edge(e));
                let uniform = sides
                    .iter()
                    .all(|b| b.line != BorderLine::None && b.color == sides[0].color);
                if uniform {
                    border_prop(theme, Some(sides[0]), prop)
                } else {
                    Ok(match prop {
                        "Color" => Variant::R8(0.0),
                        "ColorIndex" => i4(NONE),
                        _ => Variant::Null,
                    })
                }
            }
        }
    }

    fn collection_colours(&self, o: &StyleOwner) -> Option<[CellColor; 4]> {
        let StyleOwner::Range(r) = o else {
            return None;
        };
        let (sheet, rect) = self.resolve(r).ok()?;
        let wb = &self.book(r.book).ok()?.wb;
        let kind = whole_line(rect);
        let first = vstyle(wb, kind, CellAddr::new(sheet, rect.r0, rect.c0));
        Some([Edge::Left, Edge::Right, Edge::Top, Edge::Bottom].map(|e| {
            let lead = first.borders.edge(e);
            if lead.line != BorderLine::None {
                lead.color
            } else if side_uniform_none(wb, sheet, rect, kind, e, false) {
                CellColor::Indexed(64)
            } else {
                CellColor::Auto
            }
        }))
    }

    fn border_get(&self, o: &StyleOwner, index: i32, prop: &str) -> Result<Variant, OmError> {
        self.border_get_in(o, index, prop, false)
    }

    fn border_get_in(
        &self,
        o: &StyleOwner,
        index: i32,
        prop: &str,
        multi: bool,
    ) -> Result<Variant, OmError> {
        let b = self.owner_book(o);
        let theme = &self.book(b)?.wb.style_table().theme;
        match o {
            StyleOwner::Areas(rs) => {
                let first = rs.first().ok_or_else(OmError::object_required)?;
                self.border_get_in(&StyleOwner::Range(*first), index, prop, true)
            }
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
                        let touched = match o {
                            StyleOwner::Named(b, n) => {
                                self.style_edge_touched.contains(&(*b, n.clone()))
                            }
                            _ => false,
                        };
                        if touched && index == 7 {
                            match prop {
                                "LineStyle" => return Ok(i4(NONE)),
                                "Weight" => return Ok(i4(THIN)),
                                _ => {}
                            }
                        }
                        return Ok(match prop {
                            "Color" => Variant::R8(0.0),
                            "ColorIndex" => i4(NONE),
                            _ => Variant::Null,
                        });
                    }
                };

                if let StyleOwner::Cond(b, s, id, _) = o {
                    if matches!(prop, "LineStyle" | "Weight")
                        && !self
                            .cf_edge_touched
                            .contains(&(*b, *s, *id, cond_plain_index(index)))
                    {
                        return Ok(Variant::Null);
                    }
                }
                let line = self.agree_on(o, |s| side_of(s, side))?;
                border_prop(theme, line, prop)
            }
            StyleOwner::Range(r) => {
                let (sheet, rect) = self.resolve(r)?;
                let Some((cells_at, side)) = border_cells(rect, index)? else {
                    return border_prop(theme, Some(Border::default()), prop);
                };

                let wb = &self.book(r.book)?.wb;
                let kind = whole_line(rect);
                let first = odd_edge_cell(rect, index, multi)
                    .map_or(CellAddr::new(sheet, cells_at.r0, cells_at.c0), |(r, c)| {
                        CellAddr::new(sheet, r, c)
                    });

                if !matches!(side, Side::Edge(_)) {
                    let s = vstyle(wb, kind, first);
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
                border_prop(
                    theme,
                    Some(self.range_edge_read(r, index, multi, false)?),
                    prop,
                )
            }
        }
    }

    fn range_edge_read(
        &self,
        r: &RangeRef,
        index: i32,
        multi: bool,
        own: bool,
    ) -> Result<Border, OmError> {
        let (sheet, rect) = self.resolve(r)?;
        let Some((cells_at, side)) = border_cells(rect, index)? else {
            return Ok(Border::default());
        };
        let wb = &self.book(r.book)?.wb;
        let kind = whole_line(rect);
        let mut first = CellAddr::new(sheet, cells_at.r0, cells_at.c0);

        let mut stored = own || matches!(index, 11 | 12);

        if let Some((r, c)) = odd_edge_cell(rect, index, multi) {
            first = CellAddr::new(sheet, r, c);
            stored |= matches!(index, 9 | -4107);
        }
        let mut line = if stored {
            side_of(vstyle(wb, kind, first), side)
        } else if !matches!(side, Side::Edge(_)) && rect != Rect::cell(rect.r0, rect.c0) {
            side_of(
                if kind.is_some() {
                    wb.cell_style(first)
                } else {
                    vstyle(wb, kind, first)
                },
                side,
            )
        } else {
            match side {
                Side::Edge(e) => vshown(wb, kind, first, e),
                _ => side_of(wb.cell_style(first), side),
            }
        };

        if !matches!(side, Side::Edge(_)) && rect != Rect::cell(rect.r0, rect.c0) {
            line.color = CellColor::Auto;
        }
        Ok(line)
    }

    fn edge_line_read(&self, o: &StyleOwner, index: i32, own: bool) -> Result<BorderLine, OmError> {
        Ok(match o {
            StyleOwner::Range(r) => {
                let (sheet, rect) = self.resolve(r)?;
                match (cell_side_index(index), whole_line(rect)) {
                    (Some(edge), None) => {
                        let e = match edge {
                            7 => Edge::Left,
                            8 => Edge::Top,
                            9 => Edge::Bottom,
                            _ => Edge::Right,
                        };
                        let wb = &self.book(r.book)?.wb;
                        agree(cells(sheet, rect).map(|c| vshown(wb, None, c, e).line))
                            .unwrap_or(BorderLine::None)
                    }
                    _ => self.range_edge_read(r, index, false, own)?.line,
                }
            }
            StyleOwner::Areas(rs) => {
                let first = rs.first().ok_or_else(OmError::object_required)?;
                self.range_edge_read(first, index, true, own)?.line
            }
            _ => BorderLine::None,
        })
    }

    fn border_set(
        &mut self,
        o: &StyleOwner,
        index: i32,
        prop: &str,
        a: &Args,
        how: EdgePut,
    ) -> Result<(), OmError> {
        self.style_guard(o, "Border", prop)?;
        let EdgePut { clear, whole, cur } = how;

        let cur = match (cur, prop, o) {
            (None, "LineStyle" | "Weight", StyleOwner::Range(_) | StyleOwner::Areas(_)) => {
                Some(self.edge_line_read(o, index, false)?)
            }
            (c, ..) => c,
        };
        let put = border_put(self, prop, a)?;

        let base = match (o, prop) {
            (StyleOwner::Cond(b, s, id, _), _)
                if matches!(index, -4131 | -4152 | -4160 | -4107 | 5 | 6) =>
            {
                let side = match index {
                    -4131 | 7 => Side::Edge(Edge::Left),
                    -4152 | 10 => Side::Edge(Edge::Right),
                    -4160 | 8 => Side::Edge(Edge::Top),
                    5 => Side::DiagonalDown,
                    6 => Side::DiagonalUp,
                    _ => Side::Edge(Edge::Bottom),
                };
                Some(if self.cf_edge_touched.contains(&(*b, *s, *id, index)) {
                    self.agree_on(o, |st| side_of(st, side))?
                        .unwrap_or_default()
                } else {
                    Border::default()
                })
            }
            _ => None,
        };
        let edit = move |b: Border| match (base, cur) {
            (Some(base), _) => put(base),
            (None, Some(line)) => put(Border { line, ..b }),
            (None, None) => put(b),
        };
        let how = EdgePut { clear, whole, cur };

        let r = match o {
            StyleOwner::Display(_) => return Err(OmError::app_defined()),
            StyleOwner::Areas(rs) => {
                for r in rs {
                    self.border_set(&StyleOwner::Range(*r), index, prop, a, how)?;
                }
                return Ok(());
            }
            StyleOwner::Cond(b, sh, id, _) if matches!(index, 5 | 6) => {
                let down = index == 5;
                let wb = &mut self.book_mut(*b)?.wb;
                let sheet = wb.cfv_sheet_mut(*sh);
                let i = sheet.index_of(*id).ok_or_else(OmError::app_defined)?;
                let borders = &mut sheet.rules[i].dxf.style.borders;
                let was = if (down && borders.diagonal_down) || (!down && borders.diagonal_up) {
                    borders.diagonal
                } else {
                    Border::default()
                };
                borders.diagonal = edit(was);
                if down {
                    borders.diagonal_down = true;
                } else {
                    borders.diagonal_up = true;
                }
                return Ok(());
            }
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

        if let (Some(edge), None) = (cell_side_index(index), whole_line(rect)) {
            let strips: Vec<Rect> = if matches!(edge, 7 | 10) {
                (rect.c0..=rect.c1)
                    .map(|c| Rect::new(rect.r0, c, rect.r1, c))
                    .collect()
            } else {
                (rect.r0..=rect.r1)
                    .map(|row| Rect::new(row, rect.c0, row, rect.c1))
                    .collect()
            };
            for s in strips {
                let o = StyleOwner::Range(RangeRef {
                    track: crate::app::Track::Fixed(sheet, s),
                    ..r
                });
                self.border_set(&o, edge, prop, a, how)?;
            }
            return Ok(());
        }
        let Some((at, side)) = border_cells(rect, index)? else {
            return Ok(());
        };

        let mut kind = whole_line(rect);

        let spans_cols = at.c0 == 0 && at.c1 + 2 >= max_cols();
        let spans_rows = at.r0 == 0 && at.r1 + 2 >= max_rows();
        if clear {
            match kind {
                Some(false) if !spans_rows && at.r0 > 0 => return Ok(()),
                Some(true) if !spans_cols && at.c0 + 1 >= max_cols() => return Ok(()),
                _ => {}
            }
        }

        let top_strip = (kind == Some(false) && index == 8).then_some(at);
        let at = match kind {
            Some(false) => Rect::new(0, at.c0, max_rows() - 1, at.c1),
            Some(true) if clear && !spans_cols => {
                kind = None;
                at
            }
            Some(true) => Rect::new(at.r0, 0, at.r1, max_cols() - 1),
            None => at,
        };
        let kind = kind;
        let wb = &mut self.book_mut(r.book)?.wb;
        let first = CellAddr::new(sheet, at.r0, at.c0);
        let shown = match side {
            Side::Edge(_) if !clear => side_of(vstyle(wb, kind, first), side),
            Side::Edge(e) => vshown(wb, kind, first, e),

            _ if matches!(prop, "Color" | "ColorIndex" | "ThemeColor" | "TintAndShade") && {
                let b = &vstyle(wb, kind, first).borders;
                b.diagonal_down != b.diagonal_up
            } =>
            {
                vstyle(wb, kind, first).borders.diagonal
            }
            _ => diagonal_base(vstyle(wb, kind, first), side),
        };

        if !matches!(side, Side::Edge(_))
            && shown.line == BorderLine::None
            && prop == "TintAndShade"
        {
            return Ok(());
        }

        if !matches!(side, Side::Edge(_))
            && prop == "ColorIndex"
            && matches!(&a.put, Some(Variant::I4(NONE)))
        {
            return Ok(());
        }
        let mut line = edit(shown);

        if let (Side::Edge(e), "LineStyle" | "Weight") = (side, prop) {
            if line.line != BorderLine::None {
                if let Some(cols) = whole {
                    line.color = cols[edge_slot(e)];
                } else if shown.line == BorderLine::None {
                    let none = match top_strip {
                        Some(edge) => side_uniform_none(wb, sheet, edge, None, e, true),
                        None => side_uniform_none(wb, sheet, at, kind, e, true),
                    };
                    line.color = if none {
                        CellColor::Indexed(64)
                    } else {
                        CellColor::Auto
                    };
                }
            }
        }
        match side {
            Side::Edge(e) => {
                let dialog = clear && !matches!(index, 11 | 12);
                let colour = is_colour_prop(prop);
                let mut extra: Vec<(Rect, Edge, Border)> = Vec::new();
                let mut clears: Vec<(Rect, Edge)> = Vec::new();
                let at_cells = reduced(at, kind);
                let changed =
                    colour || !cells(sheet, at_cells).all(|c| vshown(wb, kind, c, e) == line);
                if dialog {
                    for c in cells(sheet, at_cells) {
                        for s in [Edge::Left, Edge::Right, Edge::Top, Edge::Bottom] {
                            if s == e {
                                continue;
                            }
                            let Some(n) = neighbour(Rect::cell(c.row, c.col), s) else {
                                continue;
                            };
                            if rect.contains(n.r0, n.c0) {
                                continue;
                            }
                            let sh = vshown(wb, kind, c, s);
                            if sh.line == BorderLine::None {
                                continue;
                            }
                            if colour {
                                extra.push((unit(kind, c), s, sh));
                                clears.push((n, s.opposite()));
                            } else if vstyle(wb, kind, c).borders.edge(s).line == BorderLine::None {
                                extra.push((unit(kind, c), s, sh));
                            }
                        }
                    }
                }
                wb.set_style_with(sheet, at, &|s: &Style| {
                    let mut s = s.clone();
                    *s.borders.edge_mut(e) = line;
                    s
                });
                for (c, s, b) in extra {
                    wb.set_style_with(sheet, c, &|st: &Style| {
                        let mut st = st.clone();
                        *st.borders.edge_mut(s) = b;
                        st
                    });
                }
                for (c, s) in clears {
                    wb.set_style_with(sheet, c, &|st: &Style| {
                        let mut st = st.clone();
                        *st.borders.edge_mut(s) = Border::default();
                        st
                    });
                }

                let strips: Vec<Rect> = if matches!(index, 11 | 12) {
                    cells(sheet, at_cells)
                        .filter_map(|c| neighbour(Rect::cell(c.row, c.col), e))
                        .filter(|_| {
                            !matches!(
                                (kind, e),
                                (Some(false), Edge::Top | Edge::Bottom)
                                    | (Some(true), Edge::Left | Edge::Right)
                            )
                        })
                        .map(|n| unit(kind, CellAddr::new(sheet, n.r0, n.c0)))
                        .collect()
                } else {
                    neighbour(at, e).into_iter().collect()
                };
                for n in strips.into_iter().filter(|_| clear && (changed || !dialog)) {
                    wb.set_style_with(sheet, n, &|s: &Style| {
                        let mut s = s.clone();
                        *s.borders.edge_mut(e.opposite()) = Border::default();
                        s
                    });
                }
            }
            Side::DiagonalDown | Side::DiagonalUp => {
                let down = side == Side::DiagonalDown;

                let both = prop == "Weight" && shown.line == BorderLine::None && kind.is_none();

                let seed = kind
                    .filter(|_| {
                        matches!(prop, "Color" | "ColorIndex" | "ThemeColor" | "TintAndShade")
                    })
                    .map(|_| {
                        let b = &wb.cell_style(first).borders;
                        (b.diagonal_down, b.diagonal_up)
                    });
                wb.set_style_with(sheet, at, &|s: &Style| {
                    let mut s = s.clone();
                    if let Some((d, u)) =
                        seed.filter(|_| !s.borders.diagonal_down && !s.borders.diagonal_up)
                    {
                        s.borders.diagonal_down = d;
                        s.borders.diagonal_up = u;
                    }
                    let on = line.line != BorderLine::None;
                    if both && on {
                        s.borders.diagonal_down = true;
                        s.borders.diagonal_up = true;
                    } else if down {
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

    fn border_around(&mut self, r: &RangeRef, a: &Args) -> Result<Variant, OmError> {
        let owner = StyleOwner::Range(*r);
        let put = |v: Variant| Args {
            values: Vec::new(),
            put: Some(v),
            lcid: a.lcid,
        };
        let mut puts = vec![
            ("LineStyle", a.get(0).cloned().unwrap_or(i4(CONTINUOUS))),
            ("Weight", a.get(1).cloned().unwrap_or(i4(THIN))),
            ("ColorIndex", a.get(2).cloned().unwrap_or(i4(AUTOMATIC))),
        ];
        if let Some(c) = a.get(3) {
            puts.push(("Color", c.clone()));
        }
        if let Some(t) = a.get(4) {
            puts.push(("ThemeColor", t.clone()));
        }
        for (prop, v) in puts {
            let args = put(v);
            for index in 7..=10 {
                self.border_set(&owner, index, prop, &args, EdgePut::single())?;
            }
        }
        Ok(Variant::Bool(true))
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
            ("BorderAround", Func) => self.border_around(r, a),
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
                let found = self.book(r.book)?.wb.style_table().get(&name).cloned();

                let Some(ns) = found else {
                    return Err(match self.agree_on(&owner, |s| s.named.clone())? {
                        Some(_) => OmError::hresult(crate::error::DISP_E_BADPARAMCOUNT),
                        None => OmError::app_defined_with(&format!("Style '{name}' not found.")),
                    });
                };
                let wb = &mut self.book_mut(r.book)?.wb;
                let formats = wb.number_formats_of_constants(sheet, rect);
                wb.set_style_with(sheet, rect, &|s: &Style| {
                    let mut s = s.clone();
                    apply_named(&mut s, &ns);
                    s
                });
                if ns.include_number {
                    wb.set_number_format_copy(sheet, rect, &ns.style.number_format);
                    wb.round_reformatted(sheet, rect, formats);
                }

                if ns.include_number || ns.include_font {
                    wb.widen_for_format(sheet, rect);
                }
                Ok(Variant::Empty)
            })(),
            (prop, Get) => {
                let wb = match self.book(r.book) {
                    Ok(b) => &b.wb,
                    Err(e) => return Some(Err(e)),
                };
                let styles = wb.range_styles(sheet, rect);
                let get = |f: &dyn Fn(&Style) -> Variant| {
                    agree(styles.iter().map(|s| f(s))).unwrap_or(Variant::Null)
                };

                if prop == "ReadingOrder" {
                    let got = agree(styles.iter().map(|s| s.alignment.reading_order));
                    return Some(Ok(i4(-5002 - i32::from(got.unwrap_or(1)))));
                }
                return format_prop(&get, prop).map(Ok);
            }
            (prop, _) => {
                let mut edit = match format_put(prop, a, self) {
                    Ok(Some(e)) => e,
                    Ok(None) => return None,
                    Err(e) => return Some(Err(e)),
                };

                if prop == "AddIndent" {
                    let mixed = self.book(r.book).is_ok_and(|b| {
                        let h = |s: &Style| s.alignment.horizontal;
                        match whole_line(rect) {
                            Some(row) => agree(
                                line_range_styles(&b.wb, sheet, rect, row)
                                    .into_iter()
                                    .map(h),
                            ),
                            None if rect.is_full_rows() => {
                                Some(h(b.wb.cell_style(CellAddr::new(sheet, 0, 0))))
                            }
                            None => agree(b.wb.range_styles(sheet, rect).into_iter().map(h)),
                        }
                        .is_none()
                    });
                    if mixed {
                        let inner = edit;
                        edit = Box::new(move |s: &mut Style| {
                            inner(s);
                            s.alignment.indent = 0;
                        });
                    }
                }
                if let Err(e) = self.edit_style(&owner, &*edit) {
                    return Some(Err(e));
                }
                if matches!(
                    prop,
                    "IndentLevel" | "VerticalAlignment" | "HorizontalAlignment" | "WrapText"
                ) {
                    if let Ok(b) = self.book_mut(r.book) {
                        let fitted = b.wb.widen_for_indent(sheet, rect);
                        self.widen_kept
                            .extend(fitted.into_iter().map(|(r, t)| (sheet, r, t)));
                    }
                }
                Ok(Variant::Empty)
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
                let mut unnamed = false;

                let given = match a.get(0) {
                    None => None,
                    Some(v) => styles_add_name(a, v)?,
                };
                let name = match given {
                    None => {
                        unnamed = true;
                        String::new()
                    }
                    Some(n) if n.is_empty() => {
                        unnamed = true;
                        String::new()
                    }
                    Some(n) => {
                        if n.encode_utf16().count() > 255 {
                            return Err(OmError::app_defined());
                        }
                        n.trim_matches(' ').to_string()
                    }
                };
                if !unnamed && (name.is_empty() || table.get(&name).is_some()) {
                    return Err(OmError::app_defined());
                }

                if matches!(a.get(1), Some(v) if !matches!(v, Variant::Dispatch(_))) {
                    return Err(OmError::app_defined_with(
                        "Add method of Styles class failed",
                    ));
                }

                let mut style = match a.get(1) {
                    Some(Variant::Dispatch(_)) => {
                        let (sheet, rect) = self.range_arg(a.get(1))?;

                        if self.book(b)?.wb.active_sheet() != sheet {
                            return Err(OmError::app_defined_with(
                                "Add method of Styles class failed",
                            ));
                        }
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
                let ns = NamedStyle {
                    name: Arc::from(name.as_str()),
                    builtin_id: None,
                    style,
                    include_number: true,
                    include_font: true,
                    include_alignment: true,
                    include_border: true,
                    include_patterns: true,
                    include_protection: true,
                };
                let tbl = self.book_mut(b)?.wb.style_table_mut();
                let name: Arc<str> = if unnamed {
                    tbl.insert_unnamed(ns)
                } else {
                    tbl.insert(ns);
                    Arc::from(name.as_str())
                };
                Ok(self.alloc(Object::Style(b, name)))
            }

            "Merge" => {
                let src = match a.get(0) {
                    Some(Variant::Dispatch(Some(id))) => match self.arena.get(*id) {
                        Some(Object::Workbook(s)) => *s,
                        _ => return Err(OmError::app_defined()),
                    },
                    Some(Variant::Bstr(name)) => {
                        match self
                            .books
                            .iter()
                            .find(|x| x.name.eq_ignore_ascii_case(name))
                        {
                            Some(x) => x.id,
                            None => return Err(OmError::app_defined()),
                        }
                    }
                    _ => return Err(OmError::app_defined()),
                };
                if src == b {
                    return Err(OmError::app_defined());
                }

                let theirs = self.book(src)?.wb.style_table().named.clone();
                let wb = &mut self.book_mut(b)?.wb;
                for ns in theirs {
                    let tbl = wb.style_table_mut();
                    let mut mine = tbl.lookup(&ns.name).ok().map(|i| tbl.named[i].clone());
                    if mine.is_none() {
                        if let Some(h) = tbl.lookup_hidden(&ns.name) {
                            mine = Some(tbl.unhide(h));
                        }
                    }
                    let Some(old) = mine else {
                        wb.register_number_format(&ns.style.number_format);
                        wb.style_table_mut().insert_missed(ns);
                        continue;
                    };
                    if style_xf_eq(wb, &old, &ns) {
                        continue;
                    }
                    let new = NamedStyle {
                        name: old.name.clone(),
                        builtin_id: old.builtin_id,
                        style: Style {
                            named: old.name.clone(),
                            ..ns.style.clone()
                        },
                        ..ns
                    };
                    wb.register_number_format(&new.style.number_format);
                    wb.restyle_named(&new.name, &old.style, &new.style);
                    if new.name.eq_ignore_ascii_case("Normal") {
                        wb.set_base_style(new.style.clone());
                    }
                    wb.style_table_mut().replace(new);
                }
                Ok(Variant::Bool(true))
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

            ("IndentLevel", Get) => {
                let set =
                    self.opts
                        .contains_key(&format!("styleindent:{}:{}", b.0, name.to_lowercase()));
                Ok(if set && ns.style.alignment.rotation == 0 {
                    Variant::I4(i32::from(ns.style.alignment.indent))
                } else {
                    Variant::Null
                })
            }
            ("MergeCells", Get) => Err(OmError::app_defined_with(
                "Unable to get the MergeCells property of the Style class",
            )),
            ("BuiltIn", Get) => flag(ns.builtin_id.is_some()),
            ("Delete", Func) if !name.eq_ignore_ascii_case("Normal") => {
                let old = Style {
                    named: name.clone(),
                    ..ns.style.clone()
                };
                let wb = &mut self.book_mut(b)?.wb;
                wb.style_table_mut().remove(name);
                wb.reset_named_style(&old);
                Ok(Variant::Bool(true))
            }
            ("Delete", Func) => Err(OmError::app_defined_with(
                "Delete method of Style class failed",
            )),

            ("NumberFormat", Get) => {
                let wb = &self.book(b)?.wb;
                let code = &ns.style.number_format;
                Ok(Variant::Bstr(
                    if ns.style.custom_code && crate::range::host_spelled(wb, code) {
                        code.to_string()
                    } else {
                        crate::range::com_format(wb, code, a.lcid)
                    },
                ))
            }

            ("NumberFormatLocal", Get)
                if self
                    .style_numfmt_local
                    .contains(&(b, Arc::from(name.as_ref()))) =>
            {
                Ok(Variant::Bstr(ns.style.number_format.to_string()))
            }
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

            (
                "IncludeNumber" | "IncludeFont" | "IncludeAlignment" | "IncludeBorder"
                | "IncludePatterns" | "IncludeProtection",
                Put,
            ) => {
                let refuse = || OmError::unable_to_set(m.name, "Style");
                let on = match &a.put {
                    Some(Variant::Bool(x)) => *x,
                    None | Some(Variant::Null | Variant::Empty) => false,
                    Some(Variant::Bstr(_)) => return Err(refuse()),
                    Some(_) => {
                        let x = self.put_f64(a)?;
                        if x == 1.0 {
                            true
                        } else if x == 0.0 || (x == 2.0 && m.name == "IncludeNumber") {
                            false
                        } else {
                            return Err(refuse());
                        }
                    }
                };
                let wb = &mut self.book_mut(b)?.wb;
                let tbl = wb.style_table_mut();
                if let Some(st) = tbl.position(name).map(|i| &mut tbl.named[i]) {
                    match m.name {
                        "IncludeNumber" => st.include_number = on,
                        "IncludeFont" => st.include_font = on,
                        "IncludeAlignment" => st.include_alignment = on,
                        "IncludeBorder" => st.include_border = on,
                        "IncludePatterns" => st.include_patterns = on,
                        _ => st.include_protection = on,
                    }
                }
                Ok(Variant::Empty)
            }

            ("MergeCells", Put) => Err(OmError::unable_to_set("MergeCells", "Style")),

            ("NumberFormat" | "NumberFormatLocal", Put) => {
                if matches!(a.put, Some(Variant::Bool(_))) {
                    return Err(OmError::unable_to_set(m.name, "Style"));
                }
                if matches!(a.put, None | Some(Variant::Null | Variant::Empty)) {
                    return Err(OmError::app_defined());
                }
                let lcid = if m.name == "NumberFormat" { a.lcid } else { 0 };
                let fmt = self.put_string(a)?.replace("\"$\"", "$");
                if xlcalc::number_format_refused(&fmt) {
                    return Err(OmError::unable_to_set(m.name, "Style"));
                }
                let code = if lcid == xlcalc_variant::Locale::EN_US.0 {
                    crate::range::engine_format(&self.book(b)?.wb, &fmt)
                } else {
                    fmt
                };
                let code: Arc<str> = Arc::from(code);

                self.book_mut(b)?.wb.register_number_format(&code);
                self.edit_style(&owner, &|s: &mut Style| s.number_format = code.clone())?;
                let key = (b, Arc::from(name.as_ref()));
                if m.name == "NumberFormatLocal" {
                    self.style_numfmt_local.insert(key);
                } else {
                    self.style_numfmt_local.remove(&key);
                }
                Ok(Variant::Empty)
            }

            ("WrapText" | "ShrinkToFit" | "Locked" | "FormulaHidden" | "AddIndent", Put)
                if matches!(a.put, Some(Variant::Bstr(_))) =>
            {
                Err(OmError::unable_to_set(m.name, "Style"))
            }

            ("Orientation", Put) => {
                let x = if matches!(a.put, Some(Variant::Null | Variant::Empty)) {
                    0
                } else {
                    self.put_i32(a)?
                };
                let r = rotation_from(x)
                    .ok_or_else(|| OmError::unable_to_set("Orientation", "Style"))?;
                self.edit_style(&owner, &move |s: &mut Style| s.alignment.rotation = r)?;
                Ok(Variant::Empty)
            }

            ("IndentLevel", Put) => {
                let Some(edit) = format_put("IndentLevel", a, self)? else {
                    return Err(unsupported("Style", m));
                };
                self.edit_style(&owner, &*edit)?;
                self.opts.insert(
                    format!("styleindent:{}:{}", b.0, name.to_lowercase()),
                    Variant::Bool(true),
                );
                Ok(Variant::Empty)
            }
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
        Edge::Right if at.c1 + 1 < max_cols() => Rect::new(at.r0, at.c1 + 1, at.r1, at.c1 + 1),
        Edge::Top => Rect::new(at.r0.checked_sub(1)?, at.c0, at.r0.checked_sub(1)?, at.c1),
        Edge::Bottom if at.r1 + 1 < max_rows() => Rect::new(at.r1 + 1, at.c0, at.r1 + 1, at.c1),
        _ => return None,
    })
}

fn style_xf_eq(wb: &Workbook, dest: &NamedStyle, src: &NamedStyle) -> bool {
    let (d, s) = (&dest.style, &src.style);
    wb.format_in_table(&s.number_format)
        && wb.fill_in_table(&s.fill)
        && dest.include_number == src.include_number
        && dest.include_font == src.include_font
        && dest.include_alignment == src.include_alignment
        && dest.include_border == src.include_border
        && dest.include_patterns == src.include_patterns
        && dest.include_protection == src.include_protection
        && (!src.include_number || d.number_format == s.number_format)
        && (!src.include_font || d.font == s.font)
        && (!src.include_alignment || d.alignment == s.alignment)
        && (!src.include_border || d.borders == s.borders)
        && (!src.include_patterns || d.fill == s.fill)
        && (!src.include_protection
            || (d.locked == s.locked && d.formula_hidden == s.formula_hidden))
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
    s.font.scheme = FontScheme::Major;
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
    if a.font.scheme == b.font.scheme {
        m.theme_font = true;
        s.font.scheme = a.font.scheme;
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

        "Background"
            if matches!(m.kind, InvokeKind::Put)
                && !matches!(
                    a.put.as_ref(),
                    Some(Variant::Bool(true) | Variant::I4(1..=3 | -4105))
                )
                && !matches!(a.put.as_ref(), Some(Variant::R8(x)) if (1.0..4.0).contains(x)) =>
        {
            Err(OmError::unable_to_set(m.name, "Font"))
        }
        "Background" | "Size" | "Underline" if text || nothing => {
            Err(OmError::unable_to_set(m.name, "Font"))
        }
        _ => Ok(Variant::Empty),
    })
}

impl Application<'_> {
    fn diagonal_line_set(
        &mut self,
        o: &StyleOwner,
        prop: &str,
        a: &Args,
        line: BorderLine,
    ) -> Result<(), OmError> {
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
                    line,
                    color: CellColor::Auto,
                });
            }
            s
        });
        Ok(())
    }
}

fn app_put_is_none(a: &Args) -> bool {
    match &a.put {
        Some(Variant::I4(n)) => *n == NONE,
        Some(Variant::R8(x)) => *x == f64::from(NONE),
        _ => false,
    }
}

fn styles_add_name(a: &Args, v: &Variant) -> Result<Option<String>, OmError> {
    match v {
        Variant::Null | Variant::Empty => Ok(None),
        Variant::Bool(b) => Ok(Some(if *b { "TRUE" } else { "FALSE" }.to_string())),
        Variant::Array(sa) => {
            let wide = sa.bounds.iter().filter(|(_, n)| *n > 1).count();
            let rank_ok = if sa.bounds.len() <= 2 {
                wide <= 1
            } else {
                sa.data.len() == 1
            };
            match sa.data.first() {
                None => Err(OmError::type_mismatch()),
                Some(_) if !rank_ok => Err(OmError::app_defined_with(
                    "Add method of Styles class failed",
                )),
                Some(first) => styles_add_name(a, first),
            }
        }
        _ => a.variant_string(v),
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    fn put(v: Variant) -> Args {
        Args {
            values: Vec::new(),
            put: Some(v),
            lcid: 0,
        }
    }

    #[test]
    fn line_args_follow_fconvertlwctoborder() {
        assert_eq!(line_arg(&put(Variant::I4(DASH)), "LineStyle").ok(), Some(2));
        assert_eq!(line_arg(&put(Variant::I4(NONE)), "LineStyle").ok(), Some(0));
        assert_eq!(
            line_arg(&put(Variant::Bool(true)), "LineStyle").ok(),
            Some(1)
        );
        assert_eq!(
            line_arg(&put(Variant::Bool(false)), "LineStyle").ok(),
            Some(0)
        );
        assert_eq!(line_arg(&put(Variant::R8(0.1)), "LineStyle").ok(), Some(0));
        assert!(line_arg(&put(Variant::R8(-0.5)), "LineStyle").is_err());
        assert!(line_arg(&put(Variant::I4(14)), "LineStyle").is_err());
        assert_eq!(line_arg(&put(Variant::Bool(true)), "Weight").ok(), Some(1));
        assert!(line_arg(&put(Variant::Bool(false)), "Weight").is_err());
        assert_eq!(line_arg(&put(Variant::I4(MEDIUM)), "Weight").ok(), Some(3));
        assert_eq!(line_arg(&put(Variant::R8(4.9)), "Weight").ok(), Some(4));
        assert!(line_arg(&put(Variant::I4(5)), "Weight").is_err());
    }

    #[test]
    fn applyls_and_its_overrun() {
        use BorderLine::*;

        assert_eq!(line_style_put(Medium, 1), Medium);
        assert_eq!(line_style_put(Double, 1), Thick);
        assert_eq!(line_style_put(Thick, 2), Dashed);
        assert_eq!(line_style_put(Medium, 2), MediumDashed);
        assert_eq!(line_style_put(Thin, 13), SlantDashDot);

        assert_eq!(line_style_put(Thin, 7), Medium);
        assert_eq!(line_style_put(Dotted, 7), Thick);
        assert_eq!(line_style_put(Double, 7), Hair);
        assert_eq!(line_style_put(None, 8), Dashed);
        assert_eq!(line_style_put(Thin, 12), Double);

        assert_eq!(line_style_put(SlantDashDot, 7), None);
        assert_eq!(line_style_put(SlantDashDot, 8), None);
        assert_eq!(line_style_put(SlantDashDot, 10), Thin);
        assert_eq!(line_style_put(SlantDashDot, 11), Medium);
        assert_eq!(line_style_put(SlantDashDot, 12), Thick);
    }

    #[test]
    fn weight_table() {
        use BorderLine::*;
        assert_eq!(weight_put(None, 2), Thin);
        assert_eq!(weight_put(Dashed, 3), MediumDashed);
        assert_eq!(weight_put(Dotted, 3), Medium);
        assert_eq!(weight_put(Double, 4), Double);
        assert_eq!(weight_put(SlantDashDot, 3), SlantDashDot);
        assert_eq!(weight_put(SlantDashDot, 2), Thin);
        assert_eq!(line_parts(SlantDashDot), (13, MEDIUM));
        assert_eq!(line_parts(None), (NONE, THIN));
        assert_eq!(line_parts(Double), (DOUBLE, 4));
    }

    #[test]
    fn null_puts_are_blank_command_arguments() {
        let app = Application::new();
        for prop in ["HorizontalAlignment", "WrapText", "Locked", "IndentLevel"] {
            let edit = format_put(prop, &put(Variant::Null), &app)
                .ok()
                .flatten()
                .expect("Null is accepted");
            let mut st = poison_style();
            let before = st.clone();
            edit(&mut st);
            assert_eq!(st, before, "{prop}");
        }
    }

    #[test]
    fn range_font_xlnone_changes_nothing() {
        let app = Application::new();
        let theme = Theme::default();
        let edit = font_put(&app, "ColorIndex", &put(Variant::I4(NONE)), &theme)
            .expect("xlNone is accepted");
        for start in [CellColor::Auto, CellColor::Rgb(0x123456)] {
            let mut st = Style::default();
            st.font.color = start;
            edit(&mut st);
            assert_eq!(st.font.color, start);
        }
    }

    #[test]
    fn colour_route_values() {
        let v = |x: Variant, p: &str| colour_put_value(&put(x), p).map_err(|_| ());
        assert_eq!(v(Variant::Null, "ColorIndex"), Ok(None));
        assert_eq!(
            v(Variant::Bool(true), "ColorIndex"),
            Ok(Some(Variant::I4(NONE)))
        );
        assert_eq!(
            v(Variant::Bool(false), "ColorIndex"),
            Ok(Some(Variant::I4(AUTOMATIC)))
        );
        assert_eq!(
            v(Variant::I4(-1), "ColorIndex"),
            Ok(Some(Variant::I4(NONE)))
        );
        assert_eq!(v(Variant::R8(2.5), "ColorIndex"), Ok(Some(Variant::I4(2))));
        assert!(v(Variant::I4(57), "ColorIndex").is_err());
        assert!(v(Variant::Bool(true), "Color").is_err());
        assert_eq!(v(Variant::Bool(false), "Color"), Ok(Some(Variant::I4(-1))));
        assert!(v(Variant::I4(i32::MAX), "Color").is_err());
        assert!(v(Variant::Bool(true), "ThemeColor").is_err());
        assert_eq!(v(Variant::I4(12), "ThemeColor"), Ok(Some(Variant::I4(12))));
        assert!(v(Variant::I4(13), "ThemeColor").is_err());
    }

    #[test]
    fn interior_colour_values() {
        let c = |x: Variant, icv: bool, theme: bool| match fill_colour(&put(x), icv, theme) {
            Ok(FillColour::Set(c)) => Ok(Some(c)),
            Ok(FillColour::Clear) => Ok(None),
            Err(_) => Err(()),
        };

        assert_eq!(
            c(Variant::Bool(true), false, false),
            Ok(Some(CellColor::Rgb(swap_rb(1))))
        );
        assert_eq!(
            c(Variant::Bool(false), false, false),
            Ok(Some(CellColor::Rgb(0)))
        );
        assert_eq!(c(Variant::I4(NONE), false, false), Ok(None));
        assert_eq!(
            c(Variant::I4(0x0100_00FF), false, false),
            Ok(Some(CellColor::Rgb(swap_rb(0xFF))))
        );

        assert_eq!(
            c(Variant::Bool(true), true, false),
            Ok(Some(CellColor::Auto))
        );
        assert_eq!(
            c(Variant::I4(AUTOMATIC), true, false),
            Ok(Some(CellColor::Auto))
        );
        assert_eq!(c(Variant::Bool(false), true, false), Ok(None));
        assert_eq!(c(Variant::I4(0), true, false), Ok(None));
        assert!(c(Variant::I4(57), true, false).is_err());

        assert!(c(Variant::Bool(true), false, true).is_ok());
        assert!(c(Variant::Bool(false), false, true).is_err());
        assert!(c(Variant::I4(13), false, true).is_err());
        assert_eq!(c(Variant::I4(NONE), false, true), Ok(None));
    }

    #[test]
    fn interior_pattern_values() {
        let ok = |x: Variant| interior_range_put("Pattern", &put(x)).is_ok();
        assert!(ok(Variant::I4(-1)));
        assert!(ok(Variant::I4(18)));
        assert!(ok(Variant::I4(-4166)));
        assert!(!ok(Variant::I4(19)));
        assert!(!ok(Variant::I4(-5)));
        assert!(matches!(
            interior_range_put("Pattern", &put(Variant::Null)),
            Ok(None)
        ));
    }
}
