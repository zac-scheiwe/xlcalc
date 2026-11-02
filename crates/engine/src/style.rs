use std::sync::Arc;

#[derive(Clone, Copy, PartialEq, Eq, Hash, Debug, Default)]
pub enum CellColor {
    #[default]
    Auto,

    Rgb(u32),

    Indexed(u8),

    Theme {
        index: u8,
        tint: u64,
    },
}

impl CellColor {
    pub fn theme(index: u8, tint: f64) -> CellColor {
        CellColor::Theme {
            index,
            tint: tint.to_bits(),
        }
    }

    pub fn tint(&self) -> f64 {
        match self {
            CellColor::Theme { tint, .. } => f64::from_bits(*tint),
            _ => 0.0,
        }
    }
}

#[derive(Clone, Copy, PartialEq, Eq, Hash, Debug, Default)]
pub enum Underline {
    #[default]
    None,
    Single,
    Double,
    SingleAccounting,
    DoubleAccounting,
}

#[derive(Clone, Copy, PartialEq, Eq, Hash, Debug, Default)]
pub enum FontScheme {
    #[default]
    None,
    Major,
    Minor,
}

#[derive(Clone, PartialEq, Eq, Hash, Debug)]
pub struct Font {
    pub name: Arc<str>,

    pub size_twips: u32,
    pub bold: bool,
    pub italic: bool,
    pub underline: Underline,
    pub strikethrough: bool,
    pub superscript: bool,
    pub subscript: bool,
    pub color: CellColor,
    pub scheme: FontScheme,

    pub family: u8,
}

#[derive(Clone, Copy, PartialEq, Eq, Hash, Debug, Default)]
pub enum Pattern {
    #[default]
    None,
    Solid,
    MediumGray,
    DarkGray,
    LightGray,
    DarkHorizontal,
    DarkVertical,
    DarkDown,
    DarkUp,
    DarkGrid,
    DarkTrellis,
    LightHorizontal,
    LightVertical,
    LightDown,
    LightUp,
    LightGrid,
    LightTrellis,
    Gray125,
    Gray0625,
}

impl Pattern {
    pub const NAMES: [&'static str; 19] = [
        "none",
        "solid",
        "mediumGray",
        "darkGray",
        "lightGray",
        "darkHorizontal",
        "darkVertical",
        "darkDown",
        "darkUp",
        "darkGrid",
        "darkTrellis",
        "lightHorizontal",
        "lightVertical",
        "lightDown",
        "lightUp",
        "lightGrid",
        "lightTrellis",
        "gray125",
        "gray0625",
    ];
    pub const ALL: [Pattern; 19] = [
        Pattern::None,
        Pattern::Solid,
        Pattern::MediumGray,
        Pattern::DarkGray,
        Pattern::LightGray,
        Pattern::DarkHorizontal,
        Pattern::DarkVertical,
        Pattern::DarkDown,
        Pattern::DarkUp,
        Pattern::DarkGrid,
        Pattern::DarkTrellis,
        Pattern::LightHorizontal,
        Pattern::LightVertical,
        Pattern::LightDown,
        Pattern::LightUp,
        Pattern::LightGrid,
        Pattern::LightTrellis,
        Pattern::Gray125,
        Pattern::Gray0625,
    ];
    pub fn name(self) -> &'static str {
        Self::NAMES[self as usize]
    }
    pub fn from_name(s: &str) -> Option<Pattern> {
        Self::NAMES
            .iter()
            .position(|n| *n == s)
            .map(|i| Self::ALL[i])
    }
}

#[derive(Clone, PartialEq, Eq, Hash, Debug, Default)]
pub struct Fill {
    pub pattern: Pattern,
    pub color: CellColor,
    pub pattern_color: CellColor,

    pub raw: Option<Arc<str>>,

    pub cleared: bool,
}

#[derive(Clone, Copy, PartialEq, Eq, Hash, Debug, Default)]
pub enum BorderLine {
    #[default]
    None,
    Thin,
    Medium,
    Dashed,
    Dotted,
    Thick,
    Double,
    Hair,
    MediumDashed,
    DashDot,
    MediumDashDot,
    DashDotDot,
    MediumDashDotDot,
    SlantDashDot,
}

impl BorderLine {
    pub const NAMES: [&'static str; 14] = [
        "none",
        "thin",
        "medium",
        "dashed",
        "dotted",
        "thick",
        "double",
        "hair",
        "mediumDashed",
        "dashDot",
        "mediumDashDot",
        "dashDotDot",
        "mediumDashDotDot",
        "slantDashDot",
    ];
    pub const ALL: [BorderLine; 14] = [
        BorderLine::None,
        BorderLine::Thin,
        BorderLine::Medium,
        BorderLine::Dashed,
        BorderLine::Dotted,
        BorderLine::Thick,
        BorderLine::Double,
        BorderLine::Hair,
        BorderLine::MediumDashed,
        BorderLine::DashDot,
        BorderLine::MediumDashDot,
        BorderLine::DashDotDot,
        BorderLine::MediumDashDotDot,
        BorderLine::SlantDashDot,
    ];
    pub fn name(self) -> &'static str {
        Self::NAMES[self as usize]
    }
    pub fn from_name(s: &str) -> Option<BorderLine> {
        Self::NAMES
            .iter()
            .position(|n| *n == s)
            .map(|i| Self::ALL[i])
    }
}

#[derive(Clone, Copy, PartialEq, Eq, Hash, Debug, Default)]
pub struct Border {
    pub line: BorderLine,
    pub color: CellColor,
}

#[derive(Clone, PartialEq, Eq, Hash, Debug, Default)]
pub struct Borders {
    pub left: Border,
    pub right: Border,
    pub top: Border,
    pub bottom: Border,

    pub diagonal: Border,
    pub diagonal_down: bool,
    pub diagonal_up: bool,
}

#[derive(Clone, Copy, PartialEq, Eq, Hash, Debug)]
pub enum Edge {
    Left,
    Right,
    Top,
    Bottom,
}

impl Edge {
    pub fn opposite(self) -> Edge {
        match self {
            Edge::Left => Edge::Right,
            Edge::Right => Edge::Left,
            Edge::Top => Edge::Bottom,
            Edge::Bottom => Edge::Top,
        }
    }
}

impl Borders {
    pub fn edge(&self, e: Edge) -> Border {
        match e {
            Edge::Left => self.left,
            Edge::Right => self.right,
            Edge::Top => self.top,
            Edge::Bottom => self.bottom,
        }
    }

    pub fn edge_mut(&mut self, e: Edge) -> &mut Border {
        match e {
            Edge::Left => &mut self.left,
            Edge::Right => &mut self.right,
            Edge::Top => &mut self.top,
            Edge::Bottom => &mut self.bottom,
        }
    }
}

impl BorderLine {
    pub fn weight_rank(self) -> u8 {
        match self {
            BorderLine::None => 0,
            BorderLine::Hair => 1,
            BorderLine::Thin
            | BorderLine::Dashed
            | BorderLine::Dotted
            | BorderLine::DashDot
            | BorderLine::DashDotDot => 2,
            BorderLine::Medium
            | BorderLine::MediumDashed
            | BorderLine::MediumDashDot
            | BorderLine::MediumDashDotDot
            | BorderLine::SlantDashDot => 3,
            BorderLine::Thick => 4,
            BorderLine::Double => 5,
        }
    }
}

#[derive(Clone, Copy, PartialEq, Eq, Hash, Debug, Default)]
pub enum HAlign {
    #[default]
    General,
    Left,
    Center,
    Right,
    Fill,
    Justify,
    CenterAcrossSelection,
    Distributed,
}

impl HAlign {
    pub const NAMES: [&'static str; 8] = [
        "general",
        "left",
        "center",
        "right",
        "fill",
        "justify",
        "centerContinuous",
        "distributed",
    ];
    pub const ALL: [HAlign; 8] = [
        HAlign::General,
        HAlign::Left,
        HAlign::Center,
        HAlign::Right,
        HAlign::Fill,
        HAlign::Justify,
        HAlign::CenterAcrossSelection,
        HAlign::Distributed,
    ];
    pub fn name(self) -> &'static str {
        Self::NAMES[self as usize]
    }
    pub fn from_name(s: &str) -> Option<HAlign> {
        Self::NAMES
            .iter()
            .position(|n| *n == s)
            .map(|i| Self::ALL[i])
    }
}

#[derive(Clone, Copy, PartialEq, Eq, Hash, Debug, Default)]
pub enum VAlign {
    Top,
    Center,
    #[default]
    Bottom,
    Justify,
    Distributed,
}

impl VAlign {
    pub const NAMES: [&'static str; 5] = ["top", "center", "bottom", "justify", "distributed"];
    pub const ALL: [VAlign; 5] = [
        VAlign::Top,
        VAlign::Center,
        VAlign::Bottom,
        VAlign::Justify,
        VAlign::Distributed,
    ];
    pub fn name(self) -> &'static str {
        Self::NAMES[self as usize]
    }
    pub fn from_name(s: &str) -> Option<VAlign> {
        Self::NAMES
            .iter()
            .position(|n| *n == s)
            .map(|i| Self::ALL[i])
    }
}

#[derive(Clone, PartialEq, Eq, Hash, Debug, Default)]
pub struct Alignment {
    pub horizontal: HAlign,
    pub vertical: VAlign,
    pub wrap: bool,
    pub indent: u8,

    pub rotation: u8,
    pub shrink: bool,
}

#[derive(Clone, PartialEq, Eq, Hash, Debug)]
pub struct Style {
    pub number_format: Arc<str>,
    pub font: Font,
    pub fill: Fill,
    pub borders: Borders,
    pub alignment: Alignment,
    pub locked: bool,
    pub formula_hidden: bool,

    pub named: Arc<str>,

    pub custom_code: bool,
}

impl Default for Style {
    fn default() -> Style {
        Style {
            number_format: Arc::from("General"),
            font: Font {
                name: Arc::from("Aptos Narrow"),
                size_twips: 220,
                bold: false,
                italic: false,
                underline: Underline::None,
                strikethrough: false,
                superscript: false,
                subscript: false,
                color: CellColor::theme(1, 0.0),
                scheme: FontScheme::Minor,
                family: 2,
            },
            fill: Fill::default(),
            borders: Borders::default(),
            alignment: Alignment::default(),
            locked: true,
            formula_hidden: false,
            named: Arc::from("Normal"),
            custom_code: false,
        }
    }
}

impl Style {
    pub fn with_number_format(&self, fmt: &str) -> Style {
        Style {
            number_format: Arc::from(fmt),
            custom_code: false,
            ..self.clone()
        }
    }
}

#[derive(Clone, PartialEq, Debug)]
pub struct NamedStyle {
    pub name: Arc<str>,

    pub builtin_id: Option<u32>,
    pub style: Style,
    pub include_number: bool,
    pub include_font: bool,
    pub include_alignment: bool,
    pub include_border: bool,
    pub include_patterns: bool,
    pub include_protection: bool,
}

#[derive(Clone, PartialEq, Debug)]
pub struct Theme {
    pub colors: [u32; 12],
    pub major_font: Arc<str>,
    pub minor_font: Arc<str>,
}

impl Default for Theme {
    fn default() -> Theme {
        Theme {
            colors: [
                0xFFFFFF, 0x000000, 0xE8E8E8, 0x0E2841, 0x156082, 0xE97132, 0x196B24, 0x0F9ED5,
                0xA02B93, 0x4EA72E, 0x467886, 0x96607D,
            ],
            major_font: Arc::from("Aptos Display"),
            minor_font: Arc::from("Aptos Narrow"),
        }
    }
}

#[derive(Clone, PartialEq, Debug)]
pub struct StyleTable {
    pub theme: Theme,
    pub named: Vec<NamedStyle>,

    pub dxfs: Vec<Arc<str>>,
}

impl Default for StyleTable {
    fn default() -> StyleTable {
        let mut t = StyleTable {
            theme: Theme::default(),
            named: Vec::new(),
            dxfs: Vec::new(),
        };
        for s in builtin_styles() {
            t.insert(s);
        }
        t
    }
}

impl StyleTable {
    pub fn get(&self, name: &str) -> Option<&NamedStyle> {
        self.named
            .iter()
            .find(|s| s.name.eq_ignore_ascii_case(name))
    }

    pub fn insert(&mut self, s: NamedStyle) {
        if let Some(old) = self
            .named
            .iter_mut()
            .find(|o| o.name.eq_ignore_ascii_case(&s.name))
        {
            *old = s;
            return;
        }
        let k = s.name.to_lowercase();
        let at = self.named.partition_point(|o| o.name.to_lowercase() < k);
        self.named.insert(at, s);
    }
}

fn builtin(name: &str, id: u32, parts: &str, edit: &dyn Fn(&mut Style)) -> NamedStyle {
    let mut style = Style {
        named: Arc::from(name),
        ..Style::default()
    };
    edit(&mut style);
    NamedStyle {
        name: Arc::from(name),
        builtin_id: Some(id),
        style,
        include_number: parts.contains('N'),
        include_font: parts.contains('F'),
        include_alignment: parts.contains('A'),
        include_border: parts.contains('B'),
        include_patterns: parts.contains('P'),
        include_protection: parts.contains('R'),
    }
}

fn solid(c: CellColor) -> Fill {
    Fill {
        pattern: Pattern::Solid,
        color: c,
        pattern_color: CellColor::Auto,
        raw: None,
        cleared: false,
    }
}

fn boxed(line: BorderLine, color: CellColor) -> Borders {
    let b = Border { line, color };
    Borders {
        left: b,
        right: b,
        top: b,
        bottom: b,
        ..Borders::default()
    }
}

fn builtin_styles() -> Vec<NamedStyle> {
    let fmt = |s: &mut Style, f: &str| s.number_format = Arc::from(f);
    let rgb_font = |s: &mut Style, c: u32| s.font.color = CellColor::Rgb(c);
    let mut v = vec![
        builtin("Normal", 0, "NFABPR", &|_| {}),
        builtin("Comma", 3, "N", &|s| {
            fmt(s, r#"_(* #,##0.00_);_(* (#,##0.00);_(* "-"??_);_(@_)"#)
        }),
        builtin("Currency", 4, "N", &|s| {
            fmt(s, r#"_($* #,##0.00_);_($* (#,##0.00);_($* "-"??_);_(@_)"#)
        }),
        builtin("Percent", 5, "N", &|s| fmt(s, "0%")),
        builtin("Comma [0]", 6, "N", &|s| {
            fmt(s, r#"_(* #,##0_);_(* (#,##0);_(* "-"_);_(@_)"#)
        }),
        builtin("Currency [0]", 7, "N", &|s| {
            fmt(s, r#"_($* #,##0_);_($* (#,##0);_($* "-"_);_(@_)"#)
        }),
        builtin("Note", 10, "BP", &|s| {
            s.fill = solid(CellColor::Rgb(0xFFFFCC));
            s.borders = boxed(BorderLine::Thin, CellColor::Rgb(0xB2B2B2));
        }),
        builtin("Warning Text", 11, "F", &|s| rgb_font(s, 0xFF0000)),
        builtin("Title", 15, "F", &|s| {
            s.font.name = Arc::from("Aptos Display");
            s.font.size_twips = 360;
            s.font.scheme = FontScheme::Major;
            s.font.color = CellColor::theme(3, 0.0);
        }),
    ];
    let headings = [
        (300, Some((BorderLine::Thick, 0.0))),
        (260, Some((BorderLine::Thick, 0.499984740745262))),
        (220, Some((BorderLine::Medium, 0.3999755851924192))),
        (220, None),
    ];
    for (i, (size, edge)) in headings.into_iter().enumerate() {
        let name = format!("Heading {}", i + 1);
        let parts = if edge.is_some() { "FB" } else { "F" };
        v.push(builtin(&name, 16 + i as u32, parts, &|s| {
            s.font.size_twips = size;
            s.font.bold = true;
            s.font.color = CellColor::theme(3, 0.0);
            if let Some((line, tint)) = edge {
                s.borders.bottom = Border {
                    line,
                    color: CellColor::theme(4, tint),
                };
            }
        }));
    }
    v.extend([
        builtin("Input", 20, "FBP", &|s| {
            rgb_font(s, 0x3F3F76);
            s.fill = solid(CellColor::Rgb(0xFFCC99));
            s.borders = boxed(BorderLine::Thin, CellColor::Rgb(0x7F7F7F));
        }),
        builtin("Output", 21, "FBP", &|s| {
            rgb_font(s, 0x3F3F3F);
            s.font.bold = true;
            s.fill = solid(CellColor::Rgb(0xF2F2F2));
            s.borders = boxed(BorderLine::Thin, CellColor::Rgb(0x3F3F3F));
        }),
        builtin("Calculation", 22, "FBP", &|s| {
            rgb_font(s, 0xFA7D00);
            s.font.bold = true;
            s.fill = solid(CellColor::Rgb(0xF2F2F2));
            s.borders = boxed(BorderLine::Thin, CellColor::Rgb(0x7F7F7F));
        }),
        builtin("Check Cell", 23, "FBP", &|s| {
            s.font.color = CellColor::theme(0, 0.0);
            s.font.bold = true;
            s.fill = solid(CellColor::Rgb(0xA5A5A5));
            s.borders = boxed(BorderLine::Double, CellColor::Rgb(0x3F3F3F));
        }),
        builtin("Linked Cell", 24, "FB", &|s| {
            rgb_font(s, 0xFA7D00);
            s.borders.bottom = Border {
                line: BorderLine::Double,
                color: CellColor::Rgb(0xFF8001),
            };
        }),
        builtin("Total", 25, "FB", &|s| {
            s.font.bold = true;
            s.borders.top = Border {
                line: BorderLine::Thin,
                color: CellColor::theme(4, 0.0),
            };
            s.borders.bottom = Border {
                line: BorderLine::Double,
                color: CellColor::theme(4, 0.0),
            };
        }),
        builtin("Good", 26, "FP", &|s| {
            rgb_font(s, 0x006100);
            s.fill = solid(CellColor::Rgb(0xC6EFCE));
        }),
        builtin("Bad", 27, "FP", &|s| {
            rgb_font(s, 0x9C0006);
            s.fill = solid(CellColor::Rgb(0xFFC7CE));
        }),
        builtin("Neutral", 28, "FP", &|s| {
            rgb_font(s, 0x9C5700);
            s.fill = solid(CellColor::Rgb(0xFFEB9C));
        }),
        builtin("Explanatory Text", 53, "F", &|s| {
            rgb_font(s, 0x7F7F7F);
            s.font.italic = true;
        }),
    ]);
    let tints = [
        (20, 0.7999816888943144),
        (40, 0.5999938962981048),
        (60, 0.3999755851924192),
    ];
    for k in 0..6u8 {
        let theme = 4 + k;
        let id = 29 + 4 * u32::from(k);
        v.push(builtin(&format!("Accent{}", k + 1), id, "FP", &|s| {
            s.font.color = CellColor::theme(0, 0.0);
            s.fill = solid(CellColor::theme(theme, 0.0));
        }));
        for (j, (pct, tint)) in tints.into_iter().enumerate() {
            let name = format!("{pct}% - Accent{}", k + 1);
            v.push(builtin(&name, id + 1 + j as u32, "FP", &|s| {
                s.fill = solid(CellColor::theme(theme, tint));
            }));
        }
    }
    v
}

pub const PALETTE: [u32; 56] = [
    0x000000, 0xFFFFFF, 0xFF0000, 0x00FF00, 0x0000FF, 0xFFFF00, 0xFF00FF, 0x00FFFF, 0x800000,
    0x008000, 0x000080, 0x808000, 0x800080, 0x008080, 0xC0C0C0, 0x808080, 0x9999FF, 0x993366,
    0xFFFFCC, 0xCCFFFF, 0x660066, 0xFF8080, 0x0066CC, 0xCCCCFF, 0x000080, 0xFF00FF, 0xFFFF00,
    0x00FFFF, 0x800080, 0x800000, 0x008080, 0x0000FF, 0x00CCFF, 0xCCFFFF, 0xCCFFCC, 0xFFFF99,
    0x99CCFF, 0xFF99CC, 0xCC99FF, 0xFFCC99, 0x3366FF, 0x33CCCC, 0x99CC00, 0xFFCC00, 0xFF9900,
    0xFF6600, 0x666699, 0x969696, 0x003366, 0x339966, 0x003300, 0x333300, 0x993300, 0x993366,
    0x333399, 0x333333,
];

pub fn nearest_color_index(rgb: u32) -> u8 {
    let ch = |c: u32, s: u32| i32::try_from((c >> s) & 0xFF).unwrap_or(0);
    let dist = |p: u32| {
        [16, 8, 0]
            .iter()
            .map(|&s| (ch(rgb, s) - ch(p, s)).pow(2))
            .sum::<i32>()
    };
    let mut best = 0;
    for (i, &p) in PALETTE.iter().enumerate() {
        if dist(p) < dist(PALETTE[best]) {
            best = i;
        }
    }
    best as u8 + 1
}

pub fn indexed_rgb(i: u8) -> Option<u32> {
    match i {
        0..=7 => Some(PALETTE[usize::from(i)]),
        8..=63 => Some(PALETTE[usize::from(i) - 8]),
        _ => None,
    }
}

pub fn stored_tint(t: f64) -> f64 {
    (t * 32767.0).trunc() / 32767.0
}

const HLS_MAX: i32 = 240;

fn rgb_to_hs(r: i32, g: i32, b: i32) -> (i32, i32) {
    let (mx, mn) = (r.max(g).max(b), r.min(g).min(b));
    if mx == mn {
        return (160, 0);
    }
    let l = ((mx + mn) * HLS_MAX + 255) / 510;
    let d = mx - mn;
    let s = if l <= HLS_MAX / 2 {
        (d * HLS_MAX + (mx + mn) / 2) / (mx + mn)
    } else {
        (d * HLS_MAX + (510 - mx - mn) / 2) / (510 - mx - mn)
    };
    let part = |c: i32| ((mx - c) * (HLS_MAX / 6) + d / 2) / d;
    let h = if r == mx {
        part(b) - part(g)
    } else if g == mx {
        HLS_MAX / 3 + part(r) - part(b)
    } else {
        2 * HLS_MAX / 3 + part(g) - part(r)
    };
    (h.rem_euclid(HLS_MAX), s)
}

fn hue_to_rgb(n1: i32, n2: i32, h: i32) -> i32 {
    let h = h.rem_euclid(HLS_MAX);
    if h < HLS_MAX / 6 {
        n1 + ((n2 - n1) * h + HLS_MAX / 12).div_euclid(HLS_MAX / 6)
    } else if h < HLS_MAX / 2 {
        n2
    } else if h < 2 * HLS_MAX / 3 {
        n1 + ((n2 - n1) * (2 * HLS_MAX / 3 - h) + HLS_MAX / 12).div_euclid(HLS_MAX / 6)
    } else {
        n1
    }
}

fn hls_to_rgb(h: i32, l: i32, s: i32) -> u32 {
    let to = |v: i32| u32::try_from(v.clamp(0, 255)).unwrap_or(0);
    if s == 0 {
        let v = to(l * 255 / HLS_MAX);
        return (v << 16) | (v << 8) | v;
    }
    let m2 = if l <= HLS_MAX / 2 {
        (l * (HLS_MAX + s) + HLS_MAX / 2) / HLS_MAX
    } else {
        l + s - (l * s + HLS_MAX / 2) / HLS_MAX
    };
    let m1 = 2 * l - m2;
    let c = |hh: i32| to((hue_to_rgb(m1, m2, hh) * 255 + HLS_MAX / 2).div_euclid(HLS_MAX));
    (c(h + HLS_MAX / 3) << 16) | (c(h) << 8) | c(h - HLS_MAX / 3)
}

pub fn apply_tint(rgb: u32, tint: f64) -> u32 {
    if tint == 0.0 {
        return rgb;
    }
    let ch = |s: u32| i32::try_from((rgb >> s) & 0xFF).unwrap_or(0);
    let (r, g, b) = (ch(16), ch(8), ch(0));
    let (h, s) = rgb_to_hs(r, g, b);
    let lum = f64::from(r.max(g).max(b) + r.min(g).min(b)) * f64::from(HLS_MAX) / 510.0;
    let l = if tint < 0.0 {
        lum * (1.0 + tint)
    } else {
        lum * (1.0 - tint) + f64::from(HLS_MAX) * tint
    };
    hls_to_rgb(h, l.floor() as i32, s)
}

impl Theme {
    pub fn rgb(&self, c: CellColor, auto: u32) -> u32 {
        match c {
            CellColor::Auto => auto,
            CellColor::Rgb(v) => v,
            CellColor::Indexed(i) => indexed_rgb(i).unwrap_or(auto),
            CellColor::Theme { index, .. } => {
                let base = self.colors.get(usize::from(index)).copied().unwrap_or(auto);
                apply_tint(base, c.tint())
            }
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn tints_match_excel() {
        let cases = [
            (0xE97132, 13106.0 / 32767.0, 8628721),
            (0x196B24, -8191.0 / 32767.0, 1724434),
            (0xE97132, -8191.0 / 32767.0, 1331390),
            (0xA02B93, 19660.0 / 32767.0, 14524132),
            (0x156082, 0.7999816888943144, 16115392),
            (0x4EA72E, 0.3999755851924192, 7592334),
            (0x4EA72E, 0.7999816888943144, 13693658),
            (0x4EA72E, 0.5999938962981048, 10675893),
            (0x0F9ED5, 0.5999938962981048, 16309396),
        ];
        for (rgb, t, bgr) in cases {
            let got = apply_tint(rgb, t);
            let want = ((bgr & 0xFF) << 16) | (bgr & 0xFF00) | (bgr >> 16);
            assert_eq!(got, want, "{rgb:06X} {t}");
        }
    }

    #[test]
    fn builtin_styles_in_excel_order() {
        let t = StyleTable::default();
        let names: Vec<&str> = t.named.iter().map(|s| &*s.name).collect();
        let want = [
            "20% - Accent1",
            "20% - Accent2",
            "20% - Accent3",
            "20% - Accent4",
            "20% - Accent5",
            "20% - Accent6",
            "40% - Accent1",
            "40% - Accent2",
            "40% - Accent3",
            "40% - Accent4",
            "40% - Accent5",
            "40% - Accent6",
            "60% - Accent1",
            "60% - Accent2",
            "60% - Accent3",
            "60% - Accent4",
            "60% - Accent5",
            "60% - Accent6",
            "Accent1",
            "Accent2",
            "Accent3",
            "Accent4",
            "Accent5",
            "Accent6",
            "Bad",
            "Calculation",
            "Check Cell",
            "Comma",
            "Comma [0]",
            "Currency",
            "Currency [0]",
            "Explanatory Text",
            "Good",
            "Heading 1",
            "Heading 2",
            "Heading 3",
            "Heading 4",
            "Input",
            "Linked Cell",
            "Neutral",
            "Normal",
            "Note",
            "Output",
            "Percent",
            "Title",
            "Total",
            "Warning Text",
        ];
        assert_eq!(names, want);
        let normal = t.get("normal").unwrap();
        assert_eq!(normal.builtin_id, Some(0));
        assert_eq!(normal.style.with_number_format("General"), Style::default());
    }

    #[test]
    fn color_index_is_nearest() {
        assert_eq!(nearest_color_index(0x0000FF), 5);
        assert_eq!(nearest_color_index(0x563412), 52);
        assert_eq!(nearest_color_index(0xF1A983), 22);
        assert_eq!(nearest_color_index(0x0E2841), 49);
    }
}
