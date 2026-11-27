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

    RgbTint {
        rgb: u32,
        tint: u64,
    },

    AutoTint {
        tint: u64,
    },

    IndexedTint {
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

    pub fn is_auto(&self) -> bool {
        matches!(self, CellColor::Auto | CellColor::Indexed(64))
    }

    pub fn tint(&self) -> f64 {
        match self {
            CellColor::Theme { tint, .. }
            | CellColor::RgbTint { tint, .. }
            | CellColor::AutoTint { tint }
            | CellColor::IndexedTint { tint, .. } => f64::from_bits(*tint),
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

    pub outline: bool,
    pub shadow: bool,
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
    pub fn priority_rank(self) -> u8 {
        match self {
            BorderLine::Double => 0,
            BorderLine::Thick => 1,
            BorderLine::Medium => 2,
            BorderLine::MediumDashed => 3,
            BorderLine::MediumDashDot => 4,
            BorderLine::SlantDashDot => 5,
            BorderLine::MediumDashDotDot => 6,
            BorderLine::Thin => 7,
            BorderLine::Dashed => 8,
            BorderLine::Dotted => 9,
            BorderLine::DashDot => 10,
            BorderLine::DashDotDot => 11,
            BorderLine::Hair => 12,
            BorderLine::None => 16,
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

    pub reading_order: u8,

    pub add_indent: bool,
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

    pub implicit_general: bool,
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
                outline: false,
                shadow: false,
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
            implicit_general: false,
        }
    }
}

impl Style {
    pub fn with_number_format(&self, fmt: &str) -> Style {
        Style {
            number_format: Arc::from(fmt),
            custom_code: false,
            implicit_general: false,
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

    pub palette: [u32; 56],
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
            palette: PALETTE,
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

    pub hidden: Vec<HiddenStyle>,
}

#[derive(Clone, PartialEq, Debug)]
pub struct HiddenStyle {
    pub before: usize,
    pub style: NamedStyle,
}

impl Default for StyleTable {
    fn default() -> StyleTable {
        let mut t = StyleTable {
            theme: Theme::default(),
            named: Vec::new(),
            dxfs: Vec::new(),
            hidden: Vec::new(),
        };
        for s in builtin_styles() {
            t.insert(s);
        }
        t
    }
}

impl StyleTable {
    fn slots(&self) -> Vec<(bool, usize)> {
        let mut out = Vec::with_capacity(self.named.len() + self.hidden.len());
        let mut h = 0;
        for v in 0..=self.named.len() {
            while h < self.hidden.len() && self.hidden[h].before <= v {
                out.push((true, h));
                h += 1;
            }
            if v < self.named.len() {
                out.push((false, v));
            }
        }
        out
    }

    fn slot_name(&self, (hid, i): (bool, usize)) -> &str {
        if hid {
            &self.hidden[i].style.name
        } else {
            &self.named[i].name
        }
    }

    fn slot_cmp(&self, slot: (bool, usize), name: &str, key_hidden: bool) -> std::cmp::Ordering {
        order(self.slot_name(slot), name).then_with(|| (!slot.0).cmp(&!key_hidden))
    }

    fn lookup_slot(
        &self,
        slots: &[(bool, usize)],
        name: &str,
        key_hidden: bool,
    ) -> Result<usize, usize> {
        let n = slots.len();
        if n == 0 || self.slot_cmp(slots[n - 1], name, key_hidden).is_lt() {
            return Err(n);
        }
        let (mut lo, mut hi) = (0, n);
        while lo != hi {
            let mid = (hi - lo) / 2 + lo;
            match self.slot_cmp(slots[mid], name, key_hidden) {
                std::cmp::Ordering::Equal => return Ok(mid),
                std::cmp::Ordering::Less => lo = mid + 1,
                std::cmp::Ordering::Greater => hi = mid,
            }
        }
        Err(lo)
    }

    pub fn lookup(&self, name: &str) -> Result<usize, usize> {
        let slots = self.slots();
        match self.lookup_slot(&slots, name, false) {
            Ok(at) => Ok(slots[at].1),
            Err(at) => Err(slots[..at].iter().filter(|s| !s.0).count()),
        }
    }

    pub fn lookup_hidden(&self, name: &str) -> Option<usize> {
        let slots = self.slots();
        self.lookup_slot(&slots, name, true)
            .ok()
            .map(|at| slots[at].1)
    }

    pub fn unhide(&mut self, h: usize) -> NamedStyle {
        let hs = self.hidden.remove(h);
        self.named.insert(hs.before, hs.style.clone());
        for o in &mut self.hidden[h..] {
            o.before += 1;
        }
        hs.style
    }

    pub fn insert_missed(&mut self, s: NamedStyle) {
        let slots = self.slots();
        match self.lookup_slot(&slots, &s.name, true) {
            Ok(at) | Err(at) => self.insert_at_slot(&slots, at, s),
        }
    }

    fn insert_at_slot(&mut self, slots: &[(bool, usize)], at: usize, s: NamedStyle) {
        let named_at = slots[..at].iter().filter(|x| !x.0).count();
        for &(hid, h) in &slots[at..] {
            if hid {
                self.hidden[h].before += 1;
            }
        }
        self.named.insert(named_at, s);
    }

    pub fn get(&self, name: &str) -> Option<&NamedStyle> {
        self.lookup(name).ok().map(|i| &self.named[i])
    }

    pub fn position(&self, name: &str) -> Option<usize> {
        self.lookup(name)
            .ok()
            .or_else(|| self.named.iter().position(|s| order(&s.name, name).is_eq()))
    }

    pub fn replace(&mut self, s: NamedStyle) {
        match self.position(&s.name) {
            Some(i) => self.named[i] = s,
            None => self.insert(s),
        }
    }

    pub fn remove(&mut self, name: &str) {
        if let Some(i) = self.position(name) {
            let s = self.named.remove(i);

            let mut at = self.hidden.len();
            for (k, o) in self.hidden.iter_mut().enumerate() {
                if o.before > i {
                    o.before -= 1;
                    at = at.min(k);
                }
            }
            if s.builtin_id.is_some() {
                self.hidden.insert(
                    at,
                    HiddenStyle {
                        before: i,
                        style: s,
                    },
                );
            }
        }
    }

    pub fn insert_unnamed(&mut self, mut s: NamedStyle) -> Arc<str> {
        let slots = self.slots();
        let at = match self.lookup_slot(&slots, "", false) {
            Ok(at) | Err(at) => at,
        };
        let name: Arc<str> = (1..)
            .map(|n| format!("Style {n}"))
            .find(|n| self.lookup(n).is_err())
            .map(|n| Arc::from(n.as_str()))
            .unwrap_or_default();
        s.name = name.clone();
        s.style.named = name.clone();
        self.insert_at_slot(&slots, at, s);
        name
    }

    pub fn ensure_hyperlink(&mut self) {
        if self.get("Hyperlink").is_none() {
            self.insert(builtin("Hyperlink", 8, "F", &|s| {
                s.font.underline = Underline::Single;
                s.font.color = CellColor::theme(10, 0.0);
            }));
        }
    }

    pub fn builtin_default(name: &str) -> Option<NamedStyle> {
        builtin_styles()
            .into_iter()
            .find(|s| s.name.eq_ignore_ascii_case(name))
    }

    pub fn unsaved_when_unused(&self, deleted: &[Arc<str>]) -> Vec<Arc<str>> {
        let mut order: Vec<(bool, Arc<str>)> = self
            .named
            .iter()
            .map(|n| (n.builtin_id.is_none(), n.name.clone()))
            .chain(deleted.iter().map(|d| (false, d.clone())))
            .collect();
        order.sort_by(|(_, a), (_, b)| self::order(a, b));
        let mut out = Vec::new();
        for (i, (user, name)) in order.iter().enumerate() {
            let Some(at) = name.find('_') else { continue };
            let stem = name[..at].to_lowercase();
            let prev = order[i.saturating_sub(1)].1.to_lowercase();
            if *user && stem.chars().count() >= 2 && prev.starts_with(&stem) {
                out.push(name.clone());
            }
        }
        out
    }

    pub fn insert(&mut self, s: NamedStyle) {
        let slots = self.slots();
        match self.lookup_slot(&slots, &s.name, false) {
            Ok(at) => self.named[slots[at].1] = s,
            Err(at) => self.insert_at_slot(&slots, at, s),
        }
    }
}

fn order(a: &str, b: &str) -> std::cmp::Ordering {
    crate::text::collate_compare(a, b).then_with(|| a.to_lowercase().cmp(&b.to_lowercase()))
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
    nearest_color_index_in(&PALETTE, rgb)
}

pub fn nearest_color_index_in(palette: &[u32; 56], rgb: u32) -> u8 {
    let ch = |c: u32, s: u32| i32::try_from((c >> s) & 0xFF).unwrap_or(0);
    let grey = |c: u32| ch(c, 16) == ch(c, 8) && ch(c, 8) == ch(c, 0);
    let want_grey = grey(rgb);
    let dist = |p: u32| {
        [16, 8, 0]
            .iter()
            .map(|&s| (ch(rgb, s) - ch(p, s)).abs())
            .sum::<i32>()
    };
    let pick = |want_grey: bool| {
        let mut best: Option<(i32, usize)> = None;
        for (i, &p) in palette.iter().enumerate() {
            if want_grey && !grey(p) {
                continue;
            }
            let d = dist(p);
            if best.is_none_or(|(bd, _)| d < bd) {
                best = Some((d, i));
                if d == 0 {
                    break;
                }
            }
        }
        best
    };

    let found = if want_grey { pick(true) } else { None };
    found
        .or_else(|| pick(false))
        .map_or(1, |(_, i)| i as u8 + 1)
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

fn mul_div(a: i32, b: i32, c: i32) -> i32 {
    let n = i64::from(a) * i64::from(b);
    let c = i64::from(c);
    ((if n >= 0 { n + c / 2 } else { n - c / 2 }) / c) as i32
}

fn cv_hls(rgb: u32) -> (i32, i32, i32) {
    let ch = |s: u32| i32::try_from((rgb >> s) & 0xFF).unwrap_or(0);
    let (r, g, b) = (ch(16), ch(8), ch(0));
    let (mx, mn) = (r.max(g).max(b), r.min(g).min(b));
    let l = mul_div(mx + mn, 240, 510);
    let d = mx - mn;
    if d == 0 {
        return (160, l, 0);
    }
    let s = if l <= 120 {
        mul_div(d, 240, mx + mn)
    } else {
        mul_div(d, 240, 510 - (mx + mn))
    };
    let part = |c: i32| mul_div(mx - c, 40, d);
    let h = if r == mx {
        part(b) - part(g)
    } else if g == mx {
        80 + part(r) - part(b)
    } else {
        160 + part(g) - part(r)
    };
    let h = if h < 0 { h + 240 } else { h };
    (if h > 240 { h - 240 } else { h }, l, s)
}

const CV_ICV_TABLE: [(u32, u8); 29] = [
    (0xFCD5B4, 51),
    (0xFABF8F, 52),
    (0x215967, 21),
    (0x7030A0, 36),
    (0x604960, 36),
    (0xB1A0C7, 36),
    (0x806426, 36),
    (0x403151, 28),
    (0xE4DFEC, 46),
    (0xCCC0DA, 46),
    (0xEBF1DE, 42),
    (0xD8E4BC, 11),
    (0xC4D79B, 11),
    (0x4F6228, 17),
    (0xF2DCDB, 45),
    (0x963634, 60),
    (0x4F81BD, 62),
    (0xDCE6F1, 31),
    (0xB8CCE4, 44),
    (0x95B3D7, 30),
    (0x244062, 18),
    (0x1F497D, 56),
    (0xC5D9F1, 31),
    (0x538DD5, 62),
    (0x163656, 18),
    (0x948A54, 19),
    (0xF2F2F2, 22),
    (0xBFBFBF, 55),
    (0x92D050, 50),
];

pub fn cv_color_index_in(palette: &[u32; 56], rgb: u32) -> u8 {
    if let Some(i) = palette.iter().position(|&p| p == rgb) {
        return i as u8 + 1;
    }
    if palette == &PALETTE {
        if let Some(&(_, ci)) = CV_ICV_TABLE.iter().find(|&&(c, _)| c == rgb) {
            return ci - 7;
        }
    }
    let ch = |c: u32, s: u32| i32::try_from((c >> s) & 0xFF).unwrap_or(0);
    let is_grey = |c: u32| {
        let (r, g, b) = (ch(c, 16), ch(c, 8), ch(c, 0));
        (r - g).abs() <= 10 && (g - b).abs() <= 10 && (b - r).abs() <= 10
    };
    let pick = |grey: bool| {
        let (h, l, s) = cv_hls(rgb);
        let mut best: Option<(i32, usize)> = None;
        for (i, &p) in palette.iter().enumerate() {
            let d = if grey {
                if !is_grey(p) {
                    continue;
                }
                [16, 8, 0]
                    .iter()
                    .map(|&s| (ch(p, s) - ch(rgb, s)).abs())
                    .sum()
            } else {
                let (ph, pl, ps) = cv_hls(p);
                let sq = |a: i32, b: i32| {
                    let x = (a - b) as f32;
                    f64::from(x * x)
                };
                (sq(l, pl) * 0.3) as i32 - (sq(s, ps) * -0.1) as i32 - (sq(h, ph) * -0.6) as i32
            };
            if best.is_none_or(|(bd, _)| d < bd) {
                best = Some((d, i));
                if d == 0 {
                    break;
                }
            }
        }
        best
    };
    let found = if is_grey(rgb) { pick(true) } else { None };
    let (_, i) = found.or_else(|| pick(false)).unwrap_or((0, 0));
    i as u8 + 1
}

pub fn cv_color_index(rgb: u32) -> u8 {
    cv_color_index_in(&PALETTE, rgb)
}

pub fn apply_tint_cv(rgb: u32, tint: f64) -> u32 {
    let t = (tint * 32767.0).round() as i32;
    if t == 0 {
        return rgb;
    }
    let (h, l, s) = cv_hls(rgb);
    let l = if t < 0 {
        (32767 + t) * l / 32767
    } else {
        (32767 - t) * l / 32767 - (32767 - t) * 240 / 32767 + 240
    };
    if s == 0 {
        let v = u32::try_from(mul_div(l, 255, 240)).unwrap_or(0);
        return (v << 16) | (v << 8) | v;
    }
    let m2 = if l <= 120 {
        mul_div(l, 240 + s, 240)
    } else {
        s + l - mul_div(l, s, 240)
    };
    let m1 = 2 * l - m2;
    let c = |hh: i32| {
        let hh = if hh < 0 { hh + 240 } else { hh };
        let hh = if hh > 240 { hh - 240 } else { hh };
        let v = if hh < 40 {
            m1 + mul_div(m2 - m1, hh, 40)
        } else if hh < 120 {
            m2
        } else if hh < 160 {
            m1 + mul_div(m2 - m1, 160 - hh, 40)
        } else {
            m1
        };
        u32::try_from(mul_div(v, 255, 240)).unwrap_or(0)
    };
    (c(h + 80) << 16) | (c(h) << 8) | c(h - 80)
}

impl Theme {
    pub fn indexed(&self, i: u8) -> Option<u32> {
        match i {
            8..=63 => self.palette.get(usize::from(i) - 8).copied(),
            _ => indexed_rgb(i),
        }
    }

    pub fn icv_from_rgb(&self, rgb: u32) -> i32 {
        let rgb = rgb & 0xFF_FFFF;
        if let Some(i) = self.palette.iter().position(|&p| p == rgb) {
            return i as i32 + 8;
        }
        PALETTE[..8]
            .iter()
            .position(|&p| p == rgb)
            .map_or(-1, |i| i as i32)
    }

    pub fn color_index(&self, rgb: u32) -> i32 {
        match self.icv_from_rgb(rgb) {
            -1 => i32::from(nearest_color_index(rgb)),
            icv => icv - 7,
        }
    }

    pub fn chart_color_index(&self, rgb: u32) -> u8 {
        nearest_color_index_in(&self.palette, rgb & 0xFF_FFFF)
    }

    pub fn pcv_color_index(&self, rgb: u32) -> u8 {
        cv_color_index_in(&self.palette, rgb & 0xFF_FFFF)
    }

    pub fn rgb_cv(&self, c: CellColor, auto: u32) -> u32 {
        match c {
            CellColor::Theme { index, .. } => {
                let base = self.colors.get(usize::from(index)).copied().unwrap_or(auto);
                apply_tint_cv(base, c.tint())
            }
            c => self.rgb(c, auto),
        }
    }

    pub fn rgb(&self, c: CellColor, auto: u32) -> u32 {
        match c {
            CellColor::Auto | CellColor::AutoTint { .. } => auto,
            CellColor::Rgb(v) => v,
            CellColor::RgbTint { rgb, .. } => apply_tint_cv(rgb, c.tint()),
            CellColor::Indexed(i) => self.indexed(i).unwrap_or(auto),
            CellColor::IndexedTint { index, .. } => {
                apply_tint_cv(self.indexed(index).unwrap_or(auto), c.tint())
            }
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
    fn color_index_reads_the_workbook_palette() {
        let mut t = Theme::default();
        t.palette[2] = 0x0A141E;
        t.palette[0] = 0x0B1621;
        assert_eq!(t.color_index(0x0A141E), 3);
        assert_eq!(t.color_index(0xFF0000), -5);
        assert_eq!(t.color_index(0x000000), -7);
        assert_eq!(t.color_index(0x0A141F), 1);
        assert_eq!(t.color_index(0x646464), 16);
        assert_eq!(Theme::default().color_index(0xE0E0E0), 2);
    }

    #[test]
    fn nearest_readers_follow_the_workbook_palette() {
        let mut t = Theme::default();
        t.palette[4] = 0x123456;
        t.palette[6] = 0x646464;
        t.palette[19] = 0x40A0C0;
        for f in [Theme::pcv_color_index, Theme::chart_color_index] {
            assert_eq!(f(&t, 0x123456), 5);
            assert_eq!(f(&t, 0x646464), 7);
            assert_eq!(f(&t, 0x40A0C0), 20);
            assert_eq!(f(&t, 0x686868), 7);
        }

        assert_eq!(Theme::default().pcv_color_index(0xFF0000), 3);
        assert_eq!(cv_color_index(0x808080), 16);
    }

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
    fn conditional_format_colours_match_excel() {
        let swap = |bgr: u32| ((bgr & 0xFF) << 16) | (bgr & 0xFF00) | (bgr >> 16);
        for (t, bgr) in [
            (16383, 0xE2C7B1),
            (8191, 0xD5AA8A),
            (24575, 0xF1E2D8),
            (-8191, 0xA4683C),
            (-16383, 0x6D4527),
        ] {
            let got = apply_tint_cv(swap(0xC68E63), f64::from(t) / 32767.0);
            assert_eq!(got, swap(bgr), "{t}");
        }

        for (bgr, ci) in [
            (6_524_614, 46),
            (0xE2C7B1, 15),
            (0xB4D5FC, 44),
            (0xFFFFFF, 2),
            (14_617_244, 7),
        ] {
            assert_eq!(cv_color_index(swap(bgr)), ci, "{bgr:06X}");
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
