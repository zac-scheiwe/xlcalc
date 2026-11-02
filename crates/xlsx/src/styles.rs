use crate::xml::Node;
use std::collections::HashMap;
use std::sync::Arc;
use xlcalc::style::{
    Alignment, Border, BorderLine, Borders, CellColor, Fill, Font, FontScheme, HAlign, NamedStyle,
    Pattern, Style, StyleTable, Underline, VAlign,
};

#[derive(Default)]
pub struct Styles {
    custom: HashMap<u32, String>,
    xf_fmt: Vec<u32>,

    xfs: Vec<Style>,

    quote: Vec<bool>,
    table: StyleTable,

    pub warnings: Vec<String>,
}

fn builtin(id: u32) -> Option<&'static str> {
    Some(match id {
        1 => "0",
        2 => "0.00",
        3 => "#,##0",
        4 => "#,##0.00",
        9 => "0%",
        10 => "0.00%",
        11 => "0.00E+00",
        12 => "# ?/?",
        13 => "# ??/??",
        14 => "d/mm/yyyy",
        15 => "d-mmm-yy",
        16 => "d-mmm",
        17 => "mmm-yy",
        18 => "h:mm AM/PM",
        19 => "h:mm:ss AM/PM",
        20 => "h:mm",
        21 => "h:mm:ss",
        22 => "d/mm/yyyy h:mm",
        5 => r##""$"#,##0;\-"$"#,##0"##,
        6 => r##""$"#,##0;[Red]\-"$"#,##0"##,
        7 => r##""$"#,##0.00;\-"$"#,##0.00"##,
        8 => r##""$"#,##0.00;[Red]\-"$"#,##0.00"##,
        37 => r##"#,##0;-#,##0"##,
        38 => r##"#,##0;[Red]-#,##0"##,
        39 => r##"#,##0.00;-#,##0.00"##,
        40 => r##"#,##0.00;[Red]-#,##0.00"##,
        41 => r##"_-* #,##0_-;\-* #,##0_-;_-* "-"_-;_@_-"##,
        42 => r##"_-"$"* #,##0_-;\-"$"* #,##0_-;_-"$"* "-"_-;_@_-"##,
        43 => r##"_-* #,##0.00_-;\-* #,##0.00_-;_-* "-"??_-;_@_-"##,
        44 => r##"_-"$"* #,##0.00_-;\-"$"* #,##0.00_-;_-"$"* "-"??_-;_@_-"##,
        45 => "mm:ss",
        46 => "[h]:mm:ss",
        47 => "mmss.0",
        48 => "##0.0E+0",
        49 => "@",
        _ => return None,
    })
}

impl Styles {
    pub fn read(n: &Node) -> Styles {
        let mut s = Styles::default();
        if let Some(nf) = n.child("numFmts") {
            for f in nf.children("numFmt") {
                if let (Some(id), Some(code)) = (
                    f.attr("numFmtId").and_then(|i| i.parse().ok()),
                    f.attr("formatCode"),
                ) {
                    s.custom.insert(id, code.to_string());
                }
            }
        }
        if let Some(xfs) = n.child("cellXfs") {
            s.xf_fmt = xfs
                .children("xf")
                .map(|x| x.attr("numFmtId").and_then(|i| i.parse().ok()).unwrap_or(0))
                .collect();
        }
        s.read_parts(n);
        s
    }

    fn read_parts(&mut self, n: &Node) {
        let mut w = Vec::new();
        fn list<'a>(n: &'a Node, name: &str, item: &'a str) -> Vec<&'a Node> {
            n.child(name)
                .map(|x| x.children(item).collect())
                .unwrap_or_default()
        }
        let fonts: Vec<Font> = list(n, "fonts", "font")
            .into_iter()
            .map(|f| font(f, &mut w))
            .collect();
        let fills: Vec<Fill> = list(n, "fills", "fill")
            .into_iter()
            .map(|f| fill(f, &mut w))
            .collect();
        let borders: Vec<Borders> = list(n, "borders", "border")
            .into_iter()
            .map(|b| borders(b, &mut w))
            .collect();
        let parts = Parts {
            fonts: &fonts,
            fills: &fills,
            borders: &borders,
        };
        let style_xfs: Vec<(Style, [bool; 6])> = list(n, "cellStyleXfs", "xf")
            .into_iter()
            .map(|x| (parts.style(x, self.fmt(x), &mut w), applies(x)))
            .collect();

        let mut names: HashMap<u32, Arc<str>> = HashMap::new();
        let mut named = Vec::new();
        for c in list(n, "cellStyles", "cellStyle") {
            let (Some(name), Some(id)) = (
                c.attr("name"),
                c.attr("xfId").and_then(|i| i.parse::<u32>().ok()),
            ) else {
                continue;
            };
            let name: Arc<str> = Arc::from(name);
            names.entry(id).or_insert_with(|| name.clone());
            let Some((st, inc)) = style_xfs.get(id as usize) else {
                w.push(format!(
                    "styles: cell style {name} has no cellStyleXfs entry {id}"
                ));
                continue;
            };
            named.push(NamedStyle {
                name: name.clone(),
                builtin_id: c.attr("builtinId").and_then(|i| i.parse().ok()),
                style: Style {
                    named: name,

                    number_format: Arc::from(norm_code(&st.number_format)),
                    ..st.clone()
                },
                include_number: inc[0],
                include_font: inc[1],
                include_alignment: inc[2],
                include_border: inc[3],
                include_patterns: inc[4],
                include_protection: inc[5],
            });
        }

        for s in named {
            self.table.insert(s);
        }
        self.quote = list(n, "cellXfs", "xf")
            .into_iter()
            .map(|x| flag(x, "quotePrefix", false))
            .collect();
        self.xfs = list(n, "cellXfs", "xf")
            .into_iter()
            .map(|x| {
                let mut st = parts.style(x, self.fmt(x), &mut w);
                let id = x.attr("xfId").and_then(|i| i.parse().ok()).unwrap_or(0);
                st.named = names
                    .get(&id)
                    .cloned()
                    .unwrap_or_else(|| Arc::from("Normal"));
                st
            })
            .collect();
        self.table.dxfs = list(n, "dxfs", "dxf")
            .into_iter()
            .map(|d| Arc::from(to_xml(d)))
            .collect();
        self.warnings = w;
    }

    fn fmt(&self, x: &Node) -> Arc<str> {
        let id = x.attr("numFmtId").and_then(|i| i.parse().ok()).unwrap_or(0);
        Arc::from(self.code(id).as_deref().unwrap_or("General"))
    }

    fn code(&self, id: u32) -> Option<String> {
        if id == 0 {
            return None;
        }
        match self.custom.get(&id) {
            Some(c) if c == "General" => None,
            Some(c) => Some(c.clone()),
            None => builtin(id).map(str::to_string),
        }
    }

    pub fn read_theme(&mut self, n: &Node) {
        let Some(el) = n.child("themeElements") else {
            return;
        };
        if let Some(cs) = el.child("clrScheme") {
            let order = [
                "lt1", "dk1", "lt2", "dk2", "accent1", "accent2", "accent3", "accent4", "accent5",
                "accent6", "hlink", "folHlink",
            ];
            for (i, name) in order.iter().enumerate() {
                let rgb = cs.child(name).and_then(|c| {
                    c.child("srgbClr")
                        .and_then(|x| x.attr("val"))
                        .or_else(|| c.child("sysClr").and_then(|x| x.attr("lastClr")))
                        .and_then(|v| u32::from_str_radix(v, 16).ok())
                });
                match rgb {
                    Some(v) => self.table.theme.colors[i] = v,
                    None => self.warnings.push(format!("theme: colour {name} not read")),
                }
            }
        }
        if let Some(fs) = el.child("fontScheme") {
            let face = |k: &str| {
                fs.child(k)
                    .and_then(|f| f.child("latin"))
                    .and_then(|l| l.attr("typeface"))
                    .map(Arc::from)
            };
            if let Some(f) = face("majorFont") {
                self.table.theme.major_font = f;
            }
            if let Some(f) = face("minorFont") {
                self.table.theme.minor_font = f;
            }
        }
    }

    pub fn style_of(&self, xf: u32) -> Option<&Style> {
        self.xfs.get(xf as usize)
    }

    pub fn quote_prefix(&self, xf: u32) -> bool {
        self.quote.get(xf as usize).copied().unwrap_or(false)
    }

    pub fn base(&self) -> Option<Style> {
        self.xfs.first().map(|s| s.with_number_format("General"))
    }

    pub fn table(&self) -> &StyleTable {
        &self.table
    }

    pub fn format_of(&self, xf: u32) -> Option<String> {
        let id = *self.xf_fmt.get(xf as usize)?;
        if id == 0 {
            return None;
        }
        match self.custom.get(&id) {
            Some(c) if c == "General" => None,
            Some(c) => Some(c.clone()),
            None => builtin(id).map(str::to_string),
        }
    }
}

struct Parts<'a> {
    fonts: &'a [Font],
    fills: &'a [Fill],
    borders: &'a [Borders],
}

impl Parts<'_> {
    fn style(&self, x: &Node, number_format: Arc<str>, w: &mut Vec<String>) -> Style {
        let mut s = Style {
            number_format,
            ..Style::default()
        };
        let id = |k: &str| x.attr(k).and_then(|i| i.parse::<usize>().ok()).unwrap_or(0);
        match self.fonts.get(id("fontId")) {
            Some(f) => s.font = f.clone(),
            None if self.fonts.is_empty() => {}
            None => w.push(format!("styles: no font {}", id("fontId"))),
        }
        match self.fills.get(id("fillId")) {
            Some(f) => s.fill = f.clone(),
            None if self.fills.is_empty() => {}
            None => w.push(format!("styles: no fill {}", id("fillId"))),
        }
        match self.borders.get(id("borderId")) {
            Some(b) => s.borders = b.clone(),
            None if self.borders.is_empty() => {}
            None => w.push(format!("styles: no border {}", id("borderId"))),
        }
        if let Some(a) = x.child("alignment") {
            s.alignment = alignment(a, w);
        }
        if let Some(p) = x.child("protection") {
            s.locked = flag(p, "locked", true);
            s.formula_hidden = flag(p, "hidden", false);
        }
        s
    }
}

fn applies(x: &Node) -> [bool; 6] {
    [
        "applyNumberFormat",
        "applyFont",
        "applyAlignment",
        "applyBorder",
        "applyFill",
        "applyProtection",
    ]
    .map(|k| flag(x, k, true))
}

fn flag(n: &Node, key: &str, default: bool) -> bool {
    n.attr(key).map_or(default, |v| v == "1" || v == "true")
}

fn on(n: &Node, key: &str) -> bool {
    n.child(key).is_some_and(|b| flag(b, "val", true))
}

fn color(c: Option<&Node>) -> CellColor {
    let Some(c) = c else {
        return CellColor::Auto;
    };
    if flag(c, "auto", false) {
        return CellColor::Auto;
    }
    if let Some(t) = c.attr("theme").and_then(|t| t.parse().ok()) {
        let tint = c.attr("tint").and_then(|t| t.parse().ok()).unwrap_or(0.0);
        return CellColor::theme(t, tint);
    }
    if let Some(v) = c.attr("rgb").and_then(|v| u32::from_str_radix(v, 16).ok()) {
        return CellColor::Rgb(v & 0xFF_FFFF);
    }
    match c.attr("indexed").and_then(|i| i.parse::<u8>().ok()) {
        Some(i) if i < 64 => CellColor::Indexed(i),
        _ => CellColor::Auto,
    }
}

fn font(f: &Node, w: &mut Vec<String>) -> Font {
    let val = |k: &str| f.child(k).and_then(|x| x.attr("val"));
    let underline = match f.child("u").map(|u| u.attr("val").unwrap_or("single")) {
        None | Some("none") => Underline::None,
        Some("single") => Underline::Single,
        Some("double") => Underline::Double,
        Some("singleAccounting") => Underline::SingleAccounting,
        Some("doubleAccounting") => Underline::DoubleAccounting,
        Some(o) => {
            w.push(format!("styles: font underline {o}"));
            Underline::None
        }
    };
    let scheme = match val("scheme") {
        Some("major") => FontScheme::Major,
        Some("minor") => FontScheme::Minor,
        _ => FontScheme::None,
    };
    let size = val("sz")
        .and_then(|s| s.parse::<f64>().ok())
        .unwrap_or(11.0);
    Font {
        name: Arc::from(val("name").unwrap_or("")),
        size_twips: (size * 20.0).round() as u32,
        bold: on(f, "b"),
        italic: on(f, "i"),
        underline,
        strikethrough: on(f, "strike"),
        superscript: val("vertAlign") == Some("superscript"),
        subscript: val("vertAlign") == Some("subscript"),
        color: color(f.child("color")),
        scheme,
        family: val("family").and_then(|v| v.parse().ok()).unwrap_or(0),
    }
}

fn fill(f: &Node, w: &mut Vec<String>) -> Fill {
    let Some(p) = f.child("patternFill") else {
        return Fill {
            raw: Some(Arc::from(to_xml(f))),
            ..Fill::default()
        };
    };
    let pattern = match p.attr("patternType") {
        None => Pattern::None,
        Some(t) => Pattern::from_name(t).unwrap_or_else(|| {
            w.push(format!("styles: fill pattern {t}"));
            Pattern::None
        }),
    };
    let (fg, bg) = (color(p.child("fgColor")), color(p.child("bgColor")));

    let (color, pattern_color) = if pattern == Pattern::Solid {
        (fg, bg)
    } else {
        (bg, fg)
    };
    Fill {
        pattern,
        color,
        pattern_color,
        raw: None,
        cleared: false,
    }
}

fn borders(b: &Node, w: &mut Vec<String>) -> Borders {
    let mut side = |names: &[&str]| -> Border {
        let Some(s) = names.iter().find_map(|n| b.child(n)) else {
            return Border::default();
        };
        let line = match s.attr("style") {
            None => BorderLine::None,
            Some(t) => BorderLine::from_name(t).unwrap_or_else(|| {
                w.push(format!("styles: border style {t}"));
                BorderLine::None
            }),
        };
        Border {
            line,
            color: if line == BorderLine::None {
                CellColor::Auto
            } else {
                color(s.child("color"))
            },
        }
    };
    Borders {
        left: side(&["left", "start"]),
        right: side(&["right", "end"]),
        top: side(&["top"]),
        bottom: side(&["bottom"]),
        diagonal: side(&["diagonal"]),
        diagonal_down: flag(b, "diagonalDown", false),
        diagonal_up: flag(b, "diagonalUp", false),
    }
}

fn alignment(a: &Node, w: &mut Vec<String>) -> Alignment {
    let horizontal = match a.attr("horizontal") {
        None => HAlign::default(),
        Some(h) => HAlign::from_name(h).unwrap_or_else(|| {
            w.push(format!("styles: horizontal alignment {h}"));
            HAlign::default()
        }),
    };
    let vertical = match a.attr("vertical") {
        None => VAlign::default(),
        Some(v) => VAlign::from_name(v).unwrap_or_else(|| {
            w.push(format!("styles: vertical alignment {v}"));
            VAlign::default()
        }),
    };
    let num = |k: &str| a.attr(k).and_then(|v| v.parse::<u8>().ok()).unwrap_or(0);
    Alignment {
        horizontal,
        vertical,
        wrap: flag(a, "wrapText", false),
        indent: num("indent"),
        rotation: num("textRotation"),
        shrink: flag(a, "shrinkToFit", false),
    }
}

fn to_xml(n: &Node) -> String {
    fn esc(s: &str, attr: bool) -> String {
        let mut o = String::with_capacity(s.len());
        for ch in s.chars() {
            match ch {
                '&' => o.push_str("&amp;"),
                '<' => o.push_str("&lt;"),
                '>' => o.push_str("&gt;"),
                '"' if attr => o.push_str("&quot;"),
                c => o.push(c),
            }
        }
        o
    }
    let mut s = format!("<{}", n.name);
    for (k, v) in &n.attrs {
        s.push_str(&format!(" {k}=\"{}\"", esc(v, true)));
    }
    if n.kids.is_empty() && n.text.is_empty() {
        s.push_str("/>");
        return s;
    }
    s.push('>');
    s.push_str(&esc(&n.text, false));
    for k in &n.kids {
        s.push_str(&to_xml(k));
    }
    s.push_str(&format!("</{}>", n.name));
    s
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn reads_whole_cell_formats() {
        let xml = br#"<styleSheet xmlns="http://schemas.openxmlformats.org/spreadsheetml/2006/main">
<numFmts count="1"><numFmt numFmtId="164" formatCode="0.000"/></numFmts>
<fonts count="2"><font><sz val="11"/><color theme="1"/><name val="Aptos Narrow"/><family val="2"/><scheme val="minor"/></font>
<font><b/><i val="0"/><u val="double"/><sz val="14"/><color rgb="FFFF0000"/><name val="Arial"/></font></fonts>
<fills count="3"><fill><patternFill patternType="none"/></fill><fill><patternFill patternType="gray125"/></fill>
<fill><patternFill patternType="solid"><fgColor theme="4" tint="0.39997558519241921"/><bgColor indexed="64"/></patternFill></fill></fills>
<borders count="2"><border><left/><right/><top/><bottom/><diagonal/></border>
<border diagonalUp="1"><left style="thin"><color indexed="10"/></left><right/><top/><bottom style="double"><color auto="1"/></bottom><diagonal style="hair"/></border></borders>
<cellStyleXfs count="2"><xf numFmtId="0" fontId="0" fillId="0" borderId="0"/>
<xf numFmtId="0" fontId="1" fillId="2" borderId="0" applyNumberFormat="0" applyAlignment="0" applyProtection="0"/></cellStyleXfs>
<cellXfs count="2"><xf numFmtId="0" fontId="0" fillId="0" borderId="0" xfId="0"/>
<xf numFmtId="164" fontId="1" fillId="2" borderId="1" xfId="1"><alignment horizontal="center" vertical="top" wrapText="1" textRotation="90" indent="2"/><protection locked="0" hidden="1"/></xf></cellXfs>
<cellStyles count="2"><cellStyle name="Normal" xfId="0" builtinId="0"/><cellStyle name="Mine" xfId="1"/></cellStyles>
<dxfs count="1"><dxf><font><b/></font></dxf></dxfs>
</styleSheet>"#;
        let s = Styles::read(&crate::xml::parse(xml).unwrap());
        assert!(s.warnings.is_empty(), "{:?}", s.warnings);
        assert_eq!(s.base().unwrap(), Style::default());
        let x = s.style_of(1).unwrap();
        assert_eq!(&*x.number_format, "0.000");
        assert!(x.font.bold && !x.font.italic);
        assert_eq!(x.font.underline, Underline::Double);
        assert_eq!(x.font.size_twips, 280);
        assert_eq!(x.font.color, CellColor::Rgb(0xFF0000));
        assert_eq!(x.fill.pattern, Pattern::Solid);
        assert_eq!(x.fill.color, CellColor::theme(4, 0.399_975_585_192_419_2));
        assert_eq!(x.borders.left.line, BorderLine::Thin);
        assert_eq!(x.borders.left.color, CellColor::Indexed(10));
        assert_eq!(x.borders.bottom.line, BorderLine::Double);
        assert_eq!(x.borders.diagonal.line, BorderLine::Hair);
        assert!(x.borders.diagonal_up && !x.borders.diagonal_down);
        assert_eq!(x.alignment.horizontal, HAlign::Center);
        assert_eq!(x.alignment.vertical, VAlign::Top);
        assert!(x.alignment.wrap);
        assert_eq!((x.alignment.rotation, x.alignment.indent), (90, 2));
        assert!(!x.locked && x.formula_hidden);
        assert_eq!(&*x.named, "Mine");
        let t = s.table();
        assert_eq!(t.named.len(), StyleTable::default().named.len() + 1);
        let mine = t.get("mine").unwrap();
        assert!(mine.include_font && mine.include_patterns && mine.include_border);
        assert!(!mine.include_number && !mine.include_alignment && !mine.include_protection);
        assert_eq!(&*t.dxfs[0], "<dxf><font><b/></font></dxf>");
    }
}

const STYLE_ORDER: &[&str] = &[
    "numFmts",
    "fonts",
    "fills",
    "borders",
    "cellStyleXfs",
    "cellXfs",
    "cellStyles",
    "dxfs",
    "tableStyles",
    "colors",
    "extLst",
];

const NEW_STYLES: &str = concat!(
    "<styleSheet xmlns=\"http://schemas.openxmlformats.org/spreadsheetml/2006/main\">",
    "<fonts count=\"1\"><font><sz val=\"11\"/><color theme=\"1\"/><name val=\"Aptos Narrow\"/>",
    "<family val=\"2\"/><scheme val=\"minor\"/></font></fonts>",
    "<fills count=\"2\"><fill><patternFill patternType=\"none\"/></fill>",
    "<fill><patternFill patternType=\"gray125\"/></fill></fills>",
    "<borders count=\"1\"><border><left/><right/><top/><bottom/><diagonal/></border></borders>",
    "<cellStyleXfs count=\"1\"><xf numFmtId=\"0\" fontId=\"0\" fillId=\"0\" borderId=\"0\"/></cellStyleXfs>",
    "<cellXfs count=\"1\"><xf numFmtId=\"0\" fontId=\"0\" fillId=\"0\" borderId=\"0\" xfId=\"0\"/></cellXfs>",
    "<cellStyles count=\"1\"><cellStyle name=\"Normal\" xfId=\"0\" builtinId=\"0\"/></cellStyles>",
    "<dxfs count=\"0\"/><tableStyles count=\"0\" defaultTableStyle=\"TableStyleMedium2\" ",
    "defaultPivotStyle=\"PivotStyleLight16\"/></styleSheet>"
);

fn norm_code(code: &str) -> String {
    if code.is_empty() || code.eq_ignore_ascii_case("general") {
        "General".into()
    } else {
        xlcalc::format_code_as_read(code)
    }
}

struct List<K> {
    tag: crate::rawxml::Tag,
    items: Vec<String>,
    index: HashMap<K, u32>,
}

impl<K: std::hash::Hash + Eq> List<K> {
    fn read(split: &crate::rawxml::Split, name: &str, values: Vec<K>) -> Result<List<K>, String> {
        let mut l = List {
            tag: crate::rawxml::Tag::new(name),
            items: Vec::new(),
            index: HashMap::new(),
        };
        if let Some(k) = split.kid(name) {
            let s = crate::rawxml::split(k.raw.as_bytes())?;
            l.tag = s.root;
            l.items = s.kids.into_iter().map(|k| k.raw).collect();
        }
        if l.items.len() != values.len() {
            return Err(format!("styles.xml: {name} not read"));
        }
        for (i, v) in values.into_iter().enumerate() {
            l.index.entry(v).or_insert(i as u32);
        }
        Ok(l)
    }

    fn push(&mut self, v: K, xml: String) -> u32 {
        self.items.push(xml);
        let i = (self.items.len() - 1) as u32;
        self.index.insert(v, i);
        i
    }

    fn get(&mut self, v: K, xml: impl FnOnce(&K) -> String) -> u32 {
        if let Some(&i) = self.index.get(&v) {
            return i;
        }
        let x = xml(&v);
        self.push(v, x)
    }

    fn set_first(&mut self, v: K, xml: String) {
        if self.items.is_empty() {
            self.items.push(xml);
        } else {
            self.items[0] = xml;
        }
        self.index.retain(|_, i| *i != 0);
        self.index.insert(v, 0);
    }

    fn write(&self) -> String {
        let mut t = self.tag.clone();
        t.set("count", Some(&self.items.len().to_string()));
        if self.items.is_empty() {
            return t.empty();
        }
        format!("{}{}</{}>", t.open(), self.items.concat(), t.name)
    }
}

type StyleXf = (Style, [bool; 6]);

fn style_xf(n: &NamedStyle) -> StyleXf {
    (
        Style {
            named: Arc::from("Normal"),
            number_format: Arc::from(norm_code(&n.style.number_format)),
            ..n.style.clone()
        },
        [
            n.include_number,
            n.include_font,
            n.include_alignment,
            n.include_border,
            n.include_patterns,
            n.include_protection,
        ],
    )
}

pub struct StyleWriter {
    split: crate::rawxml::Split,

    numfmts: Vec<(u32, String)>,
    fonts: List<Font>,
    fills: List<Fill>,
    borders: List<Borders>,
    style_xfs: List<StyleXf>,

    xfs: List<(Style, bool)>,

    src_xfs: Vec<(Style, bool)>,

    source: Vec<(Style, bool)>,
    table: StyleTable,

    named: HashMap<String, u32>,

    src_cell_styles: HashMap<String, crate::rawxml::Tag>,
}

fn nodes<'a>(n: &'a Node, name: &str, item: &'a str) -> Vec<&'a Node> {
    n.child(name)
        .map(|x| x.children(item).collect())
        .unwrap_or_default()
}

impl StyleWriter {
    pub fn new(
        src: Option<&[u8]>,
        wb: &xlcalc::Workbook,
        fresh: bool,
    ) -> Result<StyleWriter, String> {
        let source = match src {
            Some(b) => {
                let st = Styles::read(&crate::xml::parse(b)?);
                st.xfs
                    .iter()
                    .zip(&st.quote)
                    .map(|(s, q)| (s.with_number_format(&norm_code(&s.number_format)), *q))
                    .collect()
            }
            None => Vec::new(),
        };
        let src = if fresh { None } else { src };
        let bytes = src.unwrap_or(NEW_STYLES.as_bytes());
        let mut split = crate::rawxml::split(bytes)?;

        if split.root.name.contains(':') {
            split.root.set("xmlns", Some(NS_MAIN));
        }
        let node = crate::xml::parse(bytes)?;
        let src_styles = Styles::read(&node);
        let mut w = Vec::new();
        let font_v: Vec<Font> = nodes(&node, "fonts", "font")
            .into_iter()
            .map(|f| font(f, &mut w))
            .collect();
        let fill_v: Vec<Fill> = nodes(&node, "fills", "fill")
            .into_iter()
            .map(|f| fill(f, &mut w))
            .collect();
        let border_v: Vec<Borders> = nodes(&node, "borders", "border")
            .into_iter()
            .map(|b| borders(b, &mut w))
            .collect();
        let parts = Parts {
            fonts: &font_v,
            fills: &fill_v,
            borders: &border_v,
        };
        let sxf_v: Vec<StyleXf> = nodes(&node, "cellStyleXfs", "xf")
            .into_iter()
            .map(|x| {
                let s = parts.style(x, src_styles.fmt(x), &mut w);
                (
                    s.with_number_format(&norm_code(&s.number_format)),
                    applies(x),
                )
            })
            .collect();
        let src_xfs: Vec<(Style, bool)> = src_styles
            .xfs
            .iter()
            .zip(&src_styles.quote)
            .map(|(s, q)| (s.with_number_format(&norm_code(&s.number_format)), *q))
            .collect();
        let mut numfmts = Vec::new();
        if let Some(k) = split.kid("numFmts") {
            for f in crate::rawxml::split(k.raw.as_bytes())?.kids {
                let t = crate::rawxml::Tag::parse(&f.raw)?;
                if let (Some(id), Some(code)) = (
                    t.get("numFmtId").and_then(|i| i.parse().ok()),
                    t.value("formatCode"),
                ) {
                    numfmts.push((id, code));
                }
            }
        }
        let mut src_cell_styles = HashMap::new();
        if let Some(k) = split.kid("cellStyles") {
            for c in crate::rawxml::split(k.raw.as_bytes())?.kids {
                let mut t = crate::rawxml::Tag::parse(&c.raw)?;
                t.empty = true;
                if let Some(n) = t.value("name") {
                    src_cell_styles.entry(n.to_lowercase()).or_insert(t);
                }
            }
        }
        let mut sw = StyleWriter {
            fonts: List::read(&split, "fonts", font_v)?,
            fills: List::read(&split, "fills", fill_v)?,
            borders: List::read(&split, "borders", border_v)?,
            style_xfs: List::read(&split, "cellStyleXfs", sxf_v)?,
            xfs: List::read(&split, "cellXfs", src_xfs.clone())?,
            split,
            numfmts,
            src_xfs,
            source,
            table: wb.style_table().clone(),
            named: HashMap::new(),
            src_cell_styles,
        };

        let normal = match sw.table.get("Normal") {
            Some(n) => style_xf(n),
            None => (wb.base_style().with_number_format("General"), [true; 6]),
        };
        if sw.style_xfs.index.get(&normal) != Some(&0) {
            let xml = sw.style_xf_xml(&normal);
            sw.style_xfs.set_first(normal, xml);
        }
        sw.named.insert("normal".into(), 0);
        let defaults = StyleTable::default();
        for n in sw.table.named.clone() {
            let key = n.name.to_lowercase();
            let builtin_default = n.builtin_id.is_some() && defaults.get(&n.name) == Some(&n);
            if !builtin_default || sw.src_cell_styles.contains_key(&key) {
                sw.add_named(&n);
            }
        }

        let base = (wb.base_style().clone(), false);
        if sw.src_xfs.first() != Some(&base) {
            let xml = sw.xf_xml(&base);
            sw.xfs.set_first(base.clone(), xml);
            match sw.src_xfs.first_mut() {
                Some(s) => *s = base,
                None => sw.src_xfs.push(base),
            }
        }
        Ok(sw)
    }

    fn add_named(&mut self, n: &NamedStyle) -> u32 {
        let key = n.name.to_lowercase();
        if let Some(&i) = self.named.get(&key) {
            return i;
        }
        let v = style_xf(n);
        let i = match self.style_xfs.index.get(&v) {
            Some(&i) => i,
            None => {
                let xml = self.style_xf_xml(&v);
                self.style_xfs.push(v, xml)
            }
        };
        self.named.insert(key, i);
        i
    }

    fn fmt_id(&mut self, code: &str) -> u32 {
        let code = norm_code(code);
        if code == "General" {
            return 0;
        }
        if let Some((id, _)) = self.numfmts.iter().find(|(_, c)| norm_code(c) == code) {
            return *id;
        }
        if let Some(id) = (1..=49).find(|&i| builtin(i).is_some_and(|b| norm_code(b) == code)) {
            return id;
        }
        let id = self
            .numfmts
            .iter()
            .map(|(i, _)| *i + 1)
            .max()
            .unwrap_or(164)
            .max(164);
        self.numfmts.push((id, file_code(&code)));
        id
    }

    fn part_ids(&mut self, s: &Style) -> [u32; 4] {
        [
            self.fmt_id(&s.number_format),
            self.fonts.get(s.font.clone(), font_xml),
            self.fills.get(s.fill.clone(), fill_xml),
            self.borders.get(s.borders.clone(), borders_xml),
        ]
    }

    fn style_xf_xml(&mut self, (s, inc): &StyleXf) -> String {
        let ids = self.part_ids(s);
        let mut t = crate::rawxml::Tag::new("xf");
        for (k, v) in ["numFmtId", "fontId", "fillId", "borderId"].iter().zip(ids) {
            t.set(k, Some(&v.to_string()));
        }
        let keys = [
            "applyNumberFormat",
            "applyFont",
            "applyAlignment",
            "applyBorder",
            "applyFill",
            "applyProtection",
        ];
        for (k, on) in keys.iter().zip(inc) {
            if !on {
                t.set(k, Some("0"));
            }
        }
        xf_with_children(t, s)
    }

    fn xf_xml(&mut self, (s, quote): &(Style, bool)) -> String {
        let ids = self.part_ids(s);
        let named = self.table.get(&s.named).cloned();
        let xf_id = named.as_ref().map_or(0, |n| self.add_named(n));
        let parent = named.map(|n| n.style).unwrap_or_default();
        let mut t = crate::rawxml::Tag::new("xf");
        for (k, v) in ["numFmtId", "fontId", "fillId", "borderId", "xfId"]
            .iter()
            .zip(ids.into_iter().chain([xf_id]))
        {
            t.set(k, Some(&v.to_string()));
        }
        let differs = [
            (
                "applyNumberFormat",
                norm_code(&parent.number_format) != *s.number_format,
            ),
            ("applyFont", parent.font != s.font),
            ("applyFill", parent.fill != s.fill),
            ("applyBorder", parent.borders != s.borders),
            ("applyAlignment", parent.alignment != s.alignment),
            (
                "applyProtection",
                (parent.locked, parent.formula_hidden) != (s.locked, s.formula_hidden),
            ),
        ];
        for (k, d) in differs {
            if d {
                t.set(k, Some("1"));
            }
        }
        if *quote {
            t.set("quotePrefix", Some("1"));
        }
        xf_with_children(t, s)
    }

    pub fn xf(&mut self, s: &Style, quote: bool, src: Option<u32>) -> u32 {
        let key = (s.clone(), quote);
        if let Some(i) = src {
            if self.src_xfs.get(i as usize) == Some(&key) {
                return i;
            }
        }
        if let Some(&i) = self.xfs.index.get(&key) {
            return i;
        }
        let xml = self.xf_xml(&key);
        self.xfs.push(key, xml)
    }

    pub fn map_source(&mut self, i: u32) -> u32 {
        match self.source.get(i as usize).cloned() {
            Some((s, q)) => self.xf(&s, q, Some(i)),
            None => 0,
        }
    }

    pub fn write(mut self) -> Vec<u8> {
        use crate::rawxml::{escape_attr, Tag};
        let numfmts = (!self.numfmts.is_empty()).then(|| {
            let mut s = format!("<numFmts count=\"{}\">", self.numfmts.len());
            for (id, code) in &self.numfmts {
                s.push_str(&format!(
                    "<numFmt numFmtId=\"{id}\" formatCode=\"{}\"/>",
                    escape_attr(code)
                ));
            }
            s.push_str("</numFmts>");
            s
        });
        let mut cell_styles: Vec<(u32, String)> = Vec::new();
        let mut written = std::collections::HashSet::new();
        for n in &self.table.named {
            let key = n.name.to_lowercase();
            let Some(&xf) = self.named.get(&key) else {
                continue;
            };
            let mut t = self.src_cell_styles.get(&key).cloned().unwrap_or_else(|| {
                let mut t = Tag::new("cellStyle");
                t.set("name", Some(&n.name));
                if let Some(b) = n.builtin_id {
                    t.set("builtinId", Some(&b.to_string()));
                }
                t
            });
            t.set("xfId", Some(&xf.to_string()));
            cell_styles.push((xf, t.empty()));
            written.insert(key);
        }
        if !written.contains("normal") {
            cell_styles.push((
                0,
                "<cellStyle name=\"Normal\" xfId=\"0\" builtinId=\"0\"/>".into(),
            ));
        }
        cell_styles.sort_by_key(|(x, _)| *x);
        let mut cs_tag = match self.split.kid("cellStyles") {
            Some(k) => crate::rawxml::head_len(&k.raw)
                .and_then(|n| Tag::parse(&k.raw[..n]).ok())
                .unwrap_or_else(|| Tag::new("cellStyles")),
            None => Tag::new("cellStyles"),
        };
        cs_tag.set("count", Some(&cell_styles.len().to_string()));
        let cell_styles = format!(
            "{}{}</{}>",
            cs_tag.open(),
            cell_styles.into_iter().map(|(_, s)| s).collect::<String>(),
            cs_tag.name
        );
        let dxfs = if self.table.dxfs.is_empty() {
            "<dxfs count=\"0\"/>".to_string()
        } else {
            format!(
                "<dxfs count=\"{}\">{}</dxfs>",
                self.table.dxfs.len(),
                self.table.dxfs.iter().map(|d| &**d).collect::<String>()
            )
        };
        let kids = [
            ("numFmts", numfmts),
            ("fonts", Some(self.fonts.write())),
            ("fills", Some(self.fills.write())),
            ("borders", Some(self.borders.write())),
            ("cellStyleXfs", Some(self.style_xfs.write())),
            ("cellXfs", Some(self.xfs.write())),
            ("cellStyles", Some(cell_styles)),
            ("dxfs", Some(dxfs)),
        ];
        for (name, raw) in kids {
            self.split.put(name, raw, STYLE_ORDER);
        }
        self.split.write()
    }
}

const NS_MAIN: &str = "http://schemas.openxmlformats.org/spreadsheetml/2006/main";

fn xf_with_children(t: crate::rawxml::Tag, s: &Style) -> String {
    let mut kids = String::new();
    let a = &s.alignment;
    if *a != Alignment::default() {
        let mut at = crate::rawxml::Tag::new("alignment");
        if a.horizontal != HAlign::default() {
            at.set("horizontal", Some(a.horizontal.name()));
        }
        if a.vertical != VAlign::default() {
            at.set("vertical", Some(a.vertical.name()));
        }
        if a.rotation != 0 {
            at.set("textRotation", Some(&a.rotation.to_string()));
        }
        if a.wrap {
            at.set("wrapText", Some("1"));
        }
        if a.indent != 0 {
            at.set("indent", Some(&a.indent.to_string()));
        }
        if a.shrink {
            at.set("shrinkToFit", Some("1"));
        }
        kids.push_str(&at.empty());
    }
    if !s.locked || s.formula_hidden {
        let mut p = crate::rawxml::Tag::new("protection");
        if !s.locked {
            p.set("locked", Some("0"));
        }
        if s.formula_hidden {
            p.set("hidden", Some("1"));
        }
        kids.push_str(&p.empty());
    }
    if kids.is_empty() {
        t.empty()
    } else {
        format!("{}{kids}</xf>", t.open())
    }
}

fn color_xml(name: &str, c: CellColor, auto: bool) -> String {
    match c {
        CellColor::Auto if auto => format!("<{name} indexed=\"64\"/>"),
        CellColor::Auto => String::new(),
        CellColor::Rgb(v) => format!("<{name} rgb=\"FF{v:06X}\"/>"),
        CellColor::Indexed(i) => format!("<{name} indexed=\"{i}\"/>"),
        CellColor::Theme { index, .. } => {
            let t = c.tint();
            if t == 0.0 {
                format!("<{name} theme=\"{index}\"/>")
            } else {
                format!("<{name} theme=\"{index}\" tint=\"{t:?}\"/>")
            }
        }
    }
}

fn font_xml(f: &Font) -> String {
    let mut s = String::from("<font>");
    for (on, el) in [
        (f.bold, "<b/>"),
        (f.italic, "<i/>"),
        (f.strikethrough, "<strike/>"),
    ] {
        if on {
            s.push_str(el);
        }
    }
    s.push_str(match f.underline {
        Underline::None => "",
        Underline::Single => "<u/>",
        Underline::Double => "<u val=\"double\"/>",
        Underline::SingleAccounting => "<u val=\"singleAccounting\"/>",
        Underline::DoubleAccounting => "<u val=\"doubleAccounting\"/>",
    });
    if f.superscript {
        s.push_str("<vertAlign val=\"superscript\"/>");
    } else if f.subscript {
        s.push_str("<vertAlign val=\"subscript\"/>");
    }
    let size = if f.size_twips.is_multiple_of(20) {
        (f.size_twips / 20).to_string()
    } else {
        format!("{:?}", f64::from(f.size_twips) / 20.0)
    };
    s.push_str(&format!("<sz val=\"{size}\"/>"));
    s.push_str(&color_xml("color", f.color, false));
    s.push_str(&format!(
        "<name val=\"{}\"/>",
        crate::rawxml::escape_attr(&f.name)
    ));
    if f.family != 0 {
        s.push_str(&format!("<family val=\"{}\"/>", f.family));
    }
    s.push_str(match f.scheme {
        FontScheme::None => "",
        FontScheme::Major => "<scheme val=\"major\"/>",
        FontScheme::Minor => "<scheme val=\"minor\"/>",
    });
    s.push_str("</font>");
    s
}

fn fill_xml(f: &Fill) -> String {
    if let Some(r) = &f.raw {
        return r.to_string();
    }
    if f.pattern == Pattern::None {
        return "<fill><patternFill patternType=\"none\"/></fill>".into();
    }

    let (fg, bg) = if f.pattern == Pattern::Solid {
        (f.color, f.pattern_color)
    } else {
        (f.pattern_color, f.color)
    };
    format!(
        "<fill><patternFill patternType=\"{}\">{}{}</patternFill></fill>",
        f.pattern.name(),
        color_xml("fgColor", fg, false),
        color_xml("bgColor", bg, false)
    )
}

fn borders_xml(b: &Borders) -> String {
    let mut t = crate::rawxml::Tag::new("border");
    if b.diagonal_up {
        t.set("diagonalUp", Some("1"));
    }
    if b.diagonal_down {
        t.set("diagonalDown", Some("1"));
    }
    let mut s = t.open();
    for (name, e) in [
        ("left", b.left),
        ("right", b.right),
        ("top", b.top),
        ("bottom", b.bottom),
        ("diagonal", b.diagonal),
    ] {
        if e.line == BorderLine::None {
            s.push_str(&format!("<{name}/>"));
        } else {
            s.push_str(&format!(
                "<{name} style=\"{}\">{}</{name}>",
                e.line.name(),
                color_xml("color", e.color, true)
            ));
        }
    }
    s.push_str("</border>");
    s
}

fn file_code(code: &str) -> String {
    let mut out = String::with_capacity(code.len() + 4);
    let mut it = code.chars();
    let (mut quoted, mut bracket) = (false, false);
    while let Some(c) = it.next() {
        match c {
            '"' => quoted = !quoted,
            '[' if !quoted => bracket = true,
            ']' if !quoted => bracket = false,
            '\\' if !quoted => {
                out.push(c);
                out.extend(it.next());
                continue;
            }
            '$' if !quoted && !bracket => {
                out.push_str("\"$\"");
                continue;
            }
            _ => {}
        }
        out.push(c);
    }
    out
}

#[cfg(test)]
mod writer_tests {
    use super::*;

    #[test]
    fn dollars() {
        assert_eq!(super::file_code("$#,##0.00"), "\"$\"#,##0.00");
        assert_eq!(
            super::file_code(r#"[$$-409]0;\$0;"$"0"#),
            r#"[$$-409]0;\$0;"$"0"#
        );
    }

    #[test]
    fn writes_into_a_prefixed_style_sheet() {
        let src = br#"<x:styleSheet xmlns:x="http://schemas.openxmlformats.org/spreadsheetml/2006/main"><x:fonts count="1"><x:font><x:sz val="11"/><x:name val="Calibri"/></x:font></x:fonts><x:fills count="2"><x:fill><x:patternFill patternType="none"/></x:fill><x:fill><x:patternFill patternType="gray125"/></x:fill></x:fills><x:borders count="1"><x:border/></x:borders><x:cellStyleXfs count="1"><x:xf numFmtId="0" fontId="0" fillId="0" borderId="0"/></x:cellStyleXfs><x:cellXfs count="1"><x:xf numFmtId="0" fontId="0" fillId="0" borderId="0" xfId="0"/></x:cellXfs><x:cellStyles count="1"><x:cellStyle name="Normal" xfId="0" builtinId="0"/></x:cellStyles><x:dxfs count="1"><x:dxf><x:font><x:i/></x:font></x:dxf></x:dxfs></x:styleSheet>"#;
        let mut wb = xlcalc::Workbook::new();
        let first = Styles::read(&crate::xml::parse(src).unwrap());
        *wb.style_table_mut() = first.table().clone();
        wb.set_base_style(first.base().unwrap());
        let mut bold = first.base().unwrap();
        bold.font.bold = true;
        bold.number_format = Arc::from("$0.00");
        let mut w = StyleWriter::new(Some(src), &wb, false).unwrap();
        assert_eq!(w.xf(wb.base_style(), false, None), 0);
        assert_eq!(w.xf(&bold, true, None), 1);
        let out = String::from_utf8(w.write()).unwrap();
        assert!(
            out.contains(r#"xmlns="http://schemas.openxmlformats.org/spreadsheetml/2006/main""#)
        );
        assert!(out.contains(r#"formatCode="&quot;$&quot;0.00""#), "{out}");
        assert!(out.contains("<x:font><x:sz val=\"11\"/>"), "{out}");
        let back = Styles::read(&crate::xml::parse(out.as_bytes()).unwrap());
        assert!(back.warnings.is_empty(), "{:?}", back.warnings);
        let x = back.style_of(1).unwrap();
        assert!(x.font.bold && x.font.name.as_ref() == "Calibri");
        assert!(back.quote_prefix(1) && !back.quote_prefix(0));
        assert_eq!(back.table().dxfs, first.table().dxfs);
    }
}
