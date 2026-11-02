use std::collections::BTreeMap;
use std::fmt::Write as _;

use xlcalc::style::{
    Alignment, BorderLine, CellColor, HAlign, NamedStyle, Pattern, Style, Underline, VAlign,
};
use xlcalc::{CellAddr, NumberFormat, Rect, SheetId, Value, Workbook};

type StyleDefn = (Vec<(String, String)>, Style, Facets);

const EOL: &str = "\r\n";
const WRAP: usize = 80;

const NAMED_IDS: [(&str, u32); 46] = [
    ("Comma", 16),
    ("Comma [0]", 17),
    ("Currency", 18),
    ("Currency [0]", 19),
    ("Percent", 20),
    ("Title", 21),
    ("Heading 1", 22),
    ("Heading 2", 23),
    ("Heading 3", 24),
    ("Heading 4", 25),
    ("Good", 26),
    ("Bad", 27),
    ("Neutral", 28),
    ("Input", 29),
    ("Output", 30),
    ("Calculation", 31),
    ("Linked Cell", 32),
    ("Check Cell", 33),
    ("Warning Text", 34),
    ("Note", 35),
    ("Explanatory Text", 36),
    ("Total", 37),
    ("Accent1", 38),
    ("20% - Accent1", 39),
    ("40% - Accent1", 40),
    ("60% - Accent1", 41),
    ("Accent2", 42),
    ("20% - Accent2", 43),
    ("40% - Accent2", 44),
    ("60% - Accent2", 45),
    ("Accent3", 46),
    ("20% - Accent3", 47),
    ("40% - Accent3", 48),
    ("60% - Accent3", 49),
    ("Accent4", 50),
    ("20% - Accent4", 51),
    ("40% - Accent4", 52),
    ("60% - Accent4", 53),
    ("Accent5", 54),
    ("20% - Accent5", 55),
    ("40% - Accent5", 56),
    ("60% - Accent5", 57),
    ("Accent6", 58),
    ("20% - Accent6", 59),
    ("40% - Accent6", 60),
    ("60% - Accent6", 61),
];

const FIRST_XF: u32 = 62;

fn esc(s: &str) -> String {
    let mut o = String::with_capacity(s.len());
    for c in s.chars() {
        match c {
            '&' => o.push_str("&amp;"),
            '<' => o.push_str("&lt;"),
            '>' => o.push_str("&gt;"),
            '"' => o.push_str("&quot;"),
            c => o.push(c),
        }
    }
    o
}

fn rk_hundredths(x: f64) -> Option<i64> {
    let p = x * 100.0;
    if p.fract() != 0.0 || p.abs() >= 536_870_912.0 || p / 100.0 != x {
        return None;
    }
    let bits = p.to_bits();
    let shortcut = bits & 0xffff_ffff == 0 && (bits >> 32) & 3 == 0;
    if shortcut && p.abs() >= 2_097_152.0 {
        return None;
    }
    Some(p as i64)
}

pub(crate) fn xml_number(x: f64) -> String {
    if x == 0.0 {
        return "0".into();
    }
    let neg = x < 0.0;
    let a = x.abs();
    if let Some(n) = rk_hundredths(a) {
        let (i, f) = (n / 100, n % 100);
        let mut out = String::new();
        if neg {
            out.push('-');
        }
        let _ = write!(out, "{i}");
        if f != 0 {
            let _ = write!(out, ".{f:02}");
            while out.ends_with('0') {
                out.pop();
            }
        }
        return out;
    }
    let s = format!("{a:.16e}");
    let (m, e) = s.split_once('e').unwrap_or((&s, "0"));
    let digits: String = m.chars().filter(|c| *c != '.').collect();
    let exp: i32 = e.parse().unwrap_or(0);
    let digits = digits.trim_end_matches('0');
    let digits = if digits.is_empty() { "0" } else { digits };
    let n = digits.len() as i32;
    let exponent_form = !(-1..16).contains(&exp);
    let mut out = String::new();
    if neg {
        out.push('-');
    }
    if exponent_form {
        out.push_str(&digits[..1]);
        if n > 1 {
            out.push('.');
            out.push_str(&digits[1..]);
        }
        let _ = write!(out, "E{}{}", if exp < 0 { '-' } else { '+' }, exp.abs());
    } else if exp >= 0 {
        if n <= exp + 1 {
            out.push_str(digits);
            out.push_str(&"0".repeat((exp + 1 - n) as usize));
        } else {
            out.push_str(&digits[..(exp + 1) as usize]);
            out.push('.');
            out.push_str(&digits[(exp + 1) as usize..]);
        }
    } else {
        out.push_str("0.");
        out.push_str(&"0".repeat((-exp - 1) as usize));
        out.push_str(digits);
    }
    out
}

fn days_to_civil(z: i64) -> (i64, u32, u32) {
    let z = z + 719_468;
    let era = if z >= 0 { z } else { z - 146_096 } / 146_097;
    let doe = z - era * 146_097;
    let yoe = (doe - doe / 1460 + doe / 36_524 - doe / 146_096) / 365;
    let y = yoe + era * 400;
    let doy = doe - (365 * yoe + yoe / 4 - yoe / 100);
    let mp = (5 * doy + 2) / 153;
    let d = (doy - (153 * mp + 2) / 5 + 1) as u32;
    let m = if mp < 10 { mp + 3 } else { mp - 9 } as u32;
    (if m <= 2 { y + 1 } else { y }, m, d)
}

fn iso(x: f64, millis: bool) -> Option<String> {
    if !(0.0..2_958_466.0).contains(&x) {
        return None;
    }
    let total_ms = (x * 86_400_000.0).round() as i64;
    let days = total_ms.div_euclid(86_400_000);
    let ms = total_ms.rem_euclid(86_400_000);
    let (y, m, d) = if days < 60 {
        if days == 0 {
            (1899, 12, 31)
        } else {
            (1900, if days <= 31 { 1 } else { 2 }, {
                if days <= 31 {
                    days as u32
                } else {
                    days as u32 - 31
                }
            })
        }
    } else if days == 60 {
        (1900, 2, 29)
    } else {
        days_to_civil(days - 25_569)
    };
    let (h, mi, s, f) = (ms / 3_600_000, ms / 60_000 % 60, ms / 1000 % 60, ms % 1000);
    let mut o = format!("{y:04}-{m:02}-{d:02}T{h:02}:{mi:02}:{s:02}");
    if millis {
        let _ = write!(o, ".{f:03}");
    }
    Some(o)
}

struct Xml {
    out: String,
}

impl Xml {
    fn new() -> Xml {
        Xml { out: String::new() }
    }

    fn line(&mut self, s: &str) {
        self.out.push_str(s);
        self.out.push_str(EOL);
    }

    fn tag(
        &mut self,
        indent: usize,
        name: &str,
        attrs: &[(String, String)],
        close: &str,
        tail: &str,
    ) {
        let pad = " ".repeat(indent);
        let mut line = format!("<{name}");
        let mut first = true;
        let mut lines: Vec<String> = Vec::new();
        for (i, (k, v)) in attrs.iter().enumerate() {
            let a = format!("{k}=\"{}\"", esc(v));
            let last = i + 1 == attrs.len();
            let need = line.len() + 1 + a.len() + if last { close.len() } else { 0 };
            if !first && need > WRAP {
                lines.push(std::mem::take(&mut line));
                line = a;
            } else {
                line.push(' ');
                line.push_str(&a);
            }
            first = false;
        }
        line.push_str(close);
        lines.push(line);
        for (i, l) in lines.iter().enumerate() {
            self.out.push_str(&pad);
            if i > 0 {
                self.out.push(' ');
            }
            self.out.push_str(l);
            if i + 1 < lines.len() {
                self.out.push_str(EOL);
            }
        }
        self.out.push_str(tail);
        self.out.push_str(EOL);
    }
}

fn at(k: &str, v: impl Into<String>) -> (String, String) {
    (k.to_string(), v.into())
}

fn is_date_code(code: &str) -> bool {
    NumberFormat::parse(code).is_ok_and(|f| f.is_date())
}

fn xml_format(code: &str) -> String {
    match code {
        "0.00" => return "Fixed".into(),
        "#,##0.00" => return "Standard".into(),
        "0.00%" => return "Percent".into(),
        "0.00E+00" => return "Scientific".into(),
        "d-mmm-yy" => return "Medium Date".into(),
        "h:mm AM/PM" => return "Medium Time".into(),
        "h:mm:ss AM/PM" => return "Long Time".into(),
        "h:mm" => return "Short Time".into(),
        "General" => return String::new(),
        _ => {}
    }
    let date = is_date_code(code);
    let chars: Vec<char> = code.chars().collect();
    let mut out = String::new();
    if date && code.starts_with("[$-409]") {
        out.push_str("[ENG]");
    }
    let mut i = 0;
    while i < chars.len() {
        let c = chars[i];
        let rest: String = chars[i..].iter().collect();
        match c {
            '\\' => {
                out.push(c);
                if let Some(n) = chars.get(i + 1) {
                    out.push(*n);
                    i += 1;
                }
            }
            '"' => {
                out.push(c);
                i += 1;
                while i < chars.len() && chars[i] != '"' {
                    out.push(chars[i]);
                    i += 1;
                }
                if i < chars.len() {
                    out.push('"');
                }
            }
            '[' => {
                while i < chars.len() && chars[i] != ']' {
                    out.push(chars[i]);
                    i += 1;
                }
                if i < chars.len() {
                    out.push(']');
                }
            }
            '_' | '*' => {
                out.push(c);
                if let Some(n) = chars.get(i + 1) {
                    out.push(*n);
                    i += 1;
                }
            }
            '0' | '#' | '?' | '.' | '%' | '@' | '/' | ':' | ';' => out.push(c),
            ',' if !date => out.push(c),
            'E' | 'e' if matches!(chars.get(i + 1), Some('+' | '-')) && !date => {
                out.push('E');
                out.push(chars[i + 1]);
                i += 1;
            }
            'd' | 'D' | 'm' | 'M' | 'y' | 'Y' | 'h' | 'H' | 's' | 'S' if date => {
                out.push(c.to_ascii_lowercase())
            }
            'e' | 'E' => out.push('y'),
            'A' | 'a' if rest.to_ascii_uppercase().starts_with("AM/PM") => {
                out.push_str(&rest[..5]);
                i += 4;
            }
            'A' | 'a' if rest.to_ascii_uppercase().starts_with("A/P") => {
                out.push_str(&rest[..3]);
                i += 2;
            }
            'G' if rest.starts_with("General") => {
                out.push_str("General");
                i += 6;
            }
            '$' => out.push_str("\"$\""),
            c => {
                out.push('\\');
                out.push(c);
            }
        }
        i += 1;
    }
    out
}

fn color_hex(wb: &Workbook, c: CellColor, auto: u32) -> String {
    let v = wb.style_table().theme.rgb(c, auto);
    format!("#{v:06X}")
}

fn font_attrs(wb: &Workbook, s: &Style) -> Vec<(String, String)> {
    let f = &s.font;
    let mut a = vec![at("ss:FontName", &*f.name)];
    let fam = match f.family {
        1 => Some("Roman"),
        2 => Some("Swiss"),
        3 => Some("Modern"),
        4 => Some("Script"),
        5 => Some("Decorative"),
        _ => None,
    };
    if let Some(fam) = fam {
        a.push(at("x:Family", fam));
    }
    let size = f64::from(f.size_twips) / 20.0;

    if f.size_twips != 200 {
        a.push(at("ss:Size", xml_number(size)));
    }
    a.push(at("ss:Color", color_hex(wb, f.color, 0)));
    if f.bold {
        a.push(at("ss:Bold", "1"));
    }
    if f.italic {
        a.push(at("ss:Italic", "1"));
    }
    match f.underline {
        Underline::None => {}
        Underline::Single | Underline::SingleAccounting => a.push(at("ss:Underline", "Single")),
        Underline::Double | Underline::DoubleAccounting => a.push(at("ss:Underline", "Double")),
    }
    if f.strikethrough {
        a.push(at("ss:StrikeThrough", "1"));
    }
    if f.superscript {
        a.push(at("ss:VerticalAlign", "Superscript"));
    } else if f.subscript {
        a.push(at("ss:VerticalAlign", "Subscript"));
    }
    a
}

fn alignment_attrs(a: &Alignment) -> Vec<(String, String)> {
    let mut v = Vec::new();
    let h = match a.horizontal {
        HAlign::General => None,
        HAlign::Left => Some("Left"),
        HAlign::Center => Some("Center"),
        HAlign::Right => Some("Right"),
        HAlign::Fill => Some("Fill"),
        HAlign::Justify => Some("Justify"),
        HAlign::CenterAcrossSelection => Some("CenterAcrossSelection"),
        HAlign::Distributed => Some("Distributed"),
    };
    if let Some(h) = h {
        v.push(at("ss:Horizontal", h));
    }
    v.push(at(
        "ss:Vertical",
        match a.vertical {
            VAlign::Top => "Top",
            VAlign::Center => "Center",
            VAlign::Bottom => "Bottom",
            VAlign::Justify => "Justify",
            VAlign::Distributed => "Distributed",
        },
    ));
    if a.indent > 0 {
        v.push(at("ss:Indent", a.indent.to_string()));
    }
    match a.rotation {
        0 | 255 => {}
        r @ 1..=90 => v.push(at("ss:Rotate", r.to_string())),
        r => v.push(at("ss:Rotate", (-(i32::from(r) - 90)).to_string())),
    }
    if a.shrink {
        v.push(at("ss:ShrinkToFit", "1"));
    }
    if a.rotation == 255 {
        v.push(at("ss:VerticalText", "1"));
    }
    if a.wrap {
        v.push(at("ss:WrapText", "1"));
    }
    v
}

fn line_style(l: BorderLine) -> (&'static str, &'static str) {
    match l {
        BorderLine::None => ("None", "0"),
        BorderLine::Hair => ("Continuous", "0"),
        BorderLine::Thin => ("Continuous", "1"),
        BorderLine::Medium => ("Continuous", "2"),
        BorderLine::Thick => ("Continuous", "3"),
        BorderLine::Double => ("Double", "3"),
        BorderLine::Dashed => ("Dash", "1"),
        BorderLine::MediumDashed => ("Dash", "2"),
        BorderLine::Dotted => ("Dot", "1"),
        BorderLine::DashDot => ("DashDot", "1"),
        BorderLine::MediumDashDot => ("DashDot", "2"),
        BorderLine::DashDotDot => ("DashDotDot", "1"),
        BorderLine::MediumDashDotDot => ("DashDotDot", "2"),
        BorderLine::SlantDashDot => ("SlantDashDot", "2"),
    }
}

fn pattern_name(p: Pattern) -> &'static str {
    match p {
        Pattern::None => "None",
        Pattern::Solid => "Solid",
        Pattern::MediumGray => "Gray50",
        Pattern::DarkGray => "Gray75",
        Pattern::LightGray => "Gray25",
        Pattern::DarkHorizontal => "HorzStripe",
        Pattern::DarkVertical => "VertStripe",
        Pattern::DarkDown => "ReverseDiagStripe",
        Pattern::DarkUp => "DiagStripe",
        Pattern::DarkGrid => "DiagCross",
        Pattern::DarkTrellis => "ThickDiagCross",
        Pattern::LightHorizontal => "ThinHorzStripe",
        Pattern::LightVertical => "ThinVertStripe",
        Pattern::LightDown => "ThinReverseDiagStripe",
        Pattern::LightUp => "ThinDiagStripe",
        Pattern::LightGrid => "ThinHorzCross",
        Pattern::LightTrellis => "ThinDiagCross",
        Pattern::Gray125 => "Gray125",
        Pattern::Gray0625 => "Gray0625",
    }
}

#[derive(Clone, Copy, Default)]
struct Facets {
    align: bool,
    borders: bool,
    font: bool,
    fill: bool,
    number: bool,
    protection: bool,
}

fn write_style_body(xml: &mut Xml, wb: &Workbook, s: &Style, f: Facets, with_empty: bool) {
    if f.align || with_empty {
        if f.align {
            xml.tag(3, "Alignment", &alignment_attrs(&s.alignment), "/>", "");
        } else {
            xml.tag(3, "Alignment", &[at("ss:Vertical", "Bottom")], "/>", "");
        }
    }
    if f.borders {
        xml.line("   <Borders>");
        let b = &s.borders;
        let edges = [
            ("Bottom", b.bottom),
            ("Left", b.left),
            ("Right", b.right),
            ("Top", b.top),
        ];
        let mut list: Vec<(&str, xlcalc::style::Border)> = edges.to_vec();
        if b.diagonal.line != BorderLine::None {
            if b.diagonal_down {
                list.push(("DiagonalLeft", b.diagonal));
            }
            if b.diagonal_up {
                list.push(("DiagonalRight", b.diagonal));
            }
        }
        for (pos, e) in list {
            if e.line == BorderLine::None {
                continue;
            }
            let (ls, w) = line_style(e.line);
            let mut a = vec![
                at("ss:Position", pos),
                at("ss:LineStyle", ls),
                at("ss:Weight", w),
            ];
            if e.color != CellColor::Auto {
                a.push(at("ss:Color", color_hex(wb, e.color, 0)));
            }
            xml.tag(4, "Border", &a, "/>", "");
        }
        xml.line("   </Borders>");
    } else if with_empty {
        xml.line("   <Borders/>");
    }
    if f.font {
        xml.tag(3, "Font", &font_attrs(wb, s), "/>", "");
    }
    if f.fill && s.fill.pattern != Pattern::None {
        let mut a = Vec::new();
        if s.fill.pattern == Pattern::Solid {
            a.push(at("ss:Color", color_hex(wb, s.fill.color, 0xFFFFFF)));
            a.push(at("ss:Pattern", "Solid"));
        } else {
            a.push(at("ss:Color", color_hex(wb, s.fill.color, 0xFFFFFF)));
            a.push(at("ss:Pattern", pattern_name(s.fill.pattern)));
            a.push(at(
                "ss:PatternColor",
                color_hex(wb, s.fill.pattern_color, 0),
            ));
        }
        xml.tag(3, "Interior", &a, "/>", "");
    } else if with_empty || f.fill {
        xml.line("   <Interior/>");
    }
    if f.number {
        let code = xml_format(&s.number_format);
        if code.is_empty() {
            xml.line("   <NumberFormat/>");
        } else {
            xml.tag(3, "NumberFormat", &[at("ss:Format", code)], "/>", "");
        }
    } else if with_empty {
        xml.line("   <NumberFormat/>");
    }
    if f.protection {
        let mut a = Vec::new();
        if !s.locked {
            a.push(at("ss:Protected", "0"));
        }
        if s.formula_hidden {
            a.push(at("x:HideFormula", "1"));
        }
        xml.tag(3, "Protection", &a, "/>", "");
    } else if with_empty {
        xml.line("   <Protection/>");
    }
}

fn same_ignoring_custom(a: &Style, b: &Style) -> bool {
    let mut x = a.clone();
    x.custom_code = b.custom_code;
    x == *b
}

struct Xf {
    id: u32,

    def: StyleDef,
}

enum StyleDef {
    Named(String),

    Child { parent: String, style: Style },

    Plain(Style),
}

struct Styles<'a> {
    wb: &'a Workbook,
    base: &'a Style,
    table: Vec<&'a Style>,
    named: &'a [NamedStyle],
}

impl<'a> Styles<'a> {
    fn new(wb: &'a Workbook) -> Styles<'a> {
        Styles {
            wb,
            base: wb.base_style(),
            table: wb.xf_table(),
            named: &wb.style_table().named,
        }
    }

    fn position(&self, s: &Style) -> Option<u32> {
        self.table
            .iter()
            .position(|o| same_ignoring_custom(s, o))
            .map(|p| FIRST_XF + p as u32)
    }

    fn builtin(&self, name: &str) -> Option<(&'a NamedStyle, u32)> {
        let id = NAMED_IDS
            .iter()
            .find(|(n, _)| n.eq_ignore_ascii_case(name))?
            .1;
        let ns = self
            .named
            .iter()
            .find(|n| n.name.eq_ignore_ascii_case(name))?;
        Some((ns, id))
    }

    fn xf(&self, addr: CellAddr) -> Option<Xf> {
        let own = self.wb.own_style(addr)?;
        let named = &*own.named;
        if !named.eq_ignore_ascii_case("Normal") {
            let (ns, nid) = self.builtin(named)?;
            let full_font = ns.include_font;
            let mut parent = ns.style.clone();
            parent.named = own.named.clone();
            parent.custom_code = own.custom_code;
            if full_font && same_ignoring_custom(own, &parent) {
                self.position(own)?;
                return Some(Xf {
                    id: nid,
                    def: StyleDef::Named(ns.name.to_string()),
                });
            }
            let id = self.position(own)?;
            return Some(Xf {
                id,
                def: StyleDef::Child {
                    parent: ns.name.to_string(),
                    style: own.clone(),
                },
            });
        }
        if same_ignoring_custom(own, self.base) {
            return None;
        }
        let id = self.position(own)?;
        Some(Xf {
            id,
            def: StyleDef::Plain(own.clone()),
        })
    }
}

fn diff_facets(a: &Style, b: &Style) -> Facets {
    Facets {
        align: a.alignment != b.alignment,
        borders: a.borders != b.borders,
        font: a.font != b.font,
        fill: a.fill != b.fill,
        number: a.number_format != b.number_format,
        protection: a.locked != b.locked || a.formula_hidden != b.formula_hidden,
    }
}

fn r1c1_refs(text: &str, host: (u32, u32)) -> Vec<(Option<String>, i64, i64)> {
    let ch: Vec<char> = text.chars().collect();
    let mut out = Vec::new();
    let mut i = 0;
    let mut in_str = false;
    let is_word = |c: char| c.is_alphanumeric() || c == '_' || c == '.';
    while i < ch.len() {
        let c = ch[i];
        if c == '"' {
            in_str = !in_str;
            i += 1;
            continue;
        }
        if in_str {
            i += 1;
            continue;
        }
        if (c == 'R' || c == 'r') && (i == 0 || !is_word(ch[i - 1]) || ch[i - 1] == '!') {
            let mut j = i + 1;
            let part = |j: &mut usize| -> Option<(bool, i64)> {
                if *j < ch.len() && ch[*j] == '[' {
                    let mut k = *j + 1;
                    let mut s = String::new();
                    while k < ch.len() && ch[k] != ']' {
                        s.push(ch[k]);
                        k += 1;
                    }
                    *j = k + 1;
                    s.parse::<i64>().ok().map(|v| (false, v))
                } else {
                    let mut s = String::new();
                    while *j < ch.len() && ch[*j].is_ascii_digit() {
                        s.push(ch[*j]);
                        *j += 1;
                    }
                    if s.is_empty() {
                        Some((false, 0))
                    } else {
                        s.parse::<i64>().ok().map(|v| (true, v))
                    }
                }
            };
            let r = part(&mut j);
            if j < ch.len() && (ch[j] == 'C' || ch[j] == 'c') {
                j += 1;
                let cc = part(&mut j);
                let ends_word = j >= ch.len() || !(is_word(ch[j]) || ch[j] == '(');
                if let (Some(r), Some(cc), true) = (r, cc, ends_word) {
                    let mut sheet = None;
                    if i > 0 && ch[i - 1] == '!' {
                        let mut k = i - 1;
                        let mut name = String::new();
                        if k > 0 && ch[k - 1] == '\'' {
                            k -= 1;
                            while k > 0 && ch[k - 1] != '\'' {
                                name.insert(0, ch[k - 1]);
                                k -= 1;
                            }
                        } else {
                            while k > 0 && is_word(ch[k - 1]) {
                                name.insert(0, ch[k - 1]);
                                k -= 1;
                            }
                        }
                        sheet = Some(name);
                    }
                    let row = if r.0 {
                        r.1 - 1
                    } else {
                        i64::from(host.0) + r.1
                    };
                    let col = if cc.0 {
                        cc.1 - 1
                    } else {
                        i64::from(host.1) + cc.1
                    };
                    out.push((sheet, row, col));
                    i = j;
                    continue;
                }
            }
        }
        i += 1;
    }
    out
}

fn all_inside(text: &str, host: (u32, u32), sheet_name: &str, rect: Rect) -> bool {
    r1c1_refs(text, host).into_iter().all(|(sh, r, c)| {
        sh.is_none_or(|s| s.eq_ignore_ascii_case(sheet_name))
            && r >= i64::from(rect.r0)
            && r <= i64::from(rect.r1)
            && c >= i64::from(rect.c0)
            && c <= i64::from(rect.c1)
    })
}

fn rel(n: i64) -> String {
    if n == 0 {
        String::new()
    } else {
        format!("[{n}]")
    }
}

fn array_range(anchor: (u32, u32), block: Rect) -> String {
    let one = |r: u32, c: u32| {
        format!(
            "R{}C{}",
            rel(i64::from(r) - i64::from(anchor.0)),
            rel(i64::from(c) - i64::from(anchor.1))
        )
    };
    if block.r0 == block.r1 && block.c0 == block.c1 {
        one(block.r0, block.c0)
    } else {
        format!("{}:{}", one(block.r0, block.c0), one(block.r1, block.c1))
    }
}

fn merge_extent(wb: &Workbook, a: CellAddr, rect: Rect) -> Option<(Rect, Rect)> {
    let m = wb.merge_at(a)?;
    let clipped = Rect::new(
        m.r0.max(rect.r0),
        m.c0.max(rect.c0),
        m.r1.min(rect.r1),
        m.c1.min(rect.c1),
    );
    Some((m, clipped))
}

pub(crate) fn spreadsheet(
    wb: &Workbook,
    spill_of: &dyn Fn(CellAddr) -> Option<(CellAddr, Rect)>,
    sheet: SheetId,
    rect: Rect,
) -> Option<String> {
    let sheet_name = wb.sheet(sheet)?.name.clone();
    let styles = Styles::new(wb);
    let blank_sheet = wb.used_range(sheet).is_none();

    struct Cell {
        col: u32,
        attrs: Vec<(String, String)>,
        data: Option<(String, String)>,
        names: Vec<String>,
        width: u32,
    }
    let mut rows: BTreeMap<u32, Vec<Cell>> = BTreeMap::new();
    let mut used_xf: BTreeMap<u32, StyleDef> = BTreeMap::new();
    let used_names = visible_names(wb, &sheet_name, rect);

    for r in rect.r0..=rect.r1 {
        for c in rect.c0..=rect.c1 {
            let a = CellAddr::new(sheet, r, c);
            let merge = merge_extent(wb, a, rect);
            let (cr, cc) = match &merge {
                Some((_, cl)) => (cl.r0, cl.c0),
                None => (r, c),
            };
            if (cr, cc) != (r, c) {
                continue;
            }
            let src = match &merge {
                Some((m, _)) => CellAddr::new(sheet, m.r0, m.c0),
                None => a,
            };
            let v = wb.value(src);
            let mut attrs: Vec<(String, String)> = Vec::new();
            let xf = styles.xf(src);
            if let Some(xf) = &xf {
                attrs.push(at("ss:StyleID", format!("s{}", xf.id)));
            }
            let mut across = 0;
            if let Some((_, cl)) = &merge {
                across = cl.c1 - cl.c0;
                let down = cl.r1 - cl.r0;
                if across > 0 {
                    attrs.push(at("ss:MergeAcross", across.to_string()));
                }
                if down > 0 {
                    attrs.push(at("ss:MergeDown", down.to_string()));
                }
            }
            let host = (src.row, src.col);
            let anchor_cell = spill_of(src);
            let legacy = wb.current_array(src);
            let is_formula = wb.formula_text(src).is_some();
            if is_formula {
                let is_child = match (&anchor_cell, &legacy) {
                    (Some((an, _)), _) => *an != src,
                    (None, Some(l)) => (l.r0, l.c0) != (src.row, src.col),
                    _ => false,
                };
                if !is_child {
                    if let Some(f) = wb.formula_text_r1c1(src) {
                        if all_inside(&f, host, &sheet_name, rect) {
                            if let Some((_, block)) = &anchor_cell {
                                attrs.push(at("ss:ArrayRange", array_range(host, *block)));
                            } else if let Some(l) = &legacy {
                                attrs.push(at("ss:ArrayRange", array_range(host, *l)));
                            }
                            attrs.push(at("ss:Formula", f));
                        }
                    }
                }
            }
            let fmt = wb.number_format(src);
            let data = match &v {
                Value::Blank => None,
                Value::Number(x) => {
                    if is_date_code(fmt) {
                        match iso(*x, true) {
                            Some(t) => Some(("DateTime".to_string(), t)),
                            None => Some(("Number".to_string(), xml_number(*x))),
                        }
                    } else {
                        Some(("Number".to_string(), xml_number(*x)))
                    }
                }
                Value::Text(t) => Some(("String".to_string(), t.to_string())),
                Value::Bool(b) => Some(("Boolean".to_string(), if *b { "1" } else { "0" }.into())),
                Value::Error(e) => Some(("Error".to_string(), e.as_str().to_string())),
                Value::Array(_) | Value::Lambda(_) => return None,
            };
            let names: Vec<String> = used_names
                .iter()
                .filter(|n| n.covers(a.row, a.col))
                .map(|n| n.name.clone())
                .collect();

            if data.is_none() && xf.is_none() && names.is_empty() && merge.is_none() {
                continue;
            }
            if let Some(xf) = xf {
                used_xf.entry(xf.id).or_insert(xf.def);
            }
            rows.entry(r).or_default().push(Cell {
                col: c,
                attrs,
                data,
                names,
                width: across + 1,
            });
        }
    }

    for r in rect.r0..=rect.r1 {
        if wb.row_hidden(sheet, r) || wb.row_custom_h8(sheet, r).is_some() {
            rows.entry(r).or_default();
        }
    }

    let mut xml = Xml::new();
    xml.line("<?xml version=\"1.0\"?>");
    xml.line("<?mso-application progid=\"Excel.Sheet\"?>");
    xml.line("<Workbook xmlns=\"urn:schemas-microsoft-com:office:spreadsheet\"");
    xml.line(" xmlns:o=\"urn:schemas-microsoft-com:office:office\"");
    xml.line(" xmlns:x=\"urn:schemas-microsoft-com:office:excel\"");
    xml.line(" xmlns:ss=\"urn:schemas-microsoft-com:office:spreadsheet\"");
    xml.line(" xmlns:html=\"http://www.w3.org/TR/REC-html40\">");

    xml.line(" <Styles>");
    xml.line("  <Style ss:ID=\"Default\" ss:Name=\"Normal\">");
    let base_facets = Facets {
        align: true,
        borders: false,
        font: true,
        fill: false,
        number: false,
        protection: false,
    };
    write_style_body(&mut xml, wb, styles.base, base_facets, true);
    xml.line("  </Style>");

    let mut defs: BTreeMap<u32, StyleDefn> = BTreeMap::new();
    for (id, def) in &used_xf {
        match def {
            StyleDef::Named(name) => {
                if let Some((ns, nid)) = styles.builtin(name) {
                    defs.entry(nid).or_insert_with(|| named_def(ns, nid));
                }
            }
            StyleDef::Child { parent, style } => {
                let (ns, nid) = styles.builtin(parent)?;
                defs.entry(nid).or_insert_with(|| named_def(ns, nid));
                let mut p = ns.style.clone();
                p.named = style.named.clone();
                let mut f = diff_facets(style, &p);
                f.font = f.font || !ns.include_font;
                f.number = style.number_format != ns.style.number_format;
                defs.insert(
                    *id,
                    (
                        vec![
                            at("ss:ID", format!("s{id}")),
                            at("ss:Parent", format!("s{nid}")),
                        ],
                        style.clone(),
                        f,
                    ),
                );
            }
            StyleDef::Plain(style) => {
                let f = diff_facets(style, styles.base);
                defs.insert(*id, (vec![at("ss:ID", format!("s{id}"))], style.clone(), f));
            }
        }
    }

    let mut ordered: Vec<(&u32, &StyleDefn)> = defs.iter().collect();
    ordered.sort_by_key(|(id, (attrs, ..))| {
        let name = attrs.iter().find(|(k, _)| k == "ss:Name");
        (
            name.is_none(),
            name.map(|(_, v)| v.to_lowercase()).unwrap_or_default(),
            **id,
        )
    });
    for (_, (attrs, style, f)) in ordered {
        xml.tag(2, "Style", attrs, ">", "");
        write_style_body(&mut xml, wb, style, *f, false);
        xml.line("  </Style>");
    }
    xml.line(" </Styles>");

    let mut name_lines: Vec<(String, String, bool)> = used_names
        .iter()
        .map(|n| (n.name.clone(), n.refers_to.clone(), n.hidden))
        .collect();
    name_lines.sort_by_key(|n| n.0.to_lowercase());
    if !name_lines.is_empty() {
        xml.line(" <Names>");
        for (n, r, h) in &name_lines {
            let mut a = vec![at("ss:Name", n.as_str()), at("ss:RefersTo", r.as_str())];
            if *h {
                a.push(at("ss:Hidden", "1"));
            }
            xml.tag(2, "NamedRange", &a, "/>", "");
        }
        xml.line(" </Names>");
    }

    xml.tag(
        1,
        "Worksheet",
        &[at("ss:Name", sheet_name.as_str())],
        ">",
        "",
    );

    let row_h = "14.5".to_string();
    let table_attrs = vec![
        at("ss:ExpandedColumnCount", rect.cols().to_string()),
        at("ss:ExpandedRowCount", rect.rows().to_string()),
        at("ss:DefaultRowHeight", row_h.as_str()),
    ];
    if blank_sheet && rows.is_empty() {
        xml.tag(2, "Table", &table_attrs, "/>", "");
    } else {
        xml.tag(2, "Table", &table_attrs, ">", "");
        write_columns(&mut xml, wb, sheet, rect);
        let mut prev_row = 0u32;
        for (r, cells) in &rows {
            let ri = r - rect.r0 + 1;
            let mut ra = Vec::new();
            if ri != prev_row + 1 {
                ra.push(at("ss:Index", ri.to_string()));
            }
            let default_h = (row_h.parse::<f64>().unwrap_or(14.5) * 8.0).round() as u32;
            match wb.row_custom_h8(sheet, *r) {
                Some(h8) => {
                    ra.push(at("ss:AutoFitHeight", "0"));
                    if h8 != default_h {
                        ra.push(at("ss:Height", xml_number(f64::from(h8) / 8.0)));
                    }
                }
                None => {
                    let h8 = wb.row_auto_h8(sheet, *r);
                    if h8 != default_h {
                        ra.push(at("ss:Height", xml_number(f64::from(h8) / 8.0)));
                    }
                }
            }
            if wb.row_hidden(sheet, *r) {
                ra.push(at("ss:Hidden", "1"));
            }
            prev_row = ri;
            if cells.is_empty() {
                xml.tag(3, "Row", &ra, "/>", "");
                continue;
            }
            xml.tag(3, "Row", &ra, ">", "");
            let mut next_col = 1u32;
            for cell in cells {
                let ci = cell.col - rect.c0 + 1;
                let mut a: Vec<(String, String)> = Vec::new();
                if ci != next_col {
                    a.push(at("ss:Index", ci.to_string()));
                }
                a.extend(cell.attrs.iter().cloned());
                next_col = ci + cell.width;
                if cell.data.is_none() && cell.names.is_empty() {
                    xml.tag(4, "Cell", &a, "/>", "");
                } else {
                    let mut tail = String::new();
                    if let Some((ty, text)) = &cell.data {
                        let _ = write!(tail, "<Data ss:Type=\"{ty}\">{}</Data>", esc(text));
                    }
                    for n in &cell.names {
                        let _ = write!(tail, "<NamedCell ss:Name=\"{}\"/>", esc(n));
                    }
                    tail.push_str("</Cell>");
                    xml.tag(4, "Cell", &a, ">", &tail);
                }
            }
            xml.line("   </Row>");
        }
        xml.line("  </Table>");
    }
    xml.line(" </Worksheet>");
    xml.line("</Workbook>");
    Some(xml.out)
}

fn named_def(ns: &NamedStyle, id: u32) -> (Vec<(String, String)>, Style, Facets) {
    (
        vec![at("ss:ID", format!("s{id}")), at("ss:Name", &*ns.name)],
        ns.style.clone(),
        Facets {
            align: ns.include_alignment,
            borders: ns.include_border,
            font: ns.include_font,
            fill: ns.include_patterns,
            number: ns.include_number,
            protection: ns.include_protection,
        },
    )
}

fn write_columns(xml: &mut Xml, wb: &Workbook, sheet: SheetId, rect: Rect) {
    let mut cols: Vec<(u32, u32, bool, bool)> = wb
        .column_state(sheet)
        .into_iter()
        .filter(|(c, px, _, hidden)| {
            *c >= rect.c0 && *c <= rect.c1 && (*hidden || *px != wb.standard_px())
        })
        .collect();
    cols.sort_unstable();
    let mut i = 0;
    let mut next = 1u32;
    while i < cols.len() {
        let (c, px, hand, hidden) = cols[i];
        let mut j = i;
        while j + 1 < cols.len()
            && cols[j + 1].0 == cols[j].0 + 1
            && cols[j + 1].1 == px
            && cols[j + 1].2 == hand
            && cols[j + 1].3 == hidden
        {
            j += 1;
        }
        let ci = c - rect.c0 + 1;
        let mut a = Vec::new();
        if ci != next {
            a.push(at("ss:Index", ci.to_string()));
        }
        if j > i {
            a.push(at("ss:Span", (j - i).to_string()));
        }
        if hidden {
            a.push(at("ss:Hidden", "1"));
        }
        if hand {
            a.push(at("ss:AutoFitWidth", "0"));
        }
        if !hidden {
            a.push(at("ss:Width", xml_number(f64::from(px) / 2.0)));
        }
        xml.tag(3, "Column", &a, "/>", "");
        next = ci + (j - i) as u32 + 1;
        i = j + 1;
    }
}

struct UsedName {
    name: String,
    refers_to: String,
    hidden: bool,

    area: Option<Rect>,
    pure_ref: bool,
}

impl UsedName {
    fn covers(&self, r: u32, c: u32) -> bool {
        self.area
            .is_some_and(|a| r >= a.r0 && r <= a.r1 && c >= a.c0 && c <= a.c1)
    }
}

fn visible_names(wb: &Workbook, sheet_name: &str, rect: Rect) -> Vec<UsedName> {
    let (ar, ac) = wb.active_cell();
    let mut out: Vec<UsedName> = Vec::new();

    for h in wb.names().hidden().filter(|h| h.starts_with("_xlpm.")) {
        out.push(UsedName {
            name: h.to_string(),
            refers_to: "=#NAME?".into(),
            hidden: true,
            area: None,
            pure_ref: false,
        });
    }
    for d in wb.names().iter() {
        if d.scope.is_some() {
            continue;
        }
        let text = wb.name_refers_to_r1c1(d);
        let refs = r1c1_refs(&text, (ar, ac));
        if !all_inside(&text, (ar, ac), sheet_name, rect) {
            continue;
        }
        let (mut r0, mut c0, mut r1, mut c1) = (i64::MAX, i64::MAX, i64::MIN, i64::MIN);
        for (_, r, c) in &refs {
            r0 = r0.min(*r);
            c0 = c0.min(*c);
            r1 = r1.max(*r);
            c1 = c1.max(*c);
        }
        let area = if refs.is_empty() {
            None
        } else {
            Some(Rect::new(r0 as u32, c0 as u32, r1 as u32, c1 as u32))
        };
        let pure_ref = !text.contains(['+', '-', '*', '/', '(', '&', '^']) && !refs.is_empty();
        out.push(UsedName {
            name: d.name.clone(),
            refers_to: format!("={text}"),
            hidden: false,
            area,
            pure_ref,
        });
    }

    out.sort_by_key(|n| (n.pure_ref, n.name.to_lowercase()));
    out
}

#[derive(Clone, PartialEq, Debug)]
enum Sig {
    Blank,
    Cell(u8, Box<Option<Style>>),
}

#[derive(Clone, Copy, PartialEq, Debug)]
enum Ty {
    Bool,
    Int,
    Float,
    Date,
    Str,
}

fn sig_of(wb: &Workbook, a: CellAddr) -> Sig {
    let kind = match wb.value(a) {
        Value::Blank => return Sig::Blank,

        Value::Text(t) if t.chars().count() > 255 => 4,
        Value::Text(_) => 1,
        Value::Bool(_) => 2,
        Value::Number(_) => 3,
        _ => 0,
    };
    Sig::Cell(kind, Box::new(wb.own_style(a).cloned()))
}

fn header_rows(sigs: &[Sig]) -> usize {
    let Some(p) = sigs.iter().position(|s| *s != Sig::Blank) else {
        return 0;
    };
    let reference = &sigs[p];
    let Some(k) = sigs
        .iter()
        .enumerate()
        .skip(p + 1)
        .find(|(_, s)| **s != Sig::Blank && *s != reference)
        .map(|(k, _)| k)
    else {
        return 0;
    };
    if k > 2 || sigs[k - 1] == Sig::Blank {
        return 0;
    }
    k
}

pub(crate) fn rowset(wb: &Workbook, sheet: SheetId, rect: Rect) -> Option<String> {
    let ncols = rect.cols() as usize;
    let nrows = rect.rows() as usize;
    let addr = |r: usize, c: usize| CellAddr::new(sheet, rect.r0 + r as u32, rect.c0 + c as u32);
    let mut h = 0usize;
    for c in 0..ncols {
        let sigs: Vec<Sig> = (0..nrows).map(|r| sig_of(wb, addr(r, c))).collect();
        h = h.max(header_rows(&sigs));
    }

    let join_above = h == 1
        && rect.r0 > 0
        && (0..ncols).all(|c| {
            let col = rect.c0 + c as u32;
            let above = CellAddr::new(sheet, rect.r0 - 1, col);

            let clear = rect.r0 < 2 || wb.value(CellAddr::new(sheet, rect.r0 - 2, col)).is_blank();
            wb.value(above).is_blank()
                || match (sig_of(wb, above), sig_of(wb, addr(0, c))) {
                    (Sig::Cell(1, _), Sig::Cell(1, _)) => true,
                    (Sig::Cell(3, _), Sig::Cell(3, _)) => clear,
                    _ => false,
                }
        });
    let mut names: Vec<String> = Vec::new();
    let mut seen: BTreeMap<String, u32> = BTreeMap::new();
    for c in 0..ncols {
        let mut parts: Vec<String> = Vec::new();
        let cell_text = |a: CellAddr| match wb.value(a) {
            Value::Error(_) => String::new(),
            _ => wb.shown_text(a),
        };
        let mut headed = h > 0;
        if h == 1 && rect.r0 > 0 && join_above {
            let above = CellAddr::new(sheet, rect.r0 - 1, rect.c0 + c as u32);
            if wb.value(above).is_blank() {
                parts.push(String::new());
            } else {
                parts.push(cell_text(above));
            }
        }
        if h > 0 {
            for r in 0..h {
                parts.push(cell_text(addr(r, c)));
            }
        } else if nrows > 1 {
            let above = (rect.r0.min(2)) as usize;
            let mut sigs: Vec<Sig> = Vec::new();
            for k in (1..=above as u32).rev() {
                sigs.push(sig_of(
                    wb,
                    CellAddr::new(sheet, rect.r0 - k, rect.c0 + c as u32),
                ));
            }
            sigs.extend((0..nrows).map(|r| sig_of(wb, addr(r, c))));
            let k = header_rows(&sigs);

            if k > 0 && k == above {
                headed = true;
                for i in 0..k {
                    let a = CellAddr::new(sheet, rect.r0 - (above - i) as u32, rect.c0 + c as u32);
                    parts.push(cell_text(a));
                }
            }
        }
        let mut name = parts.join(" ");
        if !headed || name.is_empty() || name.chars().count() > 255 {
            name = format!("Field{}", c + 1);
        }
        let n = seen.entry(name.to_lowercase()).or_insert(0);
        *n += 1;
        if *n > 1 {
            name = format!("{name}{n}");
        }
        names.push(name);
    }

    let mut types: Vec<Ty> = Vec::new();
    for c in 0..ncols {
        let mut ty: Option<Ty> = None;
        for r in h..nrows {
            let a = addr(r, c);
            let t = match wb.value(a) {
                Value::Blank => continue,
                Value::Bool(_) => Ty::Bool,
                Value::Number(x) => {
                    if is_date_code(wb.number_format(a)) {
                        Ty::Date
                    } else if x.fract() == 0.0 && x.abs() < 2_147_483_648.0 {
                        Ty::Int
                    } else {
                        Ty::Float
                    }
                }
                _ => Ty::Str,
            };
            ty = Some(match (ty, t) {
                (None, t) => t,
                (Some(a), b) if a == b => a,
                (Some(Ty::Int), Ty::Float) | (Some(Ty::Float), Ty::Int) => Ty::Float,
                _ => Ty::Str,
            });
        }
        types.push(ty.unwrap_or(Ty::Bool));
    }

    let long: Vec<bool> = (0..ncols)
        .map(|c| {
            types[c] == Ty::Str
                && (h..nrows).any(
                    |r| matches!(wb.value(addr(r, c)), Value::Text(t) if t.chars().count() > 255),
                )
        })
        .collect();
    let mut out = String::new();
    let mut l = |s: &str| {
        out.push_str(s);
        out.push_str(EOL);
    };
    l("<xml xmlns:x=\"urn:schemas-microsoft-com:office:excel\"");
    l(" xmlns:dt=\"uuid:C2F41010-65B3-11d1-A29F-00AA00C14882\"");
    l(" xmlns:s=\"uuid:BDC6E3F0-6DA3-11d1-A2A3-00AA00C14882\"");
    l(" xmlns:rs=\"urn:schemas-microsoft-com:rowset\" xmlns:z=\"#RowsetSchema\">");
    l(" <x:PivotCache>");
    l("  <x:CacheIndex>1</x:CacheIndex>");
    l("  <s:Schema id=\"RowsetSchema\">");
    l("   <s:ElementType name=\"row\" content=\"eltOnly\">");
    for c in 0..ncols {
        l(&format!("    <s:attribute type=\"Col{}\"/>", c + 1));
    }
    l("    <s:extends type=\"rs:rowbase\"/>");
    l("   </s:ElementType>");
    for (c, n) in names.iter().enumerate() {
        l(&format!(
            "   <s:AttributeType name=\"Col{}\" rs:name=\"{}\">",
            c + 1,
            esc(n)
        ));
        match types[c] {
            Ty::Bool => l("    <s:datatype dt:type=\"boolean\"/>"),
            Ty::Int => l("    <s:datatype dt:type=\"int\"/>"),
            Ty::Float => l("    <s:datatype dt:type=\"float\"/>"),
            Ty::Date => l("    <s:datatype dt:type=\"dateTime\"/>"),
            Ty::Str if long[c] => l("    <s:datatype rs:long=\"true\"/>"),
            Ty::Str => l("    <s:datatype dt:maxLength=\"255\"/>"),
        }
        l("   </s:AttributeType>");
    }
    l("  </s:Schema>");
    l("  <rs:data>");
    for r in h..nrows {
        let mut attrs: Vec<String> = Vec::new();
        for (c, ty) in types.iter().enumerate().take(ncols) {
            let a = addr(r, c);
            let v = wb.value(a);
            let text = match (&v, *ty) {
                (Value::Blank, _) => continue,
                (Value::Bool(b), _) => if *b { "TRUE" } else { "FALSE" }.to_string(),
                (Value::Number(x), Ty::Int | Ty::Float) => xml_number(*x),
                (Value::Number(x), Ty::Date) => iso(*x, false)?,
                (Value::Error(e), _) => e.as_str().to_string(),
                _ => wb.shown_text(a),
            };
            attrs.push(format!("Col{}=\"{}\"", c + 1, esc(&text)));
        }

        let mut line = String::from("<z:row");
        let mut lines: Vec<String> = Vec::new();
        for (i, a) in attrs.iter().enumerate() {
            let tail = if i + 1 == attrs.len() { 2 } else { 0 };
            if line.chars().count() + 1 + a.chars().count() + tail > WRAP {
                lines.push(std::mem::take(&mut line));
                line = a.clone();
            } else {
                line.push(' ');
                line.push_str(a);
            }
        }
        line.push_str("/>");
        lines.push(line);
        for (i, ln) in lines.iter().enumerate() {
            l(&format!("   {}{ln}", if i > 0 { " " } else { "" }));
        }
    }
    l("  </rs:data>");
    l(" </x:PivotCache>");
    out.push_str("</xml>\u{0}");
    Some(out)
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn numbers() {
        let cases = [
            (0.1, "0.1"),
            (1.0 / 3.0, "0.33333333333333331"),
            (1e15, "1000000000000000"),
            (1e16, "1E+16"),
            (1e-4, "1E-4"),
            (0.01, "0.01"),
            (0.0123, "1.23E-2"),
            (3.5, "3.5"),
            (-2.0, "-2"),
            (1e100, "1E+100"),
            (9007199254740992.0, "9007199254740992"),
        ];
        for (x, want) in cases {
            assert_eq!(xml_number(x), want, "{x}");
        }
    }

    #[test]
    fn formats() {
        assert_eq!(xml_format("0.00"), "Fixed");
        assert_eq!(xml_format("d-mmm"), "d\\-mmm");
        assert_eq!(xml_format("# ?/?"), "#\\ ?/?");
        assert_eq!(xml_format("$#,##0.00"), "\"$\"#,##0.00");
        assert_eq!(xml_format("#,##0_);(#,##0)"), "#,##0_);\\(#,##0\\)");
        assert_eq!(xml_format("0 \"kg\""), "0\\ \"kg\"");
    }

    #[test]
    fn headers() {
        use Sig::*;
        let n = Cell(3, Box::new(None));
        let t = Cell(1, Box::new(None));
        assert_eq!(header_rows(&[n.clone(), n.clone()]), 0);
        assert_eq!(header_rows(&[n.clone(), t.clone()]), 1);
        assert_eq!(header_rows(&[n.clone(), n.clone(), t.clone()]), 2);
        assert_eq!(
            header_rows(&[n.clone(), n.clone(), n.clone(), t.clone()]),
            0
        );
        assert_eq!(header_rows(&[n, Blank, t]), 0);
    }
}
