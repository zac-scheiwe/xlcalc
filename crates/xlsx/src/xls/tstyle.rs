use super::cur::Cur;
use super::rcf::{BORDERS, PATTERNS};
use super::rstyles::Styles;
use super::wstyles::{Palette, Rec};
use crate::xml::Node;
use std::fmt::Write as _;

const BUILTIN_STYLES: u32 = 144;

pub(super) const ELEMENT_TYPES: [&str; 28] = [
    "wholeTable",
    "headerRow",
    "totalRow",
    "firstColumn",
    "lastColumn",
    "firstRowStripe",
    "secondRowStripe",
    "firstColumnStripe",
    "secondColumnStripe",
    "firstHeaderCell",
    "lastHeaderCell",
    "firstTotalCell",
    "lastTotalCell",
    "firstSubtotalColumn",
    "secondSubtotalColumn",
    "thirdSubtotalColumn",
    "firstSubtotalRow",
    "secondSubtotalRow",
    "thirdSubtotalRow",
    "blankRow",
    "firstColumnSubheading",
    "secondColumnSubheading",
    "thirdColumnSubheading",
    "firstRowSubheading",
    "secondRowSubheading",
    "thirdRowSubheading",
    "pageFieldLabels",
    "pageFieldValues",
];

const SIDES: [(&str, u16); 7] = [
    ("top", 6),
    ("bottom", 7),
    ("left", 8),
    ("right", 9),
    ("diagonal", 10),
    ("vertical", 11),
    ("horizontal", 12),
];

fn frt(id: u16) -> Vec<u8> {
    let mut d = id.to_le_bytes().to_vec();
    d.extend([0u8; 10]);
    d
}

fn flag(n: &Node) -> bool {
    n.attr("val").is_none_or(|v| !matches!(v, "0" | "false"))
}

fn color(c: &Node, pal: &Palette, auto_icv: u8) -> [u8; 8] {
    let mut d = [0u8; 8];
    let rgb = pal.rgb(c).unwrap_or(0);
    let tail = [(rgb >> 16) as u8, (rgb >> 8) as u8, rgb as u8, 0xFF];
    if let Some(t) = c.attr("theme").and_then(|v| v.parse::<u8>().ok()) {
        let tint: f64 = c.attr("tint").and_then(|v| v.parse().ok()).unwrap_or(0.0);
        d[0] = 0x07;
        d[1] = t;
        d[2..4].copy_from_slice(&((tint * 32767.0).round() as i16).to_le_bytes());
        d[4..].copy_from_slice(&tail);
    } else if let Some(i) = c.attr("indexed").and_then(|v| v.parse::<u8>().ok()) {
        d[0] = 0x03;
        d[1] = i;
        d[4..].copy_from_slice(&tail);
    } else if c.attr("rgb").is_some() {
        d[0] = 0x05;
        d[1] = 0xFF;
        d[4..].copy_from_slice(&tail);
    } else {
        d[0] = 0x01;
        d[1] = auto_icv;
    }
    d
}

fn dxf_record(dxf: &Node, pal: &Palette) -> Vec<u8> {
    let mut props: Vec<(u16, Vec<u8>)> = Vec::new();
    if let Some(f) = dxf.child("font") {
        if let Some(c) = f.child("color") {
            props.push((5, color(c, pal, 0x40).to_vec()));
        }
        if let Some(b) = f.child("b") {
            let w: u16 = if flag(b) { 700 } else { 400 };
            props.push((0x19, w.to_le_bytes().to_vec()));
        }
        if let Some(u) = f.child("u") {
            let v: u16 = match u.attr("val") {
                None | Some("single") => 1,
                Some("double") => 2,
                Some("singleAccounting") => 3,
                Some("doubleAccounting") => 4,
                _ => 0,
            };
            props.push((0x1A, v.to_le_bytes().to_vec()));
        }
        if let Some(i) = f.child("i") {
            props.push((0x1C, vec![u8::from(flag(i))]));
        }
        if let Some(s) = f.child("strike") {
            props.push((0x1D, vec![u8::from(flag(s))]));
        }
    }
    if let Some(pf) = dxf.child("fill").and_then(|f| f.child("patternFill")) {
        if let Some(p) = pf.attr("patternType") {
            let i = PATTERNS.iter().position(|x| *x == p).unwrap_or(0);
            props.push((0, vec![i as u8]));
        }
        if let Some(c) = pf.child("fgColor") {
            props.push((1, color(c, pal, 0x40).to_vec()));
        }
        if let Some(c) = pf.child("bgColor") {
            props.push((2, color(c, pal, 0x41).to_vec()));
        }
    }
    if let Some(b) = dxf.child("border") {
        for (name, ty) in SIDES {
            let Some(s) = b.child(name) else { continue };
            let Some(st) = s.attr("style").filter(|v| *v != "none") else {
                continue;
            };
            let code = BORDERS.iter().position(|x| *x == st).unwrap_or(1) as u16;
            let mut d = match s.child("color") {
                Some(c) => color(c, pal, 0).to_vec(),
                None => color(&Node::default(), pal, 0).to_vec(),
            };
            d.extend(code.to_le_bytes());
            props.push((ty, d));
        }
    }
    props.sort_by_key(|p| p.0);
    let mut d = frt(0x88D);
    d.extend(3u16.to_le_bytes());
    d.extend(0u16.to_le_bytes());
    d.extend((props.len() as u16).to_le_bytes());
    for (ty, data) in props {
        d.extend(ty.to_le_bytes());
        d.extend(((data.len() + 4) as u16).to_le_bytes());
        d.extend(data);
    }
    d
}

fn name_chars(d: &mut Vec<u8>, s: &str) {
    for u in s.encode_utf16() {
        d.extend(u.to_le_bytes());
    }
}

pub(super) fn convert(styles: &Node, pal: &Palette) -> (Vec<Rec>, Vec<Rec>) {
    let ts = styles.child("tableStyles");
    let dxf_nodes: Vec<&Node> = styles
        .child("dxfs")
        .map(|d| d.children("dxf").collect())
        .unwrap_or_default();
    let list: Vec<&Node> = ts
        .map(|t| t.children("tableStyle").collect())
        .unwrap_or_default();
    let mut dxfs: Vec<Rec> = Vec::new();
    let mut seen: Vec<Vec<u8>> = Vec::new();
    let mut out: Vec<Rec> = Vec::new();
    let mut body: Vec<Rec> = Vec::new();
    for s in list {
        let name = s.attr("name").unwrap_or("");
        let on = |k: &str| s.attr(k).is_none_or(|v| !matches!(v, "0" | "false"));
        let flags = (u16::from(on("pivot")) << 1) | (u16::from(on("table")) << 2);
        let mut elems: Vec<(u32, u32, u32)> = Vec::new();
        for e in s.children("tableStyleElement") {
            let Some(ty) = e
                .attr("type")
                .and_then(|t| ELEMENT_TYPES.iter().position(|x| *x == t))
            else {
                continue;
            };
            let Some(dx) = e
                .attr("dxfId")
                .and_then(|v| v.parse::<usize>().ok())
                .and_then(|i| dxf_nodes.get(i))
            else {
                continue;
            };
            let rec = dxf_record(dx, pal);
            let idx = match seen.iter().position(|x| *x == rec) {
                Some(i) => i,
                None => {
                    seen.push(rec.clone());
                    dxfs.push((0x88D, rec));
                    seen.len() - 1
                }
            };
            let size = e.attr("size").and_then(|v| v.parse().ok()).unwrap_or(1);
            elems.push((ty as u32, size, idx as u32));
        }
        let mut d = frt(0x88F);
        d.extend(flags.to_le_bytes());
        d.extend((elems.len() as u32).to_le_bytes());
        d.extend((name.encode_utf16().count().min(255) as u16).to_le_bytes());
        name_chars(&mut d, &name.chars().take(255).collect::<String>());
        body.push((0x88F, d));
        for (ty, size, idx) in elems {
            let mut d = frt(0x890);
            d.extend(ty.to_le_bytes());
            d.extend(size.to_le_bytes());
            d.extend(idx.to_le_bytes());
            body.push((0x890, d));
        }
    }
    let custom = body.iter().filter(|r| r.0 == 0x88F).count() as u32;
    let def_t = ts
        .and_then(|t| t.attr("defaultTableStyle"))
        .unwrap_or("TableStyleMedium2");
    let def_p = ts
        .and_then(|t| t.attr("defaultPivotStyle"))
        .unwrap_or("PivotStyleLight16");
    let mut d = frt(0x88E);
    d.extend((BUILTIN_STYLES + custom).to_le_bytes());
    d.extend((def_t.encode_utf16().count() as u16).to_le_bytes());
    d.extend((def_p.encode_utf16().count() as u16).to_le_bytes());
    name_chars(&mut d, def_t);
    name_chars(&mut d, def_p);
    out.push((0x88E, d));
    out.extend(body);
    (dxfs, out)
}

fn color_xml(tag: &str, b: &[u8]) -> String {
    if b.len() < 8 {
        return String::new();
    }
    let rgb = format!("FF{:02X}{:02X}{:02X}", b[4], b[5], b[6]);
    match (b[0] >> 1) & 0x7F {
        1 => format!("<{tag} indexed=\"{}\"/>", b[1]),
        2 => format!("<{tag} rgb=\"{rgb}\"/>"),
        3 => {
            let tint = i16::from_le_bytes([b[2], b[3]]);
            if tint == 0 {
                format!("<{tag} theme=\"{}\"/>", b[1])
            } else {
                format!(
                    "<{tag} theme=\"{}\" tint=\"{}\"/>",
                    b[1],
                    f64::from(tint) / 32767.0
                )
            }
        }
        _ => format!("<{tag} auto=\"1\"/>"),
    }
}

fn dxf_xml(d: &[u8]) -> Result<String, String> {
    let mut c = Cur::new(d);
    c.take(12)?;
    c.u16()?;
    c.u16()?;
    let n = c.u16()?;
    let mut props: Vec<(u16, &[u8])> = Vec::new();
    for _ in 0..n {
        let ty = c.u16()?;
        let cb = usize::from(c.u16()?);
        props.push((ty, c.take(cb.saturating_sub(4))?));
    }
    let get = |t: u16| props.iter().find(|p| p.0 == t).map(|p| p.1);
    let mut x = String::from("<dxf>");
    let mut font = String::new();
    if let Some(w) = get(0x19).filter(|w| w.len() >= 2) {
        font.push_str(if u16::from_le_bytes([w[0], w[1]]) >= 700 {
            "<b/>"
        } else {
            "<b val=\"0\"/>"
        });
    }
    if let Some(v) = get(0x1C).and_then(|v| v.first()) {
        font.push_str(if *v != 0 { "<i/>" } else { "<i val=\"0\"/>" });
    }
    if let Some(v) = get(0x1D).and_then(|v| v.first()) {
        font.push_str(if *v != 0 {
            "<strike/>"
        } else {
            "<strike val=\"0\"/>"
        });
    }
    if let Some(w) = get(0x1A).filter(|w| w.len() >= 2) {
        font.push_str(match u16::from_le_bytes([w[0], w[1]]) {
            0 => "<u val=\"none\"/>",
            1 => "<u/>",
            2 => "<u val=\"double\"/>",
            3 => "<u val=\"singleAccounting\"/>",
            _ => "<u val=\"doubleAccounting\"/>",
        });
    }
    if let Some(b) = get(5) {
        font.push_str(&color_xml("color", b));
    }
    if !font.is_empty() {
        let _ = write!(x, "<font>{font}</font>");
    }
    let (pat, fg, bg) = (get(0), get(1), get(2));
    if pat.is_some() || fg.is_some() || bg.is_some() {
        x.push_str("<fill><patternFill");
        if let Some(p) = pat.and_then(|p| p.first()) {
            let _ = write!(
                x,
                " patternType=\"{}\"",
                PATTERNS.get(usize::from(*p)).copied().unwrap_or("none")
            );
        }
        x.push('>');
        if let Some(b) = fg {
            x.push_str(&color_xml("fgColor", b));
        }
        if let Some(b) = bg {
            x.push_str(&color_xml("bgColor", b));
        }
        x.push_str("</patternFill></fill>");
    }
    let mut border = String::new();
    for name in [
        "left",
        "right",
        "top",
        "bottom",
        "diagonal",
        "vertical",
        "horizontal",
    ] {
        let ty = SIDES.iter().find(|s| s.0 == name).map_or(0, |s| s.1);
        if let Some(b) = get(ty).filter(|b| b.len() >= 10) {
            let st = BORDERS
                .get(usize::from(u16::from_le_bytes([b[8], b[9]])))
                .copied()
                .unwrap_or("thin");
            let _ = write!(
                border,
                "<{name} style=\"{st}\">{}</{name}>",
                color_xml("color", &b[..8])
            );
        }
    }
    if !border.is_empty() {
        let _ = write!(x, "<border>{border}</border>");
    }
    x.push_str("</dxf>");
    Ok(x)
}

#[derive(Default)]
pub(super) struct TableStyle {
    pub name: String,
    pub flags: u16,

    pub elems: Vec<(u32, u32, u32)>,
}

impl Styles {
    pub fn dxf_record(&mut self, d: &[u8]) -> Result<(), String> {
        let x = dxf_xml(d)?;
        self.dxfs.push(x);
        Ok(())
    }

    pub fn table_styles_record(&mut self, d: &[u8]) -> Result<(), String> {
        let mut c = Cur::new(d);
        c.take(12)?;
        c.u32()?;
        let a = usize::from(c.u16()?);
        let b = usize::from(c.u16()?);
        let t = c.chars(a, true, 0)?;
        let p = c.chars(b, true, 0)?;
        self.table_default = Some((t, p));
        Ok(())
    }

    pub fn table_style_record(&mut self, d: &[u8]) -> Result<(), String> {
        let mut c = Cur::new(d);
        c.take(12)?;
        let flags = c.u16()?;
        c.u32()?;
        let n = usize::from(c.u16()?);
        let name = c.chars(n, true, 0)?;
        self.table_styles.push(TableStyle {
            name,
            flags,
            elems: Vec::new(),
        });
        Ok(())
    }

    pub fn table_style_element(&mut self, d: &[u8]) -> Result<(), String> {
        let mut c = Cur::new(d);
        c.take(12)?;
        let e = (c.u32()?, c.u32()?, c.u32()?);
        if let Some(s) = self.table_styles.last_mut() {
            s.elems.push(e);
        }
        Ok(())
    }

    pub fn table_styles_xml(&self) -> String {
        let (t, p) = self.table_default.clone().unwrap_or_else(|| {
            (
                "TableStyleMedium2".to_string(),
                "PivotStyleLight16".to_string(),
            )
        });
        let mut x = format!(
            "<tableStyles count=\"{}\" defaultTableStyle=\"{}\" defaultPivotStyle=\"{}\">",
            self.table_styles.len(),
            super::super::xlsb::esc(&t),
            super::super::xlsb::esc(&p)
        );
        for s in &self.table_styles {
            let _ = write!(
                x,
                "<tableStyle name=\"{}\"",
                super::super::xlsb::esc(&s.name)
            );
            if s.flags & 2 == 0 {
                x.push_str(" pivot=\"0\"");
            }
            if s.flags & 4 == 0 {
                x.push_str(" table=\"0\"");
            }
            let _ = write!(x, " count=\"{}\">", s.elems.len());
            for (ty, size, idx) in &s.elems {
                let Some(name) = ELEMENT_TYPES.get(*ty as usize) else {
                    continue;
                };
                let _ = write!(x, "<tableStyleElement type=\"{name}\"");
                if *size != 1 {
                    let _ = write!(x, " size=\"{size}\"");
                }
                if (*idx as usize) < self.dxfs.len() {
                    let _ = write!(x, " dxfId=\"{idx}\"");
                }
                x.push_str("/>");
            }
            x.push_str("</tableStyle>");
        }
        x.push_str("</tableStyles>");
        x
    }
}
