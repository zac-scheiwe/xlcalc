use crate::xlsb_rec::{Recs, P};
use crate::xml::{self, Node};
use std::fmt::Write as _;

pub(crate) const BORDER_STYLES: [&str; 14] = [
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

pub(crate) const PATTERNS: [&str; 19] = [
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

const H_ALIGN: [&str; 8] = [
    "general",
    "left",
    "center",
    "right",
    "fill",
    "justify",
    "centerContinuous",
    "distributed",
];

const V_ALIGN: [&str; 5] = ["top", "center", "bottom", "justify", "distributed"];

const ATTRS: [&str; 6] = [
    "applyNumberFormat",
    "applyFont",
    "applyAlignment",
    "applyBorder",
    "applyFill",
    "applyProtection",
];

fn num(n: &Node, key: &str) -> u32 {
    n.attr(key).and_then(|v| v.parse().ok()).unwrap_or(0)
}

fn flag(n: &Node, key: &str, default: bool) -> bool {
    match n.attr(key) {
        Some("1" | "true") => true,
        Some(_) => false,
        None => default,
    }
}

fn index_of(list: &[&str], v: Option<&str>) -> Option<usize> {
    v.and_then(|v| list.iter().position(|x| *x == v))
}

pub(crate) fn color_bytes(c: Option<&Node>, default: [u8; 8]) -> [u8; 8] {
    let Some(c) = c else { return default };
    let tint = c
        .attr("tint")
        .and_then(|t| t.parse::<f64>().ok())
        .map_or(0i16, |t| (t * 32767.0).round() as i16)
        .to_le_bytes();
    let mut b = [0u8; 8];
    b[2] = tint[0];
    b[3] = tint[1];
    if let Some(rgb) = c.attr("rgb") {
        let v = u32::from_str_radix(rgb, 16).unwrap_or(0);
        b[0] = 0x05;
        b[1] = 0xFF;
        b[4] = (v >> 16) as u8;
        b[5] = (v >> 8) as u8;
        b[6] = v as u8;
        b[7] = (v >> 24) as u8;
    } else if let Some(t) = c.attr("theme") {
        b[0] = 0x07;
        b[1] = t.parse().unwrap_or(0);
        b[7] = 0xFF;
    } else if let Some(i) = c.attr("indexed") {
        b[0] = 0x03;
        b[1] = i.parse().unwrap_or(0);
        if b[1] == 65 {
            b[4..].fill(0xFF);
        } else {
            b[7] = 0xFF;
        }
    } else {
        b[0] = 0x01;
    }
    b
}

pub(crate) const AUTO: [u8; 8] = [1, 0, 0, 0, 0, 0, 0, 0];
const FILL_FORE: [u8; 8] = [3, 64, 0, 0, 0, 0, 0, 0xFF];
const FILL_BACK: [u8; 8] = [3, 65, 0, 0, 0xFF, 0xFF, 0xFF, 0xFF];

fn count_rec(r: &mut Recs, id: u32, n: usize) {
    r.with(id, |p| {
        p.u32(n as u32);
    });
}

fn xf_record(p: &mut P, x: &Node, parent: u16, style_xf: bool) {
    let al = x.child("alignment");
    let pr = x.child("protection");
    p.u16(parent)
        .u16(num(x, "numFmtId") as u16)
        .u16(num(x, "fontId") as u16)
        .u16(num(x, "fillId") as u16)
        .u16(num(x, "borderId") as u16);
    let (mut trot, mut indent, mut a, mut b) = (0u8, 0u8, 0x10u8, 0x10u8);
    if let Some(al) = al {
        trot = num(al, "textRotation") as u8;
        indent = num(al, "indent") as u8;
        let h = index_of(&H_ALIGN, al.attr("horizontal")).unwrap_or(0) as u8;
        let v = index_of(&V_ALIGN, al.attr("vertical")).unwrap_or(2) as u8;
        a = h | (v << 3);
        if flag(al, "wrapText", false) {
            a |= 0x40;
        }
        if flag(al, "justifyLastLine", false) {
            a |= 0x80;
        }
        b = (num(al, "readingOrder") as u8 & 3) | b & !3;
        if flag(al, "shrinkToFit", false) {
            b |= 0x04;
        }
    }
    if let Some(pr) = pr {
        if !flag(pr, "locked", true) {
            b &= !0x10;
        }
        if flag(pr, "hidden", false) {
            b |= 0x20;
        }
    }
    if flag(x, "quotePrefix", false) {
        b |= 0x80;
    }
    let mut atr = 0u16;
    for (i, k) in ATTRS.iter().enumerate() {
        let set = match x.attr(k) {
            Some(v) => (v == "1" || v == "true") != style_xf,
            None => false,
        };
        if set {
            atr |= 1 << i;
        }
    }
    p.u8(trot).u8(indent).u8(a).u8(b).u16(atr);
}

pub(crate) fn to_bin(xml_bytes: &[u8]) -> Result<Vec<u8>, String> {
    let root = xml::parse(xml_bytes)?;
    let mut r = Recs::default();
    r.empty(278);

    let fmts: Vec<&Node> = root
        .child("numFmts")
        .map(|n| n.children("numFmt").collect())
        .unwrap_or_default();
    count_rec(&mut r, 615, fmts.len());
    for f in fmts {
        r.with(44, |p| {
            p.u16(num(f, "numFmtId") as u16)
                .wstr(f.attr("formatCode").unwrap_or(""));
        });
    }
    r.empty(616);

    let fonts: Vec<&Node> = root
        .child("fonts")
        .map(|n| n.children("font").collect())
        .unwrap_or_default();
    count_rec(&mut r, 611, fonts.len());
    for f in fonts {
        r.with(43, |p| font_record(p, f));
    }
    r.empty(612);

    let fills: Vec<&Node> = root
        .child("fills")
        .map(|n| n.children("fill").collect())
        .unwrap_or_default();
    count_rec(&mut r, 603, fills.len());
    for f in fills {
        r.with(45, |p| {
            let pf = f.child("patternFill");
            let pat = pf.map_or(0, |pf| {
                index_of(&PATTERNS, pf.attr("patternType")).unwrap_or(0)
            });
            p.u32(pat as u32);
            p.raw(&color_bytes(pf.and_then(|x| x.child("fgColor")), FILL_FORE));
            p.raw(&color_bytes(pf.and_then(|x| x.child("bgColor")), FILL_BACK));
            p.raw(&[0u8; 48]);
        });
    }
    r.empty(604);

    let borders: Vec<&Node> = root
        .child("borders")
        .map(|n| n.children("border").collect())
        .unwrap_or_default();
    count_rec(&mut r, 613, borders.len());
    for b in borders {
        r.with(46, |p| {
            let mut fl = 0u8;
            if flag(b, "diagonalDown", false) {
                fl |= 1;
            }
            if flag(b, "diagonalUp", false) {
                fl |= 2;
            }
            p.u8(fl);
            for side in ["top", "bottom", "left", "right", "diagonal"] {
                let s = b.child(side);
                let style = s
                    .and_then(|s| index_of(&BORDER_STYLES, s.attr("style")))
                    .unwrap_or(0);
                p.u16(style as u16);
                p.raw(&color_bytes(s.and_then(|s| s.child("color")), AUTO));
            }
        });
    }
    r.empty(614);

    let style_xfs: Vec<&Node> = root
        .child("cellStyleXfs")
        .map(|n| n.children("xf").collect())
        .unwrap_or_default();
    count_rec(&mut r, 626, style_xfs.len());
    for x in style_xfs {
        r.with(47, |p| xf_record(p, x, 0xFFFF, true));
    }
    r.empty(627);

    let xfs: Vec<&Node> = root
        .child("cellXfs")
        .map(|n| n.children("xf").collect())
        .unwrap_or_default();
    count_rec(&mut r, 617, xfs.len());
    for x in xfs {
        r.with(47, |p| xf_record(p, x, num(x, "xfId") as u16, false));
    }
    r.empty(618);

    let styles: Vec<&Node> = root
        .child("cellStyles")
        .map(|n| n.children("cellStyle").collect())
        .unwrap_or_default();
    count_rec(&mut r, 619, styles.len());
    for s in styles {
        r.with(48, |p| {
            let builtin = s.attr("builtinId").and_then(|v| v.parse::<u8>().ok());
            p.u32(num(s, "xfId"))
                .u16(u16::from(builtin.is_some()))
                .u8(builtin.unwrap_or(0))
                .u8(0)
                .wstr(s.attr("name").unwrap_or(""));
        });
    }
    r.empty(620);

    let dxfs: Vec<&Node> = root
        .child("dxfs")
        .map(|n| n.children("dxf").collect())
        .unwrap_or_default();
    count_rec(&mut r, 505, dxfs.len());
    for x in dxfs {
        r.with(507, |p| crate::xlsb_dxf::record(p, x));
    }
    r.empty(506);
    r.with(508, |p| {
        p.u32(0)
            .wstr(
                root.child("tableStyles")
                    .and_then(|t| t.attr("defaultTableStyle"))
                    .unwrap_or("TableStyleMedium2"),
            )
            .wstr(
                root.child("tableStyles")
                    .and_then(|t| t.attr("defaultPivotStyle"))
                    .unwrap_or("PivotStyleLight16"),
            );
    });
    r.empty(509);
    r.empty(279);
    Ok(r.bytes)
}

fn font_record(p: &mut P, f: &Node) {
    let on = |k: &str| f.child(k).is_some_and(|n| flag(n, "val", true));
    let sz = f
        .child("sz")
        .and_then(|n| n.attr("val"))
        .and_then(|v| v.parse::<f64>().ok())
        .unwrap_or(11.0);
    let bold = on("b");
    let mut grbit = 0u16;
    if bold {
        grbit |= 1;
    }
    if on("i") {
        grbit |= 2;
    }
    if on("strike") {
        grbit |= 8;
    }
    let uls: u8 = match f.child("u") {
        None => 0,
        Some(u) => match u.attr("val") {
            None | Some("single") => 1,
            Some("double") => 2,
            Some("singleAccounting") => 0x21,
            Some("doubleAccounting") => 0x22,
            _ => 0,
        },
    };
    let sss: u16 = match f.child("vertAlign").and_then(|n| n.attr("val")) {
        Some("superscript") => 1,
        Some("subscript") => 2,
        _ => 0,
    };
    let val = |k: &str| f.child(k).map_or(0, |n| num(n, "val"));
    let scheme = match f.child("scheme").and_then(|n| n.attr("val")) {
        Some("major") => 1,
        Some("minor") => 2,
        _ => 0,
    };
    p.u16((sz * 20.0).round() as u16)
        .u16(grbit)
        .u16(if bold { 700 } else { 400 })
        .u16(sss)
        .u8(uls)
        .u8(val("family") as u8)
        .u8(val("charset") as u8)
        .u8(0)
        .raw(&color_bytes(f.child("color"), [7, 1, 0, 0, 0, 0, 0, 0xFF]))
        .u8(scheme)
        .wstr(f.child("name").and_then(|n| n.attr("val")).unwrap_or(""));
}

fn esc(s: &str) -> String {
    s.replace('&', "&amp;")
        .replace('<', "&lt;")
        .replace('>', "&gt;")
        .replace('"', "&quot;")
}

struct Rd<'a> {
    b: &'a [u8],
    p: usize,
}

impl Rd<'_> {
    fn take(&mut self, n: usize) -> Result<&[u8], String> {
        let s = self.b.get(self.p..self.p + n).ok_or("record too short")?;
        self.p += n;
        Ok(s)
    }
    fn u8(&mut self) -> Result<u8, String> {
        Ok(self.take(1)?[0])
    }
    fn u16(&mut self) -> Result<u16, String> {
        let s = self.take(2)?;
        Ok(u16::from_le_bytes([s[0], s[1]]))
    }
    fn u32(&mut self) -> Result<u32, String> {
        let s = self.take(4)?;
        Ok(u32::from_le_bytes([s[0], s[1], s[2], s[3]]))
    }
    fn wstr(&mut self) -> Result<String, String> {
        let n = self.u32()?;
        if n == 0xFFFF_FFFF {
            return Ok(String::new());
        }
        let units: Vec<u16> = self
            .take(n as usize * 2)?
            .as_chunks::<2>()
            .0
            .iter()
            .map(|c| u16::from_le_bytes(*c))
            .collect();
        Ok(String::from_utf16_lossy(&units))
    }
    fn color(&mut self) -> Result<[u8; 8], String> {
        let mut c = [0u8; 8];
        c.copy_from_slice(self.take(8)?);
        Ok(c)
    }
}

pub(crate) fn color_xml(tag: &str, c: [u8; 8]) -> String {
    let mut s = format!("<{tag}");
    match c[0] >> 1 {
        0 => s.push_str(" auto=\"1\""),
        1 => {
            let _ = write!(s, " indexed=\"{}\"", c[1]);
        }
        2 => {
            let _ = write!(
                s,
                " rgb=\"{:02X}{:02X}{:02X}{:02X}\"",
                c[7], c[4], c[5], c[6]
            );
        }
        3 => {
            let _ = write!(s, " theme=\"{}\"", c[1]);
        }
        _ => s.push_str(" auto=\"1\""),
    }
    let tint = i16::from_le_bytes([c[2], c[3]]);
    if tint != 0 {
        let _ = write!(s, " tint=\"{}\"", f64::from(tint) / 32767.0);
    }
    s.push_str("/>");
    s
}

fn font_xml(d: &[u8]) -> Result<String, String> {
    let mut rd = Rd { b: d, p: 0 };
    let dy = rd.u16()?;
    let grbit = rd.u16()?;
    let bls = rd.u16()?;
    let sss = rd.u16()?;
    let uls = rd.u8()?;
    let family = rd.u8()?;
    let charset = rd.u8()?;
    rd.u8()?;
    let color = rd.color()?;
    let scheme = rd.u8()?;
    let name = rd.wstr()?;
    let mut s = String::from("<font>");
    if bls >= 700 {
        s.push_str("<b/>");
    }
    if grbit & 2 != 0 {
        s.push_str("<i/>");
    }
    if grbit & 8 != 0 {
        s.push_str("<strike/>");
    }
    match uls {
        0 => {}
        2 => s.push_str("<u val=\"double\"/>"),
        0x21 => s.push_str("<u val=\"singleAccounting\"/>"),
        0x22 => s.push_str("<u val=\"doubleAccounting\"/>"),
        _ => s.push_str("<u/>"),
    }
    match sss {
        1 => s.push_str("<vertAlign val=\"superscript\"/>"),
        2 => s.push_str("<vertAlign val=\"subscript\"/>"),
        _ => {}
    }
    let _ = write!(s, "<sz val=\"{}\"/>", f64::from(dy) / 20.0);
    s.push_str(&color_xml("color", color));
    let _ = write!(s, "<name val=\"{}\"/>", esc(&name));
    if family != 0 {
        let _ = write!(s, "<family val=\"{family}\"/>");
    }
    if charset != 0 {
        let _ = write!(s, "<charset val=\"{charset}\"/>");
    }
    match scheme {
        1 => s.push_str("<scheme val=\"major\"/>"),
        2 => s.push_str("<scheme val=\"minor\"/>"),
        _ => {}
    }
    s.push_str("</font>");
    Ok(s)
}

fn fill_xml(d: &[u8]) -> Result<String, String> {
    let mut rd = Rd { b: d, p: 0 };
    let pat = rd.u32()? as usize;
    let fore = rd.color()?;
    let back = rd.color()?;
    let name = PATTERNS.get(pat).copied().unwrap_or("none");
    if pat == 0 || pat == 17 || pat == 18 {
        return Ok(format!(
            "<fill><patternFill patternType=\"{name}\"/></fill>"
        ));
    }
    Ok(format!(
        "<fill><patternFill patternType=\"{name}\">{}{}</patternFill></fill>",
        color_xml("fgColor", fore),
        color_xml("bgColor", back)
    ))
}

fn border_xml(d: &[u8]) -> Result<String, String> {
    let mut rd = Rd { b: d, p: 0 };
    let fl = rd.u8()?;
    let mut sides: Vec<(u16, [u8; 8])> = Vec::new();
    for _ in 0..5 {
        let st = rd.u16()?;
        sides.push((st, rd.color()?));
    }
    let mut s = String::from("<border");
    if fl & 1 != 0 {
        s.push_str(" diagonalDown=\"1\"");
    }
    if fl & 2 != 0 {
        s.push_str(" diagonalUp=\"1\"");
    }
    s.push('>');

    for (tag, i) in [
        ("left", 2),
        ("right", 3),
        ("top", 0),
        ("bottom", 1),
        ("diagonal", 4),
    ] {
        let (st, c) = sides[i];
        if st == 0 {
            let _ = write!(s, "<{tag}/>");
        } else {
            let _ = write!(
                s,
                "<{tag} style=\"{}\">{}</{tag}>",
                BORDER_STYLES
                    .get(usize::from(st))
                    .copied()
                    .unwrap_or("thin"),
                color_xml("color", c)
            );
        }
    }
    s.push_str("</border>");
    Ok(s)
}

fn xf_xml(d: &[u8], style_xf: bool) -> Result<String, String> {
    let mut rd = Rd { b: d, p: 0 };
    let parent = rd.u16()?;
    let fmt = rd.u16()?;
    let font = rd.u16()?;
    let fill = rd.u16()?;
    let border = rd.u16()?;
    let trot = rd.u8()?;
    let indent = rd.u8()?;
    let a = rd.u8()?;
    let b = rd.u8()?;
    let atr = rd.u16()?;
    let mut s =
        format!("<xf numFmtId=\"{fmt}\" fontId=\"{font}\" fillId=\"{fill}\" borderId=\"{border}\"");
    if !style_xf {
        let _ = write!(s, " xfId=\"{parent}\"");
    }
    for (i, k) in ATTRS.iter().enumerate() {
        let bit = atr & (1 << i) != 0;
        if style_xf {
            if bit {
                let _ = write!(s, " {k}=\"0\"");
            }
        } else {
            let _ = write!(s, " {k}=\"{}\"", u8::from(bit));
        }
    }
    if b & 0x80 != 0 {
        s.push_str(" quotePrefix=\"1\"");
    }
    let mut kids = String::new();
    let h = usize::from(a & 7);
    let v = usize::from((a >> 3) & 7);
    if h != 0 || v != 2 || a & 0xC0 != 0 || trot != 0 || indent != 0 || b & 0x07 != 0 {
        kids.push_str("<alignment");
        if h != 0 {
            let _ = write!(kids, " horizontal=\"{}\"", H_ALIGN[h]);
        }
        if v != 2 {
            let _ = write!(
                kids,
                " vertical=\"{}\"",
                V_ALIGN.get(v).copied().unwrap_or("bottom")
            );
        }
        if trot != 0 {
            let _ = write!(kids, " textRotation=\"{trot}\"");
        }
        if a & 0x40 != 0 {
            kids.push_str(" wrapText=\"1\"");
        }
        if indent != 0 {
            let _ = write!(kids, " indent=\"{indent}\"");
        }
        if a & 0x80 != 0 {
            kids.push_str(" justifyLastLine=\"1\"");
        }
        if b & 0x04 != 0 {
            kids.push_str(" shrinkToFit=\"1\"");
        }
        if b & 3 != 0 {
            let _ = write!(kids, " readingOrder=\"{}\"", b & 3);
        }
        kids.push_str("/>");
    }
    if b & 0x10 == 0 || b & 0x20 != 0 {
        kids.push_str("<protection");
        if b & 0x10 == 0 {
            kids.push_str(" locked=\"0\"");
        }
        if b & 0x20 != 0 {
            kids.push_str(" hidden=\"1\"");
        }
        kids.push_str("/>");
    }
    if kids.is_empty() {
        s.push_str("/>");
    } else {
        let _ = write!(s, ">{kids}</xf>");
    }
    Ok(s)
}

pub(crate) fn to_xml(records: impl Iterator<Item = (u32, Vec<u8>)>) -> Result<String, String> {
    let (mut fmts, mut fonts, mut fills, mut borders) =
        (String::new(), String::new(), String::new(), String::new());
    let (mut style_xfs, mut cell_xfs, mut styles) = (String::new(), String::new(), String::new());
    let mut in_cell_xfs = false;
    let mut dxfs = String::new();
    for (id, d) in records {
        match id {
            44 => {
                let mut rd = Rd { b: &d, p: 0 };
                let n = rd.u16()?;
                let code = rd.wstr()?;
                let _ = write!(
                    fmts,
                    "<numFmt numFmtId=\"{n}\" formatCode=\"{}\"/>",
                    esc(&code)
                );
            }
            507 => dxfs.push_str(&crate::xlsb_dxf::xml(&d)?),
            43 => fonts.push_str(&font_xml(&d)?),
            45 => fills.push_str(&fill_xml(&d)?),
            46 => borders.push_str(&border_xml(&d)?),
            617 => in_cell_xfs = true,
            618 => in_cell_xfs = false,
            47 if in_cell_xfs => cell_xfs.push_str(&xf_xml(&d, false)?),
            47 => style_xfs.push_str(&xf_xml(&d, true)?),
            48 => {
                let mut rd = Rd { b: &d, p: 0 };
                let xf = rd.u32()?;
                let flags = rd.u16()?;
                let builtin = rd.u8()?;
                rd.u8()?;
                let name = rd.wstr()?;
                let _ = write!(styles, "<cellStyle name=\"{}\" xfId=\"{xf}\"", esc(&name));
                if flags & 1 != 0 {
                    let _ = write!(styles, " builtinId=\"{builtin}\"");
                }
                styles.push_str("/>");
            }
            _ => {}
        }
    }
    Ok(format!(
        "<?xml version=\"1.0\" encoding=\"UTF-8\"?>\n<styleSheet xmlns=\"http://schemas.openxmlformats.org/spreadsheetml/2006/main\"><numFmts>{fmts}</numFmts><fonts>{fonts}</fonts><fills>{fills}</fills><borders>{borders}</borders><cellStyleXfs>{style_xfs}</cellStyleXfs><cellXfs>{cell_xfs}</cellXfs><cellStyles>{styles}</cellStyles><dxfs>{dxfs}</dxfs></styleSheet>"
    ))
}

#[cfg(test)]
mod tests {
    use super::*;

    fn recs(b: &[u8]) -> Vec<(u32, Vec<u8>)> {
        let mut out = Vec::new();
        let mut p = 0;
        while p < b.len() {
            let mut id = u32::from(b[p] & 0x7F);
            if b[p] & 0x80 != 0 {
                p += 1;
                id |= u32::from(b[p]) << 7;
            }
            p += 1;
            let mut len = 0usize;
            let mut k = 0;
            loop {
                let x = b[p];
                p += 1;
                len |= usize::from(x & 0x7F) << (7 * k);
                k += 1;
                if x & 0x80 == 0 {
                    break;
                }
            }
            out.push((id, b[p..p + len].to_vec()));
            p += len;
        }
        out
    }

    #[test]
    fn a_style_sheet_survives_the_binary_form() {
        let src = r#"<styleSheet xmlns="http://schemas.openxmlformats.org/spreadsheetml/2006/main"><numFmts count="1"><numFmt numFmtId="164" formatCode="yyyy-mm-dd"/></numFmts><fonts count="2"><font><sz val="11"/><color theme="1"/><name val="Aptos Narrow"/><family val="2"/><scheme val="minor"/></font><font><b/><sz val="14"/><color rgb="FFFF0000"/><name val="Courier New"/></font></fonts><fills count="3"><fill><patternFill patternType="none"/></fill><fill><patternFill patternType="gray125"/></fill><fill><patternFill patternType="solid"><fgColor rgb="FF00FF00"/></patternFill></fill></fills><borders count="2"><border><left/><right/><top/><bottom/><diagonal/></border><border><left style="thin"><color indexed="64"/></left><right/><top/><bottom style="thick"><color indexed="64"/></bottom><diagonal/></border></borders><cellStyleXfs count="1"><xf numFmtId="0" fontId="0" fillId="0" borderId="0"/></cellStyleXfs><cellXfs count="3"><xf numFmtId="164" fontId="1" fillId="2" borderId="1" xfId="0" applyNumberFormat="1" applyFont="1"/><xf numFmtId="0" fontId="0" fillId="0" borderId="0" xfId="0" applyAlignment="1"><alignment horizontal="center" wrapText="1"/></xf><xf numFmtId="0" fontId="0" fillId="0" borderId="0" xfId="0" applyProtection="1" quotePrefix="1"><protection locked="0" hidden="1"/></xf></cellXfs><cellStyles count="1"><cellStyle name="Normal" xfId="0" builtinId="0"/></cellStyles></styleSheet>"#;
        let bin = to_bin(src.as_bytes()).unwrap();
        let xml = to_xml(recs(&bin).into_iter()).unwrap();
        for want in [
            "<numFmt numFmtId=\"164\" formatCode=\"yyyy-mm-dd\"/>",
            "<b/><sz val=\"14\"/><color rgb=\"FFFF0000\"/><name val=\"Courier New\"/>",
            "<fgColor rgb=\"FF00FF00\"/>",
            "<left style=\"thin\"><color indexed=\"64\"/></left>",
            "<bottom style=\"thick\">",
            "applyNumberFormat=\"1\" applyFont=\"1\" applyAlignment=\"0\"",
            "<alignment horizontal=\"center\" wrapText=\"1\"/>",
            "quotePrefix=\"1\"><protection locked=\"0\" hidden=\"1\"/>",
            "<cellStyle name=\"Normal\" xfId=\"0\" builtinId=\"0\"/>",
        ] {
            assert!(xml.contains(want), "{want} missing in {xml}");
        }
    }

    #[test]
    fn default_records_match_excel() {
        let src = r#"<styleSheet><borders count="1"><border><left style="thin"><color indexed="64"/></left><right/><top/><bottom/><diagonal/></border></borders><cellXfs count="1"><xf numFmtId="0" fontId="0" fillId="0" borderId="0" xfId="0"/></cellXfs></styleSheet>"#;
        let r = recs(&to_bin(src.as_bytes()).unwrap());
        let xf = r.iter().find(|(i, _)| *i == 47).unwrap();
        assert_eq!(xf.1, [0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0x10, 0x10, 0, 0]);
        let b = r.iter().find(|(i, _)| *i == 46).unwrap();
        assert_eq!(&b.1[21..31], &[1, 0, 3, 0x40, 0, 0, 0, 0, 0, 0xFF]);
    }
}
