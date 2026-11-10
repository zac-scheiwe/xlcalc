use crate::xlsb_rec::P;
use crate::xlsb_styles::{color_bytes, color_xml, BORDER_STYLES, PATTERNS};
use crate::xml::Node;
use std::fmt::Write as _;

const BORDER_SIDES: [(&str, u16); 4] = [("left", 8), ("right", 9), ("top", 6), ("bottom", 7)];

fn on(n: &Node) -> bool {
    !matches!(n.attr("val"), Some("0" | "false"))
}

pub(crate) fn record(p: &mut P, dxf: &Node) {
    let mut props: Vec<(u16, Vec<u8>)> = Vec::new();
    if let Some(pf) = dxf.child("fill").and_then(|f| f.child("patternFill")) {
        if let Some(i) = pf
            .attr("patternType")
            .and_then(|t| PATTERNS.iter().position(|x| *x == t))
        {
            props.push((0, vec![i as u8]));
        }
        if let Some(c) = pf.child("fgColor") {
            props.push((1, color_bytes(Some(c), [0; 8]).to_vec()));
        }
        if let Some(c) = pf.child("bgColor") {
            props.push((2, color_bytes(Some(c), [0; 8]).to_vec()));
        }
    }
    if let Some(f) = dxf.child("font") {
        if let Some(c) = f.child("color") {
            props.push((5, color_bytes(Some(c), [0; 8]).to_vec()));
        }
        if let Some(b) = f.child("b") {
            let w: u16 = if on(b) { 700 } else { 400 };
            props.push((0x19, w.to_le_bytes().to_vec()));
        }
        if let Some(u) = f.child("u") {
            let v: u16 = match u.attr("val") {
                None | Some("single") => 1,
                Some("double") => 2,
                Some("singleAccounting") => 0x21,
                Some("doubleAccounting") => 0x22,
                _ => 0,
            };
            props.push((0x1A, v.to_le_bytes().to_vec()));
        }
        if let Some(i) = f.child("i") {
            props.push((0x1C, vec![u8::from(on(i))]));
        }
        if let Some(s) = f.child("strike") {
            props.push((0x1D, vec![u8::from(on(s))]));
        }
    }
    if let Some(b) = dxf.child("border") {
        for (side, t) in BORDER_SIDES {
            let Some(n) = b.child(side) else { continue };
            let Some(st) = n
                .attr("style")
                .and_then(|s| BORDER_STYLES.iter().position(|x| *x == s))
            else {
                continue;
            };
            let mut d = color_bytes(n.child("color"), [1, 0, 0, 0, 0, 0, 0, 0]).to_vec();
            d.extend((st as u16).to_le_bytes());
            props.push((t, d));
        }
    }
    if let Some(n) = dxf.child("numFmt") {
        let id: u16 = n.attr("numFmtId").and_then(|v| v.parse().ok()).unwrap_or(0);
        props.push((0x29, id.to_le_bytes().to_vec()));
        let code: Vec<u16> = n.attr("formatCode").unwrap_or("").encode_utf16().collect();
        let mut d = (code.len() as u16).to_le_bytes().to_vec();
        for u in code {
            d.extend(u.to_le_bytes());
        }
        props.push((0x26, d));
    }
    p.u32(0x8000).u16(props.len() as u16);
    for (t, d) in props {
        p.u16(t).u16(d.len() as u16 + 4).raw(&d);
    }
}

fn color(b: &[u8]) -> Result<[u8; 8], String> {
    b.get(..8)
        .and_then(|c| <[u8; 8]>::try_from(c).ok())
        .ok_or_else(|| "bad dxf colour".to_string())
}

pub(crate) fn xml(d: &[u8]) -> Result<String, String> {
    let short = || "dxf record too short".to_string();
    let cprops = u16::from_le_bytes(
        d.get(4..6)
            .ok_or_else(short)?
            .try_into()
            .map_err(|_| short())?,
    );
    let mut pos = 6;
    let mut pattern: Option<&str> = None;
    let (mut fg, mut bg) = (String::new(), String::new());
    let mut sides: [String; 4] = Default::default();
    let (mut code, mut id) = (None, 0u16);
    let (mut font_color, mut font_flags) = (String::new(), String::new());
    for _ in 0..cprops {
        let h = d.get(pos..pos + 4).ok_or_else(short)?;
        let t = u16::from_le_bytes([h[0], h[1]]);
        let cb = usize::from(u16::from_le_bytes([h[2], h[3]]));
        if cb < 4 {
            return Err("bad dxf property".into());
        }
        let v = d.get(pos + 4..pos + cb).ok_or_else(short)?;
        pos += cb;
        let word = v.get(..2).map_or(0, |b| u16::from_le_bytes([b[0], b[1]]));
        match t {
            0 => {
                pattern = v
                    .first()
                    .and_then(|i| PATTERNS.get(usize::from(*i)))
                    .copied()
            }
            1 => fg = color_xml("fgColor", color(v)?),
            2 => bg = color_xml("bgColor", color(v)?),
            5 => font_color = color_xml("color", color(v)?),
            0x19 => font_flags.push_str(if word >= 600 {
                "<b/>"
            } else {
                "<b val=\"0\"/>"
            }),
            0x1A => font_flags.push_str(match word {
                0 => "<u val=\"none\"/>",
                2 => "<u val=\"double\"/>",
                0x21 => "<u val=\"singleAccounting\"/>",
                0x22 => "<u val=\"doubleAccounting\"/>",
                _ => "<u/>",
            }),
            0x1C => font_flags.push_str(if v.first() == Some(&0) {
                "<i val=\"0\"/>"
            } else {
                "<i/>"
            }),
            0x1D => font_flags.push_str(if v.first() == Some(&0) {
                "<strike val=\"0\"/>"
            } else {
                "<strike/>"
            }),
            6..=9 => {
                let side = BORDER_SIDES.iter().position(|s| s.1 == t).unwrap_or(0);
                let c = color(v)?;
                let st = v
                    .get(8..10)
                    .map_or(0, |b| usize::from(u16::from_le_bytes([b[0], b[1]])));
                if let Some(name) = BORDER_STYLES.get(st).filter(|_| st != 0) {
                    sides[side] = format!(
                        "<{0} style=\"{name}\">{1}</{0}>",
                        BORDER_SIDES[side].0,
                        color_xml("color", c)
                    );
                }
            }
            0x29 => id = word,
            0x26 => {
                let units: Vec<u16> = v
                    .get(2..2 + 2 * usize::from(word))
                    .unwrap_or(&[])
                    .as_chunks::<2>()
                    .0
                    .iter()
                    .map(|c| u16::from_le_bytes(*c))
                    .collect();
                code = Some(String::from_utf16_lossy(&units));
            }
            _ => {}
        }
    }
    let mut out = String::from("<dxf>");
    if !font_flags.is_empty() || !font_color.is_empty() {
        let _ = write!(out, "<font>{font_flags}{font_color}</font>");
    }
    if let Some(c) = code {
        let esc = c
            .replace('&', "&amp;")
            .replace('<', "&lt;")
            .replace('"', "&quot;");
        let _ = write!(out, "<numFmt numFmtId=\"{id}\" formatCode=\"{esc}\"/>");
    }
    if pattern.is_some() || !fg.is_empty() || !bg.is_empty() {
        let pt = pattern.map_or(String::new(), |p| format!(" patternType=\"{p}\""));
        let _ = write!(out, "<fill><patternFill{pt}>{fg}{bg}</patternFill></fill>");
    }
    if sides.iter().any(|s| !s.is_empty()) {
        let _ = write!(out, "<border>{}</border>", sides.concat());
    }
    out.push_str("</dxf>");
    Ok(out)
}
