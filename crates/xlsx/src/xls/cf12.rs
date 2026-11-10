use super::cur::Cur;
use super::read::Fx;
use super::wextras::rgce;
use super::wsheet::Cx;
use crate::xlsb::{esc, Dec};
use crate::xlsb_enc::Cls;
use crate::xml::Node;
use std::fmt::Write as _;

const COLOR_SCALE: u8 = 3;
const DATA_BAR: u8 = 4;
const ICON_SET: u8 = 6;

const KINDS: [(u8, &str); 6] = [
    (1, "num"),
    (2, "min"),
    (3, "max"),
    (4, "percent"),
    (5, "percentile"),
    (7, "formula"),
];

const ICONS: [&str; 17] = [
    "3Arrows",
    "3ArrowsGray",
    "3Flags",
    "3TrafficLights1",
    "3TrafficLights2",
    "3Signs",
    "3Symbols",
    "3Symbols2",
    "4Arrows",
    "4ArrowsGray",
    "4RedToBlack",
    "4Rating",
    "4TrafficLights",
    "5Arrows",
    "5ArrowsGray",
    "5Rating",
    "5Quarters",
];

fn has_value(kind: u8) -> bool {
    matches!(kind, 1 | 4 | 5)
}

fn num_text(v: f64) -> String {
    format!("{v}")
}

fn formula(
    fx: &mut Fx,
    b: &[u8],
    base: (u32, u32),
    cp: u16,
    idx: u32,
    sheet: &str,
) -> Result<String, String> {
    match fx.text(b, &[], base, Some(idx), cp)? {
        Dec::Text(t) => Ok(t
            .replace(&format!("'{}'!", sheet.replace('\'', "''")), "")
            .replace(&format!("{sheet}!"), "")),
        Dec::Exp { .. } => Err("shared formula in a conditional format".into()),
    }
}

struct Vo {
    kind: u8,
    text: String,
    pos: f64,
    gte: bool,
}

fn cfvo(
    c: &mut Cur<'_>,
    fx: &mut Fx,
    base: (u32, u32),
    cp: u16,
    idx: u32,
    sheet: &str,
    tail: Tail,
) -> Result<Vo, String> {
    let kind = c.u8()?;
    let n = usize::from(c.u16()?);
    let f = c.take(n)?;
    let mut text = String::new();
    if kind == 7 {
        text = formula(fx, f, base, cp, idx, sheet)?;
    }
    if has_value(kind) {
        text = num_text(c.f64()?);
    }
    let mut vo = Vo {
        kind,
        text,
        pos: 0.0,
        gte: true,
    };
    match tail {
        Tail::Pos => vo.pos = c.f64()?,
        Tail::Gte => {
            vo.gte = c.u8()? != 0;
            c.take(4)?;
        }
        Tail::None => {}
    }
    Ok(vo)
}

#[derive(Clone, Copy)]
enum Tail {
    None,
    Pos,
    Gte,
}

fn cfvo_xml(v: &Vo) -> String {
    let name = KINDS.iter().find(|k| k.0 == v.kind).map_or("num", |k| k.1);
    let mut x = format!("<cfvo type=\"{name}\"");
    if matches!(v.kind, 1 | 4 | 5 | 7) {
        let _ = write!(x, " val=\"{}\"", esc(&v.text));
    }
    if !v.gte {
        x.push_str(" gte=\"0\"");
    }
    x.push_str("/>");
    x
}

fn color(c: &mut Cur<'_>) -> Result<String, String> {
    c.u32()?;
    let b = c.take(4)?;
    c.f64()?;
    Ok(format!("FF{:02X}{:02X}{:02X}", b[0], b[1], b[2]))
}

pub(super) fn read(
    raw: &[u8],
    base: (u32, u32),
    fx: &mut Fx,
    cp: u16,
    idx: u32,
    sheet: &str,
) -> Result<String, String> {
    let body = raw.get(12..).ok_or("short CF12 record")?;
    let mut c = Cur::new(body);
    let ct = c.u8()?;
    c.take(5)?;
    c.take(8)?;
    c.u8()?;
    let priority = c.u16()?;
    c.u16()?;
    let n = usize::from(c.u8()?);
    c.take(n)?;
    c.u16()?;
    match ct {
        COLOR_SCALE => {
            c.u8()?;
            let nv = usize::from(c.u8()?);
            let nc = usize::from(c.u8()?);
            c.u8()?;
            let mut x = format!("<cfRule type=\"colorScale\" priority=\"{priority}\"><colorScale>");
            for _ in 0..nv {
                x.push_str(&cfvo_xml(&cfvo(
                    &mut c,
                    fx,
                    base,
                    cp,
                    idx,
                    sheet,
                    Tail::Pos,
                )?));
            }
            for _ in 0..nc {
                c.f64()?;
                let _ = write!(x, "<color rgb=\"{}\"/>", color(&mut c)?);
            }
            x.push_str("</colorScale></cfRule>");
            Ok(x)
        }
        DATA_BAR => {
            c.u8()?;
            let flags = c.u8()?;
            c.u16()?;
            let col = color(&mut c)?;
            let mut x = format!("<cfRule type=\"dataBar\" priority=\"{priority}\"><dataBar");
            if flags & 2 != 0 {
                x.push_str(" showValue=\"0\"");
            }
            x.push('>');
            for _ in 0..2 {
                x.push_str(&cfvo_xml(&cfvo(
                    &mut c,
                    fx,
                    base,
                    cp,
                    idx,
                    sheet,
                    Tail::None,
                )?));
            }
            let _ = write!(x, "<color rgb=\"{col}\"/></dataBar></cfRule>");
            Ok(x)
        }
        ICON_SET => {
            c.u8()?;
            let count = usize::from(c.u8()?);
            let id = usize::from(c.u8()?);
            let flags = c.u8()?;
            let mut x = format!("<cfRule type=\"iconSet\" priority=\"{priority}\"><iconSet");
            if id != 3 {
                if let Some(name) = ICONS.get(id) {
                    let _ = write!(x, " iconSet=\"{name}\"");
                }
            }
            if flags & 1 != 0 {
                x.push_str(" showValue=\"0\"");
            }
            if flags & 4 != 0 {
                x.push_str(" reverse=\"1\"");
            }
            x.push('>');
            for _ in 0..count {
                x.push_str(&cfvo_xml(&cfvo(
                    &mut c,
                    fx,
                    base,
                    cp,
                    idx,
                    sheet,
                    Tail::Gte,
                )?));
            }
            x.push_str("</iconSet></cfRule>");
            Ok(x)
        }
        other => Err(format!("conditional format kind {other}")),
    }
}

fn kind_of(name: &str) -> Option<u8> {
    match name {
        "autoMin" => Some(2),
        "autoMax" => Some(3),
        _ => KINDS.iter().find(|k| k.1 == name).map(|k| k.0),
    }
}

fn put_cfvo(
    cx: &mut Cx<'_>,
    idx: u32,
    n: &Node,
    base: (u32, u32),
    tail: Tail,
    pos: f64,
    out: &mut Vec<u8>,
) -> Result<usize, String> {
    let kind = kind_of(n.attr("type").unwrap_or("")).ok_or("unknown threshold type")?;
    let val = n.attr("val").unwrap_or("");
    out.push(kind);
    let mut nominal = 3;
    if kind == 7 {
        let f = rgce(cx, idx, val, base, Cls::Value)?;
        out.extend((f.len() as u16).to_le_bytes());
        nominal += f.len();
        out.extend(f);
    } else {
        out.extend(0u16.to_le_bytes());
        nominal += 8;
        if has_value(kind) {
            let v: f64 = val.parse().map_err(|_| "threshold value is not a number")?;
            out.extend(v.to_le_bytes());
        }
    }
    match tail {
        Tail::Pos => {
            out.extend(pos.to_le_bytes());
            nominal += 8;
        }
        Tail::Gte => {
            out.push(u8::from(n.attr("gte") != Some("0")));
            out.extend([0u8; 4]);
            nominal += 5;
        }
        Tail::None => {}
    }
    Ok(nominal)
}

fn put_color(n: &Node, out: &mut Vec<u8>) -> Result<(), String> {
    let rgb = n
        .attr("rgb")
        .and_then(|v| u32::from_str_radix(v, 16).ok())
        .ok_or("a colour that is not RGB")?;
    out.extend(2u32.to_le_bytes());
    out.extend([(rgb >> 16) as u8, (rgb >> 8) as u8, rgb as u8, 0xFF]);
    out.extend(0f64.to_le_bytes());
    Ok(())
}

pub(super) fn write(
    cx: &mut Cx<'_>,
    idx: u32,
    rule: &Node,
    base: (u32, u32),
) -> Result<Vec<u8>, String> {
    let ty = rule.attr("type").unwrap_or("");
    let priority: u16 = rule
        .attr("priority")
        .and_then(|p| p.parse().ok())
        .unwrap_or(1);
    let mut block = Vec::new();
    let mut size = 0usize;
    let (ct, template) = match ty {
        "colorScale" => {
            let cs = rule.child("colorScale").ok_or("no colorScale")?;
            let vos: Vec<&Node> = cs.children("cfvo").collect();
            let cols: Vec<&Node> = cs.children("color").collect();
            if !(2..=3).contains(&vos.len()) || vos.len() != cols.len() {
                return Err("a colour scale needs two or three thresholds and colours".into());
            }
            let n = vos.len();
            block.extend([0, n as u8, n as u8, 3]);
            size += 4;
            let pos = |i: usize| i as f64 / (n - 1) as f64;
            for (i, v) in vos.iter().enumerate() {
                size += put_cfvo(cx, idx, v, base, Tail::Pos, pos(i), &mut block)?;
            }
            for (i, c) in cols.iter().enumerate() {
                block.extend(pos(i).to_le_bytes());
                put_color(c, &mut block)?;
                size += 24;
            }
            (COLOR_SCALE, 2u16)
        }
        "dataBar" => {
            let db = rule.child("dataBar").ok_or("no dataBar")?;
            let vos: Vec<&Node> = db.children("cfvo").collect();
            let col = db.child("color").ok_or("a data bar without a colour")?;
            if vos.len() != 2 {
                return Err("a data bar needs two thresholds".into());
            }
            let hide = db.attr("showValue") == Some("0");
            block.extend([0, if hide { 2 } else { 0 }, 0, 100]);
            put_color(col, &mut block)?;
            size += 20;
            for v in vos {
                size += put_cfvo(cx, idx, v, base, Tail::None, 0.0, &mut block)?;
            }
            (DATA_BAR, 3)
        }
        "iconSet" => {
            let is = rule.child("iconSet").ok_or("no iconSet")?;
            let vos: Vec<&Node> = is.children("cfvo").collect();
            let name = is.attr("iconSet").unwrap_or("3TrafficLights1");
            let id = ICONS
                .iter()
                .position(|n| *n == name)
                .ok_or_else(|| format!("icon set {name}"))?;
            let mut flags = 0u8;
            if is.attr("showValue") == Some("0") {
                flags |= 1;
            }
            if is.attr("reverse") == Some("1") {
                flags |= 4;
            }
            block.extend([0, vos.len() as u8, id as u8, flags]);
            size += 4;
            for v in vos {
                size += put_cfvo(cx, idx, v, base, Tail::Gte, 0.0, &mut block)?;
            }
            (ICON_SET, 4)
        }
        other => return Err(format!("rule type {other}")),
    };
    let mut d = vec![0x7A, 0x08, 0, 0];
    d.extend([0u8; 8]);
    d.extend([ct, 0, 0, 0, 0, 0]);
    d.extend([0u8; 8]);
    d.push(1);
    d.extend(priority.to_le_bytes());
    d.extend(template.to_le_bytes());
    d.push(0x10);
    d.extend([0u8; 16]);
    d.extend((size as u16).to_le_bytes());
    d.extend(block);
    Ok(d)
}
