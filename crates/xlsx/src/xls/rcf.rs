use super::cur::Cur;
use super::read::{ref8, Fx};
use super::rextras::{attr, ref_str, Rect};
use super::rstyles::{Styles, DEFAULT_PALETTE};
use crate::xlsb::{esc, Dec};
use std::fmt::Write as _;

const NUMFMT: u32 = 0x0200_0000;
const FONT: u32 = 0x0400_0000;
const BORDER: u32 = 0x1000_0000;
const AREA: u32 = 0x2000_0000;

pub(super) struct Group {
    pub ranges: Vec<Rect>,

    pub bound: Rect,
    pub rules: Vec<Vec<u8>>,

    pub rules12: Vec<Vec<u8>>,

    pub no: u16,
}

pub(super) struct Ex {
    pub header: u16,
    pub index: u16,
    pub template: u8,
    pub priority: u16,
    pub flags: u16,

    pub tail: [u8; 17],

    pub colors: Vec<(u16, u32)>,
}

pub(super) fn header(d: &[u8], no: u16) -> Result<Group, String> {
    let mut c = Cur::new(d);
    c.u16()?;
    c.u16()?;
    let bound = ref8(&mut c)?;
    let n = usize::from(c.u16()?);
    let mut ranges = Vec::new();
    for _ in 0..n {
        ranges.push(ref8(&mut c)?);
    }
    Ok(Group {
        ranges,
        bound,
        rules: Vec::new(),
        rules12: Vec::new(),
        no,
    })
}

pub(super) fn ex(d: &[u8]) -> Option<Ex> {
    if d.len() < 26 {
        return None;
    }
    let u = |i: usize| u16::from_le_bytes([d[i], d[i + 1]]);
    let flags = u(24);
    let mut e = Ex {
        header: u(16),
        index: u(18),
        template: d[21],
        priority: u(22),
        flags,
        tail: [0; 17],
        colors: Vec::new(),
    };
    let (dxf, tail_at) = if flags & 0x100 != 0 {
        let cb = u32::from_le_bytes(d.get(26..30)?.try_into().ok()?) as usize;
        (d.get(30..30 + cb)?, 30 + cb)
    } else {
        (&d[0..0], 26)
    };
    let tail = d.get(tail_at..)?;
    let n = tail.len().min(17);
    e.tail[..n].copy_from_slice(&tail[..n]);
    e.colors = dxf_colors(dxf);
    Some(e)
}

fn numfmt_len(flags: u32, ignore: u16, rest: &[u8]) -> usize {
    if flags & NUMFMT == 0 {
        0
    } else if ignore & 1 != 0 {
        rest.get(0..2)
            .map_or(2, |b| usize::from(u16::from_le_bytes([b[0], b[1]])))
    } else {
        2
    }
}

fn dxf_colors(dxf: &[u8]) -> Vec<(u16, u32)> {
    let mut out = Vec::new();
    if dxf.len() < 6 {
        return out;
    }

    let f = u32::from_le_bytes([dxf[0], dxf[1], dxf[2], dxf[3]]);
    let ignore = u16::from_le_bytes([dxf[4], dxf[5]]);
    let mut at = 6 + numfmt_len(f, ignore, &dxf[6..]);
    if f & FONT != 0 {
        at += 118;
    }
    if f & BORDER != 0 {
        at += 8;
    }
    if f & AREA != 0 {
        at += 4;
    }
    let mut p = at + 8;
    let count = dxf
        .get(at + 6..at + 8)
        .map_or(0, |b| u16::from_le_bytes([b[0], b[1]]));
    for _ in 0..count {
        let Some(h) = dxf.get(p..p + 4) else { break };
        let ty = u16::from_le_bytes([h[0], h[1]]);
        let cb = usize::from(u16::from_le_bytes([h[2], h[3]]));
        if cb < 4 {
            break;
        }
        if let Some(b) = dxf.get(p + 4..p + 12) {
            if u16::from_le_bytes([b[0], b[1]]) == 2 {
                let (r, g, bl) = (b[4], b[5], b[6]);
                out.push((
                    ty,
                    0xFF00_0000 | u32::from(r) << 16 | u32::from(g) << 8 | u32::from(bl),
                ));
            }
        }
        p += cb;
    }
    out
}

pub(super) const DXF_FORMATS: [(u8, &str); 22] = [
    (0, "General"),
    (1, "0"),
    (2, "0.00"),
    (3, "#,##0"),
    (4, "#,##0.00"),
    (13, "0%"),
    (14, "0.00%"),
    (15, "0.00E+00"),
    (16, "##0.0E+0"),
    (17, "#\\ ?/?"),
    (18, "#\\ ??/??"),
    (20, "d\\-mmm\\-yy"),
    (21, "d\\-mmm"),
    (22, "mmm\\-yy"),
    (23, "h:mm\\ am/pm"),
    (24, "h:mm:ss\\ am/pm"),
    (25, "h:mm"),
    (26, "h:mm:ss"),
    (28, "mm:ss"),
    (29, "mm:ss.0"),
    (30, "@"),
    (31, "[h]:mm:ss"),
];

pub(super) fn dxf_format_id(code: &str) -> Option<u8> {
    let norm = |s: &str| s.replace('\\', "").to_lowercase();
    let c = norm(code);
    DXF_FORMATS.iter().find(|f| norm(f.1) == c).map(|f| f.0)
}

fn numfmt_xml(ignore: u16, b: &[u8], user_id: usize) -> String {
    if ignore & 1 != 0 {
        let (Some(h), Some(&wide)) = (b.get(2..4), b.get(4)) else {
            return String::new();
        };
        let n = usize::from(u16::from_le_bytes([h[0], h[1]]));
        let text = if wide & 1 != 0 {
            let u: Vec<u16> = b
                .get(5..5 + 2 * n)
                .unwrap_or_default()
                .as_chunks::<2>()
                .0
                .iter()
                .map(|c| u16::from_le_bytes([c[0], c[1]]))
                .collect();
            String::from_utf16_lossy(&u)
        } else {
            b.get(5..5 + n)
                .unwrap_or_default()
                .iter()
                .map(|&c| char::from(c))
                .collect()
        };
        format!(
            "<numFmt numFmtId=\"{user_id}\" formatCode=\"{}\"/>",
            attr(&text)
        )
    } else {
        let id = b.get(1).copied().unwrap_or(0);
        match DXF_FORMATS.iter().find(|f| f.0 == id) {
            Some(f) => format!("<numFmt numFmtId=\"{id}\" formatCode=\"{}\"/>", attr(f.1)),
            None => String::new(),
        }
    }
}

fn palette_rgb(styles: &Styles, icv: u32) -> Option<u32> {
    let i = icv as usize;
    let rgb = match i {
        0..=7 => DEFAULT_PALETTE[i],
        8..=63 => match &styles.palette {
            Some(p) => *p.get(i - 8)?,
            None => DEFAULT_PALETTE[i - 8],
        },
        _ => return None,
    };
    Some(0xFF00_0000 | rgb)
}

fn color(tag: &str, rgb: u32) -> String {
    format!("<{tag} rgb=\"{rgb:08X}\"/>")
}

pub(super) const PATTERNS: [&str; 19] = [
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

pub(super) const BORDERS: [&str; 14] = [
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

fn dxf(flags: u32, ignore: u16, blocks: &[u8], colors: &[(u16, u32)], styles: &Styles) -> String {
    let exact = |ty: u16| colors.iter().find(|c| c.0 == ty).map(|c| c.1);
    let mut at = 0usize;
    let mut numfmt = String::new();
    if flags & NUMFMT != 0 {
        let n = numfmt_len(flags, ignore, blocks);
        numfmt = numfmt_xml(
            ignore,
            blocks.get(..n).unwrap_or_default(),
            164 + styles.dxfs.len(),
        );
        at += n;
    }
    let mut font = String::new();
    if flags & 0x0400_0000 != 0 {
        if let Some(b) = blocks.get(at..at + 118) {
            let u32at = |i: usize| u32::from_le_bytes([b[i], b[i + 1], b[i + 2], b[i + 3]]);
            let style = u32at(68);
            let weight = u16::from_le_bytes([b[72], b[73]]);
            let f1 = u32at(88);
            let f3 = u32at(96);
            if f1 & 1 == 0 {
                font.push_str(if weight >= 700 {
                    "<b/>"
                } else {
                    "<b val=\"0\"/>"
                });
            }
            if f1 & 2 == 0 {
                font.push_str(if style & 2 != 0 {
                    "<i/>"
                } else {
                    "<i val=\"0\"/>"
                });
            }
            if f1 & 0x80 == 0 {
                font.push_str(if style & 0x80 != 0 {
                    "<strike/>"
                } else {
                    "<strike val=\"0\"/>"
                });
            }
            if f3 & 1 == 0 {
                font.push_str(match b[76] {
                    1 => "<u/>",
                    2 => "<u val=\"double\"/>",
                    0x21 => "<u val=\"singleAccounting\"/>",
                    0x22 => "<u val=\"doubleAccounting\"/>",
                    _ => "<u val=\"none\"/>",
                });
            }
            let icv = u32at(80);
            if icv != 0xFFFF_FFFF {
                if let Some(c) = exact(0x0D).or_else(|| palette_rgb(styles, icv)) {
                    font.push_str(&color("color", c));
                }
            }
        }
        at += 118;
    }
    let mut border = String::new();
    if flags & 0x1000_0000 != 0 {
        if let Some(b) = blocks.get(at..at + 8) {
            let st = u16::from_le_bytes([b[0], b[1]]);
            let cl = u32::from_le_bytes([b[2], b[3], b[4], b[5]]);

            for (name, ss, cs, bit, prop) in [
                ("left", 0, 0, 10, 9u16),
                ("right", 4, 7, 11, 10),
                ("top", 8, 16, 12, 7),
                ("bottom", 12, 23, 13, 8),
            ] {
                if flags & (1 << bit) != 0 {
                    continue;
                }
                let s = usize::from((st >> ss) & 0xF);
                let style = BORDERS.get(s).copied().unwrap_or("none");
                if s == 0 {
                    let _ = write!(border, "<{name}/>");
                    continue;
                }
                let col = exact(prop).or_else(|| palette_rgb(styles, (cl >> cs) & 0x7F));
                match col {
                    Some(c) => {
                        let _ = write!(
                            border,
                            "<{name} style=\"{style}\">{}</{name}>",
                            color("color", c)
                        );
                    }
                    None => {
                        let _ = write!(border, "<{name} style=\"{style}\"/>");
                    }
                }
            }
        }
        at += 8;
    }
    let mut fill = String::new();
    if flags & 0x2000_0000 != 0 {
        if let Some(b) = blocks.get(at..at + 4) {
            let pat = usize::from(u16::from_le_bytes([b[0], b[1]]) >> 10);
            let cl = u16::from_le_bytes([b[2], b[3]]);
            let mut inner = String::new();
            let mut ptype = String::new();
            if flags & (1 << 16) == 0 {
                let name = PATTERNS.get(pat).copied().unwrap_or("none");
                if pat != 0 {
                    ptype = format!(" patternType=\"{name}\"");
                }
            }
            if flags & (1 << 17) == 0 {
                if let Some(c) = exact(4).or_else(|| palette_rgb(styles, u32::from(cl & 0x7F))) {
                    inner.push_str(&color("fgColor", c));
                }
            }
            if flags & (1 << 18) == 0 {
                if let Some(c) =
                    exact(5).or_else(|| palette_rgb(styles, u32::from((cl >> 7) & 0x7F)))
                {
                    inner.push_str(&color("bgColor", c));
                }
            }
            fill = format!("<fill><patternFill{ptype}>{inner}</patternFill></fill>");
        }
    }
    let mut x = String::from("<dxf>");
    if !font.is_empty() {
        let _ = write!(x, "<font>{font}</font>");
    }
    x.push_str(&numfmt);
    x.push_str(&fill);
    if !border.is_empty() {
        let _ = write!(x, "<border>{border}</border>");
    }
    x.push_str("</dxf>");
    x
}

fn block_len(flags: u32, ignore: u16, rest: &[u8]) -> usize {
    let mut n = numfmt_len(flags, ignore, rest);
    if flags & 0x0400_0000 != 0 {
        n += 118;
    }
    if flags & 0x1000_0000 != 0 {
        n += 8;
    }
    if flags & 0x2000_0000 != 0 {
        n += 4;
    }
    n
}

const OPERATORS: [&str; 9] = [
    "",
    "between",
    "notBetween",
    "equal",
    "notEqual",
    "greaterThan",
    "lessThan",
    "greaterThanOrEqual",
    "lessThanOrEqual",
];

pub(super) const PERIODS: [(u8, &str); 10] = [
    (15, "today"),
    (16, "tomorrow"),
    (17, "yesterday"),
    (18, "last7Days"),
    (19, "lastMonth"),
    (20, "nextMonth"),
    (21, "thisWeek"),
    (22, "nextWeek"),
    (23, "lastWeek"),
    (24, "thisMonth"),
];

fn search_text(f: &str) -> Option<(&'static str, String)> {
    let kinds = [
        ("NOT(ISERROR(SEARCH(\"", "containsText", "containsText"),
        ("ISERROR(SEARCH(\"", "notContains", "notContainsText"),
        ("LEFT(", "beginsWith", "beginsWith"),
        ("RIGHT(", "endsWith", "endsWith"),
    ];
    for (prefix, op, ty) in kinds {
        if !f.starts_with(prefix) {
            continue;
        }
        let text = if prefix.ends_with('"') {
            let rest = &f[prefix.len()..];
            let end = rest.find("\",")?;
            rest[..end].replace("\"\"", "\"")
        } else {
            let at = f.find("LEN(\"")? + 5;
            let rest = &f[at..];
            let end = rest.find("\"))=")?;
            rest[..end].replace("\"\"", "\"")
        };
        let _ = ty;
        return Some((op, text));
    }
    None
}

fn type_of_search(op: &str) -> &'static str {
    match op {
        "containsText" => "containsText",
        "notContains" => "notContainsText",
        "beginsWith" => "beginsWith",
        _ => "endsWith",
    }
}

#[allow(clippy::too_many_arguments)]
pub(super) fn groups_xml(
    groups: &[Group],
    exes: &[Ex],
    fx: &mut Fx,
    styles: &mut Styles,
    cp: u16,
    idx: u32,
    warnings: &mut Vec<String>,
    sheet: &str,
) -> String {
    let mut out = String::new();
    let mut next_priority = 1u16;
    for g in groups {
        if g.ranges.is_empty() {
            continue;
        }
        let base = (g.bound.0, g.bound.1);
        let mut rules = String::new();
        for raw in &g.rules12 {
            let plain = matches!(raw.get(12), Some(1 | 2 | 5));
            let r = if plain {
                rule12(raw, base, fx, styles, cp, idx, &mut next_priority)
            } else {
                super::cf12::read(raw, base, fx, cp, idx, sheet)
            };
            match r {
                Ok(x) => rules.push_str(&x),
                Err(m) => warnings.push(format!("{sheet}: conditional format not read: {m}")),
            }
        }
        for (i, raw) in g.rules.iter().enumerate() {
            let e = exes
                .iter()
                .find(|e| e.header == g.no && usize::from(e.index) == i);
            match rule(raw, e, base, fx, styles, cp, idx, &mut next_priority) {
                Ok(x) => rules.push_str(&x),
                Err(m) => warnings.push(format!("{sheet}: conditional format not read: {m}")),
            }
        }
        if rules.is_empty() {
            continue;
        }
        let sq: Vec<String> = g.ranges.iter().map(|&r| ref_str(r)).collect();
        let _ = write!(
            out,
            "<conditionalFormatting sqref=\"{}\">{rules}</conditionalFormatting>",
            sq.join(" ")
        );
    }
    out
}

#[allow(clippy::too_many_arguments)]
fn rule12(
    raw: &[u8],
    base: (u32, u32),
    fx: &mut Fx,
    styles: &mut Styles,
    cp: u16,
    idx: u32,
    next_priority: &mut u16,
) -> Result<String, String> {
    let mut c = Cur::new(raw.get(12..).ok_or("short CF12 record")?);
    let ct = c.u8()?;
    let op = c.u8()?;
    let n1 = c.u16()?;
    let n2 = c.u16()?;
    let cb = c.u32()? as usize;
    let dxf = c.take(cb)?;
    let f1 = c.take(usize::from(n1))?;
    let f2 = c.take(usize::from(n2))?;
    c.take(2)?;
    let stop = c.u8()?;
    let priority = c.u16()?;
    let template = c.u16()?;
    let n = usize::from(c.u8()?);
    let params = c.take(n)?;
    let mut e = Ex {
        header: 0,
        index: 0,
        template: template as u8,
        priority,
        flags: u16::from(stop),
        tail: [0; 17],
        colors: dxf_colors(dxf),
    };
    e.tail[0] = 0x10;
    let m = params.len().min(16);
    e.tail[1..=m].copy_from_slice(&params[..m]);
    if template == 5 {
        let p = c.rest();
        if let Some(q) = p.get(2..6) {
            e.tail[1] = q[1];
            e.tail[2] = q[2];
        }
    }
    let mut classic = vec![ct, op];
    classic.extend(n1.to_le_bytes());
    classic.extend(n2.to_le_bytes());
    let head = dxf.get(..6).ok_or("short DXFN")?;
    let flags = u32::from_le_bytes([head[0], head[1], head[2], head[3]]);
    let ignore = u16::from_le_bytes([head[4], head[5]]);
    let len = block_len(flags, ignore, &dxf[6..]);
    classic.extend(head);
    classic.extend(dxf.get(6..6 + len).ok_or("short DXFN")?);
    classic.extend(f1);
    classic.extend(f2);
    rule(&classic, Some(&e), base, fx, styles, cp, idx, next_priority)
}

#[allow(clippy::too_many_arguments)]
fn rule(
    raw: &[u8],
    e: Option<&Ex>,
    base: (u32, u32),
    fx: &mut Fx,
    styles: &mut Styles,
    cp: u16,
    idx: u32,
    next_priority: &mut u16,
) -> Result<String, String> {
    let mut c = Cur::new(raw);
    let ct = c.u8()?;
    let op = usize::from(c.u8()?);
    let n1 = usize::from(c.u16()?);
    let n2 = usize::from(c.u16()?);
    let flags = c.u32()?;
    let ignore = c.u16()?;
    let blocks = c.take(block_len(flags, ignore, c.rest()))?;
    let f1 = c.take(n1)?;
    let f2 = c.take(n2)?;
    let mut formula = |b: &[u8]| -> Result<String, String> {
        if b.is_empty() {
            return Ok(String::new());
        }
        match fx.text(b, &[], base, Some(idx), cp)? {
            Dec::Text(t) => Ok(t),
            Dec::Exp { .. } => Err("shared formula in a conditional format".into()),
        }
    };
    let t1 = formula(f1)?;
    let t2 = formula(f2)?;
    let colors = e.map_or(&[][..], |e| &e.colors[..]);
    let dxf_id = if flags & (NUMFMT | FONT | BORDER | AREA) != 0 {
        let x = dxf(flags, ignore, blocks, colors, styles);
        Some(match styles.dxfs.iter().position(|d| *d == x) {
            Some(i) => i,
            None => {
                styles.dxfs.push(x);
                styles.dxfs.len() - 1
            }
        })
    } else {
        None
    };
    let priority = e.map_or(*next_priority, |e| e.priority);
    *next_priority = priority.saturating_add(1);
    let stop = e.is_none_or(|e| e.flags & 2 != 0);
    let template = e.map_or(if ct == 1 { 0 } else { 1 }, |e| e.template);
    let tail = e.map_or([0u8; 17], |e| e.tail);
    let mut a = String::new();
    if let Some(i) = dxf_id {
        let _ = write!(a, " dxfId=\"{i}\"");
    }
    let _ = write!(a, " priority=\"{priority}\"");
    let stop_attr = if stop { " stopIfTrue=\"1\"" } else { "" };
    let body = |ty: &str, extra: &str, formulas: &[&String]| -> String {
        let mut x = format!("<cfRule type=\"{ty}\"{a}{extra}");
        let fs: Vec<&&String> = formulas.iter().filter(|f| !f.is_empty()).collect();
        if fs.is_empty() {
            x.push_str("/>");
        } else {
            x.push('>');
            for f in fs {
                let _ = write!(x, "<formula>{}</formula>", esc(f));
            }
            x.push_str("</cfRule>");
        }
        x
    };
    Ok(match template {
        0 => {
            let o = OPERATORS.get(op).copied().unwrap_or("");
            body(
                "cellIs",
                &format!("{stop_attr} operator=\"{o}\""),
                &[&t1, &t2],
            )
        }
        5 => {
            let mut extra = String::new();
            if tail[1] & 2 != 0 {
                extra.push_str(" percent=\"1\"");
            }
            if tail[1] & 1 == 0 {
                extra.push_str(" bottom=\"1\"");
            }
            let _ = write!(extra, " rank=\"{}\"", tail[2]);
            body("top10", &extra, &[])
        }
        7 => body("uniqueValues", "", &[]),
        27 => body("duplicateValues", "", &[]),
        25 | 26 | 29 | 30 => {
            let mut extra = stop_attr.to_string();
            if template == 26 || template == 30 {
                extra.push_str(" aboveAverage=\"0\"");
            }
            if template == 29 || template == 30 {
                extra.push_str(" equalAverage=\"1\"");
            }
            if tail[1] != 0 {
                let _ = write!(extra, " stdDev=\"{}\"", tail[1]);
            }
            body("aboveAverage", &extra, &[])
        }
        9..=12 => {
            let ty = [
                "containsBlanks",
                "notContainsBlanks",
                "containsErrors",
                "notContainsErrors",
            ][usize::from(template - 9)];
            body(ty, stop_attr, &[&t1])
        }
        15..=24 => {
            let p = PERIODS
                .iter()
                .find(|p| p.0 == template)
                .map_or("today", |p| p.1);
            body(
                "timePeriod",
                &format!("{stop_attr} timePeriod=\"{p}\""),
                &[&t1],
            )
        }
        8 => match search_text(&t1) {
            Some((o, text)) => body(
                type_of_search(o),
                &format!("{stop_attr} operator=\"{o}\" text=\"{}\"", attr(&text)),
                &[&t1],
            ),
            None => body("expression", stop_attr, &[&t1]),
        },
        _ => body("expression", stop_attr, &[&t1]),
    })
}
