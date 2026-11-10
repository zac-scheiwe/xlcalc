use crate::xlsb::{cell_name, decode, esc, Ctx, Dec, Rd};
use crate::xlsb_rec::Recs;
use crate::xlsb_styles::{color_bytes, color_xml, AUTO};
use crate::xml::Node;
use std::fmt::Write;

const COLORS: [&str; 8] = [
    "colorSeries",
    "colorNegative",
    "colorAxis",
    "colorMarkers",
    "colorFirst",
    "colorLast",
    "colorHigh",
    "colorLow",
];

const FLAGS: [(&str, u16); 8] = [
    ("markers", 3),
    ("high", 4),
    ("low", 5),
    ("first", 6),
    ("last", 7),
    ("negative", 8),
    ("displayXAxis", 9),
    ("displayHidden", 10),
];

fn on(n: &Node, k: &str) -> bool {
    matches!(n.attr(k), Some("1" | "true"))
}

fn num(n: &Node, k: &str, default: f64) -> f64 {
    n.attr(k).and_then(|v| v.parse().ok()).unwrap_or(default)
}

fn axis_bits(n: &Node, k: &str, individual: u16, group: u16) -> u16 {
    match n.attr(k) {
        Some("group") => 1 << group,
        Some("custom") => 0,
        _ => 1 << individual,
    }
}

fn guid_bytes(uid: &str) -> Option<[u8; 16]> {
    let h: String = uid.chars().filter(char::is_ascii_hexdigit).collect();
    if h.len() != 32 {
        return None;
    }
    let b: Vec<u8> = (0..16)
        .map(|i| u8::from_str_radix(&h[i * 2..i * 2 + 2], 16).unwrap_or(0))
        .collect();
    let mut g = [0u8; 16];
    g[..4].copy_from_slice(&[b[3], b[2], b[1], b[0]]);
    g[4..6].copy_from_slice(&[b[5], b[4]]);
    g[6..8].copy_from_slice(&[b[7], b[6]]);
    g[8..].copy_from_slice(&b[8..]);
    Some(g)
}

pub(crate) fn write(
    r: &mut Recs,
    groups: &Node,
    sheet: &str,
    formula: &mut dyn FnMut(&str) -> Result<Vec<u8>, String>,
    warnings: &mut Vec<String>,
) {
    r.with(35, |p| {
        p.u32(0x0E02);
    });
    r.empty(1058);
    for g in groups.children("sparklineGroup") {
        if let Some(uid) = g.attr("uid").and_then(guid_bytes) {
            r.rec(37, &[1, 0, 2, 0x10, 0, 0x80]);
            r.rec(3072, &uid);
            r.empty(38);
        }
        if g.attr("dateAxis").is_some() {
            warnings.push(format!("sheet {sheet}: sparkline date axis not carried"));
        }
        r.with(1041, |p| {
            p.u32(0);
            let mut flags: u16 = match g.attr("displayEmptyCellsAs") {
                Some("gap") => 1 << 1,
                Some("span") => 2 << 1,
                _ => 0,
            };
            for (k, bit) in FLAGS {
                if on(g, k) {
                    flags |= 1 << bit;
                }
            }
            flags |= axis_bits(g, "maxAxisType", 11, 13);
            flags |= axis_bits(g, "minAxisType", 12, 14);
            if on(g, "rightToLeft") {
                flags |= 1 << 15;
            }
            p.u16(flags);
            for c in COLORS {
                p.raw(&color_bytes(g.child(c), AUTO));
            }
            p.f64(num(g, "manualMax", 0.0))
                .f64(num(g, "manualMin", 0.0));
            p.f64(num(g, "lineWeight", 0.75));
            p.u32(match g.attr("type") {
                Some("column") => 1,
                Some("stacked") => 2,
                _ => 0,
            });
        });
        r.empty(1056);
        for s in g
            .child("sparklines")
            .iter()
            .flat_map(|s| s.children("sparkline"))
        {
            let f = s.child("f").map_or("", |f| f.text.trim());
            let loc = s
                .child("sqref")
                .and_then(|q| crate::parse_rect(&format!("{0}:{0}", q.text.trim())));
            let (Some(loc), Ok(rgce)) = (loc, formula(f)) else {
                warnings.push(format!("sheet {sheet}: sparkline {f} not carried"));
                continue;
            };
            r.with(1043, |p| {
                p.u32(6).u32(1).u32(2).u32(1);
                p.u32(loc.r0).u32(loc.r1).u32(loc.c0).u32(loc.c1);
                p.u32(1).u32(2);
                p.u32(rgce.len() as u32).u32(0).raw(&rgce);
            });
        }
        r.empty(1057);
        r.empty(1042);
    }
    r.empty(1059);
    r.empty(36);
}

#[derive(Default)]
pub(crate) struct Reader {
    xml: String,
    started: bool,
}

impl Reader {
    pub(crate) fn wants(id: u32) -> bool {
        matches!(id, 1041..=1043 | 1056..=1059)
    }

    pub(crate) fn rec(
        &mut self,
        id: u32,
        d: &[u8],
        ctx: &Ctx<'_>,
        warnings: &mut Vec<String>,
    ) -> Result<(), String> {
        let mut rd = Rd::new(d);
        let x = &mut self.xml;
        match id {
            1058 => {
                x.push_str("<extLst><ext uri=\"{05C60535-1F16-4fd2-B633-F4F36F0B64E0}\" xmlns:x14=\"http://schemas.microsoft.com/office/spreadsheetml/2009/9/main\"><x14:sparklineGroups xmlns:xm=\"http://schemas.microsoft.com/office/excel/2006/main\">");
                self.started = true;
            }
            1041 => {
                rd.u32()?;
                let flags = rd.u16()?;
                let mut colors = Vec::new();
                for _ in 0..8 {
                    let mut c = [0u8; 8];
                    c.copy_from_slice(rd.take(8)?);
                    colors.push(c);
                }
                let (max, min, weight) = (rd.f64()?, rd.f64()?, rd.f64()?);
                let ty = rd.u32()?;
                x.push_str("<x14:sparklineGroup");
                if flags & 0x2000 == 0 && flags & 0x0800 == 0 {
                    x.push_str(" maxAxisType=\"custom\"");
                    let _ = write!(x, " manualMax=\"{max}\"");
                } else if flags & 0x2000 != 0 {
                    x.push_str(" maxAxisType=\"group\"");
                }
                if flags & 0x4000 == 0 && flags & 0x1000 == 0 {
                    x.push_str(" minAxisType=\"custom\"");
                    let _ = write!(x, " manualMin=\"{min}\"");
                } else if flags & 0x4000 != 0 {
                    x.push_str(" minAxisType=\"group\"");
                }
                if (weight - 0.75).abs() > f64::EPSILON {
                    let _ = write!(x, " lineWeight=\"{weight}\"");
                }
                match ty {
                    1 => x.push_str(" type=\"column\""),
                    2 => x.push_str(" type=\"stacked\""),
                    _ => {}
                }
                match (flags >> 1) & 3 {
                    1 => x.push_str(" displayEmptyCellsAs=\"gap\""),
                    2 => x.push_str(" displayEmptyCellsAs=\"span\""),
                    _ => {}
                }
                for (k, bit) in FLAGS {
                    if flags & (1 << bit) != 0 {
                        let _ = write!(x, " {k}=\"1\"");
                    }
                }
                if flags & 0x8000 != 0 {
                    x.push_str(" rightToLeft=\"1\"");
                }
                x.push('>');
                for (name, c) in COLORS.iter().zip(colors) {
                    x.push_str(&color_xml(&format!("x14:{name}"), c));
                }
            }
            1056 => x.push_str("<x14:sparklines>"),
            1043 => {
                rd.take(16)?;
                let (r0, r1, c0, c1) = (rd.u32()?, rd.u32()?, rd.u32()?, rd.u32()?);
                rd.take(8)?;
                let cce = rd.u32()? as usize;
                rd.u32()?;
                let rgce = rd.take(cce)?;
                match decode(ctx, rgce, &[], (r0, c0)) {
                    Ok(Dec::Text(t)) if r0 == r1 && c0 == c1 => {
                        let _ = write!(
                            x,
                            "<x14:sparkline><xm:f>{}</xm:f><xm:sqref>{}</xm:sqref></x14:sparkline>",
                            esc(&t),
                            cell_name(r0, c0)
                        );
                    }
                    _ => warnings.push("sparkline source not supported".into()),
                }
            }
            1057 => x.push_str("</x14:sparklines>"),
            1042 => x.push_str("</x14:sparklineGroup>"),
            1059 => {
                x.push_str("</x14:sparklineGroups></ext></extLst>");
                self.started = false;
            }
            _ => {}
        }
        Ok(())
    }

    pub(crate) fn xml(&self) -> &str {
        &self.xml
    }
}
