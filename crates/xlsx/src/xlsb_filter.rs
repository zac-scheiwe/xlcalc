use crate::xlsb::{esc, rect_name, Rd};
use crate::xlsb_cfdv::ICON_SETS;
use crate::xlsb_rec::Recs;
use crate::xml::Node;
use std::fmt::Write;

const OPS: [&str; 7] = [
    "",
    "lessThan",
    "equal",
    "lessThanOrEqual",
    "greaterThan",
    "notEqual",
    "greaterThanOrEqual",
];

const CALENDARS: [&str; 6] = [
    "none",
    "gregorian",
    "gregorianUs",
    "japan",
    "taiwan",
    "korea",
];

const DYNAMIC: [(&str, u32); 34] = [
    ("aboveAverage", 1),
    ("belowAverage", 2),
    ("tomorrow", 8),
    ("today", 9),
    ("yesterday", 10),
    ("nextWeek", 11),
    ("thisWeek", 12),
    ("lastWeek", 13),
    ("nextMonth", 14),
    ("thisMonth", 15),
    ("lastMonth", 16),
    ("nextQuarter", 17),
    ("thisQuarter", 18),
    ("lastQuarter", 19),
    ("nextYear", 20),
    ("thisYear", 21),
    ("lastYear", 22),
    ("yearToDate", 23),
    ("Q1", 24),
    ("Q2", 25),
    ("Q3", 26),
    ("Q4", 27),
    ("M1", 28),
    ("M2", 29),
    ("M3", 30),
    ("M4", 31),
    ("M5", 32),
    ("M6", 33),
    ("M7", 34),
    ("M8", 35),
    ("M9", 36),
    ("M10", 37),
    ("M11", 38),
    ("M12", 39),
];

const GROUPS: [&str; 6] = ["year", "month", "day", "hour", "minute", "second"];

const RICH_URI: &str = "<extLst><ext uri=\"{1AD28BCE-077C-4C59-8B6E-1921CE8616D4}\" xmlns:xlrd2=\"http://schemas.microsoft.com/office/spreadsheetml/2017/richdata2\"><xlrd2:filterColumn>";

const X14_ICON: &str = "<mc:AlternateContent xmlns:mc=\"http://schemas.openxmlformats.org/markup-compatibility/2006\"><mc:Choice Requires=\"x14\"><x14:iconFilter";

fn icon_set_name(i: usize) -> Option<&'static str> {
    const X14: [&str; 3] = ["3Stars", "3Triangles", "5Boxes"];
    ICON_SETS
        .get(i)
        .or_else(|| X14.get(i.checked_sub(ICON_SETS.len())?))
        .copied()
}

fn icon_set_index(name: &str) -> Option<u32> {
    (0..20).find(|i| icon_set_name(*i as usize) == Some(name))
}

fn weekday(y: i32, m: i32, d: i32) -> u16 {
    const T: [i32; 12] = [0, 3, 2, 5, 0, 3, 5, 1, 4, 6, 2, 4];
    let y = if m < 3 { y - 1 } else { y };
    ((y + y / 4 - y / 100 + y / 400 + T[(m.clamp(1, 12) - 1) as usize] + d).rem_euclid(7) + 1)
        as u16
}

const FRT: [u8; 4] = [3, 0, 2, 0];

fn date_group(r: &mut Recs, d: &Node) {
    let g = |k: &str, def: i32| d.attr(k).and_then(|v| v.parse::<i32>().ok()).unwrap_or(def);
    let (y, mo, da) = (g("year", 1900), g("month", 1), g("day", 1));
    let grouping = d
        .attr("dateTimeGrouping")
        .and_then(|n| GROUPS.iter().position(|x| *x == n))
        .unwrap_or(0) as u32;
    r.with(5094, |p| {
        p.u32(0).u16(y as u16).u16(mo as u16).u16(da as u16).u16(0);
        p.u16(g("hour", 0) as u16)
            .u16(g("minute", 0) as u16)
            .u16(g("second", 0) as u16);
        p.u16(weekday(y, mo, da)).u32(0).u32(grouping).u32(u32::MAX);
    });
}

fn rich_filters(r: &mut Recs, f: &Node) {
    r.with(35, |p| {
        p.raw(&FRT);
    });
    r.empty(5084);
    r.with(35, |p| {
        p.raw(&FRT);
    });
    r.with(5081, |p| {
        p.u32(u32::from(on(f, "blank", false)));
        let cal = f
            .attr("calendarType")
            .and_then(|c| CALENDARS.iter().position(|x| *x == c));
        p.u32(cal.unwrap_or(0) as u32).u32(0);
    });
    for v in f.children("filter") {
        r.with(5083, |p| {
            p.u32(0)
                .u8(0)
                .u32(u32::MAX)
                .wstr(v.attr("val").unwrap_or(""));
        });
    }
    for d in f.children("dateGroupItem") {
        date_group(r, d);
    }
    r.empty(5082);
    r.empty(36);
    r.empty(5085);
    r.empty(36);
}
fn on(n: &Node, k: &str, default: bool) -> bool {
    n.attr(k).map_or(default, |v| matches!(v, "1" | "true"))
}

fn rect(n: &Node, k: &str) -> Option<crate::Rect> {
    n.attr(k).and_then(crate::parse_rect)
}

fn num(n: &Node, k: &str) -> f64 {
    n.attr(k).and_then(|v| v.parse().ok()).unwrap_or(0.0)
}

fn sort_state(r: &mut Recs, s: &Node, sheet: &str, warnings: &mut Vec<String>) {
    let Some(rc) = rect(s, "ref") else { return };
    r.with(530, |p| {
        let flags = u16::from(on(s, "columnSort", false))
            | u16::from(on(s, "caseSensitive", false)) << 1
            | u16::from(s.attr("sortMethod") == Some("stroke")) << 2;
        p.u16(flags);
        p.u32(rc.r0).u32(rc.r1).u32(rc.c0).u32(rc.c1);
    });
    for c in s.children("sortCondition") {
        let (Some(cr), None | Some("value")) = (rect(c, "ref"), c.attr("sortBy")) else {
            warnings.push(format!("sheet {sheet}: sort by colour or icon not carried"));
            continue;
        };
        r.with(532, |p| {
            p.u16(u16::from(on(c, "descending", false)));
            p.u32(cr.r0).u32(cr.r1).u32(cr.c0).u32(cr.c1);
            p.u32(0).u32(0).null_str();
        });
        r.empty(533);
    }
    r.empty(531);
}

pub(crate) fn write(r: &mut Recs, af: &Node, sheet: &str, warnings: &mut Vec<String>) {
    let Some(rc) = rect(af, "ref") else { return };
    r.with(161, |p| {
        p.u32(rc.r0).u32(rc.r1).u32(rc.c0).u32(rc.c1);
    });
    for col in af.children("filterColumn") {
        let id: u32 = col.attr("colId").and_then(|v| v.parse().ok()).unwrap_or(0);
        r.with(163, |p| {
            let hide = u16::from(on(col, "hiddenButton", false));
            let no_btn = u16::from(!on(col, "showButton", true));
            p.u32(id).u16(hide | no_btn << 1);
        });
        if let Some(f) = col
            .child("filters")
            .filter(|f| f.children("dateGroupItem").next().is_none())
        {
            r.with(165, |p| {
                p.u32(u32::from(on(f, "blank", false)));
                let cal = f
                    .attr("calendarType")
                    .and_then(|c| CALENDARS.iter().position(|x| *x == c))
                    .unwrap_or(0);
                p.u32(cal as u32);
            });
            for v in f.children("filter") {
                r.with(167, |p| {
                    p.wstr(v.attr("val").unwrap_or(""));
                });
            }
            r.empty(166);
        } else if let Some(f) = col.child("filters") {
            rich_filters(r, f);
        } else if let Some(f) = col.child("customFilters") {
            r.with(172, |p| {
                p.u32(u32::from(on(f, "and", false)));
            });
            for c in f.children("customFilter") {
                let op = c
                    .attr("operator")
                    .map_or(2, |o| OPS.iter().position(|x| *x == o).unwrap_or(2) as u8);
                let val = c.attr("val").unwrap_or("");
                r.with(174, |p| match val.parse::<f64>() {
                    Ok(x) if !val.trim().is_empty() => {
                        p.u8(4).u8(op).f64(x);
                    }
                    _ => {
                        p.u8(6).u8(op).raw(&[0; 8]).wstr(val);
                    }
                });
            }
            r.empty(173);
        } else if let Some(t) = col.child("top10") {
            r.with(170, |p| {
                let flags = u8::from(on(t, "top", true)) | u8::from(on(t, "percent", false)) << 1;
                p.u8(flags).f64(num(t, "val")).f64(num(t, "filterVal"));
            });
        } else if let Some(d) = col.child("dynamicFilter") {
            let ty = d.attr("type").unwrap_or("null");
            let code = DYNAMIC.iter().find(|x| x.0 == ty).map_or(0, |x| x.1);
            r.with(171, |p| {
                p.u32(code).u8(u8::from(d.attr("val").is_some()));
                p.f64(num(d, "val")).f64(num(d, "maxVal"));
            });
        } else if let Some(c) = col.child("colorFilter") {
            let id: u32 = c.attr("dxfId").and_then(|v| v.parse().ok()).unwrap_or(0);
            r.with(168, |p| {
                p.u32(id).u32(u32::from(on(c, "cellColor", true)));
            });
        } else if let Some(i) = col.child("iconFilter").or_else(|| {
            col.child("AlternateContent")?
                .child("Choice")?
                .child("iconFilter")
        }) {
            let set = i.attr("iconSet").and_then(icon_set_index);
            let id: u32 = i.attr("iconId").and_then(|v| v.parse().ok()).unwrap_or(0);
            match set {
                Some(s) if (s as usize) < ICON_SETS.len() => {
                    r.with(169, |p| {
                        p.u32(s).u32(id);
                    });
                }
                Some(s) => {
                    r.with(37, |p| {
                        p.raw(&[1, 0, 2, 0x0e, 0, 0x80]);
                    });
                    r.with(1181, |p| {
                        p.u32(s).u32(id);
                    });
                    r.empty(38);
                }
                None => warnings.push(format!("sheet {sheet}: icon filter set not carried")),
            }
        } else if let Some(f) = col
            .child("extLst")
            .and_then(|e| e.child("ext"))
            .and_then(|e| e.child("filterColumn"))
            .and_then(|e| e.child("filters"))
        {
            rich_filters(r, f);
        }
        r.empty(164);
    }
    if let Some(s) = af.child("sortState") {
        sort_state(r, s, sheet, warnings);
    }
    r.empty(162);
}

#[derive(Default)]
pub(crate) struct Reader {
    xml: String,
    in_col: bool,
    x14: bool,
}

impl Reader {
    pub(crate) fn wants(id: u32) -> bool {
        matches!(id, 161..=174 | 530..=533 | 1181 | 5081..=5085 | 5094)
    }

    pub(crate) fn rec(&mut self, id: u32, d: &[u8]) -> Result<(), String> {
        let mut rd = Rd::new(d);
        let x = &mut self.xml;
        match id {
            161 => {
                let (r0, r1, c0, c1) = (rd.u32()?, rd.u32()?, rd.u32()?, rd.u32()?);
                let _ = write!(x, "<autoFilter ref=\"{}\">", rect_name(r0, c0, r1, c1));
            }
            162 => {
                if self.x14 {
                    if let Some(at) = x.find('>') {
                        x.insert_str(at, " xmlns:x14=\"http://schemas.microsoft.com/office/spreadsheetml/2009/9/main\"");
                    }
                }
                x.push_str("</autoFilter>");
            }
            169 | 1181 | 5081..=5085 | 5094 if !self.in_col => {}
            163 => {
                let col = rd.u32()?;
                let flags = rd.u16()?;
                self.in_col = true;
                let _ = write!(x, "<filterColumn colId=\"{col}\"");
                if flags & 1 != 0 {
                    x.push_str(" hiddenButton=\"1\"");
                }
                if flags & 2 != 0 {
                    x.push_str(" showButton=\"0\"");
                }
                x.push('>');
            }
            164 => {
                self.in_col = false;
                x.push_str("</filterColumn>");
            }
            169 | 1181 => {
                let (set, ic) = (rd.u32()? as usize, rd.u32()?);
                let name = icon_set_name(set).unwrap_or("3Arrows");
                if id == 1181 {
                    self.x14 = true;
                    x.push_str(X14_ICON);
                    let _ = write!(
                        x,
                        " iconSet=\"{name}\" iconId=\"{ic}\"/></mc:Choice></mc:AlternateContent>"
                    );
                } else {
                    let _ = write!(x, "<iconFilter iconSet=\"{name}\" iconId=\"{ic}\"/>");
                }
            }
            5084 => x.push_str(RICH_URI),
            5085 => x.push_str("</xlrd2:filterColumn></ext></extLst>"),
            5081 => {
                let blank = rd.u32()? != 0;
                let cal = rd.u32()? as usize;
                x.push_str("<xlrd2:filters");
                if blank {
                    x.push_str(" blank=\"1\"");
                }
                if let Some(c) = CALENDARS.get(cal).filter(|_| cal != 0) {
                    let _ = write!(x, " calendarType=\"{c}\"");
                }
                x.push('>');
            }
            5082 => x.push_str("</xlrd2:filters>"),
            5083 => {
                rd.take(9)?;
                let _ = write!(x, "<xlrd2:filter val=\"{}\"/>", esc(&rd.wstr()?));
            }
            5094 => {
                rd.u32()?;
                let (y, mo, da) = (rd.u16()?, rd.u16()?, rd.u16()?);
                rd.u16()?;
                let (h, mi, s) = (rd.u16()?, rd.u16()?, rd.u16()?);
                rd.take(6)?;
                let g = rd.u32()? as usize;
                let name = GROUPS.get(g).copied().unwrap_or("year");
                let _ = write!(x, "<xlrd2:dateGroupItem year=\"{y}\"");
                for (k, v) in [
                    ("month", mo),
                    ("day", da),
                    ("hour", h),
                    ("minute", mi),
                    ("second", s),
                ] {
                    if g >= GROUPS.iter().position(|n| *n == k).unwrap_or(9) {
                        let _ = write!(x, " {k}=\"{v}\"");
                    }
                }
                let _ = write!(x, " dateTimeGrouping=\"{name}\"/>");
            }
            165 => {
                let blank = rd.u32()? != 0;
                let cal = rd.u32()? as usize;
                x.push_str("<filters");
                if blank {
                    x.push_str(" blank=\"1\"");
                }
                if let Some(c) = CALENDARS.get(cal).filter(|_| cal != 0) {
                    let _ = write!(x, " calendarType=\"{c}\"");
                }
                x.push('>');
            }
            166 => x.push_str("</filters>"),
            167 => {
                let _ = write!(x, "<filter val=\"{}\"/>", esc(&rd.wstr()?));
            }
            172 => {
                let and = rd.u32()? != 0;
                x.push_str(if and {
                    "<customFilters and=\"1\">"
                } else {
                    "<customFilters>"
                });
            }
            173 => x.push_str("</customFilters>"),
            174 => {
                let (vt, op) = (rd.u8()?, usize::from(rd.u8()?));
                let val = if vt == 6 {
                    rd.take(8)?;
                    rd.wstr()?
                } else {
                    rd.f64()?.to_string()
                };
                x.push_str("<customFilter");
                if let Some(o) = OPS.get(op).filter(|o| !o.is_empty() && **o != "equal") {
                    let _ = write!(x, " operator=\"{o}\"");
                }
                let _ = write!(x, " val=\"{}\"/>", esc(&val));
            }
            170 => {
                let flags = rd.u8()?;
                let (val, fv) = (rd.f64()?, rd.f64()?);
                x.push_str("<top10");
                if flags & 1 == 0 {
                    x.push_str(" top=\"0\"");
                }
                if flags & 2 != 0 {
                    x.push_str(" percent=\"1\"");
                }
                let _ = write!(x, " val=\"{val}\" filterVal=\"{fv}\"/>");
            }
            168 => {
                let id = rd.u32()?;
                let cell = rd.u32()? != 0;
                let _ = write!(x, "<colorFilter dxfId=\"{id}\"");
                if !cell {
                    x.push_str(" cellColor=\"0\"");
                }
                x.push_str("/>");
            }
            171 => {
                let code = rd.u32()?;
                let has = rd.u8()? != 0;
                let (val, max) = (rd.f64()?, rd.f64()?);
                let name = DYNAMIC.iter().find(|y| y.1 == code).map_or("null", |y| y.0);
                let _ = write!(x, "<dynamicFilter type=\"{name}\"");
                if has {
                    let _ = write!(x, " val=\"{val}\"");
                    if max != 0.0 {
                        let _ = write!(x, " maxVal=\"{max}\"");
                    }
                }
                x.push_str("/>");
            }
            530 => {
                let flags = rd.u16()?;
                let (r0, r1, c0, c1) = (rd.u32()?, rd.u32()?, rd.u32()?, rd.u32()?);
                let _ = write!(x, "<sortState ref=\"{}\"", rect_name(r0, c0, r1, c1));
                if flags & 1 != 0 {
                    x.push_str(" columnSort=\"1\"");
                }
                if flags & 2 != 0 {
                    x.push_str(" caseSensitive=\"1\"");
                }
                if flags & 4 != 0 {
                    x.push_str(" sortMethod=\"stroke\"");
                }
                x.push('>');
            }
            532 => {
                let flags = rd.u16()?;
                let (r0, r1, c0, c1) = (rd.u32()?, rd.u32()?, rd.u32()?, rd.u32()?);
                x.push_str("<sortCondition");
                if flags & 1 != 0 {
                    x.push_str(" descending=\"1\"");
                }
                let _ = write!(x, " ref=\"{}\"/>", rect_name(r0, c0, r1, c1));
            }
            531 => x.push_str("</sortState>"),
            _ => {}
        }
        Ok(())
    }

    pub(crate) fn xml(&self) -> &str {
        &self.xml
    }
}
