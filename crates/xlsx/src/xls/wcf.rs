use super::rcf::{dxf_format_id, BORDERS, PATTERNS, PERIODS};
use super::wextras::{rect8, rgce};
use super::write::Out;
use super::wsheet::Cx;
use crate::parse_rect;
use crate::xlsb_enc::Cls;
use crate::xml::Node;
use xlcalc::Rect;

const FONT: u32 = 0x0400_0000;
const BORDER: u32 = 0x1000_0000;
const AREA: u32 = 0x2000_0000;
const NUMFMT: u32 = 0x0200_0000;

fn flag(n: &Node, k: &str) -> bool {
    n.attr(k).is_none_or(|v| !matches!(v, "0" | "false"))
}

fn cell(r: u32, c: u32) -> String {
    let mut n = c;
    let mut name = String::new();
    loop {
        name.insert(0, char::from(b'A' + (n % 26) as u8));
        if n < 26 {
            break;
        }
        n = n / 26 - 1;
    }
    format!("{name}{}", r + 1)
}

fn abs_cell(r: u32, c: u32) -> String {
    let s = cell(r, c);
    let at = s.find(|ch: char| ch.is_ascii_digit()).unwrap_or(0);
    format!("${}${}", &s[..at], &s[at..])
}

struct Format {
    flags: u32,

    ignore: u16,
    blocks: Vec<u8>,
    colors: Vec<(u16, u32)>,
}

fn format_of(dxf: &Node, cx: &Cx<'_>) -> Format {
    let pal = &cx.styles.palette;
    let mut flags = 0x8000_0000u32 | 0xFFFF | 0x003F_0000;
    let mut blocks = Vec::new();
    let mut colors: Vec<(u16, u32)> = Vec::new();
    let mut add_color = |ty: u16, n: &Node| {
        if let Some(rgb) = pal.rgb(n) {
            colors.push((ty, rgb));
        }
    };
    if let Some(f) = dxf.child("font") {
        flags |= FONT;
        let mut b = vec![0u8; 118];
        let mut put = |at: usize, v: &[u8]| b[at..at + v.len()].copy_from_slice(v);
        put(64, &0xFFFF_FFFFu32.to_le_bytes());
        put(72, &0xFFFFu16.to_le_bytes());
        put(74, &0xFFFFu16.to_le_bytes());
        put(76, &[0xFF]);
        put(80, &0xFFFF_FFFFu32.to_le_bytes());
        let (mut f1, mut f3, mut style) = (0xFFFF_FFFFu32, 1u32, 0u32);
        let mut weight = 0xFFFFu16;
        let mut one_zero = 1u32;
        if let Some(n) = f.child("b") {
            f1 &= !1;
            weight = if flag(n, "val") { 700 } else { 400 };
            one_zero = 0;
        }
        if let Some(n) = f.child("i") {
            f1 &= !2;
            if flag(n, "val") {
                style |= 2;
            }
            one_zero = 0;
        }
        if let Some(n) = f.child("strike") {
            f1 &= !0x80;
            if flag(n, "val") {
                style |= 0x80;
            }
        }
        let mut ul = 0xFFu8;
        if let Some(n) = f.child("u") {
            f1 &= !4;
            f3 = 0;
            ul = match n.attr("val") {
                None | Some("single") => 1,
                Some("double") => 2,
                Some("singleAccounting") => 0x21,
                Some("doubleAccounting") => 0x22,
                _ => 0,
            };
        }
        let mut icv = 0xFFFF_FFFFu32;
        if let Some(n) = f.child("color") {
            let i = pal.icv(Some(n), 0xFFFF);
            if i != 0xFFFF {
                icv = u32::from(i);
            }
            add_color(0x0D, n);
        }
        put(68, &style.to_le_bytes());
        put(72, &weight.to_le_bytes());
        put(76, &[ul]);
        put(77, &[0, 0, 0]);
        put(80, &icv.to_le_bytes());
        put(88, &f1.to_le_bytes());
        put(92, &1u32.to_le_bytes());
        put(96, &f3.to_le_bytes());
        put(100, &one_zero.to_le_bytes());
        put(104, &1u32.to_le_bytes());
        put(116, &[1, 0]);
        blocks.extend(b);
    }
    if let Some(bd) = dxf.child("border") {
        let mut styles = 0u16;
        let mut colours = 0u32;
        let mut any = false;
        for (name, ss, cs, bit, prop) in [
            ("left", 0, 0, 10, 9u16),
            ("right", 4, 7, 11, 10),
            ("top", 8, 16, 12, 7),
            ("bottom", 12, 23, 13, 8),
        ] {
            let Some(side) = bd.child(name) else { continue };
            any = true;
            flags &= !(1 << bit);
            let st = side.attr("style").unwrap_or("none");
            let code = BORDERS.iter().position(|b| *b == st).unwrap_or(0) as u16;
            styles |= code << ss;
            if code != 0 {
                if let Some(c) = side.child("color") {
                    let i = pal.icv(Some(c), 0x40);
                    colours |= (u32::from(i) & 0x7F) << cs;
                    add_color(prop, c);
                } else {
                    colours |= 0x40 << cs;
                }
            }
        }
        if any {
            flags |= BORDER;
            blocks.extend(styles.to_le_bytes());
            blocks.extend(colours.to_le_bytes());
            blocks.extend(0u16.to_le_bytes());
        }
    }
    if let Some(pf) = dxf.child("fill").and_then(|f| f.child("patternFill")) {
        flags |= AREA;
        let ptype = pf.attr("patternType");
        let mut pat = 0u16;
        if let Some(t) = ptype {
            flags &= !(1 << 16);
            pat = PATTERNS.iter().position(|p| *p == t).unwrap_or(0) as u16;
            if pat == 1 {
                pat = 0;
            }
        }
        let mut fg = 0x40u16;
        let mut bg = 0x41u16;
        if let Some(c) = pf.child("fgColor") {
            flags &= !(1 << 17);
            fg = pal.icv(Some(c), 0x40) & 0x7F;
            add_color(4, c);
        }
        if let Some(c) = pf.child("bgColor") {
            flags &= !(1 << 18);
            bg = pal.icv(Some(c), 0x41) & 0x7F;
            add_color(5, c);
        }
        blocks.extend((pat << 10).to_le_bytes());
        blocks.extend((fg | bg << 7).to_le_bytes());
    }
    colors.sort_unstable();
    let mut ignore = 0x8002u16;
    if let Some(nf) = dxf.child("numFmt") {
        let code = nf.attr("formatCode").unwrap_or("General");
        let mut b = Vec::new();
        match dxf_format_id(code) {
            Some(id) => b.extend([0, id]),
            None => {
                ignore |= 1;
                let wide = !code.is_ascii();
                let units: Vec<u16> = code.encode_utf16().collect();
                let len = 5 + if wide { 2 * units.len() } else { units.len() };
                b.extend((len as u16).to_le_bytes());
                b.extend((units.len() as u16).to_le_bytes());
                b.push(u8::from(wide));
                for u in units {
                    if wide {
                        b.extend(u.to_le_bytes());
                    } else {
                        b.push(u as u8);
                    }
                }
            }
        }
        b.extend(&blocks);
        blocks = b;
        flags |= NUMFMT;
        flags &= !(1 << 19);
    }
    Format {
        ignore,
        flags,
        blocks,
        colors,
    }
}

struct Classic {
    ct: u8,
    cp: u8,
    f1: String,
    f2: String,
    template: u8,

    tail: [u8; 2],
}

fn classic(rule: &Node, range: &Rect, bound: &Rect) -> Result<Classic, String> {
    let ty = rule.attr("type").unwrap_or("");
    let formulas: Vec<String> = rule
        .children("formula")
        .map(|f| f.text.trim().trim_start_matches('=').to_string())
        .collect();
    let f = |i: usize| formulas.get(i).cloned().unwrap_or_default();
    let here = cell(bound.r0, bound.c0);
    let area = format!(
        "{}:{}",
        abs_cell(range.r0, range.c0),
        abs_cell(range.r1, range.c1)
    );
    let mk = |ct, cp, f1: String, f2: String, template, tail: [u8; 2]| Classic {
        ct,
        cp,
        f1,
        f2,
        template,
        tail,
    };
    Ok(match ty {
        "cellIs" => {
            let op = match rule.attr("operator").unwrap_or("") {
                "between" => 1,
                "notBetween" => 2,
                "equal" => 3,
                "notEqual" => 4,
                "greaterThan" => 5,
                "lessThan" => 6,
                "greaterThanOrEqual" => 7,
                "lessThanOrEqual" => 8,
                o => return Err(format!("cellIs operator {o}")),
            };
            mk(1, op, f(0), f(1), 0, [0, 0])
        }
        "expression" => mk(2, 0, f(0), String::new(), 1, [0, 0]),
        "containsText" | "notContainsText" | "beginsWith" | "endsWith" => {
            let kind = match ty {
                "containsText" => 0,
                "notContainsText" => 1,
                "beginsWith" => 2,
                _ => 3,
            };
            mk(2, 0, f(0), String::new(), 8, [kind, 0])
        }
        "containsBlanks" => mk(2, 0, f(0), String::new(), 9, [0, 0]),
        "notContainsBlanks" => mk(2, 0, f(0), String::new(), 10, [0, 0]),
        "containsErrors" => mk(2, 0, f(0), String::new(), 11, [0, 0]),
        "notContainsErrors" => mk(2, 0, f(0), String::new(), 12, [0, 0]),
        "timePeriod" => {
            let p = rule.attr("timePeriod").unwrap_or("today");
            let t = PERIODS
                .iter()
                .find(|x| x.1 == p)
                .map(|x| x.0)
                .ok_or_else(|| format!("timePeriod {p}"))?;
            mk(2, 0, f(0), String::new(), t, [0, 0])
        }
        "top10" => {
            let rank: u32 = rule.attr("rank").and_then(|r| r.parse().ok()).unwrap_or(10);
            let bottom = !flag_off(rule, "bottom");
            let percent = !flag_off(rule, "percent");
            let order = if bottom { ",1" } else { "" };
            let limit = if percent {
                format!("COUNT({area})*{rank}/100")
            } else {
                rank.to_string()
            };
            let tail = [
                u8::from(!bottom) | (u8::from(percent) << 1),
                rank.min(255) as u8,
            ];
            mk(
                2,
                0,
                format!("RANK({here},{area}{order})<={limit}"),
                String::new(),
                5,
                tail,
            )
        }
        "uniqueValues" => mk(
            2,
            0,
            format!("COUNTIF({area},{here})=1"),
            String::new(),
            7,
            [0, 0],
        ),
        "duplicateValues" => mk(
            2,
            0,
            format!("COUNTIF({area},{here})>1"),
            String::new(),
            27,
            [0, 0],
        ),
        "aboveAverage" => {
            let below = rule.attr("aboveAverage") == Some("0");
            let equal = rule.attr("equalAverage") == Some("1");
            let sd: u8 = rule
                .attr("stdDev")
                .and_then(|v| v.parse().ok())
                .unwrap_or(0);
            let op = match (below, equal) {
                (false, false) => ">",
                (false, true) => ">=",
                (true, false) => "<",
                (true, true) => "<=",
            };
            let spread = if sd == 0 {
                String::new()
            } else {
                format!("{}{sd}*STDEV({area})", if below { "-" } else { "+" })
            };
            let template = match (below, equal) {
                (false, false) => 25,
                (true, false) => 26,
                (false, true) => 29,
                (true, true) => 30,
            };
            mk(
                2,
                0,
                format!("{here}{op}AVERAGE({area}){spread}"),
                String::new(),
                template,
                [sd, 0],
            )
        }
        t => return Err(format!("rule type {t}")),
    })
}

fn flag_off(n: &Node, k: &str) -> bool {
    !matches!(n.attr(k), Some("1" | "true"))
}

fn xl_ref8(d: &mut Vec<u8>, r: &Rect) {
    rect8(d, r.r0, r.r1, r.c0, r.c1);
}

pub(super) fn write(o: &mut Out, cx: &mut Cx<'_>, root: &Node, idx: u32, ordinal: &mut u16) {
    let mut exes: Vec<Vec<u8>> = Vec::new();
    for cf in root.children("conditionalFormatting") {
        let ranges: Vec<Rect> = cf
            .attr("sqref")
            .unwrap_or("")
            .split_whitespace()
            .filter_map(parse_rect)
            .collect();
        let Some(first) = ranges.first() else {
            continue;
        };
        let mut bound = *first;
        for r in &ranges {
            bound.r0 = bound.r0.min(r.r0);
            bound.c0 = bound.c0.min(r.c0);
            bound.r1 = bound.r1.max(r.r1);
            bound.c1 = bound.c1.max(r.c1);
        }
        let mut recs: Vec<(Vec<u8>, Vec<u8>)> = Vec::new();
        let mut recs12: Vec<Vec<u8>> = Vec::new();
        for rule in cf.children("cfRule") {
            let ty = rule.attr("type").unwrap_or("");
            if matches!(ty, "colorScale" | "dataBar" | "iconSet") {
                match super::cf12::write(cx, idx, rule, (bound.r0, bound.c0)) {
                    Ok(d) => recs12.push(d),
                    Err(e) => cx.warnings.push(format!(
                        "{ty} conditional format {}: {e}",
                        cf.attr("sqref").unwrap_or("")
                    )),
                }
                continue;
            }
            let cl = match classic(rule, &bound_or(first, &ranges), &bound) {
                Ok(c) => c,
                Err(e) => {
                    cx.warnings.push(format!(
                        "conditional format {}: {e}",
                        cf.attr("sqref").unwrap_or("")
                    ));
                    continue;
                }
            };
            let base = (bound.r0, bound.c0);
            let mut tokens = |text: &str| -> Result<Vec<u8>, String> {
                if text.is_empty() {
                    return Ok(Vec::new());
                }
                rgce(cx, idx, text, base, Cls::Value)
            };
            let (t1, t2) = match (tokens(&cl.f1), tokens(&cl.f2)) {
                (Ok(a), Ok(b)) => (a, b),
                (Err(e), _) | (_, Err(e)) => {
                    cx.warnings.push(format!(
                        "conditional format {}: {e}",
                        cf.attr("sqref").unwrap_or("")
                    ));
                    continue;
                }
            };
            let fmt = rule
                .attr("dxfId")
                .and_then(|i| i.parse::<usize>().ok())
                .and_then(|i| cx.dxfs.get(i))
                .map(|d| format_of(d, cx));
            let (flags, blocks) = match &fmt {
                Some(f) => (f.flags, f.blocks.clone()),
                None => (0x8000_0000 | 0x003F_FFFF, Vec::new()),
            };
            let numfmt = flags & NUMFMT != 0;
            let ignore = fmt.as_ref().map_or(0x8002, |f| f.ignore);
            let mut d = vec![cl.ct, cl.cp];
            d.extend((t1.len() as u16).to_le_bytes());
            d.extend((t2.len() as u16).to_le_bytes());
            d.extend(flags.to_le_bytes());
            d.extend(0x8002u16.to_le_bytes());
            d.extend(&blocks);
            let dxf_bytes: Vec<u8> = {
                let mut x = Vec::new();
                x.extend(flags.to_le_bytes());
                x.extend(ignore.to_le_bytes());
                x.extend(&blocks);

                x.extend(0u16.to_le_bytes());
                x.extend(0xFFFFu16.to_le_bytes());
                x.extend(0u16.to_le_bytes());
                let colors = fmt.as_ref().map_or(&[][..], |f| &f.colors[..]);
                x.extend((colors.len() as u16).to_le_bytes());
                for &(ty, rgb) in colors {
                    x.extend(ty.to_le_bytes());
                    x.extend(20u16.to_le_bytes());
                    x.extend(2u16.to_le_bytes());
                    x.extend(0u16.to_le_bytes());
                    x.extend([(rgb >> 16) as u8, (rgb >> 8) as u8, rgb as u8, 0xFF]);
                    x.extend([1, 0, 0, 0, 0, 0, 0, 0]);
                }
                x
            };
            d.extend(&t1);
            d.extend(&t2);
            if numfmt {
                let top10 = cl.template == 5;
                let none: &[u8] = &[];
                let (f1, f2) = if top10 {
                    (none, none)
                } else {
                    (&t1[..], &t2[..])
                };
                let mut r = vec![0x7A, 0x08, 0, 0];
                r.extend([0u8; 8]);
                r.push(if top10 { 5 } else { cl.ct });
                r.push(if top10 { 0 } else { cl.cp });
                r.extend((f1.len() as u16).to_le_bytes());
                r.extend((f2.len() as u16).to_le_bytes());
                let has_colors = fmt.as_ref().is_some_and(|f| !f.colors.is_empty());
                let keep = if has_colors {
                    dxf_bytes.len()
                } else {
                    dxf_bytes.len() - 8
                };
                r.extend((keep as u32).to_le_bytes());
                r.extend(&dxf_bytes[..keep]);
                r.extend(f1);
                r.extend(f2);
                let priority: u16 = rule
                    .attr("priority")
                    .and_then(|p| p.parse().ok())
                    .unwrap_or(1);
                let stop = rule.attr("stopIfTrue") == Some("1");
                r.extend([0, 0, 1 | if stop { 2 } else { 0 }]);
                r.extend(priority.to_le_bytes());
                r.extend(u16::from(cl.template).to_le_bytes());
                r.push(0x10);
                let mut params = [0u8; 16];
                if !top10 {
                    params[..2].copy_from_slice(&cl.tail);
                }
                r.extend(params);
                if top10 {
                    let rank: u16 = rule.attr("rank").and_then(|v| v.parse().ok()).unwrap_or(10);
                    r.extend([4, 0, 0, cl.tail[0]]);
                    r.extend(rank.to_le_bytes());
                }
                recs12.push(r);
                continue;
            }

            let mut e = vec![0x7B, 0x08, 0x01, 0x00];
            xl_ref8(&mut e, &bound);
            e.extend(0u32.to_le_bytes());
            e.extend((*ordinal + 1).to_le_bytes());
            e.extend((recs.len() as u16).to_le_bytes());
            e.push(if cl.ct == 1 { cl.cp } else { 0 });
            e.push(cl.template);
            let priority: u16 = rule
                .attr("priority")
                .and_then(|p| p.parse().ok())
                .unwrap_or(1);
            e.extend(priority.to_le_bytes());
            let stop = rule.attr("stopIfTrue") == Some("1");
            let mut fl = 1u16 | if stop { 2 } else { 0 };
            if fmt.is_some() {
                fl |= 0x100;
            }
            e.extend(fl.to_le_bytes());
            if fmt.is_some() {
                e.extend((dxf_bytes.len() as u32).to_le_bytes());
                e.extend(&dxf_bytes);
            }
            let mut tail = [0u8; 17];
            tail[0] = 0x10;
            tail[1] = cl.tail[0];
            tail[2] = cl.tail[1];
            e.extend(tail);
            recs.push((d, e));
        }
        let header = |n: usize, ordinal: u16, frt: bool| {
            let mut h = Vec::new();
            if frt {
                h.extend([0x79, 0x08, 0x01, 0x00]);
                xl_ref8(&mut h, &bound);
            }
            h.extend((n as u16).to_le_bytes());
            h.extend(((ordinal << 1) | 1).to_le_bytes());
            xl_ref8(&mut h, &bound);
            h.extend((ranges.len() as u16).to_le_bytes());
            for r in &ranges {
                xl_ref8(&mut h, r);
            }
            h
        };
        if !recs.is_empty() {
            *ordinal += 1;
            o.rec(0x1B0, &header(recs.len(), *ordinal, false));
            for (d, e) in recs {
                o.rec(0x1B1, &d);
                exes.push(e);
            }
        }
        if !recs12.is_empty() {
            *ordinal += 1;
            o.rec(0x879, &header(recs12.len(), *ordinal, true));
            for d in recs12 {
                o.rec(0x87A, &d);
            }
        }
    }
    for e in exes {
        o.rec(0x87B, &e);
    }
}

fn bound_or(first: &Rect, all: &[Rect]) -> Rect {
    let mut b = *first;
    for r in all {
        b.r0 = b.r0.min(r.r0);
        b.c0 = b.c0.min(r.c0);
        b.r1 = b.r1.max(r.r1);
        b.c1 = b.c1.max(r.c1);
    }
    b
}
