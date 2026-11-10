use crate::xlsb::{decode, esc, Ctx, Dec, Rd};
use crate::xlsb_enc::{Cls, Rgce};
use crate::xlsb_rec::{Recs, P};
use crate::xlsb_styles::{color_bytes, color_xml, AUTO};
use crate::xml::Node;
use std::fmt::Write as _;

const NO_DXF: u32 = 0xFFFF_FFFF;

const OPERATORS: [&str; 8] = [
    "between",
    "notBetween",
    "equal",
    "notEqual",
    "greaterThan",
    "lessThan",
    "greaterThanOrEqual",
    "lessThanOrEqual",
];

const PERIODS: [(u32, u32, &str); 10] = [
    (15, 0, "today"),
    (17, 1, "yesterday"),
    (18, 2, "last7Days"),
    (21, 3, "thisWeek"),
    (23, 4, "lastWeek"),
    (19, 5, "lastMonth"),
    (16, 6, "tomorrow"),
    (22, 7, "nextWeek"),
    (20, 8, "nextMonth"),
    (24, 9, "thisMonth"),
];

pub(crate) const ICON_SETS: [&str; 17] = [
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

const CFVO: [&str; 8] = [
    "",
    "num",
    "min",
    "max",
    "percent",
    "percentile",
    "",
    "formula",
];

const VAL_TYPES: [&str; 8] = [
    "none",
    "whole",
    "decimal",
    "list",
    "date",
    "time",
    "textLength",
    "custom",
];

fn cell_name(r: u32, c: u32) -> String {
    format!("{}{}", xlcalc::col_to_letters(c), r + 1)
}

fn range_name(r0: u32, c0: u32, r1: u32, c1: u32) -> String {
    if (r0, c0) == (r1, c1) {
        cell_name(r0, c0)
    } else {
        format!("{}:{}", cell_name(r0, c0), cell_name(r1, c1))
    }
}

fn flag(n: &Node, k: &str, default: bool) -> bool {
    match n.attr(k) {
        Some("1" | "true") => true,
        Some(_) => false,
        None => default,
    }
}

fn num_attr(n: &Node, k: &str) -> Option<u32> {
    n.attr(k).and_then(|v| v.parse().ok())
}

#[derive(Default)]
pub(crate) struct Reader {
    cf: String,
    dv: String,
    dv_count: u32,
    in_cf: bool,
    base: (u32, u32),
}

fn rects(rd: &mut Rd<'_>) -> Result<(Vec<String>, (u32, u32)), String> {
    let n = rd.u32()?;
    let mut out = Vec::new();
    let mut base = (0, 0);
    for i in 0..n {
        let (r0, r1, c0, c1) = (rd.u32()?, rd.u32()?, rd.u32()?, rd.u32()?);
        if i == 0 {
            base = (r0, c0);
        }
        out.push(range_name(r0, c0, r1, c1));
    }
    Ok((out, base))
}

fn parsed(rd: &mut Rd<'_>, ctx: &Ctx<'_>, base: (u32, u32)) -> Result<Option<String>, String> {
    let cce = rd.u32()? as usize;
    let rgce = rd.take(cce)?;
    let cb = rd.u32()? as usize;
    let rgcb = rd.take(cb)?;
    if cce == 0 {
        return Ok(None);
    }
    match decode(ctx, rgce, rgcb, base)? {
        Dec::Text(t) => Ok(Some(t)),
        Dec::Exp { .. } => Err("shared formula in a rule".into()),
    }
}

fn wide(rd: &mut Rd<'_>) -> Result<Option<String>, String> {
    let n = rd.u32()?;
    if n == 0xFFFF_FFFF {
        return Ok(None);
    }
    Ok(Some(rd.utf16(n as usize)?))
}

fn number(v: f64) -> String {
    format!("{v}")
}

impl Reader {
    pub(crate) fn wants(id: u32) -> bool {
        matches!(id, 461..=471 | 564 | 573 | 574 | 64)
    }

    pub(crate) fn rec(
        &mut self,
        id: u32,
        d: &[u8],
        ctx: &Ctx<'_>,
        warnings: &mut Vec<String>,
    ) -> Result<(), String> {
        let mut rd = Rd::new(d);
        match id {
            461 => {
                rd.u32()?;
                let pivot = rd.u32()? != 0;
                let (list, base) = rects(&mut rd)?;
                self.base = base;
                self.in_cf = true;
                let _ = write!(
                    self.cf,
                    "<conditionalFormatting sqref=\"{}\"",
                    list.join(" ")
                );
                if pivot {
                    self.cf.push_str(" pivot=\"1\"");
                }
                self.cf.push('>');
            }
            462 => {
                self.cf.push_str("</conditionalFormatting>");
                self.in_cf = false;
            }
            463 => self.rule(&mut rd, ctx, warnings)?,
            464 => self.cf.push_str("</cfRule>"),
            465 => {
                let set = rd.u32()? as usize;
                let fl = rd.u16()?;
                self.cf.push_str("<iconSet");
                if set != 3 {
                    let _ = write!(
                        self.cf,
                        " iconSet=\"{}\"",
                        ICON_SETS.get(set).copied().unwrap_or("3TrafficLights1")
                    );
                }
                if fl & 2 != 0 {
                    self.cf.push_str(" showValue=\"0\"");
                }
                if fl & 4 != 0 {
                    self.cf.push_str(" reverse=\"1\"");
                }
                self.cf.push('>');
            }
            466 => self.cf.push_str("</iconSet>"),
            467 => {
                let (lo, hi, fl) = (rd.u8()?, rd.u8()?, rd.u8()?);
                self.cf.push_str("<dataBar");
                if lo != 10 {
                    let _ = write!(self.cf, " minLength=\"{lo}\"");
                }
                if hi != 90 {
                    let _ = write!(self.cf, " maxLength=\"{hi}\"");
                }
                if fl & 1 == 0 {
                    self.cf.push_str(" showValue=\"0\"");
                }
                self.cf.push('>');
            }
            468 => self.cf.push_str("</dataBar>"),
            469 => self.cf.push_str("<colorScale>"),
            470 => self.cf.push_str("</colorScale>"),
            471 => {
                let ty = rd.u32()? as usize;
                let v = rd.f64()?;
                let (has_gte, gte) = (rd.u32()? != 0, rd.u32()? != 0);
                let has_f = rd.u32()? != 0;
                let name = CFVO.get(ty).copied().filter(|n| !n.is_empty());
                let Some(name) = name else {
                    return Ok(());
                };
                let _ = write!(self.cf, "<cfvo type=\"{name}\"");
                match ty {
                    1 | 4 | 5 => {
                        let _ = write!(self.cf, " val=\"{}\"", number(v));
                    }
                    7 if has_f => {
                        let f = parsed(&mut rd, ctx, self.base)
                            .and_then(|f| f.ok_or_else(|| "empty cfvo formula".to_string()));
                        match f {
                            Ok(f) => {
                                let _ = write!(self.cf, " val=\"{}\"", esc(&f));
                            }
                            Err(e) => warnings.push(format!("conditional format value: {e}")),
                        }
                    }
                    _ => {}
                }
                if has_gte && !gte {
                    self.cf.push_str(" gte=\"0\"");
                }
                self.cf.push_str("/>");
            }
            564 if self.in_cf => {
                let mut c = [0u8; 8];
                c.copy_from_slice(rd.take(8)?);
                self.cf.push_str(&color_xml("color", c));
            }
            573 => {}
            574 => {}
            64 => self.validation(&mut rd, ctx, warnings)?,
            _ => {}
        }
        Ok(())
    }

    fn rule(
        &mut self,
        rd: &mut Rd<'_>,
        ctx: &Ctx<'_>,
        warnings: &mut Vec<String>,
    ) -> Result<(), String> {
        let (ty, tmpl, dxf, pri, par) = (rd.u32()?, rd.u32()?, rd.u32()?, rd.u32()?, rd.u32()?);
        rd.take(8)?;
        let flags = rd.u16()?;
        let (c1, c2) = (rd.u32()?, rd.u32()?);
        rd.u32()?;
        let text = wide(rd)?;
        let f1 = if c1 > 0 {
            parsed(rd, ctx, self.base)?
        } else {
            None
        };
        let f2 = if c2 > 0 {
            parsed(rd, ctx, self.base)?
        } else {
            None
        };
        let mut attrs = String::new();
        let mut kind = "";
        match (ty, tmpl) {
            (1, _) => {
                kind = "cellIs";
                let op = OPERATORS
                    .get((par as usize).wrapping_sub(1))
                    .copied()
                    .unwrap_or("equal");
                let _ = write!(attrs, " operator=\"{op}\"");
            }
            (2, 1) => kind = "expression",
            (2, 8) => {
                let (k, op) = match par {
                    1 => ("notContainsText", "notContains"),
                    2 => ("beginsWith", "beginsWith"),
                    3 => ("endsWith", "endsWith"),
                    _ => ("containsText", "containsText"),
                };
                kind = k;
                let _ = write!(
                    attrs,
                    " operator=\"{op}\" text=\"{}\"",
                    esc(text.as_deref().unwrap_or(""))
                );
            }
            (2, 9) => kind = "containsBlanks",
            (2, 10) => kind = "notContainsBlanks",
            (2, 11) => kind = "containsErrors",
            (2, 12) => kind = "notContainsErrors",
            (2, 7) => kind = "uniqueValues",
            (2, 0x1B) => kind = "duplicateValues",
            (2, 0x19 | 0x1A | 0x1D | 0x1E) => {
                kind = "aboveAverage";
                if matches!(tmpl, 0x1A | 0x1E) {
                    attrs.push_str(" aboveAverage=\"0\"");
                }
                if matches!(tmpl, 0x1D | 0x1E) {
                    attrs.push_str(" equalAverage=\"1\"");
                }
                if par > 0 {
                    let _ = write!(attrs, " stdDev=\"{par}\"");
                }
            }
            (2, t) => {
                if let Some(p) = PERIODS.iter().find(|p| p.0 == t) {
                    kind = "timePeriod";
                    let _ = write!(attrs, " timePeriod=\"{}\"", p.2);
                }
            }
            (3, _) => kind = "colorScale",
            (4, _) => kind = "dataBar",
            (5, _) => {
                kind = "top10";
                if flags & 8 != 0 {
                    attrs.push_str(" percent=\"1\"");
                }
                if flags & 0x10 != 0 {
                    attrs.push_str(" bottom=\"1\"");
                }
                let _ = write!(attrs, " rank=\"{par}\"");
            }
            (6, _) => kind = "iconSet",
            _ => {}
        }
        if kind.is_empty() {
            warnings.push(format!(
                "conditional format rule type {ty}/{tmpl} not carried"
            ));
            kind = "expression";
        }
        let mut s = format!("<cfRule type=\"{kind}\"");
        if dxf != NO_DXF {
            let _ = write!(s, " dxfId=\"{dxf}\"");
        }
        let _ = write!(s, " priority=\"{pri}\"");
        if flags & 2 != 0 {
            s.push_str(" stopIfTrue=\"1\"");
        }
        s.push_str(&attrs);
        s.push('>');
        for f in [f1, f2].into_iter().flatten() {
            let _ = write!(s, "<formula>{}</formula>", esc(&f));
        }

        self.cf.push_str(&s);
        Ok(())
    }

    fn validation(
        &mut self,
        rd: &mut Rd<'_>,
        ctx: &Ctx<'_>,
        warnings: &mut Vec<String>,
    ) -> Result<(), String> {
        let flags = rd.u32()?;
        let (list, base) = rects(rd)?;
        let (etitle, error, ptitle, prompt) = (wide(rd)?, wide(rd)?, wide(rd)?, wide(rd)?);
        let f1 = parsed(rd, ctx, base);
        let f2 = parsed(rd, ctx, base);
        let (f1, f2) = match (f1, f2) {
            (Ok(a), Ok(b)) => (a, b),
            (Err(e), _) | (_, Err(e)) => {
                warnings.push(format!("data validation {}: {e}", list.join(" ")));
                return Ok(());
            }
        };
        let ty = VAL_TYPES
            .get((flags & 15) as usize)
            .copied()
            .unwrap_or("none");
        let mut s = format!("<dataValidation type=\"{ty}\"");
        match (flags >> 4) & 7 {
            1 => s.push_str(" errorStyle=\"warning\""),
            2 => s.push_str(" errorStyle=\"information\""),
            _ => {}
        }
        if let Some(op) = OPERATORS
            .get(((flags >> 20) & 15) as usize)
            .filter(|o| **o != "between")
        {
            let _ = write!(s, " operator=\"{op}\"");
        }
        if flags & 0x100 != 0 {
            s.push_str(" allowBlank=\"1\"");
        }
        if flags & 0x200 != 0 {
            s.push_str(" showDropDown=\"1\"");
        }
        if flags & 0x4_0000 != 0 {
            s.push_str(" showInputMessage=\"1\"");
        }
        if flags & 0x8_0000 != 0 {
            s.push_str(" showErrorMessage=\"1\"");
        }
        for (k, v) in [
            ("errorTitle", etitle),
            ("error", error),
            ("promptTitle", ptitle),
            ("prompt", prompt),
        ] {
            if let Some(v) = v.filter(|v| !v.is_empty()) {
                let _ = write!(s, " {k}=\"{}\"", esc(&v));
            }
        }
        let _ = write!(s, " sqref=\"{}\">", list.join(" "));
        for (k, f) in [("formula1", f1), ("formula2", f2)] {
            if let Some(f) = f {
                let _ = write!(s, "<{k}>{}</{k}>", esc(&f));
            }
        }
        s.push_str("</dataValidation>");
        self.dv.push_str(&s);
        self.dv_count += 1;
        Ok(())
    }

    pub(crate) fn cf_xml(&self) -> &str {
        &self.cf
    }

    pub(crate) fn dv_xml(&self) -> String {
        if self.dv_count == 0 {
            String::new()
        } else {
            format!(
                "<dataValidations count=\"{}\">{}</dataValidations>",
                self.dv_count, self.dv
            )
        }
    }
}

pub(crate) type Enc<'a> = dyn FnMut(&str, Cls) -> Result<Rgce, String> + 'a;

fn parse_sqref(s: &str) -> Vec<xlcalc::Rect> {
    s.split_whitespace().filter_map(crate::parse_rect).collect()
}

fn size_extra(rgce: &[u8]) -> usize {
    let mut p = 0;
    let mut extra = 0;
    while p < rgce.len() {
        let ptg = rgce[p];
        let b = if ptg >= 0x20 {
            (ptg & 0x1F) | 0x20
        } else {
            ptg
        };
        let len = match b {
            0x03..=0x16 => 1,
            0x17 => {
                let n = rgce
                    .get(p + 1..p + 3)
                    .map_or(0, |s| usize::from(u16::from_le_bytes([s[0], s[1]])));
                3 + 2 * n
            }
            0x19 => {
                let ty = rgce.get(p + 1).copied().unwrap_or(0);
                if ty & 4 != 0 {
                    let n = rgce
                        .get(p + 2..p + 4)
                        .map_or(0, |s| usize::from(u16::from_le_bytes([s[0], s[1]])));
                    4 + 2 * (n + 1)
                } else {
                    4
                }
            }
            0x1C | 0x1D => 2,
            0x1E => 3,
            0x1F => 9,
            0x20 => 15,
            0x21 => 3,
            0x22 => 4,
            0x23 => {
                extra += 4;
                5
            }
            0x24 | 0x2C | 0x2A => {
                extra += 2;
                7
            }
            0x25 | 0x2D | 0x2B => {
                extra += 2;
                13
            }
            0x39 => {
                extra += 4;
                7
            }
            0x3A | 0x3C => {
                extra += 2;
                9
            }
            0x3B | 0x3D => {
                extra += 2;
                15
            }
            _ => break,
        };
        p += len;
    }
    extra
}

fn relativize(rgce: &mut [u8], base: (u32, u32)) {
    let mut p = 0;
    while p < rgce.len() {
        let ptg = rgce[p];
        let b = if ptg >= 0x20 {
            (ptg & 0x1F) | 0x20
        } else {
            ptg
        };
        let word = |s: &[u8]| u16::from_le_bytes([s[0], s[1]]);
        let len = match b {
            0x03..=0x16 => 1,
            0x17 => {
                let n = rgce.get(p + 1..p + 3).map_or(0, |s| usize::from(word(s)));
                3 + 2 * n
            }
            0x19 => {
                let ty = rgce.get(p + 1).copied().unwrap_or(0);
                if ty & 4 != 0 {
                    let n = rgce.get(p + 2..p + 4).map_or(0, |s| usize::from(word(s)));
                    4 + 2 * (n + 1)
                } else {
                    4
                }
            }
            0x1C | 0x1D => 2,
            0x1E => 3,
            0x1F => 9,
            0x20 => 15,
            0x21 => 3,
            0x22 => 4,
            0x23 | 0x39 => 5 + usize::from(b == 0x39) * 2,
            0x24 | 0x2A => {
                if b == 0x24 && p + 7 <= rgce.len() {
                    let col = word(&rgce[p + 5..]);
                    if col & 0xC000 != 0 {
                        let row = u32::from_le_bytes([
                            rgce[p + 1],
                            rgce[p + 2],
                            rgce[p + 3],
                            rgce[p + 4],
                        ]);
                        let row = if col & 0x8000 != 0 {
                            row.wrapping_sub(base.0) & 0xF_FFFF
                        } else {
                            row
                        };
                        let c = col & 0x3FFF;
                        let c = if col & 0x4000 != 0 {
                            (u32::from(c)).wrapping_sub(base.1) as u16 & 0x3FFF
                        } else {
                            c
                        };
                        rgce[p] = ptg - 0x24 + 0x2C;
                        rgce[p + 1..p + 5].copy_from_slice(&row.to_le_bytes());
                        rgce[p + 5..p + 7].copy_from_slice(&(c | (col & 0xC000)).to_le_bytes());
                    }
                }
                7
            }
            0x25 | 0x2B => {
                if b == 0x25 && p + 13 <= rgce.len() {
                    let (c0, c1) = (word(&rgce[p + 9..]), word(&rgce[p + 11..]));
                    if (c0 | c1) & 0xC000 != 0 {
                        let mut rows = [0u32; 2];
                        for (i, r) in rows.iter_mut().enumerate() {
                            let o = p + 1 + 4 * i;
                            let v = u32::from_le_bytes([
                                rgce[o],
                                rgce[o + 1],
                                rgce[o + 2],
                                rgce[o + 3],
                            ]);
                            let rel = [c0, c1][i] & 0x8000 != 0;
                            *r = if rel {
                                v.wrapping_sub(base.0) & 0xF_FFFF
                            } else {
                                v
                            };
                        }
                        let mut cols = [0u16; 2];
                        for (i, c) in cols.iter_mut().enumerate() {
                            let w = [c0, c1][i];
                            let v = w & 0x3FFF;
                            let v = if w & 0x4000 != 0 {
                                (u32::from(v)).wrapping_sub(base.1) as u16 & 0x3FFF
                            } else {
                                v
                            };
                            *c = v | (w & 0xC000);
                        }
                        rgce[p] = ptg - 0x25 + 0x2D;
                        rgce[p + 1..p + 5].copy_from_slice(&rows[0].to_le_bytes());
                        rgce[p + 5..p + 9].copy_from_slice(&rows[1].to_le_bytes());
                        rgce[p + 9..p + 11].copy_from_slice(&cols[0].to_le_bytes());
                        rgce[p + 11..p + 13].copy_from_slice(&cols[1].to_le_bytes());
                    }
                }
                13
            }
            0x2C => 7,
            0x2D => 13,
            0x3A | 0x3C => 9,
            0x3B | 0x3D => 15,
            _ => break,
        };
        p += len;
    }
}

struct Formula {
    size: u32,
    rgce: Vec<u8>,
    rgcb: Vec<u8>,
}

fn formula(enc: &mut Enc<'_>, text: &str, cls: Cls, base: (u32, u32)) -> Result<Formula, String> {
    let mut f = enc(text.trim().trim_start_matches('='), cls)?;
    relativize(&mut f.rgce, base);
    Ok(Formula {
        size: (f.rgce.len() + size_extra(&f.rgce)) as u32,
        rgce: f.rgce,
        rgcb: f.rgcb,
    })
}

fn put_formula(p: &mut P, f: &Formula) {
    p.u32(f.rgce.len() as u32)
        .raw(&f.rgce)
        .u32(f.rgcb.len() as u32)
        .raw(&f.rgcb);
}

fn cfvo(
    r: &mut Recs,
    n: &Node,
    in_icon_set: bool,
    enc: &mut Enc<'_>,
    base: (u32, u32),
) -> Result<(), String> {
    let ty = n.attr("type").unwrap_or("num");
    let code = CFVO
        .iter()
        .position(|c| *c == ty)
        .filter(|i| !CFVO[*i].is_empty())
        .ok_or_else(|| format!("cfvo type {ty}"))? as u32;
    let val = n.attr("val").unwrap_or("0");
    let mut fml = None;
    let mut num = 0.0;
    if code == 7 {
        fml = Some(formula(enc, val, Cls::Value, base)?);
    } else if matches!(code, 1 | 4 | 5) {
        num = val
            .trim()
            .parse()
            .map_err(|_| format!("cfvo value {val}"))?;
    }
    r.with(471, |p| {
        p.u32(code)
            .f64(num)
            .u32(u32::from(in_icon_set))
            .u32(u32::from(in_icon_set && flag(n, "gte", true)));
        match &fml {
            Some(f) => {
                p.u32(f.size);
                put_formula(p, f);
            }
            None => {
                p.u32(0);
            }
        }
    });
    Ok(())
}

fn colors(r: &mut Recs, n: &Node) {
    for c in n.children("color") {
        r.with(564, |p| {
            p.raw(&color_bytes(Some(c), AUTO));
        });
    }
}

fn rule(n: &Node, base: (u32, u32), enc: &mut Enc<'_>) -> Result<Vec<u8>, String> {
    let ty = n.attr("type").unwrap_or("");
    let (mut kind, mut tmpl, mut par, mut flags) = (2u32, 1u32, 0u32, 0u16);
    let mut text: Option<String> = None;
    let mut want_formulas = true;
    match ty {
        "cellIs" => {
            let op = n.attr("operator").unwrap_or("equal");
            let i = OPERATORS
                .iter()
                .position(|o| *o == op)
                .ok_or_else(|| format!("cellIs operator {op}"))?;
            (kind, tmpl, par) = (1, 0, i as u32 + 1);
        }
        "expression" => {}
        "containsText" | "notContainsText" | "beginsWith" | "endsWith" => {
            tmpl = 8;
            par = match ty {
                "containsText" => 0,
                "notContainsText" => 1,
                "beginsWith" => 2,
                _ => 3,
            };
            text = Some(n.attr("text").unwrap_or("").to_string());
        }
        "containsBlanks" => tmpl = 9,
        "notContainsBlanks" => tmpl = 10,
        "containsErrors" => tmpl = 11,
        "notContainsErrors" => tmpl = 12,
        "timePeriod" => {
            let p = n.attr("timePeriod").unwrap_or("today");
            let e = PERIODS
                .iter()
                .find(|x| x.2 == p)
                .ok_or_else(|| format!("timePeriod {p}"))?;
            (tmpl, par) = (e.0, e.1);
        }
        "uniqueValues" => (tmpl, want_formulas) = (7, false),
        "duplicateValues" => (tmpl, want_formulas) = (0x1B, false),
        "aboveAverage" => {
            let below = n.attr("aboveAverage") == Some("0");
            let equal = flag(n, "equalAverage", false);
            tmpl = match (below, equal) {
                (false, false) => 0x19,
                (true, false) => 0x1A,
                (false, true) => 0x1D,
                (true, true) => 0x1E,
            };
            par = num_attr(n, "stdDev").unwrap_or(0);
            if !below {
                flags |= 4;
            }
            want_formulas = false;
        }
        "top10" => {
            (kind, tmpl, want_formulas) = (5, 5, false);
            par = num_attr(n, "rank").unwrap_or(10);
            if flag(n, "percent", false) {
                flags |= 8;
            }
            if flag(n, "bottom", false) {
                flags |= 0x10;
            }
        }
        "colorScale" => (kind, tmpl, want_formulas) = (3, 2, false),
        "dataBar" => (kind, tmpl, want_formulas) = (4, 3, false),
        "iconSet" => (kind, tmpl, want_formulas) = (6, 4, false),
        t => return Err(format!("rule type {t}")),
    }
    if flag(n, "stopIfTrue", false) {
        flags |= 2;
    }
    let mut fs: Vec<Formula> = Vec::new();
    if want_formulas {
        for f in n.children("formula").take(2) {
            fs.push(formula(enc, &f.text, Cls::Value, base)?);
        }
    }
    let mut r = Recs::default();
    r.with(463, |p| {
        p.u32(kind)
            .u32(tmpl)
            .u32(num_attr(n, "dxfId").unwrap_or(NO_DXF))
            .u32(num_attr(n, "priority").unwrap_or(1))
            .u32(par)
            .u32(0)
            .u32(0)
            .u16(flags)
            .u32(fs.first().map_or(0, |f| f.size))
            .u32(fs.get(1).map_or(0, |f| f.size))
            .u32(0);
        match &text {
            Some(t) => {
                p.wstr(t);
            }
            None => {
                p.null_str();
            }
        }
        for f in &fs {
            put_formula(p, f);
        }
    });
    match ty {
        "colorScale" => {
            if let Some(cs) = n.child("colorScale") {
                r.empty(469);
                for v in cs.children("cfvo") {
                    cfvo(&mut r, v, false, enc, base)?;
                }
                colors(&mut r, cs);
                r.empty(470);
            }
        }
        "dataBar" => {
            if let Some(db) = n.child("dataBar") {
                r.with(467, |p| {
                    p.u8(num_attr(db, "minLength").unwrap_or(10) as u8)
                        .u8(num_attr(db, "maxLength").unwrap_or(90) as u8)
                        .u8(u8::from(flag(db, "showValue", true)));
                });
                for v in db.children("cfvo") {
                    cfvo(&mut r, v, false, enc, base)?;
                }
                colors(&mut r, db);
                r.empty(468);
            }
        }
        "iconSet" => {
            if let Some(is) = n.child("iconSet") {
                let set = is.attr("iconSet").unwrap_or("3TrafficLights1");
                let idx = ICON_SETS
                    .iter()
                    .position(|s| *s == set)
                    .ok_or_else(|| format!("icon set {set}"))?;
                let all_gte = is.children("cfvo").all(|c| flag(c, "gte", true));
                let mut fl = 0x68u16;
                if all_gte {
                    fl |= 0x10;
                }
                if !flag(is, "showValue", true) {
                    fl |= 2;
                }
                if flag(is, "reverse", false) {
                    fl |= 4;
                }
                r.with(465, |p| {
                    p.u32(idx as u32).u16(fl);
                });
                for v in is.children("cfvo") {
                    cfvo(&mut r, v, true, enc, base)?;
                }
                r.empty(466);
            }
        }
        _ => {}
    }
    r.empty(464);
    Ok(r.bytes)
}

pub(crate) fn write_cf(
    r: &mut Recs,
    root: &Node,
    sheet: &str,
    enc: &mut Enc<'_>,
    warnings: &mut Vec<String>,
) {
    for cf in root.children("conditionalFormatting") {
        let ranges = parse_sqref(cf.attr("sqref").unwrap_or(""));
        let Some(first) = ranges.first() else {
            continue;
        };
        let base = (first.r0, first.c0);
        let mut body = Vec::new();
        let mut count = 0u32;
        for n in cf.children("cfRule") {
            match rule(n, base, enc) {
                Ok(b) => {
                    body.extend(b);
                    count += 1;
                }
                Err(e) => warnings.push(format!("sheet {sheet}: conditional format: {e}")),
            }
        }
        if count == 0 {
            continue;
        }
        r.with(461, |p| {
            p.u32(count)
                .u32(u32::from(flag(cf, "pivot", false)))
                .u32(ranges.len() as u32);
            for rc in &ranges {
                p.u32(rc.r0).u32(rc.r1).u32(rc.c0).u32(rc.c1);
            }
        });
        r.bytes.extend(body);
        r.empty(462);
    }
}

fn list_literal(f: &str) -> Option<String> {
    let t = f.trim();
    let inner = t.strip_prefix('"')?.strip_suffix('"')?;
    if inner.replace("\"\"", "").contains('"') {
        return None;
    }
    Some(t.to_string())
}

pub(crate) fn write_dv(
    r: &mut Recs,
    root: &Node,
    sheet: &str,
    enc: &mut Enc<'_>,
    warnings: &mut Vec<String>,
) {
    let Some(dvs) = root.child("dataValidations") else {
        return;
    };
    let mut body = Recs::default();
    let mut count = 0u32;
    for v in dvs.children("dataValidation") {
        let ranges = parse_sqref(v.attr("sqref").unwrap_or(""));
        let Some(first) = ranges.first() else {
            continue;
        };
        let base = (first.r0, first.c0);
        let ty = VAL_TYPES
            .iter()
            .position(|t| Some(*t) == v.attr("type"))
            .unwrap_or(0) as u32;
        let mut flags = ty;
        flags |= match v.attr("errorStyle") {
            Some("warning") => 1,
            Some("information") => 2,
            _ => 0,
        } << 4;
        if flag(v, "allowBlank", false) {
            flags |= 0x100;
        }
        if flag(v, "showDropDown", false) {
            flags |= 0x200;
        }
        if flag(v, "showInputMessage", false) {
            flags |= 0x4_0000;
        }
        if flag(v, "showErrorMessage", false) {
            flags |= 0x8_0000;
        }
        let op = OPERATORS
            .iter()
            .position(|o| Some(*o) == v.attr("operator"))
            .unwrap_or(0) as u32;
        flags |= op << 20;
        let f1 = v.child("formula1").map(|f| f.text.trim().to_string());
        let f2 = v.child("formula2").map(|f| f.text.trim().to_string());
        let mut fs: [Option<Formula>; 2] = [None, None];
        let mut failed = false;
        for (i, f) in [f1, f2].into_iter().enumerate() {
            let Some(f) = f.filter(|f| !f.is_empty()) else {
                continue;
            };
            let lit = if ty == 3 && i == 0 {
                list_literal(&f)
            } else {
                None
            };
            if lit.is_some() {
                flags |= 0x80;
            }
            let cls = if ty == 3 && i == 0 {
                Cls::Ref
            } else {
                Cls::Value
            };
            match formula(enc, &f, cls, base) {
                Ok(x) => fs[i] = Some(x),
                Err(e) => {
                    warnings.push(format!(
                        "sheet {sheet}: data validation {}: {e}",
                        v.attr("sqref").unwrap_or("")
                    ));
                    failed = true;
                }
            }
        }
        if failed {
            continue;
        }
        body.with(64, |p| {
            p.u32(flags).u32(ranges.len() as u32);
            for rc in &ranges {
                p.u32(rc.r0).u32(rc.r1).u32(rc.c0).u32(rc.c1);
            }
            for k in ["errorTitle", "error", "promptTitle", "prompt"] {
                match v.attr(k).filter(|s| !s.is_empty()) {
                    Some(s) => {
                        p.wstr(s);
                    }
                    None => {
                        p.null_str();
                    }
                }
            }
            for f in &fs {
                match f {
                    Some(f) => put_formula(p, f),
                    None => {
                        p.u32(0).u32(0);
                    }
                }
            }
        });
        count += 1;
    }
    if count == 0 {
        return;
    }
    r.with(573, |p| {
        p.u16(0).u32(0).u32(0).u32(0).u32(count);
    });
    r.bytes.extend(body.bytes);
    r.empty(574);
}
