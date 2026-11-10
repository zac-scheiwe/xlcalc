use crate::rawxml::{escape_attr, escape_text};
use crate::styles::{dxf_read, StyleWriter};
use crate::xml::Node;
use xlcalc::cfv::{
    BarColor, CfKind, CfRule, CondValue, DataBarSpec, IconCrit, IconSetSpec, ScaleCrit,
};
use xlcalc::{CellAddr, Rect, SheetId, Workbook};

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

const PERIODS: [&str; 10] = [
    "today",
    "yesterday",
    "last7Days",
    "thisWeek",
    "lastWeek",
    "lastMonth",
    "tomorrow",
    "nextWeek",
    "nextMonth",
    "thisMonth",
];

const ICON_SETS: [&str; 20] = [
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
    "3Stars",
    "3Triangles",
    "5Boxes",
];

fn cfvo_ty(name: &str) -> i32 {
    match name {
        "min" => 1,
        "max" => 2,
        "percent" => 3,
        "formula" => 4,
        "percentile" => 5,
        _ => 0,
    }
}

pub(crate) struct CfDef {
    applies: Vec<Rect>,
    rule: Node,
}

pub(crate) fn read(sheet: &Node) -> Vec<CfDef> {
    let nodes: Vec<&Node> = sheet.children("conditionalFormatting").collect();
    read_nodes(&nodes)
}

fn read_nodes(nodes: &[&Node]) -> Vec<CfDef> {
    let mut out = Vec::new();
    for cf in nodes {
        let applies: Vec<Rect> = cf
            .attr("sqref")
            .unwrap_or("")
            .split_whitespace()
            .filter_map(crate::parse_rect)
            .collect();
        if applies.is_empty() {
            continue;
        }
        for r in cf.children("cfRule") {
            out.push(CfDef {
                applies: applies.clone(),
                rule: r.clone(),
            });
        }
    }
    out
}

fn flag(n: &Node, k: &str, default: bool) -> bool {
    n.attr(k).map_or(default, |v| v == "1" || v == "true")
}

fn argb(c: Option<&Node>) -> u32 {
    c.and_then(|c| c.attr("rgb"))
        .and_then(|v| u32::from_str_radix(v, 16).ok())
        .map_or(0, |v| v & 0xFF_FFFF)
}

fn value_of(v: &Node) -> (i32, f64, Option<String>) {
    let ty = cfvo_ty(v.attr("type").unwrap_or("num"));
    let val = v.attr("val").unwrap_or("");
    if ty == 4 {
        return (ty, 0.0, Some(format!("={val}")));
    }
    (ty, val.trim().parse().unwrap_or(0.0), None)
}

fn convert(wb: &Workbook, sheet: SheetId, defs: &[CfDef]) -> Vec<CfRule> {
    let dxfs = &wb.style_table().dxfs;
    let mut out: Vec<CfRule> = Vec::new();
    for d in defs {
        let n = &d.rule;
        let anchor = CellAddr::new(
            sheet,
            d.applies.iter().map(|r| r.r0).min().unwrap_or(0),
            d.applies.iter().map(|r| r.c0).min().unwrap_or(0),
        );
        let formula = |i: usize| {
            n.children("formula").nth(i).map(|f| {
                wb.cfv_to_relative(&f.text, anchor)
                    .unwrap_or_else(|_| format!("={}", f.text.trim()))
            })
        };
        let num = |k: &str| n.attr(k).and_then(|v| v.parse::<u32>().ok());
        let (mut f1, mut f2) = (None, None);
        let kind = match n.attr("type").unwrap_or("") {
            "cellIs" => {
                let op = OPERATORS
                    .iter()
                    .position(|o| Some(*o) == n.attr("operator"))
                    .unwrap_or(2) as i32
                    + 1;
                f1 = formula(0);
                if matches!(op, 1 | 2) {
                    f2 = formula(1);
                }
                CfKind::CellValue(op)
            }
            "expression" => {
                f1 = formula(0);
                CfKind::Expression
            }
            t @ ("containsText" | "notContainsText" | "beginsWith" | "endsWith") => CfKind::Text {
                op: match t {
                    "containsText" => 0,
                    "notContainsText" => 1,
                    "beginsWith" => 2,
                    _ => 3,
                },
                text: n.attr("text").unwrap_or("").to_string(),
            },
            "containsBlanks" => CfKind::Blanks,
            "notContainsBlanks" => CfKind::NoBlanks,
            "containsErrors" => CfKind::Errors,
            "notContainsErrors" => CfKind::NoErrors,
            "timePeriod" => {
                let Some(p) = PERIODS
                    .iter()
                    .position(|p| Some(*p) == n.attr("timePeriod"))
                else {
                    continue;
                };
                CfKind::TimePeriod(p as i32)
            }
            "uniqueValues" => CfKind::Unique { duplicate: false },
            "duplicateValues" => CfKind::Unique { duplicate: true },
            "top10" => CfKind::Top10 {
                bottom: flag(n, "bottom", false),
                percent: flag(n, "percent", false),
                rank: num("rank").unwrap_or(10),
            },
            "aboveAverage" => {
                let sd = num("stdDev").unwrap_or(0);
                CfKind::AboveAverage {
                    below: n.attr("aboveAverage") == Some("0"),
                    equal: flag(n, "equalAverage", false),
                    std: sd > 0,
                    num_std: sd.max(1),
                }
            }
            "colorScale" => {
                let Some(cs) = n.child("colorScale") else {
                    continue;
                };
                let colors: Vec<u32> = cs.children("color").map(|c| argb(Some(c))).collect();
                let crits: Vec<ScaleCrit> = cs
                    .children("cfvo")
                    .zip(colors)
                    .map(|(v, color)| {
                        let (ty, value, formula) = value_of(v);
                        ScaleCrit {
                            ty,
                            value,
                            formula,
                            color,
                        }
                    })
                    .collect();
                if !(2..=3).contains(&crits.len()) {
                    continue;
                }
                CfKind::ColorScale(crits)
            }
            "dataBar" => {
                let Some(db) = n.child("dataBar") else {
                    continue;
                };
                let mut spec = DataBarSpec::default();
                let vals: Vec<&Node> = db.children("cfvo").collect();
                let cv = |v: &Node| {
                    let (ty, value, formula) = value_of(v);
                    CondValue { ty, value, formula }
                };
                if let [lo, hi] = vals[..] {
                    spec.min = cv(lo);
                    spec.max = cv(hi);
                }
                spec.show_value = flag(db, "showValue", true);
                if db.child("color").is_some() {
                    spec.bar = BarColor::new(argb(db.child("color")));
                    spec.neg = spec.bar;
                }
                CfKind::DataBar(Box::new(spec))
            }
            "iconSet" => {
                let Some(is) = n.child("iconSet") else {
                    continue;
                };
                let id = ICON_SETS
                    .iter()
                    .position(|s| Some(*s) == is.attr("iconSet"))
                    .map_or(4, |i| i as i32 + 1);
                let reverse = flag(is, "reverse", false);
                let Some(mut spec) = IconSetSpec::new(id, reverse) else {
                    continue;
                };
                spec.icon_only = !flag(is, "showValue", true);
                let vals: Vec<&Node> = is.children("cfvo").collect();
                if vals.len() == spec.crits.len() {
                    let crits: Vec<IconCrit> = vals
                        .iter()
                        .zip(&spec.crits)
                        .map(|(v, old)| {
                            let (ty, value, _) = value_of(v);
                            IconCrit {
                                ty,
                                value,
                                op: if flag(v, "gte", true) { 7 } else { 5 },
                                icon: old.icon,
                            }
                        })
                        .collect();
                    spec.crits = crits;
                }
                CfKind::IconSet(spec)
            }
            _ => continue,
        };
        let dxf = num("dxfId")
            .and_then(|i| dxfs.get(i as usize))
            .and_then(|x| crate::xml::parse(x.as_bytes()).ok())
            .map(|x| dxf_read(&x))
            .unwrap_or_default();
        out.push(CfRule {
            id: 0,
            kind,
            applies: d.applies.clone(),
            stop: flag(n, "stopIfTrue", false),
            prio: num("priority").unwrap_or(1) as i32,
            seq: 0,
            f1,
            f2,
            dxf,
        });
    }
    out.sort_by_key(|r| r.prio);
    out
}

pub(crate) fn apply(wb: &mut Workbook, sheet: SheetId, defs: &[CfDef]) {
    let rules = convert(wb, sheet, defs);
    let sh = wb.cfv_sheet_mut(sheet);
    for mut r in rules {
        r.seq = sh.rules.iter().map(|x| x.seq).max().unwrap_or(0) + 1;
        sh.next_id += 1;
        r.id = sh.next_id;
        sh.rules.push(r);
    }
}

fn same(a: &[CfRule], b: &[CfRule]) -> bool {
    a.len() == b.len()
        && a.iter().zip(b).all(|(x, y)| {
            let (mut x, mut y) = (x.clone(), y.clone());
            (x.id, x.seq, y.id, y.seq) = (0, 0, 0, 0);
            x == y
        })
}

fn cfvo_xml(ty: i32, value: f64, formula: Option<&str>, gte: Option<bool>) -> String {
    let name = match ty {
        1 => "min",
        2 => "max",
        3 => "percent",
        4 => "formula",
        5 => "percentile",
        _ => "num",
    };
    let mut s = format!("<cfvo type=\"{name}\"");
    match (ty, formula) {
        (4, Some(f)) => s.push_str(&format!(
            " val=\"{}\"",
            escape_attr(f.trim_start_matches('='))
        )),
        (0 | 3 | 5, _) => s.push_str(&format!(" val=\"{value}\"")),
        _ => {}
    }
    if gte == Some(false) {
        s.push_str(" gte=\"0\"");
    }
    s.push_str("/>");
    s
}

fn rgb_xml(c: u32) -> String {
    format!("<color rgb=\"FF{c:06X}\"/>")
}

fn quote(s: &str) -> String {
    format!("\"{}\"", s.replace('"', "\"\""))
}

const PERIOD_FORMULAS: [&str; 10] = [
    "FLOOR(@,1)=TODAY()",
    "FLOOR(@,1)=TODAY()-1",
    "AND(TODAY()-FLOOR(@,1)<=6,FLOOR(@,1)<=TODAY())",
    "AND(TODAY()-ROUNDDOWN(@,0)<=WEEKDAY(TODAY())-1,ROUNDDOWN(@,0)-TODAY()<=7-WEEKDAY(TODAY()))",
    "AND(TODAY()-ROUNDDOWN(@,0)>=(WEEKDAY(TODAY())),TODAY()-ROUNDDOWN(@,0)<(WEEKDAY(TODAY())+7))",
    "AND(MONTH(@)=MONTH(EDATE(TODAY(),0-1)),YEAR(@)=YEAR(EDATE(TODAY(),0-1)))",
    "FLOOR(@,1)=TODAY()+1",
    "AND(ROUNDDOWN(@,0)-TODAY()>(7-WEEKDAY(TODAY())),ROUNDDOWN(@,0)-TODAY()<(15-WEEKDAY(TODAY())))",
    "AND(MONTH(@)=MONTH(EDATE(TODAY(),0+1)),YEAR(@)=YEAR(EDATE(TODAY(),0+1)))",
    "AND(MONTH(@)=MONTH(TODAY()),YEAR(@)=YEAR(TODAY()))",
];

fn rule_xml(r: &CfRule, cell: &str, styles: &mut StyleWriter) -> String {
    let mut attrs = String::new();
    let mut body = String::new();
    let ty: &str;
    let fml = |s: &str| format!("<formula>{}</formula>", escape_text(s));
    let strip = |f: &Option<String>| f.as_deref().map(|f| f.trim_start_matches('=').to_string());
    match &r.kind {
        CfKind::CellValue(op) => {
            ty = "cellIs";
            let name = OPERATORS[(*op as usize).saturating_sub(1).min(7)];
            attrs.push_str(&format!(" operator=\"{name}\""));
            for f in [&r.f1, &r.f2] {
                if let Some(f) = strip(f) {
                    body.push_str(&fml(&f));
                }
            }
        }
        CfKind::Expression => {
            ty = "expression";
            if let Some(f) = strip(&r.f1) {
                body.push_str(&fml(&f));
            }
        }
        CfKind::Top10 {
            bottom,
            percent,
            rank,
        } => {
            ty = "top10";
            if *percent {
                attrs.push_str(" percent=\"1\"");
            }
            if *bottom {
                attrs.push_str(" bottom=\"1\"");
            }
            attrs.push_str(&format!(" rank=\"{rank}\""));
        }
        CfKind::Unique { duplicate } => {
            ty = if *duplicate {
                "duplicateValues"
            } else {
                "uniqueValues"
            };
        }
        CfKind::Text { op, text } => {
            let t = quote(text);
            let (name, f) = match op {
                0 => ("containsText", format!("NOT(ISERROR(SEARCH({t},{cell})))")),
                1 => ("notContainsText", format!("ISERROR(SEARCH({t},{cell}))")),
                2 => ("beginsWith", format!("LEFT({cell},LEN({t}))={t}")),
                _ => ("endsWith", format!("RIGHT({cell},LEN({t}))={t}")),
            };
            ty = name;
            attrs.push_str(&format!(
                " operator=\"{name}\" text=\"{}\"",
                escape_attr(text)
            ));
            body.push_str(&fml(&f));
        }
        CfKind::Blanks => {
            ty = "containsBlanks";
            body.push_str(&fml(&format!("LEN(TRIM({cell}))=0")));
        }
        CfKind::NoBlanks => {
            ty = "notContainsBlanks";
            body.push_str(&fml(&format!("LEN(TRIM({cell}))>0")));
        }
        CfKind::Errors => {
            ty = "containsErrors";
            body.push_str(&fml(&format!("ISERROR({cell})")));
        }
        CfKind::NoErrors => {
            ty = "notContainsErrors";
            body.push_str(&fml(&format!("NOT(ISERROR({cell}))")));
        }
        CfKind::TimePeriod(p) => {
            let i = (*p as usize).min(9);
            ty = "timePeriod";
            attrs.push_str(&format!(" timePeriod=\"{}\"", PERIODS[i]));
            body.push_str(&fml(&PERIOD_FORMULAS[i].replace('@', cell)));
        }
        CfKind::AboveAverage {
            below,
            equal,
            std,
            num_std,
        } => {
            ty = "aboveAverage";
            if *below {
                attrs.push_str(" aboveAverage=\"0\"");
            }
            if *equal {
                attrs.push_str(" equalAverage=\"1\"");
            }
            if *std {
                attrs.push_str(&format!(" stdDev=\"{num_std}\""));
            }
        }
        CfKind::ColorScale(crits) => {
            ty = "colorScale";
            body.push_str("<colorScale>");
            for c in crits {
                body.push_str(&cfvo_xml(c.ty, c.value, c.formula.as_deref(), None));
            }
            for c in crits {
                body.push_str(&rgb_xml(c.color));
            }
            body.push_str("</colorScale>");
        }
        CfKind::DataBar(d) => {
            ty = "dataBar";
            body.push_str("<dataBar");
            if !d.show_value {
                body.push_str(" showValue=\"0\"");
            }
            body.push('>');
            for v in [&d.min, &d.max] {
                body.push_str(&cfvo_xml(v.ty, v.value, v.formula.as_deref(), None));
            }
            body.push_str(&rgb_xml(d.bar.rgb));
            body.push_str("</dataBar>");
        }
        CfKind::IconSet(s) => {
            ty = "iconSet";
            let name = ICON_SETS[(s.id as usize).saturating_sub(1).min(19)];
            body.push_str(&format!("<iconSet iconSet=\"{name}\""));
            if s.icon_only {
                body.push_str(" showValue=\"0\"");
            }
            if s.reverse {
                body.push_str(" reverse=\"1\"");
            }
            body.push('>');
            for c in &s.crits {
                body.push_str(&cfvo_xml(c.ty, c.value, None, Some(c.op == 7)));
            }
            body.push_str("</iconSet>");
        }
    }
    let mut head = format!("<cfRule type=\"{ty}\"");
    if let Some(i) = styles.dxf(&r.dxf) {
        head.push_str(&format!(" dxfId=\"{i}\""));
    }
    head.push_str(&format!(" priority=\"{}\"", r.prio));
    if r.stop {
        head.push_str(" stopIfTrue=\"1\"");
    }

    head.push_str(&attrs);
    if body.is_empty() {
        format!("{head}/>")
    } else {
        format!("{head}>{body}</cfRule>")
    }
}

pub(crate) fn write(
    wb: &Workbook,
    sheet: SheetId,
    source: &[Node],
    styles: &mut StyleWriter,
) -> Option<Option<String>> {
    let rules: &[CfRule] = wb.cfv_sheet(sheet).map_or(&[], |s| &s.rules);
    let src: Vec<&Node> = source.iter().collect();
    if same(&convert(wb, sheet, &read_nodes(&src)), rules) {
        return None;
    }
    if rules.is_empty() {
        return Some(None);
    }
    let mut groups: Vec<(Vec<Rect>, Vec<&CfRule>)> = Vec::new();
    for r in rules {
        match groups.iter_mut().find(|(a, _)| *a == r.applies) {
            Some((_, v)) => v.push(r),
            None => groups.push((r.applies.clone(), vec![r])),
        }
    }
    let mut x = String::new();
    for (applies, rs) in groups {
        let sqref: Vec<String> = applies.iter().map(crate::write::rect_text).collect();
        let cell = crate::write::rect_text(&Rect::new(
            applies.iter().map(|r| r.r0).min().unwrap_or(0),
            applies.iter().map(|r| r.c0).min().unwrap_or(0),
            applies.iter().map(|r| r.r0).min().unwrap_or(0),
            applies.iter().map(|r| r.c0).min().unwrap_or(0),
        ));
        x.push_str(&format!(
            "<conditionalFormatting sqref=\"{}\">",
            sqref.join(" ")
        ));
        for r in rs {
            x.push_str(&rule_xml(r, &cell, styles));
        }
        x.push_str("</conditionalFormatting>");
    }
    Some(Some(x))
}
