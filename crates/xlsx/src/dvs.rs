use crate::rawxml::{escape_attr, escape_text};
use crate::xml::Node;
use xlcalc::cfv::Dv;
use xlcalc::{Rect, SheetId, Workbook};

const KINDS: [&str; 8] = [
    "any",
    "whole",
    "decimal",
    "list",
    "date",
    "time",
    "textLength",
    "custom",
];
const OPS: [&str; 8] = [
    "between",
    "notBetween",
    "equal",
    "notEqual",
    "greaterThan",
    "lessThan",
    "greaterThanOrEqual",
    "lessThanOrEqual",
];

pub(crate) struct DvDef {
    applies: Vec<Rect>,
    kind: i32,
    alert: i32,
    op: i32,
    f1: Option<String>,
    f2: Option<String>,
    ignore_blank: bool,
    dropdown: bool,
    show_input: bool,
    show_error: bool,
    input_title: String,
    input_message: String,
    error_title: String,
    error_message: String,
}

fn flag(n: &Node, k: &str) -> bool {
    matches!(n.attr(k), Some("1" | "true"))
}

pub(crate) fn read(sheet: &Node) -> Vec<DvDef> {
    let Some(d) = sheet.child("dataValidations") else {
        return Vec::new();
    };
    d.children("dataValidation")
        .filter_map(|v| {
            let kind = KINDS.iter().position(|k| Some(*k) == v.attr("type"))? as i32;
            if kind == 0 {
                return None;
            }
            let applies: Vec<Rect> = v
                .attr("sqref")?
                .split_whitespace()
                .filter_map(crate::parse_rect)
                .collect();
            if applies.is_empty() {
                return None;
            }
            let text = |k: &str| v.attr(k).unwrap_or("").to_string();
            let formula = |n: &str| v.child(n).map(|f| f.text.clone());
            Some(DvDef {
                applies,
                kind,
                alert: match v.attr("errorStyle") {
                    Some("warning") => 2,
                    Some("information") => 3,
                    _ => 1,
                },
                op: 1 + OPS
                    .iter()
                    .position(|o| Some(*o) == v.attr("operator"))
                    .unwrap_or(0) as i32,
                f1: formula("formula1"),
                f2: formula("formula2"),
                ignore_blank: flag(v, "allowBlank"),
                dropdown: !flag(v, "showDropDown"),
                show_input: flag(v, "showInputMessage"),
                show_error: flag(v, "showErrorMessage"),
                input_title: text("promptTitle"),
                input_message: text("prompt"),
                error_title: text("errorTitle"),
                error_message: text("error"),
            })
        })
        .collect()
}

fn limit(kind: i32, t: Option<String>) -> Option<String> {
    let t = t?;
    if kind == 3 {
        return Some(
            match t.strip_prefix('"').and_then(|s| s.strip_suffix('"')) {
                Some(lit) => lit.replace("\"\"", "\""),
                None => format!("={t}"),
            },
        );
    }
    match t.trim().parse::<f64>() {
        Ok(n) if kind != 7 => Some(n.to_string()),
        _ => Some(format!("={t}")),
    }
}

pub(crate) fn apply(wb: &mut Workbook, sheet: SheetId, defs: &[DvDef]) {
    for d in defs {
        let two = matches!(d.kind, 1 | 2 | 4 | 5 | 6) && matches!(d.op, 1 | 2);
        wb.cfv_sheet_mut(sheet).dvs.push(Dv {
            applies: d.applies.clone(),
            kind: d.kind,
            alert: d.alert,
            op: d.op,
            f1: limit(d.kind, d.f1.clone()),
            f2: if two {
                limit(d.kind, d.f2.clone())
            } else {
                None
            },
            ignore_blank: d.ignore_blank,
            dropdown: d.dropdown,
            show_input: d.show_input,
            show_error: d.show_error,
            input_title: d.input_title.clone(),
            input_message: d.input_message.clone(),
            error_title: d.error_title.clone(),
            error_message: d.error_message.clone(),
            touched: false,
        });
    }
}

fn file_limit(kind: i32, f: &str) -> String {
    match f.strip_prefix('=') {
        Some(formula) => formula.to_string(),
        None if kind == 3 => format!("\"{}\"", f.replace('"', "\"\"")),
        None => f.to_string(),
    }
}

pub(crate) fn write(wb: &Workbook, sheet: SheetId) -> Option<String> {
    let dvs = &wb.cfv_sheet(sheet)?.dvs;
    let dvs: Vec<&Dv> = dvs
        .iter()
        .filter(|d| (1..=7).contains(&d.kind) && !d.applies.is_empty())
        .collect();
    if dvs.is_empty() {
        return None;
    }
    let mut x = format!("<dataValidations count=\"{}\">", dvs.len());
    for d in dvs {
        x.push_str(&format!(
            "<dataValidation type=\"{}\"",
            KINDS[d.kind as usize]
        ));
        match d.alert {
            2 => x.push_str(" errorStyle=\"warning\""),
            3 => x.push_str(" errorStyle=\"information\""),
            _ => {}
        }
        if d.op != 1 && d.kind != 3 && d.kind != 7 {
            if let Some(o) = OPS.get((d.op - 1) as usize) {
                x.push_str(&format!(" operator=\"{o}\""));
            }
        }
        if d.ignore_blank {
            x.push_str(" allowBlank=\"1\"");
        }
        if !d.dropdown {
            x.push_str(" showDropDown=\"1\"");
        }
        if d.show_input {
            x.push_str(" showInputMessage=\"1\"");
        }
        if d.show_error {
            x.push_str(" showErrorMessage=\"1\"");
        }
        for (k, v) in [
            ("errorTitle", &d.error_title),
            ("error", &d.error_message),
            ("promptTitle", &d.input_title),
            ("prompt", &d.input_message),
        ] {
            if !v.is_empty() {
                x.push_str(&format!(" {k}=\"{}\"", escape_attr(v)));
            }
        }
        let sqref: Vec<String> = d.applies.iter().map(crate::write::rect_text).collect();
        x.push_str(&format!(" sqref=\"{}\">", sqref.join(" ")));
        for (n, f) in [("formula1", &d.f1), ("formula2", &d.f2)] {
            if let Some(f) = f.as_deref().filter(|f| !f.is_empty()) {
                x.push_str(&format!(
                    "<{n}>{}</{n}>",
                    escape_text(&file_limit(d.kind, f))
                ));
            }
        }
        x.push_str("</dataValidation>");
    }
    x.push_str("</dataValidations>");
    Some(x)
}
