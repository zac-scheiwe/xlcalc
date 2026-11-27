use std::fmt::Write;

use xlcalc::{Attrs, FileColumn, FileCrit, FileSort, RangeFilterFile};

use crate::ftext::unescape_xstring;
use crate::rawxml::{escape_attr, xstring};
use crate::styles::StyleWriter;
use crate::write::{number_text, rect_text};
use crate::xml::Node;

const RICH_URI: &str = "{1AD28BCE-077C-4C59-8B6E-1921CE8616D4}";
const RICH_NS: &str = "http://schemas.microsoft.com/office/spreadsheetml/2017/richdata2";
const X14_NS: &str = "http://schemas.microsoft.com/office/spreadsheetml/2009/9/main";
const MC_NS: &str = "http://schemas.openxmlformats.org/markup-compatibility/2006";

const X14_SETS: [&str; 3] = ["3Stars", "3Triangles", "5Boxes"];

fn on(n: &Node, key: &str, default: bool) -> bool {
    n.attr(key).map_or(default, |v| matches!(v, "1" | "true"))
}

fn text(n: &Node, key: &str) -> String {
    unescape_xstring(n.attr(key).unwrap_or(""))
}

fn number(n: &Node, key: &str) -> Option<f64> {
    n.attr(key).and_then(|v| v.parse().ok())
}

fn within<'a>(n: &'a Node, name: &str) -> Option<&'a Node> {
    n.kids.iter().find_map(|k| match k.name.as_str() {
        x if x == name => Some(k),
        "extLst" | "ext" | "AlternateContent" | "Choice" | "filterColumn" => within(k, name),
        _ => None,
    })
}

fn dxf_rgb(dxf: &str) -> Option<u32> {
    let x = crate::xml::parse(dxf.as_bytes()).ok()?;
    let fill = x.child("fill")?.child("patternFill")?;
    if fill.attr("patternType") == Some("none") {
        return None;
    }
    let argb = fill.child("fgColor")?.attr("rgb")?;
    u32::from_str_radix(argb, 16).ok().map(|c| c & 0x00FF_FFFF)
}

fn read_crit(c: &Node, dxfs: &[&str]) -> Option<FileCrit> {
    if let Some(f) = within(c, "filters") {
        return Some(FileCrit::Values {
            blank: on(f, "blank", false),
            calendar: f.attr("calendarType").map(str::to_string),
            items: f.children("filter").map(|v| text(v, "val")).collect(),
            dates: f
                .children("dateGroupItem")
                .map(|d| d.attrs.clone())
                .collect(),
            rich: c.child("filters").is_none(),
        });
    }
    if let Some(f) = c.child("customFilters") {
        return Some(FileCrit::Custom {
            and: on(f, "and", false),
            items: f
                .children("customFilter")
                .map(|v| {
                    let op = v.attr("operator").filter(|o| *o != "equal");
                    (op.unwrap_or("").to_string(), text(v, "val"))
                })
                .collect(),
        });
    }
    if let Some(f) = c.child("top10") {
        return Some(FileCrit::Top10 {
            top: on(f, "top", true),
            percent: on(f, "percent", false),
            val: f.attr("val")?.to_string(),
            filter_val: number(f, "filterVal"),
        });
    }
    if let Some(f) = c.child("dynamicFilter") {
        return Some(FileCrit::Dynamic {
            kind: f.attr("type")?.to_string(),
            val: number(f, "val"),
            max: number(f, "maxVal"),
        });
    }
    if let Some(f) = c.child("colorFilter") {
        let dxf = f
            .attr("dxfId")
            .and_then(|i| i.parse::<usize>().ok())
            .and_then(|i| dxfs.get(i));
        return Some(FileCrit::Color {
            font: !on(f, "cellColor", true),
            rgb: dxf.and_then(|d| dxf_rgb(d)),
            dxf: dxf.map(|d| d.to_string()),
        });
    }
    within(c, "iconFilter").and_then(|f| {
        Some(FileCrit::Icon {
            set: f.attr("iconSet")?.to_string(),
            id: f.attr("iconId").and_then(|i| i.parse().ok()),
        })
    })
}

pub(crate) fn read(sheet: &Node, dxfs: &[&str]) -> Option<RangeFilterFile> {
    let af = sheet.child("autoFilter")?;
    let rect = crate::parse_rect(af.attr("ref")?)?;
    let columns = af
        .children("filterColumn")
        .filter_map(|c| {
            Some(FileColumn {
                field: c.attr("colId")?.parse().ok()?,
                hidden_button: on(c, "hiddenButton", false),
                show_button: on(c, "showButton", true),
                crit: read_crit(c, dxfs),
            })
        })
        .collect();
    let sort = af.child("sortState").map(|s| FileSort {
        attrs: s.attrs.clone(),
        conditions: s
            .children("sortCondition")
            .map(|c| c.attrs.clone())
            .collect(),
    });
    Some(RangeFilterFile {
        rect,
        columns,
        sort,
    })
}

fn attr(x: &mut String, key: &str, value: &str) {
    let _ = write!(x, " {key}=\"{}\"", escape_attr(&xstring(value)));
}

fn attrs(x: &mut String, list: &Attrs) {
    for (k, v) in list {
        attr(x, k, v);
    }
}

fn element(x: &mut String, name: &str, head: &str, body: &str) {
    if body.is_empty() {
        let _ = write!(x, "<{name}{head}/>");
    } else {
        let _ = write!(x, "<{name}{head}>{body}</{name}>");
    }
}

fn write_crit(x: &mut String, crit: &FileCrit, styles: &mut StyleWriter) {
    match crit {
        FileCrit::Values {
            blank,
            calendar,
            items,
            dates,
            rich,
        } => {
            let p = if *rich { "xlrd2:" } else { "" };
            let mut head = String::new();
            if *blank {
                attr(&mut head, "blank", "1");
            }
            if let Some(c) = calendar {
                attr(&mut head, "calendarType", c);
            }
            let mut body = String::new();
            for v in items {
                let _ = write!(body, "<{p}filter");
                attr(&mut body, "val", v);
                body.push_str("/>");
            }
            for d in dates {
                let _ = write!(body, "<{p}dateGroupItem");
                attrs(&mut body, d);
                body.push_str("/>");
            }
            let mut filters = String::new();
            element(&mut filters, &format!("{p}filters"), &head, &body);
            if *rich {
                let _ = write!(
                    x,
                    "<extLst><ext uri=\"{RICH_URI}\" xmlns:xlrd2=\"{RICH_NS}\"><xlrd2:filterColumn>{filters}</xlrd2:filterColumn></ext></extLst>"
                );
            } else {
                x.push_str(&filters);
            }
        }
        FileCrit::Custom { and, items } => {
            let mut body = String::new();
            for (op, val) in items {
                body.push_str("<customFilter");
                if !op.is_empty() {
                    attr(&mut body, "operator", op);
                }
                attr(&mut body, "val", val);
                body.push_str("/>");
            }
            let head = if *and { " and=\"1\"" } else { "" };
            element(x, "customFilters", head, &body);
        }
        FileCrit::Top10 {
            top,
            percent,
            val,
            filter_val,
        } => {
            x.push_str("<top10");
            if !top {
                attr(x, "top", "0");
            }
            if *percent {
                attr(x, "percent", "1");
            }
            attr(x, "val", val);
            if let Some(v) = filter_val {
                attr(x, "filterVal", &number_text(*v));
            }
            x.push_str("/>");
        }
        FileCrit::Dynamic { kind, val, max } => {
            x.push_str("<dynamicFilter");
            attr(x, "type", kind);
            if let Some(v) = val {
                attr(x, "val", &number_text(*v));
            }
            if let Some(v) = max {
                attr(x, "maxVal", &number_text(*v));
            }
            x.push_str("/>");
        }
        FileCrit::Color { font, rgb, dxf } => {
            let dxf = dxf.clone().unwrap_or_else(|| {
                let fill = match rgb {
                    Some(c) => format!(
                        "<patternFill patternType=\"solid\"><fgColor rgb=\"FF{c:06X}\"/><bgColor rgb=\"FFFFFFFF\"/></patternFill>"
                    ),
                    None => "<patternFill patternType=\"none\"><fgColor indexed=\"64\"/><bgColor indexed=\"65\"/></patternFill>".to_string(),
                };
                format!("<dxf><fill>{fill}</fill></dxf>")
            });
            let id = styles.filter_dxf(&dxf);
            let cell = if *font { " cellColor=\"0\"" } else { "" };
            let _ = write!(x, "<colorFilter dxfId=\"{id}\"{cell}/>");
        }
        FileCrit::Icon { set, id } => {
            let mut head = String::new();
            attr(&mut head, "iconSet", set);
            if let Some(id) = id {
                attr(&mut head, "iconId", &id.to_string());
            }
            if X14_SETS.contains(&set.as_str()) {
                let _ = write!(
                    x,
                    "<mc:AlternateContent xmlns:mc=\"{MC_NS}\"><mc:Choice Requires=\"x14\" xmlns:x14=\"{X14_NS}\"><x14:iconFilter{head}/></mc:Choice></mc:AlternateContent>"
                );
            } else {
                element(x, "iconFilter", &head, "");
            }
        }
    }
}

pub(crate) fn write(file: &RangeFilterFile, styles: &mut StyleWriter) -> String {
    let mut body = String::new();
    for c in &file.columns {
        let mut head = format!(" colId=\"{}\"", c.field);
        if c.hidden_button {
            head.push_str(" hiddenButton=\"1\"");
        }
        if !c.show_button {
            head.push_str(" showButton=\"0\"");
        }
        let mut crit = String::new();
        if let Some(f) = &c.crit {
            write_crit(&mut crit, f, styles);
        }
        element(&mut body, "filterColumn", &head, &crit);
    }
    if let Some(s) = &file.sort {
        let mut head = String::new();
        attrs(&mut head, &s.attrs);
        let mut conditions = String::new();
        for c in &s.conditions {
            conditions.push_str("<sortCondition");
            attrs(&mut conditions, c);
            conditions.push_str("/>");
        }
        element(&mut body, "sortState", &head, &conditions);
    }
    let mut x = String::new();
    let head = format!(" ref=\"{}\"", rect_text(&file.rect));
    element(&mut x, "autoFilter", &head, &body);
    x
}
