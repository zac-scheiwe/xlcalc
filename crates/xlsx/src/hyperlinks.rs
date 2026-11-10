use crate::package::Rel;
use crate::rawxml::escape_attr;
use crate::xml::Node;
use std::collections::{BTreeSet, HashMap};
use xlcalc::hyperlink::Hyperlink;
use xlcalc::{CellAddr, Rect, SheetId, Value, Workbook};

const REL_HYPERLINK: &str =
    "http://schemas.openxmlformats.org/officeDocument/2006/relationships/hyperlink";

pub(crate) struct LinkDef {
    pub rect: Rect,

    pub target: Option<String>,
    pub location: Option<String>,
    pub tooltip: Option<String>,
    pub display: Option<String>,
}

pub(crate) fn read(sheet: &Node, targets: &HashMap<String, String>) -> Vec<LinkDef> {
    let Some(h) = sheet.child("hyperlinks") else {
        return Vec::new();
    };
    h.children("hyperlink")
        .filter_map(|l| {
            let rect = crate::parse_rect(l.attr("ref")?)?;
            let non_empty = |k: &str| l.attr(k).filter(|v| !v.is_empty()).map(str::to_string);
            Some(LinkDef {
                rect,
                target: l.attr("id").and_then(|id| targets.get(id)).cloned(),
                location: non_empty("location"),
                tooltip: l.attr("tooltip").map(str::to_string),
                display: l.attr("display").map(str::to_string),
            })
        })
        .collect()
}

pub(crate) fn apply(wb: &mut Workbook, sheet: SheetId, defs: &[LinkDef]) {
    for d in defs {
        let (mut address, mut sub) = (String::new(), d.location.clone().unwrap_or_default());
        if let Some(t) = d.target.as_deref().filter(|t| !t.is_empty()) {
            let a = if t.to_ascii_lowercase().starts_with("mailto:") {
                t.to_string()
            } else {
                t.replace("%20", " ")
            };
            match a.strip_prefix('#') {
                Some(rest) => sub = rest.to_string(),
                None => address = a,
            }
        }
        if address.is_empty() && sub.is_empty() {
            continue;
        }
        let text = match wb.value(CellAddr::new(sheet, d.rect.r0, d.rect.c0)) {
            Value::Text(t) if !t.is_empty() => Some(t.to_string()),
            _ => None,
        };
        wb.hyperlink_add(
            sheet,
            Hyperlink {
                id: 0,
                rect: d.rect,
                address,
                sub_address: sub,
                screen_tip: d.tooltip.clone(),
                name: d.display.clone().or(text),
            },
        );
    }
}

fn target_of(address: &str) -> String {
    address.replace(' ', "%20")
}

pub(crate) fn write(
    wb: &Workbook,
    sheet: SheetId,
    prefix: &str,
    rels: &mut Vec<Rel>,
    used: &mut BTreeSet<String>,
    fresh: fn(&mut BTreeSet<String>) -> String,
) -> Option<String> {
    let links = wb.hyperlinks(sheet);
    if links.is_empty() {
        return None;
    }
    let mut x = String::from("<hyperlinks>");
    for l in links {
        x.push_str(&format!(
            "<hyperlink ref=\"{}\"",
            crate::write::rect_text(&l.rect)
        ));
        if !l.address.is_empty() {
            let id = fresh(used);
            x.push_str(&format!(" {prefix}:id=\"{id}\""));
            rels.push(Rel {
                id,
                ty: REL_HYPERLINK.into(),
                target: target_of(&l.address),
                external: true,
            });
        }
        if !l.sub_address.is_empty() {
            x.push_str(&format!(" location=\"{}\"", escape_attr(&l.sub_address)));
        }
        if let Some(t) = &l.screen_tip {
            x.push_str(&format!(" tooltip=\"{}\"", escape_attr(t)));
        }
        if let Some(n) = &l.name {
            let multi = l.rect.r0 != l.rect.r1 || l.rect.c0 != l.rect.c1;
            let cell = match wb.value(CellAddr::new(sheet, l.rect.r0, l.rect.c0)) {
                Value::Text(t) => t.to_string(),
                _ => String::new(),
            };
            if l.address.is_empty() || multi || cell != *n {
                x.push_str(&format!(" display=\"{}\"", escape_attr(n)));
            }
        }
        x.push_str("/>");
    }
    x.push_str("</hyperlinks>");
    Some(x)
}
