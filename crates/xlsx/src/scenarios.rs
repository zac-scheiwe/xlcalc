use crate::rawxml::escape_attr;
use crate::xml::Node;
use xlcalc::scenarios::{Scenario, ScenarioCell, SheetScenarios};

pub(crate) fn read(sheet: &Node) -> Option<SheetScenarios> {
    let n = sheet.child("scenarios")?;
    let flag = |s: &Node, k: &str| matches!(s.attr(k), Some("1" | "true"));
    let list: Vec<Scenario> = n
        .children("scenario")
        .map(|s| Scenario {
            name: s.attr("name").unwrap_or("").to_string(),
            locked: flag(s, "locked"),
            hidden: flag(s, "hidden"),
            user: s.attr("user").unwrap_or("").to_string(),
            comment: s.attr("comment").unwrap_or("").to_string(),
            cells: s
                .children("inputCells")
                .filter(|c| !flag(c, "deleted"))
                .filter_map(|c| {
                    let (row, col) = crate::parse_cell(c.attr("r")?)?;
                    Some(ScenarioCell {
                        row,
                        col,
                        value: c.attr("val").unwrap_or("").to_string(),
                    })
                })
                .collect(),
            areas: Vec::new(),
        })
        .collect();
    let index = |k: &str| n.attr(k).and_then(|v| v.parse().ok());
    Some(SheetScenarios {
        list,
        current: index("current"),
        show: index("show"),
        sqref: n.attr("sqref").map(str::to_string),
    })
}

pub(crate) fn write(s: Option<&SheetScenarios>) -> Option<String> {
    let s = s.filter(|s| !s.list.is_empty())?;
    let mut o = String::from("<scenarios");
    for (k, v) in [("current", s.current), ("show", s.show)] {
        if let Some(v) = v {
            o.push_str(&format!(" {k}=\"{v}\""));
        }
    }
    if let Some(q) = &s.sqref {
        o.push_str(&format!(" sqref=\"{}\"", escape_attr(q)));
    }
    o.push('>');
    for sc in &s.list {
        o.push_str(&format!("<scenario name=\"{}\"", escape_attr(&sc.name)));
        if sc.locked {
            o.push_str(" locked=\"1\"");
        }
        if sc.hidden {
            o.push_str(" hidden=\"1\"");
        }
        o.push_str(&format!(" count=\"{}\"", sc.cells.len()));
        if !sc.user.is_empty() {
            o.push_str(&format!(" user=\"{}\"", escape_attr(&sc.user)));
        }
        if !sc.comment.is_empty() {
            o.push_str(&format!(" comment=\"{}\"", escape_attr(&sc.comment)));
        }
        o.push('>');
        for c in &sc.cells {
            o.push_str(&format!(
                "<inputCells r=\"{}{}\" val=\"{}\"/>",
                xlcalc::col_to_letters(c.col),
                c.row + 1,
                escape_attr(&c.value)
            ));
        }
        o.push_str("</scenario>");
    }
    o.push_str("</scenarios>");
    Some(o)
}
