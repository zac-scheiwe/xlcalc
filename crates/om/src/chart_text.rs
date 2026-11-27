use xlcalc::drawing::{NameLink, NameRead, Part};
use xlcalc::{parse_range_ref, CellAddr, Rect, SheetId, Value, Workbook};
use xlcalc_variant::Variant;

use crate::addr::{parse_range_text, quote_sheet, range_text};
use crate::charts::{
    abs_style, absolute_r1c1, cell_variant, number_cell_text, parsed_string, part_text, r1c1_text,
};

fn head_name(head: &str) -> Option<String> {
    match head.strip_prefix('\'') {
        Some(rest) => {
            let inner = rest.strip_suffix('\'')?;
            (!inner.replace("''", "").contains('\'')).then(|| inner.replace("''", "'"))
        }
        None => (quote_sheet(head) == head).then(|| head.to_string()),
    }
}

pub(crate) fn name_link(wb: &Workbook, home: SheetId, spec: &str) -> Option<NameLink> {
    let (head, tail) = spec.trim().rsplit_once('!')?;
    let head = head_name(head)?;
    let from = wb
        .sheet_by_name(&head)
        .map(|s| s.id)
        .or_else(|| head.eq_ignore_ascii_case(wb.book_name()).then_some(home))?;
    let def = wb.names().lookup(tail, from)?;
    Some(NameLink {
        scope: def.scope,
        name: def.name.clone(),
    })
}

pub(crate) fn two_d(v: &[(SheetId, Rect)]) -> bool {
    v.iter().any(|(_, r)| r.rows() > 1 && r.cols() > 1)
}

pub(crate) fn name_refused(wb: &Workbook, n: &NameLink, categories: bool) -> bool {
    match wb.read_name_link(n) {
        NameRead::Refs(v) => !categories && two_d(&v),
        NameRead::Value(v) => matches!(v, Value::Error(_)),
        NameRead::Undefined => true,
    }
}

pub(crate) fn parse_link(wb: &Workbook, home: SheetId, body: &str) -> Option<Part> {
    let body = body.trim();
    if body.is_empty() {
        return Some(Part::Text("=".into()));
    }
    if body.starts_with('"') {
        return parsed_string(body).map(Part::Text);
    }
    let (head, tail) = match body.strip_suffix("!#REF!") {
        Some(head) => (head, "#REF!"),
        None => body.rsplit_once('!')?,
    };
    let name = head_name(head)?;
    if tail == "#REF!" {
        return Some(Part::External(format!("{head}!{tail}")));
    }
    if let Some(n) = name_link(wb, home, body) {
        return (!name_refused(wb, &n, false)).then_some(Part::Name(n));
    }
    if let Some(sheet) = wb.sheet_by_name(&name).map(|s| s.id) {
        let (s, r) = r1c1_text(wb, sheet, body).or_else(|| parse_range_text(wb, sheet, body))?;
        return Some(Part::Refs(vec![(s, r)]));
    }
    if name.eq_ignore_ascii_case(wb.book_name()) {
        return None;
    }
    let first = wb.sheets().next()?.id;

    let (_, rect) = parse_range_ref(wb, tail, first)?;
    Some(Part::External(format!(
        "{head}!{}",
        range_text(rect, &abs_style())
    )))
}

pub(crate) fn link_text(wb: &Workbook, p: &Part) -> String {
    match p {
        Part::Refs(v) => {
            let mut parts = Vec::new();
            for (s, r) in v {
                let Some(used) = wb.used_range(*s) else {
                    continue;
                };
                for row in r.r0.max(used.r0)..=r.r1.min(used.r1) {
                    for col in r.c0.max(used.c0)..=r.c1.min(used.c1) {
                        let text = cell_text(wb, *s, row, col);
                        if !text.is_empty() {
                            parts.push(text);
                        }
                    }
                }
            }
            parts.join(" ")
        }
        Part::Text(t) => t.clone(),
        Part::None => String::new(),
        Part::External(_) | Part::Dead => "#REF!".to_string(),

        Part::Name(n) => match wb.read_name_link(n) {
            NameRead::Refs(v) => {
                let columns = v.first().and_then(|&(s, r)| {
                    let used = wb.used_range(s)?;
                    Some(
                        (r.c0..=r.c1.min(used.c1))
                            .map(|c| (s, Rect::new(r.r0, c, r.r1, c)))
                            .collect(),
                    )
                });
                link_text(wb, &Part::Refs(columns.unwrap_or_default()))
            }
            NameRead::Value(Value::Number(x)) => xlcalc::format_general(x),
            NameRead::Value(Value::Text(t)) => t.to_string(),
            NameRead::Value(Value::Bool(b)) => if b { "TRUE" } else { "FALSE" }.to_string(),
            NameRead::Value(Value::Error(_)) | NameRead::Undefined => "#REF!".to_string(),
            NameRead::Value(_) => String::new(),
        },
    }
}

fn cell_text(wb: &Workbook, s: SheetId, row: u32, col: u32) -> String {
    let addr = CellAddr::new(s, row, col);
    match cell_variant(&wb.value(addr)) {
        Variant::Bstr(t) => t,
        Variant::R8(n) => number_cell_text(wb, addr, n),
        Variant::Empty => String::new(),
        _ => wb.display_text(addr),
    }
}

pub(crate) fn link_formula(wb: &Workbook, p: &Part, r1c1: bool) -> Option<String> {
    let text = match p {
        Part::Refs(_) | Part::Name(_) => {
            xlcalc::stored_formula_text(&format!("={}", part_text(wb, p, false)?))
        }
        Part::External(t) => format!("={t}"),
        Part::None => "=".to_string(),
        Part::Dead => "=#REF!".to_string(),
        Part::Text(_) => return None,
    };
    Some(if r1c1 { absolute_r1c1(&text) } else { text })
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn link_parser_edges() {
        let mut wb = Workbook::empty();
        let s = wb.add_sheet("Sheet1").unwrap();
        let cell = |r: u32, c: u32| Part::Refs(vec![(s, xlcalc::Rect::new(r, c, r, c))]);
        let text = |t: &str| Part::Text(t.into());
        for (body, link) in [
            ("Sheet1!$D$1", Some(cell(0, 3))),
            (" sheet1!d1 ", Some(cell(0, 3))),
            ("'Sheet1'!R1C4", Some(cell(0, 3))),
            ("\"a\"\"b\"", Some(text("a\"b"))),
            ("\"a\"b\"", None),
            ("", Some(text("="))),
            ("Nope!A1", Some(Part::External("Nope!$A$1".into()))),
            ("Sheet1!#REF!", Some(Part::External("Sheet1!#REF!".into()))),
            ("Nope!x+1", None),
            ("Sheet1!Nope", None),
            ("Book1!Nope", None),
            ("$D$1", None),
            ("Sheet1!$D$1+1", None),
            ("SUM(Sheet1!$D$1)", None),
            ("(Sheet1!$D$1,Sheet1!$D$2)", None),
            ("Sheet1!$D$1,Sheet1!$D$2", None),
            ("{\"a\"}", None),
            ("5", None),
        ] {
            assert_eq!(parse_link(&wb, s, body), link, "{body}");
        }
    }

    #[test]
    fn name_links_keep_the_name() {
        let mut wb = Workbook::empty();
        let s = wb.add_sheet("Sheet1").unwrap();
        for (r, c, v) in [(0, 1, "Head"), (1, 0, "x"), (1, 1, "1")] {
            wb.set_value(CellAddr::new(s, r, c), Value::Text(v.into()));
        }
        wb.define_name("Block", "=Sheet1!$B$1", None).unwrap();
        wb.define_name("Loc", "=Sheet1!$B$1", Some(s)).unwrap();
        let link = |wb: &Workbook, body: &str| parse_link(wb, s, body).unwrap();
        let block = link(&wb, "Book1!block");
        assert_eq!(
            link_formula(&wb, &block, false).as_deref(),
            Some("=Book1!Block")
        );

        wb.define_name("Block", "=Sheet1!$A$1:$B$2", None).unwrap();
        assert_eq!(link_text(&wb, &block), "x Head 1");
        let local = link(&wb, "Book1!Loc");
        assert_eq!(
            link_formula(&wb, &local, false).as_deref(),
            Some("=Sheet1!Loc")
        );
        assert!(wb.delete_name("Block", None).is_some());
        assert_eq!(link_text(&wb, &block), "#REF!");
        assert_eq!(
            link_formula(&wb, &block, false).as_deref(),
            Some("=Book1!Block")
        );
    }
}
