use crate::xml::Node;
use xlcalc::{Rect, SheetId, Workbook};

pub(crate) fn read(sheet: &Node) -> Vec<Rect> {
    let Some(m) = sheet.child("mergeCells") else {
        return Vec::new();
    };
    m.children("mergeCell")
        .filter_map(|c| crate::parse_rect(c.attr("ref")?))
        .collect()
}

pub(crate) fn apply(wb: &mut Workbook, sheet: SheetId, areas: &[Rect]) {
    for r in areas {
        wb.import_merge(sheet, *r);
    }
}

pub(crate) fn write(wb: &Workbook, sheet: SheetId) -> Option<String> {
    let areas = wb.merges(sheet);
    if areas.is_empty() {
        return None;
    }
    let mut x = format!("<mergeCells count=\"{}\">", areas.len());
    for r in &areas {
        x.push_str(&format!(
            "<mergeCell ref=\"{}\"/>",
            crate::write::rect_text(r)
        ));
    }
    x.push_str("</mergeCells>");
    Some(x)
}
