use crate::package::{write_rels, Rel};
use crate::rawxml::{escape_attr, escape_text, DECL};
use xlcalc::{col_to_letters, ExtLink, ExtName, ExtSheetData, Value};

pub struct LinkPart {
    pub xml: Vec<u8>,
    pub rels: Vec<u8>,
}

fn cell_xml(r: u32, c: u32, v: &Value) -> Option<String> {
    let at = format!("{}{}", col_to_letters(c), r + 1);
    let (t, text) = match v {
        Value::Number(n) => (None, crate::write::number_text(*n)),
        Value::Bool(b) => (Some("b"), if *b { "1" } else { "0" }.to_string()),
        Value::Error(e) => (Some("e"), e.to_string()),
        Value::Text(s) => (Some("str"), s.to_string()),
        _ => return None,
    };
    let t = t.map(|t| format!(" t=\"{t}\"")).unwrap_or_default();
    Some(format!(
        "<cell r=\"{at}\"{t}><v>{}</v></cell>",
        escape_text(&text)
    ))
}

fn enc(s: &str) -> String {
    s.replace('%', "%25").replace(' ', "%20")
}

fn parts_of(p: &str) -> Vec<&str> {
    p.split(['\\', '/']).filter(|s| !s.is_empty()).collect()
}

fn targets(link: &ExtLink, dir: &str) -> (String, Option<String>, Option<String>) {
    if link.dir().is_empty() {
        return (enc(&link.path), None, None);
    }
    let abs = format!("file:///{}", enc(&link.path));
    if dir.is_empty() {
        return (abs, None, None);
    }
    let (src, here) = (parts_of(&link.path), parts_of(dir));
    let same = |a: &str, b: &str| a.eq_ignore_ascii_case(b);
    let common = src
        .iter()
        .zip(&here)
        .take_while(|(a, b)| same(a, b))
        .count();
    if common == 0 {
        return (abs, None, None);
    }
    let rel = |up: usize| {
        let mut v: Vec<&str> = vec![".."; up];
        v.extend(&src[common..]);
        enc(&v.join("/"))
    };
    if common == here.len() {
        return (rel(0), Some(abs), None);
    }

    let drive = src[0].ends_with(':');
    let rooted: Vec<&str> = if drive {
        src[1..].to_vec()
    } else {
        src.clone()
    };
    (
        enc(&format!("/{}", rooted.join("/"))),
        Some(abs),
        Some(rel(here.len() - common)),
    )
}

fn refers_to(n: &ExtName) -> String {
    let plain = n.refers_to();
    match plain.strip_prefix(&format!("={}!", n.sheet)) {
        Some(cells) if !n.sheet.is_empty() => {
            format!("='{}'!{cells}", n.sheet.replace('\'', "''"))
        }
        _ => plain,
    }
}

pub fn link_part(
    link: &ExtLink,
    sheets: &[ExtSheetData],
    names: &[ExtName],
    dir: &str,
) -> LinkPart {
    let (main, abs, relative) = targets(link, dir);
    let mut x = String::from(DECL);
    x.push_str(
        "<externalLink xmlns=\"http://schemas.openxmlformats.org/spreadsheetml/2006/main\" xmlns:mc=\"http://schemas.openxmlformats.org/markup-compatibility/2006\" mc:Ignorable=\"x14 xxl21\" xmlns:x14=\"http://schemas.microsoft.com/office/spreadsheetml/2009/9/main\" xmlns:xxl21=\"http://schemas.microsoft.com/office/spreadsheetml/2021/extlinks2021\">",
    );
    x.push_str("<externalBook xmlns:r=\"http://schemas.openxmlformats.org/officeDocument/2006/relationships\" r:id=\"rId1\">");
    if abs.is_some() {
        x.push_str("<xxl21:alternateUrls><xxl21:absoluteUrl r:id=\"rId2\"/>");
        if relative.is_some() {
            x.push_str("<xxl21:relativeUrl r:id=\"rId3\"/>");
        }
        x.push_str("</xxl21:alternateUrls>");
    }
    x.push_str("<sheetNames>");
    for s in sheets {
        x.push_str(&format!("<sheetName val=\"{}\"/>", escape_attr(&s.name)));
    }
    x.push_str("</sheetNames>");
    if !names.is_empty() {
        x.push_str("<definedNames>");
        for n in names {
            x.push_str(&format!(
                "<definedName name=\"{}\" refersTo=\"{}\"/>",
                escape_attr(&n.name),
                escape_attr(&refers_to(n))
            ));
        }
        x.push_str("</definedNames>");
    }
    x.push_str("<sheetDataSet>");
    for (i, s) in sheets.iter().enumerate() {
        let mut cells = s.cells.clone();
        cells.sort_by_key(|c| (c.0, c.1));
        x.push_str(&format!("<sheetData sheetId=\"{i}\">"));
        let mut row = None;
        for (r, c, v) in &cells {
            let Some(cx) = cell_xml(*r, *c, v) else {
                continue;
            };
            if row != Some(*r) {
                if row.is_some() {
                    x.push_str("</row>");
                }
                x.push_str(&format!("<row r=\"{}\">", r + 1));
                row = Some(*r);
            }
            x.push_str(&cx);
        }
        if row.is_some() {
            x.push_str("</row>");
        }
        x.push_str("</sheetData>");
    }
    x.push_str("</sheetDataSet></externalBook></externalLink>");
    let ty = "http://schemas.openxmlformats.org/officeDocument/2006/relationships/externalLinkPath";
    let mk = |id: &str, target: String| Rel {
        id: id.into(),
        ty: ty.into(),
        target,
        external: true,
    };
    let mut rels = vec![mk("rId1", main)];
    if let Some(a) = abs {
        rels.push(mk("rId2", a));
    }
    if let Some(r) = relative {
        rels.push(mk("rId3", r));
    }

    rels.reverse();
    LinkPart {
        xml: x.into_bytes(),
        rels: write_rels(&rels),
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    fn link(p: &str) -> ExtLink {
        ExtLink {
            path: p.into(),
            open: false,
            broken: false,
        }
    }

    #[test]
    fn targets_follow_excel() {
        let dir = r"C:\r\a\dst\\";
        let t = |p: &str| targets(&link(p), dir);
        let abs = |p: &str| Some(format!("file:///{p}"));
        assert_eq!(
            t(r"C:\r\a\dst\s_same.xlsx"),
            ("s_same.xlsx".into(), abs(r"C:\r\a\dst\s_same.xlsx"), None)
        );
        assert_eq!(t(r"C:\r\a\dst\sub\s.xlsx").0, "sub/s.xlsx".to_string());
        assert_eq!(t(r"C:\r\a\dst\s space.xlsx").0, "s%20space.xlsx");
        let up = t(r"C:\r\a\s_up.xlsx");
        assert_eq!(up.0, "/r/a/s_up.xlsx");
        assert_eq!(up.2.as_deref(), Some("../s_up.xlsx"));
        assert_eq!(t(r"C:\r\b\s.xlsx").2.as_deref(), Some("../../b/s.xlsx"));
        assert_eq!(targets(&link("src.xlsx"), dir).0, "src.xlsx");
        assert_eq!(
            targets(&link(r"C:\x\src.xlsx"), "").0,
            r"file:///C:\x\src.xlsx"
        );
    }
}
