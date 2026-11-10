use crate::xml::Node;
use xlcalc::{ExtName, ExtSheetData, Value, XlError};

#[derive(Clone, Debug, Default)]
pub struct FileLink {
    pub target: String,

    pub absolute: Option<String>,

    pub missing: bool,

    pub rooted: bool,

    pub sheets: Vec<ExtSheetData>,

    pub names: Vec<ExtName>,

    pub part: String,
}

impl FileLink {
    pub fn key(&self) -> String {
        self.target.clone()
    }
}

fn unfile(t: &str) -> String {
    let t = t.strip_prefix("file:///").unwrap_or(t);
    t.replace('/', "\\").replace("%20", " ").replace("%25", "%")
}

pub fn read_link(part: &str, root: &Node, rels: &[(String, String, String)]) -> Option<FileLink> {
    let book = root.child("externalBook")?;
    let rel = |id: &str| rels.iter().find(|r| r.0 == id);
    let main = book.attr("id").and_then(rel)?;
    let mut link = FileLink {
        target: main.2.clone(),
        missing: main.1.ends_with("xlPathMissing"),
        part: part.to_string(),
        ..Default::default()
    };
    if link.target.contains(':') || link.target.starts_with("file:") {
        link.rooted = true;
        link.target = unfile(&link.target);
    } else {
        link.target = link.target.replace("%20", " ").replace("%25", "%");
    }

    if let Some(alt) = book.child("alternateUrls") {
        if let Some(a) = alt.child("absoluteUrl").and_then(|a| a.attr("id")) {
            link.absolute = rel(a).map(|r| unfile(&r.2));
        }
    }

    if link.target.starts_with('\\') {
        if let Some(a) = link.absolute.clone() {
            link.target = a;
            link.rooted = true;
        }
    }
    let names: Vec<String> = book
        .child("sheetNames")
        .map(|s| {
            s.children("sheetName")
                .map(|n| n.attr("val").unwrap_or("").to_string())
                .collect()
        })
        .unwrap_or_default();
    let mut sheets: Vec<ExtSheetData> = names
        .iter()
        .map(|n| ExtSheetData {
            name: n.clone(),
            cells: Vec::new(),
        })
        .collect();
    if let Some(set) = book.child("sheetDataSet") {
        for sd in set.children("sheetData") {
            let Some(i) = sd.attr("sheetId").and_then(|s| s.parse::<usize>().ok()) else {
                continue;
            };
            let Some(sh) = sheets.get_mut(i) else {
                continue;
            };
            for row in sd.children("row") {
                for c in row.children("cell") {
                    let Some((r, col)) = c.attr("r").and_then(crate::parse_cell) else {
                        continue;
                    };
                    let v = c.child("v").map(|v| v.text.as_str()).unwrap_or("");
                    let val = match c.attr("t").unwrap_or("n") {
                        "str" => Value::from(crate::ftext::unescape_xstring(v).as_str()),
                        "b" => Value::Bool(matches!(v.trim(), "1" | "true")),
                        "e" => Value::Error(XlError::parse(v.trim()).unwrap_or(XlError::VALUE)),
                        _ => match crate::file_number(v) {
                            Some(x) => x,
                            None => continue,
                        },
                    };
                    sh.cells.push((r, col, val));
                }
            }
        }
    }
    link.names = book
        .child("definedNames")
        .map(|d| {
            d.children("definedName")
                .filter_map(|n| ExtName::parse(n.attr("name")?, n.attr("refersTo")?))
                .collect()
        })
        .unwrap_or_default();
    link.sheets = sheets;
    Some(link)
}
