use crate::package::{dir_of, resolve, Package};
use crate::xml::{self, Node};

fn node_str(n: &Node) -> String {
    let mut attrs: Vec<String> = n
        .attrs
        .iter()
        .filter(|(k, _)| k != "uid")
        .map(|(k, v)| format!("{k}={v}"))
        .collect();
    attrs.sort();
    let mut s = format!("{}[{}]", n.name, attrs.join(","));
    let t = n.text.trim();
    if !t.is_empty() {
        s.push_str(&format!("={t}"));
    }
    if !n.kids.is_empty() {
        let kids: Vec<String> = n
            .kids
            .iter()
            .filter(|k| k.name != "extLst")
            .map(node_str)
            .collect();
        s.push_str(&format!("({})", kids.join(";")));
    }
    s
}

fn read(pkg: &Package, name: &str) -> Option<Node> {
    xml::parse(pkg.part(name)?).ok()
}

fn sqref(s: Option<&str>) -> String {
    let mut v: Vec<&str> = s.unwrap_or("").split_whitespace().collect();
    v.sort_unstable();
    v.join(" ")
}

fn flag(n: &Node, k: &str) -> bool {
    matches!(n.attr(k), Some("1" | "true"))
}

fn file_name(t: &str) -> String {
    t.rsplit(['/', '\\']).next().unwrap_or(t).to_string()
}

pub fn extras_summary(pkg: &Package) -> Vec<String> {
    let mut out: Vec<String> = Vec::new();
    let book_part = "xl/workbook.xml";
    let Some(book) = read(pkg, book_part) else {
        return out;
    };
    let rels = pkg.rels(book_part);
    let target = |id: &str| {
        rels.iter()
            .find(|r| r.id == id)
            .map(|r| resolve(dir_of(book_part), &r.target))
    };
    let styles = rels
        .iter()
        .find(|r| r.kind() == "styles")
        .and_then(|r| read(pkg, &resolve(dir_of(book_part), &r.target)));
    let dxfs: Vec<Node> = styles
        .as_ref()
        .and_then(|s| s.child("dxfs"))
        .map(|d| d.children("dxf").cloned().collect())
        .unwrap_or_default();
    for s in book
        .child("sheets")
        .map(|n| n.children("sheet").collect::<Vec<_>>())
        .unwrap_or_default()
    {
        let name = s.attr("name").unwrap_or("");
        let Some(part) = s.attr("id").and_then(target) else {
            continue;
        };
        let Some(root) = read(pkg, &part) else {
            continue;
        };
        let srels = pkg.rels(&part);
        let mut lines: Vec<String> = Vec::new();
        if let Some(h) = root.child("hyperlinks") {
            for l in h.children("hyperlink") {
                let t = l
                    .attr("id")
                    .and_then(|id| srels.iter().find(|r| r.id == id))
                    .map(|r| r.target.replace('\\', "/"))
                    .unwrap_or_default();
                lines.push(format!(
                    "hl {} target={t} loc={} tip={} disp={}",
                    l.attr("ref").unwrap_or(""),
                    l.attr("location").unwrap_or(""),
                    l.attr("tooltip").unwrap_or(""),
                    l.attr("display").unwrap_or("")
                ));
            }
        }
        if let Some(d) = root.child("dataValidations") {
            for v in d.children("dataValidation") {
                let ty = v.attr("type").unwrap_or("none");
                let op = if matches!(ty, "list" | "custom" | "none") {
                    ""
                } else {
                    v.attr("operator").unwrap_or("between")
                };
                lines.push(format!(
                    "dv {} type={ty} op={op} style={} blank={} drop={} inmsg={} errmsg={} pt={} p={} et={} e={} f1={} f2={}",
                    sqref(v.attr("sqref")),
                    v.attr("errorStyle").unwrap_or("stop"),
                    flag(v, "allowBlank"),
                    flag(v, "showDropDown"),
                    flag(v, "showInputMessage"),
                    flag(v, "showErrorMessage"),
                    v.attr("promptTitle").unwrap_or(""),
                    v.attr("prompt").unwrap_or(""),
                    v.attr("errorTitle").unwrap_or(""),
                    v.attr("error").unwrap_or(""),
                    v.child("formula1").map_or("", |f| f.text.trim()),
                    v.child("formula2").map_or("", |f| f.text.trim())
                ));
            }
        }
        for cf in root.children("conditionalFormatting") {
            for r in cf.children("cfRule") {
                let dxf = r
                    .attr("dxfId")
                    .and_then(|i| i.parse::<usize>().ok())
                    .and_then(|i| dxfs.get(i))
                    .map(|d| {
                        let mut d = d.clone();
                        for k in d.kids.iter_mut().filter(|k| k.name == "numFmt") {
                            if k.attr("numFmtId").and_then(|v| v.parse::<u32>().ok()) >= Some(164) {
                                k.attrs.retain(|(n, _)| n != "numFmtId");
                            }
                        }
                        node_str(&d)
                    })
                    .unwrap_or_default();
                let mut rule = r.clone();
                rule.attrs.retain(|(k, _)| k != "dxfId");
                lines.push(format!(
                    "cf {} {} dxf={dxf}",
                    sqref(cf.attr("sqref")),
                    node_str(&rule)
                ));
            }
        }
        for rel in srels.iter().filter(|r| r.kind() == "comments") {
            if let Some(c) = read(pkg, &resolve(dir_of(&part), &rel.target)) {
                let authors: Vec<String> = c
                    .child("authors")
                    .map(|a| a.children("author").map(|x| x.text.clone()).collect())
                    .unwrap_or_default();
                if let Some(list) = c.child("commentList") {
                    for n in list.children("comment") {
                        let a = n
                            .attr("authorId")
                            .and_then(|i| i.parse::<usize>().ok())
                            .and_then(|i| authors.get(i))
                            .cloned()
                            .unwrap_or_default();
                        let text = n.child("text").map(crate::si_text).unwrap_or_default();
                        lines.push(format!(
                            "note {} author={a} text={}",
                            n.attr("ref").unwrap_or(""),
                            text.replace('\n', "\\n")
                        ));
                    }
                }
            }
        }
        lines.sort();
        for l in lines {
            out.push(format!("{name}: {l}"));
        }
    }
    if let Some(refs) = book.child("externalReferences") {
        for (i, r) in refs.children("externalReference").enumerate() {
            let Some(p) = r.attr("id").and_then(target) else {
                continue;
            };
            let Some(root) = read(pkg, &p) else { continue };
            let Some(b) = root.child("externalBook") else {
                continue;
            };
            let lrels = pkg.rels(&p);
            let file = b
                .attr("id")
                .and_then(|id| lrels.iter().find(|r| r.id == id))
                .map(|r| file_name(&r.target))
                .unwrap_or_default();
            let sheets: Vec<&str> = b
                .child("sheetNames")
                .map(|s| {
                    s.children("sheetName")
                        .filter_map(|n| n.attr("val"))
                        .collect()
                })
                .unwrap_or_default();
            out.push(format!(
                "ext {} file={file} sheets={}",
                i + 1,
                sheets.join(",")
            ));
            if let Some(names) = b.child("definedNames") {
                for n in names.children("definedName") {
                    out.push(format!(
                        "ext {} name {}={}",
                        i + 1,
                        n.attr("name").unwrap_or(""),
                        n.attr("refersTo").unwrap_or("")
                    ));
                }
            }
            if let Some(ds) = b.child("sheetDataSet") {
                for sd in ds.children("sheetData") {
                    let id = sd.attr("sheetId").unwrap_or("");
                    let mut cells: Vec<String> = Vec::new();
                    for row in sd.children("row") {
                        for c in row.children("cell") {
                            cells.push(format!(
                                "{}:{}:{}",
                                c.attr("r").unwrap_or(""),
                                c.attr("t").unwrap_or("n"),
                                c.child("v").map_or("", |v| v.text.as_str())
                            ));
                        }
                    }
                    cells.sort();
                    out.push(format!(
                        "ext {} sheet {id} cells {}",
                        i + 1,
                        cells.join(" ")
                    ));
                }
            }
        }
    }

    if let Some(core) = read(pkg, "docProps/core.xml") {
        for k in [
            "title",
            "subject",
            "creator",
            "keywords",
            "description",
            "category",
        ] {
            if let Some(n) = core.child(k).filter(|n| !n.text.is_empty()) {
                out.push(format!("prop {k}={}", n.text));
            }
        }
    }
    if let Some(app) = read(pkg, "docProps/app.xml") {
        for k in ["Manager", "Company"] {
            if let Some(n) = app.child(k).filter(|n| !n.text.is_empty()) {
                out.push(format!("prop {k}={}", n.text));
            }
        }
    }
    if let Some(custom) = read(pkg, "docProps/custom.xml") {
        let mut v: Vec<String> = custom
            .children("property")
            .map(|p| {
                let val = p
                    .kids
                    .first()
                    .map(|k| format!("{}={}", k.name, k.text))
                    .unwrap_or_default();
                format!("custom {}={val}", p.attr("name").unwrap_or(""))
            })
            .collect();
        v.sort();
        out.extend(v);
    }
    out
}
