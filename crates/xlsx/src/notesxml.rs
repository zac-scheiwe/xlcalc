use crate::xml::Node;
use xlcalc::{CellAddr, SheetId, Workbook};

#[derive(Clone, Debug, PartialEq, Eq)]
pub(crate) struct NoteDef {
    pub row: u32,
    pub col: u32,
    pub author: String,
    pub text: String,
    pub visible: bool,

    pub spid: u32,
}

fn cell_of(r: &str) -> Option<(u32, u32)> {
    let rect = crate::parse_rect(r)?;
    Some((rect.r0, rect.c0))
}

pub(crate) fn parse(comments: &Node, vml: Option<&str>) -> Vec<NoteDef> {
    let authors: Vec<String> = comments
        .child("authors")
        .map(|a| a.children("author").map(|x| x.text.clone()).collect())
        .unwrap_or_default();
    let shown = vml.map(shown_cells).unwrap_or_default();
    let ids = vml.map(shape_ids).unwrap_or_default();
    let mut out = Vec::new();
    if let Some(list) = comments.child("commentList") {
        for n in list.children("comment") {
            let Some((row, col)) = n.attr("ref").and_then(cell_of) else {
                continue;
            };
            let author = n
                .attr("authorId")
                .and_then(|i| i.parse::<usize>().ok())
                .and_then(|i| authors.get(i))
                .cloned()
                .unwrap_or_default();
            out.push(NoteDef {
                row,
                col,
                author,
                text: n.child("text").map(crate::si_text).unwrap_or_default(),
                visible: shown.contains(&(row, col)),
                spid: ids.iter().find(|i| i.0 == (row, col)).map_or(0, |i| i.1),
            });
        }
    }
    out.sort_by_key(|n| (n.row, n.col));
    out
}

fn shown_cells(vml: &str) -> Vec<(u32, u32)> {
    let mut out = Vec::new();
    for part in vml.split("<x:ClientData").skip(1) {
        let body = part.split("</x:ClientData>").next().unwrap_or("");
        if !body.contains("ObjectType=\"Note\"") {
            continue;
        }
        let get = |tag: &str| -> Option<u32> {
            let open = format!("<x:{tag}>");
            let at = body.find(&open)? + open.len();
            body[at..].split('<').next()?.trim().parse().ok()
        };
        let visible = body.find("<x:Visible").is_some_and(|i| {
            let rest = &body[i..];
            !rest.starts_with("<x:Visible>False") && !rest.starts_with("<x:Visible>false")
        });
        if let (true, Some(r), Some(c)) = (visible, get("Row"), get("Column")) {
            out.push((r, c));
        }
    }
    out
}

fn shape_ids(vml: &str) -> Vec<((u32, u32), u32)> {
    let mut out = Vec::new();
    for part in vml.split("<v:shape ").skip(1) {
        let body = part.split("</v:shape>").next().unwrap_or("");
        if !body.contains("ObjectType=\"Note\"") {
            continue;
        }
        let head = body.split('>').next().unwrap_or("");
        let id = ["spid=\"_x0000_s", "id=\"_x0000_s"].iter().find_map(|key| {
            head.split(key)
                .nth(1)
                .and_then(|r| r.split('"').next())
                .and_then(|n| n.parse().ok())
        });
        let get = |tag: &str| -> Option<u32> {
            let open = format!("<x:{tag}>");
            let at = body.find(&open)? + open.len();
            body[at..].split('<').next()?.trim().parse().ok()
        };
        if let (Some(id), Some(r), Some(c)) = (id, get("Row"), get("Column")) {
            out.push(((r, c), id));
        }
    }
    out
}

pub(crate) fn from_model(wb: &Workbook, sheet: SheetId) -> Vec<NoteDef> {
    wb.notes(sheet)
        .map(|((row, col), n)| NoteDef {
            row,
            col,
            spid: n.spid,
            author: n.author.clone(),
            text: n.text.clone(),
            visible: n.visible,
        })
        .collect()
}

pub(crate) fn apply(wb: &mut Workbook, sheet: SheetId, defs: &[NoteDef]) {
    for d in defs {
        wb.note_load(
            CellAddr::new(sheet, d.row, d.col),
            &d.text,
            &d.author,
            d.visible,
            d.spid,
        );
    }
}

fn esc(s: &str) -> String {
    s.replace('&', "&amp;")
        .replace('<', "&lt;")
        .replace('>', "&gt;")
}

fn cell_ref(row: u32, col: u32) -> String {
    crate::write::rect_text(&xlcalc::Rect::cell(row, col))
}

pub(crate) fn comments_xml(notes: &[NoteDef]) -> String {
    let mut authors: Vec<&str> = Vec::new();
    let mut list = String::new();
    for n in notes {
        let ai = authors
            .iter()
            .position(|a| *a == n.author)
            .unwrap_or_else(|| {
                authors.push(&n.author);
                authors.len() - 1
            });
        list.push_str(&format!(
            "<comment ref=\"{}\" authorId=\"{ai}\"><text><t xml:space=\"preserve\">{}</t></text></comment>",
            cell_ref(n.row, n.col),
            esc(&n.text)
        ));
    }
    let mut x = String::from(
        "<?xml version=\"1.0\" encoding=\"UTF-8\" standalone=\"yes\"?>\n<comments xmlns=\"http://schemas.openxmlformats.org/spreadsheetml/2006/main\"><authors>",
    );
    for a in authors {
        x.push_str(&format!("<author>{}</author>", esc(a)));
    }
    x.push_str(&format!(
        "</authors><commentList>{list}</commentList></comments>"
    ));
    x
}

const VML_HEAD: &str = "<xml xmlns:v=\"urn:schemas-microsoft-com:vml\" xmlns:o=\"urn:schemas-microsoft-com:office:office\" xmlns:x=\"urn:schemas-microsoft-com:office:excel\">";
const VML_TYPE: &str = "<v:shapetype id=\"_x0000_t202\" coordsize=\"21600,21600\" o:spt=\"202\" path=\"m,l,21600r21600,l21600,xe\"><v:stroke joinstyle=\"miter\"/><v:path gradientshapeok=\"t\" o:connecttype=\"rect\"/></v:shapetype>";

fn shapes_xml(notes: &[NoteDef], map: u32) -> String {
    let mut vml = String::new();
    for (i, n) in notes.iter().enumerate() {
        let (c0, r0) = (n.col + 1, n.row.saturating_sub(1));
        vml.push_str(&format!(
            "<v:shape id=\"_x0000_s{}\" type=\"#_x0000_t202\" style=\"position:absolute;margin-left:80pt;margin-top:2pt;width:108pt;height:59pt;z-index:{};visibility:{}\" fillcolor=\"#ffffe1\" o:insetmode=\"auto\"><v:fill color2=\"#ffffe1\"/><v:shadow on=\"t\" color=\"black\" obscured=\"t\"/><v:path o:connecttype=\"none\"/><v:textbox style=\"mso-direction-alt:auto\"><div style=\"text-align:left\"></div></v:textbox><x:ClientData ObjectType=\"Note\"><x:MoveWithCells/><x:SizeWithCells/><x:Anchor>{}, 15, {}, 10, {}, 15, {}, 16</x:Anchor><x:AutoFill>False</x:AutoFill><x:Row>{}</x:Row><x:Column>{}</x:Column>{}</x:ClientData></v:shape>",
            if n.spid == 0 {
                map * 1024 + 1 + i as u32
            } else {
                n.spid
            },
            i + 1,
            if n.visible { "visible" } else { "hidden" },
            c0,
            r0,
            c0 + 2,
            r0 + 4,
            n.row,
            n.col,
            if n.visible { "<x:Visible/>" } else { "" }
        ));
    }
    vml
}

pub(crate) fn idmap(listed: &str, ids: impl IntoIterator<Item = u32>) -> String {
    let mut v: Vec<u32> = listed
        .split(',')
        .filter_map(|x| x.trim().parse().ok())
        .collect();
    v.extend(xlcalc::spid::clusters(ids));
    v.sort_unstable();
    v.dedup();
    let v: Vec<String> = v.iter().map(u32::to_string).collect();
    v.join(",")
}

pub(crate) fn vml_xml(notes: &[NoteDef], map: u32) -> String {
    let mut data = idmap("", notes.iter().map(|n| n.spid).filter(|i| *i != 0));
    if data.is_empty() {
        data = map.to_string();
    }
    format!(
        "{VML_HEAD}<o:shapelayout v:ext=\"edit\"><o:idmap v:ext=\"edit\" data=\"{data}\"/></o:shapelayout>{VML_TYPE}{}</xml>",
        shapes_xml(notes, map)
    )
}

pub(crate) fn merge_vml(existing: Option<&str>, notes: &[NoteDef], map: u32) -> Option<String> {
    let Some(text) = existing else {
        return (!notes.is_empty()).then(|| vml_xml(notes, map));
    };
    let mut kept = String::new();
    let mut rest = text;
    while let Some(i) = rest.find("<v:shape ") {
        let Some(len) = rest[i..].find("</v:shape>") else {
            break;
        };
        let end = i + len + "</v:shape>".len();
        kept.push_str(&rest[..i]);
        if !rest[i..end].contains("ObjectType=\"Note\"") {
            kept.push_str(&rest[i..end]);
        }
        rest = &rest[end..];
    }
    kept.push_str(rest);
    if notes.is_empty() {
        return kept.contains("<v:shape ").then_some(kept);
    }
    let listed: Option<String> = kept
        .split("<o:idmap")
        .nth(1)
        .and_then(|t| t.split("data=\"").nth(1))
        .and_then(|t| t.split('"').next())
        .map(str::to_string);

    let free = listed
        .iter()
        .flat_map(|d| d.split(',').filter_map(|x| x.trim().parse::<u32>().ok()))
        .max()
        .map_or(map, |m| (m + 1).max(map));
    let shapes = shapes_xml(notes, free);
    let at = kept.rfind("</xml>")?;
    kept.insert_str(at, &shapes);
    if let Some(old) = listed {
        let own = notes.iter().map(|n| n.spid).filter(|i| *i != 0);
        let new = idmap(&old, own);
        kept = kept.replacen(&format!("data=\"{old}\""), &format!("data=\"{new}\""), 1);
    }
    Some(kept)
}
