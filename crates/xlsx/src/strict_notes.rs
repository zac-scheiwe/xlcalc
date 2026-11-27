use crate::xml::Node;

const MC: &str = "http://schemas.openxmlformats.org/markup-compatibility/2006";
const XDR: &str = "http://schemas.openxmlformats.org/drawingml/2006/spreadsheetDrawing";
const A: &str = "http://schemas.openxmlformats.org/drawingml/2006/main";
const VML: &str = "urn:schemas-microsoft-com:vml";

#[derive(Clone, Debug, PartialEq)]
struct Shape {
    id: u32,
    row: u32,
    col: u32,
    hidden: bool,

    pt: [f64; 4],

    anchor: [i64; 8],
    fill: Option<String>,
    flags: Flags,
}

#[derive(Clone, Debug, PartialEq, Default)]
struct Flags {
    auto_fill: bool,
    lock_text: bool,
    just_last_x: bool,
    auto_scale: bool,
}

fn style_pt(style: &str, key: &str) -> Option<f64> {
    let at = style.find(&format!("{key}:"))? + key.len() + 1;
    let rest = &style[at..];
    let end = rest.find(['p', ';']).unwrap_or(rest.len());
    rest[..end].trim().parse().ok()
}

fn attr_of<'a>(tag: &'a str, name: &str) -> Option<&'a str> {
    let pat = format!(" {name}=");
    let at = tag.find(&pat)? + pat.len();
    let rest = &tag[at..];
    let q = rest.chars().next()?;
    let body = &rest[1..];
    Some(&body[..body.find(q)?])
}

fn inner<'a>(body: &'a str, tag: &str) -> Option<&'a str> {
    let open = format!("<x:{tag}>");
    let at = body.find(&open)? + open.len();
    Some(body[at..].split('<').next()?.trim())
}

fn flag(body: &str, tag: &str, default: bool) -> bool {
    if let Some(v) = inner(body, tag) {
        return !v.eq_ignore_ascii_case("false");
    }
    if body.contains(&format!("<x:{tag}/>")) {
        return true;
    }
    default
}

fn note_shapes(vml: &str) -> Vec<Shape> {
    let mut out = Vec::new();
    for chunk in vml.split("<v:shape ").skip(1) {
        let chunk = chunk.split("</v:shape>").next().unwrap_or("");
        if !chunk.contains("ObjectType=\"Note\"") {
            continue;
        }
        let head = &chunk[..chunk.find('>').unwrap_or(chunk.len())];
        let head = format!(" {head}");
        let id = attr_of(&head, "id")
            .and_then(|i| i.rsplit("_s").next())
            .and_then(|i| i.parse::<u32>().ok());
        let style = attr_of(&head, "style").unwrap_or("");
        let style: String = style.split_whitespace().collect();
        let body = chunk.split("<x:ClientData").nth(1).unwrap_or("");
        let anchor: Vec<i64> = inner(body, "Anchor")
            .map(|a| a.split(',').filter_map(|x| x.trim().parse().ok()).collect())
            .unwrap_or_default();
        let (Some(id), Some(row), Some(col), Ok(anchor)) = (
            id,
            inner(body, "Row").and_then(|v| v.parse().ok()),
            inner(body, "Column").and_then(|v| v.parse().ok()),
            <[i64; 8]>::try_from(anchor),
        ) else {
            continue;
        };
        let (Some(l), Some(t), Some(w), Some(h)) = (
            style_pt(&style, "margin-left"),
            style_pt(&style, "margin-top"),
            style_pt(&style, "width"),
            style_pt(&style, "height"),
        ) else {
            continue;
        };
        let fill = attr_of(&head, "fillcolor")
            .and_then(|f| f.strip_prefix('#'))
            .filter(|f| f.len() == 6 && !f.eq_ignore_ascii_case("ffffe1"))
            .map(str::to_uppercase);
        out.push(Shape {
            id,
            row,
            col,
            hidden: !style.contains("visibility:visible") && !body.contains("<x:Visible"),
            pt: [l, t, w, h],
            anchor,
            fill,
            flags: Flags {
                auto_fill: flag(body, "AutoFill", false),
                lock_text: flag(body, "LockText", true),
                just_last_x: flag(body, "JustLastX", false),
                auto_scale: flag(body, "AutoScale", false),
            },
        });
    }
    out
}

fn emu_pt(pt: f64) -> i64 {
    (pt * 12700.0).round() as i64
}

fn esc(s: &str) -> String {
    s.replace('&', "&amp;")
        .replace('<', "&lt;")
        .replace('>', "&gt;")
        .replace('"', "&quot;")
}

fn anchor_xml(s: &Shape, tag: (&str, &str)) -> String {
    let a = &s.anchor;
    let px = |v: i64| v * 9525;
    format!(
        "<{tag0}><xdr:col>{}</xdr:col><xdr:colOff>{}</xdr:colOff><xdr:row>{}</xdr:row><xdr:rowOff>{}</xdr:rowOff></{tag0}><{tag1}><xdr:col>{}</xdr:col><xdr:colOff>{}</xdr:colOff><xdr:row>{}</xdr:row><xdr:rowOff>{}</xdr:rowOff></{tag1}>",
        a[0],
        px(a[1]),
        a[2],
        px(a[3]),
        a[4],
        px(a[5]),
        a[6],
        px(a[7]),
        tag0 = tag.0,
        tag1 = tag.1,
    )
}

struct Run<'a> {
    props: Option<&'a Node>,
    text: String,
}

fn runs(text: &Node) -> Vec<Run<'_>> {
    let mut out = Vec::new();
    for k in &text.kids {
        match k.name.as_str() {
            "t" => out.push(Run {
                props: None,
                text: crate::ftext::unescape_xstring(&k.text),
            }),
            "r" => out.push(Run {
                props: k.child("rPr"),
                text: k
                    .child("t")
                    .map(|t| crate::ftext::unescape_xstring(&t.text))
                    .unwrap_or_default(),
            }),
            _ => {}
        }
    }
    out
}

fn bool_prop(p: Option<&Node>, name: &str) -> bool {
    p.and_then(|p| p.child(name))
        .is_some_and(|b| !matches!(b.attr("val"), Some("0" | "false")))
}

fn run_pr(p: Option<&Node>) -> String {
    let sz = p
        .and_then(|p| p.child("sz"))
        .and_then(|s| s.attr("val"))
        .and_then(|v| v.parse::<f64>().ok())
        .unwrap_or(9.0);
    let u = match p.and_then(|p| p.child("u")) {
        None => "none",
        Some(u) => match u.attr("val") {
            Some("double") => "dbl",
            Some("singleAccounting") => "sng",
            Some("doubleAccounting") => "dbl",
            _ => "sng",
        },
    };
    let baseline = match p
        .and_then(|p| p.child("vertAlign"))
        .and_then(|v| v.attr("val"))
    {
        Some("superscript") => "30%",
        Some("subscript") => "-25%",
        _ => "0%",
    };
    let color = p
        .and_then(|p| p.child("color"))
        .and_then(|c| c.attr("rgb"))
        .filter(|c| c.len() == 8)
        .map(|c| c[2..].to_uppercase())
        .unwrap_or_else(|| "000000".to_string());
    let face = esc(p
        .and_then(|p| p.child("rFont"))
        .and_then(|f| f.attr("val"))
        .unwrap_or("Tahoma"));
    format!(
        "<a:rPr lang=\"en-US\" sz=\"{}\" b=\"{}\" i=\"{}\" u=\"{u}\" strike=\"{}\" baseline=\"{baseline}\"><a:solidFill><a:srgbClr val=\"{color}\"/></a:solidFill><a:latin typeface=\"{face}\"/><a:ea typeface=\"{face}\"/><a:cs typeface=\"{face}\"/></a:rPr>",
        (sz * 100.0).round() as i64,
        u8::from(bool_prop(p, "b")),
        u8::from(bool_prop(p, "i")),
        if bool_prop(p, "strike") {
            "sngStrike"
        } else {
            "noStrike"
        },
    )
}

fn paragraphs(text: Option<&Node>) -> String {
    let mut paras: Vec<String> = vec![String::new()];
    for r in text.map(runs).unwrap_or_default() {
        let pr = run_pr(r.props);
        for (i, line) in r.text.replace('\r', "").split('\n').enumerate() {
            if i > 0 {
                paras.push(String::new());
            }
            if !line.is_empty() {
                let p = paras.last_mut().unwrap();
                p.push_str(&format!("<a:r>{pr}<a:t>{}</a:t></a:r>", esc(line)));
            }
        }
    }
    paras
        .iter()
        .map(|p| {
            format!("<a:p><a:pPr algn=\"l\" rtl=\"0\"><a:defRPr sz=\"1000\"/></a:pPr>{p}</a:p>")
        })
        .collect()
}

fn shape_block(s: &Shape, text: Option<&Node>, local_ns: bool) -> String {
    let decl = if local_ns {
        format!(" xmlns:xdr=\"{XDR}\" xmlns:a=\"{A}\"")
    } else {
        String::new()
    };
    let fill = match &s.fill {
        Some(c) => format!("<a:srgbClr val=\"{c}\"/>"),
        None => "<a:srgbClr val=\"FFFFE1\" mc:Ignorable=\"a14\" xmlns:a14=\"http://schemas.microsoft.com/office/drawing/2010/main\" a14:legacySpreadsheetColorIndex=\"80\"/>".to_string(),
    };
    format!(
        "<mc:AlternateContent xmlns:mc=\"{MC}\"{decl}><mc:Choice xmlns:v=\"{VML}\" Requires=\"v\"/><mc:Fallback><xdr:twoCellAnchor editAs=\"absolute\">{}<xdr:sp macro=\"\" textlink=\"\"><xdr:nvSpPr><xdr:cNvPr id=\"{}\" name=\"Comment {}\"{}/><xdr:cNvSpPr txBox=\"1\"><a:spLocks noChangeArrowheads=\"1\"/></xdr:cNvSpPr></xdr:nvSpPr><xdr:spPr bwMode=\"auto\"><a:xfrm><a:off x=\"{}\" y=\"{}\"/><a:ext cx=\"{}\" cy=\"{}\"/></a:xfrm><a:prstGeom prst=\"rect\"><a:avLst/></a:prstGeom><a:solidFill>{fill}</a:solidFill><a:ln w=\"9525\"><a:solidFill><a:srgbClr val=\"510000\"/></a:solidFill><a:miter lim=\"800%\"/><a:headEnd/><a:tailEnd/></a:ln><a:effectLst/><a:extLst><a:ext uri=\"{{AF507438-7753-43E0-B8FC-AC1667EBCBE1}}\"><a14:hiddenEffects xmlns:a14=\"http://schemas.microsoft.com/office/drawing/2010/main\"><a:effectLst><a:outerShdw dist=\"35921\" dir=\"2700000\" algn=\"ctr\" rotWithShape=\"0\"><a:srgbClr val=\"510000\"/></a:outerShdw></a:effectLst></a14:hiddenEffects></a:ext><a:ext uri=\"{{53640926-AAD7-44D8-BBD7-CCE9431645EC}}\"><a14:shadowObscured xmlns:a14=\"http://schemas.microsoft.com/office/drawing/2010/main\" val=\"1\"/></a:ext></a:extLst></xdr:spPr><xdr:txBody><a:bodyPr vertOverflow=\"clip\" wrap=\"square\" lIns=\"36576\" tIns=\"27432\" rIns=\"0\" bIns=\"0\" anchor=\"t\" upright=\"1\"/><a:lstStyle/>{}</xdr:txBody></xdr:sp><xdr:clientData/></xdr:twoCellAnchor></mc:Fallback></mc:AlternateContent>",
        anchor_xml(s, ("xdr:from", "xdr:to")),
        s.id,
        s.id % 1024,
        if s.hidden { " hidden=\"1\"" } else { "" },
        emu_pt(s.pt[0]),
        emu_pt(s.pt[1]),
        emu_pt(s.pt[2]),
        emu_pt(s.pt[3]),
        paragraphs(text),
    )
}

fn comment_pr(s: &Shape) -> String {
    let mut attrs = String::from(" defaultSize=\"0\"");
    if !s.flags.auto_fill {
        attrs.push_str(" autoFill=\"0\"");
    }
    attrs.push_str(" autoLine=\"0\"");
    if !s.flags.lock_text {
        attrs.push_str(" lockText=\"0\"");
    }
    if s.flags.just_last_x {
        attrs.push_str(" justLastX=\"1\"");
    }
    if s.flags.auto_scale {
        attrs.push_str(" autoScale=\"1\"");
    }
    format!(
        "<mc:AlternateContent xmlns:mc=\"{MC}\"><mc:Choice Requires=\"v\"/><mc:Fallback><commentPr{attrs}><anchor>{}</anchor></commentPr></mc:Fallback></mc:AlternateContent>",
        anchor_xml(s, ("from", "to")),
    )
}

pub(crate) struct Built {
    pub comments: String,

    pub shapes: Vec<String>,
}

pub(crate) fn build(comments: &str, vml: &str, local_ns: bool) -> Option<Built> {
    let shapes = note_shapes(vml);
    let tree = crate::xml::parse(comments.as_bytes()).ok()?;
    let list: Vec<&Node> = tree.child("commentList")?.children("comment").collect();
    let mut out = String::with_capacity(comments.len() * 2);
    let mut blocks = Vec::new();
    let mut rest = comments;
    let mut idx = 0;
    while let Some(at) = rest.find("<comment ") {
        let Some(end) = rest[at..].find("</comment>") else {
            break;
        };
        let end = at + end + "</comment>".len();
        let chunk = &rest[at..end];
        let node = list.get(idx);
        idx += 1;
        let cell = node.and_then(|n| n.attr("ref")).and_then(crate::parse_rect);
        let shape = cell.and_then(|r| {
            shapes
                .iter()
                .find(|s| s.row == r.r0 && s.col == r.c0)
                .cloned()
        });
        out.push_str(&rest[..at]);
        let Some(shape) = shape else {
            out.push_str(chunk);
            rest = &rest[end..];
            continue;
        };
        let tag_end = chunk.find('>').unwrap();
        let mut tag = chunk[..tag_end].to_string();
        if let Some(i) = tag.find(" shapeId=\"") {
            let j = tag[i + 10..].find('"').unwrap_or(0) + i + 11;
            tag.replace_range(i..j, "");
        }
        tag.push_str(&format!(" shapeId=\"{}\"", shape.id));
        let mut body = chunk[tag_end + 1..chunk.len() - "</comment>".len()].to_string();
        if let Some(i) = body.find("<mc:AlternateContent") {
            if let Some(j) = body[i..].find("</mc:AlternateContent>") {
                body.replace_range(i..i + j + "</mc:AlternateContent>".len(), "");
            }
        }
        out.push_str(&format!("{tag}>{body}{}</comment>", comment_pr(&shape)));
        blocks.push((
            shape.id,
            shape_block(&shape, node.and_then(|n| n.child("text")), local_ns),
        ));
        rest = &rest[end..];
    }
    out.push_str(rest);
    if blocks.is_empty() {
        return None;
    }

    for (prefix, uri) in [("xdr", XDR), ("v", VML)] {
        if !out.contains(&format!("xmlns:{prefix}=")) {
            if let Some(p) = out.find("<comments ") {
                out.insert_str(p + "<comments".len(), &format!(" xmlns:{prefix}=\"{uri}\""));
            }
        }
    }
    blocks.sort_by_key(|(id, _)| *id);
    Some(Built {
        comments: out,
        shapes: blocks.into_iter().map(|(_, b)| b).collect(),
    })
}

pub(crate) fn strip_shapes(drawing: &str) -> String {
    let mut out = String::with_capacity(drawing.len());
    let mut rest = drawing;
    while let Some(at) = rest.find("<mc:AlternateContent") {
        let Some(len) = rest[at..].find("</mc:AlternateContent>") else {
            break;
        };
        let end = at + len + "</mc:AlternateContent>".len();
        out.push_str(&rest[..at]);
        let block = &rest[at..end];
        let is_note = block.contains("Requires=\"v\"")
            && block.contains("<xdr:sp ")
            && block.contains("name=\"Comment ");
        if !is_note {
            out.push_str(block);
        }
        rest = &rest[end..];
    }
    out.push_str(rest);
    out
}

pub(crate) fn add_shapes(drawing: &str, shapes: &[String]) -> Option<String> {
    let at = drawing.rfind("</")?;
    let mut s = String::from(&drawing[..at]);
    for b in shapes {
        s.push_str(b);
    }
    s.push_str(&drawing[at..]);
    Some(s)
}

pub(crate) fn fresh_drawing(shapes: &[String]) -> String {
    let mut s = format!(
        "<?xml version=\"1.0\" encoding=\"UTF-8\" standalone=\"yes\"?>\n<xdr:wsDr xmlns:xdr=\"{XDR}\" xmlns:a=\"{A}\">"
    );
    for b in shapes {
        s.push_str(b);
    }
    s.push_str("</xdr:wsDr>");
    s
}

#[cfg(test)]
mod tests {
    use super::*;

    const VMLS: &str = "<xml><v:shape id=\"_x0000_s1025\" type=\"#_x0000_t202\" style='position:absolute;\n  margin-left:315.5pt;margin-top:53pt;width:104pt;height:61.5pt;z-index:1;\n  visibility:hidden' fillcolor=\"infoBackground [80]\"><x:ClientData ObjectType=\"Note\"><x:MoveWithCells/><x:Anchor>\n    2, 138, 3, 12, 4, 61, 7, 17</x:Anchor><x:AutoFill>False</x:AutoFill><x:Row>4</x:Row><x:Column>3</x:Column></x:ClientData></v:shape></xml>";
    const COMMENTS: &str = "<comments xmlns=\"http://schemas.openxmlformats.org/spreadsheetml/2006/main\"><authors><author>zac</author></authors><commentList><comment ref=\"D5\" authorId=\"0\"><text><r><rPr><b/><sz val=\"9\"/><rFont val=\"Tahoma\"/></rPr><t>note</t></r></text></comment></commentList></comments>";

    #[test]
    fn note_shape_is_read_from_vml() {
        let s = note_shapes(VMLS);
        assert_eq!(s.len(), 1);
        assert_eq!(
            (s[0].id, s[0].row, s[0].col, s[0].hidden),
            (1025, 4, 3, true)
        );
        assert_eq!(s[0].pt, [315.5, 53.0, 104.0, 61.5]);
        assert_eq!(s[0].anchor, [2, 138, 3, 12, 4, 61, 7, 17]);
    }

    #[test]
    fn comment_names_its_shape_and_anchor() {
        let b = build(COMMENTS, VMLS, false).unwrap();
        assert!(b
            .comments
            .contains("<comment ref=\"D5\" authorId=\"0\" shapeId=\"1025\">"));
        assert!(b.comments.contains(
            "<commentPr defaultSize=\"0\" autoFill=\"0\" autoLine=\"0\"><anchor><from><xdr:col>2</xdr:col><xdr:colOff>1314450</xdr:colOff>"
        ));
        assert!(b.comments.contains("xmlns:xdr="));
        let sh = &b.shapes[0];
        assert!(
            sh.contains("<a:off x=\"4006850\" y=\"673100\"/><a:ext cx=\"1320800\" cy=\"781050\"/>")
        );
        assert!(sh.contains("name=\"Comment 1\" hidden=\"1\""));
        assert!(sh.contains("b=\"1\" i=\"0\" u=\"none\" strike=\"noStrike\" baseline=\"0%\""));
        assert!(sh.contains("<a:t>note</a:t>"));

        let again = build(&b.comments, VMLS, false).unwrap();
        assert_eq!(again.comments, b.comments);
    }

    #[test]
    fn strip_and_add_round_trip() {
        let b = build(COMMENTS, VMLS, false).unwrap();
        let d = fresh_drawing(&b.shapes);
        let stripped = strip_shapes(&d);
        assert!(!stripped.contains("Comment 1"));
        let back = add_shapes(&stripped, &b.shapes).unwrap();
        assert_eq!(back, d);
    }
}
