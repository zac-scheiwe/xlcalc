use crate::package::{dir_of, relative, resolve, Package, Rel};
use crate::rawxml::{self, escape_attr, escape_text, unescape, Tag};
use crate::xml;
use std::collections::BTreeSet;
use xlcalc::SheetId;

const REL: &str = "http://schemas.openxmlformats.org/officeDocument/2006/relationships/";
const NS_CTRL: &str = "http://schemas.microsoft.com/office/spreadsheetml/2009/9/main";
const NS_XDR: &str = "http://schemas.openxmlformats.org/drawingml/2006/spreadsheetDrawing";
const NS_MC: &str = "http://schemas.openxmlformats.org/markup-compatibility/2006";
const CT_CTRL: &str = "application/vnd.ms-excel.controlproperties+xml";
const CT_VML: &str = "application/vnd.openxmlformats-officedocument.vmlDrawing";

const COL_PT: f64 = 48.0;
const ROW_PT: f64 = 14.5;
const EMU_PT: f64 = 12_700.0;

#[derive(Clone, Copy, PartialEq, Eq, Debug)]
pub enum ControlKind {
    Button,
    CheckBox,
    Drop,
    List,
    Spin,
    Scroll,
    Radio,
    GBox,
    Label,
}

impl ControlKind {
    pub fn object_type(self) -> &'static str {
        match self {
            ControlKind::Button => "Button",
            ControlKind::CheckBox => "CheckBox",
            ControlKind::Drop => "Drop",
            ControlKind::List => "List",
            ControlKind::Spin => "Spin",
            ControlKind::Scroll => "Scroll",
            ControlKind::Radio => "Radio",
            ControlKind::GBox => "GBox",
            ControlKind::Label => "Label",
        }
    }

    pub fn from_object_type(t: &str) -> Option<ControlKind> {
        Some(match t {
            "Button" => ControlKind::Button,
            "CheckBox" => ControlKind::CheckBox,
            "Drop" => ControlKind::Drop,
            "List" => ControlKind::List,
            "Spin" => ControlKind::Spin,
            "Scroll" => ControlKind::Scroll,
            "Radio" => ControlKind::Radio,
            "GBox" => ControlKind::GBox,
            "Label" => ControlKind::Label,
            _ => return None,
        })
    }

    fn vml_type(self) -> &'static str {
        match self {
            ControlKind::CheckBox => "Checkbox",
            other => other.object_type(),
        }
    }

    fn has_text(self) -> bool {
        matches!(
            self,
            ControlKind::Button
                | ControlKind::CheckBox
                | ControlKind::Radio
                | ControlKind::GBox
                | ControlKind::Label
        )
    }

    fn is_list(self) -> bool {
        matches!(self, ControlKind::Drop | ControlKind::List)
    }

    fn is_bar(self) -> bool {
        matches!(self, ControlKind::Spin | ControlKind::Scroll)
    }
}

#[derive(Clone, Debug, PartialEq)]
pub struct Control {
    pub kind: ControlKind,
    pub name: String,

    pub shape_id: u32,

    pub on_action: String,
    pub caption: String,

    pub left: f64,
    pub top: f64,
    pub width: f64,
    pub height: f64,

    pub fmla_link: String,
    pub fmla_range: String,

    pub val: i32,
    pub min: i32,
    pub max: i32,
    pub inc: i32,
    pub page: i32,

    pub checked: i32,

    pub sel: i32,
    pub drop_lines: i32,

    pub horiz: bool,

    pub first_button: bool,

    pub placement: i32,
    pub locked: bool,
    pub enabled: bool,
    pub print_object: bool,
}

impl Control {
    pub fn new(kind: ControlKind, name: &str, rect: [f64; 4]) -> Control {
        let bar = kind.is_bar();
        Control {
            kind,
            name: name.to_string(),
            shape_id: 0,
            on_action: String::new(),
            caption: if kind.has_text() {
                name.to_string()
            } else {
                String::new()
            },
            left: rect[0],
            top: rect[1],
            width: rect[2],
            height: rect[3],
            fmla_link: String::new(),
            fmla_range: String::new(),
            val: 0,
            min: 0,
            max: match kind {
                ControlKind::Spin => 30_000,
                ControlKind::Scroll => 100,
                _ => 0,
            },
            inc: 1,
            page: if bar || kind.is_list() { 10 } else { 0 },
            checked: 0,
            sel: 0,
            drop_lines: if kind == ControlKind::Drop { 8 } else { 0 },
            horiz: kind == ControlKind::Scroll && rect[2] > rect[3],
            first_button: false,
            placement: match kind {
                ControlKind::Spin | ControlKind::Button | ControlKind::Label => 1,
                _ => 2,
            },
            locked: true,
            enabled: true,
            print_object: true,
        }
    }
}

fn int(t: &Tag, key: &str, default: i32) -> i32 {
    t.value(key).and_then(|v| v.parse().ok()).unwrap_or(default)
}

struct Found {
    shape_id: u32,
    rid: String,
    name: String,
    mac: String,
    placement: i32,
    locked: bool,
    enabled: bool,
    print_object: bool,
}

fn find_controls(n: &xml::Node, out: &mut Vec<Found>) {
    for k in &n.kids {
        if k.name == "control" {
            let pr = k.child("controlPr");
            let at = |key: &str| pr.and_then(|p| p.attr(key));
            let anchor = pr.and_then(|p| p.child("anchor"));
            let flag = |key: &str| anchor.and_then(|a| a.attr(key)).is_some_and(|v| v != "0");
            out.push(Found {
                shape_id: k.attr("shapeId").and_then(|v| v.parse().ok()).unwrap_or(0),
                rid: k.attr("id").unwrap_or("").to_string(),
                name: k.attr("name").unwrap_or("").to_string(),
                mac: at("macro").unwrap_or("").to_string(),
                placement: if flag("sizeWithCells") {
                    1
                } else if flag("moveWithCells") {
                    2
                } else {
                    3
                },
                locked: at("locked") != Some("0"),
                enabled: at("disabled") != Some("1"),
                print_object: at("print") != Some("0"),
            });
        } else {
            find_controls(k, out);
        }
    }
}

struct SheetParts {
    controls: Option<String>,
    vml: Option<String>,
}

fn sheet_parts(pkg: &Package, part: &str) -> Option<SheetParts> {
    let bytes = pkg.part(part)?;
    if !bytes.windows(9).any(|w| w == b"controls>") {
        return None;
    }
    let split = rawxml::split(bytes).ok()?;
    let rels = pkg.rels(part);
    let vml = split.kid("legacyDrawing").and_then(|k| {
        let n = rawxml::head_len(&k.raw)?;
        let t = Tag::parse(&k.raw[..n]).ok()?;
        let rid = t
            .attrs
            .iter()
            .find(|(k, _)| k == "id" || k.ends_with(":id"))
            .map(|(_, v)| unescape(v))?;
        let rel = rels.iter().find(|r| r.id == rid)?;
        Some(resolve(dir_of(part), &rel.target))
    });
    Some(SheetParts {
        controls: split.kid("controls").map(|k| k.raw.clone()),
        vml,
    })
}

pub fn read_controls(pkg: &Package, sheet: SheetId) -> Vec<Control> {
    match pkg.source_sheet(sheet) {
        Some(s) => read_part(pkg, &s.part),
        None => Vec::new(),
    }
}

fn read_part(pkg: &Package, part: &str) -> Vec<Control> {
    let Some(sp) = sheet_parts(pkg, part) else {
        return Vec::new();
    };
    let Some(root) = sp.controls.and_then(|raw| xml::parse(raw.as_bytes()).ok()) else {
        return Vec::new();
    };
    let mut found = Vec::new();
    find_controls(&root, &mut found);
    let rels = pkg.rels(part);
    let vml = sp.vml.and_then(|p| {
        pkg.part(&p)
            .map(|b| String::from_utf8_lossy(b).into_owned())
    });
    let mut list = Vec::new();
    for f in found {
        let text = rels
            .iter()
            .find(|r| r.id == f.rid)
            .and_then(|r| pkg.part(&resolve(dir_of(part), &r.target)))
            .map(|b| String::from_utf8_lossy(b).into_owned())
            .unwrap_or_default();
        let Some(tag) = text.find("<formControlPr").and_then(|i| {
            let n = rawxml::head_len(&text[i..])?;
            Tag::parse(&text[i..i + n]).ok()
        }) else {
            continue;
        };
        let Some(kind) = tag
            .value("objectType")
            .and_then(|t| ControlKind::from_object_type(&t))
        else {
            continue;
        };
        let mut c = Control::new(kind, &f.name, [0.0; 4]);
        c.shape_id = f.shape_id;
        c.on_action = f.mac;
        c.placement = f.placement;
        c.locked = f.locked;
        c.enabled = f.enabled;
        c.print_object = f.print_object;
        c.fmla_link = tag.value("fmlaLink").unwrap_or_default();
        c.fmla_range = tag.value("fmlaRange").unwrap_or_default();
        c.val = int(&tag, "val", c.val);
        c.min = int(&tag, "min", c.min);
        c.max = int(&tag, "max", c.max);
        c.inc = int(&tag, "inc", c.inc);
        c.page = int(&tag, "page", c.page);
        c.sel = int(&tag, "sel", c.sel);
        c.drop_lines = int(&tag, "dropLines", c.drop_lines);
        c.checked = match tag.get("checked") {
            Some("Checked") => 1,
            Some("Mixed") => 2,
            _ => 0,
        };
        c.horiz = tag.get("horiz").is_some_and(|v| v != "0");
        c.first_button = tag.get("firstButton").is_some_and(|v| v != "0");
        if let Some(block) = vml.as_deref().and_then(|v| shape_block(v, f.shape_id)) {
            let b = &block;
            if let Some([l, t, w, h]) = shape_rect(b) {
                (c.left, c.top, c.width, c.height) = (l, t, w, h);
            }
            if c.on_action.is_empty() {
                c.on_action = elem(b, "FmlaMacro").unwrap_or_default();
            }
            if kind.has_text() {
                c.caption = caption_of(b);
            }
        }
        list.push(c);
    }
    list
}

fn shape_ranges(vml: &str) -> Vec<(usize, usize, u32)> {
    let mut v = Vec::new();
    let mut at = 0;
    while let Some(i) = vml[at..].find("<v:shape ") {
        let start = at + i;
        let Some(j) = vml[start..].find("</v:shape>") else {
            break;
        };
        let end = start + j + "</v:shape>".len();
        let head = &vml[start..vml[start..].find('>').map_or(end, |h| start + h)];
        let id = head
            .split("id=\"_x0000_s")
            .nth(1)
            .and_then(|r| r.split('"').next())
            .and_then(|n| n.parse().ok())
            .unwrap_or(0);
        v.push((start, end, id));
        at = end;
    }
    v
}

fn shape_block(vml: &str, id: u32) -> Option<String> {
    shape_ranges(vml)
        .into_iter()
        .find(|r| r.2 == id)
        .map(|(s, e, _)| vml[s..e].to_string())
}

fn elem(block: &str, tag: &str) -> Option<String> {
    if let Some(i) = block.find(&format!("<x:{tag}>")) {
        let from = i + tag.len() + 4;
        let to = block[from..].find(&format!("</x:{tag}>"))?;
        return Some(unescape(&block[from..from + to]));
    }
    block.contains(&format!("<x:{tag}/>")).then(String::new)
}

fn set_elem(block: &mut String, tag: &str, value: Option<&str>) {
    let open = format!("<x:{tag}>");
    let close = format!("</x:{tag}>");
    let empty = format!("<x:{tag}/>");
    let span = block
        .find(&open)
        .and_then(|i| Some((i, i + block[i..].find(&close)? + close.len())))
        .or_else(|| block.find(&empty).map(|i| (i, i + empty.len())));
    let new = value.map(|v| {
        if v.is_empty() {
            empty.clone()
        } else {
            format!("{open}{}{close}", escape_text(v))
        }
    });
    match (span, new) {
        (Some((s, e)), Some(n)) => block.replace_range(s..e, &n),
        (Some((s, e)), None) => block.replace_range(s..e, ""),
        (None, Some(n)) => {
            if let Some(i) = block.find("</x:ClientData>") {
                block.insert_str(i, &format!("{n}\n   "));
            }
        }
        (None, None) => {}
    }
}

fn shape_rect(block: &str) -> Option<[f64; 4]> {
    let style = style_of(block)?;
    let get = |key: &str| {
        style.split(';').find_map(|p| {
            let (k, v) = p.split_once(':')?;
            (k.trim() == key)
                .then(|| v.trim().trim_end_matches("pt").parse::<f64>().ok())
                .flatten()
        })
    };
    Some([
        get("margin-left")?,
        get("margin-top")?,
        get("width")?,
        get("height")?,
    ])
}

fn style_of(block: &str) -> Option<&str> {
    let i = block.find("style='")? + 7;
    let j = block[i..].find('\'')?;
    Some(&block[i..i + j])
}

fn set_rect(block: &mut String, rect: [f64; 4]) {
    let Some(i) = block.find("style='").map(|i| i + 7) else {
        return;
    };
    let Some(j) = block[i..].find('\'') else {
        return;
    };
    let mut out = Vec::new();
    for p in block[i..i + j].split(';') {
        let key = p.split_once(':').map_or("", |(k, _)| k.trim());
        let lead: String = p.chars().take_while(|c| c.is_whitespace()).collect();
        let v = match key {
            "margin-left" => Some(rect[0]),
            "margin-top" => Some(rect[1]),
            "width" => Some(rect[2]),
            "height" => Some(rect[3]),
            _ => None,
        };
        out.push(match v {
            Some(v) => format!("{lead}{key}:{}pt", num(v)),
            None => p.to_string(),
        });
    }
    block.replace_range(i..i + j, &out.join(";"));
}

fn num(v: f64) -> String {
    format!("{v}")
}

fn caption_of(block: &str) -> String {
    let Some(i) = block.find("<v:textbox") else {
        return String::new();
    };
    let Some(j) = block[i..].find("</v:textbox>") else {
        return String::new();
    };
    let inner = &block[i..i + j];
    let Some(start) = inner
        .find("<font")
        .and_then(|f| inner[f..].find('>').map(|g| f + g + 1))
    else {
        return String::new();
    };
    let body = inner[start..].split("</font>").next().unwrap_or("");
    let mut text = String::new();
    let mut rest = body;
    while let Some(lt) = rest.find('<') {
        text.push_str(&rest[..lt]);
        let Some(gt) = rest[lt..].find('>') else {
            break;
        };
        if rest[lt + 1..].starts_with("br") {
            text.push('\u{1}');
        }
        rest = &rest[lt + gt + 1..];
    }
    text.push_str(rest);
    let mut out = String::new();
    let mut chars = text.chars().peekable();
    while let Some(c) = chars.next() {
        match c {
            '\r' | '\n' => {
                while chars
                    .peek()
                    .is_some_and(|c| c.is_whitespace() && *c != '\u{1}')
                {
                    chars.next();
                }
                out.push(' ');
            }
            '\u{1}' => out.push('\n'),
            c => out.push(c),
        }
    }
    unescape(&out)
}

fn set_caption(block: &mut String, caption: &str) {
    let Some(i) = block.find("<font ") else {
        return;
    };
    let Some(open_end) = block[i..].find('>').map(|k| i + k + 1) else {
        return;
    };
    let Some(close) = block[open_end..].find("</font>").map(|k| open_end + k) else {
        return;
    };
    let text = escape_text(caption).replace('\n', "<br/>");
    block.replace_range(open_end..close, &text);
}

fn vml_anchor(c: &Control) -> String {
    let corner = |x: f64, y: f64| {
        let col = (x / COL_PT).floor().max(0.0);
        let row = (y / ROW_PT).floor().max(0.0);
        format!(
            "{col}, {}, {row}, {}",
            ((x - col * COL_PT) * 2.0).round(),
            ((y - row * ROW_PT) * 2.0).round()
        )
    };
    format!(
        "{}, {}",
        corner(c.left, c.top),
        corner(c.left + c.width, c.top + c.height)
    )
}

fn sheet_anchor(c: &Control) -> String {
    let corner = |x: f64, y: f64| {
        let col = (x / COL_PT).floor().max(0.0);
        let row = (y / ROW_PT).floor().max(0.0);
        format!(
            "<xdr:col>{col}</xdr:col><xdr:colOff>{}</xdr:colOff><xdr:row>{row}</xdr:row><xdr:rowOff>{}</xdr:rowOff>",
            ((x - col * COL_PT) * EMU_PT).round(),
            ((y - row * ROW_PT) * EMU_PT).round()
        )
    };
    let mut flags = String::new();
    if c.placement <= 2 {
        flags.push_str(" moveWithCells=\"1\"");
    }
    if c.placement == 1 {
        flags.push_str(" sizeWithCells=\"1\"");
    }
    format!(
        "<anchor{flags}><from>{}</from><to>{}</to></anchor>",
        corner(c.left, c.top),
        corner(c.left + c.width, c.top + c.height)
    )
}

fn owned_attrs(c: &Control) -> Vec<(&'static str, Option<String>)> {
    let s = |n: i32| Some(n.to_string());
    let mut v = vec![(
        "checked",
        match c.checked {
            1 => Some("Checked".to_string()),
            2 => Some("Mixed".to_string()),
            _ => None,
        },
    )];
    v.push((
        "fmlaLink",
        (!c.fmla_link.is_empty()).then(|| c.fmla_link.clone()),
    ));
    if c.kind.is_list() {
        v.push((
            "fmlaRange",
            (!c.fmla_range.is_empty()).then(|| c.fmla_range.clone()),
        ));
        v.push(("sel", s(c.sel)));
        v.push(("val", s(0)));
    }
    if c.kind == ControlKind::Drop {
        v.push(("dropLines", s(c.drop_lines)));
    }
    if c.kind.is_bar() {
        v.push(("val", s(c.val)));
        v.push(("max", s(c.max)));
        v.push(("min", (c.min != 0).then(|| c.min.to_string())));
        v.push(("inc", (c.inc != 1).then(|| c.inc.to_string())));
        v.push(("page", s(c.page)));
    }
    v
}

fn ctrl_prop_new(c: &Control) -> Vec<u8> {
    let mut t = Tag::new("formControlPr");
    t.set("xmlns", Some(NS_CTRL));
    t.set("objectType", Some(c.kind.object_type()));
    t.empty = true;
    let k = c.kind;
    let mut attrs: Vec<(&str, Option<String>)> = owned_attrs(c);
    let one = || "1".to_string();
    attrs.push(("dx", (k.is_bar() || k.is_list()).then(|| "31".to_string())));
    attrs.push((
        "dropStyle",
        (k == ControlKind::Drop).then(|| "combo".to_string()),
    ));
    attrs.push(("horiz", (k == ControlKind::Scroll && c.horiz).then(one)));
    attrs.push((
        "firstButton",
        (k == ControlKind::Radio && c.first_button).then(one),
    ));
    attrs.push((
        "lockText",
        matches!(
            k,
            ControlKind::Button | ControlKind::CheckBox | ControlKind::Radio | ControlKind::Label
        )
        .then(one),
    ));
    attrs.push((
        "noThreeD",
        matches!(
            k,
            ControlKind::CheckBox
                | ControlKind::Radio
                | ControlKind::Drop
                | ControlKind::List
                | ControlKind::GBox
        )
        .then(one),
    ));
    attrs.sort_by_key(|(n, _)| *n);
    for (n, v) in attrs {
        if let Some(v) = v {
            t.set(n, Some(&v));
        }
    }
    let mut s = String::from(rawxml::DECL);
    s.push_str(&t.empty());
    s.into_bytes()
}

fn ctrl_prop_patch(old: &[u8], c: &Control) -> Result<Vec<u8>, String> {
    let text = std::str::from_utf8(old).map_err(|e| e.to_string())?;
    let i = text.find("<formControlPr").ok_or("no formControlPr")?;
    let n = rawxml::head_len(&text[i..]).ok_or("bad formControlPr")?;
    let mut t = Tag::parse(&text[i..i + n])?;
    for (name, v) in owned_attrs(c) {
        t.set(name, v.as_deref());
    }
    Ok(format!("{}{}{}", &text[..i], t.write(), &text[i + n..]).into_bytes())
}

const VML_HEAD: &str = "<xml xmlns:v=\"urn:schemas-microsoft-com:vml\"\r\n xmlns:o=\"urn:schemas-microsoft-com:office:office\"\r\n xmlns:x=\"urn:schemas-microsoft-com:office:excel\">\r\n <o:shapelayout v:ext=\"edit\">\r\n  <o:idmap v:ext=\"edit\" data=\"1\"/>\r\n </o:shapelayout>";

const VML_TYPE: &str = "<v:shapetype id=\"_x0000_t201\" coordsize=\"21600,21600\" o:spt=\"201\"\r\n  path=\"m,l,21600r21600,l21600,xe\">\r\n  <v:stroke joinstyle=\"miter\"/>\r\n  <v:path shadowok=\"f\" o:extrusionok=\"f\" strokeok=\"f\" fillok=\"f\" o:connecttype=\"rect\"/>\r\n  <o:lock v:ext=\"edit\" shapetype=\"t\"/>\r\n </v:shapetype>";

fn vml_shape(c: &Control, z: usize) -> String {
    let k = c.kind;
    let style = format!(
        "position:absolute;\r\n  margin-left:{}pt;margin-top:{}pt;width:{}pt;height:{}pt;z-index:{z}{}",
        num(c.left),
        num(c.top),
        num(c.width),
        num(c.height),
        if matches!(k, ControlKind::CheckBox | ControlKind::Radio | ControlKind::Button) {
            ";\r\n  mso-wrap-style:tight"
        } else {
            ""
        }
    );
    let look = match k {
        ControlKind::CheckBox => " o:button=\"t\" filled=\"f\" fillcolor=\"windowText [64]\"\r\n  stroked=\"f\" strokecolor=\"window [65]\" strokeweight=\"3e-5mm\"",
        ControlKind::Radio => " filled=\"f\" fillcolor=\"windowText [64]\" stroked=\"f\"\r\n  strokecolor=\"window [65]\" strokeweight=\"3e-5mm\"",
        ControlKind::Button => " o:button=\"t\" fillcolor=\"buttonFace [67]\"",
        ControlKind::GBox => " fillcolor=\"window [65]\" strokecolor=\"windowText [64]\"",
        ControlKind::Label => " filled=\"f\" fillcolor=\"windowText [64]\" strokecolor=\"windowText [64]\"",
        ControlKind::Drop => " filled=\"f\"\r\n  fillcolor=\"windowText [64]\" strokecolor=\"windowText [64]\"",
        ControlKind::List => "\r\n  fillcolor=\"window [65]\" strokecolor=\"windowText [64]\"",
        _ => "",
    };
    let mut s = format!(
        "<v:shape id=\"_x0000_s{}\" type=\"#_x0000_t201\" style='{style}'{look} o:insetmode=\"auto\">\r\n",
        c.shape_id
    );
    match k {
        ControlKind::Button => s.push_str("  <v:fill color2=\"buttonFace [67]\" o:detectmouseclick=\"t\"/>\r\n"),
        ControlKind::CheckBox => s.push_str("  <v:fill color2=\"window [65]\" o:detectmouseclick=\"t\"/>\r\n  <v:path shadowok=\"t\" strokeok=\"t\" fillok=\"t\"/>\r\n"),
        ControlKind::Radio => s.push_str("  <v:fill color2=\"window [65]\"/>\r\n  <v:path shadowok=\"t\" strokeok=\"t\" fillok=\"t\"/>\r\n"),
        ControlKind::GBox | ControlKind::Label | ControlKind::Drop | ControlKind::List => {
            s.push_str("  <v:fill color2=\"window [65]\"/>\r\n")
        }
        _ => {}
    }
    s.push_str(if k.has_text() || k.is_list() {
        "  <o:lock v:ext=\"edit\" rotation=\"t\"/>\r\n"
    } else {
        "  <o:lock v:ext=\"edit\" rotation=\"t\" text=\"t\"/>\r\n"
    });
    if k.is_list() {
        s = s.replace("rotation=\"t\"/>", "rotation=\"t\" text=\"t\"/>");
    }
    if k.has_text() {
        let (face, size, color, align) = if k == ControlKind::Button {
            ("Aptos Narrow", 220, "#000000", "center")
        } else {
            ("Segoe UI", 160, "auto", "left")
        };
        s.push_str(&format!(
            "  <v:textbox style='mso-direction-alt:auto' o:singleclick=\"f\">\r\n   <div style='text-align:{align}'><font face=\"{face}\" size=\"{size}\" color=\"{color}\">{}</font></div>\r\n  </v:textbox>\r\n",
            escape_text(&c.caption).replace('\n', "<br/>")
        ));
    }
    s.push_str(&format!(
        "  <x:ClientData ObjectType=\"{}\">\r\n",
        k.vml_type()
    ));
    if matches!(
        k,
        ControlKind::CheckBox
            | ControlKind::Radio
            | ControlKind::Scroll
            | ControlKind::Drop
            | ControlKind::List
            | ControlKind::GBox
    ) {
        s.push_str("   <x:SizeWithCells/>\r\n");
    }
    s.push_str(&format!(
        "   <x:Anchor>\r\n    {}</x:Anchor>\r\n",
        vml_anchor(c)
    ));
    let mut cd = String::new();
    if k == ControlKind::Button {
        cd.push_str("   <x:PrintObject>False</x:PrintObject>\r\n");
    }
    if !k.is_bar() && k != ControlKind::GBox {
        cd.push_str("   <x:AutoFill>False</x:AutoFill>\r\n");
    }
    if matches!(k, ControlKind::CheckBox | ControlKind::Radio) {
        cd.push_str("   <x:AutoLine>False</x:AutoLine>\r\n");
    }
    cd.push_str("   <x:Placeholder/>\r\n");
    cd.push_str(match k {
        ControlKind::Button => {
            "   <x:TextHAlign>Center</x:TextHAlign>\r\n   <x:TextVAlign>Center</x:TextVAlign>\r\n"
        }
        ControlKind::CheckBox | ControlKind::Radio => "   <x:TextVAlign>Center</x:TextVAlign>\r\n",
        _ => "",
    });
    let mut body = format!("{s}{cd}  </x:ClientData>\r\n </v:shape>");
    body = body.replace("   <x:Placeholder/>\r\n", "");
    set_elem(
        &mut body,
        "FmlaMacro",
        (!c.on_action.is_empty()).then_some(c.on_action.as_str()),
    );
    set_elem(
        &mut body,
        "Checked",
        match c.checked {
            0 => None,
            n => Some(if n == 1 { "1" } else { "2" }),
        }
        .filter(|_| matches!(k, ControlKind::CheckBox | ControlKind::Radio)),
    );
    set_elem(
        &mut body,
        "FmlaLink",
        (!c.fmla_link.is_empty()).then_some(c.fmla_link.as_str()),
    );
    set_elem(
        &mut body,
        "FmlaRange",
        (k.is_list() && !c.fmla_range.is_empty()).then_some(c.fmla_range.as_str()),
    );
    if k.is_bar() || k.is_list() {
        let (v, mn, mx) = if k.is_bar() {
            (c.val, c.min, c.max)
        } else {
            (0, 0, 0)
        };
        let page = if k == ControlKind::List { 6 } else { c.page };
        for (t, x) in [
            ("Val", v),
            ("Min", mn),
            ("Max", mx),
            ("Inc", c.inc),
            ("Page", page),
        ] {
            set_elem(&mut body, t, Some(&x.to_string()));
        }
        if k == ControlKind::Scroll && c.horiz {
            set_elem(&mut body, "Horiz", Some(""));
        }
        set_elem(&mut body, "Dx", Some("31"));
    }
    if k.is_list() {
        set_elem(&mut body, "Sel", Some(&c.sel.to_string()));
        set_elem(&mut body, "NoThreeD2", Some(""));
        set_elem(&mut body, "SelType", Some("Single"));
        set_elem(&mut body, "LCT", Some("Normal"));
    }
    if k == ControlKind::Drop {
        set_elem(&mut body, "DropStyle", Some("Combo"));
        set_elem(&mut body, "DropLines", Some(&c.drop_lines.to_string()));
    }
    if matches!(
        k,
        ControlKind::CheckBox | ControlKind::Radio | ControlKind::GBox
    ) {
        set_elem(&mut body, "NoThreeD", Some(""));
    }
    if k == ControlKind::Radio && c.first_button {
        set_elem(&mut body, "FirstButton", Some(""));
    }
    body
}

fn vml_patch(block: &str, c: &Control, src: &Control) -> String {
    let mut b = block.to_string();
    if (c.left, c.top, c.width, c.height) != (src.left, src.top, src.width, src.height) {
        set_rect(&mut b, [c.left, c.top, c.width, c.height]);
        set_elem(
            &mut b,
            "Anchor",
            Some(&format!("\r\n    {}", vml_anchor(c))),
        );
    }
    if c.caption != src.caption && c.kind.has_text() {
        set_caption(&mut b, &c.caption);
    }
    let k = c.kind;
    let opt = |s: &str| (!s.is_empty()).then(|| s.to_string());
    set_elem(&mut b, "FmlaMacro", opt(&c.on_action).as_deref());
    set_elem(&mut b, "FmlaLink", opt(&c.fmla_link).as_deref());
    if matches!(k, ControlKind::CheckBox | ControlKind::Radio) {
        set_elem(
            &mut b,
            "Checked",
            match c.checked {
                0 => None,
                1 => Some("1"),
                _ => Some("2"),
            },
        );
    }
    if k.is_bar() {
        for (t, x) in [
            ("Val", c.val),
            ("Min", c.min),
            ("Max", c.max),
            ("Inc", c.inc),
            ("Page", c.page),
        ] {
            set_elem(&mut b, t, Some(&x.to_string()));
        }
    }
    if k.is_list() {
        set_elem(&mut b, "FmlaRange", opt(&c.fmla_range).as_deref());
        set_elem(&mut b, "Sel", Some(&c.sel.to_string()));
    }
    if k == ControlKind::Drop {
        set_elem(&mut b, "DropLines", Some(&c.drop_lines.to_string()));
    }
    b
}

fn sheet_block(c: &Control, rp: &str, rid: &str) -> String {
    let k = c.kind;
    let mut t = Tag::new("controlPr");
    t.set("defaultSize", Some("0"));
    if !c.print_object {
        t.set("print", Some("0"));
    }
    if !c.locked {
        t.set("locked", Some("0"));
    }
    if !c.enabled {
        t.set("disabled", Some("1"));
    }
    if !k.is_bar() && k != ControlKind::GBox {
        t.set("autoFill", Some("0"));
    }
    if matches!(k, ControlKind::CheckBox | ControlKind::Radio) {
        t.set("autoLine", Some("0"));
    }
    t.set("autoPict", Some("0"));
    if !c.on_action.is_empty() {
        t.set("macro", Some(&c.on_action));
    }
    format!(
        "<mc:AlternateContent xmlns:mc=\"{NS_MC}\"><mc:Choice Requires=\"x14\" xmlns:x14=\"{NS_CTRL}\"><control shapeId=\"{}\" {rp}:id=\"{}\" name=\"{}\">{}{}</controlPr></control></mc:Choice></mc:AlternateContent>",
        c.shape_id,
        escape_attr(rid),
        escape_attr(&c.name),
        t.open(),
        sheet_anchor(c)
    )
}

fn source_blocks(raw: &str) -> Vec<(u32, String)> {
    let mut v = Vec::new();
    let mut at = 0;
    while let Some(i) = raw[at..].find("<mc:AlternateContent") {
        let start = at + i;
        let Some(j) = raw[start..].find("</mc:AlternateContent>") else {
            break;
        };
        let end = start + j + "</mc:AlternateContent>".len();
        let block = &raw[start..end];
        let id = block
            .split("shapeId=\"")
            .nth(1)
            .and_then(|r| r.split('"').next())
            .and_then(|n| n.parse().ok())
            .unwrap_or(0);
        v.push((id, block.to_string()));
        at = end;
    }
    v
}

pub(crate) struct Env<'a> {
    pub src: &'a Package,

    pub source_part: Option<&'a str>,

    pub part: &'a str,

    pub rp: &'a str,
}

pub(crate) type NewPart = (String, Vec<u8>, &'static str);

pub(crate) fn write_sheet(
    env: &Env,
    controls: &[Control],
    split: &mut rawxml::Split,
    sheet_rels: &mut Vec<Rel>,
    used: &mut BTreeSet<String>,
    taken: &dyn Fn(&str) -> bool,
    order: &[&str],
) -> Result<Vec<NewPart>, String> {
    let before = env
        .source_part
        .map(|p| read_part(env.src, p))
        .unwrap_or_default();
    if before == controls {
        return Ok(Vec::new());
    }
    let sp = env.source_part.and_then(|p| sheet_parts(env.src, p));
    let src_blocks = sp
        .as_ref()
        .and_then(|s| s.controls.as_deref())
        .map(source_blocks)
        .unwrap_or_default();
    let src_rels = env.source_part.map(|p| env.src.rels(p)).unwrap_or_default();
    let ctrl_rel = |c: &Control| -> Option<(String, String)> {
        let (_, block) = src_blocks.iter().find(|(id, _)| *id == c.shape_id)?;
        let rid = block
            .split(":id=\"")
            .nth(1)
            .and_then(|r| r.split('"').next())?
            .to_string();
        let rel = src_rels.iter().find(|r| r.id == rid)?;
        Some((rid, resolve(dir_of(env.source_part?), &rel.target)))
    };

    let vml_src = sp.as_ref().and_then(|s| s.vml.as_ref()).and_then(|p| {
        env.src
            .part(p)
            .map(|b| String::from_utf8_lossy(b).into_owned())
    });
    let mut next_id = shape_ranges(vml_src.as_deref().unwrap_or(""))
        .iter()
        .map(|r| r.2)
        .chain(src_blocks.iter().map(|b| b.0))
        .chain(controls.iter().map(|c| c.shape_id))
        .max()
        .map_or(1025, |m| (m + 1).max(1025));

    let mut parts: Vec<NewPart> = Vec::new();
    let mut names: BTreeSet<String> = BTreeSet::new();
    let mut blocks = String::new();
    let mut shapes: Vec<(u32, String)> = Vec::new();
    let mut keep_rids: BTreeSet<String> = BTreeSet::new();
    let mut list: Vec<Control> = controls.to_vec();
    for (z, c) in list.iter_mut().enumerate() {
        let source = before
            .iter()
            .find(|b| b.shape_id == c.shape_id && c.shape_id != 0);
        let rel = source.and_then(|_| ctrl_rel(c));
        match (source, rel) {
            (Some(old), Some((rid, part))) => {
                keep_rids.insert(rid.clone());
                if let Some(bytes) = env.src.part(&part) {
                    parts.push((part.clone(), ctrl_prop_patch(bytes, c)?, CT_CTRL));
                }
                let block = src_blocks
                    .iter()
                    .find(|(id, _)| *id == c.shape_id)
                    .map(|b| b.1.clone())
                    .unwrap_or_default();
                blocks.push_str(&patch_block(&block, c, old));
                if let Some(b) = vml_src.as_deref().and_then(|v| shape_block(v, c.shape_id)) {
                    shapes.push((c.shape_id, vml_patch(&b, c, old)));
                }
            }
            _ => {
                c.shape_id = next_id;
                next_id += 1;
                let mut n = 1;
                let part = loop {
                    let p = format!("xl/ctrlProps/ctrlProp{n}.xml");
                    if !taken(&p) && names.insert(p.clone()) {
                        break p;
                    }
                    n += 1;
                };
                let rid = fresh(used);
                keep_rids.insert(rid.clone());
                sheet_rels.push(Rel {
                    id: rid.clone(),
                    ty: format!("{REL}ctrlProp"),
                    target: relative(dir_of(env.part), &part),
                    external: false,
                });
                parts.push((part, ctrl_prop_new(c), CT_CTRL));
                blocks.push_str(&sheet_block(c, env.rp, &rid));
                shapes.push((c.shape_id, vml_shape(c, z + 1)));
            }
        }
    }

    for (_, block) in &src_blocks {
        let rid = block
            .split(":id=\"")
            .nth(1)
            .and_then(|r| r.split('"').next());
        if let Some(rid) = rid.filter(|r| !keep_rids.contains(*r)) {
            sheet_rels.retain(|r| r.id != rid);
        }
    }

    let (vml_part, vml_text) = match (sp.as_ref().and_then(|s| s.vml.clone()), vml_src) {
        (Some(p), Some(text)) => (Some(p), Some(text)),
        _ => (None, None),
    };
    let text = match vml_text {
        Some(text) => merge_vml(&text, &shapes, &before, controls),
        None => format!("{VML_HEAD}{VML_TYPE}{}</xml>", shapes_text(&shapes)),
    };
    match vml_part {
        Some(p) => parts.push((p, text.into_bytes(), CT_VML)),
        None if shapes.is_empty() => {}
        None => {
            let mut n = 1;
            let p = loop {
                let p = format!("xl/drawings/vmlDrawing{n}.vml");
                if !taken(&p) && names.insert(p.clone()) {
                    break p;
                }
                n += 1;
            };
            let rid = fresh(used);
            sheet_rels.push(Rel {
                id: rid.clone(),
                ty: format!("{REL}vmlDrawing"),
                target: relative(dir_of(env.part), &p),
                external: false,
            });
            split.put(
                "legacyDrawing",
                Some(format!(
                    "<legacyDrawing {}:id=\"{}\"/>",
                    env.rp,
                    escape_attr(&rid)
                )),
                order,
            );
            parts.push((p, text.into_bytes(), CT_VML));
        }
    }
    let controls_xml = (!blocks.is_empty())
        .then(|| format!("<controls xmlns:xdr=\"{NS_XDR}\">{blocks}</controls>"));
    split.put("controls", controls_xml, order);
    Ok(parts)
}

fn fresh(used: &mut BTreeSet<String>) -> String {
    let mut i = 1;
    loop {
        let id = format!("rId{i}");
        if used.insert(id.clone()) {
            return id;
        }
        i += 1;
    }
}

fn shapes_text(shapes: &[(u32, String)]) -> String {
    shapes.iter().map(|s| s.1.as_str()).collect()
}

fn patch_block(block: &str, c: &Control, old: &Control) -> String {
    let mut b = block.to_string();
    if c.name != old.name {
        if let Some(i) = b.find(" name=\"") {
            if let Some(j) = b[i + 7..].find('"') {
                b.replace_range(i + 7..i + 7 + j, &escape_attr(&c.name));
            }
        }
    }
    let flags = |x: &Control| (x.on_action.clone(), x.locked, x.enabled, x.print_object);
    if flags(c) != flags(old) {
        if let Some(i) = b.find("<controlPr") {
            if let Some(n) = rawxml::head_len(&b[i..]) {
                if let Ok(mut t) = Tag::parse(&b[i..i + n]) {
                    let mac = (!c.on_action.is_empty()).then_some(c.on_action.as_str());
                    t.set("macro", mac);
                    t.set("locked", (!c.locked).then_some("0"));
                    t.set("disabled", (!c.enabled).then_some("1"));
                    t.set("print", (!c.print_object).then_some("0"));
                    b.replace_range(i..i + n, &t.open());
                }
            }
        }
    }
    if (c.left, c.top, c.width, c.height) != (old.left, old.top, old.width, old.height) {
        if let (Some(i), Some(j)) = (b.find("<anchor"), b.find("</anchor>")) {
            b.replace_range(i..j + "</anchor>".len(), &sheet_anchor(c));
        }
    }
    b
}

fn merge_vml(vml: &str, shapes: &[(u32, String)], before: &[Control], now: &[Control]) -> String {
    let mut out = String::new();
    let mut at = 0;
    let mut done: BTreeSet<u32> = BTreeSet::new();
    for (s, e, id) in shape_ranges(vml) {
        out.push_str(&vml[at..s]);
        at = e;
        if before.iter().any(|b| b.shape_id == id) {
            if let Some(new) = shapes.iter().find(|x| x.0 == id) {
                out.push_str(&new.1);
                done.insert(id);
            }
        } else {
            out.push_str(&vml[s..e]);
        }
    }
    let tail_at = vml[at..].rfind("</xml>").map_or(vml.len(), |i| at + i);
    out.push_str(&vml[at..tail_at]);
    let fresh: String = shapes
        .iter()
        .filter(|s| !done.contains(&s.0) && !before.iter().any(|b| b.shape_id == s.0))
        .map(|s| s.1.as_str())
        .collect();
    if !fresh.is_empty() && !out.contains("id=\"_x0000_t201\"") {
        match out.find("<v:shape ") {
            Some(i) => out.insert_str(i, VML_TYPE),
            None => out.push_str(VML_TYPE),
        }
    }
    out.push_str(&fresh);
    out.push_str(&vml[tail_at..]);
    let _ = now;
    out
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn caption_unwraps_lines() {
        let b = "<v:shape id=\"_x0000_s1025\"><v:textbox style='x'>\r\n   <div style='text-align:left'><font face=\"Segoe UI\" size=\"160\" color=\"auto\">Option\r\n   Button 2</font></div>\r\n  </v:textbox></v:shape>";
        assert_eq!(caption_of(b), "Option Button 2");
    }

    #[test]
    fn rect_and_elements() {
        let mut b = "<v:shape id=\"_x0000_s1025\" style='position:absolute;\r\n  margin-left:10pt;margin-top:10pt;width:60pt;height:20pt;z-index:1'><x:ClientData ObjectType=\"Spin\">\r\n   <x:Val>7</x:Val>\r\n  </x:ClientData></v:shape>".to_string();
        assert_eq!(shape_rect(&b), Some([10.0, 10.0, 60.0, 20.0]));
        set_rect(&mut b, [33.5, 1.0, 44.5, 2.0]);
        assert_eq!(shape_rect(&b), Some([33.5, 1.0, 44.5, 2.0]));
        assert_eq!(elem(&b, "Val").as_deref(), Some("7"));
        set_elem(&mut b, "Val", Some("9"));
        set_elem(&mut b, "Min", Some("1"));
        assert_eq!(elem(&b, "Val").as_deref(), Some("9"));
        assert_eq!(elem(&b, "Min").as_deref(), Some("1"));
        set_elem(&mut b, "Min", None);
        assert_eq!(elem(&b, "Min"), None);
    }

    #[test]
    fn new_shape_reads_back() {
        let mut c = Control::new(ControlKind::Spin, "Spinner 3", [10.0, 70.0, 20.0, 30.0]);
        c.shape_id = 1027;
        c.fmla_link = "E3".into();
        c.val = 7;
        c.min = 1;
        c.max = 10;
        c.inc = 2;
        let s = vml_shape(&c, 3);
        assert_eq!(shape_rect(&s), Some([10.0, 70.0, 20.0, 30.0]));
        assert_eq!(elem(&s, "FmlaLink").as_deref(), Some("E3"));
        assert_eq!(elem(&s, "Max").as_deref(), Some("10"));
        assert!(s.contains("0, 20, 4, 24, 0, 60, 6, 26"), "{s}");
        let p = String::from_utf8(ctrl_prop_new(&c)).unwrap();
        assert!(p.contains("objectType=\"Spin\" dx=\"31\" fmlaLink=\"E3\" inc=\"2\" max=\"10\" min=\"1\" page=\"10\" val=\"7\""), "{p}");
    }
}
