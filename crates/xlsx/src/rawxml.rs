use quick_xml::events::Event;
use quick_xml::Reader;

#[derive(Clone, Debug)]
pub struct Kid {
    pub name: String,
    pub raw: String,
}

#[derive(Clone, Debug)]
pub struct Split {
    pub root: Tag,
    pub kids: Vec<Kid>,
}

fn local(s: &str) -> String {
    match s.rsplit_once(':') {
        Some((_, l)) => l.to_string(),
        None => s.to_string(),
    }
}

pub fn split(bytes: &[u8]) -> Result<Split, String> {
    let text = std::str::from_utf8(bytes).map_err(|e| e.to_string())?;
    let text = text.trim_start_matches('\u{feff}');
    let mut rd = Reader::from_str(text);
    let mut depth = 0usize;
    let mut root: Option<Tag> = None;
    let mut kids = Vec::new();
    let mut start = 0usize;
    let mut name = String::new();
    loop {
        let before = rd.buffer_position() as usize;
        let ev = rd.read_event().map_err(|e| e.to_string())?;
        let after = rd.buffer_position() as usize;
        match ev {
            Event::Start(e) => {
                if depth == 0 {
                    root = Some(Tag::parse(&text[before..after])?);
                } else if depth == 1 {
                    start = before;
                    name = local(e.name().as_ref());
                }
                depth += 1;
            }
            Event::Empty(e) => {
                if depth == 0 {
                    let mut t = Tag::parse(&text[before..after])?;
                    t.empty = false;
                    return Ok(Split {
                        root: t,
                        kids: Vec::new(),
                    });
                }
                if depth == 1 {
                    kids.push(Kid {
                        name: local(e.name().as_ref()),
                        raw: text[before..after].to_string(),
                    });
                }
            }
            Event::End(_) => {
                depth = depth.checked_sub(1).ok_or("unbalanced XML")?;
                if depth == 1 {
                    kids.push(Kid {
                        name: std::mem::take(&mut name),
                        raw: text[start..after].to_string(),
                    });
                }
                if depth == 0 {
                    break;
                }
            }
            Event::Eof => return Err("unexpected end of XML".into()),
            _ => {}
        }
    }
    Ok(Split {
        root: root.ok_or("no root element")?,
        kids,
    })
}

impl Split {
    pub fn kid(&self, name: &str) -> Option<&Kid> {
        self.kids.iter().find(|k| k.name == name)
    }

    pub fn put(&mut self, name: &str, raw: Option<String>, order: &[&str]) {
        let at = self.kids.iter().position(|k| k.name == name);
        self.kids.retain(|k| k.name != name);
        let Some(raw) = raw else { return };
        let kid = Kid {
            name: name.to_string(),
            raw,
        };
        let idx = match at {
            Some(i) => i,
            None => {
                let rank = order.iter().position(|n| *n == name).unwrap_or(order.len());
                self.kids
                    .iter()
                    .rposition(|k| {
                        order
                            .iter()
                            .position(|n| *n == k.name)
                            .is_some_and(|r| r < rank)
                    })
                    .map_or(0, |i| i + 1)
            }
        };
        self.kids.insert(idx.min(self.kids.len()), kid);
    }

    pub fn write(&self) -> Vec<u8> {
        let mut s = String::from(DECL);
        s.push_str(&self.root.open());
        for k in &self.kids {
            s.push_str(&k.raw);
        }
        s.push_str("</");
        s.push_str(&self.root.name);
        s.push('>');
        s.into_bytes()
    }
}

pub const DECL: &str = "<?xml version=\"1.0\" encoding=\"UTF-8\" standalone=\"yes\"?>\r\n";

#[derive(Clone, Debug, Default)]
pub struct Tag {
    pub name: String,
    pub attrs: Vec<(String, String)>,
    pub empty: bool,
}

impl Tag {
    pub fn new(name: &str) -> Tag {
        Tag {
            name: name.to_string(),
            ..Tag::default()
        }
    }

    pub fn parse(raw: &str) -> Result<Tag, String> {
        let mut rd = Reader::from_str(raw);
        let (e, empty) = match rd.read_event().map_err(|e| e.to_string())? {
            Event::Start(e) => (e, false),
            Event::Empty(e) => (e, true),
            _ => return Err(format!("not a start tag: {raw}")),
        };
        let mut t = Tag {
            name: e.name().as_ref().to_string(),
            attrs: Vec::new(),
            empty,
        };
        for a in e.attributes() {
            let a = a.map_err(|x| x.to_string())?;
            t.attrs
                .push((a.key.as_ref().to_string(), a.value.to_string()));
        }
        Ok(t)
    }

    pub fn get(&self, key: &str) -> Option<&str> {
        self.attrs
            .iter()
            .find(|(k, _)| k == key)
            .map(|(_, v)| v.as_str())
    }

    pub fn value(&self, key: &str) -> Option<String> {
        self.get(key).map(unescape)
    }

    pub fn set(&mut self, key: &str, value: Option<&str>) -> &mut Tag {
        match value {
            None => self.attrs.retain(|(k, _)| k != key),
            Some(v) => {
                let v = escape_attr(v);
                match self.attrs.iter_mut().find(|(k, _)| k == key) {
                    Some(slot) => slot.1 = v,
                    None => self.attrs.push((key.to_string(), v)),
                }
            }
        }
        self
    }

    pub fn prefix_of(&self, uri: &str) -> Option<String> {
        self.attrs.iter().find_map(|(k, v)| {
            if v != uri {
                return None;
            }
            if k == "xmlns" {
                Some(String::new())
            } else {
                k.strip_prefix("xmlns:").map(str::to_string)
            }
        })
    }

    fn attrs_text(&self) -> String {
        let mut s = String::new();
        for (k, v) in &self.attrs {
            s.push(' ');
            s.push_str(k);
            s.push_str("=\"");
            s.push_str(v);
            s.push('"');
        }
        s
    }

    pub fn open(&self) -> String {
        format!("<{}{}>", self.name, self.attrs_text())
    }

    pub fn empty(&self) -> String {
        format!("<{}{}/>", self.name, self.attrs_text())
    }

    pub fn write(&self) -> String {
        if self.empty {
            self.empty()
        } else {
            self.open()
        }
    }
}

pub const MAIN_NS: &str = "http://schemas.openxmlformats.org/spreadsheetml/2006/main";

pub const APP_NS: &str =
    "http://schemas.openxmlformats.org/officeDocument/2006/extended-properties";

pub fn unprefix(bytes: &[u8], ns: &str) -> Option<Vec<u8>> {
    let text = std::str::from_utf8(bytes).ok()?;
    let sp = split(bytes).ok()?;
    let (prefix, _) = sp.root.name.split_once(':')?;
    if sp.root.prefix_of(ns).as_deref() != Some(prefix) {
        return None;
    }
    let mut out = text
        .replace(&format!("<{prefix}:"), "<")
        .replace(&format!("</{prefix}:"), "</");
    if sp.root.get("xmlns").is_none() {
        let local = &sp.root.name[prefix.len() + 1..];
        let at = out.find(&format!("<{local}"))? + local.len() + 1;
        out.insert_str(at, &format!(" xmlns=\"{ns}\""));
    }
    Some(out.into_bytes())
}

pub fn head_len(raw: &str) -> Option<usize> {
    let mut rd = Reader::from_str(raw);
    rd.read_event().ok()?;
    Some(rd.buffer_position() as usize)
}

pub fn escape_text(s: &str) -> String {
    let mut o = String::with_capacity(s.len());
    for c in s.chars() {
        match c {
            '&' => o.push_str("&amp;"),
            '<' => o.push_str("&lt;"),
            '>' => o.push_str("&gt;"),
            c => o.push(c),
        }
    }
    o
}

pub fn escape_attr(s: &str) -> String {
    let mut o = String::with_capacity(s.len());
    for c in s.chars() {
        match c {
            '&' => o.push_str("&amp;"),
            '<' => o.push_str("&lt;"),
            '>' => o.push_str("&gt;"),
            '"' => o.push_str("&quot;"),
            '\t' => o.push_str("&#9;"),
            '\n' => o.push_str("&#10;"),
            '\r' => o.push_str("&#13;"),
            c => o.push(c),
        }
    }
    o
}

pub fn unescape(raw: &str) -> String {
    quick_xml::escape::unescape(raw).map_or_else(|_| raw.to_string(), |c| c.into_owned())
}

pub fn xstring(s: &str) -> String {
    let mut o = String::with_capacity(s.len());
    let b: Vec<char> = s.chars().collect();
    let mut i = 0;
    while i < b.len() {
        let c = b[i];
        let looks_escaped = c == '_'
            && b.len() >= i + 7
            && b[i + 1] == 'x'
            && b[i + 2..i + 6].iter().all(char::is_ascii_hexdigit)
            && b[i + 6] == '_';
        if looks_escaped {
            o.push_str("_x005F_");
        } else if (c < ' ' && !matches!(c, '\t' | '\n' | '\r'))
            || c == '\u{fffe}'
            || c == '\u{ffff}'
        {
            o.push_str(&format!("_x{:04X}_", c as u32));
        } else {
            o.push(c);
        }
        i += 1;
    }
    o
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn unprefixed_main() {
        let x = format!(
            "<?xml version=\"1.0\"?><x:workbook xmlns:x=\"{MAIN_NS}\"><x:sheets><x:sheet name=\"A\"/></x:sheets></x:workbook>"
        );
        let out = String::from_utf8(unprefix(x.as_bytes(), MAIN_NS).unwrap()).unwrap();
        assert_eq!(
            out,
            format!(
                "<?xml version=\"1.0\"?><workbook xmlns=\"{MAIN_NS}\" xmlns:x=\"{MAIN_NS}\"><sheets><sheet name=\"A\"/></sheets></workbook>"
            )
        );
        assert!(unprefix(out.as_bytes(), MAIN_NS).is_none());
    }

    #[test]
    fn split_and_put() {
        let x = b"<?xml version=\"1.0\"?>\n<a xmlns=\"u\" x:k=\"1\"><b/> <c><d>t</d></c><e q=\"&amp;\"/></a>";
        let mut s = split(x).unwrap();
        assert_eq!(s.root.name, "a");
        assert_eq!(s.root.get("x:k"), Some("1"));
        let names: Vec<_> = s.kids.iter().map(|k| k.name.as_str()).collect();
        assert_eq!(names, ["b", "c", "e"]);
        assert_eq!(s.kids[1].raw, "<c><d>t</d></c>");
        assert_eq!(
            Tag::parse(&s.kids[2].raw).unwrap().value("q").as_deref(),
            Some("&")
        );
        s.put("d2", Some("<d2/>".into()), &["b", "c", "d2", "e"]);
        s.put("c", None, &[]);
        let out = String::from_utf8(s.write()).unwrap();
        assert!(out.ends_with("<a xmlns=\"u\" x:k=\"1\"><b/><d2/><e q=\"&amp;\"/></a>"));
    }

    #[test]
    fn xstrings() {
        assert_eq!(xstring("a\u{1}b"), "a_x0001_b");
        assert_eq!(xstring("_x0041_"), "_x005F_x0041_");
        assert_eq!(
            crate::ftext::unescape_xstring(&xstring("_x0041_\u{2}")),
            "_x0041_\u{2}"
        );
    }
}
