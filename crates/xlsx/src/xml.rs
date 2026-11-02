use quick_xml::events::Event;
use quick_xml::Reader;

#[derive(Debug, Default, Clone)]
pub struct Node {
    pub name: String,
    pub attrs: Vec<(String, String)>,
    pub kids: Vec<Node>,
    pub text: String,
}

impl Node {
    pub fn attr(&self, key: &str) -> Option<&str> {
        self.attrs
            .iter()
            .find(|(k, _)| k == key)
            .map(|(_, v)| v.as_str())
    }

    pub fn child(&self, name: &str) -> Option<&Node> {
        self.kids.iter().find(|k| k.name == name)
    }

    pub fn children<'a>(&'a self, name: &'a str) -> impl Iterator<Item = &'a Node> + 'a {
        self.kids.iter().filter(move |k| k.name == name)
    }
}

fn local(name: &str) -> String {
    let s = name;
    match s.rsplit_once(':') {
        Some((_, l)) => l.to_string(),
        None => s.to_string(),
    }
}

fn element(e: &quick_xml::events::BytesStart) -> Result<Node, String> {
    let mut n = Node {
        name: local(e.name().as_ref()),
        ..Node::default()
    };
    for a in e.attributes() {
        let a = a.map_err(|x| x.to_string())?;
        let key = local(a.key.as_ref());

        if key == "xmlns" || a.key.as_ref().starts_with("xmlns:") {
            continue;
        }
        let v = a
            .normalized_value(quick_xml::XmlVersion::Implicit1_0)
            .map_err(|x| x.to_string())?
            .to_string();
        n.attrs.push((key, v));
    }
    Ok(n)
}

pub fn parse(bytes: &[u8]) -> Result<Node, String> {
    let mut rd = Reader::from_reader(bytes);
    let mut buf = Vec::new();
    let mut stack: Vec<Node> = vec![Node::default()];
    loop {
        match rd.read_event_into(&mut buf).map_err(|e| e.to_string())? {
            Event::Start(e) => stack.push(element(&e)?),
            Event::Empty(e) => {
                let n = element(&e)?;
                stack.last_mut().unwrap().kids.push(n);
            }
            Event::End(_) => {
                let n = stack.pop().ok_or("unbalanced XML")?;
                stack.last_mut().ok_or("unbalanced XML")?.kids.push(n);
            }
            Event::Text(t) => {
                let s = t.xml10_content();
                stack.last_mut().unwrap().text.push_str(&s);
            }
            Event::CData(t) => {
                let s = t.xml10_content();
                stack.last_mut().unwrap().text.push_str(&s);
            }
            Event::GeneralRef(r) => {
                let name = r.xml10_content();
                let ch = match &*name {
                    "amp" => '&',
                    "lt" => '<',
                    "gt" => '>',
                    "quot" => '"',
                    "apos" => '\'',
                    _ => r
                        .resolve_char_ref()
                        .map_err(|e| e.to_string())?
                        .ok_or_else(|| format!("unknown entity &{name};"))?,
                };
                stack.last_mut().unwrap().text.push(ch);
            }
            Event::Eof => break,
            _ => {}
        }
        buf.clear();
    }
    let mut root = stack.pop().ok_or("empty XML")?;
    if !stack.is_empty() || root.kids.len() != 1 {
        return Err("malformed XML".into());
    }
    Ok(root.kids.remove(0))
}
