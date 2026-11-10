use std::collections::HashMap;

pub(crate) const XMLNS_URI: &str = "http://www.w3.org/2000/xmlns/";
pub(crate) const XML_URI: &str = "http://www.w3.org/XML/1998/namespace";

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub(crate) enum Kind {
    Element,
    Attribute,
    Text,
    CData,
    EntityRef,
    Pi,
    Comment,
    Document,
    Fragment,
}

impl Kind {
    pub(crate) fn code(self) -> i32 {
        match self {
            Kind::Element => 1,
            Kind::Attribute => 2,
            Kind::Text => 3,
            Kind::CData => 4,
            Kind::EntityRef => 5,
            Kind::Pi => 7,
            Kind::Comment => 8,
            Kind::Document => 9,
            Kind::Fragment => 11,
        }
    }

    pub(crate) fn type_string(self) -> &'static str {
        match self {
            Kind::Element => "element",
            Kind::Attribute => "attribute",
            Kind::Text => "text",
            Kind::CData => "cdatasection",
            Kind::EntityRef => "entityreference",
            Kind::Pi => "processinginstruction",
            Kind::Comment => "comment",
            Kind::Document => "document",
            Kind::Fragment => "documentfragment",
        }
    }
}

#[derive(Clone, Debug, Default)]
pub(crate) struct ParseErr {
    pub(crate) code: i32,
    pub(crate) reason: String,
    pub(crate) line: i32,
    pub(crate) linepos: i32,
    pub(crate) src: String,
    pub(crate) filepos: i32,
    pub(crate) url: Option<String>,
}

#[derive(Clone, Debug)]
pub(crate) struct DocData {
    pub(crate) legacy: bool,
    pub(crate) async_: bool,
    pub(crate) validate: bool,
    pub(crate) resolve_ext: bool,
    pub(crate) preserve: bool,
    pub(crate) xpath: bool,
    pub(crate) sel_ns: Option<String>,
    pub(crate) prohibit_dtd: bool,
    pub(crate) new_parser: bool,
    pub(crate) server_http: bool,
    pub(crate) url: Option<String>,
    pub(crate) ready: i32,
    pub(crate) err: ParseErr,

    pub(crate) encoding: Option<String>,
}

impl DocData {
    pub(crate) fn new(legacy: bool) -> DocData {
        DocData {
            legacy,
            async_: true,
            validate: true,
            resolve_ext: legacy,
            preserve: false,
            xpath: !legacy,
            sel_ns: None,
            prohibit_dtd: !legacy,
            new_parser: false,
            server_http: false,
            url: None,
            ready: 4,
            err: ParseErr::default(),
            encoding: None,
        }
    }
}

#[derive(Clone, Debug)]
pub(crate) struct Node {
    pub(crate) kind: Kind,

    pub(crate) name: String,

    pub(crate) value: String,
    pub(crate) ns: Option<String>,
    pub(crate) parent: Option<usize>,

    pub(crate) owner: Option<usize>,
    pub(crate) doc: usize,
    pub(crate) children: Vec<usize>,
    pub(crate) attrs: Vec<usize>,

    pub(crate) ws_after: bool,

    pub(crate) ws_start: bool,

    pub(crate) explicit_end: bool,

    pub(crate) synth_ns: bool,
    pub(crate) data: Option<Box<DocData>>,
}

#[derive(Debug, Default)]
pub(crate) struct Store {
    pub(crate) nodes: Vec<Node>,

    pub(crate) handles: HashMap<usize, usize>,

    pub(crate) keep_enc: bool,
}

fn blank(kind: Kind, name: &str, doc: usize) -> Node {
    Node {
        kind,
        name: name.to_string(),
        value: String::new(),
        ns: None,
        parent: None,
        owner: None,
        doc,
        children: Vec::new(),
        attrs: Vec::new(),
        ws_after: false,
        ws_start: false,
        explicit_end: false,
        synth_ns: false,
        data: None,
    }
}

pub(crate) fn is_ws(c: char) -> bool {
    matches!(c, ' ' | '\t' | '\n' | '\r')
}

pub(crate) fn is_name_start(c: char) -> bool {
    c == '_' || c == ':' || c.is_alphabetic() || (c as u32 >= 0xC0 && c.is_alphanumeric())
}

pub(crate) fn is_name_char(c: char) -> bool {
    is_name_start(c)
        || c.is_alphanumeric()
        || c == '-'
        || c == '.'
        || c == '\u{B7}'
        || (c.is_numeric() && c as u32 > 0x7F)
}

pub(crate) fn is_xml_char(c: char) -> bool {
    matches!(c, '\t' | '\n' | '\r')
        || (c as u32 >= 0x20 && c as u32 != 0xFFFE && c as u32 != 0xFFFF)
}

impl Store {
    pub(crate) fn new_doc(&mut self, legacy: bool) -> usize {
        let id = self.nodes.len();
        let mut n = blank(Kind::Document, "#document", id);
        n.data = Some(Box::new(DocData::new(legacy)));
        self.nodes.push(n);
        id
    }

    pub(crate) fn new_node(&mut self, kind: Kind, name: &str, doc: usize) -> usize {
        let name = match kind {
            Kind::Text => "#text",
            Kind::CData => "#cdata-section",
            Kind::Comment => "#comment",
            Kind::Fragment => "#document-fragment",
            _ => name,
        };
        self.nodes.push(blank(kind, name, doc));
        self.nodes.len() - 1
    }

    pub(crate) fn doc_data(&self, doc: usize) -> &DocData {
        self.nodes[doc].data.as_deref().expect("document node")
    }

    pub(crate) fn doc_data_mut(&mut self, doc: usize) -> &mut DocData {
        self.nodes[doc].data.as_deref_mut().expect("document node")
    }

    pub(crate) fn preserve(&self, n: usize) -> bool {
        self.doc_data(self.nodes[n].doc).preserve
    }

    pub(crate) fn up(&self, n: usize) -> Option<usize> {
        let node = &self.nodes[n];
        node.parent.or(node.owner)
    }

    pub(crate) fn detach(&mut self, n: usize) {
        if let Some(p) = self.nodes[n].parent.take() {
            self.nodes[p].children.retain(|&c| c != n);
        }
        if let Some(o) = self.nodes[n].owner.take() {
            self.nodes[o].attrs.retain(|&c| c != n);
        }
    }

    pub(crate) fn set_doc(&mut self, n: usize, doc: usize) {
        self.nodes[n].doc = doc;
        let kids: Vec<usize> = self.nodes[n]
            .children
            .iter()
            .chain(self.nodes[n].attrs.iter())
            .copied()
            .collect();
        for k in kids {
            self.set_doc(k, doc);
        }
    }

    pub(crate) fn append(&mut self, parent: usize, child: usize) {
        self.detach(child);
        let doc = self.nodes[parent].doc;
        if self.nodes[child].doc != doc {
            self.set_doc(child, doc);
        }
        self.nodes[child].parent = Some(parent);
        self.nodes[parent].children.push(child);
    }

    pub(crate) fn insert_at(&mut self, parent: usize, at: usize, child: usize) {
        self.detach(child);
        let doc = self.nodes[parent].doc;
        if self.nodes[child].doc != doc {
            self.set_doc(child, doc);
        }
        self.nodes[child].parent = Some(parent);
        let at = at.min(self.nodes[parent].children.len());
        self.nodes[parent].children.insert(at, child);
    }

    pub(crate) fn index_in_parent(&self, n: usize) -> Option<usize> {
        let p = self.nodes[n].parent?;
        self.nodes[p].children.iter().position(|&c| c == n)
    }

    pub(crate) fn is_ancestor(&self, anc: usize, n: usize) -> bool {
        let mut cur = Some(n);
        while let Some(c) = cur {
            if c == anc {
                return true;
            }
            cur = self.up(c);
        }
        false
    }

    pub(crate) fn clear_children(&mut self, n: usize) {
        for c in std::mem::take(&mut self.nodes[n].children) {
            self.nodes[c].parent = None;
        }
    }

    pub(crate) fn new_attr(&mut self, name: &str, value: &str, doc: usize) -> usize {
        let a = self.new_node(Kind::Attribute, name, doc);
        self.set_attr_value(a, value);
        a
    }

    pub(crate) fn set_attr_value(&mut self, a: usize, value: &str) {
        let value = &value.replace("\r\n", "\n").replace('\r', "\n");
        self.clear_children(a);
        if !value.is_empty() {
            let doc = self.nodes[a].doc;
            let t = self.new_node(Kind::Text, "", doc);
            self.nodes[t].value = value.to_string();
            self.nodes[t].parent = Some(a);
            self.nodes[a].children.push(t);
        }
    }

    pub(crate) fn attr_value(&self, a: usize) -> String {
        self.nodes[a]
            .children
            .iter()
            .map(|&c| match self.nodes[c].kind {
                Kind::EntityRef => format!("&{};", self.nodes[c].name),
                _ => self.nodes[c].value.clone(),
            })
            .collect()
    }

    pub(crate) fn find_attr(&self, el: usize, name: &str) -> Option<usize> {
        self.nodes[el]
            .attrs
            .iter()
            .copied()
            .find(|&a| self.nodes[a].name == name)
    }

    pub(crate) fn get_attr(&self, el: usize, name: &str) -> Option<String> {
        self.find_attr(el, name).map(|a| self.attr_value(a))
    }

    pub(crate) fn put_attr(&mut self, el: usize, name: &str, value: &str) {
        if let Some(a) = self.find_attr(el, name) {
            self.set_attr_value(a, value);
            return;
        }
        let doc = self.nodes[el].doc;
        let a = self.new_attr(name, value, doc);
        if name == "xmlns" || name.starts_with("xmlns:") {
            self.nodes[a].ns = Some(XMLNS_URI.to_string());
        } else if name.starts_with("xml:") {
            self.nodes[a].ns = Some(XML_URI.to_string());
        }
        self.nodes[a].owner = Some(el);
        self.nodes[el].attrs.push(a);
    }

    pub(crate) fn prefix(&self, n: usize) -> Option<&str> {
        let node = &self.nodes[n];
        if !matches!(node.kind, Kind::Element | Kind::Attribute) {
            return None;
        }
        match node.name.split_once(':') {
            Some((p, _)) => Some(p),
            None if node.kind == Kind::Attribute && node.name == "xmlns" => None,
            None => None,
        }
    }

    pub(crate) fn base_name(&self, n: usize) -> String {
        let node = &self.nodes[n];
        match node.kind {
            Kind::Element | Kind::Attribute | Kind::Pi | Kind::EntityRef => node
                .name
                .split_once(':')
                .map_or(node.name.clone(), |(_, b)| b.to_string()),
            _ => String::new(),
        }
    }

    pub(crate) fn clone_node(&mut self, n: usize, deep: bool) -> usize {
        let src = self.nodes[n].clone();
        let id = self.nodes.len();
        let mut copy = src.clone();
        copy.parent = None;
        copy.owner = None;
        copy.children = Vec::new();
        copy.attrs = Vec::new();
        copy.data = src.data.clone();
        if src.kind == Kind::Document {
            copy.doc = id;
        }
        if !deep {
            copy.ws_after = false;
        }
        self.nodes.push(copy);
        for a in src.attrs {
            let ac = self.clone_node(a, true);
            self.nodes[ac].owner = Some(id);
            self.nodes[ac].doc = self.nodes[id].doc;
            self.nodes[id].attrs.push(ac);
        }
        if src.kind == Kind::Attribute || deep {
            for c in src.children {
                let cc = self.clone_node(c, true);
                self.nodes[cc].parent = Some(id);
                let doc = self.nodes[id].doc;
                self.set_doc(cc, doc);
                self.nodes[id].children.push(cc);
            }
        }
        id
    }

    pub(crate) fn text(&self, n: usize) -> String {
        let node = &self.nodes[n];
        match node.kind {
            Kind::Text => self.trim_text(n, &node.value, true, true),
            Kind::CData | Kind::Comment | Kind::Pi => node.value.clone(),
            Kind::EntityRef => String::new(),
            Kind::Attribute => {
                let v = self.attr_value(n);
                self.trim_text(n, &v, true, true)
            }
            _ => {
                let mut pieces: Vec<(String, bool)> = Vec::new();
                self.collect_text(n, &mut pieces);
                let last = pieces.len().saturating_sub(1);
                let mut out = String::new();
                for (i, (s, is_text)) in pieces.iter().enumerate() {
                    if *is_text {
                        out.push_str(&self.trim_text(n, s, i == 0, i == last));
                    } else {
                        out.push_str(s);
                    }
                }
                out
            }
        }
    }

    fn collect_text(&self, n: usize, out: &mut Vec<(String, bool)>) {
        for &c in &self.nodes[n].children {
            match self.nodes[c].kind {
                Kind::Text => out.push((self.nodes[c].value.clone(), true)),
                Kind::CData => out.push((self.nodes[c].value.clone(), false)),
                Kind::Element | Kind::EntityRef => self.collect_text(c, out),
                _ => {}
            }
        }
    }

    fn trim_text(&self, n: usize, s: &str, start: bool, end: bool) -> String {
        if self.preserve(n) {
            return s.to_string();
        }
        let mut t = s;
        if start {
            t = t.trim_start_matches(is_ws);
        }
        if end {
            t = t.trim_end_matches(is_ws);
        }
        t.to_string()
    }

    pub(crate) fn xml(&self, n: usize) -> String {
        let mut out = String::new();
        let mut scope = Vec::new();
        self.write_node(n, 0, &mut out, &mut scope);
        out
    }

    pub(crate) fn legacy(&self, n: usize) -> bool {
        self.doc_data(self.nodes[n].doc).legacy
    }

    fn fmt_on(&self, n: usize) -> bool {
        !self.preserve(n) || self.legacy(n)
    }

    fn write_children(
        &self,
        parent: usize,
        depth: usize,
        out: &mut String,
        scope: &mut Vec<(String, String)>,
    ) {
        let kids = &self.nodes[parent].children;
        let fmt = self.fmt_on(parent);
        let p = &self.nodes[parent];
        let nl = |out: &mut String, d: usize| {
            out.push_str("\r\n");
            for _ in 0..d {
                out.push('\t');
            }
        };
        if fmt && p.ws_start {
            match kids.first() {
                None => nl(out, depth),
                Some(&k) if self.nodes[k].kind != Kind::Text => nl(out, depth + 1),
                Some(_) => {}
            }
        }
        for (i, &k) in kids.iter().enumerate() {
            self.write_node(k, depth + 1, out, scope);
            if fmt && self.nodes[k].ws_after {
                match kids.get(i + 1) {
                    Some(&nx) if self.nodes[nx].kind == Kind::Text => {}
                    Some(_) => nl(out, depth + 1),
                    None => nl(out, depth),
                }
            }
        }
    }

    fn write_node(
        &self,
        n: usize,
        depth: usize,
        out: &mut String,
        scope: &mut Vec<(String, String)>,
    ) {
        let node = &self.nodes[n];
        match node.kind {
            Kind::Document => {
                let legacy = self.doc_data(n).legacy;
                let crlf = !self.preserve(n) || legacy;
                for &c in &node.children {
                    self.write_node(c, 0, out, scope);
                    if crlf {
                        out.push_str("\r\n");
                    }
                }
            }
            Kind::Fragment => self.write_children(n, depth, out, scope),
            Kind::Text => escape_text(&node.value, out),
            Kind::CData => {
                out.push_str("<![CDATA[");
                out.push_str(&node.value);
                out.push_str("]]>");
            }
            Kind::Comment => {
                out.push_str("<!--");
                out.push_str(&node.value);
                out.push_str("-->");
            }
            Kind::EntityRef => {
                out.push('&');
                out.push_str(&node.name);
                out.push(';');
            }
            Kind::Pi => {
                out.push_str("<?");
                out.push_str(&node.name);
                if node.name == "xml" {
                    self.write_xml_decl(n, out);
                } else if !node.value.is_empty() || self.legacy(n) {
                    out.push(' ');
                    out.push_str(&node.value);
                }
                out.push_str("?>");
            }
            Kind::Attribute => {
                out.push_str(&node.name);
                out.push_str("=\"");
                escape_attr(&self.attr_value(n), self.legacy(n), out);
                out.push('"');
            }
            Kind::Element => {
                let mark = scope.len();
                out.push('<');
                out.push_str(&node.name);
                if node.synth_ns {
                    if let Some(uri) = &node.ns {
                        let pfx = node.name.split_once(':').map_or("", |(p, _)| p);
                        out.push_str(if pfx.is_empty() { " xmlns" } else { " xmlns:" });
                        out.push_str(pfx);
                        out.push_str("=\"");
                        escape_attr(uri, self.legacy(n), out);
                        out.push('"');
                        scope.push((pfx.to_string(), uri.clone()));
                    }
                }
                for &a in &node.attrs {
                    out.push(' ');
                    self.write_node(a, depth, out, scope);
                }
                let fmt = self.fmt_on(n);
                let bare =
                    node.children.is_empty() && !node.explicit_end && !(fmt && node.ws_start);
                if bare {
                    out.push_str("/>");
                } else {
                    out.push('>');
                    self.write_children(n, depth, out, scope);
                    out.push_str("</");
                    out.push_str(&node.name);
                    out.push('>');
                }
                scope.truncate(mark);
            }
        }
    }

    fn write_xml_decl(&self, n: usize, out: &mut String) {
        let node = &self.nodes[n];
        let get = |name: &str| {
            node.attrs
                .iter()
                .find(|&&a| self.nodes[a].name == name)
                .map(|&a| self.attr_value(a))
        };
        if let Some(v) = get("version") {
            out.push_str(&format!(" version=\"{v}\""));
        }
        if let Some(e) = get("encoding") {
            if self.keep_enc || e.eq_ignore_ascii_case("utf-16") {
                out.push_str(&format!(" encoding=\"{e}\""));
            }
        }
        if let Some(s) = get("standalone") {
            out.push_str(&format!(" standalone=\"{s}\""));
        }
    }
}

pub(crate) fn escape_text(s: &str, out: &mut String) {
    for c in s.chars() {
        match c {
            '<' => out.push_str("&lt;"),
            '>' => out.push_str("&gt;"),
            '&' => out.push_str("&amp;"),
            '\n' => out.push_str("\r\n"),
            _ => out.push(c),
        }
    }
}

pub(crate) fn escape_attr(s: &str, legacy: bool, out: &mut String) {
    for c in s.chars() {
        match c {
            '<' => out.push_str("&lt;"),
            '>' => out.push_str("&gt;"),
            '&' => out.push_str("&amp;"),
            '"' => out.push_str("&quot;"),
            '\n' if legacy => out.push_str("\r\n"),
            '\n' => out.push_str("&#xA;"),
            '\r' => out.push_str("&#xD;"),
            _ => out.push(c),
        }
    }
}

fn reason(code: i32) -> &'static str {
    match code {
        -1072894385 => "The namespace for 'xmlns:xml' is reserved and it can only be declared as 'http://www.w3.org/XML/1998/namespace'.",
        -1072896636 => "DTD is prohibited.",
        -1072896641 => "Invalid version number.",
        -1072896647 => "The standalone attribute must have the value 'yes' or 'no'.",
        -1072896657 => "Switch from current encoding to specified encoding not supported.",
        -1072896659 => "End tag does not match the start tag.",
        -1072896663 => "The 'version' attribute is required at this location.",
        -1072896664 => "Prefixes 'xml' and 'xmlns' are reserved for use by XML.",
        -1072896668 => "A CDATA section was not closed.",
        -1072896670 => "A declaration was not closed.",
        -1072896671 => "A comment was not closed.",
        -1072896672 => "A string literal was not closed.",
        -1072896674 => "Element was not closed.",
        -1072896676 => "The literal string ']]>' is not allowed in element content.",
        -1072896680 => "XML document must have a top level element.",
        -1072896681 => "Invalid xml declaration.",
        -1072896682 => "Invalid at the top level of the document.",
        -1072896683 => "Only one top level element is allowed in an XML document.",
        -1072896684 => "Duplicate attribute.",
        -1072896686 => "End tag was not expected at this location.",
        -1072896737 => "Invalid unicode character.",
        -1072896740 => "Multiple colons are not allowed in a name.",
        -1072896749 => "Whitespace is not allowed at this location.",
        -1072896755 => "A semi colon character was expected.",
        -1072896759 => "Required white space was missing.",
        -1072896760 => "An invalid character was found in text content.",
        -1072896761 => "Invalid syntax for an xml declaration.",
        -1072896762 => "The character '<' cannot be used in an attribute value.",
        -1072896763 => "A name contained an invalid character.",
        -1072896764 => "A name was started with an invalid character.",
        -1072896765 => "Incorrect syntax was used in a comment.",
        -1072896766 => "A string literal was expected, but no opening quote character was found.",
        -1072898009 => "Only a default namespace can have an empty URI.",
        _ => "",
    }
}

pub(crate) struct Failure {
    code: i32,
    reason: Option<String>,
    pos: Option<usize>,
}

struct Parser<'a> {
    st: &'a mut Store,
    doc: usize,
    c: Vec<char>,
    i: usize,
    preserve: bool,
    prohibit_dtd: bool,
    legacy: bool,
    validate: bool,
    has_dtd: bool,
}

type PResult<T> = Result<T, Failure>;

fn fail<T>(code: i32, pos: usize) -> PResult<T> {
    Err(Failure {
        code,
        reason: None,
        pos: Some(pos),
    })
}

impl Parser<'_> {
    fn peek(&self) -> Option<char> {
        self.c.get(self.i).copied()
    }

    fn at(&self, s: &str) -> bool {
        s.chars()
            .enumerate()
            .all(|(k, ch)| self.c.get(self.i + k) == Some(&ch))
    }

    fn skip_ws(&mut self) -> bool {
        let start = self.i;
        while self.peek().is_some_and(is_ws) {
            self.i += 1;
        }
        self.i > start
    }

    fn name(&mut self) -> PResult<String> {
        let start = self.i;
        match self.peek() {
            Some(c) if is_name_start(c) && c != ':' => {}
            Some(c) if is_ws(c) => return fail(-1072896749, self.i),
            _ => return fail(-1072896764, self.i),
        }
        while self.peek().is_some_and(is_name_char) {
            self.i += 1;
        }
        let name: String = self.c[start..self.i].iter().collect();
        let colons: Vec<usize> = name.match_indices(':').map(|(k, _)| k).collect();
        if colons.len() > 1 {
            return fail(-1072896740, start + name[..colons[1]].chars().count());
        }
        if let Some(&k) = colons.first() {
            if k + 1 == name.len() {
                return Ok(name);
            }
        }
        Ok(name)
    }

    fn char_ref_or_entity(&mut self, out: &mut String) -> PResult<()> {
        self.i += 1;
        if self.peek() == Some('#') {
            self.i += 1;
            let hex = self.peek() == Some('x');
            if hex {
                self.i += 1;
            }
            let start = self.i;
            while self
                .peek()
                .is_some_and(|c| c.is_digit(if hex { 16 } else { 10 }))
            {
                self.i += 1;
            }
            let digits: String = self.c[start..self.i].iter().collect();
            if self.peek() != Some(';') {
                return fail(-1072896755, self.i);
            }
            let v = u32::from_str_radix(&digits, if hex { 16 } else { 10 }).ok();
            let ch = v.and_then(char::from_u32).filter(|&c| is_xml_char(c));
            match ch {
                Some('\r') => out.push('\n'),
                Some(c) => out.push(c),
                None => return fail(-1072896737, self.i),
            }
            self.i += 1;
            return Ok(());
        }
        let start = self.i;
        match self.peek() {
            Some(c) if is_name_start(c) => {}
            _ => return fail(-1072896764, self.i),
        }
        while self.peek().is_some_and(is_name_char) {
            self.i += 1;
        }
        let name: String = self.c[start..self.i].iter().collect();
        if self.peek() != Some(';') {
            return fail(-1072896755, self.i);
        }
        let ch = match name.as_str() {
            "amp" => '&',
            "lt" => '<',
            "gt" => '>',
            "quot" => '"',
            "apos" => '\'',
            _ => {
                return Err(Failure {
                    code: -1072898046,
                    reason: Some(format!("Reference to undefined entity '{name}'.")),
                    pos: Some(start),
                })
            }
        };
        out.push(ch);
        self.i += 1;
        Ok(())
    }

    fn attr_value(&mut self) -> PResult<String> {
        let q = match self.peek() {
            Some(q @ ('"' | '\'')) => q,
            _ => return fail(-1072896766, self.i),
        };
        self.i += 1;
        let mut out = String::new();
        let start = self.i;
        loop {
            match self.peek() {
                None => return fail(-1072896672, start),
                Some(c) if c == q => {
                    self.i += 1;
                    return Ok(out);
                }
                Some('<') => return fail(-1072896762, self.i),
                Some('&') => self.char_ref_or_entity(&mut out)?,
                Some('\r') => {
                    self.i += 1;
                    if self.peek() == Some('\n') {
                        self.i += 1;
                    }
                    out.push('\n');
                }
                Some(c) if !is_xml_char(c) => return fail(-1072896760, self.i),
                Some(c) => {
                    out.push(c);
                    self.i += 1;
                }
            }
        }
    }

    fn xml_decl(&mut self, at: usize) -> PResult<()> {
        if self.peek() == Some('?') {
            return fail(-1072896761, self.i);
        }
        let mut attrs: Vec<(String, String)> = Vec::new();
        loop {
            self.skip_ws();
            if self.at("?>") {
                self.i += 2;
                break;
            }
            let npos = self.i;
            let name = self.name().or_else(|_| fail(-1072896681, npos))?;
            if attrs.is_empty() && name != "version" {
                return fail(-1072896663, npos);
            }
            self.skip_ws();
            if self.peek() != Some('=') {
                return fail(-1072896681, self.i);
            }
            self.i += 1;
            self.skip_ws();
            let vpos = self.i + 1;
            let value = self.attr_value()?;
            match name.as_str() {
                "version" if value != "1.0" => return fail(-1072896641, vpos),
                "standalone" if value != "yes" && value != "no" => return fail(-1072896647, vpos),
                "version" | "encoding" | "standalone" => {}
                _ => return fail(-1072896681, npos),
            }
            attrs.push((name, value));
        }
        let pi = self.st.new_node(Kind::Pi, "xml", self.doc);
        let mut data = Vec::new();
        for (k, v) in &attrs {
            self.st.put_attr(pi, k, v);
            data.push(format!("{k}=\"{v}\""));
        }
        self.st.nodes[pi].value = data.join(" ");
        self.st.append(self.doc, pi);
        let _ = at;
        Ok(())
    }

    fn comment(&mut self, parent: usize) -> PResult<()> {
        self.i += 4;
        let start = self.i;
        let mut text = String::new();
        loop {
            if self.at("--") {
                if self.c.get(self.i + 2) == Some(&'>') {
                    self.i += 3;
                    break;
                }
                return fail(-1072896765, self.i + 2);
            }
            match self.peek() {
                None => return fail(-1072896671, start),
                Some('\r') => {
                    self.i += 1;
                    if self.peek() == Some('\n') {
                        self.i += 1;
                    }
                    text.push('\n');
                }
                Some(c) if !is_xml_char(c) => return fail(-1072896760, self.i),
                Some(c) => {
                    text.push(c);
                    self.i += 1;
                }
            }
        }
        let n = self.st.new_node(Kind::Comment, "", self.doc);
        self.st.nodes[n].value = text;
        self.st.append(parent, n);
        Ok(())
    }

    fn pi(&mut self, parent: usize) -> PResult<()> {
        self.i += 2;
        let tpos = self.i;
        let target = self.name()?;
        if target.eq_ignore_ascii_case("xml") {
            return fail(-1072896681, tpos);
        }
        let mut data = String::new();
        if !self.at("?>") {
            if !self.skip_ws() {
                return fail(-1072896763, self.i);
            }
            loop {
                if self.at("?>") {
                    break;
                }
                match self.peek() {
                    None => return fail(-1072896670, tpos),
                    Some('\r') => {
                        self.i += 1;
                        if self.peek() == Some('\n') {
                            self.i += 1;
                        }
                        data.push('\n');
                    }
                    Some(c) if !is_xml_char(c) => return fail(-1072896760, self.i),
                    Some(c) => {
                        data.push(c);
                        self.i += 1;
                    }
                }
            }
        }
        self.i += 2;
        let n = self.st.new_node(Kind::Pi, &target, self.doc);
        self.st.nodes[n].value = data;
        self.st.append(parent, n);
        Ok(())
    }

    fn flag_ws(&mut self, parent: usize) {
        match self.st.nodes[parent].children.last().copied() {
            Some(prev) => self.st.nodes[prev].ws_after = true,
            None => self.st.nodes[parent].ws_start = true,
        }
    }

    fn flush_text(&mut self, parent: usize, text: &mut String, has_ref: &mut bool, keep_ws: bool) {
        if text.is_empty() {
            return;
        }
        let drop_it = !*has_ref && !keep_ws && text.chars().all(is_ws);
        if drop_it {
            self.flag_ws(parent);
        } else {
            let n = self.st.new_node(Kind::Text, "", self.doc);
            self.st.nodes[n].value = std::mem::take(text);
            self.st.append(parent, n);
        }
        text.clear();
        *has_ref = false;
    }

    fn doctype(&mut self) -> PResult<()> {
        let name_pos = self.i + 10;
        if self.prohibit_dtd {
            return fail(-1072896636, name_pos.min(self.c.len()));
        }

        self.has_dtd = true;
        let mut depth = 0i32;
        while let Some(c) = self.peek() {
            self.i += 1;
            match c {
                '[' => depth += 1,
                ']' => depth -= 1,
                '>' if depth <= 0 => return Ok(()),
                _ => {}
            }
        }
        fail(-1072896670, name_pos)
    }

    fn misc(&mut self, allow_dtd: bool) -> PResult<()> {
        loop {
            self.skip_ws();
            if self.at("<!--") {
                self.comment(self.doc)?;
            } else if self.at("<?") {
                self.pi(self.doc)?;
            } else if allow_dtd && self.at("<!DOCTYPE") {
                self.doctype()?;
            } else {
                return Ok(());
            }
        }
    }

    fn document(&mut self) -> PResult<()> {
        if self.at("<?xml")
            && self
                .c
                .get(self.i + 5)
                .is_some_and(|&c| is_ws(c) || c == '?')
        {
            self.i += 5;
            self.xml_decl(0)?;
        }
        self.misc(true)?;
        match self.peek() {
            None => {
                return Err(Failure {
                    code: -1072896680,
                    reason: None,
                    pos: None,
                })
            }
            Some('<')
                if self
                    .c
                    .get(self.i + 1)
                    .is_some_and(|&c| c != '/' && c != '!' && c != '?') => {}
            Some('<') if self.c.get(self.i + 1) == Some(&'/') => {
                return fail(-1072896686, self.i + 2)
            }
            Some('<') if self.c.get(self.i + 1).is_none() => return fail(-1072896670, self.i),
            Some(_) => {
                return Err(Failure {
                    code: -1072896682,
                    reason: None,
                    pos: if self.i == 0 { Some(0) } else { None },
                })
            }
        }
        let mut scope: Vec<(String, String)> = Vec::new();
        self.element(self.doc, &mut scope, self.preserve)?;
        self.misc(false)?;
        match self.peek() {
            None => Ok(()),
            Some('<') if self.c.get(self.i + 1).is_some_and(|&c| is_name_start(c)) => {
                fail(-1072896683, self.i + 1)
            }
            Some('<') if self.c.get(self.i + 1) == Some(&'/') => fail(-1072896686, self.i + 2),
            Some(_) => fail(-1072896682, self.i),
        }
    }

    fn element(
        &mut self,
        parent: usize,
        scope: &mut Vec<(String, String)>,
        keep_ws: bool,
    ) -> PResult<()> {
        let mut stack: Vec<(usize, String, usize, bool)> = Vec::new();
        self.start_tag(parent, scope, keep_ws, &mut stack)?;
        while !stack.is_empty() {
            let (el, _, _, keep) = stack
                .last()
                .cloned()
                .unwrap_or((0, String::new(), 0, false));
            let mut text = String::new();
            let mut has_ref = false;
            loop {
                match self.peek() {
                    None => {
                        let names: Vec<&str> = stack.iter().map(|s| s.1.as_str()).collect();
                        return Err(Failure {
                            code: -1072896685,
                            reason: Some(format!(
                                "The following tags were not closed: {}.",
                                names.join(", ")
                            )),
                            pos: None,
                        });
                    }
                    Some('<') => {
                        if self.at("<![CDATA[") {
                            self.flush_text(el, &mut text, &mut has_ref, keep);
                            self.cdata(el)?;
                        } else if self.at("<!--") {
                            self.flush_text(el, &mut text, &mut has_ref, keep);
                            self.comment(el)?;
                        } else if self.at("<?") {
                            self.flush_text(el, &mut text, &mut has_ref, keep);
                            self.pi(el)?;
                        } else if self.at("</") {
                            self.flush_text(el, &mut text, &mut has_ref, keep);
                            self.end_tag(&mut stack, scope)?;
                            break;
                        } else if self.at("<!") {
                            return fail(-1072896670, self.i);
                        } else {
                            self.flush_text(el, &mut text, &mut has_ref, keep);
                            self.start_tag(el, scope, keep, &mut stack)?;
                            break;
                        }
                    }
                    Some('&') => {
                        has_ref = true;
                        self.char_ref_or_entity(&mut text)?;
                    }
                    Some(']') if self.at("]]>") => return fail(-1072896676, self.i),
                    Some('\r') => {
                        self.i += 1;
                        if self.peek() == Some('\n') {
                            self.i += 1;
                        }
                        text.push('\n');
                    }
                    Some(c) if !is_xml_char(c) => return fail(-1072896760, self.i),
                    Some(c) => {
                        text.push(c);
                        self.i += 1;
                    }
                }
            }
        }
        Ok(())
    }

    fn cdata(&mut self, parent: usize) -> PResult<()> {
        self.i += 9;
        let start = self.i;
        let mut text = String::new();
        loop {
            if self.at("]]>") {
                self.i += 3;
                break;
            }
            match self.peek() {
                None => return fail(-1072896668, start),
                Some('\r') => {
                    self.i += 1;
                    if self.peek() == Some('\n') {
                        self.i += 1;
                    }
                    text.push('\n');
                }
                Some(c) if !is_xml_char(c) => return fail(-1072896760, self.i),
                Some(c) => {
                    text.push(c);
                    self.i += 1;
                }
            }
        }
        let n = self.st.new_node(Kind::CData, "", self.doc);
        self.st.nodes[n].value = text;
        self.st.append(parent, n);
        Ok(())
    }

    fn start_tag(
        &mut self,
        parent: usize,
        scope: &mut Vec<(String, String)>,
        keep_ws: bool,
        stack: &mut Vec<(usize, String, usize, bool)>,
    ) -> PResult<()> {
        self.i += 1;
        let npos = self.i;
        let name = self.name()?;
        let el = self.st.new_node(Kind::Element, &name, self.doc);
        let mut attrs: Vec<(String, String, usize)> = Vec::new();
        let mut empty = false;
        loop {
            let had_ws = self.skip_ws();
            match self.peek() {
                None => {
                    return fail(
                        if attrs.is_empty() && self.i == npos + name.chars().count() {
                            -1072896674
                        } else {
                            -1072896763
                        },
                        if attrs.is_empty() && self.i == npos + name.chars().count() {
                            npos
                        } else {
                            self.i
                        },
                    )
                }
                Some('>') => {
                    self.i += 1;
                    break;
                }
                Some('/') => {
                    if self.c.get(self.i + 1) == Some(&'>') {
                        self.i += 2;
                        empty = true;
                        break;
                    }
                    return fail(-1072896763, self.i + 1);
                }
                Some(_) => {
                    if !had_ws {
                        return fail(-1072896759, self.i);
                    }
                    let apos = self.i;
                    let an = self.name()?;
                    self.skip_ws();
                    if self.peek() != Some('=') {
                        return fail(-1072896763, self.i);
                    }
                    self.i += 1;
                    self.skip_ws();
                    let v = self.attr_value()?;
                    attrs.push((an, v, apos));
                }
            }
        }
        let end = self.i;

        for (k, (an, _, _)) in attrs.iter().enumerate() {
            if attrs[..k].iter().any(|(b, _, _)| b == an) {
                return fail(-1072896684, end);
            }
        }

        let mark = scope.len();
        for (an, v, apos) in &attrs {
            if an == "xmlns" {
                scope.push((String::new(), v.clone()));
            } else if let Some(p) = an.strip_prefix("xmlns:") {
                if p == "xml" && v != XML_URI {
                    return if self.legacy {
                        self.reserved(*apos)
                    } else {
                        fail(-1072894385, end)
                    };
                }
                if p == "xmlns" {
                    return self.reserved(end);
                }
                if v.is_empty() {
                    return fail(-1072898009, end);
                }
                scope.push((p.to_string(), v.clone()));
            }
        }
        let lookup = |scope: &Vec<(String, String)>, p: &str| -> Option<String> {
            if p == "xml" {
                return Some(XML_URI.to_string());
            }
            scope
                .iter()
                .rev()
                .find(|(q, _)| q == p)
                .map(|(_, u)| u.clone())
        };
        let undeclared = |p: &str| Failure {
            code: -1072898019,
            reason: Some(format!("Reference to undeclared namespace prefix: '{p}'.")),
            pos: Some(end),
        };
        match name.split_once(':') {
            Some(("xml" | "xmlns", _)) => return self.reserved(end),
            Some((p, _)) => {
                let uri = lookup(scope, p).ok_or_else(|| undeclared(p))?;
                self.st.nodes[el].ns = Some(uri);
            }
            None => {
                let uri = lookup(scope, "").filter(|u| !u.is_empty());
                self.st.nodes[el].ns = uri;
            }
        }
        for (an, v, _) in &attrs {
            let a = self.st.new_attr(an, v, self.doc);
            if an == "xmlns" || an.starts_with("xmlns:") {
                self.st.nodes[a].ns = Some(XMLNS_URI.to_string());
            } else if let Some((p, _)) = an.split_once(':') {
                let uri = lookup(scope, p).ok_or_else(|| undeclared(p))?;
                self.st.nodes[a].ns = Some(uri);
            }
            self.st.nodes[a].owner = Some(el);
            self.st.nodes[el].attrs.push(a);
        }
        self.st.append(parent, el);
        if self.has_dtd && self.validate && parent == self.doc {
            return Err(Failure {
                code: -1072898035,
                reason: Some(format!(
                    "The element '{name}' is used but not declared in the DTD/Schema."
                )),
                pos: Some(end),
            });
        }
        let space = attrs
            .iter()
            .find(|(an, _, _)| an == "xml:space")
            .map(|(_, v, _)| v.as_str());
        let keep = match space {
            Some("preserve") => true,
            Some("default") => self.preserve,
            _ => keep_ws,
        };
        if empty {
            scope.truncate(mark);
        } else {
            stack.push((el, name, mark, keep));
        }
        Ok(())
    }

    fn reserved(&self, pos: usize) -> PResult<()> {
        if self.legacy {
            return Err(Failure {
                code: -1072896664,
                reason: Some(
                    "The namespace prefix is not allowed to start with the reserved string \"xml\"."
                        .into(),
                ),
                pos: Some(pos),
            });
        }
        fail(-1072896664, pos)
    }

    fn end_tag(
        &mut self,
        stack: &mut Vec<(usize, String, usize, bool)>,
        scope: &mut Vec<(String, String)>,
    ) -> PResult<()> {
        self.i += 2;
        let npos = self.i;
        if self.peek().is_some_and(is_ws) {
            return fail(-1072896749, npos);
        }
        let name = self.name()?;
        self.skip_ws();
        let (el, open, mark, _) = stack
            .last()
            .cloned()
            .unwrap_or((0, String::new(), 0, false));
        if name != open {
            return Err(Failure {
                code: -1072896659,
                reason: Some(format!(
                    "End tag '{name}' does not match the start tag '{open}'."
                )),
                pos: Some(npos),
            });
        }
        if self.peek() != Some('>') {
            return fail(-1072896763, self.i);
        }
        self.i += 1;
        stack.pop();
        self.st.nodes[el].explicit_end = self.st.nodes[el].children.is_empty();
        scope.truncate(mark);
        Ok(())
    }
}

pub(crate) fn parse_into(st: &mut Store, doc: usize, text: &str) -> Result<(), ParseErr> {
    let (preserve, prohibit, legacy, validate) = {
        let d = st.doc_data(doc);
        (d.preserve, d.prohibit_dtd, d.legacy, d.validate)
    };
    let chars: Vec<char> = text.chars().collect();
    let res = {
        let mut p = Parser {
            st,
            doc,
            c: chars.clone(),
            i: 0,
            preserve,
            prohibit_dtd: prohibit,
            legacy,
            validate,
            has_dtd: false,
        };
        p.document()
    };
    match res {
        Ok(()) => Ok(()),
        Err(f) => {
            st.clear_children(doc);
            let mut e = ParseErr {
                code: f.code,
                reason: f.reason.unwrap_or_else(|| reason(f.code).to_string()),
                ..ParseErr::default()
            };
            e.reason.push_str("\r\n");
            if let Some(pos) = f.pos {
                let pos = pos.min(chars.len());
                let line_start = chars[..pos]
                    .iter()
                    .rposition(|&c| c == '\n')
                    .map_or(0, |k| k + 1);
                let line_end = chars[pos..]
                    .iter()
                    .position(|&c| c == '\n')
                    .map_or(chars.len(), |k| pos + k);
                let end = if chars.get(pos) == Some(&'\u{FFFF}') {
                    pos
                } else {
                    line_end
                };
                let mut src: String = chars[line_start..end].iter().collect();
                if src.ends_with('\r') {
                    src.pop();
                }
                e.line = chars[..pos].iter().filter(|&&c| c == '\n').count() as i32 + 1;
                e.linepos = (pos - line_start + 1) as i32;
                e.filepos = pos as i32;
                e.src = src;
            }
            Err(e)
        }
    }
}
