use xlcalc_variant::{change_type, flags, HResult, Locale, VarType, Variant};
use xlcalc_vba::{InvokeKind, VbaError};

use crate::comobj::ComObj;
use crate::xmldom::{is_name_char, is_name_start, Kind, ParseErr};

type R = Result<Variant, VbaError>;

#[derive(Debug, Clone)]
pub(crate) enum ListSrc {
    Children(usize),
    Attrs(usize),
    Snap(Vec<usize>),
}

#[derive(Debug, Clone)]
pub(crate) struct XList {
    pub(crate) src: ListSrc,
    pub(crate) cur: usize,

    pub(crate) sel: Option<(String, usize)>,
}

const E_FAIL: i32 = 0x8000_4005_u32 as i32;
const E_INVALIDARG: i32 = 0x8007_0057_u32 as i32;

fn invalid() -> VbaError {
    VbaError {
        number: E_INVALIDARG,
        description: "The parameter is incorrect.".into(),
    }
}

fn msx(text: &str) -> VbaError {
    VbaError {
        number: E_FAIL,
        description: format!("{text}\r\n"),
    }
}

fn bad_count() -> VbaError {
    VbaError::new(450)
}

fn unknown() -> VbaError {
    VbaError::new(438)
}

fn missing(v: &Variant) -> bool {
    matches!(v, Variant::Error(e) if *e == HResult::DISP_E_PARAMNOTFOUND.0)
}

fn check_args(args: &[Variant], n: usize) -> Result<(), VbaError> {
    if args.len() != n || args.iter().any(missing) {
        Err(bad_count())
    } else {
        Ok(())
    }
}

fn text_arg(v: &Variant) -> Result<String, VbaError> {
    match v.clone().canon() {
        Variant::Bstr(s) => Ok(s),
        Variant::BstrNull | Variant::Empty => Ok(String::new()),
        Variant::Null | Variant::Dispatch(_) => Err(VbaError::new(13)),
        other => match change_type(&other, VarType::Bstr, Locale::EN_US, flags::ALPHABOOL) {
            Ok(Variant::Bstr(s)) => Ok(s),
            _ => Err(VbaError::new(13)),
        },
    }
}

fn bool_arg(v: &Variant) -> Result<bool, VbaError> {
    match v.clone().canon() {
        Variant::Empty => Ok(false),
        Variant::Null | Variant::Dispatch(_) => Err(VbaError::new(13)),
        other => match change_type(&other, VarType::Bool, Locale::EN_US, flags::ALPHABOOL) {
            Ok(Variant::Bool(b)) => Ok(b),
            _ => Err(VbaError::new(13)),
        },
    }
}

fn int_arg(v: &Variant) -> Result<i32, VbaError> {
    match change_type(
        &v.clone().canon(),
        VarType::I4,
        Locale::EN_US,
        flags::ALPHABOOL,
    ) {
        Ok(Variant::I4(n)) => Ok(n),
        _ => Err(VbaError::new(13)),
    }
}

fn bstr(s: String) -> Variant {
    Variant::Bstr(s)
}

fn marked_name(name: &[char], at: usize) -> String {
    let pre: String = name[..at].iter().collect();
    let post: String = name[at + 1..].iter().collect();
    format!("{pre}-->{}<--{post}", name[at])
}

fn name_error(kind: &str, name: &str) -> Option<VbaError> {
    let cs: Vec<char> = name.chars().collect();
    if cs.is_empty() {
        return Some(msx(&format!(
            "To create nodes of type '{kind}', a valid name must be specified."
        )));
    }
    let mut colons = 0;
    for (i, &c) in cs.iter().enumerate() {
        let bad_colon = c == ':'
            && (i == 0
                || colons >= 1
                || cs.get(i + 1).is_none_or(|&n| !is_name_start(n) || n == ':'));
        if c == ':' {
            colons += 1;
        }
        let msg = if bad_colon {
            "contain"
        } else if i == 0 && c != ':' && !is_name_start(c) {
            "begin with"
        } else if i > 0 && c != ':' && !is_name_char(c) {
            "contain"
        } else {
            continue;
        };
        return Some(msx(&format!(
            "This name may not {msg} the '{c}' character:\r\n\r\n{}",
            marked_name(&cs, i)
        )));
    }
    None
}

fn prefix_error(name: &str, legacy: bool, attr: bool) -> Option<VbaError> {
    let p = name.split_once(':').map_or(name, |(p, _)| p);
    let bare_ok = legacy || attr;
    let reserved =
        (p == "xmlns" && (name.contains(':') || !bare_ok)) || (name.contains(':') && p == "xml");
    if !reserved {
        return None;
    }
    Some(if legacy {
        msx("The namespace prefix is not allowed to start with the reserved string \"xml\".")
    } else {
        msx("Prefixes 'xml' and 'xmlns' are reserved for use by XML.")
    })
}

fn kind_label(k: i32) -> &'static str {
    match k {
        1 => "ELEMENT",
        2 => "ATTRIBUTE",
        5 => "ENTITYREF",
        7 => "PI",
        _ => "NODE",
    }
}

impl crate::Application<'_> {
    fn xh(&mut self, n: usize) -> Variant {
        if let Some(&h) = self.xml.handles.get(&n) {
            return Variant::Dispatch(Some(crate::comobj::com_id(h)));
        }
        let v = self.new_com(ComObj::Xml(n));
        if let Variant::Dispatch(Some(id)) = v {
            self.xml.handles.insert(n, crate::comobj::index(id));
        }
        v
    }

    fn xo(&mut self, n: Option<usize>) -> Variant {
        match n {
            Some(n) => self.xh(n),
            None => Variant::Empty,
        }
    }

    fn xlist(&mut self, src: ListSrc, sel: Option<(String, usize)>) -> Variant {
        self.new_com(ComObj::XmlList(XList { src, cur: 0, sel }))
    }

    fn node_arg(&self, v: &Variant) -> Result<usize, VbaError> {
        match v {
            Variant::Dispatch(Some(id)) => match self.com.get(crate::comobj::index(*id)) {
                Some(ComObj::Xml(n)) => Ok(*n),
                _ => Err(invalid()),
            },
            Variant::Dispatch(None) => Err(invalid()),
            Variant::Empty | Variant::Null | Variant::BstrNull => Err(VbaError::new(13)),
            _ => Err(VbaError::new(13)),
        }
    }

    pub(crate) fn create_xml_doc(&mut self, legacy: bool) -> ComObj {
        ComObj::Xml(self.xml.new_doc(legacy))
    }

    fn x_doc_of(&self, n: usize) -> usize {
        self.xml.nodes[n].doc
    }

    fn x_legacy(&self, n: usize) -> bool {
        self.xml.doc_data(self.x_doc_of(n)).legacy
    }

    fn x_new_named(&mut self, doc: usize, kind: Kind, name: &str) -> usize {
        let n = self.xml.new_node(kind, name, doc);
        if kind == Kind::Attribute && (name == "xmlns" || name.starts_with("xmlns:")) {
            self.xml.nodes[n].ns = Some(crate::xmldom::XMLNS_URI.into());
        }
        n
    }

    pub(crate) fn xml_call(
        &mut self,
        n: usize,
        member: &str,
        kind: InvokeKind,
        args: &[Variant],
    ) -> R {
        let put = matches!(kind, InvokeKind::Put | InvokeKind::PutRef);
        let name = member.to_ascii_lowercase();
        let k = self.xml.nodes[n].kind;
        let doc = k == Kind::Document;

        if let Some(r) = self.x_node_common(n, &name, put, args) {
            return r;
        }
        match k {
            Kind::Document => self.x_doc_call(n, &name, put, args),
            Kind::Element => self.x_element_call(n, &name, put, args),
            Kind::Attribute => self.x_attr_call(n, &name, put, args),
            Kind::Text | Kind::CData | Kind::Comment => self.x_chardata_call(n, &name, put, args),
            Kind::Pi => self.x_pi_call(n, &name, put, args),
            Kind::EntityRef | Kind::Fragment => {
                let _ = doc;
                Err(unknown())
            }
        }
    }

    fn x_put_arg<'a>(&self, args: &'a [Variant]) -> Result<&'a Variant, VbaError> {
        match args {
            [v] if !missing(v) => Ok(v),
            _ => Err(bad_count()),
        }
    }

    fn x_node_common(&mut self, n: usize, name: &str, put: bool, args: &[Variant]) -> Option<R> {
        let k = self.xml.nodes[n].kind;
        Some(match name {
            "nodename" | "nodetype" | "nodetypestring" | "parentnode" | "childnodes"
            | "firstchild" | "lastchild" | "previoussibling" | "nextsibling" | "ownerdocument"
            | "haschildnodes" | "namespaceuri" | "prefix" | "basename" | "xml" | "datatype"
            | "definition" | "parsed" | "specified" | "selectnodes" | "selectsinglenode"
            | "clonenode" | "appendchild" | "insertbefore" | "removechild" | "replacechild"
            | "nodevalue" | "text" | "nodetypedvalue" | "attributes" | "transformnode"
                if put && !matches!(name, "nodevalue" | "text" | "nodetypedvalue") =>
            {
                Err(bad_count())
            }
            "nodename" => check_args(args, 0).map(|()| {
                bstr(match k {
                    Kind::Document => "#document".into(),
                    _ => self.xml.nodes[n].name.clone(),
                })
            }),
            "nodetype" => check_args(args, 0).map(|()| Variant::I4(k.code())),
            "nodetypestring" => check_args(args, 0).map(|()| bstr(k.type_string().to_string())),
            "parentnode" => check_args(args, 0).map(|()| {
                let p = self.xml.nodes[n].parent;
                self.xo(p)
            }),
            "childnodes" => check_args(args, 0).map(|()| self.xlist(ListSrc::Children(n), None)),
            "firstchild" => check_args(args, 0).map(|()| {
                let c = self.xml.nodes[n].children.first().copied();
                self.xo(c)
            }),
            "lastchild" => check_args(args, 0).map(|()| {
                let c = self.xml.nodes[n].children.last().copied();
                self.xo(c)
            }),
            "previoussibling" | "nextsibling" => check_args(args, 0).map(|()| {
                let sib = self.xml.nodes[n].parent.and_then(|p| {
                    let i = self.xml.index_in_parent(n)?;
                    let kids = &self.xml.nodes[p].children;
                    if name == "nextsibling" {
                        kids.get(i + 1).copied()
                    } else {
                        i.checked_sub(1).map(|j| kids[j])
                    }
                });
                self.xo(sib)
            }),
            "ownerdocument" => check_args(args, 0).map(|()| {
                if k == Kind::Document {
                    Variant::Empty
                } else {
                    let d = self.x_doc_of(n);
                    self.xh(d)
                }
            }),
            "haschildnodes" => {
                check_args(args, 0).map(|()| Variant::Bool(!self.xml.nodes[n].children.is_empty()))
            }
            "attributes" => check_args(args, 0).map(|()| {
                if k == Kind::Element || (k == Kind::Pi && self.xml.nodes[n].name == "xml") {
                    self.xlist(ListSrc::Attrs(n), None)
                } else {
                    Variant::Empty
                }
            }),
            "namespaceuri" => check_args(args, 0).map(|()| match &self.xml.nodes[n].ns {
                Some(u) if u == crate::xmldom::XMLNS_URI && self.x_legacy(n) => bstr(String::new()),
                Some(u) if matches!(k, Kind::Element | Kind::Attribute) => bstr(u.clone()),
                _ => Variant::BstrNull,
            }),
            "prefix" => check_args(args, 0).map(|()| {
                let legacy_decl =
                    k == Kind::Attribute && self.x_legacy(n) && self.xml.nodes[n].name == "xmlns";
                match self.xml.prefix(n) {
                    _ if legacy_decl => bstr("xmlns".into()),
                    Some(p) => bstr(p.to_string()),
                    None => Variant::BstrNull,
                }
            }),
            "basename" => check_args(args, 0).map(|()| match k {
                Kind::Attribute if self.x_legacy(n) && self.xml.nodes[n].name == "xmlns" => {
                    bstr(String::new())
                }
                Kind::Element | Kind::Attribute | Kind::Pi | Kind::EntityRef => {
                    bstr(self.xml.base_name(n))
                }
                _ => Variant::BstrNull,
            }),
            "xml" => check_args(args, 0).map(|()| bstr(self.xml.xml(n))),
            "datatype" => check_args(args, 0).map(|()| Variant::Null),
            "definition" => check_args(args, 0).map(|()| Variant::Empty),
            "parsed" => check_args(args, 0).map(|()| Variant::Bool(true)),
            "specified" => {
                if k == Kind::Document {
                    return Some(Err(unknown()));
                }
                check_args(args, 0).map(|()| Variant::Bool(true))
            }
            "nodetypedvalue" if !put => check_args(args, 0).map(|()| match k {
                Kind::Document | Kind::Fragment => Variant::Null,
                Kind::Text => bstr(self.xml.nodes[n].value.clone()),
                _ => bstr(self.xml.text(n)),
            }),
            "text" if !put => check_args(args, 0).map(|()| bstr(self.xml.text(n))),
            "text" => self.x_put_text(n, args),
            "nodevalue" if !put => check_args(args, 0).map(|()| match k {
                Kind::Text | Kind::CData | Kind::Comment | Kind::Pi => {
                    bstr(self.xml.nodes[n].value.clone())
                }
                Kind::Attribute => bstr(self.xml.attr_value(n)),
                _ => Variant::Null,
            }),
            "nodevalue" | "nodetypedvalue" => self.x_put_value(n, args),
            "clonenode" => (|| {
                check_args(args, 1)?;
                let deep = bool_arg(&args[0])?;
                let c = self.xml.clone_node(n, deep);
                Ok(self.xh(c))
            })(),
            "appendchild" => self.x_insert(n, args, None),
            "insertbefore" => self.x_insert_before(n, args),
            "removechild" => self.x_remove(n, args),
            "replacechild" => self.x_replace(n, args),
            "selectnodes" | "selectsinglenode" => {
                self.x_select(n, name == "selectsinglenode", args)
            }
            "transformnode" => Err(unknown()),
            _ => return None,
        })
    }

    fn x_put_text(&mut self, n: usize, args: &[Variant]) -> R {
        let v = text_arg(self.x_put_arg(args)?)?;
        match self.xml.nodes[n].kind {
            Kind::Document | Kind::Fragment => Err(self.wrong_kind(n)),
            Kind::Element => {
                self.xml.clear_children(n);
                let d = self.x_doc_of(n);
                let t = self.xml.new_node(Kind::Text, "", d);
                self.xml.nodes[t].value = v;
                self.xml.append(n, t);
                Ok(Variant::Empty)
            }
            Kind::Attribute => {
                self.xml.set_attr_value(n, &v);
                Ok(Variant::Empty)
            }
            _ => {
                self.x_set_data(n, v)?;
                Ok(Variant::Empty)
            }
        }
    }

    fn x_put_value(&mut self, n: usize, args: &[Variant]) -> R {
        let v = text_arg(self.x_put_arg(args)?)?;
        match self.xml.nodes[n].kind {
            Kind::Attribute => {
                self.xml.set_attr_value(n, &v);
                Ok(Variant::Empty)
            }
            Kind::Text | Kind::CData | Kind::Comment | Kind::Pi => {
                self.x_set_data(n, v)?;
                Ok(Variant::Empty)
            }
            _ => Err(self.wrong_kind(n)),
        }
    }

    fn wrong_kind(&self, n: usize) -> VbaError {
        if self.x_legacy(n) {
            msx(&format!(
                "This operation can not be performed with a Node of type {}.",
                kind_upper(self.xml.nodes[n].kind)
            ))
        } else {
            invalid()
        }
    }

    fn x_set_data(&mut self, n: usize, v: String) -> Result<(), VbaError> {
        if self.xml.nodes[n].kind == Kind::Pi && self.xml.nodes[n].name == "xml" {
            return Err(invalid());
        }
        self.x_check_data(self.xml.nodes[n].kind, &v)?;
        let v = v.replace("\r\n", "\n").replace('\r', "\n");
        self.xml.nodes[n].value = v;
        Ok(())
    }

    fn x_check_data(&self, kind: Kind, v: &str) -> Result<(), VbaError> {
        let bad = match kind {
            Kind::Comment => v.contains("--") || v.ends_with('-'),
            Kind::CData => v.contains("]]>"),
            Kind::Pi => v.contains("?>"),
            _ => false,
        };
        if bad {
            Err(invalid())
        } else {
            Ok(())
        }
    }

    fn x_check_insert(&self, parent: usize, child: usize) -> Result<(), VbaError> {
        let pk = self.xml.nodes[parent].kind;
        let ck = self.xml.nodes[child].kind;
        if self.x_legacy(parent) != self.x_legacy(child) {
            return Err(msx(
                "It is an error to mix objects from different versions of MSXML.",
            ));
        }
        if self.xml.is_ancestor(child, parent) {
            return Err(msx(
                "Inserting a node or its ancestor under itself is not allowed.",
            ));
        }
        let ok = match pk {
            Kind::Document => matches!(
                ck,
                Kind::Element | Kind::Comment | Kind::Pi | Kind::Fragment
            ),
            Kind::Element | Kind::Fragment => !matches!(ck, Kind::Attribute | Kind::Document),
            Kind::Attribute => matches!(ck, Kind::Text | Kind::EntityRef),
            _ => false,
        };
        if !ok {
            return Err(invalid());
        }
        Ok(())
    }

    fn x_only_one_root(
        &self,
        doc: usize,
        incoming: &[usize],
        replacing: Option<usize>,
    ) -> Result<(), VbaError> {
        let has_el = self.xml.nodes[doc]
            .children
            .iter()
            .any(|&c| self.xml.nodes[c].kind == Kind::Element && Some(c) != replacing);
        let new_el = incoming
            .iter()
            .any(|&c| self.xml.nodes[c].kind == Kind::Element);
        if has_el && new_el {
            return Err(msx(
                "Only one top level element is allowed in an XML document.",
            ));
        }
        Ok(())
    }

    fn x_incoming(&self, c: usize) -> Vec<usize> {
        if self.xml.nodes[c].kind == Kind::Fragment {
            self.xml.nodes[c].children.clone()
        } else {
            vec![c]
        }
    }

    fn x_insert(&mut self, n: usize, args: &[Variant], at: Option<usize>) -> R {
        check_args(args, 1)?;
        let c = self.node_arg(&args[0])?;
        self.x_insert_nodes(n, c, at)
    }

    fn x_insert_nodes(&mut self, n: usize, c: usize, at: Option<usize>) -> R {
        self.x_check_insert(n, c)?;
        let items = self.x_incoming(c);
        if self.xml.nodes[n].kind == Kind::Document {
            self.x_only_one_root(n, &items, None)?;
        }
        if self.xml.nodes[c].kind == Kind::Fragment {
            let mut pos = at;
            for it in items {
                self.xml.detach(it);
                match pos {
                    Some(p) => {
                        self.xml.insert_at(n, p, it);
                        pos = Some(p + 1);
                    }
                    None => self.xml.append(n, it),
                }
            }
        } else {
            match at {
                Some(p) => {
                    let old = self.xml.nodes[c].parent == Some(n);
                    let idx = self.xml.index_in_parent(c);
                    self.xml.detach(c);
                    let p = if old && idx.is_some_and(|i| i < p) {
                        p - 1
                    } else {
                        p
                    };
                    self.xml.insert_at(n, p, c);
                }
                None => self.xml.append(n, c),
            }
        }
        Ok(self.xh(c))
    }

    fn x_insert_before(&mut self, n: usize, args: &[Variant]) -> R {
        check_args(args, 2)?;
        let c = self.node_arg(&args[0])?;
        match &args[1] {
            Variant::Null | Variant::Empty => self.x_insert_nodes(n, c, None),
            Variant::Dispatch(_) => {
                let r = self.node_arg(&args[1])?;
                if self.xml.nodes[r].parent != Some(n) {
                    return Err(invalid());
                }
                if r == c {
                    return Ok(self.xh(c));
                }
                let at = self.xml.index_in_parent(r);
                self.x_insert_nodes(n, c, at)
            }
            _ => Err(invalid()),
        }
    }

    fn x_remove(&mut self, n: usize, args: &[Variant]) -> R {
        check_args(args, 1)?;
        let c = self.node_arg(&args[0])?;
        if self.xml.nodes[c].parent != Some(n) {
            return Err(invalid());
        }
        self.xml.detach(c);
        Ok(self.xh(c))
    }

    fn x_replace(&mut self, n: usize, args: &[Variant]) -> R {
        check_args(args, 2)?;
        let c = self.node_arg(&args[0])?;
        let old = self.node_arg(&args[1])?;
        if self.xml.nodes[old].parent != Some(n) {
            return Err(invalid());
        }
        if c == old {
            return Ok(self.xh(old));
        }
        self.x_check_insert(n, c)?;
        let items = self.x_incoming(c);
        if self.xml.nodes[n].kind == Kind::Document {
            self.x_only_one_root(n, &items, Some(old))?;
        }
        let at = self.xml.index_in_parent(old).unwrap_or(0);
        self.xml.detach(old);
        for (pos, it) in (at..).zip(items) {
            self.xml.detach(it);
            self.xml.insert_at(n, pos, it);
        }
        Ok(self.xh(old))
    }

    fn x_sel_ns(&self, n: usize) -> Vec<(String, String)> {
        let d = self.xml.doc_data(self.x_doc_of(n));
        let mut out = Vec::new();
        if let Some(s) = &d.sel_ns {
            for (k, v) in parse_xmlns_list(s).unwrap_or_default() {
                out.push((k, v));
            }
        }
        out
    }

    fn x_select(&mut self, n: usize, single: bool, args: &[Variant]) -> R {
        check_args(args, 1)?;
        let expr = text_arg(&args[0])?;
        if matches!(args[0], Variant::Null) {
            return Err(VbaError::new(13));
        }
        let ns = self.x_sel_ns(n);
        let found = crate::xpath::select(&self.xml, n, &expr, &ns).map_err(|m| VbaError {
            number: E_FAIL,
            description: m,
        })?;
        if single {
            let first = found.first().copied();
            return Ok(self.xo(first));
        }
        Ok(self.xlist(ListSrc::Snap(found), Some((expr, n))))
    }

    fn x_element_call(&mut self, n: usize, name: &str, put: bool, args: &[Variant]) -> R {
        if put {
            return Err(match name {
                "tagname" => bad_count(),
                _ => unknown(),
            });
        }
        match name {
            "tagname" => check_args(args, 0).map(|()| bstr(self.xml.nodes[n].name.clone())),
            "getattribute" => {
                check_args(args, 1)?;
                let a = text_arg(&args[0])?;
                Ok(match self.xml.get_attr(n, &a) {
                    Some(v) => bstr(v),
                    None => Variant::Null,
                })
            }
            "setattribute" => {
                check_args(args, 2)?;
                let a = text_arg(&args[0])?;
                let v = attr_text(&args[1])?;
                if a.is_empty() {
                    return Err(msx("An empty string '' is not a valid name."));
                }
                if let Some(e) = name_error("ATTRIBUTE", &a) {
                    return Err(e);
                }
                self.xml.put_attr(n, &a, &v);
                Ok(Variant::Empty)
            }
            "removeattribute" => {
                check_args(args, 1)?;
                let a = text_arg(&args[0])?;
                if let Some(at) = self.xml.find_attr(n, &a) {
                    self.xml.detach(at);
                }
                Ok(Variant::Empty)
            }
            "getattributenode" => {
                check_args(args, 1)?;
                let a = text_arg(&args[0])?;
                let at = self.xml.find_attr(n, &a);
                Ok(self.xo(at))
            }
            "setattributenode" => {
                check_args(args, 1)?;
                let a = self.node_arg(&args[0])?;
                self.x_set_named(n, a)
            }
            "removeattributenode" => {
                check_args(args, 1)?;
                let a = self.node_arg(&args[0])?;
                if self.xml.nodes[a].owner != Some(n) {
                    return Err(invalid());
                }
                self.xml.detach(a);
                Ok(self.xh(a))
            }
            "getelementsbytagname" => {
                check_args(args, 1)?;
                if matches!(args[0], Variant::Null) {
                    return Err(VbaError::new(13));
                }
                let t = text_arg(&args[0])?;
                if t.is_empty() {
                    return Err(msx("An empty string '' is not a valid name."));
                }
                let hits = self.by_tag(n, &t)?;
                Ok(self.xlist(ListSrc::Snap(hits), None))
            }
            "normalize" => {
                check_args(args, 0)?;
                self.x_normalize(n);
                Ok(Variant::I4(0))
            }
            _ => Err(unknown()),
        }
    }

    fn by_tag(&self, root: usize, t: &str) -> Result<Vec<usize>, VbaError> {
        if t != "*" {
            if let Some(e) = name_error("ELEMENT", t) {
                return Err(e);
            }
        }
        let mut all = Vec::new();
        self.x_descendants(root, &mut all);
        Ok(all
            .into_iter()
            .filter(|&e| {
                let node = &self.xml.nodes[e];
                node.kind == Kind::Element
                    && (t == "*"
                        || if t.contains(':') {
                            node.name == t
                        } else {
                            node.name == t && node.ns.as_deref().is_none_or(str::is_empty)
                        })
            })
            .collect())
    }

    fn x_descendants(&self, n: usize, out: &mut Vec<usize>) {
        for &c in &self.xml.nodes[n].children {
            out.push(c);
            self.x_descendants(c, out);
        }
    }

    fn x_normalize(&mut self, n: usize) {
        let kids = self.xml.nodes[n].children.clone();
        let mut prev: Option<usize> = None;
        for c in kids {
            if self.xml.nodes[c].kind == Kind::Text {
                if let Some(p) = prev {
                    let add = self.xml.nodes[c].value.clone();
                    self.xml.nodes[p].value.push_str(&add);
                    let after = self.xml.nodes[c].ws_after;
                    self.xml.nodes[p].ws_after |= after;
                    self.xml.detach(c);
                    continue;
                }
                prev = Some(c);
            } else {
                prev = None;
                self.x_normalize(c);
            }
        }
    }

    fn x_set_named(&mut self, el: usize, a: usize) -> R {
        if self.xml.nodes[a].kind != Kind::Attribute {
            return Err(invalid());
        }
        if self.xml.nodes[a].owner.is_some() {
            return Err(msx(
                "Attributes must be removed before adding them to a different node.",
            ));
        }
        let nm = self.xml.nodes[a].name.clone();
        let old = self.xml.find_attr(el, &nm);
        let doc = self.x_doc_of(el);
        if self.x_doc_of(a) != doc {
            self.xml.set_doc(a, doc);
        }
        match old {
            Some(o) => {
                let i = self.xml.nodes[el]
                    .attrs
                    .iter()
                    .position(|&x| x == o)
                    .unwrap_or(0);
                self.xml.nodes[el].attrs[i] = a;
                self.xml.nodes[o].owner = None;
            }
            None => self.xml.nodes[el].attrs.push(a),
        }
        self.xml.nodes[a].owner = Some(el);
        Ok(self.xo(old))
    }

    fn x_attr_call(&mut self, n: usize, name: &str, put: bool, args: &[Variant]) -> R {
        match name {
            "name" if !put => check_args(args, 0).map(|()| bstr(self.xml.nodes[n].name.clone())),
            "value" if !put => check_args(args, 0).map(|()| bstr(self.xml.attr_value(n))),
            "value" => {
                let v = text_arg(self.x_put_arg(args)?)?;
                self.xml.set_attr_value(n, &v);
                Ok(Variant::Empty)
            }
            "name" => Err(bad_count()),
            "length" | "data" => Err(unknown()),
            _ => Err(unknown()),
        }
    }

    fn x_char_len(&self, n: usize) -> usize {
        self.xml.nodes[n].value.encode_utf16().count()
    }

    fn x_chardata_call(&mut self, n: usize, name: &str, put: bool, args: &[Variant]) -> R {
        let kind = self.xml.nodes[n].kind;
        match name {
            "data" if !put => check_args(args, 0).map(|()| bstr(self.xml.nodes[n].value.clone())),
            "data" => {
                let v = text_arg(self.x_put_arg(args)?)?;
                self.x_set_data(n, v)?;
                Ok(Variant::Empty)
            }
            "length" if !put => {
                check_args(args, 0).map(|()| Variant::I4(self.x_char_len(n) as i32))
            }
            "substringdata" => {
                check_args(args, 2)?;
                let (o, c) = (int_arg(&args[0])?, int_arg(&args[1])?);
                let len = self.x_char_len(n) as i32;
                if o < 0 || c < 0 || o > len {
                    return Err(invalid());
                }
                if c == 0 {
                    return Ok(Variant::BstrNull);
                }
                Ok(bstr(slice16(
                    &self.xml.nodes[n].value,
                    o as usize,
                    c as usize,
                )))
            }
            "appenddata" => {
                check_args(args, 1)?;
                let t = text_arg(&args[0])?;
                let nv = format!("{}{}", self.xml.nodes[n].value, t);
                self.x_set_data(n, nv)?;
                Ok(Variant::Empty)
            }
            "insertdata" => {
                check_args(args, 2)?;
                let (o, t) = (int_arg(&args[0])?, text_arg(&args[1])?);
                let len = self.x_char_len(n) as i32;
                if o < 0 || o > len {
                    return Err(invalid());
                }
                let v = &self.xml.nodes[n].value;
                let nv = splice16(v, o as usize, 0, &t);
                self.x_set_data(n, nv)?;
                Ok(Variant::Empty)
            }
            "deletedata" => {
                check_args(args, 2)?;
                let (o, c) = (int_arg(&args[0])?, int_arg(&args[1])?);
                let len = self.x_char_len(n) as i32;
                if o < 0 || c < 0 || o > len {
                    return Err(invalid());
                }
                let nv = splice16(&self.xml.nodes[n].value, o as usize, c as usize, "");
                self.x_set_data(n, nv)?;
                Ok(Variant::Empty)
            }
            "replacedata" => {
                check_args(args, 3)?;
                let (o, c, t) = (int_arg(&args[0])?, int_arg(&args[1])?, text_arg(&args[2])?);
                let len = self.x_char_len(n) as i32;
                if o < 0 || c < 0 || o > len {
                    return Err(invalid());
                }
                let nv = splice16(&self.xml.nodes[n].value, o as usize, c as usize, &t);
                self.x_set_data(n, nv)?;
                Ok(Variant::Empty)
            }
            "splittext" if kind == Kind::Text => {
                check_args(args, 1)?;
                let o = int_arg(&args[0])?;
                if o < 0 || o as usize > self.x_char_len(n) {
                    return Err(invalid());
                }
                if o as usize == self.x_char_len(n) {
                    return Ok(Variant::Empty);
                }
                let v = self.xml.nodes[n].value.clone();
                let head = slice16(&v, 0, o as usize);
                let tail = slice16(&v, o as usize, usize::MAX);
                self.xml.nodes[n].value = head;
                let d = self.x_doc_of(n);
                let t = self.xml.new_node(Kind::Text, "", d);
                self.xml.nodes[t].value = tail;
                if let Some(p) = self.xml.nodes[n].parent {
                    let at = self.xml.index_in_parent(n).unwrap_or(0) + 1;
                    self.xml.insert_at(p, at, t);
                }
                Ok(self.xh(t))
            }
            _ => Err(unknown()),
        }
    }

    fn x_pi_call(&mut self, n: usize, name: &str, put: bool, args: &[Variant]) -> R {
        match name {
            "target" if !put => check_args(args, 0).map(|()| bstr(self.xml.nodes[n].name.clone())),
            "data" if !put => check_args(args, 0).map(|()| bstr(self.xml.nodes[n].value.clone())),
            "data" => {
                let v = text_arg(self.x_put_arg(args)?)?;
                self.x_set_data(n, v)?;
                Ok(Variant::Empty)
            }
            "target" => Err(bad_count()),
            _ => Err(unknown()),
        }
    }

    fn x_doc_call(&mut self, d: usize, name: &str, put: bool, args: &[Variant]) -> R {
        match name {
            "async" | "validateonparse" | "resolveexternals" | "preservewhitespace" => {
                if put {
                    let b = bool_arg(self.x_put_arg(args)?)?;
                    let dd = self.xml.doc_data_mut(d);
                    match name {
                        "async" => dd.async_ = b,
                        "validateonparse" => dd.validate = b,
                        "resolveexternals" => dd.resolve_ext = b,
                        _ => dd.preserve = b,
                    }
                    return Ok(Variant::Empty);
                }
                check_args(args, 0)?;
                let dd = self.xml.doc_data(d);
                Ok(Variant::Bool(match name {
                    "async" => dd.async_,
                    "validateonparse" => dd.validate,
                    "resolveexternals" => dd.resolve_ext,
                    _ => dd.preserve,
                }))
            }
            "documentelement" if !put => check_args(args, 0).map(|()| {
                let e = self.xml.nodes[d]
                    .children
                    .iter()
                    .copied()
                    .find(|&c| self.xml.nodes[c].kind == Kind::Element);
                self.xo(e)
            }),
            "documentelement" => {
                let v = self.x_put_arg(args)?.clone();
                let e = self.node_arg(&v)?;
                if self.xml.nodes[e].kind != Kind::Element {
                    return Err(VbaError::new(13));
                }
                let old = self.xml.nodes[d]
                    .children
                    .iter()
                    .copied()
                    .find(|&c| self.xml.nodes[c].kind == Kind::Element);
                match old {
                    Some(o) if o != e => {
                        let at = self.xml.index_in_parent(o).unwrap_or(0);
                        self.xml.detach(o);
                        self.xml.detach(e);
                        self.xml.insert_at(d, at, e);
                    }
                    Some(_) => {}
                    None => self.xml.append(d, e),
                }
                Ok(Variant::Empty)
            }
            "url" if !put => check_args(args, 0).map(|()| match &self.xml.doc_data(d).url {
                Some(u) => bstr(u.clone()),
                None => Variant::BstrNull,
            }),
            "readystate" if !put => {
                check_args(args, 0).map(|()| Variant::I4(self.xml.doc_data(d).ready))
            }
            "parseerror" if !put => check_args(args, 0).map(|()| {
                let e = self.xml.doc_data(d).err.clone();
                self.new_com(ComObj::XmlErr(Box::new(e)))
            }),
            "doctype" if !put => check_args(args, 0).map(|()| Variant::Empty),
            "implementation" if !put => check_args(args, 0).map(|()| self.new_com(ComObj::XmlImpl)),
            "namespaces" if !put => check_args(args, 0).map(|()| self.new_com(ComObj::XmlSchemas)),
            "schemas" if !put => check_args(args, 0).map(|()| Variant::Null),
            "validate" if !put => check_args(args, 0).map(|()| {
                let e = self.xml.doc_data(d).err.clone();
                self.new_com(ComObj::XmlErr(Box::new(e)))
            }),
            "abort" if !put => check_args(args, 0).map(|()| Variant::I4(0)),
            "loadxml" if !put => {
                check_args(args, 1)?;
                let s = text_arg(&args[0])?;
                if matches!(args[0], Variant::Null) {
                    return Err(VbaError::new(13));
                }
                Ok(Variant::Bool(self.load_text(d, &s, None)))
            }
            "load" if !put => {
                check_args(args, 1)?;
                self.x_load_file(d, &args[0])
            }
            "save" if !put => {
                check_args(args, 1)?;
                self.x_save_doc(d, &args[0])
            }
            "setproperty" if !put => {
                check_args(args, 2)?;
                self.x_set_property(d, &args[0], &args[1])
            }
            "getproperty" if !put => {
                check_args(args, 1)?;
                self.x_get_property(d, &args[0])
            }
            "createelement"
            | "createattribute"
            | "createtextnode"
            | "createcomment"
            | "createcdatasection"
            | "createprocessinginstruction"
            | "createentityreference"
            | "createdocumentfragment"
            | "createnode"
                if !put =>
            {
                self.x_create(d, name, args)
            }
            "getelementsbytagname" if !put => {
                check_args(args, 1)?;
                if matches!(args[0], Variant::Null) {
                    return Err(VbaError::new(13));
                }
                let t = text_arg(&args[0])?;
                if t.is_empty() {
                    return Err(msx("An empty string '' is not a valid name."));
                }
                let hits = self.by_tag(d, &t)?;
                Ok(self.xlist(ListSrc::Snap(hits), None))
            }
            "nodefromid" if !put => {
                check_args(args, 1)?;
                text_arg(&args[0])?;
                Ok(Variant::Empty)
            }
            _ if put
                && matches!(
                    name,
                    "readystate" | "url" | "parseerror" | "doctype" | "implementation"
                ) =>
            {
                Err(bad_count())
            }
            _ => Err(unknown()),
        }
    }

    fn x_create(&mut self, d: usize, name: &str, args: &[Variant]) -> R {
        let legacy = self.x_legacy(d);
        let (kind, rest): (i32, &[Variant]) = match name {
            "createelement" => (1, args),
            "createattribute" => (2, args),
            "createtextnode" => (3, args),
            "createcdatasection" => (4, args),
            "createentityreference" => (5, args),
            "createprocessinginstruction" => (7, args),
            "createcomment" => (8, args),
            "createdocumentfragment" => {
                check_args(args, 0)?;
                let f = self.xml.new_node(Kind::Fragment, "", d);
                return Ok(self.xh(f));
            }
            _ => {
                if args.len() != 3 || args.iter().any(missing) {
                    return Err(bad_count());
                }
                let k = match args[0].clone().canon() {
                    Variant::Bstr(s) => match s.to_ascii_lowercase().as_str() {
                        "element" => 1,
                        "attribute" => 2,
                        "text" => 3,
                        "cdatasection" => 4,
                        "entityreference" => 5,
                        "processinginstruction" => 7,
                        "comment" => 8,
                        "document" => 9,
                        "documentfragment" => 11,
                        _ => return Err(invalid()),
                    },
                    _ => int_arg(&args[0])?,
                };
                let ns = match &args[2] {
                    Variant::Bstr(s) => s.clone(),
                    Variant::Empty | Variant::BstrNull => String::new(),
                    _ => return Err(VbaError::new(13)),
                };
                let nm = text_arg(&args[1])?;
                return self.x_create_node(d, k, &nm, &ns, legacy);
            }
        };
        if rest.len() != if kind == 7 { 2 } else { 1 } || rest.iter().any(missing) {
            return Err(bad_count());
        }
        let a0 = text_arg(&rest[0])?;
        if matches!(rest[0], Variant::Null) {
            return Err(VbaError::new(13));
        }
        match kind {
            1 | 2 | 5 | 7 => {
                if kind == 7 && a0.eq_ignore_ascii_case("xml") && a0 != "xml" {
                    return Err(msx("A node of type 'PI' cannot have the name 'XML'."));
                }
                if let Some(e) = name_error(kind_label(kind), &a0) {
                    return Err(e);
                }
                if kind == 1 || kind == 2 {
                    if let Some(e) = prefix_error(&a0, legacy, kind == 2) {
                        return Err(e);
                    }
                }
                let n = self.x_new_named(d, kind_of(kind), &a0);
                if kind == 7 {
                    let data = text_arg(&rest[1])?;
                    if data.contains("?>") {
                        return Err(invalid());
                    }
                    if a0 == "xml" {
                        self.xml_decl_attrs(n, &data)?;
                    }
                    self.xml.nodes[n].value = data;
                }
                Ok(self.xh(n))
            }
            _ => {
                self.x_check_data(kind_of(kind), &a0)?;
                let n = self.xml.new_node(kind_of(kind), "", d);
                self.xml.nodes[n].value = a0.replace("\r\n", "\n").replace('\r', "\n");
                Ok(self.xh(n))
            }
        }
    }

    fn xml_decl_attrs(&mut self, n: usize, data: &str) -> Result<(), VbaError> {
        let bad = VbaError {
            number: 0xC00C_E56C_u32 as i32,
            description: "The specified attribute was not expected at this location.\r\nThe attribute may be case sensitive.\r\n".into(),
        };
        let pairs = parse_xmlns_attrs(data).map_err(|_| bad.clone())?;
        if pairs.first().is_none_or(|(k, _)| k != "version") {
            return Err(bad);
        }
        for (k, v) in pairs {
            self.xml.put_attr(n, &k, &v);
        }
        Ok(())
    }

    fn x_create_node(&mut self, d: usize, kind: i32, name: &str, ns: &str, legacy: bool) -> R {
        match kind {
            1 | 2 => {
                if let Some(e) = name_error(kind_label(kind), name)
                    .or_else(|| prefix_error(name, legacy, kind == 2))
                {
                    return Err(e);
                }
                if kind == 1 && name.contains(':') && ns.is_empty() {
                    let p = name.split_once(':').map_or("", |(p, _)| p);
                    return Err(msx(&format!(
                        "Reference to undeclared namespace prefix: '{p}'."
                    )));
                }
                let n = self.x_new_named(d, kind_of(kind), name);
                if !ns.is_empty() {
                    self.xml.nodes[n].ns = Some(ns.to_string());
                    self.xml.nodes[n].synth_ns = kind == 1;
                }
                Ok(self.xh(n))
            }
            3 | 4 | 8 => {
                let n = self.xml.new_node(kind_of(kind), "", d);
                Ok(self.xh(n))
            }
            5 | 7 => {
                if let Some(e) = name_error(kind_label(kind), name) {
                    return Err(e);
                }
                if kind == 7 && name.eq_ignore_ascii_case("xml") {
                    return Err(msx("A node of type 'PI' cannot have the name 'XML'."));
                }
                let n = self.xml.new_node(kind_of(kind), name, d);
                Ok(self.xh(n))
            }
            11 => {
                let n = self.xml.new_node(Kind::Fragment, "", d);
                Ok(self.xh(n))
            }
            _ => Err(invalid()),
        }
    }

    pub(crate) fn load_text(&mut self, d: usize, text: &str, url: Option<String>) -> bool {
        self.xml.clear_children(d);
        let res = crate::xmldom::parse_into(&mut self.xml, d, text);
        let dd = self.xml.doc_data_mut(d);
        dd.url = url.clone();
        match res {
            Ok(()) => {
                dd.err = ParseErr::default();
                true
            }
            Err(mut e) => {
                e.url = url;
                dd.err = e;
                false
            }
        }
    }

    fn x_fail_load(&mut self, d: usize, code: i32, reason: &str, url: Option<String>) -> R {
        self.xml.clear_children(d);
        let dd = self.xml.doc_data_mut(d);
        dd.url = None;
        dd.err = ParseErr {
            code,
            reason: format!("{reason}\r\n"),
            url,
            ..ParseErr::default()
        };
        Ok(Variant::Bool(false))
    }

    fn x_load_file(&mut self, d: usize, src: &Variant) -> R {
        let path = match src.clone().canon() {
            Variant::Null | Variant::Dispatch(_) => return Err(invalid()),
            Variant::Empty | Variant::BstrNull => String::new(),
            other => text_arg(&other)?,
        };
        if path.is_empty() {
            return self.x_fail_load(d, -2_147_024_809, "The parameter is incorrect.", None);
        }
        let local = path
            .strip_prefix("file:///")
            .unwrap_or(&path)
            .replace('/', "\\");
        let url = format!("file:///{}", local.replace('\\', "/"));
        let Ok(bytes) = std::fs::read(&local) else {
            return self.x_fail_load(
                d,
                -2_146_697_210,
                "The system cannot locate the object specified.",
                Some(path),
            );
        };
        match decode_bytes(&bytes) {
            Ok((text, enc)) => {
                let ok = self.load_text(d, &text, Some(url));
                self.xml.doc_data_mut(d).encoding = enc;
                Ok(Variant::Bool(ok))
            }
            Err((code, reason, pos)) => {
                let r = self.x_fail_load(d, code, reason, Some(url));
                if let Some(pos) = pos {
                    let before = &bytes[..pos.min(bytes.len())];
                    let start = before.iter().rposition(|&c| c == 10).map_or(0, |k| k + 1);
                    let e = &mut self.xml.doc_data_mut(d).err;
                    e.line = before.iter().filter(|&&c| c == 10).count() as i32 + 1;
                    e.linepos = (pos - start + 1) as i32;
                    e.filepos = pos as i32;
                    e.src = String::from_utf8_lossy(&before[start..]).into_owned();
                }
                r
            }
        }
    }

    fn x_save_doc(&mut self, d: usize, target: &Variant) -> R {
        let to_file = matches!(target.clone().canon(), Variant::Bstr(_));
        self.xml.keep_enc = to_file;
        let xml = self.xml.xml(d);
        self.xml.keep_enc = false;
        if matches!(target.clone().canon(), Variant::Bstr(s) if s.is_empty()) {
            return Err(invalid());
        }
        match target.clone().canon() {
            Variant::Bstr(path) => {
                let enc = self.x_declared_encoding(d);
                let bytes = encode_text(&xml, enc.as_deref());
                let local = path
                    .strip_prefix("file:///")
                    .unwrap_or(&path)
                    .replace('/', "\\");
                std::fs::write(&local, bytes).map_err(|_| VbaError {
                    number: 0x8007_0003_u32 as i32,
                    description: "The system cannot find the path specified.".into(),
                })?;
                Ok(Variant::Empty)
            }
            Variant::Dispatch(_) => {
                let other = self.node_arg(target)?;
                if self.xml.nodes[other].kind != Kind::Document {
                    return Err(invalid());
                }
                self.load_text(other, &xml, None);
                Ok(Variant::Empty)
            }
            Variant::Null | Variant::Empty | Variant::BstrNull => Err(invalid()),
            _ => Err(VbaError::new(13)),
        }
    }

    fn x_declared_encoding(&self, d: usize) -> Option<String> {
        let pi = self.xml.nodes[d]
            .children
            .iter()
            .copied()
            .find(|&c| self.xml.nodes[c].kind == Kind::Pi && self.xml.nodes[c].name == "xml")?;
        self.xml.get_attr(pi, "encoding")
    }

    fn x_set_property(&mut self, d: usize, name: &Variant, val: &Variant) -> R {
        let nm = text_arg(name)?.to_ascii_lowercase();
        let dd = self.xml.doc_data_mut(d);
        match nm.as_str() {
            "selectionlanguage" => {
                let v = text_arg(val)?;
                match v.as_str() {
                    "XPath" => dd.xpath = true,
                    "XSLPattern" if dd.legacy => dd.xpath = false,
                    "XSLPattern" => {
                        return Err(msx(
                            "The XSLPattern selection language is not supported in this version of MSXML.",
                        ))
                    }
                    _ => return Err(msx("Property value is invalid.")),
                }
                Ok(Variant::Empty)
            }
            "selectionnamespaces" => {
                if matches!(val, Variant::Null) {
                    return Err(VbaError {
                        number: 0x8002_0005_u32 as i32,
                        description: "Type mismatch.\r\n".into(),
                    });
                }
                let v = text_arg(val)?;
                if v.is_empty() {
                    dd.sel_ns = None;
                    return Ok(Variant::Empty);
                }
                match parse_xmlns_list(&v) {
                    Ok(_) => {
                        dd.sel_ns = Some(v);
                        Ok(Variant::Empty)
                    }
                    Err(m) => Err(msx(&m)),
                }
            }
            "prohibitdtd" => {
                dd.prohibit_dtd = bool_arg(val)?;
                Ok(Variant::Empty)
            }
            "newparser" => {
                dd.new_parser = bool_arg(val)?;
                Ok(Variant::Empty)
            }
            "serverhttprequest" => {
                dd.server_http = bool_arg(val)?;
                Ok(Variant::Empty)
            }
            "maxelementdepth" | "maxxmlsize" | "allowxsltscript" | "allowdocumentfunction" => {
                Ok(Variant::Empty)
            }
            _ => Err(msx("Property name is invalid.")),
        }
    }

    fn x_get_property(&mut self, d: usize, name: &Variant) -> R {
        let nm = text_arg(name)?.to_ascii_lowercase();
        let dd = self.xml.doc_data(d);
        Ok(match nm.as_str() {
            "selectionlanguage" => bstr(if dd.xpath { "XPath" } else { "XSLPattern" }.into()),
            "selectionnamespaces" => bstr(dd.sel_ns.clone().unwrap_or_default()),
            "prohibitdtd" => Variant::Bool(dd.prohibit_dtd),
            "newparser" => Variant::Bool(dd.new_parser),
            "serverhttprequest" => Variant::Bool(dd.server_http),
            "maxelementdepth" => Variant::I4(256),
            "maxxmlsize" => Variant::I4(0),
            "allowxsltscript" | "allowdocumentfunction" => Variant::Bool(false),
            _ => return Err(msx("Property name is invalid.")),
        })
    }

    pub(crate) fn xlist_items(&self, l: &XList) -> Vec<usize> {
        match &l.src {
            ListSrc::Children(n) => self.xml.nodes[*n].children.clone(),
            ListSrc::Attrs(n) => self.xml.nodes[*n].attrs.clone(),
            ListSrc::Snap(v) => v.clone(),
        }
    }

    pub(crate) fn xlist_call(&mut self, i: usize, member: &str, put: bool, args: &[Variant]) -> R {
        let Some(ComObj::XmlList(l)) = self.com.get(i) else {
            return Err(unknown());
        };
        let l = l.clone();
        let items = self.xlist_items(&l);
        let name = member.to_ascii_lowercase();
        let attrs = matches!(l.src, ListSrc::Attrs(_));
        let owner = if let ListSrc::Attrs(o) = l.src { o } else { 0 };
        let set_cur = |app: &mut Self, c: usize| {
            if let Some(ComObj::XmlList(x)) = app.com.get_mut(i) {
                x.cur = c;
            }
        };
        if put {
            return Err(bad_count());
        }
        match name.as_str() {
            "length" => check_args(args, 0).map(|()| Variant::I4(items.len() as i32)),
            "item" | "" => {
                check_args(args, 1)?;
                let idx = match args[0].clone().canon() {
                    Variant::Bstr(s) => s
                        .trim()
                        .parse::<f64>()
                        .map_err(|_| VbaError::new(13))?
                        .round_ties_even() as i64,
                    _ => i64::from(int_arg(&args[0])?),
                };
                let k = usize::try_from(idx).ok().filter(|&k| k < items.len());
                if let (Some(k), ListSrc::Snap(_)) = (k, &l.src) {
                    set_cur(self, k + 1);
                }
                Ok(self.xo(k.map(|k| items[k])))
            }
            "nextnode" => {
                check_args(args, 0)?;
                match items.get(l.cur).copied() {
                    Some(n) => {
                        set_cur(self, l.cur + 1);
                        Ok(self.xh(n))
                    }
                    None => Ok(Variant::Empty),
                }
            }
            "reset" => {
                check_args(args, 0)?;
                set_cur(self, 0);
                Ok(if matches!(l.src, ListSrc::Snap(_)) {
                    Variant::Empty
                } else {
                    Variant::I4(0)
                })
            }
            "peeknode" if l.sel.is_some() => {
                check_args(args, 0)?;
                match items.get(l.cur).copied() {
                    Some(n) => Ok(self.xh(n)),
                    None => Ok(Variant::Empty),
                }
            }
            "expr" if l.sel.is_some() => {
                check_args(args, 0)?;
                Ok(bstr(l.sel.map(|s| s.0).unwrap_or_default()))
            }
            "context" if l.sel.is_some() => {
                check_args(args, 0)?;
                let c = l.sel.map(|s| s.1);
                Ok(self.xo(c))
            }
            "removeall" if l.sel.is_some() => {
                check_args(args, 0)?;
                for n in items {
                    self.xml.detach(n);
                }
                if let Some(ComObj::XmlList(x)) = self.com.get_mut(i) {
                    x.src = ListSrc::Snap(Vec::new());
                }
                Ok(Variant::Empty)
            }
            "matches" if l.sel.is_some() => {
                check_args(args, 1)?;
                let n = self.node_arg(&args[0])?;
                Ok(if items.contains(&n) {
                    self.xh(n)
                } else {
                    Variant::Empty
                })
            }
            "getproperty" if l.sel.is_some() => {
                check_args(args, 1)?;
                Err(msx("Property name is invalid."))
            }
            "getnameditem" if attrs => {
                check_args(args, 1)?;
                let a = text_arg(&args[0])?;
                let n = self.xml.find_attr(owner, &a);
                Ok(self.xo(n))
            }
            "getqualifieditem" if attrs => {
                check_args(args, 2)?;
                let (a, ns) = (text_arg(&args[0])?, text_arg(&args[1])?);
                let n = items.iter().copied().find(|&x| {
                    self.xml.base_name(x) == a
                        && self.xml.nodes[x].ns.as_deref().unwrap_or("") == ns
                });
                Ok(self.xo(n))
            }
            "removenameditem" if attrs => {
                check_args(args, 1)?;
                let a = text_arg(&args[0])?;
                match self.xml.find_attr(owner, &a) {
                    Some(n) => {
                        self.xml.detach(n);
                        Ok(self.xh(n))
                    }
                    None => Ok(Variant::Empty),
                }
            }
            "setnameditem" if attrs => {
                check_args(args, 1)?;
                let a = self.node_arg(&args[0])?;
                self.x_set_named(owner, a)?;
                Ok(self.xh(a))
            }
            _ => Err(unknown()),
        }
    }

    pub(crate) fn xerr_call(&mut self, e: &ParseErr, member: &str, put: bool) -> R {
        if put {
            return Err(bad_count());
        }
        Ok(match member.to_ascii_lowercase().as_str() {
            "errorcode" => Variant::I4(e.code),
            "reason" => {
                if e.code == 0 {
                    Variant::BstrNull
                } else {
                    bstr(e.reason.clone())
                }
            }
            "line" => Variant::I4(e.line),
            "linepos" => Variant::I4(e.linepos),
            "filepos" => Variant::I4(e.filepos),
            "srctext" => {
                if e.code == 0 || e.line == 0 {
                    Variant::BstrNull
                } else {
                    bstr(e.src.clone())
                }
            }
            "url" => match &e.url {
                Some(u) => bstr(u.clone()),
                None => Variant::BstrNull,
            },
            _ => return Err(unknown()),
        })
    }

    pub(crate) fn ximpl_call(&mut self, member: &str, args: &[Variant]) -> R {
        match member.to_ascii_lowercase().as_str() {
            "hasfeature" => {
                check_args(args, 2)?;
                Err(invalid())
            }
            _ => Err(unknown()),
        }
    }

    pub(crate) fn xschemas_call(&mut self, member: &str, args: &[Variant]) -> R {
        match member.to_ascii_lowercase().as_str() {
            "length" => check_args(args, 0).map(|()| Variant::I4(0)),
            _ => Err(unknown()),
        }
    }

    pub(crate) fn xml_type_name(&self, n: usize) -> &'static str {
        match self.xml.nodes[n].kind {
            Kind::Element => "IXMLDOMElement",
            Kind::Attribute => "IXMLDOMAttribute",
            Kind::Text => "IXMLDOMText",
            Kind::CData => "IXMLDOMCDATASection",
            Kind::EntityRef => "IXMLDOMEntityReference",
            Kind::Pi => "IXMLDOMProcessingInstruction",
            Kind::Comment => "IXMLDOMComment",
            Kind::Document => "DOMDocument60",
            Kind::Fragment => "IXMLDOMDocumentFragment",
        }
    }

    pub(crate) fn xml_enumerate(&mut self, n: usize) -> Vec<Variant> {
        let kids = self.xml.nodes[n].children.clone();
        kids.into_iter().map(|c| self.xh(c)).collect()
    }

    pub(crate) fn xlist_enumerate(&mut self, i: usize) -> Vec<Variant> {
        let items = match self.com.get(i) {
            Some(ComObj::XmlList(l)) => self.xlist_items(l),
            _ => Vec::new(),
        };
        items.into_iter().map(|c| self.xh(c)).collect()
    }
}

fn kind_upper(k: Kind) -> &'static str {
    match k {
        Kind::Document => "DOCUMENT",
        Kind::Element => "ELEMENT",
        Kind::Text => "TEXT",
        Kind::CData => "CDATA",
        Kind::Comment => "COMMENT",
        Kind::Pi => "PROCESSING_INSTRUCTION",
        Kind::Attribute => "ATTRIBUTE",
        Kind::EntityRef => "ENTITY_REFERENCE",
        Kind::Fragment => "DOCUMENT_FRAGMENT",
    }
}

fn kind_of(code: i32) -> Kind {
    match code {
        1 => Kind::Element,
        2 => Kind::Attribute,
        3 => Kind::Text,
        4 => Kind::CData,
        5 => Kind::EntityRef,
        7 => Kind::Pi,
        _ => Kind::Comment,
    }
}

fn attr_text(v: &Variant) -> Result<String, VbaError> {
    match v.clone().canon() {
        Variant::Bool(b) => Ok(if b { "-1".into() } else { "0".into() }),
        other => text_arg(&other),
    }
}

fn slice16(s: &str, from: usize, count: usize) -> String {
    let u: Vec<u16> = s.encode_utf16().collect();
    let a = from.min(u.len());
    let b = a.saturating_add(count).min(u.len());
    String::from_utf16_lossy(&u[a..b])
}

fn splice16(s: &str, from: usize, count: usize, with: &str) -> String {
    let u: Vec<u16> = s.encode_utf16().collect();
    let a = from.min(u.len());
    let b = a.saturating_add(count).min(u.len());
    let mut out: Vec<u16> = u[..a].to_vec();
    out.extend(with.encode_utf16());
    out.extend_from_slice(&u[b..]);
    String::from_utf16_lossy(&out)
}

fn parse_xmlns_attrs(s: &str) -> Result<Vec<(String, String)>, String> {
    let cs: Vec<char> = s.chars().collect();
    let mut i = 0;
    let mut out: Vec<(String, String)> = Vec::new();
    loop {
        while cs.get(i).is_some_and(|&c| crate::xmldom::is_ws(c)) {
            i += 1;
        }
        if i >= cs.len() {
            return Ok(out);
        }
        if !is_name_start(cs[i]) {
            return Err("A name was started with an invalid character.".into());
        }
        let b = i;
        while cs.get(i).is_some_and(|&c| is_name_char(c)) {
            i += 1;
        }
        let name: String = cs[b..i].iter().collect();
        while cs.get(i).is_some_and(|&c| crate::xmldom::is_ws(c)) {
            i += 1;
        }
        if cs.get(i) != Some(&'=') {
            return Err("A name contained an invalid character.".into());
        }
        i += 1;
        while cs.get(i).is_some_and(|&c| crate::xmldom::is_ws(c)) {
            i += 1;
        }
        let Some(&q) = cs.get(i).filter(|&&c| c == '\'' || c == '"') else {
            return Err(
                "A string literal was expected, but no opening quote character was found.".into(),
            );
        };
        i += 1;
        let vb = i;
        while cs.get(i).is_some_and(|&c| c != q) {
            i += 1;
        }
        if i >= cs.len() {
            return Err("A string literal was not closed.".into());
        }
        let v: String = cs[vb..i].iter().collect();
        i += 1;
        if out.iter().any(|(k, _)| *k == name) {
            return Err("Duplicate attribute.".into());
        }
        out.push((name, v));
    }
}

fn parse_xmlns_list(s: &str) -> Result<Vec<(String, String)>, String> {
    let mut out = Vec::new();
    for (k, v) in parse_xmlns_attrs(s)? {
        let prefix = if k == "xmlns" {
            String::new()
        } else if let Some(p) = k.strip_prefix("xmlns:") {
            p.to_string()
        } else {
            return Err("SelectionNamespaces property value is invalid.  Only well-formed xmlns attributes are allowed.".into());
        };
        out.push((prefix, v));
    }
    Ok(out)
}

type Decoded = Result<(String, Option<String>), (i32, &'static str, Option<usize>)>;

fn decode_bytes(b: &[u8]) -> Decoded {
    let u16s = |data: &[u8], le: bool| {
        let units: Vec<u16> = data
            .as_chunks::<2>()
            .0
            .iter()
            .map(|&c| {
                if le {
                    u16::from_le_bytes(c)
                } else {
                    u16::from_be_bytes(c)
                }
            })
            .collect();
        String::from_utf16_lossy(&units)
    };
    if let Some(r) = b.strip_prefix(&[0xEF, 0xBB, 0xBF]) {
        return String::from_utf8(r.to_vec())
            .map(|s| (s, None))
            .map_err(|_| {
                (
                    -1072896760,
                    "An invalid character was found in text content.",
                    Some(0),
                )
            });
    }
    if let Some(r) = b.strip_prefix(&[0xFF, 0xFE]) {
        return Ok((u16s(r, true), None));
    }
    if let Some(r) = b.strip_prefix(&[0xFE, 0xFF]) {
        return Ok((u16s(r, false), None));
    }
    if b.starts_with(&[0x3C, 0x00]) {
        return Ok((u16s(b, true), None));
    }
    if b.starts_with(&[0x00, 0x3C]) {
        return Ok((u16s(b, false), None));
    }
    let head: String = b.iter().take(120).map(|&c| c as char).collect();
    let enc = head
        .strip_prefix("<?xml")
        .and_then(|h| h.split("?>").next())
        .and_then(|h| h.split_once("encoding="))
        .and_then(|(_, r)| {
            let q = r.chars().next()?;
            let body = &r[1..];
            Some(body[..body.find(q)?].to_ascii_lowercase())
        });
    match enc.as_deref() {
        Some("windows-1252") => Ok((b.iter().map(|&c| from_1252(c)).collect(), enc)),
        Some("iso-8859-1" | "latin1" | "us-ascii" | "ascii") => {
            Ok((b.iter().map(|&c| c as char).collect(), enc))
        }
        Some("utf-16") => Err((
            -1072896657,
            "Switch from current encoding to specified encoding not supported.",
            None,
        )),
        _ => String::from_utf8(b.to_vec())
            .map(|s| (s, enc))
            .map_err(|e| {
                (
                    -1072896760,
                    "An invalid character was found in text content.",
                    Some(e.utf8_error().valid_up_to()),
                )
            }),
    }
}

fn from_1252(b: u8) -> char {
    const HIGH: [u16; 32] = [
        0x20AC, 0x81, 0x201A, 0x0192, 0x201E, 0x2026, 0x2020, 0x2021, 0x02C6, 0x2030, 0x0160,
        0x2039, 0x0152, 0x8D, 0x017D, 0x8F, 0x90, 0x2018, 0x2019, 0x201C, 0x201D, 0x2022, 0x2013,
        0x2014, 0x02DC, 0x2122, 0x0161, 0x203A, 0x0153, 0x9D, 0x017E, 0x0178,
    ];
    if (0x80..=0x9F).contains(&b) {
        char::from_u32(u32::from(HIGH[usize::from(b - 0x80)])).unwrap_or('?')
    } else {
        char::from(b)
    }
}

fn encode_text(xml: &str, enc: Option<&str>) -> Vec<u8> {
    match enc.map(str::to_ascii_lowercase).as_deref() {
        Some("utf-16") => {
            let mut out = vec![0xFF, 0xFE];
            for u in xml.encode_utf16() {
                out.extend_from_slice(&u.to_le_bytes());
            }
            out
        }
        Some(e @ ("iso-8859-1" | "latin1" | "windows-1252")) => {
            let mut out = Vec::new();
            for c in xml.chars() {
                if e == "windows-1252" {
                    if let Some(b) = (0x80..=0x9Fu8).find(|&b| from_1252(b) == c) {
                        out.push(b);
                        continue;
                    }
                }
                if (c as u32) < 256 && !(e == "windows-1252" && (0x80..=0x9F).contains(&(c as u32)))
                {
                    out.push(c as u32 as u8);
                } else {
                    out.extend(format!("&#{};", c as u32).bytes());
                }
            }
            out
        }
        _ => xml.as_bytes().to_vec(),
    }
}
