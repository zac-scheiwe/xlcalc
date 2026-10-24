use super::args::*;
use super::{scalar, FnDef};
use crate::eval::EvalCtx;
use crate::value::{Array, Value, XlError};

pub const DEFS: &[FnDef] = &[scalar("FILTERXML", 2, 2, filterxml)];

fn filterxml(ctx: &mut EvalCtx, a: &[Value]) -> Value {
    let (doc, path) = match (text(&a[0]), text(&a[1])) {
        (Ok(d), Ok(p)) => (d, p),
        (Err(e), _) | (_, Err(e)) => return Value::Error(e),
    };
    let Some(doc) = Doc::parse(&doc) else {
        return Value::Error(XlError::Value);
    };
    let Some(expr) = XPath::parse(&path) else {
        return Value::Error(XlError::Value);
    };
    let Some(nodes) = doc.select(&expr) else {
        return Value::Error(XlError::Value);
    };
    if nodes.is_empty() {
        return Value::Error(XlError::Value);
    }
    let env = ctx.parse_env();
    let mut out: Vec<Value> = nodes
        .into_iter()
        .map(|n| match doc.result_text(n) {
            None => Value::Error(XlError::Value),
            Some(s) if s.trim().is_empty() => Value::Error(XlError::Value),
            Some(s) => convert(&s, env),
        })
        .collect();
    if out.len() == 1 {
        out.pop().unwrap()
    } else {
        Value::Array(Array::column(out).into())
    }
}

fn convert(raw: &str, env: crate::coerce::ParseEnv) -> Value {
    let env = crate::coerce::ParseEnv { iso_t: true, ..env };
    let s = raw.trim_matches(|c: char| c.is_whitespace());
    if s.eq_ignore_ascii_case("TRUE") {
        return Value::Bool(true);
    }
    if s.eq_ignore_ascii_case("FALSE") {
        return Value::Bool(false);
    }
    let text = || Value::from(s);
    let num = |t: &str| crate::coerce::parse_number_text(t, env).map(|p| p.value);
    let year2 = |n: i64| if n < 30 { 2000 + n } else { 1900 + n };
    let month_date =
        |m: i64, y: i64| crate::datetime::date_serial(env.date_system, y as f64, m as f64, 1.0);
    let digit_only = |t: &str| !t.is_empty() && t.chars().all(|c| c.is_ascii_digit());

    if s.contains(' ') && s.split(' ').all(|t| digit_only(t) || t.is_empty()) {
        return text();
    }
    if s.starts_with('$') && s.ends_with('%') {
        return text();
    }

    if let Some((y, rest)) = s.split_once('-') {
        if y.len() == 4
            && digit_only(y)
            && y.parse::<i64>().unwrap_or(9999) < 1900
            && rest.contains('-')
        {
            return text();
        }
    }

    let parts: Vec<&str> = s.split([' ', '-']).filter(|t| !t.is_empty()).collect();
    if parts.len() == 2 && digit_only(parts[1]) && parts[1].len() <= 2 {
        if let Some(m) = crate::locale::month_from_name(parts[0]) {
            if let Some(v) = month_date(m as i64, year2(parts[1].parse().unwrap())) {
                return Value::Number(v);
            }
        }
    }
    let mut t = s.to_string();

    if t.chars().any(|c| ('\u{660}'..='\u{669}').contains(&c)) {
        t = t
            .chars()
            .map(|c| {
                if ('\u{660}'..='\u{669}').contains(&c) {
                    char::from(b'0' + (c as u32 - 0x660) as u8)
                } else {
                    c
                }
            })
            .collect();
    }

    if t.len() > 11 && t.as_bytes().get(10) == Some(&b'T') {
        t = t[..10].to_string() + " " + t[11..].trim_end_matches('Z');
    }

    if let Some(r) = t.strip_prefix('€').or_else(|| t.strip_suffix('€')) {
        if let Some(v) = num(r.trim()) {
            return Value::Number(v);
        }
    }
    if let Some(v) = num(&t) {
        return Value::Number(v);
    }

    if let Some((a, b)) = t.split_once('/') {
        if digit_only(a) && digit_only(b) && b.len() <= 2 {
            if let (Ok(m), Ok(y)) = (a.parse::<i64>(), b.parse::<i64>()) {
                if (1..=12).contains(&m) && y > 0 {
                    if let Some(v) = month_date(m, year2(y)) {
                        return Value::Number(v);
                    }
                }
            }
        }
    }
    text()
}

#[derive(Clone, Copy, PartialEq, Eq, Debug)]
enum Kind {
    Root,
    Elem,
    Text,
    Attr,
}

struct Node {
    kind: Kind,
    parent: usize,
    children: Vec<usize>,
    attrs: Vec<usize>,
    name: String,
    value: String,
}

struct Doc {
    nodes: Vec<Node>,
}

fn is_name_start(c: char) -> bool {
    c == '_' || c == ':' || c.is_alphabetic()
}

fn is_name_char(c: char) -> bool {
    is_name_start(c)
        || c.is_ascii_digit()
        || c == '-'
        || c == '.'
        || c == '\u{b7}'
        || c.is_numeric()
}

struct XmlParser<'a> {
    s: &'a [char],
    i: usize,
    doc: Doc,

    ns: Vec<Vec<String>>,
}

impl Doc {
    fn parse(src: &str) -> Option<Doc> {
        let chars: Vec<char> = src.chars().collect();
        let mut p = XmlParser {
            s: &chars,
            i: 0,
            doc: Doc {
                nodes: vec![Node {
                    kind: Kind::Root,
                    parent: 0,
                    children: vec![],
                    attrs: vec![],
                    name: String::new(),
                    value: String::new(),
                }],
            },
            ns: vec![vec!["xml".to_string()]],
        };
        p.document()?;
        Some(p.doc)
    }

    fn add(&mut self, kind: Kind, parent: usize, name: String, value: String) -> usize {
        let id = self.nodes.len();
        self.nodes.push(Node {
            kind,
            parent,
            children: vec![],
            attrs: vec![],
            name,
            value,
        });
        match kind {
            Kind::Attr => self.nodes[parent].attrs.push(id),
            _ => self.nodes[parent].children.push(id),
        }
        id
    }

    fn string_value(&self, n: usize) -> String {
        match self.nodes[n].kind {
            Kind::Text | Kind::Attr => self.nodes[n].value.clone(),
            _ => {
                let mut out = String::new();
                self.collect_text(n, &mut out);
                out
            }
        }
    }

    fn collect_text(&self, n: usize, out: &mut String) {
        for &c in &self.nodes[n].children {
            match self.nodes[c].kind {
                Kind::Text => out.push_str(&self.nodes[c].value),
                Kind::Elem => self.collect_text(c, out),
                _ => {}
            }
        }
    }

    fn result_text(&self, n: usize) -> Option<String> {
        match self.nodes[n].kind {
            Kind::Text => None,
            Kind::Attr => Some(self.nodes[n].value.clone()),
            _ => Some(
                self.nodes[n]
                    .children
                    .iter()
                    .filter(|&&c| self.nodes[c].kind == Kind::Text)
                    .map(|&c| self.nodes[c].value.as_str())
                    .collect(),
            ),
        }
    }
}

impl XmlParser<'_> {
    fn peek(&self) -> Option<char> {
        self.s.get(self.i).copied()
    }
    fn starts(&self, t: &str) -> bool {
        t.chars()
            .enumerate()
            .all(|(k, c)| self.s.get(self.i + k) == Some(&c))
    }
    fn skip_ws(&mut self) -> bool {
        let st = self.i;
        while matches!(self.peek(), Some(' ' | '\t' | '\n' | '\r')) {
            self.i += 1;
        }
        self.i > st
    }
    fn until(&mut self, end: &str) -> Option<String> {
        let st = self.i;
        while self.i < self.s.len() {
            if self.starts(end) {
                let t: String = self.s[st..self.i].iter().collect();
                self.i += end.chars().count();
                return Some(t);
            }
            self.i += 1;
        }
        None
    }
    fn name(&mut self) -> Option<String> {
        let st = self.i;
        if !self.peek().is_some_and(is_name_start) {
            return None;
        }
        while self.peek().is_some_and(is_name_char) {
            self.i += 1;
        }
        Some(self.s[st..self.i].iter().collect())
    }

    fn document(&mut self) -> Option<()> {
        if self.starts("\u{feff}") {
            self.i += 1;
        }
        if self.starts("<?xml") && self.s.get(self.i + 5).is_some_and(|c| c.is_whitespace()) {
            self.i += 2;
            self.until("?>")?;
        }
        let mut have_root = false;
        loop {
            self.skip_ws();
            if self.i >= self.s.len() {
                break;
            }
            if self.starts("<!--") {
                self.i += 4;
                self.until("-->")?;
            } else if self.starts("<?") {
                self.i += 2;
                let target = self.name()?;
                if target.eq_ignore_ascii_case("xml") {
                    return None;
                }
                self.until("?>")?;
            } else if self.starts("<") && !self.starts("<!") && !have_root {
                self.element(0)?;
                have_root = true;
            } else {
                return None;
            }
        }
        have_root.then_some(())
    }

    fn reference(&mut self) -> Option<char> {
        self.i += 1;
        let body = self.until(";")?;
        match body.as_str() {
            "amp" => Some('&'),
            "lt" => Some('<'),
            "gt" => Some('>'),
            "quot" => Some('"'),
            "apos" => Some('\''),
            b => {
                let n = b.strip_prefix('#')?;
                let code = if let Some(h) = n.strip_prefix('x') {
                    u32::from_str_radix(h, 16).ok()?
                } else {
                    n.parse::<u32>().ok()?
                };
                let c = char::from_u32(code)?;
                (c != '\0').then_some(c)
            }
        }
    }

    fn declared(&self, prefix: &str) -> bool {
        self.ns.iter().any(|f| f.iter().any(|p| p == prefix))
    }

    fn qname_ok(&self, n: &str) -> bool {
        match n.split_once(':') {
            Some((pre, local)) => {
                !pre.is_empty() && !local.is_empty() && !local.contains(':') && self.declared(pre)
            }
            None => true,
        }
    }

    fn element(&mut self, parent: usize) -> Option<()> {
        self.i += 1;
        let name = self.name()?;
        let mut attrs: Vec<(String, String)> = Vec::new();
        let mut frame = Vec::new();
        loop {
            let had_ws = self.skip_ws();
            match self.peek()? {
                '/' | '>' => break,
                _ => {
                    if !had_ws {
                        return None;
                    }
                    let an = self.name()?;
                    self.skip_ws();
                    if self.peek()? != '=' {
                        return None;
                    }
                    self.i += 1;
                    self.skip_ws();
                    let q = self.peek()?;
                    if q != '"' && q != '\'' {
                        return None;
                    }
                    self.i += 1;
                    let mut v = String::new();
                    loop {
                        match self.peek()? {
                            c if c == q => {
                                self.i += 1;
                                break;
                            }
                            '<' => return None,
                            '&' => v.push(self.reference()?),
                            '\t' | '\n' | '\r' => {
                                v.push(' ');
                                self.i += 1;
                            }
                            c => {
                                v.push(c);
                                self.i += 1;
                            }
                        }
                    }
                    if attrs.iter().any(|(n, _)| *n == an) {
                        return None;
                    }
                    if let Some(p) = an.strip_prefix("xmlns:") {
                        frame.push(p.to_string());
                    }
                    attrs.push((an, v));
                }
            }
        }
        self.ns.push(frame);
        if !self.qname_ok(&name) {
            return None;
        }
        let id = self
            .doc
            .add(Kind::Elem, parent, name.clone(), String::new());
        for (an, av) in attrs {
            if an == "xmlns" || an.starts_with("xmlns:") {
                continue;
            }
            if !self.qname_ok(&an) {
                return None;
            }
            self.doc.add(Kind::Attr, id, an, av);
        }
        if self.starts("/>") {
            self.i += 2;
            self.ns.pop();
            return Some(());
        }
        self.i += 1;
        let mut buf = String::new();
        loop {
            match self.peek()? {
                '<' => {
                    if self.starts("</") {
                        self.i += 2;
                        let close = self.name()?;
                        self.skip_ws();
                        if close != name || self.peek()? != '>' {
                            return None;
                        }
                        self.i += 1;
                        self.flush(id, &mut buf);
                        self.ns.pop();
                        return Some(());
                    } else if self.starts("<!--") {
                        self.i += 4;
                        self.until("-->")?;
                    } else if self.starts("<![CDATA[") {
                        self.i += 9;
                        buf.push_str(&self.until("]]>")?);
                    } else if self.starts("<?") {
                        self.i += 2;
                        self.name()?;
                        self.until("?>")?;
                    } else if self.starts("<!") {
                        return None;
                    } else {
                        self.flush(id, &mut buf);
                        self.element(id)?;
                    }
                }
                '&' => buf.push(self.reference()?),
                c => {
                    buf.push(c);
                    self.i += 1;
                }
            }
        }
    }

    fn flush(&mut self, parent: usize, buf: &mut String) {
        if !buf.is_empty() {
            self.doc
                .add(Kind::Text, parent, String::new(), std::mem::take(buf));
        }
    }
}

#[derive(Clone, Copy, PartialEq, Debug)]
enum Axis {
    Child,
    Descendant,
    DescendantOrSelf,
    Parent,
    Ancestor,
    AncestorOrSelf,
    FollowingSibling,
    PrecedingSibling,
    Following,
    Preceding,
    Attribute,
    SelfAxis,
}

#[derive(Clone, Debug)]
enum Test {
    Name(String),
    Any,
    Text,
    Node,

    Nothing,
}

#[derive(Clone, Debug)]
struct Step {
    axis: Axis,
    test: Test,
    preds: Vec<Expr>,
}

#[derive(Clone, Copy, PartialEq, Debug)]
enum Op {
    Or,
    And,
    Eq,
    Ne,
    Lt,
    Le,
    Gt,
    Ge,
    Add,
    Sub,
    Mul,
    Div,
    Mod,
}

#[derive(Clone, Debug)]
enum Expr {
    Num(f64),
    Str(String),
    Neg(Box<Expr>),
    Bin(Op, Box<Expr>, Box<Expr>),
    Union(Box<Expr>, Box<Expr>),
    Call(String, Vec<Expr>),
    Filter(Box<Expr>, Vec<Expr>),
    Path {
        base: Option<Box<Expr>>,
        abs: bool,
        steps: Vec<Step>,
    },
}

struct XPath(Expr);

struct XpParser {
    s: Vec<char>,
    i: usize,
}

impl XPath {
    fn parse(src: &str) -> Option<XPath> {
        let mut p = XpParser {
            s: src.chars().collect(),
            i: 0,
        };
        let e = p.or()?;
        p.ws();
        (p.i == p.s.len()).then_some(XPath(e))
    }
}

fn dos(steps: &mut Vec<Step>) {
    steps.push(Step {
        axis: Axis::DescendantOrSelf,
        test: Test::Node,
        preds: vec![],
    });
}

impl XpParser {
    fn ws(&mut self) {
        while self.s.get(self.i).is_some_and(|c| c.is_whitespace()) {
            self.i += 1;
        }
    }
    fn peek(&self) -> Option<char> {
        self.s.get(self.i).copied()
    }
    fn at(&self, t: &str) -> bool {
        t.chars()
            .enumerate()
            .all(|(k, c)| self.s.get(self.i + k) == Some(&c))
    }
    fn eat(&mut self, t: &str) -> bool {
        self.ws();
        if self.at(t) {
            self.i += t.chars().count();
            true
        } else {
            false
        }
    }
    fn eat_word(&mut self, w: &str) -> bool {
        self.ws();
        if self.at(w)
            && !self
                .s
                .get(self.i + w.len())
                .is_some_and(|&c| is_name_char(c))
        {
            self.i += w.len();
            true
        } else {
            false
        }
    }

    fn or(&mut self) -> Option<Expr> {
        let mut l = self.and()?;
        while self.eat_word("or") {
            let r = self.and()?;
            l = Expr::Bin(Op::Or, Box::new(l), Box::new(r));
        }
        Some(l)
    }
    fn and(&mut self) -> Option<Expr> {
        let mut l = self.equality()?;
        while self.eat_word("and") {
            let r = self.equality()?;
            l = Expr::Bin(Op::And, Box::new(l), Box::new(r));
        }
        Some(l)
    }
    fn equality(&mut self) -> Option<Expr> {
        let mut l = self.relational()?;
        loop {
            let op = if self.eat("!=") {
                Op::Ne
            } else if self.eat("=") {
                Op::Eq
            } else {
                return Some(l);
            };
            let r = self.relational()?;
            l = Expr::Bin(op, Box::new(l), Box::new(r));
        }
    }
    fn relational(&mut self) -> Option<Expr> {
        let mut l = self.additive()?;
        loop {
            let op = if self.eat("<=") {
                Op::Le
            } else if self.eat(">=") {
                Op::Ge
            } else if self.eat("<") {
                Op::Lt
            } else if self.eat(">") {
                Op::Gt
            } else {
                return Some(l);
            };
            let r = self.additive()?;
            l = Expr::Bin(op, Box::new(l), Box::new(r));
        }
    }
    fn additive(&mut self) -> Option<Expr> {
        let mut l = self.multiplicative()?;
        loop {
            let op = if self.eat("+") {
                Op::Add
            } else if self.eat("-") {
                Op::Sub
            } else {
                return Some(l);
            };
            let r = self.multiplicative()?;
            l = Expr::Bin(op, Box::new(l), Box::new(r));
        }
    }
    fn multiplicative(&mut self) -> Option<Expr> {
        let mut l = self.unary()?;
        loop {
            let op = if self.eat("*") {
                Op::Mul
            } else if self.eat_word("div") {
                Op::Div
            } else if self.eat_word("mod") {
                Op::Mod
            } else {
                return Some(l);
            };
            let r = self.unary()?;
            l = Expr::Bin(op, Box::new(l), Box::new(r));
        }
    }
    fn unary(&mut self) -> Option<Expr> {
        if self.eat("-") {
            return Some(Expr::Neg(Box::new(self.unary()?)));
        }
        let mut l = self.path()?;
        while self.eat("|") {
            let r = self.path()?;
            l = Expr::Union(Box::new(l), Box::new(r));
        }
        Some(l)
    }

    fn path(&mut self) -> Option<Expr> {
        self.ws();
        let c = self.peek()?;
        if c == '/' {
            self.i += 1;
            let mut steps = Vec::new();
            if self.peek() == Some('/') {
                self.i += 1;
                dos(&mut steps);
                steps.push(self.step()?);
            } else {
                self.ws();
                if self.starts_step() {
                    steps.push(self.step()?);
                } else {
                    return Some(Expr::Path {
                        base: None,
                        abs: true,
                        steps,
                    });
                }
            }
            self.more_steps(&mut steps)?;
            return Some(Expr::Path {
                base: None,
                abs: true,
                steps,
            });
        }
        if self.starts_primary() {
            let mut e = self.primary()?;
            let mut preds = Vec::new();
            while self.peek_pred() {
                preds.push(self.pred()?);
            }
            if !preds.is_empty() {
                e = Expr::Filter(Box::new(e), preds);
            }
            let mut steps = Vec::new();
            self.ws();
            if self.at("//") {
                self.i += 2;
                dos(&mut steps);
                steps.push(self.step()?);
                self.more_steps(&mut steps)?;
            } else if self.at("/") {
                self.i += 1;
                steps.push(self.step()?);
                self.more_steps(&mut steps)?;
            }
            if steps.is_empty() {
                return Some(e);
            }
            return Some(Expr::Path {
                base: Some(Box::new(e)),
                abs: false,
                steps,
            });
        }
        let mut steps = vec![self.step()?];
        self.more_steps(&mut steps)?;
        Some(Expr::Path {
            base: None,
            abs: false,
            steps,
        })
    }

    fn more_steps(&mut self, steps: &mut Vec<Step>) -> Option<()> {
        loop {
            self.ws();
            if self.at("//") {
                self.i += 2;
                dos(steps);
                steps.push(self.step()?);
            } else if self.at("/") {
                self.i += 1;
                steps.push(self.step()?);
            } else {
                return Some(());
            }
        }
    }

    fn starts_step(&self) -> bool {
        match self.peek() {
            Some(c) => is_name_start(c) || c == '*' || c == '@' || c == '.',
            None => false,
        }
    }

    fn starts_primary(&self) -> bool {
        match self.peek() {
            Some('"' | '\'' | '(' | '$') => true,
            Some(c) if c.is_ascii_digit() => true,
            Some('.') => self.s.get(self.i + 1).is_some_and(|c| c.is_ascii_digit()),
            Some(c) if is_name_start(c) => {
                let mut j = self.i;
                while self.s.get(j).is_some_and(|&c| is_name_char(c)) {
                    j += 1;
                }
                let name: String = self.s[self.i..j].iter().collect();
                while self.s.get(j).is_some_and(|c| c.is_whitespace()) {
                    j += 1;
                }
                self.s.get(j) == Some(&'(')
                    && !matches!(
                        name.as_str(),
                        "node" | "text" | "comment" | "processing-instruction"
                    )
            }
            _ => false,
        }
    }

    fn peek_pred(&mut self) -> bool {
        self.ws();
        self.peek() == Some('[')
    }

    fn pred(&mut self) -> Option<Expr> {
        self.i += 1;
        let e = self.or()?;
        if !self.eat("]") {
            return None;
        }
        Some(e)
    }

    fn primary(&mut self) -> Option<Expr> {
        self.ws();
        let c = self.peek()?;
        match c {
            '"' | '\'' => {
                self.i += 1;
                let st = self.i;
                while self.peek()? != c {
                    self.i += 1;
                }
                let t: String = self.s[st..self.i].iter().collect();
                self.i += 1;
                Some(Expr::Str(t))
            }
            '(' => {
                self.i += 1;
                let e = self.or()?;
                if !self.eat(")") {
                    return None;
                }
                Some(e)
            }
            '$' => None,
            c if c.is_ascii_digit() || c == '.' => {
                let st = self.i;
                while self.peek().is_some_and(|c| c.is_ascii_digit()) {
                    self.i += 1;
                }
                if self.peek() == Some('.') {
                    self.i += 1;
                    while self.peek().is_some_and(|c| c.is_ascii_digit()) {
                        self.i += 1;
                    }
                }
                let t: String = self.s[st..self.i].iter().collect();
                t.parse::<f64>().ok().map(Expr::Num)
            }
            _ => {
                let st = self.i;
                while self.peek().is_some_and(is_name_char) {
                    self.i += 1;
                }
                let name: String = self.s[st..self.i].iter().collect();
                if !self.eat("(") {
                    return None;
                }
                let mut args = Vec::new();
                if !self.eat(")") {
                    loop {
                        args.push(self.or()?);
                        if self.eat(")") {
                            break;
                        }
                        if !self.eat(",") {
                            return None;
                        }
                    }
                }
                Some(Expr::Call(name, args))
            }
        }
    }

    fn step(&mut self) -> Option<Step> {
        self.ws();
        if self.at("..") {
            self.i += 2;
            return Some(Step {
                axis: Axis::Parent,
                test: Test::Node,
                preds: vec![],
            });
        }
        if self.at(".") {
            self.i += 1;
            return Some(Step {
                axis: Axis::SelfAxis,
                test: Test::Node,
                preds: vec![],
            });
        }
        let mut axis = Axis::Child;
        if self.peek() == Some('@') {
            self.i += 1;
            axis = Axis::Attribute;
        } else {
            let save = self.i;
            let st = self.i;
            while self.peek().is_some_and(|c| is_name_char(c) && c != ':') {
                self.i += 1;
            }
            let w: String = self.s[st..self.i].iter().collect();
            if self.at("::") {
                axis = match w.as_str() {
                    "child" => Axis::Child,
                    "descendant" => Axis::Descendant,
                    "descendant-or-self" => Axis::DescendantOrSelf,
                    "parent" => Axis::Parent,
                    "ancestor" => Axis::Ancestor,
                    "ancestor-or-self" => Axis::AncestorOrSelf,
                    "following-sibling" => Axis::FollowingSibling,
                    "preceding-sibling" => Axis::PrecedingSibling,
                    "following" => Axis::Following,
                    "preceding" => Axis::Preceding,
                    "attribute" => Axis::Attribute,
                    "self" => Axis::SelfAxis,
                    _ => return None,
                };
                self.i += 2;
            } else {
                self.i = save;
            }
        }
        self.ws();
        let test = if self.peek() == Some('*') {
            self.i += 1;
            Test::Any
        } else {
            let st = self.i;
            if !self.peek().is_some_and(is_name_start) {
                return None;
            }
            while self.peek().is_some_and(is_name_char) {
                self.i += 1;
            }
            let name: String = self.s[st..self.i].iter().collect();
            let save = self.i;
            if self.eat("(") {
                let t = match name.as_str() {
                    "text" => Test::Text,
                    "node" => Test::Node,
                    "comment" | "processing-instruction" => Test::Nothing,
                    _ => return None,
                };
                if !self.eat(")") {
                    return None;
                }
                t
            } else {
                self.i = save;
                Test::Name(name)
            }
        };
        let mut preds = Vec::new();
        while self.peek_pred() {
            preds.push(self.pred()?);
        }
        Some(Step { axis, test, preds })
    }
}

#[derive(Clone, Debug)]
enum V {
    Nodes(Vec<usize>),
    Num(f64),
    Str(String),
    Bool(bool),
}

#[derive(Clone, Copy)]
struct Cx {
    node: usize,
    pos: usize,
    size: usize,
}

fn str_to_num(s: &str) -> f64 {
    let t = s.trim_matches(|c| matches!(c, ' ' | '\t' | '\n' | '\r'));
    let body = t.strip_prefix('-').unwrap_or(t);
    let ok = body.chars().any(|c| c.is_ascii_digit())
        && body.chars().all(|c| c.is_ascii_digit() || c == '.')
        && body.matches('.').count() <= 1;
    if ok {
        t.parse().unwrap_or(f64::NAN)
    } else {
        f64::NAN
    }
}

fn num_to_str(n: f64) -> String {
    if n.is_nan() {
        "NaN".into()
    } else if n.is_infinite() {
        if n > 0.0 {
            "Infinity".into()
        } else {
            "-Infinity".into()
        }
    } else if n == n.trunc() && n.abs() < 1e15 {
        format!("{}", n as i64)
    } else {
        format!("{n}")
    }
}

fn cmp_f(op: Op, a: f64, b: f64) -> bool {
    match op {
        Op::Eq => a == b,
        Op::Ne => a != b,
        Op::Lt => a < b,
        Op::Le => a <= b,
        Op::Gt => a > b,
        _ => a >= b,
    }
}

impl Doc {
    fn select(&self, x: &XPath) -> Option<Vec<usize>> {
        match self.eval(
            &x.0,
            Cx {
                node: 0,
                pos: 1,
                size: 1,
            },
            0,
        )? {
            V::Nodes(n) => Some(n),
            _ => None,
        }
    }

    fn to_str(&self, v: &V) -> String {
        match v {
            V::Nodes(n) => n.first().map(|&i| self.string_value(i)).unwrap_or_default(),
            V::Num(n) => num_to_str(*n),
            V::Str(s) => s.clone(),
            V::Bool(b) => if *b { "true" } else { "false" }.into(),
        }
    }
    fn to_num(&self, v: &V) -> f64 {
        match v {
            V::Num(n) => *n,
            V::Bool(b) => *b as u8 as f64,
            _ => str_to_num(&self.to_str(v)),
        }
    }
    fn to_bool(&self, v: &V) -> bool {
        match v {
            V::Nodes(n) => !n.is_empty(),
            V::Num(n) => *n != 0.0 && !n.is_nan(),
            V::Str(s) => !s.is_empty(),
            V::Bool(b) => *b,
        }
    }

    fn axis_nodes(&self, n: usize, axis: Axis, out: &mut Vec<usize>) {
        let node = &self.nodes[n];
        match axis {
            Axis::Child => out.extend(node.children.iter().copied()),
            Axis::Attribute => out.extend(node.attrs.iter().copied()),
            Axis::SelfAxis => out.push(n),
            Axis::Parent => {
                if n != 0 {
                    out.push(node.parent)
                }
            }
            Axis::Descendant | Axis::DescendantOrSelf => {
                if axis == Axis::DescendantOrSelf {
                    out.push(n);
                }
                self.descend(n, out);
            }
            Axis::Ancestor | Axis::AncestorOrSelf => {
                let mut c = n;
                if axis == Axis::AncestorOrSelf {
                    out.push(c);
                }
                while c != 0 {
                    c = self.nodes[c].parent;
                    out.push(c);
                }
            }
            Axis::FollowingSibling | Axis::PrecedingSibling => {
                if n == 0 || node.kind == Kind::Attr {
                    return;
                }
                let sib = &self.nodes[node.parent].children;
                let k = sib.iter().position(|&c| c == n).unwrap_or(0);
                if axis == Axis::FollowingSibling {
                    out.extend(sib[k + 1..].iter().copied());
                } else {
                    out.extend(sib[..k].iter().rev().copied());
                }
            }
            Axis::Following => {
                let mut sub = vec![n];
                self.descend(n, &mut sub);
                let mut end = n;
                for &d in &sub {
                    end = end.max(d);
                    for &a in &self.nodes[d].attrs {
                        end = end.max(a);
                    }
                }
                out.extend(
                    (end + 1..self.nodes.len()).filter(|&i| self.nodes[i].kind != Kind::Attr),
                );
            }
            Axis::Preceding => {
                let mut anc = vec![];
                let mut c = n;
                while c != 0 {
                    c = self.nodes[c].parent;
                    anc.push(c);
                }
                out.extend(
                    (1..n)
                        .rev()
                        .filter(|&i| self.nodes[i].kind != Kind::Attr && !anc.contains(&i)),
                );
            }
        }
    }

    fn descend(&self, n: usize, out: &mut Vec<usize>) {
        for &c in &self.nodes[n].children {
            out.push(c);
            self.descend(c, out);
        }
    }

    fn test(&self, n: usize, axis: Axis, t: &Test) -> bool {
        let k = self.nodes[n].kind;
        let principal = if axis == Axis::Attribute {
            Kind::Attr
        } else {
            Kind::Elem
        };
        match t {
            Test::Node => true,
            Test::Text => k == Kind::Text,
            Test::Nothing => false,
            Test::Any => k == principal,
            Test::Name(name) => k == principal && self.nodes[n].name == *name,
        }
    }

    fn eval(&self, e: &Expr, cx: Cx, depth: u32) -> Option<V> {
        if depth > 200 {
            return None;
        }
        let d = depth + 1;
        Some(match e {
            Expr::Num(n) => V::Num(*n),
            Expr::Str(s) => V::Str(s.clone()),
            Expr::Neg(x) => V::Num(-self.to_num(&self.eval(x, cx, d)?)),
            Expr::Union(a, b) => {
                let (V::Nodes(mut x), V::Nodes(y)) = (self.eval(a, cx, d)?, self.eval(b, cx, d)?)
                else {
                    return None;
                };
                x.extend(y);
                x.sort_unstable();
                x.dedup();
                V::Nodes(x)
            }
            Expr::Bin(op, a, b) => {
                let l = self.eval(a, cx, d)?;
                match op {
                    Op::Or | Op::And => {
                        let lb = self.to_bool(&l);
                        if lb == (*op == Op::Or) {
                            return Some(V::Bool(lb));
                        }
                        let r = self.eval(b, cx, d)?;
                        V::Bool(self.to_bool(&r))
                    }
                    _ => {
                        let r = self.eval(b, cx, d)?;
                        match op {
                            Op::Eq | Op::Ne | Op::Lt | Op::Le | Op::Gt | Op::Ge => {
                                V::Bool(self.compare(*op, &l, &r))
                            }
                            _ => {
                                let (x, y) = (self.to_num(&l), self.to_num(&r));
                                V::Num(match op {
                                    Op::Add => x + y,
                                    Op::Sub => x - y,
                                    Op::Mul => x * y,
                                    Op::Div => x / y,
                                    _ => x % y,
                                })
                            }
                        }
                    }
                }
            }
            Expr::Filter(base, preds) => {
                let V::Nodes(mut nodes) = self.eval(base, cx, d)? else {
                    return None;
                };
                nodes.sort_unstable();
                for p in preds {
                    nodes = self.apply_pred(nodes, p, d)?;
                }
                V::Nodes(nodes)
            }
            Expr::Path { base, abs, steps } => {
                let mut cur = match base {
                    Some(b) => match self.eval(b, cx, d)? {
                        V::Nodes(n) => n,
                        _ => return None,
                    },
                    None if *abs => vec![0],
                    None => vec![cx.node],
                };
                for st in steps {
                    let mut next: Vec<usize> = Vec::new();
                    for &n in &cur {
                        let mut cand = Vec::new();
                        self.axis_nodes(n, st.axis, &mut cand);
                        let mut sel: Vec<usize> = cand
                            .into_iter()
                            .filter(|&c| self.test(c, st.axis, &st.test))
                            .collect();
                        for p in &st.preds {
                            sel = self.apply_pred(sel, p, d)?;
                        }
                        next.extend(sel);
                    }
                    next.sort_unstable();
                    next.dedup();
                    cur = next;
                }
                V::Nodes(cur)
            }
            Expr::Call(name, args) => self.call(name, args, cx, d)?,
        })
    }

    fn apply_pred(&self, nodes: Vec<usize>, p: &Expr, d: u32) -> Option<Vec<usize>> {
        let size = nodes.len();
        let mut out = Vec::new();
        for (k, &n) in nodes.iter().enumerate() {
            let cx = Cx {
                node: n,
                pos: k + 1,
                size,
            };
            let keep = match self.eval(p, cx, d)? {
                V::Num(x) => x == (k + 1) as f64,
                v => self.to_bool(&v),
            };
            if keep {
                out.push(n);
            }
        }
        Some(out)
    }

    fn compare(&self, op: Op, l: &V, r: &V) -> bool {
        let eq = matches!(op, Op::Eq | Op::Ne);
        let cmp_s = |a: &str, b: &str| if op == Op::Eq { a == b } else { a != b };
        match (l, r) {
            (V::Nodes(a), V::Nodes(b)) => a.iter().any(|&x| {
                let xs = self.string_value(x);
                b.iter().any(|&y| {
                    let ys = self.string_value(y);
                    if eq {
                        cmp_s(&xs, &ys)
                    } else {
                        cmp_f(op, str_to_num(&xs), str_to_num(&ys))
                    }
                })
            }),
            (V::Nodes(a), other) | (other, V::Nodes(a)) => {
                let flipped = !matches!(l, V::Nodes(_));
                if let V::Bool(b) = other {
                    let nb = !a.is_empty();
                    return if flipped {
                        cmp_f(op, *b as u8 as f64, nb as u8 as f64)
                    } else {
                        cmp_f(op, nb as u8 as f64, *b as u8 as f64)
                    };
                }
                a.iter().any(|&x| {
                    let xs = self.string_value(x);
                    match other {
                        V::Num(n) => {
                            let xn = str_to_num(&xs);
                            if flipped {
                                cmp_f(op, *n, xn)
                            } else {
                                cmp_f(op, xn, *n)
                            }
                        }
                        V::Str(s) => {
                            if eq {
                                cmp_s(&xs, s)
                            } else if flipped {
                                cmp_f(op, str_to_num(s), str_to_num(&xs))
                            } else {
                                cmp_f(op, str_to_num(&xs), str_to_num(s))
                            }
                        }
                        _ => false,
                    }
                })
            }
            _ => {
                if eq {
                    if matches!(l, V::Bool(_)) || matches!(r, V::Bool(_)) {
                        cmp_f(
                            op,
                            self.to_bool(l) as u8 as f64,
                            self.to_bool(r) as u8 as f64,
                        )
                    } else if matches!(l, V::Num(_)) || matches!(r, V::Num(_)) {
                        cmp_f(op, self.to_num(l), self.to_num(r))
                    } else {
                        cmp_s(&self.to_str(l), &self.to_str(r))
                    }
                } else {
                    cmp_f(op, self.to_num(l), self.to_num(r))
                }
            }
        }
    }

    fn call(&self, name: &str, args: &[Expr], cx: Cx, d: u32) -> Option<V> {
        let mut v = Vec::with_capacity(args.len());
        for a in args {
            v.push(self.eval(a, cx, d)?);
        }
        let s = |k: usize| -> String {
            match v.get(k) {
                Some(x) => self.to_str(x),
                None => self.string_value(cx.node),
            }
        };
        let want = |lo: usize, hi: usize| (lo..=hi).contains(&v.len());
        Some(match name {
            "last" if want(0, 0) => V::Num(cx.size as f64),
            "position" if want(0, 0) => V::Num(cx.pos as f64),
            "count" if want(1, 1) => match &v[0] {
                V::Nodes(n) => V::Num(n.len() as f64),
                _ => return None,
            },
            "true" if want(0, 0) => V::Bool(true),
            "false" if want(0, 0) => V::Bool(false),
            "not" if want(1, 1) => V::Bool(!self.to_bool(&v[0])),
            "boolean" if want(1, 1) => V::Bool(self.to_bool(&v[0])),
            "string" if want(0, 1) => V::Str(s(0)),
            "number" if want(0, 1) => V::Num(match v.first() {
                Some(x) => self.to_num(x),
                None => str_to_num(&self.string_value(cx.node)),
            }),
            "string-length" if want(0, 1) => V::Num(s(0).chars().count() as f64),
            "normalize-space" if want(0, 1) => V::Str(
                s(0).split([' ', '\t', '\n', '\r'])
                    .filter(|t| !t.is_empty())
                    .collect::<Vec<_>>()
                    .join(" "),
            ),
            "concat" if v.len() >= 2 => V::Str((0..v.len()).map(s).collect()),
            "contains" if want(2, 2) => V::Bool(s(0).contains(&s(1))),
            "starts-with" if want(2, 2) => V::Bool(s(0).starts_with(&s(1))),
            "substring-before" if want(2, 2) => {
                let (a, b) = (s(0), s(1));
                V::Str(a.find(&b).map(|p| a[..p].to_string()).unwrap_or_default())
            }
            "substring-after" if want(2, 2) => {
                let (a, b) = (s(0), s(1));
                V::Str(
                    a.find(&b)
                        .map(|p| a[p + b.len()..].to_string())
                        .unwrap_or_default(),
                )
            }
            "substring" if want(2, 3) => {
                let st: Vec<char> = s(0).chars().collect();
                let rnd = |x: f64| (x + 0.5).floor();
                let from = rnd(self.to_num(&v[1]));
                let to = if v.len() == 3 {
                    from + rnd(self.to_num(&v[2]))
                } else {
                    f64::INFINITY
                };
                V::Str(
                    st.iter()
                        .enumerate()
                        .filter(|(k, _)| {
                            let p = (*k + 1) as f64;
                            p >= from && p < to
                        })
                        .map(|(_, c)| *c)
                        .collect(),
                )
            }
            "translate" if want(3, 3) => {
                let (a, from, to) = (
                    s(0),
                    s(1).chars().collect::<Vec<_>>(),
                    s(2).chars().collect::<Vec<_>>(),
                );
                V::Str(
                    a.chars()
                        .filter_map(|c| match from.iter().position(|&f| f == c) {
                            Some(k) => to.get(k).copied(),
                            None => Some(c),
                        })
                        .collect(),
                )
            }
            "sum" if want(1, 1) => match &v[0] {
                V::Nodes(n) => V::Num(n.iter().map(|&i| str_to_num(&self.string_value(i))).sum()),
                _ => return None,
            },
            "floor" if want(1, 1) => V::Num(self.to_num(&v[0]).floor()),
            "ceiling" if want(1, 1) => V::Num(self.to_num(&v[0]).ceil()),
            "round" if want(1, 1) => V::Num((self.to_num(&v[0]) + 0.5).floor()),
            "name" | "local-name" if want(0, 1) => {
                let n = match v.first() {
                    Some(V::Nodes(n)) => n.first().copied(),
                    Some(_) => return None,
                    None => Some(cx.node),
                };
                let nm = n.map(|i| self.nodes[i].name.clone()).unwrap_or_default();
                V::Str(if name == "local-name" {
                    nm.rsplit(':').next().unwrap_or("").to_string()
                } else {
                    nm
                })
            }
            "namespace-uri" if want(0, 1) => V::Str(String::new()),
            _ => return None,
        })
    }
}
