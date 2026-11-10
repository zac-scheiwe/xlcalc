use crate::xmldom::{is_name_char, is_name_start, is_ws, Kind, Store};

#[derive(Clone, Debug, PartialEq)]
enum Tok {
    Num(f64),
    Lit(String),
    Name(String),
    Var,
    Op(&'static str),
    Eof,
    Bad,
}

impl Tok {
    fn label(&self) -> String {
        match self {
            Tok::Num(_) => "NUMBER".into(),
            Tok::Lit(_) => "LITERAL".into(),
            Tok::Name(_) => "NAME".into(),
            Tok::Var => "VARIABLE".into(),
            Tok::Op(o) => (*o).into(),
            Tok::Eof => "EOF".into(),
            Tok::Bad => "ERROR".into(),
        }
    }
}

#[derive(Clone, Copy, Debug, PartialEq)]
enum Axis {
    Child,
    Descendant,
    Parent,
    Ancestor,
    FollowingSibling,
    PrecedingSibling,
    Following,
    Preceding,
    Attribute,
    Itself,
    DescendantOrSelf,
    AncestorOrSelf,
}

#[derive(Clone, Debug)]
enum Test {
    Name(Option<String>, String),
    AnyName,
    NsAny(String),
    Node,
    Text,
    Comment,
    Pi(Option<String>),
}

#[derive(Clone, Debug)]
struct Step {
    axis: Axis,
    test: Test,
    preds: Vec<E>,
}

#[derive(Clone, Debug)]
enum E {
    Num(f64),
    Str(String),
    Neg(Box<E>),
    Bin(&'static str, Box<E>, Box<E>),
    Call(String, Vec<E>),
    Path {
        start: Option<Box<E>>,
        abs: bool,
        steps: Vec<Step>,
    },
    Filter(Box<E>, Vec<E>),
}

#[derive(Clone, Debug)]
enum V {
    Nodes(Vec<usize>),
    Num(f64),
    Str(String),
    Bool(bool),
}

fn lex(s: &[char]) -> Vec<(Tok, usize, usize)> {
    let mut out: Vec<(Tok, usize, usize)> = Vec::new();
    let mut i = 0;
    while i < s.len() {
        if is_ws(s[i]) {
            i += 1;
            continue;
        }
        let start = i;

        let operand_end = out.last().is_some_and(|(t, _, _)| match t {
            Tok::Op(o) => matches!(*o, ")" | "]" | "." | ".."),
            Tok::Name(_) | Tok::Num(_) | Tok::Lit(_) | Tok::Var => true,
            _ => false,
        });
        let c = s[i];
        let tok = if c.is_ascii_digit()
            || (c == '.' && s.get(i + 1).is_some_and(char::is_ascii_digit))
        {
            while i < s.len() && s[i].is_ascii_digit() {
                i += 1;
            }
            if i < s.len() && s[i] == '.' {
                i += 1;
                while i < s.len() && s[i].is_ascii_digit() {
                    i += 1;
                }
            }
            let t: String = s[start..i].iter().collect();
            Tok::Num(t.parse().unwrap_or(f64::NAN))
        } else if c == '"' || c == '\'' {
            i += 1;
            let b = i;
            while i < s.len() && s[i] != c {
                i += 1;
            }
            if i >= s.len() {
                out.push((Tok::Bad, start, s.len()));
                return out;
            }
            let t: String = s[b..i].iter().collect();
            i += 1;
            Tok::Lit(t)
        } else if c == '$' {
            i += 1;
            while i < s.len() && is_name_char(s[i]) {
                i += 1;
            }
            Tok::Var
        } else if c == '*' {
            i += 1;
            Tok::Op(if operand_end { "mul" } else { "*" })
        } else if is_name_start(c) && c != ':' {
            while i < s.len() && is_name_char(s[i]) && !(s[i] == ':' && s.get(i + 1) == Some(&':'))
            {
                i += 1;
            }
            if i < s.len() && s[i] == ':' && s.get(i + 1) == Some(&'*') {
                i += 2;
            }
            let t: String = s[start..i].iter().collect();
            if operand_end && matches!(t.as_str(), "and" | "or" | "mod" | "div") {
                Tok::Op(match t.as_str() {
                    "and" => "and",
                    "or" => "or",
                    "mod" => "mod",
                    _ => "div",
                })
            } else {
                Tok::Name(t)
            }
        } else {
            let two: String = s[i..(i + 2).min(s.len())].iter().collect();
            let (op, n): (&'static str, usize) = match two.as_str() {
                "//" => ("//", 2),
                ".." => ("..", 2),
                "::" => ("::", 2),
                "!=" => ("!=", 2),
                "<=" => ("<=", 2),
                ">=" => (">=", 2),
                _ => match c {
                    '/' => ("/", 1),
                    '|' => ("|", 1),
                    '+' => ("+", 1),
                    '-' => ("-", 1),
                    '=' => ("=", 1),
                    '<' => ("<", 1),
                    '>' => (">", 1),
                    '(' => ("(", 1),
                    ')' => (")", 1),
                    '[' => ("[", 1),
                    ']' => ("]", 1),
                    '@' => ("@", 1),
                    ',' => (",", 1),
                    '.' => (".", 1),
                    _ => {
                        i += 1;
                        out.push((Tok::Bad, start, i));
                        continue;
                    }
                },
            };
            i += n;
            Tok::Op(op)
        };
        out.push((tok, start, i));
    }
    out.push((Tok::Eof, s.len(), s.len()));
    out
}

struct Parser<'a> {
    src: &'a [char],
    toks: Vec<(Tok, usize, usize)>,
    p: usize,
    lead: Option<(usize, usize)>,
    ns: &'a [(String, String)],
}

type PR<T> = Result<T, String>;

impl Parser<'_> {
    fn marked(&self, a: usize, b: usize) -> String {
        let pre: String = self.src[..a].iter().collect();
        let mid: String = self.src[a..b].iter().collect();
        let post: String = self.src[b..].iter().collect();
        if a == b {
            format!("{pre}<--{post}")
        } else {
            format!("{pre}-->{mid}<--{post}")
        }
    }

    fn err<T>(&self, msg: &str, a: usize, b: usize) -> PR<T> {
        Err(format!("{msg}\r\n\n{}", self.marked(a, b)))
    }

    fn tok(&self) -> &Tok {
        &self.toks[self.p].0
    }

    fn span(&self) -> (usize, usize) {
        (self.toks[self.p].1, self.toks[self.p].2)
    }

    fn is_op(&self, o: &str) -> bool {
        matches!(self.tok(), Tok::Op(x) if *x == o)
    }

    fn peek_op(&self, k: usize, o: &str) -> bool {
        matches!(self.toks.get(self.p + k), Some((Tok::Op(x), _, _)) if *x == o)
    }

    fn expect(&mut self, o: &'static str) -> PR<()> {
        if self.is_op(o) {
            self.p += 1;
            return Ok(());
        }
        let (a, b) = self.span();
        let found = self.tok().label();
        self.err(&format!("Expected token '{o}' found '{found}'."), a, b)
    }

    fn parse_top(&mut self) -> PR<E> {
        if matches!(self.tok(), Tok::Eof | Tok::Bad)
            || (self.is_op("]") || self.is_op(")") || self.is_op("[") || self.is_op(","))
        {
            let (a, b) = self.span();
            return self.err("Expression must evaluate to a node-set.", a, b);
        }
        let e = self.or_expr()?;
        if !matches!(self.tok(), Tok::Eof) {
            let (a, b) = self.span();
            let found = self.tok().label();
            return self.err(&format!("Expected token 'EOF' found '{found}'."), a, b);
        }
        if !is_nodeset(&e) {
            let (a, b) = self.lead.unwrap_or((0, 0));
            return self.err("Expression must evaluate to a node-set.", a, b);
        }
        Ok(e)
    }

    fn bin(&mut self, ops: &[&'static str], next: fn(&mut Self) -> PR<E>) -> PR<E> {
        let mut l = next(self)?;
        loop {
            let Some(op) = ops.iter().copied().find(|o| self.is_op(o)) else {
                return Ok(l);
            };
            self.p += 1;
            let r = next(self)?;
            l = E::Bin(op, Box::new(l), Box::new(r));
        }
    }

    fn or_expr(&mut self) -> PR<E> {
        self.bin(&["or"], Self::and_expr)
    }
    fn and_expr(&mut self) -> PR<E> {
        self.bin(&["and"], Self::eq_expr)
    }
    fn eq_expr(&mut self) -> PR<E> {
        self.bin(&["=", "!="], Self::rel_expr)
    }
    fn rel_expr(&mut self) -> PR<E> {
        self.bin(&["<=", ">=", "<", ">"], Self::add_expr)
    }
    fn add_expr(&mut self) -> PR<E> {
        self.bin(&["+", "-"], Self::mul_expr)
    }
    fn mul_expr(&mut self) -> PR<E> {
        self.bin(&["mul", "div", "mod"], Self::unary)
    }

    fn unary(&mut self) -> PR<E> {
        if self.is_op("-") {
            self.p += 1;
            let e = self.unary()?;
            return Ok(E::Neg(Box::new(e)));
        }
        self.union()
    }

    fn union(&mut self) -> PR<E> {
        let mut l = self.path_expr()?;
        while self.is_op("|") {
            self.p += 1;
            let r = self.path_expr()?;
            l = E::Bin("|", Box::new(l), Box::new(r));
        }
        Ok(l)
    }

    fn starts_primary(&self) -> bool {
        match self.tok() {
            Tok::Num(_) | Tok::Lit(_) | Tok::Var => true,
            Tok::Op("(") => true,
            Tok::Name(_) => self.peek_op(1, "(") && !is_node_type(self.name_str()),
            _ => false,
        }
    }

    fn name_str(&self) -> &str {
        match self.tok() {
            Tok::Name(n) => n,
            _ => "",
        }
    }

    fn path_expr(&mut self) -> PR<E> {
        if self.starts_primary() {
            let start_tok = self.p;
            let prim = self.primary()?;
            if start_tok == 0 {
                self.lead = Some((self.toks[0].1, self.toks[self.p - 1].2));
            }
            let mut e = prim;
            let mut preds = Vec::new();
            while self.is_op("[") {
                preds.push(self.predicate()?);
            }
            if !preds.is_empty() {
                e = E::Filter(Box::new(e), preds);
            }
            if self.is_op("/") || self.is_op("//") {
                let mut steps = Vec::new();
                self.rel_steps(&mut steps)?;
                return Ok(E::Path {
                    start: Some(Box::new(e)),
                    abs: false,
                    steps,
                });
            }
            return Ok(e);
        }
        let mut steps = Vec::new();
        let abs = self.is_op("/") || self.is_op("//");
        if abs {
            let dbl = self.is_op("//");
            self.p += 1;
            if dbl {
                steps.push(descendant_or_self());
                self.step_into(&mut steps)?;
            } else if self.can_start_step() {
                self.step_into(&mut steps)?;
            }
        } else {
            if !self.can_start_step() {
                let (a, b) = self.span();
                return self.err("Expression expected.", a, b);
            }
            self.step_into(&mut steps)?;
        }
        self.rel_steps(&mut steps)?;
        Ok(E::Path {
            start: None,
            abs,
            steps,
        })
    }

    fn can_start_step(&self) -> bool {
        matches!(self.tok(), Tok::Name(_) | Tok::Op("*" | "@" | "." | ".."))
    }

    fn rel_steps(&mut self, steps: &mut Vec<Step>) -> PR<()> {
        while self.is_op("/") || self.is_op("//") {
            let dbl = self.is_op("//");
            self.p += 1;
            if dbl {
                steps.push(descendant_or_self());
            }
            self.step_into(steps)?;
        }
        Ok(())
    }

    fn step_into(&mut self, steps: &mut Vec<Step>) -> PR<()> {
        let st = self.step()?;
        steps.push(st);
        Ok(())
    }

    fn step(&mut self) -> PR<Step> {
        if self.is_op(".") {
            self.p += 1;
            return Ok(Step {
                axis: Axis::Itself,
                test: Test::Node,
                preds: Vec::new(),
            });
        }
        if self.is_op("..") {
            self.p += 1;
            return Ok(Step {
                axis: Axis::Parent,
                test: Test::Node,
                preds: Vec::new(),
            });
        }
        let mut axis = Axis::Child;
        if self.is_op("@") {
            self.p += 1;
            axis = Axis::Attribute;
        } else if let (Tok::Name(n), true) = (self.tok().clone(), self.peek_op(1, "::")) {
            axis = match n.as_str() {
                "child" => Axis::Child,
                "descendant" => Axis::Descendant,
                "parent" => Axis::Parent,
                "ancestor" => Axis::Ancestor,
                "following-sibling" => Axis::FollowingSibling,
                "preceding-sibling" => Axis::PrecedingSibling,
                "following" => Axis::Following,
                "preceding" => Axis::Preceding,
                "attribute" => Axis::Attribute,
                "self" => Axis::Itself,
                "descendant-or-self" => Axis::DescendantOrSelf,
                "ancestor-or-self" => Axis::AncestorOrSelf,
                _ => {
                    let (a, b) = self.span();
                    return self.err("Unknown axis.", a, b);
                }
            };
            self.p += 2;
        }
        let test = self.node_test()?;
        let mut preds = Vec::new();
        while self.is_op("[") {
            preds.push(self.predicate()?);
        }
        Ok(Step { axis, test, preds })
    }

    fn node_test(&mut self) -> PR<Test> {
        let (a, b) = self.span();
        match self.tok().clone() {
            Tok::Op("*") => {
                self.p += 1;
                Ok(Test::AnyName)
            }
            Tok::Name(n) => {
                if self.peek_op(1, "(") && is_node_type(&n) {
                    self.p += 2;
                    let mut lit = None;
                    if let Tok::Lit(l) = self.tok().clone() {
                        lit = Some(l);
                        self.p += 1;
                    }
                    self.expect(")")?;
                    return Ok(match n.as_str() {
                        "text" => Test::Text,
                        "comment" => Test::Comment,
                        "processing-instruction" => Test::Pi(lit),
                        _ => Test::Node,
                    });
                }
                self.p += 1;
                if let Some(p) = n.strip_suffix(":*") {
                    let uri = self.resolve(p, a, b)?;
                    return Ok(Test::NsAny(uri));
                }
                if let Some((p, l)) = n.split_once(':') {
                    let uri = self.resolve(p, a, b)?;
                    return Ok(Test::Name(Some(uri), l.to_string()));
                }
                Ok(Test::Name(None, n))
            }
            _ => self.err("NodeTest expected here.", a, b),
        }
    }

    fn resolve(&self, p: &str, _a: usize, _b: usize) -> PR<String> {
        if p == "xml" {
            return Ok(crate::xmldom::XML_URI.to_string());
        }
        if p == "xmlns" {
            return Ok(crate::xmldom::XMLNS_URI.to_string());
        }
        self.ns
            .iter()
            .rev()
            .find(|(q, _)| q == p)
            .map(|(_, u)| u.clone())
            .ok_or_else(|| format!("Reference to undeclared namespace prefix: '{p}'.\r\n"))
    }

    fn predicate(&mut self) -> PR<E> {
        self.expect("[")?;
        let e = self.or_expr_in()?;
        self.expect("]")?;
        Ok(e)
    }

    fn or_expr_in(&mut self) -> PR<E> {
        self.or_expr()
    }

    fn primary(&mut self) -> PR<E> {
        let (a, b) = self.span();
        match self.tok().clone() {
            Tok::Num(n) => {
                self.p += 1;
                Ok(E::Num(n))
            }
            Tok::Lit(s) => {
                self.p += 1;
                Ok(E::Str(s))
            }
            Tok::Var => self.err("Variables may not be used within this expression.", a, b),
            Tok::Op("(") => {
                self.p += 1;
                let e = self.or_expr()?;
                self.expect(")")?;
                Ok(e)
            }
            Tok::Name(n) => {
                self.p += 2;
                let mut args = Vec::new();
                if !self.is_op(")") {
                    loop {
                        args.push(self.or_expr()?);
                        if self.is_op(",") {
                            self.p += 1;
                        } else {
                            break;
                        }
                    }
                }
                self.expect(")")?;
                let end = self.toks[self.p - 1].2;
                let (min, max) = match arity(&n) {
                    Some(r) => r,
                    None => return self.err("Unknown method.", a, end),
                };
                if args.len() < min || args.len() > max {
                    return self.err("Invalid number of arguments.", a, end);
                }
                if n == "id" {
                    if let Some(E::Str(lit)) = args.first() {
                        if let Some(c) = lit.chars().next().filter(|&c| !is_name_start(c)) {
                            let rest: String = lit.chars().skip(1).collect();
                            return Err(format!(
                                "This name may not begin with the '{c}' character:\r\n\r\n-->{c}<--{rest}\r\n"
                            ));
                        }
                    }
                }
                Ok(E::Call(n, args))
            }
            _ => self.err("Expression expected.", a, b),
        }
    }
}

fn descendant_or_self() -> Step {
    Step {
        axis: Axis::DescendantOrSelf,
        test: Test::Node,
        preds: Vec::new(),
    }
}

fn is_node_type(n: &str) -> bool {
    matches!(n, "node" | "text" | "comment" | "processing-instruction")
}

fn arity(n: &str) -> Option<(usize, usize)> {
    Some(match n {
        "last" | "position" | "true" | "false" => (0, 0),
        "count" | "sum" | "floor" | "ceiling" | "round" | "boolean" | "not" | "lang" | "id" => {
            (1, 1)
        }
        "local-name" | "namespace-uri" | "name" | "string" | "string-length"
        | "normalize-space" | "number" => (0, 1),
        "concat" => (2, usize::MAX),
        "starts-with" | "contains" | "substring-before" | "substring-after" => (2, 2),
        "substring" => (2, 3),
        "translate" => (3, 3),
        _ => return None,
    })
}

fn is_nodeset(e: &E) -> bool {
    match e {
        E::Path { .. } | E::Filter(..) => true,
        E::Bin("|", ..) => true,
        E::Call(n, _) => n == "id",
        _ => false,
    }
}

struct Cx<'a> {
    st: &'a Store,
}

#[derive(Clone, Copy)]
struct Pos {
    node: usize,
    pos: usize,
    size: usize,
}

fn num_text(x: f64) -> String {
    if x.is_nan() {
        "NaN".into()
    } else if x.is_infinite() {
        if x > 0.0 { "Infinity" } else { "-Infinity" }.into()
    } else if x == 0.0 {
        "0".into()
    } else {
        format!("{x}")
    }
}

fn str_num(s: &str) -> f64 {
    let t = s.trim_matches(is_ws);
    let body = t.strip_prefix('-').unwrap_or(t);
    let ok = !body.is_empty()
        && body.chars().all(|c| c.is_ascii_digit() || c == '.')
        && body.matches('.').count() <= 1
        && body != ".";
    if ok {
        t.parse().unwrap_or(f64::NAN)
    } else {
        f64::NAN
    }
}

impl Cx<'_> {
    fn sort(&self, v: &mut Vec<usize>) {
        v.sort_by_key(|&n| self.order(n));
        v.dedup();
    }

    fn order(&self, n: usize) -> Vec<(u8, usize)> {
        let mut k = Vec::new();
        let mut cur = n;
        loop {
            let node = &self.st.nodes[cur];
            if let Some(p) = node.parent {
                k.push((1, self.st.index_in_parent(cur).unwrap_or(0)));
                cur = p;
            } else if let Some(o) = node.owner {
                k.push((
                    0,
                    self.st.nodes[o]
                        .attrs
                        .iter()
                        .position(|&a| a == cur)
                        .unwrap_or(0),
                ));
                cur = o;
            } else {
                break;
            }
        }
        k.reverse();
        k
    }

    fn root_of(&self, n: usize) -> usize {
        let mut cur = n;
        while let Some(p) = self.st.up(cur) {
            cur = p;
        }
        cur
    }

    fn string_value(&self, n: usize) -> String {
        let node = &self.st.nodes[n];
        match node.kind {
            Kind::Text | Kind::CData | Kind::Comment | Kind::Pi => node.value.clone(),
            Kind::Attribute => self.st.attr_value(n),
            Kind::EntityRef => String::new(),
            _ => {
                let mut out = String::new();
                self.collect(n, &mut out);
                out
            }
        }
    }

    fn collect(&self, n: usize, out: &mut String) {
        for &c in &self.st.nodes[n].children {
            match self.st.nodes[c].kind {
                Kind::Text | Kind::CData => out.push_str(&self.st.nodes[c].value),
                Kind::Element | Kind::EntityRef => self.collect(c, out),
                _ => {}
            }
        }
    }

    fn to_str(&self, v: &V) -> String {
        match v {
            V::Str(s) => s.clone(),
            V::Num(n) => num_text(*n),
            V::Bool(b) => if *b { "true" } else { "false" }.into(),
            V::Nodes(ns) => ns.first().map_or(String::new(), |&n| self.string_value(n)),
        }
    }

    fn to_num(&self, v: &V) -> f64 {
        match v {
            V::Num(n) => *n,
            V::Bool(b) => f64::from(u8::from(*b)),
            other => str_num(&self.to_str(other)),
        }
    }

    fn to_bool(v: &V) -> bool {
        match v {
            V::Bool(b) => *b,
            V::Num(n) => *n != 0.0 && !n.is_nan(),
            V::Str(s) => !s.is_empty(),
            V::Nodes(n) => !n.is_empty(),
        }
    }

    fn axis_nodes(&self, axis: Axis, n: usize) -> Vec<usize> {
        let st = self.st;
        let mut out = Vec::new();
        match axis {
            Axis::Child => out.extend(st.nodes[n].children.iter().copied()),
            Axis::Attribute => out.extend(
                st.nodes[n]
                    .attrs
                    .iter()
                    .copied()
                    .filter(|&a| st.nodes[a].ns.as_deref() != Some(crate::xmldom::XMLNS_URI)),
            ),
            Axis::Itself => out.push(n),
            Axis::Parent => out.extend(st.up(n)),
            Axis::Descendant | Axis::DescendantOrSelf => {
                if axis == Axis::DescendantOrSelf {
                    out.push(n);
                }
                self.desc(n, &mut out);
            }
            Axis::Ancestor | Axis::AncestorOrSelf => {
                if axis == Axis::AncestorOrSelf {
                    out.push(n);
                }
                let mut cur = n;
                while let Some(p) = st.up(cur) {
                    out.push(p);
                    cur = p;
                }
            }
            Axis::FollowingSibling | Axis::PrecedingSibling => {
                if st.nodes[n].kind != Kind::Attribute {
                    if let Some(p) = st.nodes[n].parent {
                        let kids = &st.nodes[p].children;
                        let i = kids.iter().position(|&c| c == n).unwrap_or(0);
                        if axis == Axis::FollowingSibling {
                            out.extend(kids[i + 1..].iter().copied());
                        } else {
                            out.extend(kids[..i].iter().rev().copied());
                        }
                    }
                }
            }
            Axis::Following | Axis::Preceding => {
                let mut cur = n;
                if st.nodes[cur].kind == Kind::Attribute {
                    cur = st.nodes[cur].owner.unwrap_or(cur);
                    if axis == Axis::Following {
                        self.desc(cur, &mut out);
                    }
                }
                let mut anc = Vec::new();
                while let Some(p) = st.nodes[cur].parent {
                    let kids = &st.nodes[p].children;
                    let i = kids.iter().position(|&c| c == cur).unwrap_or(0);
                    if axis == Axis::Following {
                        for &s in &kids[i + 1..] {
                            out.push(s);
                            self.desc(s, &mut out);
                        }
                    } else {
                        for &s in kids[..i].iter().rev() {
                            let mut sub = Vec::new();
                            self.desc(s, &mut sub);
                            out.extend(sub.into_iter().rev());
                            out.push(s);
                        }
                        anc.push(p);
                    }
                    cur = p;
                }
            }
        }
        out
    }

    fn desc(&self, n: usize, out: &mut Vec<usize>) {
        for &c in &self.st.nodes[n].children {
            out.push(c);
            self.desc(c, out);
        }
    }

    fn test(&self, t: &Test, axis: Axis, n: usize) -> bool {
        let node = &self.st.nodes[n];
        let principal = if axis == Axis::Attribute {
            Kind::Attribute
        } else {
            Kind::Element
        };
        match t {
            Test::Node => true,
            Test::Text => matches!(node.kind, Kind::Text | Kind::CData),
            Test::Comment => node.kind == Kind::Comment,
            Test::Pi(l) => node.kind == Kind::Pi && l.as_ref().is_none_or(|l| *l == node.name),
            Test::AnyName => node.kind == principal,
            Test::NsAny(u) => node.kind == principal && node.ns.as_deref() == Some(u.as_str()),
            Test::Name(u, l) => {
                node.kind == principal
                    && self.st.base_name(n) == *l
                    && match u {
                        Some(u) => node.ns.as_deref() == Some(u.as_str()),
                        None => node.ns.as_deref().is_none_or(str::is_empty),
                    }
            }
        }
    }

    fn eval(&self, e: &E, c: Pos) -> V {
        match e {
            E::Num(n) => V::Num(*n),
            E::Str(s) => V::Str(s.clone()),
            E::Neg(x) => V::Num(-self.to_num(&self.eval(x, c))),
            E::Bin(op, l, r) => self.binary(op, l, r, c),
            E::Call(n, args) => self.call(n, args, c),
            E::Filter(x, preds) => {
                let V::Nodes(mut ns) = self.eval(x, c) else {
                    return V::Nodes(Vec::new());
                };
                self.sort(&mut ns);
                for p in preds {
                    ns = self.apply_pred(ns, p);
                }
                V::Nodes(ns)
            }
            E::Path { start, abs, steps } => {
                let mut cur: Vec<usize> = match start {
                    Some(s) => match self.eval(s, c) {
                        V::Nodes(n) => n,
                        _ => Vec::new(),
                    },
                    None if *abs => vec![self.root_of(c.node)],
                    None => vec![c.node],
                };
                for s in steps {
                    let mut next = Vec::new();
                    for &n in &cur {
                        let mut cand: Vec<usize> = self
                            .axis_nodes(s.axis, n)
                            .into_iter()
                            .filter(|&m| self.test(&s.test, s.axis, m))
                            .collect();
                        for p in &s.preds {
                            cand = self.apply_pred(cand, p);
                        }
                        next.extend(cand);
                    }
                    self.sort(&mut next);
                    cur = next;
                }
                V::Nodes(cur)
            }
        }
    }

    fn apply_pred(&self, ns: Vec<usize>, p: &E) -> Vec<usize> {
        let size = ns.len();
        ns.into_iter()
            .enumerate()
            .filter(|&(i, n)| {
                let v = self.eval(
                    p,
                    Pos {
                        node: n,
                        pos: i + 1,
                        size,
                    },
                );
                match v {
                    V::Num(x) => x == (i + 1) as f64,
                    other => Self::to_bool(&other),
                }
            })
            .map(|(_, n)| n)
            .collect()
    }

    fn binary(&self, op: &str, l: &E, r: &E, c: Pos) -> V {
        match op {
            "or" => V::Bool(Self::to_bool(&self.eval(l, c)) || Self::to_bool(&self.eval(r, c))),
            "and" => V::Bool(Self::to_bool(&self.eval(l, c)) && Self::to_bool(&self.eval(r, c))),
            "|" => {
                let (V::Nodes(mut a), V::Nodes(b)) = (self.eval(l, c), self.eval(r, c)) else {
                    return V::Nodes(Vec::new());
                };
                a.extend(b);
                self.sort(&mut a);
                V::Nodes(a)
            }
            "+" | "-" | "mul" | "div" | "mod" => {
                let (a, b) = (self.to_num(&self.eval(l, c)), self.to_num(&self.eval(r, c)));
                V::Num(match op {
                    "+" => a + b,
                    "-" => a - b,
                    "mul" => a * b,
                    "div" => a / b,
                    _ => a % b,
                })
            }
            _ => V::Bool(self.compare(op, &self.eval(l, c), &self.eval(r, c))),
        }
    }

    fn cmp_atoms(&self, op: &str, a: &V, b: &V) -> bool {
        let rel = matches!(op, "<" | "<=" | ">" | ">=");
        if rel
            || (!matches!(a, V::Str(_)) && !matches!(b, V::Str(_))
                || matches!(a, V::Num(_))
                || matches!(b, V::Num(_)))
                && !matches!((a, b), (V::Bool(_), _) | (_, V::Bool(_)))
        {
            let (x, y) = (self.to_num(a), self.to_num(b));
            return match op {
                "=" => x == y,
                "!=" => x != y,
                "<" => x < y,
                "<=" => x <= y,
                ">" => x > y,
                _ => x >= y,
            };
        }
        if matches!(a, V::Bool(_)) || matches!(b, V::Bool(_)) {
            let (x, y) = (Self::to_bool(a), Self::to_bool(b));
            return if op == "=" { x == y } else { x != y };
        }
        let (x, y) = (self.to_str(a), self.to_str(b));
        if op == "=" {
            x == y
        } else {
            x != y
        }
    }

    fn compare(&self, op: &str, a: &V, b: &V) -> bool {
        match (a, b) {
            (V::Nodes(x), V::Nodes(y)) => x.iter().any(|&p| {
                y.iter().any(|&q| {
                    self.cmp_atoms(
                        op,
                        &V::Str(self.string_value(p)),
                        &V::Str(self.string_value(q)),
                    ) || (!matches!(op, "=" | "!=")
                        && self.cmp_atoms(
                            op,
                            &V::Num(str_num(&self.string_value(p))),
                            &V::Num(str_num(&self.string_value(q))),
                        ))
                })
            }),
            (V::Nodes(x), other) | (other, V::Nodes(x)) => {
                let flipped = matches!(b, V::Nodes(_));
                if let V::Bool(_) = other {
                    let nb = V::Bool(!x.is_empty());
                    return if flipped {
                        self.cmp_atoms(op, other, &nb)
                    } else {
                        self.cmp_atoms(op, &nb, other)
                    };
                }
                x.iter().any(|&n| {
                    let item = V::Str(self.string_value(n));
                    if flipped {
                        self.cmp_atoms(op, other, &item)
                    } else {
                        self.cmp_atoms(op, &item, other)
                    }
                })
            }
            _ => self.cmp_atoms(op, a, b),
        }
    }

    fn arg_str(&self, args: &[E], i: usize, c: Pos) -> String {
        match args.get(i) {
            Some(e) => self.to_str(&self.eval(e, c)),
            None => self.string_value(c.node),
        }
    }

    fn node_arg(&self, args: &[E], c: Pos) -> Option<usize> {
        match args.first() {
            None => Some(c.node),
            Some(e) => match self.eval(e, c) {
                V::Nodes(mut n) => {
                    self.sort(&mut n);
                    n.first().copied()
                }
                _ => None,
            },
        }
    }

    fn qname(&self, n: usize) -> String {
        let node = &self.st.nodes[n];
        match node.kind {
            Kind::Element | Kind::Attribute | Kind::Pi => node.name.clone(),
            _ => String::new(),
        }
    }

    fn call(&self, name: &str, args: &[E], c: Pos) -> V {
        let num = |i: usize| self.to_num(&self.eval(&args[i], c));
        match name {
            "last" => V::Num(c.size as f64),
            "position" => V::Num(c.pos as f64),
            "count" => match self.eval(&args[0], c) {
                V::Nodes(n) => V::Num(n.len() as f64),
                _ => V::Num(0.0),
            },
            "id" => V::Nodes(Vec::new()),
            "local-name" => V::Str(
                self.node_arg(args, c)
                    .map_or(String::new(), |n| self.st.base_name(n)),
            ),
            "namespace-uri" => V::Str(
                self.node_arg(args, c)
                    .and_then(|n| self.st.nodes[n].ns.clone())
                    .unwrap_or_default(),
            ),
            "name" => V::Str(
                self.node_arg(args, c)
                    .map_or(String::new(), |n| self.qname(n)),
            ),
            "string" => V::Str(self.arg_str(args, 0, c)),
            "concat" => V::Str((0..args.len()).map(|i| self.arg_str(args, i, c)).collect()),
            "starts-with" => V::Bool(
                self.arg_str(args, 0, c)
                    .starts_with(&self.arg_str(args, 1, c)),
            ),
            "contains" => V::Bool(self.arg_str(args, 0, c).contains(&self.arg_str(args, 1, c))),
            "substring-before" => {
                let (s, t) = (self.arg_str(args, 0, c), self.arg_str(args, 1, c));
                V::Str(s.find(&t).map_or(String::new(), |i| s[..i].to_string()))
            }
            "substring-after" => {
                let (s, t) = (self.arg_str(args, 0, c), self.arg_str(args, 1, c));
                V::Str(
                    s.find(&t)
                        .map_or(String::new(), |i| s[i + t.len()..].to_string()),
                )
            }
            "substring" => {
                let s: Vec<char> = self.arg_str(args, 0, c).chars().collect();
                let start = xround(num(1));
                let end = if args.len() == 3 {
                    start + xround(num(2))
                } else {
                    f64::INFINITY
                };
                V::Str(
                    s.iter()
                        .enumerate()
                        .filter(|&(i, _)| {
                            let p = (i + 1) as f64;
                            p >= start && p < end
                        })
                        .map(|(_, ch)| *ch)
                        .collect(),
                )
            }
            "string-length" => V::Num(self.arg_str(args, 0, c).chars().count() as f64),
            "normalize-space" => V::Str(
                self.arg_str(args, 0, c)
                    .split(is_ws)
                    .filter(|p| !p.is_empty())
                    .collect::<Vec<_>>()
                    .join(" "),
            ),
            "translate" => {
                let s = self.arg_str(args, 0, c);
                let from: Vec<char> = self.arg_str(args, 1, c).chars().collect();
                let to: Vec<char> = self.arg_str(args, 2, c).chars().collect();
                V::Str(
                    s.chars()
                        .filter_map(|ch| match from.iter().position(|&f| f == ch) {
                            Some(i) => to.get(i).copied(),
                            None => Some(ch),
                        })
                        .collect(),
                )
            }
            "boolean" => V::Bool(Self::to_bool(&self.eval(&args[0], c))),
            "not" => V::Bool(!Self::to_bool(&self.eval(&args[0], c))),
            "true" => V::Bool(true),
            "false" => V::Bool(false),
            "lang" => V::Bool(false),
            "number" => V::Num(match args.first() {
                Some(e) => self.to_num(&self.eval(e, c)),
                None => str_num(&self.string_value(c.node)),
            }),
            "sum" => match self.eval(&args[0], c) {
                V::Nodes(n) => V::Num(n.iter().map(|&x| str_num(&self.string_value(x))).sum()),
                _ => V::Num(f64::NAN),
            },
            "floor" => V::Num(num(0).floor()),
            "ceiling" => V::Num(num(0).ceil()),
            _ => V::Num(xround(num(0))),
        }
    }
}

fn xround(x: f64) -> f64 {
    if x.is_nan() || x.is_infinite() {
        x
    } else {
        (x + 0.5).floor()
    }
}

pub(crate) fn select(
    st: &Store,
    ctx: usize,
    expr: &str,
    ns: &[(String, String)],
) -> Result<Vec<usize>, String> {
    let src: Vec<char> = expr.chars().collect();
    let toks = lex(&src);
    let mut p = Parser {
        src: &src,
        toks,
        p: 0,
        lead: None,
        ns,
    };
    let e = p.parse_top()?;
    let cx = Cx { st };
    match cx.eval(
        &e,
        Pos {
            node: ctx,
            pos: 1,
            size: 1,
        },
    ) {
        V::Nodes(n) => Ok(n),
        _ => Ok(Vec::new()),
    }
}
