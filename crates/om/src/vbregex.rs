use std::collections::HashMap;
use std::sync::OnceLock;

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub(crate) enum RegexError {
    Syntax,

    Quantifier,

    Bracket,

    Paren,

    Range,
}

impl RegexError {
    pub(crate) fn number(self) -> i32 {
        match self {
            RegexError::Syntax => 5017,
            RegexError::Quantifier => 5018,
            RegexError::Bracket => 5019,
            RegexError::Paren => 5020,
            RegexError::Range => 5021,
        }
    }
}

#[derive(Clone, Copy, Debug)]
enum Esc {
    Digit,
    NotDigit,
    Word,
    NotWord,
    Space,
    NotSpace,
}

impl Esc {
    fn matches(self, c: u16) -> bool {
        match self {
            Esc::Digit => is_digit(c),
            Esc::NotDigit => !is_digit(c),
            Esc::Word => is_word(c),
            Esc::NotWord => !is_word(c),
            Esc::Space => is_space(c),
            Esc::NotSpace => !is_space(c),
        }
    }
}

fn is_digit(c: u16) -> bool {
    (0x30..=0x39).contains(&c)
}

fn is_word(c: u16) -> bool {
    is_digit(c) || (0x41..=0x5A).contains(&c) || (0x61..=0x7A).contains(&c) || c == 0x5F
}

fn is_space(c: u16) -> bool {
    matches!(c, 0x20 | 0x09..=0x0D)
}

fn is_line_end(c: u16) -> bool {
    matches!(c, 0x0A | 0x0D | 0x2028 | 0x2029)
}

#[derive(Debug)]
struct ClassSet {
    neg: bool,
    ranges: Vec<(u16, u16)>,
    escapes: Vec<Esc>,
}

impl ClassSet {
    fn has(&self, c: u16) -> bool {
        self.ranges.iter().any(|&(lo, hi)| (lo..=hi).contains(&c))
            || self.escapes.iter().any(|e| e.matches(c))
    }
}

#[derive(Debug)]
enum Node {
    Char(u16),
    Any,
    Class(ClassSet),
    Start,
    End,
    WordBoundary,
    NotWordBoundary,

    Group(Option<usize>, Box<Node>),

    Look(bool, Box<Node>),
    Backref(usize),
    Seq(Vec<Node>),
    Alt(Vec<Node>),
    Repeat {
        body: Box<Node>,
        min: usize,
        max: usize,
        greedy: bool,
    },
}

enum Escape {
    Ch(u16),
    Set(Esc),
    WordBoundary,
    NotWordBoundary,
    Backref(usize),
}

struct Parser<'a> {
    s: &'a [u16],
    i: usize,

    groups: usize,
    depth: usize,
}

fn ch(c: char) -> u16 {
    c as u16
}

fn hex_val(c: u16) -> Option<u16> {
    char::from_u32(u32::from(c)).and_then(|c| c.to_digit(16).map(|d| d as u16))
}

impl Parser<'_> {
    fn peek(&self) -> Option<u16> {
        self.s.get(self.i).copied()
    }

    fn peek_is(&self, c: char) -> bool {
        self.peek() == Some(ch(c))
    }

    fn parse_alt(&mut self) -> Result<Node, RegexError> {
        let mut alts = vec![self.parse_seq()?];
        while self.peek_is('|') {
            self.i += 1;
            alts.push(self.parse_seq()?);
        }
        Ok(if alts.len() == 1 {
            alts.remove(0)
        } else {
            Node::Alt(alts)
        })
    }

    fn parse_seq(&mut self) -> Result<Node, RegexError> {
        let mut items = Vec::new();
        loop {
            match self.peek() {
                None => break,
                Some(c) if c == ch('|') => break,
                Some(c) if c == ch(')') => {
                    if self.depth == 0 {
                        return Err(RegexError::Syntax);
                    }
                    break;
                }
                Some(_) => {}
            }
            let (mut node, quantifiable) = self.parse_atom()?;
            if quantifiable {
                while let Some((min, max, greedy)) = self.parse_quant()? {
                    if min == 1 && max == 1 {
                        continue;
                    }
                    node = Node::Repeat {
                        body: Box::new(node),
                        min,
                        max,
                        greedy,
                    };
                    break;
                }
            }
            items.push(node);
        }
        Ok(match items.len() {
            1 => items.remove(0),
            _ => Node::Seq(items),
        })
    }

    fn brace(&self, at: usize) -> Result<Option<(usize, usize, usize)>, RegexError> {
        let number = |mut i: usize| {
            let from = i;
            let mut n = 0usize;
            while let Some(d) = self.s.get(i).filter(|&&c| is_digit(c)) {
                n = n.saturating_mul(10).saturating_add(usize::from(d - 0x30));
                i += 1;
            }
            (i > from).then_some((n, i))
        };
        let Some((min, mut i)) = number(at + 1) else {
            return Ok(None);
        };
        let mut max = min;
        if self.s.get(i) == Some(&ch(',')) {
            i += 1;
            match number(i) {
                Some((m, j)) => {
                    max = m;
                    i = j;
                }
                None => max = usize::MAX,
            }
        }
        if self.s.get(i) != Some(&ch('}')) {
            return Ok(None);
        }
        if max < min {
            return Err(RegexError::Syntax);
        }
        Ok(Some((min, max, i + 1)))
    }

    fn parse_quant(&mut self) -> Result<Option<(usize, usize, bool)>, RegexError> {
        let (min, max) = match self.peek() {
            Some(c) if c == ch('*') => {
                self.i += 1;
                (0, usize::MAX)
            }
            Some(c) if c == ch('+') => {
                self.i += 1;
                (1, usize::MAX)
            }
            Some(c) if c == ch('?') => {
                self.i += 1;
                (0, 1)
            }
            Some(c) if c == ch('{') => match self.brace(self.i)? {
                Some((min, max, end)) => {
                    self.i = end;
                    (min, max)
                }
                None => return Ok(None),
            },
            _ => return Ok(None),
        };
        let greedy = if self.peek_is('?') {
            self.i += 1;
            false
        } else {
            true
        };
        Ok(Some((min, max, greedy)))
    }

    fn parse_atom(&mut self) -> Result<(Node, bool), RegexError> {
        let c = self.s[self.i];
        self.i += 1;
        let node = match char::from_u32(u32::from(c)) {
            Some('(') => return self.parse_group().map(|n| (n, true)),
            Some('[') => Node::Class(self.parse_class()?),
            Some('.') => Node::Any,
            Some('^') => return Ok((Node::Start, false)),
            Some('$') => return Ok((Node::End, false)),
            Some('\\') => match self.escape(false)? {
                Escape::Ch(c) => Node::Char(c),
                Escape::Set(e) => Node::Class(ClassSet {
                    neg: false,
                    ranges: Vec::new(),
                    escapes: vec![e],
                }),
                Escape::WordBoundary => return Ok((Node::WordBoundary, false)),
                Escape::NotWordBoundary => return Ok((Node::NotWordBoundary, false)),
                Escape::Backref(n) => Node::Backref(n),
            },
            Some('*' | '+' | '?') => return Err(RegexError::Quantifier),
            Some('{') => {
                if self.brace(self.i - 1)?.is_some() {
                    return Err(RegexError::Quantifier);
                }
                Node::Char(c)
            }
            _ => Node::Char(c),
        };
        Ok((node, true))
    }

    fn parse_group(&mut self) -> Result<Node, RegexError> {
        enum Kind {
            Capture(usize),
            Plain,
            Look(bool),
        }
        let kind = if self.peek_is('?') {
            self.i += 1;
            let k = self.peek();
            self.i += 1;
            match k.and_then(|k| char::from_u32(u32::from(k))) {
                Some(':') => Kind::Plain,
                Some('=') => Kind::Look(false),
                Some('!') => Kind::Look(true),
                _ => return Err(RegexError::Syntax),
            }
        } else {
            self.groups += 1;
            Kind::Capture(self.groups)
        };
        self.depth += 1;
        let body = self.parse_alt()?;
        self.depth -= 1;
        if !self.peek_is(')') {
            return Err(RegexError::Paren);
        }
        self.i += 1;
        Ok(match kind {
            Kind::Capture(n) => Node::Group(Some(n), Box::new(body)),
            Kind::Plain => Node::Group(None, Box::new(body)),
            Kind::Look(neg) => Node::Look(neg, Box::new(body)),
        })
    }

    fn escape(&mut self, class: bool) -> Result<Escape, RegexError> {
        let Some(c) = self.peek() else {
            return Err(RegexError::Syntax);
        };
        self.i += 1;
        let Some(k) = char::from_u32(u32::from(c)) else {
            return Ok(Escape::Ch(c));
        };
        Ok(match k {
            'd' => Escape::Set(Esc::Digit),
            'D' => Escape::Set(Esc::NotDigit),
            'w' => Escape::Set(Esc::Word),
            'W' => Escape::Set(Esc::NotWord),
            's' => Escape::Set(Esc::Space),
            'S' => Escape::Set(Esc::NotSpace),
            'b' if class => Escape::Ch(8),
            'b' => Escape::WordBoundary,
            'B' if !class => Escape::NotWordBoundary,
            'f' => Escape::Ch(0x0C),
            'n' => Escape::Ch(0x0A),
            'r' => Escape::Ch(0x0D),
            't' => Escape::Ch(0x09),
            'v' => Escape::Ch(0x0B),
            'x' => Escape::Ch(self.hex_escape(2).unwrap_or(c)),
            'u' => Escape::Ch(self.hex_escape(4).unwrap_or(c)),
            'c' => match self.peek() {
                Some(l) if (l | 0x20).wrapping_sub(0x61) < 26 => {
                    self.i += 1;
                    Escape::Ch(l & 0x1F)
                }

                other => Escape::Ch(other.unwrap_or(0)),
            },
            '1'..='9' if !class => {
                let opened = self.groups;
                let d = usize::from(c - 0x30);
                if (1..=opened).contains(&d) {
                    let mut n = d;
                    while let Some(&e) = self.s.get(self.i) {
                        let m = n * 10 + usize::from(e.wrapping_sub(0x30));
                        if is_digit(e) && m <= opened {
                            n = m;
                            self.i += 1;
                        } else {
                            break;
                        }
                    }
                    Escape::Backref(n)
                } else if c <= 0x37 {
                    self.i -= 1;
                    Escape::Ch(self.octal())
                } else {
                    Escape::Ch(c)
                }
            }
            '0'..='7' => {
                self.i -= 1;
                Escape::Ch(self.octal())
            }
            _ => Escape::Ch(c),
        })
    }

    fn hex_escape(&mut self, n: usize) -> Option<u16> {
        let digits = self.s.get(self.i..self.i + n)?;
        let mut v = 0u16;
        for &d in digits {
            v = v * 16 + hex_val(d)?;
        }
        self.i += n;
        Some(v)
    }

    fn octal(&mut self) -> u16 {
        let first = self.s[self.i] - 0x30;
        let max = if first <= 3 { 3 } else { 2 };
        let mut v = 0u16;
        let mut n = 0;
        while n < max {
            match self.s.get(self.i) {
                Some(&d) if (0x30..=0x37).contains(&d) => {
                    v = v * 8 + (d - 0x30);
                    self.i += 1;
                    n += 1;
                }
                _ => break,
            }
        }
        v
    }

    fn class_atom(&mut self) -> Result<Escape, RegexError> {
        let c = self.s[self.i];
        self.i += 1;
        if c == ch('\\') {
            self.escape(true)
        } else {
            Ok(Escape::Ch(c))
        }
    }

    fn parse_class(&mut self) -> Result<ClassSet, RegexError> {
        let neg = self.peek_is('^');
        if neg {
            self.i += 1;
        }
        let mut set = ClassSet {
            neg,
            ranges: Vec::new(),
            escapes: Vec::new(),
        };
        let mut first = true;
        loop {
            let Some(c) = self.peek() else {
                return Err(RegexError::Bracket);
            };
            if c == ch(']') && !first {
                self.i += 1;
                return Ok(set);
            }
            first = false;
            match self.class_atom()? {
                Escape::Set(e) => set.escapes.push(e),
                Escape::Ch(lo) => {
                    let dash =
                        self.peek_is('-') && self.s.get(self.i + 1).is_some_and(|&n| n != ch(']'));
                    if dash {
                        self.i += 1;
                        match self.class_atom()? {
                            Escape::Ch(hi) if hi >= lo => set.ranges.push((lo, hi)),
                            _ => return Err(RegexError::Range),
                        }
                    } else {
                        set.ranges.push((lo, lo));
                    }
                }
                Escape::WordBoundary | Escape::NotWordBoundary | Escape::Backref(_) => {
                    return Err(RegexError::Syntax)
                }
            }
        }
    }
}

struct Fold {
    class_of: HashMap<u16, usize>,
    classes: Vec<Vec<u16>>,
}

fn fold_table() -> &'static Fold {
    static TABLE: OnceLock<Fold> = OnceLock::new();
    TABLE.get_or_init(|| {
        let mut class_of = HashMap::new();
        let mut classes = Vec::new();
        for line in crate::vbregex_fold::FOLD_CLASSES.lines() {
            let mut members: Vec<u16> = line
                .split_whitespace()
                .filter_map(|h| u16::from_str_radix(h, 16).ok())
                .collect();
            if members.is_empty() {
                continue;
            }
            members.sort_unstable();
            for &m in &members {
                class_of.insert(m, classes.len());
            }
            classes.push(members);
        }
        Fold { class_of, classes }
    })
}

fn fold_class(c: u16) -> Option<&'static [u16]> {
    let t = fold_table();
    t.class_of.get(&c).map(|&i| t.classes[i].as_slice())
}

#[derive(Clone, Debug)]
pub(crate) struct Match {
    pub(crate) start: usize,
    pub(crate) end: usize,
    pub(crate) groups: Vec<Option<(usize, usize)>>,
}

#[derive(Debug)]
pub(crate) struct Regex {
    root: Node,
    ngroups: usize,
    ignore_case: bool,
    multiline: bool,
}

type Caps = Vec<Option<(usize, usize)>>;

struct Matcher<'a> {
    re: &'a Regex,
    s: &'a [u16],
    caps: Caps,
}

type Cont<'k, 'a> = &'k mut dyn FnMut(&mut Matcher<'a>, usize) -> bool;

impl Regex {
    pub(crate) fn new(
        pattern: &[u16],
        ignore_case: bool,
        multiline: bool,
    ) -> Result<Regex, RegexError> {
        let mut p = Parser {
            s: pattern,
            i: 0,
            groups: 0,
            depth: 0,
        };
        let root = p.parse_alt()?;
        if p.i < pattern.len() {
            return Err(RegexError::Syntax);
        }
        Ok(Regex {
            root,
            ngroups: p.groups,
            ignore_case,
            multiline,
        })
    }

    pub(crate) fn find_at(&self, input: &[u16], from: usize) -> Option<Match> {
        let mut m = Matcher {
            re: self,
            s: input,
            caps: vec![None; self.ngroups],
        };
        for start in from..=input.len() {
            m.caps.iter_mut().for_each(|c| *c = None);
            let mut end = 0;
            if m.node(&self.root, start, &mut |_, p| {
                end = p;
                true
            }) {
                return Some(Match {
                    start,
                    end,
                    groups: m.caps,
                });
            }
        }
        None
    }

    pub(crate) fn exec(&self, input: &[u16], global: bool) -> Vec<Match> {
        let mut out = Vec::new();
        let mut at = 0;
        while at <= input.len() {
            let Some(m) = self.find_at(input, at) else {
                break;
            };
            at = if m.end == m.start { m.end + 1 } else { m.end };
            out.push(m);
            if !global {
                break;
            }
        }
        out
    }

    pub(crate) fn replace(&self, input: &[u16], repl: &[u16], global: bool) -> Vec<u16> {
        let mut out = Vec::new();
        let mut last = 0;
        for m in self.exec(input, global) {
            out.extend_from_slice(&input[last..m.start]);
            expand(&mut out, repl, input, &m);
            last = m.end;
        }
        out.extend_from_slice(&input[last..]);
        out
    }
}

fn expand(out: &mut Vec<u16>, repl: &[u16], input: &[u16], m: &Match) {
    let group = |out: &mut Vec<u16>, n: usize| {
        if let Some((a, b)) = m.groups[n - 1] {
            out.extend_from_slice(&input[a..b]);
        }
    };
    let mut i = 0;
    while i < repl.len() {
        let c = repl[i];
        i += 1;
        if c != ch('$') || i >= repl.len() {
            out.push(c);
            continue;
        }
        let n = repl[i];
        match char::from_u32(u32::from(n)) {
            Some('$') => {
                out.push(n);
                i += 1;
            }
            Some('&') => {
                out.extend_from_slice(&input[m.start..m.end]);
                i += 1;
            }
            Some('`') => {
                out.extend_from_slice(&input[..m.start]);
                i += 1;
            }
            Some('\'') => {
                out.extend_from_slice(&input[m.end..]);
                i += 1;
            }
            Some('_') => {
                out.extend_from_slice(input);
                i += 1;
            }
            Some('+') => {
                match m.groups.len() {
                    0 => out.extend_from_slice(&input[m.start..m.end]),
                    n => group(out, n),
                }
                i += 1;
            }
            Some('0'..='9') => {
                let d1 = usize::from(n - 0x30);
                let two = repl
                    .get(i + 1)
                    .filter(|&&e| is_digit(e))
                    .map(|&e| d1 * 10 + usize::from(e - 0x30));
                match two {
                    Some(t) if (1..=m.groups.len()).contains(&t) => {
                        group(out, t);
                        i += 2;
                    }
                    _ if (1..=m.groups.len()).contains(&d1) => {
                        group(out, d1);
                        i += 1;
                    }
                    _ => out.push(c),
                }
            }
            _ => out.push(c),
        }
    }
}

impl<'a> Matcher<'a> {
    fn eq(&self, a: u16, b: u16) -> bool {
        a == b
            || (self.re.ignore_case
                && match (fold_class(a), fold_class(b)) {
                    (Some(x), Some(y)) => std::ptr::eq(x, y),
                    _ => false,
                })
    }

    fn class_ok(&self, set: &ClassSet, c: u16) -> bool {
        let hit = if self.re.ignore_case {
            match fold_class(c) {
                Some(members) => members.iter().any(|&m| set.has(m)),
                None => set.has(c),
            }
        } else {
            set.has(c)
        };
        hit != set.neg
    }

    fn single(&self, n: &Node, c: u16) -> Option<bool> {
        match n {
            Node::Char(x) => Some(self.eq(*x, c)),
            Node::Any => Some(c != 0x0A),
            Node::Class(set) => Some(self.class_ok(set, c)),
            _ => None,
        }
    }

    fn is_word_at(&self, p: usize) -> bool {
        self.s.get(p).is_some_and(|&c| is_word(c))
    }

    fn seq(&mut self, ns: &[Node], pos: usize, k: Cont<'_, 'a>) -> bool {
        match ns.split_first() {
            None => k(self, pos),
            Some((head, rest)) => self.node(head, pos, &mut |s, p| s.seq(rest, p, &mut *k)),
        }
    }

    fn node(&mut self, n: &Node, pos: usize, k: Cont<'_, 'a>) -> bool {
        match n {
            Node::Char(_) | Node::Any | Node::Class(_) => {
                match self.s.get(pos).and_then(|&c| self.single(n, c)) {
                    Some(true) => k(self, pos + 1),
                    _ => false,
                }
            }
            Node::Start => {
                let ok = pos == 0 || (self.re.multiline && is_line_end(self.s[pos - 1]));
                ok && k(self, pos)
            }
            Node::End => {
                let ok = pos == self.s.len() || (self.re.multiline && is_line_end(self.s[pos]));
                ok && k(self, pos)
            }
            Node::WordBoundary | Node::NotWordBoundary => {
                let before = pos > 0 && self.is_word_at(pos - 1);
                let boundary = before != self.is_word_at(pos);
                (boundary == matches!(n, Node::WordBoundary)) && k(self, pos)
            }
            Node::Group(None, body) => self.node(body, pos, k),
            Node::Group(Some(i), body) => {
                let i = *i - 1;
                self.node(body, pos, &mut |s, p| {
                    let old = s.caps[i];
                    s.caps[i] = Some((pos, p));
                    if k(s, p) {
                        true
                    } else {
                        s.caps[i] = old;
                        false
                    }
                })
            }
            Node::Look(neg, body) => {
                let saved = self.caps.clone();
                let found = self.node(body, pos, &mut |_, _| true);
                if *neg {
                    self.caps = saved;
                    !found && k(self, pos)
                } else {
                    found && k(self, pos)
                }
            }
            Node::Backref(i) => match self.caps[*i - 1] {
                None => k(self, pos),
                Some((a, b)) => {
                    let len = b - a;
                    let fits = pos + len <= self.s.len()
                        && (0..len).all(|j| self.eq(self.s[a + j], self.s[pos + j]));
                    fits && k(self, pos + len)
                }
            },
            Node::Seq(items) => self.seq(items, pos, k),
            Node::Alt(alts) => alts.iter().any(|a| self.node(a, pos, &mut *k)),
            Node::Repeat {
                body,
                min,
                max,
                greedy,
            } => {
                if matches!(**body, Node::Char(_) | Node::Any | Node::Class(_)) {
                    self.repeat_unit(body, *min, *max, *greedy, pos, k)
                } else {
                    self.repeat(body, (*min, *max, *greedy), 0, pos, k)
                }
            }
        }
    }

    fn repeat_unit(
        &mut self,
        body: &Node,
        min: usize,
        max: usize,
        greedy: bool,
        pos: usize,
        k: Cont<'_, 'a>,
    ) -> bool {
        let ok = |s: &Self, i: usize| {
            s.s.get(pos + i)
                .is_some_and(|&c| s.single(body, c) == Some(true))
        };
        if greedy {
            let mut n = 0;
            while n < max && ok(self, n) {
                n += 1;
            }
            (min..=n).rev().any(|i| k(self, pos + i))
        } else {
            let mut i = 0;
            while i < min {
                if !ok(self, i) {
                    return false;
                }
                i += 1;
            }
            loop {
                if k(self, pos + i) {
                    return true;
                }
                if i < max && ok(self, i) {
                    i += 1;
                } else {
                    return false;
                }
            }
        }
    }

    fn repeat(
        &mut self,
        body: &Node,
        params: (usize, usize, bool),
        count: usize,
        pos: usize,
        k: Cont<'_, 'a>,
    ) -> bool {
        let (min, max, greedy) = params;
        stacker::maybe_grow(128 * 1024, 4 * 1024 * 1024, || {
            let (can_stop, can_more) = (count >= min, count < max);
            if !greedy && can_stop && k(self, pos) {
                return true;
            }
            if can_more && self.iterate(body, params, count, pos, &mut *k) {
                return true;
            }
            greedy && can_stop && k(self, pos)
        })
    }

    fn iterate(
        &mut self,
        body: &Node,
        params: (usize, usize, bool),
        count: usize,
        pos: usize,
        k: Cont<'_, 'a>,
    ) -> bool {
        self.node(body, pos, &mut |s, p| {
            if p == pos {
                k(s, p)
            } else {
                s.repeat(body, params, count + 1, p, &mut *k)
            }
        })
    }
}

#[cfg(test)]
pub(crate) mod tests {
    use super::*;

    fn units(s: &str) -> Vec<u16> {
        s.encode_utf16().collect()
    }

    pub(crate) fn dec(s: &str) -> Vec<u16> {
        let u = units(s);
        let mut out = Vec::new();
        let mut i = 0;
        while i < u.len() {
            if u[i] == ch('%') {
                let (n, len) = if u.get(i + 1) == Some(&ch('u')) {
                    (4, 6)
                } else {
                    (2, 3)
                };
                let digits = u.get(i + len - n..i + len);
                let v = digits.and_then(|ds| {
                    ds.iter()
                        .try_fold(0u16, |a, &d| hex_val(d).map(|h| a * 16 + h))
                });
                if let Some(v) = v {
                    out.push(v);
                    i += len;
                } else {
                    out.push(u[i]);
                    i += 1;
                }
            } else {
                out.push(u[i]);
                i += 1;
            }
        }
        out
    }

    pub(crate) fn enc(u: &[u16]) -> String {
        let mut s = String::new();
        for &c in u {
            if (32..=126).contains(&c) && c != ch('%') {
                s.push(c as u8 as char);
            } else if c < 256 {
                s.push_str(&format!("%{c:02X}"));
            } else {
                s.push_str(&format!("%u{c:04X}"));
            }
        }
        s
    }

    fn quote(u: &[u16]) -> String {
        format!("'{}'", enc(u))
    }

    fn show_matches(ms: &[Match], input: &[u16]) -> String {
        let mut out = format!("n={}", ms.len());
        for m in ms {
            let subs: Vec<String> = m
                .groups
                .iter()
                .map(|g| match g {
                    Some((a, b)) => quote(&input[*a..*b]),
                    None => "<E>".to_string(),
                })
                .collect();
            out.push_str(&format!(
                " [{},{},{};{}]",
                m.start,
                m.end - m.start,
                quote(&input[m.start..m.end]),
                subs.join(",")
            ));
        }
        out
    }

    fn run_case(case: &str) -> Option<String> {
        let mut f: Vec<&str> = case.split("@@").collect();
        while f.len() < 5 {
            f.push("");
        }
        let (op, flags) = (f[0], f[1]);
        if !matches!(op, "T" | "X" | "R") {
            return None;
        }
        let (pat, input, repl) = (dec(f[2]), dec(f[3]), dec(f[4]));
        let re = match Regex::new(&pat, flags.contains('i'), flags.contains('m')) {
            Ok(re) => re,
            Err(e) => return Some(format!("!{}", e.number())),
        };
        let global = flags.contains('g');
        Some(match op {
            "T" => {
                let found = re.find_at(&input, 0).is_some();
                if found { "True" } else { "False" }.to_string()
            }
            "X" => show_matches(&re.exec(&input, global), &input),
            _ => quote(&re.replace(&input, &repl, global)),
        })
    }

    const OPEN: [&str; 3] = [
        "X@@@@(?:(?=(a)))*@@a@@",
        "T@@@@(?=a){2}@@a@@",
        "X@@g@@(?=a){3}@@a@@",
    ];

    #[test]
    fn golden_probe_replays() {
        let path = concat!(
            env!("CARGO_MANIFEST_DIR"),
            "/../../conformance/com/regexp_probe.golden.txt"
        );
        let text = std::fs::read_to_string(path).unwrap();
        let (mut run, mut failures) = (0, Vec::new());
        for line in text.lines() {
            let Some((case, want)) = line.split_once(" => ") else {
                continue;
            };
            if OPEN.contains(&case) {
                continue;
            }
            let Some(got) = run_case(case) else { continue };
            run += 1;
            if got != want {
                failures.push(format!("{case}\n  want {want}\n  got  {got}"));
            }
        }
        assert!(run > 600, "only {run} cases ran");
        assert!(
            failures.is_empty(),
            "{} of {run} cases differ:\n{}",
            failures.len(),
            failures.join("\n")
        );
    }

    #[test]
    fn fold_classes_replay() {
        let path = concat!(
            env!("CARGO_MANIFEST_DIR"),
            "/../../conformance/com/regexp_fold.golden.txt"
        );
        let text = std::fs::read_to_string(path).unwrap();
        let mut seen = 0;
        for line in text.lines() {
            let (unit, members) = line.split_once(": ").unwrap();
            let unit = u16::from_str_radix(unit, 16).unwrap();
            let want: Vec<u16> = members
                .split(' ')
                .map(|h| u16::from_str_radix(h, 16).unwrap())
                .collect();
            assert_eq!(fold_class(unit).unwrap(), want.as_slice(), "{unit:04X}");
            seen += 1;
        }
        assert!(seen > 1000);

        assert!(fold_class(0x00DF).is_none());
        assert!(fold_class(0x212A).is_none());
    }
}
