use super::ast::SheetSpec;
use super::ParseError;
use crate::value::XlError;

#[derive(Clone, Debug, PartialEq)]
pub enum Tok {
    Number { value: f64, raw: String },
    Str(String),
    Error(XlError),

    Ident(String),

    Sheet(SheetSpec),
    StructRef { table: Option<String>, spec: String },

    Field(String),
    Op(&'static str),
    End,
}

#[derive(Clone, Debug)]
pub struct Token {
    pub tok: Tok,
    pub pos: usize,
    pub space_before: bool,

    pub ws: String,
}

const OPS: [&str; 20] = [
    "<>", "<=", ">=", "+", "-", "*", "/", "^", "&", "=", "<", ">", "%", ":", ",", ";", "(", ")",
    "{", "}",
];

fn is_ident_start(c: char) -> bool {
    if c.is_ascii() {
        c.is_ascii_alphabetic() || matches!(c, '_' | '\\' | '$')
    } else {
        super::namech::name_char(c, true)
    }
}

fn is_ident_char(c: char) -> bool {
    if c.is_ascii() {
        c.is_ascii_alphanumeric() || matches!(c, '_' | '.' | '\\' | '?' | '$')
    } else {
        super::namech::name_char(c, false)
    }
}

fn path_name_ok(s: &str) -> bool {
    let sep = |c: char| c == '\\' || c == '/';
    let body = s.strip_prefix("\\\\").unwrap_or(s);
    let cs: Vec<char> = body.chars().collect();
    !cs.iter()
        .enumerate()
        .any(|(i, &c)| sep(c) && cs.get(i + 1).is_none_or(|&n| sep(n)))
}

pub(super) fn is_ident_start_no_dollar(c: char) -> bool {
    c != '$' && is_ident_start(c)
}

pub(super) fn is_ident_char_no_dollar(c: char) -> bool {
    c != '$' && is_ident_char(c)
}

fn looks_like_ref_part(s: &str) -> bool {
    let bare = s.trim_start_matches('$');
    crate::address::parse_a1_cell(s).is_some()
        || (s.starts_with('$')
            && !bare.is_empty()
            && bare.len() <= 3
            && bare.bytes().all(|b| b.is_ascii_alphabetic())
            && crate::address::letters_to_col(bare).is_some())
        || (!bare.is_empty() && bare.bytes().all(|b| b.is_ascii_digit()))
}

pub struct Lexer<'a> {
    chars: Vec<char>,

    orig: Vec<char>,
    i: usize,
    after_sheet: bool,

    parens: Vec<bool>,

    after_colon: bool,

    sheet_exists: &'a dyn Fn(&str) -> bool,

    typed: bool,
}

const DIGIT_LIKE: [(u32, u32); 48] = [
    (0xB2, 0xB3),
    (0xB9, 0xB9),
    (0x7C0, 0x7C9),
    (0x9F4, 0x9F9),
    (0xB72, 0xB77),
    (0xBF0, 0xBF2),
    (0xC78, 0xC7E),
    (0xD58, 0xD5E),
    (0xD70, 0xD78),
    (0xDE6, 0xDEF),
    (0xF2A, 0xF33),
    (0x1090, 0x1099),
    (0x1369, 0x137C),
    (0x17F0, 0x17F9),
    (0x1946, 0x194F),
    (0x19D0, 0x19DA),
    (0x1A80, 0x1A89),
    (0x1A90, 0x1A99),
    (0x1B50, 0x1B59),
    (0x1BB0, 0x1BB9),
    (0x1C40, 0x1C49),
    (0x1C50, 0x1C59),
    (0x2070, 0x2070),
    (0x2075, 0x2079),
    (0x2080, 0x2080),
    (0x2085, 0x2089),
    (0x2150, 0x2152),
    (0x2155, 0x215A),
    (0x215F, 0x215F),
    (0x2189, 0x2189),
    (0x24EA, 0x24FF),
    (0x2776, 0x2793),
    (0x2CFD, 0x2CFD),
    (0x3192, 0x3195),
    (0x3248, 0x324F),
    (0x3251, 0x325F),
    (0x3280, 0x3289),
    (0x32B1, 0x32BF),
    (0xA620, 0xA629),
    (0xA6E6, 0xA6EF),
    (0xA830, 0xA835),
    (0xA8D0, 0xA8D9),
    (0xA900, 0xA909),
    (0xA9D0, 0xA9D9),
    (0xA9F0, 0xA9F9),
    (0xAA50, 0xAA59),
    (0xABF0, 0xABF9),
    (0xFF10, 0xFF19),
];

fn digit_like(c: char) -> bool {
    let u = c as u32;
    DIGIT_LIKE.iter().any(|&(a, b)| (a..=b).contains(&u))
}

fn ascii_digits(src: &str) -> Vec<char> {
    if src.is_ascii() {
        return src.chars().collect();
    }
    let mut quote: Option<char> = None;
    let mut depth = 0usize;
    src.chars()
        .map(|c| {
            match (quote, c) {
                (Some(q), _) if c == q => quote = None,
                (Some(_), _) => {}
                (None, '"' | '\'') => quote = Some(c),
                (None, '[') => depth += 1,
                (None, ']') => depth = depth.saturating_sub(1),
                _ => {
                    if depth == 0 {
                        if let Some(d) = crate::coerce::national_digit(c) {
                            return d;
                        }
                    }
                }
            }
            c
        })
        .collect()
}

impl<'a> Lexer<'a> {
    pub fn new(src: &str, sheet_exists: &'a dyn Fn(&str) -> bool) -> Self {
        Lexer {
            chars: ascii_digits(src),
            orig: src.chars().collect(),
            i: 0,
            after_sheet: false,
            parens: Vec::new(),
            after_colon: false,
            sheet_exists,
            typed: false,
        }
    }

    pub fn typed(mut self, typed: bool) -> Self {
        self.typed = typed;
        self
    }

    fn span_ok(&self, a: &str, b: &str, after: usize) -> bool {
        let digit = a.starts_with(|c: char| c.is_ascii_digit());
        (digit || ((self.sheet_exists)(a) && (self.sheet_exists)(b)))
            && !self.after_colon
            && self.parens.last() != Some(&false)
            && (self.ref_follows(after) || self.chars.get(after) == Some(&' '))
    }

    fn ref_follows(&self, at: usize) -> bool {
        let word: String = self.chars[at.min(self.chars.len())..]
            .iter()
            .take_while(|&&c| is_ident_char(c))
            .collect();
        let next = self.chars.get(at + word.chars().count()).copied();
        crate::address::parse_a1_cell(&word).is_some()
            || (next == Some(':')
                && (looks_like_ref_part(&word) || crate::address::letters_to_col(&word).is_some()))
    }

    fn peek(&self, k: usize) -> Option<char> {
        self.chars.get(self.i + k).copied()
    }

    fn cell_address_follows(&self, k: usize) -> bool {
        let mut j = self.i + k;
        let at = |j: usize| self.chars.get(j).copied();
        if at(j) == Some('$') {
            j += 1;
        }
        let letters = (0..).take_while(|d| at(j + d).is_some_and(|c| c.is_ascii_alphabetic()));
        let nl = letters.count();
        if !(1..=3).contains(&nl) {
            return false;
        }
        j += nl;
        if at(j) == Some('$') {
            j += 1;
        }
        let nd = (0..)
            .take_while(|d| at(j + d).is_some_and(|c| c.is_ascii_digit()))
            .count();
        nd > 0 && !at(j + nd).is_some_and(is_ident_char)
    }

    fn err(&self, msg: impl Into<String>) -> ParseError {
        ParseError {
            pos: self.i,
            message: msg.into(),
        }
    }

    pub fn tokenize(mut self) -> Result<Vec<Token>, ParseError> {
        if let Some(i) = self.chars.iter().position(|&c| c == '\0') {
            return Err(ParseError {
                pos: i,
                message: "formulas cannot contain NUL characters".into(),
            });
        }
        let mut out = Vec::new();
        loop {
            let mut space = false;
            let ws_start = self.i;
            while let Some(c) = self.peek(0) {
                if c == ' ' || c == '\n' {
                    space = true;
                    self.i += 1;
                } else {
                    break;
                }
            }
            let pos = self.i;
            let ws: String = self.chars[ws_start..self.i].iter().collect();
            let Some(c) = self.peek(0) else {
                out.push(Token {
                    tok: Tok::End,
                    pos,
                    space_before: space,
                    ws,
                });
                return Ok(out);
            };

            self.after_colon = out.len() >= 2
                && matches!(out[out.len() - 1].tok, Tok::Op(":"))
                && (matches!(&out[out.len() - 2].tok, Tok::Ident(w)
                    if crate::address::letters_to_col(w.trim_start_matches('$')).is_some()
                        && w.bytes().all(|b| b.is_ascii_alphabetic() || b == b'$'))
                    || (out.len() >= 3
                        && matches!(&out[out.len() - 2].tok, Tok::Number { raw, .. }
                            if raw.bytes().all(|b| b.is_ascii_digit()))
                        && matches!(out[out.len() - 3].tok, Tok::Sheet(_))));
            let after_operand = out.last().is_some_and(|t: &Token| {
                matches!(
                    t.tok,
                    Tok::Ident(_)
                        | Tok::Str(_)
                        | Tok::Error(_)
                        | Tok::StructRef { .. }
                        | Tok::Field(_)
                        | Tok::Op(")")
                        | Tok::Op("}")
                        | Tok::Op("#")
                )
            });
            let tok = if c == '.' && after_operand && self.field_ahead() {
                self.field()?
            } else if c == '"' {
                self.string()?
            } else if c == '#' {
                self.error_or_hash()
            } else if c == '\'' {
                self.quoted_sheet()?
            } else if c == '[' {
                self.bracket(None)?
            } else if let Some(t) = self.bare_sheet(&out)? {
                t
            } else if c.is_ascii_digit()
                || (c == '.' && self.peek(1).is_some_and(|d| d.is_ascii_digit()))
            {
                self.number()?
            } else if c == '!'
                && self
                    .peek(1)
                    .is_some_and(|d| is_ident_start(d) || d.is_ascii_digit())
            {
                self.i += 1;
                Tok::Sheet(SheetSpec::Single(String::new()))
            } else if c == '\u{1}' {
                self.i += 1;
                Tok::Ident(format!("\u{1}{}", self.read_ident()))
            } else if is_ident_start(c) {
                self.after_sheet = matches!(
                    out.last(),
                    Some(Token {
                        tok: Tok::Sheet(_),
                        ..
                    })
                );
                let start = self.i;
                let t = self.ident()?;
                match t {
                    Tok::Ident(s)
                        if self.orig.get(start..self.i) != Some(&self.chars[start..self.i])
                            && crate::address::parse_a1_cell(&s).is_none() =>
                    {
                        Tok::Ident(self.orig[start..self.i].iter().collect())
                    }
                    t => t,
                }
            } else if c == '@' {
                self.i += 1;
                Tok::Op("@")
            } else {
                let mut found = None;
                for op in OPS {
                    let n = op.chars().count();
                    if (0..n).all(|k| self.peek(k) == op.chars().nth(k)) {
                        found = Some(op);
                        break;
                    }
                }
                match found {
                    Some(op) => {
                        if matches!(op, "-" | "+")
                            && !after_operand
                            && !matches!(
                                out.last().map(|t| &t.tok),
                                Some(Tok::Number { .. } | Tok::Op("%"))
                            )
                            && self.peek(1).is_some_and(digit_like)
                        {
                            return Err(self.err("a sign cannot be followed by this digit"));
                        }
                        self.i += op.chars().count();
                        Tok::Op(op)
                    }
                    None => return Err(self.err(format!("unexpected character '{c}'"))),
                }
            };
            match tok {
                Tok::Op("(") => {
                    let call = !space
                        && matches!(
                            out.last(),
                            Some(Token {
                                tok: Tok::Ident(_),
                                ..
                            })
                        );
                    self.parens.push(call);
                }
                Tok::Op(")") => {
                    self.parens.pop();
                }
                _ => {}
            }
            out.push(Token {
                tok,
                pos,
                space_before: space,
                ws,
            });
        }
    }

    fn sheet_run(&self, k: usize) -> usize {
        let mut n = 0;
        while self.peek(k + n).is_some_and(super::print::bare_sheet_char) {
            n += 1;
        }
        n
    }

    fn bare_sheet(&mut self, out: &[Token]) -> Result<Option<Tok>, ParseError> {
        let n = self.sheet_run(0);
        if n == 0 || self.peek(0) == Some('.') {
            return Ok(None);
        }
        let digit_led = self.peek(0).is_some_and(|c| c.is_ascii_digit());
        let take = |s: &Self, k: usize, n: usize| -> String {
            s.chars[s.i + k..s.i + k + n].iter().collect()
        };
        let (first, second, end) = if self.peek(n) == Some('!') {
            (take(self, 0, n), None, n)
        } else if digit_led && self.peek(n) == Some(':') {
            let m = self.sheet_run(n + 1);
            if m == 0 || self.peek(n + 1 + m) != Some('!') {
                return Ok(None);
            }
            let (a, b) = (take(self, 0, n), take(self, n + 1, m));
            if !self.span_ok(&a, &b, self.i + n + m + 2) {
                return Ok(None);
            }
            (a, Some(b), n + 1 + m)
        } else {
            return Ok(None);
        };
        if digit_led {
            let operand = |t: &Token| {
                matches!(
                    t.tok,
                    Tok::Ident(_)
                        | Tok::Number { .. }
                        | Tok::Str(_)
                        | Tok::Error(_)
                        | Tok::StructRef { .. }
                        | Tok::Field(_)
                        | Tok::Op(")")
                        | Tok::Op("}")
                        | Tok::Op("#")
                        | Tok::Op("%")
                )
            };

            if let Some(last) = out.last().filter(|_| second.is_none()) {
                if matches!(last.tok, Tok::Op("-") | Tok::Op("+"))
                    && !(out.len() >= 2 && operand(&out[out.len() - 2]))
                {
                    return Err(self.err("a sign cannot precede a sheet name"));
                }
            }
        }
        for name in std::iter::once(&first).chain(second.iter()) {
            if !super::print::bare_sheet_name_ok(name) || name.encode_utf16().count() > 255 {
                return Err(self.err("not a valid sheet name"));
            }
        }
        self.i += end + 1;
        Ok(Some(Tok::Sheet(match second {
            Some(b) => SheetSpec::Range(first, b),
            None => SheetSpec::Single(first),
        })))
    }

    fn field_ahead(&self) -> bool {
        match self.peek(1) {
            Some('[') => true,
            Some(c) => c.is_alphabetic(),
            None => false,
        }
    }

    fn field(&mut self) -> Result<Tok, ParseError> {
        let start = self.i;
        self.i += 1;
        if self.peek(0) == Some('[') {
            self.i += 1;
            loop {
                match self.peek(0) {
                    None | Some('[') | Some('\'') => return Err(self.err("bad field name")),
                    Some(']') => {
                        self.i += 1;
                        break;
                    }
                    Some(_) => self.i += 1,
                }
            }
        } else {
            while self
                .peek(0)
                .is_some_and(|c| c.is_alphanumeric() || c == '_')
            {
                self.i += 1;
            }
            if self.peek(0).is_some_and(|c| is_ident_char(c) && c != '.') {
                return Err(self.err("bad field name"));
            }
        }
        Ok(Tok::Field(self.chars[start..self.i].iter().collect()))
    }

    fn string(&mut self) -> Result<Tok, ParseError> {
        self.i += 1;
        let mut s = String::new();
        loop {
            match self.peek(0) {
                None => return Err(self.err("unterminated string")),
                Some('"') if self.peek(1) == Some('"') => {
                    s.push('"');
                    self.i += 2;
                }
                Some('"') => {
                    self.i += 1;
                    return Ok(Tok::Str(s));
                }
                Some(c) => {
                    s.push(c);
                    self.i += 1;
                }
            }
        }
    }

    fn error_or_hash(&mut self) -> Tok {
        const LITERALS: [XlError; 8] = [
            XlError::NULL,
            XlError::DIV0,
            XlError::VALUE,
            XlError::REF,
            XlError::NAME,
            XlError::NUM,
            XlError::NA,
            XlError::GETTING_DATA,
        ];
        for e in LITERALS {
            let lit: Vec<char> = e.as_str().chars().collect();
            if (0..lit.len()).all(|k| {
                self.peek(k)
                    .is_some_and(|c| c.to_ascii_uppercase() == lit[k])
            }) {
                self.i += lit.len();
                return Tok::Error(e);
            }
        }
        self.i += 1;
        Tok::Op("#")
    }

    fn quoted_sheet(&mut self) -> Result<Tok, ParseError> {
        self.i += 1;
        let mut s = String::new();
        loop {
            match self.peek(0) {
                None => return Err(self.err("unterminated sheet name")),
                Some('\'') if self.peek(1) == Some('\'') => {
                    s.push('\'');
                    self.i += 2;
                }
                Some('\'') => {
                    self.i += 1;
                    break;
                }
                Some(c) => {
                    s.push(c);
                    self.i += 1;
                }
            }
        }

        if self.peek(0) == Some(':') && self.peek(1).is_some_and(is_ident_start) {
            let save = self.i;
            self.i += 1;
            let second = self.read_ident();
            if self.peek(0) == Some('!') && self.span_ok(&s, &second, self.i + 1) {
                self.i += 1;
                return Ok(Tok::Sheet(SheetSpec::Range(s, second)));
            }
            self.i = save;
        }
        if self.peek(0) == Some(':') {
            return Ok(Tok::Ident(format!("\u{1}{s}")));
        }
        if self.peek(0) != Some('!') {
            return Err(self.err("expected '!' after quoted sheet name"));
        }
        self.i += 1;

        if !s.contains('[') && s.contains(']') {
            return Err(self.err("not a valid sheet or file name"));
        }

        if s.ends_with(':') && !s.eq_ignore_ascii_case("c:") {
            return Err(self.err("not a valid file name"));
        }
        let spec = if let Some((dir, rest)) = s.split_once('[').filter(|(d, _)| !d.contains('!')) {
            let (book, sheet) = rest
                .split_once(']')
                .ok_or_else(|| self.err("bad external reference"))?;
            SheetSpec::External {
                book: format!("{dir}{book}"),
                sheet: Some(sheet.to_string()),
            }
        } else if s.contains(['\\', '/']) {
            if !path_name_ok(&s) {
                return Err(self.err("not a valid file name"));
            }

            SheetSpec::Single(s)
        } else if let Some((a, b)) = s.rsplit_once(':') {
            if self.parens.last() == Some(&false) {
                SheetSpec::Range(a.to_string(), format!("[{b}]{b}"))
            } else {
                SheetSpec::Range(a.to_string(), b.to_string())
            }
        } else {
            SheetSpec::Single(s)
        };
        Ok(Tok::Sheet(spec))
    }

    fn balanced_brackets(&mut self) -> Result<String, ParseError> {
        let mut depth = 0usize;
        let mut s = String::new();
        loop {
            let c = self.peek(0).ok_or_else(|| self.err("unterminated '['"))?;
            self.i += 1;
            match c {
                '\'' => {
                    s.push(c);
                    if let Some(n) = self.peek(0) {
                        s.push(n);
                        self.i += 1;
                    }
                    continue;
                }
                '[' => {
                    depth += 1;
                    if depth == 1 {
                        continue;
                    }
                }
                ']' => {
                    depth -= 1;
                    if depth == 0 {
                        return Ok(s);
                    }
                }
                _ => {}
            }
            s.push(c);
        }
    }

    fn bracket(&mut self, table: Option<String>) -> Result<Tok, ParseError> {
        let inner = self.balanced_brackets()?;
        if table.is_none() {
            let save = self.i;
            let mut name = String::new();
            while let Some(c) = self.peek(0) {
                if is_ident_char(c) {
                    name.push(c);
                    self.i += 1;
                } else {
                    break;
                }
            }
            if !name.is_empty()
                && self.peek(0) == Some(':')
                && self.peek(1).is_some_and(is_ident_start)
            {
                let at = self.i;
                self.i += 1;
                let second = self.read_ident();
                if self.peek(0) == Some('!') {
                    name = format!("{name}:{second}");
                } else {
                    self.i = at;
                }
            }
            if self.peek(0) == Some('!') {
                if name.is_empty()
                    && !inner.is_empty()
                    && inner.bytes().all(|b| b.is_ascii_digit())
                    && (self.typed || self.cell_address_follows(1))
                {
                    return Err(self.err("link index with '!' and no sheet"));
                }
                self.i += 1;
                return Ok(Tok::Sheet(SheetSpec::External {
                    book: inner,
                    sheet: (!name.is_empty()).then_some(name),
                }));
            }
            self.i = save;

            if !inner.is_empty()
                && inner.bytes().all(|b| b.is_ascii_digit())
                && self.peek(0).is_some_and(is_ident_start)
            {
                return Ok(Tok::Sheet(SheetSpec::External {
                    book: inner,
                    sheet: None,
                }));
            }
        }
        Ok(Tok::StructRef { table, spec: inner })
    }

    fn number(&mut self) -> Result<Tok, ParseError> {
        let start = self.i;
        while self.peek(0).is_some_and(|c| c.is_ascii_digit()) {
            self.i += 1;
        }
        if self.peek(0) == Some('.') {
            self.i += 1;
            while self.peek(0).is_some_and(|c| c.is_ascii_digit()) {
                self.i += 1;
            }
        }
        if matches!(self.peek(0), Some('e' | 'E')) {
            let k = if matches!(self.peek(1), Some('+' | '-')) {
                2
            } else {
                1
            };
            if self.peek(k).is_some_and(|c| c.is_ascii_digit()) {
                self.i += k;
                while self.peek(0).is_some_and(|c| c.is_ascii_digit()) {
                    self.i += 1;
                }
            }
        }
        let mut raw: String = self.chars[start..self.i].iter().collect();

        if !matches!(raw.chars().last(), Some('e' | 'E'))
            && !raw.contains(['e', 'E'])
            && self.peek(0) == Some('*')
            && self.peek(1) == Some('*')
        {
            let k = if matches!(self.peek(2), Some('+' | '-')) {
                3
            } else {
                2
            };
            if self.peek(k).is_some_and(|c| c.is_ascii_digit()) {
                let mut j = k;
                while self.peek(j).is_some_and(|c| c.is_ascii_digit()) {
                    j += 1;
                }
                if self
                    .peek(j)
                    .is_some_and(|c| c == '.' || c.is_alphabetic() || c == '_')
                {
                    return Err(self.err("malformed number"));
                }
                raw.push('E');
                raw.extend(self.chars[self.i + 2..self.i + j].iter());
                self.i += j;
            }
        }

        if self.peek(0).is_some_and(|c| c.is_alphabetic() || c == '_') {
            return Err(self.err("malformed number"));
        }
        let value = crate::number::parse_literal(&raw).ok_or_else(|| ParseError {
            pos: start,
            message: format!("invalid number '{raw}'"),
        })?;
        Ok(Tok::Number { value, raw })
    }

    fn read_ident(&mut self) -> String {
        let mut s = String::new();
        while let Some(c) = self.peek(0) {
            if is_ident_char(c) {
                s.push(c);
                self.i += 1;
            } else {
                break;
            }
        }
        s
    }

    fn ident(&mut self) -> Result<Tok, ParseError> {
        let mut name = self.read_ident();

        if name.len() > 1 && name.ends_with('.') && self.peek(0) == Some('[') {
            name.pop();
            self.i -= 1;
        }
        if super::r1c1_pair_with_tail(&name) {
            return Err(self.err("not a valid name"));
        }

        if name
            .strip_suffix('.')
            .is_some_and(|base| super::r1c1_axis(base).is_some())
        {
            return Err(self.err("not a valid name"));
        }
        if name.encode_utf16().count() > 255 {
            return Err(self.err("names are at most 255 characters"));
        }

        if self.peek(0) == Some('(') && super::starts_like_r1c1(&name) {
            return Err(self.err("not a valid function name"));
        }
        match self.peek(0) {
            Some('!') => {
                let path = name.contains('\\');
                if !(super::print::bare_sheet_name_ok(&name)
                    || (path && super::print::bare_sheet_name_ok(&name.replace('\\', "_"))))
                    || (path && !path_name_ok(&name))
                {
                    return Err(self.err("not a valid sheet name"));
                }
                self.i += 1;
                Ok(Tok::Sheet(SheetSpec::Single(name)))
            }
            Some(':') if !looks_like_ref_part(&name) && !self.after_sheet => {
                let save = self.i;
                self.i += 1;
                if self.peek(0).is_some_and(is_ident_start) {
                    let second = self.read_ident();
                    if self.peek(0) == Some('!') && self.span_ok(&name, &second, self.i + 1) {
                        self.i += 1;
                        return Ok(Tok::Sheet(SheetSpec::Range(name, second)));
                    }
                }
                self.i = save;
                Ok(Tok::Ident(name))
            }
            Some('[') => self.bracket(Some(name)),
            _ => Ok(Tok::Ident(name)),
        }
    }
}
