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
    c.is_alphabetic() || c == '_' || c == '\\' || c == '$' || c == '\u{3000}'
}

fn is_ident_char(c: char) -> bool {
    c.is_alphanumeric()
        || c == '_'
        || c == '.'
        || c == '\\'
        || c == '?'
        || c == '$'
        || c == '\u{3000}'
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
    i: usize,
    after_sheet: bool,
    _src: &'a str,
}

impl<'a> Lexer<'a> {
    pub fn new(src: &'a str) -> Self {
        Lexer {
            chars: src.chars().collect(),
            i: 0,
            after_sheet: false,
            _src: src,
        }
    }

    fn peek(&self, k: usize) -> Option<char> {
        self.chars.get(self.i + k).copied()
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
            } else if c.is_ascii_digit()
                || (c == '.' && self.peek(1).is_some_and(|d| d.is_ascii_digit()))
            {
                self.number()?
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
                self.ident()?
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
                        self.i += op.chars().count();
                        Tok::Op(op)
                    }
                    None => return Err(self.err(format!("unexpected character '{c}'"))),
                }
            };
            out.push(Token {
                tok,
                pos,
                space_before: space,
                ws,
            });
        }
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
            XlError::Null,
            XlError::Div0,
            XlError::Value,
            XlError::Ref,
            XlError::Name,
            XlError::Num,
            XlError::NA,
            XlError::GettingData,
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
            if self.peek(0) == Some('!') {
                self.i += 1;
                return Ok(Tok::Sheet(SheetSpec::Range(s, second)));
            }
            self.i = save;
        }
        if self.peek(0) != Some('!') {
            return Err(self.err("expected '!' after quoted sheet name"));
        }
        self.i += 1;
        let spec = if let Some(rest) = s.strip_prefix('[') {
            let (book, sheet) = rest
                .split_once(']')
                .ok_or_else(|| self.err("bad external reference"))?;
            SheetSpec::External {
                book: book.to_string(),
                sheet: Some(sheet.to_string()),
            }
        } else if let Some((a, b)) = s.split_once(':') {
            SheetSpec::Range(a.to_string(), b.to_string())
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
            if self.peek(0) == Some('!') {
                self.i += 1;
                return Ok(Tok::Sheet(SheetSpec::External {
                    book: inner,
                    sheet: (!name.is_empty()).then_some(name),
                }));
            }
            self.i = save;
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
        if super::starts_like_r1c1(&name) {
            return Err(self.err("not a valid name"));
        }
        if name.encode_utf16().count() > 255 {
            return Err(self.err("names are at most 255 characters"));
        }
        match self.peek(0) {
            Some('!') => {
                self.i += 1;
                Ok(Tok::Sheet(SheetSpec::Single(name)))
            }
            Some(':') if !looks_like_ref_part(&name) && !self.after_sheet => {
                let save = self.i;
                self.i += 1;
                if self.peek(0).is_some_and(is_ident_start) {
                    let second = self.read_ident();
                    if self.peek(0) == Some('!') {
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
