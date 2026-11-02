use crate::types::date_serial;
use crate::CompileError;
use xlcalc_variant::{change_type, Locale, VarType, Variant};

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum P {
    LParen,
    RParen,
    Comma,
    Dot,
    Bang,
    Colon,
    Semicolon,
    Eq,
    Ne,
    Lt,
    Gt,
    Le,
    Ge,
    Plus,
    Minus,
    Star,
    Slash,
    Backslash,
    Caret,
    Amp,

    Assign,
    Hash,
}

#[derive(Clone, Debug, PartialEq)]
pub enum Tok {
    Ident(String, Option<char>),
    Lit(Variant),
    P(P),

    Newline,
    Eof,
}

#[derive(Clone, Debug, PartialEq)]
pub struct Token {
    pub tok: Tok,

    pub line: u32,
    pub col: u32,

    pub space: bool,
}

const TYPE_CHARS: &str = "$%&!#@^";

struct Lexer<'a> {
    chars: Vec<char>,
    pos: usize,
    line: u32,
    col: u32,
    out: Vec<Token>,
    module: &'a str,
}

pub fn lex(module: &str, src: &str) -> Result<Vec<Token>, CompileError> {
    let mut lx = Lexer {
        chars: src.chars().collect(),
        pos: 0,
        line: 1,
        col: 1,
        out: Vec::new(),
        module,
    };
    lx.run()?;
    Ok(lx.out)
}

fn is_ident_char(c: char) -> bool {
    c.is_alphanumeric() || c == '_'
}

impl Lexer<'_> {
    fn peek(&self, k: usize) -> Option<char> {
        self.chars.get(self.pos + k).copied()
    }

    fn bump(&mut self) -> Option<char> {
        let c = self.chars.get(self.pos).copied()?;
        self.pos += 1;
        if c == '\n' {
            self.line += 1;
            self.col = 1;
        } else {
            self.col += 1;
        }
        Some(c)
    }

    fn err(&self, line: u32, col: u32, message: impl Into<String>) -> CompileError {
        CompileError {
            module: self.module.to_string(),
            line,
            col,
            message: message.into(),
        }
    }

    fn at_continuation(&self) -> bool {
        if self.peek(0) != Some('_') {
            return false;
        }
        let mut k = 1;
        loop {
            match self.peek(k) {
                Some(' ' | '\t') => k += 1,
                Some('\r' | '\n') | None => return true,
                _ => return false,
            }
        }
    }

    fn skip_continuation(&mut self) {
        while let Some(c) = self.bump() {
            if c == '\n' {
                break;
            }
        }
    }

    fn skip_comment(&mut self) {
        loop {
            match self.peek(0) {
                None | Some('\n') | Some('\r') => return,
                Some(c) => {
                    let prev_space = self.pos > 0 && matches!(self.chars[self.pos - 1], ' ' | '\t');
                    if c == '_' && prev_space && self.at_continuation() {
                        self.skip_continuation();
                    } else {
                        self.bump();
                    }
                }
            }
        }
    }

    fn push(&mut self, tok: Tok, line: u32, col: u32, space: bool) {
        self.out.push(Token {
            tok,
            line,
            col,
            space,
        });
    }

    fn at_statement_start(&self) -> bool {
        matches!(
            self.out.last().map(|t| &t.tok),
            None | Some(Tok::Newline) | Some(Tok::P(P::Colon))
        )
    }

    fn run(&mut self) -> Result<(), CompileError> {
        let mut space = false;
        while let Some(c) = self.peek(0) {
            let (line, col) = (self.line, self.col);
            match c {
                ' ' | '\t' => {
                    self.bump();
                    space = true;
                    continue;
                }
                '_' if space && self.at_continuation() => {
                    self.skip_continuation();
                    continue;
                }
                '\r' => {
                    self.bump();
                    continue;
                }
                '\n' => {
                    self.bump();
                    self.push(Tok::Newline, line, col, space);
                }
                '\'' => {
                    self.skip_comment();
                    continue;
                }
                '"' => {
                    let s = self.string(line, col)?;
                    self.push(Tok::Lit(Variant::Bstr(s)), line, col, space);
                }
                '0'..='9' => {
                    let v = self.number(line, col)?;
                    self.push(Tok::Lit(v), line, col, space);
                }
                '.' if self.peek(1).is_some_and(|d| d.is_ascii_digit())
                    && !self.after_operand() =>
                {
                    let v = self.number(line, col)?;
                    self.push(Tok::Lit(v), line, col, space);
                }
                '&' if matches!(self.peek(1), Some('H' | 'h' | 'O' | 'o')) => {
                    let v = self.radix(line, col)?;
                    self.push(Tok::Lit(v), line, col, space);
                }
                '#' => {
                    if let Some(v) = self.date_literal() {
                        self.push(Tok::Lit(v), line, col, space);
                    } else {
                        self.bump();
                        self.push(Tok::P(P::Hash), line, col, space);
                    }
                }
                '[' => {
                    return Err(self.err(line, col, "unsupported: bracketed names ([A1], [x y])"));
                }
                c if c.is_alphabetic() => {
                    let mut name = String::new();
                    while let Some(c) = self.peek(0).filter(|&c| is_ident_char(c)) {
                        name.push(c);
                        self.bump();
                    }

                    let suffix = match self.peek(0) {
                        Some(t)
                            if TYPE_CHARS.contains(t)
                                && !self.peek(1).is_some_and(is_ident_char) =>
                        {
                            self.bump();
                            Some(t)
                        }
                        _ => None,
                    };
                    if suffix.is_none()
                        && name.eq_ignore_ascii_case("rem")
                        && self.at_statement_start()
                    {
                        self.skip_comment();
                        continue;
                    }
                    self.push(Tok::Ident(name, suffix), line, col, space);
                }
                _ => {
                    let p = self.punct(line, col)?;
                    self.push(Tok::P(p), line, col, space);
                }
            }
            space = false;
        }
        let (line, col) = (self.line, self.col);
        if !matches!(self.out.last().map(|t| &t.tok), None | Some(Tok::Newline)) {
            self.push(Tok::Newline, line, col, false);
        }
        self.push(Tok::Eof, line, col, false);
        Ok(())
    }

    fn after_operand(&self) -> bool {
        matches!(
            self.out.last().map(|t| &t.tok),
            Some(Tok::Ident(..) | Tok::Lit(_) | Tok::P(P::RParen))
        )
    }

    fn punct(&mut self, line: u32, col: u32) -> Result<P, CompileError> {
        let c = self.bump().unwrap_or(' ');
        let next = self.peek(0);
        let two = |lx: &mut Self, p: P| {
            lx.bump();
            p
        };
        Ok(match c {
            '(' => P::LParen,
            ')' => P::RParen,
            ',' => P::Comma,
            '.' => P::Dot,
            '!' => P::Bang,
            ':' if next == Some('=') => two(self, P::Assign),
            ':' => P::Colon,
            ';' => P::Semicolon,
            '=' => P::Eq,
            '<' if next == Some('>') => two(self, P::Ne),
            '<' if next == Some('=') => two(self, P::Le),
            '<' => P::Lt,
            '>' if next == Some('=') => two(self, P::Ge),
            '>' => P::Gt,
            '+' => P::Plus,
            '-' => P::Minus,
            '*' => P::Star,
            '/' => P::Slash,
            '\\' => P::Backslash,
            '^' => P::Caret,
            '&' => P::Amp,
            _ => return Err(self.err(line, col, format!("unexpected character '{c}'"))),
        })
    }

    fn string(&mut self, line: u32, col: u32) -> Result<String, CompileError> {
        self.bump();
        let mut s = String::new();
        loop {
            match self.bump() {
                Some('"') if self.peek(0) == Some('"') => {
                    self.bump();
                    s.push('"');
                }
                Some('"') => return Ok(s),
                Some('\n') | None => return Err(self.err(line, col, "unterminated string")),
                Some(c) => s.push(c),
            }
        }
    }

    fn literal_suffix(&mut self) -> Option<char> {
        let c = self.peek(0)?;
        if TYPE_CHARS.contains(c) && c != '$' && !self.peek(1).is_some_and(is_ident_char) {
            self.bump();
            Some(c)
        } else {
            None
        }
    }

    fn number(&mut self, line: u32, col: u32) -> Result<Variant, CompileError> {
        let mut text = String::new();
        let mut float = false;
        while let Some(c) = self.peek(0).filter(char::is_ascii_digit) {
            text.push(c);
            self.bump();
        }
        if self.peek(0) == Some('.') && self.peek(1).is_some_and(|c| c.is_ascii_digit()) {
            float = true;
            text.push('.');
            self.bump();
            while let Some(c) = self.peek(0).filter(char::is_ascii_digit) {
                text.push(c);
                self.bump();
            }
        }
        if matches!(self.peek(0), Some('e' | 'E' | 'd' | 'D')) {
            let sign = matches!(self.peek(1), Some('+' | '-'));
            let digit_at = if sign { 2 } else { 1 };
            if self.peek(digit_at).is_some_and(|c| c.is_ascii_digit()) {
                float = true;
                self.bump();
                text.push('e');
                if sign {
                    text.push(self.bump().unwrap_or('+'));
                }
                while let Some(c) = self.peek(0).filter(char::is_ascii_digit) {
                    text.push(c);
                    self.bump();
                }
            }
        }
        let suffix = self.literal_suffix();
        let bad = |lx: &Self| lx.err(line, col, format!("invalid number literal {text}"));
        let overflow = |lx: &Self| lx.err(line, col, "Overflow");
        let f = || text.parse::<f64>().ok().filter(|x| x.is_finite());
        Ok(match suffix {
            Some('%') if !float => {
                let v = text.parse::<i16>().map_err(|_| overflow(self))?;
                Variant::I2(v)
            }
            Some('&') if !float => {
                let v = text.parse::<i32>().map_err(|_| overflow(self))?;
                Variant::I4(v)
            }
            Some('^') if !float => {
                let v = text.parse::<i64>().map_err(|_| overflow(self))?;
                Variant::I8(v)
            }
            Some('%' | '&' | '^') => return Err(bad(self)),
            Some('!') => {
                let v = text.parse::<f32>().ok().filter(|x| x.is_finite());
                Variant::R4(v.ok_or_else(|| overflow(self))?)
            }
            Some('#') => Variant::R8(f().ok_or_else(|| overflow(self))?),
            Some('@') => change_type(&Variant::Bstr(text.clone()), VarType::Cy, Locale::EN_US, 0)
                .map_err(|_| overflow(self))?,
            _ if float => Variant::R8(f().ok_or_else(|| overflow(self))?),
            _ => match text.parse::<i64>() {
                Ok(v) if v <= i64::from(i16::MAX) => Variant::I2(v as i16),
                Ok(v) if v <= i64::from(i32::MAX) => Variant::I4(v as i32),
                _ => Variant::R8(f().ok_or_else(|| overflow(self))?),
            },
        })
    }

    fn radix(&mut self, line: u32, col: u32) -> Result<Variant, CompileError> {
        self.bump();
        let base = match self.bump() {
            Some('H' | 'h') => 16,
            _ => 8,
        };
        let mut v: u64 = 0;
        let mut any = false;
        while let Some(d) = self.peek(0).and_then(|c| c.to_digit(base)) {
            v = v
                .checked_mul(u64::from(base))
                .and_then(|v| v.checked_add(u64::from(d)))
                .ok_or_else(|| self.err(line, col, "Overflow"))?;
            any = true;
            self.bump();
        }
        if !any {
            return Err(self.err(line, col, "invalid hexadecimal or octal literal"));
        }
        let suffix = self.literal_suffix();
        let overflow = || self.err(line, col, "Overflow");
        Ok(match suffix {
            Some('%') if v <= 0xffff => Variant::I2(v as u16 as i16),
            Some('&') if v <= 0xffff_ffff => Variant::I4(v as u32 as i32),
            Some('^') => Variant::I8(v as i64),
            Some(_) => return Err(overflow()),
            None if v <= 0xffff => Variant::I2(v as u16 as i16),
            None if v <= 0xffff_ffff => Variant::I4(v as u32 as i32),
            None => Variant::I8(v as i64),
        })
    }

    fn date_literal(&mut self) -> Option<Variant> {
        let mut k = 1;
        let mut text = String::new();
        loop {
            match self.peek(k)? {
                '#' => break,
                '\n' | '\r' => return None,
                c => text.push(c),
            }
            k += 1;
        }
        let v = parse_date(&text)?;
        for _ in 0..=k {
            self.bump();
        }
        Some(Variant::Date(v))
    }
}

fn parse_date(text: &str) -> Option<f64> {
    let text = text.trim();
    let (mut date, mut time) = (None, None);
    let mut rest = text;
    if let Some(i) = rest.find(['/', '-']) {
        let end = rest[i..].find(' ').map_or(rest.len(), |j| i + j);
        let head = &rest[..end];
        let parts: Vec<&str> = head.split(['/', '-']).collect();
        if parts.len() != 3 {
            return None;
        }
        let n: Vec<i64> = parts
            .iter()
            .map(|p| p.trim().parse().ok())
            .collect::<Option<_>>()?;
        let (y, m, d) = if parts[0].len() == 4 {
            (n[0], n[1], n[2])
        } else {
            (n[2], n[0], n[1])
        };
        let y = match (parts[if parts[0].len() == 4 { 0 } else { 2 }].len(), y) {
            (1 | 2, y) if y < 30 => 2000 + y,
            (1 | 2, y) => 1900 + y,
            (_, y) => y,
        };
        if !(1..=12).contains(&m) || !(1..=31).contains(&d) {
            return None;
        }
        date = Some(date_serial(y, m, d));
        rest = rest[end..].trim();
    }
    if !rest.is_empty() {
        let upper = rest.to_ascii_uppercase();
        let (clock, pm) = if let Some(c) = upper.strip_suffix("PM") {
            (c.trim().to_string(), Some(true))
        } else if let Some(c) = upper.strip_suffix("AM") {
            (c.trim().to_string(), Some(false))
        } else {
            (upper.clone(), None)
        };
        let parts: Vec<i64> = clock
            .split(':')
            .map(|p| p.trim().parse().ok())
            .collect::<Option<_>>()?;
        if parts.is_empty() || parts.len() > 3 || (parts.len() == 1 && pm.is_none()) {
            return None;
        }
        let mut h = parts[0];
        let mi = parts.get(1).copied().unwrap_or(0);
        let s = parts.get(2).copied().unwrap_or(0);
        match pm {
            Some(true) if h < 12 => h += 12,
            Some(false) if h == 12 => h = 0,
            _ => {}
        }
        if h > 23 || mi > 59 || s > 59 {
            return None;
        }
        time = Some((h * 3600 + mi * 60 + s) as f64 / 86400.0);
    }
    match (date, time) {
        (None, None) => None,
        (Some(d), None) => Some(d),
        (None, Some(t)) => Some(t),

        (Some(d), Some(t)) if d < 0.0 => Some(d - t),
        (Some(d), Some(t)) => Some(d + t),
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    fn toks(src: &str) -> Vec<Tok> {
        lex("M", src).unwrap().into_iter().map(|t| t.tok).collect()
    }

    fn id(s: &str) -> Tok {
        Tok::Ident(s.to_string(), None)
    }

    #[test]
    fn continuation_comments_and_separators() {
        assert_eq!(
            toks("a = 1 + _\n   2 ' note _\n still comment\nRem x\nb: c"),
            vec![
                id("a"),
                Tok::P(P::Eq),
                Tok::Lit(Variant::I2(1)),
                Tok::P(P::Plus),
                Tok::Lit(Variant::I2(2)),
                Tok::Newline,
                Tok::Newline,
                id("b"),
                Tok::P(P::Colon),
                id("c"),
                Tok::Newline,
                Tok::Eof
            ]
        );
    }

    #[test]
    fn literals() {
        let lit = |s: &str| match &toks(s)[0] {
            Tok::Lit(v) => v.clone(),
            t => panic!("{t:?}"),
        };
        assert_eq!(lit("32767"), Variant::I2(32767));
        assert_eq!(lit("32768"), Variant::I4(32768));
        assert_eq!(lit("3000000000"), Variant::R8(3e9));
        assert_eq!(lit("1.5"), Variant::R8(1.5));
        assert_eq!(lit("1E+308"), Variant::R8(1e308));
        assert_eq!(lit("2D3"), Variant::R8(2000.0));
        assert_eq!(lit("1!"), Variant::R4(1.0));
        assert_eq!(lit("1#"), Variant::R8(1.0));
        assert_eq!(lit("5&"), Variant::I4(5));
        assert_eq!(lit("5^"), Variant::I8(5));
        assert_eq!(lit("1.5@"), Variant::Cy(15000));
        assert_eq!(lit("&HFFFF"), Variant::I2(-1));
        assert_eq!(lit("&HFFFF&"), Variant::I4(65535));
        assert_eq!(lit("&H8000"), Variant::I2(-32768));
        assert_eq!(lit("&O17"), Variant::I2(15));
        assert_eq!(lit("\"a\"\"b\""), Variant::Bstr("a\"b".into()));
        assert_eq!(lit("#1/2/2020#"), Variant::Date(43832.0));
        assert_eq!(lit("#2020-01-02#"), Variant::Date(43832.0));
        assert_eq!(lit("#12/30/1899#"), Variant::Date(0.0));
        assert_eq!(lit("#6:00:00 PM#"), Variant::Date(0.75));
        assert_eq!(lit("#12/29/1899 6:00:00 AM#"), Variant::Date(-1.25));
        assert!(lex("M", "x = 40000%").is_err());
    }

    #[test]
    fn suffixes_and_positions() {
        let t = lex("M", "Dim s$\n  x! = y#").unwrap();
        assert_eq!(t[1].tok, Tok::Ident("s".into(), Some('$')));
        assert_eq!(t[3].tok, Tok::Ident("x".into(), Some('!')));
        assert_eq!((t[3].line, t[3].col), (2, 3));
        assert_eq!(t[5].tok, Tok::Ident("y".into(), Some('#')));

        assert_eq!(toks("a!b")[1], Tok::P(P::Bang));

        let t = lex("M", "Foo (x)\nFoo(x)").unwrap();
        assert!(t[1].space);
        assert!(!t[6].space);
    }
}
