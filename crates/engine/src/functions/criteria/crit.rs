use std::cmp::Ordering;

use crate::eval::EvalCtx;
use crate::number::cmp_numbers;
use crate::text::{compare_ci, eq_ci, utf16_len};
use crate::value::{Text, Value, XlError};

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum Op {
    Eq,
    Ne,
    Lt,
    Le,
    Gt,
    Ge,
}

impl Op {
    fn holds(self, o: Ordering) -> bool {
        match self {
            Op::Eq => o == Ordering::Equal,
            Op::Ne => o != Ordering::Equal,
            Op::Lt => o == Ordering::Less,
            Op::Le => o != Ordering::Greater,
            Op::Gt => o == Ordering::Greater,
            Op::Ge => o != Ordering::Less,
        }
    }
}

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum Flavor {
    Ifs,

    Database,
}

#[derive(Clone, Debug)]
enum Target {
    Num(f64),
    Bool(bool),
    Err(XlError),
    Text {
        text: Text,
        pattern: Option<Pattern>,
    },

    EmptyBare,

    Empty,
}

#[derive(Clone, Debug)]
pub struct Criterion {
    op: Op,
    target: Target,
}

const MAX_CRITERION_LEN: usize = 255;

impl Criterion {
    pub fn parse(ctx: &EvalCtx, v: &Value, flavor: Flavor) -> Result<Criterion, XlError> {
        let eq = |target| Ok(Criterion { op: Op::Eq, target });
        match v {
            Value::Number(n) => eq(Target::Num(*n)),
            Value::Blank => eq(Target::Num(0.0)),
            Value::Bool(b) => eq(Target::Bool(*b)),
            Value::Error(e) => eq(Target::Err(*e)),
            Value::Text(s) => Self::parse_text(ctx, s, flavor),
            Value::Array(_) => Err(XlError::VALUE),

            Value::Lambda(_) => Ok(Criterion {
                op: Op::Lt,
                target: Target::Empty,
            }),
        }
    }

    fn parse_text(ctx: &EvalCtx, s: &Text, flavor: Flavor) -> Result<Criterion, XlError> {
        if s.len() > MAX_CRITERION_LEN && utf16_len(s) > MAX_CRITERION_LEN {
            return Err(XlError::VALUE);
        }
        let (op, rest, bare) = if let Some(r) = s.strip_prefix("<=") {
            (Op::Le, r, false)
        } else if let Some(r) = s.strip_prefix(">=") {
            (Op::Ge, r, false)
        } else if let Some(r) = s.strip_prefix("<>") {
            (Op::Ne, r, false)
        } else if let Some(r) = s.strip_prefix('<') {
            (Op::Lt, r, false)
        } else if let Some(r) = s.strip_prefix('>') {
            (Op::Gt, r, false)
        } else if let Some(r) = s.strip_prefix('=') {
            (Op::Eq, r, false)
        } else {
            (Op::Eq, &s[..], true)
        };
        let prefix = bare && flavor == Flavor::Database;
        let target = if rest.is_empty() {
            if prefix {
                Target::Text {
                    text: Text::from(""),
                    pattern: Some(Pattern::new("*")),
                }
            } else if bare {
                Target::EmptyBare
            } else {
                Target::Empty
            }
        } else if let Some(n) = criterion_number(ctx, rest) {
            Target::Num(n)
        } else if rest.eq_ignore_ascii_case("TRUE") {
            Target::Bool(true)
        } else if rest.eq_ignore_ascii_case("FALSE") {
            Target::Bool(false)
        } else if let Some(e) = XlError::parse(rest).filter(|e| *e <= XlError::NA) {
            Target::Err(e)
        } else {
            let pattern = if prefix {
                let mut p = rest.to_string();
                p.push('*');
                Some(Pattern::new(&p))
            } else if matches!(op, Op::Eq | Op::Ne) && Pattern::has_wildcards(rest) {
                Some(Pattern::new(rest))
            } else {
                None
            };
            Target::Text {
                text: Text::from(rest),
                pattern,
            }
        };
        Ok(Criterion { op, target })
    }

    pub fn matches(&self, ctx: &EvalCtx, v: &Value) -> bool {
        match &self.target {
            Target::Num(x) => match self.op {
                Op::Eq => match v {
                    Value::Number(n) => cmp_numbers(*n, *x) == Ordering::Equal,
                    Value::Text(_) => ctx
                        .to_number(v)
                        .is_ok_and(|n| cmp_numbers(n, *x) == Ordering::Equal),
                    _ => false,
                },
                Op::Ne => !matches!(v, Value::Number(n) if cmp_numbers(*n, *x) == Ordering::Equal),
                op => matches!(v, Value::Number(n) if op.holds(cmp_numbers(*n, *x))),
            },
            Target::Bool(b) => match (self.op, v) {
                (Op::Ne, Value::Bool(c)) => c != b,
                (Op::Ne, _) => true,
                (op, Value::Bool(c)) => op.holds(c.cmp(b)),
                _ => false,
            },
            Target::Err(e) => match (self.op, v) {
                (Op::Ne, Value::Error(c)) => c != e,
                (Op::Ne, _) => true,
                (op, Value::Error(c)) => op.holds(c.cmp(e)),
                _ => false,
            },
            Target::Text { text, pattern } => {
                let equal = || match v {
                    Value::Text(t) => match pattern {
                        Some(p) => p.matches(t),
                        None => eq_ci(t, text),
                    },
                    _ => false,
                };
                match self.op {
                    Op::Eq => equal(),
                    Op::Ne => !equal(),
                    op => matches!(v, Value::Text(t) if op.holds(compare_ci(t, text))),
                }
            }
            Target::EmptyBare => match v {
                Value::Blank => true,
                Value::Text(t) => t.is_empty(),
                _ => false,
            },
            Target::Empty => match self.op {
                Op::Eq => v.is_blank(),
                Op::Ne => !v.is_blank(),
                _ => false,
            },
        }
    }
}

fn criterion_number(ctx: &EvalCtx, s: &str) -> Option<f64> {
    if s.starts_with('\u{a0}') || s.ends_with('\u{a0}') {
        return None;
    }
    ctx.to_number(&Value::Text(Text::from(s))).ok()
}

#[derive(Clone, Debug)]
pub struct Pattern {
    toks: Vec<Tok>,
}

#[derive(Clone, Copy, Debug, PartialEq)]
enum Tok {
    Lit(char),
    One,
    Any,
}

fn fold(c: char) -> char {
    if c.is_ascii() {
        return c.to_ascii_lowercase();
    }
    crate::functions::text::lower_char(c)
}

impl Pattern {
    pub fn has_wildcards(s: &str) -> bool {
        s.contains(['*', '?'])
    }

    pub fn new(pattern: &str) -> Pattern {
        let mut toks = Vec::new();
        let mut it = pattern.chars();
        while let Some(c) = it.next() {
            match c {
                '~' => {
                    if let Some(n) = it.next() {
                        toks.push(Tok::Lit(fold(n)));
                    }
                }
                '*' => {
                    if toks.last() != Some(&Tok::Any) {
                        toks.push(Tok::Any);
                    }
                }
                '?' => toks.push(Tok::One),
                c => toks.push(Tok::Lit(fold(c))),
            }
        }
        Pattern { toks }
    }

    pub fn matches(&self, text: &str) -> bool {
        let text: Vec<char> = text.chars().map(fold).collect();
        let (mut t, mut p) = (0usize, 0usize);
        let mut star: Option<(usize, usize)> = None;
        loop {
            if p == self.toks.len() {
                if t == text.len() {
                    return true;
                }
            } else {
                match self.toks[p] {
                    Tok::Any => {
                        star = Some((p, t));
                        p += 1;
                        continue;
                    }
                    Tok::One if t < text.len() => {
                        t += 1;
                        p += 1;
                        continue;
                    }
                    Tok::Lit(c) if t < text.len() && text[t] == c => {
                        t += 1;
                        p += 1;
                        continue;
                    }
                    _ => {}
                }
            }
            match star {
                Some((sp, st)) if st < text.len() => {
                    star = Some((sp, st + 1));
                    p = sp + 1;
                    t = st + 1;
                }
                _ => return false,
            }
        }
    }
}

pub fn truthy(v: &Value) -> bool {
    match v {
        Value::Bool(b) => *b,
        Value::Number(n) => *n != 0.0,
        Value::Text(s) => s.eq_ignore_ascii_case("TRUE"),
        Value::Array(a) => truthy(&a.data[0]),
        _ => false,
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    fn m(p: &str, t: &str) -> bool {
        Pattern::new(p).matches(t)
    }

    #[test]
    fn wildcards() {
        assert!(m("a*b", "a*b"));
        assert!(m("a*b", "aXYb"));
        assert!(m("~x*", "x"));
        assert!(!m("~x*", "~x"));
        assert!(m("~~x*", "~x"));
        assert!(m("*~", "anything"));
        assert!(m("a~*", "a*"));
        assert!(!m("a~*", "ab"));
        assert!(m("?~", "z"));
        assert!(m("APPLE*", "apple pie"));
        assert!(m("äöü*", "ÄÖÜ äöü"));
        assert!(m("*", ""));
        assert!(!m("?*", ""));
    }
}
