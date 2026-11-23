use crate::addr::{max_cols, max_rows};
use xlcalc::col_to_letters;

struct Part {
    n: u32,
    abs: bool,
}

#[derive(Clone, Copy, PartialEq)]
pub(crate) enum Style {
    A1,
    R1c1,
}

pub(crate) type AbsMode = Option<i32>;

fn ident(c: char) -> bool {
    c.is_alphanumeric() || c == '_' || c == '.' || c == '\\'
}

fn letters_to_col(s: &str) -> Option<u32> {
    if s.is_empty() || s.len() > 3 {
        return None;
    }
    let mut n = 0u32;
    for c in s.chars() {
        n = n * 26 + (c.to_ascii_uppercase() as u32 - 'A' as u32 + 1);
    }
    (n <= max_cols()).then_some(n)
}

fn take_part(b: &[char], i: &mut usize, letters: bool) -> Option<Part> {
    let mut j = *i;
    let abs = b.get(j) == Some(&'$');
    if abs {
        j += 1;
    }
    let start = j;
    while b.get(j).is_some_and(|c| {
        if letters {
            c.is_ascii_alphabetic()
        } else {
            c.is_ascii_digit()
        }
    }) {
        j += 1;
    }
    if j == start {
        return None;
    }
    let text: String = b[start..j].iter().collect();
    let n = if letters {
        letters_to_col(&text)?
    } else {
        let n: u32 = text.parse().ok()?;
        if n == 0 || n > max_rows() {
            return None;
        }
        n
    };
    *i = j;
    Some(Part { n, abs })
}

enum Tok {
    Cell(Part, Part),
    Cols(Part, Part),
    Rows(Part, Part),
}

fn parse_ref(b: &[char], at: usize) -> Option<(Tok, usize)> {
    let mut i = at;
    let after_ok = |i: usize| !b.get(i).is_some_and(|&c| ident(c) || c == '(' || c == '$');
    if let Some(col) = take_part(b, &mut i, true) {
        if let Some(row) = take_part(b, &mut i, false) {
            return after_ok(i).then_some((Tok::Cell(col, row), i));
        }
        if b.get(i) == Some(&':') {
            i += 1;
            let c2 = take_part(b, &mut i, true)?;
            return after_ok(i).then_some((Tok::Cols(col, c2), i));
        }
        return None;
    }
    let r1 = take_part(b, &mut i, false)?;
    if b.get(i) != Some(&':') {
        return None;
    }
    i += 1;
    let r2 = take_part(b, &mut i, false)?;
    after_ok(i).then_some((Tok::Rows(r1, r2), i))
}

struct Out {
    style: Style,
    mode: AbsMode,
    row: u32,
    col: u32,
}

impl Out {
    fn row_abs(&self, p: &Part) -> bool {
        match self.mode {
            None => p.abs,
            Some(m) => matches!(m, 1 | 2),
        }
    }
    fn col_abs(&self, p: &Part) -> bool {
        match self.mode {
            None => p.abs,
            Some(m) => matches!(m, 1 | 3),
        }
    }
    fn r_text(&self, p: &Part) -> String {
        match self.style {
            Style::A1 => format!("{}{}", if self.row_abs(p) { "$" } else { "" }, p.n),
            Style::R1c1 => self.axis('R', self.row_abs(p), p.n, self.row),
        }
    }
    fn c_text(&self, p: &Part) -> String {
        match self.style {
            Style::A1 => format!(
                "{}{}",
                if self.col_abs(p) { "$" } else { "" },
                col_to_letters(p.n - 1)
            ),
            Style::R1c1 => self.axis('C', self.col_abs(p), p.n, self.col),
        }
    }
    fn axis(&self, letter: char, abs: bool, n: u32, host: u32) -> String {
        if abs {
            return format!("{letter}{n}");
        }
        let off = i64::from(n) - i64::from(host);
        match off {
            0 => letter.to_string(),
            _ => format!("{letter}[{off}]"),
        }
    }
    fn tok(&self, t: &Tok) -> String {
        match (t, self.style) {
            (Tok::Cell(c, r), Style::A1) => format!("{}{}", self.c_text(c), self.r_text(r)),
            (Tok::Cell(c, r), Style::R1c1) => format!("{}{}", self.r_text(r), self.c_text(c)),
            (Tok::Cols(a, b), Style::A1) => format!("{}:{}", self.c_text(a), self.c_text(b)),
            (Tok::Rows(a, b), Style::A1) => format!("{}:{}", self.r_text(a), self.r_text(b)),
            (Tok::Cols(a, b), Style::R1c1) => {
                let (x, y) = (self.c_text(a), self.c_text(b));
                if x == y {
                    x
                } else {
                    format!("{x}:{y}")
                }
            }
            (Tok::Rows(a, b), Style::R1c1) => {
                let (x, y) = (self.r_text(a), self.r_text(b));
                if x == y {
                    x
                } else {
                    format!("{x}:{y}")
                }
            }
        }
    }
}

pub(crate) fn convert(
    formula: &str,
    from: Style,
    to: Style,
    mode: AbsMode,
    book: &str,
    row: u32,
    col: u32,
) -> Option<String> {
    let a1: String = match from {
        Style::A1 => formula.to_string(),
        Style::R1c1 => crate::range::r1c1_to_a1(formula, row - 1, col - 1)?,
    };
    let b: Vec<char> = a1.chars().collect();
    let out = Out {
        style: to,
        mode,
        row,
        col,
    };
    let mut s = String::new();
    let mut i = 0;
    while i < b.len() {
        let c = b[i];
        if c == '"' {
            let start = i;
            i += 1;
            while i < b.len() {
                if b[i] == '"' {
                    if b.get(i + 1) == Some(&'"') {
                        i += 2;
                        continue;
                    }
                    break;
                }
                i += 1;
            }
            i = (i + 1).min(b.len());
            s.extend(&b[start..i]);
            continue;
        }
        if c == '\'' || c == '[' {
            let close = if c == '[' { ']' } else { '\'' };
            let start = i;
            i += 1;
            while i < b.len() && b[i] != close {
                i += 1;
            }
            i = (i + 1).min(b.len());
            let quoted: String = b[start..i].iter().collect();
            if b.get(i) == Some(&'!') && c == '\'' && !quoted.contains('[') {
                s.push_str(&format!("'[{book}]{}", &quoted[1..]));
            } else {
                s.push_str(&quoted);
            }
            continue;
        }
        if c == '\u{1}' {
            let mut j = i + 1;
            while j < b.len() && (ident(b[j]) || b[j] == '$') {
                j += 1;
            }
            let word: String = b[i + 1..j].iter().collect();
            if to == Style::A1 {
                s.push_str(&format!("'{word}'"));
            } else {
                s.push_str(&word);
            }
            i = j;
            continue;
        }
        let prev_ident = i > 0 && (ident(b[i - 1]) || b[i - 1] == '$');
        if !prev_ident && (c.is_ascii_alphanumeric() || c == '$') {
            if let Some((t, j)) = parse_ref(&b, i) {
                s.push_str(&out.tok(&t));
                i = j;
                continue;
            }
        }
        if ident(c) && !prev_ident {
            let mut j = i;
            while j < b.len() && ident(b[j]) {
                j += 1;
            }
            let word: String = b[i..j].iter().collect();
            if b.get(j) == Some(&'!') {
                s.push_str(&format!("[{book}]{word}"));
            } else {
                if from == Style::A1
                    && (is_r1c1_word(&word)
                        || (b.get(j) == Some(&'[')
                            && matches!(word.as_str(), "R" | "r" | "C" | "c")))
                {
                    return None;
                }
                s.push_str(&word);
            }
            i = j;
            continue;
        }
        s.push(c);
        i += 1;
    }
    Some(s)
}

fn is_r1c1_word(w: &str) -> bool {
    let b: Vec<char> = w.chars().collect();
    if b.first().map(|c| c.to_ascii_uppercase()) != Some('R') {
        return false;
    }
    let mut i = 1;
    while b.get(i).is_some_and(char::is_ascii_digit) {
        i += 1;
    }
    if b.get(i).map(|c| c.to_ascii_uppercase()) != Some('C') {
        return false;
    }
    i += 1;
    while b.get(i).is_some_and(char::is_ascii_digit) {
        i += 1;
    }
    i == b.len()
}
