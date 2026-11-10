use std::collections::BTreeSet;
use xlcalc::{col_to_letters, letters_to_col};

pub const ALIAS: &str = "_xlcalc_wb_";

fn is_tok(c: char) -> bool {
    c.is_ascii_alphanumeric() || c == '_' || c == '.' || c == '$' || c == '\\'
}

fn skip_quoted(chars: &[char], mut i: usize, out: &mut String) -> usize {
    let q = chars[i];
    out.push(q);
    i += 1;
    while i < chars.len() {
        out.push(chars[i]);
        if chars[i] == q {
            if chars.get(i + 1) == Some(&q) {
                out.push(q);
                i += 2;
                continue;
            }
            return i + 1;
        }
        i += 1;
    }
    i
}

fn skip_brackets(chars: &[char], mut i: usize, out: &mut String) -> usize {
    let mut depth = 0;
    while i < chars.len() {
        let c = chars[i];
        out.push(c);
        i += 1;
        match c {
            '\'' if i < chars.len() => {
                out.push(chars[i]);
                i += 1;
            }
            '[' => depth += 1,
            ']' => {
                depth -= 1;
                if depth == 0 {
                    return i;
                }
            }
            _ => {}
        }
    }
    i
}

#[derive(PartialEq)]
enum Kind {
    Cell,
    Col,
    Row,
    Other,
}

fn split_dollar(s: &str) -> (bool, &str) {
    match s.strip_prefix('$') {
        Some(r) => (true, r),
        None => (false, s),
    }
}

fn classify(tok: &str) -> Kind {
    let (_, t) = split_dollar(tok);
    let letters = t.chars().take_while(|c| c.is_ascii_alphabetic()).count();
    let rest = &t[letters..];
    let (_, digits) = split_dollar(rest);
    let col_ok =
        (1..=3).contains(&letters) && letters_to_col(&t[..letters]).is_some_and(|c| c < 16384);
    if col_ok && rest.is_empty() {
        Kind::Col
    } else if col_ok && !digits.is_empty() && digits.chars().all(|c| c.is_ascii_digit()) {
        Kind::Cell
    } else if letters == 0 && !t.is_empty() && t.chars().all(|c| c.is_ascii_digit()) {
        Kind::Row
    } else {
        Kind::Other
    }
}

fn shift_col(tok: &str, dc: i64) -> String {
    let (abs, t) = split_dollar(tok);
    if abs {
        return tok.to_string();
    }
    let c = i64::from(letters_to_col(t).unwrap_or(0));
    col_to_letters((c + dc).max(0) as u32)
}

fn shift_row(tok: &str, dr: i64) -> String {
    let (abs, t) = split_dollar(tok);
    if abs {
        return tok.to_string();
    }
    let r: i64 = t.parse().unwrap_or(1);
    (r + dr).max(1).to_string()
}

fn shift_token(tok: &str, kind: &Kind, dr: i64, dc: i64) -> String {
    match kind {
        Kind::Col => shift_col(tok, dc),
        Kind::Row => shift_row(tok, dr),
        Kind::Cell => {
            let (abs_c, t) = split_dollar(tok);
            let letters = t.chars().take_while(|c| c.is_ascii_alphabetic()).count();
            let col = if abs_c {
                format!("${}", &t[..letters])
            } else {
                shift_col(&t[..letters], dc)
            };
            format!("{col}{}", shift_row(&t[letters..], dr))
        }
        Kind::Other => tok.to_string(),
    }
}

pub fn shift_formula(text: &str, dr: i64, dc: i64) -> String {
    let chars: Vec<char> = text.chars().collect();
    let mut out = String::with_capacity(text.len() + 8);
    let mut i = 0;
    while i < chars.len() {
        let c = chars[i];
        if c == '"' || c == '\'' {
            i = skip_quoted(&chars, i, &mut out);
        } else if c == '[' {
            i = skip_brackets(&chars, i, &mut out);
        } else if is_tok(c) {
            let start = i;
            while i < chars.len() && is_tok(chars[i]) {
                i += 1;
            }
            let tok: String = chars[start..i].iter().collect();
            let next = chars.get(i).copied();
            let prev = if start > 0 {
                Some(chars[start - 1])
            } else {
                None
            };
            let kind = classify(&tok);
            let is_ref = match kind {
                Kind::Cell => next != Some('(') && next != Some('!'),

                Kind::Col | Kind::Row => {
                    (next == Some(':') || prev == Some(':'))
                        && next != Some('!')
                        && next != Some('(')
                }
                Kind::Other => false,
            };
            if is_ref {
                out.push_str(&shift_token(&tok, &kind, dr, dc));
            } else {
                out.push_str(&tok);
            }
        } else {
            out.push(c);
            i += 1;
        }
    }
    out
}

fn starts_with_at(chars: &[char], i: usize, pat: &str) -> bool {
    pat.chars()
        .enumerate()
        .all(|(k, p)| chars.get(i + k) == Some(&p))
}

fn find_outside_strings(chars: &[char], pat: &str) -> Option<usize> {
    let mut i = 0;
    let mut sink = String::new();
    while i < chars.len() {
        if chars[i] == '"' {
            i = skip_quoted(chars, i, &mut sink);
        } else if starts_with_at(chars, i, pat) {
            return Some(i);
        } else {
            i += 1;
        }
    }
    None
}

fn matching_paren(chars: &[char], open: usize) -> Option<usize> {
    let mut depth = 0;
    let mut j = open;
    let mut sink = String::new();
    while j < chars.len() {
        match chars[j] {
            '"' => {
                j = skip_quoted(chars, j, &mut sink);
                continue;
            }
            '(' => depth += 1,
            ')' => {
                depth -= 1;
                if depth == 0 {
                    return Some(j);
                }
            }
            _ => {}
        }
        j += 1;
    }
    None
}

pub fn prepare(text: &str, aliases: &mut BTreeSet<String>) -> String {
    let chars: Vec<char> = text.chars().collect();
    let mut s = String::with_capacity(text.len());
    let mut i = 0;
    while i < chars.len() {
        if chars[i] == '"' || chars[i] == '\'' {
            i = skip_quoted(&chars, i, &mut s);
        } else if starts_with_at(&chars, i, "_xlpm.") {
            i += 6;
        } else if starts_with_at(&chars, i, "[0]!") {
            let name: String = chars[i + 4..]
                .iter()
                .take_while(|c| c.is_alphanumeric() || **c == '_' || **c == '.')
                .collect();
            if name.is_empty() || name.starts_with(|c: char| c.is_ascii_digit()) {
                s.push_str("Book1!");
                i += 4;
            } else {
                s.push_str(&format!("{ALIAS}{name}"));
                aliases.insert(name.clone());
                i += 4 + name.chars().count();
            }
        } else {
            s.push(chars[i]);
            i += 1;
        }
    }
    for (name, pre, post) in [("_xlfn.ANCHORARRAY(", "", "#"), ("_xlfn.SINGLE(", "@", "")] {
        loop {
            let chars: Vec<char> = s.chars().collect();
            let Some(p) = find_outside_strings(&chars, name) else {
                break;
            };
            let open = p + name.chars().count() - 1;
            let Some(close) = matching_paren(&chars, open) else {
                break;
            };
            let head: String = chars[..p].iter().collect();
            let inner: String = chars[open + 1..close].iter().collect();
            let tail: String = chars[close + 1..].iter().collect();
            s = format!("{head}{pre}{inner}{post}{tail}");
        }
    }
    s
}

pub const UDF: &str = "_xludf.";

pub const BARE_TABLE: &str = "_xlcalc_bare_";

fn map_tokens(
    text: &str,
    f: impl Fn(&str, Option<char>, Option<char>) -> Option<String>,
) -> String {
    let chars: Vec<char> = text.chars().collect();
    let mut out = String::with_capacity(text.len() + 8);
    let mut i = 0;
    while i < chars.len() {
        let c = chars[i];
        if c == '"' || c == '\'' {
            i = skip_quoted(&chars, i, &mut out);
        } else if c == '[' {
            i = skip_brackets(&chars, i, &mut out);
        } else if is_tok(c) {
            let start = i;
            while i < chars.len() && is_tok(chars[i]) {
                i += 1;
            }
            let tok: String = chars[start..i].iter().collect();
            let prev = start.checked_sub(1).map(|p| chars[p]);
            match f(&tok, prev, chars.get(i).copied()) {
                Some(r) => out.push_str(&r),
                None => out.push_str(&tok),
            }
        } else {
            out.push(c);
            i += 1;
        }
    }
    out
}

pub fn file_meaning(text: &str, tables: &[String]) -> String {
    map_tokens(text, |tok, prev, next| {
        if prev == Some('!') || tok.starts_with(|c: char| c.is_ascii_digit()) {
            return None;
        }
        let prefixed = tok.len() > 3 && tok[..3].eq_ignore_ascii_case("_xl");
        if next == Some('(') {
            return (!prefixed && !xlcalc::file_prefix(tok).is_empty())
                .then(|| format!("{UDF}{tok}"));
        }
        (next != Some('[')
            && next != Some('!')
            && tables.iter().any(|t| t.eq_ignore_ascii_case(tok)))
        .then(|| format!("{BARE_TABLE}{tok}"))
    })
}

pub fn unescape_xstring(s: &str) -> String {
    if !s.contains("_x") {
        return s.to_string();
    }
    let chars: Vec<char> = s.chars().collect();
    let mut out = String::new();
    let mut i = 0;
    while i < chars.len() {
        if chars[i] == '_' && i + 6 < chars.len() && chars[i + 1] == 'x' && chars[i + 6] == '_' {
            let hex: String = chars[i + 2..i + 6].iter().collect();
            if let Some(ch) = u32::from_str_radix(&hex, 16).ok().and_then(char::from_u32) {
                out.push(ch);
                i += 7;
                continue;
            }
        }
        out.push(chars[i]);
        i += 1;
    }
    out
}

pub fn ext_index(text: &str, keys: &[String]) -> String {
    if keys.is_empty() || !text.contains('[') {
        return text.to_string();
    }
    let chars: Vec<char> = text.chars().collect();
    let mut s = String::with_capacity(text.len() + 16);
    let mut i = 0;
    let mut in_str = false;
    while i < chars.len() {
        let c = chars[i];
        if c == '"' {
            in_str = !in_str;
        }
        if !in_str && c == '[' {
            let prev = i.checked_sub(1).map(|p| chars[p]);
            let digits: String = chars[i + 1..]
                .iter()
                .take_while(|c| c.is_ascii_digit())
                .collect();
            let close = i + 1 + digits.len();
            let ok_prev =
                !prev.is_some_and(|p| p.is_alphanumeric() || matches!(p, '_' | '.' | '['));
            if ok_prev && !digits.is_empty() && chars.get(close) == Some(&']') {
                if let Some(key) = digits
                    .parse::<usize>()
                    .ok()
                    .and_then(|n| n.checked_sub(1))
                    .and_then(|n| keys.get(n))
                {
                    if chars.get(close + 1) == Some(&'!') {
                        let plain = key
                            .chars()
                            .all(|c| c.is_alphanumeric() || matches!(c, '_' | '.'));
                        if plain {
                            s.push_str(key);
                        } else {
                            s.push('\x27');
                            s.push_str(&key.replace('\x27', "''"));
                            s.push('\x27');
                        }
                        i = close + 1;
                        continue;
                    }
                    s.push('[');
                    s.push_str(key);
                    s.push(']');
                    i = close + 1;
                    continue;
                }
            }
        }
        s.push(c);
        i += 1;
    }
    s
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn shifts() {
        assert_eq!(shift_formula("A1+$B2*C$3+$D$4", 1, 2), "C2+$B3*E$3+$D$4");
        assert_eq!(
            shift_formula("SUM(A1:B2,\"A1\")+LOG10(A1)", 2, 0),
            "SUM(A3:B4,\"A1\")+LOG10(A3)"
        );
        assert_eq!(
            shift_formula("Sheet1!A1+'My Sheet'!B2", 1, 1),
            "Sheet1!B2+'My Sheet'!C3"
        );
        assert_eq!(
            shift_formula("SUM(A:A)+SUM(1:1)+T[[#This Row],[a]]", 3, 1),
            "SUM(B:B)+SUM(4:4)+T[[#This Row],[a]]"
        );
    }

    #[test]
    fn prepares() {
        let mut al = BTreeSet::new();
        assert_eq!(prepare("LET(_xlpm.x,1,_xlpm.x+1)", &mut al), "LET(x,1,x+1)");
        assert_eq!(prepare("[0]!Rate+1", &mut al), "_xlcalc_wb_Rate+1");
        assert!(al.contains("Rate"));
        assert_eq!(prepare("SUM(_xlfn.ANCHORARRAY(A1))", &mut al), "SUM(A1#)");
        assert_eq!(prepare("_xlfn.SINGLE(A1:A3)+1", &mut al), "@A1:A3+1");
        assert_eq!(unescape_xstring("a_x000D_b"), "a\rb");
    }
}
