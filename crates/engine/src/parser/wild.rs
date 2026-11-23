use super::ast::SheetSpec;
use super::lexer::{is_ident_char_no_dollar, is_ident_start_no_dollar, Lexer, Tok};

pub struct WildSheet<'a> {
    pub name: &'a str,
    pub candidate: bool,
}

pub fn expand(src: &str, sheets: &[WildSheet]) -> Result<String, String> {
    let (eq, mut body) = match src.strip_prefix('=') {
        Some(b) => ("=", b.to_string()),
        None => ("", src.to_string()),
    };
    let exists = |n: &str| sheets.iter().any(|s| crate::text::eq_sheet_name(s.name, n));

    while let Ok(toks) = Lexer::new(&body, &exists).tokenize() {
        let wild: Vec<(usize, String)> = toks
            .iter()
            .filter_map(|t| match &t.tok {
                Tok::Sheet(SheetSpec::Single(n)) if is_wild(n) => Some((t.pos, n.clone())),
                Tok::Sheet(SheetSpec::Range(a, b)) if is_wild(a) || is_wild(b) => {
                    Some((t.pos, format!("{a}:{b}")))
                }

                Tok::Sheet(SheetSpec::External {
                    book,
                    sheet: Some(sheet),
                }) if (is_wild(book) || is_wild(sheet))
                    && !body.chars().skip(t.pos).take(2).any(|c| c == '[') =>
                {
                    Some((t.pos, format!("[{book}]{sheet}")))
                }
                _ => None,
            })
            .collect();
        if wild.iter().any(|(_, p)| p == "?") {
            return Err("a sheet pattern cannot be a lone ?".into());
        }
        let chars: Vec<char> = body.chars().collect();

        let found = wild.into_iter().find_map(|(start, pattern)| {
            let after = bang_end(&chars, start)?;
            let len = ref_len(&chars, after).or_else(|| name_len(&chars, after))?;
            Some((start, after, len, pattern))
        });
        let Some((start, after, len, pattern)) = found else {
            break;
        };
        let item: String = chars[after..after + len].iter().collect();
        let mut runs: Vec<(usize, usize)> = Vec::new();
        for (i, s) in sheets.iter().enumerate() {
            if !(s.candidate && crate::replace::text_matches(s.name, &pattern, true, false)) {
                continue;
            }
            match runs.last_mut() {
                Some((_, last)) if *last + 1 == i => *last = i,
                _ => runs.push((i, i)),
            }
        }
        let expanded: Vec<String> = runs
            .iter()
            .map(|&(a, b)| {
                let spec = if a == b {
                    SheetSpec::Single(sheets[a].name.to_string())
                } else {
                    SheetSpec::Range(sheets[a].name.to_string(), sheets[b].name.to_string())
                };
                format!("{}{item}", super::print::sheet_spec_text(&spec))
            })
            .collect();
        let head: String = chars[..start].iter().collect();
        let tail: String = chars[after + len..].iter().collect();
        if expanded.is_empty() && !operand_follows(&tail) {
            return Err("a sheet pattern that matches no sheet leaves an operand out".into());
        }
        body = format!("{head}{}{tail}", expanded.join(","));
    }
    Ok(format!("{eq}{body}"))
}

fn operand_follows(tail: &str) -> bool {
    let first = tail.chars().find(|c| !matches!(c, ' ' | '\n' | '\r'));
    !matches!(
        first,
        None | Some(',' | ';' | ')' | '}' | '*' | '/' | '^' | '&' | '=' | '<' | '>' | ':' | '%')
    )
}

fn is_wild(name: &str) -> bool {
    name.contains(['*', '?', '\0'])
}

fn bang_end(chars: &[char], start: usize) -> Option<usize> {
    let mut i = start;
    if chars.get(i) == Some(&'\'') {
        i += 1;
        loop {
            match chars.get(i)? {
                '\'' if chars.get(i + 1) == Some(&'\'') => i += 2,
                '\'' => {
                    i += 1;
                    break;
                }
                _ => i += 1,
            }
        }
        while chars.get(i) != Some(&'!') {
            i += 1;
            chars.get(i)?;
        }
    } else {
        while *chars.get(i)? != '!' {
            i += 1;
        }
    }
    Some(i + 1)
}

fn ref_len(chars: &[char], at: usize) -> Option<usize> {
    let part = |i: usize, kind: u8| -> Option<usize> {
        let mut j = i;
        let dollar = |j: &mut usize| {
            if chars.get(*j) == Some(&'$') {
                *j += 1;
            }
        };
        let mut letters = 0;
        let mut digits = 0;
        if kind != b'r' {
            dollar(&mut j);
            while chars.get(j).is_some_and(|c| c.is_ascii_alphabetic()) {
                j += 1;
                letters += 1;
            }
        }
        if kind != b'c' {
            dollar(&mut j);
            while chars.get(j).is_some_and(|c| c.is_ascii_digit()) {
                j += 1;
                digits += 1;
            }
        }
        let text: String = chars[i..j].iter().collect();
        let ok = match kind {
            b'a' => crate::address::parse_a1_cell(&text).is_some(),
            b'c' => {
                letters > 0
                    && crate::address::letters_to_col(text.trim_start_matches('$')).is_some()
            }
            _ => {
                digits > 0
                    && text
                        .trim_start_matches('$')
                        .parse::<u32>()
                        .is_ok_and(|r| (1..=crate::address::MAX_ROWS).contains(&r))
            }
        };
        ok.then_some(j)
    };

    let ends_word = |j: usize| !chars.get(j).is_some_and(|&c| is_ident_char_no_dollar(c));
    if let Some(j) = part(at, b'a') {
        if chars.get(j) == Some(&':') {
            if let Some(k) = part(j + 1, b'a').filter(|&k| ends_word(k)) {
                return Some(k - at);
            }
        }
        return ends_word(j).then_some(j - at);
    }
    for kind in *b"cr" {
        if let Some(j) = part(at, kind).filter(|&j| chars.get(j) == Some(&':')) {
            if let Some(k) = part(j + 1, kind).filter(|&k| ends_word(k)) {
                return Some(k - at);
            }
        }
    }
    None
}

fn name_len(chars: &[char], at: usize) -> Option<usize> {
    if !chars.get(at).is_some_and(|&c| is_ident_start_no_dollar(c)) {
        return None;
    }
    let n = 1 + chars[at + 1..]
        .iter()
        .take_while(|&&c| is_ident_char_no_dollar(c))
        .count();
    Some(n)
}

#[cfg(test)]
mod tests {
    use super::*;

    fn book() -> Vec<WildSheet<'static>> {
        ["T", "S", "Data", "Data2", "X y"]
            .into_iter()
            .map(|name| WildSheet {
                name,
                candidate: name != "T",
            })
            .collect()
    }

    #[test]
    fn expands_runs_of_matching_sheets() {
        let b = book();
        assert_eq!(expand("='*'!A1", &b).unwrap(), "='S:X y'!A1");
        assert_eq!(
            expand("=SUM('Data*'!A1:B2)", &b).unwrap(),
            "=SUM(Data:Data2!A1:B2)"
        );
        assert!(expand("='?'!A1+'S?'!$A$1", &b).is_err());
        assert_eq!(expand("='D?ta'!A1+'S'!A1", &b).unwrap(), "=Data!A1+'S'!A1");
        assert_eq!(expand("=D?ta!A1", &b).unwrap(), "=Data!A1");
        assert_eq!(
            expand("='D*'!A1.x+'D*'!A1#", &b).unwrap(),
            "=Data:Data2!A1.x+Data:Data2!A1#"
        );
        assert_eq!(expand("='*a*'!B:B", &b).unwrap(), "=Data:Data2!B:B");
        assert_eq!(expand("='S*'!Foo+1", &b).unwrap(), "=S!Foo+1");
    }

    #[test]
    fn no_match_removes_the_item() {
        let b = book();
        assert_eq!(expand("='a*b'!R[1]C", &b).unwrap(), "=[1]C");
        assert_eq!(expand("='S?'!B2+1", &b).unwrap(), "=+1");
        assert_eq!(expand("='a*b'!SUM(1)", &b).unwrap(), "=(1)");
    }

    #[test]
    fn no_match_needs_an_operand_after_it() {
        let b = book();
        for f in [
            "='a*b'!A1",
            "='a*b'!A1:",
            "=SUM('a*b'!B2,1)",
            "=SUM(1,'a*b'!B2)",
            "='a*b'!B2*3",
        ] {
            assert!(expand(f, &b).is_err(), "{f}");
        }
    }

    #[test]
    fn nothing_after_the_bang_keeps_the_prefix() {
        let b = book();
        assert_eq!(expand("='a*b'! C1", &b).unwrap(), "='a*b'! C1");
        assert_eq!(expand("='a?b'!#REF!", &b).unwrap(), "='a?b'!#REF!");
        assert_eq!(expand("='a*b'! C1+'S*'!A2", &b).unwrap(), "='a*b'! C1+S!A2");
        assert!(expand(r"='C:\d\[x.xlsx]*'!A1", &b).is_err());
        assert_eq!(expand("='[x.xlsx]*'!A1", &b).unwrap(), "='[x.xlsx]*'!A1");
    }
}
