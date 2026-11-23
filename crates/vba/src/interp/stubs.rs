use crate::VbaError;

pub fn like(s: &str, pattern: &str, text: bool) -> Result<bool, VbaError> {
    let fold = |c: char| {
        if text {
            match u16::try_from(u32::from(c)) {
                Ok(u) => char::from_u32(u32::from(crate::rt::map_unit(u, false))).unwrap_or(c),
                Err(_) => c,
            }
        } else {
            c
        }
    };
    let s: Vec<char> = s.chars().map(fold).collect();
    let p: Vec<char> = pattern.chars().map(fold).collect();
    like_at(&s, &p, text)
}

fn ligature(c: char) -> Option<[char; 2]> {
    match c {
        '\u{DF}' | '\u{1E9E}' => Some(['s', 's']),
        '\u{C6}' | '\u{E6}' => Some(['a', 'e']),
        '\u{152}' | '\u{153}' => Some(['o', 'e']),
        _ => None,
    }
}

fn like_at(s: &[char], p: &[char], text: bool) -> Result<bool, VbaError> {
    let Some(&c) = p.first() else {
        return Ok(s.is_empty());
    };
    match c {
        '*' => {
            for i in 0..=s.len() {
                if like_at(&s[i..], &p[1..], text)? {
                    return Ok(true);
                }
            }
            Ok(false)
        }
        '?' => match s.split_first() {
            Some((_, rest)) => like_at(rest, &p[1..], text),
            None => Ok(false),
        },
        '#' => match s.split_first() {
            Some((d, rest)) if d.is_ascii_digit() => like_at(rest, &p[1..], text),
            _ => Ok(false),
        },
        '[' => {
            if p.get(1) == Some(&']') {
                return like_at(s, &p[2..], text);
            }
            let Some((&ch, rest)) = s.split_first() else {
                return Ok(false);
            };
            let Some(end) = p
                .get(2..)
                .unwrap_or(&[])
                .iter()
                .position(|&x| x == ']')
                .map(|e| e + 2)
            else {
                let open = p.get(2..).unwrap_or(&[]);
                if p.get(1) == Some(&'!') && open.contains(&ch) {
                    return Ok(false);
                }
                return Err(VbaError::new(93));
            };
            let mut set = &p[1..end];
            let negate = set.first() == Some(&'!') && set.len() > 1;
            if negate {
                set = &set[1..];
            }
            let mut hit = false;
            let mut i = 0;
            while i < set.len() {
                if i + 2 < set.len() && set[i + 1] == '-' {
                    if set[i] > set[i + 2] {
                        return Err(VbaError::new(93));
                    }
                    hit |= set[i] <= ch && ch <= set[i + 2];
                    i += 3;
                } else {
                    hit |= set[i] == ch;
                    i += 1;
                }
            }
            if hit == negate {
                return Ok(false);
            }
            like_at(rest, &p[end + 1..], text)
        }
        _ => {
            if let Some((&x, rest)) = s.split_first() {
                if x == c && like_at(rest, &p[1..], text)? {
                    return Ok(true);
                }
            }
            if !text {
                return Ok(false);
            }

            if let Some(l) = ligature(c) {
                if s.starts_with(&l) && like_at(&s[2..], &p[1..], text)? {
                    return Ok(true);
                }
            }
            if let Some((&x, rest)) = s.split_first() {
                if let Some([a, b]) = ligature(x) {
                    if a == c {
                        let mut tail = vec![b];
                        tail.extend_from_slice(rest);
                        return like_at(&tail, &p[1..], text);
                    }
                }
            }
            Ok(false)
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn like_patterns() {
        assert_eq!(like("abc", "a*", false), Ok(true));
        assert_eq!(like("abc", "a?c", false), Ok(true));
        assert_eq!(like("a1", "a#", false), Ok(true));
        assert_eq!(like("b", "[a-c]", false), Ok(true));
        assert_eq!(like("d", "[a-c]", false), Ok(false));
        assert_eq!(like("d", "[!a-c]", false), Ok(true));
        assert_eq!(like("ABC", "abc", true), Ok(true));
        assert_eq!(like("ABC", "abc", false), Ok(false));
        assert_eq!(like("", "[]", false), Ok(true));
        assert_eq!(like("a", "[!]", false), Ok(false));
        assert!(like("a", "[a", false).is_err());
        assert!(like("a", "[c-a]", false).is_err());
    }

    #[test]
    fn like_ligatures() {
        assert_eq!(like("\u{152}", "oe", true), Ok(true));
        assert_eq!(like("\u{C6}", "?", true), Ok(true));
        assert_eq!(like("\u{C6}", "??", true), Ok(false));
        assert_eq!(like("ae", "\u{C6}", true), Ok(true));
        assert_eq!(like("\u{E6}", "a*", true), Ok(true));
        assert_eq!(like("\u{C6}", "ae", false), Ok(false));
    }
}
