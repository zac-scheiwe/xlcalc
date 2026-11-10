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

    let letters = |t: &str| -> Vec<char> {
        t.chars()
            .flat_map(|c| match c {
                '\u{DF}' | '\u{1E9E}' if text => vec!['s', 's'],
                c => vec![fold(c)],
            })
            .collect()
    };
    let (s, p) = (letters(s), letters(pattern));
    like_at(&s, &p)
}

fn like_at(s: &[char], p: &[char]) -> Result<bool, VbaError> {
    let Some(&c) = p.first() else {
        return Ok(s.is_empty());
    };
    match c {
        '*' => {
            for i in 0..=s.len() {
                if like_at(&s[i..], &p[1..])? {
                    return Ok(true);
                }
            }
            Ok(false)
        }
        '?' => match s.split_first() {
            Some((_, rest)) => like_at(rest, &p[1..]),
            None => Ok(false),
        },
        '#' => match s.split_first() {
            Some((d, rest)) if d.is_ascii_digit() => like_at(rest, &p[1..]),
            _ => Ok(false),
        },
        '[' => {
            if p.get(1) == Some(&']') {
                return like_at(s, &p[2..]);
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
            like_at(rest, &p[end + 1..])
        }
        _ => match s.split_first() {
            Some((&x, rest)) if x == c => like_at(rest, &p[1..]),
            _ => Ok(false),
        },
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
}
