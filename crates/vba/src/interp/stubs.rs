pub fn like(s: &str, pattern: &str, text: bool) -> bool {
    let fold = |c: char| {
        if text {
            c.to_lowercase().next().unwrap_or(c)
        } else {
            c
        }
    };
    let s: Vec<char> = s.chars().map(fold).collect();
    let p: Vec<char> = pattern.chars().map(fold).collect();
    like_at(&s, &p)
}

fn like_at(s: &[char], p: &[char]) -> bool {
    let Some(&c) = p.first() else {
        return s.is_empty();
    };
    match c {
        '*' => (0..=s.len()).any(|i| like_at(&s[i..], &p[1..])),
        '?' => !s.is_empty() && like_at(&s[1..], &p[1..]),
        '#' => s.first().is_some_and(char::is_ascii_digit) && like_at(&s[1..], &p[1..]),
        '[' => {
            let Some(end) = p.iter().position(|&x| x == ']') else {
                return false;
            };
            let Some(&ch) = s.first() else {
                return false;
            };
            let mut set = &p[1..end];
            let negate = set.first() == Some(&'!');
            if negate {
                set = &set[1..];
            }
            let mut hit = false;
            let mut i = 0;
            while i < set.len() {
                if i + 2 < set.len() && set[i + 1] == '-' {
                    hit |= set[i] <= ch && ch <= set[i + 2];
                    i += 3;
                } else {
                    hit |= set[i] == ch;
                    i += 1;
                }
            }
            hit != negate && like_at(&s[1..], &p[end + 1..])
        }
        _ => s.first() == Some(&c) && like_at(&s[1..], &p[1..]),
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn like_patterns() {
        assert!(like("abc", "a*", false));
        assert!(like("abc", "a?c", false));
        assert!(like("a1", "a#", false));
        assert!(like("b", "[a-c]", false));
        assert!(!like("d", "[a-c]", false));
        assert!(like("d", "[!a-c]", false));
        assert!(like("ABC", "abc", true));
        assert!(!like("ABC", "abc", false));
    }
}
