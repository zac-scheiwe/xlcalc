use crate::address::{col_to_letters, MAX_COLS, MAX_ROWS};

#[derive(Clone, Copy, PartialEq)]
enum Part {
    Abs(u32),
    Rel(i64),
}

fn is_word(c: u8) -> bool {
    c.is_ascii_alphanumeric() || c == b'_' || c == b'.' || c == 92 || c == b'$' || c >= 0x80
}

fn part(s: &[u8], i: usize, l: u8) -> Option<(Option<Part>, bool, usize)> {
    if i >= s.len() || s[i].to_ascii_uppercase() != l {
        return None;
    }
    let mut j = i + 1;
    if j < s.len() && s[j] == b'[' {
        let mut k = j + 1;
        let mut neg = false;
        if k < s.len() && (s[k] == b'-' || s[k] == b'+') {
            neg = s[k] == b'-';
            k += 1;
        }
        let st = k;
        while k < s.len() && s[k].is_ascii_digit() {
            k += 1;
        }
        if k >= s.len() || s[k] != b']' {
            return None;
        }
        let n: i64 = if k == st {
            0
        } else {
            std::str::from_utf8(&s[st..k]).ok()?.parse().ok()?
        };
        j = k + 1;
        return Some((Some(Part::Rel(if neg { -n } else { n })), true, j));
    }
    let st = j;
    while j < s.len() && s[j].is_ascii_digit() {
        j += 1;
    }
    if j == st {
        return Some((Some(Part::Rel(0)), false, j));
    }
    let n: u64 = std::str::from_utf8(&s[st..j]).ok()?.parse().ok()?;
    if n == 0 {
        return None;
    }
    Some((Some(Part::Abs(n as u32)), false, j))
}

fn resolve(p: Part, host: u32, max: u32) -> Option<(u32, bool)> {
    match p {
        Part::Abs(n) => (n >= 1 && n <= max).then_some((n - 1, true)),
        Part::Rel(d) => {
            if d.unsigned_abs() >= max as u64 {
                return None;
            }
            Some(((host as i64 + d).rem_euclid(max as i64) as u32, false))
        }
    }
}

pub fn rewrite(src: &str, row: u32, col: u32) -> Result<String, String> {
    let lower = src.to_ascii_lowercase();
    if lower.contains("let(") || lower.contains("lambda(") {
        return Ok(src.to_string());
    }
    let (_, changed) = pass(src, row, col, false)?;
    if !changed {
        return Ok(src.to_string());
    }
    Ok(pass(src, row, col, true)?.0)
}

fn pass(src: &str, row: u32, col: u32, names: bool) -> Result<(String, bool), String> {
    let s = src.as_bytes();
    let mut out = String::with_capacity(src.len());
    let mut i = 0;
    let mut changed = false;
    while i < s.len() {
        let c = s[i];
        if c == b'"' || c == b'\'' || c == b'[' {
            let close = if c == b'[' { b']' } else { c };
            let mut j = i + 1;

            let mut depth = 1;
            while j < s.len() {
                if c == b'[' {
                    if s[j] == b'\'' {
                        j += 2;
                        continue;
                    }
                    if s[j] == b'[' {
                        depth += 1;
                    } else if s[j] == b']' {
                        depth -= 1;
                        if depth == 0 {
                            break;
                        }
                    }
                    j += 1;
                    continue;
                }
                if s[j] == close {
                    if j + 1 < s.len() && s[j + 1] == close {
                        j += 2;
                        continue;
                    }
                    break;
                }
                j += 1;
            }
            let end = (j + 1).min(s.len());
            out.push_str(&src[i..end]);
            i = end;
            continue;
        }
        let prev_word = i > 0 && is_word(s[i - 1]) && s[i - 1] != b'.';
        if (c == b'R' || c == b'r') && !prev_word && !(i > 0 && s[i - 1] == b'$') {
            if let Some((text, next)) = token(s, i, row, col)? {
                out.push_str(&text);
                i = next;
                changed = true;
                continue;
            }
        }
        if (c == b'C' || c == b'c') && !prev_word && !(i > 0 && s[i - 1] == b'$') {
            if let Some((text, next)) = line(s, i, col, MAX_COLS, b'C', |n| col_to_letters(n - 1))?
            {
                out.push_str(&text);
                i = next;
                changed = true;
                continue;
            }
        }
        if names && !prev_word && (c.is_ascii_alphabetic() || c == b'$') {
            let mut j = i;
            while j < s.len() && is_word(s[j]) {
                j += 1;
            }
            let w = &src[i..j];
            if w.contains('$') {
                return Err("$ references are invalid in R1C1 mode".into());
            }
            if crate::address::parse_a1_cell(w).is_some() && s.get(j) != Some(&b'(') {
                out.push('\u{1}');
            }
            out.push_str(w);
            i = j;
            continue;
        }

        let len = src[i..].chars().next().map_or(1, char::len_utf8);
        out.push_str(&src[i..i + len]);
        i += len;
    }
    Ok((if changed { out } else { src.to_string() }, changed))
}

fn token(s: &[u8], i: usize, row: u32, col: u32) -> Result<Option<(String, usize)>, String> {
    let Some((rp, rbr, j)) = part(s, i, b'R') else {
        return Ok(None);
    };
    let rp = rp.unwrap();
    let after = |j: usize| j >= s.len() || !(is_word(s[j]) && s[j] != b'.');
    let bang_follows = |j: usize| j < s.len() && s[j] == b'!';

    if let Some((cp, _, k)) = part(s, j, b'C') {
        let cp = cp.unwrap();

        if matches!(cp, Part::Abs(_)) && matches!(rp, Part::Rel(0)) && !rbr && s[j] != b'[' {
            return Ok(None);
        }

        if matches!(rp, Part::Rel(0))
            && matches!(cp, Part::Rel(0))
            && !rbr
            && s[j] != b'['
            && k == i + 2
            && letters_across_colon(s, i, k)
        {
            return Ok(None);
        }
        if after(k) && !bang_follows(k) {
            let (r, ra) = resolve(rp, row, MAX_ROWS).ok_or("R1C1 row out of range")?;
            let (c, ca) = resolve(cp, col, MAX_COLS).ok_or("R1C1 column out of range")?;
            let text = format!(
                "{}{}{}{}",
                if ca { "$" } else { "" },
                col_to_letters(c),
                if ra { "$" } else { "" },
                r + 1
            );
            return Ok(Some((text, k)));
        }
        return Ok(None);
    }

    line(s, i, row, MAX_ROWS, b'R', |n| n.to_string())
}

fn letters_across_colon(s: &[u8], i: usize, k: usize) -> bool {
    if s.get(k) == Some(&b':') {
        let mut j = k + 1;
        if j < s.len() && s[j] == b'$' {
            j += 1;
        }
        let st = j;
        while j < s.len() && s[j].is_ascii_alphabetic() {
            j += 1;
        }
        if j > st && (j >= s.len() || !is_word(s[j])) {
            return true;
        }
    }
    if i > 0 && s[i - 1] == b':' {
        let mut j = i - 1;
        let end = j;
        while j > 0 && s[j - 1].is_ascii_alphabetic() {
            j -= 1;
        }
        if j > 0 && s[j - 1] == b'$' {
            j -= 1;
        }
        let _ = end;
        if j < i - 1 && (j == 0 || !is_word(s[j - 1])) {
            return true;
        }
    }
    false
}

fn line(
    s: &[u8],
    i: usize,
    host: u32,
    max: u32,
    l: u8,
    name: impl Fn(u32) -> String,
) -> Result<Option<(String, usize)>, String> {
    let ok = |j: usize| (j >= s.len() || !is_word(s[j])) && !(j < s.len() && s[j] == b'!');
    let Some((Some(p), br, j)) = part(s, i, l) else {
        return Ok(None);
    };
    if matches!(p, Part::Abs(_)) || !ok(j) {
        return Ok(None);
    }
    let (a, _) = resolve(p, host, max).ok_or("R1C1 reference out of range")?;
    if j < s.len() && s[j] == b':' {
        if let Some((Some(q), br2, k)) = part(s, j + 1, l) {
            if !matches!(q, Part::Abs(_)) && ok(k) {
                if !br && !br2 {
                    return Ok(None);
                }
                let (b, _) = resolve(q, host, max).ok_or("R1C1 reference out of range")?;
                return Ok(Some((format!("{}:{}", name(a + 1), name(b + 1)), k)));
            }
        }
    }
    if i > 0 && s[i - 1] == b':' && letters_across_colon(s, i, i) {
        return Ok(None);
    }

    if !br && j < s.len() && s[j] == b':' {
        return Ok(None);
    }
    Ok(Some((format!("{0}:{0}", name(a + 1)), j)))
}
