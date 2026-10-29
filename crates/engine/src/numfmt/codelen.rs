fn split_sections(code: &str) -> Vec<Vec<char>> {
    let cs: Vec<char> = code.chars().collect();
    let mut out = vec![Vec::new()];
    let mut i = 0;
    while i < cs.len() {
        let c = cs[i];
        let take = match c {
            '"' => (i + 1..cs.len())
                .find(|&j| cs[j] == '"')
                .map_or(cs.len() - i, |j| j + 1 - i),
            '[' => (i + 1..cs.len())
                .find(|&j| cs[j] == ']')
                .map_or(cs.len() - i, |j| j + 1 - i),
            '\\' | '_' | '*' => 2.min(cs.len() - i),
            ';' => {
                out.push(Vec::new());
                i += 1;
                continue;
            }
            _ => 1,
        };
        out.last_mut().unwrap().extend_from_slice(&cs[i..i + take]);
        i += take;
    }
    out
}

pub(super) fn section_code_lens(code: &str) -> Vec<Option<u32>> {
    split_sections(code)
        .iter()
        .map(|s| section_len(s))
        .collect()
}

fn is_ph(c: char) -> bool {
    matches!(c, '0' | '#' | '?')
}

fn is_date(cs: &[char]) -> bool {
    let mut i = 0;
    while i < cs.len() {
        match cs[i] {
            '"' => {
                i = (i + 1..cs.len())
                    .find(|&j| cs[j] == '"')
                    .map_or(cs.len(), |j| j + 1);
                continue;
            }
            '[' => {
                let j = (i + 1..cs.len())
                    .find(|&j| cs[j] == ']')
                    .unwrap_or(cs.len());
                let inner: String = cs[i + 1..j].iter().collect::<String>().to_ascii_lowercase();
                if !inner.is_empty() && inner.chars().all(|c| matches!(c, 'h' | 'm' | 's')) {
                    return true;
                }
                i = j + 1;
                continue;
            }
            '\\' | '_' | '*' => {
                i += 2;
                continue;
            }
            c if "dmyhsDMYHS".contains(c) => return true,
            _ => {}
        }
        i += 1;
    }
    false
}

fn date_len(cs: &[char]) -> Option<u32> {
    let mut len = 0u32;
    let mut subsec = false;
    let mut i = 0;
    while i < cs.len() {
        let c = cs[i];
        let lc = c.to_ascii_lowercase();
        let starts = |p: &str| {
            let w: String = cs[i..(i + p.len()).min(cs.len())].iter().collect();
            w.eq_ignore_ascii_case(p)
        };
        match lc {
            '"' => {
                let j = (i + 1..cs.len()).find(|&j| cs[j] == '"')?;
                len += 2 + 2 * (j - i - 1) as u32;
                i = j + 1;
                continue;
            }
            '[' => {
                let j = (i + 1..cs.len()).find(|&j| cs[j] == ']')?;
                let inner: String = cs[i + 1..j].iter().collect::<String>().to_ascii_lowercase();
                if inner.starts_with(['<', '>', '=']) {
                    len += 10;
                } else if !inner.is_empty() && inner.chars().all(|c| matches!(c, 'h' | 'm' | 's'))
                    || matches!(
                        inner.as_str(),
                        "black"
                            | "blue"
                            | "cyan"
                            | "green"
                            | "magenta"
                            | "red"
                            | "white"
                            | "yellow"
                    )
                {
                    len += 2;
                } else {
                    return None;
                }
                i = j + 1;
                continue;
            }
            '\\' | '_' | '*' => {
                cs.get(i + 1)?;
                len += 3;
                i += 2;
                continue;
            }
            'a' if starts("am/pm") => {
                len += 11;
                i += 5;
                continue;
            }
            'a' if starts("a/p") => {
                len += 7;
                i += 3;
                continue;
            }
            'd' | 'm' | 'y' | 'h' | 's' => {
                while i < cs.len() && cs[i].to_ascii_lowercase() == lc {
                    i += 1;
                }
                len += 1;
                continue;
            }
            '.' if cs.get(i + 1) == Some(&'0') => {
                if subsec {
                    return None;
                }
                subsec = true;
                len += 2;
                i += 1;
                while i < cs.len() && cs[i] == '0' {
                    i += 1;
                }
                continue;
            }
            '/' | ':' | '$' => len += 1,
            ' ' | '-' | '.' | ',' | '+' | '(' | ')' => len += 3,
            _ => return None,
        }
        i += 1;
    }
    Some(len + if subsec { 2 } else { 0 })
}

fn section_len(cs: &[char]) -> Option<u32> {
    if is_date(cs) {
        return date_len(cs);
    }
    let mut len = 0u32;
    let mut header = 0u32;
    let mut number = false;
    let mut frac = false;
    let mut sci = false;
    let mut point = false;
    let mut grouping = false;

    let mut int_ph: Vec<char> = Vec::new();
    let mut int_open = true;

    let mut last_run: Option<char> = None;
    let mut i = 0;
    while i < cs.len() {
        let c = cs[i];
        match c {
            '"' => {
                let j = (i + 1..cs.len()).find(|&j| cs[j] == '"')?;
                len += 2 + 2 * (j - i - 1) as u32;
                last_run = None;
                int_open &= int_ph.is_empty();
                i = j + 1;
                continue;
            }
            '[' => {
                let j = (i + 1..cs.len()).find(|&j| cs[j] == ']')?;
                let inner: String = cs[i + 1..j].iter().collect::<String>().to_ascii_lowercase();
                if inner.starts_with(['<', '>', '=']) {
                    len += 10;
                } else if matches!(
                    inner.as_str(),
                    "black" | "blue" | "cyan" | "green" | "magenta" | "red" | "white" | "yellow"
                ) || inner
                    .strip_prefix("color")
                    .is_some_and(|n| n.parse::<u32>().is_ok())
                {
                    len += 2;
                } else {
                    return None;
                }
                i = j + 1;
                continue;
            }
            '\\' | '_' | '*' => {
                cs.get(i + 1)?;
                len += 3;
                last_run = None;
                int_open &= int_ph.is_empty();
                i += 2;
                continue;
            }
            '0' | '#' | '?' => {
                number = true;
                if int_open && !point && !sci && !frac {
                    int_ph.push(c);
                } else {
                    if last_run != Some(c) {
                        len += 2;
                    }
                    last_run = Some(c);
                }
            }
            '.' => {
                if frac {
                    return None;
                }
                number = true;
                point = true;
                int_open = false;
                len += 1;
                last_run = None;
            }
            ',' => {
                let prev_ph = i > 0 && (is_ph(cs[i - 1]) || cs[i - 1] == '.' || cs[i - 1] == ',');
                if !prev_ph {
                    len += 3;
                    last_run = None;
                    int_open &= int_ph.is_empty();
                } else {
                    let mut k = i;
                    while k < cs.len() && cs[k] == ',' {
                        k += 1;
                    }
                    if k < cs.len() && is_ph(cs[k]) && !point && !sci && !frac && int_open {
                        grouping = true;
                    } else if k < cs.len() && is_ph(cs[k]) {
                        return None;
                    }
                    i = k;
                    continue;
                }
            }
            '%' => {
                len += 3;
                last_run = None;
                int_open &= int_ph.is_empty();
            }
            'E' | 'e' => {
                if !matches!(cs.get(i + 1), Some('+' | '-')) || sci || frac {
                    return None;
                }
                sci = true;
                number = true;
                int_open = false;
                len += 5;
                last_run = None;
                i += 2;
                continue;
            }
            '/' => {
                if frac || sci || point {
                    return None;
                }
                frac = true;
                number = true;
                int_open = false;
                len += 1;
                last_run = None;

                let k = (i + 1..cs.len())
                    .find(|&k| !cs[k].is_ascii_digit())
                    .unwrap_or(cs.len());
                if cs[i + 1..k].iter().any(|d| ('1'..='9').contains(d)) {
                    len += 2 + 2 * (k - i - 1) as u32;
                    i = k;
                    continue;
                }
            }
            '1'..='9' => {
                number = true;
                len += 3;
                last_run = None;
                int_open &= int_ph.is_empty();
            }
            '$' => {
                len += 1;
                last_run = None;
                int_open &= int_ph.is_empty();
            }
            '@' => {
                len += 1;
            }
            'G' | 'g' => {
                let w: String = cs[i..(i + 7).min(cs.len())].iter().collect();
                if !w.eq_ignore_ascii_case("general") {
                    return None;
                }
                len += 1;
                i += 7;
                continue;
            }
            ' ' | '-' | '+' | '(' | ')' | '^' | '{' | '}' | '<' | '>' | '=' | '!' | '&' | '~'
            | '\'' => {
                len += 3;
                last_run = None;
                int_open &= int_ph.is_empty();
            }

            c if (c.is_ascii_alphabetic() && !"dmyhsebgapnDMYHSEBGAPN".contains(c))
                || !c.is_ascii() =>
            {
                len += 3;
                last_run = None;
                int_open &= int_ph.is_empty();
            }
            _ => return None,
        }
        i += 1;
    }
    if number {
        header = if frac { 6 } else { 4 };
        if !int_ph.is_empty() || frac {
            let mut ints = int_ph.clone();
            if grouping {
                while ints.len() < 4 {
                    ints.insert(0, '#');
                }
                len += if sci { 2 } else { 1 };
            }
            let mut prev = None;
            for c in ints {
                if prev != Some(c) {
                    len += 2;
                }
                prev = Some(c);
            }
        }
    }
    Some(header + len)
}

#[cfg(test)]
mod tests {
    use super::section_code_lens;

    #[test]
    fn traced_lengths() {
        for (s, n) in [
            ("0", 6),
            ("000", 6),
            ("?#", 8),
            ("0.0", 9),
            (".0", 7),
            ("0.", 7),
            ("#,##0", 9),
            ("#,##0.00", 12),
            ("0,0", 9),
            ("#,##0.0,,", 12),
            ("0%", 9),
            ("0%%", 12),
            ("\"abc\"0", 14),
            ("x0", 9),
            ("_)0", 9),
            ("*x0", 9),
            ("$0", 7),
            ("(0)", 12),
            ("0E+00", 13),
            ("0.#E-#0", 18),
            ("0.#E-0", 16),
            ("#,##0E+0", 17),
            ("# ?/?", 16),
            ("?/?", 11),
            ("0/4", 13),
            ("0/40", 15),
            ("# ?/8", 18),
            ("General", 1),
            ("General x", 7),
            ("x", 3),
            ("\"lit\"", 8),
            ("1", 7),
            ("0 1", 12),
            ("0 0", 11),
            ("[Red]0", 8),
            ("0.0.0", 12),
            ("x0x", 12),
            ("?#,,,", 8),
        ] {
            assert_eq!(section_code_lens(s)[0], Some(n), "{s}");
        }
        assert_eq!(
            section_code_lens("[=0]?#,,,;0;0.#E-#0;@")[..3],
            [Some(18), Some(6), Some(18)]
        );
        assert_eq!(section_code_lens("[=0]x;0")[0], Some(13));
        assert_eq!(section_code_lens("[=0]General")[0], Some(11));
        for (s, n) in [
            ("d/m/yyyy", 5),
            ("h:mm", 3),
            ("mm:ss.0", 7),
            ("[h]:mm", 4),
            ("m/d/yy h:mm", 11),
            ("d-mmm-yy", 9),
            ("h:mm AM/PM", 17),
            ("mmm d, yyyy", 12),
            ("hh:mm:ss.00", 9),
            ("[mm]:ss", 4),
            ("h:mm A/P", 13),
            ("\"x\"d", 5),
            ("d\\x", 4),
            ("[Red]d", 3),
            ("ss.000", 5),
            ("d$", 2),
        ] {
            assert_eq!(section_code_lens(s)[0], Some(n), "{s}");
        }
        assert_eq!(
            section_code_lens("[<=1000]m/d/yy h:mm;=#,##0,,_0;#?%%%0#;@_)")[..3],
            [Some(21), Some(15), Some(21)]
        );
    }
}
