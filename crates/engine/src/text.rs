use std::cmp::Ordering;

pub fn cut255(s: &str) -> &str {
    if s.len() <= 255 {
        return s;
    }
    let mut units = 0;
    for (i, ch) in s.char_indices() {
        units += wide::char_units(ch);
        if units > 255 {
            return &s[..i];
        }
    }
    s
}

pub fn utf16_len(s: &str) -> usize {
    if s.is_ascii() {
        s.len()
    } else {
        crate::text::wide::units(s).count()
    }
}

pub fn utf16_slice(s: &str, start: usize, len: usize) -> String {
    if s.is_ascii() {
        let a = start.min(s.len());
        let b = start.saturating_add(len).min(s.len());
        return s[a..b].to_string();
    }
    let units: Vec<u16> = crate::text::wide::units(s).collect();
    let a = start.min(units.len());
    let b = start.saturating_add(len).min(units.len());
    crate::text::wide::from_units(&units[a..b])
}

mod collate;
pub mod wide;

pub(crate) fn equal_ignoring_accents(a: char, b: char) -> bool {
    collate::equal_ignoring_accents(a, b)
}

pub fn compare_ci(a: &str, b: &str) -> Ordering {
    if a == b {
        return Ordering::Equal;
    }
    match collate::compare(a, b) {
        Ordering::Equal => collate::trailing_ignorable(a).cmp(&collate::trailing_ignorable(b)),
        o => o,
    }
}

pub fn collate_compare(a: &str, b: &str) -> Ordering {
    match collate::compare(a, b) {
        Ordering::Equal => collate::trailing_ignorable(a).cmp(&collate::trailing_ignorable(b)),
        o => o,
    }
}

pub fn collate_compare_match_case(a: &str, b: &str) -> Ordering {
    collate::compare_match_case(a, b)
}

pub fn collate_key(s: &str) -> Vec<u8> {
    let mut k = collate::key_bytes(s);
    k.push(collate::trailing_ignorable(s).min(255) as u8);
    k
}

pub fn eq_ci(a: &str, b: &str) -> bool {
    compare_ci(a, b) == Ordering::Equal
}

pub fn eq_ordinal_ci(a: &str, b: &str) -> bool {
    if a.len() == b.len() && a.eq_ignore_ascii_case(b) {
        return true;
    }
    let lower = crate::functions::text::lower_char;
    let mut x = a.chars().map(lower);
    let mut y = b.chars().map(lower);
    loop {
        match (x.next(), y.next()) {
            (None, None) => return true,
            (Some(p), Some(q)) if p == q => {}
            _ => return false,
        }
    }
}

pub fn eq_sheet_name(a: &str, b: &str) -> bool {
    if a.len() == b.len() && a.eq_ignore_ascii_case(b) {
        return true;
    }
    a.chars()
        .map(fold_name_char)
        .eq(b.chars().map(fold_name_char))
}

fn self_only(c: char) -> bool {
    matches!(
        c,
        '\u{17f}' | '\u{b5}' | '\u{131}' | '\u{130}' | '\u{1e9b}' | '\u{345}'
    )
}

fn fold_name_char(c: char) -> char {
    if self_only(c) {
        return c;
    }
    let u = fold_upper(c);
    let mut l = u.to_lowercase();
    match (l.next(), l.next()) {
        (Some(x), None) => x,
        _ => u,
    }
}

pub fn name_key(s: &str) -> String {
    s.chars()
        .map(|c| {
            if self_only(c) {
                c
            } else {
                fold_upper(fold_name_char(c))
            }
        })
        .collect()
}

pub fn fold_upper(c: char) -> char {
    let mut u = c.to_uppercase();
    match (u.next(), u.next()) {
        (Some(x), None) => x,
        _ => c,
    }
}

#[derive(Clone, Debug)]
pub struct Wildcard {
    toks: Vec<WTok>,
}

#[derive(Clone, Debug, PartialEq)]
enum WTok {
    Lit(u16),
    One,
    Any,
}

fn fold_unit(u: u16) -> u16 {
    match char::from_u32(u as u32) {
        Some(c) => {
            let f = crate::functions::text::lower_char(c) as u32;
            if f <= 0xFFFF {
                f as u16
            } else {
                u
            }
        }
        None => u,
    }
}

impl Wildcard {
    pub fn new(pattern: &str) -> Wildcard {
        let mut toks = Vec::new();
        let mut it = pattern.chars().peekable();
        let lit = |toks: &mut Vec<WTok>, c: char| {
            let mut buf = [0u8; 4];
            for u in wide::units(c.encode_utf8(&mut buf)) {
                toks.push(WTok::Lit(fold_unit(u)));
            }
        };
        while let Some(c) = it.next() {
            match c {
                '~' => {
                    if let Some(n) = it.next() {
                        lit(&mut toks, n);
                    }
                }
                '*' => {
                    if toks.last() != Some(&WTok::Any) {
                        toks.push(WTok::Any)
                    }
                }
                '?' => toks.push(WTok::One),
                c => lit(&mut toks, c),
            }
        }
        Wildcard { toks }
    }

    pub fn has_wildcards(pattern: &str) -> bool {
        pattern.contains(['*', '?', '~'])
    }

    fn match_at(&self, text: &[u16], ti: usize, pi: usize, whole: bool) -> bool {
        let (mut t, mut p) = (ti, pi);
        let mut star: Option<(usize, usize)> = None;
        loop {
            if p == self.toks.len() {
                if !whole || t == text.len() {
                    return true;
                }
            } else {
                match &self.toks[p] {
                    WTok::Any => {
                        star = Some((p, t));
                        p += 1;
                        continue;
                    }
                    WTok::One if t < text.len() => {
                        t += 1;
                        p += 1;
                        continue;
                    }
                    WTok::Lit(c) if t < text.len() && fold_unit(text[t]) == *c => {
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

    pub fn matches(&self, text: &str) -> bool {
        let units: Vec<u16> = crate::text::wide::units(text).collect();
        self.match_at(&units, 0, 0, true)
    }

    pub fn find(&self, text: &str, start: usize) -> Option<usize> {
        let units: Vec<u16> = crate::text::wide::units(text).collect();
        (start..=units.len()).find(|&i| self.match_at(&units, i, 0, false))
    }
}

#[cfg(test)]
mod name_key_tests {
    use super::name_key;

    #[test]
    fn dotless_i_and_long_s_stay_apart_and_kelvin_joins_k() {
        assert_ne!(name_key("\u{131}"), name_key("I"));
        assert_ne!(name_key("\u{17f}"), name_key("S"));
        assert_eq!(name_key("\u{212a}x"), name_key("kX"));
        assert_eq!(name_key("rate"), "RATE");
    }
}
