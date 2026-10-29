use std::cmp::Ordering;

pub fn cut255(s: &str) -> &str {
    if s.len() <= 255 {
        return s;
    }
    let mut units = 0;
    for (i, ch) in s.char_indices() {
        units += ch.len_utf16();
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
        s.encode_utf16().count()
    }
}

pub fn utf16_slice(s: &str, start: usize, len: usize) -> String {
    if s.is_ascii() {
        let a = start.min(s.len());
        let b = start.saturating_add(len).min(s.len());
        return s[a..b].to_string();
    }
    let units: Vec<u16> = s.encode_utf16().collect();
    let a = start.min(units.len());
    let b = start.saturating_add(len).min(units.len());
    String::from_utf16_lossy(&units[a..b])
}

mod collate;

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
    let fold = |c: char| -> char {
        if matches!(
            c,
            '\u{17f}' | '\u{b5}' | '\u{131}' | '\u{130}' | '\u{1e9b}' | '\u{345}'
        ) {
            return c;
        }
        let u = fold_upper(c);
        let mut l = u.to_lowercase();
        match (l.next(), l.next()) {
            (Some(x), None) => x,
            _ => u,
        }
    };
    a.chars().map(fold).eq(b.chars().map(fold))
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
            let mut buf = [0u16; 2];
            for &u in c.encode_utf16(&mut buf).iter() {
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
        let units: Vec<u16> = text.encode_utf16().collect();
        self.match_at(&units, 0, 0, true)
    }

    pub fn find(&self, text: &str, start: usize) -> Option<usize> {
        let units: Vec<u16> = text.encode_utf16().collect();
        (start..=units.len()).find(|&i| self.match_at(&units, i, 0, false))
    }
}
