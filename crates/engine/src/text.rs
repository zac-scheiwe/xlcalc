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

pub fn compare_ci(a: &str, b: &str) -> Ordering {
    if a == b {
        return Ordering::Equal;
    }
    #[cfg(windows)]
    {
        if let Some(o) = win::compare(a, b) {
            return o;
        }
    }
    portable_compare(a, b)
}

pub fn portable_compare(a: &str, b: &str) -> Ordering {
    let key = |s: &str| -> Vec<char> {
        s.chars()
            .filter(|c| *c != '-' && *c != '\'')
            .flat_map(char::to_lowercase)
            .collect()
    };
    key(a).cmp(&key(b)).then_with(|| {
        let x = a.chars().flat_map(char::to_lowercase);
        let y = b.chars().flat_map(char::to_lowercase);
        x.cmp(y)
    })
}

#[cfg(windows)]
mod win {
    use std::cmp::Ordering;

    const NORM_IGNORECASE: u32 = 0x0000_0001;

    #[link(name = "kernel32")]
    extern "system" {
        fn CompareStringEx(
            locale: *const u16,
            flags: u32,
            s1: *const u16,
            n1: i32,
            s2: *const u16,
            n2: i32,
            version: *mut core::ffi::c_void,
            reserved: *mut core::ffi::c_void,
            lparam: isize,
        ) -> i32;
    }

    pub fn compare(a: &str, b: &str) -> Option<Ordering> {
        let x: Vec<u16> = a.encode_utf16().collect();
        let y: Vec<u16> = b.encode_utf16().collect();

        let r = unsafe {
            CompareStringEx(
                std::ptr::null(),
                NORM_IGNORECASE,
                x.as_ptr(),
                x.len() as i32,
                y.as_ptr(),
                y.len() as i32,
                std::ptr::null_mut(),
                std::ptr::null_mut(),
                0,
            )
        };
        match r {
            1 => Some(Ordering::Less),
            2 => Some(Ordering::Equal),
            3 => Some(Ordering::Greater),
            _ => None,
        }
    }
}

pub fn eq_ci(a: &str, b: &str) -> bool {
    compare_ci(a, b) == Ordering::Equal
}

pub fn eq_ordinal_ci(a: &str, b: &str) -> bool {
    if a.len() == b.len() && a.eq_ignore_ascii_case(b) {
        return true;
    }
    let mut x = a.chars().map(fold_upper);
    let mut y = b.chars().map(fold_upper);
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
    Lit(char),
    One,
    Any,
}

fn fold(c: char) -> char {
    let mut l = c.to_lowercase();
    let first = l.next().unwrap_or(c);
    if l.next().is_some() {
        c
    } else {
        first
    }
}

impl Wildcard {
    pub fn new(pattern: &str) -> Wildcard {
        let mut toks = Vec::new();
        let mut it = pattern.chars().peekable();
        while let Some(c) = it.next() {
            match c {
                '~' => {
                    if let Some(n) = it.next() {
                        toks.push(WTok::Lit(fold(n)));
                    }
                }
                '*' => {
                    if toks.last() != Some(&WTok::Any) {
                        toks.push(WTok::Any)
                    }
                }
                '?' => toks.push(WTok::One),
                c => toks.push(WTok::Lit(fold(c))),
            }
        }
        Wildcard { toks }
    }

    pub fn has_wildcards(pattern: &str) -> bool {
        pattern.contains(['*', '?', '~'])
    }

    fn match_at(&self, text: &[char], ti: usize, pi: usize, whole: bool) -> bool {
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
                    WTok::Lit(c) if t < text.len() && fold(text[t]) == *c => {
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
        let chars: Vec<char> = text.chars().collect();
        self.match_at(&chars, 0, 0, true)
    }

    pub fn find(&self, text: &str, start: usize) -> Option<usize> {
        let chars: Vec<char> = text.chars().collect();
        (start..=chars.len()).find(|&i| self.match_at(&chars, i, 0, false))
    }
}
