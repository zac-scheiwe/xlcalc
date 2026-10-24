use std::cmp::Ordering;
use std::sync::LazyLock;

#[derive(Clone, Copy, Debug)]
enum Ce {
    Ignore,

    Special(u16),

    One(u32, u8, u8),

    Two(u32, u32, u8),
}

const SYM_BASE: u32 = 0x1000;
const SYM_OTHER: u32 = 0x10_0000;
const DIGIT: u32 = 0x30_0000;
const LATIN: u32 = 0x40_0000;
const GREEK: u32 = 0x50_0000;
const CYRILLIC: u32 = 0x60_0000;
const KANA: u32 = 0x70_0000;
const OTHER_LETTER: u32 = 0x80_0000;
const SUPPLEMENTARY: u32 = 0x90_0000;
const HANGUL: u32 = 0xA0_0000;
const CJK: u32 = 0xB0_0000;

include!("collate_symbols.rs");
include!("collate_letters.rs");
include!("collate_compose.rs");

const SPECIALS: &[u32] = &[
    0x7F, 0x27, 0x2D, 0x2010, 0x2011, 0x2027, 0x2043, 0x2012, 0x2013, 0x2014, 0x2015, 0x374, 0x483,
    0x484, 0x485, 0x486, 0x487,
];

#[rustfmt::skip]
const UNSORTABLE: &[u32] = &[
    0x378, 0x379, 0x37F, 0x380, 0x381, 0x382, 0x383, 0x38B, 0x38D, 0x3A2, 0x524, 0x525, 0x528, 0x529,
    0x52A, 0x52B, 0x52C, 0x52D, 0x52E, 0x52F, 0x1F16, 0x1F17, 0x1F1E, 0x1F1F, 0x1F46, 0x1F47, 0x1F4E,
    0x1F4F, 0x1F58, 0x1F5A, 0x1F5C, 0x1F5E, 0x1F7E, 0x1F7F, 0x1FB5, 0x1FC5, 0x1FD4, 0x1FD5, 0x1FDC,
    0x1FF0, 0x1FF1, 0x1FF5, 0x1FFF,
];

#[rustfmt::skip]
const SYMBOLS_AFTER: &[(u32, u32, u32)] = &[
    (0x1C0, 0x226B, 1), (0x1C1, 0x226B, 2), (0x1C2, 0x226B, 3), (0x1C3, 0xB8, 1), (0x375, 0x2DC, 1),
    (0x37A, 0x2DC, 2), (0x37E, 0xFF1A, 1), (0x384, 0x2DC, 3), (0x385, 0xFF5E, 1), (0x387, 0xB0, 1),
    (0x3F6, 0x22F0, 1), (0x1FBD, 0x301B, 1), (0x1FBF, 0x301B, 2), (0x1FC0, 0x301B, 6), (0x1FC1, 0xFF5E, 3),
    (0x1FCD, 0x301B, 4), (0x1FCE, 0x301B, 3), (0x1FCF, 0x301B, 5), (0x1FDD, 0x20B9, 3), (0x1FDE, 0x20B9, 2),
    (0x1FDF, 0x20B9, 4), (0x1FED, 0xFF5E, 2), (0x1FEE, 0xFF5E, 1), (0x1FEF, 0xFF3E, 1), (0x1FFD, 0xFF5E, 4),
    (0x1FFE, 0x20B9, 1),
];

static SYMBOL_WEIGHT: LazyLock<Vec<u32>> = LazyLock::new(|| {
    let mut w = vec![0u32; 0x1_0000];
    for (i, group) in SYMBOLS.iter().enumerate() {
        for &cp in *group {
            w[cp as usize] = SYM_BASE + 8 * i as u32;
        }
    }

    for cp in 9..=13u32 {
        w[cp as usize] = SYM_BASE - 16 + 2 * (cp - 9);
    }
    w[0x200B] = SYM_BASE + 8 * 4 + 4;
    for &(cp, after, k) in SYMBOLS_AFTER {
        w[cp as usize] = w[after as usize] + k;
    }
    w
});

#[inline]
fn letter_weight(script: u8, letter: u8, sub: Option<u8>) -> u32 {
    let base = match script {
        0 => LATIN,
        1 => GREEK,
        _ => CYRILLIC,
    };
    base + 64 * letter as u32 + sub.map_or(0, |s| 32 + s as u32)
}

#[inline]
fn latin(letter: u8) -> u32 {
    letter_weight(0, letter - b'a', None)
}

fn table_letter(cp: u32) -> Option<Ce> {
    let i = LETTERS.binary_search_by_key(&cp, |e| e.0).ok()?;
    let (_, script, letter, kind, sub, accent, tertiary) = LETTERS[i];
    Some(match kind {
        0 => Ce::One(letter_weight(script, letter, None), accent, tertiary),
        1 => Ce::One(letter_weight(script, letter, Some(sub)), 0, 0),
        _ => Ce::Two(
            letter_weight(script, letter, None),
            letter_weight(script, sub, None),
            accent,
        ),
    })
}

fn find3(table: &[(u32, u8, u8)], cp: u32) -> Option<(u8, u8)> {
    table
        .binary_search_by_key(&cp, |e| e.0)
        .ok()
        .map(|i| (table[i].1, table[i].2))
}

fn digit_like(cp: u32) -> Option<Ce> {
    let rank_of = |c: u32| find3(DIGIT_VARIANTS, c).map_or(0, |(_, r)| r);
    let after_nine = |c: u32| -> Option<u32> {
        let i = AFTER_NINE.binary_search_by_key(&c, |e| e.0).ok()?;
        Some(DIGIT + 16 * 10 + 4 * AFTER_NINE[i].1 as u32)
    };

    let (digit, accent) = match cp {
        0xBC => (0, 4 * rank_of(0x2155) + 2),
        0xBD => (0, 4 * rank_of(0x2156) + 2),
        0xBE => (0, 4 * rank_of(0x2154) + 2),
        0xB9 => (1, 4 * rank_of(0x2460)),
        0xB2 => (2, 4 * rank_of(0x2461)),
        0xB3 => (3, 4 * rank_of(0x2462)),

        0x1BB => (2, 0),
        0x1BC | 0x1BD => (5, 0),

        0x482 | 0x488 | 0x489 => {
            let k = match cp {
                0x482 => 1,
                0x488 => 2,
                _ => 3,
            };
            return Some(Ce::One(after_nine(0x2186)? + k, 0, 0));
        }
        _ => match find3(DIGIT_VARIANTS, cp) {
            Some((d, r)) => (d, 4 * r),
            None => return Some(Ce::One(after_nine(cp)?, 0, 0)),
        },
    };
    Some(Ce::One(DIGIT + 16 * digit as u32, accent, 0))
}

fn kana(cp: u32) -> Option<Ce> {
    let (h, katakana) = match cp {
        0x3041..=0x3096 => (cp, 0),
        0x30A1..=0x30F6 => (cp - 0x60, 1),
        _ => return None,
    };

    let (base, accent, small) = match h {
        0x3041 | 0x3043 | 0x3045 | 0x3047 | 0x3049 => (h + 1, 0, 1),
        0x3042 | 0x3044 | 0x3046 | 0x3048 | 0x304A => (h, 0, 0),
        0x304B..=0x3062 => {
            let k = h - 0x304B;
            (0x304B + k / 2 * 2, (k % 2) as u8, 0)
        }
        0x3063 => (0x3064, 0, 1),
        0x3064 => (0x3064, 0, 0),
        0x3065 => (0x3064, 1, 0),
        0x3066..=0x3069 => {
            let k = h - 0x3066;
            (0x3066 + k / 2 * 2, (k % 2) as u8, 0)
        }
        0x306F..=0x307D => {
            let k = h - 0x306F;
            (0x306F + k / 3 * 3, (k % 3) as u8, 0)
        }
        0x3083 | 0x3085 | 0x3087 => (h + 1, 0, 1),
        0x308E => (0x308F, 0, 1),
        0x3094 => (0x3046, 1, 0),
        0x3095 => (0x304B, 0, 1),
        0x3096 => (0x3051, 0, 1),
        _ => (h, 0, 0),
    };
    Some(Ce::One(KANA + base, accent, small + 2 * katakana))
}

fn ce(c: char) -> Ce {
    let cp = c as u32;
    if c.is_ascii_alphabetic() {
        return Ce::One(latin(c.to_ascii_lowercase() as u8), 0, 0);
    }
    if c.is_ascii_digit() {
        return Ce::One(DIGIT + 16 * (cp - 0x30), 0, 0);
    }
    if let Some(i) = SPECIALS.iter().position(|&s| s == cp) {
        return Ce::Special(2 + i as u16);
    }
    if let Some(&(_, g)) = SPECIALS_EXTRA.iter().find(|e| e.0 == cp) {
        return Ce::Special(2 + SPECIALS.len() as u16 + g);
    }
    match cp {
        0xAD | 0x640 | 0x200C..=0x200F | 0x202A..=0x202E | 0x2060..=0x206F | 0xFEFF => {
            return Ce::Ignore
        }
        1..=8 | 14..=31 => return Ce::Special(1),

        0x430..=0x44F => return Ce::One(letter_weight(2, (cp - 0x430) as u8, None), 0, 0),
        0xAA => return Ce::One(latin(b'a'), 1, 0),
        0xBA => return Ce::One(latin(b'o'), 1, 0),

        0xFF10..=0xFF19 => return Ce::One(DIGIT + 16 * (cp - 0xFF10), 0, 1),
        _ => {}
    }
    if IGNORABLE_EXTRA.binary_search(&cp).is_ok() || UNSORTABLE.binary_search(&cp).is_ok() {
        return Ce::Ignore;
    }
    if let Some(e) = table_letter(cp) {
        return e;
    }
    if let Some(e) = digit_like(cp) {
        return e;
    }
    if let Some((letter, rank)) = find3(LETTER_EXTRA, cp) {
        return Ce::One(letter_weight(0, letter, None), 200 + rank, 0);
    }
    if let Some((letter, rank)) = find3(GREEK_EXTRA, cp) {
        return Ce::One(
            letter_weight(1, letter, None),
            if rank == 0 { 0 } else { 200 + rank },
            0,
        );
    }
    if let Some((letter, rank)) = find3(HEBREW_EXTRA, cp) {
        return Ce::One(OTHER_LETTER + 0x5D0 + letter as u32, 2 * rank, 0);
    }
    if (cp as usize) < SYMBOL_WEIGHT.len() {
        let w = SYMBOL_WEIGHT[cp as usize];
        if w != 0 {
            return Ce::One(w, 0, 0);
        }
    }

    if let Some(d) = script_digit(cp) {
        return Ce::One(DIGIT + 16 * d, 0, 2);
    }
    if let Some(e) = kana(cp) {
        return e;
    }
    let lower = c.to_lowercase().next().unwrap_or(c) as u32;
    match lower {
        0x370..=0x3FF | 0x1F00..=0x1FFF => Ce::One(GREEK + 0x4000 + lower, 0, 0),
        0x400..=0x52F => Ce::One(CYRILLIC + 0x4000 + lower, 0, 0),
        0x3040..=0x30FF => Ce::One(KANA + cp, 0, 0),
        0xAC00..=0xD7AF | 0x1100..=0x11FF | 0x3130..=0x318F => Ce::One(HANGUL + cp, 0, 0),
        0x3400..=0x4DBF | 0x4E00..=0x9FFF | 0xF900..=0xFAFF => Ce::One(CJK + cp, 0, 0),
        0x10000.. => Ce::One(SUPPLEMENTARY + cp, 0, 0),
        _ if c.is_alphanumeric() => Ce::One(OTHER_LETTER + lower, 0, 0),
        _ => Ce::One(SYM_OTHER + cp, 0, 0),
    }
}

fn script_digit(cp: u32) -> Option<u32> {
    const ZEROS: &[u32] = &[
        0x660, 0x6F0, 0x7C0, 0x966, 0x9E6, 0xA66, 0xAE6, 0xB66, 0xBE6, 0xC66, 0xCE6, 0xD66, 0xDE6,
        0xE50, 0xED0, 0xF20, 0x1040, 0x1090, 0x17E0, 0x1810, 0x1946, 0x19D0,
    ];
    ZEROS
        .iter()
        .find(|&&z| (z..z + 10).contains(&cp))
        .map(|z| cp - z)
}

#[derive(Debug, PartialEq, Eq, Hash)]
struct SortKey {
    primary: Vec<u32>,

    accents: Vec<u16>,

    variants: Vec<u8>,

    case: Vec<u8>,

    marks: Vec<(u32, u16)>,
}

const MARK_ORDER: [u32; 13] = [
    0x301, 0x300, 0x307, 0x302, 0x308, 0x30C, 0x306, 0x304, 0x303, 0x30A, 0x30B, 0x328, 0x327,
];

fn mark_rank(base: char, mark: char) -> Option<u16> {
    let pos = MARK_ORDER.iter().position(|&m| m == mark as u32)?;
    let precomposed = |m: u32| -> Option<u16> {
        let i = COMPOSE
            .binary_search_by_key(&(base as u32, m), |e| (e.0, e.1))
            .ok()?;
        match ce(char::from_u32(COMPOSE[i].2)?) {
            Ce::One(_, a, _) if a < 100 => Some(a as u16),
            _ => None,
        }
    };
    if let Some(r) = precomposed(mark as u32) {
        return Some(r * 16);
    }
    let below = MARK_ORDER[..pos]
        .iter()
        .enumerate()
        .rev()
        .find_map(|(i, &m)| precomposed(m).map(|r| (i, r)));
    Some(below.map_or(pos as u16 + 1, |(i, r)| r * 16 + (pos - i) as u16))
}

fn sort_key(s: &str) -> SortKey {
    let mut k = SortKey {
        primary: Vec::with_capacity(s.len()),
        accents: Vec::with_capacity(s.len()),
        variants: Vec::with_capacity(s.len()),
        case: Vec::with_capacity(s.len()),
        marks: Vec::new(),
    };
    let mut base: Option<char> = None;
    for c in s.chars() {
        if let (Some(base), Some(last)) = (base, k.accents.last_mut()) {
            if let Some(rank) = mark_rank(base, c) {
                *last = if *last == 0 {
                    rank
                } else {
                    last.saturating_add(4000)
                };
                continue;
            }
        }
        base = c.to_lowercase().next().filter(|b| b.is_ascii_lowercase());
        match ce(c) {
            Ce::Ignore => {}
            Ce::Special(w) => k.marks.push((k.primary.len() as u32, w)),
            Ce::One(p, a, v) => {
                k.primary.push(p);
                k.accents.push(a as u16 * 16);
                k.variants.push(v);
                k.case.push(u8::from(c.is_uppercase()));
            }
            Ce::Two(p, q, a) => {
                k.primary.extend([p, q]);
                k.accents.extend([0, a as u16 * 16]);
                k.variants.extend([0, 0]);
                k.case.extend([u8::from(c.is_uppercase()); 2]);
            }
        }
    }
    k
}

fn cmp_keys(a: &SortKey, b: &SortKey) -> Ordering {
    cmp_keys_case(a, b, false)
}

fn cmp_keys_case(a: &SortKey, b: &SortKey, match_case: bool) -> Ordering {
    a.primary
        .cmp(&b.primary)
        .then_with(|| a.accents.cmp(&b.accents))
        .then_with(|| a.variants.cmp(&b.variants))
        .then_with(|| {
            if match_case {
                a.case.cmp(&b.case)
            } else {
                Ordering::Equal
            }
        })
        .then_with(|| {
            for (x, y) in a.marks.iter().zip(&b.marks) {
                let o = y.0.cmp(&x.0).then(x.1.cmp(&y.1));
                if o != Ordering::Equal {
                    return o;
                }
            }
            a.marks.len().cmp(&b.marks.len())
        })
}

#[inline]
fn ascii_plain(b: u8) -> Option<u32> {
    match b {
        b'a'..=b'z' => Some(latin(b)),
        b'A'..=b'Z' => Some(latin(b + 32)),
        b'0'..=b'9' => Some(DIGIT + 16 * (b - b'0') as u32),
        b'\'' | b'-' | 0..=8 | 14..=31 | 0x7F => None,
        _ => Some(SYMBOL_WEIGHT[b as usize]),
    }
}

fn ascii_cmp(a: &str, b: &str) -> Option<Ordering> {
    if !a.is_ascii() || !b.is_ascii() {
        return None;
    }
    let (x, y) = (a.as_bytes(), b.as_bytes());
    let mut i = 0;
    loop {
        match (x.get(i), y.get(i)) {
            (None, None) => return Some(Ordering::Equal),
            (Some(&p), q) => {
                let wp = ascii_plain(p)?;
                let Some(&q) = q else {
                    return x[i..]
                        .iter()
                        .all(|&c| ascii_plain(c).is_some())
                        .then_some(Ordering::Greater);
                };
                let wq = ascii_plain(q)?;
                if wp != wq {
                    return Some(wp.cmp(&wq));
                }
            }
            (None, Some(_)) => {
                return y[i..]
                    .iter()
                    .all(|&c| ascii_plain(c).is_some())
                    .then_some(Ordering::Less);
            }
        }
        i += 1;
    }
}

pub fn compare(a: &str, b: &str) -> Ordering {
    if let Some(o) = ascii_cmp(a, b) {
        return o;
    }
    cmp_keys(&sort_key(a), &sort_key(b))
}

pub fn compare_match_case(a: &str, b: &str) -> Ordering {
    cmp_keys_case(&sort_key(a), &sort_key(b), true)
}

pub fn equal(a: &str, b: &str) -> bool {
    compare(a, b) == Ordering::Equal
}

pub fn key(s: &str) -> Vec<u32> {
    let k = sort_key(s);
    let mut v = k.primary;
    v.push(u32::MAX);
    v.extend(k.accents.iter().map(|&a| a as u32));
    v.push(u32::MAX);
    v.extend(k.variants.iter().map(|&a| a as u32));
    v.push(u32::MAX);
    for (p, w) in k.marks {
        v.push(p);
        v.push(w as u32);
    }
    v
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn measured_orders() {
        let mut v = vec![
            "ab", "a-b", "a-c", "aa", "a b", "a'b", "_a", "-a", "1a", "Ab", "ä", "b", "é", "f", "e",
        ];
        v.sort_by(|a, b| compare(a, b));
        assert_eq!(
            v,
            [
                "_a", "1a", "-a", "ä", "a b", "aa", "ab", "Ab", "a'b", "a-b", "a-c", "b", "e", "é",
                "f"
            ]
        );
        let mut w = vec![
            "-ab", "a-b", "ab-", "ab", "a--b", "'ab", "a'b", "ab'", "a-b-", "-a-b", "a b", "-",
            "'", "--",
        ];
        w.sort_by(|a, b| compare(a, b));
        assert_eq!(
            w,
            [
                "'", "-", "--", "a b", "ab", "ab'", "ab-", "a'b", "a-b", "a-b-", "a--b", "'ab",
                "-ab", "-a-b"
            ]
        );
        assert!(equal("ß", "ss") && equal("Æ", "ae") && equal("a\u{AD}b", "ab"));
        assert!(!equal("é", "e") && !equal("a-b", "ab"));
        assert_eq!(compare("10", "9"), Ordering::Less);
        assert_eq!(compare("ée", "eé"), Ordering::Greater);
    }
}
