use super::r1c1name::{continues_word, digit_value, ref_prefix};

const MAX_COL: u32 = 16_384;
const MAX_ROW: u32 = 1_048_576;

pub(crate) fn bracket_word(text: &str) -> bool {
    let Some((book, rest)) = text.strip_prefix('[').and_then(|r| r.split_once(']')) else {
        return false;
    };
    let mut rest = rest.chars();
    !book.is_empty()
        && book.chars().all(|c| name_scan_char(c, false))
        && rest.next().is_some_and(|c| name_scan_char(c, true))
        && rest.all(|c| name_scan_char(c, false))
}

fn a1_cell_at(s: &[char], start: usize) -> Option<usize> {
    let mut i = start;
    let abs_col = s.get(i) == Some(&'$');
    if abs_col {
        i += 1;
    }
    let col_start = i;
    let mut col: u64 = 0;
    while let Some(c) = s.get(i).filter(|c| c.is_ascii_alphabetic()) {
        col = col * 26 + u64::from(c.to_ascii_uppercase() as u8 - b'A' + 1);
        if col > u64::from(MAX_COL) {
            return None;
        }
        i += 1;
    }
    if i == col_start {
        return None;
    }
    let abs_row = s.get(i) == Some(&'$');
    if abs_row {
        i += 1;
    }
    let row_start = i;
    let mut row: u64 = 0;
    while let Some(d) = s.get(i).copied().and_then(digit_value) {
        row = row * 10 + u64::from(d);
        if row > u64::from(MAX_ROW) {
            return None;
        }
        i += 1;
    }
    if i == row_start || row == 0 {
        return None;
    }
    let next = s.get(i).copied();
    if next == Some('!') {
        return None;
    }
    if !abs_col && !abs_row {
        if let Some(c) = next {
            if c != '.' && continues_word(c) {
                return None;
            }
        }
    }
    let letter = |k: usize, l: char| s.get(i + k).is_some_and(|c| c.eq_ignore_ascii_case(&l));
    if s.len() >= i + 5
        && s[i] == '.'
        && letter(1, 'R')
        && s[i + 2] == '1'
        && letter(3, 'C')
        && s[i + 4] == '1'
    {
        return None;
    }
    Some(i)
}

fn dotted_tail_is_area(s: &[char], end: usize) -> bool {
    let mut k = end + 1;
    if s.get(k) == Some(&'.') {
        k += 1;
    }
    while matches!(s.get(k), Some(' ' | '\n')) {
        k += 1;
    }
    a1_cell_at(s, k).is_some()
}

#[derive(Clone, Copy, Debug, PartialEq)]
pub(crate) enum TokenKind {
    Area,

    Error { name_valid: bool },
}

fn name_token_kind(s: &[char]) -> TokenKind {
    let refused = TokenKind::Error { name_valid: false };
    let name = TokenKind::Error { name_valid: true };
    match a1_cell_at(s, 0) {
        Some(end) if s.get(end) == Some(&'.') => {
            if dotted_tail_is_area(s, end) {
                TokenKind::Area
            } else {
                name
            }
        }
        Some(_) => TokenKind::Area,
        None if ref_prefix(s).is_some() => refused,
        None => name,
    }
}

fn kind_accepted(kind: TokenKind, allow_ref: bool) -> bool {
    match kind {
        TokenKind::Area => allow_ref,
        TokenKind::Error { name_valid } => name_valid,
    }
}

pub(crate) fn name_reads_as_ref(s: &[char]) -> bool {
    !kind_accepted(name_token_kind(s), false)
}

pub(super) fn name_scan_char(c: char, first: bool) -> bool {
    if c.is_ascii() {
        c.is_ascii_alphabetic()
            || matches!(c, '_' | '\\')
            || (!first && (c.is_ascii_digit() || matches!(c, '.' | '?')))
    } else {
        super::namech::name_char(c, first)
    }
}

pub(super) fn easy_accepts(word: &str) -> bool {
    let Some(first) = word.chars().next() else {
        return false;
    };
    if !first.is_alphabetic()
        || word.eq_ignore_ascii_case("TRUE")
        || word.eq_ignore_ascii_case("FALSE")
        || matches!(first, 'R' | 'r' | 'C' | 'c')
    {
        return false;
    }
    let (mut wide, mut plain) = (0u32, 0u32);
    for (i, c) in word.chars().enumerate() {
        if !name_scan_char(c, i == 0) || c == '.' {
            return false;
        }
        if super::namech::is_digit(c) {
            wide += 1;
        } else {
            plain += 1;
        }
    }
    wide == 0 || plain >= 4
}

pub(crate) fn prs_nam(name: &str, biff8: bool) -> bool {
    let s: Vec<char> = name.chars().collect();
    if s.first().is_none_or(|&c| c == ' ') {
        return false;
    }
    let units = name.encode_utf16().count();
    if !biff8
        && units >= 3
        && s[0] == '_'
        && s[1].eq_ignore_ascii_case(&'x')
        && s[2].eq_ignore_ascii_case(&'l')
    {
        return false;
    }
    if easy_accepts(name) {
        return true;
    }
    if name.eq_ignore_ascii_case("TRUE") || name.eq_ignore_ascii_case("FALSE") {
        return false;
    }
    s.iter()
        .enumerate()
        .all(|(i, &c)| name_scan_char(c, i == 0))
        && !name_reads_as_ref(&s)
}

#[cfg(test)]
mod tests {
    use super::*;

    fn r(name: &str) -> bool {
        name_reads_as_ref(&name.chars().collect::<Vec<_>>())
    }

    #[test]
    fn easy_word_and_preconditions() {
        let easy = easy_accepts;
        for (t, v) in [
            ("\u{4e2d}\u{6587}", true),
            ("ab\u{e9}1", false),
            ("abc\u{e9}1", true),
            ("ab\u{663}", false),
            ("ab\u{ff11}", false),
            ("ab\u{b2}", false),
            ("ab\u{2160}", true),
            ("abc\u{663}d", true),
            ("stra\u{df}e", true),
        ] {
            assert_eq!(easy(t), v, "{t}");
        }
        for yes in [
            "abcd", "abc", "abcd1", "zz1zz", "abc_d", "abcd?", "xyz_1", "xtrue",
        ] {
            assert!(easy(yes), "{yes}");
        }
        for no in [
            "abc1", "ab1", "xfe1", "abcde.x", "ab.cd", "TRUE", "false", "rates", "_abcd",
        ] {
            assert!(!easy(no), "{no}");
        }
        for refused in [
            "", " a", "_xlfoo", "_XLfoo", "_xl", "_xl.a", "abc1", "TRUE", "FALSE",
        ] {
            assert!(!prs_nam(refused, false), "{refused:?}");
        }
        for ok in ["_x", "x_xl", "ab.cd", "xyz1", "_abcd", "rates", "abcd1"] {
            assert!(prs_nam(ok, false), "{ok}");
        }
        assert!(prs_nam("_xlfoo", true));
    }

    #[test]
    fn bracketed_words() {
        use super::bracket_word;
        for yes in [
            "[1]Sheet1",
            "[A1]x",
            "[x]A1",
            "[TRUE]x",
            "[x]TRUE",
            "[R1C1]x",
        ] {
            assert!(bracket_word(yes), "{yes}");
        }
        for no in [
            "[x]",
            "[]x",
            "[x",
            "x]",
            "a[x]",
            "[x]]",
            "[x]1a",
            "[Book 1]Sheet1",
        ] {
            assert!(!bracket_word(no), "{no}");
        }
    }

    #[test]
    fn traced_verdicts() {
        for refused in [
            "R1.C1", "R1C", "A1.B2", "A1..B2", "R1C1.x", "R1C1.C1", "R1.R1C1", "XFD1", "R1.A1",
            "R1.$C1", "R1. C1", "R1.C1.x", "R1..C1", "R1.C1.C1", "RC", "R1C1", "R1C1x", "R1C1.",
        ] {
            assert!(r(refused), "{refused}");
        }
        for name in [
            "R1.x",
            "R1.",
            "RC1.x",
            "C1.x",
            "R2.c3x",
            "A1x",
            "A1_",
            "A1?",
            "A1.",
            "A1.x",
            "A1.x.B2",
            "RC.x",
            "R.x",
            "C.x",
            "R1.C1x",
            "R1.RC",
            "R1.R",
            "R1.C",
            "XFD1.x",
            "XFE1.x",
            "XFE1",
            "A1048576.x",
            "A1048577.x",
            "A0.x",
            "A0",
            "R1.1",
            "x.R1",
            "x.R1.C1",
            "R1.x.C1",
            "Rx",
            "Cx",
            "RCx",
        ] {
            assert!(!r(name), "{name}");
        }
    }
}
