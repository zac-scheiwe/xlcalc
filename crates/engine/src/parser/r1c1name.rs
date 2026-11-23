const MAX_ROW: u32 = 1_048_576;
const MAX_COL: u32 = 16_384;

fn zero_of(c: u16) -> u16 {
    match c >> 8 {
        0xd => 0x0d66,
        6 => {
            if c >= 0x06f0 {
                0x06f0
            } else {
                0x0660
            }
        }
        9 => {
            if c >= 0x09e6 {
                0x09e6
            } else {
                0x0966
            }
        }
        10 => {
            if c >= 0x0ae6 {
                0x0ae6
            } else {
                0x0a66
            }
        }
        0xb => {
            if c >= 0x0be6 {
                0x0be6
            } else {
                0x0b66
            }
        }
        0xc => {
            if c >= 0x0ce6 {
                0x0ce6
            } else {
                0x0c66
            }
        }
        0xe => {
            if c >= 0x0ed0 {
                0x0ed0
            } else {
                0x0e50
            }
        }
        0xf => 0x0f20,
        0x10 => 0x1040,
        0x17 => 0x17e0,
        0x18 => 0x1810,
        _ => 0x30,
    }
}

fn digit(c: char) -> Option<u32> {
    let u = u16::try_from(c as u32).ok()?;
    let d = u.checked_sub(zero_of(u))?;
    (d < 10).then_some(u32::from(d))
}

fn parse_int(s: &[char], i: usize, min: u32, ceiling: u32) -> Result<(u32, usize), usize> {
    let mut v: u32 = 0;
    let mut j = i;
    while let Some(d) = s.get(j).copied().and_then(digit) {
        if v > 0x0ccc_cccc {
            return Err(j);
        }
        v = v * 10 + d;
        if v >= 0x8000_0000 || (ceiling != 0 && ceiling <= v) {
            return Err(j);
        }
        j += 1;
    }
    if v < min || j == i {
        return Err(j);
    }
    Ok((v, j))
}

fn axis_part(s: &[char], i: usize, size: u32) -> usize {
    if s.get(i) == Some(&'[') {
        let mut q = i + 1;
        if matches!(s.get(q), Some('-' | '+')) {
            q += 1;
        }

        let end = match parse_int(s, q, 0, size) {
            Ok((_, e)) | Err(e) => e,
        };
        return if s.get(end) == Some(&']') { end + 1 } else { i };
    }
    match parse_int(s, i, 1, size + 1) {
        Ok((_, e)) => e,
        Err(_) => i,
    }
}

fn continues_word(c: char) -> bool {
    c.is_ascii_alphanumeric()
        || matches!(c, '.' | '?' | '\\' | '_')
        || (!c.is_ascii() && c.is_alphanumeric())
}

pub fn ref_prefix(s: &[char]) -> Option<usize> {
    let letter = |i: usize, l: char| s.get(i).is_some_and(|c| c.eq_ignore_ascii_case(&l));
    let mut end;
    let mut consumed;
    if letter(0, 'R') {
        end = axis_part(s, 1, MAX_ROW);
        consumed = end != 1;
        if letter(end, 'C') {
            let col = axis_part(s, end + 1, MAX_COL);
            consumed |= col != end + 1;
            end = col;
        }
    } else if letter(0, 'C') {
        end = axis_part(s, 1, MAX_COL);
        consumed = end != 1;
    } else {
        return None;
    }
    if !consumed && s.get(end).is_some_and(|&c| continues_word(c)) {
        return None;
    }
    Some(end)
}

fn dotted_tail_is_ref(s: &[char], end: usize) -> bool {
    let row_digits =
        s[0].eq_ignore_ascii_case(&'R') && s.get(1).is_some_and(|&c| digit(c).is_some());
    let has_col = s[..end]
        .iter()
        .skip(1)
        .any(|c| c.eq_ignore_ascii_case(&'C'));
    if row_digits && has_col {
        return true;
    }
    let rest = &s[end + 1..];
    ref_prefix(rest) == Some(rest.len()) && rest.iter().any(|&c| digit(c).is_some())
}

pub fn reads_as_r1c1(name: &str) -> bool {
    let s: Vec<char> = name.chars().collect();
    match ref_prefix(&s) {
        None => false,
        Some(end) if s.get(end) == Some(&'.') => dotted_tail_is_ref(&s, end),
        Some(_) => true,
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    fn r(name: &str) -> bool {
        reads_as_r1c1(name)
    }

    #[test]
    fn bare_letters_and_numbers() {
        for yes in [
            "R", "C", "RC", "r", "R1", "C1", "R5C5", "RC5", "R5C", "R2D2", "C3PO",
        ] {
            assert!(r(yes), "{yes}");
        }
        for no in ["RCx", "Rx", "Cx", "Rev2", "Cr1", "RR", "CR"] {
            assert!(!r(no), "{no}");
        }
    }

    #[test]
    fn axis_ceilings_and_zero() {
        assert!(r("R1048576x") && !r("R1048577x") && !r("R0x") && !r("R0"));
        assert!(r("C16384x") && !r("C16385x") && !r("C0") && r("RC16384x") && !r("RC16385x"));
        assert!(r("C016384x"));
    }

    #[test]
    fn digits_of_other_scripts() {
        assert!(r("R\u{663}") && r("C\u{0e53}x") && r("R\u{0e53}\u{0660}"));

        assert!(!r("R\u{ff11}"));
    }
}
