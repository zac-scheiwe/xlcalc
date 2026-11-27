use std::borrow::Cow;

const LONE_BASE: u32 = 0x10F000;

pub fn lone_unit(c: char) -> Option<u16> {
    let v = (c as u32).wrapping_sub(LONE_BASE);
    (v < 0x800).then(|| 0xD800 + v as u16)
}

pub fn char_units(c: char) -> usize {
    if lone_unit(c).is_some() {
        1
    } else {
        c.len_utf16()
    }
}

pub fn units(s: &str) -> impl Iterator<Item = u16> + Clone + '_ {
    s.chars().flat_map(|c| {
        let mut buf = [0u16; 2];
        let n = match lone_unit(c) {
            Some(u) => {
                buf[0] = u;
                1
            }
            None => c.encode_utf16(&mut buf).len(),
        };
        buf.into_iter().take(n)
    })
}

pub fn from_units(u: &[u16]) -> String {
    char::decode_utf16(u.iter().copied())
        .map(|r| {
            r.unwrap_or_else(|e| {
                let at = u32::from(e.unpaired_surrogate()) - 0xD800;
                char::from_u32(LONE_BASE + at).unwrap_or(char::REPLACEMENT_CHARACTER)
            })
        })
        .collect()
}

pub fn has_halves(s: &str) -> bool {
    s.as_bytes().windows(2).any(|w| w == [0xF4, 0x8F]) && s.chars().any(|c| lone_unit(c).is_some())
}

pub fn join_halves(s: String) -> String {
    if !has_halves(&s) {
        return s;
    }
    from_units(&units(&s).collect::<Vec<_>>())
}

pub fn well_formed(s: &str) -> Cow<'_, str> {
    if !has_halves(s) {
        return Cow::Borrowed(s);
    }
    Cow::Owned(
        s.chars()
            .map(|c| {
                if lone_unit(c).is_some() {
                    char::REPLACEMENT_CHARACTER
                } else {
                    c
                }
            })
            .collect(),
    )
}

pub fn byte_of_unit(s: &str, at: usize) -> usize {
    let mut n = 0;
    for (i, c) in s.char_indices() {
        if n >= at {
            return i;
        }
        n += char_units(c);
    }
    s.len()
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn halves_are_one_unit_and_join_when_they_meet() {
        let hi = from_units(&[0xD83D]);
        let lo = from_units(&[0xDE00]);
        assert_eq!(units(&hi).collect::<Vec<_>>(), [0xD83D]);
        assert!(has_halves(&hi) && !has_halves("a\u{1F600}\u{100000}"));
        assert_eq!(join_halves(format!("{hi}{lo}")), "\u{1F600}");

        let back = join_halves(format!("{lo}{hi}"));
        assert_eq!(units(&back).collect::<Vec<_>>(), [0xDE00, 0xD83D]);
        assert_eq!(well_formed(&format!("a{hi}")), "a\u{FFFD}");
        assert_eq!(byte_of_unit("\u{1F600}a", 1), 4);
    }
}
