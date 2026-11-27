use std::cell::RefCell;
use std::rc::Rc;

use crate::locale::Locale;
use crate::numfmt::{compile_for_compare, Compiled};

pub(crate) const IFMT_BASE_MAX: usize = 0x24;

const LOAD_MAP: [u8; 0x52] = [
    0, 1, 2, 3, 4, 9, 10, 11, 12, 13, 14, 15, 17, 18, 19, 20, 21, 22, 23, 24, 25, 26, 27, 255, 255,
    255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 5, 6, 7, 8, 33, 32, 35, 34, 28, 31,
    29, 16, 30, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255,
    255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255,
];

pub(crate) fn slot_of_file_id(id: u32) -> Option<usize> {
    let mapped = *LOAD_MAP.get(id as usize)?;
    if mapped != 0xff {
        return Some(usize::from(mapped));
    }
    Some(match id {
        0x1b..=0x1f | 0x24 | 0x32..=0x3a | 0x47 | 0x48 => 0x13,
        0x20..=0x23 | 0x4d => 0x1a,
        0x3b..=0x3e => id as usize - 0x3a,
        0x3f => 9,
        0x40 => 10,
        0x41..=0x44 => id as usize - 0x36,
        0x45 => 0x11,
        0x46 => 0x12,
        0x49..=0x4b => id as usize - 0x35,
        0x4c => 0x19,
        0x4e => 0x1b,
        0x4f => 0x1c,
        0x50 => 0x1f,
        0x51 => 0x1d,
        _ => 0,
    })
}

fn file_id_of_slot(slot: usize) -> Option<u32> {
    LOAD_MAP
        .iter()
        .position(|&s| usize::from(s) == slot)
        .map(|i| i as u32)
}

pub fn builtin_code_of_file_id(loc: &Locale, id: u32) -> Option<String> {
    match slot_of_file_id(id)? {
        0 => None,
        slot => slot_code(loc, slot),
    }
}

pub fn builtin_file_id_of_code(loc: &Locale, code: &str) -> Option<u32> {
    file_id_of_slot(ifmt_of_code(loc, code)?)
}

fn slot_code(loc: &Locale, slot: usize) -> Option<String> {
    let c = &loc.currency;
    Some(match slot {
        0 => "General".into(),
        1 => "0".into(),
        2 => "0.00".into(),
        3 => "#,##0".into(),
        4 => "#,##0.00".into(),
        5 => "#,##0;-#,##0".into(),
        6 => "#,##0;[Red]-#,##0".into(),
        7 => "#,##0.00;-#,##0.00".into(),
        8 => "#,##0.00;[Red]-#,##0.00".into(),
        9 => format!("{c}#,##0;-{c}#,##0"),
        10 => format!("{c}#,##0;[Red]-{c}#,##0"),
        11 => format!("{c}#,##0.00;-{c}#,##0.00"),
        12 => format!("{c}#,##0.00;[Red]-{c}#,##0.00"),
        13 => "0%".into(),
        14 => "0.00%".into(),
        15 => "0.00E+00".into(),
        16 => "##0.0E+0".into(),
        17 => "# ?/?".into(),
        18 => "# ??/??".into(),
        19 => loc.short_date_format.clone(),
        20 => "d-mmm-yy".into(),
        21 => "d-mmm".into(),
        22 => "mmm-yy".into(),
        23 => "h:mm AM/PM".into(),
        24 => "h:mm:ss AM/PM".into(),
        25 => "h:mm".into(),
        26 => "h:mm:ss".into(),
        27 => format!("{} h:mm", loc.short_date_format),
        28 => "mm:ss".into(),
        29 => "mm:ss.0".into(),
        30 => "@".into(),
        31 => "[h]:mm:ss".into(),
        0x20 => format!(r#"_-{c}* #,##0_-;\-{c}* #,##0_-;_-{c}* "-"_-;_-@_-"#),
        0x21 => r#"_-* #,##0_-;\-* #,##0_-;_-* "-"_-;_-@_-"#.into(),
        0x22 => format!(r#"_-{c}* #,##0.00_-;\-{c}* #,##0.00_-;_-{c}* "-"??_-;_-@_-"#),
        0x23 => r#"_-* #,##0.00_-;\-* #,##0.00_-;_-* "-"??_-;_-@_-"#.into(),
        _ => return None,
    })
}

fn compiled_slots(loc: &Locale) -> Rc<Vec<(usize, Compiled)>> {
    type Cache = RefCell<Vec<(String, String, Rc<Vec<(usize, Compiled)>>)>>;
    thread_local!(static CACHE: Cache = const { RefCell::new(Vec::new()) });
    CACHE.with_borrow_mut(|cache| {
        let hit = cache
            .iter()
            .find(|(c, d, _)| *c == loc.currency && *d == loc.short_date_format);
        if let Some((_, _, slots)) = hit {
            return slots.clone();
        }
        let slots: Rc<Vec<_>> = Rc::new(
            (0..IFMT_BASE_MAX)
                .filter_map(|s| Some((s, compile_for_compare(&slot_code(loc, s)?).any_ampm_case())))
                .collect(),
        );
        cache.push((
            loc.currency.clone(),
            loc.short_date_format.clone(),
            slots.clone(),
        ));
        slots
    })
}

pub(crate) fn ifmt_of_code(loc: &Locale, code: &str) -> Option<usize> {
    let want = compile_for_compare(&crate::cellfmt::format_code_as_read(code)).any_ampm_case();
    compiled_slots(loc)
        .iter()
        .find(|(_, c)| *c == want)
        .map(|&(s, _)| s)
}

pub(crate) fn builtin_spelling(loc: &Locale, code: &str) -> Option<String> {
    let slot = slot_code(loc, ifmt_of_code(loc, code)?)?;
    Some(crate::numfmt::readback_spelling(&slot, None))
}

const IFMT_MONTH_YEAR: usize = 0x16;

const MONTH_LONG_YEAR: &str = "mmm-yyyy";

pub(crate) fn is_month_year_format(loc: &Locale, code: &str) -> bool {
    let key =
        |c: &str| compile_for_compare(&crate::cellfmt::format_code_as_read(c)).any_ampm_case();
    ifmt_of_code(loc, code) == Some(IFMT_MONTH_YEAR) || key(code) == key(MONTH_LONG_YEAR)
}

pub(crate) fn is_builtin(loc: &Locale, code: &str) -> bool {
    ifmt_of_code(loc, code).is_some_and(|i| i < IFMT_BASE_MAX)
}

#[derive(Debug, Clone, Default)]
pub struct NumberFormatTable {
    codes: Vec<String>,
    index: rustc_hash::FxHashMap<Compiled, usize>,
}

impl NumberFormatTable {
    pub fn slot(&mut self, code: &str) -> usize {
        let key = compile_for_compare(&crate::cellfmt::format_code_as_read(code));
        let next = self.codes.len();
        let i = *self.index.entry(key).or_insert(next);
        if i == next {
            self.codes.push(code.to_string());
        }
        i
    }

    pub fn codes(&self) -> &[String] {
        &self.codes
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn custom_slots() {
        let mut t = NumberFormatTable::default();
        assert_eq!(t.slot("yyyy"), 0);
        assert_eq!(t.slot(r#"0.0"x""#), 1);
        assert_eq!(t.slot(r"0.0\x"), 2);
        assert_eq!(t.slot("YYYY"), 0);
        assert_eq!(t.slot(r#"0.0"X""#), 3);
        assert_eq!(t.slot("0.0x"), 2);
        assert_eq!(t.slot("0.0X"), 4);
        assert_eq!(t.codes()[0], "yyyy");
    }

    #[test]
    fn month_year_formats() {
        let loc = Locale::en_nz();

        for f in [
            "mmm-yy",
            "MMM-YY",
            r"mmm\-yy",
            "mmm-y",
            "mmm-yyyy",
            r"mmm\-yyyy",
            "mmm-yyy",
            "mmm-yyyyy",
        ] {
            assert!(is_month_year_format(&loc, f), "{f}");
        }
        for f in [
            "mmmm-yy",
            "mmm yy",
            "mmm yyyy",
            "mmm/yy",
            "mmm-yy;@",
            "mmm\"-\"yy",
            "mmm-yy;",
            "[Red]mmm-yy",
            "[$-409]mmm-yy",
            "d-mmm-yy",
            "d/mm/yyyy h:mm",
            "General",
        ] {
            assert!(!is_month_year_format(&loc, f), "{f}");
        }
    }

    #[test]
    fn slots_and_biff() {
        let loc = Locale::en_nz();
        assert_eq!(ifmt_of_code(&loc, "General"), Some(0));
        assert_eq!(ifmt_of_code(&loc, "h:mm AM/PM"), Some(23));
        assert_eq!(ifmt_of_code(&loc, "$#,##0.00;[Red]-$#,##0.00"), Some(12));
        assert_eq!(ifmt_of_code(&loc, "yyyy"), None);

        for s in 0..IFMT_BASE_MAX {
            assert!(slot_code(&loc, s).is_some());
            let id = file_id_of_slot(s).unwrap();
            assert_eq!(slot_of_file_id(id), Some(s));
        }
        assert_eq!(builtin_file_id_of_code(&loc, "#,##0;-#,##0"), Some(37));
        assert_eq!(builtin_file_id_of_code(&loc, "d/mm/yyyy"), Some(14));
        assert_eq!(builtin_file_id_of_code(&loc, "mm:ss.0"), Some(47));
        assert_eq!(
            builtin_file_id_of_code(&loc, r#"_-* #,##0_-;\-* #,##0_-;_-* "-"_-;_-@_-"#),
            Some(41)
        );
        assert_eq!(
            builtin_file_id_of_code(&loc, r#"_-* #,##0_-;\-* #,##0_-;_-* "-"_-;_@_-"#),
            None
        );
        assert_eq!(builtin_code_of_file_id(&loc, 23), None);
        assert_eq!(
            builtin_code_of_file_id(&loc, 31).as_deref(),
            Some("d/mm/yyyy")
        );
        assert_eq!(
            builtin_code_of_file_id(&loc, 34).as_deref(),
            Some("h:mm:ss")
        );
        assert_eq!(builtin_code_of_file_id(&loc, 59).as_deref(), Some("0"));
        assert_eq!(
            builtin_code_of_file_id(&loc, 80).as_deref(),
            Some("[h]:mm:ss")
        );
    }
}
