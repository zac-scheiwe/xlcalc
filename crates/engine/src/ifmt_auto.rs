use crate::legacy_ftab::FTAB;

pub(crate) const IFMT_CURRENCY: u8 = 12;

pub(crate) const IFMT_PERCENT: u8 = 13;

pub(crate) const IFMT_DATE: u8 = 19;

pub(crate) const IFMT_TIME: u8 = 23;

pub(crate) const IFMT_DATETIME: u8 = 27;

pub(crate) static AUTO_IFMT: [(u8, u8); 18] = [
    (11, IFMT_CURRENCY),
    (56, IFMT_CURRENCY),
    (57, IFMT_CURRENCY),
    (59, IFMT_CURRENCY),
    (60, IFMT_PERCENT),
    (61, IFMT_PERCENT),
    (62, IFMT_PERCENT),
    (65, IFMT_DATE),
    (66, IFMT_TIME),
    (74, IFMT_DATETIME),
    (142, IFMT_CURRENCY),
    (143, IFMT_CURRENCY),
    (144, IFMT_CURRENCY),
    (167, IFMT_CURRENCY),
    (168, IFMT_CURRENCY),
    (221, IFMT_DATE),
    (222, IFMT_CURRENCY),
    (247, IFMT_CURRENCY),
];

pub(crate) static PASS_THROUGH: [u8; 11] = [25, 39, 27, 4, 197, 5, 7, 227, 6, 212, 213];

fn ifunc_of(name: &str) -> Option<u8> {
    FTAB.iter()
        .find(|f| f.name.eq_ignore_ascii_case(name))
        .and_then(|f| u8::try_from(f.ifunc).ok())
}

pub(crate) fn auto_ifmt(name: &str) -> Option<u8> {
    let id = ifunc_of(name)?;
    AUTO_IFMT.iter().find(|(f, _)| *f == id).map(|(_, i)| *i)
}

pub(crate) fn passes_through(name: &str) -> bool {
    ifunc_of(name).is_some_and(|id| PASS_THROUGH.contains(&id))
}

pub(crate) fn is_sum(name: &str) -> bool {
    ifunc_of(name) == Some(4)
}
