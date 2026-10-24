#[derive(Clone, Copy, PartialEq, Eq, Debug)]
pub enum DateOrder {
    MDY,

    DMY,

    YMD,
}

#[derive(Clone, Debug)]
pub struct Locale {
    pub decimal_sep: char,

    pub thousands_sep: char,

    pub date_sep: char,

    pub time_sep: char,

    pub date_order: DateOrder,

    pub currency: String,

    pub short_date_format: String,
}

pub const MONTH_NAMES: [&str; 12] = [
    "January",
    "February",
    "March",
    "April",
    "May",
    "June",
    "July",
    "August",
    "September",
    "October",
    "November",
    "December",
];

pub const DAY_NAMES: [&str; 7] = [
    "Sunday",
    "Monday",
    "Tuesday",
    "Wednesday",
    "Thursday",
    "Friday",
    "Saturday",
];

impl Locale {
    pub fn en_nz() -> Locale {
        Locale {
            decimal_sep: '.',
            thousands_sep: ',',
            date_sep: '/',
            time_sep: ':',
            date_order: DateOrder::DMY,
            currency: "$".into(),
            short_date_format: "d/mm/yyyy".into(),
        }
    }

    pub fn en_us() -> Locale {
        Locale {
            date_order: DateOrder::MDY,
            short_date_format: "m/d/yyyy".into(),
            ..Locale::en_nz()
        }
    }
}

impl Default for Locale {
    fn default() -> Self {
        Locale::en_nz()
    }
}

pub fn month_from_name(s: &str) -> Option<u32> {
    if s.len() < 3 {
        return None;
    }
    MONTH_NAMES
        .iter()
        .position(|m| m.len() >= s.len() && m[..s.len()].eq_ignore_ascii_case(s))
        .map(|i| i as u32 + 1)
}
