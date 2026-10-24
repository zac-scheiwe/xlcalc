use crate::value::XlError;

pub struct CalNames {
    pub months: [&'static str; 13],
    pub abbr: [&'static str; 13],
    pub initial: [&'static str; 13],
    pub days: [&'static str; 7],
    pub days_abbr: [&'static str; 7],
}

pub fn names(cal: u8, lang: u32) -> Option<&'static CalNames> {
    match cal {
        0x06 | 0x17 if lang != 0x0409 => table(cal, 0),

        0x07 | 0x09 | 0x0A | 0x0B | 0x0C => table(cal, 0),
        _ if lang == 0 || lang == 0x0409 => table(cal, lang),
        _ => None,
    }
}

pub fn month_name(n: &'static CalNames, hebrew_leap: bool, m: u32, kind: u8) -> &'static str {
    let pick = |i: usize| match kind {
        3 => n.abbr[i],
        5 => n.initial[i],
        _ => n.months[i],
    };
    if !hebrew_leap || m < 6 || n.months[5] != "Adar" {
        return if m > 13 { "" } else { pick(m as usize - 1) };
    }
    match m {
        6 => "AdarI",
        7 => "AdarII",
        _ => pick(m as usize - 2),
    }
}

pub fn converts(cal: u8) -> bool {
    matches!(
        cal,
        0x06 | 0x08
            | 0x09
            | 0x0A
            | 0x0B
            | 0x0C
            | 0x0E
            | 0x0F
            | 0x10
            | 0x11
            | 0x12
            | 0x13
            | 0x16
            | 0x17
    )
}

pub fn year_offset(cal: u8) -> i64 {
    match cal {
        0x05 => 2333,
        0x07 => 543,
        _ => 0,
    }
}

fn civil(ms: i64) -> (i64, u32, u32) {
    let z = ms - 25569 + 719468;
    let era = z.div_euclid(146097);
    let doe = z - era * 146097;
    let yoe = (doe - doe / 1460 + doe / 36524 - doe / 146096) / 365;
    let doy = doe - (365 * yoe + yoe / 4 - yoe / 100);
    let mp = (5 * doy + 2) / 153;
    let d = (doy - (153 * mp + 2) / 5 + 1) as u32;
    let m = if mp < 10 { mp + 3 } else { mp - 9 } as u32;
    let y = yoe + era * 400 + if m <= 2 { 1 } else { 0 };
    (y, m, d)
}

fn days_from_civil(y: i64, m: u32, d: u32) -> i64 {
    let y = if m <= 2 { y - 1 } else { y };
    let era = y.div_euclid(400);
    let yoe = y - era * 400;
    let mp = if m > 2 { m - 3 } else { m + 9 } as i64;
    let doy = (153 * mp + 2) / 5 + d as i64 - 1;
    let doe = yoe * 365 + yoe / 4 - yoe / 100 + doy;
    era * 146097 + doe - 719468 + 25569
}

fn greg_leap(y: i64) -> bool {
    (y % 4 == 0 && y % 100 != 0) || y % 400 == 0
}

pub fn convert(cal: u8, serial: i64) -> Result<(i64, u32, u32, bool), XlError> {
    if serial == 60 || serial < 0 {
        return Err(XlError::Value);
    }
    let s = serial.max(1);

    let ms = if s < 60 { s + 1 } else { s };
    let tabular = || {
        crate::datetime::hijri_from_serial(s)
            .map(|(y, m, d)| (y, m, d, false))
            .ok_or(XlError::Value)
    };
    match cal {
        0x06 => tabular(),
        0x17 => um_al_qura(ms).map_or_else(tabular, Ok),
        0x08 if ms >= super::calendars_he::START => {
            Ok(super::calendars_he::from_serial(ms.min(2_958_465)))
        }
        0x08 => Ok(hebrew(ms + 693_594)),
        0x10 => Ok(saka(ms)),
        0x16 => Ok(persian(ms)),
        0x0E | 0x0F | 0x11 | 0x12 | 0x13 => {
            let (y, m, d) = super::calendars_cn::from_serial(ms.min(2_958_465));
            Ok((y, m, d, false))
        }
        _ => {
            let (y, m, d) = civil(ms);
            Ok((y, m, d, false))
        }
    }
}

fn um_al_qura(ms: i64) -> Option<(i64, u32, u32, bool)> {
    if !(62..UAQ_END).contains(&ms) {
        return None;
    }
    let (mut y, mut m, mut cur) = (1317i64, 11u32, 62i64);
    for i in 0..UAQ_MONTHS {
        let len = if (UAQ_BITS[i / 8] >> (i % 8)) & 1 == 1 {
            30
        } else {
            29
        };
        if ms < cur + len {
            return Some((y, m, (ms - cur + 1) as u32, false));
        }
        cur += len;
        m += 1;
        if m == 13 {
            m = 1;
            y += 1;
        }
    }
    None
}

fn saka(ms: i64) -> (i64, u32, u32, bool) {
    let (gy, _, _) = civil(ms);
    let start = |y: i64| days_from_civil(y, 3, if greg_leap(y) { 21 } else { 22 });
    let y = if ms < start(gy) { gy - 1 } else { gy };
    let mut d = ms - start(y);
    let first = if greg_leap(y) { 31 } else { 30 };
    let lens = [first, 31, 31, 31, 31, 31, 30, 30, 30, 30, 30, 30];
    let mut m = 1;
    for l in lens {
        if d < l {
            break;
        }
        d -= l;
        m += 1;
    }
    (y - 78, m, d as u32 + 1, false)
}

fn persian(ms: i64) -> (i64, u32, u32, bool) {
    let (mut y, mut start) = (1278i64, PERSIAN_START);
    let n = PERSIAN_LEAP.len() * 8;
    for i in 0..n {
        let leap = (PERSIAN_LEAP[i / 8] >> (i % 8)) & 1 == 1;
        let len = if leap { 366 } else { 365 };
        if ms < start + len || i + 1 == n {
            break;
        }
        start += len;
        y += 1;
    }
    let d = ms - start;
    let (m, dd) = if d < 186 {
        (d / 31 + 1, d % 31 + 1)
    } else {
        let k = ((d - 186) / 30).min(5);
        (7 + k, d - 186 - 30 * k + 1)
    };
    (y, m as u32, dd as u32, false)
}

fn heb_elapsed(y: i64) -> i64 {
    let months = (235 * y - 234).div_euclid(19);
    let parts = 12084 + 13753 * months;
    let mut day = 29 * months + parts.div_euclid(25920);
    if (3 * (day + 1)).rem_euclid(7) < 3 {
        day += 1;
    }
    day
}

fn heb_new_year(y: i64) -> i64 {
    let corr = if heb_elapsed(y + 1) - heb_elapsed(y) == 356 {
        2
    } else if heb_elapsed(y) - heb_elapsed(y - 1) == 382 {
        1
    } else {
        0
    };
    -1_373_427 + heb_elapsed(y) + corr
}

fn heb_leap(y: i64) -> bool {
    (7 * y + 1).rem_euclid(19) < 7
}

fn hebrew(rd: i64) -> (i64, u32, u32, bool) {
    let mut y = (rd + 1_373_427) * 98_496 / 35_975_351 - 1;
    while heb_new_year(y + 1) <= rd {
        y += 1;
    }
    let len = heb_new_year(y + 1) - heb_new_year(y);
    let leap = heb_leap(y);
    let mut lens = vec![
        30,
        29 + (len % 10 == 5) as i64,
        30 - (len % 10 == 3) as i64,
        29,
        30,
    ];
    if leap {
        lens.extend([30, 29]);
    } else {
        lens.push(29);
    }
    lens.extend([30, 29, 30, 29, 30, 29]);
    let mut d = rd - heb_new_year(y);
    let mut m = 1;
    for l in lens {
        if d < l {
            break;
        }
        d -= l;
        m += 1;
    }
    (y, m, d as u32 + 1, leap)
}

const UAQ_BITS: [u8; 339] = [
    169, 75, 186, 73, 59, 169, 43, 181, 165, 218, 84, 173, 170, 54, 233, 38, 237, 164, 174, 212,
    149, 218, 82, 173, 170, 86, 85, 171, 180, 149, 186, 82, 87, 178, 75, 181, 169, 90, 85, 173,
    210, 86, 234, 74, 93, 201, 46, 217, 37, 93, 149, 173, 202, 86, 105, 43, 181, 37, 187, 100, 151,
    116, 147, 182, 74, 91, 169, 173, 212, 150, 234, 146, 94, 210, 77, 218, 74, 93, 169, 181, 84,
    91, 106, 75, 118, 73, 55, 105, 43, 117, 165, 182, 84, 171, 170, 53, 217, 38, 221, 164, 157,
    212, 149, 218, 82, 173, 170, 86, 85, 171, 116, 147, 118, 82, 87, 106, 75, 117, 169, 86, 85,
    171, 170, 85, 218, 74, 93, 169, 43, 217, 37, 91, 85, 173, 170, 86, 105, 43, 117, 165, 182, 100,
    151, 108, 147, 174, 210, 86, 105, 173, 212, 149, 218, 74, 47, 241, 38, 110, 165, 182, 84, 107,
    169, 45, 181, 37, 183, 100, 155, 170, 85, 186, 74, 91, 169, 77, 213, 170, 106, 83, 118, 82, 87,
    234, 74, 109, 169, 86, 85, 107, 170, 75, 186, 73, 87, 105, 43, 181, 149, 90, 85, 85, 85, 85,
    87, 85, 85, 87, 85, 85, 85, 117, 85, 85, 85, 85, 87, 85, 85, 85, 117, 85, 85, 117, 85, 85, 85,
    85, 87, 85, 85, 85, 117, 85, 85, 85, 85, 87, 85, 85, 87, 85, 85, 85, 117, 85, 85, 85, 85, 87,
    85, 85, 87, 85, 85, 85, 117, 85, 85, 85, 85, 87, 85, 85, 85, 117, 85, 85, 117, 85, 85, 85, 85,
    87, 85, 85, 85, 117, 85, 85, 85, 85, 87, 85, 85, 87, 85, 85, 85, 117, 85, 85, 85, 85, 87, 85,
    85, 87, 85, 85, 85, 117, 85, 85, 85, 85, 87, 85, 85, 85, 117, 85, 85, 117, 85, 85, 85, 85, 87,
    85, 85, 85, 117, 85, 85, 85, 85, 87, 85, 85, 87, 85, 85, 85, 117, 85, 85, 85, 1,
];
const UAQ_END: i64 = 79971;
const UAQ_MONTHS: usize = 2706;

const PERSIAN_LEAP: [u8; 1013] = [
    68, 68, 68, 132, 136, 136, 136, 8, 17, 17, 17, 17, 34, 34, 34, 34, 68, 68, 68, 68, 136, 136,
    136, 136, 16, 17, 17, 17, 34, 34, 34, 34, 68, 68, 68, 68, 132, 136, 136, 136, 16, 17, 17, 17,
    33, 34, 34, 34, 66, 68, 68, 68, 132, 136, 136, 136, 8, 17, 17, 17, 17, 34, 34, 34, 34, 68, 68,
    68, 68, 136, 136, 136, 136, 16, 17, 17, 17, 33, 34, 34, 34, 66, 68, 68, 68, 132, 136, 136, 136,
    8, 17, 17, 17, 33, 34, 34, 34, 66, 68, 68, 68, 132, 136, 136, 136, 136, 16, 17, 17, 17, 34, 34,
    34, 34, 68, 68, 68, 68, 136, 136, 136, 136, 16, 17, 17, 17, 33, 34, 34, 34, 66, 68, 68, 68,
    132, 136, 136, 136, 8, 17, 17, 17, 17, 34, 34, 34, 34, 68, 68, 68, 68, 136, 136, 136, 136, 16,
    17, 17, 17, 33, 34, 34, 34, 68, 68, 68, 68, 136, 136, 136, 136, 8, 17, 17, 17, 17, 34, 34, 34,
    66, 68, 68, 68, 132, 136, 136, 136, 8, 17, 17, 17, 17, 33, 34, 34, 34, 68, 68, 68, 68, 136,
    136, 136, 136, 16, 17, 17, 17, 33, 34, 34, 34, 66, 68, 68, 68, 132, 136, 136, 136, 8, 17, 17,
    17, 17, 34, 34, 34, 34, 68, 68, 68, 132, 136, 136, 136, 136, 16, 17, 17, 17, 33, 34, 34, 34,
    66, 68, 68, 68, 136, 136, 136, 136, 16, 17, 17, 17, 33, 34, 34, 34, 66, 68, 68, 68, 132, 136,
    136, 136, 8, 17, 17, 17, 17, 34, 34, 34, 34, 68, 68, 68, 68, 136, 136, 136, 136, 16, 17, 17,
    17, 33, 34, 34, 34, 66, 68, 68, 68, 136, 136, 136, 136, 16, 17, 17, 17, 33, 34, 34, 34, 66, 68,
    68, 68, 132, 136, 136, 136, 8, 17, 17, 17, 17, 34, 34, 34, 34, 68, 68, 68, 132, 136, 136, 136,
    8, 17, 17, 17, 17, 34, 34, 34, 34, 68, 68, 68, 68, 136, 136, 136, 136, 16, 17, 17, 17, 33, 34,
    34, 34, 66, 68, 68, 68, 136, 136, 136, 136, 16, 17, 17, 17, 33, 34, 34, 34, 66, 68, 68, 68,
    132, 136, 136, 136, 8, 17, 17, 17, 17, 34, 34, 34, 34, 68, 68, 68, 132, 136, 136, 136, 8, 17,
    17, 17, 17, 34, 34, 34, 34, 68, 68, 68, 68, 136, 136, 136, 136, 16, 17, 17, 17, 33, 34, 34, 34,
    68, 68, 68, 68, 136, 136, 136, 136, 16, 17, 17, 17, 33, 34, 34, 34, 68, 68, 68, 68, 136, 136,
    136, 136, 16, 17, 17, 17, 33, 34, 34, 34, 66, 68, 68, 68, 132, 136, 136, 136, 8, 17, 17, 17,
    33, 34, 34, 34, 66, 68, 68, 68, 132, 136, 136, 136, 8, 17, 17, 17, 33, 34, 34, 34, 66, 68, 68,
    68, 132, 136, 136, 136, 8, 17, 17, 17, 33, 34, 34, 34, 66, 68, 68, 68, 132, 136, 136, 136, 8,
    17, 17, 17, 33, 34, 34, 34, 66, 68, 68, 68, 132, 136, 136, 136, 16, 17, 17, 17, 33, 34, 34, 34,
    66, 68, 68, 68, 136, 136, 136, 136, 16, 17, 17, 17, 33, 34, 34, 34, 66, 68, 68, 68, 136, 136,
    136, 136, 16, 17, 17, 17, 33, 34, 34, 34, 68, 68, 68, 68, 136, 136, 136, 136, 16, 17, 17, 17,
    34, 34, 34, 34, 68, 68, 68, 68, 136, 136, 136, 8, 17, 17, 17, 17, 34, 34, 34, 66, 68, 68, 68,
    132, 136, 136, 136, 16, 17, 17, 17, 33, 34, 34, 34, 66, 68, 68, 68, 136, 136, 136, 136, 16, 17,
    17, 17, 33, 34, 34, 34, 68, 68, 68, 68, 136, 136, 136, 136, 16, 17, 17, 17, 34, 34, 34, 66, 68,
    68, 68, 132, 136, 136, 136, 8, 17, 17, 17, 33, 34, 34, 34, 66, 68, 68, 68, 132, 136, 136, 136,
    16, 17, 17, 17, 34, 34, 34, 34, 68, 68, 68, 132, 136, 136, 136, 8, 17, 17, 17, 17, 34, 34, 34,
    66, 68, 68, 68, 132, 136, 136, 136, 16, 17, 17, 17, 34, 34, 34, 34, 68, 68, 68, 68, 136, 136,
    136, 8, 17, 17, 17, 33, 34, 34, 34, 66, 68, 68, 68, 136, 136, 136, 136, 16, 17, 17, 17, 34, 34,
    34, 66, 68, 68, 68, 132, 136, 136, 136, 16, 17, 17, 17, 33, 34, 34, 34, 68, 68, 68, 132, 136,
    136, 136, 8, 17, 17, 17, 33, 34, 34, 34, 66, 68, 68, 68, 136, 136, 136, 8, 17, 17, 17, 33, 34,
    34, 34, 66, 68, 68, 68, 136, 136, 136, 136, 16, 17, 17, 17, 34, 34, 34, 66, 68, 68, 68, 132,
    136, 136, 136, 16, 17, 17, 17, 34, 34, 34, 34, 68, 68, 68, 132, 136, 136, 136, 16, 17, 17, 17,
    33, 34, 34, 34, 68, 68, 68, 132, 136, 136, 136, 16, 17, 17, 17, 34, 34, 34, 34, 68, 68, 68,
    132, 136, 136, 136, 16, 17, 17, 17, 33, 34, 34, 34, 68, 68, 68, 132, 136, 136, 136, 16, 17, 17,
    17, 34, 34, 34, 34, 68, 68, 68, 132, 136, 136, 136, 16, 17, 17, 17, 34, 34, 34, 34, 68, 68, 68,
    132, 136, 136, 136, 16, 17, 17, 17, 34, 34, 34, 66, 68, 68, 68, 136, 136, 136, 136, 16, 17, 17,
    17, 34, 34, 34, 66, 68, 68, 68, 136, 136, 136, 8, 17, 17, 17, 33, 34, 34, 34, 68, 68, 68, 132,
    136, 136, 136, 16, 17, 17, 17, 33, 34, 34, 34, 68, 68, 68, 132, 136, 136, 136, 16, 17, 17, 17,
    34, 34, 34, 66, 68, 68, 68, 136, 136, 136, 8, 17, 17, 17, 33, 34, 34, 34, 68, 68, 68, 132, 136,
    136, 136, 16, 17, 17, 17, 34, 34, 34, 66, 68, 68, 68, 136, 136, 136, 8, 17, 17, 17, 33, 34, 34,
    34, 68, 68, 68, 132, 8,
];
const PERSIAN_START: i64 = -284;

static N_03_0000: CalNames = CalNames {
    months: [
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
        "",
    ],
    abbr: [
        "Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sept", "Oct", "Nov", "Dec", "",
    ],
    initial: [
        "J", "F", "M", "A", "M", "J", "J", "A", "S", "O", "N", "D", "",
    ],
    days: [
        "Sunday",
        "Monday",
        "Tuesday",
        "Wednesday",
        "Thursday",
        "Friday",
        "Saturday",
    ],
    days_abbr: ["Sun", "Mon", "Tue", "Wed", "Thu", "Fri", "Sat"],
};
static N_03_0409: CalNames = CalNames {
    months: [
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
        "",
    ],
    abbr: [
        "Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sept", "Oct", "Nov", "Dec", "",
    ],
    initial: [
        "J", "F", "M", "A", "M", "J", "J", "A", "S", "O", "N", "D", "",
    ],
    days: [
        "Sunday",
        "Monday",
        "Tuesday",
        "Wednesday",
        "Thursday",
        "Friday",
        "Saturday",
    ],
    days_abbr: ["Sun", "Mon", "Tue", "Wed", "Thu", "Fri", "Sat"],
};
static N_04_0000: CalNames = CalNames {
    months: [
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
        "",
    ],
    abbr: [
        "Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sept", "Oct", "Nov", "Dec", "",
    ],
    initial: [
        "J", "F", "M", "A", "M", "J", "J", "A", "S", "O", "N", "D", "",
    ],
    days: [
        "Sunday",
        "Monday",
        "Tuesday",
        "Wednesday",
        "Thursday",
        "Friday",
        "Saturday",
    ],
    days_abbr: ["Sun", "Mon", "Tue", "Wed", "Thu", "Fri", "Sat"],
};
static N_04_0409: CalNames = CalNames {
    months: [
        "一月",
        "二月",
        "三月",
        "四月",
        "五月",
        "六月",
        "七月",
        "八月",
        "九月",
        "十月",
        "十一月",
        "十二月",
        "",
    ],
    abbr: [
        "一月",
        "二月",
        "三月",
        "四月",
        "五月",
        "六月",
        "七月",
        "八月",
        "九月",
        "十月",
        "十一月",
        "十二月",
        "",
    ],
    initial: [
        "一", "二", "三", "四", "五", "六", "七", "八", "九", "十", "十", "十", "",
    ],
    days: [
        "星期日",
        "星期一",
        "星期二",
        "星期三",
        "星期四",
        "星期五",
        "星期六",
    ],
    days_abbr: ["週日", "週一", "週二", "週三", "週四", "週五", "週六"],
};
static N_05_0000: CalNames = CalNames {
    months: [
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
        "",
    ],
    abbr: [
        "Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sept", "Oct", "Nov", "Dec", "",
    ],
    initial: [
        "J", "F", "M", "A", "M", "J", "J", "A", "S", "O", "N", "D", "",
    ],
    days: [
        "Sunday",
        "Monday",
        "Tuesday",
        "Wednesday",
        "Thursday",
        "Friday",
        "Saturday",
    ],
    days_abbr: ["Sun", "Mon", "Tue", "Wed", "Thu", "Fri", "Sat"],
};
static N_05_0409: CalNames = CalNames {
    months: [
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
        "",
    ],
    abbr: [
        "Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sept", "Oct", "Nov", "Dec", "",
    ],
    initial: [
        "J", "F", "M", "A", "M", "J", "J", "A", "S", "O", "N", "D", "",
    ],
    days: [
        "Sunday",
        "Monday",
        "Tuesday",
        "Wednesday",
        "Thursday",
        "Friday",
        "Saturday",
    ],
    days_abbr: ["Sun", "Mon", "Tue", "Wed", "Thu", "Fri", "Sat"],
};
static N_06_0000: CalNames = CalNames {
    months: [
        "محرم",
        "صفر",
        "ربيع\u{a0}الأول",
        "ربيع\u{a0}الثاني",
        "جمادى\u{a0}الأولى",
        "جمادى\u{a0}الثانية",
        "رجب",
        "شعبان",
        "رمضان",
        "شوال",
        "ذو\u{a0}القعدة",
        "ذو\u{a0}الحجة",
        "",
    ],
    abbr: [
        "محرم",
        "صفر",
        "ربيع\u{a0}الأول",
        "ربيع\u{a0}الثاني",
        "جمادى\u{a0}الأولى",
        "جمادى\u{a0}الثانية",
        "رجب",
        "شعبان",
        "رمضان",
        "شوال",
        "ذو\u{a0}القعدة",
        "ذو\u{a0}الحجة",
        "",
    ],
    initial: [
        "م", "ص", "ر", "ر", "ج", "ج", "ر", "ش", "ر", "ش", "ذ", "ذ", "",
    ],
    days: [
        "الأحد",
        "الإثنين",
        "الثلاثاء",
        "الأربعاء",
        "الخميس",
        "الجمعة",
        "السبت",
    ],
    days_abbr: [
        "الأحد",
        "الإثنين",
        "الثلاثاء",
        "الأربعاء",
        "الخميس",
        "الجمعة",
        "السبت",
    ],
};
static N_06_0409: CalNames = CalNames {
    months: [
        "Muharram",
        "Safar",
        "Rabi' Awwal",
        "Rabi' Thani",
        "Jamada El Oula",
        "Jamada El Thaniah",
        "Rajab",
        "Sha'ban",
        "Ramadan",
        "Shawwal",
        "Thoul Ki'dah",
        "Thoul Hijjah",
        "",
    ],
    abbr: [
        "Muharram",
        "Safar",
        "Rabi' Awwal",
        "Rabi' Thani",
        "Jamada El Oula",
        "Jamada El Thaniah",
        "Rajab",
        "Sha'ban",
        "Ramadan",
        "Shawwal",
        "Thoul Ki'dah",
        "Thoul Hijjah",
        "",
    ],
    initial: [
        "M", "S", "R", "R", "J", "J", "R", "S", "R", "S", "T", "T", "",
    ],
    days: [
        "AlAhad",
        "AlEthnien",
        "AthThulatha",
        "AlArbia'a",
        "AlKhamis",
        "AlJumaa",
        "AsSabt",
    ],
    days_abbr: [
        "AlAhad",
        "AlEthnien",
        "AthThulatha",
        "AlArbia'a",
        "AlKhamis",
        "AlJumaa",
        "AsSabt",
    ],
};
static N_07_0000: CalNames = CalNames {
    months: [
        "มกราคม",
        "กุมภาพันธ์",
        "มีนาคม",
        "เมษายน",
        "พฤษภาคม",
        "มิถุนายน",
        "กรกฎาคม",
        "สิงหาคม",
        "กันยายน",
        "ตุลาคม",
        "พฤศจิกายน",
        "ธันวาคม",
        "",
    ],
    abbr: [
        "ม.ค.",
        "ก.พ.",
        "มี.ค.",
        "เม.ย.",
        "พ.ค.",
        "มิ.ย.",
        "ก.ค.",
        "ส.ค.",
        "ก.ย.",
        "ต.ค.",
        "พ.ย.",
        "ธ.ค.",
        "",
    ],
    initial: [
        "ม", "ก", "ม", "เ", "พ", "ม", "ก", "ส", "ก", "ต", "พ", "ธ", "",
    ],
    days: ["อาทิตย์", "จันทร์", "อังคาร", "พุธ", "พฤหัสบดี", "ศุกร์", "เสาร์"],
    days_abbr: ["อา.", "จ.", "อ.", "พ.", "พฤ.", "ศ.", "ส."],
};
static N_07_0409: CalNames = CalNames {
    months: [
        "มกราคม",
        "กุมภาพันธ์",
        "มีนาคม",
        "เมษายน",
        "พฤษภาคม",
        "มิถุนายน",
        "กรกฎาคม",
        "สิงหาคม",
        "กันยายน",
        "ตุลาคม",
        "พฤศจิกายน",
        "ธันวาคม",
        "",
    ],
    abbr: [
        "ม.ค.",
        "ก.พ.",
        "มี.ค.",
        "เม.ย.",
        "พ.ค.",
        "มิ.ย.",
        "ก.ค.",
        "ส.ค.",
        "ก.ย.",
        "ต.ค.",
        "พ.ย.",
        "ธ.ค.",
        "",
    ],
    initial: [
        "ม", "ก", "ม", "เ", "พ", "ม", "ก", "ส", "ก", "ต", "พ", "ธ", "",
    ],
    days: ["อาทิตย์", "จันทร์", "อังคาร", "พุธ", "พฤหัสบดี", "ศุกร์", "เสาร์"],
    days_abbr: ["อา.", "จ.", "อ.", "พ.", "พฤ.", "ศ.", "ส."],
};
static N_08_0000: CalNames = CalNames {
    months: [
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
        "",
    ],
    abbr: [
        "Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sept", "Oct", "Nov", "Dec", "",
    ],
    initial: [
        "J", "F", "M", "A", "M", "J", "J", "A", "S", "O", "N", "D", "N",
    ],
    days: [
        "Sunday",
        "Monday",
        "Tuesday",
        "Wednesday",
        "Thursday",
        "Friday",
        "Saturday",
    ],
    days_abbr: ["Sun", "Mon", "Tue", "Wed", "Thu", "Fri", "Sat"],
};
static N_08_0409: CalNames = CalNames {
    months: [
        "Tishrei", "Cheshvan", "Kislev", "Tevet", "Shvat", "Adar", "Nisan", "Iyar", "Sivan",
        "Tamuz", "Av", "Elul", "Elul",
    ],
    abbr: [
        "Tishrei", "Cheshvan", "Kislev", "Tevet", "Shvat", "Adar", "Nisan", "Iyar", "Sivan",
        "Tamuz", "Av", "Elul", "Elul",
    ],
    initial: [
        "Tishrei", "Cheshvan", "Kislev", "Tevet", "Shvat", "Adar", "Nisan", "Iyar", "Sivan",
        "Tamuz", "Av", "Elul", "Elul",
    ],
    days: [
        "Yom Rishon",
        "Yom Sheni",
        "Yom Shlishi",
        "Yom Revi'i",
        "Yom Chamishi",
        "Yom Shishi",
        "Shabat",
    ],
    days_abbr: [
        "Yom Rishon",
        "Yom Sheni",
        "Yom Shlishi",
        "Yom Revi'i",
        "Yom Chamishi",
        "Yom Shishi",
        "Shabat",
    ],
};
static N_09_0000: CalNames = CalNames {
    months: [
        "janvier",
        "février",
        "mars",
        "avril",
        "mai",
        "juin",
        "juillet",
        "août",
        "septembre",
        "octobre",
        "novembre",
        "décembre",
        "",
    ],
    abbr: [
        "janv.", "févr.", "mars", "avr.", "mai", "juin", "juil.", "août", "sept.", "oct.", "nov.",
        "déc.", "",
    ],
    initial: [
        "j", "f", "m", "a", "m", "j", "j", "a", "s", "o", "n", "d", "",
    ],
    days: [
        "dimanche", "lundi", "mardi", "mercredi", "jeudi", "vendredi", "samedi",
    ],
    days_abbr: ["dim.", "lun.", "mar.", "mer.", "jeu.", "ven.", "sam."],
};
static N_09_0409: CalNames = CalNames {
    months: [
        "janvier",
        "février",
        "mars",
        "avril",
        "mai",
        "juin",
        "juillet",
        "août",
        "septembre",
        "octobre",
        "novembre",
        "décembre",
        "",
    ],
    abbr: [
        "janv.", "févr.", "mars", "avr.", "mai", "juin", "juil.", "août", "sept.", "oct.", "nov.",
        "déc.", "",
    ],
    initial: [
        "j", "f", "m", "a", "m", "j", "j", "a", "s", "o", "n", "d", "",
    ],
    days: [
        "dimanche", "lundi", "mardi", "mercredi", "jeudi", "vendredi", "samedi",
    ],
    days_abbr: ["dim.", "lun.", "mar.", "mer.", "jeu.", "ven.", "sam."],
};
static N_0A_0000: CalNames = CalNames {
    months: [
        "كانون\u{a0}الثاني",
        "شباط",
        "آذار",
        "نيسان",
        "أيار",
        "حزيران",
        "تموز",
        "آب",
        "أيلول",
        "تشرين\u{a0}الأول",
        "تشرين\u{a0}الثاني",
        "كانون\u{a0}الأول",
        "",
    ],
    abbr: [
        "كانون\u{a0}الثاني",
        "شباط",
        "آذار",
        "نيسان",
        "أيار",
        "حزيران",
        "تموز",
        "آب",
        "أيلول",
        "تشرين\u{a0}الأول",
        "تشرين\u{a0}الثاني",
        "كانون\u{a0}الأول",
        "",
    ],
    initial: [
        "ك", "ش", "آ", "ن", "أ", "ح", "ت", "آ", "أ", "ت", "ت", "ك", "",
    ],
    days: [
        "الأحد",
        "الإثنين",
        "الثلاثاء",
        "الأربعاء",
        "الخميس",
        "الجمعة",
        "السبت",
    ],
    days_abbr: [
        "الأحد",
        "الإثنين",
        "الثلاثاء",
        "الأربعاء",
        "الخميس",
        "الجمعة",
        "السبت",
    ],
};
static N_0A_0409: CalNames = CalNames {
    months: [
        "كانون\u{a0}الثاني",
        "شباط",
        "آذار",
        "نيسان",
        "أيار",
        "حزيران",
        "تموز",
        "آب",
        "أيلول",
        "تشرين\u{a0}الأول",
        "تشرين\u{a0}الثاني",
        "كانون\u{a0}الأول",
        "",
    ],
    abbr: [
        "كانون\u{a0}الثاني",
        "شباط",
        "آذار",
        "نيسان",
        "أيار",
        "حزيران",
        "تموز",
        "آب",
        "أيلول",
        "تشرين\u{a0}الأول",
        "تشرين\u{a0}الثاني",
        "كانون\u{a0}الأول",
        "",
    ],
    initial: [
        "ك", "ش", "آ", "ن", "أ", "ح", "ت", "آ", "أ", "ت", "ت", "ك", "",
    ],
    days: [
        "الأحد",
        "الإثنين",
        "الثلاثاء",
        "الأربعاء",
        "الخميس",
        "الجمعة",
        "السبت",
    ],
    days_abbr: [
        "الأحد",
        "الإثنين",
        "الثلاثاء",
        "الأربعاء",
        "الخميس",
        "الجمعة",
        "السبت",
    ],
};
static N_0B_0000: CalNames = CalNames {
    months: [
        "يناير",
        "فبراير",
        "مارس",
        "أبريل",
        "مايو",
        "يونيو",
        "يوليو",
        "أغسطس",
        "سبتمبر",
        "أكتوبر",
        "نوفمبر",
        "ديسمبر",
        "",
    ],
    abbr: [
        "يناير",
        "فبراير",
        "مارس",
        "أبريل",
        "مايو",
        "يونيو",
        "يوليو",
        "أغسطس",
        "سبتمبر",
        "أكتوبر",
        "نوفمبر",
        "ديسمبر",
        "",
    ],
    initial: [
        "ي", "ف", "م", "أ", "م", "ي", "ي", "أ", "س", "أ", "ن", "د", "",
    ],
    days: [
        "الأحد",
        "الإثنين",
        "الثلاثاء",
        "الأربعاء",
        "الخميس",
        "الجمعة",
        "السبت",
    ],
    days_abbr: [
        "الأحد",
        "الإثنين",
        "الثلاثاء",
        "الأربعاء",
        "الخميس",
        "الجمعة",
        "السبت",
    ],
};
static N_0B_0409: CalNames = CalNames {
    months: [
        "يناير",
        "فبراير",
        "مارس",
        "أبريل",
        "مايو",
        "يونيو",
        "يوليو",
        "أغسطس",
        "سبتمبر",
        "أكتوبر",
        "نوفمبر",
        "ديسمبر",
        "",
    ],
    abbr: [
        "يناير",
        "فبراير",
        "مارس",
        "أبريل",
        "مايو",
        "يونيو",
        "يوليو",
        "أغسطس",
        "سبتمبر",
        "أكتوبر",
        "نوفمبر",
        "ديسمبر",
        "",
    ],
    initial: [
        "ي", "ف", "م", "أ", "م", "ي", "ي", "أ", "س", "أ", "ن", "د", "",
    ],
    days: [
        "الأحد",
        "الإثنين",
        "الثلاثاء",
        "الأربعاء",
        "الخميس",
        "الجمعة",
        "السبت",
    ],
    days_abbr: [
        "الأحد",
        "الإثنين",
        "الثلاثاء",
        "الأربعاء",
        "الخميس",
        "الجمعة",
        "السبت",
    ],
};
static N_0C_0000: CalNames = CalNames {
    months: [
        "يناير",
        "فبراير",
        "مارس",
        "أبريل",
        "مايو",
        "يونيو",
        "يوليو",
        "أغسطس",
        "سبتمبر",
        "أكتوبر",
        "نوفمبر",
        "ديسمبر",
        "",
    ],
    abbr: [
        "يناير",
        "فبراير",
        "مارس",
        "أبريل",
        "مايو",
        "يونيو",
        "يوليو",
        "أغسطس",
        "سبتمبر",
        "أكتوبر",
        "نوفمبر",
        "ديسمبر",
        "",
    ],
    initial: [
        "ي", "ف", "م", "أ", "م", "ي", "ي", "أ", "س", "أ", "ن", "د", "",
    ],
    days: [
        "الأحد",
        "الإثنين",
        "الثلاثاء",
        "الأربعاء",
        "الخميس",
        "الجمعة",
        "السبت",
    ],
    days_abbr: [
        "الأحد",
        "الإثنين",
        "الثلاثاء",
        "الأربعاء",
        "الخميس",
        "الجمعة",
        "السبت",
    ],
};
static N_0C_0409: CalNames = CalNames {
    months: [
        "يناير",
        "فبراير",
        "مارس",
        "أبريل",
        "مايو",
        "يونيو",
        "يوليو",
        "أغسطس",
        "سبتمبر",
        "أكتوبر",
        "نوفمبر",
        "ديسمبر",
        "",
    ],
    abbr: [
        "يناير",
        "فبراير",
        "مارس",
        "أبريل",
        "مايو",
        "يونيو",
        "يوليو",
        "أغسطس",
        "سبتمبر",
        "أكتوبر",
        "نوفمبر",
        "ديسمبر",
        "",
    ],
    initial: [
        "ي", "ف", "م", "أ", "م", "ي", "ي", "أ", "س", "أ", "ن", "د", "",
    ],
    days: [
        "الأحد",
        "الإثنين",
        "الثلاثاء",
        "الأربعاء",
        "الخميس",
        "الجمعة",
        "السبت",
    ],
    days_abbr: [
        "الأحد",
        "الإثنين",
        "الثلاثاء",
        "الأربعاء",
        "الخميس",
        "الجمعة",
        "السبت",
    ],
};
static N_10_0000: CalNames = CalNames {
    months: [
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
        "",
    ],
    abbr: [
        "Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sept", "Oct", "Nov", "Dec", "",
    ],
    initial: [
        "J", "F", "M", "A", "M", "J", "J", "A", "S", "O", "N", "D", "",
    ],
    days: [
        "Sunday",
        "Monday",
        "Tuesday",
        "Wednesday",
        "Thursday",
        "Friday",
        "Saturday",
    ],
    days_abbr: ["Sun", "Mon", "Tue", "Wed", "Thu", "Fri", "Sat"],
};
static N_10_0409: CalNames = CalNames {
    months: [
        "Chaitra",
        "Vaisakha",
        "Jyaistha",
        "Asadha",
        "Sravana",
        "Bhadra",
        "Asvina",
        "Karthika",
        "Agrahayana",
        "Pausa",
        "Magha",
        "Phalguna",
        "",
    ],
    abbr: [
        "Chaitra",
        "Vaisakha",
        "Jyaistha",
        "Asadha",
        "Sravana",
        "Bhadra",
        "Asvina",
        "Karthika",
        "Agrahayana",
        "Pausa",
        "Magha",
        "Phalguna",
        "",
    ],
    initial: [
        "C", "V", "J", "A", "S", "B", "A", "K", "A", "P", "M", "P", "",
    ],
    days: [
        "Ravivara",
        "Somavara",
        "Mangalvara",
        "Budhavara",
        "Guruvara",
        "Sukravara",
        "Sanivara",
    ],
    days_abbr: ["Ravi", "Som", "Mangal", "Budh", "Guru", "Sukra", "Sani"],
};
static N_14_0000: CalNames = CalNames {
    months: [
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
        "",
    ],
    abbr: [
        "Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sept", "Oct", "Nov", "Dec", "",
    ],
    initial: [
        "J", "F", "M", "A", "M", "J", "J", "A", "S", "O", "N", "D", "",
    ],
    days: [
        "Sunday",
        "Monday",
        "Tuesday",
        "Wednesday",
        "Thursday",
        "Friday",
        "Saturday",
    ],
    days_abbr: ["Sun", "Mon", "Tue", "Wed", "Thu", "Fri", "Sat"],
};
static N_14_0409: CalNames = CalNames {
    months: [
        "정월",
        "이월",
        "삼월",
        "사월",
        "오월",
        "유월",
        "칠월",
        "팔월",
        "구월",
        "시월",
        "동짓달",
        "섣달",
        "",
    ],
    abbr: [
        "정월",
        "이월",
        "삼월",
        "사월",
        "오월",
        "유월",
        "칠월",
        "팔월",
        "구월",
        "시월",
        "동짓달",
        "섣달",
        "",
    ],
    initial: [
        "정", "이", "삼", "사", "오", "유", "칠", "팔", "구", "시", "동", "섣", "",
    ],
    days: [
        "일요일",
        "월요일",
        "화요일",
        "수요일",
        "목요일",
        "금요일",
        "토요일",
    ],
    days_abbr: ["일", "월", "화", "수", "목", "금", "토"],
};
static N_16_0000: CalNames = CalNames {
    months: [
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
        "",
    ],
    abbr: [
        "Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sept", "Oct", "Nov", "Dec", "",
    ],
    initial: [
        "J", "F", "M", "A", "M", "J", "J", "A", "S", "O", "N", "D", "",
    ],
    days: [
        "Sunday",
        "Monday",
        "Tuesday",
        "Wednesday",
        "Thursday",
        "Friday",
        "Saturday",
    ],
    days_abbr: ["Sun", "Mon", "Tue", "Wed", "Thu", "Fri", "Sat"],
};
static N_16_0409: CalNames = CalNames {
    months: [
        "Farvardin",
        "Ordibehesht",
        "Khordād",
        "Tir",
        "Mordād",
        "Shahrivar",
        "Mehr",
        "Ᾱban",
        "Ᾱzar",
        "Day",
        "Bahmān",
        "Esfand",
        "",
    ],
    abbr: [
        "Farvardin",
        "Ordibehesht",
        "Khordād",
        "Tir",
        "Mordād",
        "Shahrivar",
        "Mehr",
        "Ᾱban",
        "Ᾱzar",
        "Day",
        "Bahmān",
        "Esfand",
        "",
    ],
    initial: [
        "F", "O", "K", "T", "M", "S", "M", "Ᾱ", "Ᾱ", "D", "B", "E", "",
    ],
    days: [
        "Yekshanbeh",
        "Doshanbeh",
        "Seshanbeh",
        "Chahārshanbeh",
        "Panjshanbeh",
        "Jom'eh",
        "shanbeh",
    ],
    days_abbr: [
        "Yekshanbeh",
        "Doshanbeh",
        "Seshanbeh",
        "Chahārshanbeh",
        "Panjshanbeh",
        "Jom'eh",
        "shanbeh",
    ],
};
static N_17_0000: CalNames = CalNames {
    months: [
        "محرم",
        "صفر",
        "ربيع\u{a0}الأول",
        "ربيع\u{a0}الثاني",
        "جمادى\u{a0}الأولى",
        "جمادى\u{a0}الثانية",
        "رجب",
        "شعبان",
        "رمضان",
        "شوال",
        "ذو\u{a0}القعدة",
        "ذو\u{a0}الحجة",
        "",
    ],
    abbr: [
        "محرم",
        "صفر",
        "ربيع\u{a0}الأول",
        "ربيع\u{a0}الثاني",
        "جمادى\u{a0}الأولى",
        "جمادى\u{a0}الثانية",
        "رجب",
        "شعبان",
        "رمضان",
        "شوال",
        "ذو\u{a0}القعدة",
        "ذو\u{a0}الحجة",
        "",
    ],
    initial: [
        "م", "ص", "ر", "ر", "ج", "ج", "ر", "ش", "ر", "ش", "ذ", "ذ", "",
    ],
    days: [
        "الأحد",
        "الإثنين",
        "الثلاثاء",
        "الأربعاء",
        "الخميس",
        "الجمعة",
        "السبت",
    ],
    days_abbr: [
        "الأحد",
        "الإثنين",
        "الثلاثاء",
        "الأربعاء",
        "الخميس",
        "الجمعة",
        "السبت",
    ],
};
static N_17_0409: CalNames = CalNames {
    months: [
        "Muharram",
        "Safar",
        "Rabi' Awwal",
        "Rabi' Thani",
        "Jamada El Oula",
        "Jamada El Thaniah",
        "Rajab",
        "Sha'ban",
        "Ramadan",
        "Shawwal",
        "Thoul Ki'dah",
        "Thoul Hijjah",
        "",
    ],
    abbr: [
        "Muharram",
        "Safar",
        "Rabi' Awwal",
        "Rabi' Thani",
        "Jamada El Oula",
        "Jamada El Thaniah",
        "Rajab",
        "Sha'ban",
        "Ramadan",
        "Shawwal",
        "Thoul Ki'dah",
        "Thoul Hijjah",
        "",
    ],
    initial: [
        "M", "S", "R", "R", "J", "J", "R", "S", "R", "S", "T", "T", "",
    ],
    days: [
        "AlAhad",
        "AlEthnien",
        "AthThulatha",
        "AlArbia'a",
        "AlKhamis",
        "AlJumaa",
        "AsSabt",
    ],
    days_abbr: [
        "AlAhad",
        "AlEthnien",
        "AthThulatha",
        "AlArbia'a",
        "AlKhamis",
        "AlJumaa",
        "AsSabt",
    ],
};

fn table(cal: u8, lang: u32) -> Option<&'static CalNames> {
    Some(match (cal, lang) {
        (0x03, 0x0000) => &N_03_0000,
        (0x03, 0x0409) => &N_03_0409,
        (0x04, 0x0000) => &N_04_0000,
        (0x04, 0x0409) => &N_04_0409,
        (0x05, 0x0000) => &N_05_0000,
        (0x05, 0x0409) => &N_05_0409,
        (0x06, 0x0000) => &N_06_0000,
        (0x06, 0x0409) => &N_06_0409,
        (0x07, 0x0000) => &N_07_0000,
        (0x07, 0x0409) => &N_07_0409,
        (0x08, 0x0000) => &N_08_0000,
        (0x08, 0x0409) => &N_08_0409,
        (0x09, 0x0000) => &N_09_0000,
        (0x09, 0x0409) => &N_09_0409,
        (0x0A, 0x0000) => &N_0A_0000,
        (0x0A, 0x0409) => &N_0A_0409,
        (0x0B, 0x0000) => &N_0B_0000,
        (0x0B, 0x0409) => &N_0B_0409,
        (0x0C, 0x0000) => &N_0C_0000,
        (0x0C, 0x0409) => &N_0C_0409,
        (0x10, 0x0000) => &N_10_0000,
        (0x10, 0x0409) => &N_10_0409,
        (0x14, 0x0000) => &N_14_0000,
        (0x14, 0x0409) => &N_14_0409,
        (0x16, 0x0000) => &N_16_0000,
        (0x16, 0x0409) => &N_16_0409,
        (0x17, 0x0000) => &N_17_0000,
        (0x17, 0x0409) => &N_17_0409,
        (0x0E | 0x0F | 0x11 | 0x12 | 0x13, 0x0000) => &super::calendars_cn::NAMES_EN0,
        (0x0E, 0x0409) => &super::calendars_cn::NAMES_ZH_E,
        (0x0F | 0x11, 0x0409) => &super::calendars_cn::NAMES_ZH_F,
        (0x12 | 0x13, 0x0409) => &super::calendars_cn::NAMES_EN9,
        _ => return None,
    })
}
