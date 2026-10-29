pub const MAX_SERIAL_1900: f64 = 2_958_465.0;

pub const MAX_SERIAL_1904: f64 = 2_957_003.0;

#[derive(Clone, Copy, PartialEq, Eq, Debug, Default)]
pub enum DateSystem {
    #[default]
    D1900,

    D1904,
}

impl DateSystem {
    pub fn max_serial(self) -> f64 {
        match self {
            DateSystem::D1900 => MAX_SERIAL_1900,
            DateSystem::D1904 => MAX_SERIAL_1904,
        }
    }
}

pub fn days_from_civil(y: i64, m: i64, d: i64) -> i64 {
    let y = if m <= 2 { y - 1 } else { y };
    let era = if y >= 0 { y } else { y - 399 } / 400;
    let yoe = y - era * 400;
    let mp = (m + 9) % 12;
    let doy = (153 * mp + 2) / 5 + d - 1;
    let doe = yoe * 365 + yoe / 4 - yoe / 100 + doy;
    era * 146_097 + doe - 719_468
}

pub fn civil_from_days(z: i64) -> (i64, u32, u32) {
    let z = z + 719_468;
    let era = if z >= 0 { z } else { z - 146_096 } / 146_097;
    let doe = z - era * 146_097;
    let yoe = (doe - doe / 1460 + doe / 36_524 - doe / 146_096) / 365;
    let y = yoe + era * 400;
    let doy = doe - (365 * yoe + yoe / 4 - yoe / 100);
    let mp = (5 * doy + 2) / 153;
    let d = (doy - (153 * mp + 2) / 5 + 1) as u32;
    let m = if mp < 10 { mp + 3 } else { mp - 9 } as u32;
    (if m <= 2 { y + 1 } else { y }, m, d)
}

const DAYS_1899_12_30: i64 = -25_569;
const DAYS_1904_01_01: i64 = -24_107;

pub fn serial_from_ymd(sys: DateSystem, y: i64, m: i64, d: i64) -> i64 {
    let days = days_from_civil(y, m, d);
    match sys {
        DateSystem::D1904 => days - DAYS_1904_01_01,
        DateSystem::D1900 => {
            let s = days - DAYS_1899_12_30;
            if s <= 60 {
                s - 1
            } else {
                s
            }
        }
    }
}

pub fn ymd_from_serial(sys: DateSystem, serial: i64) -> (i64, u32, u32) {
    match sys {
        DateSystem::D1904 => civil_from_days(serial + DAYS_1904_01_01),
        DateSystem::D1900 => match serial {
            0 => (1900, 1, 0),
            60 => (1900, 2, 29),
            s if s < 60 => civil_from_days(s + 1 + DAYS_1899_12_30),
            s => civil_from_days(s + DAYS_1899_12_30),
        },
    }
}

pub fn weekday_from_serial(sys: DateSystem, serial: i64) -> u32 {
    let base = match sys {
        DateSystem::D1900 => 6,
        DateSystem::D1904 => 5,
    };
    (serial + base).rem_euclid(7) as u32
}

pub fn is_leap_year(y: i64) -> bool {
    (y % 4 == 0 && y % 100 != 0) || y % 400 == 0
}

pub fn days_in_month(y: i64, m: u32) -> u32 {
    match m {
        1 | 3 | 5 | 7 | 8 | 10 | 12 => 31,
        4 | 6 | 9 | 11 => 30,
        2 if is_leap_year(y) => 29,
        _ => 28,
    }
}

pub fn date_serial(sys: DateSystem, year: f64, month: f64, day: f64) -> Option<f64> {
    let fl = |x: f64| {
        let x = crate::functions::args::ffuzz(x);
        if x.is_nan() {
            -1_000_000_000
        } else {
            x.clamp(-1e9, 1e9) as i64
        }
    };
    let mut y = fl(year);
    let m = fl(month);
    let mut d = fl(day);

    if !(-32768..=32767).contains(&d) {
        d = 32767;
    }
    if !(-32767..=32766).contains(&m) {
        return None;
    }
    if sys == DateSystem::D1904 && y >= 10_000 {
        return None;
    }

    if y >= 10_000 {
        return None;
    }

    if y < 1900 {
        y += 1900;
    }
    let total_months = y * 12 + (m - 1);
    let (yy, mm) = (total_months.div_euclid(12), total_months.rem_euclid(12) + 1);
    if !(1900..10_000).contains(&yy) || (sys == DateSystem::D1904 && yy < 1904) {
        return None;
    }

    let s = serial_from_ymd(sys, yy, mm, 1) + d - 1;
    if s < 0 || s as f64 > sys.max_serial() {
        return None;
    }
    Some(s as f64)
}

pub fn time_fraction(h: f64, m: f64, s: f64) -> Option<f64> {
    let fl = |x: f64| crate::functions::args::ffuzz(x);
    let total = fl(h) * 3600.0 + fl(m) * 60.0 + fl(s);
    if total < 0.0 {
        return None;
    }

    let v = total / 86_400.0;
    Some(v - v.floor())
}

pub fn hijri_from_serial(serial: i64) -> Option<(i64, u32, u32)> {
    if serial == 60 || serial < 0 {
        return None;
    }
    let s = serial.max(1) + i64::from(serial < 60);
    let leap = |y: i64| {
        matches!(
            y.rem_euclid(30),
            2 | 5 | 7 | 10 | 13 | 15 | 18 | 21 | 24 | 26 | 29
        )
    };

    let mut n = s + 2_415_018 + 2 - 1_948_440;
    let mut y = 1 + n.div_euclid(10_631) * 30;
    n = n.rem_euclid(10_631);
    loop {
        let len = if leap(y) { 355 } else { 354 };
        if n < len {
            break;
        }
        n -= len;
        y += 1;
    }
    let mut m = 1u32;
    loop {
        let len = if m % 2 == 1 || (m == 12 && leap(y)) {
            30
        } else {
            29
        };
        if n < len {
            break;
        }
        n -= len;
        m += 1;
    }
    Some((y, m, n as u32 + 1))
}

pub fn split_serial(serial: f64) -> (i64, u32, u32, u32, u32) {
    let y = serial + 0.5 / 86_400.0;
    let days = y.floor();
    let h = (y - days) * 24.0;
    let hi = h.floor();
    let m = (h - hi) * 60.0;
    let mi = m.floor();
    let s = ((m - mi) * 60.0).floor();
    (
        days as i64,
        (hi as u32).min(23),
        (mi as u32).min(59),
        (s as u32).min(59),
        0,
    )
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn serials_1900() {
        let s = DateSystem::D1900;
        assert_eq!(serial_from_ymd(s, 1900, 1, 1), 1);
        assert_eq!(serial_from_ymd(s, 1900, 2, 28), 59);
        assert_eq!(serial_from_ymd(s, 1900, 3, 1), 61);
        assert_eq!(serial_from_ymd(s, 2020, 1, 2), 43832);
        assert_eq!(serial_from_ymd(s, 9999, 12, 31), 2_958_465);
        assert_eq!(ymd_from_serial(s, 60), (1900, 2, 29));
        assert_eq!(ymd_from_serial(s, 61), (1900, 3, 1));
        assert_eq!(ymd_from_serial(s, 59), (1900, 2, 28));
        assert_eq!(ymd_from_serial(s, 1), (1900, 1, 1));
        assert_eq!(ymd_from_serial(s, 43862), (2020, 2, 1));
        for serial in [1i64, 59, 61, 1000, 43832, 2_958_465] {
            let (y, m, d) = ymd_from_serial(s, serial);
            assert_eq!(serial_from_ymd(s, y, m as i64, d as i64), serial);
        }
    }

    #[test]
    fn date_function() {
        let s = DateSystem::D1900;
        assert_eq!(date_serial(s, 1900.0, 2.0, 29.0), Some(60.0));
        assert_eq!(date_serial(s, 1900.0, 3.0, 1.0), Some(61.0));
        assert_eq!(date_serial(s, 120.0, 1.0, 2.0), Some(43832.0));
        assert_eq!(
            date_serial(s, 2020.0, 13.0, 1.0),
            Some(serial_from_ymd(s, 2021, 1, 1) as f64)
        );
        assert_eq!(
            date_serial(s, 2020.0, 1.0, 0.0),
            Some(serial_from_ymd(s, 2019, 12, 31) as f64)
        );
        assert_eq!(date_serial(s, 10000.0, 1.0, 1.0), None);
        assert_eq!(date_serial(s, 1900.0, 1.0, 0.0), Some(0.0));
        assert_eq!(date_serial(s, 1900.0, 1.0, -1.0), None);
    }

    #[test]
    fn weekdays() {
        let s = DateSystem::D1900;

        assert_eq!(weekday_from_serial(s, 43832), 4);

        assert_eq!(weekday_from_serial(s, 1), 0);
    }
}
