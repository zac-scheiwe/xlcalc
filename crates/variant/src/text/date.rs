use crate::{HResult, Locale};

pub(crate) fn civil(days: i64) -> (i64, u32, u32) {
    let z = days + 719_468;
    let era = z.div_euclid(146_097);
    let doe = z.rem_euclid(146_097);
    let yoe = (doe - doe / 1460 + doe / 36_524 - doe / 146_096) / 365;
    let y = yoe + era * 400;
    let doy = doe - (365 * yoe + yoe / 4 - yoe / 100);
    let mp = (5 * doy + 2) / 153;
    let d = (doy - (153 * mp + 2) / 5 + 1) as u32;
    let m = if mp < 10 { mp + 3 } else { mp - 9 } as u32;
    (if m <= 2 { y + 1 } else { y }, m, d)
}

pub(crate) fn split(x: f64) -> (i64, u32, u32, i64, bool) {
    let mut whole = x.trunc();
    let mut secs = ((x - whole).abs() * 86_400.0 + 0.5).floor() as i64;
    if secs == 86_400 {
        secs = 0;
        whole += 1.0;
    }
    let (y, m, d) = civil(whole as i64 - 25_569);
    (y, m, d, secs, whole != 0.0)
}

pub(crate) fn date_text(x: f64, locale: Locale) -> Result<String, HResult> {
    if x.is_nan() || x >= 2_958_466.0 || x <= -657_435.0 {
        return Err(HResult::E_INVALIDARG);
    }
    let (y, m, d, secs, has_date) = split(x);
    let mut out = String::new();
    if has_date {
        if locale == Locale::EN_US {
            out.push_str(&format!("{m}/{d}/{y}"));
        } else {
            out.push_str(&format!("{d}/{m:02}/{y}"));
        }
    }
    if secs != 0 || !has_date {
        if has_date {
            out.push(' ');
        }
        let (h, mi, s) = (secs / 3600, secs / 60 % 60, secs % 60);
        let h12 = if h % 12 == 0 { 12 } else { h % 12 };
        let pm = h >= 12;
        let mark = match (locale == Locale::EN_US, pm) {
            (true, false) => "AM",
            (true, true) => "PM",
            (false, false) => "am",
            (false, true) => "pm",
        };
        out.push_str(&format!("{h12}:{mi:02}:{s:02} {mark}"));
    }
    Ok(out)
}
