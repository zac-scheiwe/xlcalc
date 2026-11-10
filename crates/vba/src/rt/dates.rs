use super::util::{convert, double, is_null, long_or, null_error, opt, text, text_out, R};
use super::RtContext;
use crate::VbaError;
use xlcalc_variant::{Locale, VarType, Variant};

const MIN_DATE: f64 = -657_434.0;
const MAX_DATE: f64 = 2_958_465.0;

fn days_from_civil(y: i64, m: i64, d: i64) -> i64 {
    let y = if m <= 2 { y - 1 } else { y };
    let era = y.div_euclid(400);
    let yoe = y - era * 400;
    let mp = (m + 9) % 12;
    let doy = (153 * mp + 2) / 5 + d - 1;
    let doe = yoe * 365 + yoe / 4 - yoe / 100 + doy;
    era * 146_097 + doe - 719_468
}

fn civil_from_days(z: i64) -> (i64, i64, i64) {
    let z = z + 719_468;
    let era = z.div_euclid(146_097);
    let doe = z - era * 146_097;
    let yoe = (doe - doe / 1460 + doe / 36_524 - doe / 146_096) / 365;
    let doy = doe - (365 * yoe + yoe / 4 - yoe / 100);
    let mp = (5 * doy + 2) / 153;
    let d = doy - (153 * mp + 2) / 5 + 1;
    let m = if mp < 10 { mp + 3 } else { mp - 9 };
    (yoe + era * 400 + i64::from(m <= 2), m, d)
}

const EPOCH: i64 = -25_569;

fn serial_of(y: i64, m: i64, d: i64) -> i64 {
    days_from_civil(y, m, d) - EPOCH
}

fn days_in_month(y: i64, m: i64) -> i64 {
    serial_of(y + m / 12, m % 12 + 1, 1) - serial_of(y, m, 1)
}

fn split(x: f64) -> (i64, i64) {
    let day = x.trunc();
    let secs = ((x - day).abs() * 86_400.0).round() as i64;
    let day = day as i64;
    if secs < 86_400 {
        return (day, secs);
    }
    let next = if x < 0.0 { day - 1 } else { day + 1 };
    if (MIN_DATE as i64..=MAX_DATE as i64).contains(&next) {
        (next, 0)
    } else {
        (day, 86_399)
    }
}

fn join(day: i64, secs: i64) -> f64 {
    let frac = secs as f64 / 86_400.0;
    if day < 0 {
        day as f64 - frac
    } else {
        day as f64 + frac
    }
}

pub(super) fn in_date_range(x: f64) -> bool {
    x > MIN_DATE - 1.0 && x < MAX_DATE + 1.0
}

fn in_range(x: f64) -> Result<f64, VbaError> {
    if x > MIN_DATE - 1.0 && x < MAX_DATE + 1.0 {
        Ok(x)
    } else {
        Err(VbaError::new(5))
    }
}

pub(super) fn system_clock() -> f64 {
    let secs = std::time::SystemTime::now()
        .duration_since(std::time::UNIX_EPOCH)
        .map_or(0.0, |d| d.as_secs_f64());
    secs / 86_400.0 - EPOCH as f64
}

fn civil(x: f64) -> (i64, i64, i64) {
    civil_from_days(split(x).0 + EPOCH)
}

fn two_digit_year(y: i64) -> i64 {
    if y < 50 {
        2000 + y
    } else {
        1900 + y
    }
}

pub(super) struct Parts {
    pub year: i64,
    pub month: i64,

    pub serial: i64,

    pub weekday: i64,

    pub yday: i64,

    pub secs: i64,
}

pub(super) fn parts(x: f64) -> Parts {
    let x = if x.is_nan() {
        0.0
    } else {
        x.clamp(-1.0e7, 1.0e7)
    };
    let (day, secs) = split(x);
    let (year, month, _) = civil_from_days(day + EPOCH);
    Parts {
        year,
        month,
        serial: day,
        weekday: weekday_sunday(day),
        yday: day - serial_of(year, 1, 1) + 1,
        secs,
    }
}

pub(super) fn month_title(m: i64) -> String {
    let name = MONTHS[(m - 1).rem_euclid(12) as usize];
    let mut c = name.chars();
    c.next()
        .map(|f| f.to_ascii_uppercase().to_string() + c.as_str())
        .unwrap_or_default()
}

const MONTHS: [&str; 12] = [
    "january",
    "february",
    "march",
    "april",
    "may",
    "june",
    "july",
    "august",
    "september",
    "october",
    "november",
    "december",
];

fn month_name(s: &str) -> Option<i64> {
    let l = s.to_ascii_lowercase();
    if l.len() < 3 {
        return None;
    }
    MONTHS
        .iter()
        .position(|m| m.starts_with(&l))
        .map(|i| i as i64 + 1)
}

fn digits(s: &str) -> bool {
    !s.is_empty() && s.bytes().all(|c| c.is_ascii_digit())
}

fn parse_time(s: &str) -> Option<i64> {
    let lower = s.trim().to_ascii_lowercase();
    let (body, pm) = if let Some(b) = lower.strip_suffix("pm") {
        (b.trim(), Some(true))
    } else if let Some(b) = lower.strip_suffix("am") {
        (b.trim(), Some(false))
    } else {
        (lower.as_str(), None)
    };
    let sep = if body.contains(':') { ':' } else { '.' };
    let parts: Vec<&str> = body.split(sep).collect();
    if parts.len() > 3 || !parts.iter().all(|p| digits(p)) {
        return None;
    }
    if parts.len() == 1 && pm.is_none() {
        return None;
    }
    let n: Vec<i64> = parts.iter().map(|p| p.parse().unwrap_or(99)).collect();
    let (mut h, m, sec) = (
        n[0],
        n.get(1).copied().unwrap_or(0),
        n.get(2).copied().unwrap_or(0),
    );
    if h > 23 || m > 59 || sec > 59 {
        return None;
    }
    match pm {
        Some(true) if h < 12 => h += 12,
        Some(false) if h == 12 => h = 0,
        _ => {}
    }
    Some(h * 3600 + m * 60 + sec)
}

fn valid_day(y: i64, m: i64, d: i64) -> Option<i64> {
    ((100..=9999).contains(&y) && (1..=12).contains(&m) && d >= 1 && d <= days_in_month(y, m))
        .then(|| serial_of(y, m, d))
}

fn parse_day(ctx: &RtContext, s: &str) -> Option<i64> {
    let items = s
        .split(|c: char| c.is_whitespace() || matches!(c, '/' | ',' | '-'))
        .filter(|t| !t.is_empty());
    let mut month = None;
    let mut nums: Vec<(i64, usize)> = Vec::new();
    for t in items {
        if digits(t) {
            nums.push((t.parse().ok()?, t.len()));
        } else if month.is_none() {
            month = Some(month_name(t)?);
        } else {
            return None;
        }
    }
    let year_now = civil((ctx.clock)()).0;
    let mdy = ctx.locale == Locale::EN_US;
    let year = |y: i64| if y < 100 { two_digit_year(y) } else { y };
    let long_year = |(v, l): (i64, usize)| l > 2 || v > 31;
    if let Some(m) = month {
        return match nums.as_slice() {
            [a] if long_year(*a) => valid_day(a.0, m, 1),
            [(d, _)] => valid_day(year_now, m, *d).or_else(|| valid_day(year(*d), m, 1)),
            [a, (b, _)] if long_year(*a) => valid_day(a.0, m, *b),
            [(a, _), b] => valid_day(year(b.0), m, *a),
            _ => None,
        };
    }
    match nums.as_slice() {
        [a, (m, _), (d, _)] if long_year(*a) => valid_day(a.0, *m, *d),
        [(a, _), (b, _), (c, _)] => {
            let (first, second) = if mdy { (*a, *b) } else { (*b, *a) };
            valid_day(year(*c), first, second).or_else(|| valid_day(year(*c), second, first))
        }
        [a, (m, _)] if long_year(*a) => valid_day(a.0, *m, 1),
        [(a, _), b] if long_year(*b) => valid_day(year(b.0), *a, 1),
        [(a, _), (b, _)] => {
            let (first, second) = if mdy { (*a, *b) } else { (*b, *a) };
            valid_day(year_now, first, second).or_else(|| valid_day(year_now, second, first))
        }
        _ => None,
    }
}

fn is_time_token(t: &str) -> bool {
    let l = t.to_ascii_lowercase();
    t.contains(':')
        || l == "am"
        || l == "pm"
        || (l.len() > 2 && (l.ends_with("am") || l.ends_with("pm")) && digits(&l[..l.len() - 2]))
}

pub(super) fn parse_date(ctx: &RtContext, s: &str) -> Option<f64> {
    if let Some(t) = parse_time(s) {
        return Some(t as f64 / 86_400.0);
    }
    let tokens: Vec<&str> = s.split_whitespace().collect();
    let (date, time) = match tokens.iter().position(|t| is_time_token(t)) {
        Some(0) => return None,
        Some(i) => {
            let l = tokens[i].to_ascii_lowercase();
            let i = if (l == "am" || l == "pm") && digits(tokens[i - 1]) {
                i - 1
            } else {
                i
            };
            if i == 0 {
                return None;
            }
            (tokens[..i].join(" "), Some(tokens[i..].join(" ")))
        }
        None => (tokens.join(" "), None),
    };
    let day = parse_day(ctx, &date)?;
    let secs = match time {
        Some(t) => parse_time(&t)?,
        None => 0,
    };
    Some(join(day, secs))
}

pub(super) fn to_date(ctx: &RtContext, v: &Variant) -> Result<f64, VbaError> {
    let x = match v {
        Variant::Date(x) => return Ok(*x),
        Variant::Null => return Err(null_error()),
        Variant::Bstr(s) => match parse_date(ctx, s) {
            Some(x) => return Ok(x),
            None => double(ctx, v).map_err(|_| VbaError::new(13))?,
        },
        v => match convert(ctx, v, VarType::Date) {
            Ok(Variant::Date(x)) => x,
            Ok(_) => return Err(VbaError::new(13)),
            Err(e) if e.number == 6 => return Err(VbaError::new(13)),
            Err(e) => return Err(e),
        },
    };
    in_range(x).map_err(|_| VbaError::new(13))
}

fn text_to_date(ctx: &RtContext, v: &Variant) -> Result<f64, VbaError> {
    match v {
        Variant::Date(x) => Ok(*x),
        Variant::Null => Err(null_error()),
        v => parse_date(ctx, &text(ctx, v)?).ok_or(VbaError::new(13)),
    }
}

fn part(ctx: &RtContext, a: &[Variant], f: fn(f64) -> i64) -> R {
    if is_null(&a[0]) {
        return Ok(Variant::Null);
    }
    Ok(Variant::I2(f(to_date(ctx, &a[0])?) as i16))
}

pub(super) fn year(ctx: &RtContext, a: &[Variant]) -> R {
    part(ctx, a, |x| civil(x).0)
}

pub(super) fn month(ctx: &RtContext, a: &[Variant]) -> R {
    part(ctx, a, |x| civil(x).1)
}

pub(super) fn day(ctx: &RtContext, a: &[Variant]) -> R {
    part(ctx, a, |x| civil(x).2)
}

pub(super) fn hour(ctx: &RtContext, a: &[Variant]) -> R {
    part(ctx, a, |x| split(x).1 / 3600)
}

pub(super) fn minute(ctx: &RtContext, a: &[Variant]) -> R {
    part(ctx, a, |x| split(x).1 / 60 % 60)
}

pub(super) fn second(ctx: &RtContext, a: &[Variant]) -> R {
    part(ctx, a, |x| split(x).1 % 60)
}

fn weekday_sunday(d: i64) -> i64 {
    (d.rem_euclid(7) + 6) % 7 + 1
}

pub(super) fn first_day(ctx: &RtContext, a: &[Variant], i: usize) -> Result<i64, VbaError> {
    match long_or(ctx, a, i, 1)? {
        0 => Ok(if ctx.locale == Locale::EN_US { 1 } else { 2 }),
        n @ 1..=7 => Ok(i64::from(n)),
        _ => Err(VbaError::new(5)),
    }
}

pub(super) fn weekday(ctx: &RtContext, a: &[Variant]) -> R {
    if is_null(&a[0]) {
        return Ok(Variant::Null);
    }
    let first = first_day(ctx, a, 1)?;
    let d = split(to_date(ctx, &a[0])?).0;
    Ok(Variant::I2(
        ((weekday_sunday(d) - first).rem_euclid(7) + 1) as i16,
    ))
}

fn integer(ctx: &RtContext, v: &Variant) -> Result<i64, VbaError> {
    match convert(ctx, v, VarType::I2)? {
        Variant::I2(n) => Ok(i64::from(n)),
        _ => Err(VbaError::new(13)),
    }
}

pub(super) fn date_serial(ctx: &RtContext, a: &[Variant]) -> R {
    let (y, m, d) = (
        integer(ctx, &a[0])?,
        integer(ctx, &a[1])?,
        integer(ctx, &a[2])?,
    );
    let months = y * 12 + m - 1;
    let (mut y, m) = (months.div_euclid(12), months.rem_euclid(12) + 1);
    if y < 0 {
        y += 2000;
    } else if y < 100 {
        y = two_digit_year(y);
    }
    if !(100..=9999).contains(&y) {
        return Err(VbaError::new(5));
    }
    in_range((serial_of(y, m, 1) + d - 1) as f64).map(Variant::Date)
}

pub(super) fn time_serial(ctx: &RtContext, a: &[Variant]) -> R {
    let (h, m, s) = (
        integer(ctx, &a[0])?,
        integer(ctx, &a[1])?,
        integer(ctx, &a[2])?,
    );
    Ok(Variant::Date((h * 3600 + m * 60 + s) as f64 / 86_400.0))
}

pub(super) fn date_value(ctx: &RtContext, a: &[Variant]) -> R {
    let x = text_to_date(ctx, &a[0])?;
    Ok(Variant::Date(split(x).0 as f64))
}

pub(super) fn time_value(ctx: &RtContext, a: &[Variant]) -> R {
    let x = text_to_date(ctx, &a[0])?;
    Ok(Variant::Date(split(x).1 as f64 / 86_400.0))
}

pub(super) fn date_add(ctx: &RtContext, a: &[Variant]) -> R {
    let unit = text(ctx, &a[0])?.to_ascii_lowercase();
    let n = double(ctx, &a[1])?.trunc();
    if n.abs() > f64::from(i32::MAX) {
        return Err(VbaError::new(6));
    }
    let n = n as i64;
    if is_null(&a[2]) {
        return Ok(Variant::Null);
    }
    let x = to_date(ctx, &a[2])?;
    let (day, _) = split(x);
    let frac = (x - x.trunc()).abs();
    let months = |k: i64| -> Result<f64, VbaError> {
        let (y, m, d) = civil_from_days(day + EPOCH);
        let t = y * 12 + m - 1 + k;
        let (y, m) = (t.div_euclid(12), t.rem_euclid(12) + 1);
        if !(100..=9999).contains(&y) {
            return Err(VbaError::new(5));
        }
        Ok(day_frac(serial_of(y, m, d.min(days_in_month(y, m))), frac))
    };
    let r = match unit.as_str() {
        "yyyy" => months(i64::from(n as i32 as i16) * 12)?,
        "q" => months(n * 3)?,
        "m" => months(n)?,
        "y" | "d" | "w" => day_frac(day + n, frac),
        "ww" => day_frac(day + n * 7, frac),
        "h" => add_fraction(day, x, n as f64 / 24.0),
        "n" => add_fraction(day, x, n as f64 / 1440.0),
        "s" => add_fraction(day, x, n as f64 / 86_400.0),
        _ => return Err(VbaError::new(5)),
    };
    in_range(r).map(Variant::Date)
}

fn day_frac(day: i64, frac: f64) -> f64 {
    if day < 0 {
        day as f64 - frac
    } else {
        day as f64 + frac
    }
}

fn add_fraction(day: i64, x: f64, delta: f64) -> f64 {
    let m = x.abs() + delta;
    if day < 0 {
        -m
    } else {
        m
    }
}

pub(super) fn date_diff(ctx: &RtContext, a: &[Variant]) -> R {
    let unit = text(ctx, &a[0])?.to_ascii_lowercase();
    let first = long_or(ctx, a, 3, 1)?;
    if opt(a, 4).is_some_and(is_null) {
        return Err(null_error());
    }
    if is_null(&a[1]) || is_null(&a[2]) {
        return Ok(Variant::Null);
    }
    let (x, y) = (to_date(ctx, &a[1])?, to_date(ctx, &a[2])?);
    let ((dx, sx), (dy, sy)) = (split(x), split(y));
    let (cx, cy) = (civil(x), civil(y));
    let (tx, ty) = (dx * 86_400 + sx, dy * 86_400 + sy);
    let n = match unit.as_str() {
        "yyyy" => cy.0 - cx.0,
        "q" => (cy.0 * 4 + (cy.1 - 1) / 3) - (cx.0 * 4 + (cx.1 - 1) / 3),
        "m" => (cy.0 * 12 + cy.1) - (cx.0 * 12 + cx.1),
        "y" | "d" => dy - dx,
        "w" => (dy - dx) / 7,
        "ww" => {
            let first = match first {
                0 => first_day(ctx, &[Variant::I4(0)], 0)?,
                1..=7 => i64::from(first),
                _ => return Err(VbaError::new(5)),
            };
            let week = |d: i64| (d - first).div_euclid(7);
            week(dy) - week(dx)
        }
        "h" => ty.div_euclid(3600) - tx.div_euclid(3600),
        "n" => ty.div_euclid(60) - tx.div_euclid(60),
        "s" => ty - tx,
        _ => return Err(VbaError::new(5)),
    };
    Ok(i32::try_from(n).map_or(Variant::R8(n as f64), Variant::I4))
}

pub(super) fn now(ctx: &RtContext, _: &[Variant]) -> R {
    let (d, s) = split((ctx.clock)());
    Ok(Variant::Date(join(d, s)))
}

pub(super) fn date(ctx: &RtContext, _: &[Variant]) -> R {
    Ok(Variant::Date(split((ctx.clock)()).0 as f64))
}

pub(super) fn time(ctx: &RtContext, _: &[Variant]) -> R {
    Ok(Variant::Date(split((ctx.clock)()).1 as f64 / 86_400.0))
}

pub(super) fn date_text(ctx: &RtContext, _: &[Variant]) -> R {
    let (y, m, d) = civil((ctx.clock)());
    text_out(format!("{m:02}-{d:02}-{y:04}"))
}

pub(super) fn time_text(ctx: &RtContext, _: &[Variant]) -> R {
    let s = split((ctx.clock)()).1;
    text_out(format!("{:02}:{:02}:{:02}", s / 3600, s / 60 % 60, s % 60))
}

pub(super) fn timer(ctx: &RtContext, _: &[Variant]) -> R {
    let x = (ctx.clock)();
    Ok(Variant::R4(((x - x.floor()) * 86_400.0) as f32))
}

pub(super) fn date_part(ctx: &RtContext, a: &[Variant]) -> R {
    let unit = text(ctx, &a[0])?.to_ascii_lowercase();

    if is_null(&a[1]) {
        return Ok(Variant::Null);
    }
    let x = to_date(ctx, &a[1])?;
    let (day, secs) = split(x);
    let p = parts(x);
    let n = match unit.as_str() {
        "yyyy" => civil(x).0,
        "q" => (civil(x).1 - 1) / 3 + 1,
        "m" => civil(x).1,
        "y" => p.yday,
        "d" => civil(x).2,
        "w" => (weekday_sunday(day) - first_day(ctx, a, 2)?).rem_euclid(7) + 1,
        "ww" => {
            let first = first_day(ctx, a, 2)?;
            let rule = match long_or(ctx, a, 3, 1)? {
                0 | 1 => 1,
                n @ 2..=3 => i64::from(n),
                _ => return Err(VbaError::new(5)),
            };
            super::format::week_of_year(&p, super::format::Week { first, rule })
        }
        "h" => secs / 3600,
        "n" => secs / 60 % 60,
        "s" => secs % 60,
        _ => return Err(VbaError::new(5)),
    };
    Ok(Variant::I2(n as i16))
}

pub(super) fn month_name_fn(ctx: &RtContext, a: &[Variant]) -> R {
    if a.iter().any(is_null) {
        return Err(null_error());
    }
    let m = match convert(ctx, &a[0], VarType::I2)? {
        Variant::I2(n) => i64::from(n),
        _ => return Err(VbaError::new(13)),
    };
    let abbreviate = match opt(a, 1) {
        Some(v) => match convert(ctx, v, VarType::Bool)? {
            Variant::Bool(b) => b,
            _ => false,
        },
        None => false,
    };
    if !(1..=12).contains(&m) {
        return Err(VbaError::new(5));
    }
    let full = month_title(m);
    text_out(if abbreviate {
        full[..3].to_string()
    } else {
        full
    })
}

pub(super) fn weekday_name(ctx: &RtContext, a: &[Variant]) -> R {
    if a.iter().any(is_null) {
        return Err(null_error());
    }
    let w = match convert(ctx, &a[0], VarType::I2)? {
        Variant::I2(n) => i64::from(n),
        _ => return Err(VbaError::new(13)),
    };
    let abbreviate = match opt(a, 1) {
        Some(v) => match convert(ctx, v, VarType::Bool)? {
            Variant::Bool(b) => b,
            _ => false,
        },
        None => false,
    };
    let first = match opt(a, 2) {
        Some(_) => first_day(ctx, a, 2)?,
        None => first_day(ctx, &[Variant::I4(0)], 0)?,
    };
    if !(1..=7).contains(&w) {
        return Err(VbaError::new(5));
    }

    let sunday_based = (first - 1 + w - 1).rem_euclid(7);
    const DAYS: [&str; 7] = [
        "Sunday",
        "Monday",
        "Tuesday",
        "Wednesday",
        "Thursday",
        "Friday",
        "Saturday",
    ];
    let full = DAYS[sunday_based as usize];
    text_out(if abbreviate {
        full[..3].to_string()
    } else {
        full.to_string()
    })
}
