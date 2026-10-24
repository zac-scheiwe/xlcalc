use crate::datetime::{self, DateSystem};
use crate::locale::{month_from_name, DateOrder, Locale};
use crate::number;
use crate::value::{Text, Value, XlError};

#[derive(Clone, Debug, PartialEq)]
pub enum NumberKind {
    Plain,
    Scientific,
    Percent,
    Currency,
    Thousands,
    Date,
    Time { seconds: bool, ampm: bool },
    DateTime,
}

#[derive(Clone, Debug, PartialEq)]
pub struct ParsedNumber {
    pub value: f64,
    pub kind: NumberKind,

    pub format: Option<String>,
}

impl ParsedNumber {
    fn new(value: f64, kind: NumberKind) -> Self {
        ParsedNumber {
            value,
            kind,
            format: None,
        }
    }
}

#[derive(Clone, Copy)]
pub struct ParseEnv<'a> {
    pub locale: &'a Locale,
    pub date_system: DateSystem,

    pub current_year: i64,

    pub iso_t: bool,
}

pub fn parse_number_text(s: &str, env: ParseEnv) -> Option<ParsedNumber> {
    if !s.is_ascii() {
        let n = normalize_digits(s);
        if n != s {
            return parse_number_text(&n, env);
        }
    }
    let t = s.trim_matches(' ');
    if t.is_empty() {
        return None;
    }
    if let Some(p) = parse_plain(t, env.locale) {
        return Some(p);
    }
    if let Some(p) = parse_fraction_text(t) {
        return Some(p);
    }
    if let Some(p) = parse_time(t, env.locale) {
        return Some(p);
    }
    parse_date_time(t, env)
}

const DIGIT_ZEROS: [u32; 17] = [
    0x660, 0x6F0, 0x966, 0x9E6, 0xA66, 0xAE6, 0xB66, 0xBE6, 0xC66, 0xCE6, 0xD66, 0xE50, 0xED0,
    0xF20, 0x1040, 0x17E0, 0x1810,
];

fn normalize_digits(s: &str) -> String {
    s.chars()
        .map(|c| {
            let u = c as u32;
            match DIGIT_ZEROS.iter().find(|&&z| (z..z + 10).contains(&u)) {
                Some(z) => char::from(b'0' + (u - z) as u8),
                None => c,
            }
        })
        .collect()
}

fn parse_fraction_text(t: &str) -> Option<ParsedNumber> {
    let (neg, body) = match t.strip_prefix('-') {
        Some(b) => (true, b),
        None => (false, t.strip_prefix('+').unwrap_or(t)),
    };
    let (body, pct) = match body.strip_suffix('%') {
        Some(b) => (b.trim_end(), true),
        None => (body, false),
    };
    let (whole, frac) = body.split_once(' ')?;
    let (n, d) = frac.trim_start().split_once('/')?;
    if n.contains(' ') || d.trim_end().contains(' ') || d.starts_with(' ') {
        return None;
    }
    let w = parse_uint(whole)?;
    let n = parse_uint(n)?;
    let d = parse_uint(d)?;
    if d == 0.0 {
        return None;
    }
    let v = w + n / d;
    let v = if pct { v / 100.0 } else { v };
    let mut p = ParsedNumber::new(
        if neg { -v } else { v },
        if pct {
            NumberKind::Percent
        } else {
            NumberKind::Plain
        },
    );
    p.format = Some(if pct { "0.00%" } else { "# ?/?" }.to_string());
    Some(p)
}

fn parse_plain(t: &str, loc: &Locale) -> Option<ParsedNumber> {
    let mut s = t;
    let mut negative = false;
    let mut kind = NumberKind::Plain;
    let mut paren = false;
    if let Some(inner) = s.strip_prefix('(').and_then(|x| x.strip_suffix(')')) {
        paren = true;
        negative = true;
        s = inner.trim_matches(' ');
    }
    let mut percent = false;

    if !paren {
        if let Some(inner) = s.strip_suffix('%').map(|x| x.trim_end_matches(' ')) {
            if inner.starts_with('(') && inner.ends_with(')') {
                let p = parse_plain(inner, loc)?;
                return Some(ParsedNumber::new(
                    number::normalize(p.value / 100.0),
                    NumberKind::Percent,
                ));
            }
        }
    }
    if let Some(rest) = s.strip_suffix('%') {
        percent = true;
        s = rest.trim_end_matches(' ');
    } else if let Some(rest) = s.strip_prefix('%') {
        percent = true;
        s = rest.trim_start_matches(' ');
    }
    let take_sign = |s: &mut &str, negative: &mut bool| -> bool {
        if let Some(r) = s.strip_prefix('-') {
            *negative = !*negative;
            *s = r.trim_start_matches(' ');
            true
        } else if let Some(r) = s.strip_prefix('+') {
            *s = r.trim_start_matches(' ');
            true
        } else {
            false
        }
    };
    let had_sign = take_sign(&mut s, &mut negative);
    if paren && had_sign {
        return None;
    }
    if let Some(r) = s.strip_prefix(loc.currency.as_str()) {
        kind = NumberKind::Currency;
        s = r.trim_start_matches(' ');
        if !had_sign && !paren {
            take_sign(&mut s, &mut negative);
            if let Some(inner) = s.strip_prefix('(').and_then(|x| x.strip_suffix(')')) {
                if !negative {
                    negative = true;
                    s = inner.trim_matches(' ');
                }
            }
        }
    }

    if kind == NumberKind::Plain {
        if let Some(r) = s.strip_prefix('€') {
            kind = NumberKind::Currency;
            s = r.trim_start_matches(' ');
            if let Some(inner) = s.strip_prefix('(').and_then(|x| x.strip_suffix(')')) {
                if had_sign || paren {
                    return None;
                }
                negative = true;
                s = inner.trim_matches(' ');
            }
            if !had_sign && !paren {
                take_sign(&mut s, &mut negative);
            }
        } else if let Some(r) = s.strip_suffix('€') {
            kind = NumberKind::Currency;
            s = r.trim_end_matches(' ');
        }
    }
    if kind == NumberKind::Currency && percent {
        return None;
    }

    let b = s.as_bytes();
    if b.is_empty() {
        return None;
    }
    let mut clean = String::with_capacity(b.len());
    let mut i = 0;
    let mut grouped = false;
    let mut int_digits = 0usize;
    let mut group_len: Option<usize> = None;
    while i < b.len() {
        let c = b[i] as char;
        if c.is_ascii_digit() {
            clean.push(c);
            int_digits += 1;
            if let Some(g) = group_len.as_mut() {
                *g += 1;
            }
        } else if c == loc.thousands_sep {
            if int_digits == 0 || group_len.is_some_and(|g| g < 3) {
                return None;
            }
            grouped = true;
            group_len = Some(0);
        } else {
            break;
        }
        i += 1;
    }
    if group_len.is_some_and(|g| g < 3) {
        return None;
    }

    if grouped && !clean.bytes().any(|c| (b'1'..=b'9').contains(&c)) {
        return None;
    }
    let mut sci = false;
    if i < b.len() && b[i] as char == loc.decimal_sep {
        clean.push('.');
        i += 1;
        while i < b.len() && b[i].is_ascii_digit() {
            clean.push(b[i] as char);
            i += 1;
        }
    }
    if i < b.len() && (b[i] == b'e' || b[i] == b'E') {
        sci = true;
        clean.push('e');
        i += 1;
        if i < b.len() && (b[i] == b'+' || b[i] == b'-') {
            clean.push(b[i] as char);
            i += 1;
        }
        let es = i;
        while i < b.len() && b[i].is_ascii_digit() {
            clean.push(b[i] as char);
            i += 1;
        }
        if es == i {
            return None;
        }
    }
    if i != b.len() || !clean.bytes().any(|c| c.is_ascii_digit()) {
        return None;
    }

    if percent {
        clean = match clean.split_once('e') {
            Some((m, e)) => format!("{m}e{}", e.parse::<i64>().ok()? - 2),
            None => format!("{clean}e-2"),
        };
    }
    let mut v = number::parse_literal(&clean)?;
    if percent {
        kind = NumberKind::Percent;
    } else if kind == NumberKind::Plain {
        if sci {
            kind = NumberKind::Scientific;
        } else if grouped {
            kind = NumberKind::Thousands;
        }
    }
    if negative {
        v = -v;
    }
    let mut p = ParsedNumber::new(number::normalize(v), kind);
    let dec = t.contains(loc.decimal_sep);
    let exp = t.bytes().any(|c| c == b'e' || c == b'E');
    p.format = match p.kind {
        _ if exp => Some("0.00E+00".to_string()),
        NumberKind::Percent => Some(if dec { "0.00%" } else { "0%" }.to_string()),
        NumberKind::Thousands => Some(if dec { "#,##0.00" } else { "#,##0" }.to_string()),
        NumberKind::Currency => {
            let (c, d) = (loc.currency.as_str(), if dec { ".00" } else { "" });
            Some(format!("{c}#,##0{d};[Red]-{c}#,##0{d}"))
        }
        _ => None,
    };
    Some(p)
}

fn parse_time(t: &str, loc: &Locale) -> Option<ParsedNumber> {
    let (body, ampm) = split_ampm(t);
    let body = body.trim_end();

    let (body, trailing) = match body.strip_suffix(loc.time_sep) {
        Some(b) => (b, true),
        None => (body, false),
    };
    let parts: Vec<&str> = body.split(loc.time_sep).collect();
    let (mut h, m, sec, seconds);
    match parts.as_slice() {
        [hh] if ampm.is_some() || trailing => {
            h = parse_uint(hh)?;
            m = 0.0;
            sec = 0.0;
            seconds = false;
        }
        [hh, mm] => {
            if let Some((mi, frac)) = mm.split_once(loc.decimal_sep) {
                let mins = parse_uint(hh)?;
                let secs = parse_uint(mi)? + parse_fraction(frac)?;
                if secs >= 60.0 {
                    return None;
                }
                let v = (mins * 60.0 + secs) / 86_400.0;
                let mut p = ParsedNumber::new(
                    v,
                    NumberKind::Time {
                        seconds: true,
                        ampm: false,
                    },
                );
                p.format = Some("mm:ss.0".to_string());
                return Some(p);
            }
            h = parse_uint(hh)?;
            m = parse_uint(mm)?;
            sec = 0.0;
            seconds = false;
        }
        [hh, mm, ss] => {
            h = parse_uint(hh)?;
            m = parse_uint(mm)?;
            sec = match ss.split_once(loc.decimal_sep) {
                Some((a, f)) => parse_uint(a)? + parse_fraction(f)?,
                None => parse_uint(ss)?,
            };
            seconds = true;
        }
        _ => return None,
    }

    if let Some(pm) = ampm {
        if h > 12.0 || m >= 60.0 || sec >= 60.0 {
            return None;
        }
        if h == 12.0 {
            h = 0.0;
        }
        if pm {
            h += 12.0;
        }
    }
    if parts[0].len() >= 3 && parts[0].starts_with('0') {
        return None;
    }

    if parts.len() == 3 && m >= 60.0 && sec >= 60.0 {
        return None;
    }
    let v = (h * 3600.0 + m * 60.0 + sec) / 86_400.0;
    let frac = parts.last().is_some_and(|l| l.contains(loc.decimal_sep));
    let mut p = ParsedNumber::new(
        v,
        NumberKind::Time {
            seconds,
            ampm: ampm.is_some(),
        },
    );
    p.format = if frac {
        Some("mm:ss.0")
    } else if m >= 60.0 || sec >= 60.0 {
        Some("General")
    } else if h >= 24.0 {
        Some("[h]:mm:ss")
    } else if ampm.is_some() {
        Some(if seconds {
            "h:mm:ss am/pm"
        } else {
            "h:mm am/pm"
        })
    } else {
        None
    }
    .map(str::to_string);
    Some(p)
}

fn split_ampm(t: &str) -> (&str, Option<bool>) {
    let lower = t.to_ascii_lowercase();
    for (suffix, pm) in [(" am", false), (" pm", true), (" a", false), (" p", true)] {
        if lower.ends_with(suffix) {
            let body = &t[..t.len() - suffix.len()];
            if body.ends_with(|c: char| c.is_ascii_digit()) {
                return (body, Some(pm));
            }
        }
    }
    (t, None)
}

fn parse_uint(s: &str) -> Option<f64> {
    let s = s.trim();
    if s.is_empty() || s.len() > 9 || !s.bytes().all(|b| b.is_ascii_digit()) {
        return None;
    }
    s.parse::<u64>().ok().map(|v| v as f64)
}

fn parse_fraction(s: &str) -> Option<f64> {
    if s.is_empty() {
        return Some(0.0);
    }
    if !s.bytes().all(|b| b.is_ascii_digit()) {
        return None;
    }

    let f: f64 = format!("0.{s}").parse().ok()?;
    Some((f * 1000.0).round() / 1000.0)
}

enum DatePart {
    Num(u32, usize),
    Month(u32),
}

fn parse_date_time(t: &str, env: ParseEnv) -> Option<ParsedNumber> {
    let combined = |d: f64, tm: ParsedNumber| {
        let mut p = ParsedNumber::new(d + tm.value, NumberKind::DateTime);
        p.format = match tm.format.as_deref() {
            Some("mm:ss.0") => tm.format,
            Some("General" | "[h]:mm:ss") => Some("General".to_string()),
            _ => None,
        };
        p
    };

    if let Some((dp, tp)) = t.split_once('T').filter(|_| env.iso_t) {
        let tp = tp.strip_suffix('Z').unwrap_or(tp);
        let tp = tp.split('.').next().unwrap_or(tp);
        let iso_date = dp.len() == 10
            && dp.bytes().enumerate().all(|(i, b)| {
                if i == 4 || i == 7 {
                    b == b'-'
                } else {
                    b.is_ascii_digit()
                }
            });
        if iso_date && tp.matches(':').count() == 2 {
            if let (Some((d, _)), Some(tm)) = (parse_date(dp, env), parse_time(tp, env.locale)) {
                return Some(combined(d, tm));
            }
        }
    }

    if let Some((d, shape)) = parse_date(t, env) {
        let mut p = ParsedNumber::new(d, NumberKind::Date);
        p.format = shape.map(str::to_string);
        return Some(p);
    }
    let mut idx = t.len();
    while let Some(sp) = t[..idx].rfind(' ') {
        let (dp, tp) = (&t[..sp], t[sp + 1..].trim());
        if let (Some((d, _)), Some(tm)) = (parse_date(dp.trim(), env), parse_time(tp, env.locale)) {
            return Some(combined(d, tm));
        }
        idx = sp;
    }

    for (sp, _) in t.match_indices(' ') {
        let (tp, dp) = (t[..sp].trim(), t[sp + 1..].trim());
        if tp.ends_with(env.locale.time_sep) {
            continue;
        }
        if let (Some(tm), Some((d, _))) = (parse_time(tp, env.locale), parse_date(dp, env)) {
            return Some(combined(d, tm));
        }
    }
    None
}

fn expand_year(y: u32, digits: usize) -> Option<i64> {
    if digits == 1 && y == 0 {
        None
    } else if digits <= 2 {
        Some(if y < 30 {
            2000 + y as i64
        } else {
            1900 + y as i64
        })
    } else if digits == 4 && (1900..=9999).contains(&y) {
        Some(y as i64)
    } else {
        None
    }
}

fn parse_date(t: &str, env: ParseEnv) -> Option<(f64, Option<&'static str>)> {
    let is_sep = |c: char| c == env.locale.date_sep || c == '-' || c == '/';
    if t.contains(' ') {
        let cs: Vec<char> = t.chars().collect();
        let mut out = String::with_capacity(t.len());
        let mut k = 0;
        while k < cs.len() {
            if cs[k] == ' ' {
                let mut e = k;
                while e < cs.len() && cs[e] == ' ' {
                    e += 1;
                }
                let prev_sep = out.chars().next_back().is_some_and(is_sep);
                let next_sep = cs.get(e).copied().is_some_and(is_sep);
                if !prev_sep && !next_sep && !out.is_empty() && e < cs.len() {
                    out.push(' ');
                }
                k = e;
            } else {
                out.push(cs[k]);
                k += 1;
            }
        }
        let b = out.as_bytes();
        if b.windows(3)
            .any(|w| w[0].is_ascii_digit() && w[1] == b' ' && w[2].is_ascii_digit())
            && out.chars().any(is_sep)
        {
            return None;
        }
        if out != t {
            return parse_date(&out, env);
        }
    }
    let seps: &[char] = &[env.locale.date_sep, '-', ' ', '/'];
    let raw: Vec<&str> = t.split(|c| seps.contains(&c)).collect();
    if raw.len() > 3 || raw.iter().any(|p| p.is_empty()) {
        return None;
    }

    if raw
        .iter()
        .all(|p| p.bytes().all(|b| b.is_ascii_digit() || b == b','))
        && !t
            .chars()
            .any(|c| c == env.locale.date_sep || c == '-' || c == '/')
    {
        return None;
    }

    let mut split: Vec<&str> = Vec::new();
    for p in &raw {
        let mut start = 0;
        let pb = p.as_bytes();
        for k in 1..pb.len() {
            if pb[k].is_ascii_digit() != pb[k - 1].is_ascii_digit()
                && pb[k].is_ascii_alphanumeric()
                && pb[k - 1].is_ascii_alphanumeric()
            {
                split.push(&p[start..k]);
                start = k;
            }
        }
        split.push(&p[start..]);
    }
    if split.len() < 2 || split.len() > 3 {
        return None;
    }
    let mut parts = Vec::with_capacity(3);
    for p in &split {
        let p = p.trim_end_matches(',');
        if p.bytes().all(|b| b.is_ascii_digit()) {
            if p.len() > 4 {
                return None;
            }
            parts.push(DatePart::Num(p.parse().ok()?, p.len()));
        } else {
            parts.push(DatePart::Month(month_from_name(p)?));
        }
    }
    let ymd = |y: i64, m: u32, d: u32| -> Option<f64> {
        if !(1..=12).contains(&m)
            || d == 0
            || d > datetime::days_in_month(y, m) && !(y == 1900 && m == 2 && d == 29)
        {
            return None;
        }
        let s = if env.date_system == DateSystem::D1900 && y == 1900 && m == 2 && d == 29 {
            60
        } else {
            datetime::serial_from_ymd(env.date_system, y, m as i64, d as i64)
        };
        (s >= 0 && (s as f64) <= env.date_system.max_serial()).then_some(s as f64)
    };
    use DatePart::*;
    match parts.as_slice() {
        [Num(y, 4), Num(m, _), Num(d, _)] => ymd(*y as i64, *m, *d).map(|v| (v, None)),
        [Num(a, al), Num(b, bl), Num(c, cl)] => {
            let (d, m, y, yl) = match env.locale.date_order {
                DateOrder::DMY => (*a, *b, *c, *cl),
                DateOrder::MDY => (*b, *a, *c, *cl),
                DateOrder::YMD => (*c, *b, *a, *al),
            };
            let _ = bl;
            ymd(expand_year(y, yl)?, m, d).map(|v| (v, None))
        }
        [Num(a, _), Num(b, bl)] => {
            let (d, m) = match env.locale.date_order {
                DateOrder::MDY => (*b, *a),
                _ => (*a, *b),
            };
            if (1..=12).contains(&m) && d >= 1 && d <= 31 && *bl <= 2 {
                if let Some(v) = ymd(env.current_year, m, d) {
                    return Some((v, Some("d-mmm")));
                }
            }

            if (1..=12).contains(a) && *b != 0 {
                return ymd(expand_year(*b, *bl)?, *a, 1).map(|v| (v, Some("mmm-yy")));
            }
            None
        }
        [Num(d, _), Month(m), Num(y, yl)] => {
            ymd(expand_year(*y, *yl)?, *m, *d).map(|v| (v, Some("d-mmm-yy")))
        }
        [Num(d, dl), Month(m)] if *dl <= 2 => {
            ymd(env.current_year, *m, *d).map(|v| (v, Some("d-mmm")))
        }

        [Month(m), Num(y, yl)] => ymd(expand_year(*y, *yl)?, *m, 1).map(|v| (v, Some("mmm-yy"))),
        _ => None,
    }
}

pub fn to_number(v: &Value, env: ParseEnv) -> Result<f64, XlError> {
    match v {
        Value::Number(n) => Ok(*n),
        Value::Blank => Ok(0.0),
        Value::Bool(b) => Ok(if *b { 1.0 } else { 0.0 }),
        Value::Text(s) => parse_number_text(s, env)
            .map(|p| p.value)
            .ok_or(XlError::Value),
        Value::Error(e) => Err(*e),
        Value::Array(_) => Err(XlError::Value),
        Value::Lambda(_) => Err(XlError::Value),
    }
}

pub fn to_text(v: &Value) -> Result<Text, XlError> {
    match v {
        Value::Text(s) => Ok(s.clone()),
        Value::Number(n) => Ok(number::format_general(*n).into()),
        Value::Bool(b) => Ok(if *b { "TRUE" } else { "FALSE" }.into()),
        Value::Blank => Ok("".into()),
        Value::Error(e) => Err(*e),
        Value::Array(_) => Err(XlError::Value),
        Value::Lambda(_) => Err(XlError::Value),
    }
}

pub fn to_bool(v: &Value) -> Result<bool, XlError> {
    match v {
        Value::Bool(b) => Ok(*b),
        Value::Number(n) => Ok(*n != 0.0),
        Value::Blank => Ok(false),
        Value::Text(s) => {
            if s.eq_ignore_ascii_case("TRUE") {
                Ok(true)
            } else if s.eq_ignore_ascii_case("FALSE") {
                Ok(false)
            } else {
                Err(XlError::Value)
            }
        }
        Value::Error(e) => Err(*e),
        Value::Array(_) => Err(XlError::Value),
        Value::Lambda(_) => Err(XlError::Value),
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    fn env(loc: &Locale) -> ParseEnv<'_> {
        ParseEnv {
            locale: loc,
            date_system: DateSystem::D1900,
            current_year: 2026,
            iso_t: true,
        }
    }

    fn num(s: &str) -> Option<f64> {
        let loc = Locale::en_nz();
        parse_number_text(s, env(&loc)).map(|p| p.value)
    }

    #[test]
    fn plain_numbers() {
        assert_eq!(num(" 3 "), Some(3.0));
        assert_eq!(num("1,000"), Some(1000.0));
        assert_eq!(num("$1,000"), Some(1000.0));
        assert_eq!(num("-$5"), Some(-5.0));
        assert_eq!(num("50%"), Some(0.5));
        assert_eq!(num("1e3"), Some(1000.0));
        assert_eq!(num("(5)"), Some(-5.0));
        assert_eq!(num("1,00"), None);
        assert_eq!(num("TRUE"), None);
        assert_eq!(num(""), None);
    }

    #[test]
    fn dates_and_times() {
        assert_eq!(num("1/2/2020"), Some(43862.0));
        assert_eq!(num("2020-01-02"), Some(43832.0));
        assert_eq!(num("12:30"), Some(0.5208333333333334));
        assert_eq!(num("2-Jan-2020"), Some(43832.0));
        assert_eq!(num("12:00 PM"), Some(0.5));
        assert_eq!(num("12:00 AM"), Some(0.0));
        assert_eq!(num("1/2/2020 12:00"), Some(43862.5));
    }
}
