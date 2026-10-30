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
    parse_number_text_with(s, env, false)
}

pub fn parse_number_text_arith(s: &str, env: ParseEnv) -> Option<ParsedNumber> {
    parse_number_text_with(s, env, true)
}

fn parse_number_text_with(s: &str, env: ParseEnv, keep_lead: bool) -> Option<ParsedNumber> {
    if !s.is_ascii() {
        let n = normalize_digits(s);
        if n != s {
            return parse_number_text_with(&n, env, keep_lead);
        }
    }
    let t = s.trim_matches(' ');
    if t.is_empty() {
        return None;
    }
    if let Some(p) = parse_plain(t, env.locale) {
        return Some(p);
    }
    if keep_lead && s.starts_with(' ') {
        return parse_datetime_text_lead(t, env, true);
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

fn parse_plain(t: &str, loc: &Locale) -> Option<ParsedNumber> {
    let mut s = t;
    let mut negative = false;
    let mut kind = NumberKind::Plain;
    let mut euro = false;
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
            euro = true;
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
            euro = true;
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
    let mut first_group_zero = false;
    while i < b.len() {
        let c = b[i] as char;
        if c.is_ascii_digit() {
            clean.push(c);
            int_digits += 1;
            if let Some(g) = group_len.as_mut() {
                *g += 1;
            }
        } else if c == loc.thousands_sep
            && !(c == ' '
                && int_digits > 0
                && b[i + 1..].iter().take_while(|d| d.is_ascii_digit()).count() < 3
                && scan_fraction(b, i + 1).is_some())
        {
            if int_digits == 0 || group_len.is_some_and(|g| g < 3) {
                return None;
            }
            if !grouped {
                first_group_zero = clean.bytes().all(|c| c == b'0');
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

    if grouped && (first_group_zero || !clean.bytes().any(|c| (b'1'..=b'9').contains(&c))) {
        return None;
    }

    let mut fraction = None;
    if int_digits > 0 && b.get(i) == Some(&b' ') {
        if let Some((n, d, end)) = scan_fraction(b, i + 1) {
            fraction = Some((n, d));
            i = end;
        }
    }
    let mut sci = false;
    if fraction.is_none() && i < b.len() && b[i] as char == loc.decimal_sep {
        clean.push('.');
        i += 1;
        while i < b.len() && b[i].is_ascii_digit() {
            clean.push(b[i] as char);
            i += 1;
        }
    }
    if fraction.is_none() && i < b.len() && (b[i] == b'e' || b[i] == b'E') {
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

    if let Some((n, d)) = fraction {
        let q = n / d;
        v += if percent { q / 100.0 } else { q };
    }
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
        NumberKind::Percent if fraction.is_some() => Some("0.00%".to_string()),

        NumberKind::Plain | NumberKind::Thousands | NumberKind::Currency if fraction.is_some() => {
            Some(
                if fraction.is_some_and(|(_, d)| d >= 10.0) {
                    "# ??/??"
                } else {
                    "# ?/?"
                }
                .to_string(),
            )
        }
        NumberKind::Percent => Some(if dec { "0.00%" } else { "0%" }.to_string()),
        NumberKind::Thousands => Some(if dec { "#,##0.00" } else { "#,##0" }.to_string()),

        NumberKind::Currency if euro => None,
        NumberKind::Currency => {
            let (c, d) = (loc.currency.as_str(), if dec { ".00" } else { "" });
            Some(format!("{c}#,##0{d};[Red]-{c}#,##0{d}"))
        }
        _ => None,
    };
    Some(p)
}

pub fn bare_fraction_as_mixed(s: &str) -> Option<String> {
    let t = s.trim_matches(' ');
    let b = t.as_bytes();
    if t.starts_with(['=', '\'']) {
        return None;
    }
    let start = b.iter().position(|c| c.is_ascii_digit())?;
    if !t[..start].chars().all(|c| {
        matches!(c, '(' | '+' | '-' | ' ')
            || !c.is_alphanumeric() && c != '/' && c != '.' && c != ','
    }) {
        return None;
    }
    let (n, d, end) = scan_fraction(b, start)?;
    if n == 0.0 || d == 0.0 || !t[end..].chars().all(|c| matches!(c, ')' | '%' | ' ')) {
        return None;
    }
    Some(format!("{}0 {}", &t[..start], &t[start..]))
}

fn scan_fraction(b: &[u8], i: usize) -> Option<(f64, f64, usize)> {
    let int = |mut j: usize| -> Option<(f64, usize)> {
        let st = j;
        while j < b.len() && b[j].is_ascii_digit() {
            j += 1;
        }
        if j == st || j - st > 5 {
            return None;
        }
        let v: u32 = std::str::from_utf8(&b[st..j]).ok()?.parse().ok()?;
        (v <= 32767).then_some((v as f64, j))
    };
    let (n, j) = int(i)?;
    if b.get(j) != Some(&b'/') {
        return None;
    }
    let (d, j) = int(j + 1)?;
    (d != 0.0).then_some((n, d, j))
}

const MISSING: i32 = -10000;

const CODE_AM: i32 = -22;
const CODE_PM: i32 = -21;

const SEP_NONE: i32 = 0;
const SEP_SPACE: i32 = 1;
const SEP_DATE: i32 = 2;
const SEP_TIME: i32 = 3;
const SEP_DECIMAL: i32 = 4;

const SEP_INTL: [&str; 3] = [" ", ", ", ""];

#[derive(Clone, Copy)]
struct DtToken {
    d: f64,

    v: i32,
}

fn alpha_code(word: &str) -> i32 {
    if word.chars().count() >= 3 {
        if let Some(m) = month_from_name(word) {
            return -(m as i32);
        }
    }
    let lower = word.to_ascii_lowercase();
    if !lower.is_empty() && lower.len() <= 2 {
        if "am".starts_with(&lower) {
            return CODE_AM;
        }
        if "pm".starts_with(&lower) {
            return CODE_PM;
        }
    }
    0
}

fn next_sep(cs: &[char], pos: &mut usize, prev: i32, loc: &Locale) -> i32 {
    let mut cls = SEP_NONE;
    while cs.get(*pos) == Some(&' ') {
        *pos += 1;
        cls = SEP_SPACE;
    }
    let c = match cs.get(*pos) {
        Some(&c) if !c.is_alphanumeric() => c,
        _ => {
            if cls == SEP_SPACE {
                if loc.date_sep == ' ' {
                    return SEP_DATE;
                }
                if loc.time_sep == ' ' {
                    return SEP_TIME;
                }
                if loc.decimal_sep == ' ' && prev == SEP_TIME {
                    return SEP_DECIMAL;
                }
            }
            return cls;
        }
    };
    if c == loc.date_sep || c == '-' || c == '/' {
        *pos += 1;
        return SEP_DATE;
    }
    if c == loc.time_sep || c == ':' {
        *pos += 1;
        return SEP_TIME;
    }
    if c == loc.decimal_sep && prev == SEP_TIME {
        *pos += 1;
        return SEP_DECIMAL;
    }
    for (k, s) in SEP_INTL.iter().enumerate() {
        let sc: Vec<char> = s.chars().collect();
        if !sc.is_empty() && cs[*pos..].starts_with(&sc) {
            *pos += sc.len();
            return 5 + k as i32;
        }
    }
    -1
}

fn expand_year(y: i32) -> i32 {
    if y < 30 {
        2000 + y
    } else {
        1900 + y
    }
}

fn parse_datetime_text(t: &str, env: ParseEnv) -> Option<ParsedNumber> {
    parse_datetime_text_lead(t, env, false)
}

fn parse_datetime_text_lead(t: &str, env: ParseEnv, lead: bool) -> Option<ParsedNumber> {
    let loc = env.locale;
    if !t.bytes().any(|b| b.is_ascii_digit()) {
        return None;
    }
    let cs: Vec<char> = t.chars().collect();
    let mut toks: Vec<DtToken> = Vec::new();

    let mut seps: Vec<i32> = Vec::new();
    let mut prev = SEP_NONE;
    let mut month_seen = false;
    let mut ampm_seen = false;
    let mut frac_digits = 0usize;
    let mut pos = 0;
    while pos < cs.len() {
        if toks.len() >= 9 {
            return None;
        }
        let c = cs[pos];
        if c.is_alphabetic() {
            let start = pos;
            while pos < cs.len() && cs[pos].is_alphabetic() {
                pos += 1;
            }
            let word: String = cs[start..pos].iter().collect();
            let code = alpha_code(&word);
            if code == 0 {
                return None;
            }
            if code > -20 {
                if month_seen {
                    return None;
                }
                month_seen = true;
            } else {
                let k = toks.len();
                if ampm_seen || k == 0 || seps[k - 1] != SEP_SPACE {
                    return None;
                }
                ampm_seen = true;
                seps[k - 1] = SEP_TIME;
                prev = SEP_TIME;
            }

            toks.push(DtToken {
                d: code as u32 as f64,
                v: code,
            });
        } else if c.is_ascii_digit() {
            let mut d = 0.0f64;
            let mut digits = 0usize;
            while pos < cs.len() && cs[pos].is_ascii_digit() {
                d = d * 10.0 + (cs[pos] as u8 - b'0') as f64;
                digits += 1;
                pos += 1;
            }
            let v = if d >= 2_147_483_648.0 {
                MISSING
            } else {
                d as i32
            };
            if prev == SEP_DECIMAL {
                frac_digits = digits;
            } else if digits > 4 || digits > 2 && v < 100 {
                return None;
            }
            toks.push(DtToken {
                d,
                v: v.max(MISSING),
            });
        } else {
            return None;
        }
        let sep = next_sep(&cs, &mut pos, prev, loc);
        if sep == -1 {
            return None;
        }
        seps.push(sep);
        prev = sep;
        while cs.get(pos) == Some(&' ') {
            pos += 1;
        }
    }
    if lead {
        seps.insert(0, SEP_SPACE);
    }
    let n = toks.len() + lead as usize;
    let tk = |k: usize| {
        toks.get(k).copied().unwrap_or(DtToken {
            d: MISSING as f64,
            v: MISSING,
        })
    };
    let sep_at = |k: usize| seps.get(k).copied().unwrap_or(SEP_NONE);
    let is_time_sep = |s: i32| s == SEP_TIME || s == 8;

    let (mut hour_i, mut min_i, mut sec_i, mut frac_i, mut ampm_i) = (None, None, None, None, None);
    let has_time;
    let (first, last): (i64, i64);
    match (0..n).find(|&k| is_time_sep(seps[k])) {
        None => {
            has_time = false;
            first = 0;
            last = n as i64 - 1;
        }
        Some(t0) => {
            has_time = true;
            if t0 != 0 && seps[t0 - 1] != SEP_SPACE {
                return None;
            }
            let mut e = t0;
            if e + 1 < n {
                loop {
                    let s = seps[e];
                    if is_time_sep(s) {
                        e += 1;
                        if e + 1 < n {
                            continue;
                        }
                    } else if s == SEP_DECIMAL {
                        e += 1;
                        frac_i = Some(e);
                        while e + 1 < n && is_time_sep(seps[e]) {
                            e += 1;
                        }
                    }
                    break;
                }
            }
            if tk(e).v == CODE_AM || tk(e).v == CODE_PM {
                ampm_i = Some(e);
            }
            let cnt = e as i64 - frac_i.is_some() as i64 - ampm_i.is_some() as i64 - t0 as i64;
            if frac_i.is_some() && cnt == 1 {
                min_i = Some(t0);
                sec_i = Some(t0 + 1);
            } else {
                match cnt {
                    0 => hour_i = Some(t0),
                    1 => {
                        hour_i = Some(t0);
                        min_i = Some(t0 + 1);
                    }
                    2 => {
                        hour_i = Some(t0);
                        min_i = Some(t0 + 1);
                        sec_i = Some(t0 + 2);
                    }
                    _ => return None,
                }
            }
            if (e + 1..n).any(|k| is_time_sep(seps[k])) {
                return None;
            }

            if t0 != 0 {
                first = 0;
                last = t0 as i64 - 1;
            } else {
                first = e as i64 + 1;
                last = n as i64 - 1;
            }
        }
    }

    let (mut day, mut month, mut year) = (MISSING, MISSING, MISSING);
    let mut numeric = false;
    if first <= last {
        let (f, l) = (first as usize, last as usize);
        let span = l - f;
        if span > 2 || sep_at(l) > SEP_SPACE {
            return None;
        }

        let v = |k: usize| toks.get(k).map_or(MISSING, |t| t.v);
        if !month_seen {
            numeric = true;
            match span {
                1 => {
                    if sep_at(f) != SEP_DATE {
                        return None;
                    }
                    numeric = false;
                    let (a, b) = (v(f), v(l));
                    match loc.date_order {
                        DateOrder::YMD => {
                            if a > 12 {
                                year = a;
                                month = b;
                            } else {
                                month = a;
                                day = b;
                            }
                        }
                        DateOrder::DMY => {
                            if b > 12 {
                                year = b;
                                month = a;
                            } else {
                                day = a;
                                month = b;
                            }
                        }
                        DateOrder::MDY => {
                            month = a;
                            if (1..=12).contains(&a)
                                && b != 0
                                && b <= days_in_month_1900(env.current_year as i32, a)
                            {
                                day = b;
                            } else {
                                year = b;
                            }
                        }
                    }
                }
                2 => {
                    if sep_at(f) != SEP_DATE || sep_at(f + 1) != SEP_DATE {
                        return None;
                    }
                    let order = if v(f) > 999 {
                        DateOrder::YMD
                    } else {
                        loc.date_order
                    };
                    (day, month, year) = match order {
                        DateOrder::YMD => (v(l), v(f + 1), v(f)),
                        DateOrder::DMY => (v(f), v(f + 1), v(l)),
                        DateOrder::MDY => (v(f + 1), v(f), v(l)),
                    };
                }
                _ => return None,
            }
        } else if sep_at(f) <= SEP_DATE && sep_at(f + 1) <= SEP_DATE {
            let a = v(f);
            if a < 0 {
                if span > 1 {
                    return None;
                }
                year = v(l);
                month = -a;
            } else {
                let b = v(f + 1);
                if b >= 0 {
                    return None;
                }
                day = a;
                month = -b;
                if f + 1 < l {
                    year = v(l);
                }
            }
        } else {
            if span != 2 {
                return None;
            }
            for k in f..=l {
                if sep_at(k) <= SEP_SPACE && SEP_INTL[k - f].chars().any(|c| c != ' ') {
                    return None;
                }
            }
            let (di, mi) = if loc.date_order == DateOrder::MDY {
                (f + 1, f)
            } else {
                (f, f + 1)
            };
            day = v(di);
            month = v(mi).abs();
            if day < 0 {
                return None;
            }
            year = v(l);
        }
    }

    let date = if month == MISSING {
        if day != MISSING || year != MISSING || !has_time {
            return None;
        }
        None
    } else {
        if !(1..=12).contains(&month) {
            return None;
        }
        if (0..=99).contains(&year) {
            if loc.date_order != DateOrder::DMY
                && day == MISSING
                && year >= 1
                && year <= days_in_month_1900(env.current_year as i32, month)
            {
                day = year;
                year = MISSING;
            } else {
                year = expand_year(year);
            }
        }
        if numeric && (year == MISSING || day < 0) {
            return None;
        }
        if day == MISSING && year == MISSING {
            return None;
        }
        let y = if year == MISSING {
            env.current_year as i32
        } else {
            year
        };
        let d = if day == MISSING { 1 } else { day };
        if !(1900..=9999).contains(&y) || d < 1 || d > days_in_month_1900(y, month) {
            return None;
        }
        let s = if env.date_system == DateSystem::D1900 && y == 1900 && month == 2 && d == 29 {
            60
        } else {
            datetime::serial_from_ymd(env.date_system, y as i64, month as i64, d as i64)
        };
        if s < 0 || s as f64 > env.date_system.max_serial() {
            return None;
        }
        let shape = if numeric {
            None
        } else if day == MISSING {
            Some("mmm-yy")
        } else if year == MISSING {
            Some("d-mmm")
        } else {
            Some("d-mmm-yy")
        };
        Some((s as f64, shape))
    };

    if !has_time {
        let (d, shape) = date?;
        let mut p = ParsedNumber::new(d, NumberKind::Date);
        p.format = shape.map(str::to_string);
        return Some(p);
    }

    let mut overflow: Option<(usize, u8)> = None;
    let mut field = |idx: Option<usize>, limit: f64, kind: u8| -> Option<f64> {
        let Some(i) = idx else { return Some(0.0) };
        if tk(i).d <= limit {
            return Some(tk(i).v as f64);
        }
        if overflow.is_some() {
            return None;
        }
        overflow = Some((i, kind));
        Some(tk(i).d)
    };
    let mut h = field(hour_i, 23.0, 0)?;
    let m = field(min_i, 59.0, 1)?;
    let s = field(sec_i, 59.0, 2)?;
    let mut frac = 0.0;
    if let Some(i) = frac_i {
        let mut x = if i < toks.len() { toks[i].d } else { 0.0 };
        let mut dg = frac_digits;
        while dg > 3 {
            x /= 10.0;
            dg -= 1;
        }
        let ms = (x + 0.5).trunc();
        frac = ms / 10f64.powi(dg as i32);
    }
    if let Some(i) = ampm_i {
        if overflow.is_some() || h > 12.0 {
            return None;
        }
        if tk(i).v == CODE_PM {
            if h < 12.0 {
                h += 12.0;
            }
        } else if h == 12.0 {
            h = 0.0;
        }
    }
    let time = (h * 3600.0 + m * 60.0 + (s + frac)) / 86_400.0;
    let format = if frac_i.is_some() {
        Some("mm:ss.0")
    } else {
        match overflow {
            Some((_, 0)) if date.is_none() => Some("[h]:mm:ss"),
            Some(_) => Some("General"),
            None => None,
        }
    };
    let mut p = match date {
        None => ParsedNumber::new(
            time,
            NumberKind::Time {
                seconds: sec_i.is_some(),
                ampm: ampm_i.is_some(),
            },
        ),
        Some((d, _)) => ParsedNumber::new(d + time, NumberKind::DateTime),
    };
    p.format = format.map(str::to_string);
    Some(p)
}

fn days_in_month_1900(y: i32, m: i32) -> i32 {
    if y == 1900 && m == 2 {
        29
    } else {
        datetime::days_in_month(y as i64, m as u32) as i32
    }
}

fn parse_date_time(t: &str, env: ParseEnv) -> Option<ParsedNumber> {
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
            let d = parse_datetime_text(dp, env).filter(|p| p.kind == NumberKind::Date);
            let tm =
                parse_datetime_text(tp, env).filter(|p| matches!(p.kind, NumberKind::Time { .. }));
            if let (Some(d), Some(tm)) = (d, tm) {
                let mut p = ParsedNumber::new(d.value + tm.value, NumberKind::DateTime);
                p.format = match tm.format.as_deref() {
                    Some("mm:ss.0") => tm.format,
                    Some("General" | "[h]:mm:ss") => Some("General".to_string()),
                    _ => None,
                };
                return Some(p);
            }
        }
    }
    parse_datetime_text(t, env)
}

pub fn to_number(v: &Value, env: ParseEnv) -> Result<f64, XlError> {
    match v {
        Value::Number(n) => Ok(*n),
        Value::Blank => Ok(0.0),
        Value::Bool(b) => Ok(if *b { 1.0 } else { 0.0 }),
        Value::Text(s) => parse_number_text_arith(s, env)
            .map(|p| p.value)
            .ok_or(XlError::VALUE),
        Value::Error(e) => Err(*e),
        Value::Array(_) => Err(XlError::VALUE),
        Value::Lambda(_) => Err(XlError::VALUE),
    }
}

pub fn to_text(v: &Value) -> Result<Text, XlError> {
    match v {
        Value::Text(s) => Ok(s.clone()),
        Value::Number(n) => Ok(number::format_general(*n).into()),
        Value::Bool(b) => Ok(if *b { "TRUE" } else { "FALSE" }.into()),
        Value::Blank => Ok("".into()),
        Value::Error(e) => Err(*e),
        Value::Array(_) => Err(XlError::VALUE),
        Value::Lambda(_) => Err(XlError::VALUE),
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
                Err(XlError::VALUE)
            }
        }
        Value::Error(e) => Err(*e),
        Value::Array(_) => Err(XlError::VALUE),
        Value::Lambda(_) => Err(XlError::VALUE),
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
