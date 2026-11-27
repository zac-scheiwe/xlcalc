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
        let (n, system) = normalize_digits(s);
        if let Some((nn, lcid)) = system {
            let mut p = parse_number_text_with(&n, env, keep_lead)?;
            let ethiopic = nn == ETHIOPIC_NN;
            if ethiopic
                && matches!(
                    p.kind,
                    NumberKind::Date | NumberKind::DateTime | NumberKind::Time { .. }
                )
            {
                return None;
            }
            p.format = native_digit_format(&p, &n, nn, lcid, env);
            return Some(p);
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

const DIGIT_SYSTEMS: [(u32, u32, u32); 17] = [
    (0x660, 0x2, 0x401),
    (0x6F0, 0x3, 0x401),
    (0x966, 0x4, 0x439),
    (0x9E6, 0x5, 0x445),
    (0xA66, 0x6, 0x446),
    (0xAE6, 0x7, 0x447),
    (0xB66, 0x8, 0x448),
    (0xBE6, 0x9, 0x449),
    (0xC66, 0xA, 0x44A),
    (0xCE6, 0xB, 0x44B),
    (0xD66, 0xC, 0x44C),
    (0xE50, 0xD, 0x41E),
    (0xED0, 0xE, 0x454),
    (0xF20, 0xF, 0x451),
    (0x1040, 0x10, 0x455),
    (0x17E0, 0x12, 0x453),
    (0x1810, 0x13, 0x450),
];

pub(crate) fn national_digit(c: char) -> Option<char> {
    let u = c as u32;
    DIGIT_SYSTEMS
        .iter()
        .find(|&&(z, ..)| (z..z + 10).contains(&u))
        .and_then(|&(z, ..)| char::from_digit(u - z, 10))
}

const ETHIOPIC_ONE: u32 = 0x1369;
const ETHIOPIC_NN: u32 = 0x11;
const ETHIOPIC_LCID: u32 = 0x45E;

fn normalize_digits(s: &str) -> (String, Option<(u32, u32)>) {
    let mut system = None;
    let n = s
        .chars()
        .map(|c| {
            let u = c as u32;
            if let Some(&(z, nn, lcid)) = DIGIT_SYSTEMS
                .iter()
                .find(|&&(z, ..)| (z..z + 10).contains(&u))
            {
                system = Some((nn, lcid));
                char::from(b'0' + (u - z) as u8)
            } else if (ETHIOPIC_ONE..ETHIOPIC_ONE + 9).contains(&u) {
                system = Some((ETHIOPIC_NN, ETHIOPIC_LCID));
                char::from(b'1' + (u - ETHIOPIC_ONE) as u8)
            } else {
                c
            }
        })
        .collect();
    (n, system)
}

fn native_digit_format(
    p: &ParsedNumber,
    text: &str,
    nn: u32,
    lcid: u32,
    env: ParseEnv,
) -> Option<String> {
    let tag = format!("[$-{:X}]", (nn << 24) | lcid);
    let base = p
        .format
        .clone()
        .or_else(|| crate::input::auto_format(&p.kind, &env));
    if base
        .as_deref()
        .is_some_and(|f| f == "0.00E+00" || f.starts_with("# ?"))
    {
        return p.format.clone();
    }
    let loc = env.locale;
    match p.kind {
        NumberKind::Plain => {
            let dec = text
                .split_once(loc.decimal_sep)
                .map_or(0, |(_, r)| r.bytes().take_while(u8::is_ascii_digit).count());
            let hashes = "#".repeat(dec.min(14));
            Some(format!("{tag}0{}{hashes}", if dec > 0 { "." } else { "" }))
        }
        NumberKind::Percent => Some(format!("{tag}0.00%")),
        NumberKind::Thousands => base.map(|f| format!("{tag}{f}")),
        NumberKind::Currency => {
            let c = format!("[${}-{:X}]", loc.currency, (nn << 24) | lcid);
            Some(format!("{c} #,##0.00_);[Red]({c}#,##0.00)"))
        }
        NumberKind::Scientific => p.format.clone(),
        NumberKind::Time { .. } => base.map(|f| format!("[$-{:X}]{f}", nn << 24)),
        NumberKind::Date => base.map(|f| format!("[$-{:X}]{}", nn << 24, widen_date_format(&f))),
        NumberKind::DateTime => Some(format!("[$-{:X}]m/d/yyyy h:mm", nn << 24)),
    }
}

fn widen_date_format(f: &str) -> String {
    let b: Vec<char> = f.chars().collect();
    let mut out = String::new();
    let mut i = 0;
    while i < b.len() {
        let c = b[i];
        let run = b[i..].iter().take_while(|&&x| x == c).count();
        let n = match (c, run) {
            ('d' | 'm', 1) => 2,
            ('y', 2) => 4,
            _ => run,
        };
        out.extend(std::iter::repeat_n(c, n));
        i += run;
    }
    out
}

const IFMT_GROUP: u32 = 3;
const IFMT_CURRENCY: u32 = 10;
const IFMT_PCT: u32 = 13;
const IFMT_PCT2: u32 = 14;
const IFMT_SCI: u32 = 15;
const IFMT_FRAC1: u32 = 17;
const IFMT_FRAC2: u32 = 18;

struct TypedParts {
    has_dec: bool,
    group: bool,
    currency: bool,
    percent: bool,
    euro: bool,
}

fn typed_ifmt(core: Option<u32>, t: TypedParts) -> Option<u32> {
    if core.is_some() {
        return core;
    }
    if t.euro {
        return None;
    }
    let dec = u32::from(t.has_dec);
    if t.currency {
        Some(IFMT_CURRENCY + 2 * dec)
    } else if t.percent {
        Some(IFMT_PCT + dec)
    } else if t.group {
        Some(IFMT_GROUP + dec)
    } else {
        None
    }
}

fn typed_ifmt_format(id: u32, loc: &Locale) -> Option<String> {
    Some(match id {
        3 => "#,##0".to_string(),
        4 => "#,##0.00".to_string(),
        IFMT_PCT => "0%".to_string(),
        IFMT_PCT2 => "0.00%".to_string(),
        IFMT_SCI => "0.00E+00".to_string(),
        IFMT_FRAC1 => "# ?/?".to_string(),
        IFMT_FRAC2 => "# ??/??".to_string(),

        IFMT_CURRENCY | 12 => currency_fmt_code(loc, true, if id == 12 { 2 } else { 0 }),
        _ => return None,
    })
}

fn currency_fmt_code(loc: &Locale, red: bool, decimals: usize) -> String {
    let digits = if decimals == 0 {
        "#,##0".to_string()
    } else {
        format!("#,##0.{}", "0".repeat(decimals))
    };
    let c = loc.currency.as_str();
    let tag = if red { "[Red]" } else { "" };
    format!("{c}{digits};{tag}-{c}{digits}")
}

fn parse_plain(t: &str, loc: &Locale) -> Option<ParsedNumber> {
    let cs: Vec<char> = t.chars().collect();
    let own = Currency {
        symbol: &loc.currency,
        before: true,
        euro: false,
    };
    let mut euro_seen = false;
    if let Some(p) = scan_number(&cs, loc, own, &mut euro_seen) {
        return Some(p);
    }
    if !euro_seen {
        return None;
    }

    [true, false].into_iter().find_map(|before| {
        let euro = Currency {
            symbol: "\u{20ac}",
            before,
            euro: true,
        };
        scan_number(&cs, loc, euro, &mut euro_seen)
    })
}

#[derive(Clone, Copy)]
struct Currency<'a> {
    symbol: &'a str,
    before: bool,
    euro: bool,
}

impl Currency<'_> {
    fn read(&self, cs: &[char], from: usize) -> Option<usize> {
        let symbol = self.symbol.strip_prefix(' ').unwrap_or(self.symbol);
        if symbol.is_empty() {
            return None;
        }
        let mut i = from;
        while cs.get(i) == Some(&' ') {
            i += 1;
        }
        for s in symbol.chars() {
            if cs.get(i) != Some(&s) {
                return None;
            }
            i += 1;
        }
        Some(i)
    }
}

struct Prefix {
    start: usize,
    negative: bool,
    parens: u32,
    percents: u32,
    currencies: u32,
}

fn scan_prefix(
    cs: &[char],
    loc: &Locale,
    currency: Currency,
    euro_seen: &mut bool,
) -> Option<Prefix> {
    let mut p = Prefix {
        start: 0,
        negative: false,
        parens: 0,
        percents: 0,
        currencies: 0,
    };
    let mut signs = 0;
    let mut i = 0;
    loop {
        let c = *cs.get(i)?;
        if c == loc.decimal_sep {
            cs.get(i + 1).filter(|d| d.is_ascii_digit())?;
            break;
        }
        if c.is_ascii_digit() {
            break;
        }
        *euro_seen |= c == '\u{20ac}';
        if currency.before {
            if let Some(next) = currency.read(cs, i) {
                p.currencies += 1;
                i = next;
                continue;
            }
        }
        match c {
            ' ' => {}
            '%' => p.percents += 1,
            '(' | '+' | '-' => {
                if signs > 0 {
                    return None;
                }
                signs += 1;
                if c == '(' {
                    p.parens += 1;
                }
                p.negative |= c != '+';
            }
            _ => return None,
        }
        i += 1;
    }
    p.start = i;
    Some(p)
}

fn scan_number(
    cs: &[char],
    loc: &Locale,
    currency: Currency,
    euro_seen: &mut bool,
) -> Option<ParsedNumber> {
    let mut p = scan_prefix(cs, loc, currency, euro_seen)?;
    let rest = &cs[p.start..];
    let core = parse_number_core(rest, p.negative, p.percents > 0 || rest.contains(&'%'), loc)?;
    let mut i = p.start + core.end;
    while let Some(&c) = cs.get(i) {
        *euro_seen |= c == '\u{20ac}';
        if !currency.before {
            if let Some(next) = currency.read(cs, i) {
                p.currencies += 1;
                i = next;
                continue;
            }
        }
        match c {
            ' ' => {}
            '%' => p.percents += 1,
            ')' => p.parens = p.parens.checked_sub(1)?,
            _ => return None,
        }
        i += 1;
    }
    if p.parens > 0 || p.currencies + p.percents > 1 {
        return None;
    }
    let kind = if p.percents > 0 {
        NumberKind::Percent
    } else if p.currencies > 0 {
        NumberKind::Currency
    } else if core.ifmt == Some(IFMT_SCI) {
        NumberKind::Scientific
    } else if core.grouped {
        NumberKind::Thousands
    } else {
        NumberKind::Plain
    };
    let mut parsed = ParsedNumber::new(number::normalize(core.value), kind);
    let ifmt = typed_ifmt(
        core.ifmt,
        TypedParts {
            has_dec: core.decimal,
            group: parsed.kind == NumberKind::Thousands,
            currency: parsed.kind == NumberKind::Currency,
            percent: parsed.kind == NumberKind::Percent,
            euro: currency.euro,
        },
    );
    parsed.format = ifmt.and_then(|id| typed_ifmt_format(id, loc));
    Some(parsed)
}

struct CoreNumber {
    value: f64,
    end: usize,
    ifmt: Option<u32>,
    decimal: bool,
    grouped: bool,
}

struct DigitGroups {
    sizes: Vec<u32>,
    uniform: Option<u32>,
}

impl DigitGroups {
    fn from_sgrouping(g: &str) -> DigitGroups {
        let mut sizes: Vec<u32> = g
            .chars()
            .step_by(2)
            .map(|c| c as u32 - '0' as u32)
            .collect();
        if sizes.first() == Some(&0) {
            sizes = vec![0x7fff_ffff, 0x7fff_ffff];
        }
        let uniform = (sizes.len() == 2 && sizes[1] == 0).then_some(sizes[0]);
        DigitGroups { sizes, uniform }
    }

    fn size_for(&self, remaining: u32) -> u32 {
        if let Some(u) = self.uniform {
            return u;
        }
        let count = self.sizes.len() as u32;
        let mut idx = remaining.wrapping_sub(1);
        if count <= idx && count > 1 && self.sizes[count as usize - 1] == 0 {
            idx = count - 2;
        }
        if (idx as i32) < 0 || count - 1 < idx {
            0x7fff_ffff
        } else {
            self.sizes[idx as usize]
        }
    }
}

fn parse_number_core(
    c: &[char],
    negative: bool,
    percent: bool,
    loc: &Locale,
) -> Option<CoreNumber> {
    let at = |i: usize| c.get(i).copied().unwrap_or('\0');
    let digit = |ch: char| ch.is_ascii_digit();
    let groups = DigitGroups::from_sgrouping("3;0");
    let mut i = 0;
    while at(i) == '0' {
        i += 1;
    }

    let mut remaining: u32 = 1;
    if groups.uniform.is_none() {
        remaining = 0;
        let mut j = i;
        while at(j) != '\0' {
            if at(j) == loc.thousands_sep {
                remaining += 1;
            } else if !digit(at(j)) {
                break;
            }
            j += 1;
        }
    }
    let mut digits: Vec<u8> = Vec::new();
    let mut exp: i64 = 0;
    let mut any_int = false;
    let mut grouped = false;
    while digit(at(i)) {
        any_int = true;
        exp += 1;
        if digits.len() < 15 {
            digits.push(at(i) as u8 - b'0');
        }
        i += 1;
        if remaining > 0 && at(i) != '\0' && at(i) == loc.thousands_sep {
            let size = groups.size_for(remaining);
            let mut found = 0u32;
            if size != 0 {
                let mut j = i + 1;
                while at(j) != '\0' && digit(at(j)) {
                    found += 1;
                    if size <= found {
                        break;
                    }
                    j += 1;
                }
            }
            if found == size {
                if groups.uniform.is_none() {
                    remaining -= 1;
                }
                grouped = true;
                i += 1;
            }
        }
    }
    let mut ifmt = None;
    let mut decimal = false;
    let mut fraction: Option<f64> = None;

    let mixed = (at(i) == ' ').then(|| scan_fraction(c, i + 1)).flatten();
    if let Some((n, d, end)) = mixed {
        fraction = Some(n / d);
        ifmt = Some(if d > 9.0 { IFMT_FRAC2 } else { IFMT_FRAC1 });
        decimal = true;
        i = end;
    } else {
        if at(i) == loc.decimal_sep {
            decimal = true;
            i += 1;
        }
        if !any_int {
            while at(i) == '0' {
                i += 1;
                exp -= 1;
            }
        }
        if decimal {
            while digit(at(i)) {
                if digits.len() < 15 {
                    digits.push(at(i) as u8 - b'0');
                }
                i += 1;
            }
        }
        let stars = at(i) == '*' && at(i + 1) == '*';
        if matches!(at(i), 'e' | 'E') || stars {
            if stars {
                i += 1;
            }
            i += 1;
            let mut neg = false;
            match at(i) {
                '+' => i += 1,
                '-' => {
                    neg = true;
                    i += 1;
                }
                _ => {}
            }

            let mut v: u32 = 0;
            let st = i;
            while digit(at(i)) {
                if v > 0x0ccc_cccc {
                    return None;
                }
                v = v * 10 + (at(i) as u32 - '0' as u32);
                if v > i32::MAX as u32 {
                    return None;
                }
                i += 1;
            }
            if i == st {
                return None;
            }
            exp += if neg { -i64::from(v) } else { i64::from(v) };
            ifmt = Some(IFMT_SCI);
        }
    }
    if percent {
        if let Some(f) = fraction.as_mut() {
            *f /= 100.0;
            ifmt = Some(IFMT_PCT2);
        }
        exp -= 2;
    }
    if exp.abs() > 308 {
        return None;
    }
    let d = digits.iter().fold(0u64, |a, &x| a * 10 + u64::from(x));
    let mut value = number::pack_decimal(d, exp - digits.len() as i64);
    if let Some(f) = fraction {
        value += f;
    }
    if negative {
        value = -value;
    }
    Some(CoreNumber {
        value,
        end: i,
        ifmt,
        decimal,
        grouped,
    })
}

pub fn bare_fraction_as_mixed(s: &str, loc: &Locale) -> Option<String> {
    let cs: Vec<char> = s.trim_matches(' ').chars().collect();
    let own = Currency {
        symbol: &loc.currency,
        before: true,
        euro: false,
    };
    let start = scan_prefix(&cs, loc, own, &mut false)?.start;
    let zeros = cs[start..].iter().take_while(|&&c| c == '0').count();
    scan_fraction(&cs, start + zeros)?;
    let mut mixed: String = cs[..start].iter().collect();
    mixed.push_str("0 ");
    mixed.extend(&cs[start..]);
    parse_plain(&mixed, loc).map(|_| mixed)
}

fn scan_fraction(b: &[char], i: usize) -> Option<(f64, f64, usize)> {
    let int = |mut j: usize| -> Option<(f64, usize)> {
        let mut v: u32 = 0;
        let st = j;
        while let Some(d) = b.get(j).and_then(|c| c.to_digit(10)) {
            if j - st == 5 {
                return None;
            }
            v = v * 10 + d;
            j += 1;
        }
        (j > st && v <= 32767).then_some((f64::from(v), j))
    };
    let (n, j) = int(i)?;
    if b.get(j) != Some(&'/') {
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

const YEAR_PIVOT: i32 = 1930;

fn expand_year(y: i32) -> i32 {
    let p = YEAR_PIVOT % 100;
    (if p <= y { y } else { y + 100 }) - p + YEAR_PIVOT
}

pub(crate) fn parse_datetime_text(t: &str, env: ParseEnv) -> Option<ParsedNumber> {
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
    if lead {
        seps.push(SEP_NONE);
    }
    let mut prev = SEP_NONE;
    let mut month_seen = false;
    let mut ampm_seen = false;
    let mut frac_digits = 0usize;
    let mut pos = 0;
    while pos < cs.len() {
        if seps.len() >= 9 {
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
                let k = seps.len();
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
    let n = seps.len();
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

        if env.date_system == DateSystem::D1900Gregorian && y == 1900 && month == 2 && d == 29 {
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

    #[test]
    fn number_core_edges() {
        assert_eq!(num("12345678901234567"), Some(12345678901234500.0));
        assert_eq!(num("0.1234567890123456789"), Some(0.123456789012345));

        assert_eq!(num("1,0000"), Some(10000.0));
        assert_eq!(num("1,23"), None);
        assert_eq!(num("0,001"), None);
        assert_eq!(num("1 1/2"), Some(1.5));
        assert_eq!(num("1 1/0"), None);

        assert_eq!(num("1e307"), Some(1e307));
        assert_eq!(num("1e308"), None);
        assert_eq!(num("1e"), None);
        assert_eq!(num("1e+"), None);
        assert_eq!(num(".5"), Some(0.5));
        assert_eq!(num("0.000e5"), Some(0.0));
        assert_eq!(num("1e308%"), Some(1e306));
    }

    #[test]
    fn digit_groups_table() {
        let g = DigitGroups::from_sgrouping("3;0");
        assert_eq!(g.uniform, Some(3));
        let g = DigitGroups::from_sgrouping("3;2;0");
        assert_eq!(g.uniform, None);
        assert_eq!(g.size_for(1), 3);
        assert_eq!(g.size_for(2), 2);
        assert_eq!(g.size_for(3), 0);
    }
}
