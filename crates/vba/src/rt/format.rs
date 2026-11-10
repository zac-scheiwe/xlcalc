use super::dates::{self, parts, Parts};
use super::util::{double, is_null, long_or, missing, text, text_out, text_out_null, R};
use super::RtContext;
use crate::VbaError;
use xlcalc::{Locale as EngineLocale, NumberFormat};
use xlcalc_variant::{Locale, Variant};

fn engine_locale(ctx: &RtContext) -> EngineLocale {
    if ctx.locale == Locale::EN_US {
        EngineLocale::en_us()
    } else {
        EngineLocale::en_nz()
    }
}

enum Named {
    Code(String),

    Words(&'static str, &'static str),

    General,

    GeneralDate,
}

fn named(ctx: &RtContext, fmt: &str) -> Option<Named> {
    let us = ctx.locale == Locale::EN_US;
    let code = |s: &str| Some(Named::Code(s.to_string()));
    match fmt.to_ascii_lowercase().as_str() {
        "general number" => Some(Named::General),
        "general date" => Some(Named::GeneralDate),
        "currency" => code("$#,##0.00;-$#,##0.00"),
        "fixed" => code("0.00"),
        "standard" => code("#,##0.00"),
        "percent" => code("0.00%"),
        "scientific" => code("0.00E+00"),
        "yes/no" => Some(Named::Words("Yes", "No")),
        "true/false" => Some(Named::Words("True", "False")),
        "on/off" => Some(Named::Words("On", "Off")),
        "short date" => code(if us { "m/d/yyyy" } else { "d/mm/yyyy" }),
        "long date" => code(if us {
            "dddd, mmmm d, yyyy"
        } else {
            "dddd, d mmmm yyyy"
        }),
        "medium date" => code("dd-mmm-yy"),
        "long time" => code("h:mm:ss AMPM"),
        "medium time" => code("hh:mm AMPM"),
        "short time" => code("hh:mm"),
        _ => None,
    }
}

fn vba_code(code: &str, dated: bool) -> String {
    let chars: Vec<char> = code.chars().collect();
    let mut out = String::new();
    let mut digits = false;
    let mut point = false;
    let mut i = 0;
    let lit = |out: &mut String, c: char| {
        out.push('"');
        out.push(c);
        out.push('"');
    };
    while i < chars.len() {
        let c = chars[i];
        match c {
            '"' => {
                let end = chars[i + 1..].iter().position(|&q| q == '"');
                let stop = end.map_or(chars.len(), |e| i + 1 + e + 1);
                out.extend(&chars[i..stop]);
                if end.is_none() {
                    out.push('"');
                }
                i = stop;
                continue;
            }
            ';' => {
                digits = false;
                point = false;
                out.push(c);
            }
            '0' | '#' => {
                digits = true;
                out.push(c);
            }
            '%' if !digits => lit(&mut out, c),
            '@' | '?' | '_' => lit(&mut out, c),
            '\\' => {
                if let Some(&n) = chars.get(i + 1) {
                    out.push(c);
                    out.push(n);
                }
                i += 2;
                continue;
            }
            '*' => i += 1,
            ',' if point && !matches!(chars.get(i + 1), Some('0' | '#' | ',')) => {}
            '.' => {
                point = true;
                out.push(c);
            }
            c if !dated && c.is_alphabetic() && !matches!(c, 'E' | 'e') => lit(&mut out, c),
            ':' | '/' if !dated => lit(&mut out, c),
            '[' => {
                let end = chars[i..].iter().position(|&q| q == ']');
                let stop = end.map_or(chars.len(), |e| i + e + 1);
                if !matches!(chars.get(i + 1), Some('$' | '<' | '>' | '=')) {
                    out.extend(&chars[i..stop]);
                }
                i = stop;
                continue;
            }
            'E' | 'e' if !dated => {
                let sign = matches!(chars.get(i + 1), Some('+' | '-'));
                if sign {
                    if digits {
                        out.push('E');
                    } else {
                        lit(&mut out, c);
                    }
                    out.push(chars[i + 1]);

                    match chars.get(i + 2) {
                        Some('0') => {}
                        Some('#') => {
                            out.push('0');
                            i += 1;
                        }
                        _ => out.push('0'),
                    }
                    i += 2;
                    continue;
                } else if !digits || i + 1 < chars.len() && chars[i + 1] != ';' {
                    lit(&mut out, c);
                }
            }
            _ => out.push(c),
        }
        i += 1;
    }
    out
}

fn has_digits(section: &str) -> bool {
    let (mut quoted, mut escaped, mut bracket) = (false, false, false);
    for c in section.chars() {
        if escaped {
            escaped = false;
            continue;
        }
        match c {
            '\\' if !quoted => escaped = true,
            '"' => quoted = !quoted,
            '[' if !quoted => bracket = true,
            ']' if !quoted => bracket = false,
            '0' | '#' if !quoted && !bracket => return true,
            _ => {}
        }
    }
    false
}

fn sections(code: &str) -> Vec<String> {
    let (mut out, mut cur, mut quoted, mut escaped) = (Vec::new(), String::new(), false, false);
    for c in code.chars() {
        if c == ';' && !quoted && !escaped {
            out.push(std::mem::take(&mut cur));
            continue;
        }
        cur.push(c);
        escaped = c == '\\' && !escaped;
        if c == '"' && !escaped {
            quoted = !quoted;
        }
    }
    out.push(cur);
    out
}

fn has_text_mark(section: &str, marks: &str) -> bool {
    let (mut quoted, mut escaped, mut bracket) = (false, false, false);
    for c in section.chars() {
        if escaped {
            escaped = false;
            continue;
        }
        match c {
            '\\' if !quoted => escaped = true,
            '"' => quoted = !quoted,
            '[' if !quoted => bracket = true,
            ']' if !quoted => bracket = false,
            c if !quoted && !bracket && marks.contains(c) => return true,
            _ => {}
        }
    }
    false
}

fn text_format(section: &str, s: &str) -> String {
    let chars: Vec<char> = s.chars().collect();
    let codes: Vec<char> = section.chars().collect();
    let slots = {
        let (mut n, mut quoted, mut escaped) = (0, false, false);
        for &c in &codes {
            if escaped {
                escaped = false;
            } else if c == '\\' && !quoted {
                escaped = true;
            } else if c == '"' {
                quoted = !quoted;
            } else if !quoted && matches!(c, '@' | '&') {
                n += 1;
            }
        }
        n
    };
    let left = codes.contains(&'!');
    let upper = codes.contains(&'>');
    let lower = codes.contains(&'<');
    let used = if left {
        &chars[chars.len().saturating_sub(slots)..]
    } else {
        &chars[..]
    };
    let n = used.len();
    let assigned = |k: usize| -> Option<char> {
        if left {
            used.get(k).copied()
        } else if n >= slots {
            Some(used[k])
        } else if k >= slots - n {
            Some(used[k - (slots - n)])
        } else {
            None
        }
    };
    let mut out = String::new();
    let (mut quoted, mut escaped) = (false, false);
    let mut seen = 0;
    for &c in &codes {
        if escaped {
            out.push(c);
            escaped = false;
            continue;
        }
        match c {
            '\\' if !quoted => escaped = true,
            '"' => quoted = !quoted,
            '@' | '&' if !quoted => {
                match assigned(seen) {
                    Some(ch) => out.push(ch),
                    None if c == '@' => out.push(' '),
                    None => {}
                }
                seen += 1;
            }
            '<' | '>' | '!' if !quoted => {}
            c => out.push(c),
        }
    }
    if !left && n > slots {
        out.extend(used[slots..].iter());
    }
    if upper && !lower {
        super::strings::map_str(&out, true)
    } else if lower && !upper {
        super::strings::map_str(&out, false)
    } else {
        out
    }
}

fn format_string(code: &str, s: &str) -> String {
    let secs = sections(code);
    if s.is_empty() {
        return empty_text(&secs, false).0;
    }

    let is_text = |sec: &&String| {
        has_text_mark(sec, "@&<>!")
            && sec
                .find(['0', '#'])
                .is_none_or(|d| sec.find(['@', '&', '<', '>', '!']).is_some_and(|m| m < d))
    };
    match secs.iter().find(is_text) {
        Some(sec) => text_format(sec, s),
        None => s.to_string(),
    }
}

fn empty_text(secs: &[String], variant: bool) -> (String, bool) {
    let marks = |s: &String| has_text_mark(s, "@&<>!");
    if secs.len() >= 2 && !has_digits(&secs[0]) && (marks(&secs[0]) || marks(&secs[1])) {
        (text_format(&secs[1], ""), true)
    } else if variant && secs.len() >= 4 && (has_digits(&secs[0]) || secs[0].is_empty()) {
        let (mut out, mut quoted, mut escaped) = (String::new(), false, false);
        for c in secs[3].chars() {
            if escaped {
                out.push(c);
                escaped = false;
            } else if c == '\\' && !quoted {
                escaped = true;
            } else if c == '"' {
                quoted = !quoted;
            } else {
                out.push(c);
            }
        }
        (out, true)
    } else {
        (String::new(), false)
    }
}

fn quote(out: &mut String, text: &str) {
    out.push('"');
    out.push_str(text);
    out.push('"');
}

fn designators(ctx: &RtContext) -> (&'static str, &'static str) {
    if ctx.locale == Locale::EN_US {
        ("AM", "PM")
    } else {
        ("am", "pm")
    }
}

fn vba_dates(ctx: &RtContext, wk: Week, code: &str, x: f64) -> Result<String, VbaError> {
    let p = parts(x);
    let chars: Vec<char> = code.chars().collect();
    let lower: Vec<char> = code.to_ascii_lowercase().chars().collect();
    let twelve = {
        let l = code.to_ascii_lowercase();
        l.contains("am/pm") || l.contains("a/p") || l.contains("ampm")
    };
    let mut out = String::new();
    let mut after_h = false;
    let mut i = 0;
    while i < chars.len() {
        let c = chars[i];
        match c {
            '"' => {
                let end = chars[i + 1..]
                    .iter()
                    .position(|&q| q == '"')
                    .map_or(chars.len(), |e| i + 1 + e + 1);
                out.extend(&chars[i..end]);
                i = end;
                continue;
            }
            '\\' => {
                out.extend(&chars[i..(i + 2).min(chars.len())]);
                i += 2;
                continue;
            }
            '[' => {
                let end = chars[i..]
                    .iter()
                    .position(|&q| q == ']')
                    .map_or(chars.len(), |e| i + e + 1);
                let inner: String = lower[i + 1..end.saturating_sub(1).max(i + 1)]
                    .iter()
                    .collect();
                if !inner.is_empty() && inner.chars().all(|q| matches!(q, 'h' | 'm' | 's')) {
                } else {
                    out.extend(&chars[i..end]);
                }
                i = end;
                continue;
            }
            _ => {}
        }
        let lc = lower[i];
        if !c.is_alphabetic() {
            if c == '/' {
                out.push('\\');
            }
            out.push(c);
            i += 1;
            continue;
        }
        let run = lower[i..].iter().take_while(|&&q| q == lc).count();
        let rest: String = lower[i..].iter().collect();
        match lc {
            'a' if rest.starts_with("am/pm") || rest.starts_with("a/p") => {
                let short = rest.starts_with("a/p") && !rest.starts_with("am/pm");
                let pm = p.secs >= 43_200;
                let upper = c.is_uppercase();
                let word = match (short, pm) {
                    (true, true) => "p",
                    (true, false) => "a",
                    (false, true) => "pm",
                    (false, false) => "am",
                };
                let word = if upper {
                    word.to_uppercase()
                } else {
                    word.to_string()
                };
                quote(&mut out, &word);
                i += if short { 3 } else { 5 };
            }
            'a' if rest.starts_with("ampm") => {
                let (am, pm) = designators(ctx);
                quote(&mut out, if p.secs >= 43_200 { pm } else { am });
                i += 4;
            }
            'd' if run >= 5 => {
                let short = if ctx.locale == Locale::EN_US {
                    "m/d/yyyy"
                } else {
                    "d/mm/yyyy"
                };
                let long = if ctx.locale == Locale::EN_US {
                    "dddd, mmmm d, yyyy"
                } else {
                    "dddd, d mmmm yyyy"
                };
                let code = if run == 5 { short } else { long };
                out.push_str(&vba_dates(ctx, wk, code, x)?);
                if run > 6 {
                    out.push_str(&"d".repeat(run - 6));
                }
                i += run;
            }
            'y' => {
                let mut left = run;
                while left > 0 {
                    if left >= 4 {
                        quote(&mut out, &format!("{:04}", p.year));
                        left -= 4;
                    } else if left >= 2 {
                        quote(&mut out, &format!("{:02}", p.year % 100));
                        left -= 2;
                    } else {
                        quote(&mut out, &p.yday.to_string());
                        left -= 1;
                    }
                }
                i += run;
            }
            'w' if run <= 2 => {
                let n = if run == 1 {
                    (p.weekday - wk.first).rem_euclid(7) + 1
                } else {
                    week_of_year(&p, wk)
                };
                quote(&mut out, &n.to_string());
                i += run;
            }
            'q' if run == 1 => {
                quote(&mut out, &((p.month - 1) / 3 + 1).to_string());
                i += 1;
            }
            'n' => {
                let m = p.secs / 60 % 60;
                quote(
                    &mut out,
                    &if run == 1 {
                        m.to_string()
                    } else {
                        format!("{m:02}")
                    },
                );
                i += run;
            }
            'c' if run == 1 => {
                let code = general_date_code(ctx, x);
                out.push_str(&vba_dates(ctx, wk, &code, x)?);
                i += 1;
            }
            't' if run >= 5 => {
                out.push_str(&vba_dates(ctx, wk, "h:mm:ss AMPM", x)?);
                i += run;
            }
            'm' if after_h => {
                let m = p.secs / 60 % 60;
                quote(
                    &mut out,
                    &if run == 1 {
                        m.to_string()
                    } else {
                        format!("{m:02}")
                    },
                );
                after_h = false;
                i += run;
            }
            'h' if twelve => {
                after_h = true;
                let h = match p.secs / 3600 % 12 {
                    0 => 12,
                    h => h,
                };
                quote(
                    &mut out,
                    &if run == 1 {
                        h.to_string()
                    } else {
                        format!("{h:02}")
                    },
                );
                i += run;
            }
            'm' => {
                let name = dates::month_title(p.month);
                let text = match run {
                    1 => p.month.to_string(),
                    2 => format!("{:02}", p.month),
                    3 => name[..3].to_string(),
                    4 => name,
                    _ => format!("{name}{}", p.month),
                };
                quote(&mut out, &text);
                i += run;
            }
            'd' | 'h' | 's' => {
                after_h = lc == 'h';
                out.extend(&chars[i..i + run]);
                i += run;
            }
            'e' if matches!(chars.get(i + 1), Some('+' | '-')) => {
                out.push(c);
                i += 1;
            }
            _ => {
                quote(&mut out, &c.to_string());
                i += 1;
            }
        }
    }
    Ok(out)
}

#[derive(Clone, Copy)]
pub(super) struct Week {
    pub(super) first: i64,
    pub(super) rule: i64,
}

pub(super) fn week_of_year(p: &Parts, wk: Week) -> i64 {
    let off = (p.weekday - wk.first - (p.yday - 1)).rem_euclid(7);
    let lead = (p.yday - 1 + off) / 7;
    let starts_week_one = match wk.rule {
        3 => off == 0,
        2 => 7 - off >= 4 || off == 0,
        _ => true,
    };
    let leap = (p.year % 4 == 0 && p.year % 100 != 0) || p.year % 400 == 0;
    let next = p.serial - p.yday + 1 + if leap { 366 } else { 365 };
    let next_off = (parts(next as f64).weekday - wk.first).rem_euclid(7);
    let next_one = match wk.rule {
        3 => next_off == 0,
        2 => 7 - next_off >= 4 || next_off == 0,
        _ => false,
    };
    if p.month == 12 && p.serial >= next - next_off && next_one {
        1
    } else if starts_week_one {
        lead + 1
    } else if lead == 0 {
        let last = parts((p.serial - p.yday) as f64);
        week_of_year(&last, wk)
    } else {
        lead
    }
}

fn has_date_field(code: &str) -> bool {
    let chars: Vec<char> = code.chars().collect();
    let (mut quoted, mut escaped, mut bracket) = (false, false, false);
    for (i, &c) in chars.iter().enumerate() {
        if escaped {
            escaped = false;
            continue;
        }
        match c {
            '\\' if !quoted => escaped = true,
            '"' => quoted = !quoted,
            '[' if !quoted => bracket = true,
            ']' if !quoted => bracket = false,
            '0' | '#' if !quoted && !bracket => return false,
            _ if quoted || bracket => {}
            'a' | 'A' => {
                let rest: String = chars[i..].iter().take(5).collect::<String>().to_lowercase();
                if rest.starts_with("am/pm") || rest.starts_with("a/p") || rest.starts_with("ampm")
                {
                    return true;
                }
            }
            c if "dmyhswqnctDMYHSWQNCT".contains(c) => return true,
            _ => {}
        }
    }
    false
}

fn whole_time(code: &str, x: f64) -> f64 {
    if x.fract() == 0.0 {
        return x;
    }
    let p = parts(x);
    let l = code.to_ascii_lowercase();
    let secs = if l.contains('s') || l.contains('t') || l.contains('c') {
        p.secs
    } else if l.contains('m') || l.contains('n') {
        p.secs / 60 * 60
    } else {
        p.secs / 3600 * 3600
    };
    let frac = secs as f64 / 86_400.0;
    if x < 0.0 {
        p.serial as f64 - frac
    } else {
        p.serial as f64 + frac
    }
}

fn fill_sections(code: &str) -> String {
    let mut secs = sections(code);
    if secs.len() >= 2 && secs[1].is_empty() && !secs[0].is_empty() {
        secs[1] = format!("-{}", secs[0]);
    }
    if secs.len() >= 3 && secs[2].is_empty() && !secs[0].is_empty() {
        secs[2] = secs[0].clone();
    }
    secs.join(";")
}

fn zero_by_rounding(code: &str, x: f64) -> f64 {
    let secs = sections(code);
    if secs.len() < 2 || x == 0.0 {
        return x;
    }
    let first = &secs[0];
    let decimals = first.split_once('.').map_or(0, |(_, d)| {
        d.chars().take_while(|c| matches!(c, '0' | '#')).count()
    });
    let percent = first.matches('%').count() as i32;
    let scale = 10f64.powi(decimals as i32);
    let shown = (x.abs() * 100f64.powi(percent) * scale).round();
    if shown == 0.0 {
        0.0
    } else {
        x
    }
}

fn wide_scientific(ctx: &RtContext, code: &str, x: f64) -> Option<Result<String, VbaError>> {
    if code.contains(';') || code.contains('"') {
        return None;
    }
    let at = code.find(['E', 'e'])?;
    let sign = code[at + 1..]
        .chars()
        .next()
        .filter(|c| matches!(c, '+' | '-'))?;
    let mant = &code[..at];
    let int_digits = mant
        .split('.')
        .next()?
        .chars()
        .filter(|c| matches!(c, '0' | '#'))
        .count();
    if int_digits < 2 || !mant.chars().all(|c| matches!(c, '0' | '#' | '.' | ',')) {
        return None;
    }
    let rest = &code[at + 2..];
    let zeros = rest.chars().take_while(|&c| c == '0').count();
    let suffix = &rest[zeros..];
    let mut e = if x == 0.0 {
        0
    } else {
        x.abs().log10().floor() as i32 - (int_digits as i32 - 1)
    };
    let zeroed = mant.replace('#', "0");
    let nf = NumberFormat::parse(if x == 0.0 { &zeroed } else { mant }).ok()?;
    let locale = engine_locale(ctx);
    let shown = |e: i32| nf.format_vba(x / 10f64.powi(e), &locale).map(|r| r.text);
    let mut text = shown(e).ok()?;
    let digits = |t: &str| t.chars().filter(char::is_ascii_digit).count();
    if x != 0.0 && digits(&text) > int_digits + mant.split('.').nth(1).map_or(0, str::len) {
        e += 1;
        text = shown(e).ok()?;
    }
    let sgn = if e < 0 {
        "-"
    } else if sign == '+' {
        "+"
    } else {
        ""
    };
    let exp = format!("{:0width$}", e.abs(), width = zeros);
    let marker = if code.contains('e') { 'e' } else { 'E' };
    Some(Ok(format!("{text}{marker}{sgn}{exp}{suffix}")))
}

fn drop_decimal_commas(code: &str) -> String {
    if code.contains('"') || code.contains('\\') {
        return code.to_string();
    }
    let mut out = String::new();
    let mut after_point = false;
    for c in code.chars() {
        match c {
            '.' => after_point = true,
            ';' => after_point = false,
            ',' if after_point => continue,
            _ => {}
        }
        out.push(c);
    }
    out
}

fn sci_fraction_zeros(code: &str, x: f64) -> String {
    let (Some(point), Some(e)) = (code.find('.'), code.find(['E', 'e'])) else {
        return code.to_string();
    };
    if !(100.0..1000.0).contains(&x.abs())
        || code.contains(['"', ';', '\\'])
        || e < point
        || !matches!(code[e + 1..].chars().next(), Some('+' | '-'))
    {
        return code.to_string();
    }
    let mut seen_zero = false;
    let frac: String = code[point..e]
        .chars()
        .map(|c| {
            seen_zero |= c == '0';
            if c == '#' && seen_zero {
                '0'
            } else {
                c
            }
        })
        .collect();
    format!("{}{frac}{}", &code[..point], &code[e..])
}

fn render(ctx: &RtContext, wk: Week, code: &str, x: f64) -> Result<String, VbaError> {
    let dated = has_date_field(code);
    let mut plain = fill_sections(&vba_code(code, dated));

    if sections(&plain).len() > 3 {
        plain = sections(&plain)[..3].join(";");
    }
    if dated && x == 0.0 {
        plain = sections(&plain).swap_remove(0);
    }
    if !dated {
        plain = sci_fraction_zeros(&plain, x);
    }
    if plain.is_empty() && !code.is_empty() {
        return Ok(String::new());
    }
    let x = if dated {
        whole_time(&plain, x)
    } else {
        zero_by_rounding(&plain, x)
    };
    if dated && (x.trunc() > 2_958_465.0 || x.trunc() < -657_434.0) {
        return Err(VbaError::new(6));
    }
    if !dated {
        if let Some(r) = wide_scientific(ctx, &plain, x) {
            return r;
        }
    }
    let out = vba_dates(ctx, wk, &plain, x)?;
    let x = if dated && !has_date_field(&out) {
        x.abs()
    } else {
        x
    };
    let out = if dated {
        out
    } else {
        drop_decimal_commas(&out)
    };
    let nf = NumberFormat::parse(&out).map_err(|_| VbaError::new(5))?;
    let text = nf
        .format_vba(x, &engine_locale(ctx))
        .map(|r| r.text)
        .map_err(|_| VbaError::new(5))?;
    Ok(if !dated && code.contains("e+") || code.contains("e-") {
        text.replacen('E', "e", 1)
    } else {
        text
    })
}

fn general_date_code(ctx: &RtContext, x: f64) -> String {
    let us = ctx.locale == Locale::EN_US;
    let short = if us { "m/d/yyyy" } else { "d/mm/yyyy" };
    let long_time = "h:mm:ss AMPM";
    let p = parts(x);
    if p.serial == 0 {
        long_time.to_string()
    } else if p.secs == 0 {
        short.to_string()
    } else {
        format!("{short} {long_time}")
    }
}

fn general_number(ctx: &RtContext, x: f64) -> Result<String, VbaError> {
    text(ctx, &Variant::R8(x))
}

fn words(x: f64, yes: &str, no: &str) -> String {
    (if x == 0.0 { no } else { yes }).to_string()
}

fn format_impl(ctx: &RtContext, a: &[Variant]) -> R {
    let fmt = match a.get(1) {
        Some(v) if !missing(v) => text(ctx, v)?,
        _ => String::new(),
    };
    let wk = {
        let given = a.len() > 2 && !missing(&a[2]);
        let first = long_or(ctx, a, 2, 1)?;
        let rule = long_or(ctx, a, 3, 0)?;
        if !(0..=7).contains(&first) || !(0..=3).contains(&rule) {
            return Err(VbaError::new(5));
        }
        Week {
            first: if first == 0 && given {
                dates::first_day(ctx, &[Variant::I4(0)], 0)?
            } else {
                i64::from(first).max(1)
            },
            rule: i64::from(rule).max(1),
        }
    };
    let fmt = {
        let bare = fmt.trim();
        if bare.starts_with('[')
            && bare.ends_with(']')
            && !bare.contains(';')
            && bare.matches('[').count() == 1
            && bare[1..bare.len() - 1]
                .chars()
                .all(|c| "hmsHMS0".contains(c))
        {
            String::new()
        } else {
            fmt
        }
    };
    let value = if is_null(&a[0]) {
        Variant::Bstr(String::new())
    } else {
        a[0].clone()
    };
    let is_date = matches!(value, Variant::Date(_))
        || matches!(&value, Variant::Bstr(s) if fmt.is_empty() && !s.is_empty() && double(ctx, &value).is_err() && dates::parse_date(ctx, s).is_some());

    let text_marks = {
        let secs = sections(&fmt);
        let marks = |s: &String| {
            has_text_mark(s, "@&<>!")
                && (!has_digits(s) || s.starts_with(['@', '&', '<', '>', '!']))
        };
        marks(&secs[0]) || secs[0].is_empty() && secs.len() > 1 && secs.iter().any(marks)
    };

    let number = match &value {
        Variant::Bstr(s) if !fmt.is_empty() && !text_marks && !s.is_empty() => {
            let dotted_time = named(ctx, &fmt).is_none()
                && has_date_field(&fmt)
                && s.split_once('.').is_some_and(|(h, m)| {
                    !h.is_empty()
                        && !m.is_empty()
                        && h.bytes().chain(m.bytes()).all(|c| c.is_ascii_digit())
                });
            match double(ctx, &value) {
                Ok(_) if dotted_time && dates::parse_date(ctx, s).is_some() => {
                    dates::parse_date(ctx, s)
                }
                Ok(x) => Some(x),
                Err(_) if s.eq_ignore_ascii_case("true") => Some(-1.0),
                Err(_) if s.eq_ignore_ascii_case("false") => Some(0.0),
                Err(_) => dates::parse_date(ctx, s),
            }
        }

        Variant::Bstr(s) if fmt.is_empty() && !s.is_empty() && double(ctx, &value).is_err() => {
            dates::parse_date(ctx, s)
        }
        Variant::Bstr(_) => None,
        Variant::Empty => None,
        Variant::Bool(b) => Some(if *b { -1.0 } else { 0.0 }),
        Variant::Date(x) => Some(*x),
        Variant::Error(_) | Variant::Array(_) => return Err(VbaError::new(13)),
        v => {
            let x = double(ctx, v)?;

            Some(if matches!(v, Variant::R4(_)) {
                format!("{x:.6e}").parse().unwrap_or(x)
            } else {
                x
            })
        }
    };
    let number = if text_marks && !fmt.is_empty() && !matches!(value, Variant::Bstr(_)) {
        None
    } else {
        number
    };
    let Some(x) = number else {
        let s = text(ctx, &value)?;
        if fmt.is_empty() {
            return text_out_null(s);
        }
        if s.is_empty() && (is_null(&a[0]) || matches!(a[0], Variant::Empty)) {
            let (out, picked) = empty_text(&sections(&fmt), true);
            return if picked {
                text_out(out)
            } else {
                text_out_null(out)
            };
        }
        if s.is_empty() {
            let (out, picked) = empty_text(&sections(&fmt), false);
            return if picked {
                text_out(out)
            } else {
                text_out_null(out)
            };
        }
        return text_out_null(format_string(&fmt, &s));
    };
    let out = if fmt.is_empty() {
        if is_date {
            render(ctx, wk, &general_date_code(ctx, x), x)?
        } else if matches!(value, Variant::Bool(_)) {
            text(ctx, &value)?
        } else if matches!(value, Variant::Bstr(_)) {
            general_number(ctx, x)?
        } else {
            text(ctx, &value)?
        }
    } else {
        match named(ctx, &fmt) {
            Some(Named::General) if !is_date => match value {
                Variant::Bstr(_) | Variant::Bool(_) => general_number(ctx, x)?,
                _ => text(ctx, &value)?,
            },
            Some(Named::General) => general_number(ctx, x)?,
            Some(Named::GeneralDate) => render(ctx, wk, &general_date_code(ctx, x), x)?,
            Some(Named::Words(y, n)) => words(x, y, n),
            Some(Named::Code(c)) => render(ctx, wk, &c, x)?,
            None => match decimal_plain(ctx, &fmt, &value) {
                Some(s) => s,
                None => render(ctx, wk, &fmt, x)?,
            },
        }
    };
    text_out(out)
}

fn decimal_plain(ctx: &RtContext, code: &str, value: &Variant) -> Option<String> {
    if !matches!(value, Variant::Decimal(_) | Variant::Cy(_))
        || !code.chars().all(|c| matches!(c, '0' | '#' | '.' | ','))
        || matches!(value, Variant::Cy(_)) && (code.ends_with(['.', ',']) || code.contains(",."))
        || code.matches('.').count() > 1
        || !code.contains(['0', '#'])
    {
        return None;
    }
    let (int_code, frac_code) = code.split_once('.').unwrap_or((code, ""));
    let want = frac_code.chars().filter(|c| matches!(c, '0' | '#')).count();
    let min_frac = frac_code.matches('0').count();
    let min_int = int_code.matches('0').count();
    let group = int_code.contains(',');
    let t = text(ctx, value).ok()?;
    let (neg, t) = match t.strip_prefix('-') {
        Some(r) => (true, r.to_string()),
        None => (false, t),
    };
    let (ip, fp) = t.split_once('.').unwrap_or((&t, ""));
    let mut digits: Vec<u8> = ip
        .bytes()
        .chain(fp.bytes().take(want))
        .map(|b| b - b'0')
        .collect();
    if fp.as_bytes().get(want).is_some_and(|&b| b >= b'5') {
        let mut i = digits.len();
        loop {
            if i == 0 {
                digits.insert(0, 1);
                break;
            }
            i -= 1;
            if digits[i] == 9 {
                digits[i] = 0;
            } else {
                digits[i] += 1;
                break;
            }
        }
    }
    let split = digits.len() - fp.len().min(want);
    let digit_text = |d: &[u8]| -> String { d.iter().map(|d| char::from(b'0' + d)).collect() };
    let int_trimmed = digit_text(&digits[..split])
        .trim_start_matches('0')
        .to_string();
    let mut int = format!(
        "{}{int_trimmed}",
        "0".repeat(min_int.saturating_sub(int_trimmed.len()))
    );
    let mut frac = digit_text(&digits[split..]);
    while frac.len() < want {
        frac.push('0');
    }
    while frac.len() > min_frac && frac.ends_with('0') {
        frac.pop();
    }
    if group {
        let n = int.len();
        int = int
            .chars()
            .enumerate()
            .map(|(i, c)| {
                if i > 0 && (n - i) % 3 == 0 {
                    format!(",{c}")
                } else {
                    c.to_string()
                }
            })
            .collect();
    }
    let body = if frac.is_empty() {
        int
    } else {
        format!("{int}.{frac}")
    };
    let zero = !body.bytes().any(|b| (b'1'..=b'9').contains(&b));
    Some(if neg && !zero {
        format!("-{body}")
    } else {
        body
    })
}

pub(super) fn format(ctx: &RtContext, a: &[Variant]) -> R {
    format_impl(ctx, a)
}

pub(super) fn format_d(ctx: &RtContext, a: &[Variant]) -> R {
    format_impl(ctx, a)
}

fn number_code(
    ctx: &RtContext,
    a: &[Variant],
    prefix: &str,
    suffix: &str,
    group_default: bool,
) -> Result<String, VbaError> {
    let digits = long_or(ctx, a, 1, -1)?;
    let flag = |i: usize, default: bool| -> Result<bool, VbaError> {
        match long_or(ctx, a, i, -2)? {
            -2 => Ok(default),
            0 => Ok(false),
            -1 => Ok(true),
            _ => Err(VbaError::new(5)),
        }
    };
    let lead = flag(2, true)?;
    let parens = flag(3, false)?;
    let group = flag(4, group_default)?;
    if digits < -1 {
        return Err(VbaError::new(5));
    }

    let point = digits >= 256 && digits % 256 == 0;
    let digits = if digits == -1 {
        2
    } else {
        (digits % 256) as usize
    };
    let int = match (group, lead) {
        (true, true) => "#,##0",
        (true, false) => "#,###",
        (false, true) => "0",
        (false, false) => "#",
    };
    let frac = if point {
        ".".to_string()
    } else if digits == 0 {
        String::new()
    } else {
        format!(".{}", "0".repeat(digits))
    };
    let body = format!("{prefix}{int}{frac}{suffix}");
    Ok(if parens {
        format!("{body};({body})")
    } else {
        format!("{body};-{body}")
    })
}

fn formatted(ctx: &RtContext, a: &[Variant], code: String) -> R {
    if is_null(&a[0]) || matches!(a[0], Variant::Empty) {
        return Ok(Variant::BstrNull);
    }
    if matches!(&a[0], Variant::Bstr(s) if s.is_empty()) {
        return Err(VbaError::new(13));
    }
    if let Variant::Bstr(_) = &a[0] {
        double(ctx, &a[0])?;
    }
    format_impl(ctx, &[a[0].clone(), Variant::Bstr(code)])
}

pub(super) fn format_number(ctx: &RtContext, a: &[Variant]) -> R {
    let code = number_code(ctx, a, "", "", true)?;
    formatted(ctx, a, code)
}

pub(super) fn format_percent(ctx: &RtContext, a: &[Variant]) -> R {
    let code = number_code(ctx, a, "", "%", true)?;
    formatted(ctx, a, code)
}

pub(super) fn format_currency(ctx: &RtContext, a: &[Variant]) -> R {
    let code = number_code(ctx, a, "$", "", true)?;
    formatted(ctx, a, code)
}

pub(super) fn format_date_time(ctx: &RtContext, a: &[Variant]) -> R {
    let name = match long_or(ctx, a, 1, 0)? {
        0 => "General Date",
        1 => "Long Date",
        2 => "Short Date",
        3 => "Long Time",
        4 => "Short Time",
        _ => return Err(VbaError::new(5)),
    };
    if is_null(&a[0]) {
        return Ok(Variant::BstrNull);
    }
    if matches!(&a[0], Variant::Bstr(s) if s.is_empty()) {
        return Err(VbaError::new(13));
    }
    let value = match &a[0] {
        Variant::Empty => Variant::Date(0.0),
        v @ Variant::Bstr(_) => Variant::Date(dates::to_date(ctx, v)?),
        v => v.clone(),
    };
    format_impl(ctx, &[value, Variant::Bstr(name.to_string())])
}
