use super::util::{
    convert, from_hr, from_units, is_null, long, long_or, long_value, null_error, opt, text,
    text_out, units, R,
};
use super::RtContext;
use crate::VbaError;
use std::cmp::Ordering;
use xlcalc_variant::ops;
use xlcalc_variant::{SafeArray, VarType, Variant};

macro_rules! strict {
    ($name:ident, $f:ident) => {
        pub(super) fn $name(ctx: &RtContext, a: &[Variant]) -> R {
            if a.iter().any(is_null) {
                return Err(null_error());
            }
            $f(ctx, a)
        }
    };
}

fn map_char(c: char, upper: bool) -> char {
    fn one(mut it: impl Iterator<Item = char>) -> Option<char> {
        let first = it.next()?;
        it.next().is_none().then_some(first)
    }
    let (to, back) = if upper {
        (one(c.to_uppercase()), one_lower as fn(char) -> Option<char>)
    } else {
        (
            one(c.to_lowercase()).filter(|_| c != '\u{1E9E}'),
            one_upper as fn(char) -> Option<char>,
        )
    };
    match to {
        Some(m) if m != c && back(m).is_none_or(|b| b == c) => m,
        _ => c,
    }
}

fn one_lower(c: char) -> Option<char> {
    let mut it = c.to_lowercase();
    let first = it.next()?;
    it.next().is_none().then_some(first)
}

fn one_upper(c: char) -> Option<char> {
    let mut it = c.to_uppercase();
    let first = it.next()?;
    it.next().is_none().then_some(first)
}

fn map_unit(u: u16, upper: bool) -> u16 {
    match char::from_u32(u32::from(u)) {
        Some(c) => u16::try_from(u32::from(map_char(c, upper))).unwrap_or(u),
        None => u,
    }
}

fn fold_unit(u: u16) -> ([u16; 2], usize) {
    if u == 0xDF || u == 0x1E9E {
        ([u16::from(b's'); 2], 2)
    } else {
        ([map_unit(u, false), 0], 1)
    }
}

fn text_compare(
    ctx: &RtContext,
    a: &[Variant],
    i: usize,
    two_is_text: bool,
) -> Result<bool, VbaError> {
    let Some(v) = opt(a, i) else {
        return Ok(ctx.compare_text);
    };
    match long(ctx, v)? {
        0 => Ok(false),
        1 | 3.. => Ok(true),
        2 if two_is_text => Ok(true),
        _ => Err(VbaError::new(5)),
    }
}

fn match_at(hay: &[u16], needle: &[u16], at: usize, fold: bool) -> Option<usize> {
    if !fold {
        return (hay.len() - at >= needle.len() && hay[at..at + needle.len()] == *needle)
            .then_some(at + needle.len());
    }
    let (mut h, mut n) = (at, 0);
    while n < needle.len() {
        let (f, k) = fold_unit(*hay.get(h)?);
        for &u in &f[..k] {
            if needle.get(n) != Some(&u) {
                return None;
            }
            n += 1;
        }
        h += 1;
    }
    Some(h)
}

fn fold_needle(needle: &[u16], fold: bool) -> Vec<u16> {
    if !fold {
        return needle.to_vec();
    }
    needle
        .iter()
        .flat_map(|&u| {
            let (f, k) = fold_unit(u);
            f.into_iter().take(k)
        })
        .collect()
}

fn find(hay: &[u16], needle: &[u16], from: usize, fold: bool) -> Option<(usize, usize)> {
    let needle = fold_needle(needle, fold);
    (from..hay.len()).find_map(|i| match_at(hay, &needle, i, fold).map(|e| (i, e)))
}

pub(super) fn len(ctx: &RtContext, a: &[Variant]) -> R {
    if is_null(&a[0]) {
        return Ok(Variant::Null);
    }
    long_value(units(&text(ctx, &a[0])?).len() as i64)
}

fn left_impl(ctx: &RtContext, a: &[Variant]) -> R {
    if is_null(&a[0]) {
        return Ok(Variant::Null);
    }
    let n = long(ctx, &a[1])?;
    if !(0..i32::MAX).contains(&n) {
        return Err(VbaError::new(5));
    }
    let u = units(&text(ctx, &a[0])?);
    text_out(from_units(&u[..(n as usize).min(u.len())]))
}

fn right_impl(ctx: &RtContext, a: &[Variant]) -> R {
    if is_null(&a[0]) {
        return Ok(Variant::Null);
    }
    let n = long(ctx, &a[1])?;
    if !(0..i32::MAX).contains(&n) {
        return Err(VbaError::new(5));
    }
    let u = units(&text(ctx, &a[0])?);
    text_out(from_units(&u[u.len() - (n as usize).min(u.len())..]))
}

fn mid_impl(ctx: &RtContext, a: &[Variant]) -> R {
    if is_null(&a[0]) {
        return Ok(Variant::Null);
    }
    let start = long(ctx, &a[1])?;
    let count = match opt(a, 2) {
        Some(v) => Some(long(ctx, v)?),
        None => None,
    };
    if start < 1 || start == i32::MAX || count.is_some_and(|c| !(0..i32::MAX).contains(&c)) {
        return Err(VbaError::new(5));
    }
    let u = units(&text(ctx, &a[0])?);
    let from = (start as usize - 1).min(u.len());
    let to = count.map_or(u.len(), |c| (from + c as usize).min(u.len()));
    text_out(from_units(&u[from..to]))
}

pub(super) fn left(ctx: &RtContext, a: &[Variant]) -> R {
    left_impl(ctx, a)
}

pub(super) fn right(ctx: &RtContext, a: &[Variant]) -> R {
    right_impl(ctx, a)
}

pub(super) fn mid(ctx: &RtContext, a: &[Variant]) -> R {
    mid_impl(ctx, a)
}

strict!(left_d, left_impl);
strict!(right_d, right_impl);
strict!(mid_d, mid_impl);

pub(super) fn instr(ctx: &RtContext, a: &[Variant]) -> R {
    let has_start = a.len() == 4
        || a.len() == 3
            && match &a[0] {
                Variant::Bstr(s) => s.trim().parse::<f64>().is_ok(),
                _ => true,
            };
    let (start, rest) = if has_start {
        (long(ctx, &a[0])?, &a[1..])
    } else {
        (1, a)
    };
    if start < 1 || start == i32::MAX {
        return Err(VbaError::new(5));
    }
    let fold = text_compare(ctx, rest, 2, false)?;
    if is_null(&rest[0]) || is_null(&rest[1]) {
        return Ok(Variant::Null);
    }
    let (hay, needle) = (units(&text(ctx, &rest[0])?), units(&text(ctx, &rest[1])?));
    let from = start as usize - 1;
    if hay.is_empty() {
        return long_value(0);
    }
    if needle.is_empty() {
        return long_value(start.into());
    }
    if from > hay.len() {
        return long_value(0);
    }
    long_value(find(&hay, &needle, from, fold).map_or(0, |(i, _)| i as i64 + 1))
}

pub(super) fn instrrev(ctx: &RtContext, a: &[Variant]) -> R {
    let start = long_or(ctx, a, 2, -1)?;
    if start == 0 || start < -1 {
        return Err(VbaError::new(5));
    }
    let fold = text_compare(ctx, a, 3, true)?;
    if is_null(&a[0]) || is_null(&a[1]) {
        return Err(null_error());
    }
    let (hay, needle) = (units(&text(ctx, &a[0])?), units(&text(ctx, &a[1])?));
    if start != -1 && start as usize > hay.len() {
        return long_value(0);
    }
    let last = if start == -1 {
        hay.len()
    } else {
        start as usize
    };
    if hay.is_empty() {
        return long_value(0);
    }
    if needle.is_empty() {
        return long_value(last as i64);
    }
    let needle = fold_needle(&needle, fold);
    let found = (0..last)
        .rev()
        .find(|&i| match_at(&hay[..last], &needle, i, fold).is_some());
    long_value(found.map_or(0, |i| i as i64 + 1))
}

pub(super) fn replace(ctx: &RtContext, a: &[Variant]) -> R {
    if a[..3].iter().any(is_null) {
        return Err(null_error());
    }
    let start = long_or(ctx, a, 3, 1)?;
    let count = long_or(ctx, a, 4, -1)?;
    if start < 1 || count < -1 {
        return Err(VbaError::new(5));
    }
    let fold = text_compare(ctx, a, 5, true)?;
    let (expr, what, with) = (
        units(&text(ctx, &a[0])?),
        units(&text(ctx, &a[1])?),
        units(&text(ctx, &a[2])?),
    );
    let from = start as usize - 1;
    if from > expr.len() {
        return Ok(Variant::BstrNull);
    }
    if what.is_empty() {
        return text_out(from_units(&expr[from..]));
    }
    let mut out = Vec::new();
    let mut i = from;
    let mut done = 0;
    while count == -1 || done < count {
        let Some((j, end)) = find(&expr, &what, i, fold) else {
            break;
        };
        out.extend_from_slice(&expr[i..j]);
        out.extend_from_slice(&with);
        i = end;
        done += 1;
    }
    out.extend_from_slice(&expr[i..]);
    text_out(from_units(&out))
}

fn case(ctx: &RtContext, a: &[Variant], upper: bool) -> R {
    if is_null(&a[0]) {
        return Ok(Variant::Null);
    }
    let t: String = text(ctx, &a[0])?
        .chars()
        .map(|c| map_char(c, upper))
        .collect();
    text_out(t)
}

fn lcase_impl(ctx: &RtContext, a: &[Variant]) -> R {
    case(ctx, a, false)
}

fn ucase_impl(ctx: &RtContext, a: &[Variant]) -> R {
    case(ctx, a, true)
}

pub(super) fn lcase(ctx: &RtContext, a: &[Variant]) -> R {
    lcase_impl(ctx, a)
}

pub(super) fn ucase(ctx: &RtContext, a: &[Variant]) -> R {
    ucase_impl(ctx, a)
}

strict!(lcase_d, lcase_impl);
strict!(ucase_d, ucase_impl);

fn trimmed(ctx: &RtContext, a: &[Variant], left: bool, right: bool) -> R {
    if is_null(&a[0]) {
        return Ok(Variant::Null);
    }
    let s = text(ctx, &a[0])?;
    let space = |c: char| c == ' ' || c == '\u{3000}';
    let s = if left {
        s.trim_start_matches(space)
    } else {
        &s
    };
    let s = if right { s.trim_end_matches(space) } else { s };
    text_out(s.to_string())
}

fn trim_impl(ctx: &RtContext, a: &[Variant]) -> R {
    trimmed(ctx, a, true, true)
}

fn ltrim_impl(ctx: &RtContext, a: &[Variant]) -> R {
    trimmed(ctx, a, true, false)
}

fn rtrim_impl(ctx: &RtContext, a: &[Variant]) -> R {
    trimmed(ctx, a, false, true)
}

pub(super) fn trim(ctx: &RtContext, a: &[Variant]) -> R {
    trim_impl(ctx, a)
}

pub(super) fn ltrim(ctx: &RtContext, a: &[Variant]) -> R {
    ltrim_impl(ctx, a)
}

pub(super) fn rtrim(ctx: &RtContext, a: &[Variant]) -> R {
    rtrim_impl(ctx, a)
}

strict!(trim_d, trim_impl);
strict!(ltrim_d, ltrim_impl);
strict!(rtrim_d, rtrim_impl);

pub(super) fn space(ctx: &RtContext, a: &[Variant]) -> R {
    let n = long(ctx, &a[0])?;
    if n < 0 {
        return Err(VbaError::new(5));
    }
    text_out(" ".repeat(n as usize))
}

fn string_impl(ctx: &RtContext, a: &[Variant]) -> R {
    let n = long(ctx, &a[0])?;
    if n < 0 {
        return Err(VbaError::new(5));
    }
    let u = match &a[1] {
        Variant::Null => return Ok(Variant::Null),
        Variant::Bstr(s) => units(s).first().copied().ok_or(VbaError::new(5))?,
        v => {
            let Variant::I2(code) = convert(ctx, v, VarType::I2)? else {
                return Err(VbaError::new(13));
            };
            ansi_unit((code & 0xFF) as u16)
        }
    };
    text_out(from_units(&vec![u; n as usize]))
}

pub(super) fn string(ctx: &RtContext, a: &[Variant]) -> R {
    string_impl(ctx, a)
}

strict!(string_d, string_impl);

pub(super) fn strcomp(ctx: &RtContext, a: &[Variant]) -> R {
    let fold = text_compare(ctx, a, 2, false)?;
    if is_null(&a[0]) || is_null(&a[1]) {
        return Ok(Variant::Null);
    }
    let (x, y) = (text(ctx, &a[0])?, text(ctx, &a[1])?);
    let o = if fold {
        let (x, y) = (compare_form(&x), compare_form(&y));
        let code =
            ops::compare(&Variant::Bstr(x), &Variant::Bstr(y), ctx.locale, 1).map_err(from_hr)?;
        code.cmp(&1)
    } else {
        units(&x).cmp(&units(&y))
    };
    Ok(Variant::I2(match o {
        Ordering::Less => -1,
        Ordering::Equal => 0,
        Ordering::Greater => 1,
    }))
}

fn compare_form(s: &str) -> String {
    let mut out = String::new();
    for c in s.chars() {
        match c {
            '\u{AD}' | '\0' => {}
            '\u{DF}' | '\u{1E9E}' => out.push_str("ss"),
            '\u{FB00}' => out.push_str("ff"),
            '\u{FB01}' => out.push_str("fi"),
            '\u{FB02}' => out.push_str("fl"),
            '\u{FB03}' => out.push_str("ffi"),
            '\u{FB04}' => out.push_str("ffl"),
            '\u{FB05}' | '\u{FB06}' => out.push_str("st"),
            '\u{130}' => out.push_str("i\u{307}"),
            '\u{FF01}'..='\u{FF5E}' => out.push(char::from_u32(c as u32 - 0xFEE0).unwrap_or(c)),
            _ => out.push(c),
        }
    }
    out
}

pub(super) fn strreverse(ctx: &RtContext, a: &[Variant]) -> R {
    if matches!(a[0], Variant::BstrNull) {
        return Ok(Variant::BstrNull);
    }
    if is_null(&a[0]) {
        return Err(null_error());
    }
    let mut u = units(&text(ctx, &a[0])?);
    u.reverse();
    text_out(from_units(&u))
}

const CP1252: [u16; 32] = [
    0x20AC, 0, 0x201A, 0x0192, 0x201E, 0x2026, 0x2020, 0x2021, 0x02C6, 0x2030, 0x0160, 0x2039,
    0x0152, 0, 0x017D, 0, 0, 0x2018, 0x2019, 0x201C, 0x201D, 0x2022, 0x2013, 0x2014, 0x02DC,
    0x2122, 0x0161, 0x203A, 0x0153, 0, 0x017E, 0x0178,
];

fn ansi_unit(n: u16) -> u16 {
    match n {
        0x80..=0x9F => match CP1252[usize::from(n) - 0x80] {
            0 => n,
            m => m,
        },
        _ => n,
    }
}

const LATIN_A: &str = "AaAaAaCcCcCcCcDdDdEeEeEeEeEeGgGgGgGgHhHhIiIiIiIiIiJjJjKkkLlLlLlLlLlNnNnNnnNnOoOoOoOoRrRrRrSsSsSsSsTtTtTtUuUuUuUuUuUuWwYyYZzZzZzs";

pub(super) fn asc(ctx: &RtContext, a: &[Variant]) -> R {
    let u = *units(&text(ctx, &a[0])?).first().ok_or(VbaError::new(5))?;
    let code = if u < 0x80 || (0xA0..0x100).contains(&u) {
        u
    } else if let Some(i) = (0..32u16).find(|&i| CP1252[usize::from(i)] == u) {
        0x80 + i
    } else if matches!(u, 0x81 | 0x8D | 0x8F | 0x90 | 0x9D) {
        u
    } else if (0x100..0x180).contains(&u) {
        LATIN_A
            .chars()
            .nth(usize::from(u - 0x100))
            .map_or(63, |c| c as u16)
    } else if u == 0x3B1 {
        u16::from(b'a')
    } else {
        u16::from(b'?')
    };
    Ok(Variant::I2(code as i16))
}

pub(super) fn ascw(ctx: &RtContext, a: &[Variant]) -> R {
    let u = *units(&text(ctx, &a[0])?).first().ok_or(VbaError::new(5))?;
    Ok(Variant::I2(u as i16))
}

pub(super) fn chr(ctx: &RtContext, a: &[Variant]) -> R {
    let n = long(ctx, &a[0])?;
    if !(0..=255).contains(&n) {
        return Err(VbaError::new(5));
    }
    text_out(from_units(&[ansi_unit(n as u16)]))
}

pub(super) fn chrw(ctx: &RtContext, a: &[Variant]) -> R {
    let n = long(ctx, &a[0])?;
    if !(-32768..=65535).contains(&n) {
        return Err(VbaError::new(5));
    }
    text_out(from_units(&[n as u16]))
}

pub(super) fn split(ctx: &RtContext, a: &[Variant]) -> R {
    if is_null(&a[0]) {
        return Err(null_error());
    }
    let expr = units(&text(ctx, &a[0])?);
    let delim = match opt(a, 1) {
        Some(v) => units(&text(ctx, v)?),
        None => vec![u16::from(b' ')],
    };
    let limit = long_or(ctx, a, 2, -1)?;
    if limit < -1 {
        return Err(VbaError::new(5));
    }
    let fold = text_compare(ctx, a, 3, true)?;
    let mut parts: Vec<String> = Vec::new();
    if limit != 0 && (!expr.is_empty() || delim.is_empty()) {
        let mut i = 0;
        while !delim.is_empty() && (limit == -1 || (parts.len() as i32) < limit - 1) {
            let Some((j, end)) = find(&expr, &delim, i, fold) else {
                break;
            };
            parts.push(from_units(&expr[i..j]));
            i = end;
        }
        parts.push(from_units(&expr[i..]));
    }
    Ok(Variant::Array(Box::new(SafeArray {
        elem: VarType::Bstr,
        bounds: vec![(0, parts.len() as u32)],
        data: parts.into_iter().map(Variant::Bstr).collect(),
    })))
}

pub(super) fn join(ctx: &RtContext, a: &[Variant]) -> R {
    let Variant::Array(arr) = &a[0] else {
        return Err(VbaError::new(13));
    };
    if arr.bounds.len() != 1 {
        return Err(VbaError::new(5));
    }
    let delim = match opt(a, 1) {
        Some(v) => text(ctx, v)?,
        None => " ".to_string(),
    };
    let parts: Result<Vec<String>, VbaError> = arr.data.iter().map(|v| text(ctx, v)).collect();
    let parts = parts?;
    if parts.is_empty() {
        return Ok(Variant::BstrNull);
    }
    text_out(parts.join(&delim))
}
