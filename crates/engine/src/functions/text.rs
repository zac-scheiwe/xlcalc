use super::args::*;
use super::{args, scalar, FnDef, MANY, MANY254};
use crate::coerce;
use crate::eval::{EvalCtx, Operand};
use crate::text::{utf16_len, utf16_slice, Wildcard};
use crate::value::{Value, XlError, MAX_TEXT_LEN};

pub const DEFS: &[FnDef] = &[
    scalar("LEN", 1, 1, |_, a| {
        val(text(&a[0]).map(|s| Value::Number(utf16_len(&s) as f64)))
    }),
    scalar("LEFT", 1, 2, |c, a| left_right(c, a, true)),
    scalar("RIGHT", 1, 2, |c, a| left_right(c, a, false)),
    scalar("MID", 3, 3, mid),
    scalar("UPPER", 1, 1, |_, a| {
        val(text(&a[0]).map(|s| Value::from(map_case(&s, true))))
    }),
    scalar("LOWER", 1, 1, |_, a| {
        val(text(&a[0]).map(|s| Value::from(map_case(&s, false))))
    }),
    scalar("PROPER", 1, 1, |_, a| {
        val(text(&a[0]).map(|s| Value::from(proper(&s))))
    }),
    scalar("TRIM", 1, 1, |_, a| {
        val(text(&a[0]).map(|s| Value::from(trim(&s))))
    }),
    scalar("CLEAN", 1, 1, |_, a| {
        val(text(&a[0]).map(|s| {
            Value::from(
                s.chars()
                    .filter(|&c| c as u32 >= 32 && !(0x80..=0x9F).contains(&(c as u32)))
                    .collect::<String>(),
            )
        }))
    }),
    scalar("CONCATENATE", 1, MANY, concatenate),
    args("CONCAT", 1, MANY254, "R", concat),
    args("TEXTJOIN", 3, MANY254, "RVR", textjoin),
    scalar("REPT", 2, 2, rept),
    scalar("EXACT", 2, 2, |_, a| {
        val((|| Ok(Value::Bool(text(&a[0])? == text(&a[1])?)))())
    }),
    scalar("SUBSTITUTE", 3, 4, substitute),
    scalar("REPLACE", 4, 4, replace),
    scalar("FIND", 2, 3, |c, a| find(c, a, false)),
    scalar("SEARCH", 2, 3, |c, a| find(c, a, true)),
    scalar("VALUE", 1, 1, value),
    scalar("NUMBERVALUE", 1, 3, numbervalue),
    scalar("CHAR", 1, 1, char_fn),
    scalar("CODE", 1, 1, code),
    scalar("UNICHAR", 1, 1, unichar),
    scalar("UNICODE", 1, 1, unicode),
    scalar("ASC", 1, 1, |_, a| val(text(&a[0]).map(Value::Text))),
    scalar("DBCS", 1, 1, |_, a| val(text(&a[0]).map(Value::Text))),
    scalar("ENCODEURL", 1, 1, encodeurl),
    scalar("HYPERLINK", 1, 2, hyperlink),
    scalar("THAINUMSOUND", 1, 1, locale_stub),
    scalar("THAINUMSTRING", 1, 1, locale_stub),
    scalar("ROUNDBAHTDOWN", 1, 1, locale_stub),
    scalar("ROUNDBAHTUP", 1, 1, locale_stub),
    scalar("THAIDAYOFWEEK", 1, 1, locale_stub),
    scalar("THAIMONTHOFYEAR", 1, 1, locale_stub),
    scalar("THAIYEAR", 1, 1, locale_stub),
    scalar("THAIDIGIT", 1, 1, locale_stub),
    scalar("ISTHAIDIGIT", 1, 1, locale_stub),
    scalar("THAISTRINGLENGTH", 1, 1, locale_stub),
    scalar("DATESTRING", 1, 1, na_stub),
    scalar("NUMBERSTRING", 2, 2, na_stub),
    args("T", 1, 1, "R", |c, o| {
        first_cell_or_lift(c, &o[0], |v| match v {
            Value::Text(s) => Value::Text(s.clone()),
            Value::Error(e) => Value::Error(*e),
            Value::Lambda(_) => Value::Error(XlError::VALUE),
            _ => Value::empty_text(),
        })
    }),
];

fn checked(s: String) -> Value {
    if utf16_len(&s) > MAX_TEXT_LEN {
        err(XlError::VALUE)
    } else {
        Value::from(s)
    }
}

fn count_arg(ctx: &EvalCtx, a: &[Value], i: usize, default: f64) -> R<usize> {
    let n = opt_num(ctx, a, i, default)?;
    if n < 0.0 {
        return Err(XlError::VALUE);
    }
    Ok(ifuzz(n).min(1e9) as usize)
}

pub(super) fn left_right(ctx: &mut EvalCtx, a: &[Value], left: bool) -> Value {
    val((|| {
        let s = text(&a[0])?;
        let n = count_arg(ctx, a, 1, 1.0)?;

        let total = s.chars().count();
        Ok(Value::from(if left {
            s.chars().take(n).collect::<String>()
        } else {
            s.chars().skip(total.saturating_sub(n)).collect::<String>()
        }))
    })())
}

pub(super) fn mid(ctx: &mut EvalCtx, a: &[Value]) -> Value {
    val((|| {
        let s = text(&a[0])?;
        let start = num(ctx, &a[1])?.trunc();
        let n = num(ctx, &a[2])?;
        if start < 1.0 || n < 0.0 || start > 2147483648.0 || n >= 2147483648.0 {
            return Err(XlError::VALUE);
        }
        let n = n.trunc();
        Ok(Value::from(utf16_slice(
            &s,
            (start as usize).saturating_sub(1).min(usize::MAX / 2),
            n.min(1e9) as usize,
        )))
    })())
}

use super::casemap;

fn table_map(table: &[(u16, u16)], c: char) -> char {
    let n = c as u32;
    if n > 0xFFFF {
        return c;
    }
    match table.binary_search_by_key(&(n as u16), |e| e.0) {
        Ok(i) => char::from_u32(table[i].1 as u32).unwrap_or(c),
        Err(_) => c,
    }
}

pub fn lower_char(c: char) -> char {
    match c {
        '\u{3C2}' => '\u{3C3}',
        '\u{3F5}' => '\u{3B5}',
        '\u{13A0}'..='\u{13F5}' | '\u{1C90}'..='\u{1CBF}' => c,
        _ => {
            let mut l = c.to_lowercase();
            match (l.next(), l.next()) {
                (Some(x), None) => x,
                _ => c,
            }
        }
    }
}

const SIGMA_FINAL_BEFORE: &str = ".,;?!:/\\|{}[]-_'\"<>@#$%&+= \t\r\n";

pub fn map_case(s: &str, upper: bool) -> String {
    let mut out = String::with_capacity(s.len());
    let mut it = s.chars().peekable();
    while let Some(c) = it.next() {
        if !upper && c == '\u{3A3}' {
            let fin = it.peek().is_none_or(|n| SIGMA_FINAL_BEFORE.contains(*n));
            out.push(if fin { '\u{3C2}' } else { '\u{3C3}' });
            continue;
        }
        out.push(table_map(
            if upper {
                casemap::UPPER
            } else {
                casemap::LOWER
            },
            c,
        ));
    }
    out
}

fn xl_letter(c: char) -> bool {
    let n = c as u32;
    if n > 0xFFFF {
        return false;
    }
    let n = n as u16;
    let i = casemap::LETTERS.partition_point(|r| r.1 < n);
    i < casemap::LETTERS.len() && casemap::LETTERS[i].0 <= n
}

fn proper(s: &str) -> String {
    let lowered = map_case(s, false);
    let mut out = String::with_capacity(s.len());
    let mut prev_letter = false;
    for (orig, c) in s.chars().zip(lowered.chars()) {
        let letter = xl_letter(orig);
        if letter && !prev_letter {
            let p = table_map(casemap::PROPER_UPPER, c);
            out.push(if p != c {
                p
            } else {
                table_map(casemap::UPPER, c)
            });
        } else {
            out.push(c);
        }
        prev_letter = letter;
    }
    out
}

fn trim(s: &str) -> String {
    s.split(' ')
        .filter(|w| !w.is_empty())
        .collect::<Vec<_>>()
        .join(" ")
}

fn concatenate(_: &mut EvalCtx, a: &[Value]) -> Value {
    val((|| {
        let mut out = String::new();
        for v in a {
            out.push_str(&text(v)?);
        }

        Ok(Value::from(if utf16_len(&out) > MAX_TEXT_LEN {
            utf16_slice(&out, 0, MAX_TEXT_LEN)
        } else {
            out
        }))
    })())
}

fn each_text(ctx: &mut EvalCtx, op: &Operand, mut f: impl FnMut(&str, bool)) -> R<()> {
    let mut failure = None;
    match op {
        Operand::Missing => f("", true),
        Operand::Val(Value::Array(a)) => {
            for v in &a.data {
                f(&coerce::to_text(v)?, v.is_blank());
            }
        }
        Operand::Val(v) => f(&coerce::to_text(v)?, v.is_blank()),
        Operand::Ref(r) => {
            for area in r.areas() {
                let area = *area;
                let arr = ctx.area_array(&area);
                let vals: Vec<Value> = match arr {
                    Value::Array(a) => a.data.clone(),
                    v => vec![v],
                };
                for v in &vals {
                    match coerce::to_text(v) {
                        Ok(s) => f(&s, v.is_blank()),
                        Err(e) => {
                            failure = Some(e);
                            break;
                        }
                    }
                }
            }
        }
    }
    failure.map_or(Ok(()), Err)
}

fn concat(ctx: &mut EvalCtx, ops: &[Operand]) -> Operand {
    for op in ops {
        if let Operand::Val(Value::Error(e)) = op {
            return Operand::err(*e);
        }
    }
    let mut out = String::new();
    for op in ops {
        if let Err(e) = each_text(ctx, op, |s, _| out.push_str(s)) {
            return Operand::err(e);
        }
    }
    if utf16_len(&out) > MAX_TEXT_LEN {
        return Operand::err(XlError::CALC);
    }
    Operand::Val(Value::from(out))
}

fn textjoin(ctx: &mut EvalCtx, ops: &[Operand]) -> Operand {
    if let Operand::Val(Value::Error(e)) = &ops[0] {
        return Operand::err(*e);
    }
    let ignore_empty = match &ops[1] {
        Operand::Val(v) => match boolean(v) {
            Ok(b) => b,
            Err(e) => return Operand::err(e),
        },

        Operand::Missing => true,
        _ => false,
    };

    for op in &ops[2..] {
        if let Operand::Val(Value::Error(e)) = op {
            return Operand::err(*e);
        }
    }

    let mut delims: Vec<String> = Vec::new();
    if let Err(e) = each_text(ctx, &ops[0], |s, _| delims.push(s.to_string())) {
        return Operand::err(e);
    }
    if delims.is_empty() {
        delims.push(String::new());
    }
    let mut parts: Vec<String> = Vec::new();

    let (mut all_n, mut all_len) = (0usize, 0usize);
    for op in &ops[2..] {
        if let Err(e) = each_text(ctx, op, |s, blank| {
            if ignore_empty && blank {
                return;
            }
            all_n += 1;
            all_len += utf16_len(s);
            if !(ignore_empty && s.is_empty()) {
                parts.push(s.to_string());
            }
        }) {
            return Operand::err(e);
        }
    }
    let gaps: usize = (0..all_n.saturating_sub(1))
        .map(|i| utf16_len(&delims[i % delims.len()]))
        .sum();
    if all_len + gaps > MAX_TEXT_LEN {
        return Operand::err(XlError::CALC);
    }
    let mut out = String::new();
    for (i, p) in parts.iter().enumerate() {
        if i > 0 {
            out.push_str(&delims[(i - 1) % delims.len()]);
        }
        out.push_str(p);
    }
    Operand::Val(checked(out))
}

fn rept(ctx: &mut EvalCtx, a: &[Value]) -> Value {
    val((|| {
        let s = text(&a[0])?;
        let n = num(ctx, &a[1])?;
        if n < 0.0 {
            return Err(XlError::VALUE);
        }
        let n = n.trunc();
        if utf16_len(&s) as f64 * n > MAX_TEXT_LEN as f64 {
            return Err(XlError::VALUE);
        }
        Ok(Value::from(s.repeat(n as usize)))
    })())
}

fn nth_overlapping(s: &str, old: &str, k: usize) -> Option<usize> {
    let mut from = 0;
    let mut n = 0;
    while let Some(p) = s[from..].find(old) {
        let i = from + p;
        n += 1;
        if n == k {
            return Some(i);
        }
        from = i + s[i..].chars().next()?.len_utf8();
    }
    None
}

fn substitute(ctx: &mut EvalCtx, a: &[Value]) -> Value {
    val((|| {
        let s = text(&a[0])?;
        let old = text(&a[1])?;
        let new = text(&a[2])?;

        let k = match a.get(3) {
            None => None,
            Some(Value::Bool(_)) => return Err(XlError::VALUE),
            Some(v) => {
                let k = ifuzz(num(ctx, v)?);
                if k < 1.0 || k >= 2147483647.0 {
                    return Err(XlError::VALUE);
                }
                Some(k as usize)
            }
        };
        if old.is_empty() {
            return Ok(Value::Text(s));
        }
        match k {
            None => Ok(checked(s.replace(&*old, &new))),
            Some(k) => match nth_overlapping(&s, &old, k) {
                Some(i) => {
                    let mut out = String::with_capacity(s.len());
                    out.push_str(&s[..i]);
                    out.push_str(&new);
                    out.push_str(&s[i + old.len()..]);
                    Ok(checked(out))
                }
                None => Ok(Value::Text(s)),
            },
        }
    })())
}

pub(super) fn replace(ctx: &mut EvalCtx, a: &[Value]) -> Value {
    val((|| {
        let s = text(&a[0])?;
        let start = ifuzz(num(ctx, &a[1])?);
        let n = num(ctx, &a[2])?;
        let new = text(&a[3])?;
        if start < 1.0 || n < 0.0 || start > 32767.0 || n > 32767.0 {
            return Err(XlError::VALUE);
        }
        let n = ifuzz(n);
        let start = (start as usize - 1).min(utf16_len(&s));
        let mut out = utf16_slice(&s, 0, start);
        out.push_str(&new);
        out.push_str(&utf16_slice(
            &s,
            start.saturating_add(n as usize),
            usize::MAX / 2,
        ));
        Ok(checked(out))
    })())
}

fn find(ctx: &mut EvalCtx, a: &[Value], search: bool) -> Value {
    val((|| {
        let needle = text(&a[0])?;
        let hay = text(&a[1])?;
        let start = ifuzz(opt_num(ctx, a, 2, 1.0)?);
        let len = utf16_len(&hay);

        if start < 1.0
            || start as usize > len + 1
            || (start as usize > len && (search || !needle.is_empty()))
        {
            return Err(XlError::VALUE);
        }
        let start = start as usize - 1;
        if needle.is_empty() {
            return Ok(Value::Number(start as f64 + 1.0));
        }

        let unit_start = start;
        let start = {
            let mut units = 0;
            let mut idx = 0;
            for c in hay.chars() {
                if units >= start {
                    break;
                }
                units += c.len_utf16();
                idx += 1;
            }
            idx
        };
        if search {
            let pos = Wildcard::new(&needle)
                .find(&hay, unit_start)
                .ok_or(XlError::VALUE)?;
            return Ok(Value::Number(pos as f64 + 1.0));
        }
        let pos = {
            let byte_start = hay.char_indices().nth(start).map_or(hay.len(), |(i, _)| i);
            hay[byte_start..]
                .find(&*needle)
                .map(|b| start + hay[byte_start..byte_start + b].chars().count())
        };
        let pos = pos.ok_or(XlError::VALUE)?;

        let units: usize = hay.chars().take(pos).map(char::len_utf16).sum();
        Ok(Value::Number(units as f64 + 1.0))
    })())
}

fn value(ctx: &mut EvalCtx, a: &[Value]) -> Value {
    match &a[0] {
        Value::Number(n) => Value::Number(*n),
        Value::Blank => Value::Number(0.0),
        Value::Text(s) => val(crate::coerce::parse_number_text(s, ctx.parse_env())
            .map(|p| Value::Number(p.value))
            .ok_or(XlError::VALUE)),
        Value::Error(e) => Value::Error(*e),
        _ => err(XlError::VALUE),
    }
}

fn numbervalue(ctx: &mut EvalCtx, a: &[Value]) -> Value {
    val((|| {
        let s = text(&a[0])?;
        let env = ctx.parse_env();
        let first =
            |v: &Value| -> Result<char, XlError> { text(v)?.chars().next().ok_or(XlError::VALUE) };
        let ldec = env.locale.decimal_sep;
        let dec = match a.get(1) {
            Some(v) => first(v)?,
            None => ldec,
        };
        let mut group = match a.get(2) {
            Some(v) => first(v)?,
            None => env.locale.thousands_sep,
        };
        if dec == group {
            if a.len() != 2 {
                return Err(XlError::VALUE);
            }
            group = ldec;
        }
        let mut cleaned: Vec<char> = Vec::new();
        let mut seen_dec = false;
        for c in s.chars() {
            if matches!(c, '\t' | '\n' | '\r' | ' ') || (!seen_dec && c == group) {
                continue;
            }
            if cleaned.len() >= 255 {
                return Err(XlError::VALUE);
            }
            if c == dec {
                seen_dec = true;
                cleaned.push(ldec);
            } else {
                cleaned.push(c);
            }
        }
        let mut pct = 0;
        while cleaned.last() == Some(&'%') {
            cleaned.pop();
            pct += 1;
        }
        if cleaned.is_empty() {
            return Ok(Value::Number(0.0));
        }
        let mut loc = env.locale.clone();
        loc.date_order = crate::locale::DateOrder::DMY;
        let body: String = cleaned.into_iter().collect();
        let mut n = coerce::parse_number_text(
            &body,
            coerce::ParseEnv {
                locale: &loc,
                ..env
            },
        )
        .ok_or(XlError::VALUE)?
        .value;
        for _ in 0..pct {
            n /= 100.0;
        }
        Ok(Value::num(n))
    })())
}

const CP1252_HIGH: [u32; 32] = [
    0x20AC, 0x81, 0x201A, 0x0192, 0x201E, 0x2026, 0x2020, 0x2021, 0x02C6, 0x2030, 0x0160, 0x2039,
    0x0152, 0x8D, 0x017D, 0x8F, 0x90, 0x2018, 0x2019, 0x201C, 0x201D, 0x2022, 0x2013, 0x2014,
    0x02DC, 0x2122, 0x0161, 0x203A, 0x0153, 0x9D, 0x017E, 0x0178,
];

fn char_fn(ctx: &mut EvalCtx, a: &[Value]) -> Value {
    val((|| {
        let n = ifuzz(num(ctx, &a[0])?);
        if !(1.0..=255.0).contains(&n) {
            return Err(XlError::VALUE);
        }
        let n = n as u32;
        let cp = if (128..160).contains(&n) {
            CP1252_HIGH[(n - 128) as usize]
        } else {
            n
        };
        Ok(Value::from(char::from_u32(cp).unwrap_or('?').to_string()))
    })())
}

fn code(_: &mut EvalCtx, a: &[Value]) -> Value {
    val((|| {
        let s = text(&a[0])?;
        let c = s.chars().next().ok_or(XlError::VALUE)? as u32;
        let code = if c < 128
            || (128..160).contains(&c) && !CP1252_HIGH.contains(&c)
            || (160..256).contains(&c)
        {
            c
        } else {
            CP1252_HIGH
                .iter()
                .position(|&x| x == c)
                .map_or(63, |i| 128 + i as u32)
        };
        Ok(Value::Number(code as f64))
    })())
}

fn unichar(ctx: &mut EvalCtx, a: &[Value]) -> Value {
    val((|| {
        let n = num(ctx, &a[0])?.trunc();
        if n < 1.0 || n >= 2147483648.0 {
            return Err(XlError::VALUE);
        }
        let u = n as u32;

        if (u & 0xFFFE) == 0xFFFE {
            return Err(XlError::NA);
        }
        if u > 0x10FFFF {
            return Err(XlError::VALUE);
        }

        if (0xFDD0..=0xFDEF).contains(&u) || (u & 0xFFFE) == 0xFFFE {
            return Err(XlError::NA);
        }
        let c = char::from_u32(u).ok_or(XlError::NA)?;
        Ok(Value::from(c.to_string()))
    })())
}

fn unicode(_: &mut EvalCtx, a: &[Value]) -> Value {
    val((|| {
        let s = text(&a[0])?;
        let c = s.chars().next().ok_or(XlError::VALUE)?;
        Ok(Value::Number(c as u32 as f64))
    })())
}

fn encodeurl(_: &mut EvalCtx, a: &[Value]) -> Value {
    val((|| {
        let s = text(&a[0])?;
        let mut out = String::with_capacity(s.len());
        for b in s.bytes() {
            if b.is_ascii_alphanumeric() || matches!(b, b'-' | b'_' | b'.') {
                out.push(b as char);
            } else {
                out.push_str(&format!("%{b:02X}"));
            }
        }
        if utf16_len(&out) > MAX_TEXT_LEN {
            return Err(XlError::VALUE);
        }
        Ok(Value::from(out))
    })())
}

fn hyperlink(_: &mut EvalCtx, a: &[Value]) -> Value {
    let link = match text(&a[0]) {
        Ok(t) => t,
        Err(e) => return Value::Error(e),
    };
    match a.get(1) {
        None => Value::Text(link),
        Some(Value::Blank) => Value::Number(0.0),
        Some(v) => v.clone(),
    }
}

fn locale_stub(_: &mut EvalCtx, a: &[Value]) -> Value {
    a.iter()
        .find(|v| v.is_error())
        .cloned()
        .unwrap_or(err(XlError::VALUE))
}

fn na_stub(c: &mut EvalCtx, a: &[Value]) -> Value {
    for v in a {
        if let Err(e) = num(c, v) {
            return Value::Error(e);
        }
    }
    err(XlError::NA)
}
