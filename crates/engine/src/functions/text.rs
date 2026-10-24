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
        val(text(&a[0])
            .map(|s| Value::from(s.chars().filter(|&c| c as u32 >= 32).collect::<String>())))
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
            Value::Lambda(_) => Value::Error(XlError::Value),
            _ => Value::empty_text(),
        })
    }),
];

fn checked(s: String) -> Value {
    if utf16_len(&s) > MAX_TEXT_LEN {
        err(XlError::Value)
    } else {
        Value::from(s)
    }
}

fn count_arg(ctx: &EvalCtx, a: &[Value], i: usize, default: f64) -> R<usize> {
    let n = opt_num(ctx, a, i, default)?;
    if n < 0.0 {
        return Err(XlError::Value);
    }
    Ok(n.trunc().min(1e9) as usize)
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
            return Err(XlError::Value);
        }
        let n = n.trunc();
        Ok(Value::from(utf16_slice(
            &s,
            (start as usize).saturating_sub(1).min(usize::MAX / 2),
            n.min(1e9) as usize,
        )))
    })())
}

fn map_char(c: char, upper: bool, next_letter: bool) -> char {
    let n = c as u32;
    if upper {
        match n {
            181 | 383 | 837 | 1011 | 8126 | 8560..=8575 | 5112..=5117 => return c,
            912 => return '\u{3AA}',
            _ => {}
        }
    } else {
        match n {
            304 => return 'i',
            931 => return if next_letter { '\u{3C3}' } else { '\u{3C2}' },
            7838
            | 8486
            | 8490
            | 8491
            | 8544..=8559
            | 5024..=5109
            | 8124
            | 8072..=8079
            | 8088..=8095
            | 8104..=8111 => return c,
            _ => {}
        }
    }
    let mut it: Box<dyn Iterator<Item = char>> = if upper {
        Box::new(c.to_uppercase())
    } else {
        Box::new(c.to_lowercase())
    };
    match (it.next(), it.next()) {
        (Some(m), None) => m,
        _ => c,
    }
}

pub fn map_case(s: &str, upper: bool) -> String {
    let mut out = String::with_capacity(s.len());
    let mut it = s.chars().peekable();
    while let Some(c) = it.next() {
        let next_letter = it.peek().is_some_and(|n| n.is_alphabetic());
        out.push(map_char(c, upper, next_letter));
    }
    out
}

fn proper(s: &str) -> String {
    let mut out = String::with_capacity(s.len());
    let mut prev_letter = false;
    let mut it = s.chars().peekable();
    while let Some(c) = it.next() {
        if c.is_alphabetic() {
            let next_letter = it.peek().is_some_and(|n| n.is_alphabetic());

            out.push(if c == '\u{130}' && !prev_letter {
                'I'
            } else {
                map_char(c, !prev_letter, next_letter)
            });
            prev_letter = true;
        } else {
            out.push(c);
            prev_letter = false;
        }
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
    let mut out = String::new();
    for op in ops {
        if let Err(e) = each_text(ctx, op, |s, _| out.push_str(s)) {
            return Operand::err(e);
        }
    }
    if utf16_len(&out) > MAX_TEXT_LEN {
        return Operand::err(XlError::Calc);
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
        return Operand::err(XlError::Calc);
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
        let n = num(ctx, &a[1])?.trunc();
        if n < 0.0 {
            return Err(XlError::Value);
        }
        if utf16_len(&s) as f64 * n > MAX_TEXT_LEN as f64 {
            return Err(XlError::Value);
        }
        Ok(Value::from(s.repeat(n as usize)))
    })())
}

fn substitute(ctx: &mut EvalCtx, a: &[Value]) -> Value {
    val((|| {
        let s = text(&a[0])?;
        let old = text(&a[1])?;
        let new = text(&a[2])?;

        let k = match a.get(3) {
            None => None,
            Some(Value::Bool(_)) => return Err(XlError::Value),
            Some(v) => {
                let k = num(ctx, v)?.trunc();
                if k < 1.0 || k >= 2147483647.0 {
                    return Err(XlError::Value);
                }
                Some(k as usize)
            }
        };
        if old.is_empty() {
            return Ok(Value::Text(s));
        }
        match k {
            None => Ok(checked(s.replace(&*old, &new))),
            Some(k) => match s.match_indices(&*old).nth(k - 1) {
                Some((i, _)) => {
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
        let start = num(ctx, &a[1])?.trunc();
        let n = num(ctx, &a[2])?;
        let new = text(&a[3])?;
        if start < 1.0 || n < 0.0 || start > 32767.0 || n > 32767.0 {
            return Err(XlError::Value);
        }
        let n = n.trunc();
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
        let start = opt_num(ctx, a, 2, 1.0)?.trunc();
        let len = utf16_len(&hay);

        if start < 1.0
            || start as usize > len + 1
            || (start as usize > len && (search || !needle.is_empty()))
        {
            return Err(XlError::Value);
        }
        let start = start as usize - 1;
        if needle.is_empty() {
            return Ok(Value::Number(start as f64 + 1.0));
        }

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
        let pos = if search {
            Wildcard::new(&needle).find(&hay, start)
        } else {
            let byte_start = hay.char_indices().nth(start).map_or(hay.len(), |(i, _)| i);
            hay[byte_start..]
                .find(&*needle)
                .map(|b| start + hay[byte_start..byte_start + b].chars().count())
        };
        let pos = pos.ok_or(XlError::Value)?;

        let units: usize = hay.chars().take(pos).map(char::len_utf16).sum();
        Ok(Value::Number(units as f64 + 1.0))
    })())
}

fn value(ctx: &mut EvalCtx, a: &[Value]) -> Value {
    match &a[0] {
        Value::Number(n) => Value::Number(*n),
        Value::Blank => Value::Number(0.0),
        Value::Text(_) => val(ctx.to_number(&a[0]).map(Value::Number)),
        Value::Error(e) => Value::Error(*e),
        _ => err(XlError::Value),
    }
}

fn numbervalue(_: &mut EvalCtx, a: &[Value]) -> Value {
    val((|| {
        let s = text(&a[0])?;
        let dec = match a.get(1) {
            Some(v) => text(v)?.chars().next().ok_or(XlError::Value)?,
            None => '.',
        };
        let group = match a.get(2) {
            Some(v) => text(v)?.chars().next().ok_or(XlError::Value)?,
            None if dec == ',' => '.',
            None => ',',
        };
        if dec == group {
            return Err(XlError::Value);
        }
        let mut cleaned = String::new();
        let mut pct = 0;
        let mut seen_dec = false;
        for c in s.chars().filter(|c| !c.is_whitespace()) {
            if c == group {
                if seen_dec {
                    return Err(XlError::Value);
                }
                continue;
            }
            if c == dec {
                seen_dec = true;
                cleaned.push('.');
            } else if c == '%' {
                pct += 1;
            } else {
                cleaned.push(c);
            }
        }
        if cleaned.is_empty() {
            return Ok(Value::Number(0.0));
        }
        let (neg, body) = match cleaned.strip_prefix('-') {
            Some(b) => (true, b.to_string()),
            None => (false, cleaned.trim_start_matches('+').to_string()),
        };

        let (neg, body) = match body.strip_prefix('(').and_then(|b| b.strip_suffix(')')) {
            Some(inner) if !neg => (true, inner.to_string()),
            _ => (neg, body),
        };
        let mut n = crate::number::parse_literal(&body).ok_or(XlError::Value)?;
        for _ in 0..pct {
            n /= 100.0;
        }
        Ok(Value::num(if neg { -n } else { n }))
    })())
}

const CP1252_HIGH: [u32; 32] = [
    0x20AC, 0x81, 0x201A, 0x0192, 0x201E, 0x2026, 0x2020, 0x2021, 0x02C6, 0x2030, 0x0160, 0x2039,
    0x0152, 0x8D, 0x017D, 0x8F, 0x90, 0x2018, 0x2019, 0x201C, 0x201D, 0x2022, 0x2013, 0x2014,
    0x02DC, 0x2122, 0x0161, 0x203A, 0x0153, 0x9D, 0x017E, 0x0178,
];

fn char_fn(ctx: &mut EvalCtx, a: &[Value]) -> Value {
    val((|| {
        let n = num(ctx, &a[0])?.trunc();
        if !(1.0..=255.0).contains(&n) {
            return Err(XlError::Value);
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
        let c = s.chars().next().ok_or(XlError::Value)? as u32;
        let code = if c < 128 || (160..256).contains(&c) {
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
        if n < 1.0 || n > 0x10FFFF as f64 {
            return Err(XlError::Value);
        }
        let u = n as u32;

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
        let c = s.chars().next().ok_or(XlError::Value)?;
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
            return Err(XlError::Value);
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
        .unwrap_or(err(XlError::Value))
}

fn na_stub(c: &mut EvalCtx, a: &[Value]) -> Value {
    for v in a {
        if let Err(e) = num(c, v) {
            return Value::Error(e);
        }
    }
    err(XlError::NA)
}
