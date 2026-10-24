use super::args::*;
use super::text::{left_right, mid, replace};
use super::{args, scalar, FnDef};
use crate::eval::{EvalCtx, Operand};
use crate::number::format_general;
use crate::numfmt::{decimal::Dec, fixed_text, NumberFormat};
use crate::text::{utf16_len, Wildcard};
use crate::value::{Array, Value, XlError, MAX_TEXT_LEN};

pub const DEFS: &[FnDef] = &[
    scalar("TEXT", 2, 2, text_fn),
    args("FIXED", 1, 3, "VVV", |c, o| {
        let vals: Vec<Value> = o
            .iter()
            .enumerate()
            .map(|(i, op)| match op {
                Operand::Val(v) => v.clone(),
                _ if i == 1 => Value::Number(2.0),
                _ => Value::Blank,
            })
            .collect();
        Operand::Val(fixed(c, &vals))
    }),
    scalar("DOLLAR", 1, 2, dollar),
    scalar("USDOLLAR", 1, 2, dollar),
    scalar("VALUETOTEXT", 1, 2, valuetotext),
    args("ARRAYTOTEXT", 1, 2, "MV", arraytotext),
    args("TEXTBEFORE", 2, 6, "VRVVVV", |c, o| {
        before_after(c, o, true)
    }),
    args("TEXTAFTER", 2, 6, "VRVVVV", |c, o| {
        before_after(c, o, false)
    }),
    args("TEXTSPLIT", 2, 6, "VRRVVR", textsplit),
    scalar("BAHTTEXT", 1, 1, bahttext),
    scalar("LENB", 1, 1, |_, a| {
        val(text(&a[0]).map(|s| Value::Number(utf16_len(&s) as f64)))
    }),
    scalar("LEFTB", 1, 2, |c, a| left_right(c, a, true)),
    scalar("RIGHTB", 1, 2, |c, a| left_right(c, a, false)),
    scalar("MIDB", 3, 3, mid),
    scalar("FINDB", 2, 3, |c, a| find(c, a, false)),
    scalar("SEARCHB", 2, 3, |c, a| find(c, a, true)),
    scalar("REPLACEB", 4, 4, replace),
];

const MAX_FORMATTED: usize = 255;

fn limited(s: String, max: usize, e: XlError) -> Value {
    if utf16_len(&s) > max {
        err(e)
    } else {
        Value::from(s)
    }
}

fn text_fn(ctx: &mut EvalCtx, a: &[Value]) -> Value {
    let v = match &a[0] {
        Value::Error(e) => return err(*e),
        Value::Text(s) if utf16_len(s) > MAX_FORMATTED => return err(XlError::Value),
        Value::Text(s) => match ctx.to_number(&a[0]) {
            Ok(n) => Value::Number(n),
            Err(_) => Value::Text(s.clone()),
        },
        other => other.clone(),
    };
    let code: String = match &a[1] {
        Value::Text(s) => s.to_string(),
        Value::Number(n) => format_general(*n),
        Value::Blank => String::new(),
        Value::Error(e) => return err(*e),
        _ => return err(XlError::Value),
    };
    let f = match NumberFormat::parse(&code) {
        Ok(f) => f,
        Err(e) => return err(e),
    };
    match f.format(&v, &ctx.wb.locale, ctx.wb.settings.date_system) {
        Ok(r) => limited(r.text, MAX_FORMATTED, XlError::Value),
        Err(e) => err(e),
    }
}

fn decimals_arg(ctx: &EvalCtx, a: &[Value], blank_default: f64) -> R<i32> {
    let d = match a.get(1) {
        None => 2.0,
        Some(Value::Blank) => blank_default,
        Some(v) => num(ctx, v)?.trunc(),
    };
    if d > 127.0 {
        return Err(XlError::Value);
    }
    Ok(d.max(-400.0) as i32)
}

fn fixed(ctx: &mut EvalCtx, a: &[Value]) -> Value {
    val((|| {
        let x = num(ctx, &a[0])?;
        let d = decimals_arg(ctx, a, 0.0)?;
        let no_commas = match a.get(2) {
            None | Some(Value::Blank) => false,
            Some(v) => boolean(v)?,
        };
        let (body, zero) = fixed_text(x, d, !no_commas, &ctx.wb.locale);
        let s = if x < 0.0 && !zero {
            format!("-{body}")
        } else {
            body
        };
        Ok(limited(s, MAX_FORMATTED, XlError::Value))
    })())
}

fn dollar(ctx: &mut EvalCtx, a: &[Value]) -> Value {
    val((|| {
        let x = num(ctx, &a[0])?;
        let d = decimals_arg(ctx, a, 0.0)?;
        let loc = &ctx.wb.locale;
        let (body, zero) = fixed_text(x, d, true, loc);

        let negative = x < 0.0 && !(d < 0 && zero);
        let s = if negative {
            format!("-{}{body}", loc.currency)
        } else {
            format!("{}{body}", loc.currency)
        };
        Ok(limited(s, MAX_FORMATTED, XlError::Value))
    })())
}

fn strict_arg(ctx: &EvalCtx, v: Option<&Value>) -> R<bool> {
    let f = match v {
        None | Some(Value::Blank) => 0.0,
        Some(v) => num(ctx, v)?.trunc(),
    };
    match f as i64 {
        0 => Ok(false),
        1 => Ok(true),
        _ => Err(XlError::Value),
    }
}

fn quote(s: &str) -> String {
    format!("\"{}\"", s.replace('"', "\"\""))
}

fn value_text(v: &Value, strict: bool) -> String {
    match v {
        Value::Text(s) => {
            if strict {
                quote(s)
            } else {
                s.to_string()
            }
        }
        Value::Number(n) => format_general(*n),
        Value::Bool(b) => if *b { "TRUE" } else { "FALSE" }.to_string(),
        Value::Error(e) => e.as_str().to_string(),
        Value::Blank => String::new(),
        Value::Array(_) | Value::Lambda(_) => XlError::Value.as_str().to_string(),
    }
}

fn valuetotext(ctx: &mut EvalCtx, a: &[Value]) -> Value {
    let strict = match strict_arg(ctx, a.get(1)) {
        Ok(s) => s,
        Err(e) => return err(e),
    };
    limited(value_text(&a[0], strict), MAX_TEXT_LEN, XlError::Calc)
}

fn arraytotext(ctx: &mut EvalCtx, ops: &[Operand]) -> Operand {
    let strict = match ops.get(1) {
        None | Some(Operand::Missing) => false,
        Some(Operand::Val(v)) => match strict_arg(ctx, Some(v)) {
            Ok(s) => s,
            Err(e) => return Operand::err(e),
        },
        Some(_) => return Operand::err(XlError::Value),
    };
    let grid = match Grid::from_operand_3d(&ops[0]) {
        Ok(g) => g,
        Err(e) => return Operand::err(e),
    };
    let (rows, cols) = (grid.rows(), grid.cols());
    if rows == 1 && cols == 1 {
        match grid.get(ctx, 0, 0) {
            Value::Error(e) if !matches!(grid, Grid::Array(_)) => return Operand::err(e),
            Value::Blank if !matches!(grid, Grid::Scalar(_)) => {
                return Operand::err(XlError::Value)
            }
            _ => {}
        }
    }
    let mut out = String::new();
    if strict {
        out.push('{');
    }
    for r in 0..rows {
        for c in 0..cols {
            if r > 0 || c > 0 {
                if !strict {
                    out.push_str(", ");
                } else if c == 0 {
                    out.push(';');
                } else {
                    out.push(',');
                }
            }
            let v = grid.get(ctx, r, c);
            out.push_str(&value_text(&v, strict));
        }
        if utf16_len(&out) > MAX_TEXT_LEN {
            return Operand::err(XlError::Calc);
        }
    }
    if strict {
        out.push('}');
    }
    Operand::Val(limited(out, MAX_TEXT_LEN, XlError::Calc))
}

fn fold(c: char) -> char {
    let mut l = c.to_lowercase();
    let first = l.next().unwrap_or(c);
    if l.next().is_some() {
        c
    } else {
        first
    }
}

fn scalar_like(op: &Operand) -> bool {
    match op {
        Operand::Val(Value::Array(_)) => false,
        Operand::Ref(r) => r.single().is_some_and(|a| a.rect.is_cell()),
        _ => true,
    }
}

fn scalar_error(ctx: &mut EvalCtx, op: &Operand) -> Option<XlError> {
    if !scalar_like(op) {
        return None;
    }
    match op {
        Operand::Val(Value::Error(e)) => Some(*e),
        Operand::Ref(_) => match ctx.deref(op.clone()) {
            Value::Error(e) => Some(e),
            _ => None,
        },
        _ => None,
    }
}

fn delimiters(ctx: &mut EvalCtx, op: &Operand, forbid_empty: bool) -> R<Vec<Vec<char>>> {
    let grid = Grid::from_operand_3d(op)?;
    let mut out = Vec::new();
    for r in 0..grid.rows() {
        for c in 0..grid.cols() {
            let v = grid.get(ctx, r, c);
            let t: Vec<char> = text(&v)?.chars().collect();
            if forbid_empty && t.is_empty() {
                return Err(XlError::Value);
            }
            out.push(t);
        }
    }
    Ok(out)
}

struct Matcher {
    delims: Vec<Vec<char>>,
    fold: bool,
}

impl Matcher {
    fn new(delims: Vec<Vec<char>>, fold: bool) -> Matcher {
        let delims = if fold {
            delims
                .into_iter()
                .map(|d| d.into_iter().map(self::fold).collect())
                .collect()
        } else {
            delims
        };
        Matcher { delims, fold }
    }

    fn longest_at(&self, t: &[char], p: usize, end: usize) -> Option<usize> {
        let mut best: Option<usize> = None;
        for d in &self.delims {
            if d.is_empty() || p + d.len() > end {
                continue;
            }
            let ok = if self.fold {
                d.iter().zip(&t[p..]).all(|(a, b)| *a == fold(*b))
            } else {
                d[..] == t[p..p + d.len()]
            };
            if ok && best.is_none_or(|b| d.len() > b) {
                best = Some(d.len());
            }
        }
        best
    }
}

fn int_flag(ctx: &EvalCtx, op: Option<&Operand>) -> R<bool> {
    match op {
        None | Some(Operand::Missing) => Ok(false),
        Some(Operand::Val(v)) => {
            let x = num(ctx, v)?.floor();
            if x == 0.0 {
                Ok(false)
            } else if x == 1.0 {
                Ok(true)
            } else {
                Err(XlError::Value)
            }
        }
        Some(_) => Err(XlError::Value),
    }
}

fn before_after(ctx: &mut EvalCtx, ops: &[Operand], before: bool) -> Operand {
    opval((|| {
        let t: Vec<char> = match &ops[0] {
            Operand::Val(v) => text(v)?.chars().collect(),
            _ => Vec::new(),
        };

        if let Some(e) = scalar_error(ctx, &ops[1]) {
            return Err(e);
        }
        let explicit = matches!(ops.get(2), Some(Operand::Val(_)));
        let n = match ops.get(2) {
            None | Some(Operand::Missing) => 1.0,
            Some(Operand::Val(v)) => num(ctx, v)?.floor(),
            Some(_) => return Err(XlError::Value),
        };
        let fold = int_flag(ctx, ops.get(3))?;
        let match_end = int_flag(ctx, ops.get(4))?;
        let not_found = match ops.get(5) {
            None | Some(Operand::Missing) => None,
            Some(Operand::Val(v)) => Some(v.clone()),
            Some(_) => return Err(XlError::Value),
        };
        let delims = match &ops[1] {
            Operand::Missing => vec![Vec::new()],
            op => delimiters(ctx, op, false)?,
        };
        if n == 0.0 {
            return Err(XlError::Value);
        }
        let slice =
            |a: usize, b: usize| -> Value { Value::from(t[a..b].iter().collect::<String>()) };
        let len = t.len();

        if delims.iter().any(|d| d.is_empty()) {
            let one_char = t.iter().map(|c| c.len_utf16()).sum::<usize>() == 1;
            let at = if n > 0.0 && !one_char { 0 } else { len };
            return Ok(if before { slice(0, at) } else { slice(at, len) });
        }

        let min_len = delims.iter().map(Vec::len).min().unwrap_or(0) as f64;
        let max_instance = len as f64 - min_len + 1.0;
        if explicit && n.abs() > max_instance {
            return Err(XlError::Value);
        }
        let fits = max_instance >= 1.0;
        let m = Matcher::new(delims, fold);
        let want = n.abs() as usize;
        let mut count = 0usize;
        let mut found: Option<(usize, usize)> = None;
        if n > 0.0 {
            let mut p = 0;
            while p < len {
                if let Some(l) = m.longest_at(&t, p, len) {
                    count += 1;
                    if count == want {
                        found = Some((p, l));
                        break;
                    }
                    p += l;
                } else {
                    p += 1;
                }
            }
        } else {
            let mut end = len;
            let mut p = len;
            while p > 0 {
                p -= 1;
                if let Some(l) = m.longest_at(&t, p, end) {
                    count += 1;
                    if count == want {
                        found = Some((p, l));
                        break;
                    }
                    end = p;
                }
            }
        }
        if found.is_none() && match_end && fits && count + 1 == want {
            found = Some(if n > 0.0 { (len, 0) } else { (0, 0) });
        }
        match found {
            Some((p, l)) => Ok(if before {
                slice(0, p)
            } else {
                slice(p + l, len)
            }),
            None => match not_found {
                Some(v) => Ok(Value::Text(text(&v)?)),
                None => Err(XlError::NA),
            },
        }
    })())
}

fn split_delims(ctx: &mut EvalCtx, op: Option<&Operand>) -> R<Option<Vec<Vec<char>>>> {
    match op {
        None | Some(Operand::Missing) => Ok(None),
        Some(op) => Ok(Some(delimiters(ctx, op, true)?)),
    }
}

fn textsplit(ctx: &mut EvalCtx, ops: &[Operand]) -> Operand {
    let r = (|| -> R<Value> {
        let t: Vec<char> = match &ops[0] {
            Operand::Val(v) => text(v)?.chars().collect(),
            _ => Vec::new(),
        };
        for op in &ops[1..ops.len().min(3)] {
            if let Some(e) = scalar_error(ctx, op) {
                return Err(e);
            }
        }
        let ignore_empty = match ops.get(3) {
            None | Some(Operand::Missing) => false,
            Some(Operand::Val(v)) => boolean(v)?,
            Some(_) => return Err(XlError::Value),
        };
        let fold = int_flag(ctx, ops.get(4))?;
        let pad = match ops.get(5) {
            None => Value::Error(XlError::NA),
            Some(Operand::Missing) => Value::Blank,
            Some(Operand::Ref(r)) if r.is_span() => Value::Error(XlError::Ref),
            Some(Operand::Ref(r)) if r.single().is_none() => Value::Error(XlError::Value),
            Some(Operand::Val(Value::Array(_))) => return Err(XlError::Value),
            Some(op) => match Grid::from_operand(op)? {
                g if g.rows() == 1 && g.cols() == 1 => g.get(ctx, 0, 0),
                _ => return Err(XlError::Value),
            },
        };
        let col = split_delims(ctx, ops.get(1))?;
        let row = split_delims(ctx, ops.get(2))?;
        if col.is_none() && row.is_none() {
            return Err(XlError::Value);
        }
        if t.is_empty() {
            return Err(XlError::Value);
        }
        let cm = Matcher::new(col.unwrap_or_default(), fold);
        let rm = Matcher::new(row.unwrap_or_default(), fold);
        let len = t.len();
        let mut rows: Vec<Vec<String>> = vec![Vec::new()];
        let (mut start, mut p) = (0usize, 0usize);
        while p < len {
            if let Some(l) = cm.longest_at(&t, p, len) {
                rows.last_mut().unwrap().push(t[start..p].iter().collect());
                p += l;
                start = p;
            } else if let Some(l) = rm.longest_at(&t, p, len) {
                rows.last_mut().unwrap().push(t[start..p].iter().collect());
                rows.push(Vec::new());
                p += l;
                start = p;
            } else {
                p += 1;
            }
        }
        rows.last_mut().unwrap().push(t[start..].iter().collect());
        if ignore_empty {
            rows = rows
                .into_iter()
                .map(|r| r.into_iter().filter(|s| !s.is_empty()).collect::<Vec<_>>())
                .filter(|r| !r.is_empty())
                .collect();
        }
        if rows.is_empty() {
            return Err(XlError::Calc);
        }
        let width = rows.iter().map(Vec::len).max().unwrap_or(1);
        let nrows = rows.len();
        let mut data = Vec::with_capacity(nrows * width);
        for r in rows {
            let k = r.len();
            data.extend(r.into_iter().map(Value::from));
            data.extend(std::iter::repeat_n(pad.clone(), width - k));
        }
        Ok(Array::new(nrows as u32, width as u32, data).into_value())
    })();
    opval(r)
}

const THAI_DIGITS: [&str; 10] = [
    "ศูนย์",
    "หนึ่ง",
    "สอง",
    "สาม",
    "สี่",
    "ห้า",
    "หก",
    "เจ็ด",
    "แปด",
    "เก้า",
];
const THAI_UNITS: [&str; 6] = ["", "สิบ", "ร้อย", "พัน", "หมื่น", "แสน"];

fn thai_group(digits: &[u8], higher: bool, out: &mut String) {
    let n = digits.len();
    for (i, &d) in digits.iter().enumerate() {
        let pos = n - 1 - i;
        if d == 0 {
            continue;
        }
        let any_before = higher || digits[..i].iter().any(|&x| x != 0);
        match pos {
            0 => out.push_str(if d == 1 && any_before {
                "เอ็ด"
            } else {
                THAI_DIGITS[d as usize]
            }),
            1 => {
                match d {
                    1 => {}
                    2 => out.push_str("ยี่"),
                    _ => out.push_str(THAI_DIGITS[d as usize]),
                }
                out.push_str(THAI_UNITS[1]);
            }
            _ => {
                out.push_str(THAI_DIGITS[d as usize]);
                out.push_str(THAI_UNITS[pos]);
            }
        }
    }
}

fn thai_number(digits: &[u8], out: &mut String) {
    let first = digits.iter().position(|&d| d != 0);
    let Some(first) = first else { return };
    let digits = &digits[first..];
    let n = digits.len();
    let groups = n.div_ceil(6);
    let head = n - (groups - 1) * 6;
    let mut start = 0;
    for g in 0..groups {
        let len = if g == 0 { head } else { 6 };
        let group = &digits[start..start + len];
        thai_group(
            group,
            start > 0 && digits[..start].iter().any(|&x| x != 0),
            out,
        );
        start += len;
        if g + 1 < groups {
            out.push_str("ล้าน");
        }
    }
}

fn bahttext(ctx: &mut EvalCtx, a: &[Value]) -> Value {
    val((|| {
        let x = num(ctx, &a[0])?;
        let d = Dec::from_f64(x).round_to(2);
        let baht = d.int_digits();
        let satang = d.frac_digits(2);
        let mut out = String::new();
        if x < 0.0 {
            out.push_str("ลบ");
        }
        let has_baht = baht.iter().any(|&b| b != 0);
        let has_satang = satang.iter().any(|&b| b != 0);
        if has_baht {
            thai_number(&baht, &mut out);
            out.push_str("บาท");
        } else if !has_satang {
            out.push_str("ศูนย์บาท");
        }
        if has_satang {
            thai_number(&satang, &mut out);
            out.push_str("สตางค์");
        } else {
            out.push_str("ถ้วน");
        }
        Ok(Value::from(out))
    })())
}

fn find(ctx: &mut EvalCtx, a: &[Value], search: bool) -> Value {
    val((|| {
        let needle = text(&a[0])?;
        let hay = text(&a[1])?;
        let start = opt_num(ctx, a, 2, 1.0)?.trunc();
        let len = hay.chars().count();

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
