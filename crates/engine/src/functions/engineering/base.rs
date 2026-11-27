use super::atp::{self, R};
use crate::eval::{EvalCtx, Operand};
use crate::functions::args::{num, text, val};
use crate::value::{Value, XlError};

#[derive(Clone, Copy)]
pub enum Radix {
    Bin,
    Oct,
    Dec,
    Hex,
}

impl Radix {
    fn base(self) -> u32 {
        match self {
            Radix::Bin => 2,
            Radix::Oct => 8,
            Radix::Dec => 10,
            Radix::Hex => 16,
        }
    }

    fn bits(self) -> u32 {
        match self {
            Radix::Bin => 10,
            Radix::Oct => 30,
            Radix::Hex => 40,
            Radix::Dec => 0,
        }
    }

    fn range(self) -> (i64, i64) {
        let b = self.bits();
        (-(1i64 << (b - 1)), (1i64 << (b - 1)) - 1)
    }
}

fn parse_fixed(s: &str, rx: Radix) -> R<i64> {
    if s.len() > 10 {
        return Err(XlError::NUM);
    }
    let mut v: i64 = 0;
    for c in s.chars() {
        let d = c.to_digit(rx.base()).ok_or(XlError::NUM)?;
        v = v * rx.base() as i64 + d as i64;
    }
    if s.len() == 10 {
        let b = rx.bits();
        if v >= 1i64 << (b - 1) {
            v -= 1i64 << b;
        }
    }
    Ok(v)
}

fn digits(mut v: u64, base: u32) -> String {
    if v == 0 {
        return "0".into();
    }
    let mut out = Vec::new();
    while v > 0 {
        out.push(
            std::char::from_digit((v % base as u64) as u32, base)
                .unwrap()
                .to_ascii_uppercase(),
        );
        v /= base as u64;
    }
    out.iter().rev().collect()
}

fn format_fixed(v: i64, rx: Radix, places: Option<usize>) -> R<String> {
    let (lo, hi) = rx.range();
    if v < lo || v > hi {
        return Err(XlError::NUM);
    }
    if v < 0 {
        let u = (v + (1i64 << rx.bits())) as u64;
        return Ok(digits(u, rx.base()));
    }
    let s = digits(v as u64, rx.base());
    match places {
        None => Ok(s),
        Some(p) if s.len() > p => Err(XlError::NUM),
        Some(p) => Ok(format!("{}{}", "0".repeat(p - s.len()), s)),
    }
}

fn places_arg(ctx: &EvalCtx, a: &[Option<Value>], i: usize) -> R<Option<usize>> {
    match atp::opt_num(ctx, a, i)? {
        None => Ok(None),
        Some(p) => {
            let p = p.trunc();
            if !(1.0..=10.0).contains(&p) {
                return Err(XlError::NUM);
            }
            Ok(Some(p as usize))
        }
    }
}

pub fn convert(ctx: &mut EvalCtx, ops: &[Operand], from: Radix, to: Radix) -> Operand {
    atp::call(ctx, ops, "a", |ctx, a, _| {
        let r: R<Value> = (|| {
            let places = if matches!(to, Radix::Dec) {
                None
            } else {
                places_arg(ctx, a, 1)?
            };
            let v: i64 = match from {
                Radix::Dec => {
                    let n = atp::req_num(ctx, a, 0)?.trunc();
                    let (lo, hi) = to.range();
                    if n < lo as f64 || n > hi as f64 {
                        return Err(XlError::NUM);
                    }
                    n as i64
                }
                _ => {
                    let s = atp::req_text(a, 0)?;
                    parse_fixed(&s, from)?
                }
            };
            match to {
                Radix::Dec => Ok(Value::Number(v as f64)),
                _ => Ok(Value::from(format_fixed(v, to, places)?)),
            }
        })();
        val(r)
    })
}

pub fn base_fn(ctx: &mut EvalCtx, ops: &[Operand]) -> Operand {
    atp::call(ctx, ops, "v", |ctx, a, _| base_vals(ctx, a))
}

fn base_vals(ctx: &mut EvalCtx, a: &[Option<Value>]) -> Value {
    let get = |i: usize| a.get(i).cloned().flatten().unwrap_or(Value::Blank);
    val((|| {
        let n_raw = num(ctx, &get(0))?;
        let n = n_raw.trunc();
        let radix = num(ctx, &get(1))?.trunc();
        let min_len = match a.get(2) {
            None | Some(None) => None,
            Some(Some(v)) => Some(num(ctx, v)?),
        };
        if n_raw < 0.0 {
            return Err(XlError::NUM);
        }
        if !(0.0..9_007_199_254_740_992.0).contains(&n) || !(2.0..=36.0).contains(&radix) {
            return Err(XlError::NUM);
        }

        if let Some(m) = min_len {
            if !(0.0..256.0).contains(&m) {
                return Err(XlError::NUM);
            }
        }
        let min_len = min_len.map(f64::trunc);
        let mut s = if n == 0.0 {
            String::new()
        } else {
            digits(n as u64, radix as u32)
        };
        let want = min_len.map_or(1, |m| m as usize);
        if s.len() < want {
            s = format!("{}{}", "0".repeat(want - s.len()), s);
        }
        Ok(Value::from(s))
    })())
}

pub fn decimal(ctx: &mut EvalCtx, a: &[Value]) -> Value {
    val((|| {
        let s = text(&a[0])?;
        let radix = num(ctx, &a[1])?.trunc();
        if s.encode_utf16().count() > 255 {
            return Err(XlError::VALUE);
        }
        if !(2.0..=36.0).contains(&radix) {
            return Err(XlError::NUM);
        }
        parse_radix_text(&s, radix).map(Value::num)
    })())
}

fn parse_radix_text(s: &str, radix: f64) -> Result<f64, XlError> {
    let mut body = s.trim_start_matches([' ', '\t']).trim_start_matches('0');
    if body.is_empty() {
        return Ok(0.0);
    }
    if radix == 16.0 {
        if let Some(r) = body.strip_suffix(['h', 'H']) {
            body = r;
        }
        if let Some(r) = body.strip_prefix(['x', 'X']) {
            body = r.trim_start_matches('0');
            if body.is_empty() {
                return Ok(0.0);
            }
        }
    }
    if radix == 2.0 {
        if let Some(r) = body.strip_suffix(['b', 'B']) {
            body = r;
        }
    }
    if body.is_empty() {
        return Err(XlError::NUM);
    }

    let mut v = 0.0f64;
    let mut p = 1.0f64;
    for c in body.chars().rev() {
        let d = c
            .to_digit(36)
            .filter(|d| *d < radix as u32)
            .ok_or(XlError::NUM)?;
        let term = crate::x87::mul(d as f64, p);
        p = crate::x87::mul(p, radix);
        v = crate::x87::add(v, term);
    }
    Ok(v)
}

pub fn roman(ctx: &mut EvalCtx, a: &[Value]) -> Value {
    val((|| {
        let n = num(ctx, &a[0])?.trunc();
        let form = match a.get(1) {
            None => 0.0,
            Some(Value::Bool(b)) => {
                if *b {
                    0.0
                } else {
                    4.0
                }
            }
            Some(v) => num(ctx, v)?.trunc(),
        };
        if !(0.0..=3999.0).contains(&n) || !(0.0..=4.0).contains(&form) {
            return Err(XlError::VALUE);
        }
        Ok(Value::from(roman_text(n as u32, form as u32)))
    })())
}

fn roman_text(n: u32, mode: u32) -> String {
    type Row = (i32, i32, char, [u8; 5], usize);
    const ROWS: [Row; 25] = [
        (1000, 1000, 'M', [5, 4, 3, 2, 1], 0),
        (999, -1, 'I', [4, 3, 2, 1, 1], 1),
        (995, -5, 'V', [3, 2, 1, 1, 1], 2),
        (990, -10, 'X', [2, 1, 1, 1, 1], 3),
        (950, -50, 'L', [1, 1, 1, 1, 1], 4),
        (900, -100, 'C', [1, 1, 1, 1, 1], 5),
        (500, 500, 'D', [5, 4, 3, 2, 1], 0),
        (499, -1, 'I', [4, 3, 2, 1, 1], 1),
        (495, -5, 'V', [3, 2, 1, 1, 1], 2),
        (490, -10, 'X', [2, 1, 1, 1, 1], 3),
        (450, -50, 'L', [1, 1, 1, 1, 1], 4),
        (400, -100, 'C', [1, 1, 1, 1, 1], 5),
        (100, 100, 'C', [3, 2, 1, 1, 1], 0),
        (99, -1, 'I', [2, 1, 1, 1, 1], 1),
        (95, -5, 'V', [1, 1, 1, 1, 1], 2),
        (90, -10, 'X', [1, 1, 1, 1, 1], 3),
        (50, 50, 'L', [3, 2, 1, 1, 1], 0),
        (49, -1, 'I', [2, 1, 1, 1, 1], 1),
        (45, -5, 'V', [1, 1, 1, 1, 1], 2),
        (40, -10, 'X', [1, 1, 1, 1, 1], 3),
        (10, 10, 'X', [1, 1, 1, 1, 1], 0),
        (9, -1, 'I', [1, 1, 1, 1, 1], 1),
        (5, 5, 'V', [1, 1, 1, 1, 1], 0),
        (4, -1, 'I', [1, 1, 1, 1, 1], 1),
        (1, 1, 'I', [0, 0, 0, 0, 0], 0),
    ];
    let mut out = String::new();
    let (mut n, mut i) = (n as i32, 0usize);
    while n > 0 {
        let (threshold, amount, letter, next, back) = ROWS[i];
        if n < threshold {
            i += next[mode as usize] as usize;
        } else {
            out.push(letter);
            n -= amount;
            i -= back;
        }
    }
    out
}

pub fn arabic(_: &mut EvalCtx, a: &[Value]) -> Value {
    val((|| {
        let s = text(&a[0])?;
        if s.encode_utf16().count() > 255 {
            return Err(XlError::VALUE);
        }
        let t = s.trim_matches(|c| matches!(c, ' ' | '\t' | '\n' | '\r'));
        let (neg, body) = match t.strip_prefix('-') {
            Some(b) => {
                if b.is_empty() {
                    return Err(XlError::NUM);
                }
                (true, b)
            }
            None => (false, t),
        };
        let mut total = 0i64;
        let mut max = 0i64;
        for c in body.chars().rev() {
            let d = match c.to_ascii_uppercase() {
                'I' => 1,
                'V' => 5,
                'X' => 10,
                'L' => 50,
                'C' => 100,
                'D' => 500,
                'M' => 1000,
                _ => return Err(XlError::VALUE),
            };
            if d < max {
                total -= d;
            } else {
                total += d;
                max = d;
            }
        }
        Ok(Value::Number(if neg {
            -total as f64
        } else {
            total as f64
        }))
    })())
}

const BIT_LIMIT: f64 = 281_474_976_710_656.0;

fn bit_range(n: f64) -> R<u64> {
    if n < 0.0 || n >= BIT_LIMIT || n.trunc() != n {
        return Err(XlError::NUM);
    }
    Ok(n as u64)
}

pub fn bitop(ctx: &mut EvalCtx, a: &[Value], op: fn(u64, u64) -> u64) -> Value {
    val((|| {
        let (x, y) = (num(ctx, &a[0])?, num(ctx, &a[1])?);
        let (x, y) = (bit_range(x)?, bit_range(y)?);
        Ok(Value::Number(op(x, y) as f64))
    })())
}

pub fn bitshift(ctx: &mut EvalCtx, a: &[Value], dir: f64) -> Value {
    val((|| {
        let n = num(ctx, &a[0])?;
        let s = num(ctx, &a[1])?.trunc() * dir;
        let n = bit_range(n)?;
        if n == 0 {
            return Ok(Value::Number(0.0));
        }
        if s.abs() > 53.0 {
            return Err(XlError::NUM);
        }
        let s = s as i32;
        if s < 0 {
            return Ok(Value::Number((n >> (-s) as u32) as f64));
        }

        if s >= 49 {
            return Ok(Value::Number(0.0));
        }
        let r = (n as u128) << s;
        if r >= 1u128 << 48 {
            return Err(XlError::NUM);
        }
        Ok(Value::Number(r as f64))
    })())
}

pub fn delta(ctx: &mut EvalCtx, ops: &[Operand], ge: bool) -> Operand {
    atp::call(ctx, ops, "b", |ctx, a, _| {
        val((|| {
            let x = atp::req_num(ctx, a, 0);
            let y = atp::opt_num(ctx, a, 1)?.unwrap_or(0.0);
            let x = x?;
            let r = if ge { x >= y } else { x == y };
            Ok(Value::Number(if r { 1.0 } else { 0.0 }))
        })())
    })
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn roman_forms() {
        assert_eq!(roman_text(499, 0), "CDXCIX");
        assert_eq!(roman_text(499, 1), "LDVLIV");
        assert_eq!(roman_text(499, 2), "XDIX");
        assert_eq!(roman_text(499, 3), "VDIV");
        assert_eq!(roman_text(499, 4), "ID");
        assert_eq!(roman_text(1999, 1), "MLMVLIV");
        assert_eq!(roman_text(1990, 4), "MXM");
        assert_eq!(roman_text(3888, 0), "MMMDCCCLXXXVIII");
        assert_eq!(roman_text(0, 0), "");
    }

    #[test]
    fn decimal_text_padding_and_affixes() {
        let d = parse_radix_text;
        assert_eq!(d("\t101", 2.0), Ok(5.0));
        assert_eq!(d(" \t 0101", 2.0), Ok(5.0));
        assert_eq!(d("\t\t101", 2.0), Ok(5.0));
        assert_eq!(d("\n101", 2.0), Err(XlError::NUM));
        assert_eq!(d("\r101", 2.0), Err(XlError::NUM));
        assert_eq!(d("\u{a0}101", 2.0), Err(XlError::NUM));
        assert_eq!(d("101 ", 2.0), Err(XlError::NUM));
        assert_eq!(d("101\t", 2.0), Err(XlError::NUM));
        assert_eq!(d("0x\tff", 16.0), Err(XlError::NUM));
        assert_eq!(d("\t0xff", 16.0), Ok(255.0));
        assert_eq!(d("\t", 2.0), Ok(0.0));
        assert_eq!(d("x", 16.0), Ok(0.0));
        assert_eq!(d("101b", 2.0), Ok(5.0));
        assert_eq!(d("101B", 2.0), Ok(5.0));
        assert_eq!(d("b", 2.0), Err(XlError::NUM));
        assert_eq!(d("ffh", 16.0), Ok(255.0));
        assert_eq!(d("FFH", 16.0), Ok(255.0));
        assert_eq!(d("h", 16.0), Err(XlError::NUM));
        assert_eq!(d("ffh", 10.0), Err(XlError::NUM));
        assert_eq!(d("zz", 36.0), Ok(1295.0));
    }

    #[test]
    fn fixed_radix() {
        assert_eq!(parse_fixed("1111111111", Radix::Bin), Ok(-1));
        assert_eq!(parse_fixed("FFFFFFFF5B", Radix::Hex), Ok(-165));
        assert_eq!(format_fixed(-100, Radix::Bin, None).unwrap(), "1110011100");
        assert_eq!(format_fixed(100, Radix::Hex, Some(4)).unwrap(), "0064");
        assert!(format_fixed(512, Radix::Bin, None).is_err());
    }
}
