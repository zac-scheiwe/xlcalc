use super::util::{
    convert, double, from_hr, is_null, long, long_or, null_error, text, text_out, R,
};
use super::{dates, RtContext};
use crate::VbaError;
use xlcalc_variant::ops::{self, Unary};
use xlcalc_variant::{VarType, Variant};

pub(super) fn cstr(ctx: &RtContext, a: &[Variant]) -> R {
    match &a[0] {
        Variant::BstrNull => Ok(Variant::BstrNull),
        Variant::Error(e) => text_out(format!("Error {}", e & 0xFFFF)),

        Variant::Array(arr) if arr.elem == VarType::Ui1 => text_out(super::from_bytes(
            &arr.data
                .iter()
                .map(|b| if let Variant::Ui1(b) = b { *b } else { 0 })
                .collect::<Vec<_>>(),
        )),
        v => text_out(text(ctx, v)?),
    }
}

fn code_value(v: &Variant) -> Variant {
    match v {
        Variant::Error(e) => Variant::I4(e & 0xFFFF),
        v => v.clone(),
    }
}

pub(super) fn cdbl(ctx: &RtContext, a: &[Variant]) -> R {
    convert(ctx, &code_value(&a[0]), VarType::R8)
}

pub(super) fn clng(ctx: &RtContext, a: &[Variant]) -> R {
    convert(ctx, &code_value(&a[0]), VarType::I4)
}

pub(super) fn cint(ctx: &RtContext, a: &[Variant]) -> R {
    convert(ctx, &code_value(&a[0]), VarType::I2)
}

pub(super) fn cbyte(ctx: &RtContext, a: &[Variant]) -> R {
    convert(ctx, &code_value(&a[0]), VarType::Ui1)
}

pub(super) fn cbool(ctx: &RtContext, a: &[Variant]) -> R {
    convert(ctx, &code_value(&a[0]), VarType::Bool)
}

pub(super) fn ccur(ctx: &RtContext, a: &[Variant]) -> R {
    convert(ctx, &code_value(&a[0]), VarType::Cy)
}

pub(super) fn cdec(ctx: &RtContext, a: &[Variant]) -> R {
    convert(ctx, &a[0], VarType::Decimal)
}

pub(super) fn csng(ctx: &RtContext, a: &[Variant]) -> R {
    convert(ctx, &code_value(&a[0]), VarType::R4)
}

pub(super) fn cdate(ctx: &RtContext, a: &[Variant]) -> R {
    dates::to_date(ctx, &a[0]).map(Variant::Date)
}

pub(super) fn cdate_typed(ctx: &RtContext, a: &[Variant]) -> R {
    match convert(ctx, &a[0], VarType::Date)? {
        Variant::Date(x) if dates::in_date_range(x) => Ok(Variant::Date(x)),
        Variant::Date(_) => Err(VbaError::new(6)),
        _ => Err(VbaError::new(13)),
    }
}

pub(super) fn select_test(_: &RtContext, a: &[Variant]) -> R {
    Ok(Variant::Bool(matches!(a[0], Variant::Bool(true))))
}

pub(super) fn cvar(_: &RtContext, a: &[Variant]) -> R {
    Ok(a[0].clone())
}

pub(super) fn cverr(ctx: &RtContext, a: &[Variant]) -> R {
    if let Variant::Error(_) = a[0] {
        return Ok(a[0].clone());
    }
    let n = long(ctx, &a[0])?;
    if !(0..=65535).contains(&n) {
        return Err(VbaError::new(5));
    }
    Ok(Variant::Error(if n == 0 {
        0
    } else {
        0x800A_0000_u32 as i32 | n
    }))
}

pub(super) fn val(ctx: &RtContext, a: &[Variant]) -> R {
    let s = text(ctx, &a[0])?;
    let s: String = s
        .chars()
        .filter(|c| !matches!(c, ' ' | '\t' | '\n' | '\r'))
        .collect();
    let x = val_text(&s);
    if x.is_infinite() {
        return Err(VbaError::new(6));
    }
    Ok(Variant::R8(x))
}

fn val_text(s: &str) -> f64 {
    let lower = s.to_ascii_lowercase();
    for (prefix, radix) in [("&h", 16), ("&o", 8), ("&", 8)] {
        if let Some(rest) = lower.strip_prefix(prefix) {
            let digits: String = rest.chars().take_while(|c| c.is_digit(radix)).collect();
            let v = u64::from_str_radix(&digits, radix).unwrap_or(0);

            let short = digits.len() <= if radix == 16 { 4 } else { 6 }
                && !rest[digits.len()..].starts_with('&');
            return if short {
                f64::from(v as u16 as i16)
            } else {
                f64::from(v as u32 as i32)
            };
        }
    }
    let b = lower.as_bytes();
    let mut i = 0;
    let mut out = String::new();
    if matches!(b.first(), Some(b'+' | b'-')) {
        out.push(b[0] as char);
        i = 1;
    }
    let mut digits = 0;
    while i < b.len() && b[i].is_ascii_digit() {
        out.push(b[i] as char);
        i += 1;
        digits += 1;
    }
    if i < b.len() && b[i] == b'.' {
        out.push('.');
        i += 1;
        while i < b.len() && b[i].is_ascii_digit() {
            out.push(b[i] as char);
            i += 1;
            digits += 1;
        }
    }
    if digits == 0 {
        return 0.0;
    }
    if i < b.len() && matches!(b[i], b'e' | b'd') {
        let mut j = i + 1;
        let mut exp = String::from("e");
        if j < b.len() && matches!(b[j], b'+' | b'-') {
            exp.push(b[j] as char);
            j += 1;
        }
        let start = j;
        while j < b.len() && b[j].is_ascii_digit() {
            exp.push(b[j] as char);
            j += 1;
        }
        if j > start {
            out.push_str(&exp);
        }
    }
    out.parse().unwrap_or(0.0)
}

fn str_text(ctx: &RtContext, v: &Variant) -> R {
    if matches!(v, Variant::Empty) {
        return text_out(" 0".to_string());
    }
    let s = match v {
        Variant::Bstr(_) => text(ctx, &convert(ctx, v, VarType::R8)?)?,
        v => text(ctx, v)?,
    };
    let s = match s.strip_prefix("0.") {
        Some(rest) if !matches!(v, Variant::Date(_)) => format!(".{rest}"),
        _ => match s.strip_prefix("-0.") {
            Some(rest) => format!("-.{rest}"),
            None => s,
        },
    };
    let signed = s.starts_with('-') || matches!(v, Variant::Bool(_) | Variant::Date(_));
    text_out(if signed { s } else { format!(" {s}") })
}

pub(super) fn str(ctx: &RtContext, a: &[Variant]) -> R {
    if is_null(&a[0]) {
        return Ok(Variant::Null);
    }
    str_text(ctx, &a[0])
}

pub(super) fn str_d(ctx: &RtContext, a: &[Variant]) -> R {
    if is_null(&a[0]) {
        return Err(null_error());
    }
    str_text(ctx, &a[0])
}

fn radix(ctx: &RtContext, v: &Variant, base: u32) -> R {
    if is_null(v) {
        return Ok(Variant::Null);
    }
    let (n, bits): (i128, u32) = match v {
        Variant::Ui1(x) => (i128::from(*x), 16),
        Variant::I2(x) => (i128::from(*x), 16),
        Variant::Bool(b) => (-i128::from(*b), 16),
        Variant::I4(x) => (i128::from(*x), 32),
        Variant::I8(x) => (i128::from(*x), 64),
        Variant::Error(_) => return Err(VbaError::new(13)),
        v => {
            let x = double(ctx, v)?;
            let whole = x.fract() == 0.0 && x.abs() <= f64::from(i32::MAX);
            let r = x.round_ties_even();
            if !(-(2f64.powi(63))..2f64.powi(63)).contains(&r) {
                return Err(VbaError::new(6));
            }
            (
                r as i128,
                if whole && !matches!(v, Variant::Bstr(_)) {
                    32
                } else {
                    64
                },
            )
        }
    };
    let n = if n < 0 { n + (1i128 << bits) } else { n } as u128;
    text_out(if base == 16 {
        format!("{n:X}")
    } else {
        format!("{n:o}")
    })
}

pub(super) fn hex(ctx: &RtContext, a: &[Variant]) -> R {
    radix(ctx, &a[0], 16)
}

pub(super) fn hex_d(ctx: &RtContext, a: &[Variant]) -> R {
    if is_null(&a[0]) {
        return Err(null_error());
    }
    radix(ctx, &a[0], 16)
}

pub(super) fn oct(ctx: &RtContext, a: &[Variant]) -> R {
    radix(ctx, &a[0], 8)
}

pub(super) fn oct_d(ctx: &RtContext, a: &[Variant]) -> R {
    if is_null(&a[0]) {
        return Err(null_error());
    }
    radix(ctx, &a[0], 8)
}

fn unary(op: Unary, v: &Variant) -> R {
    ops::unary(op, v).map_err(from_hr)
}

pub(super) fn fix(_: &RtContext, a: &[Variant]) -> R {
    unary(Unary::Fix, &a[0])
}

pub(super) fn int(_: &RtContext, a: &[Variant]) -> R {
    unary(Unary::Int, &a[0])
}

pub(super) fn abs(_: &RtContext, a: &[Variant]) -> R {
    unary(Unary::Abs, &a[0])
}

pub(super) fn abs_typed(_: &RtContext, a: &[Variant]) -> R {
    if matches!(
        a[0],
        Variant::I2(i16::MIN) | Variant::I4(i32::MIN) | Variant::I8(i64::MIN)
    ) {
        return Err(VbaError::new(6));
    }
    unary(Unary::Abs, &a[0])
}

pub(super) fn sgn(ctx: &RtContext, a: &[Variant]) -> R {
    let x = double(ctx, &a[0])?;
    Ok(Variant::I2(if x > 0.0 {
        1
    } else if x < 0.0 {
        -1
    } else {
        0
    }))
}

pub(super) fn round(ctx: &RtContext, a: &[Variant]) -> R {
    let places = long_or(ctx, a, 1, 0)?;
    ops::round(&a[0], places).map_err(from_hr)
}

pub(super) fn clnglng(ctx: &RtContext, a: &[Variant]) -> R {
    convert(ctx, &code_value(&a[0]), VarType::I8)
}
