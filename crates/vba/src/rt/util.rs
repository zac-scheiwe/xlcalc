use super::RtContext;
use crate::VbaError;
use xlcalc_variant::{change_type, flags, HResult, VarType, Variant};

pub(super) type R = Result<Variant, VbaError>;

pub(super) fn null_error() -> VbaError {
    VbaError {
        number: 94,
        description: "Invalid use of Null".to_string(),
    }
}

pub(super) fn from_hr(h: HResult) -> VbaError {
    const DIV_ZERO: i32 = 0x8002_0012_u32 as i32;
    VbaError::new(match h {
        HResult::DISP_E_OVERFLOW => 6,
        HResult::DISP_E_TYPEMISMATCH => 13,
        HResult(DIV_ZERO) => 11,
        _ => 5,
    })
}

pub(super) fn missing(v: &Variant) -> bool {
    matches!(v, Variant::Error(e) if *e == HResult::DISP_E_PARAMNOTFOUND.0)
}

pub(super) fn opt(args: &[Variant], i: usize) -> Option<&Variant> {
    args.get(i).filter(|v| !missing(v))
}

pub(super) fn convert(ctx: &RtContext, v: &Variant, to: VarType) -> R {
    if matches!(v, Variant::Null) {
        return Err(null_error());
    }
    change_type(v, to, ctx.locale, flags::ALPHABOOL).map_err(from_hr)
}

pub(super) fn double(ctx: &RtContext, v: &Variant) -> Result<f64, VbaError> {
    match convert(ctx, v, VarType::R8)? {
        Variant::R8(x) => Ok(x),
        _ => Err(VbaError::new(13)),
    }
}

pub(super) fn long(ctx: &RtContext, v: &Variant) -> Result<i32, VbaError> {
    match convert(ctx, v, VarType::I4)? {
        Variant::I4(x) => Ok(x),
        _ => Err(VbaError::new(13)),
    }
}

pub(super) fn text(ctx: &RtContext, v: &Variant) -> Result<String, VbaError> {
    if matches!(v, Variant::BstrNull) {
        return Ok(String::new());
    }
    match convert(ctx, v, VarType::Bstr)? {
        Variant::Bstr(s) => Ok(s),
        _ => Err(VbaError::new(13)),
    }
}

pub(super) fn long_or(
    ctx: &RtContext,
    args: &[Variant],
    i: usize,
    default: i32,
) -> Result<i32, VbaError> {
    opt(args, i).map_or(Ok(default), |v| long(ctx, v))
}

pub(super) fn is_null(v: &Variant) -> bool {
    matches!(v, Variant::Null)
}

pub(super) fn units(s: &str) -> Vec<u16> {
    let (body, _) = split_odd(s);
    body_units(body)
}

const LONE_BASE: u32 = 0x10F000;

fn lone_unit(c: char) -> Option<u16> {
    let v = c as u32;
    (LONE_BASE..LONE_BASE + 0x800)
        .contains(&v)
        .then(|| (v - LONE_BASE) as u16 + 0xD800)
}

fn body_units(body: &str) -> Vec<u16> {
    let mut out = Vec::with_capacity(body.len());
    let mut buf = [0u16; 2];
    for c in body.chars() {
        match lone_unit(c) {
            Some(u) => out.push(u),
            None => out.extend_from_slice(c.encode_utf16(&mut buf)),
        }
    }
    out
}

pub fn host_text(s: &str) -> String {
    split_odd(s)
        .0
        .chars()
        .map(|c| {
            if lone_unit(c).is_some() {
                char::REPLACEMENT_CHARACTER
            } else {
                c
            }
        })
        .collect()
}

pub(crate) fn join(a: String, b: &str) -> String {
    if !b.is_empty() && split_odd(&a).1.is_some() {
        let mut bytes = bytes_of(&a);
        bytes.extend(bytes_of(b));
        return from_bytes(&bytes);
    }
    normalize(a + b)
}

pub(crate) fn normalize(s: String) -> String {
    let (body, odd) = split_odd(&s);
    if !body.chars().any(|c| lone_unit(c).is_some()) {
        return s;
    }
    let mut out = from_units(&body_units(body));
    if let Some(b) = odd {
        out.push(char::from_u32(ODD_BASE + u32::from(b)).unwrap_or('?'));
    }
    out
}

const ODD_BASE: u32 = 0x10FF00;

pub(crate) fn split_odd(s: &str) -> (&str, Option<u8>) {
    match s.chars().next_back() {
        Some(c) if (ODD_BASE..=ODD_BASE + 255).contains(&(c as u32)) => (
            &s[..s.len() - c.len_utf8()],
            Some((c as u32 - ODD_BASE) as u8),
        ),
        _ => (s, None),
    }
}

pub(crate) fn bytes_of(s: &str) -> Vec<u8> {
    let (body, odd) = split_odd(s);
    let mut v: Vec<u8> = body_units(body)
        .into_iter()
        .flat_map(u16::to_le_bytes)
        .collect();
    v.extend(odd);
    v
}

pub(crate) fn from_bytes(b: &[u8]) -> String {
    let u: Vec<u16> = b
        .as_chunks::<2>()
        .0
        .iter()
        .map(|c| u16::from_le_bytes(*c))
        .collect();
    let mut s = from_units(&u);
    if b.len() % 2 == 1 {
        s.push(char::from_u32(ODD_BASE + u32::from(b[b.len() - 1])).unwrap_or('?'));
    }
    s
}

pub(super) fn from_units(u: &[u16]) -> String {
    char::decode_utf16(u.iter().copied())
        .map(|r| {
            r.unwrap_or_else(|e| {
                char::from_u32(LONE_BASE + u32::from(e.unpaired_surrogate()) - 0xD800)
                    .unwrap_or(char::REPLACEMENT_CHARACTER)
            })
        })
        .collect()
}

pub(super) fn long_value(n: i64) -> R {
    i32::try_from(n)
        .map(Variant::I4)
        .map_err(|_| VbaError::new(6))
}

pub(super) fn text_out(s: String) -> R {
    Ok(Variant::Bstr(s))
}

pub(super) fn text_out_null(s: String) -> R {
    Ok(if s.is_empty() {
        Variant::BstrNull
    } else {
        Variant::Bstr(s)
    })
}
