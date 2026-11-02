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
    s.encode_utf16().collect()
}

pub(super) fn from_units(u: &[u16]) -> String {
    String::from_utf16_lossy(u)
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
