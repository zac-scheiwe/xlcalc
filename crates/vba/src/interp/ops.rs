use super::stubs;
use crate::ast::UnOp;
use crate::rt::{self, Builtin};
use crate::types::BinOp;
use crate::VbaError;
use std::cmp::Ordering;
use xlcalc_variant::ops::{self as var, Binary, Unary};
use xlcalc_variant::{HResult, Locale, Variant};

pub fn find_builtin(name: &str) -> Option<&'static Builtin> {
    rt::builtin(name)
}

pub fn hresult_error(e: HResult) -> VbaError {
    match e {
        HResult::DISP_E_OVERFLOW => VbaError::new(6),
        HResult::DISP_E_TYPEMISMATCH => VbaError::new(13),
        HResult::DISP_E_DIVBYZERO => VbaError::new(11),
        HResult::E_OUTOFMEMORY => VbaError::new(7),

        HResult::E_NOTIMPL => VbaError::new(13),
        HResult::E_INVALIDARG => VbaError::new(5),
        _ => VbaError {
            number: 51,
            description: format!("Internal error (not implemented in xlcalc: {:#010X})", e.0),
        },
    }
}

pub fn var_binary(
    op: BinOp,
    a: &Variant,
    b: &Variant,
    text: bool,
    locale: Locale,
) -> Result<Variant, VbaError> {
    let o = match op {
        BinOp::Add => Binary::Add,
        BinOp::Sub => Binary::Sub,
        BinOp::Mul => Binary::Mul,
        BinOp::Div => Binary::Div,
        BinOp::IntDiv => Binary::Idiv,
        BinOp::Mod => Binary::Mod,
        BinOp::Pow => Binary::Pow,
        BinOp::And => Binary::And,
        BinOp::Or => Binary::Or,
        BinOp::Xor => Binary::Xor,
        BinOp::Eqv => Binary::Eqv,
        BinOp::Imp => Binary::Imp,
        BinOp::Cat => return cat(a, b, locale),
        BinOp::Eq | BinOp::Ne | BinOp::Lt | BinOp::Le | BinOp::Gt | BinOp::Ge => {
            return compare(op, a, b, text, locale)
        }
        BinOp::Like | BinOp::Is => unreachable!("{op:?} is evaluated by the interpreter"),
    };
    var::binary(o, a, b).map_err(hresult_error)
}

pub fn var_cmp(
    a: &Variant,
    b: &Variant,
    text: bool,
    locale: Locale,
) -> Result<Option<Ordering>, VbaError> {
    if let (Variant::Bstr(x), Variant::Bstr(y), false) = (a, b, text) {
        return Ok(Some(x.encode_utf16().cmp(y.encode_utf16())));
    }
    let flags = if text { var::NORM_IGNORECASE } else { 0 };
    Ok(
        match var::compare(a, b, locale, flags).map_err(hresult_error)? {
            0 => Some(Ordering::Less),
            1 => Some(Ordering::Equal),
            2 => Some(Ordering::Greater),
            _ => None,
        },
    )
}

fn compare(
    op: BinOp,
    a: &Variant,
    b: &Variant,
    text: bool,
    locale: Locale,
) -> Result<Variant, VbaError> {
    let Some(o) = var_cmp(a, b, text, locale)? else {
        return Ok(Variant::Null);
    };
    Ok(Variant::Bool(match op {
        BinOp::Eq => o == Ordering::Equal,
        BinOp::Ne => o != Ordering::Equal,
        BinOp::Lt => o == Ordering::Less,
        BinOp::Le => o != Ordering::Greater,
        BinOp::Gt => o == Ordering::Greater,
        _ => o != Ordering::Less,
    }))
}

pub fn var_unary(op: UnOp, a: &Variant) -> Result<Variant, VbaError> {
    let o = match op {
        UnOp::Neg => Unary::Neg,
        UnOp::Not => Unary::Not,
    };
    var::unary(o, a).map_err(hresult_error)
}

pub fn like(s: &str, pattern: &str, text: bool) -> Result<bool, VbaError> {
    stubs::like(s, pattern, text)
}

fn cat(a: &Variant, b: &Variant, locale: Locale) -> Result<Variant, VbaError> {
    if let Variant::Bstr(l) = a {
        if let (_, Some(_)) = crate::rt::split_odd(l) {
            let Variant::Bstr(r) =
                var::cat(&Variant::Bstr(String::new()), b, locale).map_err(hresult_error)?
            else {
                return var::cat(a, b, locale).map_err(hresult_error);
            };
            let mut bytes = crate::rt::bytes_of(l);
            bytes.extend(crate::rt::bytes_of(&r));
            return Ok(Variant::Bstr(crate::rt::from_bytes(&bytes)));
        }
    }
    var::cat(a, b, locale).map_err(hresult_error)
}
