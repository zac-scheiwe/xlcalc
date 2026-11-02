use super::util::R;
use super::{dates, RtContext};
use xlcalc_variant::{change_type, flags, VarType, Variant, VT_ARRAY};

fn boolean(b: bool) -> R {
    Ok(Variant::Bool(b))
}

pub(super) fn is_numeric(ctx: &RtContext, a: &[Variant]) -> R {
    boolean(match &a[0] {
        Variant::Date(_)
        | Variant::Null
        | Variant::Error(_)
        | Variant::Array(_)
        | Variant::Dispatch(_)
        | Variant::Unknown(_) => false,
        v => change_type(v, VarType::R8, ctx.locale, flags::ALPHABOOL).is_ok(),
    })
}

pub(super) fn is_date(ctx: &RtContext, a: &[Variant]) -> R {
    boolean(match &a[0] {
        Variant::Date(_) => true,
        Variant::Bstr(s) => dates::parse_date(ctx, s).is_some(),
        _ => false,
    })
}

pub(super) fn is_empty(_: &RtContext, a: &[Variant]) -> R {
    boolean(matches!(a[0], Variant::Empty))
}

pub(super) fn do_events(_: &RtContext, _: &[Variant]) -> R {
    Ok(Variant::I2(0))
}

pub(super) fn iif(ctx: &RtContext, a: &[Variant]) -> R {
    let truth = match &a[0] {
        Variant::Null => false,
        Variant::Error(_) => return Err(crate::VbaError::new(13)),
        v => match super::util::convert(ctx, v, VarType::Bool)? {
            Variant::Bool(b) => b,
            _ => return Err(crate::VbaError::new(13)),
        },
    };
    Ok(if truth { a[1].clone() } else { a[2].clone() })
}

pub(super) fn is_null(_: &RtContext, a: &[Variant]) -> R {
    boolean(matches!(a[0], Variant::Null))
}

pub(super) fn is_error(_: &RtContext, a: &[Variant]) -> R {
    boolean(matches!(a[0], Variant::Error(_)))
}

pub(super) fn is_array(_: &RtContext, a: &[Variant]) -> R {
    boolean(matches!(a[0], Variant::Array(_)))
}

pub(super) fn var_type(_: &RtContext, a: &[Variant]) -> R {
    Ok(Variant::I4(i32::from(a[0].vt())))
}

fn name_of(t: u16) -> &'static str {
    match t {
        0 => "Empty",
        1 => "Null",
        2 => "Integer",
        3 => "Long",
        4 => "Single",
        5 => "Double",
        6 => "Currency",
        7 => "Date",
        8 => "String",
        9 => "Object",
        10 => "Error",
        11 => "Boolean",
        12 => "Variant",
        13 => "Unknown",
        14 => "Decimal",
        17 => "Byte",
        20 => "LongLong",
        _ => "Unknown",
    }
}

pub(super) fn type_name(_: &RtContext, a: &[Variant]) -> R {
    let vt = a[0].vt();
    let s = match &a[0] {
        Variant::Dispatch(None) => "Nothing".to_string(),
        Variant::Array(arr) => format!(
            "{}()",
            if arr.elem == VarType::Variant {
                "Variant"
            } else {
                name_of(vt & !VT_ARRAY)
            }
        ),
        _ => name_of(vt).to_string(),
    };
    Ok(Variant::Bstr(s))
}
