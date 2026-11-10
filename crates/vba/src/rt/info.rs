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

pub(super) fn error_text(ctx: &RtContext, a: &[Variant]) -> R {
    let Some(v) = a.first().filter(|v| !matches!(v, Variant::Error(_))) else {
        return Ok(Variant::BstrNull);
    };
    if super::util::is_null(v) {
        return Err(super::util::null_error());
    }
    let n = super::util::long(ctx, v)?;
    if n > 65535 {
        return Err(crate::VbaError::new(6));
    }
    if n == 0 {
        return Ok(Variant::BstrNull);
    }
    Ok(Variant::Bstr(crate::VbaError::new(n).description))
}

fn truth(ctx: &RtContext, v: &Variant) -> Result<bool, crate::VbaError> {
    match v {
        Variant::Null => Ok(false),
        Variant::Error(_) => Err(crate::VbaError::new(13)),
        v => match super::util::convert(ctx, v, VarType::Bool)? {
            Variant::Bool(b) => Ok(b),
            _ => Err(crate::VbaError::new(13)),
        },
    }
}

pub(super) fn switch(ctx: &RtContext, a: &[Variant]) -> R {
    if !a.len().is_multiple_of(2) {
        return Err(crate::VbaError::new(5));
    }
    for p in a.chunks(2) {
        if truth(ctx, &p[0])? {
            return Ok(p[1].clone());
        }
    }
    Ok(Variant::Null)
}

pub(super) fn choose(ctx: &RtContext, a: &[Variant]) -> R {
    if matches!(a[0], Variant::Null) {
        return Err(crate::VbaError::new(94));
    }
    let i = match super::util::convert(ctx, &a[0], VarType::R4)? {
        Variant::R4(x) => f64::from(x),
        _ => return Err(crate::VbaError::new(13)),
    };

    let i = i.trunc();
    if i >= 1.0 && (i as usize) < a.len() {
        Ok(a[i as usize].clone())
    } else {
        Ok(Variant::Null)
    }
}

pub(super) fn partition(ctx: &RtContext, a: &[Variant]) -> R {
    if a.iter().any(|v| matches!(v, Variant::Null)) {
        return Ok(Variant::Null);
    }
    let mut n = [0i64; 4];
    for (k, v) in a.iter().enumerate() {
        n[k] = match super::util::convert(ctx, v, VarType::I8)? {
            Variant::I8(x) => x,
            _ => return Err(crate::VbaError::new(13)),
        };
    }
    let [num, start, stop, step] = n;
    if start < 0 || stop <= start || step < 1 {
        return Err(crate::VbaError::new(5));
    }
    let w = (stop + 1).to_string().len();
    let (lo, hi) = if num < start {
        (None, Some(start - 1))
    } else if num > stop {
        (Some(stop + 1), None)
    } else {
        let lo = start + (num - start) / step * step;
        (Some(lo), Some((lo + step - 1).min(stop)))
    };
    let side = |x: Option<i64>| match x {
        Some(v) => format!("{v:>w$}"),
        None => " ".repeat(w),
    };
    Ok(Variant::Bstr(format!("{}:{}", side(lo), side(hi))))
}
