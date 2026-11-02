mod date;
mod number;
mod parse;
mod real;
#[cfg(all(test, windows))]
mod tests;

use crate::{flags, HResult, Locale, VarType, Variant};

fn supported(locale: Locale) -> Result<(), HResult> {
    if locale == Locale::EN_US || locale == Locale::EN_NZ {
        Ok(())
    } else {
        Err(HResult::E_NOTIMPL)
    }
}

pub(crate) fn to_bstr(src: &Variant, locale: Locale, flags: u16) -> Result<String, HResult> {
    supported(locale)?;
    Ok(match src {
        Variant::Empty => String::new(),
        Variant::I2(v) => v.to_string(),
        Variant::I4(v) => v.to_string(),
        Variant::I8(v) => v.to_string(),
        Variant::Ui1(v) => v.to_string(),
        Variant::Bool(b) if flags & (flags::ALPHABOOL | flags::LOCALBOOL) != 0 => {
            if *b { "True" } else { "False" }.to_string()
        }
        Variant::Bool(b) => if *b { "-1" } else { "0" }.to_string(),
        Variant::R8(x) => real::real_text(*x, real::R8_DIGITS),
        Variant::R4(x) => real::real_text(widen(*x), real::R4_DIGITS),
        Variant::Cy(v) => scaled(*v < 0, u128::from(v.unsigned_abs()), 4),
        Variant::Decimal(d) => scaled(d.negative, d.mantissa, u32::from(d.scale)),
        Variant::Bstr(s) => s.clone(),
        Variant::Date(x) => date::date_text(*x, locale)?,
        Variant::BstrNull => String::new(),
        Variant::Null
        | Variant::Error(_)
        | Variant::Dispatch(_)
        | Variant::Unknown(_)
        | Variant::Array(_) => return Err(HResult::DISP_E_TYPEMISMATCH),
    })
}

fn widen(x: f32) -> f64 {
    if x.is_nan() {
        let b = x.to_bits();
        let sign = u64::from(b >> 31) << 63;
        let payload = u64::from(b & 0x7F_FFFF | 0x40_0000) << 29;
        f64::from_bits(sign | 0x7FF0_0000_0000_0000 | payload)
    } else {
        f64::from(x)
    }
}

fn scaled(negative: bool, mantissa: u128, scale: u32) -> String {
    if mantissa == 0 {
        return "0".to_string();
    }
    let scale = scale as usize;
    let digits = format!("{mantissa:0>width$}", width = scale + 1);
    let (int, frac) = digits.split_at(digits.len() - scale);
    let frac = frac.trim_end_matches('0');
    let sign = if negative { "-" } else { "" };
    if frac.is_empty() {
        format!("{sign}{int}")
    } else {
        format!("{sign}{int}.{frac}")
    }
}

pub(crate) fn from_bstr(s: &str, to: VarType, locale: Locale) -> Result<Variant, HResult> {
    supported(locale)?;
    let parsed = |max| parse::parse(s, max);
    Ok(match to {
        VarType::Empty => Variant::Empty,
        VarType::Null => Variant::Null,
        VarType::Bstr => Variant::Bstr(s.to_string()),
        VarType::I2 => Variant::I2(number::i2(&parsed(12)?)?),
        VarType::I4 => Variant::I4(number::i4(&parsed(11)?)?),
        VarType::I8 => Variant::I8(number::i8(&parsed(21)?)?),
        VarType::Ui1 => Variant::Ui1(number::ui1(&parsed(12)?)?),
        VarType::R4 => Variant::R4(number::r4(&parsed(12)?)?),
        VarType::R8 => Variant::R8(number::r8(&parsed(20)?)?),
        VarType::Cy => Variant::Cy(number::cy(&parsed(21)?)?),
        VarType::Decimal => Variant::Decimal(number::decimal(&parsed(30)?)?),
        VarType::Bool => Variant::Bool(bool_from(s)?),
        VarType::Date => return Err(HResult::E_NOTIMPL),
        VarType::Dispatch | VarType::Unknown | VarType::Error | VarType::Variant => {
            return Err(HResult::DISP_E_TYPEMISMATCH)
        }
    })
}

fn bool_from(s: &str) -> Result<bool, HResult> {
    let s = s.split('\0').next().unwrap_or_default();
    if s == "#FALSE#" || s.eq_ignore_ascii_case("False") {
        Ok(false)
    } else if s == "#TRUE#" || s.eq_ignore_ascii_case("True") {
        Ok(true)
    } else {
        Ok(number::r8(&parse::parse(s, 20)?)? != 0.0)
    }
}
