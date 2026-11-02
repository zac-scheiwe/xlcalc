mod arith;
mod compare;
mod decimal;
mod logic;
mod unary;

use crate::{convert, HResult, Locale, VarType, Variant};

pub use compare::{compare, NORM_IGNORECASE};
pub use unary::{round, unary};

pub(crate) const OVERFLOW: HResult = HResult::DISP_E_OVERFLOW;
pub(crate) const MISMATCH: HResult = HResult::DISP_E_TYPEMISMATCH;
pub(crate) const DIV_ZERO: HResult = HResult::DISP_E_DIVBYZERO;
pub(crate) const INVALID: HResult = HResult::E_INVALIDARG;

#[derive(Clone, Copy, PartialEq, Eq, Debug)]
pub enum Binary {
    Add,
    Sub,
    Mul,
    Div,
    Idiv,
    Mod,
    Pow,
    Cat,
    And,
    Or,
    Xor,
    Eqv,
    Imp,
}

#[derive(Clone, Copy, PartialEq, Eq, Debug)]
pub enum Unary {
    Neg,
    Not,
    Abs,
    Fix,
    Int,
}

pub fn binary(op: Binary, a: &Variant, b: &Variant) -> Result<Variant, HResult> {
    match op {
        Binary::Cat => cat(a, b, Locale::EN_NZ),
        Binary::And | Binary::Or | Binary::Xor | Binary::Eqv | Binary::Imp => {
            logic::logic(op, a, b)
        }
        _ => arith::arith(op, a, b),
    }
}

pub fn cat(a: &Variant, b: &Variant, locale: Locale) -> Result<Variant, HResult> {
    use crate::flags::ALPHABOOL;
    if matches!((a, b), (Variant::Null, Variant::Null)) {
        return Ok(Variant::Null);
    }
    let text = |v: &Variant| match v {
        Variant::Null => Ok(String::new()),
        v => match convert::change_type(v, VarType::Bstr, locale, ALPHABOOL)? {
            Variant::Bstr(s) => Ok(s),
            _ => Err(MISMATCH),
        },
    };
    let mut s = text(a)?;
    s.push_str(&text(b)?);
    Ok(Variant::Bstr(s))
}

pub(crate) fn conv(v: &Variant, to: VarType) -> Result<Variant, HResult> {
    convert::change_type(v, to, Locale::EN_US, 0)
}

pub(crate) fn r8(v: &Variant) -> Result<f64, HResult> {
    match conv(v, VarType::R8)? {
        Variant::R8(x) => Ok(x),
        _ => Err(MISMATCH),
    }
}

#[derive(Clone, Copy, PartialEq, Eq, Debug)]
pub(crate) enum K {
    Empty,
    Bool,
    Ui1,
    I2,
    I4,
    I8,
    R4,
    R8,
    Cy,
    Date,
    Dec,
}

impl K {
    pub(crate) fn of(v: &Variant) -> Option<K> {
        Some(match v {
            Variant::Empty => K::Empty,
            Variant::Bool(_) => K::Bool,
            Variant::Ui1(_) => K::Ui1,
            Variant::I2(_) => K::I2,
            Variant::I4(_) => K::I4,
            Variant::I8(_) => K::I8,
            Variant::R4(_) => K::R4,
            Variant::R8(_) => K::R8,
            Variant::Cy(_) => K::Cy,
            Variant::Date(_) => K::Date,
            Variant::Decimal(_) => K::Dec,
            _ => return None,
        })
    }

    pub(crate) fn small(self) -> bool {
        matches!(self, K::Empty | K::Bool | K::Ui1 | K::I2)
    }
}

pub(crate) enum Opd {
    Null,

    Nothing,
    Str(String),
    Num(Variant),
}

impl Opd {
    pub(crate) fn of(v: &Variant) -> Result<Opd, HResult> {
        Ok(match v {
            Variant::Null => Opd::Null,
            Variant::Dispatch(None) | Variant::Unknown(None) => Opd::Nothing,
            Variant::Bstr(s) => Opd::Str(s.clone()),
            Variant::BstrNull => Opd::Str(String::new()),
            Variant::Dispatch(Some(_)) | Variant::Unknown(Some(_)) => {
                return Err(HResult::E_NOTIMPL)
            }
            Variant::Error(_) | Variant::Array(_) => return Err(MISMATCH),
            v => Opd::Num(v.clone()),
        })
    }
}

#[cfg(all(test, windows))]
mod tests;
