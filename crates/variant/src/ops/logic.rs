use super::arith::int_of;
use super::{conv, Binary, Opd, K, MISMATCH};
use crate::{HResult, VarType, Variant};

pub(super) fn string_operand(s: &str) -> Result<Variant, HResult> {
    let t = s.to_ascii_lowercase();
    Ok(match t.as_str() {
        "true" | "#true#" => Variant::Bool(true),
        "false" | "#false#" => Variant::Bool(false),
        _ => Variant::R8(super::r8(&Variant::Bstr(s.to_string()))?),
    })
}

pub(super) fn bits(v: &Variant) -> i128 {
    match v {
        Variant::Bool(b) => -i128::from(*b),
        v => int_of(v),
    }
}

pub(super) fn make(t: VarType, x: i128) -> Variant {
    match t {
        VarType::Bool => Variant::Bool(x != 0),
        VarType::Ui1 => Variant::Ui1(x as u8),
        VarType::I2 => Variant::I2(x as i16),
        VarType::I4 => Variant::I4(x as i32),
        _ => Variant::I8(x as i64),
    }
}

pub(super) fn own_type(k: K) -> VarType {
    match k {
        K::Bool => VarType::Bool,
        K::Ui1 => VarType::Ui1,
        K::Empty | K::I2 => VarType::I2,
        K::I8 => VarType::I8,
        _ => VarType::I4,
    }
}

fn pair_type(a: K, b: K) -> VarType {
    if a == K::I8 || b == K::I8 {
        VarType::I8
    } else if a == K::Bool && b == K::Bool {
        VarType::Bool
    } else if a == K::Ui1 && b == K::Ui1 {
        VarType::Ui1
    } else if a.small() && b.small() {
        VarType::I2
    } else {
        VarType::I4
    }
}

fn apply(op: Binary, a: i128, b: i128) -> i128 {
    match op {
        Binary::And => a & b,
        Binary::Or => a | b,
        Binary::Xor => a ^ b,
        Binary::Eqv => !(a ^ b),
        _ => !a | b,
    }
}

fn kind(v: &Variant) -> Result<K, HResult> {
    match v {
        Variant::Bstr(_) => Ok(K::R8),
        v => K::of(v).ok_or(MISMATCH),
    }
}

fn num(o: Opd) -> Result<Variant, HResult> {
    match o {
        Opd::Num(v) => Ok(v),
        Opd::Str(s) => super::arith::int_string(&s),
        _ => Err(MISMATCH),
    }
}

fn value(v: &Variant, t: VarType) -> Result<i128, HResult> {
    Ok(bits(&conv(v, t)?))
}

pub(super) fn logic(op: Binary, a: &Variant, b: &Variant) -> Result<Variant, HResult> {
    super::arith::left_first(a, b)?;
    let (l, r) = (Opd::of(a)?, Opd::of(b)?);
    if matches!(l, Opd::Nothing) || matches!(r, Opd::Nothing) {
        return Err(MISMATCH);
    }
    match (matches!(l, Opd::Null), matches!(r, Opd::Null)) {
        (true, true) => Ok(Variant::Null),
        (false, false) => {
            let (x, y) = (num(l)?, num(r)?);
            let (kx, ky) = (kind(&x)?, kind(&y)?);
            let t = pair_type(kx, ky);

            let left = if op == Binary::Imp && kx != K::I8 {
                value(&x, own_type(kx))?
            } else {
                value(&x, t)?
            };
            Ok(make(t, apply(op, left, value(&y, t)?)))
        }
        (null_left, _) => {
            let v = num(if null_left { r } else { l })?;
            let k = kind(&v)?;
            let t = own_type(k);
            let x = value(&v, t)?;
            let (hit, out) = match op {
                Binary::And => (x == 0, x),
                Binary::Or => (x != 0, x),
                Binary::Imp if null_left => (x != 0, x),
                Binary::Imp => (!x != 0 && narrow(t, !x) != 0, narrow(t, !x)),
                _ => (false, 0),
            };
            Ok(if hit { make(t, out) } else { Variant::Null })
        }
    }
}

fn narrow(t: VarType, x: i128) -> i128 {
    match t {
        VarType::Ui1 => x & 0xFF,
        VarType::I2 => i128::from(x as i16),
        VarType::I4 => i128::from(x as i32),
        _ => x,
    }
}

pub(super) fn not(v: &Variant) -> Result<Variant, HResult> {
    let v = match Opd::of(v)? {
        Opd::Null => return Ok(Variant::Null),
        o => num(o)?,
    };
    let t = own_type(kind(&v)?);
    Ok(make(t, !value(&v, t)?))
}
