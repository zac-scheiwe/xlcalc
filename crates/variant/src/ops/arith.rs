use super::{conv, decimal, r8, Binary, Opd, DIV_ZERO, INVALID, K, MISMATCH, OVERFLOW};
use crate::ucrt::Errno;
use crate::{HResult, VarType, Variant};

pub(super) fn arith(op: Binary, a: &Variant, b: &Variant) -> Result<Variant, HResult> {
    if op == Binary::Pow && matches!(a, Variant::Error(_) | Variant::Array(_)) {
        return Err(MISMATCH);
    }
    let int_div = matches!(op, Binary::Idiv | Binary::Mod);
    if int_div {
        left_first(a, b)?;
    }
    if op != Binary::Add {
        for v in [a, b] {
            if let Variant::Bstr(s) = v {
                if let Err(e) = string_value(s, int_div) {
                    let nothing =
                        |v: &Variant| matches!(v, Variant::Dispatch(None) | Variant::Unknown(None));
                    if !nothing(a) {
                        return Err(e);
                    }
                }
            }
        }
    }
    let (l, r) = (Opd::of(a)?, Opd::of(b)?);
    let integer_op = matches!(op, Binary::Idiv | Binary::Mod | Binary::Pow);
    match (&l, &r) {
        (Opd::Nothing, Opd::Null) if !integer_op => return Ok(Variant::Null),
        (Opd::Null, Opd::Nothing) if !integer_op => return Ok(Variant::Null),
        (Opd::Nothing, _) | (_, Opd::Nothing) => return Err(MISMATCH),
        _ => {}
    }
    if matches!(l, Opd::Null) || matches!(r, Opd::Null) {
        if op != Binary::Add {
            for o in [&l, &r] {
                match o {
                    Opd::Str(s) => {
                        let v = string_value(s, int_div)?;
                        if int_div && matches!(v, Variant::Bstr(_)) {
                            let wide = matches!(a, Variant::I8(_)) || matches!(b, Variant::I8(_));
                            conv(&v, if wide { VarType::I8 } else { VarType::I4 })?;
                        }
                    }
                    Opd::Num(v) if int_div => {
                        let t = if matches!(v, Variant::I8(_)) {
                            VarType::I8
                        } else {
                            VarType::I4
                        };
                        conv(v, t)?;
                    }
                    _ => {}
                }
            }
        }
        return Ok(Variant::Null);
    }
    if op == Binary::Add {
        match (&l, &r) {
            (Opd::Str(x), Opd::Str(y)) => return Ok(Variant::Bstr(format!("{x}{y}"))),
            (Opd::Str(x), Opd::Num(Variant::Empty)) | (Opd::Num(Variant::Empty), Opd::Str(x)) => {
                return Ok(Variant::Bstr(x.clone()))
            }
            _ => {}
        }
    }
    let num = |o: Opd| match o {
        Opd::Num(v) => Ok(v),
        Opd::Str(s) if int_div => int_string(&s),
        Opd::Str(s) => Ok(Variant::Bstr(s)),
        _ => Err(MISMATCH),
    };
    let (x, y) = (num(l)?, num(r)?);
    let kind = |v: &Variant| match v {
        Variant::Bstr(_) => Some(K::R8),
        v => K::of(v),
    };
    let (kx, ky) = (kind(&x).ok_or(MISMATCH)?, kind(&y).ok_or(MISMATCH)?);
    match op {
        Binary::Add | Binary::Sub | Binary::Mul => additive(op, &x, &y, result_type(op, kx, ky)),
        Binary::Div => divide(&x, &y, kx, ky),
        Binary::Idiv | Binary::Mod => integer_divide(op, &x, &y, kx, ky),
        Binary::Pow => power(&x, &y),
        _ => Err(MISMATCH),
    }
}

fn string_value(s: &str, int_div: bool) -> Result<Variant, HResult> {
    if int_div {
        int_string(s)
    } else {
        r8(&Variant::Bstr(s.to_string())).map(Variant::R8)
    }
}

pub(super) fn left_first(a: &Variant, b: &Variant) -> Result<(), HResult> {
    match a {
        Variant::Error(_) | Variant::Array(_) | Variant::Dispatch(None) => return Err(MISMATCH),
        Variant::Bstr(s) => {
            let v = int_string(s)?;
            if matches!(v, Variant::Bstr(_)) {
                conv(
                    &v,
                    if matches!(b, Variant::I8(_)) {
                        VarType::I8
                    } else {
                        VarType::I4
                    },
                )?;
            }
        }
        v if K::of(v).is_some_and(|k| matches!(k, K::R4 | K::R8 | K::Cy | K::Date | K::Dec)) => {
            conv(
                v,
                if matches!(b, Variant::I8(_)) {
                    VarType::I8
                } else {
                    VarType::I4
                },
            )?;
        }
        _ => {}
    }
    Ok(())
}

pub(super) fn int_string(s: &str) -> Result<Variant, HResult> {
    match super::logic::string_operand(s)? {
        Variant::Bool(b) => Ok(Variant::Bool(b)),
        _ => {
            conv(&Variant::Bstr(s.to_string()), VarType::I8)?;
            Ok(Variant::Bstr(s.to_string()))
        }
    }
}

fn result_type(op: Binary, a: K, b: K) -> VarType {
    let has = |k: K| a == k || b == k;
    if has(K::Dec) {
        return VarType::Decimal;
    }
    if has(K::Date) {
        return match op {
            Binary::Add => VarType::Date,
            Binary::Sub if a == K::Date && b == K::Date => VarType::R8,
            Binary::Sub => VarType::Date,
            _ => VarType::R8,
        };
    }
    if has(K::Cy) {
        let other = if a == K::Cy { b } else { a };
        return if op == Binary::Mul && matches!(other, K::R4 | K::R8) {
            VarType::R8
        } else {
            VarType::Cy
        };
    }
    if has(K::R8) {
        return VarType::R8;
    }
    if has(K::R4) {
        return if has(K::I4) || has(K::I8) {
            VarType::R8
        } else {
            VarType::R4
        };
    }
    if has(K::I8) {
        return VarType::I8;
    }
    if has(K::I4) {
        return VarType::I4;
    }
    let byte = matches!((a, b), (K::Ui1, K::Ui1 | K::Empty) | (K::Empty, K::Ui1));
    if byte {
        VarType::Ui1
    } else {
        VarType::I2
    }
}

pub(super) fn int_of(v: &Variant) -> i128 {
    match v {
        Variant::Ui1(x) => i128::from(*x),
        Variant::I2(x) => i128::from(*x),
        Variant::I4(x) => i128::from(*x),
        Variant::I8(x) => i128::from(*x),
        _ => 0,
    }
}

fn apply_f(op: Binary, x: f64, y: f64) -> f64 {
    match op {
        Binary::Add => x + y,
        Binary::Sub => x - y,
        _ => x * y,
    }
}

fn checked_f(z: f64) -> Result<f64, HResult> {
    if z.is_finite() {
        Ok(z)
    } else {
        Err(OVERFLOW)
    }
}

fn negate(v: Variant) -> Variant {
    match v {
        Variant::R4(x) => Variant::R4(-x),
        Variant::R8(x) => Variant::R8(-x),
        Variant::Date(x) => Variant::Date(-x),
        Variant::Decimal(d) => Variant::Decimal(crate::Decimal {
            negative: !d.negative,
            ..d
        }),
        v => v,
    }
}

fn is_int(v: &Variant) -> bool {
    matches!(
        v,
        Variant::Empty
            | Variant::Bool(_)
            | Variant::Ui1(_)
            | Variant::I2(_)
            | Variant::I4(_)
            | Variant::I8(_)
    )
}

fn cy_result(z: i128) -> Result<Variant, HResult> {
    match i64::try_from(z) {
        Ok(z) => Ok(Variant::Cy(z)),
        _ => Err(OVERFLOW),
    }
}

pub(super) fn date_negated(x: f64) -> Variant {
    if -x > -657435.0 {
        Variant::Date(-x)
    } else {
        Variant::R8(-x)
    }
}

fn date_result(z: f64) -> Variant {
    if z > -657435.0 && z < 2958466.0 {
        Variant::Date(z)
    } else {
        Variant::R8(z)
    }
}

fn additive(op: Binary, x: &Variant, y: &Variant, t: VarType) -> Result<Variant, HResult> {
    if matches!(
        t,
        VarType::R4 | VarType::R8 | VarType::Date | VarType::Decimal
    ) && op != Binary::Mul
    {
        if *y == Variant::Empty {
            return conv(x, t);
        }
        if *x == Variant::Empty {
            let z = conv(y, t)?;
            return Ok(match (op, z) {
                (Binary::Sub, Variant::Date(d)) => date_negated(d),
                (Binary::Sub, z) => negate(z),
                (_, z) => z,
            });
        }
    }
    if op == Binary::Mul && (*x == Variant::Empty || *y == Variant::Empty) {
        match t {
            VarType::R4 => return Ok(Variant::R4(0.0)),
            VarType::R8 => return Ok(Variant::R8(0.0)),
            VarType::Date => return Ok(Variant::Date(0.0)),
            VarType::Decimal => return Ok(Variant::Decimal(crate::Decimal::default())),
            _ => {}
        }
    }
    match t {
        VarType::Ui1 | VarType::I2 | VarType::I4 | VarType::I8 => {
            let (cx, cy) = (conv(x, t)?, conv(y, t)?);
            let (a, b) = (int_of(&cx), int_of(&cy));
            let z = match op {
                Binary::Add => a + b,
                Binary::Sub => a - b,
                _ => a * b,
            };
            let fit = |lo: i128, hi: i128| (lo..=hi).contains(&z);
            Ok(match t {
                VarType::Ui1 if fit(0, 255) => Variant::Ui1(z as u8),
                VarType::Ui1 | VarType::I2 if fit(-32768, 32767) => Variant::I2(z as i16),
                VarType::Ui1 | VarType::I2 | VarType::I4 if fit(-(1 << 31), (1 << 31) - 1) => {
                    Variant::I4(z as i32)
                }
                VarType::I8
                    if fit(i128::from(i64::MIN), i128::from(i64::MAX))
                        || (op == Binary::Mul && (a == -1 || b == -1)) =>
                {
                    Variant::I8(z as i64)
                }
                _ => Variant::R8(checked_f(apply_f(op, a as f64, b as f64))?),
            })
        }
        VarType::R4 => {
            let z = apply_f(op, r8(x)?, r8(y)?);
            match conv(&Variant::R8(z), VarType::R4) {
                Ok(v) => Ok(v),
                Err(_) => Ok(Variant::R8(checked_f(z)?)),
            }
        }
        VarType::R8 => Ok(Variant::R8(checked_f(apply_f(op, r8(x)?, r8(y)?))?)),
        VarType::Date => Ok(date_result(checked_f(apply_f(op, r8(x)?, r8(y)?))?)),
        VarType::Cy if op == Binary::Mul && (is_int(x) || is_int(y)) => {
            let (c, n) = if is_int(y) { (x, y) } else { (y, x) };
            let Variant::Cy(c) = conv(c, t)? else {
                return Err(MISMATCH);
            };
            let n = match n {
                Variant::Bool(b) => -i128::from(*b),
                n => int_of(n),
            };
            let z = i128::from(c) * n;
            if z == i128::from(i64::MIN)
                && (matches!(x, Variant::I8(_)) || matches!(y, Variant::I8(_)))
            {
                return Err(OVERFLOW);
            }
            cy_result(z)
        }
        VarType::Cy => {
            let (Variant::Cy(a), Variant::Cy(b)) = (conv(x, t)?, conv(y, t)?) else {
                return Err(MISMATCH);
            };
            let z = match op {
                Binary::Add => i128::from(a) + i128::from(b),
                Binary::Sub => i128::from(a) - i128::from(b),
                _ => div_round_even(i128::from(a) * i128::from(b), 10000),
            };
            cy_result(z)
        }
        VarType::Decimal => {
            let (Variant::Decimal(a), Variant::Decimal(b)) = (conv(x, t)?, conv(y, t)?) else {
                return Err(MISMATCH);
            };
            let d = match op {
                Binary::Add => decimal::add(a, b),
                Binary::Sub => decimal::sub(a, b),
                _ => decimal::mul(a, b),
            }?;
            Ok(Variant::Decimal(d))
        }
        _ => Err(MISMATCH),
    }
}

pub(super) fn div_round_even(n: i128, d: i128) -> i128 {
    let (q, r) = (n.div_euclid(d), n.rem_euclid(d));
    let twice = r * 2;
    if twice > d || (twice == d && q % 2 != 0) {
        q + 1
    } else {
        q
    }
}

fn is_zero(v: &Variant) -> Result<bool, HResult> {
    Ok(match v {
        Variant::Decimal(d) => {
            if d.scale > 28 {
                return Err(INVALID);
            }
            d.mantissa == 0
        }
        v => r8(v)? == 0.0,
    })
}

fn divide(x: &Variant, y: &Variant, kx: K, ky: K) -> Result<Variant, HResult> {
    if kx == K::Dec || ky == K::Dec {
        let (Variant::Decimal(a), Variant::Decimal(b)) =
            (conv(x, VarType::Decimal)?, conv(y, VarType::Decimal)?)
        else {
            return Err(MISMATCH);
        };
        return decimal::div(a, b).map(Variant::Decimal);
    }
    if is_zero(y)? {
        let float = matches!(kx, K::R4 | K::R8 | K::Date);
        let both = is_zero(x)? && !(ky == K::Empty && float);
        return Err(if both { OVERFLOW } else { DIV_ZERO });
    }
    let single =
        (kx.small() || kx == K::R4) && (ky.small() || ky == K::R4) && (kx == K::R4 || ky == K::R4);
    let z = checked_f(r8(x)? / r8(y)?)?;
    if single {
        return Ok(conv(&Variant::R8(z), VarType::R4).unwrap_or(Variant::R8(z)));
    }
    Ok(Variant::R8(z))
}

fn integer_divide(op: Binary, x: &Variant, y: &Variant, kx: K, ky: K) -> Result<Variant, HResult> {
    let t = if kx == K::I8 || ky == K::I8 {
        VarType::I8
    } else if kx.small() && ky.small() {
        if kx == K::Ui1 && ky == K::Ui1 {
            VarType::Ui1
        } else {
            VarType::I2
        }
    } else {
        VarType::I4
    };
    let (cx, cy) = (conv(x, t)?, conv(y, t)?);
    let (a, b) = (int_of(&cx), int_of(&cy));
    if b == 0 {
        return Err(DIV_ZERO);
    }
    let z = if op == Binary::Idiv { a / b } else { a % b };
    let fit = |lo: i128, hi: i128| (lo..=hi).contains(&z);
    match t {
        VarType::Ui1 => Ok(Variant::Ui1(z as u8)),
        VarType::I2 if fit(-32768, 32767) => Ok(Variant::I2(z as i16)),
        VarType::I2 | VarType::I4 if fit(-(1 << 31), (1 << 31) - 1) => Ok(Variant::I4(z as i32)),
        VarType::I8 if fit(i128::from(i64::MIN), i128::from(i64::MAX)) => Ok(Variant::I8(z as i64)),
        _ => Err(OVERFLOW),
    }
}

fn power(x: &Variant, y: &Variant) -> Result<Variant, HResult> {
    let (a, b) = (r8(x)?, r8(y)?);
    if a == 0.0 {
        if b == 0.0 {
            return Ok(Variant::R8(1.0));
        }
        if b < 0.0 {
            return Err(INVALID);
        }
    } else if a < 0.0 && b.floor() != b {
        return Err(INVALID);
    }
    match crate::ucrt::pow(a, b) {
        (z, Errno::Range) if z != 0.0 => Err(OVERFLOW),
        (_, Errno::Domain) => Err(INVALID),
        (z, _) => Ok(Variant::R8(z)),
    }
}
