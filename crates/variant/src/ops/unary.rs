use super::arith::int_of;
use super::logic::bits;
use super::{conv, logic, r8, Opd, Unary, K, MISMATCH, OVERFLOW};
use crate::{Decimal, HResult, VarType, Variant};

pub fn unary(op: Unary, v: &Variant) -> Result<Variant, HResult> {
    if op == Unary::Not {
        return logic::not(v);
    }
    let v = match Opd::of(v)? {
        Opd::Null => return Ok(Variant::Null),
        Opd::Num(v) => v,
        Opd::Str(s) => Variant::R8(r8(&Variant::Bstr(s))?),
        Opd::Nothing => return Err(MISMATCH),
    };
    let k = K::of(&v).ok_or(MISMATCH)?;
    match k {
        K::Empty | K::Bool | K::Ui1 | K::I2 | K::I4 | K::I8 => integer(op, &v, k),
        K::R4 => {
            let Variant::R4(x) = v else {
                return Err(MISMATCH);
            };
            let y = float(op, f64::from(x));
            conv(&Variant::R8(y), VarType::R4).or(Ok(Variant::R8(y)))
        }
        K::R8 => {
            let Variant::R8(x) = v else {
                return Err(MISMATCH);
            };
            Ok(Variant::R8(float(op, x)))
        }
        K::Date => {
            let Variant::Date(x) = v else {
                return Err(MISMATCH);
            };
            Ok(if op == Unary::Neg {
                super::arith::date_negated(x)
            } else {
                Variant::Date(float(op, x))
            })
        }
        K::Cy => {
            let Variant::Cy(c) = v else {
                return Err(MISMATCH);
            };
            let z = match op {
                Unary::Neg => c.checked_neg().ok_or(OVERFLOW)?,
                Unary::Abs => c.checked_abs().ok_or(OVERFLOW)?,
                Unary::Fix => c / 10000 * 10000,
                _ => c.div_euclid(10000).checked_mul(10000).ok_or(OVERFLOW)?,
            };
            Ok(Variant::Cy(z))
        }
        K::Dec => {
            let Variant::Decimal(d) = v else {
                return Err(MISMATCH);
            };
            decimal_unary(op, d).map(Variant::Decimal)
        }
    }
}

fn float(op: Unary, x: f64) -> f64 {
    match op {
        Unary::Neg => -x,
        Unary::Abs => x.abs(),
        Unary::Fix => x.trunc(),
        _ => x.floor(),
    }
}

fn integer(op: Unary, v: &Variant, k: K) -> Result<Variant, HResult> {
    let x = match v {
        Variant::Bool(b) => -i128::from(*b),
        v => int_of(v),
    };
    let z = match op {
        Unary::Neg => -x,
        Unary::Abs => x.abs(),
        _ => x,
    };
    let fit = |lo: i128, hi: i128| (lo..=hi).contains(&z);
    Ok(match k {
        K::Ui1 if op != Unary::Neg || z == 0 => Variant::Ui1(z as u8),
        K::Empty | K::Bool | K::Ui1 | K::I2 if fit(-32768, 32767) => Variant::I2(z as i16),
        K::Empty | K::Bool | K::Ui1 | K::I2 | K::I4 if fit(-(1 << 31), (1 << 31) - 1) => {
            Variant::I4(z as i32)
        }
        K::I8 if fit(i128::from(i64::MIN), i128::from(i64::MAX)) => Variant::I8(z as i64),
        _ => Variant::R8(z as f64),
    })
}

fn pow10(n: u32) -> u128 {
    10u128.pow(n)
}

fn decimal_unary(op: Unary, d: Decimal) -> Result<Decimal, HResult> {
    if d.scale > 28 {
        return Err(HResult::E_INVALIDARG);
    }
    Ok(match op {
        Unary::Neg => Decimal {
            negative: !d.negative,
            ..d
        },
        Unary::Abs => Decimal {
            negative: false,
            ..d
        },
        _ => {
            let p = pow10(u32::from(d.scale));
            let (q, r) = (d.mantissa / p, d.mantissa % p);
            let q = if op == Unary::Int && d.negative && r != 0 {
                q + 1
            } else {
                q
            };
            Decimal {
                mantissa: q,
                scale: 0,
                negative: d.negative,
            }
        }
    })
}

pub fn round(v: &Variant, decimals: i32) -> Result<Variant, HResult> {
    if decimals < 0 {
        return Err(HResult::E_INVALIDARG);
    }
    let v = match Opd::of(v)? {
        Opd::Null => return Ok(Variant::Null),
        Opd::Num(v) => v,
        Opd::Str(s) => Variant::R8(r8(&Variant::Bstr(s))?),
        Opd::Nothing => return Err(MISMATCH),
    };
    let k = K::of(&v).ok_or(MISMATCH)?;
    let places = decimals as u32;
    match k {
        K::Empty => Ok(Variant::I2(0)),
        K::Bool => Ok(Variant::I2(bits(&v) as i16)),
        K::Ui1 | K::I2 | K::I4 | K::I8 => Ok(v),
        K::Dec => {
            let Variant::Decimal(d) = v else {
                return Err(MISMATCH);
            };
            Ok(Variant::Decimal(round_decimal(d, places)?))
        }
        K::Cy => {
            let Variant::Cy(c) = v else {
                return Err(MISMATCH);
            };
            let d = Decimal {
                negative: c < 0,
                scale: 4,
                mantissa: u128::from(c.unsigned_abs()),
            };
            let r = round_decimal(d, places)?;
            let m = r.mantissa * pow10(4 - u32::from(r.scale).min(4));
            let z = if r.negative { -(m as i128) } else { m as i128 };
            i64::try_from(z).map(Variant::Cy).map_err(|_| OVERFLOW)
        }
        _ => {
            if places > FLOAT_PLACES {
                return Err(HResult::E_INVALIDARG);
            }
            Ok(match v {
                Variant::R4(x) => Variant::R4(r8_round(f64::from(x), places) as f32),
                Variant::Date(x) => Variant::Date(r8_round(x, places)),
                Variant::R8(x) => Variant::R8(r8_round(x, places)),
                _ => return Err(MISMATCH),
            })
        }
    }
}

fn r8_round(x: f64, places: u32) -> f64 {
    if x.abs() >= 1e16 || x.is_nan() {
        return x;
    }
    let p = POW10[places as usize];
    let y = x * p;
    let whole = y.trunc();
    let fraction = y - whole;
    if fraction.abs() == 0.5 && whole % 2.0 == 0.0 {
        return whole / p;
    }

    (whole + f64::from((fraction + fraction) as i32)) / p
}

const POW10: [f64; 23] = [
    1e0, 1e1, 1e2, 1e3, 1e4, 1e5, 1e6, 1e7, 1e8, 1e9, 1e10, 1e11, 1e12, 1e13, 1e14, 1e15, 1e16,
    1e17, 1e18, 1e19, 1e20, 1e21, 1e22,
];

const FLOAT_PLACES: u32 = 22;

fn round_decimal(d: Decimal, places: u32) -> Result<Decimal, HResult> {
    if d.scale > 28 {
        return Err(HResult::E_INVALIDARG);
    }
    let s = u32::from(d.scale);
    if places >= s {
        return Ok(d);
    }
    let p = pow10(s - places);
    let (q, r) = (d.mantissa / p, d.mantissa % p);
    let up = r * 2 > p || (r * 2 == p && q % 2 == 1);
    Ok(Decimal {
        mantissa: q + u128::from(up),
        scale: places as u8,
        negative: d.negative,
    })
}
