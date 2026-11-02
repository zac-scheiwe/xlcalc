use super::ops;
use crate::ast::UnOp;
use crate::types::{BinOp, VType};
use crate::VbaError;
use std::cmp::Ordering;
use xlcalc::crt;
use xlcalc::f80::F80;
use xlcalc_variant::{change_type, flags, Locale, VarType, Variant};

#[derive(Clone, Debug)]
pub enum Val {
    V(Variant),
    X(F80),
}

fn err(n: i32) -> VbaError {
    VbaError::new(n)
}

fn store_float(f: F80, ty: &VType) -> Result<Variant, VbaError> {
    match ty {
        VType::Single => f.to_f32().map(Variant::R4).ok_or_else(|| err(6)),
        VType::Date => f.to_f64().map(Variant::Date).ok_or_else(|| err(6)),
        _ => f.to_f64().map(Variant::R8).ok_or_else(|| err(6)),
    }
}

pub fn spill(v: Val, ty: &VType) -> Result<Variant, VbaError> {
    match v {
        Val::V(v) => Ok(v),
        Val::X(f) => store_float(f, ty),
    }
}

pub fn int_value(ty: &VType, n: i128) -> Result<Variant, VbaError> {
    let o = || err(6);
    Ok(match ty {
        VType::Byte => Variant::Ui1(u8::try_from(n).map_err(|_| o())?),
        VType::Integer => Variant::I2(i16::try_from(n).map_err(|_| o())?),
        VType::Long => Variant::I4(i32::try_from(n).map_err(|_| o())?),
        VType::Boolean => Variant::Bool(n != 0),
        _ => Variant::I8(i64::try_from(n).map_err(|_| o())?),
    })
}

fn is_float(t: &VType) -> bool {
    matches!(
        t,
        VType::Single | VType::Double | VType::Date | VType::String
    )
}

fn wrap(ty: &VType, n: i128) -> i128 {
    if *ty == VType::Byte {
        n & 0xFF
    } else {
        n
    }
}

fn div_round(n: i128, d: i128) -> i128 {
    let q = n.div_euclid(d);
    let r = n.rem_euclid(d);
    match (2 * r).cmp(&d) {
        Ordering::Greater => q + 1,
        Ordering::Equal if q & 1 == 1 => q + 1,
        _ => q,
    }
}

pub fn pow(x: f64, y: f64) -> Result<f64, VbaError> {
    if x == 0.0 && y < 0.0 {
        return Err(err(5));
    }
    let r = crt::pow(x, y);
    if r.is_nan() {
        return Err(err(5));
    }
    if r.is_infinite() && x.is_finite() && y.is_finite() {
        return Err(err(6));
    }
    Ok(r)
}

#[derive(Clone, Copy, Debug)]
pub struct Conv {
    pub locale: Locale,
}

impl Conv {
    pub fn change(self, v: &Variant, to: VarType) -> Result<Variant, VbaError> {
        change_type(v, to, self.locale, flags::ALPHABOOL).map_err(ops::hresult_error)
    }

    pub fn let_coerce(self, v: Val, from: &VType, to: &VType) -> Result<Variant, VbaError> {
        let f = match v {
            Val::X(f) => f,
            Val::V(v) => return self.coerce(v, to),
        };
        match to {
            VType::Double | VType::Single | VType::Date => store_float(f, to),
            VType::Byte | VType::Integer | VType::Long | VType::LongLong => {
                int_value(to, i128::from(f.round_to_i64().ok_or_else(|| err(6))?))
            }
            VType::Currency => {
                let c = (f * F80::from_i64(10_000))
                    .round_to_i64()
                    .ok_or_else(|| err(6))?;
                Ok(Variant::Cy(c))
            }
            VType::Boolean => Ok(Variant::Bool(!f.is_zero())),
            _ => self.coerce(store_float(f, from)?, to),
        }
    }

    pub fn coerce(self, v: Variant, to: &VType) -> Result<Variant, VbaError> {
        let Some(vt) = to.var_type() else {
            return Ok(v);
        };
        match &v {
            Variant::Null => Err(err(94)),
            Variant::Error(_) | Variant::Array(_) | Variant::Dispatch(_) | Variant::Unknown(_) => {
                Err(err(13))
            }
            _ if v.vt() == vt as u16 => Ok(v),
            _ => self.change(&v, vt),
        }
    }

    pub fn to_f80(self, v: &Val) -> Result<F80, VbaError> {
        Ok(match v {
            Val::X(f) => *f,
            Val::V(v) => match v {
                Variant::R8(x) | Variant::Date(x) => F80::from_f64(*x),
                Variant::R4(x) => F80::from_f32(*x),
                Variant::Ui1(x) => F80::from_i64(i64::from(*x)),
                Variant::I2(x) => F80::from_i64(i64::from(*x)),
                Variant::I4(x) => F80::from_i64(i64::from(*x)),
                Variant::I8(x) => F80::from_i64(*x),
                Variant::Bool(b) => F80::from_i64(-i64::from(*b)),
                Variant::Cy(c) => F80::from_i64(*c) / F80::from_i64(10_000),
                Variant::Empty => F80::from_i64(0),
                Variant::Null => return Err(err(94)),
                Variant::Bstr(_) | Variant::BstrNull | Variant::Decimal(_) => {
                    match self.change(v, VarType::R8)? {
                        Variant::R8(x) => F80::from_f64(x),
                        _ => return Err(err(13)),
                    }
                }
                _ => return Err(err(13)),
            },
        })
    }

    fn to_f64(self, v: &Val) -> Result<f64, VbaError> {
        self.to_f80(v)?.to_f64().ok_or_else(|| err(6))
    }

    pub fn to_i64(self, v: &Val) -> Result<i64, VbaError> {
        match v {
            Val::V(Variant::Ui1(x)) => Ok(i64::from(*x)),
            Val::V(Variant::I2(x)) => Ok(i64::from(*x)),
            Val::V(Variant::I4(x)) => Ok(i64::from(*x)),
            Val::V(Variant::I8(x)) => Ok(*x),
            Val::V(Variant::Bool(b)) => Ok(-i64::from(*b)),
            Val::V(Variant::Cy(c)) => {
                let (q, r) = (c.div_euclid(10_000), c.rem_euclid(10_000));
                Ok(q + i64::from(r > 5_000 || (r == 5_000 && q & 1 == 1)))
            }
            _ => self.to_f80(v)?.round_to_i64().ok_or_else(|| err(6)),
        }
    }

    fn to_cy(self, v: &Val) -> Result<i128, VbaError> {
        Ok(match v {
            Val::V(Variant::Cy(c)) => i128::from(*c),
            Val::V(
                Variant::Ui1(_)
                | Variant::I2(_)
                | Variant::I4(_)
                | Variant::I8(_)
                | Variant::Bool(_),
            ) => i128::from(self.to_i64(v)?) * 10_000,
            _ => i128::from(
                (self.to_f80(v)? * F80::from_i64(10_000))
                    .round_to_i64()
                    .ok_or_else(|| err(6))?,
            ),
        })
    }

    pub fn to_str(self, v: Val, ty: &VType) -> Result<String, VbaError> {
        match spill(v, ty)? {
            Variant::Bstr(s) => Ok(s),
            Variant::BstrNull => Ok(String::new()),
            Variant::Null => Err(err(94)),
            other => match self.coerce(other, &VType::String)? {
                Variant::Bstr(s) => Ok(s),
                _ => Err(err(13)),
            },
        }
    }

    pub fn truth(self, v: Val) -> Result<bool, VbaError> {
        match v {
            Val::X(f) => Ok(!f.is_zero()),
            Val::V(Variant::Bool(b)) => Ok(b),
            Val::V(other) => match self.coerce(other, &VType::Boolean)? {
                Variant::Bool(b) => Ok(b),
                _ => Err(err(13)),
            },
        }
    }

    pub fn binary(
        &self,
        op: BinOp,
        res: &VType,
        (a, at): (Val, &VType),
        (b, bt): (Val, &VType),
        text: bool,
    ) -> Result<Val, VbaError> {
        use BinOp::*;
        match op {
            Eq | Ne | Lt | Le | Gt | Ge => {
                let o = self.order((a, at), (b, bt), text)?;
                let r = match op {
                    Eq => o == Ordering::Equal,
                    Ne => o != Ordering::Equal,
                    Lt => o == Ordering::Less,
                    Le => o != Ordering::Greater,
                    Gt => o == Ordering::Greater,
                    _ => o != Ordering::Less,
                };
                return Ok(Val::V(Variant::Bool(r)));
            }
            Like => {
                let s = self.to_str(a, at)?;
                let p = self.to_str(b, bt)?;
                return Ok(Val::V(Variant::Bool(ops::like(&s, &p, text))));
            }
            Cat => {
                let s = self.to_str(a, at)? + &self.to_str(b, bt)?;
                return Ok(Val::V(Variant::Bstr(s)));
            }
            Pow => {
                return pow(self.to_f64(&a)?, self.to_f64(&b)?).map(|r| Val::X(F80::from_f64(r)))
            }
            _ => {}
        }
        match res {
            VType::String => {
                let s = self.to_str(a, at)? + &self.to_str(b, bt)?;
                Ok(Val::V(Variant::Bstr(s)))
            }
            VType::Single | VType::Double | VType::Date => {
                let (x, y) = (self.to_f80(&a)?, self.to_f80(&b)?);
                let r = match op {
                    Add => x + y,
                    Sub => x - y,
                    Mul => x * y,
                    _ => {
                        if y.is_zero() {
                            return Err(err(if x.is_zero() { 6 } else { 11 }));
                        }
                        x / y
                    }
                };
                if r.is_nan() || r.is_infinite() {
                    return Err(err(6));
                }
                Ok(Val::X(r))
            }
            VType::Currency => {
                let (x, y) = (self.to_cy(&a)?, self.to_cy(&b)?);
                let n = match op {
                    Add => x + y,
                    Sub => x - y,
                    _ => div_round(x * y, 10_000),
                };
                Ok(Val::V(Variant::Cy(i64::try_from(n).map_err(|_| err(6))?)))
            }
            _ => {
                let x = self.int_operand(&a, res)?;
                let y = self.int_operand(&b, res)?;
                let n = match op {
                    Add => x + y,
                    Sub => x - y,
                    Mul => x * y,
                    IntDiv | Mod => {
                        if y == 0 {
                            return Err(err(11));
                        }
                        if op == IntDiv {
                            x / y
                        } else {
                            x % y
                        }
                    }
                    And => x & y,
                    Or => x | y,
                    Xor => x ^ y,
                    Eqv => wrap(res, !(x ^ y)),
                    Imp => wrap(res, !x | y),
                    _ => unreachable!("{op:?} is not arithmetic"),
                };
                int_value(res, n).map(Val::V)
            }
        }
    }

    fn int_operand(self, v: &Val, res: &VType) -> Result<i128, VbaError> {
        let n = i128::from(self.to_i64(v)?);
        int_value(res, n)?;
        Ok(n)
    }

    fn order(
        &self,
        (a, at): (Val, &VType),
        (b, bt): (Val, &VType),
        text: bool,
    ) -> Result<Ordering, VbaError> {
        if *at == VType::String && *bt == VType::String {
            let (x, y) = (self.to_str(a, at)?, self.to_str(b, bt)?);
            let o = ops::var_cmp(&Variant::Bstr(x), &Variant::Bstr(y), text, self.locale)?;
            return o.ok_or_else(|| err(94));
        }
        if !is_float(at) && !is_float(bt) {
            return Ok(self.to_cy(&a)?.cmp(&self.to_cy(&b)?));
        }
        let (x, y) = (self.to_f80(&a)?, self.to_f80(&b)?);
        x.compare(y).ok_or_else(|| err(6))
    }

    pub fn unary(self, op: UnOp, res: &VType, a: Val) -> Result<Val, VbaError> {
        match (op, res) {
            (UnOp::Neg, VType::Single | VType::Double | VType::Date) => {
                Ok(Val::X(-self.to_f80(&a)?))
            }
            (UnOp::Neg, VType::Currency) => {
                let c = -self.to_cy(&a)?;
                Ok(Val::V(Variant::Cy(i64::try_from(c).map_err(|_| err(6))?)))
            }
            (UnOp::Neg, _) => int_value(res, -i128::from(self.to_i64(&a)?)).map(Val::V),
            (UnOp::Not, VType::Boolean) => match a {
                Val::V(Variant::Bool(b)) => Ok(Val::V(Variant::Bool(!b))),
                other => Ok(Val::V(Variant::Bool(self.to_i64(&other)? == 0))),
            },
            (UnOp::Not, _) => {
                let n = i128::from(self.to_i64(&a)?);
                int_value(res, n)?;
                int_value(res, wrap(res, !n)).map(Val::V)
            }
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    const C: Conv = Conv {
        locale: Locale::EN_US,
    };

    fn d(x: f64) -> (Val, &'static VType) {
        (Val::V(Variant::R8(x)), &VType::Double)
    }

    #[test]
    fn extended_intermediates() {
        let e = 2f64.powi(-60);
        let s = C
            .binary(BinOp::Add, &VType::Double, d(1.0), d(e), false)
            .unwrap();
        let r = C
            .binary(
                BinOp::Sub,
                &VType::Double,
                (s, &VType::Double),
                d(1.0),
                false,
            )
            .unwrap();
        assert_eq!(spill(r, &VType::Double), Ok(Variant::R8(e)));

        let big = C
            .binary(BinOp::Mul, &VType::Double, d(1e308), d(10.0), false)
            .unwrap();
        assert_eq!(spill(big.clone(), &VType::Double), Err(VbaError::new(6)));
        let back = C
            .binary(
                BinOp::Div,
                &VType::Double,
                (big, &VType::Double),
                d(10.0),
                false,
            )
            .unwrap();
        assert_eq!(spill(back, &VType::Double), Ok(Variant::R8(1e308)));
    }

    #[test]
    fn integer_overflow_and_division() {
        let i = |n: i16| (Val::V(Variant::I2(n)), &VType::Integer);
        assert_eq!(
            C.binary(BinOp::Add, &VType::Integer, i(32767), i(1), false)
                .unwrap_err(),
            VbaError::new(6)
        );
        assert_eq!(
            C.binary(BinOp::IntDiv, &VType::Integer, i(7), i(0), false)
                .unwrap_err(),
            VbaError::new(11)
        );
        assert_eq!(
            spill(
                C.binary(BinOp::Mod, &VType::Integer, i(-7), i(3), false)
                    .unwrap(),
                &VType::Integer
            ),
            Ok(Variant::I2(-1))
        );
        assert_eq!(
            C.binary(BinOp::Div, &VType::Double, d(0.0), d(0.0), false)
                .unwrap_err(),
            VbaError::new(6)
        );
        assert_eq!(
            C.binary(BinOp::Div, &VType::Double, d(1.0), d(0.0), false)
                .unwrap_err(),
            VbaError::new(11)
        );
    }

    #[test]
    fn stores_round_from_extended() {
        let v = |x: f64| Val::X(F80::from_f64(x));
        assert_eq!(
            C.let_coerce(v(2.5), &VType::Double, &VType::Long),
            Ok(Variant::I4(2))
        );
        assert_eq!(
            C.let_coerce(v(3.5), &VType::Double, &VType::Long),
            Ok(Variant::I4(4))
        );
        assert_eq!(
            C.let_coerce(v(40000.0), &VType::Double, &VType::Integer),
            Err(VbaError::new(6))
        );
        assert_eq!(
            C.let_coerce(v(1.23456789), &VType::Double, &VType::Currency),
            Ok(Variant::Cy(12346))
        );
    }

    #[test]
    fn logical_operators_are_bitwise() {
        let l = |n: i32| (Val::V(Variant::I4(n)), &VType::Long);
        let r = C
            .binary(BinOp::And, &VType::Long, l(12), l(10), false)
            .unwrap();
        assert_eq!(spill(r, &VType::Long), Ok(Variant::I4(8)));
        let r = C
            .binary(BinOp::Eqv, &VType::Long, l(0), l(0), false)
            .unwrap();
        assert_eq!(spill(r, &VType::Long), Ok(Variant::I4(-1)));
        let r = C
            .unary(UnOp::Not, &VType::Long, Val::V(Variant::I4(0)))
            .unwrap();
        assert_eq!(spill(r, &VType::Long), Ok(Variant::I4(-1)));
    }

    #[test]
    fn power_errors() {
        assert_eq!(pow(0.0, -1.0), Err(VbaError::new(5)));
        assert_eq!(pow(-8.0, 0.5), Err(VbaError::new(5)));
        assert_eq!(pow(10.0, 400.0), Err(VbaError::new(6)));
        assert_eq!(pow(2.0, 0.5), Ok(std::f64::consts::SQRT_2));
    }
}
