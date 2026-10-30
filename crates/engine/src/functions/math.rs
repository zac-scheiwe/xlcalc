use super::args::*;
use super::engineering::atp;
use super::{args, scalar, volatile, FnDef, MANY};
use crate::eval::{EvalCtx, Operand};
use crate::number;
use crate::value::{Value, XlError};

pub const DEFS: &[FnDef] = &[
    args("SUM", 1, MANY, "R", sum),
    args("PRODUCT", 1, MANY, "R", product),
    args("SUMSQ", 1, MANY, "R", sumsq),
    args("SUMPRODUCT", 1, MANY, "R", sumproduct),
    args("GCD", 1, MANY, "R", gcd),
    args("LCM", 1, MANY, "R", lcm),
    scalar("ABS", 1, 1, |c, a| m1(c, a, f64::abs)),
    scalar("SIGN", 1, 1, |c, a| {
        m1(c, a, |x| {
            if x > 0.0 {
                1.0
            } else if x < 0.0 {
                -1.0
            } else {
                0.0
            }
        })
    }),
    scalar("INT", 1, 1, |c, a| m1(c, a, int_fn)),
    scalar("TRUNC", 1, 2, trunc_fn),
    scalar("ROUND", 2, 2, |c, a| round_fn(c, a, RoundMode::HalfAway)),
    scalar("ROUNDUP", 2, 2, |c, a| round_fn(c, a, RoundMode::Up)),
    scalar("ROUNDDOWN", 2, 2, |c, a| round_fn(c, a, RoundMode::Down)),
    args("MROUND", 2, 2, "R", mround),
    scalar("CEILING", 2, 2, ceiling),
    scalar("FLOOR", 2, 2, floor),
    args("CEILING.MATH", 1, 3, "VVV", |c, o| {
        with_default_sig(c, o, ceiling_math)
    }),
    args("FLOOR.MATH", 1, 3, "VVV", |c, o| {
        with_default_sig(c, o, floor_math)
    }),
    args("CEILING.PRECISE", 1, 2, "VV", |c, o| {
        with_default_sig(c, o, |c, a| precise(c, a, true))
    }),
    args("FLOOR.PRECISE", 1, 2, "VV", |c, o| {
        with_default_sig(c, o, |c, a| precise(c, a, false))
    }),
    args("ISO.CEILING", 1, 2, "VV", |c, o| {
        with_default_sig(c, o, |c, a| precise(c, a, true))
    }),
    scalar("EVEN", 1, 1, |c, a| m1(c, a, |x| even_odd(x, true))),
    scalar("ODD", 1, 1, |c, a| m1(c, a, |x| even_odd(x, false))),
    scalar("MOD", 2, 2, modulo),
    args("QUOTIENT", 2, 2, "R", quotient),
    scalar("POWER", 2, 2, |c, a| {
        val((|| {
            Ok(crate::eval::ops::power(num(c, &a[0])?, num(c, &a[1])?))
        })())
    }),
    scalar("SQRT", 1, 1, |c, a| {
        m1r(c, a, |x| {
            if x < 0.0 {
                Err(XlError::NUM)
            } else {
                Ok(crate::x87::sqrt(x))
            }
        })
    }),
    args("SQRTPI", 1, 1, "R", sqrtpi),
    scalar("EXP", 1, 1, |c, a| m1(c, a, crate::x87::exp)),
    scalar("LN", 1, 1, |c, a| {
        m1r(c, a, |x| {
            if x <= 0.0 {
                Err(XlError::NUM)
            } else {
                Ok(crate::x87::ln(x))
            }
        })
    }),
    scalar("LOG10", 1, 1, |c, a| {
        m1r(c, a, |x| {
            if x <= 0.0 {
                Err(XlError::NUM)
            } else {
                Ok(crate::x87::log10(x))
            }
        })
    }),
    scalar("LOG", 1, 2, log),
    scalar("PI", 0, 0, |_, _| Value::Number(std::f64::consts::PI)),
    scalar("FACT", 1, 1, fact),
    args("FACTDOUBLE", 1, 1, "R", fact_double),
    scalar("COMBIN", 2, 2, combin),
    scalar("PERMUT", 2, 2, permut),
    scalar("SIN", 1, 1, |c, a| trig(c, a, crate::x87::sin)),
    scalar("COS", 1, 1, |c, a| trig(c, a, crate::x87::cos)),
    scalar("TAN", 1, 1, |c, a| trig(c, a, crate::x87::tan)),
    scalar("ASIN", 1, 1, |c, a| {
        m1r(c, a, |x| {
            if x.abs() > 1.0 {
                Err(XlError::NUM)
            } else {
                Ok(crate::xmath::asin(x))
            }
        })
    }),
    scalar("ACOS", 1, 1, |c, a| {
        m1r(c, a, |x| {
            if x.abs() > 1.0 {
                Err(XlError::NUM)
            } else {
                Ok(crate::xmath::acos(x))
            }
        })
    }),
    scalar("ATAN", 1, 1, |c, a| m1(c, a, crate::xmath::atan)),
    scalar("ATAN2", 2, 2, atan2),
    scalar("SINH", 1, 1, |c, a| m1(c, a, crate::xmath::sinh)),
    scalar("COSH", 1, 1, |c, a| m1(c, a, crate::xmath::cosh)),
    scalar("TANH", 1, 1, |c, a| m1(c, a, crate::xmath::tanh)),
    scalar("ASINH", 1, 1, |c, a| m1(c, a, crate::xmath::asinh)),
    scalar("ACOSH", 1, 1, |c, a| {
        m1r(c, a, |x| {
            if x < 1.0 {
                Err(XlError::NUM)
            } else {
                Ok(crate::xmath::acosh(x))
            }
        })
    }),
    scalar("ATANH", 1, 1, |c, a| {
        m1r(c, a, |x| {
            if x.abs() >= 1.0 {
                Err(XlError::NUM)
            } else {
                Ok(crate::xmath::atanh(x))
            }
        })
    }),
    scalar("DEGREES", 1, 1, |c, a| m1(c, a, f64::to_degrees)),
    scalar("RADIANS", 1, 1, |c, a| m1(c, a, f64::to_radians)),
    volatile(scalar("RAND", 0, 0, |c, _| Value::Number(c.wb.random()))),
    volatile(args("RANDBETWEEN", 2, 2, "R", randbetween)),
];

fn int_fn(x: f64) -> f64 {
    if x.abs() < 1e15 {
        crate::number::round15(x).floor()
    } else {
        x.floor()
    }
}

fn m1(ctx: &mut EvalCtx, a: &[Value], f: impl Fn(f64) -> f64) -> Value {
    match num(ctx, &a[0]) {
        Ok(x) => Value::num(f(x)),
        Err(e) => err(e),
    }
}

fn m1r(ctx: &mut EvalCtx, a: &[Value], f: impl Fn(f64) -> R<f64>) -> Value {
    val(num(ctx, &a[0]).and_then(f).map(Value::num))
}

fn trig(ctx: &mut EvalCtx, a: &[Value], f: impl Fn(f64) -> f64) -> Value {
    m1r(ctx, a, |x| {
        if x.abs() >= 134_217_728.0 {
            Err(XlError::NUM)
        } else {
            Ok(f(x))
        }
    })
}

fn sum(ctx: &mut EvalCtx, ops: &[Operand]) -> Operand {
    let (mut acc, mut last) = (0.0f64, (0.0f64, 0.0f64));
    let r = visit_numbers(ctx, ops, SUM_POLICY, |x| {
        last = (acc, x);
        acc = crate::x87::add(acc, x);
    });
    let acc = number::snap_add(last.0, last.1, acc);
    opval(r.map(|_| Value::num(acc)))
}

fn product(ctx: &mut EvalCtx, ops: &[Operand]) -> Operand {
    let mut acc = 1.0f64;
    let mut any = false;

    let ops: Vec<Operand> = ops
        .iter()
        .filter(|o| !matches!(o, Operand::Missing))
        .cloned()
        .collect();
    let r = visit_numbers(ctx, &ops, SUM_POLICY, |x| {
        acc = crate::x87::mul(acc, x);
        any = true;
    });
    opval(r.map(|_| Value::num(if any { acc } else { 0.0 })))
}

fn sumsq(ctx: &mut EvalCtx, ops: &[Operand]) -> Operand {
    let mut acc = 0.0f64;

    let r = visit_numbers(ctx, ops, SUM_POLICY, |x| {
        acc = crate::x87::add(acc, crate::x87::mul(x, x))
    });
    opval(r.map(|_| Value::num(acc)))
}

fn sumproduct(ctx: &mut EvalCtx, ops: &[Operand]) -> Operand {
    let mut grids = Vec::with_capacity(ops.len());
    for op in ops {
        if let Operand::Ref(crate::eval::Reference::Grid(g)) = op {
            if let Some(e) = g.first_error() {
                return Operand::err(e);
            }
            grids.push(std::sync::Arc::new(crate::value::Array::new(
                g.rows,
                g.cols,
                vec![Value::Blank; g.items.len()],
            )));
            continue;
        }
        if let Err(e) = check_array_args(ctx, &[op]) {
            return Operand::err(e);
        }
        match Grid::from_operand_3d(op) {
            Ok(g) => grids.push(g.to_array(ctx)),
            Err(e) => return Operand::err(e),
        }
    }
    let (rows, cols) = (grids[0].rows, grids[0].cols);
    if grids.iter().any(|g| g.rows != rows || g.cols != cols) {
        for g in &grids {
            for v in g.data.iter() {
                if let Value::Error(e) = v {
                    return Operand::err(*e);
                }
            }
        }
        return Operand::err(XlError::VALUE);
    }

    for g in &grids {
        for v in g.data.iter() {
            if let Value::Error(e) = v {
                return Operand::err(*e);
            }
        }
    }
    let mut acc = 0.0;
    for i in 0..grids[0].len() {
        let mut p = 1.0;
        for g in &grids {
            match &g.data[i] {
                Value::Number(n) => p = crate::x87::mul(p, *n),
                Value::Error(e) => return Operand::err(*e),
                _ => p = 0.0,
            }
        }
        acc = crate::x87::add(acc, p);
    }
    Operand::Val(Value::num(acc))
}

fn integers(ctx: &mut EvalCtx, ops: &[Operand]) -> R<Vec<f64>> {
    let mut vals: Vec<Value> = Vec::new();
    let mut skipped = false;
    for (i, op) in ops.iter().enumerate() {
        match op {
            Operand::Missing if i == 0 => return Err(XlError::NA),
            Operand::Missing => skipped = true,
            Operand::Val(Value::Array(a)) => vals.extend(a.data.iter().cloned()),
            Operand::Val(v) => vals.push(v.clone()),
            Operand::Ref(r) => {
                for area in r.areas() {
                    match ctx.area_array(area) {
                        Value::Array(a) => vals.extend(a.data.iter().cloned()),
                        Value::Blank => skipped = true,
                        v => vals.push(v),
                    }
                }
            }
        }
    }
    if vals.is_empty() && skipped {
        return Err(XlError::VALUE);
    }
    let mut nums = Vec::with_capacity(vals.len());
    for v in &vals {
        nums.push(match v {
            Value::Number(n) => *n,
            Value::Blank => 0.0,
            Value::Bool(_) => return Err(XlError::VALUE),
            Value::Error(e) => return Err(*e),
            v => ctx.to_number(v)?,
        });
    }
    nums.into_iter()
        .map(|n| {
            let t = n.trunc();
            if n < 0.0 || t > 9.007_199_254_740_992e15 {
                Err(XlError::NUM)
            } else {
                Ok(t)
            }
        })
        .collect()
}

fn gcd2(mut a: f64, mut b: f64) -> f64 {
    while b != 0.0 {
        let t = a % b;
        a = b;
        b = t;
    }
    a
}

fn gcd(ctx: &mut EvalCtx, ops: &[Operand]) -> Operand {
    opval(integers(ctx, ops).map(|v| Value::num(v.into_iter().fold(0.0, gcd2))))
}

fn lcm(ctx: &mut EvalCtx, ops: &[Operand]) -> Operand {
    opval(integers(ctx, ops).and_then(|v| {
        let mut acc = 1.0;
        for n in v {
            if n == 0.0 {
                return Ok(Value::Number(0.0));
            }
            acc = acc / gcd2(acc, n) * n;
            if acc > 9.007_199_254_740_992e15 {
                return Err(XlError::NUM);
            }
        }
        Ok(Value::num(acc))
    }))
}

#[derive(Clone, Copy, PartialEq, Eq)]
pub enum RoundMode {
    HalfAway,
    Up,
    Down,
}

pub fn round_decimal(x: f64, digits: i32, mode: RoundMode) -> f64 {
    if x == 0.0 || !x.is_finite() {
        return x;
    }
    let s = format!("{:.14e}", x.abs());
    let (mant, exp) = s.split_once('e').unwrap();
    let exp: i32 = exp.parse().unwrap();
    let ds: Vec<u8> = mant
        .bytes()
        .filter(u8::is_ascii_digit)
        .map(|b| b - b'0')
        .collect();

    let keep = exp + 1 + digits;
    if keep >= ds.len() as i32 {
        return number::round15(x);
    }
    let sign = x.signum();
    if keep < 0 {
        return match mode {
            RoundMode::Up => sign * 10f64.powi(-digits),
            _ => 0.0,
        };
    }
    let keep = keep as usize;
    let rest = &ds[keep..];
    let bump = match mode {
        RoundMode::HalfAway => rest[0] >= 5,
        RoundMode::Up => rest.iter().any(|&d| d != 0),
        RoundMode::Down => false,
    };
    let mut kept: Vec<u8> = ds[..keep].to_vec();
    let last_weight = exp - keep as i32 + 1;
    let parse = |digits: &[u8], w: i32| -> f64 {
        if digits.is_empty() {
            return 0.0;
        }
        let t: String = digits.iter().map(|d| (d + b'0') as char).collect();
        format!("{}e{}", t, w).parse().unwrap_or(0.0)
    };
    if bump && mode == RoundMode::Up {
        let v = parse(&kept, last_weight) + parse(&[1], last_weight);
        return number::normalize(sign * v);
    }
    if bump {
        let mut i = kept.len();
        loop {
            if i == 0 {
                kept.insert(0, 1);
                break;
            }
            i -= 1;
            if kept[i] == 9 {
                kept[i] = 0;
            } else {
                kept[i] += 1;
                break;
            }
        }
    }
    if kept.is_empty() {
        return 0.0;
    }

    let v = parse(&kept, last_weight);
    number::normalize(sign * v)
}

fn round_fn(ctx: &mut EvalCtx, a: &[Value], mode: RoundMode) -> Value {
    val((|| {
        let x = num(ctx, &a[0])?;
        let d = num(ctx, &a[1])?;
        let d = if matches!(mode, RoundMode::HalfAway) {
            let d = ifuzz(d);

            if d >= 2147483647.0 {
                return Ok(Value::num(0.0));
            }
            d.clamp(-400.0, 400.0) as i32
        } else {
            let n = trunc_digits(d);

            let lo = (if d.abs() < 4294967296.0 {
                d.abs() as u32
            } else {
                u32::MAX
            }) as u16 as i16;
            if matches!(mode, RoundMode::Up) && d < 0.0 && lo < 0 {
                -400
            } else {
                n
            }
        };
        Ok(Value::num(round_decimal(x, d, mode)))
    })())
}

fn trunc_digits(d: f64) -> i32 {
    let a = d.abs().trunc();
    let u = if a < 4294967296.0 { a as u32 } else { u32::MAX };
    let lo = u as u16 as i16;
    let n = if lo < 0 { 100 } else { lo as i32 };
    if d > 0.0 {
        n
    } else {
        -n
    }
}

fn trunc_fn(ctx: &mut EvalCtx, a: &[Value]) -> Value {
    val((|| {
        let x = num(ctx, &a[0])?;
        let d = trunc_digits(opt_num(ctx, a, 1, 0.0)?);
        Ok(Value::num(round_decimal(x, d, RoundMode::Down)))
    })())
}

fn sqrtpi(ctx: &mut EvalCtx, ops: &[Operand]) -> Operand {
    atp::call(ctx, ops, "a", |ctx, a, _| {
        atp::out((|| {
            let x = atp::req_num(ctx, a, 0)?;
            if x < 0.0 {
                Err(XlError::NUM)
            } else {
                Ok(crate::x87::sqrt(crate::x87::mul(x, std::f64::consts::PI)))
            }
        })())
    })
}

fn mround(ctx: &mut EvalCtx, ops: &[Operand]) -> Operand {
    atp::call(ctx, ops, "aa", |ctx, a, _| {
        val((|| {
            let x = atp::req_num(ctx, a, 0)?;
            let m = atp::req_num(ctx, a, 1)?;
            if m == 0.0 || x == 0.0 {
                return Ok(Value::Number(0.0));
            }
            if x.signum() != m.signum() {
                return Err(XlError::NUM);
            }

            let q = crate::x87::div(x, m).abs();
            let f = q.floor();
            let f = if crate::x87::sub(q, f) >= crate::x87::sub(0.5, 5e-15) {
                crate::x87::add(f, 1.0)
            } else {
                f
            };
            Ok(Value::num(crate::x87::mul(f, m.abs()).copysign(x)))
        })())
    })
}

fn ceiling(ctx: &mut EvalCtx, a: &[Value]) -> Value {
    val((|| {
        let x = num(ctx, &a[0])?;
        let s = num(ctx, &a[1])?;
        if x == 0.0 || s == 0.0 {
            return Ok(Value::Number(0.0));
        }
        if x > 0.0 && s < 0.0 {
            return Err(XlError::NUM);
        }
        let q = x / s;
        let r = round_decimal(q, 0, RoundMode::Up);

        let r = if x < 0.0 && s > 0.0 {
            round_decimal(q, 0, RoundMode::Down)
        } else {
            r
        };
        Ok(Value::num(r * s))
    })())
}

fn floor(ctx: &mut EvalCtx, a: &[Value]) -> Value {
    val((|| {
        let x = num(ctx, &a[0])?;
        let s = num(ctx, &a[1])?;
        if x == 0.0 {
            return Ok(Value::Number(0.0));
        }
        if s == 0.0 {
            return Err(XlError::DIV0);
        }
        if x > 0.0 && s < 0.0 {
            return Err(XlError::NUM);
        }
        let q = x / s;
        let r = if x < 0.0 && s > 0.0 {
            round_decimal(q, 0, RoundMode::Up)
        } else {
            round_decimal(q, 0, RoundMode::Down)
        };
        Ok(Value::num(r * s))
    })())
}

fn ceil_tol(q: f64) -> f64 {
    round_decimal(
        q,
        0,
        if q >= 0.0 {
            RoundMode::Up
        } else {
            RoundMode::Down
        },
    )
}

fn floor_tol(q: f64) -> f64 {
    round_decimal(
        q,
        0,
        if q >= 0.0 {
            RoundMode::Down
        } else {
            RoundMode::Up
        },
    )
}

fn with_default_sig(
    ctx: &mut EvalCtx,
    ops: &[Operand],
    f: impl Fn(&mut EvalCtx, &[Value]) -> Value,
) -> Operand {
    let vals: Vec<Value> = ops
        .iter()
        .enumerate()
        .map(|(i, o)| match o {
            Operand::Val(v) => v.clone(),
            _ if i == 1 => Value::Number(1.0),
            _ => Value::Blank,
        })
        .collect();
    Operand::Val(f(ctx, &vals))
}

fn ceiling_math(ctx: &mut EvalCtx, a: &[Value]) -> Value {
    val((|| {
        let x = num(ctx, &a[0])?;
        let s = opt_num(ctx, a, 1, 1.0)?.abs();
        let mode = opt_num(ctx, a, 2, 0.0)?;
        if s == 0.0 || x == 0.0 {
            return Ok(Value::Number(0.0));
        }
        let q = x / s;
        let r = if x < 0.0 && mode != 0.0 {
            round_decimal(q, 0, RoundMode::Up)
        } else {
            ceil_tol(q)
        };
        Ok(Value::num(r * s))
    })())
}

fn floor_math(ctx: &mut EvalCtx, a: &[Value]) -> Value {
    val((|| {
        let x = num(ctx, &a[0])?;
        let s = opt_num(ctx, a, 1, 1.0)?.abs();
        let mode = opt_num(ctx, a, 2, 0.0)?;
        if s == 0.0 || x == 0.0 {
            return Ok(Value::Number(0.0));
        }
        let q = x / s;
        let r = if x < 0.0 && mode != 0.0 {
            round_decimal(q, 0, RoundMode::Down)
        } else {
            floor_tol(q)
        };
        Ok(Value::num(r * s))
    })())
}

fn precise(ctx: &mut EvalCtx, a: &[Value], up: bool) -> Value {
    val((|| {
        let x = num(ctx, &a[0])?;
        let s = opt_num(ctx, a, 1, 1.0)?.abs();
        if s == 0.0 || x == 0.0 {
            return Ok(Value::Number(0.0));
        }
        let q = x / s;
        let r = if up { ceil_tol(q) } else { floor_tol(q) };
        Ok(Value::num(r * s))
    })())
}

fn even_odd(x: f64, even: bool) -> f64 {
    let sign = if x < 0.0 { -1.0 } else { 1.0 };
    let a = x.abs();
    let mut n = a.ceil();
    if even {
        if n % 2.0 != 0.0 {
            n += 1.0;
        }
    } else if n % 2.0 == 0.0 {
        n += 1.0;
    }
    sign * n
}

fn modulo(ctx: &mut EvalCtx, a: &[Value]) -> Value {
    val((|| {
        let n = num(ctx, &a[0])?;
        let d = num(ctx, &a[1])?;
        if d == 0.0 {
            return Err(XlError::DIV0);
        }

        if (n / d).abs() >= 1.1259e12 {
            return Err(XlError::NUM);
        }

        let mut r = n % d;
        if r != 0.0 && (r < 0.0) != (d < 0.0) {
            r += d;
            if r == d {
                r = 0.0;
            }
        }
        Ok(Value::num(r))
    })())
}

fn quotient(ctx: &mut EvalCtx, ops: &[Operand]) -> Operand {
    atp::call(ctx, ops, "bb", |ctx, a, _| {
        val((|| {
            let n = atp::req_num(ctx, a, 0)?;
            let d = atp::req_num(ctx, a, 1)?;
            if d == 0.0 {
                return Err(XlError::DIV0);
            }
            Ok(Value::num((n / d).trunc()))
        })())
    })
}

fn log(ctx: &mut EvalCtx, a: &[Value]) -> Value {
    val((|| {
        let x = num(ctx, &a[0])?;
        let b = opt_num(ctx, a, 1, 10.0)?;
        if x <= 0.0 || b <= 0.0 {
            return Err(XlError::NUM);
        }
        if b == 1.0 {
            return Err(XlError::DIV0);
        }

        if a.len() < 2 {
            return Ok(Value::num(crate::x87::log10(x)));
        }
        Ok(Value::num(crate::x87::div(
            crate::x87::ln(x),
            crate::x87::ln(b),
        )))
    })())
}

fn fact(ctx: &mut EvalCtx, a: &[Value]) -> Value {
    val((|| {
        let n = num(ctx, &a[0])?;
        if n < 0.0 {
            return Err(XlError::NUM);
        }
        let n = ifuzz(n);

        let mut acc = 1.0f64;
        let mut i = n;
        while i >= 2.0 {
            acc *= i;
            if !acc.is_finite() {
                return Err(XlError::NUM);
            }
            i -= 1.0;
        }
        Ok(Value::num(acc))
    })())
}

fn fact_double(ctx: &mut EvalCtx, ops: &[Operand]) -> Operand {
    atp::call(ctx, ops, "a", |ctx, a, _| {
        val((|| {
            let n = atp::req_num(ctx, a, 0)?;
            if n < -1.0 {
                return Err(XlError::NUM);
            }
            let n = n.trunc();
            let mut acc = 1.0f64;
            let mut i = n;
            while i > 1.0 {
                acc *= i;
                if !acc.is_finite() {
                    return Err(XlError::NUM);
                }
                i -= 2.0;
            }
            Ok(Value::num(acc))
        })())
    })
}

fn comb_test(x: f64) -> Result<f64, XlError> {
    let tiny = x.to_bits() & 0x7FFF_0000_0000_0000 == 0;
    if (x < 0.0 && !tiny) || x >= 2147483647.0 {
        return Err(XlError::NUM);
    }
    Ok(x.trunc())
}

fn combin(ctx: &mut EvalCtx, a: &[Value]) -> Value {
    val((|| {
        let (n, k) = (num(ctx, &a[0])?, num(ctx, &a[1])?);
        let (n, k) = (comb_test(n)?, comb_test(k)?);
        if k > n {
            return Err(XlError::NUM);
        }
        Ok(Value::num(xl_combin(n, k)))
    })())
}

pub(crate) fn xl_combin(n: f64, k: f64) -> f64 {
    let k = k.min(n - k);
    if k == 0.0 {
        return 1.0;
    }
    let mut acc = 1.0f64;
    let mut j = 2.0;
    while j <= k {
        acc = crate::x87::mul(acc, crate::x87::div(n - k + j - 1.0, j));
        j += 1.0;
    }
    crate::x87::mul(acc, n)
}

fn permut(ctx: &mut EvalCtx, a: &[Value]) -> Value {
    val((|| {
        let (n, k) = (num(ctx, &a[0])?, num(ctx, &a[1])?);
        let (n, k) = (comb_test(n)?, comb_test(k)?);
        if k > n {
            return Err(XlError::NUM);
        }
        let mut acc = 1.0f64;
        let mut i = k - 1.0;
        while i >= 0.0 {
            acc *= n - i;
            i -= 1.0;
        }
        Ok(Value::num(acc))
    })())
}

fn atan2(ctx: &mut EvalCtx, a: &[Value]) -> Value {
    val((|| {
        let x = num(ctx, &a[0])?;
        let y = num(ctx, &a[1])?;
        if x == 0.0 && y == 0.0 {
            return Err(XlError::DIV0);
        }
        Ok(Value::num(crate::xmath::atan2(y, x)))
    })())
}

fn randbetween(ctx: &mut EvalCtx, ops: &[Operand]) -> Operand {
    atp::call(ctx, ops, "aa", |ctx, a, _| {
        val((|| {
            let hi = atp::req_num(ctx, a, 1)?;
            let lo = atp::req_num(ctx, a, 0)?;
            if lo > hi {
                return Err(XlError::NUM);
            }
            let (c, f) = (lo.ceil(), hi.floor());
            let range = crate::x87::add(crate::x87::sub(f, c), 1.0);
            let r = ctx.wb.random();
            Ok(Value::num(
                crate::x87::add(crate::x87::mul(r, range).floor(), c) + 0.0,
            ))
        })())
    })
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn decimal_rounding() {
        assert_eq!(round_decimal(2.675, 2, RoundMode::HalfAway), 2.68);
        assert_eq!(round_decimal(-2.675, 2, RoundMode::HalfAway), -2.68);
        assert_eq!(round_decimal(1234.5, -2, RoundMode::HalfAway), 1200.0);
        assert_eq!(round_decimal(1250.0, -2, RoundMode::HalfAway), 1300.0);
        assert_eq!(round_decimal(0.5, 0, RoundMode::HalfAway), 1.0);
        assert_eq!(round_decimal(9.99, 1, RoundMode::HalfAway), 10.0);
        assert_eq!(round_decimal(1.23456, 3, RoundMode::Down), 1.234);

        assert_eq!(round_decimal(1.23456, 3, RoundMode::Up), 1.2349999999999999);
        assert_eq!(round_decimal(0.0001, 2, RoundMode::Up), 0.01);
        assert_eq!(round_decimal(5.0, -2, RoundMode::HalfAway), 0.0);
        assert_eq!(round_decimal(50.0, -2, RoundMode::HalfAway), 100.0);
    }
}
