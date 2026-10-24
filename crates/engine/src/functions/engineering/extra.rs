use super::atp::{self, R};
use super::xlmath;
use crate::eval::{EvalCtx, Operand};
use crate::functions::args::{check_array_args, num, val, Grid};
use crate::value::{Value, XlError};

pub fn multinomial(ctx: &mut EvalCtx, ops: &[Operand]) -> Operand {
    let r: R<f64> = (|| {
        let mut terms = Vec::new();
        let mut skipped = false;
        for (i, op) in ops.iter().enumerate() {
            if op.is_missing() {
                if i == 0 {
                    return Err(XlError::NA);
                }
                skipped = true;
                continue;
            }
            if let Operand::Ref(r) = op {
                if let Some(a) = r.single() {
                    if a.rect.is_cell() && matches!(ctx.area_array(a), Value::Blank) {
                        skipped = true;
                        continue;
                    }
                }
            }
            atp::visit_list(ctx, op, |ctx, v| {
                let n = match v {
                    Value::Blank => 0.0,
                    Value::Number(n) => *n,
                    Value::Text(_) => ctx.to_number(v)?,
                    Value::Error(e) => return Err(*e),
                    _ => return Err(XlError::Value),
                };
                terms.push(n.trunc());
                Ok(())
            })?;
        }
        if terms.is_empty() && skipped {
            return Err(XlError::Value);
        }
        if terms.iter().any(|&t| t < 0.0) {
            return Err(XlError::Num);
        }
        let total: f64 = terms.iter().sum();
        let num = ln_gamma(total + 1.0).exp();
        if !num.is_finite() || total > 170.0 {
            return Err(XlError::Num);
        }

        use crate::x87;
        let mut l = 0.0;
        let mut k = 2.0;
        while k <= total {
            l = x87::add(x87::log10(k), l);
            k += 1.0;
        }
        for t in terms.iter().rev() {
            let mut k = 2.0;
            while k <= *t {
                l = x87::add(-x87::log10(k), l);
                k += 1.0;
            }
        }
        let p = x87::exp(x87::mul(l.abs(), x87::ln(10.0)));
        Ok(if l < 0.0 { x87::div(1.0, p) } else { p })
    })();
    Operand::Val(atp::out(r))
}

fn ln_gamma(x: f64) -> f64 {
    const G: [f64; 9] = [
        0.999_999_999_999_809_9,
        676.520_368_121_885_1,
        -1_259.139_216_722_402_8,
        771.323_428_777_653_1,
        -176.615_029_162_140_6,
        12.507_343_278_686_905,
        -0.138_571_095_265_720_12,
        9.984_369_578_019_572e-6,
        1.505_632_735_149_311_6e-7,
    ];
    let x = x - 1.0;
    let mut a = G[0];
    let t = x + 7.5;
    for (i, g) in G.iter().enumerate().skip(1) {
        a += g / (x + i as f64);
    }
    0.5 * (2.0 * std::f64::consts::PI).ln() + (x + 0.5) * t.ln() - t + a.ln()
}

pub fn seriessum(ctx: &mut EvalCtx, ops: &[Operand]) -> Operand {
    atp::call(ctx, ops, "aaar", |ctx, a, raw| {
        atp::out((|| {
            let x = atp::req_num(ctx, a, 0)?;
            let n = atp::req_num(ctx, a, 1)?;
            let m = atp::req_num(ctx, a, 2)?;
            let coef = raw.get(3).ok_or(XlError::NA)?;
            if coef.is_missing() {
                return Err(XlError::NA);
            }
            let mut cs = Vec::new();
            atp::visit_list(ctx, coef, |ctx, v| {
                match v {
                    Value::Blank => cs.push(0.0),
                    Value::Bool(_) => return Err(XlError::Value),
                    other => cs.push(ctx.to_number(other)?),
                }
                Ok(())
            })?;
            let mut sum = 0.0;
            for (i, c) in cs.iter().enumerate() {
                let p = n + i as f64 * m;
                if x == 0.0 && p <= 0.0 {
                    return Err(XlError::Num);
                }
                let t = c * x.powf(p);
                if !t.is_nan() {
                    sum += t;
                }
            }
            Ok(sum)
        })())
    })
}

#[derive(Clone, Copy)]
pub enum SumKind {
    X2MY2,
    X2PY2,
    XMY2,
}

pub fn sumx(ctx: &mut EvalCtx, ops: &[Operand], kind: SumKind) -> Operand {
    let r: R<f64> = (|| {
        check_array_args(ctx, &[&ops[0], &ops[1]])?;
        let gx = Grid::from_operand(&ops[0])?.to_array(ctx);
        let gy = Grid::from_operand(&ops[1])?.to_array(ctx);
        if gx.len() != gy.len() {
            return Err(XlError::NA);
        }
        let mut sum = 0.0;
        let mut any = false;
        for (x, y) in gx.data.iter().zip(gy.data.iter()) {
            if let Value::Error(e) = x {
                return Err(*e);
            }
            if let Value::Error(e) = y {
                return Err(*e);
            }
            let (Value::Number(x), Value::Number(y)) = (x, y) else {
                continue;
            };
            any = true;
            sum += match kind {
                SumKind::X2MY2 => x * x - y * y,
                SumKind::X2PY2 => x * x + y * y,
                SumKind::XMY2 => (x - y) * (x - y),
            };
        }
        if !any {
            return Err(XlError::Div0);
        }
        Ok(sum)
    })();
    Operand::Val(atp::out(r))
}

const INT_LIMIT: f64 = 2_147_483_647.0;

pub fn combina(ctx: &mut EvalCtx, a: &[Value]) -> Value {
    val((|| {
        let n = num(ctx, &a[0])?.trunc();
        let k = num(ctx, &a[1])?.trunc();
        if n < 0.0 || k < 0.0 {
            return Err(XlError::Num);
        }
        if k == 0.0 {
            return Ok(Value::Number(1.0));
        }
        if n == 0.0 {
            return Err(XlError::Num);
        }
        if n >= INT_LIMIT || k >= INT_LIMIT {
            return Err(XlError::Num);
        }
        let nn = n + k - 1.0;

        Ok(Value::num(crate::functions::math::xl_combin(nn, k)))
    })())
}

pub fn permutationa(ctx: &mut EvalCtx, a: &[Value]) -> Value {
    val((|| {
        let n = num(ctx, &a[0])?.trunc();
        let k = num(ctx, &a[1])?.trunc();
        if n < 0.0 || k < 0.0 {
            return Err(XlError::Num);
        }
        if n >= INT_LIMIT || k >= INT_LIMIT {
            return Err(XlError::Num);
        }
        Ok(Value::num(n.powf(k)))
    })())
}

fn trig_arg(ctx: &EvalCtx, a: &[Value]) -> R<f64> {
    let x = num(ctx, &a[0])?;
    if x.abs() >= xlmath::TRIG_LIMIT {
        return Err(XlError::Num);
    }
    Ok(x)
}

fn recip(d: f64) -> R<f64> {
    if d == 0.0 {
        Err(XlError::Div0)
    } else {
        Ok(crate::x87::div(1.0, d))
    }
}

pub fn cot(ctx: &mut EvalCtx, a: &[Value]) -> Value {
    atp::out(trig_arg(ctx, a).and_then(|x| {
        if x == 0.0 {
            Err(XlError::Div0)
        } else {
            recip(crate::x87::tan(x))
        }
    }))
}

pub fn csc(ctx: &mut EvalCtx, a: &[Value]) -> Value {
    atp::out(trig_arg(ctx, a).and_then(|x| recip(crate::x87::sin(x))))
}

pub fn sec(ctx: &mut EvalCtx, a: &[Value]) -> Value {
    atp::out(trig_arg(ctx, a).and_then(|x| recip(crate::x87::cos(x))))
}

pub fn coth(ctx: &mut EvalCtx, a: &[Value]) -> Value {
    atp::out(num(ctx, &a[0]).and_then(|x| match crate::xmath::tanh(x) {
        0.0 => Err(XlError::Div0),
        t => Ok(crate::x87::div(1.0, t)),
    }))
}

pub fn csch(ctx: &mut EvalCtx, a: &[Value]) -> Value {
    atp::out(num(ctx, &a[0]).and_then(|x| recip(crate::xmath::sinh(x))))
}

pub fn sech(ctx: &mut EvalCtx, a: &[Value]) -> Value {
    atp::out(num(ctx, &a[0]).and_then(|x| recip(crate::xmath::cosh(x))))
}

pub fn acot(ctx: &mut EvalCtx, a: &[Value]) -> Value {
    atp::out(num(ctx, &a[0]).map(|x| {
        if x == 0.0 {
            std::f64::consts::FRAC_PI_2
        } else if x > 0.0 {
            xlmath::atan(1.0 / x)
        } else {
            std::f64::consts::PI + xlmath::atan(1.0 / x)
        }
    }))
}

fn acoth_abs(x: f64) -> f64 {
    use crate::x87;
    if x < 3.69662725 {
        return x87::div(x87::ln(x87::div(x87::add(x, 1.0), x87::sub(x, 1.0))), 2.0);
    }
    let x2 = x87::mul(x, x);
    let (mut s, mut p, mut k) = (x87::div(1.0, x), x, 3.0);
    loop {
        p = x87::mul(p, x2);
        let t = x87::div(1.0, x87::mul(p, k));
        let n = x87::add(s, t);
        if n == s {
            return s;
        }
        s = n;
        k += 2.0;
    }
}

pub fn acoth(ctx: &mut EvalCtx, a: &[Value]) -> Value {
    atp::out(num(ctx, &a[0]).and_then(|x| {
        if x.abs() <= 1.0 {
            Err(XlError::Num)
        } else {
            let r = acoth_abs(x.abs());
            Ok(if x < 0.0 { -r } else { r })
        }
    }))
}
