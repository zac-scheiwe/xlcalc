use super::args::*;
use super::{args, FnDef, MANY, MANY254};
use crate::eval::{EvalCtx, Operand};
use crate::value::{Value, XlError};
use crate::x87;

pub const DEFS: &[FnDef] = &[
    args("COUNT", 1, MANY, "R", count),
    args("COUNTA", 1, MANY, "R", counta),
    args("COUNTBLANK", 1, 1, "Q", countblank),
    args("AVERAGE", 1, MANY, "R", |c, o| {
        agg(c, o, SUM_POLICY, average)
    }),
    args("AVERAGEA", 1, MANY, "R", |c, o| {
        agg(c, o, AVGA_POLICY, average)
    }),
    args("MIN", 1, MANY, "R", |c, o| agg(c, o, SUM_POLICY, min)),
    args("MAX", 1, MANY, "R", |c, o| agg(c, o, SUM_POLICY, max)),
    args("MINA", 1, MANY, "R", |c, o| agg(c, o, A_POLICY, min)),
    args("MAXA", 1, MANY, "R", |c, o| agg(c, o, A_POLICY, max)),
    args("MEDIAN", 1, MANY, "R", |c, o| agg(c, o, SUM_POLICY, median)),
    args("VAR.S", 1, MANY254, "R", |c, o| {
        agg(c, o, SUM_POLICY, |v| var(v, true))
    }),
    args("VAR", 1, MANY, "R", |c, o| {
        agg(c, o, SUM_POLICY, |v| var(v, true))
    }),
    args("VAR.P", 1, MANY254, "R", |c, o| {
        agg(c, o, SUM_POLICY, |v| var(v, false))
    }),
    args("VARP", 1, MANY, "R", |c, o| {
        agg(c, o, SUM_POLICY, |v| var(v, false))
    }),
    args("VARA", 1, MANY, "R", |c, o| {
        agg(c, o, A_POLICY, |v| var(v, true))
    }),
    args("VARPA", 1, MANY, "R", |c, o| {
        agg(c, o, A_POLICY, |v| var(v, false))
    }),
    args("STDEV.S", 1, MANY254, "R", |c, o| {
        agg(c, o, SUM_POLICY, |v| stdev(v, true))
    }),
    args("STDEV", 1, MANY, "R", |c, o| {
        agg(c, o, SUM_POLICY, |v| stdev(v, true))
    }),
    args("STDEV.P", 1, MANY254, "R", |c, o| {
        agg(c, o, SUM_POLICY, |v| stdev(v, false))
    }),
    args("STDEVP", 1, MANY, "R", |c, o| {
        agg(c, o, SUM_POLICY, |v| stdev(v, false))
    }),
    args("STDEVA", 1, MANY, "R", |c, o| {
        agg(c, o, A_POLICY, |v| stdev(v, true))
    }),
    args("STDEVPA", 1, MANY, "R", |c, o| {
        agg(c, o, A_POLICY, |v| stdev(v, false))
    }),
    args("DEVSQ", 1, MANY, "R", |c, o| agg(c, o, SUM_POLICY, devsq)),
    args("AVEDEV", 1, MANY, "R", |c, o| agg(c, o, SUM_POLICY, avedev)),
    args("GEOMEAN", 1, MANY, "R", |c, o| {
        agg(c, o, SUM_POLICY, geomean)
    }),
    args("HARMEAN", 1, MANY, "R", |c, o| {
        agg(c, o, SUM_POLICY, harmean)
    }),
    args("LARGE", 2, 2, "XV", |c, o| kth(c, o, true)),
    args("SMALL", 2, 2, "XV", |c, o| kth(c, o, false)),
];

fn count(ctx: &mut EvalCtx, ops: &[Operand]) -> Operand {
    let mut n = 0.0;
    let r = visit_numbers(ctx, ops, COUNT_POLICY, |_| n += 1.0);
    opval(r.map(|_| Value::Number(n)))
}

fn counta(ctx: &mut EvalCtx, ops: &[Operand]) -> Operand {
    let mut n = 0.0;
    for op in ops {
        match op {
            Operand::Missing => n += 1.0,

            Operand::Val(Value::Array(a)) => n += a.data.len() as f64,
            Operand::Val(Value::Blank) => {}
            Operand::Val(_) => n += 1.0,
            Operand::Ref(r) => {
                for area in r.areas() {
                    ctx.visit_area(area, |_, _, v| {
                        if !v.is_blank() {
                            n += 1.0
                        }
                    });
                }
            }
        }
    }
    Operand::Val(Value::Number(n))
}

fn countblank(ctx: &mut EvalCtx, ops: &[Operand]) -> Operand {
    match &ops[0] {
        Operand::Ref(r) => {
            let Some(area) = r.single() else {
                return Operand::err(XlError::Value);
            };
            let area = *area;
            let total = area.rect.cell_count() as f64;
            let mut filled = 0.0;
            ctx.visit_area(&area, |_, _, v| match v {
                Value::Text(s) if s.is_empty() => {}
                Value::Blank => {}
                _ => filled += 1.0,
            });
            Operand::Val(Value::Number(total - filled))
        }
        Operand::Val(Value::Error(e)) => Operand::err(*e),
        _ => Operand::err(XlError::Value),
    }
}

fn agg(
    ctx: &mut EvalCtx,
    ops: &[Operand],
    p: AggPolicy,
    f: impl Fn(&mut Vec<f64>) -> R<f64>,
) -> Operand {
    opval(
        collect_numbers(ctx, ops, p)
            .and_then(|mut v| f(&mut v))
            .map(Value::num),
    )
}

fn mean(v: &[f64]) -> f64 {
    v.iter().sum::<f64>() / v.len() as f64
}

fn average(v: &mut Vec<f64>) -> R<f64> {
    if v.is_empty() {
        return Err(XlError::Div0);
    }

    let mut acc = 0.0f64;
    for &x in v.iter() {
        acc = crate::number::snap_add(acc, x, acc + x);
    }
    Ok(acc / v.len() as f64)
}

fn min(v: &mut Vec<f64>) -> R<f64> {
    Ok(v.iter()
        .copied()
        .fold(None, |m: Option<f64>, x| Some(m.map_or(x, |m| m.min(x))))
        .unwrap_or(0.0))
}

fn max(v: &mut Vec<f64>) -> R<f64> {
    Ok(v.iter()
        .copied()
        .fold(None, |m: Option<f64>, x| Some(m.map_or(x, |m| m.max(x))))
        .unwrap_or(0.0))
}

fn median(v: &mut Vec<f64>) -> R<f64> {
    if v.is_empty() {
        return Err(XlError::Num);
    }
    v.sort_by(|a, b| a.partial_cmp(b).unwrap());
    let n = v.len();
    Ok(if n % 2 == 1 {
        v[n / 2]
    } else {
        (v[n / 2 - 1] + v[n / 2]) / 2.0
    })
}

fn devsq(v: &mut Vec<f64>) -> R<f64> {
    if v.is_empty() {
        return Err(XlError::Num);
    }
    let m = mean(v);
    Ok(v.iter().map(|x| (x - m) * (x - m)).sum())
}

fn var(v: &mut Vec<f64>, sample: bool) -> R<f64> {
    let n = v.len() as f64;
    if (sample && n < 2.0) || n < 1.0 {
        return Err(XlError::Div0);
    }
    crate::functions::criteria::stats::var(v, sample)
}

fn stdev(v: &mut Vec<f64>, sample: bool) -> R<f64> {
    var(v, sample).map(crate::x87::sqrt)
}

fn avedev(v: &mut Vec<f64>) -> R<f64> {
    if v.is_empty() {
        return Err(XlError::Num);
    }
    let m = mean(v);
    Ok(v.iter().map(|x| (x - m).abs()).sum::<f64>() / v.len() as f64)
}

fn geomean(v: &mut Vec<f64>) -> R<f64> {
    if v.is_empty() || v.iter().any(|&x| x <= 0.0) {
        return Err(XlError::Num);
    }

    let n = v.len() as i64;
    let (mut m, mut e) = (1.0f64, 0i64);
    for &x in v.iter() {
        let (mx, ex) = frexp1(x);
        m = crate::x87::mul(m, mx);
        e += ex;
        if m > 1e300 {
            let (mm, em) = frexp1(m);
            m = mm;
            e += em;
        }
    }

    let (mm, em) = frexp1(m);
    m = mm;
    e += em;
    let r = e % n;
    let q = e / n;
    if r.abs() > 1000 {
        let p = 1.0 / n as f64;
        return Ok(v.iter().map(|&x| crate::xmath::pow(x, p)).product());
    }
    let base = m * 2f64.powi(r as i32);
    let root = crate::x87::exp(crate::x87::mul(
        crate::x87::ln(base),
        crate::x87::div(1.0, n as f64),
    ));
    let res = root * 2f64.powi(q.clamp(-2000, 2000) as i32);
    if res.is_finite() {
        Ok(res)
    } else {
        Err(XlError::Num)
    }
}

fn frexp1(x: f64) -> (f64, i64) {
    let bits = x.to_bits();
    let exp = ((bits >> 52) & 0x7ff) as i64;
    if exp == 0 {
        let (m, e) = frexp1(x * 2f64.powi(64));
        return (m, e - 64);
    }
    (
        f64::from_bits((bits & !(0x7ffu64 << 52)) | (1023u64 << 52)),
        exp - 1023,
    )
}

fn harmean(v: &mut Vec<f64>) -> R<f64> {
    if v.is_empty() {
        return Err(XlError::NA);
    }
    if v.iter().any(|&x| x <= 0.0) {
        return Err(XlError::Num);
    }

    let mut sum = 0.0;
    for &x in v.iter() {
        sum = x87::add(sum, x87::div(1.0, x));
    }
    Ok(x87::div(1.0, x87::div(sum, v.len() as f64)))
}

fn kth(ctx: &mut EvalCtx, ops: &[Operand], largest: bool) -> Operand {
    let k = match &ops[1] {
        Operand::Val(v) => match ctx.to_number(v) {
            Ok(k) => k,
            Err(e) => return Operand::err(e),
        },
        _ => return Operand::err(XlError::Num),
    };
    let v = match collect_numbers(ctx, &ops[..1], SUM_POLICY) {
        Ok(v) => v,
        Err(e) => return Operand::err(e),
    };
    match crate::functions::criteria::stats::kth(&v, k, largest) {
        Ok(x) => Operand::Val(Value::Number(x)),
        Err(e) => Operand::err(e),
    }
}
