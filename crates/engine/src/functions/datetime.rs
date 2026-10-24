use super::args::*;
use super::{args, scalar, volatile, FnDef};
use crate::datetime::{self as dt, DateSystem};
use crate::eval::{EvalCtx, Operand};
use crate::value::{Value, XlError};

pub const DEFS: &[FnDef] = &[
    scalar("DATE", 3, 3, date),
    scalar("TIME", 3, 3, time),
    scalar("YEAR", 1, 1, |c, a| ymd_part(c, a, 0)),
    scalar("MONTH", 1, 1, |c, a| ymd_part(c, a, 1)),
    scalar("DAY", 1, 1, |c, a| ymd_part(c, a, 2)),
    scalar("HOUR", 1, 1, |c, a| hms_part(c, a, 0)),
    scalar("MINUTE", 1, 1, |c, a| hms_part(c, a, 1)),
    scalar("SECOND", 1, 1, |c, a| hms_part(c, a, 2)),
    scalar("WEEKDAY", 1, 2, weekday),
    scalar("DATEVALUE", 1, 1, datevalue),
    scalar("TIMEVALUE", 1, 1, timevalue),
    args("EDATE", 2, 2, "R", |c, o| edate_call(c, o, false)),
    args("EOMONTH", 2, 2, "R", |c, o| edate_call(c, o, true)),
    scalar("DAYS", 2, 2, days),
    volatile(scalar("TODAY", 0, 0, |c, _| {
        Value::Number(c.wb.calc.now.floor())
    })),
    volatile(scalar("NOW", 0, 0, |c, _| Value::Number(c.wb.calc.now))),
];

fn sys(ctx: &EvalCtx) -> DateSystem {
    ctx.wb.settings.date_system
}

pub fn serial_arg(ctx: &EvalCtx, v: &Value) -> R<f64> {
    let n = ctx.to_number(v)?;
    if n < 0.0 || n >= sys(ctx).max_serial() + 1.0 {
        return Err(XlError::Num);
    }
    Ok(n)
}

fn date(ctx: &mut EvalCtx, a: &[Value]) -> Value {
    val((|| {
        let y = num(ctx, &a[0])?;
        let m = num(ctx, &a[1])?;
        let d = num(ctx, &a[2])?;
        dt::date_serial(sys(ctx), y, m, d)
            .map(Value::Number)
            .ok_or(XlError::Num)
    })())
}

fn time(ctx: &mut EvalCtx, a: &[Value]) -> Value {
    val((|| {
        let h = num(ctx, &a[0])?;
        let m = num(ctx, &a[1])?;
        let s = num(ctx, &a[2])?;
        if h > 32767.0 || m > 32767.0 || s > 32767.0 {
            return Err(XlError::Num);
        }
        dt::time_fraction(h, m, s)
            .map(Value::Number)
            .ok_or(XlError::Num)
    })())
}

fn ymd_part(ctx: &mut EvalCtx, a: &[Value], which: usize) -> Value {
    val((|| {
        let s = serial_arg(ctx, &a[0])?;
        let (y, m, d) = dt::ymd_from_serial(sys(ctx), dt::split_serial(s).0);
        Ok(Value::Number(match which {
            0 => y as f64,
            1 => m as f64,
            _ => d as f64,
        }))
    })())
}

fn hms_part(ctx: &mut EvalCtx, a: &[Value], which: usize) -> Value {
    val((|| {
        let s = serial_arg(ctx, &a[0])?;
        let (_, h, m, sec, _) = dt::split_serial(s);
        Ok(Value::Number(match which {
            0 => h,
            1 => m,
            _ => sec,
        } as f64))
    })())
}

fn weekday(ctx: &mut EvalCtx, a: &[Value]) -> Value {
    val((|| {
        let s = dt::split_serial(serial_arg(ctx, &a[0])?).0;
        let ty = opt_num(ctx, a, 1, 1.0)?.trunc() as i64;
        let wd = dt::weekday_from_serial(sys(ctx), s) as i64;
        let r = match ty {
            1 | 17 => wd + 1,
            2 | 11 => (wd + 6) % 7 + 1,
            3 => (wd + 6) % 7,
            12..=16 => {
                let first = ty - 10;
                (wd - first).rem_euclid(7) + 1
            }
            _ => return Err(XlError::Num),
        };
        Ok(Value::Number(r as f64))
    })())
}

fn datevalue(ctx: &mut EvalCtx, a: &[Value]) -> Value {
    val((|| {
        let s = text(&a[0])?;
        let p = crate::coerce::parse_number_text(&s, ctx.parse_env()).ok_or(XlError::Value)?;
        match p.kind {
            crate::coerce::NumberKind::Date | crate::coerce::NumberKind::DateTime => {
                Ok(Value::Number(p.value.floor()))
            }

            crate::coerce::NumberKind::Time { .. } => Ok(Value::Number(0.0)),
            _ => Err(XlError::Value),
        }
    })())
}

fn timevalue(ctx: &mut EvalCtx, a: &[Value]) -> Value {
    val((|| {
        let s = text(&a[0])?;
        let p = crate::coerce::parse_number_text(&s, ctx.parse_env()).ok_or(XlError::Value)?;
        match p.kind {
            crate::coerce::NumberKind::Time { .. }
            | crate::coerce::NumberKind::DateTime
            | crate::coerce::NumberKind::Date => Ok(Value::Number(p.value - p.value.floor())),
            _ => Err(XlError::Value),
        }
    })())
}

fn edate_call(ctx: &mut EvalCtx, ops: &[Operand], end_of_month: bool) -> Operand {
    super::engineering::atp::call(ctx, ops, "bb", |ctx, a, _| {
        val((|| {
            let s = super::engineering::atp::req_num(ctx, a, 0)?;
            let shift = a[0].as_ref().map_or(0.0, |v| ctx.atp_text_shift(v));

            if s + shift < 0.0 || s + shift >= sys(ctx).max_serial() + 1.0 {
                return Err(XlError::Num);
            }
            let m = super::engineering::atp::req_num(ctx, a, 1)?;
            edate(ctx, s, shift, m, end_of_month)
        })())
    })
}

fn edate(ctx: &mut EvalCtx, s: f64, shift: f64, months: f64, end_of_month: bool) -> R<Value> {
    {
        if s + shift < 0.0 || s + shift >= sys(ctx).max_serial() + 1.0 {
            return Err(XlError::Num);
        }

        let csys = if shift != 0.0 {
            DateSystem::D1900
        } else {
            sys(ctx)
        };
        let s = (s + shift).floor() as i64;
        let months = months.trunc() as i64;
        let (y, m, d) = dt::ymd_from_serial(csys, s);
        let total = y * 12 + (m as i64 - 1) + months;
        let (ny, nm) = (total.div_euclid(12), (total.rem_euclid(12) + 1) as u32);
        if !(1900..=9999).contains(&ny) {
            return Err(XlError::Num);
        }
        let dim = dt::days_in_month(ny, nm);
        let nd = if end_of_month { dim } else { d.min(dim) };
        let r = dt::serial_from_ymd(csys, ny, nm as i64, nd as i64);

        if r < 0 && sys(ctx) == DateSystem::D1900 {
            return Err(XlError::Num);
        }
        Ok(Value::Number(r as f64 - shift))
    }
}

fn days(ctx: &mut EvalCtx, a: &[Value]) -> Value {
    val((|| {
        for v in a {
            if let Value::Error(e) = v {
                return Err(*e);
            }
        }
        let e = serial_arg(ctx, &a[0])?.floor();
        let s = serial_arg(ctx, &a[1])?.floor();
        Ok(Value::Number(e - s))
    })())
}
