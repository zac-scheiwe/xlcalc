use super::args::{ifuzz, num, text, val};
use super::engineering::atp::{self, R};
use super::{args, scalar, FnDef};
use crate::datetime::{self as dt, DateSystem};
use crate::eval::{EvalCtx, Operand};
use crate::value::{Value, XlError};

pub const DEFS: &[FnDef] = &[
    args("NETWORKDAYS", 2, 3, "R", |c, o| networkdays(c, o, false)),
    args("NETWORKDAYS.INTL", 2, 4, "R", |c, o| {
        networkdays(c, o, true)
    }),
    args("WORKDAY", 2, 3, "R", |c, o| workday(c, o, false)),
    args("WORKDAY.INTL", 2, 4, "R", |c, o| workday(c, o, true)),
    args("WEEKNUM", 1, 2, "R", weeknum),
    scalar("ISOWEEKNUM", 1, 1, isoweeknum),
    scalar("DATEDIF", 3, 3, datedif),
];

fn sys(ctx: &EvalCtx) -> DateSystem {
    ctx.wb.settings.date_system
}

fn day(ctx: &EvalCtx, n: f64) -> R<i64> {
    if n < 0.0 || n.floor() > sys(ctx).max_serial() {
        return Err(XlError::NUM);
    }
    Ok(n.floor() as i64)
}

fn atp_date(ctx: &EvalCtx, a: &[Option<Value>], i: usize) -> R<i64> {
    let n = atp::req_num(ctx, a, i)?;
    let shift = a[i].as_ref().map_or(0.0, |v| ctx.atp_text_shift(v));
    day(ctx, n + shift).map(|d| d - shift as i64)
}

type Weekend = [bool; 7];

const SAT_SUN: Weekend = [true, false, false, false, false, false, true];

fn weekend_arg(ctx: &EvalCtx, v: Option<&Value>, workday: bool) -> R<Weekend> {
    let Some(v) = v else { return Ok(SAT_SUN) };
    match v {
        Value::Error(_) => Err(if workday {
            XlError::NUM
        } else {
            XlError::VALUE
        }),
        Value::Text(s) => {
            let b = s.as_bytes();
            if b.len() != 7 || b.iter().any(|&c| c != b'0' && c != b'1') {
                return Err(XlError::VALUE);
            }
            if workday && b.iter().all(|&c| c == b'1') {
                return Err(XlError::VALUE);
            }

            let mut w = [false; 7];
            for (i, &c) in b.iter().enumerate() {
                w[(i + 1) % 7] = c == b'1';
            }
            Ok(w)
        }
        other => {
            let code = num(ctx, other)?.trunc();
            let mut w = [false; 7];
            match code as i64 {
                c @ 1..=7 if code == c as f64 => {
                    let first = (c + 5) % 7;
                    w[first as usize] = true;
                    w[((first + 1) % 7) as usize] = true;
                }
                c @ 11..=17 if code == c as f64 => {
                    w[(c - 11) as usize] = true;
                }
                _ => return Err(XlError::NUM),
            }
            Ok(w)
        }
    }
}

fn holidays(ctx: &mut EvalCtx, op: Option<&Operand>) -> R<Vec<i64>> {
    let mut out = Vec::new();
    let Some(op) = op else { return Ok(out) };

    if matches!(op, Operand::Ref(r) if r.single().is_none()) {
        return Err(XlError::VALUE);
    }
    atp::visit_list(ctx, op, |ctx, v| {
        let n = match v {
            Value::Blank => return Ok(()),
            Value::Number(n) => *n,
            Value::Text(_) => ctx.to_number(v)?,
            Value::Error(e) => return Err(*e),
            _ => return Err(XlError::VALUE),
        };
        out.push(day(ctx, n)?);
        Ok(())
    })?;
    out.sort_unstable();
    out.dedup();
    Ok(out)
}

fn weekday(ctx: &EvalCtx, s: i64) -> usize {
    dt::weekday_from_serial(sys(ctx), s) as usize
}

fn networkdays(ctx: &mut EvalCtx, ops: &[Operand], intl: bool) -> Operand {
    let spec = if intl { "bbvr" } else { "bbr" };
    let hol_idx = if intl { 3 } else { 2 };
    atp::call(ctx, ops, spec, |ctx, a, raw| {
        val((|| {
            let w = if intl {
                weekend_arg(ctx, a.get(2).and_then(|v| v.as_ref()), false)?
            } else {
                SAT_SUN
            };
            let s = atp_date(ctx, a, 0)?;
            let e = atp_date(ctx, a, 1)?;
            let hol = holidays(ctx, raw.get(hol_idx))?;
            let (lo, hi, sign) = if s <= e { (s, e, 1.0) } else { (e, s, -1.0) };
            let per_week = w.iter().filter(|x| !**x).count() as i64;
            let total = hi - lo + 1;
            let mut count = total / 7 * per_week;
            let mut d = lo + total / 7 * 7;
            while d <= hi {
                if !w[weekday(ctx, d)] {
                    count += 1;
                }
                d += 1;
            }

            let scalar_hol =
                matches!(raw.get(hol_idx), Some(Operand::Val(v)) if !matches!(v, Value::Array(_)));
            let hw = if scalar_hol { SAT_SUN } else { w };
            for &h in &hol {
                if h >= lo && h <= hi && !hw[weekday(ctx, h)] {
                    count -= 1;
                }
            }
            Ok(Value::Number(sign * count as f64))
        })())
    })
}

fn workday(ctx: &mut EvalCtx, ops: &[Operand], intl: bool) -> Operand {
    let spec = if intl { "bbvr" } else { "bbr" };
    let hol_idx = if intl { 3 } else { 2 };
    atp::call(ctx, ops, spec, |ctx, a, raw| {
        val((|| {
            let s = atp_date(ctx, a, 0)?;
            let days = atp::req_num(ctx, a, 1)?.floor();

            if sys(ctx) == DateSystem::D1904 && s as f64 + days > 2_955_541.0 {
                return Err(XlError::NUM);
            }

            if days == 0.0 {
                return Ok(Value::Number(s as f64));
            }

            if s as f64 + days < 0.0 || s as f64 + days > sys(ctx).max_serial() {
                return Err(XlError::NUM);
            }
            let w = if intl {
                let w = weekend_arg(ctx, a.get(2).and_then(|v| v.as_ref()), true);

                let scalar = matches!(raw.get(hol_idx), Some(Operand::Val(v)) if !matches!(v, Value::Array(_)));
                if w.is_err() && scalar {
                    holidays(ctx, raw.get(hol_idx))?;
                }
                w?
            } else {
                SAT_SUN
            };

            let hol_res = holidays(ctx, raw.get(hol_idx));
            let max = sys(ctx).max_serial() as i64;
            if days.abs() > 4.0 * max as f64 {
                return Err(XlError::NUM);
            }
            let bad_hol = hol_res.as_ref().err().copied();
            let hol = hol_res.unwrap_or_default();
            let step = if days < 0.0 { -1 } else { 1 };
            let mut left = days.abs() as i64;
            let mut d = s;
            while left > 0 {
                d += step;
                if d < 0 || d > max {
                    return Err(XlError::NUM);
                }
                if !w[weekday(ctx, d)] && hol.binary_search(&d).is_err() {
                    left -= 1;
                }
            }
            if let Some(e) = bad_hol {
                return Err(e);
            }
            Ok(Value::Number(d as f64))
        })())
    })
}

fn jan1(cal: DateSystem, s: i64) -> i64 {
    let (y, _, _) = dt::ymd_from_serial(cal, s);
    dt::serial_from_ymd(cal, y, 1, 1)
}

fn iso_week(cal: DateSystem, s: i64) -> i64 {
    let mon_index = (dt::weekday_from_serial(cal, s) as i64 + 6) % 7;
    let thu = s - mon_index + 3;
    (thu - jan1(cal, thu)) / 7 + 1
}

fn weeknum(ctx: &mut EvalCtx, ops: &[Operand]) -> Operand {
    atp::call(ctx, ops, "b", |ctx, a, _| {
        val((|| {
            let s = atp_date(ctx, a, 0)?;

            let shift = a[0].as_ref().map_or(0.0, |v| ctx.atp_text_shift(v));
            let (cal, s) = if shift != 0.0 {
                (DateSystem::D1900, s + shift as i64)
            } else {
                (sys(ctx), s)
            };
            let ty = ifuzz(atp::opt_num(ctx, a, 1)?.unwrap_or(1.0));
            let start = match ty as i64 {
                1 | 17 => 0,
                2 | 11 => 1,
                t @ 12..=16 => t - 10,
                21 => return Ok(Value::Number(iso_week(cal, s) as f64)),
                _ => return Err(XlError::NUM),
            };
            let j = jan1(cal, s);
            let offset = (dt::weekday_from_serial(cal, j) as i64 - start).rem_euclid(7);
            let w = (s - j + offset).div_euclid(7) + 1;
            Ok(Value::Number(w as f64))
        })())
    })
}

fn isoweeknum(ctx: &mut EvalCtx, a: &[Value]) -> Value {
    val((|| {
        let n = num(ctx, &a[0])?;
        let s = day(ctx, n)?;
        Ok(Value::Number(iso_week(sys(ctx), s) as f64))
    })())
}

fn month_days(sys: DateSystem, y: i64, m: u32) -> i64 {
    if sys == DateSystem::D1900 && y == 1900 && m == 2 {
        29
    } else {
        dt::days_in_month(y, m) as i64
    }
}

fn datedif(ctx: &mut EvalCtx, a: &[Value]) -> Value {
    val((|| {
        let s = num(ctx, &a[0])?;
        let e = num(ctx, &a[1])?;
        let unit = text(&a[2])?;
        let (sf, ef) = (s, e);

        let lim = sys(ctx).max_serial() + 1.0;
        if s + 0.5 / 86_400.0 >= lim || e + 0.5 / 86_400.0 >= lim {
            return Err(XlError::NUM);
        }
        let s = day(ctx, s)?;
        let e = day(ctx, e)?;
        if s > e {
            return Err(XlError::NUM);
        }
        let sy = sys(ctx);

        let (y1, m1, d1) = dt::ymd_from_serial(sy, dt::split_serial(sf).0);
        let (y2, m2, d2) = dt::ymd_from_serial(sy, dt::split_serial(ef).0);
        let months = (y2 * 12 + m2 as i64) - (y1 * 12 + m1 as i64) - if d2 < d1 { 1 } else { 0 };

        let md = if d2 >= d1 {
            (d2 - d1) as i64
        } else {
            let (py, pm) = if m2 == 1 { (y2 - 1, 12) } else { (y2, m2 - 1) };
            month_days(sy, py, pm) - d1 as i64 + d2 as i64
        };
        let r = match unit.to_ascii_uppercase().as_str() {
            "D" => e - s,
            "M" => months,
            "Y" => y2 - y1 - if (m2, d2) < (m1, d1) { 1 } else { 0 },
            "YM" => months.rem_euclid(12),
            "MD" => md,

            "YD" => {
                let ym = months.rem_euclid(12);
                let anchor = dt::date_serial(sy, y1 as f64, (m1 as i64 + ym) as f64, d1 as f64)
                    .ok_or(XlError::NUM)?;
                anchor as i64 - dt::split_serial(sf).0 + md
            }
            _ => return Err(XlError::NUM),
        };
        Ok(Value::Number(r as f64))
    })())
}
