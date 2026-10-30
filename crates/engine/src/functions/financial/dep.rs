use super::atp::{self, Arg};
use super::days;
use super::xmath;
use crate::eval::EvalCtx;
use crate::functions::args::{boolean, num, opt_num, sfix, val, R};
use crate::functions::math::{round_decimal, RoundMode};
use crate::value::{Value, XlError};

fn finite(x: f64) -> R<f64> {
    if x.is_finite() {
        Ok(x)
    } else {
        Err(XlError::NUM)
    }
}

pub fn sln(ctx: &mut EvalCtx, a: &[Value]) -> Value {
    val((|| {
        let (cost, salvage, life) = (num(ctx, &a[0])?, num(ctx, &a[1])?, num(ctx, &a[2])?);
        if life == 0.0 {
            return Err(XlError::DIV0);
        }
        finite((cost - salvage) / life).map(Value::num)
    })())
}

pub fn syd(ctx: &mut EvalCtx, a: &[Value]) -> Value {
    val((|| {
        let (cost, salvage, life, per) = (
            num(ctx, &a[0])?,
            num(ctx, &a[1])?,
            num(ctx, &a[2])?,
            num(ctx, &a[3])?,
        );
        if salvage < 0.0 || life <= 0.0 || per <= 0.0 || per > life {
            return Err(XlError::NUM);
        }
        finite((cost - salvage) * (life + 1.0 - per) * 2.0 / (life * (life + 1.0))).map(Value::num)
    })())
}

fn ddb_core(cost: f64, salvage: f64, life: f64, period: f64, factor: f64) -> f64 {
    use crate::x87::{div, mul, sub};

    let rate = if life == 0.0 { 0.0 } else { div(factor, life) };
    let book = if rate >= 1.0 {
        if period <= 1.0 {
            cost
        } else {
            0.0
        }
    } else {
        mul(
            cost,
            crate::xmath::pow(sub(1.0, rate), sub(period, 1.0).max(0.0)),
        )
    };
    let d = mul(book, rate);
    let left = sub(book, salvage);
    let d = if sub(left, d) < 0.0 { left } else { d };
    if d < 0.0 {
        0.0
    } else {
        d
    }
}

pub fn ddb(ctx: &mut EvalCtx, a: &[Value]) -> Value {
    val((|| {
        let (cost, salvage, life, period) = (
            num(ctx, &a[0])?,
            num(ctx, &a[1])?,
            num(ctx, &a[2])?,
            num(ctx, &a[3])?,
        );
        let factor = opt_num(ctx, a, 4, 2.0)?;
        if cost < 0.0
            || salvage < 0.0
            || life <= 0.0
            || period <= 0.0
            || period > life
            || factor <= 0.0
        {
            return Err(XlError::NUM);
        }
        finite(ddb_core(cost, salvage, life, period, factor)).map(Value::num)
    })())
}

pub fn db(ctx: &mut EvalCtx, a: &[Value]) -> Value {
    val((|| {
        let (cost, salvage, life, period) = (
            num(ctx, &a[0])?,
            num(ctx, &a[1])?,
            num(ctx, &a[2])?,
            num(ctx, &a[3])?,
        );

        let month = sfix(opt_num(ctx, a, 4, 12.0)?) as f64;
        if cost < 0.0
            || salvage < 0.0
            || life <= 0.0
            || period <= 0.0
            || !(1.0..=12.0).contains(&month)
        {
            return Err(XlError::NUM);
        }

        let fixp = |x: f64| -> i32 {
            let t = x.abs().trunc();
            (if t < 4294967296.0 { t as u32 } else { u32::MAX }) as i32
        };
        let (life_i, period_i) = (fixp(life), fixp(period));
        let partial = period_i > life_i;
        if partial && (month == 12.0 || period_i > life_i.wrapping_add(1)) {
            return Err(XlError::NUM);
        }
        let pf = (period_i as f64).max(1.0);
        if cost == 0.0 {
            return Ok(Value::Number(0.0));
        }

        use crate::x87::{div, mul, sub};
        let rate = sub(1.0, crate::xmath::pow(div(salvage, cost), div(1.0, life)));
        let rate = round_decimal(rate, 3, RoundMode::HalfAway);
        let first = mul(div(month, 12.0), mul(cost, rate));
        if pf == 1.0 {
            return finite(first).map(Value::num);
        }
        let mut book = sub(cost, first);
        let mut d = 0.0;
        let imax = (life_i as f64).min(pf) as i64;
        for _ in 2..=imax {
            d = mul(book, rate);
            book = sub(book, d);
        }
        if partial {
            d = mul(mul(rate, div(book, 12.0)), 12.0 - month);
        }
        finite(d).map(Value::num)
    })())
}

fn vdb_switch(cost: f64, salvage: f64, life: f64, start: f64, end: f64, factor: f64) -> R<f64> {
    let f = std::cell::RefCell::new(crate::x87::MpFlag::default());
    let add = |a: f64, b: f64| f.borrow_mut().add(a, b);
    let sub = |a: f64, b: f64| f.borrow_mut().add(a, -b);
    let mul = |a: f64, b: f64| f.borrow_mut().mul(a, b);
    let div = |a: f64, b: f64| f.borrow_mut().div(a, b);
    let done = |v: f64| f.borrow().result(v);
    let rate = div(factor, life);
    let whole = start.floor();
    let raw1 = ddb_core(cost, salvage, life, 1.0, factor);
    let sln1 = div(sub(cost, salvage), life);
    if sub(sln1, raw1) > 0.0 {
        let v = mul(sln1, sub(end, start));
        return done(v);
    }
    let phase = sub(start, whole);
    let mut book = sub(cost, mul(phase, raw1));
    let mut life_rem = sub(life, phase);

    let period = |book: f64, life_rem: f64| {
        let raw = mul(book, rate);
        let left = sub(book, salvage);
        if sub(left, raw) < 0.0 {
            (left, raw, None)
        } else {
            (raw, raw, Some(div(left, life_rem)))
        }
    };
    let switch = |s: Option<f64>, raw: f64| s.is_some_and(|s| sub(s, raw) > 0.0);
    let mut k = 0.0;
    while k < whole {
        let (d, raw, s) = period(book, life_rem);
        if switch(s, raw) {
            let v = mul(s.unwrap(), sub(end, start));
            return done(v);
        }
        book = sub(book, d);
        life_rem = sub(life_rem, 1.0);
        k += 1.0;
    }
    let mut total = 0.0;
    let mut cur = start;
    loop {
        let (d, raw, s) = period(book, life_rem);
        if switch(s, raw) {
            let v = add(total, mul(s.unwrap(), sub(end, cur)));
            return done(v);
        }
        let rem = sub(end, cur);
        if sub(rem, 1.0) > 0.0 {
            total = add(total, d);
            book = sub(book, d);
            cur = add(cur, 1.0);
            life_rem = sub(life_rem, 1.0);
        } else {
            let v = add(total, mul(d, rem));
            return done(v);
        }
    }
}

pub fn vdb(ctx: &mut EvalCtx, a: &[Value]) -> Value {
    val((|| {
        let (cost, salvage, life, start, end) = (
            num(ctx, &a[0])?,
            num(ctx, &a[1])?,
            num(ctx, &a[2])?,
            num(ctx, &a[3])?,
            num(ctx, &a[4])?,
        );
        let factor = opt_num(ctx, a, 5, 2.0)?;
        let no_switch = match a.get(6) {
            None => false,
            Some(v) => boolean(v)?,
        };
        if start < 0.0
            || end < start
            || end > life
            || cost < 0.0
            || salvage < 0.0
            || factor < 0.0
            || life < 0.0
        {
            return Err(XlError::NUM);
        }
        let int_start = start.floor();
        let int_end = end.ceil();
        let mut result = 0.0;
        if no_switch {
            if life == 0.0 {
                return Err(XlError::DIV0);
            }

            use crate::x87::{add, mul, sub};
            let mut i = int_start + 1.0;
            while i <= int_end {
                let term = ddb_core(cost, salvage, life, i, factor);
                if i == int_start + 1.0 {
                    result = add(result, mul(term, sub(sub(i, 1.0), start)));
                }
                result = if i == int_end {
                    add(result, mul(term, add(1.0, sub(end, int_end))))
                } else {
                    add(result, term)
                };
                i += 1.0;
            }
        } else {
            result = vdb_switch(cost, salvage, life, start, end, factor)?;
        }
        finite(result).map(Value::num)
    })())
}

struct Amor {
    cost: f64,
    purchased: i64,
    first: i64,
    salvage: f64,
    period: f64,
    period_raw: f64,
    rate: f64,
    basis: i32,
}

fn amor_args(ctx: &EvalCtx, a: &[Arg]) -> R<Amor> {
    let cost = atp::req(ctx, a, 0)?;
    let purchased = atp::date(ctx, a, 1)?;
    let first = atp::date(ctx, a, 2)?;
    let salvage = atp::req(ctx, a, 3)?;
    let period = atp::req(ctx, a, 4)?;
    let rate = atp::req(ctx, a, 5)?;

    if cost <= 0.0
        || salvage < 0.0
        || salvage > cost
        || period < 0.0
        || rate <= 0.0
        || purchased > first
    {
        return Err(XlError::NUM);
    }
    let basis = atp::basis(ctx, a, 6)?;
    if basis == 2 {
        return Err(XlError::NUM);
    }
    Ok(Amor {
        cost,
        purchased,
        first,
        salvage,
        period: period.trunc(),
        period_raw: period,
        rate,
        basis,
    })
}

fn round0(x: f64) -> f64 {
    round_decimal(x, 0, RoundMode::HalfAway)
}

fn amor_yearfrac(sys: crate::datetime::DateSystem, p: i64, f: i64, basis: i32) -> R<f64> {
    if p == f {
        return Ok(1.0);
    }
    if basis != 1 && basis != 3 {
        return days::yearfrac_core(sys, p, f, basis);
    }
    let not_leap_day = |serial: i64| {
        let t = days::ymd(sys, serial);
        if t.m == 2 && t.d == 29 {
            serial - 1
        } else {
            serial
        }
    };
    let n = (not_leap_day(f) - not_leap_day(p)) as f64;
    let diy = if basis == 3 || !days::is_leap(days::ymd(sys, p).y) {
        365.0
    } else {
        366.0
    };
    Ok(n / diy)
}

pub fn amordegrc(ctx: &mut EvalCtx, a: &[Arg]) -> Value {
    atp::out((|| {
        let m = amor_args(ctx, a)?;
        let sys = atp::date_system(ctx);
        let life = 1.0 / m.rate;
        if life <= 2.0 {
            return Err(XlError::NUM);
        }
        let coef = if life > 6.0 {
            2.5
        } else if life > 4.0 {
            2.0
        } else {
            1.5
        };
        let rate = m.rate * coef;
        let yf = amor_yearfrac(sys, m.purchased, m.first, m.basis)?;

        let mut n = life.ceil();
        if m.purchased == m.first {
            n -= 1.0;
        }
        let halves = n - 2.0 >= 1.0;
        let d0 = yf * rate * m.cost;

        let d0 = round0(round0(d0).min(m.cost - m.salvage));
        if m.period == 0.0 {
            return Ok(d0);
        }
        let mut book = m.cost - d0;
        let mut half = 0.0;
        let mut d = 0.0;
        let mut k = 1.0;
        while k <= m.period {
            d = if book < m.salvage {
                0.0
            } else if halves && k == n - 2.0 {
                half = book * 0.5;
                half
            } else if halves && k == n - 1.0 {
                half
            } else if halves && k >= n {
                0.0
            } else {
                rate * book
            };
            book -= d;
            k += 1.0;
        }
        Ok(round0(d))
    })())
}

pub fn amorlinc(ctx: &mut EvalCtx, a: &[Arg]) -> Value {
    atp::out((|| {
        let m = amor_args(ctx, a)?;

        let m = Amor {
            period: m.period_raw.ceil(),
            ..m
        };
        let sys = atp::date_system(ctx);
        let one = m.cost * m.rate;
        let delta = m.cost - m.salvage;
        let yf = amor_yearfrac(sys, m.purchased, m.first, m.basis)?;
        let r0 = yf * m.rate * m.cost;
        let full = ((m.cost - m.salvage - r0) / one).trunc();
        let r = if m.period == 0.0 {
            r0.min(delta)
        } else if m.period <= full {
            one
        } else if m.period == full + 1.0 {
            let mut b = delta - r0;
            let mut i = 0.0;
            while i < full {
                b -= one;
                i += 1.0;
            }
            b
        } else {
            0.0
        };
        Ok(if r > 0.0 { r } else { 0.0 })
    })())
}

fn dollar(ctx: &mut EvalCtx, a: &[Arg], to_decimal: bool) -> Value {
    atp::out((|| {
        let x = atp::req(ctx, a, 0)?;
        let frac = atp::req(ctx, a, 1)?.trunc();
        if frac < 0.0 {
            return Err(XlError::NUM);
        }
        if frac < 1.0 {
            return Err(XlError::DIV0);
        }
        let sign = if x < 0.0 { -1.0 } else { 1.0 };
        let ax = x.abs();
        let int = ax.trunc();
        let f = ax - int;
        let scale = xmath::pow(10.0, frac.log10().ceil());
        let r = if to_decimal {
            f * scale / frac
        } else {
            f * frac / scale
        };
        finite(sign * (int + r))
    })())
}

pub fn dollarde(ctx: &mut EvalCtx, a: &[Arg]) -> Value {
    dollar(ctx, a, true)
}

pub fn dollarfr(ctx: &mut EvalCtx, a: &[Arg]) -> Value {
    dollar(ctx, a, false)
}
