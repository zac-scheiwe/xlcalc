use super::atp::{self, Arg};
use crate::datetime::{self as dt, DateSystem};
use crate::eval::EvalCtx;
use crate::functions::args::{boolean, num, val, R};
use crate::value::{Value, XlError};
use crate::x87;

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub struct Ymd {
    pub y: i64,
    pub m: u32,
    pub d: u32,
}

pub fn ymd(sys: DateSystem, s: i64) -> Ymd {
    let (y, m, d) = dt::ymd_from_serial(sys, s);
    Ymd { y, m, d }
}

pub fn serial(sys: DateSystem, y: i64, m: u32, d: u32) -> i64 {
    dt::serial_from_ymd(sys, y, m as i64, d as i64)
}

pub fn is_leap(y: i64) -> bool {
    dt::is_leap_year(y)
}

pub fn dim(y: i64, m: u32) -> u32 {
    dt::days_in_month(y, m)
}

fn last_of_month(sys: DateSystem, s: i64) -> bool {
    ymd(sys, s + 1).d == 1
}

fn atp_last_of_month(sys: DateSystem, s: i64) -> bool {
    let d = ymd(sys, s);
    d.d >= dim(d.y, d.m)
}

fn last_of_feb(sys: DateSystem, s: i64) -> bool {
    ymd(sys, s).m == 2 && last_of_month(sys, s)
}

fn d360(sd: i64, sm: u32, sy: i64, ed: i64, em: u32, ey: i64) -> i64 {
    (ey - sy) * 360 + (em as i64 - sm as i64) * 30 + (ed - sd)
}

pub fn days360_us(sys: DateSystem, a: i64, b: i64, both: bool) -> i64 {
    let s = ymd(sys, a);
    let e = ymd(sys, b);
    let (mut sd, mut ed) = (s.d as i64, e.d as i64);
    let feb_end = |t: Ymd, serial: i64| t.m == 2 && atp_last_of_month(sys, serial);
    if feb_end(e, b) && (feb_end(s, a) || both) {
        ed = 30;
    }
    if ed == 31 && (sd >= 30 || both) {
        ed = 30;
    }
    if sd == 31 {
        sd = 30;
    }
    if feb_end(s, a) {
        sd = 30;
    }
    d360(sd, s.m, s.y, ed, e.m, e.y)
}

pub fn yield_years(sys: DateSystem, basis: i32, settle: i64, mat: i64) -> f64 {
    let m = ymd(sys, mat);
    let s = ymd(sys, settle);
    let mut years = m.y - s.y;
    if (s.m, s.d) == (m.m, m.d) {
        return years as f64;
    }
    if (s.m, s.d) > (m.m, m.d) {
        years -= 1;
    }
    let whole = years as f64;
    let ay = m.y - years;
    let r = match basis {
        0 => {
            let feb_end = |y: i64, mo: u32, d: u32| mo == 2 && d >= dim(y, 2);
            let (mut sd, mut ed) = (s.d as i64, m.d as i64);
            if feb_end(ay, m.m, m.d) && feb_end(s.y, s.m, s.d) {
                ed = 30;
            }
            if sd == 31 || feb_end(s.y, s.m, s.d) {
                sd = 30;
            }

            if ed == 31 && sd >= 30 {
                ed = 30;
            }
            x87::div(d360(sd, s.m, s.y, ed, m.m, ay) as f64, 360.0)
        }
        4 => {
            let d = d360(
                (s.d as i64).min(30),
                s.m,
                s.y,
                (m.d as i64).min(30),
                m.m,
                ay,
            ) as f64;
            x87::div(d, 360.0)
        }
        _ => {
            let leap = is_leap(m.y);
            let sy = s.y + years;
            let last = dim(sy, s.m);
            let from = serial(sys, sy, s.m, s.d.min(last)) + (s.d.saturating_sub(last)) as i64;
            let days = mat - from;
            let b = match basis {
                1 if leap => 366.0,
                1 | 3 => 365.0,
                _ => 360.0,
            };
            x87::div(days as f64, b)
        }
    };

    x87::add(whole, r)
}

pub fn days360_eu(sys: DateSystem, a: i64, b: i64) -> i64 {
    let s = ymd(sys, a);
    let e = ymd(sys, b);
    d360(
        (s.d as i64).min(30),
        s.m,
        s.y,
        (e.d as i64).min(30),
        e.m,
        e.y,
    )
}

fn days360_fn_us(sys: DateSystem, a: i64, b: i64) -> i64 {
    let s = ymd(sys, a);
    let e = ymd(sys, b);
    let mut sd = s.d as i64;
    let mut ed = e.d as i64;
    if sd == 31 || last_of_feb(sys, a) {
        sd = 30;
    }
    if ed == 31 && sd >= 30 {
        ed = 30;
    }
    d360(sd, s.m, s.y, ed, e.m, e.y)
}

pub fn actual_year_length(sys: DateSystem, a: i64, b: i64) -> f64 {
    actual_year_length_ymd(ymd(sys, a), ymd(sys, b), false)
}

fn actual_year_length_ymd(s: Ymd, e: Ymd, leap1900: bool) -> f64 {
    let leap = |y: i64| is_leap(y) || (leap1900 && y == 1900);
    let le_one_year = s.y == e.y || (s.y + 1 == e.y && (s.m > e.m || (s.m == e.m && s.d >= e.d)));
    if le_one_year {
        if s.y == e.y && leap(s.y) {
            return 366.0;
        }
        let feb29_between = (leap(s.y) && s.m < 3) || (leap(e.y) && e.m >= 3);

        if feb29_between || (e.m == 2 && e.d == 29 && is_leap(e.y)) {
            366.0
        } else {
            365.0
        }
    } else {
        let years = (e.y - s.y + 1) as f64;
        let days: i64 = (s.y..=e.y).map(|y| if leap(y) { 366 } else { 365 }).sum();
        days as f64 / years
    }
}

pub fn yearfrac_core(sys: DateSystem, a: i64, b: i64, basis: i32) -> R<f64> {
    yearfrac_ymd(a, (ymd(sys, a), false), b, (ymd(sys, b), false), basis)
}

fn yearfrac_ymd(a: i64, ya: (Ymd, bool), b: i64, yb: (Ymd, bool), basis: i32) -> R<f64> {
    let ((a, (s, sf)), (b, (e, ef))) = if a > b {
        ((b, yb), (a, ya))
    } else {
        ((a, ya), (b, yb))
    };
    if a == b {
        return Ok(0.0);
    }
    let month_end = |t: Ymd| t.d == dim(t.y, t.m);
    let back = |t: Ymd, f: bool| {
        if f {
            Ymd {
                y: t.y - 4,
                m: t.m,
                d: t.d.saturating_sub(1),
            }
        } else {
            t
        }
    };
    Ok(match basis {
        0 => {
            let (mut d1, mut d2) = (s.d as i64, e.d as i64);
            if d1 == 31 && d2 == 31 {
                d1 = 30;
                d2 = 30;
            } else if d1 == 31 {
                d1 = 30;
            } else if d1 == 30 && d2 == 31 {
                d2 = 30;
            } else if s.m == 2 && e.m == 2 && month_end(s) && month_end(e) {
                d1 = 30;
                d2 = 30;
            } else if s.m == 2 && month_end(s) {
                d1 = 30;
            }
            let (bs, be) = (back(s, sf), back(e, ef));
            d360(
                d1 - (s.d - bs.d) as i64,
                s.m,
                bs.y,
                d2 - (e.d - be.d) as i64,
                e.m,
                be.y,
            ) as f64
                / 360.0
        }
        1 => (b - a) as f64 / actual_year_length_ymd(back(s, sf), back(e, ef), sf || ef),
        2 => (b - a) as f64 / 360.0,
        3 => (b - a) as f64 / 365.0,
        _ => {
            let (bs, be) = (back(s, sf), back(e, ef));
            d360(
                (s.d as i64).min(30) - (s.d - bs.d) as i64,
                s.m,
                bs.y,
                (e.d as i64).min(30) - (e.d - be.d) as i64,
                e.m,
                be.y,
            ) as f64
                / 360.0
        }
    })
}

pub fn yearfrac(ctx: &mut EvalCtx, a: &[Arg]) -> Value {
    atp::out((|| {
        if let Some(Some(Value::Error(e))) = a.get(2) {
            return Err(*e);
        }
        let s = atp::date(ctx, a, 0)?;
        let e = atp::date(ctx, a, 1)?;
        let basis = atp::basis(ctx, a, 2)?;

        let sys = atp::date_system(ctx);
        if sys == DateSystem::D1904 && (s > 2_955_541 || e > 2_955_541) {
            return Err(XlError::Num);
        }

        let civil = |i: usize, x: i64| {
            let shift = a
                .get(i)
                .and_then(|v| v.as_ref())
                .map_or(0.0, |v| ctx.atp_text_shift(v));
            (ymd(sys, x + shift as i64), shift != 0.0)
        };
        yearfrac_ymd(s, civil(0, s), e, civil(1, e), basis)
    })())
}
pub fn days360(ctx: &mut EvalCtx, a: &[Value]) -> Value {
    val((|| {
        let sys = ctx.wb.settings.date_system;

        let day = |x: f64| ((x * 86400.0).round() / 86400.0).floor() as i64;
        let s = day(crate::functions::datetime::serial_arg(ctx, &a[0])?);
        let e = day(crate::functions::datetime::serial_arg(ctx, &a[1])?);
        let european = match a.get(2) {
            None => false,
            Some(v) => boolean(v)?,
        };
        let _ = num;
        let d = if european {
            days360_eu(sys, s, e)
        } else {
            days360_fn_us(sys, s, e)
        };
        Ok(Value::Number(d as f64))
    })())
}

pub fn add_months(y: i64, m: u32, d: u32, k: i64, eom: bool) -> (i64, u32, u32) {
    let t = y * 12 + (m as i64 - 1) + k;
    let (ny, nm) = (t.div_euclid(12), (t.rem_euclid(12) + 1) as u32);
    let last = dim(ny, nm);
    (ny, nm, if eom { last } else { d.min(last) })
}

#[derive(Clone, Copy, Debug)]
pub struct Coupons {
    pub pcd: i64,

    pub ncd: i64,

    pub num: i64,
}

pub fn coupons(sys: DateSystem, settle: i64, mat: i64, freq: i32) -> Coupons {
    let m = ymd(sys, mat);
    let eom = m.d >= dim(m.y, m.m);
    let months = (12 / freq) as i64;
    let s = ymd(sys, settle);

    let span = (m.y * 12 + m.m as i64) - (s.y * 12 + s.m as i64);
    let mut k = (span / months - 1).max(1);
    let date_k = |k: i64| {
        let (y, mo, d) = add_months(m.y, m.m, m.d, -k * months, eom);
        serial(sys, y, mo, d)
    };
    while k > 1 && date_k(k - 1) <= settle {
        k -= 1;
    }
    while date_k(k) > settle {
        k += 1;
    }
    let pcd = date_k(k);
    let ncd = if k == 1 { mat } else { date_k(k - 1) };
    Coupons { pcd, ncd, num: k }
}

fn coupdays_actual(sys: DateSystem, c: &Coupons, mat: i64, freq: i32) -> f64 {
    let months = (12 / freq) as i64;
    let n = ymd(sys, c.ncd);
    let p = ymd(sys, c.pcd);

    let n_eom = n.d >= dim(n.y, n.m) && !(n.y == 1900 && n.m == 2 && sys == DateSystem::D1900);
    let (sy, sm, sd) = add_months(n.y, n.m, n.d, -months, n_eom);
    if sy < 1900 && sys == DateSystem::D1900 {
        return (civil_days(n.y, n.m, n.d) - civil_days(sy, sm, sd)) as f64;
    }
    let start = serial(sys, sy, sm, sd);
    let end = if c.ncd == mat {
        mat
    } else {
        let day = if sys == DateSystem::D1900 && c.pcd == 59 && mat <= 366 {
            ymd(sys, mat).d
        } else {
            p.d
        };
        let (ey, em, ed) = add_months(p.y, p.m, day, months, false);
        serial(sys, ey, em, ed)
    };
    (end - start) as f64
}

fn civil_days(y: i64, m: u32, d: u32) -> i64 {
    let (y, m) = if m <= 2 {
        (y - 1, m as i64 + 9)
    } else {
        (y, m as i64 - 3)
    };
    let era = y.div_euclid(400);
    let yoe = y - era * 400;
    let doy = (153 * m + 2) / 5 + d as i64 - 1;
    era * 146097 + yoe * 365 + yoe / 4 - yoe / 100 + doy
}

#[derive(Clone, Copy)]
pub struct Basis {
    pub sys: DateSystem,
    pub basis: i32,
}

impl Basis {
    pub fn days(&self, a: i64, b: i64) -> f64 {
        match self.basis {
            0 => days360_us(self.sys, a, b, false) as f64,
            4 => days360_eu(self.sys, a, b) as f64,
            _ => (b - a) as f64,
        }
    }

    pub fn year(&self, a: i64, b: i64) -> f64 {
        match self.basis {
            1 => actual_year_length(self.sys, a.min(b), a.max(b)),
            3 => 365.0,
            _ => 360.0,
        }
    }

    pub fn coupdays(&self, c: &Coupons, mat: i64, freq: i32) -> f64 {
        match self.basis {
            1 => coupdays_actual(self.sys, c, mat, freq),
            3 => 365.0 / freq as f64,
            _ => 360.0 / freq as f64,
        }
    }

    pub fn coupdaybs(&self, c: &Coupons, settle: i64) -> f64 {
        match self.basis {
            0 => days360_us(self.sys, c.pcd, settle, false) as f64,
            4 => days360_eu(self.sys, c.pcd, settle) as f64,
            _ => (settle - c.pcd) as f64,
        }
    }

    pub fn coupdaysnc(&self, c: &Coupons, settle: i64) -> f64 {
        match self.basis {
            0 => {
                (days360_us(self.sys, c.pcd, c.ncd, true)
                    - days360_us(self.sys, c.pcd, settle, false)) as f64
            }
            4 => days360_eu(self.sys, settle, c.ncd) as f64,
            _ => (c.ncd - settle) as f64,
        }
    }
}

fn coup_args(ctx: &EvalCtx, a: &[Arg]) -> R<(Basis, i64, i64, i32, Coupons)> {
    let settle = atp::date(ctx, a, 0)?;
    let mat = atp::date(ctx, a, 1)?;
    let freq = atp::frequency(atp::req(ctx, a, 2)?)?;
    let basis = atp::basis(ctx, a, 3)?;
    if settle >= mat {
        return Err(XlError::Num);
    }
    let sys = atp::date_system(ctx);
    Ok((
        Basis { sys, basis },
        settle,
        mat,
        freq,
        coupons(sys, settle, mat, freq),
    ))
}

fn clamp_pcd(c: Coupons) -> Coupons {
    Coupons {
        pcd: c.pcd.max(0),
        ..c
    }
}

pub fn coupdaybs(ctx: &mut EvalCtx, a: &[Arg]) -> Value {
    atp::out(coup_args(ctx, a).map(|(b, s, _, _, c)| b.coupdaybs(&clamp_pcd(c), s)))
}

pub fn coupdays(ctx: &mut EvalCtx, a: &[Arg]) -> Value {
    atp::out(coup_args(ctx, a).map(|(b, _, m, f, c)| b.coupdays(&c, m, f)))
}

pub fn coupdaysnc(ctx: &mut EvalCtx, a: &[Arg]) -> Value {
    atp::out(coup_args(ctx, a).map(|(b, s, _, _, c)| b.coupdaysnc(&clamp_pcd(c), s)))
}

pub fn coupncd(ctx: &mut EvalCtx, a: &[Arg]) -> Value {
    atp::out(coup_args(ctx, a).map(|(_, _, _, _, c)| c.ncd as f64))
}

pub fn couppcd(ctx: &mut EvalCtx, a: &[Arg]) -> Value {
    atp::out(coup_args(ctx, a).map(|(_, _, _, _, c)| clamp_pcd(c).pcd as f64))
}

pub fn coupnum(ctx: &mut EvalCtx, a: &[Arg]) -> Value {
    atp::out(coup_args(ctx, a).and_then(|(_, _, _, _, c)| {
        if c.pcd < 1 {
            Err(XlError::Num)
        } else {
            Ok(c.num as f64)
        }
    }))
}
