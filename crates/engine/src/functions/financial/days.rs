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

fn d360(sd: i64, sm: u32, sy: i64, ed: i64, em: u32, ey: i64) -> i64 {
    (ey - sy) * 360 + (em as i64 - sm as i64) * 30 + (ed - sd)
}

pub fn days360_nasd(sys: DateSystem, a: i64, b: i64, both: bool) -> i64 {
    let s = ymd(sys, a);
    let e = ymd(sys, b);
    let feb_end = |t: Ymd| t.m == 2 && t.d == dim(t.y, 2);
    let (mut sd, mut ed) = (s.d as i64, e.d as i64);
    if feb_end(e) && (feb_end(s) || both) {
        ed = 30;
    }
    if ed == 31 && (sd >= 30 || both) {
        ed = 30;
    }
    if sd == 31 {
        sd = 30;
    }
    if feb_end(s) {
        sd = 30;
    }
    d360(sd, s.m, s.y, ed, e.m, e.y)
}

pub fn days360_eu(sys: DateSystem, a: i64, b: i64) -> i64 {
    let s = ymd(sys, a);
    let e = ymd(sys, b);
    let day = |d: u32| if d == 31 { 30 } else { d as i64 };
    d360(day(s.d), s.m, s.y, day(e.d), e.m, e.y)
}

fn days365(sys: DateSystem, a: i64, b: i64) -> i64 {
    let s = ymd(sys, a);
    let e = ymd(sys, b);
    let day = |t: Ymd| if t.m == 2 && t.d > 28 { 28 } else { t.d };
    let in_1905 = |t: Ymd| serial(sys, 1905, t.m, day(t));
    (e.y - s.y) * 365 + in_1905(e) - in_1905(s)
}

pub fn count_days(sys: DateSystem, a: i64, b: i64, basis: i32, both: bool, plain: bool) -> i64 {
    match basis {
        0 => days360_nasd(sys, a, b, both),
        2 if !plain => days360_nasd(sys, a, b, both),
        3 if !plain => days365(sys, a, b),
        4 => days360_eu(sys, a, b),
        _ => b - a,
    }
}

pub fn annual_basis(sys: DateSystem, basis: i32, a: i64, b: i64) -> f64 {
    match basis {
        1 => {
            let (s, e) = (ymd(sys, a), ymd(sys, b));
            let year_len = |y: i64| if is_leap(y) { 366.0 } else { 365.0 };
            if s.y == e.y {
                return year_len(s.y);
            }
            if e.y - 1 != s.y || (s.m, s.d) < (e.m, e.d) {
                let total: f64 = (s.y..=e.y).map(year_len).sum();
                return x87::div(total, (e.y - s.y + 1) as f64);
            }
            let feb29 = |y: i64| dmy_to_serial(sys, 29, 2, y);
            if is_leap(s.y) {
                if feb29(s.y).is_some_and(|f| f >= a) {
                    366.0
                } else {
                    365.0
                }
            } else if is_leap(e.y) && feb29(e.y).is_some_and(|f| f <= b) {
                366.0
            } else {
                365.0
            }
        }
        3 => 365.0,
        _ => 360.0,
    }
}

pub fn diff_years(sys: DateSystem, basis: i32, settle: i64, mat: i64) -> Option<f64> {
    if settle >= mat {
        return None;
    }
    let s = date_parts(sys, settle)?;
    let m = date_parts(sys, mat)?;
    let reached = (m.m, m.d) >= (s.m, s.d);
    let years = if reached { m.y - s.y } else { m.y - s.y - 1 };
    if reached && (m.m, m.d) == (s.m, s.d) {
        return Some(years as f64);
    }
    let anniversary_year = if reached { m.y } else { m.y - 1 };
    let anniversary = dmy_to_serial(sys, s.d as i64, s.m, anniversary_year)?;
    let dsm = diff_dates(sys, anniversary, mat, basis, 0)?;
    let length = match basis {
        1 if is_leap(m.y) => 366.0,
        1 | 3 => 365.0,
        0 | 2 | 4 => 360.0,
        _ => return None,
    };

    Some(x87::add(x87::div(dsm, length), years as f64))
}

pub fn yearfrac_core(sys: DateSystem, a: i64, b: i64, basis: i32) -> f64 {
    let (a, b) = if a > b { (b, a) } else { (a, b) };
    let dsm = diff_dates(sys, a, b, basis, 0).unwrap_or(0.0);
    x87::div(dsm, annual_basis(sys, basis, a, b))
}

pub fn yearfrac(ctx: &mut EvalCtx, a: &[Arg]) -> Value {
    atp::out((|| {
        if let Some(Some(Value::Error(e))) = a.get(2) {
            return Err(*e);
        }

        if atp::opt(ctx, a, 2, 0.0)? < 0.0 {
            return Err(XlError::NUM);
        }
        let basis = atp::basis(ctx, a, 2)?;
        let s = atp::kernel_date(ctx, a, 0)?;
        let e = atp::kernel_date(ctx, a, 1)?;

        if atp::date_system(ctx) == DateSystem::D1904
            && (s > dt::MAX_SERIAL_1904 as i64 || e > dt::MAX_SERIAL_1904 as i64)
        {
            return Err(XlError::NUM);
        }
        Ok(yearfrac_core(atp::date_system(ctx), s, e, basis))
    })())
}

fn days360_fn(sys: DateSystem, a: i64, b: i64, european: bool) -> i64 {
    let s = ymd(sys, a);
    let e = ymd(sys, b);
    let lotus = sys != DateSystem::D1900Gregorian;
    let feb_last = |t: Ymd| {
        if is_leap(t.y) || lotus && t.y == 1900 {
            29
        } else {
            28
        }
    };
    let mut sd = s.d as i64;
    if sd == 31 || (!european && s.m == 2 && sd == feb_last(s)) {
        sd = 30;
    }
    let mut ed = e.d as i64;
    if ed == 31 && (european || sd == 30) {
        ed = 30;
    }
    d360(sd, s.m, s.y, ed, e.m, e.y)
}

pub fn days360(ctx: &mut EvalCtx, a: &[Value]) -> Value {
    val((|| {
        let sys = ctx.wb.settings.date_system;

        let day = |x: f64| ((x * 86400.0).round() / 86400.0).floor() as i64;

        let (n0, n1) = (ctx.to_number(&a[0]), ctx.to_number(&a[1]));
        let european = match a.get(2) {
            None => Ok(false),
            Some(v) => boolean(v),
        };
        let (n0, n1) = (n0?, n1?);
        let european = european?;
        let range = |x: f64| crate::functions::datetime::serial_arg(ctx, &Value::Number(x));
        let s = day(range(n0)?);
        let e = day(range(n1)?);
        let _ = num;
        Ok(Value::Number(days360_fn(sys, s, e, european) as f64))
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

    pub num: i64,
}

#[derive(Clone, Copy)]
pub struct Basis {
    pub sys: DateSystem,
    pub basis: i32,
}

impl Basis {
    pub fn year(&self, a: i64, b: i64) -> f64 {
        annual_basis(self.sys, self.basis, a, b)
    }
}

fn date_parts(sys: DateSystem, s: i64) -> Option<Ymd> {
    let max = if sys == DateSystem::D1904 {
        2_957_003
    } else {
        2_958_465
    };
    (0..=max).contains(&s).then(|| ymd(sys, s))
}

fn dmy_to_serial(sys: DateSystem, d: i64, m: u32, y: i64) -> Option<i64> {
    let gregorian = sys == DateSystem::D1900Gregorian;
    let min_year = match sys {
        DateSystem::D1904 => 1904,
        _ if gregorian => -9999,
        _ => 1900,
    };
    if !(min_year..10_000).contains(&y) || !(1..=12).contains(&m) || d < 0 {
        return None;
    }
    if gregorian && (d == 0 || (y, m, d) == (1900, 2, 29)) {
        return None;
    }
    let s = if sys == DateSystem::D1900 && (y, m, d) == (1900, 2, 29) {
        60
    } else {
        dt::serial_from_ymd(sys, y, m as i64, d)
    };
    date_parts(sys, s).map(|_| s)
}

fn is_end_of_month(p: Ymd) -> bool {
    p.d == dim(p.y, p.m)
}

pub fn month_end_code(sys: DateSystem, s: i64) -> i64 {
    let Some(p) = date_parts(sys, s) else {
        return -1;
    };
    let day = p.d as i64;
    if p.m == 2 {
        return (p.d == dim(p.y, 2)) as i64;
    }
    let last = if matches!(p.m, 4 | 6 | 9 | 11) {
        30
    } else {
        31
    };
    if day == last {
        1
    } else if day > 28 {
        day
    } else {
        0
    }
}

pub fn change_month(sys: DateSystem, date: i64, months: i64, basis: i32, eom: i64) -> (bool, i64) {
    let Some(p) = date_parts(sys, date) else {
        return (false, 0);
    };
    let day = p.d as i64;
    let src_eom = match day {
        28 => p.m == 2 && !is_leap(p.y),
        29 => p.m == 2,
        30 => basis == 0 || matches!(p.m, 4 | 6 | 9 | 11),
        31 => true,
        _ => false,
    };
    let t = p.m as i64 - 1 + months;
    let y = p.y + t.div_euclid(12);
    let m = (t.rem_euclid(12) + 1) as u32;
    let feb = if is_leap(y) { 29 } else { 28 };
    let mut nd = day;
    if m == 2 {
        if day > 28 {
            nd = feb;
        }
    } else if matches!(m, 4 | 6 | 9 | 11) && day > 30 {
        nd = 30;
    }
    if eom != 0 {
        nd = if m == 2 {
            feb
        } else {
            let last = if matches!(m, 4 | 6 | 9 | 11) { 30 } else { 31 };
            if eom == 1 {
                last
            } else if src_eom {
                eom
            } else {
                nd
            }
        };
    }

    if y < if sys == DateSystem::D1900Gregorian {
        -9999
    } else {
        1900
    } {
        return (false, 0);
    }
    match dmy_to_serial(sys, nd, m, y) {
        Some(s) => (true, s),
        None => (false, 0),
    }
}

fn disp_month(sys: DateSystem, base: i64, months: i64, eom: bool, ordered: bool) -> Option<i64> {
    let p = date_parts(sys, base)?;
    let sum = p.m as i64 + months;
    let y = p.y + sum.div_euclid(12);
    let m = sum.rem_euclid(12) as u32;
    let mut day = p.d as i64;
    if m == 2 {
        let feb = dim(y, 2) as i64;
        if day >= feb || eom {
            day = feb;
        }
    } else if matches!(m, 4 | 6 | 9 | 11) {
        if day >= 31 || eom {
            day = 30;
        }
    } else if eom {
        day = 31;
    }
    let (y, m) = if m == 0 { (y - 1, 12) } else { (y, m) };
    let r = dmy_to_serial(sys, day, m, y)?;
    let in_order = if months < 0 { r <= base } else { r >= base };
    (in_order || !ordered).then_some(r)
}

pub fn bond_coupons(sys: DateSystem, settle: i64, mat: i64, months: i64) -> Option<Coupons> {
    coupon_steps(sys, settle, mat, months, true)
}

fn coupon_steps(
    sys: DateSystem,
    settle: i64,
    mat: i64,
    months: i64,
    ordered: bool,
) -> Option<Coupons> {
    let eom = is_end_of_month(date_parts(sys, mat)?);
    let (mut counter, mut date) = (0, mat);
    while settle < date {
        counter -= months;
        date = disp_month(sys, mat, counter, eom, ordered)?;
    }
    Some(Coupons {
        pcd: date,
        num: -counter / months,
    })
}

pub fn calc_periods(
    sys: DateSystem,
    first: i64,
    settle: i64,
    months: i64,
    basis: i32,
    op0: bool,
) -> Option<f64> {
    if first == settle {
        return Some(0.0);
    }
    let mut count = if op0 { 1.0 } else { 0.0 };
    let mut code = month_end_code(sys, first);
    if code > 1 {
        code = month_end_code(sys, settle);
    }
    let basis = if basis != 0 { basis } else { 1 };
    let (_, mut cur) = change_month(sys, settle, 0, basis, code);
    if settle < cur {
        count = x87::add(count, 1.0);
    }
    loop {
        let (ok, next) = change_month(sys, cur, months, basis, code);
        cur = next;
        if !ok || cur >= first {
            break;
        }
        count = x87::add(count, 1.0);
    }
    Some(count)
}

#[derive(Clone, Copy, PartialEq, Eq, Debug)]
pub enum CoupOp {
    Days,

    DaysNc,

    DayBs,

    Pcd,

    Ncd,
}

pub fn diff_dates(sys: DateSystem, a: i64, b: i64, basis: i32, flag: i32) -> Option<f64> {
    match a.cmp(&b) {
        std::cmp::Ordering::Greater => None,
        std::cmp::Ordering::Equal => Some(0.0),
        std::cmp::Ordering::Less => match basis {
            0..=4 => Some(count_days(sys, a, b, basis, flag == 3, true) as f64),
            _ => None,
        },
    }
}

pub fn calc_coup(
    sys: DateSystem,
    mat: i64,
    settle: i64,
    months: i64,
    basis: i32,
    op: CoupOp,
) -> Option<f64> {
    let eom = month_end_code(sys, mat);
    let (mut ok, mut r) = change_month(sys, mat, -months, basis, eom);
    let mut ncd = mat;
    while ok && r > settle {
        ncd = r;
        (ok, r) = change_month(sys, r, -months, basis, eom);
    }
    let pcd = r;
    match op {
        CoupOp::Days => match basis {
            1 => diff_dates(sys, pcd, ncd, basis, 3),
            3 => Some(x87::div(365.0, x87::div(12.0, months as f64))),
            _ => Some(x87::div(360.0, x87::div(12.0, months as f64))),
        },
        CoupOp::DaysNc => {
            let bs = diff_dates(sys, pcd, settle, basis, 1)?;
            Some(x87::sub(diff_dates(sys, pcd, ncd, basis, 3)?, bs))
        }
        CoupOp::DayBs => diff_dates(sys, pcd, settle, basis, 1),
        CoupOp::Pcd => Some(pcd as f64),
        CoupOp::Ncd => Some(ncd as f64),
    }
}

fn coupdays_from_next(
    sys: DateSystem,
    mat: i64,
    settle: i64,
    months: i64,
    basis: i32,
) -> Option<f64> {
    let (mut date, mut settle) = (mat, settle);
    loop {
        let (ok, r) = change_month(sys, date, -months, basis, 0);
        if !ok {
            settle += 730;
            date += 730;
        }
        if r <= settle {
            break;
        }
        date = r;
    }
    calc_coup(sys, date, settle, months, basis, CoupOp::Days)
}

fn coup_args(ctx: &EvalCtx, a: &[Arg]) -> R<(Basis, i64, i64, i32)> {
    let settle = atp::kernel_date(ctx, a, 0)?;
    let mat = atp::kernel_date(ctx, a, 1)?;

    if settle >= mat {
        return Err(XlError::NUM);
    }
    let freq = atp::frequency(atp::req(ctx, a, 2)?)?;
    let basis = atp::basis(ctx, a, 3)?;
    let sys = atp::date_system(ctx);
    Ok((Basis { sys, basis }, settle, mat, freq))
}

fn coup_op(ctx: &mut EvalCtx, a: &[Arg], op: CoupOp) -> Value {
    atp::out(coup_args(ctx, a).and_then(|(b, settle, mat, freq)| {
        let months = (12 / freq) as i64;
        let r = if op == CoupOp::Days {
            coupdays_from_next(b.sys, mat, settle, months, b.basis)
        } else {
            calc_coup(b.sys, mat, settle, months, b.basis, op)
        };

        let shift = if matches!(op, CoupOp::Pcd | CoupOp::Ncd) {
            atp::kernel_offset(ctx) as f64
        } else {
            0.0
        };
        r.map(|x| x - shift).ok_or(XlError::NUM)
    }))
}

pub fn coupdaybs(ctx: &mut EvalCtx, a: &[Arg]) -> Value {
    coup_op(ctx, a, CoupOp::DayBs)
}

pub fn coupdays(ctx: &mut EvalCtx, a: &[Arg]) -> Value {
    coup_op(ctx, a, CoupOp::Days)
}

pub fn coupdaysnc(ctx: &mut EvalCtx, a: &[Arg]) -> Value {
    coup_op(ctx, a, CoupOp::DaysNc)
}

pub fn coupncd(ctx: &mut EvalCtx, a: &[Arg]) -> Value {
    coup_op(ctx, a, CoupOp::Ncd)
}

pub fn couppcd(ctx: &mut EvalCtx, a: &[Arg]) -> Value {
    coup_op(ctx, a, CoupOp::Pcd)
}

pub fn coupnum(ctx: &mut EvalCtx, a: &[Arg]) -> Value {
    atp::out(coup_args(ctx, a).and_then(|(b, settle, mat, freq)| {
        let months = (12 / freq) as i64;
        let c = coupon_steps(b.sys, settle, mat, months, false).ok_or(XlError::NUM)?;
        Ok(c.num as f64)
    }))
}

#[cfg(test)]
mod tests {
    use super::*;

    const S: DateSystem = DateSystem::D1900;

    fn d(y: i64, m: u32, day: u32) -> i64 {
        serial(S, y, m, day)
    }

    #[test]
    fn nasd_serial_60_is_not_a_month_end() {
        assert_eq!(days360_nasd(S, 60, 90, false), 31);
        assert_eq!(days360_nasd(S, d(2004, 2, 29), d(2004, 3, 31), false), 31);
    }

    #[test]
    fn annual_basis_rules() {
        assert_eq!(annual_basis(S, 1, d(2004, 3, 1), d(2004, 9, 1)), 366.0);
        assert_eq!(annual_basis(S, 1, d(2003, 3, 1), d(2003, 9, 1)), 365.0);

        assert_eq!(annual_basis(S, 1, d(2003, 3, 1), d(2004, 3, 1)), 366.0);
        assert_eq!(annual_basis(S, 1, d(2003, 2, 28), d(2004, 2, 29)), 365.5);
        assert_eq!(
            annual_basis(S, 1, d(2003, 1, 1), d(2005, 1, 1)),
            1096.0 / 3.0
        );
        assert_eq!(annual_basis(S, 3, 1, 2), 365.0);
        assert_eq!(annual_basis(S, 4, 1, 2), 360.0);
    }

    #[test]
    fn diff_years_whole_and_fractional() {
        assert_eq!(diff_years(S, 0, d(2000, 5, 3), d(2003, 5, 3)), Some(3.0));
        assert_eq!(
            diff_years(S, 3, d(2000, 5, 3), d(2003, 5, 13)),
            Some(3.0 + 10.0 / 365.0)
        );
        assert_eq!(diff_years(S, 0, d(2003, 5, 3), d(2000, 5, 3)), None);
    }

    #[test]
    fn calendar_limits() {
        assert_eq!(dmy_to_serial(S, 29, 2, 1900), Some(60));
        assert_eq!(dmy_to_serial(S, 1, 1, 1900), Some(1));
        assert_eq!(dmy_to_serial(S, 31, 12, 1899), None);
        assert_eq!(dmy_to_serial(S, 31, 12, 9999), Some(2_958_465));
        assert_eq!(dmy_to_serial(S, 1, 1, 10_000), None);
    }

    #[test]
    fn change_month_before_1900_fails() {
        assert_eq!(change_month(S, d(1900, 3, 15), -6, 1, 0), (false, 0));
        assert_eq!(
            change_month(S, d(1900, 3, 15), -2, 1, 0),
            (true, d(1900, 1, 15))
        );
    }

    #[test]
    fn month_end_chain_keeps_end_of_month() {
        let (mat, settle) = (d(2021, 2, 28), d(2020, 6, 15));
        let c = |op| calc_coup(S, mat, settle, 6, 1, op);
        assert_eq!(c(CoupOp::Ncd), Some(d(2020, 8, 31) as f64));
        assert_eq!(c(CoupOp::Pcd), Some(d(2020, 2, 29) as f64));
    }

    #[test]
    fn pre_1900_period_start_reads_zero() {
        let (mat, settle) = (d(1900, 3, 15), 5);
        assert_eq!(calc_coup(S, mat, settle, 6, 1, CoupOp::Pcd), Some(0.0));
        assert!(bond_coupons(S, settle, mat, 6).is_none());
    }
}
