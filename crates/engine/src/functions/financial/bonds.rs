use super::atp::{self, Arg};
use super::days::{self, Basis, CoupOp, Coupons};
use crate::eval::EvalCtx;
use crate::functions::args::{boolean, R};
use crate::value::{Value, XlError};
use crate::{x87, xmath};

fn finite(x: f64) -> R<f64> {
    if x.is_finite() {
        Ok(x)
    } else {
        Err(XlError::NUM)
    }
}

fn mp_store(x: f64) -> f64 {
    if x.is_finite() {
        x
    } else {
        0.0
    }
}

fn basis_of(ctx: &EvalCtx, a: &[Arg], i: usize) -> R<Basis> {
    Ok(Basis {
        sys: atp::date_system(ctx),
        basis: atp::basis(ctx, a, i)?,
    })
}

pub fn accrint(ctx: &mut EvalCtx, a: &[Arg]) -> Value {
    atp::out((|| {
        let calc_method = match a.get(7) {
            Some(Some(v)) => match v {
                Value::Blank => false,

                Value::Text(_) => return Err(XlError::VALUE),
                v => boolean(v)?,
            },
            _ => true,
        };
        let issue = atp::date(ctx, a, 0)?;
        let first = atp::date(ctx, a, 1)?;
        let settle = atp::date(ctx, a, 2)?;
        let rate = atp::req(ctx, a, 3)?;
        let par = atp::opt(ctx, a, 4, 1000.0)?;
        let freq = atp::frequency(atp::req(ctx, a, 5)?)?;
        let b = basis_of(ctx, a, 6)?;
        if issue < 1 || first < 1 {
            return Err(XlError::NUM);
        }
        if rate <= 0.0 || par <= 0.0 || issue >= settle {
            return Err(XlError::NUM);
        }

        let sys = b.sys;
        let months = (12 / freq) as i64;
        let f = days::ymd(sys, first);

        let eom = f.d == days::dim(f.y, f.m);

        let out_of_range = std::cell::Cell::new(false);
        let adj = |s: i64, k: i64| {
            let d = days::ymd(sys, s);
            let (y, m, dd) = days::add_months(d.y, d.m, d.d, k, eom);
            let r = days::serial(sys, y, m, dd);
            if !(1..=2_958_465).contains(&r) {
                out_of_range.set(true);
            }
            r
        };

        let cnt = |x: i64, y: i64, period: bool, count: bool| -> f64 {
            days::count_days(sys, x, y, b.basis, period, b.basis != 2 || count) as f64
        };
        let pcd = if settle > first && calc_method {
            let (mut pcd, mut cur) = (first, first);
            while cur < settle {
                pcd = cur;
                cur = adj(cur, months);
            }
            pcd
        } else {
            adj(first, -months)
        };
        let fq = x87::div(12.0, months as f64);
        let e = match b.basis {
            1 => (first - adj(first, -months)) as f64,
            3 => x87::div(365.0, fq),
            _ => x87::div(360.0, fq),
        };
        let mut acc = x87::div(cnt(issue.max(pcd), settle, false, true), e);
        let mut cur = pcd;
        while cur > issue {
            let prev = adj(cur, -months);
            if issue > prev {
                let d = cnt(issue, cur, false, true);
                let e2 = if b.basis == 3 {
                    x87::div(365.0, fq)
                } else {
                    cnt(prev, cur, true, false)
                };
                acc = x87::add(x87::div(d, e2), acc);
            } else if calc_method {
                acc = x87::add(acc, 1.0);
            }
            cur = prev;
        }
        if out_of_range.get() {
            return Err(XlError::NUM);
        }

        Ok(x87::mul(
            mp_store(x87::mul(par, x87::div(rate, freq as f64))),
            acc,
        ))
    })())
}

pub fn accrintm(ctx: &mut EvalCtx, a: &[Arg]) -> Value {
    atp::out((|| {
        let issue = atp::kernel_date(ctx, a, 0)?;
        let settle = atp::kernel_date(ctx, a, 1)?;
        let rate = atp::req(ctx, a, 2)?;
        let par = atp::opt(ctx, a, 3, 1000.0)?;
        let b = basis_of(ctx, a, 4)?;
        if issue > settle || par <= 0.0 || rate <= 0.0 {
            return Err(XlError::NUM);
        }
        let dsm = days::diff_dates(b.sys, issue, settle, b.basis, 0).ok_or(XlError::NUM)?;

        let mp = Mp::default();
        let yf = mp.div(dsm, b.year(issue, settle));
        Ok(mp.mul(mp.mul(par, rate), yf))
    })())
}

#[derive(Default)]
struct Mp(std::cell::RefCell<x87::MathFlag>);

impl Mp {
    fn add(&self, a: f64, b: f64) -> f64 {
        self.0.borrow_mut().add(a, b)
    }
    fn sub(&self, a: f64, b: f64) -> f64 {
        self.0.borrow_mut().sub(a, b)
    }
    fn mul(&self, a: f64, b: f64) -> f64 {
        self.0.borrow_mut().mul(a, b)
    }
    fn div(&self, a: f64, b: f64) -> f64 {
        self.0.borrow_mut().div(a, b)
    }
    fn sqrt(&self, a: f64) -> f64 {
        self.0.borrow_mut().sqrt(a)
    }
    fn clear(&self) {
        self.0.borrow_mut().0 = 0;
    }

    fn code(&self) -> u8 {
        self.0.borrow().0
    }
}

fn negate_nonzero(x: f64) -> f64 {
    if x.to_bits() >> 48 != 0 {
        -x
    } else {
        x
    }
}

fn sign_bit(x: f64) -> bool {
    (x.to_bits() as i64) < 0
}

fn disc_args(ctx: &EvalCtx, a: &[Arg]) -> R<(i64, i64, f64, f64, Basis)> {
    let b = basis_of(ctx, a, 4)?;
    let settle = atp::kernel_date(ctx, a, 0)?;
    let mat = atp::kernel_date(ctx, a, 1)?;
    let x = atp::req(ctx, a, 2)?;
    let y = atp::req(ctx, a, 3)?;
    if settle >= mat || x <= 0.0 || y <= 0.0 {
        return Err(XlError::NUM);
    }
    Ok((settle, mat, x, y, b))
}

fn securities(
    ctx: &EvalCtx,
    a: &[Arg],
    body: impl FnOnce(&Mp, f64, f64, f64, f64) -> R<f64>,
) -> R<f64> {
    let (s, m, x, y, b) = disc_args(ctx, a)?;

    let d = days::diff_dates(b.sys, s, m, b.basis, 2).ok_or(XlError::NUM)?;
    let by = b.year(s, m);
    let mp = Mp::default();
    let r = body(&mp, x, y, d, by)?;
    if mp.code() != 0 {
        return Err(XlError::NUM);
    }
    Ok(r)
}

pub fn disc(ctx: &mut EvalCtx, a: &[Arg]) -> Value {
    atp::out(securities(ctx, a, |mp, pr, redemption, d, by| {
        let q = mp.add(negate_nonzero(mp.div(pr, redemption)), 1.0);
        Ok(mp.mul(mp.div(by, d), q))
    }))
}

fn rate_over_x(mp: &Mp, x: f64, redemption: f64, d: f64, by: f64) -> f64 {
    let t = mp.div(mp.sub(redemption, x), x);
    mp.mul(mp.div(by, d), t)
}

pub fn intrate(ctx: &mut EvalCtx, a: &[Arg]) -> Value {
    atp::out(securities(ctx, a, |mp, inv, redemption, d, by| {
        Ok(rate_over_x(mp, inv, redemption, d, by))
    }))
}

pub fn yielddisc(ctx: &mut EvalCtx, a: &[Arg]) -> Value {
    atp::out(securities(ctx, a, |mp, pr, redemption, d, by| {
        Ok(rate_over_x(mp, pr, redemption, d, by))
    }))
}

pub fn pricedisc(ctx: &mut EvalCtx, a: &[Arg]) -> Value {
    atp::out(securities(ctx, a, |mp, discount, redemption, d, by| {
        let t = mp.div(mp.mul(mp.mul(discount, redemption), d), by);
        Ok(mp.add(negate_nonzero(t), redemption))
    }))
}

pub fn received(ctx: &mut EvalCtx, a: &[Arg]) -> Value {
    atp::out(securities(ctx, a, |mp, inv, discount, d, by| {
        let q = mp.div(mp.mul(discount, d), by);
        if (q.to_bits() as i64) >= 1.0f64.to_bits() as i64 {
            return Err(XlError::NUM);
        }
        Ok(mp.div(inv, mp.sub(1.0, q)))
    }))
}

fn mat_args(ctx: &EvalCtx, a: &[Arg]) -> R<(i64, i64, i64, f64, f64, Basis)> {
    let b = basis_of(ctx, a, 5)?;
    let settle = atp::kernel_date(ctx, a, 0)?;
    let mat = atp::kernel_date(ctx, a, 1)?;
    let issue = atp::kernel_date(ctx, a, 2)?;
    let rate = atp::req(ctx, a, 3)?;
    let x = atp::req(ctx, a, 4)?;
    if settle >= mat || issue >= settle || rate < 0.0 {
        return Err(XlError::NUM);
    }
    Ok((settle, mat, issue, rate, x, b))
}

fn mat_days(b: Basis, issue: i64, settle: i64, mat: i64) -> R<(f64, f64, f64)> {
    let ai = days::diff_dates(b.sys, issue, settle, b.basis, 0).ok_or(XlError::NUM)?;
    let dim = days::diff_dates(b.sys, issue, mat, b.basis, 2).ok_or(XlError::NUM)?;
    Ok((ai, dim, b.year(issue, settle)))
}

pub fn pricemat(ctx: &mut EvalCtx, a: &[Arg]) -> Value {
    atp::out((|| {
        let (s, m, issue, rate, yld, b) = mat_args(ctx, a)?;
        if yld < 0.0 {
            return Err(XlError::NUM);
        }
        let (ai, dim, by) = mat_days(b, issue, s, m)?;
        let mp = Mp::default();
        let dsm = mp.sub(dim, ai);

        let num = mp.add(mp.mul(mp.mul(mp.div(dim, by), rate), 100.0), 100.0);
        let den = mp.add(mp.mul(mp.div(dsm, by), yld), 1.0);
        let acc = mp.mul(mp.mul(mp.div(ai, by), rate), 100.0);
        Ok(mp.sub(mp.div(num, den), acc))
    })())
}

pub fn yieldmat(ctx: &mut EvalCtx, a: &[Arg]) -> Value {
    atp::out((|| {
        let (s, m, issue, rate, pr, b) = mat_args(ctx, a)?;
        if pr <= 0.0 {
            return Err(XlError::NUM);
        }
        let (ai, dim, by) = mat_days(b, issue, s, m)?;
        let mp = Mp::default();
        let dsm = mp.sub(dim, ai);

        let t2 = mp.add(mp.div(pr, 100.0), mp.mul(mp.div(ai, by), rate));
        let t1 = mp.sub(mp.add(mp.mul(mp.div(dim, by), rate), 1.0), t2);
        Ok(mp.mul(mp.div(t1, t2), mp.div(by, dsm)))
    })())
}

fn tbill_args(ctx: &EvalCtx, a: &[Arg]) -> R<(f64, f64)> {
    let settle = atp::kernel_date(ctx, a, 0)?;
    let mat = atp::kernel_date(ctx, a, 1)?;
    let x = atp::req(ctx, a, 2)?;
    if settle >= mat || x <= 0.0 {
        return Err(XlError::NUM);
    }
    let sys = atp::date_system(ctx);
    let (y, m, d) = crate::datetime::ymd_from_serial(sys, settle);
    let one_year = crate::datetime::serial_from_ymd(sys, y + 1, i64::from(m), i64::from(d));
    if mat > one_year {
        return Err(XlError::NUM);
    }
    Ok(((mat - settle) as f64, x))
}

pub fn tbillprice(ctx: &mut EvalCtx, a: &[Arg]) -> Value {
    atp::out((|| {
        let (d, discount) = tbill_args(ctx, a)?;

        if d > 366.0 {
            return Err(XlError::NUM);
        }
        let mp = Mp::default();

        let t = mp.div(mp.mul(discount, d), 360.0);
        let p = mp.mul(mp.add(negate_nonzero(t), 1.0), 100.0);
        if sign_bit(p) {
            return Err(XlError::NUM);
        }
        Ok(p)
    })())
}

pub fn tbillyield(ctx: &mut EvalCtx, a: &[Arg]) -> Value {
    atp::out((|| {
        let (d, pr) = tbill_args(ctx, a)?;
        let mp = Mp::default();

        let r = mp.div(360.0, d);
        let t = mp.div(mp.sub(100.0, pr), pr);
        Ok(mp.mul(t, r))
    })())
}

pub fn tbilleq(ctx: &mut EvalCtx, a: &[Arg]) -> Value {
    atp::out((|| {
        let (d, discount) = tbill_args(ctx, a)?;
        let mp = Mp::default();
        let r = if d <= 182.0 {
            let den = mp.sub(360.0, mp.mul(discount, d));
            mp.div(mp.mul(365.0, discount), den)
        } else {
            let p = mp.div(
                mp.sub(100.0, mp.div(mp.mul(mp.mul(discount, 100.0), d), 360.0)),
                100.0,
            );
            let yl = if d == 366.0 { 366.0 } else { 365.0 };
            let b = mp.div(mp.mul(-2.0, d), yl);
            let c = mp.sub(1.0, mp.div(1.0, p));
            let qa = mp.sub(mp.div(mp.mul(2.0, d), yl), 1.0);
            let qc = mp.mul(qa, c);
            let t2 = xmath::pow(mp.div(d, yl), 2.0);
            let disc = mp.sub(t2, qc);
            if sign_bit(disc) {
                return Err(XlError::NUM);
            }
            let s = mp.sqrt(disc);
            let qa = mp.sub(mp.div(mp.mul(2.0, d), yl), 1.0);
            if qa.to_bits() == 0 {
                return Err(XlError::NUM);
            }
            mp.div(mp.add(mp.mul(s, 2.0), b), qa)
        };
        if sign_bit(r) {
            return Err(XlError::NUM);
        }
        Ok(r)
    })())
}

struct Bond {
    settle: i64,
    mat: i64,
    freq: i32,
    b: Basis,
    c: Coupons,

    e: f64,

    a: f64,
}

#[derive(Clone, Copy, PartialEq, Eq)]
enum AccruedFrom {
    PrevCoupon,
    DayBs,
}

impl Bond {
    fn n(&self) -> f64 {
        self.c.num as f64
    }
    fn e(&self) -> f64 {
        self.e
    }
    fn a(&self) -> f64 {
        self.a
    }
}

fn bond(
    ctx: &EvalCtx,
    settle: i64,
    mat: i64,
    freq: f64,
    a: &[Arg],
    bi: usize,
    from: AccruedFrom,
) -> R<Bond> {
    let freq = atp::frequency(freq)?;
    let b = basis_of(ctx, a, bi)?;
    if settle >= mat {
        return Err(XlError::NUM);
    }
    let months = (12 / freq) as i64;
    let c = days::bond_coupons(b.sys, settle, mat, months).ok_or(XlError::NUM)?;
    let e = days::calc_coup(b.sys, mat, settle, months, b.basis, days::CoupOp::Days)
        .ok_or(XlError::NUM)?;
    let a_ = match from {
        AccruedFrom::DayBs => {
            days::calc_coup(b.sys, mat, settle, months, b.basis, days::CoupOp::DayBs)
        }

        AccruedFrom::PrevCoupon => match b.basis {
            0 => days::diff_dates(b.sys, c.pcd, settle, 0, 1),
            4 => days::diff_dates(b.sys, c.pcd, settle, 4, 1),
            _ => Some((settle - c.pcd) as f64),
        },
    }
    .ok_or(XlError::NUM)?;
    Ok(Bond {
        settle,
        mat,
        freq,
        b,
        c,
        e,
        a: a_,
    })
}

fn fw_pow(mp: &Mp, base: f64, exp: f64) -> Option<f64> {
    if (base == 0.0 && exp <= 0.0) || (base < 0.0 && exp != exp.trunc()) {
        return None;
    }
    let r = xmath::pow(base, exp);
    let set = mp.code() != 0;
    mp.clear();
    if !r.is_finite() || set {
        return None;
    }
    Some(r)
}

fn price_core(mp: &Mp, bd: &Bond, rate: f64, yld: f64, redemption: f64) -> Option<f64> {
    let f = bd.freq as f64;
    let n = bd.n();
    let (e, a_) = (bd.e(), bd.a());
    let dsc = mp.sub(e, a_);
    if n == 1.0 {
        let coupon = mp.div(mp.mul(rate, 100.0), f);
        let num = mp.add(coupon, redemption);
        let den = mp.add(mp.mul(mp.div(dsc, e), mp.div(yld, f)), 1.0);
        let accrued = mp.mul(coupon, mp.div(a_, e));
        return Some(mp.sub(mp.div(num, den), accrued));
    }
    let base = mp.add(1.0, mp.div(yld, f));
    let x = mp.div(dsc, e);
    let red = mp.div(redemption, fw_pow(mp, base, mp.add(mp.sub(n, 1.0), x))?);
    let mut acc = 0.0;
    let mut k = 1.0;
    while k <= n {
        let t = mp.add(mp.sub(k, 1.0), x);
        let c = mp.div(mp.mul(rate, 100.0), f);
        acc = mp.add(acc, mp.div(c, fw_pow(mp, base, t)?));
        k += 1.0;
    }
    let accrued = mp.mul(mp.mul(mp.div(rate, f), mp.div(a_, e)), 100.0);
    Some(mp.sub(mp.add(acc, red), accrued))
}

pub fn price(ctx: &mut EvalCtx, a: &[Arg]) -> Value {
    atp::out((|| {
        basis_of(ctx, a, 6)?;
        let settle = atp::kernel_date(ctx, a, 0)?;
        let mat = atp::kernel_date(ctx, a, 1)?;
        let rate = atp::req(ctx, a, 2)?;
        let yld = atp::req(ctx, a, 3)?;
        let redemption = atp::req(ctx, a, 4)?;
        let freq = atp::req(ctx, a, 5)?;
        let bd = bond(ctx, settle, mat, freq, a, 6, AccruedFrom::PrevCoupon)?;
        if rate < 0.0 || yld < 0.0 || redemption <= 0.0 {
            return Err(XlError::NUM);
        }
        price_core(&Mp::default(), &bd, rate, yld, redemption).ok_or(XlError::NUM)
    })())
}

pub fn yield_(ctx: &mut EvalCtx, a: &[Arg]) -> Value {
    atp::out((|| {
        basis_of(ctx, a, 6)?;
        let settle = atp::kernel_date(ctx, a, 0)?;
        let mat = atp::kernel_date(ctx, a, 1)?;
        let rate = atp::req(ctx, a, 2)?;
        let pr = atp::req(ctx, a, 3)?;
        let redemption = atp::req(ctx, a, 4)?;
        let freq = atp::req(ctx, a, 5)?;
        let bd = bond(ctx, settle, mat, freq, a, 6, AccruedFrom::DayBs)?;
        if rate < 0.0 || pr <= 0.0 || redemption <= 0.0 {
            return Err(XlError::NUM);
        }
        let f = bd.freq as f64;
        if bd.n() <= 1.0 {
            let mp = Mp::default();
            let sys = bd.b.sys;
            let months = (12 / bd.freq) as i64;
            let e = if matches!(bd.b.basis, 2 | 3) {
                let pcd = days::calc_coup(sys, bd.mat, bd.settle, months, bd.b.basis, CoupOp::Pcd)
                    .ok_or(XlError::NUM)?;
                days::diff_dates(sys, pcd as i64, bd.mat, bd.b.basis, 3).ok_or(XlError::NUM)?
            } else {
                bd.e()
            };
            let a_ = bd.a();
            let dsr =
                days::diff_dates(sys, bd.settle, bd.mat, bd.b.basis, 2).ok_or(XlError::NUM)?;
            let t1 = mp.add(mp.div(redemption, 100.0), mp.div(rate, f));
            let t2 = mp.add(mp.div(pr, 100.0), mp.mul(mp.div(rate, f), mp.div(a_, e)));
            return Ok(mp.mul(mp.div(mp.sub(t1, t2), t2), mp.div(mp.mul(f, e), dsr)));
        }
        yield_newton(&bd, rate, pr, redemption)
    })())
}

fn yield_start(mp: &Mp, rate: f64, nn: f64, pr: f64) -> f64 {
    let d = mp.sub(pr, 100.0);
    let den = mp.add(
        mp.add(mp.mul(nn, 100.0), mp.div(mp.mul(d, nn), 2.0)),
        mp.div(d, 4.0),
    );
    mp.div(mp.sub(mp.mul(mp.mul(nn, rate), 100.0), d), den)
}

fn yield_newton(bd: &Bond, rate: f64, pr: f64, redemption: f64) -> R<f64> {
    let nn = days::diff_years(bd.b.sys, bd.b.basis, bd.settle, bd.mat).ok_or(XlError::NUM)?;
    let (e, a_) = (bd.e(), bd.a());

    let mp = Mp::default();
    let dsc = mp.sub(e, a_);
    let mut y = yield_start(&mp, rate, nn, pr);
    for count in 1..=100 {
        let p = price_core(&mp, bd, rate, y, redemption).ok_or(XlError::NUM)?;
        let d = yield_deriv_mp(&mp, bd, y, dsc, rate, redemption).ok_or(XlError::NUM)?;

        let step = mp.div(mp.sub(pr, p), d);
        if step.abs() < 1e-10 {
            if count == 100 {
                return Err(XlError::NUM);
            }
            return Ok(y);
        }
        y = mp.add(y, step);
    }
    Ok(y)
}

fn yield_deriv_mp(mp: &Mp, bd: &Bond, y: f64, dsc: f64, rate: f64, redemption: f64) -> Option<f64> {
    let (f, e, n) = (bd.freq as f64, bd.e(), bd.n());
    let base = mp.add(1.0, mp.div(y, f));
    let x = mp.div(dsc, e);
    let mut deriv = 0.0;
    let mut k = 1.0;
    while k <= n {
        let pk = fw_pow(mp, base, mp.add(x, k))?;
        let t = mp.add(mp.sub(k, 1.0), x);
        let c = mp.mul(mp.div(rate, f), 100.0);
        deriv = mp.add(deriv, mp.div(-mp.mul(t, c), mp.mul(pk, f)));
        k += 1.0;
    }
    let pn = fw_pow(mp, base, mp.add(x, n))?;
    let t = mp.add(mp.sub(n, 1.0), x);
    Some(mp.sub(deriv, mp.div(mp.mul(t, redemption), mp.mul(pn, f))))
}

fn duration_core(bd: &Bond, coupon: f64, yld: f64) -> R<f64> {
    use crate::x87::{add, div, mul, sub};
    let f = bd.freq as f64;
    let dbc = bd.a();
    let e = bd.e();
    let n = bd.n();
    let dsc = sub(e, dbc);
    let x1 = div(dsc, e);
    let x2 = sub(add(x1, n), 1.0);
    let x3 = add(div(yld, f), 1.0);
    let x4 = xmath::pow(x3, x2);
    if !x4.is_finite() || x4.to_bits() >> 48 == 0 {
        return Err(XlError::NUM);
    }
    let term1 = div(mul(x2, 100.0), x4);
    let term3 = div(100.0, x4);
    let mut term2 = 0.0;
    let mut term4 = 0.0;
    let mut k = 1.0;
    while k <= n {
        let x5 = add(sub(k, 1.0), x1);
        let x6 = xmath::pow(x3, x5);
        if !x6.is_finite() || x6.to_bits() >> 48 == 0 {
            return Err(XlError::NUM);
        }
        let x7 = div(div(mul(100.0, coupon), f), x6);
        term2 = add(term2, mul(x7, x5));
        term4 = add(term4, x7);
        k += 1.0;
    }
    Ok(div(div(add(term1, term2), add(term3, term4)), f))
}

fn dur(ctx: &mut EvalCtx, a: &[Arg], modified: bool) -> Value {
    atp::out((|| {
        let settle = atp::kernel_date(ctx, a, 0)?;
        let mat = atp::kernel_date(ctx, a, 1)?;
        let coupon = atp::req(ctx, a, 2)?;
        let yld = atp::req(ctx, a, 3)?;
        let freq = atp::req(ctx, a, 4)?;
        let bd = bond(ctx, settle, mat, freq, a, 5, AccruedFrom::DayBs)?;
        if coupon < 0.0 || yld < 0.0 {
            return Err(XlError::NUM);
        }
        let d = duration_core(&bd, coupon, yld)?;
        finite(if modified {
            crate::x87::div(
                d,
                crate::x87::add(1.0, crate::x87::div(yld, bd.freq as f64)),
            )
        } else {
            d
        })
    })())
}

pub fn duration(ctx: &mut EvalCtx, a: &[Arg]) -> Value {
    dur(ctx, a, false)
}

pub fn mduration(ctx: &mut EvalCtx, a: &[Arg]) -> Value {
    dur(ctx, a, true)
}

struct OddF {
    settle: i64,
    mat: i64,
    issue: i64,
    first: i64,
    freq: i32,
    b: Basis,
}

impl OddF {
    fn months(&self) -> i64 {
        (12 / self.freq) as i64
    }
}

fn oddf_args(ctx: &EvalCtx, a: &[Arg]) -> R<(OddF, f64, f64, f64)> {
    let settle = atp::kernel_date(ctx, a, 0)?;
    let mat = atp::kernel_date(ctx, a, 1)?;
    let issue = atp::kernel_date(ctx, a, 2)?;
    let first = atp::kernel_date(ctx, a, 3)?;
    let rate = atp::req(ctx, a, 4)?;
    let x = atp::req(ctx, a, 5)?;
    let redemption = atp::req(ctx, a, 6)?;
    let freq = atp::frequency(atp::req(ctx, a, 7)?)?;
    let b = basis_of(ctx, a, 8)?;

    if !(mat > first && first > settle && settle > issue) {
        return Err(XlError::NUM);
    }
    if rate < 0.0 || redemption <= 0.0 {
        return Err(XlError::NUM);
    }
    Ok((
        OddF {
            settle,
            mat,
            issue,
            first,
            freq,
            b,
        },
        rate,
        x,
        redemption,
    ))
}

enum OddFPeriod {
    Short { dfc: f64, dsc: f64, a: f64 },

    Long { dc: f64, a: f64, dsc: f64, nq: f64 },
}

struct OddFDates {
    e: f64,

    n: f64,
    period: OddFPeriod,
}

fn oddf_dates(o: &OddF) -> R<OddFDates> {
    let (sys, basis, months) = (o.b.sys, o.b.basis, o.months());

    let code = days::month_end_code(sys, o.mat);
    if code == -1 {
        return Err(XlError::NUM);
    }
    let (mut ok, mut cur) = days::change_month(sys, o.mat, -months, basis, code);
    while ok && cur > o.first {
        (ok, cur) = days::change_month(sys, cur, -months, basis, code);
    }
    if cur != o.first {
        return Err(XlError::NUM);
    }
    let e =
        days::calc_coup(sys, o.first, o.settle, months, basis, CoupOp::Days).ok_or(XlError::NUM)?;
    let n_all = days::bond_coupons(sys, o.settle, o.mat, months).ok_or(XlError::NUM)?;
    let dfc = days::diff_dates(sys, o.issue, o.first, basis, 2).unwrap_or(0.0);
    if dfc < e {
        let dsc = days::diff_dates(sys, o.settle, o.first, basis, 2).unwrap_or(0.0);
        let a = days::diff_dates(sys, o.issue, o.settle, basis, 0).unwrap_or(0.0);
        return Ok(OddFDates {
            e,
            n: n_all.num as f64,
            period: OddFPeriod::Short { dfc, dsc, a },
        });
    }
    if dfc.partial_cmp(&e) != Some(std::cmp::Ordering::Greater) {
        return Err(XlError::NUM);
    }

    let nc = days::bond_coupons(sys, o.issue, o.first, months)
        .ok_or(XlError::NUM)?
        .num;
    let (mut dc, mut a_sum) = (0.0, 0.0);
    let mut cur = o.first;
    for rem in (1..=nc).rev() {
        let (ok, prev) = days::change_month(sys, cur, -months, basis, 0);
        if !ok {
            return Err(XlError::NUM);
        }
        let nl = if basis == 1 {
            days::diff_dates(sys, prev, cur, 1, 3).unwrap_or(0.0)
        } else {
            e
        };
        let numer = if rem < 2 {
            days::diff_dates(sys, o.issue, cur, basis, 2).unwrap_or(0.0)
        } else {
            nl
        };
        dc = x87::add(x87::div(numer, nl), dc);
        let hi = o.issue.max(prev);
        let lo = o.settle.min(cur);
        let a_num = days::diff_dates(sys, hi, lo, basis, 0).unwrap_or(0.0);
        a_sum = x87::add(x87::div(a_num, nl), a_sum);
        cur = prev;
    }
    let dsc = if matches!(basis, 2 | 3) {
        let ncd = days::calc_coup(sys, o.first, o.settle, months, basis, CoupOp::Ncd)
            .ok_or(XlError::NUM)? as i64;
        days::diff_dates(sys, o.settle, ncd, basis, 2).unwrap_or(0.0)
    } else {
        let pcd = days::calc_coup(sys, o.first, o.settle, months, basis, CoupOp::Pcd)
            .ok_or(XlError::NUM)? as i64;
        let a_ = days::diff_dates(sys, pcd, o.settle, basis, 2).ok_or(XlError::NUM)?;
        x87::sub(e, a_)
    };
    let nq =
        days::calc_periods(sys, o.first, o.settle, months, basis, false).ok_or(XlError::NUM)?;
    let n = days::bond_coupons(sys, o.first, o.mat, months).ok_or(XlError::NUM)?;
    Ok(OddFDates {
        e,
        n: n.num as f64,
        period: OddFPeriod::Long {
            dc,
            a: a_sum,
            dsc,
            nq,
        },
    })
}

fn oddf_price(
    mp: &Mp,
    o: &OddF,
    d: &OddFDates,
    rate: f64,
    yld: f64,
    redemption: f64,
) -> Option<f64> {
    let f = o.freq as f64;
    let base = mp.add(mp.div(yld, f), 1.0);

    let coupon = || mp.div(mp.mul(100.0, rate), f);
    match d.period {
        OddFPeriod::Short { dfc, dsc, a } => {
            let x = mp.div(dsc, d.e);
            let red = mp.div(redemption, fw_pow(mp, base, mp.add(mp.sub(d.n, 1.0), x))?);
            let odd = mp.div(mp.div(mp.mul(coupon(), dfc), d.e), fw_pow(mp, base, x)?);
            let mut sum = 0.0;
            let mut k = 2;
            while k as f64 <= d.n.trunc() {
                let t = mp.add(mp.sub(k as f64, 1.0), x);
                sum = mp.add(mp.div(coupon(), fw_pow(mp, base, t)?), sum);
                k += 1;
            }
            let accrued = mp.mul(mp.mul(mp.div(a, d.e), mp.div(rate, f)), 100.0);
            Some(mp.sub(mp.add(mp.add(red, odd), sum), accrued))
        }
        OddFPeriod::Long { dc, a, dsc, nq } => {
            let x = mp.div(dsc, d.e);
            let red = mp.div(redemption, fw_pow(mp, base, mp.add(mp.add(x, nq), d.n))?);
            let odd = mp.div(mp.mul(coupon(), dc), fw_pow(mp, base, mp.add(nq, x))?);
            let mut sum = 0.0;
            let mut k = 1;
            while k as f64 <= d.n.trunc() {
                let t = mp.add(mp.add(k as f64, nq), x);
                sum = mp.add(mp.div(coupon(), fw_pow(mp, base, t)?), sum);
                k += 1;
            }
            Some(mp.sub(mp.add(mp.add(red, odd), sum), mp.mul(coupon(), a)))
        }
    }
}

pub fn oddfprice(ctx: &mut EvalCtx, a: &[Arg]) -> Value {
    atp::out((|| {
        let (o, rate, yld, redemption) = oddf_args(ctx, a)?;
        if yld < 0.0 {
            return Err(XlError::NUM);
        }
        let d = oddf_dates(&o)?;
        oddf_price(&Mp::default(), &o, &d, rate, yld, redemption).ok_or(XlError::NUM)
    })())
}

fn oddf_deriv(
    mp: &Mp,
    o: &OddF,
    y: f64,
    (dsc, e, n): (f64, f64, f64),
    (rate, red): (f64, f64),
) -> Option<f64> {
    let f = o.freq as f64;
    let base = mp.add(mp.div(y, f), 1.0);
    let mut deriv = 0.0;
    let mut k = 1;
    while k as f64 <= n.trunc() {
        let pk = fw_pow(mp, base, mp.add(k as f64, mp.div(dsc, e)))?;
        let t = mp.add(mp.sub(k as f64, 1.0), mp.div(dsc, e));
        let c = negate_nonzero(mp.mul(mp.div(rate, f), 100.0));
        deriv = mp.add(mp.div(mp.mul(c, t), mp.mul(f, pk)), deriv);
        k += 1;
    }
    let base = mp.add(mp.div(y, f), 1.0);
    let pn = fw_pow(mp, base, mp.add(mp.div(dsc, e), n))?;
    let t = mp.add(mp.sub(n, 1.0), mp.div(dsc, e));
    let term = mp.div(mp.mul(t, red), mp.mul(f, pn));
    Some(mp.sub(deriv, term))
}

pub fn oddfyield(ctx: &mut EvalCtx, a: &[Arg]) -> Value {
    atp::out((|| {
        let (o, rate, pr, redemption) = oddf_args(ctx, a)?;
        if pr <= 0.0 {
            return Err(XlError::NUM);
        }
        let (sys, basis, months) = (o.b.sys, o.b.basis, o.months());
        let n_all = days::bond_coupons(sys, o.settle, o.mat, months).ok_or(XlError::NUM)?;
        let d = oddf_dates(&o)?;
        if matches!(d.period, OddFPeriod::Long { .. }) {
            days::calc_coup(sys, o.first, o.settle, months, basis, CoupOp::Ncd)
                .ok_or(XlError::NUM)?;
        }
        let dsc = days::diff_dates(sys, o.settle, o.first, basis, 2).unwrap_or(0.0);
        let nn = days::diff_years(sys, basis, o.settle, o.mat).ok_or(XlError::NUM)?;
        let n = n_all.num as f64;
        let mp = Mp::default();
        let mut y = yield_start(&mp, rate, nn, pr);
        for count in 1..=100 {
            let p = oddf_price(&mp, &o, &d, rate, y, redemption).ok_or(XlError::NUM)?;
            let deriv =
                oddf_deriv(&mp, &o, y, (dsc, d.e, n), (rate, redemption)).ok_or(XlError::NUM)?;
            let step = mp.div(mp.sub(pr, p), deriv);
            if step.abs() < 1e-10 {
                if count == 100 {
                    return Err(XlError::NUM);
                }
                return Ok(y);
            }
            y = mp.add(y, step);
        }
        Ok(y)
    })())
}

struct OddL {
    settle: i64,
    mat: i64,
    last: i64,
    freq: i32,
    b: Basis,
}

fn oddl_args(ctx: &EvalCtx, a: &[Arg]) -> R<(OddL, f64, f64, f64)> {
    let settle = atp::kernel_date(ctx, a, 0)?;
    let mat = atp::kernel_date(ctx, a, 1)?;
    let last = atp::kernel_date(ctx, a, 2)?;
    let rate = atp::req(ctx, a, 3)?;
    let x = atp::req(ctx, a, 4)?;
    let redemption = atp::req(ctx, a, 5)?;
    let freq = atp::frequency(atp::req(ctx, a, 6)?)?;
    let b = basis_of(ctx, a, 7)?;

    if !(mat > settle && settle > last) {
        return Err(XlError::NUM);
    }
    if rate < 0.0 || redemption <= 0.0 {
        return Err(XlError::NUM);
    }
    Ok((
        OddL {
            settle,
            mat,
            last,
            freq,
            b,
        },
        rate,
        x,
        redemption,
    ))
}

fn oddl_sums(mp: &Mp, o: &OddL) -> R<(f64, f64, f64)> {
    let (sys, basis) = (o.b.sys, o.b.basis);
    let months = (12 / o.freq) as i64;
    let n = days::bond_coupons(sys, o.last, o.mat, months)
        .ok_or(XlError::NUM)?
        .num;
    let (mut dc, mut ac, mut dsc) = (0.0, 0.0, 0.0);
    let mut cur = o.last;
    for i in 1..=n {
        let (ok, next) = days::change_month(sys, cur, months, basis, 0);
        if !ok {
            return Err(XlError::NUM);
        }
        let nl = days::diff_dates(sys, cur, next, basis, 3).unwrap_or(0.0);
        let dc_num = if n <= i {
            days::diff_dates(sys, cur, o.mat, basis, 3).unwrap_or(0.0)
        } else {
            nl
        };
        let a_num = if next < o.settle {
            dc_num
        } else if cur < o.settle {
            days::diff_dates(sys, cur, o.settle, basis, 1).unwrap_or(0.0)
        } else {
            0.0
        };
        let dsc_num =
            days::diff_dates(sys, o.settle.max(cur), o.mat.min(next), basis, 0).unwrap_or(0.0);
        dc = mp.add(mp.div(dc_num, nl), dc);
        ac = mp.add(mp.div(a_num, nl), ac);
        dsc = mp.add(mp.div(dsc_num, nl), dsc);
        cur = next;
    }
    Ok((dc, ac, dsc))
}

pub fn oddlprice(ctx: &mut EvalCtx, a: &[Arg]) -> Value {
    atp::out((|| {
        let (o, rate, yld, redemption) = oddl_args(ctx, a)?;
        if yld < 0.0 {
            return Err(XlError::NUM);
        }
        let mp = Mp::default();
        let f = o.freq as f64;
        let (dc, ac, dsc) = oddl_sums(&mp, &o)?;

        let cf = mp.div(mp.mul(100.0, rate), f);
        let t1 = mp.add(mp.mul(dc, cf), redemption);
        let t2 = mp.add(mp.div(mp.mul(dsc, yld), f), 1.0);
        Ok(mp.sub(mp.div(t1, t2), mp.mul(ac, cf)))
    })())
}

pub fn oddlyield(ctx: &mut EvalCtx, a: &[Arg]) -> Value {
    atp::out((|| {
        let (o, rate, pr, redemption) = oddl_args(ctx, a)?;
        if pr <= 0.0 {
            return Err(XlError::NUM);
        }
        let mp = Mp::default();
        let f = o.freq as f64;
        let (dc, ac, dsc) = oddl_sums(&mp, &o)?;
        let cf = mp.div(mp.mul(100.0, rate), f);
        let t1 = mp.add(mp.mul(dc, cf), redemption);
        let t2 = mp.add(mp.mul(ac, cf), pr);
        let ratio = mp.div(f, dsc);
        Ok(mp.mul(mp.div(mp.sub(t1, t2), t2), ratio))
    })())
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn fw_pow_fails_on_the_flag_and_clears_it() {
        let mp = Mp::default();
        assert_eq!(fw_pow(&mp, 2.0, 3.0), Some(8.0));

        assert_eq!(mp.mul(f64::MAX, 2.0), 0.0);
        assert_eq!(fw_pow(&mp, 2.0, 3.0), None);
        assert_eq!(fw_pow(&mp, 2.0, 3.0), Some(8.0));

        assert_eq!(fw_pow(&mp, 0.0, 0.0), None);
        assert_eq!(fw_pow(&mp, -2.0, 0.5), None);
        assert_eq!(fw_pow(&mp, 10.0, 400.0), None);
        assert_eq!(fw_pow(&mp, -2.0, 2.0), Some(4.0));
    }

    #[test]
    fn mp_neg_keeps_zero_and_subnormal_signs() {
        assert_eq!(negate_nonzero(0.0).to_bits(), 0);
        assert_eq!(negate_nonzero(5e-324).to_bits(), 5e-324f64.to_bits());
        assert_eq!(negate_nonzero(1.5), -1.5);
        assert!(sign_bit(-0.0));
        assert!(!sign_bit(0.0));
    }

    #[test]
    fn a_subnormal_divisor_is_flag_4() {
        let mp = Mp::default();
        assert_eq!(mp.div(1.0, 5e-324), 0.0);
        assert_eq!(mp.code(), 4);
    }
}
