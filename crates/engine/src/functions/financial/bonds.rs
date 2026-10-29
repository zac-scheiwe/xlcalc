use super::atp::{self, Arg};
use super::days::{self, Basis, Coupons};
use crate::datetime::DateSystem;
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

fn basis_of(ctx: &EvalCtx, a: &[Arg], i: usize) -> R<Basis> {
    Ok(Basis {
        sys: atp::date_system(ctx),
        basis: atp::basis(ctx, a, i)?,
    })
}

pub fn accrint(ctx: &mut EvalCtx, a: &[Arg]) -> Value {
    atp::out((|| {
        let issue = atp::date(ctx, a, 0)?;
        let first = atp::date(ctx, a, 1)?;
        let settle = atp::date(ctx, a, 2)?;
        let rate = atp::req(ctx, a, 3)?;
        let par = atp::opt(ctx, a, 4, 1000.0)?;
        let freq = atp::frequency(atp::req(ctx, a, 5)?)?;
        let b = basis_of(ctx, a, 6)?;
        let calc_method = match a.get(7) {
            Some(Some(v)) => match v {
                Value::Blank => false,
                v => boolean(v)?,
            },
            _ => true,
        };
        if issue < 1 || first < 1 {
            return Err(XlError::NUM);
        }
        if rate <= 0.0 || par <= 0.0 || issue >= settle {
            return Err(XlError::NUM);
        }

        let sys = b.sys;
        let months = (12 / freq) as i64;
        let f = days::ymd(sys, first);
        let eom = f.d >= days::dim(f.y, f.m);
        let adj = |s: i64, k: i64| {
            let d = days::ymd(sys, s);
            let (y, m, dd) = days::add_months(d.y, d.m, d.d, k, eom);
            days::serial(sys, y, m, dd)
        };

        let cnt = |x: i64, y: i64, period: bool, count: bool| -> f64 {
            (match b.basis {
                0 => days::days360_us(sys, x, y, period),
                2 if !count => days::days360_us(sys, x, y, period),
                4 => days::days360_eu(sys, x, y),
                _ => y - x,
            }) as f64
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
        finite(x87::mul(x87::mul(par, x87::div(rate, freq as f64)), acc))
    })())
}

pub fn accrintm(ctx: &mut EvalCtx, a: &[Arg]) -> Value {
    atp::out((|| {
        let issue = atp::date(ctx, a, 0)?;
        let settle = atp::date(ctx, a, 1)?;
        let rate = atp::req(ctx, a, 2)?;
        let par = atp::opt(ctx, a, 3, 1000.0)?;
        let b = basis_of(ctx, a, 4)?;
        if rate <= 0.0 || par <= 0.0 || issue > settle {
            return Err(XlError::NUM);
        }
        let yf = days::yearfrac_core(b.sys, issue, settle, b.basis)?;
        finite(par * rate * yf)
    })())
}

fn disc_args(ctx: &EvalCtx, a: &[Arg]) -> R<(i64, i64, f64, f64, Basis)> {
    let settle = atp::date(ctx, a, 0)?;
    let mat = atp::date(ctx, a, 1)?;
    let x = atp::req(ctx, a, 2)?;
    let y = atp::req(ctx, a, 3)?;
    let b = basis_of(ctx, a, 4)?;
    if settle >= mat {
        return Err(XlError::NUM);
    }
    Ok((settle, mat, x, y, b))
}

pub fn disc(ctx: &mut EvalCtx, a: &[Arg]) -> Value {
    atp::out((|| {
        let (s, m, pr, redemption, b) = disc_args(ctx, a)?;
        if pr <= 0.0 || redemption <= 0.0 {
            return Err(XlError::NUM);
        }
        finite((1.0 - pr / redemption) * (b.year(s, m) / b.days(s, m)))
    })())
}

pub fn intrate(ctx: &mut EvalCtx, a: &[Arg]) -> Value {
    atp::out((|| {
        let (s, m, inv, redemption, b) = disc_args(ctx, a)?;
        if inv <= 0.0 || redemption <= 0.0 {
            return Err(XlError::NUM);
        }

        let t = x87::div(x87::sub(redemption, inv), inv);
        finite(x87::mul(x87::div(b.year(s, m), b.days(s, m)), t))
    })())
}

pub fn received(ctx: &mut EvalCtx, a: &[Arg]) -> Value {
    atp::out((|| {
        let (s, m, inv, discount, b) = disc_args(ctx, a)?;
        if inv <= 0.0 || discount <= 0.0 {
            return Err(XlError::NUM);
        }

        let d = x87::sub(
            1.0,
            x87::div(x87::mul(discount, b.days(s, m)), b.year(s, m)),
        );
        if d <= 0.0 {
            return Err(XlError::NUM);
        }
        finite(x87::div(inv, d))
    })())
}

pub fn pricedisc(ctx: &mut EvalCtx, a: &[Arg]) -> Value {
    atp::out((|| {
        let (s, m, discount, redemption, b) = disc_args(ctx, a)?;
        if discount <= 0.0 || redemption <= 0.0 {
            return Err(XlError::NUM);
        }
        finite(redemption - discount * redemption * b.days(s, m) / b.year(s, m))
    })())
}

pub fn yielddisc(ctx: &mut EvalCtx, a: &[Arg]) -> Value {
    atp::out((|| {
        let (s, m, pr, redemption, b) = disc_args(ctx, a)?;
        if pr <= 0.0 || redemption <= 0.0 {
            return Err(XlError::NUM);
        }

        let t = x87::div(x87::sub(redemption, pr), pr);
        finite(x87::mul(x87::div(b.year(s, m), b.days(s, m)), t))
    })())
}

fn mat_args(ctx: &EvalCtx, a: &[Arg]) -> R<(i64, i64, i64, f64, f64, Basis)> {
    let settle = atp::date(ctx, a, 0)?;
    let mat = atp::date(ctx, a, 1)?;
    let issue = atp::date(ctx, a, 2)?;
    let rate = atp::req(ctx, a, 3)?;
    let x = atp::req(ctx, a, 4)?;
    let b = basis_of(ctx, a, 5)?;
    if settle >= mat || issue >= settle || rate < 0.0 {
        return Err(XlError::NUM);
    }
    Ok((settle, mat, issue, rate, x, b))
}

pub fn pricemat(ctx: &mut EvalCtx, a: &[Arg]) -> Value {
    atp::out((|| {
        let (s, m, issue, rate, yld, b) = mat_args(ctx, a)?;
        if yld < 0.0 {
            return Err(XlError::NUM);
        }
        let by = b.year(issue, s);
        let dim = b.days(issue, m);
        let ai = b.days(issue, s);
        let dsm = dim - ai;
        finite((100.0 + dim / by * rate * 100.0) / (1.0 + dsm / by * yld) - ai / by * rate * 100.0)
    })())
}

pub fn yieldmat(ctx: &mut EvalCtx, a: &[Arg]) -> Value {
    atp::out((|| {
        let (s, m, issue, rate, pr, b) = mat_args(ctx, a)?;
        if pr <= 0.0 {
            return Err(XlError::NUM);
        }
        let by = b.year(issue, s);
        let dim = b.days(issue, m);
        let ai = b.days(issue, s);
        let dsm = dim - ai;
        let t2 = pr / 100.0 + ai / by * rate;
        let t1 = (1.0 + dim / by * rate) - t2;
        finite(t1 / t2 * (by / dsm))
    })())
}

fn tbill_args(ctx: &EvalCtx, a: &[Arg]) -> R<(f64, f64)> {
    let settle = atp::date(ctx, a, 0)?;
    let mat = atp::date(ctx, a, 1)?;
    let x = atp::req(ctx, a, 2)?;
    if settle >= mat || mat - settle > 365 {
        return Err(XlError::NUM);
    }
    Ok(((mat - settle) as f64, x))
}

pub fn tbillprice(ctx: &mut EvalCtx, a: &[Arg]) -> Value {
    atp::out((|| {
        let (dsm, discount) = tbill_args(ctx, a)?;
        if discount <= 0.0 {
            return Err(XlError::NUM);
        }
        let p = 100.0 * (1.0 - discount * dsm / 360.0);
        if p <= 0.0 {
            return Err(XlError::NUM);
        }
        finite(p)
    })())
}

pub fn tbillyield(ctx: &mut EvalCtx, a: &[Arg]) -> Value {
    atp::out((|| {
        let (dsm, pr) = tbill_args(ctx, a)?;
        if pr <= 0.0 {
            return Err(XlError::NUM);
        }
        finite((100.0 - pr) / pr * (360.0 / dsm))
    })())
}

pub fn tbilleq(ctx: &mut EvalCtx, a: &[Arg]) -> Value {
    atp::out((|| {
        let (dsm, discount) = tbill_args(ctx, a)?;
        if discount <= 0.0 {
            return Err(XlError::NUM);
        }
        let price = 100.0 * (1.0 - discount * dsm / 360.0);
        if price <= 0.0 {
            return Err(XlError::NUM);
        }
        if dsm <= 182.0 {
            return finite(365.0 * discount / (360.0 - discount * dsm));
        }

        use crate::x87::{add, div, mul, sub};
        let p = div(
            sub(100.0, div(mul(mul(100.0, discount), dsm), 360.0)),
            100.0,
        );
        let b = div(mul(dsm, -2.0), 365.0);
        let c = sub(1.0, div(1.0, p));
        let qa = sub(div(mul(dsm, 2.0), 365.0), 1.0);
        let disc = sub(crate::xmath::pow(div(dsm, 365.0), 2.0), mul(c, qa));
        if disc < 0.0 {
            return Err(XlError::NUM);
        }
        finite(div(add(b, mul(2.0, crate::x87::sqrt(disc))), qa))
    })())
}

struct Bond {
    settle: i64,
    mat: i64,
    freq: i32,
    b: Basis,
    c: Coupons,
}

impl Bond {
    fn n(&self) -> f64 {
        self.c.num as f64
    }

    fn e(&self) -> f64 {
        if self.b.basis == 1 {
            self.a() + self.b.coupdaysnc(&self.c, self.settle)
        } else {
            self.b.coupdays(&self.c, self.mat, self.freq)
        }
    }
    fn a(&self) -> f64 {
        self.b.coupdaybs(&self.c, self.settle)
    }
}

fn bond(ctx: &EvalCtx, settle: i64, mat: i64, freq: f64, a: &[Arg], bi: usize) -> R<Bond> {
    let freq = atp::frequency(freq)?;
    let b = basis_of(ctx, a, bi)?;
    if settle >= mat {
        return Err(XlError::NUM);
    }
    let c = days::coupons(b.sys, settle, mat, freq);

    if c.pcd < 0 {
        return Err(XlError::NUM);
    }
    Ok(Bond {
        settle,
        mat,
        freq,
        b,
        c,
    })
}

fn price_core(bd: &Bond, rate: f64, yld: f64, redemption: f64) -> f64 {
    use crate::x87::{add, div, mul, sub};
    let f = bd.freq as f64;
    let n = bd.n();
    let (e, a_) = (bd.e(), bd.a());
    let dsc = sub(e, a_);
    if n == 1.0 {
        let coupon = div(mul(rate, 100.0), f);
        let num = add(coupon, redemption);
        let den = add(mul(div(dsc, e), div(yld, f)), 1.0);
        let accrued = mul(coupon, div(a_, e));
        return sub(div(num, den), accrued);
    }
    let base = add(1.0, div(yld, f));
    let x = div(dsc, e);
    let red = div(redemption, xmath::pow(base, add(sub(n, 1.0), x)));
    let mut acc = 0.0;
    let mut k = 1.0;
    while k <= n {
        let t = add(sub(k, 1.0), x);
        acc = add(acc, div(div(mul(rate, 100.0), f), xmath::pow(base, t)));
        k += 1.0;
    }
    let accrued = mul(mul(div(rate, f), div(a_, e)), 100.0);
    sub(add(acc, red), accrued)
}

pub fn price(ctx: &mut EvalCtx, a: &[Arg]) -> Value {
    atp::out((|| {
        let settle = atp::date(ctx, a, 0)?;
        let mat = atp::date(ctx, a, 1)?;
        let rate = atp::req(ctx, a, 2)?;
        let yld = atp::req(ctx, a, 3)?;
        let redemption = atp::req(ctx, a, 4)?;
        let freq = atp::req(ctx, a, 5)?;
        let bd = bond(ctx, settle, mat, freq, a, 6)?;
        if rate < 0.0 || yld < 0.0 || redemption <= 0.0 {
            return Err(XlError::NUM);
        }
        finite(price_core(&bd, rate, yld, redemption))
    })())
}

pub fn yield_(ctx: &mut EvalCtx, a: &[Arg]) -> Value {
    atp::out((|| {
        let settle = atp::date(ctx, a, 0)?;
        let mat = atp::date(ctx, a, 1)?;
        let rate = atp::req(ctx, a, 2)?;
        let pr = atp::req(ctx, a, 3)?;
        let redemption = atp::req(ctx, a, 4)?;
        let freq = atp::req(ctx, a, 5)?;
        let bd = bond(ctx, settle, mat, freq, a, 6)?;
        if rate < 0.0 || pr <= 0.0 || redemption <= 0.0 {
            return Err(XlError::NUM);
        }
        let f = bd.freq as f64;
        if bd.n() <= 1.0 {
            use crate::x87::{add, div, mul, sub};
            let e = match bd.b.basis {
                0 | 4 => bd.e(),
                _ => (bd.c.ncd - bd.c.pcd) as f64,
            };
            let a_ = bd.a();
            let dsr = bd.b.days(bd.settle, bd.mat);
            let t1 = add(div(redemption, 100.0), div(rate, f));
            let t2 = add(div(pr, 100.0), mul(div(rate, f), div(a_, e)));
            return finite(mul(div(sub(t1, t2), t2), div(mul(f, e), dsr)));
        }
        yield_newton(&bd, rate, pr, redemption)
    })())
}

fn yield_start(rate: f64, nn: f64, pr: f64) -> f64 {
    use crate::x87::{add, div, mul, sub};
    let d = sub(pr, 100.0);
    let den = add(add(mul(nn, 100.0), div(mul(d, nn), 2.0)), div(d, 4.0));
    div(sub(mul(mul(nn, rate), 100.0), d), den)
}

fn yield_deriv(y: f64, f: f64, dsc: f64, e: f64, n: f64, rate: f64, redemption: f64) -> f64 {
    use crate::x87::{add, div, mul, sub};
    let base = add(1.0, div(y, f));
    let x = div(dsc, e);
    let mut deriv = 0.0;
    let mut k = 1.0;
    while k <= n {
        let pk = xmath::pow(base, add(x, k));
        let t = add(sub(k, 1.0), x);
        let c = mul(div(rate, f), 100.0);
        deriv = add(deriv, div(-mul(t, c), mul(pk, f)));
        k += 1.0;
    }
    let pn = xmath::pow(base, add(x, n));
    let t = add(sub(n, 1.0), x);
    sub(deriv, div(mul(t, redemption), mul(pn, f)))
}

fn yield_iterate(
    y: f64,
    pr: f64,
    price: impl FnMut(f64) -> R<f64>,
    deriv: impl Fn(f64) -> f64,
) -> R<f64> {
    yield_iterate_ex(y, pr, price, deriv, false)
}

fn yield_iterate_ex(
    mut y: f64,
    pr: f64,
    mut price: impl FnMut(f64) -> R<f64>,
    deriv: impl Fn(f64) -> f64,
    oddf: bool,
) -> R<f64> {
    use crate::x87::{add, div, sub};
    for count in 1..=100 {
        let p = price(y)?;
        let step = div(sub(pr, p), deriv(y));
        if !step.is_finite() {
            return Err(XlError::NUM);
        }
        if step.abs() < 1e-10 {
            if oddf && count == 100 {
                return Err(XlError::NUM);
            }
            return finite(y);
        }
        y = add(y, step);
    }
    if oddf {
        return finite(y);
    }
    Err(XlError::NUM)
}

fn yield_newton(bd: &Bond, rate: f64, pr: f64, redemption: f64) -> R<f64> {
    use crate::x87::sub;

    let nn = days::yield_years(bd.b.sys, bd.b.basis, bd.settle, bd.mat);
    let f = bd.freq as f64;
    let n = bd.n();
    let (e, a_) = (bd.e(), bd.a());
    let dsc = sub(e, a_);
    let price = |y: f64| -> R<f64> { Ok(price_core(bd, rate, y, redemption)) };
    let deriv = |y: f64| yield_deriv(y, f, dsc, e, n, rate, redemption);
    yield_iterate(yield_start(rate, nn, pr), pr, price, deriv)
}

fn duration_core(bd: &Bond, coupon: f64, yld: f64) -> f64 {
    let f = bd.freq as f64;
    let dbc = bd.a();
    let e = bd.e();
    let n = bd.n();
    let dsc = e - dbc;
    let x1 = dsc / e;
    let x2 = x1 + n - 1.0;
    let x3 = yld / f + 1.0;
    let x4 = xmath::pow(x3, x2);
    let term1 = x2 * 100.0 / x4;
    let term3 = 100.0 / x4;
    let mut term2 = 0.0;
    let mut term4 = 0.0;
    let mut k = 1.0;
    while k <= n {
        let x5 = k - 1.0 + x1;
        let x6 = xmath::pow(x3, x5);
        let x7 = (100.0 * coupon / f) / x6;
        term2 += x7 * x5;
        term4 += x7;
        k += 1.0;
    }
    ((term1 + term2) / (term3 + term4)) / f
}

fn dur(ctx: &mut EvalCtx, a: &[Arg], modified: bool) -> Value {
    atp::out((|| {
        let settle = atp::date(ctx, a, 0)?;
        let mat = atp::date(ctx, a, 1)?;
        let coupon = atp::req(ctx, a, 2)?;
        let yld = atp::req(ctx, a, 3)?;
        let freq = atp::req(ctx, a, 4)?;
        let bd = bond(ctx, settle, mat, freq, a, 5)?;
        if coupon < 0.0 || yld < 0.0 {
            return Err(XlError::NUM);
        }
        let d = duration_core(&bd, coupon, yld);
        finite(if modified {
            d / (1.0 + yld / bd.freq as f64)
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

fn schedule_reaches(sys: DateSystem, anchor: i64, freq: i32, target: i64) -> R<()> {
    let a = days::ymd(sys, anchor);
    let eom = a.d >= days::dim(a.y, a.m);
    let months = (12 / freq) as i64;
    let first_valid = if sys == DateSystem::D1900 { 1 } else { 0 };
    let mut k = 1;
    loop {
        let (y, m, d) = days::add_months(a.y, a.m, a.d, -k * months, eom);
        let q = days::serial(sys, y, m, d);
        if q < first_valid {
            return Err(XlError::NUM);
        }
        if q <= target {
            return Ok(());
        }
        k += 1;
    }
}

fn oddf_args(ctx: &EvalCtx, a: &[Arg]) -> R<(OddF, f64, f64, f64)> {
    let settle = atp::date(ctx, a, 0)?;
    let mat = atp::date(ctx, a, 1)?;
    let issue = atp::date(ctx, a, 2)?;
    let first = atp::date(ctx, a, 3)?;
    let rate = atp::req(ctx, a, 4)?;
    let x = atp::req(ctx, a, 5)?;
    let redemption = atp::req(ctx, a, 6)?;
    let freq = atp::frequency(atp::req(ctx, a, 7)?)?;
    let b = basis_of(ctx, a, 8)?;
    if !(mat > first && first > settle && settle > issue) {
        return Err(XlError::NUM);
    }

    let mt = days::ymd(b.sys, mat);
    let eom = mt.d >= days::dim(mt.y, mt.m);
    let months = (12 / freq) as i64;
    let mut k = 1;
    loop {
        let (y, m, d) = days::add_months(mt.y, mt.m, mt.d, -k * months, eom);
        let q = days::serial(b.sys, y, m, d);
        if q == first {
            break;
        }
        if q < first {
            return Err(XlError::NUM);
        }
        k += 1;
    }
    if rate < 0.0 || redemption <= 0.0 {
        return Err(XlError::NUM);
    }
    schedule_reaches(b.sys, first, freq, issue)?;
    let o = OddF {
        settle,
        mat,
        issue,
        first,
        freq,
        b,
    };

    if o.b.days(issue, first) == oddf_e(&o) {
        return Err(XlError::NUM);
    }
    Ok((o, rate, x, redemption))
}

struct OddFlows {
    flows: Vec<(f64, f64, bool)>,
    accrued: f64,

    dsc: f64,
    e: f64,
    n: f64,
}

fn oddfprice_core(o: &OddF, rate: f64, yld: f64, redemption: f64) -> R<f64> {
    use crate::x87::{add, div, sub};
    let fl = oddf_flows(o, rate, redemption);
    let base = add(1.0, div(yld, o.freq as f64));

    let (regular, tail) = fl.flows.split_at(fl.flows.len() - 2);
    let mut acc = 0.0;
    for &(amt, t, _) in regular {
        acc = add(acc, div(amt, xmath::pow(base, t)));
    }
    let (odd, red) = (tail[0], tail[1]);
    let tail = add(
        div(odd.0, xmath::pow(base, odd.1)),
        div(red.0, xmath::pow(base, red.1)),
    );
    finite(sub(add(acc, tail), fl.accrued))
}

fn oddf_e(o: &OddF) -> f64 {
    let months = (12 / o.freq) as i64;
    let fc = days::ymd(o.b.sys, o.first);
    let eom = fc.d >= days::dim(fc.y, fc.m);
    let (y, m, d) = days::add_months(fc.y, fc.m, fc.d, -months, eom);
    let pcd_first = days::serial(o.b.sys, y, m, d);
    if o.b.basis == 1 {
        (o.first - pcd_first) as f64
    } else {
        let c0 = Coupons {
            pcd: pcd_first,
            ncd: o.first,
            num: 1,
        };
        o.b.coupdays(&c0, o.mat, o.freq)
    }
}

fn oddf_flows(o: &OddF, rate: f64, redemption: f64) -> OddFlows {
    use crate::x87::{add, div, mul, sub};
    let b = &o.b;
    let f = o.freq as f64;
    let months = (12 / o.freq) as i64;
    let sys = b.sys;
    let coupon = div(mul(rate, 100.0), f);

    let regular = days::coupons(sys, o.first, o.mat, o.freq);
    let _ = regular;

    let n = days::coupons(sys, o.first - 1, o.mat, o.freq).num as f64;

    let fc = days::ymd(sys, o.first);
    let eom = fc.d >= days::dim(fc.y, fc.m);
    let q = |k: i64| {
        let (y, m, d) = days::add_months(fc.y, fc.m, fc.d, k * months, eom);
        days::serial(sys, y, m, d)
    };

    let qc = |k: i64| {
        let mut cur = (fc.y, fc.m, fc.d);
        for _ in 0..-k {
            cur = days::add_months(cur.0, cur.1, cur.2, -months, false);
        }
        days::serial(sys, cur.0, cur.1, cur.2)
    };
    let pcd_first = q(-1);
    let e = oddf_e(o);
    if o.issue >= pcd_first {
        let dfc = b.days(o.issue, o.first);
        let dsc = b.days(o.settle, o.first);
        let a_ = b.days(o.issue, o.settle);
        let x = div(dsc, e);

        let mut flows = Vec::new();
        let mut k = 2.0;
        while k <= n {
            flows.push((coupon, add(k - 1.0, x), false));
            k += 1.0;
        }
        flows.push((div(mul(coupon, dfc), e), x, true));
        flows.push((redemption, add(n - 1.0, x), false));
        return OddFlows {
            flows,
            accrued: mul(mul(div(rate, f), div(a_, e)), 100.0),
            dsc,
            e,
            n,
        };
    }

    let mut k = -1i64;
    while q(k) > o.issue {
        k -= 1;
    }
    let nc = -k;
    let mut dc_sum = 0.0;
    let mut a_sum = 0.0;
    let mut nq = 0.0;
    let mut dsc_q = 0.0;
    let mut e_q = e;

    for i in 1..=nc {
        let p0 = q(-i);
        let p1 = q(-i + 1);
        let c = Coupons {
            pcd: p0,
            ncd: p1,
            num: 1,
        };
        let nl = if b.basis == 1 {
            (p1 - p0) as f64
        } else {
            b.coupdays(&c, o.mat, o.freq)
        };

        let (c0, c1) = (qc(-i), qc(-i + 1));
        let nl_c = if b.basis == 1 { (c1 - c0) as f64 } else { nl };
        let s = o.issue.max(c0);

        dc_sum = add(
            dc_sum,
            if o.issue < c0 {
                1.0
            } else {
                div(b.days(s, c1), nl_c)
            },
        );
        if o.settle > c0 {
            a_sum = add(a_sum, div(b.days(s, o.settle.min(c1)), nl_c));
        }
        if o.settle < p0 {
            nq += 1.0;
        } else if o.settle < p1 {
            dsc_q = match b.basis {
                0 | 4 => sub(nl, b.days(p0, o.settle)),
                _ => b.days(o.settle, p1),
            };
            e_q = nl;
        }
    }
    let x = div(dsc_q, e_q);

    let nq_sched = nq;

    if eom {
        let s = days::ymd(sys, o.settle);
        let span = (fc.y * 12 + fc.m as i64) - (s.y * 12 + s.m as i64);
        nq = ((span + months - 1) / months) as f64;
        if (1..=nc).any(|i| q(-i) == o.settle) {
            nq -= 1.0;
        }
    }

    let n = n - 1.0;

    let t0 = add(nq, x);
    let mut flows = Vec::new();
    let mut k = 1.0;

    while k <= n {
        flows.push((coupon, add(add(k, nq), x), false));
        k += 1.0;
    }
    flows.push((mul(coupon, dc_sum), t0, true));
    flows.push((redemption, add(t0, n), false));

    let dsc = b.days(o.settle, o.first);
    OddFlows {
        flows,
        accrued: mul(coupon, a_sum),
        dsc,
        e: e_q,
        n: n + nq_sched + 1.0,
    }
}

pub fn oddfprice(ctx: &mut EvalCtx, a: &[Arg]) -> Value {
    atp::out((|| {
        let (o, rate, yld, redemption) = oddf_args(ctx, a)?;
        if yld < 0.0 {
            return Err(XlError::NUM);
        }
        oddfprice_core(&o, rate, yld, redemption)
    })())
}

pub fn oddfyield(ctx: &mut EvalCtx, a: &[Arg]) -> Value {
    atp::out((|| {
        let (o, rate, pr, redemption) = oddf_args(ctx, a)?;
        if pr <= 0.0 {
            return Err(XlError::NUM);
        }
        let fl = oddf_flows(&o, rate, redemption);
        let f = o.freq as f64;
        let nn = days::yield_years(o.b.sys, o.b.basis, o.settle, o.mat);
        let price = |y: f64| oddfprice_core(&o, rate, y, redemption);
        let deriv = |y: f64| yield_deriv(y, f, fl.dsc, fl.e, fl.n, rate, redemption);
        yield_iterate_ex(yield_start(rate, nn, pr), pr, price, deriv, true)
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
    let settle = atp::date(ctx, a, 0)?;
    let mat = atp::date(ctx, a, 1)?;
    let last = atp::date(ctx, a, 2)?;
    let rate = atp::req(ctx, a, 3)?;
    let x = atp::req(ctx, a, 4)?;
    let redemption = atp::req(ctx, a, 5)?;
    let freq = atp::frequency(atp::req(ctx, a, 6)?)?;
    let mut b = basis_of(ctx, a, 7)?;

    if b.basis == 2 || b.basis == 3 {
        b.basis = 1;
    }
    if !(mat > settle && settle > last) {
        return Err(XlError::NUM);
    }
    if rate < 0.0 || redemption <= 0.0 {
        return Err(XlError::NUM);
    }
    schedule_reaches(b.sys, mat, freq, last)?;
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

fn oddl_sums(o: &OddL) -> (f64, f64, f64) {
    let b = &o.b;
    let sys = b.sys;
    let months = (12 / o.freq) as i64;
    let l = days::ymd(sys, o.last);

    let eom = l.d >= days::dim(l.y, l.m);
    let snapped = |k: i64| {
        let (y, mo, d) = days::add_months(l.y, l.m, l.d, k * months, eom);
        days::serial(sys, y, mo, d)
    };
    let mut nc = 1usize;
    while snapped(nc as i64) < o.mat {
        nc += 1;
    }
    let mut q = vec![o.last];
    let (mut y, mut mo, mut d) = (l.y, l.m, l.d);
    for _ in 0..=nc {
        (y, mo, d) = days::add_months(y, mo, d, months, false);
        q.push(days::serial(sys, y, mo, d));
    }

    let days = |a: i64, z: i64, both: bool| match b.basis {
        0 => days::days360_us(sys, a, z, both) as f64,
        _ => b.days(a, z),
    };
    let (mut dc, mut ac, mut dsc) = (0.0, 0.0, 0.0);
    for k in 0..nc {
        let (p0, p1) = (q[k], q[k + 1]);
        let nl = match b.basis {
            1 => (p1 - p0) as f64,
            _ => days(p0, p1, true),
        };
        let end = o.mat.min(p1);
        dc = crate::x87::add(
            dc,
            crate::x87::div(days(p0, if k == nc - 1 { o.mat } else { end }, true), nl),
        );
        if o.settle > p1 && b.basis == 0 {
            ac = crate::x87::add(ac, 1.0);
        } else if o.settle > p0 {
            ac = crate::x87::add(ac, crate::x87::div(days(p0, o.settle.min(end), false), nl));
        }
        if o.settle < end {
            dsc = crate::x87::add(dsc, crate::x87::div(days(o.settle.max(p0), end, false), nl));
        }
    }
    (dc, ac, dsc)
}

pub fn oddlprice(ctx: &mut EvalCtx, a: &[Arg]) -> Value {
    atp::out((|| {
        let (o, rate, yld, redemption) = oddl_args(ctx, a)?;
        if yld < 0.0 {
            return Err(XlError::NUM);
        }
        let f = o.freq as f64;
        let (dc, ac, dsc) = oddl_sums(&o);

        use crate::x87::{add, div, mul, sub};
        let cf = div(mul(100.0, rate), f);
        let t1 = add(redemption, mul(dc, cf));
        let t2 = add(div(mul(dsc, yld), f), 1.0);
        finite(sub(div(t1, t2), mul(ac, cf)))
    })())
}

pub fn oddlyield(ctx: &mut EvalCtx, a: &[Arg]) -> Value {
    atp::out((|| {
        let (o, rate, pr, redemption) = oddl_args(ctx, a)?;
        if pr <= 0.0 {
            return Err(XlError::NUM);
        }
        let f = o.freq as f64;
        let (dc, ac, dsc) = oddl_sums(&o);
        use crate::x87::{add, div, mul, sub};
        let cf = div(mul(100.0, rate), f);
        let t1 = add(redemption, mul(dc, cf));
        let t2 = add(pr, mul(ac, cf));
        finite(mul(div(sub(t1, t2), t2), div(f, dsc)))
    })())
}
