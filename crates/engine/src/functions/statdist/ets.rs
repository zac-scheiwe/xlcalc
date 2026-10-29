use super::invnorm;
use super::{flat_values, R};
use crate::datetime::{self as dt, DateSystem};
use crate::eval::{EvalCtx, Operand};
use crate::value::{Value, XlError};
use crate::x87;

const EPS: f64 = 2.220446049250313e-16;

const SEAS: [(f64, f64, f64); 39] = [
    (0.751, 0.001, 0.001),
    (0.501, 0.001, 0.001),
    (0.251, 0.001, 0.001),
    (0.126, 0.001, 0.001),
    (0.998, 0.099, 0.001),
    (0.126, 0.125, 0.001),
    (0.998, 0.249, 0.001),
    (0.5, 0.25, 0.001),
    (0.251, 0.25, 0.001),
    (0.998, 0.499, 0.001),
    (0.501, 0.5, 0.001),
    (0.998, 0.749, 0.001),
    (0.751, 0.75, 0.001),
    (0.998, 0.899, 0.001),
    (0.833333, 0.0833333, 0.0833333),
    (0.166667, 0.0833333, 0.0833333),
    (0.833333, 0.75, 0.0833333),
    (0.9, 0.001, 0.099),
    (0.9, 0.899, 0.099),
    (0.002, 0.001, 0.125),
    (0.666667, 0.166667, 0.166667),
    (0.333333, 0.166667, 0.166667),
    (0.666667, 0.5, 0.166667),
    (0.75, 0.001, 0.249),
    (0.75, 0.749, 0.249),
    (0.251, 0.001, 0.25),
    (0.002, 0.001, 0.25),
    (0.5, 0.25, 0.25),
    (0.251, 0.25, 0.25),
    (0.5, 0.001, 0.499),
    (0.5, 0.499, 0.499),
    (0.002, 0.001, 0.5),
    (0.333333, 0.166667, 0.5),
    (0.25, 0.001, 0.749),
    (0.25, 0.249, 0.749),
    (0.002, 0.001, 0.75),
    (0.166667, 0.0833333, 0.75),
    (0.1, 0.001, 0.899),
    (0.1, 0.099, 0.899),
];

const NONS: [(f64, f64); 25] = [
    (0.9, 0.001),
    (0.75, 0.001),
    (0.5, 0.001),
    (0.25, 0.001),
    (0.126, 0.001),
    (0.1, 0.001),
    (0.002, 0.001),
    (0.833333, 0.0833333),
    (0.166667, 0.0833333),
    (0.998, 0.099),
    (0.1, 0.099),
    (0.126, 0.125),
    (0.666667, 0.166667),
    (0.333333, 0.166667),
    (0.998, 0.249),
    (0.25, 0.249),
    (0.5, 0.25),
    (0.998, 0.499),
    (0.5, 0.499),
    (0.666667, 0.5),
    (0.998, 0.749),
    (0.75, 0.749),
    (0.833333, 0.75),
    (0.998, 0.899),
    (0.9, 0.899),
];

#[derive(Clone, Copy, PartialEq, Eq)]
pub enum Kind {
    Value,
    Confint,
    Seasonality,
    Stat,
}

use x87::{add, div, mul, sub};

pub(super) fn fix(x: f64) -> i64 {
    x.trunc() as i64
}

pub(super) fn round_one(x: f64) -> f64 {
    if x == 0.0 || !x.is_finite() {
        return x;
    }
    if x > 0.0 {
        return add(x, 0.5).trunc();
    }
    let k = sub(x, 0.5).trunc();
    if sub(add(k, 0.5), x) == 0.0 {
        x.trunc()
    } else {
        k
    }
}

fn int_arg(ctx: &EvalCtx, o: Option<&Operand>, optional: bool, lo: i64, hi: i64) -> R<Option<i64>> {
    let v = match o {
        None | Some(Operand::Missing) => {
            return if optional { Ok(None) } else { Err(XlError::NA) };
        }
        Some(Operand::Val(v)) => ctx.to_number(v)?,
        Some(Operand::Ref(_)) => return Err(XlError::VALUE),
    };
    if v.fract() != 0.0 || !v.is_finite() || v < lo as f64 || v >= hi as f64 {
        return Err(XlError::NUM);
    }
    Ok(Some(v as i64))
}

fn series(ctx: &mut EvalCtx, o: Option<&Operand>) -> R<Vec<Value>> {
    match o {
        Some(Operand::Val(Value::Error(e))) => Err(*e),
        Some(op @ Operand::Val(Value::Array(_))) => flat_values(ctx, op),
        Some(op @ Operand::Ref(r)) if r.single().is_some() => flat_values(ctx, op),
        _ => Err(XlError::VALUE),
    }
}

pub fn ets(ctx: &mut EvalCtx, o: &[Operand], kind: Kind) -> Operand {
    let r = (|| -> R<f64> {
        let mut i = 0;
        let target = if matches!(kind, Kind::Value | Kind::Confint) {
            i = 1;
            match &o[0] {
                Operand::Val(v) => ctx.to_number(v)?,
                Operand::Missing => 0.0,
                Operand::Ref(_) => return Err(XlError::VALUE),
            }
        } else {
            0.0
        };
        let vals = series(ctx, o.get(i))?;
        let times = series(ctx, o.get(i + 1))?;
        if vals
            .iter()
            .chain(&times)
            .any(|v| matches!(v, Value::Error(XlError::CIRC)))
        {
            return Err(XlError::CIRC);
        }
        if vals.len() != times.len() {
            return Err(XlError::NA);
        }
        let mut j = i + 2;
        let mut cl = 0.95;
        if kind == Kind::Confint {
            if let Some(Operand::Val(v)) = o.get(j) {
                cl = ctx.to_number(v)?;
                if !(0.0..1.0).contains(&cl) {
                    return Err(XlError::NUM);
                }
            } else if let Some(Operand::Ref(_)) = o.get(j) {
                return Err(XlError::VALUE);
            }
            j += 1;
        }
        let mut stat = 0;
        if kind == Kind::Stat {
            stat = int_arg(ctx, o.get(j), false, 1, 9)?.unwrap();
            j += 1;
        }
        let mut seas = 1;
        if kind != Kind::Seasonality {
            seas = int_arg(ctx, o.get(j), true, 0, 8785)?.unwrap_or(1);
            j += 1;
        }
        let completion = int_arg(ctx, o.get(j), true, 0, 2)?.unwrap_or(1);
        let aggregation = int_arg(ctx, o.get(j + 1), true, 1, 8)?.unwrap_or(1);
        let f = Forecaster::prepare(
            &vals,
            &times,
            seas,
            completion == 1,
            aggregation as u8,
            ctx.wb.settings.date_system,
        )?;
        match kind {
            Kind::Value => f.value(target),
            Kind::Confint => {
                let c = f.confidence(target)?;
                let p = add(1.0, -div(sub(1.0, cl), 2.0));
                Ok(mul(invnorm::norm_s_inv(p), c))
            }
            Kind::Seasonality => Ok(f.seasonality as f64),
            Kind::Stat => Ok(f.statistic(stat as u8)),
        }
    })();
    Operand::Val(match r {
        Ok(x) if x.is_finite() => Value::num(x),
        Ok(_) => Value::Error(XlError::NUM),
        Err(e) => Value::Error(e),
    })
}

fn copy_pairs(vals: &[Value], times: &[Value], zero_fill: bool, aggr: u8) -> R<Vec<(f64, f64)>> {
    let count_mode = aggr == 2 || aggr == 3;
    let counta = aggr == 3;
    let mut out = Vec::with_capacity(vals.len());
    for (v, t) in vals.iter().zip(times) {
        let v = match v {
            Value::Number(x) => Some(*x),
            Value::Error(e) => return Err(*e),
            Value::Blank => zero_fill.then_some(0.0),
            _ => counta.then_some(0.0),
        };
        let t = match t {
            Value::Number(x) => Some(*x),
            Value::Error(e) => return Err(*e),
            _ => None,
        };
        if let (Some(v), Some(t)) = (v, t) {
            out.push((t, if count_mode { 1.0 } else { v }));
        }
    }
    if out.len() < 2 {
        return Err(XlError::DIV0);
    }
    out.sort_by(|a, b| a.0.partial_cmp(&b.0).unwrap());
    Ok(out)
}

fn is_dup(d: f64) -> bool {
    d.to_bits() & 0xFFFF_0000_0000_0000 == 0
}

fn detect_step(t: &[f64], sys: DateSystem) -> R<Step> {
    let n = t.len();

    let dates = date_valid(t[0], sys) && date_valid(t[n - 1], sys);
    let mut min = f64::MAX;
    let mut max = 0.0f64;
    let mut dups = 0usize;
    let diffs: Vec<f64> = (0..n - 1).map(|i| sub(t[i + 1], t[i])).collect();
    for &d in &diffs {
        if d < min {
            if is_dup(d) {
                dups += 1;
            } else {
                min = d;
            }
        }
        if d > max {
            max = d;
        }
    }
    if max == min || (dups != 0 && max == 0.0) {
        if max == 0.0 {
            return Err(XlError::NUM);
        }
        let (months, days) = if dates {
            unique_months(max, (n - dups - 1) as i32)
        } else {
            (0, 0.0)
        };
        return Ok(Step {
            step: max,
            regrid: dups != 0,
            months,
            days,
        });
    }

    let res = if min == 0.0 || (min < 1e-5 && 1e-5 < max) {
        1e-5
    } else {
        min
    };

    let base = div(div(res, 5.0), 5.0);
    let lo = min;
    let range = sub(max, lo);
    let acc = add(div(range, base), 1.0);
    let (binsize, nbins) = if acc > 3000.0 {
        (div(range, 2999.0), 3000usize)
    } else {
        let k = fix(acc) as f64;
        let bs = div(range, k);
        (bs, fix(add(div(range, bs), 1.5)) as usize)
    };

    let mut counts = vec![0u32; nbins];
    let mut first = vec![0.0f64; nbins];
    let mut exact = vec![0u32; nbins];
    for &d in &diffs {
        if is_dup(d) {
            continue;
        }
        let b = (fix(div(sub(d, lo), binsize)).max(0) as usize).min(nbins - 1);
        if counts[b] == 0 {
            first[b] = d;
        }
        counts[b] += 1;
        if sub(first[b], d).abs() <= 3e-7 {
            exact[b] += 1;
        }
    }

    let (center, wsum) = max_center(&counts, lo, binsize);
    let step = if counts[center] > 0 && exact[center] == counts[center] {
        first[center]
    } else {
        add(mul(center as f64, binsize), lo)
    };
    let (months, days) = if dates {
        let a = add(mul(center as f64, binsize), lo);
        histogram_months(a, add(a, binsize), &counts, lo, binsize)
    } else {
        (0, 0.0)
    };
    let distinct = n - dups;
    if 100 * wsum as usize > (distinct - 1) * 50 {
        Ok(Step {
            step,
            regrid: wsum as usize != n - 1,
            months,
            days,
        })
    } else {
        Err(XlError::NUM)
    }
}

fn max_center(counts: &[u32], lo: f64, binsize: f64) -> (usize, u32) {
    let nb = counts.len() as i64;
    let c = |i: i64| counts[i as usize];
    let (mut center, mut best, mut wlo, mut whi);
    if nb < 5 {
        let total: u32 = counts.iter().sum();
        let mut bc = 0i64;
        for i in 1..nb {
            if c(bc) < c(i) {
                bc = i;
            }
        }
        return (bc as usize, total);
    }
    let w = div(lo, binsize);
    let mut left = 0i64;
    let mut sum = c(0);
    best = c(0);
    center = 0i64;
    wlo = 0i64;
    whi = 0i64;
    let mut r9 = 1i64;
    loop {
        if r9 < nb {
            sum += c(r9);
        }
        let thr = (5 * (r9 - left + 1) - (r9 + left) / 2) as f64;
        if w < thr {
            if left >= 0 {
                sum -= c(left);
            }
            left += 1;
        } else {
            left -= 1;
            if left >= 0 {
                sum += c(left);
            }
        }
        let cen = (r9 + left) / 2;
        if cen >= nb {
            break;
        }
        if best < sum || (best == sum && c(center) < c(cen)) {
            center = cen;
            best = sum;
            wlo = left;
            whi = r9;
        }
        r9 += 1;
    }
    wlo = wlo.max(0);
    whi = whi.min(nb - 1);
    let (mut m, mut mx) = (0i64, 0u32);
    for i in wlo..=whi {
        if mx < c(i) {
            mx = c(i);
            m = i;
        }
    }
    if m != center
        && (mx as i64 >= best as i64 - 1 || 15 * best as i64 > 100 * (best as i64 - mx as i64))
    {
        let k = fix(div(div(add(mul(m as f64, binsize), lo), 10.0), binsize));
        let a = (m - k).max(0);
        let b = if k + m < nb { k + m } else { nb - 1 };
        best = (a..=b).map(c).sum();
        center = m;
    }
    (center as usize, best)
}

#[derive(Clone, Copy)]
struct Step {
    step: f64,
    regrid: bool,
    months: i32,
    days: f64,
}

fn date_valid(x: f64, sys: DateSystem) -> bool {
    x >= 0.0 && x.trunc() <= sys.max_serial()
}

fn fix32(x: f64) -> i32 {
    let t = x.trunc();
    if t.is_nan() || t >= 4_294_967_296.0 {
        -1
    } else {
        t as u64 as u32 as i32
    }
}

fn lfix(x: f64) -> i32 {
    x.trunc() as i32
}

fn overlaps(a: f64, b: f64, lo: f64, hi: f64) -> bool {
    let l = if a > lo {
        -1
    } else if b < lo {
        1
    } else {
        0
    };
    let h = if hi < a {
        -1
    } else if hi > b {
        1
    } else {
        0
    };
    !(l == h && l != 0)
}

fn occurrences(counts: &[u32], lo: f64, bs: f64, a: f64, b: f64) -> u32 {
    let nb = counts.len() as i32;
    let x = div(sub(a, lo), bs);
    let mut i = if x < 0.0 { -1 } else { fix32(x) };
    let y = div(sub(b, lo), bs);
    let j0 = if y < 0.0 { -1 } else { fix32(y) };
    let mut j = j0;
    if i < 0 && j >= 0 {
        i = 0;
    }
    if j >= nb && i < nb {
        j = nb - 1;
    }
    let mut sum = 0;
    let mut e = i.max(0);
    while e <= j && e < nb {
        sum += counts[e as usize];
        e += 1;
    }
    sum
}

fn histogram_months(a: f64, b: f64, counts: &[u32], lo: f64, bs: f64) -> (i32, f64) {
    let occ = |x: f64, y: f64| occurrences(counts, lo, bs, x, y);
    if overlaps(a, b, 30.0, 31.0) {
        let all = occ(27.5, 31.5);
        if all > 1 {
            let short = occ(27.5, 29.5);
            let d30 = occ(29.5, 30.5);
            let d31 = occ(30.5, 31.5);
            let long = occ(29.5, 31.5);
            if (d31 == 0 && (d30 > 2 || short > 2))
                || (d30 == 0 && (d31 > 4 || short > 2))
                || (short > 2 && short + long == all && long < 10)
            {
                return (0, 0.0);
            }
        }
        return (1, 30.5);
    }
    if overlaps(a, b, 363.0, 367.0) {
        return (12, 365.0);
    }
    if overlaps(a, b, 90.0, 92.0) {
        let all = occ(89.5, 92.5);
        if all > 1 {
            let short = occ(89.5, 90.5);
            let d91 = occ(90.5, 91.5);
            let d92 = occ(91.5, 92.5);
            let long = occ(90.5, 92.5);
            if (d91 == 0 && (d92 > 3 || short > 2))
                || (d92 == 0 && (d91 > 3 || short > 2))
                || (short > 2 && short + long == all && long < 2)
            {
                return (0, 0.0);
            }
        }
        return (3, 91.0);
    }
    if overlaps(a, b, 181.5, 184.5) {
        let all = occ(89.5, 92.5);
        if all > 1 {
            let d181 = occ(180.5, 181.5);
            let d182 = occ(181.5, 182.5);
            let d184 = occ(183.5, 184.5);
            if (d181 == 0 && (d184 > 2 || d182 > 2))
                || (d184 == 0 && (d181 > 2 || d182 > 2))
                || (d182 > 2 && d184 + d182 + d181 == all && d184 + d181 < 6)
            {
                return (0, 0.0);
            }
        }
        return (6, 183.0);
    }
    if a > 365.0 {
        return several_years(a, b);
    }
    (0, 0.0)
}

fn unique_months(s: f64, count: i32) -> (i32, f64) {
    if overlaps(s, s, 30.0, 31.0) {
        let ok = if s > 29.5 { count <= 3 } else { count <= 2 };
        return if ok { (1, 30.5) } else { (0, 0.0) };
    }
    if overlaps(s, s, 363.0, 367.0) {
        return (12, 365.0);
    }
    if overlaps(s, s, 90.0, 92.0) {
        let ok = if s > 90.5 { count <= 3 } else { count <= 2 };
        return if ok { (3, 91.0) } else { (0, 0.0) };
    }
    if overlaps(s, s, 181.5, 184.5) {
        return if count <= 2 { (6, 183.0) } else { (0, 0.0) };
    }
    if s > 365.0 {
        return several_years(s, s);
    }
    (0, 0.0)
}

fn several_years(a: f64, b: f64) -> (i32, f64) {
    let y = fix32(round_one(div(a, 365.0)));
    if !(2..=100).contains(&y) {
        return (0, 0.0);
    }
    let d = (y as f64 * 365.0 + (y / 4) as f64) as i64;
    let w = ((y as f64).log10() + 2.0) as i64;
    if overlaps(a, b, (d - w) as f64, (d + w) as f64) {
        (12 * y, d as f64)
    } else {
        (0, 0.0)
    }
}

fn month_walk(a: f64, b: f64, months: i32, sys: DateSystem) -> (f64, i64) {
    let (dir, m) = if a < b { (1, months) } else { (-1, -months) };
    let mut out = a;
    let mut cnt = 0i64;
    let mut best = sub(a, b).abs();
    let mut d = Dtr::from_serial(a, sys);
    loop {
        d.add_months(m);
        let cur = d.num(sys);
        let dd = sub(cur, b).abs();
        if dd < best {
            out = cur;
            cnt += dir;
            best = dd;
        } else {
            return (out, cnt);
        }
    }
}

#[derive(Clone, Copy)]
struct Dtr {
    y: i64,
    m: i64,
    d: i64,
    h: f64,
    mi: f64,
    s: f64,
}

impl Dtr {
    fn from_serial(x: f64, sys: DateSystem) -> Dtr {
        let y = add(x, 0.5 / 86_400.0);
        let day = y.trunc();
        let (yy, mm, dd) = dt::ymd_from_serial(sys, day as i64);
        let hh = mul(sub(y, day), 24.0);
        let h = hh.trunc();
        let m60 = mul(sub(hh, h), 60.0);
        let mi = m60.trunc();
        let s = mul(sub(m60, mi), 60.0).trunc();
        Dtr {
            y: yy,
            m: mm as i64,
            d: dd as i64,
            h,
            mi,
            s,
        }
    }

    fn add_months(&mut self, k: i32) {
        let total = self.y * 12 + self.m + k as i64 - 1;
        if total < 0 {
            *self = Dtr {
                y: 0,
                m: 0,
                d: 0,
                h: 0.0,
                mi: 0.0,
                s: 0.0,
            };
            return;
        }
        self.y = total / 12;
        self.m = total % 12 + 1;
    }

    fn num(&self, sys: DateSystem) -> f64 {
        if self.m == 0 {
            return 0.0;
        }
        let days = (dt::serial_from_ymd(sys, self.y, self.m, 1) + self.d - 1) as f64;
        let secs = add(mul(add(mul(self.h, 60.0), self.mi), 60.0), self.s);
        add(days, div(secs, 86_400.0))
    }
}

struct Corrected {
    times: Vec<f64>,
    vals: Vec<f64>,
    missing: usize,
}

fn aggregate(p: &[(f64, f64)], top: usize, low: f64, aggr: u8, count_mode: bool) -> (f64, usize) {
    let mut b = top;
    while b > 0 && p[b - 1].0 >= low {
        b -= 1;
    }
    if b == top {
        return (if count_mode { 1.0 } else { p[top].1 }, top);
    }
    let vals = || (b..=top).rev().map(|i| p[i].1);
    let cnt = (top - b + 1) as f64;
    let v = match aggr {
        1 | 7 => {
            let s = vals().fold(0.0, add);
            if aggr == 1 {
                div(s, cnt)
            } else {
                s
            }
        }
        2 | 3 => cnt,
        4 => vals().fold(f64::NEG_INFINITY, f64::max),
        6 => vals().fold(f64::INFINITY, f64::min),
        _ => {
            let mut v: Vec<f64> = vals().collect();
            v.sort_by(|a, b| a.partial_cmp(b).unwrap());
            let mid = v.len() / 2;
            if v.len() % 2 == 1 {
                v[mid]
            } else {
                div(add(v[mid], v[mid - 1]), 2.0)
            }
        }
    };
    (v, b)
}

fn closest(p: &[(f64, f64)], from: usize, x: f64) -> (usize, f64) {
    let mut bi = from;
    let mut bd = sub(p[from].0, x).abs();
    let mut i = from;
    while i > 0 {
        let d = sub(p[i - 1].0, x).abs();
        if d < bd {
            bd = d;
            bi = i - 1;
            i -= 1;
        } else {
            break;
        }
    }
    (bi, bd)
}

fn correct(
    p: &[(f64, f64)],
    st: &Step,
    sys: DateSystem,
    interpolate: bool,
    aggr: u8,
) -> R<Corrected> {
    let step = st.step;
    let months = st.months;
    let n = p.len();
    let count_mode = aggr == 2 || aggr == 3;
    let tol = div(step, 2.0);
    let tol2 = div(step, 10.0);

    let mut end = n - 1;
    while end > 0 && p[end - 1].0 == p[end].0 {
        end -= 1;
    }
    let mut table: [Option<usize>; 21] = [None; 21];
    let mut maxrun = 0usize;
    let mut run = 0usize;
    let mut start = 0usize;
    let mut later = end;
    let mut k = end;
    while k > 0 {
        let mut e = k - 1;
        while e > 0 && p[e - 1].0 == p[e].0 {
            e -= 1;
        }
        let d = sub(sub(p[later].0, p[e].0), step).abs();
        if d <= tol2 {
            run += 1;
            if run == 1 {
                start = later;
            }
            if run >= 20 {
                break;
            }
        } else {
            if run >= 2 && run > maxrun {
                let s = run.min(20);
                if table[s].is_none() {
                    table[s] = Some(start);
                }
                maxrun = run;
            }
            run = 0;
        }
        later = e;
        k = e;
    }
    if run >= 2 && run > maxrun {
        let s = run.min(20);
        if table[s].is_none() {
            table[s] = Some(start);
        }
        maxrun = run;
    }
    if run == 1 && maxrun == 0 {
        table[2] = Some(start);
        maxrun = 2;
    }
    if maxrun == 0 {
        return Err(XlError::NUM);
    }
    let (t0, tend) = (p[0].0, p[n - 1].0);
    let span = sub(tend, t0);
    let mut pick = None;
    let mut best = f64::NEG_INFINITY;
    for k in (2..=maxrun.min(20)).rev() {
        if let Some(i) = table[k] {
            let score = mul(sub(1.0, div(sub(tend, p[i].0), span)), k as f64);
            if score > best {
                best = score;
                pick = Some(i);
            }
        }
    }
    let pi = pick.ok_or(XlError::NUM)?;
    let mut rows: Vec<(f64, f64)> = Vec::new();
    let mut missing = 0usize;
    let (grid, ins) = if pi == end {
        (tend, tend)
    } else {
        let tp = p[pi].0;
        let a = if months > 0 {
            month_walk(tp, tend, months, sys).0
        } else {
            add(tp, mul(round_one(div(sub(tend, tp), step)), step))
        };
        let (ci, cd) = closest(p, n - 1, a);
        if cd <= tol2 {
            (p[ci].0, p[ci].0)
        } else {
            (a, p[ci].0)
        }
    };
    let (v, mut ptr) = aggregate(p, n - 1, sub(grid, tol), aggr, count_mode);
    rows.push((ins, v));
    let mut prev = grid;
    let mut prevval = v;

    while ptr > 0 {
        let j = ptr - 1;
        let t = p[j].0;
        let low = sub(sub(prev, step), tol);
        let (gap, mut g) = if months > 0 {
            let (g, c) = month_walk(prev, t, months, sys);
            (-c, g)
        } else if t >= low {
            (1i64, sub(prev, step))
        } else {
            let gap = fix(round_one(div(sub(prev, t), step)));
            (gap, sub(prev, mul(gap as f64, step)))
        };
        let (ci, cd) = closest(p, j, g);

        let ins = if cd <= tol2 {
            g = p[ci].0;
            g
        } else {
            p[ci].0
        };
        let (v, b) = aggregate(p, j, sub(g, tol), aggr, count_mode);
        if gap > 1 && months > 0 {
            let mut d = Dtr::from_serial(prev, sys);
            for c in (1..gap).rev() {
                d.add_months(-months);
                let fv = if interpolate {
                    add(div(mul(c as f64, sub(prevval, v)), gap as f64), v)
                } else {
                    0.0
                };
                rows.push((d.num(sys), fv));
                missing += 1;
            }
        } else if gap > 1 {
            let mut tt = prev;
            for _ in 1..gap {
                tt = sub(tt, step);
                let fv = if interpolate {
                    add(div(mul(sub(tt, g), sub(prevval, v)), sub(prev, g)), v)
                } else {
                    0.0
                };
                rows.push((tt, fv));
                missing += 1;
            }
        }
        rows.push((ins, v));
        prev = g;
        prevval = v;
        ptr = b;
    }
    rows.reverse();
    let count = rows.len();
    if missing * 100 > 30 * count {
        return Err(XlError::NUM);
    }
    if count < 2 {
        return Err(XlError::DIV0);
    }
    Ok(Corrected {
        times: rows.iter().map(|r| r.0).collect(),
        vals: rows.iter().map(|r| r.1).collect(),
        missing,
    })
}

fn ols_slope(y: &[f64], season: Option<(&[f64], usize)>) -> f64 {
    let nn = y.len();
    if nn <= 1 {
        return 0.0;
    }
    let (mut sy, mut sty) = (0.0, 0.0);
    for (i, &v) in y.iter().enumerate() {
        let v = match season {
            Some((s, m)) => sub(v, s[m - (i % m) + 1]),
            None => v,
        };
        sy = add(sy, v);
        sty = add(mul((i + 1) as f64, v), sty);
    }
    let n = nn as f64;
    let st = round_one(div(mul(add(n, 1.0), n), 2.0));
    let stt = round_one(mul(mul(div(n, 3.0), add(n, 1.0)), add(n, 0.5)));
    let den = sub(mul(n, stt), mul(st, st));
    div(sub(mul(n, sty), mul(st, sy)), den)
}

fn windows(nn: usize, m: usize) -> (usize, usize) {
    let mut a = if nn <= 10 {
        nn
    } else {
        fix(mul(0.8, nn as f64)) as usize
    };
    let mut b = nn - a;
    if b != 0 {
        let c = fix(mul(0.4, nn as f64)) as usize;
        b = b.max(2 * m).max(10).min(c);
        a = nn - b;
    }
    (a, b)
}

fn complete(y0: &[f64], m: usize, seasonal: bool) -> Vec<f64> {
    let n = y0.len();
    let mut y = y0.to_vec();
    if 2 * n > 7 * m {
        return y;
    }
    let mut extra = mul((7 * m - 2 * n) as f64, 0.5).ceil() as usize;

    let slope = if n < m {
        let last = y[n - 1];
        y.resize(m, last);
        extra -= m - n;
        0.0
    } else if !seasonal {
        ols_slope(y0, None)
    } else {
        let mut s = 0.0;
        for i in 0..m {
            let sub_: Vec<f64> = (0..4)
                .map(|k| i + k * m)
                .filter(|&j| j < n)
                .map(|j| y0[j])
                .collect();
            s = add(s, ols_slope(&sub_, None));
        }
        div(div(s, m as f64), m as f64)
    };
    for i in 0..extra {
        let j = (n + i) % m;
        let v = add(mul(sub((n + i) as f64, j as f64), slope), y[j]);
        y.push(v);
    }
    y
}

fn normalize(s: &mut [f64], m: usize) {
    let mut sum = 0.0;
    for &v in &s[2..m + 2] {
        sum = add(sum, v);
    }
    let mean = div(sum, m as f64);
    s[0] = add(s[0], mean);
    for v in &mut s[2..m + 2] {
        *v = sub(*v, mean);
    }
}

struct Model {
    m: usize,
    w: Vec<f64>,
    alpha: f64,
    beta: f64,
    gamma: f64,
    mse: f64,
    mase: f64,
    smape: f64,
    mae: f64,
    rmse: f64,
    slope0: f64,
}

fn train(y0: &[f64], seasonality: usize) -> Model {
    let n = y0.len();
    let m = seasonality.max(2);
    let seasonal = seasonality > 1;
    let y = complete(y0, m, seasonal);
    let nn = y.len();
    let (a, _) = windows(nn, m);
    let (an, _) = windows(n, m);
    let mut s = vec![0.0; m + 2];
    s[0] = y[0];
    if seasonal {
        let k = (5 * m).min(nn);
        let mut t = vec![0.0; k.saturating_sub(2 * m)];
        let mut acc = 0.0;
        for r in 0..k {
            if r >= 2 * m {
                t[r - 2 * m] = div(acc, (2 * m) as f64);
                acc = sub(acc, y[r - 2 * m]);
            }
            acc = add(acc, y[r]);
        }
        let c = m - m / 2;
        for p in 0..m {
            let (mut sum, mut cnt) = (0.0, 0usize);
            let mut j = p;
            while j + 2 * m < k {
                cnt += 1;
                sum = sub(add(sum, y[j - 1 + m / 2]), t[j]);
                j += m;
            }
            let avg = if cnt > 0 { div(sum, cnt as f64) } else { 0.0 };
            s[2 + (c as i64 - p as i64).rem_euclid(m as i64) as usize] = avg;
        }
        s[0] = sub(y[0], s[m + 1]);
    }
    normalize(&mut s, m);
    s[1] = ols_slope(&y, Some((&s, m)));
    s[0] = sub(s[0], s[1]);

    let mut best = mul(f64::MAX, 0.1);
    let table: Vec<(f64, f64, f64)> = if seasonal {
        SEAS.to_vec()
    } else {
        NONS.iter().map(|&(a, b)| (a, b, EPS)).collect()
    };
    let mut ab = table[0];
    for &(al, be, ga) in &table {
        let mut sse = 0.0;
        let mut w = s.clone();
        let mut idx = m + 1;
        let thr = div(best, 0.1);
        for (r, &yr) in y.iter().enumerate().take(nn) {
            let f = add(add(w[0], w[1]), w[idx]);
            let e = sub(yr, f);
            sse = add(mul(e, e), sse);
            if sse > thr {
                break;
            }
            if r < a {
                w[0] = add(add(mul(e, al), w[1]), w[0]);
                w[1] = add(mul(e, be), w[1]);
                w[idx] = add(mul(e, ga), w[idx]);
            } else {
                w[0] = add(w[0], w[1]);
            }
            idx = if idx == 2 { m + 1 } else { idx - 1 };
        }
        let cand = mul(sse, 0.1);
        if cand < best {
            best = cand;
            ab = (al, be, ga);
            if best == 0.0 {
                break;
            }
        }
    }
    let (al, mut be, ga) = ab;

    let mut w = s.clone();
    let mut idx = m + 1;
    let (k0, kd) = if n > 10 { (n / 2, n / 2) } else { (0, n) };
    let mut sse = 0.0;
    let (mut sa, mut sb, mut sc, mut sd) = (0.0, 0.0, 0.0, 0.0);
    let (mut mae, mut msq) = (0.0, 0.0);
    for r in 0..n {
        let f = add(add(w[0], w[1]), w[idx]);
        let e = sub(y[r], f);
        sse = add(mul(e, e), sse);
        w[0] = add(add(mul(e, al), w[1]), w[0]);
        w[1] = add(mul(e, be), w[1]);
        w[idx] = add(mul(e, ga), w[idx]);
        idx = if idx == 2 { m + 1 } else { idx - 1 };
        if r > k0 {
            sa = add(w[1], sa);
            sb = add(mul(w[1], w[1]), sb);
        }
        if r != 0 && (r < an || an == n) {
            sc = add(sub(y[r], y[r - 1]).abs(), sc);
        }
        if r >= an || an == n {
            if e != 0.0 {
                mae = add(mae, e.abs());
                msq = add(msq, mul(e.abs(), e.abs()));
            }
            if f != 0.0 || y[r] != 0.0 {
                sd = add(div(e, add(f.abs(), y[r].abs())).abs(), sd);
            }
        }
    }
    let mut nw = w.clone();
    let sh = m + 1 - idx;
    for r9 in 0..m {
        nw[2 + (r9 + sh) % m] = w[2 + r9];
    }
    let mut w = nw;
    let mean = div(sa, kd as f64);
    let sdv = if kd > 1 {
        let k1 = (kd - 1) as f64;
        x87::sqrt(div(sub(sb, div(mul(sa, sa), k1)).abs(), k1))
    } else {
        0.0
    };
    let cv_big = mean == 0.0 || div(sdv, mean).abs() > 1.0;
    if cv_big {
        w[0] = sub(y[n - 1], w[2]);
        w[1] = s[1];
        be = 0.001;
    }

    let mse = div(sse, n as f64);
    let sc = if an > 1 { div(sc, (an - 1) as f64) } else { sc };
    let cnt = if an != n { n - an } else { n } as f64;
    let mae = div(mae, cnt);
    let rmse = x87::sqrt(div(msq, cnt));
    let mase = if sc != 0.0 { div(mae, sc) } else { 0.0 };
    let smape = div(mul(sd, 2.0), cnt);
    normalize(&mut w, m);
    Model {
        m,
        w,
        alpha: al,
        beta: be,
        gamma: ga,
        mse,
        mase,
        smape,
        mae,
        rmse,
        slope0: s[1].abs(),
    }
}

impl Model {
    fn forecast(&self, h: i64) -> f64 {
        let m = self.m as i64;
        let c = if h > 0 { h } else { h + (1 - h / m) * m };
        let idx = (m + 1 - (c - 1) % m) as usize;
        add(add(mul(h as f64, self.w[1]), self.w[0]), self.w[idx])
    }

    fn variance(&self, h: i64) -> f64 {
        if h == 1 {
            return self.mse;
        }
        let (al, be, ga) = (self.alpha, self.beta, self.gamma);
        let hh = h as f64;
        let q = ((h - 1) as usize / self.m) as f64;
        let t = (2 * h - 1) as f64;
        let x = div(mul(mul(mul(hh, t), be), be), 6.0);
        let a2 = mul(al, al);
        let v1 = add(
            mul(add(add(mul(mul(hh, al), be), x), a2), (h - 1) as f64),
            1.0,
        );
        let g = add(mul(mul(add(q, 1.0), self.m as f64), be), ga);
        let v = add(mul(mul(add(mul(2.0, al), g), ga), q), v1);
        mul(v, self.mse)
    }
}

struct Forecaster {
    step: f64,

    months: i32,
    days: f64,
    sys: DateSystem,
    last: f64,
    count: usize,
    seasonality: usize,
    model: Model,
}

impl Forecaster {
    fn prepare(
        vals: &[Value],
        times: &[Value],
        seas: i64,
        interpolate: bool,
        aggr: u8,
        sys: DateSystem,
    ) -> R<Self> {
        let p = copy_pairs(vals, times, !interpolate, aggr)?;
        let t: Vec<f64> = p.iter().map(|x| x.0).collect();
        let st = detect_step(&t, sys)?;
        let Step {
            step,
            regrid,
            months,
            days,
        } = st;
        let c = if regrid {
            correct(&p, &st, sys, interpolate, aggr)?
        } else {
            Corrected {
                times: t.clone(),
                vals: p.iter().map(|x| x.1).collect(),
                missing: 0,
            }
        };
        let _ = c.missing;
        let count = c.vals.len();
        let mut seasonality = seas as usize;
        if seasonality == 1 {
            seasonality = super::ets_seas::detect(&c.vals);
        } else if seasonality >= count * 70 / 100 {
            seasonality = 0;
        }
        let model = train(&c.vals, seasonality);
        Ok(Forecaster {
            step,
            months,
            days,
            sys,
            last: *c.times.last().unwrap(),
            count,
            seasonality,
            model,
        })
    }

    fn offset(&self, target: f64) -> (f64, bool) {
        if self.months > 0 {
            return self.month_offset(target);
        }
        let off = div(sub(target, self.last), self.step);
        let r = round_one(off);
        if mul(sub(r, off).abs(), 1000.0) <= 1.0 {
            (r, false)
        } else {
            (off, true)
        }
    }

    fn value(&self, target: f64) -> R<f64> {
        let (off, frac) = self.offset(target);
        let h = fix(off);
        if h <= -(self.count as i64) {
            return Err(XlError::NUM);
        }
        let mut v = self.model.forecast(h);
        if frac {
            let v1 = self.model.forecast(h + 1);
            v = interpolate(h, v, v1, off);
        }

        if v != 0.0 {
            let a = v.abs();
            let s = self.model.slope0;
            if a < 3e-7 && a < s && fix(x87::log10(div(s, a))) > 2 {
                v = 0.0;
            }
        }
        Ok(v)
    }

    fn confidence(&self, target: f64) -> R<f64> {
        let (off, frac) = self.offset(target);
        let h = fix(off);
        if h < 0 || (h == 0 && !frac) {
            return Err(XlError::NUM);
        }
        let mut v = if h > 0 { self.model.variance(h) } else { 0.0 };
        if frac {
            let v1 = self.model.variance(h + 1);
            v = interpolate(h, v, v1, off);
        }
        Ok(x87::sqrt(v))
    }

    fn month_offset(&self, target: f64) -> (f64, bool) {
        let mps = self.months;
        let mut k = lfix(div(sub(target, self.last), self.days).floor());
        let mut d = Dtr::from_serial(self.last, self.sys);
        d.add_months(k * mps);
        let mut cur = d.num(self.sys);
        while cur > target {
            if d.m == 0 {
                return (f64::NAN, true);
            }
            k -= 1;
            d.add_months(-mps);
            cur = d.num(self.sys);
        }
        if cur == target {
            return (k as f64, false);
        }
        let mut up = k;
        let mut next = cur;
        while next < target {
            up += 1;
            d.add_months(mps);
            next = d.num(self.sys);
        }
        if next == target {
            return (up as f64, false);
        }
        let prev = if up - 1 != k {
            k = up - 1;
            d.add_months(-mps);
            d.num(self.sys)
        } else {
            cur
        };
        (add(div(sub(target, prev), sub(next, prev)), k as f64), true)
    }

    fn statistic(&self, k: u8) -> f64 {
        let m = &self.model;
        match k {
            1 => m.alpha,
            2 => m.beta,
            3 => m.gamma,
            4 => m.mase,
            5 => m.smape,
            6 => m.mae,
            7 => m.rmse,
            _ => self.step,
        }
    }
}

fn interpolate(h: i64, v0: f64, v1: f64, off: f64) -> f64 {
    let d = sub((h + 1) as f64, h as f64);
    add(div(mul(sub(off, h as f64), sub(v1, v0)), d), v0)
}
