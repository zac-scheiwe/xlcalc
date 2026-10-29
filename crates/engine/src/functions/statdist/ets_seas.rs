use super::ets::{fix, round_one};
use crate::x87::{self, add, div, mul, sub};

fn sgn(a: f64, b: f64) -> i32 {
    let (x, y) = (a.to_bits() as i64, b.to_bits() as i64);
    let r = match x.cmp(&y) {
        std::cmp::Ordering::Less => -1,
        std::cmp::Ordering::Greater => 1,
        std::cmp::Ordering::Equal => return 0,
    };
    if (x & y) < 0 {
        -r
    } else {
        r
    }
}

fn neg(x: f64) -> f64 {
    if x.to_bits() & 0xFFFF_0000_0000_0000 != 0 {
        -x
    } else {
        x
    }
}

const TWO_PI: f64 = std::f64::consts::TAU;
const PI: f64 = std::f64::consts::PI;

fn fit(xs: &[i64], ys: &[f64]) -> (f64, f64) {
    let (mut sx, mut sy, mut sxx, mut sxy) = (0.0, 0.0, 0.0, 0.0);
    for (&x, &y) in xs.iter().zip(ys) {
        let xf = x as f64;
        sx = add(xf, sx);
        sxx = add(mul(xf, xf), sxx);
        sy = add(sy, y);
        sxy = add(mul(xf, y), sxy);
    }
    let n = xs.len() as f64;
    let den = sub(mul(n, sxx), mul(sx, sx));
    let slope = div(sub(mul(n, sxy), mul(sx, sy)), den);
    let icpt = sub(div(sy, n), div(mul(slope, sx), n));
    (slope, icpt)
}

fn fft(x: &[f64], inverse: bool) -> Vec<(f64, f64)> {
    let nn = x.len();
    let n = 2 * nn;
    let mut d = vec![0.0; n + 2];
    for (j, &v) in x.iter().enumerate() {
        d[2 * j + 1] = v;
    }
    let mut j = 1usize;
    let mut i = 1usize;
    while i < n {
        if j > i {
            d.swap(j, i);
            d.swap(j + 1, i + 1);
        }
        let mut m = nn;
        while m >= 2 && j > m {
            j -= m;
            m >>= 1;
        }
        j += m;
        i += 2;
    }
    let mut mmax = 2usize;
    while n > mmax {
        let istep = 2 * mmax;
        let mut theta = div(TWO_PI, mmax as f64);
        if inverse {
            theta = neg(theta);
        }
        let wpi = x87::sin(theta);
        let s = x87::sin(mul(theta, 0.5));
        let wpr = mul(mul(s, s), -2.0);
        let (mut wr, mut wi) = (1.0, 0.0);
        let mut m = 1;
        while m < mmax {
            let mut i = m;
            while i <= n {
                let j = i + mmax;
                let tempr = sub(mul(wr, d[j]), mul(wi, d[j + 1]));
                let tempi = add(mul(wr, d[j + 1]), mul(wi, d[j]));
                d[j] = sub(d[i], tempr);
                d[j + 1] = sub(d[i + 1], tempi);
                d[i] = add(d[i], tempr);
                d[i + 1] = add(d[i + 1], tempi);
                i += istep;
            }
            let wtemp = wr;
            wr = add(sub(mul(wpr, wr), mul(wpi, wi)), wr);
            wi = add(add(mul(wpr, wi), mul(wpi, wtemp)), wi);
            m += 2;
        }
        mmax = istep;
    }
    (0..nn).map(|k| (d[2 * k + 1], neg(d[2 * k + 2]))).collect()
}

fn dft(x: &[f64]) -> Vec<(f64, f64)> {
    let l = x.len();
    if l.is_power_of_two() {
        return fft(x, false);
    }
    let step = div(TWO_PI, l as f64);
    let mut theta = 0.0;
    let mut w = Vec::with_capacity(l);
    for _ in 0..l {
        w.push((x87::cos(theta), neg(x87::sin(theta))));
        theta = add(theta, step);
    }
    let mut out = vec![(0.0, 0.0); l];
    for (k, o) in out.iter_mut().enumerate() {
        for (j, &xj) in x.iter().enumerate() {
            let c = w[(j * k) % l];
            o.0 = add(o.0, mul(c.0, xj));
            o.1 = add(o.1, mul(c.1, xj));
        }
    }
    out
}

struct Spectrum {
    pairs: Vec<(f64, f64)>,
    mean: f64,
    sd: f64,
}

fn spectrum(y: &[f64]) -> Spectrum {
    let l = y.len();
    let k0 = 1 - ((l as i64 + 1) / 2);
    let (mut sy, mut sky) = (0.0, 0.0);
    for (i, &v) in y.iter().enumerate() {
        sy = add(sy, v);
        sky = add(mul(v, (i as i64 + k0) as f64), sky);
    }
    let mean = div(sy, l as f64);
    let den = ((l * l - 1) as u32 as f64) * (2.0 * l as f64);
    let slope = div(sky, div(den, 24.0));
    let mut x: Vec<f64> = y
        .iter()
        .enumerate()
        .map(|(i, &v)| add(neg(add(mul((i as i64 + k0) as f64, slope), mean)), v))
        .collect();

    let m = fix(mul(l as f64, 0.1)) as usize;
    let dd = mul(m as f64, 2.0);
    for i in 0..m {
        let c = x87::cos(div(mul((2 * i + 1) as f64, PI), dd));
        let w = mul(add(neg(c), 1.0), 0.5);
        x[i] = mul(w, x[i]);
        x[l - 1 - i] = mul(w, x[l - 1 - i]);
    }

    let f = dft(&x);
    let norm = mul(l as f64, 0.875);
    let half = l / 2;
    let (mut sum, mut sumsq) = (0.0, 0.0);
    let mut pairs = Vec::with_capacity(half);
    for (k, &(re, im)) in f.iter().enumerate().take(half + 1).skip(1) {
        let p = div(add(mul(im, im), mul(re, re)), norm);
        sum = add(p, sum);
        sumsq = add(mul(p, p), sumsq);
        pairs.push((div(k as f64, l as f64), p));
    }
    let mean = if half > 0 { div(sum, half as f64) } else { sum };
    let sd = if half > 1 {
        x87::sqrt(sub(div(sumsq, (half - 1) as f64), mul(mean, mean)))
    } else {
        0.0
    };
    Spectrum { pairs, mean, sd }
}

fn correlation(x: &[f64]) -> Vec<f64> {
    let n = x.len();
    let size = if n.is_power_of_two() {
        2 * n
    } else {
        4 * (1usize << (usize::BITS - 1 - n.leading_zeros()))
    };
    let mut s = 0.0;
    for &v in x {
        s = add(v, s);
    }
    let mean = div(s, n as f64);
    let mut buf = vec![0.0; size];
    for (b, &v) in buf.iter_mut().zip(x) {
        *b = if sgn(mean, 3e-7) == 1 {
            sub(v, mean)
        } else {
            v
        };
    }
    let fbn = |b: &[f64]| -> Vec<f64> {
        let f = fft(b, false);
        let p: Vec<f64> = f
            .iter()
            .map(|&(re, im)| add(mul(re, re), mul(im, im)))
            .collect();
        fft(&p, true).iter().map(|c| c.0).collect()
    };
    let acf = fbn(&buf);
    let mut mask = vec![0.0; size];
    mask[..n].iter_mut().for_each(|m| *m = 1.0);
    let mk = fbn(&mask);
    let nf = n as f64;
    let c0 = div(div(acf[0], mk[0]), nf);
    let mut r = vec![1.0; n];
    for k in 1..n {
        r[k] = div(div(div(acf[k], nf), mk[k]), c0);
    }
    r
}

#[derive(Clone, Copy)]
struct Peak {
    per: i64,
    power: f64,
    corr: f64,
}

fn crt_qsort<T: Copy>(v: &mut [T], gt: impl Fn(&T, &T) -> bool) {
    let eq = |a: &T, b: &T| !gt(a, b) && !gt(b, a);
    if v.len() < 2 {
        return;
    }
    let mut stack: Vec<(usize, usize)> = Vec::new();
    let (mut lo, mut hi) = (0usize, v.len() - 1);
    loop {
        let size = hi - lo + 1;
        if size <= 8 {
            let mut h = hi;
            while h > lo {
                let mut mx = lo;
                for p in lo + 1..=h {
                    if gt(&v[p], &v[mx]) {
                        mx = p;
                    }
                }
                v.swap(mx, h);
                h -= 1;
            }
        } else {
            let mid = lo + size / 2;
            if gt(&v[lo], &v[mid]) {
                v.swap(lo, mid);
            }
            if gt(&v[lo], &v[hi]) {
                v.swap(lo, hi);
            }
            if gt(&v[mid], &v[hi]) {
                v.swap(mid, hi);
            }
            let (mut loguy, mut higuy) = (lo as isize, hi as isize);
            let mut midv = mid as isize;
            loop {
                if midv > loguy {
                    loop {
                        loguy += 1;
                        if !(loguy < midv && !gt(&v[loguy as usize], &v[midv as usize])) {
                            break;
                        }
                    }
                }
                if midv <= loguy {
                    loop {
                        loguy += 1;
                        if !(loguy <= hi as isize && !gt(&v[loguy as usize], &v[midv as usize])) {
                            break;
                        }
                    }
                }
                loop {
                    higuy -= 1;
                    if !(higuy > midv && gt(&v[higuy as usize], &v[midv as usize])) {
                        break;
                    }
                }
                if higuy < loguy {
                    break;
                }
                v.swap(loguy as usize, higuy as usize);
                if midv == higuy {
                    midv = loguy;
                }
            }
            higuy += 1;
            if midv < higuy {
                loop {
                    higuy -= 1;
                    if !(higuy > midv && eq(&v[higuy as usize], &v[midv as usize])) {
                        break;
                    }
                }
            }
            if midv >= higuy {
                loop {
                    higuy -= 1;
                    if !(higuy > lo as isize && eq(&v[higuy as usize], &v[midv as usize])) {
                        break;
                    }
                }
            }
            if higuy - lo as isize >= hi as isize - loguy {
                if (lo as isize) < higuy {
                    stack.push((lo, higuy as usize));
                }
                if loguy < hi as isize {
                    lo = loguy as usize;
                    continue;
                }
            } else {
                if loguy < hi as isize {
                    stack.push((loguy as usize, hi));
                }
                if (lo as isize) < higuy {
                    hi = higuy as usize;
                    continue;
                }
            }
        }
        match stack.pop() {
            Some((l, h)) => {
                lo = l;
                hi = h;
            }
            None => return,
        }
    }
}

struct Detector<'a> {
    len: usize,
    corr: &'a [f64],
}

impl Detector<'_> {
    fn lagged(&self, k: i64) -> f64 {
        if k >= 0 && (k as usize) < self.corr.len() {
            self.corr[k as usize]
        } else {
            0.0
        }
    }

    fn check_peak(&self, lo: i64, hi: i64, flag: bool) -> Option<Peak> {
        let mut best = -2147483648.0;
        let mut bl = 0i64;
        let mut buf = Vec::new();
        for lag in lo..=hi {
            let v = self.lagged(lag);
            buf.push(v);
            if sgn(v, best) == 1 {
                best = v;
                bl = lag;
            }
        }
        let per = if flag {
            fix((bl as f64 / 2.0).floor())
        } else {
            bl
        };
        if per as f64 * 2.5 > self.len as f64 || bl == lo || bl == hi {
            return None;
        }
        if (3..=5).contains(&bl) || (flag && (6..=11).contains(&bl)) {
            return self.small_period(bl, per);
        }
        if hi - lo + 1 < 5 {
            return None;
        }
        self.large_period(lo, hi, per, &buf)
    }

    fn found(&self, per: i64) -> Peak {
        Peak {
            per,
            power: 0.0,
            corr: self.lagged(per),
        }
    }

    fn small_period(&self, lag: i64, per: i64) -> Option<Peak> {
        let w = if lag == 5 { 4 } else { 2 };
        let lags: Vec<i64> = (0..=2 * w).map(|i| lag - w + i).collect();
        let vals: Vec<f64> = lags.iter().map(|&l| self.lagged(l)).collect();
        let w = w as usize;
        let (s1, _) = fit(&lags[..=w], &vals[..=w]);
        let (s2, _) = fit(&lags[w + 1..], &vals[w + 1..]);
        if sgn(s1, 0.0) == 1 && sgn(s2, 0.0) == -1 {
            Some(self.found(per))
        } else {
            None
        }
    }

    fn large_period(&self, lo: i64, hi: i64, per: i64, buf: &[f64]) -> Option<Peak> {
        let (mut xs, mut ys) = (Vec::new(), Vec::new());
        for off in -2..=0 {
            let l = lo + off;
            if l >= 3 {
                xs.push(l);
                ys.push(self.lagged(l - 1));
            }
        }
        for off in 0..=2 {
            let l = hi + off;
            if l <= self.len as i64 {
                xs.push(l);
                ys.push(self.lagged(l - 1));
            }
        }
        let (bs, bi) = fit(&xs, &ys);
        let n = (hi - lo + 1) as usize;
        let lagv: Vec<i64> = (0..n as i64).map(|i| lo + i).collect();
        let det: Vec<f64> = (0..n)
            .map(|i| add(neg(add(mul(lagv[i] as f64, bs), bi)), buf[i]))
            .collect();
        let step = fix(div((n as i64 - 4) as f64, 100.0).ceil()) as usize;
        let sse = |r: std::ops::Range<usize>, s: f64, c: f64| {
            let mut t = 0.0;
            for i in r {
                let e = add(neg(add(mul(lagv[i] as f64, s), c)), det[i]);
                t = add(mul(e, e), t);
            }
            t
        };
        let mut best = 2147483647.0;
        let mut out = None;
        let mut found = false;
        let mut e = 2usize;
        while e + 2 < n {
            let (sa, ia) = fit(&lagv[..=e], &det[..=e]);
            let (sb, ib) = fit(&lagv[e..], &det[e..]);
            let a = div(sse(0..e + 1, sa, ia), (e + 1) as f64);
            let b = div(sse(e..n, sb, ib), (n - e) as f64);
            let tot = add(a, b);
            if sgn(tot, best) == -1 {
                best = tot;
                if sgn(sa, 0.0) == 1 && sgn(sb, 0.0) == -1 {
                    out = Some(self.found(per));
                    found = true;
                } else {
                    found = false;
                }
            }
            e += step.max(1);
        }
        if found {
            out
        } else {
            None
        }
    }
}

pub(super) fn detect(all: &[f64]) -> usize {
    let data = &all[all.len().saturating_sub(175_680)..];
    let mut d = data;
    if d.len() > 2048 {
        let p = 1usize << (usize::BITS - 1 - d.len().leading_zeros());
        d = &d[d.len() - p..];
    }
    let len = d.len();
    if len < 2 {
        return 0;
    }

    let xs: Vec<i64> = (1..=len as i64).collect();
    let (b, a) = fit(&xs, d);
    let mut resid = Vec::with_capacity(len);
    let (mut sum, mut sumsq) = (0.0, 0.0);
    for (i, &y) in d.iter().enumerate() {
        let r = add(neg(add(mul((i + 1) as f64, b), a)), y);
        resid.push(r);
        sum = add(r, sum);
        sumsq = add(mul(r, r), sumsq);
    }
    let mean = div(sum, len as f64);
    let sd = x87::sqrt(sub(div(sumsq, (len - 1) as f64), mul(mean, mean)));
    let std: Vec<f64> = if sgn(sd, 0.0) == 0 {
        resid.clone()
    } else {
        resid.iter().map(|&r| div(sub(r, mean), sd)).collect()
    };

    let mut p1 = 1i64;
    let mut p2 = 1i64;
    let mut top = 0.0;

    let sa = spectrum(&resid);
    let maxp = if 2 * len < 25 {
        (2 * len / 5) as i64
    } else {
        5
    };
    if len & !1 >= 4 {
        let pr = &sa.pairs;
        top = pr[0].1;
        let mut second = pr[1].1;
        let mut per = fix(round_one(div(1.0, pr[0].0)));
        if sgn(second, top) == 1 {
            second = top;
            top = pr[1].1;
            per = fix(round_one(sub(div(1.0, pr[1].0), 3e-7)));
        }
        for &(f, p) in &pr[2..] {
            if sgn(p, top) == 1 {
                second = top;
                top = p;
                per = fix(round_one(sub(div(1.0, f), 3e-7)));
            } else if sgn(p, second) == 1 {
                second = p;
            }
        }
        if (2..=maxp).contains(&per) {
            let ratio = if sgn(second, 0.0) != 0 {
                div(top, second)
            } else {
                0.0
            };
            let thr = sub(2.0, div(3.0, len as f64));
            if sgn(ratio, thr) == 1 && sgn(top, sa.mean) == 1 {
                p2 = per;
            }
        } else {
            let thr = add(mul(2.5, sa.sd), sa.mean);
            if sgn(top, thr) == 1 && len as f64 >= per as f64 * 2.5 {
                p2 = per;
            }
        }
    }

    let sb = spectrum(&std);
    let pb = &sb.pairs;
    let half = pb.len();
    let mut cands: Vec<(i64, f64)> = Vec::new();
    let mut local: Vec<(i64, f64)> = Vec::new();
    for k in 0..half {
        let (f, p) = pb[k];
        if sgn(p, sb.mean) != 1 {
            continue;
        }
        let per = fix(round_one(div(1.0, f)));
        if per < 4 || 2 * per >= len as i64 {
            continue;
        }
        cands.push((per, p));
        if k == 0 || k == half - 1 {
            continue;
        }
        let (prev, next) = (pb[k - 1].1, pb[k + 1].1);
        let (s1, s2) = (sgn(prev, 0.0), sgn(next, 0.0));
        if s1 != 0 {
            if s2 != 0 && sgn(div(p, prev), 4.0) == 1 && sgn(div(p, next), 4.0) == 1 {
                local.push((per, p));
            }
        } else if s2 == 0 {
            local.push((per, p));
        }
    }
    crt_qsort(&mut cands, |a, b| sgn(b.1, a.1) > 0);
    cands.truncate(3);
    for c in local {
        if !cands.iter().any(|x| x.0 == c.0) {
            cands.push(c);
        }
    }

    if !cands.is_empty() {
        let corr = correlation(&std);
        let det = Detector { len, corr: &corr };
        let lf = len as f64;

        let intervals: Vec<(i64, i64, i64)> = cands
            .iter()
            .map(|&(p, _)| {
                let q = div(lf, p as f64);
                let mut lo = fix(round_one(sub(div(lf, add(q, 1.0)), 1.0)));
                let mut hi = fix(round_one(add(div(lf, sub(q, 1.0)), 1.0)));
                if lo <= 3 {
                    lo = 3;
                    if hi < 3 {
                        hi = 5;
                    }
                }
                (p, lo, if hi <= len as i64 { hi } else { lo })
            })
            .collect();

        let mut peaks: Vec<Peak> = Vec::new();
        for &(p0, lo0, hi0) in &intervals {
            let (mut _p, mut lo, mut hi) = (p0, lo0, hi0);
            let mid = fix((p0 as f64 / 2.0).floor());
            if mid < hi {
                lo = lo.max(mid);
            }
            {
                let res = match det.check_peak(lo, hi, false) {
                    Some(pk) => Some(pk),
                    None if 2 * hi < len as i64 => {
                        _p *= 2;
                        lo *= 2;
                        hi *= 2;
                        det.check_peak(lo, hi, true)
                    }
                    None => None,
                };
                if let Some(pk) = res {
                    if sgn(pk.corr, 0.45) == 1 {
                        match peaks.iter_mut().find(|x| x.per == pk.per) {
                            None => peaks.push(pk),
                            Some(e) => {
                                if sgn(e.corr, pk.corr) == -1 {
                                    e.corr = pk.corr;
                                }
                            }
                        }
                    }
                }
            }
        }
        for pk in peaks.iter_mut() {
            let f = div(1.0, pk.per as f64);
            let mut best = 2147483647.0;
            for &(fk, p) in pb {
                let dd = sub(f, fk).abs();
                if sgn(dd, best) == -1 {
                    pk.power = p;
                    best = dd;
                }
            }
        }

        if peaks.len() == 1 {
            p1 = peaks[0].per;
        } else if peaks.len() > 1 {
            let mut list = peaks.clone();
            if (2..=5).contains(&p2) {
                let c = det.lagged(p2);
                match list.iter_mut().find(|x| x.per == p2) {
                    None => list.push(Peak {
                        per: p2,
                        power: top,
                        corr: c,
                    }),
                    Some(e) => {
                        if sgn(e.power, top) != 1 {
                            e.power = top;
                        }
                        if sgn(e.corr, c) != 1 {
                            e.corr = c;
                        }
                    }
                }
            }
            crt_qsort(&mut list, |a, b| sgn(b.power, a.power) > 0);
            let mut cut = list.len();
            for i in 1..list.len() {
                if sgn(div(list[i - 1].power, list[i].power), 2.0) == 1 {
                    cut = i - 1;
                    break;
                }
            }
            let c = cut.min(list.len() - 1);
            let mut best = -2147483648.0;
            for j in (0..=c).rev() {
                if sgn(list[j].corr, best) == 1 {
                    best = list[j].corr;
                    p1 = list[j].per;
                }
            }
        }
    }

    let res = if p1 == 1 {
        p2
    } else if p2 == 1 || p1 >= 6 || 2 * p2 > len as i64 {
        p1
    } else {
        let mut amp = [0.0; 2];
        for (slot, &p) in [p1, p2].iter().enumerate() {
            let pf = p as f64;
            let l = p * fix((len as f64 / pf).floor());
            let start = len as i64 - l;
            let (mut sc, mut ss) = (0.0, 0.0);
            for i in start..len as i64 {
                let ang = div(neg(mul(mul((i - start) as f64, PI), 2.0)), pf);
                let y = d[i as usize];
                sc = add(mul(x87::cos(ang), y), sc);
                ss = add(mul(x87::sin(ang), y), ss);
            }
            amp[slot] = div(x87::sqrt(add(mul(ss, ss), mul(sc, sc))), l as f64);
        }
        let mut best = -2147483648.0;
        let mut r = 1;
        for (slot, &p) in [p1, p2].iter().enumerate() {
            if sgn(amp[slot], best) == 1 {
                best = amp[slot];
                r = p;
            }
        }
        r
    };
    if res >= 8785 || res == 1 {
        0
    } else {
        res as usize
    }
}
