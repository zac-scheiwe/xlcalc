#![allow(clippy::needless_range_loop)]

use crate::error::OmError;
use crate::solver_grg::{Ev, Eval, Prob, Status};

const EPSX: f64 = 1e-15;
const BIG: f64 = 1e30;
const TINY: f64 = 1e-30;
const TOL: f64 = 1e-8;
const CONV: f64 = 1e-5;
const CONV_N: u32 = 5;
const HDER: f64 = 1e-6;
const NMAX: usize = 100;

fn f32q(v: f64) -> f64 {
    f64::from(v as f32)
}

pub(crate) fn pos(k: usize, i: usize) -> usize {
    i * (i - 1) / 2 + k
}

struct St<'a, 'b> {
    p: &'a Prob<'a>,
    ev: &'a mut Ev<'b>,
    x: Vec<f64>,
    last: Option<Eval>,

    list: Vec<usize>,
    stat: Vec<i32>,
    nsb: usize,
    g: Vec<f64>,
    gold: Vec<f64>,
    d: Vec<f64>,
    r: Vec<f64>,

    t_prev: f64,
    t_best_f: f64,
    d8: bool,
    cc: bool,
    c0: bool,
    d4: bool,
    d0: bool,
}

impl St<'_, '_> {
    fn f(&mut self, x: &[f64]) -> Result<f64, OmError> {
        let e = (self.ev)(x)?;
        let o = if self.p.max { -e.obj } else { e.obj };
        self.last = Some(e);
        Ok(o)
    }

    fn resetr(&mut self, b0: &mut bool) {
        if self.nsb == 0 {
            return;
        }
        let m = NMAX.min(self.nsb);
        self.r.fill(0.0);
        for i in 1..=m {
            self.r[pos(i, i)] = 1.0;
        }
        *b0 = true;
    }

    fn r1mod(&mut self, p: &mut [f64], n: usize, alpha: f64) {
        r1mod(&mut self.r, p, n, alpha);
    }

    fn delcol(&mut self, k: usize) {
        delcol(&mut self.r, self.nsb, k);
        self.nsb -= 1;
    }
}

pub(crate) fn r1mod(r: &mut [f64], p: &mut [f64], n: usize, alpha: f64) {
    let s = alpha.abs().sqrt();
    let mut sum = 0.0;
    for v in p.iter_mut().take(n + 1).skip(1) {
        sum += *v * *v;
        *v *= s;
    }
    sum *= alpha;
    if EPSX >= sum.abs() {
        return;
    }
    if EPSX < sum {
        let mut dpos = 1usize;
        for i in 1..=n {
            let pi = p[i];
            let rii = r[dpos];
            let rr = (pi * pi + rii * rii).sqrt();
            let next = i + dpos + 1;
            let sn = pi / rr;
            if EPSX < sn.abs() {
                r[dpos] = rr;
                if i < n {
                    let c = rii / rr;
                    let mut idx = i + dpos;
                    let mut k = i + 1;
                    while k <= n {
                        let rik = r[idx];
                        let pk = p[k];
                        r[idx] = sn * pk + c * rik;
                        p[k] = sn * rik - c * pk;
                        idx += k;
                        k += 1;
                    }
                }
            }
            dpos = next;
        }
    } else {
        p[1] /= r[1];
        let mut z2 = p[1] * p[1];
        let mut ebp = 1usize;
        for i in 2..=n {
            let mut s = p[i];
            for k in 1..i {
                ebp += 1;
                s -= p[k] * r[ebp];
            }
            ebp += 1;
            s /= r[ebp];
            p[i] = s;
            z2 += s * s;
        }
        let mut beta = 1.0 - z2;
        if beta > EPSX {
            let mut run = beta.sqrt();
            for r14 in 1..=n {
                let prev = run;
                let i = n - r14 + 1;
                let zi = p[i];
                beta += zi * zi;
                let nr = beta.sqrt();
                let c = zi / nr;
                p[i] = 0.0;
                let edx0 = ebp;
                ebp -= i;
                run = nr;
                if EPSX < c.abs() {
                    let f = prev / run;
                    let mut idx = edx0;
                    let mut k = i;
                    while k <= n {
                        let pk = p[k];
                        let rk = r[idx];
                        p[k] = rk * c + pk * f;
                        r[idx] = pk * c - rk * f;
                        idx += k;
                        k += 1;
                    }
                }
            }
        }
    }
}

pub(crate) fn delcol(r: &mut [f64], n: usize, k: usize) {
    if k < n {
        let mut ebp = k * (k + 1) / 2;
        for j in k + 1..=n {
            let r12 = ebp;
            ebp += j;
            let a = r[ebp - 1];
            let b = r[ebp];
            let mut rr = (b * b + a * a).sqrt();
            if rr == 0.0 {
                rr = EPSX;
            }
            r[ebp - 1] = rr;
            if j != n {
                let c = a / rr;
                let s = b / rr;
                let mut idx = j + ebp;
                let mut m = j + 1;
                while m <= n {
                    let t3 = r[idx - 1];
                    let ri = r[idx];
                    r[idx - 1] = s * ri + c * t3;
                    r[idx] = s * t3 - c * ri;
                    idx += m;
                    m += 1;
                }
            }

            let mut q = r12 - j + 2;
            while q <= r12 {
                r[q] = r[q + j - 1];
                q += 1;
            }
        }
    }
}

impl St<'_, '_> {
    fn redobj(&mut self, t: &mut f64, tmax: f64, x0: &[f64]) -> Result<f64, OmError> {
        self.cc = *t >= tmax;
        if self.cc {
            *t = tmax;
        }
        for k in 1..=self.nsb {
            let v = self.list[k];
            self.x[v] = self.d[k] * *t + x0[k];
        }
        let x = self.x.clone();
        let f = self.f(&x)?;
        if f <= self.t_best_f {
            self.t_prev = *t;
            self.t_best_f = f;
        }
        Ok(f)
    }

    fn search(&mut self, f0: f64) -> Result<(), OmError> {
        let nsb = self.nsb;
        self.c0 = true;
        self.d4 = true;
        let prev_t = self.t_prev;
        self.t_prev = 0.0;
        self.t_best_f = f0;
        let mut maxd = 0.0f64;
        for k in 1..=nsb {
            maxd = maxd.max(self.d[k].abs());
        }
        let mut tmax = BIG;
        let mut x0 = vec![0.0; nsb + 1];
        for k in 1..=nsb {
            let v = self.list[k];
            let dk = self.d[k];
            if dk.abs() >= TINY.min(EPSX) {
                let mut b = if dk < 0.0 { self.p.lo[v] } else { self.p.hi[v] };
                if b.abs() > 1e20 {
                    b = if b >= 0.0 { 1e20 } else { -1e20 };
                }
                let rr = (b - self.x[v]) / dk;
                if rr <= tmax {
                    tmax = rr;
                }
            }
        }
        let mut t0 = if maxd == 0.0 {
            tmax
        } else {
            (TOL / maxd).max(prev_t)
        };
        let mut cap = tmax;
        for k in 1..=nsb {
            let v = self.list[k];
            x0[k] = self.x[v];
            let ax = self.x[v].abs();
            if EPSX > self.d[k].abs() {
                continue;
            }
            let num = if 1.0 > ax { 0.05 } else { ax * 0.05 };
            let q = num / self.d[k].abs();
            if q < cap {
                cap = q;
            }
        }
        if !self.d8 || t0 > cap {
            t0 = cap;
        }
        if t0 > tmax {
            t0 = tmax;
        }
        let mut t = t0;
        let mut edi = 1u32;
        let mut tcur;
        let (mut xa, mut fa) = (0.0, f0);
        let (mut xb, mut fb);
        let (mut xc, mut fc) = (0.0, -BIG);
        let mut f1;

        loop {
            tcur = t;
            f1 = self.redobj(&mut tcur, tmax, &x0)?;
            if EPSX + f0 >= f1 {
                xb = t;
                fb = f1;
                break;
            }
            xc = tcur;
            fc = f1;
            let mut div = 1.0;
            for _ in 0..edi {
                div += div;
            }
            t = tcur / div;
            edi += 1;
            if edi > 7 {
                self.d4 = false;
                return self.finish(&x0);
            }
        }
        if self.cc {
            self.c0 = false;
            return self.finish(&x0);
        }
        let mut parabola = fc > EPSX + fb;
        if !parabola {
            let mut cnt = 1;
            loop {
                let t2 = xb + xb;
                tcur = t2;
                let f = self.redobj(&mut tcur, tmax, &x0)?;
                xc = tcur;
                fc = f;
                if fc > EPSX + fb {
                    parabola = true;
                    break;
                }
                if self.cc {
                    self.c0 = false;
                    return self.finish(&x0);
                }
                cnt += 1;
                xa = xb;
                fa = fb;
                xb = xc;
                fb = fc;
                if cnt > 30 {
                    self.d0 = true;
                    return self.finish(&x0);
                }
            }
        }
        if parabola {
            let tb = xb - xa;
            let fbb = fb - fa;
            let fcc = fc - fa;
            let tc = xc - xa;
            let t2 = fcc * tb;
            let t1 = fbb * tc;
            let den = t1 - t2;
            if den.abs() >= TINY {
                let num = (t1 * tc - t2 * tb) * 0.5;
                let ts = num / den + xa;
                if xa < ts && ts < xc {
                    let mut tt = ts;
                    self.redobj(&mut tt, tmax, &x0)?;
                }
            }
        }
        self.finish(&x0)
    }

    fn finish(&mut self, x0: &[f64]) -> Result<(), OmError> {
        self.d8 = self.c0;
        let t = self.t_prev;
        for k in 1..=self.nsb {
            let v = self.list[k];
            self.x[v] = self.d[k] * t + x0[k];
        }
        Ok(())
    }
}

pub(crate) fn applies(p: &Prob, n: usize) -> bool {
    p.rel.is_empty() && p.val.is_none() && !p.central && n <= NMAX && n > 0
}

pub(crate) fn grg_q(
    p: &Prob,
    x0: Vec<f64>,
    pre: usize,
    ev: &mut Ev,
) -> Result<(Vec<f64>, Eval, Status), OmError> {
    let n = x0.len();
    let mut s = St {
        p,
        ev,
        x: x0.clone(),
        last: None,
        list: vec![0; n + 2],
        stat: vec![0; n + 2],
        nsb: 0,
        g: vec![0.0; n + 2],
        gold: vec![0.0; n + 2],
        d: vec![0.0; n + 2],
        r: vec![0.0; (n + 1) * (n + 2) / 2 + 2],
        t_prev: 1.0,
        t_best_f: 0.0,
        d8: false,
        cc: false,
        c0: false,
        d4: false,
        d0: false,
    };
    let mut xcur = x0;
    let mut fcur = s.f(&xcur)?;
    for _ in 1..pre {
        fcur = s.f(&xcur)?;
    }
    let f_start = fcur;
    let mut xprev = xcur.clone();
    let mut fref = BIG;
    let mut cnt = 0u32;
    let (mut b0, mut b4) = (false, false);
    let mut it = 0usize;
    let mut old_list: Vec<usize> = Vec::new();
    let mut gobj = vec![0.0; n + 1];
    let status: Status;
    let nmax = NMAX.min(n);
    'outer: loop {
        s.x = xcur.clone();

        old_list.clear();
        old_list.extend((1..=s.nsb).map(|k| s.list[k]));
        let mut dist = vec![0.0; n];
        for j in 0..n {
            let a = p.hi[j] - xcur[j];
            let b = xcur[j] - p.lo[j];
            let mut m = if a >= b { b } else { a };
            if TOL >= m {
                m = 0.0;
            }
            dist[j] = m.min(1e10);
        }
        let mut nsb = 0;
        let mut tail = n;
        for j in 0..n {
            if dist[j] > 0.0 {
                nsb += 1;
                s.list[nsb] = j;
            } else {
                s.list[tail] = j;
                tail -= 1;
            }
        }

        s.nsb = nsb;
        for k in 1..=n {
            let v = s.list[k];
            let mut st = 0;
            if TOL >= (xcur[v] - p.hi[v]).abs() {
                st = 1;
            }
            if TOL >= (xcur[v] - p.lo[v]).abs() {
                st = -1;
            }
            s.stat[k] = st;
        }
        let a4 = !(nsb == old_list.len()
            && nsb != 0
            && (1..=nsb).all(|k| old_list.contains(&s.list[k])));
        let fbase = s.f(&xcur)?;
        for j in 0..n {
            if p.lo[j] == p.hi[j] {
                gobj[j] = 0.0;
                continue;
            }
            let mut h = (xcur[j].abs() + 1.0) * HDER;
            if h + xcur[j] > p.hi[j] {
                h = -h;
            }
            let mut xp = xcur.clone();
            xp[j] = xcur[j] + h;
            let f1 = s.f(&xp)?;
            gobj[j] = f32q((f1 - fbase) / h);
        }
        fcur = fbase;
        for k in 1..=n {
            s.g[k] = gobj[s.list[k]];
        }

        let mut xmax = 0.0f64;
        let mut dxmax = 0.0f64;
        for j in 0..n {
            xmax = xmax.max(xcur[j].abs());
            dxmax = dxmax.max((xcur[j] - xprev[j]).abs());
            xprev[j] = xcur[j];
        }
        let sq = TOL.sqrt();
        if (xmax + 1.0) * sq > dxmax {
            let gmax = gobj.iter().take(n).fold(0.0f64, |a, v| a.max(v.abs()));
            let mut conv = true;
            if gmax > TOL {
                let thr2 = sq * gmax;
                for k in 1..=n {
                    let gk = s.g[k];
                    let bad = match s.stat[k] {
                        -1 => -gk > thr2,
                        0 => gk.abs() > thr2,
                        1 => gk > thr2,
                        _ => false,
                    };
                    if bad {
                        conv = false;
                        break;
                    }
                }
            }
            if conv {
                status = Status::Optimal;
                break 'outer;
            }
        }
        {
            let a = (fref - f_start) * CONV;
            let b = fcur - fref;
            if b.abs() > a.abs() {
                fref = fcur;
                cnt = 0;
            } else {
                cnt += 1;
                if cnt >= CONV_N {
                    status = Status::Converged;
                    break 'outer;
                }
            }
        }

        let mut a0 = true;
        let ecx = b4 || s.nsb == 0;
        b4 = ecx;
        let mut goto_price = ecx;
        if !goto_price {
            let mut do_dir = true;
            if it == 0 || a4 || b0 {
                s.resetr(&mut b0);
            } else {
                let t = s.t_prev;
                let ac = t > EPSX;
                if ac {
                    let (mut dy, mut dgo, mut dgn) = (0.0, 0.0, 0.0);
                    let mut y = vec![0.0; s.nsb + 2];
                    for k in 1..=s.nsb {
                        let dk = s.d[k];
                        let gn = s.g[k];
                        let go = s.gold[k];
                        y[k] = gn - go;
                        dy += dk * y[k];
                        dgo += dk * go;
                        dgn += dk * gn;
                    }
                    if dgn > dgo * 0.99 && EPSX < t.abs() && EPSX <= dy.abs() {
                        let den2 = if EPSX > dgo.abs() { EPSX } else { dgo };
                        let nsbv = s.nsb;
                        s.r1mod(&mut y, nsbv, 1.0 / (dy * t));
                        let mut go = s.gold.clone();
                        s.r1mod(&mut go, nsbv, 1.0 / den2);
                    }
                }
                do_dir = true;
            }
            if do_dir {
                let nsb = s.nsb;
                'dirloop: loop {
                    for k in 1..=nsb {
                        s.d[k] = -s.g[k];
                    }
                    s.d[1] /= s.r[1];
                    for i in 2..=nsb {
                        let mut v = s.d[i];
                        for k in 1..i {
                            v -= s.r[pos(k, i)] * s.d[k];
                        }
                        s.d[i] = v / s.r[pos(i, i)];
                    }
                    for j in (1..=nsb).rev() {
                        s.d[j] /= s.r[pos(j, j)];
                        for k in 1..j {
                            let v = s.d[j] * s.r[pos(k, j)];
                            s.d[k] -= v;
                        }
                    }
                    let (mut dmax, mut dmin) = (0.0f64, BIG);
                    for k in 1..=nsb {
                        let a = s.r[pos(k, k)].abs();
                        dmax = dmax.max(a);
                        dmin = dmin.min(a);
                    }
                    if EPSX > dmin {
                        let mut gd = 0.0;
                        for k in 1..=nsb {
                            let st = s.stat[k];
                            if (st == -1 && 0.0 > s.d[k]) || (st == 1 && s.d[k] > 0.0) {
                                continue;
                            }
                            gd += s.g[k] * s.d[k];
                        }
                        if -EPSX <= gd || gd.is_nan() {
                            if b0 {
                                if (1..=nsb).all(|k| s.d[k] == 0.0) {
                                    b4 = true;
                                    goto_price = true;
                                    break 'dirloop;
                                }

                                status = Status::Optimal;
                                break 'outer;
                            }
                            b0 = true;
                            s.resetr(&mut b0);
                            continue 'dirloop;
                        }
                    }
                    break;
                }
            }
        }

        let _ = nmax;
        if !b4 || goto_price {
            let (mut cntg, mut sumsq) = (0usize, 0.0);
            for k in 1..=s.nsb {
                let gk = s.g[k];
                if gk.abs() > TOL {
                    sumsq += gk * gk;
                    cntg += 1;
                }
            }
            let rms = if b4 || cntg == 0 {
                0.0
            } else {
                sumsq.sqrt() / cntg as f64
            };
            let mut pos_i = s.nsb + 1;
            while pos_i <= n {
                let v = s.list[pos_i];
                if p.hi[v] != p.lo[v] {
                    let gneg = -s.g[pos_i];
                    let st = s.stat[pos_i];
                    let skip =
                        (st == 1 && gneg > 0.0) || (st == -1 && 0.0 > gneg) || rms > gneg.abs();
                    if !skip {
                        s.nsb += 1;
                        let nb = s.nsb;
                        s.list.swap(pos_i, nb);
                        s.g.swap(pos_i, nb);
                        s.stat.swap(pos_i, nb);
                        s.d[nb] = -s.g[nb];
                        if nb <= NMAX {
                            let base = nb * (nb - 1) / 2;
                            for kk in 1..=nb {
                                s.r[base + kk] = if kk == nb { 1.0 } else { 0.0 };
                            }
                        }
                        a0 = false;
                    }
                }
                pos_i += 1;
            }
        }
        if b4 {
            if a0 && b0 {
                status = Status::Optimal;
                break 'outer;
            }
            for k in 1..=s.nsb {
                s.d[k] = -s.g[k];
            }
            b0 = true;
            s.resetr(&mut b0);
            b4 = false;
        }
        for k in 1..=n {
            s.gold[k] = s.g[k];
        }

        s.d0 = false;
        s.search(fcur)?;
        xcur = s.x.clone();
        for v in &mut xcur {
            if EPSX > v.abs() {
                *v = 0.0;
            }
        }
        s.x = xcur.clone();
        it += 1;
        if s.cc {
            let mut esi = s.nsb;
            let mut r14 = 1;
            let nsb0 = s.nsb;
            while r14 <= nsb0 {
                let k = nsb0 - r14 + 1;
                let v = s.list[k];
                let at = TOL >= (xcur[v] - p.lo[v]).abs() || TOL >= (xcur[v] - p.hi[v]).abs();
                if at {
                    esi -= 1;
                    s.delcol(k);
                    for q in k..=esi {
                        s.gold[q] = s.gold[q + 1];
                    }
                }
                r14 += 1;
            }
        }
        if !s.d4 {
            if b4 {
                status = Status::Optimal;
                break 'outer;
            }
            b4 = true;
            continue;
        }
        if s.d0 {
            status = Status::Unbounded;
            break 'outer;
        }
        b0 = false;
        b4 = false;
        let _ = a0;
        if it > 500 {
            status = Status::Optimal;
            break 'outer;
        }
    }
    let x = xcur;

    let e = (s.ev)(&x)?;
    Ok((x, e, status))
}
