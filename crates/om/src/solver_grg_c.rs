#![allow(clippy::needless_range_loop)]

use crate::error::OmError;
use crate::solver_grg::{Ev, Eval, Prob, Status};
use crate::solver_grg_q::{delcol, pos, r1mod};

const EPSX: f64 = 1e-15;
const BIG: f64 = 1e30;
const TINY: f64 = 1e-30;
const TOL: f64 = 1e-8;
const CONV: f64 = 1e-5;
const CONV_N: u32 = 5;
const HDER: f64 = 1e-6;
const PIVTOL: f64 = 1e-4;
const DEGEN: f64 = 1e-6;
const ITLIM: usize = 10;
const NMAX: usize = 100;

struct C<'a, 'b> {
    p: &'a Prob<'a>,
    ev: &'a mut Ev<'b>,
    n: usize,
    m: usize,
    x: Vec<f64>,
    cells: Vec<f64>,
    lb: Vec<f64>,
    ub: Vec<f64>,
    typ: Vec<i32>,
    g: Vec<f64>,
    jac: Vec<Vec<f32>>,
    binv: Vec<Vec<f64>>,
    bas: Vec<usize>,
    rows: Vec<usize>,
    nb: usize,
    inact: Vec<usize>,
    ni: usize,
    pos41: Vec<usize>,
    list: Vec<usize>,
    stat: Vec<i32>,
    nsb: usize,
    rg: Vec<f64>,
    gold: Vec<f64>,
    d: Vec<f64>,
    r: Vec<f64>,
    v: Vec<f64>,
    w0: Vec<f64>,
    w8: Vec<f64>,
    wf: Vec<f64>,
    x0: Vec<f64>,
    s38: Vec<f64>,
    s40: Vec<f64>,
    t: f64,
    tmax: f64,
    tbest: f64,
    fbest: f64,
    tc: f64,
    bnd: f64,
    jd: f64,
    tobj: f64,
    c4: bool,
    c8: bool,
    cc: bool,
    c0: bool,
    d4: bool,
    d0: bool,
    d8: bool,
    a4: bool,
    a8: bool,
    d24: usize,
    d90: usize,
    d58: usize,
    d94: usize,
    d50: usize,
}

impl C<'_, '_> {
    fn gcomp(&mut self) -> Result<(), OmError> {
        let e = self.eval_at(None)?;
        self.store(&e);
        Ok(())
    }

    fn eval_at(&mut self, alt: Option<(usize, usize, f64)>) -> Result<Eval, OmError> {
        match alt {
            None => self.cells.copy_from_slice(&self.x[1..=self.n]),
            Some((j, prev, v)) => {
                self.cells[prev - 1] = self.x[prev];
                self.cells[j - 1] = v;
            }
        }
        let xs = self.cells.clone();
        (self.ev)(&xs)
    }

    fn row_values(&self, e: &Eval) -> Vec<f64> {
        let mut g = vec![0.0; self.m + 1];
        let mr = e.lhs.len();
        g[1..=mr].copy_from_slice(&e.lhs);
        if self.p.val.is_some() {
            g[mr + 1] = e.obj;
        } else {
            g[self.m] = if self.p.max { -e.obj } else { e.obj };
        }
        g
    }

    fn store(&mut self, e: &Eval) {
        self.g = self.row_values(e);
    }

    fn coef(&self, row: usize, var: usize) -> f64 {
        if var <= self.n {
            f64::from(self.jac[row][var])
        } else if var - self.n == row {
            -1.0
        } else {
            0.0
        }
    }

    fn ph1obj(&mut self, init: bool, b0: &mut bool) {
        let (n, m) = (self.n, self.m);
        self.d24 = 0;
        let mut sum = 0.0;
        for k in 1..=self.ni {
            let row = self.inact[k];
            let s = n + row;
            let gv = self.g[row];
            let lo = self.lb[s] - gv;
            if lo > TOL {
                sum += lo;
                self.d24 += 1;
            } else {
                let hi = gv - self.ub[s];
                if hi > TOL {
                    sum += hi;
                    self.d24 += 1;
                }
            }
        }
        let gobj = self.g[m];
        self.tobj = gobj;
        if self.d24 == 0 {
            return;
        }
        if init {
            *b0 = true;
        }

        self.g[m] = gobj * 0.0 + sum;
    }

    fn parsh(&mut self) -> Result<(), OmError> {
        let (n, m) = (self.n, self.m);
        let e = self.eval_at(None)?;
        let gb = self.row_values(&e);
        let mut prev = 1;
        for j in 1..=n {
            if self.lb[j] == self.ub[j] {
                for i in 1..=m {
                    self.jac[i][j] = 0.0;
                }
                continue;
            }
            let xj = self.x[j];
            let mut h = (xj.abs() + 1.0) * HDER;
            if h + xj > self.ub[j] {
                h = -h;
            }
            let e = self.eval_at(Some((j, prev, h + xj)))?;
            prev = j;
            let gp = self.row_values(&e);
            for i in 1..=m {
                self.jac[i][j] = ((gp[i] - gb[i]) / h) as f32;
            }
        }
        Ok(())
    }

    fn parshc(&mut self) -> Result<(), OmError> {
        let (n, m) = (self.n, self.m);
        let mut prev = 1;
        for j in 1..=n {
            if self.lb[j] == self.ub[j] {
                for i in 1..=m {
                    self.jac[i][j] = 0.0;
                }
                continue;
            }
            let xj = self.x[j];
            let h = (xj.abs() + 1.0) * HDER;
            let up = if h + xj > self.ub[j] {
                self.ub[j] - xj
            } else {
                h
            };
            let hp = if 0.0 > up { 0.0 } else { up };
            let dn = if self.lb[j] > -h + xj {
                self.lb[j] - xj
            } else {
                -h
            };
            let hm = if 0.0 < dn { 0.0 } else { dn };
            let e = self.eval_at(Some((j, prev, hp + xj)))?;
            let gp = self.row_values(&e);
            let e = self.eval_at(Some((j, j, hm + xj)))?;
            prev = j;
            let gm = self.row_values(&e);
            let d = hp - hm;
            for i in 1..=m {
                self.jac[i][j] = if d == 0.0 {
                    0.0
                } else {
                    ((gp[i] - gm[i]) / d) as f32
                };
            }
        }
        Ok(())
    }

    fn consbs(&mut self, rebuild_extra: bool) -> Result<(), OmError> {
        let (n, m) = (self.n, self.m);
        let old_rows: Vec<usize> = self.rows[1..=self.nb].to_vec();
        let nb_old = self.nb;
        self.a4 = false;
        self.nb = 0;
        self.ni = 0;
        for i in 1..=m {
            if self.typ[i] == 0 {
                continue;
            }
            let s = n + i;
            let gi = self.g[i];
            let mut val = None;
            if self.typ[i] == 2 && (gi - self.ub[s]).abs() < TOL {
                val = Some(self.ub[s]);
            }
            if val.is_none() && (gi - self.lb[s]).abs() < TOL {
                val = Some(self.lb[s]);
            }
            match val {
                Some(b) => {
                    self.nb += 1;
                    self.rows[self.nb] = i;
                    self.x[s] = b;
                }
                None => {
                    self.ni += 1;
                    self.inact[self.ni] = i;
                }
            }
        }
        let nb = self.nb;
        let mut rebuild = rebuild_extra || nb != nb_old || self.a8 || !self.c0;
        if !rebuild && nb != 0 {
            rebuild = (1..=nb).any(|k| self.rows[k] != old_rows[k - 1]);
        }
        self.fbest = self.g[m];
        if self.p.central {
            self.parshc()?;
        } else {
            self.parsh()?;
        }

        let mut elig = vec![0usize; n + m + 2];
        let mut dist = vec![0.0f64; n + m + 2];
        let mut piv = vec![0.0f64; n + m + 2];
        let mut prow = vec![0usize; n + m + 2];
        for k in 1..=n {
            elig[k] = if self.lb[k] == self.ub[k] { 0 } else { k };
            let a = self.ub[k] - self.x[k];
            let b = self.x[k] - self.lb[k];
            let mut dd = if a >= b { b } else { a };
            if TOL >= dd {
                dd = 0.0;
            }
            dist[k] = if 1e10 < dd { 1e10 } else { dd };
        }
        if nb > 0 {
            let cols = self.binv[1].len() - 1;
            for i in 1..=nb {
                self.bas[i] = 0;
                dist[n + i] = 0.0;
                piv[n + i] = 1.0;
                prow[n + i] = i;
                elig[n + i] = n + i;
                for c in 1..=cols {
                    self.binv[i][c] = if c <= n {
                        f64::from(self.jac[self.rows[i]][c])
                    } else {
                        0.0
                    };
                }
            }
            let mut done = 0usize;
            let mut fallback = false;
            let mut first = 0.0f64;
            let mut keep_row = 0usize;
            while done < nb {
                let mut esi = 0usize;
                let mut chosen = false;
                if !rebuild && !fallback {
                    let mut best = 0.0f64;
                    for r9 in 1..=nb {
                        let v = self.pos41[r9];
                        if v == 0 || v > n {
                            continue;
                        }
                        for row in 1..=nb {
                            if self.bas[row] != 0 {
                                continue;
                            }
                            let a = self.binv[row][v].abs();
                            if a > best {
                                best = a;
                                prow[v] = row;
                                esi = v;
                                keep_row = r9;
                            }
                        }
                    }
                    let mut accept = true;
                    if done == 0 {
                        first = best;
                        if PIVTOL >= best {
                            accept = false;
                        }
                    }
                    if accept
                        && !(done > 0 && self.d58 != 0 && best > 1e-15)
                        && PIVTOL * first > best
                        && PIVTOL > best
                    {
                        accept = false;
                    }
                    if accept {
                        self.pos41[keep_row] = 0;
                        chosen = true;
                    } else {
                        rebuild = true;
                    }
                }
                if !chosen && rebuild && !fallback {
                    let mut x3 = 0.0f64;
                    for c in 1..=n {
                        piv[c] = 0.0;
                        if elig[c] != c {
                            continue;
                        }
                        let (mut mx, mut row) = (0.0f64, 0usize);
                        for r in 1..=nb {
                            if self.bas[r] != 0 {
                                continue;
                            }
                            let a = self.binv[r][c].abs();
                            if a >= mx {
                                mx = a;
                                row = r;
                            }
                        }
                        piv[c] = mx;
                        prow[c] = row;
                        x3 = x3.max(mx);
                    }
                    if done == 0 {
                        first = x3;
                    }
                    if 1e-10 > first || (PIVTOL * first > x3 && PIVTOL > x3) {
                        fallback = true;
                    } else {
                        let mut best = 0.0f64;
                        for c in 1..=n {
                            if elig[c] == c && TOL * 10.0 <= dist[c] && piv[c] > best {
                                best = piv[c];
                                esi = c;
                            }
                        }
                        if PIVTOL > best && PIVTOL * x3 > best {
                            esi = 0;
                        }
                        if esi == 0 {
                            let mut md = 0.0f64;
                            for c in 1..=n {
                                if elig[c] == c && piv[c] > x3 * 0.01 && dist[c] >= md {
                                    md = dist[c];
                                    esi = c;
                                }
                            }
                        }
                        chosen = true;
                    }
                }
                if !chosen {
                    fallback = true;
                    let mut row = 1;
                    while row <= nb && self.bas[row] != 0 {
                        row += 1;
                    }
                    esi = n + row;
                }
                let row = prow[esi];
                if esi > n {
                    for c in 1..=n {
                        self.binv[row][c] = -self.binv[row][c];
                    }
                    self.bas[row] = esi;
                } else {
                    let pv = self.binv[row][esi];
                    let mut col = vec![0.0f64; nb + 1];
                    for r in 1..=nb {
                        col[r] = self.binv[r][esi];
                        self.binv[r][esi] = 0.0;
                    }
                    self.binv[row][esi] = 1.0;
                    for c in 1..=n {
                        self.binv[row][c] /= pv;
                    }
                    for r in 1..=nb {
                        if r == row {
                            continue;
                        }
                        for c in 1..=n {
                            let s = col[r] * self.binv[row][c];
                            self.binv[r][c] -= s;
                        }
                    }
                    dist[esi] = 0.0;
                    let e = elig[esi];
                    self.bas[row] = e;
                    elig[esi] = elig[n + row];
                    elig[n + row] = e;
                }
                done += 1;
            }

            for r8 in 1..=nb {
                if let Some(c) = (1..=n).find(|&c| elig[c] == n + r8) {
                    for r in 1..=nb {
                        self.binv[r].swap(r8, c);
                    }
                    elig[c] = elig[r8];
                    elig[r8] = 0;
                }
            }
            for r8 in 1..=nb {
                for r9 in 1..=nb {
                    if elig[n + r9] == n + r8 {
                        for r in 1..=nb {
                            self.binv[r][r8] = 0.0;
                        }
                        self.binv[r9][r8] = if r9 == r8 { -1.0 } else { 1.0 };
                    }
                }
            }
        }

        let old_sb: Vec<usize> = self.list[1..=self.nsb].to_vec();
        let nsb_old = self.nsb;
        let mut mk = vec![false; n + nb + 2];
        for i in 1..=nb {
            mk[self.bas[i]] = true;
        }
        for i in 1..=nb {
            self.pos41[i] = self.bas[i];
            if self.bas[i] > n {
                self.bas[i] = n + self.rows[self.bas[i] - n];
            }
        }
        let mut nsb = 0;
        let mut r10 = n + nb;
        let mut r9 = n;
        for r8 in 1..=nb {
            if !mk[n + r8] {
                self.pos41[r10] = n + r8;
                r10 -= 1;
                self.list[r9] = n + self.rows[r8];
                r9 -= 1;
            }
        }
        for c in 1..=n {
            if mk[c] {
                continue;
            }
            if dist[c] > 0.0 {
                nsb += 1;
                self.pos41[nb + nsb] = c;
                self.list[nsb] = c;
            } else {
                self.list[r9] = c;
                r9 -= 1;
                self.pos41[r10] = c;
                r10 -= 1;
            }
        }
        self.nsb = nsb;
        for k in 1..=n {
            let v = self.list[k];
            let mut st = 0;
            if TOL >= (self.x[v] - self.ub[v]).abs() {
                st = 1;
            }
            if TOL >= (self.x[v] - self.lb[v]).abs() {
                st = -1;
            }
            self.stat[k] = st;
        }
        self.d58 = 0;
        self.a4 =
            !(nsb == nsb_old && nsb != 0 && (1..=nsb).all(|k| old_sb.contains(&self.list[k])));
        Ok(())
    }

    fn redobj(&mut self) -> Result<(), OmError> {
        let (n, m, nb) = (self.n, self.m, self.nb);
        self.c4 = false;
        self.bnd = 0.0;
        self.d58 = 0;
        self.cc = self.t >= self.tmax;
        if self.cc {
            self.t = self.tmax;
        }
        for k in 1..=self.nsb {
            let v = self.list[k];
            self.x[v] = self.d[k] * self.t + self.x0[k];
        }
        let mut r14 = 0usize;
        let mut newton = nb != 0;
        if newton {
            for i in 1..=nb {
                let b = self.bas[i];
                self.x[b] = (self.t - self.tbest) * self.v[i] + self.s38[i];
            }
        } else {
            self.gcomp()?;
        }
        loop {
            if newton {
                let e = if self.d58 != 0 { self.bas[self.d58] } else { 0 };
                let mut prevr = BIG;
                let mut ok = false;
                let mut rk = vec![0.0f64; nb + 1];
                for it in 1..=ITLIM {
                    self.d94 = it - 1;
                    if self.d58 != 0 {
                        for k in 1..=self.nsb {
                            let v = self.list[k];
                            self.x[v] = self.d[k] * self.tc + self.x0[k];
                        }
                    }
                    self.gcomp()?;
                    let mut r0 = 0.0;
                    let mut rmax = 0.0f64;
                    if self.d58 != 0 {
                        if self.d58 > nb {
                            self.x[e] = self.g[e - n];
                        }
                        r0 = self.bnd - self.x[e];
                        rmax = r0.abs();
                    }
                    for k in 1..=nb {
                        let row = self.rows[k];
                        rk[k] = self.g[row] - self.x[n + row];
                        let a = rk[k].abs();
                        rmax = if a > rmax { a } else { rmax };
                    }
                    if TOL > rmax {
                        ok = true;
                        break;
                    }
                    if rmax > prevr {
                        break;
                    }
                    if self.d58 != 0 {
                        let mut den = -self.jd;
                        let mut num = r0;
                        for k in 1..=nb {
                            den += self.w8[k] * self.wf[k];
                            num += self.w8[k] * rk[k];
                        }
                        if EPSX > den.abs() {
                            break;
                        }
                        let dt = num / den;
                        self.tc -= dt;
                        for k in 1..=nb {
                            rk[k] -= dt * self.wf[k];
                        }
                    }
                    for i in 1..=nb {
                        let mut s = 0.0;
                        for k in 1..=nb {
                            s += self.binv[i][k] * rk[k];
                        }
                        let b = self.bas[i];
                        self.x[b] -= s;
                    }
                    prevr = rmax;
                }
                if !ok {
                    self.c4 = true;
                    return Ok(());
                }
            }

            for k in 1..=self.ni {
                let row = self.inact[k];
                self.x[n + row] = self.g[row];
                self.bas[nb + k] = n + row;
            }
            let tot = nb + self.ni;
            let mut near = true;
            if tot != 0 {
                if self.d58 != 0 {
                    if self.tc >= self.t || self.tbest >= self.tc {
                        self.c4 = true;
                        return Ok(());
                    }
                    self.t = self.tc;
                }
                self.d58 = 0;
                let mut best = BIG;
                if self.d24 == 0 {
                    for i in 1..=tot {
                        let v = self.bas[i];
                        let cur = self.x[v];
                        let diff = cur - self.s38[i];
                        if EPSX >= diff.abs() {
                            continue;
                        }
                        let lb = self.lb[v];
                        let r = if TOL < lb - cur {
                            (lb - self.s38[i]) / diff
                        } else {
                            let ub = self.ub[v];
                            if TOL >= cur - ub {
                                continue;
                            }
                            (ub - self.s38[i]) / diff
                        };
                        if r < best {
                            best = r;
                            self.d58 = i;
                        }
                    }
                } else {
                    for i in 1..=tot {
                        let v = self.bas[i];
                        let prev = self.s38[i];
                        let cur = self.x[v];
                        let diff = cur - prev;
                        if EPSX >= diff.abs() {
                            continue;
                        }
                        let lbt = self.lb[v] - TOL;
                        let r = if prev >= lbt && lbt > cur {
                            (lbt - prev) / diff
                        } else {
                            let ubt = TOL + self.ub[v];
                            if ubt < prev || cur <= ubt {
                                continue;
                            }
                            (ubt - prev) / diff
                        };
                        if r < best {
                            best = r;
                            self.d58 = i;
                        }
                    }
                    let mut dummy = false;
                    self.ph1obj(false, &mut dummy);
                    if self.d58 == 0 && self.d24 == 0 {
                        self.tbest = self.t;
                        self.c8 = true;
                        self.g[m] = self.tobj;
                        return Ok(());
                    }
                }
                if self.d58 != 0 {
                    if 0.0 > best {
                        self.c4 = true;
                        return Ok(());
                    }

                    let d58 = self.d58;
                    let v = self.bas[d58];
                    let xv = self.x[v];
                    self.bnd = if (xv - self.lb[v]).abs() > (xv - self.ub[v]).abs() {
                        self.ub[v]
                    } else {
                        self.lb[v]
                    };
                    self.tc = (self.t - self.tbest) * best + self.tbest;
                    for k in 1..=nb {
                        let row = self.rows[k];
                        let mut s = 0.0;
                        for pp in 1..=self.nsb {
                            s += self.coef(row, self.list[pp]) * self.d[pp];
                        }
                        self.wf[k] = s;
                    }
                    if d58 <= nb {
                        for k in 1..=nb {
                            self.w0[k] = 0.0;
                        }
                        self.w0[d58] = 1.0;
                        self.jd = 0.0;
                    } else {
                        let row = v - n;
                        for k in 1..=nb {
                            let b = self.bas[k];
                            self.w0[k] = if b > n {
                                0.0
                            } else {
                                f64::from(self.jac[row][b])
                            };
                        }
                        let mut s = 0.0;
                        for pp in 1..=self.nsb {
                            let lv = self.list[pp];
                            let c = if lv > n {
                                0.0
                            } else {
                                f64::from(self.jac[row][lv])
                            };
                            s += c * self.d[pp];
                        }
                        self.jd = s;
                    }
                    if nb != 0 {
                        for c in 1..=nb {
                            let mut s = 0.0;
                            for r in 1..=nb {
                                s += self.binv[r][c] * self.w0[r];
                            }
                            self.w8[c] = s;
                        }
                        for i in 1..=nb {
                            let b = self.bas[i];
                            self.x[b] = (self.x[b] - self.s38[i]) * best + self.s38[i];
                        }
                    }
                    r14 = d58;
                    newton = true;
                    continue;
                }
            } else {
                near = false;
            }
            if near {
                for k in 1..=self.ni {
                    let row = self.inact[k];
                    let gr = self.g[row];
                    let s = n + row;
                    if TOL > (gr - self.lb[s]).abs() || TOL > (gr - self.ub[s]).abs() {
                        r14 = nb + k;
                    }
                }
            }
            break;
        }

        if self.g[m] <= self.fbest {
            for i in 1..=nb {
                self.s38[i] = self.x[self.bas[i]];
            }
            for k in 1..=self.ni {
                self.s38[nb + k] = self.g[self.inact[k]];
            }
            self.s40[1..=m].copy_from_slice(&self.g[1..=m]);
            self.tbest = self.t;
            self.d90 = self.d24;
            self.fbest = self.g[m];
        }
        self.d58 = r14;
        Ok(())
    }

    fn search(&mut self) -> Result<(), OmError> {
        let (n, m, nb) = (self.n, self.m, self.nb);
        let _ = n;
        let nsb = self.nsb;
        self.d94 = 0;
        self.c0 = true;
        self.c4 = false;
        self.d4 = true;
        self.c8 = false;
        self.cc = false;
        self.t = 0.0;
        let r15 = ITLIM / 2;
        let tprev = self.tbest;
        let f0 = self.g[m];
        self.tbest = 0.0;
        self.fbest = f0;
        for i in 1..=nb {
            self.s38[i] = self.x[self.bas[i]];
        }
        self.s40[1..=m].copy_from_slice(&self.g[1..=m]);
        for k in 1..=self.ni {
            self.s38[nb + k] = self.g[self.inact[k]];
        }
        self.d90 = self.d24;
        let mut maxd = 0.0f64;
        for k in 1..=nsb {
            let a = self.d[k].abs();
            maxd = if a > maxd { a } else { maxd };
        }
        self.tmax = BIG;
        for k in 1..=nsb {
            let dk = self.d[k];
            if EPSX > dk.abs() {
                continue;
            }
            let v = self.list[k];
            let mut b = if 0.0 > dk { self.lb[v] } else { self.ub[v] };
            if b.abs() > 1e20 {
                b = if b >= 0.0 { 1e20 } else { -1e20 };
            }
            let r = (b - self.x[v]) / dk;
            if r <= self.tmax {
                self.tmax = r;
            }
        }
        let mut t0 = if maxd == 0.0 {
            self.tmax
        } else {
            let a = TOL / maxd;
            if a > tprev {
                a
            } else {
                tprev
            }
        };
        let mut cap = self.tmax;
        for k in 1..=nsb {
            let v = self.list[k];
            self.x0[k] = self.x[v];
            let ax = self.x[v].abs();
            let ad = self.d[k].abs();
            if EPSX > ad {
                continue;
            }
            let q = if 1.0 > ax { 0.05 } else { ax * 0.05 } / ad;
            cap = if q < cap { q } else { cap };
        }
        if !self.d8 || t0 > cap {
            t0 = cap;
        }
        if t0 > self.tmax {
            t0 = self.tmax;
        }
        let mut t = t0;
        let mut edi = 1u32;
        let (mut xa, mut fa) = (0.0f64, f0);
        let (mut xb, mut fb);
        let (mut xc, mut fc) = (0.0f64, -BIG);
        loop {
            let prev_c4 = self.c4;
            self.t = t;
            self.redobj()?;
            if self.c4 {
            } else if self.c8 {
                return Ok(());
            } else {
                let f1 = self.g[m];
                if f0 + EPSX >= f1 {
                    xb = t;
                    fb = f1;
                    if self.d58 != 0 || self.cc {
                        self.c0 = false;
                        return self.finish();
                    }
                    if prev_c4 {
                        return self.finish();
                    }
                    break;
                }
                xc = self.t;
                fc = f1;
            }
            let mut div = 1.0;
            for _ in 0..edi {
                div += div;
            }
            t = self.t / div;
            edi += 1;
            if edi > 7 {
                self.d4 = false;
                return self.finish();
            }
        }
        let mut parabola = fc > EPSX + fb;
        if !parabola {
            let mut cnt = 1;
            loop {
                if self.d94 >= r15 {
                    return self.finish();
                }
                self.t = xb + xb;
                self.redobj()?;
                if self.c4 {
                    return self.finish();
                }
                if self.c8 {
                    return Ok(());
                }
                xc = self.t;
                fc = self.g[m];
                if fc > EPSX + fb || (self.d24 > 0 && fc >= fb - EPSX) {
                    parabola = true;
                    break;
                }
                if self.d58 != 0 || self.cc {
                    self.c0 = false;
                    return self.finish();
                }
                cnt += 1;
                xa = xb;
                fa = fb;
                xb = xc;
                fb = fc;
                if cnt > 30 {
                    self.d0 = true;
                    return self.finish();
                }
            }
        }
        if parabola {
            let tb = xb - xa;
            let tc = xc - xa;
            let t1 = (fc - fa) * tb;
            let t2 = (fb - fa) * tc;
            let den = t2 - t1;
            if den.abs() >= TINY {
                let s = (t2 * tc - t1 * tb) * 0.5 / den + xa;
                if xa < s && s < xc {
                    self.t = s;
                    self.redobj()?;
                    if self.c4 {
                        return self.finish();
                    }
                    if self.c8 {
                        return Ok(());
                    }
                    if self.d58 != 0 && fb > self.g[m] {
                        self.c0 = false;
                    }
                }
            }
        }
        self.finish()
    }

    fn finish(&mut self) -> Result<(), OmError> {
        let (n, m) = (self.n, self.m);
        self.d8 = self.c0;
        self.t = self.tbest;
        self.d24 = self.d90;
        for k in 1..=self.nsb {
            let v = self.list[k];
            self.x[v] = self.d[k] * self.tbest + self.x0[k];
        }
        for i in 1..=self.nb {
            let b = self.bas[i];
            self.x[b] = self.s38[i];
        }
        for i in 1..=m {
            self.g[i] = self.s40[i];
            self.x[n + i] = self.s40[i];
        }
        Ok(())
    }

    fn resetr(&mut self, b0: &mut bool) {
        if self.nsb == 0 {
            return;
        }
        let k = NMAX.min(self.nsb);
        self.r.fill(0.0);
        for i in 1..=k {
            self.r[pos(i, i)] = 1.0;
        }
        *b0 = true;
    }
}

pub(crate) fn applies(p: &Prob, n: usize) -> bool {
    (!p.rel.is_empty() || p.val.is_some()) && n <= NMAX && n > 0
}

pub(crate) fn grg_c(
    p: &Prob,
    x0: Vec<f64>,
    pre: usize,
    ev: &mut Ev,
) -> Result<(Vec<f64>, Eval, Status), OmError> {
    let n = x0.len();
    let mr = p.rel.len();
    let m = mr + 1 + usize::from(p.val.is_some());
    let nv = n + m;
    let clamp = |v: f64| v.clamp(-BIG, BIG);
    let mut lb = vec![0.0; nv + 1];
    let mut ub = vec![0.0; nv + 1];
    for j in 1..=n {
        lb[j] = clamp(p.lo[j - 1]);
        ub[j] = clamp(p.hi[j - 1]);
    }
    let mut x = vec![0.0; nv + 1];
    x[1..=n].copy_from_slice(&x0);
    let cols = n.max(m) + 1;
    let mut c = C {
        p,
        ev,
        n,
        m,
        x,
        cells: x0.clone(),
        lb,
        ub,
        typ: vec![0; m + 1],
        g: vec![0.0; m + 1],
        jac: vec![vec![0.0f32; n + 1]; m + 1],
        binv: vec![vec![0.0; cols]; m + 1],
        bas: vec![0; nv + 2],
        rows: vec![0; m + 1],
        nb: 0,
        inact: vec![0; m + 1],
        ni: 0,
        pos41: vec![0; nv + 2],
        list: vec![0; nv + 2],
        stat: vec![0; nv + 2],
        nsb: 0,
        rg: vec![0.0; nv + 2],
        gold: vec![0.0; nv + 2],
        d: vec![0.0; nv + 2],
        r: vec![0.0; (n + 1) * (n + 2) / 2 + 2],
        v: vec![0.0; nv + 2],
        w0: vec![0.0; nv + 2],
        w8: vec![0.0; nv + 2],
        wf: vec![0.0; nv + 2],
        x0: vec![0.0; nv + 2],
        s38: vec![0.0; nv + 2],
        s40: vec![0.0; m + 1],
        t: 0.0,
        tmax: BIG,
        tbest: 1.0,
        fbest: 0.0,
        tc: 0.0,
        bnd: 0.0,
        jd: 0.0,
        tobj: 0.0,
        c4: false,
        c8: false,
        cc: false,
        c0: false,
        d4: false,
        d0: false,
        d8: false,
        a4: false,
        a8: true,
        d24: 0,
        d90: 0,
        d58: 0,
        d94: 0,
        d50: 0,
    };

    let e0 = (c.ev)(&x0)?;
    for i in 1..=mr {
        let s = n + i;
        let b = e0.rhs[i - 1];
        let (lo, hi, ty) = match p.rel[i - 1] {
            1 => (-BIG, b, 2),
            2 => (b, b, 1),
            _ => (b, BIG, 2),
        };
        c.lb[s] = lo;
        c.ub[s] = hi;
        c.typ[i] = ty;
    }
    if let Some(v) = p.val {
        c.lb[n + m - 1] = v;
        c.ub[n + m - 1] = v;
        c.typ[m - 1] = 1;
    }
    c.lb[n + m] = -BIG;
    c.ub[n + m] = BIG;
    c.store(&e0);
    for _ in 1..pre {
        c.gcomp()?;
    }
    let gstart = c.g[m];
    let mut xprev: Vec<f64> = c.x.clone();
    for i in 1..=m {
        c.x[n + i] = c.g[i];
    }
    let mut fref = BIG;
    let mut cnt = 0u32;
    let (mut b0, mut b4) = (false, false);
    let mut e4 = false;
    let mut first = true;
    let mut r13;
    let mut code = 0u8;
    let mut guard = 0usize;
    'main: loop {
        guard += 1;
        if guard > 500 {
            break 'main;
        }

        c.consbs(c.c8 || first)?;
        if first {
            c.ph1obj(true, &mut b0);
            for i in 1..=c.nb {
                c.s38[i] = c.x[c.bas[i]];
            }
            for k in 1..=c.ni {
                c.s38[c.nb + k] = c.g[c.inact[k]];
            }
            c.s40[1..=m].copy_from_slice(&c.g[1..=m]);
            first = false;
        }
        r13 = 0;

        'reduced: loop {
            if c.d24 != 0 {
                for j in 1..=n {
                    let mut s = 0.0;
                    for k in 1..=c.ni {
                        let row = c.inact[k];
                        let gv = c.g[row];
                        let sl = n + row;
                        if c.lb[sl] - TOL > gv {
                            s -= f64::from(c.jac[row][j]);
                        } else if gv > TOL + c.ub[sl] {
                            s += f64::from(c.jac[row][j]);
                        }
                    }
                    c.jac[m][j] = (f64::from(c.jac[m][j]) * 0.0 + s) as f32;
                }
            }
            let nb = c.nb;
            let mut u = vec![0.0f64; nb + 1];
            for k in 1..=nb {
                let mut s = 0.0;
                for i in 1..=nb {
                    let b = c.bas[i];
                    if b <= n {
                        s += f64::from(c.jac[m][b]) * c.binv[i][k];
                    }
                }
                u[k] = s;
            }
            for pp in 1..=n {
                let v = c.list[pp];
                c.rg[pp] = if v <= n {
                    let mut s = 0.0;
                    for k in 1..=nb {
                        s += f64::from(c.jac[c.rows[k]][v]) * u[k];
                    }
                    f64::from(c.jac[m][v]) - s
                } else {
                    match (1..=nb).find(|&k| c.rows[k] == v - n) {
                        Some(k) => u[k],
                        None => break 'main,
                    }
                };
            }

            if c.d24 == 0 {
                let mut xmax = 0.0f64;
                let mut dxmax = 0.0f64;
                for j in 1..=n {
                    let xj = c.x[j];
                    xmax = xmax.max(xj.abs());
                    dxmax = dxmax.max((xj - xprev[j]).abs());
                    xprev[j] = xj;
                }
                let sq = TOL.sqrt();
                if (xmax + 1.0) * sq > dxmax {
                    let mut gmax = 0.0f64;
                    for j in 1..=n {
                        gmax = gmax.max(f64::from(c.jac[m][j]).abs());
                    }
                    let mut conv = true;
                    if gmax > TOL {
                        let thr = sq * gmax;
                        for pp in 1..=n {
                            let v = c.list[pp];
                            if v > n && c.typ[v - n] == 1 {
                                continue;
                            }
                            let gk = c.rg[pp];
                            let bad = match c.stat[pp] {
                                -1 => -gk > thr,
                                0 => gk.abs() > thr,
                                1 => gk > thr,
                                _ => false,
                            };
                            if bad {
                                conv = false;
                                break;
                            }
                        }
                    }
                    if conv {
                        code = 0;
                        break 'main;
                    }
                }
            }

            if !e4 {
                let a = (fref - gstart) * CONV;
                let b = c.g[m] - fref;
                if b.abs() > a.abs() {
                    c.a8 = false;
                    fref = c.g[m];
                    cnt = 0;
                } else {
                    cnt += 1;
                    if cnt >= CONV_N {
                        code = 1;
                        break 'main;
                    }
                }
            }

            let mut a0 = true;
            b4 = b4 || c.nsb == 0;
            if c.c8 {
                b0 = true;
            }
            let mut goto_price = b4;
            if !goto_price {
                if c.d50 == 0 || c.a4 || b0 {
                    c.resetr(&mut b0);
                } else {
                    let t = c.t;
                    if t > EPSX {
                        let (mut dy, mut dgo, mut dgn) = (0.0, 0.0, 0.0);
                        let mut y = vec![0.0; c.nsb + 2];
                        for k in 1..=c.nsb {
                            let dk = c.d[k];
                            y[k] = c.rg[k] - c.gold[k];
                            dy += dk * y[k];
                            dgo += dk * c.gold[k];
                            dgn += dk * c.rg[k];
                        }
                        if dgn > dgo * 0.99 && EPSX < t.abs() && EPSX <= dy.abs() {
                            let den2 = if EPSX > dgo.abs() { EPSX } else { dgo };
                            let nsbv = c.nsb;
                            r1mod(&mut c.r, &mut y, nsbv, 1.0 / (dy * t));
                            let mut go = c.gold.clone();
                            r1mod(&mut c.r, &mut go, nsbv, 1.0 / den2);
                        }
                    }
                }
                let nsb = c.nsb;
                'dirloop: loop {
                    for k in 1..=nsb {
                        c.d[k] = -c.rg[k];
                    }
                    c.d[1] /= c.r[1];
                    for i in 2..=nsb {
                        let mut s = c.d[i];
                        for k in 1..i {
                            s -= c.r[pos(k, i)] * c.d[k];
                        }
                        c.d[i] = s / c.r[pos(i, i)];
                    }
                    for j in (1..=nsb).rev() {
                        c.d[j] /= c.r[pos(j, j)];
                        for k in 1..j {
                            let s = c.d[j] * c.r[pos(k, j)];
                            c.d[k] -= s;
                        }
                    }
                    let mut dmin = BIG;
                    for k in 1..=nsb {
                        dmin = dmin.min(c.r[pos(k, k)].abs());
                    }
                    if EPSX > dmin {
                        let mut gd = 0.0;
                        for k in 1..=nsb {
                            let st = c.stat[k];
                            if (st == -1 && 0.0 > c.d[k]) || (st == 1 && c.d[k] > 0.0) {
                                continue;
                            }
                            gd += c.rg[k] * c.d[k];
                        }
                        if -EPSX <= gd || gd.is_nan() {
                            if b0 {
                                if (1..=nsb).all(|k| c.d[k] == 0.0) {
                                    b4 = true;
                                    goto_price = true;
                                    break 'dirloop;
                                }
                                code = 0;
                                break 'main;
                            }
                            b0 = true;
                            c.resetr(&mut b0);
                            continue 'dirloop;
                        }
                    }
                    break;
                }
            }
            let _ = goto_price;

            {
                let (mut cntg, mut sumsq) = (0usize, 0.0);
                for k in 1..=c.nsb {
                    let gk = c.rg[k];
                    if gk.abs() > TOL {
                        sumsq += gk * gk;
                        cntg += 1;
                    }
                }
                let rms = if b4 || cntg == 0 {
                    0.0
                } else {
                    f64::sqrt(sumsq) / cntg as f64
                };
                let mut pi = c.nsb + 1;
                while pi <= n {
                    let v = c.list[pi];
                    if c.ub[v] != c.lb[v] {
                        let gneg = -c.rg[pi];
                        let st = c.stat[pi];
                        let skip = (st == 1 && gneg > 0.0)
                            || (st == -1 && 0.0 > gneg)
                            || rms > gneg.abs()
                            || (v > n && rms * 100.0 > gneg.abs());
                        if !skip {
                            c.nsb += 1;
                            let nb2 = c.nsb;
                            c.list.swap(pi, nb2);
                            c.rg.swap(pi, nb2);
                            c.stat.swap(pi, nb2);
                            c.pos41.swap(c.nb + pi, c.nb + nb2);
                            c.d[nb2] = -c.rg[nb2];
                            if nb2 <= NMAX {
                                let base = nb2 * (nb2 - 1) / 2;
                                for kk in 1..=nb2 {
                                    c.r[base + kk] = if kk == nb2 { 1.0 } else { 0.0 };
                                }
                            }
                            a0 = false;
                        }
                    }
                    pi += 1;
                }
            }
            if b4 {
                if a0 && b0 {
                    break 'main;
                }
                for k in 1..=c.nsb {
                    c.d[k] = -c.rg[k];
                }
                b0 = true;
                c.resetr(&mut b0);
                b4 = false;
            }
            for k in 1..=n {
                c.gold[k] = c.rg[k];
            }

            let mut degen = 0usize;
            if nb > 0 {
                for k in 1..=nb {
                    let row = c.rows[k];
                    let mut s = 0.0;
                    for pp in 1..=c.nsb {
                        let lv = c.list[pp];
                        if lv <= n {
                            s += f64::from(c.jac[row][lv]) * c.d[pp];
                        } else if lv - n == row {
                            s -= c.d[pp];
                        }
                    }
                    c.w0[k] = s;
                }
                for i in 1..=nb {
                    let mut s = 0.0;
                    for k in 1..=nb {
                        s += c.binv[i][k] * c.w0[k];
                    }
                    c.v[i] = -s;
                }
                let mut best = BIG;
                let mut d54 = 0usize;
                for i in 1..=nb {
                    let mv = -c.v[i];
                    if EPSX >= mv.abs() {
                        continue;
                    }
                    let b = c.bas[i];
                    let xb = c.x[b];
                    let r = if 0.0 > mv {
                        (xb - c.ub[b] - TOL) / mv
                    } else {
                        (xb - c.lb[b] + TOL) / mv
                    };
                    if r < best {
                        best = r;
                        d54 = i;
                    }
                }
                if d54 != 0 {
                    let mut big = 0.0f64;
                    let mut t3 = BIG;
                    for i in 1..=nb {
                        let mv = -c.v[i];
                        let av = mv.abs();
                        if EPSX > av {
                            continue;
                        }
                        let b = c.bas[i];
                        let bound = if 0.0 > mv { c.ub[b] } else { c.lb[b] };
                        let r2 = (c.x[b] - bound) / mv;
                        if r2 > best || big > av {
                            continue;
                        }
                        big = av;
                        d54 = i;
                        t3 = r2;
                    }
                    let b = c.bas[d54];
                    if b > n {
                        let vv = c.v[d54];
                        let bound = if vv > 0.0 { c.ub[b] } else { c.lb[b] };
                        t3 = (bound - c.g[b - n]) / vv;
                    }
                    if t3 <= DEGEN {
                        degen = d54;
                    }
                }
            }
            if degen != 0 {
                c.d58 = degen;
                e4 = true;
                r13 += 1;
                if r13 >= n.div_ceil(2) && r13 >= 15 {
                    code = 2;
                    break 'main;
                }
                c.t = 0.0;
                let d58 = c.d58;
                let mut q = 1usize;
                if c.nsb != 1 {
                    let w: Vec<f64> = (0..=nb)
                        .map(|k| if k == 0 { 0.0 } else { c.binv[d58][k] })
                        .collect();
                    let mut xmax = 0.0f64;
                    for pp in 1..=c.nsb {
                        let lv = c.list[pp];
                        let mut s = 0.0;
                        for k in 1..=nb {
                            let row = c.rows[k];
                            let co = if lv > n {
                                if lv - n == row {
                                    -1.0
                                } else {
                                    continue;
                                }
                            } else {
                                f64::from(c.jac[row][lv])
                            };
                            s += co * w[k];
                        }
                        c.d[pp] = s;
                        if s.abs() > xmax {
                            xmax = s.abs();
                            q = pp;
                        }
                    }
                    let thr = xmax * 0.1;
                    let mut bd = 0.0f64;
                    let mut r9 = 0usize;
                    for pp in 1..=c.nsb {
                        if thr > c.d[pp].abs() {
                            continue;
                        }
                        let lv = c.list[pp];
                        let a = (c.ub[lv] - c.x[lv]).abs();
                        let b = (c.x[lv] - c.lb[lv]).abs();
                        let dd = if a < b { a } else { b };
                        if bd > dd {
                            continue;
                        }
                        bd = dd;
                        r9 = pp;
                    }
                    if r9 > 0 {
                        q = r9;
                    }
                }
                let ev_ = c.list[q];
                let mut a = vec![0.0f64; nb + 1];
                for k in 1..=nb {
                    a[k] = if ev_ > n {
                        if c.rows[k] == ev_ - n {
                            -1.0
                        } else {
                            0.0
                        }
                    } else {
                        f64::from(c.jac[c.rows[k]][ev_])
                    };
                }
                for r in 1..=nb {
                    let mut s = 0.0;
                    for k in 1..=nb {
                        s += c.binv[r][k] * a[k];
                    }
                    c.wf[r] = s;
                }
                let inv = 1.0 / c.wf[d58];
                for k in 1..=nb {
                    c.binv[d58][k] *= inv;
                }
                for r in 1..=nb {
                    if r == d58 {
                        continue;
                    }
                    for k in 1..=nb {
                        let s = c.wf[r] * c.binv[d58][k];
                        c.binv[r][k] -= s;
                    }
                }
                let lv = c.bas[d58];
                let r10 = c.pos41[d58];
                c.bas[d58] = ev_;
                let mut r11 = ev_;
                if ev_ > n {
                    if let Some(k) = (1..=nb).find(|&k| c.rows[k] == ev_ - n) {
                        r11 = n + k;
                    }
                }
                c.pos41[d58] = r11;
                let nsb = c.nsb;
                for i in q..nsb {
                    c.pos41[nb + i] = c.pos41[nb + i + 1];
                    c.list[i] = c.list[i + 1];
                }
                c.stat[nsb] = 0;
                c.pos41[nsb + nb] = r10;
                if TOL >= (c.x[lv] - c.ub[lv]).abs() {
                    c.stat[nsb] = 1;
                }
                if TOL >= (c.x[lv] - c.lb[lv]).abs() {
                    c.stat[nsb] = -1;
                }
                c.list[nsb] = lv;
                if c.stat[nsb] != 0 {
                    c.nsb -= 1;
                }
                b0 = false;
                c.c0 = false;
                c.a4 = true;
                c.cc = true;
                continue 'reduced;
            }
            break 'reduced;
        }

        e4 = false;
        c.d0 = false;
        c.search()?;
        for j in 1..=n {
            if EPSX > c.x[j].abs() {
                c.x[j] = 0.0;
            }
        }
        c.d50 += 1;
        if c.cc {
            let mut esi = c.nsb;
            let nsb0 = c.nsb;
            for r14 in 1..=nsb0 {
                let k = nsb0 - r14 + 1;
                let v = c.list[k];
                let at = (c.x[v] - c.lb[v]).abs() <= TOL || (c.x[v] - c.ub[v]).abs() <= TOL;
                if at {
                    esi -= 1;
                    let nsbv = c.nsb;
                    delcol(&mut c.r, nsbv, k);
                    c.nsb -= 1;
                    for q in k..=esi {
                        c.gold[q] = c.gold[q + 1];
                    }
                }
            }
        }
        if !c.d4 {
            if b4 {
                code = 2;
                break 'main;
            }
            b4 = true;
            c.a8 = true;
            continue;
        }
        if c.d0 {
            code = 4;
            break 'main;
        }
        b0 = false;
        b4 = false;
    }
    let infeasible = c.d24 != 0;
    if infeasible {
        c.g[m] = c.tobj;
    }
    let xs: Vec<f64> = c.x[1..=n].to_vec();
    let e = (c.ev)(&xs)?;
    let st = if infeasible {
        Status::Infeasible
    } else {
        match code {
            1 => Status::Converged,
            4 => Status::Unbounded,
            _ => Status::Optimal,
        }
    };
    Ok((xs, e, st))
}
