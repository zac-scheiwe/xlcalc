use super::{not_lt, Evo, Fe};
use crate::error::OmError;

fn minsd(a: f64, b: f64) -> f64 {
    if a < b {
        a
    } else {
        b
    }
}

fn maxsd(a: f64, b: f64) -> f64 {
    if a > b {
        a
    } else {
        b
    }
}

fn jac_rotate(l: &mut [Vec<f64>], i: usize, a: f64, b: f64) {
    let (c, s) = if a == 0.0 {
        let s = if b > 0.0 {
            1.0
        } else if 0.0 > b {
            -1.0
        } else {
            0.0
        };
        (0.0, s)
    } else {
        let den = (a * a + b * b).sqrt();
        (a / den, b / den)
    };
    let (top, bot) = l.split_at_mut(i + 1);
    for (y, w) in top[i].iter_mut().zip(bot[0].iter_mut()) {
        let (yv, wv) = (*y, *w);
        *y = yv * c - wv * s;
        *w = yv * s + wv * c;
    }
}

struct Tol<'a> {
    sx: &'a [f64],
    gradtol: f64,
    steptol: f64,

    typf: f64,
    itnlim: i32,
}

fn stop_test(
    t: &Tol<'_>,
    (xplus, g, xc): (&[f64], &[f64], &[f64]),
    fplus: f64,
    (itn, ret, mxtake): (i32, i32, bool),
    consec: &mut i32,
) -> i32 {
    if ret == 1 {
        return 3;
    }
    let d = maxsd(t.typf, fplus.abs());
    let mut rg = 0.0;
    for i in 0..xplus.len() {
        let v = maxsd(xplus[i].abs(), 1.0 / t.sx[i]) * g[i].abs() / d;
        rg = maxsd(v, rg);
    }
    if t.gradtol >= rg {
        return 1;
    }
    if itn == 0 {
        return 0;
    }
    let mut rs = 0.0;
    for i in 0..xplus.len() {
        let v = (xplus[i] - xc[i]).abs() / maxsd(xplus[i].abs(), 1.0 / t.sx[i]);
        rs = maxsd(v, rs);
    }
    if t.steptol >= rs {
        return 2;
    }
    if mxtake {
        *consec += 1;
        if *consec >= 5 {
            return 5;
        }
    } else {
        *consec = 0;
    }
    if itn >= t.itnlim {
        return 4;
    }
    0
}

impl Evo<'_, '_> {
    fn conseval(&mut self, x: &[f64], out: &mut [f64]) -> Result<f64, OmError> {
        let n = self.n;
        let fv = self.crscomp(&x[..n])?;
        self.evals += 1;
        for (i, o) in out.iter_mut().enumerate() {
            *o = if i < self.m { fv[i] } else { 0.0 };
        }
        let mut s = 0.0;
        for (j, &v) in x[..n].iter().enumerate() {
            let d = if self.lo[j] > v {
                (self.lo[j] - v) * self.w_eq
            } else if v > self.hi[j] {
                (v - self.hi[j]) * self.w_eq
            } else {
                0.0
            };
            s += d * d;
        }
        for (i, &g) in fv[..self.m].iter().enumerate() {
            let d = if self.rlo[i] > g {
                self.rlo[i] - g
            } else if g > self.rhi[i] {
                g - self.rhi[i]
            } else {
                0.0
            };
            s += d * d;
        }
        Ok(s * 0.5)
    }

    fn grad_fd(
        &mut self,
        x: &mut [f64],
        g: &mut [f64],
        (sx, fc, eta): (&[f64], f64, f64),
        wrk: &mut [f64],
    ) -> Result<(), OmError> {
        let r = eta.sqrt();
        for j in 0..self.n {
            let xj = x[j];
            let step = maxsd(xj.abs(), 1.0 / sx[j]) * r;
            x[j] = step + xj;
            let fp = self.conseval(x, wrk)?;
            x[j] = xj;
            g[j] = (fp - fc) / step;
        }
        g[self.n..].fill(0.0);
        Ok(())
    }

    fn grad_cd(
        &mut self,
        x: &mut [f64],
        g: &mut [f64],
        (sx, eta): (&[f64], f64),
        wrk: &mut [f64],
    ) -> Result<(), OmError> {
        let r = eta.powf(1.0 / 3.0);
        for j in 0..self.n {
            let step = maxsd(self.x[j].abs(), 1.0 / sx[j]) * r;
            let xj = x[j];
            x[j] = step + xj;
            let fp = self.conseval(x, wrk)?;
            x[j] = xj - step;
            let fm = self.conseval(x, wrk)?;
            x[j] = xj;
            g[j] = (fp - fm) / (step + step);
        }
        g[self.n..].fill(0.0);
        Ok(())
    }

    pub(super) fn nl_repair(&mut self, fe: &mut Fe) -> Result<(), OmError> {
        let (n, m) = (self.n, self.m);
        let nn = n.max(m);
        let eta = self.eps * self.eps;
        let gradtol = eta.powf(1.0 / 3.0);
        let steptol = eta.sqrt();
        let mut xc: Vec<f64> = (0..nn)
            .map(|i| if i < n { self.x[i] } else { 0.0 })
            .collect();
        let sx = vec![1.0; nn];
        let mut p = vec![0.0; nn];
        let mut s0 = 0.0;
        for i in 0..nn {
            s0 += xc[i] * sx[i] * xc[i] * sx[i];
        }
        let maxstep = maxsd(1000.0, s0.sqrt() * 1000.0);
        let mut gc = vec![0.0; nn];
        let mut wrk = vec![0.0; nn];
        let mut xplus = vec![0.0; nn];
        let mut gplus = vec![0.0; nn];
        let mut fc = self.conseval(&xc, &mut gc)?;
        let tol = Tol {
            sx: &sx,
            gradtol,
            steptol,
            typf: fc,
            itnlim: nn as i32,
        };
        self.grad_fd(&mut xc, &mut gc, (&sx, fc, eta), &mut wrk)?;
        let mut consec = 0;

        let mut code = stop_test(&tol, (&xc, &gc, &xc), fc, (0, -1, false), &mut consec);
        if code == 0 {
            let mut central = false;
            let mut l = vec![vec![0.0; nn]; nn];
            for (i, row) in l.iter_mut().enumerate() {
                row[i] = fc.abs() * sx[i];
            }
            let mut itn: i32 = 1;
            while itn <= nn as i32 {
                for (w, g) in wrk.iter_mut().zip(&gc) {
                    *w = -g;
                }
                p[0] = wrk[0] / l[0][0];
                for i in 1..nn {
                    let mut s = 0.0;
                    for j in 0..i {
                        s += p[j] * l[i][j];
                    }
                    p[i] = (wrk[i] - s) / l[i][i];
                }
                p[nn - 1] /= l[nn - 1][nn - 1];
                for i in (0..nn - 1).rev() {
                    let mut s = 0.0;
                    for k in i + 1..nn {
                        s += l[k][i] * p[k];
                    }
                    p[i] = (p[i] - s) / l[i][i];
                }
                for v in &mut p {
                    *v = -*v;
                }
                let mut mxtake = false;
                let mut s = 0.0;
                for i in 0..nn {
                    s += p[i] * p[i] + sx[i] * sx[i];
                }
                let newtlen = s.sqrt();
                if newtlen > maxstep {
                    let sc = maxstep / newtlen;
                    for v in &mut p {
                        *v *= sc;
                    }
                }
                let mut slope = 0.0;
                for i in 0..nn {
                    slope += gc[i] * p[i];
                }
                let mut rel = 0.0;
                for i in 0..nn {
                    let v = p[i].abs() / maxsd(xc[i].abs(), 1.0 / sx[i]);
                    rel = maxsd(v, rel);
                }
                let minlambda = steptol / rel;
                let (mut lam, mut lamprev, mut fprev) = (1.0f64, 0.0f64, 0.0f64);
                let mut fplus;
                let mut bt = 1;
                loop {
                    for i in 0..nn {
                        if p[i] == 0.0 {
                            continue;
                        }
                        let sg = if lam >= 0.0 { 1.0 } else { -1.0 };
                        let xp = lam * p[i] + xc[i];
                        xplus[i] = xp;
                        if let (Some(&lo), Some(&hi)) = (self.lo.get(i), self.hi.get(i)) {
                            if lo > xp {
                                lam = ((xc[i] - lo) / p[i]).abs() * sg;
                            } else if xp > hi {
                                lam = ((hi - xc[i]) / p[i]).abs() * sg;
                            }
                        }
                    }
                    for i in 0..nn {
                        xplus[i] = lam * p[i] + xc[i];
                    }
                    fplus = self.conseval(&xplus, &mut gc)?;
                    let ls = lam * slope;
                    if ls * 1e-4 + fc >= fplus {
                        code = 0;
                        if lam == 1.0 && newtlen > maxstep * 0.99 {
                            mxtake = true;
                        }
                        break;
                    }
                    if minlambda > lam || bt > 100 {
                        code = 1;
                        break;
                    }
                    let t = if bt == 1 {
                        -slope / ((fplus - fc - slope) + (fplus - fc - slope))
                    } else {
                        let t1 = fplus - fc - ls;
                        let t2 = fprev - fc - lamprev * slope;
                        let inv = 1.0 / (lam - lamprev);
                        let (l2, p2) = (lam * lam, lamprev * lamprev);
                        let a = (t1 / l2 - t2 / p2) * inv;
                        let b = (t2 * lam / p2 - t1 * lamprev / l2) * inv;
                        let a3 = a * 3.0;
                        let disc = b * b - a3 * slope;
                        let sg = if a > 0.0 {
                            1.0
                        } else if 0.0 > a {
                            -1.0
                        } else {
                            0.0
                        };
                        let lt = if disc > b * b {
                            disc.sqrt() * sg - b
                        } else {
                            -b - disc.sqrt() * sg
                        };
                        minsd(lam * 0.5, lt / a3)
                    };
                    fprev = fplus;
                    lamprev = lam;
                    lam = minsd(t, lam * 0.1);
                    bt += 1;
                }
                if code == 1 && !central {
                    central = true;
                    self.grad_cd(&mut xc, &mut gc, (&sx, eta), &mut wrk)?;
                    itn += 1;
                    continue;
                }
                for i in 0..nn {
                    p[i] = xplus[i] - xc[i];
                }
                if central {
                    self.grad_cd(&mut xplus, &mut gplus, (&sx, eta), &mut wrk)?;
                } else {
                    self.grad_fd(&mut xplus, &mut gplus, (&sx, fplus, eta), &mut wrk)?;
                }
                code = stop_test(
                    &tol,
                    (&xplus, &gplus, &xc),
                    fplus,
                    (itn, code, mxtake),
                    &mut consec,
                );
                if code != 0 {
                    if code == 3 {
                        xplus.copy_from_slice(&xc);
                    }
                    break;
                }
                let s: Vec<f64> = (0..nn).map(|i| xplus[i] - xc[i]).collect();
                gplus.copy_from_slice(&gc);
                let y = gc.clone();
                bfgs(&mut l, (&s, &y), (&gc, &gplus), steptol, itn);
                fc = fplus;
                xc.copy_from_slice(&xplus);
                gc.copy_from_slice(&gplus);
                itn += 1;
            }
        }
        let x48 = xplus[..n].to_vec();
        let inside = (0..n).all(|i| {
            not_lt(x48[i], self.lo[i] - self.eps) && not_lt(self.eps + self.hi[i], x48[i])
        });
        if inside {
            if let Some(f) = self.accept_feasible(x48)? {
                *fe = Fe {
                    st: 0,
                    f,
                    obj: self.objective(&self.fv),
                };
            }
        }
        Ok(())
    }
}

fn bfgs(
    l: &mut [Vec<f64>],
    (s, y): (&[f64], &[f64]),
    (gc, gplus): (&[f64], &[f64]),
    steptol: f64,
    itn: i32,
) {
    let nn = s.len();
    let mut den1 = 0.0;
    for i in 0..nn {
        den1 += y[i] * s[i];
    }
    let snorm = s.iter().fold(0.0, |a, v| a + v * v).sqrt();
    let ynrm = y.iter().fold(0.0, |a, v| a + v * v).sqrt();
    if den1 < ynrm * (snorm * steptol) || den1.is_nan() {
        return;
    }
    let mut t1: Vec<f64> = (0..nn)
        .map(|i| {
            let mut a = 0.0;
            for k in i..nn {
                a += l[k][i] * s[k];
            }
            a
        })
        .collect();
    let den2 = t1.iter().fold(0.0, |a, v| a + v * v);
    let mut alpha = (den1 / den2).sqrt();
    let mut d6 = den2;
    if itn == 1 {
        for i in 0..nn {
            t1[i] *= alpha;
            for row in l.iter_mut().skip(i) {
                row[i] *= alpha;
            }
        }
        d6 = den1;
        alpha = 1.0;
    }
    let mut skip = true;
    let mut u = vec![0.0; nn];
    for i in 0..nn {
        let mut a = 0.0;
        for k in 0..=i {
            a += t1[k] * l[i][k];
        }
        let tol = maxsd(gc[i].abs(), gplus[i].abs()) * steptol;
        if (y[i] - a).abs() >= tol {
            skip = false;
        }
        u[i] = y[i] - a * alpha;
    }
    if skip {
        return;
    }
    let sc = 1.0 / (den1 * d6).sqrt();
    for v in &mut t1 {
        *v *= sc;
    }
    for i in 1..nn {
        let (top, bot) = l.split_at_mut(i);
        for (j, row) in top.iter_mut().enumerate() {
            row[i] = bot[0][j];
        }
    }
    let mut k = nn - 1;
    while k >= 1 && u[k] == 0.0 {
        k -= 1;
    }
    for i in (0..k).rev() {
        jac_rotate(l, i, u[i], -u[i + 1]);
        u[i] = if u[i] == 0.0 {
            u[i + 1].abs()
        } else {
            (u[i + 1] * u[i + 1] + u[i] * u[i]).sqrt()
        };
    }
    for j in 0..nn {
        l[0][j] += t1[j] * u[0];
    }
    for i in 0..k {
        let (a, b) = (l[i][i], -l[i + 1][i]);
        jac_rotate(l, i, a, b);
    }
    for i in 1..nn {
        let (top, bot) = l.split_at_mut(i);
        for (j, row) in top.iter().enumerate() {
            bot[0][j] = row[i];
        }
    }
}
