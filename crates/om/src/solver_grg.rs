use crate::error::OmError;

pub(crate) struct Eval {
    pub(crate) obj: f64,
    pub(crate) v: Vec<f64>,

    pub(crate) lhs: Vec<f64>,

    pub(crate) rhs: Vec<f64>,
}

pub(crate) struct Prob<'a> {
    pub(crate) max: bool,
    pub(crate) lo: &'a [f64],
    pub(crate) hi: &'a [f64],

    pub(crate) rel: &'a [i32],
    pub(crate) central: bool,

    pub(crate) val: Option<f64>,
}

#[derive(Clone, Copy, PartialEq, Eq, Debug)]
pub(crate) enum Status {
    Optimal,

    Converged,
    Unbounded,
    Infeasible,
}

pub(crate) type Ev<'a> = dyn FnMut(&[f64]) -> Result<Eval, OmError> + 'a;

const FEAS: f64 = 1e-6;

const ACTIVE: f64 = 1e-7;

const CONV: f64 = 1e-5;

const NEWTON_TOL: f64 = 1e-8;
const NEWTON_MAX: usize = 9;
const MAX_DOUBLINGS: usize = 30;

fn viol1(rel: i32, v: f64) -> f64 {
    match rel {
        1 => v.max(0.0),
        3 => (-v).max(0.0),
        _ => v.abs(),
    }
}

fn total_viol(p: &Prob, e: &Eval) -> f64 {
    e.v.iter().zip(p.rel).map(|(v, r)| viol1(*r, *v)).sum()
}

fn feasible(p: &Prob, e: &Eval) -> bool {
    e.v.iter().zip(p.rel).all(|(v, r)| viol1(*r, *v) <= FEAS)
}

struct Grad {
    obj: Vec<f64>,
    jac: Vec<Vec<f64>>,
}

fn round(p: &Prob, x: &[f64], e: &mut Eval, ev: &mut Ev) -> Result<Grad, OmError> {
    let n = x.len();
    if !p.central {
        *e = ev(x)?;
    }
    let m = e.v.len();
    let mut g = Grad {
        obj: vec![0.0; n],
        jac: vec![vec![0.0; n]; m],
    };
    for j in 0..n {
        if p.lo[j] == p.hi[j] {
            continue;
        }
        let h = 1e-6 * (1.0 + x[j].abs());
        let mut xp = x.to_vec();
        if p.central {
            xp[j] = (x[j] + h).min(p.hi[j]);
            let mut xm = x.to_vec();
            xm[j] = (x[j] - h).max(p.lo[j]);
            let ep = ev(&xp)?;
            let em = ev(&xm)?;
            let d = xp[j] - xm[j];
            g.obj[j] = (ep.obj - em.obj) / d;
            for i in 0..m {
                g.jac[i][j] = snap((ep.lhs[i] - em.lhs[i]) / d);
            }
        } else {
            let mut hs = h;
            if h + x[j] > p.hi[j] {
                hs = -h;
            }
            xp[j] = x[j] + hs;
            let ep = ev(&xp)?;
            g.obj[j] = f32q((ep.obj - e.obj) / hs);
            for i in 0..m {
                g.jac[i][j] = f32q((ep.lhs[i] - e.lhs[i]) / hs);
            }
        }
    }
    Ok(g)
}

fn snap(j: f64) -> f64 {
    let r = j.round();
    if (j - r).abs() < 1e-7 {
        r
    } else {
        j
    }
}

fn lin_solve(mut a: Vec<Vec<f64>>, mut b: Vec<f64>) -> Option<Vec<f64>> {
    let m = b.len();
    for c in 0..m {
        let piv = (c..m).max_by(|&i, &j| a[i][c].abs().total_cmp(&a[j][c].abs()))?;
        if a[piv][c].abs() < 1e-12 {
            return None;
        }
        a.swap(c, piv);
        b.swap(c, piv);
        for r in c + 1..m {
            let f = a[r][c] / a[c][c];
            let pivot_row = a[c].clone();
            for (k, pv) in pivot_row.iter().enumerate().skip(c) {
                a[r][k] -= f * pv;
            }
            b[r] -= f * b[c];
        }
    }
    let mut x = vec![0.0; m];
    for c in (0..m).rev() {
        let s: f64 = (c + 1..m).map(|k| a[c][k] * x[k]).sum();
        x[c] = (b[c] - s) / a[c][c];
    }
    Some(x)
}

struct Dir {
    u: Vec<f64>,
    sup: Vec<bool>,
    s0: f64,

    basic: Vec<(usize, Vec<(usize, f64)>)>,
}

enum Next {
    Converged,
    Stuck,
    Go(Dir),
}

fn direction(p: &Prob, x: &[f64], e: &Eval, g: &Grad, phase1: bool) -> Next {
    let n = x.len();
    let sgn = if p.max { 1.0 } else { -1.0 };
    let mut ga = vec![0.0; n];
    if phase1 {
        for (i, (v, r)) in e.v.iter().zip(p.rel).enumerate() {
            if viol1(*r, *v) <= FEAS {
                continue;
            }
            let s = match r {
                1 => 1.0,
                3 => -1.0,
                _ => v.signum(),
            };
            for (gj, jv) in ga.iter_mut().zip(&g.jac[i]) {
                *gj -= s * jv;
            }
        }
    } else {
        for (gj, ov) in ga.iter_mut().zip(&g.obj) {
            *gj = sgn * ov;
        }
    }
    let fixed: Vec<bool> = (0..n)
        .map(|j| {
            p.lo[j] == p.hi[j]
                || (x[j] >= p.hi[j] && ga[j] > 0.0)
                || (x[j] <= p.lo[j] && ga[j] < 0.0)
        })
        .collect();
    let free: Vec<usize> = (0..n).filter(|&j| !fixed[j]).collect();

    let done = if phase1 { Next::Stuck } else { Next::Converged };
    let mut act: Vec<usize> = Vec::new();
    {
        for (i, (v, r)) in e.v.iter().zip(p.rel).enumerate() {
            if phase1 && viol1(*r, *v) > FEAS {
                continue;
            }
            if *r == 2 || v.abs() <= ACTIVE {
                act.push(i);
            }
        }
        if !phase1 && act.len() == 1 {
            let i = act[0];
            let dot: f64 = free.iter().map(|&j| ga[j] * g.jac[i][j]).sum();
            let r = p.rel[i];
            if (r == 1 && dot < 0.0) || (r == 3 && dot > 0.0) {
                act.clear();
            }
        }
    }
    let m = act.len();
    let mut u = vec![0.0; n];
    let mut sup = vec![false; n];
    let mut coeffs: Vec<(usize, Vec<(usize, f64)>)> = Vec::new();
    if m == 0 {
        for &j in &free {
            u[j] = ga[j];
            sup[j] = true;
        }
    } else {
        let mut cand = free.clone();
        cand.sort_by(|&a, &b| x[b].abs().total_cmp(&x[a].abs()));
        let mut basic: Vec<usize> = Vec::new();
        for &j in &cand {
            if basic.len() == m {
                break;
            }
            let mut t = basic.clone();
            t.push(j);
            let k = t.len();
            let a: Vec<Vec<f64>> = (0..k)
                .map(|r| t.iter().map(|&c| g.jac[act[r]][c]).collect())
                .collect();

            if lin_solve(a, vec![0.0; k]).is_some() {
                basic.push(j);
            }
        }
        if basic.len() < m {
            return done;
        }
        let bt: Vec<Vec<f64>> = (0..m)
            .map(|c| (0..m).map(|r| g.jac[act[r]][basic[c]]).collect())
            .collect();
        let gb: Vec<f64> = basic.iter().map(|&j| ga[j]).collect();
        let Some(y) = lin_solve(bt, gb) else {
            return done;
        };
        let sups: Vec<usize> = free
            .iter()
            .copied()
            .filter(|j| !basic.contains(j))
            .collect();
        for &j in &sups {
            let mut r = ga[j];
            for (k, &i) in act.iter().enumerate() {
                r -= g.jac[i][j] * y[k];
            }
            u[j] = r;
            sup[j] = true;
        }
        let bm: Vec<Vec<f64>> = (0..m)
            .map(|r| basic.iter().map(|&c| g.jac[act[r]][c]).collect())
            .collect();
        let rhs: Vec<f64> = (0..m)
            .map(|r| -sups.iter().map(|&j| g.jac[act[r]][j] * u[j]).sum::<f64>())
            .collect();
        if sups.is_empty() {
            return done;
        }
        let Some(db) = lin_solve(bm, rhs) else {
            return done;
        };
        for (k, &j) in basic.iter().enumerate() {
            u[j] = db[k];
            coeffs.push((j, Vec::new()));
        }
        for &sj in &sups {
            let rj: Vec<f64> = (0..m).map(|r| -g.jac[act[r]][sj]).collect();
            let bm2: Vec<Vec<f64>> = (0..m)
                .map(|r| basic.iter().map(|&c| g.jac[act[r]][c]).collect())
                .collect();
            if let Some(w) = lin_solve(bm2, rj) {
                for (k, c) in coeffs.iter_mut().enumerate() {
                    c.1.push((sj, w[k]));
                }
            }
        }
    }
    let mut kmax = None;
    let mut umax = 0.0;
    for j in 0..n {
        if sup[j] && u[j].abs() > umax {
            umax = u[j].abs();
            kmax = Some(j);
        }
    }
    let Some(k) = kmax else {
        return if phase1 { Next::Stuck } else { Next::Converged };
    };
    if !phase1 && umax < CONV {
        return Next::Converged;
    }
    if phase1 && umax < 1e-12 {
        return Next::Stuck;
    }
    for v in &mut u {
        *v /= umax;
    }
    Next::Go(Dir {
        u,
        sup,
        s0: 0.05 * x[k].abs().max(1.0),
        basic: coeffs,
    })
}

enum Ls {
    Moved(Vec<f64>, Eval),
    Unbounded(Vec<f64>),
    Failed,
}

fn merit(p: &Prob, e: &Eval, phase1: bool) -> f64 {
    if phase1 {
        -total_viol(p, e)
    } else if p.max {
        e.obj
    } else {
        -e.obj
    }
}

fn along(x: &[f64], u: &[f64], s: f64) -> Vec<f64> {
    x.iter().zip(u).map(|(a, b)| a + s * b).collect()
}

fn vertex(p0: (f64, f64), p1: (f64, f64), p2: (f64, f64)) -> Option<f64> {
    let (a, b, c) = (p0, p1, p2);
    let (tb, tc) = (b.0 - a.0, c.0 - a.0);
    let t1 = (c.1 - a.1) * tb;
    let t2 = (b.1 - a.1) * tc;
    let den = t2 - t1;
    if den.abs() < 1e-30 {
        return None;
    }
    let num = t2 * tc - t1 * tb;
    let s = num * 0.5 / den + a.0;
    s.is_finite().then_some(s)
}
fn line_search(
    p: &Prob,
    jac: &[Vec<f64>],
    x: &[f64],
    e: &Eval,
    d: &Dir,
    phase1: bool,
    ev: &mut Ev,
) -> Result<Ls, OmError> {
    let n = x.len();
    let m0 = merit(p, e, phase1);

    let mut smax = f64::INFINITY;
    let mut limiting = None;
    for (j, &xj) in x.iter().enumerate() {
        if !d.sup[j] || d.u[j] == 0.0 {
            continue;
        }
        let b = if d.u[j] > 0.0 { p.hi[j] } else { p.lo[j] };
        if b.is_finite() {
            let s = (b - xj) / d.u[j];
            if s < smax {
                smax = s;
                limiting = Some((j, b));
            }
        }
    }
    let mut pts: Vec<(f64, f64)> = vec![(0.0, m0)];
    let mut prev_x = x.to_vec();
    let mut prev_e = Eval {
        obj: e.obj,
        v: e.v.clone(),
        lhs: e.lhs.clone(),
        rhs: e.rhs.clone(),
    };
    let mut s = d.s0;
    for k in 0..=MAX_DOUBLINGS {
        let mut st = s;
        let mut clipped = false;
        if st >= smax {
            st = smax;
            clipped = true;
        }
        let mut xt = along(x, &d.u, st);
        if clipped {
            if let Some((j, b)) = limiting {
                xt[j] = b;
            }
        }

        for (b, co) in &d.basic {
            xt[*b] = prev_x[*b]
                + co.iter()
                    .map(|(j, c)| c * (xt[*j] - prev_x[*j]))
                    .sum::<f64>();
        }
        let et = ev(&xt)?;

        let mut cross: Option<(f64, Option<(usize, f64)>)> = None;
        let mut hit: Option<(usize, f64)> = None;
        let mut consider =
            |vp: f64, vc: f64, target: f64, bound: Option<(usize, f64)>, ci: Option<usize>| {
                let f = (target - vp) / (vc - vp);
                if f.is_finite() && cross.as_ref().is_none_or(|c| f < c.0) {
                    cross = Some((f, bound));
                    hit = ci.map(|i| (i, target));
                }
            };
        for (i, r) in p.rel.iter().enumerate() {
            let (vp, vc) = (prev_e.v[i], et.v[i]);
            let was = viol1(*r, vp) <= FEAS;
            let now = viol1(*r, vc) > FEAS;
            if was && now {
                let off = if phase1 { 1e-8 } else { 0.0 };
                let target = if *r == 3 { -off } else { off };
                consider(vp, vc, target, None, Some(i));
            }
        }
        for j in 0..n {
            if d.sup[j] || p.lo[j] == p.hi[j] {
                continue;
            }
            if p.hi[j].is_finite() && xt[j] > p.hi[j] && prev_x[j] <= p.hi[j] {
                consider(
                    prev_x[j] - p.hi[j],
                    xt[j] - p.hi[j],
                    0.0,
                    Some((j, p.hi[j])),
                    None,
                );
            }
            if p.lo[j].is_finite() && xt[j] < p.lo[j] && prev_x[j] >= p.lo[j] {
                consider(
                    prev_x[j] - p.lo[j],
                    xt[j] - p.lo[j],
                    0.0,
                    Some((j, p.lo[j])),
                    None,
                );
            }
        }
        if let Some((f, bound)) = cross {
            let tp = pts[pts.len() - 1].0 / st;
            let tau = tp + (1.0 - tp) * f;
            let mut xc: Vec<f64> = if clipped {
                x.iter().zip(&xt).map(|(a, b)| a + (b - a) * tau).collect()
            } else {
                prev_x
                    .iter()
                    .zip(&xt)
                    .map(|(a, b)| a + (b - a) * f)
                    .collect()
            };
            if let Some((j, b)) = bound {
                xc[j] = b;
            }
            let ec = ev(&xc)?;
            if let (Some((i, target)), false, true) = (hit, clipped, d.basic.is_empty()) {
                let t_prev = pts[pts.len() - 1].0;
                let mut t = (st - t_prev) * f + t_prev;
                let mut jd = 0.0;
                for (j, uj) in d.u.iter().enumerate() {
                    if d.sup[j] {
                        jd += f64::from(jac[i][j] as f32) * uj;
                    }
                }
                let at = |t: f64| -> Vec<f64> {
                    x.iter()
                        .zip(&d.u)
                        .zip(&d.sup)
                        .map(|((a, u), s)| if *s { u * t + a } else { *a })
                        .collect()
                };
                let (mut xn, mut en) = (xc, ec);
                let mut prev_err = f64::INFINITY;
                for k in 0..=NEWTON_MAX {
                    if k > 0 {
                        xn = at(t);
                        en = ev(&xn)?;
                    }
                    let r = target - en.v[i];
                    let err = r.abs();
                    if err < NEWTON_TOL {
                        return Ok(Ls::Moved(xn, en));
                    }
                    if err > prev_err || k == NEWTON_MAX {
                        break;
                    }
                    t -= r / -jd;
                    prev_err = err;
                }
                return Ok(Ls::Moved(prev_x, prev_e));
            }
            if let Some((i, target)) = hit {
                let ji: Vec<f64> = jac[i].iter().map(|a| f64::from(*a as f32)).collect();
                let jj: f64 = ji.iter().map(|a| a * a).sum();
                let (mut xn, mut en) = (xc.clone(), ec);
                let mut tot = vec![0.0; xc.len()];
                for _ in 0..NEWTON_MAX {
                    let r = target - en.v[i];
                    if r.abs() <= NEWTON_TOL {
                        return Ok(Ls::Moved(xn, en));
                    }
                    for (t, ji) in tot.iter_mut().zip(&ji) {
                        *t += r * ji / jj;
                    }
                    xn = xc.iter().zip(&tot).map(|(a, t)| a + t).collect();
                    en = ev(&xn)?;
                    if (target - en.v[i]).abs() > r.abs() {
                        break;
                    }
                }
                return Ok(Ls::Moved(prev_x, prev_e));
            }
            return Ok(Ls::Moved(xc, ec));
        }

        if !phase1
            && p.rel
                .iter()
                .enumerate()
                .any(|(i, r)| *r != 2 && prev_e.v[i].abs() > ACTIVE && et.v[i].abs() <= ACTIVE)
            && feasible(p, &et)
        {
            return Ok(Ls::Moved(xt, et));
        }
        let mt = merit(p, &et, phase1);
        if phase1 && feasible(p, &et) {
            return Ok(Ls::Moved(xt, et));
        }
        let last = pts[pts.len() - 1].1;
        if mt > last {
            if clipped {
                return Ok(Ls::Moved(xt, et));
            }
            if k == MAX_DOUBLINGS {
                return Ok(Ls::Unbounded(xt));
            }
            pts.push((st, mt));
            prev_x = xt;
            prev_e = et;
            s *= 2.0;
            continue;
        }

        if pts.len() >= 2 {
            let a = pts[pts.len() - 2];
            let b = pts[pts.len() - 1];
            let best = if b.1 >= a.1 { b } else { a };
            if let Some(sv) = vertex(a, b, (st, mt)) {
                let xv = along(x, &d.u, sv);
                let evv = ev(&xv)?;
                let mv = merit(p, &evv, phase1);
                if mv > best.1 {
                    return Ok(Ls::Moved(xv, evv));
                }
            }
            if best.0 > 0.0 {
                let xb = along(x, &d.u, best.0);
                let eb = ev(&xb)?;
                return Ok(Ls::Moved(xb, eb));
            }
            return Ok(Ls::Failed);
        }

        let mut sh = st;
        for q in 1..=5 {
            sh /= f64::from(1u32 << q);
            let xs = along(x, &d.u, sh);
            let es = ev(&xs)?;
            if merit(p, &es, phase1) > m0 && (phase1 || feasible(p, &es)) {
                return Ok(Ls::Moved(xs, es));
            }
        }
        return Ok(Ls::Failed);
    }
    Ok(Ls::Failed)
}

pub(crate) fn grg(
    p: &Prob,
    x0: Vec<f64>,
    pre: usize,
    ev: &mut Ev,
) -> Result<(Vec<f64>, Eval, Status), OmError> {
    if crate::solver_grg_q::applies(p, x0.len()) {
        return crate::solver_grg_q::grg_q(p, x0, pre, ev);
    }
    if crate::solver_grg_c::applies(p, x0.len()) {
        return crate::solver_grg_c::grg_c(p, x0, pre, ev);
    }
    let mut x = x0;
    let mut e = ev(&x)?;
    for _ in 1..pre {
        e = ev(&x)?;
    }
    let mut stuck = 0;
    for _ in 0..200 {
        let g = round(p, &x, &mut e, ev)?;
        let phase1 = !feasible(p, &e);
        match direction(p, &x, &e, &g, phase1) {
            Next::Converged => {
                e = ev(&x)?;
                round(p, &x, &mut e, ev)?;
                e = ev(&x)?;
                let st = if feasible(p, &e) {
                    Status::Optimal
                } else {
                    Status::Infeasible
                };
                return Ok((x, e, st));
            }
            Next::Stuck => {
                e = ev(&x)?;
                stuck += 1;
                if stuck >= 6 {
                    return Ok((x, e, Status::Infeasible));
                }
            }
            Next::Go(d) => match line_search(p, &g.jac, &x, &e, &d, phase1, ev)? {
                Ls::Moved(nx, ne) => {
                    x = nx;
                    e = ne;
                }
                Ls::Unbounded(nx) => {
                    let ne = ev(&nx)?;
                    return Ok((nx, ne, Status::Unbounded));
                }
                Ls::Failed => {
                    e = ev(&x)?;
                    return Ok((
                        x,
                        e,
                        if phase1 {
                            Status::Infeasible
                        } else {
                            Status::Optimal
                        },
                    ));
                }
            },
        }
    }
    let st = if feasible(p, &e) {
        Status::Optimal
    } else {
        Status::Infeasible
    };
    Ok((x, e, st))
}

fn f32q(v: f64) -> f64 {
    f64::from(v as f32)
}
