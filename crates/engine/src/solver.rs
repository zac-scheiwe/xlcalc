#[derive(Clone, Copy, PartialEq, Eq, Debug)]
pub enum Rel {
    Le,

    Eq,

    Ge,
}

#[derive(Clone, Debug)]
pub struct Row {
    pub coef: Vec<f64>,
    pub rel: Rel,
    pub rhs: f64,
}

#[derive(Clone, Debug)]
pub struct Lp {
    pub n: usize,

    pub obj: Vec<f64>,
    pub maximize: bool,
    pub rows: Vec<Row>,
    pub nonneg: bool,
}

#[derive(Clone, PartialEq, Debug)]
pub enum LpStatus {
    Optimal(Vec<f64>),

    Infeasible(Vec<f64>),
    Unbounded,
}

const EPS: f64 = 1e-9;

pub fn solve(lp: &Lp) -> LpStatus {
    let n = lp.n;
    let cols = if lp.nonneg { n } else { 2 * n };

    struct R {
        a: Vec<f64>,
        rel: Rel,
        b: f64,
    }
    let mut rs: Vec<R> = Vec::new();
    for r in &lp.rows {
        let mut a = r.coef.clone();
        if !lp.nonneg {
            a.extend(r.coef.iter().map(|v| -v));
        }
        let (mut a, mut rel, mut b) = (a, r.rel, r.rhs);
        if b < 0.0 {
            for v in &mut a {
                *v = -*v;
            }
            b = -b;
            rel = match rel {
                Rel::Le => Rel::Ge,
                Rel::Ge => Rel::Le,
                Rel::Eq => Rel::Eq,
            };
        }
        rs.push(R { a, rel, b });
    }
    let m = rs.len();
    let slacks = rs.iter().filter(|r| r.rel != Rel::Eq).count();
    let arts = rs.iter().filter(|r| r.rel != Rel::Le).count();
    let total = cols + slacks + arts;

    let mut t = vec![vec![0.0; total + 1]; m];
    let mut basis = vec![0usize; m];
    let (mut si, mut ai) = (cols, cols + slacks);
    for (i, r) in rs.iter().enumerate() {
        t[i][..cols].copy_from_slice(&r.a);
        t[i][total] = r.b;
        match r.rel {
            Rel::Le => {
                t[i][si] = 1.0;
                basis[i] = si;
                si += 1;
            }
            Rel::Ge => {
                t[i][si] = -1.0;
                si += 1;
                t[i][ai] = 1.0;
                basis[i] = ai;
                ai += 1;
            }
            Rel::Eq => {
                t[i][ai] = 1.0;
                basis[i] = ai;
                ai += 1;
            }
        }
    }
    let art_start = cols + slacks;

    if arts > 0 {
        let mut cost = vec![0.0; total];
        for c in cost.iter_mut().skip(art_start) {
            *c = 1.0;
        }
        match run(&mut t, &mut basis, &cost, total) {
            Some(_) => {}
            None => return LpStatus::Infeasible(vec![0.0; n]),
        }
        let infeas: f64 = (0..m)
            .filter(|&i| basis[i] >= art_start)
            .map(|i| t[i][total])
            .sum();
        if infeas > 1e-7 {
            let mut x = vec![0.0; total];
            for (i, &b) in basis.iter().enumerate() {
                x[b] = t[i][total];
            }
            return LpStatus::Infeasible(
                (0..n)
                    .map(|j| if lp.nonneg { x[j] } else { x[j] - x[n + j] })
                    .collect(),
            );
        }

        for i in 0..m {
            if basis[i] >= art_start {
                if let Some(j) = (0..art_start).find(|&j| t[i][j].abs() > EPS) {
                    pivot(&mut t, &mut basis, i, j, total);
                }
            }
        }
    }

    let mut cost = vec![0.0; total];
    for j in 0..n {
        let c = if lp.maximize { -lp.obj[j] } else { lp.obj[j] };
        cost[j] = c;
        if !lp.nonneg {
            cost[n + j] = -c;
        }
    }
    let barred = art_start;
    let t_rows = t.len();
    let _ = t_rows;
    match run_limited(&mut t, &mut basis, &cost, total, barred) {
        Some(_) => {}
        None => return LpStatus::Unbounded,
    }
    let mut x = vec![0.0; total];
    for (i, &b) in basis.iter().enumerate() {
        x[b] = t[i][total];
    }
    let out: Vec<f64> = (0..n)
        .map(|j| if lp.nonneg { x[j] } else { x[j] - x[n + j] })
        .collect();
    LpStatus::Optimal(out)
}

fn run(t: &mut [Vec<f64>], basis: &mut [usize], cost: &[f64], total: usize) -> Option<()> {
    run_limited(t, basis, cost, total, total)
}

fn run_limited(
    t: &mut [Vec<f64>],
    basis: &mut [usize],
    cost: &[f64],
    total: usize,
    limit: usize,
) -> Option<()> {
    for _ in 0..10_000 {
        let mut best = None;
        let mut best_val = -EPS;
        for j in 0..limit {
            if basis.contains(&j) {
                continue;
            }
            let mut z = cost[j];
            for (i, &b) in basis.iter().enumerate() {
                z -= cost[b] * t[i][j];
            }
            if z < best_val {
                best_val = z;
                best = Some(j);
            }
        }
        let Some(j) = best else { return Some(()) };

        let mut row = None;
        let mut ratio = f64::INFINITY;
        for i in 0..t.len() {
            if t[i][j] > EPS {
                let r = t[i][total] / t[i][j];
                if r < ratio - 1e-12
                    || (r <= ratio + 1e-12 && row.is_some_and(|k| basis[i] < basis[k]))
                {
                    ratio = r;
                    row = Some(i);
                }
            }
        }
        let i = row?;
        pivot(t, basis, i, j, total);
    }
    Some(())
}

fn pivot(t: &mut [Vec<f64>], basis: &mut [usize], i: usize, j: usize, total: usize) {
    let p = 1.0 / t[i][j];
    for v in &mut t[i][..=total] {
        *v *= p;
    }
    let pr = t[i].clone();
    for (k, row) in t.iter_mut().enumerate() {
        if k != i && row[j].abs() > 0.0 {
            let f = row[j];
            for (v, pv) in row.iter_mut().zip(&pr) {
                *v -= f * pv;
            }
        }
    }
    basis[i] = j;
}

pub fn bounds_conflict(lp: &Lp) -> bool {
    let mut lo = vec![if lp.nonneg { 0.0 } else { f64::NEG_INFINITY }; lp.n];
    let mut hi = vec![f64::INFINITY; lp.n];
    for r in &lp.rows {
        let nz: Vec<usize> = (0..lp.n).filter(|&j| r.coef[j] != 0.0).collect();
        if let [j] = nz[..] {
            let v = r.rhs / r.coef[j];
            let flip = r.coef[j] < 0.0;
            match (r.rel, flip) {
                (Rel::Eq, _) => {
                    lo[j] = lo[j].max(v);
                    hi[j] = hi[j].min(v);
                }
                (Rel::Le, false) | (Rel::Ge, true) => hi[j] = hi[j].min(v),
                _ => lo[j] = lo[j].max(v),
            }
        }
    }
    if (0..lp.n).any(|j| lo[j] > hi[j]) {
        return true;
    }
    lp.rows.iter().any(|r| {
        let (mut mn, mut mx) = (0.0, 0.0);
        for j in 0..lp.n {
            let c = r.coef[j];
            let (a, b) = (c * lo[j], c * hi[j]);
            let (a, b) = if c < 0.0 { (b, a) } else { (a, b) };
            mn += if c == 0.0 { 0.0 } else { a };
            mx += if c == 0.0 { 0.0 } else { b };
        }
        match r.rel {
            Rel::Le => mn > r.rhs + 1e-9,
            Rel::Ge => mx < r.rhs - 1e-9,
            Rel::Eq => mn > r.rhs + 1e-9 || mx < r.rhs - 1e-9,
        }
    })
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn small_lp() {
        let lp = Lp {
            n: 2,
            obj: vec![3.0, 2.0],
            maximize: true,
            rows: vec![
                Row {
                    coef: vec![1.0, 1.0],
                    rel: Rel::Le,
                    rhs: 4.0,
                },
                Row {
                    coef: vec![1.0, 0.0],
                    rel: Rel::Le,
                    rhs: 3.0,
                },
            ],
            nonneg: true,
        };
        assert_eq!(solve(&lp), LpStatus::Optimal(vec![3.0, 1.0]));
    }
}
