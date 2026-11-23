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

    pub direct: Vec<bool>,

    pub start: Vec<f64>,
}

#[derive(Clone, PartialEq, Debug)]
pub enum LpStatus {
    Optimal(Vec<f64>),

    Infeasible(Vec<f64>),
    Unbounded,
}

const INF: f64 = 1e30;
const TINY: f64 = 1e-15;

const E_COL: f64 = 1e-5;

const E_ROW: f64 = 1e-6;

const E_FEAS: f64 = 1e-6;

const L: u8 = b'L';
const U: u8 = b'U';
const X: u8 = b'X';
const F: u8 = b'F';

struct Eng {
    n: usize,
    m: usize,

    t: Vec<Vec<f64>>,
    lb: Vec<f64>,
    ub: Vec<f64>,

    rng: Vec<f64>,
    x: Vec<f64>,

    state: Vec<u8>,

    sense: Vec<u8>,
    art: Vec<bool>,
    colvar: Vec<usize>,
    rowvar: Vec<usize>,

    w: Vec<f64>,

    nc: usize,

    dir: u8,
}

impl Eng {
    fn sq(&self, j: usize) -> f64 {
        (self.w[j] + 1.0).sqrt()
    }

    fn pivot_col(&self, obj: usize, ncols: usize) -> usize {
        let (mut best, mut bv) = (0, 0.0);
        for j in 1..=ncols {
            let a = self.t[obj][j] / self.sq(j);
            let mag = a.abs();
            match self.state[j] {
                L => {
                    if a > 0.0 && mag > bv {
                        best = j;
                        bv = mag;
                    }
                }
                U => {
                    if 0.0 > a && mag > bv {
                        best = j;
                        bv = mag;
                    }
                }
                X if mag * self.sq(j) > E_COL => return j,
                _ => {}
            }
        }
        if bv * self.sq(best) > E_COL {
            best
        } else {
            0
        }
    }

    fn tie_break(&self, col: usize, ra: usize, rb: usize, dir: bool) -> usize {
        let r10 = if dir { rb } else { ra };
        let (va, vb) = (self.rowvar[ra], self.rowvar[rb]);
        let mut pick = if va > vb {
            if dir {
                ra
            } else {
                rb
            }
        } else {
            r10
        };
        let mut minv = va.min(vb);
        let (ia, ib) = (1.0 / self.t[ra][col], 1.0 / self.t[rb][col]);
        for j in 1..=self.n {
            if j == col {
                continue;
            }
            let (ta, tb) = (self.t[ra][j], self.t[rb][j]);
            let a = if ta.abs() > TINY { ta * ia } else { 0.0 };
            let b = if tb.abs() > TINY { tb * ib } else { 0.0 };
            if a == b || minv <= self.colvar[j] {
                continue;
            }
            minv = self.colvar[j];
            pick = if a > b {
                r10
            } else if dir {
                ra
            } else {
                rb
            };
        }
        pick
    }

    fn pivot_row(&mut self, col: usize, obj: usize) -> i64 {
        let n = self.n;
        let dir = self.t[obj][col] > 0.0;
        let (mut best, mut bratio) = (0usize, INF);
        for pass in 0..2 {
            for i in 1..=self.m {
                let a = self.t[i][col];
                if !(if pass == 0 { a > E_ROW } else { -E_ROW > a }) {
                    continue;
                }
                let var = self.rowvar[i];
                let lower = (pass == 0) == dir;
                let bound = if lower {
                    if var > n {
                        0.0
                    } else {
                        let v = self.lb[var];
                        if -INF >= v || v >= INF {
                            continue;
                        }
                        v
                    }
                } else {
                    let v = if var > n {
                        self.rng[var - n]
                    } else {
                        self.ub[var]
                    };
                    if -INF >= v || v >= INF {
                        continue;
                    }
                    v
                };
                let ratio = ((self.t[i][0] - bound) / a).abs();
                let take = if best == 0 || bratio > ratio {
                    true
                } else if ratio == bratio {
                    self.tie_break(col, best, i, dir) != best
                } else {
                    false
                };
                if take {
                    best = i;
                    bratio = ratio;
                    self.dir = if lower { L } else { U };
                }
            }
        }
        let j = self.colvar[col];
        let lbj = if j > n { 0.0 } else { self.lb[j] };
        let ubj = if j > n { self.rng[j - n] } else { self.ub[j] };
        if lbj > -INF && INF > ubj {
            let range = ubj - lbj;
            let flip = if bratio > range {
                true
            } else if bratio == range {
                (1..=self.m).all(|i| {
                    let (xv, var) = (self.t[i][0], self.rowvar[i]);
                    if var <= n {
                        xv != self.lb[var] && xv != self.ub[var]
                    } else {
                        xv != 0.0 && xv != self.rng[var - n]
                    }
                })
            } else {
                false
            };
            if flip {
                self.dir = if self.state[col] == L { U } else { L };
                return -(col as i64);
            }
        }
        best as i64
    }

    fn pivot(&mut self, row: i64, col: usize, phase1: bool) {
        let (n, m1) = (self.n, self.m + 1);
        if row < 0 {
            let j = self.colvar[col];
            let target = self.dir;
            let delta = if j > n {
                self.state[col] = if target == U { U } else { L };
                if target == U {
                    self.rng[j - n]
                } else {
                    -self.rng[j - n]
                }
            } else {
                let old = self.x[j];
                self.x[j] = if target == U { self.ub[j] } else { self.lb[j] };
                self.state[col] = if target == U { U } else { L };
                -old + self.x[j]
            };
            if delta.abs() > TINY {
                for i in 0..=m1 {
                    let a = self.t[i][col];
                    if a.abs() > TINY {
                        self.t[i][0] -= a * delta;
                    }
                }
            }
            return;
        }
        let row = row as usize;

        let v1 = match self.state[col] {
            X => 0.0,
            st => {
                let var = self.colvar[col];
                if st == L {
                    if var > n {
                        0.0
                    } else {
                        self.lb[var]
                    }
                } else if var <= n {
                    self.ub[var]
                } else {
                    let r = self.rng[var - n];
                    if r == INF {
                        0.0
                    } else {
                        r
                    }
                }
            }
        };
        if v1.abs() > TINY {
            for i in 0..=m1 {
                let a = self.t[i][col];
                if a.abs() > TINY {
                    self.t[i][0] += v1 * a;
                }
            }
        }

        let rv = self.rowvar[row];
        let v2;
        if rv > n {
            let r = self.rng[rv - n];
            if self.dir == U && INF > r {
                v2 = r;
                self.state[col] = U;
            } else {
                v2 = 0.0;
                self.state[col] = L;
            }
        } else if self.dir == U {
            self.x[rv] = self.ub[rv];
            v2 = self.x[rv];
            self.state[col] = U;
        } else {
            self.x[rv] = self.lb[rv];
            v2 = self.x[rv];
            self.state[col] = L;
        }
        self.t[row][0] -= v2;
        let p = 1.0 / self.t[row][col];
        let prow = {
            let r = &mut self.t[row];
            for j in (0..col).chain(col + 1..=n) {
                if r[j].abs() > TINY {
                    r[j] *= p;
                }
            }
            r.clone()
        };
        let objrow = if phase1 { m1 } else { 0 };
        let last = if phase1 { self.m } else { m1 };
        let eliminate = |s: &mut Eng, i: usize, weights: bool| {
            let f = s.t[i][col];
            if f.abs() <= TINY {
                return;
            }
            for j in (0..col).chain(col + 1..=n) {
                if prow[j].abs() > TINY {
                    let v = s.t[i][j] - prow[j] * f;
                    s.t[i][j] = v;
                    if weights && j > 0 && -E_COL > s.t[objrow][j] {
                        s.w[j] += v * v;
                    }
                }
            }
        };
        eliminate(self, 0, false);
        if phase1 {
            eliminate(self, m1, false);
        }
        for i in (1..row).chain(row + 1..=last) {
            eliminate(self, i, true);
        }
        for i in (0..row).chain(row + 1..=m1) {
            let a = self.t[i][col];
            if a.abs() > TINY {
                self.t[i][col] = -p * a;
            }
        }
        self.t[row][col] = p;
        let (cv, rv2) = (self.colvar[col], self.rowvar[row]);
        self.colvar[col] = rv2;
        self.rowvar[row] = cv;
    }

    fn artificial_update(&mut self, row: i64, col: usize) {
        if row <= 0 {
            return;
        }
        let row = row as usize;
        if !self.art[row] {
            return;
        }
        self.art[row] = false;
        let m1 = self.m + 1;
        match self.sense[row] {
            b'>' => {
                self.t[m1][col] += 1.0;
                for i in 0..=m1 {
                    self.t[i][col] = -self.t[i][col];
                }
            }
            b'=' => {
                let nc = self.nc;
                for i in 0..=m1 {
                    let old = self.t[i][col];
                    self.t[i][col] = self.t[i][nc];
                    self.t[i][nc] = -old;
                }
                self.colvar.swap(col, nc);
                self.state.swap(col, nc);
                self.nc -= 1;
            }
            _ => {}
        }
    }

    fn phase_two(&mut self, limit: usize) -> u8 {
        self.w.iter_mut().for_each(|v| *v = 0.0);
        for _ in 0..limit {
            let col = self.pivot_col(0, self.nc);
            if col == 0 {
                return 0;
            }
            let row = self.pivot_row(col, 0);
            if row == 0 {
                return 4;
            }
            self.pivot(row, col, false);
        }
        0
    }

    fn simplex(&mut self) -> u8 {
        let (n, m) = (self.n, self.m);
        let limit = 200 * (n + m + 10);

        for j in 1..=n {
            let (l, u) = (self.lb[j], self.ub[j]);
            let xs = self.x[j];
            let (st, v) = if l == u {
                (F, l)
            } else if INF > u {
                if l > -INF {
                    if 2.0 * xs > l + u {
                        (U, u)
                    } else {
                        (L, l)
                    }
                } else {
                    (U, u)
                }
            } else if l > -INF {
                (L, l)
            } else {
                (X, 0.0)
            };
            self.state[j] = st;
            self.x[j] = v;
            if v.abs() > TINY {
                for i in 0..=m {
                    let a = self.t[i][j];
                    if a.abs() > TINY {
                        self.t[i][0] -= a * v;
                    }
                }
            }
        }
        for i in 1..=m {
            if 0.0 > self.t[i][0] {
                for j in 0..=n {
                    self.t[i][j] = -self.t[i][j];
                }
                self.sense[i] = match self.sense[i] {
                    b'<' => b'>',
                    b'>' => b'<',
                    s => s,
                };
            }
        }
        for j in 0..=n {
            let mut sum = 0.0;
            for i in 1..=m {
                if self.rng[i] == INF {
                    if self.sense[i] != b'<' {
                        sum += self.t[i][j];
                    }
                } else if self.sense[i] == b'<' {
                    if self.t[i][0] > self.rng[i] {
                        sum += if j == 0 {
                            self.t[i][0] - self.rng[i]
                        } else {
                            self.t[i][j]
                        };
                    }
                } else {
                    sum += self.t[i][j];
                }
            }
            self.t[m + 1][j] = sum;
        }
        for j in 1..=n {
            self.colvar[j] = j;
            self.w[j] = 0.0;
        }
        for i in 1..=m {
            self.rowvar[i] = n + i;
            self.art[i] = true;
        }
        self.nc = n;
        let mut guard = 0;
        loop {
            let col = self.pivot_col(m + 1, self.nc);
            if col == 0 {
                break;
            }
            let row = self.pivot_row(col, m + 1);
            if row == 0 {
                return 4;
            }
            if row > 0 {
                let r = row as usize;
                let rv = self.rowvar[r];
                if rv > n {
                    let rg = self.rng[rv - n];
                    if INF > rg && self.dir == L && self.t[r][0] > rg && self.sense[r] == b'<' {
                        self.t[m + 1][0] += rg;
                    }
                }
            }
            self.pivot(row, col, true);
            self.artificial_update(row, col);
            guard += 1;
            if guard > limit {
                break;
            }
        }
        if self.t[m + 1][0] > E_FEAS {
            return 5;
        }
        for r in 1..=m {
            if !self.art[r] {
                continue;
            }
            match self.sense[r] {
                b'<' => self.art[r] = false,
                b'>' => {
                    for j in 0..=n {
                        self.t[r][j] = -self.t[r][j];
                    }
                    self.art[r] = false;
                }
                b'=' => {
                    let (mut idx, mut mx) = (0, 0.0);
                    for j in 1..=self.nc {
                        let v = self.t[r][j].abs();
                        if v > mx {
                            mx = v;
                            idx = j;
                        }
                    }
                    if mx > E_ROW && idx > 0 {
                        self.pivot(r as i64, idx, true);
                        self.artificial_update(r as i64, idx);
                    }
                }
                _ => {}
            }
        }
        self.phase_two(limit)
    }

    fn solution(&self) -> Vec<f64> {
        let mut x = self.x[1..=self.n].to_vec();
        for i in 1..=self.m {
            let v = self.rowvar[i];
            if v <= self.n {
                x[v - 1] = self.t[i][0];
            }
        }
        x
    }
}

pub fn solve(lp: &Lp) -> LpStatus {
    let n = lp.n;
    let mut lb = vec![if lp.nonneg { 0.0 } else { -INF }; n + 1];
    let mut ub = vec![INF; n + 1];
    let mut rows: Vec<&Row> = Vec::new();
    for (ri, r) in lp.rows.iter().enumerate() {
        let nz: Vec<usize> = (0..n).filter(|&j| r.coef[j] != 0.0).collect();
        let bound = lp.direct.get(ri).copied().unwrap_or(true);
        if let ([j], true) = (&nz[..], bound) {
            let j = *j;
            let v = r.rhs / r.coef[j];
            let flip = r.coef[j] < 0.0;
            match (r.rel, flip) {
                (Rel::Eq, _) => {
                    lb[j + 1] = lb[j + 1].max(v);
                    ub[j + 1] = ub[j + 1].min(v);
                }
                (Rel::Le, false) | (Rel::Ge, true) => ub[j + 1] = ub[j + 1].min(v),
                _ => lb[j + 1] = lb[j + 1].max(v),
            }
        } else {
            rows.push(r);
        }
    }
    let m = rows.len();
    let mut e = Eng {
        n,
        m,
        t: vec![vec![0.0; n + 1]; m + 2],
        lb,
        ub,
        rng: vec![INF; m + 1],
        x: vec![0.0; n + 1],
        state: vec![0; n + 1],
        sense: vec![0; m + 1],
        art: vec![false; m + 1],
        colvar: vec![0; n + 1],
        rowvar: vec![0; m + 1],
        w: vec![0.0; n + 1],
        nc: n,
        dir: L,
    };
    for j in 1..=n {
        e.x[j] = lp.start.get(j - 1).copied().unwrap_or(0.0);
        e.t[0][j] = if lp.maximize {
            lp.obj[j - 1]
        } else {
            -lp.obj[j - 1]
        };
    }
    for (i, r) in rows.iter().enumerate() {
        let i = i + 1;
        e.t[i][0] = r.rhs;
        for j in 1..=n {
            e.t[i][j] = r.coef[j - 1];
        }
        e.sense[i] = match r.rel {
            Rel::Le => b'<',
            Rel::Eq => b'=',
            Rel::Ge => b'>',
        };
    }
    match e.simplex() {
        0 => LpStatus::Optimal(e.solution()),
        5 => LpStatus::Infeasible(e.solution()),
        _ => LpStatus::Unbounded,
    }
}

pub fn bounds_conflict(lp: &Lp, direct: &[bool]) -> bool {
    let mut lo = vec![if lp.nonneg { 0.0 } else { f64::NEG_INFINITY }; lp.n];
    let mut hi = vec![f64::INFINITY; lp.n];
    for (ri, r) in lp.rows.iter().enumerate() {
        let nz: Vec<usize> = (0..lp.n).filter(|&j| r.coef[j] != 0.0).collect();
        if let ([j], true) = (&nz[..], direct.get(ri).copied().unwrap_or(false)) {
            let j = *j;
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
            direct: Vec::new(),
            start: Vec::new(),
        };
        assert_eq!(solve(&lp), LpStatus::Optimal(vec![3.0, 1.0]));
    }
}
