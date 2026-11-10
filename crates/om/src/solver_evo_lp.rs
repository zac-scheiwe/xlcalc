use crate::solver_evo::not_lt;

const BIG: f64 = 1e30;
const TINY: f64 = 1e-15;

const COL_TOL: f64 = 1e-5;
const ROW_TOL: f64 = 1e-6;

#[derive(Clone)]
pub(crate) struct Lin {
    pub(crate) rows: Vec<usize>,

    pub(crate) vars: Vec<usize>,

    pub(crate) t: Vec<Vec<f64>>,

    pub(crate) rel: Vec<u8>,

    pub(crate) lo: Vec<f64>,
    pub(crate) hi: Vec<f64>,
}

impl Lin {
    pub(crate) fn build(
        jac: &[Vec<f64>],
        rows: &[usize],
        vars: &[usize],
        b: (&[f64], &[f64]),
        r: (&[f64], &[f64]),
        max: bool,
    ) -> Lin {
        let rows: Vec<usize> = std::iter::once(0)
            .chain(rows.iter().map(|&i| i + 1))
            .collect();
        let vars: Vec<usize> = std::iter::once(0)
            .chain(vars.iter().map(|&j| j + 1))
            .collect();
        let (nrow, l) = (rows.len() - 1, vars.len() - 1);
        let (c, nr) = (3 * l, nrow + l);
        let (mut lo, mut hi) = (vec![0.0; c + 1], vec![0.0; c + 1]);
        for k in 1..=l {
            lo[k] = b.0[vars[k] - 1];
            hi[k] = b.1[vars[k] - 1];
            let w = hi[k] - lo[k];
            hi[k + 2 * l] = w;
            hi[k + l] = w;
        }
        let m = jac.len() - 1;
        let mut t = vec![vec![0.0; c + 1]; nr + 2];
        for i in 1..=nrow {
            for k in 0..=l {
                t[i][k] = jac[rows[i] - 1][vars[k]];
            }
        }
        for (k, row) in t[nrow + 1..=nr].iter_mut().enumerate() {
            let k = k + 1;
            row[k] = 1.0;
            row[k + l] = 1.0;
            row[k + 2 * l] = -1.0;
        }
        for k in 0..=c {
            t[0][k] = if k <= l {
                let v = jac[m][vars[k]];
                if max {
                    v
                } else {
                    -v
                }
            } else {
                -1.0
            };
        }
        let mut rel = vec![0; nr + 1];
        for i in 1..=nrow {
            let (a, h) = (r.0[rows[i] - 1], r.1[rows[i] - 1]);
            if a == h {
                t[i][0] = h - t[i][0];
                rel[i] = b'=';
            } else if a > -BIG && h >= BIG {
                t[i][0] = a - t[i][0];
                rel[i] = b'>';
            } else if -BIG >= a && BIG > h {
                t[i][0] = h - t[i][0];
                rel[i] = b'<';
            }
        }
        for v in &mut rel[nrow + 1..] {
            *v = b'=';
        }
        Lin {
            rows,
            vars,
            t,
            rel,
            lo,
            hi,
        }
    }

    pub(crate) fn nrows(&self) -> usize {
        self.rows.len() - 1
    }
}

struct Lp {
    t: Vec<Vec<f64>>,
    rel: Vec<u8>,
    lo: Vec<f64>,
    hi: Vec<f64>,

    sol: Vec<f64>,

    stat: Vec<u8>,

    list: Vec<usize>,
    basis: Vec<usize>,

    art: Vec<bool>,

    ncol: usize,
    nr: usize,
    c: usize,
}

impl Lp {
    fn pivot_col(&self, row: usize) -> usize {
        let (mut best, mut maxv) = (0, 0.0);
        for k in 1..=self.ncol {
            let v = self.t[row][k];
            let a = v.abs();
            let ok = match self.stat[k] {
                b'L' => v > 0.0,
                b'U' => 0.0 > v,
                b'X' => {
                    if a > COL_TOL {
                        return k;
                    }
                    false
                }
                _ => false,
            };
            if ok && a > maxv {
                best = k;
                maxv = a;
            }
        }
        if maxv > COL_TOL {
            best
        } else {
            0
        }
    }

    fn tie_break(&self, inc: bool, col: usize, best: usize, i: usize) -> usize {
        let (bb, bi) = (self.basis[best], self.basis[i]);
        let r10 = if inc { i } else { best };
        let other = if inc { best } else { i };
        let mut res = if bb > bi { other } else { r10 };
        let mut m = bb.min(bi);
        let (ib, ii) = (1.0 / self.t[best][col], 1.0 / self.t[i][col]);
        for k in 1..=self.c {
            if k == col {
                continue;
            }
            let a = self.t[best][k];
            let a = if a.abs() > TINY { a * ib } else { 0.0 };
            let b = self.t[i][k];
            let b = if b.abs() > TINY { b * ii } else { 0.0 };
            if a == b || m <= self.list[k] {
                continue;
            }
            m = self.list[k];
            res = if a > b { r10 } else { other };
        }
        res
    }

    fn pivot_row(&self, col: usize, objrow: usize, dir: &mut u8) -> isize {
        let inc = self.t[objrow][col] > 0.0;
        let (mut best, mut minr) = (0, BIG);
        for pass in 0..2 {
            for i in 1..=self.nr {
                let v = self.t[i][col];
                if if pass == 0 {
                    v <= ROW_TOL
                } else {
                    v >= -ROW_TOL
                } {
                    continue;
                }
                let b = self.basis[i];
                let use_lo = (pass == 0) == inc;
                let bound = if b > self.c {
                    if !use_lo {
                        continue;
                    }
                    0.0
                } else {
                    let x = if use_lo { self.lo[b] } else { self.hi[b] };
                    if -BIG >= x || x >= BIG {
                        continue;
                    }
                    x
                };
                let r = ((self.t[i][0] - bound) / v).abs();
                if best != 0
                    && not_lt(r, minr)
                    && (r != minr || self.tie_break(inc, col, best, i) == best)
                {
                    continue;
                }
                best = i;
                minr = r;
                *dir = if use_lo { b'L' } else { b'U' };
            }
        }
        let k = self.list[col];
        let (lk, hk) = if k > self.c {
            (0.0, BIG)
        } else {
            (self.lo[k], self.hi[k])
        };
        if lk > -BIG && BIG > hk {
            let range = hk - lk;
            let flip = minr > range
                || (range == minr
                    && (1..=self.nr).all(|i| {
                        let t0 = self.t[i][0];
                        let b = self.basis[i];
                        if b <= self.c {
                            t0 != self.lo[b] && t0 != self.hi[b]
                        } else {
                            t0 != 0.0
                        }
                    }));
            if flip {
                *dir = if self.stat[col] == b'L' { b'U' } else { b'L' };
                return -(col as isize);
            }
        }
        best as isize
    }

    fn pivot(&mut self, nrows: usize, row: isize, col: usize, dir: u8) {
        if row < 0 {
            let k = self.list[col];
            let mut d = -self.sol[k];
            if dir == b'U' {
                self.sol[k] = self.hi[k];
                self.stat[col] = b'U';
            } else {
                self.sol[k] = self.lo[k];
                self.stat[col] = b'L';
            }
            d += self.sol[k];
            if d.abs() > TINY {
                for i in 0..=nrows {
                    let a = self.t[i][col];
                    if a.abs() > TINY {
                        self.t[i][0] -= a * d;
                    }
                }
            }
            return;
        }
        let row = row as usize;
        let lk = self.list[col];
        let cur = if lk > self.c || self.stat[col] == b'X' {
            0.0
        } else if self.stat[col] == b'L' {
            self.lo[lk]
        } else {
            self.hi[lk]
        };
        if cur.abs() > TINY {
            for i in 0..=nrows {
                let a = self.t[i][col];
                if a.abs() > TINY {
                    self.t[i][0] += cur * a;
                }
            }
        }
        let b = self.basis[row];
        let mut v = 0.0;
        if b <= self.c && dir == b'U' {
            v = self.hi[b];
            self.sol[b] = v;
            self.stat[col] = b'U';
        } else {
            if b <= self.c {
                v = self.lo[b];
                self.sol[b] = v;
            }
            self.stat[col] = b'L';
        }
        let c = self.c;
        self.t[row][0] -= v;
        let inv = 1.0 / self.t[row][col];
        for k in (0..=c).filter(|&k| k != col) {
            let p = self.t[row][k];
            if p.abs() > TINY {
                self.t[row][k] = p * inv;
            }
        }
        let pr = self.t[row].clone();
        for i in (0..=nrows).filter(|&i| i != row) {
            let a = self.t[i][col];
            if a.abs() <= TINY {
                continue;
            }
            for k in (0..=c).filter(|&k| k != col) {
                if pr[k].abs() > TINY {
                    self.t[i][k] -= pr[k] * a;
                }
            }
        }
        for i in (0..=nrows).filter(|&i| i != row) {
            let a = self.t[i][col];
            if a.abs() > TINY {
                self.t[i][col] = -inv * a;
            }
        }
        self.t[row][col] = inv;
        std::mem::swap(&mut self.basis[row], &mut self.list[col]);
    }

    fn artificial(&mut self, row: isize, col: usize) {
        if row <= 0 || !self.art[row as usize] {
            return;
        }
        let row = row as usize;
        self.art[row] = false;
        match self.rel[row] {
            b'=' => {
                let e = self.ncol;
                for r in 0..=self.nr + 1 {
                    let x = -self.t[r][col];
                    self.t[r][col] = self.t[r][e];
                    self.t[r][e] = x;
                }
                self.list.swap(col, e);
                self.stat.swap(col, e);
                self.ncol -= 1;
            }
            b'>' => {
                self.t[self.nr + 1][col] += 1.0;
                for r in 0..=self.nr + 1 {
                    self.t[r][col] = -self.t[r][col];
                }
            }
            _ => {}
        }
    }

    fn prepare(&mut self) {
        for i in 1..=self.nr {
            let b = self.basis[i];
            if b <= self.c {
                self.sol[b] = self.t[i][0];
            }
        }
    }
}

pub(crate) fn repair(
    lin: &Lin,
    rhs: &[f64],
    x: &mut [f64],
    b: (&[f64], &[f64]),
    eps: f64,
    dir: &mut u8,
) -> i32 {
    let nrow = lin.nrows();
    let l = lin.vars.len() - 1;
    let (c, nr) = (3 * l, nrow + l);
    let mut lp = Lp {
        t: lin.t.clone(),
        rel: lin.rel.clone(),
        lo: lin.lo.clone(),
        hi: lin.hi.clone(),
        sol: vec![0.0; c + 1],
        stat: vec![0; c + 1],
        list: (0..=c).collect(),
        basis: (0..=nr).map(|i| if i == 0 { 0 } else { i + c }).collect(),
        art: vec![true; nr + 1],
        ncol: c,
        nr,
        c,
    };
    for k in 1..=l {
        lp.lo[k] = b.0[lin.vars[k] - 1];
        lp.hi[k] = b.1[lin.vars[k] - 1];
    }
    for (i, &v) in rhs.iter().enumerate().skip(1).take(nrow) {
        lp.t[i][0] = v;
    }
    for k in 1..=l {
        lp.t[nrow + k][0] = x[lin.vars[k] - 1];
        lp.t[0][k] = 0.0;
    }
    for k in 1..=c {
        let (lo, hi) = (lp.lo[k], lp.hi[k]);
        let v = if lo == hi {
            lp.sol[k] = lo;
            lp.stat[k] = b'F';
            lo
        } else if BIG > hi {
            if lo > -BIG && not_lt(hi + lo, lp.sol[k] + lp.sol[k]) {
                lp.sol[k] = lo;
                lp.stat[k] = b'L';
                lo
            } else {
                lp.sol[k] = hi;
                lp.stat[k] = b'U';
                hi
            }
        } else if lo > -BIG {
            lp.sol[k] = lo;
            lp.stat[k] = b'L';
            lo
        } else {
            lp.sol[k] = 0.0;
            lp.stat[k] = b'X';
            0.0
        };
        if v.abs() > TINY {
            for i in 0..=nr {
                let a = lp.t[i][k];
                if a.abs() > TINY {
                    lp.t[i][0] -= a * v;
                }
            }
        }
    }
    for i in 1..=nr {
        if 0.0 > lp.t[i][0] {
            for v in &mut lp.t[i] {
                *v = -*v;
            }
            lp.rel[i] = match lp.rel[i] {
                b'<' => b'>',
                b'>' => b'<',
                r => r,
            };
        }
    }
    for k in 0..=c {
        let mut s = 0.0;
        for i in 1..=nr {
            if lp.rel[i] != b'<' {
                s += lp.t[i][k];
            }
        }
        lp.t[nr + 1][k] = s;
    }
    let st = phases(&mut lp, eps, dir);
    lp.prepare();
    for k in 1..=l {
        x[lin.vars[k] - 1] = lp.sol[k];
    }
    st
}

fn phases(lp: &mut Lp, eps: f64, dir: &mut u8) -> i32 {
    let nr = lp.nr;
    loop {
        let col = lp.pivot_col(nr + 1);
        if col == 0 {
            break;
        }
        let row = lp.pivot_row(col, nr + 1, dir);
        if row == 0 {
            return 4;
        }
        lp.pivot(nr + 1, row, col, *dir);
        lp.artificial(row, col);
    }
    if lp.t[nr + 1][0] > eps {
        return 5;
    }
    for i in 1..=nr {
        if !lp.art[i] {
            continue;
        }
        match lp.rel[i] {
            b'<' => lp.art[i] = false,
            b'=' => {
                let (mut col, mut maxv) = (0, 0.0);
                for k in 1..=lp.ncol {
                    let a = lp.t[i][k].abs();
                    if a > maxv {
                        maxv = a;
                        col = k;
                    }
                }
                if maxv > ROW_TOL && col != 0 {
                    lp.pivot(nr + 1, i as isize, col, *dir);
                    lp.artificial(i as isize, col);
                }
            }
            b'>' => {
                for v in &mut lp.t[i] {
                    *v = -*v;
                }
                lp.art[i] = false;
            }
            _ => {}
        }
    }
    loop {
        let col = lp.pivot_col(0);
        if col == 0 {
            return 0;
        }
        let row = lp.pivot_row(col, 0, dir);
        if row == 0 {
            return 4;
        }
        lp.pivot(nr, row, col, *dir);
    }
}
