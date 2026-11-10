use crate::error::OmError;
use crate::solver_evo_lp::{repair, Lin};

mod nl;

pub(crate) struct Ran1 {
    idum: i32,
    iy: i32,
    iv: [i32; 32],
}

const IM: i32 = 0x7FFF_FFFF;
const BIG: f64 = 1e30;

pub(crate) fn not_lt(a: f64, b: f64) -> bool {
    a.partial_cmp(&b) != Some(std::cmp::Ordering::Less)
}

fn schrage(r: i32) -> i32 {
    let k = r / 127_773;
    let mut v = r.wrapping_mul(16807).wrapping_sub(k.wrapping_mul(IM));
    if v < 0 {
        v += IM;
    }
    v
}

impl Ran1 {
    pub(crate) fn new(seed: i32) -> Ran1 {
        Ran1 {
            idum: if seed > 0 { -seed } else { seed },
            iy: 0,
            iv: [0; 32],
        }
    }

    pub(crate) fn next(&mut self) -> f64 {
        let mut r = self.idum;
        let iy = if r <= 0 || self.iy == 0 {
            r = r.wrapping_neg().max(1);
            for c in (0..40).rev() {
                r = schrage(r);
                if c < 32 {
                    self.iv[c] = r;
                }
            }
            self.iv[0]
        } else {
            self.iy
        };
        let v = schrage(r);
        self.idum = v;

        let j = (iy >> 26) as usize;
        self.iy = self.iv[j];
        self.iv[j] = v;
        let am = f64::from_bits(0x3E00_0000_0020_0000);
        let rnmx = f64::from_bits(0x3FEF_FFFF_BF93_5359);
        let u = f64::from(self.iy) * am;
        if u > rnmx {
            rnmx
        } else {
            u
        }
    }
}

struct Node {
    f: f64,

    obj: f64,
    age: f64,
    x: Vec<f64>,
}

impl Node {
    fn feasible(&self) -> bool {
        self.f == self.obj
    }
}

pub(crate) struct Opts {
    pub(crate) popsize: i32,
    pub(crate) seed: i32,

    pub(crate) eps: f64,

    pub(crate) conv: f64,

    pub(crate) mutation: f64,

    pub(crate) maxit: i32,

    pub(crate) limits: Option<(i32, i32)>,
}

pub(crate) enum Call<'x> {
    Eval(&'x [f64]),

    Show(i32),
}

pub(crate) type Ev<'a> = dyn FnMut(Call<'_>) -> Result<Vec<f64>, OmError> + 'a;

pub(crate) struct Prob {
    pub(crate) lo: Vec<f64>,
    pub(crate) hi: Vec<f64>,
    pub(crate) x0: Vec<f64>,
    pub(crate) max: bool,
    pub(crate) rlo: Vec<f64>,
    pub(crate) rhi: Vec<f64>,

    pub(crate) ty: Vec<i32>,
    pub(crate) groups: Vec<Vec<usize>>,
}

pub(crate) struct Outcome {
    pub(crate) code: i16,
    pub(crate) x: Vec<f64>,
}

struct Stop(i32);

enum Fail {
    Stop(Stop),
    Err(OmError),
}

impl From<OmError> for Fail {
    fn from(e: OmError) -> Fail {
        Fail::Err(e)
    }
}

struct Evo<'a, 'b> {
    ev: &'a mut Ev<'b>,
    n: usize,
    lo: Vec<f64>,
    hi: Vec<f64>,
    x0: Vec<f64>,
    max: bool,
    rng: Ran1,
    nodes: Vec<Node>,

    pop: Vec<usize>,

    loc: Vec<usize>,

    best_feas: Option<usize>,

    cur: Option<usize>,
    popsize: i32,
    eps: f64,
    conv: f64,
    mutation: f64,
    maxit: i32,

    use_mut: Vec<i32>,
    use_glob: Vec<i32>,
    cnt_mut: i32,
    cnt_glob: i32,
    iter: i32,
    dups: i32,
    thr: f64,
    miss: i32,

    x: Vec<f64>,

    feas: i32,

    subs: i32,
    limits: (Option<i32>, Option<i32>),

    m: usize,
    rlo: Vec<f64>,
    rhi: Vec<f64>,
    ty: Vec<i32>,
    groups: Vec<Vec<usize>>,

    w_eq: f64,
    w_in: f64,
    eps_eq: f64,

    streak: i32,
    lim: i32,

    evals: i32,

    fv: Vec<f64>,

    lin: Option<Lin>,

    lp_runs: i32,
    lp_dir: u8,

    ad_share: f64,
}

#[derive(Clone, Copy)]
struct Fe {
    st: i32,
    f: f64,
    obj: f64,
}

impl Evo<'_, '_> {
    fn ran(&mut self) -> f64 {
        self.rng.next()
    }

    fn comp(&mut self, x: &[f64]) -> Result<Vec<f64>, OmError> {
        let fv = (self.ev)(Call::Eval(x))?;
        self.fv.clone_from(&fv);
        Ok(fv)
    }

    fn objective(&self, fv: &[f64]) -> f64 {
        if self.max {
            -fv[self.m]
        } else {
            fv[self.m]
        }
    }

    fn feaseval(&self, fv: &[f64]) -> (f64, f64) {
        let (mut p_eq, mut p_in) = (0.0, 0.0);
        for ((&g, &lo), &hi) in fv.iter().zip(&self.rlo).zip(&self.rhi) {
            let den = g.abs() + 1.0;
            let a = (lo - g) / den;
            let b = (g - hi) / den;
            if lo == hi {
                let p = b * a;
                if -p > self.eps {
                    p_eq -= p;
                }
            } else if a > self.eps {
                p_in += a * a;
            } else if b > self.eps {
                p_in += b * b;
            }
        }
        (p_eq, p_in)
    }

    fn funceval(&mut self) -> Result<Fe, OmError> {
        self.evals += 1;
        let x = std::mem::take(&mut self.x);
        let fv = self.comp(&x);
        self.x = x;
        let fv = fv?;
        let (p_eq, p_in) = self.feaseval(&fv);
        let obj = self.objective(&fv);
        let f = p_eq * self.w_eq + p_in * self.w_in + obj;
        let mut st = 0;
        if p_eq > self.eps_eq || p_in > self.eps {
            self.streak += 1;
            if self.streak <= self.lim {
                st = 5;
            } else {
                self.streak = 0;
            }
        } else {
            self.streak = 0;
        }
        if st == 0 && f == obj {
            self.feas += 1;
        }
        Ok(Fe { st, f, obj })
    }

    fn integervar(&mut self, all: bool) {
        if self.ty.is_empty() {
            return;
        }
        for j in 0..self.n {
            let t = self.ty[j];
            if !(if all { t >= 3 } else { t == 1 || t == 2 }) {
                continue;
            }
            let y = self.x[j];
            let (fl, ce) = (y.floor(), y.ceil());
            let d = y - fl;
            if self.eps >= d.abs() || self.eps >= (ce - y).abs() {
                continue;
            }
            let u = self.ran();
            let mut v = y;
            if u > d {
                if fl >= self.lo[j] {
                    v = fl;
                } else if not_lt(self.hi[j], ce) {
                    v = ce;
                }
            } else if not_lt(self.hi[j], ce) {
                v = ce;
            } else if fl >= self.lo[j] {
                v = fl;
            }
            self.x[j] = v;
        }
    }

    fn alldiff_fill(&mut self) {
        for g in 0..self.groups.len() {
            let k = self.groups[g].len() as i32;
            let mut free = vec![true; k as usize + 1];
            for e in 0..self.groups[g].len() {
                let v = self.groups[g][e];
                let r = (self.x[v] + 0.5).floor() as i32;
                let is_free = |free: &[bool], c: i32| c >= 1 && free.get(c as usize) == Some(&true);
                let mut d = 0;
                let c = loop {
                    let mut c = r - d;
                    if c <= 0 {
                        c = k - c;
                    }
                    if is_free(&free, c) {
                        break c;
                    }
                    let mut c = d + r;
                    if c > k {
                        c -= k;
                    }
                    if is_free(&free, c) {
                        break c;
                    }
                    d += 1;
                };
                free[c as usize] = false;
                self.x[v] = f64::from(c);
            }
        }
    }

    fn crscomp(&mut self, x: &[f64]) -> Result<Vec<f64>, OmError> {
        (self.ev)(Call::Eval(x))
    }

    fn find_linear(&mut self) -> Result<usize, Fail> {
        let (n, m) = (self.n, self.m);
        if m == 0 {
            return Ok(0);
        }
        let mut jac = vec![vec![0.0; n + 1]; m + 1];
        let zeros = vec![0.0; n];
        let fz = self.crscomp(&zeros)?;
        self.evals += 1;
        for (row, v) in jac.iter_mut().zip(&fz) {
            row[0] = *v;
        }

        let mut scale = vec![1.0; n];

        let mut last = self.crscomp(&zeros)?;
        self.evals += 1;
        let mut api_x = zeros.clone();
        for j in 0..n {
            if self.ty.get(j) == Some(&3) || self.lo[j] == self.hi[j] {
                continue;
            }
            let fb = last.clone();
            let (mut t, mut used, mut moved) = (1.0, 1.0, false);
            while 1e6 > t && !moved {
                api_x[j] = zeros[j] + t;
                let fv = self.crscomp(&api_x)?;
                self.evals += 1;
                used = t;
                last.clone_from(&fv);
                for r in 0..=m {
                    let d = fv[r] - fb[r];
                    jac[r][j + 1] = d;
                    if !moved {
                        moved = d.abs() > 0.01;
                    }
                }
                t *= 10.0;
            }
            if used != 1.0 {
                let inv = 1.0 / used;
                for row in &mut jac {
                    row[j + 1] *= inv;
                }
            }
            scale[j] = used;
        }
        self.crscomp(&self.x0.clone())?;

        let mut cls = vec![vec![b'Z'; n]; m + 2];
        for r in 0..m {
            for j in 0..n {
                cls[r][j] = if jac[r][j + 1] == 0.0 { b'Z' } else { b'L' };
            }
        }
        for (j, sc) in scale.iter_mut().enumerate() {
            let mut s = *sc * 7.5;
            if self.lo[j] > s {
                s = self.lo[j];
            }
            if s > self.hi[j] {
                s = self.hi[j];
            }
            *sc = s;
        }
        let mut probe = vec![0.0; n];
        for test in 0..3 {
            let mut xr = vec![0.0; n];
            for j in 0..n {
                let mut l = self.lo[j];
                if l == -BIG {
                    l = scale[j] * -1000.0;
                }
                let mut h = self.hi[j];
                if h == BIG {
                    h = scale[j] * 1000.0;
                }
                let u = self.ran();
                xr[j] = u * (h - l) + l;
            }
            let fr = self.crscomp(&xr)?;
            self.evals += 1;
            let mut pred: Vec<f64> = (0..m).map(|r| jac[r][0]).collect();
            for j in 0..n {
                for r in 0..m {
                    pred[r] += jac[r][j + 1] * xr[j];
                }
                if test == 0 {
                    probe[j] = scale[j];
                    if j != 0 {
                        probe[j - 1] = 0.0;
                    }
                    let fv = self.crscomp(&probe)?;
                    self.evals += 1;
                    for r in 0..m {
                        if cls[r][j] == b'L' || cls[r][j] == b'Z' {
                            let p = jac[r][j + 1] * scale[j] + jac[r][0];
                            if (fv[r] - p).abs() / (fv[r].abs() + 1.0) > 1e-6 {
                                cls[r][j] = b'S';
                            }
                        }
                    }
                }
            }
            probe[n - 1] = 0.0;
            for r in 0..m {
                if (fr[r] - pred[r]).abs() / (fr[r].abs() + 1.0) > 1e-6 {
                    cls[r].fill(b'S');
                }
            }
        }

        let lin: Vec<usize> = (0..m)
            .filter(|&r| cls[r].iter().all(|&c| c == b'L' || c == b'Z'))
            .collect();
        if lin.is_empty() {
            return Ok(0);
        }

        let list: Vec<usize> = std::iter::once(0)
            .chain(lin.iter().map(|r| r + 1))
            .take(lin.len())
            .collect();
        let vars: Vec<usize> = (0..n)
            .filter(|&c| list.iter().any(|&r| cls[r][c] == b'L'))
            .collect();
        if !vars.is_empty() {
            self.lin = Some(Lin::build(
                &jac,
                &lin,
                &vars,
                (&self.lo, &self.hi),
                (&self.rlo, &self.rhi),
                self.max,
            ));
        }
        Ok(vars.len())
    }

    fn accept_feasible(&mut self, trial: Vec<f64>) -> Result<Option<f64>, OmError> {
        let x = std::mem::replace(&mut self.x, trial);
        self.integervar(false);
        let trial = std::mem::replace(&mut self.x, x);
        self.evals += 1;
        let fv = self.crscomp(&trial)?;
        let (p_eq, p_in) = self.feaseval(&fv);
        if p_eq > self.eps_eq || p_in > self.eps {
            return Ok(None);
        }
        self.x = trial;
        let obj = self.objective(&fv);
        self.fv = fv;
        Ok(Some(p_eq * self.w_eq + p_in * self.w_in + obj))
    }

    fn make_feasible(
        &mut self,
        fe: &mut Fe,
        node: Option<usize>,
        sample: bool,
    ) -> Result<(), OmError> {
        fe.st = 5;
        let n = self.n;
        let obj = self.objective(&self.fv);
        if n == self.ty.iter().filter(|&&t| t == 3).count() {
            return Ok(());
        }
        let alldiff = |s: &Self, j: usize| s.ty.get(j).is_some_and(|&t| t >= 3);
        if 0.5 > self.ran() {
            let src = node
                .filter(|&i| self.nodes[i].feasible())
                .or(self.best_feas);
            if let Some(src) = src {
                let tries = 3;
                for k in (1..=tries).rev() {
                    self.evals += 1;
                    let w = f64::from(k) / f64::from(tries) * self.ran();
                    let trial = (0..n)
                        .map(|j| {
                            if alldiff(self, j) {
                                self.x[j]
                            } else {
                                (1.0 - w) * self.nodes[src].x[j] + w * self.x[j]
                            }
                        })
                        .collect();
                    if let Some(f) = self.accept_feasible(trial)? {
                        *fe = Fe {
                            st: 0,
                            f,
                            obj: self.objective(&self.fv),
                        };
                        return Ok(());
                    }
                }
            }
        }

        let (lo0, hi0) = (self.lo.clone(), self.hi.clone());
        for j in 0..n {
            if self.ty.get(j) == Some(&3) {
                self.lo[j] = self.x[j];
                self.hi[j] = self.x[j];
            }
        }
        for g in &self.groups {
            for &j in g {
                self.lo[j] = self.x[j];
                self.hi[j] = self.x[j];
            }
        }

        let nl = !sample
            && self.best_feas.is_none_or(|b| {
                let bo = self.nodes[b].obj;
                (bo - obj) / (bo + 1.0).abs() > 0.0
            });
        let lp = nl
            || (sample && self.lp_runs < self.popsize / 2)
            || self.pop.last().is_some_and(|&t| self.nodes[t].obj > obj);
        if lp && !self.lp_repair(fe)? && nl {
            self.nl_repair(fe)?;
        }
        self.lo = lo0;
        self.hi = hi0;
        Ok(())
    }

    fn lp_repair(&mut self, fe: &mut Fe) -> Result<bool, OmError> {
        let Some(lin) = self.lin.take() else {
            return Ok(false);
        };
        let r = self.lp_try(&lin, fe);
        self.lin = Some(lin);
        r
    }

    fn lp_try(&mut self, lin: &Lin, fe: &mut Fe) -> Result<bool, OmError> {
        let violated = lin.rows[1..].iter().any(|&g| {
            let v = self.fv[g - 1];
            self.rlo[g - 1] - self.eps > v || v > self.rhi[g - 1] + self.eps
        });
        if !violated {
            return Ok(false);
        }
        self.lp_runs += 1;
        let mut x48 = self.x.clone();
        for &v in &lin.vars[1..] {
            x48[v - 1] = 0.0;
        }
        let g = self.crscomp(&x48)?;
        let mut rhs = vec![0.0; lin.nrows() + 1];
        for (i, r) in rhs.iter_mut().enumerate().skip(1) {
            let row = lin.rows[i] - 1;
            let b = if lin.rel[i] == b'>' {
                self.rlo[row]
            } else {
                self.rhi[row]
            };
            *r = b - g[row];
        }
        let st = repair(
            lin,
            &rhs,
            &mut x48,
            (&self.lo, &self.hi),
            self.eps,
            &mut self.lp_dir,
        );
        if st != 0 {
            return Ok(false);
        }

        let Some(f) = self.accept_feasible(x48)? else {
            return Ok(false);
        };
        *fe = Fe {
            st: 0,
            f,
            obj: self.objective(&self.fv),
        };
        Ok(true)
    }

    fn new_node(&mut self, f: f64, obj: f64, age: f64) -> usize {
        self.nodes.push(Node {
            f,
            obj,
            age,
            x: self.x.clone(),
        });
        self.nodes.len() - 1
    }

    fn same_x(&self, a: usize, b: usize) -> bool {
        let (xa, xb) = (&self.nodes[a].x, &self.nodes[b].x);
        (0..self.n).all(|j| self.eps > (xa[j] - xb[j]).abs())
    }

    fn merge(&mut self, nw: usize, k: usize) {
        if self.nodes[nw].age > self.nodes[k].age {
            self.nodes[k].age = self.nodes[nw].age;
        }
        self.dups += 1;
    }

    fn insert(&mut self, local: bool, nw: usize) -> bool {
        let list = if local { &self.loc } else { &self.pop };
        if list.is_empty() {
            if local {
                self.loc.push(nw);
            } else {
                self.pop.push(nw);
            }
            return true;
        }
        let f = self.nodes[nw].f;
        let tol = self.eps * 10.0;
        let head = list[0];
        if tol > (f - self.nodes[head].f).abs() && self.same_x(nw, head) {
            self.merge(nw, head);
            return false;
        }
        let pos = if self.nodes[head].f > f {
            0
        } else {
            let mut at = list.len();
            for i in 1..list.len() {
                let (prev, cur) = (list[i - 1], list[i]);
                if not_lt(f, self.nodes[prev].f) && self.nodes[cur].f >= f {
                    if tol > (f - self.nodes[cur].f).abs() && self.same_x(nw, cur) {
                        self.merge(nw, cur);
                        return false;
                    }
                    at = i;
                    break;
                }
            }
            at
        };
        let list = if local { &mut self.loc } else { &mut self.pop };
        list.insert(pos, nw);
        if list.len() as i32 > self.popsize {
            self.cull(local, nw);
        }
        true
    }

    fn cull(&mut self, local: bool, keep: usize) {
        let list = if local { &self.loc } else { &self.pop };
        let l = list.len() as i32;
        let k = list.iter().filter(|&&i| self.nodes[i].feasible()).count() as i32;
        let frac = f64::from(k) / f64::from(l);
        let mm = (f64::from(l) * 0.3) as i32;
        let m = if l - mm <= l - 3 { mm } else { 3 };
        let off = l - m - 1;
        let md = f64::from(m);
        loop {
            let u = self.ran();
            let k = (u * md) as i32 + off;
            let list = if local { &self.loc } else { &self.pop };
            let p = (k.max(1) - 1) as usize;
            let Some(&v) = list.get(p + 1) else {
                continue;
            };
            if v == keep || Some(v) == self.best_feas {
                continue;
            }
            if self.nodes[v].feasible() && self.ran() > frac {
                continue;
            }
            let list = if local { &mut self.loc } else { &mut self.pop };
            list.remove(p + 1);
            return;
        }
    }

    fn best_feasible(&self) -> Option<usize> {
        let mut best: Option<usize> = None;
        for &i in &self.pop {
            if self.nodes[i].feasible()
                && best.map_or(BIG, |b| self.nodes[b].obj) > self.nodes[i].obj
            {
                best = Some(i);
            }
        }
        best
    }

    fn best(&self) -> usize {
        self.pop[0]
    }

    fn worst(&self) -> usize {
        self.pop[self.pop.len() - 1]
    }

    fn element(list: &[usize], k: i32) -> Result<usize, Fail> {
        if k < 1 || k as usize > list.len() {
            return Err(Fail::Stop(Stop(13)));
        }
        Ok(list[k as usize - 1])
    }

    fn accept(&mut self, f: f64, obj: f64) -> usize {
        let id = self.new_node(f, obj, 0.0);
        if self.nodes[id].feasible() {
            match self.best_feas {
                Some(b) if not_lt(obj, self.nodes[b].obj) => {}
                _ => self.best_feas = Some(id),
            }
        }
        id
    }

    fn sample(&mut self) -> Result<(), Fail> {
        let lim0 = self.lim;
        self.x = self.x0.clone();
        let s0 = self.funceval()?;
        let mut period = self.n as i32;
        if s0.st != 0 {
            period *= 2;
        }
        if period == 1 {
            period = 2;
        }
        let mut r12 = self.pop.len() as i32 + 1;
        let mut k = 0;
        while r12 <= self.popsize {
            if k % period == 0 {
                for j in 0..self.n {
                    self.x[j] = if self.lo[j] > self.x0[j] {
                        self.lo[j]
                    } else if self.hi[j] < self.x0[j] {
                        self.hi[j]
                    } else {
                        self.x0[j]
                    };
                }
            } else {
                for j in 0..self.n {
                    self.x[j] = self.ran();
                }
                for j in 0..self.n {
                    self.x[j] = (self.hi[j] - self.lo[j]) * self.x[j] + self.lo[j];
                }
            }
            k += 1;
            self.integervar(true);
            self.alldiff_fill();

            if let Some(&v) = self.groups.iter().rev().find_map(|g| g.last()) {
                k = v as i32;
            }
            self.integervar(false);
            let mut fe = self.funceval()?;
            if fe.st == 5 && self.best_feas.is_some() {
                self.make_feasible(&mut fe, self.best_feas, true)?;
            }
            if fe.st != 0 {
                continue;
            }
            let (f, obj) = (fe.f, fe.obj);
            let id = self.new_node(f, obj, 0.0);
            if self.thr > obj {
                self.thr = obj;
            }
            if self.nodes[id].feasible() {
                match self.best_feas {
                    Some(b) if not_lt(obj, self.nodes[b].obj) => {}
                    _ => self.best_feas = Some(id),
                }
            }
            if !self.insert(false, id) {
                if self.best_feas == Some(id) {
                    self.best_feas = None;
                }
                r12 -= 1;
                if self.dups > 10 {
                    self.popsize -= 1;
                    self.use_mut.truncate(self.popsize as usize);
                    self.use_glob.truncate(self.popsize as usize);
                }
            }
            let n = self.n as i32;
            if self.evals > n * r12 {
                self.lim = (f64::from(n) * f64::from(r12) * f64::from(self.lim)
                    / f64::from(self.evals)) as i32;
            }
            r12 += 1;
        }
        self.lim = lim0;
        Ok(())
    }

    fn unifdelta(&mut self, d: f64) -> f64 {
        let u = self.ran();
        let t = 1.0 - f64::from(self.iter) / f64::from(self.maxit);
        let m = if 0.0 > t { 0.0 } else { t };
        (1.0 - u.powf(m * m)) * d
    }

    fn varselect(&mut self) -> (usize, usize) {
        let n1 = f64::from(self.n as i32 - 1);
        if 0.5 > self.ran() {
            return (0, self.n - 1);
        }
        let a = (n1 * self.ran()) as usize;
        let b = (n1 * self.ran()) as usize;
        (a.min(b), a.max(b))
    }

    fn mutant(&mut self) -> Result<bool, Fail> {
        self.cnt_mut += 1;
        let pop = self.popsize;
        let mut idx = 0;
        for _ in 0..2 * pop {
            idx = (f64::from(pop) * self.ran()) as i32;
            if (idx as u32) <= 1 {
                idx = 2;
            }
            if self.use_mut[idx as usize] <= 3 * self.cnt_mut / pop {
                break;
            }
        }
        self.use_mut[idx as usize] += 1;
        let parent = Self::element(&self.pop, idx)?;
        self.x.clone_from(&self.nodes[parent].x);
        let u4 = self.ran();
        let u2 = self.ran();
        let v = (f64::from(self.n as i32 - 1) * u2) as usize;
        if let Some(g) = self.groups.iter().position(|g| g.contains(&v)) {
            self.alldiff_mutate(g, u4);
        } else {
            self.mutate_var(v, u4);
        }
        self.integervar(false);
        let mut fe = self.funceval()?;
        if fe.st == 5 {
            self.make_feasible(&mut fe, Some(parent), false)?;
        }
        if fe.st != 0 {
            return Ok(false);
        }
        let (f, obj) = (fe.f, fe.obj);
        let worst = self.nodes[self.worst()].f;
        if not_lt(f, worst * 0.99) {
            return Ok(false);
        }
        let id = self.accept(f, obj);
        self.cur = Some(id);
        Ok(true)
    }

    fn mutate_var(&mut self, v: usize, u4: f64) {
        self.ran();
        if 0.5 > u4 {
            let u = self.ran();
            self.x[v] = if 0.5 <= u { self.hi[v] } else { self.lo[v] };
        } else if self.iter < self.maxit && 0.5 > self.ran() {
            if 0.5 > self.ran() {
                let d = self.unifdelta(self.hi[v] - self.x[v]);
                self.x[v] += d;
            } else {
                let d = self.unifdelta(self.x[v] - self.lo[v]);
                self.x[v] += -d;
            }
        } else {
            let u = self.ran();
            self.x[v] = (self.hi[v] - self.lo[v]) * u + self.lo[v];
        }
    }

    fn alldiff_mutate(&mut self, g: usize, u4: f64) {
        let grp = self.groups[g].clone();
        let lenf = f64::from(grp.len() as i32);
        let (mut a, mut b) = loop {
            let a = (self.ran() * lenf) as i32 as usize;
            let b = (self.ran() * lenf) as i32 as usize;
            if a != b {
                break (a.min(b), a.max(b));
            }
        };
        if 0.67 > u4 {
            while a < b {
                self.x.swap(grp[a], grp[b]);
                a += 1;
                b -= 1;
            }
        } else {
            for i in a..=b {
                let c = (self.ran() * lenf) as i32 as usize;
                if i != c {
                    self.x.swap(grp[i], grp[c]);
                }
            }
        }
    }

    fn pick(&mut self) -> i32 {
        let pop = self.popsize;
        let mut idx = 0;
        for _ in 0..2 * pop {
            idx = (f64::from(pop) * self.ran()) as i32;
            if (idx as u32) <= 1 {
                idx = 2;
            }
            if self.use_glob[idx as usize] <= 6 * self.cnt_glob / pop {
                break;
            }
        }
        idx
    }

    fn global(&mut self) -> Result<bool, Fail> {
        self.cnt_glob += 1;
        self.cur = None;
        let u1 = self.ran();
        let u2 = self.ran();
        let (mut best1, mut best2) = (self.n as i32, self.n as i32);
        let (mut bf1, mut bf2) = (BIG, BIG);
        let (mut p1, mut p2) = (0, 0);

        let rounds = if self.ad_share > u1 && 0.67 > u2 {
            1
        } else {
            2
        };
        for _ in 0..rounds {
            let (i1, i2) = loop {
                let a = self.pick();
                let b = self.pick();
                if a != b {
                    break (a, b);
                }
            };
            p1 = Self::element(&self.pop, i1)?;
            p2 = Self::element(&self.pop, i2)?;
            if self.same_x(p1, p2) {
                return Ok(false);
            }
            if bf1 > self.nodes[p1].f {
                bf1 = self.nodes[p1].f;
                best1 = i1;
            }
            if bf2 > self.nodes[p2].f {
                bf2 = self.nodes[p2].f;
                best2 = i2;
            }
        }
        self.use_glob[best1 as usize] += 1;
        self.use_glob[best2 as usize] += 1;
        let n = self.n;
        if self.ad_share > u1 {
            if !self.alldiff_cross(p1, p2, u2) {
                return Ok(false);
            }
        } else if 0.67 > u2 {
            let l = self.loc.len() as i32;
            let u = self.ran();
            let third = if 5.0 / (f64::from(l) + 5.0) > u {
                self.best()
            } else {
                let u = self.ran();
                Self::element(&self.loc, (u * f64::from(l - 1)) as i32 + 1)?
            };
            let (a, b, c) = (&self.nodes[p1], &self.nodes[third], &self.nodes[p2]);
            let (mut fa, mut fb) = (a.f - a.obj, b.f - b.obj);
            let fc;
            if fa > fb {
                fc = c.f - c.obj;
            } else {
                fa = a.f;
                fb = b.f;
                fc = c.f;
            }
            let (i1, i2) = self.varselect();
            for j in 0..n {
                let (xa, xb, xc) = (
                    self.nodes[p1].x[j],
                    self.nodes[third].x[j],
                    self.nodes[p2].x[j],
                );
                let v = if j < i1 {
                    xa
                } else if j > i2 {
                    xc
                } else {
                    let den = (xa - xc) * fb + (xc - xb) * fa + (xb - xa) * fc;
                    if den == 0.0 {
                        0.0
                    } else {
                        let (a2, b2, c2) = (xa * xa, xb * xb, xc * xc);
                        ((a2 - c2) * fb + (c2 - b2) * fa + (b2 - a2) * fc) / den * 0.5
                    }
                };
                self.x[j] = self.clamp(j, v);
            }
        } else {
            let w = self.ran();
            let (i1, i2) = self.varselect();
            for j in 0..n {
                let (x1, x2) = (self.nodes[p1].x[j], self.nodes[p2].x[j]);
                let v = if j < i1 {
                    x1
                } else if j <= i2 {
                    w * x1 + (1.0 - w) * x2
                } else {
                    x2
                };
                self.x[j] = self.clamp(j, v);
            }
        }
        self.integervar(false);
        let mut fe = self.funceval()?;
        if fe.st == 5 {
            let node = if self.nodes[p1].feasible() { p1 } else { p2 };
            self.make_feasible(&mut fe, Some(node), false)?;
        }
        let (f, obj) = (fe.f, fe.obj);
        let built = fe.st == 0 && !not_lt(f, self.nodes[self.worst()].f);
        if built {
            let id = self.accept(f, obj);
            self.iter += 1;
            self.cur = Some(id);
        }
        self.limit_check()?;
        Ok(built)
    }

    fn alldiff_cross(&mut self, p1: usize, p2: usize, u2: f64) -> bool {
        let ng = self.groups.len() as i32;
        let u = self.ran();
        let grp = self.groups[(f64::from(ng - 1) * u) as i32 as usize].clone();
        let len = grp.len();
        let lenf = f64::from(len as i32);
        let eps = self.eps;
        for &v in &grp {
            self.x[v] = -1.0;
        }
        if 0.67 > u2 {
            let (a, b) = loop {
                let a = (self.ran() * lenf) as i32 as usize;
                let b = (self.ran() * lenf) as i32 as usize;
                if a != b {
                    break (a.min(b), a.max(b));
                }
            };
            for &v in &grp[a..=b] {
                self.x[v] = self.nodes[p1].x[v];
            }
            for r in a..=b {
                let want = self.nodes[p2].x[grp[r]];
                if grp[a..=b].iter().any(|&v| eps > (self.x[v] - want).abs()) {
                    continue;
                }
                let mut cur = grp[r];
                loop {
                    let v1 = self.nodes[p1].x[cur];
                    let Some(&v) = grp
                        .iter()
                        .find(|&&v| eps > (self.nodes[p2].x[v] - v1).abs())
                    else {
                        return false;
                    };
                    cur = v;
                    if self.x[v] == -1.0 {
                        self.x[v] = want;
                        break;
                    }
                }
            }
            for &v in &grp {
                if self.x[v] == -1.0 {
                    self.x[v] = self.nodes[p2].x[v];
                }
            }
        } else {
            let cnt = (self.ran() * lenf) as i32;
            let mut filled = 0;
            while filled < cnt {
                let v = grp[(lenf * self.ran()) as i32 as usize];
                if self.x[v] == -1.0 {
                    self.x[v] = self.nodes[p1].x[v];
                    filled += 1;
                }
            }
            let mut at = 0;
            for &v in &grp {
                let want = self.nodes[p2].x[v];
                if grp.iter().any(|&w| {
                    let xv = self.x[w];
                    xv != -1.0 && eps > (xv - want).abs()
                }) {
                    continue;
                }
                while at < len && self.x[grp[at]] != -1.0 {
                    at += 1;
                }
                if at == len {
                    return false;
                }
                self.x[grp[at]] = want;
            }
        }
        true
    }

    fn limit_check(&mut self) -> Result<(), Fail> {
        self.subs += 1;
        let reason = if self.limits.0.is_some_and(|m| self.subs >= m) {
            self.limits.0 = None;
            4
        } else if self.limits.1.is_some_and(|m| self.feas >= m) {
            self.limits.1 = None;
            5
        } else {
            return Ok(());
        };
        let b = self.best_feasible().unwrap_or_else(|| self.best());
        let x = self.nodes[b].x.clone();
        self.comp(&x)?;
        if (self.ev)(Call::Show(reason))?
            .first()
            .is_some_and(|&v| v != 0.0)
        {
            return Err(Fail::Stop(Stop(6)));
        }
        Ok(())
    }

    fn clamp(&self, j: usize, v: f64) -> f64 {
        if self.lo[j] > v {
            self.lo[j]
        } else if v > self.hi[j] {
            self.hi[j]
        } else {
            v
        }
    }

    fn gamma(&mut self, a: f64) -> f64 {
        if 1.0 > a {
            let (t10, t9) = (1.0 / a, 1.0 / (1.0 - a));
            let (x2, s) = loop {
                let x1 = self.ran().powf(t9);
                let x2 = self.ran().powf(t10);
                let s = x2 + x1;
                if not_lt(1.0, s) {
                    break (x2, s);
                }
            };
            let r = -self.ran().ln();
            return (r * x2) / s;
        }
        if a == 1.0 {
            return -self.ran().ln();
        }
        let am = a - 1.0;
        let sq = ((a + a) - 1.0).sqrt();
        loop {
            let (v1, v2) = loop {
                let t = self.ran();
                let v1 = (t + t) - 1.0;
                let t = self.ran();
                let v2 = (t + t) - 1.0;
                if not_lt(1.0, v2 * v2 + v1 * v1) {
                    break (v1, v2);
                }
            };
            let y = v1 / v2;
            let e = y * sq;
            let x = e + am;
            if 0.0 >= x {
                continue;
            }
            let t6 = self.ran();
            let w = ((x / am).ln() * am - e).exp() * (y * y + 1.0);
            if t6 > w {
                continue;
            }
            return x;
        }
    }

    fn random_local(&mut self) -> usize {
        let (b, w) = (self.best(), self.worst());
        for i in 0..self.n {
            if self.ty.get(i).is_some_and(|&t| t >= 3) {
                continue;
            }
            let r = self.hi[i] - self.lo[i];
            let (xb, xw) = (self.nodes[b].x[i], self.nodes[w].x[i]);
            let u = (xb - self.lo[i]) / r;
            let s = (xw - xb).abs() * 0.1;
            let omu = 1.0 - u;
            let nu = (r * r * u * omu) / s - 1.0;
            let a = nu * u;
            if 0.0 >= a {
                return i;
            }
            let bb = omu * nu;
            if 0.0 >= bb {
                return i;
            }
            let g1 = self.gamma(a);
            let g2 = self.gamma(bb);
            let p = g1 / (g2 + g1);
            let mut y = (self.hi[i] - self.lo[i]) * p + self.lo[i];
            if !y.is_finite() {
                y = self.lo[i];
            }
            self.x[i] = self.clamp(i, y);
        }
        self.n
    }

    fn local_tries(&mut self, last: &mut Option<Fe>) -> Result<(), Fail> {
        for _ in 0..2 {
            if self.random_local() >= self.n {
                self.integervar(false);
                let mut fe = self.funceval()?;
                if fe.st == 5 && self.best_feas.is_some() {
                    self.make_feasible(&mut fe, self.best_feas, false)?;
                }
                *last = Some(fe);
                if fe.st == 0 && self.nodes[self.worst()].f > fe.f {
                    break;
                }
            }
        }
        Ok(())
    }

    fn local(&mut self, prev: usize) -> Result<bool, Fail> {
        if self.ty.iter().filter(|&&t| t >= 3).count() == self.n {
            return Ok(false);
        }
        let fin = self.nodes[prev].f;
        if fin > self.thr {
            self.miss += 1;
            if self.miss < 20 {
                return Ok(false);
            }
            self.miss = 0;
            self.thr += (self.thr.abs() + 1.0) * 0.2;
            if fin > self.thr {
                return Ok(false);
            }
        }
        let x0 = self.x.clone();
        let mut last = None;
        self.local_tries(&mut last)?;
        let Some(Fe {
            st: 0,
            mut f,
            mut obj,
        }) = last
        else {
            return Ok(false);
        };
        if self.nodes[prev].f > f {
            let age = x0
                .iter()
                .zip(&self.x)
                .fold(0.0, |s, (a, b)| s + (b - a) * (b - a))
                .sqrt();
            let id = self.new_node(f, obj, age);
            if self.insert(true, id) {
                self.thr = f;
                self.miss = 0;
            }
            if self.ran() > self.mutation {
                return Ok(false);
            }

            self.x = x0;
            let mut last = Some(Fe { st: 0, f, obj });
            self.local_tries(&mut last)?;
            match last {
                Some(Fe {
                    st: 0,
                    f: f2,
                    obj: o2,
                }) => (f, obj) = (f2, o2),
                _ => return Ok(false),
            }
        }
        if not_lt(f, self.nodes[self.worst()].f) {
            return Ok(false);
        }
        let id = self.accept(f, obj);
        self.cur = Some(id);
        Ok(true)
    }

    fn add_cur(&mut self) -> bool {
        let Some(id) = self.cur else {
            return false;
        };
        if self.insert(false, id) {
            return true;
        }
        if self.best_feas == Some(id) {
            self.best_feas = None;
        }
        self.cur = None;
        false
    }

    fn run(&mut self) -> Result<(), Fail> {
        loop {
            let l = self.pop.len() as i32;
            let e = Self::element(&self.pop, l * 99 / 100);
            if self.iter >= self.n as i32 {
                let e = e?;
                let fb = self.nodes[self.best()].f;
                let rel = (self.nodes[e].f - fb).abs() / fb.abs().max(1.0);
                if self.conv >= rel {
                    return Ok(());
                }
            }
            self.cur = None;
            let mut have = false;
            if self.mutation > self.ran() && self.mutant()? {
                have = self.add_cur();
            }
            if !have {
                if !self.global()? {
                    continue;
                }
                if !self.add_cur() {
                    continue;
                }
            }
            let prev = self.cur.unwrap_or_default();
            if self.local(prev)? {
                self.add_cur();
            }
        }
    }
}

pub(crate) fn evo(p: Prob, o: &Opts, ev: &mut Ev<'_>) -> Result<Outcome, OmError> {
    let Prob {
        lo,
        hi,
        x0,
        max,
        rlo,
        rhi,
        ty,
        groups,
    } = p;
    let n = x0.len();
    let m = rlo.len();

    let eps = if o.eps == 0.0 { 1e-8 } else { o.eps };
    let w_in = 1.0 / eps.sqrt();
    let w_eq = w_in.sqrt();
    let mut popsize = o.popsize;
    if popsize < 10 {
        popsize = (10 * n as i32).min(200);
    }
    let mut s = Evo {
        ev,
        n,
        lo,
        hi,
        x0: x0.clone(),
        max,
        rng: Ran1::new(o.seed),
        nodes: Vec::new(),
        pop: Vec::new(),
        loc: Vec::new(),
        best_feas: None,
        cur: None,
        popsize,
        eps,
        conv: o.conv,
        mutation: o.mutation,
        maxit: o.maxit,
        use_mut: vec![0; popsize as usize],
        use_glob: vec![0; popsize as usize],
        cnt_mut: 0,
        cnt_glob: 0,
        iter: 0,
        dups: 0,
        thr: BIG,
        miss: 0,
        x: x0,
        feas: -1,
        subs: 0,
        limits: match o.limits {
            Some((a, b)) => (Some(a), Some(b)),
            None => (None, None),
        },
        m,
        rlo,
        rhi,
        ty,
        groups,
        w_eq,
        w_in,

        eps_eq: 0.01,
        streak: 0,
        lim: 2 * n as i32,
        evals: 0,
        fv: Vec::new(),
        lin: None,
        lp_runs: 0,
        lp_dir: 0,
        ad_share: 0.0,
    };
    s.ad_share = s.ty.iter().filter(|&&t| t >= 3).count() as f64 / n as f64;
    let sampled = s.find_linear().and_then(|lin| {
        if lin as i32 > n as i32 / 3 {
            s.eps_eq = (eps / 100.0).min(0.01);
        }
        s.sample()?;
        s.run()
    });
    let code = match sampled {
        Ok(()) => 0,
        Err(Fail::Stop(Stop(c))) => c,
        Err(Fail::Err(e)) => return Err(e),
    };
    let best = s.best_feasible();

    for i in s.pop.clone() {
        if best.is_none() || s.nodes[i].feasible() {
            (s.ev)(Call::Eval(&s.nodes[i].x))?;
        }
    }
    let out = match best {
        Some(b) => Outcome {
            code: if code == 0 { 1 } else { code as i16 },
            x: s.nodes[b].x.clone(),
        },
        None => Outcome {
            code: 5,
            x: s.nodes[s.best()].x.clone(),
        },
    };
    (s.ev)(Call::Eval(&out.x))?;
    Ok(out)
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn ran1_seed7() {
        let mut r = Ran1::new(7);
        assert_eq!(r.next() * 4.0, 0.4913459999912167);
        assert_eq!(r.next() * 4.0, 1.713879900851231);
    }
}
