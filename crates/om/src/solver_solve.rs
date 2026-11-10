use xlcalc::solver::{solve, Lp, LpStatus, Rel, Row};
use xlcalc::{CellAddr, Value};
use xlcalc_variant::Variant;

use crate::app::Application;
use crate::conv::Args;
use crate::error::OmError;
use crate::solver::{int, missing, number, parse_ref, value_error, Ctx, Ref};

type SolAnswer = (i16, Option<Vec<f64>>);

pub(crate) struct LastSolve {
    book: crate::BookId,
    cells: Vec<(CellAddr, Value)>,
    answer: i16,
}

enum Rhs {
    Num(f64),
    Text(String),
}

struct Con {
    lhs: Ref,
    rel: i32,
    rhs: Rhs,
}

struct Model {
    adj: Ref,
    opt: Option<CellAddr>,
    typ: i32,
    val: f64,
    cons: Vec<Con>,
    nonneg: bool,
    eng: i32,
}

struct Probe {
    obj: f64,
    lhs: Vec<Vec<f64>>,
    rhs: Vec<Vec<f64>>,
}

impl Model {
    fn bounded(&self) -> bool {
        let covers = |rel: i32, r: u32, col: u32| {
            self.cons.iter().any(|k| {
                k.rel == rel
                    && r >= k.lhs.r0
                    && r < k.lhs.r0 + k.lhs.rows
                    && col >= k.lhs.c0
                    && col < k.lhs.c0 + k.lhs.cols
            })
        };
        (self.adj.r0..self.adj.r0 + self.adj.rows).all(|r| {
            (self.adj.c0..self.adj.c0 + self.adj.cols)
                .all(|c| covers(1, r, c) && (self.nonneg || covers(3, r, c)))
        })
    }
}

impl Application<'_> {
    fn sol_grg(
        &mut self,
        c: &Ctx,
        m: &Model,
        n: usize,
    ) -> Result<(i16, Option<Vec<f64>>), OmError> {
        let mut lo = vec![if m.nonneg { 0.0 } else { f64::NEG_INFINITY }; n];
        let mut hi = vec![f64::INFINITY; n];
        let mut rel = Vec::new();
        let mut int_vars: Vec<usize> = Vec::new();
        let idx = |r: u32, col: u32| ((r - m.adj.r0) * m.adj.cols + (col - m.adj.c0)) as usize;
        let inside = |r: &Ref| {
            r.r0 >= m.adj.r0
                && r.c0 >= m.adj.c0
                && r.r0 + r.rows <= m.adj.r0 + m.adj.rows
                && r.c0 + r.cols <= m.adj.c0 + m.adj.cols
        };
        let mut general = Vec::new();
        for (ci, con) in m.cons.iter().enumerate() {
            if con.rel >= 4 {
                for r in con.lhs.r0..con.lhs.r0 + con.lhs.rows {
                    for col in con.lhs.c0..con.lhs.c0 + con.lhs.cols {
                        let j = idx(r, col);
                        int_vars.push(j);
                        if con.rel == 5 {
                            lo[j] = lo[j].max(0.0);
                            hi[j] = hi[j].min(1.0);
                        }
                    }
                }
                continue;
            }
            if let (true, Rhs::Num(b)) = (inside(&con.lhs), &con.rhs) {
                for r in con.lhs.r0..con.lhs.r0 + con.lhs.rows {
                    for col in con.lhs.c0..con.lhs.c0 + con.lhs.cols {
                        let j = idx(r, col);
                        if con.rel != 3 {
                            hi[j] = hi[j].min(*b);
                        }
                        if con.rel != 1 {
                            lo[j] = lo[j].max(*b);
                        }
                    }
                }
                continue;
            }
            general.push(ci);
            for _ in 0..con.lhs.rows * con.lhs.cols {
                rel.push(con.rel);
            }
        }
        int_vars.sort_unstable();
        int_vars.dedup();
        let x0: Vec<f64> = {
            let wb = &self.book(c.book)?.wb;
            (0..n)
                .map(|j| {
                    let r = m.adj.r0 + j as u32 / m.adj.cols;
                    let col = m.adj.c0 + j as u32 % m.adj.cols;
                    match wb.value(CellAddr::new(c.sheet, r, col)) {
                        Value::Number(v) => v,
                        _ => 0.0,
                    }
                })
                .collect()
        };
        let central = self.sol_num(c, "solver_drv") == Some(2.0);
        let mut ev = |x: &[f64]| -> Result<crate::solver_grg::Eval, OmError> {
            let pr = self.sol_probe(c, m, x)?;
            let (mut v, mut lhs, mut rhs) = (Vec::new(), Vec::new(), Vec::new());
            for &ci in &general {
                for e in 0..pr.lhs[ci].len() {
                    let r = &pr.rhs[ci];
                    let b = r[if r.len() == 1 { 0 } else { e }];
                    v.push(pr.lhs[ci][e] - b);
                    lhs.push(pr.lhs[ci][e]);
                    rhs.push(b);
                }
            }
            Ok(crate::solver_grg::Eval {
                obj: pr.obj,
                v,
                lhs,
                rhs,
            })
        };
        let mut best: Option<(f64, Vec<f64>)> = None;
        let mut stack = vec![(lo, hi, x0, true)];
        while let Some((lo, hi, xs, root)) = stack.pop() {
            let prob = crate::solver_grg::Prob {
                max: m.typ == 1,
                lo: &lo,
                hi: &hi,
                rel: &rel,
                central,
                val: (m.typ == 3).then_some(m.val),
            };
            let pre = if root && !int_vars.is_empty() { 2 } else { 1 };
            let (x, e, st) = crate::solver_grg::grg(&prob, xs, pre, &mut ev)?;
            match st {
                crate::solver_grg::Status::Unbounded if root => return Ok((4, Some(x))),
                crate::solver_grg::Status::Infeasible if root => return Ok((5, Some(x))),
                crate::solver_grg::Status::Converged if root && int_vars.is_empty() => {
                    return Ok((1, Some(x)))
                }
                crate::solver_grg::Status::Optimal | crate::solver_grg::Status::Converged => {}
                _ => continue,
            }
            if let Some((b, _)) = &best {
                let worse = if m.typ == 1 { e.obj <= *b } else { e.obj >= *b };
                if worse {
                    continue;
                }
            }
            match int_vars
                .iter()
                .find(|&&j| (x[j] - x[j].round()).abs() > 1e-6)
            {
                None => {
                    let mut x = x;
                    for &j in &int_vars {
                        x[j] = x[j].round();
                    }
                    best = Some((e.obj, x));
                }
                Some(&j) => {
                    let mut hi_lo = hi.clone();
                    hi_lo[j] = x[j].floor();
                    let mut x_lo = x.clone();
                    x_lo[j] = x[j].floor();
                    let mut lo_hi = lo.clone();
                    lo_hi[j] = x[j].ceil();
                    let mut x_hi = x.clone();
                    x_hi[j] = x[j].ceil();
                    stack.push((lo_hi, hi.clone(), x_hi, false));
                    stack.push((lo, hi_lo, x_lo, false));
                }
            }
        }
        Ok(match best {
            Some((_, x)) => (0, Some(x)),
            None => (5, None),
        })
    }

    fn sol_evo(
        &mut self,
        c: &Ctx,
        m: &Model,
        n: usize,
        show: Option<&str>,
    ) -> Result<Option<SolAnswer>, OmError> {
        let mut lo = vec![if m.nonneg { 0.0 } else { f64::NEG_INFINITY }; n];
        let mut hi = vec![f64::INFINITY; n];
        let inside = |r: &Ref| {
            r.r0 >= m.adj.r0
                && r.c0 >= m.adj.c0
                && r.r0 + r.rows <= m.adj.r0 + m.adj.rows
                && r.c0 + r.cols <= m.adj.c0 + m.adj.cols
        };
        let (mut rlo, mut rhi) = (Vec::new(), Vec::new());
        let mut ty = vec![0; n];
        let mut groups = Vec::new();
        let var = |r: u32, col: u32| ((r - m.adj.r0) * m.adj.cols + (col - m.adj.c0)) as usize;
        for con in &m.cons {
            let cells: Vec<(u32, u32)> = (con.lhs.r0..con.lhs.r0 + con.lhs.rows)
                .flat_map(|r| (con.lhs.c0..con.lhs.c0 + con.lhs.cols).map(move |c| (r, c)))
                .collect();
            if con.rel >= 4 {
                if con.rel > 6 || !inside(&con.lhs) {
                    return Ok(None);
                }
                let vars: Vec<usize> = cells.iter().map(|&(r, c)| var(r, c)).collect();
                for &j in &vars {
                    ty[j] = con.rel - 3;
                    if con.rel == 5 {
                        lo[j] = lo[j].max(0.0);
                        hi[j] = hi[j].min(1.0);
                    }
                }
                if con.rel == 6 {
                    groups.push(vars);
                }
                continue;
            }
            let size = cells.len();
            let rhs: Vec<f64> = match &con.rhs {
                Rhs::Num(b) => vec![*b; size],
                Rhs::Text(t) => {
                    let Some(r) = parse_ref(t, &c.sname) else {
                        return Ok(None);
                    };
                    let wb = &self.book(c.book)?.wb;
                    let mut v = Vec::new();
                    for row in r.r0..r.r0 + r.rows {
                        for col in r.c0..r.c0 + r.cols {
                            match wb.value(CellAddr::new(c.sheet, row, col)) {
                                Value::Number(x) => v.push(x),
                                _ => return Ok(None),
                            }
                        }
                    }
                    if v.len() == 1 {
                        vec![v[0]; size]
                    } else if v.len() == size {
                        v
                    } else {
                        return Ok(None);
                    }
                }
            };
            if !inside(&con.lhs) {
                for &b in &rhs {
                    rlo.push(if con.rel == 1 { -1e30 } else { b });
                    rhi.push(if con.rel == 3 { 1e30 } else { b });
                }
                continue;
            }
            for (e, &(r, col)) in cells.iter().enumerate() {
                let j = var(r, col);
                if con.rel != 3 {
                    hi[j] = hi[j].min(rhs[e]);
                }
                if con.rel != 1 {
                    lo[j] = lo[j].max(rhs[e]);
                }
            }
        }
        if ty.iter().all(|&t| t == 0) {
            ty.clear();
        }
        let x0: Vec<f64> = {
            let wb = &self.book(c.book)?.wb;
            (0..n)
                .map(|j| {
                    let r = m.adj.r0 + j as u32 / m.adj.cols;
                    let col = m.adj.c0 + j as u32 % m.adj.cols;
                    match wb.value(CellAddr::new(c.sheet, r, col)) {
                        Value::Number(v) => v,
                        _ => 0.0,
                    }
                })
                .collect()
        };
        let num = |s: &Self, name: &str, def: f64| s.sol_num(c, name).unwrap_or(def);
        let o = crate::solver_evo::Opts {
            popsize: num(self, "solver_ssz", 100.0) as i32,
            seed: num(self, "solver_rsd", 0.0) as i32,

            eps: num(self, "solver_pre", 1e-8),
            conv: num(self, "solver_cvg", 1e-5),
            mutation: num(self, "solver_mrt", 0.075),

            maxit: num(self, "solver_nod", 8000.0) as i32,
            limits: show.map(|_| {
                (
                    num(self, "solver_nod", 1000.0) as i32,
                    num(self, "solver_mip", 1000.0) as i32,
                )
            }),
        };
        let mut ev = |call: crate::solver_evo::Call<'_>| -> Result<Vec<f64>, OmError> {
            match call {
                crate::solver_evo::Call::Eval(x) => {
                    let p = self.sol_probe(c, m, x)?;
                    let mut fv = Vec::new();
                    for (con, lhs) in m.cons.iter().zip(&p.lhs) {
                        if con.rel <= 3 && !inside(&con.lhs) {
                            fv.extend_from_slice(lhs);
                        }
                    }
                    fv.push(p.obj);
                    Ok(fv)
                }
                crate::solver_evo::Call::Show(reason) => {
                    let a = Args {
                        values: vec![
                            show.map(|s| Variant::Bstr(s.to_string())),
                            Some(Variant::I2(reason as i16)),
                        ],
                        put: None,
                        lcid: 0,
                    };

                    let r = self.app_run(&a)?;
                    Ok(vec![crate::solver::number(&r).unwrap_or(0.0)])
                }
            }
        };
        let p = crate::solver_evo::Prob {
            lo,
            hi,
            x0: x0.clone(),
            max: m.typ == 1,
            rlo,
            rhi,
            ty,
            groups,
        };
        let out = crate::solver_evo::evo(p, &o, &mut ev)?;
        Ok(Some((out.code, Some(out.x))))
    }

    fn sol_model(&self, c: &Ctx) -> Option<Model> {
        let adj = parse_ref(&self.sol_text(c, "solver_adj")?, &c.sname)?;
        let opt = self
            .sol_text(c, "solver_opt")
            .and_then(|t| parse_ref(&t, &c.sname))
            .map(|r| CellAddr::new(c.sheet, r.r0, r.c0));
        let typ = self.sol_num(c, "solver_typ").unwrap_or(1.0) as i32;
        let val = self.sol_num(c, "solver_val").unwrap_or(0.0);
        let num = self.sol_num(c, "solver_num").unwrap_or(0.0) as i64;
        let mut cons = Vec::new();
        for i in 1..=num {
            let lhs = parse_ref(&self.sol_text(c, &format!("solver_lhs{i}"))?, &c.sname)?;
            let rel = self.sol_num(c, &format!("solver_rel{i}"))? as i32;
            let r = self.sol_text(c, &format!("solver_rhs{i}"))?;
            let rhs = match r.trim().parse::<f64>() {
                Ok(n) => Rhs::Num(n),
                Err(_) => Rhs::Text(r),
            };
            cons.push(Con { lhs, rel, rhs });
        }
        let nonneg = self.sol_num(c, "solver_neg") != Some(2.0);
        Some(Model {
            adj,
            opt,
            typ,
            val,
            cons,
            nonneg,
            eng: self.sol_num(c, "solver_eng").unwrap_or(2.0) as i32,
        })
    }

    fn sol_probe(&mut self, c: &Ctx, m: &Model, x: &[f64]) -> Result<Probe, OmError> {
        let wb = &mut self.book_mut(c.book)?.wb;

        let mode = wb.settings().mode;
        wb.set_mode(xlcalc::CalcMode::Manual);
        let mut k = 0;
        for r in m.adj.r0..m.adj.r0 + m.adj.rows {
            for col in m.adj.c0..m.adj.c0 + m.adj.cols {
                wb.set_value(CellAddr::new(c.sheet, r, col), Value::Number(x[k]));
                k += 1;
            }
        }
        wb.recalc();
        wb.set_mode(mode);
        let wb = &self.book(c.book)?.wb;
        let num = |v: Value| match v {
            Value::Number(n) => Ok(n),
            Value::Bool(b) => Ok(f64::from(u8::from(b))),
            Value::Blank => Ok(0.0),
            _ => Err(OmError::app_defined()),
        };
        let cells = |r: &Ref| -> Result<Vec<f64>, OmError> {
            let mut out = Vec::new();
            for row in r.r0..r.r0 + r.rows {
                for col in r.c0..r.c0 + r.cols {
                    out.push(num(wb.value(CellAddr::new(c.sheet, row, col)))?);
                }
            }
            Ok(out)
        };
        let obj = match m.opt {
            Some(a) => num(wb.value(a))?,
            None => 0.0,
        };
        let (mut lhs, mut rhs) = (Vec::new(), Vec::new());
        for con in &m.cons {
            lhs.push(cells(&con.lhs)?);
            if con.rel >= 4 {
                rhs.push(Vec::new());
                continue;
            }
            rhs.push(match &con.rhs {
                Rhs::Num(n) => vec![*n],
                Rhs::Text(t) => match parse_ref(t, &c.sname) {
                    Some(r) => cells(&r)?,
                    None => {
                        let a1 = CellAddr::new(c.sheet, 0, 0);
                        vec![num(wb.cfv_eval(t, a1, a1))?]
                    }
                },
            });
        }
        Ok(Probe { obj, lhs, rhs })
    }

    fn sol_set_cells(
        &mut self,
        book: crate::BookId,
        cells: &[(CellAddr, Value)],
    ) -> Result<(), OmError> {
        let wb = &mut self.book_mut(book)?.wb;
        let mode = wb.settings().mode;
        wb.set_mode(xlcalc::CalcMode::Manual);
        for (a, v) in cells {
            wb.set_value(*a, v.clone());
        }
        wb.recalc();
        wb.set_mode(mode);
        Ok(())
    }

    pub(crate) fn sol_solve(&mut self, c: &Ctx, a: &Args) -> Result<Variant, OmError> {
        let Some(m) = self.sol_model(c) else {
            self.book_mut(c.book)?.wb.set_mode(xlcalc::CalcMode::Manual);
            return int(13);
        };
        if m.eng == 3 && !m.bounded() {
            self.solver_last = Some(LastSolve {
                book: c.book,
                cells: Vec::new(),
                answer: 18,
            });
            return int(18);
        }
        let n = (m.adj.rows * m.adj.cols) as usize;
        let mut addrs = Vec::new();
        for r in m.adj.r0..m.adj.r0 + m.adj.rows {
            for col in m.adj.c0..m.adj.c0 + m.adj.cols {
                addrs.push(CellAddr::new(c.sheet, r, col));
            }
        }
        let old: Vec<(CellAddr, Value)> = {
            let wb = &self.book(c.book)?.wb;
            addrs.iter().map(|a| (*a, wb.value(*a))).collect()
        };
        let show = match a.get(2) {
            Some(Variant::Bstr(s)) => Some(s.clone()),
            _ => None,
        };
        let result = self.sol_solve_inner(c, &m, n, show.as_deref());

        let (code, x) = match result {
            Ok(r) => r,
            Err(e) => {
                self.sol_set_cells(c.book, &old)?;
                return Err(e);
            }
        };
        if x.is_none() {
            self.sol_set_cells(c.book, &old)?;
        }
        if let Some(x) = x {
            let new: Vec<(CellAddr, Value)> = addrs
                .iter()
                .zip(&x)
                .map(|(a, v)| (*a, Value::Number(*v)))
                .collect();
            self.sol_set_cells(c.book, &new)?;
        }
        self.solver_last = Some(LastSolve {
            book: c.book,
            cells: old,
            answer: code,
        });
        int(code)
    }

    fn sol_solve_inner(
        &mut self,
        c: &Ctx,
        m: &Model,
        n: usize,
        show: Option<&str>,
    ) -> Result<SolAnswer, OmError> {
        if m.eng == 1 && m.cons.iter().all(|k| k.rel != 6) {
            return self.sol_grg(c, m, n);
        }
        if m.eng == 3 && m.typ != 3 {
            if let Some(r) = self.sol_evo(c, m, n, show)? {
                return Ok(r);
            }
        }

        let start: Vec<f64> = {
            let wb = &self.book(c.book)?.wb;
            let mut v = Vec::with_capacity(n);
            for r in m.adj.r0..m.adj.r0 + m.adj.rows {
                for col in m.adj.c0..m.adj.c0 + m.adj.cols {
                    v.push(match wb.value(CellAddr::new(c.sheet, r, col)) {
                        Value::Number(x) => x,
                        _ => 0.0,
                    });
                }
            }
            v
        };
        let tol = self.sol_num(c, "solver_tol").unwrap_or(0.0);
        let mut pts = vec![vec![0.0; n]];
        for j in 0..n {
            let mut e = vec![0.0; n];
            e[j] = 1.0;
            pts.push(e);
        }
        let mut probes = Vec::new();
        for p in &pts {
            probes.push(self.sol_probe(c, m, p)?);
        }
        let p0 = &probes[0];
        let coef = |f: &dyn Fn(&Probe) -> f64| -> (Vec<f64>, f64) {
            let c0 = f(p0);
            ((0..n).map(|j| f(&probes[j + 1]) - c0).collect(), c0)
        };
        let (obj, obj0) = coef(&|p| p.obj);
        let mut rows = Vec::new();
        let mut ints = Vec::new();
        let mut groups: Vec<Vec<usize>> = Vec::new();
        for (ci, con) in m.cons.iter().enumerate() {
            if con.rel >= 4 {
                let mut group = Vec::new();
                for r in con.lhs.r0..con.lhs.r0 + con.lhs.rows {
                    for col in con.lhs.c0..con.lhs.c0 + con.lhs.cols {
                        let j = ((r - m.adj.r0) * m.adj.cols + (col - m.adj.c0)) as usize;
                        if con.rel == 6 {
                            group.push(j);
                        } else {
                            ints.push((j, con.rel == 5));
                        }
                    }
                }
                if con.rel == 6 {
                    groups.push(group);
                }
                continue;
            }
            for e in 0..probes[0].lhs[ci].len() {
                let rhs_at = |p: &Probe| {
                    let r = &p.rhs[ci];
                    r[if r.len() == 1 { 0 } else { e }]
                };
                let (a, a0) = coef(&|p| p.lhs[ci][e] - rhs_at(p));
                let rel = match con.rel {
                    1 => Rel::Le,
                    2 => Rel::Eq,
                    _ => Rel::Ge,
                };
                rows.push(Row {
                    coef: a,
                    rel,
                    rhs: -a0,
                });
            }
        }
        let mut lp = Lp {
            n,
            obj: obj.clone(),
            maximize: m.typ == 1,
            rows,
            nonneg: m.nonneg,
        };
        if m.typ == 3 {
            lp.rows.push(Row {
                coef: obj,
                rel: Rel::Eq,
                rhs: m.val - obj0,
            });
            lp.obj = vec![0.0; n];
            lp.maximize = true;
        }
        for &(j, bin) in &ints {
            if bin {
                let mut co = vec![0.0; n];
                co[j] = 1.0;
                lp.rows.push(Row {
                    coef: co,
                    rel: Rel::Le,
                    rhs: 1.0,
                });
            }
        }

        let test: Vec<f64> = (0..n).map(|j| 1.5 + j as f64).collect();
        let pt = self.sol_probe(c, m, &test)?;
        let close = |a: f64, b: f64| (a - b).abs() <= 1e-9 * (1.0 + a.abs().max(b.abs()));
        let affine = |f: &dyn Fn(&Probe) -> f64| {
            let (co, c0) = coef(f);
            c0 + co.iter().zip(&test).map(|(a, b)| a * b).sum::<f64>()
        };
        let mut linear = close(affine(&|p| p.obj), pt.obj);
        for (ci, con) in m.cons.iter().enumerate() {
            if con.rel >= 4 {
                continue;
            }
            for e in 0..pt.lhs[ci].len() {
                let r = |p: &Probe| {
                    let r = &p.rhs[ci];
                    r[if r.len() == 1 { 0 } else { e }]
                };
                linear &= close(affine(&|p| p.lhs[ci][e] - r(p)), pt.lhs[ci][e] - r(&pt));
            }
        }
        if !linear {
            return Ok((7, None));
        }
        if xlcalc::solver::bounds_conflict(&lp) {
            return Ok((20, None));
        }
        let int_vars: Vec<usize> = {
            let mut v: Vec<usize> = ints.iter().map(|x| x.0).collect();
            v.sort_unstable();
            v.dedup();
            v
        };

        let mut fixings: Vec<Vec<(usize, f64)>> = vec![Vec::new()];
        for g in &groups {
            if g.len() > 7 {
                return Err(OmError::app_defined());
            }
            let mut next = Vec::new();
            for f in &fixings {
                for perm in permutations(g.len()) {
                    let mut f2 = f.clone();
                    f2.extend(g.iter().zip(&perm).map(|(&j, &v)| (j, v as f64)));
                    next.push(f2);
                }
            }
            fixings = next;
        }
        let mut best: Option<(f64, Vec<f64>)> = None;
        let mut gap_stop = false;
        for fix in &fixings {
            let mut node = lp.clone();
            for &(j, v) in fix {
                let mut co = vec![0.0; n];
                co[j] = 1.0;
                node.rows.push(Row {
                    coef: co,
                    rel: Rel::Eq,
                    rhs: v,
                });
            }
            match branch_and_bound(&node, obj0, &int_vars, n, groups.is_empty(), tol, &start) {
                Bnb::Unbounded => return Ok((4, None)),
                Bnb::Infeasible(x) => {
                    if groups.is_empty() {
                        return Ok((5, Some(x)));
                    }
                }
                Bnb::Done(b, stop) => {
                    gap_stop |= stop;
                    if let Some((v, x)) = b {
                        let better = match &best {
                            None => true,
                            Some((bv, _)) => {
                                if lp.maximize {
                                    v > *bv
                                } else {
                                    v < *bv
                                }
                            }
                        };
                        if better {
                            best = Some((v, x));
                        }
                    }
                }
            }
        }
        Ok(match best {
            Some((_, x)) => (if gap_stop { 14 } else { 0 }, Some(x)),
            None => (5, None),
        })
    }
    pub(crate) fn sol_finish(&mut self, c: &Ctx, a: &Args) -> Result<Variant, OmError> {
        if self.sol_num(c, "solver_num").is_none() || self.solver_last.is_none() {
            return Ok(value_error());
        }
        let keep = a
            .get(1)
            .filter(|v| !missing(Some(v)))
            .and_then(number)
            .unwrap_or(1.0);
        if !(1.0..=3.0).contains(&keep) {
            return int(1);
        }
        if let Some(last) = self.solver_last.take() {
            if keep == 2.0 && last.answer <= 14 {
                self.sol_set_cells(last.book, &last.cells)?;
            }
        }
        int(0)
    }
}

enum Bnb {
    Unbounded,
    Infeasible(Vec<f64>),

    Done(Option<(f64, Vec<f64>)>, bool),
}

const BB_EPS: f64 = 1e-6;

struct BbNode {
    state: u8,
    kids: [Option<usize>; 2],
    parent: Option<usize>,

    obj: f64,
    var: usize,
    xval: f64,

    rows: Vec<Row>,
}

struct PseudoCosts {
    down: Vec<f64>,
    up: Vec<f64>,
    count: Vec<u32>,
}

fn bb_choose(x: &mut [f64], int_vars: &[usize], pc: &PseudoCosts) -> Option<usize> {
    let mut best = None;
    let mut score = -1.0;
    for &j in int_vars {
        let f = x[j] - x[j].floor();
        let (d, r) = if f > 0.5 {
            (1.0 - f, x[j].floor() + 1.0)
        } else {
            (f, x[j].floor())
        };
        if BB_EPS > d {
            x[j] = r;
            continue;
        }
        if pc.count[j] == 0 {
            return Some(j);
        }
        let a = f * pc.down[j];
        let b = (1.0 - f) * pc.up[j];
        let s = 2.0 * a.min(b) + a.max(b);
        if s > score {
            best = Some(j);
            score = s;
        }
    }
    best
}

fn bb_depth(t: &[BbNode], mut i: usize) -> usize {
    let mut d = 0;
    while let Some(p) = t[i].parent {
        d += 1;
        i = p;
    }
    d
}

fn bb_select(t: &[BbNode], i: usize, best_first: bool) -> Option<usize> {
    match t[i].state {
        0 => Some(i),
        1 => {
            let a = t[i].kids[0].and_then(|k| bb_select(t, k, best_first));
            let b = t[i].kids[1].and_then(|k| bb_select(t, k, best_first));
            match (a, b) {
                (None, b) => b,
                (a, None) => a,
                (Some(a), Some(b)) => {
                    if !best_first {
                        let (da, db) = (bb_depth(t, a), bb_depth(t, b));
                        if da != db {
                            return Some(if da > db { a } else { b });
                        }
                    }
                    Some(if t[b].obj >= t[a].obj { a } else { b })
                }
            }
        }
        _ => None,
    }
}

fn branch_and_bound(
    lp: &Lp,
    obj0: f64,
    int_vars: &[usize],
    n: usize,
    report: bool,
    tol: f64,
    start: &[f64],
) -> Bnb {
    let sign = if lp.maximize { -1.0 } else { 1.0 };
    let value = |x: &[f64]| obj0 + lp.obj.iter().zip(x).map(|(a, b)| a * b).sum::<f64>();
    let with = |rows: &[Row]| {
        let mut p = lp.clone();
        p.rows.extend(rows.iter().cloned());
        p
    };
    let mut pc = PseudoCosts {
        down: vec![0.0; n],
        up: vec![0.0; n],
        count: vec![0; n],
    };

    let mut inc: Option<(f64, Vec<f64>, Option<usize>)> = None;
    let start_ok = start.iter().enumerate().all(|(j, &v)| {
        let f = v - v.floor();
        let d = if f > 0.5 { 1.0 - f } else { f };
        !(lp.nonneg && v < -BB_EPS) && !(int_vars.contains(&j) && d > BB_EPS)
    }) && lp.rows.iter().all(|r| {
        let g: f64 = r.coef.iter().zip(start).map(|(a, b)| a * b).sum();
        match r.rel {
            Rel::Le => g - BB_EPS <= r.rhs,
            Rel::Ge => r.rhs <= g + BB_EPS,
            Rel::Eq => g - BB_EPS <= r.rhs && r.rhs <= g + BB_EPS,
        }
    });
    if start_ok {
        let mut x = start.to_vec();
        if bb_choose(&mut x, int_vars, &pc).is_none() {
            inc = Some((sign * value(&x), x, None));
        }
    }
    let mut x = match solve(lp) {
        LpStatus::Optimal(x) => x,
        LpStatus::Unbounded if report => return Bnb::Unbounded,
        LpStatus::Infeasible(x) if report => return Bnb::Infeasible(x),
        _ => return Bnb::Done(None, false),
    };
    let Some(var) = bb_choose(&mut x, int_vars, &pc) else {
        return Bnb::Done(Some((value(&x), x)), false);
    };
    let mut t = vec![BbNode {
        state: 0,
        kids: [None, None],
        parent: None,
        obj: sign * value(&x),
        var,
        xval: x[var],
        rows: Vec::new(),
    }];
    let mut bound = t[0].obj;
    let mut best_first = false;
    let mut gap_stop = false;
    let mut sel = 0;
    let (mut down_obj, mut up_obj) = (0.0, 0.0);
    loop {
        t[sel].state = 1;
        let (var, xval, pobj) = (t[sel].var, t[sel].xval, t[sel].obj);
        let up_first = if pc.count[var] != 0 {
            pc.down[var] <= pc.up[var]
        } else {
            xval - xval.floor() < 0.5
        };
        let mut new_inc = false;
        for up in [up_first, !up_first] {
            let mut co = vec![0.0; n];
            co[var] = 1.0;
            let mut rows = t[sel].rows.clone();
            rows.push(if up {
                Row {
                    coef: co,
                    rel: Rel::Ge,
                    rhs: xval.ceil(),
                }
            } else {
                Row {
                    coef: co,
                    rel: Rel::Le,
                    rhs: xval.floor(),
                }
            });

            let mut cobj = pobj;
            if let LpStatus::Optimal(mut cx) = solve(&with(&rows)) {
                cobj = sign * value(&cx);
                if pobj > cobj || (cobj - pobj).abs() < BB_EPS {
                    cobj = pobj;
                }
                let cvar = bb_choose(&mut cx, int_vars, &pc);
                let pruned = inc.as_ref().is_some_and(|(v, _, _)| cobj > *v);
                if !pruned {
                    let k = t.len();
                    t.push(BbNode {
                        state: if cvar.is_some() { 0 } else { 2 },
                        kids: [None, None],
                        parent: Some(sel),
                        obj: cobj,
                        var: cvar.unwrap_or(0),
                        xval: cvar.map_or(0.0, |v| cx[v]),
                        rows,
                    });
                    t[sel].kids[usize::from(up)] = Some(k);
                    if cvar.is_none() {
                        if let Some((_, _, Some(old))) = inc {
                            t[old].state = 3;
                        }
                        inc = Some((cobj, cx, Some(k)));
                        new_inc = true;
                    }
                }
            }
            if up {
                up_obj = cobj;
            } else {
                down_obj = cobj;
            }
        }
        if new_inc {
            let iv = inc.as_ref().map_or(f64::INFINITY, |v| v.0);
            for node in t.iter_mut() {
                if node.state != 3 && node.obj > iv {
                    node.state = 3;
                }
            }
        }
        let f = xval - xval.floor();
        let c = xval.ceil() - xval;
        if f != 0.0 {
            pc.down[var] += (down_obj - pobj).abs() / f;
        }
        if c != 0.0 {
            pc.up[var] += (up_obj - pobj).abs() / c;
        }
        pc.count[var] += 1;
        let live = t.iter().filter(|v| v.state != 3).count();
        best_first = if best_first {
            live < 1024
        } else {
            inc.is_some() && live < 512
        };
        let next = loop {
            let Some(s) = bb_select(&t, 0, best_first) else {
                break None;
            };
            if inc.as_ref().is_some_and(|(v, _, _)| t[s].obj > *v) {
                t[s].state = 3;
                continue;
            }
            break Some(s);
        };
        let Some(s) = next else {
            break;
        };
        if best_first {
            bound = t[s].obj;
        }
        if let Some((v, _, _)) = &inc {
            if tol >= (v - bound).abs() / (bound.abs() + 1.0) {
                gap_stop = true;
                break;
            }
        }
        sel = s;
    }
    Bnb::Done(inc.map(|(v, x, _)| (sign * v, x)), gap_stop)
}

fn permutations(k: usize) -> Vec<Vec<usize>> {
    fn go(cur: &mut Vec<usize>, used: &mut Vec<bool>, k: usize, out: &mut Vec<Vec<usize>>) {
        if cur.len() == k {
            out.push(cur.clone());
            return;
        }
        for v in 1..=k {
            if !used[v - 1] {
                used[v - 1] = true;
                cur.push(v);
                go(cur, used, k, out);
                cur.pop();
                used[v - 1] = false;
            }
        }
    }
    let mut out = Vec::new();
    go(&mut Vec::new(), &mut vec![false; k], k, &mut out);
    out
}
