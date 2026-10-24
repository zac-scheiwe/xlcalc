use crate::address::CellAddr;
use crate::calc;
use crate::value::Value;
use crate::workbook::{EditError, Entry, Workbook};
use crate::x87;

const MAX_EVALS: u32 = 100;

struct Seek<'a> {
    wb: &'a mut Workbook,
    target: CellAddr,
    changing: CellAddr,
    goal: f64,
    evals: u32,
}

impl Seek<'_> {
    fn eval(&mut self, x: f64) -> (f64, f64) {
        self.evals += 1;
        let a = self.changing;
        self.wb
            .sheet_mut(a.sheet)
            .unwrap()
            .cells
            .set(a.row, a.col, Entry::Const(Value::Number(x)));
        calc::cell_changed(self.wb, a);
        calc::recalc_cells(self.wb);
        self.read()
    }

    fn read(&self) -> (f64, f64) {
        match self.wb.value(self.target) {
            Value::Number(f) => ((f - self.goal).abs(), f),
            _ => (f64::INFINITY, f64::NAN),
        }
    }

    fn spent(&self) -> bool {
        self.evals >= MAX_EVALS
    }
}

struct Best {
    d: f64,
    f: f64,
}

impl Best {
    fn take(&mut self, d: f64, f: f64, fp: f64, fc: f64, goal: f64) -> bool {
        let side = |f: f64| f > goal;
        if d < self.d || (d == self.d && side(f) == side(self.f) && f != self.f) {
            *self = Best { d, f };
            return true;
        }
        f.is_finite()
            && side(f) != side(self.f)
            && side(f) != side(fc)
            && (f - fp) * (goal - fp) > 0.0
    }
}

impl Workbook {
    pub fn goal_seek(
        &mut self,
        target: CellAddr,
        goal: f64,
        changing: CellAddr,
    ) -> Result<bool, EditError> {
        let r = self.seek(target, goal, changing);
        if r.is_ok() {
            calc::recalc(self);
        }
        r
    }

    fn seek(&mut self, target: CellAddr, goal: f64, changing: CellAddr) -> Result<bool, EditError> {
        let x0 = match self
            .sheet(changing.sheet)
            .ok_or(EditError::NoSuchSheet)?
            .cells
            .get(changing.row, changing.col)
        {
            None => 0.0,
            Some(Entry::Const(Value::Number(n))) => *n,
            Some(_) => return Err(EditError::Compile("Cell must contain a value.".into())),
        };
        if self.formula_at(target).is_none() {
            return Err(EditError::Compile("Cell must contain a formula.".into()));
        }
        let tol = self.settings.max_change;
        let mut s = Seek {
            wb: self,
            target,
            changing,
            goal,
            evals: 0,
        };
        let (d0, f0) = s.read();
        if d0 < tol {
            return Ok(true);
        }

        let mut best = Best { d: d0, f: f0 };
        let (mut xp, mut fp, mut xc, mut fc) = (x0, f0, x0, f0);
        let h = if x0 != 0.0 { x0 / 100.0 } else { 0.01 };

        let mut probes = vec![h, -h];
        for k in 1..64 {
            let far = h * 2f64.powi(k);
            let near = h / 2f64.powi(k);
            let (a, b) = if k % 2 == 1 { (1.0, -1.0) } else { (-1.0, 1.0) };
            probes.extend([a * far, b * far, b * near, a * near]);
        }
        let mut first = None;
        for d in probes {
            let x = x0 + d;
            let (dt, ft) = s.eval(x);
            if best.take(dt, ft, fp, fc, goal) {
                (xp, fp, xc, fc) = (x0, f0, x, ft);
                first = Some(d);
                break;
            }
            if s.spent() {
                return Ok(false);
            }
        }
        let Some(first) = first else {
            return Ok(false);
        };
        let mut cap = 10.0 * first.abs();
        loop {
            if best.d < tol {
                return Ok(true);
            }
            if s.spent() {
                return Ok(false);
            }

            let slope = x87::div(x87::sub(fc, fp), x87::sub(xc, xp));
            let mut step = x87::div(x87::sub(goal, fc), slope);
            if step.is_infinite() {
                step = cap.copysign(step);
            } else if step.abs() > cap {
                step = x87::div(step, x87::div(step.abs(), cap));
            }

            let mut n = 0u32;
            loop {
                let m = trial_multiplier(n);
                n += 1;
                let x = x87::add(xc, step * m);
                let (dt, ft) = s.eval(x);
                if best.take(dt, ft, fp, fc, goal) {
                    (xp, fp, xc, fc) = (xc, fc, x, ft);
                    cap = 2.0 * (step * m).abs();
                    break;
                }
                if s.spent() {
                    return Ok(false);
                }
            }
        }
    }
}

fn trial_multiplier(n: u32) -> f64 {
    if n < 4 {
        return 0.5f64.powi(n as i32);
    }
    let (block, i) = ((n - 4) / 4, (n - 4) % 4);
    if block % 2 == 0 {
        2f64.powi((4 * (block / 2) + 1 + i) as i32)
    } else {
        0.5f64.powi((4 * (block / 2) + 4 + i) as i32)
    }
}
