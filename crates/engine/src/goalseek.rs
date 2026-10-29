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
}

impl Seek<'_> {
    fn eval(&mut self, x: f64) -> f64 {
        let a = self.changing;

        let x = if self.wb.precision_as_displayed() {
            self.wb.precision_value(a, x)
        } else {
            x
        };
        self.wb
            .sheet_mut(a.sheet)
            .unwrap()
            .cells
            .set(a.row, a.col, Entry::Const(Value::Number(x)));
        calc::cell_changed(self.wb, a);
        calc::recalc_cells(self.wb);
        self.read()
    }

    fn read(&self) -> f64 {
        match self.wb.value(self.target) {
            Value::Number(f) => f,
            Value::Bool(b) => f64::from(u8::from(b)),
            _ => f64::NAN,
        }
    }
}

impl Workbook {
    pub fn goal_seek(
        &mut self,
        target: CellAddr,
        goal: f64,
        changing: CellAddr,
    ) -> Result<bool, EditError> {
        let iteration = std::mem::replace(&mut self.settings.iteration, false);
        let r = self.seek(target, goal, changing);
        let r = self.goal_seek_settle(r);
        self.settings.iteration = iteration;
        r
    }

    fn goal_seek_settle(&mut self, r: Result<bool, EditError>) -> Result<bool, EditError> {
        if r.is_ok() {
            let tables = self.settings.mode == crate::workbook::CalcMode::Manual;
            if tables {
                calc::recalc_with(self, true);
            } else {
                calc::recalc(self);
            }
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
        };

        match s.wb.value(target) {
            Value::Error(crate::XlError::NA) => return Ok(true),
            Value::Error(_) | Value::Text(_) => {
                return Err(EditError::Compile("Reference isn't valid.".into()))
            }
            _ => {}
        }

        calc::recalc_cells(s.wb);
        let f0 = s.read();

        let mut rec = Records::default();

        if !f0.is_nan() {
            rec.accept(f0, goal);
        }
        if !f0.is_nan() && converged(f0, goal, tol) {
            s.eval(x0);
            return Ok(true);
        }

        let mut err = false;
        let ddiv = |a: f64, b: f64, err: &mut bool| -> f64 {
            if b == 0.0 || b.abs() < f64::MIN_POSITIVE {
                *err = true;
                return a;
            }
            let q = x87::div(a, b);
            *err |= !q.is_finite();
            q
        };

        let (mut xb, mut fb) = (x0, f0);
        let mut d = if (x0.to_bits() >> 48) == 0 {
            0.01
        } else {
            ddiv(x0, 100.0, &mut err)
        };
        let mut saved = 0.0;
        let mut use_saved = false;
        let mut secant = false;
        let mut fails = 0u32;
        let iters = trial_count(s.wb);
        for _ in 0..iters {
            let step = if use_saved { saved } else { d };
            let x = x87::add(xb, step);
            err |= !x.is_finite();
            let ok = if std::mem::take(&mut err) {
                None
            } else {
                let f = s.eval(x);
                if f.is_nan() || !rec.accept(f, goal) {
                    None
                } else if converged(f, goal, tol) {
                    return Ok(true);
                } else {
                    let slope = ddiv(x87::sub(f, fb), x87::sub(x, xb), &mut err);
                    let mut st = if std::mem::take(&mut err) {
                        None
                    } else {
                        let st = ddiv(x87::sub(goal, f), slope, &mut err);
                        (!std::mem::take(&mut err)).then_some(st)
                    };
                    if let Some(s0) = st {
                        let c = if secant { 2.0 } else { 10.0 };
                        let t = ddiv(s0, step, &mut err).abs();
                        let dd = x87::sub(t, c);
                        if !dd.is_finite() {
                            err = true;
                        } else if dd > 0.0 && (dd.to_bits() >> 48) != 0 {
                            let mut e2 = false;
                            st = Some(x87::mul(ddiv(s0, t, &mut e2), c));
                        }
                    }
                    st.map(|st| (x, f, st))
                }
            };
            match ok {
                Some((x, f, st)) => {
                    secant = true;
                    fails = 0;
                    d = st;
                    use_saved = false;
                    (xb, fb) = (x, f);
                }
                None => {
                    if fails == 0 {
                        saved = d;
                    }
                    fails += 1;
                    if !secant && fails % 2 == 1 {
                        let v = if use_saved { &mut saved } else { &mut d };
                        if (v.to_bits() >> 48) != 0 {
                            *v = -*v;
                        }
                    } else {
                        if !secant || fails.is_multiple_of(4) {
                            use_saved = !use_saved;
                        }
                        if use_saved {
                            saved = x87::mul(saved, 2.0);
                            err |= !saved.is_finite();
                        } else {
                            d = ddiv(d, 2.0, &mut err);
                        }
                    }
                }
            }
        }
        Ok(false)
    }
}

#[derive(Default)]
struct Records {
    above: Option<f64>,
    below: Option<f64>,
}

impl Records {
    fn accept(&mut self, f: f64, goal: f64) -> bool {
        let e = x87::sub(f, goal);
        let up = e >= 0.0 || e.is_nan() || (e.to_bits() >> 48) == 0;
        let rec = if up { &mut self.above } else { &mut self.below };
        let take = match *rec {
            None => true,
            Some(r) => {
                let g = x87::sub(r, f);
                g.is_finite() && (g.to_bits() >> 48) != 0 && if up { g > 0.0 } else { g < 0.0 }
            }
        };
        if take {
            *rec = Some(f);
        }
        take
    }
}

fn converged(f: f64, goal: f64, tol: f64) -> bool {
    let v = x87::sub(x87::sub(f, goal).abs(), tol);
    v <= 0.0 || v.is_nan() || (v.to_bits() >> 48) == 0
}

fn trial_count(wb: &Workbook) -> u32 {
    wb.settings.max_iterations.max(MAX_EVALS)
}
