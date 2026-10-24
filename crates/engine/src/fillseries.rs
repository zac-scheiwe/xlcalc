use crate::address::{CellAddr, Rect, SheetId};
use crate::autofill::{add_weekdays_fast, date_in_month, round15, FillType};
use crate::calc::{self, RecalcReport};
use crate::datetime::ymd_from_serial;
use crate::functions::statdist::regress::fit_line;
use crate::value::Value;
use crate::workbook::{CalcMode, EditError, Entry, Workbook};
use crate::x87;

#[derive(Clone, Copy, PartialEq, Eq, Debug)]
pub enum SeriesType {
    Linear,
    Growth,
    Date,
    AutoFill,
}

#[derive(Clone, Copy, PartialEq, Eq, Debug)]
pub enum DateUnit {
    Day,
    Weekday,
    Month,
    Year,
}

impl SeriesType {
    pub fn parse(s: &str) -> Option<SeriesType> {
        Some(match s.to_ascii_lowercase().as_str() {
            "linear" => SeriesType::Linear,
            "growth" => SeriesType::Growth,
            "date" => SeriesType::Date,
            "autofill" => SeriesType::AutoFill,
            _ => return None,
        })
    }
}

impl DateUnit {
    pub fn parse(s: &str) -> Option<DateUnit> {
        Some(match s.to_ascii_lowercase().as_str() {
            "day" => DateUnit::Day,
            "weekday" => DateUnit::Weekday,
            "month" => DateUnit::Month,
            "year" => DateUnit::Year,
            _ => return None,
        })
    }
}

#[derive(Clone, Copy, Debug, Default)]
pub struct SeriesSpec {
    pub rows: Option<bool>,
    pub ty: Option<SeriesType>,
    pub unit: Option<DateUnit>,
    pub step: Option<f64>,
    pub stop: Option<f64>,
    pub trend: bool,
}

impl Workbook {
    fn series_seed(&self, addr: CellAddr) -> Option<f64> {
        let sh = self.sheet(addr.sheet)?;
        match sh.cells.get(addr.row, addr.col) {
            Some(Entry::Const(Value::Number(n))) => Some(*n),
            Some(Entry::Const(_)) => None,

            _ => match self.value(addr) {
                Value::Number(n) => Some(n),
                _ => None,
            },
        }
    }

    fn series_is_blank(&self, addr: CellAddr) -> bool {
        self.sheet(addr.sheet)
            .and_then(|s| s.cells.get(addr.row, addr.col))
            .is_none_or(|e| matches!(e, Entry::Const(Value::Blank)))
    }

    pub fn data_series(
        &mut self,
        sheet: SheetId,
        range: Rect,
        spec: &SeriesSpec,
    ) -> Result<RecalcReport, EditError> {
        self.sheet(sheet).ok_or(EditError::NoSuchSheet)?;
        let rows = spec.rows.unwrap_or(range.cols() >= range.rows());
        let ty = spec.ty.unwrap_or(SeriesType::Linear);
        let nlines = if rows { range.rows() } else { range.cols() };
        let len = if rows { range.cols() } else { range.rows() };
        if nlines as u64 * len as u64 > crate::structure::MAX_PASTE_CELLS {
            return Err(EditError::Compile("The series range is too large.".into()));
        }
        let at = |l: u32, k: u32| -> CellAddr {
            if rows {
                CellAddr::new(sheet, range.r0 + l, range.c0 + k)
            } else {
                CellAddr::new(sheet, range.r0 + k, range.c0 + l)
            }
        };
        let step_given = spec.step;
        let step = step_given.unwrap_or(1.0);

        let no_fill = spec.stop.is_some() && ty == SeriesType::Growth && step == 1.0;

        if spec.stop.is_some() && step == 0.0 && matches!(ty, SeriesType::Linear) {
            return Err(EditError::Compile("cannot fill that range".into()));
        }

        if ty != SeriesType::AutoFill
            && len > 1
            && (0..nlines).any(|l| self.series_seed(at(l, 0)).is_some())
        {
            let seeds = if rows {
                Rect::new(range.r0, range.c0, range.r1, range.c0)
            } else {
                Rect::new(range.r0, range.c0, range.r0, range.c1)
            };
            self.guard_merges_touch(sheet, seeds)?;

            let crossing = self.merges(sheet).into_iter().any(|m| {
                m.intersects(&range)
                    && if rows {
                        m.r0 < range.r0
                    } else {
                        m.c0 < range.c0
                    }
            });
            if crossing {
                return Err(EditError::Compile(
                    "To do this, all the merged cells need to be the same size.".into(),
                ));
            }
        }
        let mut blocks: Vec<Rect> = Vec::new();
        let mut grows: Vec<Rect> = Vec::new();
        for l in 0..nlines {
            let from = if ty == SeriesType::AutoFill {
                match (0..len).rev().find(|&k| !self.series_is_blank(at(l, k))) {
                    Some(last) if last + 1 < len => last + 1,
                    _ => continue,
                }
            } else if self.series_seed(at(l, 0)).is_some() && len > 1 {
                1
            } else {
                continue;
            };
            let (a, z) = (at(l, from), at(l, len - 1));
            let written = Rect::new(a.row, a.col, z.row, z.col);
            self.guard_merges_touch(sheet, written)?;
            self.guard_arrays_rect(sheet, written)?;
            self.data_tables_overwritten(sheet, written)?;
            if ty != SeriesType::AutoFill {
                grows.push(written);
            }
            blocks.extend(
                self.array_blocks(sheet)
                    .into_iter()
                    .map(|(_, b)| b)
                    .filter(|b| b.intersects(&written)),
            );
        }
        for b in blocks {
            self.clear_range(sheet, b)?;
        }

        self.fill_grow_tables(sheet, range, &grows);
        let mode = self.settings.mode;
        self.settings.mode = CalcMode::Manual;
        let mut report = RecalcReport::default();
        let mut failed = None;

        if ty != SeriesType::AutoFill
            && len > 1
            && (0..nlines).any(|l| self.series_seed(at(l, 0)).is_some())
        {
            let last_active = (0..nlines)
                .rev()
                .find(|&l| self.series_seed(at(l, 0)).is_some())
                .unwrap_or(0);
            for l in 0..last_active {
                if self.series_seed(at(l, 0)).is_none() {
                    let (a, z) = (at(l, 1), at(l, len - 1));
                    if let Ok(r) = self.clear_range(sheet, Rect::new(a.row, a.col, z.row, z.col)) {
                        report.changed.extend(r.changed);
                    }
                }
            }
        }
        for l in 0..nlines {
            let first = at(l, 0);
            let cells: Vec<(CellAddr, Value)> = match ty {
                SeriesType::AutoFill => {
                    let last = (0..len).rev().find(|&k| !self.series_is_blank(at(l, k)));
                    match last {
                        Some(last) if last + 1 < len => {
                            let (a, b) = (at(l, 0), at(l, last));
                            let (z, _) = (at(l, len - 1), 0);
                            let src = Rect::new(a.row, a.col, b.row, b.col);
                            let dst = Rect::new(a.row, a.col, z.row, z.col);
                            match self.autofill(sheet, src, dst, FillType::Default) {
                                Ok(r) => report.changed.extend(r.changed),
                                Err(e) => failed = Some(e),
                            }
                        }
                        _ => {}
                    }
                    if failed.is_some() {
                        break;
                    }
                    continue;
                }
                _ if no_fill => Vec::new(),
                _ => self.series_line(&at, l, len, ty, spec, step, step_given),
            };
            let fmt = self.number_format(first).to_string();
            for (addr, v) in cells {
                report.changed.extend(self.set_value_exact(addr, v).changed);
                if self.number_format(addr) != fmt {
                    report.changed.extend(
                        self.set_number_format(sheet, Rect::cell(addr.row, addr.col), &fmt)
                            .changed,
                    );
                }
            }
        }
        self.settings.mode = mode;
        if mode == CalcMode::Automatic {
            let r = calc::recalc(self);
            report.evaluations = r.evaluations;
            report.circular = r.circular;
            report.changed.extend(r.changed);
        }
        match failed {
            Some(e) => Err(e),
            None => Ok(calc::finish_report(report)),
        }
    }

    #[allow(clippy::too_many_arguments)]
    fn series_line(
        &self,
        at: &dyn Fn(u32, u32) -> CellAddr,
        l: u32,
        len: u32,
        ty: SeriesType,
        spec: &SeriesSpec,
        step: f64,
        _step_given: Option<f64>,
    ) -> Vec<(CellAddr, Value)> {
        let mut out = Vec::new();
        let growth = ty == SeriesType::Growth;
        if spec.trend && matches!(ty, SeriesType::Linear | SeriesType::Growth) {
            let mut xs = Vec::new();
            let mut ys = Vec::new();
            for k in 0..len {
                let a = at(l, k);
                if self.series_is_blank(a) {
                    continue;
                }
                match self.series_seed(a) {
                    Some(n) => {
                        xs.push((k + 1) as f64);
                        ys.push(n);
                    }
                    None => break,
                }
            }
            if xs.len() >= 2 {
                if let Some((m, b)) = fit_line(&xs, &ys, growth) {
                    let (c, r) = (x87::exp(b), x87::exp(m));
                    for k in 0..len {
                        let x = (k + 1) as f64;
                        let v = if growth {
                            x87::mul(c, crate::autofill::powi87(r, x))
                        } else {
                            x87::add(x87::mul(m, x), b)
                        };
                        out.push((at(l, k), Value::num(v)));
                    }
                }
                return out;
            }
        }
        let Some(seed) = self.series_seed(at(l, 0)) else {
            return out;
        };
        let sys = self.settings.date_system;
        let stop = spec.stop;

        let up = match ty {
            SeriesType::Growth => step.abs() > 1.0,
            _ => step >= 0.0,
        };
        let past = |v: f64| -> bool {
            match stop {
                None => false,
                Some(s) => {
                    let v = round15(v);
                    if up {
                        v > s
                    } else {
                        v < s
                    }
                }
            }
        };
        let unit = spec.unit.unwrap_or(DateUnit::Day);
        let base = seed.floor() as i64;
        let frac = seed - seed.floor();

        let (y0, m0, d0) = if base == 0 && sys == crate::datetime::DateSystem::D1900 {
            (1899, 12, 0)
        } else {
            ymd_from_serial(sys, base)
        };
        let mi0 = y0 * 12 + m0 as i64 - 1;
        for k in 1..len {
            let kf = k as f64;
            let v = match ty {
                SeriesType::Linear => seed + kf * step,

                SeriesType::Growth => x87::mul(seed, crate::autofill::powi87(step, kf)),
                _ => match unit {
                    DateUnit::Day => seed + kf * step,
                    DateUnit::Weekday => {
                        add_weekdays_fast(sys, base, (kf * step).trunc() as i64) as f64 + frac
                    }
                    DateUnit::Month | DateUnit::Year => {
                        let n = (kf * step).trunc() as i64;
                        let n = if unit == DateUnit::Year { n * 12 } else { n };
                        date_in_month(sys, mi0 + n, d0, false) as f64 + frac
                    }
                },
            };
            if past(v) {
                break;
            }
            out.push((at(l, k), Value::num(v)));
        }
        out
    }
}
