use crate::address::{CellAddr, Rect, SheetId};
use crate::autofill::{add_weekdays_fast, date_in_month, round15, FillType};
use crate::calc::{self, RecalcReport};
use crate::datetime::ymd_from_serial;
use crate::functions::statdist::regress::fit_line;
use crate::value::Value;
use crate::workbook::{CalcMode, EditError, Entry, Op, Workbook};
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

enum GrowthCount {
    Run,
    Skip,
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
        self.data_series_body(sheet, range, spec)
    }

    fn data_series_body(
        &mut self,
        sheet: SheetId,
        range: Rect,
        spec: &SeriesSpec,
    ) -> Result<RecalcReport, EditError> {
        self.sheet(sheet).ok_or(EditError::NoSuchSheet)?;

        if self.filter_mode(sheet) {
            return Err(EditError::OutOfScope("DataSeries in filter mode"));
        }
        if self.merge_intersects(sheet, &range) {
            return Err(EditError::OutOfScope("DataSeries over merged cells"));
        }
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

        if spec.stop.is_some() && step == 0.0 && ty == SeriesType::Growth && !spec.trend {
            return Err(EditError::Compile(
                "Unable to get the DataSeries property of the Range class".into(),
            ));
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

        if !rows {
            self.fill_grow_tables(sheet, range.r0, range, &grows);
        }
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
                    if l > 0 {
                        continue;
                    }
                    let last = (0..len)
                        .rev()
                        .find(|&k| (0..nlines).any(|m| !self.series_is_blank(at(m, k))));
                    match last {
                        Some(last) if last + 1 < len => {
                            let (a, b) = (at(0, 0), at(nlines - 1, last));
                            let (z, _) = (at(nlines - 1, len - 1), 0);
                            let src = Rect::new(a.row, a.col, b.row, b.col);
                            let dst = Rect::new(a.row, a.col, z.row, z.col);
                            let r = self.autofill(sheet, src, dst, FillType::Default);
                            match r {
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
                SeriesType::Growth if spec.stop.is_some() && !spec.trend => {
                    match self.growth_stop_count(first, spec.stop.unwrap_or(0.0), step) {
                        GrowthCount::Skip => Vec::new(),
                        GrowthCount::Run => self.series_line(&at, l, len, ty, spec, step),
                    }
                }
                _ => self.series_line(&at, l, len, ty, spec, step),
            };
            let fmt = self.number_format(first).to_string();
            for (addr, v) in cells {
                let r = self.with_op(Op::NoAutoExpand, true, |this| this.set_value_exact(addr, v));
                report.changed.extend(r.changed);

                if self.number_format(addr) != fmt {
                    self.set_number_format_unrounded(sheet, Rect::cell(addr.row, addr.col), &fmt);
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

    fn growth_stop_count(&self, first: CellAddr, stop: f64, step: f64) -> GrowthCount {
        let Some(seed) = self.series_seed(first) else {
            return GrowthCount::Run;
        };
        let mp_zero = |x: f64| x.to_bits() & 0xFFFF_0000_0000_0000 == 0;
        if mp_zero(seed) {
            return GrowthCount::Skip;
        }
        let ratio = stop / seed;
        if !mp_zero(ratio) && ratio < 0.0 {
            return GrowthCount::Skip;
        }
        if step < 0.0 {
            return GrowthCount::Skip;
        }
        GrowthCount::Run
    }

    fn series_line(
        &self,
        at: &dyn Fn(u32, u32) -> CellAddr,
        l: u32,
        len: u32,
        ty: SeriesType,
        spec: &SeriesSpec,
        step: f64,
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
                        let n = (kf * step).trunc() as i64;
                        if n == 0 {
                            let wd = crate::datetime::weekday_from_serial(sys, base);
                            (base
                                + match wd {
                                    6 => 2,
                                    0 => 1,
                                    _ => 0,
                                }) as f64
                                + frac
                        } else {
                            add_weekdays_fast(sys, base, n) as f64 + frac
                        }
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
