use crate::address::SheetId;
use crate::calc::{self, RecalcReport};

use super::{CalcMode, Workbook};

#[derive(Default)]
pub(crate) struct Passes {
    pub(super) notifying: bool,

    silent: u32,

    internal: u32,

    merge: u32,
    merged: bool,
}

impl Workbook {
    pub(crate) fn recalc_point(&mut self) -> RecalcReport {
        if self.settings.mode == CalcMode::Automatic {
            calc::recalc(self)
        } else {
            RecalcReport::default()
        }
    }

    pub(crate) fn notify_cell_change(&mut self, sheet: SheetId) -> Option<RecalcReport> {
        if self.settings.mode != CalcMode::Automatic
            || self.passes.notifying
            || !self.tables.iter().any(|t| t.sheet == sheet)
        {
            return None;
        }
        self.passes.notifying = true;
        let r = calc::recalc(self);
        self.passes.notifying = false;
        Some(r)
    }

    pub(crate) fn notify_cell_changes(&mut self, sheets: &[SheetId]) {
        for &s in sheets {
            self.notify_cell_change(s);
        }
    }

    pub(crate) fn silently<R>(&mut self, f: impl FnOnce(&mut Workbook) -> R) -> R {
        self.passes.silent += 1;
        let r = f(self);
        self.passes.silent -= 1;
        r
    }

    pub(crate) fn one_pass<R>(&mut self, f: impl FnOnce(&mut Workbook) -> R) -> R {
        if self.passes.merge == 0 {
            self.passes.merged = false;
        }
        self.passes.merge += 1;
        let r = f(self);
        self.passes.merge -= 1;
        r
    }

    pub(crate) fn in_last_pass<R>(&mut self, f: impl FnOnce(&mut Workbook) -> R) -> R {
        let merged = self.passes.merged;
        self.passes.merge += 1;
        self.passes.merged = true;
        let r = f(self);
        self.passes.merge -= 1;
        self.passes.merged = merged;
        r
    }

    pub(crate) fn not_a_pass<R>(&mut self, f: impl FnOnce(&mut Workbook) -> R) -> R {
        self.passes.internal += 1;
        let r = f(self);
        self.passes.internal -= 1;
        r
    }

    pub(crate) fn bookkeeping(&self) -> bool {
        self.passes.internal > 0
    }

    pub(crate) fn note_pass(&mut self, sheets: &[SheetId]) {
        if self.passes.internal > 0 {
            return;
        }
        if self.passes.merge > 0 && self.passes.merged {
            self.notes.calculated(sheets);
            return;
        }
        self.passes.merged = self.passes.merge > 0;
        let silent = self.passes.silent > 0;
        if self.after_skipped(!self.calc.circular.is_empty()) {
            self.notes.no_after = true;
        }
        self.notes.pass(sheets, silent);
    }

    pub(crate) fn after_skipped(&self, restart_pending: bool) -> bool {
        restart_pending && !self.settings.iteration
    }
}

#[cfg(test)]
mod tests {
    use crate::address::{CellAddr, Rect};
    use crate::workbook::Notice;
    use crate::{Value, Workbook};

    #[test]
    fn a_row_resize_runs_two_passes() {
        let mut wb = Workbook::new();
        let s = wb.sheet_at(0).id;
        let a = |r, c| CellAddr::new(s, r, c);
        wb.set_iteration(true, Some(1), Some(0.001));
        wb.set_formula(a(0, 25), "=Z1+1").unwrap();
        wb.set_input(a(0, 3), "h1").unwrap();
        wb.set_input(a(0, 4), "h2").unwrap();
        wb.set_value(a(1, 3), Value::Number(1.0));
        wb.set_value(a(2, 3), Value::Number(1.0));
        wb.add_table("T1", s, Rect::new(0, 3, 2, 4)).unwrap();
        wb.take_notices();
        let Value::Number(before) = wb.value(a(0, 25)) else {
            panic!("the cycle holds a number");
        };
        wb.resize_table("T1", Rect::new(0, 3, 3, 4)).unwrap();
        assert!(matches!(wb.value(a(0, 25)), Value::Number(n) if n == before + 2.0));
        let calc = Notice::Calculated { sheets: vec![s] };
        assert_eq!(
            wb.take_notices(),
            vec![
                calc.clone(),
                Notice::AfterCalculate,
                Notice::Changed {
                    sheet: s,
                    areas: vec![Rect::new(0, 3, 3, 4)]
                },
                calc,
                Notice::AfterCalculate
            ]
        );
    }

    fn num(wb: &Workbook, a: CellAddr) -> f64 {
        match wb.value(a) {
            Value::Number(n) => n,
            v => panic!("{a:?} holds {v:?}"),
        }
    }

    #[test]
    fn a_sheet_calculation_follows_same_sheet_precedents() {
        let mut wb = Workbook::new();
        let s = wb.sheet_at(0).id;
        let a = |r, c| CellAddr::new(s, r, c);
        wb.set_mode(crate::CalcMode::Manual);
        wb.set_input(a(0, 10), "1").unwrap();

        wb.set_input(a(0, 0), "=A2+1").unwrap();
        wb.set_input(a(1, 0), "=A3*2").unwrap();
        wb.set_input(a(2, 0), "=K1").unwrap();
        wb.set_input(a(0, 1), "=INDIRECT(\"B2\")*1").unwrap();
        wb.set_input(a(1, 1), "=K1*5").unwrap();
        wb.set_input(a(0, 10), "10").unwrap();
        wb.calculate_sheet(s);
        assert_eq!(num(&wb, a(2, 0)), 10.0);
        assert_eq!(num(&wb, a(1, 0)), 20.0);
        assert_eq!(num(&wb, a(0, 0)), 21.0);
        assert_eq!(num(&wb, a(1, 1)), 50.0);
        assert_eq!(num(&wb, a(0, 1)), 50.0);
    }

    #[test]
    fn a_sheet_calculation_reads_other_sheets_stale() {
        let mut wb = Workbook::new();
        let s = wb.sheet_at(0).id;
        let t = wb.add_sheet("Sheet2").unwrap();
        let a = |r, c| CellAddr::new(s, r, c);
        let b1 = CellAddr::new(t, 0, 1);
        wb.set_input(a(1, 10), "2").unwrap();
        wb.set_input(a(1, 1), "=K2").unwrap();
        wb.set_input(b1, "=Sheet1!B2*2").unwrap();
        wb.set_input(a(0, 1), "=Sheet2!B1+1").unwrap();
        wb.set_mode(crate::CalcMode::Manual);
        wb.set_input(a(1, 10), "20").unwrap();
        wb.calculate_sheet(s);
        assert_eq!(num(&wb, a(1, 1)), 20.0);
        assert_eq!(num(&wb, b1), 4.0);
        assert_eq!(num(&wb, a(0, 1)), 5.0);
        wb.recalc();
        assert_eq!(num(&wb, b1), 40.0);
        assert_eq!(num(&wb, a(0, 1)), 41.0);
    }
}
