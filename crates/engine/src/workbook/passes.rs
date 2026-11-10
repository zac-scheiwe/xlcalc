use crate::address::SheetId;
use crate::calc::{self, RecalcReport};

use super::{CalcMode, Workbook};

#[derive(Default)]
pub(crate) struct Passes {
    notifying: bool,

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

        if !self.settings.iteration && !self.calc.circular.is_empty() {
            self.notes.no_after = true;
        }
        self.notes.pass(sheets, silent);
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
                Notice::Changed {
                    sheet: s,
                    areas: vec![Rect::new(0, 3, 3, 4)]
                },
                calc.clone(),
                Notice::AfterCalculate,
                calc,
                Notice::AfterCalculate
            ]
        );
    }
}
