use crate::address::{CellAddr, Rect, SheetId};
use crate::calc::{self, RecalcReport};
use crate::value::Value;
use crate::workbook::{CalcMode, EditError, Entry, Workbook};

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum SubFunc {
    Sum,
    Count,
    Average,
    Max,
    Min,
    Product,
    CountNums,
    StDev,
    StDevP,
    Var,
    VarP,
}

impl SubFunc {
    fn code_name(self) -> (u32, &'static str) {
        match self {
            SubFunc::Sum => (9, "Total"),
            SubFunc::Count => (3, "Count"),
            SubFunc::Average => (1, "Average"),
            SubFunc::Max => (4, "Max"),
            SubFunc::Min => (5, "Min"),
            SubFunc::Product => (6, "Product"),
            SubFunc::CountNums => (2, "Count"),
            SubFunc::StDev => (7, "StdDev"),
            SubFunc::StDevP => (8, "StdDevp"),
            SubFunc::Var => (10, "Var"),
            SubFunc::VarP => (11, "Varp"),
        }
    }

    pub fn parse(s: &str) -> Option<SubFunc> {
        Some(match s {
            "sum" => SubFunc::Sum,
            "count" => SubFunc::Count,
            "average" => SubFunc::Average,
            "max" => SubFunc::Max,
            "min" => SubFunc::Min,
            "product" => SubFunc::Product,
            "countnums" => SubFunc::CountNums,
            "stdev" => SubFunc::StDev,
            "stdevp" => SubFunc::StDevP,
            "var" => SubFunc::Var,
            "varp" => SubFunc::VarP,
            _ => return None,
        })
    }
}

#[derive(Clone, Debug)]
pub struct SubtotalSpec {
    pub group: u32,
    pub function: SubFunc,

    pub totals: Vec<u32>,

    pub replace: bool,

    pub below: bool,
}

fn generic_error() -> EditError {
    EditError::Compile("Application-defined or object-defined error".into())
}

fn col_name(c: u32) -> String {
    crate::address::col_to_letters(c)
}

impl Workbook {
    fn hdr_row_gappy(&self, sheet: SheetId, r: u32, c0: u32, c1: u32) -> bool {
        let filled: Vec<bool> = (c0..=c1)
            .map(|c| !matches!(self.value(CellAddr::new(sheet, r, c)), Value::Blank))
            .collect();
        match (
            filled.iter().position(|&f| f),
            filled.iter().rposition(|&f| f),
        ) {
            (Some(a), Some(b)) => filled[a..=b].iter().any(|&f| !f),
            _ => true,
        }
    }

    fn hdr_rows_identical(&self, sheet: SheetId, r: u32, c0: u32, c1: u32) -> bool {
        fn kind(v: &Value) -> u8 {
            match v {
                Value::Blank => 0,
                Value::Number(_) => 1,
                Value::Text(_) => 2,
                Value::Bool(_) => 3,
                Value::Error(_) => 4,
                _ => 5,
            }
        }
        let cased = |ch: char| ch.is_uppercase() || ch.is_lowercase();
        for c in c0..=c1 {
            let (a, b) = (CellAddr::new(sheet, r, c), CellAddr::new(sheet, r + 1, c));
            let (va, vb) = (self.value(a), self.value(b));
            let (ka, kb) = (kind(&va), kind(&vb));
            if ka == 0 || kb == 0 {
                continue;
            }
            if ka != kb || self.number_format(a) != self.number_format(b) {
                return false;
            }
            if let (Value::Text(ta), Value::Text(tb)) = (&va, &vb) {
                let caps = ta.chars().any(cased)
                    && ta.chars().filter(|&ch| cased(ch)).all(char::is_uppercase);
                if caps && tb.chars().any(|ch| cased(ch) && !ch.is_uppercase()) {
                    return false;
                }
            }
        }
        true
    }

    fn subtotal_header(&self, sheet: SheetId, rect: Rect, autosel: bool) -> (u32, u32) {
        let (r0, c0, c1) = (rect.r0, rect.c0, rect.c1);
        let differs = |r: u32| {
            !self.hdr_row_gappy(sheet, r, c0, c1) && !self.hdr_rows_identical(sheet, r, c0, c1)
        };
        if differs(r0) {
            (r0, r0 + 1)
        } else if r0 > 0 && differs(r0 - 1) {
            (r0 - 1, r0)
        } else if autosel && differs(r0 + 1) {
            (r0 + 1, r0 + 2)
        } else {
            (r0, r0 + 1)
        }
    }

    fn subtotal_rows(&self, sheet: SheetId, rect: Rect) -> Vec<u32> {
        (rect.r0..=rect.r1)
            .filter(|&r| {
                (rect.c0..=rect.c1).any(|c| {
                    let a = CellAddr::new(sheet, r, c);
                    matches!(
                        self.sheet(sheet).unwrap().cells.get(r, c),
                        Some(Entry::Formula(_))
                    ) && self
                        .formula_text(a)
                        .is_some_and(|t| t.to_ascii_uppercase().contains("SUBTOTAL("))
                })
            })
            .collect()
    }

    fn subtotal_guard(&self, sheet: SheetId, rect: Rect) -> Result<(), EditError> {
        crate::structure::range_too_large(&rect)?;
        if self.filter_mode(sheet) {
            return Err(EditError::OutOfScope("Subtotal in filter mode"));
        }
        if self.merge_intersects(sheet, &rect) {
            return Err(EditError::OutOfScope("Subtotal over merged cells"));
        }
        Ok(())
    }

    pub fn remove_subtotal(
        &mut self,
        sheet: SheetId,
        rect: Rect,
    ) -> Result<RecalcReport, EditError> {
        self.sheet(sheet).ok_or(EditError::NoSuchSheet)?;
        let rect = if rect.is_cell() {
            self.current_region(sheet, rect)
        } else {
            rect
        };
        self.subtotal_guard(sheet, rect)?;

        self.set_subtotal_levels(sheet, rect.r0, rect.r1, None);
        let rows = self.subtotal_rows(sheet, rect);
        let mut report = RecalcReport::default();
        let mode = self.settings.mode;
        self.settings.mode = CalcMode::Manual;
        let mut res = Ok(());
        for &r in rows.iter().rev() {
            match self.delete_rows(sheet, r, 1) {
                Ok(rep) => report.changed.extend(rep.changed),
                Err(e) => {
                    res = Err(e);
                    break;
                }
            }
        }
        self.settings.mode = mode;
        res?;
        let r = calc::recalc(self);
        report.evaluations += r.evaluations;
        report.circular = r.circular;
        report.changed.extend(r.changed);
        Ok(calc::finish_report(report))
    }

    pub fn subtotal(
        &mut self,
        sheet: SheetId,
        rect: Rect,
        spec: &SubtotalSpec,
    ) -> Result<RecalcReport, EditError> {
        self.sheet(sheet).ok_or(EditError::NoSuchSheet)?;
        let autosel = rect.is_cell();
        let mut rect = if autosel {
            self.current_region(sheet, rect)
        } else {
            rect
        };
        self.subtotal_guard(sheet, rect)?;
        if !spec.replace {
            return Err(EditError::OutOfScope(
                "Subtotal without replacing the current subtotals",
            ));
        }
        let cols = rect.cols();
        if spec.group < 1
            || spec.group > cols
            || spec.totals.is_empty()
            || spec.totals.iter().any(|&t| t < 1 || t > cols)
        {
            return Err(generic_error());
        }
        if spec.totals.contains(&spec.group) {
            return Err(EditError::OutOfScope("Subtotal totalling the group column"));
        }
        for (_, b) in self.array_blocks(sheet) {
            if b.intersects(&rect) {
                return Err(crate::cse::part_of_array());
            }
        }
        if !self
            .sheet(sheet)
            .unwrap()
            .spills
            .intersecting(&rect)
            .is_empty()
        {
            return Err(crate::cse::part_of_array());
        }
        if self
            .tables
            .iter()
            .any(|t| t.sheet == sheet && t.rect.intersects(&rect))
        {
            return Err(generic_error());
        }
        let mut report = RecalcReport::default();
        let mode = self.settings.mode;
        self.settings.mode = CalcMode::Manual;
        let r = self.subtotal_rows_apply(sheet, &mut rect, spec, autosel, &mut report);
        self.settings.mode = mode;
        let fresh = r?;

        let r = if mode == CalcMode::Manual {
            calc::recalc_only(self, &fresh)
        } else {
            calc::recalc(self)
        };
        report.evaluations += r.evaluations;
        report.circular = r.circular;
        report.changed.extend(r.changed);
        Ok(calc::finish_report(report))
    }

    fn subtotal_rows_apply(
        &mut self,
        sheet: SheetId,
        rect: &mut Rect,
        spec: &SubtotalSpec,
        autosel: bool,
        report: &mut RecalcReport,
    ) -> Result<Vec<CellAddr>, EditError> {
        if rect.rows() == 1 {
            return Err(generic_error());
        }
        let (list_r0, mut first) = self.subtotal_header(sheet, *rect, autosel);
        rect.r0 = list_r0;

        self.set_subtotal_levels(sheet, rect.r0, rect.r1, None);
        let old = self.subtotal_rows(sheet, *rect);
        for &r in old.iter().rev() {
            let rep = self.delete_rows(sheet, r, 1)?;
            report.changed.extend(rep.changed);
        }
        rect.r1 -= old.len() as u32;
        first -= old.iter().filter(|&&r| r < first).count() as u32;
        if rect.rows() == 1 {
            return Err(generic_error());
        }
        let c0 = rect.c0;
        let last = rect.r1;
        if first > last {
            return Ok(Vec::new());
        }
        let gcol = c0 + spec.group - 1;
        let key = |wb: &Workbook, r: u32| -> String {
            wb.display_text(CellAddr::new(sheet, r, gcol))
                .trim()
                .to_lowercase()
        };

        struct Run {
            a: u32,
            b: u32,
            label: String,
        }
        let mut runs: Vec<Run> = Vec::new();
        let mut cur: Option<(u32, u32, String)> = None;
        for r in first..=last {
            let k = key(self, r);
            match &mut cur {
                Some((_, b, ck)) if *ck == k => *b = r,
                _ => {
                    if let Some((a, b, ck)) = cur.take() {
                        if !ck.is_empty() {
                            let label = self.display_text(CellAddr::new(sheet, a, gcol));
                            runs.push(Run { a, b, label });
                        }
                    }
                    cur = Some((r, r, k));
                }
            }
        }
        if let Some((a, b, ck)) = cur.take() {
            if !ck.is_empty() {
                let label = self.display_text(CellAddr::new(sheet, a, gcol));
                runs.push(Run { a, b, label });
            }
        }

        let mut at: std::collections::BTreeMap<u32, u32> = std::collections::BTreeMap::new();
        for run in &runs {
            *at.entry(if spec.below { run.b + 1 } else { run.a })
                .or_default() += 1;
        }
        *at.entry(if spec.below { last + 1 } else { first })
            .or_default() += 1;
        for (_, b) in self.array_blocks(sheet) {
            if at.keys().any(|&p| b.r0 < p && p <= b.r1) {
                return Err(crate::cse::part_of_array());
            }
        }
        let all = Rect {
            r0: 0,
            c0: 0,
            r1: u32::MAX >> 12,
            c1: u32::MAX >> 16,
        };
        for (_, b) in self.sheet(sheet).unwrap().spills.intersecting(&all) {
            if at.keys().any(|&p| b.r0 < p && p <= b.r1) {
                return Err(crate::cse::part_of_array());
            }
        }

        let shift = |r: u32| -> u32 { at.range(..=r).map(|(_, n)| n).sum::<u32>() };
        let below_shift = |r: u32| -> u32 { at.range(..r).map(|(_, n)| n).sum::<u32>() };

        let mut added = 0;
        for (&p, &n) in at.iter() {
            let rep = self.insert_rows(sheet, p + added, n)?;
            report.changed.extend(rep.changed);
            added += n;
        }
        let (code, name) = spec.function.code_name();

        let keeps_fmt = matches!(
            spec.function,
            SubFunc::Sum | SubFunc::Average | SubFunc::Max | SubFunc::Min
        );

        let fin = |r: u32| r + shift(r);
        let mut fresh = Vec::new();
        let mut put = |wb: &mut Workbook,
                       row: u32,
                       label: &str,
                       a: u32,
                       b: u32|
         -> Result<(), EditError> {
            let g = CellAddr::new(sheet, row, gcol);
            wb.set_value(g, Value::from(label));
            report.changed.push(g);
            for &t in &spec.totals {
                let col = c0 + t - 1;
                let addr = CellAddr::new(sheet, row, col);
                let text = format!(
                    "=SUBTOTAL({code},{0}{1}:{0}{2})",
                    col_name(col),
                    a + 1,
                    b + 1
                );
                let rep = wb.set_formula(addr, &text)?;
                report.changed.extend(rep.changed);
                report.changed.push(addr);
                fresh.push(addr);
                if !keeps_fmt {
                    wb.set_number_format_unrounded(sheet, Rect::new(row, col, row, col), "General");
                }
            }
            Ok(())
        };
        let (fa, fb) = (fin(first), fin(last));
        let mut run_rows = Vec::new();
        for run in &runs {
            let (a, b) = (fin(run.a), fin(run.b));

            let row = if spec.below { b + 1 } else { a - 1 };
            put(self, row, &format!("{} {}", run.label, name), a, b)?;
            run_rows.push(row);
        }
        let grand = if spec.below {
            fb + at.get(&(last + 1)).copied().unwrap_or(0)
        } else {
            first + below_shift(first)
        };
        put(self, grand, &format!("Grand {name}"), fa, fb)?;

        let lo = run_rows.iter().copied().chain([fa]).min().unwrap();
        let hi = run_rows.iter().copied().chain([fb]).max().unwrap();
        self.set_subtotal_levels(sheet, lo, hi, Some(2));
        for r in run_rows {
            self.set_subtotal_levels(sheet, r, r, Some(1));
        }
        Ok(fresh)
    }
}
