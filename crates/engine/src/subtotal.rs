use crate::address::{CellAddr, Rect, SheetId, MAX_COLS, MAX_ROWS};
use crate::calc::{self, RecalcReport};
use crate::value::Value;
use crate::workbook::Command;
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

    pub page_breaks: bool,
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

    pub(crate) fn subtotal_rows(&self, sheet: SheetId, rect: Rect) -> Vec<u32> {
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

    fn subtotal_guard(&self, rect: Rect) -> Result<(), EditError> {
        crate::structure::range_too_large(&rect)?;
        Ok(())
    }

    fn drop_total_break(&mut self, sheet: SheetId, row: u32) {
        self.delete_page_break(sheet, true, row + 1);
    }

    pub fn remove_subtotal(
        &mut self,
        sheet: SheetId,
        rect: Rect,
    ) -> Result<RecalcReport, EditError> {
        self.command(Command::Subtotal, |wb| wb.remove_subtotal_run(sheet, rect))
    }

    pub(crate) fn remove_subtotal_run(
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
        self.subtotal_guard(rect)?;
        self.note_cell_format(CellAddr::new(sheet, rect.r0, rect.c0));

        if !self.filter_mode(sheet) {
            self.set_subtotal_levels(sheet, rect.r0, rect.r1, None);
        }
        let rows = self.subtotal_rows(sheet, rect);
        let mut report = RecalcReport::default();
        let mode = self.settings.mode;
        self.settings.mode = CalcMode::Manual;
        let mut res = Ok(());
        for &r in rows.iter().rev() {
            self.drop_total_break(sheet, r);
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

        if mode == CalcMode::Manual {
            return Ok(calc::finish_report(report));
        }
        if mode == CalcMode::Automatic {
            self.dirty_rows_edited(sheet);
        }
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
        self.command(Command::Subtotal, |wb| wb.subtotal_run(sheet, rect, spec))
    }

    fn subtotal_run(
        &mut self,
        sheet: SheetId,
        rect: Rect,
        spec: &SubtotalSpec,
    ) -> Result<RecalcReport, EditError> {
        self.sheet(sheet).ok_or(EditError::NoSuchSheet)?;
        let autosel = rect.is_cell();
        let mut rect = if autosel {
            self.current_region(sheet, rect)
        } else if rect.r0 == 0 && rect.c0 == 0 && rect.r1 + 1 == MAX_ROWS && rect.c1 + 1 == MAX_COLS
        {
            self.used_range(sheet).ok_or_else(generic_error)?
        } else {
            rect
        };
        self.subtotal_guard(rect)?;
        let cols = rect.cols();
        if spec.group < 1
            || spec.group > cols
            || spec.totals.is_empty()
            || spec.totals.iter().any(|&t| t < 1 || t > cols)
        {
            return Err(generic_error());
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

        let pending = calc::pending_formulas(self);
        self.settings.mode = CalcMode::Manual;
        let r = self.subtotal_rows_apply(sheet, &mut rect, spec, autosel, &mut report);
        self.settings.mode = mode;
        let fresh = r?;

        let r = if mode == CalcMode::Manual {
            let mut r = calc::recalc_only(self, &fresh);
            let more = calc::recalc_pending(self, &pending);
            r.evaluations += more.evaluations;
            r.changed.extend(more.changed);
            self.calc.entered.clear();
            self.calc.entered_groups.clear();
            r
        } else {
            self.dirty_rows_edited(sheet);
            calc::recalc(self)
        };
        report.evaluations += r.evaluations;
        report.circular = r.circular;
        report.changed.extend(r.changed);
        Ok(calc::finish_report(report))
    }

    fn dirty_rows_edited(&mut self, sheet: SheetId) {
        let ids: Vec<crate::workbook::FormulaId> = self
            .formula_ids()
            .filter(|&f| {
                let c = self.formula(f);
                c.addr.sheet == sheet
                    || c.precedents.iter().any(|p| match p {
                        crate::graph::Prec::Cell(s, ..) | crate::graph::Prec::Range(s, _) => {
                            *s == sheet
                        }
                        crate::graph::Prec::Name(_) => false,
                    })
            })
            .collect();
        for f in ids {
            calc::mark_dirty(self, f);
        }
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

        self.note_cell_format(CellAddr::new(sheet, rect.r0, rect.c0));

        let outline = !self.filter_mode(sheet);

        if outline && spec.replace {
            self.set_subtotal_levels(sheet, rect.r0, rect.r1, None);
        }
        let mut old = self.subtotal_rows(sheet, *rect);

        let existing = if spec.replace {
            Vec::new()
        } else {
            std::mem::take(&mut old)
        };

        let has_levels = self.views.get(&sheet).is_some_and(|v| !v.levels.is_empty());
        let below = if existing.is_empty() && !(has_levels && self.filter_mode(sheet)) {
            spec.below
        } else {
            !self.outline_settings(sheet).0
        };
        let spec = &SubtotalSpec {
            below,
            ..spec.clone()
        };
        {
            let (_, left, styles) = self.outline_settings(sheet);
            self.set_outline_settings(sheet, !below, left, styles);
        }
        for &r in old.iter().rev() {
            self.drop_total_break(sheet, r);
            let rep = self.delete_rows(sheet, r, 1)?;
            report.changed.extend(rep.changed);
        }
        rect.r1 -= old.len() as u32;
        first -= old.iter().filter(|&&r| r < first).count() as u32;
        if rect.rows() == 1 {
            return Err(generic_error());
        }
        let mut c0 = rect.c0;
        let mut last = rect.r1;

        let blank_row = |wb: &Workbook, r: u32| {
            (rect.c0..=rect.c1).all(|c| wb.sheet(sheet).unwrap().cells.get(r, c).is_none())
        };

        let in_filter = |wb: &Workbook, r: u32| {
            wb.views.get(&sheet).is_some_and(|v| {
                v.filters
                    .iter()
                    .any(|f| f.table.is_none() && f.rect.r0 <= r && r <= f.rect.r1)
            })
        };

        let used = self.view_used_r1(sheet);
        while last > first
            && ((blank_row(self, last) && !in_filter(self, last) && used.is_none_or(|u| last > u))
                || (existing.contains(&last) && self.row_level(sheet, last) == 1))
        {
            last -= 1;
        }
        if first > last {
            return Ok(Vec::new());
        }

        let grand_in_range = existing
            .iter()
            .any(|&r| r > last && r <= rect.r1 && self.row_level(sheet, r) == 1);

        let lcol = c0;
        if spec.totals.contains(&spec.group) {
            let rep = self.insert_cols(sheet, c0, 1)?;
            report.changed.extend(rep.changed);
            rect.c0 += 1;
            rect.c1 += 1;
            c0 += 1;
        }
        let gcol = c0 + spec.group - 1;
        let key = |wb: &Workbook, r: u32| -> String {
            let mut at = CellAddr::new(sheet, r, gcol);
            if let Some(m) = wb.merge_at(at).filter(|_| wb.merged_hidden(at)) {
                at = CellAddr::new(sheet, m.r0, m.c0);
            }
            wb.display_text(at).trim().to_lowercase()
        };

        struct Run {
            a: u32,
            b: u32,
            label: String,
        }

        let fm = self.filter_mode(sheet);
        let all_hidden =
            |wb: &Workbook, a: u32, b: u32| fm && (a..=b).all(|r| wb.row_hidden(sheet, r));
        let mut runs: Vec<Run> = Vec::new();
        let mut cur: Option<(u32, u32, String)> = None;
        for r in first..=last {
            if existing.contains(&r) {
                if let Some((a, b, ck)) = cur.take() {
                    if !ck.is_empty() && !all_hidden(self, a, b) {
                        let label = self.display_text(CellAddr::new(sheet, a, gcol));
                        runs.push(Run { a, b, label });
                    }
                }
                continue;
            }
            let k = key(self, r);

            if !spec.below && k.is_empty() {
                if let Some((a, b, ck)) = cur.take() {
                    if !ck.is_empty() && !all_hidden(self, a, b) {
                        let label = self.display_text(CellAddr::new(sheet, a, gcol));
                        runs.push(Run { a, b, label });
                    }
                }
                if !all_hidden(self, r, r) {
                    runs.push(Run {
                        a: r,
                        b: r,
                        label: String::new(),
                    });
                }
                continue;
            }
            match &mut cur {
                Some((_, b, ck)) if *ck == k => *b = r,
                _ => {
                    if let Some((a, b, ck)) = cur.take() {
                        if !ck.is_empty() && !all_hidden(self, a, b) {
                            let label = self.display_text(CellAddr::new(sheet, a, gcol));
                            runs.push(Run { a, b, label });
                        }
                    }
                    cur = Some((r, r, k));
                }
            }
        }
        if let Some((a, b, ck)) = cur.take() {
            if !ck.is_empty() && !all_hidden(self, a, b) {
                let label = self.display_text(CellAddr::new(sheet, a, gcol));
                runs.push(Run { a, b, label });
            }
        }

        let mut at: std::collections::BTreeMap<u32, u32> = std::collections::BTreeMap::new();

        let ins_at = |wb: &Workbook, run: &Run| -> u32 {
            if spec.below {
                return run.b + 1;
            }
            let lab = run.label.trim().to_lowercase();
            let mut p = run.a;
            while p > first
                && existing.contains(&(p - 1))
                && !lab.is_empty()
                && wb
                    .display_text(CellAddr::new(sheet, p - 1, gcol))
                    .trim()
                    .to_lowercase()
                    .starts_with(&lab)
            {
                p -= 1;
            }
            p
        };
        let ins_pts: Vec<u32> = runs.iter().map(|r| ins_at(self, r)).collect();
        for &p in &ins_pts {
            *at.entry(p).or_default() += 1;
        }

        let (code_n, _) = spec.function.code_name();
        let grand_dup = existing.iter().copied().find(|&r| {
            let lc = if lcol < c0 { lcol } else { gcol };
            let t = c0 + spec.totals[0] - 1;
            self.display_text(CellAddr::new(sheet, r, lc))
                .to_lowercase()
                .starts_with("grand")
                && self
                    .formula_text(CellAddr::new(sheet, r, t))
                    .is_some_and(|f| {
                        f.to_ascii_uppercase()
                            .starts_with(&format!("=SUBTOTAL({code_n},"))
                    })
        });
        if grand_dup.is_none() {
            *at.entry(if spec.below { last + 1 } else { first })
                .or_default() += 1;
        }
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

        let old_breaks = self.views.get(&sheet).map(|v| v.hbreaks.clone());
        let mut added = 0;
        for (&p, &n) in at.iter() {
            let rep = self.insert_rows_raw(sheet, p + added, n)?;
            report.changed.extend(rep.changed);
            added += n;
        }

        if let (false, Some(ob)) = (spec.below, old_breaks) {
            self.view_mut(sheet).hbreaks = ob.iter().map(|&r| r + below_shift(r)).collect();
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
            let g = CellAddr::new(sheet, row, if lcol < c0 { lcol } else { gcol });

            let hold = wb.merge_at(g).filter(|_| wb.merged_hidden(g));
            if let Some(m) = hold {
                wb.suspend_merges(sheet, &[m], true);
            }
            wb.set_value(g, Value::from(label));
            if let Some(m) = hold {
                wb.suspend_merges(sheet, &[m], false);
            }
            wb.subtotal_label_format(g);
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
                let hold = wb.merge_at(addr).filter(|_| wb.merged_hidden(addr));
                if let Some(m) = hold {
                    wb.suspend_merges(sheet, &[m], true);
                }
                let rep = wb.set_formula(addr, &text);
                if let Some(m) = hold {
                    wb.suspend_merges(sheet, &[m], false);
                }
                let rep = rep?;
                report.changed.extend(rep.changed);
                report.changed.push(addr);
                fresh.push(addr);
                if !keeps_fmt {
                    wb.set_number_format_unrounded(sheet, Rect::new(row, col, row, col), "General");
                }
            }
            Ok(())
        };
        let (fb, fa) = (
            fin(last),
            (first..=last)
                .find(|r| !existing.contains(r))
                .map_or(fin(first), fin),
        );
        let mut run_rows = Vec::new();
        for (run, &p) in runs.iter().zip(&ins_pts) {
            let (a, b) = (fin(run.a), fin(run.b));

            let row = if spec.below {
                b + 1
            } else if p < run.a {
                p + below_shift(p) + u32::from(p == first)
            } else {
                a - 1
            };
            put(self, row, &format!("{} {}", run.label, name), a, b)?;
            run_rows.push(row);
        }
        let grand = if let Some(g) = grand_dup {
            fin(g)
        } else if spec.below {
            fb + at.get(&(last + 1)).copied().unwrap_or(0)
        } else {
            first + below_shift(first)
        };

        if spec.page_breaks {
            for (run, &t) in runs.iter().zip(&run_rows) {
                if spec.below {
                    self.add_page_break(sheet, true, t + 1 + u32::from(grand == t + 1));
                } else if Some(run.a) != runs.first().map(|r| r.a) {
                    self.add_page_break(sheet, true, t);
                }
            }
        }
        let data_end = (first..=last)
            .rev()
            .find(|r| !existing.contains(r))
            .map_or(fb, fin);

        if grand_dup.is_none() {
            put(self, grand, &format!("Grand {name}"), fa, data_end)?;
        }

        let lo = run_rows.iter().copied().chain([fa]).min().unwrap();
        let hi = run_rows.iter().copied().chain([fb]).max().unwrap();
        if outline {
            let kept: std::collections::BTreeSet<u32> = existing.iter().map(|&r| fin(r)).collect();
            let lv = |wb: &Workbook, r: u32| wb.row_level(sheet, r).saturating_sub(1) as u8;
            let grand_base = lv(self, fb);
            let trailing_blank = spec.below && runs.last().is_none_or(|r| fin(r.b) < fb);
            let all_rows: std::collections::BTreeSet<u32> = run_rows.iter().copied().collect();
            let mut data_lv: Vec<(u32, u8)> = Vec::new();
            for (run, &t) in runs.iter().zip(&run_rows) {
                let (a, b) = (fin(run.a), fin(run.b));

                let nest = !(kept.is_empty() || spec.below);
                let base = if nest {
                    lv(self, b).saturating_sub(1)
                } else {
                    lv(self, b)
                };
                self.set_subtotal_levels(sheet, t, t, Some(base));
                for r in a..=b {
                    if !nest && !kept.contains(&r) && !all_rows.contains(&r) {
                        data_lv.push((r, lv(self, r) + 1));
                    }
                }
            }
            for (r, l) in data_lv {
                self.set_subtotal_levels(sheet, r, r, Some(l));
            }
            if kept.is_empty() {
                for r in lo..=hi {
                    let l = lv(self, r) + 1;
                    self.set_subtotal_levels(sheet, r, r, Some(l));
                }
                let g = if trailing_blank {
                    lv(self, fb)
                } else {
                    grand_base
                };
                self.set_subtotal_levels(sheet, grand, grand, Some(g));
            } else {
                if grand_dup.is_none() {
                    let end = if existing.contains(&last) {
                        fin(last).saturating_sub(1)
                    } else {
                        hi
                    };
                    let top = if spec.below && !grand_in_range && end >= fa {
                        for r in fa..=end {
                            let l = lv(self, r) + 1;
                            self.set_subtotal_levels(sheet, r, r, Some(l));
                        }
                        Some(lv(self, grand.saturating_sub(1)))
                    } else {
                        None
                    };
                    self.set_subtotal_levels(sheet, grand, grand, top);
                }
            }
        } else {
            let above = self.row_level(sheet, grand.saturating_sub(1));
            let lv = (above > 2).then(|| (above - 2) as u8);
            self.set_subtotal_levels(sheet, grand, grand, lv);
        }
        Ok(fresh)
    }
}
