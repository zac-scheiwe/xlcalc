use crate::address::{CellAddr, Rect, SheetId, MAX_COLS, MAX_ROWS};
use crate::calc::{self, RecalcReport};
use crate::value::Value;
use crate::workbook::Command;
use crate::workbook::{CalcMode, EditError, Entry, Op, Workbook};

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

struct AboveIn<'a> {
    rect: Rect,

    gcol: u32,

    first: u32,

    existing: &'a [u32],

    runs: &'a [(u32, u32)],

    totals: (&'a [u32], u32),

    code: u32,

    fm: bool,
}

struct AbovePlan {
    ins: Vec<u32>,

    grouped: Vec<bool>,

    breaks: Vec<Option<u32>>,

    n2: i64,

    grand: Option<(u32, bool)>,
}

impl Workbook {
    pub fn guess_list_header(&self, sheet: SheetId, rect: Rect, autosel: bool) -> Option<u32> {
        let (c0, c1) = (rect.c0, rect.c1);
        let differs = |r: u32| {
            r < rect.r1
                && !self.hdr_row_gappy(sheet, r, c0, c1)
                && !self.hdr_rows_identical(sheet, r, c0, c1)
        };
        if differs(rect.r0) {
            Some(rect.r0)
        } else if rect.r0 > 0
            && !self.hdr_row_gappy(sheet, rect.r0 - 1, c0, c1)
            && !self.hdr_rows_identical(sheet, rect.r0 - 1, c0, c1)
        {
            None
        } else if autosel && differs(rect.r0 + 1) {
            Some(rect.r0 + 1)
        } else {
            None
        }
    }

    pub fn guess_header_above(&self, sheet: SheetId, rect: Rect) -> bool {
        let (c0, c1) = (rect.c0, rect.c1);
        let heads = |r: u32| {
            !self.hdr_row_gappy(sheet, r, c0, c1) && !self.hdr_rows_identical(sheet, r, c0, c1)
        };
        let first_heads = rect.r0 < rect.r1 && heads(rect.r0);
        rect.r0 > 0 && !first_heads && heads(rect.r0 - 1)
    }

    pub(crate) fn filter_title_skip(&self, sheet: SheetId, rect: Rect) -> u32 {
        let (r0, c0, c1) = (rect.r0, rect.c0, rect.c1);
        let heads = |r: u32| {
            !self.hdr_row_gappy(sheet, r, c0, c1) && !self.hdr_rows_identical(sheet, r, c0, c1)
        };
        if r0 >= rect.r1 || r0 + 1 >= MAX_ROWS || heads(r0) || (r0 > 0 && heads(r0 - 1)) {
            return r0;
        }
        if r0 + 2 < MAX_ROWS && heads(r0 + 1) {
            r0 + 1
        } else {
            r0
        }
    }

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

            let (sa, sb) = (self.cell_style(a), self.cell_style(b));
            let fill = |s: &crate::style::Style| {
                (
                    s.fill.pattern,
                    s.fill.color,
                    s.fill.pattern_color,
                    s.fill.raw.clone(),
                )
            };
            if sa.font != sb.font || sa.alignment != sb.alignment || fill(sa) != fill(sb) {
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
                    ) && self.formula_text(a).is_some_and(|t| is_subtotal_call(&t))
                })
            })
            .collect()
    }

    pub(crate) fn subtotal_refs_rows(&self, sheet: SheetId, rect: Rect, row: u32) -> bool {
        (rect.c0..=rect.c1).any(|c| {
            self.subtotal_ref_rows_at(CellAddr::new(sheet, row, c))
                .is_some_and(|rows| rows.iter().any(|&n| n > rect.r0 && n <= rect.r1 + 1))
        })
    }

    fn subtotal_ref_rows_at(&self, at: CellAddr) -> Option<Vec<u32>> {
        let t = self.formula_text(at)?;
        if !is_subtotal_call(&t) {
            return None;
        }
        let t = t.to_ascii_uppercase();

        let b = t.as_bytes();
        let mut rows = Vec::new();
        let mut i = 0;
        while i < b.len() {
            if b[i].is_ascii_alphabetic() && (i == 0 || !b[i - 1].is_ascii_alphanumeric()) {
                let mut j = i;
                while j < b.len() && (b[j].is_ascii_alphabetic() || b[j] == b'$') {
                    j += 1;
                }
                let k = j;
                while j < b.len() && b[j].is_ascii_digit() {
                    j += 1;
                }
                if j > k && j - k < 8 && (j == b.len() || !b[j].is_ascii_alphanumeric()) {
                    if let Ok(n) = t[k..j].parse::<u32>() {
                        rows.push(n);
                    }
                }
                i = j.max(i + 1);
            } else {
                i += 1;
            }
        }
        Some(rows)
    }

    fn subtotal_row_any(&self, sheet: SheetId, rect: &Rect, row: u32) -> bool {
        let one = Rect {
            r0: row,
            r1: row,
            ..*rect
        };
        !self.subtotal_rows(sheet, one).is_empty()
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

        let bad = || EditError::Compile("RemoveSubtotal method of Range class failed".into());
        let rect = if rect.is_cell() || self.merge_area_exact(sheet, rect) {
            self.select_for_single_cell(sheet, rect).ok_or_else(bad)?
        } else {
            rect
        };
        let rect = self.lim_ref(sheet, rect).ok_or_else(bad)?;
        self.subtotal_guard(rect)?;
        self.note_cell_format(CellAddr::new(sheet, rect.r0, rect.c0));
        self.remove_subtotal_rows(sheet, rect, true)
    }

    pub(crate) fn remove_subtotal_rows(
        &mut self,
        sheet: SheetId,
        rect: Rect,
        recalc: bool,
    ) -> Result<RecalcReport, EditError> {
        let Some(e) = self.cell_table_extent(sheet) else {
            return Ok(RecalcReport::default());
        };
        let rect = Rect {
            r1: rect.r1.min(e.r1),
            c1: rect.c1.min(e.c1),
            ..rect
        };
        if rect.r0 > rect.r1 || rect.c0 > rect.c1 {
            return Ok(RecalcReport::default());
        }

        if !self.filter_mode(sheet) {
            let outlined = self
                .views
                .get(&sheet)
                .is_some_and(|v| v.levels.range(rect.r0..=rect.r1).next().is_some());
            self.set_subtotal_levels(sheet, rect.r0, rect.r1, None);
            if outlined {
                self.show_rows(sheet, rect.r0, rect.r1);
            }
        }
        let rows = self.subtotal_rows(sheet, rect);
        let mut report = RecalcReport::default();
        let mode = self.settings.mode;
        self.settings.mode = CalcMode::Manual;
        let mut res = Ok(());
        for &r in rows.iter().rev() {
            self.drop_total_break(sheet, r);
            match self.with_op(Op::SubtotalDelete, true, |wb| wb.delete_rows(sheet, r, 1)) {
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
        if !recalc {
            return Ok(calc::finish_report(report));
        }
        let r = calc::recalc(self);
        report.evaluations += r.evaluations;
        report.circular = r.circular;
        report.changed.extend(r.changed);
        Ok(calc::finish_report(report))
    }

    fn subtotal_above_plan(&self, sheet: SheetId, inp: &AboveIn) -> AbovePlan {
        let AboveIn {
            rect,
            gcol,
            first,
            existing,
            runs,
            totals,
            code,
            fm,
        } = *inp;
        let hdr = i64::from(first) - 1;
        let is_old = |r: i64| r >= 0 && existing.contains(&(r as u32));

        let area_last = |r: i64| -> Option<i64> {
            let r = u32::try_from(r).ok()?;
            let at = (rect.c0..=rect.c1)
                .map(|c| CellAddr::new(sheet, r, c))
                .find(|&a| self.subtotal_ref_rows_at(a).is_some())?;
            subtotal_area_code(&self.formula_text(at)?)?;
            let rows = self.subtotal_ref_rows_at(at)?;
            Some(i64::from(rows.into_iter().max()?) - 1)
        };
        let (mut n, mut n2, mut g) = (0i64, 0i64, true);
        let mut ins = vec![0u32; runs.len()];
        let mut grouped = vec![true; runs.len()];
        let mut breaks = vec![None; runs.len()];
        for k in (0..runs.len()).rev() {
            let (a, b) = (i64::from(runs[k].0), i64::from(runs[k].1));
            let mut cur = a - 1;
            while is_old(cur) {
                cur -= 1;
            }
            let exhausted = cur <= hdr;
            let p = if cur == a - 1 {
                g = !fm;
                a
            } else if n != 0 {
                a - n
            } else {
                let e = if exhausted { cur - 1 } else { cur };
                n2 = a - e - 1;
                g = !fm;
                let mut r = a - 1;
                while r > e {
                    let Some(last) = area_last(r) else { break };
                    if last == b {
                        g = false;
                    }
                    if b < last {
                        break;
                    }
                    r -= 1;
                }
                n = a - r - 1;
                r + 1
            };
            ins[k] = p.max(cur + 1).max(i64::from(first)) as u32;
            grouped[k] = g;
            breaks[k] = (!exhausted).then(|| (cur + 1) as u32);
        }

        let mut grand = Some((first, false));
        if let (Some(&(top, _)), Some(&plast)) = (runs.first(), ins.first()) {
            let plast = i64::from(plast);
            let f = i64::from(top) + 1;

            let orig = |row: i64| -> Option<u32> {
                if row == plast {
                    None
                } else if row < plast {
                    u32::try_from(row).ok()
                } else {
                    u32::try_from(row - 1).ok()
                }
            };
            let formula_ok = |row: i64, col: u32| match orig(row) {
                None => true,
                Some(o) => self
                    .formula_text(CellAddr::new(sheet, o, col))
                    .is_some_and(|t| is_subtotal_area(&t, code)),
            };
            let text_at = |row: i64, col: u32| match orig(row) {
                None => col == gcol,
                Some(o) => self
                    .sheet(sheet)
                    .unwrap()
                    .cells
                    .get(o, col)
                    .is_some_and(|e| matches!(e, Entry::Const(Value::Text(_)))),
            };
            let mut need = true;
            if n2 >= 1 {
                let (tot, c0) = totals;
                need = (hdr + 1..=f - n2 - 2)
                    .any(|row| tot.iter().any(|&t| !formula_ok(row, c0 + t - 1)));
            }
            grand = need.then(|| {
                let x = if n2 == n {
                    hdr + 1
                } else {
                    let label = (rect.c0..=rect.c1).find(|&c| text_at(f - n - 2, c));
                    let mut r = f - n2 - 2;
                    while r > hdr && !label.is_some_and(|c| text_at(r, c)) {
                        r -= 1;
                    }
                    r + 1
                }
                .max(hdr + 1);
                if x <= plast {
                    (x as u32, false)
                } else {
                    ((x - 1) as u32, true)
                }
            });
        }
        AbovePlan {
            ins,
            grouped,
            breaks,
            n2,
            grand,
        }
    }

    pub fn subtotal(
        &mut self,
        sheet: SheetId,
        rect: Rect,
        spec: &SubtotalSpec,
    ) -> Result<RecalcReport, EditError> {
        let r = self.command(Command::Subtotal, |wb| wb.subtotal_run(sheet, rect, spec));

        if r.is_ok() {
            self.view_notify_sel(sheet, rect.r0, rect.c0);
        }
        r
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
            self.calc.settled.clear();
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

        if !spec.replace {
            let rect = *rect;
            old.retain(|&r| self.subtotal_refs_rows(sheet, rect, r));
        }

        let existing = if spec.replace {
            Vec::new()
        } else {
            std::mem::take(&mut old)
        };

        let list_levels = self
            .views
            .get(&sheet)
            .is_some_and(|v| v.levels.range(rect.r0..=rect.r1).next().is_some());
        let below = if list_levels {
            !self.outline_settings(sheet).0
        } else {
            spec.below
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
            let rep = self.with_op(Op::SubtotalDelete, true, |wb| wb.delete_rows(sheet, r, 1))?;
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
            && blank_row(self, last)
            && !in_filter(self, last)
            && used.is_none_or(|u| last > u)
        {
            last -= 1;
        }
        if first > last {
            return Ok(Vec::new());
        }

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
        let mut pending: Option<(u32, u32)> = None;
        for r in first..=last {
            if existing.contains(&r) {
                if let Some((a, b, ck)) = cur.take() {
                    if !ck.is_empty() && !all_hidden(self, a, b) {
                        let label = self.display_text(CellAddr::new(sheet, a, gcol));
                        runs.push(Run { a, b, label });
                    }
                }

                if r == last && !spec.below && !all_hidden(self, r, r) {
                    let label = self.display_text(CellAddr::new(sheet, r, gcol));
                    runs.push(Run { a: r, b: r, label });
                }
                continue;
            }

            let merged_hidden = !spec.below && {
                let at = CellAddr::new(sheet, r, gcol);
                self.merge_at(at).is_some() && self.merged_hidden(at)
            };
            if let Some((a, _)) = pending.take() {
                if merged_hidden {
                    pending = Some((a, r));
                } else {
                    runs.push(Run {
                        a,
                        b: r,
                        label: String::new(),
                    });
                }
                continue;
            }
            if merged_hidden {
                if let Some((a, b, ck)) = cur.take() {
                    if !ck.is_empty() && !all_hidden(self, a, b) {
                        let label = self.display_text(CellAddr::new(sheet, a, gcol));
                        runs.push(Run { a, b, label });
                    }
                }
                pending = Some((r, r));
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
        if let Some((a, b)) = pending.take() {
            runs.push(Run {
                a,
                b,
                label: String::new(),
            });
        }

        let mut at: std::collections::BTreeMap<u32, u32> = std::collections::BTreeMap::new();
        let (code_n, _) = spec.function.code_name();

        let plan = (!spec.below).then(|| {
            let span: Vec<(u32, u32)> = runs.iter().map(|r| (r.a, r.b)).collect();
            self.subtotal_above_plan(
                sheet,
                &AboveIn {
                    rect: *rect,
                    gcol,
                    first,
                    existing: &existing,
                    runs: &span,
                    totals: (&spec.totals, c0),
                    code: code_n,
                    fm,
                },
            )
        });
        let ins_pts: Vec<u32> = match &plan {
            Some(pl) => pl.ins.clone(),
            None => runs.iter().map(|r| r.b + 1).collect(),
        };
        for &p in &ins_pts {
            *at.entry(p).or_default() += 1;
        }

        let tail = if spec.below {
            runs.last()
                .map(|r| r.b)
                .filter(|&b| {
                    b < last && (b + 1..=last).all(|r| self.subtotal_row_any(sheet, rect, r))
                })
                .map(|b| {
                    let l = self.row_level(sheet, last);
                    let mut ec = last;
                    while ec - 1 > b && self.row_level(sheet, ec - 1) == l {
                        ec -= 1;
                    }
                    (b, ec)
                })
        } else {
            None
        };

        let tail_start = tail.filter(|&(e, ec)| ec > e + 1).map(|(_, ec)| ec);
        let text_at = |wb: &Workbook, r: u32, c: u32| {
            matches!(
                wb.sheet(sheet).unwrap().cells.get(r, c),
                Some(Entry::Const(Value::Text(_)))
            )
        };

        let grand_plan: Option<Result<(u32, bool), u32>> = match &plan {
            Some(pl) => Some(pl.grand.ok_or(first)),
            None => tail.and_then(|(e, _)| {
                let ec = tail_start?;
                let dup = (ec..=last).all(|r| {
                    spec.totals.iter().all(|&t| {
                        self.formula_text(CellAddr::new(sheet, r, c0 + t - 1))
                            .is_some_and(|f| is_subtotal_area(&f, code_n))
                    })
                });
                Some(if dup {
                    Err(ec)
                } else {
                    let lc = (rect.c0..=rect.c1).find(|&c| text_at(self, e + 1, c));
                    Ok((
                        lc.and_then(|c| (ec..=last).find(|&r| text_at(self, r, c)))
                            .unwrap_or(last + 1),
                        false,
                    ))
                })
            }),
        };
        let grand_dup = grand_plan.and_then(|g| g.err());
        let (grand_key, grand_after) = grand_plan
            .and_then(|g| g.ok())
            .unwrap_or((if spec.below { last + 1 } else { first }, false));
        if grand_dup.is_none() {
            *at.entry(grand_key).or_default() += 1;
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

        let below_end = spec.below.then(|| {
            (first..=last).rev().find(|&r| {
                (!existing.contains(&r) && !self.subtotal_row_any(sheet, rect, r))
                    || (self.row_level(sheet, r) > 1
                        && r > first
                        && self.row_level(sheet, r) >= self.row_level(sheet, r - 1))
            })
        });

        let shift = |r: u32| -> u32 { at.range(..=r).map(|(_, n)| n).sum::<u32>() };
        let below_shift = |r: u32| -> u32 { at.range(..r).map(|(_, n)| n).sum::<u32>() };

        if !spec.below && spec.page_breaks {
            for r in rect.r0..=last {
                self.delete_page_break(sheet, true, r);
            }
        }

        let reach: Vec<usize> = self.views.get(&sheet).map_or(Vec::new(), |v| {
            v.filters
                .iter()
                .enumerate()
                .filter(|(_, f)| {
                    f.table.is_none()
                        && f.rect.r0 <= last
                        && f.rect.r1 >= last
                        && f.rect.c0 <= rect.c1
                        && f.rect.c1 >= rect.c0
                })
                .map(|(k, _)| k)
                .collect()
        });
        let orig_lv: Vec<(u32, u8)> = (rect.r0..=last)
            .map(|r| (r, self.row_level(sheet, r).saturating_sub(1) as u8))
            .collect();

        let above_info: Vec<(u8, bool)> = match &plan {
            None => Vec::new(),
            Some(pl) => ins_pts
                .iter()
                .zip(&pl.grouped)
                .map(|(&p, &g)| (self.row_level(sheet, p).saturating_sub(1) as u8, !g))
                .collect(),
        };
        let mut added = 0;
        for (&p, &n) in at.iter() {
            let rep = self.insert_rows_raw(sheet, p + added, n)?;
            report.changed.extend(rep.changed);
            added += n;
        }
        if let Some(v) = self.views.get_mut(&sheet) {
            for k in reach {
                if let Some(f) = v.filters.get_mut(k) {
                    f.rect.r1 = f.rect.r1.max(last + added);
                }
            }
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

            wb.apply_style_with(sheet, Rect::new(g.row, g.col, g.row, g.col), &|s| {
                let mut s = s.clone();
                s.font.bold = true;
                s
            });
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
            } else {
                p + below_shift(p)
                    + u32::from(grand_dup.is_none() && p == grand_key && !grand_after)
            };
            put(self, row, &format!("{} {}", run.label, name), a, b)?;
            run_rows.push(row);
        }
        let grand = if let Some(g) = grand_dup {
            fin(g)
        } else if spec.below {
            grand_key + below_shift(grand_key) + at.get(&grand_key).copied().unwrap_or(1) - 1
        } else {
            grand_key
                + below_shift(grand_key)
                + if grand_after {
                    at.get(&grand_key).copied().unwrap_or(1) - 1
                } else {
                    0
                }
        };

        if spec.page_breaks {
            if let Some(pl) = &plan {
                for &r in pl.breaks.iter().flatten() {
                    let own = u32::from(grand_dup.is_none() && grand_key == r);
                    self.add_page_break(sheet, true, r + below_shift(r) + own);
                }
            } else {
                for &t in &run_rows {
                    self.add_page_break(sheet, true, t + 1 + u32::from(grand == t + 1));
                }
            }
        }
        let data_end = (first..=last)
            .rev()
            .find(|r| !existing.contains(r))
            .map_or(fb, fin);

        if grand_dup.is_none() {
            let (ga, gb) = match below_end {
                Some(e) => (fin(first), e.map_or(fb, fin)),
                None => (fa, data_end),
            };
            put(self, grand, &format!("Grand {name}"), ga, gb)?;
        }

        if outline && spec.below {
            let mut lv: std::collections::BTreeMap<u32, u8> =
                orig_lv.iter().map(|&(r, l)| (fin(r), l)).collect();
            let deeper = |lv: &mut std::collections::BTreeMap<u32, u8>, r: u32| {
                let l = lv.entry(r).or_default();
                *l = (*l + 1).min(7);
            };
            for (run, &t) in runs.iter().zip(&run_rows) {
                let above = lv.get(&(t - 1)).copied().unwrap_or(0);
                lv.insert(t, above);
                for r in fin(run.a)..=fin(run.b) {
                    deeper(&mut lv, r);
                }
            }
            if grand_dup.is_none() {
                let above = lv.get(&(grand - 1)).copied().unwrap_or(0);
                lv.insert(grand, above);
                if tail_start.is_none() {
                    let end = tail.map_or(last, |(b, _)| b);
                    for r in fin(first)..=fin(end) + 1 {
                        deeper(&mut lv, r);
                    }
                } else if Some(grand_key) == tail_start && above > 0 {
                    lv.insert(grand, above - 1);
                }
            }
            for (r, l) in lv {
                self.set_subtotal_levels(sheet, r, r, Some(l));
            }
        } else if outline {
            let mut lv: std::collections::BTreeMap<u32, u8> =
                orig_lv.iter().map(|&(r, l)| (fin(r), l)).collect();
            let deeper = |lv: &mut std::collections::BTreeMap<u32, u8>, r: u32| {
                let l = lv.entry(r).or_default();
                *l = (*l + 1).min(7);
            };
            for ((run, &t), &(above, covered)) in runs.iter().zip(&run_rows).zip(&above_info) {
                lv.insert(t, above);
                if !covered {
                    for r in t + 1..=fin(run.b) {
                        deeper(&mut lv, r);
                    }
                }
            }
            if grand_dup.is_none() {
                let above = grand
                    .checked_sub(1)
                    .and_then(|g| lv.get(&g).copied())
                    .unwrap_or(0);
                lv.insert(grand, above);

                if plan.as_ref().is_some_and(|pl| pl.n2 == 0) {
                    if let Some(&t0) = run_rows.first() {
                        for r in t0..=fb {
                            deeper(&mut lv, r);
                        }
                    }
                }
            }
            for (r, l) in lv {
                self.set_subtotal_levels(sheet, r, r, Some(l));
            }
        } else {
            let lv = if spec.below {
                let inherited = self
                    .row_level(sheet, grand.saturating_sub(1))
                    .saturating_sub(1);
                let n = tail.zip(tail_start).map_or(0, |((b, _), ec)| ec - b - 1);
                let one_up = run_rows
                    .last()
                    .is_some_and(|&t| grand > 0 && grand == t + 1 + n);
                let l = if one_up {
                    inherited.saturating_sub(1)
                } else {
                    inherited
                };
                (l > 0).then_some(l as u8)
            } else {
                let below = self.row_level(sheet, grand + 1);
                (below > 1).then(|| (below - 1) as u8)
            };
            self.set_subtotal_levels(sheet, grand, grand, lv);
        }
        Ok(fresh)
    }
}

fn is_subtotal_call(t: &str) -> bool {
    let t = t.trim_start_matches('=');
    let Some(head) = t.get(..9) else {
        return false;
    };
    if !head.eq_ignore_ascii_case("SUBTOTAL(") {
        return false;
    }
    let (mut depth, mut quoted) = (0u32, false);
    for (i, ch) in t.char_indices().skip(8) {
        match ch {
            '"' => quoted = !quoted,
            '(' if !quoted => depth += 1,
            ')' if !quoted => {
                depth -= 1;
                if depth == 0 {
                    return i + 1 == t.len();
                }
            }
            _ => {}
        }
    }
    false
}

fn is_subtotal_area(t: &str, code: u32) -> bool {
    subtotal_area_code(t) == Some(code)
}

fn subtotal_area_code(t: &str) -> Option<u32> {
    let t = t.trim_start_matches('=').to_ascii_uppercase();
    let args = t
        .strip_prefix("SUBTOTAL(")
        .and_then(|s| s.strip_suffix(')'))?;
    let (n, area) = args.split_once(',')?;

    let part = |s: &str| {
        let s = s.trim_start_matches('$');
        let letters = s.bytes().take_while(u8::is_ascii_alphabetic).count();
        let rest = s[letters..].trim_start_matches('$');
        (letters > 0 || !rest.is_empty()) && rest.bytes().all(|b| b.is_ascii_digit())
    };
    area.split_once(':')
        .is_some_and(|(a, b)| part(a) && part(b))
        .then(|| n.parse::<u32>().ok())
        .flatten()
}

#[cfg(test)]
mod tests {
    use crate::address::{CellAddr, Rect};
    use crate::value::Value;
    use crate::view::FilterSpec;
    use crate::workbook::Workbook;

    fn filter_qty_over_3(wb: &mut Workbook) {
        let s = wb.sheet_at(0).id;
        let spec = FilterSpec {
            field: 2,
            criteria1: Some(">3"),
            ..FilterSpec::default()
        };
        wb.autofilter(s, Rect::cell(0, 0), None, spec).unwrap();
    }

    #[test]
    fn autofilter_from_cell_skips_title_row() {
        let mut wb = Workbook::new();
        let s = wb.sheet_at(0).id;
        let a = |r, c| CellAddr::new(s, r, c);
        wb.set_value(a(0, 0), Value::text("Report"));
        wb.set_value(a(0, 2), Value::text("x"));
        for (c, h) in ["Name", "Qty", "Grade"].into_iter().enumerate() {
            wb.set_value(a(1, c as u32), Value::text(h));
        }
        for (r, q) in [5.0, 1.0, 7.0].into_iter().enumerate() {
            let r = r as u32 + 2;
            wb.set_value(a(r, 0), Value::text("k"));
            wb.set_value(a(r, 1), Value::Number(q));
            wb.set_value(a(r, 2), Value::text("Low"));
        }
        filter_qty_over_3(&mut wb);

        assert_eq!(wb.hidden_rows(s), vec![3]);
    }

    #[test]
    fn autofilter_from_lone_empty_cell_is_refused() {
        let mut wb = Workbook::new();
        let s = wb.sheet_at(0).id;
        let a = |r, c| CellAddr::new(s, r, c);
        for (r, t) in ["h", "Low", "High", "Low"].into_iter().enumerate() {
            wb.set_value(a(r as u32, 2), Value::text(t));
        }
        let spec = || FilterSpec {
            field: 1,
            criteria1: Some("Low"),
            ..FilterSpec::default()
        };
        assert!(wb.autofilter(s, Rect::cell(0, 0), None, spec()).is_err());
        assert!(wb.hidden_rows(s).is_empty());

        for (r, t) in ["g", "Low", "High", "High"].into_iter().enumerate() {
            wb.set_value(a(r as u32, 1), Value::text(t));
        }
        wb.autofilter(s, Rect::cell(0, 0), None, spec()).unwrap();
        assert_eq!(wb.hidden_rows(s), vec![2, 3]);
    }

    fn kv_list(wb: &mut Workbook) {
        let s = wb.sheet_at(0).id;
        let a = |r, c| CellAddr::new(s, r, c);
        wb.set_value(a(0, 0), Value::text("k"));
        wb.set_value(a(0, 1), Value::text("v"));
        for (i, k) in ["a", "a", "b", "b", "c", "c"].into_iter().enumerate() {
            let r = i as u32 + 1;
            wb.set_value(a(r, 0), Value::text(k));
            wb.set_value(a(r, 1), Value::Number(f64::from(r)));
        }
    }

    fn kv_spec(function: crate::subtotal::SubFunc, replace: bool) -> crate::subtotal::SubtotalSpec {
        crate::subtotal::SubtotalSpec {
            group: 1,
            function,
            totals: vec![2],
            replace,
            below: true,
            page_breaks: false,
        }
    }

    #[test]
    fn remove_subtotal_shows_rows_of_an_outline() {
        use crate::subtotal::SubFunc;
        let mut wb = Workbook::new();
        let s = wb.sheet_at(0).id;
        kv_list(&mut wb);
        wb.subtotal(s, Rect::new(0, 0, 6, 1), &kv_spec(SubFunc::Sum, true))
            .unwrap();
        wb.set_rows_hidden(s, 1, 2, true).unwrap();
        wb.remove_subtotal(s, Rect::cell(0, 0)).unwrap();
        assert!(wb.hidden_rows(s).is_empty());
        wb.set_rows_hidden(s, 2, 1, true).unwrap();
        wb.remove_subtotal(s, Rect::cell(0, 0)).unwrap();
        assert_eq!(wb.hidden_rows(s), vec![2]);
    }

    #[test]
    fn remove_subtotal_refuses_outside_the_cell_table() {
        let mut wb = Workbook::new();
        let s = wb.sheet_at(0).id;
        kv_list(&mut wb);
        assert!(wb.remove_subtotal(s, Rect::cell(20, 5)).is_err());
        assert!(wb.remove_subtotal(s, Rect::new(30, 0, 40, 3)).is_err());
        assert!(wb.remove_subtotal(s, Rect::new(0, 0, 500, 50)).is_ok());
    }

    #[test]
    fn old_grand_total_kept_below_only_with_an_outline() {
        use crate::subtotal::SubFunc;
        for flat in [false, true] {
            let mut wb = Workbook::new();
            let s = wb.sheet_at(0).id;
            kv_list(&mut wb);
            wb.subtotal(s, Rect::new(0, 0, 6, 1), &kv_spec(SubFunc::Sum, true))
                .unwrap();
            if flat {
                wb.set_subtotal_levels(s, 0, 20, None);
            }
            wb.subtotal(s, Rect::cell(0, 0), &kv_spec(SubFunc::Max, false))
                .unwrap();
            let label = |r| wb.display_text(CellAddr::new(s, r, 0));
            let (max, total) = if flat { (14, 13) } else { (13, 14) };
            assert_eq!(label(max), "Grand Max");
            assert_eq!(label(total), "Grand Total");
        }
    }

    #[test]
    fn nested_subtotal_levels_follow_the_rows_above() {
        use crate::subtotal::SubFunc;
        for flat in [false, true] {
            let mut wb = Workbook::new();
            let s = wb.sheet_at(0).id;
            kv_list(&mut wb);
            wb.subtotal(s, Rect::new(0, 0, 6, 1), &kv_spec(SubFunc::Sum, true))
                .unwrap();

            let (r0, r1) = if flat { (0, 20) } else { (1, 2) };
            wb.set_subtotal_levels(s, r0, r1, None);
            wb.subtotal(s, Rect::cell(0, 0), &kv_spec(SubFunc::Max, false))
                .unwrap();
            let levels: Vec<u32> = (0..16).map(|r| wb.row_level(s, r)).collect();
            let want = if flat {
                [1, 3, 3, 2, 2, 3, 3, 2, 2, 3, 3, 2, 1, 1, 1, 1]
            } else {
                [1, 2, 2, 1, 2, 4, 4, 3, 2, 4, 4, 3, 2, 1, 1, 1]
            };
            assert_eq!(levels, want, "flat {flat}");
        }
    }

    #[test]
    fn subtotal_area_shape() {
        use super::is_subtotal_area;
        assert!(is_subtotal_area("=SUBTOTAL(9,B2:B11)", 9));
        assert!(is_subtotal_area("=subtotal(4,$B$2:B$11)", 4));
        assert!(is_subtotal_area("=SUBTOTAL(9,B:B)", 9));
        assert!(!is_subtotal_area("=SUBTOTAL(9,B2:B11)", 4));
        assert!(!is_subtotal_area("=SUBTOTAL(109,B2:B11)", 9));
        assert!(!is_subtotal_area("=SUBTOTAL(9,B2)", 9));
        assert!(!is_subtotal_area("=SUBTOTAL(9,B2:B3,C2:C3)", 9));
    }

    #[test]
    fn filter_mode_grand_total_level() {
        use crate::subtotal::{SubFunc, SubtotalSpec};
        let run = |groups: u32, crit: &str, field: u32| {
            let mut wb = Workbook::new();
            let s = wb.sheet_at(0).id;
            let a = |r, c| CellAddr::new(s, r, c);
            for (c, h) in ["Name", "Qty", "Grade"].into_iter().enumerate() {
                wb.set_value(a(0, c as u32), Value::text(h));
            }
            for (i, n) in ["a", "a", "b", "b", "c", "c"].into_iter().enumerate() {
                let r = i as u32 + 1;
                wb.set_value(a(r, 0), Value::text(n));
                wb.set_value(a(r, 1), Value::Number(f64::from(r)));
                wb.set_value(a(r, 2), Value::text("x"));
            }
            for _ in 0..groups {
                wb.group_rows(s, 1, 6, true).unwrap();
            }
            let spec = FilterSpec {
                field,
                criteria1: Some(crit),
                ..FilterSpec::default()
            };
            wb.autofilter(s, Rect::new(0, 0, 6, 2), None, spec).unwrap();
            let spec = SubtotalSpec {
                group: 1,
                function: SubFunc::Sum,
                totals: vec![2],
                replace: true,
                below: true,
                page_breaks: false,
            };
            wb.subtotal(s, Rect::new(0, 0, 6, 2), &spec).unwrap();
            (0..10).map(|r| wb.row_level(s, r)).collect::<Vec<_>>()
        };

        assert_eq!(run(1, "zzz", 3), [1, 2, 2, 2, 2, 2, 2, 2, 1, 1]);

        assert_eq!(run(2, "c", 1), [1, 3, 3, 3, 3, 3, 3, 3, 2, 1]);
    }

    #[test]
    fn autofilter_from_cell_keeps_header_row() {
        let mut wb = Workbook::new();
        let s = wb.sheet_at(0).id;
        let a = |r, c| CellAddr::new(s, r, c);
        wb.set_value(a(0, 0), Value::text("Name"));
        wb.set_value(a(0, 1), Value::text("Qty"));
        for (r, q) in [5.0, 1.0, 7.0].into_iter().enumerate() {
            wb.set_value(a(r as u32 + 1, 0), Value::text("k"));
            wb.set_value(a(r as u32 + 1, 1), Value::Number(q));
        }
        filter_qty_over_3(&mut wb);
        assert_eq!(wb.hidden_rows(s), vec![2]);
    }

    #[test]
    fn subtotal_above_nests_totals_over_the_old_ones() {
        use crate::subtotal::SubFunc;
        let mut wb = Workbook::new();
        let s = wb.sheet_at(0).id;
        kv_list(&mut wb);
        let mut spec = kv_spec(SubFunc::Sum, true);
        spec.below = false;
        wb.subtotal(s, Rect::new(0, 0, 6, 1), &spec).unwrap();
        let label = |wb: &Workbook, r| wb.display_text(CellAddr::new(s, r, 0));
        assert_eq!(label(&wb, 1), "Grand Total");
        assert_eq!(label(&wb, 2), "a Total");
        assert_eq!(label(&wb, 5), "b Total");
        let lv: Vec<u32> = (0..11).map(|r| wb.row_level(s, r)).collect();
        assert_eq!(lv, [1, 1, 2, 3, 3, 2, 3, 3, 2, 3, 3]);
        let mut again = kv_spec(SubFunc::Count, false);
        again.below = false;
        wb.subtotal(s, Rect::new(0, 0, 12, 1), &again).unwrap();
        assert_eq!(label(&wb, 1), "Grand Count");
        assert_eq!(label(&wb, 2), "Grand Total");
        assert_eq!(label(&wb, 3), "a Count");
        assert_eq!(label(&wb, 4), "a Total");
    }
}
