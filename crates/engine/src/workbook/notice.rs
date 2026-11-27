use crate::address::{CellAddr, Rect, SheetId, MAX_COLS, MAX_ROWS};
use crate::calc::RecalcReport;

use super::{CalcMode, EditError, Workbook};

#[derive(Clone, Debug, PartialEq, Eq)]
pub enum Notice {
    Changed { sheet: SheetId, areas: Vec<Rect> },

    ChangedAsOne { sheet: SheetId, areas: Vec<Rect> },

    Calculated { sheets: Vec<SheetId> },

    AfterCalculate,

    SheetAdded(SheetId),

    SheetActivated { sheet: SheetId, previous: SheetId },

    SheetDeleting(SheetId),

    SheetMoved(SheetId),

    SelectionChanged { sheet: SheetId, areas: Vec<Rect> },
}

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
#[non_exhaustive]
pub enum Command {
    Entry,

    ArrayEntry,

    ClearContents,

    Clear,

    ClearFormats,

    Format,

    Insert,

    Delete,

    Copy,

    Cut,

    PasteSpecial,

    Fill,

    DataSeries,

    Sort,

    Atp,

    Pivot,

    Scenario,

    Replace,

    RemoveDuplicates,

    TextToColumns,

    Subtotal,

    Merge,

    TableAdd,

    Table,

    TableResize,

    DataTable,

    GoalSeek,

    View,

    Name,

    Control,

    Calculate,

    CalculateSheet,

    CalculateRange,

    Setting,

    AddSheet,

    CopySheet,

    RenameSheet,

    MoveSheet,

    DeleteSheet,

    ActivateSheet,
}

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub(crate) enum Order {
    CalcFirst,

    CalcThenChange,

    ChangeFirst,
}

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub(crate) enum Recalc {
    Never,

    Auto,

    Forced,

    Always,
}

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub(crate) enum Dispatch {
    Direct,

    Menu(u16),

    Table,

    Unlisted,
}

impl Dispatch {
    pub(crate) const fn after_at(self) -> AfterAt {
        match self {
            Dispatch::Direct => AfterAt::Recalc,
            Dispatch::Menu(_) | Dispatch::Table | Dispatch::Unlisted => AfterAt::CommandEnd,
        }
    }
}

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub(crate) struct Rule {
    pub order: Order,

    pub dispatch: Dispatch,

    pub change: bool,

    pub calc: bool,

    pub recalc: Recalc,
}

const fn r(site: Site, dispatch: Dispatch, change: bool, calc: bool, recalc: Recalc) -> Rule {
    Rule {
        order: order(site, dispatch.after_at()),
        dispatch,
        change,
        calc,
        recalc,
    }
}

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub(crate) enum Site {
    ChangeAfterRecalc,

    ChangeBeforeRecalc,
}

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub(crate) enum AfterAt {
    Recalc,

    CommandEnd,
}

const fn order(site: Site, after: AfterAt) -> Order {
    match (site, after) {
        (Site::ChangeBeforeRecalc, _) => Order::ChangeFirst,
        (Site::ChangeAfterRecalc, AfterAt::Recalc) => Order::CalcFirst,
        (Site::ChangeAfterRecalc, AfterAt::CommandEnd) => Order::CalcThenChange,
    }
}

pub(crate) const fn rule(cmd: Command) -> Rule {
    use Dispatch::{Direct, Menu, Table, Unlisted};
    use Recalc::{Always, Auto, Forced, Never};
    use Site::{ChangeAfterRecalc as AfterRecalc, ChangeBeforeRecalc as BeforeRecalc};
    match cmd {
        Command::Entry => r(AfterRecalc, Direct, true, true, Auto),

        Command::Delete => r(AfterRecalc, Menu(0x36), true, true, Auto),

        Command::ClearContents => r(AfterRecalc, Menu(0x5d6), true, true, Auto),
        Command::Clear | Command::ClearFormats => r(AfterRecalc, Menu(0x34), true, true, Auto),

        Command::PasteSpecial => r(BeforeRecalc, Menu(0x35), true, true, Auto),

        Command::Copy => r(BeforeRecalc, Menu(0x32), true, true, Auto),

        Command::RemoveDuplicates => r(BeforeRecalc, Menu(0x377), true, true, Auto),

        Command::ArrayEntry | Command::Fill => r(AfterRecalc, Table, true, true, Auto),

        Command::Insert => r(BeforeRecalc, Menu(0x37), true, true, Auto),
        Command::Cut => r(BeforeRecalc, Menu(0x31), true, true, Auto),

        Command::Replace => r(BeforeRecalc, Direct, true, true, Auto),

        Command::DataSeries => r(BeforeRecalc, Menu(0x28), true, true, Auto),
        Command::Format | Command::TableAdd => r(BeforeRecalc, Direct, false, false, Never),

        Command::Sort => r(BeforeRecalc, Menu(0x27), false, true, Auto),
        Command::Scenario => r(BeforeRecalc, Menu(0x135), false, true, Auto),
        Command::Subtotal => r(BeforeRecalc, Menu(0x18e), false, true, Auto),
        Command::DataTable => r(BeforeRecalc, Menu(0x29), false, true, Auto),
        Command::View => r(BeforeRecalc, Menu(0x1bf), false, true, Auto),
        Command::TextToColumns => r(BeforeRecalc, Menu(0x1a6), false, true, Auto),
        Command::Merge => r(BeforeRecalc, Menu(0x1e8), false, true, Auto),
        Command::Control => r(BeforeRecalc, Menu(0x1b3), false, true, Auto),

        Command::Atp => r(BeforeRecalc, Unlisted, false, true, Auto),

        Command::Name | Command::Pivot | Command::Setting => {
            r(BeforeRecalc, Direct, false, true, Auto)
        }

        Command::Table | Command::TableResize => r(BeforeRecalc, Direct, true, true, Auto),

        Command::GoalSeek => r(BeforeRecalc, Menu(0xc6), false, true, Forced),
        Command::Calculate => r(BeforeRecalc, Menu(0x1f), false, true, Always),

        Command::CalculateSheet => r(BeforeRecalc, Menu(0xa7), false, true, Never),
        Command::CalculateRange => r(BeforeRecalc, Menu(0x1f), false, true, Never),
        Command::AddSheet | Command::ActivateSheet => r(BeforeRecalc, Direct, false, false, Never),

        Command::CopySheet => r(AfterRecalc, Menu(0x11b), false, true, Auto),
        Command::DeleteSheet => r(AfterRecalc, Menu(0x181), false, true, Auto),
        Command::RenameSheet => r(AfterRecalc, Menu(0x182), false, true, Auto),
        Command::MoveSheet => r(AfterRecalc, Menu(0x11a), false, true, Auto),
    }
}

pub trait Outcome {
    fn succeeded(&self) -> bool;
}

impl<T> Outcome for Result<T, EditError> {
    fn succeeded(&self) -> bool {
        self.is_ok()
    }
}

impl Outcome for RecalcReport {
    fn succeeded(&self) -> bool {
        true
    }
}

impl Outcome for Option<RecalcReport> {
    fn succeeded(&self) -> bool {
        self.is_some()
    }
}

impl Outcome for () {
    fn succeeded(&self) -> bool {
        true
    }
}

#[derive(Default)]
pub(crate) struct Notes {
    log: Vec<Notice>,

    depth: u32,

    slot: Vec<Notice>,

    early: Vec<Notice>,

    pub(crate) passes: Vec<Pass>,

    partial: Vec<SheetId>,

    pending: Vec<SheetId>,

    active: Option<SheetId>,

    pub(crate) no_after: bool,

    pub(crate) change_late: bool,

    pub(crate) header_written: bool,

    pub(crate) after_if_calculated: bool,
}

#[derive(Clone, Debug, Default)]
pub(crate) struct Pass {
    pub sheets: Vec<SheetId>,
    pub silent: bool,
}

impl Notes {
    pub(crate) fn idle(&self) -> bool {
        self.depth == 0 && self.slot.is_empty() && self.passes.is_empty() && self.partial.is_empty()
    }

    pub(crate) fn forget_calculated(&mut self, sheet: SheetId) {
        for p in &mut self.passes {
            p.sheets.retain(|&s| s != sheet);
        }
    }

    pub(crate) fn pass(&mut self, sheets: &[SheetId], silent: bool) {
        if self.depth == 0 {
            return;
        }
        let mut all = std::mem::take(&mut self.partial);
        add_new(&mut all, sheets);
        self.passes.push(Pass {
            sheets: all,
            silent,
        });
    }

    pub(crate) fn calculated(&mut self, sheets: &[SheetId]) {
        if self.depth == 0 {
            return;
        }
        match self.passes.last_mut() {
            Some(p) => add_new(&mut p.sheets, sheets),
            None => add_new(&mut self.partial, sheets),
        }
    }

    fn close_partial(&mut self) {
        if !self.partial.is_empty() && self.passes.is_empty() {
            let sheets = std::mem::take(&mut self.partial);
            self.passes.push(Pass {
                sheets,
                silent: false,
            });
        }
        self.partial.clear();
    }

    fn clear(&mut self) {
        self.slot.clear();
        self.early.clear();
        self.passes.clear();
        self.partial.clear();
        self.active = None;
        self.no_after = false;
        self.change_late = false;
        self.header_written = false;
        self.after_if_calculated = false;
    }

    fn finish(
        &mut self,
        cmd: Command,
        ok: bool,
        mode: CalcMode,
        active: SheetId,
        calcs: Vec<Option<Notice>>,
    ) {
        let rule = rule(cmd);
        let mut slot = std::mem::take(&mut self.slot);
        if !ok {
            slot.clear();
            self.early.clear();
        } else if !rule.change {
            slot.retain(|n| !matches!(n, Notice::Changed { .. }));
        }
        if let Some(previous) = self.active.filter(|&p| p != active) {
            slot.push(Notice::SheetActivated {
                sheet: active,
                previous,
            });
        }

        let ran = !self.passes.is_empty();
        let auto = mode == CalcMode::Automatic;
        let per_pass =
            rule.dispatch.after_at() == AfterAt::Recalc && rule.recalc == Recalc::Auto && auto;
        let after = !per_pass
            && match rule.recalc {
                Recalc::Never => false,
                Recalc::Auto => ran && auto,
                Recalc::Forced => ran,
                Recalc::Always => true,
            };
        let mut calc: Vec<Notice> = Vec::new();
        let mut posted = false;
        let late = self.change_late && rule.order == Order::ChangeFirst;
        let mut last_start = 0;
        for n in calcs {
            last_start = calc.len();

            if per_pass && n.is_some() && !self.no_after {
                calc.extend(n);
                calc.push(Notice::AfterCalculate);
                posted = true;
            } else {
                calc.extend(n);
                posted = false;
            }
        }
        let after = after || (per_pass && ran && !posted && !self.after_if_calculated);
        let after = (after && !self.no_after).then_some(Notice::AfterCalculate);
        let log = &mut self.log;
        log.append(&mut self.early);
        if ok && self.header_written && per_pass {
            let tail = calc.split_off(last_start.min(calc.len()));
            log.extend(calc);
            if log.last() != Some(&Notice::AfterCalculate) {
                log.push(Notice::AfterCalculate);
            }
            log.append(&mut slot);
            log.extend(tail);
            if !self.no_after && log.last() != Some(&Notice::AfterCalculate) {
                log.push(Notice::AfterCalculate);
            }
            self.clear();
            return;
        }
        match rule.order {
            Order::CalcFirst => {
                log.extend(calc);
                log.extend(after);
                log.append(&mut slot);
            }
            Order::CalcThenChange => {
                log.extend(calc);
                log.append(&mut slot);
                log.extend(after);
            }
            Order::ChangeFirst if late && last_start > 0 => {
                let tail = calc.split_off(last_start);
                log.extend(calc);
                log.append(&mut slot);
                log.extend(tail);
                log.extend(after);
            }
            Order::ChangeFirst => {
                log.append(&mut slot);
                log.extend(calc);
                log.extend(after);
            }
        }
        self.clear();
    }
}

fn add_new(to: &mut Vec<SheetId>, more: &[SheetId]) {
    for &s in more {
        if !to.contains(&s) {
            to.push(s);
        }
    }
}

pub(crate) fn cell(a: CellAddr) -> Rect {
    Rect::new(a.row, a.col, a.row, a.col)
}

pub(crate) fn rows_rect(at: u32, count: u32) -> Rect {
    Rect::new(at, 0, last(at, count, MAX_ROWS), MAX_COLS - 1)
}

pub(crate) fn cols_rect(at: u32, count: u32) -> Rect {
    Rect::new(0, at, MAX_ROWS - 1, last(at, count, MAX_COLS))
}

fn last(at: u32, count: u32, max: u32) -> u32 {
    at.saturating_add(count.max(1) - 1).min(max - 1)
}

pub fn paste_area(src: Rect, dst: Rect) -> Rect {
    let (rows, cols) = (src.rows(), src.cols());
    if dst.rows().is_multiple_of(rows) && dst.cols().is_multiple_of(cols) {
        dst
    } else {
        Rect::new(
            dst.r0,
            dst.c0,
            last(dst.r0, rows, MAX_ROWS),
            last(dst.c0, cols, MAX_COLS),
        )
    }
}

pub(crate) fn fill_rest(src: Rect, dst: Rect) -> Vec<Rect> {
    let same_cols = src.c0 == dst.c0 && src.c1 == dst.c1;
    let same_rows = src.r0 == dst.r0 && src.r1 == dst.r1;
    let rest = if same_cols && dst.r0 == src.r0 && dst.r1 > src.r1 {
        Rect::new(src.r1 + 1, dst.c0, dst.r1, dst.c1)
    } else if same_cols && dst.r1 == src.r1 && dst.r0 < src.r0 {
        Rect::new(dst.r0, dst.c0, src.r0 - 1, dst.c1)
    } else if same_rows && dst.c0 == src.c0 && dst.c1 > src.c1 {
        Rect::new(dst.r0, src.c1 + 1, dst.r1, dst.c1)
    } else if same_rows && dst.c1 == src.c1 && dst.c0 < src.c0 {
        Rect::new(dst.r0, dst.c0, dst.r1, src.c0 - 1)
    } else if src == dst {
        return Vec::new();
    } else {
        dst
    };
    vec![rest]
}

pub(crate) fn bounds(cells: impl Iterator<Item = CellAddr>) -> Option<Rect> {
    cells.fold(None, |acc: Option<Rect>, a| {
        Some(match acc {
            None => cell(a),
            Some(r) => Rect::new(
                r.r0.min(a.row),
                r.c0.min(a.col),
                r.r1.max(a.row),
                r.c1.max(a.col),
            ),
        })
    })
}

pub(crate) fn note_cut(wb: &mut Workbook, sheet: SheetId, src: Rect, dst: CellAddr) {
    wb.note_changed(sheet, &[src]);
    let moved = Rect::new(
        dst.row,
        dst.col,
        last(dst.row, src.rows(), MAX_ROWS),
        last(dst.col, src.cols(), MAX_COLS),
    );
    wb.note_changed(dst.sheet, &[moved]);
}

pub(crate) fn note_deleting(wb: &mut Workbook, id: SheetId) {
    if wb.sheet(id).is_some() {
        wb.note_sheet(Notice::SheetDeleting(id));
    }
}

struct Scope<'a> {
    wb: &'a mut Workbook,
}

impl Drop for Scope<'_> {
    fn drop(&mut self) {
        let notes = &mut self.wb.notes;
        notes.depth -= 1;
        if notes.depth == 0 {
            notes.clear();
        }
    }
}

impl Workbook {
    pub fn command<R: Outcome>(&mut self, cmd: Command, f: impl FnOnce(&mut Workbook) -> R) -> R {
        if self.notes.depth == 0 {
            self.notes.active = Some(self.session.active);
            self.calc.cmd_manual = self.settings.mode == super::CalcMode::Manual;
            self.sbr_command(cmd);
        }
        self.notes.depth += 1;
        let scope = Scope { wb: self };
        let r = if scope.wb.notes.depth == 1 {
            crate::command_stack(|| f(scope.wb))
        } else {
            f(scope.wb)
        };
        let wb = &mut *scope.wb;
        if wb.notes.depth == 1 {
            wb.sbr_command_end();
            wb.notes.close_partial();
            let calcs = wb.pass_notices(rule(cmd).calc);
            let (mode, active) = (wb.settings.mode, wb.session.active);
            wb.notes.finish(cmd, r.succeeded(), mode, active, calcs);
        }
        r
    }

    fn pass_notices(&mut self, fires: bool) -> Vec<Option<Notice>> {
        let mut out = Vec::with_capacity(self.notes.passes.len());
        let mut pending = std::mem::take(&mut self.notes.pending);
        for i in 0..self.notes.passes.len() {
            if !fires {
                out.push(None);
                continue;
            }
            let p = &self.notes.passes[i];

            if p.silent {
                add_new(&mut pending, &p.sheets);
                out.push(None);
                continue;
            }
            add_new(&mut pending, &p.sheets);
            let sheets = self.by_name(std::mem::take(&mut pending));
            out.push((!sheets.is_empty()).then_some(Notice::Calculated { sheets }));
        }

        if fires && self.notes.passes.last().is_some_and(|p| p.silent) {
            let sheets = self.by_name(std::mem::take(&mut pending));
            out.push((!sheets.is_empty()).then_some(Notice::Calculated { sheets }));
        }
        self.notes.pending = self.by_name(pending);
        out
    }

    fn by_name(&self, mut sheets: Vec<SheetId>) -> Vec<SheetId> {
        sheets.retain(|&s| self.sheet(s).is_some());
        sheets.sort_by(|&a, &b| match (self.sheet(a), self.sheet(b)) {
            (Some(x), Some(y)) => crate::text::compare_ci(&x.name, &y.name),
            _ => std::cmp::Ordering::Equal,
        });
        sheets
    }

    pub fn unfired(&mut self, notices: &[Notice]) {
        for n in notices {
            if let Notice::Calculated { sheets } = n {
                add_new(&mut self.notes.pending, sheets);
            }
        }
    }

    pub fn note_changed(&mut self, sheet: SheetId, areas: &[Rect]) {
        self.slicer_reload_cells(sheet, areas);
        if self.notes.depth == 1 && !areas.is_empty() {
            self.notes.slot.push(Notice::Changed {
                sheet,
                areas: areas.to_vec(),
            });
        }
    }

    pub(crate) fn note_changed_early(&mut self, sheet: SheetId, area: Rect) {
        if self.notes.depth == 1 {
            self.notes.early.push(Notice::Changed {
                sheet,
                areas: vec![area],
            });
        }
    }

    pub(crate) fn note_sheet(&mut self, n: Notice) {
        if self.notes.depth == 1 {
            self.notes.slot.push(n);
        }
    }

    pub fn take_notices(&mut self) -> Vec<Notice> {
        std::mem::take(&mut self.notes.log)
    }

    pub fn notices(&self) -> &[Notice] {
        &self.notes.log
    }

    pub fn notices_mut(&mut self) -> &mut Vec<Notice> {
        &mut self.notes.log
    }
}

#[cfg(test)]
mod tests {

    use super::*;
    use crate::{FillType, ReplaceSpec, SortKey, SortSpec, Value};

    fn at(s: SheetId, r: u32, c: u32) -> CellAddr {
        CellAddr::new(s, r, c)
    }

    fn rect(r0: u32, c0: u32, r1: u32, c1: u32) -> Rect {
        Rect::new(r0, c0, r1, c1)
    }

    fn changed(sheet: SheetId, areas: &[Rect]) -> Notice {
        Notice::Changed {
            sheet,
            areas: areas.to_vec(),
        }
    }

    fn calculated(sheets: &[SheetId]) -> Notice {
        Notice::Calculated {
            sheets: sheets.to_vec(),
        }
    }

    fn no_change(n: &[Notice]) -> bool {
        !n.iter().any(|n| matches!(n, Notice::Changed { .. }))
    }

    fn book() -> (Workbook, SheetId) {
        let mut wb = Workbook::new();
        let s = wb.sheet_at(0).id;
        for r in 0..6 {
            wb.set_value(at(s, r, 0), Value::Number(f64::from(r + 1)));
        }
        wb.set_formula(at(s, 0, 1), "=SUM(A1:A6)").unwrap();
        wb.take_notices();
        (wb, s)
    }

    #[test]
    fn entry_calculates_before_change() {
        let (mut wb, s) = book();
        wb.set_value(at(s, 0, 0), Value::Number(9.0));
        let a1 = rect(0, 0, 0, 0);
        assert_eq!(
            wb.take_notices(),
            [calculated(&[s]), Notice::AfterCalculate, changed(s, &[a1])]
        );

        wb.set_input(at(s, 0, 2), "x").unwrap();
        assert_eq!(
            wb.take_notices(),
            [Notice::AfterCalculate, changed(s, &[rect(0, 2, 0, 2)])]
        );

        wb.set_array_formula(s, rect(10, 0, 11, 0), "=A1:A2")
            .unwrap();
        assert_eq!(wb.take_notices()[1], changed(s, &[rect(10, 0, 11, 0)]));
        assert!(wb.set_input(at(s, 10, 0), "1").is_err());
        assert!(no_change(&wb.take_notices()));
    }

    #[test]
    fn manual_entry_gives_only_change() {
        let (mut wb, s) = book();
        wb.set_mode(CalcMode::Manual);
        assert_eq!(wb.take_notices(), []);
        wb.set_value(at(s, 0, 0), Value::Number(9.0));
        wb.set_formula(at(s, 0, 3), "=A1+1").unwrap();
        assert_eq!(
            wb.take_notices(),
            [
                changed(s, &[rect(0, 0, 0, 0)]),
                changed(s, &[rect(0, 3, 0, 3)])
            ]
        );

        wb.set_mode(CalcMode::Automatic);
        assert_eq!(
            wb.take_notices(),
            [calculated(&[s]), Notice::AfterCalculate]
        );
    }

    #[test]
    fn clear_contents_calculates_before_changing() {
        let (mut wb, s) = book();
        let r = rect(1, 0, 2, 0);
        wb.clear_range(s, r).unwrap();
        assert_eq!(
            wb.take_notices(),
            [calculated(&[s]), changed(s, &[r]), Notice::AfterCalculate]
        );

        wb.set_number_format(s, r, "0.00");
        assert_eq!(wb.take_notices(), []);
        wb.clear_formats(s, r);
        assert_eq!(wb.take_notices()[0], changed(s, &[r]));
    }

    #[test]
    fn insert_changes_first_delete_calculates_first() {
        let (mut wb, s) = book();
        let row2 = rect(1, 0, 1, MAX_COLS - 1);
        wb.insert_rows(s, 1, 1).unwrap();
        assert_eq!(
            wb.take_notices(),
            [
                changed(s, &[row2]),
                calculated(&[s]),
                Notice::AfterCalculate
            ]
        );
        wb.delete_rows(s, 1, 1).unwrap();
        assert_eq!(
            wb.take_notices(),
            [
                calculated(&[s]),
                changed(s, &[row2]),
                Notice::AfterCalculate
            ]
        );
        wb.insert_cols(s, 9, 1).unwrap();
        assert_eq!(
            wb.take_notices()[0],
            changed(s, &[rect(0, 9, MAX_ROWS - 1, 9)])
        );
        let b34 = rect(2, 1, 3, 1);
        wb.delete_cells(s, b34, true).unwrap();
        let n = wb.take_notices();
        assert_eq!(n[n.len() - 2], changed(s, &[b34]));
    }

    #[test]
    fn fill_targets_leave_out_the_source() {
        let (mut wb, s) = book();
        wb.autofill(s, rect(2, 0, 2, 0), rect(2, 0, 7, 0), FillType::Copy)
            .unwrap();
        assert_eq!(
            wb.take_notices(),
            [
                calculated(&[s]),
                changed(s, &[rect(3, 0, 7, 0)]),
                Notice::AfterCalculate
            ]
        );
        wb.set_value(at(s, 0, 15), Value::Number(1.0));
        wb.take_notices();
        let spec = crate::SeriesSpec {
            rows: Some(false),
            ..Default::default()
        };
        wb.data_series(s, rect(0, 15, 4, 15), &spec).unwrap();

        assert_eq!(
            wb.take_notices(),
            [changed(s, &[rect(1, 15, 4, 15)]), Notice::AfterCalculate]
        );
    }

    #[test]
    fn cut_changes_source_then_destination() {
        let (mut wb, s) = book();
        wb.set_value(at(s, 4, 3), Value::Number(1.0));
        wb.take_notices();

        wb.move_range(s, rect(4, 3, 4, 3), at(s, 4, 5)).unwrap();
        assert_eq!(
            wb.take_notices(),
            [
                changed(s, &[rect(4, 3, 4, 3)]),
                changed(s, &[rect(4, 5, 4, 5)]),
                Notice::AfterCalculate
            ]
        );
        wb.copy_range(s, rect(0, 0, 2, 0), at(s, 0, 17)).unwrap();
        assert_eq!(wb.take_notices()[0], changed(s, &[rect(0, 17, 2, 17)]));
    }

    #[test]
    fn replace_gives_the_cells_it_changed() {
        let (mut wb, s) = book();
        let spec = ReplaceSpec {
            what: "6".into(),
            with: "8".into(),
            ..Default::default()
        };
        wb.replace_range(s, rect(0, 0, 7, 0), &spec).unwrap();
        assert_eq!(
            wb.take_notices(),
            [
                changed(s, &[rect(5, 0, 5, 0)]),
                calculated(&[s]),
                Notice::AfterCalculate
            ]
        );
    }

    #[test]
    fn sort_and_table_creation_give_no_change() {
        let (mut wb, s) = book();
        let spec = SortSpec {
            keys: vec![SortKey {
                index: 0,
                descending: true,
                color: None,
            }],
            ..Default::default()
        };
        wb.sort_range(s, rect(0, 0, 5, 0), &spec).unwrap();
        let n = wb.take_notices();
        assert!(no_change(&n) && n.last() == Some(&Notice::AfterCalculate));
        wb.set_input(at(s, 0, 12), "h").unwrap();
        wb.set_value(at(s, 1, 12), Value::Number(1.0));
        wb.take_notices();
        wb.add_table("T", s, rect(0, 12, 1, 12)).unwrap();
        assert_eq!(wb.take_notices(), []);
    }

    fn table_book() -> (Workbook, SheetId) {
        let (mut wb, s) = book();
        for (c, h) in [(10, "a"), (11, "b")] {
            wb.set_input(at(s, 0, c), h).unwrap();
            wb.set_value(at(s, 1, c), Value::Number(1.0));
            wb.set_value(at(s, 2, c), Value::Number(2.0));
        }
        wb.add_table("T", s, rect(0, 10, 2, 11)).unwrap();
        wb.take_notices();
        (wb, s)
    }

    fn changes(n: Vec<Notice>) -> Vec<Rect> {
        n.into_iter()
            .filter_map(|n| match n {
                Notice::Changed { areas, .. } => Some(areas[0]),
                _ => None,
            })
            .collect()
    }

    #[test]
    fn table_totals_and_headers_change_what_they_write() {
        let (mut wb, s) = table_book();
        let row = rect(3, 10, 3, 11);
        wb.set_table_totals("T", true).unwrap();
        assert_eq!(
            changes(wb.take_notices()),
            [row, rect(3, 10, 3, 10), rect(3, 11, 3, 11)]
        );

        wb.set_table_totals_calc("T", 1, 1).unwrap();
        wb.take_notices();
        wb.set_table_totals_calc("T", 1, 1).unwrap();
        assert_eq!(wb.take_notices(), []);

        wb.set_table_totals_calc("T", 1, 0).unwrap();
        assert_eq!(wb.take_notices(), [changed(s, &[rect(3, 11, 3, 11)])]);
        wb.set_table_totals_calc("T", 1, 0).unwrap();
        assert_eq!(wb.take_notices(), []);
        wb.set_table_totals_calc("T", 0, 0).unwrap();
        wb.take_notices();
        wb.set_table_totals_calc("T", 0, 0).unwrap();
        assert_eq!(wb.take_notices(), [changed(s, &[rect(3, 10, 3, 10)])]);
        wb.set_table_totals("T", false).unwrap();
        assert_eq!(changes(wb.take_notices()), [row]);

        let header = rect(0, 10, 0, 11);
        wb.set_table_headers("T", false).unwrap();
        assert_eq!(changes(wb.take_notices()), [header]);

        wb.set_table_headers("T", true).unwrap();
        assert_eq!(changes(wb.take_notices()), [header]);

        wb.set_table_headers("T", false).unwrap();
        wb.set_input(at(s, 0, 10), "x").unwrap();
        wb.take_notices();
        wb.set_table_headers("T", true).unwrap();
        let below = rect(1, 10, 1, 11);
        assert_eq!(changes(wb.take_notices()), [below, below]);
    }

    fn uncalculated(mut n: Vec<Notice>) -> Vec<Notice> {
        n.retain(|n| !matches!(n, Notice::Calculated { .. }));
        n
    }

    #[test]
    fn resize_enters_new_headers_first() {
        let (mut wb, s) = table_book();
        let m1 = rect(0, 12, 0, 12);
        let wide = rect(0, 10, 2, 12);

        wb.resize_table("T", wide).unwrap();
        assert_eq!(
            uncalculated(wb.take_notices()),
            [
                changed(s, &[m1]),
                Notice::AfterCalculate,
                changed(s, &[wide]),
                Notice::AfterCalculate
            ]
        );
        let narrow = rect(0, 10, 2, 11);
        wb.resize_table("T", narrow).unwrap();
        assert_eq!(
            uncalculated(wb.take_notices()),
            [
                Notice::AfterCalculate,
                changed(s, &[narrow]),
                Notice::AfterCalculate
            ]
        );

        wb.set_input(at(s, 0, 13), "n").unwrap();
        wb.set_value(at(s, 0, 14), Value::Number(7.0));
        wb.take_notices();
        let wider = rect(0, 10, 2, 14);
        wb.resize_table("T", wider).unwrap();
        assert_eq!(changes(wb.take_notices()), [m1, rect(0, 14, 0, 14), wider]);
    }

    #[test]
    fn calculate_fires_after_calculate_with_nothing_dirty() {
        let (mut wb, _) = book();
        wb.recalc();
        assert_eq!(wb.take_notices(), [Notice::AfterCalculate]);
    }

    #[test]
    fn calculated_sheets_in_name_order() {
        let (mut wb, s1) = book();
        let s2 = wb.add_sheet("S2").unwrap();
        let s3 = wb.add_sheet("S3").unwrap();
        wb.set_formula(at(s3, 0, 0), "=Sheet1!B1+1").unwrap();
        wb.set_formula(at(s2, 0, 0), "=S3!A1*2").unwrap();
        wb.set_formula(at(s1, 0, 1), "=A1").unwrap();
        wb.take_notices();

        wb.set_value(at(s1, 0, 0), Value::Number(5.0));
        assert_eq!(wb.take_notices()[0], calculated(&[s2, s3, s1]));

        wb.rename_sheet(s3, "a-b").unwrap();
        wb.rename_sheet(s2, "AB").unwrap();
        wb.take_notices();
        wb.set_value(at(s1, 0, 0), Value::Number(6.0));
        assert_eq!(wb.take_notices()[0], calculated(&[s2, s3, s1]));

        let fired = [
            "_x",
            "~t",
            "10",
            "9",
            "a",
            "A c",
            "\u{c3}\u{201e}",
            "ab",
            "a-b",
            "B",
            "b2",
            "e",
            "m",
            "S10",
            "S9",
            "Z",
        ];
        let mut names = fired;
        names.reverse();
        names.sort_by(|a, b| crate::text::compare_ci(a, b));
        assert_eq!(names, fired);
    }

    #[test]
    fn a_command_groups_nested_calls() {
        let (mut wb, s) = book();
        let block = rect(0, 4, 1, 5);
        wb.command(Command::Entry, |wb| {
            wb.note_changed(s, &[block]);
            for r in 0..2 {
                for c in 4..6 {
                    wb.set_value(at(s, r, c), Value::Number(7.0));
                }
            }
        });
        assert_eq!(
            wb.take_notices(),
            [Notice::AfterCalculate, changed(s, &[block])]
        );
        assert!(wb.notes.idle());

        let p = std::panic::catch_unwind(std::panic::AssertUnwindSafe(|| {
            wb.command(Command::Entry, |_| -> () { panic!("inside a command") })
        }));
        assert!(p.is_err());
        assert!(wb.notes.idle());
        assert_eq!(wb.take_notices(), []);
    }

    #[test]
    fn sheet_notices() {
        let (mut wb, s1) = book();
        let s2 = wb.add_sheet("S2").unwrap();
        assert_eq!(
            wb.take_notices(),
            [
                Notice::SheetAdded(s2),
                Notice::SheetActivated {
                    sheet: s2,
                    previous: s1
                }
            ]
        );
        wb.set_active_sheet(s1).unwrap();
        assert_eq!(
            wb.take_notices(),
            [Notice::SheetActivated {
                sheet: s1,
                previous: s2
            }]
        );
        wb.delete_sheet(s2).unwrap();

        assert_eq!(
            wb.take_notices(),
            [
                Notice::Calculated { sheets: vec![s1] },
                Notice::SheetDeleting(s2),
                Notice::AfterCalculate
            ]
        );
        assert!(wb.notes.idle());
    }
}
