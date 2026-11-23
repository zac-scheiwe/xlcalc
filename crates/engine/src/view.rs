use crate::workbook::Command;
use std::collections::{BTreeMap, BTreeSet};

use crate::address::{CellAddr, Rect, SheetId, MAX_COLS, MAX_ROWS};
use crate::calc::{self, RecalcReport};
use crate::eval::EvalCtx;
use crate::functions::criteria::crit::{Criterion, Flavor, Pattern};
use crate::value::Value;
use crate::workbook::{EditError, Workbook};

pub const VALUES_SEP: char = '\u{1}';

#[derive(Clone, Debug, Default)]
pub struct ColCrit {
    pub c1: Option<String>,

    pub op: Option<String>,
    pub c2: Option<String>,

    pub op_code: i32,

    pub month_first: bool,
}

#[derive(Clone, Debug)]
pub struct Filter {
    pub rect: Rect,

    pub anchor_r1: u32,
    pub table: Option<String>,

    pub crit: BTreeMap<u32, ColCrit>,

    pub total_row: bool,
}

#[derive(Clone, Debug, Default)]
pub struct SheetView {
    pub hidden_rows: BTreeSet<u32>,

    pub hbreaks: BTreeSet<u32>,
    pub vbreaks: BTreeSet<u32>,

    pub row_h: BTreeMap<u32, crate::rowheight::RowHt>,

    pub std_frozen: Option<u32>,

    pub table_rows: BTreeSet<u32>,
    pub hidden_cols: BTreeSet<u32>,

    pub col_px: BTreeMap<u32, (u32, bool)>,

    pub std_cw: Option<f64>,

    pub filters: Vec<Filter>,

    pub filter_sorted: bool,

    pub levels: BTreeMap<u32, u8>,

    pub col_levels: BTreeMap<u32, u8>,

    pub summary_above: bool,
    pub summary_left: bool,

    pub auto_styles: bool,

    pub used_r1: Option<u32>,

    pub fm: bool,

    pub adv: Option<Rect>,

    pub fm_last: bool,

    pub sel_table: Option<String>,
}

impl Workbook {
    pub fn row_hidden(&self, sheet: SheetId, row: u32) -> bool {
        self.views
            .get(&sheet)
            .is_some_and(|v| v.hidden_rows.contains(&row))
    }

    pub fn col_hidden(&self, sheet: SheetId, col: u32) -> bool {
        self.views
            .get(&sheet)
            .is_some_and(|v| v.hidden_cols.contains(&col))
    }

    pub fn hidden_rows(&self, sheet: SheetId) -> Vec<u32> {
        self.views
            .get(&sheet)
            .map(|v| v.hidden_rows.iter().copied().collect())
            .unwrap_or_default()
    }

    pub fn hidden_cols(&self, sheet: SheetId) -> Vec<u32> {
        self.views
            .get(&sheet)
            .map(|v| v.hidden_cols.iter().copied().collect())
            .unwrap_or_default()
    }

    pub fn row_level(&self, sheet: SheetId, row: u32) -> u32 {
        self.row_outline_level(sheet, row)
    }

    pub fn row_outline_level(&self, sheet: SheetId, row: u32) -> u32 {
        self.views
            .get(&sheet)
            .and_then(|v| v.levels.get(&row))
            .map_or(1, |l| u32::from(*l) + 1)
    }

    pub fn filter_mode(&self, sheet: SheetId) -> bool {
        self.views.get(&sheet).is_some_and(|v| v.fm)
    }

    pub fn autofilter_object_mode(&self, sheet: SheetId, table: Option<&str>) -> bool {
        let (r, c) = self.sheet_sel(sheet);
        let at = table.map(str::to_string).or_else(|| {
            self.table_at(CellAddr::new(sheet, r, c))
                .filter(|t| t.headers && t.show_filter)
                .map(|t| t.name.clone())
        });
        match at {
            Some(t) => self.table_filter_has_crit(sheet, &t),
            None => self.range_filter_has_crit(sheet),
        }
    }

    pub(crate) fn filter_mode_for_subtotal(&self, sheet: SheetId) -> bool {
        self.views.get(&sheet).is_some_and(|v| {
            v.fm || v.filters.iter().any(|f| {
                !f.crit.is_empty()
                    && f.table
                        .as_ref()
                        .is_none_or(|t| self.table_by_name(t).is_some())
            })
        })
    }

    fn range_filter_has_crit(&self, sheet: SheetId) -> bool {
        self.views.get(&sheet).is_some_and(|v| {
            v.filters
                .iter()
                .any(|f| f.table.is_none() && !f.crit.is_empty())
        })
    }

    fn table_filter_has_crit(&self, sheet: SheetId, name: &str) -> bool {
        self.views.get(&sheet).is_some_and(|v| {
            v.filters.iter().any(|f| {
                f.table
                    .as_deref()
                    .is_some_and(|t| t.eq_ignore_ascii_case(name))
                    && !f.crit.is_empty()
            })
        })
    }

    fn sheet_has_maf(&self, sheet: SheetId) -> bool {
        self.views
            .get(&sheet)
            .is_some_and(|v| !v.filters.is_empty())
            || self
                .tables
                .iter()
                .any(|t| t.sheet == sheet && t.headers && t.show_filter)
    }

    pub(crate) fn view_ins_del_ez_filter(&mut self, sheet: SheetId) {
        if self
            .views
            .get(&sheet)
            .is_some_and(|v| v.filters.iter().any(|f| f.table.is_none()))
        {
            let has = self.range_filter_has_crit(sheet);
            self.set_filter_mode(sheet, has);
        }
    }

    pub(crate) fn set_filter_mode(&mut self, sheet: SheetId, on: bool) {
        if on || self.views.contains_key(&sheet) {
            self.view_mut(sheet).fm = on;
        }
    }

    fn fm_after_apply(&mut self, sheet: SheetId, table: Option<&str>, active_inside: bool) {
        let has = match table {
            Some(t) => self.table_filter_has_crit(sheet, t),
            None => self.range_filter_has_crit(sheet),
        };
        if !has {
            self.set_filter_mode(sheet, false);
        } else if active_inside {
            self.set_filter_mode(sheet, true);
        }
    }

    pub fn autofilter_mode(&self, sheet: SheetId) -> bool {
        self.views
            .get(&sheet)
            .is_some_and(|v| v.filters.iter().any(|f| f.table.is_none()))
    }

    pub(crate) fn view_mut(&mut self, sheet: SheetId) -> &mut SheetView {
        self.views.entry(sheet).or_default()
    }

    pub fn set_subtotal_levels(&mut self, sheet: SheetId, r0: u32, r1: u32, level: Option<u8>) {
        if level.is_none() && !self.views.contains_key(&sheet) {
            return;
        }
        let v = self.view_mut(sheet);
        for r in r0..=r1 {
            match level {
                Some(l) if l > 0 => {
                    v.levels.insert(r, l);
                }
                _ => {
                    v.levels.remove(&r);
                }
            }
        }
    }

    fn view_touch_row(&mut self, sheet: SheetId, row: u32) {
        let v = self.view_mut(sheet);
        v.used_r1 = Some(v.used_r1.map_or(row, |r| r.max(row)));
    }

    pub(crate) fn view_touch_row_pub(&mut self, sheet: SheetId, row: u32) {
        if self.sheet(sheet).is_some() {
            self.view_touch_row(sheet, row.min(MAX_ROWS - 1));
        }
    }

    pub(crate) fn view_notify_sel(&mut self, sheet: SheetId, row: u32, col: u32) {
        let t = self
            .table_at(CellAddr::new(sheet, row, col))
            .map(|t| t.name.clone());
        let cur = self.views.get(&sheet).and_then(|v| v.sel_table.clone());
        if t == cur {
            return;
        }

        let flipped = t.clone().or(cur);
        self.view_mut(sheet).sel_table = t.clone();
        if let Some(name) = flipped {
            self.fm_update_list(sheet, &name, t.is_some());
        }
    }

    fn fm_update_list(&mut self, sheet: SheetId, name: &str, entered: bool) {
        let trect = self.table_by_name(name).map(|t| t.rect);
        let adv = self.views.get(&sheet).and_then(|v| v.adv);
        if let (Some(a), Some(tr)) = (adv, trect) {
            if a.intersects(&tr) {
                return;
            }
        }
        let on = if entered {
            self.table_filter_has_crit(sheet, name)
        } else {
            self.sheet_has_maf(sheet) && (self.range_filter_has_crit(sheet) || adv.is_some())
        };
        self.set_filter_mode(sheet, on);
    }

    pub(crate) fn view_leave_tables(&mut self, sheet: SheetId) -> bool {
        let Some(name) = self.views.get_mut(&sheet).and_then(|v| v.sel_table.take()) else {
            return false;
        };
        self.fm_update_list(sheet, &name, false);
        true
    }

    pub(crate) fn view_table_rows_changed(&mut self, sheet: SheetId, name: &str) {
        let Some(rect) = self.table_by_name(name).map(|t| t.rect) else {
            return;
        };
        let (r, c) = self.sheet_sel(sheet);
        let inside = rect.contains(r, c);
        let flag = self
            .views
            .get(&sheet)
            .and_then(|v| v.sel_table.as_deref())
            .is_some_and(|t| t.eq_ignore_ascii_case(name));
        if inside == flag {
            return;
        }
        self.view_mut(sheet).sel_table = inside.then(|| name.to_string());
        self.fm_update_list(sheet, name, inside);
    }

    pub(crate) fn sheet_sel(&self, sheet: SheetId) -> (u32, u32) {
        self.session
            .selection
            .get(&sheet)
            .map_or((0, 0), |s| s.active)
    }

    pub(crate) fn view_clipboard_op(&mut self, dst: SheetId, row: u32, col: u32) {
        self.view_notify_sel(dst, row, col);
        if self.table_at(CellAddr::new(dst, row, col)).is_some() {
            let (r, c) = self.sheet_sel(dst);
            self.view_notify_sel(dst, r, c);
        }
    }

    pub(crate) fn view_sorted(&mut self, sheet: SheetId, rect: Rect) {
        self.view_notify_sel(sheet, rect.r0, rect.c0);
        if sheet == self.session.active {
            let (r, c) = self.sheet_sel(sheet);
            self.view_notify_sel(sheet, r, c);
        }
    }

    pub(crate) fn view_typed(&mut self, addr: CellAddr) {
        let n = self.views.get(&addr.sheet).map_or(0, |v| v.filters.len());
        for i in 0..n {
            let f = &self.views[&addr.sheet].filters[i];
            if f.table.is_some() || addr.row <= f.rect.r1 {
                continue;
            }
            let r1 = self.filter_area_r1(addr.sheet, f);
            if r1 > f.rect.r1 {
                self.view_mut(addr.sheet).filters[i].rect.r1 = r1;
            }
        }
    }

    pub(crate) fn view_touch_paste(&mut self, sheet: SheetId, src: Rect, dst: Rect) {
        let h = src.rows().max(1);
        let end = if dst.rows() >= h && dst.rows().is_multiple_of(h) {
            dst.r1
        } else {
            dst.r0 + h - 1
        };
        self.view_touch_row_pub(sheet, end);
    }

    pub(crate) fn view_used_r1(&self, sheet: SheetId) -> Option<u32> {
        let cells = self
            .sheet(sheet)
            .and_then(|sh| sh.cells.sticky_bounds())
            .map(|b| b.r1);
        let rows = self.views.get(&sheet).and_then(|v| v.used_r1);
        match (cells, rows) {
            (Some(a), Some(b)) => Some(a.max(b)),
            (a, b) => a.or(b),
        }
    }

    pub(crate) fn mark_area_dependents(&mut self, sheet: SheetId, area: (u32, u32, u32, u32)) {
        let (r0, c0, r1, c1) = area;
        if r0 > r1 || c0 > c1 {
            return;
        }
        let area = Rect::new(r0, c0, r1, c1);
        let mut deps = Vec::new();
        self.graph.dependents_of_rect(sheet, &area, &mut deps);
        deps.sort_unstable();
        deps.dedup();
        for fid in deps {
            calc::mark_dirty(self, fid);
        }
    }

    pub(crate) fn mark_filter_area(&mut self, sheet: SheetId, filter: Rect) {
        let Some(used) = self.ever_used(sheet) else {
            return;
        };
        self.mark_area_dependents(sheet, (filter.r0 + 1, used.c0, filter.r1, used.c1));
    }

    pub(crate) fn rows_visibility_changed(&mut self, sheet: SheetId, rows: &[u32]) {
        if rows.is_empty() {
            return;
        }
        let mut deps = Vec::new();
        let mut i = 0;
        while i < rows.len() {
            let mut j = i;
            while j + 1 < rows.len() && rows[j + 1] == rows[j] + 1 {
                j += 1;
            }
            self.graph.dependents_of_rect(
                sheet,
                &Rect::new(rows[i], 0, rows[j], crate::address::MAX_COLS - 1),
                &mut deps,
            );
            i = j + 1;
        }
        deps.sort_unstable();
        deps.dedup();
        for fid in deps {
            if crate::functions::criteria::subtotal::is_subtotal_formula(self, fid) {
                calc::mark_dirty(self, fid);
            }
        }
    }

    pub(crate) fn view_settle(&mut self) -> RecalcReport {
        let sheets: Vec<SheetId> = self.views.keys().copied().collect();
        for s in sheets {
            let fm = self.filter_mode_for_subtotal(s);
            let v = self.view_mut(s);
            if v.fm_last != fm {
                v.fm_last = fm;
                let hidden: Vec<u32> = v.hidden_rows.iter().copied().collect();
                self.rows_visibility_changed(s, &hidden);
            }
        }
        self.after_edit(Vec::new())
    }

    pub fn set_rows_hidden(
        &mut self,
        sheet: SheetId,
        at: u32,
        count: u32,
        hide: bool,
    ) -> Result<RecalcReport, EditError> {
        self.command(Command::View, |wb| {
            wb.set_rows_hidden_run(sheet, at, count, hide)
        })
    }

    fn set_rows_hidden_run(
        &mut self,
        sheet: SheetId,
        at: u32,
        count: u32,
        hide: bool,
    ) -> Result<RecalcReport, EditError> {
        if self.sheet(sheet).is_none() {
            return Err(EditError::NoSuchSheet);
        }
        let last = (at + count).min(MAX_ROWS) - 1;
        if !hide && self.filter_mode(sheet) {
            let v = self.view_mut(sheet);
            if (at..=last).any(|r| !v.hidden_rows.contains(&r)) {
                return Ok(self.view_settle());
            }
        }
        if hide {
            self.view_touch_row(sheet, last);
            self.mark_rows(sheet, at, last, false);
        }
        let mut changed = Vec::new();
        {
            let v = self.view_mut(sheet);
            for r in at..=last {
                let was = v.hidden_rows.contains(&r);
                if was != hide {
                    changed.push(r);
                    if hide {
                        v.hidden_rows.insert(r);
                    } else {
                        v.hidden_rows.remove(&r);
                    }
                }
            }
        }

        self.mark_area_dependents(sheet, (at, 0, last, crate::address::MAX_COLS - 1));
        self.rows_visibility_changed(sheet, &changed);
        Ok(self.view_settle())
    }

    pub(crate) fn show_rows(&mut self, sheet: SheetId, r0: u32, r1: u32) {
        let Some(v) = self.views.get_mut(&sheet) else {
            return;
        };
        let shown: Vec<u32> = v.hidden_rows.range(r0..=r1).copied().collect();
        for r in &shown {
            v.hidden_rows.remove(r);
        }
        if !shown.is_empty() {
            self.rows_visibility_changed(sheet, &shown);
        }
    }

    pub fn set_cols_hidden(&mut self, sheet: SheetId, at: u32, count: u32, hide: bool) {
        self.command(Command::View, |wb| {
            wb.set_cols_hidden_run(sheet, at, count, hide)
        })
    }

    fn set_cols_hidden_run(&mut self, sheet: SheetId, at: u32, count: u32, hide: bool) {
        let last = (at + count).min(crate::address::MAX_COLS) - 1;
        let v = self.view_mut(sheet);
        for c in at..=last {
            if hide {
                v.hidden_cols.insert(c);
            } else {
                v.hidden_cols.remove(&c);
            }
        }
    }

    pub(crate) fn view_move_rows(&mut self, sheet: SheetId, moves: &[(u32, u32)]) {
        let v = self.view_mut(sheet);
        let old: Vec<(Option<u8>, bool)> = moves
            .iter()
            .map(|&(s, _)| (v.levels.get(&s).copied(), v.hidden_rows.contains(&s)))
            .collect();
        let mut changed = Vec::new();
        for (&(_, d), (l, h)) in moves.iter().zip(old) {
            match l {
                Some(l) => v.levels.insert(d, l),
                None => v.levels.remove(&d),
            };
            if v.hidden_rows.contains(&d) != h {
                changed.push(d);
                if h {
                    v.hidden_rows.insert(d);
                } else {
                    v.hidden_rows.remove(&d);
                }
            }
        }
        changed.sort_unstable();
        self.rows_visibility_changed(sheet, &changed);
    }

    pub fn group_rows(
        &mut self,
        sheet: SheetId,
        at: u32,
        count: u32,
        group: bool,
    ) -> Result<RecalcReport, EditError> {
        self.group_axis(sheet, true, at, count, group)
    }

    pub fn group_axis(
        &mut self,
        sheet: SheetId,
        rows: bool,
        at: u32,
        count: u32,
        group: bool,
    ) -> Result<RecalcReport, EditError> {
        self.command(Command::View, |wb| {
            wb.group_axis_run(sheet, rows, at, count, group)
        })
    }

    fn group_axis_run(
        &mut self,
        sheet: SheetId,
        rows: bool,
        at: u32,
        count: u32,
        group: bool,
    ) -> Result<RecalcReport, EditError> {
        let limit = if rows { MAX_ROWS } else { MAX_COLS };
        let last = (at + count).min(limit) - 1;
        if group && rows {
            let touch = if self.view_mut(sheet).summary_above {
                at.saturating_sub(1)
            } else {
                (last + 1).min(MAX_ROWS - 1)
            };
            self.view_touch_row(sheet, touch);
        }
        let v = self.view_mut(sheet);
        let levels = if rows {
            &mut v.levels
        } else {
            &mut v.col_levels
        };
        if group && (at..=last).any(|i| levels.get(&i).is_some_and(|&l| l >= 7)) {
            return Err(EditError::Compile("cannot group more levels".into()));
        }

        if !group && !(at..=last).any(|i| levels.contains_key(&i)) {
            return Err(EditError::Compile("Ungroup method failed".into()));
        }
        for i in at..=last {
            let l = levels.get(&i).copied().unwrap_or(0);
            if group {
                levels.insert(i, l + 1);
            } else if l > 1 {
                levels.insert(i, l - 1);
            } else {
                levels.remove(&i);
            }
        }
        Ok(RecalcReport::default())
    }

    pub fn col_outline_level(&self, sheet: SheetId, col: u32) -> u32 {
        self.views
            .get(&sheet)
            .and_then(|v| v.col_levels.get(&col))
            .map_or(1, |l| u32::from(*l) + 1)
    }

    pub fn set_outline_levels(
        &mut self,
        sheet: SheetId,
        rows: bool,
        at: u32,
        count: u32,
        level: u32,
    ) {
        let limit = if rows { MAX_ROWS } else { MAX_COLS };
        let last = (at + count).min(limit) - 1;
        if rows {
            self.view_touch_row(sheet, last);
        }
        let v = self.view_mut(sheet);
        let levels = if rows {
            &mut v.levels
        } else {
            &mut v.col_levels
        };
        for i in at..=last {
            if level <= 1 {
                levels.remove(&i);
            } else {
                levels.insert(i, (level - 1) as u8);
            }
        }
    }

    pub fn clear_outline(&mut self, sheet: SheetId, rect: Rect) -> RecalcReport {
        self.command(Command::View, |wb| {
            {
                let v = wb.view_mut(sheet);
                let rows: Vec<u32> = v.levels.range(rect.r0..=rect.r1).map(|(r, _)| *r).collect();
                for r in rows {
                    v.levels.remove(&r);
                    v.hidden_rows.remove(&r);
                }
                let cols: Vec<u32> = v
                    .col_levels
                    .range(rect.c0..=rect.c1)
                    .map(|(c, _)| *c)
                    .collect();
                for c in cols {
                    v.col_levels.remove(&c);
                    v.hidden_cols.remove(&c);
                }
            }
            RecalcReport::default()
        })
    }

    pub fn outline_show(&mut self, sheet: SheetId, level: u32) -> RecalcReport {
        self.outline_show_levels(sheet, level, 0)
    }

    pub fn outline_show_levels(&mut self, sheet: SheetId, rows: u32, cols: u32) -> RecalcReport {
        self.command(Command::View, |wb| wb.outline_show_run(sheet, rows, cols))
    }

    fn outline_show_run(&mut self, sheet: SheetId, level: u32, cols: u32) -> RecalcReport {
        let mut changed = Vec::new();
        {
            let v = self.view_mut(sheet);
            if level > 0 {
                let rows: Vec<(u32, u8)> = v.levels.iter().map(|(r, l)| (*r, *l)).collect();
                for (r, l) in rows {
                    let hide = 1 + u32::from(l) > level;
                    if v.hidden_rows.contains(&r) != hide {
                        changed.push(r);
                        if hide {
                            v.hidden_rows.insert(r);
                        } else {
                            v.hidden_rows.remove(&r);
                        }
                    }
                }
            }
            if cols > 0 {
                let cs: Vec<(u32, u8)> = v.col_levels.iter().map(|(c, l)| (*c, *l)).collect();
                for (c, l) in cs {
                    if 1 + u32::from(l) > cols {
                        v.hidden_cols.insert(c);
                    } else {
                        v.hidden_cols.remove(&c);
                    }
                }
            }
        }

        if level > 0 {
            if let Some(used) = self.ever_used(sheet) {
                self.mark_area_dependents(sheet, (0, used.c0, used.r1, used.c1));
            }
        }
        self.rows_visibility_changed(sheet, &changed);
        self.view_settle()
    }

    fn outline_detail(&self, sheet: SheetId, rows: bool, idx: u32) -> Vec<u32> {
        let Some(v) = self.views.get(&sheet) else {
            return Vec::new();
        };
        let (levels, back) = if rows {
            (&v.levels, !v.summary_above)
        } else {
            (&v.col_levels, !v.summary_left)
        };
        let lv = |i: u32| levels.get(&i).map_or(1, |l| u32::from(*l) + 1);
        let limit = if rows { MAX_ROWS } else { MAX_COLS };
        let base = lv(idx);
        let mut out = Vec::new();
        let mut i = idx;
        loop {
            i = if back {
                match i.checked_sub(1) {
                    Some(j) => j,
                    None => break,
                }
            } else if i + 1 < limit {
                i + 1
            } else {
                break;
            };
            if lv(i) <= base {
                break;
            }
            out.push(i);
        }
        out
    }

    pub fn detail_shown(&self, sheet: SheetId, rows: bool, idx: u32) -> bool {
        let detail = self.outline_detail(sheet, rows, idx);
        let (Some(&near), Some(v)) = (detail.first(), self.views.get(&sheet)) else {
            return true;
        };
        !if rows {
            v.hidden_rows.contains(&near)
        } else {
            v.hidden_cols.contains(&near)
        }
    }

    pub fn set_detail_shown(
        &mut self,
        sheet: SheetId,
        rows: bool,
        idx: u32,
        show: bool,
    ) -> RecalcReport {
        self.command(Command::View, |wb| {
            let detail = wb.outline_detail(sheet, rows, idx);
            let mut changed = Vec::new();
            if let Some(&far) = detail.last() {
                if rows && !show {
                    wb.view_touch_row(sheet, far.max(idx));
                }
                let v = wb.view_mut(sheet);
                let level = |v: &SheetView, i: u32| {
                    let m = if rows { &v.levels } else { &v.col_levels };
                    m.get(&i).map_or(1, |l| u32::from(*l) + 1)
                };
                let base = level(v, idx);
                for i in detail {
                    if show && level(v, i) != base + 1 {
                        continue;
                    }
                    let set = if rows {
                        &mut v.hidden_rows
                    } else {
                        &mut v.hidden_cols
                    };
                    let was = set.contains(&i);
                    if was != !show {
                        if show {
                            set.remove(&i);
                        } else {
                            set.insert(i);
                        }
                        if rows {
                            changed.push(i);
                        }
                    }
                }
                changed.sort_unstable();
            }
            wb.rows_visibility_changed(sheet, &changed);
            wb.view_settle()
        })
    }

    pub fn outline_settings(&self, sheet: SheetId) -> (bool, bool, bool) {
        self.views.get(&sheet).map_or((false, false, false), |v| {
            (v.summary_above, v.summary_left, v.auto_styles)
        })
    }

    pub fn set_outline_settings(&mut self, sheet: SheetId, above: bool, left: bool, styles: bool) {
        let v = self.view_mut(sheet);
        v.summary_above = above;
        v.summary_left = left;
        v.auto_styles = styles;
    }

    fn nonblank(&self, sheet: SheetId, r: u32, c: u32) -> bool {
        !self.value(CellAddr::new(sheet, r, c)).is_blank()
    }

    pub fn current_region(&self, sheet: SheetId, rect: Rect) -> Rect {
        self.current_region_in(sheet, rect, 0, crate::address::MAX_COLS - 1)
    }

    fn current_region_in(&self, sheet: SheetId, rect: Rect, lo: u32, hi: u32) -> Rect {
        let mut b = rect;
        loop {
            let mut grew = false;
            let (r0, r1, c0, c1) = (
                b.r0.saturating_sub(1),
                (b.r1 + 1).min(MAX_ROWS - 1),
                b.c0.saturating_sub(1).max(lo),
                (b.c1 + 1).min(hi),
            );
            let any = |rr: (u32, u32), cc: (u32, u32)| {
                (rr.0..=rr.1).any(|r| (cc.0..=cc.1).any(|c| self.nonblank(sheet, r, c)))
            };
            if b.r0 > 0 && any((b.r0 - 1, b.r0 - 1), (c0, c1)) {
                b.r0 -= 1;
                grew = true;
            }
            if b.r1 + 1 < MAX_ROWS && any((b.r1 + 1, b.r1 + 1), (c0, c1)) {
                b.r1 += 1;
                grew = true;
            }
            if b.c0 > lo && any((r0, r1), (b.c0 - 1, b.c0 - 1)) {
                b.c0 -= 1;
                grew = true;
            }
            if b.c1 < hi && any((r0, r1), (b.c1 + 1, b.c1 + 1)) {
                b.c1 += 1;
                grew = true;
            }
            if !grew {
                return b;
            }
        }
    }

    pub(crate) fn filter_rect(&self, f: &Filter) -> Option<Rect> {
        match &f.table {
            None => Some(f.rect),
            Some(name) => {
                let t = self.table_by_name(name)?;
                if !t.headers {
                    return None;
                }
                let mut r = t.rect;
                if t.totals {
                    r.r1 -= 1;
                }
                Some(r)
            }
        }
    }

    fn cell_is_date(&self, addr: CellAddr) -> bool {
        crate::numfmt::NumberFormat::parse(self.number_format(addr)).is_ok_and(|f| f.is_date())
    }

    fn field_pass(
        &self,
        sheet: SheetId,
        rect: Rect,
        field: u32,
        cc: &ColCrit,
        check_r1: Option<u32>,
    ) -> Option<Vec<bool>> {
        let col = rect.c0 + field;
        let rows = rect.r0 + 1..=rect.r1;
        let vals: Vec<Value> = rows
            .clone()
            .map(|r| self.value(CellAddr::new(sheet, r, col)))
            .collect();
        let op = cc.op.as_deref();
        if let Some(op @ ("top10" | "bottom10" | "top10pct" | "bottom10pct")) = op {
            let n: f64 = cc.c1.as_deref().and_then(|s| s.trim().parse().ok())?;
            if !(1.0..=500.0).contains(&n) {
                return None;
            }
            let err_at = vals.iter().position(|v| matches!(v, Value::Error(_)));
            if let Some(i) = err_at {
                let row = rect.r0 + 1 + i as u32;
                if check_r1.is_some_and(|c| row <= c) {
                    return None;
                }

                return Some(rows.map(|r| !self.row_hidden(sheet, r)).collect());
            }
            let mut nums: Vec<f64> = vals
                .iter()
                .filter_map(|v| {
                    if let Value::Number(x) = v {
                        Some(*x)
                    } else {
                        None
                    }
                })
                .collect();
            let top = op.starts_with("top");
            if top {
                nums.sort_by(|a, b| b.total_cmp(a));
            } else {
                nums.sort_by(|a, b| a.total_cmp(b));
            }
            if nums.is_empty() {
                return None;
            }
            let k = if op.ends_with("pct") {
                ((nums.len() as f64 * n / 100.0).floor() as usize).max(1)
            } else {
                n.floor() as usize
            }
            .min(nums.len());
            let thr = nums[k - 1];
            return Some(
                vals.iter()
                    .map(|v| match v {
                        Value::Number(x) => {
                            if top {
                                *x >= thr
                            } else {
                                *x <= thr
                            }
                        }
                        _ => false,
                    })
                    .collect(),
            );
        }
        if op == Some("dynamic") {
            let code: u32 = cc.c1.as_deref().and_then(|s| s.trim().parse().ok())?;
            let nums: Vec<f64> = vals
                .iter()
                .filter_map(|v| {
                    if let Value::Number(x) = v {
                        Some(*x)
                    } else {
                        None
                    }
                })
                .collect();

            let chk = check_r1.map(|c| if c <= rect.r0 { rect.r1 } else { c });
            let err_rows = || {
                vals.iter()
                    .enumerate()
                    .filter(|(_, v)| matches!(v, Value::Error(_)))
                    .map(|(i, _)| rect.r0 + 1 + i as u32)
            };
            let refused = nums.is_empty() || err_rows().any(|r| chk.is_some_and(|c| r <= c));
            return match code {
                33 | 34 if check_r1.is_some() && refused => None,
                33 | 34 => {
                    let mean = if err_rows().next().is_some() {
                        0.0
                    } else {
                        nums.iter().sum::<f64>() / nums.len().max(1) as f64
                    };
                    Some(
                        vals.iter()
                            .map(|v| match v {
                                Value::Number(x) => {
                                    (code == 33 && *x > mean) || (code == 34 && *x < mean)
                                }
                                _ => false,
                            })
                            .collect(),
                    )
                }
                17..=32 => {
                    let sys = self.settings.date_system;
                    Some(
                        vals.iter()
                            .enumerate()
                            .map(|(i, v)| match v {
                                Value::Number(x)
                                    if *x >= 0.0
                                        && self.cell_is_date(CellAddr::new(
                                            sheet,
                                            rect.r0 + 1 + i as u32,
                                            col,
                                        )) =>
                                {
                                    let (_, m, _) =
                                        crate::datetime::ymd_from_serial(sys, x.floor() as i64);
                                    if code <= 20 {
                                        (m - 1) / 3 + 17 == code
                                    } else {
                                        m + 20 == code
                                    }
                                }
                                _ => false,
                            })
                            .collect(),
                    )
                }
                _ => None,
            };
        }
        let host = CellAddr::new(sheet, rect.r0, rect.c0);
        let ctx = EvalCtx::new(self, host);

        let us = crate::locale::Locale::en_us();
        let mut us_ctx = EvalCtx::new(self, host);
        us_ctx.locale = &us;

        let trim = |s: &str| s.trim_matches(' ').to_string();
        let crit = |s: &str, v: &Value, r: u32| -> bool {
            let (op, rest) = ["<=", ">=", "<>", "<", ">", "="]
                .iter()
                .find_map(|p| s.strip_prefix(p).map(|r| (*p, r)))
                .unwrap_or(("=", s.trim_matches(' ')));
            let shown = trim(&self.filter_text(CellAddr::new(sheet, r, col)));
            let blank_rest = rest.trim_matches(' ').is_empty();
            if matches!(op, "=" | "<>") && blank_rest {
                return (v.is_blank() || shown.is_empty()) == (op == "=");
            }

            if op == "=" && !Pattern::has_wildcards(rest) {
                let literal = ctx.to_number(&Value::from(rest)).is_ok()
                    || rest.eq_ignore_ascii_case("TRUE")
                    || rest.eq_ignore_ascii_case("FALSE")
                    || rest.starts_with('#');
                if literal || !matches!(v, Value::Text(_)) {
                    return crate::text::eq_ci(&shown, rest);
                }
            }
            let v = match v {
                Value::Text(_) => Value::from(shown.as_str()),
                v => v.clone(),
            };
            let ctx = if op == "=" || !cc.month_first {
                &ctx
            } else {
                &us_ctx
            };
            match Criterion::parse(ctx, &Value::from(format!("{op}{rest}")), Flavor::Ifs) {
                Ok(c) => c.matches(ctx, &v),
                Err(_) => false,
            }
        };
        let pass = |i: usize, r: u32| -> bool {
            let v = &vals[i];
            if op == Some("values") {
                let list = cc.c1.as_deref().unwrap_or("");
                return list.split(VALUES_SEP).any(|item| {
                    let item = item.trim_matches(' ');
                    if item.is_empty() || item == "=" {
                        return v.is_blank()
                            || trim(&self.filter_text(CellAddr::new(sheet, r, col))).is_empty();
                    }
                    if item.starts_with(['=', '<', '>']) || item.contains(['*', '?']) {
                        return crit(item, v, r);
                    }
                    let shown = trim(&self.filter_text(CellAddr::new(sheet, r, col)));
                    crate::text::eq_ci(&shown, item)
                });
            }
            let a = cc.c1.as_deref().is_none_or(|c| crit(c, v, r));
            match (op, &cc.c2) {
                (Some("or"), Some(c2)) => a || crit(c2, v, r),
                (Some("and"), Some(c2)) => a && crit(c2, v, r),
                _ => a,
            }
        };
        Some(rows.enumerate().map(|(i, r)| pass(i, r)).collect())
    }

    pub(crate) fn refilter(
        &mut self,
        sheet: SheetId,
        check_r1: Option<u32>,
        only_hide: Option<(usize, u32)>,
        given: Option<Rect>,
    ) -> Result<(), ()> {
        let Some(v) = self.views.get(&sheet) else {
            return Ok(());
        };
        let filters = v.filters.clone();
        let mut set: Vec<(u32, bool)> = Vec::new();

        for (fi, f) in filters
            .iter()
            .enumerate()
            .filter(|(_, f)| !f.crit.is_empty() || check_r1.is_none())
        {
            let Some(rect) = self.filter_rect(f) else {
                continue;
            };
            let mut ok = vec![true; (rect.r1 - rect.r0) as usize];
            for (&field, cc) in &f.crit {
                if only_hide.is_some_and(|(i, fl)| i == fi && fl != field) {
                    continue;
                }
                let p = self
                    .field_pass(sheet, rect, field, cc, check_r1)
                    .ok_or(())?;
                for (o, x) in ok.iter_mut().zip(p) {
                    *o &= x;
                }
            }

            if f.total_row && rect.r1 > rect.r0 {
                ok.pop();
            }
            set.extend(
                ok.iter()
                    .enumerate()
                    .map(|(i, &k)| (rect.r0 + 1 + i as u32, !k)),
            );
        }
        let mut changed = Vec::new();
        for (r, hide) in set {
            if only_hide.is_some() && !hide {
                continue;
            }
            let v = self.view_mut(sheet);
            if v.hidden_rows.contains(&r) != hide {
                changed.push(r);
                if hide {
                    v.hidden_rows.insert(r);
                } else {
                    v.hidden_rows.remove(&r);
                }
            }
        }
        changed.sort_unstable();
        changed.dedup();
        if let Some(g) = given {
            changed = (g.r0 + 1..=g.r1).collect();
        }
        self.rows_visibility_changed(sheet, &changed);
        Ok(())
    }

    pub(crate) fn view_dedup_refilter(
        &mut self,
        sheet: SheetId,
        area: Rect,
        list: Rect,
        lost: Option<(u32, u32)>,
        gave_up: Option<(u32, u32)>,
    ) {
        let Some(v) = self.views.get(&sheet) else {
            return;
        };
        let n = v.filters.len();
        let mut any = false;
        for i in 0..n {
            let f = self.views[&sheet].filters[i].clone();
            if f.table.is_some() {
                let hit = self.filter_rect(&f).is_some_and(|r| r.intersects(&area));
                if hit && !f.crit.is_empty() {
                    if let Some((a, b)) = gave_up {
                        self.view_show_rows(sheet, a, b);
                    }
                }
                any |= hit;
                continue;
            }
            if !f.rect.intersects(&area) {
                continue;
            }
            any = true;

            if f.rect != list {
                continue;
            }
            let mut r1 = f.rect.r1;
            while r1 > f.rect.r0 && !(f.rect.c0..=f.rect.c1).any(|c| self.nonblank(sheet, r1, c)) {
                r1 -= 1;
            }
            let region =
                self.current_region(sheet, Rect::new(f.rect.r0, f.rect.c0, f.rect.r0, f.rect.c0));
            r1 = r1.max(region.r1.min(f.rect.r1));
            if r1 < f.rect.r1 {
                self.view_show_rows(sheet, r1 + 1, f.rect.r1);
            }
            self.view_mut(sheet).filters[i].rect.r1 = r1;
        }
        if let Some((a, b)) = lost {
            self.view_show_rows(sheet, a, b);
        }
        if any {
            let _ = self.refilter(sheet, None, None, None);
        }
    }

    fn ends_in_subtotal_row(&self, sheet: SheetId, rect: Rect) -> bool {
        rect.r1 > rect.r0
            && (rect.c0..=rect.c1).any(|c| {
                crate::functions::criteria::subtotal::is_bare_subtotal_cell(
                    self,
                    CellAddr::new(sheet, rect.r1, c),
                )
            })
    }

    fn smart_select(&self, sheet: SheetId, row: u32, col: u32, given: Option<Rect>) -> Rect {
        let seed = Rect::new(row, col, row, col);
        let mut b = self.current_region(sheet, seed);
        if b.is_cell() {
            return b;
        }
        while let Some(t) = self
            .tables
            .iter()
            .find(|t| t.sheet == sheet && t.rect.intersects(&b))
        {
            let t = t.rect;
            let cols_level =
                (t.c0 <= col && col <= t.c1) || given.is_some_and(|g| g.c0 <= t.c1 && t.c0 <= g.c1);
            if (row < t.r0 || t.r1 < row) && cols_level {
                if t.r1 < row {
                    if t.r1 == MAX_ROWS - 1 {
                        b = seed;
                        break;
                    }
                    b.r0 = t.r1 + 1;
                } else {
                    if t.r0 == 0 {
                        b = seed;
                        break;
                    }
                    b.r1 = t.r0 - 1;
                }
            } else if t.c1 < col {
                if t.c1 == MAX_COLS - 1 {
                    b = seed;
                    break;
                }
                b.c0 = t.c1 + 1;
            } else {
                if t.c0 == 0 {
                    b = seed;
                    break;
                }
                b.c1 = t.c0 - 1;
            }
        }
        let row_has = |r: u32, b: &Rect| (b.c0..=b.c1).any(|c| self.nonblank(sheet, r, c));
        let col_has = |c: u32, b: &Rect| (b.r0..=b.r1).any(|r| self.nonblank(sheet, r, c));
        if let Some(r) = (row + 1..=b.r1).find(|&r| !row_has(r, &b)) {
            b.r1 = r - 1;
        }
        if let Some(r) = (b.r0..row).rev().find(|&r| !row_has(r, &b)) {
            b.r0 = r + 1;
        }
        if let Some(c) = (b.c0..col).rev().find(|&c| !col_has(c, &b)) {
            b.c0 = c + 1;
        }
        if let Some(c) = (col + 1..=b.c1).find(|&c| !col_has(c, &b)) {
            b.c1 = c - 1;
        }
        b
    }

    fn filter_area_r1(&self, sheet: SheetId, f: &Filter) -> u32 {
        let stored = Rect {
            r1: f.anchor_r1,
            ..f.rect
        };
        self.smart_select(sheet, f.anchor_r1, f.rect.c0, Some(stored))
            .r1
            .max(f.anchor_r1)
    }

    pub(crate) fn cell_table_extent(&self, sheet: SheetId) -> Option<Rect> {
        let mut e = self.ever_used(sheet);
        if let Some(r1) = self.views.get(&sheet).and_then(|v| v.used_r1) {
            e = Some(match e {
                Some(x) => Rect {
                    r1: x.r1.max(r1),
                    ..x
                },
                None => Rect::new(r1, 0, r1, 0),
            });
        }
        e
    }

    fn new_filter_ref(&self, sheet: SheetId, range: Rect) -> Option<Rect> {
        let single = range.is_cell() || self.merge_area_exact(sheet, range);
        let sel = if single {
            self.select_for_single_cell(sheet, range)?
        } else {
            range
        };
        let mut lim = self.lim_ref(sheet, sel)?;
        if single {
            lim.r0 = self.filter_title_skip(sheet, lim);
        }
        Some(lim)
    }

    pub(crate) fn select_for_single_cell(&self, sheet: SheetId, range: Rect) -> Option<Rect> {
        let b = self.smart_select(sheet, range.r0, range.c0, None);
        if b.is_cell() && !self.nonblank(sheet, b.r0, b.c0) {
            return None;
        }
        let mut sel = b;
        let (r0, r1, c0, c1) = (b.r0, b.r1, b.c0, b.c1);
        let row_has = |r: u32| (c0..=c1).any(|c| self.nonblank(sheet, r, c));
        let col_has = |c: u32| (r0..=r1).any(|r| self.nonblank(sheet, r, c));
        if !row_has(r1) {
            sel.r1 = sel.r1.saturating_sub(1);
        }
        if !col_has(c0) {
            sel.c0 += 1;
        }
        if !row_has(r0) {
            sel.r0 += 1;
        }
        if !col_has(c1) {
            sel.c1 = sel.c1.saturating_sub(1);
        }
        Some(sel)
    }

    pub(crate) fn lim_ref(&self, sheet: SheetId, sel: Rect) -> Option<Rect> {
        let e = self.cell_table_extent(sheet)?;
        let (r0, c0, r1, c1) = (
            sel.r0.max(e.r0),
            sel.c0.max(e.c0),
            sel.r1.min(e.r1),
            sel.c1.min(e.c1),
        );
        (r0 <= r1 && c0 <= c1).then_some(Rect::new(r0, c0, r1, c1))
    }

    pub(crate) fn merge_area_exact(&self, sheet: SheetId, rect: Rect) -> bool {
        self.merge_at(CellAddr::new(sheet, rect.r0, rect.c0))
            .is_some_and(|m| m == rect)
    }

    fn new_range_filter(&self, sheet: SheetId, stored: Rect) -> Filter {
        let mut f = Filter {
            rect: stored,
            anchor_r1: stored.r1,
            table: None,
            crit: BTreeMap::new(),
            total_row: false,
        };
        f.rect.r1 = self.filter_area_r1(sheet, &f);
        f.total_row = self.ends_in_subtotal_row(sheet, f.rect);
        f
    }

    pub(crate) fn define_filter_name(
        &mut self,
        sheet: SheetId,
        name: &str,
        target: SheetId,
        rect: Rect,
        hidden: bool,
    ) {
        let (Some(_), Some(on)) = (self.sheet(sheet), self.sheet(target)) else {
            return;
        };
        let key = name.to_uppercase();
        let scope = if hidden
            || self.names.get(&key, Some(sheet)).is_some()
            || self.names.get(&key, None).is_none()
        {
            Some(sheet)
        } else {
            None
        };
        let corner = |r: u32, c: u32| format!("${}${}", crate::address::col_to_letters(c), r + 1);
        let area = if rect.is_cell() {
            corner(rect.r0, rect.c0)
        } else {
            format!("{}:{}", corner(rect.r0, rect.c0), corner(rect.r1, rect.c1))
        };
        let text = format!("='{}'!{area}", on.name.replace('\'', "''"));
        let new = self.names.get(&key, scope).is_none();
        if self.define_name_quiet(name, &text, scope).is_ok() {
            if hidden && new {
                self.names.set_visible(&key, scope, false);
            }
            calc::name_changed(self, &key);
        }
    }

    fn filter_db_ref(&self, sheet: SheetId) -> Option<Rect> {
        let def = self.names.get("_FILTERDATABASE", Some(sheet))?;
        let crate::formula::Node::Ref(sr, cref) = &def.formula.root else {
            return None;
        };
        if !matches!(sr, crate::formula::SheetRef::Sheet(s) if *s == sheet) {
            return None;
        }
        let at = |v: i32| u32::try_from(v).ok();
        match *cref {
            crate::formula::CRef::Cell { row, col } => Some(Rect::cell(at(row.v)?, at(col.v)?)),
            crate::formula::CRef::Area { r0, c0, r1, c1 } => {
                Some(Rect::new(at(r0.v)?, at(c0.v)?, at(r1.v)?, at(c1.v)?))
            }
            _ => None,
        }
    }

    pub fn list_add_meets_filter(&self, sheet: SheetId, rect: Rect, headed: bool) -> bool {
        let Some(f) = self
            .views
            .get(&sheet)
            .and_then(|v| v.filters.iter().find(|f| f.table.is_none()))
            .map(|f| f.rect)
        else {
            return false;
        };
        f.intersects(&rect)
            && (!headed || f.r0 != rect.r0 || rect.r1 < f.r1 || f.c0 < rect.c0 || rect.c1 < f.c1)
    }

    pub(crate) fn create_filters_on_list(&mut self, sheet: SheetId, name: &str, rect: Rect) {
        let Some(i) = self
            .views
            .get(&sheet)
            .and_then(|v| v.filters.iter().position(|f| f.table.is_none()))
        else {
            return;
        };
        let Some(db) = self.filter_db_ref(sheet) else {
            return;
        };
        if db == rect {
            let v = self.view_mut(sheet);
            let f = &mut v.filters[i];
            f.table = Some(name.to_string());
            f.rect = rect;
            f.anchor_r1 = rect.r1;
            f.total_row = false;
            v.fm = false;
        } else if db.intersects(&rect) {
            let _ = self.autofilter_off_run(sheet, None);
        }
    }

    pub(crate) fn table_filter_show_all(&mut self, sheet: SheetId, name: &str) {
        let Some(i) = self.views.get(&sheet).and_then(|v| {
            v.filters
                .iter()
                .position(|f| f.table.as_deref() == Some(name) && !f.crit.is_empty())
        }) else {
            return;
        };
        let f = self.views[&sheet].filters[i].clone();
        let mut changed = Vec::new();
        if let Some(rect) = self.filter_rect(&f) {
            let last = if f.total_row { rect.r1 - 1 } else { rect.r1 };
            let v = self.view_mut(sheet);
            changed = (rect.r0 + 1..=last)
                .filter(|r| v.hidden_rows.remove(r))
                .collect();
        }
        self.view_mut(sheet).filters[i].crit.clear();
        self.rows_visibility_changed(sheet, &changed);
    }

    pub(crate) fn drop_filter_name(&mut self, sheet: SheetId, name: &str) {
        let key = name.to_uppercase();
        if self.names.remove(&key, Some(sheet)).is_some() {
            calc::name_changed(self, &key);
        }
    }

    fn drop_advanced_filter(&mut self, sheet: SheetId) {
        if let Some(a) = self.views.get_mut(&sheet).and_then(|v| v.adv.take()) {
            let v = self.view_mut(sheet);
            v.fm = false;
            let shown: Vec<u32> = (a.r0 + 1..=a.r1)
                .filter(|r| v.hidden_rows.remove(r))
                .collect();
            self.rows_visibility_changed(sheet, &shown);
        }
    }

    pub fn autofilter_toggle(
        &mut self,
        sheet: SheetId,
        range: Rect,
    ) -> Result<RecalcReport, EditError> {
        self.command(Command::View, |wb| wb.autofilter_toggle_run(sheet, range))
    }

    fn autofilter_toggle_run(
        &mut self,
        sheet: SheetId,
        range: Rect,
    ) -> Result<RecalcReport, EditError> {
        if self.sheet(sheet).is_none() {
            return Err(EditError::NoSuchSheet);
        }
        if self.autofilter_mode(sheet) {
            return self.autofilter_off(sheet, None);
        }
        let had_adv = self.views.get(&sheet).is_some_and(|v| v.adv.is_some());
        self.drop_advanced_filter(sheet);
        let stored = self
            .new_filter_ref(sheet, range)
            .ok_or_else(|| EditError::Compile("AutoFilter method of Range class failed".into()))?;
        let f = self.new_range_filter(sheet, stored);
        self.view_mut(sheet).filters.push(f);
        self.define_filter_name(sheet, "_FilterDatabase", sheet, stored, true);

        if !had_adv {
            return Ok(RecalcReport::default());
        }
        Ok(self.view_settle())
    }

    pub fn autofilter(
        &mut self,
        sheet: SheetId,
        range: Rect,
        table: Option<&str>,
        spec: FilterSpec<'_>,
    ) -> Result<RecalcReport, EditError> {
        self.command(Command::View, |wb| {
            let had_adv = wb.views.get(&sheet).is_some_and(|v| v.adv.is_some());
            let r = wb.autofilter_run(sheet, range, table, spec);

            if r.is_err()
                && had_adv
                && wb.views.get(&sheet).is_none_or(|v| v.adv.is_none())
                && wb.settings.mode == crate::workbook::CalcMode::Automatic
            {
                calc::recalc(wb);
            }
            r
        })
    }

    pub(crate) fn autofilter_run(
        &mut self,
        sheet: SheetId,
        range: Rect,
        table: Option<&str>,
        spec: FilterSpec<'_>,
    ) -> Result<RecalcReport, EditError> {
        let FilterSpec {
            field,
            criteria1: c1,
            operator: op,
            criteria2: c2,
            op_code,
        } = spec;
        let bad = || EditError::Compile("AutoFilter method of Range class failed".into());
        if self.sheet(sheet).is_none() {
            return Err(EditError::NoSuchSheet);
        }
        if let Some(op) = op {
            if !matches!(
                op,
                "and"
                    | "or"
                    | "top10"
                    | "bottom10"
                    | "top10pct"
                    | "bottom10pct"
                    | "values"
                    | "dynamic"
            ) {
                return Err(bad());
            }
        }

        if table.is_none()
            && self.tables.iter().any(|t| {
                t.sheet == sheet
                    && t.rect.intersects(&range)
                    && !(t.rect.contains(range.r0, range.c0) && t.rect.contains(range.r1, range.c1))
            })
        {
            return Err(bad());
        }

        let in_table: Option<String> = match table {
            Some(_) => None,
            None => self
                .table_at(CellAddr::new(sheet, range.r0, range.c0))
                .filter(|t| t.headers)
                .map(|t| t.name.clone()),
        };
        let table = table.or(in_table.as_deref());
        self.drop_advanced_filter(sheet);
        let saved = self.views.get(&sheet).map(|v| v.filters.clone());
        let existing = self.views.get(&sheet).and_then(|v| {
            v.filters.iter().position(|f| match table {
                Some(t) => f.table.as_deref() == Some(t),
                None => f.table.is_none(),
            })
        });
        let mut new_ref = None;
        let rect = match (existing, table) {
            (Some(i), _) => {
                let f = self.views[&sheet].filters[i].clone();
                let mut r = self.filter_rect(&f).ok_or_else(bad)?;
                if f.table.is_none() {
                    r.r1 = self.filter_area_r1(sheet, &f);
                    let total_row = self.ends_in_subtotal_row(sheet, r);
                    let g = &mut self.view_mut(sheet).filters[i];
                    g.rect = r;
                    g.total_row = total_row;
                }
                r
            }
            (None, Some(t)) => {
                let t = self.table_by_name(t).ok_or_else(bad)?;
                if !t.headers {
                    return Err(bad());
                }
                let mut r = t.rect;
                if t.totals {
                    r.r1 -= 1;
                }
                r
            }
            (None, None) => {
                let stored = self.new_filter_ref(sheet, range).ok_or_else(bad)?;
                new_ref = Some(stored);
                self.new_range_filter(sheet, stored).rect
            }
        };
        if field == 0 || field > rect.cols() {
            return Err(bad());
        }
        if existing.is_none() {
            let f = match new_ref {
                Some(stored) => self.new_range_filter(sheet, stored),
                None => Filter {
                    rect,
                    anchor_r1: rect.r1,
                    table: table.map(str::to_string),
                    crit: BTreeMap::new(),
                    total_row: false,
                },
            };
            self.view_mut(sheet).filters.push(f);
        }
        let i = existing.unwrap_or(self.views[&sheet].filters.len() - 1);

        let only_hide = (existing.is_some()
            && c1.is_some()
            && !self.views[&sheet].filters[i].crit.is_empty()
            && !self.views[&sheet].filters[i]
                .crit
                .contains_key(&(field - 1)))
        .then_some((i, field - 1));
        let mut cleared_last = false;
        let month_first = self
            .input_locale
            .as_ref()
            .is_some_and(|l| l.date_order == crate::locale::DateOrder::MDY);
        let stored = |c: Option<&str>| {
            c.map(|c| match op {
                None | Some("and" | "or") if month_first => self.us_date_criterion(c),
                _ => c.to_string(),
            })
        };
        let (c1, c2) = (stored(c1), stored(c2));
        let (c1, c2) = (c1.as_deref(), c2.as_deref());
        let v = self.view_mut(sheet);
        match c1 {
            Some(c1) => {
                v.filters[i].crit.insert(
                    field - 1,
                    ColCrit {
                        c1: Some(c1.to_string()),
                        op: op.map(str::to_string),
                        c2: c2.map(str::to_string),
                        op_code,
                        month_first,
                    },
                );
            }
            None => {
                cleared_last = v.filters[i].crit.remove(&(field - 1)).is_some()
                    && v.filters[i].crit.is_empty();
            }
        }

        let check_r1 = if table.is_none() && !range.is_cell() {
            range.r1
        } else {
            rect.r1
        };

        let given = (existing.is_none() && table.is_none() && !range.is_cell()).then_some(range);
        if self
            .refilter(sheet, (!cleared_last).then_some(check_r1), only_hide, given)
            .is_err()
        {
            self.view_mut(sheet).filters = saved.unwrap_or_default();
            return Err(bad());
        }

        let ftable = self.views[&sheet].filters[i].table.clone();
        self.fm_after_apply(sheet, ftable.as_deref(), true);
        if given.is_some() {
            let fm = self.filter_mode_for_subtotal(sheet);
            self.view_mut(sheet).fm_last = fm;
        }
        if let Some(ti) = table.and_then(|t| self.table_position(t).ok()) {
            self.tables[ti].show_filter = true;
        }

        if table.is_none() {
            let total = existing.is_some_and(|i| self.views[&sheet].filters[i].total_row);
            let named = new_ref.unwrap_or(Rect {
                r1: rect.r1 - u32::from(total && rect.r1 > rect.r0),
                ..rect
            });
            self.define_filter_name(sheet, "_FilterDatabase", sheet, named, true);
        }

        let marked = match given {
            Some(g) => Some(g),
            None => self.filter_rect(&self.views[&sheet].filters[i].clone()),
        };
        if let Some(rect) = marked {
            self.mark_filter_area(sheet, rect);
        }
        Ok(self.view_settle())
    }

    fn us_date_criterion(&self, c: &str) -> String {
        let Some((op, rest)) = ["<=", ">=", "<>", "<", ">"]
            .iter()
            .find_map(|p| c.strip_prefix(p).map(|r| (*p, r)))
        else {
            return c.to_string();
        };
        let us = crate::locale::Locale::en_us();
        let env = crate::coerce::ParseEnv {
            locale: &us,
            date_system: self.settings.date_system,
            current_year: self.calc.current_year,
            iso_t: false,
        };
        match crate::coerce::parse_number_text(rest, env) {
            Some(p)
                if matches!(
                    p.kind,
                    crate::coerce::NumberKind::Date | crate::coerce::NumberKind::DateTime
                ) =>
            {
                format!("{op}{}", crate::number::format_general(p.value))
            }
            _ => c.to_string(),
        }
    }

    pub fn show_all_data(&mut self, sheet: SheetId) -> Result<RecalcReport, EditError> {
        self.command(Command::View, |wb| wb.show_all_data_run(sheet))
    }

    fn show_all_data_run(&mut self, sheet: SheetId) -> Result<RecalcReport, EditError> {
        if let Some(rect) = self.views.get_mut(&sheet).and_then(|v| v.adv.take()) {
            let v = self.view_mut(sheet);
            v.fm = false;
            let changed: Vec<u32> = (rect.r0 + 1..=rect.r1)
                .filter(|r| v.hidden_rows.remove(r))
                .collect();
            self.rows_visibility_changed(sheet, &changed);

            if let Some(used) = self.ever_used(sheet) {
                self.mark_area_dependents(sheet, (rect.r0, used.c0, rect.r1, used.c1));
            }
            return Ok(self.view_settle());
        }

        let sorted: Vec<usize> = (0..self.tables.len())
            .filter(|&i| self.tables[i].sheet == sheet && self.tables[i].sort_state)
            .collect();
        for &i in &sorted {
            self.tables[i].sort_state = false;
        }

        let target = self.show_all_target(sheet);
        let filters = self
            .views
            .get(&sheet)
            .map(|v| v.filters.clone())
            .unwrap_or_default();
        let is_target = |f: &Filter| match (&f.table, &target) {
            (Some(a), Some(b)) => crate::text::eq_ordinal_ci(a, b),
            (a, b) => a.is_none() && b.is_none(),
        };
        let filter_sorted =
            target.is_none() && self.views.get(&sheet).is_some_and(|v| v.filter_sorted);
        if filter_sorted {
            self.view_mut(sheet).filter_sorted = false;
        }
        if !filters.iter().any(|f| is_target(f) && !f.crit.is_empty()) {
            if !sorted.is_empty() || filter_sorted {
                return Ok(RecalcReport::default());
            }
            return Err(EditError::Compile("ShowAllData method failed".into()));
        }
        let mut changed = Vec::new();
        for (i, f) in filters.iter().enumerate() {
            if f.crit.is_empty() || !is_target(f) {
                continue;
            }
            if let Some(rect) = self.filter_rect(f) {
                let last = if f.total_row { rect.r1 - 1 } else { rect.r1 };
                let v = self.view_mut(sheet);
                for r in rect.r0 + 1..=last {
                    if v.hidden_rows.remove(&r) {
                        changed.push(r);
                    }
                }
            }
            self.view_mut(sheet).filters[i].crit.clear();

            if let Some(rect) = self.filter_rect(f) {
                self.mark_filter_area(sheet, rect);
            }
        }

        self.view_mut(sheet).fm = false;
        changed.sort_unstable();
        changed.dedup();
        self.rows_visibility_changed(sheet, &changed);
        Ok(self.view_settle())
    }

    fn show_all_target(&self, sheet: SheetId) -> Option<String> {
        let (r, c) = self.active_cell();
        self.table_at(CellAddr::new(sheet, r, c))
            .filter(|t| t.headers && t.show_filter)
            .map(|t| t.name.clone())
    }

    pub fn sheet_autofilter_missing(&self, sheet: SheetId) -> bool {
        let Some(v) = self.views.get(&sheet) else {
            return true;
        };
        let (r, c) = self.sheet_sel(sheet);
        !v.filters.iter().any(|f| f.table.is_none())
            && self
                .table_at(CellAddr::new(sheet, r, c))
                .is_none_or(|t| !(t.headers && t.show_filter))
    }

    pub fn autofilter_view(&self, sheet: SheetId) -> Option<FilterView> {
        let v = self.views.get(&sheet)?;
        let f = v.filters.iter().find(|f| f.table.is_none()).or_else(|| {
            let (r, c) = self.sheet_sel(sheet);
            let t = self.table_at(CellAddr::new(sheet, r, c))?;
            v.filters
                .iter()
                .find(|f| f.table.as_deref().is_some_and(|n| n == t.name))
        })?;
        let rect = self.filter_rect(f)?;
        let fields = (0..rect.cols())
            .map(|i| {
                let cc = f.crit.get(&i)?;
                Some(self.col_view(sheet, rect, i, cc))
            })
            .collect();
        Some(FilterView { rect, fields })
    }

    fn col_view(&self, sheet: SheetId, rect: Rect, field: u32, cc: &ColCrit) -> ColView {
        let text = |s: &str| {
            if s.starts_with(['=', '<', '>']) {
                s.to_string()
            } else {
                format!("={s}")
            }
        };
        let c1 = cc.c1.clone().unwrap_or_default();
        match cc.op.as_deref() {
            Some(op @ ("top10" | "bottom10" | "top10pct" | "bottom10pct")) => {
                let col = rect.c0 + field;
                let nums: Vec<f64> = (rect.r0 + 1..=rect.r1)
                    .filter_map(|r| match self.value(CellAddr::new(sheet, r, col)) {
                        Value::Number(x) => Some(x),
                        _ => None,
                    })
                    .collect();
                let n: f64 = c1.trim().parse().unwrap_or(0.0);
                let thr = top_threshold(nums, op, n);
                let shown = match thr {
                    Some(t) => format!(
                        "{}{}",
                        if op.starts_with("top") { ">=" } else { "<=" },
                        crate::number::format_general(t)
                    ),
                    None => c1,
                };
                ColView {
                    c1: shown,
                    op: cc.op_code,
                    c2: None,
                    values: Vec::new(),
                }
            }
            Some("values") => {
                let items: Vec<String> = c1
                    .split(VALUES_SEP)
                    .map(|i| {
                        if i.is_empty() || i == "=" {
                            "=".to_string()
                        } else {
                            text(i)
                        }
                    })
                    .collect();

                if let (Some(c2), 1) = (cc.c2.as_deref(), items.len()) {
                    ColView {
                        c1: items[0].clone(),
                        op: 1,
                        c2: Some(text(c2)),
                        values: Vec::new(),
                    }
                } else if items.len() <= 2 {
                    ColView {
                        c1: items[0].clone(),
                        op: 2,
                        c2: items.get(1).cloned(),
                        values: Vec::new(),
                    }
                } else {
                    ColView {
                        c1: items.first().cloned().unwrap_or_default(),
                        op: 7,
                        c2: None,
                        values: items,
                    }
                }
            }
            Some("dynamic") => ColView {
                c1,
                op: 11,
                c2: None,
                values: Vec::new(),
            },
            _ => ColView {
                c1: text(&c1),
                op: cc.op_code,
                c2: cc.c2.as_deref().map(text),
                values: Vec::new(),
            },
        }
    }

    pub fn reapply_filter(&mut self, sheet: SheetId) -> Result<RecalcReport, EditError> {
        self.command(Command::View, |wb| wb.reapply_filter_run(sheet))
    }

    fn reapply_filter_run(&mut self, sheet: SheetId) -> Result<RecalcReport, EditError> {
        let (r, c) = self.sheet_sel(sheet);
        let target = self
            .table_at(CellAddr::new(sheet, r, c))
            .filter(|t| t.headers && t.show_filter)
            .map(|t| t.name.clone());

        let table_af = self
            .tables
            .iter()
            .any(|t| t.sheet == sheet && t.headers && t.show_filter);
        if !table_af && self.views.get(&sheet).is_none_or(|v| v.filters.is_empty()) {
            return Err(EditError::Compile("ApplyFilter method failed".into()));
        }

        let n = self.views.get(&sheet).map_or(0, |v| v.filters.len());
        for i in 0..n {
            let f = self.views[&sheet].filters[i].clone();
            if f.table.is_some() {
                continue;
            }
            let mut r1 = f.rect.r1;
            while r1 + 1 < MAX_ROWS
                && (f.rect.c0..=f.rect.c1).any(|c| self.nonblank(sheet, r1 + 1, c))
            {
                r1 += 1;
            }
            self.view_mut(sheet).filters[i].rect.r1 = r1;

            let rect = self.views[&sheet].filters[i].rect;
            let total_row = self.ends_in_subtotal_row(sheet, rect);
            self.view_mut(sheet).filters[i].total_row = total_row;
        }
        let _ = self.refilter(sheet, None, None, None);
        let inside = target.is_some()
            || self.views.get(&sheet).is_some_and(|v| {
                v.filters.iter().any(|f| {
                    f.table.is_none() && self.filter_rect(f).is_some_and(|x| x.contains(r, c))
                })
            });
        let has_range = self
            .views
            .get(&sheet)
            .is_some_and(|v| v.filters.iter().any(|f| f.table.is_none()));
        if target.is_some() || has_range {
            self.fm_after_apply(sheet, target.as_deref(), inside);
        }

        let rects: Vec<Rect> = self.views.get(&sheet).map_or_else(Vec::new, |v| {
            v.filters
                .iter()
                .filter_map(|f| self.filter_rect(f))
                .collect()
        });
        for rect in rects {
            self.mark_filter_area(sheet, rect);
        }
        Ok(self.view_settle())
    }

    pub fn autofilter_off(
        &mut self,
        sheet: SheetId,
        table: Option<&str>,
    ) -> Result<RecalcReport, EditError> {
        self.command(Command::View, |wb| wb.autofilter_off_run(sheet, table))
    }

    fn autofilter_off_run(
        &mut self,
        sheet: SheetId,
        table: Option<&str>,
    ) -> Result<RecalcReport, EditError> {
        if let Some(ti) = table.and_then(|t| self.table_position(t).ok()) {
            self.tables[ti].show_filter = false;
            self.tables[ti].sort_state = false;
        }
        let Some(i) = self.views.get(&sheet).and_then(|v| {
            v.filters.iter().position(|f| match table {
                Some(t) => f.table.as_deref() == Some(t),
                None => f.table.is_none(),
            })
        }) else {
            return Ok(RecalcReport::default());
        };
        let f = self.views[&sheet].filters[i].clone();
        let mut changed = Vec::new();
        if !f.crit.is_empty() {
            self.view_mut(sheet).fm = false;
            if let Some(rect) = self.filter_rect(&f) {
                let last = if f.total_row { rect.r1 - 1 } else { rect.r1 };
                let v = self.view_mut(sheet);
                for r in rect.r0 + 1..=last {
                    if v.hidden_rows.remove(&r) {
                        changed.push(r);
                    }
                }
            }
        }

        if !f.crit.is_empty() {
            if let Some(rect) = self.filter_rect(&f) {
                self.mark_filter_area(sheet, rect);
            }
        }
        let had_crit = !f.crit.is_empty();
        self.view_mut(sheet).filters.remove(i);
        self.rows_visibility_changed(sheet, &changed);

        if !had_crit {
            return Ok(RecalcReport::default());
        }
        Ok(self.view_settle())
    }

    pub(crate) fn view_used_after_shift(
        &mut self,
        sheet: SheetId,
        old: Option<u32>,
        at: u32,
        count: u32,
        insert: bool,
    ) {
        let Some(old) = old else { return };
        let r = if insert && old >= at {
            (old + count).min(MAX_ROWS - 1)
        } else {
            old
        };
        self.view_mut(sheet).used_r1 = Some(r);
    }

    pub(crate) fn view_predelete_cols(&mut self, sheet: SheetId, at: u32, count: u32) {
        let Some(v) = self.views.get_mut(&sheet) else {
            return;
        };
        let mut saved = Vec::new();
        for (k, f) in v.filters.iter_mut().enumerate() {
            let gone = f.rect.c0 >= at && f.rect.c1 < at + count;
            if f.table.is_some() || gone {
                continue;
            }
            let keep: BTreeMap<u32, ColCrit> = f
                .crit
                .iter()
                .filter(|(&fd, _)| {
                    let col = f.rect.c0 + fd;
                    col < at || col >= at + count
                })
                .map(|(&fd, cc)| (fd, cc.clone()))
                .collect();
            if keep.len() < f.crit.len() {
                saved.push((k, std::mem::replace(&mut f.crit, keep)));
            }
        }
        if saved.is_empty() {
            return;
        }
        let _ = self.refilter(sheet, None, None, None);
        if let Some(v) = self.views.get_mut(&sheet) {
            for (k, crit) in saved {
                v.filters[k].crit = crit;
            }
        }
    }

    pub(crate) fn view_shift(
        &mut self,
        sheet: SheetId,
        rows: bool,
        at: u32,
        count: u32,
        insert: bool,
    ) {
        let below = self.insert_format_below;
        let subtotal_delete = self.op_on(crate::workbook::Op::SubtotalDelete);
        let Some(v) = self.views.get_mut(&sheet) else {
            return;
        };

        let from = if below {
            at + count
        } else {
            at.wrapping_sub(1)
        };
        let map = |i: u32| -> Option<u32> {
            if insert {
                Some(if i >= at { i + count } else { i })
            } else if i < at {
                Some(i)
            } else if i < at + count {
                None
            } else {
                Some(i - count)
            }
        };
        if rows {
            v.hbreaks = v.hbreaks.iter().filter_map(|&r| map(r)).collect();
            v.hidden_rows = v.hidden_rows.iter().filter_map(|&r| map(r)).collect();
            v.row_h = v
                .row_h
                .iter()
                .filter_map(|(&r, &h)| map(r).map(|r| (r, h)))
                .collect();
            v.levels = v
                .levels
                .iter()
                .filter_map(|(&r, &l)| map(r).map(|r| (r, l)))
                .collect();

            if insert && (at > 0 || below) {
                if let Some(&h) = v.row_h.get(&from) {
                    for r in at..(at + count).min(MAX_ROWS) {
                        v.row_h.insert(r, h);
                    }
                }
                if let Some(&l) = v.levels.get(&from) {
                    for r in at..(at + count).min(MAX_ROWS) {
                        v.levels.insert(r, l);
                    }
                }
            }
        } else {
            v.vbreaks = v.vbreaks.iter().filter_map(|&c| map(c)).collect();
            v.hidden_cols = v.hidden_cols.iter().filter_map(|&c| map(c)).collect();
            v.col_px = v
                .col_px
                .iter()
                .filter_map(|(&c, &w)| map(c).map(|c| (c, w)))
                .collect();
            v.col_levels = v
                .col_levels
                .iter()
                .filter_map(|(&c, &l)| map(c).map(|c| (c, l)))
                .collect();
            if insert && (at > 0 || below) {
                if let Some(&l) = v.col_levels.get(&from) {
                    for c in at..(at + count).min(MAX_COLS) {
                        v.col_levels.insert(c, l);
                    }
                }
            }

            if insert && (at > 0 || below) {
                if let Some(&w) = v.col_px.get(&from) {
                    for c in at..(at + count).min(crate::address::MAX_COLS) {
                        v.col_px.insert(c, w);
                    }
                }
            }
        }
        if rows {
            if let Some(a) = v.adv {
                let (r0, r1) = if insert {
                    (
                        if at <= a.r0 { a.r0 + count } else { a.r0 },
                        if at <= a.r1 { a.r1 + count } else { a.r1 },
                    )
                } else {
                    let e = at + count;
                    let r0 = if at < a.r0 {
                        a.r0.saturating_sub(count.min(a.r0 - at))
                    } else {
                        a.r0
                    };
                    let r1 = if e <= a.r1 {
                        a.r1 - count
                    } else if at <= a.r1 {
                        at.saturating_sub(1)
                    } else {
                        a.r1
                    };
                    (r0, r1)
                };
                v.adv = (r1 > r0).then_some(Rect { r0, r1, ..a });
            }
        }
        let mut drop = Vec::new();
        for (k, f) in v.filters.iter_mut().enumerate() {
            if f.table.is_some() {
                continue;
            }
            let (a, b) = if rows {
                (f.rect.r0, f.rect.r1)
            } else {
                (f.rect.c0, f.rect.c1)
            };
            let (na, nb) = if insert {
                (
                    if at <= a { a + count } else { a },
                    if at <= b { b + count } else { b },
                )
            } else {
                let e = at + count;
                let na = if at < a {
                    a.saturating_sub(count.min(a - at))
                } else {
                    a
                };
                let nb = if e <= b {
                    b - count
                } else if at <= b {
                    at.saturating_sub(1)
                } else {
                    b
                };
                if a >= at && b < e {
                    drop.push(k);
                    continue;
                }
                if rows && a >= at && a < e {
                    if !subtotal_delete {
                        drop.push(k);
                        continue;
                    }
                    (at, nb)
                } else {
                    (na, nb)
                }
            };
            if rows {
                let anchor = f.anchor_r1;
                let shifted = if insert {
                    if at <= anchor {
                        anchor + count
                    } else {
                        anchor
                    }
                } else if at + count <= anchor {
                    anchor - count
                } else if at <= anchor {
                    at.saturating_sub(1)
                } else {
                    anchor
                };
                f.rect.r0 = na;
                f.rect.r1 = nb;
                f.anchor_r1 = shifted.max(na).min(nb.max(na));
            } else {
                f.rect.c0 = na;
                f.rect.c1 = nb;
                if !insert && (at..at + count).any(|c| c >= a && c <= b) {
                    let mut m = BTreeMap::new();
                    for (&fd, cc) in &f.crit {
                        let col = a + fd;
                        if col < at {
                            m.insert(fd, cc.clone());
                        } else if col >= at + count {
                            m.insert(fd - count, cc.clone());
                        }
                    }
                    f.crit = m;
                } else if insert && at > a && at <= b {
                    let mut m = BTreeMap::new();
                    for (&fd, cc) in &f.crit {
                        m.insert(if a + fd >= at { fd + count } else { fd }, cc.clone());
                    }
                    f.crit = m;
                }
            }
        }
        let dropped = !drop.is_empty();

        let range = v
            .filters
            .iter()
            .enumerate()
            .find(|(_, f)| f.table.is_none())
            .map(|(k, f)| (drop.contains(&k), !f.crit.is_empty()));
        match range {
            Some((false, has)) => v.fm = has,
            Some((true, true)) => v.fm = false,
            _ => {}
        }
        for k in drop.into_iter().rev() {
            let f = v.filters.remove(k);

            if rows && !f.crit.is_empty() {
                for r in f.rect.r0..=f.rect.r1 {
                    if let Some(n) = map(r) {
                        v.hidden_rows.remove(&n);
                    }
                }
            }
        }

        if dropped {
            self.drop_filter_name(sheet, "_FilterDatabase");
        }
    }

    pub(crate) fn view_show_rows(&mut self, sheet: SheetId, r0: u32, r1: u32) {
        let Some(v) = self.views.get_mut(&sheet) else {
            return;
        };
        let changed: Vec<u32> = (r0..=r1).filter(|r| v.hidden_rows.remove(r)).collect();
        if !changed.is_empty() {
            self.rows_visibility_changed(sheet, &changed);
        }
    }

    pub(crate) fn view_rename_table(&mut self, sheet: SheetId, old: &str, new: &str) {
        if let Some(v) = self.views.get_mut(&sheet) {
            for f in &mut v.filters {
                if f.table
                    .as_deref()
                    .is_some_and(|t| t.eq_ignore_ascii_case(old))
                {
                    f.table = Some(new.to_string());
                }
            }

            if v.sel_table
                .as_deref()
                .is_some_and(|t| t.eq_ignore_ascii_case(old))
            {
                v.sel_table = Some(new.to_string());
            }
        }
    }

    pub(crate) fn view_drop_table(&mut self, sheet: SheetId, name: &str) {
        self.slicers_drop_table(name);
        let Some(i) = self.views.get(&sheet).and_then(|v| {
            v.filters.iter().position(|f| {
                f.table
                    .as_deref()
                    .is_some_and(|t| t.eq_ignore_ascii_case(name))
            })
        }) else {
            return;
        };
        let f = self.views[&sheet].filters[i].clone();
        let mut changed = Vec::new();
        if !f.crit.is_empty() {
            if let Some(rect) = self.filter_rect(&f) {
                let v = self.view_mut(sheet);
                for r in rect.r0 + 1..=rect.r1 {
                    if v.hidden_rows.remove(&r) {
                        changed.push(r);
                    }
                }
            }
        }
        self.view_mut(sheet).filters.remove(i);
        self.rows_visibility_changed(sheet, &changed);
    }

    pub(crate) fn table_filtered(&self, sheet: SheetId, name: &str) -> bool {
        self.views.get(&sheet).is_some_and(|v| {
            v.filters.iter().any(|f| {
                f.table
                    .as_deref()
                    .is_some_and(|t| t.eq_ignore_ascii_case(name))
                    && !f.crit.is_empty()
            })
        })
    }

    pub(crate) fn view_visible_runs(&self, sheet: SheetId, rect: Rect) -> Option<Vec<Rect>> {
        if !self.filter_mode(sheet) || rect.rows() > 1 << 16 {
            return None;
        }
        let mut runs: Vec<Rect> = Vec::new();
        for r in rect.r0..=rect.r1 {
            if self.row_hidden(sheet, r) {
                continue;
            }
            match runs.last_mut() {
                Some(l) if l.r1 + 1 == r => l.r1 = r,
                _ => runs.push(Rect::new(r, rect.c0, r, rect.c1)),
            }
        }

        if !runs.is_empty()
            && rect.cols() <= 1 << 14
            && (rect.c0..=rect.c1).any(|c| self.col_hidden(sheet, c))
        {
            let mut cr: Vec<(u32, u32)> = Vec::new();
            for c in rect.c0..=rect.c1 {
                if self.col_hidden(sheet, c) {
                    continue;
                }
                match cr.last_mut() {
                    Some(l) if l.1 + 1 == c => l.1 = c,
                    _ => cr.push((c, c)),
                }
            }
            if !cr.is_empty() {
                runs = runs
                    .iter()
                    .flat_map(|a| {
                        cr.iter()
                            .map(move |&(c0, c1)| Rect::new(a.r0, c0, a.r1, c1))
                    })
                    .collect();
            }
        }
        (!runs.is_empty() && !(runs.len() == 1 && runs[0] == rect)).then_some(runs)
    }

    pub(crate) fn copy_filter_op(&self, ss: SheetId, src: Rect) -> bool {
        self.filter_mode(ss)
            && !self
                .tables
                .iter()
                .any(|t| t.sheet == ss && t.rect == src && !t.list_sel)
    }

    pub fn mark_copy(&mut self, src: Option<(SheetId, Rect)>) {
        self.session.copy_mark = src.map(|(s, r)| (s, r, self.copy_filter_op(s, r)));
    }

    #[allow(clippy::type_complexity)]
    pub(crate) fn view_copy_plan(
        &self,
        ss: SheetId,
        src: Rect,
        ds: SheetId,
        dr_in: Rect,
        transpose: bool,
        special: bool,
    ) -> Option<Result<(Vec<(Rect, CellAddr)>, Rect, bool), EditError>> {
        let dr = if transpose {
            Rect::new(
                dr_in.r0,
                dr_in.c0,
                dr_in.r0 + dr_in.cols() - 1,
                dr_in.c0 + dr_in.rows() - 1,
            )
        } else {
            dr_in
        };

        let fm_src = match self.session.copy_mark {
            Some((s, r, on)) if special && s == ss && r == src => on,
            _ => self.copy_filter_op(ss, src),
        };
        let fm_dst = self.filter_mode(ds);
        if !fm_src && !fm_dst {
            return None;
        }
        let mut runs: Vec<(u32, u32)> = Vec::new();
        for r in src.r0..=src.r1 {
            if fm_src && self.row_hidden(ss, r) {
                continue;
            }
            match runs.last_mut() {
                Some(l) if l.1 + 1 == r => l.1 = r,
                _ => runs.push((r, r)),
            }
        }

        let mut cruns: Vec<(u32, u32)> = Vec::new();
        for c in src.c0..=src.c1 {
            if fm_src && src.cols() <= 1 << 14 && self.col_hidden(ss, c) {
                continue;
            }
            match cruns.last_mut() {
                Some(l) if l.1 + 1 == c => l.1 = c,
                _ => cruns.push((c, c)),
            }
        }
        let h: u32 = runs.iter().map(|x| x.1 - x.0 + 1).sum();
        let wv: u32 = cruns.iter().map(|x| x.1 - x.0 + 1).sum();
        let hidden_in_src = h != src.rows() || wv != src.cols();
        if !hidden_in_src && !fm_dst {
            return None;
        }
        let multi = Err(EditError::Compile(
            "This action won't work on multiple selections.".into(),
        ));

        let all_hidden = runs.is_empty() || cruns.is_empty();
        if all_hidden {
            runs = vec![(src.r0, src.r1)];
            cruns = vec![(src.c0, src.c1)];
        }
        let h: u32 = runs.iter().map(|x| x.1 - x.0 + 1).sum();
        let w: u32 = cruns.iter().map(|x| x.1 - x.0 + 1).sum();
        let several = runs.len() > 1 || cruns.len() > 1;
        let tile = |row: u32, col: u32| -> Vec<(Rect, CellAddr)> {
            let mut out = Vec::new();
            let mut off = 0;
            for &(a, b) in &runs {
                let mut coff = 0;
                for &(c, d) in &cruns {
                    out.push((
                        Rect::new(a, c, b, d),
                        CellAddr::new(ds, row + off, col + coff),
                    ));
                    coff += d - c + 1;
                }
                off += b - a + 1;
            }
            out
        };

        let one_vis = special && !all_hidden && h == 1 && w == 1;
        let mut areas: Vec<Rect> = Vec::new();
        if fm_dst && !transpose && !dr.is_cell() {
            for r in dr.r0..=dr.r1 {
                if !one_vis && self.row_hidden(ds, r) {
                    continue;
                }
                match areas.last_mut() {
                    Some(l) if l.r1 + 1 == r => l.r1 = r,
                    _ => areas.push(Rect::new(r, dr.c0, r, dr.c1)),
                }
            }

            if dr.cols() <= 1 << 14 && (dr.c0..=dr.c1).any(|c| self.col_hidden(ds, c)) {
                let mut cr: Vec<(u32, u32)> = Vec::new();
                for c in dr.c0..=dr.c1 {
                    if self.col_hidden(ds, c) {
                        continue;
                    }
                    match cr.last_mut() {
                        Some(l) if l.1 + 1 == c => l.1 = c,
                        _ => cr.push((c, c)),
                    }
                }
                areas = areas
                    .iter()
                    .flat_map(|a| {
                        cr.iter()
                            .map(move |&(c0, c1)| Rect::new(a.r0, c0, a.r1, c1))
                    })
                    .collect();
            }
        }
        if areas.is_empty() {
            areas.push(dr);
        }
        if areas.len() > 1 && all_hidden {
            return Some(Err(EditError::Compile("No cells were found.".into())));
        }
        if areas.len() > 1 && several {
            return Some(multi);
        }
        let mut steps = Vec::new();
        let mut whole: Option<Rect> = None;
        for a in areas {
            let (nr, nc) = if fm_dst && !(transpose && a.rows() > 1 && a.cols() > 1) {
                (
                    if a.rows() % h == 0 { a.rows() / h } else { 1 },
                    if a.cols() % w == 0 { a.cols() / w } else { 1 },
                )
            } else if a.rows() % h == 0 && a.cols() % w == 0 {
                (a.rows() / h, a.cols() / w)
            } else {
                (1, 1)
            };
            for ti in 0..nr {
                for tj in 0..nc {
                    steps.extend(tile(a.r0 + ti * h, a.c0 + tj * w));
                }
            }
            let cover = Rect::new(a.r0, a.c0, a.r0 + nr * h - 1, a.c0 + nc * w - 1);
            whole = Some(match whole {
                None => cover,
                Some(x) => Rect::new(
                    x.r0.min(cover.r0),
                    x.c0.min(cover.c0),
                    x.r1.max(cover.r1),
                    x.c1.max(cover.c1),
                ),
            });
        }
        let whole = whole.unwrap_or(dr);
        let flip = |r: Rect| {
            Rect::new(
                dr_in.r0 + (r.c0 - dr.c0),
                dr_in.c0 + (r.r0 - dr.r0),
                dr_in.r0 + (r.c1 - dr.c0),
                dr_in.c0 + (r.r1 - dr.r0),
            )
        };
        let (whole, steps) = if transpose {
            let steps = steps
                .into_iter()
                .map(|(run, to)| {
                    let p = flip(Rect::new(to.row, to.col, to.row, to.col));
                    (run, CellAddr::new(ds, p.r0, p.c0))
                })
                .collect();
            (flip(whole), steps)
        } else {
            (whole, steps)
        };

        if several && ss == ds && src.intersects(&whole) {
            return Some(Err(EditError::Compile(
                "This selection isn't valid. Make sure the copy and paste areas don't overlap \
                 unless they are the same size and shape."
                    .into(),
            )));
        }
        Some(Ok((steps, whole, several)))
    }
}

#[derive(Clone, Copy, Debug, Default)]
pub struct FilterSpec<'a> {
    pub field: u32,

    pub criteria1: Option<&'a str>,

    pub operator: Option<&'a str>,

    pub criteria2: Option<&'a str>,

    pub op_code: i32,
}

#[derive(Clone, Debug)]
pub struct FilterView {
    pub rect: Rect,

    pub fields: Vec<Option<ColView>>,
}

#[derive(Clone, Debug)]
pub struct ColView {
    pub c1: String,
    pub op: i32,
    pub c2: Option<String>,

    pub values: Vec<String>,
}

pub(crate) fn top_threshold(mut nums: Vec<f64>, op: &str, n: f64) -> Option<f64> {
    if op.starts_with("top") {
        nums.sort_by(|a, b| b.total_cmp(a));
    } else {
        nums.sort_by(|a, b| a.total_cmp(b));
    }
    if nums.is_empty() {
        return None;
    }
    let k = if op.ends_with("pct") {
        ((nums.len() as f64 * n / 100.0).floor() as usize).max(1)
    } else {
        n.floor() as usize
    }
    .min(nums.len());
    nums.get(k.checked_sub(1)?).copied()
}

#[cfg(test)]
mod tests {
    use crate::address::{CellAddr, Rect};
    use crate::value::Value;
    use crate::view::FilterSpec;
    use crate::workbook::Workbook;

    #[test]
    fn criterion_dates_read_month_first() {
        let run = |c: &str, us: bool| {
            let mut wb = Workbook::new();
            let s = wb.sheet_at(0).id;
            wb.set_value(CellAddr::new(s, 0, 0), Value::text("d"));

            for (r, d) in [45663.0, 45809.0, 46357.0].into_iter().enumerate() {
                wb.set_value(CellAddr::new(s, r as u32 + 1, 0), Value::Number(d));
            }
            let spec = FilterSpec {
                field: 1,
                criteria1: Some(c),
                ..FilterSpec::default()
            };
            let r = Rect::new(0, 0, 3, 0);
            if us {
                let loc = crate::locale::Locale::en_us();
                wb.with_input_locale(&loc, |wb| wb.autofilter(s, r, None, spec))
                    .unwrap();
            } else {
                wb.autofilter(s, r, None, spec).unwrap();
            }
            wb.hidden_rows(s)
        };
        assert_eq!(
            crate::locale::Locale::default().date_order,
            crate::locale::DateOrder::DMY
        );

        assert_eq!(run(">=6/1/2025", true), vec![1]);
        assert_eq!(run("<6/1/2025", true), vec![2, 3]);
        assert_eq!(run(">=45809", true), vec![1]);

        assert_eq!(run(">=13/1/2025", true), vec![1, 2, 3]);

        assert_eq!(run(">=6/1/2025", false), Vec::<u32>::new());
        assert_eq!(run(">=13/1/2025", false), vec![1]);
    }

    fn grid(cells: &[(u32, u32)]) -> (Workbook, crate::address::SheetId) {
        let mut wb = Workbook::new();
        let s = wb.sheet_at(0).id;
        for &(r, c) in cells {
            wb.set_value(CellAddr::new(s, r, c), Value::Number(1.0));
        }
        (wb, s)
    }

    #[test]
    fn smart_select_cuts_tables() {
        let cells: Vec<(u32, u32)> = (0..6).flat_map(|r| [(r, 0), (r, 1)]).collect();
        let (mut wb, s) = grid(&cells);
        assert_eq!(wb.smart_select(s, 0, 0, None), Rect::new(0, 0, 5, 1));
        wb.add_table("T", s, Rect::new(3, 0, 5, 1)).unwrap();
        assert_eq!(wb.smart_select(s, 0, 0, None), Rect::new(0, 0, 2, 1));

        let mut cells: Vec<(u32, u32)> = (0..3).flat_map(|r| [(r, 0), (r, 1), (r, 2)]).collect();
        cells.extend((0..3).map(|r| (r, 3)));
        let (mut wb, s) = grid(&cells);
        wb.add_table("U", s, Rect::new(0, 3, 2, 3)).unwrap();
        assert_eq!(wb.smart_select(s, 1, 0, None), Rect::new(0, 0, 2, 2));
    }

    #[test]
    fn new_filter_ref_limits_and_trims() {
        let cells: Vec<(u32, u32)> = (1..5).flat_map(|r| [(r, 1), (r, 2)]).collect();
        let (wb, s) = grid(&cells);
        assert_eq!(
            wb.new_filter_ref(s, Rect::new(1, 0, 20, 5)),
            Some(Rect::new(1, 1, 4, 2))
        );
        assert_eq!(wb.new_filter_ref(s, Rect::new(10, 10, 12, 12)), None);
        assert_eq!(wb.new_filter_ref(s, Rect::new(8, 8, 8, 8)), None);
        assert_eq!(
            wb.new_filter_ref(s, Rect::new(0, 0, 0, 0)),
            Some(Rect::new(1, 1, 4, 2))
        );
    }

    #[test]
    fn filter_area_follows_bottom_left_region() {
        let mut cells: Vec<(u32, u32)> = (0..3).flat_map(|r| [(r, 0), (r, 1), (r, 2)]).collect();
        cells.extend([(3, 2), (4, 2)]);
        let (wb, s) = grid(&cells);
        let f = wb.new_range_filter(s, Rect::new(0, 0, 2, 2));
        assert_eq!(f.rect.r1, 4);
        let f = wb.new_range_filter(s, Rect::new(0, 0, 2, 0));
        assert_eq!(f.rect.r1, 4);
    }
}
