use std::collections::{BTreeMap, BTreeSet};

use crate::address::{CellAddr, Rect, SheetId, MAX_ROWS};
use crate::calc::{self, RecalcReport};
use crate::eval::EvalCtx;
use crate::functions::criteria::crit::{Criterion, Flavor};
use crate::value::Value;
use crate::workbook::{EditError, Workbook};

pub const VALUES_SEP: char = '\u{1}';

#[derive(Clone, Debug, Default)]
pub struct ColCrit {
    pub c1: Option<String>,

    pub op: Option<String>,
    pub c2: Option<String>,
}

#[derive(Clone, Debug)]
pub struct Filter {
    pub rect: Rect,
    pub table: Option<String>,

    pub crit: BTreeMap<u32, ColCrit>,

    pub total_row: bool,
}

#[derive(Clone, Debug, Default)]
pub struct SheetView {
    pub hidden_rows: BTreeSet<u32>,
    pub hidden_cols: BTreeSet<u32>,

    pub levels: BTreeMap<u32, u8>,
    pub filters: Vec<Filter>,

    pub used_r1: Option<u32>,

    pub ghost_fm: bool,
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
        1 + self
            .views
            .get(&sheet)
            .and_then(|v| v.levels.get(&row))
            .copied()
            .unwrap_or(0) as u32
    }

    pub fn filter_mode(&self, sheet: SheetId) -> bool {
        self.views.get(&sheet).is_some_and(|v| {
            v.ghost_fm
                || v.filters.iter().any(|f| {
                    !f.crit.is_empty()
                        && f.table
                            .as_ref()
                            .is_none_or(|t| self.table_by_name(t).is_some())
                })
        })
    }

    pub fn autofilter_mode(&self, sheet: SheetId) -> bool {
        self.views
            .get(&sheet)
            .is_some_and(|v| v.filters.iter().any(|f| f.table.is_none()))
    }

    fn view_mut(&mut self, sheet: SheetId) -> &mut SheetView {
        self.views.entry(sheet).or_default()
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

    pub(crate) fn view_touch_paste(&mut self, sheet: SheetId, src: Rect, dst: Rect) {
        let h = src.rows().max(1);
        let end = if dst.rows() >= h && dst.rows() % h == 0 {
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

    fn rows_visibility_changed(&mut self, sheet: SheetId, rows: &[u32]) {
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

    fn view_settle(&mut self) -> RecalcReport {
        self.after_edit(Vec::new())
    }

    pub fn set_rows_hidden(
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
        self.rows_visibility_changed(sheet, &changed);
        Ok(self.view_settle())
    }

    pub fn set_cols_hidden(&mut self, sheet: SheetId, at: u32, count: u32, hide: bool) {
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

    pub fn group_rows(
        &mut self,
        sheet: SheetId,
        at: u32,
        count: u32,
        group: bool,
    ) -> Result<RecalcReport, EditError> {
        let last = (at + count).min(MAX_ROWS) - 1;
        if group {
            self.view_touch_row(sheet, (last + 1).min(MAX_ROWS - 1));
        }
        let v = self.view_mut(sheet);

        if !group && !(at..=last).any(|r| v.levels.contains_key(&r)) {
            return Err(EditError::Compile("Ungroup method failed".into()));
        }
        for r in at..=last {
            let l = v.levels.get(&r).copied().unwrap_or(0);
            if group {
                if l >= 7 {
                    return Err(EditError::Compile("cannot group more levels".into()));
                }
                v.levels.insert(r, l + 1);
            } else if l > 0 {
                if l == 1 {
                    v.levels.remove(&r);
                } else {
                    v.levels.insert(r, l - 1);
                }
            }
        }
        Ok(RecalcReport::default())
    }

    pub fn outline_show(&mut self, sheet: SheetId, level: u32) -> RecalcReport {
        let mut changed = Vec::new();
        {
            let v = self.view_mut(sheet);
            let rows: Vec<(u32, u8)> = v.levels.iter().map(|(r, l)| (*r, *l)).collect();
            for (r, l) in rows {
                let hide = 1 + l as u32 > level;
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
        self.rows_visibility_changed(sheet, &changed);
        self.view_settle()
    }

    fn nonblank(&self, sheet: SheetId, r: u32, c: u32) -> bool {
        !self.value(CellAddr::new(sheet, r, c)).is_blank()
    }

    pub fn current_region(&self, sheet: SheetId, rect: Rect) -> Rect {
        let mut b = rect;
        loop {
            let mut grew = false;
            let (r0, r1, c0, c1) = (
                b.r0.saturating_sub(1),
                (b.r1 + 1).min(MAX_ROWS - 1),
                b.c0.saturating_sub(1),
                (b.c1 + 1).min(crate::address::MAX_COLS - 1),
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
            if b.c0 > 0 && any((r0, r1), (b.c0 - 1, b.c0 - 1)) {
                b.c0 -= 1;
                grew = true;
            }
            if b.c1 + 1 < crate::address::MAX_COLS && any((r0, r1), (b.c1 + 1, b.c1 + 1)) {
                b.c1 += 1;
                grew = true;
            }
            if !grew {
                return b;
            }
        }
    }

    fn filter_rect(&self, f: &Filter) -> Option<Rect> {
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
                return Some(vec![true; vals.len()]);
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
        let host = CellAddr::new(sheet, rect.r0, rect.c0);
        let ctx = EvalCtx::new(self, host);
        let crit = |s: &str, v: &Value| -> bool {
            match Criterion::parse(&ctx, &Value::from(s), Flavor::Ifs) {
                Ok(c) => c.matches(&ctx, v),
                Err(_) => false,
            }
        };
        let pass = |i: usize, r: u32| -> bool {
            let v = &vals[i];
            if op == Some("values") {
                let list = cc.c1.as_deref().unwrap_or("");
                return list.split(VALUES_SEP).any(|item| {
                    if item.is_empty() || item == "=" {
                        return matches!(v, Value::Blank)
                            || matches!(v, Value::Text(t) if t.is_empty());
                    }
                    if item.starts_with(['=', '<', '>']) || item.contains(['*', '?']) {
                        return crit(item, v);
                    }
                    let shown = self.display_text(CellAddr::new(sheet, r, col));
                    shown.to_lowercase() == item.to_lowercase()
                });
            }
            let a = cc.c1.as_deref().is_none_or(|c| crit(c, v));
            match (op, &cc.c2) {
                (Some("or"), Some(c2)) => a || crit(c2, v),
                (Some("and"), Some(c2)) => a && crit(c2, v),
                _ => a,
            }
        };
        Some(rows.enumerate().map(|(i, r)| pass(i, r)).collect())
    }

    fn refilter(
        &mut self,
        sheet: SheetId,
        check_r1: Option<u32>,
        only_hide: bool,
    ) -> Result<(), ()> {
        let Some(v) = self.views.get(&sheet) else {
            return Ok(());
        };
        let filters = v.filters.clone();
        let mut set: Vec<(u32, bool)> = Vec::new();
        for f in filters.iter().filter(|f| !f.crit.is_empty()) {
            let Some(rect) = self.filter_rect(f) else {
                continue;
            };
            let mut ok = vec![true; (rect.r1 - rect.r0) as usize];
            for (&field, cc) in &f.crit {
                let p = self
                    .field_pass(sheet, rect, field, cc, check_r1)
                    .ok_or(())?;
                for (o, x) in ok.iter_mut().zip(p) {
                    *o &= x;
                }
            }
            if f.total_row && rect.r1 > rect.r0 {
                if let Some(l) = ok.last_mut() {
                    *l = true;
                }
            }
            set.extend(
                ok.iter()
                    .enumerate()
                    .map(|(i, &k)| (rect.r0 + 1 + i as u32, !k)),
            );
        }
        let mut changed = Vec::new();
        for (r, hide) in set {
            if only_hide && !hide {
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
        self.rows_visibility_changed(sheet, &changed);
        Ok(())
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

    fn range_filter_rect(&self, sheet: SheetId, range: Rect) -> Rect {
        if range.is_cell() {
            self.current_region(sheet, range)
        } else {
            let head = Rect::new(range.r0, range.c0, range.r0, range.c1);
            let reg = self.current_region(sheet, head);

            let used = self.view_used_r1(sheet).map_or(0, |r| r.min(range.r1));
            let mut r1 = reg.r1.max(used).max(range.r0);
            while r1 >= range.r1
                && r1 + 1 < MAX_ROWS
                && (range.c0..=range.c1).any(|c| self.nonblank(sheet, r1 + 1, c))
            {
                r1 += 1;
            }
            Rect::new(range.r0, range.c0, r1, range.c1)
        }
    }

    pub fn autofilter_toggle(
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
        let rect = self.range_filter_rect(sheet, range);
        let total_row = self.ends_in_subtotal_row(sheet, rect);
        self.view_mut(sheet).filters.push(Filter {
            rect,
            table: None,
            crit: BTreeMap::new(),
            total_row,
        });
        Ok(self.view_settle())
    }

    #[allow(clippy::too_many_arguments)]
    pub fn autofilter(
        &mut self,
        sheet: SheetId,
        range: Rect,
        table: Option<&str>,
        field: u32,
        c1: Option<&str>,
        op: Option<&str>,
        c2: Option<&str>,
    ) -> Result<RecalcReport, EditError> {
        let bad = || EditError::Compile("AutoFilter method of Range class failed".into());
        if self.sheet(sheet).is_none() {
            return Err(EditError::NoSuchSheet);
        }
        if let Some(op) = op {
            if !matches!(
                op,
                "and" | "or" | "top10" | "bottom10" | "top10pct" | "bottom10pct" | "values"
            ) {
                return Err(bad());
            }
        }

        let in_table: Option<String> = match table {
            Some(_) => None,
            None => match self.table_at(CellAddr::new(sheet, range.r0, range.c0)) {
                Some(t)
                    if !(t.rect.contains(range.r0, range.c0)
                        && t.rect.contains(range.r1, range.c1)) =>
                {
                    return Err(bad());
                }
                t => t.filter(|t| t.headers).map(|t| t.name.clone()),
            },
        };
        let table = table.or(in_table.as_deref());
        let saved = self.views.get(&sheet).map(|v| v.filters.clone());
        let existing = self.views.get(&sheet).and_then(|v| {
            v.filters.iter().position(|f| match table {
                Some(t) => f.table.as_deref() == Some(t),
                None => f.table.is_none(),
            })
        });
        let rect = match (existing, table) {
            (Some(i), _) => {
                let f = self.views[&sheet].filters[i].clone();
                self.filter_rect(&f).ok_or_else(bad)?
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
                let r = self.range_filter_rect(sheet, range);
                if r.rows() < 2 && r.cols() < 1 {
                    return Err(bad());
                }
                r
            }
        };
        if field == 0 || field > rect.cols() {
            return Err(bad());
        }
        if existing.is_none() {
            let total_row = table.is_none() && self.ends_in_subtotal_row(sheet, rect);
            self.view_mut(sheet).filters.push(Filter {
                rect,
                table: table.map(str::to_string),
                crit: BTreeMap::new(),
                total_row,
            });
        }
        let i = existing.unwrap_or(self.views[&sheet].filters.len() - 1);

        let only_hide = existing.is_some()
            && c1.is_some()
            && !self.views[&sheet].filters[i]
                .crit
                .contains_key(&(field - 1));
        let v = self.view_mut(sheet);
        match c1 {
            Some(c1) => {
                v.filters[i].crit.insert(
                    field - 1,
                    ColCrit {
                        c1: Some(c1.to_string()),
                        op: op.map(str::to_string),
                        c2: c2.map(str::to_string),
                    },
                );
            }
            None => {
                v.filters[i].crit.remove(&(field - 1));
            }
        }

        let check_r1 = if table.is_none() && !range.is_cell() {
            range.r1
        } else {
            rect.r1
        };
        if self.refilter(sheet, Some(check_r1), only_hide).is_err() {
            self.view_mut(sheet).filters = saved.unwrap_or_default();
            return Err(bad());
        }
        if let Some(ti) = table.and_then(|t| self.table_position(t).ok()) {
            self.tables[ti].show_filter = true;
        }
        Ok(self.view_settle())
    }

    pub fn show_all_data(&mut self, sheet: SheetId) -> Result<RecalcReport, EditError> {
        let sorted: Vec<usize> = (0..self.tables.len())
            .filter(|&i| self.tables[i].sheet == sheet && self.tables[i].sort_state)
            .collect();
        for &i in &sorted {
            self.tables[i].sort_state = false;
        }
        if !self.filter_mode(sheet) {
            if !sorted.is_empty() {
                return Ok(RecalcReport::default());
            }
            return Err(EditError::Compile("ShowAllData method failed".into()));
        }
        let filters = self.views[&sheet].filters.clone();
        let mut changed = Vec::new();
        for (i, f) in filters.iter().enumerate() {
            if f.crit.is_empty() {
                continue;
            }
            if let Some(rect) = self.filter_rect(f) {
                let v = self.view_mut(sheet);
                for r in rect.r0 + 1..=rect.r1 {
                    if v.hidden_rows.remove(&r) {
                        changed.push(r);
                    }
                }
            }
            self.view_mut(sheet).filters[i].crit.clear();
        }
        changed.sort_unstable();
        changed.dedup();
        self.rows_visibility_changed(sheet, &changed);
        Ok(self.view_settle())
    }

    pub fn reapply_filter(&mut self, sheet: SheetId) -> Result<RecalcReport, EditError> {
        let table_af = self
            .tables
            .iter()
            .any(|t| t.sheet == sheet && t.headers && t.show_filter);
        if !table_af && self.views.get(&sheet).is_none_or(|v| v.filters.is_empty()) {
            return Err(EditError::Compile("ApplyFilter method failed".into()));
        }

        let n = self.views[&sheet].filters.len();
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
        }
        let _ = self.refilter(sheet, None, false);
        Ok(self.view_settle())
    }

    pub fn autofilter_off(
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

    pub(crate) fn view_shift(
        &mut self,
        sheet: SheetId,
        rows: bool,
        at: u32,
        count: u32,
        insert: bool,
    ) {
        let Some(v) = self.views.get_mut(&sheet) else {
            return;
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
            v.hidden_rows = v.hidden_rows.iter().filter_map(|&r| map(r)).collect();
            v.levels = v
                .levels
                .iter()
                .filter_map(|(&r, &l)| map(r).map(|r| (r, l)))
                .collect();
        } else {
            v.hidden_cols = v.hidden_cols.iter().filter_map(|&c| map(c)).collect();
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
                    at.saturating_sub(1).max(0)
                } else {
                    b
                };
                if a >= at && b < e {
                    drop.push(k);
                    continue;
                }
                if rows && a >= at && a < e {
                    drop.push(k);
                    continue;
                }
                (na, nb)
            };
            if rows {
                f.rect.r0 = na;
                f.rect.r1 = nb;
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
        }
    }

    pub(crate) fn view_drop_table(&mut self, sheet: SheetId, name: &str) {
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
            self.view_mut(sheet).ghost_fm = true;
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
        (!runs.is_empty() && !(runs.len() == 1 && runs[0] == rect)).then_some(runs)
    }

    #[allow(clippy::type_complexity)]
    pub(crate) fn view_copy_plan(
        &self,
        ss: SheetId,
        src: Rect,
        ds: SheetId,
        dr: Rect,
    ) -> Option<Result<(Vec<(Rect, CellAddr)>, Rect, bool), EditError>> {
        let fm_src = self.filter_mode(ss);
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
        let h: u32 = runs.iter().map(|x| x.1 - x.0 + 1).sum();
        let hidden_in_src = h != src.rows();
        if !hidden_in_src && !fm_dst {
            return None;
        }
        let multi = Err(EditError::Compile(
            "This action won't work on multiple selections.".into(),
        ));

        let all_hidden = runs.is_empty();
        if all_hidden {
            runs.push((src.r0, src.r1));
        }
        let h: u32 = runs.iter().map(|x| x.1 - x.0 + 1).sum();
        let w = src.cols();
        let tile = |row: u32, col: u32| -> Vec<(Rect, CellAddr)> {
            let mut off = 0;
            runs.iter()
                .map(|&(a, b)| {
                    let to = CellAddr::new(ds, row + off, col);
                    off += b - a + 1;
                    (Rect::new(a, src.c0, b, src.c1), to)
                })
                .collect()
        };

        let mut areas: Vec<Rect> = Vec::new();
        if fm_dst && !dr.is_cell() {
            for r in dr.r0..=dr.r1 {
                if self.row_hidden(ds, r) {
                    continue;
                }
                match areas.last_mut() {
                    Some(l) if l.r1 + 1 == r => l.r1 = r,
                    _ => areas.push(Rect::new(r, dr.c0, r, dr.c1)),
                }
            }
        }
        if areas.is_empty() {
            areas.push(dr);
        }
        if areas.len() > 1 && all_hidden {
            return Some(Err(EditError::Compile("No cells were found.".into())));
        }
        if areas.len() > 1 && runs.len() > 1 {
            return Some(multi);
        }
        let mut steps = Vec::new();
        let mut whole: Option<Rect> = None;
        for a in areas {
            let (nr, nc) = if fm_dst {
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

        if runs.len() > 1 && ss == ds && src.intersects(&whole) {
            return Some(Err(EditError::Compile(
                "This selection isn't valid. Make sure the copy and paste areas don't overlap \
                 unless they are the same size and shape."
                    .into(),
            )));
        }
        Some(Ok((steps, whole, runs.len() > 1)))
    }
}
