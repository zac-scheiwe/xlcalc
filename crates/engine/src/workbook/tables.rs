use crate::address::{Rect, SheetId};
use crate::formula::Formula;
use crate::workbook::FormulaId;
use std::sync::Arc;

#[derive(Clone, Copy, Debug, PartialEq, Eq, Hash, Default)]
pub struct Items {
    pub headers: bool,

    pub data: bool,

    pub totals: bool,

    pub this_row: bool,
}

impl Items {
    pub const DATA: Items = Items {
        headers: false,
        data: true,
        totals: false,
        this_row: false,
    };

    pub const ALL: Items = Items {
        headers: true,
        data: true,
        totals: true,
        this_row: false,
    };
}

#[derive(Clone, Debug)]
pub struct Table {
    pub name: String,

    pub sheet: SheetId,

    pub rect: Rect,

    pub totals: bool,

    pub columns: Vec<String>,

    pub calc: Vec<bool>,

    pub calc_key: Vec<(String, String)>,

    pub headers: bool,

    pub totals_code: Option<u32>,
}

impl Table {
    pub fn header_row(&self) -> Option<u32> {
        self.headers.then_some(self.rect.r0)
    }

    pub fn data_start(&self) -> u32 {
        self.rect.r0 + u32::from(self.headers)
    }

    pub fn data_rows(&self) -> (u32, u32) {
        let r1 = if self.totals {
            self.rect.r1 - 1
        } else {
            self.rect.r1
        };
        let d0 = self.data_start();
        (d0, r1.max(d0))
    }

    pub fn totals_row(&self) -> Option<u32> {
        self.totals.then_some(self.rect.r1)
    }

    pub fn column_index(&self, name: &str) -> Option<usize> {
        self.columns
            .iter()
            .position(|c| crate::text::eq_ordinal_ci(c, name))
    }

    pub fn contains(&self, sheet: SheetId, row: u32, col: u32) -> bool {
        self.sheet == sheet && self.rect.contains(row, col)
    }

    pub fn resolve(
        &self,
        items: Items,
        cols: Option<(usize, usize)>,
        host_row: u32,
    ) -> Option<Result<Rect, ()>> {
        let (c0, c1) = match cols {
            Some((a, b)) => (
                self.rect.c0 + a.min(b) as u32,
                self.rect.c0 + a.max(b) as u32,
            ),
            None => (self.rect.c0, self.rect.c1),
        };
        let (d0, d1) = self.data_rows();
        if items.this_row {
            return Some(if (d0..=d1).contains(&host_row) {
                Ok(Rect::new(host_row, c0, host_row, c1))
            } else {
                Err(())
            });
        }
        let mut rows: Vec<(u32, u32)> = Vec::new();
        if items.headers {
            match self.header_row() {
                Some(h) => rows.push((h, h)),
                None if !items.data && !items.totals => return None,
                None => {}
            }
        }
        if items.data {
            rows.push((d0, d1));
        }
        if items.totals {
            match self.totals_row() {
                Some(t) => rows.push((t, t)),
                None if !items.data && !items.headers => return None,
                None => {}
            }
        }
        let r0 = rows.iter().map(|r| r.0).min()?;
        let r1 = rows.iter().map(|r| r.1).max()?;
        Some(Ok(Rect::new(r0, c0, r1, c1)))
    }
}

fn map_struct_refs(
    node: &crate::formula::Node,
    f: &mut dyn FnMut(&str, &Spec) -> Option<Spec>,
) -> crate::formula::Node {
    use crate::formula::Node;
    crate::formula::map_node(node, &mut |n| match n {
        Node::StructRef {
            table,
            implicit,
            spec,
        } => f(table, spec).map(|s| Node::StructRef {
            table: table.clone(),
            implicit: *implicit,
            spec: s,
        }),
        _ => None,
    })
}

pub fn table_dep_key(name: &str) -> crate::value::Text {
    format!("\u{1}TABLE:{}", name.to_uppercase()).into()
}

use crate::address::CellAddr;
use crate::calc::{self, RecalcReport};
use crate::value::Value;
use crate::workbook::{EditError, Entry, Workbook};

impl Workbook {
    pub fn table_by_name(&self, name: &str) -> Option<&Table> {
        self.tables
            .iter()
            .find(|t| crate::text::eq_ordinal_ci(&t.name, name))
    }

    pub fn table_at(&self, addr: CellAddr) -> Option<&Table> {
        self.tables
            .iter()
            .find(|t| t.contains(addr.sheet, addr.row, addr.col))
    }

    pub(crate) fn header_text(&mut self, addr: CellAddr, v: &Value) -> String {
        if let Value::Number(n) = v {
            if self.cellfmt.get(addr).is_some() {
                if let Some(nf) = self.compiled_format(addr) {
                    if let Ok(r) = nf.format_number(*n, &self.locale, self.settings.date_system) {
                        return r.text;
                    }
                }
            }
        }
        crate::coerce::to_text(v)
            .map(|s| s.to_string())
            .unwrap_or_else(|e| e.to_string())
    }

    pub fn add_table(
        &mut self,
        name: &str,
        sheet: SheetId,
        rect: Rect,
    ) -> Result<RecalcReport, EditError> {
        crate::structure::range_too_large(&rect)?;

        let mut rect = rect;
        loop {
            let grown = self
                .merges(sheet)
                .iter()
                .filter(|m| m.intersects(&rect))
                .fold(rect, |a, m| {
                    Rect::new(
                        a.r0.min(m.r0),
                        a.c0.min(m.c0),
                        a.r1.max(m.r1),
                        a.c1.max(m.c1),
                    )
                });
            if grown == rect {
                break;
            }
            rect = grown;
        }

        if self
            .array_blocks(sheet)
            .iter()
            .any(|(_, b)| b.intersects(&rect))
        {
            return Err(EditError::Compile(
                "multi-cell array formulas aren't allowed in tables".into(),
            ));
        }
        self.unmerge_for_table(sheet, rect);
        if name.is_empty() || self.table_by_name(name).is_some() {
            return Err(EditError::BadName(name.to_string()));
        }
        if rect.rows() < 2
            || self
                .tables
                .iter()
                .any(|t| t.sheet == sheet && t.rect.intersects(&rect))
        {
            return Err(EditError::Compile(
                "tables need a header row and a data row, and cannot overlap".into(),
            ));
        }
        let mut columns: Vec<String> = Vec::new();

        let named: Vec<Option<String>> = (rect.c0..=rect.c1)
            .map(|c| match self.value(CellAddr::new(sheet, rect.r0, c)) {
                Value::Blank => None,
                v => Some(self.header_text(CellAddr::new(sheet, rect.r0, c), &v)),
            })
            .collect();

        let key = |s: &str| s.chars().map(crate::text::fold_upper).collect::<String>();
        let named_keys: std::collections::HashSet<String> =
            named.iter().flatten().map(|s| key(s)).collect();
        let mut seen: std::collections::HashSet<String> = std::collections::HashSet::new();
        let mut next_blank = 1u32;
        for given in &named {
            let mut n = match given {
                None => {
                    loop {
                        let k = key(&format!("Column{next_blank}"));
                        if named_keys.contains(&k) || seen.contains(&k) {
                            next_blank += 1;
                        } else {
                            break;
                        }
                    }
                    format!("Column{next_blank}")
                }
                Some(s) => s.clone(),
            };
            if seen.contains(&key(&n)) {
                let mut k = 2;
                while seen.contains(&key(&format!("{n}{k}"))) {
                    k += 1;
                }
                n = format!("{n}{k}");
            }
            seen.insert(key(&n));
            columns.push(n);
        }

        for (i, name) in columns.iter().enumerate() {
            let a = CellAddr::new(sheet, rect.r0, rect.c0 + i as u32);
            let fid = self.formula_at(a);
            if fid.is_some() || !matches!(self.value(a), Value::Text(ref t) if **t == **name) {
                if fid.is_some() {
                    self.remove_cell(a);
                }
                if let Some(sh) = self.sheet_mut(sheet) {
                    sh.cells
                        .set(a.row, a.col, Entry::Const(Value::from(name.as_str())));
                }
                calc::cell_changed(self, a);
            }
        }
        self.tables_made += 1;
        self.tables.push(Table {
            name: name.to_string(),
            sheet,
            rect,
            totals: false,
            calc: vec![false; columns.len()],
            calc_key: Vec::new(),
            columns,
            headers: true,
            totals_code: None,
        });

        let anchors: Vec<_> = self
            .sheet(sheet)
            .map(|sh| {
                sh.cells
                    .iter_rect(rect)
                    .filter_map(|(r, c, _)| self.formula_at(CellAddr::new(sheet, r, c)))
                    .collect::<Vec<_>>()
            })
            .unwrap_or_default()
            .into_iter()
            .filter(|&f| self.formula(f).spill.is_some_and(|s| !s.is_cell()))
            .collect();
        for f in anchors {
            calc::mark_dirty(self, f);
        }
        Ok(self.table_changed(name))
    }

    pub fn set_table_totals(&mut self, name: &str, show: bool) -> Result<RecalcReport, EditError> {
        let i = self
            .tables
            .iter()
            .position(|t| crate::text::eq_ordinal_ci(&t.name, name))
            .ok_or(EditError::NoSuchSheet)?;
        let t = self.tables[i].clone();
        if t.totals == show {
            return Ok(RecalcReport::default());
        }

        let row = if show { t.rect.r1 + 1 } else { t.rect.r1 };
        self.guard_arrays_shift(&crate::structure::Shift {
            sheet: t.sheet,
            axis: crate::structure::Axis::Rows,
            at: row,
            count: 1,
            insert: show,
            limit: Some((t.rect.c0, t.rect.c1)),
            cells: false,
        })?;
        let mode = self.settings.mode;
        self.settings.mode = crate::workbook::CalcMode::Manual;

        let shift = |wb: &mut Workbook, at: u32, insert: bool| {
            let _ = wb.shift(crate::structure::Shift {
                sheet: t.sheet,
                axis: crate::structure::Axis::Rows,
                at,
                count: 1,
                insert,
                limit: Some((t.rect.c0, t.rect.c1)),
                cells: false,
            });
        };
        if show {
            let row = t.rect.r1 + 1;
            shift(self, row, true);
            self.tables[i].rect.r1 = row;
            self.tables[i].totals = true;
            let first = CellAddr::new(t.sheet, row, t.rect.c0);
            let last = CellAddr::new(t.sheet, row, t.rect.c1);
            self.set_value(first, Value::from("Total"));
            let (d0, d1) = t.data_rows();

            let kinds: Vec<Value> = (d0..=d1)
                .map(|r| self.value(CellAddr::new(t.sheet, r, t.rect.c1)))
                .filter(|v| !matches!(v, Value::Blank))
                .collect();
            let numeric = !kinds.is_empty() && kinds.iter().all(|v| matches!(v, Value::Number(_)));
            let code = t.totals_code.unwrap_or(if numeric { 109 } else { 103 });
            let col = escape_column(t.columns.last().unwrap());
            self.set_formula(last, &format!("=SUBTOTAL({code},[{col}])"))?;
        } else {
            let row = t.rect.r1;
            let cell = CellAddr::new(t.sheet, row, t.rect.c1);
            let code = self.formula_at(cell).and_then(|f| {
                let txt = self.formula(f).text.to_string();
                let rest = txt.strip_prefix("SUBTOTAL(")?;
                rest.split(',').next()?.parse::<u32>().ok()
            });
            self.tables[i].totals_code = code.or(t.totals_code);
            self.tables[i].rect.r1 = row - 1;
            self.tables[i].totals = false;
            shift(self, row, false);
        }
        self.settings.mode = mode;
        Ok(self.table_changed(name))
    }

    pub(crate) fn table_calc_fill(&mut self, addr: CellAddr) {
        let Some(ti) = self.tables.iter().position(|t| {
            let (d0, d1) = t.data_rows();
            t.sheet == addr.sheet
                && (d0..=d1).contains(&addr.row)
                && (t.rect.c0..=t.rect.c1).contains(&addr.col)
                && !(t.totals && addr.row == t.rect.r1)
        }) else {
            return;
        };
        let t = &self.tables[ti];
        let (d0, d1) = t.data_rows();
        let idx = (addr.col - t.rect.c0) as usize;
        let calc = t.calc.get(idx).copied().unwrap_or(false);
        let others: Vec<Option<bool>> = (d0..=d1)
            .filter(|&r| r != addr.row)
            .map(|r| {
                match self
                    .sheet(addr.sheet)
                    .and_then(|s| s.cells.get(r, addr.col))
                {
                    None => None,
                    Some(Entry::Const(_)) => Some(false),
                    Some(_) => Some(true),
                }
            })
            .collect();
        let ok = if calc {
            let keys: std::collections::BTreeSet<String> = (d0..=d1)
                .filter(|&r| r != addr.row)
                .filter_map(|r| self.formula_at(CellAddr::new(addr.sheet, r, addr.col)))
                .map(|f| format!("{:?}", self.formula(f).formula.root))
                .collect();
            others.iter().all(|o| *o != Some(false)) && keys.len() <= 1
        } else {
            others.iter().all(|o| o.is_none())
        };
        if !ok {
            return;
        }

        if let Some(fid) = self.formula_at(addr) {
            if !self.fill_fits(fid, addr.sheet, addr.col, d0..=d1) {
                return;
            }
        }
        if self.tables[ti].calc.len() <= idx {
            let n = self.tables[ti].columns.len();
            self.tables[ti].calc.resize(n, false);
        }
        self.tables[ti].calc[idx] = true;
        if let Some(fid) = self.formula_at(addr) {
            let key = format!("{:?}", self.formula(fid).formula.root);
            let name = self.tables[ti].columns[idx].to_lowercase();
            let keys = &mut self.tables[ti].calc_key;
            keys.retain(|(n, _)| *n != name);
            keys.push((name, key));
        }
        let src = Rect::new(addr.row, addr.col, addr.row, addr.col);
        let mode = self.settings.mode;
        self.settings.mode = crate::workbook::CalcMode::Manual;
        let src_fmt: Arc<str> = Arc::from(self.number_format(addr));
        let src_general = src_fmt.eq_ignore_ascii_case("general");
        let mut filled: Vec<CellAddr> = Vec::new();
        let src_key = self
            .formula_at(addr)
            .map(|f| format!("{:?}", self.formula(f).formula.root));
        for r in d0..=d1 {
            if r != addr.row {
                if src_key.is_some()
                    && self
                        .formula_at(CellAddr::new(addr.sheet, r, addr.col))
                        .map(|f| format!("{:?}", self.formula(f).formula.root))
                        == src_key
                {
                    continue;
                }

                let to = CellAddr::new(addr.sheet, r, addr.col);
                let cur: Arc<str> = Arc::from(self.number_format(to));
                let own = self.own_format(to);
                let _ = self.copy_range(addr.sheet, src, to);
                filled.push(to);

                if let (crate::workbook::CalcMode::Automatic, Some(fid)) =
                    (mode, self.formula_at(to))
                {
                    self.formula_mut(fid).value = Value::Number(0.0);
                }
                self.restore_own_format(to, own);
                if cur.eq_ignore_ascii_case("general") {
                    self.store_format(to, &src_fmt);
                } else if src_general && Workbook::is_yielding_format(&cur) {
                    self.store_format(to, "General");
                }
            }
        }
        self.settings.mode = mode;
        if mode == crate::workbook::CalcMode::Automatic {
            calc::recalc(self);
        } else {
            calc::recalc_only(self, &filled);
        }
    }

    pub(crate) fn fill_fits(
        &self,
        fid: crate::workbook::FormulaId,
        sheet: SheetId,
        col: u32,
        rows: std::ops::RangeInclusive<u32>,
    ) -> bool {
        let root = &self.formula(fid).formula.root;
        rows.into_iter().all(|r| {
            let to = CellAddr::new(sheet, r, col);
            let mut ok = true;
            crate::structure::map_refs(root, &mut |_, cref| {
                if crate::formula_print::cref_text(cref, to).is_none() {
                    ok = false;
                }
                None
            });
            ok
        })
    }

    pub(crate) fn table_fill_new_rows(
        &mut self,
        ti: usize,
        rows: (u32, u32),
        sources: &[(u32, FormulaId, Arc<Formula>)],
    ) {
        let mut filled: Vec<CellAddr> = Vec::new();
        let t = self.tables[ti].clone();
        let (d0, d1) = t.data_rows();
        for (idx, &is_calc) in t.calc.iter().enumerate() {
            if !is_calc {
                continue;
            }
            let col = t.rect.c0 + idx as u32;
            let sheet = t.sheet;
            let snap = sources.iter().find(|s| s.0 == col).cloned();
            let found = match snap {
                Some((_, fid, arc)) => Some((self.formula(fid).addr.row, Some((fid, arc)))),
                None => {
                    let order: Vec<u32> = (d0..rows.0).rev().chain(rows.1 + 1..=d1).collect();
                    self.calc_source_row(&t, idx, &order).map(|r| (r, None))
                }
            };
            let Some((sr, swap)) = found else { continue };
            let sf = self.formula_at(CellAddr::new(sheet, sr, col)).unwrap();
            if !self.fill_fits(sf, sheet, col, rows.0..=rows.1) {
                continue;
            }

            let kept = swap.map(|(fid, arc)| {
                let old = std::mem::replace(&mut self.formula_mut(fid).formula, arc);
                (fid, old)
            });
            let from = Rect::new(sr, col, sr, col);
            for r in rows.0..=rows.1 {
                if self
                    .sheet(sheet)
                    .is_some_and(|s| s.cells.get(r, col).is_some())
                {
                    continue;
                }
                let _ = self.copy_range(sheet, from, CellAddr::new(sheet, r, col));
                filled.push(CellAddr::new(sheet, r, col));
            }
            if let Some((fid, old)) = kept {
                self.formula_mut(fid).formula = old;
            }
        }

        if self.settings.mode != crate::workbook::CalcMode::Automatic && !filled.is_empty() {
            calc::recalc_only(self, &filled);
        }
    }

    pub(crate) fn calc_source_row(&self, t: &Table, idx: usize, order: &[u32]) -> Option<u32> {
        let col = t.rect.c0 + idx as u32;
        let formula = |r: u32| self.formula_at(CellAddr::new(t.sheet, r, col));
        let name = t.columns[idx].to_lowercase();
        let key = t.calc_key.iter().find(|(n, _)| *n == name).map(|(_, k)| k);
        if let Some(key) = key {
            let own = order.iter().copied().find(|&r| {
                formula(r).is_some_and(|f| format!("{:?}", self.formula(f).formula.root) == *key)
            });
            if own.is_some() {
                return own;
            }
        }
        order.iter().copied().find(|&r| formula(r).is_some())
    }

    pub(crate) fn calc_column_sources(
        &self,
        t: &Table,
        at: u32,
    ) -> Vec<(u32, FormulaId, Arc<Formula>)> {
        let (d0, d1) = t.data_rows();
        let mut out = Vec::new();
        for (idx, &is_calc) in t.calc.iter().enumerate() {
            if !is_calc {
                continue;
            }
            let col = t.rect.c0 + idx as u32;
            let order: Vec<u32> = (d0..at.max(d0)).rev().chain(at..=d1).collect();

            let hit = self
                .formula_at(CellAddr::new(t.sheet, d0, col))
                .or_else(|| {
                    self.calc_source_row(t, idx, &order)
                        .and_then(|r| self.formula_at(CellAddr::new(t.sheet, r, col)))
                });
            if let Some(fid) = hit {
                out.push((col, fid, self.formula(fid).formula.clone()));
            }
        }
        out
    }

    pub(crate) fn table_expansion(
        &self,
        addr: CellAddr,
        is_formula: bool,
    ) -> Option<(usize, Rect, bool)> {
        let ti = self.tables.iter().position(|t| {
            t.sheet == addr.sheet
                && !t.rect.contains(addr.row, addr.col)
                && ((!t.totals
                    && !is_formula
                    && addr.row == t.rect.r1 + 1
                    && (t.rect.c0..=t.rect.c1).contains(&addr.col))
                    || (t.headers
                        && addr.col == t.rect.c1 + 1
                        && (t.rect.r0..=t.rect.r1).contains(&addr.row)))
        })?;
        let mut rect = self.tables[ti].rect;
        let grow_col = addr.col == rect.c1 + 1 && addr.row <= rect.r1;
        if grow_col {
            rect.c1 += 1;
        } else {
            rect.r1 += 1;
        }
        if self
            .tables
            .iter()
            .enumerate()
            .any(|(i, o)| i != ti && o.sheet == addr.sheet && o.rect.intersects(&rect))
        {
            return None;
        }

        let (r0, c0) = if grow_col {
            (rect.r0, rect.c1)
        } else {
            (rect.r1, rect.c0)
        };
        for r in r0..=rect.r1 {
            for c in c0..=rect.c1 {
                let a = CellAddr::new(addr.sheet, r, c);
                if a != addr && !matches!(self.value(a), Value::Blank) {
                    return None;
                }
            }
        }
        Some((ti, rect, grow_col))
    }

    pub(crate) fn table_formula_under(&mut self, addr: CellAddr) {
        let Some(ti) = self.tables.iter().position(|t| {
            t.sheet == addr.sheet
                && !t.totals
                && addr.row == t.rect.r1 + 1
                && (t.rect.c0..=t.rect.c1).contains(&addr.col)
        }) else {
            return;
        };
        let mut rect = self.tables[ti].rect;
        rect.r1 += 1;
        if self
            .tables
            .iter()
            .enumerate()
            .any(|(i, o)| i != ti && o.sheet == addr.sheet && o.rect.intersects(&rect))
        {
            return;
        }

        let busy = (rect.c0..=rect.c1).any(|c| {
            c != addr.col
                && self
                    .sheet(addr.sheet)
                    .is_some_and(|s| s.cells.get(rect.r1, c).is_some())
        });
        if busy {
            return;
        }
        self.tables[ti].rect = rect;
        self.tables[ti].totals = true;
        self.extend_row_formats(addr.sheet, rect);
        let name = self.tables[ti].name.clone();
        let _ = self.table_changed(&name);
    }

    fn table_headers_at(&self, sheet: SheetId, r0: u32) -> bool {
        self.tables
            .iter()
            .find(|t| t.sheet == sheet && t.rect.r0 == r0)
            .is_none_or(|t| t.headers)
    }

    pub(crate) fn extend_row_formats(&mut self, sheet: SheetId, rect: Rect) {
        if !self.cellfmt.any() {
            return;
        }
        let row = rect.r1;
        let d0 = rect.r0 + u32::from(self.table_headers_at(sheet, rect.r0));
        if row <= d0 {
            return;
        }
        let n = row - d0;
        let need = n.min(3) as usize;
        for c in rect.c0..=rect.c1 {
            let mut seen: Vec<(Arc<str>, usize)> = Vec::new();
            for r in row.saturating_sub(5).max(d0)..row {
                if let Some(f) = self.cellfmt.get(CellAddr::new(sheet, r, c)) {
                    if f.eq_ignore_ascii_case("general") {
                        continue;
                    }
                    match seen.iter_mut().find(|(g, _)| g == f) {
                        Some(e) => e.1 += 1,
                        None => seen.push((f.clone(), 1)),
                    }
                }
            }
            if let Some((f, _)) = seen.into_iter().find(|(_, k)| *k >= need) {
                self.store_format(CellAddr::new(sheet, row, c), &f);
            }
        }
    }

    fn extend_col_formats(&mut self, ti: usize) {
        if !self.cellfmt.any() {
            return;
        }
        let t = &self.tables[ti];
        let (sheet, rect) = (t.sheet, t.rect);
        let (d0, d1) = t.data_rows();
        if rect.c1 == rect.c0 || d1 <= rect.r0 {
            return;
        }
        let n = rect.c1 - rect.c0;
        let need = n.min(3) as usize;
        let mut seen: Vec<(Arc<str>, usize)> = Vec::new();
        for c in rect.c1.saturating_sub(5).max(rect.c0)..rect.c1 {
            if let Some(f) = self.cellfmt.get(CellAddr::new(sheet, d1, c)) {
                if f.eq_ignore_ascii_case("general") {
                    continue;
                }
                match seen.iter_mut().find(|(g, _)| g == f) {
                    Some(e) => e.1 += 1,
                    None => seen.push((f.clone(), 1)),
                }
            }
        }
        if let Some((f, _)) = seen.into_iter().find(|(_, k)| *k >= need) {
            for r in d0..=d1 {
                self.store_format(CellAddr::new(sheet, r, rect.c1), &f);
            }
        }
    }

    pub(crate) fn table_auto_expand(&mut self, addr: CellAddr, is_formula: bool) {
        let Some((ti, rect, grow_col)) = self.table_expansion(addr, is_formula) else {
            return;
        };
        let name = self.tables[ti].name.clone();
        let old = self.tables[ti].rect;
        self.tables[ti].rect = rect;
        let refs_grew = !grow_col && self.grow_exact_refs(ti, old, rect.r1 - old.r1, false);
        if grow_col {
            self.extend_col_formats(ti);
        } else {
            self.extend_row_formats(addr.sheet, rect);
        }
        if grow_col {
            let hdr = CellAddr::new(addr.sheet, rect.r0, rect.c1);

            let next = match self.value(hdr) {
                Value::Blank => self.tables[ti]
                    .columns
                    .last()
                    .and_then(|prev| next_header_name(prev, &self.tables[ti].columns)),
                _ => None,
            };
            self.tables[ti].columns.push(String::new());
            self.tables[ti].calc.push(false);
            if let Some(n) = next {
                if let Some(sh) = self.sheet_mut(addr.sheet) {
                    sh.cells.set(
                        hdr.row,
                        hdr.col,
                        crate::workbook::Entry::Const(Value::from(n.as_str())),
                    );
                }
                calc::cell_changed(self, hdr);
            }
            self.header_edited(hdr);
        }
        if refs_grew {
            let _ = self.relink_and_recalc();
        }
        let _ = self.table_changed(&name);
        if !grow_col {
            self.table_fill_new_rows(ti, (rect.r1, rect.r1), &[]);
        }
    }

    pub(crate) fn header_edited(&mut self, addr: CellAddr) {
        let Some(ti) = self.tables.iter().position(|t| {
            t.sheet == addr.sheet
                && t.header_row() == Some(addr.row)
                && (t.rect.c0..=t.rect.c1).contains(&addr.col)
        }) else {
            return;
        };
        let idx = (addr.col - self.tables[ti].rect.c0) as usize;
        let old = self.tables[ti].columns[idx].clone();

        let numeric = matches!(self.value(addr), Value::Number(_));
        let cur_value = self.value(addr);
        let cur_base = match &cur_value {
            Value::Blank => String::new(),
            v => self.header_text(addr, v),
        };
        let others: Vec<&String> = {
            let cols = &self.tables[ti].columns;
            if numeric {
                (0..cols.len())
                    .filter(|&j| j != idx)
                    .map(|j| &cols[j])
                    .collect()
            } else {
                cols[..idx].iter().collect()
            }
        };
        let taken = |n: &str| others.iter().any(|c| crate::text::eq_ordinal_ci(c, n));
        let new = match self.value(addr) {
            Value::Blank => {
                let used: std::collections::HashSet<u64> = self.tables[ti]
                    .columns
                    .iter()
                    .enumerate()
                    .filter(|&(i, _)| i != idx)
                    .filter_map(|(_, c)| {
                        let (h, d) = c.split_at_checked(6)?;
                        let canon = !d.is_empty()
                            && d.bytes().all(|b| b.is_ascii_digit())
                            && !d.starts_with('0');
                        (h.eq_ignore_ascii_case("column") && canon)
                            .then(|| d.parse::<u64>().ok())
                            .flatten()
                    })
                    .collect();
                let mut k = 1u64;
                while used.contains(&k) {
                    k += 1;
                }
                format!("Column{k}")
            }
            _ => {
                let base = cur_base;
                if taken(&base) {
                    let fold = |n: &str| n.chars().map(crate::text::fold_upper).collect::<String>();
                    let fb = fold(&base);
                    let used: std::collections::HashSet<u64> = others
                        .iter()
                        .filter_map(|c| {
                            let d = if c.is_ascii() && fb.is_ascii() {
                                let (h, d) = c.split_at_checked(fb.len())?;
                                h.eq_ignore_ascii_case(&fb).then_some(d.to_string())?
                            } else {
                                fold(c).strip_prefix(fb.as_str())?.to_string()
                            };
                            let canon = !d.is_empty()
                                && d.bytes().all(|b| b.is_ascii_digit())
                                && !d.starts_with('0');
                            canon.then(|| d.parse::<u64>().ok()).flatten()
                        })
                        .collect();
                    let mut k = 2u64;
                    while used.contains(&k) {
                        k += 1;
                    }
                    format!("{base}{k}")
                } else {
                    base
                }
            }
        };
        if !matches!(self.value(addr), Value::Text(ref s) if **s == *new) {
            if self.formula_at(addr).is_some() {
                self.remove_cell(addr);
            }
            if self.sheet(addr.sheet).is_some() {
                self.sheet_mut(addr.sheet).unwrap().cells.set(
                    addr.row,
                    addr.col,
                    crate::workbook::Entry::Const(Value::from(new.as_str())),
                );
            }
        }

        let n_cols = self.tables[ti].columns.len();
        for j in idx + 1..n_cols {
            let cur = self.tables[ti].columns[j].clone();
            if !crate::text::eq_ordinal_ci(&cur, &new) {
                continue;
            }
            let mut k = 2;
            let fresh = loop {
                let cand = format!("{cur}{k}");
                if !self.tables[ti]
                    .columns
                    .iter()
                    .any(|c| crate::text::eq_ordinal_ci(c, &cand))
                    && !crate::text::eq_ordinal_ci(&cand, &new)
                {
                    break cand;
                }
                k += 1;
            };
            let t = &self.tables[ti];
            let (sheet, row, col) = (t.sheet, t.rect.r0, t.rect.c0 + j as u32);
            self.tables[ti].columns[j] = fresh.clone();
            self.sheet_mut(sheet).unwrap().cells.set(
                row,
                col,
                crate::workbook::Entry::Const(Value::from(fresh.as_str())),
            );
            calc::cell_changed(self, CellAddr::new(sheet, row, col));
            self.rename_table_column(ti, &cur, &fresh);
        }
        self.tables[ti].columns[idx] = new.clone();
        if old == new {
            return;
        }
        self.rename_table_column(ti, &old, &new);
    }

    fn rename_table_column(&mut self, ti: usize, old: &str, new: &str) {
        let (old, new) = (old.to_string(), new.to_string());
        let table_name = self.tables[ti].name.clone();
        let rename = |spec: &Spec| -> Option<Spec> {
            let (a, b) = spec.cols.as_ref()?;
            if !crate::text::eq_ordinal_ci(a, &old) && !crate::text::eq_ordinal_ci(b, &old) {
                return None;
            }
            let fix = |c: &String| {
                if crate::text::eq_ordinal_ci(c, &old) {
                    new.clone()
                } else {
                    c.clone()
                }
            };
            Some(Spec {
                cols: Some((fix(a), fix(b))),
                ..spec.clone()
            })
        };
        let ids: Vec<crate::workbook::FormulaId> = self.formula_ids().collect();
        for fid in ids {
            let formula = self.formula(fid).formula.clone();
            let mut changed = false;
            let root = map_struct_refs(&formula.root, &mut |table, spec| {
                if !crate::text::eq_ordinal_ci(table, &table_name) {
                    return None;
                }
                let r = rename(spec);
                changed |= r.is_some();
                r
            });
            if changed {
                self.formula_mut(fid).formula = std::sync::Arc::new(crate::formula::Formula {
                    root,
                    volatile: formula.volatile,
                });
            }
        }
        let _ = self.table_changed(&table_name);
    }

    pub(crate) fn table_changed(&mut self, name: &str) -> RecalcReport {
        calc::name_changed(self, &table_dep_key(name));
        calc::name_changed(self, &name.to_uppercase());
        if self.settings.mode == crate::workbook::CalcMode::Automatic {
            calc::recalc(self)
        } else {
            RecalcReport::default()
        }
    }
}

pub fn escape_column(name: &str) -> String {
    let mut s = String::with_capacity(name.len());
    for c in name.chars() {
        if matches!(c, '[' | ']' | '#' | '\'') {
            s.push('\'');
        }
        s.push(c);
    }
    s
}

fn needs_brackets(name: &str) -> bool {
    name.chars()
        .any(|c| !(c.is_alphanumeric() || c == '_' || c == '.'))
}

#[derive(Clone, Debug, PartialEq, Default)]
pub struct Spec {
    pub items: Items,

    pub explicit_data: bool,

    pub cols: Option<(String, String)>,

    pub range: bool,

    pub pad: (String, String),
}

pub fn parse_spec(inner: &str) -> Result<Spec, String> {
    let s = inner.trim();
    let mut spec = Spec::default();

    if !s.is_empty() && !s.contains('[') && !s.starts_with(['@', '#']) {
        let lead = inner.len() - inner.trim_start().len();
        let trail = inner.len() - inner.trim_end().len();
        spec.pad = (
            inner[..lead].to_string(),
            inner[inner.len() - trail..].to_string(),
        );
        spec.items = Items::DATA;
        let n = unescape(s);
        spec.cols = Some((n.clone(), n));
        return Ok(spec);
    }
    if !s.is_empty() && !s.starts_with('@') {
        let lead = inner.len() - inner.trim_start().len();
        let trail = inner.len() - inner.trim_end().len();
        spec.pad = (
            inner[..lead].to_string(),
            inner[inner.len() - trail..].to_string(),
        );
    }
    if s.is_empty() {
        spec.items = Items::DATA;
        return Ok(spec);
    }

    let parts = split_top(s, ',');
    let mut col_parts: Vec<String> = Vec::new();
    let mut any_item = false;
    let (mut all, mut named) = (false, 0);
    for (i, part) in parts.iter().enumerate() {
        let p = part.trim();
        if i == 0 && p.starts_with('@') {
            spec.items.this_row = true;
            any_item = true;
            let rest = p[1..].trim();
            if !rest.is_empty() {
                col_parts.push(rest.to_string());
            }
            continue;
        }
        let bare = strip_brackets(p);
        if let Some(item) = bare.strip_prefix('#') {
            any_item = true;
            if item.eq_ignore_ascii_case("all") {
                all = true;
            } else {
                named += 1;
            }
            match item.to_ascii_lowercase().as_str() {
                "all" => {
                    spec.items = Items {
                        headers: true,
                        data: true,
                        totals: true,
                        ..spec.items
                    }
                }
                "data" => {
                    spec.items.data = true;
                    spec.explicit_data = true;
                }
                "headers" => spec.items.headers = true,
                "totals" => spec.items.totals = true,
                "this row" => spec.items.this_row = true,
                other => return Err(format!("unknown item #{other}")),
            }
        } else {
            col_parts.push(p.to_string());
        }
    }
    if !any_item {
        spec.items = Items::DATA;
    }

    let it = spec.items;
    if all && named > 0
        || it.this_row && (named > 1 || all || spec.items.headers || spec.items.totals)
        || it.headers && it.totals && !it.data
    {
        return Err("invalid combination of items".into());
    }
    match col_parts.len() {
        0 => {}
        1 => {
            let range = split_top(&col_parts[0], ':');
            let names: Vec<String> = range
                .iter()
                .map(|x| unescape(strip_brackets(x.trim())))
                .collect();
            if names.iter().any(|n| n == "@") {
                return Err("'@' is not a column".into());
            }
            spec.range = names.len() == 2;
            spec.cols = match names.as_slice() {
                [a] => Some((a.clone(), a.clone())),
                [a, b] => Some((a.clone(), b.clone())),
                _ => return Err("bad column range".into()),
            };
        }
        _ => return Err("too many column specifiers".into()),
    }
    Ok(spec)
}

fn strip_brackets(s: &str) -> &str {
    let s = s.trim();
    if s.starts_with('[') && s.ends_with(']') && s.len() >= 2 {
        &s[1..s.len() - 1]
    } else {
        s
    }
}

fn unescape(s: &str) -> String {
    let mut out = String::with_capacity(s.len());
    let mut it = s.chars();
    while let Some(c) = it.next() {
        if c == '\'' {
            if let Some(n) = it.next() {
                out.push(n);
            }
        } else {
            out.push(c);
        }
    }
    out
}

fn split_top(s: &str, sep: char) -> Vec<String> {
    let mut parts = Vec::new();
    let mut depth = 0i32;
    let mut cur = String::new();
    let mut escape = false;
    for c in s.chars() {
        if escape {
            cur.push(c);
            escape = false;
            continue;
        }
        match c {
            '\'' => {
                escape = true;
                cur.push(c);
            }
            '[' => {
                depth += 1;
                cur.push(c);
            }
            ']' => {
                depth -= 1;
                cur.push(c);
            }
            c if c == sep && depth == 0 => parts.push(std::mem::take(&mut cur)),
            c => cur.push(c),
        }
    }
    parts.push(cur);
    parts
}

pub fn spec_text(table: Option<&str>, spec: &Spec) -> String {
    let mut s = String::new();
    if let Some(t) = table {
        s.push_str(t);
    }
    let it = spec.items;
    let item_list: Vec<&str> = if it.this_row {
        vec![]
    } else if it.headers && it.data && it.totals {
        vec!["#All"]
    } else {
        let mut v = Vec::new();
        if it.headers {
            v.push("#Headers");
        }
        if it.data && (it.headers || it.totals || spec.explicit_data) {
            v.push("#Data");
        }
        if it.totals {
            v.push("#Totals");
        }
        v
    };
    let col_text = |a: &str, b: &str, force: bool| -> String {
        if a == b && !force && !spec.range {
            if needs_brackets(a) {
                format!("[{}]", escape_column(a))
            } else {
                escape_column(a)
            }
        } else if a == b && !spec.range {
            format!("[{}]", escape_column(a))
        } else {
            format!("[{}]:[{}]", escape_column(a), escape_column(b))
        }
    };
    s.push('[');
    if it.this_row {
        s.push('@');
        if let Some((a, b)) = &spec.cols {
            s.push_str(&col_text(a, b, false));
        }
    } else if item_list.is_empty() {
        if let Some((a, b)) = &spec.cols {
            if a == b && !spec.range {
                s.push_str(&spec.pad.0);
                s.push_str(&escape_column(a));
                s.push_str(&spec.pad.1);
            } else {
                s.push_str(&col_text(a, b, true));
            }
        } else if table.is_some() {
            s.pop();
            return s;
        }
    } else if spec.cols.is_none() && item_list.len() == 1 {
        s.push_str(item_list[0]);
    } else {
        let mut parts: Vec<String> = item_list.iter().map(|i| format!("[{i}]")).collect();
        if let Some((a, b)) = &spec.cols {
            parts.push(col_text(a, b, true));
        }
        s.push_str(&parts.join(","));
    }
    s.push(']');
    s
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn specs() {
        let t = |x: &str| spec_text(Some("Sales"), &parse_spec(x).unwrap());
        assert_eq!(t("Qty"), "Sales[Qty]");
        assert_eq!(t("[#This Row],[Qty]"), "Sales[@Qty]");
        assert_eq!(t("@[Unit Price]"), "Sales[@[Unit Price]]");
        assert_eq!(t("[#All],[Qty]"), "Sales[[#All],[Qty]]");
        assert_eq!(t("#Headers"), "Sales[#Headers]");
        assert_eq!(
            t("[#Headers],[Item]:[Qty]"),
            "Sales[[#Headers],[Item]:[Qty]]"
        );
        assert_eq!(t("[Qty]:[Unit Price]"), "Sales[[Qty]:[Unit Price]]");
        assert_eq!(t("Unit Price"), "Sales[Unit Price]");
        assert_eq!(t("#Data"), "Sales[#Data]");
        assert_eq!(t(""), "Sales");
    }
}

pub(crate) fn next_header_name(prev: &str, columns: &[String]) -> Option<String> {
    let base = prev.trim_end_matches(' ');
    let digits = base
        .bytes()
        .rev()
        .take_while(|b| b.is_ascii_digit())
        .count();
    if digits == 0 || digits > 15 {
        return None;
    }
    let (head, num) = base.split_at(base.len() - digits);
    let n: u64 = num.parse().ok()?;
    if n > u32::MAX as u64 {
        return None;
    }
    let next = (n + 1) as u32 as u64;
    let width = if next == 0 { 0 } else { digits };
    let cand = format!("{head}{next:0width$}");
    let taken = |c: &str| columns.iter().any(|o| crate::text::eq_ordinal_ci(o, c));
    if !taken(&cand) {
        return Some(cand);
    }
    (2..).map(|k| format!("{prev}{k}")).find(|c| !taken(c))
}
