use super::Command;
use crate::address::{Rect, SheetId};
use crate::formula::Formula;
use crate::workbook::FormulaId;
use std::sync::atomic::{AtomicU64, Ordering};
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

pub(crate) const TOTALS_CODES: [u32; 8] = [109, 101, 103, 102, 105, 104, 107, 110];

#[derive(Clone, Copy, PartialEq, Eq, Hash, Debug)]
pub struct TableId(u64);

impl TableId {
    pub(crate) fn fresh() -> TableId {
        static NEXT: AtomicU64 = AtomicU64::new(1);
        TableId(NEXT.fetch_add(1, Ordering::Relaxed))
    }
}

#[derive(Clone, Copy, PartialEq, Eq, Hash, Debug)]
pub struct ColumnId(u64);

impl ColumnId {
    pub(crate) fn fresh() -> ColumnId {
        static NEXT: AtomicU64 = AtomicU64::new(1);
        ColumnId(NEXT.fetch_add(1, Ordering::Relaxed))
    }
}

#[derive(Clone, Copy, Debug)]
pub enum TableKey<'a> {
    Name(&'a str),

    Id(TableId),
}

impl<'a> From<&'a str> for TableKey<'a> {
    fn from(name: &'a str) -> Self {
        TableKey::Name(name)
    }
}

impl<'a> From<&'a String> for TableKey<'a> {
    fn from(name: &'a String) -> Self {
        TableKey::Name(name)
    }
}

impl From<TableId> for TableKey<'_> {
    fn from(id: TableId) -> Self {
        TableKey::Id(id)
    }
}

#[derive(Clone, Debug)]
pub struct Table {
    pub id: TableId,

    pub name: String,

    pub disp: String,

    pub sheet: SheetId,

    pub rect: Rect,

    pub totals: bool,

    pub columns: Vec<String>,

    pub column_ids: Vec<ColumnId>,

    pub calc: Vec<bool>,

    pub calc_key: Vec<(String, String)>,

    pub(crate) calc_snap: Vec<(String, Arc<Formula>)>,

    pub headers: bool,

    pub tot_put: Vec<(String, Option<String>)>,

    pub flags: Vec<u8>,

    pub dxf: Vec<(String, Arc<str>)>,

    pub tot_snap: Option<Vec<(String, Option<String>)>>,

    pub tot_hidden: Option<Vec<(String, Option<String>)>>,

    pub tot_fmts: Option<Vec<(String, Option<Arc<str>>)>>,

    pub show_filter: bool,

    pub sort_state: bool,

    pub list_sel: bool,

    pub ins_rows: Vec<u32>,

    pub emptied: bool,

    pub fmt_snap: Vec<Option<Arc<str>>>,

    pub hdr_fmt: Option<Option<Arc<str>>>,

    pub style: Option<String>,

    pub stripes: [bool; 4],

    pub comment: String,

    pub(crate) sbr_lists: Option<std::collections::BTreeSet<u16>>,
}

pub(crate) const FLAG_BLANK: u8 = 1;

pub(crate) const FLAG_TEXT: u8 = 4;

impl Table {
    pub(crate) fn rename_in_snapshots(&mut self, old: &str, new: &str) {
        for (cname, snap) in self.calc_snap.iter_mut() {
            let root = crate::formula::map_node(&snap.root, &mut |n| match n {
                crate::formula::Node::StructRef {
                    table,
                    implicit,
                    spec,
                } if crate::text::eq_ordinal_ci(table, old) => {
                    Some(crate::formula::Node::StructRef {
                        table: new.into(),
                        implicit: *implicit,
                        spec: spec.clone(),
                    })
                }
                _ => None,
            });
            if let Some(k) = self.calc_key.iter_mut().find(|(n, _)| n == cname) {
                k.1 = format!("{root:?}");
            }
            *snap = Arc::new(Formula {
                root,
                volatile: snap.volatile,
                dynamic: snap.dynamic.clone(),
            });
        }
    }
}

impl Table {
    pub fn calc_snap_columns(&self) -> Vec<&str> {
        self.calc_snap.iter().map(|(n, _)| n.as_str()).collect()
    }

    fn auto_key(name: &str) -> String {
        name.chars().map(crate::text::fold_upper).collect()
    }

    pub(crate) fn add_flag(&mut self, j: usize, bit: u8) {
        if self.flags.len() <= j {
            self.flags.resize(j + 1, 0);
        }
        self.flags[j] |= bit;
    }

    pub(crate) fn drop_blanks_header(&self, j: usize) -> bool {
        let f = self.flags.get(j).copied().unwrap_or(0);
        f & 3 != 0 && f & FLAG_TEXT == 0
    }

    pub(crate) fn col_dxf(&self, name: &str) -> Option<Arc<str>> {
        let k = Self::auto_key(name);
        self.dxf
            .iter()
            .find(|(n, _)| *n == k)
            .map(|(_, f)| f.clone())
    }

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

pub fn table_display_name(name: &str) -> String {
    crate::parser::cch_legal_name(name).unwrap_or_else(|| "_".to_string())
}

impl Workbook {
    pub fn table_by_name(&self, name: &str) -> Option<&Table> {
        self.tables
            .iter()
            .find(|t| crate::text::eq_ordinal_ci(&t.name, name))
            .or_else(|| {
                self.tables
                    .iter()
                    .find(|t| crate::text::eq_ordinal_ci(&t.disp, name))
            })
    }

    pub fn table<'a>(&self, key: impl Into<TableKey<'a>>) -> Option<&Table> {
        self.table_index(key.into()).map(|ti| &self.tables[ti])
    }

    pub(crate) fn table_on(&self, sheet: SheetId, name: &str) -> Option<&Table> {
        self.table_index_on(sheet, name).map(|ti| &self.tables[ti])
    }

    pub(crate) fn table_index_on(&self, sheet: SheetId, name: &str) -> Option<usize> {
        let find = |text: fn(&Table) -> &str| {
            self.tables
                .iter()
                .position(|t| t.sheet == sheet && crate::text::eq_ordinal_ci(text(t), name))
        };
        find(|t| &t.name).or_else(|| find(|t| &t.disp))
    }

    pub(crate) fn table_name_shadowed(&self, ti: usize) -> bool {
        self.table_index(TableKey::Name(&self.tables[ti].name)) != Some(ti)
    }

    pub(crate) fn display_name_taken(&self, text: &str, except: Option<usize>) -> bool {
        let k = crate::text::name_key(text);
        self.names
            .iter()
            .any(|d| crate::text::name_key(&d.name) == k)
            || self
                .tables
                .iter()
                .enumerate()
                .any(|(i, t)| Some(i) != except && crate::text::name_key(&t.disp) == k)
    }

    pub fn display_name_in_use(&self, text: &str) -> bool {
        self.display_name_taken(text, None)
    }

    pub(crate) fn unique_display_name(&self, name: &str, except: Option<usize>) -> String {
        let base = table_display_name(name);
        if !self.display_name_taken(&base, except) {
            return base;
        }
        (1u32..)
            .map(|n| format!("{base}_{n}"))
            .find(|c| !self.display_name_taken(c, except))
            .unwrap_or(base)
    }

    pub(crate) fn renamed_display_name(&self, name: &str, ti: usize) -> String {
        const MAX: usize = 256;
        let is_table = |s: &str| {
            let k = crate::text::name_key(s);
            self.tables
                .iter()
                .any(|t| crate::text::name_key(&t.disp) == k)
        };
        let base =
            crate::parser::cch_legal_name_with(name, &is_table).unwrap_or_else(|| "_".to_string());
        if !self.display_name_taken(&base, Some(ti)) {
            return base;
        }
        let chars: Vec<char> = base.chars().collect();
        let numbered = chars
            .iter()
            .enumerate()
            .skip(1)
            .find(|(_, &c)| c == '_')
            .map(|(i, _)| i)
            .or_else(|| (chars.first() == Some(&'_')).then_some(0))
            .and_then(|i| {
                let tail: String = chars[i + 1..].iter().collect();
                let digits = !tail.is_empty() && tail.bytes().all(|b| b.is_ascii_digit());
                let n = tail.parse::<u32>().ok().filter(|_| digits)?;
                Some((chars[..=i].iter().collect::<String>(), n))
            });
        let (stem, from) = numbered.unwrap_or_else(|| {
            let kept: String = chars.iter().take(MAX - 12).collect();
            (format!("{kept}_"), 0)
        });
        (from.saturating_add(1)..=u32::MAX)
            .map(|n| format!("{stem}{n}"))
            .find(|c| !self.display_name_taken(c, Some(ti)))
            .unwrap_or(base)
    }

    pub fn list_row_append_free<'a>(&self, key: impl Into<TableKey<'a>>) -> bool {
        let Some(ti) = self.table_index(key.into()) else {
            return false;
        };
        let t = &self.tables[ti];
        if t.rect.r1 + 1 >= crate::address::MAX_ROWS {
            return false;
        }
        let strip = Rect {
            r0: t.rect.r1 + 1,
            r1: t.rect.r1 + 1,
            ..t.rect
        };
        let sheet = t.sheet;
        let nonblank = (strip.c0..=strip.c1)
            .any(|c| !matches!(self.value(CellAddr::new(sheet, strip.r0, c)), Value::Blank));
        let cfv = self.cfv_sheet(sheet);
        !(nonblank
            || self
                .tables
                .iter()
                .enumerate()
                .any(|(i, o)| i != ti && o.sheet == sheet && o.rect.intersects(&strip))
            || self.merges(sheet).iter().any(|m| m.intersects(&strip))
            || self.pivot_meeting(sheet, &strip).is_some()
            || cfv.is_some_and(|c| {
                c.dvs
                    .iter()
                    .any(|d| d.applies.iter().any(|r| r.intersects(&strip)))
                    || c.blocks
                        .iter()
                        .any(|b| b.areas.iter().any(|r| r.intersects(&strip)))
            })
            || self.views.get(&sheet).is_some_and(|v| {
                v.filters
                    .iter()
                    .any(|f| f.table.is_none() && f.rect.intersects(&strip))
            })
            || self.spark_groups(sheet).iter().any(|g| g.touches(&strip))
            || self.plain_refs_touch(sheet, strip))
    }

    pub fn table_text_area(
        &self,
        body: &str,
        host: CellAddr,
    ) -> Option<Result<crate::Area, crate::XlError>> {
        let (tname, inner) = match body.find('[') {
            Some(i) if body.ends_with(']') => (&body[..i], Some(&body[i + 1..body.len() - 1])),
            _ => (body, None),
        };

        let t = self
            .tables
            .iter()
            .find(|t| crate::text::eq_ordinal_ci(&t.disp, tname.trim()))?;
        let mut spec = match inner {
            None => Spec {
                items: Items::DATA,
                ..Default::default()
            },
            Some(i) => parse_spec(i).ok()?,
        };
        if let Some((a, b)) = spec.cols.clone() {
            let canon = |c: &str| {
                t.columns
                    .iter()
                    .find(|x| crate::text::eq_ordinal_ci(x, c))
                    .cloned()
            };
            spec.cols = Some((canon(&a)?, canon(&b)?));
        }
        let tn = t.name.clone();
        Some(crate::eval::refs::struct_rect(self, &tn, &spec, host))
    }

    pub fn tables_made(&self) -> usize {
        self.tables_made
    }

    pub fn set_table_look<'a>(
        &mut self,
        key: impl Into<TableKey<'a>>,
        f: impl FnOnce(&mut Table),
    ) -> bool {
        match self.table_index(key.into()) {
            Some(ti) => {
                f(&mut self.tables[ti]);
                true
            }
            None => false,
        }
    }

    pub fn table_filter_on<'a>(&mut self, key: impl Into<TableKey<'a>>) -> bool {
        self.set_table_look(key, |t| t.show_filter = true)
    }

    pub fn table_at(&self, addr: CellAddr) -> Option<&Table> {
        self.tables
            .iter()
            .find(|t| t.contains(addr.sheet, addr.row, addr.col))
    }

    pub(crate) fn header_text(&mut self, addr: CellAddr, v: &Value) -> String {
        if let Value::Number(n) = v {
            if self.cellfmt.fmt(addr).is_some() {
                if let Some(nf) = self.compiled_format(addr) {
                    if let Ok(r) = nf.format_number(*n, &self.locale, self.settings.date_system) {
                        return r.text;
                    }
                }
            }

            return crate::numfmt::format_general_width(*n, crate::numfmt::TEXT_GENERAL_WIDTH);
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
        let r = self.command(Command::TableAdd, |wb| wb.add_table_run(name, sheet, rect));

        let (sr, sc) = self.sheet_sel(sheet);
        if r.is_ok() || sheet != self.session.active {
            self.view_notify_sel(sheet, sr, sc);
        }
        if sheet != self.session.active {
            self.view_leave_tables(sheet);
        }
        r
    }

    pub fn table_create_off_sheet(&mut self, sheet: SheetId) {
        if sheet != self.session.active {
            let (sr, sc) = self.sheet_sel(sheet);
            self.view_notify_sel(sheet, sr, sc);
            self.view_leave_tables(sheet);
        }
    }

    fn add_table_run(
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

        if self.merges(sheet).iter().any(|m| m.intersects(&rect)) {
            self.apply_style_with(sheet, rect, &|s: &crate::style::Style| {
                let mut n = s.clone();
                if n.alignment.horizontal == crate::style::HAlign::CenterAcrossSelection {
                    n.alignment.horizontal = crate::style::HAlign::Center;
                }
                n
            });
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
            .map(|c| {
                let a = CellAddr::new(sheet, rect.r0, c);

                let member = self
                    .sheet(sheet)
                    .is_some_and(|sh| sh.cells.get(a.row, a.col).is_none())
                    && self.spill_anchor(a).is_some();

                let blocked = self.formula_at(a).is_some_and(|f| {
                    self.formula(f)
                        .spill
                        .is_some_and(|r| r.rows() * r.cols() > 1)
                });
                match self.value(a) {
                    Value::Blank => None,
                    _ if member => None,
                    _ if blocked => Some("#SPILL!".to_string()),
                    v => Some(self.header_text(a, &v)),
                }
            })
            .collect();

        let key = |s: &str| s.chars().map(crate::text::fold_upper).collect::<String>();
        let named_keys: std::collections::HashSet<String> =
            named.iter().flatten().map(|s| key(s)).collect();
        let mut seen: std::collections::HashSet<String> = std::collections::HashSet::new();

        let mut counter = 0u32;
        let mut flags: Vec<u8> = Vec::new();
        for (j, given) in named.iter().enumerate() {
            let mut n = match given {
                None => {
                    let cand = loop {
                        counter += 1;
                        let cand = format!("Column{counter}");
                        let k = key(&cand);
                        if !named_keys.contains(&k) && !seen.contains(&k) {
                            break cand;
                        }
                    };
                    flags.push(FLAG_BLANK);
                    cand
                }
                Some(s) => {
                    let text = matches!(
                        self.value(CellAddr::new(sheet, rect.r0, rect.c0 + j as u32)),
                        Value::Text(_)
                    );
                    flags.push(if text { 0 } else { FLAG_TEXT });
                    s.clone()
                }
            };
            if seen.contains(&key(&n)) {
                flags[j] |= FLAG_TEXT;
                loop {
                    counter = (counter + 1).max(2);
                    let cand = format!("{n}{counter}");
                    let k = key(&cand);
                    if !seen.contains(&k) && !named_keys.contains(&k) {
                        n = cand;
                        break;
                    }
                }
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
        self.grow_cell_table_for_new_table(sheet, rect);
        self.tables_made += 1;
        let disp = self.unique_display_name(name, None);
        self.tables.push(Table {
            id: TableId::fresh(),
            name: name.to_string(),
            disp,
            sheet,
            rect,
            totals: false,
            calc: vec![false; columns.len()],
            calc_key: Vec::new(),
            calc_snap: Vec::new(),
            column_ids: columns.iter().map(|_| ColumnId::fresh()).collect(),
            columns,
            headers: true,
            tot_put: Vec::new(),
            flags,
            dxf: Vec::new(),
            tot_snap: None,
            tot_hidden: None,
            tot_fmts: None,
            show_filter: true,
            sort_state: false,
            list_sel: false,
            ins_rows: Vec::new(),
            emptied: false,
            fmt_snap: Vec::new(),
            hdr_fmt: None,
            style: Some("TableStyleMedium2".to_string()),
            stripes: [true, false, false, false],
            comment: String::new(),
            sbr_lists: None,
        });
        let ti = self.tables.len() - 1;
        let (ar, ac) = self.active_cell_of(sheet);
        self.tables[ti].list_sel = sheet == self.session.active && rect.contains(ar, ac);
        self.sbr_table_added(ti);
        self.widen_for_new_table(sheet, rect);
        let d_first = rect.r0 + 1;
        let snap: Vec<Option<Arc<str>>> = (rect.c0..=rect.c1)
            .map(|c| self.counted_row_format(sheet, c, d_first, rect.r1 + 1))
            .collect();
        self.tables[ti].fmt_snap = snap;
        for c in rect.c0..=rect.c1 {
            self.init_col_dxf(ti, c);
        }
        self.infer_calc_columns(ti);
        self.create_filters_on_list(sheet, name, rect);

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

        calc::name_changed(self, &table_dep_key(name));
        calc::name_changed(self, &name.to_uppercase());
        if self.calc.dirty_list.is_empty() {
            self.calc.skip_host_volatile = true;
            let r = self.recalc_point();
            self.calc.skip_host_volatile = false;
            return Ok(r);
        }
        Ok(self.table_changed(name))
    }

    fn grow_cell_table_for_new_table(&mut self, sheet: SheetId, rect: Rect) {
        let n = rect.cols();
        let base = self.cellfmt.base().clone();
        let mut i = 0;
        while i + 1 < n {
            let a = CellAddr::new(sheet, rect.r0, rect.c0 + i);
            let default_xf = !self.has_apostrophe(a) && *self.cell_style(a) == *base;
            let linked = self
                .hyperlinks(sheet)
                .iter()
                .any(|h| h.rect.contains(a.row, a.col));
            if !default_xf && !linked {
                break;
            }
            i += 1;
        }
        if let Some(sh) = self.sheet_mut(sheet) {
            sh.cells.extend_sticky(rect);
            sh.cells.extend_sticky(Rect::new(rect.r0, i, rect.r0, i));
        }
    }

    fn infer_calc_columns(&mut self, ti: usize) {
        let (d0, d1) = self.tables[ti].data_rows();
        let (sheet, c0) = (self.tables[ti].sheet, self.tables[ti].rect.c0);
        let w0 = if d1 >= d0 + 4 { d1 - 4 } else { d0 };
        for idx in 0..self.tables[ti].columns.len() {
            let col = c0 + idx as u32;

            let key = |r: u32| -> Option<(bool, String)> {
                let f = self.formula_at(CellAddr::new(sheet, r, col))?;
                let fc = self.formula(f);
                if fc.array.is_some_and(|(h, w)| h * w > 1) {
                    return None;
                }
                Some((fc.array.is_some(), format!("{:?}", fc.formula.root)))
            };
            let Some(last) = key(d1) else {
                continue;
            };
            let exceptions = (w0..=d1)
                .filter(|&r| key(r).as_ref() != Some(&last))
                .count();
            if exceptions > 1 {
                continue;
            }
            if d1 != d0 {
                let Some(fid) = self.formula_at(CellAddr::new(sheet, d1, col)) else {
                    continue;
                };
                let mut off_sheet = false;
                let mut row_off = |rc: &crate::formula::Rc| {
                    if !rc.abs {
                        let row = d0 as i64 + rc.v as i64;
                        off_sheet |= !(0..crate::address::MAX_ROWS as i64).contains(&row);
                    }
                };
                crate::structure::map_refs(&self.formula(fid).formula.root, &mut |_, c| {
                    use crate::formula::CRef;
                    match c {
                        CRef::Cell { row, .. } => row_off(row),
                        CRef::Area { r0, r1, .. } | CRef::Rows { r0, r1 } => {
                            row_off(r0);
                            row_off(r1);
                        }
                        CRef::Cols { .. } => {}
                    }
                    None
                });
                if off_sheet {
                    continue;
                }
            }
            let name = self.tables[ti].columns[idx].to_lowercase();
            let t = &mut self.tables[ti];
            t.calc[idx] = true;
            t.calc_key.retain(|(n, _)| *n != name);
            t.calc_key.push((name, last.1));
        }
    }

    pub(crate) fn calc_columns_covered(
        &self,
        sheet: SheetId,
        rect: Rect,
    ) -> Vec<(TableId, String)> {
        let mut out = Vec::new();
        for t in self.tables.iter().filter(|t| t.sheet == sheet) {
            let (d0, d1) = t.data_rows();
            if rect.r0 > d0 || rect.r1 < d1 {
                continue;
            }
            for (idx, &is_calc) in t.calc.iter().enumerate() {
                let col = t.rect.c0 + idx as u32;
                if is_calc && (rect.c0..=rect.c1).contains(&col) {
                    out.push((t.id, t.columns[idx].to_lowercase()));
                }
            }
        }
        out
    }

    pub(crate) fn end_calc_columns(&mut self, cols: &[(TableId, String)]) {
        for (id, name) in cols {
            let Some(t) = self.tables.iter_mut().find(|t| t.id == *id) else {
                continue;
            };
            if let Some(idx) = t.columns.iter().position(|c| c.to_lowercase() == *name) {
                t.calc[idx] = false;
            }
            t.calc_key.retain(|(n, _)| n != name);
            t.calc_snap.retain(|(n, _)| n != name);
        }
    }

    pub(crate) fn paste_ends_calc(&mut self, sheet: SheetId, dest: Rect) {
        if self.op_on(super::Op::NoAutoExpand) {
            return;
        }
        for ti in 0..self.tables.len() {
            let t = &self.tables[ti];
            if t.sheet != sheet || !t.calc.iter().any(|&c| c) {
                continue;
            }
            let (d0, d1) = t.data_rows();
            if dest.r0 > d0 || dest.r1 < d1 {
                continue;
            }
            for idx in 0..t.calc.len() {
                let col = self.tables[ti].rect.c0 + idx as u32;
                if !self.tables[ti].calc[idx] || col < dest.c0 || col > dest.c1 {
                    continue;
                }
                let all_const = (d0..=d1).all(|r| {
                    matches!(
                        self.sheet(sheet).and_then(|s| s.cells.get(r, col)),
                        Some(Entry::Const(v)) if !matches!(v, Value::Blank)
                    )
                });
                if all_const {
                    let name = self.tables[ti].columns[idx].to_lowercase();
                    let t = &mut self.tables[ti];
                    t.calc[idx] = false;
                    t.calc_key.retain(|(n, _)| *n != name);
                    t.calc_snap.retain(|(n, _)| *n != name);
                }
            }
        }
    }

    pub(crate) fn paste_copies_table(
        &mut self,
        src_sheet: SheetId,
        src: Rect,
        dst_sheet: SheetId,
        dest: Rect,
    ) {
        let Some(ti) = self.tables.iter().position(|t| {
            t.sheet == src_sheet
                && t.headers
                && src.r0 <= t.rect.r0
                && t.rect.r1 <= src.r1
                && src.c0 <= t.rect.c0
                && t.rect.c1 <= src.c1
        }) else {
            return;
        };
        if (dest.rows(), dest.cols()) != (src.rows(), src.cols()) {
            return;
        }
        let tr = self.tables[ti].rect;
        let (dr, dc) = (
            dest.r0 as i64 - src.r0 as i64,
            dest.c0 as i64 - src.c0 as i64,
        );
        let dest = Rect::new(
            (tr.r0 as i64 + dr) as u32,
            (tr.c0 as i64 + dc) as u32,
            (tr.r1 as i64 + dr) as u32,
            (tr.c1 as i64 + dc) as u32,
        );
        if self
            .tables
            .iter()
            .any(|t| t.sheet == dst_sheet && t.rect.intersects(&dest))
        {
            return;
        }
        let old_name = self.tables[ti].name.clone();
        let name = self.copy_table_name(&old_name);
        let mut copy = self.tables[ti].clone();
        copy.id = TableId::fresh();
        copy.disp = self.unique_display_name(&name, None);
        copy.name = name.clone();
        copy.sheet = dst_sheet;
        copy.rect = dest;
        copy.rename_in_snapshots(&old_name, &name);
        self.tables_made += 1;
        self.tables.push(copy);
        let nti = self.tables.len() - 1;

        let ids: Vec<FormulaId> = self
            .sheet(dst_sheet)
            .map(|sh| {
                sh.cells
                    .iter_rect(dest)
                    .filter_map(|(r, c, _)| self.formula_at(CellAddr::new(dst_sheet, r, c)))
                    .collect()
            })
            .unwrap_or_default();
        for fid in ids {
            let (addr, f) = {
                let x = self.formula(fid);
                (x.addr, x.formula.clone())
            };
            let mut hit = false;
            let root = crate::formula::map_node(&f.root, &mut |n| match n {
                crate::formula::Node::StructRef {
                    table,
                    implicit,
                    spec,
                } if crate::text::eq_ordinal_ci(table, &old_name) => {
                    hit = true;
                    Some(crate::formula::Node::StructRef {
                        table: name.as_str().into(),
                        implicit: *implicit,
                        spec: spec.clone(),
                    })
                }
                _ => None,
            });
            if hit {
                let text = crate::formula_print::print_node(self, &root, addr);
                let nf = Arc::new(Formula {
                    root,
                    volatile: f.volatile,
                    dynamic: f.dynamic.clone(),
                });
                self.set_compiled_formula(addr, nf, text);
            }
        }
        let _ = nti;
        let _ = self.table_changed(&name);
    }

    pub fn restore_table_totals(&mut self, name: &str, rect: crate::address::Rect) {
        self.command(Command::Table, |wb| wb.restore_table_totals_run(name, rect))
    }

    fn restore_table_totals_run(&mut self, name: &str, rect: crate::address::Rect) {
        if let Some(t) = self
            .tables
            .iter_mut()
            .find(|t| crate::text::eq_ordinal_ci(&t.name, name))
        {
            t.rect = rect;
            t.totals = true;
        }
    }

    pub fn set_table_totals<'a>(
        &mut self,
        key: impl Into<TableKey<'a>>,
        show: bool,
    ) -> Result<RecalcReport, EditError> {
        let key = key.into();
        self.command(Command::Table, |wb| wb.set_table_totals_run(key, show))
    }

    fn set_table_totals_run(
        &mut self,
        key: TableKey<'_>,
        show: bool,
    ) -> Result<RecalcReport, EditError> {
        let i = self.table_index(key).ok_or(EditError::NoSuchSheet)?;
        let t = self.tables[i].clone();
        if t.totals == show {
            return Ok(RecalcReport::default());
        }

        if !show
            && self.plain_refs_touch(
                t.sheet,
                Rect::new(t.rect.r1, t.rect.c0, t.rect.r1, t.rect.c1),
            )
        {
            return Ok(RecalcReport::default());
        }

        let row = if show { t.rect.r1 + 1 } else { t.rect.r1 };
        let band = crate::structure::Shift {
            sheet: t.sheet,
            axis: crate::structure::Axis::Rows,
            at: row,
            count: 1,
            insert: show,
            limit: Some((t.rect.c0, t.rect.c1)),
            cells: false,
        };
        self.guard_arrays_shift(&band)?;

        if show
            && self.merges(t.sheet).iter().any(|m| {
                m.r0 <= row
                    && row <= m.r1
                    && (m.c0 < t.rect.c0 || m.c1 > t.rect.c1)
                    && m.c0 <= t.rect.c1
                    && m.c1 >= t.rect.c0
            })
        {
            return Err(EditError::Compile(
                "Cannot complete operation: A table cannot overlap with a PivotTable report, query results, a table, merged cells, or an XML mapping."
                    .into(),
            ));
        }
        let mode = self.settings.mode;
        self.settings.mode = crate::workbook::CalcMode::Manual;

        let shift = |wb: &mut Workbook, at: u32, insert: bool| {
            let s = crate::structure::Shift {
                sheet: t.sheet,
                axis: crate::structure::Axis::Rows,
                at,
                count: 1,
                insert,
                limit: Some((t.rect.c0, t.rect.c1)),
                cells: false,
            };
            if !wb.merge_cut_sideways(&s) {
                let _ = wb.shift(s);
            } else if !insert {
                for c in t.rect.c0..=t.rect.c1 {
                    wb.clear(CellAddr::new(t.sheet, at, c));
                }
            }
        };
        if show {
            let row = t.rect.r1 + 1;
            shift(self, row, true);

            self.note_changed(t.sheet, &[Rect::new(row, t.rect.c0, row, t.rect.c1)]);

            for c in t.rect.c0..=t.rect.c1 {
                self.store_format(CellAddr::new(t.sheet, row, c), "General");
            }
            self.tables[i].rect.r1 = row;
            self.tables[i].totals = true;
            self.extend_row_formats(t.sheet, self.tables[i].rect);
            let hidden = self.tables[i].tot_hidden.take();
            let fmts = self.tables[i].tot_fmts.take();
            let def = |j: usize| -> Option<String> {
                let h = hidden.as_ref()?;
                let k = Table::auto_key(&t.columns[j]);
                h.iter().find(|(n, _)| *n == k).and_then(|(_, x)| x.clone())
            };
            for c in t.rect.c0..=t.rect.c1 {
                let a = CellAddr::new(t.sheet, row, c);

                let sub = match &hidden {
                    Some(_) => {
                        def((c - t.rect.c0) as usize).is_some_and(|x| x.starts_with('\u{1}'))
                    }
                    None => c == t.rect.c1,
                };
                if !sub || self.date_or_text_format(a, true) {
                    self.store_format(a, "General");
                }
                let k = Table::auto_key(&t.columns[(c - t.rect.c0) as usize]);
                if let Some(Some(f)) = fmts
                    .as_ref()
                    .and_then(|v| v.iter().find(|(n, _)| *n == k).map(|(_, f)| f.clone()))
                {
                    self.store_format(a, &f);
                }
            }

            let outer = self.calc.defer_entry;
            self.calc.defer_entry = mode == crate::workbook::CalcMode::Automatic;
            if hidden.is_some() {
                let entered = self.creating_formulas(|wb| {
                    (0..t.columns.len()).try_for_each(|j| {
                        let a = CellAddr::new(t.sheet, row, t.rect.c0 + j as u32);
                        wb.write_totals_cell(a, &t.columns[j], def(j).as_deref())
                    })
                });
                self.calc.defer_entry = outer;
                entered?;
                self.note_totals_entries(t.sheet, row, t.rect);
                self.settings.mode = mode;
                self.redirty_totals_row(i, mode);
                return Ok(self.table_changed(&t.name));
            }
            let entered = self.creating_formulas(|wb| wb.enter_default_totals(i, &t, row));
            self.calc.defer_entry = outer;
            entered?;
            self.note_totals_entries(t.sheet, row, t.rect);
        } else {
            let row = t.rect.r1;

            let snap = match self.tables[i].tot_snap.take() {
                Some(s) => s,
                None => (0..t.columns.len())
                    .map(|j| {
                        let a = CellAddr::new(t.sheet, row, t.rect.c0 + j as u32);
                        let txt = self.formula_text(a).or(match self.value(a) {
                            Value::Blank => None,
                            v => Some(self.header_text(a, &v)),
                        });
                        (Table::auto_key(&t.columns[j]), txt)
                    })
                    .collect(),
            };
            let own = |j: usize, x: &str| -> Option<String> {
                let rest = x.strip_prefix("=SUBTOTAL(")?;
                let (code, tail) = rest.split_once(',')?;
                code.parse::<u32>().ok()?;
                (tail == format!("[{}])", escape_column(&t.columns[j])))
                    .then(|| format!("\u{1}{code}"))
            };
            let snap = snap
                .into_iter()
                .map(|(k, x)| {
                    let j = t.columns.iter().position(|c| Table::auto_key(c) == k);
                    let x = x.map(|x| j.and_then(|j| own(j, &x)).unwrap_or(x));
                    (k, x)
                })
                .collect();
            self.tables[i].tot_hidden = Some(snap);
            let fmts = (0..t.columns.len())
                .map(|j| {
                    let a = CellAddr::new(t.sheet, row, t.rect.c0 + j as u32);
                    let f = self.cellfmt.fmt(a).cloned();
                    (
                        Table::auto_key(&t.columns[j]),
                        f.filter(|f| !f.eq_ignore_ascii_case("general")),
                    )
                })
                .collect();
            self.tables[i].tot_fmts = Some(fmts);
            self.tables[i].rect.r1 = row - 1;
            self.tables[i].totals = false;
            shift(self, row, false);

            self.note_changed(t.sheet, &[Rect::new(row, t.rect.c0, row, t.rect.c1)]);
        }
        self.settings.mode = mode;
        if show {
            self.redirty_totals_row(i, mode);
        }
        Ok(self.table_changed(&t.name))
    }

    fn note_totals_entries(&mut self, sheet: SheetId, row: u32, rect: Rect) {
        for c in rect.c0..=rect.c1 {
            let a = CellAddr::new(sheet, row, c);
            if c == rect.c0
                || self.formula_at(a).is_some()
                || !matches!(self.value(a), Value::Blank)
            {
                self.note_changed(sheet, &[Rect::new(row, c, row, c)]);
            }
        }
    }

    fn enter_default_totals(&mut self, i: usize, t: &Table, row: u32) -> Result<(), EditError> {
        let first = CellAddr::new(t.sheet, row, t.rect.c0);
        let last = CellAddr::new(t.sheet, row, t.rect.c1);
        self.set_value(first, Value::from("Total"));
        let (d0, d1) = t.data_rows();

        let kinds: Vec<Value> = (d0..=d1)
            .map(|r| self.value(CellAddr::new(t.sheet, r, t.rect.c1)))
            .filter(|v| !matches!(v, Value::Blank))
            .collect();

        let mut dated = false;
        for r in d0..=d1 {
            dated |= self.date_or_text_format(CellAddr::new(t.sheet, r, t.rect.c1), false);
        }
        let numeric =
            !dated && !kinds.is_empty() && kinds.iter().all(|v| matches!(v, Value::Number(_)));
        let code = if numeric { 109 } else { 103 };
        let col = escape_column(t.columns.last().unwrap());
        self.set_formula(last, &format!("=SUBTOTAL({code},[{col}])"))?;

        if code == 102 || code == 103 {
            self.store_format(last, "General");
        }

        let put = std::mem::take(&mut self.tables[i].tot_put);
        for (j, name) in t.columns.iter().enumerate() {
            let Some((_, x)) = put.iter().find(|(k, _)| *k == Table::auto_key(name)) else {
                continue;
            };
            let a = CellAddr::new(t.sheet, row, t.rect.c0 + j as u32);
            self.write_totals_cell(a, name, x.as_deref())?;
        }
        Ok(())
    }

    fn table_index(&self, key: TableKey<'_>) -> Option<usize> {
        self.tables.iter().position(|t| match key {
            TableKey::Name(name) => crate::text::eq_ordinal_ci(&t.name, name),
            TableKey::Id(id) => t.id == id,
        })
    }

    fn write_totals_cell(
        &mut self,
        a: CellAddr,
        column: &str,
        entry: Option<&str>,
    ) -> Result<(), EditError> {
        match entry {
            Some(x) if x.starts_with('\u{1}') => {
                let col = escape_column(column);
                self.set_formula(a, &format!("=SUBTOTAL({},[{col}])", &x[1..]))?;
            }
            Some(x) if x.starts_with('=') => {
                let _ = self.set_formula(a, x);
            }
            Some(x) => {
                self.set_value(a, Value::from(x));
            }
            None => {
                self.clear(a);
            }
        }
        Ok(())
    }

    pub fn table_totals_calc<'a>(&self, key: impl Into<TableKey<'a>>, j: usize) -> i32 {
        let Some(ti) = self.table_index(key.into()) else {
            return 0;
        };
        let t = &self.tables[ti];
        let Some(name) = t.columns.get(j) else {
            return 0;
        };
        let entry = if t.totals {
            let a = CellAddr::new(t.sheet, t.rect.r1, t.rect.c0 + j as u32);
            self.formula_text(a)
        } else {
            let k = Table::auto_key(name);
            let hidden = t.tot_hidden.as_ref().unwrap_or(&t.tot_put);
            hidden
                .iter()
                .find(|(n, _)| *n == k)
                .and_then(|(_, x)| x.clone())
        };
        let Some(x) = entry else {
            return 0;
        };
        if let Some(code) = x.strip_prefix('\u{1}') {
            let c = code.parse::<u32>().unwrap_or(0);
            return TOTALS_CODES
                .iter()
                .position(|&x| x == c)
                .map_or(9, |n| n as i32 + 1);
        }
        if !x.starts_with('=') {
            return 0;
        }
        let col = escape_column(name);
        for (n, c) in TOTALS_CODES.iter().enumerate() {
            if x.eq_ignore_ascii_case(&format!("=SUBTOTAL({c},[{col}])")) {
                return n as i32 + 1;
            }
        }
        9
    }

    pub fn set_table_totals_calc<'a>(
        &mut self,
        key: impl Into<TableKey<'a>>,
        j: usize,
        code: i32,
    ) -> Result<(), EditError> {
        let key = key.into();
        self.command(Command::Table, |wb| {
            wb.set_table_totals_calc_run(key, j, code)
        })
    }

    fn set_table_totals_calc_run(
        &mut self,
        key: TableKey<'_>,
        j: usize,
        code: i32,
    ) -> Result<(), EditError> {
        let ti = self.table_index(key).ok_or(EditError::NoSuchSheet)?;
        let t = self.tables[ti].clone();
        self.notes.after_if_calculated = true;
        let Some(name) = t.columns.get(j) else {
            return Ok(());
        };
        let entry = match code {
            0 => None,
            1..=8 => Some(format!("\u{1}{}", TOTALS_CODES[code as usize - 1])),
            _ => return Ok(()),
        };
        if t.totals {
            let a = CellAddr::new(t.sheet, t.rect.r1, t.rect.c0 + j as u32);
            let held = self.formula_text(a).or_else(|| match self.value(a) {
                Value::Blank => None,
                v => Some(self.header_text(a, &v)),
            });

            let same = match &entry {
                Some(x) => {
                    let col = escape_column(name);
                    let f = format!("=SUBTOTAL({},[{col}])", &x[1..]);
                    held.is_some_and(|h| h.eq_ignore_ascii_case(&f))
                }
                None => j > 0 && held.is_none(),
            };
            if !same {
                self.write_totals_cell(a, name, entry.as_deref())?;
                self.note_changed(t.sheet, &[Rect::new(a.row, a.col, a.row, a.col)]);
            }
            return Ok(());
        }
        let k = Table::auto_key(name);
        let tab = &mut self.tables[ti];
        let list = tab.tot_hidden.as_mut().unwrap_or(&mut tab.tot_put);
        list.retain(|(n, _)| *n != k);
        list.push((k, entry));
        Ok(())
    }

    fn redirty_totals_row(&mut self, ti: usize, mode: crate::workbook::CalcMode) {
        if mode == crate::workbook::CalcMode::Manual {
            return;
        }
        let t = &self.tables[ti];
        let (sheet, row, c0, c1) = (t.sheet, t.rect.r1, t.rect.c0, t.rect.c1);
        for c in c0..=c1 {
            if let Some(fid) = self.formula_at(CellAddr::new(sheet, row, c)) {
                self.formula_mut(fid).value = Value::Number(0.0);
                crate::calc::mark_dirty(self, fid);
            }
        }
    }

    pub(crate) fn table_calc_fill(&mut self, addr: CellAddr) -> bool {
        if self.op_on(super::Op::RangeEntry) || !self.settings.auto_fill_lists {
            return false;
        }
        let Some(ti) = self.tables.iter().position(|t| {
            let (d0, d1) = t.data_rows();
            t.sheet == addr.sheet
                && (d0..=d1).contains(&addr.row)
                && (t.rect.c0..=t.rect.c1).contains(&addr.col)
                && !(t.totals && addr.row == t.rect.r1)
        }) else {
            return false;
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
            return false;
        }
        self.calc_fill_column(ti, addr)
    }

    pub(crate) fn calc_fill_column(&mut self, ti: usize, addr: CellAddr) -> bool {
        let (d0, d1) = self.tables[ti].data_rows();
        let idx = (addr.col - self.tables[ti].rect.c0) as usize;

        if let Some(fid) = self.formula_at(addr) {
            if !self.fill_fits(fid, addr.sheet, addr.col, d0..=d1) {
                return false;
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
            keys.push((name.clone(), key));

            let f = self.formula(fid).formula.clone();
            let mut has_ref = false;
            crate::structure::map_refs(&f.root, &mut |_, _| {
                has_ref = true;
                None
            });
            let snaps = &mut self.tables[ti].calc_snap;
            snaps.retain(|(n, _)| *n != name);
            if !has_ref {
                snaps.push((name, f));
            }
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
        let mut skipped = false;
        for r in d0..=d1 {
            if r != addr.row {
                if src_key.is_some()
                    && self
                        .formula_at(CellAddr::new(addr.sheet, r, addr.col))
                        .map(|f| format!("{:?}", self.formula(f).formula.root))
                        == src_key
                {
                    if let Some(fid) = self.formula_at(CellAddr::new(addr.sheet, r, addr.col)) {
                        self.chain_to_top(fid);
                    }
                    skipped = true;
                    continue;
                }

                let to = CellAddr::new(addr.sheet, r, addr.col);
                let cur: Arc<str> = Arc::from(self.number_format(to));
                let own = self.own_format(to);
                let _ = self.copy_range(addr.sheet, src, to);
                filled.push(to);

                if let Some(fid) = self.formula_at(to) {
                    self.formula_mut(fid).value = Value::Number(0.0);
                }
                self.restore_own_format(to, own);
                if cur.eq_ignore_ascii_case("general") {
                    self.store_format(to, &src_fmt);
                } else if src_general && self.should_install_format(&cur, "General") {
                    self.store_format(to, "General");
                }
            } else {
                if let Some(fid) = self.formula_at(addr) {
                    self.chain_to_top(fid);
                }
                filled.push(addr);
            }
        }
        self.settings.mode = mode;
        self.sbr_fill(addr.sheet, addr.col, d0..=d1, !skipped);
        if mode == crate::workbook::CalcMode::Automatic {
            calc::recalc(self);
        } else if filled.len() > 1 {
            if let Some(fid) = self.formula_at(addr) {
                let f = self.formula_mut(fid);
                f.value = Value::Number(0.0);
                f.dirty = true;
            }
            calc::recalc_only(self, &filled);
        }
        true
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
        typed: bool,
    ) {
        let mut filled: Vec<CellAddr> = Vec::new();
        let t = self.tables[ti].clone();

        let mut fill = |wb: &mut Workbook| {
            for (idx, &is_calc) in t.calc.iter().enumerate() {
                if is_calc {
                    wb.table_fill_new_rows_col(&t, idx, rows, sources, typed, &mut filled);
                }
            }
        };
        self.creating_formulas(&mut fill);
        if self.settings.mode != crate::workbook::CalcMode::Automatic && !filled.is_empty() {
            if typed {
                calc::recalc_only(self, &filled);
            } else {
                for &a in &filled {
                    if let Some(fid) = self.formula_at(a) {
                        self.formula_mut(fid).value = Value::Number(0.0);
                    }
                }
            }
        }
    }

    fn table_fill_new_rows_col(
        &mut self,
        t: &Table,
        idx: usize,
        rows: (u32, u32),
        sources: &[(u32, FormulaId, Arc<Formula>)],
        typed: bool,
        filled: &mut Vec<CellAddr>,
    ) {
        let (d0, d1) = t.data_rows();
        let col = t.rect.c0 + idx as u32;
        let sheet = t.sheet;
        let snap = sources.iter().find(|s| s.0 == col).cloned();
        let found = match snap {
            Some((_, fid, arc)) => Some((self.formula(fid).addr.row, Some((fid, arc)))),
            None => {
                let order: Vec<u32> = (d0..rows.0).rev().chain(rows.1 + 1..=d1).collect();
                self.calc_source_row(t, idx, &order).map(|r| (r, None))
            }
        };
        let Some((sr, swap)) = found else {
            if let Some(snap) = self.calc_snapshot(t, idx) {
                for r in rows.0..=rows.1 {
                    let to = CellAddr::new(sheet, r, col);
                    if self
                        .sheet(sheet)
                        .is_some_and(|s| s.cells.get(r, col).is_some())
                    {
                        continue;
                    }
                    let text = crate::formula_print::print_node(self, &snap.root, to);
                    self.set_compiled_formula(to, snap.clone(), text);
                    filled.push(to);
                    if typed {
                        let f = t.col_dxf(&t.columns[idx]);
                        self.store_format(to, f.as_deref().unwrap_or("General"));
                    }
                }
            }
            return;
        };
        let sf = self.formula_at(CellAddr::new(sheet, sr, col)).unwrap();

        for r in rows.0..=rows.1 {
            if self
                .sheet(sheet)
                .is_some_and(|s| s.cells.get(r, col).is_some())
            {
                self.fmt_copy_pairs(&[(
                    CellAddr::new(sheet, sr, col),
                    CellAddr::new(sheet, r, col),
                )]);
            }
        }
        if !self.fill_fits(sf, sheet, col, rows.0..=rows.1) {
            return;
        }

        let swapped = swap.is_some();
        let kept = swap.map(|(fid, arc)| {
            let old = std::mem::replace(&mut self.formula_mut(fid).formula, arc);
            (fid, old)
        });
        let from = Rect::new(sr, col, sr, col);
        let order: Vec<u32> = (d0..rows.0).rev().chain(rows.1 + 1..=d1).collect();
        let def = if swapped {
            None
        } else {
            self.calc_definition_over(t, idx, &order)
        };
        for r in rows.0..=rows.1 {
            if self
                .sheet(sheet)
                .is_some_and(|s| s.cells.get(r, col).is_some())
            {
                continue;
            }
            let _ = self.copy_range(sheet, from, CellAddr::new(sheet, r, col));
            if let Some(def) = &def {
                let to = CellAddr::new(sheet, r, col);
                let text = crate::formula_print::print_node(self, &def.root, to);
                self.set_compiled_formula(to, def.clone(), text);
            }
            filled.push(CellAddr::new(sheet, r, col));
            if typed {
                if let Some(f) = t.col_dxf(&t.columns[idx]) {
                    self.store_format(CellAddr::new(sheet, r, col), &f);
                } else if sr + 1 != r {
                    self.store_format(CellAddr::new(sheet, r, col), "General");
                }
            }
        }
        if let Some((fid, old)) = kept {
            self.formula_mut(fid).formula = old;
        }
    }

    pub(crate) fn calc_snapshot(&self, t: &Table, idx: usize) -> Option<Arc<Formula>> {
        let name = t.columns.get(idx)?.to_lowercase();
        t.calc_snap
            .iter()
            .find(|(n, _)| *n == name)
            .map(|(_, f)| f.clone())
    }

    pub(crate) fn snapshot_calc_formula(&mut self, addr: CellAddr, fid: FormulaId) {
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
        let idx = (addr.col - t.rect.c0) as usize;
        if !t.calc.get(idx).copied().unwrap_or(false) {
            return;
        }
        let (d0, d1) = t.data_rows();
        let others = (d0..=d1).filter(|&r| r != addr.row).any(|r| {
            self.formula_at(CellAddr::new(addr.sheet, r, addr.col))
                .is_some()
        });
        if others {
            return;
        }
        let f = self.formula(fid).formula.clone();
        let mut has_ref = false;
        crate::structure::map_refs(&f.root, &mut |_, _| {
            has_ref = true;
            None
        });
        if has_ref {
            return;
        }
        let name = t.columns[idx].to_lowercase();
        let snaps = &mut self.tables[ti].calc_snap;
        snaps.retain(|(n, _)| *n != name);
        snaps.push((name, f));
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

    pub(crate) fn calc_definition_over(
        &self,
        t: &Table,
        idx: usize,
        order: &[u32],
    ) -> Option<Arc<Formula>> {
        let col = t.rect.c0 + idx as u32;
        let name = t.columns[idx].to_lowercase();
        let key = t
            .calc_key
            .iter()
            .find(|(n, _)| *n == name)
            .map(|(_, k)| k)?;
        let own = order.iter().any(|&r| {
            self.formula_at(CellAddr::new(t.sheet, r, col))
                .is_some_and(|f| format!("{:?}", self.formula(f).formula.root) == *key)
        });
        if own {
            return None;
        }
        self.calc_snapshot(t, idx)
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
        if self.op_on(super::Op::ReplaceEntry) && !self.op_on(super::Op::TextToColumns) {
            return None;
        }
        let ti = self.tables.iter().position(|t| {
            t.sheet == addr.sheet
                && !t.rect.contains(addr.row, addr.col)
                && ((!t.totals
                    && !t.emptied
                    && !is_formula
                    && addr.row == t.rect.r1 + 1
                    && (t.rect.c0..=t.rect.c1).contains(&addr.col))
                    || (addr.col == t.rect.c1 + 1 && (t.rect.r0..=t.rect.r1).contains(&addr.row)))
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

        if let (true, true, Some((s, area, pieces))) = (
            grow_col,
            self.op_on(super::Op::TextToColumns),
            &self.ttc_block,
        ) {
            if *s == addr.sheet {
                let c = rect.c1;
                let outside = pieces
                    .iter()
                    .any(|&(r, pc)| pc == c && !(rect.r0..=rect.r1).contains(&r));
                let others = (rect.r0..=rect.r1).any(|r| {
                    let a = CellAddr::new(addr.sheet, r, c);
                    a != addr && !area.contains(r, c) && !matches!(self.value(a), Value::Blank)
                });
                return (!outside && !others).then_some((ti, rect, grow_col));
            }
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

    fn counted_row_format(&self, sheet: SheetId, c: u32, d0: u32, row: u32) -> Option<Arc<str>> {
        if row <= d0 {
            return None;
        }
        let need = (row - d0).min(3) as usize;
        let mut seen: Vec<(Arc<str>, usize)> = Vec::new();
        for r in row.saturating_sub(5).max(d0)..row {
            if let Some(f) = self.cellfmt.counted(CellAddr::new(sheet, r, c)) {
                if f.eq_ignore_ascii_case("general") {
                    continue;
                }
                match seen.iter_mut().find(|(g, _)| *g == f) {
                    Some(e) => e.1 += 1,
                    None => seen.push((f, 1)),
                }
            }
        }
        seen.into_iter().find(|(_, k)| *k >= need).map(|(f, _)| f)
    }

    fn header_row_format(&self, sheet: SheetId, c0: u32, c1: u32, row: u32) -> Option<Arc<str>> {
        let n = c1 - c0 + 1;
        let need = n.min(3) as usize;
        let mut seen: Vec<(Arc<str>, usize)> = Vec::new();
        for c in (c1 + 1).saturating_sub(5).max(c0)..=c1 {
            if let Some(f) = self.cellfmt.counted(CellAddr::new(sheet, row, c)) {
                if f.eq_ignore_ascii_case("general") {
                    continue;
                }
                match seen.iter_mut().find(|(g, _)| *g == f) {
                    Some(e) => e.1 += 1,
                    None => seen.push((f.clone(), 1)),
                }
            }
        }
        let found = seen.into_iter().find(|(_, k)| *k >= need).map(|(f, _)| f);
        if found.is_some() {
            return found;
        }

        let mut all: Option<Arc<str>> = None;
        for c in (c1 + 1).saturating_sub(5).max(c0)..=c1 {
            let f = self.cellfmt.fmt(CellAddr::new(sheet, row, c))?;
            if f.eq_ignore_ascii_case("general") || all.as_ref().is_some_and(|a| a != f) {
                return None;
            }
            all = Some(f.clone());
        }
        all
    }

    pub(crate) fn freeze_header_formats(&mut self, sheet: SheetId, at: u32, count: u32) {
        if !self.cellfmt.any() {
            return;
        }
        for ti in 0..self.tables.len() {
            let t = &self.tables[ti];
            let r = t.rect;
            if t.sheet != sheet || !t.headers || t.hdr_fmt.is_some() {
                continue;
            }
            if at > r.c1 || at + count <= r.c0 {
                continue;
            }
            let f = self.header_row_format(sheet, r.c0, r.c1, r.r0);
            self.tables[ti].hdr_fmt = Some(f);
        }
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
        let snap = self
            .tables
            .iter()
            .find(|t| t.sheet == sheet && t.rect.r0 == rect.r0 && t.rect.c0 == rect.c0)
            .map(|t| t.fmt_snap.clone())
            .unwrap_or_default();
        for c in rect.c0..=rect.c1 {
            let f = match snap.get((c - rect.c0) as usize) {
                Some(f) => f.clone(),
                None => self.counted_row_format(sheet, c, d0, row),
            };
            if let Some(f) = f {
                self.store_format(CellAddr::new(sheet, row, c), &f);
            }
        }
        if let Some(ti) = self
            .tables
            .iter()
            .position(|t| t.sheet == sheet && t.rect.r0 == rect.r0 && t.rect.c0 == rect.c0)
        {
            if !(self.tables[ti].totals && self.tables[ti].rect.r1 == row) {
                self.apply_col_dxf(ti, row, row);
            }
        }
    }

    pub(crate) fn init_col_dxf(&mut self, ti: usize, c: u32) {
        let t = &self.tables[ti];
        let (d0, d1) = t.data_rows();
        let sheet = t.sheet;
        let Some(name) = t.columns.get((c - t.rect.c0) as usize).cloned() else {
            return;
        };
        let first = self.number_format(CellAddr::new(sheet, d0, c)).to_string();

        if first.eq_ignore_ascii_case("general") {
            return;
        }
        if (d0 + 1..=d1).any(|r| self.number_format(CellAddr::new(sheet, r, c)) != first) {
            return;
        }
        let k = Table::auto_key(&name);
        let t = &mut self.tables[ti];
        t.dxf.retain(|(n, _)| *n != k);
        t.dxf.push((k, Arc::from(first.as_str())));
    }

    pub(crate) fn apply_col_dxf(&mut self, ti: usize, r0: u32, r1: u32) {
        let t = &self.tables[ti];
        if t.dxf.is_empty() {
            return;
        }
        let sheet = t.sheet;
        let set: Vec<(u32, Arc<str>)> = (t.rect.c0..=t.rect.c1)
            .zip(t.columns.iter())
            .filter_map(|(c, n)| t.col_dxf(n).map(|f| (c, f)))
            .collect();
        for (c, f) in set {
            for r in r0..=r1 {
                self.store_format(CellAddr::new(sheet, r, c), &f);
            }
        }
    }

    pub(crate) fn note_pasted_col_formats(&mut self, sheet: SheetId, rect: Rect) {
        let mut set: Vec<(usize, String, Arc<str>)> = Vec::new();
        for (ti, t) in self.tables.iter().enumerate() {
            let (d0, d1) = t.data_rows();
            if t.sheet != sheet
                || rect.r0 > d0
                || rect.r1 < d1
                || rect.c1 < t.rect.c0
                || rect.c0 > t.rect.c1
            {
                continue;
            }
            for c in rect.c0.max(t.rect.c0)..=rect.c1.min(t.rect.c1) {
                let first = self.number_format(CellAddr::new(sheet, d0, c)).to_string();
                if first.eq_ignore_ascii_case("general")
                    || (d0 + 1..=d1)
                        .any(|r| self.number_format(CellAddr::new(sheet, r, c)) != first)
                {
                    continue;
                }
                let k = Table::auto_key(&t.columns[(c - t.rect.c0) as usize]);
                set.push((ti, k, Arc::from(first.as_str())));
            }
        }
        for (ti, k, f) in set {
            let t = &mut self.tables[ti];
            t.dxf.retain(|(n, _)| *n != k);
            t.dxf.push((k, f));
        }
    }

    pub(crate) fn note_table_col_formats(&mut self, sheet: SheetId, rect: Rect, fmt: &str) {
        let fmt: Arc<str> = Arc::from(if fmt.eq_ignore_ascii_case("general") {
            "General"
        } else {
            fmt
        });
        for t in self.tables.iter_mut().filter(|t| t.sheet == sheet) {
            let (d0, d1) = t.data_rows();
            if rect.r0 > d0 || rect.r1 < d1 || rect.c1 < t.rect.c0 || rect.c0 > t.rect.c1 {
                continue;
            }
            for c in rect.c0.max(t.rect.c0)..=rect.c1.min(t.rect.c1) {
                let k = Table::auto_key(&t.columns[(c - t.rect.c0) as usize]);
                t.dxf.retain(|(n, _)| *n != k);
                t.dxf.push((k, fmt.clone()));
            }
        }
    }

    pub(crate) fn insert_row_formats(&mut self, ti: usize, new: (u32, u32)) {
        if !self.cellfmt.any() {
            return;
        }
        let t = &self.tables[ti];
        let (sheet, rect) = (t.sheet, t.rect);
        let (d0, d1) = t.data_rows();
        let old: Vec<u32> = (d0..=d1).filter(|r| *r < new.0 || *r > new.1).collect();
        let n = old.len();
        if n == 0 {
            return;
        }
        let need = n.min(3);

        let first = new.0 == d0;
        for c in rect.c0..=rect.c1 {
            if first {
                break;
            }
            let mut seen: Vec<(Arc<str>, usize)> = Vec::new();

            for &r in &old[n.saturating_sub(5)..] {
                if let Some(f) = self.cellfmt.counted(CellAddr::new(sheet, r, c)) {
                    if f.eq_ignore_ascii_case("general") {
                        continue;
                    }
                    match seen.iter_mut().find(|(g, _)| *g == f) {
                        Some(e) => e.1 += 1,
                        None => seen.push((f, 1)),
                    }
                }
            }
            if let Some((f, _)) = seen.into_iter().find(|(_, k)| *k >= need) {
                for r in new.0..=new.1 {
                    self.store_format(CellAddr::new(sheet, r, c), &f);
                }
            }
        }
        self.apply_col_dxf(ti, new.0, new.1);
    }

    pub(crate) fn extend_col_formats(&mut self, ti: usize) {
        if !self.cellfmt.any() {
            return;
        }
        let t = &self.tables[ti];
        let (sheet, rect) = (t.sheet, t.rect);
        let (d0, d1) = t.data_rows();
        if rect.c1 == rect.c0 {
            return;
        }
        let n = rect.c1 - rect.c0;
        let need = n.min(3) as usize;
        let majority = |wb: &Self, row: u32| {
            let mut seen: Vec<(Arc<str>, usize)> = Vec::new();
            for c in rect.c1.saturating_sub(5).max(rect.c0)..rect.c1 {
                if let Some(f) = wb.cellfmt.fmt(CellAddr::new(sheet, row, c)) {
                    if f.eq_ignore_ascii_case("general") {
                        continue;
                    }
                    match seen.iter_mut().find(|(g, _)| g == f) {
                        Some(e) => e.1 += 1,
                        None => seen.push((f.clone(), 1)),
                    }
                }
            }
            seen.into_iter().find(|(_, k)| *k >= need).map(|(f, _)| f)
        };
        if t.headers {
            let hf = t
                .hdr_fmt
                .clone()
                .unwrap_or_else(|| self.header_row_format(sheet, rect.c0, rect.c1 - 1, rect.r0));
            if let Some(f) = hf {
                self.store_format(CellAddr::new(sheet, rect.r0, rect.c1), &f);
            }
        }
        if d1 <= rect.r0 {
            return;
        }
        if let Some(f) = majority(self, d1) {
            for r in d0..=d1 {
                self.store_format(CellAddr::new(sheet, r, rect.c1), &f);
            }
        }
    }

    pub(crate) fn table_auto_expand(&mut self, addr: CellAddr, is_formula: bool) {
        if self.op_on(super::Op::PivotWrite) {
            return;
        }
        let Some((ti, rect, grow_col)) = self.table_expansion(addr, is_formula) else {
            return;
        };

        if self.op_on(super::Op::NoAutoExpand) && !grow_col {
            return;
        }

        if grow_col
            && self
                .paste_block()
                .is_some_and(|(s, d)| s == addr.sheet && d.r0 < self.tables[ti].rect.r0)
        {
            return;
        }

        if grow_col
            && self.paste_block().is_some_and(|(s, d)| {
                let t = self.tables[ti].rect;
                s == addr.sheet && d.c0 <= t.c1 && d.c1 > t.c1 && d.r0 >= t.r0 && d.r0 <= t.r1
            })
        {
            return;
        }

        if grow_col
            && self
                .paste_block()
                .is_some_and(|(s, d)| s == addr.sheet && d.r1 > self.tables[ti].rect.r1)
        {
            return;
        }
        let name = self.tables[ti].name.clone();
        let old = self.tables[ti].rect;
        self.tables[ti].rect = rect;
        if rect.r1 > old.r1 {
            self.tables[ti].list_sel = true;
        }
        let (refs_grew, firsts) = if grow_col {
            (false, Vec::new())
        } else {
            self.grow_exact_refs_typed(ti, old, rect.r1 - old.r1)
        };
        if grow_col {
            self.extend_col_formats(ti);
        } else {
            self.extend_row_formats(addr.sheet, rect);
            if rect.r1 > old.r1 {
                self.pivots_grow(addr.sheet, old, rect.r1 - old.r1);
            }
        }
        if grow_col && !self.tables[ti].headers {
            let t = &mut self.tables[ti];
            let mut k = 1u64;
            while t
                .columns
                .iter()
                .any(|c| c.eq_ignore_ascii_case(&format!("Column{k}")))
            {
                k += 1;
            }
            let name = format!("Column{k}");
            t.columns.push(name);
            t.column_ids.push(ColumnId::fresh());
            t.calc.push(false);

            t.flags.resize(t.columns.len() - 1, 0);
            t.flags.push(FLAG_BLANK);
        } else if grow_col {
            let hdr = CellAddr::new(addr.sheet, rect.r0, rect.c1);

            let next = match self.value(hdr) {
                Value::Blank => self.tables[ti]
                    .columns
                    .last()
                    .and_then(|prev| next_header_name(prev, &self.tables[ti].columns)),
                _ => None,
            };
            self.tables[ti].columns.push(String::new());
            self.tables[ti].column_ids.push(ColumnId::fresh());
            self.tables[ti].calc.push(false);
            let n = self.tables[ti].columns.len() - 1;
            self.tables[ti].flags.resize(n, 0);
            self.tables[ti].flags.push(0);
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

        calc::name_changed(self, &table_dep_key(&name));
        calc::name_changed(self, &name.to_uppercase());
        if !grow_col {
            let mode = std::mem::replace(&mut self.settings.mode, super::CalcMode::Manual);
            self.table_fill_new_rows(ti, (rect.r1, rect.r1), &firsts, true);
            self.settings.mode = mode;
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

        if self
            .paste_block()
            .is_some_and(|(s, b)| s == addr.sheet && b.contains(addr.row, addr.col))
        {
            return;
        }

        let paste = self.op_on(super::Op::PasteHeader);
        self.validate_headers(ti, Some(addr.col), &|c| c == addr.col && !paste);
    }

    pub(crate) fn paste_headers_pass(&mut self, sheet: SheetId, block: Rect) {
        let mut ran = false;
        for ti in 0..self.tables.len() {
            let t = &self.tables[ti];
            let Some(row) = t.header_row() else { continue };
            if t.sheet == sheet && block.r0 <= row && row <= block.r1 && block.intersects(&t.rect) {
                let cells: Vec<CellAddr> = (t.rect.c0..=t.rect.c1)
                    .map(|c| CellAddr::new(sheet, row, c))
                    .collect();
                let before: Vec<String> = cells
                    .iter()
                    .map(|&a| format!("{:?}", self.value(a)))
                    .collect();
                self.validate_headers(ti, None, &|_| false);
                for (a, b) in cells.into_iter().zip(before) {
                    if format!("{:?}", self.value(a)) != b {
                        calc::cell_changed(self, a);
                        ran = true;
                    }
                }
            }
        }

        if ran
            && self.settings.mode == super::CalcMode::Automatic
            && !self.op_on(super::Op::DeferPasteRecalc)
        {
            calc::recalc(self);
        }
    }

    pub(crate) fn snap_totals_before_sort(&mut self, sheet: SheetId, rect: Rect) {
        for ti in 0..self.tables.len() {
            let t = &self.tables[ti];
            if t.sheet != sheet || !t.totals || t.tot_snap.is_some() {
                continue;
            }
            let r = t.rect.r1;
            if !rect.intersects(&Rect::new(r, t.rect.c0, r, t.rect.c1)) {
                continue;
            }
            let c0 = t.rect.c0;
            let cols = t.columns.clone();
            let snap = cols
                .iter()
                .enumerate()
                .map(|(j, n)| {
                    let a = CellAddr::new(sheet, r, c0 + j as u32);
                    let v = self.value(a);
                    let txt = self.formula_text(a).or(match v {
                        Value::Blank => None,
                        v => Some(self.header_text(a, &v)),
                    });
                    (Table::auto_key(n), txt)
                })
                .collect();
            self.tables[ti].tot_snap = Some(snap);
        }
    }

    pub(crate) fn totals_pending(&self, sheet: SheetId, rect: Rect) -> bool {
        self.tables
            .iter()
            .any(|t| t.sheet == sheet && t.tot_snap.is_some() && t.rect.intersects(&rect))
    }

    pub(crate) fn restore_totals(&mut self, sheet: SheetId, rect: Rect, keep: Option<Rect>) {
        for ti in 0..self.tables.len() {
            let t = &self.tables[ti];
            if t.sheet != sheet || t.tot_snap.is_none() || !t.rect.intersects(&rect) {
                continue;
            }

            self.resync_table_headers(ti);
            let snap = self.tables[ti].tot_snap.take().unwrap();
            let t = self.tables[ti].clone();
            if !t.totals {
                continue;
            }
            for (j, n) in t.columns.iter().enumerate() {
                let Some((_, txt)) = snap.iter().find(|(k, _)| *k == Table::auto_key(n)) else {
                    continue;
                };
                let a = CellAddr::new(sheet, t.rect.r1, t.rect.c0 + j as u32);
                if keep.is_some_and(|k| k.contains(a.row, a.col)) {
                    continue;
                }
                match txt {
                    Some(f) if f.starts_with('=') => {
                        if self.formula_text(a).as_deref() != Some(f.as_str()) {
                            let _ = self.set_formula(a, f);
                        }
                    }
                    Some(x) => {
                        if !matches!(self.value(a), Value::Text(ref s) if **s == **x)
                            || self.formula_at(a).is_some()
                        {
                            self.set_value(a, Value::from(x.as_str()));
                        }
                    }
                    None => {
                        if !matches!(self.value(a), Value::Blank) {
                            self.clear(a);
                            self.store_format(a, "General");
                        }
                    }
                }
            }
        }
    }

    pub(crate) fn resync_table_headers(&mut self, ti: usize) {
        self.validate_headers(ti, None, &|_| false);
    }

    pub(crate) fn resync_headers_after_cut(&mut self, ti: usize, dest: Option<Rect>) {
        let row = self.tables[ti].header_row().unwrap_or(0);
        self.validate_headers(ti, None, &|c| !dest.is_some_and(|d| d.contains(row, c)));
    }

    fn validate_headers(&mut self, ti: usize, edited: Option<u32>, entry: &dyn Fn(u32) -> bool) {
        let totals = self.tables[ti].totals;
        let cont = |c: u32| totals || !entry(c);
        let Some(row) = self.tables[ti].header_row() else {
            return;
        };
        let (sheet, c0) = (self.tables[ti].sheet, self.tables[ti].rect.c0);
        let n = self.tables[ti].columns.len();
        let key = |s: &str| s.chars().map(crate::text::fold_upper).collect::<String>();

        let mut shown: std::collections::HashMap<String, u32> = std::collections::HashMap::new();
        for j in 0..n {
            if let Value::Text(t) = self.value(CellAddr::new(sheet, row, c0 + j as u32)) {
                *shown.entry(key(&t)).or_insert(0) += 1;
            }
        }

        let mut earlier: std::collections::HashSet<String> = std::collections::HashSet::new();
        let mut counter = 1u32;

        let mut renames: Vec<(String, String)> = Vec::new();
        for j in 0..n {
            let col = c0 + j as u32;
            let addr = CellAddr::new(sheet, row, col);
            let v = self.value(addr);
            let old = self.tables[ti].columns[j].clone();
            let drift = match &v {
                Value::Blank => !old.is_empty(),
                Value::Text(t) => **t != *old,
                _ => true,
            };
            let dup = matches!(&v, Value::Text(t) if !t.is_empty() && earlier.contains(&key(t)));
            if !(drift || dup || edited == Some(col)) {
                earlier.insert(key(&old));
                continue;
            }
            let was_blank = matches!(v, Value::Blank);

            if was_blank && !old.is_empty() && self.op_on(super::Op::TextToColumns) {
                self.tables[ti].columns[j] = String::new();
                continue;
            }
            let has = |shown: &std::collections::HashMap<String, u32>, s: &str| {
                shown.get(&key(s)).is_some_and(|&c| c > 0)
            };

            let numbered = |shown: &std::collections::HashMap<String, u32>,
                            counter: &mut u32,
                            base: &str,
                            from: u32| {
                let mut k = (*counter).max(from);
                loop {
                    let cand = format!("{base}{k}");
                    k += 1;
                    if !has(shown, &cand) {
                        *counter = k;
                        return cand;
                    }
                }
            };
            let cut = |s: &str| s.chars().take(252).collect::<String>();
            let blank = was_blank || matches!(&v, Value::Text(t) if t.is_empty());
            let mut notify = false;
            let new = if blank {
                let prev = if j > 0 {
                    self.tables[ti].columns[j - 1].clone()
                } else {
                    String::new()
                };
                let smart = (j > 0 && cont(col))
                    .then(|| crate::autofill::smart_fill_next(&prev))
                    .flatten()
                    .filter(|c| !crate::text::eq_ordinal_ci(c, &prev));
                match smart {
                    Some(c) if !has(&shown, &c) => {
                        notify = true;
                        c
                    }
                    Some(_) => {
                        notify = true;
                        counter = 2;
                        numbered(&shown, &mut counter, &prev, 2)
                    }
                    None => numbered(&shown, &mut counter, "Column", 1),
                }
            } else {
                let cand = match &v {
                    Value::Text(t) => t.to_string(),
                    other => self.header_text(addr, other),
                };
                if (matches!(&v, Value::Text(_)) && !dup) || !has(&shown, &cand) {
                    cand
                } else {
                    notify = !drift && edited != Some(col);
                    numbered(&shown, &mut counter, &cut(&cand), 2)
                }
            };
            if !matches!(self.value(addr), Value::Text(ref s) if **s == *new) {
                if let Value::Text(t) = &v {
                    if let Some(c) = shown.get_mut(&key(t)) {
                        *c = c.saturating_sub(1);
                    }
                }
                if self.formula_at(addr).is_some() {
                    self.remove_cell(addr);
                }
                if let Some(sh) = self.sheet_mut(sheet) {
                    sh.cells
                        .set(row, col, Entry::Const(Value::from(new.as_str())));
                }
                *shown.entry(key(&new)).or_insert(0) += 1;
                if notify {
                    calc::cell_changed(self, addr);
                }
            }
            earlier.insert(key(&new));
            self.tables[ti].columns[j] = new.clone();

            if blank {
                self.tables[ti].add_flag(j, FLAG_BLANK);
            } else if !matches!(&v, Value::Text(t) if **t == *old && !dup) {
                self.tables[ti].add_flag(j, FLAG_TEXT);
            }
            if old == new {
                continue;
            }
            renames.push((old, new));
        }
        self.apply_column_renames(ti, renames);
    }

    fn apply_column_renames(&mut self, ti: usize, mut renames: Vec<(String, String)>) {
        while !renames.is_empty() {
            let i = (0..renames.len())
                .find(|&i| {
                    !renames
                        .iter()
                        .enumerate()
                        .any(|(k, r)| k != i && crate::text::eq_ordinal_ci(&r.0, &renames[i].1))
                })
                .unwrap_or(0);
            let (old, new) = renames.remove(i);
            for (n, _) in self.tables[ti].calc_snap.iter_mut() {
                if *n == old.to_lowercase() {
                    *n = new.to_lowercase();
                }
            }
            self.rename_table_column(ti, &old, &new);
        }
    }

    pub(crate) fn rename_table_column(&mut self, ti: usize, old: &str, new: &str) {
        if self.table_name_shadowed(ti) {
            return;
        }
        let tname = self.tables[ti].name.clone();
        for c in self.slicer_caches.iter_mut().filter(|c| !c.pivot) {
            if crate::text::eq_ordinal_ci(&c.table, &tname)
                && crate::text::eq_ordinal_ci(&c.field, old)
            {
                c.field = new.to_string();
            }
        }
        let (ko, kn) = (Table::auto_key(old), Table::auto_key(new));
        if ko != kn {
            let t = &mut self.tables[ti];
            if let Some(h) = t.tot_fmts.as_mut() {
                if h.iter().any(|(n, _)| *n == ko) {
                    h.retain(|(n, _)| *n != kn);
                    for (n, _) in h.iter_mut() {
                        if *n == ko {
                            *n = kn.clone();
                        }
                    }
                }
            }
            if let Some(h) = t.tot_hidden.as_mut() {
                if h.iter().any(|(n, _)| *n == ko) {
                    h.retain(|(n, _)| *n != kn);
                    for (n, _) in h.iter_mut() {
                        if *n == ko {
                            *n = kn.clone();
                        }
                    }
                }
            }
            if t.tot_put.iter().any(|(n, _)| *n == ko) {
                t.tot_put.retain(|(n, _)| *n != kn);
                for (n, _) in t.tot_put.iter_mut() {
                    if *n == ko {
                        *n = kn.clone();
                    }
                }
            }
            if let Some(snap) = t.tot_snap.as_mut() {
                if snap.iter().any(|(n, _)| *n == ko) {
                    snap.retain(|(n, _)| *n != kn);
                    for (n, _) in snap.iter_mut() {
                        if *n == ko {
                            *n = kn.clone();
                        }
                    }
                }
            }
            if let Some(i) = t.dxf.iter().position(|(n, _)| *n == ko) {
                t.dxf.retain(|(n, _)| *n != kn);
                let i = t.dxf.iter().position(|(n, _)| *n == ko).unwrap_or(i);
                t.dxf[i].0 = kn;
            }
        }
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
                    dynamic: formula.dynamic.clone(),
                });
            }
        }
        for ti2 in 0..self.tables.len() {
            for k in 0..self.tables[ti2].calc_snap.len() {
                let formula = self.tables[ti2].calc_snap[k].1.clone();
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
                    self.tables[ti2].calc_snap[k].1 = Arc::new(Formula {
                        root,
                        volatile: formula.volatile,
                        dynamic: formula.dynamic.clone(),
                    });
                }
            }
        }

        let names: Vec<_> = self.names.iter().cloned().collect();
        for mut def in names {
            let host = def.host();
            let mut changed = false;
            let root = map_struct_refs(&def.formula.root, &mut |table, spec| {
                if !crate::text::eq_ordinal_ci(table, &table_name) {
                    return None;
                }
                let r = rename(spec);
                changed |= r.is_some();
                r
            });
            if changed {
                def.refers_to = crate::formula_print::print_node(self, &root, host);
                def.refresh_info();
                def.formula = Arc::new(Formula {
                    root,
                    volatile: def.formula.volatile,
                    dynamic: def.formula.dynamic.clone(),
                });
                self.names.insert(def);
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
        .any(|c| c.is_ascii() && !(c.is_ascii_alphanumeric() || c == '|'))
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

            let mut depth = 0;
            let bare_punct = rest.chars().any(|c| {
                match c {
                    '[' => depth += 1,
                    ']' => depth -= 1,
                    _ if depth == 0 => {
                        return c.is_ascii_punctuation() && !matches!(c, ':' | '|' | '\'');
                    }
                    _ => {}
                }
                false
            });
            if bare_punct {
                return Err("a column after '@' with punctuation needs brackets".into());
            }
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
            if a == b && !spec.range && a.trim() == a {
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

pub(crate) fn next_header_name(prev: &str, columns: &[String]) -> Option<String> {
    let cand =
        crate::autofill::smart_fill_next(prev).filter(|c| !crate::text::eq_ordinal_ci(c, prev))?;
    let taken = |c: &str| columns.iter().any(|o| crate::text::eq_ordinal_ci(o, c));
    if !taken(&cand) {
        return Some(cand);
    }
    (2..).map(|k| format!("{prev}{k}")).find(|c| !taken(c))
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

    #[test]
    fn totals_calculation_is_the_stored_function() {
        use crate::address::CellAddr;
        let mut wb = crate::workbook::Workbook::new();
        let s = wb.sheet_at(0).id;
        for (c, h) in ["X", "Y"].iter().enumerate() {
            wb.set_input(CellAddr::new(s, 0, c as u32), h).unwrap();
            wb.set_input(CellAddr::new(s, 1, c as u32), "1").unwrap();
        }
        wb.add_table("T", s, Rect::new(0, 0, 1, 1)).unwrap();

        wb.set_table_totals_calc("T", 0, 2).unwrap();
        assert_eq!(wb.table_totals_calc("T", 0), 2);
        assert_eq!(wb.table_totals_calc("T", 1), 0);
        wb.set_table_totals("T", true).unwrap();
        assert_eq!(wb.table_totals_calc("T", 0), 2);
        assert_eq!(wb.table_totals_calc("T", 1), 1);
        wb.set_input(CellAddr::new(s, 2, 0), "=SUBTOTAL(109,[X])+0")
            .unwrap();
        assert_eq!(wb.table_totals_calc("T", 0), 9);
        wb.set_input(CellAddr::new(s, 2, 0), "=SUBTOTAL(104,[X])")
            .unwrap();
        assert_eq!(wb.table_totals_calc("T", 0), 6);
        wb.set_input(CellAddr::new(s, 2, 0), "text").unwrap();
        assert_eq!(wb.table_totals_calc("T", 0), 0);
        wb.set_table_totals_calc("T", 1, 0).unwrap();
        assert!(wb.formula_text(CellAddr::new(s, 2, 1)).is_none());
    }

    #[test]
    fn display_name_avoids_defined_names_and_tables() {
        use crate::address::CellAddr;
        let mut wb = crate::workbook::Workbook::new();
        let s = wb.sheet_at(0).id;
        wb.set_input(CellAddr::new(s, 0, 0), "X").unwrap();
        wb.set_input(CellAddr::new(s, 1, 0), "1").unwrap();
        wb.define_name("nm", "=Sheet1!$H$1", None).unwrap();
        wb.add_table("T", s, Rect::new(0, 0, 1, 0)).unwrap();
        wb.rename_table("T", "nm").unwrap();
        assert_eq!(wb.table_by_name("nm").unwrap().disp, "nm_1");
        assert!(wb.set_table_display_name("nm", "nm").is_err());
        assert!(wb.define_name("NM_1", "=Sheet1!$H$2", None).is_err());
        wb.set_table_display_name("nm", "Sales").unwrap();
        assert_eq!(wb.table_by_name("Sales").unwrap().disp, "Sales");
    }

    #[test]
    fn renamed_display_name_skips_names_read_as_tables() {
        let mut wb = crate::workbook::Workbook::new();
        let s = wb.sheet_at(0).id;
        for (c, name) in ["P", "Q", "R", "S"].iter().enumerate() {
            let c = c as u32 * 2;
            wb.add_table(name, s, Rect::new(0, c, 1, c)).unwrap();
        }
        let mut disp = |old: &str, new: &str| {
            wb.rename_table(old, new).unwrap();
            wb.table_by_name(new).unwrap().disp.clone()
        };
        assert_eq!(disp("P", "ab1"), "ab1_");
        assert_eq!(disp("Q", " "), "_");
        assert_eq!(disp("R", "_"), "__");

        wb.add_sheet("Other").unwrap();
        let o = wb.sheet_at(1).id;
        wb.add_table("T", o, Rect::new(0, 0, 1, 0)).unwrap();
        wb.rename_table("T", "AB1").unwrap();
        assert_eq!(wb.table_on(o, "AB1").unwrap().disp, "_AB1");

        wb.rename_table("S", "Tbl").unwrap();
        wb.add_table("U", o, Rect::new(0, 2, 1, 2)).unwrap();
        wb.rename_table("U", "Tbl").unwrap();
        assert_eq!(wb.table_on(o, "Tbl").unwrap().disp, "Tbl_1");
    }

    #[test]
    fn creation_infers_calc_columns_from_the_last_five_rows() {
        use crate::address::CellAddr;

        let calc = |pat: &str, top: u32, f: &dyn Fn(u32) -> String| -> bool {
            let mut wb = crate::workbook::Workbook::new();
            let s = wb.sheet_at(0).id;
            for c in 0..3u32 {
                let a = CellAddr::new(s, top - 1, c);
                wb.set_input(a, ["X", "Y", "Z"][c as usize]).unwrap();
            }
            for (i, ch) in pat.chars().enumerate() {
                let r = top + 1 + i as u32;
                wb.set_input(CellAddr::new(s, r - 1, 0), &r.to_string())
                    .unwrap();
                wb.set_input(CellAddr::new(s, r - 1, 1), "1").unwrap();
                let input = match ch {
                    'F' => f(r),
                    'G' => format!("=A{r}*B{r}"),
                    'K' => "5".to_string(),
                    _ => continue,
                };
                wb.set_input(CellAddr::new(s, r - 1, 2), &input).unwrap();
            }
            let last = top - 1 + pat.len() as u32;
            wb.add_table("T", s, Rect::new(top - 1, 0, last, 2))
                .unwrap();
            wb.table_by_name("T").unwrap().calc[2]
        };
        let plain = |r: u32| format!("=A{r}+B{r}");
        assert!(calc("FFFFFFFFFF", 1, &plain));

        assert!(calc("KKKKKFFFFF", 1, &plain));
        assert!(calc("KKKKKKFFFF", 1, &plain));
        assert!(!calc("KKKKKKKFFF", 1, &plain));

        assert!(calc("FFFFFFFFKF", 1, &plain));
        assert!(!calc("FFFFFFFKKF", 1, &plain));
        assert!(!calc("FFFFFFFFFK", 1, &plain));
        assert!(!calc("FFFFFFFFFB", 1, &plain));
        assert!(calc("FFFFFFFF_F", 1, &plain));
        assert!(calc("GGGGGGFFFF", 1, &plain));
        assert!(!calc("FFFFFFFFFG", 1, &plain));

        assert!(calc("F", 1, &plain));
        assert!(calc("KF", 1, &plain));
        assert!(!calc("FK", 1, &plain));
        assert!(!calc("KKF", 1, &plain));

        assert!(calc("KKKKKFFFFF", 4, &|r| format!("=A{}+B{r}", r - 4)));
        assert!(!calc("KKKKKFFFFF", 4, &|r| format!("=A{}+B{r}", r - 5)));

        assert!(!calc("KKKKKFFFFF", 4, &|r| format!("=A${}+B{r}", r - 4)));
        assert!(calc("KKKKKFFFFF", 4, &|r| format!("=$A$2+B{r}")));
    }
}
