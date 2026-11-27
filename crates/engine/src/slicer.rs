use crate::dfrtree::xls_qsort;
use crate::pivot::{CacheField, PivotCache};
use crate::slicer_layout::{header_ascent, SlicerBox};
use crate::text::collate_compare;
use crate::view::filterfile::picked_values;
use crate::view::{FilterSpec, VALUES_SEP};
use crate::workbook::Command;
use crate::{CellAddr, EditError, RecalcReport, Rect, SheetId, Value, Workbook, XlError};
use std::cmp::Ordering;
use std::collections::hash_map::Entry;
use std::collections::HashMap;

#[derive(Clone, Debug, Default, PartialEq)]
pub struct Loaded {
    lists: Vec<Vec<String>>,

    captions: Vec<(usize, String)>,
}

#[derive(Clone, Debug, PartialEq)]
pub struct Slicer {
    pub name: String,
    pub caption: String,
    pub sheet: SheetId,

    pub shape: u32,
    pub columns: i32,
    pub style: String,
    pub header: bool,

    pub position_locked: bool,

    pub row_h: i64,

    pub tv: TimelineView,
}

#[derive(Clone, Debug, PartialEq)]
pub struct TimelineView {
    pub level: i32,
    pub header: bool,
    pub hscroll: bool,
    pub label: bool,
    pub time_level: bool,
}

impl Default for TimelineView {
    fn default() -> Self {
        TimelineView {
            level: 2,
            header: true,
            hscroll: true,
            label: true,
            time_level: true,
        }
    }
}

#[derive(Clone, Debug, Default, PartialEq)]
pub struct Timeline {
    pub range: Option<(f64, f64)>,
}

#[derive(Clone, Debug, PartialEq)]
pub struct SlicerCache {
    pub name: String,

    pub table: String,
    pub pivot: bool,
    pub field: String,

    pub sort: i32,
    pub cross: i32,
    pub manual: bool,
    pub custom_lists: bool,

    pub show_all: bool,

    loaded: Loaded,

    pub off: Vec<String>,
    pub slicers: Vec<Slicer>,

    pub timeline: Option<Timeline>,

    pub pivots: Vec<String>,
}

#[derive(Clone, Debug, PartialEq)]
pub struct SlicerClip {
    index: usize,
    cache: SlicerCache,
    slicer: Slicer,
}

pub const DEFAULT_ROW_HEIGHT: i64 = 251_883;

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum SlicerError {
    Invalid,

    BadIndex,

    AppDefined,

    NotFound,
}

fn same(a: &str, b: &str) -> bool {
    a.to_lowercase() == b.to_lowercase()
}

pub const BUILTIN_LISTS: [&str; 8] = [
    "Mon,Tue,Wed,Thu,Fri,Sat,Sun",
    "Monday,Tuesday,Wednesday,Thursday,Friday,Saturday,Sunday",
    "Jan,Feb,Mar,Apr,May,Jun,Jul,Aug,Sep,Oct,Nov,Dec",
    "January,February,March,April,May,June,July,August,September,October,November,December",
    "Mon,Tue,Wed,Thu,Fri,Sat,Sun",
    "Monday,Tuesday,Wednesday,Thursday,Friday,Saturday,Sunday",
    "Jan,Feb,Mar,Apr,May,Jun,Jul,Aug,Sept,Oct,Nov,Dec",
    "January,February,March,April,May,June,July,August,September,October,November,December",
];

struct Item {
    text: String,
    value: Value,
    data: bool,
}

fn same_entry(a: &str, b: &str) -> bool {
    a.chars().count() == b.chars().count()
        && a.chars()
            .zip(b.chars())
            .all(|(x, y)| x == y || crate::text::equal_ignoring_accents(x, y))
}

fn list_cmp(a: &str, b: &str, lists: &[Vec<&str>]) -> Option<Ordering> {
    let place = |list: &[&str], s: &str| list.iter().rposition(|x| same_entry(x, s));
    let mut listed = false;
    for list in lists.iter().rev() {
        if let Some(x) = place(list, a) {
            listed = true;
            if let Some(y) = place(list, b) {
                return Some(x.cmp(&y));
            }
        }
    }
    if listed {
        return Some(Ordering::Less);
    }
    lists
        .iter()
        .any(|l| place(l, b).is_some())
        .then_some(Ordering::Greater)
}

fn smart_lists(on: bool, user: &[Vec<String>]) -> Vec<Vec<&str>> {
    if !on {
        return Vec::new();
    }
    BUILTIN_LISTS
        .iter()
        .map(|l| l.split(',').collect())
        .chain(user.iter().map(|l| l.iter().map(String::as_str).collect()))
        .collect()
}

fn drop_down_bucket(text: &str) -> usize {
    let upper = |u: u16| {
        let c = char::from_u32(u32::from(u));
        let mut up = c.into_iter().flat_map(char::to_uppercase);
        match (up.next(), up.next()) {
            (Some(x), None) => u16::try_from(u32::from(x)).unwrap_or(u),
            _ => u,
        }
    };
    let units: Vec<u16> = text.encode_utf16().collect();
    match (units.first(), units.last()) {
        (Some(&f), Some(&l)) => (usize::from(upper(f)) + usize::from(upper(l)) + units.len()) % 500,
        _ => 0,
    }
}

fn item_cmp(a: &Item, b: &Item, lists: &[Vec<&str>]) -> Ordering {
    let rank = |v: &Value| match v {
        Value::Text(_) => 1,
        Value::Bool(_) => 2,
        Value::Error(_) => 3,
        _ => 0,
    };
    match (&a.value, &b.value) {
        (Value::Number(x), Value::Number(y)) => x
            .total_cmp(y)
            .then_with(|| collate_compare(&a.text, &b.text)),
        (Value::Bool(x), Value::Bool(y)) => x.cmp(y),
        (Value::Error(x), Value::Error(y)) => x.cmp(y),
        (Value::Text(_), Value::Text(_)) => {
            list_cmp(&a.text, &b.text, lists).unwrap_or_else(|| collate_compare(&a.text, &b.text))
        }

        (Value::Bool(x), Value::Error(XlError::NULL)) => x.cmp(&false),
        (Value::Error(XlError::NULL), Value::Bool(y)) => false.cmp(y),
        (x, y) => rank(x).cmp(&rank(y)),
    }
}

struct PivotItem {
    at: usize,
    index: usize,
    item: crate::pivot::Item,
}

fn pivot_value_cmp(
    a: &crate::pivot::Item,
    b: &crate::pivot::Item,
    lists: &[Vec<&str>],
) -> Ordering {
    match (&a.value, &b.value) {
        (Value::Text(x), Value::Text(y)) => {
            list_cmp(x, y, lists).unwrap_or_else(|| collate_compare(x, y))
        }
        (Value::Error(x), Value::Error(y)) => y.cmp(x),
        _ => crate::pivot::item_order(a, b),
    }
}

pub fn cache_name(field: &str) -> String {
    let f: String = field
        .chars()
        .map(|c| {
            if c.is_alphanumeric() || c == '_' {
                c
            } else {
                '_'
            }
        })
        .collect();
    format!("Slicer_{f}")
}

impl Workbook {
    pub fn slicer_caches(&self) -> &[SlicerCache] {
        &self.slicer_caches
    }

    pub fn slicer_cache_mut(&mut self, i: usize) -> Option<&mut SlicerCache> {
        self.slicer_caches.get_mut(i)
    }

    pub fn slicer_cache_add(
        &mut self,
        table: &str,
        field: &str,
        pivot: bool,
    ) -> Result<usize, SlicerError> {
        let (table, field) = if pivot {
            let p = &self.pivots;
            let t = p
                .tables
                .iter()
                .find(|t| same(&t.name, table))
                .ok_or(SlicerError::Invalid)?;
            let c = p.caches.get(t.cache).ok_or(SlicerError::Invalid)?;
            let f = c
                .fields
                .iter()
                .find(|f| same(&f.name, field))
                .ok_or(SlicerError::Invalid)?;
            (t.name.clone(), f.name.clone())
        } else {
            let t = self.table_by_name(table).ok_or(SlicerError::Invalid)?;
            let f = t
                .columns
                .iter()
                .find(|c| same(c, field))
                .ok_or(SlicerError::Invalid)?;
            (t.name.clone(), f.clone())
        };
        if let Some(i) = self.slicer_caches.iter().position(|c| {
            c.pivot == pivot && c.timeline.is_none() && c.table == table && same(&c.field, &field)
        }) {
            return Ok(i);
        }
        let mut name = cache_name(&field);
        let mut n = 0;
        while self.slicer_caches.iter().any(|c| same(&c.name, &name)) {
            n += 1;
            name = format!("{}{n}", cache_name(&field));
        }
        let table_name = table.clone();
        self.slicer_caches.push(SlicerCache {
            name,
            table,
            pivot,
            field,
            sort: 2,
            cross: 2,
            manual: false,
            custom_lists: true,
            show_all: true,
            loaded: Loaded::default(),
            off: Vec::new(),
            slicers: Vec::new(),
            timeline: None,
            pivots: if pivot {
                vec![table_name.clone()]
            } else {
                Vec::new()
            },
        });
        let n = self.slicer_caches[self.slicer_caches.len() - 1]
            .name
            .clone();
        self.slicer_name(&n, true);
        let at = self.slicer_caches.len() - 1;
        self.slicer_reload(at);
        Ok(at)
    }

    fn slicer_name(&mut self, name: &str, add: bool) {
        if add {
            let _ = self.define_name(name, "=#N/A", None);
        } else {
            let _ = self.delete_name(name, None);
        }
    }

    pub fn slicer_items(&self, i: usize) -> Vec<String> {
        self.slicer_items_data(i)
            .into_iter()
            .map(|(s, _)| s)
            .collect()
    }

    pub fn slicer_items_data(&self, i: usize) -> Vec<(String, bool)> {
        let Some(c) = self.slicer_caches.get(i).filter(|c| c.timeline.is_none()) else {
            return Vec::new();
        };
        if !c.pivot {
            return self.table_slicer_items(c);
        }
        self.pivot_slicer_items(c)
    }

    pub fn pivot_item_name(&self, field: &CacheField, item: &crate::pivot::Item) -> String {
        let n = match &item.value {
            Value::Blank => return "(blank)".into(),
            Value::Text(t) if t.is_empty() => return "(blank)".into(),
            Value::Number(n) => *n,
            _ => return item.label.clone(),
        };
        let code = match &field.format {
            Some(code) => code.clone(),
            None if !item.date => return crate::number::format_general(n),
            None if n.trunc() == 0.0 => "h:mm:ss AM/PM".into(),
            None if n.fract() == 0.0 => self.locale.short_date_format.clone(),
            None => format!("{} h:mm", self.locale.short_date_format),
        };
        crate::NumberFormat::parse(&code)
            .ok()
            .and_then(|f| {
                f.format(&item.value, &self.locale, self.settings.date_system)
                    .ok()
            })
            .map_or_else(|| item.label.clone(), |r| r.text)
    }

    pub fn slicer_reload(&mut self, i: usize) {
        let Some(c) = self.slicer_caches.get(i) else {
            return;
        };
        let loaded = Loaded {
            lists: self.custom_lists.clone(),
            captions: if c.pivot {
                self.slicer_captions(c)
            } else {
                Vec::new()
            },
        };
        self.slicer_caches[i].loaded = loaded;
    }

    pub fn slicer_reload_all(&mut self) {
        for i in 0..self.slicer_caches.len() {
            self.slicer_reload(i);
        }
    }

    pub(crate) fn slicer_reload_pivot(&mut self, name: &str) {
        for i in 0..self.slicer_caches.len() {
            let c = &self.slicer_caches[i];
            if c.pivot && c.pivots.iter().any(|p| same(p, name)) {
                self.slicer_reload(i);
            }
        }
    }

    pub(crate) fn slicer_reload_table(&mut self, name: &str) {
        for i in 0..self.slicer_caches.len() {
            let c = &self.slicer_caches[i];
            if !c.pivot && c.timeline.is_none() && same(&c.table, name) {
                self.slicer_reload(i);
            }
        }
    }

    pub fn slicer_loads(&self) -> Vec<Loaded> {
        self.slicer_caches
            .iter()
            .map(|c| c.loaded.clone())
            .collect()
    }

    pub fn restore_slicer_loads(&mut self, loads: Vec<Loaded>) {
        for (c, l) in self.slicer_caches.iter_mut().zip(loads) {
            c.loaded = l;
        }
    }

    fn slicer_captions(&self, c: &SlicerCache) -> Vec<(usize, String)> {
        let tables = &self.pivots.tables;
        let Some(fi) = tables
            .iter()
            .find(|t| same(&t.name, &c.table))
            .and_then(|t| self.pivots.caches.get(t.cache))
            .and_then(|cache| cache.fields.iter().position(|f| same(&f.name, &c.field)))
        else {
            return Vec::new();
        };
        let mut out: Vec<(usize, String)> = Vec::new();
        for p in &c.pivots {
            let field = tables
                .iter()
                .find(|t| same(&t.name, p))
                .and_then(|t| t.fields.get(fi));
            for (k, caption) in field.into_iter().flat_map(|f| &f.item_caps) {
                if !out.iter().any(|x| x.0 == *k as usize) {
                    out.push((*k as usize, caption.clone()));
                }
            }
        }
        out
    }

    pub fn slicer_item_label(&self, c: &SlicerCache, field: &CacheField, k: usize) -> String {
        let caption = self.slicer_captions(c).into_iter().find(|x| x.0 == k);
        caption.map_or_else(|| self.pivot_item_name(field, &field.items[k]), |x| x.1)
    }

    pub(crate) fn slicer_reload_cells(&mut self, sheet: SheetId, areas: &[Rect]) {
        for i in 0..self.slicer_caches.len() {
            let c = &self.slicer_caches[i];
            if c.pivot || c.timeline.is_some() {
                continue;
            }
            let Some(t) = self.table_by_name(&c.table).filter(|t| t.sheet == sheet) else {
                continue;
            };
            let Some(col) = t.columns.iter().position(|x| same(x, &c.field)) else {
                continue;
            };
            let col = t.rect.c0 + col as u32;
            let r0 = t.rect.r0 + u32::from(t.headers);
            let r1 = t.rect.r1.saturating_sub(u32::from(t.totals));
            let touched = areas
                .iter()
                .any(|a| a.c0 <= col && col <= a.c1 && a.r0 <= r1 && r0 <= a.r1);
            if touched {
                self.slicer_reload(i);
            }
        }
    }

    fn pivot_slicer_items(&self, c: &SlicerCache) -> Vec<(String, bool)> {
        let tables = &self.pivots.tables;
        let Some(cache) = tables
            .iter()
            .find(|t| same(&t.name, &c.table))
            .and_then(|t| self.pivots.caches.get(t.cache))
        else {
            return Vec::new();
        };
        let Some(fi) = cache.fields.iter().position(|f| same(&f.name, &c.field)) else {
            return Vec::new();
        };
        let items = &cache.fields[fi].items;
        let caption = |k: usize| {
            c.loaded
                .captions
                .iter()
                .find(|x| x.0 == k)
                .map(|x| x.1.as_str())
        };

        let mut first = vec![usize::MAX; items.len()];
        for (r, row) in cache.idx.iter().enumerate() {
            if let Some(f) = row.get(fi).and_then(|k| first.get_mut(*k as usize)) {
                *f = (*f).min(r);
            }
        }
        let mut numbered: Vec<usize> = (0..items.len()).collect();
        numbered.sort_by_key(|&k| (first[k], k));
        let keys: Vec<PivotItem> = numbered
            .into_iter()
            .enumerate()
            .map(|(index, at)| PivotItem {
                index,
                at,
                item: match caption(at) {
                    Some(cap) => crate::pivot::Item {
                        value: Value::text(cap),
                        label: cap.to_string(),
                        date: false,
                    },
                    None => items[at].clone(),
                },
            })
            .collect();
        let lists = smart_lists(c.custom_lists, &c.loaded.lists);
        let ghost_last = |k: &PivotItem| !c.show_all && first[k.at] == usize::MAX;
        let mut order: Vec<&PivotItem> = keys.iter().collect();
        xls_qsort(&mut order, |a, b| {
            ghost_last(a).cmp(&ghost_last(b)).then_with(|| {
                let o = pivot_value_cmp(&a.item, &b.item, &lists).then(a.index.cmp(&b.index));
                if c.sort == 3 {
                    o.reverse()
                } else {
                    o
                }
            })
        });
        let data = self.pivot_items_with_data(c, cache, fi);

        if c.cross == 2 || c.cross == 4 {
            order.sort_by_key(|k| !data[k.at]);
        }
        order
            .into_iter()
            .map(|k| {
                let name = self.slicer_item_label(c, &cache.fields[fi], k.at);
                (name, data[k.at])
            })
            .collect()
    }

    fn pivot_items_with_data(&self, c: &SlicerCache, cache: &PivotCache, fi: usize) -> Vec<bool> {
        let mut pass = vec![true; cache.idx.len()];
        for t in c
            .pivots
            .iter()
            .filter_map(|p| self.pivots.tables.iter().find(|t| same(&t.name, p)))
        {
            let grouped = cache.grouped(t);
            let mut hidden = crate::pivot::effective_hidden(t, &grouped).hard;

            for s in &self.slicer_caches {
                if s.pivot && s.pivots.iter().any(|p| same(p, &t.name)) {
                    let g = cache.fields.iter().position(|f| same(&f.name, &s.field));
                    if let Some(g) = g.filter(|&g| g < t.fields.len()) {
                        hidden[g].clone_from(&t.fields[g].hidden);
                    }
                }
            }
            if let Some(own) = hidden.get_mut(fi) {
                own.clear();
            }
            for (r, p) in pass.iter_mut().enumerate() {
                *p &= grouped.idx.get(r).is_none_or(|row| {
                    hidden
                        .iter()
                        .zip(row)
                        .all(|(h, &k)| !h.contains(&(k as usize)))
                });
            }
        }
        let mut data = vec![false; cache.fields[fi].items.len()];
        for (row, _) in cache.idx.iter().zip(&pass).filter(|(_, &p)| p) {
            if let Some(d) = row.get(fi).and_then(|&k| data.get_mut(k as usize)) {
                *d = true;
            }
        }
        data
    }

    fn table_slicer_items(&self, c: &SlicerCache) -> Vec<(String, bool)> {
        let Some(t) = self.table_by_name(&c.table) else {
            return Vec::new();
        };
        let Some(col) = t.columns.iter().position(|x| same(x, &c.field)) else {
            return Vec::new();
        };
        let r0 = t.rect.r0 + u32::from(t.headers);
        let r1 = t.rect.r1.saturating_sub(u32::from(t.totals));
        let shown = self.rows_passing_others(t.sheet, &t.name, col as u32);
        let mut items: Vec<Item> = Vec::new();
        let mut index: HashMap<String, usize> = HashMap::new();
        let mut blank: Option<bool> = None;
        if r0 <= r1 {
            for r in r0..=r1 {
                let a = CellAddr::new(t.sheet, r, t.rect.c0 + col as u32);
                let data = shown.get((r - r0) as usize).copied().unwrap_or(true);
                let text = self.filter_text(a).trim().to_string();
                if text.is_empty() {
                    blank = Some(blank.unwrap_or(false) | data);
                    continue;
                }
                match index.entry(text.to_lowercase()) {
                    Entry::Occupied(e) => items[*e.get()].data |= data,
                    Entry::Vacant(e) => {
                        e.insert(items.len());
                        items.push(Item {
                            value: self.value(a),
                            text,
                            data,
                        });
                    }
                }
            }
        }
        let lists = smart_lists(c.custom_lists, &c.loaded.lists);

        items.sort_by(|a, b| {
            drop_down_bucket(&a.text)
                .cmp(&drop_down_bucket(&b.text))
                .then_with(|| collate_compare(&a.text, &b.text))
        });

        let to_end = c.cross == 2;
        xls_qsort(&mut items, |a, b| {
            match (to_end && a.data != b.data, a.data) {
                (true, true) => Ordering::Less,
                (true, false) => Ordering::Greater,
                _ => {
                    let o = item_cmp(a, b, &lists).then(Ordering::Less);
                    if c.sort == 3 {
                        o.reverse()
                    } else {
                        o
                    }
                }
            }
        });
        let mut out: Vec<(String, bool)> = items.into_iter().map(|i| (i.text, i.data)).collect();
        if let Some(data) = blank {
            let mut at = out.len();
            if c.cross != 1 && data {
                while at > 0 && !out[at - 1].1 {
                    at -= 1;
                }
            }
            out.insert(at, ("(blank)".into(), data));
        }
        out
    }

    fn rows_passing_others(&self, sheet: SheetId, name: &str, col: u32) -> Vec<bool> {
        let Some(f) = self
            .views
            .get(&sheet)
            .and_then(|v| v.filters.iter().find(|f| f.table.as_deref() == Some(name)))
        else {
            return Vec::new();
        };
        let Some(rect) = self.filter_rect(sheet, f) else {
            return Vec::new();
        };
        let mut ok = vec![true; (rect.r1 - rect.r0) as usize];
        for (&field, cc) in f.crit.iter().filter(|(&k, _)| k != col) {
            if let Some(p) = self.field_pass(sheet, rect, field, cc, None) {
                for (o, x) in ok.iter_mut().zip(p) {
                    *o &= x;
                }
            }
        }
        ok
    }

    pub fn set_custom_lists(&mut self, lists: Vec<Vec<String>>) {
        self.custom_lists = lists;
    }

    pub fn slicer_off(&self, i: usize) -> Vec<String> {
        let Some(c) = self.slicer_caches.get(i) else {
            return Vec::new();
        };
        if !c.pivot {
            return c.off.clone();
        }
        let Some(t) = self.pivots.tables.iter().find(|t| same(&t.name, &c.table)) else {
            return Vec::new();
        };
        let Some(cache) = self.pivots.caches.get(t.cache) else {
            return Vec::new();
        };
        let Some(fi) = cache.fields.iter().position(|f| same(&f.name, &c.field)) else {
            return Vec::new();
        };
        let hidden = t
            .fields
            .get(fi)
            .map(|f| f.hidden.clone())
            .unwrap_or_default();
        hidden
            .into_iter()
            .filter_map(|h| {
                let field = &cache.fields[fi];
                (h < field.items.len()).then(|| self.slicer_item_label(c, field, h))
            })
            .collect()
    }

    pub fn slicer_apply(&mut self, i: usize) -> Result<RecalcReport, EditError> {
        let Some(c) = self.slicer_caches.get(i).cloned() else {
            return Ok(RecalcReport::default());
        };
        let report = self.slicer_apply_run(i, &c);

        if let Some(own) = self.slicer_caches.get_mut(i) {
            own.loaded = c.loaded;
        }
        report
    }

    fn slicer_apply_run(&mut self, i: usize, c: &SlicerCache) -> Result<RecalcReport, EditError> {
        if c.pivot {
            let Some(ti0) = self
                .pivots
                .tables
                .iter()
                .position(|t| same(&t.name, &c.table))
            else {
                return Ok(RecalcReport::default());
            };
            let cf = self.pivots.tables[ti0].cache;
            let Some(fi) = self.pivots.caches[cf]
                .fields
                .iter()
                .position(|f| same(&f.name, &c.field))
            else {
                return Ok(RecalcReport::default());
            };
            let hidden: Vec<usize> = self.pivots.caches[cf].fields[fi]
                .items
                .iter()
                .enumerate()
                .filter(|(k, it)| {
                    match &c.timeline {
                    Some(t) => t.range.is_some_and(|(a, b)| {
                        !matches!(&it.value, crate::Value::Number(n) if *n >= a && *n <= b)
                    }),
                    None => {
                        let field = &self.pivots.caches[cf].fields[fi];
                        let name = self.slicer_item_label(c, field, *k);
                        c.off.iter().any(|o| same(o, &name))
                    }
                }
                })
                .map(|(k, _)| k)
                .collect();
            let targets: Vec<usize> = self
                .pivots
                .tables
                .iter()
                .enumerate()
                .filter(|(_, t)| t.cache == cf && c.pivots.iter().any(|p| same(p, &t.name)))
                .map(|(k, _)| k)
                .collect();
            for ti in targets {
                self.pivots.tables[ti].fields[fi].hidden = hidden.clone();
                self.pivot_update(ti)?;
            }
            return Ok(RecalcReport::default());
        }
        let Some(t) = self.table_by_name(&c.table) else {
            return Ok(RecalcReport::default());
        };
        let (sheet, rect) = (t.sheet, t.rect);
        let field = t
            .columns
            .iter()
            .position(|x| same(x, &c.field))
            .map_or(1, |p| p as u32 + 1);
        let mut on: Vec<String> = self
            .slicer_items(i)
            .into_iter()
            .filter(|s| !c.off.iter().any(|o| same(o, s)))
            .map(|s| if s == "(blank)" { "=".to_string() } else { s })
            .collect();

        if on.len() > 2 {
            xls_qsort(&mut on, |a, b| match (a == "=", b == "=") {
                (false, false) => collate_compare(a, b),
                (x, y) => x.cmp(&y),
            });
        }
        let joined = on.join(&VALUES_SEP.to_string());
        let spec = if c.off.is_empty() {
            FilterSpec {
                field,
                ..FilterSpec::default()
            }
        } else {
            FilterSpec {
                field,
                criteria1: Some(&joined),
                operator: Some("values"),
                criteria2: None,
                op_code: 7,
                rich: false,
            }
        };
        let clearing = c.off.is_empty();
        let table = c.table.clone();
        self.slicer_reload_table(&table);
        self.command(Command::View, |wb| {
            let r = wb.autofilter_run(sheet, rect, Some(&table), spec)?;
            if clearing {
                let _ = wb.refilter(sheet, None, None, None);
            }
            Ok(r)
        })
    }

    pub fn slicer_select(
        &mut self,
        i: usize,
        item: &str,
        on: bool,
    ) -> Result<RecalcReport, EditError> {
        let items = self.slicer_items(i);
        let Some(name) = items.iter().find(|s| same(s, item)).cloned() else {
            return Ok(RecalcReport::default());
        };
        let mut off = self.slicer_off(i);
        off.retain(|o| !same(o, &name));
        if !on {
            off.push(name);
        }
        if items.iter().all(|s| off.iter().any(|o| same(o, s))) {
            off.clear();
        }
        let Some(c) = self.slicer_caches.get_mut(i) else {
            return Ok(RecalcReport::default());
        };
        c.off = off;
        self.slicer_apply(i)
    }

    pub fn slicer_clear(&mut self, i: usize) -> Result<RecalcReport, EditError> {
        if let Some(c) = self.slicer_caches.get_mut(i) {
            c.off.clear();
        }
        self.slicer_apply(i)
    }

    pub fn timeline_cache_add(&mut self, pivot: &str, field: &str) -> Result<usize, SlicerError> {
        let t = self
            .pivots
            .tables
            .iter()
            .find(|t| same(&t.name, pivot))
            .ok_or(SlicerError::Invalid)?;
        let c = self
            .pivots
            .caches
            .get(t.cache)
            .ok_or(SlicerError::Invalid)?;
        let f = c
            .fields
            .iter()
            .find(|f| same(&f.name, field))
            .ok_or(SlicerError::NotFound)?;
        let (table, field) = (t.name.clone(), f.name.clone());
        if self
            .slicer_caches
            .iter()
            .any(|c| c.timeline.is_some() && c.table == table && same(&c.field, &field))
        {
            return Err(SlicerError::AppDefined);
        }
        let base = format!(
            "NativeTimeline_{}",
            cache_name(&field).trim_start_matches("Slicer_")
        );
        let table_name = table.clone();
        let mut name = base.clone();
        let mut n = 0;
        while self.slicer_caches.iter().any(|c| same(&c.name, &name)) {
            n += 1;
            name = format!("{base}{n}");
        }
        self.slicer_caches.push(SlicerCache {
            name,
            table,
            pivot: true,
            field,
            sort: 2,
            cross: 2,
            manual: false,
            custom_lists: true,
            show_all: true,
            loaded: Loaded::default(),
            off: Vec::new(),
            slicers: Vec::new(),
            timeline: Some(Timeline::default()),
            pivots: vec![table_name],
        });
        let n = self.slicer_caches[self.slicer_caches.len() - 1]
            .name
            .clone();
        self.slicer_name(&n, true);
        let at = self.slicer_caches.len() - 1;
        self.slicer_reload(at);
        Ok(at)
    }

    pub fn timeline_set(&mut self, i: usize, a: Option<f64>, b: Option<f64>) -> i32 {
        let (Some(a), Some(b)) = (a, b) else {
            return 3;
        };
        if b < a {
            return 1;
        }
        let Some(c) = self.slicer_caches.get_mut(i) else {
            return 3;
        };
        let Some(t) = c.timeline.as_mut() else {
            return 3;
        };
        t.range = Some((a, b));
        let _ = self.slicer_apply(i);
        0
    }

    pub fn slicer_pivot_add(&mut self, i: usize, pivot: &str) -> Result<(), SlicerError> {
        let c = self.slicer_caches.get(i).ok_or(SlicerError::BadIndex)?;
        let main = self
            .pivots
            .tables
            .iter()
            .find(|t| same(&t.name, &c.table))
            .map(|t| t.cache);
        let t = self
            .pivots
            .tables
            .iter()
            .find(|t| same(&t.name, pivot))
            .ok_or(SlicerError::Invalid)?;
        if main.is_some_and(|m| m != t.cache) {
            return Err(SlicerError::Invalid);
        }
        let name = t.name.clone();
        let c = &mut self.slicer_caches[i];
        if !c.pivots.iter().any(|p| same(p, &name)) {
            c.pivots.push(name);
        }
        let _ = self.slicer_apply(i);
        Ok(())
    }

    pub fn slicer_pivot_remove(&mut self, i: usize, pivot: &str) {
        if let Some(c) = self.slicer_caches.get_mut(i) {
            c.pivots.retain(|p| !same(p, pivot));
        }
    }

    pub fn timeline_clear(&mut self, i: usize) {
        if let Some(t) = self
            .slicer_caches
            .get_mut(i)
            .and_then(|c| c.timeline.as_mut())
        {
            t.range = None;
        }
        let _ = self.slicer_apply(i);
    }

    pub fn slicer_add(
        &mut self,
        i: usize,
        sheet: SheetId,
        name: Option<&str>,
        caption: Option<&str>,
        rect_pt: [f64; 4],
    ) -> Result<usize, SlicerError> {
        let field = self
            .slicer_caches
            .get(i)
            .ok_or(SlicerError::BadIndex)?
            .field
            .clone();
        let all_names: Vec<String> = self
            .slicer_caches
            .iter()
            .flat_map(|c| c.slicers.iter().map(|s| s.name.clone()))
            .collect();
        let name = match name {
            Some(n) if all_names.iter().any(|x| same(x, n)) => return Err(SlicerError::Invalid),
            Some(n) => n.to_string(),
            None => {
                let mut n = field.clone();
                let mut k = 0;
                while all_names.iter().any(|x| same(x, &n)) {
                    k += 1;
                    n = format!("{field} {k}");
                }
                n
            }
        };
        let timeline = self.slicer_caches[i].timeline.is_some();
        let (kind, base) = if timeline {
            (crate::drawing::ShapeKind::Timeline, "Timeline")
        } else {
            (crate::drawing::ShapeKind::Slicer, "Slicer")
        };
        let shape = self.add_shape(sheet, kind, base, rect_pt);
        if let Some(s) = self.drawing_mut(sheet).get_mut(shape) {
            s.name = name.clone();
        }
        let c = &mut self.slicer_caches[i];
        c.slicers.push(Slicer {
            name,
            caption: caption.unwrap_or(&field).to_string(),
            sheet,
            shape,
            columns: 1,
            style: if timeline {
                "TimeSlicerStyleLight1"
            } else {
                "SlicerStyleLight1"
            }
            .into(),
            header: true,
            position_locked: false,
            row_h: DEFAULT_ROW_HEIGHT,
            tv: TimelineView::default(),
        });
        Ok(c.slicers.len() - 1)
    }

    pub fn slicer_locked(&self, s: &Slicer) -> bool {
        self.drawing(s.sheet)
            .and_then(|d| d.get(s.shape))
            .is_none_or(|sh| sh.locked)
    }

    pub fn slicer_position_locked(&self, sheet: SheetId, shape: u32) -> bool {
        self.slicer_caches
            .iter()
            .flat_map(|c| &c.slicers)
            .any(|s| s.sheet == sheet && s.shape == shape && s.position_locked)
    }

    pub fn slicer_item_width(&self, i: usize, j: usize) -> Option<i64> {
        let s = self.slicer_caches.get(i)?.slicers.get(j)?;
        let shape = self.drawing(s.sheet)?.get(s.shape)?;
        let layout = SlicerBox {
            rect: self.shape_rect_emu(s.sheet, &shape.pos),
            header: s.header,
            ascent: header_ascent(&self.base_style().font),
            columns: i64::from(s.columns),
            items: self.slicer_items(i).len() as i64,
            row_height: s.row_h,
        };
        Some(layout.item_width())
    }

    pub fn slicer_set_item_width(&mut self, i: usize, j: usize, width: i64) {
        let Some(old) = self.slicer_item_width(i, j) else {
            return;
        };
        if old == width {
            return;
        }
        let s = &self.slicer_caches[i].slicers[j];
        let (sheet, shape, columns) = (s.sheet, s.shape, i64::from(s.columns));
        let Some(sh) = self.drawing(sheet).and_then(|d| d.get(shape)) else {
            return;
        };
        let (x, y, w, h) = self.shape_rect_emu(sheet, &sh.pos);
        let sheet_w = self.col_left_emu(sheet, crate::address::MAX_COLS);
        let sheet_h = self.row_top_emu(sheet, crate::address::MAX_ROWS);
        let w = w.saturating_add((width - old).saturating_mul(columns));
        let (x0, y0) = (x.clamp(0, sheet_w), y.clamp(0, sheet_h));
        let right = x0.saturating_add(w).min(sheet_w).max(x0);
        let bottom = y0.saturating_add(h).min(sheet_h).max(y0);
        self.set_shape_rect(sheet, shape, (x0, y0, right - x0, bottom - y0));
    }

    pub fn slicer_rename(&mut self, i: usize, j: usize, name: &str) -> Result<(), SlicerError> {
        let taken = self.slicer_caches.iter().enumerate().any(|(ci, c)| {
            c.slicers
                .iter()
                .enumerate()
                .any(|(si, s)| (ci, si) != (i, j) && same(&s.name, name))
        });
        if name.is_empty() || taken {
            return Err(SlicerError::Invalid);
        }
        let s = self
            .slicer_caches
            .get_mut(i)
            .and_then(|c| c.slicers.get_mut(j))
            .ok_or(SlicerError::BadIndex)?;
        s.name = name.to_string();
        let (sheet, shape) = (s.sheet, s.shape);
        if let Some(sh) = self.drawing_mut(sheet).get_mut(shape) {
            sh.name = name.to_string();
        }
        Ok(())
    }

    pub fn slicer_delete(&mut self, i: usize, j: usize) {
        let Some(c) = self.slicer_caches.get_mut(i) else {
            return;
        };
        if let Some(s) = c.slicers.get(j) {
            let (sheet, shape) = (s.sheet, s.shape);
            self.delete_shape(sheet, shape);
        }
    }

    pub fn slicer_clip(&self, sheet: SheetId, shape: u32) -> Option<SlicerClip> {
        self.slicer_caches
            .iter()
            .enumerate()
            .find_map(|(index, c)| {
                let slicer = c
                    .slicers
                    .iter()
                    .find(|s| s.sheet == sheet && s.shape == shape)?
                    .clone();
                let mut cache = c.clone();
                cache.slicers.clear();
                Some(SlicerClip {
                    index,
                    cache,
                    slicer,
                })
            })
    }

    pub fn slicer_paste(&mut self, clip: &SlicerClip, sheet: SheetId, shape: u32) {
        let from = &clip.cache;
        let source = if from.pivot {
            self.pivots
                .tables
                .iter()
                .any(|t| same(&t.name, &from.table))
        } else {
            self.table_by_name(&from.table).is_some()
        };
        let found = self
            .slicer_caches
            .iter()
            .position(|c| same(&c.name, &from.name));
        let i = match found {
            Some(i) => {
                let c = &self.slicer_caches[i];
                if c.pivot != from.pivot
                    || !same(&c.table, &from.table)
                    || !same(&c.field, &from.field)
                {
                    return;
                }
                i
            }
            None if source => {
                let i = clip.index.min(self.slicer_caches.len());
                self.slicer_caches.insert(i, from.clone());
                self.slicer_name(&from.name, true);
                i
            }
            None => return,
        };
        let mut s = clip.slicer.clone();
        let mut n = 0;
        while self
            .slicer_caches
            .iter()
            .any(|c| c.slicers.iter().any(|x| same(&x.name, &s.name)))
        {
            n += 1;
            s.name = format!("{} {n}", clip.slicer.name);
        }
        s.sheet = sheet;
        s.shape = shape;
        if let Some(sh) = self.drawing_mut(sheet).get_mut(shape) {
            sh.name.clone_from(&s.name);
        }
        self.slicer_caches[i].slicers.push(s);
    }

    pub(crate) fn slicers_drop_shapes(&mut self, sheet: SheetId, gone: &[u32]) {
        let mut i = 0;
        while let Some(c) = self.slicer_caches.get_mut(i) {
            let before = c.slicers.len();
            c.slicers
                .retain(|s| s.sheet != sheet || !gone.contains(&s.shape));
            if c.timeline.is_none() && before > 0 && c.slicers.is_empty() {
                let c = self.slicer_caches.remove(i);
                self.slicer_name(&c.name, false);
            } else {
                i += 1;
            }
        }
    }

    pub(crate) fn slicers_drop_table(&mut self, name: &str) {
        while let Some(i) = self
            .slicer_caches
            .iter()
            .position(|c| !c.pivot && same(&c.table, name))
        {
            self.slicer_cache_delete(i);
        }
    }

    pub(crate) fn slicers_drop_sheet(&mut self, sheet: SheetId) {
        for c in &mut self.slicer_caches {
            c.slicers.retain(|s| s.sheet != sheet);
        }
        let gone = |wb: &Workbook, c: &SlicerCache| {
            if c.pivot {
                wb.pivots
                    .tables
                    .iter()
                    .find(|t| same(&t.name, &c.table))
                    .is_some_and(|t| t.sheet == sheet)
            } else {
                wb.tables
                    .iter()
                    .find(|t| same(&t.name, &c.table))
                    .is_some_and(|t| t.sheet == sheet)
            }
        };
        while let Some(i) =
            (0..self.slicer_caches.len()).find(|&i| gone(self, &self.slicer_caches[i]))
        {
            self.slicer_cache_delete(i);
        }
    }

    pub fn slicer_cache_delete(&mut self, i: usize) {
        if i < self.slicer_caches.len() {
            let c = self.slicer_caches.remove(i);
            self.slicer_name(&c.name, false);
            for s in c.slicers {
                self.delete_shape(s.sheet, s.shape);
            }
        }
    }

    pub fn slicer_cache_rename(&mut self, i: usize, name: &str) -> bool {
        if i >= self.slicer_caches.len() {
            return false;
        }
        let old = std::mem::replace(&mut self.slicer_caches[i].name, name.to_string());
        self.slicer_name(&old, false);
        self.slicer_name(name, true);
        true
    }

    pub fn slicer_cache_import(
        &mut self,
        name: &str,
        table: &str,
        field: &str,
        pivot: bool,
    ) -> Option<usize> {
        let i = self.slicer_cache_add(table, field, pivot).ok()?;
        let old = std::mem::replace(&mut self.slicer_caches[i].name, name.to_string());
        self.slicer_name(&old, false);
        self.slicer_name(name, true);
        Some(i)
    }

    pub fn slicer_attach(&mut self, i: usize, mut s: Slicer, rect_pt: [f64; 4]) {
        let sheet = s.sheet;
        let found = self
            .drawing(sheet)
            .and_then(|d| {
                d.shapes.iter().find(|x| {
                    matches!(
                        x.kind,
                        crate::drawing::ShapeKind::Slicer | crate::drawing::ShapeKind::Timeline
                    ) && x.name == s.name
                })
            })
            .map(|x| x.id);
        s.shape = match found {
            Some(id) => id,
            None => {
                let kind = if self
                    .slicer_caches
                    .get(i)
                    .is_some_and(|c| c.timeline.is_some())
                {
                    crate::drawing::ShapeKind::Timeline
                } else {
                    crate::drawing::ShapeKind::Slicer
                };
                let id = self.add_shape(sheet, kind, "Slicer", rect_pt);
                if let Some(x) = self.drawing_mut(sheet).get_mut(id) {
                    x.name = s.name.clone();
                }
                id
            }
        };
        if let Some(c) = self.slicer_caches.get_mut(i) {
            c.slicers.push(s);
        }
    }

    pub fn slicer_sync_off(&mut self, i: usize) {
        let Some(c) = self.slicer_caches.get(i) else {
            return;
        };
        if c.pivot {
            return;
        }
        let Some(t) = self.table_by_name(&c.table) else {
            return;
        };
        let Some(col) = t.columns.iter().position(|x| same(x, &c.field)) else {
            return;
        };
        let (sheet, name) = (t.sheet, t.name.clone());
        let picked = self
            .views
            .get(&sheet)
            .and_then(|v| v.filters.iter().find(|f| f.table.as_deref() == Some(&name)))
            .and_then(|f| f.crit.get(&(col as u32)))
            .and_then(picked_values);
        let Some(picked) = picked else {
            return;
        };
        let on: Vec<String> = picked
            .into_iter()
            .map(|s| if s.is_empty() { "(blank)" } else { s }.to_string())
            .collect();
        let off: Vec<String> = self
            .slicer_items(i)
            .into_iter()
            .filter(|s| !on.iter().any(|o| same(o, s)))
            .collect();
        self.slicer_caches[i].off = off;
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn drop_down_bucket_takes_the_ends_and_the_length() {
        assert_eq!(drop_down_bucket(""), 0);
        assert_eq!(drop_down_bucket("a"), 65 + 65 + 1);
        assert_eq!(drop_down_bucket("Mar"), drop_down_bucket("mXr"));
    }

    #[test]
    fn list_cmp_asks_the_newest_common_list() {
        let user = vec![vec!["zeta".to_string(), "alpha".to_string()]];
        let lists = smart_lists(true, &user);
        assert_eq!(list_cmp("ZETA", "\u{e1}lpha", &lists), Some(Ordering::Less));
        assert_eq!(list_cmp("beta", "alpha", &lists), Some(Ordering::Greater));
        assert_eq!(list_cmp("beta", "gamma", &lists), None);

        assert_eq!(list_cmp("Monday", "Jan", &lists), Some(Ordering::Less));
        assert_eq!(list_cmp("Jan", "Monday", &lists), Some(Ordering::Less));
        assert!(smart_lists(false, &user).is_empty());
    }

    #[test]
    fn pivot_values_order_errors_from_the_highest_code() {
        let item = |value| crate::pivot::Item {
            value,
            label: String::new(),
            date: false,
        };
        let na = item(Value::Error(XlError::NA));
        let null = item(Value::Error(XlError::NULL));
        assert_eq!(pivot_value_cmp(&na, &null, &[]), Ordering::Less);
        assert_eq!(
            pivot_value_cmp(&item(Value::Bool(true)), &na, &[]),
            Ordering::Less
        );
    }
}
