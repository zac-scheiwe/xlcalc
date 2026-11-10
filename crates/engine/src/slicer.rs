use crate::view::{FilterSpec, VALUES_SEP};
use crate::workbook::Command;
use crate::{CellAddr, EditError, RecalcReport, SheetId, Workbook};

#[derive(Clone, Debug, PartialEq)]
pub struct Slicer {
    pub name: String,
    pub caption: String,
    pub sheet: SheetId,

    pub shape: u32,
    pub columns: i32,
    pub style: String,
    pub header: bool,
    pub locked: bool,
    pub col_w: f64,
    pub row_h: f64,

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

    pub off: Vec<String>,
    pub slicers: Vec<Slicer>,

    pub timeline: Option<Timeline>,

    pub pivots: Vec<String>,
}

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
        Ok(self.slicer_caches.len() - 1)
    }

    fn slicer_name(&mut self, name: &str, add: bool) {
        if add {
            let _ = self.define_name(name, "=#N/A", None);
        } else {
            let _ = self.delete_name(name, None);
        }
    }

    pub fn slicer_items(&self, i: usize) -> Vec<String> {
        let Some(c) = self.slicer_caches.get(i).filter(|c| c.timeline.is_none()) else {
            return Vec::new();
        };
        if c.pivot {
            let Some(cache) = self
                .pivots
                .tables
                .iter()
                .find(|t| same(&t.name, &c.table))
                .and_then(|t| self.pivots.caches.get(t.cache))
            else {
                return Vec::new();
            };
            let Some(f) = cache.fields.iter().find(|f| same(&f.name, &c.field)) else {
                return Vec::new();
            };
            let mut items: Vec<String> = f.items.iter().map(|i| i.label.clone()).collect();
            items.sort_by_key(|s| s.to_lowercase());
            if c.sort == 3 {
                items.reverse();
            }
            return items;
        }
        let Some(t) = self.table_by_name(&c.table) else {
            return Vec::new();
        };
        let Some(col) = t.columns.iter().position(|x| same(x, &c.field)) else {
            return Vec::new();
        };
        let r0 = t.rect.r0 + u32::from(t.headers);
        let r1 = t.rect.r1.saturating_sub(u32::from(t.totals));
        let mut items: Vec<String> = Vec::new();
        if r0 <= r1 {
            for r in r0..=r1 {
                let a = CellAddr::new(t.sheet, r, t.rect.c0 + col as u32);
                let mut s = self.filter_text(a).trim().to_string();
                if s.is_empty() {
                    s = "(blank)".into();
                }
                if !items.iter().any(|x| same(x, &s)) {
                    items.push(s);
                }
            }
        }
        items.sort_by_key(|s| s.to_lowercase());
        if c.sort == 3 {
            items.reverse();
        }
        items
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
            .filter_map(|h| cache.fields[fi].items.get(h).map(|i| i.label.clone()))
            .collect()
    }

    pub fn slicer_apply(&mut self, i: usize) -> Result<RecalcReport, EditError> {
        let Some(c) = self.slicer_caches.get(i).cloned() else {
            return Ok(RecalcReport::default());
        };
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
                .filter(|(_, it)| {
                    match &c.timeline {
                    Some(t) => t.range.is_some_and(|(a, b)| {
                        !matches!(&it.value, crate::Value::Number(n) if *n >= a && *n <= b)
                    }),
                    None => c.off.iter().any(|o| same(o, &it.label)),
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
        let on: Vec<String> = self
            .slicer_items(i)
            .into_iter()
            .filter(|s| !c.off.iter().any(|o| same(o, s)))
            .map(|s| if s == "(blank)" { "=".to_string() } else { s })
            .collect();
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
            }
        };
        let clearing = c.off.is_empty();
        let table = c.table.clone();
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
            off: Vec::new(),
            slicers: Vec::new(),
            timeline: Some(Timeline::default()),
            pivots: vec![table_name],
        });
        let n = self.slicer_caches[self.slicer_caches.len() - 1]
            .name
            .clone();
        self.slicer_name(&n, true);
        Ok(self.slicer_caches.len() - 1)
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
            locked: true,
            col_w: 130.5,
            row_h: 19.833_307_086_614_173,
            tv: TimelineView::default(),
        });
        Ok(c.slicers.len() - 1)
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
        if j < c.slicers.len() {
            let s = c.slicers.remove(j);
            self.delete_shape(s.sheet, s.shape);
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
        let list = self
            .views
            .get(&sheet)
            .and_then(|v| v.filters.iter().find(|f| f.table.as_deref() == Some(&name)))
            .and_then(|f| f.crit.get(&(col as u32)))
            .filter(|cc| cc.op.as_deref() == Some("values"))
            .and_then(|cc| cc.c1.clone());
        let Some(list) = list else {
            return;
        };
        let on: Vec<String> = list
            .split(VALUES_SEP)
            .map(|s| {
                if s == "=" {
                    "(blank)".to_string()
                } else {
                    s.to_string()
                }
            })
            .collect();
        let off: Vec<String> = self
            .slicer_items(i)
            .into_iter()
            .filter(|s| !on.iter().any(|o| same(o, s)))
            .collect();
        self.slicer_caches[i].off = off;
    }

    pub fn table_values(&self, table: &str) -> Vec<(u32, Vec<Option<String>>)> {
        let Some(t) = self.table_by_name(table) else {
            return Vec::new();
        };
        self.views
            .get(&t.sheet)
            .and_then(|v| {
                v.filters
                    .iter()
                    .find(|f| f.table.as_deref() == Some(&t.name))
            })
            .map(|f| {
                f.crit
                    .iter()
                    .filter(|(_, c)| c.op.as_deref() == Some("values"))
                    .map(|(k, c)| {
                        let list = c.c1.as_deref().unwrap_or("");
                        (
                            *k,
                            list.split(VALUES_SEP)
                                .map(|s| (s != "=").then(|| s.to_string()))
                                .collect(),
                        )
                    })
                    .collect()
            })
            .unwrap_or_default()
    }

    pub fn table_values_set(&mut self, table: &str, col: u32, vals: &[Option<String>]) {
        let Some(t) = self.table_by_name(table) else {
            return;
        };
        let (sheet, name, mut rect_src, totals) = (t.sheet, t.name.clone(), t.rect, t.totals);
        let joined = vals
            .iter()
            .map(|v| v.as_deref().unwrap_or("="))
            .collect::<Vec<_>>()
            .join(&VALUES_SEP.to_string());
        if totals {
            rect_src.r1 -= 1;
        }
        let rect = rect_src;
        let crit = crate::view::ColCrit {
            c1: Some(joined),
            op: Some("values".into()),
            c2: None,
            op_code: 7,
        };
        let v = self.view_mut(sheet);
        if let Some(f) = v
            .filters
            .iter_mut()
            .find(|f| f.table.as_deref() == Some(&name))
        {
            f.crit.insert(col, crit);
        } else {
            v.filters.push(crate::view::Filter {
                rect,
                table: Some(name),
                crit: std::iter::once((col, crit)).collect(),
                total_row: false,
            });
        }
    }
}
