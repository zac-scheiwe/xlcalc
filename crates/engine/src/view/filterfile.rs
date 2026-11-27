use super::{date_filter_bounds, top_threshold, ColCrit, Filter, VALUES_NONE, VALUES_SEP};
use crate::address::{CellAddr, Rect, SheetId};
use crate::functions::criteria::crit::Pattern;
use crate::value::Value;
use crate::workbook::Workbook;

pub type Attrs = Vec<(String, String)>;

#[derive(Clone, Debug, PartialEq)]
pub enum FileCrit {
    Values {
        blank: bool,
        calendar: Option<String>,
        items: Vec<String>,
        dates: Vec<Attrs>,
        rich: bool,
    },

    Custom {
        and: bool,
        items: Vec<(String, String)>,
    },
    Top10 {
        top: bool,
        percent: bool,
        val: String,
        filter_val: Option<f64>,
    },

    Dynamic {
        kind: String,
        val: Option<f64>,
        max: Option<f64>,
    },

    Color {
        font: bool,
        rgb: Option<u32>,
        dxf: Option<String>,
    },
    Icon {
        set: String,
        id: Option<u32>,
    },
}

#[derive(Clone, Debug, PartialEq)]
pub struct FileColumn {
    pub field: u32,
    pub hidden_button: bool,
    pub show_button: bool,
    pub crit: Option<FileCrit>,
}

#[derive(Clone, Debug, PartialEq)]
pub struct FileSort {
    pub attrs: Attrs,
    pub conditions: Vec<Attrs>,
}

#[derive(Clone, Debug, PartialEq)]
pub struct RangeFilterFile {
    pub rect: Rect,
    pub columns: Vec<FileColumn>,
    pub sort: Option<FileSort>,
}

#[derive(Clone, Debug, Default)]
pub struct FilterExtra {
    columns: Vec<ColExtra>,

    buttons: Vec<(u32, bool, bool)>,

    sort: Option<(Rect, FileSort)>,
}

impl FilterExtra {
    pub(super) fn value(&self, cc: &ColCrit) -> Option<f64> {
        let key = key(cc);
        let x = self.columns.iter().find(|x| x.key == key)?;
        x.value.map(|v| v.0)
    }
}

#[derive(Clone, Debug)]
struct ColExtra {
    key: Key,

    value: Option<(f64, Option<f64>)>,
    loaded: Option<FileCrit>,
}

type Key = (Option<String>, Option<String>, Option<String>);

fn key(cc: &ColCrit) -> Key {
    (cc.c1.clone(), cc.op.clone(), cc.c2.clone())
}

const DYNAMIC: [&str; 34] = [
    "today",
    "yesterday",
    "tomorrow",
    "thisWeek",
    "lastWeek",
    "nextWeek",
    "thisMonth",
    "lastMonth",
    "nextMonth",
    "thisQuarter",
    "lastQuarter",
    "nextQuarter",
    "thisYear",
    "lastYear",
    "nextYear",
    "yearToDate",
    "Q1",
    "Q2",
    "Q3",
    "Q4",
    "M1",
    "M2",
    "M3",
    "M4",
    "M5",
    "M6",
    "M7",
    "M8",
    "M9",
    "M10",
    "M11",
    "M12",
    "aboveAverage",
    "belowAverage",
];

const SIGNS: [(&str, &str); 6] = [
    ("lessThanOrEqual", "<="),
    ("greaterThanOrEqual", ">="),
    ("notEqual", "<>"),
    ("lessThan", "<"),
    ("greaterThan", ">"),
    ("equal", "="),
];

fn swap_red_blue(c: u32) -> u32 {
    (c & 0xff) << 16 | (c & 0xff00) | (c >> 16 & 0xff)
}

fn split(s: &str) -> (&str, &str) {
    SIGNS
        .iter()
        .find_map(|&(_, sign)| s.strip_prefix(sign).map(|r| (sign, r)))
        .unwrap_or(("=", s))
}

fn file_crit(cc: &ColCrit, value: Option<(f64, Option<f64>)>) -> Option<FileCrit> {
    let c1 = cc.c1.as_deref().unwrap_or("");
    let values = |items: &[&str]| FileCrit::Values {
        blank: items.iter().any(|i| split(i).1.is_empty()),
        calendar: None,
        items: items
            .iter()
            .map(|i| split(i).1.to_string())
            .filter(|i| !i.is_empty())
            .collect(),
        dates: Vec::new(),
        rich: false,
    };
    let custom = |and: bool, items: &[&str]| FileCrit::Custom {
        and,
        items: items
            .iter()
            .map(|i| {
                let (sign, rest) = split(i);
                let name = SIGNS
                    .iter()
                    .find(|&&(n, s)| s == sign && n != "equal")
                    .map_or("", |&(n, _)| n);

                let rest = if sign == "<>" && rest.is_empty() {
                    " "
                } else {
                    rest
                };
                (name.to_string(), rest.to_string())
            })
            .collect(),
    };
    Some(match cc.op.as_deref() {
        Some(op @ ("top10" | "bottom10" | "top10pct" | "bottom10pct")) => FileCrit::Top10 {
            top: op.starts_with("top"),
            percent: op.ends_with("pct"),
            val: c1.trim().to_string(),
            filter_val: value.map(|v| v.0),
        },
        Some("dynamic") => {
            let code: usize = c1.trim().parse().ok()?;
            FileCrit::Dynamic {
                kind: DYNAMIC.get(code.checked_sub(1)?)?.to_string(),
                val: value.map(|v| v.0),
                max: value.and_then(|v| v.1),
            }
        }
        Some(op @ ("cellcolor" | "fontcolor" | "nofill" | "autofont")) => FileCrit::Color {
            font: matches!(op, "fontcolor" | "autofont"),
            rgb: matches!(op, "cellcolor" | "fontcolor")
                .then(|| c1.trim().parse::<i64>().ok())
                .flatten()
                .map(|bgr| swap_red_blue(bgr as u32)),
            dxf: None,
        },

        Some("icon" | "dategroups") => return None,
        Some("values") if c1 == VALUES_NONE => values(&[]),
        Some("values") => {
            let items: Vec<&str> = c1.split(VALUES_SEP).collect();
            match (cc.c2.as_deref(), items.as_slice()) {
                (Some(c2), [one]) => custom(true, &[one, c2]),
                _ => values(&items),
            }
        }
        op => {
            let items: Vec<&str> = std::iter::once(c1).chain(cc.c2.as_deref()).collect();
            let and = items.len() == 2 && op != Some("or");
            let plain = |i: &&str| {
                let (sign, rest) = split(i);
                sign == "=" && !Pattern::has_wildcards(rest)
            };
            if !and && items.iter().all(plain) {
                values(&items)
            } else {
                custom(and, &items)
            }
        }
    })
}

fn col_crit(f: &FileCrit) -> Option<ColCrit> {
    let mut cc = ColCrit::default();
    let set = |cc: &mut ColCrit, op: &str, c1: String, code: i32| {
        cc.op = Some(op.to_string());
        cc.c1 = Some(c1);
        cc.op_code = code;
    };
    match f {
        FileCrit::Values {
            blank,
            items,
            dates,
            ..
        } => {
            if !dates.is_empty() {
                set(&mut cc, "dategroups", format!("{items:?}{dates:?}"), 0);
                return Some(cc);
            }
            let mut all: Vec<String> = items.iter().map(|i| format!("={i}")).collect();

            if *blank {
                let at = if all.len() < 2 { 0 } else { all.len() };
                all.insert(at, "=".to_string());
            }
            match all.len() {
                0 => set(&mut cc, "values", VALUES_NONE.to_string(), 7),
                1 => cc.c1 = all.pop(),
                2 => {
                    cc.c2 = all.pop();
                    set(&mut cc, "or", all.pop()?, 2);
                }
                _ => set(&mut cc, "values", all.join(&VALUES_SEP.to_string()), 7),
            }
        }
        FileCrit::Custom { and, items } => {
            let mut all = items.iter().map(|(name, val)| {
                let sign = SIGNS
                    .iter()
                    .find(|(n, _)| n == name)
                    .map_or("=", |&(_, s)| s);
                let val = if sign == "<>" && val == " " { "" } else { val };
                format!("{sign}{val}")
            });
            cc.c1 = Some(all.next()?);
            cc.c2 = all.next();
            if cc.c2.is_some() {
                cc.op = Some(if *and { "and" } else { "or" }.to_string());
                cc.op_code = if *and { 1 } else { 2 };
            }
        }
        FileCrit::Top10 {
            top, percent, val, ..
        } => {
            let (op, code) = match (top, percent) {
                (true, false) => ("top10", 3),
                (false, false) => ("bottom10", 4),
                (true, true) => ("top10pct", 5),
                (false, true) => ("bottom10pct", 6),
            };
            set(&mut cc, op, val.clone(), code);
        }
        FileCrit::Dynamic { kind, .. } => {
            let code = DYNAMIC.iter().position(|k| k == kind)? + 1;
            set(&mut cc, "dynamic", code.to_string(), 11);
        }
        FileCrit::Color { font, rgb, .. } => {
            let (op, code) = match (font, rgb) {
                (false, Some(_)) => ("cellcolor", 8),
                (true, Some(_)) => ("fontcolor", 9),
                (false, None) => ("nofill", 12),
                (true, None) => ("autofont", 13),
            };
            set(
                &mut cc,
                op,
                swap_red_blue(rgb.unwrap_or(0)).to_string(),
                code,
            );
        }
        FileCrit::Icon { set: name, id } => set(&mut cc, "icon", format!("{name} {id:?}"), 10),
    }
    Some(cc)
}

fn file_value(f: &FileCrit) -> Option<(f64, Option<f64>)> {
    match f {
        FileCrit::Top10 { filter_val, .. } => filter_val.map(|v| (v, None)),
        FileCrit::Dynamic { val, max, .. } => val.map(|v| (v, *max)),
        _ => None,
    }
}

fn file_columns(f: &Filter, rect: Rect) -> Vec<FileColumn> {
    (0..rect.cols())
        .filter_map(|field| {
            let button = f.extra.buttons.iter().find(|b| b.0 == field);
            let crit = f.crit.get(&field).and_then(|cc| {
                let key = key(cc);
                let extra = f.extra.columns.iter().find(|x| x.key == key);
                match extra.and_then(|x| x.loaded.as_ref()) {
                    Some(l) if !matches!(l, FileCrit::Top10 { .. } | FileCrit::Dynamic { .. }) => {
                        Some(l.clone())
                    }
                    _ => file_crit(cc, extra.and_then(|x| x.value)),
                }
            });
            (button.is_some() || crit.is_some()).then(|| FileColumn {
                field,
                hidden_button: button.is_some_and(|b| b.1),
                show_button: button.is_none_or(|b| b.2),
                crit,
            })
        })
        .collect()
}

fn install(f: &mut Filter, cols: u32, file: &RangeFilterFile) {
    for c in file.columns.iter().filter(|c| c.field < cols) {
        if c.hidden_button || !c.show_button || c.crit.is_none() {
            f.extra
                .buttons
                .push((c.field, c.hidden_button, c.show_button));
        }
        let Some((l, cc)) = c.crit.as_ref().and_then(|l| Some((l, col_crit(l)?))) else {
            continue;
        };
        f.extra.columns.push(ColExtra {
            key: key(&cc),
            value: file_value(l),
            loaded: Some(l.clone()),
        });
        f.crit.insert(c.field, cc);
    }
}

pub(crate) fn picked_values(cc: &ColCrit) -> Option<Vec<&str>> {
    let c1 = cc.c1.as_deref()?;
    match cc.op.as_deref() {
        Some("values") if c1 == VALUES_NONE => Some(Vec::new()),
        Some("values") => Some(
            c1.split(VALUES_SEP)
                .map(|s| s.strip_prefix('=').unwrap_or(s))
                .collect(),
        ),
        None => c1.strip_prefix('=').map(|v| vec![v]),
        Some("or") => [Some(c1), cc.c2.as_deref()]
            .into_iter()
            .flatten()
            .map(|c| c.strip_prefix('='))
            .collect(),
        _ => None,
    }
}

impl Workbook {
    pub fn range_filter_file(&self, sheet: SheetId) -> Option<RangeFilterFile> {
        let v = self.views.get(&sheet)?;
        let f = v.filters.iter().find(|f| f.table.is_none())?;
        let rect = self.filter_rect(sheet, f)?;
        let sort = f
            .extra
            .sort
            .as_ref()
            .filter(|(at, _)| v.filter_sorted && *at == rect)
            .map(|(_, s)| s.clone());
        Some(RangeFilterFile {
            rect,
            columns: file_columns(f, rect),
            sort,
        })
    }

    pub fn table_filter_file(&self, table: &str) -> Option<RangeFilterFile> {
        let t = self.table_by_name(table)?;
        if !t.headers || !t.show_filter {
            return None;
        }
        let mut rect = t.rect;
        if t.totals {
            rect.r1 -= 1;
        }
        let columns = self
            .views
            .get(&t.sheet)
            .and_then(|v| {
                v.filters
                    .iter()
                    .find(|f| f.table.as_deref() == Some(&t.name))
            })
            .map_or_else(Vec::new, |f| file_columns(f, rect));
        Some(RangeFilterFile {
            rect,
            columns,
            sort: None,
        })
    }

    pub fn load_range_filter(&mut self, sheet: SheetId, file: &RangeFilterFile) -> bool {
        let taken = self
            .tables
            .iter()
            .any(|t| t.sheet == sheet && t.rect.intersects(&file.rect))
            || self
                .views
                .get(&sheet)
                .is_some_and(|v| v.filters.iter().any(|f| f.table.is_none()));
        if self.sheet(sheet).is_none() || taken {
            return false;
        }
        let mut f = self.new_range_filter(sheet, file.rect);
        let cols = f.rect.cols();
        install(&mut f, cols, file);
        f.extra.sort = file.sort.clone().map(|s| (f.rect, s));
        let v = self.view_mut(sheet);
        v.filter_sorted = f.extra.sort.is_some();
        v.filters.push(f);
        true
    }

    pub fn load_table_filter(&mut self, table: &str, file: Option<&RangeFilterFile>) {
        let Some(t) = self.table_by_name(table) else {
            return;
        };
        if !t.headers {
            return;
        }
        let (sheet, name) = (t.sheet, t.name.clone());
        let mut rect = t.rect;
        if t.totals {
            rect.r1 -= 1;
        }
        let Some(file) = file else {
            self.set_table_look(name.as_str(), |t| t.show_filter = false);
            return;
        };
        let mut f = Filter {
            rect,
            anchor_r1: rect.r1,
            table: Some(name),
            crit: Default::default(),
            total_row: false,
            extra: Default::default(),
        };
        install(&mut f, rect.cols(), file);
        if f.crit.is_empty() && f.extra.buttons.is_empty() {
            return;
        }
        self.view_mut(sheet).filters.push(f);
        let mode = self.filter_mode_for_subtotal(sheet);
        self.view_mut(sheet).fm_last = mode;
    }

    pub fn filter_mode_to_save(&self, sheet: SheetId) -> bool {
        self.views.get(&sheet).is_some_and(|v| {
            v.adv.is_some()
                || v.filters
                    .iter()
                    .any(|f| f.table.is_none() && !f.crit.is_empty())
                || v.fm
                    && v.filters
                        .iter()
                        .any(|f| f.table.is_none() && f.extra.buttons.iter().any(|b| !b.1 && b.2))
        })
    }

    pub fn load_filter_mode(&mut self, sheet: SheetId) {
        let db = self.filter_db_ref(sheet);
        let v = self.view_mut(sheet);
        v.fm = true;
        if !v.filters.iter().any(|f| f.table.is_none()) {
            v.adv = db;
        }
        let mode = self.filter_mode_for_subtotal(sheet);
        self.view_mut(sheet).fm_last = mode;
    }

    pub(super) fn note_filter_values(&mut self, sheet: SheetId, only: Option<(usize, u32)>) {
        let Some(v) = self.views.get(&sheet) else {
            return;
        };
        let mut found: Vec<(usize, ColExtra)> = Vec::new();
        for (i, f) in v.filters.iter().enumerate() {
            let Some(rect) = self.filter_rect(sheet, f) else {
                continue;
            };
            let wanted = f
                .crit
                .iter()
                .filter(|(&field, _)| only.is_none_or(|o| o == (i, field)));
            for (&field, cc) in wanted {
                if let Some(value) = self.evaluated_value(sheet, rect, field, cc) {
                    let x = ColExtra {
                        key: key(cc),
                        value: Some(value),
                        loaded: None,
                    };
                    found.push((i, x));
                }
            }
        }
        for (i, x) in found {
            let columns = &mut self.view_mut(sheet).filters[i].extra.columns;
            columns.retain(|c| c.key != x.key);
            columns.push(x);
        }
    }

    fn evaluated_value(
        &self,
        sheet: SheetId,
        rect: Rect,
        field: u32,
        cc: &ColCrit,
    ) -> Option<(f64, Option<f64>)> {
        let op = cc.op.as_deref()?;
        let c1: f64 = cc.c1.as_deref()?.trim().parse().ok()?;
        let numbers = || -> Vec<f64> {
            (rect.r0 + 1..=rect.r1)
                .filter_map(
                    |r| match self.value(CellAddr::new(sheet, r, rect.c0 + field)) {
                        Value::Number(x) => Some(x),
                        _ => None,
                    },
                )
                .collect()
        };
        match op {
            "top10" | "bottom10" | "top10pct" | "bottom10pct" => {
                top_threshold(numbers(), op, c1).map(|t| (t, None))
            }
            "dynamic" if c1 >= 33.0 => {
                let n = numbers();
                (!n.is_empty()).then(|| (n.iter().sum::<f64>() / n.len() as f64, None))
            }
            "dynamic" => {
                let now = self
                    .calc
                    .fixed_now
                    .unwrap_or_else(crate::workbook::current_serial);
                let (lo, hi) =
                    date_filter_bounds(self.settings.date_system, c1 as u32, now.floor() as i64)?;
                Some((lo as f64, Some(hi as f64)))
            }
            _ => None,
        }
    }
}
