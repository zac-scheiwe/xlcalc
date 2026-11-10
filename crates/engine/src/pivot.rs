use std::cmp::Ordering;
use std::collections::HashMap;

use crate::address::{CellAddr, Rect, SheetId};
use crate::colwidth::{ColFit, PivotFit};
use crate::value::{Value, XlError};
use crate::workbook::{Command, EditError, Workbook};

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum Func {
    Sum,
    Count,
    Average,
    Max,
    Min,
    Product,
    CountNums,
    StdDev,
    StdDevP,
    Var,
    VarP,
}

impl Func {
    pub fn code(self) -> i32 {
        match self {
            Func::Sum => -4157,
            Func::Count => -4112,
            Func::Average => -4106,
            Func::Max => -4136,
            Func::Min => -4139,
            Func::Product => -4149,
            Func::CountNums => 11,
            Func::StdDev => -4155,
            Func::StdDevP => -4156,
            Func::Var => -4164,
            Func::VarP => -4165,
        }
    }

    pub fn from_code(c: i32) -> Option<Func> {
        Some(match c {
            -4157 => Func::Sum,
            -4112 => Func::Count,
            -4106 => Func::Average,
            -4136 => Func::Max,
            -4139 => Func::Min,
            -4149 => Func::Product,
            -4113 | 11 => Func::CountNums,
            -4155 => Func::StdDev,
            -4156 => Func::StdDevP,
            -4164 => Func::Var,
            -4165 => Func::VarP,
            _ => return None,
        })
    }

    pub fn word(self) -> &'static str {
        match self {
            Func::Sum => "Sum",
            Func::Count | Func::CountNums => "Count",
            Func::Average => "Average",
            Func::Max => "Max",
            Func::Min => "Min",
            Func::Product => "Product",
            Func::StdDev => "StdDev",
            Func::StdDevP => "StdDevp",
            Func::Var => "Var",
            Func::VarP => "Varp",
        }
    }
}

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum Orient {
    Hidden,
    Row,
    Column,
    Page,
    Data,
}

impl Orient {
    pub fn code(self) -> i32 {
        match self {
            Orient::Hidden => 0,
            Orient::Row => 1,
            Orient::Column => 2,
            Orient::Page => 3,
            Orient::Data => 4,
        }
    }

    pub fn from_code(c: i32) -> Option<Orient> {
        Some(match c {
            0 => Orient::Hidden,
            1 => Orient::Row,
            2 => Orient::Column,
            3 => Orient::Page,
            4 => Orient::Data,
            _ => return None,
        })
    }
}

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum Form {
    Compact,
    Tabular,
    Outline,
}

impl Form {
    pub fn code(self) -> i32 {
        match self {
            Form::Compact => 0,
            Form::Tabular => 1,
            Form::Outline => 2,
        }
    }

    pub fn from_code(c: i32) -> Option<Form> {
        Some(match c {
            0 => Form::Compact,
            1 => Form::Tabular,
            2 => Form::Outline,
            _ => return None,
        })
    }
}

#[derive(Clone, Debug)]
pub struct Item {
    pub value: Value,

    pub label: String,
}

#[derive(Clone, Debug)]
pub struct CacheField {
    pub name: String,

    pub items: Vec<Item>,

    pub formula: Option<String>,
}

#[derive(Clone, Debug)]
pub struct PivotCache {
    pub source: String,
    pub sheet: SheetId,
    pub rect: Rect,
    pub fields: Vec<CacheField>,

    pub records: Vec<Vec<Value>>,

    pub idx: Vec<Vec<u32>>,

    pub orphaned: bool,

    pub missing_limit: i32,

    pub refresh_open: bool,
}

#[derive(Clone, Debug)]
pub struct DataField {
    pub src: usize,
    pub caption: String,
    pub func: Func,
    pub numfmt: Option<String>,

    pub show: ShowAs,
}

#[derive(Clone, Debug, PartialEq)]
pub struct ShowAs {
    pub kind: i32,
    pub base_field: Option<usize>,
    pub base_item: Option<u32>,
}

impl Default for ShowAs {
    fn default() -> Self {
        ShowAs {
            kind: -4143,
            base_field: None,
            base_item: None,
        }
    }
}

#[derive(Clone, Copy, Debug, Default, PartialEq, Eq)]
pub enum Sort {
    #[default]
    Manual,

    Name(bool),

    Data(bool, usize),
}

#[derive(Clone, Debug)]
pub struct PtField {
    pub orient: Orient,

    pub subtotals: bool,

    pub custom: Vec<u8>,

    pub hidden: Vec<usize>,
    pub sort: Sort,

    pub repeat: bool,

    pub blank_line: bool,

    pub form: Option<Form>,

    pub sub_top: Option<bool>,

    pub filter: Option<Filter>,

    pub page_item: Option<u32>,

    pub multi_page: bool,

    pub calc_items: Vec<CalcItem>,

    pub group: Option<Group>,

    pub manual: Option<Vec<u32>>,

    pub caption: Option<String>,

    pub item_caps: Vec<(u32, String)>,

    pub group_order: Vec<String>,
}

#[derive(Clone, Debug, PartialEq)]
pub enum Group {
    Num {
        start: Option<f64>,
        end: Option<f64>,
        by: f64,
    },

    Derived {
        src: usize,
        start: Option<f64>,
        end: Option<f64>,
        unit: DateUnit,
    },

    Manual {
        src: usize,
        groups: Vec<(String, Vec<String>)>,
    },
}

#[derive(Clone, Copy, Debug, PartialEq)]
pub enum DateUnit {
    Days(u32),
    Month,
    Quarter,
    Year,
}

#[derive(Clone, Debug)]
pub struct CalcItem {
    pub name: String,

    pub formula: String,
}

impl Default for PtField {
    fn default() -> Self {
        PtField {
            orient: Orient::Hidden,
            subtotals: true,
            custom: Vec::new(),
            hidden: Vec::new(),
            sort: Sort::Manual,
            repeat: false,
            blank_line: false,
            form: None,
            sub_top: None,
            filter: None,
            page_item: None,
            multi_page: false,
            calc_items: Vec::new(),
            group: None,
            manual: None,
            caption: None,
            item_caps: Vec::new(),
            group_order: Vec::new(),
        }
    }
}

impl PivotTable {
    pub fn field_name<'a>(&'a self, c: &'a PivotCache, f: usize) -> &'a str {
        self.fields[f]
            .caption
            .as_deref()
            .unwrap_or(&c.fields[f].name)
    }

    pub fn add_field_slot(&mut self) {
        self.fields.push(PtField::default());
    }

    pub fn group_field_name(&self, c: &PivotCache, f: usize) -> String {
        match &self.fields[f].group {
            Some(Group::Derived { src, unit, .. }) => {
                let p = match unit {
                    DateUnit::Days(_) => "Days",
                    DateUnit::Month => "Months",
                    DateUnit::Quarter => "Quarters",
                    DateUnit::Year => "Years",
                };
                format!("{p} ({})", c.fields[*src].name)
            }
            Some(Group::Manual { src, .. }) => {
                let base = c.fields[*src].name.clone();
                let mut n = 2;
                while c
                    .fields
                    .iter()
                    .any(|x| same_text(&x.name, &format!("{base}{n}")))
                {
                    n += 1;
                }
                format!("{base}{n}")
            }
            _ => c.fields[f].name.clone(),
        }
    }

    pub fn add_date_levels(
        &mut self,
        src: usize,
        units: &[DateUnit],
        start: Option<f64>,
        end: Option<f64>,
    ) {
        let first = self.fields.len();
        let on_cols = !self.rows.contains(&Ax::Field(src)) && self.cols.contains(&Ax::Field(src));
        let on_axis = self.rows.contains(&Ax::Field(src)) || on_cols;
        for unit in units {
            self.fields.push(PtField {
                group: Some(Group::Derived {
                    src,
                    start,
                    end,
                    unit: *unit,
                }),
                orient: if on_cols { Orient::Column } else { Orient::Row },
                ..PtField::default()
            });
        }
        let axis = if on_cols {
            &mut self.cols
        } else {
            &mut self.rows
        };
        let at = axis
            .iter()
            .position(|a| *a == Ax::Field(src))
            .unwrap_or(axis.len());
        if on_axis {
            axis.remove(at);
            self.fields[src].orient = Orient::Hidden;
        }
        for k in 0..units.len() {
            axis.insert(at + k, Ax::Field(first + units.len() - 1 - k));
        }
    }

    pub fn add_manual_group(&mut self, src: usize, name: &str, members: Vec<String>) {
        let n = self.fields.len();
        self.fields.push(PtField {
            group: Some(Group::Manual {
                src,
                groups: vec![(name.to_string(), members)],
            }),
            orient: self.fields[src].orient,
            ..PtField::default()
        });
        for axis in [&mut self.rows, &mut self.cols] {
            if let Some(p) = axis.iter().position(|a| *a == Ax::Field(src)) {
                axis.insert(p, Ax::Field(n));
            }
        }
    }

    pub fn remove_group(&mut self, f: usize) {
        match self.fields[f].group.clone() {
            Some(Group::Derived { src, .. }) => {
                let mine: Vec<usize> = (0..self.fields.len())
                    .filter(|&i| {
                        matches!(&self.fields[i].group,
                            Some(Group::Derived { src: s, .. }) if *s == src)
                    })
                    .collect();
                let on_cols = mine.iter().any(|&i| self.cols.contains(&Ax::Field(i)));
                let axis = if on_cols { &self.cols } else { &self.rows };
                let at = axis
                    .iter()
                    .position(|a| matches!(a, Ax::Field(i) if mine.contains(i)))
                    .unwrap_or(0);
                for &i in mine.iter().rev() {
                    self.remove_field(i);
                }
                let axis = if on_cols {
                    &mut self.cols
                } else {
                    &mut self.rows
                };
                axis.insert(at.min(axis.len()), Ax::Field(src));
                self.fields[src].orient = if on_cols { Orient::Column } else { Orient::Row };
            }
            Some(Group::Manual { src, .. }) => {
                for d in &mut self.data {
                    if d.show.base_field.is_some_and(|b| b == src || b == f) {
                        d.show = ShowAs::default();
                    }
                }
                self.remove_field(f)
            }
            _ => self.fields[f].group = None,
        }
    }

    pub fn remove_field(&mut self, f: usize) {
        let gone: Vec<usize> = (0..self.data.len())
            .filter(|&d| self.data[d].src == f)
            .collect();
        for &d in gone.iter().rev() {
            self.data.remove(d);
        }
        let fix = |x: usize| if x > f { x - 1 } else { x };
        self.fields.remove(f);
        self.rows.retain(|a| *a != Ax::Field(f));
        self.cols.retain(|a| *a != Ax::Field(f));
        self.pages.retain(|p| *p != f);
        for a in self.rows.iter_mut().chain(self.cols.iter_mut()) {
            if let Ax::Field(x) = a {
                *x = fix(*x);
            }
        }
        for p in &mut self.pages {
            *p = fix(*p);
        }
        for d in &mut self.data {
            d.src = fix(d.src);
            d.show.base_field = d.show.base_field.map(fix);
        }
        for pf in &mut self.fields {
            let shift = |d: usize| {
                let removed = gone.iter().filter(|&&g| g < d).count();
                d - removed
            };
            match pf.sort {
                Sort::Data(desc, d) if gone.contains(&d) => {
                    let _ = desc;
                    pf.sort = Sort::Manual;
                }
                Sort::Data(desc, d) => pf.sort = Sort::Data(desc, shift(d)),
                _ => {}
            }
            if let Some(fl) = &mut pf.filter {
                if let Some(d) = fl.data {
                    fl.data = (!gone.contains(&d)).then(|| shift(d));
                }
            }
        }
        self.fields_dirty_check();
    }

    fn fields_dirty_check(&mut self) {
        self.filter_cleanup();
    }

    fn filter_cleanup(&mut self) {
        for pf in &mut self.fields {
            if pf
                .filter
                .as_ref()
                .is_some_and(|f| (1..=14).contains(&f.kind) && f.data.is_none())
            {
                pf.filter = None;
            }
        }
    }
}

#[derive(Clone, Debug)]
pub struct Filter {
    pub kind: i32,

    pub data: Option<usize>,
    pub v1: Value,
    pub v2: Value,
}

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum Ax {
    Field(usize),
    Values,
}

#[derive(Clone, Debug)]
pub struct PivotTable {
    pub name: String,
    pub cache: usize,
    pub sheet: SheetId,

    pub row0: u32,
    pub col0: u32,
    pub fields: Vec<PtField>,
    pub rows: Vec<Ax>,
    pub cols: Vec<Ax>,
    pub pages: Vec<usize>,

    pub page_over: bool,

    pub page_wrap: u32,
    pub data: Vec<DataField>,
    pub form: Form,
    pub row_grand: bool,
    pub col_grand: bool,

    pub repeat_all: bool,

    pub style: String,
    pub style_flags: [bool; 5],

    pub row_caption: Option<String>,
    pub col_caption: Option<String>,

    pub written: Option<Rect>,

    pub ranges: Ranges,

    pub opts: Opts,
}

pub const OPT_BOOLS: [(&str, bool); 18] = [
    ("MergeLabels", false),
    ("HasAutoFormat", true),
    ("ShowDrillIndicators", true),
    ("PreserveFormatting", true),
    ("EnableDrilldown", true),
    ("SaveData", true),
    ("InGridDropZones", false),
    ("DisplayContextTooltips", true),
    ("PrintTitles", false),
    ("RepeatItemsOnEachPrintedPage", true),
    ("AllowMultipleFilters", false),
    ("EnableDataValueEditing", false),
    ("ShowValuesRow", false),
    ("ShowPageMultipleItemLabel", true),
    ("DisplayFieldCaptions", true),
    ("DisplayImmediateItems", true),
    ("VisualTotals", true),
    ("TotalsAnnotation", false),
];

#[derive(Clone, Debug)]
pub struct Opts {
    pub display_error: bool,
    pub error_string: String,
    pub display_null: bool,
    pub null_string: String,
    pub bools: [bool; 18],
    pub tag: String,
    pub alt_text: Option<String>,
    pub summary: Option<String>,
    pub compact_indent: i32,
}

impl Default for Opts {
    fn default() -> Opts {
        Opts {
            display_error: false,
            error_string: String::new(),
            display_null: true,
            null_string: String::new(),
            bools: OPT_BOOLS.map(|(_, d)| d),
            tag: String::new(),
            alt_text: None,
            summary: None,
            compact_indent: 1,
        }
    }
}

pub fn option_string_value(s: &str) -> Value {
    if s.is_empty() {
        return Value::Blank;
    }
    let t = s.trim();
    if t.is_empty() || t == "-" {
        return Value::Number(0.0);
    }
    let digits = t.strip_prefix('-').unwrap_or(t);
    if !digits.is_empty() && digits.bytes().all(|b| b.is_ascii_digit()) {
        if let Ok(n) = t.parse::<f64>() {
            return Value::Number(n);
        }
    }
    Value::text(s)
}

#[derive(Clone, Debug, Default, PartialEq, Eq)]
pub struct Ranges {
    pub table1: Option<Rect>,
    pub table2: Option<Rect>,
    pub row: Option<Rect>,
    pub column: Option<Rect>,
    pub data_body: Option<Rect>,
    pub data_label: Option<Rect>,
    pub page: Option<Rect>,
}

#[derive(Clone, Debug, Default)]
pub struct Pivots {
    pub caches: Vec<PivotCache>,
    pub tables: Vec<PivotTable>,
}

const BUTTON_PX: u32 = 25;

const PARENT_PX: u32 = 19;

fn compact_indent_px(level: u32, step: i32) -> u32 {
    const MEASURED: [[u32; 2]; 3] = [[18, 36], [24, 49], [37, 74]];
    match (step, level) {
        (i32::MIN..=0, _) => 0,
        (1..=3, 1..=2) => MEASURED[step as usize - 1][level as usize - 1],
        (1..=3, _) => MEASURED[step as usize - 1][0] * level,
        (n, _) => 12 * n as u32 * level,
    }
}

#[derive(Clone, Debug)]
pub struct RCell {
    pub row: u32,
    pub col: u32,
    pub value: Value,
}

#[derive(Clone, Debug, Default)]
pub struct Rendered {
    pub cells: Vec<RCell>,
    pub rows: u32,
    pub cols: u32,
    pub table1: Option<Rect>,
    pub row: Option<Rect>,
    pub column: Option<Rect>,
    pub data_body: Option<Rect>,
    pub data_label: Option<Rect>,
    pub page: Option<Rect>,

    pub top: u32,

    pub indents: Vec<(u32, u8)>,

    pub row_items: Vec<AxItem>,
    pub col_items: Vec<AxItem>,

    pub hdr_rows: u32,
    pub left_cols: u32,
    pub real_cols: bool,

    pub value_keys: Vec<ValueKey>,

    pub label_cells: Vec<(u32, u32, usize, Option<u32>)>,

    pub hdr_cells: Vec<(u32, u32, u8)>,

    pub(crate) fit: Vec<PivotFit>,

    pub(crate) col_levels: Vec<u8>,

    pub(crate) compact: bool,
}

#[derive(Clone, Debug)]
pub struct ValueKey {
    pub row: u32,
    pub col: u32,
    pub keys: Vec<(usize, u32)>,
    pub data: usize,
}

#[derive(Clone, Debug, Default)]
pub struct AxItem {
    pub kind: u8,

    pub r: u32,

    pub xs: Vec<u32>,

    pub i: Option<u32>,
}

#[derive(Hash, PartialEq, Eq)]
enum Key {
    Blank,
    Num(u64),
    Text(Vec<u8>),
    Bool(bool),
    Err(String),
}

fn key_of(v: &Value) -> Key {
    match v {
        Value::Number(n) => Key::Num(if *n == 0.0 { 0 } else { n.to_bits() }),
        Value::Text(t) if t.is_empty() => Key::Blank,
        Value::Text(t) => Key::Text(crate::text::collate_key(t)),
        Value::Bool(b) => Key::Bool(*b),
        Value::Error(e) => Key::Err(e.to_string()),
        _ => Key::Blank,
    }
}

fn rank(v: &Value) -> u8 {
    match v {
        Value::Number(_) => 0,
        Value::Text(_) => 1,
        Value::Bool(_) => 2,
        Value::Error(_) => 3,
        _ => 4,
    }
}

pub fn item_cmp(a: &Value, b: &Value) -> Ordering {
    match (a, b) {
        (Value::Number(x), Value::Number(y)) => x.partial_cmp(y).unwrap_or(Ordering::Equal),
        (Value::Text(x), Value::Text(y)) => crate::text::collate_compare(x, y),
        (Value::Bool(x), Value::Bool(y)) => x.cmp(y),
        (Value::Error(x), Value::Error(y)) => x.to_string().cmp(&y.to_string()),
        _ => rank(a).cmp(&rank(b)),
    }
}

fn nums(vals: &[&Value]) -> Vec<f64> {
    vals.iter()
        .filter_map(|v| match v {
            Value::Number(n) => Some(*n),
            _ => None,
        })
        .collect()
}

fn subst_fields(
    c: &PivotCache,
    formula: &str,
    mut value: impl FnMut(usize) -> Option<f64>,
) -> Option<String> {
    subst_idents(formula, |name| {
        let f = c
            .fields
            .iter()
            .position(|f| f.name.eq_ignore_ascii_case(name))?;
        value(f)
    })
}

fn exact_literal(v: f64) -> String {
    if !v.is_finite() {
        return format!("{v:?}");
    }
    let head: f64 = format!("{v:.14e}").parse().unwrap_or(v);
    let rest = v - head;
    if rest == 0.0 {
        format!("({head:?})")
    } else {
        format!("({head:?}+({rest:?}))")
    }
}

fn subst_idents(formula: &str, mut value: impl FnMut(&str) -> Option<f64>) -> Option<String> {
    map_idents(formula, |n| value(n).map(exact_literal))
}

pub fn map_idents(formula: &str, mut value: impl FnMut(&str) -> Option<String>) -> Option<String> {
    let ch: Vec<char> = formula.chars().collect();
    let mut out = String::new();
    let mut i = 0;
    while i < ch.len() {
        let x = ch[i];
        if x == '"' {
            out.push(x);
            i += 1;
            while i < ch.len() {
                out.push(ch[i]);
                i += 1;
                if ch[i - 1] == '"' {
                    break;
                }
            }
        } else if x == '\'' {
            let mut j = i + 1;
            while j < ch.len() && ch[j] != '\'' {
                j += 1;
            }
            let name: String = ch[i + 1..j.min(ch.len())].iter().collect();
            out.push_str(&value(&name)?);
            i = j + 1;
        } else if x.is_alphabetic() || x == '_' {
            let mut j = i;
            while j < ch.len() && (ch[j].is_alphanumeric() || ch[j] == '_' || ch[j] == '.') {
                j += 1;
            }
            let name: String = ch[i..j].iter().collect();
            let mut k = j;
            while k < ch.len() && ch[k] == ' ' {
                k += 1;
            }
            let is_fn = k < ch.len() && ch[k] == '(';
            if is_fn || matches!(name.to_uppercase().as_str(), "TRUE" | "FALSE") {
                out.push_str(&name);
            } else {
                out.push_str(&value(&name)?);
            }
            i = j;
        } else {
            out.push(x);
            i += 1;
        }
    }
    Some(out)
}

pub fn calc_formula_valid(c: &PivotCache, formula: &str) -> bool {
    let Some(text) = subst_fields(c, formula, |_| Some(1.0)) else {
        return false;
    };
    let wb = Workbook::new();
    let at = CellAddr::new(wb.active_sheet(), 0, 0);
    wb.compile_normalized(&text, at).is_ok()
}

pub fn aggregate(func: Func, vals: &[&Value]) -> Value {
    if vals.iter().all(|v| matches!(v, Value::Blank)) {
        return Value::Blank;
    }
    let n = nums(vals);
    let count = n.len() as f64;
    match func {
        Func::Sum => Value::Number(n.iter().fold(0.0, |a, b| a + b)),
        Func::Count => {
            Value::Number(vals.iter().filter(|v| !matches!(v, Value::Blank)).count() as f64)
        }
        Func::CountNums => Value::Number(count),
        Func::Average => {
            if n.is_empty() {
                Value::Error(XlError::DIV0)
            } else {
                Value::Number(n.iter().fold(0.0, |a, b| a + b) / count)
            }
        }
        Func::Max => Value::Number(n.iter().copied().fold(f64::NEG_INFINITY, f64::max)).or_zero(&n),
        Func::Min => Value::Number(n.iter().copied().fold(f64::INFINITY, f64::min)).or_zero(&n),
        Func::Product => Value::Number(n.iter().fold(1.0, |a, b| a * b)).or_zero(&n),
        Func::StdDev | Func::Var | Func::StdDevP | Func::VarP => {
            let pop = matches!(func, Func::StdDevP | Func::VarP);
            let min = if pop { 1 } else { 2 };
            if n.len() < min {
                return Value::Error(XlError::DIV0);
            }
            let var = pivot_var(&n, !pop);
            if matches!(func, Func::StdDev | Func::StdDevP) {
                Value::Number(crate::x87::sqrt(var))
            } else {
                Value::Number(var)
            }
        }
    }
}

fn pivot_sq(v: &[f64]) -> (f64, f64) {
    use crate::x87;
    let (mut s, mut q) = (0.0f64, 0.0f64);
    for &x in v {
        s = x87::add(s, x);
        q = x87::add(x87::mul(x, x), q);
    }
    (s, q)
}

fn pivot_var_sq(nf: f64, s: f64, q: f64, sample: bool) -> f64 {
    use crate::x87;
    let t = x87::mul(s, s);
    if sample {
        x87::div(x87::sub(q, x87::div(t, nf)), nf - 1.0)
    } else {
        x87::div(x87::sub(x87::mul(q, nf), t), x87::mul(nf, nf))
    }
}

fn pivot_var(v: &[f64], sample: bool) -> f64 {
    let (s, q) = pivot_sq(v);
    pivot_var_sq(v.len() as f64, s, q, sample)
}

pub fn aggregate_merged(func: Func, groups: &[Vec<&Value>]) -> Value {
    let flat: Vec<&Value> = groups.iter().flatten().copied().collect();
    if matches!(func, Func::Sum) && groups.len() > 1 {
        if flat.iter().all(|v| matches!(v, Value::Blank)) {
            return Value::Blank;
        }
        let mut total = 0.0;
        for g in groups {
            total += nums(g).iter().fold(0.0, |a, b| a + b);
        }
        return Value::Number(total);
    }
    if !matches!(func, Func::StdDev | Func::Var | Func::StdDevP | Func::VarP) || groups.len() < 2 {
        return aggregate(func, &flat);
    }
    if flat.iter().all(|v| matches!(v, Value::Blank)) {
        return Value::Blank;
    }
    let pop = matches!(func, Func::StdDevP | Func::VarP);
    let total: usize = groups.iter().map(|g| nums(g).len()).sum();
    if total < if pop { 1 } else { 2 } {
        return Value::Error(XlError::DIV0);
    }
    let (mut ts, mut tq) = (0.0f64, 0.0f64);
    for g in groups {
        let (s, q) = pivot_sq(&nums(g));
        ts = crate::x87::add(ts, s);
        tq = crate::x87::add(tq, q);
    }
    let var = pivot_var_sq(total as f64, ts, tq, !pop);
    if matches!(func, Func::StdDev | Func::StdDevP) {
        Value::Number(crate::x87::sqrt(var))
    } else {
        Value::Number(var)
    }
}

trait OrZero {
    fn or_zero(self, n: &[f64]) -> Value;
}

impl OrZero for Value {
    fn or_zero(self, n: &[f64]) -> Value {
        if n.is_empty() {
            Value::Number(0.0)
        } else {
            self
        }
    }
}

impl PivotCache {
    fn sort_items(&mut self) {
        for (f, field) in self.fields.iter_mut().enumerate() {
            let mut order: Vec<usize> = (0..field.items.len()).collect();
            order.sort_by(|&a, &b| item_cmp(&field.items[a].value, &field.items[b].value));
            let mut remap = vec![0u32; order.len()];
            for (new, &old) in order.iter().enumerate() {
                remap[old] = new as u32;
            }
            field.items = order.iter().map(|&p| field.items[p].clone()).collect();
            for row in &mut self.idx {
                if let Some(r) = remap.get(row[f] as usize) {
                    row[f] = *r;
                }
            }
        }
    }

    pub fn read(wb: &Workbook, source: &str, sheet: SheetId, rect: Rect) -> Option<PivotCache> {
        if rect.r1 <= rect.r0 {
            return None;
        }
        let ncols = (rect.c1 - rect.c0 + 1) as usize;
        let mut fields: Vec<CacheField> = Vec::with_capacity(ncols);
        for c in rect.c0..=rect.c1 {
            let addr = CellAddr::new(sheet, rect.r0, c);
            let mut name = wb.display_text(addr);
            if name.is_empty() {
                name = format!("Field{}", c - rect.c0 + 1);
            }

            let base = name.clone();
            let mut n = 2;
            while fields.iter().any(|f| crate::text::eq_ci(&f.name, &name)) {
                name = format!("{base}{n}");
                n += 1;
            }
            fields.push(CacheField {
                name,
                items: Vec::new(),
                formula: None,
            });
        }
        let mut maps: Vec<HashMap<Key, u32>> = (0..ncols).map(|_| HashMap::new()).collect();
        let mut records = Vec::new();
        let mut idx = Vec::new();
        for r in rect.r0 + 1..=rect.r1 {
            let mut rec = Vec::with_capacity(ncols);
            let mut ix = Vec::with_capacity(ncols);
            for (i, c) in (rect.c0..=rect.c1).enumerate() {
                let addr = CellAddr::new(sheet, r, c);
                let v = wb.value(addr);
                let v = match v {
                    Value::Array(_) | Value::Lambda(_) => Value::Blank,
                    other => other,
                };
                let k = key_of(&v);
                let at = *maps[i].entry(k).or_insert_with(|| {
                    let label = wb.display_text(addr);
                    fields[i].items.push(Item {
                        value: v.clone(),
                        label,
                    });
                    (fields[i].items.len() - 1) as u32
                });
                rec.push(v);
                ix.push(at);
            }
            records.push(rec);
            idx.push(ix);
        }
        let mut c = PivotCache {
            source: source.to_string(),
            sheet,
            rect,
            fields,
            records,
            idx,
            orphaned: false,
            missing_limit: -1,
            refresh_open: false,
        };
        c.sort_items();
        Some(c)
    }
}

impl PivotTable {
    pub fn new(
        name: &str,
        cache: usize,
        nfields: usize,
        sheet: SheetId,
        row0: u32,
        col0: u32,
    ) -> PivotTable {
        PivotTable {
            name: name.to_string(),
            cache,
            sheet,
            row0,
            col0,
            fields: vec![PtField::default(); nfields],
            rows: Vec::new(),
            cols: Vec::new(),
            pages: Vec::new(),
            page_over: false,
            page_wrap: 0,
            data: Vec::new(),
            form: Form::Compact,
            row_grand: true,
            col_grand: true,
            repeat_all: false,
            style: "PivotStyleLight16".to_string(),
            style_flags: [true, true, false, false, true],
            row_caption: None,
            col_caption: None,
            written: None,
            ranges: Ranges::default(),
            opts: Opts::default(),
        }
    }

    pub fn eff_form(&self, f: usize) -> Form {
        self.fields[f].form.unwrap_or(self.form)
    }

    pub fn sub_at_top(&self, f: usize) -> bool {
        self.fields[f]
            .sub_top
            .unwrap_or(self.eff_form(f) != Form::Tabular)
    }

    pub fn row_columns(&self, rax: &[Ax]) -> (Vec<u32>, Vec<u32>) {
        let per_field = rax
            .iter()
            .any(|a| matches!(a, Ax::Field(f) if self.fields[*f].form.is_some()));
        let form_of = |a: &Ax| match a {
            Ax::Field(f) => self.eff_form(*f),
            Ax::Values => self.form,
        };
        let mut cols = Vec::new();
        let mut ind = Vec::new();
        for k in 0..rax.len() {
            let (c, i) = if !per_field {
                if self.form == Form::Compact {
                    (0, k as u32)
                } else {
                    (k as u32, 0)
                }
            } else if k == 0 {
                (0, 0)
            } else if form_of(&rax[k - 1]) == Form::Compact {
                (cols[k - 1], ind[k - 1] + 1)
            } else {
                (cols[k - 1] + 1, 0)
            };
            cols.push(c);
            ind.push(i);
        }
        (cols, ind)
    }

    pub fn page_grid(&self) -> (u32, u32) {
        let n = self.pages.len() as u32;
        let w = self.page_wrap;
        if n == 0 {
            return (0, 0);
        }
        if self.page_over {
            let cols = if w == 0 { n } else { w.min(n) };
            (n.div_ceil(cols), cols)
        } else {
            let rows = if w == 0 { n } else { w.min(n) };
            (rows, n.div_ceil(rows))
        }
    }

    fn values_axis_rows(&self) -> bool {
        self.rows.contains(&Ax::Values)
    }

    pub fn set_orient(&mut self, field: usize, orient: Orient) {
        let placed = match orient {
            Orient::Row => self.rows.contains(&Ax::Field(field)),
            Orient::Column => self.cols.contains(&Ax::Field(field)),
            Orient::Page => self.pages.contains(&field),
            Orient::Hidden | Orient::Data => false,
        };
        if placed && self.fields[field].orient == orient {
            return;
        }
        self.rows.retain(|a| *a != Ax::Field(field));
        self.cols.retain(|a| *a != Ax::Field(field));
        self.pages.retain(|f| *f != field);
        self.fields[field].orient = orient;
        match orient {
            Orient::Row => self.rows.push(Ax::Field(field)),
            Orient::Column => self.cols.push(Ax::Field(field)),

            Orient::Page => self.pages.insert(0, field),
            Orient::Hidden | Orient::Data => {}
        }
    }

    pub fn add_data(&mut self, d: DataField) {
        self.data.push(d);
        if self.data.len() == 2
            && !self.rows.contains(&Ax::Values)
            && !self.cols.contains(&Ax::Values)
        {
            self.cols.push(Ax::Values);
        }
    }

    pub fn axes(&self) -> (Vec<Ax>, Vec<Ax>) {
        let mut rows = self.rows.clone();
        let mut cols = self.cols.clone();
        if self.data.len() > 1 && !self.values_axis_rows() && !cols.contains(&Ax::Values) {
            cols.push(Ax::Values);
        }
        if self.data.len() <= 1 {
            rows.retain(|a| *a != Ax::Values);
            cols.retain(|a| *a != Ax::Values);
        }
        (rows, cols)
    }
}

#[derive(Clone, Debug)]
struct Line {
    labels: Vec<Option<Value>>,

    keys: Vec<(usize, u32)>,

    data: Option<usize>,

    values: bool,

    func: Option<Func>,

    depth: u8,
}

struct Ctx<'a> {
    t: &'a PivotTable,
    c: &'a PivotCache,

    live: Vec<usize>,

    live_exist: Vec<usize>,

    scratch: std::cell::OnceCell<Workbook>,

    func_ovr: std::cell::Cell<Option<Func>>,

    hide: Hide,
}

impl Ctx<'_> {
    fn repeats(&self, a: Ax, is_row: bool) -> bool {
        (!is_row
            || match a {
                Ax::Field(f) => self.t.eff_form(f) != Form::Compact,
                Ax::Values => self.t.form != Form::Compact,
            })
            && match a {
                Ax::Field(f) => self.t.repeat_all || self.t.fields[f].repeat,
                Ax::Values => false,
            }
    }

    fn calc(&self, field: usize, keys: &[(usize, u32)], depth: u8) -> Value {
        let Some(formula) = &self.c.fields[field].formula else {
            return Value::Blank;
        };
        if depth > 8 {
            return Value::Error(XlError::REF);
        }
        let recs = self.ordered_recs(keys);
        if recs.is_empty() {
            return Value::Blank;
        }
        let text = subst_fields(self.c, formula, |f| {
            if self.c.fields[f].formula.is_some() {
                match self.calc(f, keys, depth + 1) {
                    Value::Number(n) => Some(n),
                    _ => None,
                }
            } else {
                let mut s = 0.0;
                for &r in &recs {
                    if let Value::Number(n) = &self.c.records[r][f] {
                        s += n;
                    }
                }
                Some(s)
            }
        });
        let Some(text) = text else {
            return Value::Error(XlError::VALUE);
        };
        let wb = self.scratch.get_or_init(Workbook::new);
        let at = CellAddr::new(wb.active_sheet(), 0, 0);
        match wb.cfv_eval(&text, at, at) {
            Value::Number(n) => Value::Number(n),
            Value::Error(e) => Value::Error(e),
            _ => Value::Error(XlError::VALUE),
        }
    }

    fn agg(&self, d: usize, keys: &[(usize, u32)]) -> Value {
        self.agg_depth(d, keys, 0)
    }

    fn agg_depth(&self, d: usize, keys: &[(usize, u32)], depth: u8) -> Value {
        if let Some(pos) = keys
            .iter()
            .position(|&(f, it)| it as usize >= self.c.fields[f].items.len())
        {
            return self.calc_item(d, keys, pos, depth);
        }
        let base = self.agg_plain(d, keys);

        if depth > 6 || !matches!(self.t.data[d].func, Func::Sum | Func::Count) {
            return base;
        }
        let mut sum = match &base {
            Value::Number(n) => Some(*n),
            _ => None,
        };
        let mut extra = false;
        for (f, pf) in self.t.fields.iter().enumerate() {
            if pf.calc_items.is_empty()
                || !matches!(pf.orient, Orient::Row | Orient::Column)
                || keys.iter().any(|k| k.0 == f)
            {
                continue;
            }
            let n = self.c.fields[f].items.len();
            for j in 0..pf.calc_items.len() {
                if self.hide.all[f].contains(&(n + j)) {
                    continue;
                }
                let mut ks = keys.to_vec();
                ks.push((f, (n + j) as u32));
                match self.agg_depth(d, &ks, depth + 1) {
                    Value::Number(x) => {
                        sum = Some(sum.unwrap_or(0.0) + x);
                        extra = true;
                    }
                    Value::Error(e) => return Value::Error(e),
                    _ => {}
                }
            }
        }
        if extra {
            sum.map_or(base, Value::Number)
        } else {
            base
        }
    }

    fn calc_item(&self, d: usize, keys: &[(usize, u32)], pos: usize, depth: u8) -> Value {
        let (f, it) = keys[pos];
        let n = self.c.fields[f].items.len();
        let ci = &self.t.fields[f].calc_items[it as usize - n];
        if depth > 8 {
            return Value::Error(XlError::REF);
        }
        if ci.formula.trim().is_empty() {
            return Value::Blank;
        }
        let mut bad = false;
        let text = subst_idents(&ci.formula, |name| {
            let target = self.c.fields[f]
                .items
                .iter()
                .position(|x| x.label.eq_ignore_ascii_case(name))
                .or_else(|| {
                    self.t.fields[f]
                        .calc_items
                        .iter()
                        .position(|x| x.name.eq_ignore_ascii_case(name))
                        .map(|j| n + j)
                });
            let Some(target) = target else {
                bad = true;
                return None;
            };
            let mut ks = keys.to_vec();
            ks[pos] = (f, target as u32);
            match self.agg_depth(d, &ks, depth + 1) {
                Value::Number(x) => Some(x),
                Value::Blank => Some(0.0),
                _ => None,
            }
        });
        let Some(text) = text else {
            return Value::Error(if bad { XlError::NAME } else { XlError::VALUE });
        };
        let wb = self.scratch.get_or_init(Workbook::new);
        let at = CellAddr::new(wb.active_sheet(), 0, 0);
        match wb.cfv_eval(&text, at, at) {
            Value::Number(x) => Value::Number(x),
            Value::Error(e) => Value::Error(e),
            _ => Value::Error(XlError::VALUE),
        }
    }

    fn agg_plain(&self, d: usize, keys: &[(usize, u32)]) -> Value {
        let df = &self.t.data[d];
        if self.c.fields[df.src].formula.is_some() {
            return self.calc(df.src, keys, 0);
        }
        self.agg_func(self.func_ovr.get().unwrap_or(df.func), df.src, keys)
    }

    fn detail_recs(&self, keys: &[(usize, u32)]) -> Vec<usize> {
        let mut arr: Vec<usize> = self.live.clone();
        let hit = |r: usize| keys.iter().all(|&(f, i)| self.c.idx[r][f] == i);
        let (mut i, mut j) = (0usize, arr.len());
        loop {
            while i < j && hit(arr[i]) {
                i += 1;
            }
            while j > i && !hit(arr[j - 1]) {
                j -= 1;
            }
            if j <= i + 1 {
                break;
            }
            arr.swap(i, j - 1);
            i += 1;
            j -= 1;
        }
        arr.truncate(arr.iter().filter(|&&r| hit(r)).count());
        let (rax, cax) = self.t.axes();
        let order: Vec<usize> = rax
            .iter()
            .chain(cax.iter())
            .filter_map(|a| match a {
                Ax::Field(f) => Some(*f),
                Ax::Values => None,
            })
            .collect();
        arr.sort_by_key(|&r| {
            order
                .iter()
                .map(|&f| self.c.idx[r][f])
                .collect::<Vec<u32>>()
        });
        arr
    }

    fn ordered_recs(&self, keys: &[(usize, u32)]) -> Vec<usize> {
        let (rax, cax) = self.t.axes();
        let order: Vec<usize> = rax
            .iter()
            .chain(cax.iter())
            .filter_map(|a| match a {
                Ax::Field(f) => Some(*f),
                Ax::Values => None,
            })
            .collect();
        let mut recs: Vec<usize> = self
            .live
            .iter()
            .copied()
            .filter(|&r| keys.iter().all(|&(f, i)| self.c.idx[r][f] == i))
            .collect();
        recs.sort_by_key(|&r| {
            let mut key: Vec<u32> = Vec::new();
            for &f in &order {
                key.push(self.c.idx[r][f]);
            }
            key
        });
        recs
    }

    fn agg_func(&self, func: Func, src: usize, keys: &[(usize, u32)]) -> Value {
        let recs = if !keys.is_empty()
            && matches!(func, Func::Var | Func::VarP | Func::StdDev | Func::StdDevP)
            && self.live.len() < self.c.idx.len()
        {
            self.detail_recs(keys)
        } else {
            self.ordered_recs(keys)
        };
        let (rax, cax) = self.t.axes();
        let has_cols = cax.iter().any(|a| matches!(a, Ax::Field(_)));
        let row_fields: Vec<usize> = rax
            .iter()
            .filter_map(|a| match a {
                Ax::Field(f) => Some(*f),
                Ax::Values => None,
            })
            .collect();

        let group_by: &[usize] = if has_cols {
            row_fields.get(..1).unwrap_or(&[])
        } else if matches!(func, Func::Sum) {
            &row_fields
        } else {
            &[]
        };

        let sum_levels = matches!(func, Func::Sum) && !has_cols;

        let col_fields: Vec<usize> = cax
            .iter()
            .filter_map(|a| match a {
                Ax::Field(f) => Some(*f),
                Ax::Values => None,
            })
            .collect();

        let row_total = matches!(func, Func::Sum)
            && self.t.data.len() == 1
            && !keys.is_empty()
            && !col_fields.is_empty()
            && keys.len() == row_fields.len()
            && keys.iter().all(|k| row_fields.contains(&k.0));

        let col_total = matches!(func, Func::Sum)
            && !keys.is_empty()
            && !row_fields.is_empty()
            && keys.len() == col_fields.len()
            && keys.iter().all(|k| col_fields.contains(&k.0));
        let group_by: &[usize] = if row_total {
            &col_fields
        } else if col_total {
            &row_fields
        } else {
            group_by
        };
        if (keys.is_empty() || sum_levels || row_total || col_total) && !group_by.is_empty() {
            let mut groups: Vec<Vec<&Value>> = Vec::new();
            let mut last = None;
            for &r in &recs {
                let k: Vec<u32> = group_by.iter().map(|&f| self.c.idx[r][f]).collect();
                if last.as_ref() != Some(&k) {
                    groups.push(Vec::new());
                    last = Some(k);
                }
                if let Some(g) = groups.last_mut() {
                    g.push(&self.c.records[r][src]);
                }
            }
            return aggregate_merged(func, &groups);
        }
        let vals: Vec<&Value> = recs.iter().map(|&r| &self.c.records[r][src]).collect();
        aggregate(func, &vals)
    }

    fn show_denominator_error(
        &self,
        d: usize,
        rk: &[(usize, u32)],
        ck: &[(usize, u32)],
        empty: bool,
    ) -> Option<Value> {
        let sh = &self.t.data[d].show;
        match sh.kind {
            6..=8 => {
                let keys: &[(usize, u32)] = match sh.kind {
                    8 => &[],
                    7 => ck,
                    _ => rk,
                };
                match self.agg(d, keys) {
                    Value::Error(e) => Some(Value::Error(e)),
                    Value::Blank => Some(Value::Error(XlError::DIV0)),
                    _ => None,
                }
            }
            3 | 4 => {
                let (rax, cax) = self.t.axes();
                let bf = sh.base_field.or_else(|| {
                    rax.iter().chain(cax.iter()).find_map(|a| match a {
                        Ax::Field(f) => Some(*f),
                        Ax::Values => None,
                    })
                })?;
                if !rax.contains(&Ax::Field(bf)) && !cax.contains(&Ax::Field(bf)) {
                    return None;
                }
                let all: Vec<(usize, u32)> = rk.iter().chain(ck.iter()).copied().collect();
                all.iter().find(|k| k.0 == bf)?;
                let bi = sh
                    .base_item
                    .or_else(|| self.children(bf, &[]).first().copied())?;

                let first = sh
                    .base_item
                    .or_else(|| (0..self.c.records.len()).map(|r| self.c.idx[r][bf]).min());
                if empty && first.is_some_and(|f| self.children(bf, &[]).contains(&f)) {
                    let mine = all.iter().find(|k| k.0 == bf)?.1;
                    return (Some(mine) != first).then_some(Value::Error(XlError::NULL));
                }
                let ks: Vec<(usize, u32)> = all
                    .iter()
                    .map(|&(f, i)| if f == bf { (f, bi) } else { (f, i) })
                    .collect();
                match self.agg(d, &ks) {
                    Value::Number(_) => None,
                    _ => Some(Value::Error(XlError::NA)),
                }
            }
            _ => None,
        }
    }

    fn show_as(&self, d: usize, rk: &[(usize, u32)], ck: &[(usize, u32)], v: f64) -> Value {
        let sh = &self.t.data[d].show;
        let num = |x: Value| match x {
            Value::Number(n) => Some(n),
            _ => None,
        };
        let ratio = |a: f64, b: f64| {
            if b == 0.0 {
                Value::Error(crate::value::XlError::DIV0)
            } else {
                Value::Number(a / b)
            }
        };
        let all: Vec<(usize, u32)> = rk.iter().chain(ck.iter()).copied().collect();
        match sh.kind {
            8 => num(self.agg(d, &[])).map_or(Value::Blank, |t| ratio(v, t)),
            7 => num(self.agg(d, ck)).map_or(Value::Blank, |t| ratio(v, t)),
            6 => num(self.agg(d, rk)).map_or(Value::Blank, |t| ratio(v, t)),
            9 => {
                let (Some(g), Some(r), Some(c)) = (
                    num(self.agg(d, &[])),
                    num(self.agg(d, rk)),
                    num(self.agg(d, ck)),
                ) else {
                    return Value::Blank;
                };
                ratio(v * g / r, c)
            }
            12 => {
                let (rax, cax) = self.t.axes();
                let bf = sh.base_field.or_else(|| {
                    rax.iter().chain(cax.iter()).find_map(|a| match a {
                        Ax::Field(f) => Some(*f),
                        Ax::Values => None,
                    })
                });
                let Some(bf) = bf else { return Value::Blank };
                if !rax.contains(&Ax::Field(bf)) && !cax.contains(&Ax::Field(bf)) {
                    return Value::Error(XlError::NA);
                }
                if !all.iter().any(|k| k.0 == bf) {
                    return Value::Blank;
                }

                let axis = if rax.contains(&Ax::Field(bf)) {
                    &rax
                } else {
                    &cax
                };
                let pos = axis.iter().position(|x| *x == Ax::Field(bf)).unwrap_or(0);
                let above: Vec<usize> = axis[..pos]
                    .iter()
                    .filter_map(|x| match x {
                        Ax::Field(f) => Some(*f),
                        Ax::Values => None,
                    })
                    .collect();
                if above.is_empty() {
                    return if v == 0.0 {
                        Value::Blank
                    } else {
                        Value::Number(1.0)
                    };
                }
                let below: Vec<usize> = axis[pos..]
                    .iter()
                    .filter_map(|x| match x {
                        Ax::Field(f) => Some(*f),
                        Ax::Values => None,
                    })
                    .collect();
                let keys: Vec<(usize, u32)> = all
                    .iter()
                    .copied()
                    .filter(|k| !below.contains(&k.0))
                    .collect();
                num(self.agg(d, &keys)).map_or(Value::Blank, |t| ratio(v, t))
            }
            10 => {
                let mut up = rk.to_vec();
                up.pop();
                up.extend(ck.iter().copied());
                num(self.agg(d, &up)).map_or(Value::Blank, |t| ratio(v, t))
            }
            11 => {
                let mut left = ck.to_vec();
                left.pop();
                let mut ks = rk.to_vec();
                ks.extend(left);
                num(self.agg(d, &ks)).map_or(Value::Blank, |t| ratio(v, t))
            }
            2..=5 | 13..=15 => {
                let (rax, cax) = self.t.axes();
                let first = |a: &[Ax], b: &[Ax]| {
                    a.iter().chain(b.iter()).find_map(|a| match a {
                        Ax::Field(f) => Some(*f),
                        Ax::Values => None,
                    })
                };
                let bf = sh.base_field.or_else(|| {
                    if sh.kind >= 14 {
                        first(&cax, &rax)
                    } else {
                        first(&rax, &cax)
                    }
                });

                let Some(bf) = bf else { return Value::Blank };
                if !rax.contains(&Ax::Field(bf)) && !cax.contains(&Ax::Field(bf)) {
                    return Value::Error(XlError::NA);
                }

                if matches!(sh.kind, 3 | 4) {
                    let first = sh
                        .base_item
                        .or_else(|| (0..self.c.records.len()).map(|r| self.c.idx[r][bf]).min());
                    if first.is_some_and(|f| !self.children(bf, &[]).contains(&f)) {
                        return Value::Error(XlError::NA);
                    }
                }
                let Some(mine) = all.iter().find(|k| k.0 == bf).map(|k| k.1) else {
                    return Value::Blank;
                };
                let order = self.children(bf, &[]);
                let with = |it: u32| -> Vec<(usize, u32)> {
                    all.iter()
                        .map(|&(f, i)| if f == bf { (f, it) } else { (f, i) })
                        .collect()
                };
                if sh.kind >= 14 {
                    let mut mine_v = None;
                    let mut vals: Vec<f64> = Vec::new();
                    for it in &order {
                        if let Some(x) = num(self.agg(d, &with(*it))) {
                            vals.push(x);
                            if *it == mine {
                                mine_v = Some(x);
                            }
                        }
                    }
                    let Some(x) = mine_v else { return Value::Blank };
                    let better = vals
                        .iter()
                        .filter(|y| if sh.kind == 14 { **y < x } else { **y > x })
                        .count();
                    return Value::Number((better + 1) as f64);
                }
                if sh.kind == 13 {
                    let mut sum = 0.0;
                    let mut total = 0.0;
                    let mut seen = false;
                    for it in &order {
                        total += num(self.agg(d, &with(*it))).unwrap_or(0.0);
                        if !seen {
                            sum = total;
                        }
                        if *it == mine {
                            seen = true;
                        }
                    }
                    return ratio(sum, total);
                }
                if sh.kind == 5 {
                    let mut sum = 0.0;

                    for it in order {
                        sum += num(self.agg(d, &with(it))).unwrap_or(0.0);
                        if it == mine {
                            break;
                        }
                    }
                    return Value::Number(sum);
                }
                let Some(bi) = sh.base_item.or_else(|| order.first().copied()) else {
                    return Value::Blank;
                };
                let Some(rf) = num(self.agg(d, &with(bi))).or((sh.kind == 2).then_some(0.0)) else {
                    let axis = if rax.contains(&Ax::Field(bf)) {
                        &rax
                    } else {
                        &cax
                    };
                    let pos = axis.iter().position(|x| *x == Ax::Field(bf)).unwrap_or(0);
                    let up: Vec<(usize, u32)> = all
                        .iter()
                        .copied()
                        .filter(|k| axis[..pos].contains(&Ax::Field(k.0)))
                        .collect();
                    return if sh.kind == 3
                        && !up.is_empty()
                        && !self.children(bf, &up).contains(&bi)
                    {
                        Value::Error(XlError::NA)
                    } else {
                        Value::Blank
                    };
                };
                match sh.kind {
                    2 if mine != bi => Value::Number(v - rf),
                    4 if mine != bi => {
                        if rf == 0.0 {
                            Value::Error(crate::value::XlError::DIV0)
                        } else {
                            Value::Number((v - rf) / rf)
                        }
                    }
                    3 => ratio(v, rf),
                    _ => Value::Blank,
                }
            }
            _ => Value::Number(v),
        }
    }

    fn children(&self, field: usize, keys: &[(usize, u32)]) -> Vec<u32> {
        let mut seen: Vec<u32> = Vec::new();
        for &r in &self.live_exist {
            if keys.iter().all(|&(f, i)| self.c.idx[r][f] == i) {
                let it = self.c.idx[r][field];
                if !seen.contains(&it) && !self.hide.hidden_under(field, it as usize, keys) {
                    seen.push(it);
                }
            }
        }
        seen.sort_unstable();
        let nreal = self.c.fields[field].items.len();
        let under_calc = keys
            .iter()
            .any(|&(f, i)| i as usize >= self.c.fields[f].items.len());
        if under_calc {
            seen = (0..nreal as u32)
                .filter(|&i| !self.hide.all[field].contains(&(i as usize)))
                .collect();
        }
        let calc_items: Vec<u32> = (0..self.t.fields[field].calc_items.len())
            .map(|j| (nreal + j) as u32)
            .filter(|&i| !self.hide.all[field].contains(&(i as usize)) && !under_calc)
            .collect();
        let placed = matches!(self.t.fields[field].sort, Sort::Manual)
            && self.t.fields[field].manual.is_some();
        if placed || matches!(self.t.fields[field].sort, Sort::Data(..)) {
            seen.extend(calc_items.iter().copied());
        }
        match self.t.fields[field].sort {
            Sort::Manual => {
                if let Some(m) = &self.t.fields[field].manual {
                    let pos = |i: u32| m.iter().position(|x| *x == i).unwrap_or(usize::MAX);
                    seen.sort_by_key(|i| pos(*i));
                }
            }
            Sort::Name(desc) => {
                if !self.t.fields[field].item_caps.is_empty() {
                    let cap = |i: u32| Value::text(self.label(field, i).1.as_str());
                    seen.sort_by(|a, b| item_cmp(&cap(*a), &cap(*b)));
                }
                if desc {
                    seen.reverse();
                }
            }
            Sort::Data(desc, d) => {
                let key = |it: u32| {
                    let mut ks = keys.to_vec();
                    ks.push((field, it));
                    match self.agg(d, &ks) {
                        Value::Number(n) => n,
                        _ => f64::NEG_INFINITY,
                    }
                };
                let mut v: Vec<(u32, f64)> = seen.iter().map(|&i| (i, key(i))).collect();
                v.sort_by(|a, b| {
                    let o = a.1.total_cmp(&b.1);
                    if desc {
                        o.reverse()
                    } else {
                        o
                    }
                });
                seen = v.into_iter().map(|x| x.0).collect();
            }
        }
        if !placed && !matches!(self.t.fields[field].sort, Sort::Data(..)) {
            seen.extend(calc_items);
        }
        seen
    }

    fn label(&self, field: usize, item: u32) -> (Value, String) {
        let nreal = self.c.fields[field].items.len();
        if item as usize >= nreal {
            let name = self.t.fields[field].calc_items[item as usize - nreal]
                .name
                .clone();
            return (Value::text(name.as_str()), name);
        }
        if let Some((_, cap)) = self.t.fields[field].item_caps.iter().find(|x| x.0 == item) {
            return (Value::text(cap.as_str()), cap.clone());
        }
        let it = &self.c.fields[field].items[item as usize];
        match &it.value {
            Value::Blank => (Value::text("(blank)"), "(blank)".to_string()),
            Value::Text(t) if t.is_empty() => (Value::text("(blank)"), "(blank)".to_string()),
            Value::Number(_) | Value::Bool(_) | Value::Error(_) => {
                (it.value.clone(), it.label.clone())
            }
            _ => (Value::text(it.label.as_str()), it.label.clone()),
        }
    }

    fn lines(&self, ax: &[Ax], is_row: bool) -> Vec<Line> {
        let mut out = Vec::new();
        if ax.is_empty() {
            out.push(Line {
                labels: Vec::new(),
                keys: Vec::new(),
                data: None,
                values: true,
                func: None,
                depth: 0,
            });
            return out;
        }
        let mut st = Walk {
            ax,
            is_row,
            keys: Vec::new(),
            cur: vec![None; ax.len()],
            out: &mut out,
        };
        self.walk(&mut st, 0, None);

        if let (true, true, Some(Ax::Field(f))) = (is_row, ax.len() >= 2, ax.last()) {
            let level = ax.len() - 1;

            let value_filtered = self.t.fields[*f]
                .filter
                .as_ref()
                .is_some_and(|flt| flt.kind <= 14);

            let items: Vec<u32> = if value_filtered || st.out.is_empty() {
                Vec::new()
            } else {
                let mut v = self.children(*f, &[]);
                let n = self.c.fields[*f].items.len() as u32;
                let rest: Vec<u32> = (0..n)
                    .filter(|i| !v.contains(i) && !self.hide.all[*f].contains(&(*i as usize)))
                    .collect();
                v.extend(rest);
                v
            };
            for i in items {
                let (_, text) = self.label(*f, i);
                for &ix in &self.t.fields[*f].custom {
                    let (func, name) = sub_func(ix);
                    let mut sl = vec![None; ax.len()];
                    sl[level] = Some(Value::text(format!("{text} {name}")));
                    st.out.push(Line {
                        labels: sl,
                        keys: vec![(*f, i)],
                        data: None,
                        values: true,
                        func: Some(func),
                        depth: ax.len() as u8,
                    });
                }
            }
        }
        out
    }

    fn walk(&self, st: &mut Walk, level: usize, data: Option<usize>) {
        let ax = st.ax;
        let tabular = match ax[level] {
            Ax::Field(f) => self.t.eff_form(f) == Form::Tabular,
            Ax::Values => self.t.form == Form::Tabular,
        };
        let sub_top = match ax[level] {
            Ax::Field(f) => self.t.sub_at_top(f),
            Ax::Values => !tabular,
        };
        let last = level + 1 == ax.len();
        let children: Vec<(Option<u32>, Option<usize>, Value, String)> = match ax[level] {
            Ax::Values => (0..self.t.data.len())
                .map(|d| {
                    let c = self.t.data[d].caption.clone();
                    (None, Some(d), Value::text(c.as_str()), c)
                })
                .collect(),
            Ax::Field(f) => self
                .children(f, &st.keys)
                .into_iter()
                .map(|i| {
                    let (v, s) = self.label(f, i);
                    (Some(i), data, v, s)
                })
                .collect(),
        };
        for (item, d, lv, text) in children {
            st.cur[level] = Some(lv);
            for l in st.cur.iter_mut().skip(level + 1) {
                *l = None;
            }
            if let (Some(i), Ax::Field(f)) = (item, ax[level]) {
                st.keys.push((f, i));
            }
            let nodata = d.or(data);
            if last {
                st.out.push(Line {
                    labels: st.cur.clone(),
                    keys: st.keys.clone(),
                    data: nodata,
                    values: true,
                    func: None,
                    depth: (level + 1) as u8,
                });
                for (k, l) in st.cur.iter_mut().enumerate().take(level + 1) {
                    if self.repeats(ax[k], st.is_row) {
                        continue;
                    }
                    *l = None;
                }
            } else {
                let next_values = ax[level + 1] == Ax::Values;
                let sub_on = match ax[level] {
                    Ax::Field(f) => self.t.fields[f].subtotals,
                    Ax::Values => false,
                };

                let deeper = !next_values && ax[level + 1..].contains(&Ax::Values);

                let header_fn =
                    (st.is_row && !tabular && !sub_on && !next_values && !deeper && sub_top)
                        .then(|| match ax[level] {
                            Ax::Field(f) if self.t.fields[f].custom.len() == 1 => {
                                Some(sub_func(self.t.fields[f].custom[0]).0)
                            }
                            _ => None,
                        })
                        .flatten();
                let top = st.is_row && !tabular;
                if top {
                    st.out.push(Line {
                        labels: st.cur.clone(),
                        keys: st.keys.clone(),
                        data: nodata,
                        values: ((sub_on && sub_top) || header_fn.is_some())
                            && !next_values
                            && !deeper,
                        func: header_fn,
                        depth: (level + 1) as u8,
                    });
                    for (k, l) in st.cur.iter_mut().enumerate().take(level + 1) {
                        if self.repeats(ax[k], st.is_row) {
                            continue;
                        }
                        *l = None;
                    }
                }
                self.walk(st, level + 1, nodata);
                if let Ax::Field(f) = ax[level] {
                    if !next_values && header_fn.is_none() {
                        for &ix in &self.t.fields[f].custom {
                            let (func, name) = sub_func(ix);
                            let mut sl = vec![None; ax.len()];
                            sl[level] = Some(Value::text(format!("{text} {name}")));
                            st.out.push(Line {
                                labels: sl,
                                keys: st.keys.clone(),
                                data: nodata,
                                values: true,
                                func: Some(func),
                                depth: (level + 1) as u8,
                            });
                        }
                    }
                }
                if deeper && sub_on {
                    for (d, df) in self.t.data.iter().enumerate() {
                        let mut sl = vec![None; ax.len()];
                        sl[level] = Some(Value::text(format!("{text} {}", df.caption)));
                        st.out.push(Line {
                            labels: sl,
                            keys: st.keys.clone(),
                            data: Some(d),
                            values: true,
                            func: None,
                            depth: (level + 1) as u8,
                        });
                    }
                } else if !(top && sub_top)
                    && sub_on
                    && !next_values
                    && !matches!(ax[level], Ax::Values)
                {
                    let mut sl = vec![None; ax.len()];
                    sl[level] = Some(Value::text(format!("{text} Total")));
                    st.out.push(Line {
                        labels: sl,
                        keys: st.keys.clone(),
                        data: nodata,
                        values: !next_values,
                        func: None,
                        depth: (level + 1) as u8,
                    });
                }
                if st.is_row && matches!(ax[level], Ax::Field(f) if self.t.fields[f].blank_line) {
                    st.out.push(Line {
                        labels: vec![None; ax.len()],
                        keys: Vec::new(),
                        data: None,
                        values: false,
                        func: None,
                        depth: 0,
                    });
                }
            }
            if item.is_some() {
                st.keys.pop();
            }
        }
    }
}

struct Walk<'a> {
    ax: &'a [Ax],
    is_row: bool,
    keys: Vec<(usize, u32)>,
    cur: Vec<Option<Value>>,
    out: &'a mut Vec<Line>,
}

pub fn sub_func(ix: u8) -> (Func, &'static str) {
    match ix {
        2 => (Func::Sum, "Sum"),
        3 => (Func::Count, "Count"),
        4 => (Func::Average, "Average"),
        5 => (Func::Max, "Max"),
        6 => (Func::Min, "Min"),
        7 => (Func::Product, "Product"),
        8 => (Func::CountNums, "Count"),
        9 => (Func::StdDev, "StdDev"),
        10 => (Func::StdDevP, "StdDevp"),
        11 => (Func::Var, "Var"),
        _ => (Func::VarP, "Varp"),
    }
}

pub fn render(t: &PivotTable, c: &PivotCache) -> Rendered {
    let grouped;
    let c = if t.fields.iter().any(|f| f.group.is_some()) {
        grouped = c.grouped(t);
        &grouped
    } else {
        c
    };
    let (rax, cax) = t.axes();
    let hide = effective_hidden(t, c);
    let live: Vec<usize> = pass_hidden(c, &hide.all)
        .into_iter()
        .filter(|&r| !hide.hides_record(c, r))
        .collect();
    let live_exist = pass_hidden(c, &hide.hard);
    let cx = Ctx {
        t,
        c,
        live,
        live_exist,
        scratch: Default::default(),
        func_ovr: Default::default(),
        hide,
    };
    let nd = t.data.len();
    let empty = rax.is_empty() && cax.is_empty() && nd == 0;
    let mut out = Rendered::default();
    let npages = t.pages.len() as u32;

    let (page_rows, page_cols) = t.page_grid();
    let top = if npages > 0 { page_rows + 1 } else { 0 };
    out.top = top;
    if empty {
        out.rows = top + 18;
        out.cols = 3;
        out.table1 = Some(Rect::new(top, 0, top + 17, 2));
        out.data_body = Some(Rect::new(top + 1, 0, top + 17, 2));
        return out;
    }
    let mut rlines = cx.lines(&rax, true);
    let mut clines = cx.lines(&cax, false);

    if rax.is_empty() && nd == 0 {
        rlines.clear();
    }

    let row_vals = rax.contains(&Ax::Values);
    let col_vals = cax.contains(&Ax::Values);
    if t.col_grand && (!rax.is_empty() || nd > 0) {
        if row_vals {
            for d in 0..nd {
                let mut labels = vec![None; rax.len()];
                labels[0] = Some(Value::text(format!("Total {}", t.data[d].caption)));
                rlines.push(Line {
                    labels,
                    keys: vec![],
                    data: Some(d),
                    values: true,
                    func: None,
                    depth: 0,
                });
            }
        } else if !rax.is_empty() {
            let mut labels = vec![None; rax.len()];
            labels[0] = Some(Value::text("Grand Total"));
            rlines.push(Line {
                labels,
                keys: vec![],
                data: None,
                values: true,
                func: None,
                depth: 0,
            });
        }
    }
    if t.row_grand && cax.iter().any(|a| matches!(a, Ax::Field(_))) {
        if col_vals {
            for d in 0..nd {
                let mut labels = vec![None; cax.len()];
                labels[0] = Some(Value::text(format!("Total {}", t.data[d].caption)));
                clines.push(Line {
                    labels,
                    keys: vec![],
                    data: Some(d),
                    values: true,
                    func: None,
                    depth: 0,
                });
            }
        } else {
            let mut labels = vec![None; cax.len()];
            labels[0] = Some(Value::text("Grand Total"));
            clines.push(Line {
                labels,
                keys: vec![],
                data: None,
                values: true,
                func: None,
                depth: 0,
            });
        }
    }

    let virtual_col = cax.is_empty() && nd > 0;
    let ncl = cax.len();
    let real_cols = cax.iter().any(|a| matches!(a, Ax::Field(_)));
    let hdr_rows: u32 = if real_cols {
        1 + ncl as u32
    } else {
        ncl.max(1) as u32
    };
    let compact = t.form == Form::Compact;
    let (row_col, row_ind) = t.row_columns(&rax);
    let per_field = rax
        .iter()
        .any(|a| matches!(a, Ax::Field(f) if t.fields[*f].form.is_some()));

    let caption_col = rax.is_empty() && real_cols && nd == 1;
    let left_cols: u32 = if rax.is_empty() {
        u32::from(caption_col)
    } else {
        row_col.last().map_or(0, |c| c + 1)
    };
    let ncols_data: u32 = if virtual_col {
        1
    } else if cax.is_empty() {
        0
    } else {
        clines.len() as u32
    };
    let width = left_cols + ncols_data;
    let height = hdr_rows + rlines.len() as u32;
    let mut cells: Vec<RCell> = Vec::new();
    let mut indents: Vec<(u32, u8)> = Vec::new();
    let mut value_keys: Vec<ValueKey> = Vec::new();
    let mut label_cells: Vec<(u32, u32, usize, Option<u32>)> = Vec::new();
    let mut hdr_cells: Vec<(u32, u32, u8)> = Vec::new();
    let mut fit: Vec<PivotFit> = Vec::new();
    let mut put = |r: u32, col: u32, v: Value| {
        cells.push(RCell {
            row: top + r,
            col,
            value: v,
        });
    };
    let txt = |s: &str| Value::Text(s.into());

    let last_hdr = hdr_rows - 1;
    if left_cols > 0 && !caption_col {
        if per_field {
            for j in 0..left_cols {
                let name = if j == 0 {
                    t.row_caption.as_deref().unwrap_or("Row Labels").to_string()
                } else {
                    match rax.get(row_col.iter().position(|c| *c == j).unwrap_or(0)) {
                        Some(Ax::Field(f)) => t.field_name(c, *f).to_string(),
                        _ => "Values".to_string(),
                    }
                };
                put(last_hdr, j, txt(&name));
                if j == 0 {
                    hdr_cells.push((top + last_hdr, 0, 2));
                }
                fit.push(PivotFit::header(top + last_hdr, j, BUTTON_PX));
            }
        } else if compact {
            put(
                last_hdr,
                0,
                txt(t.row_caption.as_deref().unwrap_or("Row Labels")),
            );
            hdr_cells.push((top + last_hdr, 0, 2));
            fit.push(PivotFit::header(top + last_hdr, 0, BUTTON_PX));
        } else {
            for (i, a) in rax.iter().enumerate() {
                let name = match a {
                    Ax::Field(f) => t.field_name(c, *f).to_string(),
                    Ax::Values => "Values".to_string(),
                };
                put(last_hdr, i as u32, txt(&name));
                fit.push(PivotFit::header(top + last_hdr, i as u32, BUTTON_PX));
            }
        }
    }
    if virtual_col {
        put(last_hdr, left_cols, txt(&t.data[0].caption));
        hdr_cells.push((top + last_hdr, left_cols, 1));
        fit.push(PivotFit::header(top + last_hdr, left_cols, 0));
    } else {
        if real_cols {
            if nd == 1 && !caption_col {
                put(0, 0, txt(&t.data[0].caption));
                hdr_cells.push((top, 0, 1));
                fit.push(PivotFit::header(top, 0, 0));
            }
            if compact {
                put(
                    0,
                    left_cols,
                    txt(t.col_caption.as_deref().unwrap_or("Column Labels")),
                );
                hdr_cells.push((top, left_cols, 3));
                fit.push(PivotFit::header(top, left_cols, BUTTON_PX));
            } else {
                for (k, a) in cax.iter().enumerate() {
                    let name = match a {
                        Ax::Field(f) => t.field_name(c, *f).to_string(),
                        Ax::Values => "Values".to_string(),
                    };
                    put(0, left_cols + k as u32, txt(&name));
                    fit.push(PivotFit::header(top, left_cols + k as u32, BUTTON_PX));
                }
            }
        }
        let base = u32::from(real_cols);
        for (j, l) in clines.iter().enumerate() {
            for (lv, lab) in l.labels.iter().enumerate() {
                if let Some(s) = lab {
                    if let Some(Ax::Field(f)) = cax.get(lv) {
                        let item = l.keys.iter().find(|k| k.0 == *f).map(|k| k.1);
                        label_cells.push((top + base + lv as u32, left_cols + j as u32, *f, item));
                    }
                    let parent = lv + 1 < cax.len() && !l.keys.is_empty();
                    fit.push(PivotFit::header(
                        top + base + lv as u32,
                        left_cols + j as u32,
                        if parent { PARENT_PX } else { 0 },
                    ));
                    put(base + lv as u32, left_cols + j as u32, s.clone());
                }
            }
        }
    }

    if caption_col {
        put(hdr_rows, 0, txt(&t.data[0].caption));
        hdr_cells.push((top + hdr_rows, 0, 1));
        fit.push(PivotFit::header(top + hdr_rows, 0, 0));
    }
    for (i, l) in rlines.iter().enumerate() {
        let r = hdr_rows + i as u32;
        for (lv, lab) in l.labels.iter().enumerate() {
            if let Some(s) = lab {
                let col = row_col[lv];
                let level = row_ind[lv];
                if compact || (per_field && (level > 0 || row_col.get(lv + 1) == Some(&col))) {
                    indents.push((top + r, (level as i32 * t.opts.compact_indent) as u8));
                }

                let mut grand = true;
                let mut total = true;
                if rax.get(lv) == Some(&Ax::Values) {
                    let is_total = matches!(s, Value::Text(x)
                        if t.data.iter().any(|d| format!("Total {}", d.caption) == **x));
                    grand = is_total;
                    total = is_total;
                }
                if let Some(Ax::Field(f)) = rax.get(lv) {
                    let item = l.keys.iter().find(|k| k.0 == *f).map(|k| k.1);
                    label_cells.push((top + r, col, *f, item));
                    grand = item.is_none();
                    total = match (item, s) {
                        (Some(i), Value::Text(x)) => c.fields[*f]
                            .items
                            .get(i as usize)
                            .is_none_or(|it| it.label != **x),
                        (Some(_), _) => false,
                        (None, _) => true,
                    };
                }
                let parent = !total && lv + 1 < rax.len();
                let indent = if level > 0 && !total {
                    compact_indent_px(level, t.opts.compact_indent)
                } else {
                    0
                };
                fit.push(PivotFit {
                    row: top + r,
                    col,
                    bold: grand || parent,
                    extra: indent
                        + if parent || (level > 0 && !total) {
                            PARENT_PX
                        } else {
                            0
                        },
                });
                put(r, col, s.clone());
            }
        }
    }

    let cols_iter: Vec<Option<&Line>> = if virtual_col {
        vec![None]
    } else {
        clines.iter().map(Some).collect()
    };
    for (i, rl) in rlines.iter().enumerate() {
        if !rl.values {
            continue;
        }
        for (j, cl) in cols_iter.iter().enumerate() {
            if let Some(cl) = cl {
                if !cl.values {
                    continue;
                }
            }
            let d = rl.data.or_else(|| cl.and_then(|c| c.data)).or(if nd == 1 {
                Some(0)
            } else {
                None
            });
            let Some(d) = d else { continue };
            let df = &t.data[d];
            let mut all_keys = rl.keys.clone();
            if let Some(cl) = cl {
                all_keys.extend(cl.keys.iter().copied());
            }
            let ovr = rl.func.or(cl.and_then(|c| c.func));
            cx.func_ovr.set(ovr);
            let mut v = match ovr {
                Some(fo) => cx.agg_func(fo, df.src, &all_keys),
                None => cx.agg(d, &all_keys),
            };

            let count0 = matches!(df.func, Func::Count | Func::CountNums)
                && matches!(df.show.kind, 6..=9 | 12);
            if matches!(v, Value::Blank) && (matches!(df.show.kind, 2 | 10 | 11 | 13) || count0) {
                v = Value::Number(0.0);
            }
            let ck: &[(usize, u32)] = cl.map_or(&[], |c| c.keys.as_slice());

            let mut shown = false;
            if let (Value::Number(n), true) = (&v, df.show.kind != -4143) {
                v = cx.show_as(d, &rl.keys, ck, *n);
                shown = true;
            } else if let (true, Some(e)) = (
                matches!(v, Value::Blank | Value::Error(_)),
                cx.show_denominator_error(d, &rl.keys, ck, matches!(v, Value::Blank)),
            ) {
                v = e;
            } else if matches!(v, Value::Error(_))
                && (2..=15).contains(&df.show.kind)
                && !matches!(df.show.kind, 6..=8)
            {
                v = Value::Error(XlError::NA);
            }
            cx.func_ovr.set(None);
            if matches!(v, Value::Blank) {
                v = if t.opts.display_null {
                    option_string_value(&t.opts.null_string)
                } else {
                    Value::Error(crate::value::XlError::DIV0)
                };
            }
            if matches!(v, Value::Error(_)) && t.opts.display_error {
                v = option_string_value(&t.opts.error_string);
            }
            if shown || !matches!(v, Value::Blank) {
                let mut keys = rl.keys.clone();
                if let Some(cl) = cl {
                    keys.extend(cl.keys.iter().copied());
                }
                value_keys.push(ValueKey {
                    row: top + hdr_rows + i as u32,
                    col: left_cols + j as u32,
                    keys,
                    data: d,
                });
                if !matches!(v, Value::Blank) {
                    put(hdr_rows + i as u32, left_cols + j as u32, v);
                }
            }
        }
    }

    for (k, f) in t.pages.iter().enumerate() {
        let k = k as u32;
        let (row, col) = if t.page_over {
            (k / page_cols, k % page_cols)
        } else {
            (k % page_rows, k / page_rows)
        };
        cells.push(RCell {
            row,
            col: col * 3,
            value: txt(&c.fields[*f].name),
        });
        cells.push(RCell {
            row,
            col: col * 3 + 1,
            value: txt(&page_text(t, c, *f)),
        });
    }
    let page_width = (3 * page_cols).saturating_sub(1);
    if npages > 0 {
        out.page = Some(Rect::new(0, 0, page_rows - 1, page_width - 1));
    }
    out.cells = cells;
    out.indents = indents;
    out.value_keys = value_keys;
    out.label_cells = label_cells;
    out.hdr_cells = hdr_cells;
    out.fit = fit;

    let mut col_levels = vec![2u8; left_cols as usize];
    if virtual_col {
        col_levels.push(1);
    } else {
        col_levels.extend(
            clines
                .iter()
                .map(|l| if real_cols { l.depth + 1 } else { 1 }),
        );
    }
    out.col_levels = col_levels;
    out.compact = compact;
    out.hdr_rows = hdr_rows;
    out.left_cols = left_cols;
    out.real_cols = real_cols;
    out.row_items = axis_items(t, c, &rax, &rlines, true);
    out.col_items = axis_items(t, c, &cax, &clines, false);
    out.rows = top + height;
    out.cols = width.max(page_width);
    out.table1 = Some(Rect::new(top, 0, top + height - 1, width.saturating_sub(1)));

    if left_cols > 0 && !caption_col {
        out.row = Some(Rect::new(
            top + last_hdr,
            0,
            top + height - 1,
            left_cols - 1,
        ));
    }
    if real_cols || col_vals {
        out.column = Some(Rect::new(top, left_cols, top + last_hdr, width - 1));
    }
    if ncols_data > 0 && !rlines.is_empty() {
        out.data_body = Some(Rect::new(
            top + hdr_rows,
            left_cols,
            top + height - 1,
            width - 1,
        ));
    }
    if nd == 0 && height > 1 {
        out.data_body = Some(Rect::new(
            top + 1,
            0,
            top + height - 1,
            width.saturating_sub(1),
        ));
    }
    out.data_label = if nd == 1 && real_cols {
        Some(Rect::new(top, 0, top, 0))
    } else if nd > 1 && col_vals {
        let vpos = cax.iter().position(|a| *a == Ax::Values).unwrap_or(0) as u32;
        let row = top + u32::from(real_cols) + vpos;
        Some(Rect::new(row, left_cols, row, width - 1))
    } else if nd > 1 && row_vals {
        let vpos = rax.iter().position(|a| *a == Ax::Values).unwrap_or(0) as u32;
        match (compact, out.row) {
            (false, Some(r)) => Some(Rect::new(r.r0, vpos, r.r1, vpos)),
            (_, r) => r,
        }
    } else {
        None
    };
    out
}

impl Workbook {
    pub fn pivots(&self) -> &Pivots {
        &self.pivots
    }

    pub fn pivots_mut(&mut self) -> &mut Pivots {
        &mut self.pivots
    }

    pub fn pivot_update(&mut self, ti: usize) -> Result<(), EditError> {
        self.pivot_update_with(ti, true)
    }

    pub fn pivot_update_with(&mut self, ti: usize, refit: bool) -> Result<(), EditError> {
        let (t, rendered) = {
            let mut t = self
                .pivots
                .tables
                .get(ti)
                .ok_or(EditError::NoSuchSheet)?
                .clone();
            let c = self
                .pivots
                .caches
                .get(t.cache)
                .ok_or(EditError::NoSuchSheet)?;
            let r = render(&t, c);

            if t.row0 < r.top {
                t.row0 = r.top;
                self.pivots.tables[ti].row0 = r.top;
            }
            (t, r)
        };
        let wb_fmts: Vec<String> = match self.pivots.caches.get(t.cache) {
            Some(c) => (0..t.data.len())
                .map(|d| self.pivot_data_format(&t, c, d))
                .collect(),
            None => Vec::new(),
        };
        let value_cells: Vec<(u32, u32, usize)> = rendered
            .value_keys
            .iter()
            .map(|vk| (vk.row, vk.col, vk.data))
            .collect();
        let wb_fmt = wb_fmts.first().cloned().unwrap_or_default();

        let label_fmts: Vec<(Vec<f64>, String)> = match self.pivots.caches.get(t.cache) {
            Some(c) => t
                .rows
                .iter()
                .chain(t.cols.iter())
                .filter_map(|ax| match ax {
                    Ax::Field(f) => Some(*f),
                    Ax::Values => None,
                })
                .filter_map(|f| {
                    let at = CellAddr::new(c.sheet, c.rect.r0 + 1, c.rect.c0 + f as u32);
                    let fmt = self.number_format(at).to_string();
                    (fmt != "General").then(|| {
                        let nums = c.fields[f]
                            .items
                            .iter()
                            .filter_map(|i| match i.value {
                                Value::Number(n) => Some(n),
                                _ => None,
                            })
                            .collect();
                        (nums, fmt)
                    })
                })
                .collect(),
            None => Vec::new(),
        };
        self.command(Command::Pivot, |wb| {
            let sheet = t.sheet;
            let new_written = Rect::new(
                t.row0 - rendered.top,
                t.col0,
                t.row0 - rendered.top + rendered.rows.saturating_sub(1),
                t.col0 + rendered.cols.saturating_sub(1),
            );
            wb.pivots.tables[ti].written = Some(new_written);
            if let Some(old) = t.written {
                let _ = wb.clear_range(sheet, old);
                if let Some(body) = t.ranges.data_body {
                    wb.set_number_format(sheet, body, "General");
                }
                wb.apply_style_with(sheet, old, &|s: &crate::style::Style| {
                    let mut n = s.clone();
                    n.alignment.indent = 0;
                    if n.alignment.horizontal == crate::style::HAlign::Left {
                        n.alignment.horizontal = crate::style::HAlign::default();
                    }
                    n
                });
            }
            let origin_row = t.row0 - rendered.top;
            wb.with_op(crate::workbook::Op::PivotWrite, true, |wb| {
                for cell in &rendered.cells {
                    wb.set_value(
                        CellAddr::new(sheet, origin_row + cell.row, t.col0 + cell.col),
                        cell.value.clone(),
                    );
                }
            });
            let mut fit: Option<Rect> = None;
            for cell in &rendered.cells {
                let Value::Number(n) = cell.value else {
                    continue;
                };
                let in_labels = [rendered.row, rendered.column].iter().flatten().any(|r| {
                    (r.r0..=r.r1).contains(&cell.row) && (r.c0..=r.c1).contains(&cell.col)
                });
                if !in_labels {
                    continue;
                }
                if let Some((_, fmt)) = label_fmts.iter().find(|(v, _)| v.contains(&n)) {
                    let at = Rect::cell(origin_row + cell.row, t.col0 + cell.col);
                    wb.set_number_format(sheet, at, fmt);
                    fit = Some(match fit {
                        Some(f) => Rect::new(
                            f.r0.min(at.r0),
                            f.c0.min(at.c0),
                            f.r1.max(at.r1),
                            f.c1.max(at.c1),
                        ),
                        None => at,
                    });
                }
            }

            if let Some(f) = fit {
                wb.autofit_columns(sheet, f);
            }
            let abs = |r: Rect| {
                Rect::new(
                    origin_row + r.r0,
                    t.col0 + r.c0,
                    origin_row + r.r1,
                    t.col0 + r.c1,
                )
            };
            for &(r, lv) in &rendered.indents {
                let at = Rect::cell(origin_row + r, t.col0);
                wb.apply_style_with(sheet, at, &|s: &crate::style::Style| {
                    let mut n = s.clone();
                    n.alignment.indent = lv;
                    n.alignment.horizontal = crate::style::HAlign::Left;
                    n
                });
            }
            let ranges = Ranges {
                table1: rendered.table1.map(abs),
                table2: Some(abs(Rect::new(
                    0,
                    0,
                    rendered.rows.saturating_sub(1),
                    rendered.cols.saturating_sub(1),
                ))),
                row: rendered.row.map(abs),
                column: rendered.column.map(abs),
                data_body: rendered.data_body.map(abs),
                data_label: rendered.data_label.map(abs),
                page: rendered.page.map(abs),
            };
            if t.data.len() == 1 {
                if let Some(body) = ranges.data_body {
                    let fmt = wb_fmt.clone();
                    if fmt != "General" {
                        wb.set_number_format(sheet, body, &fmt);
                    }
                }
            }
            if t.data.len() > 1 {
                for (r, c, d) in &value_cells {
                    let fmt = &wb_fmts[*d];
                    if fmt != "General" {
                        wb.set_number_format(sheet, Rect::cell(origin_row + r, t.col0 + c), fmt);
                    }
                }
            }
            if refit && t.opts.bools[1] {
                if let Some(whole) = ranges.table2 {
                    let attrs: Vec<PivotFit> = rendered
                        .fit
                        .iter()
                        .map(|a| PivotFit {
                            row: origin_row + a.row,
                            col: t.col0 + a.col,
                            ..*a
                        })
                        .collect();
                    let plan = ColFit {
                        compact: rendered.compact,
                        left_cols: rendered.left_cols,
                        levels: rendered.col_levels.clone(),
                    };
                    wb.autofit_pivot(sheet, whole, &attrs, &plan);
                }
            }
            let tm = &mut wb.pivots.tables[ti];
            tm.written = ranges.table2;
            tm.ranges = ranges;
            Ok(())
        })?;

        if self.settings.mode == crate::workbook::CalcMode::Manual {
            let fids: Vec<crate::workbook::FormulaId> = self
                .calc
                .volatile
                .iter()
                .copied()
                .filter(|f| {
                    let a = self.formula(*f).addr;
                    self.formula_text(a)
                        .is_some_and(|t| t.to_ascii_uppercase().contains("GETPIVOTDATA"))
                })
                .collect();
            if !fids.is_empty() {
                crate::calc::recalc_pending(self, &fids);
            }
        }
        Ok(())
    }
}

impl PivotCache {
    fn group_dates(
        &mut self,
        f: usize,
        (start, end): (Option<f64>, Option<f64>),
        unit: DateUnit,
        levels: bool,
        order: &[String],
    ) {
        use crate::datetime::{ymd_from_serial, DateSystem};
        let ymd = |v: f64| ymd_from_serial(DateSystem::D1900, v.floor() as i64);
        let date_text = |v: f64| {
            let (y, m, d) = ymd(v);
            format!("{d}/{m:02}/{y}")
        };
        let old = self.fields[f].items.clone();
        let nums: Vec<f64> = old
            .iter()
            .filter_map(|i| match &i.value {
                Value::Number(n) => Some(*n),
                _ => None,
            })
            .collect();
        if nums.is_empty() {
            return;
        }
        let min = nums.iter().cloned().fold(f64::INFINITY, f64::min).floor();
        let max = nums
            .iter()
            .cloned()
            .fold(f64::NEG_INFINITY, f64::max)
            .floor();

        let (lo, hi) = (
            start.unwrap_or(min).floor(),
            end.map_or(max + 1.0, f64::floor),
        );
        let mk = |label: String, value: Value| Item { value, label };
        let text = |s: String| mk(s.clone(), Value::text(s.as_str()));
        let mut items: Vec<Item> = vec![text(format!("<{}", date_text(lo)))];
        const MONTHS: [&str; 12] = [
            "Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sept", "Oct", "Nov", "Dec",
        ];

        let bin: Box<dyn Fn(f64) -> u32> = match unit {
            DateUnit::Month => {
                for n in MONTHS {
                    items.push(text(n.to_string()));
                }
                Box::new(move |v| ymd(v).1)
            }
            DateUnit::Quarter => {
                for q in 1..=4 {
                    items.push(text(format!("Qtr{q}")));
                }
                Box::new(move |v| (ymd(v).1 - 1) / 3 + 1)
            }
            DateUnit::Year => {
                let (y0, y1) = (ymd(lo).0, ymd(hi).0);
                let mut ys: Vec<i64> = (y0..=y1).collect();
                ys.sort_by_key(|y| {
                    order
                        .iter()
                        .position(|l| *l == y.to_string())
                        .unwrap_or(usize::MAX)
                });
                for y in &ys {
                    items.push(text(y.to_string()));
                }
                Box::new(move |v| {
                    let y = ymd(v).0;
                    ys.iter().position(|x| *x == y).map_or(0, |p| p as u32 + 1)
                })
            }
            DateUnit::Days(_) if levels => {
                const LEN: [u32; 12] = [31, 29, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31];
                for (m, len) in LEN.iter().enumerate() {
                    for d in 1..=*len {
                        items.push(text(format!("{d}-{}", MONTHS[m])));
                    }
                }
                Box::new(move |v| {
                    let (_, m, d) = ymd(v);
                    LEN[..m as usize - 1].iter().sum::<u32>() + d
                })
            }
            DateUnit::Days(n) => {
                let n = f64::from(n.max(1));
                let k = ((hi - lo) / n).floor() as usize + 1;
                for i in 0..k {
                    let a = lo + i as f64 * n;
                    items.push(text(format!(
                        "{} - {}",
                        date_text(a),
                        date_text((a + n - 1.0).min(hi))
                    )));
                }
                Box::new(move |v| ((v - lo) / n).floor() as u32 + 1)
            }
        };
        let last = items.len() as u32;
        items.push(text(format!(">{}", date_text(hi))));
        let mut extra: Vec<u32> = Vec::new();
        let mut map = vec![0u32; old.len()];
        for (i, it) in old.iter().enumerate() {
            map[i] = match &it.value {
                Value::Number(v) => {
                    let v = v.floor();
                    if v < lo {
                        0
                    } else if v > hi {
                        last
                    } else {
                        bin(v)
                    }
                }
                _ => {
                    extra.push(i as u32);
                    last + extra.len() as u32
                }
            };
        }
        for i in &extra {
            items.push(old[*i as usize].clone());
        }
        self.fields[f].items = items;
        for row in &mut self.idx {
            row[f] = map[row[f] as usize];
        }
    }

    fn push_copy(&mut self, src: usize, name: String) {
        let mut f = self.fields[src].clone();
        f.name = name;
        f.formula = None;
        self.fields.push(f);
        for (rec, ix) in self.records.iter_mut().zip(self.idx.iter_mut()) {
            let v = rec[src].clone();
            rec.push(v);
            let i = ix[src];
            ix.push(i);
        }
    }

    fn group_manual(&mut self, f: usize, groups: &[(String, Vec<String>)]) {
        let old = self.fields[f].items.clone();
        let mut items: Vec<Item> = Vec::new();
        let mut map = vec![0u32; old.len()];
        let mut at_group: Vec<Option<u32>> = vec![None; groups.len()];
        for (i, it) in old.iter().enumerate() {
            let g = groups
                .iter()
                .position(|(_, m)| m.iter().any(|l| same_text(l, &it.label)));
            map[i] = match g {
                Some(g) => match at_group[g] {
                    Some(k) => k,
                    None => {
                        items.push(Item {
                            value: Value::text(groups[g].0.as_str()),
                            label: groups[g].0.clone(),
                        });
                        let k = (items.len() - 1) as u32;
                        at_group[g] = Some(k);
                        k
                    }
                },
                None => {
                    items.push(it.clone());
                    (items.len() - 1) as u32
                }
            };
        }
        self.fields[f].items = items;
        for row in &mut self.idx {
            row[f] = map[row[f] as usize];
        }
    }

    pub fn grouped(&self, t: &PivotTable) -> PivotCache {
        let mut g = self.clone();
        for (f, pf) in t.fields.iter().enumerate() {
            match &pf.group {
                Some(Group::Derived {
                    src,
                    start,
                    end,
                    unit,
                }) => {
                    if f == g.fields.len() && *src < f {
                        g.push_copy(*src, t.group_field_name(&g, f));
                        let levels = t
                            .fields
                            .iter()
                            .filter(|x| matches!(x.group, Some(Group::Derived { src: s, .. }) if s == *src))
                            .count()
                            > 1;
                        g.group_dates(f, (*start, *end), *unit, levels, &pf.group_order);
                    }
                    continue;
                }
                Some(Group::Manual { src, groups }) => {
                    if f == g.fields.len() && *src < f {
                        g.push_copy(*src, t.group_field_name(&g, f));
                        g.group_manual(f, groups);
                    }
                    continue;
                }
                _ => {}
            }
            let Some(Group::Num { start, end, by }) = &pf.group else {
                continue;
            };
            if f >= g.fields.len() {
                continue;
            }
            let data: Vec<f64> = self.fields[f]
                .items
                .iter()
                .filter_map(|i| match &i.value {
                    Value::Number(n) => Some(*n),
                    _ => None,
                })
                .collect();

            let start = start.unwrap_or_else(|| data.iter().cloned().fold(f64::INFINITY, f64::min));
            let end = end.unwrap_or_else(|| data.iter().cloned().fold(f64::NEG_INFINITY, f64::max));
            let by = *by;
            if !start.is_finite() || !end.is_finite() {
                continue;
            }
            let whole = by.fract() == 0.0 && start.fract() == 0.0;
            let nb = (((end - start) / by).ceil() as i64).max(1) as usize;
            let lo = |k: usize| start + k as f64 * by;
            let hi = |k: usize| {
                let natural = if whole { lo(k) + by - 1.0 } else { lo(k) + by };
                if k + 1 == nb {
                    natural.max(end)
                } else {
                    natural
                }
            };
            let mut items: Vec<Item> = Vec::new();
            let mk = |label: String| Item {
                value: Value::text(label.as_str()),
                label,
            };
            items.push(mk(format!("<{}", number_label(start))));
            for k in 0..nb {
                items.push(mk(format!(
                    "{}-{}",
                    number_label(lo(k)),
                    number_label(hi(k))
                )));
            }
            let top = hi(nb - 1);
            items.push(mk(format!(
                ">{}",
                number_label(if whole { lo(nb - 1) + by } else { top })
            )));

            let mut extra: Vec<(usize, u32)> = Vec::new();
            let old = self.fields[f].items.clone();
            let mut map = vec![0u32; old.len()];
            for (i, it) in old.iter().enumerate() {
                map[i] = match &it.value {
                    Value::Number(v) => {
                        if *v < start {
                            0
                        } else if *v > top {
                            (nb + 1) as u32
                        } else {
                            1 + (((v - start) / by).floor() as usize).min(nb - 1) as u32
                        }
                    }
                    _ => {
                        extra.push((i, (nb + 2 + extra.len()) as u32));
                        (nb + 2 + extra.len() - 1) as u32
                    }
                };
            }
            for (i, _) in &extra {
                items.push(old[*i].clone());
            }
            g.fields[f].items = items;
            for row in &mut g.idx {
                row[f] = map[row[f] as usize];
            }
        }
        g
    }

    pub fn add_calc(&mut self, name: &str, formula: &str) {
        self.fields.push(CacheField {
            name: name.to_string(),
            items: Vec::new(),
            formula: Some(formula.to_string()),
        });
        for r in &mut self.records {
            r.push(Value::Blank);
        }
        for r in &mut self.idx {
            r.push(0);
        }
    }

    pub fn keep_calcs(&mut self, old: &PivotCache) {
        self.missing_limit = old.missing_limit;
        self.refresh_open = old.refresh_open;
        for f in &old.fields {
            if let Some(fm) = &f.formula {
                self.add_calc(&f.name.clone(), fm);
            }
        }
    }

    pub fn del_calc(&mut self, f: usize) {
        self.fields.remove(f);
        for r in &mut self.records {
            r.remove(f);
        }
        for r in &mut self.idx {
            r.remove(f);
        }
    }

    pub fn keep_item_order(&mut self, old: &PivotCache, dropped: &[usize]) {
        for (f, field) in self.fields.iter_mut().enumerate() {
            let Some(of) = old
                .fields
                .iter()
                .find(|o| o.name.eq_ignore_ascii_case(&field.name))
            else {
                continue;
            };

            let mut items: Vec<Item> = Vec::new();
            let mut remap = vec![u32::MAX; field.items.len()];
            for oi in &of.items {
                let k = key_of(&oi.value);
                if let Some(p) = field.items.iter().position(|n| key_of(&n.value) == k) {
                    remap[p] = items.len() as u32;
                    items.push(field.items[p].clone());
                } else if !dropped.contains(&f) {
                    items.push(oi.clone());
                }
            }

            let mut first = vec![usize::MAX; remap.len()];
            for (rec, row) in self.idx.iter().enumerate() {
                if let Some(x) = first.get_mut(row[f] as usize) {
                    *x = (*x).min(rec);
                }
            }
            let mut fresh: Vec<usize> =
                (0..remap.len()).filter(|&p| remap[p] == u32::MAX).collect();
            fresh.sort_by_key(|&p| first[p]);
            for p in fresh {
                remap[p] = items.len() as u32;
                items.push(field.items[p].clone());
            }
            field.items = items;
            for row in &mut self.idx {
                if let Some(r) = remap.get(row[f] as usize) {
                    row[f] = *r;
                }
            }
        }
    }
}

impl PivotTable {
    fn follow(&mut self, f: impl Fn(&Rect) -> Option<Rect>) {
        let Some(w) = self.written else { return };
        let Some(n) = f(&w) else { return };
        let (dr, dc) = (
            i64::from(n.r0) - i64::from(w.r0),
            i64::from(n.c0) - i64::from(w.c0),
        );
        let mv = |r: &Rect| {
            Rect::new(
                (i64::from(r.r0) + dr) as u32,
                (i64::from(r.c0) + dc) as u32,
                (i64::from(r.r1) + dr) as u32,
                (i64::from(r.c1) + dc) as u32,
            )
        };
        self.row0 = (i64::from(self.row0) + dr) as u32;
        self.col0 = (i64::from(self.col0) + dc) as u32;
        self.written = Some(n);
        let g = &mut self.ranges;
        for r in [
            &mut g.table1,
            &mut g.table2,
            &mut g.row,
            &mut g.column,
            &mut g.data_body,
            &mut g.data_label,
            &mut g.page,
        ] {
            *r = r.as_ref().map(mv);
        }
    }
}

impl Workbook {
    pub(crate) fn pivots_shift(&mut self, s: &crate::structure::Shift) {
        for t in self.pivots.tables.iter_mut().filter(|t| t.sheet == s.sheet) {
            t.follow(|r| s.adjust_rect(r));
        }

        for c in self.pivots.caches.iter_mut().filter(|c| c.sheet == s.sheet) {
            let Some(n) = s.adjust_rect(&c.rect) else {
                continue;
            };
            if n == c.rect {
                continue;
            }
            if let Some((sheet, tail)) = c.source.rsplit_once('!') {
                let plain = tail.strip_prefix('R').is_some_and(|x| {
                    x.chars()
                        .all(|ch| ch.is_ascii_digit() || matches!(ch, 'C' | ':' | 'R'))
                });
                if plain {
                    c.source = format!(
                        "{sheet}!R{}C{}:R{}C{}",
                        n.r0 + 1,
                        n.c0 + 1,
                        n.r1 + 1,
                        n.c1 + 1
                    );
                }
            }
            c.rect = n;
        }
    }

    pub(crate) fn pivots_grow(&mut self, sheet: SheetId, old: Rect, grow: u32) {
        for c in self.pivots.caches.iter_mut().filter(|c| c.sheet == sheet) {
            let r = c.rect;
            if r.r1 != old.r1 || r.r0 < old.r0 || r.c0 < old.c0 || r.c1 > old.c1 {
                continue;
            }
            let Some((book, tail)) = c.source.rsplit_once('!') else {
                continue;
            };
            let plain = tail.strip_prefix('R').is_some_and(|x| {
                x.chars()
                    .all(|ch| ch.is_ascii_digit() || matches!(ch, 'C' | ':' | 'R'))
            });
            if !plain {
                continue;
            }
            c.source = format!(
                "{book}!R{}C{}:R{}C{}",
                r.r0 + 1,
                r.c0 + 1,
                r.r1 + grow + 1,
                r.c1 + 1
            );
            c.rect.r1 += grow;
        }
    }

    pub(crate) fn pivots_move(&mut self, mv: &crate::moves::Move) {
        use crate::moves::Adj;
        for t in self.pivots.tables.iter_mut() {
            let sheet = t.sheet;
            t.follow(|r| match mv.adjust(sheet, r) {
                Adj::To(n) | Adj::Moved(n) => Some(n),
                _ => None,
            });
        }
    }

    pub fn pivot_field_is_date(&self, ci: usize, f: usize) -> bool {
        let Some(c) = self.pivots.caches.get(ci) else {
            return false;
        };
        let items = &c.fields[f].items;
        if items.is_empty() || !items.iter().all(|i| matches!(i.value, Value::Number(_))) {
            return false;
        }
        let at = CellAddr::new(c.sheet, c.rect.r0 + 1, c.rect.c0 + f as u32);
        crate::NumberFormat::parse(self.number_format(at)).is_ok_and(|n| n.is_date())
    }

    pub fn pivot_meeting(&self, sheet: SheetId, rect: &Rect) -> Option<usize> {
        self.pivots
            .tables
            .iter()
            .position(|t| t.sheet == sheet && t.written.is_some_and(|w| w.intersects(rect)))
    }

    pub fn pivot_data_format(&self, t: &PivotTable, c: &PivotCache, d: usize) -> String {
        let df = &t.data[d];
        if let Some(f) = &df.numfmt {
            return f.clone();
        }
        if matches!(df.show.kind, 3 | 4 | 6 | 7 | 8 | 10 | 11 | 12 | 13) {
            return "0.00%".to_string();
        }
        let at = CellAddr::new(c.sheet, c.rect.r0 + 1, c.rect.c0 + df.src as u32);
        self.number_format(at).to_string()
    }

    pub fn pivot_move(
        &mut self,
        ti: usize,
        sheet: SheetId,
        row: u32,
        col: u32,
    ) -> Result<bool, EditError> {
        let t = self
            .pivots
            .tables
            .get(ti)
            .ok_or(EditError::NoSuchSheet)?
            .clone();
        let c = self
            .pivots
            .caches
            .get(t.cache)
            .ok_or(EditError::NoSuchSheet)?;
        let r = render(&t, c);
        if row < r.top {
            return Ok(false);
        }
        let new = Rect::new(
            row - r.top,
            col,
            row - r.top + r.rows.saturating_sub(1),
            col + r.cols.saturating_sub(1),
        );
        if let Some(k) = self.pivot_meeting(sheet, &new) {
            if k != ti {
                return Ok(false);
            }
        }
        if t.sheet != sheet {
            if let Some(old) = t.written {
                self.command(Command::Pivot, |wb| {
                    let _ = wb.clear_range(t.sheet, old);
                    wb.apply_style_with(t.sheet, old, &|s: &crate::style::Style| {
                        let mut n = s.clone();
                        n.alignment.indent = 0;
                        if n.alignment.horizontal == crate::style::HAlign::Left {
                            n.alignment.horizontal = crate::style::HAlign::default();
                        }
                        n
                    });
                    Ok(())
                })?;
            }
            let tm = &mut self.pivots.tables[ti];
            tm.written = None;
            tm.ranges = Ranges::default();
        }
        let tm = &mut self.pivots.tables[ti];
        tm.sheet = sheet;
        tm.row0 = row;
        tm.col0 = col;
        self.pivot_update(ti)?;
        Ok(true)
    }

    pub fn pivot_forget(&mut self, ti: usize, orphan: bool) {
        let cache = self.pivots.tables.remove(ti).cache;
        if orphan && !self.pivots.tables.iter().any(|t| t.cache == cache) {
            if let Some(c) = self.pivots.caches.get_mut(cache) {
                c.orphaned = true;
            }
        }
    }
}

impl Workbook {
    pub fn pivot_place(&mut self, ti: usize) {
        let Some(t) = self.pivots.tables.get(ti) else {
            return;
        };
        let Some(c) = self.pivots.caches.get(t.cache) else {
            return;
        };
        let r = render(t, c);
        let origin = t.row0.saturating_sub(r.top);
        let col0 = t.col0;
        let abs = |x: Rect| Rect::new(origin + x.r0, col0 + x.c0, origin + x.r1, col0 + x.c1);
        let ranges = Ranges {
            table1: r.table1.map(abs),
            table2: Some(abs(Rect::new(
                0,
                0,
                r.rows.saturating_sub(1),
                r.cols.saturating_sub(1),
            ))),
            row: r.row.map(abs),
            column: r.column.map(abs),
            data_body: r.data_body.map(abs),
            data_label: r.data_label.map(abs),
            page: r.page.map(abs),
        };
        let tm = &mut self.pivots.tables[ti];
        tm.written = ranges.table2;
        tm.ranges = ranges;
    }
}

fn pass_hidden(c: &PivotCache, hidden: &[Vec<usize>]) -> Vec<usize> {
    (0..c.records.len())
        .filter(|&r| {
            hidden
                .iter()
                .enumerate()
                .all(|(f, h)| h.is_empty() || !h.contains(&(c.idx[r][f] as usize)))
        })
        .collect()
}

pub fn page_text(t: &PivotTable, c: &PivotCache, f: usize) -> String {
    let pf = &t.fields[f];
    if let Some(i) = pf.page_item {
        return c.fields[f].items[i as usize].label.clone();
    }
    if !pf.hidden.is_empty() {
        let n = c.fields[f].items.len();
        let vis: Vec<usize> = (0..n).filter(|i| !pf.hidden.contains(i)).collect();
        if pf.multi_page && vis.len() == 1 {
            return c.fields[f].items[vis[0]].label.clone();
        }
        return "(Multiple Items)".to_string();
    }
    "(All)".to_string()
}

fn label_matches(kind: i32, label: &str, v1: &str, v2: &str) -> bool {
    let (l, a, b) = (label.to_lowercase(), v1.to_lowercase(), v2.to_lowercase());
    match kind {
        15 => l == a,
        16 => l != a,
        17 => l.starts_with(&a),
        18 => !l.starts_with(&a),
        19 => l.ends_with(&a),
        20 => !l.ends_with(&a),
        21 => l.contains(&a),
        22 => !l.contains(&a),
        23 => l > a,
        24 => l >= a,
        25 => l < a,
        26 => l <= a,
        27 => l >= a && l <= b,
        28 => !(l >= a && l <= b),
        _ => true,
    }
}

#[derive(Clone, Debug, Default)]
pub struct Hide {
    pub hard: Vec<Vec<usize>>,

    pub all: Vec<Vec<usize>>,

    pub parent: Vec<ParentHide>,
}

#[derive(Clone, Debug)]
pub struct ParentHide {
    pub field: usize,
    pub keys: Vec<(usize, u32)>,
    pub items: Vec<usize>,
}

impl Hide {
    fn hides_record(&self, c: &PivotCache, r: usize) -> bool {
        self.parent.iter().any(|p| {
            p.items.contains(&(c.idx[r][p.field] as usize))
                && p.keys.iter().all(|&(f, i)| c.idx[r][f] == i)
        })
    }

    fn hidden_under(&self, field: usize, item: usize, keys: &[(usize, u32)]) -> bool {
        self.all[field].contains(&item)
            || self.parent.iter().any(|p| {
                p.field == field
                    && p.items.contains(&item)
                    && p.keys.iter().all(|k| keys.contains(k))
            })
    }
}

fn value_keep(
    kind: i32,
    a: f64,
    b: f64,
    vals: &[(usize, Option<f64>)],
    errs: &[usize],
) -> Vec<usize> {
    match kind {
        7..=14 => vals
            .iter()
            .filter(|(_, v)| {
                v.is_some_and(|x| match kind {
                    7 => x == a,
                    8 => x != a,
                    9 => x > a,
                    10 => x >= a,
                    11 => x < a,
                    12 => x <= a,
                    13 => x >= a && x <= b,
                    _ => !(x >= a && x <= b),
                })
            })
            .map(|(i, _)| *i)
            .collect(),
        1..=6 => {
            let desc = matches!(kind, 1 | 3 | 5);
            let mut order: Vec<(usize, f64)> = vals
                .iter()
                .filter_map(|(i, v)| v.map(|x| (*i, x)))
                .collect();
            order.sort_by(|x, y| {
                let o = x.1.total_cmp(&y.1);
                if desc {
                    o.reverse()
                } else {
                    o
                }
            });
            let total: f64 = order.iter().map(|x| x.1).sum();

            order.extend(errs.iter().map(|&i| (i, 0.0)));
            let mut keep = Vec::new();
            let mut cum = 0.0;
            let mut last: Option<f64> = None;
            for (i, x) in order {
                match kind {
                    1 | 2 => {
                        if (keep.len() as f64) >= a && last != Some(x) {
                            break;
                        }
                    }
                    3 | 4 => {
                        if total != 0.0 && cum / total * 100.0 >= a {
                            break;
                        }
                    }
                    _ => {
                        if cum >= a {
                            break;
                        }
                    }
                }
                cum += x;
                last = Some(x);
                keep.push(i);
            }
            keep
        }
        _ => vals.iter().map(|(i, _)| *i).collect(),
    }
}

pub fn effective_hidden(t: &PivotTable, c: &PivotCache) -> Hide {
    let mut hidden: Vec<Vec<usize>> = t.fields.iter().map(|f| f.hidden.clone()).collect();

    for (f, h) in hidden.iter_mut().enumerate() {
        let on = t.rows.contains(&Ax::Field(f))
            || t.cols.contains(&Ax::Field(f))
            || t.pages.contains(&f);
        if !on {
            h.clear();
        }
    }
    for (f, pf) in t.fields.iter().enumerate() {
        if let (Some(sel), true) = (pf.page_item, t.pages.contains(&f)) {
            hidden[f] = (0..c.fields[f].items.len())
                .filter(|&i| i != sel as usize)
                .collect();
        }
    }
    for (f, pf) in t.fields.iter().enumerate() {
        let Some(flt) = &pf.filter else { continue };
        if !(15..=28).contains(&flt.kind) {
            continue;
        }
        let n = c.fields[f].items.len();
        let text = |v: &Value| match v {
            Value::Text(s) => s.to_string(),
            Value::Number(x) => crate::coerce::to_text(&Value::Number(*x))
                .map(|t| t.to_string())
                .unwrap_or_default(),
            _ => String::new(),
        };
        let (v1, v2) = (text(&flt.v1), text(&flt.v2));
        for i in 0..n {
            if !label_matches(flt.kind, &c.fields[f].items[i].label, &v1, &v2)
                && !hidden[f].contains(&i)
            {
                hidden[f].push(i);
            }
        }
    }
    let mut hide = Hide {
        hard: hidden.clone(),
        all: hidden,
        parent: Vec::new(),
    };

    let (rax, cax) = t.axes();
    let mut order: Vec<usize> = Vec::new();
    let mut parents: Vec<Vec<usize>> = vec![Vec::new(); t.fields.len()];
    for ax in [&rax, &cax] {
        let mut before: Vec<usize> = Vec::new();
        for a in ax {
            if let Ax::Field(f) = a {
                parents[*f] = before.clone();
                before.push(*f);
                order.push(*f);
            }
        }
    }
    let rest: Vec<usize> = (0..t.fields.len()).filter(|f| !order.contains(f)).collect();
    order.extend(rest);
    for f in order {
        let Some(flt) = &t.fields[f].filter else {
            continue;
        };
        let Some(d) = flt.data.filter(|_| !(15..=28).contains(&flt.kind)) else {
            continue;
        };

        if !t.rows.contains(&Ax::Field(f))
            && !t.cols.contains(&Ax::Field(f))
            && !t.pages.contains(&f)
        {
            continue;
        }
        let n = c.fields[f].items.len();
        let base: Vec<usize> = pass_hidden(c, &hide.all)
            .into_iter()
            .filter(|&r| !hide.hides_record(c, r))
            .collect();
        let probe = Ctx {
            t,
            c,
            live: base.clone(),
            live_exist: base.clone(),
            scratch: Default::default(),
            func_ovr: Default::default(),
            hide: hide.clone(),
        };
        let num = |v: &Value| match v {
            Value::Number(x) => *x,
            _ => f64::NAN,
        };
        let (a, b) = (num(&flt.v1), num(&flt.v2));
        let eval = |keys: &[(usize, u32)], items: Vec<usize>| {
            let mut vals: Vec<(usize, Option<f64>)> = Vec::new();
            let mut errs: Vec<usize> = Vec::new();
            for i in items {
                let mut ks = keys.to_vec();
                ks.push((f, i as u32));
                match probe.agg(d, &ks) {
                    Value::Number(x) => vals.push((i, Some(x))),
                    Value::Error(_) => {
                        vals.push((i, None));
                        errs.push(i);
                    }
                    _ => vals.push((i, None)),
                }
            }
            value_keep(flt.kind, a, b, &vals, &errs)
        };
        if parents[f].is_empty() {
            let items: Vec<usize> = (0..n).filter(|i| !hide.all[f].contains(i)).collect();
            let keep = eval(&[], items);
            for i in 0..n {
                if !keep.contains(&i) && !hide.all[f].contains(&i) {
                    hide.all[f].push(i);
                }
            }
            continue;
        }

        let mut combos: Vec<Vec<(usize, u32)>> = Vec::new();
        for &r in &base {
            let k: Vec<(usize, u32)> = parents[f].iter().map(|&p| (p, c.idx[r][p])).collect();
            if !combos.contains(&k) {
                combos.push(k);
            }
        }
        for k in combos {
            let mut items: Vec<usize> = Vec::new();
            for &r in &base {
                if k.iter().all(|&(p, i)| c.idx[r][p] == i) {
                    let it = c.idx[r][f] as usize;
                    if !items.contains(&it) && !hide.all[f].contains(&it) {
                        items.push(it);
                    }
                }
            }
            items.sort_unstable();
            let keep = eval(&k, items.clone());
            let gone: Vec<usize> = items.into_iter().filter(|i| !keep.contains(i)).collect();
            if !gone.is_empty() {
                hide.parent.push(ParentHide {
                    field: f,
                    keys: k,
                    items: gone,
                });
            }
        }
    }
    hide
}

pub fn shown_order(t: &PivotTable, c: &PivotCache, f: usize) -> Vec<u32> {
    let hide = effective_hidden(t, c);
    let live: Vec<usize> = pass_hidden(c, &hide.all)
        .into_iter()
        .filter(|&r| !hide.hides_record(c, r))
        .collect();
    let live_exist = pass_hidden(c, &hide.hard);
    let cx = Ctx {
        t,
        c,
        live,
        live_exist,
        scratch: Default::default(),
        func_ovr: Default::default(),
        hide,
    };
    let mut v = cx.children(f, &[]);
    let n = c.fields[f].items.len() as u32;
    v.retain(|&i| i < n);
    let rest: Vec<u32> = (0..n).filter(|i| !v.contains(i)).collect();
    v.extend(rest);
    v
}

pub fn display_order(t: &PivotTable, c: &PivotCache, f: usize) -> Vec<u32> {
    let mut v: Vec<u32> = (0..c.fields[f].items.len() as u32).collect();
    if t.fields[f].sort == Sort::Name(true) {
        v.reverse();
    }
    let n = c.fields[f].items.len();
    v.extend((0..t.fields[f].calc_items.len()).map(|j| (n + j) as u32));

    if let (Sort::Manual, Some(m)) = (&t.fields[f].sort, &t.fields[f].manual) {
        v.sort_by_key(|i| m.iter().position(|x| x == i).unwrap_or(usize::MAX));
    }
    v
}

fn axis_items(
    t: &PivotTable,
    c: &PivotCache,
    ax: &[Ax],
    lines: &[Line],
    is_row: bool,
) -> Vec<AxItem> {
    let tabular = t.form == Form::Tabular;
    let mut out: Vec<AxItem> = Vec::new();
    let mut prev: Vec<u32> = Vec::new();
    for l in lines {
        let values_only = matches!(ax, [Ax::Values]);
        if l.keys.is_empty() && l.labels.iter().flatten().count() > 0 && !values_only {
            let d = l.data;
            out.push(AxItem {
                kind: 2,
                r: 0,
                xs: vec![d.unwrap_or(0) as u32],
                i: d.filter(|&d| d > 0).map(|d| d as u32),
            });
            continue;
        }
        let Some(depth) = l.labels.iter().rposition(|x| x.is_some()).map(|p| p + 1) else {
            continue;
        };
        let mut path: Vec<u32> = Vec::new();
        let mut has_values = false;
        for a in ax.iter().take(depth) {
            match a {
                Ax::Field(f) => {
                    let item = l.keys.iter().find(|k| k.0 == *f).map_or(0, |k| k.1);
                    let order = display_order(t, c, *f);
                    path.push(order.iter().position(|&o| o == item).unwrap_or(0) as u32);
                }
                Ax::Values => {
                    has_values = true;
                    path.push(l.data.unwrap_or(0) as u32);
                }
            }
        }
        let default = (!is_row || tabular) && depth < ax.len() && !has_values;
        let r = if default {
            0
        } else {
            path.iter().zip(&prev).take_while(|(a, b)| a == b).count() as u32
        };
        let r = r.min(path.len().saturating_sub(1) as u32);
        let custom = l
            .func
            .and_then(|f| (2..=12u8).find(|&ix| sub_func(ix).0 == f));
        out.push(AxItem {
            kind: custom.map_or(u8::from(default), |ix| 10 + ix),
            r,
            xs: path[r as usize..].to_vec(),
            i: l.data.filter(|&d| d > 0 && has_values).map(|d| d as u32),
        });
        prev = path;
    }
    out
}

fn same_text(a: &str, b: &str) -> bool {
    a.to_lowercase() == b.to_lowercase()
}

#[derive(Clone, Debug)]
pub enum CellRole {
    Label(usize, usize, Option<u32>),

    Header(usize, u8),
}

impl Workbook {
    pub fn pivot_cell_role(&self, addr: CellAddr) -> Option<CellRole> {
        let at = Rect::cell(addr.row, addr.col);
        let ti = self.pivot_meeting(addr.sheet, &at)?;
        let t = &self.pivots.tables[ti];
        let c = self.pivots.caches.get(t.cache)?;
        let c = &c.grouped(t);
        let r = render(t, c);
        let origin = t.row0.checked_sub(r.top)?;
        let hit = |row: u32, col: u32| origin + row == addr.row && t.col0 + col == addr.col;
        if let Some(l) = r.label_cells.iter().find(|l| hit(l.0, l.1)) {
            return Some(CellRole::Label(ti, l.2, l.3));
        }
        r.hdr_cells
            .iter()
            .find(|h| hit(h.0, h.1))
            .map(|h| CellRole::Header(ti, h.2))
    }
}

impl Workbook {
    pub fn pivot_sort_order(&self, addr: CellAddr, desc: bool) -> Option<(usize, usize, Vec<u32>)> {
        let at = Rect::cell(addr.row, addr.col);
        let ti = self.pivot_meeting(addr.sheet, &at)?;
        let t = &self.pivots.tables[ti];
        let c = self.pivots.caches.get(t.cache)?;
        let r0 = render(t, c);
        let origin = t.row0.checked_sub(r0.top)?;
        let mut t2 = t.clone();
        let field;
        if let Some(l) = r0
            .label_cells
            .iter()
            .find(|l| origin + l.0 == addr.row && t.col0 + l.1 == addr.col)
        {
            field = l.2;
            t2.fields[field].sort = Sort::Name(desc);
        } else {
            let vk = r0
                .value_keys
                .iter()
                .find(|v| origin + v.row == addr.row && t.col0 + v.col == addr.col)?;
            field = t
                .rows
                .iter()
                .rev()
                .filter_map(|a| match a {
                    Ax::Field(f) => Some(*f),
                    _ => None,
                })
                .find(|f| vk.keys.iter().any(|k| k.0 == *f))?;
            t2.fields[field].sort = Sort::Data(desc, vk.data);
        }
        let r = render(&t2, c);
        let mut order: Vec<u32> = Vec::new();
        for l in &r.label_cells {
            if l.2 == field {
                if let Some(i) = l.3 {
                    if !order.contains(&i) {
                        order.push(i);
                    }
                }
            }
        }
        let n = c.fields[field].items.len() as u32;
        for i in 0..n {
            if !order.contains(&i) {
                order.push(i);
            }
        }
        Some((ti, field, order))
    }
}

impl Workbook {
    pub fn pivot_label_at(&self, addr: CellAddr) -> Option<(usize, usize, Option<u32>)> {
        let at = Rect::cell(addr.row, addr.col);
        let ti = self.pivot_meeting(addr.sheet, &at)?;
        let t = &self.pivots.tables[ti];
        let c = self.pivots.caches.get(t.cache)?;
        let r = render(t, c);
        let origin = t.row0.checked_sub(r.top)?;
        r.label_cells
            .iter()
            .find(|l| origin + l.0 == addr.row && t.col0 + l.1 == addr.col)
            .map(|l| (ti, l.2, l.3))
    }
}
impl Workbook {
    pub fn pivot_detail_at(&self, addr: CellAddr) -> Option<Vec<Vec<Value>>> {
        let at = Rect::cell(addr.row, addr.col);
        let ti = self.pivot_meeting(addr.sheet, &at)?;
        let t = &self.pivots.tables[ti];
        let c = self.pivots.caches.get(t.cache)?;
        let r = render(t, c);
        let origin = t.row0.checked_sub(r.top)?;
        let vk = r
            .value_keys
            .iter()
            .find(|v| origin + v.row == addr.row && t.col0 + v.col == addr.col)?;
        let grouped;
        let c = if t.fields.iter().any(|f| f.group.is_some()) {
            grouped = c.grouped(t);
            &grouped
        } else {
            c
        };
        let hide = effective_hidden(t, c);
        let live: Vec<usize> = pass_hidden(c, &hide.all)
            .into_iter()
            .filter(|&r| !hide.hides_record(c, r))
            .collect();
        let live_exist = pass_hidden(c, &hide.hard);
        let cx = Ctx {
            t,
            c,
            live,
            live_exist,
            scratch: Default::default(),
            func_ovr: Default::default(),
            hide,
        };
        let cols: Vec<usize> = (0..c.fields.len())
            .filter(|&f| c.fields[f].formula.is_none())
            .collect();
        let mut out = vec![cols
            .iter()
            .map(|&f| Value::from(c.fields[f].name.clone()))
            .collect::<Vec<_>>()];
        for rec in cx.detail_recs(&vk.keys) {
            out.push(cols.iter().map(|&f| c.records[rec][f].clone()).collect());
        }
        Some(out)
    }
}

impl Workbook {
    pub fn get_pivot_data(
        &self,
        data: &Value,
        anchor: CellAddr,
        pairs: &[(Value, Value)],
    ) -> Value {
        let at = Rect::cell(anchor.row, anchor.col);
        let ref_err = Value::Error(crate::value::XlError::REF);
        let Some(ti) = self.pivot_meeting(anchor.sheet, &at) else {
            return ref_err;
        };
        match self.pivot_data_cell(ti, data, pairs) {
            Some(a) => self.value(a),
            None => ref_err,
        }
    }

    pub fn pivot_data_cell(
        &self,
        ti: usize,
        data: &Value,
        pairs: &[(Value, Value)],
    ) -> Option<CellAddr> {
        let t = &self.pivots.tables[ti];
        let c = self.pivots.caches.get(t.cache)?;
        let grouped;
        let c = if t.fields.iter().any(|f| f.group.is_some()) {
            grouped = c.grouped(t);
            &grouped
        } else {
            c
        };
        let name = match data {
            Value::Text(s) => s.to_string(),
            Value::Number(n) => crate::coerce::to_text(&Value::Number(*n))
                .map(|t| t.to_string())
                .unwrap_or_default(),
            _ => return None,
        };
        let d = t
            .data
            .iter()
            .position(|d| same_text(&d.caption, &name))
            .or_else(|| {
                t.data
                    .iter()
                    .position(|d| same_text(&c.fields[d.src].name, &name))
            });
        let d = d?;
        let mut sel: Vec<(usize, u32)> = Vec::new();
        for (f, item) in pairs {
            let fname = match f {
                Value::Text(s) => s.to_string(),
                _ => return None,
            };
            let fi = c.fields.iter().position(|x| same_text(&x.name, &fname))?;
            if sel.iter().any(|s| s.0 == fi)
                || !matches!(t.fields[fi].orient, Orient::Row | Orient::Column)
            {
                return None;
            }
            let ii = c.fields[fi]
                .items
                .iter()
                .position(|it| match (&it.value, item) {
                    (Value::Text(a), Value::Text(b)) => same_text(a, b),
                    (Value::Number(a), Value::Number(b)) => a == b,
                    (Value::Number(_), Value::Text(b)) => same_text(&it.label, b),
                    (Value::Text(a), Value::Number(n)) => same_text(
                        a,
                        &crate::coerce::to_text(&Value::Number(*n))
                            .map(|t| t.to_string())
                            .unwrap_or_default(),
                    ),
                    (Value::Bool(a), Value::Bool(b)) => a == b,
                    _ => false,
                });
            let ii = ii?;
            sel.push((fi, ii as u32));
        }
        let r = render(t, c);
        let origin = t.row0 - r.top;
        for vk in &r.value_keys {
            if vk.data != d || vk.keys.len() != sel.len() {
                continue;
            }
            if sel.iter().all(|s| vk.keys.contains(s)) {
                let abs_row = origin + vk.row;
                let abs_col = t.col0 + vk.col;
                return Some(CellAddr::new(t.sheet, abs_row, abs_col));
            }
        }
        None
    }
}

pub fn number_label(x: f64) -> String {
    crate::coerce::to_text(&Value::Number(x))
        .map(|t| t.to_string())
        .unwrap_or_default()
}

pub fn item_formula_names(formula: &str, mut f: impl FnMut(&str)) -> bool {
    let mut seen = true;
    let text = subst_idents(formula, |name| {
        f(name);
        Some(1.0)
    });
    if let Some(text) = text {
        let wb = Workbook::new();
        let at = CellAddr::new(wb.active_sheet(), 0, 0);
        seen = wb.compile_normalized(&text, at).is_ok();
    }
    seen && text_is_some(formula)
}

fn text_is_some(formula: &str) -> bool {
    subst_idents(formula, |_| Some(1.0)).is_some()
}
