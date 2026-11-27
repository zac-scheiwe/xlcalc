mod show;

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

    pub date: bool,
}

#[derive(Clone, Debug)]
pub struct CacheField {
    pub name: String,

    pub items: Vec<Item>,

    pub formula: Option<String>,

    pub format: Option<String>,
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

pub const BASE_PREVIOUS: u32 = u32::MAX - 3;
pub const BASE_NEXT: u32 = u32::MAX - 4;

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

    pub drag: u8,

    pub include_new: bool,

    pub page_break: bool,

    pub subtotal_name: String,

    pub calc_items: Vec<CalcItem>,

    pub group: Option<Group>,

    pub manual: Option<Vec<u32>>,

    pub caption: Option<String>,

    pub item_caps: Vec<(u32, String)>,

    pub group_order: Vec<String>,

    pub collapsed: Vec<u32>,

    pub show_all: bool,
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
    Hour,
    Minute,
    Second,
    Days(u32),
    Month,
    Quarter,
    Year,
}

#[derive(Clone, Debug)]
pub struct CalcItem {
    pub name: String,

    pub formula: String,

    pub seq: u64,
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
            drag: 0x1f,
            include_new: false,
            page_break: false,
            subtotal_name: String::new(),
            calc_items: Vec::new(),
            group: None,
            manual: None,
            caption: None,
            item_caps: Vec::new(),
            group_order: Vec::new(),
            collapsed: Vec::new(),
            show_all: false,
        }
    }
}

impl PivotTable {
    pub fn push_calc_item(&mut self, f: usize, name: String, formula: String) {
        let seq = self
            .fields
            .iter()
            .flat_map(|pf| pf.calc_items.iter().map(|c| c.seq + 1))
            .max()
            .unwrap_or(0);
        self.fields[f]
            .calc_items
            .push(CalcItem { name, formula, seq });
    }

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
                    DateUnit::Hour => "Hours",
                    DateUnit::Minute => "Minutes",
                    DateUnit::Second => "Seconds",
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

    pub item_selection: bool,

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

    pub(crate) sel_rows: Vec<SelLine>,
    pub(crate) sel_cols: Vec<SelLine>,
}

#[derive(Clone, Debug, Default)]
pub(crate) struct SelLine {
    pub at: u32,

    pub items: Vec<u32>,

    pub level: usize,

    pub same: usize,

    pub kind: u16,

    pub data: Option<usize>,
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

pub fn item_order(a: &Item, b: &Item) -> Ordering {
    let r = |i: &Item| match (&i.value, i.date) {
        (Value::Number(_), true) => 3.5,
        (v, _) => f64::from(rank(v)),
    };
    r(a).partial_cmp(&r(b))
        .unwrap_or(Ordering::Equal)
        .then_with(|| item_cmp(&a.value, &b.value))
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
    if v.abs() >= 1e307 {
        return format!("({}*16)", exact_literal(v / 16.0));
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

fn sx_snap(x: f64, acc: f64) -> f64 {
    let exp = |v: f64| ((v.to_bits() >> 52) & 0x7ff) as i32;
    if exp(x) - exp(acc) >= 50 {
        0.0
    } else {
        acc
    }
}

fn partition(mut arr: Vec<usize>, pass: impl Fn(usize) -> bool) -> Vec<usize> {
    let (mut i, mut j) = (0, arr.len());
    while i < j {
        if pass(arr[i]) {
            i += 1;
        } else {
            j -= 1;
            arr.swap(i, j);
        }
    }
    arr.truncate(i);
    arr
}

pub fn aggregate(func: Func, vals: &[&Value]) -> Value {
    if vals.iter().all(|v| matches!(v, Value::Blank)) {
        return Value::Blank;
    }
    let n = nums(vals);
    let count = n.len() as f64;

    if !matches!(func, Func::Count | Func::CountNums) {
        if let Some(Value::Error(e)) = vals.iter().find(|v| matches!(v, Value::Error(_))) {
            return Value::Error(*e);
        }
    }

    let fold =
        |step: fn(f64, f64) -> f64| n.iter().skip(1).fold(n[0], |a, &x| sx_snap(x, step(a, x)));
    let sum = || n.iter().fold(0.0, |a, &x| sx_snap(x, a + x));
    match func {
        Func::Sum => Value::Number(sum()),
        Func::Count => {
            Value::Number(vals.iter().filter(|v| !matches!(v, Value::Blank)).count() as f64)
        }
        Func::CountNums => Value::Number(count),
        Func::Average => {
            if let Some(Value::Error(e)) = vals.iter().find(|v| matches!(v, Value::Error(_))) {
                return Value::Error(*e);
            }
            if n.is_empty() {
                Value::Error(XlError::DIV0)
            } else {
                Value::Number(sum() / count)
            }
        }
        Func::Max | Func::Min | Func::Product if n.is_empty() => Value::Number(0.0),
        Func::Max => Value::Number(fold(f64::max)),
        Func::Min => Value::Number(fold(f64::min)),
        Func::Product => Value::Number(fold(|a, x| a * x)),
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

struct FormBuf {
    slots: Vec<(usize, FormSlot)>,
    err: Option<XlError>,
}

#[derive(Clone, Copy, Default)]
struct FormSlot {
    sum: f64,
    err: Option<XlError>,
    ovf: bool,
}

impl FormSlot {
    fn add(&mut self, v: &Value) {
        match v {
            Value::Number(x) => {
                self.sum = sx_snap(*x, self.sum + x);
                self.ovf |= !self.sum.is_finite();
            }
            Value::Error(e) => self.err = Some(*e),
            _ => {}
        }
    }

    fn merge(&mut self, o: &FormSlot) {
        if o.ovf {
            self.ovf = true;
        } else if let Some(e) = o.err {
            self.err = Some(e);
        } else {
            self.sum += o.sum;
            self.ovf |= !self.sum.is_finite();
        }
    }
}

#[derive(Clone, Debug)]
struct SxBuf {
    fresh: bool,

    nums: u32,

    all: u32,

    err: Option<XlError>,
    sum: f64,
    max: f64,
    min: f64,
    prod: f64,
    sq: f64,

    adj: f64,
}

impl SxBuf {
    const SUM: u32 = 1;
    const NUMS: u32 = 2;
    const MAX: u32 = 8;
    const MIN: u32 = 0x10;
    const PROD: u32 = 0x20;
    const SQ: u32 = 0x40;

    fn new() -> SxBuf {
        SxBuf {
            fresh: true,
            nums: 0,
            all: 0,
            err: None,
            sum: 0.0,
            max: 0.0,
            min: 0.0,
            prod: 0.0,
            sq: 0.0,
            adj: 0.0,
        }
    }

    fn mask(func: Func) -> u32 {
        match func {
            Func::Sum => SxBuf::SUM,
            Func::Count => 4,
            Func::Average => SxBuf::SUM | SxBuf::NUMS,
            Func::Max => SxBuf::MAX,
            Func::Min => SxBuf::MIN,
            Func::Product => SxBuf::PROD,
            Func::CountNums => SxBuf::NUMS,
            Func::StdDev | Func::StdDevP | Func::Var | Func::VarP => {
                SxBuf::SUM | SxBuf::NUMS | SxBuf::SQ
            }
        }
    }

    fn add(&mut self, v: &Value, mask: u32, gen: bool) {
        if matches!(v, Value::Blank) || self.err.is_some() {
            return;
        }
        let was_fresh = self.fresh;
        self.fresh = false;
        match v {
            Value::Error(e) if gen || mask & 0x79 != 0 => self.err = Some(*e),
            Value::Number(x) => {
                let x = *x;
                if gen && mask & (SxBuf::NUMS | 4) != 0 {
                    self.adj = self.adj + x - 1.0;
                }
                if mask & SxBuf::SUM != 0 {
                    self.sum = sx_snap(x, self.sum + x);
                }
                self.all += 1;
                if mask & SxBuf::NUMS != 0 {
                    self.nums += 1;
                }
                if mask & SxBuf::MAX != 0 {
                    self.max = sx_snap(x, if was_fresh { x } else { self.max.max(x) });
                }
                if mask & SxBuf::MIN != 0 {
                    self.min = sx_snap(x, if was_fresh { x } else { self.min.min(x) });
                }
                if mask & SxBuf::PROD != 0 {
                    self.prod = sx_snap(x, if was_fresh { x } else { self.prod * x });
                }
                if mask & SxBuf::SQ != 0 {
                    self.sq = sx_snap(x, crate::x87::add(crate::x87::mul(x, x), self.sq));
                }
            }
            _ => {
                self.all += 1;

                if was_fresh && mask & (SxBuf::MAX | SxBuf::MIN | SxBuf::PROD) != 0 {
                    self.fresh = true;
                }
            }
        }
    }

    fn merge(&mut self, o: &SxBuf, mask: u32) {
        if o.fresh || self.err.is_some() {
            return;
        }
        let was_fresh = self.fresh;
        self.fresh = false;
        if let Some(e) = o.err {
            self.err = Some(e);
            return;
        }
        if mask & SxBuf::SUM != 0 {
            self.sum += o.sum;
        }
        if mask & SxBuf::NUMS != 0 {
            self.nums += o.nums;
        }
        self.all += o.all;
        if mask & (SxBuf::NUMS | 4) != 0 {
            self.adj += o.adj;
        }
        if mask & SxBuf::MAX != 0 && (was_fresh || o.max > self.max) {
            self.max = o.max;
        }
        if mask & SxBuf::MIN != 0 && (was_fresh || o.min < self.min) {
            self.min = o.min;
        }
        if mask & SxBuf::PROD != 0 {
            self.prod = if was_fresh {
                o.prod
            } else {
                self.prod * o.prod
            };
        }
        if mask & SxBuf::SQ != 0 {
            self.sq = crate::x87::add(self.sq, o.sq);
        }
    }

    fn get(&self, func: Func) -> Value {
        if let Some(e) = self.err {
            return Value::Error(e);
        }
        if self.all == 0 {
            return Value::Blank;
        }
        match func {
            Func::Sum => Value::Number(self.sum),
            Func::Count => Value::Number(f64::from(self.all) + self.adj),
            Func::CountNums => Value::Number(f64::from(self.nums) + self.adj),
            Func::Average if self.nums == 0 => Value::Error(XlError::DIV0),
            Func::Average => Value::Number(self.sum / f64::from(self.nums)),
            Func::Max => Value::Number(self.max),
            Func::Min => Value::Number(self.min),
            Func::Product => Value::Number(self.prod),
            Func::StdDev | Func::Var | Func::StdDevP | Func::VarP => {
                let pop = matches!(func, Func::StdDevP | Func::VarP);
                if self.nums < if pop { 1 } else { 2 } {
                    return Value::Error(XlError::DIV0);
                }
                let var = pivot_var_sq(f64::from(self.nums), self.sum, self.sq, !pop);
                if matches!(func, Func::StdDev | Func::StdDevP) {
                    Value::Number(crate::x87::sqrt(var))
                } else {
                    Value::Number(var)
                }
            }
        }
    }
}

impl PivotCache {
    fn sort_items(&mut self) {
        for (f, field) in self.fields.iter_mut().enumerate() {
            let mut order: Vec<usize> = (0..field.items.len()).collect();
            order.sort_by(|&a, &b| item_order(&field.items[a], &field.items[b]));
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

    pub fn from_records(
        source: &str,
        sheet: SheetId,
        names: Vec<String>,
        rows: Vec<Vec<Value>>,
        label: impl Fn(&Value) -> String,
    ) -> PivotCache {
        let mut fields: Vec<CacheField> = names
            .into_iter()
            .map(|name| CacheField {
                name,
                items: Vec::new(),
                formula: None,
                format: None,
            })
            .collect();
        let mut maps: Vec<HashMap<Key, u32>> = fields.iter().map(|_| HashMap::new()).collect();
        let mut idx = Vec::with_capacity(rows.len());
        for rec in &rows {
            let ix = rec
                .iter()
                .enumerate()
                .map(|(i, v)| {
                    *maps[i].entry(key_of(v)).or_insert_with(|| {
                        fields[i].items.push(Item {
                            value: v.clone(),
                            label: label(v),
                            date: false,
                        });
                        (fields[i].items.len() - 1) as u32
                    })
                })
                .collect();
            idx.push(ix);
        }
        let mut c = PivotCache {
            source: source.to_string(),
            sheet,
            rect: Rect::new(
                0,
                0,
                rows.len() as u32,
                fields.len().saturating_sub(1) as u32,
            ),
            fields,
            records: rows,
            idx,
            orphaned: false,
            missing_limit: -1,
            refresh_open: false,
        };
        c.sort_items();
        c
    }

    pub fn read(wb: &Workbook, source: &str, sheet: SheetId, rect: Rect) -> Option<PivotCache> {
        if rect.r1 <= rect.r0 {
            return None;
        }
        let ncols = (rect.c1 - rect.c0 + 1) as usize;

        let mut names: Vec<Option<String>> = (rect.c0..=rect.c1)
            .map(|c| {
                let t = wb.display_text(CellAddr::new(sheet, rect.r0, c));
                (!t.is_empty()).then_some(t)
            })
            .collect();
        let mut taken: Vec<String> = Vec::with_capacity(ncols);
        let unique = |taken: &[String], base: &str| {
            let mut name = base.to_string();
            let mut n = 2;
            while taken.iter().any(|f| crate::text::eq_ci(f, &name)) {
                name = format!("{base}{n}");
                n += 1;
            }
            name
        };
        for slot in names.iter_mut().flatten() {
            *slot = unique(&taken, slot);
            taken.push(slot.clone());
        }
        let mut fields: Vec<CacheField> = Vec::with_capacity(ncols);
        for (i, name) in names.into_iter().enumerate() {
            let name = name.unwrap_or_else(|| {
                let n = unique(&taken, &format!("Field{}", i + 1));
                taken.push(n.clone());
                n
            });
            let column = rect.c0 + i as u32;
            let first = wb.number_format(CellAddr::new(sheet, rect.r0 + 1, column));
            let mixed = (rect.r0 + 2..=rect.r1)
                .any(|r| wb.number_format(CellAddr::new(sheet, r, column)) != first);
            let general = mixed || first.eq_ignore_ascii_case("general");
            let format = (!general).then(|| first.to_string());
            fields.push(CacheField {
                name,
                items: Vec::new(),
                formula: None,
                format,
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
                    let date = matches!(v, Value::Number(_))
                        && crate::NumberFormat::parse(wb.number_format(addr))
                            .is_ok_and(|n| n.is_date());
                    fields[i].items.push(Item {
                        value: v.clone(),
                        label,
                        date,
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
            item_selection: true,
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

    kind: u16,

    top: bool,
}

struct Ctx<'a> {
    t: &'a PivotTable,
    c: &'a PivotCache,

    live: Vec<usize>,

    live_exist: Vec<usize>,

    scratch: std::cell::OnceCell<Workbook>,

    hide: Hide,

    sorted: std::cell::OnceCell<Vec<usize>>,

    gens: std::cell::OnceCell<Vec<GenRec>>,
}

struct GenRec {
    idx: Vec<u32>,
    src: usize,
    val: Value,

    pass: bool,
}

enum GenState {
    Busy,
    Done(Value),
}

impl<'a> Ctx<'a> {
    fn new(
        t: &'a PivotTable,
        c: &'a PivotCache,
        hide: Hide,
        live: Vec<usize>,
        live_exist: Vec<usize>,
    ) -> Ctx<'a> {
        Ctx {
            t,
            c,
            live,
            live_exist,
            scratch: Default::default(),
            hide,
            sorted: Default::default(),
            gens: Default::default(),
        }
    }

    fn of(t: &'a PivotTable, c: &'a PivotCache) -> Ctx<'a> {
        let hide = effective_hidden(t, c);
        let live: Vec<usize> = pass_hidden(c, &hide.all)
            .into_iter()
            .filter(|&r| !hide.hides_record(c, r))
            .collect();
        let live_exist = pass_hidden(c, &hide.hard);
        Ctx::new(t, c, hide, live, live_exist)
    }
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

    fn calc(&self, field: usize, keys: &[(usize, u32)]) -> Value {
        let recs = self.ordered_recs(keys);
        let mut fields = Vec::new();
        self.form_fields(field, &mut fields, 0);
        let buf = self.form_buf(field, &fields, keys, &recs);
        if let Some(e) = buf.err {
            return Value::Error(e);
        }
        match self.form_eval(field, &buf, 0) {
            Value::Number(_) if recs.is_empty() && self.t.data.len() < 2 => Value::Blank,
            v => v,
        }
    }

    fn form_fields(&self, field: usize, out: &mut Vec<usize>, depth: u8) {
        let Some(formula) = &self.c.fields[field].formula else {
            return;
        };
        if depth > 8 {
            return;
        }
        map_idents(formula, |name| {
            let f = self
                .c
                .fields
                .iter()
                .position(|f| f.name.eq_ignore_ascii_case(name))?;
            if self.c.fields[f].formula.is_some() {
                self.form_fields(f, out, depth + 1);
            } else if !out.contains(&f) {
                out.push(f);
            }
            Some(String::new())
        });
    }

    fn form_eval(&self, field: usize, buf: &FormBuf, depth: u8) -> Value {
        let Some(formula) = &self.c.fields[field].formula else {
            return Value::Blank;
        };
        if depth > 8 {
            return Value::Error(XlError::REF);
        }
        let text = map_idents(formula, |name| {
            let f = self
                .c
                .fields
                .iter()
                .position(|f| f.name.eq_ignore_ascii_case(name))?;
            if self.c.fields[f].formula.is_some() {
                return Some(match self.form_eval(f, buf, depth + 1) {
                    Value::Number(x) => exact_literal(x),
                    Value::Error(e) => e.as_str().to_string(),
                    _ => XlError::VALUE.as_str().to_string(),
                });
            }
            let slot = buf
                .slots
                .iter()
                .find(|s| s.0 == f)
                .map_or_else(FormSlot::default, |s| s.1);
            Some(if slot.ovf {
                XlError::NUM.as_str().to_string()
            } else if let Some(e) = slot.err {
                e.as_str().to_string()
            } else {
                exact_literal(slot.sum)
            })
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

    fn form_buf(
        &self,
        field: usize,
        fields: &[usize],
        keys: &[(usize, u32)],
        recs: &[usize],
    ) -> FormBuf {
        let new = || FormBuf {
            slots: fields.iter().map(|&f| (f, FormSlot::default())).collect(),
            err: None,
        };
        let feed = |buf: &mut FormBuf, cell: &[usize], _mask: u32| {
            for &r in cell {
                for (f, slot) in &mut buf.slots {
                    slot.add(self.rv(r, *f));
                }
            }
        };
        if self.leaf_cell(keys) {
            let mut buf = new();
            feed(&mut buf, recs, 0);
            return buf;
        }
        let merge = |buf: &mut FormBuf, o: &FormBuf, _mask: u32| {
            if buf.err.is_some() {
                return;
            }
            let line = o
                .err
                .map_or_else(|| self.form_eval(field, o, 0), Value::Error);
            if let Value::Error(e) = line {
                buf.err = Some(e);
                return;
            }
            for (s, t) in buf.slots.iter_mut().zip(&o.slots) {
                s.1.merge(&t.1);
            }
        };
        self.accumulate(keys, recs, new, feed, merge)
    }

    fn agg(&self, d: usize, keys: &[(usize, u32)]) -> Value {
        let df = &self.t.data[d];
        if self.c.fields[df.src].formula.is_some() {
            return self.calc(df.src, keys);
        }
        self.agg_func(df.func, d, keys)
    }

    fn ix(&self, r: usize, f: usize) -> u32 {
        let n = self.c.records.len();
        if r < n {
            self.c.idx[r][f]
        } else {
            self.gens()[r - n].idx[f]
        }
    }

    fn rv(&self, r: usize, f: usize) -> &Value {
        let n = self.c.records.len();
        if r < n {
            &self.c.records[r][f]
        } else {
            let g = &self.gens()[r - n];
            if g.src == f {
                &g.val
            } else {
                &Value::Blank
            }
        }
    }

    fn is_gen(&self, r: usize) -> bool {
        r >= self.c.records.len()
    }

    fn agg_recs(&self, func: Func, src: usize, recs: &[usize]) -> Value {
        let vals: Vec<&Value> = recs.iter().map(|&r| self.rv(r, src)).collect();
        let base = aggregate(func, &vals);
        if !matches!(func, Func::Count | Func::CountNums) {
            return base;
        }
        let mut adj = 0.0;
        for &r in recs.iter().filter(|&&r| self.is_gen(r)) {
            match self.rv(r, src) {
                Value::Number(v) => adj = adj + v - 1.0,
                Value::Error(e) => return Value::Error(*e),
                _ => {}
            }
        }
        match base {
            Value::Number(n) if adj != 0.0 => Value::Number(n + adj),
            v => v,
        }
    }

    fn gens(&self) -> &[GenRec] {
        self.gens.get_or_init(|| self.generate())
    }

    fn generate(&self) -> Vec<GenRec> {
        let nf = self.c.fields.len().min(self.t.fields.len());
        let ncalc_of = |f: usize| self.t.fields[f].calc_items.len();
        let mut env: Vec<usize> = (0..nf)
            .filter(|&f| matches!(self.t.fields[f].orient, Orient::Row | Orient::Column))
            .collect();
        let ncalc = env.iter().filter(|&&f| ncalc_of(f) > 0).count();
        if ncalc == 0 {
            return Vec::new();
        }
        for i in 0..ncalc {
            if ncalc_of(env[i]) == 0 {
                if let Some(j) = (ncalc..env.len()).find(|&j| ncalc_of(env[j]) > 0) {
                    env.swap(i, j);
                }
            }
        }

        let mut marked = vec![false; nf];
        fn mark(c: &PivotCache, f: usize, marked: &mut [bool], depth: u8) {
            match &c.fields[f].formula {
                None => {
                    if let Some(m) = marked.get_mut(f) {
                        *m = true;
                    }
                }
                Some(formula) if depth < 8 => {
                    let _ = subst_fields(c, formula, |g| {
                        mark(c, g, marked, depth + 1);
                        Some(0.0)
                    });
                }
                Some(_) => {}
            }
        }
        for d in &self.t.data {
            if d.src < nf {
                mark(self.c, d.src, &mut marked, 0);
            }
        }
        let targets: Vec<(usize, Func)> = (0..nf)
            .filter(|&f| {
                marked[f]
                    && self.c.fields[f].formula.is_none()
                    && !matches!(
                        self.t.fields[f].orient,
                        Orient::Row | Orient::Column | Orient::Page
                    )
            })
            .map(|f| {
                let func = self
                    .t
                    .data
                    .iter()
                    .find(|d| d.src == f)
                    .map_or(Func::Sum, |d| d.func);
                (f, func)
            })
            .collect();

        let mut pass = vec![false; self.c.records.len()];
        for &r in &self.live {
            pass[r] = true;
        }
        let src_recs = partition((0..self.c.records.len()).collect(), |r| pass[r]);
        let mut combos: Vec<Vec<u32>> = Vec::new();
        let mut cur = vec![0u32; env.len()];
        self.combos(&env, 0, ncalc as i64, &mut cur, &mut combos);
        let mut base = vec![u32::MAX; self.c.fields.len()];
        for &f in &self.t.pages {
            if let Some(it) = self.t.fields.get(f).and_then(|pf| pf.page_item) {
                base[f] = it;
            }
        }
        let mut out: Vec<GenRec> = Vec::new();
        for &(src, func) in &targets {
            let mut memo: std::collections::HashMap<Vec<u32>, GenState> = Default::default();
            let mut order: Vec<Vec<u32>> = Vec::new();
            let g = GenEval {
                cx: self,
                env: &env,
                src,
                func,
                src_recs: &src_recs,
            };
            for combo in &combos {
                let _ = g.eval(combo, &mut memo, &mut order);
            }
            for combo in order {
                let Some(GenState::Done(val)) = memo.get(&combo) else {
                    continue;
                };
                if matches!(val, Value::Blank) {
                    continue;
                }
                let mut idx = base.clone();
                for (k, &f) in env.iter().enumerate() {
                    idx[f] = combo[k];
                }
                let pass =
                    idx.iter().enumerate().all(|(f, &it)| {
                        it == u32::MAX || !self.hide.all[f].contains(&(it as usize))
                    }) && !self.hide.hides_items(&idx);
                out.push(GenRec {
                    idx,
                    src,
                    val: val.clone(),
                    pass,
                });
            }
        }
        out
    }

    fn combos(
        &self,
        env: &[usize],
        level: usize,
        budget: i64,
        cur: &mut Vec<u32>,
        out: &mut Vec<Vec<u32>>,
    ) {
        if budget == 0 {
            return;
        }
        if level == env.len() {
            out.push(cur.clone());
            return;
        }
        let f = env[level];
        let nreal = self.c.fields[f].items.len();
        let n = nreal + self.t.fields[f].calc_items.len();
        for it in (0..n).rev() {
            cur[level] = it as u32;
            let b = if it >= nreal { -1 } else { budget - 1 };
            self.combos(env, level + 1, b, cur, out);
        }
    }
}

struct GenEval<'c, 'a> {
    cx: &'c Ctx<'a>,
    env: &'c [usize],
    src: usize,
    func: Func,
    src_recs: &'c [usize],
}

impl GenEval<'_, '_> {
    fn eval(
        &self,
        combo: &[u32],
        memo: &mut std::collections::HashMap<Vec<u32>, GenState>,
        order: &mut Vec<Vec<u32>>,
    ) -> Result<Value, ()> {
        match memo.get(combo) {
            Some(GenState::Done(v)) => return Ok(v.clone()),
            Some(GenState::Busy) => return Err(()),
            None => {}
        }
        let cx = self.cx;

        let Some((pos, ci)) = self
            .env
            .iter()
            .enumerate()
            .filter_map(|(k, &f)| {
                let n = cx.c.fields[f].items.len();
                let ci = cx.t.fields[f]
                    .calc_items
                    .get((combo[k] as usize).checked_sub(n)?)?;
                Some((k, ci))
            })
            .max_by_key(|(_, ci)| ci.seq)
        else {
            return Ok(Value::Blank);
        };
        memo.insert(combo.to_vec(), GenState::Busy);
        let val = if ci.formula.trim().is_empty() {
            Ok(Value::Blank)
        } else {
            self.formula(combo, pos, &ci.formula, memo, order)
        };
        let done = match &val {
            Ok(v) => v.clone(),
            Err(()) => Value::Error(XlError::REF),
        };
        memo.insert(combo.to_vec(), GenState::Done(done));
        order.push(combo.to_vec());
        val
    }

    fn formula(
        &self,
        combo: &[u32],
        pos: usize,
        formula: &str,
        memo: &mut std::collections::HashMap<Vec<u32>, GenState>,
        order: &mut Vec<Vec<u32>>,
    ) -> Result<Value, ()> {
        let cx = self.cx;
        let f = self.env[pos];
        let n = cx.c.fields[f].items.len();
        let mut cycle = false;
        let text = map_idents(formula, |name| {
            let target = cx.c.fields[f]
                .items
                .iter()
                .position(|x| x.label.eq_ignore_ascii_case(name))
                .or_else(|| {
                    cx.t.fields[f]
                        .calc_items
                        .iter()
                        .position(|x| x.name.eq_ignore_ascii_case(name))
                        .map(|j| n + j)
                });
            let Some(target) = target else {
                return Some(XlError::NAME.as_str().to_string());
            };
            let mut at = combo.to_vec();
            at[pos] = target as u32;
            let v = self.operand(&at, memo, order);
            Some(match v {
                Ok(Value::Number(x)) => exact_literal(x),
                Ok(Value::Error(e)) => e.as_str().to_string(),
                Ok(_) => exact_literal(0.0),
                Err(()) => {
                    cycle = true;
                    XlError::REF.as_str().to_string()
                }
            })
        });
        if cycle {
            return Err(());
        }
        let Some(text) = text else {
            return Ok(Value::Error(XlError::VALUE));
        };
        let wb = cx.scratch.get_or_init(Workbook::new);
        let at = CellAddr::new(wb.active_sheet(), 0, 0);
        Ok(match wb.cfv_eval(&text, at, at) {
            Value::Number(x) => Value::Number(x),
            Value::Error(e) => Value::Error(e),
            _ => Value::Error(XlError::VALUE),
        })
    }

    fn operand(
        &self,
        at: &[u32],
        memo: &mut std::collections::HashMap<Vec<u32>, GenState>,
        order: &mut Vec<Vec<u32>>,
    ) -> Result<Value, ()> {
        let cx = self.cx;
        let on_calc = self
            .env
            .iter()
            .enumerate()
            .any(|(k, &f)| at[k] as usize >= cx.c.fields[f].items.len());
        let v = if on_calc {
            let v = self.eval(at, memo, order)?;
            if matches!(v, Value::Blank) {
                Value::Blank
            } else {
                let r = [&v];
                let base = aggregate(self.func, &r);
                match (self.func, &v, base) {
                    (Func::Count | Func::CountNums, Value::Number(x), _) => Value::Number(*x),
                    (Func::Count | Func::CountNums, Value::Error(e), _) => Value::Error(*e),
                    (_, _, b) => b,
                }
            }
        } else {
            let recs: Vec<&Value> = self
                .src_recs
                .iter()
                .filter(|&&r| {
                    self.env
                        .iter()
                        .enumerate()
                        .all(|(k, &f)| cx.c.idx[r][f] == at[k])
                })
                .map(|&r| &cx.c.records[r][self.src])
                .collect();
            aggregate(self.func, &recs)
        };
        Ok(v)
    }
}

impl Ctx<'_> {
    fn detail_recs(&self, keys: &[(usize, u32)]) -> Vec<usize> {
        let mut live = vec![false; self.c.records.len()];
        for &r in &self.live {
            live[r] = true;
        }
        let hit = |r: usize| live[r] && keys.iter().all(|&(f, i)| self.c.idx[r][f] == i);
        let mut arr = partition((0..self.c.records.len()).collect(), hit);
        let (rows, cols) = self.axis_fields();
        let open = |fs: &[usize]| -> Vec<usize> {
            fs.iter()
                .rev()
                .copied()
                .filter(|f| !keys.iter().any(|k| k.0 == *f))
                .collect()
        };
        let order: Vec<usize> = open(&rows).into_iter().chain(open(&cols)).collect();
        let rank = self.ranks(&order);
        arr.sort_by_cached_key(|&r| {
            order
                .iter()
                .zip(&rank)
                .map(|(&f, rk)| {
                    rk.get(self.c.idx[r][f] as usize)
                        .copied()
                        .unwrap_or(u32::MAX)
                })
                .collect::<Vec<u32>>()
        });
        arr
    }

    fn ranks(&self, order: &[usize]) -> Vec<Vec<u32>> {
        order
            .iter()
            .map(|&f| {
                let n = self.c.fields[f].items.len() + self.t.fields[f].calc_items.len();
                let mut rank = vec![u32::MAX; n + 1];
                let mut order = display_order(self.t, self.c, f);
                if matches!(self.t.fields[f].sort, Sort::Name(_)) {
                    let shown = self.children(f, &[]);
                    order.retain(|i| !shown.contains(i));
                    order.splice(0..0, shown);
                }
                for (pos, i) in order.into_iter().enumerate() {
                    if let Some(x) = rank.get_mut(i as usize) {
                        *x = pos as u32;
                    }
                }
                rank
            })
            .collect()
    }

    fn axis_fields(&self) -> (Vec<usize>, Vec<usize>) {
        let (rax, cax) = self.t.axes();
        let fields = |ax: &[Ax]| -> Vec<usize> {
            ax.iter()
                .filter_map(|a| match a {
                    Ax::Field(f) => Some(*f),
                    Ax::Values => None,
                })
                .collect()
        };
        (fields(&rax), fields(&cax))
    }

    fn line_key(&self, r: usize, fields: &[usize], ax: &[Ax]) -> Vec<u32> {
        let mut k = Vec::with_capacity(fields.len());
        for &f in fields {
            let it = self.ix(r, f);
            k.push(it);
            if self.collapses(f, it, ax) {
                break;
            }
        }
        k
    }

    fn collapses(&self, f: usize, it: u32, ax: &[Ax]) -> bool {
        self.t.fields[f].collapsed.contains(&it)
            && ax
                .iter()
                .position(|a| *a == Ax::Field(f))
                .is_some_and(|p| !ax[p + 1..].contains(&Ax::Values))
    }

    fn sorted_recs(&self) -> &[usize] {
        self.sorted.get_or_init(|| {
            let mut pass = vec![false; self.c.records.len()];
            for &r in &self.live {
                pass[r] = true;
            }
            let n = self.c.records.len();
            let mut arr = partition((0..n).collect(), |r| pass[r]);

            let gens = self.gens();
            arr.extend(n..n + gens.len());
            let mut arr = partition(arr, |r| r < n || gens[r - n].pass);
            let (rax, cax) = self.t.axes();
            let (rows, cols) = self.axis_fields();
            let order: Vec<usize> = rows.iter().chain(cols.iter()).copied().collect();
            let rank = self.ranks(&order);
            let pos = |f: usize| order.iter().position(|&g| g == f).unwrap_or(0);
            arr.sort_by_cached_key(|&r| {
                let mut k: Vec<u32> = Vec::with_capacity(order.len());
                for (fs, ax) in [(&rows, &rax), (&cols, &cax)] {
                    let line = self.line_key(r, fs, ax);
                    for (n, &f) in fs.iter().enumerate() {
                        k.push(match line.get(n) {
                            Some(&it) => rank[pos(f)].get(it as usize).copied().unwrap_or(u32::MAX),
                            None => 0,
                        });
                    }
                }
                k
            });
            arr
        })
    }

    fn ordered_recs(&self, keys: &[(usize, u32)]) -> Vec<usize> {
        self.sorted_recs()
            .iter()
            .copied()
            .filter(|&r| keys.iter().all(|&(f, i)| self.ix(r, f) == i))
            .collect()
    }

    fn entry_mask(&self, fields: &[usize], other: &[usize], keys: &[(usize, u32)]) -> u32 {
        let custom = |f: usize| {
            self.t.fields[f]
                .custom
                .iter()
                .fold(0, |m, &ix| m | SxBuf::mask(sub_func(ix).0))
        };
        let level = fields.iter().rposition(|f| keys.iter().any(|k| k.0 == *f));
        match level.map(|l| fields[l]) {
            Some(f) if !self.t.fields[f].custom.is_empty() => custom(f),
            _ => {
                self.t.data.iter().fold(0, |m, d| m | SxBuf::mask(d.func))
                    | other.iter().fold(0, |m, &f| m | custom(f))
            }
        }
    }

    fn cells_feed_totals(&self) -> bool {
        let (rows, cols) = self.axis_fields();
        let custom_ok = |fs: &[usize]| {
            fs.iter().enumerate().all(|(n, &f)| {
                let custom = &self.t.fields[f].custom;
                if n + 1 == fs.len() {
                    custom.is_empty()
                } else {
                    custom.iter().all(|&ix| matches!(sub_func(ix).0, Func::Sum))
                }
            })
        };
        self.t.data.iter().all(|d| matches!(d.func, Func::Sum))
            && custom_ok(&rows)
            && custom_ok(&cols)
    }

    fn agg_func(&self, func: Func, d: usize, keys: &[(usize, u32)]) -> Value {
        let src = self.t.data[d].src;
        let dfunc = self.t.data[d].func;
        let recs = self.ordered_recs(keys);
        if self.leaf_cell(keys) {
            return self.agg_recs(func, src, &recs);
        }
        let cells_feed = self.cells_feed_totals();

        let feed = |buf: &mut SxBuf, cell: &[usize], mask: u32| {
            if cells_feed {
                buf.add(&self.agg_recs(dfunc, src, cell), mask, false);
                return;
            }
            for &r in cell {
                let v = self.rv(r, src);
                let gen = self.is_gen(r);
                buf.add(v, mask, gen);
                if matches!(v, Value::Error(_))
                    && (gen || !matches!(dfunc, Func::Count | Func::CountNums))
                {
                    break;
                }
            }
        };
        self.accumulate(keys, &recs, SxBuf::new, feed, SxBuf::merge)
            .get(func)
    }

    fn leaf_cell(&self, keys: &[(usize, u32)]) -> bool {
        let (rax, cax) = self.t.axes();
        let (rows, cols) = self.axis_fields();
        self.is_leaf(keys, &rows, &rax) && self.is_leaf(keys, &cols, &cax)
    }

    fn is_leaf(&self, keys: &[(usize, u32)], fs: &[usize], ax: &[Ax]) -> bool {
        fs.iter().all(|f| keys.iter().any(|k| k.0 == *f))
            || fs.iter().rev().find_map(|f| {
                keys.iter()
                    .find(|k| k.0 == *f)
                    .map(|k| self.collapses(*f, k.1, ax))
            }) == Some(true)
    }

    fn accumulate<B>(
        &self,
        keys: &[(usize, u32)],
        recs: &[usize],
        new: impl Fn() -> B,
        feed: impl Fn(&mut B, &[usize], u32),
        merge: impl Fn(&mut B, &B, u32),
    ) -> B {
        let (rax, cax) = self.t.axes();
        let (rows, cols) = self.axis_fields();
        let (r_leaf, c_leaf) = (
            self.is_leaf(keys, &rows, &rax),
            self.is_leaf(keys, &cols, &cax),
        );

        let mut lines: Vec<Vec<&[usize]>> = Vec::new();
        let mut i = 0;
        while i < recs.len() {
            let rk = self.line_key(recs[i], &rows, &rax);
            let mut cells: Vec<&[usize]> = Vec::new();
            while i < recs.len() && self.line_key(recs[i], &rows, &rax) == rk {
                let ck = self.line_key(recs[i], &cols, &cax);
                let start = i;
                while i < recs.len()
                    && self.line_key(recs[i], &rows, &rax) == rk
                    && self.line_key(recs[i], &cols, &cax) == ck
                {
                    i += 1;
                }
                cells.push(&recs[start..i]);
            }
            lines.push(cells);
        }
        let row_mask = self.entry_mask(&rows, &cols, keys);
        let col_mask = self.entry_mask(&cols, &rows, keys);
        let mut buf = new();
        if r_leaf || c_leaf {
            let mask = if r_leaf { col_mask } else { row_mask };
            for cell in lines.iter().flatten() {
                feed(&mut buf, cell, mask);
            }
        } else {
            for line in &lines {
                let mut sub = new();
                for cell in line {
                    feed(&mut sub, cell, col_mask);
                }
                merge(&mut buf, &sub, row_mask);
            }
        }
        buf
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

        let other = match self.t.fields[field].orient {
            Orient::Row => Some(Orient::Column),
            Orient::Column => Some(Orient::Row),
            _ => None,
        };
        let calc_beside = self.t.fields.iter().enumerate().any(|(g, pf)| {
            g != field
                && Some(pf.orient) == other
                && (0..pf.calc_items.len())
                    .any(|j| !self.hide.all[g].contains(&(self.c.fields[g].items.len() + j)))
        });
        let under_calc = keys
            .iter()
            .any(|&(f, i)| i as usize >= self.c.fields[f].items.len());
        if under_calc || calc_beside || self.t.fields[field].show_all {
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

        let sorted = matches!(self.t.fields[field].sort, Sort::Data(..) | Sort::Name(_));
        if placed || sorted {
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
                let cap = |i: u32| {
                    if self.t.fields[field].item_caps.is_empty() && (i as usize) < nreal {
                        self.c.fields[field].items[i as usize].clone()
                    } else {
                        let text = self.label(field, i).1;
                        Item {
                            value: Value::text(text.as_str()),
                            label: text,
                            date: false,
                        }
                    }
                };
                seen.sort_by(|a, b| item_order(&cap(*a), &cap(*b)));
                if desc {
                    seen.reverse();
                }
            }
            Sort::Data(desc, d) => {
                let mut v: Vec<(u32, Value)> = seen
                    .iter()
                    .map(|&it| {
                        let mut ks = keys.to_vec();
                        ks.push((field, it));
                        (it, self.agg(d, &ks))
                    })
                    .collect();
                let hi = v.len() as isize - 1;
                qsort_item_group(&mut v, 0, hi, !desc);
                seen = v.into_iter().map(|x| x.0).collect();
            }
        }
        if !placed && !sorted {
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
            Value::Number(_) => (it.value.clone(), it.label.clone()),
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
                kind: show::DATA,
                top: false,
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
                        kind: u16::from(ix),
                        top: false,
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
            if last
                || (matches!((item, ax[level]), (Some(i), Ax::Field(f))
                    if self.t.fields[f].collapsed.contains(&i))
                    && !ax[level + 1..].contains(&Ax::Values))
            {
                st.out.push(Line {
                    labels: st.cur.clone(),
                    keys: st.keys.clone(),
                    data: nodata,
                    values: true,
                    func: None,
                    depth: (level + 1) as u8,
                    kind: show::DATA,
                    top: false,
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

                let deeper = ax[level + 1..].contains(&Ax::Values)
                    && !(next_values && level + 2 == ax.len());

                let header_ix =
                    (st.is_row && !tabular && !sub_on && !next_values && !deeper && sub_top)
                        .then(|| match ax[level] {
                            Ax::Field(f) if self.t.fields[f].custom.len() == 1 => {
                                Some(self.t.fields[f].custom[0])
                            }
                            _ => None,
                        })
                        .flatten();
                let header_fn = header_ix.map(|ix| sub_func(ix).0);
                let top = st.is_row && !tabular;
                if top {
                    let values =
                        ((sub_on && sub_top) || header_fn.is_some()) && !next_values && !deeper;

                    let hidden_total = !values
                        && sub_top
                        && !next_values
                        && !deeper
                        && matches!(ax[level], Ax::Field(f) if !self.t.fields[f].custom.is_empty());
                    st.out.push(Line {
                        labels: st.cur.clone(),
                        keys: st.keys.clone(),
                        data: nodata,
                        values,
                        func: header_fn,
                        depth: (level + 1) as u8,
                        kind: match header_ix {
                            Some(ix) => u16::from(ix),
                            None if values || hidden_total => show::AUTO_SUB,
                            None => show::NON_VALUE,
                        },
                        top: true,
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
                                kind: u16::from(ix),
                                top: false,
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
                            kind: show::AUTO_SUB,
                            top: false,
                        });
                    }
                } else if !(top && sub_top)
                    && sub_on
                    && !next_values
                    && !matches!(ax[level], Ax::Values)
                {
                    let mut sl = vec![None; ax.len()];
                    for k in 0..level {
                        if self.repeats(ax[k], st.is_row) {
                            sl[k] = st.cur[k].clone();
                        }
                    }
                    sl[level] = Some(Value::text(format!("{text} Total")));
                    st.out.push(Line {
                        labels: sl,
                        keys: st.keys.clone(),
                        data: nodata,
                        values: !next_values,
                        func: None,
                        depth: (level + 1) as u8,
                        kind: show::AUTO_SUB,
                        top: false,
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
                        kind: show::DATA,
                        top: false,
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

pub const DEFAULT_BASE: usize = 0;

pub fn default_group_step(start: f64, end: f64) -> f64 {
    let n = ((end - start) / 3.0).log10();
    if !n.is_finite() || n < -2147483648.0 {
        return 1.0;
    }
    let p = 10f64.powi(n.floor() as i32);
    if p.is_finite() && p != 0.0 {
        p
    } else {
        1.0
    }
}

fn qsort_item_group(v: &mut [(u32, Value)], mut lo: isize, mut hi: isize, asc: bool) {
    let is_err = |x: &Value| matches!(x, Value::Error(_));
    while lo < hi && lo >= 0 && hi >= 0 {
        v.swap(lo as usize, ((lo + hi) / 2) as usize);
        let mut last = lo;
        for i in lo + 1..=hi {
            let (p, e) = (&v[lo as usize].1, &v[i as usize].1);
            let before = if is_err(p) == is_err(e) {
                asc == (compare_pivot_values(p, e) > 0)
            } else {
                is_err(p)
            };
            if before {
                last += 1;
                v.swap(last as usize, i as usize);
            }
        }
        v.swap(lo as usize, last as usize);
        if hi - last < last - lo {
            qsort_item_group(v, last + 1, hi, asc);
            hi = last - 1;
        } else {
            qsort_item_group(v, lo, last - 1, asc);
            lo = last + 1;
        }
    }
}

fn compare_pivot_values(a: &Value, b: &Value) -> i32 {
    let ty = |x: &Value| -> i32 {
        match x {
            Value::Number(_) => 1,
            Value::Text(_) => 2,
            Value::Bool(_) => 4,
            Value::Error(_) => 0x10,
            _ => 0x100,
        }
    };
    match (a, b) {
        (Value::Number(x), Value::Number(y)) => {
            let d = (x - y).to_bits();
            let top = d & 0xffff_0000_0000_0000;
            if top == 0 || d & 0x7ff0_0000_0000_0000 == 0x7ff0_0000_0000_0000 {
                0
            } else if d >> 63 == 0 {
                1
            } else {
                -1
            }
        }
        _ => (ty(a) - ty(b)).signum(),
    }
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

fn grid_lines<'l>(lines: &'l [Line], hidden: &'l [Line]) -> Vec<(Option<usize>, &'l Line)> {
    let mut out = Vec::new();
    let mut pending: Vec<(usize, &Line)> = Vec::new();
    for (i, l) in lines.iter().enumerate() {
        while pending.last().is_some_and(|p| l.depth <= p.1.depth) {
            let (pi, pl) = pending.pop().expect("pending line");
            out.push(((pi != usize::MAX).then_some(pi), pl));
        }
        if l.top && l.values {
            pending.push((i, l));
        } else if l.top && l.kind == show::AUTO_SUB {
            pending.push((usize::MAX, l));
        } else if l.values {
            out.push((Some(i), l));
        }
    }
    while let Some((pi, pl)) = pending.pop() {
        out.push(((pi != usize::MAX).then_some(pi), pl));
    }
    out.extend(hidden.iter().map(|l| (None, l)));
    out
}

fn sxli_of(l: &Line, ax: &[Ax]) -> show::Sxli {
    if l.kind == show::GRAND {
        return show::Sxli {
            items: vec![show::NO_ITEM],
            kind: l.kind,
            data: l.data.unwrap_or(0),
            level: 0,
        };
    }
    let mut items = ax
        .iter()
        .take(l.depth as usize)
        .map(|a| match a {
            Ax::Field(f) => l
                .keys
                .iter()
                .find(|k| k.0 == *f)
                .map_or(show::NO_ITEM, |k| k.1),
            Ax::Values => l.data.map_or(show::NO_ITEM, |d| d as u32),
        })
        .collect::<Vec<u32>>();

    if !l.top && (2..=12).contains(&l.kind) {
        items.push(show::NO_ITEM);
    }
    show::Sxli {
        items,
        kind: l.kind,
        data: l.data.unwrap_or(0),
        level: l.depth as usize,
    }
}

fn show_spec(cx: &Ctx, rax: &[Ax], cax: &[Ax], d: usize) -> show::Spec {
    let sh = &cx.t.data[d].show;
    let kind = if (2..=15).contains(&sh.kind) {
        sh.kind - 1
    } else {
        0
    };
    let bf = sh.base_field.unwrap_or(DEFAULT_BASE);
    let (mut axis, dim) = match (
        rax.iter().position(|a| *a == Ax::Field(bf)),
        cax.iter().position(|a| *a == Ax::Field(bf)),
    ) {
        (Some(p), _) => (1, p),
        (_, Some(p)) => (2, p),
        _ => (0, 0),
    };
    let base = match sh.base_item {
        Some(BASE_PREVIOUS) => show::BaseItem::Prev,
        Some(BASE_NEXT) => show::BaseItem::Next,
        Some(i) => show::BaseItem::Item(i),

        None => show::BaseItem::Item(
            (0..cx.c.records.len())
                .map(|r| cx.c.idx[r][bf])
                .min()
                .unwrap_or(0),
        ),
    };

    if let show::BaseItem::Item(i) = base {
        if matches!(kind, 1..=3) && axis != 0 && cx.t.fields[bf].hidden.contains(&(i as usize)) {
            axis = 0;
        }
    }
    show::Spec {
        kind,
        axis,
        dim,
        base,
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
    let cx = Ctx::of(t, c);
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
    let grand_lines = |n: usize, vals: bool| -> Vec<Line> {
        let line = |data: Option<usize>, text: String| {
            let mut labels = vec![None; n];
            labels[0] = Some(Value::text(text));
            Line {
                labels,
                keys: vec![],
                data,
                values: true,
                func: None,
                depth: 0,
                kind: show::GRAND,
                top: false,
            }
        };
        if vals {
            (0..nd)
                .map(|d| line(Some(d), format!("Total {}", t.data[d].caption)))
                .collect()
        } else {
            vec![line(None, "Grand Total".into())]
        }
    };

    let mut hidden_rgrand = Vec::new();
    let mut hidden_cgrand = Vec::new();
    if t.col_grand && (!rax.is_empty() || nd > 0) && (row_vals || !rax.is_empty()) {
        rlines.extend(grand_lines(rax.len(), row_vals));
    } else if rax.iter().any(|a| matches!(a, Ax::Field(_))) {
        hidden_rgrand = grand_lines(rax.len(), row_vals);
    }
    let real_col_field = cax.iter().any(|a| matches!(a, Ax::Field(_)));
    if t.row_grand && real_col_field {
        clines.extend(grand_lines(cax.len(), col_vals));
    } else if real_col_field {
        hidden_cgrand = grand_lines(cax.len(), col_vals);
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
                let compact_col = compact
                    || rax
                        .iter()
                        .any(|a| matches!(a, Ax::Field(f) if t.eff_form(*f) == Form::Compact));
                let name = if j == 0 && (compact_col || t.row_caption.is_some()) {
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
    if virtual_col && rax.contains(&Ax::Values) {
    } else if virtual_col {
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

    if nd > 0 && !rlines.is_empty() && !clines.is_empty() {
        let grows = grid_lines(&rlines, &hidden_rgrand);
        let gcols = grid_lines(&clines, &hidden_cgrand);
        let raw: Vec<Vec<Value>> = grows
            .iter()
            .map(|(_, rl)| {
                gcols
                    .iter()
                    .map(|(_, cl)| {
                        let d = rl.data.or(cl.data).or((nd == 1).then_some(0));
                        let Some(d) = d else { return Value::Blank };
                        let mut keys = rl.keys.clone();
                        keys.extend(cl.keys.iter().copied());
                        match rl.func.or(cl.func) {
                            Some(fo) => cx.agg_func(fo, d, &keys),
                            None => cx.agg(d, &keys),
                        }
                    })
                    .collect()
            })
            .collect();
        let sxli = |ls: &[(Option<usize>, &Line)], ax: &[Ax]| -> Vec<show::Sxli> {
            ls.iter().map(|(_, l)| sxli_of(l, ax)).collect()
        };
        let mut grid = show::Grid {
            rows: sxli(&grows, &rax),
            cols: sxli(&gcols, &cax),
            row_values: rax.iter().map(|a| *a == Ax::Values).collect(),
            col_values: cax.iter().map(|a| *a == Ax::Values).collect(),
            cells: raw
                .iter()
                .map(|r| r.iter().map(show::XNum::from_value).collect())
                .collect(),
            nd,
            data_axis: if row_vals {
                1
            } else if col_vals {
                2
            } else {
                0
            },
        };
        let sel = |ls: &[(Option<usize>, &Line)], ax: &[Ax], base: u32| -> Vec<SelLine> {
            let mut shown: Vec<(usize, &Line)> =
                ls.iter().filter_map(|(i, l)| Some(((*i)?, *l))).collect();
            shown.sort_by_key(|x| x.0);
            let mut prev: Vec<u32> = Vec::new();
            shown
                .into_iter()
                .map(|(i, l)| {
                    let s = sxli_of(l, ax);
                    let shared = s.items.iter().zip(&prev).take_while(|(a, b)| a == b);
                    let same = shared.count().min(s.level.saturating_sub(1));
                    if s.level > 0 {
                        prev.clone_from(&s.items);
                    }
                    SelLine {
                        at: base + i as u32,
                        items: s.items,
                        level: s.level,
                        same,

                        kind: match (l.values, l.top) {
                            (false, _) => show::NON_VALUE,
                            (true, true) => show::DATA,
                            (true, false) => s.kind,
                        },
                        data: l.data,
                    }
                })
                .collect()
        };
        out.sel_rows = sel(&grows, &rax, top + hdr_rows);
        out.sel_cols = sel(&gcols, &cax, left_cols);
        let specs: Vec<show::Spec> = (0..nd).map(|d| show_spec(&cx, &rax, &cax, d)).collect();
        for (d, spec) in specs.iter().enumerate() {
            grid.apply(d, spec);
        }
        for (gi, (ri, rl)) in grows.iter().enumerate() {
            let Some(i) = *ri else { continue };
            for (gj, (cj, cl)) in gcols.iter().enumerate() {
                let Some(j) = *cj else { continue };
                let d = rl.data.or(cl.data).or((nd == 1).then_some(0));
                let Some(d) = d else { continue };
                let k = specs[d].kind;
                let mut v = grid.cells[gi][gj].to_value();

                let shown = k > 0
                    && match raw[gi][gj] {
                        Value::Number(_) => true,
                        Value::Blank => !matches!(k, 2 | 3 | 0xd | 0xe),
                        _ => false,
                    };
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
                    keys.extend(cl.keys.iter().copied());
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
        if let Some(name) = self.pivots.tables.get(ti).map(|t| t.name.clone()) {
            self.slicer_reload_pivot(&name);
        }
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
                    wb.set_number_format_copy(sheet, at, fmt);
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
                        wb.set_number_format_copy(sheet, body, &fmt);
                    }
                }
            }
            if t.data.len() > 1 {
                for (r, c, d) in &value_cells {
                    let fmt = &wb_fmts[*d];
                    if fmt != "General" {
                        wb.set_number_format_copy(
                            sheet,
                            Rect::cell(origin_row + r, t.col0 + c),
                            fmt,
                        );
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

fn secs_of_day(v: f64) -> u32 {
    ((v - v.floor()) * 86400.0).round() as u32 % 86400
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
        let mk = |label: String, value: Value| Item {
            value,
            label,
            date: false,
        };
        let text = |s: String| mk(s.clone(), Value::text(s.as_str()));
        let mut items: Vec<Item> = vec![text(format!("<{}", date_text(lo)))];
        const MONTHS: [&str; 12] = [
            "Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sept", "Oct", "Nov", "Dec",
        ];

        let bin: Box<dyn Fn(f64) -> u32> = match unit {
            DateUnit::Hour => {
                for h in 0..24 {
                    let (n, ap) = (if h % 12 == 0 { 12 } else { h % 12 }, ["am", "pm"][h / 12]);
                    items.push(text(format!("{n} {ap}")));
                }
                Box::new(move |v| secs_of_day(v) / 3600 + 1)
            }
            DateUnit::Minute => {
                for m in 0..60 {
                    items.push(text(format!(":{m:02}")));
                }
                Box::new(move |v| secs_of_day(v) / 60 % 60 + 1)
            }
            DateUnit::Second => {
                for m in 0..60 {
                    items.push(text(format!(":{m:02}")));
                }
                Box::new(move |v| secs_of_day(v) % 60 + 1)
            }
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
            DateUnit::Days(n) if levels || n == 1 => {
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
                Value::Number(raw) => {
                    let v = raw.floor();
                    if v < lo {
                        0
                    } else if v > hi {
                        last
                    } else {
                        bin(*raw)
                    }
                }

                Value::Blank => 0,
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
                            date: false,
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

            let whole = by.fract() == 0.0
                && start.fract() == 0.0
                && end.fract() == 0.0
                && data.iter().all(|v| v.fract() == 0.0);
            let nb = (((end - start) / by).ceil() as i64).max(1) as usize;
            let lo = |k: usize| k as f64 * by + start;

            let hi = |k: usize| {
                let top = lo(k) + by;
                if whole && top != end {
                    top - 1.0
                } else {
                    top
                }
            };
            let mut items: Vec<Item> = Vec::new();
            let mk = |label: String| Item {
                value: Value::text(label.as_str()),
                label,
                date: false,
            };
            items.push(mk(format!("<{}", number_label(start))));
            for k in 0..nb {
                items.push(mk(format!(
                    "{}-{}",
                    number_label(lo(k)),
                    number_label(hi(k))
                )));
            }

            items.push(mk(format!(">{}", number_label(nb as f64 * by + start))));

            let top = hi(nb - 1).max(end);

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
            format: None,
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

        if !items.iter().any(|i| matches!(i.value, Value::Number(_)))
            || !items
                .iter()
                .all(|i| matches!(i.value, Value::Number(_) | Value::Blank))
        {
            return false;
        }
        let at = CellAddr::new(c.sheet, c.rect.r0 + 1, c.rect.c0 + f as u32);
        self.cellfmt
            .parse_stored(self.number_format(at))
            .is_ok_and(|n| n.is_date())
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
        self.hides_items(&c.idx[r])
    }

    fn hides_items(&self, idx: &[u32]) -> bool {
        self.parent.iter().any(|p| {
            p.items.contains(&(idx[p.field] as usize)) && p.keys.iter().all(|&(f, i)| idx[f] == i)
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
        let probe = Ctx::new(t, c, hide.clone(), base.clone(), base.clone());
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
    let cx = Ctx::of(t, c);
    let mut v = cx.children(f, &[]);
    let n = c.fields[f].items.len() as u32;
    v.retain(|&i| i < n);
    let rest: Vec<u32> = (0..n).filter(|i| !v.contains(i)).collect();
    v.extend(rest);
    v
}

pub fn display_order(t: &PivotTable, c: &PivotCache, f: usize) -> Vec<u32> {
    let mut v: Vec<u32> = (0..c.fields[f].items.len() as u32).collect();
    let n = c.fields[f].items.len();
    if let Sort::Name(desc) = t.fields[f].sort {
        let key = |i: u32| match t.fields[f].item_caps.iter().find(|x| x.0 == i) {
            Some((_, cap)) => Item {
                value: Value::text(cap.as_str()),
                label: cap.clone(),
                date: false,
            },
            None => c.fields[f].items[i as usize].clone(),
        };
        v.sort_by(|a, b| item_order(&key(*a), &key(*b)));
        if desc {
            v.reverse();
        }
    }
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
    pub fn pivot_chart_series(&self, ti: usize) -> usize {
        let t = &self.pivots.tables[ti];
        let Some(c) = self.pivots.caches.get(t.cache) else {
            return 0;
        };
        let r = render(t, c);
        let need: Vec<usize> = t
            .cols
            .iter()
            .filter_map(|a| match a {
                Ax::Field(f) => Some(*f),
                _ => None,
            })
            .collect();
        let mut cols: Vec<u32> = r
            .value_keys
            .iter()
            .filter(|v| need.iter().all(|f| v.keys.iter().any(|k| k.0 == *f)))
            .map(|v| v.col)
            .collect();
        cols.sort_unstable();
        cols.dedup();
        cols.len()
    }

    pub fn pivot_label_above(&self, addr: CellAddr) -> Option<(usize, usize, Option<u32>)> {
        let at = Rect::cell(addr.row, addr.col);
        let ti = self.pivot_meeting(addr.sheet, &at)?;
        let t = &self.pivots.tables[ti];
        let c = self.pivots.caches.get(t.cache)?;
        let r = render(t, c);
        let origin = t.row0.checked_sub(r.top)?;
        r.label_cells
            .iter()
            .filter(|l| t.col0 + l.1 == addr.col && origin + l.0 < addr.row)
            .max_by_key(|l| l.0)
            .map(|l| (ti, l.2, l.3))
    }
}
impl Workbook {
    pub fn pivot_header_field(&self, addr: CellAddr) -> Option<(usize, usize)> {
        let at = Rect::cell(addr.row, addr.col);
        let ti = self.pivot_meeting(addr.sheet, &at)?;
        let t = &self.pivots.tables[ti];
        let c = self.pivots.caches.get(t.cache)?;
        let r = render(t, c);
        let origin = t.row0.checked_sub(r.top)?;
        let (hr, hc) = (addr.row.checked_sub(origin)?, addr.col.checked_sub(t.col0)?);
        if hr < r.top || hr >= r.top + r.hdr_rows {
            return None;
        }
        r.label_cells
            .iter()
            .filter(|l| l.1 == hc && l.0 > hr)
            .min_by_key(|l| l.0)
            .map(|l| (ti, l.2))
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
        let cx = Ctx::of(t, c);
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

struct SelFilt {
    axis: usize,
    pos: usize,
    field: Option<usize>,
    items: Vec<u32>,
    mask: u32,
}

fn sel_type(l: &SelLine, pos: usize) -> u16 {
    if l.kind == show::NON_VALUE || l.kind == show::GRAND || pos + 1 >= l.level {
        l.kind
    } else {
        show::DATA
    }
}

fn sel_filt_match(f: &SelFilt, l: &SelLine) -> bool {
    let item = match f.field {
        None if l.kind != show::DATA => l.data.unwrap_or(0) as u32,
        _ => {
            if f.field.is_some() {
                if l.level <= f.pos {
                    return false;
                }
                let t = u32::from(sel_type(l, f.pos));
                if t >= 32 || f.mask >> t & 1 == 0 {
                    return false;
                }
            }
            match l.items.get(f.pos) {
                Some(&i) => i,
                None => return false,
            }
        }
    };

    f.items.is_empty() || f.items.contains(&item)
}

fn sel_line_match(fs: &[SelFilt], grand: bool, axis: usize, l: &SelLine) -> bool {
    let on: Vec<&SelFilt> = fs.iter().filter(|f| f.axis == axis).collect();
    if l.kind == show::GRAND {
        return grand
            && on
                .iter()
                .filter(|f| f.field.is_none())
                .all(|f| sel_filt_match(f, l));
    }
    if grand || on.is_empty() {
        return false;
    }
    on.iter().all(|f| sel_filt_match(f, l))
}

impl Workbook {
    pub fn get_pivot_data(
        &self,
        data: &Value,
        anchor: CellAddr,
        single: bool,
        pairs: &[(Value, Value)],
    ) -> Value {
        let at = Rect::cell(anchor.row, anchor.col);
        let ref_err = Value::Error(crate::value::XlError::REF);
        let ti = match self.pivot_meeting(anchor.sheet, &at) {
            Some(ti) => ti,

            None if single => {
                let below = Rect::cell(anchor.row + 1, anchor.col);
                match self.pivot_meeting(anchor.sheet, &below) {
                    Some(ti)
                        if self.pivots.tables[ti].row0 == anchor.row + 1
                            && self.pivots.tables[ti].col0 == anchor.col =>
                    {
                        ti
                    }
                    _ => return ref_err,
                }
            }
            None => return ref_err,
        };
        match self.pivot_data_cell(ti, data, pairs) {
            Some(a) => match self.value(a) {
                Value::Blank => Value::Number(0.0),
                v => v,
            },
            None => ref_err,
        }
    }

    pub fn get_pivot_data_name(&self, ti: usize, text: &str) -> Value {
        let ref_err = Value::Error(crate::value::XlError::REF);
        let Some(t) = self.pivots.tables.get(ti) else {
            return ref_err;
        };
        let Some(c) = self.pivots.caches.get(t.cache) else {
            return ref_err;
        };
        let grouped;
        let c = if t.fields.iter().any(|f| f.group.is_some()) {
            grouped = c.grouped(t);
            &grouped
        } else {
            c
        };
        let chars: Vec<char> = text.chars().collect();
        let fs = if chars.is_empty() {
            Some(Vec::new())
        } else {
            let cx = NameCtx { t, c, s: &chars };
            let rule = scan_name_terms(&chars).and_then(|mut terms| {
                cx.fill_terms(&mut terms)?;
                cx.build_rule(&terms)
            });
            match rule {
                Some(fs) => fs,
                None => return Value::Error(crate::value::XlError::NA),
            }
        };
        let (rax, cax) = t.axes();
        match fs.and_then(|fs| rule_cell(t, c, [&rax, &cax], fs)) {
            Some(a) => match self.value(a) {
                Value::Blank => Value::Number(0.0),
                v => v,
            },
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
        let text_of = |v: &Value| match v {
            Value::Text(s) => Some(s.to_string()),
            Value::Number(n) => Some(number_label(*n)),
            _ => None,
        };

        let data_by_name = |name: &str| pivot_data_named(t, c, name);
        let d = data_by_name(&text_of(data)?)?;
        let (rax, cax) = t.axes();
        let axes = [&rax, &cax];
        let place =
            |a: Ax| (0..2).find_map(|x| axes[x].iter().position(|y| *y == a).map(|p| (x, p)));
        let mut fs: Vec<SelFilt> = Vec::new();
        let add =
            |fs: &mut Vec<SelFilt>, axis: usize, pos: usize, field: Option<usize>, item: u32| {
                match fs.iter_mut().find(|f| f.field == field && f.axis == axis) {
                    Some(f) if !f.items.contains(&item) => f.items.push(item),
                    Some(_) => {}
                    None => fs.push(SelFilt {
                        axis,
                        pos,
                        field,
                        items: vec![item],
                        mask: 1,
                    }),
                }
            };
        if let Some((x, p)) = place(Ax::Values) {
            add(&mut fs, x, p, None, d as u32);
        }
        for (f, item) in pairs {
            let Value::Text(fname) = f else { return None };

            let Some(fi) = (0..c.fields.len()).find(|&k| same_text(t.field_name(c, k), fname))
            else {
                if !same_text(fname, "Values") {
                    return None;
                }
                let dd = data_by_name(&text_of(item)?)?;
                match place(Ax::Values) {
                    Some((x, p)) => add(&mut fs, x, p, None, dd as u32),
                    None if dd == d => {}
                    None => return None,
                }
                continue;
            };
            let ii = pivot_item_named(t, c, fi, item)?;
            match place(Ax::Field(fi)) {
                Some((x, p)) => add(&mut fs, x, p, Some(fi), ii),

                None if t.fields[fi].orient == Orient::Page
                    && t.fields[fi].page_item == Some(ii) => {}
                None => return None,
            }
        }
        rule_cell(t, c, [&rax, &cax], fs)
    }
}

fn rule_cell(
    t: &PivotTable,
    c: &PivotCache,
    axes: [&Vec<Ax>; 2],
    mut fs: Vec<SelFilt>,
) -> Option<CellAddr> {
    let fs = &mut fs;

    let mut grand = [false; 2];
    for x in 0..2 {
        let named: Vec<usize> = (0..fs.len()).filter(|&i| fs[i].axis == x).collect();
        if named.is_empty() {
            grand[x] = true;
            continue;
        }
        let n = axes[x].len();
        if named.len() == n {
            continue;
        }
        let mut best: Option<usize> = None;
        let mut multi = false;
        for &i in &named {
            if fs[i].field.is_none() && named.len() == 1 && n != 1 {
                grand[x] = true;
            }

            if fs[i].mask > 1 {
                multi = true;
                break;
            }

            if fs[i].field.is_some() && best.is_none_or(|b| fs[b].pos < fs[i].pos) {
                best = Some(i);
            }
        }
        if multi {
            continue;
        }
        let mut last = n - 1;
        while last > 0 && axes[x][last] == Ax::Values {
            last -= 1;
        }
        if let Some(b) = best {
            if let Some(f) = fs[b].field {
                let pf = &t.fields[f];

                if fs[b].pos < last
                    && fs[b].items.len() == 1
                    && !pf.collapsed.contains(&fs[b].items[0])
                    && pf.subtotals
                    && pf.custom.is_empty()
                {
                    fs[b].mask = 2;
                }
            }
        }
    }

    let values_at = axes[0].iter().position(|a| *a == Ax::Values);
    let mut headers = false;
    for f in fs.iter_mut().filter(|f| f.axis == 0) {
        let Some(field) = f.field else {
            continue;
        };
        let pf = &t.fields[field];
        let single = usize::from(pf.subtotals) + pf.custom.len() == 1;
        if t.eff_form(field) != Form::Tabular && t.sub_at_top(field) && single {
            headers = true;
            if !(t.data.len() > 1 && values_at.is_some_and(|v| v > f.pos)) {
                f.mask = 1;
            }
        }
    }
    let starts = |l: &SelLine| {
        l.kind == show::GRAND
            || fs
                .iter()
                .any(|f| f.axis == 0 && f.pos < l.level && l.same <= f.pos)
    };
    let r = render(t, c);
    let pick = |x: usize, ls: &[SelLine]| -> Option<u32> {
        let hit: Vec<u32> = if axes[x].is_empty() {
            ls.iter().map(|l| l.at).collect()
        } else {
            ls.iter()
                .filter(|l| sel_line_match(fs, grand[x], x, l))
                .filter(|l| x != 0 || !headers || starts(l))
                .map(|l| l.at)
                .collect()
        };
        match hit[..] {
            [one] => Some(one),
            _ => None,
        }
    };
    let row = pick(0, &r.sel_rows)?;
    let col = pick(1, &r.sel_cols)?;
    Some(CellAddr::new(t.sheet, t.row0 - r.top + row, t.col0 + col))
}

fn pivot_data_named(t: &PivotTable, c: &PivotCache, name: &str) -> Option<usize> {
    t.data
        .iter()
        .position(|d| same_text(&d.caption, name))
        .or_else(|| {
            let over: Vec<usize> = (0..t.data.len())
                .filter(|&d| same_text(t.field_name(c, t.data[d].src), name))
                .collect();
            over.iter()
                .copied()
                .find(|&d| t.data[d].func == Func::Sum)
                .or(over.first().copied())
        })
}

const NAME_STOP_TOP: [char; 3] = [' ', '[', ':'];

const NAME_STOP_ITEM: [char; 4] = [',', ']', ';', ':'];

const TERM_FIELD: u8 = 1;
const TERM_ITEM: u8 = 2;
const TERM_SUB: u8 = 4;
const TERM_KEYWORD: u8 = 8;
const TERM_SPECIAL: u8 = 0x10;

const ITEM_ALL: i32 = 0x10_00fe;
const ITEM_BLANK: i32 = 0x10_0100;

const GRAND_WORDS: [&str; 4] = [
    "Grand Total",
    "Row Grand Total",
    "Column Grand Total",
    "Total",
];

const KEYWORDS: [(&str, u8); 18] = [
    ("Sum", 2),
    ("Count", 3),
    ("Average", 4),
    ("Max", 5),
    ("Min", 6),
    ("Product", 7),
    ("Count Numbers", 8),
    ("StdDev", 9),
    ("StdDevp", 10),
    ("Var", 11),
    ("Varp", 12),
    ("Distinct Count", 13),
    ("Total", 1),
    ("Data", 0),
    ("Blank", 0x11),
    ("Count Nums", 8),
    ("Stdev", 9),
    ("Stdevp", 10),
];

#[derive(Clone, Debug)]
struct SxTerm {
    start: usize,
    len: usize,

    quoted: bool,

    class: u8,

    cand: u8,

    kind: u8,

    isxvd: i32,

    item: i32,

    sub: u8,

    data: Option<usize>,

    position: bool,
}

fn scan_name_token(s: &[char], pos: &mut usize, stops: &[char]) -> Option<(usize, usize, bool)> {
    let mut p = *pos;
    while p < s.len() && s[p] == ' ' {
        p += 1;
    }
    if p >= s.len() {
        return None;
    }
    let quoted = s[p] == '\'';
    if quoted {
        p += 1;
    }
    let start = p;
    let len = loop {
        if p >= s.len() {
            if !quoted && stops[0] == ' ' {
                break p - start;
            }
            return None;
        }
        let ch = s[p];
        if quoted {
            p += 1;
            if ch != '\'' {
                continue;
            }
            if p < s.len() && s[p] == '\'' {
                p += 1;
                continue;
            }
            break p - start - 1;
        }
        if stops.contains(&ch) {
            let mut l = p - start;
            while l > 0 && s[start + l - 1] == ' ' {
                l -= 1;
            }
            break l;
        }
        p += 1;
    };

    if !quoted && len == 0 {
        return None;
    }
    while p < s.len() && s[p] == ' ' {
        p += 1;
    }
    *pos = p;
    Some((start, len, quoted))
}

fn scan_name_terms(s: &[char]) -> Option<Vec<SxTerm>> {
    let mut terms: Vec<SxTerm> = Vec::new();
    let mut state = 1u8;
    let mut pos = 0usize;
    loop {
        while pos < s.len() && s[pos] == ' ' {
            pos += 1;
        }
        let stops: &[char] = if matches!(state, 1 | 4) {
            &NAME_STOP_TOP
        } else {
            &NAME_STOP_ITEM
        };
        let Some((start, len, quoted)) = scan_name_token(s, &mut pos, stops) else {
            if state != 1 || terms.is_empty() || (pos < s.len() && s[pos] == '(') {
                return None;
            }
            return (pos == s.len()).then_some(terms);
        };
        let mut next = state;
        match state {
            1 => match s.get(pos) {
                Some('[') => {
                    next = 2;
                    pos += 1;
                }
                Some(':') => {
                    next = 4;
                    pos += 1;
                }
                _ => {}
            },
            2 | 3 | 5 => {
                let ch = *s.get(pos)?;
                pos += 1;
                next = match (state, ch) {
                    (_, ']') => 1,
                    (2, ':') => 5,
                    (2 | 5, ';') => 3,
                    (5, ',') => 2,
                    _ => state,
                };
            }
            _ => next = 1,
        }
        terms.push(SxTerm {
            start,
            len,
            quoted,
            class: state,
            cand: 0,
            kind: 0,
            isxvd: -1,
            item: -1,
            sub: 0,
            data: None,
            position: false,
        });
        state = next;
    }
}

fn name_position(text: &str) -> Option<i32> {
    let cs: Vec<char> = text.chars().collect();
    let first = *cs.first()?;
    let signed = first == '+' || first == '-';
    let digits = if signed { &cs[1..] } else { &cs[..] };
    if digits.is_empty() {
        return None;
    }
    let mut v = 0i32;
    for d in digits {
        v = v
            .wrapping_mul(10)
            .wrapping_add(i32::try_from(d.to_digit(10)?).ok()?);
    }
    let pos = if signed { v } else { v.wrapping_sub(1) };
    Some(if first == '-' {
        pos.wrapping_neg()
    } else {
        pos
    })
}

struct RuleFilt {
    axis: u8,
    dim: i32,
    field: i32,
    items: Vec<u32>,

    mask: u32,

    position: bool,
}

struct NameCtx<'a> {
    t: &'a PivotTable,
    c: &'a PivotCache,
    s: &'a [char],
}

impl NameCtx<'_> {
    fn text(&self, tm: &SxTerm) -> Option<String> {
        let sl = &self.s[tm.start..tm.start + tm.len];
        let mut out = String::new();
        if tm.quoted {
            let mut k = 0;
            while k < sl.len() {
                if sl[k] == '\'' {
                    k += 1;
                }
                if let Some(&ch) = sl.get(k) {
                    out.push(ch);
                }
                k += 1;
            }
        } else {
            out.extend(sl);
        }
        (out.encode_utf16().count() <= 256).then_some(out)
    }

    fn field_named(&self, name: &str) -> Option<i32> {
        let n = self.c.fields.len().min(self.t.fields.len());
        (0..n)
            .find(|&k| same_text(self.t.field_name(self.c, k), name))
            .map(|k| k as i32)
            .or_else(|| same_text(name, "Values").then_some(-2))
    }

    fn search_item(&self, f: i32, text: &str) -> Option<i32> {
        if f == -2 {
            let named = |d: &DataField| same_text(&d.caption, text);
            return self.t.data.iter().position(named).map(|d| d as i32);
        }
        let f = usize::try_from(f).ok()?;
        pivot_item_named(self.t, self.c, f, &Value::text(text)).map(|i| i as i32)
    }

    fn search_all(&self, text: &str) -> Option<Option<(i32, i32)>> {
        let nd = self.t.data.len();
        let n = self.c.fields.len().min(self.t.fields.len()) as i32;
        for pass in 0..2 {
            let mut hit = None;
            for f in -2..n {
                if f == -1 {
                    continue;
                }
                let active = if f == -2 {
                    nd > 1
                } else {
                    matches!(
                        self.t.fields[f as usize].orient,
                        Orient::Row | Orient::Column | Orient::Page
                    )
                };
                if (pass == 0) != active {
                    continue;
                }
                if let Some(it) = self.search_item(f, text) {
                    if hit.is_some() {
                        return None;
                    }
                    hit = Some((f, it));
                }
            }
            if hit.is_some() {
                return Some(hit);
            }
        }
        Some(None)
    }

    fn item_caption(&self, f: usize, i: u32) -> String {
        if let Some(cap) = self.t.fields[f].item_caps.iter().find(|x| x.0 == i) {
            return cap.1.to_string();
        }
        let items = &self.c.fields[f].items;
        match items.get(i as usize) {
            Some(it) => it.label.to_string(),
            None => self.t.fields[f]
                .calc_items
                .get(i as usize - items.len())
                .map_or_else(String::new, |x| x.name.to_string()),
        }
    }

    fn search_sub(&self, text: &str) -> Option<(i32, i32, u8, Option<usize>)> {
        let (rax, cax) = self.t.axes();
        let nd = self.t.data.len();
        let n = self.c.fields.len().min(self.t.fields.len());
        for f in 0..n {
            let Some(axis) = [&rax, &cax].into_iter().find(|a| a.contains(&Ax::Field(f))) else {
                continue;
            };
            let datas: Vec<Option<usize>> = if nd > 1 && axis.contains(&Ax::Values) {
                (0..nd).map(Some).collect()
            } else {
                vec![None]
            };
            let pf = &self.t.fields[f];
            let types: Vec<u8> = pf
                .subtotals
                .then_some(1)
                .into_iter()
                .chain(pf.custom.iter().copied())
                .collect();
            for i in self.order(f as i32) {
                let item = self.item_caption(f, i);
                for &ty in &types {
                    for &d in &datas {
                        let mut names = Vec::new();
                        if ty == 1 {
                            names.push(match d {
                                Some(d) => self.t.data[d].caption.to_string(),
                                None => "Total".to_string(),
                            });
                        } else {
                            names.push(sub_func(ty).1.to_string());

                            match ty {
                                9 => names.push("Stdev".to_string()),
                                10 => names.push("Stdevp".to_string()),
                                _ => {}
                            }
                            if let Some(d) = d {
                                let src = self.t.field_name(self.c, self.t.data[d].src);
                                for x in &mut names {
                                    *x = format!("{x} of {src}");
                                }
                            }
                        }
                        if names
                            .iter()
                            .any(|x| same_text(&format!("{item} {x}"), text))
                        {
                            return Some((f as i32, i as i32, ty, d));
                        }
                    }
                }
            }
        }
        None
    }

    fn search_data_total(&self, text: &str) -> Option<usize> {
        let (rax, cax) = self.t.axes();
        let shown = (rax.contains(&Ax::Values) && self.t.col_grand)
            || (cax.contains(&Ax::Values) && self.t.row_grand);
        (0..self.t.data.len()).find(|&d| {
            shown
                && self.t.data.len() > 1
                && same_text(&format!("Total {}", self.t.data[d].caption), text)
        })
    }

    fn order(&self, f: i32) -> Vec<u32> {
        if f == -2 {
            (0..self.t.data.len() as u32).collect()
        } else if f >= 0 {
            display_order(self.t, self.c, f as usize)
        } else {
            Vec::new()
        }
    }

    fn fill_one(&self, terms: &mut [SxTerm], i: usize) -> Option<bool> {
        if terms[i].class == 6 {
            let prev = terms[i - 1].clone();
            let tm = &mut terms[i];
            tm.start = prev.start;
            tm.len = prev.len;
            tm.quoted = prev.quoted;
            tm.isxvd = prev.isxvd;
            tm.item = prev.item;
            tm.kind = 0;
            return Some(true);
        }
        let text = self.text(&terms[i])?;
        let (class, cand) = (terms[i].class, terms[i].cand);
        if cand & TERM_FIELD != 0 {
            if let Some(f) = self.field_named(&text) {
                terms[i].kind = TERM_FIELD;
                terms[i].isxvd = f;
                return Some(true);
            }
        }
        if class == 1 && cand & TERM_ITEM != 0 {
            if let Some((f, it)) = self.search_all(&text)? {
                terms[i].kind = TERM_ITEM;
                terms[i].isxvd = f;
                terms[i].item = it;
                return Some(true);
            }
        }

        if cand & TERM_SUB != 0 {
            if GRAND_WORDS.iter().any(|w| same_text(w, &text)) {
                terms[i].class = 6;
                terms[i].kind = 0;
                return Some(true);
            }
            if let Some((f, it, ty, data)) = self.search_sub(&text) {
                let tm = &mut terms[i];
                tm.kind = TERM_SUB;
                tm.isxvd = f;
                tm.item = it;

                tm.sub = if ty == 1 && data.is_none() { 0 } else { ty };
                tm.data = data;
                return Some(true);
            }
            if let Some(d) = self.search_data_total(&text) {
                let tm = &mut terms[i];
                tm.kind = TERM_SUB;
                tm.isxvd = -1;
                tm.item = 0;
                tm.sub = show::GRAND as u8;
                tm.data = Some(d);
                return Some(true);
            }
        }
        if class == 1 {
            return Some(false);
        }
        let prev = terms[i - 1].isxvd;
        if cand & TERM_SPECIAL != 0 {
            let mut position = false;
            let special = if same_text(&text, "All") {
                Some(ITEM_ALL)
            } else if same_text(&text, "Blank") {
                Some(ITEM_BLANK)
            } else if let Some(pos) = name_position(&text) {
                position = true;

                Some(
                    usize::try_from(pos)
                        .ok()
                        .and_then(|p| self.order(prev).get(p).copied())
                        .map_or(-1, |i| i as i32),
                )
            } else {
                None
            };
            if let Some(item) = special {
                terms[i].kind = if item == ITEM_ALL || item == ITEM_BLANK {
                    TERM_SPECIAL
                } else {
                    TERM_ITEM
                };
                terms[i].isxvd = prev;
                terms[i].item = item;
                terms[i].position = position;
                return Some(true);
            }
        }
        if cand & TERM_KEYWORD != 0 {
            if let Some(k) = KEYWORDS.iter().find(|k| same_text(k.0, &text)) {
                terms[i].kind = TERM_KEYWORD;
                terms[i].isxvd = prev;
                terms[i].sub = k.1;
                return Some(true);
            }
        }
        if cand & TERM_ITEM != 0 {
            if let Some(it) = self.search_item(prev, &text) {
                terms[i].kind = TERM_ITEM;
                terms[i].isxvd = prev;
                terms[i].item = it;
                return Some(true);
            }
        }
        Some(false)
    }

    fn fill_terms(&self, terms: &mut [SxTerm]) -> Option<()> {
        let n = terms.len();
        for i in 0..n {
            let next = if i + 1 < n { terms[i + 1].class } else { 1 };
            terms[i].cand = match terms[i].class {
                1 => {
                    let m = if next == 2 { 0 } else { TERM_ITEM };
                    match next {
                        4 => m,
                        1 => m | TERM_SUB | TERM_FIELD,
                        _ => m | TERM_FIELD,
                    }
                }
                2 | 5 => {
                    if terms[i].quoted {
                        TERM_ITEM
                    } else {
                        TERM_ITEM | TERM_SPECIAL
                    }
                }
                3 => TERM_KEYWORD,
                4 => TERM_ITEM,
                _ => return None,
            };
        }
        for i in 0..n {
            let mut j = i;
            while !self.fill_one(terms, i)? {
                let nx = j + 1;
                let bad = nx >= n
                    || terms[i].quoted
                    || terms[nx].quoted
                    || !matches!(terms[i].class, 1 | 4)
                    || !matches!(terms[nx].class, 1 | 4);
                if bad {
                    return None;
                }
                terms[nx].class = 6;
                terms[i].len = terms[nx].start + terms[nx].len - terms[i].start;
                j = nx;
            }
        }
        let refused = (1..n).any(|i| {
            terms[i].kind == TERM_SPECIAL
                && (terms[i - 1].class == 2 || terms.get(i + 1).is_some_and(|t| t.class == 2))
        });
        (!refused).then_some(())
    }

    fn place(&self, isxvd: i32) -> (u8, i32) {
        let (rax, cax) = self.t.axes();
        let target = if isxvd == -2 {
            Ax::Values
        } else {
            Ax::Field(isxvd as usize)
        };
        if let Some(p) = rax.iter().position(|a| *a == target) {
            return (1, p as i32);
        }
        if let Some(p) = cax.iter().position(|a| *a == target) {
            return (2, p as i32);
        }
        if isxvd >= 0 {
            if let Some(p) = self.t.pages.iter().position(|&f| f as i32 == isxvd) {
                return (4, p as i32);
            }
        }
        if isxvd == -2 && self.t.data.len() == 1 {
            return (8, 0);
        }
        (0, -1)
    }

    fn filt(fs: &mut Vec<RuleFilt>, (axis, dim): (u8, i32), field: i32) -> &mut RuleFilt {
        let k = match fs.iter().position(|f| f.axis == axis && f.dim == dim) {
            Some(k) => k,
            None => {
                fs.push(RuleFilt {
                    axis,
                    dim,
                    field,
                    items: Vec::new(),
                    mask: 0,
                    position: false,
                });
                fs.len() - 1
            }
        };
        &mut fs[k]
    }

    fn add_item(f: &mut RuleFilt, item: i32) {
        f.mask |= 1;
        if item == ITEM_ALL {
            f.items.clear();
            return;
        }
        let it = u32::try_from(item).unwrap_or(u32::MAX);
        if !f.items.contains(&it) {
            f.items.push(it);
        }
    }

    fn build_rule(&self, terms: &[SxTerm]) -> Option<Option<Vec<SelFilt>>> {
        let mut fs: Vec<RuleFilt> = Vec::new();

        let mut cur = -1i32;
        let mut lo = 0i64;
        let n = terms.len();
        let pos_of = |f: i32, item: i32| -> i64 {
            self.order(f)
                .iter()
                .position(|&x| i64::from(x) == i64::from(item))
                .map_or(-1, |p| p as i64)
        };
        for (k, tm) in terms.iter().enumerate() {
            match tm.class {
                1 if tm.kind == TERM_SUB && u16::from(tm.sub) == show::GRAND => {
                    if let Some(d) = tm.data {
                        let f = Self::filt(&mut fs, self.place(-2), -2);
                        Self::add_item(f, d as i32);
                    }
                }
                1 | 2 => {
                    let (mut kind, mut isxvd, mut item) = (tm.kind, tm.isxvd, tm.item);
                    if tm.class == 1 {
                        let mut alone =
                            kind == TERM_FIELD && (k + 1 == n || terms[k + 1].class == 1);

                        if alone && isxvd >= 0 {
                            if let Some(d) = self.t.data.iter().position(|d| d.src as i32 == isxvd)
                            {
                                kind = TERM_ITEM;
                                isxvd = -2;
                                item = d as i32;
                                alone = false;
                            }
                        }
                        cur = isxvd;
                        let f = Self::filt(&mut fs, self.place(cur), cur);
                        if alone {
                            if f.mask != 0 {
                                return None;
                            }
                            f.mask = 1;
                        }
                        if kind == TERM_FIELD {
                            continue;
                        }
                    }
                    lo = pos_of(cur, item);
                    let f = Self::filt(&mut fs, self.place(cur), cur);
                    f.position = tm.position;
                    if tm.sub == 0 {
                        Self::add_item(f, item);
                        continue;
                    }

                    f.mask |= 1 << tm.sub;
                    let mask = f.mask;
                    Self::add_item(f, item);
                    f.mask = mask;

                    if let Some(d) = tm.data {
                        let it = self.order(cur).get(d).map_or(-1, |&i| i as i32);
                        Self::add_item(f, it);
                    }
                }
                3 => {
                    let f = Self::filt(&mut fs, self.place(cur), cur);
                    if terms[k - 1].class != 3 {
                        f.mask = 0;
                    }
                    f.mask |= 1 << tm.sub;
                }
                4 | 5 => {
                    let hi = pos_of(cur, tm.item);
                    let (a, b) = if hi < lo { (hi, lo) } else { (lo, hi) };
                    lo = a;
                    let order = self.order(cur);
                    let f = Self::filt(&mut fs, self.place(cur), cur);
                    for p in a.max(0)..=b.min(order.len() as i64 - 1) {
                        Self::add_item(f, order[p as usize] as i32);
                    }
                }
                6 => {}
                _ => return None,
            }
        }
        Some(fs.iter().all(|f| self.scans(f)).then(|| {
            fs.into_iter()
                .filter(|f| f.axis == 1 || f.axis == 2)
                .map(|f| SelFilt {
                    axis: usize::from(f.axis - 1),
                    pos: f.dim as usize,
                    field: usize::try_from(f.field).ok(),
                    items: f.items,
                    mask: f.mask,
                })
                .collect()
        }))
    }

    fn scans(&self, f: &RuleFilt) -> bool {
        if f.position {
            return false;
        }
        match f.axis {
            1 | 2 => true,
            4 if f.items.is_empty() => f.mask & 1 != 0,
            4 => usize::try_from(f.field)
                .ok()
                .and_then(|k| self.t.fields[k].page_item)
                .is_some_and(|i| f.items.contains(&i)),
            8 => f.items.is_empty() || f.items.contains(&0),
            _ => false,
        }
    }
}

fn pivot_item_named(t: &PivotTable, c: &PivotCache, f: usize, item: &Value) -> Option<u32> {
    let caps = &t.fields[f].item_caps;
    if let Value::Text(b) = item {
        if let Some(x) = caps.iter().find(|x| same_text(&x.1, b)) {
            return Some(x.0);
        }
    }
    let items = &c.fields[f].items;
    let by_value = (0..items.len()).find(|&i| match (&items[i].value, item) {
        (Value::Text(a), Value::Text(b)) => same_text(a, b),
        (Value::Number(a), Value::Number(b)) => a == b,
        (Value::Bool(a), Value::Bool(b)) => a == b,
        _ => false,
    });
    let text = match item {
        Value::Text(s) => s.to_string(),
        Value::Number(n) => number_label(*n),
        _ => return by_value.map(|i| i as u32),
    };
    let calc = &t.fields[f].calc_items;
    by_value
        .or_else(|| (0..items.len()).find(|&i| same_text(&items[i].label, &text)))
        .or_else(|| {
            (0..calc.len())
                .find(|&j| same_text(&calc[j].name, &text))
                .map(|j| items.len() + j)
        })
        .map(|i| i as u32)
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

#[cfg(test)]
mod tests {
    use super::default_group_step;

    #[test]
    fn group_step_is_a_floored_power_of_ten_of_a_third_of_the_span() {
        assert_eq!(default_group_step(1.0, 9.0), 1.0);
        assert_eq!(default_group_step(1.0, 25.0), 1.0);
        assert_eq!(default_group_step(1.0, 31.0), 10.0);
        assert_eq!(default_group_step(1.0, 100.0), 10.0);
        assert_eq!(default_group_step(0.1, 0.5), 0.1);
        assert_eq!(default_group_step(5.0, 6.0), 0.1);
        assert_eq!(default_group_step(0.002, 0.02), 0.001);
        assert_eq!(default_group_step(3.0, 3.0), 1.0);
    }

    use super::*;

    fn chars(s: &str) -> Vec<char> {
        s.chars().collect()
    }

    fn scanned(s: &str) -> Option<Vec<(String, u8, bool)>> {
        let cs = chars(s);
        scan_name_terms(&cs).map(|ts| {
            ts.iter()
                .map(|t| {
                    (
                        cs[t.start..t.start + t.len].iter().collect::<String>(),
                        t.class,
                        t.quoted,
                    )
                })
                .collect()
        })
    }

    fn w(t: &str, c: u8) -> (String, u8, bool) {
        (t.to_string(), c, false)
    }

    #[test]
    fn scanner_splits_words_and_brackets() {
        assert_eq!(scanned("East"), Some(vec![w("East", 1)]));
        assert_eq!(
            scanned("  Region   East  Sales "),
            Some(vec![w("Region", 1), w("East", 1), w("Sales", 1)])
        );

        assert_eq!(
            scanned("Region[East, West]"),
            Some(vec![w("Region", 1), w("East", 2), w("West", 2)])
        );
        assert_eq!(
            scanned("Region[East;Sum] Sales"),
            Some(vec![
                w("Region", 1),
                w("East", 2),
                w("Sum", 3),
                w("Sales", 1)
            ])
        );

        assert_eq!(scanned("a:b"), Some(vec![w("a", 1), w("b", 4)]));
        assert_eq!(
            scanned("F[a:b]"),
            Some(vec![w("F", 1), w("a", 2), w("b", 5)])
        );

        assert_eq!(scanned("F[ a b ]"), Some(vec![w("F", 1), w("a b", 2)]));
    }

    #[test]
    fn scanner_quotes() {
        assert_eq!(
            scanned("'North America' x"),
            Some(vec![("North America".into(), 1, true), w("x", 1)])
        );

        assert_eq!(scanned("'it''s'"), Some(vec![("it''s".into(), 1, true)]));

        assert_eq!(scanned("''"), Some(vec![(String::new(), 1, true)]));

        assert_eq!(scanned("'abc"), None);
        assert_eq!(scanned("F[a"), None);
        assert_eq!(scanned("SUM(a)"), Some(vec![w("SUM(a)", 1)]));
        assert_eq!(scanned("   "), None);

        assert_eq!(scanned("a:"), None);
        assert_eq!(scanned("[a]"), None);
    }

    #[test]
    fn positions_are_digits_with_an_optional_sign() {
        assert_eq!(name_position("1"), Some(0));
        assert_eq!(name_position("12"), Some(11));
        assert_eq!(name_position("0"), Some(-1));
        assert_eq!(name_position("+2"), Some(2));
        assert_eq!(name_position("-2"), Some(-2));
        assert_eq!(name_position("+"), None);
        assert_eq!(name_position("1a"), None);
        assert_eq!(name_position("a"), None);
        assert_eq!(name_position(""), None);
    }

    fn fixture(two: bool) -> (Workbook, usize) {
        let mut wb = Workbook::new();
        let s = wb.sheet_at(0).id;
        let recs: [(&str, &str, f64, f64); 5] = [
            ("East", "Apple", 10.0, 1.0),
            ("East", "Plum", 20.0, 2.0),
            ("West", "Apple", 30.0, 3.0),
            ("West", "Plum", 40.0, 4.0),
            ("North America", "Apple", 5.0, 5.0),
        ];
        let rows: Vec<Vec<Value>> = recs
            .iter()
            .map(|r| {
                vec![
                    Value::text(r.0),
                    Value::text(r.1),
                    Value::Number(r.2),
                    Value::Number(r.3),
                ]
            })
            .collect();
        let names = ["Region", "Product", "Sales", "Cost"]
            .iter()
            .map(|n| n.to_string())
            .collect();
        let cache = PivotCache::from_records("", s, names, rows, |v| match v {
            Value::Text(t) => t.to_string(),
            Value::Number(x) => number_label(*x),
            _ => String::new(),
        });
        let pv = wb.pivots_mut();
        pv.caches.push(cache);
        let mut t = PivotTable::new("PivotTable1", 0, 4, s, 0, 0);
        t.set_orient(0, Orient::Row);
        t.set_orient(1, Orient::Column);
        let df = |src: usize, cap: &str| DataField {
            src,
            caption: cap.to_string(),
            func: Func::Sum,
            numfmt: Some("General".into()),
            show: Default::default(),
        };
        t.fields[2].orient = Orient::Data;
        t.add_data(df(2, "Sum of Sales"));
        if two {
            t.fields[3].orient = Orient::Data;
            t.add_data(df(3, "Sum of Cost"));
        }
        pv.tables.push(t);
        wb.pivot_update(0).unwrap();
        (wb, 0)
    }

    #[derive(Debug, PartialEq)]
    enum Ans {
        Num(f64),
        Na,
        Ref,
        Other,
    }

    fn name(wb: &Workbook, ti: usize, text: &str) -> Ans {
        match wb.get_pivot_data_name(ti, text) {
            Value::Number(x) => Ans::Num(x),
            Value::Error(e) if e == XlError::NA => Ans::Na,
            Value::Error(e) if e == XlError::REF => Ans::Ref,
            _ => Ans::Other,
        }
    }

    fn num(x: f64) -> Ans {
        Ans::Num(x)
    }

    fn err(e: XlError) -> Ans {
        if e == XlError::NA {
            Ans::Na
        } else {
            Ans::Ref
        }
    }

    #[test]
    fn name_syntax_grand_total_and_data_field() {
        let (wb, ti) = fixture(false);

        assert_eq!(name(&wb, ti, ""), num(105.0));

        assert_eq!(name(&wb, ti, "Sales"), num(105.0));
        assert_eq!(name(&wb, ti, "sales"), num(105.0));

        assert_eq!(name(&wb, ti, "Nothing"), err(XlError::NA));
        assert_eq!(name(&wb, ti, "East Nothing"), err(XlError::NA));
        assert_eq!(name(&wb, ti, "   "), err(XlError::NA));
        assert_eq!(name(&wb, ti, "'East"), err(XlError::NA));
    }

    #[test]
    fn name_syntax_items_and_fields() {
        let (wb, ti) = fixture(false);

        assert_eq!(name(&wb, ti, "East"), num(30.0));
        assert_eq!(name(&wb, ti, "Apple"), num(45.0));
        assert_eq!(name(&wb, ti, "East Apple"), num(10.0));
        assert_eq!(name(&wb, ti, "Apple East"), num(10.0));
        assert_eq!(name(&wb, ti, "Region East Product Plum"), num(20.0));
        assert_eq!(name(&wb, ti, "East Plum Sales"), num(20.0));

        assert_eq!(name(&wb, ti, "North America"), num(5.0));
        assert_eq!(name(&wb, ti, "'North America' Apple"), num(5.0));

        assert_eq!(name(&wb, ti, "North America Plum"), num(0.0));

        assert_eq!(name(&wb, ti, "Region[East] Product[Apple]"), num(10.0));
        assert_eq!(name(&wb, ti, "Region['North America']"), num(5.0));
        assert_eq!(name(&wb, ti, "Product[Plum]"), num(60.0));

        assert_eq!(name(&wb, ti, "Region Product[Plum]"), err(XlError::REF));

        assert_eq!(name(&wb, ti, "Region[East,West]"), err(XlError::REF));

        assert_eq!(name(&wb, ti, "'North' America"), err(XlError::NA));

        assert_eq!(name(&wb, ti, "Region[Plum]"), err(XlError::NA));

        assert_eq!(name(&wb, ti, "Region[East;West]"), err(XlError::NA));
    }

    #[test]
    fn name_syntax_positions_and_ranges() {
        let (wb, ti) = fixture(false);

        for text in [
            "Region[1]",
            "Region[+1]",
            "Region[4]",
            "Region[0]",
            "Region[3] Product[2]",
        ] {
            assert_eq!(name(&wb, ti, text), err(XlError::REF), "{text}");
        }
        assert_eq!(name(&wb, ti, "Region[$2]"), err(XlError::NA));

        assert_eq!(name(&wb, ti, "Region['1']"), err(XlError::NA));

        assert_eq!(name(&wb, ti, "Region[East:West]"), err(XlError::REF));
        assert_eq!(name(&wb, ti, "Region[West:West]"), num(70.0));
        assert_eq!(name(&wb, ti, "East:East"), num(30.0));
        assert_eq!(name(&wb, ti, "West:East"), err(XlError::REF));
    }

    #[test]
    fn name_syntax_several_data_fields() {
        let (wb, ti) = fixture(true);

        assert_eq!(name(&wb, ti, ""), err(XlError::REF));
        assert_eq!(name(&wb, ti, "East"), err(XlError::REF));

        assert_eq!(name(&wb, ti, "Sum of Sales"), num(105.0));
        assert_eq!(name(&wb, ti, "Cost"), num(15.0));
        assert_eq!(name(&wb, ti, "East Cost"), num(3.0));
        assert_eq!(name(&wb, ti, "Sum of Cost West Plum"), num(4.0));
        assert_eq!(name(&wb, ti, "Values['Sum of Cost'] East"), num(3.0));

        assert_eq!(name(&wb, ti, "Values[Cost] East"), err(XlError::NA));
    }

    #[test]
    fn name_syntax_item_in_two_fields_is_ambiguous() {
        let mut wb = Workbook::new();
        let s = wb.sheet_at(0).id;
        let rows = vec![
            vec![Value::text("x"), Value::text("y"), Value::Number(1.0)],
            vec![Value::text("y"), Value::text("x"), Value::Number(2.0)],
        ];
        let names = ["A", "B", "N"].iter().map(|n| n.to_string()).collect();
        let cache = PivotCache::from_records("", s, names, rows, |v| match v {
            Value::Text(t) => t.to_string(),
            _ => String::new(),
        });
        let pv = wb.pivots_mut();
        pv.caches.push(cache);
        let mut t = PivotTable::new("PivotTable1", 0, 3, s, 0, 0);
        t.set_orient(0, Orient::Row);
        t.set_orient(1, Orient::Column);
        t.fields[2].orient = Orient::Data;
        t.add_data(DataField {
            src: 2,
            caption: "Sum of N".into(),
            func: Func::Sum,
            numfmt: Some("General".into()),
            show: Default::default(),
        });
        pv.tables.push(t);
        wb.pivot_update(0).unwrap();

        assert_eq!(name(&wb, 0, "x"), err(XlError::NA));

        assert_eq!(name(&wb, 0, "A[x]"), num(1.0));
        assert_eq!(name(&wb, 0, "A[x] B[y]"), num(1.0));

        assert_eq!(name(&wb, 0, "A x"), err(XlError::NA));

        assert_eq!(name(&wb, 0, "B"), err(XlError::REF));
    }

    #[test]
    fn name_syntax_field_off_the_axes_selects_nothing() {
        let (mut wb, ti) = fixture(false);

        wb.pivots_mut().tables[ti].set_orient(1, Orient::Hidden);
        wb.pivot_update(ti).unwrap();
        assert_eq!(name(&wb, ti, "East"), num(30.0));
        assert_eq!(name(&wb, ti, "Product[Plum]"), err(XlError::REF));
        assert_eq!(name(&wb, ti, "East Plum"), err(XlError::REF));
    }
}
