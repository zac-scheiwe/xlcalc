use crate::workbook::Command;
use std::sync::Arc;

use rustc_hash::FxHashMap;

use crate::address::{CellAddr, Rect, SheetId};
use crate::calc::{self, RecalcReport};
use crate::formula::{CRef, Node, Rc, SheetRef};
use crate::functions;
use crate::numfmt::{
    digits_with_date, format_key, quote_ampm, readback_spelling, spelled_for_parse,
    typed_elapsed_ok, NumberFormat,
};
use crate::parser::ast::{BinOp, UnaryOp};
use crate::style::Style;
use crate::value::{Array, Value};
use crate::workbook::{CalcMode, Entry, Workbook};

pub(crate) type Sty = Arc<Style>;

#[derive(Default)]
pub struct CellFormats {
    pub(crate) map: FxHashMap<(SheetId, u32, u32), Sty>,

    pub(crate) sticky: FxHashMap<SheetId, Rect>,

    pub(crate) rows: FxHashMap<(SheetId, u32), (u64, Sty)>,
    pub(crate) cols: FxHashMap<(SheetId, u32), (u64, Sty)>,
    pub(crate) sheets: FxHashMap<SheetId, (u64, Sty)>,
    seq: u64,
    compiled: FxHashMap<Arc<str>, Option<Arc<NumberFormat>>>,

    registry: FxHashMap<String, String>,

    ampm_upper: bool,
    pub(crate) precision: bool,

    late: FxHashMap<usize, (std::sync::Weak<Style>, Option<Arc<str>>)>,

    base: Sty,
    interned: rustc_hash::FxHashSet<Sty>,

    xf_order: Vec<Sty>,
}

fn key_of(s: &Sty) -> usize {
    Arc::as_ptr(s) as usize
}

impl CellFormats {
    pub(crate) fn any(&self) -> bool {
        !(self.map.is_empty()
            && self.rows.is_empty()
            && self.cols.is_empty()
            && self.sheets.is_empty())
    }

    pub(crate) fn import_sheet(&mut self, other: &CellFormats, from: SheetId, to: SheetId) {
        for (k, v) in other.map.iter().filter(|(k, _)| k.0 == from) {
            let s = self.intern((**v).clone());
            self.map.insert((to, k.1, k.2), s);
        }
        if let Some(r) = other.sticky.get(&from) {
            self.sticky.insert(to, *r);
        }
        let mut lines: Vec<(u64, u8, u32, Sty)> = Vec::new();
        for (k, (n, v)) in other.rows.iter().filter(|(k, _)| k.0 == from) {
            lines.push((*n, 0, k.1, v.clone()));
        }
        for (k, (n, v)) in other.cols.iter().filter(|(k, _)| k.0 == from) {
            lines.push((*n, 1, k.1, v.clone()));
        }
        if let Some((n, v)) = other.sheets.get(&from) {
            lines.push((*n, 2, 0, v.clone()));
        }
        lines.sort_by_key(|l| (l.0, l.1, l.2));
        for (_, kind, i, v) in lines {
            let s = self.intern((*v).clone());
            self.seq += 1;
            let stamp = (self.seq, s);
            match kind {
                0 => {
                    self.rows.insert((to, i), stamp);
                }
                1 => {
                    self.cols.insert((to, i), stamp);
                }
                _ => {
                    self.sheets.insert(to, stamp);
                }
            }
        }
    }

    pub(crate) fn borderless(&mut self, s: Sty) -> Sty {
        if s.borders == crate::style::Borders::default() {
            return s;
        }
        let mut b = (*s).clone();
        b.borders = Default::default();
        self.intern(b)
    }

    pub(crate) fn intern(&mut self, s: Style) -> Sty {
        if let Some(x) = self.interned.get(&s) {
            return x.clone();
        }
        let x = Arc::new(s);
        self.interned.insert(x.clone());
        self.xf_order.push(x.clone());
        x
    }

    pub(crate) fn base(&self) -> &Sty {
        &self.base
    }

    pub(crate) fn set_base(&mut self, s: Style) {
        self.base = self.intern(s);
    }

    pub(crate) fn line(&self, a: CellAddr) -> Option<&Sty> {
        let r = self.rows.get(&(a.sheet, a.row));
        let c = self.cols.get(&(a.sheet, a.col));
        let s = self.sheets.get(&a.sheet);
        [r, c, s]
            .into_iter()
            .flatten()
            .max_by_key(|(seq, _)| *seq)
            .map(|(_, f)| f)
    }

    pub(crate) fn get(&self, a: CellAddr) -> Option<&Sty> {
        self.map
            .get(&(a.sheet, a.row, a.col))
            .or_else(|| self.line(a))
    }

    pub(crate) fn style(&self, a: CellAddr) -> &Sty {
        self.get(a).unwrap_or(&self.base)
    }

    pub(crate) fn fmt(&self, a: CellAddr) -> Option<&Arc<str>> {
        self.get(a).map(|s| &s.number_format)
    }

    pub(crate) fn is_late(&self, f: &Sty) -> bool {
        self.late.contains_key(&key_of(f))
    }

    pub(crate) fn counted(&self, a: CellAddr) -> Option<Arc<str>> {
        let f = self.get(a)?;
        match self.late.get(&key_of(f)) {
            Some((_, prior)) => prior.clone(),
            None => Some(f.number_format.clone()),
        }
    }

    fn register_stored(&mut self, code: &str) {
        if code.contains("AM/PM") {
            self.ampm_upper = true;
        }
        if code != "General" {
            self.registry
                .entry(format_key(code).to_ascii_lowercase())
                .or_insert_with(|| code.to_string());
        }
    }

    fn registered(&mut self, typed: &str, spelled: String) -> String {
        let lower = format_key(&spelled).to_ascii_lowercase();

        let untagged = lower.strip_prefix("[$-f400]").unwrap_or(&lower);
        let built_in_ampm = matches!(untagged, "h:mm am/pm" | "h:mm:ss am/pm");
        let spelled = if self.ampm_upper || built_in_ampm {
            spelled.replace("am/pm", "AM/PM")
        } else {
            spelled
        };
        if spelled == "General" {
            return spelled;
        }
        let mut key = registry_key(&spelled);
        key.push_str(&escaped_letters(typed));
        if let Some(b) = BUILT_IN_PAIRS
            .iter()
            .find(|b| format_key(b).to_ascii_lowercase() == key)
        {
            return (*b).to_string();
        }
        self.registry.entry(key).or_insert(spelled).clone()
    }

    fn compile(&mut self, f: &Arc<str>) -> Option<Arc<NumberFormat>> {
        if let Some(c) = self.compiled.get(f) {
            return c.clone();
        }
        let c = NumberFormat::parse(f)
            .or_else(|e| {
                if ampm_literal_allowed(f) {
                    NumberFormat::parse(&quote_ampm(f))
                } else {
                    Err(e)
                }
            })
            .ok()
            .map(Arc::new);
        self.compiled.insert(f.clone(), c.clone());
        c
    }
}

fn escaped_letters(typed: &str) -> String {
    let mut out = String::new();
    let mut it = typed.chars();
    while let Some(c) = it.next() {
        if c == '\\' {
            out.extend(it.next().filter(char::is_ascii_alphabetic));
        }
    }
    out
}

fn registry_key(spelled: &str) -> String {
    let key = format_key(spelled);
    let mut out = String::with_capacity(key.len());
    let mut escaped = false;
    for c in key.chars() {
        out.push(if escaped { c } else { c.to_ascii_lowercase() });
        escaped = !escaped && c == '\\';
    }
    out
}

const BUILT_IN_PAIRS: [&str; 4] = [
    "#,##0;-#,##0",
    "#,##0;[Red]-#,##0",
    "#,##0.00;-#,##0.00",
    "#,##0.00;[Red]-#,##0.00",
];

const YIELDING: [&str; 16] = [
    "0%",
    "0.00%",
    "0.00E+00",
    "# ?/?",
    "# ??/??",
    "d-mmm-yy",
    "d-mmm",
    "mmm-yy",
    "h:mm AM/PM",
    "h:mm:ss AM/PM",
    "h:mm",
    "h:mm:ss",
    "mm:ss",
    "[h]:mm:ss",
    "mm:ss.0",
    "##0.0E+0",
];

#[derive(Clone, Copy, PartialEq, Eq)]
enum FormatClass {
    Percent,
    Scientific,
    Fraction,
    Currency,
    Date,
    Time,
    Other,
}

fn pos(x: Rc, host: u32) -> i64 {
    if x.abs {
        x.v as i64
    } else {
        host as i64 + x.v as i64
    }
}

impl Workbook {
    pub fn number_format(&self, addr: CellAddr) -> &str {
        &self.cellfmt.style(addr).number_format
    }

    pub fn cell_style(&self, addr: CellAddr) -> &Style {
        self.cellfmt.style(addr)
    }

    pub fn own_style(&self, addr: CellAddr) -> Option<&Style> {
        self.cellfmt
            .map
            .get(&(addr.sheet, addr.row, addr.col))
            .map(|s| &**s)
    }

    pub fn xf_table(&self) -> Vec<&Style> {
        let base = self.cellfmt.base();
        let same = |a: &Style, b: &Style| {
            Style {
                custom_code: b.custom_code,
                implicit_general: b.implicit_general,
                ..a.clone()
            } == *b
        };
        let mut out: Vec<&Style> = Vec::new();
        for s in &self.cellfmt.xf_order {
            if !same(s, base) && !out.iter().any(|o| same(s, o)) {
                out.push(s);
            }
        }
        out
    }

    pub fn formatted_cells(&self, sheet: SheetId) -> Vec<(u32, u32)> {
        let mut v: Vec<(u32, u32)> = self
            .cellfmt
            .map
            .keys()
            .filter(|k| k.0 == sheet)
            .map(|k| (k.1, k.2))
            .collect();
        v.sort_unstable();
        v
    }

    pub fn has_line_formats(&self, sheet: SheetId) -> bool {
        let f = &self.cellfmt;
        f.sheets.contains_key(&sheet)
            || f.rows.keys().any(|k| k.0 == sheet)
            || f.cols.keys().any(|k| k.0 == sheet)
    }

    pub fn debug_format(&self, addr: CellAddr) -> String {
        match self.cellfmt.get(addr) {
            None => "General".into(),
            Some(f) => format!(
                "{}@{:x}{}",
                f.number_format,
                key_of(f) & 0xffff,
                if self.cellfmt.is_late(f) { " LATE" } else { "" }
            ),
        }
    }

    pub(crate) fn cellfmt_has(&self, addr: CellAddr) -> bool {
        self.cellfmt
            .fmt(addr)
            .is_some_and(|f| !f.eq_ignore_ascii_case("general"))
    }

    pub fn note_number_format(&mut self, fmt: Option<&str>) {
        self.session.last_format = fmt.map(str::to_string);
    }

    pub fn note_cell_format(&mut self, addr: CellAddr) {
        let f = self.number_format(addr);
        if !f.eq_ignore_ascii_case("general") {
            self.session.last_format = Some(f.to_string());
        }
    }

    pub(crate) fn subtotal_label_format(&mut self, addr: CellAddr) {
        let Some(f) = self.session.last_format.take() else {
            return;
        };
        let cur = self.number_format(addr);
        if !cur.eq_ignore_ascii_case(&f)
            && (cur.eq_ignore_ascii_case("general") || self.auto_replaces(cur, &f))
        {
            self.store_format(addr, &f);
        }
    }

    pub(crate) fn auto_replaces(&self, cur: &str, new: &str) -> bool {
        let short = self
            .input_env()
            .locale
            .short_date_format
            .to_ascii_lowercase();
        let cur_sym = self.input_env().locale.currency.to_ascii_lowercase();
        let kind = |f: &str| -> Option<u8> {
            let f = f.to_ascii_lowercase();
            let f = f.as_str();
            let c = cur_sym.as_str();
            if f == format!("{c}#,##0;-{c}#,##0")
                || f == format!("{c}#,##0;[red]-{c}#,##0")
                || f == format!("{c}#,##0.00;-{c}#,##0.00")
                || f == format!("{c}#,##0.00;[red]-{c}#,##0.00")
            {
                Some(6)
            } else if f == "0%" || f == "0.00%" {
                Some(1)
            } else if f == "0.00e+00" || f == "##0.0e+0" {
                Some(2)
            } else if f == "# ?/?" || f == "# ??/??" {
                Some(3)
            } else if f == short
                || f == "d-mmm-yy"
                || f == "d-mmm"
                || f == "mmm-yy"
                || f == format!("{short} h:mm")
            {
                Some(4)
            } else if matches!(
                f,
                "h:mm am/pm"
                    | "h:mm:ss am/pm"
                    | "h:mm"
                    | "h:mm:ss"
                    | "mm:ss"
                    | "[h]:mm:ss"
                    | "mm:ss.0"
            ) {
                Some(5)
            } else {
                None
            }
        };
        match kind(cur) {
            None => cur.eq_ignore_ascii_case("general"),
            Some(k) => {
                let mut plain = String::new();
                let mut quoted = false;
                for ch in new.to_ascii_lowercase().chars() {
                    match ch {
                        '"' => quoted = !quoted,
                        _ if !quoted => plain.push(ch),
                        _ => {}
                    }
                }
                let loose = if plain.contains(cur_sym.as_str()) {
                    6
                } else if plain.contains('%') {
                    1
                } else if plain.contains("e+") {
                    2
                } else if plain.contains("?/") {
                    3
                } else if plain.contains('y') || plain.contains('d') {
                    4
                } else if plain.contains('h') || plain.contains("ss") {
                    5
                } else {
                    0
                };
                loose != 0 && loose != k
            }
        }
    }

    pub(crate) fn is_text_format(&self, addr: CellAddr) -> bool {
        self.cellfmt
            .fmt(addr)
            .is_some_and(|f| is_text_format_code(f))
    }

    pub(crate) fn yielding_format(&self, cur: &str) -> bool {
        if YIELDING.iter().any(|y| y.eq_ignore_ascii_case(cur)) {
            return true;
        }
        let loc = self.input_env().locale;
        let (short, c) = (&loc.short_date_format, &loc.currency);
        [
            short.clone(),
            format!("{short} h:mm"),
            format!("{c}#,##0;[Red]-{c}#,##0"),
            format!("{c}#,##0.00;[Red]-{c}#,##0.00"),
            format!("{c}#,##0;-{c}#,##0"),
            format!("{c}#,##0.00;-{c}#,##0.00"),
        ]
        .iter()
        .any(|y| y.eq_ignore_ascii_case(cur))
    }

    pub(crate) fn fraction_first(&mut self, addr: CellAddr) -> bool {
        let Some(f) = self.cellfmt.fmt(addr).cloned() else {
            return false;
        };
        if f.split(';')
            .all(|p| p.trim().eq_ignore_ascii_case("general"))
            || &*f == "@"
        {
            return false;
        }
        self.cellfmt.compile(&f).is_some_and(|c| !c.is_date())
    }

    pub(crate) fn date_or_text_format(&mut self, addr: CellAddr, or_text: bool) -> bool {
        let Some(f) = self.cellfmt.fmt(addr).cloned() else {
            return false;
        };
        (or_text && &*f == "@") || self.cellfmt.compile(&f).is_some_and(|c| c.is_date())
    }

    pub fn precision_as_displayed(&self) -> bool {
        self.cellfmt.precision
    }

    pub(crate) fn own_format(&self, a: CellAddr) -> Option<Sty> {
        self.cellfmt.map.get(&(a.sheet, a.row, a.col)).cloned()
    }

    pub(crate) fn restore_own_format(&mut self, a: CellAddr, f: Option<Sty>) {
        let key = (a.sheet, a.row, a.col);
        match f {
            Some(f) => self.cellfmt.map.insert(key, f),
            None => self.cellfmt.map.remove(&key),
        };
    }

    pub(crate) fn store_format(&mut self, addr: CellAddr, fmt: &str) {
        let stored = stored_format(fmt);
        self.cellfmt.register_stored(&stored);
        let s = self.cellfmt.style(addr).with_number_format(&stored);
        self.store_style(addr, s);
    }

    pub(crate) fn store_style(&mut self, addr: CellAddr, s: Style) {
        let key = (addr.sheet, addr.row, addr.col);
        let line = self.cellfmt.line(addr).unwrap_or(&self.cellfmt.base);
        if **line == s {
            self.cellfmt.map.remove(&key);
        } else {
            let s = self.cellfmt.intern(s);
            self.cellfmt.map.insert(key, s);
            let cell = Rect::new(addr.row, addr.col, addr.row, addr.col);
            self.cellfmt
                .sticky
                .entry(addr.sheet)
                .and_modify(|b| *b = b.union_bounds(&cell))
                .or_insert(cell);
        }
    }

    fn line_crossings(
        &self,
        sheet: SheetId,
        rect: Rect,
        f: &dyn Fn(&Style) -> Style,
    ) -> Vec<(CellAddr, Style)> {
        let cf = &self.cellfmt;
        let cols = rect.is_full_columns();
        let (other, (a0, a1)) = if cols {
            (&cf.rows, (rect.c0, rect.c1))
        } else {
            (&cf.cols, (rect.r0, rect.r1))
        };
        let mut out = Vec::new();
        for (k, _) in other.iter().filter(|(k, _)| k.0 == sheet) {
            for i in a0..=a1 {
                let a = if cols {
                    CellAddr::new(sheet, k.1, i)
                } else {
                    CellAddr::new(sheet, i, k.1)
                };
                if cf.map.contains_key(&(sheet, a.row, a.col)) {
                    continue;
                }
                let shown = cf.style(a);
                let own = if cols {
                    cf.cols.get(&(sheet, i))
                } else {
                    cf.rows.get(&(sheet, i))
                };

                if own.is_some_and(|o| Arc::ptr_eq(&o.1, shown)) {
                    continue;
                }
                out.push((a, f(shown)));
            }
        }
        out
    }

    pub(crate) fn apply_style_with(
        &mut self,
        sheet: SheetId,
        rect: Rect,
        f: &dyn Fn(&Style) -> Style,
    ) {
        if rect.is_full_columns() || rect.is_full_rows() {
            let whole = rect.is_full_columns() && rect.is_full_rows();
            let inside =
                |k: &(SheetId, u32, u32)| k.0 == sheet && (whole || rect.contains(k.1, k.2));
            let cells: Vec<_> = self
                .cellfmt
                .map
                .iter()
                .filter(|(k, _)| inside(k))
                .map(|(k, v)| (*k, v.clone()))
                .collect();
            let crossings = if whole {
                Vec::new()
            } else {
                self.line_crossings(sheet, rect, f)
            };
            let cf = &mut self.cellfmt;
            let base = cf.base.clone();
            let cols = rect.is_full_columns();
            if whole {
                let old = cf.sheets.get(&sheet).cloned();
                let cur: Sty = old.as_ref().map_or(base, |o| o.1.clone());
                let new = cf.intern(f(&cur));
                let lines: Vec<_> = cf
                    .rows
                    .iter()
                    .filter(|(k, _)| k.0 == sheet)
                    .map(|(k, v)| (true, k.1, v.clone()))
                    .chain(
                        cf.cols
                            .iter()
                            .filter(|(k, _)| k.0 == sheet)
                            .map(|(k, v)| (false, k.1, v.clone())),
                    )
                    .collect();
                for (row, i, (q, v)) in lines {
                    let nv = f(&v);
                    let nv = (nv != *new).then(|| cf.intern(nv));
                    let m = if row { &mut cf.rows } else { &mut cf.cols };
                    match nv {
                        Some(nv) => m.insert((sheet, i), (q, nv)),
                        None => m.remove(&(sheet, i)),
                    };
                }
                let seq = match old {
                    Some((q, _)) => q,
                    None => {
                        cf.seq += 1;
                        cf.seq
                    }
                };
                cf.sheets.insert(sheet, (seq, new));
            } else {
                let (a0, a1) = if cols {
                    (rect.c0, rect.c1)
                } else {
                    (rect.r0, rect.r1)
                };
                cf.seq += 1;
                let seq = cf.seq;
                for i in a0..=a1 {
                    let m = if cols { &cf.cols } else { &cf.rows };
                    let cur = m
                        .get(&(sheet, i))
                        .or_else(|| cf.sheets.get(&sheet))
                        .map_or(base.clone(), |(_, v)| v.clone());
                    let new = cf.intern(f(&cur));
                    let m = if cols { &mut cf.cols } else { &mut cf.rows };
                    m.insert((sheet, i), (seq, new));
                }
            }
            for (a, s) in crossings {
                self.store_style(a, s);
            }
            for (k, v) in cells {
                let s = f(&v);
                self.store_style(CellAddr::new(k.0, k.1, k.2), s);
            }
            return;
        }
        if rect.rows() as u64 * rect.cols() as u64 > 1 << 20 {
            return;
        }
        for r in rect.r0..=rect.r1 {
            for c in rect.c0..=rect.c1 {
                let a = CellAddr::new(sheet, r, c);
                let s = f(self.cellfmt.style(a));
                self.store_style(a, s);
            }
        }
    }

    fn apply_format(&mut self, sheet: SheetId, rect: Rect, fmt: &str) {
        let spelled = norm_format(fmt);

        let implicit = !fmt.contains(';') && spelled.ends_with(";General");
        let fmt = self.cellfmt.registered(fmt, spelled);
        self.apply_style_with(sheet, rect, &|s: &Style| Style {
            implicit_general: implicit,
            ..s.with_number_format(&fmt)
        });
    }

    pub fn set_number_format_stored(&mut self, sheet: SheetId, rect: Rect, code: &str) {
        let fmt = stored_format(code);
        self.cellfmt.register_stored(&fmt);
        self.command(Command::Format, |wb| {
            wb.apply_style_with(sheet, rect, &|s: &Style| s.with_number_format(&fmt));
            RecalcReport::default()
        });
    }

    pub fn clear_formats(&mut self, sheet: SheetId, rect: Rect) -> RecalcReport {
        self.command(Command::ClearFormats, |wb| {
            wb.note_changed(sheet, &[rect]);
            wb.clear_formats_run(sheet, rect)
        })
    }

    fn clear_formats_run(&mut self, sheet: SheetId, rect: Rect) -> RecalcReport {
        self.reset_style(sheet, rect);
        let r = self.unmerge_cells(sheet, rect);

        self.notify_cell_change(sheet);
        r
    }

    fn reset_style(&mut self, sheet: SheetId, rect: Rect) {
        if self.sheet(sheet).is_none() {
            return;
        }
        let before = self.late_candidates(sheet, rect, false);
        let base = (**self.cellfmt.base()).clone();
        self.apply_style_with(sheet, rect, &|_: &Style| base.clone());
        self.note_late_formats(sheet, before);
        self.note_table_col_formats(sheet, rect, "General");
    }

    pub fn set_style_with(&mut self, sheet: SheetId, rect: Rect, f: &dyn Fn(&Style) -> Style) {
        self.command(Command::Format, |wb| wb.set_style_with_run(sheet, rect, f))
    }

    fn set_style_with_run(&mut self, sheet: SheetId, rect: Rect, f: &dyn Fn(&Style) -> Style) {
        if self.sheet(sheet).is_none() {
            return;
        }

        let late = self.late_cells(sheet, rect);
        self.apply_style_with(sheet, rect, f);
        self.note_late_formats(sheet, late);
    }

    pub fn set_cell_style(&mut self, sheet: SheetId, rect: Rect, style: &Style) -> RecalcReport {
        self.command(Command::Format, |wb| {
            wb.set_cell_style_run(sheet, rect, style)
        })
    }

    fn set_cell_style_run(&mut self, sheet: SheetId, rect: Rect, style: &Style) -> RecalcReport {
        let fmt = style.number_format.clone();
        let rest = style.clone();
        self.set_style_with(sheet, rect, &|s: &Style| Style {
            number_format: s.number_format.clone(),
            ..rest.clone()
        });
        self.set_number_format_body(sheet, rect, &fmt)
    }

    pub fn shown_border(&self, addr: CellAddr, edge: crate::style::Edge) -> crate::style::Border {
        use crate::style::Edge;
        let own = self.cell_style(addr).borders.edge(edge);
        let (r, c) = (i64::from(addr.row), i64::from(addr.col));
        let (nr, nc) = match edge {
            Edge::Left => (r, c - 1),
            Edge::Right => (r, c + 1),
            Edge::Top => (r - 1, c),
            Edge::Bottom => (r + 1, c),
        };
        let (Ok(nr), Ok(nc)) = (u32::try_from(nr), u32::try_from(nc)) else {
            return own;
        };
        if nr >= crate::address::MAX_ROWS || nc >= crate::address::MAX_COLS {
            return own;
        }
        let other = self
            .cell_style(CellAddr::new(addr.sheet, nr, nc))
            .borders
            .edge(edge.opposite());
        if other.line.weight_rank() > own.line.weight_rank() {
            other
        } else {
            own
        }
    }

    pub fn style_table(&self) -> &crate::style::StyleTable {
        &self.styles
    }

    pub fn style_table_mut(&mut self) -> &mut crate::style::StyleTable {
        &mut self.styles
    }

    pub fn base_style(&self) -> &Style {
        self.cellfmt.base()
    }

    pub fn set_base_style(&mut self, s: Style) {
        self.cellfmt.set_base(s);
    }

    pub fn reset_number_format(&mut self, sheet: SheetId, rect: Rect) {
        self.command(Command::Format, |wb| {
            wb.reset_number_format_run(sheet, rect)
        })
    }

    fn reset_number_format_run(&mut self, sheet: SheetId, rect: Rect) {
        if let Some(runs) = self.view_visible_runs(sheet, rect) {
            for run in runs {
                self.reset_number_format_run(sheet, run);
            }
            return;
        }
        self.reset_style(sheet, rect);
        let mode = std::mem::replace(&mut self.settings.mode, CalcMode::Manual);
        self.unmerge_cells(sheet, rect);
        self.settings.mode = mode;
    }

    pub(crate) fn set_number_format_unrounded(&mut self, sheet: SheetId, rect: Rect, fmt: &str) {
        if self.sheet(sheet).is_none() {
            return;
        }
        let before = self.late_candidates(sheet, rect, false);
        self.apply_format(sheet, rect, fmt);
        self.note_late_formats(sheet, before);
        self.note_table_col_formats(sheet, rect, fmt);
    }

    pub fn set_number_format(&mut self, sheet: SheetId, rect: Rect, fmt: &str) -> RecalcReport {
        self.command(Command::Format, |wb| {
            wb.set_number_format_run(sheet, rect, fmt)
        })
    }

    fn set_number_format_run(&mut self, sheet: SheetId, rect: Rect, fmt: &str) -> RecalcReport {
        let r = self.set_number_format_body(sheet, rect, fmt);
        if self.sheet(sheet).is_some() && !(rect.r0 == 0 && rect.r1 + 1 >= crate::address::MAX_ROWS)
        {
            self.view_touch_row_pub(sheet, rect.r1);
        }
        r
    }

    pub fn mark_custom_code(&mut self, sheet: SheetId, rect: Rect, fmt: &str, host_spelling: bool) {
        let short = self.input_env().locale.short_date_format.clone();
        let is_short = fmt.eq_ignore_ascii_case(&short)
            || fmt
                .strip_suffix(" h:mm")
                .is_some_and(|d| d.eq_ignore_ascii_case(&short));
        if (is_short || host_spelling) && self.sheet(sheet).is_some() {
            self.apply_style_with(sheet, rect, &|s: &Style| Style {
                custom_code: true,
                ..s.clone()
            });
        }
    }

    fn set_number_format_body(&mut self, sheet: SheetId, rect: Rect, fmt: &str) -> RecalcReport {
        if self.sheet(sheet).is_none() || format_refused(fmt) {
            return RecalcReport::default();
        }
        let before = self.late_candidates(sheet, rect, true);
        self.apply_format(sheet, rect, fmt);
        self.note_late_formats(sheet, before);
        self.note_table_col_formats(sheet, rect, fmt);
        if !self.cellfmt.precision {
            return RecalcReport::default();
        }
        self.round_range(sheet, rect)
    }

    pub(crate) fn late_candidates(
        &self,
        sheet: SheetId,
        rect: Rect,
        header: bool,
    ) -> Vec<(u32, u32, Option<Arc<str>>)> {
        let mut out = Vec::new();
        for t in self.tables.iter().filter(|t| t.sheet == sheet) {
            let (d0, d1) = t.data_rows();
            let top = if header && t.headers { t.rect.r0 } else { d0 };
            let Some(b) = rect.intersection(&Rect::new(top, t.rect.c0, d1, t.rect.c1)) else {
                continue;
            };
            if b.rows() as u64 * b.cols() as u64 > 1 << 16 {
                continue;
            }
            for r in b.r0..=b.r1 {
                for c in b.c0..=b.c1 {
                    let prior = self
                        .cellfmt
                        .counted(CellAddr::new(sheet, r, c))
                        .filter(|f| !f.eq_ignore_ascii_case("general"));
                    out.push((r, c, prior));
                }
            }
        }
        out
    }

    fn late_cells(&self, sheet: SheetId, rect: Rect) -> Vec<(u32, u32, Option<Arc<str>>)> {
        let cf = &self.cellfmt;
        if cf.late.is_empty() {
            return Vec::new();
        }
        cf.map
            .iter()
            .filter(|(k, v)| k.0 == sheet && rect.contains(k.1, k.2) && cf.is_late(v))
            .map(|(k, _)| (k.1, k.2, cf.counted(CellAddr::new(sheet, k.1, k.2))))
            .collect()
    }

    pub(crate) fn note_late_formats(
        &mut self,
        sheet: SheetId,
        cells: Vec<(u32, u32, Option<Arc<str>>)>,
    ) {
        for (r, c, prior) in cells {
            let f = match self.cellfmt.map.get(&(sheet, r, c)) {
                Some(f) => f.clone(),
                None if prior.is_some() => self.cellfmt.style(CellAddr::new(sheet, r, c)).clone(),
                None => continue,
            };
            let own: Sty = Arc::new((*f).clone());
            self.cellfmt
                .late
                .insert(key_of(&own), (Arc::downgrade(&own), prior));
            self.cellfmt.map.insert((sheet, r, c), own);
        }
    }

    fn round_range(&mut self, sheet: SheetId, rect: Rect) -> RecalcReport {
        let sh = self.sheet(sheet).unwrap();
        let mut consts = Vec::new();
        let mut formulas = Vec::new();
        for (r, c, e) in sh.cells.iter_rect(rect) {
            match e {
                Entry::Const(Value::Number(n)) => consts.push((CellAddr::new(sheet, r, c), *n)),
                Entry::Formula(fid) => formulas.push(*fid),
                _ => {}
            }
        }
        for (fid, _) in sh.spills.intersecting(&rect) {
            formulas.push(fid);
        }
        let mode = self.settings.mode;
        self.settings.mode = CalcMode::Manual;
        for (a, n) in consts {
            let v = self.precision_value(a, n);
            if v != n {
                self.set_value(a, Value::Number(v));
            }
        }
        for fid in formulas {
            calc::mark_dirty(self, fid);
        }
        self.settings.mode = mode;
        if mode == CalcMode::Automatic {
            calc::recalc(self)
        } else {
            RecalcReport::default()
        }
    }

    pub fn set_precision_as_displayed(&mut self, on: bool) -> RecalcReport {
        self.command(Command::Setting, |wb| wb.set_precision_as_displayed_run(on))
    }

    fn set_precision_as_displayed_run(&mut self, on: bool) -> RecalcReport {
        self.cellfmt.precision = on;

        let ids: Vec<SheetId> = self.sheets().map(|s| s.id).collect();
        let mut consts = Vec::new();
        for s in ids.into_iter().filter(|_| on) {
            let sh = self.sheet(s).unwrap();
            for (r, c, e) in sh.cells.iter() {
                if let Entry::Const(Value::Number(n)) = e {
                    consts.push((CellAddr::new(s, r, c), *n));
                }
            }
        }
        let mode = self.settings.mode;
        self.settings.mode = CalcMode::Manual;
        for (a, n) in consts {
            let v = self.precision_value(a, n);
            if v != n {
                self.set_value(a, Value::Number(v));
            }
        }
        let fids: Vec<_> = self.formula_ids().collect();
        for fid in fids {
            if on {
                if let Value::Number(n) = self.formula(fid).value {
                    let a = self.formula(fid).addr;
                    let v = self.precision_value(a, n);
                    self.formula_mut(fid).value = Value::Number(v);
                }
            }
            calc::mark_dirty(self, fid);
        }
        self.settings.mode = mode;
        self.recalc_point()
    }

    pub(crate) fn compiled_format(&mut self, addr: CellAddr) -> Option<Arc<NumberFormat>> {
        let f = self.cellfmt.style(addr).number_format.clone();
        self.cellfmt.compile(&f)
    }

    pub(crate) fn precision_value(&mut self, addr: CellAddr, n: f64) -> f64 {
        match self.compiled_format(addr) {
            Some(f) => f.displayed_value(n),
            None => n,
        }
    }

    pub(crate) fn set_value_exact(&mut self, addr: CellAddr, v: Value) -> RecalcReport {
        let pad = std::mem::replace(&mut self.cellfmt.precision, false);
        let r = self.set_value(addr, v);
        self.cellfmt.precision = pad;
        r
    }

    pub(crate) fn precision_const(&mut self, addr: CellAddr, v: Value) -> Value {
        match v {
            Value::Number(n) if self.cellfmt.precision => {
                Value::Number(self.precision_value(addr, n))
            }
            v => v,
        }
    }

    pub(crate) fn precision_result(&mut self, addr: CellAddr, v: Value) -> Value {
        match v {
            Value::Number(n) => Value::Number(self.precision_value(addr, n)),
            Value::Array(a) => {
                let mut data = a.data.clone();
                for (i, x) in data.iter_mut().enumerate() {
                    if let Value::Number(n) = x {
                        let (r, c) = (i as u32 / a.cols, i as u32 % a.cols);
                        let at = CellAddr::new(addr.sheet, addr.row + r, addr.col + c);
                        *n = self.precision_value(at, *n);
                    }
                }
                Value::Array(Arc::new(Array::new(a.rows, a.cols, data)))
            }
            v => v,
        }
    }

    pub(crate) fn note_typed_format(&mut self, addr: CellAddr, input: &str) {
        let Some(typed) = self.typed_format(input) else {
            return;
        };

        let text_ttc = self.op_on(crate::workbook::Op::TextToColumns)
            && self.number_format(addr).eq_ignore_ascii_case("@");
        if self.cellfmt_has(addr) && !text_ttc {
            let cur = self.number_format(addr).to_string();
            let yields = self.yielding_format(&cur);

            let general = typed.eq_ignore_ascii_case("General");
            let (a, b) = (self.format_class(&cur), self.format_class(&typed));
            if !yields || (!general && (a == b || b == FormatClass::Other)) {
                return;
            }
        }
        self.store_format(addr, &typed);
    }

    pub(crate) fn wrap_for_line_break(&mut self, addr: CellAddr) {
        let breaks = matches!(self.value(addr), Value::Text(t) if t.contains('\n'));
        if !breaks || self.formula_text(addr).is_some() || self.cell_style(addr).alignment.wrap {
            return;
        }
        let r = Rect::cell(addr.row, addr.col);
        self.apply_style_with(addr.sheet, r, &|s: &Style| {
            let mut s = s.clone();
            s.alignment.wrap = true;
            s
        });
    }

    fn format_class(&mut self, f: &str) -> FormatClass {
        if f.contains("E+") || f.contains("E-") {
            return FormatClass::Scientific;
        }
        if f.ends_with('%') {
            return FormatClass::Percent;
        }
        if f.contains('$') {
            return FormatClass::Currency;
        }
        if f.contains('?') && f.contains('/') {
            return FormatClass::Fraction;
        }
        match self.cellfmt.compile(&Arc::from(f)) {
            Some(c) if c.has_calendar_date() => FormatClass::Date,
            Some(c) if c.is_date() => FormatClass::Time,
            _ => FormatClass::Other,
        }
    }

    fn ref_format(&self, sr: &SheetRef, cref: &CRef, host: CellAddr) -> Option<Arc<str>> {
        let sheet = match sr {
            SheetRef::Current | SheetRef::Own => host.sheet,
            SheetRef::Sheet(s) => *s,
            _ => return None,
        };
        let (r, c) = match *cref {
            CRef::Cell { row, col } => (pos(row, host.row), pos(col, host.col)),
            CRef::Area { r0, c0, r1, c1 } => (
                pos(r0, host.row).min(pos(r1, host.row)),
                pos(c0, host.col).min(pos(c1, host.col)),
            ),

            CRef::Rows { r0, r1 } => (pos(r0, host.row).min(pos(r1, host.row)), 0),
            CRef::Cols { c0, c1 } => (0, pos(c0, host.col).min(pos(c1, host.col))),
        };
        if r < 0 || c < 0 {
            return None;
        }
        let f = self
            .cellfmt
            .fmt(CellAddr::new(sheet, r as u32, c as u32))
            .cloned()?;

        if sheet != host.sheet && !self.is_builtin_format(&f) {
            return None;
        }
        Some(f)
    }

    fn is_builtin_format(&self, f: &str) -> bool {
        const FIXED: [&str; 16] = [
            "General",
            "0",
            "0.00",
            "#,##0",
            "#,##0.00",
            "0%",
            "0.00%",
            "0.00E+00",
            "# ?/?",
            "# ??/??",
            "d-mmm-yy",
            "d-mmm",
            "mmm-yy",
            "h:mm AM/PM",
            "h:mm:ss AM/PM",
            "h:mm",
        ];
        const MORE: [&str; 8] = [
            "h:mm:ss",
            "mm:ss",
            "[h]:mm:ss",
            "mm:ss.0",
            "##0.0E+0",
            "@",
            "#,##0 ;(#,##0)",
            "#,##0 ;[Red](#,##0)",
        ];
        if FIXED
            .iter()
            .chain(MORE.iter())
            .any(|y| y.eq_ignore_ascii_case(f))
        {
            return true;
        }
        let loc = self.input_env().locale;
        let (short, c) = (&loc.short_date_format, &loc.currency);
        [
            short.clone(),
            format!("{short} h:mm"),
            format!("{c}#,##0;[Red]-{c}#,##0"),
            format!("{c}#,##0.00;[Red]-{c}#,##0.00"),
            format!("{c}#,##0;-{c}#,##0"),
            format!("{c}#,##0.00;-{c}#,##0.00"),
            "#,##0 ;(#,##0)".into(),
            "#,##0.00;(#,##0.00)".into(),
            "#,##0.00;[Red](#,##0.00)".into(),
            "#,##0;-#,##0".into(),
            "#,##0.00;-#,##0.00".into(),
            "#,##0;[Red]-#,##0".into(),
            "#,##0.00;[Red]-#,##0.00".into(),
        ]
        .iter()
        .any(|y| y.eq_ignore_ascii_case(f))
    }

    pub(crate) fn infer_format(&mut self, n: &Node, host: CellAddr) -> Option<Arc<str>> {
        let mut reg = Reg::Unset;
        SUM_SEEN.with(|c| c.set(false));
        FN_DEPTH.with(|d| d.set(0));
        self.infer_walk(n, host, &mut reg);
        match reg {
            Reg::Fmt(f) if f.eq_ignore_ascii_case("h:mm:ss") && SUM_SEEN.with(|c| c.get()) => {
                Some(Arc::from("[h]:mm:ss"))
            }
            Reg::Fmt(f) => Some(f),
            _ => None,
        }
    }

    fn is_date_format(&mut self, f: &Option<Arc<str>>) -> bool {
        f.as_ref().is_some_and(|f| {
            self.cellfmt
                .compile(f)
                .is_some_and(|c| c.has_calendar_date())
        })
    }

    fn date_check(&mut self, l: &Option<Arc<str>>, r: &Option<Arc<str>>, reg: &mut Reg) {
        if let Reg::Fmt(cur) = reg {
            let cur = Some(cur.clone());
            if self.is_date_format(&cur) && self.is_date_format(l) && self.is_date_format(r) {
                *reg = Reg::Date;
            }
        }
    }

    fn infer_walk(&mut self, n: &Node, host: CellAddr, reg: &mut Reg) -> Option<Arc<str>> {
        crate::deep(|| self.infer_walk_inner(n, host, reg))
    }

    fn infer_walk_inner(&mut self, n: &Node, host: CellAddr, reg: &mut Reg) -> Option<Arc<str>> {
        match n {
            Node::Ref(sr, cref) => {
                let f = self.ref_format(sr, cref, host);
                if let (Some(f), Reg::Unset) = (&f, &*reg) {
                    *reg = Reg::Fmt(f.clone());
                }
                f
            }
            Node::Paren(x) | Node::Unary(UnaryOp::Neg | UnaryOp::Plus, x) => {
                self.infer_walk(x, host, reg)
            }
            Node::Ws { node, .. } => self.infer_walk(node, host, reg),
            Node::Binary(BinOp::Add | BinOp::Sub, a, b) => {
                let l = self.infer_walk(a, host, reg);
                let r = self.infer_walk(b, host, reg);
                self.date_check(&l, &r, reg);
                l.or(r)
            }
            Node::Binary(
                BinOp::Mul
                | BinOp::Div
                | BinOp::Pow
                | BinOp::Concat
                | BinOp::Eq
                | BinOp::Ne
                | BinOp::Lt
                | BinOp::Le
                | BinOp::Gt
                | BinOp::Ge,
                a,
                b,
            ) => {
                self.infer_walk(a, host, reg);
                self.infer_walk(b, host, reg);
                *reg = Reg::Hard;
                None
            }
            Node::Percent(x) => {
                self.infer_walk(x, host, reg);
                *reg = Reg::Hard;
                None
            }
            Node::Func { id, args } => {
                let own = |_: &Workbook, s: String| Some(Arc::from(s.as_str()));
                let env = self.input_env();
                let own_fmt: Option<Arc<str>> = match functions::def(*id).name {
                    "DATE" | "TODAY" => own(self, env.locale.short_date_format.clone()),
                    "NOW" => own(self, format!("{} h:mm", env.locale.short_date_format)),
                    "TIME" => own(self, "h:mm am/pm".into()),
                    "PMT" | "FV" | "PV" | "NPV" | "IPMT" | "PPMT" | "SLN" | "SYD" | "DB"
                    | "DDB" | "VDB" => {
                        let c = &env.locale.currency;
                        own(self, format!("{c}#,##0.00;[Red]-{c}#,##0.00"))
                    }
                    "IRR" | "MIRR" | "RATE" => own(self, "0%".into()),
                    "SUM" | "MIN" | "MAX" | "AVERAGE" | "MEDIAN" | "MOD" | "ROUND" | "ROUNDUP"
                    | "ROUNDDOWN" | "TRUNC" | "INT" => {
                        let mut lead: Option<Arc<str>> = None;

                        let outer = FN_DEPTH.with(|d| d.get()) == 0;
                        if outer && functions::def(*id).name == "SUM" {
                            SUM_SEEN.with(|c| c.set(true));
                        }
                        FN_DEPTH.with(|d| d.set(d.get() + 1));
                        for a in args {
                            let f = self.infer_walk(a, host, reg);
                            if lead.is_some() {
                                self.date_check(&lead, &f, reg);
                            }
                            lead = lead.or(f);
                        }
                        if matches!(reg, Reg::Hard) {
                            *reg = Reg::Unset;
                        }
                        FN_DEPTH.with(|d| d.set(d.get() - 1));
                        return lead;
                    }
                    _ => return None,
                };
                if let (Some(f), Reg::Unset) = (&own_fmt, &*reg) {
                    *reg = Reg::Fmt(f.clone());
                }
                own_fmt
            }
            _ => None,
        }
    }
}

thread_local! {
    static FN_DEPTH: std::cell::Cell<u32> = const { std::cell::Cell::new(0) };
    static SUM_SEEN: std::cell::Cell<bool> = const { std::cell::Cell::new(false) };
}

enum Reg {
    Unset,
    Fmt(Arc<str>),

    Hard,

    Date,
}

pub(crate) fn is_text_format_code(f: &str) -> bool {
    let secs = format_sections(f);
    let Some(text) = secs.last() else {
        return false;
    };
    if !text.contains('@') || secs.len() > 4 {
        return false;
    }
    let numeric = |s: &str| {
        let l = s
            .to_ascii_lowercase()
            .replace("general", "")
            .replace("am/pm", "")
            .replace("a/p", "");
        l.contains([
            '0', '#', '?', '%', 'd', 'm', 'y', 'h', 's', 'e', '@', 'b', 'g',
        ])
    };
    let n = secs.len();
    if n == 1 {
        let l = text.to_ascii_lowercase();
        return !l.contains(['0', '#', '?', '%', 'd', 'm', 'y', 'h', 's', 'e', 'b', 'g']);
    }
    secs[..n - 1].iter().all(|s| !numeric(s))
        && !text.to_ascii_lowercase().contains(['0', '#', '?'])
}

pub fn number_format_refused(fmt: &str) -> bool {
    format_refused(fmt)
}

fn format_refused(fmt: &str) -> bool {
    if fmt.is_empty() {
        return false;
    }
    if at_sign_rejected(&quote_ampm(fmt)) {
        return true;
    }
    let spelled = spelled_for_parse(fmt);
    !typed_elapsed_ok(fmt)
        || digits_with_date(&spelled)
        || percent_in_fourth(&spelled)
        || (NumberFormat::parse(&spelled).is_err()
            && (!ampm_literal_allowed(&spelled)
                || NumberFormat::parse(&quote_ampm(&spelled)).is_err()))
}

fn ampm_literal_allowed(code: &str) -> bool {
    let lower = code.to_ascii_lowercase();
    let Some(a) = lower.find("am/pm") else {
        return true;
    };
    let start = lower[..a].rfind(';').map_or(0, |s| s + 1);
    let sec = &lower[start..a];
    !sec.split('[').skip(1).any(|b| {
        b.split(']').next().is_some_and(|n| {
            n.chars()
                .next()
                .is_some_and(|c| matches!(c, 'h' | 'm' | 's'))
                && n.chars().all(|x| n.starts_with(x))
        })
    })
}

fn percent_in_fourth(spelled: &str) -> bool {
    let secs = format_sections(spelled);
    secs.len() == 4 && secs[3].contains('%')
}

fn at_sign_rejected(f: &str) -> bool {
    let secs = format_sections(f);
    let n = secs.len();

    if secs
        .iter()
        .any(|s| (s.contains("e+") || s.contains("e-")) && s.contains(['0', '#', '?']))
    {
        return true;
    }
    secs.iter().enumerate().any(|(i, s)| {
        if !s.contains('@') {
            return false;
        }
        let l = s.to_ascii_lowercase();
        i + 1 != n || l.contains(['0', '#', '?', '%', 'd', 'm', 'y', 'h', 's', 'e', 'g'])
    })
}

fn format_sections(f: &str) -> Vec<String> {
    let mut secs: Vec<String> = vec![String::new()];
    let mut it = f.chars();
    while let Some(c) = it.next() {
        match c {
            '"' => {
                for d in it.by_ref() {
                    if d == '"' {
                        break;
                    }
                }
            }
            '\\' | '_' | '*' => {
                it.next();
            }
            '[' => {
                for d in it.by_ref() {
                    if d == ']' {
                        break;
                    }
                }
            }
            ';' => secs.push(String::new()),
            c => secs.last_mut().unwrap().push(c),
        }
    }
    secs
}

pub fn format_code_as_read(fmt: &str) -> String {
    unquote_dollar(fmt).into_owned()
}

fn norm_format(fmt: &str) -> String {
    if fmt.is_empty() || fmt.eq_ignore_ascii_case("general") {
        "General".into()
    } else {
        readback_spelling(&unquote_dollar(fmt))
    }
}

fn stored_format(fmt: &str) -> String {
    if fmt.is_empty() || fmt.eq_ignore_ascii_case("general") {
        "General".into()
    } else {
        unquote_dollar(fmt).into_owned()
    }
}

fn unquote_dollar(fmt: &str) -> std::borrow::Cow<'_, str> {
    if !fmt.contains("\"$\"") {
        return fmt.into();
    }
    let mut out = String::with_capacity(fmt.len());
    let mut it = fmt.chars().peekable();
    while let Some(c) = it.next() {
        match c {
            '\\' => {
                out.push(c);
                out.extend(it.next());
            }
            '"' => {
                let mut lit = String::new();
                let mut closed = false;
                for d in it.by_ref() {
                    if d == '"' {
                        closed = true;
                        break;
                    }
                    lit.push(d);
                }
                if closed && lit == "$" {
                    out.push('$');
                } else {
                    out.push('"');
                    out.push_str(&lit);
                    if closed {
                        out.push('"');
                    }
                }
            }
            _ => out.push(c),
        }
    }
    out.into()
}
