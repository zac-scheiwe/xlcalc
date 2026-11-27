use crate::workbook::{Command, LastFormat};
use std::collections::BTreeMap;
use std::sync::Arc;

use rustc_hash::{FxHashMap, FxHashSet};

use crate::address::{CellAddr, Rect, SheetId};
use crate::calc::{self, RecalcReport};
use crate::formula::{CRef, Node, Rc, SheetRef};
use crate::functions;
use crate::ifmt_slots::{builtin_spelling, NumberFormatTable};
use crate::locale::Locale;
use crate::numfmt::fillfmt::Fm;
use crate::numfmt::LimPrec;
use crate::numfmt::{
    compile_for_compare, designators, digits_with_date, quote_ampm, readback_spelling,
    spelled_for_parse, typed_elapsed_ok, Compiled, Designators, NumberFormat,
};
use crate::parser::ast::{BinOp, UnaryOp};
use crate::style::{Fill, Pattern, Style};
use crate::value::{Array, Value};
use crate::workbook::{CalcMode, Entry, Workbook};

pub(crate) type Sty = Arc<Style>;

struct Line<'a> {
    seq: u64,
    style: &'a Sty,

    owned: u64,
}

impl<'a> Line<'a> {
    fn showing(
        m: &'a FxHashMap<(SheetId, u32), (u64, Sty)>,
        sheet: SheetId,
        span: std::ops::RangeInclusive<u32>,
        floor: Option<u64>,
    ) -> FxHashMap<u32, Line<'a>> {
        m.iter()
            .filter(|((s, i), (seq, _))| {
                *s == sheet && span.contains(i) && floor.is_none_or(|f| *seq > f)
            })
            .map(|(&(_, i), (seq, style))| {
                let line = Line {
                    seq: *seq,
                    style,
                    owned: 0,
                };
                (i, line)
            })
            .collect()
    }
}

#[derive(Default)]
pub struct CellFormats {
    pub(crate) map: FxHashMap<(SheetId, u32, u32), Sty>,

    pub(crate) sticky: FxHashMap<SheetId, Rect>,

    pub(crate) row_marks: FxHashMap<SheetId, std::collections::BTreeMap<u32, bool>>,

    pub(crate) rows: FxHashMap<(SheetId, u32), (u64, Sty)>,
    pub(crate) cols: FxHashMap<(SheetId, u32), (u64, Sty)>,
    pub(crate) sheets: FxHashMap<SheetId, (u64, Sty)>,
    seq: u64,
    compiled: FxHashMap<Arc<str>, Option<Arc<NumberFormat>>>,

    registry: FxHashMap<Compiled, String>,

    format_order: Vec<Arc<str>>,
    format_seen: rustc_hash::FxHashSet<Arc<str>>,

    typed_of: FxHashMap<String, String>,
    pub(crate) precision: bool,

    protected: FxHashSet<SheetId>,

    late: FxHashMap<usize, (std::sync::Weak<Style>, Option<Arc<str>>)>,

    base: Sty,
    interned: rustc_hash::FxHashSet<Sty>,

    xf_order: Vec<Sty>,

    dxf_clock: u64,
}

fn key_of(s: &Sty) -> usize {
    Arc::as_ptr(s) as usize
}

impl CellFormats {
    pub(crate) fn any(&self) -> bool {
        !(self.map.is_empty()
            && self.row_marks.is_empty()
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
        if let Some(m) = other.row_marks.get(&from) {
            self.row_marks.insert(to, m.clone());
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

    pub(crate) fn inherited(&mut self, source: Sty, other: Option<&Sty>) -> Sty {
        let mut b = crate::style::Borders::default();
        let (s, t) = (
            &source.borders,
            other.map_or(&self.base.borders, |o| &o.borders),
        );
        if s.left == t.left {
            b.left = s.left;
        }
        if s.right == t.right {
            b.right = s.right;
        }
        if s.top == t.top {
            b.top = s.top;
        }
        if s.bottom == t.bottom {
            b.bottom = s.bottom;
        }
        if (s.diagonal, s.diagonal_down, s.diagonal_up)
            == (t.diagonal, t.diagonal_down, t.diagonal_up)
        {
            b.diagonal = s.diagonal;
            b.diagonal_down = s.diagonal_down;
            b.diagonal_up = s.diagonal_up;
        }
        if source.borders == b {
            return source;
        }
        let mut n = (*source).clone();
        n.borders = b;
        self.intern(n)
    }

    fn replace_number_format(&mut self, held: &dyn Fn(&str) -> bool, to: &str) {
        let mut moved: FxHashMap<usize, Sty> = FxHashMap::default();
        for i in 0..self.xf_order.len() {
            let old = self.xf_order[i].clone();
            if !held(&old.number_format) {
                continue;
            }
            self.interned.remove(&old);
            let new = old.with_number_format(to);
            let new = match self.interned.get(&new) {
                Some(x) => x.clone(),
                None => {
                    let x = Arc::new(new);
                    self.interned.insert(x.clone());
                    self.xf_order[i] = x.clone();
                    x
                }
            };
            moved.insert(key_of(&old), new);
        }
        self.xf_order.retain(|s| !moved.contains_key(&key_of(s)));
        let swap = |s: &mut Sty| {
            if let Some(x) = moved.get(&key_of(s)) {
                *s = x.clone();
            }
        };
        self.map.values_mut().for_each(swap);
        self.rows.values_mut().for_each(|v| swap(&mut v.1));
        self.cols.values_mut().for_each(|v| swap(&mut v.1));
        self.sheets.values_mut().for_each(|v| swap(&mut v.1));
        swap(&mut self.base);
        self.compiled.clear();
    }

    pub(crate) fn intern(&mut self, s: Style) -> Sty {
        if let Some(x) = self.interned.get(&s) {
            return x.clone();
        }
        let x = Arc::new(s);
        self.interned.insert(x.clone());
        self.xf_order.push(x.clone());
        self.note_format(&x.number_format);
        x
    }

    pub(crate) fn base(&self) -> &Sty {
        &self.base
    }

    pub(crate) fn set_base(&mut self, s: Style) {
        self.base = self.intern(s);
    }

    fn note_format(&mut self, code: &str) {
        if code != "General" && !self.format_seen.contains(code) {
            let code: Arc<str> = Arc::from(code);
            self.format_seen.insert(code.clone());
            self.format_order.push(code);
        }
    }

    pub(crate) fn row_record(&self, sheet: SheetId, row: u32) -> Option<&Sty> {
        let own = self.rows.get(&(sheet, row))?;
        let all = self.sheets.get(&sheet);
        let (_, style) = all.filter(|s| s.0 > own.0).unwrap_or(own);
        let over_columns = || all.is_some() || self.cols.keys().any(|k| k.0 == sheet);
        (*style != self.base || over_columns()).then_some(style)
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

    fn stored_styles(&self, sheet: SheetId, rect: Rect) -> Vec<&Sty> {
        let sheet_style = self.sheets.get(&sheet);
        let floor = sheet_style.map(|(seq, _)| *seq);
        let mut rows = Line::showing(&self.rows, sheet, rect.r0..=rect.r1, floor);
        let mut cols = Line::showing(&self.cols, sheet, rect.c0..=rect.c1, floor);
        let mut out = Vec::new();
        let mut owned_plain = 0u64;
        for (&(s, r, c), style) in &self.map {
            if s != sheet || !rect.contains(r, c) {
                continue;
            }
            out.push(style);
            match (rows.get_mut(&r), cols.get_mut(&c)) {
                (Some(row), Some(col)) if row.seq > col.seq => row.owned += 1,
                (_, Some(col)) => col.owned += 1,
                (Some(row), None) => row.owned += 1,
                (None, None) => owned_plain += 1,
            }
        }
        let sorted = |m: &FxHashMap<u32, Line>| {
            let mut v: Vec<u64> = m.values().map(|l| l.seq).collect();
            v.sort_unstable();
            v
        };
        let (row_seqs, col_seqs) = (sorted(&rows), sorted(&cols));
        let (height, width) = (u64::from(rect.rows()), u64::from(rect.cols()));

        let plain_rows = height - rows.len() as u64;
        let plain_cols = width - cols.len() as u64;
        for row in rows.values() {
            let older = col_seqs.partition_point(|&q| q < row.seq) as u64;
            if plain_cols + older > row.owned {
                out.push(row.style);
            }
        }
        for col in cols.values() {
            let older = row_seqs.partition_point(|&q| q <= col.seq) as u64;
            if plain_rows + older > col.owned {
                out.push(col.style);
            }
        }
        if plain_rows * plain_cols > owned_plain {
            out.push(sheet_style.map_or(&self.base, |(_, s)| s));
        }
        out
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
        self.note_format(code);
        if code != "General" {
            self.registry
                .entry(compile_for_compare(code))
                .or_insert_with(|| code.to_string());
        }
    }

    fn registered(&mut self, loc: &Locale, typed: &str, spelled: String) -> String {
        if spelled == "General" {
            return spelled;
        }

        if let Some(b) = builtin_spelling(loc, &spelled) {
            return b;
        }
        let key = compile_for_compare(&spelled);
        let read_back = self.registry.entry(key).or_insert(spelled).clone();
        self.note_format(&read_back);
        if read_back != typed && has_elapsed_bracket(typed) {
            self.typed_of
                .entry(read_back.clone())
                .or_insert_with(|| typed.to_string());
        }
        read_back
    }

    pub(crate) fn parse_stored(&self, code: &str) -> Result<NumberFormat, crate::XlError> {
        match self.typed_of.get(code) {
            Some(typed) => NumberFormat::parse(typed),
            None => NumberFormat::parse_stored(code),
        }
    }

    fn compile(&mut self, f: &Arc<str>) -> Option<Arc<NumberFormat>> {
        if let Some(c) = self.compiled.get(f) {
            return c.clone();
        }
        let c = self
            .parse_stored(f)
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

fn has_elapsed_bracket(code: &str) -> bool {
    code.split('[').skip(1).any(|b| {
        b.split(']').next().is_some_and(|i| {
            !i.is_empty()
                && i.chars()
                    .all(|c| matches!(c, 'h' | 'H' | 'm' | 'M' | 's' | 'S'))
        })
    })
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

    pub fn cell_formats_in_table_order(&self) -> Vec<(&Style, bool)> {
        let mut used: FxHashMap<usize, [bool; 2]> = FxHashMap::default();
        for (&(sheet, r, c), s) in &self.cellfmt.map {
            if self.sheet(sheet).is_some() {
                let quote = self.has_apostrophe(CellAddr::new(sheet, r, c));
                used.entry(key_of(s)).or_default()[usize::from(quote)] = true;
            }
        }
        let mut out = Vec::new();
        for s in &self.cellfmt.xf_order {
            if let Some(&[plain, quoted]) = used.get(&key_of(s)) {
                out.extend(plain.then_some((&**s, false)));
                out.extend(quoted.then_some((&**s, true)));
            }
        }
        out
    }

    pub fn range_styles(&self, sheet: SheetId, rect: Rect) -> Vec<&Style> {
        let f = &self.cellfmt;
        let stored = f.map.len() + f.rows.len() + f.cols.len();
        let styles = if rect.cell_count() <= stored as u64 + 1 {
            (rect.r0..=rect.r1)
                .flat_map(|r| (rect.c0..=rect.c1).map(move |c| CellAddr::new(sheet, r, c)))
                .map(|a| f.style(a))
                .collect()
        } else {
            f.stored_styles(sheet, rect)
        };
        let mut seen = rustc_hash::FxHashSet::default();
        styles
            .into_iter()
            .filter(|s| seen.insert(Arc::as_ptr(s)))
            .map(|s| &**s)
            .collect()
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

    pub fn row_formats(&self, sheet: SheetId) -> BTreeMap<u32, Style> {
        let cf = &self.cellfmt;
        cf.rows
            .keys()
            .filter(|k| k.0 == sheet)
            .filter_map(|&(_, r)| cf.row_record(sheet, r).map(|s| (r, (**s).clone())))
            .collect()
    }

    pub fn row_column_crossings(&self, sheet: SheetId) -> Vec<(u32, u32)> {
        let cf = &self.cellfmt;
        let rows: Vec<(u32, u64, &Sty)> = cf
            .rows
            .iter()
            .filter(|(k, _)| k.0 == sheet)
            .filter_map(|(&(_, r), (seq, _))| cf.row_record(sheet, r).map(|s| (r, *seq, s)))
            .collect();
        let Some(oldest) = rows.iter().map(|r| r.1).min() else {
            return Vec::new();
        };
        let newer: Vec<u32> = cf
            .cols
            .iter()
            .filter(|(k, (seq, _))| k.0 == sheet && *seq > oldest)
            .map(|(k, _)| k.1)
            .collect();
        let mut out = Vec::new();
        for &(r, _, written) in &rows {
            for &c in &newer {
                let a = CellAddr::new(sheet, r, c);
                if !cf.map.contains_key(&(sheet, r, c)) && **cf.style(a) != **written {
                    out.push((r, c));
                }
            }
        }
        out
    }

    pub fn load_row_style(&mut self, sheet: SheetId, row: u32, style: &Style) {
        let fmt = stored_format(&style.number_format);
        self.cellfmt.register_stored(&fmt);
        let style = style.with_number_format(&fmt);
        let cf = &mut self.cellfmt;
        cf.seq += 1;
        let (seq, style) = (cf.seq, cf.intern(style));
        cf.rows.insert((sheet, row), (seq, style));
        self.mark_rows(sheet, row, row, true);
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
        self.session.last_format.set(fmt.map(str::to_string));
    }

    pub fn note_cell_format(&mut self, addr: CellAddr) {
        let f = self.number_format(addr);
        if !f.eq_ignore_ascii_case("general") {
            self.session.last_format.set(Some(f.to_string()));
        }
    }

    pub fn note_exact_cell_format(&mut self, addr: CellAddr) {
        let f = self.number_format(addr).to_string();
        self.session.last_format.set(Some(f));
    }

    pub fn share_last_format(&mut self, shared: &LastFormat) {
        self.session.last_format = shared.clone();
    }

    pub fn adopt_last_format(&mut self, shared: &LastFormat) {
        if !self.session.last_format.is(shared) {
            shared.set(self.session.last_format.get());
            self.share_last_format(shared);
        }
    }

    pub(crate) fn subtotal_label_format(&mut self, addr: CellAddr) {
        let Some(f) = self.session.last_format.take() else {
            return;
        };
        let cur = self.number_format(addr);
        if self.should_install_format(cur, &f) {
            self.store_format(addr, &f);
        }
    }

    pub(crate) fn should_install_format(&self, cur: &str, new: &str) -> bool {
        if cur.eq_ignore_ascii_case(new) || !self.is_builtin_format(cur) {
            return false;
        }
        if cur.eq_ignore_ascii_case("general") {
            return true;
        }
        let ty = |f: &str| Fm::of(f, !self.is_builtin_format(f)).ty;
        let t_cur = ty(cur);
        if t_cur == 1 {
            return false;
        }
        let t_new = ty(new);
        t_new != 1 && t_new != t_cur
    }

    pub(crate) fn is_text_format(&self, addr: CellAddr) -> bool {
        self.cellfmt
            .fmt(addr)
            .is_some_and(|f| is_text_format_code(f))
    }

    pub(crate) fn fraction_first(&mut self, addr: CellAddr) -> bool {
        let Some(f) = self.cellfmt.fmt(addr).cloned() else {
            return false;
        };
        matches!(
            Fm::of(&f, !self.is_builtin_format(&f)).ty,
            1 | 2 | 5 | 6 | 7 | 10
        )
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

    pub(crate) fn mark_rows(&mut self, sheet: SheetId, r0: u32, r1: u32, hard: bool) {
        if r1.saturating_sub(r0) >= 100_000 {
            return;
        }
        let m = self.cellfmt.row_marks.entry(sheet).or_default();
        for r in r0..=r1 {
            let e = m.entry(r).or_insert(hard);
            *e |= hard;
        }
    }

    pub fn marked_rows(&self, sheet: SheetId) -> Option<(u32, u32)> {
        let m = self.cellfmt.row_marks.get(&sheet)?;
        Some((*m.keys().next()?, *m.keys().next_back()?))
    }

    pub fn recompute_used_range(&mut self) {
        let ids: Vec<SheetId> = self.sheets().map(|s| s.id).collect();
        for id in ids {
            self.recompute_used_range_of(id);
        }
    }

    pub fn used_range_with_formats(&self, id: SheetId) -> Option<Rect> {
        let mut b = self.used_range(id);

        let base = self.cellfmt.base();
        let bare = |s: &Style| {
            let mut s = s.clone();
            s.borders = Default::default();
            s == **base
        };
        for (k, v) in self.cellfmt.map.iter().filter(|(k, _)| k.0 == id) {
            let line = self.cellfmt.line(CellAddr::new(id, k.1, k.2));
            if bare(v) && line.is_none_or(|l| bare(l)) {
                continue;
            }
            let cell = Rect::new(k.1, k.2, k.1, k.2);
            b = Some(b.map_or(cell, |x| x.union_bounds(&cell)));
        }
        if let Some(marks) = self.cellfmt.row_marks.get(&id) {
            let custom: Vec<u32> = self.row_custom_heights(id).iter().map(|h| h.0).collect();

            let mut hard = marks.keys().copied().filter(|r| {
                custom.contains(r)
                    || self.row_hidden(id, *r)
                    || self.cellfmt.row_record(id, *r).is_some()
            });
            let lo = hard.next();
            let hi = hard.last().or(lo);
            if let (Some(lo), Some(hi)) = (lo, hi) {
                b = Some(match b {
                    Some(x) => Rect::new(x.r0.min(lo), x.c0, x.r1.max(hi), x.c1),
                    None => Rect::new(lo, 0, hi, 0),
                });
            }
        }
        b
    }

    pub fn recompute_used_range_of(&mut self, id: SheetId) {
        {
            let used = self.used_range(id);
            if let Some(sh) = self.sheet_mut(id) {
                sh.cells.reset_sticky(used);
            }
            let mut b: Option<Rect> = None;
            for k in self.cellfmt.map.keys().filter(|k| k.0 == id) {
                let cell = Rect::new(k.1, k.2, k.1, k.2);
                b = Some(b.map_or(cell, |x| x.union_bounds(&cell)));
            }
            match b {
                Some(r) => self.cellfmt.sticky.insert(id, r),
                None => self.cellfmt.sticky.remove(&id),
            };
            if let Some(m) = self.cellfmt.row_marks.get(&id).cloned() {
                let keep: std::collections::BTreeMap<u32, bool> = m
                    .into_iter()
                    .filter(|(r, hard)| *hard || self.row_hidden(id, *r))
                    .collect();
                self.cellfmt.row_marks.insert(id, keep);
            }
        }
    }

    pub(crate) fn apply_style_with(
        &mut self,
        sheet: SheetId,
        rect: Rect,
        f: &dyn Fn(&Style) -> Style,
    ) {
        self.apply_style_with_opt(sheet, rect, f, false);
    }

    fn apply_style_with_opt(
        &mut self,
        sheet: SheetId,
        rect: Rect,
        f: &dyn Fn(&Style) -> Style,
        keep_merged: bool,
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
            if rect.is_full_rows() && !whole {
                self.mark_rows(sheet, rect.r0, rect.r1, true);
            }

            let pinned: Vec<_> = if whole || !keep_merged {
                Vec::new()
            } else {
                self.merges(sheet)
                    .into_iter()
                    .filter(|m| !(rect.contains(m.r0, m.c0) && rect.contains(m.r1, m.c1)))
                    .filter_map(|m| m.intersection(&rect))
                    .filter(|x| x.rows() as u64 * x.cols() as u64 <= 1 << 16)
                    .flat_map(|x| {
                        (x.r0..=x.r1).flat_map(move |r| {
                            (x.c0..=x.c1).map(move |c| CellAddr::new(sheet, r, c))
                        })
                    })
                    .map(|a| (a, (*self.cellfmt.style(a)).clone()))
                    .collect()
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
            for (a, s) in pinned {
                self.store_style(a, (*s).clone());
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

    fn apply_format(&mut self, sheet: SheetId, rect: Rect, fmt: &str, ampm: Designators) -> String {
        let spelled = norm_format(fmt, ampm);

        let implicit = !fmt.contains(';') && spelled.ends_with(";General");
        let loc = self.input_env().locale.clone();
        let fmt = self.cellfmt.registered(&loc, fmt, spelled);
        self.apply_style_with(sheet, rect, &|s: &Style| Style {
            implicit_general: implicit,
            ..s.with_number_format(&fmt)
        });
        fmt
    }

    pub fn register_number_format(&mut self, code: &str) {
        let fmt = stored_format(code);
        self.cellfmt.register_stored(&fmt);
    }

    fn custom_slot(&self, code: &str) -> Option<usize> {
        (code != "General" && !self.is_builtin_format(code))
            .then(|| self.number_format_table().slot(code))
    }

    pub fn cfv_format_made(&mut self, sheet: SheetId, id: u32) {
        self.cellfmt.dxf_clock += 1;
        let stamp = self.cellfmt.dxf_clock;
        let sh = self.cfv_sheet_mut(sheet);
        if let Some(i) = sh.index_of(id) {
            sh.rules[i].dxf.made.stamp = stamp;
            sh.rules[i].dxf.made.source = None;
        }
    }

    pub fn cfv_formats_loaded<'a>(&mut self, dxfs: impl Iterator<Item = &'a mut crate::cfv::Dxf>) {
        let mut formatted = Vec::new();
        for d in dxfs {
            self.cellfmt.dxf_clock += 1;
            d.made.stamp = self.cellfmt.dxf_clock;
            if d.mask.number_format {
                self.register_number_format(&d.style.number_format);
                formatted.push(d);
            }
        }
        if formatted.is_empty() {
            return;
        }
        let mut table = self.number_format_table();
        for d in formatted {
            let code = stored_format(&d.style.number_format);
            d.made.slot =
                (code != "General" && !self.is_builtin_format(&code)).then(|| table.slot(&code));
        }
    }

    pub fn cfv_put_number_format(&mut self, sheet: SheetId, id: u32, code: &str) {
        let spelled = norm_format(code, designators(0));
        let loc = self.input_env().locale.clone();
        let fmt = self.cellfmt.registered(&loc, code, spelled);
        let slot = self.custom_slot(&fmt);
        let sh = self.cfv_sheet_mut(sheet);
        if let Some(i) = sh.index_of(id) {
            let d = &mut sh.rules[i].dxf;
            d.mask.number_format = true;
            d.style.number_format = fmt.as_str().into();
            d.made.slot = slot;
        }

        if let Some(b) = sh.block_of(id) {
            sh.reenter(b);
        }
        self.cfv_format_made(sheet, id);
    }

    pub fn cfv_number_format(&self, d: &crate::cfv::Dxf) -> CfNumberFormat {
        if !d.mask.number_format {
            return CfNumberFormat::Unset;
        }
        match d.made.slot {
            Some(s) => match self.number_format_table().codes().get(s) {
                Some(c) => CfNumberFormat::Code(c.clone()),
                None => CfNumberFormat::Gone,
            },
            None => CfNumberFormat::Code(d.style.number_format.to_string()),
        }
    }

    pub fn delete_number_format(&mut self, code: &str) -> bool {
        let spelled = norm_format(code, None);
        if spelled == "General" || self.is_builtin_format(&spelled) {
            self.note_number_format(Some(&spelled));
            return false;
        }
        let key = compile_for_compare(&spelled);
        let held = |f: &str| f != "General" && compile_for_compare(f) == key;
        let Some(slot) = self
            .number_format_table()
            .codes()
            .iter()
            .position(|f| held(f))
        else {
            self.note_number_format(None);
            return true;
        };
        let cf = &mut self.cellfmt;
        cf.format_order.retain(|f| !held(f));
        cf.format_seen.retain(|f| !held(f));
        cf.registry.remove(&key);
        let normal = cf.base.number_format.clone();
        let to = if held(&normal) {
            Arc::from("General")
        } else {
            normal
        };
        cf.replace_number_format(&held, &to);
        for n in &mut self.style_table_mut().named {
            if held(&n.style.number_format) {
                n.style = n.style.with_number_format(&to);
            }
        }
        for sh in self.cfv.values_mut() {
            for r in &mut sh.rules {
                if r.dxf.mask.number_format && held(&r.dxf.style.number_format) {
                    r.dxf.style.number_format = to.clone();
                }
            }
        }

        let next = self.number_format_table().codes().get(slot).cloned();
        self.note_number_format(next.as_deref());
        true
    }

    pub fn number_format_table(&self) -> NumberFormatTable {
        let mut t = NumberFormatTable::default();
        for code in &self.cellfmt.format_order {
            if !self.is_builtin_format(code) {
                t.slot(code);
            }
        }
        t
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
        self.command(Command::Format, |wb| {
            wb.set_style_with_run(sheet, rect, f, true)
        })
    }

    pub fn set_style_with_merged(
        &mut self,
        sheet: SheetId,
        rect: Rect,
        f: &dyn Fn(&Style) -> Style,
    ) {
        self.command(Command::Format, |wb| {
            wb.set_style_with_run(sheet, rect, f, false)
        })
    }

    fn set_style_with_run(
        &mut self,
        sheet: SheetId,
        rect: Rect,
        f: &dyn Fn(&Style) -> Style,
        keep_merged: bool,
    ) {
        if self.sheet(sheet).is_none() {
            return;
        }

        let late = self.late_cells(sheet, rect);
        self.apply_style_with_opt(sheet, rect, f, keep_merged);
        self.note_late_formats(sheet, late);
    }

    pub fn reset_named_style(&mut self, old: &Style) {
        let base = (*self.cellfmt.base).clone();
        self.restyle_with(&old.named.clone(), old, &base, true);
    }

    pub fn restyle_named(&mut self, name: &str, old: &Style, new: &Style) {
        self.restyle_with(name, old, new, false);
    }

    fn restyle_with(&mut self, name: &str, old: &Style, new: &Style, rebase: bool) {
        let cf = &mut self.cellfmt;
        let follow = |s: &Style| -> Option<Style> {
            if !s.named.eq_ignore_ascii_case(name) {
                return None;
            }
            let mut t = s.clone();
            if rebase {
                t.named = new.named.clone();
            }
            if old.number_format != new.number_format && s.number_format == old.number_format {
                t.number_format = new.number_format.clone();
                t.custom_code = new.custom_code;
                t.implicit_general = new.implicit_general;
            }
            if old.font != new.font && s.font == old.font {
                t.font = new.font.clone();
            }
            if old.alignment != new.alignment && s.alignment == old.alignment {
                t.alignment = new.alignment.clone();
            }
            if old.borders != new.borders && s.borders == old.borders {
                t.borders = new.borders.clone();
            }
            if old.fill != new.fill && s.fill == old.fill {
                t.fill = new.fill.clone();
            }
            if (old.locked, old.formula_hidden) != (new.locked, new.formula_hidden)
                && (s.locked, s.formula_hidden) == (old.locked, old.formula_hidden)
            {
                t.locked = new.locked;
                t.formula_hidden = new.formula_hidden;
            }
            (t != *s).then_some(t)
        };
        let cells: Vec<_> = cf
            .map
            .iter()
            .filter_map(|(k, v)| follow(v).map(|t| (*k, t)))
            .collect();
        for (k, t) in cells {
            let s = cf.intern(t);
            cf.map.insert(k, s);
        }
        let rows: Vec<_> = cf
            .rows
            .iter()
            .filter_map(|(k, v)| follow(&v.1).map(|t| (*k, t)))
            .collect();
        for (k, t) in rows {
            let s = cf.intern(t);
            if let Some(v) = cf.rows.get_mut(&k) {
                v.1 = s;
            }
        }
        let cols: Vec<_> = cf
            .cols
            .iter()
            .filter_map(|(k, v)| follow(&v.1).map(|t| (*k, t)))
            .collect();
        for (k, t) in cols {
            let s = cf.intern(t);
            if let Some(v) = cf.cols.get_mut(&k) {
                v.1 = s;
            }
        }
        let sheets: Vec<_> = cf
            .sheets
            .iter()
            .filter_map(|(k, v)| follow(&v.1).map(|t| (*k, t)))
            .collect();
        for (k, t) in sheets {
            let s = cf.intern(t);
            if let Some(v) = cf.sheets.get_mut(&k) {
                v.1 = s;
            }
        }
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
        self.set_number_format_body(sheet, rect, &fmt, None)
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
        merge_borders(own, other, &self.styles.theme)
    }

    pub fn line_style(&self, sheet: SheetId, row: bool, idx: u32) -> &Style {
        let cf = &self.cellfmt;
        let m = if row { &cf.rows } else { &cf.cols };
        m.get(&(sheet, idx))
            .or_else(|| cf.sheets.get(&sheet))
            .map_or(&cf.base, |(_, s)| s)
    }

    pub fn column_formats(&self, sheet: SheetId) -> Vec<(u32, Style)> {
        let cf = &self.cellfmt;
        let all = cf.sheets.get(&sheet);
        let mut v: Vec<(u32, &Sty)> = cf
            .cols
            .iter()
            .filter(|((s, _), _)| *s == sheet)
            .map(|((_, i), (_, st))| (*i, st))
            .collect();
        if let Some((_, st)) = all {
            let own: std::collections::HashSet<u32> = v.iter().map(|x| x.0).collect();
            v.extend(
                (0..crate::address::MAX_COLS)
                    .filter(|i| !own.contains(i))
                    .map(|i| (i, st)),
            );
        }
        v.sort_unstable_by_key(|x| x.0);
        v.into_iter()
            .filter(|(_, st)| ***st != *cf.base)
            .map(|(i, st)| (i, (**st).clone()))
            .collect()
    }

    pub fn line_formatted(&self, a: CellAddr) -> bool {
        let cf = &self.cellfmt;
        let column = cf
            .cols
            .get(&(a.sheet, a.col))
            .or_else(|| cf.sheets.get(&a.sheet));
        column.is_some_and(|(_, st)| **st != *cf.base)
            || cf
                .row_record(a.sheet, a.row)
                .is_some_and(|st| **st != *cf.base)
    }

    pub fn line_format_indices(&self, sheet: SheetId, row: bool) -> Vec<u32> {
        let m = if row {
            &self.cellfmt.rows
        } else {
            &self.cellfmt.cols
        };
        let mut v: Vec<u32> = m.keys().filter(|k| k.0 == sheet).map(|k| k.1).collect();
        v.sort_unstable();
        v
    }

    pub fn merge_sides(
        &self,
        own: crate::style::Border,
        other: crate::style::Border,
    ) -> crate::style::Border {
        merge_borders(own, other, &self.styles.theme)
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
        let stored = self.apply_format(sheet, rect, fmt, None);
        self.note_late_formats(sheet, before);
        self.note_table_col_formats(sheet, rect, &stored);
    }

    pub fn set_number_format(&mut self, sheet: SheetId, rect: Rect, fmt: &str) -> RecalcReport {
        self.set_number_format_lcid(sheet, rect, fmt, 0)
    }

    pub fn set_number_format_lcid(
        &mut self,
        sheet: SheetId,
        rect: Rect,
        fmt: &str,
        lcid: u32,
    ) -> RecalcReport {
        self.command(Command::Format, |wb| {
            wb.set_number_format_run(sheet, rect, fmt, designators(lcid))
        })
    }

    pub fn set_number_format_copy(
        &mut self,
        sheet: SheetId,
        rect: Rect,
        code: &str,
    ) -> RecalcReport {
        self.command(Command::Format, |wb| {
            wb.set_number_format_run(sheet, rect, code, None)
        })
    }

    fn set_number_format_run(
        &mut self,
        sheet: SheetId,
        rect: Rect,
        fmt: &str,
        ampm: Designators,
    ) -> RecalcReport {
        let r = self.set_number_format_body(sheet, rect, fmt, ampm);
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

    fn set_number_format_body(
        &mut self,
        sheet: SheetId,
        rect: Rect,
        fmt: &str,
        ampm: Designators,
    ) -> RecalcReport {
        if self.sheet(sheet).is_none() || format_refused(fmt) {
            return RecalcReport::default();
        }
        let before = self.late_candidates(sheet, rect, true);
        let formats = self.number_formats_of_constants(sheet, rect);
        let stored = self.apply_format(sheet, rect, fmt, ampm);
        self.note_late_formats(sheet, before);
        self.note_table_col_formats(sheet, rect, &stored);
        self.round_reformatted(sheet, rect, formats)
    }

    pub fn number_formats_of_constants(
        &self,
        sheet: SheetId,
        rect: Rect,
    ) -> Vec<(CellAddr, Arc<str>)> {
        let Some(sh) = self.sheet(sheet).filter(|_| self.cellfmt.precision) else {
            return Vec::new();
        };
        sh.cells
            .iter_rect(rect)
            .filter(|(_, _, e)| matches!(e, Entry::Const(Value::Number(_))))
            .map(|(r, c, _)| {
                let a = CellAddr::new(sheet, r, c);
                (a, self.cellfmt.style(a).number_format.clone())
            })
            .collect()
    }

    pub fn round_reformatted(
        &mut self,
        sheet: SheetId,
        rect: Rect,
        before: Vec<(CellAddr, Arc<str>)>,
    ) -> RecalcReport {
        if !self.cellfmt.precision {
            return RecalcReport::default();
        }
        let changed = before
            .into_iter()
            .filter(|(a, was)| self.cellfmt.style(*a).number_format != *was)
            .map(|(a, _)| a)
            .collect();
        self.round_range(sheet, rect, changed)
    }

    fn round_stored(&mut self, addr: CellAddr) {
        let entry = self
            .sheet(addr.sheet)
            .and_then(|s| s.cells.get(addr.row, addr.col));
        let Some(Entry::Const(Value::Number(n))) = entry else {
            return;
        };
        let n = *n;
        if self.locked_on_protected(addr) {
            return;
        }
        let v = self.precision_value(addr, n, LimPrec::Cell);
        if v.to_bits() != n.to_bits() {
            self.set_value_exact(addr, Value::Number(v));
        }
    }

    fn locked_on_protected(&self, addr: CellAddr) -> bool {
        self.cellfmt.protected.contains(&addr.sheet) && self.cellfmt.style(addr).locked
    }

    pub fn set_contents_protected(&mut self, sheet: SheetId, on: bool) {
        if on {
            self.cellfmt.protected.insert(sheet);
        } else {
            self.cellfmt.protected.remove(&sheet);
        }
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

    fn round_range(&mut self, sheet: SheetId, rect: Rect, changed: Vec<CellAddr>) -> RecalcReport {
        let sh = self.sheet(sheet).unwrap();
        let mut formulas = Vec::new();
        for (_, _, e) in sh.cells.iter_rect(rect) {
            if let Entry::Formula(fid) = e {
                formulas.push(*fid);
            }
        }
        for (fid, _) in sh.spills.intersecting(&rect) {
            formulas.push(fid);
        }
        let mode = self.settings.mode;
        self.settings.mode = CalcMode::Manual;
        for a in changed {
            self.round_stored(a);
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
                if let Entry::Const(Value::Number(_)) = e {
                    consts.push(CellAddr::new(s, r, c));
                }
            }
        }
        let mode = self.settings.mode;
        self.settings.mode = CalcMode::Manual;
        for a in consts {
            self.round_stored(a);
        }
        let fids: Vec<_> = self.formula_ids().collect();
        for fid in fids {
            if on {
                if let Value::Number(n) = self.formula(fid).value {
                    let a = self.formula(fid).addr;
                    if !self.locked_on_protected(a) {
                        let v = self.precision_value(a, n, LimPrec::Cell);
                        self.formula_mut(fid).value = Value::Number(v);
                    }
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

    pub(crate) fn precision_value(&mut self, addr: CellAddr, n: f64, routine: LimPrec) -> f64 {
        match self.compiled_format(addr) {
            Some(f) => f.displayed_value(n, routine),
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
                Value::Number(self.precision_value(addr, n, LimPrec::Store))
            }
            v => v,
        }
    }

    pub(crate) fn precision_result(&mut self, addr: CellAddr, v: Value) -> Value {
        match v {
            Value::Number(n) => Value::Number(self.precision_value(addr, n, LimPrec::Store)),
            Value::Array(a) => {
                let mut data = a.data.clone();
                for (i, x) in data.iter_mut().enumerate() {
                    if let Value::Number(n) = x {
                        let (r, c) = (i as u32 / a.cols, i as u32 % a.cols);
                        let at = CellAddr::new(addr.sheet, addr.row + r, addr.col + c);
                        *n = self.precision_value(at, *n, LimPrec::Store);
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
            if !self.should_install_format(&cur, &typed) {
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

        if f.eq_ignore_ascii_case("general") || (sheet != host.sheet && !self.is_builtin_format(&f))
        {
            return None;
        }
        Some(f)
    }

    pub(crate) fn is_builtin_format(&self, f: &str) -> bool {
        crate::ifmt_slots::is_builtin(self.input_env().locale, f)
    }

    pub fn format_in_table(&self, f: &str) -> bool {
        self.is_builtin_format(f)
            || self
                .style_table()
                .named
                .iter()
                .any(|s| *s.style.number_format == *f)
    }

    pub fn fill_in_table(&self, f: &Fill) -> bool {
        let same = |a: &Fill| {
            a.pattern == f.pattern
                && a.color == f.color
                && a.pattern_color == f.pattern_color
                && a.raw == f.raw
        };
        let t = self.style_table();
        let plain = f.raw.is_none() && f.color == Fill::default().color;
        (plain && matches!(f.pattern, Pattern::None | Pattern::Gray125))
            || t.named.iter().any(|n| same(&n.style.fill))
            || t.hidden.iter().any(|n| same(&n.style.style.fill))
            || self.cellfmt.xf_order.iter().any(|s| same(&s.fill))
            || same(&self.cellfmt.base.fill)
    }

    pub(crate) fn infer_format(&mut self, n: &Node, host: CellAddr) -> Option<Arc<str>> {
        let mut st = AutoFmt {
            need: true,
            auto: None,
            date: None,
        };
        self.infer_walk(n, host, &mut st);

        self.opr_push(&mut st, true);

        let f = match (st.date, st.auto) {
            (None, a) => a,
            (d, None) => d,
            (Some(_), Some(_)) => None,
        };

        let sum_last = last_closed_function(n).is_some_and(crate::ifmt_auto::is_sum);
        match f {
            Some(f) if f.eq_ignore_ascii_case("h:mm:ss") && sum_last => {
                Some(Arc::from("[h]:mm:ss"))
            }
            f => f,
        }
    }

    fn is_date_format(&mut self, f: &Option<Arc<str>>) -> bool {
        f.as_ref().is_some_and(|f| {
            self.cellfmt
                .compile(f)
                .is_some_and(|c| c.has_calendar_date())
        })
    }

    fn opr_push(&mut self, st: &mut AutoFmt, keeps: bool) {
        if !st.need {
            return;
        }
        if keeps {
            if self.is_date_format(&st.auto) {
                if st.date.is_some() {
                    return;
                }
                st.date = st.auto.clone();
            } else if st.date.is_none() {
                return;
            }
        } else {
            st.date = None;
            st.need = false;
        }
        st.auto = None;
    }

    fn infer_walk(&mut self, n: &Node, host: CellAddr, st: &mut AutoFmt) {
        crate::deep(|| self.infer_walk_inner(n, host, st))
    }

    fn infer_walk_inner(&mut self, n: &Node, host: CellAddr, st: &mut AutoFmt) {
        match n {
            Node::Ref(sr, cref) => {
                if st.need && st.auto.is_none() {
                    st.auto = self.ref_format(sr, cref, host);
                }
            }
            Node::Paren(x) => self.infer_walk(x, host, st),

            Node::Unary(UnaryOp::Implicit, x) => {
                self.opr_push(st, false);
                self.infer_walk(x, host, st);
            }
            Node::Ws { node, .. } => self.infer_walk(node, host, st),
            Node::Unary(UnaryOp::Neg | UnaryOp::Plus, x) => {
                self.opr_push(st, true);
                self.infer_walk(x, host, st);
            }
            Node::Binary(op, a, b) => {
                self.infer_walk(a, host, st);
                self.opr_push(st, matches!(op, BinOp::Add | BinOp::Sub));
                self.infer_walk(b, host, st);
            }
            Node::Percent(x) => {
                self.infer_walk(x, host, st);
                self.opr_push(st, false);
            }
            Node::Func { id, args } => {
                let saved = st.need;
                if st.need && st.auto.is_none() {
                    let name = functions::def(*id).name;
                    match crate::ifmt_auto::auto_ifmt(name) {
                        Some(ifmt) => {
                            st.auto = Some(self.auto_ifmt_format(ifmt));
                            st.need = false;
                        }
                        None => st.need = crate::ifmt_auto::passes_through(name),
                    }
                }
                for (i, a) in args.iter().enumerate() {
                    if i > 0 {
                        self.opr_push(st, true);
                    }
                    self.infer_walk(a, host, st);
                }
                st.need = saved;
            }
            _ => {}
        }
    }

    fn auto_ifmt_format(&self, ifmt: u8) -> Arc<str> {
        use crate::ifmt_auto::{IFMT_CURRENCY, IFMT_DATE, IFMT_DATETIME, IFMT_PERCENT, IFMT_TIME};
        let env = self.input_env();
        let c = &env.locale.currency;
        let f = match ifmt {
            IFMT_CURRENCY => format!("{c}#,##0.00;[Red]-{c}#,##0.00"),
            IFMT_PERCENT => "0%".to_string(),
            IFMT_DATE => env.locale.short_date_format.clone(),
            IFMT_TIME => "h:mm am/pm".to_string(),
            IFMT_DATETIME => format!("{} h:mm", env.locale.short_date_format),
            _ => String::new(),
        };
        Arc::from(f.as_str())
    }
}

fn last_closed_function(n: &Node) -> Option<&'static str> {
    crate::deep(|| match n {
        Node::Unary(_, x) | Node::Percent(x) | Node::Spill(x) | Node::Paren(x) => {
            last_closed_function(x)
        }
        Node::Ws { node, .. } => last_closed_function(node),
        Node::Binary(_, a, b) => last_closed_function(b).or_else(|| last_closed_function(a)),
        Node::Func { id, .. } => Some(functions::def(*id).name),
        Node::NamedCall { args, .. } => args.iter().rev().find_map(last_closed_function),
        Node::Call { callee, args } => args
            .iter()
            .rev()
            .find_map(last_closed_function)
            .or_else(|| last_closed_function(callee)),
        Node::Let { values, body, .. } => last_closed_function(body)
            .or_else(|| values.iter().rev().find_map(last_closed_function)),
        _ => None,
    })
}

struct AutoFmt {
    need: bool,
    auto: Option<Arc<str>>,
    date: Option<Arc<str>>,
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
        let l = strip_literals(s)
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

fn strip_literals(s: &str) -> String {
    let mut out = String::new();
    let mut cs = s.chars().peekable();
    while let Some(c) = cs.next() {
        match c {
            '"' => {
                for d in cs.by_ref() {
                    if d == '"' {
                        break;
                    }
                }
            }
            '\\' | '_' | '*' => {
                cs.next();
            }
            '[' => {
                let mut inner = String::new();
                for d in cs.by_ref() {
                    if d == ']' {
                        break;
                    }
                    inner.push(d);
                }
                if inner.starts_with(['h', 'H', 'm', 'M', 's', 'S']) {
                    out.push('0');
                }
            }
            _ => out.push(c),
        }
    }
    out
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

pub enum CfNumberFormat {
    Unset,
    Code(String),

    Gone,
}

pub fn format_code_normalized(fmt: &str) -> String {
    norm_format(fmt, designators(0))
}

fn norm_format(fmt: &str, ampm: Designators) -> String {
    if fmt.is_empty() || fmt.eq_ignore_ascii_case("general") {
        "General".into()
    } else {
        readback_spelling(&unquote_dollar(fmt), ampm)
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

fn merge_borders(
    own: crate::style::Border,
    other: crate::style::Border,
    theme: &crate::style::Theme,
) -> crate::style::Border {
    use crate::style::BorderLine;
    if other.line == BorderLine::None || own == other || own.line == BorderLine::None {
        return if own.line == BorderLine::None {
            other
        } else {
            own
        };
    }
    let (ra, rb) = (own.line.priority_rank(), other.line.priority_rank());
    if ra != rb {
        return if ra < rb { own } else { other };
    }
    let weight = |b: crate::style::Border| {
        let v = theme.rgb(b.color, 0);
        2 * ((v >> 16) & 0xff) + 5 * ((v >> 8) & 0xff) + (v & 0xff)
    };
    if weight(other) < weight(own) {
        other
    } else {
        own
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn stored_styles_match_a_walk() {
        let mut seed = 0x2545_f491_4f6c_dd1du64;
        let mut next = move |n: u32| {
            seed ^= seed << 13;
            seed ^= seed >> 7;
            seed ^= seed << 17;
            (seed % u64::from(n)) as u32
        };
        let (last_row, last_col) = (crate::address::MAX_ROWS - 1, crate::address::MAX_COLS - 1);
        for _ in 0..300 {
            let mut wb = Workbook::new();
            let sheet = wb.sheet_at(0).id;
            for _ in 0..next(14) {
                let (r, c) = (next(5), next(5));
                let rect = match next(8) {
                    0 => Rect::new(r, 0, r + next(2), last_col),
                    1 => Rect::new(0, c, last_row, c + next(2)),
                    2 if next(3) == 0 => Rect::new(0, 0, last_row, last_col),
                    _ => Rect::new(r, c, r + next(3), c + next(3)),
                };
                let size = 100 + i32::try_from(next(5)).unwrap() * 20;
                wb.set_style_with(sheet, rect, &|s: &Style| {
                    let mut s = s.clone();
                    s.font.size_twips = size.try_into().unwrap();
                    s
                });
            }
            for _ in 0..6 {
                let (r, c) = (next(6), next(6));
                let rect = Rect::new(r, c, r + next(6), c + next(6));
                let sizes = |v: Vec<&Sty>| {
                    let mut v: Vec<_> = v.into_iter().map(|s| s.font.size_twips).collect();
                    v.sort_unstable();
                    v.dedup();
                    v
                };
                let walked = (rect.r0..=rect.r1)
                    .flat_map(|r| (rect.c0..=rect.c1).map(move |c| CellAddr::new(sheet, r, c)))
                    .map(|a| wb.cellfmt.style(a))
                    .collect();
                assert_eq!(
                    sizes(wb.cellfmt.stored_styles(sheet, rect)),
                    sizes(walked),
                    "{rect:?}"
                );
            }
        }
    }

    #[test]
    fn a_formula_takes_a_date_unless_a_second_date_cancels_it() {
        let mut wb = Workbook::new();
        let s = wb.sheet_at(0).id;
        let at = |r: u32, c: u32| CellAddr::new(s, r, c);
        wb.set_input(at(0, 0), "1/02/2024").unwrap();
        wb.set_input(at(1, 0), "3/04/2024").unwrap();
        wb.set_input(at(2, 0), "5").unwrap();
        wb.set_number_format(s, Rect::new(2, 0, 2, 0), "0.00");
        let date = wb.number_format(at(0, 0)).to_string();
        let formats: Vec<String> = ["=A1+A3", "=A3+A1", "=A1-A2", "=A1+A3*1"]
            .iter()
            .zip(0..)
            .map(|(f, r)| {
                wb.set_input(at(r, 1), f).unwrap();
                wb.number_format(at(r, 1)).to_string()
            })
            .collect();

        assert_eq!(formats, [date.as_str(), "0.00", "General", "General"]);
    }

    #[test]
    fn deleting_a_format_leaves_its_number_in_vifmt() {
        let mut wb = Workbook::new();
        let s = wb.sheet_at(0).id;
        let shared = LastFormat::default();
        wb.adopt_last_format(&shared);
        for (c, f) in [(0, "0.000\" a\""), (1, "0.000\" b\"")] {
            wb.set_number_format(s, Rect::cell(0, c), f);
        }
        assert!(wb.delete_number_format("0.000\" a\""));
        assert_eq!(shared.get().as_deref(), Some("0.000\" b\""));
        assert!(wb.delete_number_format("0.000\" b\""));
        assert_eq!(shared.get(), None);

        shared.set(Some("0%".into()));
        assert!(wb.delete_number_format("0.000\" z\""));
        assert_eq!(shared.get(), None);
        assert!(!wb.delete_number_format("0%"));
        assert_eq!(shared.get().as_deref(), Some("0%"));
    }
}
