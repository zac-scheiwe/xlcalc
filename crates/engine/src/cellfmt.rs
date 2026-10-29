use std::sync::Arc;

use rustc_hash::FxHashMap;

use crate::address::{CellAddr, Rect, SheetId};
use crate::calc::{self, RecalcReport};
use crate::formula::{CRef, Node, Rc, SheetRef};
use crate::functions;
use crate::numfmt::NumberFormat;
use crate::parser::ast::{BinOp, UnaryOp};
use crate::value::{Array, Value};
use crate::workbook::{CalcMode, Entry, Workbook};

#[derive(Default)]
pub struct CellFormats {
    pub(crate) map: FxHashMap<(SheetId, u32, u32), Arc<str>>,

    pub(crate) rows: FxHashMap<(SheetId, u32), (u64, Arc<str>)>,
    pub(crate) cols: FxHashMap<(SheetId, u32), (u64, Arc<str>)>,
    pub(crate) sheets: FxHashMap<SheetId, (u64, Arc<str>)>,
    seq: u64,
    compiled: FxHashMap<Arc<str>, Option<Arc<NumberFormat>>>,
    pub(crate) precision: bool,

    late: FxHashMap<usize, (std::sync::Weak<str>, Option<Arc<str>>)>,
}

impl CellFormats {
    pub(crate) fn any(&self) -> bool {
        !(self.map.is_empty()
            && self.rows.is_empty()
            && self.cols.is_empty()
            && self.sheets.is_empty())
    }

    pub(crate) fn line(&self, a: CellAddr) -> Option<&Arc<str>> {
        let r = self.rows.get(&(a.sheet, a.row));
        let c = self.cols.get(&(a.sheet, a.col));
        let s = self.sheets.get(&a.sheet);
        [r, c, s]
            .into_iter()
            .flatten()
            .max_by_key(|(seq, _)| *seq)
            .map(|(_, f)| f)
    }

    pub(crate) fn get(&self, a: CellAddr) -> Option<&Arc<str>> {
        self.map
            .get(&(a.sheet, a.row, a.col))
            .or_else(|| self.line(a))
    }
    pub(crate) fn is_late(&self, f: &Arc<str>) -> bool {
        self.late
            .contains_key(&(Arc::as_ptr(f) as *const u8 as usize))
    }

    pub(crate) fn counted(&self, a: CellAddr) -> Option<Arc<str>> {
        let f = self.get(a)?;
        match self.late.get(&(Arc::as_ptr(f) as *const u8 as usize)) {
            Some((_, prior)) => prior.clone(),
            None => Some(f.clone()),
        }
    }

    fn compile(&mut self, f: &Arc<str>) -> Option<Arc<NumberFormat>> {
        if let Some(c) = self.compiled.get(f) {
            return c.clone();
        }
        let c = NumberFormat::parse(f).ok().map(Arc::new);
        self.compiled.insert(f.clone(), c.clone());
        c
    }
}

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
        self.cellfmt.get(addr).map_or("General", |f| &**f)
    }

    pub fn debug_format(&self, addr: CellAddr) -> String {
        match self.cellfmt.get(addr) {
            None => "General".into(),
            Some(f) => format!(
                "{f}@{:x}{}",
                (Arc::as_ptr(f) as *const u8 as usize) & 0xffff,
                if self.cellfmt.is_late(f) { " LATE" } else { "" }
            ),
        }
    }

    pub(crate) fn cellfmt_has(&self, addr: CellAddr) -> bool {
        self.cellfmt
            .get(addr)
            .is_some_and(|f| !f.eq_ignore_ascii_case("general"))
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
            .get(addr)
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
        let Some(f) = self.cellfmt.get(addr).cloned() else {
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
        let Some(f) = self.cellfmt.get(addr).cloned() else {
            return false;
        };
        (or_text && &*f == "@") || self.cellfmt.compile(&f).is_some_and(|c| c.is_date())
    }

    pub fn precision_as_displayed(&self) -> bool {
        self.cellfmt.precision
    }

    pub(crate) fn own_format(&self, a: CellAddr) -> Option<Arc<str>> {
        self.cellfmt.map.get(&(a.sheet, a.row, a.col)).cloned()
    }

    pub(crate) fn restore_own_format(&mut self, a: CellAddr, f: Option<Arc<str>>) {
        let key = (a.sheet, a.row, a.col);
        match f {
            Some(f) => self.cellfmt.map.insert(key, f),
            None => self.cellfmt.map.remove(&key),
        };
    }

    pub(crate) fn store_format(&mut self, addr: CellAddr, fmt: &str) {
        let key = (addr.sheet, addr.row, addr.col);
        let fmt = if fmt.eq_ignore_ascii_case("general") {
            "General"
        } else {
            fmt
        };
        let line = self.cellfmt.line(addr).map_or("General", |f| &**f);
        if fmt == line {
            self.cellfmt.map.remove(&key);
        } else {
            self.cellfmt.map.insert(key, Arc::from(fmt));
        }
    }

    fn apply_format(&mut self, sheet: SheetId, rect: Rect, fmt: &str) {
        let fmt: Arc<str> = Arc::from(if fmt.is_empty() || fmt.eq_ignore_ascii_case("general") {
            "General"
        } else {
            fmt
        });
        let f = &mut self.cellfmt;
        if rect.is_full_columns() || rect.is_full_rows() {
            f.seq += 1;
            let line = (f.seq, fmt.clone());
            if rect.is_full_columns() && rect.is_full_rows() {
                f.map.retain(|k, _| k.0 != sheet);
                f.rows.retain(|k, _| k.0 != sheet);
                f.cols.retain(|k, _| k.0 != sheet);
                f.sheets.insert(sheet, line);
            } else if rect.is_full_columns() {
                f.map
                    .retain(|k, _| !(k.0 == sheet && (rect.c0..=rect.c1).contains(&k.2)));
                for c in rect.c0..=rect.c1 {
                    f.cols.insert((sheet, c), line.clone());
                }
            } else {
                f.map
                    .retain(|k, _| !(k.0 == sheet && (rect.r0..=rect.r1).contains(&k.1)));
                for r in rect.r0..=rect.r1 {
                    f.rows.insert((sheet, r), line.clone());
                }
            }
            return;
        }
        if rect.rows() as u64 * rect.cols() as u64 > 1 << 20 {
            return;
        }
        for r in rect.r0..=rect.r1 {
            for c in rect.c0..=rect.c1 {
                self.store_format(CellAddr::new(sheet, r, c), &fmt);
            }
        }
    }

    pub fn clear_formats(&mut self, sheet: SheetId, rect: Rect) -> RecalcReport {
        self.set_number_format_unrounded(sheet, rect, "General");
        self.unmerge_cells(sheet, rect)
    }

    pub fn reset_number_format(&mut self, sheet: SheetId, rect: Rect) {
        self.set_number_format_unrounded(sheet, rect, "General");
        let mode = std::mem::replace(&mut self.settings.mode, CalcMode::Manual);
        self.unmerge_cells(sheet, rect);
        self.settings.mode = mode;
    }

    pub(crate) fn set_number_format_unrounded(&mut self, sheet: SheetId, rect: Rect, fmt: &str) {
        if self.sheet(sheet).is_none() {
            return;
        }
        let before = self.late_candidates(sheet, rect);
        self.apply_format(sheet, rect, fmt);
        self.note_late_formats(sheet, before);
        self.note_table_col_formats(sheet, rect, fmt);
    }

    pub fn set_number_format(&mut self, sheet: SheetId, rect: Rect, fmt: &str) -> RecalcReport {
        self.set_number_format_body(sheet, rect, fmt)
    }

    fn set_number_format_body(&mut self, sheet: SheetId, rect: Rect, fmt: &str) -> RecalcReport {
        if self.sheet(sheet).is_none() || at_sign_rejected(fmt) {
            return RecalcReport::default();
        }
        let before = self.late_candidates(sheet, rect);
        self.apply_format(sheet, rect, fmt);
        self.note_late_formats(sheet, before);
        self.note_table_col_formats(sheet, rect, fmt);
        if !self.cellfmt.precision {
            return RecalcReport::default();
        }
        self.round_range(sheet, rect)
    }

    fn late_candidates(&self, sheet: SheetId, rect: Rect) -> Vec<(u32, u32, Option<Arc<str>>)> {
        let mut out = Vec::new();
        for t in self.tables.iter().filter(|t| t.sheet == sheet) {
            let (d0, d1) = t.data_rows();
            let Some(b) = rect.intersection(&Rect::new(d0, t.rect.c0, d1, t.rect.c1)) else {
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

    fn note_late_formats(&mut self, sheet: SheetId, cells: Vec<(u32, u32, Option<Arc<str>>)>) {
        for (r, c, prior) in cells {
            let Some(f) = self.cellfmt.map.get(&(sheet, r, c)) else {
                continue;
            };
            let own: Arc<str> = Arc::from(&**f);
            let key = Arc::as_ptr(&own) as *const u8 as usize;
            self.cellfmt.late.insert(key, (Arc::downgrade(&own), prior));
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
        let was = self.cellfmt.precision;
        self.cellfmt.precision = on;

        if !on && !was {
            return RecalcReport::default();
        }

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
        if mode == CalcMode::Automatic {
            calc::recalc(self)
        } else {
            RecalcReport::default()
        }
    }

    pub(crate) fn compiled_format(&mut self, addr: CellAddr) -> Option<Arc<NumberFormat>> {
        match self.cellfmt.get(addr).cloned() {
            Some(f) => self.cellfmt.compile(&f),
            None => self.cellfmt.compile(&Arc::from("General")),
        }
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
            let (a, b) = (self.format_class(&cur), self.format_class(&typed));
            if !yields || a == b || b == FormatClass::Other {
                return;
            }
        }
        self.store_format(addr, &typed);
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
            SheetRef::Current => host.sheet,
            SheetRef::Sheet(s) => *s,
            _ => return None,
        };
        let (r, c) = match *cref {
            CRef::Cell { row, col } => (pos(row, host.row), pos(col, host.col)),
            CRef::Area { r0, c0, r1, c1 } => (
                pos(r0, host.row).min(pos(r1, host.row)),
                pos(c0, host.col).min(pos(c1, host.col)),
            ),
            _ => return None,
        };
        if r < 0 || c < 0 {
            return None;
        }
        let f = self
            .cellfmt
            .get(CellAddr::new(sheet, r as u32, c as u32))
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
        let l = s.to_ascii_lowercase().replace("general", "");
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

fn at_sign_rejected(f: &str) -> bool {
    let secs = format_sections(f);
    let n = secs.len();

    if secs.iter().any(|s| s.contains("e+") || s.contains("e-")) {
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
