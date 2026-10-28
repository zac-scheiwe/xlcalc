pub mod api;
pub mod names;
mod op;
pub mod sheetops;
pub mod spill;
pub mod store;
pub mod tablelife;
pub mod tables;

use std::fmt;
use std::sync::atomic::{AtomicU64, Ordering as AtomicOrdering};
use std::sync::Arc;

use rustc_hash::{FxHashMap, FxHashSet};

use crate::address::{CellAddr, Rect, SheetId};
use crate::calc::{self, RecalcReport};
use crate::datetime::DateSystem;
use crate::formula::{self, CompileCtx, Formula, SheetResolver};
use crate::graph::{collect_precedents, DepGraph, Prec};
use crate::input::{self, Entered};
use crate::locale::Locale;
use crate::parser;
use crate::value::{Text, Value};

pub use names::{NameDef, Names};
pub(crate) use op::Op;
pub use spill::SpillIndex;
pub use store::{CellStore, Entry};

pub type FormulaId = u32;

pub const MAX_ARRAY_CELLS: u64 = 536_870_912 / 10;

pub const ABANDON_CELLS: u64 = 536_870_912 / 10;

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum CalcMode {
    Automatic,

    Manual,
}

#[derive(Clone, Debug)]
pub struct Settings {
    pub mode: CalcMode,

    pub iteration: bool,

    pub max_iterations: u32,

    pub max_change: f64,

    pub date_system: DateSystem,

    pub threads: usize,

    pub except_tables: bool,
}

impl Default for Settings {
    fn default() -> Self {
        Settings {
            mode: CalcMode::Automatic,
            iteration: false,
            max_iterations: 100,
            max_change: 0.001,
            date_system: DateSystem::D1900,
            threads: 0,
            except_tables: false,
        }
    }
}

pub struct Sheet {
    pub id: SheetId,

    pub name: String,

    pub(crate) cells: CellStore,

    pub(crate) spills: SpillIndex,

    pub(crate) dirty: DirtyIndex,
}

#[derive(Default)]
pub(crate) struct DirtyIndex {
    cols: std::collections::BTreeMap<u32, std::collections::BTreeSet<u32>>,
}

impl DirtyIndex {
    pub fn insert(&mut self, row: u32, col: u32) {
        self.cols.entry(col).or_default().insert(row);
    }

    pub fn remove(&mut self, row: u32, col: u32) {
        if let Some(rows) = self.cols.get_mut(&col) {
            rows.remove(&row);
            if rows.is_empty() {
                self.cols.remove(&col);
            }
        }
    }

    pub fn in_rect(&self, rect: &Rect, mut f: impl FnMut(u32, u32)) {
        for (&c, rows) in self.cols.range(rect.c0..=rect.c1) {
            for &r in rows.range(rect.r0..=rect.r1) {
                f(r, c);
            }
        }
    }
}

impl Sheet {
    pub fn used_bounds(&self) -> Option<Rect> {
        match (self.cells.bounds(), self.spills.bounds()) {
            (Some(a), Some(b)) => Some(a.union_bounds(&b)),
            (Some(a), None) | (None, Some(a)) => Some(a),
            (None, None) => None,
        }
    }

    pub fn entries(&self) -> impl Iterator<Item = (u32, u32, &Entry)> + '_ {
        self.cells.iter()
    }

    pub fn cell_count(&self) -> usize {
        self.cells.len()
    }

    pub fn bounded(&self, rect: &Rect) -> Option<Rect> {
        let used = match (self.cells.bounds(), self.spills.bounds()) {
            (Some(a), Some(b)) => a.union_bounds(&b),
            (Some(a), None) | (None, Some(a)) => a,
            (None, None) => return None,
        };
        rect.intersection(&used)
    }
}

pub struct FormulaCell {
    pub addr: CellAddr,

    pub(crate) formula: Arc<Formula>,

    pub text: String,

    pub value: Value,

    pub spill: Option<Rect>,

    pub dirty: bool,

    pub(crate) on_stack: bool,

    pub(crate) active: bool,

    pub(crate) seq: i64,

    pub(crate) born: i64,
    pub(crate) precedents: Vec<Prec>,

    pub(crate) circular_pass: u32,

    pub(crate) array: Option<(u32, u32)>,
}

impl FormulaCell {
    pub fn is_legacy_array(&self) -> bool {
        self.array.is_some()
    }

    pub fn value_at(&self, row: u32, col: u32) -> Value {
        if let (Some(rect), Value::Array(a)) = (self.spill, &self.value) {
            if rect.contains(row, col) {
                return a.get(row - rect.r0, col - rect.c0).clone();
            }
            return Value::Blank;
        }
        if row == self.addr.row && col == self.addr.col {
            match &self.value {
                Value::Array(a) => a.data[0].clone(),
                v => v.clone(),
            }
        } else {
            Value::Blank
        }
    }
}

pub struct CalcState {
    pub now: f64,

    pub current_year: i64,
    pub(crate) rng: AtomicU64,
    pub(crate) pass: u32,

    pub(crate) next_new_seq: i64,

    pub(crate) next_done_seq: i64,
    pub(crate) next_born: i64,

    pub(crate) spilling: FxHashSet<FormulaId>,

    pub(crate) shifted: FxHashSet<FormulaId>,

    pub(crate) stale_tables: bool,

    pub(crate) stale_copy: Vec<FormulaId>,
    pub(crate) volatile: FxHashSet<FormulaId>,
    pub(crate) circular: FxHashSet<FormulaId>,
    pub(crate) dirty_list: Vec<FormulaId>,

    pub circular_ref: Option<CellAddr>,

    pub circ_iterating: bool,

    pub fixed_now: Option<f64>,

    pub(crate) dt_seed: Vec<FormulaId>,
}

#[derive(Debug, Clone, PartialEq)]
pub enum EditError {
    Parse(parser::ParseError),

    Compile(String),

    NoSuchSheet,

    BadName(String),

    DuplicateSheet(String),

    Locked,

    OutOfScope(&'static str),
}

impl fmt::Display for EditError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            EditError::Parse(e) => write!(f, "formula error: {e}"),
            EditError::Compile(e) => write!(f, "formula error: {e}"),
            EditError::NoSuchSheet => f.write_str("no such sheet"),
            EditError::BadName(n) => write!(f, "invalid name '{n}'"),
            EditError::DuplicateSheet(n) => write!(f, "a sheet named '{n}' already exists"),
            EditError::Locked => f.write_str("cell is locked"),
            EditError::OutOfScope(what) => write!(f, "out of scope: {what}"),
        }
    }
}

impl std::error::Error for EditError {}

pub struct Workbook {
    pub(crate) sheets: Vec<Sheet>,
    next_sheet: u32,
    pub(crate) formulas: Vec<Option<FormulaCell>>,
    free: Vec<FormulaId>,

    pub(crate) names: Names,
    pub(crate) graph: DepGraph,

    pub(crate) settings: Settings,

    pub(crate) locale: Locale,

    pub(crate) calc: CalcState,

    pub(crate) tables: Vec<tables::Table>,

    pub(crate) tables_made: usize,

    pub(crate) cellfmt: crate::cellfmt::CellFormats,

    pub(crate) data_tables: Vec<crate::datatable::DataTable>,

    pub(crate) merges: crate::merge::Merges,

    pub(crate) session: Session,

    pub(crate) arrays_seen: bool,

    pub(crate) apostrophe: FxHashMap<CellAddr, Text>,

    op: op::OpFlags,

    pub(crate) views: FxHashMap<SheetId, crate::view::SheetView>,
}

pub(crate) struct Session {
    pub(crate) active: SheetId,
    pub(crate) selection: FxHashMap<SheetId, (u32, u32)>,
}

impl Default for Workbook {
    fn default() -> Self {
        Workbook::new()
    }
}

struct Resolve<'a> {
    wb: &'a Workbook,
    host: CellAddr,
}

impl SheetResolver for Resolve<'_> {
    fn sheet_id(&self, name: &str) -> Option<SheetId> {
        self.wb
            .sheets
            .iter()
            .find(|s| crate::text::eq_sheet_name(&s.name, name))
            .map(|s| s.id)
    }

    fn table(&self, name: Option<&str>) -> Option<(String, Vec<String>)> {
        let t = match name {
            Some(n) => self.wb.table_by_name(n),
            None => self.wb.table_at(self.host),
        }?;
        Some((t.name.clone(), t.columns.clone()))
    }
}

impl Workbook {
    pub fn new() -> Workbook {
        let mut wb = Workbook::empty();
        wb.add_sheet("Sheet1").unwrap();
        wb
    }

    pub fn empty() -> Workbook {
        let now = chrono::Local::now().naive_local();
        let serial = local_serial(now);
        Workbook {
            sheets: Vec::new(),
            session: Session {
                active: SheetId(0),
                selection: Default::default(),
            },
            op: op::OpFlags::IDLE,
            next_sheet: 1,
            formulas: Vec::new(),
            free: Vec::new(),
            names: Names::default(),
            graph: DepGraph::default(),
            settings: Settings::default(),
            locale: Locale::default(),
            calc: CalcState {
                now: serial,
                current_year: chrono::Datelike::year(&now) as i64,
                rng: AtomicU64::new(0x853c_49e6_748f_ea9b),
                pass: 0,
                next_new_seq: -1,
                next_done_seq: 1,
                next_born: 0,
                spilling: FxHashSet::default(),
                shifted: FxHashSet::default(),
                stale_tables: false,
                stale_copy: Vec::new(),
                volatile: FxHashSet::default(),
                circular: FxHashSet::default(),
                dirty_list: Vec::new(),
                circular_ref: None,
                circ_iterating: false,
                fixed_now: None,
                dt_seed: Vec::new(),
            },
            tables: Vec::new(),
            tables_made: 0,
            cellfmt: Default::default(),
            data_tables: Vec::new(),
            merges: Default::default(),
            arrays_seen: false,
            apostrophe: FxHashMap::default(),
            views: Default::default(),
        }
    }

    pub fn add_sheet(&mut self, name: &str) -> Result<SheetId, EditError> {
        if self.sheet_by_name(name).is_some() {
            return Err(EditError::DuplicateSheet(name.to_string()));
        }
        let id = SheetId(self.next_sheet);
        self.next_sheet += 1;
        self.session.active = id;
        self.sheets.push(Sheet {
            id,
            name: name.to_string(),
            cells: CellStore::default(),
            spills: SpillIndex::default(),
            dirty: DirtyIndex::default(),
        });

        self.recompile_all();
        Ok(id)
    }

    pub fn sheet(&self, id: SheetId) -> Option<&Sheet> {
        self.sheets.iter().find(|s| s.id == id)
    }

    pub(crate) fn sheet_mut(&mut self, id: SheetId) -> Option<&mut Sheet> {
        self.sheets.iter_mut().find(|s| s.id == id)
    }

    pub fn sheet_by_name(&self, name: &str) -> Option<&Sheet> {
        self.sheets
            .iter()
            .find(|s| crate::text::eq_sheet_name(&s.name, name))
    }

    pub fn sheet_position(&self, id: SheetId) -> Option<usize> {
        self.sheets.iter().position(|s| s.id == id)
    }

    pub fn sheet_at(&self, i: usize) -> &Sheet {
        &self.sheets[i]
    }

    pub fn sheet_count(&self) -> usize {
        self.sheets.len()
    }

    pub fn sheets(&self) -> impl Iterator<Item = &Sheet> {
        self.sheets.iter()
    }

    pub fn formula(&self, fid: FormulaId) -> &FormulaCell {
        self.formulas[fid as usize]
            .as_ref()
            .expect("live formula id")
    }

    pub(crate) fn formula_mut(&mut self, fid: FormulaId) -> &mut FormulaCell {
        self.formulas[fid as usize]
            .as_mut()
            .expect("live formula id")
    }

    pub(crate) fn set_dirty(&mut self, fid: FormulaId, dirty: bool) {
        let f = self.formulas[fid as usize]
            .as_mut()
            .expect("live formula id");
        if f.dirty == dirty {
            return;
        }
        f.dirty = dirty;
        let a = f.addr;
        if let Some(sh) = self.sheets.iter_mut().find(|s| s.id == a.sheet) {
            if dirty {
                sh.dirty.insert(a.row, a.col);
            } else {
                sh.dirty.remove(a.row, a.col);
            }
        }
    }

    pub fn formula_ids(&self) -> impl Iterator<Item = FormulaId> + '_ {
        self.formulas
            .iter()
            .enumerate()
            .filter(|(_, f)| f.is_some())
            .map(|(i, _)| i as FormulaId)
    }

    pub fn value(&self, addr: CellAddr) -> Value {
        let Some(sh) = self.sheet(addr.sheet) else {
            return Value::Blank;
        };
        match sh.cells.get(addr.row, addr.col) {
            Some(Entry::Const(v)) => v.clone(),
            Some(Entry::Formula(fid)) => self.formula(*fid).value_at(addr.row, addr.col),
            None => match sh.spills.owner_of(addr.row, addr.col) {
                Some(fid) => self.formula(fid).value_at(addr.row, addr.col),
                None => Value::Blank,
            },
        }
    }

    pub fn formula_at(&self, addr: CellAddr) -> Option<FormulaId> {
        match self.sheet(addr.sheet)?.cells.get(addr.row, addr.col)? {
            Entry::Formula(fid) => Some(*fid),
            _ => None,
        }
    }

    pub fn formula_text(&self, addr: CellAddr) -> Option<String> {
        let (fid, host) = match self.formula_at(addr) {
            Some(f) => (f, addr),
            None => {
                let (f, _) = self.array_at(addr)?;
                (f, self.formula(f).addr)
            }
        };
        Some(format!(
            "={}",
            crate::formula_print::print_node(self, &self.formula(fid).formula.root, host)
        ))
    }

    pub fn spill_anchor(&self, addr: CellAddr) -> Option<CellAddr> {
        let sh = self.sheet(addr.sheet)?;
        let fid = match sh.cells.get(addr.row, addr.col) {
            Some(Entry::Formula(f)) => *f,
            Some(_) => return None,
            None => sh.spills.owner_of(addr.row, addr.col)?,
        };
        let f = self.formula(fid);
        f.spill.map(|_| f.addr)
    }

    pub fn dependents(&self, addr: CellAddr) -> Vec<CellAddr> {
        let mut deps = Vec::new();
        self.graph
            .dependents_of_cell(addr.sheet, addr.row, addr.col, &mut deps);
        if let Some(fid) = self.formula_at(addr) {
            if let Some(rect) = self.formula(fid).spill {
                self.graph.dependents_of_rect(addr.sheet, &rect, &mut deps);
            }
        }
        let mut out: Vec<CellAddr> = deps.into_iter().map(|f| self.formula(f).addr).collect();
        out.sort();
        out.dedup();
        out.retain(|a| *a != addr);
        out
    }

    pub fn precedents(&self, addr: CellAddr) -> Vec<String> {
        let Some(fid) = self.formula_at(addr) else {
            return vec![];
        };
        let name = |s: SheetId| {
            self.sheet(s)
                .map(|s| s.name.clone())
                .unwrap_or_else(|| "#REF".into())
        };
        self.formula(fid)
            .precedents
            .iter()
            .map(|p| match p {
                Prec::Cell(s, r, c) => format!("{}!{}", name(*s), Rect::cell(*r, *c).a1()),
                Prec::Range(s, rect) => format!("{}!{}", name(*s), rect.a1()),
                Prec::Name(k) => match k.strip_prefix("\u{1}TABLE:") {
                    Some(t) => format!(
                        "table {}",
                        self.table_by_name(t).map_or(t, |x| x.name.as_str())
                    ),
                    None => k.to_string(),
                },
            })
            .collect::<Vec<_>>()
            .into_iter()
            .fold(Vec::new(), |mut v, p| {
                if !v.contains(&p) {
                    v.push(p);
                }
                v
            })
    }

    pub fn formula_count(&self) -> usize {
        self.formulas.len() - self.free.len()
    }

    pub fn random(&self) -> f64 {
        let mut z = self
            .calc
            .rng
            .fetch_add(0x9E37_79B9_7F4A_7C15, AtomicOrdering::Relaxed)
            .wrapping_add(0x9E37_79B9_7F4A_7C15);
        z = (z ^ (z >> 30)).wrapping_mul(0xBF58_476D_1CE4_E5B9);
        z = (z ^ (z >> 27)).wrapping_mul(0x94D0_49BB_1331_11EB);
        z ^= z >> 31;
        (z >> 11) as f64 / (1u64 << 53) as f64
    }

    pub fn seed_random(&mut self, seed: u64) {
        self.calc.rng.store(seed, AtomicOrdering::Relaxed);
    }

    fn note_placeholders(&mut self, f: &Formula, host: CellAddr) {
        let mut found: Vec<(Text, Text)> = vec![];
        formula::walk(&f.root, &mut |n| match n {
            formula::Node::Name {
                key,
                display,
                sheet: None,
            }
            | formula::Node::NamedCall { key, display, .. }
                if self.names.lookup(key, host.sheet).is_none() =>
            {
                found.push((display.to_uppercase().into(), display.clone()))
            }
            _ => {}
        });
        for (k, d) in found {
            self.names.note_placeholder(&k, &d);
        }
    }

    pub(crate) fn compile_formula(&self, text: &str, host: CellAddr) -> Result<Formula, EditError> {
        self.compile_normalized(text, host).map(|(f, _)| f)
    }

    pub fn compile_normalized(
        &self,
        text: &str,
        host: CellAddr,
    ) -> Result<(Formula, String), EditError> {
        let text = parser::r1c1::rewrite(text, host.row, host.col)
            .map_err(|m| EditError::Parse(parser::ParseError { pos: 0, message: m }))?;
        let ast = parser::parse(&text).map_err(EditError::Parse)?;
        self.compile_expr_normalized(&ast, host)
    }

    pub(crate) fn compile_expr_normalized(
        &self,
        ast: &parser::ast::Expr,
        host: CellAddr,
    ) -> Result<(Formula, String), EditError> {
        let resolver = Resolve { wb: self, host };
        let cx = CompileCtx {
            sheets: &resolver,
            host_row: host.row,
            host_col: host.col,
        };
        let f = formula::compile(ast, &cx).map_err(|e| EditError::Compile(e.0))?;
        let f = self.normalize_spans(f);

        let text = crate::formula_print::print_node(self, &f.root, host);
        Ok((f, text))
    }

    pub fn typed_text<'a>(&mut self, addr: CellAddr, input: &'a str) -> std::borrow::Cow<'a, str> {
        if !self.op_on(Op::ReplaceEntry) && self.fraction_first(addr) {
            if let Some(t) = crate::coerce::bare_fraction_as_mixed(input) {
                return t.into();
            }
        }
        input.into()
    }

    pub fn set_input(&mut self, addr: CellAddr, input: &str) -> Result<RecalcReport, EditError> {
        let one = Rect::new(addr.row, addr.col, addr.row, addr.col);
        let pending = self.totals_pending(addr.sheet, one);
        let r = self.set_input_raw(addr, input);
        if r.is_ok() && input.starts_with('\'') && input.len() > 1 {
            self.apostrophe.insert(addr, Text::from(&input[1..]));
        }
        if pending {
            self.restore_totals(addr.sheet, one, Some(one));
        }
        if r.is_ok() && !input.is_empty() {
            self.view_typed(addr);
        }
        r
    }

    fn set_input_raw(&mut self, addr: CellAddr, input: &str) -> Result<RecalcReport, EditError> {
        if !input.is_empty() && self.is_text_format(addr) && !self.op_on(Op::ReplaceEntry) {
            return Ok(self.set_value(addr, Value::from(input)));
        }
        let input = &*self.typed_text(addr, input);
        match input::interpret(input, &self.input_env()) {
            Entered::Formula(text) => self.set_formula(addr, &text),

            Entered::Value(_) if self.in_multi_cell_array(addr) => Err(EditError::Locked),
            Entered::Value(v) => {
                let grown = self.table_expansion(addr, false).and_then(|(ti, _, col)| {
                    (col && self.tables[ti].totals && addr.row == self.tables[ti].rect.r1)
                        .then_some(())
                });
                let in_totals = grown.is_some()
                    || self.tables.iter().any(|t| {
                        t.sheet == addr.sheet
                            && t.totals
                            && t.contains(addr.sheet, addr.row, addr.col)
                            && addr.row == t.rect.r1
                    });
                let v = if in_totals && !matches!(v, Value::Text(_)) {
                    Value::from(input)
                } else {
                    v
                };
                let typed = matches!(v, Value::Number(_));

                let in_header = typed
                    && self.tables.iter().any(|t| {
                        t.sheet == addr.sheet
                            && t.header_row() == Some(addr.row)
                            && (t.rect.c0..=t.rect.c1).contains(&addr.col)
                    });
                if in_header {
                    self.note_typed_format(addr, input);
                }
                let report = self.set_value(addr, v);
                if typed && !in_header {
                    self.note_typed_format(addr, input);
                }
                Ok(report)
            }
            Entered::Clear => Ok(self.clear(addr)),
        }
    }

    pub fn set_text_prefixed(
        &mut self,
        addr: CellAddr,
        text: &str,
    ) -> Result<RecalcReport, EditError> {
        let r = if !text.is_empty() && self.is_text_format(addr) {
            Ok(self.set_value(addr, Value::from(text)))
        } else {
            self.set_input(addr, &format!("'{text}"))
        };
        if r.is_ok() && !text.is_empty() {
            self.apostrophe.insert(addr, Text::from(text));
        }
        r
    }

    pub fn typed_format(&self, input: &str) -> Option<String> {
        let env = self.input_env();
        match input::interpret(input, &env) {
            Entered::Value(Value::Number(_)) => {
                let p = crate::coerce::parse_number_text(input, env)?;
                p.format.or_else(|| input::auto_format(&p.kind, &env))
            }
            _ => None,
        }
    }

    pub(crate) fn input_env(&self) -> crate::coerce::ParseEnv<'_> {
        crate::coerce::ParseEnv {
            locale: &self.locale,
            date_system: self.settings.date_system,
            current_year: self.calc.current_year,
            iso_t: true,
        }
    }

    pub fn set_formula(&mut self, addr: CellAddr, text: &str) -> Result<RecalcReport, EditError> {
        if self.sheet(addr.sheet).is_none() {
            return Err(EditError::NoSuchSheet);
        }
        if self.data_table_ignores(addr) {
            return Ok(RecalcReport::default());
        }
        if self.in_multi_cell_array(addr) || self.merged_hidden(addr) {
            return Err(EditError::Locked);
        }
        let body = input::auto_close_call(text.strip_prefix('=').unwrap_or(text));
        let body = body.as_ref();

        let widened = self
            .table_expansion(addr, true)
            .map(|(ti, rect, _)| (ti, std::mem::replace(&mut self.tables[ti].rect, rect)));
        let compiled = self.compile_normalized(body, addr);
        if let Some((ti, old)) = widened {
            self.tables[ti].rect = old;
        }
        let (formula, body) = compiled?;

        let formula = match crate::legacy::scalar_entry_expr(self, &body, addr)
            .and_then(|e| self.compile_expr_normalized(&e, addr).ok())
        {
            Some((f, _)) => f,
            None => formula,
        };
        self.note_placeholders(&formula, addr);
        let formula = Arc::new(formula);
        self.remove_cell(addr);
        let fid = self.alloc_formula(addr, formula, body);
        self.sheet_mut(addr.sheet)
            .unwrap()
            .cells
            .set(addr.row, addr.col, Entry::Formula(fid));
        self.link_formula(fid);
        {
            let root = self.formula(fid).formula.root.clone();
            if let Some(f) = self.infer_format(&root, addr) {
                if !self.cellfmt_has(addr)
                    || self.auto_replaces(&self.number_format(addr).to_string(), &f)
                {
                    self.store_format(addr, &f);
                }
            }
        }
        calc::cell_changed(self, addr);
        calc::mark_dirty(self, fid);
        if self.calc.stale_tables {
            self.refresh_stale_tables(fid);
        }
        if !self.tables.is_empty() {
            self.table_auto_expand(addr, true);

            let header = self.tables.iter().any(|t| {
                t.sheet == addr.sheet
                    && t.header_row() == Some(addr.row)
                    && (t.rect.c0..=t.rect.c1).contains(&addr.col)
            });

            if header && self.formula_at(addr).is_some() {
                self.remove_cell(addr);
                self.sheet_mut(addr.sheet).unwrap().cells.set(
                    addr.row,
                    addr.col,
                    Entry::Const(Value::Number(0.0)),
                );
            }
            self.header_edited(addr);
            self.table_formula_under(addr);
        }
        let report = self.after_edit(vec![addr]);

        if !self.tables.is_empty() && !self.op_on(Op::ReplaceEntry) {
            self.table_calc_fill(addr);
        }
        Ok(report)
    }

    fn refresh_stale_tables(&mut self, fid: FormulaId) {
        let mut tables: Vec<String> = Vec::new();
        let named = |f: &Formula, out: &mut Vec<String>| {
            crate::formula::walk(&f.root, &mut |n| {
                if let crate::formula::Node::StructRef { table, .. } = n {
                    out.push(table.to_string());
                }
            })
        };
        named(&self.formula(fid).formula, &mut tables);
        if tables.is_empty() {
            return;
        }
        self.calc.stale_tables = false;
        let ids: Vec<FormulaId> = self.formula_ids().collect();
        for g in ids {
            let mut mine = Vec::new();
            named(&self.formula(g).formula, &mut mine);
            if mine
                .iter()
                .any(|m| tables.iter().any(|t| crate::text::eq_ordinal_ci(m, t)))
            {
                calc::mark_dirty(self, g);
            }
        }
    }

    pub fn set_value(&mut self, addr: CellAddr, v: Value) -> RecalcReport {
        if v.is_blank() {
            return self.clear(addr);
        }
        if self.sheet(addr.sheet).is_none()
            || self.in_multi_cell_array(addr)
            || self.merged_hidden(addr)
        {
            return RecalcReport::default();
        }
        if self.data_table_ignores(addr) {
            return RecalcReport::default();
        }

        let v = match v {
            Value::Number(x) if !x.is_finite() => Value::Error(crate::value::XlError::Num),
            v => v,
        };
        let v = self.precision_const(addr, v);
        self.remove_cell(addr);
        self.sheet_mut(addr.sheet)
            .unwrap()
            .cells
            .set(addr.row, addr.col, Entry::Const(v));
        calc::cell_changed(self, addr);

        if !self.tables.is_empty() && self.merge_at(addr).is_none() {
            self.table_auto_expand(addr, false);
            self.header_edited(addr);
        }
        self.after_edit(vec![addr])
    }

    pub(crate) fn set_value_raw(&mut self, addr: CellAddr, v: Value) -> RecalcReport {
        if self.sheet(addr.sheet).is_none() {
            return RecalcReport::default();
        }
        if v.is_blank() {
            if self.remove_cell(addr) {
                calc::cell_changed(self, addr);
            }
            return self.after_edit(vec![addr]);
        }
        self.remove_cell(addr);
        self.sheet_mut(addr.sheet)
            .unwrap()
            .cells
            .set(addr.row, addr.col, Entry::Const(v));
        calc::cell_changed(self, addr);
        self.after_edit(vec![addr])
    }

    pub fn clear(&mut self, addr: CellAddr) -> RecalcReport {
        if !self.clear_core(addr) {
            return RecalcReport::default();
        }
        self.after_edit(vec![addr])
    }

    pub(crate) fn clear_core(&mut self, addr: CellAddr) -> bool {
        if self.sheet(addr.sheet).is_none()
            || self.data_table_ignores(addr)
            || self.in_multi_cell_array(addr)
        {
            return false;
        }
        let was_formula = !self.tables.is_empty() && self.formula_at(addr).is_some();
        let had = self.remove_cell(addr);
        if was_formula {
            self.forget_calc_snapshot(addr);
        }
        if had {
            calc::cell_changed(self, addr);
        }
        if !had && self.settings.mode == CalcMode::Manual {
            calc::block_spill_at(self, addr);
        }
        if !self.tables.is_empty() {
            self.header_edited(addr);
        }
        true
    }

    pub(crate) fn after_edit(&mut self, edited: Vec<CellAddr>) -> RecalcReport {
        let mut report = if self.settings.mode == CalcMode::Automatic {
            calc::recalc(self)
        } else {
            let mut cells: Vec<CellAddr> = Vec::new();
            for a in &edited {
                if let Some(sh) = self.sheet(a.sheet) {
                    for fid in sh.spills.anchors_wanting(a.row, a.col) {
                        if sh.spills.is_blocked(fid) {
                            continue;
                        }
                        let addr = self.formula(fid).addr;
                        if !cells.contains(&addr) {
                            cells.push(addr);
                        }
                    }
                }
            }
            for a in &edited {
                if !cells.contains(a) {
                    cells.push(*a);
                }
            }
            calc::recalc_only(self, &cells)
        };

        let have: rustc_hash::FxHashSet<CellAddr> = report.changed.iter().copied().collect();
        let mut seen = have.clone();
        let mut front: Vec<CellAddr> = Vec::new();
        for a in edited {
            if seen.insert(a) {
                front.push(a);
            }
        }
        if !front.is_empty() {
            front.reverse();
            front.append(&mut report.changed);
            report.changed = front;
        }
        report
    }

    pub(crate) fn remove_cell(&mut self, addr: CellAddr) -> bool {
        let Some(sh) = self.sheet_mut(addr.sheet) else {
            return false;
        };
        match sh.cells.remove(addr.row, addr.col) {
            None => false,
            Some(Entry::Const(_)) => true,
            Some(Entry::Formula(fid)) => {
                if !self.tables.is_empty() {
                    self.snapshot_calc_formula(addr, fid);
                }
                let sh = self.sheet_mut(addr.sheet).unwrap();
                sh.dirty.remove(addr.row, addr.col);
                let old_spill = sh.spills.remove(fid);
                self.unlink_formula(fid);
                if let Some(rect) = old_spill {
                    calc::rect_changed(self, addr.sheet, rect);
                }
                self.formulas[fid as usize] = None;
                self.free.push(fid);
                true
            }
        }
    }

    pub(crate) fn set_compiled_formula(
        &mut self,
        addr: CellAddr,
        formula: Arc<Formula>,
        text: String,
    ) {
        self.remove_cell(addr);
        let fid = self.alloc_formula(addr, formula, text);
        self.sheet_mut(addr.sheet)
            .unwrap()
            .cells
            .set(addr.row, addr.col, Entry::Formula(fid));
        self.link_formula(fid);
        calc::cell_changed(self, addr);
        calc::mark_dirty(self, fid);
    }

    pub(crate) fn free_formula(&mut self, fid: FormulaId) {
        self.formulas[fid as usize] = None;
        self.free.push(fid);
    }

    fn alloc_formula(&mut self, addr: CellAddr, formula: Arc<Formula>, text: String) -> FormulaId {
        let seq = self.calc.next_new_seq;
        self.calc.next_new_seq -= 1;
        let cell = FormulaCell {
            addr,
            formula,
            text,
            value: Value::Number(0.0),
            spill: None,
            dirty: false,
            on_stack: false,
            active: false,
            seq,
            born: {
                self.calc.next_born += 1;
                self.calc.next_born
            },
            precedents: Vec::new(),
            circular_pass: 0,
            array: None,
        };
        match self.free.pop() {
            Some(fid) => {
                self.formulas[fid as usize] = Some(cell);
                fid
            }
            None => {
                self.formulas.push(Some(cell));
                (self.formulas.len() - 1) as FormulaId
            }
        }
    }

    pub(crate) fn link_formula(&mut self, fid: FormulaId) {
        let f = self.formula(fid);
        let precs = collect_precedents(self, &f.formula, f.addr);
        let volatile = f.formula.volatile;
        self.graph.add(fid, &precs);
        if volatile {
            self.calc.volatile.insert(fid);
        }
        self.formula_mut(fid).precedents = precs;
    }

    pub(crate) fn unlink_formula(&mut self, fid: FormulaId) {
        let precs = std::mem::take(&mut self.formula_mut(fid).precedents);
        self.graph.remove(fid, &precs);
        self.calc.volatile.remove(&fid);
        self.calc.circular.remove(&fid);
    }

    fn recompile_all(&mut self) {
        let ids: Vec<FormulaId> = self.formula_ids().collect();

        self.with_op(Op::DtBlock, true, |this| {
            for fid in ids {
                let (text, addr) = {
                    let f = this.formula(fid);
                    (f.text.clone(), f.addr)
                };
                if let Ok(formula) = this.compile_formula(&text, addr) {
                    this.unlink_formula(fid);
                    this.formula_mut(fid).formula = Arc::new(formula);
                    this.link_formula(fid);
                    calc::mark_dirty(this, fid);
                }
            }
        });
        let names: Vec<NameDef> = self.names.iter().cloned().collect();
        for mut def in names {
            let mut missing = false;
            formula::walk(&def.formula.root, &mut |n| {
                missing |= matches!(
                    n,
                    formula::Node::Ref(formula::SheetRef::Missing(_), _)
                        | formula::Node::DeadRef(formula::SheetRef::Missing(_))
                        | formula::Node::Name {
                            sheet: Some(formula::SheetRef::Missing(_)),
                            ..
                        }
                );
            });
            if !missing {
                continue;
            }
            let host = def.host();
            if let Ok(f) = self.compile_formula(&def.refers_to, host) {
                def.formula = Arc::new(f);
                self.names.insert(def);
            }
        }
    }

    pub fn define_name_active(
        &mut self,
        name: &str,
        refers_to: &str,
        scope: Option<SheetId>,
    ) -> Result<RecalcReport, EditError> {
        let active = self.session.active;

        let builtin =
            name.eq_ignore_ascii_case("Print_Area") || name.eq_ignore_ascii_case("Print_Titles");
        let scope = match scope {
            None if !name.contains('!')
                && (builtin || self.names.get(&name.to_uppercase(), Some(active)).is_some()) =>
            {
                Some(active)
            }
            s => s,
        };
        self.define_name(name, refers_to, scope)
    }

    pub fn active_sheet(&self) -> SheetId {
        self.session.active
    }

    pub fn set_active_sheet(&mut self, sheet: SheetId) -> Result<(), EditError> {
        if self.sheet(sheet).is_none() {
            return Err(EditError::NoSuchSheet);
        }
        self.session.active = sheet;
        Ok(())
    }

    pub fn active_cell(&self) -> (u32, u32) {
        self.active_cell_of(self.session.active)
    }

    pub fn active_cell_of(&self, sheet: SheetId) -> (u32, u32) {
        let (r, c) = self
            .session
            .selection
            .get(&sheet)
            .copied()
            .unwrap_or((0, 0));
        match self.merges(sheet).into_iter().find(|m| m.contains(r, c)) {
            Some(m) => (m.r0, m.c0),
            None => (r, c),
        }
    }

    pub fn set_active_cell(&mut self, row: u32, col: u32) -> Result<(), EditError> {
        if row >= crate::address::MAX_ROWS || col >= crate::address::MAX_COLS {
            return Err(EditError::Compile("cell outside the grid".into()));
        }
        let sheet = self.session.active;
        self.session.selection.insert(sheet, (row, col));
        Ok(())
    }

    fn backward_cellish_span(&self, body: &str) -> bool {
        let b: Vec<char> = body.chars().collect();
        let word = |c: char| c.is_alphanumeric() || c == '_' || c == '.';
        let mut i = 0;
        while i < b.len() {
            match b[i] {
                q @ ('"' | '\'') => {
                    i += 1;
                    while i < b.len() {
                        if b[i] == q {
                            if b.get(i + 1) == Some(&q) {
                                i += 1;
                            } else {
                                break;
                            }
                        }
                        i += 1;
                    }
                    i += 1;
                }
                c if word(c) && (i == 0 || !word(b[i - 1])) => {
                    let s = i;
                    while i < b.len() && word(b[i]) {
                        i += 1;
                    }
                    if b.get(i) != Some(&':') {
                        continue;
                    }
                    let m = i + 1;
                    let mut e = m;
                    while e < b.len() && word(b[e]) {
                        e += 1;
                    }
                    if e == m || b.get(e) != Some(&'!') {
                        continue;
                    }
                    let x: String = b[s..i].iter().collect();
                    let y: String = b[m..e].iter().collect();
                    if crate::address::parse_a1_cell(&y).is_none()
                        || crate::address::parse_a1_cell(&x).is_some()
                    {
                        continue;
                    }
                    let pos = |n: &str| {
                        self.sheet_by_name(n)
                            .and_then(|sh| self.sheet_position(sh.id))
                    };
                    if let (Some(px), Some(py)) = (pos(&x), pos(&y)) {
                        if py < px {
                            return true;
                        }
                    }
                }
                _ => i += 1,
            }
        }
        false
    }

    pub fn define_name(
        &mut self,
        name: &str,
        refers_to: &str,
        scope: Option<SheetId>,
    ) -> Result<RecalcReport, EditError> {
        let (name, scope) = match name.rsplit_once('!') {
            Some((sheet, rest)) if scope.is_none() => {
                let sheet = sheet.trim_matches('\'');
                let id = self
                    .sheet_by_name(sheet)
                    .map(|s| s.id)
                    .ok_or_else(|| EditError::BadName(name.to_string()))?;
                (rest, Some(id))
            }
            _ => (name, scope),
        };
        let bad_name = !parser::is_valid_name(name)
            || scope.is_none() && parser::is_workbook_reserved_name(name);
        let host = CellAddr::new(scope.unwrap_or(SheetId(0)), 0, 0);

        let text;
        let body = match refers_to.strip_prefix('=') {
            Some(b) if !b.is_empty() => b,
            _ => {
                text = format!("\"{}\"", refers_to.replace('"', "\"\""));
                text.as_str()
            }
        };
        if self.backward_cellish_span(body) {
            return Err(EditError::Compile(
                "There's a problem with this formula.".into(),
            ));
        }
        let compiled = self.compile_formula(body, host)?;

        self.note_placeholders(&compiled, host);
        if bad_name {
            return Err(EditError::BadName(name.to_string()));
        }

        let ctx = scope.unwrap_or(self.session.active);
        let compiled = {
            let mut changed = false;
            let root = formula::map_node(&compiled.root, &mut |n| match n {
                formula::Node::Name {
                    key,
                    display,
                    sheet: None,
                } if self.names.get(key, Some(ctx)).is_some() => {
                    changed = true;
                    Some(formula::Node::Name {
                        key: key.clone(),
                        display: display.clone(),
                        sheet: Some(formula::SheetRef::Sheet(ctx)),
                    })
                }

                formula::Node::Ref(formula::SheetRef::Current, c) => {
                    changed = true;
                    Some(formula::Node::Ref(formula::SheetRef::Sheet(ctx), *c))
                }
                _ => None,
            });
            if changed {
                Formula {
                    root,
                    volatile: compiled.volatile,
                }
            } else {
                compiled
            }
        };
        let formula = Arc::new(compiled);

        if let Some(s) = scope {
            let key = name.to_uppercase();

            if self.names.get(&key, Some(s)).is_none()
                && !self.names.was_deleted(&key, s)
                && self.names.get(&key, None).is_some()
            {
                self.pin_book_level(&key, s);
            }
        }
        self.names.insert(NameDef {
            name: name.to_string(),
            scope,
            refers_to: body.to_string(),
            formula,
        });
        calc::name_changed(self, &name.to_uppercase());
        Ok(self.after_edit(vec![]))
    }

    fn pin_book_level(&mut self, key: &str, s: SheetId) {
        let dead = Some(formula::SheetRef::Sheet(SheetId(0)));
        let map = |root: &formula::Node| {
            let mut changed = false;
            let r = formula::map_node(root, &mut |n| match n {
                formula::Node::Name {
                    key: k,
                    display,
                    sheet: Some(formula::SheetRef::Sheet(x)),
                } if *x == s && &**k == key => {
                    changed = true;
                    Some(formula::Node::Name {
                        key: k.clone(),
                        display: display.clone(),
                        sheet: dead.clone(),
                    })
                }
                _ => None,
            });
            changed.then_some(r)
        };
        let fids: Vec<FormulaId> = self.formula_ids().collect();
        for fid in fids {
            let f = self.formula(fid).formula.clone();
            if let Some(root) = map(&f.root) {
                self.formula_mut(fid).formula = Arc::new(Formula {
                    root,
                    volatile: f.volatile,
                });
            }
        }
        let defs: Vec<NameDef> = self.names.iter().cloned().collect();
        for d in defs {
            if let Some(root) = map(&d.formula.root) {
                self.names.insert(NameDef {
                    formula: Arc::new(Formula {
                        root,
                        volatile: d.formula.volatile,
                    }),
                    ..d
                });
            }
        }
    }

    pub fn set_mode(&mut self, mode: CalcMode) -> RecalcReport {
        self.settings.mode = mode;
        self.settings.except_tables = false;
        if mode == CalcMode::Automatic {
            calc::recalc(self)
        } else {
            RecalcReport::default()
        }
    }

    pub fn settings(&self) -> &Settings {
        &self.settings
    }

    pub fn restore_settings(&mut self, settings: Settings) -> RecalcReport {
        self.settings = settings;
        self.full_recalc()
    }

    pub fn set_date_system(&mut self, ds: DateSystem) -> RecalcReport {
        self.settings.date_system = ds;
        if self.settings.mode == CalcMode::Automatic {
            self.full_recalc()
        } else {
            RecalcReport::default()
        }
    }

    pub fn set_threads(&mut self, threads: usize) {
        self.settings.threads = threads;
    }

    pub fn names(&self) -> &Names {
        &self.names
    }

    pub fn tables(&self) -> &[tables::Table] {
        &self.tables
    }

    pub fn has_data_tables(&self) -> bool {
        !self.data_tables.is_empty()
    }

    pub fn locale(&self) -> &Locale {
        &self.locale
    }

    pub fn set_locale(&mut self, locale: Locale) -> RecalcReport {
        self.locale = locale;
        if self.settings.mode == CalcMode::Automatic {
            self.full_recalc()
        } else {
            RecalcReport::default()
        }
    }

    pub fn circular_ref(&self) -> Option<CellAddr> {
        if self.calc.circ_iterating {
            None
        } else {
            self.calc.circular_ref
        }
    }

    pub fn set_iteration(
        &mut self,
        enabled: bool,
        max_iter: Option<u32>,
        max_change: Option<f64>,
    ) -> RecalcReport {
        let mut report = RecalcReport::default();
        let mut step = |wb: &mut Workbook| {
            if wb.settings.iteration && wb.settings.mode == CalcMode::Automatic {
                let r = calc::recalc(wb);
                report.evaluations += r.evaluations;
                report.changed.extend(r.changed);
                report.circular = r.circular;
            }
        };
        self.settings.iteration = enabled;

        if self.settings.mode == CalcMode::Automatic {
            self.calc.circ_iterating = enabled;
        }
        step(self);
        if let Some(n) = max_iter {
            self.settings.max_iterations = n.min(32_767);
            step(self);
        }

        if let Some(x) = max_change.filter(|x| !x.is_nan()) {
            self.settings.max_change = x;
            step(self);
        }
        report
    }

    pub fn recalc(&mut self) -> RecalcReport {
        calc::recalc_with(self, true)
    }

    pub fn full_recalc(&mut self) -> RecalcReport {
        let ids: Vec<FormulaId> = self.formula_ids().collect();
        for fid in ids {
            calc::mark_dirty(self, fid);
        }
        calc::recalc_with(self, true)
    }

    pub fn rebuild(&mut self) -> RecalcReport {
        self.graph = DepGraph::default();
        self.calc.volatile.clear();
        let ids: Vec<FormulaId> = self.formula_ids().collect();
        for &fid in &ids {
            self.formula_mut(fid).precedents.clear();
            self.link_formula(fid);
        }
        self.full_recalc()
    }
}

fn local_serial(now: chrono::NaiveDateTime) -> f64 {
    use chrono::{Datelike, Timelike};
    let days = crate::datetime::serial_from_ymd(
        DateSystem::D1900,
        now.year() as i64,
        now.month() as i64,
        now.day() as i64,
    );
    let secs = now.num_seconds_from_midnight() as f64 + now.nanosecond() as f64 / 1e9;
    days as f64 + secs / 86_400.0
}

pub(crate) fn current_serial() -> f64 {
    local_serial(chrono::Local::now().naive_local())
}
