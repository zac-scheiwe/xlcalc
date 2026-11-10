pub mod api;
mod chain;
mod edit;
pub mod ext;
pub mod names;
pub mod notice;
mod op;
mod passes;
mod sbr;
pub mod sheetops;
pub mod spill;
pub mod store;
pub mod tablelife;
pub mod tables;
pub mod track;
mod transfer;

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
pub use notice::{paste_area, Command, Notice, Outcome};
pub(crate) use op::Op;
pub use spill::SpillIndex;
pub use store::{CellStore, Entry};
pub use track::TrackId;

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

    pub multi_threaded: bool,

    pub force_full: bool,

    pub force_full_seen: bool,
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
            multi_threaded: true,
            force_full: false,
            force_full_seen: false,
        }
    }
}

#[derive(Clone, Copy, Debug, Default, PartialEq, Eq)]
pub enum SheetState {
    #[default]
    Visible,

    Hidden,

    VeryHidden,
}

#[derive(Clone, Copy, Debug, Default, PartialEq, Eq)]
pub enum SheetKind {
    #[default]
    Worksheet,

    Chart,

    Macro,

    MacroIntl,
}

impl SheetKind {
    pub fn is_macro(self) -> bool {
        matches!(self, SheetKind::Macro | SheetKind::MacroIntl)
    }
}

pub struct Sheet {
    pub id: SheetId,

    pub name: String,

    pub state: SheetState,

    pub kind: SheetKind,

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

    pub(crate) partial: Option<Box<PartialCirc>>,

    pub(crate) stored: u64,

    pub(crate) spill_kept: bool,

    pub(crate) spill_src: bool,

    pub(crate) member_retry: bool,

    pub(crate) share: Option<u32>,
}

#[derive(Clone, Debug)]
pub(crate) struct PartialCirc {
    pub(crate) pass: u32,

    pub(crate) mask: Vec<bool>,

    pub(crate) sources: Vec<FormulaId>,

    pub(crate) tries: u8,
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

    pub(crate) chain: std::collections::BTreeMap<i64, FormulaId>,

    pub(crate) unseen: crate::host::Unseen,

    pub(crate) waited: FxHashMap<FormulaId, (u32, FormulaId)>,

    pub(crate) stores: u64,
    pub(crate) next_born: i64,

    pub(crate) skip_volatile: bool,

    pub(crate) skip_host_volatile: bool,

    pub(crate) spilling: FxHashSet<FormulaId>,

    pub(crate) shifted: FxHashSet<FormulaId>,

    pub(crate) keep_walk: bool,

    pub(crate) no_empty_call: bool,

    pub(crate) dead_pending: bool,

    pub(crate) stale_tables: bool,

    pub(crate) stale_copy: Vec<FormulaId>,
    pub(crate) volatile: FxHashSet<FormulaId>,

    pub(crate) host_volatile: FxHashSet<FormulaId>,

    pub(crate) cmd_manual: bool,
    pub(crate) circular: FxHashSet<FormulaId>,
    pub(crate) dirty_list: Vec<FormulaId>,

    pub(crate) spill_dirty: Vec<FormulaId>,

    pub(crate) read_spill: bool,

    pub(crate) partials: Vec<FormulaId>,

    pub circular_ref: Vec<CellAddr>,

    pub circ_iterating: bool,

    pub fixed_now: Option<f64>,

    pub(crate) dt_seed: Vec<FormulaId>,

    pub(crate) relink_scope: Option<crate::structure::DirtyScope>,

    pub(crate) entered: Vec<FormulaId>,

    pub(crate) entered_groups: Vec<usize>,

    pub(crate) chain_epoch: u64,

    pub(crate) walk: crate::walk::WalkState,
}

#[derive(Debug, Clone, PartialEq)]
pub enum EditError {
    Parse(parser::ParseError),

    Compile(String),

    NoSuchSheet,

    NoVisibleSheet,

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
            EditError::NoVisibleSheet => {
                f.write_str("a workbook must contain at least one visible worksheet")
            }
            EditError::BadName(n) => write!(f, "invalid name '{n}'"),
            EditError::DuplicateSheet(n) => write!(f, "a sheet named '{n}' already exists"),
            EditError::Locked => f.write_str("cell is locked"),
            EditError::OutOfScope(what) => write!(f, "out of scope: {what}"),
        }
    }
}

impl std::error::Error for EditError {}

pub(crate) type TtcBlock = (SheetId, Rect, Vec<(u32, u32)>);

pub struct Workbook {
    pub(crate) sheets: Vec<Sheet>,

    pub(crate) ext_sheets: Vec<Sheet>,
    pub(crate) ext: ext::ExtCell,
    next_sheet: u32,

    book_name: Option<String>,
    pub(crate) formulas: Vec<Option<FormulaCell>>,
    free: Vec<FormulaId>,

    pub(crate) names: Names,
    pub(crate) graph: DepGraph,

    pub(crate) settings: Settings,

    pub(crate) locale: Locale,

    pub(crate) input_locale: Option<Locale>,

    pub(crate) calc: CalcState,

    pub(crate) tables: Vec<tables::Table>,

    pub(crate) tables_made: usize,

    pub(crate) cellfmt: crate::cellfmt::CellFormats,

    pub(crate) styles: crate::style::StyleTable,

    pub(crate) data_tables: Vec<crate::datatable::DataTable>,

    pub(crate) merges: crate::merge::Merges,

    pub(crate) cfv: FxHashMap<SheetId, crate::cfv::SheetCfv>,

    pub(crate) drawings: crate::drawing::Drawings,

    pub(crate) session: Session,

    pub(crate) arrays_seen: bool,

    pub(crate) apostrophe: FxHashMap<CellAddr, Text>,

    pub insert_format_below: bool,

    op: op::OpFlags,

    pub(crate) ttc_block: Option<TtcBlock>,

    pub(crate) notes: notice::Notes,

    pub(crate) passes: passes::Passes,

    pub(crate) views: FxHashMap<SheetId, crate::view::SheetView>,

    pub(crate) scenarios: FxHashMap<SheetId, crate::scenarios::SheetScenarios>,

    pub(crate) sparklines: FxHashMap<SheetId, Vec<crate::sparkline::SparkGroup>>,
    pub(crate) spark_next: u32,

    pub(crate) hyperlinks: FxHashMap<SheetId, Vec<crate::hyperlink::Hyperlink>>,
    pub(crate) hl_next: u32,
    pub(crate) cell_notes: FxHashMap<SheetId, crate::notes::NoteMap>,
    pub(crate) note_next: u32,

    pub(crate) spark_free: FxHashMap<SheetId, Vec<u32>>,
    pub(crate) slicer_caches: Vec<crate::slicer::SlicerCache>,

    pub(crate) pivots: crate::pivot::Pivots,

    pub(crate) tracks: track::Tracks,

    pub(crate) sbr: sbr::Sbr,

    pub(crate) function_host: Option<std::sync::Arc<dyn crate::host::FunctionHost>>,

    pub(crate) rtd_source: Option<std::sync::Arc<dyn crate::host::RtdSource>>,

    pub(crate) share_plan: Option<crate::share::SharePlan>,

    pub(crate) next_share: u32,
}

pub(crate) struct Session {
    pub(crate) active: SheetId,
    pub(crate) selection: FxHashMap<SheetId, Selection>,

    pub(crate) fill_selected: FxHashMap<SheetId, (u32, u32)>,

    pub(crate) rows_over_header: bool,

    pub(crate) last_format: Option<String>,
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct Selection {
    pub areas: Vec<Rect>,

    pub active: (u32, u32),
}

impl Selection {
    pub fn cell(row: u32, col: u32) -> Selection {
        Selection {
            areas: vec![Rect::cell(row, col)],
            active: (row, col),
        }
    }

    pub fn of(areas: &[Rect]) -> Selection {
        let first = areas.first().copied().unwrap_or(Rect::cell(0, 0));
        Selection {
            areas: if areas.is_empty() {
                vec![first]
            } else {
                areas.to_vec()
            },
            active: (first.r0, first.c0),
        }
    }

    pub fn contains(&self, row: u32, col: u32) -> bool {
        self.areas.iter().any(|a| a.contains(row, col))
    }
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
    fn ext_sheet(&self, book: &str, sheet: &str) -> Option<SheetId> {
        self.wb.ext_resolve(book, sheet).ok()
    }

    fn ext_book(&self, book: &str) -> bool {
        self.wb.ext_touch_book(book)
    }

    fn sheet_id(&self, name: &str) -> Option<SheetId> {
        self.wb
            .sheets
            .iter()
            .find(|s| crate::text::eq_sheet_name(&s.name, name))
            .map(|s| s.id)
    }

    fn is_chart_sheet(&self, id: SheetId) -> bool {
        self.wb
            .sheet(id)
            .is_some_and(|s| s.kind == SheetKind::Chart)
    }

    fn macro_host(&self) -> bool {
        formula::FORCE_MACRO.get()
            || self
                .wb
                .sheet(self.host.sheet)
                .is_some_and(|s| s.kind.is_macro())
    }

    fn table(&self, name: Option<&str>) -> Option<(String, Vec<String>)> {
        let t = match name {
            Some(n) => self.wb.table_by_name(n),
            None => self.wb.table_at(self.host),
        }?;
        Some((t.name.clone(), t.columns.clone()))
    }

    fn bind_function(&self, name: &str) -> Option<String> {
        self.wb.function_host.as_ref()?.bind(name)
    }

    fn name_exists(&self, name: &str) -> bool {
        self.wb
            .names
            .lookup(&name.to_uppercase(), self.host.sheet)
            .is_some()
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
            ext_sheets: Vec::new(),
            ext: Default::default(),
            session: Session {
                active: SheetId(0),
                selection: Default::default(),
                fill_selected: Default::default(),
                rows_over_header: false,
                last_format: Some("General".into()),
            },
            op: op::OpFlags::IDLE,
            ttc_block: None,
            notes: Default::default(),
            passes: Default::default(),
            next_sheet: 1,
            book_name: None,
            formulas: Vec::new(),
            free: Vec::new(),
            names: Names::default(),
            graph: DepGraph::default(),
            settings: Settings::default(),
            locale: Locale::default(),
            input_locale: None,
            calc: CalcState {
                now: serial,
                current_year: chrono::Datelike::year(&now) as i64,
                rng: AtomicU64::new(0x853c_49e6_748f_ea9b),
                pass: 0,
                next_new_seq: -1,
                chain: Default::default(),
                unseen: Default::default(),
                waited: Default::default(),
                stores: 0,
                next_born: 0,
                skip_volatile: false,
                skip_host_volatile: false,
                spilling: FxHashSet::default(),
                shifted: FxHashSet::default(),
                keep_walk: false,
                no_empty_call: false,
                dead_pending: false,
                stale_tables: false,
                stale_copy: Vec::new(),
                volatile: FxHashSet::default(),
                host_volatile: FxHashSet::default(),
                cmd_manual: false,
                circular: FxHashSet::default(),
                dirty_list: Vec::new(),
                spill_dirty: Vec::new(),
                read_spill: false,
                partials: Vec::new(),
                circular_ref: Vec::new(),
                circ_iterating: false,
                fixed_now: None,
                dt_seed: Vec::new(),
                relink_scope: None,
                entered: Vec::new(),
                entered_groups: Vec::new(),
                chain_epoch: 0,
                walk: Default::default(),
            },
            tables: Vec::new(),
            tables_made: 0,
            cellfmt: Default::default(),
            styles: Default::default(),
            data_tables: Vec::new(),
            merges: Default::default(),
            cfv: Default::default(),
            drawings: Default::default(),
            arrays_seen: false,
            apostrophe: FxHashMap::default(),
            insert_format_below: false,
            views: Default::default(),
            scenarios: Default::default(),
            sparklines: Default::default(),
            spark_free: Default::default(),
            spark_next: 0,
            hyperlinks: Default::default(),
            hl_next: 0,
            cell_notes: Default::default(),
            note_next: 0,
            slicer_caches: Vec::new(),
            pivots: Default::default(),
            tracks: Default::default(),
            sbr: Default::default(),
            function_host: None,
            rtd_source: None,
            share_plan: None,
            next_share: 0,
        }
    }

    pub fn add_sheet(&mut self, name: &str) -> Result<SheetId, EditError> {
        self.command(Command::AddSheet, |wb| wb.add_sheet_run(name))
    }

    pub fn add_sheet_before(
        &mut self,
        name: &str,
        before: Option<SheetId>,
    ) -> Result<SheetId, EditError> {
        self.command(Command::AddSheet, |wb| {
            let id = wb.add_sheet_run(name)?;
            wb.place_added_sheet(id, before)?;
            Ok(id)
        })
    }

    fn place_added_sheet(&mut self, id: SheetId, before: Option<SheetId>) -> Result<(), EditError> {
        if before.is_none() {
            return Ok(());
        }
        self.calc.skip_volatile = true;
        let r = self.move_sheet(id, before);
        self.calc.skip_volatile = false;
        self.calc.dead_pending = true;
        r.map(|_| ())
    }

    pub fn add_chart_sheet_before(
        &mut self,
        name: &str,
        before: Option<SheetId>,
    ) -> Result<SheetId, EditError> {
        self.command(Command::AddSheet, |wb| {
            let id = wb.add_sheet_kind(name, SheetKind::Chart)?;
            wb.move_sheet(id, before)?;
            Ok(id)
        })
    }

    pub fn add_macro_sheet_before(
        &mut self,
        name: &str,
        intl: bool,
        before: Option<SheetId>,
    ) -> Result<SheetId, EditError> {
        let kind = if intl {
            SheetKind::MacroIntl
        } else {
            SheetKind::Macro
        };
        self.command(Command::AddSheet, |wb| {
            let id = wb.add_sheet_kind(name, kind)?;
            wb.move_sheet(id, before)?;
            Ok(id)
        })
    }

    pub fn set_name_macro_type(&mut self, key: &str, scope: Option<SheetId>, t: Option<i32>) {
        self.names.set_macro_type(key, scope, t);
    }

    pub fn compile_xlm(
        &self,
        text: &str,
        host: CellAddr,
    ) -> Result<std::sync::Arc<Formula>, EditError> {
        formula::FORCE_MACRO.set(true);
        formula::NO_BANG.set(true);
        let r = self.compile_formula(text, host);
        formula::NO_BANG.set(false);
        formula::FORCE_MACRO.set(false);
        r.map(std::sync::Arc::new)
    }

    pub fn add_sheet_of_kind(&mut self, name: &str, kind: SheetKind) -> Result<SheetId, EditError> {
        self.command(Command::AddSheet, |wb| wb.add_sheet_kind(name, kind))
    }

    fn add_sheet_run(&mut self, name: &str) -> Result<SheetId, EditError> {
        self.add_sheet_kind(name, SheetKind::Worksheet)
    }

    fn add_sheet_kind(&mut self, name: &str, kind: SheetKind) -> Result<SheetId, EditError> {
        if self.sheet_by_name(name).is_some() {
            return Err(EditError::DuplicateSheet(name.to_string()));
        }
        let id = SheetId(self.next_sheet);
        self.next_sheet += 1;
        self.session.active = id;
        self.sheets.push(Sheet {
            id,
            name: name.to_string(),
            state: SheetState::Visible,
            kind,
            cells: CellStore::default(),
            spills: SpillIndex::default(),
            dirty: DirtyIndex::default(),
        });

        self.recompile_all();
        self.note_sheet(Notice::SheetAdded(id));
        self.calc.dead_pending = true;
        Ok(id)
    }

    pub fn sheet(&self, id: SheetId) -> Option<&Sheet> {
        if id.0 >= ext::EXT_BASE {
            return self.ext_sheets.iter().find(|s| s.id == id);
        }
        self.sheets.iter().find(|s| s.id == id)
    }

    pub(crate) fn sheet_mut(&mut self, id: SheetId) -> Option<&mut Sheet> {
        if id.0 >= ext::EXT_BASE {
            return self.ext_sheets.iter_mut().find(|s| s.id == id);
        }
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

    pub fn book_name(&self) -> &str {
        self.book_name.as_deref().unwrap_or("Book1")
    }

    pub fn set_book_name(&mut self, name: &str) {
        self.book_name = Some(name.to_string());
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
        if dirty && self.calc.walk.tracking() {
            self.calc.walk.dirtied.push(fid);
        }
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
        let (mut row, mut col) = (addr.row, addr.col);
        let fid = match sh.cells.get(addr.row, addr.col) {
            Some(Entry::Const(_)) if self.ext_const_closed(addr.sheet) => {
                return Value::Error(crate::XlError::REF)
            }
            Some(Entry::Const(v)) => return v.clone(),
            Some(Entry::Formula(fid)) => *fid,
            None => match sh.spills.owner_of(addr.row, addr.col) {
                Some(fid) => {
                    match sh.spills.source(fid, row, col) {
                        Some(spill::SpillSource::Zero) => return Value::Number(0.0),
                        Some(spill::SpillSource::Value(v)) => return v,
                        Some(spill::SpillSource::At(r, c)) => (row, col) = (r, c),
                        None => {}
                    }
                    fid
                }
                None => return Value::Blank,
            },
        };
        let f = self.formula(fid);
        let uncalculated = f.dirty && !f.active || self.calc.unseen.is_late(fid, self.calc.pass);
        if uncalculated && self.calc.unseen.hides(addr) {
            return Value::Blank;
        }
        f.value_at(row, col)
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

    pub(crate) fn note_rejected_names(&mut self, body: &str, pos: usize, host: CellAddr) {
        use crate::parser::ast::SheetSpec;
        use crate::parser::lexer::{Lexer, Tok};
        let exists = |name: &str| {
            self.sheets
                .iter()
                .any(|s| crate::text::eq_sheet_name(&s.name, name))
        };

        let mut pos = pos;
        let toks = loop {
            let prefix: String = body.chars().take(pos).collect();
            match Lexer::new(&prefix, &exists).tokenize() {
                Ok(t) => break t,
                Err(_) if pos == 0 => return,

                Err(e) => pos = e.pos.min(pos - 1),
            }
        };
        let mut found = Vec::new();
        for (i, t) in toks.iter().enumerate() {
            let Tok::Ident(w) = &t.tok else {
                continue;
            };
            let sheet_ok = match i.checked_sub(1).map(|j| &toks[j].tok) {
                Some(Tok::Sheet(SheetSpec::Single(s))) => exists(s),
                Some(Tok::Sheet(_)) => false,
                _ => true,
            };
            if !sheet_ok {
                continue;
            }
            let call = matches!(toks.get(i + 1).map(|n| &n.tok), Some(Tok::Op("(")));
            let text = if call { format!("{w}()") } else { w.clone() };
            if let Ok(f) = self.compile_formula(&text, host) {
                found.push(f);
            }
        }
        for f in found {
            self.note_placeholders(&f, host);
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
        if text.starts_with('=') || text.trim_matches(' ').is_empty() {
            return Err(EditError::Parse(parser::ParseError {
                pos: 0,
                message: "empty or doubled equals sign".into(),
            }));
        }
        let text = parser::r1c1::rewrite(text, host.row, host.col)
            .map_err(|m| EditError::Parse(parser::ParseError { pos: 0, message: m }))?;
        let exists = |name: &str| {
            self.sheets
                .iter()
                .any(|s| crate::text::eq_sheet_name(&s.name, name))
        };
        let ast = parser::parse_with(&text, &exists).map_err(EditError::Parse)?;
        let r = self.compile_expr_normalized(&ast, host);
        ast.drop_deep();
        r
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

        let mut on_chart = false;
        formula::walk(&f.root, &mut |n| {
            if let formula::Node::Ref(formula::SheetRef::Missing(name), _) = n {
                on_chart |= self
                    .sheet_by_name(name)
                    .is_some_and(|s| s.kind == SheetKind::Chart);
            }
        });
        if on_chart {
            return Err(EditError::Compile(
                "a formula cannot refer to a chart sheet".into(),
            ));
        }
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
        self.command(Command::Entry, |wb| {
            wb.note_changed(addr.sheet, &[notice::cell(addr)]);
            wb.set_input_run(addr, input)
        })
    }

    fn set_input_run(&mut self, addr: CellAddr, input: &str) -> Result<RecalcReport, EditError> {
        let one = Rect::new(addr.row, addr.col, addr.row, addr.col);
        let pending = self.totals_pending(addr.sheet, one);
        let r = self.set_input_raw(addr, input);
        if r.is_ok() && input.starts_with('\'') {
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
        let r = self.set_input_raw_body(addr, input);
        if r.is_ok() {
            self.wrap_for_line_break(addr);
        }
        r
    }

    fn set_input_raw_body(
        &mut self,
        addr: CellAddr,
        input: &str,
    ) -> Result<RecalcReport, EditError> {
        if !input.is_empty() && self.is_text_format(addr) && !self.op_on(Op::ReplaceEntry) {
            let text = input.strip_prefix('\'').unwrap_or(input);
            return Ok(self.set_value(addr, Value::from(text)));
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
                let typed = matches!(v, Value::Number(_))
                    || matches!(&v, Value::Text(t) if self.text_date_format(input, &self.input_env()).is_some() && t.as_ref() == input)
                    || (matches!(v, Value::Text(_))
                        && input::is_overflow_scientific(input, &self.input_env()));

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

    pub fn has_apostrophe(&self, addr: CellAddr) -> bool {
        match self.value(addr) {
            Value::Text(t) => self.apostrophe.get(&addr).is_some_and(|a| *a == t),
            _ => false,
        }
    }

    pub fn mark_apostrophe(&mut self, addr: CellAddr) {
        if let Value::Text(t) = self.value(addr) {
            self.apostrophe.insert(addr, t);
        }
    }

    pub fn set_text_prefixed(
        &mut self,
        addr: CellAddr,
        text: &str,
    ) -> Result<RecalcReport, EditError> {
        self.command(Command::Entry, |wb| {
            wb.note_changed(addr.sheet, &[notice::cell(addr)]);
            wb.set_text_prefixed_run(addr, text)
        })
    }

    fn set_text_prefixed_run(
        &mut self,
        addr: CellAddr,
        text: &str,
    ) -> Result<RecalcReport, EditError> {
        let r = if text.is_empty() {
            Ok(self.set_value(addr, Value::from("")))
        } else if self.is_text_format(addr) {
            let r = self.set_value(addr, Value::from(text));
            self.wrap_for_line_break(addr);
            Ok(r)
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
        if input::wrapped_exponent(input).is_some() {
            return Some("0.00E+00".to_string());
        }
        match input::interpret(input, &env) {
            Entered::Value(Value::Text(_)) if input::is_overflow_scientific(input, &env) => {
                Some("0.00E+00".to_string())
            }
            Entered::Value(Value::Number(_)) => {
                let p = crate::coerce::parse_number_text(input, env)?;
                p.format.or_else(|| input::auto_format(&p.kind, &env))
            }
            Entered::Value(Value::Text(_)) => self.text_date_format(input, &env),
            _ => None,
        }
    }

    fn text_date_format(&self, input: &str, env: &crate::coerce::ParseEnv) -> Option<String> {
        let t = input.trim_end();
        let (head, last) = t.rsplit_once(' ')?;
        let (head, time, marked) =
            if last.eq_ignore_ascii_case("am") || last.eq_ignore_ascii_case("pm") {
                let (h, tm) = head.trim_end().rsplit_once(' ')?;
                (h, tm, true)
            } else {
                (head, last, false)
            };
        let (clock, frac) = time.split_once('.').map_or((time, ""), |(c, f)| (c, f));
        let parts: Vec<&str> = clock.split(':').collect();
        let digits = |s: &str| !s.is_empty() && s.bytes().all(|b| b.is_ascii_digit());
        if !parts.iter().all(|p| digits(p))
            || parts.len() > 3
            || !frac.bytes().all(|b| b.is_ascii_digit())
            || (parts.len() == 1 && (!marked || !frac.is_empty()))
        {
            return None;
        }
        let p = crate::coerce::parse_number_text(head.trim_end(), *env)?;
        if p.kind != crate::coerce::NumberKind::Date {
            return None;
        }
        p.format.or_else(|| input::auto_format(&p.kind, env))
    }

    pub(crate) fn input_env(&self) -> crate::coerce::ParseEnv<'_> {
        crate::coerce::ParseEnv {
            locale: self.input_locale.as_ref().unwrap_or(&self.locale),
            date_system: self.settings.date_system,
            current_year: self.calc.current_year,
            iso_t: false,
        }
    }

    pub fn set_formula(&mut self, addr: CellAddr, text: &str) -> Result<RecalcReport, EditError> {
        self.command(Command::Entry, |wb| {
            wb.note_changed(addr.sheet, &[notice::cell(addr)]);
            wb.set_formula_run(addr, text)
        })
    }

    fn set_formula_run(&mut self, addr: CellAddr, text: &str) -> Result<RecalcReport, EditError> {
        let edit = edit::Edit::formula(self, addr, text);
        self.run_edit(edit)
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
        self.refresh_tables_named(tables);
    }

    pub(crate) fn refresh_tables_named(&mut self, tables: Vec<String>) {
        let named = |f: &Formula, out: &mut Vec<String>| {
            crate::formula::walk(&f.root, &mut |n| {
                if let crate::formula::Node::StructRef { table, .. } = n {
                    out.push(table.to_string());
                }
            })
        };
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
        self.command(Command::Entry, |wb| {
            wb.note_changed(addr.sheet, &[notice::cell(addr)]);
            wb.set_value_run(addr, v)
        })
    }

    pub(crate) fn set_value_run(&mut self, addr: CellAddr, v: Value) -> RecalcReport {
        let edit = edit::Edit::value(self, addr, v);

        self.run_edit(edit).unwrap_or_default()
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
        self.command(Command::ClearContents, |wb| {
            wb.note_changed(addr.sheet, &[notice::cell(addr)]);
            wb.clear_run(addr)
        })
    }

    fn clear_run(&mut self, addr: CellAddr) -> RecalcReport {
        let edit = edit::Edit::clear(self, addr);
        self.run_edit(edit).unwrap_or_default()
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
            let r = calc::recalc_only(self, &cells);
            calc::close_entered_group(self, &cells);
            r
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
        self.note_hidden_names(fid);
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
        self.calc.volatile.remove(&fid);
        self.calc.host_volatile.remove(&fid);
        self.free.push(fid);
    }

    fn alloc_formula(&mut self, addr: CellAddr, formula: Arc<Formula>, text: String) -> FormulaId {
        let seq = self.chain_top();
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
            partial: None,
            stored: 0,
            spill_kept: false,
            spill_src: false,
            member_retry: false,
            share: None,
        };
        let fid = match self.free.pop() {
            Some(fid) => {
                self.formulas[fid as usize] = Some(cell);
                fid
            }
            None => {
                self.formulas.push(Some(cell));
                (self.formulas.len() - 1) as FormulaId
            }
        };
        self.chain_insert(fid);
        fid
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
        self.calc.host_volatile.remove(&fid);
        self.calc.circular.remove(&fid);
    }

    fn recompile_all(&mut self) {
        let ids: Vec<FormulaId> = self.formula_ids().collect();

        self.with_op(Op::DtBlock, true, |this| {
            for fid in ids {
                let (text, addr, missing) = {
                    let f = this.formula(fid);
                    let mut missing = false;
                    formula::walk(&f.formula.root, &mut |n| {
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
                    (f.text.clone(), f.addr, missing)
                };

                if !missing {
                    continue;
                }
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
        self.command(Command::Name, |wb| {
            wb.define_name_active_run(name, refers_to, scope)
        })
    }

    fn define_name_active_run(
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
        self.command(Command::ActivateSheet, |wb| wb.set_active_sheet_run(sheet))
    }

    fn set_active_sheet_run(&mut self, sheet: SheetId) -> Result<(), EditError> {
        if self.sheet(sheet).is_none() {
            return Err(EditError::NoSuchSheet);
        }
        self.session.active = self.shown_sheet(sheet);
        Ok(())
    }

    pub fn sheet_state(&self, sheet: SheetId) -> Option<SheetState> {
        self.sheet(sheet).map(|s| s.state)
    }

    pub fn set_sheet_state(&mut self, sheet: SheetId, state: SheetState) -> Result<(), EditError> {
        self.command(Command::ActivateSheet, |wb| {
            wb.set_sheet_state_run(sheet, state)
        })
    }

    fn set_sheet_state_run(&mut self, sheet: SheetId, state: SheetState) -> Result<(), EditError> {
        let pos = self.sheet_position(sheet).ok_or(EditError::NoSuchSheet)?;
        if state != SheetState::Visible
            && self.sheets[pos].state == SheetState::Visible
            && self
                .sheets
                .iter()
                .all(|s| s.id == sheet || s.state != SheetState::Visible)
        {
            return Err(EditError::NoVisibleSheet);
        }
        self.sheets[pos].state = state;
        self.session.active = self.shown_sheet(self.session.active);
        Ok(())
    }

    pub(crate) fn shown_sheet(&self, sheet: SheetId) -> SheetId {
        let Some(pos) = self.sheet_position(sheet) else {
            return sheet;
        };
        let shown = |s: &&Sheet| s.state == SheetState::Visible;
        self.sheets[pos..]
            .iter()
            .find(shown)
            .or_else(|| self.sheets[..pos].iter().rev().find(shown))
            .map_or(sheet, |s| s.id)
    }

    pub fn active_cell(&self) -> (u32, u32) {
        self.active_cell_of(self.session.active)
    }

    pub fn active_cell_of(&self, sheet: SheetId) -> (u32, u32) {
        let (r, c) = self
            .session
            .selection
            .get(&sheet)
            .map_or((0, 0), |s| s.active);
        match self.merges(sheet).into_iter().find(|m| m.contains(r, c)) {
            Some(m) => (m.r0, m.c0),
            None => (r, c),
        }
    }

    pub fn selection_of(&self, sheet: SheetId) -> Selection {
        self.session
            .selection
            .get(&sheet)
            .cloned()
            .unwrap_or(Selection::cell(0, 0))
    }

    pub fn set_selection(&mut self, sheet: SheetId, sel: Selection) -> Result<(), EditError> {
        if self.sheet(sheet).is_none() {
            return Err(EditError::NoSuchSheet);
        }
        let (r, c) = sel.active;
        let inside = |a: &Rect| a.r1 < crate::address::MAX_ROWS && a.c1 < crate::address::MAX_COLS;
        if sel.areas.is_empty() || !sel.areas.iter().all(inside) || !sel.contains(r, c) {
            return Err(EditError::Compile("selection outside the grid".into()));
        }
        self.session.selection.insert(sheet, sel);
        self.notify_list_select(sheet, r, c);
        self.view_notify_sel(sheet, r, c);
        Ok(())
    }

    pub fn set_active_cell(&mut self, row: u32, col: u32) -> Result<(), EditError> {
        if row >= crate::address::MAX_ROWS || col >= crate::address::MAX_COLS {
            return Err(EditError::Compile("cell outside the grid".into()));
        }
        let sheet = self.session.active;
        self.session
            .selection
            .insert(sheet, Selection::cell(row, col));
        self.notify_list_select(sheet, row, col);
        self.view_notify_sel(sheet, row, col);
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
        self.command(Command::Name, |wb| {
            wb.define_name_run(name, refers_to, scope)
        })
    }

    fn define_name_run(
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
        formula::NAME_CONTEXT.set(true);
        formula::NAME_BANG.set(
            self.sheet(self.session.active)
                .is_some_and(|s| s.kind.is_macro()),
        );
        let compiled = self.compile_formula(body, host);
        formula::NAME_BANG.set(false);
        formula::NAME_CONTEXT.set(false);
        let compiled = compiled?;

        let active = self.session.active;
        let mut one_column = false;
        formula::walk(&compiled.root, &mut |n| {
            if let formula::Node::StructRef { table, spec, .. } = n {
                let under_cursor = self
                    .table_by_name(table)
                    .is_some_and(|t| t.sheet == active && t.rect.contains(0, 0));
                if !under_cursor {
                    return;
                }
                let single = spec.cols.as_ref().is_some_and(|(a, b)| a == b) && !spec.range;
                let implicit = spec.items == tables::Items::DATA && !spec.explicit_data;
                one_column |= single && (spec.items.this_row || implicit);
            }
        });
        if one_column {
            return Err(EditError::Compile(
                "There's a problem with this formula.".into(),
            ));
        }

        self.note_placeholders(&compiled, host);
        let root = compiled.root.clone();
        self.note_hidden_in(&root, None);
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

            self.names.mark_copy_stale(s, key.into(), Vec::new());
        }
        self.names.insert(NameDef {
            name: name.to_string(),
            scope,
            refers_to: body.to_string(),
            formula,
        });
        if let Some(d) = self.names.get(&name.to_uppercase(), scope) {
            let root = d.formula.root.clone();
            self.note_hidden_in(&root, None);
        }
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
        self.command(Command::Setting, |wb| wb.set_mode_run(mode))
    }

    fn set_mode_run(&mut self, mode: CalcMode) -> RecalcReport {
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

    pub fn set_force_full(&mut self, on: bool) {
        self.settings.force_full = on;
        self.settings.force_full_seen |= on;
    }

    pub fn restore_settings(&mut self, settings: Settings) -> RecalcReport {
        self.command(Command::Setting, |wb| wb.restore_settings_run(settings))
    }

    fn restore_settings_run(&mut self, settings: Settings) -> RecalcReport {
        self.settings = settings;
        self.full_recalc()
    }

    pub fn set_date_system(&mut self, ds: DateSystem) -> RecalcReport {
        self.command(Command::Setting, |wb| wb.set_date_system_run(ds))
    }

    fn set_date_system_run(&mut self, ds: DateSystem) -> RecalcReport {
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

    pub fn set_multi_threaded(&mut self, on: bool) {
        self.settings.multi_threaded = on;
    }

    pub fn names(&self) -> &Names {
        &self.names
    }

    pub fn set_name_visible(&mut self, key: &str, scope: Option<SheetId>, visible: bool) {
        self.names.set_visible(key, scope, visible);
    }

    pub fn delete_hidden_name(&mut self, key: &str) -> bool {
        self.names.delete_hidden(key)
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
        self.command(Command::Setting, |wb| wb.set_locale_run(locale))
    }

    fn set_locale_run(&mut self, locale: Locale) -> RecalcReport {
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
            self.calc.circular_ref.first().copied()
        }
    }

    pub fn circular_ref_on(&self, sheet: SheetId) -> Option<CellAddr> {
        if self.calc.circ_iterating {
            None
        } else {
            self.calc
                .circular_ref
                .iter()
                .copied()
                .find(|c| c.sheet == sheet)
        }
    }

    pub fn set_iteration(
        &mut self,
        enabled: bool,
        max_iter: Option<u32>,
        max_change: Option<f64>,
    ) -> RecalcReport {
        self.command(Command::Setting, |wb| {
            wb.set_iteration_run(enabled, max_iter, max_change)
        })
    }

    fn set_iteration_run(
        &mut self,
        enabled: bool,
        max_iter: Option<u32>,
        max_change: Option<f64>,
    ) -> RecalcReport {
        let mut report = self.set_iteration_enabled(enabled);
        if let Some(n) = max_iter {
            let r = self.set_max_iterations(n);
            report.evaluations += r.evaluations;
            report.add_sheets(&r);
            report.changed.extend(r.changed);
            report.circular = r.circular;
        }
        if let Some(x) = max_change {
            let r = self.set_max_change(x);
            report.evaluations += r.evaluations;
            report.add_sheets(&r);
            report.changed.extend(r.changed);
            report.circular = r.circular;
        }
        report
    }

    fn iteration_step(&mut self) -> RecalcReport {
        if self.settings.mode != CalcMode::Automatic {
            return RecalcReport::default();
        }
        if self.settings.iteration {
            return calc::recalc(self);
        }
        let mut sheets: Vec<SheetId> = Vec::new();
        for &f in &self.calc.circular {
            let s = self.formula(f).addr.sheet;
            if !sheets.contains(&s) {
                sheets.push(s);
            }
        }
        if !sheets.is_empty() {
            self.note_pass(&sheets);
            self.notes.no_after = true;
        }
        RecalcReport::default()
    }

    pub fn set_iteration_enabled(&mut self, enabled: bool) -> RecalcReport {
        self.command(Command::Setting, |wb| wb.set_iteration_enabled_run(enabled))
    }

    fn set_iteration_enabled_run(&mut self, enabled: bool) -> RecalcReport {
        self.settings.iteration = enabled;

        if self.settings.mode == CalcMode::Automatic {
            self.calc.circ_iterating = enabled;
        }
        self.iteration_step()
    }

    pub fn set_max_iterations(&mut self, n: u32) -> RecalcReport {
        self.command(Command::Setting, |wb| wb.set_max_iterations_run(n))
    }

    fn set_max_iterations_run(&mut self, n: u32) -> RecalcReport {
        self.settings.max_iterations = n.min(32_767);
        self.iteration_step()
    }

    pub fn set_max_change(&mut self, x: f64) -> RecalcReport {
        self.command(Command::Setting, |wb| wb.set_max_change_run(x))
    }

    fn set_max_change_run(&mut self, x: f64) -> RecalcReport {
        if x.is_nan() {
            return RecalcReport::default();
        }
        self.settings.max_change = x;
        self.iteration_step()
    }

    pub fn recalc(&mut self) -> RecalcReport {
        self.command(Command::Calculate, |wb| wb.recalc_run())
    }

    fn recalc_run(&mut self) -> RecalcReport {
        calc::recalc_with(self, true)
    }

    pub fn calculate_sheet(&mut self, sheet: SheetId) -> RecalcReport {
        self.command(Command::CalculateSheet, |wb| {
            let ids: Vec<FormulaId> = wb
                .formula_ids()
                .filter(|&f| wb.formula(f).addr.sheet == sheet)
                .collect();
            let mut cells = Vec::new();

            let stale = |wb: &Workbook, fid: FormulaId| {
                !wb.settings.iteration && wb.calc.circular.contains(&fid)
            };
            let mut left_alone = false;
            for fid in ids {
                left_alone |= stale(wb, fid);
                if !stale(wb, fid)
                    && (wb.calc.volatile.contains(&fid)
                        || wb.calc.host_volatile.contains(&fid)
                        || wb.formula(fid).dirty
                        || wb.calc.circular.contains(&fid))
                {
                    calc::mark_dirty(wb, fid);
                    cells.push(wb.formula(fid).addr);
                }
            }
            let mut report = calc::recalc_only(wb, &cells);

            if left_alone && !report.sheets.contains(&sheet) {
                report.sheets.push(sheet);
                if wb.settings.mode == CalcMode::Automatic {
                    wb.notes.calculated(&[sheet]);
                }
            }
            if wb.settings.mode != CalcMode::Automatic {
                wb.notes.calculated(&report.sheets);
            }
            report
        })
    }

    pub fn calculate_range(&mut self, sheet: SheetId, rect: Rect) -> RecalcReport {
        if self.settings.mode == CalcMode::Automatic {
            return self.recalc();
        }
        self.command(Command::Calculate, |wb| {
            let cells: Vec<CellAddr> = wb
                .formula_ids()
                .filter(|&f| {
                    let a = wb.formula(f).addr;
                    a.sheet == sheet
                        && rect.contains(a.row, a.col)
                        && !wb.calc.circular.contains(&f)
                })
                .map(|f| wb.formula(f).addr)
                .collect();
            for &a in &cells {
                if let Some(f) = wb.formula_at(a) {
                    calc::mark_dirty(wb, f);
                }
            }

            let mut readers = Vec::new();
            wb.graph.dependents_of_rect(sheet, &rect, &mut readers);
            for fid in readers {
                calc::mark_dirty(wb, fid);
            }
            wb.notes.no_after = true;
            wb.silently(|wb| calc::recalc_only(wb, &cells))
        })
    }

    pub fn dirty_range(&mut self, sheet: SheetId, rect: Rect) -> RecalcReport {
        let auto = self.settings.mode == CalcMode::Automatic;
        self.command(Command::Calculate, |wb| {
            let ids: Vec<FormulaId> = wb
                .formula_ids()
                .filter(|&f| {
                    let a = wb.formula(f).addr;
                    a.sheet == sheet && rect.contains(a.row, a.col)
                })
                .collect();
            for fid in ids {
                calc::mark_dirty(wb, fid);
            }

            let mut readers = Vec::new();
            wb.graph.dependents_of_rect(sheet, &rect, &mut readers);
            for fid in readers {
                calc::mark_dirty(wb, fid);
            }
            if auto {
                wb.recalc_run()
            } else {
                RecalcReport::default()
            }
        })
    }

    pub fn full_recalc(&mut self) -> RecalcReport {
        self.command(Command::Calculate, |wb| wb.full_recalc_run())
    }

    fn full_recalc_run(&mut self) -> RecalcReport {
        let ids: Vec<FormulaId> = self.formula_ids().collect();
        for fid in ids {
            calc::mark_dirty(self, fid);
        }
        calc::recalc_with(self, true)
    }

    pub fn rebuild(&mut self) -> RecalcReport {
        self.command(Command::Calculate, |wb| wb.rebuild_run())
    }

    fn rebuild_run(&mut self) -> RecalcReport {
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
