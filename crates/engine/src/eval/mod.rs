pub mod call;
pub(crate) mod fdyn;
pub mod lambda;
pub mod ops;
pub(crate) mod ptgclass;
pub mod refs;

use std::sync::Arc;

use smallvec::SmallVec;

use crate::address::{CellAddr, Rect, SheetId};
use crate::coerce::{self, ParseEnv};
use crate::formula::Node;
use crate::parser::ast::{BinOp, UnaryOp};
use crate::value::{Array, Value, XlError, MAX_TEXT_LEN};
use crate::workbook::{Entry, FormulaId, Workbook};

#[derive(Clone, Copy)]
pub(crate) struct ArgFrame {
    pub(crate) lo: usize,
    pub(crate) hi: usize,
    pub(crate) ftab: Option<&'static crate::legacy_ftab::Ftab>,
}

fn ptgclass_kind(node: &Node) -> ptgclass::Kind {
    match node {
        Node::Func { id, .. } => ptgclass::Kind::Call(ptgclass::ftab_of(*id)),
        Node::Unary(UnaryOp::Implicit, _) => ptgclass::Kind::Call(ptgclass::ftab_of_name("SINGLE")),
        Node::Spill(_) => ptgclass::Kind::Call(ptgclass::ftab_of_name("ANCHORARRAY")),
        Node::NamedCall { .. } | Node::Call { .. } => ptgclass::Kind::Call(ptgclass::ftab_call()),
        Node::Let { .. } => ptgclass::Kind::Call(ptgclass::ftab_of_name("LET")),
        Node::Name { .. }
        | Node::Ref(..)
        | Node::StructRef { .. }
        | Node::DeadRef(_)
        | Node::Unsupported { .. } => ptgclass::Kind::RefToken { array_const: false },
        Node::Lit(Value::Array(_)) => ptgclass::Kind::RefToken { array_const: true },
        _ => ptgclass::Kind::Other,
    }
}

#[derive(Clone, Copy, Debug, PartialEq, Eq, Hash)]
pub struct Area {
    pub sheet: SheetId,
    pub rect: Rect,
}

impl Area {
    pub fn cell(sheet: SheetId, row: u32, col: u32) -> Area {
        Area {
            sheet,
            rect: Rect::cell(row, col),
        }
    }
}

#[derive(Clone, Debug, PartialEq)]
pub enum Reference {
    Area(Area),
    Multi(Arc<[Area]>),

    Span(Arc<[Area]>),

    Grid(Arc<RefGrid>),
}

#[derive(Debug, PartialEq)]
pub struct RefGrid {
    pub rows: u32,
    pub cols: u32,
    pub items: Vec<Result<Area, XlError>>,

    pub areas: Arc<[Area]>,

    pub by_index: bool,
}

impl RefGrid {
    pub fn new(rows: u32, cols: u32, items: Vec<Result<Area, XlError>>) -> RefGrid {
        let areas = items
            .iter()
            .filter_map(|a| a.ok())
            .map(|a| Area::cell(a.sheet, a.rect.r0, a.rect.c0))
            .collect::<Vec<_>>()
            .into();
        RefGrid {
            rows,
            cols,
            items,
            areas,
            by_index: false,
        }
    }

    pub fn value_array(&self) -> Arc<crate::value::Array> {
        Arc::new(crate::value::Array::new(
            self.rows,
            self.cols,
            self.items
                .iter()
                .map(|i| Value::Error(i.err().unwrap_or(XlError::VALUE)))
                .collect(),
        ))
    }

    pub fn first_error(&self) -> Option<XlError> {
        self.items.iter().find_map(|i| i.err())
    }

    pub fn all_errors(&self) -> bool {
        self.items.iter().all(|i| i.is_err())
    }

    pub fn broadcast_get(&self, r: u32, c: u32) -> Operand {
        let rr = if self.rows == 1 { 0 } else { r };
        let cc = if self.cols == 1 { 0 } else { c };
        if rr >= self.rows || cc >= self.cols {
            return Operand::err(XlError::NA);
        }
        match self.items[(rr * self.cols + cc) as usize] {
            Ok(a) => Operand::Ref(Reference::Area(a)),
            Err(e) => Operand::err(e),
        }
    }
}

impl Reference {
    pub fn areas(&self) -> &[Area] {
        match self {
            Reference::Area(a) => std::slice::from_ref(a),
            Reference::Multi(v) | Reference::Span(v) => v,
            Reference::Grid(g) => &g.areas,
        }
    }

    pub fn single(&self) -> Option<&Area> {
        match self {
            Reference::Area(a) => Some(a),
            Reference::Multi(v) | Reference::Span(v) if v.len() == 1 => Some(&v[0]),
            Reference::Multi(_) | Reference::Span(_) | Reference::Grid(_) => None,
        }
    }

    pub fn is_span(&self) -> bool {
        matches!(self, Reference::Span(_))
    }

    pub fn from_areas(mut areas: Vec<Area>) -> Reference {
        if areas.len() == 1 {
            Reference::Area(areas.pop().unwrap())
        } else {
            Reference::Multi(areas.into())
        }
    }
}

#[derive(Clone, Debug)]
pub enum Operand {
    Val(Value),
    Ref(Reference),

    Missing,
}

impl Operand {
    pub fn err(e: XlError) -> Operand {
        Operand::Val(Value::Error(e))
    }

    pub fn is_missing(&self) -> bool {
        matches!(self, Operand::Missing)
    }
}

impl From<Value> for Operand {
    fn from(v: Value) -> Self {
        Operand::Val(v)
    }
}

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum Interrupt {
    Pending(FormulaId),

    Cycle(FormulaId),
}

pub const MAX_CALL_DEPTH: u32 = 1024;

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum ReadMode {
    Calc,

    Stale,

    Iterative,
}

#[derive(Clone, Copy, Debug, Default)]
pub struct Observed {
    pub read_dirty: bool,

    pub read_own: bool,

    pub abandoned: bool,

    pub partial: bool,

    pub active_cse: Option<FormulaId>,

    pub read_spill: bool,

    pub spill_masked: bool,

    pub host_volatile: Option<bool>,

    pub needs_serial: bool,
}

pub struct EvalCtx<'a> {
    pub wb: &'a Workbook,

    pub(crate) locale: &'a crate::Locale,
    pub host: CellAddr,

    mode: ReadMode,

    pub env: Vec<Operand>,
    pub interrupt: Option<Interrupt>,

    pub observed: Observed,

    pub wrap: bool,
    pub depth: u32,

    defer_cycles: bool,

    circ_fid: Option<FormulaId>,

    pend_fid: Option<FormulaId>,

    pub(crate) circ_live: u32,

    pub(crate) circ_args: bool,

    pub(crate) lambda_name_abort: bool,

    pub(crate) allow_partial: bool,

    pub(crate) forced_partial: bool,

    pub(crate) in_evaluate: bool,

    pub(crate) outside_formula: bool,

    pub(crate) xlm_entry: bool,

    pub xlm_vars: Option<std::sync::Arc<crate::xlm::XlmVars>>,

    pub xlm_cells: Option<std::sync::Arc<crate::xlm::XlmCells>>,

    pub(crate) circ_sources: Vec<FormulaId>,

    pub(crate) circ_lifted: bool,

    pub(crate) own: Option<FormulaId>,

    pub(crate) name_scope: Option<Option<crate::address::SheetId>>,

    pub(crate) node_cls: ptgclass::Class,

    pub(crate) node_mask: u32,

    pub(crate) slot: Option<u32>,

    pub(crate) name_tables: Vec<(usize, std::sync::Arc<fdyn::ClassTable>)>,

    pub(crate) frames: Vec<ArgFrame>,

    pub(crate) name_key: Option<crate::value::Text>,

    pub(crate) in_lambda: bool,

    pub(crate) let_names: Vec<(u32, crate::value::Text, Node)>,

    pub(crate) frame_root: Option<(usize, ptgclass::Class)>,

    pub(crate) consumed_by_ref: bool,

    pub(crate) cse_lifted: bool,

    pub(crate) fn_depth: u32,

    pub(crate) at_call: Option<u32>,

    pub(crate) at_named: bool,

    pub(crate) vchain: bool,

    pub(crate) parallel: bool,

    pub(crate) walk: bool,

    pub(crate) walk_target: Option<crate::walk::Node>,

    pub(crate) walk_pend: Option<crate::walk::Node>,

    walk_first_pull: Option<bool>,
}

impl<'a> EvalCtx<'a> {
    pub fn new(wb: &'a Workbook, host: CellAddr) -> EvalCtx<'a> {
        EvalCtx::with_mode(wb, host, ReadMode::Calc)
    }

    pub fn with_mode(wb: &'a Workbook, host: CellAddr, mode: ReadMode) -> EvalCtx<'a> {
        EvalCtx {
            wb,
            locale: &wb.locale,
            host,
            mode,
            env: Vec::new(),
            interrupt: None,
            observed: Observed::default(),
            wrap: false,
            xlm_vars: None,
            xlm_cells: None,
            depth: 0,
            defer_cycles: false,
            circ_fid: None,
            pend_fid: None,
            circ_live: 0,
            circ_args: false,
            lambda_name_abort: false,
            allow_partial: false,
            forced_partial: false,
            in_evaluate: false,
            outside_formula: false,
            xlm_entry: false,
            circ_sources: Vec::new(),
            circ_lifted: false,
            own: None,
            name_scope: None,
            node_cls: ptgclass::Class::Val,
            node_mask: ptgclass::MASK_OPERATOR,
            slot: None,
            name_tables: Vec::new(),
            frames: Vec::new(),
            name_key: None,
            in_lambda: false,
            let_names: Vec::new(),
            frame_root: None,
            consumed_by_ref: false,
            fn_depth: 0,
            at_call: None,
            at_named: false,
            vchain: true,
            cse_lifted: false,
            parallel: false,
            walk: false,
            walk_target: None,
            walk_pend: None,
            walk_first_pull: None,
        }
    }

    pub(crate) fn let_name(&self, slot: u32, name: &str) -> Option<&Node> {
        self.let_names
            .iter()
            .rev()
            .find(|(s, n, _)| *s == slot && n.eq_ignore_ascii_case(name))
            .map(|(_, _, node)| node)
    }

    pub(crate) fn is_name_node(&self, node: &Node) -> bool {
        match node {
            Node::Name { .. } => true,
            Node::Local { slot, name } => self.let_name(*slot, name).is_some(),
            _ => false,
        }
    }

    pub(crate) fn too_big(&mut self, cells: f64) -> bool {
        if cells > crate::workbook::ABANDON_CELLS as f64 {
            self.observed.abandoned = true;
        }
        self.observed.abandoned
    }

    pub(crate) fn shape_too_big(&mut self, rows: u32, cols: u32) -> bool {
        if refs::value_block_fails(u64::from(rows) * u64::from(cols)) {
            self.observed.abandoned = true;
        }
        self.observed.abandoned
    }

    #[inline]
    pub(crate) fn has_placeholders(&self) -> bool {
        self.circ_fid.is_some() || self.pend_fid.is_some()
    }

    pub(crate) fn circ_used(&mut self) {
        if self.interrupt.is_none() {
            self.interrupt = match (self.pend_fid, self.circ_fid) {
                (Some(f), _) => Some(Interrupt::Pending(f)),
                (None, Some(f)) => Some(Interrupt::Cycle(f)),
                (None, None) => None,
            };
        }
    }

    #[inline]
    pub fn interrupted(&self) -> bool {
        self.interrupt.is_some()
    }

    pub(crate) fn read_mode(&self) -> ReadMode {
        self.mode
    }

    pub fn parse_env(&self) -> ParseEnv<'a> {
        ParseEnv {
            locale: self.locale,
            date_system: self.wb.settings.date_system,
            current_year: self.wb.calc.current_year,
            iso_t: false,
        }
    }

    pub fn to_number_atp(&self, v: &Value) -> Result<f64, XlError> {
        self.to_number(v)
    }

    pub fn atp_text_shift(&self, v: &Value) -> f64 {
        if matches!(v, Value::Text(_))
            && self.wb.settings.date_system == crate::datetime::DateSystem::D1904
        {
            1462.0
        } else {
            0.0
        }
    }

    pub fn to_number(&self, v: &Value) -> Result<f64, XlError> {
        coerce::to_number(v, self.parse_env())
    }

    pub fn formula_value(&mut self, fid: FormulaId, row: u32, col: u32) -> Value {
        let f = self.wb.formula(fid);
        if self.own == Some(fid) {
            self.observed.read_own = true;

            return match f.value_at(row, col) {
                Value::Error(XlError::CIRC) => self.placeholder(fid, false),
                v => v,
            };
        }
        if self.walk {
            let n = crate::walk::node_at(self.wb, fid, row, col);
            if let Some((t, pull)) = crate::walk::read_target(self.wb, n) {
                if self.defer_cycles {
                    self.walk_first_pull.get_or_insert(pull);
                    if pull && self.pend_fid.is_none() {
                        self.walk_pend = Some(t);
                    }
                    return self.placeholder(fid, pull);
                }
                if self.interrupt.is_none() {
                    self.interrupt = Some(if pull {
                        self.walk_target = Some(t);
                        Interrupt::Pending(fid)
                    } else {
                        Interrupt::Cycle(fid)
                    });
                }
                return f.value_at(row, col);
            }
            if f.spill_src {
                self.observed.read_spill = true;
            }
            return f.value_at(row, col);
        }
        if f.dirty && self.mode == ReadMode::Calc && crate::calc::out_of_bound(self.wb, fid) {
            self.observed.read_dirty = true;
            return f.value_at(row, col);
        }
        if f.dirty {
            match self.mode {
                ReadMode::Stale => self.observed.read_dirty = true,

                ReadMode::Iterative => {}
                ReadMode::Calc => {
                    let pass = self.wb.calc.pass;
                    if let Some(p) = f.partial.as_deref().filter(|p| p.pass == pass) {
                        let cols = f.array.map_or(1, |a| a.1);
                        let i = ((row - f.addr.row) * cols + (col - f.addr.col)) as usize;
                        if !p.mask.get(i).copied().unwrap_or(true) {
                            return f.value_at(row, col);
                        }
                        if f.circular_pass != pass
                            && (f.active || !crate::calc::partial_retry(self.wb, fid))
                        {
                            if self.defer_cycles {
                                self.circ_sources.push(fid);
                                return self.placeholder(fid, false);
                            }
                            if self.interrupt.is_none() {
                                self.interrupt = Some(Interrupt::Cycle(fid));
                            }
                            return f.value_at(row, col);
                        }
                    }
                    if self.forced_partial && f.on_stack && !f.active {
                        self.circ_sources.push(fid);
                        return self.placeholder(fid, false);
                    }
                    if f.active {
                        if f.array.is_some() && self.allow_partial && !self.forced_partial {
                            self.observed.active_cse.get_or_insert(fid);
                        }
                        if self.defer_cycles {
                            self.circ_sources.push(fid);
                        }
                        if self.defer_cycles {
                            return self.placeholder(fid, false);
                        } else if self.interrupt.is_none() {
                            self.interrupt = Some(Interrupt::Cycle(fid));
                        }
                    } else if f.circular_pass == self.wb.calc.pass {
                        if self.defer_cycles {
                            return self.placeholder(fid, false);
                        }
                        if self.interrupt.is_none() {
                            self.interrupt = Some(Interrupt::Cycle(fid));
                        }
                    } else if self.defer_cycles {
                        return self.placeholder(fid, true);
                    } else if self.interrupt.is_none() {
                        self.interrupt = Some(Interrupt::Pending(fid));
                    }
                }
            }
        }
        if f.spill_src {
            self.observed.read_spill = true;
        }
        f.value_at(row, col)
    }

    fn placeholder(&mut self, fid: FormulaId, pending: bool) -> Value {
        if pending {
            self.pend_fid.get_or_insert(fid);
        } else {
            self.circ_fid.get_or_insert(fid);
        }
        self.circ_live += 1;
        Value::Error(XlError::CIRC)
    }

    pub fn cell_value(&mut self, sheet: SheetId, row: u32, col: u32) -> Value {
        let wb = self.wb;
        let Some(sh) = wb.sheet(sheet) else {
            return Value::Error(XlError::REF);
        };
        if let Some(v) = self
            .xlm_cells
            .as_ref()
            .and_then(|c| c.get(&(sheet, row, col)))
        {
            return v.clone();
        }
        match sh.cells.get(row, col) {
            Some(Entry::Const(_)) if wb.ext_const_closed(sheet) => Value::Error(XlError::REF),
            Some(Entry::Const(v)) => v.clone(),
            Some(Entry::Formula(fid)) => self.formula_value(*fid, row, col),
            None => match sh.spills.owner_of(row, col) {
                Some(fid) => self.spill_value(fid, row, col),
                None => Value::Blank,
            },
        }
    }

    pub fn visit_area(&mut self, area: &Area, mut f: impl FnMut(u32, u32, &Value)) {
        self.visit_area_until(area, |r, c, v| {
            f(r, c, v);
            true
        });
    }

    pub fn visit_area_until(&mut self, area: &Area, f: impl FnMut(u32, u32, &Value) -> bool) {
        self.visit_area_where(area, |_, _| true, f);
    }

    pub(crate) fn visit_area_lazy(
        &mut self,
        area: &Area,
        keep: impl Fn(u32, u32) -> bool,
        f: impl FnMut(u32, u32, &Value) -> bool,
    ) {
        let defer = std::mem::replace(&mut self.defer_cycles, self.mode != ReadMode::Iterative);
        self.visit_area_where(area, keep, f);
        self.defer_cycles = defer;
    }

    pub fn visit_area_where(
        &mut self,
        area: &Area,
        keep: impl Fn(u32, u32) -> bool,
        mut f: impl FnMut(u32, u32, &Value) -> bool,
    ) {
        let wb = self.wb;
        let Some(sh) = wb.sheet(area.sheet) else {
            if area.sheet == refs::GHOST_SHEET {
                self.visit_ghost(area, f);
            }
            return;
        };
        let Some(rect) = sh.bounded(&area.rect) else {
            return;
        };
        let spills = sh.spills.intersecting(&rect);
        if spills.is_empty() {
            let (r0, c0) = (area.rect.r0, area.rect.c0);
            let mut go = true;
            sh.cells.for_each_in(rect, |r, c, e| {
                if !go || self.interrupt.is_some() || !keep(r - r0, c - c0) {
                    return;
                }
                match e {
                    Entry::Const(v) => go = f(r - r0, c - c0, v),
                    Entry::Formula(fid) => {
                        let v = self.formula_value(*fid, r, c);
                        go = f(r - r0, c - c0, &v);
                    }
                }
            });
            return;
        }

        for r in rect.r0..=rect.r1 {
            let mut cols: SmallVec<[(u32, Option<FormulaId>); 16]> = SmallVec::new();
            for (c, _) in sh.cells.iter_row(r, rect.c0, rect.c1) {
                cols.push((c, None));
            }
            for (fid, sr) in &spills {
                if r < sr.r0 || r > sr.r1 {
                    continue;
                }
                for c in sr.c0.max(rect.c0)..=sr.c1.min(rect.c1) {
                    if !(r == sr.r0 && c == sr.c0) && !sh.spills.is_hole(*fid, r, c) {
                        cols.push((c, Some(*fid)));
                    }
                }
            }
            cols.sort_unstable_by_key(|x| x.0);
            for (c, owner) in cols {
                if !keep(r - area.rect.r0, c - area.rect.c0) {
                    continue;
                }
                let v = match owner {
                    Some(fid) => self.spill_value(fid, r, c),
                    None => match sh.cells.get(r, c) {
                        Some(Entry::Const(v)) => v.clone(),
                        Some(Entry::Formula(fid)) => self.formula_value(*fid, r, c),
                        None => continue,
                    },
                };
                if !f(r - area.rect.r0, c - area.rect.c0, &v) || self.interrupt.is_some() {
                    return;
                }
            }
        }
    }

    fn spill_value(&mut self, fid: FormulaId, row: u32, col: u32) -> Value {
        let src = self
            .wb
            .sheet(self.wb.formula(fid).addr.sheet)
            .and_then(|sh| sh.spills.source(fid, row, col));
        match src {
            Some(crate::workbook::spill::SpillSource::Zero) => Value::Number(0.0),
            Some(crate::workbook::spill::SpillSource::Value(v)) => v,
            Some(crate::workbook::spill::SpillSource::At(r, c)) => self.formula_value(fid, r, c),
            None => self.formula_value(fid, row, col),
        }
    }

    fn visit_ghost(&mut self, area: &Area, mut f: impl FnMut(u32, u32, &Value) -> bool) {
        let err = Value::Error(XlError::REF);
        let (rows, cols) = (area.rect.rows(), area.rect.cols());
        if rows as u64 * cols as u64 > refs::MATERIALISE_MAX as u64 {
            f(0, 0, &err);
            return;
        }
        for r in 0..rows {
            for c in 0..cols {
                if !f(r, c, &err) {
                    return;
                }
            }
        }
    }

    pub fn area_array(&mut self, area: &Area) -> Value {
        let (rows, cols) = (area.rect.rows(), area.rect.cols());
        if area.sheet == refs::GHOST_SHEET
            && (area.rect.is_cell() || rows as u64 * cols as u64 > refs::MATERIALISE_MAX as u64)
        {
            return Value::Error(XlError::REF);
        }
        if area.rect.is_cell() {
            return self.cell_value(area.sheet, area.rect.r0, area.rect.c0);
        }
        let n = rows as u64 * cols as u64;
        if n > crate::workbook::MAX_ARRAY_CELLS {
            self.too_big(n as f64);
            return Value::Error(XlError::NUM);
        }
        let mut data = vec![Value::Blank; n as usize];

        let defer = std::mem::replace(&mut self.defer_cycles, self.mode != ReadMode::Iterative);

        let wb = self.wb;
        let sh = wb.sheet(area.sheet);
        let (r0, c0) = (area.rect.r0, area.rect.c0);
        self.visit_area(area, |r, c, v| {
            data[(r * cols + c) as usize] = match v {
                Value::Text(t)
                    if t.is_empty()
                        && sh.is_some_and(|sh| {
                            !matches!(sh.cells.get(r0 + r, c0 + c), Some(Entry::Const(_)))
                        }) =>
                {
                    Value::Text(crate::value::cell_empty())
                }
                v => v.clone(),
            }
        });
        self.defer_cycles = defer;
        Array::new(rows, cols, data).into_value()
    }

    pub fn grid_cells(&mut self, g: &RefGrid) -> Value {
        let data = g
            .items
            .iter()
            .map(|i| match i {
                Ok(a) if a.rect.is_cell() => self.cell_value(a.sheet, a.rect.r0, a.rect.c0),
                Ok(_) => Value::Error(XlError::VALUE),
                Err(e) => Value::Error(*e),
            })
            .collect();
        Array::new(g.rows, g.cols, data).into_value()
    }

    pub fn deref_marked(&mut self, op: Operand) -> Value {
        let cell = match &op {
            Operand::Ref(r) => r.single().filter(|a| a.rect.is_cell()).copied(),
            _ => None,
        };
        let v = self.deref(op);
        match (cell, &v) {
            (Some(a), Value::Text(t)) if t.is_empty() => {
                let constant = self.wb.sheet(a.sheet).is_some_and(|sh| {
                    matches!(sh.cells.get(a.rect.r0, a.rect.c0), Some(Entry::Const(_)))
                });
                if constant {
                    v
                } else {
                    Value::Text(crate::value::cell_empty())
                }
            }
            _ => v,
        }
    }

    pub(crate) fn eval_selector(&mut self, node: &Node) -> Value {
        fn lifts_ref(node: &Node, root: bool) -> bool {
            match node {
                Node::Ref(..) | Node::Name { .. } => !root,
                Node::Unary(_, x) | Node::Percent(x) => lifts_ref(x, false),
                Node::Paren(x) => lifts_ref(x, root),
                Node::Binary(_, a, b) => lifts_ref(a, false) || lifts_ref(b, false),
                Node::Func { id, args } => {
                    let name = crate::functions::def(*id).name;
                    let argc = args.len();
                    args.iter().enumerate().any(|(i, a)| {
                        crate::legacy_classes::class_of_n(name, i, argc) == Some(b'V')
                            && lifts_ref(a, false)
                    })
                }
                _ => false,
            }
        }
        let o = self.eval(node);
        let v = self.deref(o);
        match v {
            Value::Array(a)
                if a.data
                    .iter()
                    .any(|x| matches!(x, Value::Error(XlError::SPILL)))
                    && self.in_array_block()
                    && (!self.wb.settings.multi_threaded
                        || self
                            .wb
                            .formula_at(self.host)
                            .is_some_and(|f| self.member_calc(f))
                            && lifts_ref(node, true)) =>
            {
                self.observed.spill_masked = true;
                let data = a
                    .data
                    .iter()
                    .map(|x| match x {
                        Value::Error(XlError::SPILL) => Value::Error(XlError::VALUE),
                        x => x.clone(),
                    })
                    .collect();
                Array::new(a.rows, a.cols, data).into_value()
            }
            v => v,
        }
    }

    fn member_calc(&self, fid: FormulaId) -> bool {
        let f = self.wb.formula(fid);
        f.spill_kept
            || f.member_retry
            || f.partial
                .as_deref()
                .is_some_and(|p| p.pass == self.wb.calc.pass)
    }

    pub(crate) fn array_entry(&self) -> bool {
        self.in_array_block() || self.dynamic_host()
    }

    pub(crate) fn host_formula(&self) -> Option<FormulaId> {
        self.wb
            .formula_at(self.host)
            .filter(|_| !self.outside_formula)
    }

    pub(crate) fn dynamic_host(&self) -> bool {
        !self.outside_formula && fdyn::cached(self.wb, self.host)
    }

    fn stored_class(&self, node: &Node) -> Option<ptgclass::Class> {
        let key = node as *const Node as usize;
        let ae = self.array_entry();
        if let Some(c) =
            fdyn::stored_classes(self.wb, self.host, ae).and_then(|t| t.get(&key).copied())
        {
            return Some(c);
        }
        self.name_tables
            .iter()
            .rev()
            .find_map(|(_, t)| t.get(&key).copied())
    }

    pub(crate) fn enter_name_table(&mut self, formula: &std::sync::Arc<crate::formula::Formula>) {
        let id = std::sync::Arc::as_ptr(formula) as usize;
        let at = self.name_tables.iter().position(|(i, _)| *i == id);
        let entry = match at {
            Some(k) => self.name_tables.remove(k),
            None => (
                id,
                std::sync::Arc::new({
                    let mut t = fdyn::body_classes(&formula.root, ptgclass::MASK_NAME, true);

                    crate::formula::walk(&formula.root, &mut |n| {
                        if matches!(
                            n,
                            Node::Name { .. } | Node::Ref(..) | Node::StructRef { .. }
                        ) {
                            let c = t
                                .entry(n as *const Node as usize)
                                .or_insert(ptgclass::Class::Val);
                            if *c == ptgclass::Class::Val {
                                *c = ptgclass::Class::Ref;
                            }
                        }
                    });

                    if let Some(c) = t.get_mut(&(&formula.root as *const Node as usize)) {
                        if *c == ptgclass::Class::Val {
                            *c = ptgclass::Class::Ref;
                        }
                    }
                    t
                }),
            ),
        };
        self.name_tables.push(entry);
    }

    pub(crate) fn in_array_block(&self) -> bool {
        self.host_formula()
            .is_some_and(|f| self.wb.formula(f).array.is_some())
    }

    pub(crate) fn cse_value_grid(&mut self, op: Operand, chain: bool) -> Operand {
        let Operand::Ref(Reference::Grid(g)) = &op else {
            return op;
        };
        let Some(block) = self
            .wb
            .formula_at(self.host)
            .and_then(|f| self.wb.formula(f).array)
        else {
            return op;
        };
        if chain && block == (1, 1) {
            return match g.items.first() {
                Some(Ok(a)) => Operand::Ref(Reference::Area(*a)),
                Some(Err(e)) => Operand::err(*e),
                None => Operand::err(XlError::VALUE),
            };
        }
        Operand::Val(Value::Array(g.value_array()))
    }

    pub fn deref(&mut self, op: Operand) -> Value {
        match op {
            Operand::Val(v) => v,
            Operand::Missing => Value::Blank,
            Operand::Ref(Reference::Grid(g)) => Value::Array(g.value_array()),
            Operand::Ref(r) => match r.single() {
                Some(a) => {
                    let a = *a;
                    self.area_array(&a)
                }

                None if r.areas().iter().any(|a| a.sheet != r.areas()[0].sheet) => {
                    Value::Error(XlError::REF)
                }
                None => Value::Error(XlError::VALUE),
            },
        }
    }

    pub(crate) fn call_back(&mut self, call: &Node) -> Value {
        let op = self.call_from_outside(call);
        self.convert_back(op)
    }

    pub(crate) fn convert_back(&mut self, op: Operand) -> Value {
        let v = self.deref(op);
        if self.observed.abandoned {
            return Value::Number(0.0);
        }
        v
    }

    pub(crate) fn call_from_outside(&mut self, call: &Node) -> Operand {
        self.frame_root = Some((call as *const Node as usize, ptgclass::Class::Ref));
        self.eval(call)
    }

    pub fn eval(&mut self, node: &Node) -> Operand {
        if self.interrupt.is_some() {
            return Operand::err(XlError::CALC);
        }

        let mask = self.slot_of(node);
        let paren = matches!(node, Node::Paren(_) | Node::Ws { .. });
        let cls = if paren {
            self.node_cls
        } else if let Some((_, c)) = self
            .frame_root
            .filter(|(p, _)| *p == node as *const Node as usize)
        {
            c
        } else {
            let kind = ptgclass_kind(node);
            let cls = self
                .stored_class(node)
                .unwrap_or_else(|| ptgclass::classify(kind, mask, &[], self.array_entry()));

            if cls == ptgclass::Class::Val
                && (self.in_array_block() || self.in_lambda || self.dynamic_host())
            {
                ptgclass::Class::Arr
            } else {
                cls
            }
        };
        let saved = (self.node_cls, self.node_mask);
        (self.node_cls, self.node_mask) = (cls, mask);
        let out = xlcalc_stack::maybe_grow(64 * 1024, 2 * 1024 * 1024, || self.eval_inner(node));
        (self.node_cls, self.node_mask) = saved;

        let key = node as *const Node as usize;

        let in_body = self
            .name_tables
            .last()
            .is_some_and(|(_, t)| t.contains_key(&key))
            && self.host_formula().is_some();
        if cls == ptgclass::Class::Val
            && matches!(
                node,
                Node::Name { .. }
                    | Node::Ref(..)
                    | Node::StructRef { .. }
                    | Node::Lit(Value::Array(_))
            )
            && matches!(out, Operand::Ref(_) | Operand::Val(Value::Array(_)))
            && (in_body
                || (!self.array_entry()
                    && fdyn::stored_classes(self.wb, self.host, false)
                        .is_some_and(|t| t.contains_key(&key))))
        {
            return refs::implicit_intersection(self, out);
        }

        if cls == ptgclass::Class::Val
            && in_body
            && matches!(
                node,
                Node::Func { .. }
                    | Node::NamedCall { .. }
                    | Node::Call { .. }
                    | Node::Binary(..)
                    | Node::Unary(..)
                    | Node::Percent(_)
            )
        {
            match &out {
                Operand::Val(Value::Array(a)) if a.data.len() > 1 => {
                    return Operand::Val(a.data[0].clone());
                }
                Operand::Ref(Reference::Grid(g)) => {
                    return match g.items.first() {
                        Some(Ok(a)) => Operand::Ref(Reference::Area(*a)),
                        Some(Err(e)) => Operand::err(*e),
                        None => Operand::err(XlError::VALUE),
                    };
                }
                _ => {}
            }
        }
        out
    }

    fn in_call_frame<T>(&mut self, args: &[Node], f: impl FnOnce(&mut Self) -> T) -> T {
        let lo = args.as_ptr() as usize;
        self.frames.push(ArgFrame {
            lo,
            hi: lo + std::mem::size_of_val(args),
            ftab: ptgclass::ftab_call(),
        });
        let out = f(self);
        self.frames.pop();
        out
    }

    fn slot_of(&mut self, node: &Node) -> u32 {
        if let Some(m) = self.slot.take() {
            return m;
        }
        if let Some(f) = self.frames.last() {
            let p = node as *const Node as usize;
            if p >= f.lo && p < f.hi {
                return ptgclass::arg_mask(f.ftab, (p - f.lo) / std::mem::size_of::<Node>());
            }
        }
        ptgclass::MASK_OPERATOR
    }

    fn eval_operand(&mut self, node: &Node) -> Operand {
        if let (
            Node::Name {
                key,
                sheet: Some(_),
                ..
            },
            Some(Some(scope)),
            Some(name),
        ) = (node.unwrap_ws(), self.name_scope, self.name_key.as_ref())
        {
            if self.wb.names.is_copy_stale(scope, name, key) {
                self.slot = None;
                return Operand::err(XlError::VALUE);
            }
        }
        let v = self.eval(node);

        if matches!(node.unwrap_ws(), Node::Local { .. })
            && matches!(&v, Operand::Ref(r) if r.single().is_some_and(|x| !x.rect.is_cell()))
            && !self.array_entry()
            && !self.in_lambda
            && self.node_cls == ptgclass::Class::Val
        {
            return refs::implicit_intersection(self, v);
        }

        if self.xlm_vars.is_some()
            && matches!(&v, Operand::Ref(r) if r.single().is_some_and(|x| !x.rect.is_cell()))
        {
            return refs::implicit_intersection(self, v);
        }
        v
    }

    fn eval_inner(&mut self, node: &Node) -> Operand {
        let mask = self.node_mask;
        match node {
            Node::Lit(v) => Operand::Val(v.clone()),
            Node::Ref(sheet, cref) => refs::resolve(self, sheet, cref),
            Node::Name { key, sheet, .. } => {
                let array_frame = match self.node_cls {
                    ptgclass::Class::Arr => true,
                    ptgclass::Class::Ref => mask & 7 == 0,
                    ptgclass::Class::Val => false,
                };
                self.slot = array_frame.then_some(ptgclass::MASK_REF_ARRAY);

                let out = refs::eval_name(self, key, sheet.as_ref());
                self.slot = None;
                out
            }
            Node::Local { slot, name } if self.let_name(*slot, name).is_some() => {
                let n = self.let_name(*slot, name).cloned().unwrap_or(Node::Missing);
                self.slot = Some(mask);
                self.eval(&n)
            }
            Node::Local { slot, .. } => self
                .env
                .get(*slot as usize)
                .cloned()
                .unwrap_or(Operand::err(XlError::NAME)),
            Node::Unary(UnaryOp::Implicit, x) => {
                let at_call =
                    matches!(x.unwrap_ws(), Node::Func { .. }).then_some(self.fn_depth + 1);
                let saved_at = std::mem::replace(&mut self.at_call, at_call);
                let saved_named = std::mem::replace(
                    &mut self.at_named,
                    matches!(x.unwrap_ws(), Node::NamedCall { .. }),
                );
                self.slot = Some(ptgclass::arg_mask(ptgclass::ftab_of_name("SINGLE"), 0));
                let v = self.eval(x);
                self.at_named = saved_named;
                self.at_call = saved_at;

                let v = match v {
                    Operand::Ref(Reference::Grid(g)) if matches!(**x, Node::Name { .. }) => {
                        match g.items.first() {
                            Some(Ok(a)) => Operand::Ref(Reference::Area(*a)),
                            Some(Err(e)) => Operand::err(*e),
                            None => Operand::err(XlError::VALUE),
                        }
                    }
                    v => v,
                };

                if let Operand::Val(Value::Array(a)) = &v {
                    let ghost_node = matches!(
                        x.unwrap_ws(),
                        Node::Ref(crate::formula::SheetRef::Missing(_), _)
                            | Node::Binary(BinOp::Range, _, _)
                    );
                    if ghost_node
                        && a.rows > 1
                        && a.cols > 1
                        && a.data
                            .iter()
                            .all(|e| matches!(e, Value::Error(XlError::REF)))
                    {
                        return Operand::err(XlError::VALUE);
                    }
                }
                refs::implicit_intersection(self, v)
            }
            Node::Unary(UnaryOp::ArrayZero, x) => {
                let v = self.eval(x);
                match self.deref(v) {
                    Value::Array(a) if a.rows * a.cols > 1 => Operand::Val(Value::Number(0.0)),
                    v => Operand::Val(v),
                }
            }
            Node::Unary(op, x) => {
                self.slot = Some(ptgclass::MASK_OPERATOR);

                let through = *op == UnaryOp::Plus && self.consumed_by_ref && call::hands_on(x);
                let saved_ref = std::mem::replace(&mut self.consumed_by_ref, through);
                let v = self.eval_operand(x);
                self.consumed_by_ref = saved_ref;
                if through && matches!(&v, Operand::Ref(r) if r.is_span()) {
                    return v;
                }

                if *op == UnaryOp::Plus && self.fn_depth > 0 {
                    if let Operand::Ref(Reference::Grid(g)) = &v {
                        if g.items
                            .iter()
                            .all(|i| i.as_ref().is_ok_and(|a| a.rect.is_cell()))
                        {
                            let g = g.clone();
                            return Operand::Val(self.grid_cells(&g));
                        }
                        return v;
                    }
                }
                let v = self.cse_value_grid(v, self.vchain);
                let v = self.deref(v);
                Operand::Val(ops::unary(self, *op, v))
            }
            Node::Percent(x) => {
                self.slot = Some(ptgclass::MASK_OPERATOR);
                let saved_ref = std::mem::replace(&mut self.consumed_by_ref, false);
                let v = self.eval_operand(x);
                self.consumed_by_ref = saved_ref;
                let v = self.deref(v);
                Operand::Val(ops::percent(self, v))
            }
            Node::Spill(x) => {
                self.slot = Some(ptgclass::arg_mask(ptgclass::ftab_of_name("ANCHORARRAY"), 0));
                let v = self.eval(x);
                refs::spill_ref(self, v)
            }
            Node::Binary(op, a, b) => match op {
                BinOp::Range | BinOp::Intersect | BinOp::Union => {
                    self.slot = Some(ptgclass::MASK_RANGE);
                    let l = self.eval(a);
                    self.slot = Some(ptgclass::MASK_RANGE);
                    let r = self.eval(b);

                    let dead = |n: &Node, o: &Operand| {
                        matches!(
                            n.unwrap_ws(),
                            Node::Ref(crate::formula::SheetRef::Missing(_), _)
                        ) && matches!(o, Operand::Val(Value::Error(XlError::REF)))
                    };
                    if *op == BinOp::Range
                        && (dead(a, &l) && matches!(r, Operand::Ref(_))
                            || dead(b, &r) && matches!(l, Operand::Ref(_)))
                    {
                        return Operand::err(XlError::VALUE);
                    }
                    refs::ref_op(self, *op, l, r)
                }
                _ => {
                    self.slot = Some(ptgclass::MASK_OPERATOR);
                    let saved_ref = std::mem::replace(&mut self.consumed_by_ref, false);
                    let l = self.eval_operand(a);
                    self.consumed_by_ref = saved_ref;
                    let l = self.cse_value_grid(l, self.vchain);
                    let l = self.deref(l);
                    self.slot = Some(ptgclass::MASK_OPERATOR);
                    let saved_ref = std::mem::replace(&mut self.consumed_by_ref, false);
                    let r = self.eval_operand(b);
                    self.consumed_by_ref = saved_ref;
                    let r = self.cse_value_grid(r, self.vchain);
                    let r = self.deref(r);
                    Operand::Val(ops::binary(self, *op, l, r))
                }
            },
            Node::Paren(x) => {
                self.slot = Some(mask);
                self.eval(x)
            }

            Node::MemErr(error, _) => Operand::err(*error),
            Node::Func { id, args } => call::call_builtin(self, *id, args),
            Node::NamedCall { key, args, .. } => {
                self.in_call_frame(args, |c| lambda::call_named(c, key, args))
            }
            Node::Call { callee, args } => {
                self.slot = Some(ptgclass::callee_mask());
                let f = self.eval(callee);
                if matches!(f, Operand::Ref(_)) {
                    return Operand::err(XlError::REF);
                }
                let f = self.deref(f);
                self.in_call_frame(args, |c| lambda::call_value(c, f, args))
            }
            Node::Let {
                names,
                values,
                body,
                base,
            } => {
                let let_mask = ptgclass::arg_mask(ptgclass::ftab_of_name("LET"), 1);
                let base = *base as usize;
                self.env.truncate(base);
                let aliases = self.let_names.len();
                for (k, v) in values.iter().enumerate() {
                    self.slot = Some(let_mask);
                    let x = self.eval(v);
                    self.env.push(x);
                    if let (Node::Name { .. }, Some(var)) = (v.unwrap_ws(), names.get(k)) {
                        self.let_names.push((
                            (base + k) as u32,
                            var.clone(),
                            v.unwrap_ws().clone(),
                        ));
                    }
                }
                self.slot = Some(let_mask);
                let out = self.eval(body);
                self.let_names.truncate(aliases);
                self.env.truncate(base);
                out
            }
            Node::Lambda(def) => {
                let captured = self.env[..(def.base as usize).min(self.env.len())].to_vec();

                if captured.iter().any(|o| matches!(o, Operand::Ref(_)))
                    && def
                        .params
                        .iter()
                        .any(|p| crate::address::parse_a1_cell(p).is_some())
                {
                    self.lambda_name_abort = true;
                    return Operand::err(XlError::NAME);
                }
                Operand::Val(Value::Lambda(Arc::new(lambda::LambdaValue {
                    def: def.clone(),
                    captured,
                })))
            }

            Node::Missing | Node::Unwritten => Operand::Missing,
            Node::Unsupported { error, .. } => Operand::err(*error),
            Node::DeadRef(_) => Operand::err(XlError::REF),
            Node::StructRef { table, spec, .. } => refs::struct_ref(self, table, spec),
            Node::Ws { node, .. } => {
                self.slot = Some(mask);
                self.eval(node)
            }
        }
    }
}

pub fn eval_formula(ctx: &mut EvalCtx, root: &Node) -> Value {
    finalize(eval_formula_raw(ctx, root))
}

pub(crate) fn finalize_keeping_nil(v: Value) -> Value {
    match v {
        Value::Array(a) if a.data.iter().any(|x| matches!(x, Value::Blank)) => {
            let kept = a.data.iter().map(|x| match x {
                Value::Blank => Value::Blank,
                other => finalize(other.clone()),
            });
            Array::new(a.rows, a.cols, kept.collect()).into_value()
        }
        Value::Blank => Value::Blank,
        v => finalize(v),
    }
}

fn eval_formula_raw(ctx: &mut EvalCtx, root: &Node) -> Value {
    let macro_host = ctx
        .wb
        .sheet(ctx.host.sheet)
        .is_some_and(|s| s.kind.is_macro());
    let v = match root.unwrap_ws() {
        Node::Binary(op @ (BinOp::Add | BinOp::Sub), a, b) => {
            let l = ctx.eval(a);
            let l = ctx.deref(l);
            let r = ctx.eval(b);
            let r = ctx.deref(r);
            ops::binary_snapped(ctx, *op, l, r)
        }
        _ => {
            let op = ctx.eval(root);

            let op =
                if !macro_host && !ctx.array_entry() && matches!(op, Operand::Val(Value::Array(_)))
                {
                    Operand::err(XlError::VALUE)
                } else {
                    op
                };

            let single_root = macro_host
                || (!ctx.array_entry()
                    && ctx
                        .wb
                        .formula_at(ctx.host)
                        .is_some_and(|f| std::ptr::eq(&ctx.wb.formula(f).formula.root, root)));
            let op = if single_root && matches!(op, Operand::Ref(_)) {
                refs::implicit_intersection(ctx, op)
            } else {
                op
            };

            match &op {
                Operand::Ref(Reference::Grid(g)) if ctx.in_array_block() => {
                    let one = ctx
                        .wb
                        .formula_at(ctx.host)
                        .and_then(|f| ctx.wb.formula(f).array)
                        == Some((1, 1));
                    if g.by_index || one {
                        let data = g
                            .items
                            .iter()
                            .map(|i| match i {
                                Ok(a) => ctx.cell_value(a.sheet, a.rect.r0, a.rect.c0),
                                Err(e) => Value::Error(*e),
                            })
                            .collect();
                        let v = Array::new(g.rows, g.cols, data).into_value();
                        match v {
                            Value::Array(a) if one => a.data[0].clone(),
                            v => v,
                        }
                    } else {
                        ctx.deref(op)
                    }
                }
                _ => ctx.deref(op),
            }
        }
    };
    let v = if std::mem::take(&mut ctx.lambda_name_abort) {
        Value::Error(XlError::NAME)
    } else {
        v
    };

    let v = match v {
        Value::Array(a) if macro_host && !a.data.is_empty() && !ctx.in_array_block() => {
            a.data[0].clone()
        }
        v => v,
    };
    if ctx.has_placeholders() && contains_circ(&v) {
        let block = ctx
            .wb
            .formula_at(ctx.host)
            .and_then(|f| ctx.wb.formula(f).array);
        let used = match block {
            Some((r, c)) => contains_circ(&crate::cse::lift_to_block(v.clone(), r, c)),
            None => true,
        };
        if used {
            let partial = ctx.allow_partial
                && (ctx.pend_fid.is_none() || ctx.walk)
                && ctx.interrupt.is_none()
                && block.is_some_and(|(r, c)| r * c > 1);
            let anchor = ctx.walk
                && block.is_none()
                && ctx.interrupt.is_none()
                && matches!(&v, Value::Array(a) if a.rows * a.cols > 1);
            if partial {
                ctx.observed.partial = true;
            } else if anchor && ctx.walk_first_pull == Some(true) {
                ctx.circ_used();
            } else if anchor {
                let g = ctx.pend_fid.or(ctx.circ_fid).unwrap_or_default();
                ctx.interrupt = Some(Interrupt::Cycle(g));
            } else {
                ctx.circ_used();
            }
        }
    }
    if ctx.observed.abandoned {
        return Value::Number(0.0);
    }
    v
}

pub(crate) fn contains_circ(v: &Value) -> bool {
    match v {
        Value::Error(XlError::CIRC) => true,
        Value::Array(a) => a
            .data
            .iter()
            .any(|x| matches!(x, Value::Error(XlError::CIRC))),
        _ => false,
    }
}

pub fn finalize(v: Value) -> Value {
    match v {
        Value::Blank => Value::Number(0.0),
        Value::Lambda(_) => Value::Error(XlError::CALC),
        Value::Text(ref s) if s.len() > MAX_TEXT_LEN && s.encode_utf16().count() > MAX_TEXT_LEN => {
            Value::Error(XlError::VALUE)
        }
        Value::Array(a) => {
            if a.rows == 1 && a.cols == 1 {
                return finalize(a.data[0].clone());
            }
            if a.data
                .iter()
                .any(|x| matches!(x, Value::Blank | Value::Lambda(_) | Value::Array(_)))
            {
                let data = a
                    .data
                    .iter()
                    .map(|x| match x {
                        Value::Blank => Value::Number(0.0),
                        Value::Lambda(_) | Value::Array(_) => Value::Error(XlError::CALC),
                        other => other.clone(),
                    })
                    .collect();
                Array::new(a.rows, a.cols, data).into_value()
            } else {
                Value::Array(a)
            }
        }
        other => other,
    }
}
