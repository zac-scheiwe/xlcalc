pub mod call;
pub mod lambda;
pub mod ops;
pub mod refs;

use std::sync::Arc;

use smallvec::SmallVec;

use crate::address::{CellAddr, Rect, SheetId};
use crate::coerce::{self, ParseEnv};
use crate::formula::Node;
use crate::parser::ast::{BinOp, UnaryOp};
use crate::value::{Array, Value, XlError, MAX_TEXT_LEN};
use crate::workbook::{Entry, FormulaId, Workbook};

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
}

pub struct EvalCtx<'a> {
    pub wb: &'a Workbook,
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

    pub(crate) circ_lifted: bool,

    pub(crate) own: Option<FormulaId>,

    pub(crate) name_scope: Option<Option<crate::address::SheetId>>,

    pub(crate) arr_ctx: u8,

    pub(crate) name_key: Option<crate::value::Text>,

    pub(crate) arr_direct: bool,

    pub(crate) name_first: bool,

    pub(crate) fn_depth: u32,

    pub(crate) legacy_form: bool,
}

impl<'a> EvalCtx<'a> {
    pub fn new(wb: &'a Workbook, host: CellAddr) -> EvalCtx<'a> {
        EvalCtx::with_mode(wb, host, ReadMode::Calc)
    }

    pub fn with_mode(wb: &'a Workbook, host: CellAddr, mode: ReadMode) -> EvalCtx<'a> {
        EvalCtx {
            wb,
            host,
            mode,
            env: Vec::new(),
            interrupt: None,
            observed: Observed::default(),
            wrap: false,
            depth: 0,
            defer_cycles: false,
            circ_fid: None,
            pend_fid: None,
            circ_live: 0,
            circ_args: false,
            circ_lifted: false,
            own: None,
            name_scope: None,
            arr_ctx: 0,
            name_key: None,
            arr_direct: false,
            name_first: false,
            fn_depth: 0,
            legacy_form: false,
        }
    }

    pub(crate) fn too_big(&mut self, cells: f64) -> bool {
        if cells > crate::workbook::ABANDON_CELLS as f64 {
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

    pub fn parse_env(&self) -> ParseEnv<'a> {
        ParseEnv {
            locale: &self.wb.locale,
            date_system: self.wb.settings.date_system,
            current_year: self.wb.calc.current_year,
            iso_t: false,
        }
    }

    pub fn to_number_atp(&self, v: &Value) -> Result<f64, XlError> {
        Ok(self.to_number(v)? - self.atp_text_shift(v))
    }

    pub fn atp_text_shift(&self, v: &Value) -> f64 {
        if let (Value::Text(s), crate::datetime::DateSystem::D1904) =
            (v, self.wb.settings.date_system)
        {
            if let Some(p) = coerce::parse_number_text(s, self.parse_env()) {
                if matches!(
                    p.kind,
                    coerce::NumberKind::Date | coerce::NumberKind::DateTime
                ) {
                    return 1462.0;
                }
            }
        }
        0.0
    }

    pub fn to_number(&self, v: &Value) -> Result<f64, XlError> {
        coerce::to_number(v, self.parse_env())
    }

    pub fn formula_value(&mut self, fid: FormulaId, row: u32, col: u32) -> Value {
        let f = self.wb.formula(fid);
        if self.own == Some(fid) {
            self.observed.read_own = true;
            return f.value_at(row, col);
        }
        if f.dirty {
            match self.mode {
                ReadMode::Stale => self.observed.read_dirty = true,

                ReadMode::Iterative => {}
                ReadMode::Calc => {
                    if f.active {
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
        match sh.cells.get(row, col) {
            Some(Entry::Const(v)) => v.clone(),
            Some(Entry::Formula(fid)) => self.formula_value(*fid, row, col),
            None => match sh.spills.owner_of(row, col) {
                Some(fid) => self.formula_value(fid, row, col),
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
                    if !(r == sr.r0 && c == sr.c0) {
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
                    Some(fid) => self.formula_value(fid, r, c),
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

    pub fn area_array(&mut self, area: &Area) -> Value {
        let (rows, cols) = (area.rect.rows(), area.rect.cols());
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

    pub(crate) fn in_array_block(&self) -> bool {
        self.wb
            .formula_at(self.host)
            .is_some_and(|f| self.wb.formula(f).array.is_some())
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

    pub fn eval(&mut self, node: &Node) -> Operand {
        if self.interrupt.is_some() {
            return Operand::err(XlError::CALC);
        }
        stacker::maybe_grow(64 * 1024, 2 * 1024 * 1024, || self.eval_inner(node))
    }

    fn eval_inner(&mut self, node: &Node) -> Operand {
        let direct = std::mem::replace(&mut self.arr_direct, false);
        let first = std::mem::replace(&mut self.name_first, false);
        match node {
            Node::Lit(v) => Operand::Val(v.clone()),
            Node::Ref(sheet, cref) => refs::resolve(self, sheet, cref),
            Node::Name { key, sheet, .. } => {
                let inner = if self.arr_ctx == 2 || (self.arr_ctx == 1 && direct) {
                    2
                } else {
                    0
                };
                let saved = std::mem::replace(&mut self.arr_ctx, inner);
                let out = refs::eval_name(self, key, sheet.as_ref());
                self.arr_ctx = saved;
                match out {
                    Operand::Ref(Reference::Grid(g)) if first => match g.items.first() {
                        Some(Ok(a)) => Operand::Ref(Reference::Area(*a)),
                        Some(Err(e)) => Operand::err(*e),
                        None => Operand::err(XlError::VALUE),
                    },
                    out => out,
                }
            }
            Node::Local { slot, .. } => self
                .env
                .get(*slot as usize)
                .cloned()
                .unwrap_or(Operand::err(XlError::NAME)),
            Node::Unary(UnaryOp::Implicit | UnaryOp::ImplicitQuiet, x) => {
                let quiet = matches!(node, Node::Unary(UnaryOp::ImplicitQuiet, _));
                let saved = self.legacy_form;
                self.legacy_form = saved || quiet;
                let v = self.eval(x);
                self.legacy_form = saved;

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
                let v = self.eval(x);

                if *op == UnaryOp::Plus && matches!(v, Operand::Ref(Reference::Grid(_))) {
                    return v;
                }
                let v = self.deref(v);
                Operand::Val(ops::unary(self, *op, v))
            }
            Node::Percent(x) => {
                let v = self.eval(x);
                let v = self.deref(v);
                Operand::Val(ops::percent(self, v))
            }
            Node::Spill(x) => {
                let v = self.eval(x);
                refs::spill_ref(self, v)
            }
            Node::Binary(op, a, b) => match op {
                BinOp::Range | BinOp::Intersect | BinOp::Union => {
                    let l = self.eval(a);
                    let r = self.eval(b);

                    let dead = |n: &Node, o: &Operand| {
                        matches!(n, Node::Ref(crate::formula::SheetRef::Missing(_), _))
                            && matches!(o, Operand::Val(Value::Error(XlError::REF)))
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
                    let l = self.eval(a);
                    let l = self.deref(l);
                    let r = self.eval(b);
                    let r = self.deref(r);
                    Operand::Val(ops::binary(self, *op, l, r))
                }
            },
            Node::Paren(x) => {
                self.arr_direct = direct;
                self.eval(x)
            }
            Node::Func { id, args } => call::call_builtin(self, *id, args),
            Node::NamedCall { key, args, .. } => lambda::call_named(self, key, args),
            Node::Call { callee, args } => {
                let f = self.eval(callee);
                if matches!(f, Operand::Ref(_)) {
                    return Operand::err(XlError::REF);
                }
                let f = self.deref(f);
                lambda::call_value(self, f, args)
            }
            Node::Let {
                values, body, base, ..
            } => {
                let saved = std::mem::replace(&mut self.arr_ctx, 0);
                let base = *base as usize;
                self.env.truncate(base);
                for v in values {
                    let x = self.eval(v);
                    self.env.push(x);
                }
                let out = self.eval(body);
                self.env.truncate(base);
                self.arr_ctx = saved;
                out
            }
            Node::Lambda(def) => {
                let captured = self.env[..(def.base as usize).min(self.env.len())].to_vec();
                Operand::Val(Value::Lambda(Arc::new(lambda::LambdaValue {
                    def: def.clone(),
                    captured,
                })))
            }
            Node::Missing => Operand::Missing,
            Node::Unsupported { error, .. } => Operand::err(*error),
            Node::DeadRef(_) => Operand::err(XlError::REF),
            Node::StructRef { table, spec, .. } => refs::struct_ref(self, table, spec),
            Node::Ws { node, .. } => {
                self.arr_direct = direct;
                self.eval(node)
            }
        }
    }
}

pub fn eval_formula(ctx: &mut EvalCtx, root: &Node) -> Value {
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

            match &op {
                Operand::Ref(Reference::Grid(g)) if ctx.in_array_block() => {
                    let one = ctx
                        .wb
                        .formula_at(ctx.host)
                        .and_then(|f| ctx.wb.formula(f).array)
                        == Some((1, 1));
                    if g.by_index || one {
                        let g = g.clone();
                        let v = ctx.grid_cells(&g);
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
    if ctx.has_placeholders() && contains_circ(&v) {
        let block = ctx
            .wb
            .formula_at(ctx.host)
            .and_then(|f| ctx.wb.formula(f).array);
        let used = match block {
            Some((r, c)) => contains_circ(&crate::cse::fit(v.clone(), r, c)),
            None => true,
        };
        if used {
            ctx.circ_used();
        }
    }
    if ctx.observed.abandoned {
        return Value::Number(0.0);
    }
    finalize(v)
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
