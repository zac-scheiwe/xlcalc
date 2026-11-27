use crate::address::CellAddr;
use crate::eval::Area;
use crate::value::Value;
use crate::workbook::{FormulaId, Workbook};

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum ParamKind {
    Value,

    Reference,
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct Signature {
    pub params: Vec<ParamKind>,

    pub rest: ParamKind,

    pub volatile: bool,
}

impl Signature {
    pub fn kind(&self, index: usize) -> ParamKind {
        self.params.get(index).copied().unwrap_or(self.rest)
    }
}

#[derive(Clone, Debug)]
pub enum Arg {
    Missing,
    Value(Value),

    Ref(Vec<Area>),

    XRef(Vec<Area>),
}

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub struct RuntimeError;

pub trait FunctionHost: Send + Sync {
    fn signature(&self, name: &str) -> Option<Signature>;

    fn bind(&self, _name: &str) -> Option<String> {
        None
    }

    fn display(&self, _key: &str) -> Option<String> {
        None
    }

    fn macro_callable(&self, _name: &str) -> bool {
        true
    }

    fn refused_after_host_call(&self, _name: &str) -> bool {
        true
    }

    fn call(
        &self,
        name: &str,
        args: &[Arg],
        cx: &mut CallContext<'_>,
    ) -> Result<Value, RuntimeError>;
}

#[derive(Default)]
pub(crate) struct Unseen {
    on: std::sync::atomic::AtomicBool,
    areas: std::sync::Mutex<Vec<Area>>,

    late: std::sync::Mutex<rustc_hash::FxHashMap<FormulaId, u32>>,

    late_done: std::sync::Mutex<rustc_hash::FxHashMap<FormulaId, u32>>,

    own: std::sync::Mutex<Option<CellAddr>>,
    own_on: std::sync::atomic::AtomicBool,
    read_own: std::sync::atomic::AtomicBool,
}

impl Unseen {
    fn begin_own(&self, addr: CellAddr) {
        *self.own.lock().unwrap_or_else(|e| e.into_inner()) = Some(addr);
        self.read_own
            .store(false, std::sync::atomic::Ordering::Relaxed);
        self.own_on
            .store(true, std::sync::atomic::Ordering::Relaxed);
    }

    fn end_own(&self) -> bool {
        self.own_on
            .store(false, std::sync::atomic::Ordering::Relaxed);
        *self.own.lock().unwrap_or_else(|e| e.into_inner()) = None;
        self.read_own
            .swap(false, std::sync::atomic::Ordering::Relaxed)
    }

    pub(crate) fn hides_own(&self, addr: CellAddr) -> bool {
        if !self.own_on.load(std::sync::atomic::Ordering::Relaxed) {
            return false;
        }
        let own = *self.own.lock().unwrap_or_else(|e| e.into_inner()) == Some(addr);
        if own {
            self.read_own
                .store(true, std::sync::atomic::Ordering::Relaxed);
        }
        own
    }

    pub(crate) fn begin(&self, areas: Vec<Area>) {
        *self.areas.lock().unwrap_or_else(|e| e.into_inner()) = areas;
        self.on.store(true, std::sync::atomic::Ordering::Relaxed);
    }

    pub(crate) fn end(&self) {
        self.on.store(false, std::sync::atomic::Ordering::Relaxed);
        self.areas.lock().unwrap_or_else(|e| e.into_inner()).clear();
    }

    pub(crate) fn set_late(&self, fid: FormulaId, pass: u32) {
        self.late
            .lock()
            .unwrap_or_else(|e| e.into_inner())
            .insert(fid, pass);
    }

    pub(crate) fn is_late(&self, fid: FormulaId, pass: u32) -> bool {
        self.late
            .lock()
            .unwrap_or_else(|e| e.into_inner())
            .get(&fid)
            == Some(&pass)
    }

    pub(crate) fn clear_late(&self) {
        self.late.lock().unwrap_or_else(|e| e.into_inner()).clear();
        self.late_done
            .lock()
            .unwrap_or_else(|e| e.into_inner())
            .clear();
    }

    pub(crate) fn deferred(&self, caller: FormulaId, pass: u32) {
        let mut done = self.late_done.lock().unwrap_or_else(|e| e.into_inner());
        done.insert(caller, pass);
    }

    fn late_in(
        &self,
        wb: &Workbook,
        caller: FormulaId,
        areas: &[Area],
        pass: u32,
    ) -> Option<FormulaId> {
        let mut done = self.late_done.lock().unwrap_or_else(|e| e.into_inner());
        if done.get(&caller) == Some(&pass) {
            return None;
        }
        let late = self.late.lock().unwrap_or_else(|e| e.into_inner());
        let g = late
            .iter()
            .filter(|&(&g, &p)| p == pass && g != caller)
            .map(|(&g, _)| g)
            .filter(|&g| {
                let a = wb.formula(g).addr;
                areas
                    .iter()
                    .any(|r| r.sheet == a.sheet && r.rect.contains(a.row, a.col))
            })
            .min()?;
        done.insert(caller, pass);
        Some(g)
    }

    pub(crate) fn hides(&self, addr: CellAddr) -> bool {
        self.on.load(std::sync::atomic::Ordering::Relaxed)
            && self
                .areas
                .lock()
                .unwrap_or_else(|e| e.into_inner())
                .iter()
                .any(|a| a.sheet == addr.sheet && a.rect.contains(addr.row, addr.col))
    }
}

#[derive(Clone, Copy)]
pub struct ReadView<'a> {
    wb: &'a Workbook,
    caller: CellAddr,
}

impl<'a> ReadView<'a> {
    pub(crate) fn new(wb: &'a Workbook, caller: CellAddr) -> ReadView<'a> {
        ReadView { wb, caller }
    }

    pub fn workbook(&self) -> &'a Workbook {
        self.wb
    }

    pub fn caller(&self) -> CellAddr {
        self.caller
    }

    pub fn value(&self, addr: CellAddr) -> Value {
        self.wb.value(addr)
    }

    pub fn formula(&self, addr: CellAddr) -> Option<String> {
        self.wb.formula_text(addr)
    }

    pub fn number_format(&self, addr: CellAddr) -> &'a str {
        self.wb.number_format(addr)
    }
}

pub struct CallContext<'a> {
    view: ReadView<'a>,
    volatile: bool,
    result_ref: Option<Vec<Area>>,
    circular: bool,
    legacy: bool,
}

impl<'a> CallContext<'a> {
    pub(crate) fn new(view: ReadView<'a>, volatile: bool) -> CallContext<'a> {
        CallContext {
            view,
            volatile,
            result_ref: None,
            circular: false,
            legacy: false,
        }
    }

    pub fn legacy_form(&self) -> bool {
        self.legacy
    }

    pub fn set_result_ref(&mut self, areas: Vec<Area>) {
        self.result_ref = Some(areas).filter(|a| !a.is_empty());
    }

    pub fn set_circular(&mut self) {
        self.circular = true;
    }

    pub fn view(&self) -> ReadView<'a> {
        self.view
    }

    pub fn set_volatile(&mut self, volatile: bool) {
        self.volatile = volatile;
    }

    pub fn is_volatile(&self) -> bool {
        self.volatile
    }
}

pub trait RtdSource: Send + Sync {
    fn topic(&self, prog_id: &str, server: &str, topics: &[String]) -> Option<Value>;
}

impl Workbook {
    pub fn set_rtd_source(&mut self, source: Option<std::sync::Arc<dyn RtdSource>>) {
        self.rtd_source = source;
    }

    pub fn renew_function_host(&mut self, host: std::sync::Arc<dyn FunctionHost>) {
        self.function_host = Some(host);
    }

    pub fn set_function_host(&mut self, host: Option<std::sync::Arc<dyn FunctionHost>>) {
        let old = std::mem::replace(&mut self.function_host, host);
        let has = |h: &Option<std::sync::Arc<dyn FunctionHost>>, k: &str| {
            h.as_ref().is_some_and(|h| h.signature(k).is_some())
        };
        let keys: Vec<crate::value::Text> = self
            .graph
            .name_keys()
            .filter(|k| has(&old, k) || has(&self.function_host, k))
            .cloned()
            .collect();
        let mut deps = Vec::new();
        for k in keys {
            self.graph.dependents_of_name(&k, &mut deps);
        }
        deps.sort_unstable();
        deps.dedup();
        for fid in deps {
            if self.formula(fid).dirty {
                self.calc.dirty_list.push(fid);
            } else {
                crate::calc::mark_dirty(self, fid);
            }
        }
    }

    pub fn dirty_name_errors(&mut self) {
        let keys: Vec<crate::value::Text> = self.graph.name_keys().cloned().collect();
        let mut deps = Vec::new();
        for k in keys {
            self.graph.dependents_of_name(&k, &mut deps);
        }
        deps.sort_unstable();
        deps.dedup();
        for fid in deps {
            let f = self.formula(fid);
            if !matches!(f.value, Value::Error(e) if e == crate::value::XlError::NAME) {
                continue;
            }
            if f.dirty {
                self.calc.dirty_list.push(fid);
            } else {
                crate::calc::mark_dirty(self, fid);
            }
        }
    }

    pub fn function_host(&self) -> Option<&std::sync::Arc<dyn FunctionHost>> {
        self.function_host.as_ref()
    }
}

pub(crate) fn calls_host(wb: &Workbook, root: &crate::formula::Node) -> bool {
    let Some(host) = wb.function_host.as_deref() else {
        return false;
    };
    let mut found = false;
    crate::formula::walk(root, &mut |n| {
        if let crate::formula::Node::NamedCall { key, .. } = n {
            found = found || host.signature(key).is_some();
        }
    });
    found
}

fn covers(a: &Area, at: CellAddr) -> bool {
    a.sheet == at.sheet
        && (a.rect.r0..=a.rect.r1).contains(&at.row)
        && (a.rect.c0..=a.rect.c1).contains(&at.col)
}

fn computed_ref(n: &crate::formula::Node) -> bool {
    use crate::formula::Node;
    use crate::parser::ast::BinOp;
    crate::deep(|| match n {
        Node::Ws { node, .. } | Node::Paren(node) => computed_ref(node),
        Node::Binary(BinOp::Intersect, ..) => true,
        Node::Binary(BinOp::Range | BinOp::Union, l, r) => computed_ref(l) || computed_ref(r),
        Node::Func { id, args } => match crate::functions::def(*id).name {
            "INDEX" => true,
            "OFFSET" => args.first().is_some_and(computed_ref),
            "IF" | "CHOOSE" => args.iter().skip(1).any(computed_ref),
            _ => false,
        },
        _ => false,
    })
}

pub(crate) fn call(
    ctx: &mut crate::eval::EvalCtx,
    host: &dyn FunctionHost,
    sig: &Signature,
    key: &str,
    args: &[crate::formula::Node],
) -> crate::eval::Operand {
    use crate::eval::{contains_circ, Interrupt, Operand, ReadMode, Reference};
    use crate::value::XlError;
    if ctx.parallel {
        ctx.observed.needs_serial = true;
        return Operand::err(XlError::VALUE);
    }
    let under_at = std::mem::take(&mut ctx.at_named);
    let mut vals = Vec::with_capacity(args.len());

    let mut waits: Option<FormulaId> = None;
    let mut unseen: Vec<Area> = Vec::new();

    let mut holds_own = false;

    let mut fails: Option<FormulaId> = None;
    let caller = ctx.wb.formula_at(ctx.host);
    for (i, a) in args.iter().enumerate() {
        let op = ctx.eval(a);
        let arg = match (sig.kind(i), op) {
            (_, Operand::Missing) => Arg::Missing,

            (ParamKind::Reference, Operand::Ref(r))
                if !matches!(r, Reference::Grid(_))
                    && r.areas()
                        .iter()
                        .any(|a| ctx.wb.ext_sheet_info(a.sheet).is_some_and(|(l, _)| !l.open)) =>
            {
                Arg::Value(ctx.deref(Operand::Ref(r)))
            }
            (ParamKind::Reference, Operand::Ref(r)) if !matches!(r, Reference::Grid(_)) => {
                let clear = !ctx.interrupted();

                let me = ctx.host;
                for area in r.areas() {
                    let (r0, c0) = (area.rect.r0, area.rect.c0);
                    let own = (area.sheet == me.sheet).then_some((me.row, me.col));
                    if own.is_some_and(|(row, col)| area.rect.contains(row, col)) {
                        holds_own = true;
                    }
                    ctx.visit_area_where(
                        area,
                        |dr, dc| Some((r0 + dr, c0 + dc)) != own,
                        |_, _, _| true,
                    );
                }
                if let Some(Interrupt::Pending(g)) = ctx.interrupt {
                    if clear && Some(g) != caller && ctx.read_mode() == ReadMode::Calc {
                        ctx.interrupt = None;
                        if waits.is_none() {
                            waits = Some(if ctx.wb.settings.multi_threaded {
                                g
                            } else if ctx.walk {
                                let first = crate::walk::first_in_areas(ctx.wb, caller, r.areas());
                                ctx.walk_target = first.or(ctx.walk_target);
                                g
                            } else {
                                crate::calc::first_dirty_in_areas(ctx.wb, caller, g, r.areas())
                            });
                        }

                        if let Some(c) = caller {
                            ctx.wb.calc.unseen.deferred(c, ctx.wb.calc.pass);
                        }
                        unseen.extend(r.areas().iter().copied());
                    }
                } else if let Some(Interrupt::Cycle(g)) = ctx.interrupt {
                    if clear && ctx.walk && Some(g) != caller && ctx.read_mode() == ReadMode::Calc {
                        ctx.interrupt = None;
                        fails.get_or_insert(g);
                        unseen.extend(r.areas().iter().copied());
                    }
                }
                if !ctx.interrupted() && ctx.read_mode() == ReadMode::Calc && waits.is_none() {
                    let late = caller
                        .filter(|_| ctx.wb.settings.multi_threaded)
                        .and_then(|c| {
                            let pass = ctx.wb.calc.pass;
                            ctx.wb.calc.unseen.late_in(ctx.wb, c, r.areas(), pass)
                        });
                    if let Some(g) = late {
                        waits = Some(g);
                        unseen.extend(r.areas().iter().copied());
                    }
                }
                if computed_ref(a) {
                    Arg::XRef(r.areas().to_vec())
                } else {
                    Arg::Ref(r.areas().to_vec())
                }
            }
            (_, op) => {
                let v = ctx.deref(op);
                if contains_circ(&v) {
                    ctx.circ_used();
                }
                Arg::Value(v)
            }
        };
        vals.push(arg);
    }
    if ctx.interrupted() {
        return Operand::err(XlError::CALC);
    }
    let mut cx = CallContext::new(ReadView::new(ctx.wb, ctx.host), sig.volatile);
    cx.legacy = under_at && !ctx.in_array_block();
    if waits.is_some_and(|g| ctx.wb.calc.no_empty_call && !ctx.wb.calc.moved_now.contains(&g)) {
        ctx.interrupt = waits.map(Interrupt::Pending);
        return Operand::err(XlError::CALC);
    }

    let own = holds_own
        && ctx.walk
        && caller.is_some()
        && ctx.read_mode() == ReadMode::Calc
        && !ctx.in_array_block();
    if own {
        ctx.wb.calc.unseen.begin_own(ctx.host);
    }
    let out = if unseen.is_empty() {
        host.call(key, &vals, &mut cx)
    } else {
        ctx.wb.calc.unseen.begin(unseen);
        let out = host.call(key, &vals, &mut cx);
        ctx.wb.calc.unseen.end();
        out
    };
    if own && ctx.wb.calc.unseen.end_own() {
        cx.set_circular();
    }
    if let (Some(g), None) = (fails, waits) {
        ctx.interrupt = Some(Interrupt::Cycle(g));
        return Operand::err(XlError::CALC);
    }
    let volatile = ctx.observed.host_volatile.unwrap_or(false) || cx.is_volatile();
    ctx.observed.host_volatile = Some(volatile);
    if let Some(g) = waits {
        ctx.interrupt = Some(Interrupt::Pending(g));
        return Operand::err(XlError::CALC);
    }
    if cx.circular
        || cx
            .result_ref
            .as_ref()
            .is_some_and(|a| a.iter().any(|a| covers(a, ctx.host)))
    {
        if let (Some(c), None) = (caller, ctx.interrupt) {
            ctx.interrupt = Some(Interrupt::Cycle(c));
        }
        return Operand::err(XlError::CALC);
    }
    match out {
        Ok(_) if cx.result_ref.is_some() => {
            let mut areas = cx.result_ref.take().unwrap_or_default();
            Operand::Ref(if areas.len() == 1 {
                Reference::Area(areas.remove(0))
            } else {
                Reference::Multi(areas.into())
            })
        }
        Ok(v) => Operand::Val(v),
        Err(RuntimeError) => Operand::err(XlError::VALUE),
    }
}

#[derive(Clone, Debug)]
pub enum Evaluated {
    Value(Value),
    Ref(Vec<Area>),
}

impl Workbook {
    pub fn evaluate_text(&self, text: &str, host: CellAddr) -> Evaluated {
        use crate::eval::{EvalCtx, Reference};
        use crate::value::XlError;
        let bad = || Evaluated::Value(Value::Error(XlError::VALUE));
        let body = text.strip_prefix('=').unwrap_or(text);
        if body.trim().is_empty() || text.chars().count() > 255 {
            return bad();
        }

        let up = body.trim().to_ascii_uppercase();
        let full_rc = up.split(':').any(|part| {
            part.strip_prefix('R').is_some_and(|r| {
                r.split_once('C').is_some_and(|(a, b)| {
                    !a.is_empty()
                        && !b.is_empty()
                        && a.bytes().all(|c| c.is_ascii_digit())
                        && b.bytes().all(|c| c.is_ascii_digit())
                })
            })
        });
        if full_rc || up.contains("R[") || up.contains("C[") {
            return bad();
        }
        let Ok((mut f, _)) = self.compile_normalized(body, host) else {
            let t = body.trim_start();
            if t.starts_with('[')
                && t.split_once(']')
                    .is_some_and(|(_, r)| r.starts_with(|c: char| c.is_alphabetic() || c == '$'))
            {
                return Evaluated::Value(Value::Error(XlError::NAME));
            }
            return bad();
        };
        poison_indirect(&mut f.root, true);

        if let Some(h) = self.function_host.as_deref() {
            use crate::formula::Node;
            let mut calls: Vec<(*const Node, &str)> = Vec::new();
            crate::formula::walk(&f.root, &mut |n| {
                if let Node::NamedCall { key, .. } = n {
                    if h.signature(key).is_some() {
                        calls.push((n as *const Node, key));
                    }
                }
            });
            for (k, (node, key)) in calls.iter().enumerate() {
                if !h.refused_after_host_call(key) {
                    continue;
                }
                let enclosing = calls[..k]
                    .iter()
                    .filter(|(outer, _)| {
                        let mut inside = false;

                        if let Node::NamedCall { args, .. } = unsafe { &**outer } {
                            for a in args {
                                crate::formula::walk(a, &mut |m| {
                                    inside |= std::ptr::eq(m, *node);
                                });
                            }
                        }
                        inside
                    })
                    .count();
                if k > enclosing {
                    return Evaluated::Value(Value::Error(XlError::NAME));
                }
            }
        }
        let mut ctx = EvalCtx::with_mode(self, host, crate::eval::ReadMode::Stale);
        ctx.outside_formula = true;
        let op = ctx.eval(&f.root);
        if ctx.interrupted() && !matches!(op, crate::eval::Operand::Ref(_)) {
            return bad();
        }
        match op {
            crate::eval::Operand::Ref(Reference::Area(a)) => Evaluated::Ref(vec![a]),
            crate::eval::Operand::Ref(Reference::Multi(m)) => Evaluated::Ref(m.to_vec()),
            op => Evaluated::Value(ctx.deref(op)),
        }
    }

    pub fn dot_notation_range(&self, text: &str, host: CellAddr) -> Option<Vec<Area>> {
        use crate::eval::{EvalCtx, Operand, Reference};
        use crate::formula::Node;
        let body = text.strip_prefix('=').unwrap_or(text);
        let (f, _) = self.compile_normalized(body, host).ok()?;
        let field = crate::functions::lookup(crate::functions::info::FIELD_FN)?;
        let mut node = &f.root;
        let mut fields = 0;
        while let Node::Func { id, args } = node {
            if *id != field {
                break;
            }
            node = args.first()?;
            fields += 1;
        }
        if fields == 0 || !matches!(node, Node::Ref(..)) {
            return None;
        }
        let mut ctx = EvalCtx::with_mode(self, host, crate::eval::ReadMode::Stale);
        match ctx.eval(node) {
            Operand::Ref(Reference::Area(a)) => Some(vec![a]),
            Operand::Ref(Reference::Multi(m)) => Some(m.to_vec()),
            _ => None,
        }
    }

    pub fn note_dot_notation_name(&mut self, text: &str) {
        self.names.note_hidden(text.trim());
    }

    pub fn call_function(&self, at: CellAddr, name: &str, args: &[Arg]) -> Option<Value> {
        use crate::eval::{EvalCtx, Operand, ReadMode, Reference};
        Some(match self.call_function_in(at, name, args, &self.locale)? {
            Evaluated::Value(v) => v,
            Evaluated::Ref(areas) => {
                let mut ctx = EvalCtx::with_mode(self, at, ReadMode::Stale);
                let r = match areas.as_slice() {
                    [a] => Reference::Area(*a),
                    _ => Reference::Multi(areas.into()),
                };
                crate::eval::finalize_keeping_nil(ctx.deref(Operand::Ref(r)))
            }
        })
    }

    pub fn call_function_in(
        &self,
        at: CellAddr,
        name: &str,
        args: &[Arg],
        locale: &crate::Locale,
    ) -> Option<Evaluated> {
        use crate::eval::{EvalCtx, Operand, ReadMode, Reference};
        use crate::formula::{CRef, Node, Rc, SheetRef};
        use crate::parser::ast::{BinOp, UnaryOp};
        let id = crate::functions::lookup(&name.to_uppercase())?;
        let def = crate::functions::def(id);
        if args.len() < usize::from(def.min_args) || args.len() > usize::from(def.max_args) {
            return Some(Evaluated::Value(Value::Error(crate::value::XlError::VALUE)));
        }
        let abs = |v: u32| Rc {
            v: v as i32,
            abs: true,
        };
        let area_node = |area: &Area| {
            let r = area.rect;
            let cref = if r.is_cell() {
                CRef::Cell {
                    row: abs(r.r0),
                    col: abs(r.c0),
                }
            } else {
                CRef::Area {
                    r0: abs(r.r0),
                    c0: abs(r.c0),
                    r1: abs(r.r1),
                    c1: abs(r.c1),
                }
            };
            Node::Ref(SheetRef::Sheet(area.sheet), cref)
        };
        let mut nodes: Vec<Node> = args
            .iter()
            .map(|a| match a {
                Arg::Missing => Node::Missing,
                Arg::Value(v) => Node::Lit(v.clone()),
                Arg::Ref(areas) | Arg::XRef(areas) => {
                    let union = areas
                        .iter()
                        .map(area_node)
                        .reduce(|a, b| Node::Binary(BinOp::Union, Box::new(a), Box::new(b)));
                    match union {
                        Some(u) if areas.len() > 1 => Node::Paren(Box::new(u)),
                        Some(one) => one,
                        None => Node::Lit(Value::Error(crate::value::XlError::REF)),
                    }
                }
            })
            .collect();

        let root = match (def.name, nodes.len()) {
            ("SINGLE", 1) => Node::Unary(UnaryOp::Implicit, Box::new(nodes.remove(0))),
            _ => Node::Func { id, args: nodes },
        };
        let mut ctx = EvalCtx::with_mode(self, at, ReadMode::Stale);
        ctx.locale = locale;
        ctx.outside_formula = true;

        let op = ctx.call_from_outside(&root);
        let areas = match &op {
            Operand::Ref(Reference::Area(a)) => Some(vec![*a]),
            Operand::Ref(Reference::Multi(m)) => Some(m.to_vec()),
            _ => None,
        };
        Some(match areas {
            Some(areas) if crate::functions::returns_ref(def.name) => Evaluated::Ref(areas),
            _ => Evaluated::Value(crate::eval::finalize_keeping_nil(ctx.convert_back(op))),
        })
    }
}

fn poison_indirect(node: &mut crate::formula::Node, keep: bool) {
    use crate::formula::Node;
    use crate::parser::ast::BinOp;
    let fid = |n: &str| crate::functions::lookup(n);
    match node {
        Node::Func { id, args } => {
            let (indirect, if_, choose, index, offset) = (
                fid("INDIRECT"),
                fid("IF"),
                fid("CHOOSE"),
                fid("INDEX"),
                fid("OFFSET"),
            );
            let me = Some(*id);
            if me == indirect {
                if !keep {
                    *node = Node::Lit(Value::Error(crate::value::XlError::REF));
                    return;
                }
                for a in args.iter_mut() {
                    poison_indirect(a, true);
                }
                return;
            }
            for (i, a) in args.iter_mut().enumerate() {
                let pass = keep
                    && ((me == if_ || me == choose) && i > 0
                        || (me == index || me == offset) && i == 0);
                poison_indirect(a, pass);
            }
        }
        Node::NamedCall { args, .. } => args.iter_mut().for_each(|a| poison_indirect(a, false)),
        Node::Call { callee, args } => {
            poison_indirect(callee, false);
            args.iter_mut().for_each(|a| poison_indirect(a, false));
        }
        Node::Unary(_, x) | Node::Percent(x) | Node::Spill(x) => poison_indirect(x, false),
        Node::Paren(x) => poison_indirect(x, keep),
        Node::Ws { node, .. } => poison_indirect(node, keep),
        Node::Binary(op, a, b) => {
            let pass = keep && matches!(op, BinOp::Range | BinOp::Intersect | BinOp::Union);
            poison_indirect(a, pass);
            poison_indirect(b, pass);
        }
        Node::Let { values, body, .. } => {
            values.iter_mut().for_each(|v| poison_indirect(v, keep));
            poison_indirect(body, keep);
        }
        _ => {}
    }
}
