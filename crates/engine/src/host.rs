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
}

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub struct RuntimeError;

pub trait FunctionHost: Send + Sync {
    fn signature(&self, name: &str) -> Option<Signature>;

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
}

impl Unseen {
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
}

impl<'a> CallContext<'a> {
    pub(crate) fn new(view: ReadView<'a>, volatile: bool) -> CallContext<'a> {
        CallContext { view, volatile }
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

impl Workbook {
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
    let mut vals = Vec::with_capacity(args.len());

    let mut waits: Option<FormulaId> = None;
    let mut unseen: Vec<Area> = Vec::new();
    let caller = ctx.wb.formula_at(ctx.host);
    for (i, a) in args.iter().enumerate() {
        let op = ctx.eval(a);
        let arg = match (sig.kind(i), op) {
            (_, Operand::Missing) => Arg::Missing,
            (ParamKind::Reference, Operand::Ref(r)) if !matches!(r, Reference::Grid(_)) => {
                let clear = !ctx.interrupted();
                for area in r.areas() {
                    ctx.visit_area_until(area, |_, _, _| true);
                }
                if let Some(Interrupt::Pending(g)) = ctx.interrupt {
                    if clear && Some(g) != caller && ctx.read_mode() == ReadMode::Calc {
                        ctx.interrupt = None;
                        if waits.is_none() {
                            waits = Some(if ctx.wb.settings.multi_threaded {
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
                Arg::Ref(r.areas().to_vec())
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
    let out = if unseen.is_empty() {
        host.call(key, &vals, &mut cx)
    } else {
        ctx.wb.calc.unseen.begin(unseen);
        let out = host.call(key, &vals, &mut cx);
        ctx.wb.calc.unseen.end();
        out
    };
    let volatile = ctx.observed.host_volatile.unwrap_or(false) || cx.is_volatile();
    ctx.observed.host_volatile = Some(volatile);
    if let Some(g) = waits {
        ctx.interrupt = Some(Interrupt::Pending(g));
        return Operand::err(XlError::CALC);
    }
    match out {
        Ok(v) => Operand::Val(v),
        Err(RuntimeError) => Operand::err(XlError::VALUE),
    }
}

impl Workbook {
    pub fn call_function(&self, at: CellAddr, name: &str, args: &[Arg]) -> Option<Value> {
        use crate::formula::{CRef, Node, Rc, SheetRef};
        let id = crate::functions::lookup(&name.to_uppercase())?;
        let def = crate::functions::def(id);
        if args.len() < usize::from(def.min_args) || args.len() > usize::from(def.max_args) {
            return Some(Value::Error(crate::value::XlError::VALUE));
        }
        let abs = |v: u32| Rc {
            v: v as i32,
            abs: true,
        };
        let nodes = args
            .iter()
            .map(|a| match a {
                Arg::Missing => Node::Missing,
                Arg::Value(v) => Node::Lit(v.clone()),
                Arg::Ref(areas) => match areas.first() {
                    Some(area) => {
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
                    }
                    None => Node::Lit(Value::Error(crate::value::XlError::REF)),
                },
            })
            .collect();
        let root = Node::Func { id, args: nodes };
        let mut ctx = crate::eval::EvalCtx::with_mode(self, at, crate::eval::ReadMode::Stale);
        Some(crate::eval::eval_formula(&mut ctx, &root))
    }
}
