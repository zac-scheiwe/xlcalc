use std::collections::BTreeMap;

use rustc_hash::{FxHashMap, FxHashSet};

use crate::address::{Rect, SheetId};
use crate::calc::RecalcReport;
use crate::eval::{eval_formula, EvalCtx, Interrupt};
use crate::graph::Prec;
use crate::value::{Array, Value, XlError};
use crate::workbook::{CalcMode, Entry, FormulaId, Workbook};

pub(crate) const HEAD: u32 = u32::MAX;

const MAX_PASSES: usize = 256;

#[derive(Clone, Copy, PartialEq, Eq, Hash, Debug)]
pub(crate) struct Node {
    pub(crate) fid: FormulaId,
    pub(crate) m: u32,
}

impl Node {
    fn head(fid: FormulaId) -> Node {
        Node { fid, m: HEAD }
    }
}

#[derive(Clone, Copy, PartialEq, Eq, Debug)]
pub(crate) enum St {
    Clean,

    Edited,

    Left,

    Failed,

    Pending,

    Deferred,

    Stored,
}

fn pullable(s: St) -> bool {
    matches!(s, St::Edited | St::Left | St::Stored)
}

#[derive(Default)]
pub(crate) struct WalkState {
    valid: bool,
    epoch: u64,
    chain: Vec<Node>,
    st: FxHashMap<Node, St>,

    ident: FxHashMap<FormulaId, Ident>,

    pub(crate) running: bool,

    pub(crate) dirtied: Vec<FormulaId>,

    spill_nodes: Vec<Node>,
}

type Ident = (i64, i64, Option<(u32, u32)>);

impl WalkState {
    pub(crate) fn state(&self, n: Node) -> St {
        self.st.get(&n).copied().unwrap_or(St::Clean)
    }

    fn set(&mut self, n: Node, s: St) {
        if s == St::Clean {
            self.st.remove(&n);
        } else {
            self.st.insert(n, s);
        }
    }

    pub(crate) fn tracking(&self) -> bool {
        self.valid && !self.running
    }

    pub(crate) fn in_use(&self) -> bool {
        self.valid
    }

    pub(crate) fn renumber(&mut self, keys: &FxHashMap<FormulaId, i64>, epoch: u64) {
        for (f, id) in self.ident.iter_mut() {
            if let Some(&k) = keys.get(f) {
                id.0 = k;
            }
        }
        self.epoch = epoch;
    }

    pub(crate) fn invalidate(&mut self) {
        if self.valid {
            *self = WalkState::default();
        }
    }
}

pub(crate) fn active(wb: &Workbook) -> bool {
    !wb.settings.multi_threaded && !wb.settings.iteration && wb.settings.mode == CalcMode::Automatic
}

fn live(wb: &Workbook, fid: FormulaId) -> bool {
    wb.formulas.get(fid as usize).is_some_and(|f| f.is_some())
}

fn members(wb: &Workbook, fid: FormulaId) -> Option<(u32, u32)> {
    wb.formula(fid).array.filter(|&(r, c)| r * c > 1)
}

fn nodes_of(wb: &Workbook, fid: FormulaId) -> Vec<Node> {
    let mut v = Vec::new();
    if let Some((r, c)) = members(wb, fid) {
        v.extend((0..r * c).map(|m| Node { fid, m }));
    } else {
        v.push(Node::head(fid));
    }
    v
}

pub(crate) fn node_at(wb: &Workbook, fid: FormulaId, row: u32, col: u32) -> Node {
    let f = wb.formula(fid);
    match members(wb, fid) {
        Some((r, c))
            if row >= f.addr.row
                && col >= f.addr.col
                && row - f.addr.row < r
                && col - f.addr.col < c =>
        {
            Node {
                fid,
                m: (row - f.addr.row) * c + (col - f.addr.col),
            }
        }
        _ => Node::head(fid),
    }
}

pub(crate) fn read_target(wb: &Workbook, n: Node) -> Option<(Node, bool)> {
    let w = &wb.calc.walk;
    match w.state(n) {
        St::Clean => None,
        St::Deferred => {
            let h = Node::head(n.fid);
            Some((h, pullable(w.state(h))))
        }
        s => Some((n, pullable(s))),
    }
}

pub(crate) fn first_in_areas(
    wb: &Workbook,
    caller: Option<FormulaId>,
    areas: &[crate::Area],
) -> Option<Node> {
    let w = &wb.calc.walk;
    w.chain.iter().copied().find(|&n| {
        if Some(n.fid) == caller || !live(wb, n.fid) || !pullable(w.state(n)) {
            return false;
        }
        let (sheet, rect) = node_area(wb, n);
        areas
            .iter()
            .any(|a| a.sheet == sheet && a.rect.intersects(&rect))
    })
}

fn node_area(wb: &Workbook, n: Node) -> (SheetId, Rect) {
    let f = wb.formula(n.fid);
    match (n.m, f.array) {
        (HEAD, _) => formula_area(wb, n.fid),
        (m, Some((_, c))) => (
            f.addr.sheet,
            Rect::cell(f.addr.row + m / c, f.addr.col + m % c),
        ),
        (_, None) => formula_area(wb, n.fid),
    }
}

pub(crate) fn partly_clean(wb: &Workbook, fid: FormulaId) -> bool {
    let w = &wb.calc.walk;
    w.ident.contains_key(&fid)
        && members(wb, fid).is_some()
        && w.state(Node::head(fid)) == St::Clean
        && nodes_of(wb, fid)
            .into_iter()
            .any(|n| matches!(w.state(n), St::Clean | St::Stored))
}

fn ident(wb: &Workbook, fid: FormulaId) -> Ident {
    let f = wb.formula(fid);
    (f.seq, f.born, f.array)
}

fn initial(wb: &Workbook, fid: FormulaId) -> Vec<(Node, St)> {
    let f = wb.formula(fid);
    let edited = |d: bool| if d { St::Edited } else { St::Clean };
    match members(wb, fid) {
        None => vec![(Node::head(fid), edited(f.dirty))],
        Some((r, c)) => {
            let fresh = f.dirty && f.stored == 0;
            let mut v = vec![(Node::head(fid), edited(fresh))];
            for m in 0..r * c {
                let s = if fresh { St::Deferred } else { edited(f.dirty) };
                v.push((Node { fid, m }, s));
            }
            v
        }
    }
}

fn apply_dirty(wb: &mut Workbook, fid: FormulaId) {
    let nodes = nodes_of(wb, fid);
    let block = members(wb, fid).is_some();
    let w = &mut wb.calc.walk;
    if block && w.state(Node::head(fid)) != St::Clean {
        w.set(Node::head(fid), St::Edited);
        return;
    }
    for n in nodes {
        if matches!(w.state(n), St::Clean | St::Stored) {
            w.set(n, St::Edited);
        }
    }
}

fn sync(wb: &mut Workbook) {
    let epoch = wb.calc.chain_epoch;
    let rebuild = !wb.calc.walk.valid || wb.calc.walk.epoch != epoch;
    if rebuild {
        wb.calc.walk = WalkState::default();
    } else {
        let ok: FxHashSet<FormulaId> = wb
            .calc
            .walk
            .ident
            .iter()
            .filter(|(&g, id)| live(wb, g) && ident(wb, g) == **id)
            .map(|(&g, _)| g)
            .collect();
        let w = &mut wb.calc.walk;
        w.chain.retain(|n| ok.contains(&n.fid));
        w.st.retain(|n, _| ok.contains(&n.fid));
        w.ident.retain(|g, _| ok.contains(g));
    }

    let order: Vec<FormulaId> = wb
        .calc
        .chain
        .iter()
        .filter(|&(&k, &g)| {
            wb.formulas
                .get(g as usize)
                .and_then(|f| f.as_ref())
                .is_some_and(|f| f.seq == k)
        })
        .map(|(_, &g)| g)
        .collect();
    let mut front = Vec::new();
    for g in order {
        if wb.calc.walk.ident.contains_key(&g) {
            continue;
        }
        let id = ident(wb, g);
        for (n, s) in initial(wb, g) {
            front.push(n);
            wb.calc.walk.set(n, s);
        }
        wb.calc.walk.ident.insert(g, id);
    }
    let rest = std::mem::take(&mut wb.calc.walk.chain);
    front.extend(rest);
    wb.calc.walk.chain = front;

    let mut dirtied = std::mem::take(&mut wb.calc.walk.dirtied);
    let fids: Vec<FormulaId> = wb.calc.walk.ident.keys().copied().collect();
    for g in fids {
        let all_clean = nodes_of(wb, g)
            .into_iter()
            .chain(std::iter::once(Node::head(g)))
            .all(|n| wb.calc.walk.state(n) == St::Clean);
        if wb.formula(g).dirty && all_clean {
            dirtied.push(g);
        }
    }
    for g in dirtied {
        if live(wb, g) && wb.calc.walk.ident.get(&g) == Some(&ident(wb, g)) {
            apply_dirty(wb, g);
        }
    }

    let w = &mut wb.calc.walk;
    for n in w.chain.clone() {
        if matches!(w.state(n), St::Left | St::Failed) {
            w.set(n, St::Edited);
        }
    }
    w.epoch = epoch;
    w.valid = true;
}

fn any_uncalc(wb: &Workbook) -> bool {
    let w = &wb.calc.walk;
    w.chain.iter().any(|&n| w.state(n) != St::Clean)
}

pub(crate) fn recalc(wb: &mut Workbook, report: &mut RecalcReport) {
    sync(wb);
    if trace_on() {
        eprintln!("=== recalc {}", wb.calc.pass);
    }
    wb.calc.walk.running = true;
    if any_uncalc(wb) {
        let mut passes = 0;
        let mut saved = None;
        while round(wb, 0, report, &mut passes, &mut saved) && passes < MAX_PASSES {
            let w = &mut wb.calc.walk;
            for n in w.chain.clone() {
                if matches!(w.state(n), St::Edited | St::Left | St::Failed) {
                    w.set(n, St::Left);
                }
            }
        }
        if any_uncalc(wb) && passes < MAX_PASSES {
            let p = saved.unwrap_or(0);
            round(wb, p, report, &mut passes, &mut saved);
        }
    }
    finish(wb);
    wb.calc.walk.running = false;
}

fn round(
    wb: &mut Workbook,
    mut pos: usize,
    report: &mut RecalcReport,
    passes: &mut usize,
    saved: &mut Option<usize>,
) -> bool {
    let mut circular = false;
    loop {
        let p = pass(wb, pos, report);
        *passes += 1;
        circular |= p.circular;
        *saved = p.valid.then_some(p.next);
        if !(p.progress && p.valid && *passes < MAX_PASSES) {
            return circular;
        }
        pos = p.next;
    }
}

struct PassEnd {
    next: usize,
    valid: bool,
    progress: bool,
    circular: bool,
}

struct Pass {
    removed: Vec<bool>,
    reins: BTreeMap<usize, Vec<Node>>,
    u: Vec<(Node, St)>,
    in_u: FxHashSet<Node>,
    slot: FxHashMap<Node, usize>,
    progress: bool,
    circular: bool,
}

impl Pass {
    fn move_to_u(&mut self, n: Node, s: St) {
        self.u.push((n, s));
        self.in_u.insert(n);
        if let Some(&k) = self.slot.get(&n) {
            self.removed[k] = true;
        }
    }

    fn reinsert(&mut self, root_slot: usize, n: Node) {
        self.reins.entry(root_slot).or_default().push(n);
        if let Some(&k) = self.slot.get(&n) {
            self.removed[k] = true;
        }
    }

    fn unwind(&mut self, stack: &mut Vec<(Node, St)>, wb: &mut Workbook) {
        while let Some((n, s)) = stack.pop() {
            let keep = if s == St::Failed { St::Left } else { s };
            self.move_to_u(n, keep);
            wb.calc.walk.set(n, St::Failed);
        }
    }
}

fn label(wb: &Workbook, n: Node) -> String {
    let f = wb.formula(n.fid);
    let a = f.addr;
    match (n.m, f.array) {
        (HEAD, Some(_)) => format!("{{{}}}", a.a1()),
        (HEAD, None) => a.a1(),
        (m, Some((_, c))) => crate::CellAddr::new(a.sheet, a.row + m / c, a.col + m % c).a1(),
        (_, None) => a.a1(),
    }
}

fn trace_on() -> bool {
    static ON: std::sync::OnceLock<bool> = std::sync::OnceLock::new();
    *ON.get_or_init(|| std::env::var_os("XLCALC_WALK_TRACE").is_some())
}

fn dump(wb: &Workbook) -> String {
    let w = &wb.calc.walk;
    let v: Vec<String> = w
        .chain
        .iter()
        .filter(|n| live(wb, n.fid))
        .map(|&n| {
            let s = match w.state(n) {
                St::Clean => '0',
                St::Edited => 'f',
                St::Left => 'c',
                St::Failed => 'e',
                St::Pending => '3',
                St::Deferred => '4',
                St::Stored => '5',
            };
            format!("{}:{s}", label(wb, n))
        })
        .collect();
    v.join(" ")
}

fn pass(wb: &mut Workbook, start: usize, report: &mut RecalcReport) -> PassEnd {
    if trace_on() {
        eprintln!("@ start {start} {}", dump(wb));
    }
    let chain = wb.calc.walk.chain.clone();
    let len = chain.len();
    let mut p = Pass {
        removed: vec![false; len],
        reins: BTreeMap::new(),
        u: Vec::new(),
        in_u: FxHashSet::default(),
        slot: chain.iter().enumerate().map(|(i, &n)| (n, i)).collect(),
        progress: false,
        circular: false,
    };
    for (i, &x) in chain.iter().enumerate().skip(start) {
        if p.removed[i] || !live(wb, x.fid) {
            continue;
        }
        match wb.calc.walk.state(x) {
            St::Clean | St::Pending => {}
            St::Stored => wb.calc.walk.set(x, St::Clean),
            St::Deferred => {
                if p.in_u.contains(&Node::head(x.fid)) {
                    p.move_to_u(x, St::Deferred);
                }
            }
            St::Edited | St::Left | St::Failed => root(wb, i, x, &mut p, report),
        }
    }
    let mut out = Vec::with_capacity(len);
    for (k, &n) in chain.iter().enumerate() {
        if let Some(l) = p.reins.get(&k) {
            out.extend(l);
        }
        if !p.removed[k] {
            out.push(n);
        }
    }
    let next = out.len();
    for &(n, s) in &p.u {
        out.push(n);
        wb.calc.walk.set(n, s);
    }
    wb.calc.walk.chain = out;
    if trace_on() {
        eprintln!("@ end next={next} valid={} {}", !p.u.is_empty(), dump(wb));
    }
    PassEnd {
        next,
        valid: !p.u.is_empty(),
        progress: p.progress,
        circular: p.circular,
    }
}

enum Ev {
    Ok,

    Circular,
    Pull(Node),
    Fail,
}

fn root(wb: &mut Workbook, i: usize, x: Node, p: &mut Pass, report: &mut RecalcReport) {
    let mut stack: Vec<(Node, St)> = Vec::new();
    loop {
        let top = stack.last().map_or(x, |s| s.0);
        let ev = eval_node(wb, top, report);
        if trace_on() {
            let what = match &ev {
                Ev::Ok => String::new(),
                Ev::Circular => "?circ".into(),
                Ev::Pull(t) => format!("? +{}", label(wb, *t)),
                Ev::Fail => "? fail".into(),
            };
            eprintln!("  | {}{what}", label(wb, top));
        }
        match ev {
            Ev::Ok => {
                p.progress = true;
                wb.calc.walk.set(top, St::Clean);
                if stack.is_empty() {
                    return;
                }
                p.reinsert(i, top);
                stack.pop();
                if stack.is_empty() {
                    return;
                }
            }
            Ev::Circular => {
                p.progress = true;
                p.circular = true;
                if stack.is_empty() {
                    return;
                }

                p.reinsert(i, top);
                stack.pop();
                if stack.is_empty() {
                    return;
                }
            }
            Ev::Pull(t) if t != x && !stack.iter().any(|s| s.0 == t) => {
                if stack.is_empty() {
                    stack.push((x, wb.calc.walk.state(x)));
                    wb.calc.walk.set(x, St::Pending);
                }
                if wb.calc.walk.state(t) == St::Stored {
                    wb.calc.walk.set(t, St::Clean);
                    p.reinsert(i, t);
                    continue;
                }
                stack.push((t, wb.calc.walk.state(t)));
                wb.calc.walk.set(t, St::Pending);
            }
            Ev::Pull(_) | Ev::Fail => {
                if stack.is_empty() {
                    let s = wb.calc.walk.state(x);
                    if matches!(s, St::Edited | St::Left) {
                        p.move_to_u(x, s);
                    }
                    wb.calc.walk.set(x, St::Failed);
                    return;
                }
                p.unwind(&mut stack, wb);
                return;
            }
        }
    }
}

fn eval_node(wb: &mut Workbook, n: Node, report: &mut RecalcReport) -> Ev {
    let fid = n.fid;
    let block = members(wb, fid);
    let (formula, addr) = {
        let f = wb.formula(fid);
        (f.formula.clone(), f.addr)
    };
    let (value, interrupt, observed, target) = {
        let mut ctx = EvalCtx::new(wb, addr);
        ctx.walk = true;
        ctx.allow_partial = block.is_some();
        let v = eval_formula(&mut ctx, &formula.root);
        let target = ctx.walk_target.or(ctx.walk_pend);
        (v, ctx.interrupt, ctx.observed, target)
    };
    report.evaluated(addr.sheet);
    match interrupt {
        Some(Interrupt::Pending(_)) => return target.map_or(Ev::Fail, Ev::Pull),
        Some(Interrupt::Cycle(_)) => return Ev::Fail,
        None => {}
    }
    crate::calc::note_host_volatile(wb, fid, observed.host_volatile);
    let read_spill = observed.read_spill && !observed.spill_masked;
    match block {
        None => store_formula(wb, n, value, read_spill, report),
        Some((rows, cols)) if n.m == HEAD => {
            store_header(wb, fid, value, (rows, cols), read_spill, report)
        }
        Some((rows, cols)) => {
            let ev = store_member(wb, n, value, (rows, cols), report);
            if matches!(ev, Ev::Ok) && read_spill {
                wb.formula_mut(fid).spill_src = true;
                wb.calc.walk.spill_nodes.push(n);
            }
            ev
        }
    }
}

fn store_formula(
    wb: &mut Workbook,
    n: Node,
    value: Value,
    read_spill: bool,
    report: &mut RecalcReport,
) -> Ev {
    let fid = n.fid;
    let (addr, old) = {
        let f = wb.formula(fid);
        (f.addr, f.spill)
    };
    wb.calc.read_spill = read_spill;
    crate::calc::store_result(wb, fid, value, report);
    let new = wb.formula(fid).spill;
    if old == new || wb.formula(fid).array.is_some() {
        return Ev::Ok;
    }

    wb.calc.walk.set(n, St::Clean);
    let mut cells = Vec::new();
    for r in [old, new].into_iter().flatten() {
        if r.rows() as u64 * r.cols() as u64 > 4096 {
            cells.push(r);
            continue;
        }
        for row in r.r0..=r.r1 {
            for col in r.c0..=r.c1 {
                if (row, col) != (addr.row, addr.col) {
                    cells.push(Rect::cell(row, col));
                }
            }
        }
    }
    fail_readers(wb, addr.sheet, cells);
    let circular = wb.calc.walk.state(n) == St::Failed
        || new.is_some_and(|r| reads_area(wb, fid, addr.sheet, &r, (addr.row, addr.col)));
    if circular {
        wb.calc.walk.set(n, St::Failed);
        Ev::Circular
    } else {
        Ev::Ok
    }
}

fn reads_area(
    wb: &Workbook,
    fid: FormulaId,
    sheet: SheetId,
    rect: &Rect,
    anchor: (u32, u32),
) -> bool {
    wb.formula(fid).precedents.iter().any(|p| match p {
        Prec::Cell(s, r, c) => *s == sheet && rect.contains(*r, *c) && (*r, *c) != anchor,
        Prec::Range(s, pr) => {
            *s == sheet && pr.intersects(rect) && !(pr.is_cell() && (pr.r0, pr.c0) == anchor)
        }
        Prec::Name(_) => false,
    })
}

fn fail_readers(wb: &mut Workbook, sheet: SheetId, cells: Vec<Rect>) {
    let mut work: Vec<(SheetId, Rect)> = cells.into_iter().map(|r| (sheet, r)).collect();
    let mut done: FxHashSet<FormulaId> = FxHashSet::default();
    let mut deps = Vec::new();
    while let Some((s, r)) = work.pop() {
        deps.clear();
        wb.graph.dependents_of_rect(s, &r, &mut deps);
        if let Some(sh) = wb.sheet(s) {
            deps.extend(sh.spills.anchors_wanting_rect(&r));
        }
        for &d in &deps {
            if !live(wb, d) || !done.insert(d) {
                continue;
            }
            for n in nodes_of(wb, d) {
                if !matches!(wb.calc.walk.state(n), St::Deferred | St::Pending) {
                    wb.calc.walk.set(n, St::Failed);
                }
            }
            work.push(formula_area(wb, d));
        }
    }
}

fn formula_area(wb: &Workbook, fid: FormulaId) -> (SheetId, Rect) {
    let f = wb.formula(fid);
    let (row, col) = (f.addr.row, f.addr.col);
    let rect = match (f.array, f.spill) {
        (Some((rows, cols)), _) => Rect::new(row, col, row + rows - 1, col + cols - 1),
        (None, Some(spill)) => spill,
        (None, None) => Rect::cell(row, col),
    };
    (f.addr.sheet, rect)
}

fn is_circ(v: &Value) -> bool {
    matches!(v, Value::Error(XlError::CIRC))
}

fn store_header(
    wb: &mut Workbook,
    fid: FormulaId,
    value: Value,
    (rows, cols): (u32, u32),
    read_spill: bool,
    report: &mut RecalcReport,
) -> Ev {
    let Value::Array(new) = crate::cse::fit_circ(value, rows, cols) else {
        return Ev::Fail;
    };
    if new.data.iter().all(is_circ) {
        return Ev::Fail;
    }
    let old = match crate::cse::fit(wb.formula(fid).value.clone(), rows, cols) {
        Value::Array(a) => a.data.clone(),
        _ => vec![Value::Number(0.0); new.data.len()],
    };
    let mask: Vec<bool> = new.data.iter().map(is_circ).collect();
    let data = new
        .data
        .iter()
        .zip(old.iter())
        .map(|(n, o)| match n {
            Value::Error(XlError::CIRC) => match o {
                Value::Blank | Value::Error(XlError::CIRC) => Value::Number(0.0),
                o => o.clone(),
            },
            n => n.clone(),
        })
        .collect();
    wb.calc.read_spill = read_spill;
    crate::calc::store_result(wb, fid, Array::new(rows, cols, data).into_value(), report);
    for (m, failed) in mask.into_iter().enumerate() {
        let s = if failed { St::Edited } else { St::Stored };
        wb.calc.walk.set(Node { fid, m: m as u32 }, s);
    }
    Ev::Ok
}

fn store_member(
    wb: &mut Workbook,
    n: Node,
    value: Value,
    (rows, cols): (u32, u32),
    report: &mut RecalcReport,
) -> Ev {
    let fid = n.fid;
    let Value::Array(new) = crate::cse::fit_circ(value, rows, cols) else {
        return Ev::Fail;
    };
    let Some(v) = new.data.get(n.m as usize).cloned() else {
        return Ev::Fail;
    };
    if is_circ(&v) {
        return member_target(wb, fid, n.m / cols, n.m % cols);
    }
    let addr = wb.formula(fid).addr;
    let mut data = match crate::cse::fit(wb.formula(fid).value.clone(), rows, cols) {
        Value::Array(a) => a.data.clone(),
        _ => vec![Value::Number(0.0); (rows * cols) as usize],
    };
    let i = n.m as usize;
    if !data[i].same_as(&v) {
        report.changed.push(addr);
    }
    data[i] = v;
    wb.formula_mut(fid).value = Array::new(rows, cols, data).into_value();
    Ev::Ok
}

fn member_target(wb: &Workbook, fid: FormulaId, i: u32, j: u32) -> Ev {
    for p in &wb.formula(fid).precedents {
        let (s, r, c) = match p {
            Prec::Cell(s, r, c) => (*s, *r, *c),
            Prec::Range(s, rect) => {
                let ri = if rect.rows() == 1 { 0 } else { i };
                let cj = if rect.cols() == 1 { 0 } else { j };
                if ri >= rect.rows() || cj >= rect.cols() {
                    continue;
                }
                (*s, rect.r0 + ri, rect.c0 + cj)
            }
            Prec::Name(_) => continue,
        };
        let Some(sh) = wb.sheet(s) else { continue };
        let g = match sh.cells.get(r, c) {
            Some(Entry::Formula(g)) => *g,
            Some(Entry::Const(_)) => continue,
            None => match sh.spills.owner_of(r, c) {
                Some(g) => g,
                None => continue,
            },
        };
        if let Some((t, pull)) = read_target(wb, node_at(wb, g, r, c)) {
            return if pull { Ev::Pull(t) } else { Ev::Fail };
        }
    }
    Ev::Fail
}

fn finish(wb: &mut Workbook) {
    wb.calc.circular.clear();
    let chain = wb.calc.walk.chain.clone();
    for &n in &chain {
        if wb.calc.walk.state(n) != St::Clean && live(wb, n.fid) {
            wb.calc.circular.insert(n.fid);
            crate::calc::note_circular(wb, n.fid);
        }
    }
    for fid in std::mem::take(&mut wb.calc.spill_dirty) {
        if !live(wb, fid) || wb.calc.circular.contains(&fid) {
            continue;
        }
        for n in nodes_of(wb, fid) {
            wb.calc.walk.set(n, St::Left);
        }
    }
    for n in std::mem::take(&mut wb.calc.walk.spill_nodes) {
        if live(wb, n.fid) && !wb.calc.circular.contains(&n.fid) {
            wb.calc.walk.set(n, St::Left);
        }
    }
    let mut seen = FxHashSet::default();
    for &n in &chain {
        if !live(wb, n.fid) || !seen.insert(n.fid) {
            continue;
        }
        let want = nodes_of(wb, n.fid)
            .into_iter()
            .chain(std::iter::once(Node::head(n.fid)))
            .any(|m| wb.calc.walk.state(m) != St::Clean);
        wb.set_dirty(n.fid, want);
    }
}
