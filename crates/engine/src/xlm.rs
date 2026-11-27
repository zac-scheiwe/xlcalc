use std::sync::Arc;

use rustc_hash::FxHashMap;

use crate::address::{CellAddr, Rect, SheetId};
use crate::eval::{eval_formula, EvalCtx, Reference};
pub use crate::eval::{Area, Operand};
use crate::formula::Node;
use crate::functions;
use crate::value::{Text, Value, XlError};
use crate::workbook::{Entry, Workbook};

pub type XlmVars = FxHashMap<(SheetId, Text), Operand>;

pub type XlmNames = FxHashMap<(SheetId, Text), String>;

#[derive(Clone, Debug, PartialEq)]
pub enum XlmError {
    Unsupported(String),

    NoReturn,

    Runaway,

    Failed(String),
}

pub trait XlmEnv {
    fn wb(&self) -> &Workbook;

    fn store(&mut self, addr: CellAddr, v: Value);

    fn command(&mut self, name: &str, args: &[Operand], at: CellAddr) -> Result<Value, XlmError>;

    fn write_values(&mut self, cells: Vec<(CellAddr, Value)>);

    fn persist(&mut self, _vars: &XlmVars, _names: &XlmNames) {}

    fn overlay(&self) -> Option<Arc<XlmCells>> {
        None
    }
}

thread_local! {

    static PENDING: std::cell::RefCell<Vec<(CellAddr, Value)>> = const { std::cell::RefCell::new(Vec::new()) };

    static PENDING_NAMES: std::cell::RefCell<Vec<(SheetId, String, String)>> = const { std::cell::RefCell::new(Vec::new()) };
}

impl Workbook {
    pub(crate) fn xlm_apply_pending(&mut self) {
        let writes = PENDING.with(|p| std::mem::take(&mut *p.borrow_mut()));
        for (a, v) in writes {
            if format!("{:?}", self.value(a)) != format!("{:?}", v) {
                self.xlm_write_value(a, v);
            }
        }
        let names = PENDING_NAMES.with(|p| std::mem::take(&mut *p.borrow_mut()));
        for (sheet, name, text) in names {
            let _ = self.define_name(&name, &text, Some(sheet));
        }
    }
}

pub const INFO_FUNCTIONS: &[&str] = &[
    "GET.WORKSPACE",
    "GET.DOCUMENT",
    "GET.WORKBOOK",
    "GET.FORMULA",
    "GET.NAME",
    "GET.DEF",
];

#[derive(Clone, Debug)]
pub struct XlmInfoAsk {
    pub host: CellAddr,

    pub name: String,

    pub args: Vec<Operand>,
    key: String,
}

#[derive(Default)]
pub struct XlmInfo {
    answers: FxHashMap<String, Value>,
    asked: Vec<XlmInfoAsk>,
}

pub(crate) fn info_call(ctx: &EvalCtx, name: &str, args: Vec<Operand>) -> Value {
    let key = format!("{name}|{:?}|{args:?}", ctx.host);
    let Ok(mut st) = ctx.wb.xlm_info.lock() else {
        return Value::Bool(false);
    };
    if let Some(v) = st.answers.get(&key) {
        return v.clone();
    }
    if !st.asked.iter().any(|a| a.key == key) {
        st.asked.push(XlmInfoAsk {
            host: ctx.host,
            name: name.to_string(),
            args,
            key,
        });
    }
    Value::Bool(false)
}

impl Workbook {
    pub fn xlm_info_take(&mut self) -> Vec<XlmInfoAsk> {
        self.xlm_info
            .get_mut()
            .map(|st| std::mem::take(&mut st.asked))
            .unwrap_or_default()
    }

    pub fn xlm_info_answer(&mut self, answers: Vec<(XlmInfoAsk, Value)>) {
        let mut hosts = Vec::new();
        if let Ok(st) = self.xlm_info.get_mut() {
            for (a, v) in answers {
                if !hosts.contains(&a.host) {
                    hosts.push(a.host);
                }
                st.answers.insert(a.key, v);
            }
        }
        for &h in &hosts {
            if let Some(fid) = self.formula_at(h) {
                crate::calc::mark_dirty(self, fid);
            }
        }
        if self.settings.mode == crate::workbook::CalcMode::Automatic {
            crate::calc::recalc(self);
        } else {
            crate::calc::recalc_only(self, &hosts);
        }
    }

    pub fn xlm_info_forget(&mut self) {
        if let Ok(st) = self.xlm_info.get_mut() {
            st.answers.clear();
            st.asked.clear();
        }
    }
}

pub fn const_text(v: &Value) -> Option<String> {
    fn item(v: &Value) -> Option<String> {
        Some(match v {
            Value::Number(n) => format!("{n}"),
            Value::Text(t) => format!("\"{}\"", t.replace('"', "\"\"")),
            Value::Bool(b) => (if *b { "TRUE" } else { "FALSE" }).to_string(),
            Value::Error(e) => format!("{e}"),
            _ => return None,
        })
    }
    match v {
        Value::Array(a) => {
            let rows: Option<Vec<String>> = (0..a.rows)
                .map(|r| {
                    let cells: Option<Vec<String>> = (0..a.cols)
                        .map(|c| item(&a.data[(r * a.cols + c) as usize]))
                        .collect();
                    cells.map(|c| c.join(","))
                })
                .collect();
            Some(format!("={{{}}}", rows?.join(";")))
        }
        v => item(v).map(|t| format!("={t}")),
    }
}

pub fn var_text(wb: &Workbook, op: &Operand) -> Option<String> {
    match op {
        Operand::Val(v) => const_text(v),
        Operand::Ref(r) if !r.areas().is_empty() => {
            let parts: Vec<String> = r
                .areas()
                .iter()
                .map(|a| {
                    let sn = wb
                        .sheet(a.sheet)
                        .map(|s| s.name.clone())
                        .unwrap_or_default();
                    let x = a.rect;
                    let end = if x.r0 == x.r1 && x.c0 == x.c1 {
                        String::new()
                    } else {
                        format!(":R{}C{}", x.r1 + 1, x.c1 + 1)
                    };
                    format!(
                        "'{}'!R{}C{}{end}",
                        sn.replace('\'', "''"),
                        x.r0 + 1,
                        x.c0 + 1
                    )
                })
                .collect();
            Some(format!("={}", parts.join(",")))
        }
        _ => None,
    }
}

pub type XlmCells = FxHashMap<(SheetId, u32, u32), Value>;

fn value_arg(wb: &Workbook, op: &Operand, at: CellAddr) -> Value {
    let r = match op {
        Operand::Val(v) => return v.clone(),
        Operand::Missing => return Value::Number(0.0),
        Operand::Ref(r) => r,
    };
    let [a] = r.areas() else {
        return Value::Error(XlError::VALUE);
    };
    let x = a.rect;
    let cell = if x.is_cell() {
        Some((x.r0, x.c0))
    } else if x.c0 == x.c1 && (x.r0..=x.r1).contains(&at.row) {
        Some((at.row, x.c0))
    } else if x.r0 == x.r1 && (x.c0..=x.c1).contains(&at.col) {
        Some((x.r0, at.col))
    } else {
        None
    };
    cell.map_or(Value::Error(XlError::VALUE), |(r, c)| {
        match wb.value(CellAddr::new(a.sheet, r, c)) {
            Value::Blank => Value::Number(0.0),
            v => v,
        }
    })
}

fn writable<'o>(wb: &Workbook, target: &'o Operand, sheet: SheetId) -> Option<&'o Area> {
    let Operand::Ref(r) = target else {
        return None;
    };
    let [a] = r.areas() else {
        return None;
    };
    let own = a.sheet == sheet && wb.sheet(sheet).is_some_and(|s| s.kind.is_macro());
    own.then_some(a)
}

fn lay_value(sheet: SheetId, rect: Rect, v: &Value) -> Vec<(CellAddr, Value)> {
    let mut out = Vec::new();
    for r in rect.r0..=rect.r1 {
        for c in rect.c0..=rect.c1 {
            let x = match v {
                Value::Array(a) => {
                    let (i, j) = (r - rect.r0, c - rect.c0);
                    let i = if a.rows == 1 { 0 } else { i };
                    let j = if a.cols == 1 { 0 } else { j };
                    if i < a.rows && j < a.cols {
                        a.data[(i * a.cols + j) as usize].clone()
                    } else {
                        Value::Error(XlError::NA)
                    }
                }
                v => v.clone(),
            };
            out.push((CellAddr::new(sheet, r, c), x));
        }
    }
    out
}

pub fn set_value_command(env: &mut dyn XlmEnv, args: &[Operand], at: CellAddr) -> Value {
    let [target, value, ..] = args else {
        return Value::Error(XlError::VALUE);
    };
    let Some(area) = writable(env.wb(), target, at.sheet) else {
        return Value::Error(XlError::VALUE);
    };
    let v = value_arg(env.wb(), value, at);
    let cells = lay_value(area.sheet, area.rect, &v);
    env.write_values(cells);
    Value::Bool(true)
}

pub struct ReadOnlyEnv<'a> {
    wb: &'a Workbook,
    cells: Arc<XlmCells>,
}

impl<'a> ReadOnlyEnv<'a> {
    pub fn new(wb: &'a Workbook) -> Self {
        ReadOnlyEnv {
            wb,
            cells: Arc::new(XlmCells::default()),
        }
    }
}

impl XlmEnv for ReadOnlyEnv<'_> {
    fn wb(&self) -> &Workbook {
        self.wb
    }
    fn store(&mut self, _addr: CellAddr, _v: Value) {}
    fn command(&mut self, name: &str, args: &[Operand], at: CellAddr) -> Result<Value, XlmError> {
        if name == "SET.VALUE" {
            return Ok(set_value_command(self, args, at));
        }
        Ok(Value::Bool(false))
    }
    fn write_values(&mut self, writes: Vec<(CellAddr, Value)>) {
        let cells = Arc::make_mut(&mut self.cells);
        for (a, v) in &writes {
            cells.insert((a.sheet, a.row, a.col), v.clone());
        }
        PENDING.with(|p| p.borrow_mut().extend(writes));
    }
    fn persist(&mut self, vars: &XlmVars, names: &XlmNames) {
        let defs: Vec<(SheetId, String, String)> = vars
            .iter()
            .filter_map(|((sheet, key), op)| {
                let text = var_text(self.wb, op)?;
                let shown = names
                    .get(&(*sheet, key.clone()))
                    .cloned()
                    .unwrap_or_else(|| key.to_string());
                Some((*sheet, shown, text))
            })
            .collect();
        PENDING_NAMES.with(|p| p.borrow_mut().extend(defs));
    }
    fn overlay(&self) -> Option<Arc<XlmCells>> {
        Some(self.cells.clone())
    }
}

enum Flow {
    Next,
    Jump(CellAddr),
    Return(Value),
    Halt,
}

enum LoopKind {
    For {
        name: Text,
        end: f64,
        step: f64,
    },
    ForCell {
        name: Text,
        cells: Vec<CellAddr>,
        next: usize,
    },
    While,
}

struct Loop {
    head: CellAddr,
    kind: LoopKind,
}

struct Frame {
    pc: CellAddr,
    loops: Vec<Loop>,
    args: Vec<Operand>,
    arg_n: usize,
    flow: Option<Flow>,

    result: Option<i64>,
}

const MAX_STEPS: u64 = 20_000_000;
const MAX_DEPTH: u32 = 64;

pub struct Interp<'e> {
    env: &'e mut dyn XlmEnv,
    vars: Arc<XlmVars>,
    names: XlmNames,
    steps: u64,
    depth: u32,
    halted: bool,

    whole_array: bool,
}

pub fn run(env: &mut dyn XlmEnv, start: CellAddr, args: Vec<Operand>) -> Result<Value, XlmError> {
    let mut it = Interp {
        env,
        vars: Arc::new(XlmVars::default()),
        names: XlmNames::default(),
        steps: 0,
        depth: 0,
        halted: false,
        whole_array: false,
    };

    let r = it.call(start, args).map(|v| match v {
        Value::Array(a) if !it.whole_array => a.data.first().cloned().unwrap_or(Value::Blank),
        v => v,
    });
    let vars = std::mem::take(&mut it.vars);
    let names = std::mem::take(&mut it.names);
    it.env.persist(&vars, &names);
    r
}

fn strip(node: &Node) -> &Node {
    match node.unwrap_ws() {
        Node::Unary(crate::parser::ast::UnaryOp::Implicit, x) => strip(x),
        n => n,
    }
}

fn head_name(node: &Node) -> Option<(String, &[Node])> {
    match strip(node) {
        Node::NamedCall { key, args, .. } => Some((key.to_string(), args)),
        Node::Func { id, args } => Some((functions::def(*id).name.to_string(), args)),
        _ => None,
    }
}

fn is_cmd(name: &str) -> bool {
    functions::xlm::XLM_ONLY.contains(&name)
}

impl Interp<'_> {
    fn root_at(&self, a: CellAddr) -> Option<Arc<crate::formula::Formula>> {
        let wb = self.env.wb();
        let fid = wb.formula_at(a)?;
        Some(wb.formula(fid).formula.clone())
    }

    fn next_filled(&self, a: CellAddr) -> Option<CellAddr> {
        let wb = self.env.wb();
        let s = wb.sheet(a.sheet)?;
        let last = s.cells.bounds()?.r1;
        (a.row + 1..=last)
            .find(|&r| s.cells.get(r, a.col).is_some())
            .map(|r| CellAddr::new(a.sheet, r, a.col))
    }

    fn cell_is_blank(&self, a: CellAddr) -> bool {
        let wb = self.env.wb();
        wb.sheet(a.sheet)
            .is_none_or(|s| s.cells.get(a.row, a.col).is_none())
    }

    fn head_at(&self, a: CellAddr) -> Option<(String, usize)> {
        let f = self.root_at(a)?;
        let (n, args) = head_name(&f.root)?;
        let n = if n == "IF" && args.len() != 1 {
            return None;
        } else {
            n
        };
        Some((n, args.len()))
    }

    fn eval_node(&mut self, node: &Node, host: CellAddr) -> Operand {
        let mut ctx = EvalCtx::new(self.env.wb(), host);
        ctx.xlm_vars = Some(self.vars.clone());
        ctx.xlm_cells = self.env.overlay();
        ctx.eval(node)
    }

    fn eval_value(&mut self, node: &Node, host: CellAddr) -> Value {
        let mut ctx = EvalCtx::new(self.env.wb(), host);
        ctx.xlm_vars = Some(self.vars.clone());
        ctx.xlm_cells = self.env.overlay();
        eval_formula(&mut ctx, node)
    }

    fn deref(&mut self, op: Operand, host: CellAddr) -> Value {
        let mut ctx = EvalCtx::new(self.env.wb(), host);
        ctx.xlm_vars = Some(self.vars.clone());
        ctx.xlm_cells = self.env.overlay();
        match op {
            Operand::Missing => Value::Blank,
            op => ctx.deref(op),
        }
    }

    fn truthy(&mut self, op: Operand, host: CellAddr) -> Result<bool, Value> {
        let v = self.deref(op, host);
        match v {
            Value::Bool(b) => Ok(b),
            Value::Number(n) => Ok(n != 0.0),
            Value::Blank => Ok(false),
            Value::Error(e) => Err(Value::Error(e)),
            Value::Text(t) if t.eq_ignore_ascii_case("TRUE") => Ok(true),
            Value::Text(t) if t.eq_ignore_ascii_case("FALSE") => Ok(false),
            _ => Err(Value::Error(XlError::VALUE)),
        }
    }

    fn convert_result(&mut self, v: Value, ty: Option<i64>, host: CellAddr) -> Value {
        self.whole_array = ty.is_some_and(|t| t & 64 != 0);
        let Some(ty) = ty else { return v };
        let v = match v {
            Value::Array(_) if ty & 64 != 0 => return v,

            Value::Array(a) => a.data.first().cloned().unwrap_or(Value::Blank),
            v => v,
        };

        if matches!(v, Value::Error(_)) && (ty & 64 == 0 || ty & 7 != 0) {
            return v;
        }
        self.coerce_to(v, ty, host)
            .unwrap_or(Value::Error(XlError::VALUE))
    }

    fn coerce_to(&mut self, v: Value, ty: i64, host: CellAddr) -> Option<Value> {
        let v = match v {
            Value::Blank => Value::Number(0.0),

            Value::Array(a) if ty & 64 == 0 => match a.data.first() {
                Some(Value::Blank) | None => Value::Number(0.0),
                Some(x) => x.clone(),
            },
            v => v,
        };
        let bit = match &v {
            Value::Number(_) => 1,
            Value::Text(_) => 2,
            Value::Bool(_) => 4,
            Value::Error(_) => 16,
            Value::Array(_) => 64,
            _ => 0,
        };
        if bit & ty != 0 {
            return Some(v);
        }
        let ctx = EvalCtx::new(self.env.wb(), host);
        if ty & 1 != 0 {
            match &v {
                Value::Bool(b) => return Some(Value::Number(f64::from(u8::from(*b)))),
                Value::Text(_) => {
                    if let Ok(n) = ctx.to_number(&v) {
                        return Some(Value::Number(n));
                    }
                }
                _ => {}
            }
        }
        if ty & 2 != 0 && matches!(v, Value::Number(_) | Value::Bool(_)) {
            if let Ok(t) = crate::coerce::to_text(&v) {
                return Some(Value::Text(t));
            }
        }
        if ty & 4 != 0 {
            match &v {
                Value::Number(n) => return Some(Value::Bool(*n != 0.0)),
                Value::Text(t) if t.eq_ignore_ascii_case("TRUE") => return Some(Value::Bool(true)),
                Value::Text(t) if t.eq_ignore_ascii_case("FALSE") => {
                    return Some(Value::Bool(false))
                }
                _ => {}
            }
        }
        if ty & 64 != 0 {
            return Some(crate::value::Array::new(1, 1, vec![v]).into_value());
        }

        (ty & 16 != 0).then_some(Value::Error(XlError::VALUE))
    }

    fn number(&mut self, op: Operand, host: CellAddr) -> Option<f64> {
        match self.deref(op, host) {
            Value::Number(n) => Some(n),
            Value::Bool(b) => Some(f64::from(u8::from(b))),
            Value::Blank => Some(0.0),
            _ => None,
        }
    }

    fn needs(&self, node: &Node, sheet: SheetId) -> bool {
        let mut found = false;
        crate::formula::walk(node, &mut |n| match n {
            Node::NamedCall { key, .. } => {
                if is_cmd(key) || self.macro_target(key, sheet).is_some() {
                    found = true;
                }
            }

            Node::Call { callee, .. } if matches!(strip(callee), Node::Ref(..)) => {
                found = true;
            }
            _ => {}
        });
        found
    }

    fn macro_target(&self, key: &str, sheet: SheetId) -> Option<CellAddr> {
        name_target(self.env.wb(), key, sheet)
    }

    fn step(&mut self) -> Result<(), XlmError> {
        self.steps += 1;
        if self.steps > MAX_STEPS {
            return Err(XlmError::Runaway);
        }
        Ok(())
    }

    fn call(&mut self, start: CellAddr, args: Vec<Operand>) -> Result<Value, XlmError> {
        self.depth += 1;
        if self.depth > MAX_DEPTH {
            return Err(XlmError::Runaway);
        }
        let mut fr = Frame {
            pc: start,
            loops: Vec::new(),
            args,
            arg_n: 0,
            flow: None,
            result: None,
        };
        let out = loop {
            self.step()?;
            if self.halted {
                break Value::Blank;
            }
            let pc = fr.pc;
            if self.cell_is_blank(pc) {
                match self.next_filled(pc) {
                    Some(a) => {
                        fr.pc = a;
                        continue;
                    }
                    None => return Err(XlmError::NoReturn),
                }
            }
            let Some(f) = self.root_at(pc) else {
                fr.pc.row += 1;
                continue;
            };
            fr.flow = None;
            let flow = self.stmt(&mut fr, &f.root, pc)?;
            match flow {
                Flow::Next => fr.pc.row += 1,
                Flow::Jump(a) => fr.pc = a,
                Flow::Return(v) => break v,
                Flow::Halt => {
                    self.halted = true;
                    break Value::Blank;
                }
            }
        };
        self.depth -= 1;
        Ok(out)
    }

    fn scan(&self, from: CellAddr, stop_at_else: bool) -> Option<(CellAddr, String)> {
        let mut depth = 0u32;
        let mut a = from;
        for _ in 0..100_000 {
            a.row += 1;
            if a.row >= crate::address::MAX_ROWS {
                return None;
            }
            let Some((n, argc)) = self.head_at(a) else {
                continue;
            };
            match n.as_str() {
                "IF" if argc == 1 => depth += 1,
                "FOR" | "FOR.CELL" | "WHILE" => depth += 1,
                "END.IF" | "NEXT" if depth == 0 => return Some((a, n)),
                "END.IF" | "NEXT" => depth -= 1,
                "ELSE" | "ELSE.IF" if depth == 0 && stop_at_else => return Some((a, n)),
                _ => {}
            }
        }
        None
    }

    fn set_var(&mut self, host: CellAddr, name: &str, v: Operand) {
        let key: (SheetId, Text) = (host.sheet, name.to_uppercase().into());
        self.names
            .entry(key.clone())
            .or_insert_with(|| name.to_string());
        Arc::make_mut(&mut self.vars).insert(key, v);
    }

    fn stmt(&mut self, fr: &mut Frame, root: &Node, host: CellAddr) -> Result<Flow, XlmError> {
        let mut ctl = None;
        if let Some((name, args)) = head_name(root) {
            ctl = self.control(fr, &name, args, host)?;
        }
        if let Some(flow) = ctl {
            return Ok(flow);
        }

        let v = if self.needs(root, host.sheet) {
            let op = self.expr(fr, root, host)?;
            self.deref(op, host)
        } else {
            self.eval_value(root, host)
        };
        self.env.store(host, v);
        Ok(fr.flow.take().unwrap_or(Flow::Next))
    }

    fn control(
        &mut self,
        fr: &mut Frame,
        name: &str,
        args: &[Node],
        host: CellAddr,
    ) -> Result<Option<Flow>, XlmError> {
        let flow = match name {
            "IF" if args.len() == 1 => {
                let op = self.expr(fr, &args[0], host)?;
                match self.truthy(op, host) {
                    Ok(true) => Flow::Next,
                    Ok(false) | Err(_) => self.skip_branch(host),
                }
            }
            "ELSE.IF" | "ELSE" => match self.scan(host, false) {
                Some((a, _)) => Flow::Jump(CellAddr::new(a.sheet, a.row + 1, a.col)),
                None => Flow::Next,
            },
            "END.IF" => Flow::Next,
            "GOTO" => {
                let op = match args.first() {
                    Some(n) => self.expr(fr, n, host)?,
                    None => Operand::Missing,
                };
                match self.target_of(op, host) {
                    Some(a) => Flow::Jump(a),
                    None => Flow::Return(Value::Error(XlError::REF)),
                }
            }
            "RETURN" => {
                self.env.store(host, Value::Bool(true));
                let v = match args.first() {
                    Some(Node::Missing) | None => Value::Error(XlError::NA),
                    Some(n) => {
                        let op = self.expr(fr, n, host)?;
                        self.deref(op, host)
                    }
                };
                let v = if v.is_blank() { Value::Number(0.0) } else { v };
                Flow::Return(self.convert_result(v, fr.result, host))
            }
            "HALT" => Flow::Halt,
            "BREAK" => match fr.loops.pop() {
                Some(l) => match self.scan(l.head, false) {
                    Some((a, _)) => Flow::Jump(CellAddr::new(a.sheet, a.row + 1, a.col)),
                    None => Flow::Next,
                },
                None => Flow::Next,
            },
            "WHILE" => {
                let op = self.expr(fr, &args[0], host)?;
                let ok = self.truthy(op, host).unwrap_or(false);
                if ok {
                    if fr.loops.last().is_none_or(|l| l.head != host) {
                        fr.loops.push(Loop {
                            head: host,
                            kind: LoopKind::While,
                        });
                    }
                    Flow::Next
                } else {
                    if fr.loops.last().is_some_and(|l| l.head == host) {
                        fr.loops.pop();
                    }
                    match self.scan(host, false) {
                        Some((a, _)) => Flow::Jump(CellAddr::new(a.sheet, a.row + 1, a.col)),
                        None => Flow::Next,
                    }
                }
            }
            "FOR" => self.for_head(fr, args, host)?,
            "FOR.CELL" => self.for_cell_head(fr, args, host)?,
            "NEXT" => self.next(fr),
            "ARGUMENT" => {
                let n = match args.first() {
                    Some(n) => {
                        let op = self.expr(fr, n, host)?;
                        self.deref(op, host)
                    }
                    None => Value::Blank,
                };
                let given = fr.args.get(fr.arg_n).cloned();
                fr.arg_n += 1;
                let ty = match args.get(1) {
                    Some(Node::Missing) | None => None,
                    Some(n) => {
                        let op = self.expr(fr, n, host)?;
                        self.number(op, host).map(|n| n as i64)
                    }
                };
                let val = match given {
                    Some(Operand::Missing) | None => Operand::Val(Value::Error(XlError::NA)),

                    Some(Operand::Val(Value::Error(e)))
                        if ty.is_some_and(|t| t & (16 | 64) == 0) =>
                    {
                        return Ok(Some(Flow::Return(Value::Error(e))));
                    }

                    Some(Operand::Val(_)) if ty == Some(8) => {
                        Operand::Val(Value::Error(XlError::VALUE))
                    }

                    Some(Operand::Ref(r))
                        if ty.is_none_or(|t| t & (8 | 64) == 0)
                            && r.areas().iter().map(|a| a.rect.cell_count()).sum::<u64>() > 1 =>
                    {
                        Operand::Val(Value::Error(XlError::VALUE))
                    }

                    Some(o) => match ty {
                        Some(t) if t & 8 == 0 => {
                            let v = self.deref(o, host);
                            if let Value::Error(e) = v {
                                if t & (16 | 64) == 0 {
                                    return Ok(Some(Flow::Return(Value::Error(e))));
                                }
                            }
                            match self.coerce_to(v, t, host) {
                                Some(v) => Operand::Val(v),
                                None => {
                                    return Ok(Some(Flow::Return(Value::Error(XlError::VALUE))))
                                }
                            }
                        }
                        _ => o,
                    },
                };
                if let Value::Text(t) = n {
                    let mut val = val;
                    if let Some(r) = args.get(2) {
                        let target = self.expr(fr, r, host)?;
                        let by_ref =
                            matches!(val, Operand::Ref(_)) && ty.is_some_and(|t| t & 8 != 0);
                        let area = writable(self.env.wb(), &target, host.sheet).cloned();
                        if let Some(a) = area.filter(|_| !by_ref) {
                            let v = self.deref(val, host);
                            let (rows, cols) = match &v {
                                Value::Array(x) => (x.rows, x.cols),
                                _ => (1, 1),
                            };
                            let x = a.rect;
                            let cut = Rect::new(
                                x.r0,
                                x.c0,
                                x.r1.min(x.r0 + rows - 1),
                                x.c1.min(x.c0 + cols - 1),
                            );
                            self.env.write_values(lay_value(a.sheet, cut, &v));
                            val = target;
                        }
                    }
                    self.set_var(host, &t, val);
                }
                self.env.store(host, Value::Bool(true));
                Flow::Next
            }
            "RESULT" => {
                fr.result = match args.first() {
                    Some(Node::Missing) | None => None,
                    Some(n) => {
                        let op = self.expr(fr, n, host)?;
                        self.number(op, host).map(|n| n as i64)
                    }
                };
                self.env.store(host, Value::Bool(true));
                Flow::Next
            }
            "SET.NAME" => {
                let n = match args.first() {
                    Some(n) => {
                        let op = self.expr(fr, n, host)?;
                        self.deref(op, host)
                    }
                    None => Value::Blank,
                };
                if let (Value::Text(t), Some(vn)) = (n, args.get(1)) {
                    let mut op = self.expr(fr, vn, host)?;

                    if let Operand::Val(Value::Array(a)) = &op {
                        if !matches!(strip(vn), Node::Lit(_)) {
                            op = Operand::Val(a.data.first().cloned().unwrap_or(Value::Blank));
                        }
                    }
                    self.set_var(host, &t, op);
                }
                self.env.store(host, Value::Bool(true));
                Flow::Next
            }
            "RUN" => {
                let op = match args.first() {
                    Some(n) => self.expr(fr, n, host)?,
                    None => Operand::Missing,
                };
                let v = self.run_target(op, host, Vec::new())?;
                self.env.store(host, v);
                Flow::Next
            }
            _ => return Ok(None),
        };
        if matches!(
            name,
            "GOTO"
                | "IF"
                | "ELSE"
                | "ELSE.IF"
                | "END.IF"
                | "BREAK"
                | "WHILE"
                | "FOR"
                | "FOR.CELL"
                | "NEXT"
        ) {
            self.env.store(host, Value::Bool(true));
        }
        Ok(Some(flow))
    }

    fn skip_branch(&mut self, host: CellAddr) -> Flow {
        let mut at = host;
        loop {
            match self.scan(at, true) {
                Some((a, n)) if n == "ELSE.IF" => {
                    let cond = self.root_at(a).and_then(|f| match head_name(&f.root) {
                        Some((_, args)) => args.first().cloned(),
                        None => None,
                    });
                    let ok = match cond {
                        Some(c) => {
                            let op = self.eval_node(&c, a);
                            self.truthy(op, a).unwrap_or(false)
                        }
                        None => false,
                    };

                    self.env.store(a, Value::Bool(true));
                    if ok {
                        return Flow::Jump(CellAddr::new(a.sheet, a.row + 1, a.col));
                    }
                    at = a;
                }
                Some((a, n)) => {
                    if n == "ELSE" {
                        self.env.store(a, Value::Bool(true));
                    }
                    return Flow::Jump(CellAddr::new(a.sheet, a.row + 1, a.col));
                }
                None => return Flow::Next,
            }
        }
    }

    fn for_head(
        &mut self,
        fr: &mut Frame,
        args: &[Node],
        host: CellAddr,
    ) -> Result<Flow, XlmError> {
        let name = match args.first() {
            Some(n) => {
                let op = self.expr(fr, n, host)?;
                match self.deref(op, host) {
                    Value::Text(t) => t,
                    _ => return Ok(Flow::Next),
                }
            }
            None => return Ok(Flow::Next),
        };
        let num = |this: &mut Self, fr: &mut Frame, i: usize, d: f64| -> Result<f64, XlmError> {
            match args.get(i) {
                Some(Node::Missing) | None => Ok(d),
                Some(n) => {
                    let op = this.expr(fr, n, host)?;
                    Ok(this.number(op, host).unwrap_or(d))
                }
            }
        };
        let start = num(self, fr, 1, 1.0)?;
        let end = num(self, fr, 2, 1.0)?;
        let step = num(self, fr, 3, 1.0)?;
        let done = if step >= 0.0 {
            start > end
        } else {
            start < end
        };
        if done {
            return Ok(match self.scan(host, false) {
                Some((a, _)) => Flow::Jump(CellAddr::new(a.sheet, a.row + 1, a.col)),
                None => Flow::Next,
            });
        }
        self.set_var(host, &name, Operand::Val(Value::Number(start)));
        fr.loops.push(Loop {
            head: host,
            kind: LoopKind::For { name, end, step },
        });
        Ok(Flow::Next)
    }

    fn for_cell_head(
        &mut self,
        fr: &mut Frame,
        args: &[Node],
        host: CellAddr,
    ) -> Result<Flow, XlmError> {
        let name = match args.first() {
            Some(n) => {
                let op = self.expr(fr, n, host)?;
                match self.deref(op, host) {
                    Value::Text(t) => t,
                    _ => return Ok(Flow::Next),
                }
            }
            None => return Ok(Flow::Next),
        };
        let target = match args.get(1) {
            Some(n) => self.expr(fr, n, host)?,
            None => Operand::Missing,
        };
        let skip_blanks = match args.get(2) {
            Some(Node::Missing) | None => false,
            Some(n) => {
                let op = self.expr(fr, n, host)?;
                self.truthy(op, host).unwrap_or(false)
            }
        };
        let mut cells = Vec::new();
        if let Operand::Ref(r) = &target {
            for Area { sheet, rect } in r.areas() {
                for row in rect.r0..=rect.r1 {
                    for col in rect.c0..=rect.c1 {
                        let a = CellAddr::new(*sheet, row, col);
                        if skip_blanks && self.env.wb().value(a).is_blank() {
                            continue;
                        }
                        cells.push(a);
                    }
                }
            }
        }
        if cells.is_empty() {
            return Ok(match self.scan(host, false) {
                Some((a, _)) => Flow::Jump(CellAddr::new(a.sheet, a.row + 1, a.col)),
                None => Flow::Next,
            });
        }
        let first = cells[0];
        self.set_var(host, &name, cell_operand(first));
        fr.loops.push(Loop {
            head: host,
            kind: LoopKind::ForCell {
                name,
                cells,
                next: 1,
            },
        });
        Ok(Flow::Next)
    }

    fn next(&mut self, fr: &mut Frame) -> Flow {
        if let Some(Loop {
            head,
            kind: LoopKind::While,
        }) = fr.loops.last()
        {
            return Flow::Jump(*head);
        }
        let Some(l) = fr.loops.pop() else {
            return Flow::Next;
        };
        let head = l.head;
        match l.kind {
            LoopKind::While => Flow::Next,
            LoopKind::For { name, end, step } => {
                let key = (head.sheet, Text::from(name.to_uppercase()));
                let cur = match self.vars.get(&key) {
                    Some(Operand::Val(Value::Number(n))) => *n,
                    _ => 0.0,
                };
                let nxt = cur + step;
                let done = if step >= 0.0 { nxt > end } else { nxt < end };

                self.set_var(head, &name, Operand::Val(Value::Number(nxt)));
                if done {
                    Flow::Next
                } else {
                    fr.loops.push(Loop {
                        head,
                        kind: LoopKind::For { name, end, step },
                    });
                    Flow::Jump(CellAddr::new(head.sheet, head.row + 1, head.col))
                }
            }
            LoopKind::ForCell { name, cells, next } => {
                if next >= cells.len() {
                    Flow::Next
                } else {
                    self.set_var(head, &name, cell_operand(cells[next]));
                    fr.loops.push(Loop {
                        head,
                        kind: LoopKind::ForCell {
                            name,
                            cells,
                            next: next + 1,
                        },
                    });
                    Flow::Jump(CellAddr::new(head.sheet, head.row + 1, head.col))
                }
            }
        }
    }

    fn first_cell(&self, op: &Operand) -> Option<CellAddr> {
        match op {
            Operand::Ref(r) => r
                .areas()
                .first()
                .map(|a| CellAddr::new(a.sheet, a.rect.r0, a.rect.c0)),
            _ => None,
        }
    }

    fn target_of(&mut self, op: Operand, host: CellAddr) -> Option<CellAddr> {
        match op {
            Operand::Ref(_) => self.first_cell(&op),
            op => match self.deref(op, host) {
                Value::Text(t) => {
                    let key = t.to_uppercase();
                    self.macro_target(&key, host.sheet).or_else(|| {
                        let wb = self.env.wb();
                        crate::parse_cell_ref(wb, &t, host.sheet)
                    })
                }
                _ => None,
            },
        }
    }

    fn run_target(
        &mut self,
        op: Operand,
        host: CellAddr,
        args: Vec<Operand>,
    ) -> Result<Value, XlmError> {
        match self.target_of(op, host) {
            Some(a) => self.call(a, args),
            None => Ok(Value::Error(XlError::REF)),
        }
    }

    fn expr(&mut self, fr: &mut Frame, node: &Node, host: CellAddr) -> Result<Operand, XlmError> {
        self.step()?;
        let node = strip(node);
        if matches!(node, Node::Missing) {
            return Ok(Operand::Missing);
        }
        if !self.needs(node, host.sheet) {
            return Ok(self.eval_node(node, host));
        }
        match node {
            Node::Func { id, args } if functions::def(*id).name == "IF" && args.len() >= 2 => {
                let c = self.expr(fr, &args[0], host)?;
                let pick = match self.truthy(c, host) {
                    Ok(true) => Some(&args[1]),
                    Ok(false) => args.get(2),
                    Err(e) => return Ok(Operand::Val(e)),
                };
                match pick {
                    Some(n) => self.expr(fr, n, host),
                    None => Ok(Operand::Val(Value::Bool(false))),
                }
            }
            Node::Call { callee, args } if matches!(strip(callee), Node::Ref(..)) => {
                let target = self.eval_node(callee, host);
                let mut ops = Vec::with_capacity(args.len());
                for a in args {
                    ops.push(self.expr(fr, a, host)?);
                }
                match self.first_cell(&target) {
                    Some(a) => {
                        let v = self.call(a, ops)?;
                        Ok(Operand::Val(v))
                    }
                    None => Ok(Operand::Val(Value::Error(XlError::REF))),
                }
            }
            Node::NamedCall { key, args, .. } => {
                let key = key.to_string();
                let mut ops = Vec::with_capacity(args.len());
                if !matches!(key.as_str(), "GOTO" | "RETURN" | "HALT") {
                    for a in args {
                        ops.push(self.expr(fr, a, host)?);
                    }
                }
                if key == "RUN" {
                    let target = ops.first().cloned().unwrap_or(Operand::Missing);
                    let rest = ops.iter().skip(1).cloned().collect();
                    let v = self.run_target(target, host, rest)?;
                    return Ok(Operand::Val(v));
                }
                if let Some(t) = self.macro_target(&key, host.sheet) {
                    if !is_cmd(&key) {
                        let v = self.call(t, ops)?;
                        return Ok(Operand::Val(v));
                    }
                }
                if let Some(flow) = self.control_inline(fr, &key, args, host)? {
                    fr.flow = Some(flow);
                    return Ok(Operand::Val(Value::Bool(true)));
                }
                let v = self.env.command(&key, &ops, host)?;
                Ok(Operand::Val(v))
            }
            other => {
                let rebuilt = self.rebuild(fr, other, host)?;
                Ok(self.eval_node(&rebuilt, host))
            }
        }
    }

    fn control_inline(
        &mut self,
        fr: &mut Frame,
        name: &str,
        args: &[Node],
        host: CellAddr,
    ) -> Result<Option<Flow>, XlmError> {
        match name {
            "GOTO" | "RETURN" | "HALT" | "BREAK" | "SET.NAME" | "RUN" | "NEXT" => {
                self.control(fr, name, args, host)
            }
            _ => Ok(None),
        }
    }

    fn rebuild(&mut self, fr: &mut Frame, node: &Node, host: CellAddr) -> Result<Node, XlmError> {
        let sub = |this: &mut Self, fr: &mut Frame, n: &Node| -> Result<Node, XlmError> {
            if this.needs(n, host.sheet) {
                let op = this.expr(fr, n, host)?;
                let v = this.deref(op, host);
                Ok(Node::Lit(v))
            } else {
                Ok(n.clone())
            }
        };

        let early = |this: &mut Self, n: &Node| -> Node {
            if matches!(
                strip(n),
                Node::Name { .. } | Node::Ref(..) | Node::Lit(_) | Node::Local { .. }
            ) {
                return n.clone();
            }
            match this.eval_node(n, host) {
                Operand::Ref(_) | Operand::Missing => n.clone(),
                op => Node::Lit(this.deref(op, host)),
            }
        };
        Ok(match node {
            Node::Func { id, args } => {
                let last = args.iter().rposition(|a| self.needs(a, host.sheet));
                let mut out = Vec::with_capacity(args.len());
                for (i, a) in args.iter().enumerate() {
                    if last.is_some_and(|l| i < l) && !self.needs(a, host.sheet) {
                        let lazy = matches!(functions::def(*id).imp, functions::Imp::Lazy(_));
                        out.push(if lazy { a.clone() } else { early(self, a) });
                    } else {
                        out.push(sub(self, fr, a)?);
                    }
                }
                Node::Func { id: *id, args: out }
            }
            Node::Binary(op, a, b) => {
                let a = if self.needs(b, host.sheet) && !self.needs(a, host.sheet) {
                    early(self, a)
                } else {
                    sub(self, fr, a)?
                };
                let b = sub(self, fr, b)?;
                Node::Binary(*op, Box::new(a), Box::new(b))
            }
            Node::Unary(op, a) => Node::Unary(*op, Box::new(sub(self, fr, a)?)),
            Node::Paren(a) => Node::Paren(Box::new(sub(self, fr, a)?)),
            n => n.clone(),
        })
    }
}

fn cell_operand(a: CellAddr) -> Operand {
    Operand::Ref(Reference::Area(Area {
        sheet: a.sheet,
        rect: crate::address::Rect::new(a.row, a.col, a.row, a.col),
    }))
}

pub fn call_from_cell(ctx: &mut EvalCtx, target: CellAddr, args: Vec<Operand>) -> Operand {
    let mut env = ReadOnlyEnv::new(ctx.wb);
    match run(&mut env, target, args) {
        Ok(Value::Blank) => Operand::Val(Value::Number(0.0)),

        Ok(Value::Array(a)) => Operand::Val(a.data.first().cloned().unwrap_or(Value::Blank)),
        Ok(v) => Operand::Val(v),
        Err(_) => Operand::err(XlError::VALUE),
    }
}

impl Workbook {
    pub fn xlm_store(&mut self, addr: CellAddr, v: Value) {
        if let Some(fid) = self.formula_at(addr) {
            self.formula_mut(fid).value = v;
        }
    }

    pub fn xlm_write_value(&mut self, addr: CellAddr, v: Value) -> bool {
        let block = self.array_at(addr).map(|b| b.0);
        let Some(fid) = block.or_else(|| self.formula_at(addr)) else {
            self.set_value(addr, v);
            return true;
        };
        let v = if v.is_blank() {
            Value::Error(XlError::NA)
        } else {
            v
        };
        let f = self.formula_mut(fid);
        match (f.spill, &mut f.value) {
            (Some(rect), Value::Array(a)) if rect.contains(addr.row, addr.col) => {
                let i = (addr.row - rect.r0) * a.cols + (addr.col - rect.c0);
                std::sync::Arc::make_mut(a).data[i as usize] = v;
            }
            _ => f.value = v,
        }
        false
    }

    pub fn xlm_has_cell(&self, addr: CellAddr) -> bool {
        self.sheet(addr.sheet).is_some_and(|s| {
            matches!(
                s.cells.get(addr.row, addr.col),
                Some(Entry::Const(_) | Entry::Formula(_))
            )
        })
    }
}

pub fn eval_expression(
    env: &mut dyn XlmEnv,
    node: &Node,
    host: CellAddr,
) -> Result<Value, XlmError> {
    let mut it = Interp {
        env,
        vars: Arc::new(XlmVars::default()),
        names: XlmNames::default(),
        steps: 0,
        depth: 0,
        halted: false,
        whole_array: false,
    };
    let mut fr = Frame {
        pc: host,
        loops: Vec::new(),
        args: Vec::new(),
        arg_n: 0,
        flow: None,
        result: None,
    };
    let op = it.expr(&mut fr, node, host)?;
    let v = match fr.flow.take() {
        Some(Flow::Return(v)) => v,
        _ => it.deref(op, host),
    };
    let vars = std::mem::take(&mut it.vars);
    let names = std::mem::take(&mut it.names);
    it.env.persist(&vars, &names);
    Ok(v)
}

pub fn eval_text(
    env: &mut dyn XlmEnv,
    text: &str,
    host: CellAddr,
) -> Result<Result<Value, XlmError>, String> {
    let f = env
        .wb()
        .compile_xlm(text, host)
        .map_err(|e| e.to_string())?;

    if let Some(h) = env.wb().function_host() {
        let mut blocked = false;
        crate::formula::walk(&f.root, &mut |n| {
            if let Node::NamedCall { key, .. } = n {
                blocked |= h.signature(key).is_some() && !h.macro_callable(key);
            }
        });
        if blocked {
            return Ok(Ok(Value::Bool(false)));
        }
    }
    Ok(eval_expression(env, &f.root, host))
}

pub fn name_target(wb: &Workbook, key: &str, sheet: SheetId) -> Option<CellAddr> {
    let def = wb.names.lookup(key, sheet)?;
    let mut ctx = EvalCtx::new(wb, CellAddr::new(sheet, 0, 0));
    let op = crate::eval::refs::eval_name(&mut ctx, &def.name.to_uppercase(), None);
    let Operand::Ref(Reference::Area(a)) = op else {
        return None;
    };
    if a.rect.rows() != 1 || a.rect.cols() != 1 {
        return None;
    }
    wb.sheet(a.sheet)
        .is_some_and(|s| s.kind.is_macro())
        .then(|| CellAddr::new(a.sheet, a.rect.r0, a.rect.c0))
}

pub fn r1c1_rect(text: &str, row: u32, col: u32) -> Option<crate::address::Rect> {
    functions::lookup::parse_r1c1(text, row, col)
}
