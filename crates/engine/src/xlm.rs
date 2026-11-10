use std::sync::Arc;

use rustc_hash::FxHashMap;

use crate::address::{CellAddr, SheetId};
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

    fn persist(&mut self, _vars: &XlmVars, _names: &XlmNames) {}

    fn overlay(&self) -> Option<Arc<XlmCells>> {
        None
    }
}

thread_local! {

    static PENDING: std::cell::RefCell<Vec<(CellAddr, Value)>> = const { std::cell::RefCell::new(Vec::new()) };
}

impl Workbook {
    pub(crate) fn xlm_apply_pending(&mut self) {
        let writes = PENDING.with(|p| std::mem::take(&mut *p.borrow_mut()));
        for (a, v) in writes {
            if format!("{:?}", self.value(a)) != format!("{:?}", v) {
                self.set_value(a, v);
            }
        }
    }
}

pub type XlmCells = FxHashMap<(SheetId, u32, u32), Value>;

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
    fn command(&mut self, name: &str, args: &[Operand], _at: CellAddr) -> Result<Value, XlmError> {
        if name == "SET.VALUE" {
            if let (Some(Operand::Ref(r)), Some(v)) = (args.first(), args.get(1)) {
                let v = match v {
                    Operand::Val(v) => v.clone(),
                    Operand::Ref(r) => r.areas().first().map_or(Value::Blank, |a| {
                        self.wb.value(CellAddr::new(a.sheet, a.rect.r0, a.rect.c0))
                    }),
                    Operand::Missing => Value::Blank,
                };
                let cells = Arc::make_mut(&mut self.cells);
                let mut done = false;
                let mut pending = Vec::new();
                for a in r.areas() {
                    if self.wb.sheet(a.sheet).is_some_and(|s| s.kind.is_macro()) {
                        for row in a.rect.r0..=a.rect.r1 {
                            for col in a.rect.c0..=a.rect.c1 {
                                cells.insert((a.sheet, row, col), v.clone());
                                pending.push((CellAddr::new(a.sheet, row, col), v.clone()));
                                done = true;
                            }
                        }
                    }
                }
                PENDING.with(|p| p.borrow_mut().extend(pending));
                return Ok(Value::Bool(done));
            }
        }
        Ok(Value::Bool(false))
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
}

pub fn run(env: &mut dyn XlmEnv, start: CellAddr, args: Vec<Operand>) -> Result<Value, XlmError> {
    let mut it = Interp {
        env,
        vars: Arc::new(XlmVars::default()),
        names: XlmNames::default(),
        steps: 0,
        depth: 0,
        halted: false,
    };
    let r = it.call(start, args);
    let vars = std::mem::take(&mut it.vars);
    let names = std::mem::take(&mut it.names);
    it.env.persist(&vars, &names);
    r
}

fn strip(node: &Node) -> &Node {
    match node.unwrap_ws() {
        Node::Unary(
            crate::parser::ast::UnaryOp::Implicit | crate::parser::ast::UnaryOp::ImplicitQuiet,
            x,
        ) => strip(x),
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
        let Some(ty) = ty else { return v };
        if matches!(v, Value::Error(_)) && ty & 16 != 0 {
            return v;
        }
        let ctx = EvalCtx::new(self.env.wb(), host);
        match (ty, &v) {
            (2, Value::Number(_) | Value::Bool(_)) => {
                crate::coerce::to_text(&v).map_or(v, Value::Text)
            }
            (1, Value::Text(_) | Value::Bool(_)) => match ctx.to_number(&v) {
                Ok(n) => Value::Number(n),
                Err(e) => Value::Error(e),
            },
            (4, Value::Number(n)) => Value::Bool(*n != 0.0),
            (4, Value::Text(t)) if t.eq_ignore_ascii_case("TRUE") => Value::Bool(true),
            (4, Value::Text(t)) if t.eq_ignore_ascii_case("FALSE") => Value::Bool(false),
            (4, Value::Text(_)) => Value::Error(XlError::VALUE),
            _ => v,
        }
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
        crate::formula::walk(node, &mut |n| {
            if let Node::NamedCall { key, .. } = n {
                if is_cmd(key) || self.macro_target(key, sheet).is_some() {
                    found = true;
                }
            }
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
                return Err(XlmError::NoReturn);
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

                    Some(Operand::Val(_)) if ty == Some(8) => {
                        Operand::Val(Value::Error(XlError::VALUE))
                    }

                    Some(Operand::Ref(r))
                        if ty.is_none_or(|t| t & (8 | 64) == 0)
                            && r.areas().iter().map(|a| a.rect.cell_count()).sum::<u64>() > 1 =>
                    {
                        Operand::Val(Value::Error(XlError::VALUE))
                    }
                    Some(o) => o,
                };
                if let Value::Text(t) = n {
                    if let Some(r) = args.get(2) {
                        let target = self.expr(fr, r, host)?;
                        if let Some(a) = self.first_cell(&target) {
                            let v = self.deref(val.clone(), host);
                            self.env.store(a, v);
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
                    let op = self.expr(fr, vn, host)?;
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
                    if ok {
                        return Flow::Jump(CellAddr::new(a.sheet, a.row + 1, a.col));
                    }
                    at = a;
                }
                Some((a, _)) => return Flow::Jump(CellAddr::new(a.sheet, a.row + 1, a.col)),
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
        Ok(match node {
            Node::Func { id, args } => {
                let mut out = Vec::with_capacity(args.len());
                for a in args {
                    out.push(sub(self, fr, a)?);
                }
                Node::Func { id: *id, args: out }
            }
            Node::Binary(op, a, b) => {
                let a = sub(self, fr, a)?;
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

    pub fn xlm_get_formula(&self, addr: CellAddr) -> String {
        if let Some(f) = self.formula_text_r1c1(addr) {
            return f;
        }
        match self.value(addr) {
            Value::Blank => String::new(),
            v => crate::coerce::to_text(&v)
                .map(|t| t.to_string())
                .unwrap_or_default(),
        }
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
