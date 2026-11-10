use std::collections::{BTreeMap, HashMap, HashSet};
use std::path::PathBuf;

type Cell = (u32, u32);

fn name(c: Cell) -> String {
    let mut s = String::new();
    let mut n = c.1 + 1;
    while n > 0 {
        let m = (n - 1) % 26;
        s.insert(0, char::from(b'A' + m as u8));
        n = (n - 1) / 26;
    }
    format!("{s}{}", c.0 + 1)
}

fn parse_cell(s: &str) -> Option<Cell> {
    let i = s.find(|ch: char| !ch.is_ascii_uppercase())?;
    if i == 0 {
        return None;
    }
    let col = s[..i]
        .bytes()
        .fold(0u32, |a, b| a * 26 + u32::from(b - b'A' + 1));
    let row: u32 = s[i..].parse().ok()?;
    Some((row - 1, col - 1))
}

#[derive(Clone, Copy, PartialEq, Eq, Debug)]
struct Rect {
    r1: u32,
    c1: u32,
    r2: u32,
    c2: u32,
}

impl Rect {
    fn rows(&self) -> u32 {
        self.r2 - self.r1 + 1
    }
    fn cols(&self) -> u32 {
        self.c2 - self.c1 + 1
    }
    fn contains(&self, c: Cell) -> bool {
        (self.r1..=self.r2).contains(&c.0) && (self.c1..=self.c2).contains(&c.1)
    }
    fn within(&self, o: &Rect) -> bool {
        o.contains((self.r1, self.c1)) && o.contains((self.r2, self.c2))
    }
    fn meets(&self, o: &Rect) -> bool {
        self.r1 <= o.r2 && o.r1 <= self.r2 && self.c1 <= o.c2 && o.c1 <= self.c2
    }
    fn cells(&self) -> Vec<Cell> {
        (self.r1..=self.r2)
            .flat_map(|r| (self.c1..=self.c2).map(move |c| (r, c)))
            .collect()
    }
}

fn parse_rect(s: &str) -> Rect {
    let (a, b) = s.split_once(':').unwrap_or((s, s));
    let (a, b) = (parse_cell(a).unwrap(), parse_cell(b).unwrap());
    Rect {
        r1: a.0,
        c1: a.1,
        r2: b.0,
        c2: b.1,
    }
}

#[derive(Clone, Debug)]
enum Expr {
    Num(f64),
    Ref(Cell),
    Range(Rect),
    Neg(Box<Expr>),
    Bin(u8, Box<Expr>, Box<Expr>),
    Call(String, Vec<Expr>),
}

struct Parser<'a> {
    s: &'a [u8],
    i: usize,
}

impl Parser<'_> {
    fn peek(&self) -> u8 {
        self.s.get(self.i).copied().unwrap_or(0)
    }
    fn cmp(&mut self) -> Expr {
        let l = self.add();
        match self.peek() {
            op @ (b'>' | b'<' | b'=') => {
                self.i += 1;
                Expr::Bin(op, Box::new(l), Box::new(self.add()))
            }
            _ => l,
        }
    }
    fn add(&mut self) -> Expr {
        let mut l = self.mul();
        while let op @ (b'+' | b'-') = self.peek() {
            self.i += 1;
            l = Expr::Bin(op, Box::new(l), Box::new(self.mul()));
        }
        l
    }
    fn mul(&mut self) -> Expr {
        let mut l = self.unary();
        while let op @ (b'*' | b'/') = self.peek() {
            self.i += 1;
            l = Expr::Bin(op, Box::new(l), Box::new(self.unary()));
        }
        l
    }
    fn unary(&mut self) -> Expr {
        if self.peek() == b'-' {
            self.i += 1;
            return Expr::Neg(Box::new(self.unary()));
        }
        self.primary()
    }
    fn word(&mut self, f: fn(u8) -> bool) -> String {
        let j = self.i;
        while f(self.peek()) {
            self.i += 1;
        }
        String::from_utf8(self.s[j..self.i].to_vec()).unwrap()
    }
    fn primary(&mut self) -> Expr {
        let ch = self.peek();
        if ch.is_ascii_digit() || ch == b'.' {
            let w = self.word(|b| b.is_ascii_digit() || b == b'.');
            return Expr::Num(w.parse().unwrap());
        }
        if ch == b'(' {
            self.i += 1;
            let e = self.cmp();
            self.i += 1;
            return e;
        }
        let w = self.word(|b| b.is_ascii_uppercase());
        if self.peek() == b'(' {
            self.i += 1;
            let mut args = Vec::new();
            while self.peek() != b')' {
                args.push(self.cmp());
                if self.peek() == b',' {
                    self.i += 1;
                }
            }
            self.i += 1;
            return Expr::Call(w, args);
        }
        let a = format!("{w}{}", self.word(|b| b.is_ascii_digit()));
        if self.peek() == b':' {
            self.i += 1;
            let b = self.word(|b| b.is_ascii_alphanumeric());
            return Expr::Range(parse_rect(&format!("{a}:{b}")));
        }
        Expr::Ref(parse_cell(&a).unwrap())
    }
}

fn parse_formula(text: &str) -> Expr {
    let mut p = Parser {
        s: text.trim_start_matches('=').as_bytes(),
        i: 0,
    };
    p.cmp()
}

fn shifted(e: &Expr, dr: u32, dc: u32) -> Expr {
    let sh = |c: Cell| (c.0 + dr, c.1 + dc);
    match e {
        Expr::Num(v) => Expr::Num(*v),
        Expr::Ref(c) => Expr::Ref(sh(*c)),
        Expr::Range(r) => Expr::Range(Rect {
            r1: r.r1 + dr,
            c1: r.c1 + dc,
            r2: r.r2 + dr,
            c2: r.c2 + dc,
        }),
        Expr::Neg(a) => Expr::Neg(Box::new(shifted(a, dr, dc))),
        Expr::Bin(op, a, b) => Expr::Bin(
            *op,
            Box::new(shifted(a, dr, dc)),
            Box::new(shifted(b, dr, dc)),
        ),
        Expr::Call(f, args) => {
            Expr::Call(f.clone(), args.iter().map(|a| shifted(a, dr, dc)).collect())
        }
    }
}

fn refs(e: &Expr, ii: Option<Cell>, out: &mut Vec<Rect>) {
    let one = |c: Cell| Rect {
        r1: c.0,
        c1: c.1,
        r2: c.0,
        c2: c.1,
    };
    match e {
        Expr::Num(_) => {}
        Expr::Ref(c) => out.push(one(*c)),
        Expr::Range(r) => match ii {
            None => out.push(*r),
            Some(_) if r.rows() == 1 && r.cols() == 1 => out.push(*r),
            Some(at) if r.cols() == 1 && (r.r1..=r.r2).contains(&at.0) => {
                out.push(one((at.0, r.c1)))
            }
            Some(at) if r.rows() == 1 && (r.c1..=r.c2).contains(&at.1) => {
                out.push(one((r.r1, at.1)))
            }
            Some(_) => {}
        },
        Expr::Neg(a) => refs(a, ii, out),
        Expr::Bin(_, a, b) => {
            refs(a, ii, out);
            refs(b, ii, out);
        }
        Expr::Call(f, args) => {
            let inner = if matches!(f.as_str(), "COUNT" | "SUM" | "INDEX") {
                None
            } else {
                ii
            };
            args.iter().for_each(|a| refs(a, inner, out));
        }
    }
}

fn shape(e: &Expr) -> (u32, u32) {
    let mx = |a: (u32, u32), b: (u32, u32)| (a.0.max(b.0), a.1.max(b.1));
    match e {
        Expr::Num(_) | Expr::Ref(_) => (1, 1),
        Expr::Range(r) => (r.rows(), r.cols()),
        Expr::Neg(a) => shape(a),
        Expr::Bin(_, a, b) => mx(shape(a), shape(b)),
        Expr::Call(f, args) if f == "IF" || f == "IFERROR" => {
            args.iter().map(shape).fold((1, 1), mx)
        }
        Expr::Call(..) => (1, 1),
    }
}

#[derive(Clone, Copy, PartialEq, Debug)]
enum Val {
    Blank,
    Num(f64),
    Bool(bool),
    Err(&'static str),
}

fn num(v: Val) -> Result<f64, &'static str> {
    match v {
        Val::Blank => Ok(0.0),
        Val::Num(x) => Ok(x),
        Val::Bool(b) => Ok(if b { 1.0 } else { 0.0 }),
        Val::Err(e) => Err(e),
    }
}

#[derive(Clone, Copy, PartialEq, Eq, Hash, Debug, PartialOrd, Ord)]
enum Node {
    Cell(Cell),

    Arr(Cell),
}

fn label(n: Node) -> String {
    match n {
        Node::Cell(c) => name(c),
        Node::Arr(c) => format!("{{{}}}", name(c)),
    }
}

#[derive(Clone, Copy, PartialEq, Eq, Debug)]
enum St {
    Clean,

    Dirty(char),

    Failed,

    Pending,

    Deferred,

    Stored,
}

fn flag(s: St) -> char {
    match s {
        St::Clean => '0',
        St::Dirty(f) => f,
        St::Failed => 'e',
        St::Pending => '3',
        St::Deferred => '4',
        St::Stored => '5',
    }
}

#[derive(Clone, Copy, PartialEq, Debug)]
enum Kind {
    Plain,
    Anchor,
    Member(Cell),
}

#[derive(Clone, Debug)]
enum Content {
    Val(Val),
    Formula(Expr, Kind),
}

#[derive(Clone, Copy, PartialEq, Eq)]
enum Stop {
    Pull(Node),
    Fail,
}

enum Ev {
    Ok,

    Circular,
    Stop(Stop),
}

#[derive(Clone, Copy)]
enum Ctx {
    Plain(Cell),

    Elem {
        top: Cell,
        i: u32,
        j: u32,
        pull_ranges: bool,
    },
}

#[derive(Default)]
struct Model {
    cells: HashMap<Cell, Content>,
    vals: HashMap<Cell, Val>,
    blocks: HashMap<Cell, Rect>,
    areas: HashMap<Cell, Rect>,
    st: HashMap<Node, St>,
    chain: Vec<Node>,
    saved: Option<usize>,
}

#[derive(Debug, PartialEq)]
struct PassLog {
    start: usize,
    chain0: String,
    events: String,
    chain1: String,
    next: usize,
    valid: bool,
}

struct Walk {
    ev: Vec<String>,
    removed: Vec<bool>,
    reins: BTreeMap<usize, Vec<Node>>,
    u: Vec<(Node, St)>,
    in_u: HashSet<Node>,
    slot: HashMap<Node, usize>,
    progress: bool,
    circular: bool,
}

impl Model {
    fn st(&self, n: Node) -> St {
        self.st.get(&n).copied().unwrap_or(St::Clean)
    }
    fn set(&mut self, n: Node, s: St) {
        self.st.insert(n, s);
    }
    fn node_at(&self, c: Cell) -> Option<Node> {
        match self.cells.get(&c)? {
            Content::Formula(_, Kind::Anchor) => Some(Node::Arr(c)),
            Content::Formula(..) => Some(Node::Cell(c)),
            Content::Val(_) => None,
        }
    }
    fn top_of(&self, n: Node) -> Option<Cell> {
        match (
            n,
            self.cells.get(&match n {
                Node::Cell(c) | Node::Arr(c) => c,
            }),
        ) {
            (Node::Cell(_), Some(Content::Formula(_, Kind::Member(t)))) => Some(*t),
            _ => None,
        }
    }
    fn chain_str(&self) -> String {
        let v: Vec<String> = self
            .chain
            .iter()
            .map(|&n| format!("{}:{}", label(n), flag(self.st(n))))
            .collect();
        v.join(" ")
    }
    fn uncalc(s: St) -> bool {
        !matches!(s, St::Clean)
    }
    fn any_uncalc(&self) -> bool {
        self.chain.iter().any(|&n| Self::uncalc(self.st(n)))
    }

    fn resolve(&self, c: Cell) -> Result<Val, Node> {
        for (&a, r) in &self.areas {
            if a != c && r.contains(c) {
                let n = Node::Arr(a);
                return match self.st(n) {
                    St::Clean => Ok(self.vals.get(&c).copied().unwrap_or(Val::Blank)),
                    _ => Err(n),
                };
            }
        }
        match self.cells.get(&c) {
            None => Ok(Val::Blank),
            Some(Content::Val(v)) => Ok(*v),
            Some(Content::Formula(_, kind)) => {
                let n = self.node_at(c).unwrap();
                match (self.st(n), kind) {
                    (St::Clean, _) => Ok(self.vals.get(&c).copied().unwrap_or(Val::Blank)),
                    (St::Deferred, Kind::Member(t)) => Err(Node::Arr(*t)),
                    _ => Err(n),
                }
            }
        }
    }

    fn read(&self, c: Cell, pull: bool, reads: &mut Vec<Cell>) -> Result<Val, Stop> {
        reads.push(c);
        self.resolve(c).map_err(|n| {
            if pull && matches!(self.st(n), St::Dirty(_) | St::Stored) {
                Stop::Pull(n)
            } else {
                Stop::Fail
            }
        })
    }

    fn eval(&self, e: &Expr, ctx: Ctx, reads: &mut Vec<Cell>) -> Result<Val, Stop> {
        let at = match ctx {
            Ctx::Plain(c) => c,
            Ctx::Elem { top, i, j, .. } => (top.0 + i, top.1 + j),
        };
        Ok(match e {
            Expr::Num(v) => Val::Num(*v),
            Expr::Ref(c) => self.read(*c, true, reads)?,
            Expr::Range(r) => match ctx {
                Ctx::Plain(_) => {
                    let cell = if r.rows() == 1 && r.cols() == 1 {
                        Some((r.r1, r.c1))
                    } else if r.cols() == 1 && (r.r1..=r.r2).contains(&at.0) {
                        Some((at.0, r.c1))
                    } else if r.rows() == 1 && (r.c1..=r.c2).contains(&at.1) {
                        Some((r.r1, at.1))
                    } else {
                        None
                    };
                    match cell {
                        Some(c) => self.read(c, true, reads)?,
                        None => Val::Err("#VALUE!"),
                    }
                }
                Ctx::Elem {
                    i, j, pull_ranges, ..
                } => {
                    let ri = if r.rows() == 1 {
                        Some(0)
                    } else {
                        (i < r.rows()).then_some(i)
                    };
                    let cj = if r.cols() == 1 {
                        Some(0)
                    } else {
                        (j < r.cols()).then_some(j)
                    };
                    match (ri, cj) {
                        (Some(a), Some(b)) => {
                            self.read((r.r1 + a, r.c1 + b), pull_ranges, reads)?
                        }
                        _ => Val::Err("#N/A"),
                    }
                }
            },
            Expr::Neg(a) => match num(self.eval(a, ctx, reads)?) {
                Ok(x) => Val::Num(-x),
                Err(e) => Val::Err(e),
            },
            Expr::Bin(op, a, b) => {
                let (x, y) = (self.eval(a, ctx, reads)?, self.eval(b, ctx, reads)?);
                match (num(x), num(y)) {
                    (Err(e), _) | (_, Err(e)) => Val::Err(e),
                    (Ok(x), Ok(y)) => match op {
                        b'+' => Val::Num(x + y),
                        b'-' => Val::Num(x - y),
                        b'*' => Val::Num(x * y),
                        b'/' if y == 0.0 => Val::Err("#DIV/0!"),
                        b'/' => Val::Num(x / y),
                        b'>' => Val::Bool(x > y),
                        b'<' => Val::Bool(x < y),
                        _ => Val::Bool(x == y),
                    },
                }
            }
            Expr::Call(f, args) => match f.as_str() {
                "COUNT" | "SUM" => {
                    let mut vs = Vec::new();
                    for a in args {
                        match a {
                            Expr::Range(r) => {
                                for c in r.cells() {
                                    vs.push(self.read(c, true, reads)?);
                                }
                            }
                            _ => vs.push(self.eval(a, ctx, reads)?),
                        }
                    }
                    if f == "COUNT" {
                        Val::Num(vs.iter().filter(|v| matches!(v, Val::Num(_))).count() as f64)
                    } else if let Some(Val::Err(e)) = vs.iter().find(|v| matches!(v, Val::Err(_))) {
                        Val::Err(e)
                    } else {
                        Val::Num(vs.iter().filter_map(|v| num(*v).ok()).sum())
                    }
                }
                "IFERROR" => match self.eval(&args[0], ctx, reads)? {
                    Val::Err(_) => self.eval(&args[1], ctx, reads)?,
                    v => v,
                },
                "IF" => match num(self.eval(&args[0], ctx, reads)?) {
                    Err(e) => Val::Err(e),
                    Ok(x) if x != 0.0 => self.eval(&args[1], ctx, reads)?,
                    Ok(_) => match args.get(2) {
                        Some(a) => self.eval(a, ctx, reads)?,
                        None => Val::Bool(false),
                    },
                },
                "INDEX" => {
                    let Expr::Range(r) = &args[0] else {
                        unreachable!()
                    };
                    let k = num(self.eval(&args[1], ctx, reads)?).unwrap_or(1.0) as u32 - 1;
                    let c = if r.rows() == 1 {
                        (r.r1, r.c1 + k)
                    } else {
                        (r.r1 + k, r.c1)
                    };
                    self.read(c, true, reads)?
                }
                "COLUMN" => Val::Num(f64::from(at.1 + 1)),
                _ => panic!("function {f}"),
            },
        })
    }

    fn expr_at(&self, c: Cell) -> Expr {
        match &self.cells[&c] {
            Content::Formula(e, _) => e.clone(),
            Content::Val(_) => unreachable!(),
        }
    }

    fn eval_node(&mut self, n: Node) -> Ev {
        let mut reads = Vec::new();
        match n {
            Node::Cell(c) => {
                let r = match self.top_of(n) {
                    Some(top) => self.eval(
                        &self.expr_at(c),
                        Ctx::Elem {
                            top,
                            i: c.0 - top.0,
                            j: c.1 - top.1,
                            pull_ranges: true,
                        },
                        &mut reads,
                    ),
                    None => self.eval(&self.expr_at(c), Ctx::Plain(c), &mut reads),
                };
                match r {
                    Ok(v) => {
                        self.vals.insert(c, v);
                        Ev::Ok
                    }
                    Err(s) => Ev::Stop(s),
                }
            }
            Node::Arr(top) if self.blocks.contains_key(&top) => self.calc_block(top),
            Node::Arr(a) => self.calc_anchor(a),
        }
    }

    fn calc_block(&mut self, top: Cell) -> Ev {
        let rect = self.blocks[&top];
        let expr = self.expr_at(top);
        let mut out = Vec::new();
        for c in rect.cells() {
            let ctx = Ctx::Elem {
                top,
                i: c.0 - top.0,
                j: c.1 - top.1,
                pull_ranges: false,
            };
            match self.eval(&expr, ctx, &mut Vec::new()) {
                Ok(v) => out.push((c, Some(v))),
                Err(Stop::Fail) => out.push((c, None)),
                Err(s) => return Ev::Stop(s),
            }
        }
        if out.iter().all(|(_, v)| v.is_none()) {
            return Ev::Stop(Stop::Fail);
        }
        for (c, v) in out {
            match v {
                Some(v) => {
                    self.vals.insert(c, v);
                    self.set(Node::Cell(c), St::Stored);
                }
                None => self.set(Node::Cell(c), St::Dirty('f')),
            }
        }
        Ev::Ok
    }

    fn calc_anchor(&mut self, a: Cell) -> Ev {
        let expr = self.expr_at(a);
        let (rows, cols) = shape(&expr);
        let mut reads = Vec::new();
        let mut vals = Vec::new();
        for i in 0..rows {
            for j in 0..cols {
                let ctx = Ctx::Elem {
                    top: a,
                    i,
                    j,
                    pull_ranges: true,
                };
                match self.eval(&expr, ctx, &mut reads) {
                    Ok(v) => vals.push(((a.0 + i, a.1 + j), v)),

                    Err(Stop::Pull(n)) if n == Node::Arr(a) => return Ev::Stop(Stop::Fail),
                    Err(s) => return Ev::Stop(s),
                }
            }
        }
        let rect = Rect {
            r1: a.0,
            c1: a.1,
            r2: a.0 + rows - 1,
            c2: a.1 + cols - 1,
        };
        let blocked = rect.cells().into_iter().any(|c| {
            c != a
                && (self.cells.contains_key(&c)
                    || self.areas.iter().any(|(&b, r)| b != a && r.contains(c)))
        });
        let new = (!blocked).then_some(rect);
        let old = self.areas.get(&a).copied();
        if blocked {
            self.vals.insert(a, Val::Err("#SPILL!"));
        } else {
            self.vals.extend(vals);
        }
        let n = Node::Arr(a);
        if new == old {
            self.set(n, St::Clean);
            return Ev::Ok;
        }
        match new {
            Some(r) => self.areas.insert(a, r),
            None => self.areas.remove(&a),
        };
        self.set(n, St::Clean);
        let mut from: Vec<Cell> = old.into_iter().chain(new).flat_map(|r| r.cells()).collect();
        from.retain(|&c| c != a);
        self.dirty_from(&from, St::Failed);

        let circular =
            self.st(n) == St::Failed || new.is_some_and(|r| reads.iter().any(|&c| r.contains(c)));
        if circular {
            self.set(n, St::Failed);
            Ev::Circular
        } else {
            Ev::Ok
        }
    }

    fn dirty_from(&mut self, from: &[Cell], to: St) {
        let mut seen: HashSet<Cell> = HashSet::new();
        let mut work: Vec<Cell> = from.to_vec();
        let mut done = HashSet::new();
        while let Some(x) = work.pop() {
            if !seen.insert(x) {
                continue;
            }
            let readers: Vec<Cell> = self
                .cells
                .iter()
                .filter_map(|(&c, ct)| match ct {
                    Content::Formula(e, kind) => {
                        let mut rs = Vec::new();
                        refs(e, (*kind == Kind::Plain).then_some(c), &mut rs);
                        rs.iter().any(|r| r.contains(x)).then_some(c)
                    }
                    Content::Val(_) => None,
                })
                .collect();
            for c in readers {
                let n = self.node_at(c).unwrap();
                if !done.insert(n) {
                    continue;
                }

                match (self.st(n), to) {
                    (St::Clean | St::Stored, _) => self.set(n, to),
                    (St::Deferred | St::Pending, _) => {}
                    (_, St::Failed) => self.set(n, to),
                    _ => continue,
                }
                work.push(c);
                if let Some(r) = self.areas.get(&c) {
                    work.extend(r.cells());
                }
            }
        }
    }

    fn remove_node(&mut self, n: Node) {
        self.chain.retain(|&m| m != n);
        self.st.remove(&n);
    }

    fn take(&mut self, c: Cell, edited: &mut Vec<Cell>) {
        if let Some(Content::Formula(_, Kind::Member(top))) = self.cells.get(&c).cloned() {
            let rect = self.blocks.remove(&top).unwrap();
            self.remove_node(Node::Arr(top));
            for m in rect.cells() {
                self.remove_node(Node::Cell(m));
                self.cells.remove(&m);
                edited.push(m);
            }
            return;
        }
        if let Some(n) = self.node_at(c) {
            self.remove_node(n);
            if let Some(r) = self.areas.remove(&c) {
                edited.extend(r.cells());
            }
        }
        self.cells.remove(&c);

        let hit: Vec<Cell> = self
            .areas
            .iter()
            .filter(|(&a, r)| a != c && r.contains(c))
            .map(|(&a, _)| a)
            .collect();
        for a in hit {
            let r = self.areas.remove(&a).unwrap();
            edited.extend(r.cells());
            self.set(Node::Arr(a), St::Dirty('f'));
        }
        edited.push(c);
    }

    fn apply(&mut self, line: &str) -> bool {
        let mut p = line.splitn(3, ' ');
        let (verb, target, text) = (
            p.next().unwrap(),
            p.next().unwrap_or(""),
            p.next().unwrap_or(""),
        );
        if verb == "new" {
            *self = Model::default();
            return false;
        }
        let rect = parse_rect(target);
        let refused = self
            .blocks
            .values()
            .any(|b| b.meets(&rect) && !b.within(&rect));
        if refused {
            return false;
        }
        let mut edited = Vec::new();
        let mut front = Vec::new();
        for c in rect.cells() {
            self.take(c, &mut edited);
        }
        let formula = text.starts_with('=');
        match verb {
            "array" => {
                let e = parse_formula(text);
                let top = (rect.r1, rect.c1);
                self.blocks.insert(top, rect);
                front.push(Node::Arr(top));
                self.set(Node::Arr(top), St::Dirty('f'));
                for c in rect.cells() {
                    self.cells
                        .insert(c, Content::Formula(e.clone(), Kind::Member(top)));
                    front.push(Node::Cell(c));
                    self.set(Node::Cell(c), St::Deferred);
                }
            }
            "set" | "set2" if formula => {
                let e = parse_formula(text);
                for c in rect.cells() {
                    let e = shifted(&e, c.0 - rect.r1, c.1 - rect.c1);
                    let (rows, cols) = shape(&e);
                    let kind = if verb == "set2" && rows * cols > 1 {
                        Kind::Anchor
                    } else {
                        Kind::Plain
                    };
                    self.cells.insert(c, Content::Formula(e, kind));
                    let n = self.node_at(c).unwrap();
                    front.push(n);
                    self.set(n, St::Dirty('f'));
                }
            }
            "set" | "set2" | "const" => {
                for c in rect.cells() {
                    self.cells
                        .insert(c, Content::Val(Val::Num(text.parse().unwrap())));
                }
            }
            "clear" => {}
            _ => panic!("step {line}"),
        }
        for n in self.chain.clone() {
            if matches!(self.st(n), St::Dirty(_) | St::Failed) {
                self.set(n, St::Dirty('f'));
            }
        }
        front.append(&mut self.chain);
        self.chain = front;
        self.dirty_from(&edited, St::Dirty('f'));
        true
    }

    fn recalc(&mut self, logs: &mut Vec<PassLog>) {
        if !self.any_uncalc() {
            return;
        }

        while self.round(0, logs) {
            for n in self.chain.clone() {
                if matches!(self.st(n), St::Dirty(_) | St::Failed) {
                    self.set(n, St::Dirty('c'));
                }
            }
            if logs.len() > 60 {
                return;
            }
        }

        if self.any_uncalc() {
            self.round(self.saved.unwrap_or(0), logs);
        }
    }

    fn round(&mut self, mut pos: usize, logs: &mut Vec<PassLog>) -> bool {
        let mut circular = false;
        loop {
            let (log, progress, circ) = self.pass(pos);
            circular |= circ;
            self.saved = log.valid.then_some(log.next);
            let again = progress && log.valid && logs.len() < 60;
            pos = log.next;
            logs.push(log);
            if !again {
                return circular;
            }
        }
    }

    fn pass(&mut self, start: usize) -> (PassLog, bool, bool) {
        let chain0 = self.chain_str();
        let len = self.chain.len();
        let mut w = Walk {
            ev: Vec::new(),
            removed: vec![false; len],
            reins: BTreeMap::new(),
            u: Vec::new(),
            in_u: HashSet::new(),
            slot: self
                .chain
                .iter()
                .enumerate()
                .map(|(i, &n)| (n, i))
                .collect(),
            progress: false,
            circular: false,
        };
        for i in start..len {
            if w.removed[i] {
                continue;
            }
            let x = self.chain[i];
            match self.st(x) {
                St::Clean | St::Pending => {}
                St::Stored => self.set(x, St::Clean),
                St::Deferred => {
                    let header = Node::Arr(self.top_of(x).unwrap());
                    if w.in_u.contains(&header) {
                        w.move_to_u(x, St::Deferred);
                    }
                }
                St::Dirty(_) | St::Failed => self.root(i, x, &mut w),
            }
        }
        let mut chain = Vec::new();
        for k in 0..len {
            if let Some(l) = w.reins.get(&k) {
                chain.extend(l);
            }
            if !w.removed[k] {
                chain.push(self.chain[k]);
            }
        }
        let next = chain.len();
        for &(n, s) in &w.u {
            chain.push(n);
            self.set(n, s);
        }
        self.chain = chain;
        let log = PassLog {
            start,
            chain0,
            events: w.ev.join(" "),
            chain1: self.chain_str(),
            next,
            valid: !w.u.is_empty(),
        };
        (log, w.progress, w.circular)
    }

    fn root(&mut self, i: usize, x: Node, w: &mut Walk) {
        let mut stack: Vec<(Node, St)> = Vec::new();
        loop {
            let top = stack.last().map_or(x, |s| s.0);
            let r = self.eval_node(top);
            let arr = matches!(top, Node::Arr(_));
            let tok = |q: &str| {
                if arr {
                    format!("{{ }}{q}")
                } else {
                    format!("{}{q}", label(top))
                }
            };
            match r {
                Ev::Ok => {
                    w.ev.push(tok(""));
                    w.progress = true;
                    self.set(top, St::Clean);
                    if stack.is_empty() {
                        return;
                    }
                    w.reinsert(i, top);
                    stack.pop();
                    w.ev.push("-S:-".into());
                    if stack.is_empty() {
                        return;
                    }
                }
                Ev::Circular => {
                    w.ev.push(tok("?0xa71b0008"));
                    w.progress = true;
                    w.circular = true;
                    if stack.is_empty() {
                        return;
                    }

                    w.reinsert(i, top);
                    stack.pop();
                    w.ev.push("-S:-".into());
                    if stack.is_empty() {
                        return;
                    }
                }
                Ev::Stop(Stop::Pull(t)) => {
                    w.ev.push(tok("?"));
                    if stack.is_empty() {
                        stack.push((x, self.st(x)));
                        w.ev.push(format!("+S:{}", label(x)));
                        self.set(x, St::Pending);
                    }
                    w.ev.push(format!("+S:{}", label(t)));
                    if self.st(t) == St::Stored {
                        self.set(t, St::Clean);
                        w.reinsert(i, t);
                        w.ev.push("-S:-".into());
                        continue;
                    }
                    stack.push((t, self.st(t)));
                    self.set(t, St::Pending);
                }
                Ev::Stop(Stop::Fail) => {
                    w.ev.push(tok("?"));
                    if stack.is_empty() {
                        match self.st(x) {
                            s @ St::Dirty(_) => {
                                w.move_to_u(x, s);
                                self.set(x, St::Failed);
                            }
                            _ => self.set(x, St::Failed),
                        }
                        return;
                    }
                    w.unwind(&mut stack, self);
                    return;
                }
            }
        }
    }
}

impl Walk {
    fn move_to_u(&mut self, n: Node, s: St) {
        self.ev.push(format!("+U:{}", label(n)));
        self.u.push((n, s));
        self.in_u.insert(n);
        if let Some(&k) = self.slot.get(&n) {
            self.removed[k] = true;
        }
    }
    fn reinsert(&mut self, root_slot: usize, n: Node) {
        self.ev.push(format!("^{}", label(n)));
        self.reins.entry(root_slot).or_default().push(n);
        if let Some(&k) = self.slot.get(&n) {
            self.removed[k] = true;
        }
    }

    fn unwind(&mut self, stack: &mut Vec<(Node, St)>, m: &mut Model) {
        while let Some((n, s)) = stack.pop() {
            let keep = if matches!(s, St::Failed) {
                St::Dirty('c')
            } else {
                s
            };
            self.move_to_u(n, keep);
            m.set(n, St::Failed);
        }
        self.ev.push("clear:S".into());
    }
}

struct RecPass {
    start: usize,
    chain0: String,
    end: Option<(usize, bool, String)>,
    events: String,
}

fn read_walk(text: &str) -> Vec<(String, Vec<RecPass>)> {
    let mut steps: Vec<(String, Vec<RecPass>)> = Vec::new();
    for l in text.lines() {
        let t = l.trim();
        if let Some(h) = t.strip_prefix("=== ") {
            steps.push((h.to_string(), Vec::new()));
        } else if let Some(s) = t.strip_prefix("@ start ") {
            let (pos, chain) = s.split_once(' ').unwrap_or((s, ""));
            let start = pos.split(',').nth(1).unwrap().parse().unwrap();
            steps.last_mut().unwrap().1.push(RecPass {
                start,
                chain0: chain.to_string(),
                end: None,
                events: String::new(),
            });
        } else if let Some(s) = t.strip_prefix("@ end ") {
            let f: Vec<&str> = s.splitn(5, ' ').collect();
            let next = f[1]
                .trim_start_matches("next=")
                .split(',')
                .nth(1)
                .unwrap()
                .parse()
                .unwrap();
            let valid = f[2] == "valid=1";
            let p = steps.last_mut().unwrap().1.last_mut().unwrap();
            p.end = Some((next, valid, f.get(4).unwrap_or(&"").to_string()));
        } else if let Some(s) = t.strip_prefix('|') {
            let toks: Vec<&str> = s
                .split_whitespace()
                .filter(|k| {
                    !k.starts_with("move:")
                        && !(k.starts_with('s')
                            && k[1..].starts_with(|c: char| c.is_ascii_uppercase()))
                })
                .collect();
            steps.last_mut().unwrap().1.last_mut().unwrap().events = toks.join(" ");
        }
    }
    steps
}

fn same_events(rec: &str, sim: &str) -> bool {
    let (a, b): (Vec<&str>, Vec<&str>) = (
        rec.split_whitespace().collect(),
        sim.split_whitespace().collect(),
    );
    a.len() == b.len()
        && a.iter()
            .zip(&b)
            .all(|(x, y)| x == y || (*x == "^?" && y.starts_with('^')))
}

fn replay(probe: &str) -> Vec<String> {
    let dir = PathBuf::from(env!("CARGO_MANIFEST_DIR")).join("../../oracle/trace/calcchain_probes");
    let steps = std::fs::read_to_string(dir.join(format!("{probe}.txt"))).unwrap();
    let walk = std::fs::read_to_string(dir.join(format!("walk_{probe}.txt"))).unwrap();
    let rec = read_walk(&walk);
    let lines: Vec<&str> = steps
        .lines()
        .map(str::trim)
        .filter(|l| !l.is_empty() && !l.starts_with('#'))
        .collect();
    assert_eq!(lines.len(), rec.len(), "{probe}: steps vs recorded steps");
    let mut m = Model::default();
    let mut bad = Vec::new();
    for (k, (line, (head, passes))) in lines.iter().zip(&rec).enumerate() {
        assert!(head.ends_with(line), "{probe}: step {} is {head}", k + 1);
        let mut logs = Vec::new();
        if m.apply(line) {
            m.recalc(&mut logs);
        }
        let mut why = Vec::new();
        if logs.len() != passes.len() {
            why.push(format!("{} passes, Excel {}", logs.len(), passes.len()));
        }
        for (j, (s, r)) in logs.iter().zip(passes).enumerate() {
            let mut d = Vec::new();
            if s.start != r.start {
                d.push(format!("start {} / Excel {}", s.start, r.start));
            }
            if s.chain0 != r.chain0 {
                d.push(format!(
                    "chain\n      sim   {}\n      excel {}",
                    s.chain0, r.chain0
                ));
            }
            if !same_events(&r.events, &s.events) {
                d.push(format!(
                    "walk\n      sim   {}\n      excel {}",
                    s.events, r.events
                ));
            }
            if let Some((next, valid, chain1)) = &r.end {
                if *valid != s.valid || (*valid && *next != s.next) {
                    d.push(format!(
                        "next {} {} / Excel {next} {valid}",
                        s.next, s.valid
                    ));
                }
                if *chain1 != s.chain1 {
                    d.push(format!(
                        "end chain\n      sim   {}\n      excel {}",
                        s.chain1, chain1
                    ));
                }
            }
            if !d.is_empty() {
                why.push(format!("pass {}: {}", j + 1, d.join("; ")));
            }
        }
        if !why.is_empty() {
            bad.push(format!(
                "{probe} step {} ({line}):\n    {}",
                k + 1,
                why.join("\n    ")
            ));
        }
    }
    bad
}

#[test]
fn one_thread_walk_matches_excel() {
    let probes = [
        "linear",
        "cycles",
        "cycles_mid",
        "firstref",
        "rangeorder",
        "redirty",
        "headerpull",
        "blocks",
        "fy09r",
        "fy09st",
        "r5",
    ];
    let bad: Vec<String> = probes.iter().flat_map(|p| replay(p)).collect();
    assert!(
        bad.is_empty(),
        "{} steps differ:\n{}",
        bad.len(),
        bad.join("\n")
    );
}
