pub mod ast;
pub mod lexer;
mod namech;
pub mod print;
pub mod r1c1;

use std::fmt;

use ast::*;
use lexer::{Lexer, Tok, Token};

use crate::address::{letters_to_col, parse_a1_cell, MAX_ROWS};

#[derive(Clone, Debug, PartialEq)]
pub struct ParseError {
    pub pos: usize,
    pub message: String,
}

impl fmt::Display for ParseError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{} (at {})", self.message, self.pos)
    }
}

impl std::error::Error for ParseError {}

pub fn parse(src: &str) -> Result<Expr, ParseError> {
    let body = src.strip_prefix('=').unwrap_or(src);
    if src.encode_utf16().count() > MAX_FORMULA_CHARS {
        return Err(ParseError {
            pos: 0,
            message: "formula is too long".into(),
        });
    }
    let tokens = Lexer::new(body).tokenize()?;
    let mut p = Parser {
        toks: tokens,
        i: 0,
        funcs: 0,
        parens: 0,
        rhs: 0,
        names_mode: false,
        empty_ws: String::new(),
        binder: 0,
        locals: Vec::new(),
    };
    let mut e = p.expr()?;

    while p.is_op(",") {
        let ws = p.cur().ws.clone();
        e = e.with_ws("", &ws);
        p.bump();
        let r = p.expr()?;
        e = Expr::Binary {
            op: BinOp::Union,
            left: Box::new(e),
            right: Box::new(r),
        };
    }
    match &p.cur().tok {
        Tok::End => {
            let mut trailing = p.cur().ws.clone();
            if let Some(i) = trailing.find('\n') {
                trailing.truncate(i);
                trailing.truncate(trailing.trim_end_matches(' ').len());
            }
            Ok(e.with_ws("", &trailing))
        }
        other => Err(p.err(format!("unexpected {}", describe(other)))),
    }
}

fn describe(t: &Tok) -> String {
    match t {
        Tok::Number { raw, .. } => format!("number {raw}"),
        Tok::Str(_) => "string".into(),
        Tok::Error(e) => e.to_string(),
        Tok::Ident(s) => format!("'{s}'"),
        Tok::Sheet(_) => "sheet reference".into(),
        Tok::StructRef { .. } => "structured reference".into(),
        Tok::Field(f) => format!("field '{f}'"),
        Tok::Op(o) => format!("'{o}'"),
        Tok::End => "end of formula".into(),
    }
}

struct Parser {
    toks: Vec<Token>,
    i: usize,
    funcs: u32,
    parens: u32,

    rhs: u32,

    names_mode: bool,

    empty_ws: String,

    binder: u8,

    locals: Vec<String>,
}

const MAX_FORMULA_CHARS: usize = 8192;
const MAX_FUNC_DEPTH: u32 = 65;
const MAX_PAREN_DEPTH: u32 = 256;

fn col_part(s: &str) -> Option<Coord> {
    let (abs, rest) = match s.strip_prefix('$') {
        Some(r) => (true, r),
        None => (false, s),
    };
    if rest.is_empty() || !rest.bytes().all(|b| b.is_ascii_alphabetic()) {
        return None;
    }
    letters_to_col(rest).map(|index| Coord { index, abs })
}

fn row_part(s: &str) -> Option<Coord> {
    let (abs, rest) = match s.strip_prefix('$') {
        Some(r) => (true, r),
        None => (false, s),
    };
    if rest.is_empty() || !rest.bytes().all(|b| b.is_ascii_digit()) || rest.starts_with('0') {
        return None;
    }
    let n: u32 = rest.parse().ok()?;
    (1..=MAX_ROWS)
        .contains(&n)
        .then_some(Coord { index: n - 1, abs })
}

fn cell_part(s: &str) -> Option<(Coord, Coord)> {
    parse_a1_cell(s)
        .map(|(r, c, ra, ca)| (Coord { index: r, abs: ra }, Coord { index: c, abs: ca }))
}

fn split_fields(name: &str) -> Option<(&str, Vec<String>)> {
    let (base, rest) = name.split_once('.')?;
    cell_part(base)?;
    let mut segs = Vec::new();
    for seg in rest.split('.') {
        let mut cs = seg.chars();
        let first = cs.next()?;
        let r1c1 = seg
            .chars()
            .all(|c| matches!(c.to_ascii_uppercase(), 'R' | 'C' | '0'..='9'))
            && matches!(first.to_ascii_uppercase(), 'R' | 'C');
        if !first.is_alphanumeric() || !cs.all(|c| c.is_alphanumeric() || c == '_') || r1c1 {
            return None;
        }
        segs.push(format!(".{seg}"));
    }
    Some((base, segs))
}

fn demote_field(e: Expr) -> Expr {
    if let Expr::Field { expr, .. } = &e {
        let mut base = expr.as_ref();
        while let Expr::Field { expr, .. } = base {
            base = expr;
        }
        match base {
            Expr::Ref {
                sheet: None,
                kind: RefKind::Cell { .. },
            } => {
                let mut name = String::new();
                print::write_expr(&mut name, &e);
                return Expr::Name { sheet: None, name };
            }
            Expr::Ref {
                sheet,
                kind: RefKind::Area { r0, c0, r1, c1 },
            } => {
                let cell = |row: Coord, col: Coord| Expr::Ref {
                    sheet: None,
                    kind: RefKind::Cell { row, col },
                };
                let mut first = String::new();
                print::write_expr(&mut first, &cell(*r0, *c0));

                let mut segs = Vec::new();
                let mut cur = &e;
                while let Expr::Field { expr, field } = cur {
                    segs.push(field.clone());
                    cur = expr;
                }
                segs.reverse();
                let mut second = String::new();
                print::write_expr(&mut second, &wrap_fields(cell(*r1, *c1), &segs));
                return Expr::Binary {
                    op: BinOp::Range,
                    left: Box::new(Expr::Name {
                        sheet: sheet.clone(),
                        name: first,
                    }),
                    right: Box::new(Expr::Name {
                        sheet: None,
                        name: second,
                    }),
                };
            }
            _ => {}
        }
    }
    e
}

fn cell_to_name(e: Expr) -> Expr {
    if let Expr::Ref {
        sheet: None,
        kind: RefKind::Cell { .. },
    } = &e
    {
        let mut name = String::new();
        print::write_expr(&mut name, &e);
        return Expr::Name { sheet: None, name };
    }
    e
}

fn r1c1_axis(word: &str) -> Option<(char, u32)> {
    let axis = word.chars().next()?.to_ascii_uppercase();
    let digits = &word[1..];
    if !matches!(axis, 'C' | 'R')
        || digits.is_empty()
        || !digits.bytes().all(|b| b.is_ascii_digit())
    {
        return None;
    }
    let n: u32 = digits.parse().ok()?;
    let max = if axis == 'C' { 16384 } else { 1_048_576 };
    (1..=max).contains(&n).then_some((axis, n))
}

fn wrap_fields(mut e: Expr, segs: &[String]) -> Expr {
    for f in segs {
        e = Expr::Field {
            expr: Box::new(e),
            field: f.clone(),
        };
    }
    e
}

impl Parser {
    fn cur(&self) -> &Token {
        &self.toks[self.i]
    }

    fn peek_tok(&self, k: usize) -> &Tok {
        &self.toks[(self.i + k).min(self.toks.len() - 1)].tok
    }

    fn bump(&mut self) -> Token {
        let t = self.toks[self.i].clone();
        if self.i < self.toks.len() - 1 {
            self.i += 1;
        }
        t
    }

    fn err(&self, msg: impl Into<String>) -> ParseError {
        ParseError {
            pos: self.cur().pos,
            message: msg.into(),
        }
    }

    fn is_op(&self, op: &str) -> bool {
        matches!(&self.cur().tok, Tok::Op(o) if *o == op)
    }

    fn expect_op(&mut self, op: &str) -> Result<(), ParseError> {
        if self.is_op(op) {
            self.bump();
            Ok(())
        } else {
            Err(self.err(format!(
                "expected '{op}', found {}",
                describe(&self.cur().tok)
            )))
        }
    }

    fn expr(&mut self) -> Result<Expr, ParseError> {
        stacker::maybe_grow(64 * 1024, 1024 * 1024, || self.comparison())
    }

    fn binary_level(
        &mut self,
        next: fn(&mut Parser) -> Result<Expr, ParseError>,
        ops: &[(&str, BinOp)],
    ) -> Result<Expr, ParseError> {
        let mut left = next(self)?;
        'outer: loop {
            for (sym, op) in ops {
                if self.is_op(sym) {
                    let ws = self.cur().ws.clone();
                    left = left.with_ws("", &ws);
                    self.bump();
                    self.rhs += 1;
                    let right = next(self);
                    self.rhs -= 1;
                    let right = right?;
                    left = Expr::Binary {
                        op: *op,
                        left: Box::new(left),
                        right: Box::new(right),
                    };
                    continue 'outer;
                }
            }
            return Ok(left);
        }
    }

    fn comparison(&mut self) -> Result<Expr, ParseError> {
        self.binary_level(
            Parser::concat,
            &[
                ("=", BinOp::Eq),
                ("<>", BinOp::Ne),
                ("<=", BinOp::Le),
                (">=", BinOp::Ge),
                ("<", BinOp::Lt),
                (">", BinOp::Gt),
            ],
        )
    }

    fn concat(&mut self) -> Result<Expr, ParseError> {
        self.binary_level(Parser::additive, &[("&", BinOp::Concat)])
    }

    fn additive(&mut self) -> Result<Expr, ParseError> {
        self.binary_level(
            Parser::multiplicative,
            &[("+", BinOp::Add), ("-", BinOp::Sub)],
        )
    }

    fn multiplicative(&mut self) -> Result<Expr, ParseError> {
        self.binary_level(Parser::power, &[("*", BinOp::Mul), ("/", BinOp::Div)])
    }

    fn power(&mut self) -> Result<Expr, ParseError> {
        self.binary_level(Parser::percent, &[("^", BinOp::Pow)])
    }

    fn percent(&mut self) -> Result<Expr, ParseError> {
        let mut e = self.prefix()?;
        while self.is_op("%") {
            let ws = self.cur().ws.clone();
            self.bump();
            e = Expr::Percent(Box::new(e.with_ws("", &ws)));
        }
        Ok(e)
    }

    fn prefix(&mut self) -> Result<Expr, ParseError> {
        self.check_unary_chain(self.rhs)?;
        let ws = self.cur().ws.clone();
        let e = stacker::maybe_grow(64 * 1024, 1024 * 1024, || self.prefix_inner())?;
        Ok(e.with_ws(&ws, ""))
    }

    fn check_unary_chain(&self, ctx: u32) -> Result<(), ParseError> {
        let mut n = 0usize;
        while matches!(self.peek_tok(n), Tok::Op("-" | "+" | "@")) {
            n += 1;
        }
        if n == 0 {
            return Ok(());
        }
        let base = match (self.peek_tok(n), self.peek_tok(n + 1)) {
            (Tok::Number { .. }, _) => 0,
            (Tok::Op("("), _) | (Tok::Ident(_), Tok::Op("(")) => 2,
            _ => 1,
        };
        if n as u32 + ctx + base > 1024 {
            return Err(self.err("too many consecutive unary operators"));
        }
        Ok(())
    }

    fn prefix_inner(&mut self) -> Result<Expr, ParseError> {
        let op = match &self.cur().tok {
            Tok::Op("-") => Some(UnaryOp::Neg),
            Tok::Op("+") => Some(UnaryOp::Plus),
            Tok::Op("@") => Some(UnaryOp::Implicit),
            _ => None,
        };
        if let Some(op) = op {
            self.bump();
            let expr = self.prefix()?;

            if op == UnaryOp::Implicit {
                if let Expr::Field { expr: inner, field } = expr.unwrap_ws() {
                    return Ok(Expr::Field {
                        expr: Box::new(Expr::Unary {
                            op,
                            expr: inner.clone(),
                        }),
                        field: field.clone(),
                    });
                }
            }
            return Ok(Expr::Unary {
                op,
                expr: Box::new(expr),
            });
        }
        self.intersection()
    }

    fn starts_operand(t: &Tok) -> bool {
        matches!(
            t,
            Tok::Ident(_)
                | Tok::Sheet(_)
                | Tok::StructRef { .. }
                | Tok::Number { .. }
                | Tok::Op("(")
        )
    }

    fn intersection(&mut self) -> Result<Expr, ParseError> {
        let mut left = self.range()?;
        while self.cur().space_before && Self::starts_operand(&self.cur().tok) {
            let ws = self.cur().ws.clone();
            left = left.with_ws("", &ws);
            let right = self.range()?;
            left = Expr::Binary {
                op: BinOp::Intersect,
                left: Box::new(left),
                right: Box::new(right),
            };
        }
        Ok(left)
    }

    fn range(&mut self) -> Result<Expr, ParseError> {
        let mut left = self.postfix()?;
        while self.is_op(":") {
            let demoted = matches!(left, Expr::Field { .. });
            left = demote_field(left);
            self.names_mode |= demoted;
            self.bump();
            let mut right = self.postfix()?;
            if demoted {
                right = cell_to_name(right);
            }

            let mut segs = Vec::new();
            if !demoted && !self.names_mode {
                while let Expr::Field { expr, field } = right {
                    segs.push(field);
                    right = *expr;
                }
                segs.reverse();
            }
            left = wrap_fields(
                Expr::Binary {
                    op: BinOp::Range,
                    left: Box::new(left),
                    right: Box::new(right),
                },
                &segs,
            );
        }
        Ok(left)
    }

    fn postfix(&mut self) -> Result<Expr, ParseError> {
        let mut e = self.primary()?;
        loop {
            if self.is_op("#") {
                self.bump();
                e = Expr::Spill(Box::new(demote_field(e)));
            } else if let Tok::Field(f) = &self.cur().tok {
                let field = f.clone();
                self.bump();
                e = Expr::Field {
                    expr: Box::new(e),
                    field,
                };
            } else if self.is_op("(")
                && !self.cur().space_before
                && matches!(
                    e,
                    Expr::Func { .. }
                        | Expr::Call { .. }
                        | Expr::Paren(_)
                        | Expr::Ref { .. }
                        | Expr::Spill(_)
                )
            {
                let args = self.args()?;
                let marker = self.empty_call_marker(&args);
                e = Expr::Call {
                    callee: Box::new(e),
                    args,
                }
                .with_ws(&marker, "");
            } else {
                return Ok(e);
            }
        }
    }

    fn empty_call_marker(&mut self, args: &[Expr]) -> String {
        let ws = std::mem::take(&mut self.empty_ws);
        if args.is_empty() && !ws.is_empty() {
            format!("\u{2}{ws}")
        } else {
            String::new()
        }
    }

    fn args(&mut self) -> Result<Vec<Expr>, ParseError> {
        self.funcs += 1;
        if self.funcs > MAX_FUNC_DEPTH {
            return Err(self.err("functions nested too deeply"));
        }
        let mark = self.locals.len();
        let r = self.args_inner();
        self.locals.truncate(mark);
        self.funcs -= 1;
        r
    }

    fn args_inner(&mut self) -> Result<Vec<Expr>, ParseError> {
        self.expect_op("(")?;
        let mut args = Vec::new();
        if self.is_op(")") {
            self.empty_ws = self.cur().ws.clone();
            self.bump();
            return Ok(args);
        }
        let binder = std::mem::take(&mut self.binder);
        let mut pending: Option<String> = None;
        loop {
            let k = args.len();
            if self.is_op(",") || self.is_op(")") {
                args.push(Expr::Missing);
            } else {
                let var = match (&self.cur().tok, self.peek_tok(1)) {
                    (Tok::Ident(w), Tok::Op(","))
                        if binder != 0
                            && (binder == 2 || k % 2 == 0)
                            && !w.contains('$')
                            && cell_part(w).is_some()
                            && split_fields(w).is_none() =>
                    {
                        Some(w.clone())
                    }
                    _ => None,
                };
                if let Some(w) = var {
                    self.bump();
                    if binder == 2 {
                        self.locals.push(w.to_uppercase());
                    } else {
                        pending = Some(w.to_uppercase());
                    }
                    args.push(Expr::Name {
                        sheet: None,
                        name: w,
                    });
                } else {
                    args.push(self.expr()?);
                    if let Some(w) = pending.take() {
                        self.locals.push(w);
                    }
                }
            }
            if self.is_op(",") {
                self.bump();
                continue;
            }
            if self.is_op(")") {
                let ws = self.cur().ws.clone();
                if let Some(last) = args.pop() {
                    args.push(if matches!(last, Expr::Missing) {
                        last
                    } else {
                        last.with_ws("", &ws)
                    });
                }
            }
            self.expect_op(")")?;
            return Ok(args);
        }
    }

    fn primary(&mut self) -> Result<Expr, ParseError> {
        let t = self.bump();
        match t.tok {
            Tok::Number { value, raw } => {
                if self.is_op(":") {
                    if let Some(r0) = row_part(&raw) {
                        if let Some(r1) = self.row_after_colon() {
                            return Ok(Expr::Ref {
                                sheet: None,
                                kind: RefKind::Rows { r0, r1 },
                            });
                        }
                    }
                }
                Ok(Expr::Number(value))
            }
            Tok::Str(s) => Ok(Expr::Text(s)),
            Tok::Error(e) => Ok(Expr::Error(e)),
            Tok::Op("(") => {
                self.parens += 1;
                if self.parens > MAX_PAREN_DEPTH {
                    return Err(self.err("parentheses nested too deeply"));
                }
                let mut e = self.expr()?;
                while self.is_op(",") {
                    let ws = self.cur().ws.clone();
                    e = e.with_ws("", &ws);
                    self.bump();
                    let r = self.expr()?;
                    e = Expr::Binary {
                        op: BinOp::Union,
                        left: Box::new(e),
                        right: Box::new(r),
                    };
                }
                let ws = if self.is_op(")") {
                    self.cur().ws.clone()
                } else {
                    String::new()
                };
                self.parens -= 1;
                self.expect_op(")")?;
                Ok(Expr::Paren(Box::new(e.with_ws("", &ws))))
            }
            Tok::Op("{") => self.array_constant(),

            Tok::StructRef { table, .. } if table.as_deref().is_some_and(|t| !is_valid_name(t)) => {
                self.i = self.i.saturating_sub(1);
                Err(self.err("table name is not a valid name"))
            }
            Tok::StructRef { table, spec } => Ok(Expr::StructRef { table, spec }),
            Tok::Sheet(spec) => self.after_sheet(spec),
            Tok::Ident(name) => self.after_ident(None, name),
            other => {
                self.i = self.i.saturating_sub(1);
                Err(self.err(format!("unexpected {}", describe(&other))))
            }
        }
    }

    fn row_after_colon(&mut self) -> Option<Coord> {
        if !self.is_op(":") {
            return None;
        }
        let r1 = match self.peek_tok(1) {
            Tok::Number { raw, .. } => row_part(raw),
            Tok::Ident(s) => row_part(s),
            _ => None,
        }?;
        self.bump();
        self.bump();
        Some(r1)
    }

    fn after_sheet(&mut self, spec: SheetSpec) -> Result<Expr, ParseError> {
        let t = self.bump();
        match t.tok {
            Tok::Ident(name) => self.after_ident(Some(spec), name),
            Tok::Number { raw, .. } => {
                let r0 = row_part(&raw)
                    .ok_or_else(|| self.err("expected a reference after sheet name"))?;
                let r1 = self
                    .row_after_colon()
                    .ok_or_else(|| self.err("expected a row range"))?;
                Ok(Expr::Ref {
                    sheet: Some(spec),
                    kind: RefKind::Rows { r0, r1 },
                })
            }
            Tok::Error(e) => Ok(Expr::Error(e)),
            _ => {
                self.i = self.i.saturating_sub(1);
                Err(self.err("expected a reference after sheet name"))
            }
        }
    }

    fn after_ident(&mut self, sheet: Option<SheetSpec>, name: String) -> Result<Expr, ParseError> {
        if let Some(n) = name.strip_prefix('\u{1}') {
            return Ok(Expr::Name {
                sheet,
                name: n.to_string(),
            });
        }

        if sheet.is_none() && self.is_op("(") && !self.cur().space_before {
            self.binder = match name.to_ascii_uppercase().trim_start_matches("_XLFN.") {
                "LET" => 1,
                "LAMBDA" => 2,
                _ => 0,
            };
            let mut args = self.args()?;

            if args.len() == 1
                && !matches!(args[0], Expr::Missing)
                && name.to_ascii_uppercase().trim_start_matches("_XLFN.") == "SINGLE"
            {
                return Ok(Expr::Unary {
                    op: UnaryOp::Implicit,
                    expr: Box::new(args.pop().unwrap()),
                });
            }
            let marker = self.empty_call_marker(&args);
            return Ok(Expr::Func { name, args }.with_ws(&marker, ""));
        }
        if sheet.is_none() && !self.names_mode && cell_part(&name).is_some() {
            let (mut n, mut dotted) = (1, None);
            while matches!(self.peek_tok(2 * n - 2), Tok::Op(":")) {
                let Tok::Ident(x) = self.peek_tok(2 * n - 1) else {
                    break;
                };
                if split_fields(x).is_some() {
                    dotted = Some(n);
                }
                n += 1;
            }
            if let Some(d) = dotted {
                if d + 1 < n {
                    self.names_mode = true;
                }
            }
        }
        if self.names_mode {
            if let Some((base, _)) = split_fields(&name) {
                if r1c1_axis(base).is_some() {
                    return Err(self.err(format!("invalid name '{name}'")));
                }
                return Ok(Expr::Name { sheet, name });
            }
            if cell_part(&name).is_some() {
                match r1c1_axis(&name) {
                    Some(('C', n)) => {
                        let c = Coord {
                            index: n - 1,
                            abs: true,
                        };
                        return Ok(Expr::Ref {
                            sheet,
                            kind: RefKind::Cols { c0: c, c1: c },
                        });
                    }
                    Some((_, n)) => {
                        let r = Coord {
                            index: n - 1,
                            abs: true,
                        };
                        return Ok(Expr::Ref {
                            sheet,
                            kind: RefKind::Rows { r0: r, r1: r },
                        });
                    }
                    None => {}
                }
                return Ok(Expr::Name { sheet, name });
            }
        }
        if let Some((base, segs)) = split_fields(&name) {
            let (row, col) = cell_part(base).unwrap();
            return Ok(wrap_fields(
                Expr::Ref {
                    sheet,
                    kind: RefKind::Cell { row, col },
                },
                &segs,
            ));
        }
        if let Some((row, col)) = cell_part(&name) {
            if sheet.is_none()
                && !self.locals.is_empty()
                && self.locals.contains(&name.to_uppercase())
                && !self.is_op(":")
                && !self.is_op("(")
                && !(self.i >= 2 && matches!(self.toks[self.i - 2].tok, Tok::Op(":")))
            {
                return Ok(Expr::Name { sheet, name });
            }

            if self.is_op(":") {
                if let Tok::Ident(second) = self.peek_tok(1) {
                    let second = second.clone();
                    let (second_cell, segs) = match split_fields(&second) {
                        Some((b, segs)) => (cell_part(b), segs),
                        None => (cell_part(&second), Vec::new()),
                    };
                    if let Some((r1, c1)) = second_cell {
                        let call_next = matches!(self.peek_tok(2), Tok::Op("("))
                            && !self.toks[(self.i + 2).min(self.toks.len() - 1)].space_before;
                        if !call_next {
                            self.bump();
                            self.bump();
                            return Ok(wrap_fields(
                                Expr::Ref {
                                    sheet,
                                    kind: RefKind::Area {
                                        r0: row,
                                        c0: col,
                                        r1,
                                        c1,
                                    },
                                },
                                &segs,
                            ));
                        }
                    }
                }
            }
            return Ok(Expr::Ref {
                sheet,
                kind: RefKind::Cell { row, col },
            });
        }
        if let Some(c0) = col_part(&name) {
            if self.is_op(":") {
                if let Tok::Ident(second) = self.peek_tok(1) {
                    if let Some(c1) = col_part(second) {
                        self.bump();
                        self.bump();
                        return Ok(Expr::Ref {
                            sheet,
                            kind: RefKind::Cols { c0, c1 },
                        });
                    }
                }
            }
        }
        if let Some(r0) = row_part(&name) {
            if let Some(r1) = self.row_after_colon() {
                return Ok(Expr::Ref {
                    sheet,
                    kind: RefKind::Rows { r0, r1 },
                });
            }
        }
        if sheet.is_none() {
            if name.eq_ignore_ascii_case("TRUE") {
                return Ok(Expr::Bool(true));
            }
            if name.eq_ignore_ascii_case("FALSE") {
                return Ok(Expr::Bool(false));
            }
        }
        if name.contains('$') || name.starts_with(|c: char| c.is_ascii_digit()) {
            return Err(self.err(format!("invalid reference '{name}'")));
        }
        Ok(Expr::Name { sheet, name })
    }

    fn array_constant(&mut self) -> Result<Expr, ParseError> {
        let mut rows: Vec<Vec<ArrayItem>> = vec![Vec::new()];
        loop {
            let item = self.array_item()?;
            rows.last_mut().unwrap().push(item);
            if self.is_op(",") {
                self.bump();
            } else if self.is_op(";") {
                self.bump();
                rows.push(Vec::new());
            } else {
                self.expect_op("}")?;
                break;
            }
        }
        let width = rows[0].len();
        if rows.iter().any(|r| r.len() != width) {
            return Err(self.err("array constant rows must have the same length"));
        }
        Ok(Expr::Array(rows))
    }

    fn array_item(&mut self) -> Result<ArrayItem, ParseError> {
        let mut neg = false;
        if self.is_op("-") || self.is_op("+") {
            neg = self.is_op("-");
            self.bump();
            if self.cur().space_before || !matches!(self.cur().tok, Tok::Number { .. }) {
                return Err(
                    self.err("array constants may only contain numbers, text, booleans and errors")
                );
            }
        }
        let t = self.bump();
        let item = match t.tok {
            Tok::Number { value, .. } => ArrayItem::Number(if neg { -value } else { value }),
            Tok::Str(s) if !neg => ArrayItem::Text(s),
            Tok::Error(e) if !neg => ArrayItem::Error(e),
            Tok::Ident(s) if !neg && s.eq_ignore_ascii_case("TRUE") => ArrayItem::Bool(true),
            Tok::Ident(s) if !neg && s.eq_ignore_ascii_case("FALSE") => ArrayItem::Bool(false),
            _ => {
                self.i = self.i.saturating_sub(1);
                return Err(
                    self.err("array constants may only contain numbers, text, booleans and errors")
                );
            }
        };
        Ok(item)
    }
}

pub fn reads_as_r1c1(name: &str) -> bool {
    let b = name.as_bytes();
    let up = |i: usize| b.get(i).map(|c| c.to_ascii_uppercase());
    let digits = |i: usize| {
        let mut j = i;
        let mut v = 0u64;
        while j < b.len() && b[j].is_ascii_digit() {
            v = v
                .saturating_mul(10)
                .saturating_add((b[j] - b'0') as u64)
                .min(u64::MAX / 20);
            j += 1;
        }
        (j, j - i, v)
    };

    let col = |i: usize| {
        let (j, d, v) = digits(i);
        if d == 0 {
            return j >= b.len();
        }
        if v == 0 {
            return false;
        }
        if j >= b.len() {
            return true;
        }
        v <= 16384 && b[j] != b'.'
    };
    match up(0) {
        Some(b'R') => {
            let (j, d, v) = digits(1);
            if d > 0 {
                if v == 0 || v > 1_048_576 {
                    return false;
                }
                return match up(j) {
                    None => true,
                    Some(b'C') => true,
                    Some(b'.') => up(j + 1) == Some(b'C'),
                    Some(_) => true,
                };
            }
            match up(1) {
                None => true,
                Some(b'C') => col(2),
                _ => false,
            }
        }
        Some(b'C') => col(1),
        _ => false,
    }
}

pub fn r1c1_pair_with_tail(name: &str) -> bool {
    let b = name.as_bytes();
    let digits = |mut i: usize| {
        let st = i;
        while i < b.len() && b[i].is_ascii_digit() {
            i += 1;
        }
        (i, i - st)
    };
    let l1 = b.first().map_or(0, |c| c.to_ascii_uppercase());
    if l1 == b'R' || l1 == b'C' {
        let (i, d1) = digits(1);
        if i < b.len() {
            let l2 = b[i].to_ascii_uppercase();
            if (l2 == b'R' || l2 == b'C') && l2 != l1 {
                let (end, d2) = digits(i + 1);
                return d1 + d2 > 0 && end < b.len();
            }
        }
    }
    false
}

pub fn starts_like_r1c1(name: &str) -> bool {
    if !reads_as_r1c1(name) {
        return false;
    }

    let b = name.as_bytes();
    let skip_digits = |i: &mut usize| {
        while *i < b.len() && b[*i].is_ascii_digit() {
            *i += 1;
        }
    };
    let mut i = 1;
    skip_digits(&mut i);
    if b[0].eq_ignore_ascii_case(&b'R') && i < b.len() && b[i].eq_ignore_ascii_case(&b'C') {
        i += 1;
        skip_digits(&mut i);
    }
    i < b.len()
}

pub fn is_workbook_reserved_name(name: &str) -> bool {
    name.starts_with('\\') && name.chars().count() == 2
        || name.len() > 3
            && name.get(..3).is_some_and(|p| p.eq_ignore_ascii_case("_xl"))
            && name.contains('.')
}

pub fn is_valid_name(name: &str) -> bool {
    let mut chars = name.chars();
    let Some(first) = chars.next() else {
        return false;
    };
    let ok = |c: char, first: bool| {
        if c.is_ascii() {
            c.is_ascii_alphabetic()
                || matches!(c, '_' | '\\')
                || (!first && (c.is_ascii_digit() || matches!(c, '.' | '?')))
        } else {
            namech::name_char(c, first)
        }
    };
    if !ok(first, true) || !chars.all(|c| ok(c, false)) {
        return false;
    }
    if name.encode_utf16().count() > 255 || starts_like_r1c1(name) {
        return false;
    }
    if name.eq_ignore_ascii_case("TRUE") || name.eq_ignore_ascii_case("FALSE") {
        return false;
    }
    if name.eq_ignore_ascii_case("R") || name.eq_ignore_ascii_case("C") {
        return false;
    }

    if let Some((base, rest)) = name.split_once('.') {
        if cell_part(base).is_some() && rest.split('.').any(|seg| cell_part(seg).is_some()) {
            return false;
        }
    }
    if cell_part(name).is_some() {
        return false;
    }

    let up = name.to_ascii_uppercase();
    let rc = up
        .strip_prefix('R')
        .map(|r| r.trim_start_matches(|c: char| c.is_ascii_digit()));
    if let Some(rest) = rc {
        if rest.is_empty()
            || rest
                .strip_prefix('C')
                .is_some_and(|c| c.chars().all(|d| d.is_ascii_digit()))
        {
            return false;
        }
    }
    if let Some(rest) = up.strip_prefix('C') {
        if rest.chars().all(|d| d.is_ascii_digit()) {
            return false;
        }
    }
    true
}

#[cfg(test)]
mod tests {
    use super::print::print;
    use super::*;

    fn rt(src: &str) -> String {
        print(&parse(src).unwrap_or_else(|e| panic!("{src}: {e}")))
    }

    #[test]
    fn round_trips() {
        for (src, want) in [
            ("=1+2*3", "1+2*3"),
            ("=-2^2", "-2^2"),
            ("=sum(A1:B2, $C$3)", "SUM(A1:B2, $C$3)"),
            ("=SUM( 1 , 2 )", "SUM( 1, 2 )"),
            ("=  A1  &  B1  ", "  A1  &  B1  "),
            ("=-  1", "-  1"),
            ("=Sheet1!A1", "Sheet1!A1"),
            ("='My Sheet'!$A$1:B2", "'My Sheet'!$A$1:B2"),
            ("=SUM(A:A)", "SUM(A:A)"),
            ("=SUM(1:3)", "SUM(1:3)"),
            ("=SUM($1:$3)", "SUM($1:$3)"),
            ("={1,2;3,4}", "{1,2;3,4}"),
            ("={-1,\"a\",TRUE,#N/A}", "{-1,\"a\",TRUE,#N/A}"),
            ("=A1#", "A1#"),
            ("=@A1:A3", "@A1:A3"),
            ("=SUM((A1,B2))", "SUM((A1,B2))"),
            ("=A1:B2 B1:C3", "A1:B2 B1:C3"),
            ("=IF(A1,,)", "IF(A1,,)"),
            ("=LAMBDA(x,x+1)(2)", "LAMBDA(x,x+1)(2)"),
            ("=50%%", "50%%"),
            ("=\"a\"\"b\"", "\"a\"\"b\""),
            ("=Sheet1:Sheet3!A1", "Sheet1:Sheet3!A1"),
            ("=A1:INDEX(B:B,3)", "A1:INDEX(B:B,3)"),
            ("=TRUE", "TRUE"),
            ("=_xlfn.XLOOKUP(1,A:A,B:B)", "_xlfn.XLOOKUP(1,A:A,B:B)"),
            ("=Table1[Col]", "Table1[Col]"),
        ] {
            assert_eq!(rt(src), want, "source: {src}");
        }
    }

    #[test]
    fn precedence() {
        let e = parse("-2^2").unwrap();
        assert!(matches!(e, Expr::Binary { op: BinOp::Pow, .. }));

        let e = parse("2^3^2").unwrap();
        match e {
            Expr::Binary {
                op: BinOp::Pow,
                left,
                ..
            } => {
                assert!(matches!(*left, Expr::Binary { op: BinOp::Pow, .. }))
            }
            _ => panic!(),
        }
    }

    #[test]
    fn rejects() {
        for src in [
            "=1+", "=SUM(", "=1E-320", "=(1", "={1,A1}", "=A1 +", "={1,2;3}",
        ] {
            assert!(parse(src).is_err(), "{src} should not parse");
        }
    }

    #[test]
    fn names() {
        assert!(is_valid_name("rate"));
        assert!(is_valid_name("_x.y"));
        assert!(!is_valid_name("A1"));
        assert!(!is_valid_name("R1C1"));
        assert!(!is_valid_name("TRUE"));
        assert!(!is_valid_name("1abc"));
    }
}
