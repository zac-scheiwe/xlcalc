pub mod ast;
pub mod lexer;
pub(crate) mod namech;
pub mod print;
pub(crate) mod prsnam;
pub mod r1c1;
pub(crate) mod r1c1name;
pub mod wild;

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
    parse_with(src, &|_| true)
}

pub fn parse_with(src: &str, sheet_exists: &dyn Fn(&str) -> bool) -> Result<Expr, ParseError> {
    parse_entry(src, sheet_exists, false)
}

pub fn parse_typed(src: &str, sheet_exists: &dyn Fn(&str) -> bool) -> Result<Expr, ParseError> {
    parse_entry(src, sheet_exists, true)
}

fn parse_entry(
    src: &str,
    sheet_exists: &dyn Fn(&str) -> bool,
    typed: bool,
) -> Result<Expr, ParseError> {
    let body = src.strip_prefix('=').unwrap_or(src);

    if body.encode_utf16().count() + 1 > MAX_FORMULA_CHARS {
        return Err(ParseError {
            pos: 0,
            message: "formula is too long".into(),
        });
    }
    let tokens = Lexer::new(body, sheet_exists).typed(typed).tokenize()?;
    let mut p = Parser {
        sheet_exists,
        toks: tokens,
        i: 0,
        funcs: 0,
        parens: 0,
        rhs: 0,
        uops: 0,
        operands: 0,
        names_mode: false,
        empty_ws: String::new(),
        binder: 0,
        locals: Vec::new(),
        union_ok: true,
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
        Tok::End if matches!(e.unwrap_ws(), Expr::Unwritten) => {
            Err(p.err("expected a reference after sheet name"))
        }
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

struct Parser<'a> {
    sheet_exists: &'a dyn Fn(&str) -> bool,
    toks: Vec<Token>,
    i: usize,
    funcs: u32,
    parens: u32,

    rhs: u32,

    uops: u32,

    operands: u32,

    names_mode: bool,

    empty_ws: String,

    binder: u8,

    locals: Vec<String>,

    union_ok: bool,
}

const MAX_FORMULA_CHARS: usize = 0x2000;

const MAX_FUNC_DEPTH: u32 = 0x41;

const MAX_PAREN_DEPTH: u32 = 0x100;

const MAX_OPR_PUSHES: u32 = (0x8070 - 0x2070) / 0x18 - 1;

const MAX_OPERANDS: u32 = 0x400;

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
                sheet,
                kind: RefKind::Cell { .. },
            } => {
                let mut name = String::new();
                print::write_expr(&mut name, &strip_sheet(&e));
                return Expr::Name {
                    sheet: sheet.clone(),
                    name,
                };
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

fn strip_sheet(e: &Expr) -> Expr {
    match e {
        Expr::Field { expr, field } => Expr::Field {
            expr: Box::new(strip_sheet(expr)),
            field: field.clone(),
        },
        Expr::Ref { kind, .. } => Expr::Ref {
            sheet: None,
            kind: *kind,
        },
        other => other.clone(),
    }
}

fn chain_names(left: Expr, right: Expr) -> (Expr, Expr) {
    if matches!(left, Expr::Field { .. }) || matches!(right, Expr::Field { .. }) {
        (
            cell_to_name(demote_field(left)),
            cell_to_name(demote_field(right)),
        )
    } else {
        (left, right)
    }
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

impl Parser<'_> {
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
        xlcalc_stack::maybe_grow(64 * 1024, 1024 * 1024, || self.comparison())
    }

    fn binary_level(
        &mut self,
        next: fn(&mut Self) -> Result<Expr, ParseError>,
        ops: &[(&str, BinOp)],
    ) -> Result<Expr, ParseError> {
        let mut left = next(self)?;
        'outer: loop {
            for (sym, op) in ops {
                if self.is_op(sym) {
                    let ws = self.cur().ws.clone();
                    left = left.with_ws("", &ws);
                    self.bump();
                    self.opr_room(1)?;
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

        loop {
            if self.is_op("%") {
                let ws = self.cur().ws.clone();
                self.bump();
                e = Expr::Percent(Box::new(e.with_ws("", &ws)));
            } else if let (Expr::Percent(_), Tok::Field(f)) = (&e, &self.cur().tok) {
                let field = f.clone();
                self.bump();
                e = Expr::Field {
                    expr: Box::new(e),
                    field,
                };
            } else {
                return Ok(e);
            }
        }
    }

    fn prefix(&mut self) -> Result<Expr, ParseError> {
        let ws = self.cur().ws.clone();
        let e = xlcalc_stack::maybe_grow(64 * 1024, 1024 * 1024, || self.prefix_inner())?;
        Ok(e.with_ws(&ws, ""))
    }

    fn opr_room(&self, n: u32) -> Result<(), ParseError> {
        if self.rhs + self.uops + n > MAX_OPR_PUSHES {
            return Err(self.err("formula is too complex"));
        }
        Ok(())
    }

    fn operand_room(&self) -> Result<(), ParseError> {
        if self.rhs + self.operands >= MAX_OPERANDS {
            return Err(self.err("formula is too complex"));
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
            let folded = op != UnaryOp::Implicit && matches!(self.peek_tok(1), Tok::Number { .. });
            let pushed = u32::from(!folded);
            self.opr_room(pushed)?;
            self.bump();

            let keep = self.union_ok && op != UnaryOp::Implicit;
            let union_ok = std::mem::replace(&mut self.union_ok, keep);
            self.uops += pushed;
            let expr = self.prefix();
            self.uops -= pushed;
            self.union_ok = union_ok;
            let expr = expr?;
            let at_union = op == UnaryOp::Implicit && union_ok;

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
            let e = Expr::Unary {
                op,
                expr: Box::new(expr),
            };
            return if at_union { self.union_tail(e) } else { Ok(e) };
        }
        self.operand_room()?;
        self.intersection()
    }

    fn starts_operand(t: &Tok) -> bool {
        matches!(
            t,
            Tok::Ident(_)
                | Tok::Sheet(_)
                | Tok::StructRef { .. }
                | Tok::Number { .. }
                | Tok::Op("(" | "@")
        )
    }

    fn intersection(&mut self) -> Result<Expr, ParseError> {
        let left = self.intersection_inner()?;
        self.union_tail(left)
    }

    fn union_tail(&mut self, mut left: Expr) -> Result<Expr, ParseError> {
        while self.union_ok && self.is_op(",") {
            let ws = self.cur().ws.clone();
            self.bump();
            let before_ws = if self.is_op("@") {
                String::new()
            } else {
                self.cur().ws.clone()
            };
            let right = if self.is_op("@") {
                let saved = std::mem::replace(&mut self.union_ok, false);
                let r = self.prefix();
                self.union_ok = saved;
                r?
            } else {
                self.intersection_inner()?
            };
            let (l, right) = chain_names(left, right);
            left = l.with_ws("", &ws);
            let right = right.with_ws(&before_ws, "");
            left = Expr::Binary {
                op: BinOp::Union,
                left: Box::new(left),
                right: Box::new(right),
            };
        }
        Ok(left)
    }

    fn intersection_inner(&mut self) -> Result<Expr, ParseError> {
        let mut left = self.range()?;
        while self.cur().space_before && Self::starts_operand(&self.cur().tok) {
            let mut ws = self.cur().ws.clone();
            if ws.contains('\n') {
                ws = linebreak_operator(&ws).ok_or_else(|| self.err("unexpected blank"))?;
            }

            let at = self.is_op("@");
            let right = if at {
                match self.prefix()? {
                    Expr::Ws { after, expr, .. } if after.is_empty() => *expr,
                    Expr::Ws { after, expr, .. } => Expr::Ws {
                        before: String::new(),
                        after,
                        expr,
                    },
                    e => e,
                }
            } else {
                self.range()?
            };
            let (l, right) = if at {
                (left, right)
            } else {
                chain_names(left, right)
            };
            left = l.with_ws("", &ws);
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

            let (ws_before, ws_after) = {
                let b = self.cur().ws.clone();
                self.bump();
                (b, self.cur().ws.clone())
            };

            let right_op = if self.is_op("@") {
                self.prefix()?
            } else {
                self.postfix()?
            };

            let lone_cell = matches!(
                left.unwrap_ws(),
                Expr::Ref {
                    kind: RefKind::Cell { .. },
                    ..
                }
            );
            let fuses = lone_cell
                && matches!(
                    right_op.unwrap_ws(),
                    Expr::Ref {
                        sheet: None,
                        kind: RefKind::Cell { .. } | RefKind::Area { .. },
                    }
                );
            let (ws_before, ws_after) = if fuses {
                (String::new(), String::new())
            } else if lone_cell {
                (format!("{ws_before}{ws_after}"), ws_after)
            } else {
                (ws_before, ws_after)
            };
            left = left.with_ws("", &ws_before);
            let mut right = right_op.with_ws(&ws_after, "");
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
                let ws = self.cur().ws.clone();
                self.bump();
                if !ws.is_empty() && !matches!(self.toks[self.i].tok, Tok::End) {
                    let next = &mut self.toks[self.i];
                    next.ws = format!("{ws}{}", next.ws);
                }
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
        let union_ok = std::mem::replace(&mut self.union_ok, false);
        let operands = self.operands;
        let r = self.args_inner();
        self.operands = operands;
        self.union_ok = union_ok;
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
                self.operand_room()?;
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
                        self.locals.push(crate::text::name_key(&w));
                    } else {
                        pending = Some(crate::text::name_key(&w));
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
            self.operands += 1;
            if self.is_op(",") {
                self.bump();
                continue;
            }
            if self.is_op(")") {
                self.opr_room(1)?;
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
                let union_ok = std::mem::replace(&mut self.union_ok, true);
                let e = self.expr();
                self.union_ok = union_ok;
                let mut e = e?;
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
                if self.is_op(")") {
                    self.opr_room(1)?;
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

            Tok::StructRef { table, .. } if table.as_deref().is_some_and(|t| !is_table_name(t)) => {
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
        if !self.is_op(":")
            || self.cur().space_before
            || self.toks[(self.i + 1).min(self.toks.len() - 1)].space_before
        {
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

    fn after_sheet(&mut self, sheet: SheetSpec) -> Result<Expr, ParseError> {
        if let SheetSpec::Range(a, b) = &sheet {
            if self.cur().ws.starts_with(' ') && (self.sheet_exists)(a) && (self.sheet_exists)(b) {
                return Ok(Expr::Unwritten);
            }
        }
        let t = self.bump();
        match t.tok {
            Tok::Ident(name) => self.after_ident(Some(sheet), name),
            Tok::Number { raw, .. } => {
                if self.names_mode {
                    return Err(self.err("expected a name after sheet name"));
                }
                let r0 = row_part(&raw)
                    .ok_or_else(|| self.err("expected a reference after sheet name"))?;
                let r1 = self
                    .row_after_colon()
                    .ok_or_else(|| self.err("expected a row range"))?;
                Ok(Expr::Ref {
                    sheet: Some(sheet),
                    kind: RefKind::Rows { r0, r1 },
                })
            }
            Tok::Error(crate::XlError::REF) => Ok(Expr::DeadRef(sheet)),
            Tok::Error(e) => Ok(Expr::Error(e)),

            Tok::StructRef {
                table: Some(table),
                spec,
            } if is_table_name(&table) => {
                let missing = match &sheet {
                    SheetSpec::Single(n) => !n.is_empty() && !(self.sheet_exists)(n),
                    SheetSpec::Range(..) => true,
                    SheetSpec::External { .. } => true,
                };
                if missing {
                    let inner = if spec.is_empty() {
                        "#Data"
                    } else {
                        spec.as_str()
                    };
                    return Ok(Expr::DeadTable {
                        sheet,
                        text: format!("{table}[{inner}]"),
                    });
                }
                Ok(Expr::StructRef {
                    table: Some(table),
                    spec,
                })
            }
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

        if let (Some(_), Some((r, c, ra, ca))) = (&sheet, crate::address::parse_a1_cell(&name)) {
            if !self.names_mode && self.is_op("(") && !self.cur().space_before {
                let args = self.args()?;
                return Ok(Expr::Call {
                    callee: Box::new(Expr::Ref {
                        sheet,
                        kind: RefKind::Cell {
                            row: Coord { index: r, abs: ra },
                            col: Coord { index: c, abs: ca },
                        },
                    }),
                    args,
                });
            }
        }

        let name = match &sheet {
            Some(SheetSpec::Single(book))
                if self.is_op("(") && !self.cur().space_before && is_addin_file(book) =>
            {
                let qualified = format!("{book}!{name}");
                return self.after_ident(None, qualified);
            }
            _ => name,
        };

        if let Some(SheetSpec::Single(book)) = &sheet {
            if !self.names_mode && self.is_op("(") && !self.cur().space_before {
                let args = self.args()?;
                return Ok(Expr::Func {
                    name: format!("{book}!{name}"),
                    args,
                });
            }
        }

        if sheet.is_none() && self.is_op("(") && !self.cur().space_before {
            if name.contains('$') && crate::address::parse_a1_cell(&name).is_none() {
                return Err(self.err("a name cannot contain '$'"));
            }

            if name
                .split_once('.')
                .is_some_and(|(base, _)| cell_part(base).is_some() && reads_as_r1c1(base))
            {
                return Err(self.err("cell word before a dot in a call name"));
            }
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
                    if matches!(self.peek_tok(2 * n - 1), Tok::Sheet(_)) {
                        n += 1;
                    }
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
            if name.contains('$')
                || (sheet.is_some()
                    && matches!(name.to_ascii_uppercase().as_str(), "TRUE" | "FALSE"))
            {
                return Err(self.err(format!("invalid name '{name}'")));
            }
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
                && self.locals.contains(&crate::text::name_key(&name))
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
            let tight = !self.cur().space_before
                && !self.toks[(self.i + 1).min(self.toks.len() - 1)].space_before;
            if self.is_op(":") && tight {
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
        if name.eq_ignore_ascii_case("TRUE") || name.eq_ignore_ascii_case("FALSE") {
            if sheet.is_some() {
                return Err(self.err(format!("invalid name '{name}'")));
            }
            return Ok(Expr::Bool(name.eq_ignore_ascii_case("TRUE")));
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
            Tok::Number { value, .. } => {
                ArrayItem::Number(crate::number::normalize(if neg { -value } else { value }))
            }
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

pub use r1c1name::reads_as_r1c1;

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

fn is_table_name(name: &str) -> bool {
    (name.encode_utf16().count() <= 255 && prsnam::prs_nam(name, true))
        || (crate::workbook::tables::table_display_name(name) == name
            && !name.eq_ignore_ascii_case("TRUE")
            && !name.eq_ignore_ascii_case("FALSE"))
}

pub fn is_valid_name(name: &str) -> bool {
    name.encode_utf16().count() <= 255 && prsnam::prs_nam(name, false)
}

pub fn name_from_label(raw: &str) -> Option<String> {
    if raw.eq_ignore_ascii_case("TRUE") || raw.eq_ignore_ascii_case("FALSE") {
        return None;
    }
    cch_legal_name(raw)
}

pub fn cch_legal_name(raw: &str) -> Option<String> {
    cch_legal_name_with(raw, &|_| false)
}

fn is_easy_name(name: &str) -> bool {
    let Some(first) = name.chars().next() else {
        return false;
    };
    if !first.is_alphabetic()
        || matches!(first, 'R' | 'r' | 'C' | 'c')
        || name.eq_ignore_ascii_case("TRUE")
        || name.eq_ignore_ascii_case("FALSE")
    {
        return false;
    }
    let (mut digits, mut others) = (0, 0);
    for (i, c) in name.chars().enumerate() {
        if !prsnam::name_scan_char(c, i == 0) {
            return false;
        }
        if c.is_ascii_digit() {
            digits += 1;
        } else {
            others += 1;
        }
    }
    digits == 0 || others >= 4
}

pub(crate) fn cch_legal_name_with(raw: &str, is_table: &dyn Fn(&str) -> bool) -> Option<String> {
    const MAX: usize = 255;
    let valid = |s: &str| is_valid_name(s) && (is_easy_name(s) || !is_table(s));
    let chars: Vec<char> = raw.chars().collect();
    let first = *chars.first()?;
    let (first_us, last_us) = (first == '_', *chars.last()? == '_');
    let mut out: Vec<char> = Vec::new();
    if !prsnam::name_scan_char(first, true) {
        out.push('_');
    }
    for &c in &chars {
        if out.len() >= MAX {
            break;
        }
        out.push(if prsnam::name_scan_char(c, out.is_empty()) {
            c
        } else {
            '_'
        });
    }
    if !last_us {
        while out.len() > 1 && out.last() == Some(&'_') {
            out.pop();
        }
    }
    if !first_us {
        while out.len() > 1 && out[0] == '_' && prsnam::name_scan_char(out[1], true) {
            out.remove(0);
        }
    }
    let base: String = out.iter().collect();
    if valid(&base) {
        return Some(base);
    }
    if out.len() + 1 < MAX {
        let appended = format!("{base}_");
        if valid(&appended) {
            return Some(appended);
        }
    }
    let prepended = format!("_{base}");
    is_valid_name(&prepended).then_some(prepended)
}

fn linebreak_operator(ws: &str) -> Option<String> {
    if ws.chars().any(|c| c != ' ' && c != '\n') {
        return None;
    }
    let breaks = ws.matches('\n').count();
    let tail = ws.len() - ws.rfind('\n')? - 1;
    Some(format!(
        "{}{}",
        "\n".repeat(breaks),
        " ".repeat((tail + 255) % 256 + 1)
    ))
}

pub(super) fn is_addin_file(book: &str) -> bool {
    let lower = book.to_ascii_lowercase();
    [".xlam", ".xla", ".xll"].iter().any(|e| lower.ends_with(e))
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
    fn national_digits_and_signs() {
        assert_eq!(rt("=\u{663}.\u{665}"), "3.5");
        assert_eq!(rt("=x\u{660}\u{661}\u{663}\u{669}"), "X139");

        assert_eq!(rt("=A\u{660}:A\u{669}"), "A\u{660}:A9");

        assert!(parse("=\u{ff10}").is_ok());
        for src in ["=-\u{ff10}", "=+\u{b2}", "=2^-\u{b2}", "=SUM(-\u{b2})"] {
            assert!(parse(src).is_err(), "{src}");
        }
        assert!(parse("=1-\u{b2}").is_ok());
        assert!(parse("=- \u{b2}").is_ok());

        assert!(parse("=-\u{2074}").is_ok());
    }

    #[test]
    fn unary_chain_at_sign() {
        let chain = |op: &str, n: usize| format!("={}1", op.repeat(n));
        assert!(parse(&chain("-", 1024)).is_ok());
        assert!(parse(&chain("@", 1023)).is_ok());
        assert!(parse(&chain("@", 1024)).is_err());
        assert!(parse(&format!("={}@1", "-".repeat(1023))).is_err());
    }

    #[test]
    fn blanks_before_spill_move_after_it() {
        assert_eq!(rt("=SUM(A1 #)"), "SUM(A1# )");
        assert_eq!(rt("=A1 #+1"), "A1# +1");
        assert_eq!(rt("=A1 # +1"), "A1#  +1");
        assert_eq!(rt("=SUM(A1 #,1)"), "SUM(A1#,1)");
        assert_eq!(rt("=A1 #"), "A1#");
    }

    #[test]
    fn dollar_callee_is_a_reference_or_refused() {
        assert!(parse("=$A$1()").is_ok());

        assert!(matches!(
            parse("=Data!$A$1()"),
            Ok(Expr::Call { callee, .. }) if matches!(*callee, Expr::Ref { .. })
        ));
        assert!(parse("=$A()").is_err());
        assert!(parse("=$$A$1()").is_err());
        assert!(parse("=A$()").is_err());
    }

    #[test]
    fn bare_dot_after_an_r1c1_axis_word_is_refused() {
        assert!(parse("=C1.").is_err());
        assert!(parse("=(C1.)").is_err());
        assert!(parse("=SUM(C1.)").is_err());
        assert!(parse("=A1.").is_ok());
        assert!(parse("=C1.x").is_ok());
    }

    #[test]
    fn file_name_characters_in_a_quoted_prefix() {
        assert!(parse("='a*b'! C1").is_ok());
        assert!(parse("='a?b'!#REF!").is_ok());
        assert!(parse("='a]b'!A1").is_err());
        assert!(parse("='a/b'!A1").is_ok());
        assert!(parse("='a|b'!A1").is_ok());
    }

    #[test]
    fn a_span_and_a_blank_write_no_operand() {
        let sheets = |n: &str| n == "Data" || n == "S";
        for f in ["='Data:S'! B2", "=S:Data! B2:C3", "='Data':S!  $B$2"] {
            let e = parse_with(f, &sheets).unwrap();
            assert!(
                matches!(e.unwrap_ws(), Expr::Binary { op: BinOp::Intersect, left, .. }
                    if matches!(left.unwrap_ws(), Expr::Unwritten)),
                "{f}: {e:?}"
            );
        }

        assert!(parse_with("='Data:S'! ", &sheets).is_err());

        assert!(!matches!(
            parse_with("='Data:Nope'! B2", &sheets).unwrap().unwrap_ws(),
            Expr::Binary { .. }
        ));
    }

    #[test]
    fn table_after_a_missing_sheet_is_a_dead_reference() {
        let only_data = |n: &str| n == "Data";
        let dead = parse_with("=Nope!Tb[a]", &only_data).unwrap();
        assert!(matches!(dead, Expr::DeadTable { .. }));
        assert_eq!(print(&dead), "Nope!Tb[a]");
        let dead = parse_with("=C1!Tb[a]", &only_data).unwrap();
        assert_eq!(print(&dead), "'C1'!Tb[a]");
        assert!(matches!(
            parse_with("=''!Tb[a]", &only_data).unwrap(),
            Expr::StructRef { .. }
        ));
        let kept = parse_with("=Data!Tb[a]", &only_data).unwrap();
        assert!(matches!(kept, Expr::StructRef { .. }));
    }

    #[test]
    fn r1c1_word_before_bang_is_a_sheet() {
        assert_eq!(r1c1::rewrite("=C1!RC", 5, 5).unwrap(), "=C1!F6");
        assert_eq!(r1c1::rewrite("=R1C1!R[1]C", 5, 5).unwrap(), "=R1C1!F7");
        assert_eq!(r1c1::rewrite("=C1!R1C1", 5, 5).unwrap(), "=C1!$A$1");

        assert_eq!(
            r1c1::rewrite("=SUM(R1C1: $L$6)", 5, 5).unwrap(),
            "=SUM(\u{1}R1C1: $L$6)"
        );
        assert!(r1c1::rewrite("=SUM(R1C1 : $L$6)", 5, 5).is_err());
        assert!(r1c1::rewrite("=SUM(R1C1:R2C2)", 5, 5)
            .unwrap()
            .contains("$B$2"));
        assert_eq!(r1c1::rewrite("=RC.", 5, 5).unwrap(), "=RC.");
        assert_eq!(r1c1::rewrite("=R1C1.x", 5, 5).unwrap(), "=$A$1.x");
        assert!(r1c1::rewrite("=RC[1].", 5, 5).is_err());
        assert!(r1c1::rewrite("=R1C1.", 5, 5).is_err());
    }

    #[test]
    fn let_binders_stay_names_and_free_words_become_references() {
        assert_eq!(r1c1::rewrite("=LET(x,1,c)", 5, 5).unwrap(), "=LET(x,1,F:F)");
        assert_eq!(r1c1::rewrite("=LET(x,1,RC)", 5, 5).unwrap(), "=LET(x,1,F6)");
        assert_eq!(r1c1::rewrite("=LET(c,1,c)", 5, 5).unwrap(), "=LET(c,1,c)");
        let file_form = "=_xlfn.LET(_xlpm.r,1,_xlpm.r+c)";
        assert_eq!(
            r1c1::rewrite(file_form, 5, 5).unwrap(),
            "=_xlfn.LET(_xlpm.r,1,_xlpm.r+F:F)"
        );
        assert_eq!(
            r1c1::rewrite("=LAMBDA(a,[c],c+a)", 5, 5).unwrap(),
            "=LAMBDA(a,[c],c+a)"
        );
        assert_eq!(
            r1c1::rewrite("=LET(x,1,LET(rc,2,rc+x))", 5, 5).unwrap(),
            "=LET(x,1,LET(rc,2,rc+x))"
        );
    }

    #[test]
    fn names() {
        assert!(is_valid_name("rate"));
        assert!(is_valid_name("_x.y"));
        assert!(!is_valid_name("A1"));
        assert!(!is_valid_name("R1C1"));
        assert!(!is_valid_name("TRUE"));
        assert!(!is_valid_name("1abc"));
        assert!(!is_valid_name("_xlfoo"));

        assert!(is_table_name("_xlfoo"));
    }

    #[test]
    fn names_from_labels() {
        for (label, name) in [
            ("_xlfoo", Some("__xlfoo")),
            ("_x", Some("_x")),
            ("abc1", Some("abc1_")),
            ("R1C1", Some("_R1C1")),
            ("R1", Some("_R1")),
            ("R1C", Some("_R1C")),
            ("r", Some("r_")),
            ("RC", Some("RC_")),
            ("xfd1", Some("xfd1_")),
            ("xfe1", Some("xfe1")),
            ("R1.C1", Some("R1.C1_")),
            ("1abc", Some("_1abc")),
            ("a b", Some("a_b")),
            ("TRUE", None),
        ] {
            assert_eq!(name_from_label(label).as_deref(), name, "{label}");
        }
    }
}
