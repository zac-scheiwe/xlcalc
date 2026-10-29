use std::sync::Arc;

use crate::address::{SheetId, MAX_COLS, MAX_ROWS};
use crate::functions::{self, FuncId};
use crate::parser::ast::{self, ArrayItem, BinOp, Expr, RefKind, SheetSpec, UnaryOp};
use crate::value::{Array, Text, Value, XlError};

#[derive(Clone, Copy, Debug, PartialEq, Eq, Hash)]
pub struct Rc {
    pub v: i32,
    pub abs: bool,
}

impl Rc {
    #[inline]
    pub fn resolve(self, host: u32, max: u32, wrap: bool) -> Option<u32> {
        if self.abs {
            return Some(self.v as u32);
        }
        let x = host as i64 + self.v as i64;
        if wrap {
            Some(x.rem_euclid(max as i64) as u32)
        } else if (0..max as i64).contains(&x) {
            Some(x as u32)
        } else {
            None
        }
    }
}

#[derive(Clone, Copy, Debug, PartialEq, Eq, Hash)]
pub enum CRef {
    Cell { row: Rc, col: Rc },
    Area { r0: Rc, c0: Rc, r1: Rc, c1: Rc },
    Cols { c0: Rc, c1: Rc },
    Rows { r0: Rc, r1: Rc },
}

#[derive(Clone, Debug, PartialEq, Eq, Hash)]
pub enum SheetRef {
    Current,
    Sheet(SheetId),

    Span(SheetId, SheetId),

    Missing(Text),

    External(Text),
}

#[derive(Debug)]
pub struct LambdaDef {
    pub params: Vec<Text>,

    pub required: usize,
    pub base: u32,
    pub body: Node,
}

#[derive(Debug)]
pub enum Node {
    Lit(Value),
    Ref(SheetRef, CRef),

    Name {
        key: Text,
        display: Text,
        sheet: Option<SheetRef>,
    },

    Local {
        slot: u32,
        name: Text,
    },
    Unary(UnaryOp, Box<Node>),
    Percent(Box<Node>),
    Spill(Box<Node>),
    Binary(BinOp, Box<Node>, Box<Node>),
    Paren(Box<Node>),
    Func {
        id: FuncId,
        args: Vec<Node>,
    },

    NamedCall {
        key: Text,
        display: Text,
        args: Vec<Node>,
    },

    Call {
        callee: Box<Node>,
        args: Vec<Node>,
    },
    Let {
        names: Vec<Text>,
        values: Vec<Node>,
        body: Box<Node>,
        base: u32,
    },
    Lambda(Arc<LambdaDef>),
    Missing,

    Unsupported {
        text: Text,
        error: XlError,
    },

    DeadRef(SheetRef),

    StructRef {
        table: Text,
        implicit: bool,
        spec: crate::workbook::tables::Spec,
    },

    Ws {
        before: Text,
        after: Text,
        node: Box<Node>,
    },
}

impl Clone for Node {
    fn clone(&self) -> Node {
        crate::deep(|| self.clone_inner())
    }
}

impl Node {
    fn clone_inner(&self) -> Node {
        match self {
            Node::Lit(v) => Node::Lit(v.clone()),
            Node::Ref(s, c) => Node::Ref(s.clone(), *c),
            Node::Name {
                key,
                display,
                sheet,
            } => Node::Name {
                key: key.clone(),
                display: display.clone(),
                sheet: sheet.clone(),
            },
            Node::Local { slot, name } => Node::Local {
                slot: *slot,
                name: name.clone(),
            },
            Node::Unary(op, x) => Node::Unary(*op, x.clone()),
            Node::Percent(x) => Node::Percent(x.clone()),
            Node::Spill(x) => Node::Spill(x.clone()),
            Node::Binary(op, a, b) => Node::Binary(*op, a.clone(), b.clone()),
            Node::Paren(x) => Node::Paren(x.clone()),
            Node::Func { id, args } => Node::Func {
                id: *id,
                args: args.clone(),
            },
            Node::NamedCall { key, display, args } => Node::NamedCall {
                key: key.clone(),
                display: display.clone(),
                args: args.clone(),
            },
            Node::Call { callee, args } => Node::Call {
                callee: callee.clone(),
                args: args.clone(),
            },
            Node::Let {
                names,
                values,
                body,
                base,
            } => Node::Let {
                names: names.clone(),
                values: values.clone(),
                body: body.clone(),
                base: *base,
            },
            Node::Lambda(d) => Node::Lambda(d.clone()),
            Node::Missing => Node::Missing,
            Node::Unsupported { text, error } => Node::Unsupported {
                text: text.clone(),
                error: *error,
            },
            Node::DeadRef(s) => Node::DeadRef(s.clone()),
            Node::StructRef {
                table,
                implicit,
                spec,
            } => Node::StructRef {
                table: table.clone(),
                implicit: *implicit,
                spec: spec.clone(),
            },
            Node::Ws {
                before,
                after,
                node,
            } => Node::Ws {
                before: before.clone(),
                after: after.clone(),
                node: node.clone(),
            },
        }
    }
}

impl Node {
    pub fn unwrap_ws(&self) -> &Node {
        match self {
            Node::Ws { node, .. } => node.unwrap_ws(),
            n => n,
        }
    }
}

#[derive(Debug)]
pub struct Formula {
    pub root: Node,

    pub volatile: bool,
}

pub trait SheetResolver {
    fn sheet_id(&self, name: &str) -> Option<SheetId>;

    fn table(&self, name: Option<&str>) -> Option<(String, Vec<String>)>;

    fn name_exists(&self, _name: &str) -> bool {
        false
    }
}

pub struct CompileCtx<'a> {
    pub sheets: &'a dyn SheetResolver,
    pub host_row: u32,
    pub host_col: u32,
}

struct Compiler<'a> {
    cx: &'a CompileCtx<'a>,
    scope: Vec<Text>,
    volatile: bool,
}

#[derive(Debug, Clone, PartialEq)]
pub struct CompileError(pub String);

impl std::fmt::Display for CompileError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.write_str(&self.0)
    }
}

impl std::error::Error for CompileError {}

pub fn compile(expr: &Expr, cx: &CompileCtx) -> Result<Formula, CompileError> {
    let mut c = Compiler {
        cx,
        scope: Vec::new(),
        volatile: false,
    };
    let root = c.node(expr)?;
    Ok(Formula {
        root,
        volatile: c.volatile,
    })
}

pub fn strip_prefixes(name: &str) -> &str {
    let has = |n: &str, p: &str| n.get(..p.len()).is_some_and(|x| x.eq_ignore_ascii_case(p));
    if has(name, "_xlfn.") {
        let n = &name[6..];
        return if has(n, "_xlws.") { &n[6..] } else { n };
    }
    name
}

fn upper(s: &str) -> Text {
    s.to_uppercase().into()
}

impl<'a> Compiler<'a> {
    fn rc_row(&self, c: ast::Coord) -> Rc {
        if c.abs {
            Rc {
                v: c.index as i32,
                abs: true,
            }
        } else {
            Rc {
                v: c.index as i32 - self.cx.host_row as i32,
                abs: false,
            }
        }
    }

    fn rc_col(&self, c: ast::Coord) -> Rc {
        if c.abs {
            Rc {
                v: c.index as i32,
                abs: true,
            }
        } else {
            Rc {
                v: c.index as i32 - self.cx.host_col as i32,
                abs: false,
            }
        }
    }

    fn sheet(&self, spec: &Option<SheetSpec>) -> SheetRef {
        match spec {
            None => SheetRef::Current,
            Some(SheetSpec::Single(n)) => match self.cx.sheets.sheet_id(n) {
                Some(id) => SheetRef::Sheet(id),
                None => SheetRef::Missing(n.as_str().into()),
            },
            Some(SheetSpec::Range(a, b)) => {
                match (self.cx.sheets.sheet_id(a), self.cx.sheets.sheet_id(b)) {
                    (Some(x), Some(y)) if x == y => SheetRef::Sheet(x),
                    (Some(x), Some(y)) => SheetRef::Span(x, y),
                    _ => SheetRef::Missing(format!("{a}:{b}").into()),
                }
            }
            Some(SheetSpec::External { book, .. }) => SheetRef::External(book.as_str().into()),
        }
    }

    fn cref(&self, kind: &RefKind) -> CRef {
        let ord = |a: ast::Coord, b: ast::Coord| if a.index > b.index { (b, a) } else { (a, b) };
        let kind = &match *kind {
            RefKind::Area { r0, c0, r1, c1 } => {
                let (r0, r1) = ord(r0, r1);
                let (c0, c1) = ord(c0, c1);
                RefKind::Area { r0, c0, r1, c1 }
            }
            RefKind::Cols { c0, c1 } => {
                let (c0, c1) = ord(c0, c1);
                RefKind::Cols { c0, c1 }
            }
            RefKind::Rows { r0, r1 } => {
                let (r0, r1) = ord(r0, r1);
                RefKind::Rows { r0, r1 }
            }
            k => k,
        };
        match *kind {
            RefKind::Cell { row, col } => CRef::Cell {
                row: self.rc_row(row),
                col: self.rc_col(col),
            },

            RefKind::Area { r0, c0, r1, c1 }
                if r0.index.min(r1.index) == 0
                    && r0.index.max(r1.index) == MAX_ROWS - 1
                    && r0.abs
                    && r1.abs =>
            {
                if c0.index.min(c1.index) == 0 && c0.index.max(c1.index) == MAX_COLS - 1 {
                    CRef::Rows {
                        r0: self.rc_row(r0),
                        r1: self.rc_row(r1),
                    }
                } else {
                    CRef::Cols {
                        c0: self.rc_col(c0),
                        c1: self.rc_col(c1),
                    }
                }
            }
            RefKind::Area { r0, c0, r1, c1 }
                if c0.index.min(c1.index) == 0
                    && c0.index.max(c1.index) == MAX_COLS - 1
                    && c0.abs
                    && c1.abs =>
            {
                CRef::Rows {
                    r0: self.rc_row(r0),
                    r1: self.rc_row(r1),
                }
            }
            RefKind::Area { r0, c0, r1, c1 } => CRef::Area {
                r0: self.rc_row(r0),
                c0: self.rc_col(c0),
                r1: self.rc_row(r1),
                c1: self.rc_col(c1),
            },
            RefKind::Cols { c0, c1 } => CRef::Cols {
                c0: self.rc_col(c0),
                c1: self.rc_col(c1),
            },
            RefKind::Rows { r0, r1 } => CRef::Rows {
                r0: self.rc_row(r0),
                r1: self.rc_row(r1),
            },
        }
    }

    fn local_range_ends(&self, kind: &RefKind) -> Option<(u32, u32)> {
        let (n0, n1) = match kind {
            RefKind::Cols { c0, c1 } if !c0.abs && !c1.abs => (
                crate::address::col_to_letters(c0.index),
                crate::address::col_to_letters(c1.index),
            ),
            RefKind::Area { r0, c0, r1, c1 } if !r0.abs && !c0.abs && !r1.abs && !c1.abs => {
                (cell_name(r0.index, c0.index), cell_name(r1.index, c1.index))
            }
            _ => return None,
        };
        Some((self.local(&n0)?, self.local(&n1)?))
    }

    fn local(&self, name: &str) -> Option<u32> {
        let key = strip_prefixes(name).to_uppercase();
        self.scope
            .iter()
            .rposition(|s| s.to_uppercase() == key)
            .map(|i| i as u32)
    }

    fn field_node(&self, x: Node, field: &str) -> Node {
        Node::Func {
            id: functions::lookup(functions::info::FIELD_FN).expect("field function"),
            args: vec![x, Node::Lit(Value::text(field))],
        }
    }

    fn dotted_name(&self, e: &Expr) -> Option<String> {
        let mut segs = Vec::new();
        let mut cur = e;
        while let Expr::Field { expr, field } = cur {
            segs.push(field.as_str());
            cur = expr;
        }
        let Expr::Ref { sheet: None, kind } = cur else {
            return None;
        };
        let mut name = crate::parser::print::print(&Expr::Ref {
            sheet: None,
            kind: *kind,
        });
        for f in segs.iter().rev() {
            name.push_str(f);
        }
        self.cx.sheets.name_exists(&name).then_some(name)
    }

    fn nodes(&mut self, args: &[Expr]) -> Result<Vec<Node>, CompileError> {
        args.iter().map(|a| self.node(a)).collect()
    }

    fn node(&mut self, e: &Expr) -> Result<Node, CompileError> {
        crate::deep(|| self.node_inner(e))
    }

    fn node_inner(&mut self, e: &Expr) -> Result<Node, CompileError> {
        Ok(match e {
            Expr::Number(n) => Node::Lit(Value::Number(*n)),
            Expr::Text(s) => {
                if s.encode_utf16().count() > 255 {
                    return Err(CompileError(
                        "text values in formulas are limited to 255 characters".into(),
                    ));
                }
                Node::Lit(Value::Text(s.as_str().into()))
            }
            Expr::Bool(b) => Node::Lit(Value::Bool(*b)),
            Expr::Error(err) => Node::Lit(Value::Error(*err)),
            Expr::Array(rows) => {
                let r = rows.len() as u32;
                let c = rows[0].len() as u32;
                if rows
                    .iter()
                    .flatten()
                    .any(|it| matches!(it, ArrayItem::Text(s) if s.encode_utf16().count() > 255))
                {
                    return Err(CompileError(
                        "text values in formulas are limited to 255 characters".into(),
                    ));
                }
                let data = rows
                    .iter()
                    .flatten()
                    .map(|it| match it {
                        ArrayItem::Number(n) => Value::Number(*n),
                        ArrayItem::Text(s) => Value::Text(s.as_str().into()),
                        ArrayItem::Bool(b) => Value::Bool(*b),
                        ArrayItem::Error(e) => Value::Error(*e),
                    })
                    .collect();

                Node::Lit(Array::new(r, c, data).into_value())
            }

            Expr::Ref { sheet: None, kind } if self.local_range_ends(kind).is_some() => {
                let (l, r) = self.local_range_ends(kind).unwrap();
                let mk = |slot: u32, s: &Self| Node::Local {
                    slot,
                    name: s.scope[slot as usize].clone(),
                };
                Node::Binary(BinOp::Range, Box::new(mk(l, self)), Box::new(mk(r, self)))
            }
            Expr::Ref {
                sheet: None,
                kind: RefKind::Cell { row, col },
            } if !row.abs && !col.abs && self.local(&cell_name(row.index, col.index)).is_some() => {
                let slot = self.local(&cell_name(row.index, col.index)).unwrap();
                Node::Local {
                    slot,
                    name: self.scope[slot as usize].clone(),
                }
            }

            Expr::Ref {
                sheet: Some(SheetSpec::Range(a, b)),
                kind,
            } if self.cx.sheets.sheet_id(a).is_none()
                && self.cx.sheets.sheet_id(b).is_some()
                && self.local(a).is_some() =>
            {
                let slot = self.local(a).unwrap();
                let left = Node::Local {
                    slot,
                    name: self.scope[slot as usize].clone(),
                };
                let right = Node::Ref(
                    self.sheet(&Some(SheetSpec::Single(b.clone()))),
                    self.cref(kind),
                );
                Node::Binary(BinOp::Range, Box::new(left), Box::new(right))
            }
            Expr::Ref { sheet, kind } => Node::Ref(self.sheet(sheet), self.cref(kind)),
            Expr::Name { sheet, name } => {
                if sheet.is_none() {
                    if let Some(slot) = self.local(name) {
                        return Ok(Node::Local {
                            slot,
                            name: self.scope[slot as usize].clone(),
                        });
                    }

                    if let Some((base, rest)) = name.split_once('.') {
                        if let Some(slot) = self.local(base) {
                            let mut node = Node::Local {
                                slot,
                                name: self.scope[slot as usize].clone(),
                            };
                            for seg in rest.split('.') {
                                if seg.is_empty()
                                    || !seg.chars().all(|c| c.is_alphanumeric() || c == '_')
                                    || seg.starts_with('_')
                                {
                                    return Err(CompileError("invalid field name".into()));
                                }
                                node = self.field_node(node, &format!(".{seg}"));
                            }
                            return Ok(node);
                        }
                    }
                }
                if sheet.is_none() && crate::parser::starts_like_r1c1(name) {
                    return Err(CompileError("not a valid name".into()));
                }
                if sheet.is_none() {
                    if let Some((tname, _)) = self.cx.sheets.table(Some(name)) {
                        let spec = crate::workbook::tables::Spec {
                            items: crate::workbook::tables::Items::DATA,
                            ..Default::default()
                        };
                        return Ok(Node::StructRef {
                            table: tname.as_str().into(),
                            implicit: false,
                            spec,
                        });
                    }
                }
                let sheet = sheet.as_ref().map(|s| self.sheet(&Some(s.clone())));
                Node::Name {
                    key: upper(name),
                    display: name.as_str().into(),
                    sheet,
                }
            }
            Expr::StructRef { table, spec } => self.struct_ref(table.as_deref(), spec)?,
            Expr::Func { name, args } => self.func(name, args)?,
            Expr::Call { callee, args } => {
                if !functions::can_be_called(callee) {
                    return Err(CompileError(
                        "the called expression is not a function".into(),
                    ));
                }
                let callee = Box::new(self.node(callee)?);
                Node::Call {
                    callee,
                    args: self.nodes(args)?,
                }
            }
            Expr::Unary { op, expr } => Node::Unary(*op, Box::new(self.node(expr)?)),
            Expr::Percent(x) => Node::Percent(Box::new(self.node(x)?)),
            Expr::Spill(x) => {
                if !functions::is_reference_expr(x) {
                    return Err(CompileError("`#` needs a reference".into()));
                }
                Node::Spill(Box::new(self.node(x)?))
            }
            Expr::Field { expr, field } => {
                if let Some(name) = self.dotted_name(e) {
                    return self.node(&Expr::Name { sheet: None, name });
                }
                let x = self.node(expr)?;
                self.field_node(x, field)
            }
            Expr::Binary { op, left, right } => {
                if matches!(op, BinOp::Range | BinOp::Intersect | BinOp::Union)
                    && !(functions::is_reference_expr(left) && functions::is_reference_expr(right))
                {
                    return Err(CompileError(format!(
                        "the '{}' operator needs references",
                        op.symbol()
                    )));
                }
                Node::Binary(*op, Box::new(self.node(left)?), Box::new(self.node(right)?))
            }
            Expr::Paren(x) => Node::Paren(Box::new(self.node(x)?)),
            Expr::Missing => Node::Missing,
            Expr::Ws {
                before,
                after,
                expr,
            } => Node::Ws {
                before: before.as_str().into(),
                after: after.as_str().into(),
                node: Box::new(self.node(expr)?),
            },
        })
    }

    fn struct_ref(&mut self, table: Option<&str>, spec: &str) -> Result<Node, CompileError> {
        let mut parsed = crate::workbook::tables::parse_spec(spec).map_err(CompileError)?;
        let (tname, columns) = self.cx.sheets.table(table).ok_or_else(|| {
            CompileError(match table {
                Some(t) => format!("'{t}' is not a table"),
                None => "structured reference outside a table".into(),
            })
        })?;

        if parsed.pad != Default::default() {
            if let Some((a, _)) = &parsed.cols {
                let full = format!("{}{}{}", parsed.pad.0, a, parsed.pad.1);
                if columns.iter().any(|x| crate::text::eq_ordinal_ci(x, &full)) {
                    parsed.cols = Some((full.clone(), full));
                    parsed.pad = Default::default();
                }
            }
        }
        if let Some((a, b)) = &parsed.cols {
            let canon = |c: &str| {
                columns
                    .iter()
                    .find(|x| crate::text::eq_ordinal_ci(x, c))
                    .cloned()
                    .ok_or_else(|| CompileError(format!("table '{tname}' has no column '{c}'")))
            };
            parsed.cols = Some((canon(a)?, canon(b)?));
        }
        Ok(Node::StructRef {
            table: tname.as_str().into(),
            implicit: table.is_none(),
            spec: parsed,
        })
    }

    fn param_name(&self, e: &Expr) -> Result<Text, CompileError> {
        match e.unwrap_ws() {
            Expr::Name { sheet: None, name } => {
                let n = strip_prefixes(name);

                let ok = relaxed_param_name(n);
                if ok && !n.contains('.') {
                    Ok(n.into())
                } else {
                    Err(CompileError(format!(
                        "'{name}' is not a valid parameter name"
                    )))
                }
            }
            Expr::Ref {
                sheet: None,
                kind: RefKind::Cell { row, col },
            } if !row.abs && !col.abs => {
                let n = cell_name(row.index, col.index);
                if r1_style(&n) {
                    return Err(CompileError(format!("'{n}' is not a valid parameter name")));
                }
                Ok(n.as_str().into())
            }
            _ => Err(CompileError("expected a name".into())),
        }
    }

    fn func(&mut self, name: &str, args: &[Expr]) -> Result<Node, CompileError> {
        let mut bare = strip_prefixes(name);

        if bare.len() != name.len() {
            let u = bare.to_uppercase();
            if !(matches!(u.as_str(), "LET" | "LAMBDA" | "SINGLE")
                || functions::lookup(&u).is_some())
            {
                bare = name;
            }
        }

        let up0 = bare.to_uppercase();
        let builtin = matches!(up0.as_str(), "LET" | "LAMBDA") || functions::lookup(&up0).is_some();
        if let Some(slot) = self.local(bare).filter(|_| !builtin) {
            let callee = Box::new(Node::Local {
                slot,
                name: self.scope[slot as usize].clone(),
            });
            return Ok(Node::Call {
                callee,
                args: self.nodes(args)?,
            });
        }
        let up = bare.to_uppercase();
        match up.as_str() {
            "LET" => {
                if args.len() < 3 || args.len().is_multiple_of(2) {
                    return Err(CompileError(
                        "LET needs name/value pairs and a calculation".into(),
                    ));
                }
                let base = self.scope.len() as u32;
                let mut names = Vec::new();
                let mut values = Vec::new();
                for pair in args[..args.len() - 1].chunks(2) {
                    let n = self.param_name(&pair[0])?;
                    if names.iter().any(|q: &Text| q.eq_ignore_ascii_case(&n)) {
                        return Err(CompileError("duplicate LET name".into()));
                    }
                    let mut v = self.node(&pair[1])?;

                    if let Expr::Ws { before, .. } = &pair[0] {
                        if !before.is_empty() {
                            v = Node::Ws {
                                before: format!("\u{3}{before}\u{3}").as_str().into(),
                                after: "".into(),
                                node: Box::new(v),
                            };
                        }
                    }
                    self.scope.push(n.clone());
                    names.push(n);
                    values.push(v);
                }
                let body = self.node(&args[args.len() - 1]);
                self.scope.truncate(base as usize);
                return Ok(Node::Let {
                    names,
                    values,
                    body: Box::new(body?),
                    base,
                });
            }
            "LAMBDA" => {
                if args.is_empty() || args.len() > 254 {
                    return Err(CompileError("LAMBDA needs a calculation".into()));
                }
                let base = self.scope.len() as u32;
                let mut params = Vec::new();
                let mut required = None;
                for p in &args[..args.len() - 1] {
                    let (n, optional) = match p.unwrap_ws() {
                        Expr::StructRef { table: None, spec } => {
                            let inner = spec.trim_matches(|c| c == '[' || c == ']');
                            let e = Expr::Name {
                                sheet: None,
                                name: inner.to_string(),
                            };
                            (self.param_name(&e)?, true)
                        }
                        _ => (self.param_name(p)?, false),
                    };
                    if optional {
                        required.get_or_insert(params.len());
                    } else if required.is_some() {
                        return Err(CompileError(
                            "a required parameter follows an optional one".into(),
                        ));
                    }
                    if params.iter().any(|q: &Text| q.eq_ignore_ascii_case(&n)) {
                        return Err(CompileError("duplicate LAMBDA parameter".into()));
                    }
                    params.push(n);
                }
                let required = required.unwrap_or(params.len());
                self.scope.extend(params.iter().cloned());
                let body = if matches!(args[args.len() - 1], Expr::Missing) {
                    Err(CompileError("LAMBDA needs a calculation".into()))
                } else {
                    self.node(&args[args.len() - 1])
                };
                self.scope.truncate(base as usize);
                return Ok(Node::Lambda(Arc::new(LambdaDef {
                    params,
                    required,
                    base,
                    body: body?,
                })));
            }
            _ => {}
        }

        if up == "TABLE" {
            return Err(CompileError("That function isn't valid".into()));
        }
        let stripped_up = strip_prefixes(name).to_uppercase();
        if functions::lookup(&stripped_up).is_none()
            && functions::xlm::XLM_ONLY.contains(&&*stripped_up)
        {
            return Err(CompileError("That function isn't valid".into()));
        }
        match functions::lookup(&up) {
            Some(id) => {
                let def = functions::def(id);
                if args.len() < def.min_args as usize || args.len() > def.max_args as usize {
                    return Err(CompileError(format!(
                        "{} takes {} argument(s)",
                        def.name,
                        if def.min_args == def.max_args {
                            def.min_args.to_string()
                        } else {
                            format!("{}..{}", def.min_args, def.max_args)
                        }
                    )));
                }
                for (i, a) in args.iter().enumerate() {
                    if def.param_class(i) == functions::ParamClass::Reference
                        && !functions::is_reference_expr(a)
                    {
                        return Err(CompileError(format!(
                            "argument {} of {} must be a reference",
                            i + 1,
                            def.name
                        )));
                    }
                }
                if let Some(check) = def.check {
                    check(args).map_err(|m| CompileError(format!("{}: {m}", def.name)))?;
                }
                if def.volatile {
                    self.volatile = true;
                }
                let mut nodes = self.nodes(args)?;

                for (i, a) in args.iter().enumerate() {
                    if def.param_class(i) == functions::ParamClass::Reference
                        && matches!(a.unwrap_ws(), Expr::Field { .. })
                    {
                        mark_name_field(&mut nodes[i]);
                    }
                }
                Ok(Node::Func { id, args: nodes })
            }
            None if crate::address::parse_a1_cell(bare).is_some() => {
                let (r, c, ra, ca) = crate::address::parse_a1_cell(bare).unwrap();
                let callee = self.node(&Expr::Ref {
                    sheet: None,
                    kind: RefKind::Cell {
                        row: crate::parser::ast::Coord { index: r, abs: ra },
                        col: crate::parser::ast::Coord { index: c, abs: ca },
                    },
                })?;
                Ok(Node::Call {
                    callee: Box::new(callee),
                    args: self.nodes(args)?,
                })
            }
            None => Ok(Node::NamedCall {
                key: up.as_str().into(),
                display: bare.into(),
                args: self.nodes(args)?,
            }),
        }
    }
}

fn mark_name_field(n: &mut Node) {
    crate::deep(|| match n {
        Node::Ws { node, .. } => mark_name_field(node),
        Node::Func { id, args } if functions::def(*id).name == functions::info::FIELD_FN => {
            args.push(Node::Lit(Value::Bool(true)));
        }
        _ => {}
    })
}

fn cell_name(row: u32, col: u32) -> String {
    format!("{}{}", crate::address::col_to_letters(col), row + 1)
}

fn relaxed_param_name(n: &str) -> bool {
    let mut chars = n.chars();
    let Some(first) = chars.next() else {
        return false;
    };
    (first.is_alphabetic() || first == '_' || first == '\\')
        && n.chars()
            .all(|c| c.is_alphanumeric() || c == '_' || c == '.' || c == '\\' || c == '?')
        && !n.eq_ignore_ascii_case("TRUE")
        && !n.eq_ignore_ascii_case("FALSE")
        && !r1_style(n)
}

fn r1_style(n: &str) -> bool {
    let up = n.to_ascii_uppercase();
    let rest = up
        .strip_prefix("RC")
        .or_else(|| up.strip_prefix('R'))
        .or_else(|| up.strip_prefix('C'));
    rest.is_some_and(|d| !d.is_empty() && d.chars().all(|c| c.is_ascii_digit()))
}

pub fn map_node(node: &Node, f: &mut dyn FnMut(&Node) -> Option<Node>) -> Node {
    crate::deep(|| map_node_inner(node, f))
}

fn map_node_inner(node: &Node, f: &mut dyn FnMut(&Node) -> Option<Node>) -> Node {
    if let Some(n) = f(node) {
        return n;
    }
    let mut m = |n: &Node| map_node(n, f);
    match node {
        Node::Unary(op, x) => Node::Unary(*op, Box::new(m(x))),
        Node::Percent(x) => Node::Percent(Box::new(m(x))),
        Node::Spill(x) => Node::Spill(Box::new(m(x))),
        Node::Paren(x) => Node::Paren(Box::new(m(x))),
        Node::Binary(op, a, b) => Node::Binary(*op, Box::new(m(a)), Box::new(m(b))),
        Node::Func { id, args } => Node::Func {
            id: *id,
            args: args.iter().map(&mut m).collect(),
        },
        Node::NamedCall { key, display, args } => Node::NamedCall {
            key: key.clone(),
            display: display.clone(),
            args: args.iter().map(&mut m).collect(),
        },
        Node::Call { callee, args } => Node::Call {
            callee: Box::new(m(callee)),
            args: args.iter().map(&mut m).collect(),
        },
        Node::Let {
            names,
            values,
            body,
            base,
        } => Node::Let {
            names: names.clone(),
            values: values.iter().map(&mut m).collect(),
            body: Box::new(m(body)),
            base: *base,
        },
        Node::Lambda(def) => Node::Lambda(Arc::new(LambdaDef {
            params: def.params.clone(),
            required: def.required,
            base: def.base,
            body: m(&def.body),
        })),
        Node::Ws {
            before,
            after,
            node,
        } => Node::Ws {
            before: before.clone(),
            after: after.clone(),
            node: Box::new(m(node)),
        },
        other => other.clone(),
    }
}

pub fn walk<'n>(node: &'n Node, f: &mut dyn FnMut(&'n Node)) {
    crate::deep(|| walk_inner(node, f))
}

fn walk_inner<'n>(node: &'n Node, f: &mut dyn FnMut(&'n Node)) {
    f(node);
    match node {
        Node::Unary(_, x) | Node::Percent(x) | Node::Spill(x) | Node::Paren(x) => walk(x, f),
        Node::Ws { node, .. } => walk(node, f),
        Node::Binary(_, a, b) => {
            walk(a, f);
            walk(b, f);
        }
        Node::Func { args, .. } | Node::NamedCall { args, .. } => {
            args.iter().for_each(|a| walk(a, f))
        }
        Node::Call { callee, args } => {
            walk(callee, f);
            args.iter().for_each(|a| walk(a, f));
        }
        Node::Let { values, body, .. } => {
            values.iter().for_each(|v| walk(v, f));
            walk(body, f);
        }
        Node::Lambda(def) => walk(&def.body, f),
        Node::Lit(_)
        | Node::Ref(..)
        | Node::Name { .. }
        | Node::Local { .. }
        | Node::Missing
        | Node::Unsupported { .. }
        | Node::DeadRef(_)
        | Node::StructRef { .. } => {}
    }
}
