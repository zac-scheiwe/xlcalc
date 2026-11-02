use crate::value::XlError;

#[derive(Clone, Debug, PartialEq)]
pub enum SheetSpec {
    Single(String),

    Range(String, String),

    External { book: String, sheet: Option<String> },
}

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub struct Coord {
    pub index: u32,
    pub abs: bool,
}

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum RefKind {
    Cell {
        row: Coord,
        col: Coord,
    },
    Area {
        r0: Coord,
        c0: Coord,
        r1: Coord,
        c1: Coord,
    },

    Cols {
        c0: Coord,
        c1: Coord,
    },

    Rows {
        r0: Coord,
        r1: Coord,
    },
}

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum UnaryOp {
    Neg,
    Plus,

    Implicit,

    ImplicitQuiet,

    ArrayZero,
}

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum BinOp {
    Add,
    Sub,
    Mul,
    Div,
    Pow,
    Concat,
    Eq,
    Ne,
    Lt,
    Le,
    Gt,
    Ge,

    Range,

    Intersect,

    Union,
}

impl BinOp {
    pub fn symbol(self) -> &'static str {
        match self {
            BinOp::Add => "+",
            BinOp::Sub => "-",
            BinOp::Mul => "*",
            BinOp::Div => "/",
            BinOp::Pow => "^",
            BinOp::Concat => "&",
            BinOp::Eq => "=",
            BinOp::Ne => "<>",
            BinOp::Lt => "<",
            BinOp::Le => "<=",
            BinOp::Gt => ">",
            BinOp::Ge => ">=",
            BinOp::Range => ":",
            BinOp::Intersect => " ",
            BinOp::Union => ",",
        }
    }

    pub fn precedence(self) -> u8 {
        match self {
            BinOp::Range => 10,
            BinOp::Intersect => 9,
            BinOp::Union => 8,
            BinOp::Pow => 5,
            BinOp::Mul | BinOp::Div => 4,
            BinOp::Add | BinOp::Sub => 3,
            BinOp::Concat => 2,
            _ => 1,
        }
    }
}

#[derive(Clone, Debug, PartialEq)]
pub enum ArrayItem {
    Number(f64),
    Text(String),
    Bool(bool),
    Error(XlError),
}

#[derive(Debug)]
pub enum Expr {
    Number(f64),
    Text(String),
    Bool(bool),
    Error(XlError),
    Array(Vec<Vec<ArrayItem>>),
    Ref {
        sheet: Option<SheetSpec>,
        kind: RefKind,
    },
    Name {
        sheet: Option<SheetSpec>,
        name: String,
    },

    StructRef {
        table: Option<String>,
        spec: String,
    },

    Func {
        name: String,
        args: Vec<Expr>,
    },

    Call {
        callee: Box<Expr>,
        args: Vec<Expr>,
    },
    Unary {
        op: UnaryOp,
        expr: Box<Expr>,
    },
    Percent(Box<Expr>),

    Spill(Box<Expr>),

    Field {
        expr: Box<Expr>,
        field: String,
    },
    Binary {
        op: BinOp,
        left: Box<Expr>,
        right: Box<Expr>,
    },
    Paren(Box<Expr>),

    Missing,

    Ws {
        before: String,
        after: String,
        expr: Box<Expr>,
    },
}

impl Expr {
    pub fn unwrap_ws(&self) -> &Expr {
        match self {
            Expr::Ws { expr, .. } => expr.unwrap_ws(),
            e => e,
        }
    }

    pub fn with_ws(self, before: &str, after: &str) -> Expr {
        if before.is_empty() && after.is_empty() {
            return self;
        }
        match self {
            Expr::Ws {
                before: b,
                after: a,
                expr,
            } => Expr::Ws {
                before: format!("{before}{b}"),
                after: format!("{a}{after}"),
                expr,
            },
            e => Expr::Ws {
                before: before.to_string(),
                after: after.to_string(),
                expr: Box::new(e),
            },
        }
    }
}

impl Clone for Expr {
    fn clone(&self) -> Expr {
        crate::deep(|| self.clone_inner())
    }
}

impl Expr {
    fn clone_inner(&self) -> Expr {
        match self {
            Expr::Number(n) => Expr::Number(*n),
            Expr::Text(s) => Expr::Text(s.clone()),
            Expr::Bool(b) => Expr::Bool(*b),
            Expr::Error(e) => Expr::Error(*e),
            Expr::Array(a) => Expr::Array(a.clone()),
            Expr::Ref { sheet, kind } => Expr::Ref {
                sheet: sheet.clone(),
                kind: *kind,
            },
            Expr::Name { sheet, name } => Expr::Name {
                sheet: sheet.clone(),
                name: name.clone(),
            },
            Expr::StructRef { table, spec } => Expr::StructRef {
                table: table.clone(),
                spec: spec.clone(),
            },
            Expr::Func { name, args } => Expr::Func {
                name: name.clone(),
                args: args.clone(),
            },
            Expr::Call { callee, args } => Expr::Call {
                callee: callee.clone(),
                args: args.clone(),
            },
            Expr::Unary { op, expr } => Expr::Unary {
                op: *op,
                expr: expr.clone(),
            },
            Expr::Percent(x) => Expr::Percent(x.clone()),
            Expr::Spill(x) => Expr::Spill(x.clone()),
            Expr::Field { expr, field } => Expr::Field {
                expr: expr.clone(),
                field: field.clone(),
            },
            Expr::Binary { op, left, right } => Expr::Binary {
                op: *op,
                left: left.clone(),
                right: right.clone(),
            },
            Expr::Paren(x) => Expr::Paren(x.clone()),
            Expr::Missing => Expr::Missing,
            Expr::Ws {
                before,
                after,
                expr,
            } => Expr::Ws {
                before: before.clone(),
                after: after.clone(),
                expr: expr.clone(),
            },
        }
    }
}

impl PartialEq for Expr {
    fn eq(&self, other: &Expr) -> bool {
        crate::deep(|| self.eq_inner(other))
    }
}

impl Expr {
    fn eq_inner(&self, other: &Expr) -> bool {
        match (self, other) {
            (Expr::Number(a), Expr::Number(b)) => a == b,
            (Expr::Text(a), Expr::Text(b)) => a == b,
            (Expr::Bool(a), Expr::Bool(b)) => a == b,
            (Expr::Error(a), Expr::Error(b)) => a == b,
            (Expr::Array(a), Expr::Array(b)) => a == b,
            (
                Expr::Ref {
                    sheet: s1,
                    kind: k1,
                },
                Expr::Ref {
                    sheet: s2,
                    kind: k2,
                },
            ) => s1 == s2 && k1 == k2,
            (
                Expr::Name {
                    sheet: s1,
                    name: n1,
                },
                Expr::Name {
                    sheet: s2,
                    name: n2,
                },
            ) => s1 == s2 && n1 == n2,
            (
                Expr::StructRef {
                    table: t1,
                    spec: s1,
                },
                Expr::StructRef {
                    table: t2,
                    spec: s2,
                },
            ) => t1 == t2 && s1 == s2,
            (Expr::Func { name: n1, args: a1 }, Expr::Func { name: n2, args: a2 }) => {
                n1 == n2 && a1 == a2
            }
            (
                Expr::Call {
                    callee: c1,
                    args: a1,
                },
                Expr::Call {
                    callee: c2,
                    args: a2,
                },
            ) => c1 == c2 && a1 == a2,
            (Expr::Unary { op: o1, expr: x1 }, Expr::Unary { op: o2, expr: x2 }) => {
                o1 == o2 && x1 == x2
            }
            (Expr::Percent(a), Expr::Percent(b))
            | (Expr::Spill(a), Expr::Spill(b))
            | (Expr::Paren(a), Expr::Paren(b)) => a == b,
            (
                Expr::Field {
                    expr: x1,
                    field: f1,
                },
                Expr::Field {
                    expr: x2,
                    field: f2,
                },
            ) => x1 == x2 && f1 == f2,
            (
                Expr::Binary {
                    op: o1,
                    left: l1,
                    right: r1,
                },
                Expr::Binary {
                    op: o2,
                    left: l2,
                    right: r2,
                },
            ) => o1 == o2 && l1 == l2 && r1 == r2,
            (Expr::Missing, Expr::Missing) => true,
            (
                Expr::Ws {
                    before: b1,
                    after: a1,
                    expr: x1,
                },
                Expr::Ws {
                    before: b2,
                    after: a2,
                    expr: x2,
                },
            ) => b1 == b2 && a1 == a2 && x1 == x2,
            _ => false,
        }
    }
}

impl Expr {
    pub fn drop_deep(self) {
        let mut todo = vec![self];
        while let Some(e) = todo.pop() {
            match e {
                Expr::Func { args, .. } => todo.extend(args),
                Expr::Call { callee, args } => {
                    todo.push(*callee);
                    todo.extend(args);
                }
                Expr::Unary { expr, .. } | Expr::Field { expr, .. } | Expr::Ws { expr, .. } => {
                    todo.push(*expr)
                }
                Expr::Percent(x) | Expr::Spill(x) | Expr::Paren(x) => todo.push(*x),
                Expr::Binary { left, right, .. } => {
                    todo.push(*left);
                    todo.push(*right);
                }
                _ => {}
            }
        }
    }
}
