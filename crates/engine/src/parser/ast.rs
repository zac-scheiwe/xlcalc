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

#[derive(Clone, Debug, PartialEq)]
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
