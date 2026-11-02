use crate::types::{BinOp, VType};
use xlcalc_variant::Variant;

#[derive(Clone, Copy, Debug, Default, PartialEq, Eq)]
pub struct Pos {
    pub line: u32,
    pub col: u32,
}

#[derive(Clone, Debug, Default)]
pub struct Options {
    pub explicit: bool,

    pub base: i32,
    pub compare_text: bool,
    pub private_module: bool,
}

#[derive(Clone, Debug, Default)]
pub struct Module {
    pub options: Options,
    pub vars: Vec<ModuleVar>,
    pub consts: Vec<ConstDecl>,
    pub enums: Vec<EnumDecl>,

    pub types: Vec<TypeDecl>,
    pub procs: Vec<Proc>,

    pub events: Vec<EventDecl>,

    pub implements: Vec<(String, Pos)>,

    pub default_member: Option<String>,
}

#[derive(Clone, Debug)]
pub struct EventDecl {
    pub name: String,
    pub pos: Pos,
    pub params: Vec<Param>,
}

#[derive(Clone, Debug)]
pub struct ModuleVar {
    pub public: bool,

    pub with_events: bool,
    pub var: VarDecl,
}

#[derive(Clone, Debug)]
pub struct VarDecl {
    pub name: String,
    pub pos: Pos,

    pub ty: Option<TypeName>,

    pub array: Option<Vec<Bounds>>,

    pub new: bool,

    pub fixed_len: Option<Expr>,
}

#[derive(Clone, Debug, PartialEq)]
pub enum TypeName {
    Builtin(VType),
    Class(String),
}

#[derive(Clone, Debug)]
pub struct Bounds {
    pub lower: Option<Expr>,
    pub upper: Expr,
}

#[derive(Clone, Debug)]
pub struct ConstDecl {
    pub public: bool,
    pub name: String,
    pub pos: Pos,
    pub ty: Option<TypeName>,
    pub value: Expr,
}

#[derive(Clone, Debug)]
pub struct EnumDecl {
    pub public: bool,
    pub name: String,
    pub pos: Pos,
    pub members: Vec<(String, Pos, Option<Expr>)>,
}

#[derive(Clone, Debug)]
pub struct TypeDecl {
    pub public: bool,
    pub name: String,
    pub pos: Pos,
    pub fields: Vec<VarDecl>,
}

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum ProcKind {
    Function,
    Sub,
    PropertyGet,
    PropertyLet,
    PropertySet,
}

impl ProcKind {
    pub fn returns_value(self) -> bool {
        matches!(self, ProcKind::Function | ProcKind::PropertyGet)
    }

    pub fn is_property(self) -> bool {
        matches!(
            self,
            ProcKind::PropertyGet | ProcKind::PropertyLet | ProcKind::PropertySet
        )
    }
}

#[derive(Clone, Debug)]
pub struct Proc {
    pub kind: ProcKind,
    pub name: String,
    pub pos: Pos,
    pub public: bool,

    pub friend: bool,

    pub is_static: bool,
    pub params: Vec<Param>,

    pub ret: Option<TypeName>,

    pub ret_array: bool,
    pub body: Vec<Stmt>,
}

#[derive(Clone, Debug)]
pub struct Param {
    pub var: VarDecl,
    pub by_val: bool,
    pub optional: bool,
    pub param_array: bool,
    pub default: Option<Expr>,
}

#[derive(Clone, Debug)]
pub struct Stmt {
    pub pos: Pos,
    pub kind: StmtKind,
}

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum ExitKind {
    Function,
    Sub,
    Property,
    For,
    Do,
}

#[derive(Clone, Debug)]
pub enum PrintArg {
    Value(Expr),

    Zone,

    Join,
}

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum LoopCond {
    While,
    Until,
}

#[derive(Clone, Debug)]
pub enum CaseClause {
    Value(Expr),
    Range(Expr, Expr),

    Is(BinOp, Expr),
}

#[derive(Clone, Debug)]
pub enum OnError {
    GoTo(String),

    Disable,

    Reset,
    ResumeNext,
}

#[derive(Clone, Debug)]
pub enum ResumeTarget {
    Again,
    Next,
    Label(String),
}

#[derive(Clone, Debug)]
pub enum StmtKind {
    Dim {
        is_static: bool,
        vars: Vec<VarDecl>,
    },
    Const(Vec<ConstDecl>),
    ReDim {
        preserve: bool,
        items: Vec<(Expr, Vec<Bounds>, Option<TypeName>)>,
    },
    Erase(Vec<Expr>),

    Assign {
        target: Expr,
        value: Expr,
        set: bool,
    },

    Call {
        callee: Expr,
        args: Vec<Arg>,
    },
    If {
        arms: Vec<(Expr, Vec<Stmt>)>,
        otherwise: Option<Vec<Stmt>>,
    },
    Select {
        subject: Expr,
        cases: Vec<(Vec<CaseClause>, Vec<Stmt>)>,
        otherwise: Option<Vec<Stmt>>,
    },
    For {
        var: Expr,
        from: Expr,
        to: Expr,
        step: Option<Expr>,
        body: Vec<Stmt>,
    },
    ForEach {
        var: Expr,
        collection: Expr,
        body: Vec<Stmt>,
    },
    Do {
        pre: Option<(LoopCond, Expr)>,
        post: Option<(LoopCond, Expr)>,
        body: Vec<Stmt>,
    },
    With {
        object: Expr,
        body: Vec<Stmt>,
    },
    Exit(ExitKind),
    GoTo(String),
    Label(String),
    OnError(OnError),
    Resume(ResumeTarget),

    DebugPrint(Vec<PrintArg>),

    DebugAssert(Expr),

    RaiseEvent {
        name: String,
        args: Vec<Arg>,
    },

    End,

    Stop,
    GoSub(String),
    Return,

    OnGoTo {
        index: Expr,
        labels: Vec<String>,
        gosub: bool,
    },

    LSet {
        target: Expr,
        value: Expr,
        right: bool,
    },
}

#[derive(Clone, Debug)]
pub struct Arg {
    pub name: Option<String>,

    pub value: Option<Expr>,
}

#[derive(Clone, Debug)]
pub struct Expr {
    pub pos: Pos,
    pub kind: ExprKind,
}

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum UnOp {
    Neg,
    Not,
}

#[derive(Clone, Debug)]
pub enum ExprKind {
    Lit(Variant),

    Name(String, Option<char>),

    Member(Option<Box<Expr>>, String),

    Index(Box<Expr>, Vec<Arg>),

    Paren(Box<Expr>),
    Unary(UnOp, Box<Expr>),
    Binary(BinOp, Box<Expr>, Box<Expr>),

    TypeOf(Box<Expr>, String),

    New(String),
    Me,
    Nothing,
    Empty,
    Null,
}
