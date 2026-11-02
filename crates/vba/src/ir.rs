use crate::ast::{Pos, ProcKind, UnOp};
use crate::rt::Builtin;
use crate::types::{BinOp, VType};
use xlcalc_variant::Variant;

pub type ProcId = usize;

#[derive(Clone, Copy)]
pub struct BuiltinRef(pub &'static Builtin);

impl std::fmt::Debug for BuiltinRef {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.write_str(self.0.name)
    }
}

#[derive(Clone, Copy, Debug, PartialEq, Eq, Hash)]
pub enum VarRef {
    Local(usize),
    Global(usize),
    Field(usize),
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct SType {
    pub ty: VType,
    pub array: bool,
}

impl SType {
    pub fn scalar(ty: VType) -> SType {
        SType { ty, array: false }
    }
}

#[derive(Clone, Debug)]
pub struct CExpr {
    pub st: SType,
    pub kind: EK,
}

impl CExpr {
    pub fn ty(&self) -> &VType {
        &self.st.ty
    }
}

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum ErrProp {
    Number,
    Description,
    Source,
}

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum Intrinsic {
    Array,
    IsMissing,
    IsArray,
    IsObject,
    TypeName,

    StrPtr,
    VarType,
    UBound,
    LBound,

    Ask(crate::host::Interaction),
}

#[derive(Clone, Debug)]
pub enum EK {
    Lit(Variant),
    Const(usize),
    Var(VarRef),

    Elem(VarRef, Vec<CExpr>),
    Call(ProcId, Vec<CArg>),
    Builtin(BuiltinRef, Vec<CExpr>),
    Intrinsic(Intrinsic, Vec<CExpr>),
    Unary(UnOp, Box<CExpr>),
    Binary(BinOp, Box<CExpr>, Box<CExpr>),

    Member(Box<CExpr>, String, Vec<NamedArg>),

    Index(Box<CExpr>, Vec<NamedArg>),

    Global(String, Vec<NamedArg>),
    Err(ErrProp),
    TypeOf(Box<CExpr>, String),

    Missing,

    New(usize),

    AutoNew(VarRef, usize),

    Me,

    Document(usize),
}

#[derive(Clone, Debug)]
pub struct NamedArg {
    pub name: Option<String>,
    pub value: CExpr,

    pub place: Option<Place>,
}

#[derive(Clone, Debug)]
pub enum CArg {
    Value(CExpr),

    Ref(Place),
    Missing,

    Rest(Vec<CExpr>),
}

#[derive(Clone, Debug)]
pub enum Place {
    Var(VarRef),
    Elem(VarRef, Vec<CExpr>),
    Member(Box<CExpr>, String, Vec<NamedArg>),
    Index(Box<CExpr>, Vec<NamedArg>),
    Global(String, Vec<NamedArg>),
    Err(ErrProp),
}

#[derive(Clone, Debug)]
pub enum OnErrorMode {
    Disable,
    Reset,
    ResumeNext,
    GoTo(usize),
}

#[derive(Clone, Debug)]
pub enum ResumeKind {
    Again,
    Next,
    Label(usize),
}

#[derive(Clone, Debug)]
pub enum Op {
    Let(Place, CExpr),

    Set(Place, CExpr),

    Bind(usize, CExpr),
    Eval(CExpr),
    Jump(usize),
    JumpIfNot(CExpr, usize),
    JumpIf(CExpr, usize),

    ForInit {
        var: Place,
        from: CExpr,
        to: CExpr,
        step: Option<CExpr>,
        hidden: usize,
        exit: usize,
    },

    ForNext {
        var: Place,
        counter: VType,
        hidden: usize,
        body: usize,
    },

    ForEachInit {
        var: Place,
        collection: CExpr,
        hidden: usize,
        exit: usize,
    },
    ForEachNext {
        var: Place,
        hidden: usize,
        body: usize,
    },
    ReDim {
        var: VarRef,
        preserve: bool,
        bounds: Vec<(CExpr, CExpr)>,
        elem: VType,
    },
    Erase(VarRef),
    OnError(OnErrorMode),
    Resume(ResumeKind),
    ErrClear,
    ErrRaise(Vec<NamedArg>),
    Return,

    GoSub(usize),

    GoSubReturn,

    OnGoTo {
        index: CExpr,
        targets: Vec<usize>,
        gosub: bool,
    },

    LSet {
        place: Place,
        value: CExpr,
        right: bool,
    },
    DebugPrint(Vec<PrintItem>),
    DebugAssert(CExpr),

    RaiseEvent {
        event: usize,
        args: Vec<CArg>,
    },

    End,
    Stop,
}

#[derive(Clone, Debug)]
pub enum PrintItem {
    Value(CExpr),

    Zone,

    Join,
}

#[derive(Clone, Debug)]
pub enum ArrayDecl {
    Scalar,
    Dynamic,

    Fixed(Vec<(CExpr, CExpr)>),
}

#[derive(Clone, Debug)]
pub struct LocalInfo {
    pub name: String,
    pub ty: VType,
    pub array: ArrayDecl,

    pub fixed: Option<u32>,

    pub auto_new: Option<usize>,
}

#[derive(Clone, Debug)]
pub struct ParamInfo {
    pub name: String,
    pub ty: VType,
    pub array: bool,
    pub by_val: bool,
    pub optional: bool,
    pub param_array: bool,
    pub default: Option<CExpr>,
}

#[derive(Clone, Debug)]
pub struct ProcCode {
    pub name: String,
    pub module: usize,
    pub kind: ProcKind,
    pub public: bool,
    pub friend: bool,
    pub pos: Pos,
    pub params: Vec<ParamInfo>,
    pub ret: SType,

    pub locals: Vec<LocalInfo>,
    pub ops: Vec<Op>,

    pub stmt_start: Vec<bool>,

    pub volatile: bool,
}

#[derive(Clone, Debug)]
pub struct GlobalInfo {
    pub name: String,
    pub ty: VType,
    pub array: ArrayDecl,

    pub fixed: Option<u32>,

    pub auto_new: Option<usize>,

    pub public: bool,

    pub with_events: Option<usize>,
}

#[derive(Clone, Debug)]
pub struct ConstInfo {
    pub name: String,
    pub st: SType,
    pub value: CExpr,
}

pub const COLLECTION_CLASS: usize = usize::MAX;
