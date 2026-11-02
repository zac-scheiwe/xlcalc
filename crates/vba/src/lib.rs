pub mod ast;
pub mod compile;
pub mod host;
pub(crate) mod interp;
pub mod ir;
pub mod lexer;
pub mod parser;
pub mod rt;
pub mod types;
pub(crate) mod udt;

pub use compile::{ModuleKind, SourceModule};
pub use host::{DebugItem, ExcelNames, Interaction, InvokeKind, Library, NoHost, ObjectHost};
pub use interp::{is_instance, MacroLookup, ParamSig, ProcSignature, Project, Settings, Sink};
pub use types::VType;

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct VbaError {
    pub number: i32,
    pub description: String,
}

impl VbaError {
    pub fn new(number: i32) -> VbaError {
        let description = match number {
            5 => "Invalid procedure call or argument",
            6 => "Overflow",
            7 => "Out of memory",
            9 => "Subscript out of range",
            10 => "This array is fixed or temporarily locked",
            11 => "Division by zero",
            13 => "Type mismatch",
            20 => "Resume without error",
            28 => "Out of stack space",
            35 => "Sub or Function not defined",
            51 => "Internal error",
            91 => "Object variable or With block variable not set",
            94 => "Invalid use of Null",
            424 => "Object required",
            438 => "Object doesn't support this property or method",
            448 => "Named argument not found",
            449 => "Argument not optional",
            450 => "Wrong number of arguments or invalid property assignment",
            _ => "Application-defined or object-defined error",
        };
        VbaError {
            number,
            description: description.to_string(),
        }
    }
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct CompileError {
    pub module: String,
    pub line: u32,
    pub col: u32,
    pub message: String,
}

impl std::fmt::Display for CompileError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(
            f,
            "{}:{}:{}: {}",
            self.module, self.line, self.col, self.message
        )
    }
}
