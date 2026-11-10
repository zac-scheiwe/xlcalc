pub mod ast;
pub mod compile;
mod frame;
pub mod host;
mod hresult_text;
pub(crate) mod interp;
pub mod ir;
pub mod lexer;
pub mod parser;
mod precomp;
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
            3 => "Return without GoSub",
            5 => "Invalid procedure call or argument",
            6 => "Overflow",
            7 => "Out of memory",
            9 => "Subscript out of range",
            10 => "This array is fixed or temporarily locked",
            11 => "Division by zero",
            13 => "Type mismatch",
            14 => "Out of string space",
            16 => "Expression too complex",
            17 => "Can't perform requested operation",
            18 => "User interrupt occurred",
            20 => "Resume without error",
            28 => "Out of stack space",
            35 => "Sub or Function not defined",
            47 => "Too many DLL application clients",
            48 => "Error in loading DLL",
            49 => "Bad DLL calling convention",
            51 => "Internal error",
            52 => "Bad file name or number",
            53 => "File not found",
            54 => "Bad file mode",
            55 => "File already open",
            57 => "Device I/O error",
            58 => "File already exists",
            59 => "Bad record length",
            61 => "Disk full",
            62 => "Input past end of file",
            63 => "Bad record number",
            67 => "Too many files",
            68 => "Device unavailable",
            70 => "Permission denied",
            71 => "Disk not ready",
            74 => "Can't rename with different drive",
            75 => "Path/File access error",
            76 => "Path not found",
            91 => "Object variable or With block variable not set",
            92 => "For loop not initialized",
            93 => "Invalid pattern string",
            94 => "Invalid use of Null",
            96 => "Unable to sink events of object because the object is already firing events to the maximum number of event receivers that it supports",
            97 => "Can not call friend function on object which is not an instance of defining class",
            98 => "A property or method call cannot include a reference to a private object, either as an argument or as a return value",
            321 => "Invalid file format",
            322 => "Can't create necessary temporary file",
            325 => "Invalid format in resource file",
            380 => "Invalid property value",
            381 => "Invalid property array index",
            382 => "Set not supported at runtime",
            383 => "Set not supported (read-only property)",
            385 => "Need property array index",
            387 => "Set not permitted",
            393 => "Get not supported at runtime",
            394 => "Get not supported (write-only property)",
            422 => "Property not found",
            423 => "Property or method not found",
            424 => "Object required",
            429 => "ActiveX component can't create object",
            430 => "Class does not support Automation or does not support expected interface",
            432 => "File name or class name not found during Automation operation",
            438 => "Object doesn't support this property or method",
            440 => "Automation error",
            442 => "Connection to type library or object library for remote process has been lost. Press OK for dialog to remove reference.",
            443 => "Automation object does not have a default value",
            445 => "Object doesn't support this action",
            446 => "Object doesn't support named arguments",
            447 => "Object doesn't support current locale setting",
            448 => "Named argument not found",
            449 => "Argument not optional",
            450 => "Wrong number of arguments or invalid property assignment",
            451 => "Property let procedure not defined and property get procedure did not return an object",
            452 => "Invalid ordinal",
            453 => "Specified DLL function not found",
            454 => "Code resource not found",
            455 => "Code resource lock error",
            457 => "This key is already associated with an element of this collection",
            458 => "Variable uses an Automation type not supported in Visual Basic",
            459 => "Object or class does not support the set of events",
            460 => "Invalid clipboard format",
            461 => "Method or data member not found",
            462 => "The remote server machine does not exist or is unavailable",
            463 => "Class not registered on local machine",
            481 => "Invalid picture",
            482 => "Printer error",
            735 => "Can't save file to TEMP",
            744 => "Search text not found",
            746 => "Replacements too long",
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
