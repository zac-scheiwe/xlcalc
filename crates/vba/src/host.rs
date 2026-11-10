use crate::VbaError;
use xlcalc_variant::{ObjectId, Variant};

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum InvokeKind {
    Get,

    Put,

    PutRef,
}

pub trait ObjectHost {
    fn global(&mut self) -> Result<ObjectId, VbaError>;

    fn invoke(
        &mut self,
        obj: ObjectId,
        member: &str,
        kind: InvokeKind,
        positional: &[Variant],
        named: &[(&str, Variant)],
    ) -> Result<Variant, VbaError>;

    fn type_name(&mut self, obj: ObjectId) -> String {
        let _ = obj;
        "Object".to_string()
    }

    fn enumerate(&mut self, obj: ObjectId) -> Result<Vec<Variant>, VbaError> {
        let _ = obj;
        Err(VbaError::new(438))
    }

    fn document(&mut self, module: &str) -> Result<ObjectId, VbaError> {
        let _ = module;
        Err(VbaError::new(424))
    }

    fn debug_print(&mut self, items: &[DebugItem]) {
        let _ = items;
    }

    fn debug_assert(&mut self) {}

    fn dll_call(&mut self, lib: &str, name: &str, args: &[Variant]) -> Result<Variant, VbaError> {
        let _ = (lib, name, args);
        Err(VbaError::new(453))
    }

    fn create_object(&mut self, progid: &str) -> Result<ObjectId, VbaError> {
        let _ = progid;
        Err(VbaError::new(429))
    }

    fn get_object(
        &mut self,
        path: Option<&str>,
        class: Option<&str>,
    ) -> Result<ObjectId, VbaError> {
        Err(VbaError::new(if path.is_some() && class.is_none() {
            432
        } else {
            429
        }))
    }

    fn interaction(&mut self, what: Interaction, args: &[Variant]) -> Result<Variant, VbaError> {
        let _ = args;
        Err(VbaError {
            number: 438,
            description: format!("not supported by xlcalc: {what:?}"),
        })
    }
}

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum Interaction {
    MsgBox,

    InputBox,

    Beep,
}

#[derive(Clone, Debug, PartialEq)]
pub enum DebugItem {
    Value(Variant),

    Zone,

    Join,
}

pub struct NoHost;

impl ObjectHost for NoHost {
    fn global(&mut self) -> Result<ObjectId, VbaError> {
        Err(VbaError::new(424))
    }

    fn invoke(
        &mut self,
        _: ObjectId,
        _: &str,
        _: InvokeKind,
        _: &[Variant],
        _: &[(&str, Variant)],
    ) -> Result<Variant, VbaError> {
        Err(VbaError::new(424))
    }
}

pub trait Library {
    fn constant(&self, name: &str) -> Option<Variant>;

    fn is_global(&self, name: &str) -> bool;
}

pub struct ExcelNames;

const GLOBAL_MEMBERS: &[&str] = &[
    "ActiveCell",
    "ActiveSheet",
    "ActiveWindow",
    "ActiveWorkbook",
    "Application",
    "Calculate",
    "Cells",
    "Columns",
    "Evaluate",
    "Intersect",
    "Names",
    "Range",
    "Rows",
    "Selection",
    "Sheets",
    "ThisWorkbook",
    "Union",
    "Windows",
    "Workbooks",
    "Worksheets",
    "WorksheetFunction",
];

const CONSTANTS: &[(&str, i32)] = &[
    ("xlErrDiv0", 2007),
    ("xlErrNA", 2042),
    ("xlErrName", 2029),
    ("xlErrNull", 2000),
    ("xlErrNum", 2036),
    ("xlErrRef", 2023),
    ("xlErrValue", 2015),
];

impl Library for ExcelNames {
    fn constant(&self, name: &str) -> Option<Variant> {
        CONSTANTS
            .iter()
            .find(|(n, _)| n.eq_ignore_ascii_case(name))
            .map(|&(_, v)| Variant::I4(v))
    }

    fn is_global(&self, name: &str) -> bool {
        GLOBAL_MEMBERS.iter().any(|n| n.eq_ignore_ascii_case(name))
    }
}
