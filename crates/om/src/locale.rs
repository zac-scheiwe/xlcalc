use xlcalc_variant::HResult;

use crate::error::{OmError, SCODE_UNKNOWN_LCID};
use crate::typelib::{InvokeKind, Member};

pub(crate) fn english(lcid: u32) -> bool {
    matches!(lcid, 0 | 0x400 | 0x800) || lcid & 0x3FF == 0x09
}

#[derive(Clone, Copy, PartialEq, Eq, Debug)]
pub(crate) enum Foreign {
    Ignored,

    Fails,

    Raises,

    Refuses,

    RefusesByName,

    EmptyText,
}

impl Foreign {
    pub(crate) fn error(self) -> Option<OmError> {
        match self {
            Foreign::Fails => Some(OmError::hresult(HResult(SCODE_UNKNOWN_LCID))),
            Foreign::Raises => Some(OmError::exception(SCODE_UNKNOWN_LCID, None)),
            Foreign::Refuses | Foreign::RefusesByName => Some(OmError::app_defined()),
            Foreign::Ignored | Foreign::EmptyText => None,
        }
    }
}

pub(crate) fn foreign(interface: &str, m: &Member) -> Foreign {
    use Foreign::*;
    use InvokeKind::*;
    let put = matches!(m.kind, Put | PutRef);
    match (interface, m.name) {
        (
            "_Application",
            "Calculation" | "Iteration" | "MaxIterations" | "MaxChange" | "DisplayAlerts"
            | "ScreenUpdating" | "Calculate",
        ) => Fails,
        ("_Workbook", "FullName" | "Path" | "Saved" | "Activate") => Fails,
        ("_Worksheet", "UsedRange" | "Visible" | "Calculate" | "Activate") => Fails,
        ("_Worksheet", "Name") if put => Raises,
        ("Range", "HasFormula" | "NumberFormat" | "Text" | "Calculate") => Fails,
        ("Range", "Value" | "Value2" | "Formula" | "Formula2" | "FormulaR1C1" | "_Default") => {
            if put {
                Refuses
            } else {
                Raises
            }
        }
        ("Range", "Address") => EmptyText,
        ("Name", "Name" | "RefersTo" | "Value" | "RefersToR1C1" | "_Default") => Raises,
        ("Names", "Item" | "_Default") => RefusesByName,
        _ => Ignored,
    }
}
