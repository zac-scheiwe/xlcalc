use xlcalc_variant::HResult;

pub const DISP_E_MEMBERNOTFOUND: HResult = HResult(0x8002_0003_u32 as i32);
pub const DISP_E_PARAMNOTFOUND: HResult = HResult(0x8002_0004_u32 as i32);
pub const DISP_E_UNKNOWNNAME: HResult = HResult(0x8002_0006_u32 as i32);
pub const DISP_E_EXCEPTION: HResult = HResult(0x8002_0009_u32 as i32);
pub const DISP_E_BADPARAMCOUNT: HResult = HResult(0x8002_000E_u32 as i32);
pub const DISP_E_TYPEMISMATCH: HResult = HResult(0x8002_0005_u32 as i32);
pub const DISP_E_PARAMNOTOPTIONAL: HResult = HResult(0x8002_000F_u32 as i32);

pub const OBJECT_REQUIRED: HResult = HResult(0x800A_01A8_u32 as i32);
pub const RPC_E_DISCONNECTED: HResult = HResult(0x8001_0108_u32 as i32);

pub const SCODE_TYPE_MISMATCH: i32 = 0x8002_0005_u32 as i32;

pub const SCODE_BAD_INDEX: i32 = 0x8002_000B_u32 as i32;

pub const SCODE_UNKNOWN_LCID: i32 = 0x8002_8018_u32 as i32;

pub const fn vba_scode(number: i32) -> i32 {
    (0x800A_0000_u32 | number as u32) as i32
}

#[derive(Clone, Debug)]
pub struct OmError {
    pub number: i32,
    pub hresult: HResult,

    pub scode: i32,
    pub description: Option<String>,

    pub source: Option<String>,

    pub arg_err: Option<u32>,

    pub error_info: bool,

    pub info_description: Option<String>,

    pub detail: Option<String>,
}

impl PartialEq for OmError {
    fn eq(&self, other: &OmError) -> bool {
        self.number == other.number
            && self.hresult == other.hresult
            && self.scode == other.scode
            && self.description == other.description
            && self.source == other.source
            && self.arg_err == other.arg_err
            && self.error_info == other.error_info
            && self.info_description == other.info_description
    }
}

impl Eq for OmError {}

impl OmError {
    pub fn with_detail(mut self, detail: impl Into<String>) -> OmError {
        self.detail = Some(detail.into());
        self
    }

    pub fn exception(scode: i32, description: Option<&str>) -> OmError {
        OmError {
            number: if (scode as u32) >> 16 == 0x800A {
                scode & 0xFFFF
            } else {
                scode
            },
            hresult: DISP_E_EXCEPTION,
            scode,
            description: description.map(str::to_string),
            source: description.map(|_| "Microsoft Excel".to_string()),
            arg_err: None,
            error_info: false,
            info_description: None,
            detail: None,
        }
    }

    pub fn hresult(hresult: HResult) -> OmError {
        OmError {
            number: hresult.0,
            hresult,
            scode: 0,
            description: None,
            source: None,
            arg_err: None,
            error_info: false,
            info_description: None,
            detail: None,
        }
    }

    pub(crate) fn arg(hresult: HResult, index: u32) -> OmError {
        OmError {
            arg_err: Some(index),
            ..OmError::hresult(hresult)
        }
    }

    pub fn app_defined() -> OmError {
        OmError::exception(vba_scode(1004), None)
    }

    pub fn app_defined_with(description: &str) -> OmError {
        OmError::exception(vba_scode(1004), Some(description))
    }

    pub fn unable_to_set(property: &str, class: &str) -> OmError {
        OmError::app_defined_with(&format!(
            "Unable to set the {property} property of the {class} class"
        ))
    }

    pub fn bad_index() -> OmError {
        OmError::exception(SCODE_BAD_INDEX, None)
    }

    pub fn type_mismatch() -> OmError {
        OmError::exception(SCODE_TYPE_MISMATCH, None)
    }

    pub fn object_required() -> OmError {
        OmError {
            number: 424,
            ..OmError::hresult(OBJECT_REQUIRED)
        }
    }

    pub fn unsupported(what: &str) -> OmError {
        OmError::exception(
            vba_scode(438),
            Some(&format!("not supported by xlcalc: {what}")),
        )
    }
}

impl std::fmt::Display for OmError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match &self.description {
            Some(d) => write!(f, "{} ({:#x}): {d}", self.number, self.hresult.0 as u32),
            None => write!(f, "{} ({:#x})", self.number, self.hresult.0 as u32),
        }
    }
}

impl std::error::Error for OmError {}

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct NameError {
    pub hresult: HResult,
    pub ids: Vec<i32>,
}
