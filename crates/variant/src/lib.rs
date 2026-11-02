mod convert;
#[cfg(all(test, windows))]
mod oleaut;
pub mod ops;
mod text;
mod ucrt;

#[derive(Clone, Copy, PartialEq, Eq, Hash, Debug)]
pub enum VarType {
    Empty = 0,
    Null = 1,
    I2 = 2,
    I4 = 3,
    R4 = 4,
    R8 = 5,
    Cy = 6,
    Date = 7,
    Bstr = 8,
    Dispatch = 9,
    Error = 10,
    Bool = 11,
    Variant = 12,
    Unknown = 13,
    Decimal = 14,
    Ui1 = 17,
    I8 = 20,
}

#[derive(Clone, Copy, PartialEq, Eq, Hash, Debug)]
pub struct ObjectId(pub u64);

#[derive(Clone, PartialEq, Debug)]
pub enum Variant {
    Empty,
    Null,
    I2(i16),
    I4(i32),
    R4(f32),
    R8(f64),

    Cy(i64),

    Date(f64),

    Bstr(String),

    BstrNull,

    Dispatch(Option<ObjectId>),

    Unknown(Option<ObjectId>),

    Error(i32),
    Bool(bool),
    Decimal(Decimal),
    Ui1(u8),
    I8(i64),

    Array(Box<SafeArray>),
}

pub const VT_ARRAY: u16 = 0x2000;

impl Variant {
    pub fn canon(self) -> Variant {
        match self {
            Variant::BstrNull => Variant::Bstr(String::new()),
            v => v,
        }
    }

    pub fn vt(&self) -> u16 {
        let t = match self {
            Variant::Empty => VarType::Empty,
            Variant::Null => VarType::Null,
            Variant::I2(_) => VarType::I2,
            Variant::I4(_) => VarType::I4,
            Variant::R4(_) => VarType::R4,
            Variant::R8(_) => VarType::R8,
            Variant::Cy(_) => VarType::Cy,
            Variant::Date(_) => VarType::Date,
            Variant::Bstr(_) | Variant::BstrNull => VarType::Bstr,
            Variant::Dispatch(_) => VarType::Dispatch,
            Variant::Unknown(_) => VarType::Unknown,
            Variant::Error(_) => VarType::Error,
            Variant::Bool(_) => VarType::Bool,
            Variant::Decimal(_) => VarType::Decimal,
            Variant::Ui1(_) => VarType::Ui1,
            Variant::I8(_) => VarType::I8,
            Variant::Array(a) => return VT_ARRAY | a.elem as u16,
        };
        t as u16
    }
}

#[derive(Clone, Copy, PartialEq, Eq, Hash, Debug, Default)]
pub struct Decimal {
    pub negative: bool,

    pub scale: u8,

    pub mantissa: u128,
}

#[derive(Clone, PartialEq, Debug)]
pub struct SafeArray {
    pub elem: VarType,

    pub bounds: Vec<(i32, u32)>,

    pub data: Vec<Variant>,
}

#[derive(Clone, Copy, PartialEq, Eq, Hash, Debug)]
pub struct HResult(pub i32);

impl HResult {
    pub const E_NOTIMPL: HResult = HResult(0x8000_4001_u32 as i32);
    pub const E_INVALIDARG: HResult = HResult(0x8007_0057_u32 as i32);
    pub const E_OUTOFMEMORY: HResult = HResult(0x8007_000E_u32 as i32);
    pub const DISP_E_TYPEMISMATCH: HResult = HResult(0x8002_0005_u32 as i32);
    pub const DISP_E_BADVARTYPE: HResult = HResult(0x8002_0008_u32 as i32);
    pub const DISP_E_OVERFLOW: HResult = HResult(0x8002_000A_u32 as i32);
    pub const DISP_E_PARAMNOTFOUND: HResult = HResult(0x8002_0004_u32 as i32);
    pub const DISP_E_DIVBYZERO: HResult = HResult(0x8002_0012_u32 as i32);
}

#[derive(Clone, Copy, PartialEq, Eq, Hash, Debug)]
pub struct Locale(pub u32);

impl Locale {
    pub const EN_US: Locale = Locale(0x0409);
    pub const EN_NZ: Locale = Locale(0x1409);
}

pub mod flags {

    pub const NOVALUEPROP: u16 = 0x01;

    pub const ALPHABOOL: u16 = 0x02;

    pub const NOUSEROVERRIDE: u16 = 0x04;

    pub const LOCALBOOL: u16 = 0x10;
}

pub fn change_type(
    src: &Variant,
    to: VarType,
    locale: Locale,
    flags: u16,
) -> Result<Variant, HResult> {
    convert::change_type(src, to, locale, flags)
}
