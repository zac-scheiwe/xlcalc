use std::collections::HashMap;
use std::sync::Mutex;

use xlcalc::{Array, Value, XlError};

pub const NUM: u32 = 0x0001;
pub const STR: u32 = 0x0002;
pub const BOOL: u32 = 0x0004;
pub const REF: u32 = 0x0008;
pub const ERR: u32 = 0x0010;
pub const MULTI: u32 = 0x0040;
pub const MISSING: u32 = 0x0080;
pub const NIL: u32 = 0x0100;
pub const SREF: u32 = 0x0400;
pub const INT: u32 = 0x0800;

pub const TYPE_MASK: u32 = 0x0FFF;
pub const BIT_XLFREE: u32 = 0x1000;
pub const BIT_DLLFREE: u32 = 0x4000;

pub const ERR_NULL: i32 = 0;
pub const ERR_DIV0: i32 = 7;
pub const ERR_VALUE: i32 = 15;
pub const ERR_REF: i32 = 23;
pub const ERR_NAME: i32 = 29;
pub const ERR_NUM: i32 = 36;
pub const ERR_NA: i32 = 42;
pub const ERR_GETTING_DATA: i32 = 43;

#[repr(C)]
#[derive(Clone, Copy, Debug, Default, PartialEq, Eq)]
pub struct Xlref12 {
    pub row_first: i32,
    pub row_last: i32,
    pub col_first: i32,
    pub col_last: i32,
}

#[repr(C)]
pub struct Xlmref12 {
    pub count: u16,
    pub table: [Xlref12; 1],
}

#[repr(C)]
#[derive(Clone, Copy)]
pub struct SRef {
    pub count: u16,
    pub rect: Xlref12,
}

#[repr(C)]
#[derive(Clone, Copy)]
pub struct MRef {
    pub refs: *mut Xlmref12,
    pub sheet: usize,
}

#[repr(C)]
#[derive(Clone, Copy)]
pub struct ArrayVal {
    pub cells: *mut Xloper12,
    pub rows: i32,
    pub cols: i32,
}

#[repr(C)]
#[derive(Clone, Copy)]
pub union Val {
    pub num: f64,
    pub str: *mut u16,
    pub boolean: i32,
    pub err: i32,
    pub int: i32,
    pub sref: SRef,
    pub mref: MRef,
    pub array: ArrayVal,
}

#[repr(C)]
#[derive(Clone, Copy)]
pub struct Xloper12 {
    pub val: Val,
    pub xltype: u32,
}

impl Xloper12 {
    pub fn with(val: Val, xltype: u32) -> Xloper12 {
        Xloper12 { val, xltype }
    }

    pub fn nil() -> Xloper12 {
        Xloper12::with(Val { int: 0 }, NIL)
    }

    pub fn missing() -> Xloper12 {
        Xloper12::with(Val { int: 0 }, MISSING)
    }

    pub fn number(n: f64) -> Xloper12 {
        Xloper12::with(Val { num: n }, NUM)
    }

    pub fn error(code: i32) -> Xloper12 {
        Xloper12::with(Val { err: code }, ERR)
    }

    pub fn kind(&self) -> u32 {
        self.xltype & TYPE_MASK
    }
}

#[derive(Default)]
pub struct Alloc {
    strs: Vec<Box<[u16]>>,
    cells: Vec<Box<[Xloper12]>>,
}

unsafe impl Send for Alloc {}

impl Alloc {
    fn string(&mut self, s: &str) -> *mut u16 {
        let units: Vec<u16> = s.encode_utf16().take(32767).collect();
        let mut buf = Vec::with_capacity(units.len() + 1);
        buf.push(units.len() as u16);
        buf.extend(units);
        let mut b = buf.into_boxed_slice();
        let p = b.as_mut_ptr();
        self.strs.push(b);
        p
    }

    pub fn convert(&mut self, v: &Value) -> Xloper12 {
        match v {
            Value::Blank => Xloper12::nil(),
            Value::Number(n) => Xloper12::number(*n),
            Value::Text(t) => Xloper12::with(
                Val {
                    str: self.string(t),
                },
                STR,
            ),
            Value::Bool(b) => Xloper12::with(
                Val {
                    boolean: i32::from(*b),
                },
                BOOL,
            ),
            Value::Error(e) => Xloper12::error(error_code(*e)),
            Value::Array(a) => {
                let cells: Vec<Xloper12> = a
                    .data
                    .iter()
                    .map(|x| match x {
                        Value::Array(_) | Value::Lambda(_) => Xloper12::error(ERR_VALUE),
                        x => self.convert(x),
                    })
                    .collect();
                let mut b = cells.into_boxed_slice();
                let p = b.as_mut_ptr();
                self.cells.push(b);
                Xloper12::with(
                    Val {
                        array: ArrayVal {
                            cells: p,
                            rows: a.rows as i32,
                            cols: a.cols as i32,
                        },
                    },
                    MULTI,
                )
            }
            Value::Lambda(_) => Xloper12::error(ERR_VALUE),
        }
    }
}

static LIVE: Mutex<Option<HashMap<usize, Alloc>>> = Mutex::new(None);

pub fn keep(x: &Xloper12, alloc: Alloc) {
    let key = unsafe {
        match x.kind() {
            STR => x.val.str as usize,
            MULTI => x.val.array.cells as usize,
            _ => return,
        }
    };
    LIVE.lock()
        .unwrap_or_else(|e| e.into_inner())
        .get_or_insert_with(HashMap::new)
        .insert(key, alloc);
}

pub fn free(x: &Xloper12) {
    let key = unsafe {
        match x.kind() {
            STR => x.val.str as usize,
            MULTI => x.val.array.cells as usize,
            _ => return,
        }
    };
    if let Some(map) = LIVE.lock().unwrap_or_else(|e| e.into_inner()).as_mut() {
        map.remove(&key);
    }
}

pub fn error_code(e: XlError) -> i32 {
    match e {
        XlError::NULL => ERR_NULL,
        XlError::DIV0 => ERR_DIV0,
        XlError::VALUE => ERR_VALUE,
        XlError::REF => ERR_REF,
        XlError::NAME => ERR_NAME,
        XlError::NUM => ERR_NUM,
        XlError::NA => ERR_NA,
        XlError::GETTING_DATA => ERR_GETTING_DATA,
        _ => ERR_VALUE,
    }
}

pub fn error_from_code(code: i32) -> XlError {
    match code {
        ERR_NULL => XlError::NULL,
        ERR_DIV0 => XlError::DIV0,
        ERR_REF => XlError::REF,
        ERR_NAME => XlError::NAME,
        ERR_NUM => XlError::NUM,
        ERR_NA => XlError::NA,
        ERR_GETTING_DATA => XlError::GETTING_DATA,
        _ => XlError::VALUE,
    }
}

pub unsafe fn counted(p: *const u16) -> String {
    if p.is_null() {
        return String::new();
    }
    let n = usize::from(*p);
    String::from_utf16_lossy(std::slice::from_raw_parts(p.add(1), n))
}

pub unsafe fn to_value(x: &Xloper12) -> Option<Value> {
    Some(match x.kind() {
        NUM => Value::Number(x.val.num),
        STR => Value::Text(counted(x.val.str).into()),
        BOOL => Value::Bool(x.val.boolean != 0),
        ERR => Value::Error(error_from_code(x.val.err)),
        INT => Value::Number(f64::from(x.val.int)),
        MISSING | NIL => Value::Blank,
        MULTI => {
            let a = x.val.array;
            let (rows, cols) = (a.rows.max(0) as u32, a.cols.max(0) as u32);
            let n = rows as usize * cols as usize;
            let mut data = Vec::with_capacity(n);
            for i in 0..n {
                let cell = &*a.cells.add(i);
                data.push(to_value(cell).unwrap_or(Value::Error(XlError::VALUE)));
            }
            Array::new(rows, cols, data).into_value()
        }
        _ => return None,
    })
}
