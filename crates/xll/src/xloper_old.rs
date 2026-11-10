use xlcalc::{Array, Value, XlError};

use crate::args::ansi_bytes_lossy;
use crate::xloper::{error_code, error_from_code, ERR_NA};

pub const NUM: u16 = 0x0001;
pub const STR: u16 = 0x0002;
pub const BOOL: u16 = 0x0004;
pub const REF: u16 = 0x0008;
pub const ERR: u16 = 0x0010;
pub const MULTI: u16 = 0x0040;
pub const MISSING: u16 = 0x0080;
pub const NIL: u16 = 0x0100;
pub const SREF: u16 = 0x0400;
pub const INT: u16 = 0x0800;
pub const BIT_DLLFREE: u16 = 0x4000;

#[repr(C)]
#[derive(Clone, Copy)]
pub struct Xloper {
    pub val: [u64; 2],
    pub xltype: u16,
}

impl Xloper {
    pub fn with(w0: u64, w1: u64, xltype: u16) -> Xloper {
        Xloper {
            val: [w0, w1],
            xltype,
        }
    }

    pub fn nil() -> Xloper {
        Xloper::with(0, 0, NIL)
    }

    pub fn missing() -> Xloper {
        Xloper::with(0, 0, MISSING)
    }

    pub fn error(code: i32) -> Xloper {
        Xloper::with(code as u64 & 0xFFFF, 0, ERR)
    }

    pub fn kind(&self) -> u16 {
        self.xltype & 0x0FFF
    }
}

#[derive(Default)]
pub struct Alloc {
    strs: Vec<Box<[u8]>>,
    cells: Vec<Box<[Xloper]>>,
    words: Vec<Box<[u64]>>,
}

unsafe impl Send for Alloc {}

impl Alloc {
    pub fn convert(&mut self, v: &Value) -> Xloper {
        match v {
            Value::Blank => Xloper::nil(),
            Value::Number(n) => Xloper::with(n.to_bits(), 0, NUM),
            Value::Bool(b) => Xloper::with(u64::from(*b), 0, BOOL),
            Value::Error(e) => Xloper::error(error_code(*e)),
            Value::Text(t) => {
                let bytes = ansi_bytes_lossy(t);
                if bytes.len() > 255 {
                    return Xloper::error(ERR_NA);
                }
                let mut buf = Vec::with_capacity(bytes.len() + 1);
                buf.push(bytes.len() as u8);
                buf.extend(bytes);
                let mut b = buf.into_boxed_slice();
                let p = b.as_mut_ptr() as u64;
                self.strs.push(b);
                Xloper::with(p, 0, STR)
            }
            Value::Array(a) => {
                if a.rows > 65535 || a.cols > 65535 {
                    return Xloper::error(ERR_NA);
                }
                let cells: Vec<Xloper> = a
                    .data
                    .iter()
                    .map(|x| match x {
                        Value::Array(_) | Value::Lambda(_) => Xloper::error(15),
                        x => self.convert(x),
                    })
                    .collect();
                let mut b = cells.into_boxed_slice();
                let p = b.as_mut_ptr() as u64;
                self.cells.push(b);
                Xloper::with(p, u64::from(a.rows) | (u64::from(a.cols) << 16), MULTI)
            }
            Value::Lambda(_) => Xloper::error(15),
        }
    }

    pub fn sref(&mut self, r: xlcalc::Rect) -> Xloper {
        match rect_words(r) {
            Some(w) => Xloper::with(1 | (w << 16), 0, SREF),
            None => Xloper::error(23),
        }
    }

    pub fn mref(&mut self, rects: &[xlcalc::Rect], sheet: u32) -> Xloper {
        let mut words = vec![0u64; (2 + 6 * rects.len()).div_ceil(8) + 1];
        let bytes = words.as_mut_ptr().cast::<u8>();

        unsafe {
            *bytes.cast::<u16>() = rects.len() as u16;
            for (i, r) in rects.iter().enumerate() {
                let Some(w) = rect_words(*r) else {
                    return Xloper::error(23);
                };
                std::ptr::copy_nonoverlapping(w.to_le_bytes().as_ptr(), bytes.add(2 + 6 * i), 6);
            }
        }
        let mut b = words.into_boxed_slice();
        let p = b.as_mut_ptr() as u64;
        self.words.push(b);
        Xloper::with(p, u64::from(sheet), REF)
    }
}

fn rect_words(r: xlcalc::Rect) -> Option<u64> {
    if r.r1 > 65535 || r.c1 > 255 {
        return None;
    }
    Some(
        u64::from(r.r0)
            | (u64::from(r.r1) << 16)
            | (u64::from(r.c0) << 32)
            | (u64::from(r.c1) << 40),
    )
}

unsafe fn read_rect(p: *const u8) -> xlcalc::Rect {
    let w = |o: usize| u32::from(p.add(o).cast::<u16>().read_unaligned());
    xlcalc::Rect {
        r0: w(0),
        r1: w(2),
        c0: u32::from(*p.add(4)),
        c1: u32::from(*p.add(5)),
    }
}

pub unsafe fn first_rect(x: &Xloper) -> Option<(xlcalc::Rect, u32)> {
    match x.kind() {
        SREF => Some((read_rect(x.val.as_ptr().cast::<u8>().add(2)), 0)),
        REF => {
            let m = x.val[0] as *const u8;
            if m.is_null() || m.cast::<u16>().read_unaligned() == 0 {
                return None;
            }
            Some((read_rect(m.add(2)), x.val[1] as u32))
        }
        _ => None,
    }
}

pub unsafe fn rects(x: &Xloper) -> Option<(Vec<xlcalc::Rect>, u32)> {
    match x.kind() {
        SREF => Some((vec![read_rect(x.val.as_ptr().cast::<u8>().add(2))], 0)),
        REF => {
            let m = x.val[0] as *const u8;
            if m.is_null() {
                return None;
            }
            let n = usize::from(m.cast::<u16>().read_unaligned());
            let list = (0..n).map(|i| read_rect(m.add(2 + 6 * i))).collect();
            Some((list, x.val[1] as u32))
        }
        _ => None,
    }
}

pub unsafe fn to_value(x: &Xloper) -> Option<Value> {
    Some(match x.kind() {
        NUM => Value::Number(f64::from_bits(x.val[0])),
        STR => {
            let p = x.val[0] as *const u8;
            if p.is_null() {
                Value::Text("".into())
            } else {
                let b = std::slice::from_raw_parts(p.add(1), usize::from(*p));
                Value::Text(crate::args::ansi_string(b).into())
            }
        }
        BOOL => Value::Bool(x.val[0] as u16 != 0),
        ERR => Value::Error(error_from_code(i32::from(x.val[0] as u16))),
        INT => Value::Number(f64::from(x.val[0] as u16 as i16)),
        MISSING | NIL => Value::Blank,
        MULTI => {
            let cells = x.val[0] as *const Xloper;
            let rows = u32::from(x.val[1] as u16);
            let cols = u32::from((x.val[1] >> 16) as u16);
            let n = rows as usize * cols as usize;
            let mut data = Vec::with_capacity(n);
            for i in 0..n {
                data.push(to_value(&*cells.add(i)).unwrap_or(Value::Error(XlError::VALUE)));
            }
            Array::new(rows, cols, data).into_value()
        }
        _ => return None,
    })
}
