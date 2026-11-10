use std::ffi::c_void;

use windows::core::{IUnknown, Interface};
use xlcalc_variant::{Decimal, HResult, SafeArray, VarType, Variant, VT_ARRAY};

#[repr(C)]
#[derive(Clone, Copy, Default)]
pub struct RawVariant {
    pub vt: u16,
    r1: u16,
    r2: u16,
    r3: u16,
    data: [u64; 2],
}

const _: () = assert!(
    std::mem::size_of::<RawVariant>()
        == std::mem::size_of::<windows::Win32::System::Variant::VARIANT>()
);

pub struct Objects<'a> {
    pub known: &'a dyn Fn(xlcalc_variant::ObjectId) -> Option<IUnknown>,
    pub made: Vec<IUnknown>,
    pub next: u64,
}

impl Objects<'_> {
    fn resolve(&self, id: xlcalc_variant::ObjectId) -> Option<IUnknown> {
        (self.known)(id)
    }

    fn adopt(&mut self, unk: &IUnknown) -> xlcalc_variant::ObjectId {
        if let Some(id) = crate::proxy::id_of(unk) {
            return id;
        }
        self.made.push(unk.clone());
        xlcalc_variant::ObjectId(self.next + self.made.len() as u64 - 1)
    }
}

const VT_BYREF: u16 = 0x4000;

pub fn byref_bool(b: &mut i16) -> RawVariant {
    let mut r = RawVariant {
        vt: VT_BYREF | VarType::Bool as u16,
        ..RawVariant::default()
    };
    r.data[0] = b as *mut i16 as u64;
    r
}

pub unsafe fn clear(raw: &mut RawVariant) {
    VariantClear(raw);
}

#[repr(C)]
struct Bound {
    elements: u32,
    lbound: i32,
}

#[link(name = "oleaut32")]
extern "system" {
    fn SysAllocStringLen(s: *const u16, len: u32) -> *mut u16;
    fn SysStringLen(s: *const u16) -> u32;
    fn SafeArrayCreate(vt: u16, dims: u32, bounds: *const Bound) -> *mut c_void;
    fn SafeArrayDestroy(sa: *mut c_void) -> i32;
    fn SafeArrayGetDim(sa: *mut c_void) -> u32;
    fn SafeArrayGetLBound(sa: *mut c_void, dim: u32, lbound: *mut i32) -> i32;
    fn SafeArrayGetUBound(sa: *mut c_void, dim: u32, ubound: *mut i32) -> i32;
    fn SafeArrayGetVartype(sa: *mut c_void, vt: *mut u16) -> i32;
    fn SafeArrayGetElement(sa: *mut c_void, indices: *const i32, v: *mut c_void) -> i32;
    fn SafeArrayPutElement(sa: *mut c_void, indices: *const i32, v: *const c_void) -> i32;
    fn VariantClear(v: *mut RawVariant) -> i32;
}

fn indices(bounds: &[(i32, u32)]) -> impl Iterator<Item = Vec<i32>> + '_ {
    let total: u32 = bounds.iter().map(|b| b.1).product();
    (0..total).map(move |mut k| {
        bounds
            .iter()
            .map(|&(lo, n)| {
                let i = lo + (k % n) as i32;
                k /= n;
                i
            })
            .collect()
    })
}

pub fn bstr(s: &str) -> *mut u16 {
    let units: Vec<u16> = s.encode_utf16().collect();
    unsafe { SysAllocStringLen(units.as_ptr(), units.len() as u32) }
}

unsafe fn read_bstr(p: *const u16) -> String {
    if p.is_null() {
        return String::new();
    }
    let units = std::slice::from_raw_parts(p, SysStringLen(p) as usize);
    String::from_utf16_lossy(units)
}

pub fn to_raw(v: &Variant, cx: &mut Objects) -> Result<RawVariant, HResult> {
    let mut r = RawVariant {
        vt: v.vt(),
        ..RawVariant::default()
    };
    r.data[0] = match v {
        Variant::Empty | Variant::Null | Variant::BstrNull => 0,
        Variant::I2(x) => *x as u16 as u64,
        Variant::I4(x) | Variant::Error(x) => *x as u32 as u64,
        Variant::R4(x) => x.to_bits() as u64,
        Variant::R8(x) | Variant::Date(x) => x.to_bits(),
        Variant::Cy(x) | Variant::I8(x) => *x as u64,
        Variant::Bstr(s) => bstr(s) as u64,
        Variant::Dispatch(None) | Variant::Unknown(None) => 0,
        Variant::Dispatch(Some(id)) | Variant::Unknown(Some(id)) => {
            let unk = cx.resolve(*id).ok_or(HResult::DISP_E_TYPEMISMATCH)?;
            r.vt = if matches!(v, Variant::Dispatch(_)) {
                VarType::Dispatch as u16
            } else {
                VarType::Unknown as u16
            };
            let p = if r.vt == VarType::Dispatch as u16 {
                unk.cast::<windows::Win32::System::Com::IDispatch>()
                    .map_err(|e| HResult(e.code().0))?
                    .into_raw()
            } else {
                unk.into_raw()
            };
            p as u64
        }
        Variant::Bool(b) => {
            if *b {
                0xFFFF
            } else {
                0
            }
        }
        Variant::Decimal(d) => {
            r.r1 = d.scale as u16 | if d.negative { 0x8000 } else { 0 };
            r.r2 = (d.mantissa >> 64) as u16;
            r.r3 = (d.mantissa >> 80) as u16;
            d.mantissa as u64
        }
        Variant::Ui1(x) => *x as u64,
        Variant::Array(a) => array_to_raw(a, cx)? as u64,
    };
    Ok(r)
}

fn array_to_raw(a: &SafeArray, cx: &mut Objects) -> Result<*mut c_void, HResult> {
    let bounds: Vec<Bound> = a
        .bounds
        .iter()
        .map(|&(lbound, elements)| Bound { elements, lbound })
        .collect();
    let sa = unsafe { SafeArrayCreate(a.elem as u16, bounds.len() as u32, bounds.as_ptr()) };
    if sa.is_null() {
        return Err(HResult::E_OUTOFMEMORY);
    }
    for (ix, e) in indices(&a.bounds).zip(&a.data) {
        let mut raw = match to_raw(e, cx) {
            Ok(raw) => raw,
            Err(hr) => {
                unsafe { SafeArrayDestroy(sa) };
                return Err(hr);
            }
        };
        let hr = unsafe {
            let hr = if a.elem == VarType::Variant {
                SafeArrayPutElement(sa, ix.as_ptr(), (&raw as *const RawVariant).cast())
            } else if a.elem == VarType::Bstr {
                SafeArrayPutElement(sa, ix.as_ptr(), raw.data[0] as *const c_void)
            } else {
                SafeArrayPutElement(sa, ix.as_ptr(), raw.data.as_ptr().cast())
            };
            VariantClear(&mut raw);
            hr
        };
        if hr < 0 {
            unsafe { SafeArrayDestroy(sa) };
            return Err(HResult(hr));
        }
    }
    Ok(sa)
}

pub unsafe fn from_raw(r: &RawVariant, cx: &mut Objects) -> Result<Variant, HResult> {
    if r.vt & VT_BYREF != 0 {
        let base = r.vt & !VT_BYREF;
        let p = r.data[0] as *const u8;
        if p.is_null() {
            return Err(HResult::E_INVALIDARG);
        }
        if base == VarType::Variant as u16 {
            return from_raw(&*(p as *const RawVariant), cx);
        }
        let mut v = RawVariant {
            vt: base,
            ..RawVariant::default()
        };
        if base == VarType::Decimal as u16 {
            std::ptr::copy_nonoverlapping(p, (&mut v as *mut RawVariant).cast::<u8>(), 16);
            v.vt = base;
        } else {
            let size = match base & !VT_ARRAY {
                2 | 11 => 2,
                3 | 4 | 10 => 4,
                17 => 1,
                _ => 8,
            };
            let size = if base & VT_ARRAY != 0 { 8 } else { size };
            std::ptr::copy_nonoverlapping(p, v.data.as_mut_ptr().cast::<u8>(), size);
        }
        return from_raw(&v, cx);
    }
    let d = r.data[0];
    Ok(match r.vt {
        0 => Variant::Empty,
        1 => Variant::Null,
        2 => Variant::I2(d as u16 as i16),
        3 => Variant::I4(d as u32 as i32),
        4 => Variant::R4(f32::from_bits(d as u32)),
        5 => Variant::R8(f64::from_bits(d)),
        6 => Variant::Cy(d as i64),
        7 => Variant::Date(f64::from_bits(d)),
        8 => Variant::Bstr(read_bstr(d as *const u16)),
        9 | 13 => {
            let id = if d == 0 {
                None
            } else {
                let ptr = d as *mut c_void;
                let unk = IUnknown::from_raw_borrowed(&ptr).ok_or(HResult::E_INVALIDARG)?;
                Some(cx.adopt(unk))
            };
            if r.vt == 9 {
                Variant::Dispatch(id)
            } else {
                Variant::Unknown(id)
            }
        }
        10 => Variant::Error(d as u32 as i32),
        11 => Variant::Bool(d as u16 != 0),
        14 => Variant::Decimal(Decimal {
            negative: r.r1 & 0x8000 != 0,
            scale: r.r1 as u8,
            mantissa: (r.r3 as u128) << 80 | (r.r2 as u128) << 64 | d as u128,
        }),
        16 => Variant::I2(d as u8 as i8 as i16),
        17 => Variant::Ui1(d as u8),
        18 => Variant::I4(d as u16 as i32),
        19 | 23 => Variant::R8((d as u32) as f64),
        20 => Variant::I8(d as i64),
        22 => Variant::I4(d as u32 as i32),
        vt if vt & VT_ARRAY != 0 => Variant::Array(Box::new(array_from_raw(d as *mut c_void, cx)?)),
        _ => return Err(HResult::DISP_E_BADVARTYPE),
    })
}

unsafe fn array_from_raw(sa: *mut c_void, cx: &mut Objects) -> Result<SafeArray, HResult> {
    if sa.is_null() {
        return Err(HResult::E_INVALIDARG);
    }
    let dims = SafeArrayGetDim(sa);
    let mut bounds = Vec::with_capacity(dims as usize);
    for dim in 1..=dims {
        let (mut lo, mut hi) = (0, 0);
        if SafeArrayGetLBound(sa, dim, &mut lo) < 0 || SafeArrayGetUBound(sa, dim, &mut hi) < 0 {
            return Err(HResult::E_INVALIDARG);
        }
        bounds.push((lo, (hi - lo + 1).max(0) as u32));
    }
    let mut vt = 0u16;
    if SafeArrayGetVartype(sa, &mut vt) < 0 {
        return Err(HResult::DISP_E_BADVARTYPE);
    }
    let elem = match vt {
        12 => VarType::Variant,
        2 => VarType::I2,
        3 => VarType::I4,
        4 => VarType::R4,
        5 => VarType::R8,
        6 => VarType::Cy,
        7 => VarType::Date,
        8 => VarType::Bstr,
        10 => VarType::Error,
        11 => VarType::Bool,
        17 => VarType::Ui1,
        20 => VarType::I8,
        _ => return Err(HResult::DISP_E_BADVARTYPE),
    };
    let mut data = Vec::new();
    for ix in indices(&bounds) {
        let mut raw = RawVariant::default();
        let value = if elem == VarType::Variant {
            if SafeArrayGetElement(sa, ix.as_ptr(), (&mut raw as *mut RawVariant).cast()) < 0 {
                return Err(HResult::E_INVALIDARG);
            }
            let v = from_raw(&raw, cx);
            VariantClear(&mut raw);
            v?
        } else {
            raw.vt = elem as u16;
            if SafeArrayGetElement(sa, ix.as_ptr(), raw.data.as_mut_ptr().cast()) < 0 {
                return Err(HResult::E_INVALIDARG);
            }
            let v = from_raw(&raw, cx);
            VariantClear(&mut raw);
            v?
        };
        data.push(value);
    }
    Ok(SafeArray { elem, bounds, data })
}
