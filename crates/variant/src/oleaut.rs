use crate::{Decimal, HResult, Locale, SafeArray, VarType, Variant, VT_ARRAY};
use std::ffi::c_void;

#[repr(C)]
#[derive(Clone, Copy, Default)]
struct RawVariant {
    vt: u16,
    r1: u16,
    r2: u16,
    r3: u16,
    data: [u64; 2],
}

#[repr(C)]
struct Bound {
    elements: u32,
    lbound: i32,
}

#[link(name = "oleaut32")]
extern "system" {
    fn VariantChangeTypeEx(
        dest: *mut RawVariant,
        src: *const RawVariant,
        lcid: u32,
        flags: u16,
        vt: u16,
    ) -> i32;
    fn VarAdd(l: *const RawVariant, r: *const RawVariant, out: *mut RawVariant) -> i32;
    fn VarSub(l: *const RawVariant, r: *const RawVariant, out: *mut RawVariant) -> i32;
    fn VarMul(l: *const RawVariant, r: *const RawVariant, out: *mut RawVariant) -> i32;
    fn VarDiv(l: *const RawVariant, r: *const RawVariant, out: *mut RawVariant) -> i32;
    fn VarIdiv(l: *const RawVariant, r: *const RawVariant, out: *mut RawVariant) -> i32;
    fn VarMod(l: *const RawVariant, r: *const RawVariant, out: *mut RawVariant) -> i32;
    fn VarPow(l: *const RawVariant, r: *const RawVariant, out: *mut RawVariant) -> i32;
    fn VarCat(l: *const RawVariant, r: *const RawVariant, out: *mut RawVariant) -> i32;
    fn VarAnd(l: *const RawVariant, r: *const RawVariant, out: *mut RawVariant) -> i32;
    fn VarOr(l: *const RawVariant, r: *const RawVariant, out: *mut RawVariant) -> i32;
    fn VarXor(l: *const RawVariant, r: *const RawVariant, out: *mut RawVariant) -> i32;
    fn VarEqv(l: *const RawVariant, r: *const RawVariant, out: *mut RawVariant) -> i32;
    fn VarImp(l: *const RawVariant, r: *const RawVariant, out: *mut RawVariant) -> i32;
    fn VarCmp(l: *const RawVariant, r: *const RawVariant, lcid: u32, flags: u32) -> i32;
    fn VarNeg(v: *const RawVariant, out: *mut RawVariant) -> i32;
    fn VarNot(v: *const RawVariant, out: *mut RawVariant) -> i32;
    fn VarAbs(v: *const RawVariant, out: *mut RawVariant) -> i32;
    fn VarFix(v: *const RawVariant, out: *mut RawVariant) -> i32;
    fn VarInt(v: *const RawVariant, out: *mut RawVariant) -> i32;
    fn VarRound(v: *const RawVariant, decimals: i32, out: *mut RawVariant) -> i32;
    fn VariantCopy(dest: *mut RawVariant, src: *const RawVariant) -> i32;
    fn VariantClear(v: *mut RawVariant) -> i32;
    fn SysAllocStringLen(s: *const u16, len: u32) -> *mut u16;
    fn SysStringLen(s: *const u16) -> u32;
    fn SafeArrayCreate(vt: u16, dims: u32, bounds: *const Bound) -> *mut c_void;
    fn SafeArrayGetDim(sa: *mut c_void) -> u32;
    fn SafeArrayGetLBound(sa: *mut c_void, dim: u32, lbound: *mut i32) -> i32;
    fn SafeArrayGetUBound(sa: *mut c_void, dim: u32, ubound: *mut i32) -> i32;
    fn SafeArrayGetVartype(sa: *mut c_void, vt: *mut u16) -> i32;
    fn SafeArrayPutElement(sa: *mut c_void, indices: *const i32, v: *const c_void) -> i32;
    fn SafeArrayGetElement(sa: *mut c_void, indices: *const i32, v: *mut c_void) -> i32;
    fn SafeArrayAccessData(sa: *mut c_void, data: *mut *mut c_void) -> i32;
    fn SafeArrayUnaccessData(sa: *mut c_void) -> i32;
}

fn ok(hr: i32) {
    assert!(hr >= 0, "oleaut32 failed: {:#x}", hr as u32);
}

fn indices(bounds: &[(i32, u32)]) -> Vec<Vec<i32>> {
    let total: u32 = bounds.iter().map(|b| b.1).product();
    (0..total)
        .map(|mut k| {
            bounds
                .iter()
                .map(|&(lo, n)| {
                    let i = lo + (k % n) as i32;
                    k /= n;
                    i
                })
                .collect()
        })
        .collect()
}

fn to_raw(v: &Variant) -> RawVariant {
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
        Variant::Bstr(s) => {
            let units: Vec<u16> = s.encode_utf16().collect();
            unsafe { SysAllocStringLen(units.as_ptr(), units.len() as u32) as u64 }
        }
        Variant::Dispatch(None) | Variant::Unknown(None) => 0,
        Variant::Dispatch(Some(_)) | Variant::Unknown(Some(_)) => {
            panic!("the oracle has no objects")
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
        Variant::Array(a) => {
            assert_eq!(
                a.elem,
                VarType::Variant,
                "the oracle marshals arrays of VARIANT"
            );
            let bounds: Vec<Bound> = a
                .bounds
                .iter()
                .map(|&(lbound, elements)| Bound { elements, lbound })
                .collect();
            let sa = unsafe {
                SafeArrayCreate(
                    VarType::Variant as u16,
                    bounds.len() as u32,
                    bounds.as_ptr(),
                )
            };
            assert!(!sa.is_null());
            for (ix, e) in indices(&a.bounds).iter().zip(&a.data) {
                let mut raw = to_raw(e);
                unsafe {
                    ok(SafeArrayPutElement(
                        sa,
                        ix.as_ptr(),
                        (&raw as *const RawVariant).cast(),
                    ));
                    VariantClear(&mut raw);
                }
            }
            sa as u64
        }
    };
    r
}

fn from_raw(r: &RawVariant) -> Variant {
    let x = r.data[0];
    match r.vt {
        0 => Variant::Empty,
        1 => Variant::Null,
        2 => Variant::I2(x as i16),
        3 => Variant::I4(x as i32),
        4 => Variant::R4(f32::from_bits(x as u32)),
        5 => Variant::R8(f64::from_bits(x)),
        6 => Variant::Cy(x as i64),
        7 => Variant::Date(f64::from_bits(x)),
        8 => {
            let p = x as *const u16;
            let units = unsafe { std::slice::from_raw_parts(p, SysStringLen(p) as usize) };
            Variant::Bstr(String::from_utf16_lossy(units))
        }
        9 if x == 0 => Variant::Dispatch(None),
        13 if x == 0 => Variant::Unknown(None),
        10 => Variant::Error(x as i32),
        11 => Variant::Bool(x as u16 != 0),
        14 => Variant::Decimal(Decimal {
            negative: r.r1 & 0x8000 != 0,
            scale: r.r1 as u8,
            mantissa: (r.r3 as u128) << 80 | (r.r2 as u128) << 64 | x as u128,
        }),
        17 => Variant::Ui1(x as u8),
        20 => Variant::I8(x as i64),
        vt if vt == VT_ARRAY | VarType::Variant as u16 => {
            let sa = x as *mut c_void;
            let dims = unsafe { SafeArrayGetDim(sa) };
            let bounds: Vec<(i32, u32)> = (1..=dims)
                .map(|d| {
                    let (mut lo, mut hi) = (0, 0);
                    unsafe {
                        ok(SafeArrayGetLBound(sa, d, &mut lo));
                        ok(SafeArrayGetUBound(sa, d, &mut hi));
                    }
                    (lo, (hi - lo + 1) as u32)
                })
                .collect();
            let data = indices(&bounds)
                .iter()
                .map(|ix| {
                    let mut e = RawVariant::default();
                    unsafe {
                        ok(SafeArrayGetElement(
                            sa,
                            ix.as_ptr(),
                            (&mut e as *mut RawVariant).cast(),
                        ));
                    }
                    let v = from_raw(&e);
                    unsafe { VariantClear(&mut e) };
                    v
                })
                .collect();
            Variant::Array(Box::new(SafeArray {
                elem: VarType::Variant,
                bounds,
                data,
            }))
        }
        vt => panic!("the oracle does not marshal VARTYPE {vt:#x}"),
    }
}

pub(crate) fn change_type(
    src: &Variant,
    to: VarType,
    locale: Locale,
    flags: u16,
) -> Result<Variant, HResult> {
    let mut s = to_raw(src);
    let mut d = RawVariant::default();
    let hr = unsafe { VariantChangeTypeEx(&mut d, &s, locale.0, flags, to as u16) };
    let out = if hr < 0 {
        Err(HResult(hr))
    } else {
        Ok(from_raw(&d))
    };
    unsafe {
        VariantClear(&mut d);
        VariantClear(&mut s);
    }
    out
}

type Binary =
    unsafe extern "system" fn(*const RawVariant, *const RawVariant, *mut RawVariant) -> i32;
type Unary = unsafe extern "system" fn(*const RawVariant, *mut RawVariant) -> i32;

fn result(hr: i32, d: &mut RawVariant) -> Result<Variant, HResult> {
    let out = if hr < 0 {
        Err(HResult(hr))
    } else {
        Ok(from_raw(d))
    };
    unsafe { VariantClear(d) };
    out
}

pub(crate) fn binary(op: crate::ops::Binary, a: &Variant, b: &Variant) -> Result<Variant, HResult> {
    use crate::ops::Binary as B;
    let f: Binary = match op {
        B::Add => VarAdd,
        B::Sub => VarSub,
        B::Mul => VarMul,
        B::Div => VarDiv,
        B::Idiv => VarIdiv,
        B::Mod => VarMod,
        B::Pow => VarPow,
        B::Cat => VarCat,
        B::And => VarAnd,
        B::Or => VarOr,
        B::Xor => VarXor,
        B::Eqv => VarEqv,
        B::Imp => VarImp,
    };
    let (mut l, mut r, mut d) = (to_raw(a), to_raw(b), RawVariant::default());
    let hr = unsafe { f(&l, &r, &mut d) };
    unsafe {
        VariantClear(&mut l);
        VariantClear(&mut r);
    }
    result(hr, &mut d)
}

pub(crate) fn unary(op: crate::ops::Unary, a: &Variant) -> Result<Variant, HResult> {
    use crate::ops::Unary as U;
    let f: Unary = match op {
        U::Neg => VarNeg,
        U::Not => VarNot,
        U::Abs => VarAbs,
        U::Fix => VarFix,
        U::Int => VarInt,
    };
    let (mut l, mut d) = (to_raw(a), RawVariant::default());
    let hr = unsafe { f(&l, &mut d) };
    unsafe { VariantClear(&mut l) };
    result(hr, &mut d)
}

pub(crate) fn round(a: &Variant, decimals: i32) -> Result<Variant, HResult> {
    let (mut l, mut d) = (to_raw(a), RawVariant::default());
    let hr = unsafe { VarRound(&l, decimals, &mut d) };
    unsafe { VariantClear(&mut l) };
    result(hr, &mut d)
}

pub(crate) fn compare(a: &Variant, b: &Variant, locale: Locale, flags: u32) -> Result<u8, HResult> {
    let (mut l, mut r) = (to_raw(a), to_raw(b));
    let hr = unsafe { VarCmp(&l, &r, locale.0, flags) };
    unsafe {
        VariantClear(&mut l);
        VariantClear(&mut r);
    }
    if hr < 0 {
        Err(HResult(hr))
    } else {
        Ok(hr as u8)
    }
}

pub(crate) fn same(a: &Variant, b: &Variant) -> bool {
    match (a, b) {
        (Variant::R4(x), Variant::R4(y)) => x.to_bits() == y.to_bits(),
        (Variant::R8(x), Variant::R8(y)) | (Variant::Date(x), Variant::Date(y)) => {
            x.to_bits() == y.to_bits()
        }
        (Variant::Array(x), Variant::Array(y)) => {
            x.elem == y.elem
                && x.bounds == y.bounds
                && x.data.len() == y.data.len()
                && x.data.iter().zip(&y.data).all(|(a, b)| same(a, b))
        }
        _ => a == b,
    }
}

pub(crate) fn differences(
    cases: impl IntoIterator<Item = Variant>,
    to: VarType,
    locale: Locale,
    flags: u16,
) -> Vec<String> {
    cases
        .into_iter()
        .filter_map(|v| {
            let ours = crate::change_type(&v, to, locale, flags);
            let theirs = change_type(&v, to, locale, flags);
            let agree = match (&ours, &theirs) {
                (Ok(a), Ok(b)) => same(a, b),
                (a, b) => a == b,
            };
            (!agree).then(|| format!("{v:?} -> {to:?}: ours {ours:?}, oleaut32 {theirs:?}"))
        })
        .collect()
}

pub(crate) struct Rng(pub u64);

impl Rng {
    pub(crate) fn next(&mut self) -> u64 {
        self.0 = self.0.wrapping_add(0x9E37_79B9_7F4A_7C15);
        let mut z = self.0;
        z = (z ^ (z >> 30)).wrapping_mul(0xBF58_476D_1CE4_E5B9);
        z = (z ^ (z >> 27)).wrapping_mul(0x94D0_49BB_1331_11EB);
        z ^ (z >> 31)
    }

    pub(crate) fn below(&mut self, n: u64) -> u64 {
        self.next() % n
    }

    pub(crate) fn double(&mut self, lo: i32, hi: i32) -> f64 {
        let e = (lo + self.below((hi - lo + 1) as u64) as i32 + 1023) as u64;
        let bits = self.next();
        f64::from_bits(bits & 0x800F_FFFF_FFFF_FFFF | e << 52)
    }

    pub(crate) fn any_double(&mut self) -> f64 {
        f64::from_bits(self.next())
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    fn roundtrip(v: &Variant) -> Variant {
        let mut r = to_raw(v);
        let mut c = RawVariant::default();
        unsafe { ok(VariantCopy(&mut c, &r)) };
        let back = from_raw(&c);
        unsafe {
            VariantClear(&mut c);
            VariantClear(&mut r);
        }
        back
    }

    #[test]
    fn marshalling_roundtrips() {
        let dec = Decimal {
            negative: true,
            scale: 28,
            mantissa: (1 << 96) - 1,
        };
        let scalars = [
            Variant::Empty,
            Variant::Null,
            Variant::I2(-2),
            Variant::I4(-70_000),
            Variant::R4(-0.1),
            Variant::R8(-0.0),
            Variant::Cy(-12_345),
            Variant::Date(45_000.25),
            Variant::Bstr("a\u{0}é😀".to_string()),
            Variant::Dispatch(None),
            Variant::Unknown(None),
            Variant::Error(0x800A_07FA_u32 as i32),
            Variant::Bool(true),
            Variant::Bool(false),
            Variant::Decimal(dec),
            Variant::Ui1(200),
            Variant::I8(i64::MIN),
        ];
        for v in &scalars {
            assert!(same(&roundtrip(v), v), "{v:?}");
        }
        let array = Variant::Array(Box::new(SafeArray {
            elem: VarType::Variant,
            bounds: vec![(1, 2), (0, 3)],
            data: scalars[..6].to_vec(),
        }));
        assert!(same(&roundtrip(&array), &array));
    }

    #[test]
    fn arrays_store_the_first_index_fastest() {
        let data: Vec<Variant> = (0..6).map(Variant::I4).collect();
        let array = Variant::Array(Box::new(SafeArray {
            elem: VarType::Variant,
            bounds: vec![(1, 2), (1, 3)],
            data,
        }));
        let mut r = to_raw(&array);
        let sa = r.data[0] as *mut c_void;
        let (mut vt, mut hi, mut p) = (0u16, 0, std::ptr::null_mut());
        unsafe {
            ok(SafeArrayGetVartype(sa, &mut vt));
            ok(SafeArrayGetUBound(sa, 1, &mut hi));
            ok(SafeArrayAccessData(sa, &mut p));
        }
        assert_eq!((vt, hi), (VarType::Variant as u16, 2));

        let second = unsafe { from_raw(&*(p as *const RawVariant).add(1)) };
        let mut e = RawVariant::default();
        unsafe {
            ok(SafeArrayUnaccessData(sa));
            ok(SafeArrayGetElement(
                sa,
                [2, 1].as_ptr(),
                (&mut e as *mut RawVariant).cast(),
            ));
        }
        assert_eq!((second, from_raw(&e)), (Variant::I4(1), Variant::I4(1)));
        unsafe {
            VariantClear(&mut e);
            VariantClear(&mut r);
        }
    }

    #[test]
    fn oracle_answers() {
        let to_i4 = |x| change_type(&Variant::R8(x), VarType::I4, Locale::EN_US, 0);
        assert_eq!(to_i4(2.5), Ok(Variant::I4(2)));
        assert_eq!(to_i4(3.5), Ok(Variant::I4(4)));
        assert_eq!(to_i4(3e9), Err(HResult::DISP_E_OVERFLOW));
        let dec = change_type(&Variant::R8(-1.5), VarType::Decimal, Locale::EN_US, 0);
        let want = Decimal {
            negative: true,
            scale: 1,
            mantissa: 15,
        };
        assert_eq!(dec, Ok(Variant::Decimal(want)));
        let text = change_type(&Variant::Bstr("12".into()), VarType::R8, Locale::EN_NZ, 0);
        assert_eq!(text, Ok(Variant::R8(12.0)));
        let mut rng = Rng(1);
        assert!((0..1000).all(|_| (rng.double(-3, 3).abs().log2().floor() as i32).abs() <= 3));
        assert!(rng.any_double().to_bits() != rng.any_double().to_bits());
    }
}
