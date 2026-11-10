#![cfg(windows)]

use std::sync::atomic::{AtomicU32, AtomicUsize, Ordering};

#[link(name = "kernel32", kind = "raw-dylib")]
extern "system" {
    fn GetModuleHandleW(name: *const u16) -> isize;
    fn GetProcAddress(module: isize, name: *const u8) -> usize;
}

type MdCallBack12 = unsafe extern "system" fn(i32, i32, *mut *mut X, *mut X) -> i32;

unsafe fn excel12v(xlfn: i32, res: *mut X, count: i32, opers: *mut *mut X) -> i32 {
    static CALLBACK: AtomicUsize = AtomicUsize::new(0);
    let mut cb = CALLBACK.load(Ordering::SeqCst);
    if cb == 0 {
        cb = GetProcAddress(
            GetModuleHandleW(std::ptr::null()),
            c"MdCallBack12".as_ptr().cast(),
        );
        CALLBACK.store(cb, Ordering::SeqCst);
    }
    if cb == 0 {
        return 32;
    }
    let f: MdCallBack12 = std::mem::transmute::<usize, MdCallBack12>(cb);
    f(xlfn, count, opers, res)
}

const NUM: u32 = 1;
const STR: u32 = 2;
const BOOL: u32 = 4;
const REF: u32 = 8;
const ERR: u32 = 0x10;
const MULTI: u32 = 0x40;
const MISSING: u32 = 0x80;
const NIL: u32 = 0x100;
const SREF: u32 = 0x400;
const INT: u32 = 0x800;
const XLFREE: u32 = 0x1000;
const DLLFREE: u32 = 0x4000;

const XLF_REGISTER: i32 = 149;
const XLF_CALLER: i32 = 89;
const XL_FREE: i32 = 0x4000;
const XL_COERCE: i32 = 0x4002;
const XL_SHEET_ID: i32 = 0x4004;
const XL_GET_NAME: i32 = 0x4009;
const XL_ASYNC_RETURN: i32 = 0x4010;
const XL_EVENT_REGISTER: i32 = 0x4011;

#[repr(C)]
#[derive(Clone, Copy)]
pub struct X {
    val: [u64; 3],
    t: u32,
}

impl X {
    fn raw(t: u32) -> X {
        X { val: [0; 3], t }
    }
    fn num(n: f64) -> X {
        let mut x = X::raw(NUM);
        x.val[0] = n.to_bits();
        x
    }
    fn int(n: i32) -> X {
        let mut x = X::raw(INT);
        x.val[0] = u64::from(n as u32);
        x
    }
    fn err(c: i32) -> X {
        let mut x = X::raw(ERR);
        x.val[0] = u64::from(c as u32);
        x
    }
    fn bool(b: bool) -> X {
        let mut x = X::raw(BOOL);
        x.val[0] = u64::from(b);
        x
    }
    fn text(s: &str) -> X {
        let mut x = X::raw(STR);
        x.val[0] = wide(s) as u64;
        x
    }
    fn n(&self) -> f64 {
        f64::from_bits(self.val[0])
    }
    fn i(&self) -> i32 {
        self.val[0] as u32 as i32
    }
}

fn wide(s: &str) -> *mut u16 {
    let u: Vec<u16> = s.encode_utf16().collect();
    let mut v = Vec::with_capacity(u.len() + 1);
    v.push(u.len() as u16);
    v.extend(u);
    Box::leak(v.into_boxed_slice()).as_mut_ptr()
}

unsafe fn counted(p: *const u16) -> String {
    if p.is_null() {
        return String::new();
    }
    String::from_utf16_lossy(std::slice::from_raw_parts(p.add(1), usize::from(*p)))
}

unsafe fn zstr(p: *const u16) -> String {
    let mut n = 0;
    while *p.add(n) != 0 {
        n += 1;
    }
    String::from_utf16_lossy(std::slice::from_raw_parts(p, n))
}

unsafe fn describe(x: *const X) -> String {
    if x.is_null() {
        return "null".into();
    }
    let x = &*x;
    let free = if x.t & XLFREE != 0 { "+xl" } else { "" };
    let body = match x.t & 0xFFF {
        NUM => format!("num:{}", x.n()),
        STR => format!("str:{}", counted(x.val[0] as *const u16)),
        BOOL => format!("bool:{}", x.i()),
        ERR => format!("err:{}", x.i()),
        INT => format!("int:{}", x.i()),
        MISSING => "missing".into(),
        NIL => "nil".into(),
        SREF => {
            let b = x.val.as_ptr().cast::<i32>();
            format!(
                "sref:{},{},{},{}",
                *b.add(1),
                *b.add(2),
                *b.add(3),
                *b.add(4)
            )
        }
        REF => {
            let m = x.val[0] as *const u8;
            if m.is_null() {
                return format!("ref:none{free}");
            }
            let b = m.add(4).cast::<i32>();
            format!("ref:{},{},{},{}", *b, *b.add(1), *b.add(2), *b.add(3))
        }
        MULTI => {
            let cells = x.val[0] as *const X;
            let rows = (x.val[1] & 0xFFFF_FFFF) as i32;
            let cols = (x.val[1] >> 32) as i32;
            let mut parts = Vec::new();
            for i in 0..(rows * cols) as usize {
                parts.push(describe(cells.add(i)));
            }
            format!("multi:{rows}x{cols}[{}]", parts.join(";"))
        }
        other => format!("type{other:#x}"),
    };
    format!("{body}{free}")
}

fn give(mut x: X) -> *mut X {
    x.t |= DLLFREE;
    Box::into_raw(Box::new(x))
}

fn give_text(s: &str) -> *mut X {
    give(X::text(s))
}

unsafe fn copy_value(p: *const X) -> X {
    let x = *p;
    match x.t & 0xFFF {
        STR => X::text(&counted(x.val[0] as *const u16)),
        NUM | BOOL | ERR | INT | NIL | MISSING => X {
            t: x.t & 0xFFF,
            ..x
        },
        _ => X::err(15),
    }
}

unsafe fn call(xlfn: i32, opers: &mut [*mut X]) -> (i32, X) {
    let mut res = X::raw(NIL);
    let code = excel12v(xlfn, &mut res, opers.len() as i32, opers.as_mut_ptr());
    (code, res)
}

unsafe fn release(res: &mut X) {
    if res.t & XLFREE != 0 {
        let mut p = res as *mut X;
        excel12v(XL_FREE, std::ptr::null_mut(), 1, &mut p);
    }
}

unsafe fn hex(p: *const u8, n: usize) -> String {
    std::slice::from_raw_parts(p, n)
        .iter()
        .map(|b| format!("{b:02x}"))
        .collect::<Vec<_>>()
        .join("")
}

unsafe fn ansi_report(p: *const u8, counted: bool) -> String {
    if p.is_null() {
        return "null".into();
    }
    let n = if counted {
        usize::from(*p)
    } else {
        let mut n = 0;
        while *p.add(n) != 0 {
            n += 1;
        }
        n
    };
    let b = std::slice::from_raw_parts(if counted { p.add(1) } else { p }, n);
    format!("len={n} hex={}", hex(b.as_ptr(), n.min(40)))
}

unsafe fn wide_report(p: *const u16, counted: bool) -> String {
    if p.is_null() {
        return "null".into();
    }
    let n = if counted {
        usize::from(*p)
    } else {
        let mut n = 0;
        while *p.add(n) != 0 {
            n += 1;
        }
        n
    };
    let b = std::slice::from_raw_parts(if counted { p.add(1) } else { p }, n);
    let units: Vec<String> = b.iter().take(12).map(|u| format!("{u:04x}")).collect();
    format!("len={n} u={}", units.join(","))
}

#[no_mangle]
unsafe extern "system" fn XlcT_A(raw: u64) -> *mut X {
    give_text(&format!("A raw={raw:#x} short={}", raw as u16 as i16))
}
#[no_mangle]
unsafe extern "system" fn XlcT_B(a: f64) -> *mut X {
    give_text(&format!("B {a:?} bits={:#x}", a.to_bits()))
}
#[no_mangle]
unsafe extern "system" fn XlcT_E(a: *const f64) -> *mut X {
    give_text(&format!("E {:?}", *a))
}
#[no_mangle]
unsafe extern "system" fn XlcT_H(raw: u64) -> *mut X {
    give_text(&format!("H raw={raw:#x} u16={}", raw as u16))
}
#[no_mangle]
unsafe extern "system" fn XlcT_I(raw: u64) -> *mut X {
    give_text(&format!("I raw={raw:#x} i16={}", raw as u16 as i16))
}
#[no_mangle]
unsafe extern "system" fn XlcT_J(raw: u64) -> *mut X {
    give_text(&format!("J raw={raw:#x} i32={}", raw as u32 as i32))
}
#[no_mangle]
unsafe extern "system" fn XlcT_L(raw: u64) -> *mut X {
    if raw > 0x10000 {
        give_text(&format!("L ptr *={}", *(raw as *const i32)))
    } else {
        give_text(&format!("L val={raw:#x}"))
    }
}
#[no_mangle]
unsafe extern "system" fn XlcT_C(p: *const u8) -> *mut X {
    give_text(&format!("C {}", ansi_report(p, false)))
}
#[no_mangle]
unsafe extern "system" fn XlcT_D(p: *const u8) -> *mut X {
    give_text(&format!("D {}", ansi_report(p, true)))
}
#[no_mangle]
unsafe extern "system" fn XlcT_Cw(p: *const u16) -> *mut X {
    give_text(&format!("C% {}", wide_report(p, false)))
}
#[no_mangle]
unsafe extern "system" fn XlcT_Dw(p: *const u16) -> *mut X {
    give_text(&format!("D% {}", wide_report(p, true)))
}
#[no_mangle]
unsafe extern "system" fn XlcT_K(p: *const u8) -> *mut X {
    if p.is_null() {
        return give_text("K null");
    }
    let (r, c) = (*p.cast::<u16>(), *p.cast::<u16>().add(1));
    let d = p.add(8).cast::<f64>();
    let n = (usize::from(r) * usize::from(c)).min(12);
    let v: Vec<String> = (0..n).map(|i| format!("{:?}", *d.add(i))).collect();
    give_text(&format!("K {r}x{c} [{}]", v.join(";")))
}
#[no_mangle]
unsafe extern "system" fn XlcT_Kw(p: *const u8) -> *mut X {
    if p.is_null() {
        return give_text("K% null");
    }
    let (r, c) = (*p.cast::<i32>(), *p.cast::<i32>().add(1));
    let d = p.add(8).cast::<f64>();
    let n = (r.max(0) as usize * c.max(0) as usize).min(12);
    let v: Vec<String> = (0..n).map(|i| format!("{:?}", *d.add(i))).collect();
    give_text(&format!("K% {r}x{c} [{}]", v.join(";")))
}

unsafe fn describe_old(x: *const u8) -> String {
    if x.is_null() {
        return "null".into();
    }
    let t = *x.add(16).cast::<u16>();
    let free = if t & 0x1000 != 0 { "+xl" } else { "" };
    let body = match t & 0xFFF {
        1 => format!("num:{}", *x.cast::<f64>()),
        2 => {
            let p = *x.cast::<*const u8>();
            if p.is_null() {
                "str:null".into()
            } else {
                let n = usize::from(*p);
                let b = std::slice::from_raw_parts(p.add(1), n);
                format!(
                    "str:{}",
                    b.iter().map(|&c| char::from(c)).collect::<String>()
                )
            }
        }
        4 => format!("bool:{}", *x.cast::<u16>()),
        0x10 => format!("err:{}", *x.cast::<u16>()),
        0x800 => format!("int:{}", *x.cast::<i16>()),
        0x80 => "missing".into(),
        0x100 => "nil".into(),
        0x400 => {
            let w = x.cast::<u16>();
            format!(
                "sref:{},{},{},{}",
                *w.add(1),
                *w.add(2),
                *w.add(3) & 0xFF,
                *w.add(3) >> 8
            )
        }
        8 => {
            let m = *x.cast::<*const u8>();
            if m.is_null() {
                "ref:none".into()
            } else {
                let w = m.cast::<u16>();
                format!(
                    "ref:{},{},{},{}",
                    *w.add(1),
                    *w.add(2),
                    *w.add(3) & 0xFF,
                    *w.add(3) >> 8
                )
            }
        }
        0x40 => {
            let cells = *x.cast::<*const u8>();
            let rows = *x.add(8).cast::<u16>();
            let cols = *x.add(10).cast::<u16>();
            let mut parts = Vec::new();
            for i in 0..usize::from(rows) * usize::from(cols) {
                parts.push(describe_old(cells.add(24 * i)));
            }
            format!("multi:{rows}x{cols}[{}]", parts.join(";"))
        }
        other => format!("type{other:#x} {}", hex(x, 24)),
    };
    format!("{body}{free}")
}
#[no_mangle]
unsafe extern "system" fn XlcT_P(p: *const u8) -> *mut X {
    give_text(&format!("P {}", describe_old(p)))
}
#[no_mangle]
unsafe extern "system" fn XlcT_R(p: *const u8) -> *mut X {
    give_text(&format!("R {}", describe_old(p)))
}

static VOL: AtomicU32 = AtomicU32::new(0);
static CALC_ENDS: AtomicU32 = AtomicU32::new(0);
static LAST_CODE: AtomicU32 = AtomicU32::new(0);

#[no_mangle]
unsafe extern "system" fn XlcType(a: *const X) -> *mut X {
    give_text(&describe(a))
}

#[no_mangle]
unsafe extern "system" fn XlcDbl(a: f64) -> f64 {
    a * 2.0
}

#[no_mangle]
unsafe extern "system" fn XlcInt(a: i32) -> i32 {
    a * 2
}

static BOOL_OUT: AtomicU32 = AtomicU32::new(0);

#[no_mangle]
unsafe extern "system" fn XlcBool(a: *const i32) -> *const u32 {
    BOOL_OUT.store(u32::from(*a == 0), Ordering::SeqCst);
    BOOL_OUT.as_ptr()
}

#[no_mangle]
unsafe extern "system" fn XlcBoolIn(a: *const i32) -> f64 {
    f64::from(*a)
}

#[no_mangle]
unsafe extern "system" fn XlcBoolOut(a: f64) -> *const u32 {
    BOOL_OUT.store(u32::from(a != 0.0), Ordering::SeqCst);
    BOOL_OUT.as_ptr()
}

#[no_mangle]
unsafe extern "system" fn XlcAIn(a: i16) -> f64 {
    f64::from(a)
}

#[no_mangle]
unsafe extern "system" fn XlcAOut(a: f64) -> i16 {
    i16::from(a != 0.0)
}

#[no_mangle]
unsafe extern "system" fn XlcShort(a: i16) -> i16 {
    a.wrapping_mul(2)
}

#[no_mangle]
unsafe extern "system" fn XlcMix(a: f64, b: i32, s: *const u16) -> f64 {
    a * f64::from(b) + zstr(s).chars().count() as f64
}

thread_local! {
    static BUF: std::cell::RefCell<Vec<u16>> = const { std::cell::RefCell::new(Vec::new()) };
}

#[no_mangle]
unsafe extern "system" fn XlcStr(s: *const u16) -> *const u16 {
    let t = format!("{}!", zstr(s));
    BUF.with(|b| {
        let mut b = b.borrow_mut();
        *b = t.encode_utf16().collect();
        b.push(0);
        b.as_ptr()
    })
}

#[no_mangle]
unsafe extern "system" fn XlcCounted(s: *const u16) -> *const u16 {
    let t = format!("{}?", counted(s));
    BUF.with(|b| {
        let mut b = b.borrow_mut();
        let u: Vec<u16> = t.encode_utf16().collect();
        *b = vec![u.len() as u16];
        b.extend(u);
        b.as_ptr()
    })
}

#[no_mangle]
unsafe extern "system" fn XlcVol() -> *mut X {
    give(X::num(f64::from(VOL.fetch_add(1, Ordering::SeqCst) + 1)))
}

#[no_mangle]
unsafe extern "system" fn XlcEcho(a: *const X) -> *mut X {
    give(copy_value(a))
}

#[no_mangle]
unsafe extern "system" fn XlcArr() -> *mut X {
    let cells: Vec<X> = vec![X::num(1.0), X::text("b"), X::bool(true), X::err(7)];
    let p = Box::leak(cells.into_boxed_slice()).as_mut_ptr();
    let mut x = X::raw(MULTI);
    x.val[0] = p as u64;
    x.val[1] = 2 | (2 << 32);
    give(x)
}

#[no_mangle]
unsafe extern "system" fn XlcCaller() -> *mut X {
    let (code, mut res) = call(XLF_CALLER, &mut []);
    let s = format!("ret={code};{}", describe(&res));
    release(&mut res);
    give_text(&s)
}

#[no_mangle]
unsafe extern "system" fn XlcName() -> *mut X {
    let (code, mut res) = call(XL_GET_NAME, &mut []);
    let full = describe(&res);
    release(&mut res);
    let file = full.rsplit(['\\', '/']).next().unwrap_or("").to_string();
    give_text(&format!("ret={code};{file}"))
}

#[no_mangle]
unsafe extern "system" fn XlcCall(f: *const X, arg: *mut X) -> *mut X {
    let n = (*f).n() as i32;
    let mut opers: Vec<*mut X> = Vec::new();
    if (*arg).t & 0xFFF != MISSING {
        opers.push(arg);
    }
    let (code, mut res) = call(n, &mut opers);
    let s = format!("ret={code};{}", describe(&res));
    release(&mut res);
    give_text(&s)
}

#[no_mangle]
unsafe extern "system" fn XlcCallN(
    f: *const X,
    a: *mut X,
    b: *mut X,
    c: *mut X,
    d: *mut X,
) -> *mut X {
    let n = (*f).n() as i32;
    let mut opers: Vec<*mut X> = Vec::new();
    for p in [a, b, c, d] {
        if (*p).t & 0xFFF != MISSING {
            opers.push(p);
        }
    }
    let (code, mut res) = call(n, &mut opers);
    let s = format!("ret={code};{}", describe(&res));
    release(&mut res);
    give_text(&s)
}

#[no_mangle]
unsafe extern "system" fn XlcCallNM(
    f: *const X,
    a: *mut X,
    b: *mut X,
    c: *mut X,
    d: *mut X,
) -> *mut X {
    XlcCallN(f, a, b, c, d)
}

#[no_mangle]
unsafe extern "system" fn XlcRefTo(r: *const X, c: *const X) -> *mut X {
    let (row, col) = ((*r).n() as u64, (*c).n() as u64);
    let mut x = X::raw(SREF);
    x.val[0] = 1 | (row << 32);
    x.val[1] = row | (col << 32);
    x.val[2] = col;
    give(x)
}

#[no_mangle]
unsafe extern "system" fn XlcRefSheet(r: *const X, c: *const X, name: *mut X) -> *mut X {
    let (code, mut id) = call(XL_SHEET_ID, &mut [name]);
    if code != 0 {
        return give(X::err(15));
    }
    let (row, col) = ((*r).n() as u64, (*c).n() as u64);

    let mref: Vec<u64> = vec![1 | (row << 32), row | (col << 32), col];
    let p = Box::leak(mref.into_boxed_slice()).as_mut_ptr();
    let mut x = X::raw(REF);
    x.val[0] = p as u64;
    x.val[1] = id.val[1];
    release(&mut id);
    give(x)
}

#[no_mangle]
unsafe extern "system" fn XlcRefToV(r: *const X, c: *const X) -> *mut X {
    XlcRefTo(r, c)
}

#[no_mangle]
unsafe extern "system" fn XlcCallM(f: *const X, arg: *mut X) -> *mut X {
    XlcCall(f, arg)
}

#[no_mangle]
unsafe extern "system" fn XlcSheetId(name: *mut X) -> *mut X {
    let (code, mut res) = call(XL_SHEET_ID, &mut [name]);
    let s = format!("ret={code};{}", describe(&res));
    release(&mut res);
    give_text(&s)
}

#[no_mangle]
unsafe extern "system" fn XlcCoerce(a: *mut X, ty: *mut X) -> *mut X {
    let mut opers = vec![a];
    if (*ty).t & 0xFFF != MISSING {
        opers.push(ty);
    }
    let (code, mut res) = call(XL_COERCE, &mut opers);
    let s = format!("ret={code};{}", describe(&res));
    release(&mut res);
    give_text(&s)
}

#[no_mangle]
unsafe extern "system" fn XlcAsync(a: f64, handle: *mut X) {
    let mut result = X::num(a * 2.0);
    let mut args = [handle, &mut result as *mut X];
    let (code, mut res) = call(XL_ASYNC_RETURN, &mut args);
    release(&mut res);
    LAST_CODE.store(code as u32, Ordering::SeqCst);
}

#[no_mangle]
unsafe extern "system" fn XlcAsyncLater(delay: f64, a: f64, handle: *mut X) {
    let handle = *handle;
    std::thread::spawn(move || {
        std::thread::sleep(std::time::Duration::from_millis(delay as u64));
        let mut handle = handle;
        let mut result = X::num(a * 2.0);
        let mut args = [&mut handle as *mut X, &mut result as *mut X];

        let (_, mut res) = unsafe { call(XL_ASYNC_RETURN, &mut args) };
        unsafe { release(&mut res) };
    });
}

#[no_mangle]
unsafe extern "system" fn XlcRtd(a: *mut X, b: *mut X, c: *mut X, d: *mut X) -> *mut X {
    let mut opers: Vec<*mut X> = Vec::new();
    for p in [a, b, c, d] {
        opers.push(p);
    }
    while opers.last().is_some_and(|p| (**p).t & 0xFFF == MISSING) {
        opers.pop();
    }
    let (code, mut res) = call(379, &mut opers);
    let s = format!("ret={code};{}", describe(&res));
    release(&mut res);
    give_text(&s)
}

#[no_mangle]
unsafe extern "system" fn XlcRtdValue(a: *mut X, b: *mut X, c: *mut X, d: *mut X) -> *mut X {
    let mut opers: Vec<*mut X> = Vec::new();
    for p in [a, b, c, d] {
        opers.push(p);
    }
    while opers.last().is_some_and(|p| (**p).t & 0xFFF == MISSING) {
        opers.pop();
    }
    let (_, mut res) = call(379, &mut opers);

    let out = match res.t & 0xFFF {
        STR => X::text(&counted(res.val[0] as *const u16)),
        _ => X {
            t: res.t & 0xFFF,
            ..res
        },
    };
    release(&mut res);
    give(out)
}

#[no_mangle]
unsafe extern "system" fn XlcAsyncNever(_a: f64, _handle: *mut X) {}

#[no_mangle]
unsafe extern "system" fn XlcThreadCall(f: *const X) -> *mut X {
    let n = (*f).n() as i32;
    let text = std::thread::spawn(move || {
        let (code, mut res) = unsafe { call(n, &mut []) };
        let s = format!("ret={code};{}", describe(&res));
        unsafe { release(&mut res) };
        s
    })
    .join()
    .unwrap_or_else(|_| "panic".to_string());
    give_text(&text)
}

#[no_mangle]
unsafe extern "system" fn XlcLastCode() -> *mut X {
    give(X::num(f64::from(LAST_CODE.load(Ordering::SeqCst))))
}

#[no_mangle]
unsafe extern "system" fn XlcCalcEnds() -> *mut X {
    give(X::num(f64::from(CALC_ENDS.load(Ordering::SeqCst))))
}

#[no_mangle]
unsafe extern "system" fn XlcOnCalcEnd() -> i32 {
    CALC_ENDS.fetch_add(1, Ordering::SeqCst);
    1
}

fn log(line: &str) {
    use std::io::Write;
    let path = std::env::temp_dir().join("xlc_testxll.log");
    if let Ok(mut f) = std::fs::OpenOptions::new()
        .create(true)
        .append(true)
        .open(path)
    {
        let _ = writeln!(f, "{line}");
    }
}

#[link(name = "xlcall32", kind = "raw-dylib")]
extern "C" {
    fn Excel4(xlfn: i32, res: *mut XO, count: i32, ...) -> i32;
    fn Excel4v(xlfn: i32, res: *mut XO, count: i32, opers: *mut *mut XO) -> i32;
}

#[repr(C)]
#[derive(Clone, Copy)]
pub struct XO {
    val: [u64; 2],
    t: u16,
}

impl XO {
    fn raw(t: u16) -> XO {
        XO { val: [0; 2], t }
    }
    fn int(n: i16) -> XO {
        let mut x = XO::raw(0x800);
        x.val[0] = u64::from(n as u16);
        x
    }
    fn text(s: &str) -> XO {
        let mut b = Vec::with_capacity(s.len() + 1);
        b.push(s.len() as u8);
        b.extend(s.bytes());
        let mut x = XO::raw(2);
        x.val[0] = Box::leak(b.into_boxed_slice()).as_mut_ptr() as u64;
        x
    }
}

unsafe fn call4(xlfn: i32, opers: &mut [*mut XO], varargs: bool) -> (i32, XO) {
    let mut res = XO::raw(0x100);
    let n = opers.len();
    let code = if varargs {
        match n {
            0 => Excel4(xlfn, &mut res, 0),
            1 => Excel4(xlfn, &mut res, 1, opers[0]),
            2 => Excel4(xlfn, &mut res, 2, opers[0], opers[1]),
            3 => Excel4(xlfn, &mut res, 3, opers[0], opers[1], opers[2]),
            _ => Excel4(xlfn, &mut res, 4, opers[0], opers[1], opers[2], opers[3]),
        }
    } else {
        Excel4v(xlfn, &mut res, n as i32, opers.as_mut_ptr())
    };
    (code, res)
}

unsafe fn release4(res: &mut XO) {
    if res.t & 0x1000 != 0 {
        let mut p = res as *mut XO;
        Excel4v(XL_FREE, std::ptr::null_mut(), 1, &mut p);
    }
}

unsafe fn old_call(f: f64, a: *mut XO, b: *mut XO, c: *mut XO, d: *mut XO, va: bool) -> *mut X {
    let mut opers: Vec<*mut XO> = Vec::new();
    for p in [a, b, c, d] {
        if !p.is_null() && (*p).t & 0xFFF != 0x80 {
            opers.push(p);
        }
    }
    let (code, mut res) = call4(f as i32, &mut opers, va);
    let s = format!("ret={code};{}", describe_old((&res as *const XO).cast()));
    release4(&mut res);
    give_text(&s)
}

#[no_mangle]
unsafe extern "system" fn Xlc4Call(
    f: f64,
    a: *mut XO,
    b: *mut XO,
    c: *mut XO,
    d: *mut XO,
) -> *mut X {
    old_call(f, a, b, c, d, false)
}

#[no_mangle]
unsafe extern "system" fn Xlc4CallV(
    f: f64,
    a: *mut XO,
    b: *mut XO,
    c: *mut XO,
    d: *mut XO,
) -> *mut X {
    old_call(f, a, b, c, d, true)
}

#[no_mangle]
unsafe extern "system" fn Xlc4CallM(
    f: f64,
    a: *mut XO,
    b: *mut XO,
    c: *mut XO,
    d: *mut XO,
) -> *mut X {
    old_call(f, a, b, c, d, false)
}

#[no_mangle]
unsafe extern "system" fn Xlc4CallP(
    f: f64,
    a: *mut XO,
    b: *mut XO,
    c: *mut XO,
    d: *mut XO,
) -> *mut X {
    old_call(f, a, b, c, d, false)
}

unsafe fn register4(proc_name: &str, types: &str, name: &str) -> i32 {
    let (_, mut module) = call4(XL_GET_NAME, &mut [], false);
    let mut p = XO::text(proc_name);
    let mut t = XO::text(types);
    let mut n = XO::text(name);
    let mut args_text = XO::text("");
    let mut macro_type = XO::int(1);
    let mut cat = XO::text("XlcTest");
    let mut opers = [
        &mut module as *mut XO,
        &mut p,
        &mut t,
        &mut n,
        &mut args_text,
        &mut macro_type,
        &mut cat,
    ];
    let (code, mut res) = call4(XLF_REGISTER, &mut opers, false);
    release4(&mut res);
    release4(&mut module);
    code
}

unsafe fn register(proc_name: &str, types: &str, name: &str) -> i32 {
    let (_, mut module) = call(XL_GET_NAME, &mut []);
    let mut p = X::text(proc_name);
    let mut t = X::text(types);
    let mut n = X::text(name);
    let mut args_text = X::text("");
    let mut macro_type = X::int(1);
    let mut cat = X::text("XlcTest");
    let mut opers = [
        &mut module as *mut X,
        &mut p,
        &mut t,
        &mut n,
        &mut args_text,
        &mut macro_type,
        &mut cat,
    ];
    let (code, mut res) = call(XLF_REGISTER, &mut opers);
    release(&mut res);
    release(&mut module);
    code
}

#[no_mangle]
unsafe extern "system" fn xlAutoOpen() -> i32 {
    log("xlAutoOpen");
    for (p, t, n) in [
        ("XlcT_A", "QA", "XlcT_A"),
        ("XlcT_B", "QB", "XlcT_B"),
        ("XlcT_E", "QE", "XlcT_E"),
        ("XlcT_H", "QH", "XlcT_H"),
        ("XlcT_I", "QI", "XlcT_I"),
        ("XlcT_J", "QJ", "XlcT_J"),
        ("XlcT_L", "QL", "XlcT_L"),
        ("XlcT_C", "QC", "XlcT_C"),
        ("XlcT_D", "QD", "XlcT_D"),
        ("XlcT_Cw", "QC%", "XlcT_Cw"),
        ("XlcT_Dw", "QD%", "XlcT_Dw"),
        ("XlcT_K", "QK", "XlcT_K"),
        ("XlcT_Kw", "QK%", "XlcT_Kw"),
        ("XlcT_P", "QP", "XlcT_P"),
        ("XlcT_R", "QR", "XlcT_R"),
        ("XlcType", "QQ", "XlcType"),
        ("XlcType", "QQ", "XlcT_Type"),
        ("XlcType", "QU", "XlcT_TypeU"),
        ("XlcType", "QU", "XlcTypeU"),
        ("XlcDbl", "BB", "XlcDbl"),
        ("XlcInt", "JJ", "XlcInt"),
        ("XlcBool", "LL", "XlcBool"),
        ("XlcBoolIn", "BL", "XlcBoolIn"),
        ("XlcBoolOut", "LB", "XlcBoolOut"),
        ("XlcAIn", "BA", "XlcAIn"),
        ("XlcAOut", "AB", "XlcAOut"),
        ("XlcShort", "II", "XlcShort"),
        ("XlcMix", "BBJC%", "XlcMix"),
        ("XlcStr", "C%C%", "XlcStr"),
        ("XlcCounted", "D%D%", "XlcCounted"),
        ("XlcVol", "Q!", "XlcVol"),
        ("XlcEcho", "QQ$", "XlcEcho"),
        ("XlcArr", "Q", "XlcArr"),
        ("XlcCaller", "Q", "XlcCaller"),
        ("XlcCaller", "Q!", "XlcCallerVol"),
        ("XlcName", "Q", "XlcName"),
        ("XlcCall", "QQQ", "XlcCall"),
        ("XlcCallM", "QQQ#", "XlcCallMacro"),
        ("XlcCallN", "QQQQQQ", "XlcCallN"),
        ("XlcCallNM", "QQQQQQ#", "XlcCallNM"),
        ("XlcSheetId", "QQ", "XlcSheetId"),
        ("XlcRefTo", "UQQ", "XlcRefTo"),
        ("XlcRefToV", "UQQ!", "XlcRefToVol"),
        ("XlcRefSheet", "UQQQ", "XlcRefSheet"),
        ("XlcCoerce", "QUU", "XlcCoerce"),
        ("XlcAsync", ">BX", "XlcAsync"),
        ("XlcAsyncLater", ">BBX", "XlcAsyncLater"),
        ("XlcAsyncNever", ">BX", "XlcAsyncNever"),
        ("XlcRtd", "QQQQQ", "XlcRtd"),
        ("XlcRtdValue", "QQQQQ", "XlcRtdValue"),
        ("XlcThreadCall", "QQ", "XlcThreadCall"),
        ("XlcLastCode", "Q!", "XlcLastCode"),
        ("XlcCalcEnds", "Q!", "XlcCalcEnds"),
    ] {
        let code = register(p, t, n);
        log(&format!("register {n} {t} -> {code}"));
        if code != 0 {
            LAST_CODE.store(code as u32, Ordering::SeqCst);
        }
    }
    for (p, t, n) in [
        ("Xlc4Call", "QBRRRR", "Xlc4Call"),
        ("Xlc4CallV", "QBRRRR", "Xlc4CallV"),
        ("Xlc4CallM", "QBRRRR#", "Xlc4CallM"),
        ("Xlc4CallP", "QBPPPP", "Xlc4CallP"),
    ] {
        let code = register4(p, t, n);
        log(&format!("register4 {n} {t} -> {code}"));
        if code != 0 {
            LAST_CODE.store(code as u32, Ordering::SeqCst);
        }
    }
    let mut proc_name = X::text("XlcOnCalcEnd");
    let mut event = X::int(1);
    let mut opers = [&mut proc_name as *mut X, &mut event];
    let (_, mut res) = call(XL_EVENT_REGISTER, &mut opers);
    release(&mut res);
    1
}

#[no_mangle]
unsafe extern "system" fn xlAutoClose() -> i32 {
    1
}

#[no_mangle]
unsafe extern "system" fn xlAutoFree12(p: *mut X) {
    if p.is_null() {
        return;
    }
    let b = Box::from_raw(p);
    if b.t & 0xFFF == STR && b.val[0] != 0 {
        let s = b.val[0] as *mut u16;
        let n = usize::from(*s) + 1;
        drop(Box::from_raw(std::ptr::slice_from_raw_parts_mut(s, n)));
    }
}

#[no_mangle]
unsafe extern "system" fn xlAddInManagerInfo12(a: *const X) -> *mut X {
    let kind = copy_value(a);
    if kind.t & 0xFFF == NUM && kind.n() == 1.0 {
        give(X::text("Xlc Test XLL"))
    } else {
        give(X::err(15))
    }
}
