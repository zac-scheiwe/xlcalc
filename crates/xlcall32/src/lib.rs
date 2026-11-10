#![cfg(windows)]

use std::sync::atomic::{AtomicUsize, Ordering};

#[link(name = "kernel32", kind = "raw-dylib")]
extern "system" {
    fn GetModuleHandleW(name: *const u16) -> isize;
    fn GetProcAddress(module: isize, name: *const u8) -> usize;
}

pub enum Xloper {}

type MdCallBack = unsafe extern "system" fn(i32, i32, *mut *mut Xloper, *mut Xloper) -> i32;

const XLRET_FAILED: i32 = 32;

fn callback() -> Option<MdCallBack> {
    static CALLBACK: AtomicUsize = AtomicUsize::new(0);
    let mut cb = CALLBACK.load(Ordering::SeqCst);
    if cb == 0 {
        cb = unsafe {
            GetProcAddress(
                GetModuleHandleW(std::ptr::null()),
                c"MdCallBack".as_ptr().cast(),
            )
        };
        CALLBACK.store(cb, Ordering::SeqCst);
    }

    (cb != 0).then(|| unsafe { std::mem::transmute::<usize, MdCallBack>(cb) })
}

#[allow(clippy::missing_safety_doc)]
#[no_mangle]
pub unsafe extern "C" fn Excel4v(
    xlfn: i32,
    res: *mut Xloper,
    count: i32,
    opers: *mut *mut Xloper,
) -> i32 {
    match callback() {
        Some(f) => f(xlfn, count, opers, res),
        None => XLRET_FAILED,
    }
}

#[allow(clippy::missing_safety_doc)]
#[no_mangle]
#[allow(clippy::too_many_arguments)]
pub unsafe extern "C" fn Excel4(
    xlfn: i32,
    res: *mut Xloper,
    count: i32,
    a1: *mut Xloper,
    a2: *mut Xloper,
    a3: *mut Xloper,
    a4: *mut Xloper,
    a5: *mut Xloper,
    a6: *mut Xloper,
    a7: *mut Xloper,
    a8: *mut Xloper,
    a9: *mut Xloper,
    a10: *mut Xloper,
    a11: *mut Xloper,
    a12: *mut Xloper,
    a13: *mut Xloper,
    a14: *mut Xloper,
    a15: *mut Xloper,
    a16: *mut Xloper,
    a17: *mut Xloper,
    a18: *mut Xloper,
    a19: *mut Xloper,
    a20: *mut Xloper,
    a21: *mut Xloper,
    a22: *mut Xloper,
    a23: *mut Xloper,
    a24: *mut Xloper,
    a25: *mut Xloper,
    a26: *mut Xloper,
    a27: *mut Xloper,
    a28: *mut Xloper,
    a29: *mut Xloper,
    a30: *mut Xloper,
) -> i32 {
    let mut all = [
        a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12, a13, a14, a15, a16, a17, a18, a19, a20,
        a21, a22, a23, a24, a25, a26, a27, a28, a29, a30,
    ];
    let n = count.clamp(0, 30);
    Excel4v(xlfn, res, n, all.as_mut_ptr())
}

#[allow(clippy::missing_safety_doc)]
#[no_mangle]
pub extern "C" fn XLCallVer() -> i32 {
    0x0500
}
