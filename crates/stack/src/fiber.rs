use std::any::Any;
use std::cell::Cell;
use std::ffi::c_void;
use std::panic::{catch_unwind, resume_unwind, AssertUnwindSafe};
use std::ptr::null_mut;

#[link(name = "kernel32")]
extern "system" {
    fn ConvertThreadToFiberEx(parameter: *mut c_void, flags: u32) -> *mut c_void;
    fn ConvertFiberToThread() -> i32;
    fn CreateFiberEx(
        commit: usize,
        reserve: usize,
        flags: u32,
        start: unsafe extern "system" fn(*mut c_void),
        parameter: *mut c_void,
    ) -> *mut c_void;
    fn DeleteFiber(fiber: *mut c_void);
    fn SwitchToFiber(fiber: *mut c_void);
    fn IsThreadAFiber() -> i32;
    fn GetCurrentThreadStackLimits(low: *mut usize, high: *mut usize);
}

const FIBER_FLAG_FLOAT_SWITCH: u32 = 1;

const MARGIN: usize = 16 * 1024;

const FOREIGN_ROOM: usize = 256 * 1024;

type Panic = Box<dyn Any + Send>;

struct Link {
    parent: *mut c_void,
    body: Cell<Option<*mut dyn FnMut()>>,

    job: Cell<Option<*mut dyn FnMut()>>,
}

thread_local! {

    static LIMIT: Cell<usize> = const { Cell::new(0) };

    static LINK: Cell<*const Link> = const { Cell::new(std::ptr::null()) };
}

fn stack_limit() -> usize {
    let (mut low, mut high) = (0, 0);

    unsafe { GetCurrentThreadStackLimits(&mut low, &mut high) };
    low + MARGIN
}

fn remaining() -> usize {
    let limit = LIMIT.with(|l| {
        if l.get() == 0 {
            l.set(stack_limit());
        }
        l.get()
    });
    let local = 0u8;
    (std::hint::black_box(std::ptr::addr_of!(local)) as usize).saturating_sub(limit)
}

fn current_fiber() -> *mut c_void {
    let fiber: *mut c_void;

    unsafe {
        #[cfg(target_arch = "x86_64")]
        std::arch::asm!("mov {}, gs:[0x20]", out(reg) fiber, options(nostack, readonly));
        #[cfg(target_arch = "aarch64")]
        std::arch::asm!("ldr {}, [x18, #0x20]", out(reg) fiber, options(nostack, readonly));
        #[cfg(target_arch = "x86")]
        std::arch::asm!("mov {}, fs:[0x10]", out(reg) fiber, options(nostack, readonly));
    }
    fiber
}

#[inline]
pub fn maybe_grow<R>(red_zone: usize, stack_size: usize, f: impl FnOnce() -> R) -> R {
    let red_zone = if LINK.get().is_null() {
        red_zone.max(FOREIGN_ROOM)
    } else {
        red_zone
    };
    if remaining() >= red_zone {
        f()
    } else {
        grow(stack_size, f)
    }
}

pub fn grow<R>(stack_size: usize, f: impl FnOnce() -> R) -> R {
    let mut f = Some(f);
    let mut out: Option<Result<R, Panic>> = None;
    run_on_fiber(stack_size, &mut || {
        if let Some(f) = f.take() {
            out = Some(catch_unwind(AssertUnwindSafe(f)));
        }
    });
    finish(out)
}

pub fn on_thread_stack<R>(f: impl FnOnce() -> R) -> R {
    let link = LINK.get();
    if link.is_null() {
        return f();
    }
    let mut f = Some(f);
    let mut out: Option<Result<R, Panic>> = None;
    let mut job = || {
        if let Some(f) = f.take() {
            out = Some(catch_unwind(AssertUnwindSafe(|| on_thread_stack(f))));
        }
    };
    let job: &mut dyn FnMut() = &mut job;
    let limit = LIMIT.get();

    unsafe {
        (*link).job.set(Some(erase(job)));
        SwitchToFiber((*link).parent);
    }
    LINK.set(link);
    LIMIT.set(limit);
    finish(out)
}

fn finish<R>(out: Option<Result<R, Panic>>) -> R {
    match out {
        Some(Ok(r)) => r,
        Some(Err(p)) => resume_unwind(p),
        None => unreachable!("the closure was not run"),
    }
}

unsafe fn erase(f: &mut dyn FnMut()) -> *mut dyn FnMut() {
    std::mem::transmute::<*mut (dyn FnMut() + '_), *mut (dyn FnMut() + 'static)>(f)
}

unsafe extern "system" fn start(parameter: *mut c_void) {
    let link = parameter.cast_const().cast::<Link>();
    LINK.set(link);
    LIMIT.set(stack_limit());
    if let Some(body) = (*link).body.take() {
        (*body)();
    }

    SwitchToFiber((*link).parent);
}

fn run_on_fiber(stack_size: usize, body: &mut dyn FnMut()) {
    unsafe {
        let was_fiber = IsThreadAFiber() != 0;
        let parent = if was_fiber {
            current_fiber()
        } else {
            ConvertThreadToFiberEx(null_mut(), FIBER_FLAG_FLOAT_SWITCH)
        };
        assert!(
            !parent.is_null(),
            "unable to convert the thread to a fiber: {}",
            std::io::Error::last_os_error()
        );
        let link = Link {
            parent,
            body: Cell::new(Some(erase(body))),
            job: Cell::new(None),
        };
        let fiber = CreateFiberEx(
            0,
            stack_size,
            FIBER_FLAG_FLOAT_SWITCH,
            start,
            std::ptr::addr_of!(link).cast_mut().cast(),
        );
        if fiber.is_null() {
            let error = std::io::Error::last_os_error();
            if !was_fiber {
                ConvertFiberToThread();
            }
            panic!("unable to allocate a stack of {stack_size} bytes: {error}");
        }
        let (outer, limit) = (LINK.get(), LIMIT.get());
        loop {
            SwitchToFiber(fiber);
            LINK.set(outer);
            LIMIT.set(limit);
            match link.job.take() {
                Some(job) => (*job)(),
                None => break,
            }
        }
        DeleteFiber(fiber);
        if !was_fiber {
            ConvertFiberToThread();
        }
    }
}
