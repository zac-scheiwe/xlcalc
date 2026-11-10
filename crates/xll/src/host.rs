use std::cell::RefCell;
use std::collections::HashMap;
use std::sync::atomic::{AtomicU32, Ordering};
use std::sync::{Arc, Condvar, LazyLock, Mutex};
use std::time::{Duration, Instant};

use windows::core::{HSTRING, PCSTR};
use windows::Win32::Foundation::{FreeLibrary, HMODULE};
use windows::Win32::System::LibraryLoader::{
    GetModuleHandleW, GetProcAddress, LoadLibraryExW, LoadLibraryW, LOAD_WITH_ALTERED_SEARCH_PATH,
};
use xlcalc::{
    Arg, CallContext, CellAddr, FunctionHost, ParamKind, RuntimeError, Signature, Value, XlError,
};

use crate::args::{self, Arena};
use crate::dyncall;
use crate::sig::{self, Sig, Ty};
use crate::xloper::{self, Alloc, Xloper12, BIT_DLLFREE};
use crate::xloper_old::{self, Xloper};

#[derive(Debug, PartialEq, Eq)]
pub enum XllError {
    Load(u32),

    NoAutoOpen,
}

impl std::fmt::Display for XllError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            XllError::Load(code) => write!(f, "LoadLibrary failed ({code})"),
            XllError::NoAutoOpen => f.write_str("the library has no xlAutoOpen"),
        }
    }
}

pub(crate) struct Func {
    pub name: String,
    pub address: usize,
    pub sig: Sig,
}

pub(crate) struct Inner {
    pub path: String,
    module: Mutex<Option<usize>>,
    pub funcs: Mutex<Vec<Func>>,

    pub events: Mutex<Vec<(i32, usize)>>,
    next_id: AtomicU32,
}

static NEXT_HANDLE: AtomicU32 = AtomicU32::new(0);

static LATE: LazyLock<(Mutex<HashMap<u64, Value>>, Condvar)> =
    LazyLock::new(|| (Mutex::new(HashMap::new()), Condvar::new()));

const ASYNC_WAIT: Duration = Duration::from_secs(5);

pub(crate) fn deliver_late(handle: u64, value: Value) {
    LATE.0
        .lock()
        .unwrap_or_else(|e| e.into_inner())
        .insert(handle, value);
    LATE.1.notify_all();
}

fn wait_late(handle: u64) -> Option<Value> {
    let deadline = Instant::now() + ASYNC_WAIT;
    let mut map = LATE.0.lock().unwrap_or_else(|e| e.into_inner());
    loop {
        if let Some(v) = map.remove(&handle) {
            return Some(v);
        }
        let left = deadline.checked_duration_since(Instant::now())?;
        map = LATE
            .1
            .wait_timeout(map, left)
            .unwrap_or_else(|e| e.into_inner())
            .0;
    }
}

impl Inner {
    pub fn module(&self) -> Option<HMODULE> {
        self.module
            .lock()
            .ok()
            .and_then(|m| *m)
            .map(|p| HMODULE(p as *mut _))
    }

    pub fn symbol(&self, name: &str) -> Option<usize> {
        let m = self.module()?;
        let c = std::ffi::CString::new(name).ok()?;
        unsafe { GetProcAddress(m, PCSTR(c.as_ptr().cast())) }.map(|f| f as usize)
    }

    pub fn symbol_ordinal(&self, ordinal: u16) -> Option<usize> {
        let m = self.module()?;
        unsafe { GetProcAddress(m, PCSTR(usize::from(ordinal) as *const u8)) }.map(|f| f as usize)
    }

    pub fn register(&self, name: &str, address: usize, sig: Sig) -> u32 {
        let id = self.next_id.fetch_add(1, Ordering::SeqCst) + 1;
        let mut funcs = self.funcs.lock().unwrap_or_else(|e| e.into_inner());
        let upper = name.to_uppercase();
        funcs.retain(|f| f.name != upper);
        funcs.push(Func {
            name: upper,
            address,
            sig,
        });
        id
    }
}

pub(crate) struct Frame {
    pub inner: Arc<Inner>,
    pub wb: Option<*const xlcalc::Workbook>,
    pub caller: Option<CellAddr>,

    pub keep: Vec<Alloc>,
    pub keep_old: Vec<xloper_old::Alloc>,

    pub async_handle: Option<u64>,
    pub async_result: Option<Value>,

    pub macro_sheet: bool,

    pub used_rtd: bool,
}

thread_local! {
    pub(crate) static FRAMES: RefCell<Vec<Frame>> = const { RefCell::new(Vec::new()) };
}

struct FrameGuard;

impl FrameGuard {
    fn push(frame: Frame) -> FrameGuard {
        FRAMES.with(|f| f.borrow_mut().push(frame));
        FrameGuard
    }

    fn finish(self) -> Option<Value> {
        FRAMES.with(|f| f.borrow().last().and_then(|x| x.async_result.clone()))
    }
}

impl Drop for FrameGuard {
    fn drop(&mut self) {
        FRAMES.with(|f| {
            f.borrow_mut().pop();
        });
    }
}

fn ensure_xlcall32() {
    let name = HSTRING::from("xlcall32.dll");

    unsafe {
        if GetModuleHandleW(&name).is_ok() {
            return;
        }
        let Some(dir) = std::env::current_exe()
            .ok()
            .and_then(|e| e.parent().map(std::path::Path::to_path_buf))
        else {
            return;
        };
        let stub = dir.join("xlcall32.dll");
        if stub.exists() {
            let _ = LoadLibraryW(&HSTRING::from(stub.to_string_lossy().as_ref()));
        }
    }
}

#[derive(Clone)]
pub struct XllHost {
    pub(crate) inner: Arc<Inner>,
}

impl XllHost {
    pub fn load(path: &str) -> Result<XllHost, XllError> {
        ensure_xlcall32();

        let module =
            unsafe { LoadLibraryExW(&HSTRING::from(path), None, LOAD_WITH_ALTERED_SEARCH_PATH) }
                .map_err(|e| XllError::Load(e.code().0 as u32 & 0xFFFF))?;
        let inner = Arc::new(Inner {
            path: path.to_string(),
            module: Mutex::new(Some(module.0 as usize)),
            funcs: Mutex::new(Vec::new()),
            events: Mutex::new(Vec::new()),
            next_id: AtomicU32::new(0),
        });
        let host = XllHost { inner };
        let Some(open) = host.inner.symbol("xlAutoOpen") else {
            host.unload();
            return Err(XllError::NoAutoOpen);
        };
        let _guard = host.frame(None, None);
        unsafe { dyncall::call(open, &[]) };
        Ok(host)
    }

    fn frame(&self, wb: Option<*const xlcalc::Workbook>, caller: Option<CellAddr>) -> FrameGuard {
        FrameGuard::push(Frame {
            inner: self.inner.clone(),
            wb,
            caller,
            keep: Vec::new(),
            keep_old: Vec::new(),
            async_handle: None,
            async_result: None,
            macro_sheet: false,
            used_rtd: false,
        })
    }

    pub fn path(&self) -> &str {
        &self.inner.path
    }

    pub fn function_names(&self) -> Vec<String> {
        self.inner
            .funcs
            .lock()
            .map(|f| f.iter().map(|x| x.name.clone()).collect())
            .unwrap_or_default()
    }

    pub fn manager_info(&self) -> Option<String> {
        let f = self.inner.symbol("xlAddInManagerInfo12")?;
        let mut arg = Xloper12::number(1.0);
        let _guard = self.frame(None, None);
        let r = unsafe { dyncall::call(f, &[std::ptr::addr_of_mut!(arg) as u64]) };
        let value = unsafe { self.take_result(r.rax as *mut Xloper12) };
        match value {
            Some(Value::Text(t)) => Some(t.to_string()),
            _ => None,
        }
    }

    unsafe fn take_result(&self, p: *mut Xloper12) -> Option<Value> {
        if p.is_null() {
            return None;
        }
        let x = *p;
        let v = xloper::to_value(&x);
        if x.xltype & BIT_DLLFREE != 0 {
            if let Some(free) = self.inner.symbol("xlAutoFree12") {
                dyncall::call(free, &[p as u64]);
            }
        }
        v
    }

    pub fn fire_event(&self, event: i32) {
        let procs: Vec<usize> = self
            .inner
            .events
            .lock()
            .map(|e| e.iter().filter(|x| x.0 == event).map(|x| x.1).collect())
            .unwrap_or_default();
        let _guard = self.frame(None, None);
        for p in procs {
            unsafe { dyncall::call(p, &[]) };
        }
    }

    pub fn close(&self) {
        if let Some(f) = self.inner.symbol("xlAutoClose") {
            let _guard = self.frame(None, None);
            unsafe { dyncall::call(f, &[]) };
        }
        self.unload();
    }

    fn unload(&self) {
        if let Ok(mut m) = self.inner.module.lock() {
            if let Some(p) = m.take() {
                unsafe {
                    let _ = FreeLibrary(HMODULE(p as *mut _));
                }
            }
        }
        if let Ok(mut f) = self.inner.funcs.lock() {
            f.clear();
        }
    }

    fn func(&self, name: &str) -> Option<(usize, Sig)> {
        let funcs = self.inner.funcs.lock().ok()?;
        funcs
            .iter()
            .find(|f| f.name == name)
            .map(|f| (f.address, f.sig.clone()))
    }
}

impl FunctionHost for XllHost {
    fn signature(&self, name: &str) -> Option<Signature> {
        let (_, sig) = self.func(name)?;
        let declared = sig.args.len() - usize::from(sig.is_async);
        Some(Signature {
            params: vec![ParamKind::Reference; declared],
            rest: ParamKind::Value,
            volatile: sig.volatile,
        })
    }

    fn call(
        &self,
        name: &str,
        args: &[Arg],
        cx: &mut CallContext<'_>,
    ) -> Result<Value, RuntimeError> {
        let (address, sig) = self.func(name).ok_or(RuntimeError)?;
        let declared = sig.args.len() - usize::from(sig.is_async);
        if args.len() > declared {
            return Ok(Value::Error(XlError::VALUE));
        }
        let view = cx.view();
        let wb = view.workbook();
        let caller = view.caller();
        let mut arena = Arena::default();
        let mut slots: Vec<u64> = Vec::with_capacity(sig.args.len());
        for (i, ty) in sig.args.iter().enumerate() {
            let arg = args.get(i).unwrap_or(&Arg::Missing);
            if *ty == Ty::AsyncHandle {
                let handle = u64::from(NEXT_HANDLE.fetch_add(1, Ordering::SeqCst)) + 1;
                slots.push(arena.async_handle(handle));
                continue;
            }
            match args::slot(
                &mut arena,
                wb,
                caller,
                sig.thread_safe,
                cx.legacy_form(),
                *ty,
                arg,
            ) {
                Ok(s) => slots.push(s),
                Err(code) => return Ok(Value::Error(xloper::error_from_code(code))),
            }
        }
        let handle = arena.handle();
        let guard = self.frame(Some(wb as *const _), Some(caller));
        FRAMES.with(|f| {
            if let Some(top) = f.borrow_mut().last_mut() {
                top.async_handle = handle;
                top.macro_sheet = sig.macro_sheet;
            }
        });
        let r = unsafe { dyncall::call(address, &slots) };
        if FRAMES.with(|f| f.borrow().last().is_some_and(|x| x.used_rtd)) {
            cx.set_volatile(true);
        }
        let value = match sig.ret {
            Ty::Void => guard
                .finish()
                .or_else(|| handle.and_then(wait_late))
                .unwrap_or(Value::Error(XlError::NA)),
            ty => unsafe { self.returned(ty, r, wb, caller) },
        };
        Ok(value)
    }
}

impl XllHost {
    unsafe fn returned(
        &self,
        ty: Ty,
        r: dyncall::Returned,
        wb: &xlcalc::Workbook,
        caller: CellAddr,
    ) -> Value {
        match ty {
            Ty::Double => Value::Number(r.xmm0),
            Ty::Int => Value::Number(f64::from(r.rax as u32 as i32)),
            Ty::Short => Value::Number(f64::from(r.rax as u16 as i16)),
            Ty::Ushort => Value::Number(f64::from(r.rax as u16)),
            Ty::Short01 => Value::Bool(r.rax as u16 != 0),
            Ty::BoolPtr => {
                let p = r.rax as *const i32;
                if p.is_null() {
                    Value::Error(XlError::VALUE)
                } else {
                    Value::Bool(*p != 0)
                }
            }
            Ty::DoublePtr => {
                let p = r.rax as *const f64;
                if p.is_null() {
                    Value::Error(XlError::VALUE)
                } else {
                    Value::Number(*p)
                }
            }
            Ty::Wide => args::wide_text(r.rax as *const u16),
            Ty::WideCounted => args::counted_text(r.rax as *const u16),
            Ty::Ansi => args::ansi_text(r.rax as *const u8),
            Ty::AnsiCounted => args::ansi_counted_text(r.rax as *const u8),
            Ty::Oper | Ty::OperRef => {
                let p = r.rax as *mut Xloper12;
                if p.is_null() {
                    return Value::Error(XlError::VALUE);
                }
                let x = *p;
                let v = args::read(wb, caller, &x);
                if x.xltype & BIT_DLLFREE != 0 {
                    if let Some(free) = self.inner.symbol("xlAutoFree12") {
                        dyncall::call(free, &[p as u64]);
                    }
                }
                v
            }
            Ty::OperOld | Ty::OperRefOld => {
                let p = r.rax as *mut Xloper;
                if p.is_null() {
                    return Value::Error(XlError::VALUE);
                }
                let x = *p;
                let v = args::read_old(wb, caller, &x);
                if x.xltype & xloper_old::BIT_DLLFREE != 0 {
                    if let Some(free) = self.inner.symbol("xlAutoFree") {
                        dyncall::call(free, &[p as u64]);
                    }
                }
                v
            }
            Ty::Fp12 | Ty::Fp | Ty::AsyncHandle | Ty::Void => Value::Error(XlError::VALUE),
        }
    }
}

pub(crate) fn register(
    inner: &Inner,
    procedure: &Value,
    type_text: &str,
    name: &str,
) -> Option<u32> {
    let sig = sig::parse(type_text)?;
    let address = match procedure {
        Value::Text(t) => inner.symbol(t)?,
        Value::Number(n) if *n >= 1.0 && *n <= f64::from(u16::MAX) => {
            inner.symbol_ordinal(*n as u16)?
        }
        _ => return None,
    };
    if name.is_empty() {
        return None;
    }
    Some(inner.register(name, address, sig))
}
