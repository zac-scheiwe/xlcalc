use std::cell::{Cell, RefCell};
use std::collections::HashMap;

use windows::core::{implement, IUnknown, Interface, GUID, HRESULT, PCWSTR};
use windows::Win32::Foundation::{DISP_E_UNKNOWNNAME, E_NOTIMPL, TYPE_E_ELEMENTNOTFOUND};
use windows::Win32::System::Com::{
    CoCreateGuid, IDispatch, IDispatch_Impl, ITypeInfo, DISPATCH_FLAGS, DISPPARAMS, EXCEPINFO,
    SYS_WIN64, TKIND_DISPATCH,
};
use windows::Win32::System::Ole::{CreateTypeLib2, ICreateTypeLib};
use windows::Win32::System::Variant::VARIANT;
use xlcalc_variant::{ObjectId, Variant};

use crate::marshal::{clear, from_raw, to_raw, Objects, RawVariant};

pub trait Callback {
    fn ids(&mut self, id: ObjectId, name: &str) -> Result<i32, i32>;

    fn invoke(
        &mut self,
        id: ObjectId,
        dispid: i32,
        flags: u16,
        lcid: u32,
        args: &[Variant],
    ) -> Result<Variant, i32>;

    fn type_name(&mut self, _id: ObjectId) -> Option<String> {
        None
    }
}

thread_local! {
    static CURRENT: Cell<Option<*mut (dyn Callback + 'static)>> = const { Cell::new(None) };
}

pub fn with_callback<R>(cb: &mut dyn Callback, f: impl FnOnce() -> R) -> R {
    let ptr: *mut (dyn Callback + 'static) = unsafe { std::mem::transmute(cb) };
    struct Restore(Option<*mut (dyn Callback + 'static)>);
    impl Drop for Restore {
        fn drop(&mut self) {
            CURRENT.with(|c| c.set(self.0));
        }
    }
    let _restore = Restore(CURRENT.with(|c| c.replace(Some(ptr))));
    f()
}

fn current<R>(f: impl FnOnce(&mut dyn Callback) -> R) -> Option<R> {
    let p = CURRENT.with(Cell::get)?;

    Some(f(unsafe { &mut *p }))
}

#[implement(IDispatch)]
pub(crate) struct Proxy {
    pub(crate) id: ObjectId,
}

pub fn make(id: ObjectId) -> IUnknown {
    Proxy { id }.into()
}

pub(crate) fn id_of(unk: &IUnknown) -> Option<ObjectId> {
    unk.cast_object_ref::<Proxy>().ok().map(|p| p.id)
}

fn err(code: i32) -> windows::core::Error {
    windows::core::Error::from_hresult(HRESULT(code))
}

thread_local! {
    static TYPE_INFOS: RefCell<HashMap<String, ITypeInfo>> = RefCell::new(HashMap::new());
}

fn type_info(name: &str) -> windows::core::Result<ITypeInfo> {
    if let Some(t) = TYPE_INFOS.with(|m| m.borrow().get(name).cloned()) {
        return Ok(t);
    }
    let wide: Vec<u16> = name.encode_utf16().chain(Some(0)).collect();
    let file: Vec<u16> = "xlcalc.tlb".encode_utf16().chain(Some(0)).collect();

    let info = unsafe {
        let lib = CreateTypeLib2(SYS_WIN64, PCWSTR(file.as_ptr()))?;
        let lib: ICreateTypeLib = lib.cast()?;
        let ci = lib.CreateTypeInfo(PCWSTR(wide.as_ptr()), TKIND_DISPATCH)?;
        ci.SetGuid(&CoCreateGuid()?)?;

        ci.SetTypeFlags(0x1000)?;
        ci.LayOut()?;
        let info: ITypeInfo = ci.cast()?;

        std::mem::forget(lib);
        info
    };
    TYPE_INFOS.with(|m| m.borrow_mut().insert(name.to_owned(), info.clone()));
    Ok(info)
}

const FAILED: i32 = 0x800A03ECu32 as i32;

impl IDispatch_Impl for Proxy_Impl {
    fn GetTypeInfoCount(&self) -> windows::core::Result<u32> {
        Ok(u32::from(
            current(|cb| cb.type_name(self.id)).flatten().is_some(),
        ))
    }

    fn GetTypeInfo(&self, itinfo: u32, _lcid: u32) -> windows::core::Result<ITypeInfo> {
        if itinfo != 0 {
            return Err(err(TYPE_E_ELEMENTNOTFOUND.0));
        }
        match current(|cb| cb.type_name(self.id)).flatten() {
            Some(name) => type_info(&name),
            None => Err(err(E_NOTIMPL.0)),
        }
    }

    fn GetIDsOfNames(
        &self,
        _riid: *const GUID,
        names: *const PCWSTR,
        count: u32,
        _lcid: u32,
        ids: *mut i32,
    ) -> windows::core::Result<()> {
        if count == 0 || names.is_null() || ids.is_null() {
            return Err(err(windows::Win32::Foundation::E_INVALIDARG.0));
        }
        let out = unsafe { std::slice::from_raw_parts_mut(ids, count as usize) };
        out.fill(-1);
        let name = unsafe { (*names).to_string().unwrap_or_default() };
        match current(|cb| cb.ids(self.id, &name)) {
            Some(Ok(id)) if count == 1 => {
                out[0] = id;
                Ok(())
            }
            Some(Ok(id)) => {
                out[0] = id;
                Err(err(DISP_E_UNKNOWNNAME.0))
            }
            Some(Err(code)) => Err(err(code)),
            None => Err(err(DISP_E_UNKNOWNNAME.0)),
        }
    }

    fn Invoke(
        &self,
        dispid: i32,
        _riid: *const GUID,
        lcid: u32,
        flags: DISPATCH_FLAGS,
        params: *const DISPPARAMS,
        result: *mut VARIANT,
        _excep: *mut EXCEPINFO,
        _arg_err: *mut u32,
    ) -> windows::core::Result<()> {
        let empty = DISPPARAMS::default();
        let p = if params.is_null() {
            &empty
        } else {
            unsafe { &*params }
        };
        let raw: &[RawVariant] = if p.cArgs == 0 || p.rgvarg.is_null() {
            &[]
        } else {
            unsafe { std::slice::from_raw_parts(p.rgvarg.cast(), p.cArgs as usize) }
        };
        let no_known = |id: ObjectId| Some(make(id));
        let mut cx = Objects {
            known: &no_known,
            made: Vec::new(),
            next: 0,
        };

        let mut args = Vec::with_capacity(raw.len());
        for r in raw.iter().skip(p.cNamedArgs as usize).rev() {
            args.push(unsafe { from_raw(r, &mut cx) }.map_err(|e| err(e.0))?);
        }
        if p.cNamedArgs > 0 {
            for r in raw.iter().take(p.cNamedArgs as usize) {
                args.push(unsafe { from_raw(r, &mut cx) }.map_err(|e| err(e.0))?);
            }
        }
        let r = current(|cb| cb.invoke(self.id, dispid, flags.0, lcid, &args));
        match r {
            Some(Ok(v)) => {
                let mut raw = to_raw(&v, &mut cx).map_err(|e| err(e.0))?;
                if result.is_null() {
                    unsafe { clear(&mut raw) };
                } else {
                    unsafe { *result.cast::<RawVariant>() = raw };
                }
                Ok(())
            }
            Some(Err(code)) => Err(err(if code == 0 { FAILED } else { code })),
            None => Err(err(FAILED)),
        }
    }
}
