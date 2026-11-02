use std::cell::{Cell, RefCell};
use std::mem::ManuallyDrop;

use windows::core::{
    implement, IUnknown, IUnknownImpl, Interface, Ref, BOOL, BSTR, GUID, HRESULT, HSTRING, PCWSTR,
};
use windows::Win32::Foundation::{DISP_E_EXCEPTION, E_NOINTERFACE, S_FALSE, S_OK};
use windows::Win32::System::Com::{
    IClassFactory, IClassFactory_Impl, IConnectionPoint, IConnectionPointContainer,
    IConnectionPointContainer_Impl, IDispatch, IDispatch_Impl, IEnumConnectionPoints, IErrorInfo,
    ITypeInfo, ITypeLib, SetErrorInfo, DISPATCH_FLAGS, DISPPARAMS, EXCEPINFO,
};
use windows::Win32::System::Ole::{
    CreateErrorInfo, IEnumVARIANT, IEnumVARIANT_Impl, IProvideClassInfo2, IProvideClassInfo2_Impl,
    IProvideClassInfo_Impl, LoadRegTypeLib, GUIDKIND_DEFAULT_SOURCE_DISP_IID,
};
use windows::Win32::System::Variant::VARIANT;
use xlcalc_om::{typelib, Application, Call, EventSource, OmError};
use xlcalc_variant::{ObjectId, Variant};

use crate::events;
use crate::marshal::{bstr, clear, from_raw, to_raw, RawVariant};
use crate::server;

const DISPID_PROPERTYPUT: i32 = -3;

const EXCEL_TYPELIB: GUID = GUID::from_u128(0x00020813_0000_0000_c000_000000000046);

const VBIDE_TYPELIB: GUID = GUID::from_u128(0x0002e157_0000_0000_c000_000000000046);

thread_local! {
    static APP: RefCell<Option<Application>> = const { RefCell::new(None) };

    static PENDING: RefCell<Vec<ObjectId>> = const { RefCell::new(Vec::new()) };
    static TYPELIB: RefCell<Option<ITypeLib>> = const { RefCell::new(None) };
    static VBIDE: RefCell<Option<ITypeLib>> = const { RefCell::new(None) };

    static LENT: Cell<*mut Application<'static>> = const { Cell::new(std::ptr::null_mut()) };
}

static OPEN_MACROS: std::sync::atomic::AtomicBool = std::sync::atomic::AtomicBool::new(false);

pub(crate) fn set_open_macros(on: bool) {
    OPEN_MACROS.store(on, std::sync::atomic::Ordering::Relaxed);
}

static EXCEL_BOOKS: std::sync::atomic::AtomicBool = std::sync::atomic::AtomicBool::new(false);

pub(crate) fn set_excel_books(on: bool) {
    EXCEL_BOOKS.store(on, std::sync::atomic::Ordering::Relaxed);
}

struct ServerHost {
    open_macros: bool,
}

impl xlcalc_om::Host for ServerHost {
    fn run_open_macros(&self) -> bool {
        self.open_macros
    }
}

fn new_app() -> Application<'static> {
    let mut app = Application::new();
    app.set_host(Box::new(ServerHost {
        open_macros: OPEN_MACROS.load(std::sync::atomic::Ordering::Relaxed),
    }));
    if EXCEL_BOOKS.load(std::sync::atomic::Ordering::Relaxed) {
        app.reserve_book_numbers(&Application::AUTOMATION_BOOK_NUMBERS);
    }
    app
}

pub(crate) fn with_app<T>(f: impl FnOnce(&mut Application) -> T) -> T {
    let lent = LENT.with(Cell::get);
    if !lent.is_null() {
        return f(unsafe { &mut *lent });
    }
    APP.with(|cell| {
        let mut slot = cell.borrow_mut();
        let app = slot.get_or_insert_with(new_app);
        let out = f(app);
        for id in PENDING.with(|p| std::mem::take(&mut *p.borrow_mut())) {
            app.release(id);
        }
        out
    })
}

pub(crate) fn lend<T>(app: &mut Application<'_>, f: impl FnOnce() -> T) -> T {
    struct Restore(*mut Application<'static>);
    impl Drop for Restore {
        fn drop(&mut self) {
            LENT.with(|l| l.set(self.0));
        }
    }

    let p: *mut Application<'static> = (app as *mut Application<'_>).cast();
    let _restore = Restore(LENT.with(|l| l.replace(p)));
    f()
}

fn release(id: ObjectId) {
    APP.with(|cell| match cell.try_borrow_mut() {
        Ok(mut slot) => {
            if let Some(app) = slot.as_mut() {
                if id != app.handle() {
                    app.release(id);
                }
            }
        }
        Err(_) => PENDING.with(|p| p.borrow_mut().push(id)),
    })
}

pub(crate) fn is_dispatch(id: ObjectId) -> bool {
    with_app(|app| app.type_name(id) != Some("IEnumVARIANT"))
}

pub(crate) fn wrap(id: ObjectId) -> IUnknown {
    server::object_created();
    if !is_dispatch(id) {
        Enumerator { id }.into()
    } else if with_app(|app| app.event_source(id)).is_some() {
        Source { id }.into()
    } else {
        Dispatch { id }.into()
    }
}

pub(crate) fn handle_of(unk: &IUnknown) -> Option<ObjectId> {
    unk.cast_object_ref::<Dispatch>()
        .ok()
        .map(|d| d.id)
        .or_else(|| unk.cast_object_ref::<Source>().ok().map(|d| d.id))
}

pub(crate) fn application() -> IDispatch {
    let id = with_app(|app| app.handle());
    server::object_created();
    Source { id }.into()
}

#[implement(IDispatch, Agile = false)]
struct Dispatch {
    id: ObjectId,
}

impl Drop for Dispatch {
    fn drop(&mut self) {
        release(self.id);
        server::object_released();
    }
}

#[implement(
    IDispatch,
    IConnectionPointContainer,
    IProvideClassInfo2,
    Agile = false
)]
struct Source {
    id: ObjectId,
}

impl Drop for Source {
    fn drop(&mut self) {
        release(self.id);
        server::object_released();
    }
}

impl Source_Impl {
    fn source(&self) -> windows::core::Result<EventSource> {
        with_app(|app| app.event_source(self.id)).ok_or_else(events::no_interface)
    }

    fn point(&self) -> windows::core::Result<IConnectionPoint> {
        let source = self.source()?;
        let iid = events::iid(source);
        events::find(source, &iid, self.to_interface())
    }
}

impl IConnectionPointContainer_Impl for Source_Impl {
    fn EnumConnectionPoints(&self) -> windows::core::Result<IEnumConnectionPoints> {
        Ok(events::Points::create(self.point()?))
    }

    fn FindConnectionPoint(&self, riid: *const GUID) -> windows::core::Result<IConnectionPoint> {
        events::find(self.source()?, riid, self.to_interface())
    }
}

fn coclass(source: EventSource) -> GUID {
    match source {
        EventSource::Application => GUID::from_u128(0x00024500_0000_0000_c000_000000000046),
        EventSource::Workbook(_) => GUID::from_u128(0x00020819_0000_0000_c000_000000000046),
        EventSource::Worksheet(..) => GUID::from_u128(0x00020820_0000_0000_c000_000000000046),
    }
}

impl IProvideClassInfo_Impl for Source_Impl {
    fn GetClassInfo(&self) -> windows::core::Result<ITypeInfo> {
        let clsid = coclass(self.source()?);
        with_typelib(|tl| unsafe { tl.GetTypeInfoOfGuid(&clsid) })
    }
}

impl IProvideClassInfo2_Impl for Source_Impl {
    fn GetGUID(&self, kind: u32) -> windows::core::Result<GUID> {
        if kind != GUIDKIND_DEFAULT_SOURCE_DISP_IID.0 as u32 {
            return Err(hr(windows::Win32::Foundation::E_INVALIDARG.0));
        }
        Ok(events::iid(self.source()?))
    }
}

pub(crate) fn hr(code: i32) -> windows::core::Error {
    windows::core::Error::from_hresult(HRESULT(code))
}

fn type_info(name: &str) -> windows::core::Result<ITypeInfo> {
    let iid = typelib::interface(name)
        .map(|i| GUID::from_u128(i.iid.0))
        .ok_or_else(|| hr(E_NOINTERFACE.0))?;
    with_typelib(|tl| unsafe { tl.GetTypeInfoOfGuid(&iid) }).or_else(|_| {
        VBIDE.with(|cell| {
            let tl = loaded(cell, &VBIDE_TYPELIB, 5, 3)?;
            unsafe { tl.GetTypeInfoOfGuid(&iid) }
        })
    })
}

fn with_typelib<T>(
    f: impl FnOnce(&ITypeLib) -> windows::core::Result<T>,
) -> windows::core::Result<T> {
    TYPELIB.with(|cell| f(&loaded(cell, &EXCEL_TYPELIB, 1, 9)?))
}

fn loaded(
    cell: &RefCell<Option<ITypeLib>>,
    id: &GUID,
    major: u16,
    minor: u16,
) -> windows::core::Result<ITypeLib> {
    let mut slot = cell.borrow_mut();
    if slot.is_none() {
        *slot = Some(unsafe { LoadRegTypeLib(id, major, minor, 0)? });
    }
    Ok(slot.as_ref().unwrap().clone())
}

fn set_error_info(e: &OmError) {
    unsafe {
        let Ok(info) = CreateErrorInfo() else {
            return;
        };
        let _ = info.SetSource(&HSTRING::from(e.source.as_deref().unwrap_or_default()));
        let description = e.info_description.as_ref().or(e.description.as_ref());
        let _ = info.SetDescription(&HSTRING::from(description.map_or("", String::as_str)));
        if let Ok(info) = info.cast::<IErrorInfo>() {
            let _ = SetErrorInfo(0, &info);
        }
    }
}

fn allocated(s: &str) -> BSTR {
    unsafe { BSTR::from_raw(bstr(s)) }
}

unsafe fn report(e: &OmError, excep: *mut EXCEPINFO, arg_err: *mut u32) -> windows::core::Error {
    if e.hresult.0 == DISP_E_EXCEPTION.0 && !excep.is_null() {
        let x = &mut *excep;
        *x = EXCEPINFO::default();
        x.scode = e.scode;
        if let Some(s) = &e.source {
            x.bstrSource = ManuallyDrop::new(allocated(s));
        }
        if let Some(d) = &e.description {
            x.bstrDescription = ManuallyDrop::new(allocated(d));
        }
    }
    if e.error_info {
        set_error_info(e);
    }
    if let (Some(i), false) = (e.arg_err, arg_err.is_null()) {
        *arg_err = i;
    }
    hr(e.hresult.0)
}

macro_rules! dispatch_impl {
    ($t:ty) => {
        impl IDispatch_Impl for $t {
            fn GetTypeInfoCount(&self) -> windows::core::Result<u32> {
                Ok(1)
            }

            fn GetTypeInfo(&self, itinfo: u32, _lcid: u32) -> windows::core::Result<ITypeInfo> {
                get_type_info(self.id, itinfo)
            }

            fn GetIDsOfNames(
                &self,
                _riid: *const GUID,
                names: *const PCWSTR,
                count: u32,
                _lcid: u32,
                ids: *mut i32,
            ) -> windows::core::Result<()> {
                get_ids_of_names(self.id, names, count, ids)
            }

            fn Invoke(
                &self,
                dispid: i32,
                _riid: *const GUID,
                lcid: u32,
                flags: DISPATCH_FLAGS,
                params: *const DISPPARAMS,
                result: *mut VARIANT,
                excep: *mut EXCEPINFO,
                arg_err: *mut u32,
            ) -> windows::core::Result<()> {
                let out = Out {
                    result,
                    excep,
                    arg_err,
                };
                invoke(self.id, dispid, lcid, flags, params, out)
            }
        }
    };
}

dispatch_impl!(Dispatch_Impl);
dispatch_impl!(Source_Impl);

fn get_type_info(id: ObjectId, itinfo: u32) -> windows::core::Result<ITypeInfo> {
    if itinfo != 0 {
        return Err(hr(windows::Win32::Foundation::DISP_E_BADINDEX.0));
    }
    let name = with_app(|app| app.type_name(id)).ok_or_else(|| hr(E_NOINTERFACE.0))?;
    type_info(name)
}

fn get_ids_of_names(
    id: ObjectId,
    names: *const PCWSTR,
    count: u32,
    ids: *mut i32,
) -> windows::core::Result<()> {
    {
        if count == 0 || names.is_null() || ids.is_null() {
            return Err(hr(windows::Win32::Foundation::E_INVALIDARG.0));
        }
        let names: Vec<String> = (0..count as usize)
            .map(|i| unsafe { (*names.add(i)).to_string().unwrap_or_default() })
            .collect();
        let named: Vec<&str> = names[1..].iter().map(String::as_str).collect();
        let r = with_app(|app| app.get_ids_of_names(id, &names[0], &named));
        let out = unsafe { std::slice::from_raw_parts_mut(ids, count as usize) };
        match r {
            Ok(v) => {
                out.copy_from_slice(&v);
                Ok(())
            }
            Err(e) => {
                out.copy_from_slice(&e.ids);
                Err(hr(e.hresult.0))
            }
        }
    }
}

struct Out {
    result: *mut VARIANT,
    excep: *mut EXCEPINFO,
    arg_err: *mut u32,
}

fn invoke(
    id: ObjectId,
    dispid: i32,
    lcid: u32,
    flags: DISPATCH_FLAGS,
    params: *const DISPPARAMS,
    out: Out,
) -> windows::core::Result<()> {
    let Out {
        result,
        excep,
        arg_err,
    } = out;
    {
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
        let named_ids: &[i32] = if p.cNamedArgs == 0 || p.rgdispidNamedArgs.is_null() {
            &[]
        } else {
            unsafe { std::slice::from_raw_parts(p.rgdispidNamedArgs, p.cNamedArgs as usize) }
        };
        if named_ids.len() > raw.len() {
            return Err(hr(windows::Win32::Foundation::E_INVALIDARG.0));
        }

        let mut args = Vec::with_capacity(raw.len());
        for (i, r) in raw.iter().enumerate() {
            match unsafe { from_raw(r) } {
                Ok(v) => args.push(v),
                Err(e) => {
                    if !arg_err.is_null() {
                        unsafe { *arg_err = i as u32 };
                    }
                    return Err(hr(e.0));
                }
            }
        }
        let mut put = None;
        let mut named = Vec::new();
        for (i, &id) in named_ids.iter().enumerate() {
            if id == DISPID_PROPERTYPUT {
                put = Some(args[i].clone());
            } else {
                named.push((id, args[i].clone()));
            }
        }
        let mut positional: Vec<Variant> = args[named_ids.len()..].iter().rev().cloned().collect();
        positional.extend(put);
        let call = Call {
            flags: flags.0,
            lcid,
        };
        let r = with_app(|app| app.invoke_dispid(id, dispid, call, &positional, &named));
        match r {
            Ok(v) => {
                let raw = to_raw(&v).map_err(|e| hr(e.0))?;
                if result.is_null() {
                    let mut raw = raw;
                    unsafe { clear(&mut raw) };
                } else {
                    unsafe { *result.cast::<RawVariant>() = raw };
                }
                Ok(())
            }
            Err(e) => Err(unsafe { report(&e, excep, arg_err) }),
        }
    }
}

#[implement(IEnumVARIANT, Agile = false)]
struct Enumerator {
    id: ObjectId,
}

impl Drop for Enumerator {
    fn drop(&mut self) {
        release(self.id);
        server::object_released();
    }
}

impl IEnumVARIANT_Impl for Enumerator_Impl {
    fn Next(&self, celt: u32, rgvar: *mut VARIANT, fetched: *mut u32) -> HRESULT {
        let items = with_app(|app| app.enum_next(self.id, celt as usize));
        let mut n = 0;
        for v in &items {
            match to_raw(v) {
                Ok(raw) => {
                    unsafe { *rgvar.cast::<RawVariant>().add(n) = raw };
                    n += 1;
                }
                Err(e) => return HRESULT(e.0),
            }
        }
        if !fetched.is_null() {
            unsafe { *fetched = n as u32 };
        }
        if n == celt as usize {
            S_OK
        } else {
            S_FALSE
        }
    }

    fn Skip(&self, celt: u32) -> HRESULT {
        let got = with_app(|app| app.enum_next(self.id, celt as usize).len());
        if got == celt as usize {
            S_OK
        } else {
            S_FALSE
        }
    }

    fn Reset(&self) -> windows::core::Result<()> {
        with_app(|app| app.enum_reset(self.id));
        Ok(())
    }

    fn Clone(&self) -> windows::core::Result<IEnumVARIANT> {
        Err(hr(windows::Win32::Foundation::E_NOTIMPL.0))
    }
}

#[implement(IClassFactory, Agile = false)]
pub(crate) struct Factory;

impl IClassFactory_Impl for Factory_Impl {
    fn CreateInstance(
        &self,
        outer: Ref<IUnknown>,
        riid: *const GUID,
        out: *mut *mut core::ffi::c_void,
    ) -> windows::core::Result<()> {
        if out.is_null() || riid.is_null() {
            return Err(hr(windows::Win32::Foundation::E_POINTER.0));
        }
        unsafe { *out = std::ptr::null_mut() };
        if outer.is_some() {
            return Err(hr(windows::Win32::Foundation::CLASS_E_NOAGGREGATION.0));
        }
        let app = application();
        unsafe { app.query(riid, out).ok() }
    }

    fn LockServer(&self, lock: BOOL) -> windows::core::Result<()> {
        if lock.as_bool() {
            server::object_created();
        } else {
            server::object_released();
        }
        Ok(())
    }
}

pub(crate) fn factory() -> IClassFactory {
    Factory.into()
}
