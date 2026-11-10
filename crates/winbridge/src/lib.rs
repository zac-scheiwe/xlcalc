#![cfg(windows)]
#![allow(clippy::missing_safety_doc)]

pub mod marshal;
pub mod proxy;
pub mod registry;
pub mod rtd;

use std::ffi::c_void;

use marshal::{Objects, RawVariant};
use windows::core::{Interface, GUID, HSTRING, PCWSTR};
use windows::Win32::System::Com::{
    CLSIDFromProgID, CoCreateInstance, CoInitializeEx, IDispatch, CLSCTX_ALL,
    COINIT_APARTMENTTHREADED, DISPATCH_METHOD, DISPATCH_PROPERTYGET, DISPATCH_PROPERTYPUT,
    DISPATCH_PROPERTYPUTREF, DISPPARAMS, EXCEPINFO,
};
use windows::Win32::System::Variant::VARIANT;
use xlcalc_variant::{ObjectId, Variant};

pub use windows::core::IUnknown;

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum Access {
    Get,
    Put,
    PutRef,
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct BridgeError {
    pub number: i32,
    pub description: String,
}

impl BridgeError {
    fn new(number: i32, description: &str) -> BridgeError {
        BridgeError {
            number,
            description: description.to_string(),
        }
    }
}

const DISP_E_UNKNOWNNAME: i32 = 0x8002_0006_u32 as i32;
const DISP_E_MEMBERNOTFOUND: i32 = 0x8002_0003_u32 as i32;
const DISP_E_TYPEMISMATCH: i32 = 0x8002_0005_u32 as i32;
const DISP_E_EXCEPTION: i32 = 0x8002_0009_u32 as i32;
const DISP_E_BADPARAMCOUNT: i32 = 0x8002_000E_u32 as i32;
const DISP_E_PARAMNOTOPTIONAL: i32 = 0x8002_000F_u32 as i32;
const CO_E_CLASSSTRING: i32 = 0x8004_01F3_u32 as i32;
const REGDB_E_CLASSNOTREG: i32 = 0x8004_0154_u32 as i32;

fn hresult_error(hr: i32, excep: Option<&EXCEPINFO>) -> BridgeError {
    match hr {
        DISP_E_EXCEPTION => {
            let (code, scode, desc) = match excep {
                Some(e) => (i32::from(e.wCode), e.scode, e.bstrDescription.to_string()),
                None => (0, 0, String::new()),
            };
            let number = if code != 0 {
                code
            } else if (scode as u32) >> 16 == 0x800A {
                scode & 0xFFFF
            } else {
                scode
            };
            BridgeError {
                number,
                description: desc,
            }
        }
        DISP_E_MEMBERNOTFOUND | DISP_E_UNKNOWNNAME => {
            BridgeError::new(438, "Object doesn't support this property or method")
        }
        DISP_E_TYPEMISMATCH => BridgeError::new(13, "Type mismatch"),
        DISP_E_BADPARAMCOUNT | DISP_E_PARAMNOTOPTIONAL => BridgeError::new(
            450,
            "Wrong number of arguments or invalid property assignment",
        ),
        CO_E_CLASSSTRING | REGDB_E_CLASSNOTREG => {
            BridgeError::new(429, "ActiveX component can't create object")
        }
        _ => BridgeError {
            number: hr,
            description: format!("Automation error {hr:#010x}"),
        },
    }
}

pub fn init_com() {
    unsafe {
        let _ = CoInitializeEx(None, COINIT_APARTMENTTHREADED);
    }
}

pub struct Server(pub IDispatch);

unsafe impl Send for Server {}

fn clsid_of(progid: &str) -> Result<GUID, BridgeError> {
    let text = progid.trim();
    if text.starts_with('{') {
        return GUID::try_from(text.trim_matches(|c| c == '{' || c == '}'))
            .map_err(|_| BridgeError::new(429, "ActiveX component can't create object"));
    }
    unsafe { CLSIDFromProgID(&HSTRING::from(text)) }.map_err(|e| hresult_error(e.code().0, None))
}

pub fn create(progid: &str) -> Result<Server, BridgeError> {
    init_com();
    let clsid = clsid_of(progid)?;
    let unk: IUnknown = unsafe { CoCreateInstance(&clsid, None, CLSCTX_ALL) }
        .map_err(|e| hresult_error(e.code().0, None))?;
    let disp: IDispatch = unk
        .cast()
        .map_err(|_| BridgeError::new(430, "Class doesn't support Automation"))?;
    Ok(Server(disp))
}

pub struct Outcome {
    pub value: Variant,
    pub made: Vec<IUnknown>,
}

pub fn has_member(server: &IDispatch, member: &str) -> bool {
    let name = HSTRING::from(member);
    let names = [PCWSTR(name.as_ptr())];
    let mut id = 0i32;
    unsafe { server.GetIDsOfNames(&GUID::zeroed(), names.as_ptr(), 1, 0x0409, &mut id) }.is_ok()
}

pub fn invoke(
    server: &IDispatch,
    member: &str,
    access: Access,
    args: &[Variant],
    known: &dyn Fn(ObjectId) -> Option<IUnknown>,
    next: u64,
) -> Result<Outcome, BridgeError> {
    let mut cx = Objects {
        known,
        made: Vec::new(),
        next,
    };
    let dispid = if member.is_empty() {
        0
    } else {
        let name = HSTRING::from(member);
        let names = [PCWSTR(name.as_ptr())];
        let mut id = 0i32;
        unsafe { server.GetIDsOfNames(&GUID::zeroed(), names.as_ptr(), 1, 0x0409, &mut id) }
            .map_err(|e| hresult_error(e.code().0, None))?;
        id
    };

    let mut raw: Vec<RawVariant> = Vec::with_capacity(args.len() + 1);
    for a in args.iter().rev() {
        match marshal::to_raw(a, &mut cx) {
            Ok(r) => raw.push(r),
            Err(hr) => {
                for r in &mut raw {
                    unsafe { marshal::clear(r) };
                }
                return Err(hresult_error(hr.0, None));
            }
        }
    }
    let mut named = [-3i32];
    let put = access != Access::Get;
    let params = DISPPARAMS {
        rgvarg: raw.as_mut_ptr().cast::<VARIANT>(),
        rgdispidNamedArgs: if put {
            named.as_mut_ptr()
        } else {
            std::ptr::null_mut()
        },
        cArgs: raw.len() as u32,
        cNamedArgs: u32::from(put),
    };
    let flags = match access {
        Access::Get => DISPATCH_METHOD | DISPATCH_PROPERTYGET,
        Access::Put => DISPATCH_PROPERTYPUT,
        Access::PutRef => DISPATCH_PROPERTYPUTREF,
    };
    let mut result = VARIANT::default();
    let mut excep = EXCEPINFO::default();
    let r = unsafe {
        server.Invoke(
            dispid,
            &GUID::zeroed(),
            0x0409,
            flags,
            &params,
            Some(&mut result),
            Some(&mut excep),
            None,
        )
    };
    for r in &mut raw {
        unsafe { marshal::clear(r) };
    }
    let out = match r {
        Ok(()) => {
            let rv: &RawVariant = unsafe { &*(&result as *const VARIANT).cast::<RawVariant>() };
            unsafe { marshal::from_raw(rv, &mut cx) }.map_err(|hr| hresult_error(hr.0, None))
        }
        Err(e) => Err(hresult_error(e.code().0, Some(&excep))),
    };
    drop(result);
    out.map(|value| Outcome {
        value,
        made: cx.made,
    })
}

pub fn call_function(
    server: &IDispatch,
    member: &str,
    args: &[Variant],
    cb: &mut dyn proxy::Callback,
) -> Result<Variant, BridgeError> {
    let known = |id: ObjectId| Some(proxy::make(id));
    proxy::with_callback(cb, || invoke(server, member, Access::Get, args, &known, 0))
        .map(|o| o.value)
}

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum Extensibility {
    OnConnection,
    OnDisconnection,
    OnAddInsUpdate,
    OnStartupComplete,
    OnBeginShutdown,
}

pub const IID_EXTENSIBILITY2: GUID = GUID::from_u128(0xB65AD801_ABAF_11D0_BB8B_00A0C90F2744);

#[repr(C)]
struct Ext2Vtbl {
    base: [usize; 7],
    on_connection: unsafe extern "system" fn(
        *mut c_void,
        *mut c_void,
        i32,
        *mut c_void,
        *mut *mut c_void,
    ) -> i32,
    on_disconnection: unsafe extern "system" fn(*mut c_void, i32, *mut *mut c_void) -> i32,
    on_addins_update: unsafe extern "system" fn(*mut c_void, *mut *mut c_void) -> i32,
    on_startup_complete: unsafe extern "system" fn(*mut c_void, *mut *mut c_void) -> i32,
    on_begin_shutdown: unsafe extern "system" fn(*mut c_void, *mut *mut c_void) -> i32,
}

#[link(name = "oleaut32")]
extern "system" {
    fn SafeArrayCreateVector(vt: u16, lbound: i32, elements: u32) -> *mut c_void;
    fn SafeArrayDestroy(sa: *mut c_void) -> i32;
}

pub fn call_extensibility(
    server: &IDispatch,
    method: Extensibility,
    application: Option<&IDispatch>,
    mode: i32,
    add_in: Option<&IDispatch>,
) -> Result<bool, BridgeError> {
    let mut itf: *mut c_void = std::ptr::null_mut();
    let hr = unsafe {
        (Interface::vtable(server).base__.QueryInterface)(
            server.as_raw(),
            &IID_EXTENSIBILITY2,
            &mut itf,
        )
    };
    if hr.is_err() || itf.is_null() {
        return Ok(false);
    }

    let mut custom = unsafe { SafeArrayCreateVector(12, 0, 0) };
    let hr = unsafe {
        let vt = &**(itf as *mut *const Ext2Vtbl);
        let r = match method {
            Extensibility::OnConnection => (vt.on_connection)(
                itf,
                application.map_or(std::ptr::null_mut(), |a| a.as_raw()),
                mode,
                add_in.map_or(std::ptr::null_mut(), |a| a.as_raw()),
                &mut custom,
            ),
            Extensibility::OnDisconnection => (vt.on_disconnection)(itf, mode, &mut custom),
            Extensibility::OnAddInsUpdate => (vt.on_addins_update)(itf, &mut custom),
            Extensibility::OnStartupComplete => (vt.on_startup_complete)(itf, &mut custom),
            Extensibility::OnBeginShutdown => (vt.on_begin_shutdown)(itf, &mut custom),
        };

        let unk = *(itf as *mut *const [usize; 3]);
        let release: unsafe extern "system" fn(*mut c_void) -> u32 = std::mem::transmute((*unk)[2]);
        release(itf);
        if !custom.is_null() {
            SafeArrayDestroy(custom);
        }
        r
    };
    if hr < 0 {
        return Err(hresult_error(hr, None));
    }
    Ok(true)
}
