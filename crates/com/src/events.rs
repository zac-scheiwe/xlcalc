use std::cell::{Cell, RefCell};
use std::collections::HashMap;
use std::sync::atomic::{AtomicU32, Ordering};
use std::sync::Arc;

use windows::core::{implement, IUnknown, Interface, Ref, GUID, HRESULT};
use windows::Win32::Foundation::{
    E_INVALIDARG, E_NOINTERFACE, E_NOTIMPL, E_POINTER, S_FALSE, S_OK,
};
use windows::Win32::System::Com::{
    IConnectionPoint, IConnectionPointContainer, IConnectionPoint_Impl, IDispatch,
    IEnumConnectionPoints, IEnumConnectionPoints_Impl, IEnumConnections, IEnumConnections_Impl,
    CONNECTDATA, DISPATCH_METHOD, DISPPARAMS, EXCEPINFO,
};
use windows::Win32::System::Ole::{CONNECT_E_CANNOTCONNECT, CONNECT_E_NOCONNECTION};
use windows::Win32::System::Variant::VARIANT;
use xlcalc_om::typelib::Ty;
use xlcalc_om::{Application, Event, EventSink, EventSource, SinkFailed};
use xlcalc_variant::Variant;

use crate::dispatch::{self, hr};
use crate::marshal::{self, RawVariant};
use crate::server;

thread_local! {

    static SINKS: RefCell<HashMap<u32, IDispatch>> = RefCell::new(HashMap::new());
}

pub(crate) fn iid(source: EventSource) -> GUID {
    GUID::from_u128(source.interface().iid.0)
}

struct ComSink {
    cookie: AtomicU32,
}

impl EventSink for ComSink {
    fn deliver(&self, app: &mut Application<'_>, event: &mut Event<'_>) -> Result<(), SinkFailed> {
        let Some(sink) = SINKS.with(|s| {
            s.borrow()
                .get(&self.cookie.load(Ordering::Relaxed))
                .cloned()
        }) else {
            return Ok(());
        };
        dispatch::lend(app, || call(&sink, event))
    }
}

fn call(sink: &IDispatch, event: &mut Event<'_>) -> Result<(), SinkFailed> {
    let m = event.member;
    let n = event.args.len();
    let mut bools: Vec<i16> = vec![0; n];
    let mut raw: Vec<RawVariant> = Vec::with_capacity(n);
    for (k, (p, v)) in m.params.iter().zip(event.args.iter()).enumerate() {
        if matches!(p.ty, Ty::ByRef(_)) {
            bools[k] = if matches!(v, Variant::Bool(true)) {
                -1
            } else {
                0
            };
            raw.push(RawVariant::default());
            continue;
        }

        let own = match v {
            Variant::Dispatch(Some(id)) => {
                Variant::Dispatch(dispatch::with_app(|app| app.duplicate(*id)))
            }
            other => other.clone(),
        };
        raw.push(marshal::to_raw(&own).unwrap_or_default());
    }
    for (k, p) in m.params.iter().enumerate().take(n) {
        if matches!(p.ty, Ty::ByRef(_)) {
            raw[k] = marshal::byref_bool(&mut bools[k]);
        }
    }
    let mut named: Vec<i32> = (0..n as i32).collect();
    let params = DISPPARAMS {
        rgvarg: raw.as_mut_ptr().cast::<VARIANT>(),
        rgdispidNamedArgs: named.as_mut_ptr(),
        cArgs: n as u32,
        cNamedArgs: n as u32,
    };
    let mut result = VARIANT::default();
    let mut excep = EXCEPINFO::default();
    let r = unsafe {
        sink.Invoke(
            m.dispid,
            &GUID::zeroed(),
            0x0409,
            DISPATCH_METHOD,
            &params,
            Some(&mut result),
            Some(&mut excep),
            None,
        )
    };
    for r in &mut raw {
        unsafe { marshal::clear(r) };
    }
    for (k, p) in m.params.iter().enumerate().take(n) {
        if matches!(p.ty, Ty::ByRef(_)) {
            event.args[k] = Variant::Bool(bools[k] != 0);
        }
    }

    unsafe {
        std::mem::ManuallyDrop::drop(&mut excep.bstrSource);
        std::mem::ManuallyDrop::drop(&mut excep.bstrDescription);
        std::mem::ManuallyDrop::drop(&mut excep.bstrHelpFile);
    }
    drop(result);
    r.map_err(|_| SinkFailed)
}

#[implement(IConnectionPoint, Agile = false)]
pub(crate) struct ConnectionPoint {
    source: EventSource,
    container: IConnectionPointContainer,
}

impl ConnectionPoint {
    pub(crate) fn create(
        source: EventSource,
        container: IConnectionPointContainer,
    ) -> IConnectionPoint {
        server::object_created();
        ConnectionPoint { source, container }.into()
    }
}

impl Drop for ConnectionPoint {
    fn drop(&mut self) {
        server::object_released();
    }
}

impl IConnectionPoint_Impl for ConnectionPoint_Impl {
    fn GetConnectionInterface(&self) -> windows::core::Result<GUID> {
        Ok(iid(self.source))
    }

    fn GetConnectionPointContainer(&self) -> windows::core::Result<IConnectionPointContainer> {
        Ok(self.container.clone())
    }

    fn Advise(&self, unk: Ref<IUnknown>) -> windows::core::Result<u32> {
        let unk = unk.ok().map_err(|_| hr(E_POINTER.0))?;

        let sink: IDispatch = unk.cast().map_err(|_| hr(CONNECT_E_CANNOTCONNECT.0))?;
        let com = Arc::new(ComSink {
            cookie: AtomicU32::new(0),
        });
        let cookie = dispatch::with_app(|app| app.advise(self.source, com.clone()));
        com.cookie.store(cookie, Ordering::Relaxed);
        SINKS.with(|s| s.borrow_mut().insert(cookie, sink));
        Ok(cookie)
    }

    fn Unadvise(&self, cookie: u32) -> windows::core::Result<()> {
        let mine = dispatch::with_app(|app| app.advised(self.source).contains(&cookie));
        if !mine || !dispatch::with_app(|app| app.unadvise(cookie)) {
            return Err(hr(CONNECT_E_NOCONNECTION.0));
        }

        let sink = SINKS.with(|s| s.borrow_mut().remove(&cookie));
        drop(sink);
        Ok(())
    }

    fn EnumConnections(&self) -> windows::core::Result<IEnumConnections> {
        let cookies = dispatch::with_app(|app| app.advised(self.source));
        let items = SINKS.with(|s| {
            let s = s.borrow();
            cookies
                .into_iter()
                .filter_map(|c| Some((s.get(&c)?.cast::<IUnknown>().ok()?, c)))
                .collect()
        });
        Ok(Connections::create(items, 0))
    }
}

#[implement(IEnumConnections, Agile = false)]
struct Connections {
    items: Vec<(IUnknown, u32)>,
    pos: Cell<usize>,
}

impl Connections {
    fn create(items: Vec<(IUnknown, u32)>, pos: usize) -> IEnumConnections {
        server::object_created();
        Connections {
            items,
            pos: Cell::new(pos),
        }
        .into()
    }
}

impl Drop for Connections {
    fn drop(&mut self) {
        server::object_released();
    }
}

impl IEnumConnections_Impl for Connections_Impl {
    fn Next(&self, count: u32, out: *mut CONNECTDATA, fetched: *mut u32) -> HRESULT {
        if out.is_null() {
            return E_POINTER;
        }
        let start = self.pos.get();
        let n = (count as usize).min(self.items.len() - start);
        for (k, (unk, cookie)) in self.items[start..start + n].iter().enumerate() {
            unsafe {
                out.add(k).write(CONNECTDATA {
                    pUnk: std::mem::ManuallyDrop::new(Some(unk.clone())),
                    dwCookie: *cookie,
                })
            };
        }
        self.pos.set(start + n);
        if !fetched.is_null() {
            unsafe { *fetched = n as u32 };
        }
        if n == count as usize {
            S_OK
        } else {
            S_FALSE
        }
    }

    fn Skip(&self, count: u32) -> windows::core::Result<()> {
        let start = self.pos.get();
        let n = (count as usize).min(self.items.len() - start);
        self.pos.set(start + n);
        if n == count as usize {
            Ok(())
        } else {
            Err(hr(S_FALSE.0))
        }
    }

    fn Reset(&self) -> windows::core::Result<()> {
        self.pos.set(0);
        Ok(())
    }

    fn Clone(&self) -> windows::core::Result<IEnumConnections> {
        Ok(Connections::create(self.items.clone(), self.pos.get()))
    }
}

#[implement(IEnumConnectionPoints, Agile = false)]
pub(crate) struct Points {
    point: IConnectionPoint,
    done: Cell<bool>,
}

impl Points {
    pub(crate) fn create(point: IConnectionPoint) -> IEnumConnectionPoints {
        server::object_created();
        Points {
            point,
            done: Cell::new(false),
        }
        .into()
    }
}

impl Drop for Points {
    fn drop(&mut self) {
        server::object_released();
    }
}

impl IEnumConnectionPoints_Impl for Points_Impl {
    fn Next(&self, count: u32, out: *mut Option<IConnectionPoint>, fetched: *mut u32) -> HRESULT {
        if out.is_null() {
            return E_POINTER;
        }
        let n = u32::from(count > 0 && !self.done.get());
        if n == 1 {
            unsafe { out.write(Some(self.point.clone())) };
            self.done.set(true);
        }
        if !fetched.is_null() {
            unsafe { *fetched = n };
        }
        if n == count {
            S_OK
        } else {
            S_FALSE
        }
    }

    fn Skip(&self, count: u32) -> windows::core::Result<()> {
        if count == 0 {
            return Ok(());
        }
        let had = !self.done.get();
        self.done.set(true);
        if had && count == 1 {
            Ok(())
        } else {
            Err(hr(S_FALSE.0))
        }
    }

    fn Reset(&self) -> windows::core::Result<()> {
        self.done.set(false);
        Ok(())
    }

    fn Clone(&self) -> windows::core::Result<IEnumConnectionPoints> {
        Err(hr(E_NOTIMPL.0))
    }
}

pub(crate) fn find(
    source: EventSource,
    riid: *const GUID,
    container: IConnectionPointContainer,
) -> windows::core::Result<IConnectionPoint> {
    if riid.is_null() {
        return Err(hr(E_INVALIDARG.0));
    }
    if unsafe { *riid } != iid(source) {
        return Err(hr(CONNECT_E_NOCONNECTION.0));
    }
    Ok(ConnectionPoint::create(source, container))
}

pub(crate) fn no_interface() -> windows::core::Error {
    hr(E_NOINTERFACE.0)
}
