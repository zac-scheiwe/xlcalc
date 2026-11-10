use windows::core::{implement, Interface, GUID, HRESULT, PCWSTR};
use windows::Win32::Foundation::{DISP_E_MEMBERNOTFOUND, E_NOTIMPL, S_OK};
use windows::Win32::System::Com::{
    IDispatch, IDispatch_Impl, IDispatch_Vtbl, ITypeInfo, DISPATCH_FLAGS, DISPPARAMS, EXCEPINFO,
};
use windows::Win32::System::Variant::VARIANT;
use xlcalc_variant::{ObjectId, SafeArray, VarType, Variant};

use crate::{create, invoke, Access, BridgeError, Server};

#[implement(IRTDUpdateEvent)]
struct UpdateEvent;

#[windows::core::interface("A43788C1-D91B-11D3-8F39-00C04F3651B8")]
unsafe trait IRTDUpdateEvent: IDispatch {
    fn update_notify(&self) -> HRESULT;
    fn heartbeat_interval(&self, interval: *mut i32) -> HRESULT;
    fn set_heartbeat_interval(&self, interval: i32) -> HRESULT;
    fn disconnect(&self) -> HRESULT;
}

impl IRTDUpdateEvent_Impl for UpdateEvent_Impl {
    unsafe fn update_notify(&self) -> HRESULT {
        S_OK
    }

    unsafe fn heartbeat_interval(&self, interval: *mut i32) -> HRESULT {
        unsafe { *interval = 60_000 };
        S_OK
    }

    unsafe fn set_heartbeat_interval(&self, _: i32) -> HRESULT {
        S_OK
    }

    unsafe fn disconnect(&self) -> HRESULT {
        S_OK
    }
}

impl IDispatch_Impl for UpdateEvent_Impl {
    fn GetTypeInfoCount(&self) -> windows::core::Result<u32> {
        Ok(0)
    }

    fn GetTypeInfo(&self, _: u32, _: u32) -> windows::core::Result<ITypeInfo> {
        Err(E_NOTIMPL.into())
    }

    fn GetIDsOfNames(
        &self,
        _: *const GUID,
        names: *const PCWSTR,
        count: u32,
        _: u32,
        ids: *mut i32,
    ) -> windows::core::Result<()> {
        for i in 0..count as usize {
            let name = unsafe { (*names.add(i)).to_string() }.unwrap_or_default();
            let id = match name.to_ascii_lowercase().as_str() {
                "updatenotify" => 10,
                "heartbeatinterval" => 11,
                "disconnect" => 12,
                _ => return Err(DISP_E_MEMBERNOTFOUND.into()),
            };
            unsafe { *ids.add(i) = id };
        }
        Ok(())
    }

    fn Invoke(
        &self,
        _: i32,
        _: *const GUID,
        _: u32,
        _: DISPATCH_FLAGS,
        _: *const DISPPARAMS,
        _: *mut VARIANT,
        _: *mut EXCEPINFO,
        _: *mut u32,
    ) -> windows::core::Result<()> {
        Ok(())
    }
}

pub struct RtdServer {
    server: Server,
    next_topic: i32,
}

fn bstrs(texts: &[String]) -> Variant {
    Variant::Array(Box::new(SafeArray {
        elem: VarType::Variant,
        bounds: vec![(0, texts.len() as u32)],
        data: texts.iter().map(|t| Variant::Bstr(t.clone())).collect(),
    }))
}

impl RtdServer {
    pub fn start(prog_id: &str) -> Result<RtdServer, BridgeError> {
        let server = create(prog_id)?;
        let callback: IRTDUpdateEvent = UpdateEvent.into();
        let unknown: windows::core::IUnknown = callback.cast().map_err(|_| BridgeError {
            number: 430,
            description: "Class doesn't support Automation".into(),
        })?;
        let known = |id: ObjectId| (id.0 == 1).then(|| unknown.clone());
        invoke(
            &server.0,
            "ServerStart",
            Access::Get,
            &[Variant::Dispatch(Some(ObjectId(1)))],
            &known,
            2,
        )?;
        Ok(RtdServer {
            server,
            next_topic: 2,
        })
    }

    pub fn connect(&mut self, topics: &[String]) -> Result<(i32, Variant), BridgeError> {
        let id = self.next_topic;
        self.next_topic += 1;
        let no_objects = |_: ObjectId| None;
        let out = invoke(
            &self.server.0,
            "ConnectData",
            Access::Get,
            &[Variant::I4(id), bstrs(topics), Variant::Bool(true)],
            &no_objects,
            1,
        )?;
        Ok((id, out.value))
    }

    pub fn refresh(&mut self) -> Result<Vec<(i32, Variant)>, BridgeError> {
        let no_objects = |_: ObjectId| None;
        let out = invoke(
            &self.server.0,
            "RefreshData",
            Access::Get,
            &[Variant::I4(0)],
            &no_objects,
            1,
        )?;
        let Variant::Array(a) = out.value else {
            return Ok(Vec::new());
        };

        let cols = a.bounds.get(1).map_or(0, |b| b.1) as usize;
        let mut changed = Vec::with_capacity(cols);
        for c in 0..cols {
            if let (Some(Variant::I4(id)), Some(v)) = (a.data.get(2 * c), a.data.get(2 * c + 1)) {
                changed.push((*id, v.clone()));
            }
        }
        Ok(changed)
    }

    pub fn disconnect(&mut self, topic: i32) {
        let no_objects = |_: ObjectId| None;
        let _ = invoke(
            &self.server.0,
            "DisconnectData",
            Access::Get,
            &[Variant::I4(topic)],
            &no_objects,
            1,
        );
    }

    pub fn terminate(&mut self) {
        let no_objects = |_: ObjectId| None;
        let _ = invoke(
            &self.server.0,
            "ServerTerminate",
            Access::Get,
            &[],
            &no_objects,
            1,
        );
    }
}
