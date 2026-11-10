use xlcalc_variant::{HResult, ObjectId, Variant};
use xlcalc_vba::{InvokeKind as VbaKind, VbaError};

use crate::app::{unsupported, Application, Object};
use crate::conv::Args;
use crate::error::OmError;
use crate::typelib::{InvokeKind, Member};

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct RegisteredAddin {
    pub prog_id: String,

    pub friendly_name: String,
    pub description: String,

    pub guid: String,

    pub load_behavior: u32,
}

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum ExtCall {
    OnConnection {
        mode: i32,
        application: Option<ObjectId>,
        add_in: Option<ObjectId>,
    },

    OnDisconnection {
        mode: i32,
    },
    OnAddInsUpdate,
    OnStartupComplete,
    OnBeginShutdown,
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct AutomationServer {
    pub prog_id: String,

    pub file: String,

    pub title: String,
}

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum CallFailure {
    Overflow,
    Value,
}

pub trait ObjectCallback {
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

pub trait ComBridge: Send {
    fn registered_addins(&mut self) -> Vec<RegisteredAddin>;

    fn create(&mut self, prog_id: &str) -> Result<u64, VbaError>;

    fn invoke(
        &mut self,
        handle: u64,
        member: &str,
        kind: VbaKind,
        args: &[Variant],
    ) -> Result<Variant, VbaError>;

    fn extensibility(&mut self, handle: u64, call: ExtCall) -> Result<bool, VbaError>;

    fn release(&mut self, handle: u64);

    fn automation_servers(&mut self) -> Vec<AutomationServer>;

    fn has_member(&mut self, handle: u64, name: &str) -> bool;

    fn call_function(
        &mut self,
        handle: u64,
        name: &str,
        args: &[Variant],
        _cb: &mut dyn ObjectCallback,
    ) -> Result<Variant, CallFailure> {
        self.invoke(handle, name, VbaKind::Get, args).map_err(|e| {
            if e.number == HResult::DISP_E_OVERFLOW.0 {
                CallFailure::Overflow
            } else {
                CallFailure::Value
            }
        })
    }
}

#[derive(Debug)]
struct Entry {
    reg: RegisteredAddin,

    server: Option<u64>,

    object: Variant,
}

pub(crate) type SharedBridge = std::sync::Arc<std::sync::Mutex<Box<dyn ComBridge>>>;

#[derive(Default)]
pub(crate) struct ComAddIns {
    bridge: Option<SharedBridge>,
    entries: Vec<Entry>,

    pub(crate) automation: std::sync::Arc<crate::automation::Automation>,
}

impl std::fmt::Debug for ComAddIns {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.debug_struct("ComAddIns")
            .field("entries", &self.entries)
            .finish()
    }
}

const DISP_E_BADINDEX: HResult = HResult(0x8002_000B_u32 as i32);
const E_ABORT: HResult = HResult(0x8000_4004_u32 as i32);
const E_FAIL: HResult = HResult(0x8000_4005_u32 as i32);

impl Application<'_> {
    pub fn set_com_bridge(&mut self, mut bridge: Box<dyn ComBridge>) {
        let regs = bridge.registered_addins();
        self.com_addins.entries = regs
            .into_iter()
            .map(|reg| Entry {
                reg,
                server: None,
                object: Variant::Dispatch(None),
            })
            .collect();
        let shared: SharedBridge = std::sync::Arc::new(std::sync::Mutex::new(bridge));
        self.com_addins.automation.set_bridge(shared.clone());
        self.com_addins.bridge = Some(shared);
        self.discover_automation();
        for i in 0..self.com_addins.entries.len() {
            if self.com_addins.entries[i].reg.load_behavior & 1 != 0 {
                let _ = self.com_connect(i, 1);
            }
        }
        let connected: Vec<usize> = self.connected_addins();
        for i in connected {
            self.ext_call(i, ExtCall::OnStartupComplete);
        }
    }

    pub(crate) fn with_bridge<R>(&self, f: impl FnOnce(&mut dyn ComBridge) -> R) -> Option<R> {
        let shared = self.com_addins.bridge.as_ref()?;
        let mut guard = shared.try_lock().ok()?;
        Some(f(guard.as_mut()))
    }

    pub fn has_com_bridge(&self) -> bool {
        self.com_addins.bridge.is_some()
    }

    fn connected_addins(&self) -> Vec<usize> {
        (0..self.com_addins.entries.len())
            .filter(|&i| self.com_addins.entries[i].server.is_some())
            .collect()
    }

    fn ext_call(&mut self, i: usize, call: ExtCall) -> bool {
        let Some(h) = self.com_addins.entries[i].server else {
            return false;
        };
        self.with_bridge(|b| b.extensibility(h, call))
            .is_some_and(|r| r.is_ok())
    }

    fn com_connect(&mut self, i: usize, mode: i32) -> Result<(), OmError> {
        if self.com_addins.entries[i].server.is_some() {
            return Ok(());
        }
        let abort = || OmError::hresult(E_ABORT);
        let prog_id = self.com_addins.entries[i].reg.prog_id.clone();
        let h = self
            .with_bridge(|b| b.create(&prog_id))
            .and_then(Result::ok)
            .ok_or_else(abort)?;
        self.com_addins.entries[i].server = Some(h);
        let app = self.handle();
        let add_in = match self.alloc(Object::ComAddIn(prog_id.to_lowercase())) {
            Variant::Dispatch(Some(id)) => Some(id),
            _ => None,
        };
        let ok = self.ext_call(
            i,
            ExtCall::OnConnection {
                mode,
                application: Some(app),
                add_in,
            },
        );
        if !ok {
            self.com_release(i);
            return Err(abort());
        }
        for j in self.connected_addins() {
            self.ext_call(j, ExtCall::OnAddInsUpdate);
        }
        Ok(())
    }

    fn com_release(&mut self, i: usize) {
        let e = &mut self.com_addins.entries[i];
        e.object = Variant::Dispatch(None);
        if let Some(h) = e.server.take() {
            self.with_bridge(|b| b.release(h));
        }
    }

    fn com_disconnect(&mut self, i: usize) -> Result<(), OmError> {
        if self.com_addins.entries[i].server.is_none() {
            let prog_id = self.com_addins.entries[i].reg.prog_id.clone();
            self.with_bridge(|b| {
                if let Ok(h) = b.create(&prog_id) {
                    let _ = b.extensibility(h, ExtCall::OnDisconnection { mode: 1 });
                    b.release(h);
                }
            });
            return Ok(());
        }
        self.ext_call(i, ExtCall::OnDisconnection { mode: 1 });
        self.com_release(i);
        Ok(())
    }

    pub(crate) fn com_shutdown(&mut self) {
        let connected = self.connected_addins();
        for &i in &connected {
            self.ext_call(i, ExtCall::OnBeginShutdown);
        }
        for &i in &connected {
            self.ext_call(i, ExtCall::OnDisconnection { mode: 0 });
            self.com_release(i);
        }
    }

    pub(crate) fn com_addins_member(
        &mut self,
        m: &'static Member,
        a: &Args,
    ) -> Result<Variant, OmError> {
        match (m.name, m.kind) {
            ("Count", InvokeKind::Get) => Ok(Variant::I4(self.com_addins.entries.len() as i32)),
            ("Item" | "_Default", InvokeKind::Get | InvokeKind::Func) => {
                let i = self
                    .item_index(a.get(0), a.lcid, &self.com_addins.entries, |e| {
                        &e.reg.prog_id
                    })
                    .map_err(|e| {
                        if e == OmError::bad_index() {
                            OmError::hresult(DISP_E_BADINDEX)
                        } else {
                            e
                        }
                    })?;
                let key = self.com_addins.entries[i].reg.prog_id.to_lowercase();
                Ok(self.alloc(Object::ComAddIn(key)))
            }
            ("_NewEnum", InvokeKind::Get) => {
                let keys: Vec<String> = self
                    .com_addins
                    .entries
                    .iter()
                    .map(|e| e.reg.prog_id.to_lowercase())
                    .collect();
                let items = keys
                    .into_iter()
                    .map(|k| self.alloc(Object::ComAddIn(k)))
                    .collect();
                Ok(self.alloc_enumerator(items))
            }

            ("Update" | "SetAppModal", _) => Ok(Variant::Empty),
            ("Application" | "Parent", InvokeKind::Get) => {
                Ok(Variant::Dispatch(Some(self.handle())))
            }
            ("Creator", InvokeKind::Get) => Ok(Variant::I4(0x5843_454C_u32 as i32)),
            _ => Err(unsupported("COMAddIns", m)),
        }
    }

    pub(crate) fn com_addin_member(
        &mut self,
        key: &str,
        m: &'static Member,
        a: &Args,
    ) -> Result<Variant, OmError> {
        let i = self
            .com_addins
            .entries
            .iter()
            .position(|e| e.reg.prog_id.eq_ignore_ascii_case(key))
            .ok_or_else(OmError::object_required)?;
        match (m.name, m.kind) {
            ("ProgId", InvokeKind::Get) => Ok(Variant::Bstr(
                self.com_addins.entries[i].reg.prog_id.clone(),
            )),
            ("Description", InvokeKind::Get) => Ok(Variant::Bstr(
                self.com_addins.entries[i].reg.friendly_name.clone(),
            )),

            ("Description", InvokeKind::Put) => Err(OmError::hresult(E_FAIL)),
            ("Guid", InvokeKind::Get) => {
                Ok(Variant::Bstr(self.com_addins.entries[i].reg.guid.clone()))
            }
            ("Connect", InvokeKind::Get) => {
                Ok(Variant::Bool(self.com_addins.entries[i].server.is_some()))
            }
            ("Connect", InvokeKind::Put) => {
                if self.put_bool(a)? {
                    self.com_connect(i, 0)?;
                } else {
                    self.com_disconnect(i)?;
                }
                Ok(Variant::Empty)
            }
            ("Object", InvokeKind::Get) => Ok(self.com_addins.entries[i].object.clone()),
            ("Object", InvokeKind::Put | InvokeKind::PutRef) => {
                if self.com_addins.entries[i].server.is_none() {
                    return Err(OmError::hresult(E_FAIL));
                }
                if let Some(v) = &a.put {
                    self.com_addins.entries[i].object = v.clone();
                }
                Ok(Variant::Empty)
            }
            ("Application" | "Parent", InvokeKind::Get) => {
                Ok(Variant::Dispatch(Some(self.handle())))
            }
            ("Creator", InvokeKind::Get) => Ok(Variant::I4(0x5843_454C_u32 as i32)),
            _ => Err(unsupported("COMAddIn", m)),
        }
    }

    pub(crate) fn bridge_create(&mut self, progid: &str) -> Result<ObjectId, VbaError> {
        let h = self
            .with_bridge(|b| b.create(progid))
            .ok_or_else(|| VbaError::new(429))??;
        Ok(self.adopt_bridged(h))
    }

    fn adopt_bridged(&mut self, h: u64) -> ObjectId {
        self.com.push(crate::comobj::ComObj::Bridged(h));
        crate::comobj::com_id(self.com.len() - 1)
    }

    fn bridged_handle(&self, id: ObjectId) -> Option<u64> {
        match self.com.get(crate::comobj::index(id)) {
            Some(crate::comobj::ComObj::Bridged(h)) if crate::comobj::is_com(id) => Some(*h),
            _ => None,
        }
    }

    fn to_bridge(&self, v: &Variant) -> Result<Variant, VbaError> {
        match v {
            Variant::Dispatch(Some(id)) | Variant::Unknown(Some(id)) => {
                let h = self.bridged_handle(*id).ok_or_else(|| VbaError::new(13))?;
                Ok(Variant::Dispatch(Some(ObjectId(h))))
            }
            Variant::Array(a) => {
                let mut a = a.clone();
                for e in &mut a.data {
                    *e = self.to_bridge(e)?;
                }
                Ok(Variant::Array(a))
            }
            other => Ok(other.clone()),
        }
    }

    fn adopt_value(&mut self, v: Variant) -> Variant {
        match v {
            Variant::Dispatch(Some(id)) | Variant::Unknown(Some(id)) => {
                Variant::Dispatch(Some(self.adopt_bridged(id.0)))
            }
            Variant::Array(mut a) => {
                for e in &mut a.data {
                    *e = self.adopt_value(std::mem::replace(e, Variant::Empty));
                }
                Variant::Array(a)
            }
            other => other,
        }
    }

    pub(crate) fn bridged_invoke(
        &mut self,
        id: ObjectId,
        member: &str,
        kind: VbaKind,
        args: &[Variant],
    ) -> Result<Variant, VbaError> {
        let h = self.bridged_handle(id).ok_or_else(|| VbaError::new(424))?;
        let mapped: Vec<Variant> = args
            .iter()
            .map(|a| self.to_bridge(a))
            .collect::<Result<_, _>>()?;
        let r = self
            .with_bridge(|b| b.invoke(h, member, kind, &mapped))
            .ok_or_else(|| VbaError::new(429))?;
        Ok(self.adopt_value(r?))
    }
}
