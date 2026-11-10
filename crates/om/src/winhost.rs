use windows::core::{IUnknown, Interface};
use windows::Win32::System::Com::IDispatch;
use xlcalc_variant::Variant;
use xlcalc_vba::{InvokeKind, VbaError};
use xlcalc_winbridge as wb;

use crate::comaddin::{AutomationServer, ComBridge, ExtCall, RegisteredAddin};

#[derive(Default)]
pub struct WindowsBridge {
    objects: Vec<Option<wb::Server>>,

    application: Option<IDispatch>,
}

unsafe impl Send for WindowsBridge {}

impl WindowsBridge {
    pub fn new() -> WindowsBridge {
        wb::init_com();
        WindowsBridge::default()
    }

    pub fn set_application(&mut self, application: Option<IDispatch>) {
        self.application = application;
    }

    fn server(&self, h: u64) -> Result<&IDispatch, VbaError> {
        self.objects
            .get(h as usize)
            .and_then(Option::as_ref)
            .map(|s| &s.0)
            .ok_or_else(|| VbaError::new(424))
    }
}

fn error(e: wb::BridgeError) -> VbaError {
    VbaError {
        number: e.number,
        description: e.description,
    }
}

impl ComBridge for WindowsBridge {
    fn registered_addins(&mut self) -> Vec<crate::comaddin::RegisteredAddin> {
        wb::registry::registered()
            .into_iter()
            .map(|r| RegisteredAddin {
                prog_id: r.prog_id,
                friendly_name: r.friendly_name,
                description: r.description,
                guid: r.guid,
                load_behavior: r.load_behavior,
            })
            .collect()
    }

    fn create(&mut self, prog_id: &str) -> Result<u64, VbaError> {
        let s = wb::create(prog_id).map_err(error)?;
        self.objects.push(Some(s));
        Ok(self.objects.len() as u64 - 1)
    }

    fn invoke(
        &mut self,
        handle: u64,
        member: &str,
        kind: InvokeKind,
        args: &[Variant],
    ) -> Result<Variant, VbaError> {
        let target = self.server(handle)?.clone();
        let access = match kind {
            InvokeKind::Get => wb::Access::Get,
            InvokeKind::Put => wb::Access::Put,
            InvokeKind::PutRef => wb::Access::PutRef,
        };
        let known = |id: xlcalc_variant::ObjectId| -> Option<IUnknown> {
            self.objects
                .get(id.0 as usize)
                .and_then(Option::as_ref)
                .map(|s| s.0.clone().into())
        };
        let next = self.objects.len() as u64;
        let out = wb::invoke(&target, member, access, args, &known, next).map_err(error)?;
        for unk in out.made {
            if let Ok(d) = unk.cast::<IDispatch>() {
                self.objects.push(Some(wb::Server(d)));
            } else {
                self.objects.push(None);
            }
        }
        Ok(out.value)
    }

    fn extensibility(&mut self, handle: u64, call: ExtCall) -> Result<bool, VbaError> {
        let target = self.server(handle)?.clone();
        let (method, mode, app) = match call {
            ExtCall::OnConnection { mode, .. } => (
                wb::Extensibility::OnConnection,
                mode,
                self.application.clone(),
            ),
            ExtCall::OnDisconnection { mode } => (wb::Extensibility::OnDisconnection, mode, None),
            ExtCall::OnAddInsUpdate => (wb::Extensibility::OnAddInsUpdate, 0, None),
            ExtCall::OnStartupComplete => (wb::Extensibility::OnStartupComplete, 0, None),
            ExtCall::OnBeginShutdown => (wb::Extensibility::OnBeginShutdown, 0, None),
        };
        wb::call_extensibility(&target, method, app.as_ref(), mode, None).map_err(error)
    }

    fn automation_servers(&mut self) -> Vec<AutomationServer> {
        wb::registry::automation_entries()
            .into_iter()
            .map(|e| AutomationServer {
                prog_id: e.prog_id,
                file: e.file,
                title: e.class,
            })
            .collect()
    }

    fn has_member(&mut self, handle: u64, name: &str) -> bool {
        self.server(handle).is_ok_and(|s| wb::has_member(s, name))
    }

    fn call_function(
        &mut self,
        handle: u64,
        name: &str,
        args: &[Variant],
        cb: &mut dyn crate::comaddin::ObjectCallback,
    ) -> Result<Variant, crate::comaddin::CallFailure> {
        use crate::comaddin::CallFailure;
        struct Adapter<'a>(&'a mut dyn crate::comaddin::ObjectCallback);
        impl wb::proxy::Callback for Adapter<'_> {
            fn ids(&mut self, id: xlcalc_variant::ObjectId, name: &str) -> Result<i32, i32> {
                self.0.ids(id, name)
            }
            fn invoke(
                &mut self,
                id: xlcalc_variant::ObjectId,
                dispid: i32,
                flags: u16,
                lcid: u32,
                args: &[Variant],
            ) -> Result<Variant, i32> {
                self.0.invoke(id, dispid, flags, lcid, args)
            }
            fn type_name(&mut self, id: xlcalc_variant::ObjectId) -> Option<String> {
                self.0.type_name(id)
            }
        }
        let target = self.server(handle).map_err(|_| CallFailure::Value)?.clone();
        wb::call_function(&target, name, args, &mut Adapter(cb)).map_err(|e| {
            if e.number == xlcalc_variant::HResult::DISP_E_OVERFLOW.0 {
                CallFailure::Overflow
            } else {
                CallFailure::Value
            }
        })
    }

    fn release(&mut self, handle: u64) {
        if let Some(slot) = self.objects.get_mut(handle as usize) {
            *slot = None;
        }
    }
}
