use std::sync::{Arc, Mutex};

use xlcalc::{Arg, CallContext, FunctionHost, ParamKind, RuntimeError, Signature, Value, XlError};
use xlcalc_variant::{HResult, ObjectId, Variant};

use crate::app::{Application, Call};
use crate::comaddin::{CallFailure, ObjectCallback, SharedBridge};
use crate::conv::error_scode;
use crate::vba::value_variant;

struct Server {
    prog_id: String,
    handle: u64,
}

#[derive(Default)]
struct Inner {
    bridge: Option<SharedBridge>,
    servers: Vec<Server>,
}

#[derive(Default)]
pub(crate) struct Automation {
    inner: Mutex<Inner>,
}

impl Automation {
    pub(crate) fn set_bridge(&self, b: SharedBridge) {
        if let Ok(mut g) = self.inner.lock() {
            g.bridge = Some(b);
        }
    }

    pub(crate) fn add_server(&self, prog_id: &str, handle: u64) {
        if let Ok(mut g) = self.inner.lock() {
            if !g.servers.iter().any(|s| s.prog_id == prog_id) {
                g.servers.push(Server {
                    prog_id: prog_id.to_string(),
                    handle,
                });
            }
        }
    }

    pub(crate) fn has_server(&self, prog_id: &str) -> bool {
        self.inner
            .lock()
            .is_ok_and(|g| g.servers.iter().any(|s| s.prog_id == prog_id))
    }

    pub(crate) fn is_empty(&self) -> bool {
        self.inner.lock().is_ok_and(|g| g.servers.is_empty())
    }

    fn find(&self, name: &str) -> Option<(SharedBridge, u64, String)> {
        let g = self.inner.lock().ok()?;
        let bridge = g.bridge.clone()?;
        for s in &g.servers {
            let prefix = format!("{}.", s.prog_id);
            let member = match name.get(..prefix.len()) {
                Some(p) if p.eq_ignore_ascii_case(&prefix) => &name[prefix.len()..],
                _ => name,
            };
            let ok = bridge
                .try_lock()
                .ok()
                .is_some_and(|mut b| b.has_member(s.handle, member));
            if ok {
                return Some((bridge.clone(), s.handle, member.to_string()));
            }
        }
        None
    }
}

fn argument(app: &mut Application<'_>, a: &Arg) -> Variant {
    match a {
        Arg::Value(Value::Error(e)) => Variant::I4(error_scode(*e)),
        Arg::Value(v) => value_variant(v),
        Arg::Missing => Variant::Error(HResult::DISP_E_PARAMNOTFOUND.0),
        Arg::Ref(areas) | Arg::XRef(areas) => {
            let (Some(first), Some(book)) = (areas.first(), app.running) else {
                return Variant::Error(error_scode(XlError::REF));
            };
            let rects: Vec<_> = areas
                .iter()
                .filter(|x| x.sheet == first.sheet)
                .map(|x| x.rect)
                .collect();
            app.new_multi(book, first.sheet, &rects, crate::app::Mode::Cells)
                .unwrap_or(Variant::Error(error_scode(XlError::REF)))
        }
    }
}

struct ViewCallback<'a, 'v> {
    app: &'a mut Application<'v>,

    caller: xlcalc::CellAddr,

    names: std::collections::HashMap<i32, String>,
    read_own: bool,
}

impl ViewCallback<'_, '_> {
    fn holds_caller(&self, id: ObjectId) -> bool {
        self.app
            .range_areas(Some(&Variant::Dispatch(Some(id))))
            .is_ok_and(|(_, sheet, rects)| {
                sheet == self.caller.sheet
                    && rects.iter().any(|r| {
                        (r.r0..=r.r1).contains(&self.caller.row)
                            && (r.c0..=r.c1).contains(&self.caller.col)
                    })
            })
    }
}

impl ObjectCallback for ViewCallback<'_, '_> {
    fn ids(&mut self, id: ObjectId, name: &str) -> Result<i32, i32> {
        match self.app.get_ids_of_names(id, name, &[]) {
            Ok(ids) => {
                let d = ids.first().copied().ok_or(0)?;
                self.names.insert(d, name.to_ascii_lowercase());
                Ok(d)
            }
            Err(e) => Err(e.hresult.0),
        }
    }

    fn type_name(&mut self, id: ObjectId) -> Option<String> {
        self.app.type_name(id).map(str::to_owned)
    }

    fn invoke(
        &mut self,
        id: ObjectId,
        dispid: i32,
        flags: u16,
        lcid: u32,
        args: &[Variant],
    ) -> Result<Variant, i32> {
        if flags & 12 != 0 {
            return Err(0);
        }
        let value_read = dispid == 0
            || self
                .names
                .get(&dispid)
                .is_some_and(|n| matches!(n.as_str(), "value" | "value2" | "text" | "_default"));
        if value_read && self.holds_caller(id) {
            self.read_own = true;
        }
        self.app
            .invoke_dispid(id, dispid, Call { flags, lcid }, args, &[])
            .map_err(|e| e.hresult.0)
    }
}

pub(crate) struct AutomationHost(pub(crate) Arc<Automation>);

impl FunctionHost for AutomationHost {
    fn macro_callable(&self, _name: &str) -> bool {
        false
    }

    fn signature(&self, name: &str) -> Option<Signature> {
        self.0.find(name)?;
        Some(Signature {
            params: Vec::new(),
            rest: ParamKind::Reference,
            volatile: false,
        })
    }

    fn call(
        &self,
        name: &str,
        args: &[Arg],
        cx: &mut CallContext<'_>,
    ) -> Result<Value, RuntimeError> {
        let (bridge, handle, member) = self.0.find(name).ok_or(RuntimeError)?;
        let view = cx.view();
        let mut app = Application::view(view.workbook(), "Book", view.caller());
        let values: Vec<Variant> = args.iter().map(|a| argument(&mut app, a)).collect();
        let mut callback = ViewCallback {
            app: &mut app,
            caller: view.caller(),
            names: std::collections::HashMap::new(),
            read_own: false,
        };
        let r = bridge
            .try_lock()
            .ok()
            .map(|mut b| b.call_function(handle, &member, &values, &mut callback));
        if callback.read_own {
            cx.set_circular();
        }
        match r {
            Some(Ok(Variant::Array(a)))
                if a.data
                    .contains(&Variant::Error(HResult::DISP_E_PARAMNOTFOUND.0)) =>
            {
                Ok(Value::Error(XlError::VALUE))
            }

            Some(Ok(v @ Variant::Dispatch(Some(_)))) => match app.range_areas(Some(&v)) {
                Ok((book, sheet, rects)) if Some(book) == app.running => {
                    cx.set_result_ref(
                        rects
                            .into_iter()
                            .map(|rect| xlcalc::Area { sheet, rect })
                            .collect(),
                    );
                    Ok(Value::Blank)
                }
                _ => Ok(app.result_value(v, 0)),
            },
            Some(Ok(Variant::Error(sc))) if sc == HResult::DISP_E_PARAMNOTFOUND.0 => {
                Ok(Value::Blank)
            }
            Some(Ok(v)) => Ok(app.result_value(v, 0)),
            Some(Err(CallFailure::Overflow)) => Ok(Value::Error(XlError::NUM)),
            Some(Err(CallFailure::Value)) | None => Ok(Value::Error(XlError::VALUE)),
        }
    }
}

impl Application<'_> {
    pub(crate) fn discover_automation(&mut self) {
        let servers = self
            .with_bridge(|b| b.automation_servers())
            .unwrap_or_default();
        for s in servers {
            let entry = crate::addins::automation_entry(s);
            self.add_listed_addin(entry);
        }
    }

    pub(crate) fn start_automation(&mut self, prog_id: &str) -> Result<(), crate::error::OmError> {
        if self.com_addins.automation.has_server(prog_id) {
            return Ok(());
        }
        let handle = self
            .with_bridge(|b| b.create(prog_id))
            .and_then(Result::ok)
            .ok_or_else(crate::error::OmError::app_defined)?;
        self.com_addins.automation.add_server(prog_id, handle);
        Ok(())
    }
}
