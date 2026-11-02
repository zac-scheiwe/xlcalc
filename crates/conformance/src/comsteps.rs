use std::sync::{Arc, Mutex};

use serde_json::{json, Value as J};
use xlcalc::Workbook;
use xlcalc_om::typelib::Ty;
use xlcalc_om::{flags, Application, Call, Event, EventSink, SinkFailed};
use xlcalc_variant::Variant;

use super::omsteps::OmSession;
use super::{Outcome, Tally};

pub type SinkLog = Arc<Mutex<Vec<J>>>;

struct RunnerSink {
    label: String,
    iface: &'static str,
    cancel: bool,
    fail: bool,
    into_vba: Option<String>,
    log: SinkLog,
}

impl EventSink for RunnerSink {
    fn deliver(&self, app: &mut Application<'_>, event: &mut Event<'_>) -> Result<(), SinkFailed> {
        let m = event.member;
        let mut args = Vec::with_capacity(event.args.len());
        for (p, v) in m.params.iter().zip(event.args.iter_mut()) {
            if matches!(p.ty, Ty::ByRef(_)) {
                args.push(json!({ "vt": "BYREF|BOOL", "v": matches!(v, Variant::Bool(true)) }));
                if self.cancel {
                    *v = Variant::Bool(true);
                }
            } else {
                args.push(describe(app, v));
            }
        }

        let named: Vec<usize> = (0..args.len()).collect();
        self.log.lock().unwrap().push(json!({
            "sink": self.label,
            "iface": self.iface,
            "dispid": m.dispid,
            "name": m.name,
            "named": named,
            "args": args,
        }));
        if let Some(proc) = &self.into_vba {
            let line = format!("sink:{}:{}", self.label, m.name);
            let call = Call {
                flags: flags::METHOD,
                lcid: 0x0409,
            };
            let h = app.handle();
            if let Ok(Variant::Dispatch(Some(id))) = app.invoke(
                h,
                "Run",
                call,
                &[Variant::Bstr(proc.clone()), Variant::Bstr(line)],
                &[],
            ) {
                app.release(id);
            }
        }
        if self.fail {
            Err(SinkFailed)
        } else {
            Ok(())
        }
    }
}

fn describe(app: &mut Application<'_>, v: &Variant) -> J {
    match v {
        Variant::Bool(b) => json!({ "vt": "BOOL", "v": b }),
        Variant::Dispatch(None) => json!({ "vt": "DISPATCH", "v": null }),
        Variant::Dispatch(Some(id)) => {
            let ty = app.type_name(*id).unwrap_or("?");
            let mut rec = json!({ "vt": "DISPATCH", "type": ty });
            let text = |app: &mut Application<'_>, member: &str, args: &[Variant]| match app.invoke(
                *id,
                member,
                Call::GET,
                args,
                &[],
            ) {
                Ok(Variant::Bstr(s)) => Some(s),
                _ => None,
            };
            if ty == "Range" {
                let args = [
                    Variant::Bool(true),
                    Variant::Bool(true),
                    Variant::I4(1),
                    Variant::Bool(true),
                ];
                if let Some(a) = text(app, "Address", &args) {
                    let a = match a.strip_prefix('[') {
                        Some(rest) => format!("[wb]{}", rest.split_once(']').map_or(rest, |x| x.1)),
                        None => a,
                    };
                    rec["address"] = json!(a);
                }
            } else if ty == "_Worksheet" {
                if let Some(n) = text(app, "Name", &[]) {
                    rec["name"] = json!(n);
                }
            }
            rec
        }
        other => json!({ "vt": "?", "v": format!("{other:?}") }),
    }
}

pub fn run(
    session: &mut OmSession,
    wb: &mut Workbook,
    op: &str,
    step: &J,
    rec: &J,
    n: usize,
    tally: &mut Tally,
) {
    let r = if op == "com_advise" {
        session.with(wb, |s| advise(s, step, rec))
    } else {
        events(session, rec)
    };
    match r {
        Ok(()) => tally.add(Outcome::Exact, String::new),
        Err(why) => tally.add(Outcome::Wrong, || format!("step {n}: {op}: {why}")),
    }
}

fn advise(s: &mut OmSession, step: &J, rec: &J) -> Result<(), String> {
    let iface = step["iface"].as_str().unwrap_or("");
    let path = step["path"].as_str().unwrap_or("");
    let want_fail = rec.get("fail").is_some_and(|f| !f.is_null());
    let source = s
        .eval_handle(path)
        .and_then(|h| s.app.event_source(h))
        .filter(|src| src.interface().name == iface);
    let Some(source) = source else {
        return if want_fail {
            Ok(())
        } else {
            Err(format!("no connection point {iface} on '{path}'"))
        };
    };
    if want_fail {
        return Err(format!("advised {iface}, Excel failed: {}", rec["fail"]));
    }
    let sink = RunnerSink {
        label: step["sink"].as_str().unwrap_or(iface).to_string(),
        iface: source.interface().name,
        cancel: step["cancel"].as_bool().unwrap_or(false),
        fail: step["fail"].as_bool().unwrap_or(false),
        into_vba: step["into_vba"].as_str().map(str::to_string),
        log: s.sink_log.clone(),
    };
    s.app.advise(source, Arc::new(sink));
    Ok(())
}

fn events(s: &mut OmSession, rec: &J) -> Result<(), String> {
    let got = std::mem::take(&mut *s.sink_log.lock().unwrap());
    let want = rec["events"].as_array().cloned().unwrap_or_default();
    let line = |e: &J| format!("{}:{}", e["sink"].as_str().unwrap_or(""), e["name"]);
    for (k, (g, w)) in got.iter().zip(&want).enumerate() {
        if g != w {
            return Err(format!("event {k}: expected {w}, got {g}"));
        }
    }
    if got.len() != want.len() {
        let names = |v: &[J]| v.iter().map(line).collect::<Vec<_>>().join(" ");
        return Err(format!(
            "expected {} events ({}), got {} ({})",
            want.len(),
            names(&want),
            got.len(),
            names(&got)
        ));
    }
    Ok(())
}
