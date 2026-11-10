#![cfg(windows)]

use std::collections::HashMap;
use std::sync::{Arc, Mutex};

use xlcalc::{RtdSource, Value};
use xlcalc_winbridge::rtd::RtdServer;

use crate::app::Application;
use crate::vba::variant_value;

#[derive(Default)]
struct State {
    servers: HashMap<(String, String), RtdServer>,

    values: HashMap<(String, String, i32), Value>,

    ids: HashMap<(String, String, Vec<String>), i32>,
}

#[derive(Default, Clone)]
pub(crate) struct RtdHost {
    state: Arc<Mutex<State>>,
}

impl RtdSource for RtdHost {
    fn topic(&self, prog_id: &str, server: &str, topics: &[String]) -> Option<Value> {
        let mut st = self.state.lock().ok()?;
        let key = (prog_id.to_lowercase(), server.to_string());
        let id_key = (key.0.clone(), key.1.clone(), topics.to_vec());
        if let Some(id) = st.ids.get(&id_key) {
            return st.values.get(&(key.0, key.1, *id)).cloned();
        }
        if !st.servers.contains_key(&key) {
            let s = RtdServer::start(prog_id).ok()?;
            st.servers.insert(key.clone(), s);
        }
        let (id, first) = st.servers.get_mut(&key)?.connect(topics).ok()?;
        let value = variant_value(&first).unwrap_or(Value::Error(xlcalc::XlError::NA));
        st.ids.insert(id_key, id);
        st.values.insert((key.0, key.1, id), value.clone());
        Some(value)
    }
}

impl RtdHost {
    fn refresh(&self) -> bool {
        let Ok(mut st) = self.state.lock() else {
            return false;
        };
        let st = &mut *st;
        let mut changed = false;
        for ((prog, server), s) in &mut st.servers {
            for (id, v) in s.refresh().unwrap_or_default() {
                let value = variant_value(&v).unwrap_or(Value::Error(xlcalc::XlError::NA));
                st.values.insert((prog.clone(), server.clone(), id), value);
                changed = true;
            }
        }
        changed
    }

    fn terminate(&self) {
        if let Ok(mut st) = self.state.lock() {
            for s in st.servers.values_mut() {
                s.terminate();
            }
            st.servers.clear();
            st.values.clear();
            st.ids.clear();
        }
    }
}

impl Application<'_> {
    pub fn enable_rtd(&mut self) {
        let host = self.rtd.get_or_insert_with(RtdHost::default).clone();
        for b in &mut self.books {
            if let crate::app::BookWb::Own(wb) = &mut b.wb {
                wb.set_rtd_source(Some(Arc::new(host.clone())));
            }
        }
    }

    pub fn refresh_rtd(&mut self) -> bool {
        let Some(host) = &self.rtd else {
            return false;
        };
        let changed = host.refresh();
        if changed {
            for b in &mut self.books {
                if let crate::app::BookWb::Own(wb) = &mut b.wb {
                    wb.recalc();
                }
            }
        }
        changed
    }

    pub fn stop_rtd(&mut self) {
        if let Some(host) = self.rtd.take() {
            host.terminate();
            for b in &mut self.books {
                if let crate::app::BookWb::Own(wb) = &mut b.wb {
                    wb.set_rtd_source(None);
                }
            }
        }
    }
}
