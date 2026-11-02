use super::objects::{holds_instance, instance_of, is_instance};
use super::{empty_array, Cell, ErrState, Exec, Kind, Settings, Slot, State};
use crate::compile::Code;
use crate::host::ObjectHost;
use crate::ir::{ProcCode, ProcId};
use crate::types::VType;
use crate::{ModuleKind, VbaError};
use std::sync::{Condvar, Mutex, PoisonError};
use xlcalc_variant::{ObjectId, Variant};

#[derive(Default)]
pub(crate) struct Shared {
    state: Mutex<Option<Box<State>>>,
    free: Condvar,
}

impl Shared {
    pub(crate) fn put(&self, st: Box<State>) {
        *self.state.lock().unwrap_or_else(PoisonError::into_inner) = Some(st);
        self.free.notify_one();
    }

    fn try_take(&self) -> Option<Box<State>> {
        self.state
            .lock()
            .unwrap_or_else(PoisonError::into_inner)
            .take()
    }

    pub(crate) fn take(&self) -> Box<State> {
        let mut g = self.state.lock().unwrap_or_else(PoisonError::into_inner);
        loop {
            if let Some(st) = g.take() {
                return st;
            }
            g = self.free.wait(g).unwrap_or_else(PoisonError::into_inner);
        }
    }
}

pub struct Project {
    code: Code,
    shared: Shared,
    errors: Vec<crate::CompileError>,
    settings: Settings,
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct ParamSig {
    pub name: String,
    pub ty: VType,

    pub array: bool,
    pub by_val: bool,
    pub optional: bool,
    pub param_array: bool,
}

impl ParamSig {
    pub fn takes_reference(&self) -> bool {
        self.param_array || (!self.array && (self.ty == VType::Variant || self.ty.is_object()))
    }
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct ProcSignature {
    pub name: String,
    pub module: String,
    pub kind: crate::ast::ProcKind,

    pub public: bool,
    pub params: Vec<ParamSig>,
    pub ret: VType,
    pub ret_array: bool,

    pub volatile: bool,
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub enum MacroLookup {
    Found(ProcSignature),
    NotFound,

    Ambiguous,
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct Sink {
    pub procedure: String,
    handler: ProcId,
    me: Option<ObjectId>,
}

impl Project {
    pub fn compile(sources: &[crate::SourceModule], lib: &dyn crate::Library) -> Project {
        let (code, mut errors) = crate::compile::compile(sources, lib);
        let st = Box::new(State {
            consts: vec![None; code.consts.len()],
            ..State::default()
        });
        let mut host = crate::NoHost;
        let mut ex = Exec::new(&code, Settings::default(), st, None, &mut host);
        let bad = ex.alloc_globals();
        let st = ex.into_state();
        for (name, e) in bad {
            errors.push(crate::CompileError {
                module: String::new(),
                line: 0,
                col: 0,
                message: format!("bounds of {name}: {}", e.description),
            });
        }
        let shared = Shared::default();
        shared.put(st);
        Project {
            code,
            shared,
            errors,
            settings: Settings::default(),
        }
    }

    pub fn compile_appended(
        old: &Project,
        sources: &[crate::SourceModule],
        lib: &dyn crate::Library,
    ) -> Project {
        let new = Project::compile(sources, lib);
        let (o, n) = (&old.code, &new.code);
        let prefix = o.modules.len() <= n.modules.len()
            && o.procs.len() <= n.procs.len()
            && o.consts.len() <= n.consts.len()
            && o.globals.len() <= n.globals.len()
            && o.modules
                .iter()
                .zip(&n.modules)
                .all(|(a, b)| a.name == b.name)
            && o.globals
                .iter()
                .zip(&n.globals)
                .all(|(a, b)| a.name == b.name);
        if !prefix {
            return new;
        }
        let Some(mut st) = old.shared.try_take() else {
            return new;
        };
        let mut fresh = new.shared.take();
        st.globals.extend(fresh.globals.drain(o.globals.len()..));
        st.consts.resize(n.consts.len(), None);
        old.shared.put(fresh);
        new.shared.put(st);
        Project {
            settings: old.settings,
            ..new
        }
    }

    pub fn set_settings(&mut self, settings: Settings) {
        self.settings = settings;
    }

    pub fn errors(&self) -> &[crate::CompileError] {
        &self.errors
    }

    fn find(&self, name: &str) -> Option<ProcId> {
        let (module, name) = match name.rsplit_once('.') {
            Some((m, n)) => (Some(m), n),
            None => (None, name),
        };
        let matches = |p: &ProcCode| {
            let m = &self.code.modules[p.module];
            m.kind == ModuleKind::Standard
                && p.name.eq_ignore_ascii_case(name)
                && module.is_none_or(|x| m.name.eq_ignore_ascii_case(x))
        };
        let procs = &self.code.procs;
        procs
            .iter()
            .position(|p| matches(p) && p.public)
            .or_else(|| procs.iter().position(matches))
    }

    fn find_macro(&self, name: &str) -> Result<ProcId, MacroLookup> {
        let name = name.trim();
        let (module, name) = match name.rsplit_once('.') {
            Some((m, n)) => (Some(m), n),
            None => (None, name),
        };
        let mut found = self.code.procs.iter().enumerate().filter(|(_, p)| {
            let m = &self.code.modules[p.module];
            !p.kind.is_property()
                && p.name.eq_ignore_ascii_case(name)
                && match module {
                    Some(x) => m.kind != ModuleKind::Class && m.name.eq_ignore_ascii_case(x),
                    None => m.kind == ModuleKind::Standard,
                }
        });
        match (found.next(), found.next()) {
            (Some((id, _)), None) => Ok(id),
            (Some(_), Some(_)) => Err(MacroLookup::Ambiguous),
            (None, _) => Err(MacroLookup::NotFound),
        }
    }

    pub fn lookup_macro(&self, name: &str) -> MacroLookup {
        match self.find_macro(name) {
            Ok(id) => MacroLookup::Found(self.sig(&self.code.procs[id])),
            Err(e) => e,
        }
    }

    fn exec<T>(
        &self,
        host: &mut dyn ObjectHost,
        f: impl FnOnce(&mut Exec) -> Result<T, VbaError>,
    ) -> Result<T, VbaError> {
        let mut st = self.shared.take();
        let outer = st.calls == 0;
        let saved = if outer {
            st.err = ErrState::default();
            None
        } else {
            Some(st.err.clone())
        };
        st.calls += 1;
        let mut ex = Exec::new(&self.code, self.settings, st, Some(&self.shared), host);
        let r = f(&mut ex);
        ex.st.calls -= 1;
        if let Some(e) = saved {
            ex.st.err = e;
        }
        let r = if ex.st.ending {
            Err(super::end_error())
        } else {
            r
        };
        if ex.st.ending && ex.st.calls == 0 {
            ex.reset();
        }
        r
    }

    fn finish(ex: &mut Exec, v: Variant) -> Result<Variant, VbaError> {
        let depth = ex.st.frames.len();
        ex.st.pins.retain(|&(d, _)| d < depth);
        ex.st.dirty = true;
        let inst = holds_instance(&v);
        ex.collect();
        if inst {
            return Err(VbaError {
                number: 98,
                description: String::new(),
            });
        }
        Ok(v)
    }

    pub fn call(
        &self,
        name: &str,
        args: &[Variant],
        host: &mut dyn ObjectHost,
    ) -> Result<Variant, VbaError> {
        let id = self.find(name).ok_or_else(|| VbaError::new(35))?;
        self.exec(host, |ex| {
            let cells = ex.bind_values(id, args)?;
            let v = ex.run(id, cells, None, None)?;
            Self::finish(ex, v)
        })
    }

    pub fn run_macro(
        &self,
        name: &str,
        args: &mut [Variant],
        host: &mut dyn ObjectHost,
    ) -> Result<Variant, VbaError> {
        let id = self.find_macro(name).map_err(|_| VbaError::new(35))?;
        self.exec(host, |ex| {
            let cells = ex.bind_values(id, args).map_err(Self::run_bind_error)?;
            let mut back = Vec::new();
            let v = ex.run(id, cells, None, Some(&mut back));
            for (a, b) in args.iter_mut().zip(back) {
                if !holds_instance(&b) {
                    *a = b;
                }
            }
            let v = Self::finish(ex, v?)?;

            let doc = ex.code.modules[ex.code.procs[id].module].kind == ModuleKind::Document;
            Ok(if doc { Variant::Empty } else { v })
        })
    }

    fn run_bind_error(e: VbaError) -> VbaError {
        let hr: u32 = match e.number {
            449 => 0x8002_000F,
            450 => 0x8002_000E,
            13 => 0x8002_0005,
            _ => return e,
        };
        VbaError {
            number: hr as i32,
            description: String::new(),
        }
    }

    pub fn sinks(&self, source: ObjectId, event: &str) -> Vec<Sink> {
        let st = self.shared.take();
        let mut host = crate::NoHost;
        let ex = Exec::new(&self.code, self.settings, st, Some(&self.shared), &mut host);
        ex.connections(None, Some(source), event)
            .into_iter()
            .map(|c| {
                let p = &self.code.procs[c.handler];
                Sink {
                    procedure: format!("{}.{}", self.code.modules[p.module].name, p.name),
                    handler: c.handler,
                    me: c.me,
                }
            })
            .collect()
    }

    pub fn call_sink(
        &self,
        sink: &Sink,
        args: &mut [Variant],
        host: &mut dyn ObjectHost,
    ) -> Result<(), VbaError> {
        self.exec(host, |ex| {
            if let Some((key, _)) = sink.me.and_then(instance_of) {
                if !ex.st.instances.contains_key(&key) {
                    return Ok(());
                }
            }
            let cells = ex.bind_values(sink.handler, args)?;
            let mut back = Vec::new();
            let v = ex.run(sink.handler, cells, sink.me, Some(&mut back));
            for (a, b) in args.iter_mut().zip(back) {
                *a = b;
            }
            Self::finish(ex, v?).map(drop)
        })
    }

    pub fn held_objects(&self) -> Vec<ObjectId> {
        let st = self.shared.take();
        let mut out = Vec::new();
        fn walk(v: &Variant, out: &mut Vec<ObjectId>) {
            match v {
                Variant::Dispatch(Some(id)) | Variant::Unknown(Some(id)) if !is_instance(*id) => {
                    out.push(*id);
                }
                Variant::Array(a) => a.data.iter().for_each(|x| walk(x, out)),
                _ => {}
            }
        }
        let fields = st.instances.values().flat_map(|x| x.fields.iter());
        for c in st.globals.iter().chain(&st.stack).chain(fields) {
            if let Cell::Own(s) = c {
                walk(&s.val, &mut out);
            }
        }
        self.shared.put(st);
        out.sort_by_key(|o| o.0);
        out.dedup();
        out
    }

    fn sig(&self, p: &ProcCode) -> ProcSignature {
        let m = &self.code.modules[p.module];
        ProcSignature {
            name: p.name.clone(),
            module: m.name.clone(),
            kind: p.kind,
            public: p.public && !m.options.private_module && m.kind == ModuleKind::Standard,
            params: p
                .params
                .iter()
                .map(|q| ParamSig {
                    name: q.name.clone(),
                    ty: q.ty.clone(),
                    array: q.array,
                    by_val: q.by_val,
                    optional: q.optional,
                    param_array: q.param_array,
                })
                .collect(),
            ret: p.ret.ty.clone(),
            ret_array: p.ret.array,
            volatile: p.volatile,
        }
    }

    pub fn signature(&self, name: &str) -> Option<ProcSignature> {
        self.find(name).map(|id| self.sig(&self.code.procs[id]))
    }

    pub fn functions(&self) -> Vec<ProcSignature> {
        self.code
            .procs
            .iter()
            .map(|p| self.sig(p))
            .filter(|s| s.public && s.kind == crate::ast::ProcKind::Function)
            .collect()
    }
}

impl Exec<'_> {
    pub(super) fn alloc_globals(&mut self) -> Vec<(String, VbaError)> {
        let code = self.code;
        let mut bad = Vec::new();
        let mut globals = Vec::new();
        for g in &code.globals {
            let slot = self.alloc_info(g).unwrap_or_else(|e| {
                bad.push((g.name.clone(), e));
                Slot {
                    ty: g.ty.clone(),
                    kind: Kind::Dynamic,
                    val: empty_array(&g.ty),
                    fixed: None,
                }
            });
            globals.push(Cell::Own(slot));
        }
        self.st.globals = globals;
        bad
    }

    fn reset(&mut self) {
        self.st.instances.clear();
        self.st.pins.clear();
        self.st.seq.clear();
        self.st.stack.clear();
        self.st.frames.clear();
        self.st.err = ErrState::default();
        self.st.dirty = false;
        self.st.ending = false;
        self.alloc_globals();
    }
}
