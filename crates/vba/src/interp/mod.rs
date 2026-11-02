pub(crate) mod arith;
mod objects;
pub(crate) mod ops;
mod project;
mod stubs;

use crate::compile::Code;
use crate::host::{DebugItem, InvokeKind, ObjectHost};
use crate::ir::*;
use crate::rt::{Builtin, RtContext};
use crate::types::{binary_type, BinOp, VType};
use crate::VbaError;
use arith::{spill, Conv, Val};
use objects::{instance_of, Instance, Item};
use std::cmp::Ordering;
use std::collections::{BTreeMap, HashMap};
use xlcalc_variant::{HResult, Locale, ObjectId, SafeArray, VarType, Variant};

pub use objects::is_instance;
pub use project::{MacroLookup, ParamSig, ProcSignature, Project, Sink};

pub(crate) const MISSING: Variant = Variant::Error(HResult::DISP_E_PARAMNOTFOUND.0);

const MAX_DEPTH: usize = 200;

pub(crate) fn end_error() -> VbaError {
    VbaError {
        number: 40040,
        description: String::new(),
    }
}

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
enum Kind {
    Scalar,
    Dynamic,
    Fixed,
}

#[derive(Clone, Debug)]
struct Slot {
    ty: VType,
    kind: Kind,
    val: Variant,

    fixed: Option<u32>,
}

impl Slot {
    fn value(ty: VType, val: Variant) -> Slot {
        Slot {
            ty,
            kind: Kind::Scalar,
            val,
            fixed: None,
        }
    }
}

#[derive(Clone, Debug)]
enum Cell {
    Own(Slot),

    Ref(Loc),
}

#[derive(Clone, Debug)]
enum Loc {
    Stack(usize),
    Global(usize),

    Field(u64, usize),

    Elem(Box<Loc>, usize),
}

#[derive(Clone, Debug, Default)]
struct ErrState {
    number: i32,
    description: String,
    source: String,
}

#[derive(Debug, Default)]
pub(crate) struct State {
    globals: Vec<Cell>,
    consts: Vec<Option<Variant>>,
    err: ErrState,
    stack: Vec<Cell>,

    frames: Vec<Option<ObjectId>>,

    pins: Vec<(usize, ObjectId)>,
    instances: BTreeMap<u64, Instance>,
    next_key: u64,

    seq: HashMap<usize, u64>,
    next_seq: u64,

    dirty: bool,
    collecting: bool,

    ending: bool,

    calls: usize,
}

fn elem_vt(ty: &VType) -> VarType {
    match ty {
        VType::Object(_) => VarType::Dispatch,
        _ => ty.var_type().unwrap_or(VarType::Variant),
    }
}

fn empty_array(elem: &VType) -> Variant {
    Variant::Array(Box::new(SafeArray {
        elem: elem_vt(elem),
        bounds: Vec::new(),
        data: Vec::new(),
    }))
}

fn new_array(elem: &VType, bounds: Vec<(i32, u32)>) -> Variant {
    let n = bounds.iter().map(|&(_, c)| c as usize).product();
    Variant::Array(Box::new(SafeArray {
        elem: elem_vt(elem),
        bounds,
        data: vec![elem.default_value(); n],
    }))
}

fn flat(bounds: &[(i32, u32)], idx: &[i64]) -> Result<usize, VbaError> {
    if idx.len() != bounds.len() {
        return Err(VbaError::new(9));
    }
    let mut k = 0usize;
    let mut stride = 1usize;
    for (&i, &(lo, n)) in idx.iter().zip(bounds) {
        let off = i - i64::from(lo);
        if off < 0 || off >= i64::from(n) {
            return Err(VbaError::new(9));
        }
        k += off as usize * stride;
        stride *= n as usize;
    }
    Ok(k)
}

fn fit(s: &str, n: usize, right: bool) -> String {
    let len = s.chars().count();
    if len >= n {
        return s.chars().take(n).collect();
    }
    let pad = " ".repeat(n - len);
    if right {
        pad + s
    } else {
        s.to_string() + &pad
    }
}

fn type_name_of(v: &Variant) -> &'static str {
    match v {
        Variant::Empty => "Empty",
        Variant::Null => "Null",
        Variant::I2(_) => "Integer",
        Variant::I4(_) => "Long",
        Variant::R4(_) => "Single",
        Variant::R8(_) => "Double",
        Variant::Cy(_) => "Currency",
        Variant::Date(_) => "Date",
        Variant::Bstr(_) | Variant::BstrNull => "String",
        Variant::Dispatch(_) => "Object",
        Variant::Unknown(_) => "Unknown",
        Variant::Error(_) => "Error",
        Variant::Bool(_) => "Boolean",
        Variant::Decimal(_) => "Decimal",
        Variant::Ui1(_) => "Byte",
        Variant::I8(_) => "LongLong",
        Variant::Array(_) => "Variant()",
    }
}

type HostArgs = (Vec<Variant>, Vec<(String, Variant)>);

enum Flow {
    Next,
    Jump(usize),
    Return,
}

struct Ctl {
    mode: OnErrorMode,

    in_handler: bool,

    err_pc: usize,

    armed: bool,

    gosub: Vec<usize>,
}

fn next_start(p: &ProcCode, pc: usize) -> usize {
    let mut j = pc + 1;
    while j < p.ops.len() && !p.stmt_start[j] {
        j += 1;
    }
    j
}

fn stmt_at(p: &ProcCode, pc: usize) -> usize {
    let mut j = pc;
    while j > 0 && !p.stmt_start[j] {
        j -= 1;
    }
    j
}

#[derive(Clone, Copy, Debug)]
pub struct Settings {
    pub locale: Locale,

    pub clock: fn() -> f64,
}

impl Default for Settings {
    fn default() -> Settings {
        let rt = RtContext::default();
        Settings {
            locale: rt.locale,
            clock: rt.clock,
        }
    }
}

pub(crate) struct Exec<'a> {
    code: &'a Code,
    settings: Settings,
    cv: Conv,
    st: Box<State>,

    spare: Option<Box<State>>,
    shared: Option<&'a project::Shared>,

    holding: bool,
    host: &'a mut dyn ObjectHost,
    base: usize,
    module: usize,
    me: Option<ObjectId>,

    raise_source: Option<String>,
}

impl Drop for Exec<'_> {
    fn drop(&mut self) {
        if let (true, Some(shared)) = (self.holding, self.shared) {
            shared.put(std::mem::take(&mut self.st));
        }
    }
}

impl<'a> Exec<'a> {
    pub(crate) fn new(
        code: &'a Code,
        settings: Settings,
        st: Box<State>,
        shared: Option<&'a project::Shared>,
        host: &'a mut dyn ObjectHost,
    ) -> Exec<'a> {
        Exec {
            code,
            settings,
            cv: Conv {
                locale: settings.locale,
            },
            st,
            spare: Some(Box::default()),
            shared,
            holding: shared.is_some(),
            host,
            base: 0,
            module: 0,
            me: None,
            raise_source: None,
        }
    }

    fn into_state(mut self) -> Box<State> {
        self.holding = false;
        std::mem::take(&mut self.st)
    }

    fn lend<T>(
        &mut self,
        f: impl FnOnce(&mut dyn ObjectHost) -> Result<T, VbaError>,
    ) -> Result<T, VbaError> {
        let Some(shared) = self.shared else {
            return f(&mut *self.host);
        };
        let real = std::mem::replace(&mut self.st, self.spare.take().unwrap_or_default());
        self.holding = false;
        shared.put(real);
        let r = f(&mut *self.host);
        let real = shared.take();
        self.spare = Some(std::mem::replace(&mut self.st, real));
        self.holding = true;
        if self.st.ending {
            return Err(end_error());
        }
        r
    }

    fn text(&self) -> bool {
        self.code
            .modules
            .get(self.module)
            .is_some_and(|m| m.options.compare_text)
    }

    fn var_loc(&self, r: VarRef) -> Loc {
        match r {
            VarRef::Local(i) => Loc::Stack(self.base + i),
            VarRef::Global(i) => Loc::Global(i),
            VarRef::Field(i) => {
                let key = self.me.and_then(instance_of).map_or(u64::MAX, |(k, _)| k);
                Loc::Field(key, i)
            }
        }
    }

    fn root_cell(&mut self, loc: &Loc) -> Result<&mut Cell, VbaError> {
        match loc {
            Loc::Stack(i) => Ok(&mut self.st.stack[*i]),
            Loc::Global(i) => Ok(&mut self.st.globals[*i]),
            Loc::Field(k, i) => self
                .st
                .instances
                .get_mut(k)
                .and_then(|x| x.fields.get_mut(*i))
                .ok_or_else(|| VbaError::new(91)),
            Loc::Elem(..) => Err(VbaError::new(51)),
        }
    }

    fn resolve_loc(&mut self, loc: &Loc) -> Result<Loc, VbaError> {
        if let Loc::Elem(..) = loc {
            return Ok(loc.clone());
        }
        if let Cell::Ref(next) = self.root_cell(loc)? {
            let next = next.clone();
            return self.resolve_loc(&next);
        }
        Ok(loc.clone())
    }

    fn at(&mut self, loc: &Loc) -> Result<(&mut Variant, VType, Kind, Option<u32>), VbaError> {
        if let Loc::Elem(base, k) = loc {
            let (v, _, _, _) = self.at(base)?;
            let Variant::Array(a) = v else {
                return Err(VbaError::new(13));
            };
            let ty = VType::of_var_type(a.elem);
            let v = a.data.get_mut(*k).ok_or_else(|| VbaError::new(9))?;
            return Ok((v, ty, Kind::Scalar, None));
        }
        if let Cell::Ref(next) = self.root_cell(loc)? {
            let next = next.clone();
            return self.at(&next);
        }
        match self.root_cell(loc)? {
            Cell::Own(s) => Ok((&mut s.val, s.ty.clone(), s.kind, s.fixed)),
            Cell::Ref(_) => Err(VbaError::new(51)),
        }
    }

    fn load(&mut self, loc: &Loc) -> Result<Variant, VbaError> {
        Ok(self.at(loc)?.0.clone())
    }

    fn hidden(&self, slot: usize) -> &Variant {
        match &self.st.stack[self.base + slot] {
            Cell::Own(s) => &s.val,
            Cell::Ref(_) => &Variant::Empty,
        }
    }

    fn set_hidden(&mut self, slot: usize, val: Variant) {
        self.st.stack[self.base + slot] = Cell::Own(Slot::value(VType::Variant, val));
    }

    fn alloc(
        &mut self,
        ty: &VType,
        decl: &ArrayDecl,
        fixed: Option<u32>,
    ) -> Result<Slot, VbaError> {
        let (kind, val) = match decl {
            ArrayDecl::Scalar => match fixed {
                Some(n) => (Kind::Scalar, Variant::Bstr("\0".repeat(n as usize))),
                None => (Kind::Scalar, ty.default_value()),
            },
            ArrayDecl::Dynamic => (Kind::Dynamic, empty_array(ty)),
            ArrayDecl::Fixed(bounds) => (Kind::Fixed, new_array(ty, self.bounds(bounds)?)),
        };
        Ok(Slot {
            ty: ty.clone(),
            kind,
            val,
            fixed,
        })
    }

    fn alloc_info(&mut self, g: &GlobalInfo) -> Result<Slot, VbaError> {
        self.alloc(&g.ty, &g.array, g.fixed)
    }

    fn long(&mut self, e: &CExpr) -> Result<i32, VbaError> {
        let v = self.eval(e)?;
        let v = self.deobj_val(v)?;
        i32::try_from(self.cv.to_i64(&v)?).map_err(|_| VbaError::new(6))
    }

    fn bounds(&mut self, bounds: &[(CExpr, CExpr)]) -> Result<Vec<(i32, u32)>, VbaError> {
        let mut out = Vec::new();
        for (lo, hi) in bounds {
            let l = self.long(lo)?;
            let h = self.long(hi)?;
            if h < l {
                return Err(VbaError::new(9));
            }
            out.push((l, (i64::from(h) - i64::from(l) + 1) as u32));
        }
        Ok(out)
    }

    fn set_err(&mut self, e: &VbaError) {
        self.st.err = ErrState {
            number: e.number,
            description: e.description.clone(),
            source: self
                .raise_source
                .take()
                .unwrap_or_else(|| "VBAProject".to_string()),
        };
    }

    fn clear_err(&mut self) {
        self.st.err = ErrState::default();
    }

    fn me_for(&self, id: ProcId) -> Option<ObjectId> {
        (self.code.procs[id].module == self.module)
            .then_some(self.me)
            .flatten()
    }

    fn run(
        &mut self,
        id: ProcId,
        args: Vec<Cell>,
        me: Option<ObjectId>,
        back: Option<&mut Vec<Variant>>,
    ) -> Result<Variant, VbaError> {
        if self.st.frames.len() >= MAX_DEPTH {
            return Err(VbaError::new(28));
        }
        let code = self.code;
        let p = &code.procs[id];
        let me = match code.modules[p.module].kind {
            crate::ModuleKind::Standard => None,
            crate::ModuleKind::Class => me,
            crate::ModuleKind::Document => match me {
                Some(m) => Some(m),
                None => self.document_object(p.module).ok(),
            },
        };
        let base = self.st.stack.len();
        let ret = Slot {
            ty: p.ret.ty.clone(),
            kind: if p.ret.array {
                Kind::Dynamic
            } else {
                Kind::Scalar
            },
            val: if p.ret.array {
                empty_array(&p.ret.ty)
            } else {
                p.ret.ty.default_value()
            },
            fixed: None,
        };
        self.st.stack.push(Cell::Own(ret));
        self.st.stack.extend(args);
        let saved = (self.base, self.module, self.me);
        self.base = base;
        self.module = p.module;
        self.me = me;
        self.st.frames.push(me);
        let r = self.enter(p);
        let ret = match &mut self.st.stack[base] {
            Cell::Own(s) => std::mem::replace(&mut s.val, Variant::Empty),
            Cell::Ref(_) => Variant::Empty,
        };
        if let Some(out) = back {
            for i in 0..p.params.len() {
                out.push(
                    self.load(&Loc::Stack(base + 1 + i))
                        .unwrap_or(Variant::Empty),
                );
            }
        }
        let depth = self.st.frames.len();
        self.pin_all(depth - 1, &ret);
        self.release_frame(base, depth);
        self.st.frames.pop();
        (self.base, self.module, self.me) = saved;
        if me.is_some_and(is_instance) {
            self.st.dirty = true;
        }
        if self.st.dirty {
            self.collect();
        }
        r.map(|()| ret)
    }

    fn enter(&mut self, p: &'a ProcCode) -> Result<(), VbaError> {
        for li in &p.locals[1 + p.params.len()..] {
            let s = self.alloc(&li.ty, &li.array, li.fixed)?;
            self.st.stack.push(Cell::Own(s));
        }
        let mut ctl = Ctl {
            mode: OnErrorMode::Disable,
            in_handler: false,
            err_pc: 0,
            armed: false,
            gosub: Vec::new(),
        };
        let mut pc = 0;
        while pc < p.ops.len() {
            if p.stmt_start[pc] {
                self.boundary();
            }
            match self.op(p, pc, &mut ctl) {
                Ok(Flow::Next) => pc += 1,
                Ok(Flow::Jump(t)) => pc = t,
                Ok(Flow::Return) => break,
                Err(e) => {
                    if ctl.in_handler || self.st.ending {
                        return Err(e);
                    }
                    match ctl.mode {
                        OnErrorMode::ResumeNext => {
                            self.set_err(&e);
                            pc = next_start(p, pc);
                        }
                        OnErrorMode::GoTo(t) => {
                            self.set_err(&e);
                            ctl.in_handler = true;
                            ctl.err_pc = pc;
                            pc = t;
                        }
                        OnErrorMode::Disable | OnErrorMode::Reset => return Err(e),
                    }
                }
            }
        }

        if ctl.armed {
            self.clear_err();
        }
        Ok(())
    }

    fn missing_param(&mut self, q: &ParamInfo) -> Result<Cell, VbaError> {
        let val = match &q.default {
            Some(d) => {
                let v = self.eval(d)?;
                if q.ty.var_type().is_some() {
                    self.cv.let_coerce(v, d.ty(), &q.ty)?
                } else {
                    spill(v, d.ty())?
                }
            }
            None if q.ty == VType::Variant => MISSING,
            None => q.ty.default_value(),
        };
        Ok(Cell::Own(Slot::value(q.ty.clone(), val)))
    }

    fn param_value(&mut self, q: &ParamInfo, v: Val, from: &VType) -> Result<Cell, VbaError> {
        if q.array {
            let v = spill(v, from)?;
            let Variant::Array(a) = &v else {
                return Err(VbaError::new(13));
            };
            if q.ty != VType::Variant && a.elem != elem_vt(&q.ty) {
                return Err(VbaError::new(13));
            }
            return Ok(Cell::Own(Slot {
                ty: q.ty.clone(),
                kind: Kind::Dynamic,
                val: v,
                fixed: None,
            }));
        }
        let val = match &q.ty {
            VType::Variant => spill(v, from)?,
            VType::Object(_) => {
                let v = spill(v, from)?;
                if !matches!(v, Variant::Dispatch(_) | Variant::Unknown(_)) {
                    return Err(VbaError::new(13));
                }
                self.coerce_obj(&q.ty, v)?
            }
            ty => {
                let v = self.deobj_val(v)?;
                self.cv.let_coerce(v, from, ty)?
            }
        };
        Ok(Cell::Own(Slot::value(q.ty.clone(), val)))
    }

    fn rest_array(items: Vec<Variant>) -> Cell {
        let n = items.len() as u32;
        Cell::Own(Slot::value(
            VType::Variant,
            Variant::Array(Box::new(SafeArray {
                elem: VarType::Variant,
                bounds: vec![(0, n)],
                data: items,
            })),
        ))
    }

    fn bind_values(&mut self, id: ProcId, args: &[Variant]) -> Result<Vec<Cell>, VbaError> {
        let code = self.code;
        let p = &code.procs[id];
        let fixed = p.params.iter().filter(|q| !q.param_array).count();
        let rest = p.params.last().is_some_and(|q| q.param_array);
        if args.len() > fixed && !rest {
            return Err(VbaError::new(450));
        }
        let mut cells = Vec::new();
        for (i, q) in p.params.iter().enumerate() {
            if q.param_array {
                cells.push(Self::rest_array(args.get(i..).unwrap_or(&[]).to_vec()));
                continue;
            }
            match args.get(i).filter(|a| **a != MISSING) {
                Some(v) => {
                    let from = VType::of_value(v);
                    cells.push(self.param_value(q, Val::V(v.clone()), &from)?);
                }
                None if q.optional => cells.push(self.missing_param(q)?),
                None => return Err(VbaError::new(449)),
            }
        }
        Ok(cells)
    }

    fn bind_args(&mut self, params: &[ParamInfo], args: &[CArg]) -> Result<Vec<Cell>, VbaError> {
        let mut cells = Vec::new();
        for (q, a) in params.iter().zip(args) {
            cells.push(match a {
                CArg::Value(e) => {
                    let v = self.eval(e)?;
                    self.param_value(q, v, e.ty())?
                }
                CArg::Ref(place) => match self.place_loc(place)? {
                    Some(loc) => Cell::Ref(loc),
                    None => {
                        let v = self.place_value(place)?;
                        self.param_value(q, Val::V(v), &VType::Variant)?
                    }
                },
                CArg::Missing => self.missing_param(q)?,
                CArg::Rest(items) => {
                    let mut vals = Vec::new();
                    for e in items {
                        vals.push(self.eval_spilled(e)?);
                    }
                    Self::rest_array(vals)
                }
            });
        }
        Ok(cells)
    }

    fn call_proc(&mut self, id: ProcId, args: &[CArg]) -> Result<Variant, VbaError> {
        let code = self.code;
        let cells = self.bind_args(&code.procs[id].params, args)?;
        let me = self.me_for(id);
        self.run(id, cells, me, None)
    }

    fn indices(&mut self, idx: &[CExpr]) -> Result<Vec<Variant>, VbaError> {
        let mut out = Vec::new();
        for e in idx {
            let v = self.eval_spilled(e)?;
            out.push(self.deobj(v)?);
        }
        Ok(out)
    }

    fn flat_at(&mut self, base: &Loc, idx: &[Variant]) -> Result<Option<usize>, VbaError> {
        let mut ns = Vec::new();
        for v in idx {
            ns.push(self.cv.to_i64(&Val::V(v.clone()))?);
        }
        match self.at(base)?.0 {
            Variant::Array(a) => flat(&a.bounds, &ns).map(Some),
            Variant::Dispatch(_) => Ok(None),
            _ => Err(VbaError::new(13)),
        }
    }

    fn place_loc(&mut self, place: &Place) -> Result<Option<Loc>, VbaError> {
        match place {
            Place::Var(r) => Ok(Some(self.var_loc(*r))),
            Place::Elem(r, idx) => {
                let base = self.var_loc(*r);
                let idx = self.indices(idx)?;
                Ok(self
                    .flat_at(&base, &idx)?
                    .map(|k| Loc::Elem(Box::new(base), k)))
            }
            _ => Ok(None),
        }
    }

    fn place_value(&mut self, place: &Place) -> Result<Variant, VbaError> {
        match place {
            Place::Elem(r, idx) => self.elem_value(*r, idx),
            Place::Var(r) => {
                let loc = self.var_loc(*r);
                self.load(&loc)
            }
            _ => Err(VbaError::new(13)),
        }
    }

    fn object_id(v: &Variant) -> Result<ObjectId, VbaError> {
        match v {
            Variant::Dispatch(Some(id)) | Variant::Unknown(Some(id)) => Ok(*id),
            Variant::Dispatch(None) | Variant::Unknown(None) => Err(VbaError::new(91)),
            _ => Err(VbaError::new(424)),
        }
    }

    fn let_loc(&mut self, loc: &Loc, val: Val, from: &VType) -> Result<(), VbaError> {
        let (cur, ty, kind, fixed) = self.at(loc)?;
        let cur_obj = match (&ty, kind) {
            (VType::Object(_), Kind::Scalar) => Some(cur.clone()),
            _ => None,
        };
        let v = if kind != Kind::Scalar {
            let v = spill(val, from)?;
            let Variant::Array(a) = &v else {
                return Err(VbaError::new(13));
            };
            if kind == Kind::Fixed || (ty != VType::Variant && a.elem != elem_vt(&ty)) {
                return Err(VbaError::new(13));
            }
            v
        } else if let Some(obj) = cur_obj {
            let id = Self::object_id(&obj)?;
            let v = self.operand(val, from)?;
            self.invoke(id, "", InvokeKind::Put, vec![v], Vec::new())?;
            return Ok(());
        } else if ty == VType::Variant {
            self.operand_raw(val, from)?
        } else {
            let v = self.deobj_val(val)?;
            match (self.cv.let_coerce(v, from, &ty)?, fixed) {
                (Variant::Bstr(s), Some(n)) => Variant::Bstr(fit(&s, n as usize, false)),
                (v, _) => v,
            }
        };
        let slot = self.at(loc)?.0;

        let dropped = objects::holds_instance(slot);
        *slot = v;
        if dropped {
            self.st.dirty = true;
        }
        Ok(())
    }

    fn set_loc(&mut self, loc: &Loc, v: Variant) -> Result<(), VbaError> {
        let real = self.resolve_loc(loc)?;
        let (_, ty, kind, _) = self.at(&real)?;
        if kind != Kind::Scalar || !(ty == VType::Variant || ty.is_object()) {
            return Err(VbaError::new(13));
        }
        let v = self.coerce_obj(&ty, v)?;
        *self.at(&real)?.0 = v.clone();
        self.connect(&real, &v);
        self.touch();
        Ok(())
    }

    fn touch(&mut self) {
        if !self.st.instances.is_empty() {
            self.st.dirty = true;
        }
    }

    fn store_let(&mut self, place: &Place, val: Val, from: &VType) -> Result<(), VbaError> {
        match place {
            Place::Var(r) => {
                let loc = self.var_loc(*r);
                self.let_loc(&loc, val, from)
            }
            Place::Elem(r, idx) => {
                let base = self.var_loc(*r);
                let mut idx = self.indices(idx)?;
                match self.flat_at(&base, &idx)? {
                    Some(k) => self.let_loc(&Loc::Elem(Box::new(base), k), val, from),
                    None => {
                        let obj = self.load(&base)?;
                        let id = Self::object_id(&obj)?;
                        idx.push(self.operand_raw(val, from)?);
                        self.invoke(id, "", InvokeKind::Put, idx, Vec::new())?;
                        Ok(())
                    }
                }
            }
            Place::Member(o, name, args) => {
                let ov = self.eval_spilled(o)?;
                let id = Self::object_id(&ov)?;
                let v = self.operand_raw(val, from)?;
                self.put_member(id, o.ty(), name, args, v, InvokeKind::Put)
            }
            Place::Index(o, args) => {
                let id = self.object(o)?;
                let (mut pos, named) = self.host_args(args)?;
                pos.push(self.operand_raw(val, from)?);
                self.invoke(id, "", InvokeKind::Put, pos, named)?;
                Ok(())
            }
            Place::Global(name, args) => {
                let (mut pos, named) = self.host_args(args)?;
                pos.push(self.operand_raw(val, from)?);
                self.global_call(name, InvokeKind::Put, pos, named)?;
                Ok(())
            }
            Place::Err(p) => {
                let v = self.deobj_val(val)?;
                match p {
                    ErrProp::Number => {
                        self.st.err.number =
                            i32::try_from(self.cv.to_i64(&v)?).map_err(|_| VbaError::new(6))?;
                    }
                    ErrProp::Description => self.st.err.description = self.cv.to_str(v, from)?,
                    ErrProp::Source => self.st.err.source = self.cv.to_str(v, from)?,
                }
                Ok(())
            }
        }
    }

    fn put_member(
        &mut self,
        id: ObjectId,
        static_ty: &VType,
        name: &str,
        args: &[NamedArg],
        v: Variant,
        kind: InvokeKind,
    ) -> Result<(), VbaError> {
        if is_instance(id) {
            let mut items = self.items(args)?;
            items.push(Item::value(v));
            let early = self.early(static_ty);
            self.call_member(id, name, kind, items, early)?;
        } else {
            let (mut pos, named) = self.host_args(args)?;
            pos.push(v);
            self.invoke(id, name, kind, pos, named)?;
        }
        Ok(())
    }

    fn store_set(&mut self, place: &Place, v: Variant) -> Result<(), VbaError> {
        if !matches!(v, Variant::Dispatch(_) | Variant::Unknown(_)) {
            return Err(VbaError::new(424));
        }
        match place {
            Place::Var(_) | Place::Elem(..) => match self.place_loc(place)? {
                Some(loc) => self.set_loc(&loc, v),
                None => {
                    let Place::Elem(r, idx) = place else {
                        return Err(VbaError::new(424));
                    };
                    let base = self.var_loc(*r);
                    let obj = self.load(&base)?;
                    let id = Self::object_id(&obj)?;
                    let mut idx = self.indices(idx)?;
                    idx.push(v);
                    self.invoke(id, "", InvokeKind::PutRef, idx, Vec::new())?;
                    Ok(())
                }
            },
            Place::Member(o, name, args) => {
                let ov = self.eval_spilled(o)?;
                let id = Self::object_id(&ov)?;
                self.put_member(id, o.ty(), name, args, v, InvokeKind::PutRef)
            }
            Place::Index(o, args) => {
                let id = self.object(o)?;
                let (mut pos, named) = self.host_args(args)?;
                pos.push(v);
                self.invoke(id, "", InvokeKind::PutRef, pos, named)?;
                Ok(())
            }
            Place::Global(name, args) => {
                let (mut pos, named) = self.host_args(args)?;
                pos.push(v);
                self.global_call(name, InvokeKind::PutRef, pos, named)?;
                Ok(())
            }
            Place::Err(_) => Err(VbaError::new(424)),
        }
    }

    fn invoke(
        &mut self,
        id: ObjectId,
        member: &str,
        kind: InvokeKind,
        pos: Vec<Variant>,
        named: Vec<(String, Variant)>,
    ) -> Result<Variant, VbaError> {
        if is_instance(id) {
            let mut items: Vec<Item> = pos.into_iter().map(Item::value).collect();
            items.extend(named.into_iter().map(|(n, v)| Item {
                name: Some(n),
                val: v,
                loc: None,
            }));
            return self.call_member(id, member, kind, items, false);
        }

        let pos: Vec<Variant> = pos.into_iter().map(Variant::canon).collect();
        self.lend(|h| {
            let named: Vec<(&str, Variant)> = named
                .iter()
                .map(|(n, v)| (n.as_str(), v.clone().canon()))
                .collect();
            h.invoke(id, member, kind, &pos, &named)
        })
    }

    fn global_call(
        &mut self,
        name: &str,
        kind: InvokeKind,
        pos: Vec<Variant>,
        named: Vec<(String, Variant)>,
    ) -> Result<Variant, VbaError> {
        if self.code.modules[self.module].kind == crate::ModuleKind::Document {
            if let Some(me) = self.me {
                match self.invoke(me, name, kind, pos.clone(), named.clone()) {
                    Err(e) if e.number == 438 && !self.st.ending => {}
                    r => return r,
                }
            }
        }
        let id = self.lend(|h| h.global())?;
        self.invoke(id, name, kind, pos, named)
    }

    fn document_object(&mut self, module: usize) -> Result<ObjectId, VbaError> {
        let code = self.code;
        let name = &code.modules[module].name;
        self.lend(|h| h.document(name))
    }

    fn deobj(&mut self, v: Variant) -> Result<Variant, VbaError> {
        match v {
            Variant::Dispatch(Some(id)) => {
                self.invoke(id, "", InvokeKind::Get, Vec::new(), Vec::new())
            }
            Variant::Dispatch(None) => Err(VbaError::new(91)),
            v => Ok(v),
        }
    }

    fn deobj_val(&mut self, v: Val) -> Result<Val, VbaError> {
        match v {
            Val::V(v @ Variant::Dispatch(_)) => self.deobj(v).map(Val::V),
            v => Ok(v),
        }
    }

    fn operand(&mut self, v: Val, ty: &VType) -> Result<Variant, VbaError> {
        Ok(self.operand_raw(v, ty)?.canon())
    }

    fn operand_raw(&mut self, v: Val, ty: &VType) -> Result<Variant, VbaError> {
        let v = spill(v, ty)?;
        self.deobj(v)
    }

    fn object(&mut self, e: &CExpr) -> Result<ObjectId, VbaError> {
        let v = self.eval_spilled(e)?;
        Self::object_id(&v)
    }

    fn host_args(&mut self, args: &[NamedArg]) -> Result<HostArgs, VbaError> {
        let mut pos = Vec::new();
        let mut named = Vec::new();
        for a in args {
            let v = self.eval_spilled(&a.value)?;
            match &a.name {
                Some(n) => named.push((n.clone(), v)),
                None => pos.push(v),
            }
        }
        Ok((pos, named))
    }

    fn eval_spilled(&mut self, e: &CExpr) -> Result<Variant, VbaError> {
        let v = self.eval(e)?;
        spill(v, e.ty())
    }

    fn cond(&mut self, e: &CExpr) -> Result<bool, VbaError> {
        let v = self.eval(e)?;
        let v = self.deobj_val(v)?;
        self.cv.truth(v)
    }

    fn constant(&mut self, id: usize) -> Result<Variant, VbaError> {
        if let Some(v) = &self.st.consts[id] {
            return Ok(v.clone());
        }
        let code = self.code;
        let c = &code.consts[id];
        let v = self.eval(&c.value)?;
        let v = if c.st.ty.var_type().is_some() {
            self.cv.let_coerce(v, c.value.ty(), &c.st.ty)?
        } else {
            spill(v, c.value.ty())?
        };
        self.st.consts[id] = Some(v.clone());
        Ok(v)
    }

    fn elem_value(&mut self, r: VarRef, idx: &[CExpr]) -> Result<Variant, VbaError> {
        let base = self.var_loc(r);
        let idx = self.indices(idx)?;
        match self.flat_at(&base, &idx)? {
            Some(k) => self.load(&Loc::Elem(Box::new(base), k)),
            None => {
                let obj = self.load(&base)?;
                let id = Self::object_id(&obj)?;
                self.invoke(id, "", InvokeKind::Get, idx, Vec::new())
            }
        }
    }

    fn eval(&mut self, e: &CExpr) -> Result<Val, VbaError> {
        Ok(match &e.kind {
            EK::Lit(v) => Val::V(v.clone()),
            EK::Const(id) => Val::V(self.constant(*id)?),
            EK::Var(r) => {
                let loc = self.var_loc(*r);
                Val::V(self.load(&loc)?)
            }
            EK::Elem(r, idx) => Val::V(self.elem_value(*r, idx)?),
            EK::Call(id, args) => Val::V(self.call_proc(*id, args)?),
            EK::Builtin(b, args) => Val::V(self.builtin(b.0, args, e.ty())?),
            EK::Intrinsic(i, args) => Val::V(self.intrinsic(*i, args)?),
            EK::Unary(op, a) => {
                let v = self.eval(a)?;
                if e.ty().is_dynamic() {
                    let v = self.operand(v, a.ty())?;
                    Val::V(ops::var_unary(*op, &v)?)
                } else {
                    let v = self.deobj_val(v)?;
                    self.cv.unary(*op, e.ty(), v)?
                }
            }
            EK::Binary(op, a, b) => self.binary(*op, e.ty(), a, b)?,
            EK::Member(o, name, args) => {
                let ov = self.eval_spilled(o)?;
                let id = Self::object_id(&ov)?;
                if is_instance(id) {
                    let items = self.items(args)?;
                    let early = self.early(o.ty());
                    Val::V(self.call_member(id, name, InvokeKind::Get, items, early)?)
                } else {
                    let (pos, named) = self.host_args(args)?;
                    Val::V(self.invoke(id, name, InvokeKind::Get, pos, named)?)
                }
            }
            EK::Index(o, args) => {
                let v = self.eval_spilled(o)?;
                Val::V(self.index_value(v, args)?)
            }
            EK::Global(name, args) => {
                let (pos, named) = self.host_args(args)?;
                Val::V(self.global_call(name, InvokeKind::Get, pos, named)?)
            }
            EK::Err(p) => Val::V(match p {
                ErrProp::Number => Variant::I4(self.st.err.number),
                ErrProp::Description => Variant::Bstr(self.st.err.description.clone()),
                ErrProp::Source => Variant::Bstr(self.st.err.source.clone()),
            }),
            EK::TypeOf(a, class) => {
                let v = self.eval_spilled(a)?;
                Val::V(Variant::Bool(match v {
                    Variant::Dispatch(Some(id)) => self.type_of(id, class)?,
                    Variant::Dispatch(None) => false,
                    _ => return Err(VbaError::new(424)),
                }))
            }
            EK::Missing => Val::V(MISSING),
            EK::New(class) => Val::V(Variant::Dispatch(Some(self.new_instance(*class)?))),
            EK::AutoNew(r, class) => {
                let loc = self.var_loc(*r);
                let v = self.load(&loc)?;
                if let Variant::Dispatch(None) = v {
                    let id = self.new_instance(*class)?;
                    let v = Variant::Dispatch(Some(id));
                    self.set_loc(&loc, v.clone())?;
                    Val::V(v)
                } else {
                    Val::V(v)
                }
            }
            EK::Me => match self.me {
                Some(id) => Val::V(Variant::Dispatch(Some(id))),
                None if self.code.modules[self.module].kind == crate::ModuleKind::Document => {
                    let id = self.document_object(self.module)?;
                    Val::V(Variant::Dispatch(Some(id)))
                }
                None => return Err(VbaError::new(91)),
            },
            EK::Document(j) => {
                let id = match self.me {
                    Some(me) if *j == self.module => me,
                    _ => self.document_object(*j)?,
                };
                Val::V(Variant::Dispatch(Some(id)))
            }
        })
    }

    fn index_value(&mut self, v: Variant, args: &[NamedArg]) -> Result<Variant, VbaError> {
        match v {
            Variant::Array(a) => {
                let mut ns = Vec::new();
                for x in args {
                    if x.name.is_some() {
                        return Err(VbaError::new(13));
                    }
                    let v = self.eval(&x.value)?;
                    let v = self.deobj_val(v)?;
                    ns.push(self.cv.to_i64(&v)?);
                }
                let k = flat(&a.bounds, &ns)?;
                Ok(a.data[k].clone())
            }
            v @ Variant::Dispatch(_) => {
                let id = Self::object_id(&v)?;
                let (pos, named) = self.host_args(args)?;
                self.invoke(id, "", InvokeKind::Get, pos, named)
            }
            _ => Err(VbaError::new(13)),
        }
    }

    fn binary(&mut self, op: BinOp, res: &VType, a: &CExpr, b: &CExpr) -> Result<Val, VbaError> {
        let x = self.eval(a)?;
        let y = self.eval(b)?;
        if op == BinOp::Is {
            let x = spill(x, a.ty())?;
            let y = spill(y, b.ty())?;
            return Ok(Val::V(Variant::Bool(
                objects::identity(&x)? == objects::identity(&y)?,
            )));
        }
        let text = self.text();
        if res.is_dynamic() || a.ty().is_dynamic() || b.ty().is_dynamic() {
            let x = self.operand(x, a.ty())?;
            let y = self.operand(y, b.ty())?;
            if op == BinOp::Like {
                if matches!(x, Variant::Null) || matches!(y, Variant::Null) {
                    return Ok(Val::V(Variant::Null));
                }
                let (xt, yt) = (VType::of_value(&x), VType::of_value(&y));
                let r = self.cv.binary(
                    op,
                    &VType::Boolean,
                    (Val::V(x), &xt),
                    (Val::V(y), &yt),
                    text,
                )?;
                return Ok(r);
            }
            return Ok(Val::V(ops::var_binary(op, &x, &y, text, self.cv.locale)?));
        }
        let x = self.deobj_val(x)?;
        let y = self.deobj_val(y)?;
        self.cv.binary(op, res, (x, a.ty()), (y, b.ty()), text)
    }

    fn builtin(&mut self, b: &Builtin, args: &[CExpr], ty: &VType) -> Result<Variant, VbaError> {
        let mut vals = Vec::new();
        for a in args {
            let v = self.eval(a)?;

            if matches!(b.name, "CStr" | "StrReverse") {
                vals.push(self.operand_raw(v, a.ty())?);
            } else {
                vals.push(self.operand(v, a.ty())?);
            }
        }
        let ctx = RtContext {
            compare_text: self.text(),
            locale: self.cv.locale,
            clock: self.settings.clock,
        };
        let r = (b.call)(&ctx, &vals)?;

        if ty.var_type().is_some() && !matches!(r, Variant::Null) {
            self.cv.coerce(r, ty)
        } else {
            Ok(r)
        }
    }

    fn type_name(&mut self, v: &Variant) -> Result<String, VbaError> {
        Ok(match v {
            Variant::Dispatch(Some(id)) => match instance_of(*id) {
                Some((key, _)) => self.instance_class_name(key)?,
                None => {
                    let id = *id;
                    self.lend(|h| Ok(h.type_name(id)))?
                }
            },
            Variant::Dispatch(None) => "Nothing".to_string(),
            Variant::Array(a) => {
                let elem = match a.elem {
                    VarType::Variant => "Variant",
                    VarType::Dispatch => "Object",
                    t => VType::of_var_type(t).name(),
                };
                format!("{elem}()")
            }
            v => type_name_of(v).to_string(),
        })
    }

    fn intrinsic(&mut self, i: Intrinsic, args: &[CExpr]) -> Result<Variant, VbaError> {
        if i == Intrinsic::Array {
            let mut items = Vec::new();
            for a in args {
                items.push(self.eval_spilled(a)?);
            }
            let base = self.code.modules[self.module].options.base;
            let n = items.len() as u32;
            return Ok(Variant::Array(Box::new(SafeArray {
                elem: VarType::Variant,
                bounds: vec![(base, n)],
                data: items,
            })));
        }
        if let Intrinsic::Ask(what) = i {
            let mut vals = Vec::with_capacity(args.len());
            for a in args {
                let v = self.eval(a)?;
                vals.push(self.operand(v, a.ty())?);
            }
            return self.lend(|h| h.interaction(what, &vals));
        }
        let v = self.eval_spilled(&args[0])?;
        Ok(match i {
            Intrinsic::IsMissing => Variant::Bool(v == MISSING),
            Intrinsic::IsArray => Variant::Bool(matches!(v, Variant::Array(_))),
            Intrinsic::IsObject => {
                Variant::Bool(matches!(v, Variant::Dispatch(_) | Variant::Unknown(_)))
            }
            Intrinsic::TypeName => Variant::Bstr(self.type_name(&v)?),
            Intrinsic::VarType => Variant::I4(i32::from(v.vt())),
            Intrinsic::StrPtr => match v {
                Variant::BstrNull => Variant::I8(0),
                Variant::Bstr(_) => Variant::I8(0x1000),
                _ => return Err(VbaError::new(13)),
            },
            Intrinsic::UBound | Intrinsic::LBound => {
                let Variant::Array(a) = &v else {
                    return Err(VbaError::new(13));
                };
                let dim = match args.get(1) {
                    Some(d) => self.long(d)?,
                    None => 1,
                };
                if dim < 1 || dim as usize > a.bounds.len() {
                    return Err(VbaError::new(9));
                }
                let (lo, n) = a.bounds[dim as usize - 1];
                Variant::I4(if i == Intrinsic::LBound {
                    lo
                } else {
                    (i64::from(lo) + i64::from(n) - 1) as i32
                })
            }
            Intrinsic::Array | Intrinsic::Ask(_) => unreachable!("handled above"),
        })
    }

    fn op(&mut self, p: &'a ProcCode, pc: usize, ctl: &mut Ctl) -> Result<Flow, VbaError> {
        match &p.ops[pc] {
            Op::Let(place, e) => {
                let v = self.eval(e)?;
                self.store_let(place, v, e.ty())?;
            }
            Op::Set(place, e) => {
                let v = self.eval_spilled(e)?;
                self.store_set(place, v)?;
            }
            Op::Bind(slot, e) => {
                let v = self.eval_spilled(e)?;
                if objects::holds_instance(self.hidden(*slot)) {
                    self.st.dirty = true;
                }
                self.set_hidden(*slot, v);
            }
            Op::Eval(e) => {
                self.eval(e)?;
            }
            Op::Jump(t) => return Ok(Flow::Jump(*t)),
            Op::JumpIfNot(c, t) => {
                if !self.cond(c)? {
                    return Ok(Flow::Jump(*t));
                }
            }
            Op::JumpIf(c, t) => {
                if self.cond(c)? {
                    return Ok(Flow::Jump(*t));
                }
            }
            Op::ForInit {
                var,
                from,
                to,
                step,
                hidden,
                exit,
            } => {
                let Place::Var(r) = var else {
                    return Err(VbaError::new(13));
                };
                let loc = self.var_loc(*r);
                let (_, cty, _, _) = self.at(&loc)?;
                let f = self.eval(from)?;
                let f = self.counter_value(f, from.ty(), &cty)?;
                let t = self.eval(to)?;
                let t = self.counter_value(t, to.ty(), &cty)?;
                let s = match step {
                    Some(e) => {
                        let v = self.eval(e)?;
                        self.counter_value(v, e.ty(), &cty)?
                    }
                    None => self.counter_value(Val::V(Variant::I2(1)), &VType::Integer, &cty)?,
                };
                *self.at(&loc)?.0 = f.clone();
                let done = for_done(&self.cv, &f, &t, &s)?;
                self.set_hidden(*hidden, t);
                self.set_hidden(*hidden + 1, s);
                if done {
                    return Ok(Flow::Jump(*exit));
                }
            }
            Op::ForNext {
                var,
                counter,
                hidden,
                body,
            } => {
                let Place::Var(r) = var else {
                    return Err(VbaError::new(13));
                };
                let loc = self.var_loc(*r);
                let cur = self.load(&loc)?;
                let t = self.hidden(*hidden).clone();
                let s = self.hidden(*hidden + 1).clone();
                let next = if counter.is_dynamic() {
                    ops::var_binary(BinOp::Add, &cur, &s, false, self.cv.locale)?
                } else {
                    let res = binary_type(BinOp::Add, counter, counter);
                    let r = self.cv.binary(
                        BinOp::Add,
                        &res,
                        (Val::V(cur), counter),
                        (Val::V(s.clone()), counter),
                        false,
                    )?;
                    self.cv.let_coerce(r, &res, counter)?
                };
                let done = for_done(&self.cv, &next, &t, &s)?;
                *self.at(&loc)?.0 = next;
                if !done {
                    return Ok(Flow::Jump(*body));
                }
            }
            Op::ForEachInit {
                var,
                collection,
                hidden,
                exit,
            } => {
                let c = self.eval_spilled(collection)?;
                let items = match c {
                    Variant::Array(a) => a.data,
                    v => {
                        let id = Self::object_id(&v)?;
                        if is_instance(id) {
                            let Some(items) = self.collection_items(id) else {
                                return Err(VbaError::new(438));
                            };
                            items
                        } else {
                            self.lend(|h| h.enumerate(id))?
                        }
                    }
                };
                let Some(first) = items.first().cloned() else {
                    return Ok(Flow::Jump(*exit));
                };
                self.set_hidden(
                    *hidden,
                    Variant::Array(Box::new(SafeArray {
                        elem: VarType::Variant,
                        bounds: vec![(0, items.len() as u32)],
                        data: items,
                    })),
                );
                self.set_hidden(*hidden + 1, Variant::I4(0));
                self.assign_item(var, first.canon())?;
            }
            Op::ForEachNext { var, hidden, body } => {
                let pos = match self.hidden(*hidden + 1) {
                    Variant::I4(n) => *n as usize + 1,
                    _ => usize::MAX,
                };
                let item = match self.hidden(*hidden) {
                    Variant::Array(a) => a.data.get(pos).cloned(),
                    _ => None,
                };
                if let Some(item) = item {
                    self.set_hidden(*hidden + 1, Variant::I4(pos as i32));
                    self.assign_item(var, item.canon())?;
                    return Ok(Flow::Jump(*body));
                }
            }
            Op::ReDim {
                var,
                preserve,
                bounds,
                elem,
            } => {
                self.redim(*var, *preserve, bounds, elem)?;
                self.touch();
            }
            Op::Erase(r) => {
                self.erase(*r)?;
                self.touch();
            }
            Op::OnError(m) => {
                if matches!(m, OnErrorMode::Reset) {
                    ctl.in_handler = false;
                } else {
                    ctl.mode = m.clone();
                }
                ctl.armed |= !matches!(m, OnErrorMode::Disable);
                self.clear_err();
            }
            Op::Resume(k) => {
                if !ctl.in_handler {
                    return Err(VbaError::new(20));
                }
                ctl.in_handler = false;
                self.clear_err();
                return Ok(Flow::Jump(match k {
                    ResumeKind::Again => stmt_at(p, ctl.err_pc),
                    ResumeKind::Next => next_start(p, ctl.err_pc),
                    ResumeKind::Label(t) => *t,
                }));
            }
            Op::ErrClear => self.clear_err(),
            Op::ErrRaise(args) => return Err(self.raise(args)?),
            Op::Return => return Ok(Flow::Return),
            Op::GoSub(t) => {
                ctl.gosub.push(pc + 1);
                return Ok(Flow::Jump(*t));
            }
            Op::GoSubReturn => {
                let back = ctl.gosub.pop().ok_or_else(|| VbaError::new(3))?;
                return Ok(Flow::Jump(back));
            }
            Op::OnGoTo {
                index,
                targets,
                gosub,
            } => {
                let i = self.long(index)?;
                if i < 0 {
                    return Err(VbaError::new(5));
                }

                if let Some(&t) = (i as usize).checked_sub(1).and_then(|k| targets.get(k)) {
                    if *gosub {
                        ctl.gosub.push(pc + 1);
                    }
                    return Ok(Flow::Jump(t));
                }
            }
            Op::LSet {
                place,
                value,
                right,
            } => {
                let cur = self.place_value(place)?;
                let n = match &cur {
                    Variant::Bstr(s) => s.chars().count(),
                    _ => 0,
                };
                let v = self.eval(value)?;
                let v = self.deobj_val(v)?;
                let s = self.cv.to_str(v, value.ty())?;
                let s = Variant::Bstr(fit(&s, n, *right));
                self.store_let(place, Val::V(s), &VType::String)?;
            }
            Op::DebugPrint(items) => {
                let mut out = Vec::new();
                for it in items {
                    out.push(match it {
                        PrintItem::Value(e) => {
                            let v = self.eval(e)?;
                            DebugItem::Value(self.operand(v, e.ty())?)
                        }
                        PrintItem::Zone => DebugItem::Zone,
                        PrintItem::Join => DebugItem::Join,
                    });
                }
                self.lend(|h| {
                    h.debug_print(&out);
                    Ok(())
                })?;
            }
            Op::DebugAssert(e) => {
                if !self.cond(e)? {
                    self.lend(|h| {
                        h.debug_assert();
                        Ok(())
                    })?;
                }
            }
            Op::RaiseEvent { event, args } => self.raise_event(*event, args)?,
            Op::End => {
                self.st.ending = true;
                return Err(end_error());
            }
            Op::Stop => {
                return Err(VbaError {
                    number: 438,
                    description: "not supported by xlcalc: Stop".to_string(),
                })
            }
        }
        Ok(Flow::Next)
    }

    fn counter_value(
        &mut self,
        v: Val,
        from: &VType,
        counter: &VType,
    ) -> Result<Variant, VbaError> {
        if counter.is_dynamic() {
            self.operand(v, from)
        } else {
            let v = self.deobj_val(v)?;
            self.cv.let_coerce(v, from, counter)
        }
    }

    fn assign_item(&mut self, var: &Place, item: Variant) -> Result<(), VbaError> {
        if matches!(item, Variant::Dispatch(_) | Variant::Unknown(_)) {
            self.store_set(var, item)
        } else {
            self.store_let(var, Val::V(item), &VType::Variant)
        }
    }

    fn redim(
        &mut self,
        var: VarRef,
        preserve: bool,
        bounds: &[(CExpr, CExpr)],
        elem: &VType,
    ) -> Result<(), VbaError> {
        let bs = self.bounds(bounds)?;
        let loc = self.var_loc(var);
        let (slot, ty, kind, _) = self.at(&loc)?;
        let elem = match kind {
            Kind::Fixed => return Err(VbaError::new(10)),
            Kind::Dynamic => ty,
            Kind::Scalar => match &*slot {
                Variant::Array(a) if preserve => VType::of_var_type(a.elem),
                _ => elem.clone(),
            },
        };
        let mut new = new_array(&elem, bs.clone());
        if preserve {
            if let (Variant::Array(old), Variant::Array(fresh)) = (&*slot, &mut new) {
                if !old.bounds.is_empty() {
                    let n = bs.len();
                    if old.bounds.len() != n || old.bounds[..n - 1] != bs[..n - 1] {
                        return Err(VbaError::new(9));
                    }
                    let keep = old.data.len().min(fresh.data.len());
                    fresh.data[..keep].clone_from_slice(&old.data[..keep]);
                }
            }
        }
        *slot = new;
        Ok(())
    }

    fn erase(&mut self, r: VarRef) -> Result<(), VbaError> {
        let loc = self.var_loc(r);
        let (slot, ty, kind, _) = self.at(&loc)?;
        match kind {
            Kind::Dynamic => *slot = empty_array(&ty),
            Kind::Fixed => {
                if let Variant::Array(a) = slot {
                    a.data.fill(ty.default_value());
                }
            }
            Kind::Scalar => match slot {
                Variant::Array(_) => *slot = Variant::Empty,
                _ => return Err(VbaError::new(13)),
            },
        }
        Ok(())
    }

    fn raise(&mut self, args: &[NamedArg]) -> Result<VbaError, VbaError> {
        const NAMES: [&str; 5] = ["number", "source", "description", "helpfile", "helpcontext"];
        let mut vals: [Option<Variant>; 5] = Default::default();
        for (i, a) in args.iter().enumerate() {
            let k = match &a.name {
                Some(n) => NAMES
                    .iter()
                    .position(|x| x.eq_ignore_ascii_case(n))
                    .ok_or_else(|| VbaError::new(448))?,
                None => i.min(4),
            };
            let v = self.eval_spilled(&a.value)?;
            if v != MISSING {
                vals[k] = Some(self.deobj(v)?);
            }
        }
        let Some(n) = vals[0].take() else {
            return Err(VbaError::new(449));
        };
        let n = i32::try_from(self.cv.to_i64(&Val::V(n))?).map_err(|_| VbaError::new(6))?;
        if n == 0 {
            return Err(VbaError::new(5));
        }
        let mut e = VbaError::new(n);
        if let Some(d) = vals[2].take() {
            e.description = self.cv.to_str(Val::V(d), &VType::Variant)?;
        }
        if let Some(s) = vals[1].take() {
            self.raise_source = Some(self.cv.to_str(Val::V(s), &VType::Variant)?);
        }
        Ok(e)
    }
}

fn for_done(cv: &Conv, cur: &Variant, to: &Variant, step: &Variant) -> Result<bool, VbaError> {
    let num = |v: &Variant| cv.to_f80(&Val::V(v.clone()));
    let zero = xlcalc::f80::F80::from_i64(0);
    let up = num(step)?.compare(zero) != Some(Ordering::Less);
    let o = num(cur)?
        .compare(num(to)?)
        .ok_or_else(|| VbaError::new(6))?;
    Ok(if up {
        o == Ordering::Greater
    } else {
        o == Ordering::Less
    })
}
