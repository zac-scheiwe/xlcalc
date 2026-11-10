use crate::ast::{self, *};
use crate::host::{Interaction, Library};
use crate::interp::ops::find_builtin;
use crate::ir::*;
use crate::parser::parse_module;
use crate::types::{binary_type, unary_type, BinOp, VType};
use crate::CompileError;
use std::collections::HashMap;
use xlcalc_variant::Variant;

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum ModuleKind {
    Standard,
    Class,

    Document,
}

#[derive(Clone, Debug)]
pub struct SourceModule {
    pub name: String,
    pub kind: ModuleKind,
    pub source: String,
}

#[derive(Clone, Debug)]
pub struct ModInfo {
    pub name: String,
    pub kind: ModuleKind,
    pub options: Options,

    pub class: Option<ClassInfo>,
}

#[derive(Clone, Debug, Default)]
pub struct Member {
    pub field: Option<usize>,
    pub get: Option<ProcId>,
    pub let_: Option<ProcId>,
    pub set: Option<ProcId>,
}

#[derive(Clone, Debug)]
pub struct EventInfo {
    pub name: String,
    pub params: Vec<ParamInfo>,
}

#[derive(Clone, Debug, Default)]
pub struct ClassInfo {
    pub fields: Vec<GlobalInfo>,

    pub members: HashMap<String, Member>,

    pub default: Option<String>,
    pub init: Option<ProcId>,
    pub term: Option<ProcId>,

    pub implements: Vec<usize>,
    pub events: Vec<EventInfo>,
}

#[derive(Clone, Debug, Default)]
pub struct Code {
    pub modules: Vec<ModInfo>,
    pub procs: Vec<ProcCode>,
    pub consts: Vec<ConstInfo>,
    pub globals: Vec<GlobalInfo>,

    pub class_by_name: HashMap<String, usize>,

    pub proc_names: HashMap<(usize, String), ProcId>,
}

impl Code {
    pub fn class(&self, module: usize) -> Option<&ClassInfo> {
        self.modules.get(module).and_then(|m| m.class.as_ref())
    }
}

type R<T> = Result<T, CompileError>;

pub fn compile(sources: &[SourceModule], lib: &dyn Library) -> (Code, Vec<CompileError>) {
    let mut errors = Vec::new();
    let mut parsed: Vec<(usize, Module)> = Vec::new();
    for (i, s) in sources.iter().enumerate() {
        match parse_module(&s.name, &s.source) {
            Ok(m) => parsed.push((i, m)),
            Err(e) => errors.push(e),
        }
    }
    let shared: Vec<_> = parsed
        .iter()
        .flat_map(|(_, m)| m.types.iter().filter(|t| t.public).cloned())
        .collect();
    let global = crate::udt::global(&parsed.iter().map(|(_, m)| m).collect::<Vec<_>>(), &shared);
    parsed = parsed
        .into_iter()
        .filter_map(|(i, mut m)| {
            match crate::udt::lower(&sources[i].name, &mut m, &shared, &global) {
                Ok(()) => Some((i, m)),
                Err(e) => {
                    errors.push(e);
                    None
                }
            }
        })
        .collect();
    loop {
        let mods: Vec<(&SourceModule, &Module)> =
            parsed.iter().map(|(i, m)| (&sources[*i], m)).collect();
        match Compiler::run(&mods, lib) {
            Ok(code) => return (code, errors),
            Err((bad, e)) => {
                errors.push(e);
                parsed.remove(bad);
            }
        }
    }
}

struct Sig {
    module: usize,

    ast: usize,
    name: String,
    kind: ProcKind,
    public: bool,
    friend: bool,
    params: Vec<ParamInfo>,
    ret: SType,
}

#[derive(Default)]
struct ModScope {
    vars: HashMap<String, (usize, bool)>,
    consts: HashMap<String, (usize, bool)>,

    procs: HashMap<String, (ProcId, bool)>,

    props: HashMap<String, [Option<ProcId>; 3]>,
    events: HashMap<String, usize>,

    enums: HashMap<String, (HashMap<String, usize>, bool)>,
}

struct Compiler<'a> {
    lib: &'a dyn Library,
    mods: &'a [(&'a SourceModule, &'a Module)],
    scopes: Vec<ModScope>,
    sigs: Vec<Sig>,
    code: Code,
}

#[derive(Clone, Debug)]
enum Sym {
    Var(VarRef, SType),
    Const(usize, SType),
    Proc(ProcId),
    Module(usize),
    Enum(usize, String),
    Err,
    Intrinsic(Intrinsic),
    Builtin(BuiltinRef),
    Value(Variant),
    LibGlobal(String),
}

fn builtin_class(name: &str) -> Option<usize> {
    name.eq_ignore_ascii_case("collection")
        .then_some(crate::ir::COLLECTION_CLASS)
}

fn lower(s: &str) -> String {
    s.to_ascii_lowercase()
}

fn vba_constant(name: &str) -> Option<Variant> {
    let s = |t: &str| Some(Variant::Bstr(t.to_string()));
    let i = |v: i32| Some(Variant::I4(v));
    match lower(name).as_str() {
        "vbcrlf" | "vbnewline" => s("\r\n"),
        "vbcr" => s("\r"),
        "vblf" => s("\n"),
        "vbtab" => s("\t"),
        "vbnullstring" => Some(Variant::BstrNull),
        "vbnullchar" => s("\0"),

        "vbokonly" | "vbdefaultbutton1" | "vbapplicationmodal" => i(0),
        "vbokcancel" | "vbok" => i(1),
        "vbabortretryignore" | "vbcancel" => i(2),
        "vbyesnocancel" | "vbabort" => i(3),
        "vbyesno" | "vbretry" => i(4),
        "vbretrycancel" | "vbignore" => i(5),
        "vbyes" => i(6),
        "vbno" => i(7),
        "vbcritical" => i(16),
        "vbquestion" => i(32),
        "vbexclamation" => i(48),
        "vbinformation" => i(64),
        "vbdefaultbutton2" => i(256),
        "vbdefaultbutton3" => i(512),
        "vbsystemmodal" => i(4096),
        "vbbinarycompare" => i(0),
        "vbtextcompare" => i(1),
        "vbdatabasecompare" => i(2),
        "vbformfeed" => s(""),
        "vbverticaltab" => s(""),
        "vbback" => s(""),

        "vbusesystemdayofweek" | "vbusesystem" | "vbgeneraldate" => i(0),
        "vbsunday" | "vbfirstjan1" | "vblongdate" | "vbuppercase" => i(1),
        "vbmonday" | "vbfirstfourdays" | "vbshortdate" | "vblowercase" => i(2),
        "vbtuesday" | "vbfirstfullweek" | "vblongtime" | "vbpropercase" => i(3),
        "vbwednesday" | "vbshorttime" | "vbwide" => i(4),
        "vbthursday" => i(5),
        "vbfriday" => i(6),
        "vbsaturday" => i(7),
        "vbnarrow" => i(8),
        "vbkatakana" => i(16),
        "vbhiragana" => i(32),
        "vbunicode" => i(64),
        "vbfromunicode" => i(128),
        "vbobjecterror" => i(-2147221504),

        "vbnormal" => i(0),
        "vbreadonly" => i(1),
        "vbhidden" => i(2),
        "vbsystem" => i(4),
        "vbvolume" => i(8),
        "vbdirectory" => i(16),
        "vbarchive" => i(32),
        "vbtrue" => i(-1),
        "vbfalse" => i(0),
        "vbmethod" => i(1),
        "vbget" => i(2),
        "vblet" => i(4),
        "vbset" => i(8),
        "vbusedefault" => i(-2),
        "vbempty" => i(0),
        "vbnull" => i(1),
        "vbinteger" => i(2),
        "vblong" => i(3),
        "vbsingle" => i(4),
        "vbdouble" => i(5),
        "vbcurrency" => i(6),
        "vbdate" => i(7),
        "vbstring" => i(8),
        "vbobject" => i(9),
        "vberror" => i(10),
        "vbboolean" => i(11),
        "vbvariant" => i(12),
        "vbdataobject" => i(13),
        "vbdecimal" => i(14),
        "vbuserdefinedtype" => i(36),
        "vbbyte" => i(17),
        "vblonglong" => i(20),
        "vbarray" => i(8192),
        _ => None,
    }
}

fn intrinsic(name: &str) -> Option<Intrinsic> {
    Some(match lower(name).as_str() {
        "array" => Intrinsic::Array,
        "ismissing" => Intrinsic::IsMissing,
        "isarray" => Intrinsic::IsArray,
        "isobject" => Intrinsic::IsObject,
        "typename" => Intrinsic::TypeName,
        "strptr" => Intrinsic::StrPtr,
        "vartype" => Intrinsic::VarType,
        "ubound" => Intrinsic::UBound,
        "lbound" => Intrinsic::LBound,
        "msgbox" => Intrinsic::Ask(Interaction::MsgBox),
        "inputbox" => Intrinsic::Ask(Interaction::InputBox),
        "beep" => Intrinsic::Ask(Interaction::Beep),
        "createobject" => Intrinsic::CreateObject,
        "declarecall" => Intrinsic::DeclareCall,
        "callbyname" => Intrinsic::CallByName,
        "erl" => Intrinsic::Erl,
        "getobject" => Intrinsic::GetObject,
        _ => return None,
    })
}

fn intrinsic_type(i: Intrinsic) -> VType {
    match i {
        Intrinsic::Array => VType::Variant,
        Intrinsic::IsMissing | Intrinsic::IsArray | Intrinsic::IsObject => VType::Boolean,
        Intrinsic::TypeName => VType::String,
        Intrinsic::VarType => VType::Long,
        Intrinsic::StrPtr => VType::LongLong,
        Intrinsic::UBound | Intrinsic::LBound => VType::Long,
        Intrinsic::Ask(Interaction::MsgBox) => VType::Integer,
        Intrinsic::Ask(Interaction::InputBox) => VType::String,
        Intrinsic::Ask(Interaction::Beep) => VType::Variant,
        Intrinsic::CreateObject | Intrinsic::GetObject => VType::Object(None),
        Intrinsic::DeclareCall | Intrinsic::CallByName => VType::Variant,
        Intrinsic::Erl => VType::Long,
    }
}

fn lit(v: Variant) -> CExpr {
    CExpr {
        st: SType::scalar(VType::of_value(&v)),
        kind: EK::Lit(v),
    }
}

impl<'a> Compiler<'a> {
    fn run(
        mods: &'a [(&'a SourceModule, &'a Module)],
        lib: &'a dyn Library,
    ) -> Result<Code, (usize, CompileError)> {
        let mut c = Compiler {
            lib,
            mods,
            scopes: Vec::new(),
            sigs: Vec::new(),
            code: Code::default(),
        };
        for (i, (src, m)) in mods.iter().enumerate() {
            c.code.modules.push(ModInfo {
                name: src.name.clone(),
                kind: src.kind,
                options: m.options.clone(),
                class: (src.kind == ModuleKind::Class).then(ClassInfo::default),
            });
            if src.kind == ModuleKind::Class {
                c.code.class_by_name.insert(lower(&src.name), i);
            }
            c.scopes.push(ModScope::default());
        }
        for mi in 0..mods.len() {
            c.declare(mi).map_err(|e| (mi, e))?;
        }
        for mi in 0..mods.len() {
            c.module_values(mi).map_err(|e| (mi, e))?;
        }
        for id in 0..c.sigs.len() {
            let mi = c.sigs[id].module;
            let proc = c.find_proc_ast(id);
            let code = Ctx::new(&mut c, mi, Some(id))
                .procedure(proc)
                .map_err(|e| (mi, e))?;
            c.code.procs.push(code);
        }
        for (id, p) in c.code.procs.iter().enumerate() {
            if matches!(
                p.kind,
                ProcKind::Function | ProcKind::Sub | ProcKind::PropertyGet
            ) {
                c.code.proc_names.insert((p.module, lower(&p.name)), id);
            }
        }
        Ok(c.code)
    }

    fn err(&self, mi: usize, pos: Pos, message: impl Into<String>) -> CompileError {
        CompileError {
            module: self.mods[mi].0.name.clone(),
            line: pos.line,
            col: pos.col,
            message: message.into(),
        }
    }

    fn find_proc_ast(&self, id: ProcId) -> &'a Proc {
        let sig = &self.sigs[id];
        &self.mods[sig.module].1.procs[sig.ast]
    }

    fn is_class(&self, mi: usize) -> bool {
        self.code.modules[mi].kind == ModuleKind::Class
    }

    fn class_named(&self, name: &str) -> Option<usize> {
        self.code.class_by_name.get(&lower(name)).copied()
    }

    fn var_info(&self, mi: usize, id: usize) -> &GlobalInfo {
        match &self.code.modules[mi].class {
            Some(c) => &c.fields[id],
            None => &self.code.globals[id],
        }
    }

    fn var_info_mut(&mut self, mi: usize, id: usize) -> &mut GlobalInfo {
        match &mut self.code.modules[mi].class {
            Some(c) => &mut c.fields[id],
            None => &mut self.code.globals[id],
        }
    }

    fn var_sym(&self, mi: usize, id: usize) -> Sym {
        let g = self.var_info(mi, id);
        let st = SType {
            ty: g.ty.clone(),
            array: !matches!(g.array, ArrayDecl::Scalar),
        };
        let r = if self.is_class(mi) {
            VarRef::Field(id)
        } else {
            VarRef::Global(id)
        };
        Sym::Var(r, st)
    }

    fn resolve_type(&self, tn: &Option<TypeName>) -> VType {
        match tn {
            None => VType::Variant,
            Some(TypeName::Builtin(t)) => t.clone(),
            Some(TypeName::Class(c)) => {
                let l = lower(c);
                if self.scopes.iter().any(|s| s.enums.contains_key(&l)) {
                    VType::Long
                } else {
                    VType::Object(Some(l))
                }
            }
        }
    }

    fn declare(&mut self, mi: usize) -> R<()> {
        let m = self.mods[mi].1;
        let check = |c: &Compiler, name: &str, pos: Pos| -> R<()> {
            let s = &c.scopes[mi];
            let l = lower(name);
            if s.vars.contains_key(&l)
                || s.consts.contains_key(&l)
                || s.procs.contains_key(&l)
                || s.enums.contains_key(&l)
                || s.events.contains_key(&l)
            {
                return Err(c.err(mi, pos, format!("duplicate declaration of {name}")));
            }
            Ok(())
        };
        for e in &m.enums {
            check(self, &e.name, e.pos)?;
            let mut members = HashMap::new();
            for (name, _, _) in &e.members {
                members.insert(lower(name), usize::MAX);
            }
            self.scopes[mi]
                .enums
                .insert(lower(&e.name), (members, e.public));
        }
        for c in &m.consts {
            check(self, &c.name, c.pos)?;
            self.scopes[mi]
                .consts
                .insert(lower(&c.name), (usize::MAX, c.public));
        }
        let kind = self.mods[mi].0.kind;
        for v in &m.vars {
            check(self, &v.var.name, v.var.pos)?;
            if v.with_events && kind == ModuleKind::Standard {
                return Err(self.err(mi, v.var.pos, "WithEvents is only valid in object modules"));
            }
            let info = GlobalInfo {
                name: v.var.name.clone(),
                ty: VType::Variant,
                array: ArrayDecl::Scalar,
                fixed: None,
                auto_new: None,
                public: v.public,
                with_events: v.with_events.then_some(mi),
            };
            let id = match &mut self.code.modules[mi].class {
                Some(c) => {
                    c.fields.push(info);
                    c.fields.len() - 1
                }
                None => {
                    self.code.globals.push(info);
                    self.code.globals.len() - 1
                }
            };
            self.scopes[mi]
                .vars
                .insert(lower(&v.var.name), (id, v.public));
        }
        for (i, e) in m.events.iter().enumerate() {
            if kind == ModuleKind::Standard {
                return Err(self.err(mi, e.pos, "Event is only valid in object modules"));
            }
            check(self, &e.name, e.pos)?;
            self.scopes[mi].events.insert(lower(&e.name), i);
        }
        for (ast, p) in m.procs.iter().enumerate() {
            let l = lower(&p.name);
            let slot = match p.kind {
                ProcKind::PropertyGet => Some(0),
                ProcKind::PropertyLet => Some(1),
                ProcKind::PropertySet => Some(2),
                _ => None,
            };
            let id = self.sigs.len();
            match slot {
                Some(k) if self.scopes[mi].props.contains_key(&l) => {
                    if self.scopes[mi].props[&l][k].is_some() {
                        return Err(self.err(
                            mi,
                            p.pos,
                            format!("duplicate declaration of {}", p.name),
                        ));
                    }
                }
                _ => check(self, &p.name, p.pos)?,
            }
            if p.friend && kind == ModuleKind::Standard {
                return Err(self.err(mi, p.pos, "Friend is only valid in object modules"));
            }
            if p.is_static && kind == ModuleKind::Class {
                return Err(self.err(mi, p.pos, "unsupported: Static procedures in class modules"));
            }

            let public = p.public && kind == ModuleKind::Standard;
            if let Some(k) = slot {
                let entry = self.scopes[mi].props.entry(l.clone()).or_default();
                entry[k] = Some(id);
                let scope = &mut self.scopes[mi];
                if k == 0 || !scope.procs.contains_key(&l) {
                    scope.procs.insert(l, (id, public));
                }
            } else {
                self.scopes[mi].procs.insert(l, (id, public));
            }
            self.sigs.push(Sig {
                module: mi,
                ast,
                name: p.name.clone(),
                kind: p.kind,
                public: p.public,
                friend: p.friend,
                params: Vec::new(),
                ret: SType::scalar(VType::Variant),
            });
        }
        Ok(())
    }

    fn param_infos(&mut self, mi: usize, params: &[Param], pos: Pos) -> R<Vec<ParamInfo>> {
        let mut out = Vec::new();
        for prm in params {
            let ty = if prm.param_array {
                VType::Variant
            } else {
                self.resolve_type(&prm.var.ty)
            };
            let default = match &prm.default {
                Some(d) => {
                    if !prm.optional {
                        return Err(self.err(mi, prm.var.pos, "a default needs Optional"));
                    }
                    let mut cx = Ctx::new(self, mi, None);
                    let v = cx.expr(d)?;
                    cx.require_const(&v, prm.var.pos)?;
                    Some(v)
                }
                None => None,
            };
            out.push(ParamInfo {
                name: prm.var.name.clone(),
                ty,
                array: prm.var.array.is_some(),
                by_val: prm.by_val,
                optional: prm.optional,
                param_array: prm.param_array,
                default,
            });
        }
        if let Some(i) = out.iter().position(|q| q.optional || q.param_array) {
            if out[i..].iter().any(|q| !q.optional && !q.param_array) {
                return Err(self.err(mi, pos, "parameters after an Optional one must be Optional"));
            }
        }
        if out.iter().rev().skip(1).any(|q| q.param_array) {
            return Err(self.err(mi, pos, "ParamArray must be the last parameter"));
        }
        Ok(out)
    }

    fn fixed_len(&self, mi: usize, v: &VarDecl) -> R<Option<u32>> {
        let Some(e) = &v.fixed_len else {
            return Ok(None);
        };
        let n = match &e.kind {
            ExprKind::Lit(Variant::I2(n)) => i64::from(*n),
            ExprKind::Lit(Variant::I4(n)) => i64::from(*n),
            _ => {
                return Err(self.err(
                    mi,
                    v.pos,
                    "unsupported: a fixed string length that is not a number",
                ))
            }
        };
        if !(1..=65526).contains(&n) {
            return Err(self.err(mi, v.pos, "fixed-length string out of range"));
        }
        Ok(Some(n as u32))
    }

    fn auto_new(&self, mi: usize, v: &VarDecl) -> R<Option<usize>> {
        if !v.new {
            return Ok(None);
        }
        if v.array.is_some() {
            return Err(self.err(mi, v.pos, "unsupported: arrays declared As New"));
        }
        let Some(TypeName::Class(c)) = &v.ty else {
            return Err(self.err(mi, v.pos, "As New needs a class"));
        };
        match self.class_named(c).or_else(|| builtin_class(c)) {
            Some(j) => Ok(Some(j)),
            None => Err(self.err(mi, v.pos, format!("unsupported: New {c}"))),
        }
    }

    fn module_values(&mut self, mi: usize) -> R<()> {
        let m = self.mods[mi].1;
        for e in &m.enums {
            let mut prev: Option<usize> = None;
            for (name, pos, value) in &e.members {
                let value = match value {
                    Some(v) => {
                        let mut cx = Ctx::new(self, mi, None);
                        let v = cx.expr(v)?;
                        cx.require_const(&v, *pos)?;
                        v
                    }
                    None => match prev {
                        None => lit(Variant::I4(0)),
                        Some(p) => CExpr {
                            st: SType::scalar(VType::Long),
                            kind: EK::Binary(
                                BinOp::Add,
                                Box::new(CExpr {
                                    st: SType::scalar(VType::Long),
                                    kind: EK::Const(p),
                                }),
                                Box::new(lit(Variant::I4(1))),
                            ),
                        },
                    },
                };
                let id = self.code.consts.len();
                self.code.consts.push(ConstInfo {
                    name: name.clone(),
                    st: SType::scalar(VType::Long),
                    value,
                });
                if let Some((members, _)) = self.scopes[mi].enums.get_mut(&lower(&e.name)) {
                    members.insert(lower(name), id);
                }
                prev = Some(id);
            }
        }
        for c in &m.consts {
            let id = self.add_const(mi, c)?;
            self.scopes[mi]
                .consts
                .insert(lower(&c.name), (id, c.public));
        }
        for v in &m.vars {
            let (id, _) = self.scopes[mi].vars[&lower(&v.var.name)];
            let ty = self.resolve_type(&v.var.ty);
            let array = {
                let mut cx = Ctx::new(self, mi, None);
                cx.array_decl(&v.var)?
            };
            let fixed = self.fixed_len(mi, &v.var)?;
            let auto_new = self.auto_new(mi, &v.var)?;
            if v.with_events && (!matches!(array, ArrayDecl::Scalar) || !ty.is_object()) {
                return Err(self.err(mi, v.var.pos, "WithEvents needs an object type"));
            }
            let g = self.var_info_mut(mi, id);
            g.ty = ty;
            g.array = array;
            g.fixed = fixed;
            g.auto_new = auto_new;
        }
        let ids: Vec<ProcId> = (0..self.sigs.len())
            .filter(|&i| self.sigs[i].module == mi)
            .collect();
        for &id in &ids {
            let p = self.find_proc_ast(id);
            let params = self.param_infos(mi, &p.params, p.pos)?;
            let ret = SType {
                ty: if p.kind.returns_value() {
                    self.resolve_type(&p.ret)
                } else {
                    VType::Variant
                },
                array: p.ret_array,
            };
            self.sigs[id].params = params;
            self.sigs[id].ret = ret;
        }
        if self.is_class(mi) {
            self.class_members(mi, &ids)?;
        } else if let Some((_, pos)) = m.implements.first() {
            return Err(self.err(mi, *pos, "Implements is only valid in class modules"));
        }
        Ok(())
    }

    fn class_members(&mut self, mi: usize, ids: &[ProcId]) -> R<()> {
        let m = self.mods[mi].1;
        let mut events = Vec::new();
        for e in &m.events {
            let params = self.param_infos(mi, &e.params, e.pos)?;
            events.push(EventInfo {
                name: e.name.clone(),
                params,
            });
        }
        let mut implements = Vec::new();
        for (name, pos) in &m.implements {
            match self.class_named(name) {
                Some(j) if j != mi => implements.push(j),
                _ => return Err(self.err(mi, *pos, format!("Implements needs a class: {name}"))),
            }
        }
        let mut members: HashMap<String, Member> = HashMap::new();
        let mut init = None;
        let mut term = None;
        for &id in ids {
            let sig = &self.sigs[id];
            let l = lower(&sig.name);
            if sig.kind == ProcKind::Sub && sig.params.is_empty() {
                match l.as_str() {
                    "class_initialize" => init = Some(id),
                    "class_terminate" => term = Some(id),
                    _ => {}
                }
            }
            let e = members.entry(l).or_default();
            match sig.kind {
                ProcKind::PropertyLet => e.let_ = Some(id),
                ProcKind::PropertySet => e.set = Some(id),
                _ => e.get = Some(id),
            }
        }
        for (l, &(i, _)) in &self.scopes[mi].vars {
            members.entry(l.clone()).or_default().field = Some(i);
        }
        if let Some(c) = &mut self.code.modules[mi].class {
            c.members = members;
            c.default = m.default_member.clone();
            c.init = init;
            c.term = term;
            c.implements = implements;
            c.events = events;
        }
        Ok(())
    }

    fn add_const(&mut self, mi: usize, c: &ConstDecl) -> R<usize> {
        let value = Ctx::new(self, mi, None).expr(&c.value)?;
        if !is_const(&value) {
            return Err(self.err(mi, c.pos, "constant expression required"));
        }
        let st = match &c.ty {
            Some(t) => SType::scalar(self.resolve_type(&Some(t.clone()))),
            None => value.st.clone(),
        };
        if st.ty.is_object() {
            return Err(self.err(mi, c.pos, "a constant cannot be an object"));
        }
        let id = self.code.consts.len();
        self.code.consts.push(ConstInfo {
            name: c.name.clone(),
            st,
            value,
        });
        Ok(id)
    }

    fn lookup_module(&self, mi: usize, name: &str) -> Option<Sym> {
        let l = lower(name);
        let own = &self.scopes[mi];
        let sym = |j: usize, s: &ModScope, public_only: bool| -> Option<Sym> {
            if let Some(&(id, public)) = s.vars.get(&l) {
                if !public_only || public {
                    return Some(self.var_sym(j, id));
                }
            }
            if let Some(&(id, public)) = s.consts.get(&l) {
                if (!public_only || public) && id != usize::MAX {
                    return Some(Sym::Const(id, self.code.consts[id].st.clone()));
                }
            }
            if let Some(&(id, public)) = s.procs.get(&l) {
                if !public_only || public {
                    return Some(Sym::Proc(id));
                }
            }
            for (members, public) in s.enums.values() {
                if public_only && !public {
                    continue;
                }
                if let Some(&id) = members.get(&l) {
                    if id != usize::MAX {
                        return Some(Sym::Const(id, SType::scalar(VType::Long)));
                    }
                }
            }
            None
        };
        if let Some(s) = sym(mi, own, false) {
            return Some(s);
        }
        for (j, s) in self.scopes.iter().enumerate() {
            if j != mi && self.code.modules[j].kind == ModuleKind::Standard {
                if let Some(found) = sym(j, s, true) {
                    return Some(found);
                }
            }
        }
        if let Some(j) = self.code.modules.iter().position(|m| lower(&m.name) == l) {
            return Some(Sym::Module(j));
        }
        for (j, s) in self.scopes.iter().enumerate() {
            if s.enums.contains_key(&l) {
                return Some(Sym::Enum(j, l));
            }
        }
        if l == "err" {
            return Some(Sym::Err);
        }
        if let Some(i) = intrinsic(name) {
            return Some(Sym::Intrinsic(i));
        }
        if let Some(b) = find_builtin(name) {
            return Some(Sym::Builtin(BuiltinRef(b)));
        }
        if let Some(v) = vba_constant(name) {
            return Some(Sym::Value(v));
        }
        if let Some(v) = self.lib.constant(name) {
            return Some(Sym::Value(v));
        }
        if self.lib.is_global(name) {
            return Some(Sym::LibGlobal(name.to_string()));
        }
        None
    }
}

fn is_const(e: &CExpr) -> bool {
    match &e.kind {
        EK::Lit(_) | EK::Const(_) => true,
        EK::Unary(_, a) => is_const(a),
        EK::Binary(_, a, b) => is_const(a) && is_const(b),
        _ => false,
    }
}

#[derive(Clone, Copy, PartialEq, Eq)]
enum LoopKind {
    For,
    Do,
}

enum Fixup {
    Label(String, Pos),

    Labels(Vec<String>, Pos),
    Exit,
}

struct Ctx<'c, 'a> {
    c: &'c mut Compiler<'a>,
    mi: usize,
    proc: Option<ProcId>,
    locals: Vec<LocalInfo>,
    local_map: HashMap<String, Sym>,
    ops: Vec<Op>,
    starts: Vec<bool>,
    labels: HashMap<String, usize>,
    fixups: Vec<(usize, Fixup)>,
    loops: Vec<(LoopKind, Vec<usize>)>,
    with: Vec<usize>,
    all_static: bool,
    volatile: bool,
}

impl<'c, 'a> Ctx<'c, 'a> {
    fn new(c: &'c mut Compiler<'a>, mi: usize, proc: Option<ProcId>) -> Ctx<'c, 'a> {
        Ctx {
            c,
            mi,
            proc,
            locals: Vec::new(),
            local_map: HashMap::new(),
            ops: Vec::new(),
            starts: Vec::new(),
            labels: HashMap::new(),
            fixups: Vec::new(),
            loops: Vec::new(),
            with: Vec::new(),
            all_static: false,
            volatile: false,
        }
    }

    fn err(&self, pos: Pos, message: impl Into<String>) -> CompileError {
        self.c.err(self.mi, pos, message)
    }

    fn options(&self) -> &Options {
        &self.c.code.modules[self.mi].options
    }

    fn require_const(&self, e: &CExpr, pos: Pos) -> R<()> {
        if is_const(e) {
            Ok(())
        } else {
            Err(self.err(pos, "constant expression required"))
        }
    }

    fn array_decl(&mut self, v: &VarDecl) -> R<ArrayDecl> {
        let Some(bounds) = &v.array else {
            return Ok(ArrayDecl::Scalar);
        };
        if bounds.is_empty() {
            return Ok(ArrayDecl::Dynamic);
        }
        if bounds.len() > 60 {
            return Err(self.err(v.pos, "too many dimensions"));
        }
        let mut out = Vec::new();
        for b in bounds {
            let (lo, hi) = self.bounds(b)?;
            self.require_const(&lo, v.pos)?;
            self.require_const(&hi, v.pos)?;
            out.push((lo, hi));
        }
        Ok(ArrayDecl::Fixed(out))
    }

    fn bounds(&mut self, b: &Bounds) -> R<(CExpr, CExpr)> {
        let lo = match &b.lower {
            Some(e) => self.expr(e)?,
            None => lit(Variant::I4(self.options().base)),
        };
        Ok((lo, self.expr(&b.upper)?))
    }

    fn declare_local(&mut self, v: &VarDecl, is_static: bool) -> R<()> {
        let l = lower(&v.name);
        if self.local_map.contains_key(&l) {
            return Err(self.err(v.pos, format!("duplicate declaration of {}", v.name)));
        }
        let ty = self.c.resolve_type(&v.ty);
        let array = self.array_decl(v)?;
        let fixed = self.c.fixed_len(self.mi, v)?;
        let auto_new = self.c.auto_new(self.mi, v)?;
        let st = SType {
            ty: ty.clone(),
            array: !matches!(array, ArrayDecl::Scalar),
        };
        let r = if is_static || self.all_static {
            let info = GlobalInfo {
                name: v.name.clone(),
                ty,
                array,
                fixed,
                auto_new,
                public: false,
                with_events: None,
            };

            if let Some(c) = &mut self.c.code.modules[self.mi].class {
                c.fields.push(info);
                VarRef::Field(c.fields.len() - 1)
            } else {
                let id = self.c.code.globals.len();
                self.c.code.globals.push(info);
                VarRef::Global(id)
            }
        } else {
            let id = self.locals.len();
            self.locals.push(LocalInfo {
                name: v.name.clone(),
                ty,
                array,
                fixed,
                auto_new,
            });
            VarRef::Local(id)
        };
        self.local_map.insert(l, Sym::Var(r, st));
        Ok(())
    }

    fn hidden(&mut self, ty: VType) -> usize {
        self.locals.push(LocalInfo {
            name: String::new(),
            ty,
            array: ArrayDecl::Scalar,
            fixed: None,
            auto_new: None,
        });
        self.locals.len() - 1
    }

    fn auto_new_of(&self, r: VarRef) -> Option<usize> {
        match r {
            VarRef::Local(i) => self.locals[i].auto_new,
            VarRef::Global(i) => self.c.code.globals[i].auto_new,
            VarRef::Field(i) => self.c.code.class(self.mi)?.fields[i].auto_new,
        }
    }

    fn var_expr(&self, r: VarRef, st: SType) -> CExpr {
        match self.auto_new_of(r) {
            Some(class) => CExpr {
                st,
                kind: EK::AutoNew(r, class),
            },
            None => CExpr {
                st,
                kind: EK::Var(r),
            },
        }
    }

    fn procedure(mut self, p: &Proc) -> R<ProcCode> {
        let id = self.proc.expect("procedure context");
        let (params, ret) = {
            let sig = &self.c.sigs[id];
            (sig.params.clone(), sig.ret.clone())
        };
        self.all_static = p.is_static;

        self.locals.push(LocalInfo {
            name: p.name.clone(),
            ty: ret.ty.clone(),
            array: if ret.array {
                ArrayDecl::Dynamic
            } else {
                ArrayDecl::Scalar
            },
            fixed: None,
            auto_new: None,
        });
        if p.kind.returns_value() {
            self.local_map
                .insert(lower(&p.name), Sym::Var(VarRef::Local(0), ret.clone()));
        }
        for (prm, info) in p.params.iter().zip(&params) {
            let l = lower(&prm.var.name);
            if self.local_map.contains_key(&l) {
                return Err(self.err(prm.var.pos, format!("duplicate parameter {}", prm.var.name)));
            }
            let slot = self.locals.len();
            self.locals.push(LocalInfo {
                name: prm.var.name.clone(),
                ty: info.ty.clone(),
                array: if info.array {
                    ArrayDecl::Dynamic
                } else {
                    ArrayDecl::Scalar
                },
                fixed: None,
                auto_new: None,
            });
            let st = SType {
                ty: info.ty.clone(),
                array: info.array,
            };
            self.local_map.insert(l, Sym::Var(VarRef::Local(slot), st));
        }
        self.block(&p.body)?;
        self.resolve_fixups()?;
        let sig = &self.c.sigs[id];
        let frame_units =
            crate::frame::frame_units(sig.kind, &params, &ret, &self.locals, &self.ops);
        Ok(ProcCode {
            name: sig.name.clone(),
            module: self.mi,
            kind: sig.kind,
            public: sig.public,
            friend: sig.friend,
            pos: p.pos,
            params,
            ret,
            locals: self.locals,
            ops: self.ops,
            stmt_start: self.starts,
            volatile: self.volatile,
            frame_units,
        })
    }

    fn resolve_fixups(&mut self) -> R<()> {
        for (at, fix) in std::mem::take(&mut self.fixups) {
            let (names, pos) = match fix {
                Fixup::Label(name, pos) => (vec![name], pos),
                Fixup::Labels(names, pos) => (names, pos),
                Fixup::Exit => continue,
            };
            let mut found = Vec::new();
            for name in &names {
                let Some(&target) = self.labels.get(name) else {
                    return Err(self.err(pos, format!("label not defined: {name}")));
                };
                found.push(target);
            }
            let target = found[0];
            match &mut self.ops[at] {
                Op::Jump(t) | Op::GoSub(t) => *t = target,
                Op::OnError(OnErrorMode::GoTo(t)) => *t = target,
                Op::Resume(ResumeKind::Label(t)) => *t = target,
                Op::OnGoTo { targets, .. } => *targets = found,
                _ => unreachable!("label fixup on another op"),
            }
        }
        Ok(())
    }

    fn emit(&mut self, op: Op, start: bool) -> usize {
        self.ops.push(op);
        self.starts.push(start);
        self.ops.len() - 1
    }

    fn here(&self) -> usize {
        self.ops.len()
    }

    fn set_target(&mut self, at: usize, target: usize) {
        match &mut self.ops[at] {
            Op::Jump(t)
            | Op::JumpIf(_, t)
            | Op::JumpIfNot(_, t)
            | Op::ForInit { exit: t, .. }
            | Op::ForEachInit { exit: t, .. } => *t = target,
            _ => unreachable!("jump fixup on another op"),
        }
    }

    fn block(&mut self, body: &[Stmt]) -> R<()> {
        for s in body {
            self.stmt(s)?;
        }
        Ok(())
    }

    fn stmt(&mut self, s: &Stmt) -> R<()> {
        match &s.kind {
            StmtKind::Dim { is_static, vars } => {
                for v in vars {
                    self.declare_local(v, *is_static)?;
                }
            }
            StmtKind::Const(list) => {
                for c in list {
                    let l = lower(&c.name);
                    if self.local_map.contains_key(&l) {
                        return Err(self.err(c.pos, format!("duplicate declaration of {}", c.name)));
                    }
                    let id = {
                        let value = self.expr(&c.value)?;
                        if !is_const(&value) {
                            return Err(self.err(c.pos, "constant expression required"));
                        }
                        let st = match &c.ty {
                            Some(t) => SType::scalar(self.c.resolve_type(&Some(t.clone()))),
                            None => value.st.clone(),
                        };
                        let id = self.c.code.consts.len();
                        self.c.code.consts.push(ConstInfo {
                            name: c.name.clone(),
                            st,
                            value,
                        });
                        id
                    };
                    let st = self.c.code.consts[id].st.clone();
                    self.local_map.insert(l, Sym::Const(id, st));
                }
            }
            StmtKind::ReDim { preserve, items } => {
                for (target, bounds, ty) in items {
                    self.redim(target, bounds, ty, *preserve, s.pos)?;
                }
            }
            StmtKind::Erase(list) => {
                for e in list {
                    match self.place(e)? {
                        (Place::Var(r), st) if st.array || st.ty == VType::Variant => {
                            self.emit(Op::Erase(r), true);
                        }
                        _ => return Err(self.err(e.pos, "Erase needs an array")),
                    }
                }
            }
            StmtKind::Assign {
                target,
                value,
                set: false,
            } if let Some((inner, mut args)) = self.mid_statement(target) => {
                args.push(Arg {
                    name: None,
                    value: Some(value.clone()),
                });
                let call = Expr {
                    pos: target.pos,
                    kind: ExprKind::Index(
                        Box::new(Expr {
                            pos: target.pos,
                            kind: ExprKind::Name("MidAssign".to_string(), None),
                        }),
                        args,
                    ),
                };
                let (place, _) = self.place(&inner)?;
                let v = self.expr(&call)?;
                self.emit(Op::Let(place, v), true);
            }
            StmtKind::Assign { target, value, set } => {
                let (mut place, st) = self.place(target)?;
                let v = self.expr(value)?;
                if let Place::Var(r) = place {
                    if !*set && self.auto_new_of(r).is_some() {
                        let o = self.var_expr(r, st.clone());
                        place = Place::Index(Box::new(o), Vec::new());
                    }
                }
                if *set {
                    if !(st.ty.is_object() || st.ty == VType::Variant) || st.array {
                        return Err(self.err(target.pos, "Set needs an object variable"));
                    }
                    self.emit(Op::Set(place, v), true);
                } else {
                    if let Place::Var(VarRef::Local(_) | VarRef::Global(_)) = place {
                        if st.array && !self.is_dynamic_array(&place) && !is_file_get(value) {
                            return Err(self.err(target.pos, "cannot assign to a fixed array"));
                        }
                    }
                    self.emit(Op::Let(place, v), true);
                }
            }
            StmtKind::Call { callee, args } => self.call_stmt(callee, args, s.pos)?,
            StmtKind::If { arms, otherwise } => {
                let mut ends = Vec::new();
                for (cond, body) in arms {
                    let c = self.expr(cond)?;
                    let test = self.emit(Op::JumpIfNot(c, 0), true);
                    self.block(body)?;
                    ends.push(self.emit(Op::Jump(0), true));
                    let next = self.here();
                    self.set_target(test, next);
                }
                if let Some(body) = otherwise {
                    self.block(body)?;
                }
                let end = self.here();
                for j in ends {
                    self.set_target(j, end);
                }
            }
            StmtKind::Select {
                subject,
                cases,
                otherwise,
            } => self.select(subject, cases, otherwise)?,
            StmtKind::For {
                var,
                from,
                to,
                step,
                body,
            } => {
                let (place, st) = self.place(var)?;
                if !matches!(place, Place::Var(_)) || st.array {
                    return Err(self.err(var.pos, "a For counter must be a variable"));
                }
                if st.ty.is_object() || st.ty == VType::String || st.ty == VType::Boolean {
                    return Err(self.err(var.pos, "a For counter must be numeric"));
                }
                let from = self.expr(from)?;
                let to = self.expr(to)?;
                let step = step.as_ref().map(|e| self.expr(e)).transpose()?;
                let hidden = self.hidden(st.ty.clone());
                self.hidden(st.ty.clone());
                let init = self.emit(
                    Op::ForInit {
                        var: place.clone(),
                        from,
                        to,
                        step,
                        hidden,
                        exit: 0,
                    },
                    true,
                );
                self.loops.push((LoopKind::For, Vec::new()));
                self.block(body)?;
                self.emit(
                    Op::ForNext {
                        var: place,
                        counter: st.ty.clone(),
                        hidden,
                        body: init + 1,
                    },
                    true,
                );
                self.close_loop(Some(init));
            }
            StmtKind::ForEach {
                var,
                collection,
                body,
            } => {
                let (place, st) = self.place(var)?;
                if !matches!(place, Place::Var(_)) || !(st.ty.is_dynamic()) || st.array {
                    return Err(self.err(
                        var.pos,
                        "a For Each variable must be a Variant or an object",
                    ));
                }
                let collection = self.expr(collection)?;
                let hidden = self.hidden(VType::Variant);
                self.hidden(VType::Variant);
                self.hidden(VType::Variant);
                let again = collection.clone();
                let init = self.emit(
                    Op::ForEachInit {
                        var: place.clone(),
                        collection,
                        hidden,
                        exit: 0,
                    },
                    true,
                );
                self.loops.push((LoopKind::For, Vec::new()));
                self.block(body)?;
                self.emit(
                    Op::ForEachNext {
                        var: place,
                        collection: again,
                        hidden,
                        body: init + 1,
                    },
                    true,
                );
                self.close_loop(Some(init));
            }
            StmtKind::Do { pre, post, body } => {
                let top = self.here();
                let mut test = None;
                if let Some((kind, cond)) = pre {
                    let c = self.expr(cond)?;
                    test = Some(match kind {
                        LoopCond::While => self.emit(Op::JumpIfNot(c, 0), true),
                        LoopCond::Until => self.emit(Op::JumpIf(c, 0), true),
                    });
                }
                self.loops.push((LoopKind::Do, Vec::new()));
                self.block(body)?;
                match post {
                    Some((kind, cond)) => {
                        let c = self.expr(cond)?;
                        match kind {
                            LoopCond::While => self.emit(Op::JumpIf(c, top), true),
                            LoopCond::Until => self.emit(Op::JumpIfNot(c, top), true),
                        };
                    }
                    None => {
                        self.emit(Op::Jump(top), true);
                    }
                }
                let exit = self.here();
                if let Some(t) = test {
                    self.set_target(t, exit);
                }
                self.close_loop(None);
            }
            StmtKind::With { object, body } => {
                let obj = self.expr(object)?;
                let slot = self.hidden(VType::Variant);
                self.emit(Op::Bind(slot, obj), true);
                self.with.push(slot);
                self.block(body)?;
                self.with.pop();

                self.emit(Op::Bind(slot, lit(Variant::Empty)), true);
            }
            StmtKind::Exit(kind) => match kind {
                ExitKind::Function | ExitKind::Sub | ExitKind::Property => {
                    let id = self.proc.expect("procedure");
                    let have = self.c.sigs[id].kind;
                    let ok = match kind {
                        ExitKind::Function => have == ProcKind::Function,
                        ExitKind::Sub => have == ProcKind::Sub,
                        _ => have.is_property(),
                    };
                    if !ok {
                        return Err(self.err(s.pos, "Exit does not match the procedure"));
                    }
                    self.emit(Op::Return, true);
                }
                ExitKind::For | ExitKind::Do => {
                    let k = if *kind == ExitKind::For {
                        LoopKind::For
                    } else {
                        LoopKind::Do
                    };
                    let Some(depth) = self.loops.iter().rposition(|(lk, _)| *lk == k) else {
                        return Err(self.err(s.pos, "Exit outside its loop"));
                    };
                    let j = self.emit(Op::Jump(0), true);
                    self.loops[depth].1.push(j);
                    self.fixups.push((j, Fixup::Exit));
                }
            },
            StmtKind::GoTo(label) => {
                let j = self.emit(Op::Jump(0), true);
                self.fixups.push((j, Fixup::Label(label.clone(), s.pos)));
            }
            StmtKind::Label(name) => {
                if self.labels.insert(name.clone(), self.here()).is_some() {
                    return Err(self.err(s.pos, format!("duplicate label {name}")));
                }
                if let Ok(n) = name.parse::<i32>() {
                    self.emit(Op::Line(n), true);
                }
            }
            StmtKind::OnError(mode) => {
                let m = match mode {
                    ast::OnError::Disable => OnErrorMode::Disable,
                    ast::OnError::Reset => OnErrorMode::Reset,
                    ast::OnError::ResumeNext => OnErrorMode::ResumeNext,
                    ast::OnError::GoTo(_) => OnErrorMode::GoTo(0),
                };
                let at = self.emit(Op::OnError(m), true);
                if let ast::OnError::GoTo(label) = mode {
                    self.fixups.push((at, Fixup::Label(label.clone(), s.pos)));
                }
            }
            StmtKind::Resume(target) => {
                let k = match target {
                    ResumeTarget::Again => ResumeKind::Again,
                    ResumeTarget::Next => ResumeKind::Next,
                    ResumeTarget::Label(_) => ResumeKind::Label(0),
                };
                let at = self.emit(Op::Resume(k), true);
                if let ResumeTarget::Label(label) = target {
                    self.fixups.push((at, Fixup::Label(label.clone(), s.pos)));
                }
            }
            StmtKind::DebugPrint(list) => {
                let mut items = Vec::new();
                for a in list {
                    items.push(match a {
                        PrintArg::Value(e) if is_print_layout(e) => continue,
                        PrintArg::Value(e) => {
                            let v = self.expr(e)?;
                            self.no_array(&v, e.pos)?;
                            PrintItem::Value(v)
                        }
                        PrintArg::Zone => PrintItem::Zone,
                        PrintArg::Join => PrintItem::Join,
                    });
                }
                self.emit(Op::DebugPrint(items), true);
            }
            StmtKind::Block(body) => self.block(body)?,
            StmtKind::DebugAssert(e) => {
                let v = self.expr(e)?;
                self.no_array(&v, e.pos)?;
                self.emit(Op::DebugAssert(v), true);
            }
            StmtKind::RaiseEvent { name, args } => {
                let Some(&event) = self.c.scopes[self.mi].events.get(&lower(name)) else {
                    return Err(self.err(s.pos, format!("event not found: {name}")));
                };
                let Some(class) = self.c.code.class(self.mi) else {
                    return Err(self.err(s.pos, "unsupported: RaiseEvent outside a class module"));
                };
                let params = class.events[event].params.clone();
                let args = self.call_args_for(&params, name, args, s.pos)?;
                self.emit(Op::RaiseEvent { event, args }, true);
            }
            StmtKind::End => {
                self.emit(Op::End, true);
            }
            StmtKind::Stop => {
                self.emit(Op::Stop, true);
            }
            StmtKind::GoSub(label) => {
                let j = self.emit(Op::GoSub(0), true);
                self.fixups.push((j, Fixup::Label(label.clone(), s.pos)));
            }
            StmtKind::Return => {
                self.emit(Op::GoSubReturn, true);
            }
            StmtKind::OnGoTo {
                index,
                labels,
                gosub,
            } => {
                let index = self.expr(index)?;
                self.no_array(&index, s.pos)?;
                let j = self.emit(
                    Op::OnGoTo {
                        index,
                        targets: Vec::new(),
                        gosub: *gosub,
                    },
                    true,
                );
                self.fixups.push((j, Fixup::Labels(labels.clone(), s.pos)));
            }
            StmtKind::LSet {
                target,
                value,
                right,
            } => {
                let (place, st) = self.place(target)?;
                if st.array
                    || st.ty != VType::String
                    || !matches!(place, Place::Var(_) | Place::Elem(..))
                {
                    return Err(self.err(
                        target.pos,
                        "unsupported: LSet and RSet need a String variable",
                    ));
                }
                let value = self.expr(value)?;
                self.no_array(&value, s.pos)?;
                self.emit(
                    Op::LSet {
                        place,
                        value,
                        right: *right,
                    },
                    true,
                );
            }
        }
        Ok(())
    }

    fn close_loop(&mut self, init: Option<usize>) {
        let exit = self.here();
        if let Some(i) = init {
            self.set_target(i, exit);
        }
        if let Some((_, exits)) = self.loops.pop() {
            for j in exits {
                self.set_target(j, exit);
            }
        }
    }

    fn is_dynamic_array(&self, place: &Place) -> bool {
        match place {
            Place::Var(VarRef::Local(i)) => !matches!(self.locals[*i].array, ArrayDecl::Fixed(_)),
            Place::Var(VarRef::Global(i)) => {
                !matches!(self.c.code.globals[*i].array, ArrayDecl::Fixed(_))
            }
            Place::Var(VarRef::Field(i)) => self
                .c
                .code
                .class(self.mi)
                .is_some_and(|c| !matches!(c.fields[*i].array, ArrayDecl::Fixed(_))),
            _ => false,
        }
    }

    fn case_test(&mut self, op: BinOp, a: CExpr, b: CExpr, pos: Pos) -> R<CExpr> {
        let (Some(var), Some(test)) = (find_builtin("CVar"), find_builtin("Select<test>")) else {
            return self.compare(op, a, b, pos);
        };

        let whole;
        let (a, b) = {
            let conv = |subject: &CExpr, other: CExpr| {
                let name = match subject.ty() {
                    VType::Byte => "CByte",
                    VType::Integer => "CInt",
                    VType::Long => "CLng",
                    _ => return other,
                };
                match find_builtin(name) {
                    Some(f) if matches!(other.ty(), VType::Single | VType::Double) => CExpr {
                        st: SType::scalar(subject.ty().clone()),
                        kind: EK::Builtin(BuiltinRef(f), vec![other]),
                    },
                    _ => other,
                }
            };
            let is_value = |e: &CExpr| matches!(e.kind, EK::Builtin(..));
            if matches!(a.kind, EK::Var(VarRef::Local(_))) {
                let was = is_value(&b);
                let b = conv(&a, b);
                whole = !was && is_value(&b);
                (a, b)
            } else {
                let was = is_value(&a);
                let a = conv(&b, a);
                whole = !was && is_value(&a);
                (a, b)
            }
        };

        let wrap = |e: CExpr| {
            if matches!(e.kind, EK::Var(VarRef::Local(_))) && e.ty() != &VType::Variant && !whole {
                CExpr {
                    st: SType::scalar(VType::Variant),
                    kind: EK::Builtin(BuiltinRef(var), vec![e]),
                }
            } else {
                e
            }
        };
        let c = self.compare(op, wrap(a), wrap(b), pos)?;
        Ok(CExpr {
            st: SType::scalar(VType::Boolean),
            kind: EK::Builtin(BuiltinRef(test), vec![c]),
        })
    }

    fn select(
        &mut self,
        subject: &Expr,
        cases: &[(Vec<CaseClause>, Vec<Stmt>)],
        otherwise: &Option<Vec<Stmt>>,
    ) -> R<()> {
        let subj = self.expr(subject)?;
        let st = subj.st.clone();
        if st.array {
            return Err(self.err(subject.pos, "Select Case needs a value"));
        }
        let slot = self.hidden(st.ty.clone());
        self.emit(Op::Bind(slot, subj), true);

        let over = self.emit(Op::Jump(0), false);
        let mut ends = vec![self.emit(Op::Jump(0), true)];
        let tests = self.here();
        self.set_target(over, tests);
        let s = CExpr {
            st: st.clone(),
            kind: EK::Var(VarRef::Local(slot)),
        };
        for (clauses, body) in cases {
            let mut to_body = Vec::new();
            for clause in clauses {
                match clause {
                    CaseClause::Value(e) => {
                        let v = self.expr(e)?;
                        let c = self.case_test(BinOp::Eq, s.clone(), v, e.pos)?;
                        to_body.push(self.emit(Op::JumpIf(c, 0), true));
                    }
                    CaseClause::Is(op, e) => {
                        let v = self.expr(e)?;
                        let c = self.case_test(*op, s.clone(), v, e.pos)?;
                        to_body.push(self.emit(Op::JumpIf(c, 0), true));
                    }
                    CaseClause::Range(a, b) => {
                        let lo = self.expr(a)?;
                        let hi = self.expr(b)?;
                        let c1 = self.case_test(BinOp::Le, lo, s.clone(), a.pos)?;
                        let c2 = self.case_test(BinOp::Le, s.clone(), hi, b.pos)?;
                        let skip = self.emit(Op::JumpIfNot(c1, 0), true);
                        to_body.push(self.emit(Op::JumpIf(c2, 0), false));
                        let next = self.here();
                        self.set_target(skip, next);
                    }
                }
            }
            let miss = self.emit(Op::Jump(0), false);
            let start = self.here();
            for j in to_body {
                self.set_target(j, start);
            }
            self.block(body)?;
            ends.push(self.emit(Op::Jump(0), true));
            let next = self.here();
            self.set_target(miss, next);
        }
        if let Some(body) = otherwise {
            self.block(body)?;
        }
        let end = self.here();
        for j in ends {
            self.set_target(j, end);
        }
        Ok(())
    }

    fn redim(
        &mut self,
        target: &Expr,
        bounds: &[Bounds],
        ty: &Option<TypeName>,
        preserve: bool,
        pos: Pos,
    ) -> R<()> {
        let ExprKind::Name(name, suffix) = &target.kind else {
            return Err(self.err(pos, "ReDim needs a variable"));
        };
        let elem_given = ty
            .as_ref()
            .map(|t| self.c.resolve_type(&Some(t.clone())))
            .or_else(|| suffix.map(VType::of_suffix));
        let sym = self.lookup(name);
        let (r, st) = match sym {
            Some(Sym::Var(r, st)) => (r, st),
            None => {
                let ty = elem_given.clone().unwrap_or(VType::Variant);
                let id = self.locals.len();
                self.locals.push(LocalInfo {
                    name: name.clone(),
                    ty: ty.clone(),
                    array: ArrayDecl::Dynamic,
                    fixed: None,
                    auto_new: None,
                });
                let st = SType { ty, array: true };
                self.local_map
                    .insert(lower(name), Sym::Var(VarRef::Local(id), st.clone()));
                (VarRef::Local(id), st)
            }
            Some(_) => return Err(self.err(pos, format!("{name} is not a variable"))),
        };
        if st.array && !self.is_dynamic_array(&Place::Var(r)) {
            return Err(self.err(pos, "array already dimensioned"));
        }
        if !st.array && st.ty != VType::Variant {
            return Err(self.err(pos, "ReDim needs an array or a Variant"));
        }
        if let Some(t) = &elem_given {
            if st.array && *t != st.ty {
                return Err(self.err(pos, "ReDim cannot change the element type"));
            }
        }
        let elem = if st.array {
            st.ty.clone()
        } else {
            elem_given.unwrap_or(VType::Variant)
        };
        let mut bs = Vec::new();
        for b in bounds {
            bs.push(self.bounds(b)?);
        }
        self.emit(
            Op::ReDim {
                var: r,
                preserve,
                bounds: bs,
                elem,
            },
            true,
        );
        Ok(())
    }

    fn call_stmt(&mut self, callee: &Expr, args: &[ast::Arg], pos: Pos) -> R<()> {
        if let ExprKind::Member(Some(obj), m) = &callee.kind {
            if let ExprKind::Name(n, None) = &obj.kind {
                if matches!(self.lookup(n), Some(Sym::Err)) {
                    match lower(m).as_str() {
                        "clear" if args.is_empty() => {
                            self.emit(Op::ErrClear, true);
                            return Ok(());
                        }
                        "raise" => {
                            let a = self.named_args(args)?;
                            if a.is_empty() {
                                return Err(self.err(pos, "Err.Raise needs a number"));
                            }
                            self.emit(Op::ErrRaise(a), true);
                            return Ok(());
                        }
                        _ => return Err(self.err(pos, format!("Err.{m} is not a method"))),
                    }
                }
            }
        }
        let e = match &callee.kind {
            ExprKind::Name(name, _) => match self.lookup(name) {
                Some(Sym::Proc(id)) => {
                    let a = self.call_args(id, args, pos)?;
                    CExpr {
                        st: SType::scalar(VType::Variant),
                        kind: EK::Call(id, a),
                    }
                }
                Some(Sym::Var(r, st)) if st.ty.is_dynamic() && !st.array => {
                    let a = self.named_args(args)?;
                    CExpr {
                        st: SType::scalar(VType::Variant),
                        kind: EK::Index(
                            Box::new(CExpr {
                                st,
                                kind: EK::Var(r),
                            }),
                            a,
                        ),
                    }
                }
                Some(Sym::Builtin(_) | Sym::Intrinsic(_) | Sym::LibGlobal(_)) => {
                    let wrapped = Expr {
                        pos,
                        kind: ExprKind::Index(Box::new(callee.clone()), args.to_vec()),
                    };
                    self.expr(&wrapped)?
                }
                None => return Err(self.err(pos, format!("Sub or Function not defined: {name}"))),
                Some(_) => return Err(self.err(pos, format!("{name} is not a procedure"))),
            },
            ExprKind::Member(obj, name) => {
                if let Some(obj) = obj {
                    if let Some(e) = self.module_member(obj, name, args, pos)? {
                        self.emit(Op::Eval(e), true);
                        return Ok(());
                    }
                }
                let wrapped = Expr {
                    pos,
                    kind: ExprKind::Index(Box::new(callee.clone()), args.to_vec()),
                };
                if args.is_empty() {
                    self.expr(callee)?
                } else {
                    self.expr(&wrapped)?
                }
            }
            _ => return Err(self.err(pos, "expected a procedure call")),
        };
        self.emit(Op::Eval(e), true);
        Ok(())
    }

    fn lookup(&self, name: &str) -> Option<Sym> {
        if let Some(s) = self.local_map.get(&lower(name)) {
            return Some(s.clone());
        }
        self.c.lookup_module(self.mi, name)
    }

    fn resolve(&mut self, name: &str, suffix: Option<char>, pos: Pos) -> R<Sym> {
        if suffix == Some('$') && matches!(self.lookup(name), None | Some(Sym::Builtin(_))) {
            if let Some(b) = find_builtin(&format!("{name}$")) {
                return Ok(Sym::Builtin(BuiltinRef(b)));
            }
        }
        if let Some(s) = self.lookup(name) {
            if let (Some(c), Sym::Var(_, st)) = (suffix, &s) {
                if st.ty != VType::of_suffix(c) {
                    return Err(self.err(pos, format!("type character does not match {name}")));
                }
            }
            return Ok(s);
        }
        if self.options().explicit || self.proc.is_none() {
            return Err(self.err(pos, format!("variable not defined: {name}")));
        }
        let v = VarDecl {
            name: name.to_string(),
            pos,
            ty: suffix.map(|c| TypeName::Builtin(VType::of_suffix(c))),
            array: None,
            new: false,
            fixed_len: None,
        };
        self.declare_local(&v, false)?;
        Ok(self.lookup(name).expect("just declared"))
    }

    pub(crate) fn expr(&mut self, e: &Expr) -> R<CExpr> {
        let pos = e.pos;
        Ok(match &e.kind {
            ExprKind::Lit(v) => lit(v.clone()),
            ExprKind::Nothing => lit(Variant::Dispatch(None)),
            ExprKind::Empty => CExpr {
                st: SType::scalar(VType::Variant),
                kind: EK::Lit(Variant::Empty),
            },
            ExprKind::Null => CExpr {
                st: SType::scalar(VType::Variant),
                kind: EK::Lit(Variant::Null),
            },
            ExprKind::Paren(inner) => self.expr(inner)?,
            ExprKind::Name(name, suffix) => self.name_value(name, *suffix, pos)?,
            ExprKind::Index(target, args) => self.index(target, args, pos)?,
            ExprKind::Member(obj, name) => self.member(obj.as_deref(), name, Vec::new(), pos)?,
            ExprKind::Unary(op, a) => {
                let a = self.expr(a)?;
                self.no_array(&a, pos)?;
                CExpr {
                    st: SType::scalar(unary_type(*op == UnOp::Not, a.ty())),
                    kind: EK::Unary(*op, Box::new(a)),
                }
            }
            ExprKind::Binary(op, a, b) => {
                let a = self.expr(a)?;
                let b = self.expr(b)?;
                self.compare(*op, a, b, pos)?
            }
            ExprKind::TypeOf(a, class) => {
                let a = self.expr(a)?;
                CExpr {
                    st: SType::scalar(VType::Boolean),
                    kind: EK::TypeOf(Box::new(a), class.clone()),
                }
            }
            ExprKind::New(class) => {
                match self.c.class_named(class).or_else(|| builtin_class(class)) {
                    Some(j) => CExpr {
                        st: SType::scalar(VType::Object(Some(lower(class)))),
                        kind: EK::New(j),
                    },
                    None => return Err(self.err(pos, format!("unsupported: New {class}"))),
                }
            }
            ExprKind::Me => {
                let m = &self.c.code.modules[self.mi];
                let ty = match m.kind {
                    ModuleKind::Class => VType::Object(Some(lower(&m.name))),
                    ModuleKind::Document => VType::Object(None),
                    ModuleKind::Standard => return Err(self.err(pos, "invalid use of Me")),
                };
                CExpr {
                    st: SType::scalar(ty),
                    kind: EK::Me,
                }
            }
        })
    }

    fn no_array(&self, e: &CExpr, pos: Pos) -> R<()> {
        if e.st.array {
            Err(self.err(pos, "type mismatch: an array is not a value here"))
        } else {
            Ok(())
        }
    }

    fn compare(&mut self, op: BinOp, a: CExpr, b: CExpr, pos: Pos) -> R<CExpr> {
        self.no_array(&a, pos)?;
        self.no_array(&b, pos)?;
        if op == BinOp::Is && (!(a.ty().is_dynamic()) || !(b.ty().is_dynamic())) {
            return Err(self.err(pos, "Is needs objects"));
        }
        let ty = binary_type(op, a.ty(), b.ty());
        Ok(CExpr {
            st: SType::scalar(ty),
            kind: EK::Binary(op, Box::new(a), Box::new(b)),
        })
    }

    fn name_value(&mut self, name: &str, suffix: Option<char>, pos: Pos) -> R<CExpr> {
        Ok(match self.resolve(name, suffix, pos)? {
            Sym::Var(r, st) => self.var_expr(r, st),
            Sym::Const(id, st) => CExpr {
                st,
                kind: EK::Const(id),
            },
            Sym::Proc(id) => {
                if !self.c.sigs[id].kind.returns_value() {
                    return Err(self.err(pos, format!("expected a Function or variable: {name}")));
                }
                let a = self.call_args(id, &[], pos)?;
                CExpr {
                    st: self.c.sigs[id].ret.clone(),
                    kind: EK::Call(id, a),
                }
            }
            Sym::Err => CExpr {
                st: SType::scalar(VType::Long),
                kind: EK::Err(ErrProp::Number),
            },
            Sym::Intrinsic(i) => self.intrinsic_call(i, &[], pos)?,
            Sym::Builtin(b) => self.builtin_call(b, &[], pos)?,
            Sym::Value(v) => lit(v),
            Sym::LibGlobal(n) => CExpr {
                st: SType::scalar(VType::Variant),
                kind: EK::Global(n, Vec::new()),
            },
            Sym::Module(j) if self.c.code.modules[j].kind == ModuleKind::Document => CExpr {
                st: SType::scalar(VType::Object(None)),
                kind: EK::Document(j),
            },
            Sym::Module(_) | Sym::Enum(..) => {
                return Err(self.err(pos, format!("{name} is not a value")))
            }
        })
    }

    fn module_member(
        &mut self,
        obj: &Expr,
        name: &str,
        args: &[ast::Arg],
        pos: Pos,
    ) -> R<Option<CExpr>> {
        let ExprKind::Name(n, None) = &obj.kind else {
            return Ok(None);
        };
        if self.local_map.contains_key(&lower(n)) {
            return Ok(None);
        }
        let Some(Sym::Module(j)) = self.c.lookup_module(self.mi, n) else {
            return Ok(None);
        };
        let kind = self.c.code.modules[j].kind;
        if kind == ModuleKind::Class {
            return Err(self.err(pos, format!("invalid use of the class name {n}")));
        }
        let l = lower(name);
        let doc = kind == ModuleKind::Document;
        let own = !doc || j == self.mi;
        let proc = self.c.scopes[j].procs.get(&l).map(|p| p.0);
        let var = self.c.scopes[j].vars.get(&l).copied();
        let konst = self.c.scopes[j].consts.get(&l).map(|c| c.0);
        if let Some(id) = proc {
            if own || self.c.sigs[id].public {
                let a = self.call_args(id, args, pos)?;
                return Ok(Some(CExpr {
                    st: self.c.sigs[id].ret.clone(),
                    kind: EK::Call(id, a),
                }));
            }
        }
        if let Some((id, public)) = var {
            if public || own {
                let Sym::Var(r, st) = self.c.var_sym(j, id) else {
                    unreachable!("a variable")
                };
                if args.is_empty() {
                    return Ok(Some(self.var_expr(r, st)));
                }
                if st.array {
                    let idx = self.indices(args, pos)?;
                    return Ok(Some(CExpr {
                        st: SType::scalar(st.ty),
                        kind: EK::Elem(r, idx),
                    }));
                }
                return Err(self.err(pos, format!("unsupported: {n}.{name}(...)")));
            }
        }
        if let Some(id) = konst {
            if id != usize::MAX && args.is_empty() {
                return Ok(Some(CExpr {
                    st: self.c.code.consts[id].st.clone(),
                    kind: EK::Const(id),
                }));
            }
        }
        if doc {
            let a = self.named_args(args)?;
            let o = CExpr {
                st: SType::scalar(VType::Object(None)),
                kind: EK::Document(j),
            };
            return Ok(Some(CExpr {
                st: SType::scalar(VType::Variant),
                kind: EK::Member(Box::new(o), name.to_string(), a),
            }));
        }
        Err(self.err(pos, format!("{n}.{name} not found")))
    }

    fn index(&mut self, target: &Expr, args: &[ast::Arg], pos: Pos) -> R<CExpr> {
        match &target.kind {
            ExprKind::Name(name, suffix) => {
                let own_function = self.proc.is_some_and(|id| {
                    self.c.sigs[id].kind.returns_value()
                        && lower(&self.c.sigs[id].name) == lower(name)
                });
                if own_function && !self.c.sigs[self.proc.unwrap_or(0)].ret.array {
                    let id = self.proc.expect("procedure");
                    let a = self.call_args(id, args, pos)?;
                    return Ok(CExpr {
                        st: self.c.sigs[id].ret.clone(),
                        kind: EK::Call(id, a),
                    });
                }
                match self.resolve(name, *suffix, pos)? {
                    Sym::Var(r, st) => {
                        if st.array {
                            let idx = self.indices(args, pos)?;
                            return Ok(CExpr {
                                st: SType::scalar(st.ty),
                                kind: EK::Elem(r, idx),
                            });
                        }
                        if !st.ty.is_dynamic() {
                            return Err(self.err(pos, format!("expected an array: {name}")));
                        }
                        let a = self.named_args(args)?;
                        if a.iter().all(|x| x.name.is_none()) && self.auto_new_of(r).is_none() {
                            let idx = a.into_iter().map(|x| x.value).collect();
                            return Ok(CExpr {
                                st: SType::scalar(VType::Variant),
                                kind: EK::Elem(r, idx),
                            });
                        }
                        Ok(CExpr {
                            st: SType::scalar(VType::Variant),
                            kind: EK::Index(Box::new(self.var_expr(r, st)), a),
                        })
                    }
                    Sym::Proc(id) => {
                        let a = self.call_args(id, args, pos)?;
                        Ok(CExpr {
                            st: self.c.sigs[id].ret.clone(),
                            kind: EK::Call(id, a),
                        })
                    }
                    Sym::Intrinsic(i) => self.intrinsic_call(i, args, pos),
                    Sym::Builtin(b) => self.builtin_call(b, args, pos),
                    Sym::LibGlobal(n) => {
                        let a = self.named_args(args)?;
                        Ok(CExpr {
                            st: SType::scalar(VType::Variant),
                            kind: EK::Global(n, a),
                        })
                    }
                    Sym::Err => Err(self.err(pos, "Err takes no arguments")),
                    _ => Err(self.err(pos, format!("{name} cannot take arguments"))),
                }
            }
            ExprKind::Member(obj, name) => {
                if let Some(o) = obj {
                    if let Some(e) = self.module_member(o, name, args, pos)? {
                        return Ok(e);
                    }
                }
                let a = self.named_args(args)?;
                self.member(obj.as_deref(), name, a, pos)
            }
            _ => {
                let t = self.expr(target)?;
                let a = self.named_args(args)?;
                Ok(CExpr {
                    st: SType::scalar(VType::Variant),
                    kind: EK::Index(Box::new(t), a),
                })
            }
        }
    }

    fn indices(&mut self, args: &[ast::Arg], pos: Pos) -> R<Vec<CExpr>> {
        let mut out = Vec::new();
        for a in args {
            match (&a.name, &a.value) {
                (None, Some(v)) => {
                    let v = self.expr(v)?;
                    self.no_array(&v, pos)?;
                    out.push(v);
                }
                _ => return Err(self.err(pos, "an array index cannot be named or left out")),
            }
        }
        if out.is_empty() {
            return Err(self.err(pos, "an array needs an index"));
        }
        Ok(out)
    }

    fn named_args(&mut self, args: &[ast::Arg]) -> R<Vec<NamedArg>> {
        let mut out = Vec::new();
        for a in args {
            let value = match &a.value {
                Some(v) => self.expr(v)?,
                None => CExpr {
                    st: SType::scalar(VType::Variant),
                    kind: EK::Missing,
                },
            };
            let place = match &a.value {
                Some(v) if self.is_place_like(v) => Some(self.place(v)?.0),
                _ => None,
            };
            out.push(NamedArg {
                name: a.name.clone(),
                value,
                place,
            });
        }
        Ok(out)
    }

    fn is_place_like(&self, e: &Expr) -> bool {
        match &e.kind {
            ExprKind::Name(n, _) => matches!(self.lookup(n), Some(Sym::Var(..)) | None),
            ExprKind::Index(t, _) => match &t.kind {
                ExprKind::Name(n, _) => matches!(self.lookup(n), Some(Sym::Var(_, st)) if st.array),
                _ => false,
            },
            _ => false,
        }
    }

    fn member(
        &mut self,
        obj: Option<&Expr>,
        name: &str,
        args: Vec<NamedArg>,
        pos: Pos,
    ) -> R<CExpr> {
        let late = |o: CExpr, args| CExpr {
            st: SType::scalar(VType::Variant),
            kind: EK::Member(Box::new(o), name.to_string(), args),
        };
        let Some(obj) = obj else {
            let Some(&slot) = self.with.last() else {
                return Err(self.err(pos, "a leading . needs a With block"));
            };
            let o = CExpr {
                st: SType::scalar(VType::Variant),
                kind: EK::Var(VarRef::Local(slot)),
            };
            return Ok(late(o, args));
        };
        if let Some(e) = self.module_member(obj, name, &[], pos)? {
            if !args.is_empty() {
                return Err(self.err(pos, "unsupported: Module.member(args) here"));
            }
            return Ok(e);
        }
        if let ExprKind::Name(n, None) = &obj.kind {
            if !self.local_map.contains_key(&lower(n)) {
                match self.c.lookup_module(self.mi, n) {
                    Some(Sym::Err) => {
                        let prop = match lower(name).as_str() {
                            "number" => (ErrProp::Number, VType::Long),
                            "description" => (ErrProp::Description, VType::String),
                            "source" => (ErrProp::Source, VType::String),
                            "helpfile" => (ErrProp::HelpFile, VType::String),
                            "helpcontext" => (ErrProp::HelpContext, VType::Long),
                            "lastdllerror" => (ErrProp::LastDllError, VType::Long),
                            _ => return Err(self.err(pos, format!("unsupported: Err.{name}"))),
                        };
                        return Ok(CExpr {
                            st: SType::scalar(prop.1),
                            kind: EK::Err(prop.0),
                        });
                    }
                    Some(Sym::Enum(j, en)) => {
                        let id = self.c.scopes[j].enums[&en].0.get(&lower(name)).copied();
                        return match id {
                            Some(id) if id != usize::MAX && args.is_empty() => Ok(CExpr {
                                st: SType::scalar(VType::Long),
                                kind: EK::Const(id),
                            }),
                            _ => Err(self.err(pos, format!("{n}.{name} not found"))),
                        };
                    }
                    _ => {}
                }
            }
            if lower(n) == "vba" && !self.local_map.contains_key("vba") {
                let inner = Expr {
                    pos,
                    kind: ExprKind::Name(name.to_string(), None),
                };
                if args.is_empty() {
                    return self.expr(&inner);
                }
                return Err(self.err(pos, "unsupported: VBA.Function(args); call it unqualified"));
            }
        }
        let o = self.expr(obj)?;
        if o.st.array {
            return Err(self.err(pos, "an array has no members"));
        }
        if let EK::Global(g, _) = &o.kind {
            if g.eq_ignore_ascii_case("application") && name.eq_ignore_ascii_case("volatile") {
                self.volatile = true;
            }
        }
        if !o.ty().is_dynamic() {
            return Err(self.err(pos, format!("invalid qualifier before .{name}")));
        }
        Ok(late(o, args))
    }

    fn builtin_call(&mut self, b: BuiltinRef, args: &[ast::Arg], pos: Pos) -> R<CExpr> {
        if args.len() < usize::from(b.0.min_args) || args.len() > usize::from(b.0.max_args) {
            return Err(self.err(pos, format!("wrong number of arguments to {}", b.0.name)));
        }
        let mut out = Vec::new();
        for a in args {
            if a.name.is_some() {
                return Err(self.err(pos, format!("unsupported: named arguments to {}", b.0.name)));
            }
            out.push(match &a.value {
                Some(v) => self.expr(v)?,
                None => CExpr {
                    st: SType::scalar(VType::Variant),
                    kind: EK::Missing,
                },
            });
        }
        if out.len() < usize::from(b.0.min_args)
            || out
                .iter()
                .take(usize::from(b.0.min_args))
                .any(|e| matches!(e.kind, EK::Missing))
        {
            return Err(self.err(pos, format!("argument not optional ({})", b.0.name)));
        }

        let mut b = b;
        if let [a] = out.as_slice() {
            if b.0.name == "Abs"
                && !a.st.array
                && matches!(a.ty(), VType::Integer | VType::Long | VType::LongLong)
            {
                if let Some(t) = find_builtin("Abs<typed>") {
                    b = BuiltinRef(t);
                }
            }
        }

        if let [a] = out.as_slice() {
            if b.0.name == "CDate"
                && !a.st.array
                && matches!(
                    a.ty(),
                    VType::Byte
                        | VType::Integer
                        | VType::Long
                        | VType::LongLong
                        | VType::Single
                        | VType::Double
                        | VType::Currency
                )
            {
                if let Some(t) = find_builtin("CDate<typed>") {
                    b = BuiltinRef(t);
                }
            }
        }

        if let [a] = out.as_slice() {
            let bytes = b.0.name.eq_ignore_ascii_case("lenb");
            if (bytes || b.0.name.eq_ignore_ascii_case("len")) && !a.st.array {
                let size = match a.ty() {
                    VType::Byte => Some(1),
                    VType::Boolean | VType::Integer => Some(2),
                    VType::Long | VType::Single => Some(4),
                    VType::LongLong | VType::Double | VType::Currency | VType::Date => Some(8),
                    _ => None,
                };
                if let Some(n) = size {
                    return Ok(lit(Variant::I4(n)));
                }
            }
        }

        if matches!(b.0.name, "FilePut<>" | "FileGet<>" | "InputField<>") {
            if let Some(a) = out.last() {
                let elem = a.ty().var_type().map_or(12, |t| t as i32);
                let mut code = if a.st.array { 0x2000 | elem } else { elem };

                let fixed = match &a.kind {
                    EK::Var(VarRef::Local(i)) => {
                        self.locals.get(*i).is_some_and(|l| l.fixed.is_some())
                    }
                    EK::Var(VarRef::Global(i)) => self
                        .c
                        .code
                        .globals
                        .get(*i)
                        .is_some_and(|g| g.fixed.is_some()),
                    _ => false,
                };
                if fixed && !a.st.array {
                    code |= 0x1000;
                }
                out.push(lit(Variant::I4(code)));
            }
        }

        let ret = if b.0.name.to_ascii_lowercase().starts_with("len")
            && out.first().is_some_and(|a| a.ty().is_dynamic())
        {
            VType::Variant
        } else {
            b.0.ret.clone()
        };
        Ok(CExpr {
            st: SType::scalar(ret),
            kind: EK::Builtin(b, out),
        })
    }

    fn intrinsic_call(&mut self, i: Intrinsic, args: &[ast::Arg], pos: Pos) -> R<CExpr> {
        let (min, max) = match i {
            Intrinsic::Array => (0, usize::MAX),
            Intrinsic::UBound | Intrinsic::LBound => (1, 2),
            Intrinsic::Ask(Interaction::MsgBox) => (1, 5),
            Intrinsic::Ask(Interaction::InputBox) => (1, 7),
            Intrinsic::Ask(Interaction::Beep) | Intrinsic::Erl => (0, 0),
            Intrinsic::CreateObject => (1, 2),
            Intrinsic::GetObject => (0, 2),
            Intrinsic::DeclareCall => (2, usize::MAX),
            Intrinsic::CallByName => (3, usize::MAX),
            _ => (1, 1),
        };
        if args.len() < min || args.len() > max {
            return Err(self.err(pos, format!("wrong number of arguments to {i:?}")));
        }

        if i == Intrinsic::TypeName {
            if let Some(ExprKind::Name(n, _)) =
                args.first().and_then(|a| a.value.as_ref()).map(|v| &v.kind)
            {
                if matches!(self.lookup(n), Some(Sym::Err)) {
                    return Ok(lit(Variant::Bstr("ErrObject".to_string())));
                }
            }
        }
        let mut out = Vec::new();
        for a in args {
            match (&a.name, &a.value) {
                (None, Some(v)) => out.push(self.expr(v)?),
                (None, None) if i == Intrinsic::Array => {
                    return Err(self.err(pos, "Array cannot have a left-out element"))
                }
                (None, None)
                    if matches!(i, Intrinsic::GetObject)
                        || matches!(i, Intrinsic::Ask(_)) && !out.is_empty() =>
                {
                    out.push(CExpr {
                        st: SType::scalar(VType::Variant),
                        kind: EK::Missing,
                    })
                }
                _ => {
                    return Err(self.err(
                        pos,
                        format!("unsupported: named or left-out arguments to {i:?}"),
                    ))
                }
            }
        }
        if i == Intrinsic::IsMissing && !matches!(out[0].kind, EK::Var(VarRef::Local(_))) {
            return Err(self.err(pos, "IsMissing needs a parameter"));
        }
        Ok(CExpr {
            st: SType::scalar(intrinsic_type(i)),
            kind: EK::Intrinsic(i, out),
        })
    }

    fn call_args(&mut self, id: ProcId, args: &[ast::Arg], pos: Pos) -> R<Vec<CArg>> {
        let params = self.c.sigs[id].params.clone();
        let name = self.c.sigs[id].name.clone();
        self.call_args_for(&params, &name, args, pos)
    }

    fn call_args_for(
        &mut self,
        params: &[ParamInfo],
        name: &str,
        args: &[ast::Arg],
        pos: Pos,
    ) -> R<Vec<CArg>> {
        let mut slots: Vec<Option<&Expr>> = vec![None; params.len()];
        let mut given = vec![false; params.len()];
        let mut rest = Vec::new();
        let mut positional = 0;
        let mut named_seen = false;
        for a in args {
            match &a.name {
                Some(n) => {
                    named_seen = true;
                    let Some(i) = params.iter().position(|p| lower(&p.name) == lower(n)) else {
                        return Err(self.err(pos, format!("named argument not found: {n}")));
                    };
                    if given[i] || params[i].param_array {
                        return Err(self.err(pos, format!("argument {n} given twice")));
                    }
                    given[i] = true;
                    slots[i] = a.value.as_ref();
                }
                None => {
                    if named_seen {
                        return Err(self.err(pos, "a positional argument after a named one"));
                    }
                    if positional < params.len() && !params[positional].param_array {
                        given[positional] = true;
                        slots[positional] = a.value.as_ref();
                        positional += 1;
                    } else if params.last().is_some_and(|p| p.param_array) {
                        match &a.value {
                            Some(v) => rest.push(v),
                            None => {
                                return Err(
                                    self.err(pos, "a ParamArray argument cannot be left out")
                                )
                            }
                        }
                    } else {
                        return Err(self.err(pos, format!("wrong number of arguments to {name}")));
                    }
                }
            }
        }
        let mut out = Vec::new();
        for (i, p) in params.iter().enumerate() {
            if p.param_array {
                let mut items = Vec::new();
                for e in &rest {
                    items.push(self.expr(e)?);
                }
                out.push(CArg::Rest(items));
                continue;
            }
            let Some(e) = slots[i] else {
                if !p.optional {
                    return Err(
                        self.err(pos, format!("argument not optional: {} of {name}", p.name))
                    );
                }
                out.push(CArg::Missing);
                continue;
            };
            out.push(self.arg_for(p, e)?);
        }
        Ok(out)
    }

    fn arg_for(&mut self, p: &ParamInfo, e: &Expr) -> R<CArg> {
        let place_like = self.is_place_like(e);

        if p.array {
            if !place_like {
                return Err(self.err(
                    e.pos,
                    format!("type mismatch: {} needs an array variable", p.name),
                ));
            }
            let (place, st) = self.place(e)?;
            if !st.array
                || (st.ty != p.ty && p.ty != VType::Variant)
                || (p.ty == VType::Variant && st.ty != VType::Variant)
            {
                return Err(self.err(
                    e.pos,
                    format!("type mismatch: array argument for {}", p.name),
                ));
            }
            return Ok(CArg::Ref(place));
        }
        if !p.by_val && place_like {
            let (place, st) = self.place(e)?;
            if st.array {
                if p.ty != VType::Variant {
                    return Err(
                        self.err(e.pos, format!("ByRef argument type mismatch: {}", p.name))
                    );
                }
                return Ok(CArg::Ref(place));
            }

            let same = match (&st.ty, &p.ty) {
                (VType::Object(_), VType::Object(None)) => true,
                (VType::Object(Some(a)), VType::Object(Some(b))) => a.eq_ignore_ascii_case(b),
                (a, b) => a == b,
            };
            if p.ty != VType::Variant && !same {
                return Err(self.err(e.pos, format!("ByRef argument type mismatch: {}", p.name)));
            }
            return Ok(CArg::Ref(place));
        }
        Ok(CArg::Value(self.expr(e)?))
    }

    fn mid_statement(&mut self, target: &Expr) -> Option<(Expr, Vec<Arg>)> {
        let ExprKind::Index(callee, args) = &target.kind else {
            return None;
        };
        let ExprKind::Name(name, _) = &callee.kind else {
            return None;
        };
        if !["mid", "mid$", "midb", "midb$"].contains(&name.to_ascii_lowercase().as_str())
            || !(2..=3).contains(&args.len())
            || matches!(self.resolve(name, None, target.pos), Ok(Sym::Var(..)))
        {
            return None;
        }
        let first = args[0].value.clone()?;
        let mut out = vec![Arg {
            name: None,
            value: Some(first.clone()),
        }];
        out.extend(args[1..].iter().cloned());
        if args.len() == 2 {
            out.push(Arg {
                name: None,
                value: Some(Expr {
                    pos: target.pos,
                    kind: ExprKind::Lit(Variant::I4(i32::MAX)),
                }),
            });
        }
        Some((first, out))
    }

    fn place(&mut self, e: &Expr) -> R<(Place, SType)> {
        let pos = e.pos;
        match &e.kind {
            ExprKind::Name(name, suffix) => match self.resolve(name, *suffix, pos)? {
                Sym::Var(r, st) => Ok((Place::Var(r), st)),
                Sym::Const(..) | Sym::Value(_) => Err(self.err(pos, "assignment to a constant")),
                Sym::Err => Ok((Place::Err(ErrProp::Number), SType::scalar(VType::Long))),
                Sym::LibGlobal(n) => {
                    Ok((Place::Global(n, Vec::new()), SType::scalar(VType::Variant)))
                }
                _ => Err(self.err(pos, format!("cannot assign to {name}"))),
            },
            ExprKind::Index(target, args) => match &target.kind {
                ExprKind::Name(name, suffix) => match self.resolve(name, *suffix, pos)? {
                    Sym::Var(r, st) if st.array => {
                        let idx = self.indices(args, pos)?;
                        Ok((Place::Elem(r, idx), SType::scalar(st.ty)))
                    }
                    Sym::Var(r, st) if st.ty.is_dynamic() => {
                        let a = self.named_args(args)?;
                        if a.iter().all(|x| x.name.is_none()) {
                            let idx = a.into_iter().map(|x| x.value).collect();
                            return Ok((Place::Elem(r, idx), SType::scalar(VType::Variant)));
                        }
                        let o = CExpr {
                            st,
                            kind: EK::Var(r),
                        };
                        Ok((Place::Index(Box::new(o), a), SType::scalar(VType::Variant)))
                    }
                    Sym::LibGlobal(n) => {
                        let a = self.named_args(args)?;
                        Ok((Place::Global(n, a), SType::scalar(VType::Variant)))
                    }
                    _ => Err(self.err(pos, format!("cannot assign to {name}(...)"))),
                },
                ExprKind::Member(obj, name) => {
                    let a = self.named_args(args)?;
                    let o = self.member_object(obj.as_deref(), pos)?;
                    Ok((
                        Place::Member(Box::new(o), name.clone(), a),
                        SType::scalar(VType::Variant),
                    ))
                }
                _ => {
                    let t = self.expr(target)?;
                    let a = self.named_args(args)?;
                    Ok((Place::Index(Box::new(t), a), SType::scalar(VType::Variant)))
                }
            },
            ExprKind::Member(obj, name) => {
                if let Some(o) = obj {
                    if let ExprKind::Name(n, None) = &o.kind {
                        if !self.local_map.contains_key(&lower(n)) {
                            match self.c.lookup_module(self.mi, n) {
                                Some(Sym::Err) => {
                                    let p = match lower(name).as_str() {
                                        "number" => (ErrProp::Number, VType::Long),
                                        "description" => (ErrProp::Description, VType::String),
                                        "source" => (ErrProp::Source, VType::String),
                                        "helpfile" => (ErrProp::HelpFile, VType::String),
                                        "helpcontext" => (ErrProp::HelpContext, VType::Long),
                                        "lastdllerror" => (ErrProp::LastDllError, VType::Long),
                                        _ => {
                                            return Err(self
                                                .err(pos, format!("cannot assign to Err.{name}")))
                                        }
                                    };
                                    return Ok((Place::Err(p.0), SType::scalar(p.1)));
                                }
                                Some(Sym::Module(_)) => {
                                    let r = self.member(Some(o), name, Vec::new(), pos)?;
                                    match r.kind {
                                        EK::Var(v) | EK::AutoNew(v, _) => {
                                            return Ok((Place::Var(v), r.st))
                                        }
                                        EK::Member(o, m, a) => {
                                            return Ok((Place::Member(o, m, a), r.st))
                                        }
                                        _ => {}
                                    }
                                    return Err(
                                        self.err(pos, format!("cannot assign to {n}.{name}"))
                                    );
                                }
                                _ => {}
                            }
                        }
                    }
                }
                let o = self.member_object(obj.as_deref(), pos)?;
                Ok((
                    Place::Member(Box::new(o), name.clone(), Vec::new()),
                    SType::scalar(VType::Variant),
                ))
            }
            ExprKind::Paren(_) => Err(self.err(pos, "cannot assign to a parenthesised expression")),
            _ => Err(self.err(pos, "expected a variable or property")),
        }
    }

    fn member_object(&mut self, obj: Option<&Expr>, pos: Pos) -> R<CExpr> {
        match obj {
            None => {
                let Some(&slot) = self.with.last() else {
                    return Err(self.err(pos, "a leading . needs a With block"));
                };
                Ok(CExpr {
                    st: SType::scalar(VType::Variant),
                    kind: EK::Var(VarRef::Local(slot)),
                })
            }
            Some(o) => {
                let o = self.expr(o)?;
                if o.st.array || !o.ty().is_dynamic() {
                    return Err(self.err(pos, "invalid qualifier"));
                }
                Ok(o)
            }
        }
    }
}

fn is_file_get(e: &Expr) -> bool {
    matches!(&e.kind, ExprKind::Index(f, _) if matches!(&f.kind, ExprKind::Name(n, None) if n == "FileGet<>"))
}

fn is_print_layout(e: &Expr) -> bool {
    let is_layout = |n: &str| n.eq_ignore_ascii_case("spc") || n.eq_ignore_ascii_case("tab");
    match &e.kind {
        ExprKind::Index(f, a) => {
            matches!(&f.kind, ExprKind::Name(n, _) if is_layout(n)) && a.len() == 1
        }
        ExprKind::Name(n, None) => n.eq_ignore_ascii_case("tab"),
        _ => false,
    }
}
