use std::collections::HashMap;

use crate::ast::{
    Arg, Bounds, CaseClause, Expr, ExprKind, Module, ModuleVar, Pos, Stmt, StmtKind, TypeDecl,
    TypeName, VarDecl,
};
use crate::types::VType;
use crate::CompileError;
use xlcalc_variant::Variant;

type R<T> = Result<T, CompileError>;

enum Res {
    Udt(Place),

    Arr(String, String),

    Leaf(String, Option<Vec<Arg>>, bool),
    No,
}

#[derive(Clone)]
struct Place {
    name: String,
    ty: String,
    idx: Option<Vec<Arg>>,
}

type Sig = Vec<Option<(String, bool)>>;

struct Lower<'a> {
    module: &'a str,
    types: HashMap<String, TypeDecl>,

    vars: HashMap<String, String>,

    arrays: HashMap<String, String>,

    sigs: HashMap<String, Sig>,

    with: Vec<Option<Place>>,

    rets: HashMap<String, String>,

    pre: Vec<Stmt>,

    post: Vec<Stmt>,
    temps: u32,
}

#[derive(Default)]
pub struct Global {
    sigs: HashMap<String, Sig>,
    rets: HashMap<String, String>,
    vars: HashMap<String, String>,
    arrays: HashMap<String, String>,
}

pub fn global(modules: &[&Module], shared: &[TypeDecl]) -> Global {
    let mut g = Global::default();
    for m in modules {
        let mut types: HashMap<String, TypeDecl> = HashMap::new();
        for t in shared.iter().chain(m.types.iter()) {
            types.insert(t.name.to_ascii_lowercase(), t.clone());
        }
        let of = |v: &VarDecl| match &v.ty {
            Some(TypeName::Class(c)) => {
                let l = c.to_ascii_lowercase();
                types.contains_key(&l).then_some(l)
            }
            _ => None,
        };
        for p in &m.procs {
            let name = p.name.to_ascii_lowercase();
            let sig: Sig = p
                .params
                .iter()
                .map(|q| of(&q.var).map(|t| (t, q.var.array.is_some())))
                .collect();
            if sig.iter().any(Option::is_some) {
                g.sigs.insert(name.clone(), sig);
            }
            if p.kind.returns_value() && !p.ret_array {
                if let Some(TypeName::Class(c)) = &p.ret {
                    let l = c.to_ascii_lowercase();
                    if types.contains_key(&l) {
                        g.rets.insert(name, l);
                    }
                }
            }
        }
        for mv in &m.vars {
            if let Some(t) = of(&mv.var) {
                if mv.public {
                    let key = mv.var.name.to_ascii_lowercase();
                    if mv.var.array.is_some() {
                        g.arrays.insert(key, t);
                    } else {
                        g.vars.insert(key, t);
                    }
                }
            }
        }
    }
    g
}

pub fn lower(module: &str, m: &mut Module, shared: &[TypeDecl], g: &Global) -> R<()> {
    if m.types.is_empty() && shared.is_empty() {
        return Ok(());
    }
    let mut types: HashMap<String, TypeDecl> = HashMap::new();
    for t in shared.iter().chain(m.types.iter()) {
        types.insert(t.name.to_ascii_lowercase(), t.clone());
    }
    let mut l = Lower {
        module,
        types,
        vars: g.vars.clone(),
        arrays: g.arrays.clone(),
        sigs: g.sigs.clone(),
        with: Vec::new(),
        rets: g.rets.clone(),
        pre: Vec::new(),
        post: Vec::new(),
        temps: 0,
    };
    for p in &m.procs {
        let key = p.name.to_ascii_lowercase();
        let sig: Sig = p
            .params
            .iter()
            .map(|q| l.udt_of(&q.var).map(|t| (t, q.var.array.is_some())))
            .collect();
        if sig.iter().any(Option::is_some) {
            l.sigs.insert(key.clone(), sig);
        } else {
            l.sigs.remove(&key);
        }
        l.rets.remove(&key);
        if p.kind.returns_value() && !p.ret_array {
            if let Some(TypeName::Class(c)) = &p.ret {
                let t = c.to_ascii_lowercase();
                if l.types.contains_key(&t) {
                    l.rets.insert(key, t);
                }
            }
        }
    }
    let mut vars = Vec::new();
    for mv in std::mem::take(&mut m.vars) {
        match l.udt_of(&mv.var) {
            Some(t) => {
                l.declare(&mv.var, &t);
                for var in l.flatten_decl(&mv.var, &t) {
                    vars.push(ModuleVar {
                        public: mv.public,
                        with_events: false,
                        var,
                    });
                }
            }
            None => {
                let key = mv.var.name.to_ascii_lowercase();
                l.vars.remove(&key);
                l.arrays.remove(&key);
                vars.push(mv);
            }
        }
    }
    m.vars = vars;
    let globals = (l.vars.clone(), l.arrays.clone());
    for p in &mut m.procs {
        (l.vars, l.arrays) = globals.clone();
        let mut params = Vec::new();
        for prm in std::mem::take(&mut p.params) {
            match l.udt_of(&prm.var) {
                Some(t) => {
                    if prm.optional || prm.param_array {
                        return Err(l.err(prm.var.pos, "an optional user-defined type parameter"));
                    }
                    l.declare(&prm.var, &t);
                    for var in l.flatten_decl(&prm.var, &t) {
                        params.push(crate::ast::Param { var, ..prm.clone() });
                    }
                }
                None => {
                    let key = prm.var.name.to_ascii_lowercase();
                    l.vars.remove(&key);
                    l.arrays.remove(&key);
                    params.push(prm);
                }
            }
        }
        if let Some(TypeName::Class(c)) = &p.ret {
            let t = c.to_ascii_lowercase();
            if l.types.contains_key(&t) {
                if p.ret_array || !p.kind.returns_value() {
                    return Err(l.err(p.pos, "an array of a user-defined type as a result"));
                }

                let rv = VarDecl {
                    name: p.name.clone(),
                    pos: p.pos,
                    ty: p.ret.clone(),
                    array: None,
                    new: false,
                    fixed_len: None,
                };
                l.declare(&rv, &t);
                for var in l.flatten_decl(&rv, &t) {
                    params.push(crate::ast::Param {
                        var,
                        by_val: false,
                        optional: false,
                        param_array: false,
                        default: None,
                    });
                }
                p.ret = None;
            }
        }
        p.params = params;
        p.body = l.block(std::mem::take(&mut p.body))?;
    }
    Ok(())
}

impl Lower<'_> {
    fn err(&self, pos: Pos, what: &str) -> CompileError {
        CompileError {
            module: self.module.to_string(),
            line: pos.line,
            col: pos.col,
            message: format!("unsupported: {what}"),
        }
    }

    fn udt_of(&self, v: &VarDecl) -> Option<String> {
        let Some(TypeName::Class(c)) = &v.ty else {
            return None;
        };
        let l = c.to_ascii_lowercase();
        self.types.contains_key(&l).then_some(l)
    }

    fn declare(&mut self, v: &VarDecl, t: &str) {
        let key = v.name.to_ascii_lowercase();
        if v.array.is_some() {
            self.vars.remove(&key);
            self.arrays.insert(key, t.to_string());
        } else {
            self.arrays.remove(&key);
            self.vars.insert(key, t.to_string());
        }
    }

    fn flatten_decl(&self, v: &VarDecl, ty: &str) -> Vec<VarDecl> {
        let mut out = self.flatten(&v.name, ty, v.pos);
        if let Some(elem) = &v.array {
            for leaf in &mut out {
                leaf.array = Some(match (&leaf.array, elem.is_empty()) {
                    (_, true) => Vec::new(),
                    (Some(field), false) => field.iter().chain(elem).cloned().collect(),
                    (None, false) => elem.clone(),
                });
            }
        }
        out
    }

    fn field_type(&self, f: &VarDecl) -> Option<String> {
        let Some(TypeName::Class(c)) = &f.ty else {
            return None;
        };
        let l = c.to_ascii_lowercase();
        (self.types.contains_key(&l) && f.array.is_none()).then_some(l)
    }

    fn flatten(&self, name: &str, ty: &str, pos: Pos) -> Vec<VarDecl> {
        let mut out = Vec::new();
        for f in &self.types[ty].fields {
            let flat = format!("{name}.{}", f.name);
            match self.field_type(f) {
                Some(t) => out.extend(self.flatten(&flat, &t, pos)),
                None => out.push(VarDecl {
                    name: flat,
                    pos,
                    ..f.clone()
                }),
            }
        }
        out
    }

    fn field(&self, at: &Place, field: &str, pos: Pos) -> R<Res> {
        let Some(f) = self.types[&at.ty]
            .fields
            .iter()
            .find(|f| f.name.eq_ignore_ascii_case(field))
        else {
            return Err(self.err(pos, &format!("member '{field}' of a user-defined type")));
        };
        let flat = format!("{}.{}", at.name, f.name);
        Ok(match self.field_type(f) {
            Some(t) => Res::Udt(Place {
                name: flat,
                ty: t,
                idx: at.idx.clone(),
            }),
            None => Res::Leaf(flat, at.idx.clone(), f.array.is_some()),
        })
    }

    fn resolve(&self, e: &Expr) -> R<Res> {
        Ok(match &e.kind {
            ExprKind::Name(n, _) => {
                let key = n.to_ascii_lowercase();
                if let Some(t) = self.vars.get(&key) {
                    Res::Udt(Place {
                        name: n.clone(),
                        ty: t.clone(),
                        idx: None,
                    })
                } else if let Some(t) = self.arrays.get(&key) {
                    Res::Arr(n.clone(), t.clone())
                } else {
                    Res::No
                }
            }
            ExprKind::Index(callee, args) => match &callee.kind {
                ExprKind::Name(n, _) => match self.arrays.get(&n.to_ascii_lowercase()) {
                    Some(t) => Res::Udt(Place {
                        name: n.clone(),
                        ty: t.clone(),
                        idx: Some(args.clone()),
                    }),
                    None => Res::No,
                },
                _ => Res::No,
            },
            ExprKind::Member(None, f) => match self.with.last() {
                Some(Some(at)) => self.field(at, f, e.pos)?,
                _ => Res::No,
            },
            ExprKind::Member(Some(inner), f) => match self.resolve(inner)? {
                Res::Udt(at) => self.field(&at, f, e.pos)?,
                _ => Res::No,
            },
            _ => Res::No,
        })
    }

    fn leaf_expr(pos: Pos, flat: &str, field: &[i64], idx: &Option<Vec<Arg>>) -> Expr {
        let base = Expr {
            pos,
            kind: ExprKind::Name(flat.to_string(), None),
        };
        let mut args: Vec<Arg> = field
            .iter()
            .map(|&i| Arg {
                name: None,
                value: Some(Expr {
                    pos,
                    kind: ExprKind::Lit(Variant::I4(i as i32)),
                }),
            })
            .collect();
        args.extend(idx.iter().flatten().cloned());
        if args.is_empty() {
            return base;
        }
        Expr {
            pos,
            kind: ExprKind::Index(Box::new(base), args),
        }
    }

    fn field_size(&self, f: &VarDecl, bytes: bool) -> Option<(i64, i64)> {
        let literal = |e: &Expr| match &e.kind {
            ExprKind::Lit(Variant::I2(n)) => Some(i64::from(*n)),
            ExprKind::Lit(Variant::I4(n)) => Some(i64::from(*n)),
            _ => None,
        };
        let (one, align) = match &f.ty {
            Some(TypeName::Class(c)) => {
                let t = c.to_ascii_lowercase();
                self.types.contains_key(&t).then_some(())?;
                let (size, align) = self.type_size(&t, bytes)?;
                (size, align)
            }
            Some(TypeName::Builtin(VType::String)) => match &f.fixed_len {
                Some(n) => (literal(n)? * if bytes { 2 } else { 1 }, 1),
                None => (8, 8),
            },
            Some(TypeName::Builtin(VType::Byte)) => (1, 1),
            Some(TypeName::Builtin(VType::Integer | VType::Boolean)) => (2, 2),
            Some(TypeName::Builtin(VType::Long | VType::Single)) => (4, 4),
            Some(TypeName::Builtin(
                VType::Double | VType::Currency | VType::Date | VType::LongLong,
            )) => (8, 8),
            None | Some(TypeName::Builtin(VType::Variant)) => (24, 8),
            _ => return None,
        };
        let count = match &f.array {
            None => 1,
            Some(bs) if bs.is_empty() => return None,
            Some(bs) => {
                let mut n = 1;
                for b in bs {
                    let lo = b.lower.as_ref().map_or(Some(0), literal)?;
                    n *= literal(&b.upper)? - lo + 1;
                }
                n
            }
        };
        Some((one * count, align))
    }

    fn type_size(&self, ty: &str, bytes: bool) -> Option<(i64, i64)> {
        let (mut at, mut max) = (0, 1);
        for f in &self.types[ty].fields {
            let (size, align) = self.field_size(f, bytes)?;
            if bytes {
                at = (at + align - 1) / align * align;
            }
            at += size;
            max = max.max(align);
        }
        Some(if bytes {
            ((at + max - 1) / max * max, max)
        } else {
            (at, max)
        })
    }

    fn expr(&mut self, e: &mut Expr) -> R<()> {
        if let ExprKind::Index(t, args) = &e.kind {
            if let (ExprKind::Name(n, _), [a]) = (&t.kind, args.as_slice()) {
                let bytes = n.eq_ignore_ascii_case("lenb");
                if bytes || n.eq_ignore_ascii_case("len") {
                    if let Some(Res::Udt(at)) =
                        a.value.as_ref().map(|v| self.resolve(v)).transpose()?
                    {
                        if let Some((size, _)) = self.type_size(&at.ty, bytes) {
                            e.kind = ExprKind::Lit(Variant::I4(size as i32));
                            return Ok(());
                        }
                    }
                }
            }

            if let Res::Leaf(flat, Some(idx), true) = self.resolve(t)? {
                let mut args = args.clone();
                self.args(&mut args)?;
                let mut idx = idx;
                self.args(&mut idx)?;
                args.extend(idx);
                e.kind = ExprKind::Index(
                    Box::new(Expr {
                        pos: e.pos,
                        kind: ExprKind::Name(flat, None),
                    }),
                    args,
                );
                return Ok(());
            }
            if let ExprKind::Member(obj, n) = &t.kind {
                let key = n.to_ascii_lowercase();
                if self.sigs.contains_key(&key) && matches!(self.resolve(t)?, Res::No) {
                    let obj = obj.is_some();
                    let ExprKind::Index(callee, args) = &mut e.kind else {
                        return Ok(());
                    };
                    if obj {
                        if let ExprKind::Member(Some(o), _) = &mut callee.kind {
                            self.expr(o)?;
                        }
                    }
                    return self.call_args(&key, args);
                }
            }
            if let ExprKind::Name(n, _) = &t.kind {
                let key = n.to_ascii_lowercase();
                if self.sigs.contains_key(&key) {
                    let ExprKind::Index(_, args) = &mut e.kind else {
                        return Ok(());
                    };
                    return self.call_args(&key, args);
                }
                if key == "lbound" || key == "ubound" {
                    let ExprKind::Index(_, args) = &mut e.kind else {
                        return Ok(());
                    };
                    self.bound_args(args)?;
                    return self.args(args);
                }
            }
        }
        match self.resolve(e)? {
            Res::Leaf(n, idx, array) => {
                if idx.is_some() && array {
                    return Err(self.err(e.pos, "an array field of an array element as a value"));
                }
                let mut idx = idx;
                if let Some(ix) = &mut idx {
                    self.args(ix)?;
                }
                *e = Self::leaf_expr(e.pos, &n, &[], &idx);
                return Ok(());
            }
            Res::Udt(..) => return Err(self.err(e.pos, "a user-defined type used as a value")),
            Res::Arr(..) => {
                return Err(self.err(e.pos, "an array of a user-defined type as a value"))
            }
            Res::No => {}
        }
        match &mut e.kind {
            ExprKind::Member(Some(i), _)
            | ExprKind::Paren(i)
            | ExprKind::Unary(_, i)
            | ExprKind::TypeOf(i, _) => self.expr(i)?,
            ExprKind::Index(t, args) => {
                self.expr(t)?;
                self.args(args)?;
            }
            ExprKind::Binary(_, a, b) => {
                self.expr(a)?;
                self.expr(b)?;
            }
            _ => {}
        }
        Ok(())
    }

    fn bound_args(&mut self, args: &mut Vec<Arg>) -> R<()> {
        let Some(first) = args.first().and_then(|a| a.value.as_ref()) else {
            return Ok(());
        };
        let Res::Arr(name, ty) = self.resolve(first)? else {
            return Ok(());
        };
        let leaves = self.flatten(&name, &ty, Pos::default());
        let Some(leaf) = leaves
            .iter()
            .min_by_key(|l| l.array.as_ref().map_or(0, Vec::len))
        else {
            return Ok(());
        };
        let field = self
            .flatten(&name, &ty, Pos::default())
            .iter()
            .find(|l| l.name == leaf.name)
            .and_then(|l| l.array.as_ref().map(Vec::len))
            .unwrap_or(0);
        let pos = first.pos;
        let name = leaf.name.clone();
        let own = self.types.get(&ty).map(|_| field).unwrap_or(0);
        args[0].value = Some(Expr {
            pos,
            kind: ExprKind::Name(name, None),
        });
        if own > 0 {
            match args.get_mut(1).and_then(|a| a.value.as_mut()) {
                None => args.push(Arg {
                    name: None,
                    value: Some(Expr {
                        pos,
                        kind: ExprKind::Lit(Variant::I4(own as i32 + 1)),
                    }),
                }),
                Some(Expr {
                    kind: ExprKind::Lit(Variant::I2(n)),
                    ..
                }) => *n += own as i16,
                Some(Expr {
                    kind: ExprKind::Lit(Variant::I4(n)),
                    ..
                }) => *n += own as i32,
                Some(_) => return Err(self.err(pos, "a computed dimension of an array of a type")),
            }
        }
        Ok(())
    }

    fn call_args(&mut self, proc_name: &str, args: &mut Vec<Arg>) -> R<()> {
        let sig = self.sigs[proc_name].clone();
        let mut out = Vec::new();
        for (k, a) in std::mem::take(args).into_iter().enumerate() {
            let Some(Some((ty, _))) = sig.get(k) else {
                let mut a = a;
                if let Some(v) = &mut a.value {
                    self.expr(v)?;
                }
                out.push(a);
                continue;
            };
            let mut a = a;
            if let Some(v) = &mut a.value {
                self.hoist(v)?;
            }
            let Some(v) = &a.value else {
                return Err(self.err(Pos::default(), "a left-out user-defined type argument"));
            };
            let pos = v.pos;
            let (name, idx, whole) = match self.resolve(v)? {
                Res::Udt(at) if at.ty == *ty => (at.name, at.idx, false),
                Res::Arr(n, t) if t == *ty => (n, None, true),
                _ => return Err(self.err(pos, "an argument that is not of the parameter's type")),
            };
            let mut idx = idx;
            if let Some(ix) = &mut idx {
                self.args(ix)?;
            }
            let (mut name, mut idx) = (name, idx);
            if idx.is_some()
                && self
                    .flatten(&name, ty, pos)
                    .iter()
                    .any(|l| l.array.is_some())
            {
                self.temps += 1;
                let tmp = format!("__t{}", self.temps);
                let decl = VarDecl {
                    name: tmp.clone(),
                    pos,
                    ty: Some(TypeName::Class(self.types[ty].name.clone())),
                    array: None,
                    new: false,
                    fixed_len: None,
                };
                self.declare(&decl, ty);
                let vars = self.flatten_decl(&decl, ty);
                self.pre.push(Stmt {
                    pos,
                    kind: StmtKind::Dim {
                        is_static: false,
                        vars,
                    },
                });
                let tidx = None;
                let cin = self.copy_udt(pos, ty, (&tmp, &tidx), (&name, &idx))?;
                self.pre.extend(cin);
                let cout = self.copy_udt(pos, ty, (&name, &idx), (&tmp, &tidx))?;
                self.post.extend(cout);
                name = tmp;
                idx = None;
            }
            let _ = whole;
            for leaf in self.flatten(&name, ty, pos) {
                out.push(Arg {
                    name: None,
                    value: Some(Self::leaf_expr(pos, &leaf.name, &[], &idx)),
                });
            }
        }
        *args = out;
        Ok(())
    }

    fn copy_udt(
        &self,
        pos: Pos,
        ty: &str,
        (tn, tidx): (&str, &Option<Vec<Arg>>),
        (vn, vidx): (&str, &Option<Vec<Arg>>),
    ) -> R<Vec<Stmt>> {
        let lits = |e: &Expr| match &e.kind {
            ExprKind::Lit(Variant::I2(n)) => Some(i64::from(*n)),
            ExprKind::Lit(Variant::I4(n)) => Some(i64::from(*n)),
            _ => None,
        };
        let mut out = Vec::new();
        let names = self.leaves(vn, ty);
        for (d, b) in self.flatten(tn, ty, pos).into_iter().zip(names) {
            let mut idx: Vec<Vec<i64>> = vec![Vec::new()];
            if let Some(bs) = &d.array {
                for bd in bs {
                    let (Some(lo), Some(hi)) =
                        (bd.lower.as_ref().map_or(Some(0), lits), lits(&bd.upper))
                    else {
                        return Err(self.err(pos, "copying a dynamic array field"));
                    };
                    let mut next = Vec::new();
                    for pre in &idx {
                        for i in lo..=hi {
                            let mut v = pre.clone();
                            v.push(i);
                            next.push(v);
                        }
                    }
                    idx = next;
                }
            }
            for ix in idx {
                out.push(Stmt {
                    pos,
                    kind: StmtKind::Assign {
                        target: Self::leaf_expr(pos, &d.name, &ix, tidx),
                        value: Self::leaf_expr(pos, &b, &ix, vidx),
                        set: false,
                    },
                });
            }
        }
        Ok(out)
    }

    fn lset_text(&mut self, pos: Pos, dst: &str, dty: &str, src: &str, sty: &str) -> R<String> {
        let literal = |e: &Expr| match &e.kind {
            ExprKind::Lit(Variant::I2(n)) => Some(i64::from(*n)),
            ExprKind::Lit(Variant::I4(n)) => Some(i64::from(*n)),
            _ => None,
        };
        self.temps += 1;
        let n = self.temps;
        let (bs, bd, kk, cc, ss) = (
            format!("zzls{n}"),
            format!("zzld{n}"),
            format!("zzlk{n}"),
            format!("zzlc{n}"),
            format!("zzln{n}"),
        );
        let ssz = self.type_size(sty, true);
        let dsz = self.type_size(dty, true);
        let (Some((ssz, _)), Some((dsz, _))) = (ssz, dsz) else {
            return Err(self.err(pos, "LSet of types of unknown size"));
        };
        let pack = |ty: &str, var: &str, buf: &str, text: &mut String, un: bool| -> R<()> {
            let mut at = 0i64;
            for f in &self.types[ty].fields {
                let Some((size, align)) = self.field_size(f, true) else {
                    return Err(self.err(pos, "LSet of a field of unknown size"));
                };
                at = (at + align - 1) / align * align;
                let count = match &f.array {
                    None => 1,
                    Some(bs) => {
                        let mut c = 1;
                        for b in bs {
                            let lo = b.lower.as_ref().map_or(Some(0), literal).unwrap_or(0);
                            c *= literal(&b.upper).unwrap_or(0) - lo + 1;
                        }
                        c
                    }
                };
                let one = size / count;

                let mut combos: Vec<Vec<i64>> = vec![Vec::new()];
                if let Some(bs) = &f.array {
                    for b in bs {
                        let lo = b.lower.as_ref().map_or(Some(0), literal).unwrap_or(0);
                        let hi = literal(&b.upper).unwrap_or(0);
                        let mut next = Vec::new();
                        for i in lo..=hi {
                            for p in &combos {
                                let mut v = p.clone();
                                v.push(i);
                                next.push(v);
                            }
                        }
                        combos = next;
                    }
                }
                for ix in combos {
                    let mut e = format!("{var}.{}", f.name);
                    if !ix.is_empty() {
                        let l: Vec<String> = ix.iter().map(i64::to_string).collect();
                        e = format!("{e}({})", l.join(", "));
                    }
                    let b = |k: i64| format!("{buf}({})", at + k);
                    match (&f.ty, &f.fixed_len) {
                        (Some(TypeName::Builtin(VType::Byte)), _) => {
                            if un {
                                text.push_str(&format!("{e} = {}\n", b(0)));
                            } else {
                                text.push_str(&format!("{} = {e}\n", b(0)));
                            }
                        }
                        (Some(TypeName::Builtin(VType::Integer)), _) => {
                            if un {
                                text.push_str(&format!("{cc} = {} + {} * 256&\nIf {cc} > 32767 Then {cc} = {cc} - 65536\n{e} = {cc}\n", b(0), b(1)));
                            } else {
                                text.push_str(&format!("{cc} = {e}\n{} = {cc} And &HFF&\n{} = (({cc} And &HFF00&) \\ &H100&) And &HFF&\n", b(0), b(1)));
                            }
                        }
                        (Some(TypeName::Builtin(VType::Long)), _) => {
                            if un {
                                text.push_str(&format!("{cc} = {} + {} * 256& + {} * 65536 + ({} And &H7F&) * 16777216\nIf {} > 127 Then {cc} = {cc} Or &H80000000\n{e} = {cc}\n", b(0), b(1), b(2), b(3), b(3)));
                            } else {
                                text.push_str(&format!("{cc} = {e}\n{} = {cc} And &HFF&\n{} = ({cc} And &HFF00&) \\ &H100&\n{} = ({cc} And &HFF0000) \\ &H10000\n{} = (({cc} And &HFF000000) \\ &H1000000) And &HFF&\n", b(0), b(1), b(2), b(3)));
                            }
                        }
                        (Some(TypeName::Builtin(VType::String)), Some(_)) => {
                            let chars = one / 2;
                            if un {
                                text.push_str(&format!("{ss} = \"\"\n"));
                            }
                            for c in 0..chars {
                                if un {
                                    text.push_str(&format!(
                                        "{cc} = {} + {} * 256&\n{ss} = {ss} & ChrW({cc})\n",
                                        b(2 * c),
                                        b(2 * c + 1)
                                    ));
                                } else {
                                    text.push_str(&format!("{cc} = AscW(Mid$({e}, {}, 1))\n{} = {cc} And &HFF&\n{} = (({cc} And &HFF00&) \\ &H100&) And &HFF&\n", c + 1, b(2 * c), b(2 * c + 1)));
                                }
                            }
                            if un {
                                text.push_str(&format!("{e} = {ss}\n"));
                            }
                        }
                        _ => return Err(self.err(pos, "LSet of a field of this type")),
                    }
                    at += one;
                }
            }
            Ok(())
        };
        let mut text = format!(
            "Sub lsetx()\nDim {bs}(0 To {}) As Byte, {bd}(0 To {}) As Byte, {kk} As Long, {cc} As Long, {ss} As String\n",
            ssz - 1,
            dsz - 1
        );
        pack(sty, src, &bs, &mut text, false)?;
        pack(dty, dst, &bd, &mut text, false)?;
        text.push_str(&format!(
            "For {kk} = 0 To {}\n{bd}({kk}) = {bs}({kk})\nNext {kk}\n",
            ssz.min(dsz) - 1
        ));
        pack(dty, dst, &bd, &mut text, true)?;
        text.push_str("End Sub\n");
        Ok(text)
    }

    fn args(&mut self, args: &mut [Arg]) -> R<()> {
        for a in args {
            if let Some(v) = &mut a.value {
                self.expr(v)?;
            }
        }
        Ok(())
    }

    fn bounds(&mut self, b: &mut Bounds) -> R<()> {
        if let Some(l) = &mut b.lower {
            self.expr(l)?;
        }
        self.expr(&mut b.upper)
    }

    fn leaves(&self, name: &str, ty: &str) -> Vec<String> {
        self.flatten(name, ty, Pos::default())
            .into_iter()
            .map(|v| v.name)
            .collect()
    }

    fn block(&mut self, body: Vec<Stmt>) -> R<Vec<Stmt>> {
        let mut out = Vec::new();
        for s in body {
            self.stmt(s, &mut out)?;
        }
        Ok(out)
    }

    fn stmt(&mut self, s: Stmt, out: &mut Vec<Stmt>) -> R<()> {
        let outer = std::mem::take(&mut self.pre);
        let outer_post = std::mem::take(&mut self.post);
        let mut lowered = Vec::new();
        let r = self.stmt_inner(s, &mut lowered);
        out.extend(std::mem::replace(&mut self.pre, outer));
        out.extend(lowered);
        out.extend(std::mem::replace(&mut self.post, outer_post));
        r
    }

    #[allow(clippy::type_complexity)]
    fn ret_call(&self, e: &Expr) -> R<Option<(Option<Expr>, String, Vec<Arg>, String)>> {
        let local = |n: &str| {
            let k = n.to_ascii_lowercase();
            self.vars.contains_key(&k) || self.arrays.contains_key(&k)
        };
        let (callee, args) = match &e.kind {
            ExprKind::Index(c, a) => (&**c, a.clone()),
            _ => (e, Vec::new()),
        };
        Ok(match &callee.kind {
            ExprKind::Name(n, _) if !local(n) => self
                .rets
                .get(&n.to_ascii_lowercase())
                .map(|t| (None, n.clone(), args, t.clone())),
            ExprKind::Member(obj, n) if self.rets.contains_key(&n.to_ascii_lowercase()) => {
                if !matches!(self.resolve(callee)?, Res::No) {
                    return Ok(None);
                }
                let t = self.rets[&n.to_ascii_lowercase()].clone();
                Some((obj.as_deref().cloned(), n.clone(), args, t))
            }
            _ => None,
        })
    }

    fn hoist(&mut self, e: &mut Expr) -> R<()> {
        let Some((obj, name, mut args, ty)) = self.ret_call(e)? else {
            return Ok(());
        };
        let pos = e.pos;
        self.temps += 1;
        let tmp = format!("__t{}", self.temps);
        let decl = VarDecl {
            name: tmp.clone(),
            pos,
            ty: Some(TypeName::Class(self.types[&ty].name.clone())),
            array: None,
            new: false,
            fixed_len: None,
        };
        let mut obj = obj;
        if let Some(o) = &mut obj {
            self.expr(o)?;
        }
        let key = name.to_ascii_lowercase();
        if self.sigs.contains_key(&key) {
            self.call_args(&key, &mut args)?;
        } else {
            self.args(&mut args)?;
        }
        let leaves = self.flatten_decl(&decl, &ty);
        for leaf in &leaves {
            args.push(Arg {
                name: None,
                value: Some(Self::leaf_expr(pos, &leaf.name, &[], &None)),
            });
        }
        let dummy = format!("__d{}", self.temps);
        let mut vars = leaves;
        vars.push(VarDecl {
            name: dummy.clone(),
            pos,
            ty: None,
            array: None,
            new: false,
            fixed_len: None,
        });
        self.declare(&decl, &ty);
        self.pre.push(Stmt {
            pos,
            kind: StmtKind::Dim {
                is_static: false,
                vars,
            },
        });
        let callee = match obj {
            Some(o) => Expr {
                pos,
                kind: ExprKind::Member(Some(Box::new(o)), name),
            },
            None => match &e.kind {
                ExprKind::Member(None, _) => Expr {
                    pos,
                    kind: ExprKind::Member(None, name),
                },
                _ => Expr {
                    pos,
                    kind: ExprKind::Name(name, None),
                },
            },
        };
        self.pre.push(Stmt {
            pos,
            kind: StmtKind::Assign {
                target: Expr {
                    pos,
                    kind: ExprKind::Name(dummy, None),
                },
                value: Expr {
                    pos,
                    kind: ExprKind::Index(Box::new(callee), args),
                },
                set: false,
            },
        });
        *e = Expr {
            pos,
            kind: ExprKind::Name(tmp, None),
        };
        Ok(())
    }

    fn elems(&self, name: &str, ty: &str, pos: Pos, idx: &Option<Vec<Arg>>) -> R<Vec<(Expr, u8)>> {
        let lits = |e: &Expr| match &e.kind {
            ExprKind::Lit(Variant::I2(n)) => Some(i64::from(*n)),
            ExprKind::Lit(Variant::I4(n)) => Some(i64::from(*n)),
            _ => None,
        };
        let mut out = Vec::new();
        for d in self.flatten(name, ty, pos) {
            let is_str = matches!(d.ty, Some(TypeName::Builtin(VType::String)));
            let varlen: u8 = match (is_str, d.fixed_len.is_some()) {
                (true, false) => 16,
                (true, true) => 32,
                _ => 0,
            };
            let mut combos: Vec<Vec<i64>> = vec![Vec::new()];
            if let Some(bs) = &d.array {
                for bd in bs {
                    let (Some(lo), Some(hi)) =
                        (bd.lower.as_ref().map_or(Some(0), lits), lits(&bd.upper))
                    else {
                        return Err(self.err(pos, "a dynamic array field in file I/O"));
                    };
                    let mut next = Vec::new();
                    for i in lo..=hi {
                        for pre in &combos {
                            let mut v = pre.clone();
                            v.push(i);
                            next.push(v);
                        }
                    }
                    combos = next;
                }
            }
            for ix in combos {
                out.push((Self::leaf_expr(pos, &d.name, &ix, idx), varlen));
            }
        }
        Ok(out)
    }

    fn file_udt(&mut self, put: bool, args: &mut [Arg], pos: Pos, out: &mut Vec<Stmt>) -> R<bool> {
        let [num, rec, var] = args else {
            return Ok(false);
        };
        let (Some(num), Some(rec), Some(var)) = (&num.value, &rec.value, &var.value) else {
            return Ok(false);
        };
        let Res::Udt(mut at) = self.resolve(var)? else {
            return Ok(false);
        };
        if let Some(ix) = &mut at.idx {
            self.args(ix)?;
        }
        let name_of = |n: &str| Expr {
            pos,
            kind: ExprKind::Name(n.to_string(), None),
        };
        let func = |n: &str, a: Vec<Expr>| Expr {
            pos,
            kind: ExprKind::Index(
                Box::new(name_of(n)),
                a.into_iter()
                    .map(|v| Arg {
                        name: None,
                        value: Some(v),
                    })
                    .collect(),
            ),
        };
        let elems = self.elems(&at.name, &at.ty, pos, &at.idx)?;
        if !matches!(rec.kind, ExprKind::Lit(Variant::Empty)) {
            out.push(Stmt {
                pos,
                kind: StmtKind::Call {
                    callee: name_of("FileSeek<>"),
                    args: vec![
                        Arg {
                            name: None,
                            value: Some(num.clone()),
                        },
                        Arg {
                            name: None,
                            value: Some(rec.clone()),
                        },
                    ],
                },
            });
        }
        let n = elems.len();
        for (k, (e, varlen)) in elems.into_iter().enumerate() {
            let kind: i32 = match (n, k) {
                (1, _) => 4,
                (_, 0) => 1,
                (_, k) if k == n - 1 => 3,
                _ => 2,
            };
            let piece = Expr {
                pos,
                kind: ExprKind::Lit(Variant::Ui1((kind + i32::from(varlen)) as u8)),
            };
            if put {
                out.push(Stmt {
                    pos,
                    kind: StmtKind::Call {
                        callee: name_of("FilePut<>"),
                        args: vec![
                            Arg {
                                name: None,
                                value: Some(num.clone()),
                            },
                            Arg {
                                name: None,
                                value: Some(piece),
                            },
                            Arg {
                                name: None,
                                value: Some(e),
                            },
                        ],
                    },
                });
            } else {
                out.push(Stmt {
                    pos,
                    kind: StmtKind::Assign {
                        target: e.clone(),
                        value: func("FileGet<>", vec![num.clone(), piece, e]),
                        set: false,
                    },
                });
            }
        }
        Ok(true)
    }

    fn stmt_inner(&mut self, mut s: Stmt, out: &mut Vec<Stmt>) -> R<()> {
        let pos = s.pos;
        match &mut s.kind {
            StmtKind::Dim { vars, .. } => {
                let mut flat = Vec::new();
                for v in std::mem::take(vars) {
                    let key = v.name.to_ascii_lowercase();
                    match self.udt_of(&v) {
                        Some(t) => {
                            flat.extend(self.flatten_decl(&v, &t));
                            self.declare(&v, &t);
                        }
                        None => {
                            self.vars.remove(&key);
                            self.arrays.remove(&key);
                            flat.push(v);
                        }
                    }
                }
                *vars = flat;
            }
            StmtKind::Const(_)
            | StmtKind::Exit(_)
            | StmtKind::GoTo(_)
            | StmtKind::Label(_)
            | StmtKind::OnError(_)
            | StmtKind::Resume(_)
            | StmtKind::End
            | StmtKind::Stop
            | StmtKind::GoSub(_)
            | StmtKind::Return => {}
            StmtKind::ReDim { items, .. } => {
                let mut out_items = Vec::new();
                for (mut e, mut bounds, ty) in std::mem::take(items) {
                    for b in &mut bounds {
                        self.bounds(b)?;
                    }
                    if let Res::Arr(name, t) = self.resolve(&e)? {
                        for leaf in self.flatten(&name, &t, e.pos) {
                            let mut all = leaf.array.clone().unwrap_or_default();
                            if all.iter().any(|b| b.lower.is_none() && !is_lit(&b.upper)) {
                                return Err(self.err(pos, "a dynamic array field"));
                            }
                            all.extend(bounds.iter().cloned());
                            out_items.push((
                                Expr {
                                    pos: e.pos,
                                    kind: ExprKind::Name(leaf.name, None),
                                },
                                all,
                                None,
                            ));
                        }
                        continue;
                    }
                    self.expr(&mut e)?;
                    out_items.push((e, bounds, ty));
                }
                *items = out_items;
            }
            StmtKind::Erase(es) => {
                let mut out_es = Vec::new();
                for mut e in std::mem::take(es) {
                    if let Res::Arr(name, t) = self.resolve(&e)? {
                        for leaf in self.flatten(&name, &t, e.pos) {
                            out_es.push(Expr {
                                pos: e.pos,
                                kind: ExprKind::Name(leaf.name, None),
                            });
                        }
                        continue;
                    }
                    self.expr(&mut e)?;
                    out_es.push(e);
                }
                *es = out_es;
            }
            StmtKind::Assign { target, value, .. } => {
                if let ExprKind::Index(c, a) = &mut value.kind {
                    if matches!(&c.kind, ExprKind::Name(n, _) if n == "FileGet<>")
                        && self.file_udt(false, a, pos, out)?
                    {
                        return Ok(());
                    }
                }
                self.hoist(value)?;
                if let (Res::No, Res::Udt(vp)) = (self.resolve(target)?, self.resolve(value)?) {
                    let (obj, name) = match &target.kind {
                        ExprKind::Member(obj, n) => (obj.clone(), n.clone()),
                        _ => return Err(self.err(pos, "assigning a user-defined type")),
                    };
                    let mut leaves = self.leaves(&vp.name, &vp.ty);
                    let Some(last) = leaves.pop() else {
                        return Ok(());
                    };
                    let mut obj = obj;
                    if let Some(o) = &mut obj {
                        self.expr(o)?;
                    }
                    let member = Expr {
                        pos,
                        kind: ExprKind::Member(obj, name),
                    };
                    let tgt = if leaves.is_empty() {
                        member
                    } else {
                        Expr {
                            pos,
                            kind: ExprKind::Index(
                                Box::new(member),
                                leaves
                                    .iter()
                                    .map(|l| Arg {
                                        name: None,
                                        value: Some(Self::leaf_expr(pos, l, &[], &vp.idx)),
                                    })
                                    .collect(),
                            ),
                        }
                    };
                    out.push(Stmt {
                        pos,
                        kind: StmtKind::Assign {
                            target: tgt,
                            value: Self::leaf_expr(pos, &last, &[], &vp.idx),
                            set: false,
                        },
                    });
                    return Ok(());
                }
                if let Res::Udt(mut tp) = self.resolve(target)? {
                    let Res::Udt(mut vp) = self.resolve(value)? else {
                        return Err(self.err(pos, "assigning a user-defined type from a value"));
                    };
                    if vp.ty != tp.ty {
                        return Err(self.err(pos, "assigning between different types"));
                    }
                    for ix in tp.idx.iter_mut().chain(vp.idx.iter_mut()) {
                        self.args(ix)?;
                    }
                    let (tn, tt, vn) = (tp.name.clone(), tp.ty.clone(), vp.name.clone());
                    let lits = |e: &Expr| match &e.kind {
                        ExprKind::Lit(Variant::I2(n)) => Some(i64::from(*n)),
                        ExprKind::Lit(Variant::I4(n)) => Some(i64::from(*n)),
                        _ => None,
                    };
                    let decls = self.flatten(&tn, &tt, pos);
                    let names = self.leaves(&vn, &tt);
                    for (d, b) in decls.into_iter().zip(names) {
                        let mut idx: Vec<Vec<i64>> = vec![Vec::new()];
                        if let Some(bs) = &d.array {
                            for bd in bs {
                                let (Some(lo), Some(hi)) =
                                    (bd.lower.as_ref().map_or(Some(0), lits), lits(&bd.upper))
                                else {
                                    return Err(self.err(pos, "copying a dynamic array field"));
                                };
                                let mut next = Vec::new();
                                for pre in &idx {
                                    for i in lo..=hi {
                                        let mut v = pre.clone();
                                        v.push(i);
                                        next.push(v);
                                    }
                                }
                                idx = next;
                            }
                        }
                        for ix in idx {
                            out.push(Stmt {
                                pos,
                                kind: StmtKind::Assign {
                                    target: Self::leaf_expr(pos, &d.name, &ix, &tp.idx),
                                    value: Self::leaf_expr(pos, &b, &ix, &vp.idx),
                                    set: false,
                                },
                            });
                        }
                    }
                    return Ok(());
                }
                self.expr(target)?;
                self.expr(value)?;
            }
            StmtKind::Call { callee, args } => {
                if matches!(&callee.kind, ExprKind::Name(n, _) if n == "FilePut<>")
                    && self.file_udt(true, args, pos, out)?
                {
                    return Ok(());
                }
                let key = match &callee.kind {
                    ExprKind::Name(n, _) | ExprKind::Member(_, n) => Some(n.to_ascii_lowercase()),
                    _ => None,
                };
                match key {
                    Some(k) if self.sigs.contains_key(&k) => {
                        if let ExprKind::Member(Some(o), _) = &mut callee.kind {
                            self.expr(o)?;
                        }
                        self.call_args(&k, args)?
                    }
                    _ => {
                        self.expr(callee)?;
                        self.args(args)?;
                    }
                }
            }
            StmtKind::If { arms, otherwise } => {
                for (c, b) in arms {
                    self.expr(c)?;
                    *b = self.block(std::mem::take(b))?;
                }
                if let Some(b) = otherwise {
                    *b = self.block(std::mem::take(b))?;
                }
            }
            StmtKind::Select {
                subject,
                cases,
                otherwise,
            } => {
                self.expr(subject)?;
                for (clauses, b) in cases {
                    for c in clauses {
                        match c {
                            CaseClause::Value(e) | CaseClause::Is(_, e) => self.expr(e)?,
                            CaseClause::Range(a, b) => {
                                self.expr(a)?;
                                self.expr(b)?;
                            }
                        }
                    }
                    *b = self.block(std::mem::take(b))?;
                }
                if let Some(b) = otherwise {
                    *b = self.block(std::mem::take(b))?;
                }
            }
            StmtKind::For {
                var,
                from,
                to,
                step,
                body,
            } => {
                self.expr(var)?;
                self.expr(from)?;
                self.expr(to)?;
                if let Some(e) = step {
                    self.expr(e)?;
                }
                *body = self.block(std::mem::take(body))?;
            }
            StmtKind::ForEach {
                var,
                collection,
                body,
            } => {
                self.expr(var)?;
                self.expr(collection)?;
                *body = self.block(std::mem::take(body))?;
            }
            StmtKind::Do { pre, post, body } => {
                for (_, e) in pre.iter_mut().chain(post.iter_mut()) {
                    self.expr(e)?;
                }
                *body = self.block(std::mem::take(body))?;
            }
            StmtKind::With { object, body } => {
                self.hoist(object)?;
                if let Res::Udt(mut at) = self.resolve(object)? {
                    if let Some(ix) = &mut at.idx {
                        self.args(ix)?;
                    }
                    self.with.push(Some(at));
                    let b = self.block(std::mem::take(body));
                    self.with.pop();
                    out.extend(b?);
                    return Ok(());
                }
                self.expr(object)?;
                self.with.push(None);
                let b = self.block(std::mem::take(body));
                self.with.pop();
                *body = b?;
            }
            StmtKind::DebugPrint(items) => {
                for i in items {
                    if let crate::ast::PrintArg::Value(e) = i {
                        self.expr(e)?;
                    }
                }
            }
            StmtKind::DebugAssert(e) => self.expr(e)?,
            StmtKind::Block(body) => *body = self.block(std::mem::take(body))?,
            StmtKind::RaiseEvent { args, .. } => self.args(args)?,
            StmtKind::OnGoTo { index, .. } => self.expr(index)?,
            StmtKind::LSet { target, value, .. } => {
                if let (ExprKind::Name(tn, _), ExprKind::Name(vn, _)) = (&target.kind, &value.kind)
                {
                    if let (Res::Udt(ta), Res::Udt(va)) =
                        (self.resolve(target)?, self.resolve(value)?)
                    {
                        if ta.idx.is_none() && va.idx.is_none() {
                            let src = self.lset_text(pos, tn, &ta.ty, vn, &va.ty)?;
                            let body = crate::parser::parse_module("lset", &src)?
                                .procs
                                .into_iter()
                                .next()
                                .map(|p| p.body)
                                .unwrap_or_default();
                            for st in body {
                                self.stmt(st, out)?;
                            }
                            return Ok(());
                        }
                    }
                }
                self.expr(target)?;
                self.expr(value)?;
            }
        }
        out.push(s);
        Ok(())
    }
}

fn is_lit(e: &Expr) -> bool {
    matches!(&e.kind, ExprKind::Lit(Variant::I2(_) | Variant::I4(_)))
}
