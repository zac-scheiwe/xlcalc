use std::collections::HashMap;

use crate::ast::{
    Arg, Bounds, CaseClause, Expr, ExprKind, Module, ModuleVar, Pos, Stmt, StmtKind, TypeDecl,
    TypeName, VarDecl,
};
use crate::CompileError;

type R<T> = Result<T, CompileError>;

enum Res {
    Udt(String, String),

    Leaf(String),
    No,
}

struct Lower<'a> {
    module: &'a str,
    types: HashMap<String, TypeDecl>,

    vars: HashMap<String, String>,

    with: Vec<Option<(String, String)>>,
}

pub fn lower(module: &str, m: &mut Module, shared: &[TypeDecl]) -> R<()> {
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
        vars: HashMap::new(),
        with: Vec::new(),
    };
    let mut vars = Vec::new();
    for mv in std::mem::take(&mut m.vars) {
        match l.udt_of(&mv.var)? {
            Some(t) => {
                l.vars.insert(mv.var.name.to_ascii_lowercase(), t.clone());
                for var in l.flatten(&mv.var.name, &t, mv.var.pos) {
                    vars.push(ModuleVar {
                        public: mv.public,
                        with_events: false,
                        var,
                    });
                }
            }
            None => vars.push(mv),
        }
    }
    m.vars = vars;
    let globals = l.vars.clone();
    for p in &mut m.procs {
        l.vars = globals.clone();
        for prm in &p.params {
            if l.udt_of(&prm.var)?.is_some() {
                return Err(l.err(prm.var.pos, "a user-defined type parameter"));
            }
            l.vars.remove(&prm.var.name.to_ascii_lowercase());
        }
        if let Some(TypeName::Class(c)) = &p.ret {
            if l.types.contains_key(&c.to_ascii_lowercase()) {
                return Err(l.err(p.pos, "a user-defined type result"));
            }
        }
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

    fn udt_of(&self, v: &VarDecl) -> R<Option<String>> {
        let Some(TypeName::Class(c)) = &v.ty else {
            return Ok(None);
        };
        let l = c.to_ascii_lowercase();
        if !self.types.contains_key(&l) {
            return Ok(None);
        }
        if v.array.is_some() {
            return Err(self.err(v.pos, "an array of a user-defined type"));
        }
        Ok(Some(l))
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

    fn field(&self, name: &str, ty: &str, field: &str, pos: Pos) -> R<Res> {
        let Some(f) = self.types[ty]
            .fields
            .iter()
            .find(|f| f.name.eq_ignore_ascii_case(field))
        else {
            return Err(self.err(pos, &format!("member '{field}' of a user-defined type")));
        };
        let flat = format!("{name}.{}", f.name);
        Ok(match self.field_type(f) {
            Some(t) => Res::Udt(flat, t),
            None => Res::Leaf(flat),
        })
    }

    fn resolve(&self, e: &Expr) -> R<Res> {
        Ok(match &e.kind {
            ExprKind::Name(n, _) => match self.vars.get(&n.to_ascii_lowercase()) {
                Some(t) => Res::Udt(n.clone(), t.clone()),
                None => Res::No,
            },
            ExprKind::Member(None, f) => match self.with.last() {
                Some(Some((n, t))) => self.field(n, t, f, e.pos)?,
                _ => Res::No,
            },
            ExprKind::Member(Some(inner), f) => match self.resolve(inner)? {
                Res::Udt(n, t) => self.field(&n, &t, f, e.pos)?,
                _ => Res::No,
            },
            _ => Res::No,
        })
    }

    fn expr(&mut self, e: &mut Expr) -> R<()> {
        match self.resolve(e)? {
            Res::Leaf(n) => {
                e.kind = ExprKind::Name(n, None);
                return Ok(());
            }
            Res::Udt(..) => return Err(self.err(e.pos, "a user-defined type used as a value")),
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

    fn stmt(&mut self, mut s: Stmt, out: &mut Vec<Stmt>) -> R<()> {
        let pos = s.pos;
        match &mut s.kind {
            StmtKind::Dim { vars, .. } => {
                let mut flat = Vec::new();
                for v in std::mem::take(vars) {
                    let key = v.name.to_ascii_lowercase();
                    match self.udt_of(&v)? {
                        Some(t) => {
                            flat.extend(self.flatten(&v.name, &t, v.pos));
                            self.vars.insert(key, t);
                        }
                        None => {
                            self.vars.remove(&key);
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
                for (e, bounds, _) in items {
                    self.expr(e)?;
                    for b in bounds {
                        self.bounds(b)?;
                    }
                }
            }
            StmtKind::Erase(es) => {
                for e in es {
                    self.expr(e)?;
                }
            }
            StmtKind::Assign { target, value, .. } => {
                if let Res::Udt(tn, tt) = self.resolve(target)? {
                    let Res::Udt(vn, vt) = self.resolve(value)? else {
                        return Err(self.err(pos, "assigning a user-defined type from a value"));
                    };
                    if vt != tt {
                        return Err(self.err(pos, "assigning between different types"));
                    }
                    for (a, b) in self.leaves(&tn, &tt).into_iter().zip(self.leaves(&vn, &vt)) {
                        let name = |n: String| Expr {
                            pos,
                            kind: ExprKind::Name(n, None),
                        };
                        out.push(Stmt {
                            pos,
                            kind: StmtKind::Assign {
                                target: name(a),
                                value: name(b),
                                set: false,
                            },
                        });
                    }
                    return Ok(());
                }
                self.expr(target)?;
                self.expr(value)?;
            }
            StmtKind::Call { callee, args } => {
                self.expr(callee)?;
                self.args(args)?;
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
                if let Res::Udt(n, t) = self.resolve(object)? {
                    self.with.push(Some((n, t)));
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
            StmtKind::RaiseEvent { args, .. } => self.args(args)?,
            StmtKind::OnGoTo { index, .. } => self.expr(index)?,
            StmtKind::LSet { target, value, .. } => {
                self.expr(target)?;
                self.expr(value)?;
            }
        }
        out.push(s);
        Ok(())
    }
}
