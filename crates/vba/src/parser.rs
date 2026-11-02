use crate::ast::*;
use crate::lexer::{lex, Tok, Token, P};
use crate::types::{BinOp, VType};
use crate::CompileError;
use xlcalc_variant::Variant;

pub fn parse_module(module: &str, src: &str) -> Result<Module, CompileError> {
    let toks = lex(module, src)?;
    let mut p = Parser {
        toks,
        i: 0,
        module: module.to_string(),
        default_member: None,
    };
    p.module()
}

type R<T> = Result<T, CompileError>;

struct Parser {
    toks: Vec<Token>,
    i: usize,
    module: String,

    default_member: Option<String>,
}

const RESERVED: &[&str] = &[
    "then", "else", "elseif", "to", "step", "and", "or", "xor", "eqv", "imp", "mod", "like", "is",
    "end", "next", "loop", "wend", "case", "as", "in", "each", "until", "while",
];

impl Parser {
    fn tok(&self) -> &Tok {
        &self.toks[self.i].tok
    }

    fn peek_tok(&self, k: usize) -> &Tok {
        let j = (self.i + k).min(self.toks.len() - 1);
        &self.toks[j].tok
    }

    fn pos(&self) -> Pos {
        let t = &self.toks[self.i];
        Pos {
            line: t.line,
            col: t.col,
        }
    }

    fn advance(&mut self) -> Tok {
        let t = self.toks[self.i].tok.clone();
        if self.i < self.toks.len() - 1 {
            self.i += 1;
        }
        t
    }

    fn err_at(&self, pos: Pos, message: impl Into<String>) -> CompileError {
        CompileError {
            module: self.module.clone(),
            line: pos.line,
            col: pos.col,
            message: message.into(),
        }
    }

    fn err(&self, message: impl Into<String>) -> CompileError {
        self.err_at(self.pos(), message)
    }

    fn expected(&self, what: &str) -> CompileError {
        let found = match self.tok() {
            Tok::Ident(s, _) => format!("'{s}'"),
            Tok::Lit(v) => format!("literal {v:?}"),
            Tok::P(p) => format!("{p:?}"),
            Tok::Newline => "end of line".to_string(),
            Tok::Eof => "end of module".to_string(),
        };
        self.err(format!("expected {what}, found {found}"))
    }

    fn is_kw(&self, kw: &str) -> bool {
        matches!(self.tok(), Tok::Ident(s, None) if s.eq_ignore_ascii_case(kw))
    }

    fn is_kw_at(&self, k: usize, kw: &str) -> bool {
        matches!(self.peek_tok(k), Tok::Ident(s, None) if s.eq_ignore_ascii_case(kw))
    }

    fn eat_kw(&mut self, kw: &str) -> bool {
        if self.is_kw(kw) {
            self.advance();
            true
        } else {
            false
        }
    }

    fn expect_kw(&mut self, kw: &str) -> R<()> {
        if self.eat_kw(kw) {
            Ok(())
        } else {
            Err(self.expected(&format!("'{kw}'")))
        }
    }

    fn is_p(&self, p: P) -> bool {
        *self.tok() == Tok::P(p)
    }

    fn eat_p(&mut self, p: P) -> bool {
        if self.is_p(p) {
            self.advance();
            true
        } else {
            false
        }
    }

    fn expect_p(&mut self, p: P) -> R<()> {
        if self.eat_p(p) {
            Ok(())
        } else {
            Err(self.expected(&format!("{p:?}")))
        }
    }

    fn ident(&mut self) -> R<(String, Option<char>)> {
        match self.tok().clone() {
            Tok::Ident(s, c) if !RESERVED.contains(&s.to_ascii_lowercase().as_str()) => {
                self.advance();
                Ok((s, c))
            }
            _ => Err(self.expected("a name")),
        }
    }

    fn any_ident(&mut self) -> R<String> {
        match self.tok().clone() {
            Tok::Ident(s, _) => {
                self.advance();
                Ok(s)
            }
            _ => Err(self.expected("a member name")),
        }
    }

    fn at_eol(&self) -> bool {
        matches!(self.tok(), Tok::Newline | Tok::Eof)
    }

    fn at_stmt_end(&self) -> bool {
        self.at_eol() || self.is_p(P::Colon) || self.is_kw("else")
    }

    fn expect_eol(&mut self) -> R<()> {
        match self.tok() {
            Tok::Newline => {
                self.advance();
                Ok(())
            }
            Tok::Eof => Ok(()),
            _ => Err(self.expected("end of line")),
        }
    }

    fn expect_header_end(&mut self) -> R<()> {
        if self.eat_p(P::Colon) {
            Ok(())
        } else {
            self.expect_eol()
        }
    }

    fn skip_line(&mut self) {
        while !self.at_eol() {
            self.advance();
        }
        if *self.tok() == Tok::Newline {
            self.advance();
        }
    }

    fn skip_separators(&mut self) {
        while matches!(self.tok(), Tok::Newline | Tok::P(P::Colon)) {
            self.advance();
        }
    }

    fn at_line_start(&self) -> bool {
        self.i == 0 || self.toks[self.i - 1].tok == Tok::Newline
    }

    fn module(&mut self) -> R<Module> {
        let mut m = Module::default();
        self.skip_separators();
        self.class_header();
        loop {
            self.skip_separators();
            if *self.tok() == Tok::Eof {
                m.default_member = self.default_member.take();
                return Ok(m);
            }
            self.module_item(&mut m)?;
        }
    }

    fn class_header(&mut self) {
        if self.is_kw("version") {
            self.skip_line();
            self.skip_separators();
        }
        if self.is_kw("begin") {
            while *self.tok() != Tok::Eof {
                let end = self.is_kw("end") && matches!(self.peek_tok(1), Tok::Newline | Tok::Eof);
                self.skip_line();
                if end {
                    break;
                }
            }
        }
    }

    fn attribute(&mut self) {
        let mut words = Vec::new();
        while !self.at_eol() {
            words.push(self.advance());
        }
        if *self.tok() == Tok::Newline {
            self.advance();
        }
        if let [Tok::Ident(member, _), Tok::P(P::Dot), Tok::Ident(attr, _), Tok::P(P::Eq), Tok::Lit(v)] =
            words.as_slice()
        {
            let zero = matches!(v, Variant::I2(0) | Variant::I4(0));
            if attr.eq_ignore_ascii_case("vb_usermemid") && zero {
                self.default_member = Some(member.to_ascii_lowercase());
            }
        }
    }

    fn unsupported(&self, what: &str) -> CompileError {
        self.err(format!("unsupported: {what}"))
    }

    fn module_item(&mut self, m: &mut Module) -> R<()> {
        if self.eat_kw("attribute") {
            self.attribute();
            return Ok(());
        }
        if self.eat_kw("option") {
            return self.option(m);
        }
        let mut public = None;
        let mut friend = false;
        if self.eat_kw("public") || self.eat_kw("global") {
            public = Some(true);
        } else if self.eat_kw("private") {
            public = Some(false);
        } else if self.eat_kw("friend") {
            public = Some(true);
            friend = true;
        }
        let is_static = self.eat_kw("static");
        if self.is_kw("function") || self.is_kw("sub") || self.is_kw("property") {
            let mut proc = self.procedure(public.unwrap_or(true), is_static)?;
            proc.friend = friend;
            m.procs.push(proc);
            return Ok(());
        }
        if is_static || friend {
            return Err(self.expected("Function, Sub or Property"));
        }
        if self.eat_kw("event") {
            let pos = self.pos();
            let (name, _) = self.ident()?;
            let params = self.param_list()?;
            m.events.push(EventDecl { name, pos, params });
            return self.expect_eol();
        }
        if self.eat_kw("implements") {
            let pos = self.pos();
            let mut name = self.any_ident()?;
            while self.eat_p(P::Dot) {
                name = self.any_ident()?;
            }
            m.implements.push((name, pos));
            return self.expect_eol();
        }
        if self.is_kw("declare") {
            return Err(self.unsupported("Declare (DLL calls)"));
        }
        if self.eat_kw("type") {
            let t = self.type_decl(public.unwrap_or(true))?;
            m.types.push(t);
            return Ok(());
        }
        if let Tok::Ident(s, None) = self.tok() {
            let l = s.to_ascii_lowercase();
            if l.len() == 6 && l.starts_with("def") || l == "defbool" || l == "deflnglng" {
                return Err(self.unsupported("Deftype statements"));
            }
        }
        if self.eat_kw("const") {
            let consts = self.const_list(public.unwrap_or(false))?;
            m.consts.extend(consts);
            return self.expect_eol();
        }
        if self.eat_kw("enum") {
            let e = self.enum_decl(public.unwrap_or(true))?;
            m.enums.push(e);
            return Ok(());
        }
        if public.is_none() && !self.eat_kw("dim") {
            return Err(self.expected("a declaration or procedure"));
        }
        if public.is_some() {
            self.eat_kw("dim");
        }
        let public = public.unwrap_or(false);
        loop {
            let with_events = self.eat_kw("withevents");
            let var = self.var_decl()?;
            m.vars.push(ModuleVar {
                public,
                with_events,
                var,
            });
            if !self.eat_p(P::Comma) {
                break;
            }
        }
        self.expect_eol()
    }

    fn option(&mut self, m: &mut Module) -> R<()> {
        if self.eat_kw("explicit") {
            m.options.explicit = true;
        } else if self.eat_kw("base") {
            match self.advance() {
                Tok::Lit(Variant::I2(0)) => m.options.base = 0,
                Tok::Lit(Variant::I2(1)) => m.options.base = 1,
                _ => return Err(self.err("Option Base must be 0 or 1")),
            }
        } else if self.eat_kw("compare") {
            if self.eat_kw("text") {
                m.options.compare_text = true;
            } else if self.eat_kw("binary") {
                m.options.compare_text = false;
            } else if self.is_kw("database") {
                return Err(self.unsupported("Option Compare Database (Access)"));
            } else {
                return Err(self.expected("Text or Binary"));
            }
        } else if self.eat_kw("private") {
            self.expect_kw("module")?;
            m.options.private_module = true;
        } else {
            return Err(self.expected("an option"));
        }
        self.expect_eol()
    }

    fn type_decl(&mut self, public: bool) -> R<TypeDecl> {
        let pos = self.pos();
        let (name, _) = self.ident()?;
        self.expect_eol()?;
        let mut fields = Vec::new();
        loop {
            self.skip_separators();
            if self.is_kw("end") && self.is_kw_at(1, "type") {
                self.advance();
                self.advance();
                break;
            }
            if *self.tok() == Tok::Eof {
                return Err(self.expected("End Type"));
            }
            fields.push(self.var_decl()?);
            self.expect_eol()?;
        }
        self.expect_eol()?;
        Ok(TypeDecl {
            public,
            name,
            pos,
            fields,
        })
    }

    fn enum_decl(&mut self, public: bool) -> R<EnumDecl> {
        let pos = self.pos();
        let (name, _) = self.ident()?;
        self.expect_eol()?;
        let mut members = Vec::new();
        loop {
            self.skip_separators();
            if self.is_kw("end") && self.is_kw_at(1, "enum") {
                self.advance();
                self.advance();
                break;
            }
            if *self.tok() == Tok::Eof {
                return Err(self.expected("End Enum"));
            }
            let mpos = self.pos();
            let (m, _) = self.ident()?;
            let value = if self.eat_p(P::Eq) {
                Some(self.expr()?)
            } else {
                None
            };
            members.push((m, mpos, value));
            self.expect_eol()?;
        }
        self.expect_eol()?;
        Ok(EnumDecl {
            public,
            name,
            pos,
            members,
        })
    }

    fn type_name(&mut self) -> R<TypeName> {
        let pos = self.pos();
        let mut name = self.any_ident()?;

        while self.eat_p(P::Dot) {
            name = self.any_ident()?;
        }
        Ok(match VType::named(&name) {
            Some(t) => TypeName::Builtin(t),
            None if name.eq_ignore_ascii_case("any") => {
                return Err(self.err_at(pos, "unsupported: As Any"))
            }
            None => TypeName::Class(name),
        })
    }

    fn as_type(&mut self, suffix: Option<char>) -> R<Option<TypeName>> {
        let declared = if self.eat_kw("as") {
            Some(self.type_name()?)
        } else {
            None
        };
        match (suffix, declared) {
            (Some(_), Some(_)) => Err(self.err("a name with a type character cannot have As")),
            (Some(c), None) => Ok(Some(TypeName::Builtin(VType::of_suffix(c)))),
            (None, t) => Ok(t),
        }
    }

    fn bounds_list(&mut self) -> R<Vec<Bounds>> {
        let mut out = Vec::new();
        if self.is_p(P::RParen) {
            return Ok(out);
        }
        loop {
            let first = self.expr()?;
            let b = if self.eat_kw("to") {
                Bounds {
                    lower: Some(first),
                    upper: self.expr()?,
                }
            } else {
                Bounds {
                    lower: None,
                    upper: first,
                }
            };
            out.push(b);
            if !self.eat_p(P::Comma) {
                return Ok(out);
            }
        }
    }

    fn var_decl(&mut self) -> R<VarDecl> {
        if self.is_kw("withevents") {
            return Err(self.err("WithEvents is only valid at module level"));
        }
        let pos = self.pos();
        let (name, suffix) = self.ident()?;
        let array = if self.eat_p(P::LParen) {
            let b = self.bounds_list()?;
            self.expect_p(P::RParen)?;
            Some(b)
        } else {
            None
        };
        let mut new = false;
        let ty = if suffix.is_none() && self.is_kw("as") && self.is_kw_at(1, "new") {
            self.advance();
            self.advance();
            new = true;
            Some(self.type_name()?)
        } else {
            self.as_type(suffix)?
        };
        let fixed_len = if self.eat_p(P::Star) {
            if ty != Some(TypeName::Builtin(VType::String)) {
                return Err(self.err("only a String can have a fixed length"));
            }
            Some(self.postfix(false, true)?)
        } else {
            None
        };
        Ok(VarDecl {
            name,
            pos,
            ty,
            array,
            new,
            fixed_len,
        })
    }

    fn var_list(&mut self) -> R<Vec<VarDecl>> {
        let mut out = vec![self.var_decl()?];
        while self.eat_p(P::Comma) {
            out.push(self.var_decl()?);
        }
        Ok(out)
    }

    fn const_list(&mut self, public: bool) -> R<Vec<ConstDecl>> {
        let mut out = Vec::new();
        loop {
            let pos = self.pos();
            let (name, suffix) = self.ident()?;
            let ty = self.as_type(suffix)?;
            self.expect_p(P::Eq)?;
            let value = self.expr()?;
            out.push(ConstDecl {
                public,
                name,
                pos,
                ty,
                value,
            });
            if !self.eat_p(P::Comma) {
                return Ok(out);
            }
        }
    }

    fn param_list(&mut self) -> R<Vec<Param>> {
        let mut params = Vec::new();
        if self.eat_p(P::LParen) {
            if !self.is_p(P::RParen) {
                loop {
                    params.push(self.param()?);
                    if !self.eat_p(P::Comma) {
                        break;
                    }
                }
            }
            self.expect_p(P::RParen)?;
        }
        Ok(params)
    }

    fn procedure(&mut self, public: bool, is_static: bool) -> R<Proc> {
        let kind = if self.eat_kw("function") {
            ProcKind::Function
        } else if self.eat_kw("property") {
            if self.eat_kw("get") {
                ProcKind::PropertyGet
            } else if self.eat_kw("let") {
                ProcKind::PropertyLet
            } else if self.eat_kw("set") {
                ProcKind::PropertySet
            } else {
                return Err(self.expected("Get, Let or Set"));
            }
        } else {
            self.expect_kw("sub")?;
            ProcKind::Sub
        };
        let pos = self.pos();
        let (name, suffix) = self.ident()?;
        let params = self.param_list()?;
        let mut ret_array = false;
        let ret = if kind.returns_value() {
            let t = self.as_type(suffix)?;
            if self.eat_p(P::LParen) {
                self.expect_p(P::RParen)?;
                ret_array = true;
            }
            t
        } else {
            None
        };
        self.expect_header_end()?;
        let end = match kind {
            ProcKind::Function => "function",
            ProcKind::Sub => "sub",
            _ => "property",
        };
        let body = self.block(&|p: &Parser| p.is_kw("end") && p.is_kw_at(1, end))?;
        self.advance();
        self.advance();
        self.expect_eol()?;
        Ok(Proc {
            kind,
            name,
            pos,
            public,
            friend: false,
            is_static,
            params,
            ret,
            ret_array,
            body,
        })
    }

    fn param(&mut self) -> R<Param> {
        let optional = self.eat_kw("optional");
        let mut by_val = false;
        if self.eat_kw("byval") {
            by_val = true;
        } else {
            self.eat_kw("byref");
        }
        let param_array = self.eat_kw("paramarray");
        let pos = self.pos();
        let (name, suffix) = self.ident()?;
        let array = if self.eat_p(P::LParen) {
            self.expect_p(P::RParen)?;
            Some(Vec::new())
        } else {
            None
        };
        let ty = self.as_type(suffix)?;
        let default = if self.eat_p(P::Eq) {
            Some(self.expr()?)
        } else {
            None
        };
        if param_array && (array.is_none() || optional || by_val) {
            return Err(self.err_at(pos, "ParamArray must be a Variant array: ParamArray x()"));
        }
        Ok(Param {
            var: VarDecl {
                name,
                pos,
                ty,
                array,
                new: false,
                fixed_len: None,
            },
            by_val,
            optional,
            param_array,
            default,
        })
    }

    fn block(&mut self, end: &dyn Fn(&Parser) -> bool) -> R<Vec<Stmt>> {
        let mut out = Vec::new();
        loop {
            self.skip_separators();
            if *self.tok() == Tok::Eof {
                return Err(self.expected("the end of the block"));
            }
            if end(self) {
                return Ok(out);
            }
            self.statement(&mut out)?;
        }
    }

    fn statement(&mut self, out: &mut Vec<Stmt>) -> R<()> {
        let pos = self.pos();

        if self.at_line_start() {
            match self.tok().clone() {
                Tok::Ident(name, None)
                    if *self.peek_tok(1) == Tok::P(P::Colon)
                        && !RESERVED.contains(&name.to_ascii_lowercase().as_str()) =>
                {
                    self.advance();
                    self.advance();
                    out.push(Stmt {
                        pos,
                        kind: StmtKind::Label(name.to_ascii_lowercase()),
                    });
                    return Ok(());
                }
                Tok::Lit(Variant::I2(_) | Variant::I4(_)) => {
                    let Tok::Lit(v) = self.advance() else {
                        unreachable!()
                    };
                    out.push(Stmt {
                        pos,
                        kind: StmtKind::Label(line_label(&v)),
                    });
                    self.eat_p(P::Colon);
                    return Ok(());
                }
                _ => {}
            }
        }
        let kind = self.statement_kind(out)?;
        if let Some(kind) = kind {
            out.push(Stmt { pos, kind });
        }
        Ok(())
    }

    fn statement_kind(&mut self, out: &mut Vec<Stmt>) -> R<Option<StmtKind>> {
        let Tok::Ident(word, None) = self.tok().clone() else {
            return self.simple_statement().map(Some);
        };
        let word = word.to_ascii_lowercase();
        let pos = self.pos();
        const FILE_WORDS: [&str; 17] = [
            "open", "close", "print", "input", "line", "get", "put", "write", "seek", "lock",
            "unlock", "kill", "mkdir", "rmdir", "chdir", "name", "reset",
        ];

        if FILE_WORDS.contains(&word.as_str())
            && matches!(self.peek_tok(1), Tok::P(P::Eq | P::Dot | P::LParen))
        {
            return self.simple_statement().map(Some);
        }
        Ok(Some(match word.as_str() {
            "attribute" => {
                self.advance();
                self.attribute();
                return Ok(None);
            }
            "dim" | "static" => {
                self.advance();
                StmtKind::Dim {
                    is_static: word == "static",
                    vars: self.var_list()?,
                }
            }
            "const" => {
                self.advance();
                StmtKind::Const(self.const_list(false)?)
            }
            "redim" => {
                self.advance();
                let preserve = self.eat_kw("preserve");
                let mut items = Vec::new();
                loop {
                    let tpos = self.pos();
                    let (name, suffix) = self.ident()?;
                    let target = Expr {
                        pos: tpos,
                        kind: ExprKind::Name(name, suffix),
                    };
                    self.expect_p(P::LParen)?;
                    let bounds = self.bounds_list()?;
                    self.expect_p(P::RParen)?;
                    if bounds.is_empty() {
                        return Err(self.err_at(tpos, "ReDim needs bounds: ReDim a(n)"));
                    }
                    let ty = self.as_type(None)?;
                    items.push((target, bounds, ty));
                    if !self.eat_p(P::Comma) {
                        break;
                    }
                }
                StmtKind::ReDim { preserve, items }
            }
            "erase" => {
                self.advance();
                let mut list = vec![self.expr()?];
                while self.eat_p(P::Comma) {
                    list.push(self.expr()?);
                }
                StmtKind::Erase(list)
            }
            "set" | "let" => {
                self.advance();
                let target = self.postfix(false, false)?;
                self.expect_p(P::Eq)?;
                let value = self.expr()?;
                StmtKind::Assign {
                    target,
                    value,
                    set: word == "set",
                }
            }
            "call" => {
                self.advance();
                let target = self.postfix(false, false)?;
                match target.kind {
                    ExprKind::Index(callee, args) => StmtKind::Call {
                        callee: *callee,
                        args,
                    },
                    _ => StmtKind::Call {
                        callee: target,
                        args: Vec::new(),
                    },
                }
            }
            "if" => return self.if_statement(out),
            "select" => self.select_statement()?,
            "for" => self.for_statement()?,
            "do" => {
                self.advance();
                let pre = self.loop_cond()?;
                self.expect_header_end()?;
                let body = self.block(&|p: &Parser| p.is_kw("loop"))?;
                self.advance();
                let post = self.loop_cond()?;
                if pre.is_some() && post.is_some() {
                    return Err(self.err_at(pos, "a Do loop has its condition at one end only"));
                }
                StmtKind::Do { pre, post, body }
            }
            "while" => {
                self.advance();
                let cond = self.expr()?;
                self.expect_header_end()?;
                let body = self.block(&|p: &Parser| p.is_kw("wend"))?;
                self.advance();
                StmtKind::Do {
                    pre: Some((LoopCond::While, cond)),
                    post: None,
                    body,
                }
            }
            "with" => {
                self.advance();
                let object = self.expr()?;
                self.expect_header_end()?;
                let body = self.block(&|p: &Parser| p.is_kw("end") && p.is_kw_at(1, "with"))?;
                self.advance();
                self.advance();
                StmtKind::With { object, body }
            }
            "exit" => {
                self.advance();
                let k = if self.eat_kw("function") {
                    ExitKind::Function
                } else if self.eat_kw("sub") {
                    ExitKind::Sub
                } else if self.eat_kw("property") {
                    ExitKind::Property
                } else if self.eat_kw("for") {
                    ExitKind::For
                } else if self.eat_kw("do") {
                    ExitKind::Do
                } else {
                    return Err(self.expected("Function, Sub, Property, For or Do"));
                };
                StmtKind::Exit(k)
            }
            "goto" => {
                self.advance();
                StmtKind::GoTo(self.label_name()?)
            }
            "gosub" => {
                self.advance();
                StmtKind::GoSub(self.label_name()?)
            }
            "return" => {
                self.advance();
                StmtKind::Return
            }
            "on" if !self.is_kw_at(1, "error") => {
                self.advance();
                let index = self.expr()?;
                let gosub = if self.eat_kw("gosub") {
                    true
                } else {
                    self.expect_kw("goto")?;
                    false
                };
                let mut labels = vec![self.label_name()?];
                while self.eat_p(P::Comma) {
                    labels.push(self.label_name()?);
                }
                StmtKind::OnGoTo {
                    index,
                    labels,
                    gosub,
                }
            }
            "on" => {
                self.advance();
                self.advance();
                if self.eat_kw("resume") {
                    self.expect_kw("next")?;
                    StmtKind::OnError(OnError::ResumeNext)
                } else {
                    self.expect_kw("goto")?;
                    match self.tok().clone() {
                        Tok::Lit(Variant::I2(0)) => {
                            self.advance();
                            StmtKind::OnError(OnError::Disable)
                        }
                        Tok::P(P::Minus) => {
                            self.advance();
                            match self.advance() {
                                Tok::Lit(Variant::I2(1)) => StmtKind::OnError(OnError::Reset),
                                _ => return Err(self.expected("1")),
                            }
                        }
                        _ => StmtKind::OnError(OnError::GoTo(self.label_name()?)),
                    }
                }
            }
            "resume" => {
                self.advance();
                if self.eat_kw("next") {
                    StmtKind::Resume(ResumeTarget::Next)
                } else if self.at_stmt_end() {
                    StmtKind::Resume(ResumeTarget::Again)
                } else if *self.tok() == Tok::Lit(Variant::I2(0)) {
                    self.advance();
                    StmtKind::Resume(ResumeTarget::Again)
                } else {
                    StmtKind::Resume(ResumeTarget::Label(self.label_name()?))
                }
            }
            "debug" if *self.peek_tok(1) == Tok::P(P::Dot) => {
                self.advance();
                self.advance();
                let m = self.any_ident()?;
                if m.eq_ignore_ascii_case("assert") {
                    StmtKind::DebugAssert(self.expr()?)
                } else if m.eq_ignore_ascii_case("print") {
                    let mut list = Vec::new();
                    while !self.at_stmt_end() {
                        if self.eat_p(P::Semicolon) {
                            list.push(PrintArg::Join);
                        } else if self.eat_p(P::Comma) {
                            list.push(PrintArg::Zone);
                        } else {
                            list.push(PrintArg::Value(self.expr()?));
                        }
                    }
                    StmtKind::DebugPrint(list)
                } else {
                    return Err(self.err_at(pos, format!("unknown Debug member {m}")));
                }
            }
            "end" => {
                if let Tok::Ident(s, _) = self.peek_tok(1) {
                    if !s.eq_ignore_ascii_case("else") {
                        return Err(self.err(format!("End {s} without its block")));
                    }
                }
                self.advance();
                StmtKind::End
            }
            "else" | "elseif" | "loop" | "next" | "wend" | "case" => {
                return Err(self.err(format!("{word} without its block")))
            }
            "private" | "public" | "global" | "option" | "enum" | "function" | "sub" => {
                return Err(self.err(format!("{word} is not valid inside a procedure")))
            }
            "open" | "close" | "print" | "input" | "line" | "get" | "put" | "write" | "seek"
            | "lock" | "unlock" | "kill" | "mkdir" | "rmdir" | "chdir" | "name" | "reset" => {
                return Err(self.unsupported("file statements"))
            }
            "stop" => {
                self.advance();
                StmtKind::Stop
            }
            "lset" | "rset" => {
                self.advance();
                let target = self.postfix(false, false)?;
                self.expect_p(P::Eq)?;
                let value = self.expr()?;
                StmtKind::LSet {
                    target,
                    value,
                    right: word == "rset",
                }
            }
            "raiseevent" => {
                self.advance();
                let (name, _) = self.ident()?;
                let mut args = Vec::new();
                if self.eat_p(P::LParen) {
                    if !self.is_p(P::RParen) {
                        loop {
                            args.push(self.arg(false)?);
                            if !self.eat_p(P::Comma) {
                                break;
                            }
                        }
                    }
                    self.expect_p(P::RParen)?;
                }
                StmtKind::RaiseEvent { name, args }
            }
            "error" => return Err(self.unsupported("the Error statement (use Err.Raise)")),
            _ => self.simple_statement()?,
        }))
    }

    fn label_name(&mut self) -> R<String> {
        match self.advance() {
            Tok::Ident(s, None) => Ok(s.to_ascii_lowercase()),
            Tok::Lit(v @ (Variant::I2(_) | Variant::I4(_))) => Ok(line_label(&v)),
            _ => Err(self.err("expected a label")),
        }
    }

    fn simple_statement(&mut self) -> R<StmtKind> {
        let target = self.postfix(true, false)?;
        if self.eat_p(P::Eq) {
            let value = self.expr()?;
            return Ok(StmtKind::Assign {
                target,
                value,
                set: false,
            });
        }
        let mut args = Vec::new();
        if !self.at_stmt_end() {
            loop {
                args.push(self.arg(true)?);
                if !self.eat_p(P::Comma) {
                    break;
                }
            }
        }
        if !self.at_stmt_end() {
            return Err(self.expected("end of statement"));
        }
        match target.kind {
            ExprKind::Name(..) | ExprKind::Member(..) => Ok(StmtKind::Call {
                callee: target,
                args,
            }),
            ExprKind::Index(callee, inner) if args.is_empty() => Ok(StmtKind::Call {
                callee: *callee,
                args: inner,
            }),
            _ => Err(self.err_at(target.pos, "expected a statement")),
        }
    }

    fn if_statement(&mut self, out: &mut Vec<Stmt>) -> R<Option<StmtKind>> {
        let pos = self.pos();
        self.advance();
        let cond = self.expr()?;
        self.expect_kw("then")?;
        if self.at_eol() {
            self.expect_eol()?;
            let mut arms = Vec::new();
            let mut otherwise = None;
            let mut cond = cond;
            loop {
                let body = self.block(&|p: &Parser| {
                    p.is_kw("elseif") || p.is_kw("else") || (p.is_kw("end") && p.is_kw_at(1, "if"))
                })?;
                arms.push((cond, body));
                if self.eat_kw("elseif") {
                    cond = self.expr()?;
                    self.expect_kw("then")?;
                    self.expect_eol()?;
                    continue;
                }
                if self.eat_kw("else") {
                    otherwise =
                        Some(self.block(&|p: &Parser| p.is_kw("end") && p.is_kw_at(1, "if"))?);
                }
                self.advance();
                self.advance();
                break;
            }
            return Ok(Some(StmtKind::If { arms, otherwise }));
        }

        let then = self.single_line_body()?;
        let otherwise = if self.eat_kw("else") {
            Some(self.single_line_body()?)
        } else {
            None
        };
        if !self.at_eol() {
            return Err(self.expected("end of line"));
        }
        out.push(Stmt {
            pos,
            kind: StmtKind::If {
                arms: vec![(cond, then)],
                otherwise,
            },
        });
        Ok(None)
    }

    fn single_line_body(&mut self) -> R<Vec<Stmt>> {
        let mut out = Vec::new();

        if let Tok::Lit(v @ (Variant::I2(_) | Variant::I4(_))) = self.tok().clone() {
            let pos = self.pos();
            self.advance();
            out.push(Stmt {
                pos,
                kind: StmtKind::GoTo(line_label(&v)),
            });
            return Ok(out);
        }
        loop {
            while self.eat_p(P::Colon) {}
            if self.at_eol() || self.is_kw("else") {
                return Ok(out);
            }
            let pos = self.pos();
            if let Some(kind) = self.statement_kind(&mut out)? {
                out.push(Stmt { pos, kind });
            }
        }
    }

    fn select_statement(&mut self) -> R<StmtKind> {
        self.advance();
        self.expect_kw("case")?;
        let subject = self.expr()?;
        self.expect_header_end()?;
        self.skip_separators();
        let mut cases = Vec::new();
        let mut otherwise = None;
        let end = |p: &Parser| p.is_kw("case") || (p.is_kw("end") && p.is_kw_at(1, "select"));
        loop {
            if self.is_kw("end") && self.is_kw_at(1, "select") {
                self.advance();
                self.advance();
                break;
            }
            self.expect_kw("case")?;
            if self.eat_kw("else") {
                otherwise = Some(self.block(&end)?);
                continue;
            }
            let mut clauses = Vec::new();
            loop {
                if self.eat_kw("is") {
                    let op = match self.advance() {
                        Tok::P(P::Eq) => BinOp::Eq,
                        Tok::P(P::Ne) => BinOp::Ne,
                        Tok::P(P::Lt) => BinOp::Lt,
                        Tok::P(P::Le) => BinOp::Le,
                        Tok::P(P::Gt) => BinOp::Gt,
                        Tok::P(P::Ge) => BinOp::Ge,
                        _ => return Err(self.err("expected a comparison after Case Is")),
                    };
                    clauses.push(CaseClause::Is(op, self.expr()?));
                } else {
                    let a = self.expr()?;
                    if self.eat_kw("to") {
                        clauses.push(CaseClause::Range(a, self.expr()?));
                    } else {
                        clauses.push(CaseClause::Value(a));
                    }
                }
                if !self.eat_p(P::Comma) {
                    break;
                }
            }
            let body = self.block(&end)?;
            cases.push((clauses, body));
        }
        Ok(StmtKind::Select {
            subject,
            cases,
            otherwise,
        })
    }

    fn for_statement(&mut self) -> R<StmtKind> {
        self.advance();
        let each = self.eat_kw("each");
        let var = self.postfix(false, false)?;
        let (header, from, to, step) = if each {
            self.expect_kw("in")?;
            (true, self.expr()?, None, None)
        } else {
            self.expect_p(P::Eq)?;
            let from = self.expr()?;
            self.expect_kw("to")?;
            let to = self.expr()?;
            let step = if self.eat_kw("step") {
                Some(self.expr()?)
            } else {
                None
            };
            (false, from, Some(to), step)
        };
        self.expect_header_end()?;
        let body = self.block(&|p: &Parser| p.is_kw("next"))?;
        self.advance();
        if !self.at_stmt_end() {
            self.postfix(false, false)?;

            if self.is_p(P::Comma) {
                let t = &mut self.toks[self.i];
                t.tok = Tok::Ident("Next".to_string(), None);
            }
        }
        Ok(if header {
            StmtKind::ForEach {
                var,
                collection: from,
                body,
            }
        } else {
            StmtKind::For {
                var,
                from,
                to: to.expect("For has To"),
                step,
                body,
            }
        })
    }

    fn loop_cond(&mut self) -> R<Option<(LoopCond, Expr)>> {
        if self.eat_kw("while") {
            Ok(Some((LoopCond::While, self.expr()?)))
        } else if self.eat_kw("until") {
            Ok(Some((LoopCond::Until, self.expr()?)))
        } else {
            Ok(None)
        }
    }

    pub(crate) fn expr(&mut self) -> R<Expr> {
        self.binary(0)
    }

    fn binary(&mut self, level: usize) -> R<Expr> {
        const LEVELS: usize = 11;
        if level == 5 {
            if self.is_kw("not") {
                let pos = self.pos();
                self.advance();
                let e = self.binary(5)?;
                return Ok(Expr {
                    pos,
                    kind: ExprKind::Unary(UnOp::Not, Box::new(e)),
                });
            }
            return self.binary(6);
        }
        if level > LEVELS {
            return self.unary();
        }
        let mut left = self.binary(level + 1)?;
        loop {
            let op = match (level, self.tok()) {
                (0, Tok::Ident(..)) if self.is_kw("imp") => BinOp::Imp,
                (1, Tok::Ident(..)) if self.is_kw("eqv") => BinOp::Eqv,
                (2, Tok::Ident(..)) if self.is_kw("xor") => BinOp::Xor,
                (3, Tok::Ident(..)) if self.is_kw("or") => BinOp::Or,
                (4, Tok::Ident(..)) if self.is_kw("and") => BinOp::And,
                (6, Tok::P(P::Eq)) => BinOp::Eq,
                (6, Tok::P(P::Ne)) => BinOp::Ne,
                (6, Tok::P(P::Lt)) => BinOp::Lt,
                (6, Tok::P(P::Le)) => BinOp::Le,
                (6, Tok::P(P::Gt)) => BinOp::Gt,
                (6, Tok::P(P::Ge)) => BinOp::Ge,
                (6, Tok::Ident(..)) if self.is_kw("like") => BinOp::Like,
                (6, Tok::Ident(..)) if self.is_kw("is") => BinOp::Is,
                (7, Tok::P(P::Amp)) => BinOp::Cat,
                (8, Tok::P(P::Plus)) => BinOp::Add,
                (8, Tok::P(P::Minus)) => BinOp::Sub,
                (9, Tok::Ident(..)) if self.is_kw("mod") => BinOp::Mod,
                (10, Tok::P(P::Backslash)) => BinOp::IntDiv,
                (11, Tok::P(P::Star)) => BinOp::Mul,
                (11, Tok::P(P::Slash)) => BinOp::Div,
                _ => return Ok(left),
            };
            let pos = self.pos();
            self.advance();
            let right = self.binary(level + 1)?;
            left = Expr {
                pos,
                kind: ExprKind::Binary(op, Box::new(left), Box::new(right)),
            };
        }
    }

    fn unary(&mut self) -> R<Expr> {
        let pos = self.pos();
        if self.eat_p(P::Minus) {
            let e = self.unary()?;
            return Ok(Expr {
                pos,
                kind: ExprKind::Unary(UnOp::Neg, Box::new(e)),
            });
        }
        if self.eat_p(P::Plus) {
            return self.unary();
        }
        self.power()
    }

    fn power(&mut self) -> R<Expr> {
        let mut left = self.postfix(false, true)?;
        while self.is_p(P::Caret) {
            let pos = self.pos();
            self.advance();
            let right = if self.is_p(P::Minus) || self.is_p(P::Plus) {
                let neg = self.is_p(P::Minus);
                let upos = self.pos();
                self.advance();
                let e = self.postfix(false, true)?;
                if neg {
                    Expr {
                        pos: upos,
                        kind: ExprKind::Unary(UnOp::Neg, Box::new(e)),
                    }
                } else {
                    e
                }
            } else {
                self.postfix(false, true)?
            };
            left = Expr {
                pos,
                kind: ExprKind::Binary(BinOp::Pow, Box::new(left), Box::new(right)),
            };
        }
        Ok(left)
    }

    fn postfix(&mut self, stmt: bool, value: bool) -> R<Expr> {
        let mut e = self.primary(value)?;
        loop {
            let pos = self.pos();
            if self.is_p(P::Dot) && !self.toks[self.i].space {
                self.advance();
                let name = self.any_ident()?;
                e = Expr {
                    pos,
                    kind: ExprKind::Member(Some(Box::new(e)), name),
                };
            } else if self.is_p(P::Bang) {
                self.advance();
                let name = self.any_ident()?;
                let arg = Arg {
                    name: None,
                    value: Some(Expr {
                        pos,
                        kind: ExprKind::Lit(Variant::Bstr(name)),
                    }),
                };
                e = Expr {
                    pos,
                    kind: ExprKind::Index(Box::new(e), vec![arg]),
                };
            } else if self.is_p(P::LParen) && !(stmt && self.toks[self.i].space) {
                if !matches!(
                    e.kind,
                    ExprKind::Name(..) | ExprKind::Member(..) | ExprKind::Index(..)
                ) {
                    return Ok(e);
                }
                self.advance();
                let mut args = Vec::new();
                if !self.is_p(P::RParen) {
                    loop {
                        args.push(self.arg(false)?);
                        if !self.eat_p(P::Comma) {
                            break;
                        }
                    }
                }
                self.expect_p(P::RParen)?;
                e = Expr {
                    pos,
                    kind: ExprKind::Index(Box::new(e), args),
                };
            } else {
                return Ok(e);
            }
        }
    }

    fn arg(&mut self, stmt: bool) -> R<Arg> {
        if self.is_p(P::Comma) || (!stmt && self.is_p(P::RParen)) {
            return Ok(Arg {
                name: None,
                value: None,
            });
        }
        if let (Tok::Ident(n, None), Tok::P(P::Assign)) = (self.tok().clone(), self.peek_tok(1)) {
            self.advance();
            self.advance();
            return Ok(Arg {
                name: Some(n),
                value: Some(self.expr()?),
            });
        }
        if self.eat_kw("byval") {
            return Err(self.unsupported("ByVal in a call"));
        }
        Ok(Arg {
            name: None,
            value: Some(self.expr()?),
        })
    }

    fn primary(&mut self, value: bool) -> R<Expr> {
        let pos = self.pos();
        let kind = match self.tok().clone() {
            Tok::Lit(v) => {
                self.advance();
                ExprKind::Lit(v)
            }
            Tok::P(P::LParen) => {
                self.advance();
                let e = self.expr()?;
                self.expect_p(P::RParen)?;
                ExprKind::Paren(Box::new(e))
            }
            Tok::P(P::Dot) | Tok::P(P::Bang) => {
                let bang = self.is_p(P::Bang);
                self.advance();
                let name = self.any_ident()?;
                if bang {
                    return Err(self.unsupported("a leading ! inside With"));
                }
                ExprKind::Member(None, name)
            }
            Tok::Ident(word, suffix) => {
                let lower = word.to_ascii_lowercase();
                match lower.as_str() {
                    "true" | "false" if suffix.is_none() => {
                        self.advance();
                        ExprKind::Lit(Variant::Bool(lower == "true"))
                    }
                    "nothing" => {
                        self.advance();
                        ExprKind::Nothing
                    }
                    "empty" => {
                        self.advance();
                        ExprKind::Empty
                    }
                    "null" => {
                        self.advance();
                        ExprKind::Null
                    }
                    "new" => {
                        self.advance();
                        let mut name = self.any_ident()?;
                        while self.eat_p(P::Dot) {
                            name = self.any_ident()?;
                        }
                        ExprKind::New(name)
                    }
                    "me" if suffix.is_none() => {
                        self.advance();
                        ExprKind::Me
                    }
                    "addressof" => return Err(self.unsupported("AddressOf")),
                    "typeof" => {
                        self.advance();
                        let e = self.postfix(false, true)?;
                        self.expect_kw("is")?;
                        let (TypeName::Class(c) | TypeName::Builtin(VType::Object(Some(c)))) =
                            self.type_name()?
                        else {
                            return Err(self.err("TypeOf ... Is needs a class name"));
                        };
                        ExprKind::TypeOf(Box::new(e), c.to_ascii_lowercase())
                    }
                    "not" if value => {
                        self.advance();
                        let e = self.binary(6)?;
                        ExprKind::Unary(UnOp::Not, Box::new(e))
                    }
                    w if RESERVED.contains(&w) || w == "not" => {
                        return Err(self.expected("an expression"))
                    }
                    _ => {
                        self.advance();
                        ExprKind::Name(word, suffix)
                    }
                }
            }
            _ => return Err(self.expected("an expression")),
        };
        Ok(Expr { pos, kind })
    }
}

fn line_label(v: &Variant) -> String {
    match v {
        Variant::I2(n) => n.to_string(),
        Variant::I4(n) => n.to_string(),
        _ => String::new(),
    }
}
