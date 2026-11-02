use std::collections::HashMap;

use xlcalc::SheetId;
use xlcalc_variant::Variant;
use xlcalc_vba::{ModuleKind, SourceModule};
use xlcalc_vbaproject::write::{build, header, ModuleSource};
use xlcalc_xlsx::VbaWrite;

use crate::app::{Application, BookId, Object};
use crate::conv::Args;
use crate::error::{vba_scode, OmError};
use crate::typelib::Member;

pub(crate) struct Ide {
    pub changed: bool,
    workbook_code: Option<String>,
    sheet_codes: HashMap<SheetId, String>,
    project: String,

    uids: Vec<(u32, String)>,
    next_uid: u32,
}

impl Default for Ide {
    fn default() -> Ide {
        Ide {
            changed: false,
            workbook_code: None,
            sheet_codes: HashMap::new(),
            project: "VBAProject".to_string(),
            uids: Vec::new(),
            next_uid: 1,
        }
    }
}

#[derive(Clone, Copy, PartialEq, Eq, Hash, Debug)]
pub(crate) enum Comp {
    Workbook,
    Sheet(SheetId),

    Module(u32),
}

const STD: i32 = 1;
const CLASS: i32 = 2;
const DOCUMENT: i32 = 100;

const WORKBOOK_BASE: &str = "{00020819-0000-0000-C000-000000000046}";
const SHEET_BASE: &str = "{00020820-0000-0000-C000-000000000046}";

const RESERVED: &[&str] = &[
    "and",
    "as",
    "boolean",
    "byref",
    "byte",
    "byval",
    "call",
    "case",
    "const",
    "currency",
    "date",
    "declare",
    "dim",
    "do",
    "double",
    "each",
    "else",
    "elseif",
    "empty",
    "end",
    "enum",
    "eqv",
    "error",
    "event",
    "exit",
    "false",
    "for",
    "friend",
    "function",
    "get",
    "global",
    "gosub",
    "goto",
    "if",
    "imp",
    "implements",
    "in",
    "integer",
    "is",
    "let",
    "like",
    "long",
    "loop",
    "me",
    "mod",
    "new",
    "next",
    "not",
    "nothing",
    "null",
    "object",
    "on",
    "option",
    "optional",
    "or",
    "paramarray",
    "preserve",
    "private",
    "property",
    "public",
    "raiseevent",
    "redim",
    "rem",
    "resume",
    "return",
    "select",
    "set",
    "single",
    "static",
    "stop",
    "string",
    "sub",
    "then",
    "to",
    "true",
    "type",
    "until",
    "variant",
    "wend",
    "while",
    "with",
    "withevents",
    "xor",
];

fn range_error() -> OmError {
    OmError {
        source: Some(String::new()),
        ..OmError::exception(
            0x8007_0057_u32 as i32,
            Some("Invalid procedure call or argument"),
        )
    }
}

fn bad_index() -> OmError {
    OmError {
        source: Some("VBAProject".to_string()),
        ..OmError::exception(vba_scode(9), Some("Subscript out of range"))
    }
}

fn invalid_name() -> OmError {
    OmError::exception(vba_scode(50132), None)
}

fn name_conflict() -> OmError {
    OmError::exception(vba_scode(32813), None)
}

fn valid_name(name: &str) -> bool {
    let mut chars = name.chars();
    chars.next().is_some_and(|c| c.is_ascii_alphabetic())
        && name.len() <= 31
        && name.chars().all(|c| c.is_ascii_alphanumeric() || c == '_')
        && !RESERVED.contains(&name.to_ascii_lowercase().as_str())
}

fn split_lines(text: &str) -> Vec<String> {
    let mut out = Vec::new();
    let mut cur = String::new();
    let mut chars = text.chars().peekable();
    while let Some(c) = chars.next() {
        match c {
            '\r' => {
                if chars.peek() == Some(&'\n') {
                    chars.next();
                }
                out.push(std::mem::take(&mut cur));
            }
            '\n' => out.push(std::mem::take(&mut cur)),
            c => cur.push(c),
        }
    }
    if !text.is_empty() {
        out.push(cur);
    }
    out
}

fn body(source: &str) -> Vec<String> {
    let mut lines = split_lines(source);
    if source.ends_with(['\r', '\n']) {
        lines.pop();
    }
    let hidden = lines
        .iter()
        .take_while(|l| l.to_ascii_lowercase().starts_with("attribute vb_"))
        .count();
    lines.drain(..hidden);
    lines
}

fn rename_attribute(source: &str, name: &str) -> String {
    let mut out = String::with_capacity(source.len());
    for l in source.split_inclusive('\n') {
        if l.starts_with("Attribute VB_Name") {
            let end = &l[l.trim_end_matches(['\r', '\n']).len()..];
            out.push_str(&format!("Attribute VB_Name = \"{name}\"{end}"));
        } else {
            out.push_str(l);
        }
    }
    out
}

fn starts_procedure(line: &str) -> bool {
    let mut words = line.split_whitespace().map(str::to_ascii_lowercase);
    let mut w = words.next();
    while matches!(
        w.as_deref(),
        Some("public" | "private" | "friend" | "static")
    ) {
        w = words.next();
    }
    matches!(w.as_deref(), Some("sub" | "function" | "property"))
}

fn declaration_lines(lines: &[String]) -> usize {
    let first = lines
        .iter()
        .position(|l| starts_procedure(l))
        .unwrap_or(lines.len());
    let mut n = first;
    while n > 0 && lines[n - 1].trim().is_empty() {
        n -= 1;
    }
    n
}

impl Application<'_> {
    pub(crate) fn ide_import(&mut self, b: BookId, part: &xlcalc_xlsx::VbaPart) {
        let Ok(book) = self.book_mut(b) else { return };
        book.ide.workbook_code = part.workbook_code_name.clone();
        let ids: Vec<(SheetId, String)> =
            book.wb.sheets().map(|s| (s.id, s.name.clone())).collect();
        for (name, code) in &part.sheet_code_names {
            let id = ids.iter().find(|(_, n)| n == name).map(|(id, _)| *id);
            if let (Some(id), Some(code)) = (id, code) {
                book.ide.sheet_codes.insert(id, code.clone());
            }
        }
    }

    fn ide_sync(&mut self, b: BookId) -> Result<(), OmError> {
        let book = self.book_mut(b)?;
        let sheets: Vec<SheetId> = book.wb.sheets().map(|s| s.id).collect();
        let mut taken: Vec<String> = book
            .vba
            .modules
            .iter()
            .filter(|m| m.kind != ModuleKind::Document)
            .map(|m| m.name.to_ascii_lowercase())
            .collect();
        let wb_code = book.vba.workbook_code_name.clone();
        taken.push(
            book.ide
                .workbook_code
                .get_or_insert_with(|| wb_code.unwrap_or_else(|| "ThisWorkbook".to_string()))
                .to_ascii_lowercase(),
        );
        taken.extend(
            book.ide
                .sheet_codes
                .values()
                .chain(book.vba.sheet_code_names.iter().map(|(_, c)| c))
                .map(|c| c.to_ascii_lowercase()),
        );
        for (pos, id) in sheets.iter().enumerate() {
            if book.ide.sheet_codes.contains_key(id) {
                continue;
            }

            if let Some((_, code)) = book.vba.sheet_code_names.iter().find(|(s, _)| s == id) {
                book.ide.sheet_codes.insert(*id, code.clone());
                continue;
            }
            let mut n = pos + 1;
            while taken.contains(&format!("sheet{n}")) {
                n += 1;
            }
            taken.push(format!("sheet{n}"));
            book.ide.sheet_codes.insert(*id, format!("Sheet{n}"));
        }

        book.vba.workbook_code_name = book.ide.workbook_code.clone();
        book.vba.sheet_code_names = sheets
            .iter()
            .filter_map(|id| Some((*id, book.ide.sheet_codes.get(id)?.clone())))
            .collect();
        let names: Vec<String> = book
            .vba
            .modules
            .iter()
            .filter(|m| m.kind != ModuleKind::Document)
            .map(|m| m.name.clone())
            .collect();
        book.ide
            .uids
            .retain(|(_, n)| names.iter().any(|m| m.eq_ignore_ascii_case(n)));
        for name in names {
            if !book
                .ide
                .uids
                .iter()
                .any(|(_, n)| n.eq_ignore_ascii_case(&name))
            {
                let uid = book.ide.next_uid;
                book.ide.next_uid += 1;
                book.ide.uids.push((uid, name));
            }
        }
        Ok(())
    }

    fn components(&self, b: BookId) -> Result<Vec<Comp>, OmError> {
        let book = self.book(b)?;
        let mut out = vec![Comp::Workbook];
        out.extend(book.wb.sheets().map(|s| Comp::Sheet(s.id)));
        for m in &book.vba.modules {
            if m.kind == ModuleKind::Document {
                continue;
            }
            if let Some((uid, _)) = book
                .ide
                .uids
                .iter()
                .find(|(_, n)| n.eq_ignore_ascii_case(&m.name))
            {
                out.push(Comp::Module(*uid));
            }
        }
        Ok(out)
    }

    fn comp_name(&self, b: BookId, c: Comp) -> Result<String, OmError> {
        let book = self.book(b)?;
        match c {
            Comp::Workbook => Ok(book.ide.workbook_code.clone().unwrap_or_default()),
            Comp::Sheet(s) => book
                .ide
                .sheet_codes
                .get(&s)
                .filter(|_| book.wb.sheet(s).is_some())
                .cloned()
                .ok_or_else(OmError::object_required),
            Comp::Module(uid) => book
                .ide
                .uids
                .iter()
                .find(|(u, _)| *u == uid)
                .map(|(_, n)| n.clone())
                .ok_or_else(OmError::object_required),
        }
    }

    fn comp_type(&self, b: BookId, c: Comp) -> Result<i32, OmError> {
        match c {
            Comp::Workbook | Comp::Sheet(_) => Ok(DOCUMENT),
            Comp::Module(_) => {
                let name = self.comp_name(b, c)?;
                let m = self.module_of(b, &name)?;
                Ok(if m.kind == ModuleKind::Class {
                    CLASS
                } else {
                    STD
                })
            }
        }
    }

    fn module_of(&self, b: BookId, name: &str) -> Result<&SourceModule, OmError> {
        self.book(b)?
            .vba
            .modules
            .iter()
            .find(|m| m.name.eq_ignore_ascii_case(name))
            .ok_or_else(OmError::object_required)
    }

    fn comp_lines(&self, b: BookId, c: Comp) -> Result<Vec<String>, OmError> {
        let name = self.comp_name(b, c)?;
        Ok(self
            .book(b)?
            .vba
            .modules
            .iter()
            .find(|m| m.name.eq_ignore_ascii_case(&name))
            .map(|m| body(&m.source))
            .unwrap_or_default())
    }

    fn set_comp_lines(&mut self, b: BookId, c: Comp, lines: &[String]) -> Result<(), OmError> {
        let name = self.comp_name(b, c)?;

        let mut source = lines.join("\r\n");
        if !lines.is_empty() {
            source.push_str("\r\n");
        }
        let book = self.book_mut(b)?;
        match book
            .vba
            .modules
            .iter_mut()
            .find(|m| m.name.eq_ignore_ascii_case(&name))
        {
            Some(m) => m.source = source,
            None => book.vba.modules.push(SourceModule {
                name,
                kind: ModuleKind::Document,
                source,
            }),
        }
        book.ide.changed = true;
        self.compile_vba(b)?;
        Ok(())
    }

    fn name_free(&self, b: BookId, name: &str, except: Option<Comp>) -> Result<bool, OmError> {
        for c in self.components(b)? {
            if Some(c) != except && self.comp_name(b, c)?.eq_ignore_ascii_case(name) {
                return Ok(false);
            }
        }
        Ok(true)
    }

    fn rename_comp(&mut self, b: BookId, c: Comp, name: &str) -> Result<(), OmError> {
        if !valid_name(name) {
            return Err(invalid_name());
        }
        if !self.name_free(b, name, Some(c))? {
            return Err(name_conflict());
        }
        let old = self.comp_name(b, c)?;
        let book = self.book_mut(b)?;
        match c {
            Comp::Workbook => book.ide.workbook_code = Some(name.to_string()),
            Comp::Sheet(s) => {
                book.ide.sheet_codes.insert(s, name.to_string());
            }
            Comp::Module(uid) => {
                if let Some(e) = book.ide.uids.iter_mut().find(|(u, _)| *u == uid) {
                    e.1 = name.to_string();
                }
            }
        }
        if let Some(m) = book
            .vba
            .modules
            .iter_mut()
            .find(|m| m.name.eq_ignore_ascii_case(&old))
        {
            m.name = name.to_string();
        }
        book.ide.changed = true;
        self.compile_vba(b)?;
        Ok(())
    }

    pub(crate) fn code_name(
        &mut self,
        b: BookId,
        sheet: Option<SheetId>,
    ) -> Result<String, OmError> {
        self.ide_sync(b)?;
        self.comp_name(b, sheet.map_or(Comp::Workbook, Comp::Sheet))
    }

    pub(crate) fn vba_write(&mut self, b: BookId) -> Result<Option<VbaWrite>, OmError> {
        self.ide_sync(b)?;
        let book = self.book(b)?;
        if book.vba.modules.is_empty() {
            return Ok(None);
        }
        let kept = book
            .package
            .as_ref()
            .is_some_and(|p| p.part("/xl/vbaProject.bin").is_some());
        if kept && !book.ide.changed {
            return Ok(None);
        }
        let mut sources = Vec::new();
        for c in self.components(b)? {
            let name = self.comp_name(b, c)?;
            let (kind, base) = match c {
                Comp::Workbook => (ModuleKind::Document, Some(WORKBOOK_BASE)),
                Comp::Sheet(_) => (ModuleKind::Document, Some(SHEET_BASE)),
                Comp::Module(_) => (self.module_of(b, &name)?.kind, None),
            };
            let pkind = match kind {
                ModuleKind::Standard => xlcalc_vbaproject::ModuleKind::Standard,
                ModuleKind::Class => xlcalc_vbaproject::ModuleKind::Class,
                ModuleKind::Document => xlcalc_vbaproject::ModuleKind::Document,
            };
            let mut source = header(pkind, &name, base);
            for l in self.comp_lines(b, c)? {
                source.push_str(&l);
                source.push_str("\r\n");
            }
            sources.push(ModuleSource {
                name,
                kind: pkind,
                source,
            });
        }
        let book = self.book(b)?;
        let project = build(&book.ide.project, &sources)
            .map_err(|e| OmError::app_defined_with(&e.to_string()))?;
        let sheet_code_names = book
            .wb
            .sheets()
            .map(|s| book.ide.sheet_codes.get(&s.id).cloned().unwrap_or_default())
            .collect();
        Ok(Some(VbaWrite {
            project,
            workbook_code_name: book.ide.workbook_code.clone().unwrap_or_default(),
            sheet_code_names,
        }))
    }

    fn component_handle(&mut self, b: BookId, c: Comp) -> Variant {
        self.alloc(Object::VBComponent(b, c))
    }

    fn component_arg(&self, b: BookId, v: Option<&Variant>) -> Result<Comp, OmError> {
        match v {
            Some(Variant::Dispatch(Some(id))) => match self.arena.get(*id) {
                Some(Object::VBComponent(ob, c)) if *ob == b => Ok(*c),
                _ => Err(OmError::type_mismatch()),
            },
            _ => Err(OmError::type_mismatch()),
        }
    }

    pub(crate) fn vbproject_member(
        &mut self,
        b: BookId,
        m: &'static Member,
        a: &Args,
    ) -> Result<Variant, OmError> {
        use crate::typelib::InvokeKind::*;
        self.ide_sync(b)?;
        match (m.name, m.kind) {
            ("VBComponents", Get) => Ok(self.alloc(Object::VBComponents(b))),
            ("Name", Get) => Ok(Variant::Bstr(self.book(b)?.ide.project.clone())),
            ("Name", Put) => {
                let name = self.put_string(a)?;
                if !valid_name(&name) {
                    return Err(invalid_name());
                }
                let book = self.book_mut(b)?;
                book.ide.project = name;
                book.ide.changed = true;
                Ok(Variant::Empty)
            }

            ("Type", Get) => Ok(Variant::I4(100)),
            ("Protection", Get) => Ok(Variant::I4(0)),
            ("Mode", Get) => Ok(Variant::I4(2)),
            ("Description" | "HelpFile" | "BuildFileName", Get) => Ok(Variant::Bstr(String::new())),
            ("HelpContextID", Get) => Ok(Variant::I4(0)),
            ("FileName", Get) => Ok(Variant::Bstr(self.book(b)?.path.clone())),
            ("Saved", Get) => Ok(Variant::Bool(!self.book(b)?.ide.changed)),
            _ => Err(OmError::unsupported(&format!("VBProject.{}", m.name))),
        }
    }

    pub(crate) fn vbcomponents_member(
        &mut self,
        b: BookId,
        m: &'static Member,
        a: &Args,
    ) -> Result<Variant, OmError> {
        use crate::typelib::InvokeKind::*;
        self.ide_sync(b)?;
        match (m.name, m.kind) {
            ("Count", Get) => Ok(Variant::I4(self.components(b)?.len() as i32)),
            ("Item", Func | Get) => {
                let comps = self.components(b)?;
                let c = match a.get(0) {
                    Some(Variant::Bstr(name)) => {
                        let mut found = None;
                        for c in &comps {
                            if self.comp_name(b, *c)?.eq_ignore_ascii_case(name) {
                                found = Some(*c);
                            }
                        }
                        found
                    }
                    Some(_) => {
                        let i = a.i32(0)?.unwrap_or(0);
                        usize::try_from(i)
                            .ok()
                            .and_then(|i| i.checked_sub(1))
                            .and_then(|i| comps.get(i).copied())
                    }
                    None => return Err(OmError::hresult(crate::error::DISP_E_PARAMNOTFOUND)),
                };
                let c = c.ok_or_else(bad_index)?;
                Ok(self.component_handle(b, c))
            }
            ("_NewEnum", Func) => {
                let mut items = Vec::new();
                for c in self.components(b)? {
                    items.push(self.component_handle(b, c));
                }
                Ok(self.alloc_enumerator(items))
            }
            ("Add", Func) => {
                let kind = a.i32(0)?.ok_or_else(OmError::type_mismatch)?;
                let (prefix, mk) = match kind {
                    STD => ("Module", ModuleKind::Standard),
                    CLASS => ("Class", ModuleKind::Class),
                    3 => return Err(OmError::unsupported("adding a UserForm")),
                    _ => return Err(OmError::exception(vba_scode(440), None)),
                };
                let mut n = 1;
                while !self.name_free(b, &format!("{prefix}{n}"), None)? {
                    n += 1;
                }
                let book = self.book_mut(b)?;
                book.vba.modules.push(SourceModule {
                    name: format!("{prefix}{n}"),
                    kind: mk,
                    source: String::new(),
                });
                book.ide.changed = true;
                self.compile_vba(b)?;
                self.ide_sync(b)?;
                let uid = self
                    .book(b)?
                    .ide
                    .uids
                    .iter()
                    .find(|(_, name)| name.eq_ignore_ascii_case(&format!("{prefix}{n}")))
                    .map(|(u, _)| *u)
                    .ok_or_else(OmError::app_defined)?;
                Ok(self.component_handle(b, Comp::Module(uid)))
            }
            ("Remove", Func) => {
                let c = self.component_arg(b, a.get(0))?;
                let Comp::Module(_) = c else {
                    return Err(range_error());
                };
                let name = self.comp_name(b, c)?;
                let book = self.book_mut(b)?;
                book.vba
                    .modules
                    .retain(|m| !m.name.eq_ignore_ascii_case(&name));
                book.ide.changed = true;
                self.compile_vba(b)?;
                Ok(Variant::Empty)
            }
            ("Import", Func) => {
                let path = a.string(0)?.ok_or_else(OmError::type_mismatch)?;
                self.import_component(b, &path)
            }
            ("Parent", Get) => Ok(self.alloc(Object::VBProject(b))),
            _ => Err(OmError::unsupported(&format!("VBComponents.{}", m.name))),
        }
    }

    pub(crate) fn vbcomponent_member(
        &mut self,
        b: BookId,
        c: Comp,
        m: &'static Member,
        a: &Args,
    ) -> Result<Variant, OmError> {
        use crate::typelib::InvokeKind::*;
        self.ide_sync(b)?;
        self.comp_name(b, c)?;
        match (m.name, m.kind) {
            ("Name", Get) => Ok(Variant::Bstr(self.comp_name(b, c)?)),
            ("Name", Put) => {
                let name = self.put_string(a)?;
                self.rename_comp(b, c, &name)?;
                Ok(Variant::Empty)
            }
            ("Type", Get) => Ok(Variant::I4(self.comp_type(b, c)?)),
            ("CodeModule", Get) => Ok(self.alloc(Object::CodeModule(b, c))),
            ("Collection", Get) => Ok(self.alloc(Object::VBComponents(b))),
            ("HasOpenDesigner", Get) => Ok(Variant::Bool(false)),
            ("Saved", Get) => Ok(Variant::Bool(!self.book(b)?.ide.changed)),
            _ => Err(OmError::unsupported(&format!("VBComponent.{}", m.name))),
        }
    }

    pub(crate) fn codemodule_member(
        &mut self,
        b: BookId,
        c: Comp,
        m: &'static Member,
        a: &Args,
    ) -> Result<Variant, OmError> {
        use crate::typelib::InvokeKind::*;
        self.ide_sync(b)?;
        let lines = self.comp_lines(b, c)?;
        let int = |i: usize| a.i32(i).and_then(|v| v.ok_or_else(OmError::type_mismatch));
        match (m.name, m.kind) {
            ("Name", Get) => Ok(Variant::Bstr(self.comp_name(b, c)?)),
            ("Name", Put) => {
                let name = self.put_string(a)?;
                self.rename_comp(b, c, &name)?;
                Ok(Variant::Empty)
            }
            ("Parent", Get) => Ok(self.alloc(Object::VBComponent(b, c))),
            ("CountOfLines", Get) => Ok(Variant::I4(lines.len() as i32)),
            ("CountOfDeclarationLines", Get) => Ok(Variant::I4(declaration_lines(&lines) as i32)),
            ("Lines", Func | Get) => {
                let (start, count) = (int(0)?, int(1)?);
                if start < 1 || count < 1 {
                    return Err(range_error());
                }
                let (start, count) = (start as usize, count as usize);
                if start > lines.len() {
                    return Ok(Variant::Bstr(String::new()));
                }
                let end = lines.len().min(start + count - 1);
                let mut text = lines[start - 1..end].join("\r\n");
                if start + count - 1 > lines.len() {
                    text.push_str("\r\n");
                }
                Ok(Variant::Bstr(text))
            }
            ("AddFromString", Func) => {
                let text = a.string(0)?.ok_or_else(OmError::type_mismatch)?;
                let new = split_lines(&text);
                if new.is_empty() {
                    return Ok(Variant::Empty);
                }
                let at = declaration_lines(&lines);
                let mut all = lines;
                all.splice(at..at, new);
                self.set_comp_lines(b, c, &all)?;
                Ok(Variant::Empty)
            }
            ("InsertLines", Func) => {
                let line = int(0)?;
                let text = a.string(1)?.ok_or_else(OmError::type_mismatch)?;
                if line < 1 {
                    return Err(range_error());
                }
                let at = (line as usize - 1).min(lines.len());
                let mut all = lines;
                all.splice(at..at, split_lines(&text));
                self.set_comp_lines(b, c, &all)?;
                Ok(Variant::Empty)
            }
            ("ReplaceLine", Func) => {
                let line = int(0)?;
                let text = a.string(1)?.ok_or_else(OmError::type_mismatch)?;
                if line < 1 || line as usize > lines.len() {
                    return Err(range_error());
                }
                let at = line as usize - 1;
                let mut new = split_lines(&text);
                if new.is_empty() {
                    new.push(String::new());
                }
                let mut all = lines;
                all.splice(at..=at, new);
                self.set_comp_lines(b, c, &all)?;
                Ok(Variant::Empty)
            }
            ("DeleteLines", Func) => {
                let start = int(0)?;
                let count = a.i32(1)?.unwrap_or(1);
                if start < 1 {
                    return Err(OmError::exception(0x8007_0057_u32 as i32, None));
                }
                if count < 1 {
                    return Err(range_error());
                }
                let (start, count) = (start as usize, count as usize);
                if start > lines.len() || start + count - 1 > lines.len() {
                    return Err(range_error());
                }
                let mut all = lines;
                all.drain(start - 1..start - 1 + count);
                self.set_comp_lines(b, c, &all)?;
                Ok(Variant::Empty)
            }
            _ => Err(OmError::unsupported(&format!("CodeModule.{}", m.name))),
        }
    }

    fn import_component(&mut self, b: BookId, path: &str) -> Result<Variant, OmError> {
        let bytes = self
            .fs
            .read(path)
            .map_err(|_| OmError::exception(vba_scode(53), None))?;

        let source = String::from_utf8(bytes)
            .unwrap_or_else(|e| e.into_bytes().iter().map(|&c| char::from(c)).collect());
        let first = source.lines().find(|l| !l.trim().is_empty()).unwrap_or("");
        let kind = if first.to_ascii_uppercase().starts_with("VERSION") {
            ModuleKind::Class
        } else {
            ModuleKind::Standard
        };
        let stated = source
            .lines()
            .find_map(|l| {
                let rest = l.strip_prefix("Attribute VB_Name")?;
                Some(rest.trim().trim_start_matches('=').trim().trim_matches('"'))
            })
            .filter(|n| valid_name(n))
            .unwrap_or(if kind == ModuleKind::Class {
                "Class1"
            } else {
                "Module1"
            })
            .to_string();
        let mut name = stated.clone();
        let mut n = 1;
        while !self.name_free(b, &name, None)? {
            name = format!("{stated}{n}");
            n += 1;
        }
        let source = if name == stated {
            source
        } else {
            rename_attribute(&source, &name)
        };
        self.add_vba_module(
            b,
            SourceModule {
                name: name.clone(),
                kind,
                source,
            },
        )?;
        self.book_mut(b)?.ide.changed = true;
        self.ide_sync(b)?;
        let uid = self
            .book(b)?
            .ide
            .uids
            .iter()
            .find(|(_, n)| n.eq_ignore_ascii_case(&name))
            .map(|(u, _)| *u)
            .ok_or_else(OmError::app_defined)?;
        Ok(self.component_handle(b, Comp::Module(uid)))
    }

    pub(crate) fn vbproject_handle(&mut self, b: BookId) -> Result<Variant, OmError> {
        self.book(b)?;
        Ok(self.alloc(Object::VBProject(b)))
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn lines_split_on_every_line_end() {
        assert_eq!(split_lines("a\r\nb\nc\rd"), ["a", "b", "c", "d"]);
        assert_eq!(split_lines("T1\r\n"), ["T1", ""]);
        assert!(split_lines("").is_empty());
        assert_eq!(
            body("Attribute VB_Name = \"M\"\r\nSub A()\r\nEnd Sub\r\n"),
            ["Sub A()", "End Sub"]
        );
    }

    #[test]
    fn declarations_stop_before_the_first_procedure() {
        let l = |s: &str| split_lines(s);
        assert_eq!(
            declaration_lines(&l("Option Explicit\r\nDim x\r\n\r\nSub A()")),
            2
        );
        assert_eq!(
            declaration_lines(&l("Public Function F()\r\nEnd Function")),
            0
        );
        assert_eq!(declaration_lines(&l("Option Explicit")), 1);
    }

    #[test]
    fn names_follow_the_vbe_rules() {
        assert!(valid_name("Mod_A") && valid_name(&"y".repeat(31)));
        assert!(!valid_name(&"y".repeat(32)));
        assert!(!valid_name("1a") && !valid_name("a b") && !valid_name("") && !valid_name("Sub"));
    }
}
