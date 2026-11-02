use std::collections::HashMap;
use std::sync::Arc;

use xlcalc::{Arg, CallContext, CellAddr, FunctionHost, ParamKind, RuntimeError, Signature};
use xlcalc::{SheetId, Value, Workbook, XlError};
use xlcalc_variant::{change_type, HResult, Locale, ObjectId, VarType, Variant};
use xlcalc_vba::{ExcelNames, InvokeKind, MacroLookup, ObjectHost, Project, SourceModule};
use xlcalc_vba::{Sink, VbaError};

use crate::app::{flags, Application, BookId, BookWb, Call, Mode, Object, DISPID_VALUE};
use crate::conv::{array_variant, cell_variant, error_scode, scode_error, Flavour};
use crate::error::{OmError, DISP_E_BADPARAMCOUNT, DISP_E_EXCEPTION, DISP_E_MEMBERNOTFOUND};
use crate::error::{DISP_E_PARAMNOTFOUND, DISP_E_UNKNOWNNAME, OBJECT_REQUIRED};

const VBA_STACK: usize = 16 * 1024 * 1024;
const RED_ZONE: usize = 8 * 1024 * 1024;

fn deep<R>(f: impl FnOnce() -> R) -> R {
    stacker::maybe_grow(RED_ZONE, VBA_STACK, f)
}

#[derive(Default)]
pub(crate) struct BookVba {
    pub modules: Vec<SourceModule>,
    pub host: Option<Arc<VbaHost>>,

    pub workbook_code_name: Option<String>,

    pub sheet_code_names: Vec<(SheetId, String)>,
}

pub(crate) struct ViewState {
    pub caller: CellAddr,

    pub volatile: Option<bool>,
}

pub struct VbaHost {
    project: Project,

    functions: HashMap<String, Signature>,

    book_name: String,
}

impl VbaHost {
    pub fn compile(
        modules: &[SourceModule],
        book_name: &str,
    ) -> (VbaHost, Vec<xlcalc_vba::CompileError>) {
        VbaHost::from_project(Project::compile(modules, &OmNames), book_name)
    }

    pub fn compile_appended(
        old: &VbaHost,
        modules: &[SourceModule],
    ) -> (VbaHost, Vec<xlcalc_vba::CompileError>) {
        let project = Project::compile_appended(&old.project, modules, &OmNames);
        VbaHost::from_project(project, &old.book_name)
    }

    fn from_project(project: Project, book_name: &str) -> (VbaHost, Vec<xlcalc_vba::CompileError>) {
        let errors = project.errors().to_vec();
        let all = project.functions();

        let once = |name: &str| {
            all.iter()
                .filter(|f| f.name.eq_ignore_ascii_case(name))
                .count()
                == 1
        };
        let functions = all
            .iter()
            .filter(|f| once(&f.name))
            .map(|f| {
                let params = f
                    .params
                    .iter()
                    .filter(|p| !p.param_array)
                    .map(param_kind)
                    .collect();
                let rest = match f.params.last() {
                    Some(p) if p.param_array => param_kind(p),
                    _ => ParamKind::Value,
                };
                let sig = Signature {
                    params,
                    rest,

                    volatile: false,
                };
                (f.name.to_uppercase(), sig)
            })
            .collect();
        let host = VbaHost {
            project,
            functions,
            book_name: book_name.to_string(),
        };
        (host, errors)
    }

    pub fn function_names(&self) -> Vec<String> {
        let mut v: Vec<String> = self.functions.keys().cloned().collect();
        v.sort();
        v
    }

    pub fn project(&self) -> &Project {
        &self.project
    }

    fn with_objects<T>(
        &self,
        app: &mut Application<'_>,
        f: impl FnOnce(&Project, &mut Objects) -> Result<T, VbaError>,
        result_object: impl Fn(&T) -> Option<ObjectId>,
    ) -> Result<T, VbaError> {
        let mut objects = Objects::new(app);
        let r = deep(|| f(&self.project, &mut objects));
        let mut keep = self.project.held_objects();
        if let Some(id) = r.as_ref().ok().and_then(&result_object) {
            keep.push(id);
        }
        for id in std::mem::take(&mut objects.made) {
            if !keep.contains(&id) {
                objects.app.release(id);
            }
        }
        r
    }

    pub fn run_macro(
        &self,
        name: &str,
        args: &mut [Variant],
        app: &mut Application<'_>,
    ) -> Result<Variant, VbaError> {
        self.with_objects(
            app,
            |p, o| p.run_macro(name, args, o),
            |v| match v {
                Variant::Dispatch(Some(id)) => Some(*id),
                _ => None,
            },
        )
    }

    pub fn call_sink(
        &self,
        sink: &Sink,
        args: &mut [Variant],
        app: &mut Application<'_>,
    ) -> Result<(), VbaError> {
        self.with_objects(app, |p, o| p.call_sink(sink, args, o), |_| None)
    }
}

struct OmNames;

impl xlcalc_vba::Library for OmNames {
    fn constant(&self, name: &str) -> Option<Variant> {
        ExcelNames.constant(name).or_else(|| {
            crate::typelib::ENUMERATIONS
                .iter()
                .flat_map(|e| e.constants.iter())
                .find(|(n, _)| n.eq_ignore_ascii_case(name))
                .map(|&(_, v)| Variant::I4(v))
        })
    }

    fn is_global(&self, name: &str) -> bool {
        ExcelNames.is_global(name)
    }
}

fn param_kind(p: &xlcalc_vba::ParamSig) -> ParamKind {
    if p.takes_reference() {
        ParamKind::Reference
    } else {
        ParamKind::Value
    }
}

impl FunctionHost for VbaHost {
    fn signature(&self, name: &str) -> Option<Signature> {
        self.functions.get(name).cloned()
    }

    fn call(
        &self,
        name: &str,
        args: &[Arg],
        cx: &mut CallContext<'_>,
    ) -> Result<Value, RuntimeError> {
        let view = cx.view();
        let project = &self.project;
        let mut app = Application::view(view.workbook(), &self.book_name, view.caller());
        let book = app.active.ok_or(RuntimeError)?;
        let values: Vec<Variant> = args.iter().map(|a| app.arg_variant(book, a)).collect();
        let r = deep(|| project.call(name, &values, &mut Objects::new(&mut app)));
        if let Some(v) = app.view.as_ref().and_then(|v| v.volatile) {
            cx.set_volatile(v);
        }
        match r {
            Ok(v) => Ok(app.result_value(v, 0)),
            Err(_) => Err(RuntimeError),
        }
    }
}

impl<'v> Application<'v> {
    pub fn view(wb: &'v Workbook, name: &str, caller: CellAddr) -> Application<'v> {
        let mut app = Application::new();
        let id = app.push_book_wb(BookWb::View(wb), name.to_string(), String::new());
        app.view = Some(ViewState {
            caller,
            volatile: None,
        });
        app.running = Some(id);
        app
    }

    pub fn add_vba_module(
        &mut self,
        book: BookId,
        mut module: SourceModule,
    ) -> Result<Vec<xlcalc_vba::CompileError>, OmError> {
        if module.kind == xlcalc_vba::ModuleKind::Class {
            module.source = strip_class_header(&module.source);
        }
        let b = self.book_mut(book)?;
        let mods = &mut b.vba.modules;
        match mods
            .iter_mut()
            .find(|m| m.name.eq_ignore_ascii_case(&module.name))
        {
            Some(m) => *m = module,
            None => mods.push(module),
        }
        self.compile_vba(book)
    }

    pub(crate) fn compile_vba(
        &mut self,
        book: BookId,
    ) -> Result<Vec<xlcalc_vba::CompileError>, OmError> {
        let b = self.book_mut(book)?;
        let (host, errors) = VbaHost::compile(&b.vba.modules, &b.name);
        let host = Arc::new(host);
        b.vba.host = Some(host.clone());
        b.wb.set_function_host(Some(host));
        Ok(errors)
    }

    pub fn vba_host(&self, book: BookId) -> Option<Arc<VbaHost>> {
        self.book(book).ok().and_then(|b| b.vba.host.clone())
    }

    pub fn import_warnings(&self, book: BookId) -> &[String] {
        self.book(book).map_or(&[], |b| &b.warnings)
    }

    fn arg_variant(&mut self, book: BookId, a: &Arg) -> Variant {
        match a {
            Arg::Missing => Variant::Error(HResult::DISP_E_PARAMNOTFOUND.0),
            Arg::Value(v) => value_variant(v),
            Arg::Ref(areas) => match areas.first() {
                Some(area) => self
                    .new_range(book, area.sheet, area.rect, Mode::Cells)
                    .unwrap_or(Variant::Error(error_scode(XlError::REF))),
                None => Variant::Error(error_scode(XlError::REF)),
            },
        }
    }

    fn result_value(&mut self, v: Variant, depth: u32) -> Value {
        let number = |v: &Variant| match change_type(v, VarType::R8, Locale::EN_US, 0) {
            Ok(Variant::R8(n)) => Value::Number(n),
            _ => Value::Error(XlError::VALUE),
        };
        match v {
            Variant::Empty => Value::Blank,

            Variant::Null => Value::Error(XlError::NA),
            Variant::Bstr(s) => Value::Text(s.as_str().into()),
            Variant::Bool(b) => Value::Bool(b),
            Variant::Error(sc) => Value::Error(scode_error(sc).unwrap_or(XlError::VALUE)),
            Variant::Dispatch(Some(id)) if depth == 0 => {
                match self.invoke_dispid(id, DISPID_VALUE, Call::GET, &[], &[]) {
                    Ok(inner) => self.result_value(inner, depth + 1),
                    Err(_) => Value::Error(XlError::VALUE),
                }
            }
            Variant::Array(a) if depth < 2 => {
                let (rows, cols) = match a.bounds.as_slice() {
                    [(_, n)] => (1, *n),
                    [(_, r), (_, c)] => (*r, *c),
                    _ => return Value::Error(XlError::VALUE),
                };
                if rows == 0 || cols == 0 {
                    return Value::Error(XlError::VALUE);
                }
                let mut data = Vec::with_capacity(a.data.len());
                for r in 0..rows as usize {
                    for c in 0..cols as usize {
                        let e = a.data[c * rows as usize + r].clone();
                        data.push(match e {
                            Variant::Array(_) | Variant::Dispatch(_) | Variant::Unknown(_) => {
                                Value::Error(XlError::VALUE)
                            }
                            e => self.result_value(e, depth + 1),
                        });
                    }
                }
                xlcalc::Array::new(rows, cols, data).into_value()
            }
            Variant::Dispatch(_) | Variant::Unknown(_) | Variant::Array(_) => {
                Value::Error(XlError::VALUE)
            }
            other => number(&other),
        }
    }

    pub(crate) fn app_caller(&mut self) -> Result<Variant, OmError> {
        if let Some(name) = &self.action_caller {
            return Ok(Variant::Bstr(name.clone()));
        }
        let Some(caller) = self.view.as_ref().map(|v| v.caller) else {
            return Ok(Variant::Error(error_scode(XlError::REF)));
        };
        let book = self.active.ok_or_else(OmError::object_required)?;
        let wb = &self.book(book)?.wb;
        let rect = wb
            .current_array(caller)
            .unwrap_or(xlcalc::Rect::cell(caller.row, caller.col));
        self.new_range(book, caller.sheet, rect, Mode::Cells)
    }

    pub(crate) fn app_volatile(&mut self, on: bool) {
        if let Some(v) = self.view.as_mut() {
            v.volatile = Some(on);
        }
    }

    pub(crate) fn app_run(&mut self, a: &crate::conv::Args) -> Result<Variant, OmError> {
        let text = a.string(0)?.ok_or_else(OmError::app_defined)?;
        let cannot = || {
            OmError::app_defined_with(&format!(
                "Cannot run the macro '{text}'. The macro may not be available in this workbook \
                 or all macros may be disabled."
            ))
        };
        let (book, proc) = match text.rsplit_once('!') {
            Some((b, p)) => {
                let quoted = b.len() > 1 && b.starts_with('\'') && b.ends_with('\'');
                let b = b.trim_matches('\'');
                let id = self
                    .books
                    .iter()
                    .find(|x| x.name.eq_ignore_ascii_case(b))
                    .map(|x| x.id);
                let Some(id) = id else {
                    return Err(if quoted {
                        let dir = self.default_file_path.trim_end_matches(['\\', '/']);
                        OmError::app_defined_with(&format!(
                            "Sorry, we couldn't find {dir}\\{b}.xlsx. Is it possible it was \
                             moved, renamed or deleted?"
                        ))
                    } else {
                        cannot()
                    });
                };
                (id, p.to_string())
            }
            None => (self.active_book().map_err(|_| cannot())?.id, text.clone()),
        };
        {
            let wb = &self.book(book)?.wb;
            let key = proc.to_uppercase();
            if let Some(def) = wb.names().lookup(&key, wb.active_sheet()) {
                let refers = wb.name_refers_to(def);
                return if refers.trim_start().to_uppercase().starts_with("LAMBDA(") {
                    Err(cannot())
                } else {
                    Ok(Variant::Error(error_scode(XlError::VALUE)))
                };
            }
        }
        let host = self.book(book)?.vba.host.clone().ok_or_else(cannot)?;

        if !matches!(host.project().lookup_macro(&proc), MacroLookup::Found(_)) {
            return Err(cannot());
        }
        let mut args: Vec<Variant> = a.values[1..]
            .iter()
            .map(|v| {
                v.clone()
                    .unwrap_or(Variant::Error(HResult::DISP_E_PARAMNOTFOUND.0))
            })
            .collect();
        while args.last().is_some_and(crate::conv::is_missing) {
            args.pop();
        }
        let prev = self.running.replace(book);
        let r = host.run_macro(&proc, &mut args, self);
        self.running = prev;
        r.map_err(|e| {
            if e.number < 0 {
                OmError::exception(e.number, None)
            } else {
                OmError::exception(crate::error::vba_scode(e.number), Some(&e.description))
            }
        })
    }
}

impl Application<'_> {
    pub(crate) fn worksheet_function(
        &mut self,
        m: &'static crate::typelib::Member,
        a: &crate::conv::Args,
        raise: bool,
    ) -> Result<Variant, OmError> {
        let book = match self.running {
            Some(b) => b,
            None => self.active_book()?.id,
        };
        let mut args = Vec::new();
        for v in &a.values {
            args.push(match v {
                None => Arg::Missing,
                Some(v) => self.engine_arg(v)?,
            });
        }
        while matches!(args.last(), Some(Arg::Missing)) {
            args.pop();
        }
        let wb = &self.book(book)?.wb;
        let at = match &self.view {
            Some(v) => v.caller,
            None => CellAddr::new(wb.active_sheet(), 0, 0),
        };
        let name = m.name.trim_start_matches('_');
        let r = wb
            .call_function(at, name, &args)
            .ok_or_else(|| crate::app::unsupported("WorksheetFunction", m))?;
        if raise && matches!(r, Value::Error(_)) {
            return Err(OmError::app_defined_with(&format!(
                "Unable to get the {name} property of the WorksheetFunction class"
            )));
        }
        Ok(value_variant(&r))
    }

    fn engine_arg(&self, v: &Variant) -> Result<Arg, OmError> {
        if let Variant::Dispatch(Some(id)) = v {
            if let Some(crate::app::Object::Range(r)) = self.arena.get(*id) {
                let (sheet, rect) = self.resolve(r)?;
                return Ok(Arg::Ref(vec![xlcalc::Area { sheet, rect }]));
            }
            if let Some(crate::app::Object::Multi(rs)) = self.arena.get(*id) {
                let mut areas = Vec::with_capacity(rs.len());
                for r in rs {
                    let (sheet, rect) = self.resolve(r)?;
                    areas.push(xlcalc::Area { sheet, rect });
                }
                return Ok(Arg::Ref(areas));
            }
            return Err(OmError::type_mismatch());
        }
        Ok(Arg::Value(variant_value(v)?))
    }
}

fn variant_value(v: &Variant) -> Result<Value, OmError> {
    Ok(match v {
        Variant::Empty => Value::Blank,
        Variant::Bstr(s) => Value::Text(s.as_str().into()),
        Variant::Bool(b) => Value::Bool(*b),
        Variant::Error(sc) => Value::Error(scode_error(*sc).unwrap_or(XlError::VALUE)),
        Variant::Array(_) => {
            let (rows, cols, cells) =
                crate::conv::array_cells(v).ok_or_else(OmError::type_mismatch)?;
            let data = cells
                .iter()
                .map(variant_value)
                .collect::<Result<Vec<_>, _>>()?;
            xlcalc::Array::new(rows, cols, data).into_value()
        }
        other => match change_type(other, VarType::R8, Locale::EN_US, 0) {
            Ok(Variant::R8(n)) => Value::Number(n),
            _ => return Err(OmError::type_mismatch()),
        },
    })
}

pub(crate) fn import_modules(part: &xlcalc_xlsx::VbaPart) -> (Vec<SourceModule>, Vec<String>) {
    use xlcalc_vba::ModuleKind as K;
    use xlcalc_vbaproject::ModuleKind as P;
    let mut warnings = Vec::new();
    let project = match &part.project {
        Ok(p) => p,
        Err(e) => {
            warnings.push(format!(
                "vbaProject.bin cannot be read ({e}); its VBA functions are #NAME?"
            ));
            return (Vec::new(), warnings);
        }
    };
    let mut modules = Vec::new();
    for m in &project.modules {
        match &m.source {
            Ok(text) => modules.push(SourceModule {
                name: m.name.clone(),
                kind: match m.kind {
                    P::Standard => K::Standard,
                    P::Class => K::Class,
                    P::Document => K::Document,
                },
                source: text.clone(),
            }),
            Err(e) => warnings.push(format!(
                "VBA module {} cannot be read ({e}); its functions are #NAME?",
                m.name
            )),
        }
    }
    (modules, warnings)
}

fn value_variant(v: &Value) -> Variant {
    match v {
        Value::Array(a) => {
            array_variant(a.rows, a.cols, a.data.iter().map(value_variant).collect())
        }
        v => cell_variant(v, "General", Flavour::Value2, false),
    }
}

struct Objects<'a, 'v> {
    app: &'a mut Application<'v>,

    made: Vec<ObjectId>,
}

impl<'a, 'v> Objects<'a, 'v> {
    fn new(app: &'a mut Application<'v>) -> Objects<'a, 'v> {
        Objects {
            app,
            made: Vec::new(),
        }
    }
}

fn vba_error(e: OmError) -> VbaError {
    let number = match e.hresult {
        DISP_E_EXCEPTION => match e.scode as u32 {
            0x8002_0005 => 13,
            0x8002_000B => 9,
            0x8002_000A => 6,
            _ => e.number,
        },
        DISP_E_UNKNOWNNAME | DISP_E_MEMBERNOTFOUND => 438,
        DISP_E_BADPARAMCOUNT => 450,
        DISP_E_PARAMNOTFOUND => 449,
        OBJECT_REQUIRED => 424,
        h if h.0 as u32 == 0x8002_0005 => 13,
        h if h.0 as u32 == 0x8002_000A => 6,
        _ => e.number,
    };
    match e.description {
        Some(d) => VbaError {
            number,
            description: d,
        },
        None => VbaError::new(number),
    }
}

impl ObjectHost for Objects<'_, '_> {
    fn global(&mut self) -> Result<ObjectId, VbaError> {
        Ok(self.app.handle())
    }

    fn invoke(
        &mut self,
        obj: ObjectId,
        member: &str,
        kind: InvokeKind,
        positional: &[Variant],
        named: &[(&str, Variant)],
    ) -> Result<Variant, VbaError> {
        let call = Call {
            flags: match kind {
                InvokeKind::Get => flags::METHOD | flags::GET,
                InvokeKind::Put => flags::PUT,
                InvokeKind::PutRef => flags::PUTREF,
            },
            lcid: 0x0409,
        };
        let r = if member.is_empty() {
            if !named.is_empty() {
                return Err(VbaError::new(448));
            }
            self.app
                .invoke_dispid(obj, DISPID_VALUE, call, positional, &[])
        } else {
            self.app.invoke(obj, member, call, positional, named)
        };
        let v = r.map_err(vba_error)?;
        if let Variant::Dispatch(Some(id)) | Variant::Unknown(Some(id)) = v {
            self.made.push(id);
        }
        Ok(v)
    }

    fn debug_print(&mut self, items: &[xlcalc_vba::DebugItem]) {
        let line = crate::host::debug_line(items);
        self.app.vba_debug_print(&line);
    }

    fn interaction(
        &mut self,
        what: xlcalc_vba::Interaction,
        args: &[Variant],
    ) -> Result<Variant, VbaError> {
        use xlcalc_vba::Interaction;
        match what {
            Interaction::MsgBox => self.app.vba_msg_box(args),
            Interaction::InputBox => self.app.vba_input_box(args),
            Interaction::Beep => {
                self.app.vba_beep();
                Ok(Variant::Empty)
            }
        }
        .map_err(vba_error)
    }

    fn type_name(&mut self, obj: ObjectId) -> String {
        self.app
            .type_name(obj)
            .map_or("Object", |n| n.trim_start_matches('_'))
            .to_string()
    }

    fn document(&mut self, module: &str) -> Result<ObjectId, VbaError> {
        let book = match self.app.running {
            Some(b) => b,
            None => self.app.active_book().map_err(vba_error)?.id,
        };
        let vba = &self.app.book(book).map_err(vba_error)?.vba;
        let wb_name = vba.workbook_code_name.as_deref().unwrap_or("ThisWorkbook");
        let obj = if module.eq_ignore_ascii_case(wb_name) {
            Object::Workbook(book)
        } else {
            let sheet = vba
                .sheet_code_names
                .iter()
                .find(|(_, n)| n.eq_ignore_ascii_case(module))
                .map(|(s, _)| *s)
                .ok_or_else(|| VbaError::new(424))?;
            Object::Worksheet(book, sheet)
        };
        match self.app.alloc(obj) {
            Variant::Dispatch(Some(id)) => {
                self.made.push(id);
                Ok(id)
            }
            _ => Err(VbaError::new(424)),
        }
    }

    fn enumerate(&mut self, obj: ObjectId) -> Result<Vec<Variant>, VbaError> {
        let call = Call {
            flags: flags::METHOD | flags::GET,
            lcid: 0x0409,
        };
        let e = match self.app.invoke(obj, "_NewEnum", call, &[], &[]) {
            Ok(Variant::Unknown(Some(e)) | Variant::Dispatch(Some(e))) => e,
            Ok(_) => return Err(VbaError::new(438)),
            Err(e) => return Err(vba_error(e)),
        };
        let items = self.app.enum_next(e, usize::MAX);
        self.app.release(e);
        for v in &items {
            if let Variant::Dispatch(Some(id)) | Variant::Unknown(Some(id)) = v {
                self.made.push(*id);
            }
        }
        Ok(items)
    }
}

fn strip_class_header(source: &str) -> String {
    let mut lines = source.lines().peekable();
    while lines.peek().is_some_and(|l| l.trim().is_empty()) {
        lines.next();
    }
    if lines
        .peek()
        .is_some_and(|l| l.to_ascii_uppercase().starts_with("VERSION"))
    {
        lines.next();
        if lines
            .peek()
            .is_some_and(|l| l.trim().eq_ignore_ascii_case("begin"))
        {
            for l in lines.by_ref() {
                if l.trim().eq_ignore_ascii_case("end") {
                    break;
                }
            }
        }
    } else {
        return source.to_string();
    }
    let mut out = String::new();
    for l in lines {
        out.push_str(l);
        out.push_str("\r\n");
    }
    out
}

#[cfg(test)]
mod tests {
    use super::import_modules;
    use xlcalc_vbaproject::VbaError;

    fn part(project: Result<xlcalc_vbaproject::VbaProject, VbaError>) -> xlcalc_xlsx::VbaPart {
        xlcalc_xlsx::VbaPart {
            raw: Vec::new(),
            project,
            workbook_code_name: None,
            sheet_code_names: Vec::new(),
        }
    }

    #[test]
    fn unreadable_projects_and_modules_are_warnings() {
        let (m, w) = import_modules(&part(Err(VbaError::Cfb("bad header".into()))));
        assert!(m.is_empty());
        assert_eq!(w.len(), 1);
        assert!(w[0].contains("vbaProject.bin") && w[0].contains("bad header"));
        let project = xlcalc_vbaproject::VbaProject {
            name: "VBAProject".into(),
            code_page: 932,
            modules: vec![xlcalc_vbaproject::Module {
                name: "Module1".into(),
                stream_name: "Module1".into(),
                kind: xlcalc_vbaproject::ModuleKind::Standard,
                text_offset: 0,
                source: Err(VbaError::UnsupportedCodePage(932)),
            }],
        };
        let (m, w) = import_modules(&part(Ok(project)));
        assert!(m.is_empty());
        assert!(
            w[0].contains("Module1") && w[0].contains("code page 932"),
            "{w:?}"
        );
    }
}
