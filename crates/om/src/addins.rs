use std::sync::Arc;

use xlcalc::{Arg, CallContext, FunctionHost, RuntimeError, Signature, Value};
use xlcalc_variant::Variant;

use crate::app::{unsupported, Application, BookId, Object};
use crate::conv::Args;
use crate::error::OmError;
use crate::typelib::{InvokeKind, Member};
use crate::vba::{ChainSig, VbaHost};

pub(crate) const LIBRARY: &str = r"C:\Program Files\Microsoft Office\Root\Office16\LIBRARY";

const EUROTOOL: &str = "EUROTOOL.XLAM";

#[derive(Clone)]
pub(crate) struct DocProps {
    pub title: String,
    pub comments: String,
    pub subject: String,
    pub author: String,
    pub keywords: String,

    pub password: bool,
}

impl Default for DocProps {
    fn default() -> Self {
        DocProps {
            title: String::new(),
            comments: String::new(),
            subject: String::new(),
            author: "zac".into(),
            keywords: String::new(),
            password: false,
        }
    }
}

impl DocProps {
    pub(crate) fn slot(&mut self, name: &str) -> Option<&mut String> {
        Some(match name.to_ascii_lowercase().as_str() {
            "title" => &mut self.title,
            "subject" => &mut self.subject,
            "author" => &mut self.author,
            "keywords" => &mut self.keywords,
            "comments" => &mut self.comments,
            _ => return None,
        })
    }
}

#[derive(Clone)]
pub(crate) struct Entry {
    name: String,

    folder: String,

    full: String,
    title: String,
    comments: String,
    installed: bool,

    xll: bool,

    automation: Option<String>,
}

impl Entry {
    fn key(&self) -> String {
        if let Some(p) = &self.automation {
            return format!("auto:{}", p.to_lowercase());
        }
        self.full.to_lowercase()
    }
}

pub(crate) fn automation_entry(s: crate::comaddin::AutomationServer) -> Entry {
    Entry {
        name: s.file.clone(),
        folder: String::new(),
        full: s.file,
        title: s.title,
        comments: String::new(),
        installed: false,
        xll: false,
        automation: Some(s.prog_id),
    }
}

pub(crate) struct AddInList {
    entries: Vec<Entry>,
}

impl Default for AddInList {
    fn default() -> AddInList {
        let lib = r"C:\Program Files\Microsoft Office\root\Office16\Library";
        let full = r"C:\Program Files\Microsoft Office\Root\Office16\LIBRARY";
        let built = |name: &str, sub: &str, title: &str, xll: bool| {
            let tail = if sub.is_empty() {
                String::new()
            } else {
                format!(r"\{sub}")
            };
            Entry {
                name: name.to_string(),
                folder: format!("{lib}{tail}"),
                full: format!("{full}{}\\{name}", tail.to_uppercase()),
                title: title.to_string(),
                comments: String::new(),
                installed: false,
                xll,
                automation: None,
            }
        };
        AddInList {
            entries: vec![
                built("ANALYS32.XLL", "Analysis", "Analysis ToolPak", true),
                built("ATPVBAEN.XLAM", "Analysis", "Analysis ToolPak - VBA", false),
                built("EUROTOOL.XLAM", "", "Euro Currency Tools", false),
                built("SOLVER.XLAM", "SOLVER", "Solver Add-in", false),
            ],
        }
    }
}

struct ChainHost {
    hosts: Vec<(String, Arc<dyn FunctionHost>)>,

    own: usize,

    paths: Vec<(String, String)>,

    caller: String,
    others: Vec<(String, Arc<VbaHost>)>,
}

enum ChainMode {
    Install,

    Renew,

    Refresh,
}

enum Route<'a> {
    Host(&'a Arc<dyn FunctionHost>, &'a str),
    Other(&'a Arc<VbaHost>, &'a str),
}

impl ChainHost {
    fn route<'a>(&'a self, name: &'a str) -> Option<Route<'a>> {
        match name.rsplit_once('!') {
            Some((book, func)) => self
                .hosts
                .iter()
                .find(|(n, _)| n == book)
                .map(|(_, h)| Route::Host(h, func))
                .or_else(|| self.other(book).map(|(_, h)| Route::Other(h, func))),

            None => self.hosts[..self.own]
                .iter()
                .find(|(_, h)| h.signature(name).is_some())
                .map(|(_, h)| Route::Host(h, name)),
        }
    }

    fn other(&self, book: &str) -> Option<&(String, Arc<VbaHost>)> {
        let book = book.trim_matches('\'');
        self.others
            .iter()
            .find(|(n, _)| n.eq_ignore_ascii_case(book))
    }
}

impl FunctionHost for ChainHost {
    fn display(&self, name: &str) -> Option<String> {
        let (book, _) = name.rsplit_once('!')?;
        if self.hosts.iter().any(|(n, _)| n == book) || self.other(book).is_some() {
            return None;
        }
        let (_, full) = self.paths.iter().find(|(n, _)| n == book)?;
        Some(format!("'{full}'!"))
    }

    fn bind(&self, name: &str) -> Option<String> {
        if let Some((book, func)) = name.rsplit_once('!') {
            return self.hosts[self.own..]
                .iter()
                .any(|(n, h)| n == book && h.signature(func).is_some())
                .then(|| name.to_string());
        }
        if self.hosts[..self.own]
            .iter()
            .any(|(_, h)| h.signature(name).is_some())
        {
            return None;
        }
        self.hosts[self.own..]
            .iter()
            .find(|(_, h)| h.signature(name).is_some())
            .map(|(book, _)| format!("{book}!{name}"))
    }

    fn macro_callable(&self, name: &str) -> bool {
        match self.route(name) {
            Some(Route::Host(h, func)) => h.macro_callable(func),
            _ => true,
        }
    }

    fn refused_after_host_call(&self, name: &str) -> bool {
        match self.route(name) {
            Some(Route::Host(h, func)) => h.refused_after_host_call(func),
            Some(Route::Other(h, func)) => h.refused_after_host_call(func),
            None => true,
        }
    }

    fn signature(&self, name: &str) -> Option<Signature> {
        match self.route(name)? {
            Route::Host(h, func) => h.signature(func),
            Route::Other(h, func) => h.signature(func),
        }
    }

    fn call(
        &self,
        name: &str,
        args: &[Arg],
        cx: &mut CallContext<'_>,
    ) -> Result<Value, RuntimeError> {
        match self.route(name) {
            Some(Route::Host(h, func)) => h.call(func, args, cx),
            Some(Route::Other(h, func)) => h.call_from(func, args, cx, Some(&self.caller)),
            None => Err(RuntimeError),
        }
    }
}

fn unable_to_get(property: &str, class: &str) -> OmError {
    OmError::app_defined_with(&format!(
        "Unable to get the {property} property of the {class} class"
    ))
}

fn core_property(core: &str, tag: &str) -> String {
    let open = format!("<{tag}>");
    let close = format!("</{tag}>");
    let Some(i) = core.find(&open) else {
        return String::new();
    };
    let rest = &core[i + open.len()..];
    let text = rest.find(&close).map_or("", |j| &rest[..j]);
    text.replace("&lt;", "<")
        .replace("&gt;", ">")
        .replace("&quot;", "\"")
        .replace("&apos;", "'")
        .replace("&amp;", "&")
}

pub(crate) fn doc_props(package: &xlcalc_xlsx::Package) -> DocProps {
    let core = package
        .part("docProps/core.xml")
        .map(|b| String::from_utf8_lossy(b).into_owned())
        .unwrap_or_default();
    DocProps {
        title: core_property(&core, "dc:title"),
        comments: core_property(&core, "dc:description"),
        subject: core_property(&core, "dc:subject"),
        author: core_property(&core, "dc:creator"),
        keywords: core_property(&core, "cp:keywords"),
        password: false,
    }
}

impl<'v> Application<'v> {
    fn addin_entry(&self, key: &str) -> Result<&Entry, OmError> {
        self.addins
            .entries
            .iter()
            .find(|e| e.key() == key)
            .ok_or_else(OmError::bad_index)
    }

    fn addin_book(&self, e: &Entry) -> Option<BookId> {
        self.books
            .iter()
            .find(|b| b.is_addin && b.path.eq_ignore_ascii_case(&e.full))
            .map(|b| b.id)
    }

    pub(crate) fn addins_member(
        &mut self,
        m: &'static Member,
        a: &Args,
    ) -> Result<Variant, OmError> {
        match (m.name, m.kind) {
            ("Count", InvokeKind::Get) => Ok(Variant::I4(self.addins.entries.len() as i32)),
            ("Item" | "_Default", InvokeKind::Get) => {
                let i = self.item_index(a.get(0), a.lcid, &self.addins.entries, |e| &e.title)?;
                let key = self.addins.entries[i].key();
                Ok(self.alloc(Object::AddIn(key)))
            }
            ("_NewEnum", InvokeKind::Get) => {
                let keys: Vec<String> = self.addins.entries.iter().map(Entry::key).collect();
                let items = keys
                    .into_iter()
                    .map(|k| self.alloc(Object::AddIn(k)))
                    .collect();
                Ok(self.alloc_enumerator(items))
            }
            ("Add", InvokeKind::Func) => self.addins_add(a),
            ("Application" | "Parent", InvokeKind::Get) => {
                Ok(Variant::Dispatch(Some(self.handle())))
            }
            ("Creator", InvokeKind::Get) => Ok(Variant::I4(0x5843_454C_u32 as i32)),
            _ => Err(unsupported("AddIns", m)),
        }
    }

    fn addins_add(&mut self, a: &Args) -> Result<Variant, OmError> {
        let failed = || OmError::app_defined_with("Add method of AddIns class failed");
        let file = a.string(0)?.unwrap_or_default();
        if self.books.is_empty() {
            return Err(OmError::app_defined_with(
                "Unable to get the Add property of the AddIns class",
            ));
        }
        let absolute = file.len() > 2 && (file.as_bytes()[1] == b':' || file.starts_with(r"\\"));
        let lower = file.to_lowercase();
        let known_ext = [".xlam", ".xla", ".xll", ".dll"]
            .iter()
            .any(|e| lower.ends_with(e));
        if !absolute || !known_ext || !self.fs.exists(&file) {
            return Err(failed());
        }
        let full = self.fs.full_path(&file).ok_or_else(failed)?;
        if let Some(e) = self
            .addins
            .entries
            .iter()
            .find(|e| e.full.eq_ignore_ascii_case(&full))
        {
            let key = e.key();
            return Ok(self.alloc(Object::AddIn(key)));
        }
        let native = lower.ends_with(".xll") || lower.ends_with(".dll");
        let props = if native {
            let stem = crate::files::file_name(&full);
            let stem = stem.rsplit_once('.').map_or(stem.as_str(), |x| x.0);
            let mut title = String::new();
            let mut up = true;
            for c in stem.chars() {
                if c.is_alphabetic() {
                    if up {
                        title.extend(c.to_uppercase());
                    } else {
                        title.extend(c.to_lowercase());
                    }
                    up = false;
                } else {
                    title.push(c);
                    up = true;
                }
            }
            DocProps {
                title,
                comments: String::new(),
                ..DocProps::default()
            }
        } else {
            let bytes = self.fs.read(&full).map_err(|_| failed())?;
            xlcalc_xlsx::Package::read(&bytes)
                .map(|p| doc_props(&p))
                .map_err(|_| failed())?
        };
        if let Some(e) = self
            .addins
            .entries
            .iter()
            .find(|e| !props.title.is_empty() && e.title.eq_ignore_ascii_case(&props.title))
        {
            let key = e.key();
            return Ok(self.alloc(Object::AddIn(key)));
        }
        let name = crate::files::file_name(&full);
        let entry = Entry {
            folder: crate::files::folder(&full),
            full,
            title: props.title,
            comments: props.comments,
            installed: false,
            xll: native,
            automation: None,
            name,
        };
        let key = entry.key();
        let at = self
            .addins
            .entries
            .partition_point(|e| e.title.to_lowercase() <= entry.title.to_lowercase());
        self.addins.entries.insert(at, entry);
        Ok(self.alloc(Object::AddIn(key)))
    }

    pub(crate) fn addin_member(
        &mut self,
        key: &str,
        m: &'static Member,
        a: &Args,
    ) -> Result<Variant, OmError> {
        let e = self.addin_entry(key)?.clone();
        match (m.name, m.kind) {
            ("Name", InvokeKind::Get) => Ok(Variant::Bstr(e.name)),
            ("Title", InvokeKind::Get) => Ok(Variant::Bstr(e.title)),
            ("Path", InvokeKind::Get) => Ok(Variant::Bstr(e.folder)),
            ("FullName", InvokeKind::Get) => Ok(Variant::Bstr(e.full)),
            ("Comments", InvokeKind::Get) if e.xll => Err(unable_to_get("Comments", "AddIn")),
            ("Comments", InvokeKind::Get) => Ok(Variant::Bstr(e.comments)),
            ("Installed", InvokeKind::Get) => Ok(Variant::Bool(e.installed)),
            ("IsOpen", InvokeKind::Get) => Ok(Variant::Bool(
                self.addin_book(&e).is_some() || (e.installed && e.name == EUROTOOL),
            )),
            ("Application" | "Parent", InvokeKind::Get) => {
                Ok(Variant::Dispatch(Some(self.handle())))
            }
            ("Creator", InvokeKind::Get) => Ok(Variant::I4(0x5843_454C_u32 as i32)),
            ("Installed", InvokeKind::Put) => {
                let on = self.put_bool(a)?;
                self.set_installed(key, on)?;
                Ok(Variant::Empty)
            }
            _ => Err(unsupported("AddIn", m)),
        }
    }

    fn set_installed(&mut self, key: &str, on: bool) -> Result<(), OmError> {
        let e = self.addin_entry(key)?.clone();
        if e.installed == on {
            return Ok(());
        }
        if let Some(x) = self.addins.entries.iter_mut().find(|x| x.key() == key) {
            x.installed = on;
        }
        if e.xll {
            #[cfg(windows)]
            {
                let lower = e.full.to_lowercase();
                if on {
                    self.xll_off.retain(|p| *p != lower);
                    if !self.load_xll(&e.full) {
                        return Err(OmError::app_defined());
                    }
                } else if !self.xll_off.contains(&lower) {
                    self.xll_off.push(lower);
                }
                self.refresh_function_hosts(on);
            }
            return Ok(());
        }
        if let Some(prog_id) = &e.automation {
            if on {
                self.start_automation(prog_id)?;
            }
            self.refresh_function_hosts(on);
            return Ok(());
        }
        if on {
            self.open_addin(&e)?;
        } else {
            self.close_addin(&e);
        }
        self.refresh_function_hosts(on);
        Ok(())
    }

    pub(crate) fn addin_installed(&self, title: &str) -> bool {
        self.addins
            .entries
            .iter()
            .any(|e| e.installed && e.title.eq_ignore_ascii_case(title))
    }

    pub(crate) fn add_listed_addin(&mut self, entry: Entry) {
        if self.addins.entries.iter().any(|e| e.key() == entry.key()) {
            return;
        }
        let at = self
            .addins
            .entries
            .partition_point(|e| e.title.to_lowercase() <= entry.title.to_lowercase());
        self.addins.entries.insert(at, entry);
    }

    fn open_addin(&mut self, e: &Entry) -> Result<(), OmError> {
        if e.xll || e.name == EUROTOOL || !self.fs.exists(&e.full) {
            return Ok(());
        }
        let active = self.active;
        let bytes = self.fs.read(&e.full).map_err(|_| OmError::app_defined())?;
        let id = self.open_bytes(bytes, &e.name, e.full.clone(), e.name.clone(), false)?;
        let b = self.book_mut(id)?;
        b.is_addin = true;
        b.saved = true;
        b.props = b.package.as_ref().map(doc_props).unwrap_or_default();
        self.active = active;
        if self.book(id)?.vba.host.is_some() {
            self.compile_vba(id)?;
        }
        self.refresh_function_hosts(true);
        if self.host.run_open_macros() {
            self.raise_workbook(id, "AddinInstall");
            self.raise_workbook(id, "Open");
            self.run_book_macro(id, "Auto_Open");
        }
        Ok(())
    }

    fn close_addin(&mut self, e: &Entry) {
        let Some(id) = self.addin_book(e) else {
            return;
        };
        if self.host.run_open_macros() {
            self.raise_workbook(id, "AddinUninstall");
            self.run_book_macro(id, "Auto_Close");
            self.raise_before_close(id);
        }
        self.books.retain(|x| x.id != id);
    }

    pub(crate) fn run_book_macro(&mut self, id: BookId, name: &str) {
        let Some(host) = self.book(id).ok().and_then(|b| b.vba.host.clone()) else {
            return;
        };
        if !matches!(
            host.project().lookup_macro(name),
            xlcalc_vba::MacroLookup::Found(_)
        ) {
            return;
        }
        let prev = self.running.replace(id);
        let _ = host.run_macro(name, &mut [], self);
        self.running = prev;
    }

    pub(crate) fn installed_addin_books(&self) -> Vec<BookId> {
        self.addins
            .entries
            .iter()
            .filter(|e| e.installed)
            .filter_map(|e| self.addin_book(e))
            .collect()
    }

    pub(crate) fn refresh_function_hosts(&mut self, dirty: bool) {
        self.apply_chains(if dirty {
            ChainMode::Install
        } else {
            ChainMode::Renew
        });
    }

    pub(crate) fn refresh_chains(&mut self) {
        self.apply_chains(ChainMode::Refresh);
    }

    fn apply_chains(&mut self, mode: ChainMode) {
        let projects: Vec<(BookId, String, Arc<VbaHost>)> = self
            .books
            .iter()
            .filter_map(|b| Some((b.id, b.name.clone(), b.vba.host.clone()?)))
            .collect();
        let addins: Vec<(BookId, String, Arc<VbaHost>)> = self
            .installed_addin_books()
            .into_iter()
            .filter_map(|id| {
                let b = self.book(id).ok()?;
                Some((id, b.name.to_uppercase(), b.vba.host.clone()?))
            })
            .collect();
        let euro = self
            .addins
            .entries
            .iter()
            .any(|e| e.installed && e.name == EUROTOOL);
        let paths: Vec<(String, String)> = self
            .addins
            .entries
            .iter()
            .map(|e| (e.name.to_uppercase(), format!("{}\\{}", e.folder, e.name)))
            .collect();
        #[cfg(windows)]
        let xll_count = self.xlls.len();
        #[cfg(not(windows))]
        let xll_count = 0;
        let auto: Option<Arc<dyn FunctionHost>> = if self.com_addins.automation.is_empty() {
            None
        } else {
            Some(Arc::new(crate::automation::AutomationHost(
                self.com_addins.automation.clone(),
            )))
        };
        for b in &mut self.books {
            let mut chain: Vec<(String, Arc<dyn FunctionHost>)> = Vec::new();
            if let Some(h) = &b.vba.host {
                chain.push((b.name.to_uppercase(), h.clone()));
            }
            if let Some(h) = &auto {
                chain.push(("\u{1}AUTOMATION".to_string(), h.clone()));
            }
            #[cfg(windows)]
            for (i, x) in self.xlls.iter().enumerate() {
                let h: Arc<dyn FunctionHost> = if self.xll_off.contains(&x.path().to_lowercase()) {
                    Arc::new(crate::xllhost::Unloaded(x.clone()))
                } else {
                    Arc::new(x.clone())
                };
                chain.push((format!("\u{1}XLL{i}"), h));
            }
            for (id, name, h) in &addins {
                if *id != b.id {
                    chain.push((name.clone(), h.clone()));
                }
            }
            if euro {
                chain.push((EUROTOOL.to_string(), Arc::new(crate::euro::EuroHost)));
            }
            let others: Vec<(String, Arc<VbaHost>)> = projects
                .iter()
                .filter(|(id, _, _)| *id != b.id)
                .map(|(_, n, h)| (n.clone(), h.clone()))
                .collect();
            let sig = ChainSig::new(&b.name, b.vba.host.clone(), others.clone());
            let host: Arc<dyn FunctionHost> = Arc::new(ChainHost {
                own: usize::from(b.vba.host.is_some()) + usize::from(auto.is_some()) + xll_count,
                hosts: chain,
                paths: paths.clone(),
                caller: b.name.clone(),
                others,
            });
            let old = b.vba.chain.replace(sig);
            let sig = b.vba.chain.as_ref();
            let crate::app::BookWb::Own(wb) = &mut b.wb else {
                continue;
            };
            match mode {
                ChainMode::Install => {
                    wb.set_function_host(Some(host));
                    wb.recalc();
                }

                ChainMode::Renew => {
                    wb.renew_function_host(host);
                    wb.dirty_name_errors();
                }
                ChainMode::Refresh => {
                    let (Some(old), Some(new)) = (&old, sig) else {
                        wb.set_function_host(Some(host));
                        continue;
                    };
                    if old.same(new) {
                        continue;
                    }

                    if std::mem::take(&mut b.vba.renew) || old.lost_only(new) {
                        wb.renew_function_host(host);
                    } else {
                        wb.set_function_host(Some(host));
                    }
                }
            }
        }
    }

    pub(crate) fn workbook_addin_member(
        &mut self,
        b: BookId,
        m: &'static Member,
        a: &Args,
    ) -> Option<Result<Variant, OmError>> {
        let r = match (m.name, m.kind) {
            ("IsAddin", InvokeKind::Get) => self.book(b).map(|bk| Variant::Bool(bk.is_addin)),
            ("IsAddin", InvokeKind::Put) => self.put_bool(a).and_then(|on| {
                self.book_mut(b)?.is_addin = on;

                if !on {
                    self.active = Some(b);
                } else if self.active == Some(b) {
                    self.active = self.books.iter().rev().find(|x| !x.is_addin).map(|x| x.id);
                }
                self.refresh_function_hosts(true);
                Ok(Variant::Empty)
            }),
            ("Title", InvokeKind::Get) => {
                self.book(b).map(|bk| Variant::Bstr(bk.props.title.clone()))
            }
            ("Comments", InvokeKind::Get) => self
                .book(b)
                .map(|bk| Variant::Bstr(bk.props.comments.clone())),
            ("Subject" | "Author" | "Keywords", InvokeKind::Get) => self.book_mut(b).map(|bk| {
                Variant::Bstr(bk.props.slot(m.name).map(|s| s.clone()).unwrap_or_default())
            }),
            ("Subject" | "Author" | "Keywords", InvokeKind::Put) => {
                self.put_text(a).and_then(|t| {
                    if let Some(slot) = self.book_mut(b)?.props.slot(m.name) {
                        *slot = t;
                    }
                    Ok(Variant::Empty)
                })
            }
            ("BuiltinDocumentProperties", InvokeKind::Get) => self
                .book(b)
                .map(|_| ())
                .map(|()| self.alloc(Object::DocProps(b, false))),
            ("Title", InvokeKind::Put) => self.put_text(a).and_then(|t| {
                self.book_mut(b)?.props.title = t;
                Ok(Variant::Empty)
            }),
            ("Comments", InvokeKind::Put) => self.put_text(a).and_then(|t| {
                self.book_mut(b)?.props.comments = t;
                Ok(Variant::Empty)
            }),
            ("HasPassword", InvokeKind::Get) => {
                self.book(b).map(|bk| Variant::Bool(bk.props.password))
            }
            ("Password", InvokeKind::Put) => self.put_text(a).and_then(|t| {
                self.book_mut(b)?.props.password = !t.is_empty();
                Ok(Variant::Empty)
            }),
            ("WritePassword", InvokeKind::Put) => Ok(Variant::Empty),
            ("Password", InvokeKind::Get) => self.book(b).map(|_| Variant::Bstr("********".into())),
            _ => return None,
        };
        Some(r)
    }

    fn put_text(&self, a: &Args) -> Result<String, OmError> {
        Ok(Application::put_value(a).string(0)?.unwrap_or_default())
    }
}

impl Application<'_> {
    pub(crate) fn doc_props_member(
        &mut self,
        b: BookId,
        custom: bool,
        m: &'static Member,
        a: &Args,
    ) -> Result<Variant, OmError> {
        match (m.name, m.kind) {
            ("Item" | "_Default", InvokeKind::Get) if !custom => {
                let name = a.string(0)?.ok_or_else(OmError::bad_index)?;
                if self.book_mut(b)?.props.slot(&name).is_none() {
                    return Err(OmError::bad_index());
                }
                Ok(self.alloc(Object::DocProp(b, name.to_ascii_lowercase())))
            }
            ("Application", InvokeKind::Get) => Ok(Variant::Dispatch(Some(self.handle()))),
            ("Parent", InvokeKind::Get) => Ok(self.alloc(Object::Workbook(b))),
            _ => Err(crate::app::unsupported("DocumentProperties", m)),
        }
    }

    pub(crate) fn doc_prop_member(
        &mut self,
        b: BookId,
        name: &str,
        m: &'static Member,
        a: &Args,
    ) -> Result<Variant, OmError> {
        match (m.name, m.kind) {
            ("Value" | "_Default", InvokeKind::Get) => {
                let v = self
                    .book_mut(b)?
                    .props
                    .slot(name)
                    .map(|s| s.clone())
                    .unwrap_or_default();
                Ok(Variant::Bstr(v))
            }
            ("Value" | "_Default", InvokeKind::Put) => {
                let t = self.put_text(a)?;
                if let Some(slot) = self.book_mut(b)?.props.slot(name) {
                    *slot = t;
                }
                Ok(Variant::Empty)
            }
            ("Name", InvokeKind::Get) => {
                let mut c = name.chars();
                let first = c.next().map(|f| f.to_ascii_uppercase()).unwrap_or(' ');
                Ok(Variant::Bstr(format!("{first}{}", c.as_str())))
            }
            ("Type", InvokeKind::Get) => Ok(Variant::I4(4)),
            ("Parent", InvokeKind::Get) => Ok(self.alloc(Object::Workbook(b))),
            ("Application", InvokeKind::Get) => Ok(Variant::Dispatch(Some(self.handle()))),
            _ => Err(crate::app::unsupported("DocumentProperty", m)),
        }
    }
}
