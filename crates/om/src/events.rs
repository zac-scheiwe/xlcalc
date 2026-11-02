use std::sync::Arc;

use xlcalc::{Notice, Rect, SheetId};
use xlcalc_variant::{ObjectId, Variant};
use xlcalc_vba::MacroLookup;

use crate::app::{Application, BookId, Mode, Object, RangeRef, Track};
use crate::conv::Args;
use crate::error::OmError;
use crate::typelib::{self, Interface, Member};

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum EventSource {
    Application,

    Workbook(BookId),

    Worksheet(BookId, SheetId),
}

impl EventSource {
    pub fn interface(&self) -> &'static Interface {
        let name = match self {
            EventSource::Application => "AppEvents",
            EventSource::Workbook(_) => "WorkbookEvents",
            EventSource::Worksheet(..) => "DocEvents",
        };
        typelib::interface(name).expect("the event interfaces are in the type library")
    }
}

pub struct Event<'a> {
    pub source: EventSource,

    pub member: &'static Member,

    pub args: &'a mut [Variant],
}

pub trait EventSink: Send + Sync {
    fn deliver(&self, app: &mut Application<'_>, event: &mut Event<'_>) -> Result<(), SinkFailed>;
}

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub struct SinkFailed;

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct HandlerError {
    pub procedure: String,
    pub number: i32,
    pub description: String,
}

#[derive(Default)]
pub(crate) struct Bus {
    sinks: Vec<(u32, EventSource, Arc<dyn EventSink>)>,
    next_cookie: u32,
    errors: Vec<HandlerError>,
}

struct VbaSink;

impl EventSink for VbaSink {
    fn deliver(&self, app: &mut Application<'_>, event: &mut Event<'_>) -> Result<(), SinkFailed> {
        let name = event.member.name;

        let doc = match event.source {
            EventSource::Worksheet(b, s) => app
                .sheet_code_name(b, s)
                .map(|m| (b, format!("{m}.Worksheet_{name}"))),
            EventSource::Workbook(b) => app
                .book(b)
                .ok()
                .map(|bk| {
                    bk.vba
                        .workbook_code_name
                        .as_deref()
                        .unwrap_or("ThisWorkbook")
                })
                .map(|m| (b, format!("{m}.Workbook_{name}"))),
            EventSource::Application => None,
        };
        if let Some((b, proc)) = doc {
            if let Some(host) = app.vba_host(b) {
                if matches!(host.project().lookup_macro(&proc), MacroLookup::Found(_)) {
                    let prev = app.running.replace(b);
                    let r = host.run_macro(&proc, event.args, app);
                    app.running = prev;
                    app.handler_done(prev);
                    if let Err(e) = r {
                        app.handler_failed(HandlerError {
                            procedure: proc,
                            number: e.number,
                            description: e.description,
                        });
                    }
                }
            }
        }

        let books: Vec<BookId> = app.books.iter().map(|b| b.id).collect();
        for b in books {
            let Some(host) = app.vba_host(b) else {
                continue;
            };
            let held = host.project().held_objects();
            let mut seen: Vec<ObjectId> = Vec::new();
            for id in held {
                if seen.contains(&id) || !app.is_source(id, event.source) {
                    continue;
                }
                seen.push(id);
                for sink in host.project().sinks(id, name) {
                    let prev = app.running.replace(b);
                    let r = host.call_sink(&sink, event.args, app);
                    app.running = prev;
                    app.handler_done(prev);
                    if let Err(e) = r {
                        app.handler_failed(HandlerError {
                            procedure: sink.procedure.clone(),
                            number: e.number,
                            description: e.description,
                        });
                    }
                }
            }
        }
        Ok(())
    }
}

#[derive(Default)]
struct Ctx {
    book: Option<BookId>,
    sheet: Option<SheetId>,

    target: Vec<Rect>,

    values: Vec<(&'static str, Variant)>,
}

impl<'v> Application<'v> {
    pub fn advise(&mut self, source: EventSource, sink: Arc<dyn EventSink>) -> u32 {
        let bus = &mut self.events;
        bus.next_cookie += 1;
        let cookie = bus.next_cookie;
        bus.sinks.push((cookie, source, sink));
        cookie
    }

    pub fn unadvise(&mut self, cookie: u32) -> bool {
        let before = self.events.sinks.len();
        self.events.sinks.retain(|(c, ..)| *c != cookie);
        self.events.sinks.len() != before
    }

    pub fn event_source(&self, handle: ObjectId) -> Option<EventSource> {
        match self.arena.get(handle)? {
            Object::Application => Some(EventSource::Application),
            Object::Workbook(b) => Some(EventSource::Workbook(*b)),
            Object::Worksheet(b, s) => Some(EventSource::Worksheet(*b, *s)),
            _ => None,
        }
    }

    pub fn advised(&self, source: EventSource) -> Vec<u32> {
        self.events
            .sinks
            .iter()
            .filter(|(_, s, _)| *s == source)
            .map(|(c, ..)| *c)
            .collect()
    }

    pub fn duplicate(&mut self, handle: ObjectId) -> Option<ObjectId> {
        let mut obj = self.arena.get(handle)?.clone();
        let ranges: &mut [RangeRef] = match &mut obj {
            Object::Range(r) => std::slice::from_mut(r),
            Object::Multi(rs) | Object::Areas(rs) => rs,
            _ => &mut [],
        };
        for r in ranges {
            if let Track::Live(_) = r.track {
                let (sheet, rect) = self.resolve(r).ok()?;
                let t = self.book_mut(r.book).ok()?.wb.track(sheet, rect);
                r.track = Track::Live(t);
            }
        }
        match self.alloc(obj) {
            Variant::Dispatch(Some(id)) => Some(id),
            _ => None,
        }
    }

    fn is_source(&self, handle: ObjectId, source: EventSource) -> bool {
        self.event_source(handle) == Some(source)
    }

    pub fn handler_failed(&mut self, e: HandlerError) {
        self.host.unhandled_error(&e);
        self.events.errors.push(e);
    }

    pub fn take_handler_errors(&mut self) -> Vec<HandlerError> {
        std::mem::take(&mut self.events.errors)
    }

    fn handler_done(&mut self, prev: Option<BookId>) {
        if prev.is_none() {
            self.display_alerts = true;
        }
    }

    pub fn flush_events(&mut self) {
        if self.view.is_some() {
            return;
        }
        let books: Vec<BookId> = self.books.iter().map(|b| b.id).collect();
        for b in books {
            let notices = match self.book_mut(b) {
                Ok(bk) if !bk.wb.notices().is_empty() => bk.wb.take_notices(),
                _ => continue,
            };
            if !self.enable_events {
                if let Ok(bk) = self.book_mut(b) {
                    bk.wb.unfired(&notices);
                }
                continue;
            }
            for n in notices {
                self.fire_notice(b, n);
            }
        }
    }

    fn fire_notice(&mut self, b: BookId, n: Notice) {
        match n {
            Notice::Changed { sheet, areas } => {
                for area in areas {
                    self.sheet_levels(b, sheet, "Change", "SheetChange", &[area]);
                }
            }
            Notice::Calculated { sheets } => {
                for s in sheets {
                    self.sheet_levels(b, s, "Calculate", "SheetCalculate", &[]);
                }
            }
            Notice::AfterCalculate => {
                self.raise(EventSource::Application, "AfterCalculate", &Ctx::default());
            }
            Notice::SheetAdded(s) => {
                let cx = Ctx {
                    book: Some(b),
                    sheet: Some(s),
                    ..Ctx::default()
                };
                self.raise(EventSource::Workbook(b), "NewSheet", &cx);
                self.raise(EventSource::Application, "WorkbookNewSheet", &cx);
            }
            Notice::SheetActivated { sheet, previous } => {
                if self.book(b).is_ok_and(|bk| bk.wb.sheet(previous).is_some()) {
                    self.sheet_levels(b, previous, "Deactivate", "SheetDeactivate", &[]);
                }
                self.sheet_levels(b, sheet, "Activate", "SheetActivate", &[]);
            }
            Notice::SelectionChanged { sheet, areas } => {
                self.sheet_levels(b, sheet, "SelectionChange", "SheetSelectionChange", &areas);
            }

            Notice::SheetDeleting(_) | Notice::SheetMoved(_) => {}
        }
    }

    fn sheet_levels(
        &mut self,
        b: BookId,
        s: SheetId,
        sheet_event: &str,
        book_event: &str,
        target: &[Rect],
    ) {
        if self.book(b).map_or(true, |bk| bk.wb.sheet(s).is_none()) {
            return;
        }
        let cx = Ctx {
            book: Some(b),
            sheet: Some(s),
            target: target.to_vec(),
            values: Vec::new(),
        };
        self.raise(EventSource::Worksheet(b, s), sheet_event, &cx);
        self.raise(EventSource::Workbook(b), book_event, &cx);
        self.raise(EventSource::Application, book_event, &cx);
    }

    fn raise(&mut self, source: EventSource, name: &str, cx: &Ctx) -> Vec<Variant> {
        let Some(member) = source.interface().named(name).first() else {
            return Vec::new();
        };
        if !self.enable_events {
            return Vec::new();
        }
        let mut made = Vec::new();
        let mut args: Vec<Variant> = member
            .params
            .iter()
            .map(|p| self.event_arg(p.name, cx, &mut made))
            .collect();

        let sinks: Vec<Arc<dyn EventSink>> = self
            .events
            .sinks
            .iter()
            .rev()
            .filter(|(_, s, _)| *s == source)
            .map(|(.., k)| k.clone())
            .collect();
        let mut failed = false;
        for sink in sinks {
            let mut ev = Event {
                source,
                member,
                args: &mut args,
            };
            failed |= sink.deliver(self, &mut ev).is_err();
        }
        if !failed {
            let mut ev = Event {
                source,
                member,
                args: &mut args,
            };

            let _ = VbaSink.deliver(self, &mut ev);
        }

        let kept: Vec<ObjectId> = self
            .books
            .iter()
            .filter_map(|b| b.vba.host.clone())
            .flat_map(|h| h.project().held_objects())
            .collect();
        for id in made {
            if !kept.contains(&id) {
                self.release(id);
            }
        }
        args
    }

    fn event_arg(&mut self, param: &str, cx: &Ctx, made: &mut Vec<ObjectId>) -> Variant {
        let v = match param {
            "Sh" => match (cx.book, cx.sheet) {
                (Some(b), Some(s)) => self.alloc(Object::Worksheet(b, s)),
                _ => Variant::Dispatch(None),
            },
            "Wb" => match cx.book {
                Some(b) => self.alloc(Object::Workbook(b)),
                None => Variant::Dispatch(None),
            },
            "Target" => match (cx.book, cx.sheet) {
                (Some(b), Some(s)) if !cx.target.is_empty() => self
                    .new_multi(b, s, &cx.target, Mode::Cells)
                    .unwrap_or(Variant::Dispatch(None)),
                _ => Variant::Dispatch(None),
            },
            _ => cx
                .values
                .iter()
                .find(|(n, _)| *n == param)
                .map_or(Variant::Bool(false), |(_, v)| v.clone()),
        };
        if let Variant::Dispatch(Some(id)) = v {
            made.push(id);
        }
        v
    }

    pub(crate) fn sheet_code_name(&self, b: BookId, s: SheetId) -> Option<String> {
        let vba = &self.book(b).ok()?.vba;
        vba.sheet_code_names
            .iter()
            .find(|(id, _)| *id == s)
            .map(|(_, n)| n.clone())
    }

    fn ensure_code_name(&mut self, b: BookId, s: SheetId) -> Result<String, OmError> {
        if let Some(n) = self.sheet_code_name(b, s) {
            return Ok(n);
        }
        let bk = self.book_mut(b)?;
        let pos = bk.wb.sheet_position(s).ok_or_else(OmError::app_defined)? + 1;
        let taken = |n: &str, bk: &crate::app::Book<'_>| {
            bk.vba
                .modules
                .iter()
                .any(|m| m.name.eq_ignore_ascii_case(n))
                || bk
                    .vba
                    .sheet_code_names
                    .iter()
                    .any(|(_, c)| c.eq_ignore_ascii_case(n))
        };
        let name = (pos..)
            .map(|k| format!("Sheet{k}"))
            .find(|n| !taken(n, bk))
            .unwrap_or_default();
        bk.vba.sheet_code_names.push((s, name.clone()));
        Ok(name)
    }

    pub fn add_document_module(
        &mut self,
        book: BookId,
        module: &str,
        source: &str,
    ) -> Result<(String, Vec<xlcalc_vba::CompileError>), OmError> {
        let bk = self.book(book)?;
        let wb_name = bk
            .vba
            .workbook_code_name
            .clone()
            .unwrap_or_else(|| "ThisWorkbook".to_string());
        let by_code = bk
            .vba
            .sheet_code_names
            .iter()
            .find(|(_, c)| c.eq_ignore_ascii_case(module))
            .map(|(_, c)| c.clone());
        let name = if module.eq_ignore_ascii_case(&wb_name) {
            wb_name
        } else if let Some(c) = by_code {
            c
        } else {
            let s = bk
                .wb
                .sheet_by_name(module)
                .map(|s| s.id)
                .ok_or_else(OmError::bad_index)?;
            self.ensure_code_name(book, s)?
        };
        let mods = &mut self.book_mut(book)?.vba.modules;
        match mods.iter_mut().find(|m| m.name.eq_ignore_ascii_case(&name)) {
            Some(m) => {
                if !m.source.is_empty() && !m.source.ends_with('\n') {
                    m.source.push_str("\r\n");
                }
                m.source.push_str(source);
            }
            None => mods.push(xlcalc_vba::SourceModule {
                name: name.clone(),
                kind: xlcalc_vba::ModuleKind::Document,
                source: source.to_string(),
            }),
        }
        let errors = self.compile_vba(book)?;
        Ok((name, errors))
    }

    pub(crate) fn copy_document_module(&mut self, b: BookId, src: SheetId) -> Result<(), OmError> {
        let Some(code) = self.sheet_code_name(b, src) else {
            return Ok(());
        };
        let bk = self.book(b)?;
        let copy = bk.wb.active_sheet();
        let Some(source) = bk
            .vba
            .modules
            .iter()
            .find(|m| m.name.eq_ignore_ascii_case(&code))
            .map(|m| m.source.clone())
        else {
            return Ok(());
        };
        if copy == src {
            return Ok(());
        }
        let name = self.ensure_code_name(b, copy)?;
        self.book_mut(b)?
            .vba
            .modules
            .push(xlcalc_vba::SourceModule {
                name,
                kind: xlcalc_vba::ModuleKind::Document,
                source,
            });

        let bk = self.book_mut(b)?;
        let (host, _) = match &bk.vba.host {
            Some(old) => crate::vba::VbaHost::compile_appended(old, &bk.vba.modules),
            None => crate::vba::VbaHost::compile(&bk.vba.modules, &bk.name),
        };
        let host = std::sync::Arc::new(host);
        bk.vba.host = Some(host.clone());

        bk.wb.renew_function_host(host);
        Ok(())
    }

    pub(crate) fn save_with_events(
        &mut self,
        b: BookId,
        save: impl FnOnce(&mut Self) -> Result<(), OmError>,
    ) -> Result<Variant, OmError> {
        self.book(b)?;
        if !self.enable_events {
            save(self)?;
            return Ok(Variant::Bool(true));
        }
        let cancel = self.before(b, "BeforeSave", "WorkbookBeforeSave", true);
        let ok = !cancel;
        if ok {
            save(self)?;
        }
        let cx = Ctx {
            book: Some(b),
            values: vec![("Success", Variant::Bool(ok))],
            ..Ctx::default()
        };
        self.raise(EventSource::Workbook(b), "AfterSave", &cx);
        self.raise(EventSource::Application, "WorkbookAfterSave", &cx);
        Ok(Variant::Bool(ok))
    }

    fn before(&mut self, b: BookId, book_event: &str, app_event: &str, save: bool) -> bool {
        let mut cx = Ctx {
            book: Some(b),
            values: vec![("Cancel", Variant::Bool(false))],
            ..Ctx::default()
        };
        if save {
            cx.values.push(("SaveAsUI", Variant::Bool(false)));
        }
        let mut cancel = false;
        for (source, name) in [
            (EventSource::Workbook(b), book_event),
            (EventSource::Application, app_event),
        ] {
            cx.values[0].1 = Variant::Bool(cancel);
            let member = source.interface().named(name).first();
            let args = self.raise(source, name, &cx);
            let at = member.and_then(|m| m.params.iter().position(|p| p.name == "Cancel"));
            if let Some(v) = at.and_then(|i| args.get(i)) {
                cancel = truthy(v);
            }
        }
        cancel
    }

    pub(crate) fn close_with_events(&mut self, b: BookId, a: &Args) -> Result<Variant, OmError> {
        self.book(b)?;
        if self.enable_events && self.before(b, "BeforeClose", "WorkbookBeforeClose", false) {
            return Ok(Variant::Bool(true));
        }
        self.close_book(b, a)
    }

    pub(crate) fn add_with_events(&mut self, a: &Args) -> Result<Variant, OmError> {
        let r = self.workbooks_add(a)?;
        if let Some(b) = self.opened_book(&r) {
            let cx = Ctx {
                book: Some(b),
                ..Ctx::default()
            };
            self.raise(EventSource::Application, "NewWorkbook", &cx);
        }
        Ok(r)
    }

    pub(crate) fn open_with_events(&mut self, a: &Args) -> Result<Variant, OmError> {
        let before: Vec<BookId> = self.books.iter().map(|b| b.id).collect();
        let r = self.workbooks_open(a)?;
        if let Some(b) = self.opened_book(&r).filter(|b| !before.contains(b)) {
            let cx = Ctx {
                book: Some(b),
                ..Ctx::default()
            };

            if self.host.run_open_macros() {
                self.raise(EventSource::Workbook(b), "Open", &cx);
            }
            self.raise(EventSource::Application, "WorkbookOpen", &cx);
        }
        Ok(r)
    }

    pub(crate) fn count_large(&mut self, r: &RangeRef) -> Result<Variant, OmError> {
        let (_, rect) = self.resolve(r)?;
        let n = match r.mode {
            Mode::Cells => rect.cell_count(),
            Mode::Rows => u64::from(rect.rows()),
            Mode::Columns => u64::from(rect.cols()),
        };
        Ok(Variant::I8(n as i64))
    }

    pub(crate) fn range_list_object(&mut self, r: &RangeRef) -> Result<Variant, OmError> {
        let (sheet, rect) = self.resolve(r)?;
        let name = self
            .book(r.book)?
            .wb
            .table_at(xlcalc::CellAddr::new(sheet, rect.r0, rect.c0))
            .map(|t| t.name.clone());
        Ok(match name {
            Some(n) => self.alloc(Object::Table(r.book, n)),
            None => Variant::Dispatch(None),
        })
    }

    fn opened_book(&self, v: &Variant) -> Option<BookId> {
        match v {
            Variant::Dispatch(Some(id)) => self.book_of(*id),
            _ => None,
        }
    }
}

fn truthy(v: &Variant) -> bool {
    match v {
        Variant::Bool(b) => *b,
        Variant::I2(n) => *n != 0,
        Variant::I4(n) => *n != 0,
        Variant::R8(x) => *x != 0.0,
        _ => false,
    }
}
