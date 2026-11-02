use xlcalc::{Rect, SheetId, TrackId, Workbook};
use xlcalc_variant::{HResult, ObjectId, Variant};

use crate::arena::Arena;
use crate::conv::{is_missing, Args};
use crate::error::{
    NameError, OmError, DISP_E_BADPARAMCOUNT, DISP_E_MEMBERNOTFOUND, DISP_E_PARAMNOTFOUND,
    DISP_E_PARAMNOTOPTIONAL, DISP_E_UNKNOWNNAME, RPC_E_DISCONNECTED,
};
use crate::locale::{english, foreign, Foreign};
use crate::typelib::{self, DefaultValue, Interface, InvokeKind, Member, Opt, Ty};

pub mod flags {
    pub const METHOD: u16 = 1;
    pub const GET: u16 = 2;
    pub const PUT: u16 = 4;
    pub const PUTREF: u16 = 8;
}

pub const DISPID_VALUE: i32 = 0;

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub struct Call {
    pub flags: u16,
    pub lcid: u32,
}

impl Call {
    pub const VB: Call = Call {
        flags: flags::METHOD | flags::GET,
        lcid: 0x0409,
    };
    pub const GET: Call = Call {
        flags: flags::GET,
        lcid: 0x0409,
    };
    pub const PUT: Call = Call {
        flags: flags::PUT,
        lcid: 0x0409,
    };
}

#[derive(Clone, Copy, PartialEq, Eq, Hash, Debug)]
pub struct BookId(pub(crate) u32);

pub(crate) struct Book<'v> {
    pub id: BookId,
    pub wb: BookWb<'v>,
    pub name: String,

    pub path: String,
    pub saved: bool,

    pub next_sheet: u32,

    pub next_chart: u32,

    pub vba: crate::vba::BookVba,

    pub ide: crate::vbide::Ide,

    pub warnings: Vec<String>,

    pub file_format: i32,

    pub package: Option<xlcalc_xlsx::Package>,

    pub read_only: bool,

    pub controls: crate::controls::BookControls,
}

pub(crate) enum BookWb<'v> {
    Own(Box<Workbook>),
    View(&'v Workbook),
}

impl std::ops::Deref for BookWb<'_> {
    type Target = Workbook;
    fn deref(&self) -> &Workbook {
        match self {
            BookWb::Own(wb) => wb,
            BookWb::View(wb) => wb,
        }
    }
}

impl std::ops::DerefMut for BookWb<'_> {
    fn deref_mut(&mut self) -> &mut Workbook {
        match self {
            BookWb::Own(wb) => wb,

            BookWb::View(_) => unreachable!("a read-only view workbook was borrowed mutably"),
        }
    }
}

#[derive(Clone, Copy, PartialEq, Eq, Debug)]
pub(crate) enum Mode {
    Cells,
    Rows,
    Columns,
}

#[derive(Clone, Copy, Debug)]
pub(crate) struct RangeRef {
    pub book: BookId,
    pub track: Track,
    pub mode: Mode,
}

#[derive(Clone, Copy, Debug)]
pub(crate) enum Track {
    Live(TrackId),
    Fixed(SheetId, Rect),
}

#[derive(Clone, Copy, PartialEq, Eq, Debug)]
pub(crate) enum SheetsKind {
    All,
    Worksheets,
    Charts,
}

#[derive(Clone, Debug)]
pub(crate) enum Object {
    Application,
    Workbooks,

    Windows,

    Window(BookId),
    Workbook(BookId),

    Sheets(BookId, SheetsKind),
    Worksheet(BookId, SheetId),

    Chart(BookId, SheetId),
    Range(RangeRef),

    Multi(Vec<RangeRef>),

    Areas(Vec<RangeRef>),
    Names(BookId, Option<SheetId>),

    Name(BookId, Option<SheetId>, String),
    ListObjects(BookId, SheetId),

    Table(BookId, String),
    ListColumns(BookId, String),
    ListColumn(BookId, String, usize),

    Enumerator(Vec<Variant>, usize),

    Sort(SortOwner),
    SortFields(SortOwner),

    SortField(SortOwner, usize),

    AutoFilter(BookId, SheetId),

    WorksheetFunction,

    MultiThreadedCalculation,

    Font(crate::style::StyleOwner),
    Interior(crate::style::StyleOwner),
    Borders(crate::style::StyleOwner),
    Border(crate::style::StyleOwner, i32),

    Style(BookId, std::sync::Arc<str>),
    Styles(BookId),

    Scenarios(BookId, SheetId),

    Scenario(BookId, SheetId, String),

    VBProject(BookId),
    VBComponents(BookId),
    VBComponent(BookId, crate::vbide::Comp),
    CodeModule(BookId, crate::vbide::Comp),

    Controls(BookId, SheetId, crate::controls::Coll),

    Control(crate::controls::CtlRef),

    ControlFormat(crate::controls::CtlRef),
}

#[derive(Clone, PartialEq, Eq, Hash, Debug)]
pub(crate) enum SortOwner {
    Sheet(BookId, SheetId),
    Table(BookId, String),
}

impl SortOwner {
    pub(crate) fn key(&self) -> SortOwner {
        match self {
            SortOwner::Table(b, n) => SortOwner::Table(*b, n.to_lowercase()),
            other => other.clone(),
        }
    }
}

#[derive(Clone, Debug)]
pub(crate) struct SortState {
    pub fields: Vec<(SheetId, Rect, i32)>,
    pub range: Option<(SheetId, Rect)>,
    pub orientation: i32,
    pub header: i32,
    pub match_case: bool,
}

impl Default for SortState {
    fn default() -> SortState {
        SortState {
            fields: Vec::new(),
            range: None,

            orientation: 1,
            header: 0,
            match_case: false,
        }
    }
}

impl Object {
    pub(crate) fn interface_name(&self) -> &'static str {
        match self {
            Object::Application => "_Application",
            Object::Workbooks => "Workbooks",
            Object::Windows => "Windows",
            Object::Window(_) => "Window",
            Object::Workbook(_) => "_Workbook",
            Object::Sheets(..) => "Sheets",
            Object::Worksheet(..) => "_Worksheet",
            Object::Chart(..) => "_Chart",
            Object::Range(_) | Object::Multi(_) => "Range",
            Object::Areas(_) => "Areas",
            Object::Names(..) => "Names",
            Object::Name(..) => "Name",
            Object::ListObjects(..) => "ListObjects",
            Object::Table(..) => "ListObject",
            Object::ListColumns(..) => "ListColumns",
            Object::ListColumn(..) => "ListColumn",
            Object::Enumerator(..) => "IEnumVARIANT",
            Object::Sort(_) => "Sort",
            Object::SortFields(_) => "SortFields",
            Object::SortField(..) => "SortField",
            Object::AutoFilter(..) => "AutoFilter",
            Object::WorksheetFunction => "WorksheetFunction",
            Object::MultiThreadedCalculation => "MultiThreadedCalculation",
            Object::Font(_) => "Font",
            Object::Interior(_) => "Interior",
            Object::Borders(_) => "Borders",
            Object::Border(..) => "Border",
            Object::Style(..) => "Style",
            Object::Styles(_) => "Styles",
            Object::Scenarios(..) => "Scenarios",
            Object::Scenario(..) => "Scenario",
            Object::VBProject(_) => "_VBProject",
            Object::VBComponents(_) => "_VBComponents",
            Object::VBComponent(..) => "_VBComponent",
            Object::CodeModule(..) => "_CodeModule",
            Object::Controls(_, _, c) => c.interface_name(),
            Object::Control(r) => r.interface_name(),
            Object::ControlFormat(_) => "ControlFormat",
        }
    }
}

pub struct Application<'v> {
    pub(crate) arena: Arena<Object>,
    pub(crate) books: Vec<Book<'v>>,
    next_book: u32,
    pub(crate) next_number: u32,

    pub(crate) reserved_numbers: Vec<u32>,

    pub(crate) template_counts: std::collections::HashMap<String, u32>,

    pub(crate) default_file_path: String,

    pub(crate) calculate_before_save: bool,

    pub(crate) multi_threaded: bool,

    pub(crate) fs: Box<dyn crate::files::FileSystem>,
    pub(crate) active: Option<BookId>,
    pub(crate) display_alerts: bool,
    pub(crate) screen_updating: bool,
    pub(crate) visible: bool,
    pub(crate) enable_events: bool,
    pub(crate) quit: bool,

    pub(crate) clip: Option<(BookId, SheetId, Rect)>,

    pub(crate) sorts: std::collections::HashMap<SortOwner, SortState>,
    root: ObjectId,

    pub(crate) view: Option<crate::vba::ViewState>,

    pub(crate) running: Option<BookId>,

    pub(crate) events: crate::events::Bus,

    pub(crate) host: Box<dyn crate::host::Host>,

    pub(crate) on_time: Vec<crate::host::OnTimeRequest>,

    pub(crate) action_caller: Option<String>,
}

impl Default for Application<'_> {
    fn default() -> Self {
        Application::new()
    }
}

impl<'v> Application<'v> {
    pub fn new() -> Self {
        let mut arena = Arena::new();
        let root = arena.insert(Object::Application);
        Application {
            arena,
            books: Vec::new(),
            next_book: 0,
            next_number: 1,
            reserved_numbers: Vec::new(),
            template_counts: std::collections::HashMap::new(),
            default_file_path: std::env::current_dir()
                .map(|d| d.to_string_lossy().to_string())
                .unwrap_or_default(),
            fs: Box::new(crate::files::RealFs),
            calculate_before_save: true,
            multi_threaded: true,
            active: None,
            display_alerts: true,
            screen_updating: true,
            visible: false,
            enable_events: true,
            quit: false,
            clip: None,
            sorts: std::collections::HashMap::new(),
            root,
            view: None,
            running: None,
            events: Default::default(),
            host: Box::new(crate::host::NoDialogs),
            on_time: Vec::new(),
            action_caller: None,
        }
    }

    pub fn handle(&self) -> ObjectId {
        self.root
    }

    pub fn add_workbook(&mut self, wb: Workbook) -> BookId {
        let name = format!("Book{}", self.next_book_number());
        self.push_book(wb, name, String::new())
    }

    pub(crate) fn push_book(&mut self, wb: Workbook, name: String, path: String) -> BookId {
        self.push_book_wb(BookWb::Own(Box::new(wb)), name, path)
    }

    pub(crate) fn push_book_wb(&mut self, wb: BookWb<'v>, name: String, path: String) -> BookId {
        let id = BookId(self.next_book);
        self.next_book += 1;

        let next_sheet = wb.sheet_count() as u32 + 1;
        self.books.push(Book {
            id,
            wb,
            name,
            path,
            saved: true,
            next_sheet,
            next_chart: 1,
            vba: Default::default(),
            ide: Default::default(),
            warnings: Vec::new(),
            file_format: 51,
            package: None,
            read_only: false,
            controls: Default::default(),
        });
        self.active = Some(id);
        id
    }

    pub fn swap_engine(&mut self, book: BookId, wb: &mut Workbook) {
        if let Some(BookWb::Own(own)) = self
            .books
            .iter_mut()
            .find(|b| b.id == book)
            .map(|b| &mut b.wb)
        {
            std::mem::swap(&mut **own, wb);
        }
    }

    pub fn active_workbook(&self) -> Option<BookId> {
        self.active
    }

    pub fn engine(&self, book: BookId) -> Option<&Workbook> {
        self.books.iter().find(|b| b.id == book).map(|b| &*b.wb)
    }

    pub(crate) fn book(&self, id: BookId) -> Result<&Book<'v>, OmError> {
        self.books
            .iter()
            .find(|b| b.id == id)
            .ok_or(OmError::hresult(RPC_E_DISCONNECTED))
    }

    pub(crate) fn book_mut(&mut self, id: BookId) -> Result<&mut Book<'v>, OmError> {
        let b = self
            .books
            .iter_mut()
            .find(|b| b.id == id)
            .ok_or(OmError::hresult(RPC_E_DISCONNECTED))?;
        match b.wb {
            BookWb::Own(_) => Ok(b),
            BookWb::View(_) => Err(OmError::app_defined()),
        }
    }

    pub(crate) fn active_book(&self) -> Result<&Book<'v>, OmError> {
        match self.active {
            Some(id) => self.book(id),
            None => Err(OmError::exception(crate::error::vba_scode(91), None)),
        }
    }

    pub(crate) fn alloc(&mut self, o: Object) -> Variant {
        Variant::Dispatch(Some(self.arena.insert(o)))
    }

    pub(crate) fn track_ref(
        &mut self,
        book: BookId,
        sheet: SheetId,
        rect: Rect,
        mode: Mode,
    ) -> Result<RangeRef, OmError> {
        let track = match self.book(book)?.wb {
            BookWb::View(_) => Track::Fixed(sheet, rect),
            BookWb::Own(_) => Track::Live(self.book_mut(book)?.wb.track(sheet, rect)),
        };
        Ok(RangeRef { book, track, mode })
    }

    pub(crate) fn new_range(
        &mut self,
        book: BookId,
        sheet: SheetId,
        rect: Rect,
        mode: Mode,
    ) -> Result<Variant, OmError> {
        let r = self.track_ref(book, sheet, rect, mode)?;
        Ok(self.alloc(Object::Range(r)))
    }

    pub(crate) fn new_multi(
        &mut self,
        book: BookId,
        sheet: SheetId,
        rects: &[Rect],
        mode: Mode,
    ) -> Result<Variant, OmError> {
        if let [one] = rects {
            return self.new_range(book, sheet, *one, mode);
        }
        let areas = self.track_areas(book, sheet, rects, mode)?;
        Ok(self.alloc(Object::Multi(areas)))
    }

    pub(crate) fn track_areas(
        &mut self,
        book: BookId,
        sheet: SheetId,
        rects: &[Rect],
        mode: Mode,
    ) -> Result<Vec<RangeRef>, OmError> {
        rects
            .iter()
            .map(|rect| self.track_ref(book, sheet, *rect, mode))
            .collect()
    }

    pub(crate) fn resolve(&self, r: &RangeRef) -> Result<(SheetId, Rect), OmError> {
        let wb = &self.book(r.book)?.wb;
        match r.track {
            Track::Live(t) => wb.tracked(t).ok_or_else(OmError::object_required),
            Track::Fixed(sheet, rect) => wb
                .sheet(sheet)
                .map(|_| (sheet, rect))
                .ok_or_else(OmError::object_required),
        }
    }

    pub fn release(&mut self, id: ObjectId) {
        if id == self.root {
            return;
        }
        let refs = match self.arena.remove(id) {
            Some(Object::Range(r)) => vec![r],
            Some(Object::Multi(rs) | Object::Areas(rs)) => rs,
            _ => return,
        };
        for r in refs {
            if let (Track::Live(t), Ok(b)) = (r.track, self.book_mut(r.book)) {
                b.wb.untrack(t);
            }
        }
    }

    pub fn type_name(&self, id: ObjectId) -> Option<&'static str> {
        self.arena.get(id).map(Object::interface_name)
    }

    pub fn enum_next(&mut self, id: ObjectId, count: usize) -> Vec<Variant> {
        match self.arena.get_mut(id) {
            Some(Object::Enumerator(items, pos)) => {
                let n = count.min(items.len() - *pos);
                let out = items[*pos..*pos + n].to_vec();
                *pos += n;
                out
            }
            _ => Vec::new(),
        }
    }

    pub fn enum_reset(&mut self, id: ObjectId) {
        if let Some(Object::Enumerator(_, pos)) = self.arena.get_mut(id) {
            *pos = 0;
        }
    }

    fn interface_of(&self, handle: ObjectId) -> Result<(Object, &'static Interface), OmError> {
        let obj = self
            .arena
            .get(handle)
            .ok_or_else(OmError::object_required)?
            .clone();
        match typelib::interface(obj.interface_name()) {
            Some(i) => Ok((obj, i)),
            None => Err(OmError::hresult(DISP_E_MEMBERNOTFOUND)),
        }
    }

    pub fn get_ids_of_names(
        &self,
        handle: ObjectId,
        member: &str,
        named: &[&str],
    ) -> Result<Vec<i32>, NameError> {
        let fail = |hresult, ids| Err(NameError { hresult, ids });
        let Ok((_, iface)) = self.interface_of(handle) else {
            return fail(DISP_E_MEMBERNOTFOUND, vec![-1; 1 + named.len()]);
        };
        let candidates = iface.named(member);
        let Some(first) = candidates.first() else {
            return fail(DISP_E_UNKNOWNNAME, vec![-1; 1 + named.len()]);
        };
        let mut ids = vec![first.dispid];
        let mut failed = false;
        for name in named {
            let pos = if failed {
                None
            } else {
                candidates.iter().find_map(|m| {
                    m.params
                        .iter()
                        .position(|p| !p.name.is_empty() && p.name.eq_ignore_ascii_case(name))
                })
            };
            match pos {
                Some(i) => ids.push(i as i32),
                None => {
                    failed = true;
                    ids.push(-1);
                }
            }
        }
        if failed {
            return fail(HResult(crate::error::vba_scode(1004)), ids);
        }
        Ok(ids)
    }

    pub fn invoke(
        &mut self,
        handle: ObjectId,
        member: &str,
        call: Call,
        positional: &[Variant],
        named: &[(&str, Variant)],
    ) -> Result<Variant, OmError> {
        let names: Vec<&str> = named.iter().map(|n| n.0).collect();
        let ids = match self.get_ids_of_names(handle, member, &names) {
            Ok(ids) => ids,
            Err(e) => {
                let wsf =
                    typelib::interface("WorksheetFunction").and_then(|i| i.named(member).first());
                if let (Some(m), Some(Object::Application), true) =
                    (wsf, self.arena.get(handle), named.is_empty())
                {
                    if positional.len() > m.params.len() {
                        return Err(OmError::hresult(DISP_E_BADPARAMCOUNT));
                    }
                    let args = bind(m, positional, &[], None, call.lcid)?;
                    return self.worksheet_function(m, &args, false);
                }
                return Err(OmError::hresult(e.hresult));
            }
        };
        let by_id: Vec<(i32, Variant)> = ids[1..]
            .iter()
            .zip(named)
            .map(|(id, (_, v))| (*id, v.clone()))
            .collect();
        self.invoke_dispid(handle, ids[0], call, positional, &by_id)
    }

    pub fn invoke_dispid(
        &mut self,
        handle: ObjectId,
        dispid: i32,
        call: Call,
        positional: &[Variant],
        named: &[(i32, Variant)],
    ) -> Result<Variant, OmError> {
        let (obj, iface) = self.interface_of(handle)?;
        let candidates: Vec<&'static Member> = iface
            .members
            .iter()
            .filter(|m| m.dispid == dispid)
            .collect();
        if candidates.is_empty() {
            return Err(OmError::hresult(DISP_E_MEMBERNOTFOUND));
        }

        let reads_item = iface.name == "Scenarios" && dispid == 0xaa;
        let has_args = !reads_item && (!positional.is_empty() || !named.is_empty());
        let m = pick_member(&candidates, call.flags, has_args)?;
        let is_put = matches!(m.kind, InvokeKind::Put | InvokeKind::PutRef);
        let foreign = if english(call.lcid) {
            Foreign::Ignored
        } else {
            foreign(iface.name, m)
        };
        if foreign != Foreign::RefusesByName {
            if let Some(e) = foreign.error() {
                return Err(e);
            }
        }
        let (pos, value) = if is_put {
            match positional.split_last() {
                Some((v, rest)) => (rest, Some(v.clone())),
                None => return Err(OmError::hresult(DISP_E_BADPARAMCOUNT)),
            }
        } else {
            (positional, None)
        };
        let nparams = m.params.len() - usize::from(is_put);
        let follows = !is_put && nparams == 0 && matches!(m.ret, Ty::Object(_) | Ty::Dispatch);
        if pos.len() + named.len() > nparams && !follows {
            return Err(OmError::hresult(DISP_E_BADPARAMCOUNT));
        }
        let (own, extra) = pos.split_at(pos.len().min(nparams));
        let put_arg = value.clone();
        let args = bind(m, own, named, value, call.lcid)?;
        if foreign == Foreign::RefusesByName && matches!(args.get(0), Some(Variant::Bstr(_))) {
            return Err(OmError::app_defined());
        }

        self.flush_events();
        let stale = if is_put {
            self.rows_stale_begin(&obj, m.name)
        } else {
            None
        };
        let result = self.member(&obj, m, &args, call);
        if let Some(st) = stale {
            if result.is_ok() {
                self.rows_stale_end(st, put_arg.as_ref());
            }
        }
        self.flush_events();
        let mut result = result?;
        if foreign == Foreign::EmptyText {
            result = Variant::Bstr(String::new());
        }
        if extra.is_empty() {
            return Ok(result);
        }

        let Variant::Dispatch(Some(inner)) = result else {
            return Err(OmError::hresult(DISP_E_BADPARAMCOUNT));
        };

        let english = Call {
            lcid: 0x0409,
            ..call
        };
        let r = self.invoke_dispid(inner, DISPID_VALUE, english, extra, &[]);
        self.release(inner);
        r
    }
}

fn pick_member(
    candidates: &[&'static Member],
    call_flags: u16,
    has_args: bool,
) -> Result<&'static Member, OmError> {
    use InvokeKind::*;
    let find = |k| candidates.iter().find(|m| m.kind == k).copied();
    if call_flags & (flags::PUT | flags::PUTREF) != 0 {
        let put = if call_flags & flags::PUTREF != 0 {
            find(PutRef).or_else(|| find(Put))
        } else {
            find(Put).or_else(|| find(PutRef))
        };
        return put.ok_or(OmError::hresult(DISP_E_BADPARAMCOUNT));
    }
    if call_flags & flags::GET != 0 {
        if let Some(m) = find(Get) {
            return Ok(m);
        }
    }
    if let Some(m) = find(Func) {
        if call_flags & flags::METHOD != 0 || !has_args {
            return Ok(m);
        }
    }
    Err(OmError::hresult(DISP_E_MEMBERNOTFOUND))
}

fn bind(
    m: &Member,
    pos: &[Variant],
    named: &[(i32, Variant)],
    put: Option<Variant>,
    lcid: u32,
) -> Result<Args, OmError> {
    let nparams = m.params.len() - usize::from(put.is_some());
    let mut values: Vec<Option<Variant>> = vec![None; nparams];
    for (i, v) in pos.iter().enumerate() {
        if !is_missing(v) {
            values[i] = Some(v.clone());
        }
    }
    for (k, (id, v)) in named.iter().enumerate() {
        let slot = usize::try_from(*id).ok().filter(|&i| i < nparams);
        match slot {
            Some(i) if i >= pos.len() && values[i].is_none() => {
                if !is_missing(v) {
                    values[i] = Some(v.clone());
                }
            }
            _ => return Err(OmError::arg(DISP_E_PARAMNOTFOUND, k as u32)),
        }
    }
    for (i, p) in m.params[..nparams].iter().enumerate() {
        if values[i].is_some() {
            continue;
        }
        match p.opt {
            Opt::Required if matches!(m.name, "Union" | "Intersect") => {
                if i >= pos.len() {
                    return Err(OmError::hresult(DISP_E_PARAMNOTOPTIONAL));
                }
            }
            Opt::Required if i >= pos.len() => {
                return Err(OmError::hresult(DISP_E_BADPARAMCOUNT));
            }
            Opt::Required => return Err(OmError::type_mismatch()),
            Opt::Optional => {}
            Opt::Default(d) => {
                values[i] = Some(match d {
                    DefaultValue::I2(n) => Variant::I2(n),
                    DefaultValue::I4(n) => Variant::I4(n),
                    DefaultValue::R4(n) => Variant::R4(n),
                    DefaultValue::Bool(b) => Variant::Bool(b),
                })
            }
        }
    }
    Ok(Args { values, put, lcid })
}

impl Application<'_> {
    fn member(
        &mut self,
        obj: &Object,
        m: &'static Member,
        a: &Args,
        call: Call,
    ) -> Result<Variant, OmError> {
        if self.view.is_some() && refused_in_view(obj, m, call) {
            return Err(OmError::app_defined());
        }
        let r = match obj {
            Object::Application => self.application_member(m, a),
            Object::Workbooks => self.workbooks_member(m, a),
            Object::Windows => self.windows_member(m, a),
            Object::Window(b) => self.window_member(*b, m),
            Object::Workbook(b) => self.workbook_member(*b, m, a),
            Object::Sheets(b, k) => self.sheets_member(*b, *k, m, a),
            Object::Worksheet(b, s) => self.worksheet_member(*b, *s, m, a),
            Object::Chart(b, s) => self.chart_member(*b, *s, m, a),
            Object::Range(r) => self.range_member(r, m, a),
            Object::Multi(rs) => self.multi_member(rs, m, a),
            Object::Areas(rs) => self.areas_member(rs, m, a),
            Object::Names(b, s) => self.names_member(*b, *s, m, a),
            Object::Name(b, s, k) => self.name_member(*b, *s, k, m),
            Object::ListObjects(b, s) => self.list_objects_member(*b, *s, m, a),
            Object::Table(b, n) => self.list_object_member(*b, n, m, a),
            Object::ListColumns(b, n) => self.list_columns_member(*b, n, m, a),
            Object::ListColumn(b, n, i) => self.list_column_member(*b, n, *i, m),
            Object::Enumerator(..) => Err(OmError::hresult(DISP_E_MEMBERNOTFOUND)),
            Object::Sort(o) => self.sort_member(o, m, a),
            Object::SortFields(o) => self.sort_fields_member(o, m, a),
            Object::SortField(o, i) => self.sort_field_member(o, *i, m, a),
            Object::AutoFilter(b, s) => self.autofilter_member(*b, *s, m),
            Object::WorksheetFunction => self.worksheet_function(m, a, true),
            Object::MultiThreadedCalculation => self.multi_threaded_member(m, a),
            Object::Font(o) => self.font_member(o, m, a),
            Object::Interior(o) => self.interior_member(o, m, a),
            Object::Borders(o) => self.borders_member(o, m, a),
            Object::Border(o, i) => self.border_member(o, *i, m, a),
            Object::Style(b, n) => self.style_member(*b, n, m, a),
            Object::Styles(b) => self.styles_member(*b, m, a),
            Object::Scenarios(b, s) => self.scenarios_member(*b, *s, m, a),
            Object::Scenario(b, s, n) => self.scenario_member(*b, *s, n, m, a),
            Object::VBProject(b) => self.vbproject_member(*b, m, a),
            Object::VBComponents(b) => self.vbcomponents_member(*b, m, a),
            Object::VBComponent(b, c) => self.vbcomponent_member(*b, *c, m, a),
            Object::CodeModule(b, c) => self.codemodule_member(*b, *c, m, a),
            Object::Controls(b, s, c) => self.controls_member(*b, *s, *c, m, a),
            Object::Control(r) => self.control_member(r, m, a),
            Object::ControlFormat(r) => self.control_format_member(r, m, a),
        };
        let changes = matches!(
            (obj.interface_name(), m.name),
            ("Range", "ClearContents" | "Clear" | "ClearFormats")
                | ("Names", "Add")
                | ("Name", "Delete")
                | ("Sheets", "Add")
                | ("_Worksheet" | "_Chart", "Delete")
                | ("ListObjects", "Add")
                | ("ListObject", "Delete" | "Unlist" | "Resize")
                | ("Sort", "Apply")
        );
        if r.is_ok()
            && m.name != "Saved"
            && (changes || call.flags & (flags::PUT | flags::PUTREF) != 0)
        {
            self.mark_unsaved(obj);
        }
        r
    }

    fn mark_unsaved(&mut self, obj: &Object) {
        let book = match obj {
            Object::Workbook(b)
            | Object::Sheets(b, _)
            | Object::Worksheet(b, _)
            | Object::Chart(b, _)
            | Object::Names(b, _)
            | Object::ListObjects(b, _)
            | Object::Table(b, _)
            | Object::AutoFilter(b, _)
            | Object::Sort(SortOwner::Table(b, _))
            | Object::Sort(SortOwner::Sheet(b, _))
            | Object::Name(b, ..) => Some(*b),
            Object::Range(r) => Some(r.book),
            Object::Multi(rs) | Object::Areas(rs) => rs.first().map(|r| r.book),
            Object::Style(b, _) | Object::Styles(b) => Some(*b),
            Object::Scenarios(b, _) | Object::Scenario(b, ..) => Some(*b),
            Object::Controls(b, ..) => Some(*b),
            Object::Control(r) | Object::ControlFormat(r) => Some(r.book),
            Object::Font(o) | Object::Interior(o) | Object::Borders(o) | Object::Border(o, _) => {
                Some(match o {
                    crate::style::StyleOwner::Range(r) => r.book,
                    crate::style::StyleOwner::Named(b, _) => *b,
                })
            }
            _ => self.active,
        };
        if let Some(b) = book.and_then(|b| self.book_mut(b).ok()) {
            b.saved = false;
        }
    }
}

fn refused_in_view(obj: &Object, m: &Member, call: Call) -> bool {
    if call.flags & (flags::PUT | flags::PUTREF) != 0 {
        return true;
    }
    m.kind == InvokeKind::Func
        && !matches!(obj, Object::WorksheetFunction | Object::Enumerator(..))
        && !matches!(
            m.name,
            "Volatile"
                | "Run"
                | "Evaluate"
                | "_Evaluate"
                | "Intersect"
                | "Union"
                | "Find"
                | "FindNext"
                | "FindPrevious"
                | "_NewEnum"
                | "Item"
                | "_Default"
        )
}

pub(crate) fn unsupported(interface: &str, m: &Member) -> OmError {
    OmError::unsupported(&format!("{}.{}", interface.trim_start_matches('_'), m.name))
}

#[derive(Clone, Copy, PartialEq)]
enum StaleKind {
    Width,

    Entry,

    Format,
}

pub(crate) struct Stale {
    book: BookId,
    sheet: SheetId,
    rect: Rect,
    kind: StaleKind,
    snap: Vec<(u32, u32)>,
}

impl Application<'_> {
    fn rows_stale_begin(&mut self, obj: &Object, member: &str) -> Option<Stale> {
        let r = match obj {
            Object::Range(r) => r,
            Object::Font(crate::style::StyleOwner::Range(r))
            | Object::Interior(crate::style::StyleOwner::Range(r))
            | Object::Borders(crate::style::StyleOwner::Range(r))
            | Object::Border(crate::style::StyleOwner::Range(r), _) => r,
            _ => return None,
        };
        let kind = match member {
            "ColumnWidth" => StaleKind::Width,
            "Formula" | "Formula2" | "FormulaLocal" | "Formula2Local" | "FormulaR1C1"
            | "FormulaR1C1Local" | "Formula2R1C1" | "Formula2R1C1Local" | "Value" | "Value2" => {
                StaleKind::Entry
            }
            "Hidden" | "RowHeight" | "Height" | "Width" => return None,
            _ => StaleKind::Format,
        };
        let (sheet, rect) = self.resolve(r).ok()?;
        let snap = if kind == StaleKind::Format {
            Vec::new()
        } else {
            self.book(r.book).ok()?.wb.auto_snapshot(sheet)
        };
        Some(Stale {
            book: r.book,
            sheet,
            rect,
            kind,
            snap,
        })
    }

    fn rows_stale_end(&mut self, st: Stale, arg: Option<&Variant>) {
        let Ok(b) = self.book_mut(st.book) else {
            return;
        };
        let wb = &mut b.wb;
        let constant = !matches!(arg, Some(Variant::Bstr(t)) if t.starts_with('='));
        let refit = match st.kind {
            StaleKind::Width => false,
            StaleKind::Format => true,
            StaleKind::Entry => constant && wb.holds_wrapped_text(st.sheet, st.rect),
        };
        if refit {
            wb.auto_refit(st.sheet, st.rect.r0, st.rect.r1);
        }
        let keep: Vec<(u32, u32)> = st
            .snap
            .iter()
            .copied()
            .filter(|(r, _)| !(refit && (st.rect.r0..=st.rect.r1).contains(r)))
            .collect();
        wb.auto_settle(st.sheet, &keep);
    }
}
