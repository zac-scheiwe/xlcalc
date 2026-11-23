use xlcalc::{Rect, SheetId, TrackId, Workbook};
use xlcalc_variant::{HResult, ObjectId, Variant};

pub(crate) const INDEX_MISMATCH: &str = "index";

use crate::arena::Arena;
use crate::conv::{is_missing, Args};
use crate::error::{
    NameError, OmError, DISP_E_BADPARAMCOUNT, DISP_E_MEMBERNOTFOUND, DISP_E_PARAMNOTFOUND,
    DISP_E_PARAMNOTOPTIONAL, DISP_E_TYPEMISMATCH, DISP_E_UNKNOWNNAME, RPC_E_DISCONNECTED,
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

    pub next_macro: (u32, u32),

    pub threads: Vec<((SheetId, u32, u32), crate::notesobj::Thread)>,

    pub window: crate::window::WindowState,

    pub consolidate: std::collections::HashMap<SheetId, crate::consolidate::ConsolidateState>,

    pub vba: crate::vba::BookVba,

    pub ide: crate::vbide::Ide,

    pub warnings: Vec<String>,

    pub file_format: i32,

    pub compat: bool,

    pub package: Option<xlcalc_xlsx::Package>,

    pub read_only: bool,

    pub controls: crate::controls::BookControls,

    pub is_addin: bool,

    pub props: crate::addins::DocProps,

    pub protect: Option<Option<crate::sheetprot::Pw>>,

    pub sheet_prot: crate::sheetprot::BookSheetProt,

    pub edit_ranges: crate::sheetprot::BookEditRanges,

    pub page_setup: crate::pagesetup::BookPageSetup,

    pub no_link_values: bool,

    pub no_remote_refs: bool,

    pub ro_recommended: bool,

    pub draw_objects: i32,
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

    pub axis: Option<Mode>,
}

impl RangeRef {
    pub(crate) fn eff_mode(&self) -> Mode {
        match (self.mode, self.axis) {
            (Mode::Cells, Some(a)) => a,
            (m, _) => m,
        }
    }
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

    Macro,

    MacroIntl,

    Selected,

    Subset(u128),

    Modules,

    Dialogs,
}

#[derive(Clone, Debug)]
pub(crate) enum Object {
    Application,
    Workbooks,

    Windows,

    BookWindows(BookId),

    Window(BookId, u32),
    Panes(BookId, u32),
    Pane(BookId, u32, u32),
    Workbook(BookId),

    Sheets(BookId, SheetsKind),
    Worksheet(BookId, SheetId),

    Chart(BookId, SheetId),

    Module(BookId, SheetId),

    Dialog(BookId, SheetId),
    Range(RangeRef),

    Multi(Vec<RangeRef>),

    Areas(Vec<RangeRef>),
    Names(BookId, Option<SheetId>),

    Name(BookId, Option<SheetId>, String),
    ListObjects(BookId, SheetId),

    Table(BookId, String),
    ListColumns(BookId, String),
    ListColumn(BookId, String, usize),

    TableStyle(String),
    ListRows(BookId, String),
    ListRow(BookId, String, usize),

    Enumerator(Vec<Variant>, usize),

    Sort(SortOwner),
    SortFields(SortOwner),

    SortField(SortOwner, usize),

    SortOnValue(SortOwner, usize, bool),

    AutoFilter(BookId, SheetId, Option<String>),

    Filters(BookId, SheetId),

    Filter(BookId, SheetId, usize),

    WorksheetFunction,

    MultiThreadedCalculation,

    Font(crate::style::StyleOwner),
    Interior(crate::style::StyleOwner),
    Borders(crate::style::StyleOwner),
    Border(crate::style::StyleOwner, i32),

    Style(BookId, std::sync::Arc<str>),
    Styles(BookId),

    Scenarios(BookId, SheetId),

    OleObjects(BookId),

    Scenario(BookId, SheetId, String),

    CustomProperties(BookId, SheetId),

    CommentShape(BookId, SheetId, u32),

    CommentTextFrame(BookId, SheetId, u32),
    CustomProperty(BookId, SheetId, u32),

    Spark(BookId, SheetId, crate::sparklines::SparkPart),

    Slicer(BookId, crate::slicers::SlicerPart),

    VBProject(BookId),
    VBComponents(BookId),
    VBComponent(BookId, crate::vbide::Comp),
    CodeModule(BookId, crate::vbide::Comp),

    Controls(BookId, SheetId, crate::controls::Coll),

    Control(crate::controls::CtlRef),

    Draw(crate::shapes::DrawRef),

    ControlFormat(crate::controls::CtlRef),

    AddIns,

    AddIns2,

    PageBreaks(bool, Option<(BookId, SheetId)>),

    PageBreak(bool, BookId, SheetId, u32),

    ComAddIns,

    ComAddIn(String),

    AddIn(String),

    FormatConditions(RangeRef),

    FormatCondition(BookId, SheetId, u32, crate::cfv::CfIface, Rect),

    ScaleCriteria(BookId, SheetId, u32),

    ScaleCriterion(BookId, SheetId, u32, usize),

    ScaleColor(BookId, SheetId, u32, usize),

    CfPart(BookId, SheetId, u32, crate::cfdeco::CfPart),

    IconSets(BookId),

    IconSetDef(BookId, i32),

    Validation(RangeRef),

    ValidationMulti(RangeRef),

    DisplayFormat(RangeRef),

    Hyperlinks(BookId, SheetId, Rect),

    Comments(BookId, SheetId),

    Comment(BookId, SheetId, u32),

    CommentThreaded(BookId, SheetId, u32, u32),

    ThreadList(BookId, SheetId, Option<(u32, u32)>),

    Hyperlink(BookId, SheetId, u32),

    DocProp(BookId, String),

    Stub(&'static str),

    DocProps(BookId, bool),

    Outline(BookId, SheetId),

    Tab(BookId, SheetId),

    Pages(BookId, SheetId),

    Protection(BookId, SheetId),

    AllowEditRanges(BookId, SheetId),

    AllowEditRange(BookId, SheetId, usize),

    PageSetup(BookId, SheetId),

    Characters(RangeRef, crate::rangeparts::CharSpan),

    CharFont(RangeRef, crate::rangeparts::CharSpan),

    Phonetic(RangeRef),

    Pivot(crate::pivot::PivotRef),
}

#[derive(Clone, PartialEq, Eq, Hash, Debug)]
pub(crate) enum SortOwner {
    Sheet(BookId, SheetId),

    Filter(BookId, SheetId),
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

    pub custom: Vec<Vec<String>>,

    pub method: i32,

    pub order_custom: i32,

    pub extra: Vec<FieldExtra>,
}

#[derive(Clone, Debug, Default)]
pub(crate) struct FieldExtra {
    pub sort_on: i32,

    pub color: Option<i32>,
    pub data_option: i32,

    pub custom_order: Option<Variant>,
    pub sub_field: Option<String>,
}

impl FieldExtra {
    pub(crate) fn sort_color(&self) -> Option<xlcalc::SortColor> {
        if !(1..=2).contains(&self.sort_on) {
            return None;
        }

        let rgb = self.color.filter(|&c| c >= 0).map(|c| {
            let c = c as u32;
            ((c & 0xFF) << 16) | (c & 0xFF00) | ((c >> 16) & 0xFF)
        });
        Some(xlcalc::SortColor {
            font: self.sort_on == 2,
            rgb,
        })
    }
}

impl Default for SortState {
    fn default() -> SortState {
        SortState {
            fields: Vec::new(),
            range: None,

            orientation: 1,
            header: 0,
            match_case: false,
            custom: Vec::new(),
            method: 1,
            order_custom: 0,
            extra: Vec::new(),
        }
    }
}

impl Object {
    pub(crate) fn interface_name(&self) -> &'static str {
        match self {
            Object::Application => "_Application",
            Object::Workbooks => "Workbooks",
            Object::Windows => "Windows",
            Object::BookWindows(_) => "Windows",
            Object::Window(..) => "Window",
            Object::Panes(..) => "Panes",
            Object::Pane(..) => "Pane",
            Object::Workbook(_) => "_Workbook",
            Object::Sheets(..) => "Sheets",
            Object::Worksheet(..) => "_Worksheet",
            Object::Chart(..) => "_Chart",
            Object::Module(..) => "Module",
            Object::Dialog(..) => "DialogSheet",
            Object::Range(_) | Object::Multi(_) => "Range",
            Object::Areas(_) => "Areas",
            Object::Names(..) => "Names",
            Object::Name(..) => "Name",
            Object::ListObjects(..) => "ListObjects",
            Object::Table(..) => "ListObject",
            Object::ListColumns(..) => "ListColumns",
            Object::ListColumn(..) => "ListColumn",
            Object::TableStyle(_) => "TableStyle",
            Object::ListRows(..) => "ListRows",
            Object::ListRow(..) => "ListRow",
            Object::Enumerator(..) => "IEnumVARIANT",
            Object::Sort(_) => "Sort",
            Object::SortFields(_) => "SortFields",
            Object::SortField(..) => "SortField",
            Object::SortOnValue(_, _, true) => "Font",
            Object::SortOnValue(_, _, false) => "Interior",
            Object::AutoFilter(..) => "AutoFilter",
            Object::Filters(..) => "Filters",
            Object::Filter(..) => "Filter",
            Object::WorksheetFunction => "WorksheetFunction",
            Object::MultiThreadedCalculation => "MultiThreadedCalculation",
            Object::Font(_) => "Font",
            Object::Interior(_) => "Interior",
            Object::Borders(_) => "Borders",
            Object::Border(..) => "Border",
            Object::Style(..) => "Style",
            Object::Styles(_) => "Styles",
            Object::Scenarios(..) => "Scenarios",
            Object::OleObjects(..) => "OLEObjects",
            Object::Scenario(..) => "Scenario",
            Object::CustomProperties(..) => "CustomProperties",
            Object::CommentShape(..) => "Shape",
            Object::CommentTextFrame(..) => "TextFrame",
            Object::CustomProperty(..) => "CustomProperty",
            Object::Spark(_, _, p) => p.interface_name(),
            Object::Slicer(_, p) => p.interface_name(),
            Object::VBProject(_) => "_VBProject",
            Object::VBComponents(_) => "_VBComponents",
            Object::VBComponent(..) => "_VBComponent",
            Object::CodeModule(..) => "_CodeModule",
            Object::Controls(_, _, c) => c.interface_name(),
            Object::Control(r) => r.interface_name(),
            Object::ControlFormat(_) => "ControlFormat",
            Object::AddIns => "AddIns",
            Object::AddIns2 => "AddIns2",
            Object::PageBreaks(true, _) => "HPageBreaks",
            Object::PageBreaks(false, _) => "VPageBreaks",
            Object::PageBreak(true, ..) => "HPageBreak",
            Object::PageBreak(false, ..) => "VPageBreak",
            Object::AddIn(_) => "AddIn",
            Object::ComAddIns => "COMAddIns",
            Object::ComAddIn(_) => "COMAddIn",
            Object::Draw(d) => d.kind.interface_name(),
            Object::FormatConditions(_) => "FormatConditions",
            Object::FormatCondition(_, _, _, i, _) => i.interface_name(),
            Object::ScaleCriteria(..) => "ColorScaleCriteria",
            Object::ScaleCriterion(..) => "ColorScaleCriterion",
            Object::ScaleColor(..) => "FormatColor",
            Object::CfPart(_, _, _, p) => p.interface_name(),
            Object::IconSets(_) => "IconSets",
            Object::IconSetDef(..) => "IconSet",
            Object::Validation(_) | Object::ValidationMulti(_) => "Validation",
            Object::DisplayFormat(_) => "DisplayFormat",
            Object::Hyperlinks(..) => "Hyperlinks",
            Object::Comments(..) => "Comments",
            Object::Comment(..) => "Comment",
            Object::CommentThreaded(..) => "CommentThreaded",
            Object::ThreadList(..) => "CommentsThreaded",
            Object::Hyperlink(..) => "Hyperlink",
            Object::DocProp(..) => "DocumentProperty",
            Object::Stub(name) => name,
            Object::DocProps(..) => "DocumentProperties",
            Object::Outline(..) => "Outline",
            Object::Tab(..) => "Tab",
            Object::Pages(..) => "Pages",
            Object::Protection(..) => "Protection",
            Object::AllowEditRanges(..) => "AllowEditRanges",
            Object::AllowEditRange(..) => "AllowEditRange",
            Object::PageSetup(..) => "PageSetup",
            Object::Characters(..) => "Characters",
            Object::CharFont(..) => "Font",
            Object::Phonetic(_) => "Phonetic",
            Object::Pivot(p) => p.interface_name(),
        }
    }
}

pub struct Application<'v> {
    pub(crate) arena: Arena<Object>,
    pub(crate) books: Vec<Book<'v>>,
    next_book: u32,
    pub(crate) next_number: u32,

    pub(crate) reserved_numbers: Vec<u32>,

    pub(crate) widen_kept: Vec<(SheetId, u32, u32)>,

    pub(crate) chars_font_put: bool,

    pub(crate) emptied: std::collections::HashSet<(BookId, SheetId, u32, u32)>,

    pub(crate) find: crate::rangefind::FindState,

    pub(crate) moving_group: bool,

    pub(crate) in_vba: bool,

    pub(crate) expr_call: bool,

    pub(crate) template_counts: std::collections::HashMap<String, u32>,

    pub(crate) shape_ranges: Vec<Vec<u32>>,

    pub(crate) custom_props:
        std::collections::HashMap<(BookId, SheetId), Vec<crate::customprops::CustomProp>>,
    pub(crate) next_custom: u32,

    pub(crate) range_ids: std::collections::HashMap<(BookId, SheetId, u32, u32), String>,

    pub(crate) group_ranges: std::collections::HashMap<usize, u32>,

    pub(crate) pins: std::collections::HashMap<ObjectId, (BookId, SheetId, u32)>,

    pub(crate) default_file_path: String,

    pub(crate) calculate_before_save: bool,

    pub(crate) multi_threaded: bool,

    pub(crate) opts: std::collections::HashMap<String, Variant>,

    pub(crate) stub_bag: xlcalc::drawing::Bag,

    pub(crate) user_lists: Vec<Vec<String>>,

    pub(crate) fs: Box<dyn crate::files::FileSystem>,
    pub(crate) active: Option<BookId>,

    pub(crate) wins: Vec<(BookId, u32)>,

    pub(crate) cur_win: Vec<(BookId, u32)>,

    pub(crate) event_win: Option<(BookId, u32)>,

    pub(crate) hold_activation: bool,

    pub(crate) chart_add_failed: bool,

    pub(crate) fc_poisoned: bool,

    pub(crate) comment_shapes:
        std::collections::HashMap<(BookId, SheetId, u32), crate::commentshape::CShape>,

    pub(crate) char_runs: crate::rangeparts::CharRuns,
    pub(crate) mixed_style_color: std::collections::HashSet<(BookId, std::sync::Arc<str>)>,

    pub(crate) style_numfmt_local: std::collections::HashSet<(BookId, std::sync::Arc<str>)>,
    pub(crate) style_edge_touched: std::collections::HashSet<(BookId, std::sync::Arc<str>)>,
    pub(crate) display_alerts: bool,

    pub(crate) solver_last: Option<crate::solver_solve::LastSolve>,

    pub(crate) ask_to_update_links: bool,
    pub(crate) screen_updating: bool,
    pub(crate) visible: bool,
    pub(crate) enable_events: bool,

    pub(crate) deferred_changes: Vec<(BookId, SheetId, Vec<Rect>)>,
    pub(crate) quit: bool,

    pub(crate) clip: Option<(BookId, SheetId, Rect)>,

    pub(crate) clip_cut: bool,

    pub(crate) clip_more: (Vec<Rect>, bool),

    pub(crate) shape_clip: Option<crate::shapes::ShapeClip>,

    pub(crate) paste_chain: std::collections::HashMap<(BookId, SheetId, i64, i64), (i64, i64)>,

    pub(crate) paste_source: Option<(BookId, SheetId, u32)>,

    pub(crate) sorts: std::collections::HashMap<SortOwner, SortState>,
    root: ObjectId,

    pub(crate) view: Option<crate::vba::ViewState>,

    pub(crate) running: Option<BookId>,

    pub(crate) events: crate::events::Bus,

    pub(crate) host: Box<dyn crate::host::Host>,

    pub(crate) on_time: Vec<crate::host::OnTimeRequest>,

    pub(crate) on_key: Vec<(String, String)>,

    pub(crate) action_caller: Option<String>,

    pub(crate) addins: crate::addins::AddInList,

    pub(crate) status_bar: Option<String>,

    pub(crate) com: Vec<crate::comobj::ComObj>,

    pub(crate) xml: crate::xmldom::Store,

    pub(crate) com_addins: crate::comaddin::ComAddIns,

    #[cfg(windows)]
    pub(crate) xlls: Vec<xlcalc_xll::XllHost>,

    #[cfg(windows)]
    pub(crate) xll_off: Vec<String>,

    #[cfg(windows)]
    pub(crate) rtd: Option<crate::rtdhost::RtdHost>,

    pub(crate) ticks: i32,
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
            widen_kept: Vec::new(),
            chars_font_put: false,
            emptied: Default::default(),
            find: crate::rangefind::FindState::default(),
            moving_group: false,
            in_vba: false,
            expr_call: false,
            template_counts: std::collections::HashMap::new(),
            shape_ranges: Vec::new(),
            custom_props: std::collections::HashMap::new(),
            next_custom: 0,
            range_ids: std::collections::HashMap::new(),
            group_ranges: std::collections::HashMap::new(),
            pins: std::collections::HashMap::new(),
            default_file_path: std::env::current_dir()
                .map(|d| d.to_string_lossy().to_string())
                .unwrap_or_default(),
            fs: Box::new(crate::files::RealFs),
            calculate_before_save: true,
            multi_threaded: true,
            opts: std::collections::HashMap::new(),
            stub_bag: xlcalc::drawing::Bag::new(),
            user_lists: Vec::new(),
            active: None,
            wins: Vec::new(),
            cur_win: Vec::new(),
            event_win: None,
            hold_activation: false,
            chart_add_failed: false,
            fc_poisoned: false,
            comment_shapes: std::collections::HashMap::new(),
            char_runs: Default::default(),
            mixed_style_color: std::collections::HashSet::new(),
            style_edge_touched: std::collections::HashSet::new(),
            style_numfmt_local: std::collections::HashSet::new(),
            display_alerts: true,
            solver_last: None,
            ask_to_update_links: true,
            screen_updating: true,
            visible: false,
            enable_events: true,
            deferred_changes: Vec::new(),
            quit: false,
            clip: None,
            clip_cut: false,
            clip_more: (Vec::new(), true),
            shape_clip: None,
            paste_chain: std::collections::HashMap::new(),
            paste_source: None,
            sorts: std::collections::HashMap::new(),
            root,
            view: None,
            running: None,
            events: Default::default(),
            host: Box::new(crate::host::NoDialogs),
            on_time: Vec::new(),
            on_key: Vec::new(),
            action_caller: None,
            addins: Default::default(),
            status_bar: None,
            com: Vec::new(),
            xml: Default::default(),
            com_addins: crate::comaddin::ComAddIns::default(),
            #[cfg(windows)]
            xlls: Vec::new(),
            #[cfg(windows)]
            xll_off: Vec::new(),
            #[cfg(windows)]
            rtd: None,
            ticks: 1_000_000,
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

    pub(crate) fn push_book_wb(
        &mut self,
        mut wb: BookWb<'v>,
        name: String,
        path: String,
    ) -> BookId {
        if let BookWb::Own(w) = &mut wb {
            w.set_book_name(&name);
        }
        #[cfg(windows)]
        if let (Some(h), BookWb::Own(w)) = (&self.rtd, &mut wb) {
            w.set_rtd_source(Some(std::sync::Arc::new(h.clone())));
        }
        let id = BookId(self.next_book);
        self.next_book += 1;

        let next_sheet = if path.is_empty() {
            wb.sheet_count() as u32 + 1
        } else {
            1
        };
        self.books.push(Book {
            id,
            wb,
            name,
            path,
            saved: true,
            next_sheet,
            next_chart: 1,
            next_macro: (1, 1),
            threads: Vec::new(),
            window: Default::default(),
            consolidate: Default::default(),
            vba: Default::default(),
            ide: Default::default(),
            warnings: Vec::new(),
            file_format: 51,
            compat: false,
            protect: None,
            sheet_prot: Default::default(),
            edit_ranges: Default::default(),
            page_setup: Default::default(),
            no_link_values: false,
            no_remote_refs: false,
            ro_recommended: false,
            draw_objects: -4104,
            package: None,
            read_only: false,
            controls: Default::default(),
            is_addin: false,
            props: Default::default(),
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

    pub(crate) fn has_book_named(&self, name: &str) -> bool {
        self.books.iter().any(|b| b.name.eq_ignore_ascii_case(name))
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
        crate::addr::set_compat(self.book(book)?.compat);
        let rect = crate::addr::fit_grid(rect).ok_or_else(OmError::app_defined)?;
        let track = match self.book(book)?.wb {
            BookWb::View(_) => Track::Fixed(sheet, rect),
            BookWb::Own(_) => Track::Live(self.book_mut(book)?.wb.track(sheet, rect)),
        };
        Ok(RangeRef {
            book,
            track,
            mode,
            axis: None,
        })
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
        if let Some(pin) = self.pins.remove(&id) {
            if !self.pins.values().any(|p| *p == pin) {
                if let Ok(b) = self.book_mut(pin.0) {
                    b.wb.drawing_mut(pin.1).listing.remove(&pin.2);
                }
            }
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

                    let mut values: Vec<Option<Variant>> = positional
                        .iter()
                        .map(|v| match v {
                            v if is_missing(v) => None,
                            Variant::Null => Some(Variant::Empty),
                            v => Some(v.clone()),
                        })
                        .collect();
                    while matches!(values.last(), Some(None)) {
                        values.pop();
                    }
                    let needed = m
                        .params
                        .iter()
                        .rposition(|p| matches!(p.opt, Opt::Required))
                        .map_or(0, |i| i + 1);
                    if values.len() < needed {
                        let mut e = OmError::app_defined_with("Invalid number of arguments.");
                        e.error_info = true;
                        return Err(e);
                    }
                    for v in &mut values {
                        v.get_or_insert(Variant::Empty);
                    }
                    let args = Args {
                        values,
                        put: None,
                        lcid: call.lcid,
                    };
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

        let lenient = matches!(
            (iface.name, candidates[0].name),
            ("PivotTable", "PivotFields") | ("_Chart", "SeriesCollection" | "Axes")
        );
        let never_get = call.flags & flags::METHOD == 0
            && matches!(
                (iface.name, candidates[0].name),
                ("Shapes", "Item")
                    | ("PivotTable", "GetPivotData")
                    | ("Axes", "Item")
                    | ("Comment", "Text" | "Next" | "Previous")
            );
        if never_get && call.flags & (flags::PUT | flags::PUTREF) == 0 {
            return Err(OmError::hresult(DISP_E_MEMBERNOTFOUND));
        }
        let has_args = has_args && !lenient;

        if call.flags & (flags::GET | flags::PUT | flags::PUTREF) == 0
            && iface.name == "_Application"
            && candidates[0].name == "CutCopyMode"
        {
            return Err(OmError::app_defined_with(
                "CutCopyMode method of Application class failed",
            ));
        }

        if call.flags & (flags::PUT | flags::PUTREF) != 0
            && iface.name == "_Workbook"
            && matches!(candidates[0].name, "FullName" | "ReadOnly" | "Path")
        {
            return Err(OmError::unable_to_set(candidates[0].name, "Workbook"));
        }
        let m = pick_member(&candidates, call.flags, has_args);
        let m = match (m, &obj) {
            (Err(_), Object::Validation(r))
                if candidates[0].name == "AlertStyle" && !self.validation_exists(r) =>
            {
                return Err(OmError::app_defined());
            }
            (m, _) => m?,
        };
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
        let value = match value {
            Some(v)
                if is_put
                    && !(iface.name == "_Chart" && m.name == "Visible")
                    && !(iface.name == "AutoCorrect" && m.name == "ReplaceText")
                    && !matches!(
                        iface.name,
                        "_Application"
                            | "_Workbook"
                            | "_Worksheet"
                            | "Range"
                            | "OLEObjects"
                            | "PivotField"
                            | "PageSetup"
                            | "Pane"
                            | "Outline"
                            | "Tab"
                            | "Window"
                            | "LineFormat"
                            | "FillFormat"
                            | "Scenario"
                    ) =>
            {
                if iface.name == "Style"
                    && matches!(v, Variant::Bstr(_))
                    && matches!(
                        m.name,
                        "WrapText" | "ShrinkToFit" | "Locked" | "FormulaHidden" | "AddIndent"
                    )
                {
                    return Err(OmError::unable_to_set(m.name, "Style"));
                }

                let v = match v {
                    Variant::R8(x) if iface.name == "Style" && m.name == "IndentLevel" => {
                        Variant::R8(x.trunc())
                    }

                    Variant::R8(x) if iface.name == "Style" && m.name == "Orientation" => {
                        Variant::R8(x.floor())
                    }
                    Variant::Bool(x) if iface.name == "Style" && m.name == "Orientation" => {
                        Variant::I4(i32::from(x))
                    }
                    v => v,
                };

                if iface.name == "Trendline" && matches!(m.name, "Forward" | "Backward") {
                    let v = xlcalc_variant::change_type(
                        &v,
                        xlcalc_variant::VarType::R8,
                        crate::conv::variant_locale(call.lcid),
                        0,
                    )
                    .map_err(|_| OmError::arg(crate::error::DISP_E_TYPEMISMATCH, 0))?;
                    Some(v)
                } else {
                    Some(coerce_put(m, v, call.lcid)?)
                }
            }
            v => v,
        };
        let nparams = m.params.len() - usize::from(is_put);
        let follows = !is_put && nparams == 0 && matches!(m.ret, Ty::Object(_) | Ty::Dispatch);
        if pos.len() + named.len() > nparams && !follows {
            if (m.name == "Count" && matches!(iface.name, "Sheets" | "Workbooks"))
                || (m.name == "Value2" && iface.name == "Range")
            {
                return Err(OmError::hresult(xlcalc_variant::HResult(
                    0x8002_0011_u32 as i32,
                )));
            }
            return Err(OmError::hresult(DISP_E_BADPARAMCOUNT));
        }

        if !is_put
            && nparams > 0
            && pos.is_empty()
            && named.is_empty()
            && not_optional(iface.name, m.name)
        {
            return Err(OmError::hresult(DISP_E_PARAMNOTOPTIONAL));
        }

        if !is_put && iface.name == "DocumentProperties" && m.name == "Item" && pos.is_empty() {
            return Err(OmError::hresult(DISP_E_BADPARAMCOUNT));
        }
        let (own, extra) = pos.split_at(pos.len().min(nparams));
        let put_arg = value.clone();
        let args = bind(iface.name, m, own, named, value, call.lcid)?;
        if foreign == Foreign::RefusesByName && matches!(args.get(0), Some(Variant::Bstr(_))) {
            return Err(OmError::app_defined());
        }

        self.after_call();
        self.flush_events();
        if let (true, Object::Range(r), Some(Variant::Bstr(t))) = (is_put, &obj, &put_arg) {
            self.prelink_text(r.book, t);
        }

        let stale: Vec<Stale> = if !is_put {
            Vec::new()
        } else if let Object::Multi(rs) = &obj {
            rs.iter()
                .filter_map(|r| self.rows_stale_begin(&Object::Range(*r), m.name))
                .collect()
        } else {
            self.rows_stale_begin(&obj, m.name).into_iter().collect()
        };
        self.widen_kept.clear();
        let result = self.member(&obj, m, &args, call);
        self.after_call();
        self.xlm_info_settle();
        if result.is_ok() {
            let mut refit = Vec::new();
            for st in stale {
                let (book, sheet) = (st.book, st.sheet);
                if self.rows_stale_end(st, put_arg.as_ref()) {
                    refit.push((book, sheet));
                }
            }

            let kept = std::mem::take(&mut self.widen_kept);
            for (book, sheet) in refit {
                if let Ok(b) = self.book_mut(book) {
                    let rows: Vec<(u32, u32)> = kept
                        .iter()
                        .filter(|k| k.0 == sheet)
                        .map(|k| (k.1, k.2))
                        .collect();
                    b.wb.auto_keep(sheet, &rows);
                }
            }
        }
        self.widen_kept.clear();
        self.flush_events();

        let result = match result {
            Err(e)
                if is_put
                    && matches!(iface.name, "Range" | "Scenario")
                    && e.scode == 0x8002_0005_u32 as i32
                    && e.detail.as_deref() != Some(INDEX_MISMATCH) =>
            {
                Err(OmError::unable_to_set(m.name, iface.name))
            }
            r => r,
        };

        let result = match result {
            Err(e)
                if iface.name == "Range"
                    && e.scode == crate::error::SCODE_TYPE_MISMATCH
                    && matches!(
                        m.name,
                        "Insert"
                            | "Delete"
                            | "Copy"
                            | "Cut"
                            | "AutoFill"
                            | "AutoFilter"
                            | "Sort"
                            | "PasteSpecial"
                    ) =>
            {
                Err(OmError::app_defined_with(&format!(
                    "{} method of Range class failed",
                    m.name
                )))
            }
            r => r,
        };
        let mut result = result?;
        if foreign == Foreign::EmptyText {
            result = Variant::Bstr(String::new());
        }
        if extra.is_empty() {
            return Ok(result);
        }

        let Variant::Dispatch(Some(inner)) = result else {
            return Err(if matches!(result, Variant::Dispatch(None)) {
                OmError::app_defined()
            } else {
                OmError::hresult(DISP_E_BADPARAMCOUNT)
            });
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

fn not_optional(interface: &str, member: &str) -> bool {
    let list: &[&str] = match interface.trim_start_matches('_') {
        "Styles" => &["Add", "Merge"],
        "Scenarios" => &["Item", "Merge"],
        "OLEObjects" => &["Item"],
        "Characters" => &["Insert"],
        "Workbook" => &[
            "ChangeFileAccess",
            "ChangeLink",
            "DeleteNumberFormat",
            "LinkInfo",
            "OpenLinks",
            "RunAutoMacros",
            "SendMail",
            "SetLinkOnData",
        ],
        "Application" => &[
            "CentimetersToPoints",
            "GetCustomListNum",
            "InchesToPoints",
            "DDEExecute",
            "DDEInitiate",
            "DDEPoke",
            "DDERequest",
            "DDETerminate",
            "DeleteCustomList",
            "GetCustomListContents",
        ],
        "Range" => &[
            "AdvancedFilter",
            "AutoComplete",
            "ColumnDifferences",
            "EditionOptions",
            "RowDifferences",
            "SpecialCells",
            "SubscribeTo",
        ],
        "Worksheet" => &["Evaluate", "_Evaluate"],
        "Workbooks" => &["Open", "_Open", "OpenText"],
        _ => &[],
    };
    list.contains(&member)
}

pub(crate) fn coerce_put(m: &Member, v: Variant, lcid: u32) -> Result<Variant, OmError> {
    use xlcalc_variant::{change_type, VarType};
    let Some(p) = m.params.last() else {
        return Ok(v);
    };
    let to = match p.ty {
        Ty::Bool => VarType::Bool,
        Ty::I4 | Ty::Int | Ty::Enum(_) => VarType::I4,
        Ty::R8 => VarType::R8,
        Ty::R4 => VarType::R4,
        Ty::Bstr => VarType::Bstr,
        _ => return Ok(v),
    };

    if is_missing(&v) || (to == VarType::Bstr && matches!(v, Variant::Bool(_) | Variant::Empty)) {
        return Ok(v);
    }
    change_type(&v, to, crate::conv::variant_locale(lcid), 0).map_err(|h| {
        if h == HResult::DISP_E_OVERFLOW {
            OmError::hresult(h)
        } else {
            OmError::arg(crate::error::DISP_E_TYPEMISMATCH, 0)
        }
    })
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

    if call_flags & flags::METHOD != 0 && candidates.iter().all(|m| m.name == "Subtotals") {
        if let Some(m) = find(Get) {
            return Ok(m);
        }
    }
    if let Some(m) = find(Func) {
        if call_flags & flags::METHOD != 0
            || !has_args
            || matches!(
                m.name,
                "Union" | "Intersect" | "SpecialCells" | "Evaluate" | "_Evaluate"
            )
        {
            return Ok(m);
        }
    }
    Err(OmError::hresult(DISP_E_MEMBERNOTFOUND))
}

fn bind(
    iface: &str,
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
        if let (Some(v), Ty::Object("Range"), "SetSourceData") = (&values[i], p.ty, m.name) {
            if i < pos.len() && !matches!(v, Variant::Dispatch(_)) {
                return Err(OmError::arg(
                    DISP_E_TYPEMISMATCH,
                    (pos.len() - 1 - i) as u32,
                ));
            }
        }

        let own_check = matches!(
            m.name,
            "Sort"
                | "AutoFill"
                | "AutoFilter"
                | "PasteSpecial"
                | "CentimetersToPoints"
                | "InchesToPoints"
                | "ConvertFormula"
                | "Resize"
        );

        let by_value =
            iface == "WorksheetFunction" && matches!(values[i], Some(Variant::Dispatch(_)));
        if let (Some(v), Some(vt), false) = (&values[i], scalar_vt(p.ty), own_check || by_value) {
            if i < pos.len()
                && xlcalc_variant::change_type(v, vt, crate::conv::variant_locale(lcid), 0).is_err()
            {
                return Err(OmError::arg(
                    DISP_E_TYPEMISMATCH,
                    (pos.len() - 1 - i) as u32,
                ));
            }
        }
        if values[i].is_some() {
            continue;
        }
        match p.opt {
            Opt::Required
                if m.name == "Add"
                    && m.params.get(1).is_some_and(|q| q.name == "LinkToContent") =>
            {
                if i >= pos.len() {
                    return Err(if i < 2 {
                        OmError::hresult(DISP_E_BADPARAMCOUNT)
                    } else {
                        OmError::exception(0x8007_0057_u32 as i32, None)
                    });
                }
                if i < 2 {
                    return Err(OmError::hresult(DISP_E_PARAMNOTOPTIONAL));
                }
            }

            Opt::Required if i == 0 && i < pos.len() && m.name == "Item" && iface == "Range" => {}

            Opt::Required
                if i < pos.len()
                    && m.name == "Item"
                    && matches!(iface, "ListRows" | "ListColumns") => {}

            Opt::Required
                if i < pos.len() && m.name == "Add" && p.name == "Name" && m.params.len() == 2 => {}
            Opt::Required
                if matches!(
                    m.name,
                    "Union"
                        | "Intersect"
                        | "Subtotal"
                        | "Evaluate"
                        | "AutoFill"
                        | "FillAcrossSheets"
                ) =>
            {
                if i >= pos.len() {
                    return Err(OmError::hresult(DISP_E_PARAMNOTOPTIONAL));
                }
            }

            Opt::Required if m.name == "ConvertFormula" => {
                if i >= pos.len() {
                    return Err(OmError::hresult(DISP_E_PARAMNOTOPTIONAL));
                }
            }

            Opt::Required if i < pos.len() && m.name == "Range" && p.name == "Cell1" => {
                return Err(OmError::app_defined());
            }
            Opt::Required
                if i < pos.len()
                    && matches!(
                        (m.name, p.name),
                        ("Find", "What")
                            | ("Replace", "What")
                            | ("Replace", "Replacement")
                            | ("Item", "RowIndex")
                            | ("CentimetersToPoints", "Centimeters")
                            | ("InchesToPoints", "Inches")
                            | ("GetCustomListNum", "ListArray")
                    ) => {}
            Opt::Required if i >= pos.len() => {
                return Err(OmError::hresult(DISP_E_BADPARAMCOUNT));
            }
            Opt::Required if p.ty != Ty::Variant => {
                return Err(OmError::hresult(DISP_E_PARAMNOTOPTIONAL));
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

fn scalar_vt(ty: Ty) -> Option<xlcalc_variant::VarType> {
    use xlcalc_variant::VarType;
    match ty {
        Ty::I4 | Ty::Int | Ty::Enum(_) => Some(VarType::I4),
        Ty::Bool => Some(VarType::Bool),
        Ty::R4 => Some(VarType::R4),
        Ty::R8 => Some(VarType::R8),
        Ty::Bstr => Some(VarType::Bstr),
        _ => None,
    }
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
        if let Some(r) = self.pivot_intercept(obj, m, a) {
            return r;
        }
        if let Object::Font(o) | Object::Interior(o) | Object::Borders(o) | Object::Border(o, _) =
            obj
        {
            self.note_style_owner_format(o);
        }
        let compat = self
            .object_book(obj)
            .and_then(|b| self.book(b).ok())
            .is_some_and(|b| b.compat);
        crate::addr::set_compat(compat);
        let r = match obj {
            Object::Application => self.application_member(m, a),
            Object::Workbooks => self.workbooks_member(m, a),
            Object::Windows => self.windows_member(m, a),
            Object::BookWindows(b) => self.book_windows_member(*b, m, a),
            Object::Window(b, n) => self.window_member(*b, *n, m, a),
            Object::Panes(b, n) => self.panes_member(*b, *n, m, a),
            Object::Pane(b, n, i) => self.pane_member(*b, *n, *i, m, a),
            Object::Workbook(b) => self.workbook_member(*b, m, a),
            Object::Sheets(b, k) => self.sheets_member(*b, *k, m, a),
            Object::Worksheet(b, s) => self.worksheet_member(*b, *s, m, a),
            Object::Chart(b, s) => self.chart_member(*b, *s, m, a),
            Object::Module(b, s) | Object::Dialog(b, s) => self.legacy_sheet_member(*b, *s, m, a),
            Object::Range(r) => self.range_member(r, m, a),
            Object::Multi(rs) => self.multi_member(rs, m, a),
            Object::Areas(rs) => self.areas_member(rs, m, a),
            Object::Names(b, s) => self.names_member(*b, *s, m, a),
            Object::Name(b, s, k) => self.name_member(*b, *s, k, m, a),
            Object::ListObjects(b, s) => self.list_objects_member(*b, *s, m, a),
            Object::Table(b, n) => self.list_object_member(*b, n, m, a),
            Object::ListColumns(b, n) => self.list_columns_member(*b, n, m, a),
            Object::ListColumn(b, n, i) => self.list_column_member(*b, n, *i, m, a),
            Object::TableStyle(n) => self.table_style_member(n, m, a),
            Object::ListRows(b, n) => self.list_rows_member(*b, n, m, a),
            Object::ListRow(b, n, i) => self.list_row_member(*b, n, *i, m),
            Object::Enumerator(..) => Err(OmError::hresult(DISP_E_MEMBERNOTFOUND)),
            Object::Sort(o) => self.sort_member(o, m, a),
            Object::SortFields(o) => self.sort_fields_member(o, m, a),
            Object::SortField(o, i) => self.sort_field_member(o, *i, m, a),
            Object::SortOnValue(o, i, _) => self.sort_on_value_member(o, *i, m, a),
            Object::AutoFilter(b, s, t) => {
                let t = t.clone();
                self.autofilter_member(*b, *s, t.as_deref(), m)
            }
            Object::Filters(b, s) => self.autofilter_cols_member(*b, *s, m, a),
            Object::Filter(b, s, i) => self.autofilter_col_member(*b, *s, *i, m),
            Object::WorksheetFunction => self.worksheet_function(m, a, true),
            Object::MultiThreadedCalculation => self.multi_threaded_member(m, a),
            Object::Font(crate::style::StyleOwner::Range(r)) => {
                self.char_font_member(r, crate::rangeparts::CharSpan::WHOLE, m, a)
            }
            Object::Font(o) => self.font_member(o, m, a),
            Object::Interior(o) => self.interior_member(o, m, a),
            Object::Borders(o) => self.borders_member(o, m, a),
            Object::Border(o, i) => self.border_member(o, *i, m, a),
            Object::Style(b, n) => self.style_member(*b, n, m, a),
            Object::Styles(b) => self.styles_member(*b, m, a),
            Object::Scenarios(b, s) => self.scenarios_member(*b, *s, m, a),
            Object::OleObjects(..) => self.ole_objects_member(m, a),
            Object::Scenario(b, s, n) => self.scenario_member(*b, *s, n, m, a),
            Object::CustomProperties(b, s) => self.custom_properties_member(*b, *s, m, a),
            Object::CommentShape(b, s, i) => self.comment_shape_member(*b, *s, *i, m, a),
            Object::CommentTextFrame(b, s, i) => self.comment_textframe_member(*b, *s, *i, m, a),
            Object::CustomProperty(b, s, i) => self.custom_property_member(*b, *s, *i, m, a),
            Object::Spark(b, s, p) => self.spark_member(*b, *s, p, m, a),
            Object::Slicer(b, p) => self.slicer_member(*b, p, m, a),
            Object::VBProject(b) => self.vbproject_member(*b, m, a),
            Object::VBComponents(b) => self.vbcomponents_member(*b, m, a),
            Object::VBComponent(b, c) => self.vbcomponent_member(*b, *c, m, a),
            Object::CodeModule(b, c) => self.codemodule_member(*b, *c, m, a),
            Object::Controls(b, s, c) => self.controls_member(*b, *s, *c, m, a),
            Object::Control(r) => self.control_member(r, m, a),
            Object::Draw(d) => self.draw_member(d, m, a),
            Object::ControlFormat(r) => self.control_format_member(r, m, a),
            Object::AddIns | Object::AddIns2 => self.addins_member(m, a),
            Object::PageBreaks(rows, owner) => self.page_breaks_member(*rows, *owner, m, a),
            Object::PageBreak(rows, b, s, at) => self.page_break_member((*rows, *b, *s, *at), m, a),
            Object::AddIn(k) => self.addin_member(k, m, a),
            Object::ComAddIns => self.com_addins_member(m, a),
            Object::ComAddIn(k) => self.com_addin_member(k, m, a),
            Object::FormatConditions(r) => self.format_conditions_member(r, m, a),
            Object::FormatCondition(b, s, i, _, sc) => {
                self.format_condition_member(*b, *s, *i, *sc, m, a)
            }
            Object::ScaleCriteria(b, s, i) => self.scale_criteria_member(*b, *s, *i, m, a),
            Object::ScaleCriterion(b, s, i, k) => self.scale_criterion_member(*b, *s, *i, *k, m, a),
            Object::ScaleColor(b, s, i, k) => self.scale_color_member(*b, *s, *i, *k, m, a),
            Object::CfPart(b, s, i, p) => self.cf_part_member(*b, *s, *i, *p, m, a),
            Object::IconSets(b) => self.icon_sets_member(*b, m, a),
            Object::IconSetDef(_, n) => self.icon_set_def_member(*n, m),
            Object::Validation(r) => self.validation_member(r, m, a),
            Object::ValidationMulti(r) => self.validation_multi_member(r, m),
            Object::DisplayFormat(r) => self.display_format_member(r, m, a),
            Object::Hyperlinks(b, s, r) => self.hyperlinks_member(*b, *s, *r, m, a),
            Object::Comments(b, s) => self.comments_member(*b, *s, m, a),
            Object::Comment(b, s, id) => self.comment_member(*b, *s, *id, m, a),
            Object::CommentThreaded(b, s, r, c) => self.thread_member(*b, *s, *r, *c, m, a),
            Object::ThreadList(b, s, cell) => self.thread_list_member(*b, *s, *cell, m),
            Object::Hyperlink(b, s, id) => self.hyperlink_member(*b, *s, *id, m, a),
            Object::DocProp(b, n) => self.doc_prop_member(*b, &n.clone(), m, a),

            Object::Stub("Dialogs") if m.name == "Count" => Ok(Variant::I4(1531)),

            Object::Stub("AutoCorrect")
                if m.name == "ReplacementList"
                    && m.kind == InvokeKind::Get
                    && a.get(0).is_none() =>
            {
                let pairs: Vec<(&str, &str)> = include_str!("autocorrect_list.txt")
                    .lines()
                    .filter_map(|l| l.split_once('\t'))
                    .collect();
                let n = pairs.len() as u32;
                let mut data: Vec<Variant> =
                    pairs.iter().map(|p| Variant::Bstr(p.0.into())).collect();
                data.extend(pairs.iter().map(|p| Variant::Bstr(p.1.into())));
                Ok(Variant::Array(Box::new(xlcalc_variant::SafeArray {
                    elem: xlcalc_variant::VarType::Variant,
                    bounds: vec![(1, n), (1, 2)],
                    data,
                })))
            }
            Object::Stub(name) => match self.bag_member(crate::bag::Store::App, name, name, m, a) {
                Some(res) => res,
                None => Err(unsupported("object", m)),
            },
            Object::DocProps(b, custom) => self.doc_props_member(*b, *custom, m, a),
            Object::Outline(b, s) => self.outline_member(*b, *s, m, a),
            Object::Tab(b, s) => self.tab_member(*b, *s, m, a),
            Object::Pages(b, s) => self.pages_member(*b, *s, m),
            Object::Protection(b, s) => self.protection_member(*b, *s, m),
            Object::AllowEditRanges(b, s) => self.allow_edit_ranges_member(*b, *s, m, a),
            Object::AllowEditRange(b, s, i) => self.allow_edit_range_member(*b, *s, *i, m, a),
            Object::PageSetup(b, s) => self.page_setup_member(*b, *s, m, a),
            Object::Characters(r, span) => self.characters_member(r, *span, m, a),
            Object::CharFont(r, span) => self.char_font_member(r, *span, m, a),
            Object::Phonetic(r) => self.phonetic_member(r, m),
            Object::Pivot(p) => self.pivot_member(p, m, a),
        };
        let r = self.stub_fallback(m, a, r);
        let changes = matches!(
            (obj.interface_name(), m.name),
            (
                "Range",
                "ClearContents" | "Clear" | "ClearFormats" | "Delete" | "Insert"
            ) | ("Names", "Add")
                | ("Name", "Delete")
                | ("Sheets", "Add")
                | ("_Worksheet" | "_Chart", "Delete")
                | ("ListObjects", "Add")
                | ("ListObject", "Delete" | "Unlist" | "Resize")
                | ("Sort", "Apply")
        );
        if r.is_ok()
            && m.name != "Saved"
            && !matches!(m.name, "Iteration" | "MaxIterations" | "MaxChange")
            && (changes || call.flags & (flags::PUT | flags::PUTREF) != 0)
        {
            self.mark_unsaved(obj);
        }
        r
    }

    fn stub_fallback(
        &mut self,
        m: &'static Member,
        _a: &Args,
        r: Result<Variant, OmError>,
    ) -> Result<Variant, OmError> {
        let Err(e) = &r else { return r };
        if e.scode != crate::error::vba_scode(438)
            || m.kind != InvokeKind::Get
            || !m.params.is_empty()
        {
            return r;
        }
        match m.name {
            "ActiveSlicer" | "SharedWorkspace" | "PreviousSelections" => {
                return Ok(Variant::Dispatch(None));
            }
            "DocumentLibraryVersions" => {
                return Err(OmError::exception(0x8004_1050_u32 as i32, None))
            }
            "ContentTypeProperties" => {
                return Err(OmError::exception(0x8004_1403_u32 as i32, None))
            }
            "FormatStaleValues" | "MaxSupportedCompatibilityVersion" => {
                return Err(OmError::exception(0x8000_4001_u32 as i32, None));
            }
            "MailEnvelope" => return Err(OmError::exception(0x8000_4005_u32 as i32, None)),
            "ServerPolicy" => return Err(OmError::exception(0x8007_0490_u32 as i32, None)),
            "SensitivityLabel" => return Err(OmError::exception(0x8004_030A_u32 as i32, None)),
            _ => {}
        }
        match m.ret {
            Ty::Object(n) => {
                let n = match n {
                    "CommandBars" => "_CommandBars",
                    "CustomXMLParts" => "_CustomXMLParts",
                    n => n,
                };
                Ok(self.alloc(Object::Stub(n)))
            }
            _ => r,
        }
    }

    fn mark_unsaved(&mut self, obj: &Object) {
        let book = self.object_book(obj);
        if let Some(b) = book.and_then(|b| self.book_mut(b).ok()) {
            b.saved = false;
        }
    }

    fn object_book(&self, obj: &Object) -> Option<BookId> {
        match obj {
            Object::Workbook(b)
            | Object::Sheets(b, _)
            | Object::Worksheet(b, _)
            | Object::Chart(b, _)
            | Object::Module(b, _)
            | Object::Dialog(b, _)
            | Object::Names(b, _)
            | Object::ListObjects(b, _)
            | Object::Table(b, _)
            | Object::AutoFilter(b, ..)
            | Object::Sort(SortOwner::Table(b, _))
            | Object::Sort(SortOwner::Sheet(b, _))
            | Object::Sort(SortOwner::Filter(b, _))
            | Object::Name(b, ..) => Some(*b),
            Object::Range(r) => Some(r.book),
            Object::Multi(rs) | Object::Areas(rs) => rs.first().map(|r| r.book),
            Object::Style(b, _) | Object::Styles(b) => Some(*b),
            Object::Scenarios(b, _)
            | Object::OleObjects(b)
            | Object::Scenario(b, ..)
            | Object::CustomProperties(b, _)
            | Object::CommentShape(b, ..)
            | Object::CommentTextFrame(b, ..)
            | Object::CustomProperty(b, ..)
            | Object::Spark(b, ..)
            | Object::Slicer(b, ..) => Some(*b),
            Object::Controls(b, ..) => Some(*b),
            Object::Hyperlinks(b, ..) | Object::Hyperlink(b, ..) => Some(*b),
            Object::Comments(b, ..)
            | Object::Comment(b, ..)
            | Object::CommentThreaded(b, ..)
            | Object::ThreadList(b, ..) => Some(*b),
            Object::Pivot(p) => Some(p.book()),
            Object::Draw(d) => Some(d.book),
            Object::Control(r) | Object::ControlFormat(r) => Some(r.book),
            Object::DocProp(b, _)
            | Object::DocProps(b, _)
            | Object::Protection(b, _)
            | Object::Outline(b, _)
            | Object::Tab(b, _)
            | Object::Pages(b, _)
            | Object::AllowEditRanges(b, _)
            | Object::AllowEditRange(b, _, _)
            | Object::PageSetup(b, _) => Some(*b),
            Object::FormatConditions(r)
            | Object::Validation(r)
            | Object::ValidationMulti(r)
            | Object::DisplayFormat(r)
            | Object::Characters(r, _)
            | Object::CharFont(r, _)
            | Object::Phonetic(r) => Some(r.book),
            Object::FormatCondition(b, ..)
            | Object::ScaleCriteria(b, ..)
            | Object::ScaleCriterion(b, ..)
            | Object::ScaleColor(b, ..)
            | Object::CfPart(b, ..)
            | Object::IconSets(b)
            | Object::IconSetDef(b, _) => Some(*b),
            Object::Font(o) | Object::Interior(o) | Object::Borders(o) | Object::Border(o, _) => {
                Some(match o {
                    crate::style::StyleOwner::Range(r) => r.book,
                    crate::style::StyleOwner::Areas(rs) => rs.first().map_or(BookId(0), |r| r.book),
                    crate::style::StyleOwner::Named(b, _) => *b,
                    crate::style::StyleOwner::Display(r) => r.book,
                    crate::style::StyleOwner::Cond(b, ..) => *b,
                })
            }
            _ => self.active,
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

    Xf,

    Border,

    Scan,
}

pub(crate) struct Stale {
    book: BookId,
    sheet: SheetId,
    rect: Rect,
    kind: StaleKind,
    member: String,
    snap: Vec<(u32, u32)>,
    fmt: Option<xlcalc::refit::FormatSnap>,
    test: xlcalc::refit::RefitTest,
}

impl Application<'_> {
    fn rows_stale_begin(&mut self, obj: &Object, member: &str) -> Option<Stale> {
        let r = match obj {
            Object::Range(r) => r,
            Object::Font(crate::style::StyleOwner::Range(r))
            | Object::Borders(crate::style::StyleOwner::Range(r))
            | Object::Border(crate::style::StyleOwner::Range(r), _) => r,
            _ => return None,
        };
        let border = matches!(obj, Object::Borders(_) | Object::Border(..));
        let kind = match member {
            _ if border => StaleKind::Border,
            "Locked" | "FormulaHidden" => return None,
            _ if matches!(obj, Object::Font(_)) => StaleKind::Xf,
            "MergeCells" | "AddIndent" | "ReadingOrder" => StaleKind::Scan,
            "NumberFormat"
            | "HorizontalAlignment"
            | "VerticalAlignment"
            | "IndentLevel"
            | "ShrinkToFit"
            | "WrapText"
            | "Orientation"
            | "Style" => StaleKind::Xf,
            "ColumnWidth" | "UseStandardWidth" => StaleKind::Width,
            "Formula" | "Formula2" | "FormulaLocal" | "Formula2Local" | "FormulaR1C1"
            | "FormulaR1C1Local" | "Formula2R1C1" | "Formula2R1C1Local" | "Value" | "Value2" => {
                StaleKind::Entry
            }
            "Hidden" | "RowHeight" | "Height" | "Width" => return None,
            _ => StaleKind::Format,
        };
        let (sheet, rect) = self.resolve(r).ok()?;
        let snap = if !matches!(kind, StaleKind::Width | StaleKind::Entry) {
            Vec::new()
        } else {
            self.book(r.book).ok()?.wb.auto_snapshot(sheet)
        };
        let fmt = if matches!(kind, StaleKind::Xf | StaleKind::Format | StaleKind::Border) {
            Some(self.book(r.book).ok()?.wb.format_snapshot(sheet, rect))
        } else {
            None
        };
        Some(Stale {
            book: r.book,
            sheet,
            rect,
            kind,
            member: member.to_string(),
            snap,
            fmt,
            test: match kind {
                StaleKind::Format => xlcalc::refit::RefitTest::AnyChange,
                StaleKind::Border => xlcalc::refit::RefitTest::HorizontalBorder,
                _ => xlcalc::refit::RefitTest::Xf {
                    font_put: matches!(obj, Object::Font(_)),
                },
            },
        })
    }

    fn rows_stale_end(&mut self, st: Stale, arg: Option<&Variant>) -> bool {
        let Ok(b) = self.book_mut(st.book) else {
            return false;
        };
        let wb = &mut b.wb;
        let constant = !matches!(arg, Some(Variant::Bstr(t)) if t.starts_with('='));
        let refit = match st.kind {
            StaleKind::Width => false,

            StaleKind::Format => st.member != "NumberFormat",
            StaleKind::Border | StaleKind::Xf => false,
            StaleKind::Scan => {
                wb.post_scan_refit(st.sheet, st.rect.r0, st.rect.r1);
                true
            }
            StaleKind::Entry => constant && wb.holds_wrapped_text(st.sheet, st.rect),
        };
        if st.kind == StaleKind::Entry {
            wb.note_table_rows(st.sheet, st.rect);
        }
        let mut xf_rows = Vec::new();
        if let Some(f) = &st.fmt {
            xf_rows = wb.refit_rows_after(f, st.test);
            if st.kind == StaleKind::Border {
                if !xf_rows.is_empty() {
                    wb.post_scan_refit(st.sheet, st.rect.r0, st.rect.r1);
                }
            } else {
                for &r in &xf_rows {
                    wb.auto_refit(st.sheet, r, r);
                }

                if st.member == "IndentLevel" && !xf_rows.is_empty() {
                    wb.post_scan_refit(st.sheet, st.rect.r0, st.rect.r1);
                }
            }
        }
        let refit = refit || !xf_rows.is_empty();
        if refit
            && matches!(st.kind, StaleKind::Format | StaleKind::Entry)
            && st.member != "NumberFormat"
        {
            wb.auto_refit(st.sheet, st.rect.r0, st.rect.r1);
        }
        let keep: Vec<(u32, u32)> = st
            .snap
            .iter()
            .copied()
            .filter(|(r, _)| !(refit && (st.rect.r0..=st.rect.r1).contains(r)))
            .collect();
        wb.auto_settle(st.sheet, &keep);
        refit
    }
}
