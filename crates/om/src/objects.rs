use xlcalc::{
    CalcMode, DateSystem, EditError, Notice, Rect, SheetId, SheetKind, SheetState, Workbook,
};
use xlcalc_variant::{ObjectId, SafeArray, VarType, Variant};

use crate::addr::{parse_areas_text, parse_range_text, MAX_COLS, MAX_ROWS};
use crate::app::{unsupported, Application, BookId, Mode, Object, SheetsKind, SortOwner};
use crate::conv::Args;
use crate::error::OmError;
use crate::typelib::{InvokeKind, Member};

#[derive(Clone)]
struct NameEntry {
    shown: String,
    bare: String,
    scope: Option<SheetId>,
}

const BAD_NAME: &str = "The syntax of this name isn't correct.\n\nVerify that the name:\n\t-Starts with a letter or underscore (_)\n\t-Doesn't include a space or character that isn't allowed\n\t-Doesn't conflict with an existing name in the workbook.";

pub(crate) fn edit_error(e: EditError) -> OmError {
    let detail = e.to_string();
    match e {
        EditError::BadName(_) => OmError::app_defined_with(BAD_NAME),
        EditError::OutOfScope(what) => OmError::app_defined_with(&format!("out of scope: {what}")),
        _ => OmError::app_defined(),
    }
    .with_detail(detail)
}

pub(crate) fn whole_sheet() -> Rect {
    Rect::new(0, 0, MAX_ROWS - 1, MAX_COLS - 1)
}

impl Application<'_> {
    pub(crate) fn alloc_enumerator(&mut self, items: Vec<Variant>) -> Variant {
        match self.alloc(Object::Enumerator(items, 0)) {
            Variant::Dispatch(id) => Variant::Unknown(id),
            other => other,
        }
    }

    fn range_target(
        &self,
        book: BookId,
        sheet: SheetId,
        v: &Variant,
    ) -> Result<(SheetId, Rect), OmError> {
        match v {
            Variant::Bstr(t) => {
                parse_range_text(&self.book(book)?.wb, sheet, t).ok_or_else(OmError::app_defined)
            }
            Variant::Dispatch(Some(id)) => match self.arena.get(*id) {
                Some(Object::Range(r)) => self.resolve(r),
                _ => Err(OmError::app_defined()),
            },
            _ => Err(OmError::app_defined()),
        }
    }

    fn range_of_args(
        &mut self,
        book: BookId,
        sheet: SheetId,
        a: &Args,
        own_sheet: bool,
    ) -> Result<Variant, OmError> {
        let first = a.get(0).ok_or_else(OmError::app_defined)?;

        if let (Variant::Bstr(t), None) = (first, a.get(1)) {
            if t.contains([',', ' ', '(']) {
                let (s, rects) = parse_areas_text(&self.book(book)?.wb, sheet, t)
                    .ok_or_else(OmError::app_defined)?;
                return self.new_multi(book, s, &rects, Mode::Cells);
            }
        }
        let (s, mut rect) = self.range_target(book, sheet, first)?;

        if own_sheet && s != sheet {
            return Err(OmError::app_defined());
        }
        if let Some(second) = a.get(1) {
            let (s2, r2) = self.range_target(book, sheet, second)?;
            if s2 != s {
                return Err(OmError::app_defined());
            }
            rect = rect.union_bounds(&r2);
        }
        self.new_range(book, s, rect, Mode::Cells)
    }

    fn sheet_range(
        &mut self,
        book: BookId,
        sheet: SheetId,
        mode: Mode,
    ) -> Result<Variant, OmError> {
        self.new_range(book, sheet, whole_sheet(), mode)
    }

    pub(crate) fn application_member(
        &mut self,
        m: &'static Member,
        a: &Args,
    ) -> Result<Variant, OmError> {
        use InvokeKind::*;
        let put = m.kind == Put;
        let flag = |this: &mut Application, which: &str| -> Result<Variant, OmError> {
            let slot = match which {
                "DisplayAlerts" => &mut this.display_alerts,
                "Visible" => &mut this.visible,
                "EnableEvents" => &mut this.enable_events,
                "AskToUpdateLinks" => &mut this.ask_to_update_links,
                _ => &mut this.screen_updating,
            };
            if put {
                *slot = match &a.put {
                    Some(v) => crate::conv::Args {
                        values: vec![Some(v.clone())],
                        put: None,
                        lcid: a.lcid,
                    }
                    .bool(0)?
                    .unwrap_or(false),
                    None => false,
                };
                Ok(Variant::Empty)
            } else {
                Ok(Variant::Bool(*slot))
            }
        };
        match (m.name, m.kind) {
            ("Workbooks", Get) => Ok(self.alloc(Object::Workbooks)),
            ("AddIns", Get) => Ok(self.alloc(Object::AddIns)),
            ("AddIns2", Get) => Ok(self.alloc(Object::AddIns2)),
            ("COMAddIns", Get) => Ok(self.alloc(Object::ComAddIns)),
            ("StartupPath", Get) => Ok(Variant::Bstr(
                r"C:\Users\zac\AppData\Roaming\Microsoft\Excel\XLSTART".into(),
            )),
            ("StatusBar", Get) => Ok(match &self.status_bar {
                Some(t) => Variant::Bstr(t.clone()),
                None => Variant::Bool(false),
            }),
            ("StatusBar", Put) => {
                let v = match a.put.as_ref() {
                    Some(Variant::Bool(b)) => if *b { "TRUE" } else { "FALSE" }.to_string(),
                    _ => Application::put_value(a).string(0)?.unwrap_or_default(),
                };
                self.status_bar = Some(v);
                Ok(Variant::Empty)
            }
            ("AltStartupPath", Get) => Ok(Variant::Bstr(String::new())),
            ("LibraryPath", Get) => Ok(Variant::Bstr(crate::addins::LIBRARY.into())),
            ("UserLibraryPath", Get) => Ok(Variant::Bstr(
                r"C:\Users\zac\AppData\Roaming\Microsoft\AddIns\".into(),
            )),
            ("Windows", Get) => Ok(self.alloc(Object::Windows)),
            ("ActiveWindow", Get) => match self.current_window() {
                Some((b, n)) => Ok(self.alloc(Object::Window(b, n))),
                None => Ok(Variant::Dispatch(None)),
            },
            ("DefaultFilePath", Get) => Ok(Variant::Bstr(self.default_file_path.clone())),
            ("DefaultFilePath", Put) => {
                self.default_file_path = Application::put_value(a).string(0)?.unwrap_or_default();
                Ok(Variant::Empty)
            }
            ("CalculateBeforeSave", Get) => Ok(Variant::Bool(self.calculate_before_save)),
            ("CalculateBeforeSave", Put) => {
                self.calculate_before_save = self.put_bool(a)?;
                Ok(Variant::Empty)
            }

            ("CalculationVersion", Get) => Ok(Variant::I4(191_029)),

            ("ActiveWorkbook", Get) => match self.active {
                Some(id) => Ok(self.alloc(Object::Workbook(id))),
                None => Ok(Variant::Dispatch(None)),
            },
            ("ActiveSheet", Get) if self.active.is_none() => Ok(Variant::Dispatch(None)),
            ("ActiveSheet", Get) => {
                let b = self.active_book()?;
                let (id, sheet) = (b.id, b.wb.active_sheet());
                Ok(self.sheet_object(id, sheet))
            }
            ("ActiveChart", Get) => {
                let b = self.active_book()?;
                let (id, sheet) = (b.id, b.wb.active_sheet());
                Ok(self.active_chart(id, sheet))
            }
            ("Selection", Get) => self.app_selection(),
            ("Goto", Func) => self.app_goto(a),
            ("ActiveCell", Get) => {
                let b = self.active_book()?;
                let (id, sheet) = (b.id, b.wb.active_sheet());
                let (r, c) = b.wb.active_cell();
                self.new_range(id, sheet, Rect::cell(r, c), Mode::Cells)
            }
            ("Calculate", Func) => {
                self.calculate_linked(false)?;
                Ok(Variant::Bool(true))
            }
            ("CalculateFull", Func) => {
                self.calculate_linked(true)?;
                Ok(Variant::Empty)
            }
            ("CalculateFullRebuild", Func) => {
                self.with_active(|wb| {
                    wb.rebuild();
                })?;
                Ok(Variant::Empty)
            }
            ("Calculation", Get) => {
                let wb = &self.active_book()?.wb;

                Ok(match (wb.settings().mode, wb.is_semiautomatic()) {
                    (CalcMode::Manual, _) => Variant::I4(-4135),
                    (CalcMode::Automatic, true) => Variant::R8(2.0),
                    (CalcMode::Automatic, false) => Variant::I4(-4105),
                })
            }

            ("Calculation", Put) if matches!(a.put, Some(Variant::Bstr(_))) => {
                match self.put_f64(a) {
                    Ok(_) => Ok(Variant::Empty),
                    Err(_) => Err(OmError::unable_to_set("Calculation", "Application")),
                }
            }
            ("Calculation", Put) => {
                let mode = self
                    .put_i32(a)
                    .map_err(|_| OmError::unable_to_set("Calculation", "Application"))?;
                self.with_active(|wb| {
                    match mode {
                        -4105 => wb.set_mode(CalcMode::Automatic),
                        2 => wb.set_semiautomatic(),
                        _ => wb.set_mode(CalcMode::Manual),
                    };
                })?;
                Ok(Variant::Empty)
            }
            ("Iteration", Get) => Ok(Variant::Bool(self.active_book()?.wb.settings().iteration)),
            ("Iteration", Put) => {
                let on = self.put_bool(a)?;
                self.with_active(|wb| {
                    wb.set_iteration_enabled(on);
                })?;
                Ok(Variant::Empty)
            }
            ("MaxIterations", Get) => Ok(Variant::R8(
                self.active_book()?.wb.settings().max_iterations as f64,
            )),
            ("MaxIterations", Put) => {
                let x = self.put_f64(a)?.trunc();
                if !(1.0..=32767.0).contains(&x) {
                    return Err(OmError::unable_to_set("MaxIterations", "Application"));
                }
                self.with_active(|wb| {
                    wb.set_max_iterations(x as u32);
                })?;
                Ok(Variant::Empty)
            }
            ("MaxChange", Get) => Ok(Variant::R8(self.active_book()?.wb.settings().max_change)),
            ("MaxChange", Put) => {
                let x = self.put_f64(a)?;
                self.with_active(|wb| {
                    wb.set_max_change(x);
                })?;
                Ok(Variant::Empty)
            }
            ("DisplayAlerts", _) => flag(self, "DisplayAlerts"),
            ("AskToUpdateLinks", _) => flag(self, "AskToUpdateLinks"),
            ("ScreenUpdating", _) => flag(self, "ScreenUpdating"),

            ("Visible", _) => flag(self, "Visible"),
            ("EnableEvents", _) => flag(self, "EnableEvents"),

            ("Version", Get) => Ok(Variant::Bstr("16.0".into())),
            ("Build", Get) => Ok(Variant::R8(20527.0)),
            ("OperatingSystem", Get) => Ok(Variant::Bstr("Windows (64-bit) NT 10.00".into())),
            ("Quit", Func) => {
                self.com_shutdown();
                self.quit = true;
                Ok(Variant::Empty)
            }
            ("Range", Get) => {
                let (b, s) = self.active_sheet_ids()?;
                self.range_of_args(b, s, a, false).map_err(|e| {
                    if e.number == 1004 && e.description.is_none() {
                        OmError::app_defined_with("Method 'Range' of object '_Application' failed")
                    } else {
                        e
                    }
                })
            }
            ("Union" | "Intersect", Func | Get) => self.union_intersect(m, a),
            (
                "AutoCorrect"
                | "AutoRecover"
                | "ProtectedViewWindows"
                | "Dialogs"
                | "ErrorCheckingOptions",
                Get,
            ) => Ok(self.alloc(Object::Stub(m.name))),
            ("CalculateUntilAsyncQueriesDone", Func) => Ok(Variant::Empty),
            ("Undo", Func) => Err(OmError::app_defined_with(
                "Undo method of Application class failed",
            )),
            ("Cells", Get) => {
                let (b, s) = self.active_sheet_ids()?;
                self.sheet_range(b, s, Mode::Cells)
            }
            ("Rows", Get) => {
                let (b, s) = self.active_sheet_ids()?;
                self.sheet_range(b, s, Mode::Rows)
            }
            ("Columns", Get) => {
                let (b, s) = self.active_sheet_ids()?;
                self.sheet_range(b, s, Mode::Columns)
            }
            ("CustomListCount", Get) => Ok(Variant::R8(
                (BUILTIN_LISTS.len() + self.user_lists.len()) as f64,
            )),
            ("GetCustomListContents", Func) => {
                let n = a.i32(0)?.unwrap_or(0);
                let Some(list) = self.custom_list(n) else {
                    return Ok(Variant::Bool(false));
                };
                Ok(Variant::Array(Box::new(SafeArray {
                    elem: VarType::Variant,
                    bounds: vec![(1, list.len() as u32)],
                    data: list.into_iter().map(Variant::Bstr).collect(),
                })))
            }
            ("AddCustomList", Func) => {
                let Some((_, _, cells)) = a.get(0).and_then(crate::conv::array_cells) else {
                    return Err(unsupported(
                        "AddCustomList with a ListArray that is not an array",
                        m,
                    ));
                };
                let list: Vec<String> = cells
                    .iter()
                    .map(|v| match v {
                        Variant::Bstr(s) => s.clone(),
                        other => Args {
                            values: vec![Some(other.clone())],
                            put: None,
                            lcid: a.lcid,
                        }
                        .string(0)
                        .ok()
                        .flatten()
                        .unwrap_or_default(),
                    })
                    .collect();

                if !self.user_lists.contains(&list)
                    && !BUILTIN_LISTS
                        .iter()
                        .any(|b| b.split(',').eq(list.iter().map(String::as_str)))
                {
                    self.user_lists.push(list);
                }
                Ok(Variant::Bool(true))
            }
            ("DeleteCustomList", Func) => {
                let n = a.i32(0)?.unwrap_or(0) as usize;

                if n <= BUILTIN_LISTS.len() || n > BUILTIN_LISTS.len() + self.user_lists.len() {
                    return Err(OmError::app_defined());
                }
                self.user_lists.remove(n - BUILTIN_LISTS.len() - 1);
                Ok(Variant::Bool(true))
            }
            ("CutCopyMode", Get) => Ok(match self.clip {
                Some(_) => Variant::I4(1),
                None => Variant::I4(0),
            }),
            ("CutCopyMode", Put) => {
                self.clip = None;
                self.clip_more.0.clear();
                Ok(Variant::Empty)
            }
            ("Names", Get) => {
                let id = self.active_book()?.id;
                Ok(self.alloc(Object::Names(id, None)))
            }
            (
                "Worksheets" | "Sheets" | "Charts" | "Excel4MacroSheets" | "Excel4IntlMacroSheets",
                Get,
            ) => {
                let id = self.active_book()?.id;
                Ok(self.alloc(Object::Sheets(id, sheets_kind(m.name))))
            }

            ("ThisWorkbook", Get) => {
                let id = self.running.ok_or_else(OmError::app_defined)?;
                Ok(self.alloc(Object::Workbook(id)))
            }
            ("Application", Get) => Ok(Variant::Dispatch(Some(self.handle()))),
            ("Caller", Get) => self.app_caller(),
            ("Volatile", Func) => {
                let on = a.bool(0)?.unwrap_or(true);
                self.app_volatile(on);
                Ok(Variant::Null)
            }
            ("Run", Func) => self.app_run(a),
            ("Evaluate" | "_Evaluate", Func) => self.evaluate_member(None, None, a),
            ("ExecuteExcel4Macro", Func) => self.execute_excel4_macro(a),
            #[cfg(windows)]
            ("RegisterXLL", Func) => self.register_xll(a),
            ("WorksheetFunction", Get) => Ok(self.alloc(Object::WorksheetFunction)),
            ("MultiThreadedCalculation", Get) => Ok(self.alloc(Object::MultiThreadedCalculation)),
            _ => {
                if let Some(r) = self.stored_option(None, m, a) {
                    return r;
                }
                match self.host_member(m, a) {
                    Some(r) => r,
                    None => Err(unsupported("_Application", m)),
                }
            }
        }
    }

    fn active_sheet_ids(&self) -> Result<(BookId, SheetId), OmError> {
        let b = self.active_book()?;
        Ok((b.id, b.wb.active_sheet()))
    }

    fn calculate_linked(&mut self, full: bool) -> Result<(), OmError> {
        let id = self.active_book()?.id;
        self.book_mut(id)?;
        for i in 0..self.books.len() {
            if let crate::app::BookWb::Own(wb) = &mut self.books[i].wb {
                if full {
                    wb.full_recalc();
                } else {
                    wb.recalc();
                }
            }
            self.sync_links();
        }
        for b in &mut self.books {
            if let crate::app::BookWb::Own(wb) = &mut b.wb {
                if !wb.has_ext_links() {
                    continue;
                }
                let mark = wb.notices().len();
                wb.recalc();
                wb.notices_mut().truncate(mark);
            }
        }
        Ok(())
    }

    fn with_active(&mut self, mut f: impl FnMut(&mut Workbook)) -> Result<(), OmError> {
        let id = self.active_book()?.id;
        self.book_mut(id)?;
        for b in &mut self.books {
            if let crate::app::BookWb::Own(wb) = &mut b.wb {
                f(wb);
            }
        }
        Ok(())
    }

    pub(crate) fn put_value(a: &Args) -> Args {
        Args {
            values: vec![a.put.clone()],
            put: None,
            lcid: a.lcid,
        }
    }

    pub(crate) fn coerce_wb_put(&self, m: &Member, a: &Args) -> Result<(), OmError> {
        match a.put.clone() {
            Some(v) => crate::app::coerce_put(m, v, a.lcid).map(drop),
            None => Ok(()),
        }
    }

    pub(crate) fn put_i32(&self, a: &Args) -> Result<i32, OmError> {
        Ok(Application::put_value(a).i32(0)?.unwrap_or(0))
    }

    pub(crate) fn multi_threaded_member(
        &mut self,
        m: &'static Member,
        a: &Args,
    ) -> Result<Variant, OmError> {
        match (m.name, m.kind) {
            ("Enabled", InvokeKind::Get) => Ok(Variant::Bool(self.multi_threaded)),
            ("Enabled", InvokeKind::Put) => {
                let on = self.put_bool(a)?;
                self.multi_threaded = on;
                self.with_active(|wb| wb.set_multi_threaded(on))?;
                Ok(Variant::Empty)
            }
            _ => Err(unsupported("MultiThreadedCalculation", m)),
        }
    }

    pub(crate) fn put_bool(&self, a: &Args) -> Result<bool, OmError> {
        Ok(Application::put_value(a).bool(0)?.unwrap_or(false))
    }

    pub(crate) fn put_f64(&self, a: &Args) -> Result<f64, OmError> {
        use xlcalc_variant::{change_type, VarType};
        let v = a.put.clone().unwrap_or(Variant::Empty);
        match change_type(&v, VarType::R8, crate::conv::variant_locale(a.lcid), 0) {
            Ok(Variant::R8(x)) => Ok(x),
            _ => Err(OmError::type_mismatch()),
        }
    }

    pub(crate) fn put_string(&self, a: &Args) -> Result<String, OmError> {
        Ok(Application::put_value(a).string(0)?.unwrap_or_default())
    }

    fn book_by_index(&self, v: &Variant, lcid: u32) -> Result<BookId, OmError> {
        if let Variant::Bstr(name) = v {
            return self
                .books
                .iter()
                .find(|b| b.name.eq_ignore_ascii_case(name))
                .map(|b| b.id)
                .ok_or_else(OmError::bad_index);
        }
        let n = Args {
            values: vec![Some(v.clone())],
            put: None,
            lcid,
        }
        .i32(0)?
        .unwrap_or(0);
        usize::try_from(n - 1)
            .ok()
            .and_then(|i| self.books.iter().filter(|b| !b.is_addin).nth(i))
            .map(|b| b.id)
            .ok_or_else(OmError::bad_index)
    }

    pub(crate) fn workbooks_member(
        &mut self,
        m: &'static Member,
        a: &Args,
    ) -> Result<Variant, OmError> {
        match m.name {
            "Count" => Ok(Variant::I4(
                self.books.iter().filter(|b| !b.is_addin).count() as i32,
            )),
            "Item" | "_Default" => {
                let v = a.get(0).ok_or_else(OmError::bad_index)?;
                let id = self.book_by_index(v, a.lcid)?;
                Ok(self.alloc(Object::Workbook(id)))
            }
            "_NewEnum" => {
                let ids: Vec<BookId> = self
                    .books
                    .iter()
                    .filter(|b| !b.is_addin)
                    .map(|b| b.id)
                    .collect();
                let items = ids
                    .into_iter()
                    .map(|b| self.alloc(Object::Workbook(b)))
                    .collect();
                Ok(self.alloc_enumerator(items))
            }
            "Add" => self.add_with_events(a),
            "Open" | "_Open" => self.open_with_events(a),

            "OpenXML" | "OpenText" | "OpenDatabase" | "CanCheckOut" | "CheckOut" => {
                let checkout = matches!(m.name, "CanCheckOut" | "CheckOut");
                let empty = match a.get(0) {
                    None | Some(Variant::Empty | Variant::Null) => true,
                    Some(v) if crate::conv::is_missing(v) => true,
                    Some(Variant::Bstr(s)) => s.is_empty(),
                    _ => false,
                };
                let second = a
                    .get(1)
                    .is_some_and(|v| !crate::conv::is_missing(v) && *v != Variant::Empty);
                if empty && !checkout && m.name != "OpenDatabase" {
                    Err(OmError::hresult(crate::error::DISP_E_PARAMNOTOPTIONAL))
                } else if (empty && checkout) || (second && m.name == "OpenXML") {
                    Err(OmError::exception(0x8007_0057_u32 as i32, None))
                } else {
                    Err(OmError::app_defined())
                }
            }
            "Close" => {
                self.books.clear();
                self.active = None;
                Ok(Variant::Empty)
            }
            _ => Err(unsupported("Workbooks", m)),
        }
    }

    pub(crate) fn window_list(&self) -> Vec<(BookId, u32)> {
        let mut l: Vec<(BookId, u32)> = self
            .wins
            .iter()
            .copied()
            .filter(|(b, _)| self.book(*b).is_ok_and(|bk| !bk.is_addin))
            .collect();
        for bk in self.books.iter().filter(|b| !b.is_addin) {
            if !l.iter().any(|(b, _)| *b == bk.id) {
                l.insert(0, (bk.id, 1));
            }
        }
        l
    }

    pub(crate) fn window_order(&self) -> Vec<BookId> {
        let mut out: Vec<BookId> = Vec::new();
        for (b, _) in self.window_list() {
            if !out.contains(&b) {
                out.push(b);
            }
        }
        out
    }

    pub(crate) fn current_window(&self) -> Option<(BookId, u32)> {
        let b = self.active?;
        let list = self.window_list();
        let n = self
            .cur_win
            .iter()
            .find(|(x, _)| *x == b)
            .map(|(_, n)| *n)
            .filter(|n| list.contains(&(b, *n)));
        n.or_else(|| list.iter().find(|(x, _)| *x == b).map(|(_, n)| *n))
            .map(|n| (b, n))
    }

    pub(crate) fn set_current_window(&mut self, w: (BookId, u32)) {
        self.cur_win.retain(|(x, _)| *x != w.0);
        self.cur_win.push(w);
    }

    pub(crate) fn window_caption(&self, w: (BookId, u32)) -> Result<String, OmError> {
        let name = self.book(w.0)?.name.clone();
        let several = self.window_list().iter().filter(|(b, _)| *b == w.0).count() > 1;
        Ok(if several {
            format!("{name}  -  {}", w.1)
        } else {
            name
        })
    }

    fn window_by_item(
        &self,
        list: &[(BookId, u32)],
        v: Option<&Variant>,
        a: &Args,
    ) -> Result<(BookId, u32), OmError> {
        if let Some(Variant::Bstr(name)) = v {
            let hit = list
                .iter()
                .copied()
                .find(|w| {
                    self.window_caption(*w)
                        .is_ok_and(|c| c.eq_ignore_ascii_case(name))
                })
                .or_else(|| {
                    list.iter().copied().find(|(b, _)| {
                        self.book(*b)
                            .is_ok_and(|bk| bk.name.eq_ignore_ascii_case(name))
                    })
                });
            return hit.ok_or_else(OmError::bad_index);
        }
        let n = a.i32(0)?.ok_or_else(OmError::bad_index)?;
        usize::try_from(n - 1)
            .ok()
            .and_then(|i| list.get(i).copied())
            .ok_or_else(OmError::bad_index)
    }

    pub(crate) fn after_call(&mut self) {
        self.sync_links();
    }

    pub(crate) fn windows_member(
        &mut self,
        m: &'static Member,
        a: &Args,
    ) -> Result<Variant, OmError> {
        let list = self.window_list();
        match m.name {
            "Count" => Ok(Variant::I4(list.len() as i32)),
            "Item" | "_Default" => {
                let w = self.window_by_item(&list, a.get(0), a)?;
                Ok(self.alloc(Object::Window(w.0, w.1)))
            }
            _ => Err(unsupported("Windows", m)),
        }
    }

    pub(crate) fn book_windows_member(
        &mut self,
        b: BookId,
        m: &'static Member,
        a: &Args,
    ) -> Result<Variant, OmError> {
        self.book(b)?;
        let list: Vec<(BookId, u32)> = self
            .window_list()
            .into_iter()
            .filter(|(x, _)| *x == b)
            .collect();
        match m.name {
            "Count" => Ok(Variant::I4(list.len() as i32)),
            "Item" | "_Default" => {
                let w = self.window_by_item(&list, a.get(0), a)?;
                Ok(self.alloc(Object::Window(w.0, w.1)))
            }
            _ => Err(unsupported("Windows", m)),
        }
    }

    pub(crate) fn new_window(&mut self, b: BookId) -> Result<Variant, OmError> {
        if self.book(b)?.is_addin {
            return Err(OmError::app_defined());
        }
        let list = self.window_list();
        let mine = list.iter().filter(|(x, _)| *x == b).map(|(_, n)| *n);
        let n = mine.clone().max().unwrap_or(0) + 1;
        let first = mine.min().map(|n| (b, n));
        self.wins = list;
        self.wins.insert(0, (b, n));
        self.active = Some(b);
        self.set_current_window((b, n));
        self.window_swap_events(first, (b, n));
        Ok(self.alloc(Object::Window(b, n)))
    }

    pub(crate) fn window_member(
        &mut self,
        b: BookId,
        n: u32,
        m: &'static Member,
        a: &Args,
    ) -> Result<Variant, OmError> {
        match (m.name, m.kind) {
            ("Activate", _) => {
                self.book(b)?;
                if !self.window_list().contains(&(b, n)) {
                    return Err(OmError::bad_index());
                }
                let was = self.active == Some(b) && self.current_window() == Some((b, n));
                self.active = Some(b);
                self.set_current_window((b, n));
                Ok(if was {
                    Variant::Null
                } else {
                    Variant::Bool(true)
                })
            }
            ("Caption", InvokeKind::Get) => match self.book(b)?.window.caption.clone() {
                Some(c) => Ok(Variant::Bstr(c)),
                None => Ok(Variant::Bstr(self.window_caption((b, n))?)),
            },
            ("Panes", InvokeKind::Get) => {
                self.book(b)?;
                Ok(self.alloc(Object::Panes(b, n)))
            }
            ("Index", InvokeKind::Get) => {
                let at = self.window_list().iter().position(|&w| w == (b, n));
                Ok(Variant::I4(at.map_or(1, |p| p as i32 + 1)))
            }
            _ => self.window_view_member(b, m, a),
        }
    }

    pub(crate) fn workbook_member(
        &mut self,
        b: BookId,
        m: &'static Member,
        a: &Args,
    ) -> Result<Variant, OmError> {
        use InvokeKind::*;
        if let Some(r) = self.stored_option(Some(b), m, a) {
            self.book(b)?;
            return r;
        }
        match (m.name, m.kind) {
            ("Application" | "Parent", Get) => {
                self.book(b)?;
                Ok(Variant::Dispatch(Some(self.handle())))
            }
            (
                "Worksheets" | "Sheets" | "Charts" | "Excel4MacroSheets" | "Excel4IntlMacroSheets",
                Get,
            ) => {
                self.book(b)?;
                Ok(self.alloc(Object::Sheets(b, sheets_kind(m.name))))
            }
            ("Names", Get) => {
                self.book(b)?;
                Ok(self.alloc(Object::Names(b, None)))
            }
            ("TableStyles", Get) => {
                self.book(b)?;
                Ok(self.alloc(Object::Stub("TableStyles")))
            }
            ("Theme", Get) => {
                self.book(b)?;
                Ok(self.alloc(Object::Stub("OfficeTheme")))
            }
            ("CustomDocumentProperties", Get) => {
                self.book(b)?;
                Ok(self.alloc(Object::DocProps(b, true)))
            }
            ("SlicerCaches", Get) => {
                self.book(b)?;
                Ok(self.alloc(Object::Slicer(b, crate::slicers::SlicerPart::Caches)))
            }
            ("IconSets", Get) => {
                self.book(b)?;
                Ok(self.alloc(Object::IconSets(b)))
            }
            ("Styles", Get) => {
                self.book(b)?;
                Ok(self.alloc(Object::Styles(b)))
            }
            ("ActiveSheet", Get) => {
                let sheet = self.book(b)?.wb.active_sheet();
                Ok(self.sheet_object(b, sheet))
            }
            ("ActiveChart", Get) => {
                let sheet = self.book(b)?.wb.active_sheet();
                Ok(self.active_chart(b, sheet))
            }
            ("Name", Get) => Ok(Variant::Bstr(self.book(b)?.name.clone())),
            ("FullName", Get) => {
                let bk = self.book(b)?;
                Ok(Variant::Bstr(if bk.path.is_empty() {
                    bk.name.clone()
                } else {
                    bk.path.clone()
                }))
            }
            ("Path", Get) => Ok(Variant::Bstr(crate::files::folder(&self.book(b)?.path))),

            ("FileFormat", Get) => Ok(match self.book(b)?.file_format {
                n if n < 0 => Variant::I4(n),
                n => Variant::R8(f64::from(n)),
            }),
            ("ReadOnly", Get) => Ok(Variant::Bool(self.book(b)?.read_only)),
            ("HasVBProject", Get) => Ok(Variant::Bool(self.has_vb_project(b)?)),
            ("VBProject", Get) => self.vbproject_handle(b),
            ("CodeName", Get) if !self.has_vb_project(b)? => Ok(Variant::Bstr(String::new())),
            ("CodeName", Get) => Ok(Variant::Bstr(self.code_name(b, None)?)),
            ("Save", Func) => self.save_with_events(b, |app| app.save_book(b)),
            ("SaveAs" | "_SaveAs", Func) => {
                self.save_with_events(b, |app| app.save_as_member(b, a).map(drop))
            }
            ("SaveCopyAs", Func) => {
                let file = a.string(0)?.unwrap_or_default();
                let format = self.book(b)?.file_format;
                self.save_book_as(b, &file, Some(format), true)?;
                Ok(Variant::Bool(true))
            }
            ("PrecisionAsDisplayed", Get) => {
                Ok(Variant::Bool(self.book(b)?.wb.precision_as_displayed()))
            }
            ("PrecisionAsDisplayed", Put) => {
                let on = self.put_bool(a)?;
                self.book_mut(b)?.wb.set_precision_as_displayed(on);
                Ok(Variant::Empty)
            }
            ("ForceFullCalculation", Get) => {
                Ok(Variant::Bool(self.book(b)?.wb.settings().force_full))
            }
            ("ForceFullCalculation", Put) => {
                let on = self.put_bool(a)?;
                self.book_mut(b)?.wb.set_force_full(on);
                Ok(Variant::Empty)
            }
            ("UpdateRemoteReferences", Get) => Ok(Variant::Bool(!self.book(b)?.no_remote_refs)),
            ("UpdateRemoteReferences", Put) => {
                let on = self.put_bool(a).map_err(|_| OmError::app_defined())?;
                self.book_mut(b)?.no_remote_refs = !on;
                Ok(Variant::Empty)
            }
            ("SaveLinkValues", Get) => Ok(Variant::Bool(!self.book(b)?.no_link_values)),
            ("SaveLinkValues", Put) => {
                let on = self.put_bool(a)?;
                self.book_mut(b)?.no_link_values = !on;
                Ok(Variant::Empty)
            }
            ("Date1904", Get) => Ok(Variant::Bool(
                self.book(b)?.wb.settings().date_system == DateSystem::D1904,
            )),
            ("Date1904", Put) => {
                let on = self.put_bool(a)?;
                let ds = if on {
                    DateSystem::D1904
                } else {
                    DateSystem::D1900
                };
                self.book_mut(b)?.wb.set_date_system(ds);
                Ok(Variant::Empty)
            }

            (
                "AutoUpdateSaveChanges"
                | "ExternalCodeServiceTimeout"
                | "CompatibilityVersion"
                | "PythonRuntime",
                Get | Put,
            ) => {
                if m.kind == Put {
                    self.coerce_wb_put(m, a)?;
                }

                if m.name == "AutoUpdateSaveChanges" {
                    Err(OmError::app_defined())
                } else {
                    Err(OmError::exception(0x8000_4001_u32 as i32, None))
                }
            }
            ("WorkIdentity" | "_CodeName", Put) => {
                self.coerce_wb_put(m, a)?;
                Err(OmError::app_defined())
            }
            ("WorkIdentity", Get) => Ok(Variant::Bstr(String::new())),
            ("_CodeName", Get) if !self.has_vb_project(b)? => Ok(Variant::Bstr(String::new())),
            ("_CodeName", Get) => Ok(Variant::Bstr(self.code_name(b, None)?)),
            ("DefaultSlicerStyle" | "DefaultTimelineStyle", Get) => {
                let n = if m.name == "DefaultSlicerStyle" {
                    "SlicerStyleLight1"
                } else {
                    "TimeSlicerStyleLight1"
                };
                Ok(self.alloc(Object::TableStyle(n.into())))
            }
            ("DefaultSlicerStyle" | "DefaultTimelineStyle", Put) => {
                Err(OmError::hresult(crate::error::DISP_E_BADPARAMCOUNT))
            }
            ("EncryptionProvider", Get) => Ok(Variant::Bstr(String::new())),
            ("EncryptionProvider", Put) => {
                self.coerce_wb_put(m, a)?;
                if self.put_string(a)?.is_empty() {
                    Ok(Variant::Empty)
                } else {
                    Err(OmError::exception(0x8007_0057_u32 as i32, None))
                }
            }
            ("AccuracyVersion", Get) => {
                let key = format!("{}:AccuracyVersion", b.0 + 1);
                Ok(self.opts.get(&key).cloned().unwrap_or(Variant::I4(0)))
            }
            ("AccuracyVersion", Put) => {
                self.coerce_wb_put(m, a)?;
                let n = self.put_i32(a)?;
                if n < 0 {
                    return Err(OmError::exception(0x8007_0057_u32 as i32, None));
                }

                let key = format!("{}:AccuracyVersion", b.0 + 1);
                self.opts
                    .insert(key, Variant::I4(if n <= 2 { n } else { 0 }));
                Ok(Variant::Empty)
            }
            ("ProtectStructure", Get) => Ok(Variant::Bool(self.book(b)?.protect.is_some())),
            ("ProtectWindows", Get) => Ok(Variant::Bool(false)),
            ("Protect", Func) => {
                self.protect_workbook(b, a)?;
                Ok(Variant::Empty)
            }
            ("Unprotect", Func) => {
                self.unprotect_workbook(b, a)?;
                Ok(Variant::Bool(true))
            }
            ("Saved", Get) => Ok(Variant::Bool(self.book(b)?.saved)),
            ("Saved", Put) => {
                let v = self.put_bool(a)?;
                self.book_mut(b)?.saved = v;
                Ok(Variant::Empty)
            }
            ("Activate", Func) => {
                if !self.book(b)?.is_addin {
                    if self.active != Some(b) {
                        let first = self
                            .window_list()
                            .into_iter()
                            .filter(|(x, _)| *x == b)
                            .map(|(_, n)| n)
                            .min();
                        if let Some(n) = first {
                            self.set_current_window((b, n));
                        }
                    }
                    self.active = Some(b);
                }
                Ok(Variant::Bool(true))
            }
            ("Close", Func) => self.close_with_events(b, a),
            ("Windows", Get) => Ok(self.alloc(Object::BookWindows(b))),
            ("NewWindow", Func) => self.new_window(b),
            ("LinkSources" | "UpdateLink" | "ChangeLink" | "BreakLink" | "OpenLinks", Func) => {
                self.book(b)?;
                self.link_member(b, m.name, a)
            }
            _ => match self.workbook_addin_member(b, m, a) {
                Some(r) => r,
                None => Err(unsupported("_Workbook", m)),
            },
        }
    }

    fn kind_sheets(&self, b: BookId, kind: SheetsKind) -> Result<Vec<SheetId>, OmError> {
        let selected = if kind == SheetsKind::Selected {
            self.selected_sheets(b)?
        } else {
            Vec::new()
        };
        Ok(self
            .book(b)?
            .wb
            .sheets()
            .filter(|s| match kind {
                SheetsKind::All => true,
                SheetsKind::Worksheets => s.kind == SheetKind::Worksheet,
                SheetsKind::Charts => s.kind == SheetKind::Chart,
                SheetsKind::Macro => s.kind == SheetKind::Macro,
                SheetsKind::MacroIntl => s.kind == SheetKind::MacroIntl,
                SheetsKind::Selected => selected.contains(&s.id),
            })
            .map(|s| s.id)
            .collect())
    }

    pub(crate) fn sheet_object(&mut self, b: BookId, s: SheetId) -> Variant {
        let chart = self
            .book(b)
            .ok()
            .and_then(|bk| bk.wb.sheet(s))
            .is_some_and(|x| x.kind == SheetKind::Chart);
        self.alloc(if chart {
            Object::Chart(b, s)
        } else {
            Object::Worksheet(b, s)
        })
    }

    fn active_chart(&mut self, b: BookId, s: SheetId) -> Variant {
        match self.sheet_object(b, s) {
            Variant::Dispatch(Some(id))
                if matches!(self.arena.get(id), Some(Object::Chart(..))) =>
            {
                Variant::Dispatch(Some(id))
            }
            _ => Variant::Dispatch(None),
        }
    }

    fn sheet_by_index(
        &self,
        b: BookId,
        kind: SheetsKind,
        v: &Variant,
        lcid: u32,
    ) -> Result<SheetId, OmError> {
        let list = self.kind_sheets(b, kind)?;
        if let Variant::Bstr(name) = v {
            return self
                .book(b)?
                .wb
                .sheet_by_name(name)
                .map(|s| s.id)
                .filter(|id| list.contains(id))
                .ok_or_else(OmError::bad_index);
        }
        let n = Args {
            values: vec![Some(v.clone())],
            put: None,
            lcid,
        }
        .i32(0)?
        .unwrap_or(0);
        usize::try_from(n - 1)
            .ok()
            .and_then(|i| list.get(i).copied())
            .ok_or_else(OmError::bad_index)
    }

    pub(crate) fn sheets_member(
        &mut self,
        b: BookId,
        kind: SheetsKind,
        m: &'static Member,
        a: &Args,
    ) -> Result<Variant, OmError> {
        match m.name {
            "Count" => Ok(Variant::I4(self.kind_sheets(b, kind)?.len() as i32)),
            "Item" | "_Default" => {
                let v = a.get(0).ok_or_else(OmError::bad_index)?;
                let s = self.sheet_by_index(b, kind, v, a.lcid)?;
                Ok(self.sheet_object(b, s))
            }
            "_NewEnum" => {
                let ids = self.kind_sheets(b, kind)?;
                let items = ids.into_iter().map(|s| self.sheet_object(b, s)).collect();
                Ok(self.alloc_enumerator(items))
            }
            "Add" => self.add_sheets(b, kind, a),
            "HPageBreaks" | "VPageBreaks" => {
                Ok(self.alloc(Object::PageBreaks(m.name == "HPageBreaks", None)))
            }
            "Visible" => self.sheets_visible(b, kind, m, a),
            "Select" => self.sheets_select(b, kind, a),
            "Delete" => self.sheets_delete(b, kind),
            "FillAcrossSheets" => self.sheets_fill_across(b, kind, a),
            "Copy" | "Move" => self.sheets_transfer(b, kind, m, a),

            "Add2" if kind == SheetsKind::Charts => Err(OmError::app_defined()),
            "Add2" => Err(OmError::exception(0x8000_4001_u32 as i32, None)),
            _ => Err(unsupported("Sheets", m)),
        }
    }

    fn sheets_visible(
        &mut self,
        b: BookId,
        kind: SheetsKind,
        m: &'static Member,
        a: &Args,
    ) -> Result<Variant, OmError> {
        if m.kind != InvokeKind::Put {
            return Err(OmError::app_defined_with(
                "Unable to get the Visible property of the Sheets class",
            ));
        }
        let bad = || OmError::unable_to_set("Visible", "Sheets");
        let v = match a.put {
            Some(Variant::Bool(false)) => 0,
            _ => self.put_i32(a).map_err(|_| bad())?,
        };
        let ids = self.kind_sheets(b, kind)?;
        let wb = &self.book(b)?.wb;
        let others_visible = wb
            .sheets()
            .any(|s| !ids.contains(&s.id) && wb.sheet_state(s.id) == Some(SheetState::Visible));
        if v != 0 || ids.is_empty() || !others_visible || self.book(b)?.protect.is_some() {
            return Err(bad());
        }
        for id in ids {
            self.book_mut(b)?
                .wb
                .set_sheet_state(id, SheetState::Hidden)
                .map_err(|_| bad())?;
        }
        Ok(Variant::Empty)
    }

    fn sheets_select(&mut self, b: BookId, kind: SheetsKind, a: &Args) -> Result<Variant, OmError> {
        if matches!(a.get(0), Some(Variant::Bstr(_))) {
            return Err(OmError::app_defined());
        }
        let add = matches!(a.get(0), Some(Variant::Bool(false)));
        let ids: Vec<SheetId> = self.kind_sheets(b, kind)?;
        let wb = &self.book(b)?.wb;
        let ids: Vec<SheetId> = ids
            .into_iter()
            .filter(|s| wb.sheet_state(*s) == Some(SheetState::Visible))
            .collect();
        if ids.is_empty() {
            return Err(OmError::app_defined());
        }
        for (i, id) in ids.into_iter().enumerate() {
            self.group_select(b, id, add || i > 0)?;
        }
        self.active = Some(b);
        Ok(Variant::Bool(true))
    }

    fn sheets_delete(&mut self, b: BookId, kind: SheetsKind) -> Result<Variant, OmError> {
        let ids = self.kind_sheets(b, kind)?;
        let wb = &self.book(b)?.wb;
        let left = wb
            .sheets()
            .any(|s| !ids.contains(&s.id) && wb.sheet_state(s.id) == Some(SheetState::Visible));
        if ids.is_empty() || !left {
            return Err(OmError::app_defined());
        }
        if self.display_alerts {
            return Ok(Variant::Bool(false));
        }
        self.refuse_if_protected(b, "Delete method of Sheets class failed")?;
        for id in ids {
            self.delete_one_sheet(b, id)?;
        }
        Ok(Variant::Bool(true))
    }

    pub(crate) fn delete_one_sheet(&mut self, b: BookId, id: SheetId) -> Result<(), OmError> {
        self.sheet_levels(b, id, "BeforeDelete", "SheetBeforeDelete", &[]);
        let wb = &self.book(b)?.wb;
        let successor = (wb.active_sheet() == id)
            .then(|| wb.sheet_after_delete(id))
            .flatten();
        if let Some(next) = successor {
            self.sheet_levels(b, id, "Deactivate", "SheetDeactivate", &[]);
            self.sheet_levels(b, next, "Activate", "SheetActivate", &[]);
        }
        let mark = self.notice_mark(b);
        let wb = &mut self.book_mut(b)?.wb;
        wb.delete_sheet(id).map_err(edit_error)?;
        if successor.is_some() {
            let log = wb.notices_mut();
            let mark = mark.min(log.len());
            let tail = log.split_off(mark);
            log.extend(
                tail.into_iter()
                    .filter(|n| !matches!(n, Notice::SheetActivated { .. })),
            );
        }
        Ok(())
    }

    fn sheets_fill_across(
        &mut self,
        b: BookId,
        kind: SheetsKind,
        a: &Args,
    ) -> Result<Variant, OmError> {
        let Some(arg) = a.get(0) else {
            return Err(OmError::hresult(crate::error::DISP_E_PARAMNOTOPTIONAL));
        };
        let Variant::Dispatch(Some(id)) = arg else {
            return Err(OmError::app_defined());
        };
        let Some(Object::Range(r)) = self.arena.get(*id) else {
            return Err(OmError::app_defined());
        };
        let r = *r;
        if let Some(t) = a.get(1) {
            let ok = matches!(
                t,
                Variant::I4(-4104 | -4163 | -4122) | Variant::I2(-4104 | -4163 | -4122)
            );
            if !ok {
                return Err(OmError::app_defined());
            }
        }
        let ids = self.kind_sheets(b, kind)?;
        let (ss, rect) = self.resolve(&r)?;
        if r.book != b || !ids.contains(&ss) {
            return Err(OmError::app_defined());
        }
        for d in ids.into_iter().filter(|d| *d != ss) {
            self.paste_copy(b, (ss, rect), (d, rect), false)?;
        }
        Ok(Variant::Bool(true))
    }

    fn sheets_transfer(
        &mut self,
        b: BookId,
        kind: SheetsKind,
        m: &'static Member,
        a: &Args,
    ) -> Result<Variant, OmError> {
        let moving = m.name == "Move";
        if a.values.len() > 2 {
            return Err(OmError::hresult(crate::error::DISP_E_BADPARAMCOUNT));
        }
        let ids = self.kind_sheets(b, kind)?;
        let named = |i: usize| -> Result<Option<(BookId, SheetId)>, OmError> {
            match a.get(i) {
                None => Ok(None),
                Some(Variant::Dispatch(Some(id))) => match self.arena.get(*id) {
                    Some(Object::Worksheet(ob, s) | Object::Chart(ob, s)) => Ok(Some((*ob, *s))),
                    _ => Err(OmError::app_defined()),
                },
                Some(_) => Err(OmError::app_defined()),
            }
        };
        let (before, after) = (named(0)?, named(1)?);
        if (before.is_some() && after.is_some()) || ids.is_empty() {
            return Err(OmError::app_defined());
        }
        let call = |app: &mut Self, s: SheetId, first: Option<Variant>, second: Option<Variant>| {
            let args = Args {
                values: vec![first, second],
                put: None,
                lcid: a.lcid,
            };
            app.worksheet_member(b, s, m, &args)
        };
        let last_of = |app: &Self, ob: BookId| -> Result<SheetId, OmError> {
            let wb = &app.book(ob)?.wb;
            Ok(wb.sheet_at(wb.sheet_count() - 1).id)
        };

        if before.is_none() && after.is_none() && moving {
            let wb = &self.book(b)?.wb;
            if !wb.sheets().any(|s| !ids.contains(&s.id)) {
                return Err(OmError::app_defined());
            }
        }
        let mut first: Option<(BookId, SheetId)> = None;
        match before.or(after) {
            None => {
                call(self, ids[0], None, None)?;
                if let Some(tb) = self.active {
                    first = Some((tb, self.book(tb)?.wb.active_sheet()));
                }
                for &s in &ids[1..] {
                    let tb = self.active.ok_or_else(OmError::app_defined)?;
                    let last = last_of(self, tb)?;
                    let h = self.sheet_object(tb, last);
                    call(self, s, None, Some(h))?;
                }
            }
            Some((tb, t)) if tb != b => {
                for (i, &s) in ids.iter().enumerate() {
                    let at = if i == 0 {
                        t
                    } else {
                        self.book(tb)?.wb.active_sheet()
                    };
                    let h = self.sheet_object(tb, at);
                    if before.is_some() {
                        call(self, s, Some(h), None)?;
                    } else {
                        call(self, s, None, Some(h))?;
                    }
                    if i == 0 {
                        first = Some((tb, self.book(tb)?.wb.active_sheet()));
                    }
                }
            }
            Some((_, t)) => {
                let order: Vec<SheetId> = self.book(b)?.wb.sheets().map(|s| s.id).collect();
                let pos = order.iter().position(|s| *s == t).unwrap_or(0);
                let from = if before.is_some() { pos } else { pos + 1 };
                let anchor = if moving {
                    order[from.min(order.len())..]
                        .iter()
                        .copied()
                        .find(|s| !ids.contains(s))
                } else {
                    order.get(from).copied()
                };
                for (i, &s) in ids.iter().enumerate() {
                    if let Some(t) = anchor {
                        let h = self.sheet_object(b, t);
                        call(self, s, Some(h), None)?;
                    } else {
                        let last = last_of(self, b)?;
                        let h = self.sheet_object(b, last);
                        call(self, s, None, Some(h))?;
                    }
                    if i == 0 {
                        first = Some((b, self.book(b)?.wb.active_sheet()));
                    }
                }
            }
        }
        if let Some((fb, fs)) = first {
            self.book_mut(fb)?
                .wb
                .set_active_sheet(fs)
                .map_err(edit_error)?;
            self.active = Some(fb);
        }
        Ok(Variant::Bool(true))
    }

    fn sheet_arg_any(&self, v: Option<&Variant>) -> Result<Option<(BookId, SheetId)>, OmError> {
        match v {
            None => Ok(None),
            Some(Variant::Dispatch(Some(id))) => match self.arena.get(*id) {
                Some(Object::Worksheet(b, s) | Object::Chart(b, s)) => Ok(Some((*b, *s))),
                _ => Err(OmError::app_defined()),
            },
            Some(_) => Err(OmError::app_defined()),
        }
    }

    pub(crate) fn sheet_to_other_book(
        &mut self,
        b: BookId,
        s: SheetId,
        moving: bool,
        a: &Args,
    ) -> Result<Option<Variant>, OmError> {
        let before = self.sheet_arg_any(a.get(0))?;
        let after = self.sheet_arg_any(a.get(1))?;
        let (dest, at): (Option<BookId>, Option<SheetId>) = match (before, after) {
            (Some(_), Some(_)) => return Err(OmError::app_defined()),
            (None, None) => (None, None),
            (Some((tb, _)), None) | (None, Some((tb, _))) if tb == b => return Ok(None),
            (Some((tb, t)), None) => (Some(tb), Some(t)),
            (None, Some((tb, t))) => {
                let wb = &self.book(tb)?.wb;
                let p = wb.sheet_position(t).ok_or_else(OmError::app_defined)?;
                (
                    Some(tb),
                    (p + 1 < wb.sheet_count()).then(|| wb.sheet_at(p + 1).id),
                )
            }
        };
        if dest.is_none() {
            let sh = self.book(b)?.wb.sheet(s).map(|x| x.kind);
            if sh == Some(xlcalc::SheetKind::Chart) && !moving {
                return self.chart_sheet_to_new_book(b, s).map(Some);
            }
        }
        if moving && self.book(b)?.wb.sheet_count() < 2 {
            return Err(OmError::app_defined_with(
                "Move method of Worksheet class failed",
            ));
        }
        let tb = match dest {
            Some(tb) => tb,
            None => {
                let name = format!("Book{}", self.next_book_number());
                let id = self.open_new(Workbook::empty(), name, String::new());
                self.book_mut(id)?.saved = true;
                id
            }
        };
        self.sync_links();
        let src_name = self.book(b)?.name.clone();
        let dst_name = self.book(tb)?.name.clone();
        let mut dst = Workbook::empty();
        match &mut self.book_mut(tb)?.wb {
            crate::app::BookWb::Own(w) => std::mem::swap(&mut **w, &mut dst),
            crate::app::BookWb::View(_) => return Err(OmError::app_defined()),
        }
        let done = dst.import_sheet(&self.book(b)?.wb, s, &src_name, at);
        let new_id = done.as_ref().ok().copied();
        let new_name = new_id
            .and_then(|n| dst.sheet(n).map(|x| x.name.to_string()))
            .unwrap_or_default();
        if let Some(n) = new_id {
            let drawing = self.book(b)?.wb.drawing(s).cloned();
            if let Some(mut d) = drawing {
                let sw = &self.book(b)?.wb;
                for sh in &mut d.shapes {
                    if let Some(c) = &mut sh.chart {
                        for p in c.parts_mut() {
                            if let xlcalc::drawing::Part::Refs(v) = p {
                                if v.iter().all(|e| e.0 == s) {
                                    v.iter_mut().for_each(|e| e.0 = n);
                                } else if let Some(t) =
                                    crate::charts::external_text(sw, p, &src_name)
                                {
                                    *p = xlcalc::drawing::Part::External(t);
                                }
                            }
                        }
                    }
                }
                *dst.drawing_mut(n) = d;
            }
        }
        if let crate::app::BookWb::Own(w) = &mut self.book_mut(tb)?.wb {
            std::mem::swap(&mut **w, &mut dst);
        }
        done.map_err(edit_error)?;
        self.book_mut(tb)?.saved = false;
        self.sync_links();
        if moving {
            let ghost = self.book(b)?.wb.external_sheet(&dst_name, &new_name);
            self.book_mut(b)?
                .wb
                .leave_sheet(s, ghost)
                .map_err(edit_error)?;
            self.sync_links();
        }
        self.active = Some(tb);
        Ok(Some(Variant::Bool(true)))
    }

    fn sheet_arg(&self, b: BookId, v: Option<&Variant>) -> Result<Option<SheetId>, OmError> {
        match v {
            None => Ok(None),
            Some(Variant::Dispatch(Some(id))) => match self.arena.get(*id) {
                Some(Object::Worksheet(ob, s) | Object::Chart(ob, s)) if *ob == b => Ok(Some(*s)),
                _ => Err(OmError::app_defined()),
            },
            Some(_) => Err(OmError::app_defined()),
        }
    }

    fn refuse_if_protected(&self, b: BookId, what: &str) -> Result<(), OmError> {
        if self.book(b)?.protect.is_some() {
            return Err(OmError::app_defined_with(what));
        }
        Ok(())
    }

    pub(crate) fn protect_workbook(&mut self, b: BookId, a: &Args) -> Result<(), OmError> {
        let password = a.string(0)?.filter(|p| !p.is_empty());
        let structure = a.bool(1)?.unwrap_or(true);
        let book = self.book_mut(b)?;
        match &book.protect {
            Some(None) => book.protect = None,
            Some(Some(_)) => return Err(OmError::app_defined()),
            None if structure => book.protect = Some(password),
            None => {}
        }
        Ok(())
    }

    pub(crate) fn unprotect_workbook(&mut self, b: BookId, a: &Args) -> Result<(), OmError> {
        let given = a.string(0)?;
        let book = self.book_mut(b)?;
        match &book.protect {
            Some(Some(p)) if given.as_deref() != Some(p.as_str()) => Err(OmError::app_defined_with(
                "The password you supplied is not correct. Verify that the CAPS LOCK key is off and be sure to use the correct capitalization.",
            )),
            _ => {
                book.protect = None;
                Ok(())
            }
        }
    }

    fn add_sheets(&mut self, b: BookId, kind: SheetsKind, a: &Args) -> Result<Variant, OmError> {
        let failed = || OmError::app_defined_with("Add method of Sheets class failed");
        self.refuse_if_protected(b, "Add method of Sheets class failed")?;

        let own = |i: usize| {
            let v = a.get(i);
            let foreign = matches!(v, Some(Variant::Dispatch(Some(id)))
                if matches!(self.arena.get(*id),
                    Some(Object::Worksheet(ob, _) | Object::Chart(ob, _)) if *ob != b));
            if foreign {
                None
            } else {
                v
            }
        };
        let before = self.sheet_arg(b, own(0)).map_err(|_| failed())?;
        let after = self.sheet_arg(b, own(1)).map_err(|_| failed())?;
        if before.is_some() && after.is_some() {
            return Err(failed());
        }

        let number = |i: usize, text: bool| -> Result<Option<i32>, OmError> {
            match a.get(i) {
                None => Ok(None),
                Some(Variant::Bool(t)) => Ok(Some(i32::from(*t))),
                Some(Variant::Bstr(s)) if text => s
                    .trim()
                    .parse::<f64>()
                    .ok()
                    .filter(|f| f.abs() < 2e9)
                    .map(|f| Some(f.trunc() as i32))
                    .ok_or_else(failed),
                Some(Variant::R8(f)) if f.abs() < 2e9 => Ok(Some(f.trunc() as i32)),
                Some(Variant::Bstr(_) | Variant::R8(_)) => Err(failed()),
                Some(_) => a.i32(i).map_err(|_| failed()),
            }
        };
        let count = number(2, true)?.unwrap_or(1);
        if !(1..=255).contains(&count) {
            return Err(failed());
        }
        if kind == SheetsKind::Charts && a.get(3).is_some() {
            return Err(failed());
        }
        let ty = number(3, false)?;

        let macro_kind = match (kind, ty) {
            (SheetsKind::Macro, _) | (SheetsKind::All | SheetsKind::Worksheets, Some(3)) => {
                Some(false)
            }
            (SheetsKind::MacroIntl, _) | (SheetsKind::All | SheetsKind::Worksheets, Some(4)) => {
                Some(true)
            }
            _ => None,
        };
        let chart = match (kind, ty) {
            _ if macro_kind.is_some() => false,
            (SheetsKind::Charts, _) | (SheetsKind::All, Some(-4109)) => true,
            (_, None | Some(-4167 | 1)) => false,
            (SheetsKind::All, Some(-4116)) => {
                return Err(OmError::unsupported(
                    "Sheets.Add of a sheet type other than xlWorksheet or xlChart",
                ))
            }
            _ => return Err(failed()),
        };
        let mark = self.notice_mark(b);
        let book = self.book_mut(b)?;

        let active = book.wb.active_sheet();
        let source = (chart
            && book
                .wb
                .sheet(active)
                .is_some_and(|x| x.kind == SheetKind::Worksheet))
        .then(|| book.wb.selection_of(active).areas.first().copied())
        .flatten()
        .filter(|r| r.rows() * r.cols() > 1)
        .map(|r| (active, r));

        let next_after = after.and_then(|s| {
            let wb = &book.wb;
            let pos = wb.sheet_position(s)?;
            (pos + 1 < wb.sheet_count()).then(|| wb.sheet_at(pos + 1).id)
        });
        let mut last = None;
        let mut made = Vec::new();
        for _ in 0..count {
            let anchor = match (last, before) {
                (Some(l), Some(_)) => l,
                (_, b) => b.unwrap_or_else(|| book.wb.active_sheet()),
            };
            let (prefix, counter) = if macro_kind.is_some() {
                ("Macro", &mut book.next_macro.0)
            } else if chart {
                ("Chart", &mut book.next_chart)
            } else {
                ("Sheet", &mut book.next_sheet)
            };
            let mut name = format!("{prefix}{counter}");
            *counter += 1;
            while book.wb.sheet_by_name(&name).is_some() {
                name = format!("{prefix}{counter}");
                *counter += 1;
            }
            let target = match after {
                Some(s) => {
                    let pos = book.wb.sheet_position(s).ok_or_else(OmError::app_defined)?;

                    if pos + 1 < book.wb.sheet_count() {
                        Some(book.wb.sheet_at(pos + 1).id)
                    } else {
                        chart.then(|| last.unwrap_or(s))
                    }
                }
                None => Some(anchor),
            };
            let id = if let Some(intl) = macro_kind {
                book.wb.add_macro_sheet_before(&name, intl, target)
            } else if chart {
                book.wb.add_chart_sheet_before(&name, target)
            } else {
                book.wb.add_sheet_before(&name, target)
            }
            .map_err(edit_error)?;
            last = Some(id);
            made.push(id);
        }
        let id = last.ok_or_else(OmError::app_defined)?;

        if let Some(named) = before.or(next_after).or(after) {
            let log = book.wb.notices_mut();
            let mark = mark.min(log.len());
            if let Some(Notice::SheetActivated { previous, .. }) = log[mark..]
                .iter_mut()
                .find(|n| matches!(n, Notice::SheetActivated { .. }))
            {
                *previous = named;
            }
        }
        if let Some((ss, rect)) = source {
            for &c in &made {
                let r = self.chart_sheet_ref(b, c)?;
                let wb = &mut self.book_mut(b)?.wb;
                let (by, series) = crate::charts::series_from_block(wb, ss, rect, None, 51, &[]);
                if let Some(ch) = wb
                    .drawing_mut(c)
                    .get_mut(r.id)
                    .and_then(|s| s.chart.as_mut())
                {
                    ch.plot_by = by;
                    ch.series = series;
                    ch.source = Some((ss, rect));
                }
            }
        }
        Ok(self.sheet_object(b, id))
    }

    pub(crate) fn chart_member(
        &mut self,
        b: BookId,
        s: SheetId,
        m: &'static Member,
        a: &Args,
    ) -> Result<Variant, OmError> {
        use InvokeKind::*;
        if self.book(b)?.wb.sheet(s).is_none() {
            return Err(OmError::object_required());
        }
        match (m.name, m.kind) {
            (
                "Name" | "Index" | "Visible" | "Delete" | "Activate" | "Select" | "Move" | "Copy",
                _,
            ) => self.worksheet_member(b, s, m, a),

            ("Type", Get) => Ok(Variant::I4(3)),
            ("Next" | "Previous", Get) => self.sheet_neighbour(b, s, m.name == "Next"),
            ("Parent", Get) => Ok(self.alloc(Object::Workbook(b))),
            _ => {
                let r = self.chart_sheet_ref(b, s)?;
                self.chart_draw_member(&r, m, a)
            }
        }
    }

    fn sheet_neighbour(&mut self, b: BookId, s: SheetId, next: bool) -> Result<Variant, OmError> {
        let wb = &self.book(b)?.wb;
        let pos = wb.sheet_position(s).ok_or_else(OmError::object_required)?;
        let other = if next {
            (pos + 1 < wb.sheet_count()).then(|| wb.sheet_at(pos + 1).id)
        } else {
            pos.checked_sub(1).map(|p| wb.sheet_at(p).id)
        };
        Ok(match other {
            Some(o) => self.sheet_object(b, o),
            None => Variant::Dispatch(None),
        })
    }

    pub(crate) fn worksheet_member(
        &mut self,
        b: BookId,
        s: SheetId,
        m: &'static Member,
        a: &Args,
    ) -> Result<Variant, OmError> {
        use InvokeKind::*;
        let sheet_gone = |wb: &Workbook| wb.sheet(s).is_none();
        if sheet_gone(&self.book(b)?.wb) {
            return Err(OmError::object_required());
        }
        if let Some(r) = self.sheet_stored_option(b, s, m, a) {
            return r;
        }
        match (m.name, m.kind) {
            ("_CodeName", Get) => Ok(Variant::Bstr(
                self.has_vb_project(b)?
                    .then(|| self.code_name(b, Some(s)))
                    .transpose()?
                    .unwrap_or_default(),
            )),
            ("_CodeName", Put) => {
                self.put_string(a)?;
                Err(OmError::app_defined())
            }
            ("Name", Get) => Ok(Variant::Bstr(
                self.book(b)?
                    .wb
                    .sheet(s)
                    .map(|x| x.name.clone())
                    .unwrap_or_default(),
            )),
            ("Name", Put) => {
                if self.book(b)?.protect.is_some() {
                    return Err(OmError::app_defined());
                }
                let name = self.put_string(a)?;
                self.book_mut(b)?
                    .wb
                    .rename_sheet(s, &name)
                    .map_err(edit_error)?;
                Ok(Variant::Empty)
            }
            ("Type", Get) => Ok(Variant::I4(
                match self.book(b)?.wb.sheet(s).map(|x| x.kind) {
                    Some(SheetKind::Macro) => 3,
                    Some(SheetKind::MacroIntl) => 4,
                    _ => -4167,
                },
            )),
            ("Next" | "Previous", Get) => self.sheet_neighbour(b, s, m.name == "Next"),
            ("Index", Get) => Ok(Variant::I4(
                self.book(b)?
                    .wb
                    .sheet_position(s)
                    .map_or(0, |p| p as i32 + 1),
            )),
            ("Range", Get) => self.range_of_args(b, s, a, true),
            ("Cells", Get) => self.sheet_range(b, s, Mode::Cells),
            ("Hyperlinks", Get) => Ok(self.alloc(Object::Hyperlinks(b, s, whole_sheet()))),
            ("Comments", Get) => Ok(self.alloc(Object::Comments(b, s))),
            ("Rows", Get) => self.sheet_range(b, s, Mode::Rows),
            ("Columns", Get) => self.sheet_range(b, s, Mode::Columns),
            ("UsedRange", Get) => {
                let rect = self
                    .book(b)?
                    .wb
                    .used_range(s)
                    .unwrap_or_else(|| Rect::cell(0, 0));
                self.new_range(b, s, rect, Mode::Cells)
            }
            ("Evaluate" | "_Evaluate", Func) => self.evaluate_member(Some(b), Some(s), a),
            ("Calculate", Func) => {
                self.book_mut(b)?.wb.calculate_sheet(s);
                Ok(Variant::Bool(true))
            }
            (
                "AutoFilter" | "AutoFilterMode" | "FilterMode" | "ShowAllData" | "StandardWidth"
                | "StandardHeight",
                _,
            ) => self.sheet_view_member(b, s, m, a),
            ("Shapes" | "ChartObjects", Get | Func) => self.sheet_draw_member(b, s, m, a),
            (name, Get | Func) if crate::controls::is_collection_member(name) => {
                self.sheet_controls_member(b, s, m, a)
            }
            ("Names", Get) => Ok(self.alloc(Object::Names(b, Some(s)))),
            ("Scenarios", Func) => self.sheet_scenarios(b, s, a),
            ("CustomProperties", Get) => Ok(self.alloc(Object::CustomProperties(b, s))),
            ("OLEObjects", Get | Func) => {
                if a.get(0).is_some() {
                    return Err(OmError::app_defined_with(
                        "Unable to get the OLEObjects property of the Worksheet class",
                    ));
                }
                Ok(self.alloc(Object::OleObjects(b)))
            }
            ("Sort", Get) => Ok(self.alloc(Object::Sort(SortOwner::Sheet(b, s)))),
            ("ListObjects", Get) => Ok(self.alloc(Object::ListObjects(b, s))),
            ("Activate", Func) => {
                self.book_mut(b)?
                    .wb
                    .set_active_sheet(s)
                    .map_err(edit_error)?;
                self.active = Some(b);
                Ok(Variant::Bool(true))
            }

            ("Select", Func) => {
                let wb = &mut self.book_mut(b)?.wb;
                if wb.sheet_state(s) != Some(SheetState::Visible) {
                    return Err(OmError::app_defined_with(
                        "Select method of Worksheet class failed",
                    ));
                }
                let add = matches!(a.get(0), Some(Variant::Bool(false)));
                self.group_select(b, s, add)?;
                self.active = Some(b);
                Ok(Variant::Bool(true))
            }
            ("Visible", Get) => Ok(Variant::I4(
                match self.book(b)?.wb.sheet_state(s).unwrap_or_default() {
                    SheetState::Visible => -1,
                    SheetState::Hidden => 0,
                    SheetState::VeryHidden => 2,
                },
            )),

            ("Visible", Put) => {
                let bad = || OmError::unable_to_set("Visible", "Worksheet");
                if self.book(b)?.protect.is_some() {
                    return Err(bad());
                }
                let state = match self.put_i32(a).map_err(|_| bad())? {
                    -1 | 1 => SheetState::Visible,
                    0 => SheetState::Hidden,
                    2 => SheetState::VeryHidden,
                    _ => return Err(bad()),
                };
                self.book_mut(b)?
                    .wb
                    .set_sheet_state(s, state)
                    .map_err(|_| bad())?;
                Ok(Variant::Empty)
            }

            ("CodeName", Get) if !self.has_vb_project(b)? => Ok(Variant::Bstr(
                self.sheet_code_name(b, s).unwrap_or_default(),
            )),
            ("CodeName", Get) => Ok(Variant::Bstr(self.code_name(b, Some(s))?)),

            ("Delete", Func) if self.display_alerts => Ok(Variant::Bool(false)),
            ("Delete", Func) => {
                self.refuse_if_protected(b, "Delete method of Worksheet class failed")?;
                self.delete_one_sheet(b, s)?;
                Ok(Variant::Bool(true))
            }
            ("HPageBreaks" | "VPageBreaks", Get) => {
                Ok(self.alloc(Object::PageBreaks(m.name == "HPageBreaks", Some((b, s)))))
            }
            ("ResetAllPageBreaks", Func) => {
                self.book_mut(b)?.wb.reset_page_breaks(s);
                Ok(Variant::Empty)
            }
            ("Paste", Func) => self.sheet_paste(b, s, m, a),
            ("Move" | "Copy", Func) => {
                if self.book(b)?.protect.is_some() {
                    return Err(if m.name == "Move" {
                        OmError::app_defined_with("Move method of Worksheet class failed")
                    } else {
                        OmError::app_defined_with("Workbook is protected and cannot be changed.")
                    });
                }
                if let Some(r) = self.sheet_to_other_book(b, s, m.name == "Move", a)? {
                    return Ok(r);
                }
                let before = self.sheet_arg(b, a.get(0))?;
                let after = self.sheet_arg(b, a.get(1))?;

                if before.is_some() == after.is_some() {
                    return Err(OmError::app_defined());
                }
                let wb = &mut self.book_mut(b)?.wb;
                let target = match (before, after) {
                    (Some(t), _) => Some(t),
                    (None, Some(t)) => {
                        let p = wb.sheet_position(t).ok_or_else(OmError::app_defined)?;
                        (p + 1 < wb.sheet_count()).then(|| wb.sheet_at(p + 1).id)
                    }
                    _ => None,
                };

                if m.name == "Move" {
                    wb.move_sheet(s, target).map_err(edit_error)?;
                } else {
                    wb.copy_sheet(s, target).map_err(edit_error)?;

                    self.book_mut(b)?.next_sheet += 1;
                    self.copy_document_module(b, s)?;

                    let copy = self.book(b)?.wb.active_sheet();
                    let book = self.book_mut(b)?;
                    if let Some(p) = book.sheet_prot.get(&s).cloned() {
                        book.sheet_prot.insert(copy, p);
                    }
                    if let Some(p) = book.page_setup.get(&s).cloned() {
                        book.page_setup.insert(copy, p);
                    }
                }
                Ok(Variant::Bool(true))
            }
            ("ProtectContents", Get) => Ok(Variant::Bool(self.sheet_prot(b, s).contents)),
            ("ProtectDrawingObjects", Get) => Ok(Variant::Bool(self.sheet_prot(b, s).objects)),
            ("ProtectScenarios", Get) => Ok(Variant::Bool(self.sheet_prot(b, s).scenarios)),
            ("ProtectionMode", Get) => Ok(Variant::Bool(self.sheet_prot(b, s).ui_only)),
            ("EnableSelection", Get) => Ok(Variant::I4(self.sheet_prot(b, s).selection)),
            ("EnableSelection", Put) => {
                self.coerce_wb_put(m, a)?;
                let v = self.put_i32(a)?;
                let v = match v {
                    0 | 1 | -4142 => v,
                    2 => -4142,
                    _ => return Err(OmError::app_defined()),
                };
                self.set_enable_selection(b, s, v)?;
                Ok(Variant::Empty)
            }
            ("Protection", Get) => Ok(self.alloc(Object::Protection(b, s))),
            ("Outline", Get) => Ok(self.alloc(Object::Outline(b, s))),
            ("PageSetup", Get) => Ok(self.alloc(Object::PageSetup(b, s))),
            ("Protect", Func) => {
                self.protect_sheet(b, s, a)?;
                Ok(Variant::Empty)
            }
            ("Unprotect", Func) => Ok(if self.unprotect_sheet(b, s, a)? {
                Variant::Bool(true)
            } else {
                Variant::Null
            }),
            ("CircularReference", Get) => {
                let hit = self.book(b)?.wb.circular_ref_on(s);
                match hit {
                    Some(c) => self.new_range(b, s, Rect::cell(c.row, c.col), Mode::Cells),
                    None => Ok(Variant::Dispatch(None)),
                }
            }
            _ => Err(unsupported("_Worksheet", m)),
        }
    }

    fn name_list(&self, b: BookId, scope: Option<SheetId>) -> Result<Vec<NameEntry>, OmError> {
        let wb = &self.book(b)?.wb;
        let mut list: Vec<NameEntry> = wb
            .names()
            .iter()
            .filter(|d| scope.is_none() || d.scope == scope)
            .map(|d| NameEntry {
                shown: match d.scope.and_then(|s| wb.sheet(s)) {
                    Some(sh) => {
                        format!("{}!{}", xlcalc::testing::sheet_name_text(&sh.name), d.name)
                    }
                    None => d.name.clone(),
                },
                bare: d.name.clone(),
                scope: d.scope,
            })
            .collect();
        if scope.is_none() {
            for h in wb.names().hidden() {
                if wb.names().get(&h.to_uppercase(), None).is_none() {
                    list.push(NameEntry {
                        shown: h.to_string(),
                        bare: h.to_string(),
                        scope: None,
                    });
                }
            }
        }

        list.sort_by_key(|e| (e.bare.to_lowercase(), e.scope.is_none()));
        Ok(list)
    }

    fn name_handle(&mut self, b: BookId, e: &NameEntry) -> Variant {
        self.alloc(Object::Name(b, e.scope, e.bare.to_uppercase()))
    }

    pub(crate) fn names_member(
        &mut self,
        b: BookId,
        scope: Option<SheetId>,
        m: &'static Member,
        a: &Args,
    ) -> Result<Variant, OmError> {
        match m.name {
            "Count" => Ok(Variant::I4(self.name_list(b, scope)?.len() as i32)),
            "Item" | "_Default" => {
                let list = self.name_list(b, scope)?;
                let found = match (a.get(0), a.get(1)) {
                    (Some(Variant::Bstr(t)), _) | (None, Some(Variant::Bstr(t))) => {
                        let wb = &self.book(b)?.wb;
                        let first = wb.sheets().next().map(|x| x.id);
                        let key = t.to_uppercase();
                        let preferred = if scope.is_some() || t.contains('!') {
                            None
                        } else if first.is_some_and(|f| wb.names().get(&key, Some(f)).is_some()) {
                            first
                        } else if wb.names().get(&key, None).is_some()
                            && wb.names().get(&key, Some(wb.active_sheet())).is_some()
                        {
                            Some(wb.active_sheet())
                        } else {
                            None
                        };
                        match preferred.and_then(|p| {
                            list.iter()
                                .find(|e| e.scope == Some(p) && e.bare.to_uppercase() == key)
                        }) {
                            Some(e) => e.clone(),

                            None => list
                                .iter()
                                .find(|e| {
                                    e.shown.eq_ignore_ascii_case(t)
                                        || (scope.is_some() || e.scope.is_none())
                                            && e.bare.eq_ignore_ascii_case(t)
                                })
                                .cloned()
                                .ok_or_else(OmError::app_defined)?,
                        }
                    }
                    (Some(v), _) => {
                        let n = Args {
                            values: vec![Some(v.clone())],
                            put: None,
                            lcid: a.lcid,
                        }
                        .i32(0)?
                        .unwrap_or(0);
                        usize::try_from(n - 1)
                            .ok()
                            .and_then(|i| list.get(i))
                            .cloned()
                            .ok_or_else(OmError::bad_index)?
                    }
                    (None, None) => {
                        let t = a.string(2)?.ok_or_else(OmError::app_defined)?;
                        let wb = &self.book(b)?.wb;
                        list.iter()
                            .find(|e| {
                                wb.names()
                                    .get(&e.bare.to_uppercase(), e.scope)
                                    .is_some_and(|d| {
                                        format!("={}", wb.name_refers_to(d))
                                            .eq_ignore_ascii_case(&t)
                                    })
                            })
                            .cloned()
                            .ok_or_else(OmError::app_defined)?
                    }
                    _ => return Err(OmError::app_defined()),
                };
                Ok(self.name_handle(b, &found))
            }
            "_NewEnum" => {
                let list = self.name_list(b, scope)?;
                let items = list.iter().map(|e| self.name_handle(b, e)).collect();
                Ok(self.alloc_enumerator(items))
            }
            "Add" => {
                let name = a
                    .string(0)?
                    .or(a.string(6)?)
                    .ok_or_else(OmError::app_defined)?;
                let refers = a
                    .string(1)?
                    .or(a.string(7)?)
                    .or(a.string(9)?)
                    .or(a.string(10)?)
                    .ok_or_else(OmError::app_defined)?;
                let wb = &mut self.book_mut(b)?.wb;
                wb.define_name_active(&name, &refers, scope)
                    .map_err(edit_error)?;
                let bare = name.rsplit_once('!').map_or(name.as_str(), |x| x.1);
                let def = wb
                    .names()
                    .iter()
                    .filter(|d| d.name.eq_ignore_ascii_case(bare))
                    .find(|d| scope.is_none() || d.scope == scope)
                    .cloned()
                    .ok_or_else(OmError::app_defined)?;
                let entry = NameEntry {
                    shown: def.name.clone(),
                    bare: def.name.clone(),
                    scope: def.scope,
                };
                if let Some(t) = a.i32(3)?.filter(|t| matches!(t, 1 | 2)) {
                    let key = def.name.to_uppercase();
                    self.book_mut(b)?
                        .wb
                        .set_name_macro_type(&key, def.scope, Some(t));
                }
                if a.bool(2)? == Some(false) {
                    let key = def.name.to_uppercase();
                    self.book_mut(b)?
                        .wb
                        .set_name_visible(&key, def.scope, false);
                }
                Ok(self.name_handle(b, &entry))
            }
            _ => Err(unsupported("Names", m)),
        }
    }

    pub(crate) fn name_member(
        &mut self,
        b: BookId,
        scope: Option<SheetId>,
        key: &str,
        m: &'static Member,
        a: &Args,
    ) -> Result<Variant, OmError> {
        use InvokeKind::*;
        if m.name == "MacroType" && m.kind == Put {
            let t = Application::put_value(a)
                .i32(0)?
                .ok_or_else(OmError::app_defined)?;
            let wb = &mut self.book_mut(b)?.wb;
            if wb.names().get(key, scope).is_none() {
                return Err(OmError::object_required());
            }

            if !matches!(t, 1 | 2) {
                return Err(OmError::app_defined());
            }
            wb.set_name_macro_type(key, scope, Some(t));
            return Ok(Variant::Empty);
        }

        if matches!(m.name, "Category" | "CategoryLocal") {
            return Err(OmError::app_defined());
        }
        let wb = &self.book(b)?.wb;
        let Some(def) = wb.names().get(key, scope).cloned() else {
            let h = scope
                .is_none()
                .then(|| wb.names().hidden_get(key).map(str::to_string))
                .flatten()
                .ok_or_else(OmError::object_required)?;
            return match (m.name, m.kind) {
                ("Name" | "NameLocal", Get) => Ok(Variant::Bstr(h)),
                ("RefersTo" | "RefersToLocal" | "Value" | "_Default", Get) => {
                    Ok(Variant::Bstr("=#NAME?".into()))
                }
                ("RefersToR1C1" | "RefersToR1C1Local", Get) => Ok(Variant::Bstr("=#NAME?".into())),
                ("Visible", Get) => Ok(Variant::Bool(false)),
                ("Parent", Get) => Ok(self.alloc(Object::Workbook(b))),
                ("Delete", Func) => {
                    self.book_mut(b)?.wb.delete_hidden_name(key);
                    Ok(Variant::Empty)
                }
                _ => Err(unsupported("Name", m)),
            };
        };
        let shown = match def.scope.and_then(|s| wb.sheet(s)) {
            Some(sh) => format!(
                "{}!{}",
                xlcalc::testing::sheet_name_text(&sh.name),
                def.name
            ),
            None => def.name.clone(),
        };
        match (m.name, m.kind) {
            ("RefersTo" | "RefersToR1C1", Put) => {
                let mut text = Application::put_value(a)
                    .string(0)?
                    .ok_or_else(OmError::type_mismatch)?;

                if text.trim().parse::<f64>().is_ok() {
                    text = format!("={}", text.trim());
                }
                let name = def.name.clone();
                self.book_mut(b)?
                    .wb
                    .define_name(&name, &text, def.scope)
                    .map_err(edit_error)?;
                Ok(Variant::Empty)
            }

            ("Name", Put) => {
                let new = Application::put_value(a)
                    .string(0)?
                    .ok_or_else(OmError::type_mismatch)?;
                let old = def.name.clone();
                let _ = self.book_mut(b)?.wb.rename_name(&old, def.scope, &new);
                Ok(Variant::Empty)
            }
            ("Name" | "NameLocal", Get) => Ok(Variant::Bstr(shown)),
            ("RefersTo" | "RefersToLocal" | "Value" | "_Default", Get) => {
                Ok(Variant::Bstr(format!("={}", wb.name_refers_to(&def))))
            }
            ("RefersToR1C1" | "RefersToR1C1Local", Get) => {
                Ok(Variant::Bstr(format!("={}", wb.name_refers_to_r1c1(&def))))
            }
            ("RefersToRange", Get) => {
                let text = wb.name_refers_to(&def);
                let (sheet, rect) =
                    parse_range_text(wb, def.scope.unwrap_or(wb.active_sheet()), &text)
                        .ok_or_else(OmError::app_defined)?;
                self.new_range(b, sheet, rect, Mode::Cells)
            }
            ("Index", Get) => {
                let list = self.name_list(b, None)?;
                let at = list
                    .iter()
                    .position(|e| e.scope == def.scope && e.bare.eq_ignore_ascii_case(&def.name));
                Ok(Variant::I4(at.map_or(1, |i| i as i32 + 1)))
            }
            ("Visible", Get) => Ok(Variant::Bool(wb.names().is_visible(key, scope))),
            ("MacroType", Get) => Ok(Variant::I4(
                wb.names().macro_type(key, scope).unwrap_or(-4142),
            )),
            ("Parent", Get) => Ok(self.alloc(match def.scope {
                Some(s) => Object::Worksheet(b, s),
                None => Object::Workbook(b),
            })),
            ("Delete", Func) => {
                self.book_mut(b)?.wb.delete_name(key, scope);
                Ok(Variant::Empty)
            }
            _ => Err(unsupported("Name", m)),
        }
    }

    pub(crate) fn range_name_get(
        &mut self,
        b: BookId,
        sheet: SheetId,
        rect: Rect,
    ) -> Result<Variant, OmError> {
        let wb = &self.book(b)?.wb;
        let want = self.range_refers_text(wb, sheet, rect);
        let found = wb.names().iter().find(|d| {
            d.scope.is_none_or(|s| s == sheet) && wb.name_refers_to(d).eq_ignore_ascii_case(&want)
        });
        let entry = found
            .map(|d| NameEntry {
                shown: d.name.clone(),
                bare: d.name.clone(),
                scope: d.scope,
            })
            .ok_or_else(OmError::app_defined)?;
        Ok(self.name_handle(b, &entry))
    }

    pub(crate) fn range_name_put(
        &mut self,
        b: BookId,
        sheet: SheetId,
        rect: Rect,
        name: &str,
    ) -> Result<Variant, OmError> {
        let refers = format!(
            "={}",
            self.range_refers_text(&self.book(b)?.wb, sheet, rect)
        );
        self.book_mut(b)?
            .wb
            .define_name_active(name, &refers, None)
            .map_err(edit_error)?;
        Ok(Variant::Empty)
    }

    pub(crate) fn range_create_names(
        &mut self,
        b: BookId,
        sheet: SheetId,
        rect: Rect,
        sides: [bool; 4],
    ) -> Result<(), OmError> {
        let [top, left, bottom, right] = sides;
        let r_lo = rect.r0 + u32::from(top);
        let r_hi = rect.r1.saturating_sub(u32::from(bottom));
        let c_lo = rect.c0 + u32::from(left);
        let c_hi = rect.c1.saturating_sub(u32::from(right));
        if r_lo > r_hi || c_lo > c_hi {
            return Ok(());
        }
        let mut defs: Vec<(Option<String>, Rect)> = Vec::new();
        {
            let wb = &self.book(b)?.wb;
            let label = |r: u32, c: u32| {
                label_name(wb.shown_text(xlcalc::CellAddr::new(sheet, r, c)).trim())
            };
            for c in c_lo..=c_hi {
                let col = Rect::new(r_lo, c, r_hi, c);
                if top {
                    defs.push((label(rect.r0, c), col));
                }
                if bottom {
                    defs.push((label(rect.r1, c), col));
                }
            }
            for r in r_lo..=r_hi {
                let row = Rect::new(r, c_lo, r, c_hi);
                if left {
                    defs.push((label(r, rect.c0), row));
                }
                if right {
                    defs.push((label(r, rect.c1), row));
                }
            }
        }
        for (name, area) in defs {
            let Some(name) = name else { continue };
            let refers = format!(
                "={}",
                self.range_refers_text(&self.book(b)?.wb, sheet, area)
            );

            let _ = self
                .book_mut(b)?
                .wb
                .define_name_active(&name, &refers, None);
        }
        Ok(())
    }

    fn range_refers_text(&self, wb: &xlcalc::Workbook, sheet: SheetId, rect: Rect) -> String {
        let st = crate::addr::AddressStyle {
            row_abs: true,
            col_abs: true,
            r1c1: false,
            relative_to: (0, 0),
        };
        let sh = wb
            .sheet(sheet)
            .map(|s| xlcalc::testing::sheet_name_text(&s.name))
            .unwrap_or_default();
        format!("{sh}!{}", crate::addr::range_text(rect, &st))
    }

    pub fn book_of(&self, handle: ObjectId) -> Option<BookId> {
        match self.arena.get(handle)? {
            Object::Workbook(b) => Some(*b),
            _ => None,
        }
    }
}

fn sheets_kind(property: &str) -> SheetsKind {
    match property {
        "Worksheets" => SheetsKind::Worksheets,
        "Charts" => SheetsKind::Charts,
        "Excel4MacroSheets" => SheetsKind::Macro,
        "Excel4IntlMacroSheets" => SheetsKind::MacroIntl,
        _ => SheetsKind::All,
    }
}

fn label_name(raw: &str) -> Option<String> {
    if raw.is_empty() {
        return None;
    }
    let mut name: String = raw
        .chars()
        .map(|c| {
            if c.is_alphanumeric() || c == '_' || c == '.' {
                c
            } else {
                '_'
            }
        })
        .collect();
    if name.chars().next().is_some_and(|c| c.is_ascii_digit()) {
        name.insert(0, '_');
    }
    xlcalc::is_valid_name(&name).then_some(name)
}

pub(crate) const BUILTIN_LISTS: [&str; 8] = [
    "Mon,Tue,Wed,Thu,Fri,Sat,Sun",
    "Monday,Tuesday,Wednesday,Thursday,Friday,Saturday,Sunday",
    "Jan,Feb,Mar,Apr,May,Jun,Jul,Aug,Sep,Oct,Nov,Dec",
    "January,February,March,April,May,June,July,August,September,October,November,December",
    "Mon,Tue,Wed,Thu,Fri,Sat,Sun",
    "Monday,Tuesday,Wednesday,Thursday,Friday,Saturday,Sunday",
    "Jan,Feb,Mar,Apr,May,Jun,Jul,Aug,Sept,Oct,Nov,Dec",
    "January,February,March,April,May,June,July,August,September,October,November,December",
];

impl Application<'_> {
    pub(crate) fn custom_list(&self, n: i32) -> Option<Vec<String>> {
        let i = usize::try_from(n).ok()?.checked_sub(1)?;
        match BUILTIN_LISTS.get(i) {
            Some(l) => Some(l.split(',').map(String::from).collect()),
            None => self.user_lists.get(i - BUILTIN_LISTS.len()).cloned(),
        }
    }
}
