use xlcalc::{
    CalcMode, DateSystem, EditError, Notice, Rect, SheetId, SheetKind, SheetState, Workbook,
};
use xlcalc_variant::{ObjectId, Variant};

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
            ("Windows", Get) => Ok(self.alloc(Object::Windows)),
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
                self.with_active(|wb| {
                    wb.recalc();
                })?;
                Ok(Variant::Bool(true))
            }
            ("CalculateFull", Func) => {
                self.with_active(|wb| {
                    wb.full_recalc();
                })?;
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
            ("ScreenUpdating", _) => flag(self, "ScreenUpdating"),

            ("Visible", _) => flag(self, "Visible"),
            ("EnableEvents", _) => flag(self, "EnableEvents"),

            ("Version", Get) => Ok(Variant::Bstr("16.0".into())),
            ("Build", Get) => Ok(Variant::R8(20527.0)),
            ("OperatingSystem", Get) => Ok(Variant::Bstr("Windows (64-bit) NT 10.00".into())),
            ("Quit", Func) => {
                self.quit = true;
                Ok(Variant::Empty)
            }
            ("Range", Get) => {
                let (b, s) = self.active_sheet_ids()?;
                self.range_of_args(b, s, a)
            }
            ("Union" | "Intersect", Func) => self.union_intersect(m, a),
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
            ("CutCopyMode", Get) => Ok(match self.clip {
                Some(_) => Variant::I4(1),
                None => Variant::Bool(false),
            }),
            ("CutCopyMode", Put) => {
                self.clip = None;
                Ok(Variant::Empty)
            }
            ("Names", Get) => {
                let id = self.active_book()?.id;
                Ok(self.alloc(Object::Names(id, None)))
            }
            ("Worksheets" | "Sheets" | "Charts", Get) => {
                let id = self.active_book()?.id;
                Ok(self.alloc(Object::Sheets(id, sheets_kind(m.name))))
            }
            ("ThisWorkbook", Get) => {
                let id = match self.running {
                    Some(id) => id,
                    None => self.active_book()?.id,
                };
                Ok(self.alloc(Object::Workbook(id)))
            }
            ("Application", Get) => Ok(Variant::Dispatch(Some(self.handle()))),
            ("Caller", Get) => self.app_caller(),
            ("Volatile", Func) => {
                let on = a.bool(0)?.unwrap_or(true);
                self.app_volatile(on);
                Ok(Variant::Empty)
            }
            ("Run", Func) => self.app_run(a),
            ("WorksheetFunction", Get) => Ok(self.alloc(Object::WorksheetFunction)),
            ("MultiThreadedCalculation", Get) => Ok(self.alloc(Object::MultiThreadedCalculation)),
            _ => match self.host_member(m, a) {
                Some(r) => r,
                None => Err(unsupported("_Application", m)),
            },
        }
    }

    fn active_sheet_ids(&self) -> Result<(BookId, SheetId), OmError> {
        let b = self.active_book()?;
        Ok((b.id, b.wb.active_sheet()))
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

    fn put_value(a: &Args) -> Args {
        Args {
            values: vec![a.put.clone()],
            put: None,
            lcid: a.lcid,
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
            .and_then(|i| self.books.get(i))
            .map(|b| b.id)
            .ok_or_else(OmError::bad_index)
    }

    pub(crate) fn workbooks_member(
        &mut self,
        m: &'static Member,
        a: &Args,
    ) -> Result<Variant, OmError> {
        match m.name {
            "Count" => Ok(Variant::I4(self.books.len() as i32)),
            "Item" | "_Default" => {
                let v = a.get(0).ok_or_else(OmError::bad_index)?;
                let id = self.book_by_index(v, a.lcid)?;
                Ok(self.alloc(Object::Workbook(id)))
            }
            "_NewEnum" => {
                let ids: Vec<BookId> = self.books.iter().map(|b| b.id).collect();
                let items = ids
                    .into_iter()
                    .map(|b| self.alloc(Object::Workbook(b)))
                    .collect();
                Ok(self.alloc_enumerator(items))
            }
            "Add" => self.add_with_events(a),
            "Open" | "_Open" => self.open_with_events(a),
            "Close" => {
                self.books.clear();
                self.active = None;
                Ok(Variant::Empty)
            }
            _ => Err(unsupported("Workbooks", m)),
        }
    }

    fn window_order(&self) -> Vec<BookId> {
        let active = self.active;
        let first = self
            .books
            .iter()
            .map(|b| b.id)
            .filter(|&b| Some(b) == active);
        let rest = self
            .books
            .iter()
            .map(|b| b.id)
            .filter(|&b| Some(b) != active);
        first.chain(rest).collect()
    }

    pub(crate) fn windows_member(
        &mut self,
        m: &'static Member,
        a: &Args,
    ) -> Result<Variant, OmError> {
        let order = self.window_order();
        match m.name {
            "Count" => Ok(Variant::I4(order.len() as i32)),
            "Item" | "_Default" => {
                let n = a.i32(0)?.ok_or_else(OmError::bad_index)?;
                let book = usize::try_from(n - 1)
                    .ok()
                    .and_then(|i| order.get(i).copied())
                    .ok_or_else(OmError::bad_index)?;
                Ok(self.alloc(Object::Window(book)))
            }
            _ => Err(unsupported("Windows", m)),
        }
    }

    pub(crate) fn window_member(
        &mut self,
        b: BookId,
        m: &'static Member,
    ) -> Result<Variant, OmError> {
        match m.name {
            "Activate" => {
                self.book(b)?;
                self.active = Some(b);
                Ok(Variant::Null)
            }
            _ => Err(unsupported("Window", m)),
        }
    }

    pub(crate) fn workbook_member(
        &mut self,
        b: BookId,
        m: &'static Member,
        a: &Args,
    ) -> Result<Variant, OmError> {
        use InvokeKind::*;
        match (m.name, m.kind) {
            ("Worksheets" | "Sheets" | "Charts", Get) => {
                self.book(b)?;
                Ok(self.alloc(Object::Sheets(b, sheets_kind(m.name))))
            }
            ("Names", Get) => {
                self.book(b)?;
                Ok(self.alloc(Object::Names(b, None)))
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
            ("Saved", Get) => Ok(Variant::Bool(self.book(b)?.saved)),
            ("Saved", Put) => {
                let v = self.put_bool(a)?;
                self.book_mut(b)?.saved = v;
                Ok(Variant::Empty)
            }
            ("Activate", Func) => {
                self.book(b)?;
                self.active = Some(b);
                Ok(Variant::Bool(true))
            }
            ("Close", Func) => self.close_with_events(b, a),
            _ => Err(unsupported("_Workbook", m)),
        }
    }

    fn kind_sheets(&self, b: BookId, kind: SheetsKind) -> Result<Vec<SheetId>, OmError> {
        Ok(self
            .book(b)?
            .wb
            .sheets()
            .filter(|s| match kind {
                SheetsKind::All => true,
                SheetsKind::Worksheets => s.kind == SheetKind::Worksheet,
                SheetsKind::Charts => s.kind == SheetKind::Chart,
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
            _ => Err(unsupported("Sheets", m)),
        }
    }

    fn sheet_arg(&self, b: BookId, v: Option<&Variant>) -> Result<Option<SheetId>, OmError> {
        match v {
            None => Ok(None),
            Some(Variant::Dispatch(Some(id))) => match self.arena.get(*id) {
                Some(Object::Worksheet(ob, s) | Object::Chart(ob, s)) if *ob == b => Ok(Some(*s)),
                _ => Err(OmError::app_defined()),
            },
            Some(_) => Err(OmError::type_mismatch()),
        }
    }

    fn add_sheets(&mut self, b: BookId, kind: SheetsKind, a: &Args) -> Result<Variant, OmError> {
        let before = self.sheet_arg(b, a.get(0))?;
        let after = self.sheet_arg(b, a.get(1))?;
        if before.is_some() && after.is_some() {
            return Err(OmError::app_defined());
        }
        let count = a.i32(2)?.unwrap_or(1);
        if count < 1 {
            return Err(OmError::app_defined());
        }

        let chart = match (kind, a.i32(3)?) {
            (SheetsKind::Charts, _) | (_, Some(-4109)) => true,
            (_, None | Some(-4167)) => false,
            _ => {
                return Err(OmError::unsupported(
                    "Sheets.Add of a sheet type other than xlWorksheet or xlChart",
                ))
            }
        };
        let mark = self.notice_mark(b);
        let book = self.book_mut(b)?;
        let mut last = None;
        for _ in 0..count {
            let anchor = before.unwrap_or_else(|| book.wb.active_sheet());
            let (prefix, counter) = if chart {
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
                    (pos + 1 < book.wb.sheet_count()).then(|| book.wb.sheet_at(pos + 1).id)
                }
                None => Some(anchor),
            };
            let id = if chart {
                book.wb.add_chart_sheet_before(&name, target)
            } else {
                book.wb.add_sheet_before(&name, target)
            }
            .map_err(edit_error)?;
            last = Some(id);
        }
        let id = last.ok_or_else(OmError::app_defined)?;

        if let Some(named) = before.or(after) {
            let log = book.wb.notices_mut();
            let mark = mark.min(log.len());
            if let Some(Notice::SheetActivated { previous, .. }) = log[mark..]
                .iter_mut()
                .find(|n| matches!(n, Notice::SheetActivated { .. }))
            {
                *previous = named;
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
            ("Name" | "Index" | "Visible" | "Delete" | "Activate" | "Select" | "Move", _) => {
                self.worksheet_member(b, s, m, a)
            }

            ("Type", Get) => Ok(Variant::I4(3)),
            ("Next" | "Previous", Get) => self.sheet_neighbour(b, s, m.name == "Next"),
            ("Parent", Get) => Ok(self.alloc(Object::Workbook(b))),
            _ => Err(unsupported("_Chart", m)),
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
        match (m.name, m.kind) {
            ("Name", Get) => Ok(Variant::Bstr(
                self.book(b)?
                    .wb
                    .sheet(s)
                    .map(|x| x.name.clone())
                    .unwrap_or_default(),
            )),
            ("Name", Put) => {
                let name = self.put_string(a)?;
                self.book_mut(b)?
                    .wb
                    .rename_sheet(s, &name)
                    .map_err(edit_error)?;
                Ok(Variant::Empty)
            }
            ("Type", Get) => Ok(Variant::I4(-4167)),
            ("Next" | "Previous", Get) => self.sheet_neighbour(b, s, m.name == "Next"),
            ("Index", Get) => Ok(Variant::I4(
                self.book(b)?
                    .wb
                    .sheet_position(s)
                    .map_or(0, |p| p as i32 + 1),
            )),
            ("Range", Get) => self.range_of_args(b, s, a),
            ("Cells", Get) => self.sheet_range(b, s, Mode::Cells),
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
            ("Calculate", Func) => {
                self.book_mut(b)?.wb.calculate_sheet(s);
                Ok(Variant::Bool(true))
            }
            (
                "AutoFilter" | "AutoFilterMode" | "FilterMode" | "ShowAllData" | "StandardWidth"
                | "StandardHeight",
                _,
            ) => self.sheet_view_member(b, s, m, a),
            (name, Get | Func) if crate::controls::is_collection_member(name) => {
                self.sheet_controls_member(b, s, m, a)
            }
            ("Names", Get) => Ok(self.alloc(Object::Names(b, Some(s)))),
            ("Scenarios", Func) => self.sheet_scenarios(b, s, a),
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
                wb.set_active_sheet(s).map_err(edit_error)?;
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
                self.book_mut(b)?.wb.delete_sheet(s).map_err(edit_error)?;
                Ok(Variant::Bool(true))
            }
            ("Paste", Func) => self.sheet_paste(b, s, m, a),
            ("Move" | "Copy", Func) => {
                let before = self.sheet_arg(b, a.get(0))?;
                let after = self.sheet_arg(b, a.get(1))?;
                if m.name == "Copy"
                    && self
                        .book(b)?
                        .wb
                        .sheet(s)
                        .is_some_and(|x| x.kind == SheetKind::Chart)
                {
                    return Err(OmError::unsupported("Chart.Copy"));
                }

                if before.is_some() == after.is_some() {
                    return if before.is_some() {
                        Err(OmError::app_defined())
                    } else {
                        Err(OmError::unsupported(
                            "Worksheet.Move or Copy into a new workbook",
                        ))
                    };
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
                }
                Ok(Variant::Bool(true))
            }

            ("ProtectContents", Get) => Ok(Variant::Bool(false)),
            ("CircularReference", Get) => {
                let hit = self.book(b)?.wb.circular_ref().filter(|c| c.sheet == s);
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
    ) -> Result<Variant, OmError> {
        use InvokeKind::*;
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
            ("Visible", Get) => Ok(Variant::Bool(true)),
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
        _ => SheetsKind::All,
    }
}
