use xlcalc::xlm::{self, XlmEnv, XlmError, XlmNames, XlmVars};
use xlcalc::{CellAddr, Value, Workbook, XlError};

use crate::app::{Application, BookId, Call, Mode};
use crate::conv::Args;
use crate::error::OmError;
use xlcalc_variant::{ObjectId, Variant};

use xlcalc::xlm::{Area, Operand};

struct AppEnv<'a, 'v> {
    app: &'a mut Application<'v>,
    book: BookId,

    close_after: bool,
}

fn first_element(v: Value) -> Value {
    match v {
        Value::Array(a) => a.data.first().cloned().unwrap_or(Value::Blank),
        v => v,
    }
}

fn first_value(wb: &Workbook, op: &Operand) -> Value {
    match op {
        Operand::Val(v) => v.clone(),
        Operand::Missing => Value::Blank,
        Operand::Ref(r) => r.areas().first().map_or(Value::Error(XlError::REF), |a| {
            wb.value(CellAddr::new(a.sheet, a.rect.r0, a.rect.c0))
        }),
    }
}

impl AppEnv<'_, '_> {
    fn wbr(&self) -> &Workbook {
        &self.app.book(self.book).expect("macro book").wb
    }

    fn range(&mut self, area: &Area) -> Result<ObjectId, XlmError> {
        match self
            .app
            .new_range(self.book, area.sheet, area.rect, Mode::Cells)
        {
            Ok(Variant::Dispatch(Some(id))) => Ok(id),
            _ => Err(XlmError::Failed("no range".into())),
        }
    }

    fn call(
        &mut self,
        id: ObjectId,
        member: &str,
        call: Call,
        args: &[Variant],
    ) -> Result<Variant, XlmError> {
        self.app
            .invoke(id, member, call, args, &[])
            .map_err(|e| XlmError::Failed(format!("{member}: {e:?}")))
    }

    fn selection(&mut self) -> Result<ObjectId, XlmError> {
        match self.app.app_selection() {
            Ok(Variant::Dispatch(Some(id))) => Ok(id),
            _ => Err(XlmError::Failed("no selection".into())),
        }
    }

    fn area_text(&self, a: &Area) -> String {
        let name = self
            .wbr()
            .sheet(a.sheet)
            .map(|s| s.name.replace('\'', "''"))
            .unwrap_or_default();
        let cell = |r: u32, c: u32| format!("${}${}", xlcalc::col_to_letters(c), r + 1);
        let one = a.rect.r0 == a.rect.r1 && a.rect.c0 == a.rect.c1;
        if one {
            format!("='{name}'!{}", cell(a.rect.r0, a.rect.c0))
        } else {
            format!(
                "='{name}'!{}:{}",
                cell(a.rect.r0, a.rect.c0),
                cell(a.rect.r1, a.rect.c1)
            )
        }
    }

    fn target(&mut self, op: Option<&Operand>) -> Result<ObjectId, XlmError> {
        match op.and_then(Self::reference) {
            Some(a) => self.range(&a.clone()),
            None => self.selection(),
        }
    }

    fn wbr_num(&self, args: &[Operand], i: usize) -> Option<i32> {
        match args.get(i).map(|a| first_value(self.wbr(), a)) {
            Some(Value::Number(n)) => Some(n as i32),
            _ => None,
        }
    }

    fn inv(
        &mut self,
        id: ObjectId,
        member: &str,
        call: Call,
        pos: &[Variant],
        named: &[(&str, Variant)],
    ) -> Result<Variant, XlmError> {
        self.app
            .invoke(id, member, call, pos, named)
            .map_err(|e| XlmError::Failed(format!("{member}: {e:?}")))
    }

    fn dispatch(v: Variant) -> Result<ObjectId, XlmError> {
        match v {
            Variant::Dispatch(Some(id)) => Ok(id),
            _ => Err(XlmError::Failed("no object".into())),
        }
    }

    fn active_book_obj(&mut self) -> Result<ObjectId, XlmError> {
        let root = self.app.handle();
        Self::dispatch(self.inv(root, "ActiveWorkbook", Call::GET, &[], &[])?)
    }

    fn sheet_obj(&mut self, op: Option<&Operand>) -> Result<ObjectId, XlmError> {
        let Some(Value::Text(t)) = op.map(|a| first_element(first_value(self.wbr(), a))) else {
            return Err(XlmError::Failed("sheet name".into()));
        };
        let name = t.rsplit_once(']').map_or(&*t, |x| x.1).to_string();
        let wb = self.active_book_obj()?;
        let sheets = Self::dispatch(self.inv(wb, "Sheets", Call::GET, &[], &[])?)?;
        Self::dispatch(self.inv(sheets, "Item", Call::GET, &[Variant::Bstr(name)], &[])?)
    }

    fn text_arg(&self, args: &[Operand], i: usize) -> Option<String> {
        match args.get(i).map(|a| first_value(self.wbr(), a)) {
            Some(Value::Text(t)) => Some(t.to_string()),
            _ => None,
        }
    }

    fn new_book(&mut self, args: &[Operand]) -> Result<Value, XlmError> {
        let ty = self.wbr_num(args, 0).unwrap_or(5);
        let template = match ty {
            1 => Some(-4167),
            2 => Some(-4109),
            3 => Some(3),
            4 => Some(4),
            5 | 6 => None,
            _ => return Ok(Value::Error(XlError::VALUE)),
        };
        let root = self.app.handle();
        let books = Self::dispatch(self.inv(root, "Workbooks", Call::GET, &[], &[])?)?;
        let pos: Vec<Variant> = template.map(Variant::I4).into_iter().collect();
        self.inv(books, "Add", Call::VB, &pos, &[])?;
        if ty == 6 {
            let wb = self.active_book_obj()?;
            let sheets = Self::dispatch(self.inv(wb, "Sheets", Call::GET, &[], &[])?)?;
            self.inv(sheets, "Add", Call::VB, &[], &[])?;

            let first = Self::dispatch(self.inv(
                sheets,
                "Item",
                Call::GET,
                &[Variant::Bstr("Sheet1".into())],
                &[],
            )?)?;
            self.inv(first, "Activate", Call::VB, &[], &[])?;
        }
        Ok(Value::Bool(true))
    }

    fn workbook_move(&mut self, name: &str, args: &[Operand]) -> Result<Value, XlmError> {
        let bad = Ok(Value::Error(XlError::VALUE));
        let Some(Value::Text(t)) = args
            .first()
            .map(|a| first_element(first_value(self.wbr(), a)))
        else {
            return bad;
        };
        let sheet = t.rsplit_once(']').map_or(&*t, |x| x.1).to_string();
        let book_name = self
            .app
            .book(self.book)
            .map(|b| b.name.clone())
            .unwrap_or_default();
        let (count, exists) = {
            let wb = self.wbr();
            (wb.sheet_count() as i32, wb.sheet_by_name(&sheet).is_some())
        };
        if !exists {
            return bad;
        }
        let mut other_book: Option<String> = None;
        match args.get(1).map(|a| first_value(self.wbr(), a)) {
            None | Some(Value::Blank) => {
                let s = self.sheet_obj(args.first())?;
                let m = if name == "WORKBOOK.MOVE" {
                    "Move"
                } else {
                    "Copy"
                };
                self.inv(s, m, Call::VB, &[], &[])?;
                return Ok(Value::Bool(true));
            }
            Some(Value::Text(b)) if b.eq_ignore_ascii_case(&book_name) => {}
            Some(Value::Text(b)) => {
                if self.app.has_book_named(&b) {
                    other_book = Some(b.to_string());
                } else {
                    return bad;
                }
            }
            Some(_) => return bad,
        }
        if let Some(ob) = other_book {
            let root = self.app.handle();
            let books = Self::dispatch(self.inv(root, "Workbooks", Call::GET, &[], &[])?)?;
            let tb = Self::dispatch(self.inv(
                books,
                "Item",
                Call::GET,
                &[Variant::Bstr(ob.as_str().into())],
                &[],
            )?)?;
            let tsheets = Self::dispatch(self.inv(tb, "Sheets", Call::GET, &[], &[])?)?;
            let tcount = self.get_i32(tsheets, "Count");
            let before = self
                .wbr_num(args, 2)
                .filter(|n| (1..=tcount + 1).contains(n));
            let Some(before) = before else {
                return bad;
            };
            let s = self.sheet_obj(args.first())?;
            let (arg, idx) = if before > tcount {
                ("After", tcount)
            } else {
                ("Before", before)
            };
            let at =
                Self::dispatch(self.inv(tsheets, "Item", Call::GET, &[Variant::I4(idx)], &[])?)?;
            let m = if name == "WORKBOOK.MOVE" {
                "Move"
            } else {
                "Copy"
            };
            self.inv(s, m, Call::VB, &[], &[(arg, Variant::Dispatch(Some(at)))])?;
            return Ok(Value::Bool(true));
        }
        let Some(before) = self
            .wbr_num(args, 2)
            .filter(|n| (1..=count + 1).contains(n))
        else {
            return bad;
        };
        let wb = self.active_book_obj()?;
        let sheets = Self::dispatch(self.inv(wb, "Sheets", Call::GET, &[], &[])?)?;
        let s = self.sheet_obj(args.first())?;
        let m = if name == "WORKBOOK.MOVE" {
            "Move"
        } else {
            "Copy"
        };

        let (arg, at_idx) = if before > count {
            ("After", count)
        } else {
            ("Before", before)
        };
        let at =
            Self::dispatch(self.inv(sheets, "Item", Call::GET, &[Variant::I4(at_idx)], &[])?)?;
        let at_name = self.get_text(at, "Name");
        let own_name = self.get_text(s, "Name");

        if !(name == "WORKBOOK.MOVE" && arg == "Before" && at_name == own_name) {
            self.inv(s, m, Call::VB, &[], &[(arg, Variant::Dispatch(Some(at)))])?;
        }
        if name == "WORKBOOK.MOVE" {
            self.inv(s, "Activate", Call::VB, &[], &[])?;
        }
        Ok(Value::Bool(true))
    }

    fn get_name(&mut self, args: &[Operand], at: CellAddr) -> Result<Value, XlmError> {
        let bad = Ok(Value::Error(XlError::NAME));
        let Some(Value::Text(t)) = args.first().map(|a| first_value(self.wbr(), a)) else {
            return bad;
        };
        if args.len() > 1 {
            return bad;
        }
        let book_name = self
            .app
            .book(self.book)
            .map(|b| b.name.clone())
            .unwrap_or_default();
        let wb = self.wbr();
        let (scope, name) = match t.rsplit_once('!') {
            Some((s, n)) => match wb.sheet_by_name(s.trim_matches('\'')) {
                Some(sh) => (sh.id, n),
                None => return bad,
            },
            None if wb.sheet(at.sheet).is_some_and(|s| s.kind.is_macro()) => (at.sheet, &*t),
            None => return bad,
        };
        let Some(def) = wb.names().lookup(&name.to_uppercase(), scope) else {
            return bad;
        };
        let mut text = format!("={}", wb.name_refers_to_r1c1(def));
        for s in wb.sheets() {
            let prefix = format!("{}!", xlcalc::testing::sheet_name_text(&s.name));
            let with = if s.id == scope {
                String::new()
            } else {
                format!("[{book_name}]{prefix}")
            };
            text = text.replace(&prefix, &with);
        }
        Ok(Value::Text(text.into()))
    }

    fn workbook_protect(&mut self, args: &[Operand]) -> Result<Value, XlmError> {
        let set = match args.first() {
            None | Some(Operand::Missing) => None,
            Some(a) => match first_value(self.wbr(), a) {
                Value::Number(n) => Some(n != 0.0),
                Value::Bool(b) => Some(b),
                Value::Blank => None,
                _ => return Ok(Value::Error(XlError::VALUE)),
            },
        };
        let password = match args.get(2).map(|a| first_value(self.wbr(), a)) {
            Some(Value::Text(t)) if !t.is_empty() => Some(t.to_string()),
            Some(Value::Number(n)) => Some(format!("{n}")),
            _ => None,
        };
        let Ok(book) = self.app.book_mut(self.book) else {
            return Ok(Value::Error(XlError::VALUE));
        };
        let protected = book.protect.is_some();
        let want = set.unwrap_or(!protected);
        if want && !protected {
            book.protect = Some(password);
        } else if !want && protected {
            let needs = book.protect.as_ref().and_then(Option::as_ref);

            if set.is_some() && needs.is_some() && needs != password.as_ref() {
                return Ok(Value::Error(XlError::VALUE));
            }
            book.protect = None;
        }
        Ok(Value::Bool(true))
    }

    fn protect_document_cmd(&mut self, args: &[Operand]) -> Result<Value, XlmError> {
        let flag = |i: usize| -> Result<Option<bool>, XlmError> {
            Ok(match args.get(i) {
                None | Some(Operand::Missing) => None,
                Some(a) => match first_value(self.wbr(), a) {
                    Value::Number(n) => Some(n != 0.0),
                    Value::Bool(b) => Some(b),
                    Value::Blank => None,
                    _ => return Err(XlmError::Failed("flag".into())),
                },
            })
        };
        let (Ok(c), Ok(o), Ok(s)) = (flag(0), flag(3), flag(4)) else {
            return Ok(Value::Error(XlError::VALUE));
        };
        let password = match args.get(2).map(|a| first_value(self.wbr(), a)) {
            Some(Value::Text(t)) if !t.is_empty() => Some(t.to_string()),
            Some(Value::Number(n)) => Some(format!("{n}")),
            _ => None,
        };
        let sheet = self.wbr().active_sheet();
        match self
            .app
            .protect_document(self.book, sheet, [c, o, s], password)
        {
            Ok(true) => Ok(Value::Bool(true)),
            _ => Ok(Value::Error(XlError::VALUE)),
        }
    }

    fn book_command(&mut self, name: &str, args: &[Operand]) -> Result<Value, XlmError> {
        let root = self.app.handle();
        let num = |wb: &Workbook, i: usize| match args.get(i).map(|a| first_value(wb, a)) {
            Some(Value::Number(n)) => Some(n),
            Some(Value::Bool(b)) => Some(f64::from(u8::from(b))),
            _ => None,
        };

        if matches!(
            name,
            "WORKBOOK.INSERT"
                | "WORKBOOK.DELETE"
                | "WORKBOOK.NAME"
                | "WORKBOOK.HIDE"
                | "WORKBOOK.UNHIDE"
                | "WORKBOOK.MOVE"
                | "WORKBOOK.COPY"
        ) && self.app.book(self.book).is_ok_and(|b| b.protect.is_some())
        {
            return Ok(Value::Error(XlError::VALUE));
        }
        match name {
            "WORKBOOK.PROTECT" => return self.workbook_protect(args),
            "WORKBOOK.MOVE" | "WORKBOOK.COPY" => return self.workbook_move(name, args),
            "NEW" => return self.new_book(args),
            "CLOSE" => {
                if self.app.active_book().is_ok_and(|b| b.id == self.book) {
                    self.close_after = true;
                } else {
                    let wb = self.active_book_obj()?;
                    self.inv(wb, "Close", Call::VB, &[Variant::Bool(false)], &[])?;
                }
                return Ok(Value::Bool(true));
            }
            "PASTE" => {
                let sheet = Self::dispatch(self.inv(root, "ActiveSheet", Call::GET, &[], &[])?)?;
                self.inv(sheet, "Paste", Call::VB, &[], &[])?;
            }
            "PASTE.SPECIAL" => {
                let kind = match num(self.wbr(), 0).unwrap_or(1.0) as i32 {
                    1 => -4104,
                    2 => -4123,
                    3 => -4163,
                    4 => -4122,
                    _ => return Ok(Value::Bool(false)),
                };
                let sel = self.selection()?;
                self.inv(sel, "PasteSpecial", Call::VB, &[Variant::I4(kind)], &[])?;
            }
            "SORT" => {
                let by = num(self.wbr(), 0).unwrap_or(1.0) as i32;
                let order = num(self.wbr(), 2).unwrap_or(2.0) as i32;
                if !(1..=2).contains(&by) || !(1..=2).contains(&order) {
                    return Ok(Value::Error(XlError::VALUE));
                }

                if args.get(1).is_none() {
                    return Ok(Value::Bool(true));
                }
                let key = self.target(args.get(1))?;
                let sel = self.selection()?;
                if by == 2 {
                    let row = |me: &mut Self, id| match me.inv(id, "Row", Call::GET, &[], &[]) {
                        Ok(Variant::I4(n)) => n,
                        _ => 0,
                    };
                    let krow = row(self, key);
                    let srow = row(self, sel);
                    let rows = Self::dispatch(self.inv(sel, "Rows", Call::GET, &[], &[])?)?;
                    let n = match self.inv(rows, "Count", Call::GET, &[], &[])? {
                        Variant::I4(n) => n,
                        _ => 0,
                    };
                    if krow < srow || krow >= srow + n {
                        return Ok(Value::Error(XlError::VALUE));
                    }
                }
                let named = [
                    ("Key1", Variant::Dispatch(Some(key))),
                    ("Order1", Variant::I4(order)),
                    ("Header", Variant::I4(2)),
                    ("Orientation", Variant::I4(by)),
                ];
                self.inv(sel, "Sort", Call::VB, &[], &named)?;
            }
            "FILTER" => {
                let sel = self.selection()?;
                self.inv(sel, "AutoFilter", Call::VB, &[], &[])?;
            }
            "WORKBOOK.INSERT" => {
                let ty = match num(self.wbr(), 0).unwrap_or(1.0) as i32 {
                    1 => -4167,
                    3 => 3,
                    4 => 4,
                    _ => return Ok(Value::Bool(false)),
                };
                let wb = self.active_book_obj()?;
                let sheets = Self::dispatch(self.inv(wb, "Sheets", Call::GET, &[], &[])?)?;
                self.inv(sheets, "Add", Call::VB, &[], &[("Type", Variant::I4(ty))])?;
            }
            "WORKBOOK.DELETE" => {
                let s = self.sheet_obj(args.first())?;
                self.inv(s, "Delete", Call::VB, &[], &[])?;
            }
            "WORKBOOK.NAME" => {
                let s = self.sheet_obj(args.first())?;
                let Some(n) = self.text_arg(args, 1) else {
                    return Ok(Value::Bool(false));
                };
                self.inv(s, "Name", Call::PUT, &[Variant::Bstr(n)], &[])?;
            }
            "WORKBOOK.ACTIVATE" | "WORKBOOK.SELECT" => {
                let s = self.sheet_obj(args.first())?;
                let m = if name == "WORKBOOK.ACTIVATE" {
                    "Activate"
                } else {
                    "Select"
                };
                self.inv(s, m, Call::VB, &[], &[])?;
            }
            "WORKBOOK.HIDE" | "WORKBOOK.UNHIDE" => {
                let s = self.sheet_obj(args.first())?;
                let v = if name == "WORKBOOK.HIDE" { 0 } else { -1 };
                self.inv(s, "Visible", Call::PUT, &[Variant::I4(v)], &[])?;

                if v == -1 {
                    self.inv(s, "Activate", Call::VB, &[], &[])?;
                }
            }
            "SAVE" => {
                let wb = self.active_book_obj()?;
                self.inv(wb, "Save", Call::VB, &[], &[])?;
            }
            "SAVE.AS" => {
                let Some(p) = self.text_arg(args, 0) else {
                    return Ok(Value::Bool(false));
                };
                let wb = self.active_book_obj()?;
                let mut named = vec![("Filename", Variant::Bstr(p))];
                if let Some(f) = num(self.wbr(), 1) {
                    named.push(("FileFormat", Variant::I4(f as i32)));
                }
                self.inv(wb, "SaveAs", Call::VB, &[], &named)?;
            }
            "OPEN" => {
                let Some(p) = self.text_arg(args, 0) else {
                    return Ok(Value::Bool(false));
                };
                let books = Self::dispatch(self.inv(root, "Workbooks", Call::GET, &[], &[])?)?;
                self.inv(books, "Open", Call::VB, &[Variant::Bstr(p)], &[])?;
            }
            _ => return Err(XlmError::Unsupported(name.to_string())),
        }
        Ok(Value::Bool(true))
    }

    fn get_i32(&mut self, id: ObjectId, member: &str) -> i32 {
        match self.inv(id, member, Call::GET, &[], &[]) {
            Ok(Variant::I4(n)) => n,
            _ => 0,
        }
    }

    fn get_text(&mut self, id: ObjectId, member: &str) -> Option<String> {
        match self.inv(id, member, Call::GET, &[], &[]) {
            Ok(Variant::Bstr(s)) => Some(s.to_string()),
            _ => None,
        }
    }

    fn doc_sheet(&self, args: &[Operand]) -> Result<xlcalc::SheetId, Value> {
        let Some(a) = args.get(1) else {
            return Ok(self.wbr().active_sheet());
        };
        let Value::Text(t) = first_element(first_value(self.wbr(), a)) else {
            return Err(Value::Error(XlError::NA));
        };
        let n = t.rsplit_once(']').map_or(&*t, |x| x.1).to_string();
        self.wbr()
            .sheet_by_name(&n)
            .map(|s| s.id)
            .ok_or(Value::Error(XlError::REF))
    }

    fn get_function(&mut self, name: &str, args: &[Operand]) -> Result<Value, XlmError> {
        let na = Value::Error(XlError::NA);
        let root = self.app.handle();
        let ty = match args.first().map(|a| first_value(self.wbr(), a)) {
            Some(Value::Number(n)) => n as i32,
            _ => 0,
        };
        let book = self.active_book_obj()?;
        let book_name = self.get_text(book, "Name").unwrap_or_default();
        let text = |s: String| Ok(Value::Text(s.into()));
        match (name, ty) {
            ("GET.WORKSPACE", 1) => text("Windows (64-bit) NT 10.00".into()),
            ("GET.WORKSPACE", 2) => text("16.0".into()),
            ("GET.DOCUMENT", 1 | 76) => {
                let sheet = match args.get(1) {
                    Some(_) => {
                        let s = self.sheet_obj(args.get(1))?;
                        self.get_text(s, "Name")
                    }
                    None => {
                        let s =
                            Self::dispatch(self.inv(root, "ActiveSheet", Call::GET, &[], &[])?)?;
                        self.get_text(s, "Name")
                    }
                };
                text(format!("[{book_name}]{}", sheet.unwrap_or_default()))
            }
            ("GET.DOCUMENT", 2) => {
                let path = self.get_text(book, "Path").unwrap_or_default();
                if path.is_empty() {
                    Ok(na)
                } else {
                    text(path)
                }
            }

            ("GET.WORKBOOK", 10) => Ok(na),
            ("GET.WORKBOOK", 11) => Ok(Value::Bool(false)),
            ("GET.WORKBOOK", 33 | 37) => {
                let member = if ty == 33 { "Title" } else { "Comments" };
                let v = self.inv(book, member, Call::GET, &[], &[])?;
                Ok(variant_value(&v).unwrap_or(na))
            }
            ("GET.DOCUMENT", 15..=17) => {
                let member = ["Iteration", "MaxIterations", "MaxChange"][(ty - 15) as usize];
                let v = self.inv(root, member, Call::GET, &[], &[])?;
                Ok(variant_value(&v).unwrap_or(na))
            }

            ("GET.DOCUMENT", 13) => {
                let w = Self::dispatch(self.inv(book, "Windows", Call::GET, &[], &[])?)?;
                let v = self.inv(w, "Count", Call::GET, &[], &[])?;
                Ok(variant_value(&v).unwrap_or(na))
            }
            ("GET.DOCUMENT", 14) => {
                Ok(match self.inv(root, "Calculation", Call::GET, &[], &[])? {
                    Variant::I4(-4105) => Value::Number(1.0),
                    Variant::R8(2.0) => Value::Number(2.0),
                    Variant::I4(-4135) => Value::Number(3.0),
                    _ => na,
                })
            }
            ("GET.DOCUMENT", 18) => {
                let v = self.inv(book, "UpdateRemoteReferences", Call::GET, &[], &[])?;
                Ok(variant_value(&v).unwrap_or(na))
            }
            ("GET.DOCUMENT", 48) => Ok(Value::Number(self.wbr().standard_width())),
            ("GET.WORKBOOK", 17) | ("GET.DOCUMENT", 34) => Ok(Value::Bool(false)),

            ("GET.DOCUMENT", 78) => Ok(Value::Number(600.0)),
            ("GET.DOCUMENT", 42) => {
                let s = Self::dispatch(self.inv(root, "ActiveSheet", Call::GET, &[], &[])?)?;
                let shapes = Self::dispatch(self.inv(s, "Shapes", Call::GET, &[], &[])?)?;
                if self.inv(shapes, "Count", Call::GET, &[], &[])? == Variant::I4(0) {
                    return Ok(na);
                }
                let first =
                    Self::dispatch(self.inv(shapes, "Item", Call::VB, &[Variant::I4(1)], &[])?)?;
                Ok(self
                    .get_text(first, "Name")
                    .map_or(na, |n| Value::Text(n.into())))
            }
            ("GET.DOCUMENT", 6) => {
                let v = self.inv(book, "HasPassword", Call::GET, &[], &[])?;
                Ok(variant_value(&v).unwrap_or(na))
            }
            ("GET.WORKBOOK", 34..=36) => {
                let member = ["Subject", "Author", "Keywords"][(ty - 34) as usize];
                let v = self.inv(book, member, Call::GET, &[], &[])?;
                Ok(variant_value(&v).unwrap_or(na))
            }
            ("GET.DOCUMENT", 19 | 20 | 43) | ("GET.WORKBOOK", 22) => {
                let member = match ty {
                    19 => "PrecisionAsDisplayed",
                    20 => "Date1904",
                    _ => "SaveLinkValues",
                };
                let v = self.inv(book, member, Call::GET, &[], &[])?;
                Ok(variant_value(&v).unwrap_or(na))
            }

            ("GET.DOCUMENT", 4) | ("GET.WORKBOOK", 24) => {
                let v = self.inv(book, "Saved", Call::GET, &[], &[])?;
                Ok(match v {
                    Variant::Bool(b) => Value::Bool(!b),
                    _ => na,
                })
            }

            ("GET.DOCUMENT", 21 | 22) => {
                let styles = Self::dispatch(self.inv(book, "Styles", Call::GET, &[], &[])?)?;
                let key = [Variant::Bstr("Normal".into())];
                let normal = Self::dispatch(self.inv(styles, "Item", Call::GET, &key, &[])?)?;
                let font = Self::dispatch(self.inv(normal, "Font", Call::GET, &[], &[])?)?;
                let member = if ty == 21 { "Name" } else { "Size" };
                let v = self.inv(font, member, Call::GET, &[], &[])?;
                Ok(variant_value(&v).unwrap_or(na))
            }

            ("GET.DOCUMENT", 3 | 87) => {
                let sheet = match self.doc_sheet(args) {
                    Ok(s) => s,
                    Err(v) => return Ok(v),
                };
                if ty == 87 {
                    let pos = self.wbr().sheet_position(sheet);
                    return Ok(pos.map_or(na, |p| Value::Number((p + 1) as f64)));
                }
                let s = match args.get(1) {
                    Some(_) => self.sheet_obj(args.get(1))?,
                    None => Self::dispatch(self.inv(root, "ActiveSheet", Call::GET, &[], &[])?)?,
                };
                Ok(match self.inv(s, "Type", Call::GET, &[], &[])? {
                    Variant::I4(-4167) => Value::Number(1.0),
                    Variant::I4(-4109) => Value::Number(2.0),
                    Variant::I4(3 | 4) => Value::Number(3.0),
                    _ => na,
                })
            }

            ("GET.DOCUMENT", 9..=12) => {
                let sheet = match self.doc_sheet(args) {
                    Ok(s) => s,
                    Err(v) => return Ok(v),
                };
                let n = self.wbr().ever_used(sheet).map_or(0, |r| match ty {
                    9 => r.r0 + 1,
                    10 => r.r1 + 1,
                    11 => r.c0 + 1,
                    _ => r.c1 + 1,
                });
                Ok(Value::Number(f64::from(n)))
            }

            ("GET.DOCUMENT", 7 | 44 | 83) => {
                let sheet = match self.doc_sheet(args) {
                    Ok(s) => s,
                    Err(v) => return Ok(v),
                };
                let p = self.app.sheet_prot(self.book, sheet);
                Ok(Value::Bool(match ty {
                    7 => p.contents,
                    44 => p.objects,
                    _ => p.scenarios,
                }))
            }

            ("GET.DOCUMENT", 49 | 50 | 52..=59 | 61..=63 | 77 | 79..=82) => {
                let sheet = match self.doc_sheet(args) {
                    Ok(s) => s,
                    Err(v) => return Ok(v),
                };
                let (b, ps) = (self.book, self.app.page_setup(self.book, sheet));
                let num = |n: f64| Ok(Value::Number(n));
                let whole = |t: &str| {
                    let t = t.replace('$', "");
                    t.chars().all(|c| c.is_ascii_alphabetic() || c == ':')
                        || t.chars().all(|c| c.is_ascii_digit() || c == ':')
                };
                match ty {
                    49 => ps
                        .first_page
                        .filter(|n| *n != -4105.0)
                        .map_or(Ok(na), |n| num(n.trunc())),
                    50 => {
                        let parts = self.app.print_area_parts(b, sheet);

                        num(if parts.len() == 1 && whole(&parts[0]) {
                            0.0
                        } else {
                            parts.len() as f64
                        })
                    }
                    52 => num(ps.left / 72.0),
                    53 => num(ps.orientation),
                    54 | 55 => text(self.app.header_footer_text(b, sheet, ty == 55)),
                    56 => Ok(Value::Bool(ps.center_horizontally)),
                    57 => Ok(Value::Bool(ps.headings)),
                    58 => Ok(Value::Bool(ps.gridlines)),
                    59 => Ok(Value::Bool(ps.black_and_white)),
                    61 => num(ps.order),
                    62 => ps.zoom.map_or(Ok(na), |n| num(f64::from(n))),
                    63 => match (ps.zoom, ps.fit_wide) {
                        (None, Some(n)) => num(f64::from(n)),
                        (None, None) => num(0.0),
                        _ => Ok(na),
                    },
                    77 => num(ps.paper_size),
                    79 => Ok(Value::Bool(ps.draft)),
                    80 => Ok(Value::Bool(ps.comments != -4142)),
                    81 => text(self.app.print_area_parts(b, sheet).join(",")),
                    _ => {
                        let t = self.app.print_title_rows(b, sheet).replace('$', "");
                        let mut ends: Vec<&str> = t.split(':').filter(|n| !n.is_empty()).collect();
                        ends.dedup();
                        text(
                            ends.iter()
                                .map(|n| format!("R{n}"))
                                .collect::<Vec<_>>()
                                .join(":"),
                        )
                    }
                }
            }
            ("GET.DOCUMENT", 68 | 88) | ("GET.WORKBOOK", 16) => text(book_name),
            ("GET.WORKBOOK", 1 | 3) => {
                let sheets = Self::dispatch(self.inv(book, "Sheets", Call::GET, &[], &[])?)?;
                let Variant::I4(n) = self.inv(sheets, "Count", Call::GET, &[], &[])? else {
                    return Ok(na);
                };
                let mut out = Vec::new();
                if ty == 3 {
                    let s = Self::dispatch(self.inv(root, "ActiveSheet", Call::GET, &[], &[])?)?;
                    let n = self.get_text(s, "Name").unwrap_or_default();
                    out.push(Value::Text(format!("[{book_name}]{n}").into()));
                } else {
                    for i in 1..=n {
                        let s = Self::dispatch(self.inv(
                            sheets,
                            "Item",
                            Call::GET,
                            &[Variant::I4(i)],
                            &[],
                        )?)?;
                        let n = self.get_text(s, "Name").unwrap_or_default();
                        out.push(Value::Text(format!("[{book_name}]{n}").into()));
                    }
                }
                if out.len() == 1 {
                    Ok(out.remove(0))
                } else {
                    let cols = out.len() as u32;
                    Ok(xlcalc::Array::new(1, cols, out).into_value())
                }
            }
            ("GET.WORKBOOK", 4) => {
                let sheets = Self::dispatch(self.inv(book, "Sheets", Call::GET, &[], &[])?)?;
                match self.inv(sheets, "Count", Call::GET, &[], &[])? {
                    Variant::I4(n) => Ok(Value::Number(f64::from(n))),
                    _ => Ok(na),
                }
            }
            ("GET.WORKBOOK", 38) => {
                let s = Self::dispatch(self.inv(root, "ActiveSheet", Call::GET, &[], &[])?)?;
                text(self.get_text(s, "Name").unwrap_or_default())
            }
            ("GET.DOCUMENT", 37) | ("GET.WORKBOOK", 20) => {
                match self.inv(book, "FileFormat", Call::GET, &[], &[])? {
                    Variant::I4(n) => Ok(Value::Number(f64::from(n))),
                    _ => Ok(Value::Number(51.0)),
                }
            }
            ("GET.FORMULA", _) => {
                let Some(a) = args.first().and_then(Self::reference) else {
                    return Ok(Value::Error(XlError::REF));
                };
                let a = *a;
                let addr = CellAddr::new(a.sheet, a.rect.r0, a.rect.c0);
                Ok(Value::Text(self.wbr().xlm_get_formula(addr).into()))
            }
            ("GET.NOTE", _) => text(String::new()),
            _ => Err(XlmError::Unsupported(format!("{name}({ty})"))),
        }
    }

    fn range_command(&mut self, name: &str, args: &[Operand]) -> Result<Value, XlmError> {
        let num = |wb: &Workbook, i: usize| match args.get(i).map(|a| first_value(wb, a)) {
            Some(Value::Number(n)) => Some(n),
            Some(Value::Bool(b)) => Some(f64::from(u8::from(b))),
            _ => None,
        };
        let text = |wb: &Workbook, i: usize| match args.get(i).map(|a| first_value(wb, a)) {
            Some(Value::Text(t)) => Some(t.to_string()),
            _ => None,
        };
        let flag = |wb: &Workbook, i: usize| match args.get(i).map(|a| first_value(wb, a)) {
            Some(Value::Bool(b)) => Some(b),
            Some(Value::Number(n)) => Some(n != 0.0),
            _ => None,
        };
        match name {
            "CLEAR" => {
                let member = match num(self.wbr(), 0).unwrap_or(3.0) as i32 {
                    1 => "Clear",
                    2 => "ClearFormats",
                    3 => "ClearContents",
                    4 => "ClearComments",
                    _ => return Ok(Value::Bool(false)),
                };
                let id = self.selection()?;
                self.call(id, member, Call::VB, &[])?;
            }
            "COPY" | "CUT" => {
                let member = if name == "COPY" { "Copy" } else { "Cut" };
                let id = self.target(args.first())?;
                match args.get(1).and_then(Self::reference) {
                    Some(to) => {
                        let dest = self.range(&to.clone())?;
                        self.call(id, member, Call::VB, &[Variant::Dispatch(Some(dest))])?;
                    }
                    None => {
                        self.call(id, member, Call::VB, &[])?;
                    }
                }
            }
            "INSERT" => {
                let id = self.selection()?;
                match num(self.wbr(), 0).unwrap_or(2.0) as i32 {
                    1 => self.call(id, "Insert", Call::VB, &[Variant::I4(-4161)])?,
                    2 => self.call(id, "Insert", Call::VB, &[Variant::I4(-4121)])?,
                    n @ (3 | 4) => {
                        let part = if n == 3 { "EntireRow" } else { "EntireColumn" };
                        let Variant::Dispatch(Some(e)) = self.call(id, part, Call::GET, &[])?
                        else {
                            return Ok(Value::Bool(false));
                        };
                        self.call(e, "Insert", Call::VB, &[])?
                    }
                    _ => return Ok(Value::Bool(false)),
                };
            }
            "FORMAT.NUMBER" => {
                let Some(fmt) = text(self.wbr(), 0) else {
                    return Ok(Value::Bool(false));
                };
                let id = self.selection()?;
                self.call(id, "NumberFormat", Call::PUT, &[Variant::Bstr(fmt)])?;
            }
            "FORMAT.FONT" => {
                let id = self.selection()?;
                let Variant::Dispatch(Some(font)) = self.call(id, "Font", Call::GET, &[])? else {
                    return Ok(Value::Bool(false));
                };
                if let Some(n) = text(self.wbr(), 0) {
                    self.call(font, "Name", Call::PUT, &[Variant::Bstr(n)])?;
                }
                if let Some(s) = num(self.wbr(), 1) {
                    self.call(font, "Size", Call::PUT, &[Variant::R8(s)])?;
                }
                if let Some(b) = flag(self.wbr(), 2) {
                    self.call(font, "Bold", Call::PUT, &[Variant::Bool(b)])?;
                }
                if let Some(b) = flag(self.wbr(), 3) {
                    self.call(font, "Italic", Call::PUT, &[Variant::Bool(b)])?;
                }
            }
            "COLUMN.WIDTH" | "ROW.HEIGHT" => {
                let Some(w) = num(self.wbr(), 0) else {
                    return Ok(Value::Bool(false));
                };
                let id = self.target(args.get(1))?;
                let member = if name == "COLUMN.WIDTH" {
                    "ColumnWidth"
                } else {
                    "RowHeight"
                };
                self.call(id, member, Call::PUT, &[Variant::R8(w)])?;
            }
            _ => return Err(XlmError::Unsupported(name.to_string())),
        }
        Ok(Value::Bool(true))
    }

    fn reference(op: &Operand) -> Option<&Area> {
        match op {
            Operand::Ref(r) => r.areas().first(),
            _ => None,
        }
    }
}

impl XlmEnv for AppEnv<'_, '_> {
    fn wb(&self) -> &Workbook {
        self.wbr()
    }

    fn store(&mut self, addr: CellAddr, v: Value) {
        if let Ok(b) = self.app.book_mut(self.book) {
            b.wb.xlm_store(addr, v);
        }
    }

    fn command(&mut self, name: &str, args: &[Operand], at: CellAddr) -> Result<Value, XlmError> {
        let yes = Ok(Value::Bool(true));
        match name {
            "SET.VALUE" => {
                let (Some(area), Some(val)) = (Self::reference(&args[0]), args.get(1)) else {
                    return Ok(Value::Error(XlError::VALUE));
                };
                let v = first_value(self.wbr(), val);
                let id = self.range(&area.clone())?;
                self.call(id, "Value2", Call::PUT, &[crate::vba::value_variant(&v)])?;
                yes
            }
            "FORMULA" | "FORMULA.FILL" => {
                let text = match args.first() {
                    Some(op) => first_value(self.wbr(), op),
                    None => return Ok(Value::Error(XlError::VALUE)),
                };
                let area = match args.get(1) {
                    Some(op) => Self::reference(op).cloned(),
                    None => {
                        let wb = self.wbr();
                        let s = wb.active_sheet();
                        let (r, c) = wb.active_cell_of(s);
                        Some(Area {
                            sheet: s,
                            rect: xlcalc::Rect::new(r, c, r, c),
                        })
                    }
                };
                let Some(area) = area else {
                    return Ok(Value::Bool(false));
                };
                let id = self.range(&area)?;
                let v = crate::vba::value_variant(&text);
                self.call(id, "FormulaR1C1", Call::PUT, &[v])?;
                yes
            }
            "ACTIVATE" => {
                let name = match args.first() {
                    Some(op) => first_value(self.wbr(), op),
                    None => return Ok(Value::Bool(false)),
                };
                let Value::Text(t) = name else {
                    return Ok(Value::Bool(false));
                };
                let sheet = t.rsplit_once(']').map_or(&*t, |x| x.1).to_string();
                let found = self.wbr().sheet_by_name(&sheet).map(|s| s.id);
                match found {
                    Some(id) => {
                        if let Ok(b) = self.app.book_mut(self.book) {
                            let _ = b.wb.set_active_sheet(id);
                        }
                        yes
                    }
                    None => Ok(Value::Bool(false)),
                }
            }
            "ACTIVE.CELL" => {
                let wb = self.wbr();
                let s = wb.active_sheet();
                let (r, c) = wb.active_cell_of(s);
                Ok(wb.value(CellAddr::new(s, r, c)))
            }
            "SELECT" => {
                let Some(area) = Self::reference(&args[0]) else {
                    return Ok(Value::Bool(false));
                };
                let id = self.range(&area.clone())?;
                self.call(id, "Select", Call::VB, &[])?;
                yes
            }
            "CLEAR" | "COPY" | "CUT" | "INSERT" | "FORMAT.NUMBER" | "FORMAT.FONT"
            | "COLUMN.WIDTH" | "ROW.HEIGHT" => self.range_command(name, args),
            "DEFINE.NAME" => {
                let Some(Value::Text(n)) = args.first().map(|a| first_value(self.wbr(), a)) else {
                    return Ok(Value::Error(XlError::VALUE));
                };
                let refers = match args.get(1) {
                    Some(op) => match Self::reference(op) {
                        Some(a) => self.area_text(a),
                        None => match first_value(self.wbr(), op) {
                            Value::Text(t) => t.to_string(),
                            Value::Number(x) => format!("={x}"),
                            _ => return Ok(Value::Error(XlError::VALUE)),
                        },
                    },
                    None => return Ok(Value::Error(XlError::VALUE)),
                };
                let ok = self
                    .app
                    .book_mut(self.book)
                    .is_ok_and(|b| b.wb.define_name(&n, &refers, None).is_ok());
                Ok(Value::Bool(ok))
            }
            "DELETE.NAME" => {
                let Some(Value::Text(n)) = args.first().map(|a| first_value(self.wbr(), a)) else {
                    return Ok(Value::Error(XlError::VALUE));
                };
                let ok = self
                    .app
                    .book_mut(self.book)
                    .is_ok_and(|b| b.wb.delete_name(&n, None).is_some());
                Ok(Value::Bool(ok))
            }
            "PASTE" | "PASTE.SPECIAL" | "SORT" | "FILTER" | "WORKBOOK.INSERT"
            | "WORKBOOK.DELETE" | "WORKBOOK.NAME" | "WORKBOOK.ACTIVATE" | "WORKBOOK.SELECT"
            | "WORKBOOK.HIDE" | "WORKBOOK.UNHIDE" | "WORKBOOK.PROTECT" | "WORKBOOK.MOVE"
            | "WORKBOOK.COPY" | "NEW" | "CLOSE" | "SAVE" | "SAVE.AS" | "OPEN" => {
                self.book_command(name, args)
            }
            "GET.NAME" => self.get_name(args, at),
            "PROTECT.DOCUMENT" => self.protect_document_cmd(args),
            "GET.DOCUMENT" | "GET.WORKBOOK" | "GET.WORKSPACE" | "GET.FORMULA" | "GET.NOTE" => {
                self.get_function(name, args)
            }

            "INPUT" => Ok(Value::Bool(false)),
            "DELETE" => {
                let id = self.selection()?;
                match self.wbr_num(args, 0).unwrap_or(2) {
                    1 => self.call(id, "Delete", Call::VB, &[Variant::I4(-4159)])?,
                    2 => self.call(id, "Delete", Call::VB, &[Variant::I4(-4162)])?,
                    n @ (3 | 4) => {
                        let part = if n == 3 { "EntireRow" } else { "EntireColumn" };
                        let Variant::Dispatch(Some(e)) = self.call(id, part, Call::GET, &[])?
                        else {
                            return Ok(Value::Bool(false));
                        };
                        self.call(e, "Delete", Call::VB, &[])?
                    }
                    _ => return Ok(Value::Bool(false)),
                };
                yes
            }
            "ALERT" | "MESSAGE" | "BEEP" | "ECHO" | "ERROR" | "CANCEL.KEY" | "ON.TIME" => yes,
            other => Err(XlmError::Unsupported(other.to_string())),
        }
    }

    fn persist(&mut self, vars: &XlmVars, names: &XlmNames) {
        let mut defs = Vec::new();
        for ((sheet, key), op) in vars {
            let text = match op {
                Operand::Val(Value::Number(n)) => format!("={n}"),
                Operand::Val(Value::Text(t)) => format!("=\"{}\"", t.replace('"', "\"\"")),
                Operand::Val(Value::Bool(b)) => format!("={}", if *b { "TRUE" } else { "FALSE" }),
                Operand::Val(Value::Error(e)) => format!("={e}"),
                Operand::Ref(r) => match r.areas().first() {
                    Some(a) => {
                        let wb = self.wbr();
                        let sn = wb
                            .sheet(a.sheet)
                            .map(|s| s.name.clone())
                            .unwrap_or_default();
                        format!(
                            "='{}'!R{}C{}",
                            sn.replace('\'', "''"),
                            a.rect.r0 + 1,
                            a.rect.c0 + 1
                        )
                    }
                    None => continue,
                },
                _ => continue,
            };
            let shown = names
                .get(&(*sheet, key.clone()))
                .cloned()
                .unwrap_or_else(|| key.to_string());
            defs.push((*sheet, shown, text));
        }
        let Ok(b) = self.app.book_mut(self.book) else {
            return;
        };
        for (sheet, key, text) in defs {
            let _ = b.wb.define_name(&key, &text, Some(sheet));
        }
    }
}

fn run_error(e: XlmError) -> OmError {
    match e {
        XlmError::Unsupported(n) => OmError::unsupported(&format!("macro function {n}")),
        XlmError::NoReturn => {
            OmError::app_defined_with("No RETURN() or HALT() function found on macro sheet.")
        }
        XlmError::Runaway => OmError::app_defined_with("The macro does not stop."),
        XlmError::Failed(m) => OmError::app_defined_with(&m),
    }
}

impl Application<'_> {
    fn xlm_target(&self, text: &str) -> Option<(BookId, CellAddr)> {
        let book = self.active_book().ok()?;
        let wb = &book.wb;
        let key = text.to_uppercase();
        if let Some(t) = xlm::name_target(wb, &key, wb.active_sheet()) {
            return Some((book.id, t));
        }
        let (sheet, cell) = text.rsplit_once('!')?;
        let sheet = sheet.trim_matches('\'');
        let s = wb.sheet_by_name(sheet).filter(|s| s.kind.is_macro())?;
        let at = xlcalc::parse_cell_ref(wb, &cell.replace('$', ""), s.id)?;
        Some((book.id, at))
    }

    pub(crate) fn xlm_run(&mut self, text: &str, a: &Args) -> Option<Result<Variant, OmError>> {
        let (book, start) = self.xlm_target(text)?;
        let mut args = Vec::new();
        for v in a.values.iter().skip(1) {
            match v {
                Some(v) if !crate::conv::is_missing(v) => match crate::vba::variant_value(v) {
                    Ok(v) => args.push(Operand::Val(v)),
                    Err(e) => return Some(Err(e)),
                },
                _ => args.push(Operand::Missing),
            }
        }
        while matches!(args.last(), Some(Operand::Missing)) {
            args.pop();
        }
        let mut env = AppEnv {
            app: self,
            book,
            close_after: false,
        };
        let r = xlm::run(&mut env, start, args)
            .map(|v| crate::vba::value_variant(&first_element(v)))
            .map_err(run_error);
        let close = env.close_after;
        self.close_active_after_macro(close);
        Some(r)
    }

    pub(crate) fn execute_excel4_macro(&mut self, a: &Args) -> Result<Variant, OmError> {
        let text = a.string(0)?.ok_or_else(OmError::app_defined)?;
        let (book, host) = {
            let b = self.active_book()?;

            let s = b.wb.active_sheet();
            (b.id, CellAddr::new(s, 1, 1))
        };
        let mut env = AppEnv {
            app: self,
            book,
            close_after: false,
        };
        let r = match xlm::eval_text(&mut env, &text, host) {
            Err(_) => Err(OmError::app_defined_with(
                "There's an error in the formula you entered.",
            )),
            Ok(r) => r
                .map(|v| match first_element(v) {
                    Value::Blank => Variant::R8(0.0),
                    v => crate::vba::value_variant(&v),
                })
                .map_err(run_error),
        };
        let close = env.close_after;
        self.close_active_after_macro(close);
        r
    }

    fn close_active_after_macro(&mut self, close: bool) {
        if !close {
            return;
        }
        let root = self.handle();
        if let Ok(Variant::Dispatch(Some(wb))) =
            self.invoke(root, "ActiveWorkbook", Call::GET, &[], &[])
        {
            let _ = self.invoke(wb, "Close", Call::VB, &[Variant::Bool(false)], &[]);
        }
    }
}

fn variant_value(v: &Variant) -> Option<Value> {
    Some(match v {
        Variant::Bool(b) => Value::Bool(*b),
        Variant::I4(n) => Value::Number(f64::from(*n)),
        Variant::R8(x) => Value::Number(*x),
        Variant::Bstr(t) => Value::Text(t.as_str().into()),
        _ => return None,
    })
}
