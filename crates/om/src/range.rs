use xlcalc::{CellAddr, DateSystem, EditError, Rect, SheetId, Value, Workbook, XlError};
use xlcalc_variant::{change_type, HResult, Locale, VarType, Variant};

use crate::addr::{
    external_prefix, parse_range_text, range_text, AddressStyle, MAX_COLS, MAX_ROWS,
};
use crate::app::{unsupported, Application, BookId, Mode, Object, RangeRef};
use crate::conv::{
    array_cells, array_variant, cell_variant, currency_overflow, date_overflow, input_locale,
    scode_error, Args, Flavour, HOST_LCID,
};
use crate::error::{vba_scode, OmError, DISP_E_TYPEMISMATCH};
use crate::objects::{edit_error, whole_sheet};
use crate::typelib::{InvokeKind, Member};

fn mergeable(a: &Rect, b: &Rect) -> bool {
    let holds = |x: &Rect, y: &Rect| x.r0 <= y.r0 && x.c0 <= y.c0 && x.r1 >= y.r1 && x.c1 >= y.c1;
    let touch = |a0: u32, a1: u32, b0: u32, b1: u32| {
        a0 <= b1.saturating_add(1) && b0 <= a1.saturating_add(1)
    };
    holds(a, b)
        || holds(b, a)
        || (a.r0 == b.r0 && a.r1 == b.r1 && touch(a.c0, a.c1, b.c0, b.c1))
        || (a.c0 == b.c0 && a.c1 == b.c1 && touch(a.r0, a.r1, b.r0, b.r1))
}

fn union_add(mut list: Vec<Rect>, area: Rect) -> Vec<Rect> {
    let mut cur = area;
    while let Some(i) = list.iter().position(|x| mergeable(x, &cur)) {
        cur = list.remove(i).union_bounds(&cur);
    }
    list.push(cur);
    list
}

const MAX_READ_CELLS: u64 = 4_000_000;

pub(crate) fn r1c1_to_a1(text: &str, row: u32, col: u32) -> String {
    let b: Vec<char> = text.chars().collect();
    let mut out = String::new();
    let mut i = 0;
    let ident = |c: char| c.is_alphanumeric() || c == '_' || c == '.';
    while i < b.len() {
        let c = b[i];
        if c == '"' {
            out.push(c);
            i += 1;
            while i < b.len() {
                out.push(b[i]);
                i += 1;
                if b[i - 1] == '"' {
                    if i < b.len() && b[i] == '"' {
                        out.push('"');
                        i += 1;
                    } else {
                        break;
                    }
                }
            }
            continue;
        }
        let prev_ident = i > 0 && ident(b[i - 1]);
        if (c == 'R' || c == 'r' || c == 'C' || c == 'c') && !prev_ident {
            if let Some((text, used)) = parse_r1c1_ref(&b[i..], row, col) {
                let after = b.get(i + used).copied();
                if !after.is_some_and(|a| ident(a) || a == '(') {
                    out.push_str(&text);
                    i += used;
                    continue;
                }
            }
        }
        out.push(c);
        i += 1;
    }
    out
}

fn parse_r1c1_ref(s: &[char], row: u32, col: u32) -> Option<(String, usize)> {
    fn axis(s: &[char], pos: &mut usize, letter: char) -> Option<Option<(bool, i64)>> {
        if s.get(*pos).map(|c| c.to_ascii_uppercase()) != Some(letter) {
            return None;
        }
        *pos += 1;
        match s.get(*pos) {
            Some('[') => {
                let end = s[*pos..].iter().position(|&c| c == ']')? + *pos;
                let n: i64 = s[*pos + 1..end].iter().collect::<String>().parse().ok()?;
                *pos = end + 1;
                Some(Some((false, n)))
            }
            Some(d) if d.is_ascii_digit() => {
                let start = *pos;
                while s.get(*pos).is_some_and(char::is_ascii_digit) {
                    *pos += 1;
                }
                let n: i64 = s[start..*pos].iter().collect::<String>().parse().ok()?;
                Some(Some((true, n)))
            }
            _ => Some(None),
        }
    }
    let mut pos = 0;
    let r = axis(s, &mut pos, 'R');
    let c = if r.is_some() {
        let mut p2 = pos;
        match axis(s, &mut p2, 'C') {
            Some(c) => {
                pos = p2;
                Some(c)
            }
            None => None,
        }
    } else {
        axis(s, &mut pos, 'C').map(Some)?
    };
    let row_txt = |v: Option<(bool, i64)>| -> Option<String> {
        let n = match v {
            Some((true, n)) => n,
            Some((false, d)) => row as i64 + d + 1,
            None => row as i64 + 1,
        };
        let abs = matches!(v, Some((true, _)));
        (1..=MAX_ROWS as i64)
            .contains(&n)
            .then(|| format!("{}{n}", if abs { "$" } else { "" }))
    };
    let col_txt = |v: Option<(bool, i64)>| -> Option<String> {
        let n = match v {
            Some((true, n)) => n - 1,
            Some((false, d)) => col as i64 + d,
            None => col as i64,
        };
        let abs = matches!(v, Some((true, _)));
        (0..MAX_COLS as i64).contains(&n).then(|| {
            format!(
                "{}{}",
                if abs { "$" } else { "" },
                xlcalc::col_to_letters(n as u32)
            )
        })
    };
    match (r, c) {
        (Some(r), Some(c)) => {
            let (rt, ct) = (row_txt(r), col_txt(c));
            let col_part = ct.unwrap_or_else(|| "#REF!".into());
            let row_part = rt.unwrap_or_else(|| "#REF!".into());
            if col_part == "#REF!" || row_part == "#REF!" {
                return Some(("#REF!".into(), pos));
            }
            Some((format!("{col_part}{row_part}"), pos))
        }
        (Some(r), None) => row_txt(r).map(|t| (t, pos)),
        (None, Some(c)) => col_txt(c).map(|t| (t, pos)),
        (None, None) => None,
    }
}

impl Application<'_> {
    fn rect_cells(sheet: SheetId, rect: Rect) -> impl Iterator<Item = CellAddr> {
        (rect.r0..=rect.r1)
            .flat_map(move |r| (rect.c0..=rect.c1).map(move |c| CellAddr::new(sheet, r, c)))
    }

    fn read_cells(
        &self,
        book: BookId,
        sheet: SheetId,
        rect: Rect,
        f: impl Fn(&Workbook, CellAddr) -> Variant,
    ) -> Result<Variant, OmError> {
        let wb = &self.book(book)?.wb;
        if rect.is_cell() {
            return Ok(f(wb, CellAddr::new(sheet, rect.r0, rect.c0)));
        }
        if rect.cell_count() > MAX_READ_CELLS {
            return Err(OmError::exception(vba_scode(7), None));
        }
        let items = Application::rect_cells(sheet, rect)
            .map(|a| f(wb, a))
            .collect();
        Ok(array_variant(rect.rows(), rect.cols(), items))
    }

    fn value_variant(wb: &Workbook, a: CellAddr, flavour: Flavour) -> Variant {
        let d1904 = wb.settings().date_system == DateSystem::D1904;
        cell_variant(&wb.value(a), wb.number_format(a), flavour, d1904)
    }

    fn formula_variant(wb: &Workbook, a: CellAddr, r1c1: bool, legacy: bool) -> Variant {
        let text = if r1c1 {
            wb.formula_text_r1c1(a)
        } else if legacy {
            wb.legacy_formula_text(a)
        } else {
            wb.formula_text(a)
        };
        let spilled_child = wb.spill_anchor(a).is_some_and(|anchor| anchor != a);
        Variant::Bstr(text.unwrap_or_else(|| match wb.value(a) {
            _ if spilled_child => String::new(),
            Value::Blank => String::new(),
            Value::Number(n) => xlcalc::format_general(n),
            Value::Text(t) => t.to_string(),
            Value::Bool(b) => if b { "TRUE" } else { "FALSE" }.into(),
            Value::Error(e) => e.to_string(),
            Value::Array(_) | Value::Lambda(_) => "#VALUE!".into(),
        }))
    }

    fn same_or_null(
        &self,
        book: BookId,
        sheet: SheetId,
        rect: Rect,
        f: impl Fn(&Workbook, CellAddr) -> Variant,
    ) -> Result<Variant, OmError> {
        self.same_or_null_in(book, &[(sheet, rect)], f)
    }

    fn same_or_null_in(
        &self,
        book: BookId,
        parts: &[(SheetId, Rect)],
        f: impl Fn(&Workbook, CellAddr) -> Variant,
    ) -> Result<Variant, OmError> {
        let wb = &self.book(book)?.wb;
        let mut first: Option<Variant> = None;
        for &(sheet, rect) in parts {
            for a in Application::rect_cells(sheet, rect) {
                let v = f(wb, a);
                match &first {
                    None => first = Some(v),
                    Some(p) if *p == v => {}
                    Some(_) => return Ok(Variant::Null),
                }
            }
        }
        Ok(first.unwrap_or(Variant::Null))
    }

    fn sub_range(
        &mut self,
        r: &RangeRef,
        sheet: SheetId,
        rect: Rect,
        mode: Mode,
    ) -> Result<Variant, OmError> {
        self.new_range(r.book, sheet, rect, mode)
    }

    fn index_arg(a: &Args, i: usize, column: bool) -> Result<Option<i64>, OmError> {
        if column {
            if let Some(Variant::Bstr(t)) = a.get(i) {
                if let Some(c) = xlcalc::letters_to_col(t.trim()) {
                    return Ok(Some(c as i64 + 1));
                }
            }
        }
        Ok(a.i32(i)?.map(i64::from))
    }

    fn item(
        &mut self,
        r: &RangeRef,
        sheet: SheetId,
        rect: Rect,
        a: &Args,
        direct: bool,
    ) -> Result<Variant, OmError> {
        if let (Some(Variant::Bstr(t)), None, Mode::Rows | Mode::Columns) =
            (a.get(0), a.get(1), r.mode)
        {
            if t.contains(':') && rect == whole_sheet() {
                let (s, found) = parse_range_text(&self.book(r.book)?.wb, sheet, t)
                    .ok_or_else(OmError::app_defined)?;
                return self.sub_range(r, s, found, Mode::Cells);
            }
        }

        let index = |i: usize, column: bool| {
            Application::index_arg(a, i, column).map_err(|e| match a.get(i) {
                Some(Variant::Bstr(_)) if direct => OmError::app_defined(),
                _ => e,
            })
        };

        if let (Some(Variant::Bstr(t)), None, Mode::Rows | Mode::Columns) =
            (a.get(0), a.get(1), r.mode)
        {
            let columns = r.mode == Mode::Columns;
            let axis = |part: &str| -> Option<i64> {
                let part = part.trim();
                if columns {
                    xlcalc::letters_to_col(part).map(|c| i64::from(c) + 1)
                } else {
                    part.parse::<i64>().ok()
                }
            };
            let (lo, hi) = match t.split_once(':') {
                Some((x, y)) => (axis(x), axis(y)),
                None => (axis(t), axis(t)),
            };
            let (Some(lo), Some(hi)) = (lo, hi) else {
                return Err(OmError::app_defined());
            };
            let (lo, hi) = (lo.min(hi), lo.max(hi));
            let (r0, c0) = (i64::from(rect.r0), i64::from(rect.c0));
            let found = if columns {
                grid_rect(r0, c0 + lo - 1, i64::from(rect.r1), c0 + hi - 1)
            } else {
                grid_rect(r0 + lo - 1, c0, r0 + hi - 1, i64::from(rect.c1))
            };
            let found = found.ok_or_else(OmError::app_defined)?;
            return self.sub_range(r, sheet, found, r.mode);
        }

        if direct && a.get(1).is_some() && r.mode != Mode::Cells {
            return Err(OmError::app_defined());
        }
        let first = index(0, false)?;
        let second = index(1, true)?;

        let (r0, c0) = (i64::from(rect.r0), i64::from(rect.c0));
        let target = match (first, second, r.mode) {
            (None, _, _) => return Err(OmError::app_defined()),
            (Some(n), None, Mode::Rows) => {
                grid_rect(r0 + n - 1, c0, r0 + n - 1, i64::from(rect.c1))
            }
            (Some(n), None, Mode::Columns) => {
                grid_rect(r0, c0 + n - 1, i64::from(rect.r1), c0 + n - 1)
            }
            (Some(n), None, Mode::Cells) => {
                let cols = i64::from(rect.cols());
                let (row, col) = (r0 + (n - 1) / cols, c0 + (n - 1) % cols);
                grid_rect(row, col, row, col)
            }
            (Some(i), Some(j), _) => grid_rect(r0 + i - 1, c0 + j - 1, r0 + i - 1, c0 + j - 1),
        };
        let rect = target.ok_or_else(OmError::app_defined)?;
        let mode = if second.is_some() {
            Mode::Cells
        } else {
            r.mode
        };
        let mode = if mode == Mode::Cells || second.is_some() {
            Mode::Cells
        } else {
            mode
        };
        self.sub_range(r, sheet, rect, mode)
    }

    pub(crate) fn range_member(
        &mut self,
        r: &RangeRef,
        m: &'static Member,
        a: &Args,
    ) -> Result<Variant, OmError> {
        use InvokeKind::*;
        let (sheet, rect) = self.resolve(r)?;
        let book = r.book;
        let top_left = CellAddr::new(sheet, rect.r0, rect.c0);

        match (m.name, m.kind) {
            (
                "Value" | "Value2" | "_Default" | "Formula" | "FormulaLocal" | "Formula2"
                | "Formula2Local" | "FormulaR1C1",
                Put,
            )
            | ("ClearContents" | "Clear", Func) => {
                self.book_mut(book)?.wb.note_cell_format(top_left)
            }
            ("NumberFormat" | "NumberFormatLocal" | "Text", Get) => {
                let wb = &mut self.book_mut(book)?.wb;
                let f = wb.number_format(top_left).to_string();
                wb.note_number_format(Some(&f));
            }
            _ => {}
        }
        match (m.name, m.kind) {
            ("Select" | "Activate", Func) => self.range_select(book, sheet, &[rect], m),
            ("Address", Get) => self.address_text(book, &[(sheet, rect)], a),
            ("Areas", Get) => {
                let areas = self.track_areas(book, sheet, &[rect], Mode::Cells)?;
                Ok(self.alloc(Object::Areas(areas)))
            }
            ("Calculate", Func) => {
                self.book_mut(book)?.wb.calculate_range(sheet, rect);
                Ok(Variant::Null)
            }
            ("Cells" | "Rows" | "Columns", Get) => {
                let mode = match m.name {
                    "Cells" => Mode::Cells,
                    "Rows" => Mode::Rows,
                    _ => Mode::Columns,
                };
                if a.get(0).is_some() || a.get(1).is_some() {
                    let moded = RangeRef { mode, ..*r };
                    return self.item(&moded, sheet, rect, a, false);
                }
                self.sub_range(r, sheet, rect, mode)
            }
            ("ClearContents", Func) => {
                self.book_mut(book)?
                    .wb
                    .clear_range(sheet, rect)
                    .map_err(edit_error)?;
                Ok(Variant::Empty)
            }
            ("Insert", Func) | ("Delete", Func) => {
                let insert = m.name == "Insert";
                let shift = a.i32(0)?;
                let whole_rows = rect.c0 == 0 && rect.c1 == MAX_COLS - 1;
                let whole_cols = rect.r0 == 0 && rect.r1 == MAX_ROWS - 1;
                if whole_rows && whole_cols {
                    return Err(OmError::app_defined());
                }
                let wb = &mut self.book_mut(book)?.wb;
                let done = if whole_rows {
                    let n = rect.rows();
                    if insert {
                        wb.insert_rows(sheet, rect.r0, n)
                    } else {
                        wb.delete_rows(sheet, rect.r0, n)
                    }
                } else if whole_cols {
                    let n = rect.cols();
                    if insert {
                        wb.insert_cols(sheet, rect.c0, n)
                    } else {
                        wb.delete_cols(sheet, rect.c0, n)
                    }
                } else {
                    let down = match (shift, insert) {
                        (Some(-4121), true) | (Some(-4162), false) => true,
                        (Some(-4161), true) | (Some(-4159), false) => false,
                        (None, _) => rect.cols() >= rect.rows(),
                        _ => return Err(OmError::app_defined()),
                    };
                    if insert {
                        wb.insert_cells(sheet, rect, down)
                    } else {
                        wb.delete_cells(sheet, rect, down)
                    }
                };
                done.map_err(edit_error)?;
                Ok(Variant::Bool(true))
            }
            ("Cut", Func) | ("Copy", Func) => {
                let Some(Variant::Dispatch(Some(id))) = a.get(0) else {
                    if m.name == "Copy" {
                        self.clip = Some((book, sheet, rect));
                        return Ok(Variant::Bool(true));
                    }
                    return Err(unsupported("Range", m));
                };
                let Some(Object::Range(dest)) = self.arena.get(*id) else {
                    return Err(OmError::app_defined());
                };
                let dest = *dest;
                let (ds, dr) = self.resolve(&dest)?;
                self.paste_copy(book, (sheet, rect), (ds, dr), m.name == "Cut")?;
                Ok(Variant::Bool(true))
            }
            ("Dirty", Func) => {
                self.book_mut(book)?.wb.dirty_range(sheet, rect);
                Ok(Variant::Empty)
            }

            ("FillDown" | "FillRight", Func)
                if (m.name == "FillDown" && rect.r1 == rect.r0)
                    || (m.name == "FillRight" && rect.c1 == rect.c0) =>
            {
                let src = if m.name == "FillDown" {
                    (rect.r0 > 0).then(|| Rect::new(rect.r0 - 1, rect.c0, rect.r0 - 1, rect.c1))
                } else {
                    (rect.c0 > 0).then(|| Rect::new(rect.r0, rect.c0 - 1, rect.r1, rect.c0 - 1))
                };
                let Some(src) = src else {
                    return Err(OmError::app_defined_with(&format!(
                        "{} method of Range class failed",
                        m.name
                    )));
                };
                self.book_mut(book)?
                    .wb
                    .command(xlcalc::Command::Fill, |wb| {
                        wb.note_changed(sheet, &[rect]);
                        wb.fill(sheet, src, rect)
                    })
                    .map_err(edit_error)?;
                Ok(Variant::Bool(true))
            }
            ("FillDown" | "FillRight", Func) => {
                let down = m.name == "FillDown";
                let (src, dst) = if down {
                    (
                        Rect::new(rect.r0, rect.c0, rect.r0, rect.c1),
                        Rect::new(rect.r0 + 1, rect.c0, rect.r1, rect.c1),
                    )
                } else {
                    (
                        Rect::new(rect.r0, rect.c0, rect.r1, rect.c0),
                        Rect::new(rect.r0, rect.c0 + 1, rect.r1, rect.c1),
                    )
                };
                if (down && rect.r1 > rect.r0) || (!down && rect.c1 > rect.c0) {
                    self.book_mut(book)?
                        .wb
                        .fill(sheet, src, dst)
                        .map_err(edit_error)?;
                }
                Ok(Variant::Bool(true))
            }
            ("Sort" | "AutoFill" | "DataSeries" | "PasteSpecial" | "Replace", Func) => {
                self.command_member(r, sheet, rect, m, a)
            }
            ("CountLarge", Get) => self.count_large(r),
            ("ListObject", Get) => self.range_list_object(r),
            ("Column", Get) => Ok(Variant::I4(rect.c0 as i32 + 1)),
            ("Row", Get) => Ok(Variant::I4(rect.r0 as i32 + 1)),
            ("Count", Get) => {
                let n = match r.mode {
                    Mode::Cells => rect.cell_count(),
                    Mode::Rows => u64::from(rect.rows()),
                    Mode::Columns => u64::from(rect.cols()),
                };
                i32::try_from(n)
                    .map(Variant::I4)
                    .map_err(|_| OmError::exception(HResult::DISP_E_OVERFLOW.0, None))
            }
            ("EntireRow", Get) => {
                let whole = Rect::new(rect.r0, 0, rect.r1, MAX_COLS - 1);
                self.sub_range(r, sheet, whole, Mode::Rows)
            }
            ("EntireColumn", Get) => {
                let whole = Rect::new(0, rect.c0, MAX_ROWS - 1, rect.c1);
                self.sub_range(r, sheet, whole, Mode::Columns)
            }
            ("Worksheet", Get) => Ok(self.alloc(Object::Worksheet(book, sheet))),
            ("Offset", Get) => {
                let dr = i64::from(a.i32(0)?.unwrap_or(0));
                let dc = i64::from(a.i32(1)?.unwrap_or(0));
                let moved = (|| {
                    let r0 = u32::try_from(i64::from(rect.r0) + dr).ok()?;
                    let r1 = u32::try_from(i64::from(rect.r1) + dr).ok()?;
                    let c0 = u32::try_from(i64::from(rect.c0) + dc).ok()?;
                    let c1 = u32::try_from(i64::from(rect.c1) + dc).ok()?;
                    (r1 < MAX_ROWS && c1 < MAX_COLS).then(|| Rect::new(r0, c0, r1, c1))
                })();
                let moved = moved.ok_or_else(OmError::app_defined)?;
                self.sub_range(r, sheet, moved, r.mode)
            }
            ("Resize", Get) => {
                let rows = a.i32(0)?.map_or(i64::from(rect.rows()), i64::from);
                let cols = a.i32(1)?.map_or(i64::from(rect.cols()), i64::from);
                let r1 = i64::from(rect.r0) + rows - 1;
                let c1 = i64::from(rect.c0) + cols - 1;
                if rows < 1 || cols < 1 || r1 >= i64::from(MAX_ROWS) || c1 >= i64::from(MAX_COLS) {
                    return Err(OmError::app_defined());
                }
                let resized = Rect::new(rect.r0, rect.c0, r1 as u32, c1 as u32);
                self.sub_range(r, sheet, resized, r.mode)
            }
            ("Item" | "_Default", Get) if a.get(0).is_some() || a.get(1).is_some() => {
                self.item(r, sheet, rect, a, m.name == "Item")
            }
            ("Item", Get) => Err(OmError::app_defined()),
            ("Value" | "_Default", Get) => {
                if m.name == "Value" {
                    match a.i32(0)? {
                        None | Some(10) => {}
                        Some(kind @ (11 | 12)) => {
                            let wb = &self.book(book)?.wb;
                            let text = if kind == 11 {
                                crate::xmlvalue::spreadsheet(wb, &|x| spill_of(wb, x), sheet, rect)
                            } else {
                                crate::xmlvalue::rowset(wb, sheet, rect)
                            };
                            return text
                                .map(Variant::Bstr)
                                .ok_or_else(|| unsupported("Range", m));
                        }
                        Some(_) => return Err(OmError::app_defined()),
                    }
                }
                if rect.is_cell() {
                    let wb = &self.book(book)?.wb;
                    let x = CellAddr::new(sheet, rect.r0, rect.c0);
                    if currency_overflow(&wb.value(x), wb.number_format(x))
                        || date_overflow(&wb.value(x), wb.number_format(x))
                    {
                        return Err(OmError::exception(HResult::DISP_E_OVERFLOW.0, None));
                    }
                }
                self.read_cells(book, sheet, rect, |wb, x| {
                    Application::value_variant(wb, x, Flavour::Value)
                })
            }
            ("Value2", Get) => self.read_cells(book, sheet, rect, |wb, x| {
                Application::value_variant(wb, x, Flavour::Value2)
            }),
            ("Formula" | "Formula2" | "FormulaLocal" | "Formula2Local", Get) => {
                let legacy = matches!(m.name, "Formula" | "FormulaLocal");
                self.read_cells(book, sheet, rect, |wb, x| {
                    Application::formula_variant(wb, x, false, legacy)
                })
            }
            ("FormulaR1C1", Get) => self.read_cells(book, sheet, rect, |wb, x| {
                Application::formula_variant(wb, x, true, false)
            }),
            ("HasArray", Get) => self.same_or_null(book, sheet, rect, |wb, x| {
                Variant::Bool(wb.current_array(x).is_some())
            }),
            ("CurrentArray", Get) => {
                let block = self
                    .book(book)?
                    .wb
                    .current_array(CellAddr::new(sheet, rect.r0, rect.c0))
                    .ok_or_else(OmError::app_defined)?;
                self.sub_range(r, sheet, block, Mode::Cells)
            }
            ("MergeCells", Get) => self.same_or_null(book, sheet, rect, |wb, x| {
                Variant::Bool(wb.merge_at(x).is_some())
            }),
            ("MergeArea", Get) => {
                let area = self
                    .book(book)?
                    .wb
                    .merge_at(CellAddr::new(sheet, rect.r0, rect.c0))
                    .unwrap_or(rect);
                self.sub_range(r, sheet, area, Mode::Cells)
            }
            ("HasSpill", Get) => self.same_or_null(book, sheet, rect, |wb, x| {
                Variant::Bool(spill_of(wb, x).is_some())
            }),
            ("SpillingToRange", Get) => {
                let block = spill_of(&self.book(book)?.wb, CellAddr::new(sheet, rect.r0, rect.c0))
                    .map(|(_, block)| block)
                    .ok_or_else(OmError::app_defined)?;
                self.sub_range(r, sheet, block, Mode::Cells)
            }
            ("SpillParent", Get) => {
                let anchor = spill_of(&self.book(book)?.wb, CellAddr::new(sheet, rect.r0, rect.c0))
                    .map(|(anchor, _)| anchor)
                    .ok_or_else(OmError::app_defined)?;
                self.sub_range(r, sheet, Rect::cell(anchor.row, anchor.col), Mode::Cells)
            }
            ("HasFormula", Get) => self.same_or_null(book, sheet, rect, |wb, x| {
                Variant::Bool(wb.formula_text(x).is_some())
            }),
            ("NumberFormat", Get) => self.same_or_null(book, sheet, rect, |wb, x| {
                let code = wb.number_format(x);
                if wb.cell_style(x).custom_code && host_spelled(wb, code) {
                    Variant::Bstr(code.to_string())
                } else {
                    Variant::Bstr(com_format(wb, code, a.lcid))
                }
            }),

            ("NumberFormatLocal", Get) => self.same_or_null(book, sheet, rect, |wb, x| {
                Variant::Bstr(com_format(wb, wb.number_format(x), 0))
            }),
            ("Text", Get) => {
                if rect.is_cell() {
                    let wb = &self.book(book)?.wb;
                    Ok(Variant::Bstr(cell_text_capped(
                        wb,
                        CellAddr::new(sheet, rect.r0, rect.c0),
                    )))
                } else {
                    Ok(Variant::Null)
                }
            }

            ("_NewEnum", Get) => {
                let parts: Vec<Rect> = match r.mode {
                    Mode::Cells => {
                        if rect.cell_count() > MAX_READ_CELLS {
                            return Err(OmError::exception(vba_scode(7), None));
                        }
                        Application::rect_cells(sheet, rect)
                            .map(|x| Rect::cell(x.row, x.col))
                            .collect()
                    }
                    Mode::Rows => (rect.r0..=rect.r1)
                        .map(|row| Rect::new(row, rect.c0, row, rect.c1))
                        .collect(),
                    Mode::Columns => (rect.c0..=rect.c1)
                        .map(|col| Rect::new(rect.r0, col, rect.r1, col))
                        .collect(),
                };
                let mut items = Vec::with_capacity(parts.len());
                for part in parts {
                    items.push(self.new_range(book, sheet, part, r.mode)?);
                }
                Ok(self.alloc_enumerator(items))
            }
            ("Value", Put) | ("_Default", Put) => {
                self.put_values(r, sheet, rect, a, Flavour::Value)
            }
            ("Value2", Put) => self.put_values(r, sheet, rect, a, Flavour::Value2),
            ("Formula" | "FormulaLocal", Put) => {
                let local = m.name == "FormulaLocal";
                self.put_formulas(book, sheet, rect, a, Entry::Legacy, local)
            }
            ("Formula2" | "Formula2Local", Put) => {
                let local = m.name == "Formula2Local";
                self.put_formulas(book, sheet, rect, a, Entry::Dynamic, local)
            }
            ("FormulaR1C1", Put) => self.put_formulas(book, sheet, rect, a, Entry::R1C1, false),
            ("NumberFormat", Put) => {
                let value = a.put.clone().unwrap_or(Variant::Empty);
                let (rows, cols, items) = array_cells(&value).ok_or_else(OmError::app_defined)?;
                let mut codes = Vec::with_capacity(items.len());
                for item in &items {
                    let one = Args {
                        values: Vec::new(),
                        put: Some(item.clone()),
                        lcid: a.lcid,
                    };

                    let fmt = self.put_string(&one)?.replace("\"$\"", "$");

                    if !fmt.contains('[') && xlcalc::NumberFormat::parse(&fmt).is_err() {
                        return Err(OmError::unable_to_set("NumberFormat", "Range"));
                    }
                    codes.push(fmt);
                }
                let wb = &mut self.book_mut(book)?.wb;
                let engine = |wb: &Workbook, code: &str| {
                    if us_names(a.lcid) {
                        engine_format(wb, code)
                    } else {
                        code.to_string()
                    }
                };
                if let Some(c) = codes.first() {
                    let first = engine(wb, c);
                    wb.note_number_format(Some(&first));
                }
                let host = |wb: &Workbook, code: &str| us_names(a.lcid) && host_spelled(wb, code);
                if codes.len() == 1 {
                    let fmt = engine(wb, &codes[0]);
                    wb.set_number_format(sheet, rect, &fmt);
                    let h = host(wb, &codes[0]);
                    wb.mark_custom_code(sheet, rect, &fmt, h);
                } else {
                    for (i, row) in (rect.r0..=rect.r1).enumerate() {
                        for (j, col) in (rect.c0..=rect.c1).enumerate() {
                            let k = (i % rows as usize) * cols as usize + j % cols as usize;
                            let fmt = engine(wb, &codes[k]);
                            wb.set_number_format(sheet, Rect::cell(row, col), &fmt);
                            let h = host(wb, &codes[k]);
                            wb.mark_custom_code(sheet, Rect::cell(row, col), &fmt, h);
                        }
                    }
                }
                wb.widen_for_format(sheet, rect);
                Ok(Variant::Empty)
            }
            ("FormulaArray", Put) => {
                let text = self.put_string(a)?;
                let wb = &mut self.book_mut(book)?.wb;
                let locale = input_locale(a.lcid, wb.locale());
                wb.with_input_locale(&locale, |wb| wb.set_array_formula(sheet, rect, &text))
                    .map_err(edit_error)?;
                Ok(Variant::Empty)
            }
            ("ClearFormats", Func) | ("Clear", Func) => {
                let wb = &mut self.book_mut(book)?.wb;
                if m.name == "Clear" {
                    wb.clear_range(sheet, rect).map_err(edit_error)?;

                    wb.reset_number_format(sheet, rect);
                } else {
                    wb.clear_formats(sheet, rect);
                }
                Ok(Variant::Bool(true))
            }
            ("MergeCells", Put) => self.range_view_member(r, sheet, rect, m, a),
            (
                "Hidden" | "OutlineLevel" | "AutoFilter" | "Merge" | "UnMerge" | "ColumnWidth"
                | "Width" | "AutoFit" | "RowHeight" | "Height",
                _,
            ) => self.range_view_member(r, sheet, rect, m, a),
            _ => match self.range_style_member(r, sheet, rect, m, a) {
                Some(done) => done,
                None => self.data_member(book, (sheet, rect), m, a),
            },
        }
    }

    fn address_text(
        &self,
        book: BookId,
        parts: &[(SheetId, Rect)],
        a: &Args,
    ) -> Result<Variant, OmError> {
        let flag = |i| a.bool(i).ok().flatten().unwrap_or(true);
        let r1c1 = a.i32(2)?.unwrap_or(1) != 1;
        let external = a.bool(3)?.unwrap_or(false);
        let host = match a.get(4) {
            Some(Variant::Dispatch(Some(id))) => match self.arena.get(*id) {
                Some(Object::Range(h)) => {
                    let (_, hr) = self.resolve(h)?;
                    (hr.r0, hr.c0)
                }
                _ => (0, 0),
            },
            _ => (0, 0),
        };
        let style = AddressStyle {
            row_abs: flag(0),
            col_abs: flag(1),
            r1c1,
            relative_to: host,
        };
        let mut text = parts
            .iter()
            .map(|&(_, rect)| range_text(rect, &style))
            .collect::<Vec<_>>()
            .join(",");
        if external {
            let sheet = parts
                .first()
                .map(|p| p.0)
                .ok_or_else(OmError::app_defined)?;
            let b = self.book(book)?;
            let name =
                b.wb.sheet(sheet)
                    .map(|s| s.name.clone())
                    .unwrap_or_default();
            text = format!("{}{text}", external_prefix(&b.name, &name));
        }
        Ok(Variant::Bstr(text))
    }

    pub(crate) fn areas_member(
        &mut self,
        areas: &[RangeRef],
        m: &'static Member,
        a: &Args,
    ) -> Result<Variant, OmError> {
        match m.name {
            "Count" => Ok(Variant::I4(areas.len() as i32)),
            "Item" | "_Default" => {
                let n = a
                    .i32(0)
                    .map_err(|_| OmError::arg(DISP_E_TYPEMISMATCH, 0))?
                    .ok_or_else(OmError::app_defined)?;
                let area = usize::try_from(n - 1)
                    .ok()
                    .and_then(|i| areas.get(i))
                    .ok_or_else(OmError::app_defined)?;
                let (sheet, rect) = self.resolve(area)?;
                self.new_range(area.book, sheet, rect, Mode::Cells)
            }
            "_NewEnum" => {
                let mut items = Vec::with_capacity(areas.len());
                for area in areas {
                    let (sheet, rect) = self.resolve(area)?;
                    items.push(self.new_range(area.book, sheet, rect, Mode::Cells)?);
                }
                Ok(self.alloc_enumerator(items))
            }
            _ => Err(unsupported("Areas", m)),
        }
    }

    pub(crate) fn multi_member(
        &mut self,
        areas: &[RangeRef],
        m: &'static Member,
        a: &Args,
    ) -> Result<Variant, OmError> {
        use InvokeKind::*;

        let live: Vec<(RangeRef, (SheetId, Rect))> = areas
            .iter()
            .filter_map(|r| self.resolve(r).ok().map(|at| (*r, at)))
            .collect();
        let areas: Vec<RangeRef> = live.iter().map(|l| l.0).collect();
        let parts: Vec<(SheetId, Rect)> = live.iter().map(|l| l.1).collect();
        let first = *areas.first().ok_or_else(OmError::object_required)?;
        let (book, mode) = (first.book, first.mode);
        let sheet = parts[0].0;
        let rects: Vec<Rect> = parts.iter().map(|p| p.1).collect();
        let indexed = a.get(0).is_some() || a.get(1).is_some();
        match (m.name, m.kind) {
            ("Select" | "Activate", Func) => self.range_select(book, sheet, &rects, m),
            ("Address", Get) => self.address_text(book, &parts, a),
            ("Areas", Get) => {
                let tracked = self.track_areas(book, sheet, &rects, Mode::Cells)?;
                Ok(self.alloc(Object::Areas(tracked)))
            }
            ("Count", Get) => {
                let n = match mode {
                    Mode::Cells => rects.iter().map(Rect::cell_count).sum(),
                    Mode::Rows => u64::from(rects[0].rows()),
                    Mode::Columns => u64::from(rects[0].cols()),
                };
                i32::try_from(n)
                    .map(Variant::I4)
                    .map_err(|_| OmError::exception(HResult::DISP_E_OVERFLOW.0, None))
            }

            ("Cells" | "Rows" | "Columns" | "Item" | "_Default", Get) if indexed => {
                self.range_member(&first, m, a)
            }
            ("Cells" | "Rows" | "Columns", Get) => {
                let to = match m.name {
                    "Cells" => Mode::Cells,
                    "Rows" => Mode::Rows,
                    _ => Mode::Columns,
                };
                self.new_multi(book, sheet, &rects, to)
            }
            ("EntireRow" | "EntireColumn", Get) => {
                let rows = m.name == "EntireRow";
                let whole: Vec<Rect> = rects
                    .iter()
                    .map(|r| {
                        if rows {
                            Rect::new(r.r0, 0, r.r1, MAX_COLS - 1)
                        } else {
                            Rect::new(0, r.c0, MAX_ROWS - 1, r.c1)
                        }
                    })
                    .collect();
                let to = if rows { Mode::Rows } else { Mode::Columns };
                self.new_multi(book, sheet, &whole, to)
            }
            ("Offset", Get) => {
                let dr = i64::from(a.i32(0)?.unwrap_or(0));
                let dc = i64::from(a.i32(1)?.unwrap_or(0));
                let moved: Option<Vec<Rect>> = rects
                    .iter()
                    .map(|rect| {
                        let r0 = u32::try_from(i64::from(rect.r0) + dr).ok()?;
                        let r1 = u32::try_from(i64::from(rect.r1) + dr).ok()?;
                        let c0 = u32::try_from(i64::from(rect.c0) + dc).ok()?;
                        let c1 = u32::try_from(i64::from(rect.c1) + dc).ok()?;
                        (r1 < MAX_ROWS && c1 < MAX_COLS).then(|| Rect::new(r0, c0, r1, c1))
                    })
                    .collect();
                let moved = moved.ok_or_else(OmError::app_defined)?;
                self.new_multi(book, sheet, &moved, mode)
            }
            ("Resize" | "Item", Get) => Err(OmError::app_defined()),
            (
                "Value" | "_Default" | "Value2" | "Formula" | "Formula2" | "FormulaLocal"
                | "Formula2Local" | "FormulaR1C1" | "Row" | "Column" | "Worksheet" | "Calculate",
                Get | Func,
            ) => self.range_member(&first, m, a),
            ("HasArray", Get) => self.same_or_null_in(book, &parts, |wb, x| {
                Variant::Bool(wb.current_array(x).is_some())
            }),
            ("MergeCells", Get) => self.same_or_null_in(book, &parts, |wb, x| {
                Variant::Bool(wb.merge_at(x).is_some())
            }),
            ("HasFormula", Get) => self.same_or_null_in(book, &parts, |wb, x| {
                Variant::Bool(wb.formula_text(x).is_some())
            }),
            ("NumberFormat", Get) => self.same_or_null_in(book, &parts, |wb, x| {
                Variant::Bstr(com_format(wb, wb.number_format(x), a.lcid))
            }),
            ("Text", Get) => {
                self.same_or_null_in(book, &parts, |wb, x| Variant::Bstr(cell_text_capped(wb, x)))
            }
            ("_NewEnum", Get) => {
                let cells: u64 = rects.iter().map(Rect::cell_count).sum();
                if mode == Mode::Cells && cells > MAX_READ_CELLS {
                    return Err(OmError::exception(vba_scode(7), None));
                }
                let mut items = Vec::new();
                for &(sheet, rect) in &parts {
                    let pieces: Vec<Rect> = match mode {
                        Mode::Cells => Application::rect_cells(sheet, rect)
                            .map(|x| Rect::cell(x.row, x.col))
                            .collect(),
                        Mode::Rows => (rect.r0..=rect.r1)
                            .map(|row| Rect::new(row, rect.c0, row, rect.c1))
                            .collect(),
                        Mode::Columns => (rect.c0..=rect.c1)
                            .map(|col| Rect::new(rect.r0, col, rect.r1, col))
                            .collect(),
                    };
                    for piece in pieces {
                        items.push(self.new_range(book, sheet, piece, mode)?);
                    }
                }
                Ok(self.alloc_enumerator(items))
            }
            ("Formula" | "FormulaLocal", Put) => {
                let local = m.name == "FormulaLocal";
                self.put_formulas_in(book, &parts, a, Entry::Legacy, local)
            }
            ("Formula2" | "Formula2Local", Put) => {
                let local = m.name == "Formula2Local";
                self.put_formulas_in(book, &parts, a, Entry::Dynamic, local)
            }
            ("FormulaR1C1", Put) => self.put_formulas_in(book, &parts, a, Entry::R1C1, false),

            ("Value" | "Value2" | "_Default" | "NumberFormat", Put)
            | ("ClearContents" | "ClearFormats" | "Clear", Func) => {
                let mut last = Variant::Empty;
                for area in &areas {
                    last = self.range_member(area, m, a)?;
                }
                Ok(last)
            }
            _ => Err(unsupported("Range", m)),
        }
    }

    pub(crate) fn range_areas(
        &self,
        v: Option<&Variant>,
    ) -> Result<(BookId, SheetId, Vec<Rect>), OmError> {
        let Some(Variant::Dispatch(Some(id))) = v else {
            return Err(OmError::app_defined());
        };
        let refs: Vec<RangeRef> = match self.arena.get(*id) {
            Some(Object::Range(r)) => vec![*r],
            Some(Object::Multi(rs)) => rs.clone(),
            _ => return Err(OmError::app_defined()),
        };
        let mut sheet = None;
        let mut rects = Vec::new();
        for r in &refs {
            let (s, rect) = self.resolve(r)?;
            if *sheet.get_or_insert(s) != s {
                return Err(OmError::app_defined());
            }
            rects.push(rect);
        }
        let book = refs
            .first()
            .map(|r| r.book)
            .ok_or_else(OmError::app_defined)?;
        Ok((book, sheet.ok_or_else(OmError::app_defined)?, rects))
    }

    pub(crate) fn union_intersect(
        &mut self,
        m: &'static Member,
        a: &Args,
    ) -> Result<Variant, OmError> {
        let union = m.name == "Union";
        let failed =
            || OmError::app_defined_with(&format!("{} method of Application class failed", m.name));
        let mut args = Vec::new();
        for v in a.values.iter().flatten() {
            let (book, sheet, rects) = self.range_areas(Some(v)).map_err(|_| failed())?;
            args.push((book, sheet, rects));
        }
        let Some((book, sheet, _)) = args.first().map(|(b, s, r)| (*b, *s, r.len())) else {
            return Err(OmError::app_defined());
        };
        if args.iter().any(|(b, s, _)| *b != book || *s != sheet) {
            return Err(failed());
        }
        let mut args = args.into_iter().map(|(_, _, rects)| rects);

        let mut acc: Vec<Rect> = args
            .next()
            .unwrap_or_default()
            .into_iter()
            .fold(Vec::new(), union_add);
        if union {
            for rects in args {
                acc = rects.into_iter().fold(acc, union_add);
            }
        } else {
            for rects in args {
                let mut next = Vec::new();
                for x in &acc {
                    for y in &rects {
                        if let Some(common) = x.intersection(y) {
                            next = union_add(next, common);
                        }
                    }
                }
                acc = next;
            }
        }
        if acc.is_empty() {
            return Ok(Variant::Dispatch(None));
        }
        self.new_multi(book, sheet, &acc, Mode::Cells)
    }

    fn put_values(
        &mut self,
        r: &RangeRef,
        sheet: SheetId,
        rect: Rect,
        a: &Args,
        flavour: Flavour,
    ) -> Result<Variant, OmError> {
        let value = a.put.clone().unwrap_or(Variant::Empty);
        let value = self.range_object_value(value)?;
        let (rows, cols, items) = array_cells(&value).ok_or_else(OmError::app_defined)?;
        let scalar = !matches!(value, Variant::Array(_));

        let target = rect;
        let lcid = a.lcid;
        let wb = &mut self.book_mut(r.book)?.wb;
        for (i, row) in (target.r0..=target.r1).enumerate() {
            for (j, col) in (target.c0..=target.c1).enumerate() {
                let (ai, aj) = (if rows == 1 { 0 } else { i }, if cols == 1 { 0 } else { j });
                let item = if scalar {
                    Some(&items[0])
                } else {
                    items
                        .get(ai * cols as usize + aj)
                        .filter(|_| (ai as u32) < rows && (aj as u32) < cols)
                };
                let addr = CellAddr::new(sheet, row, col);
                match item {
                    Some(v) => put_cell(wb, addr, v, flavour, lcid)?,
                    None => {
                        wb.set_value(addr, Value::Error(XlError::NA));
                    }
                }
            }
        }
        Ok(Variant::Empty)
    }

    fn range_object_value(&mut self, value: Variant) -> Result<Variant, OmError> {
        let Variant::Dispatch(Some(id)) = &value else {
            return Ok(value);
        };
        let Some(Object::Range(src)) = self.arena.get(*id) else {
            return Err(OmError::app_defined());
        };
        let src = *src;
        let (ss, sr) = self.resolve(&src)?;
        if sr.is_cell() {
            self.read_cells(src.book, ss, sr, |wb, x| {
                Application::value_variant(wb, x, Flavour::Value2)
            })
        } else {
            Ok(Variant::Empty)
        }
    }

    fn put_formulas(
        &mut self,
        book: BookId,
        sheet: SheetId,
        rect: Rect,
        a: &Args,
        entry: Entry,
        local: bool,
    ) -> Result<Variant, OmError> {
        self.put_formulas_in(book, &[(sheet, rect)], a, entry, local)
    }

    fn put_formulas_in(
        &mut self,
        book: BookId,
        parts: &[(SheetId, Rect)],
        a: &Args,
        entry: Entry,
        local: bool,
    ) -> Result<Variant, OmError> {
        let value = self.range_object_value(a.put.clone().unwrap_or(Variant::Empty))?;
        let (rows, cols, items) = array_cells(&value).ok_or_else(OmError::app_defined)?;
        let scalar = !matches!(value, Variant::Array(_));
        let lcid = if local { HOST_LCID } else { a.lcid };
        let wb = &mut self.book_mut(book)?.wb;
        let &(sheet, rect) = parts.first().ok_or_else(OmError::app_defined)?;
        if scalar {
            let first = CellAddr::new(sheet, rect.r0, rect.c0);
            put_formula_cell(wb, first, &items[0], entry, lcid)?;
            let src = Rect::cell(rect.r0, rect.c0);
            let is_text = matches!(items[0], Variant::Bstr(_));
            for &(sheet, rect) in parts {
                for addr in Application::rect_cells(sheet, rect) {
                    if addr == first {
                        continue;
                    }
                    if is_text {
                        wb.copy_range(sheet, src, addr).map_err(edit_error)?;
                    } else {
                        put_formula_cell(wb, addr, &items[0], entry, lcid)?;
                    }
                }
            }
        } else {
            for &(sheet, rect) in parts {
                for (i, row) in (rect.r0..rect.r0 + rows).enumerate() {
                    for (j, col) in (rect.c0..rect.c0 + cols).enumerate() {
                        if row > rect.r1 || col > rect.c1 {
                            continue;
                        }
                        let addr = CellAddr::new(sheet, row, col);
                        put_formula_cell(wb, addr, &items[i * cols as usize + j], entry, lcid)?;
                    }
                }
            }
        }
        Ok(Variant::Empty)
    }
}

#[derive(Clone, Copy, PartialEq, Eq)]
enum Entry {
    Legacy,

    Dynamic,

    R1C1,
}

fn enter_text(
    wb: &mut Workbook,
    addr: CellAddr,
    text: &str,
    legacy: bool,
    lcid: u32,
) -> Result<(), OmError> {
    let locale = input_locale(lcid, wb.locale());
    let r = wb.with_input_locale(&locale, |wb| {
        let text = &*wb.typed_text(addr, text).into_owned();
        if !text.starts_with('=') && xlcalc::testing::is_formula_entry(wb, text) {
            wb.set_value(addr, Value::from(text));
            return Ok(());
        }
        if legacy {
            wb.set_legacy_input(addr, text).map(|_| ())
        } else {
            wb.set_input(addr, text).map(|_| ())
        }
    });
    match r {
        Ok(()) => Ok(()),
        Err(EditError::Locked) => Ok(()),
        Err(e @ EditError::OutOfScope(_)) => Err(edit_error(e)),
        Err(e) => Err(OmError::app_defined().with_detail(e.to_string())),
    }
}

fn put_formula_cell(
    wb: &mut Workbook,
    addr: CellAddr,
    v: &Variant,
    entry: Entry,
    lcid: u32,
) -> Result<(), OmError> {
    match v {
        Variant::Bstr(t) => {
            let text = if entry == Entry::R1C1 && t.starts_with('=') {
                r1c1_to_a1(t, addr.row, addr.col)
            } else {
                t.clone()
            };
            enter_text(wb, addr, &text, entry == Entry::Legacy, lcid)
        }
        other => put_cell(wb, addr, other, Flavour::Value2, lcid),
    }
}

fn spill_of(wb: &Workbook, a: CellAddr) -> Option<(CellAddr, Rect)> {
    let anchor = wb.spill_anchor(a)?;
    let f = wb.formula(wb.formula_at(anchor)?);
    if f.is_legacy_array() {
        return None;
    }
    f.spill.map(|block| (anchor, block))
}

fn us_names(lcid: u32) -> bool {
    lcid == xlcalc_variant::Locale::EN_US.0
}

fn date_format(d: f64) -> &'static str {
    if d.abs() < 1.0 {
        "h:mm:ss AM/PM"
    } else if d.fract() == 0.0 {
        "m/d/yyyy"
    } else {
        "m/d/yyyy h:mm"
    }
}

fn builtin_pairs(wb: &Workbook) -> [(String, String); 12] {
    let c = wb.locale().currency.clone();
    let us = |s: &str| s.to_string();
    [
        (format!("{c}#,##0;-{c}#,##0"), us("$#,##0_);($#,##0)")),
        (
            format!("{c}#,##0;[Red]-{c}#,##0"),
            us("$#,##0_);[Red]($#,##0)"),
        ),
        (
            format!("{c}#,##0.00;-{c}#,##0.00"),
            us("$#,##0.00_);($#,##0.00)"),
        ),
        (
            format!("{c}#,##0.00;[Red]-{c}#,##0.00"),
            us("$#,##0.00_);[Red]($#,##0.00)"),
        ),
        (us("#,##0;-#,##0"), us("#,##0_);(#,##0)")),
        (us("#,##0;[Red]-#,##0"), us("#,##0_);[Red](#,##0)")),
        (us("#,##0.00;-#,##0.00"), us("#,##0.00_);(#,##0.00)")),
        (
            us("#,##0.00;[Red]-#,##0.00"),
            us("#,##0.00_);[Red](#,##0.00)"),
        ),
        (
            us(r#"_-* #,##0_-;-* #,##0_-;_-* "-"_-;_-@_-"#),
            us(r#"_(* #,##0_);_(* (#,##0);_(* "-"_);_(@_)"#),
        ),
        (
            format!(r#"_-{c}* #,##0_-;-{c}* #,##0_-;_-{c}* "-"_-;_-@_-"#),
            us(r#"_($* #,##0_);_($* (#,##0);_($* "-"_);_(@_)"#),
        ),
        (
            us(r#"_-* #,##0.00_-;-* #,##0.00_-;_-* "-"??_-;_-@_-"#),
            us(r#"_(* #,##0.00_);_(* (#,##0.00);_(* "-"??_);_(@_)"#),
        ),
        (
            format!(r#"_-{c}* #,##0.00_-;-{c}* #,##0.00_-;_-{c}* "-"??_-;_-@_-"#),
            us(r#"_($* #,##0.00_);_($* (#,##0.00);_($* "-"??_);_(@_)"#),
        ),
    ]
}

fn host_spelled(wb: &Workbook, code: &str) -> bool {
    let code = plain_escapes(code);
    builtin_pairs(wb)
        .iter()
        .any(|(host, us)| host.eq_ignore_ascii_case(&code) && !us.eq_ignore_ascii_case(&code))
}

fn engine_format(wb: &Workbook, code: &str) -> String {
    let short = &wb.locale().short_date_format;
    if code.eq_ignore_ascii_case("m/d/yyyy") {
        short.clone()
    } else if code.eq_ignore_ascii_case("m/d/yyyy h:mm") {
        format!("{short} h:mm")
    } else if let Some((host, _)) = builtin_pairs(wb)
        .into_iter()
        .find(|(_, us)| us.eq_ignore_ascii_case(code))
    {
        host
    } else {
        plain_escapes(code)
    }
}

fn plain_escapes(code: &str) -> String {
    const KEEP: &str = "\"#$%*,./0:;?@ABDEGHMNRSY[\\]_abdeghmnrsy";
    let mut out = String::with_capacity(code.len());
    let (mut quoted, mut bracket) = (false, false);
    let mut chars = code.chars();
    while let Some(c) = chars.next() {
        match c {
            '"' if !bracket => quoted = !quoted,
            '[' if !quoted => bracket = true,
            ']' if !quoted => bracket = false,
            '\\' if !quoted && !bracket => {
                if let Some(n) = chars.next() {
                    if n.is_ascii() && !KEEP.contains(n) {
                        out.push(n);
                    } else {
                        out.push('\\');
                        out.push(n);
                    }
                    continue;
                }
            }
            _ => {}
        }
        out.push(c);
    }
    out
}

fn com_format(wb: &Workbook, code: &str, lcid: u32) -> String {
    let short = &wb.locale().short_date_format;
    if !us_names(lcid) {
        host_ampm(code)
    } else if code == short {
        "m/d/yyyy".into()
    } else if code.strip_suffix(" h:mm") == Some(short) {
        "m/d/yyyy h:mm".into()
    } else if let Some((_, us)) = builtin_pairs(wb)
        .into_iter()
        .find(|(host, _)| host.eq_ignore_ascii_case(code))
    {
        us
    } else {
        code.to_string()
    }
}

fn host_ampm(code: &str) -> String {
    let mut out = String::with_capacity(code.len());
    let (mut quoted, mut escaped) = (false, false);
    let mut i = 0;
    while let Some(c) = code[i..].chars().next() {
        if escaped {
            escaped = false;
        } else if c == '"' {
            quoted = !quoted;
        } else if !quoted && c == '\\' {
            escaped = true;
        } else if !quoted
            && code[i..]
                .get(..5)
                .is_some_and(|t| t.eq_ignore_ascii_case("am/pm"))
        {
            out.push_str("am/pm");
            i += 5;
            continue;
        }
        out.push(c);
        i += c.len_utf8();
    }
    out
}

fn format_replaced(code: &str, date_day: bool, currency: bool) -> bool {
    use crate::conv::{has_date_part, is_time_only};
    code.eq_ignore_ascii_case("general")
        || code.contains('%')
        || (currency && has_date_part(code))
        || (date_day && is_time_only(code))
}

fn put_cell(
    wb: &mut Workbook,
    addr: CellAddr,
    v: &Variant,
    flavour: Flavour,
    lcid: u32,
) -> Result<(), OmError> {
    let d1904 = wb.settings().date_system == DateSystem::D1904;
    let number = |wb: &mut Workbook, n: f64| {
        let n = if n != 0.0 && n.abs() < f64::MIN_POSITIVE {
            0.0
        } else {
            n
        };
        wb.set_value(addr, Value::Number(n));
    };
    match v {
        Variant::Empty | Variant::Null => {
            wb.clear(addr);
        }
        Variant::Bool(b) => {
            wb.set_value(addr, Value::Bool(*b));
        }

        Variant::Bstr(t) if t.len() > 1 && t.starts_with('\'') => {
            wb.set_text_prefixed(addr, &t[1..])
                .map_err(|e| OmError::app_defined().with_detail(e.to_string()))?;
        }
        Variant::Bstr(t) => enter_text(wb, addr, t, false, lcid)?,
        Variant::Date(d) => {
            let serial = if d1904 { d - 1462.0 } else { *d };
            if flavour == Flavour::Value && wb.number_format(addr) == "@" {
                let code = if serial.abs() < 1.0 {
                    "h:mm:ss AM/PM"
                } else if serial.fract() == 0.0 {
                    "m/d/yyyy"
                } else {
                    "m/d/yyyy  h:mm:ss AM/PM"
                };
                let text = render_us(code, serial, wb.settings().date_system);
                wb.set_value(addr, Value::Text(text.into()));
                return Ok(());
            }
            number(wb, serial);
            if flavour == Flavour::Value
                && format_replaced(wb.number_format(addr), d.abs() >= 1.0, false)
            {
                let fmt = engine_format(wb, date_format(*d));
                wb.set_number_format(addr.sheet, Rect::cell(addr.row, addr.col), &fmt);
            }
        }
        Variant::Cy(c) if flavour == Flavour::Value => {
            let rounded = xlcalc::round_worksheet(*c as f64 / 10_000.0, 2);
            if wb.number_format(addr) == "@" {
                let text = render_us(CURRENCY_FORMAT, rounded, wb.settings().date_system);
                wb.set_value(addr, Value::Text(text.into()));
                wb.set_number_format(addr.sheet, Rect::cell(addr.row, addr.col), CURRENCY_FORMAT);
                return Ok(());
            }
            number(wb, rounded);
            if format_replaced(wb.number_format(addr), false, true) {
                wb.set_number_format(addr.sheet, Rect::cell(addr.row, addr.col), CURRENCY_FORMAT);
            }
        }
        Variant::Cy(c) => number(wb, *c as f64 / 10_000.0),
        Variant::Error(code) => match scode_error(*code) {
            Some(e) => {
                wb.set_value(addr, Value::Error(e));
            }
            None => return Err(OmError::app_defined()),
        },
        Variant::Dispatch(_) | Variant::Unknown(_) | Variant::Array(_) => {
            return Err(OmError::app_defined())
        }
        other => match change_type(other, VarType::R8, Locale::EN_US, 0) {
            Ok(Variant::R8(n)) => number(wb, n),
            _ => return Err(OmError::type_mismatch()),
        },
    }
    Ok(())
}

fn grid_rect(r0: i64, c0: i64, r1: i64, c1: i64) -> Option<Rect> {
    let rows = 0..i64::from(MAX_ROWS);
    let cols = 0..i64::from(MAX_COLS);
    (rows.contains(&r0) && rows.contains(&r1) && cols.contains(&c0) && cols.contains(&c1))
        .then(|| Rect::new(r0 as u32, c0 as u32, r1 as u32, c1 as u32))
}

const CURRENCY_FORMAT: &str = "$#,##0.00_);($#,##0.00)";

fn render_us(code: &str, x: f64, ds: DateSystem) -> String {
    xlcalc::NumberFormat::parse(code)
        .and_then(|f| f.format_number(x, &xlcalc::Locale::en_us(), ds))
        .map_or_else(
            |_| x.to_string(),
            |r| match r
                .text
                .strip_suffix("am")
                .or_else(|| r.text.strip_suffix("pm"))
            {
                Some(head) => format!("{head}{}", r.text[head.len()..].to_uppercase()),
                None => r.text,
            },
        )
}

fn cell_text_capped(wb: &xlcalc::Workbook, at: CellAddr) -> String {
    let mut s = wb.shown_text(at);
    if let Some((i, _)) = s.char_indices().nth(8221) {
        s.truncate(i);
    }
    s
}

#[cfg(test)]
mod tests {
    use super::*;

    fn joined(areas: &[Rect]) -> Vec<Rect> {
        areas.iter().copied().fold(Vec::new(), union_add)
    }

    #[test]
    fn union_joins_touching_areas_and_moves_the_joined_one_last() {
        let c = Rect::cell;
        assert_eq!(
            joined(&[c(0, 2), c(0, 0), c(4, 1), c(0, 3)]),
            [c(0, 0), c(4, 1), Rect::new(0, 2, 0, 3)]
        );
        assert_eq!(
            joined(&[c(0, 0), c(0, 2), c(0, 1)]),
            [Rect::new(0, 0, 0, 2)]
        );

        let (a, b) = (Rect::new(0, 0, 1, 1), Rect::new(1, 1, 2, 2));
        assert_eq!(joined(&[a, b]), [a, b]);

        assert_eq!(joined(&[a, c(0, 0)]), [a]);
    }
}
