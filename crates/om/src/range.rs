use xlcalc::{CellAddr, DateSystem, EditError, Notice, Rect, SheetId, Value, Workbook, XlError};
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

enum R1c1Tok {
    Cell(String, String),
    Row(String),
    Col(String),
}

pub(crate) fn r1c1_to_a1(text: &str, row: u32, col: u32) -> Option<String> {
    let b: Vec<char> = text.chars().collect();
    let mut out = String::new();
    let mut i = 0;
    let ident = |c: char| c.is_alphanumeric() || c == '_' || c == '.' || c == '\\' || c == '$';

    let mut cell_before_colon: Option<(String, String)> = None;

    let mut pair_open = false;
    while i < b.len() {
        let c = b[i];
        if c == '"' || c == '\'' || c == '[' {
            let start = i;
            i += 1;
            let mut depth = 1;
            while i < b.len() {
                if c == '[' {
                    if b[i] == '\'' {
                        i += 2;
                        continue;
                    }
                    if b[i] == '[' {
                        depth += 1;
                    } else if b[i] == ']' {
                        depth -= 1;
                        if depth == 0 {
                            break;
                        }
                    }
                    i += 1;
                    continue;
                }
                if b[i] == c {
                    if b.get(i + 1) == Some(&c) {
                        i += 2;
                        continue;
                    }
                    break;
                }
                i += 1;
            }
            let end = (i + 1).min(b.len());
            out.extend(&b[start..end]);
            i = end;
            cell_before_colon = None;
            continue;
        }
        let prev_ident = i > 0 && ident(b[i - 1]);
        if prev_ident || !ident(c) {
            if !ident(c) && c != ':' {
                cell_before_colon = None;
                pair_open = false;
            }
            out.push(c);
            i += 1;
            continue;
        }

        let mut j = i;
        while j < b.len() && ident(b[j]) {
            j += 1;
        }
        if matches!(c, 'R' | 'r' | 'C' | 'c') {
            if let Some((tok, used)) = parse_r1c1_ref(&b[i..], row, col).ok()? {
                let after = b.get(i + used).copied();
                if !after.is_some_and(|a| ident(a) || a == '(') {
                    let colon_after = after == Some(':')
                        && b.get(i + used + 1).is_some_and(|n| !n.is_whitespace());
                    let colon_before = out.ends_with(':');
                    let blank_before = out.ends_with(char::is_whitespace);

                    let prev = cell_before_colon.take();
                    let open = std::mem::take(&mut pair_open);
                    let mut line = |l: String| {
                        if blank_before && colon_after {
                            format!("{l}:{l}")
                        } else if colon_before && open {
                            l
                        } else if colon_after {
                            pair_open = true;
                            l
                        } else {
                            format!("{l}:{l}")
                        }
                    };
                    match tok {
                        R1c1Tok::Cell(c, r) => {
                            out.push_str(&format!("{c}{r}"));
                            cell_before_colon = colon_after.then_some((c, r));
                        }
                        R1c1Tok::Col(l) => match prev {
                            Some((_, r)) if colon_before => out.push_str(&format!("{l}{r}")),
                            _ => out.push_str(&line(l)),
                        },
                        R1c1Tok::Row(n) => match prev {
                            Some((c, _)) if colon_before => out.push_str(&format!("{c}{n}")),
                            _ => out.push_str(&line(n)),
                        },
                    }
                    i += used;
                    continue;
                }
            }
        }
        cell_before_colon = None;
        pair_open = false;
        let word: String = b[i..j].iter().collect();
        if word.contains('$') {
            return None;
        }
        if xlcalc::parser::r1c1::forced_name(&word) && b.get(j) != Some(&'(') {
            out.push('\u{1}');
        }
        out.push_str(&word);
        i = j;
    }
    Some(out)
}

fn parse_r1c1_ref(s: &[char], row: u32, col: u32) -> Result<Option<(R1c1Tok, usize)>, ()> {
    #[derive(Clone, Copy)]
    enum Part {
        Abs(i64),
        Rel(i64),
    }

    fn axis(s: &[char], pos: &mut usize, letter: char) -> Option<Option<Part>> {
        if s.get(*pos).map(|c| c.to_ascii_uppercase()) != Some(letter) {
            return None;
        }
        *pos += 1;
        match s.get(*pos) {
            Some('[') => {
                let end = s[*pos..].iter().position(|&c| c == ']')? + *pos;
                let body: String = s[*pos + 1..end].iter().collect();
                let n: i64 = if body.is_empty() {
                    0
                } else {
                    body.parse().ok()?
                };
                *pos = end + 1;
                Some(Some(Part::Rel(n)))
            }
            Some(d) if d.is_ascii_digit() => {
                let start = *pos;
                while s.get(*pos).is_some_and(char::is_ascii_digit) {
                    *pos += 1;
                }
                let n: i64 = s[start..*pos].iter().collect::<String>().parse().ok()?;
                Some(Some(Part::Abs(n)))
            }
            _ => Some(None),
        }
    }

    fn resolve(p: Option<Part>, host: u32, max: u32) -> Result<Option<(u32, bool)>, ()> {
        match p {
            None => Ok(Some((host, false))),
            Some(Part::Abs(0)) => Ok(None),

            Some(Part::Abs(n)) if n > i64::from(max) && max == MAX_COLS => Err(()),
            Some(Part::Abs(n)) if n > i64::from(max) => Ok(None),
            Some(Part::Abs(n)) => Ok(Some((n as u32 - 1, true))),
            Some(Part::Rel(d)) if d.unsigned_abs() >= u64::from(max) => Err(()),
            Some(Part::Rel(d)) => Ok(Some((
                (i64::from(host) + d).rem_euclid(i64::from(max)) as u32,
                false,
            ))),
        }
    }
    let mut pos = 0;
    let r = axis(s, &mut pos, 'R');
    let c = if r.is_some() {
        let mut p2 = pos;
        let c = axis(s, &mut p2, 'C');
        if c.is_some() {
            pos = p2;
        }
        c
    } else {
        match axis(s, &mut pos, 'C') {
            Some(c) => Some(c),
            None => return Ok(None),
        }
    };
    let row_txt = |p: Option<Part>| -> Result<Option<String>, ()> {
        Ok(resolve(p, row, MAX_ROWS)?
            .map(|(n, abs)| format!("{}{}", if abs { "$" } else { "" }, n + 1)))
    };
    let col_txt = |p: Option<Part>| -> Result<Option<String>, ()> {
        Ok(resolve(p, col, MAX_COLS)?.map(|(n, abs)| {
            format!(
                "{}{}",
                if abs { "$" } else { "" },
                xlcalc::col_to_letters(n)
            )
        }))
    };
    Ok(match (r, c) {
        (Some(r), Some(c)) => match (row_txt(r)?, col_txt(c)?) {
            (Some(rt), Some(ct)) => Some((R1c1Tok::Cell(ct, rt), pos)),
            _ => None,
        },
        (Some(r), None) => row_txt(r)?.map(|t| (R1c1Tok::Row(t), pos)),
        (None, Some(c)) => col_txt(c)?.map(|t| (R1c1Tok::Col(t), pos)),
        (None, None) => None,
    })
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

    pub(crate) fn formula_variant(wb: &Workbook, a: CellAddr, r1c1: bool, legacy: bool) -> Variant {
        let text = if r1c1 && legacy {
            wb.legacy_formula_text_r1c1(a)
        } else if r1c1 {
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
                let mut found_ref = self.track_ref(r.book, s, found, Mode::Cells)?;
                found_ref.axis = Some(r.mode);
                return Ok(self.alloc(Object::Range(found_ref)));
            }
        }

        let index = |i: usize, column: bool| {
            Application::index_arg(a, i, column).map_err(|e| match a.get(i) {
                Some(Variant::Bstr(_))
                    if direct
                        && r.mode == Mode::Cells
                        && rect != whole_sheet()
                        && a.get(1).is_none() =>
                {
                    OmError::exception(HResult::E_INVALIDARG.0, None)
                }
                Some(Variant::Bstr(_)) if direct => OmError::app_defined(),

                Some(Variant::R8(_) | Variant::I8(_)) => OmError::app_defined(),
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
                let letters = |p: &str| {
                    let p = p.trim();
                    !p.is_empty() && p.chars().all(|c| c.is_ascii_alphabetic())
                };
                let all_letters = t.split(':').all(letters);
                return Err(if columns && all_letters {
                    OmError::type_mismatch()
                } else {
                    OmError::app_defined()
                });
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
        self.pivot_guard(book, sheet, rect, m, a)?;
        if let Err(e) = self.protect_guard(book, sheet, rect, m) {
            if let ("Value" | "Value2" | "_Default", Put) = (m.name, m.kind) {
                let flavour = if m.name == "Value2" {
                    Flavour::Value2
                } else {
                    Flavour::Value
                };
                let _ = self.put_values_in(r, sheet, rect, a, flavour, true);
            }
            return Err(e);
        }

        match (m.name, m.kind) {
            (
                "Value" | "Value2" | "_Default" | "Formula" | "FormulaLocal" | "Formula2"
                | "Formula2Local" | "FormulaR1C1" | "FormulaR1C1Local" | "Formula2R1C1"
                | "Formula2R1C1Local",
                Put,
            )
            | ("ClearContents" | "Clear", Func) => {
                self.book_mut(book)?.wb.note_cell_format(top_left)
            }

            ("NumberFormat" | "NumberFormatLocal" | "Text", Get) => {
                if let Ok(b) = self.book_mut(book) {
                    let f = b.wb.number_format(top_left).to_string();
                    b.wb.note_number_format(Some(&f));
                }
            }
            _ => {}
        }
        match (m.name, m.kind) {
            ("Select" | "Activate", Func) => self.range_select(book, sheet, &[rect], m),
            ("Address" | "AddressLocal", Get) => self.address_text(book, &[(sheet, rect)], a),
            ("Name", Get) => self.range_name_get(book, sheet, rect),
            ("Name", Put) => {
                let n = Application::put_value(a)
                    .string(0)?
                    .ok_or_else(OmError::type_mismatch)?;
                self.range_name_put(book, sheet, rect, &n)
            }
            ("Areas", Get) => {
                let areas = self.track_areas(book, sheet, &[rect], Mode::Cells)?;
                Ok(self.alloc(Object::Areas(areas)))
            }
            ("Calculate" | "CalculateRowMajorOrder", Func) => {
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
                self.book_mut(book)?.wb.hl_clear(sheet, &rect);
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
                let below = insert && (whole_rows || whole_cols) && a.i32(1)? == Some(1);
                let wb = &mut self.book_mut(book)?.wb;
                wb.insert_format_below = below;
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
                        (None, _) => match wb.data_beside(sheet, rect, insert) {
                            (true, false) => false,
                            (false, true) => true,
                            (true, true) => rect.cols() >= rect.rows(),
                            (false, false) => insert,
                        },
                        _ => return Err(OmError::app_defined()),
                    };
                    if insert {
                        wb.insert_cells(sheet, rect, down)
                    } else {
                        wb.delete_cells(sheet, rect, down)
                    }
                };
                wb.insert_format_below = false;
                done.map_err(edit_error)?;
                Ok(Variant::Bool(true))
            }
            ("Cut", Func) | ("Copy", Func) => {

                let Some(Variant::Dispatch(Some(id))) = a.get(0) else {
                    if m.name == "Copy" {
                        self.clip = Some((book, sheet, rect));
                        self.clip_more.0.clear();
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

            ("FillUp" | "FillLeft", Func) => {
                let up = m.name == "FillUp";
                let (src, dst) = if up {
                    let single = rect.r1 == rect.r0;
                    let s = if single { rect.r1 + 1 } else { rect.r1 };
                    if s >= MAX_ROWS {
                        return Err(OmError::app_defined_with(&format!(
                            "{} method of Range class failed",
                            m.name
                        )));
                    }
                    (
                        Rect::new(s, rect.c0, s, rect.c1),
                        Rect::new(rect.r0, rect.c0, rect.r1.min(s - 1), rect.c1),
                    )
                } else {
                    let single = rect.c1 == rect.c0;
                    let s = if single { rect.c1 + 1 } else { rect.c1 };
                    if s >= MAX_COLS {
                        return Err(OmError::app_defined_with(&format!(
                            "{} method of Range class failed",
                            m.name
                        )));
                    }
                    (
                        Rect::new(rect.r0, s, rect.r1, s),
                        Rect::new(rect.r0, rect.c0, rect.r1, rect.c1.min(s - 1)),
                    )
                };
                self.book_mut(book)?
                    .wb
                    .command(xlcalc::Command::Fill, |wb| {
                        wb.note_changed(sheet, &[rect]);
                        wb.fill(sheet, src, dst)
                    })
                    .map_err(edit_error)?;
                Ok(Variant::Bool(true))
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
            ("Find" | "FindNext" | "FindPrevious" | "End", _) => {
                self.range_search(book, sheet, &[rect], m, a)
            }
            ("SpecialCells", Func) => self.special_cells_member(book, sheet, &[rect], m, a),
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
            ("Worksheet" | "Parent", Get) => Ok(self.alloc(Object::Worksheet(book, sheet))),
            ("Application", Get) => Ok(Variant::Dispatch(Some(self.handle()))),
            ("Comment", Get) => self.range_comment(book, sheet, rect),
            ("AddComment", Func) => self.range_add_comment(book, sheet, rect, a),
            ("NoteText", Func) => self.range_note_text(book, sheet, rect, a),
            ("ClearComments" | "ClearNotes", Func) => {
                self.range_clear_notes(book, sheet, &[rect], m)
            }
            ("CurrentRegion", Get) => {
                let region = self
                    .book(book)?
                    .wb
                    .current_region(sheet, Rect::cell(rect.r0, rect.c0));
                self.sub_range(r, sheet, region, Mode::Cells)
            }
            ("Next", Get) if rect.c0 + 1 < MAX_COLS => {
                self.sub_range(r, sheet, Rect::cell(rect.r0, rect.c0 + 1), Mode::Cells)
            }
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

            ("FormulaR1C1" | "FormulaR1C1Local", Get) => {
                self.read_cells(book, sheet, rect, |wb, x| {
                    Application::formula_variant(wb, x, true, true)
                })
            }
            ("Formula2R1C1" | "Formula2R1C1Local", Get) => {
                self.read_cells(book, sheet, rect, |wb, x| {
                    Application::formula_variant(wb, x, true, false)
                })
            }
            ("FormulaArray", Get) => Ok(Application::formula_variant(
                &self.book(book)?.wb,
                CellAddr::new(sheet, rect.r0, rect.c0),
                false,
                true,
            )),
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
                let merged = self
                    .book(book)?
                    .wb
                    .merge_at(CellAddr::new(sheet, rect.r0, rect.c0));

                if merged.is_none() && !rect.is_cell() {
                    return Err(OmError::app_defined());
                }
                let area = merged.unwrap_or(rect);
                self.sub_range(r, sheet, area, Mode::Cells)
            }
            ("HasSpill", Get) => self.same_or_null(book, sheet, rect, |wb, x| {
                Variant::Bool(spill_of(wb, x).is_some())
            }),
            ("SpillingToRange", Get) => {
                let block = spill_of(&self.book(book)?.wb, CellAddr::new(sheet, rect.r0, rect.c0))
                    .map(|(_, block)| block);

                let Some(block) = block else {
                    return if rect.is_cell() {
                        Ok(Variant::Dispatch(None))
                    } else {
                        Err(OmError::app_defined())
                    };
                };
                self.sub_range(r, sheet, block, Mode::Cells)
            }
            ("SpillParent", Get) => {
                let anchor = spill_of(&self.book(book)?.wb, CellAddr::new(sheet, rect.r0, rect.c0))
                    .map(|(anchor, _)| anchor);
                let Some(anchor) = anchor else {
                    return if rect.is_cell() {
                        Ok(Variant::Dispatch(None))
                    } else {
                        Err(OmError::app_defined())
                    };
                };
                self.sub_range(r, sheet, Rect::cell(anchor.row, anchor.col), Mode::Cells)
            }
            ("HasFormula", Get) => self.same_or_null(book, sheet, rect, |wb, x| {
                Variant::Bool(wb.formula_text(x).is_some())
            }),
            ("PrefixCharacter", Get) => self.same_or_null(book, sheet, rect, |wb, x| {
                Variant::Bstr(if wb.has_apostrophe(x) { "'" } else { "" }.to_string())
            }),
            ("NumberFormat", Get) => self.same_or_null(book, sheet, rect, |wb, x| {
                let code = wb.number_format(x);
                if wb.cell_style(x).custom_code && host_spelled(wb, code) {
                    Variant::Bstr(code.to_string())
                } else {
                    Variant::Bstr(com_format(wb, code, a.lcid))
                }
            }),
            (
                "PivotField" | "PivotItem" | "PivotTable" | "PivotCell" | "QueryTable"
                | "ServerActions" | "Summary" | "LocationInTable",
                Get,
            ) => Err(OmError::app_defined()),
            ("XPath", Get) if rect.is_cell() => Ok(self.alloc(Object::Stub("XPath"))),
            ("XPath", Get) => Err(OmError::exception(
                HResult::E_INVALIDARG.0,
                Some(XPATH_RANGE_INVALID),
            )),
            ("Errors", Get) if rect.is_cell() => Ok(self.alloc(Object::Stub("Errors"))),
            ("ID", Get) if rect.is_cell() => Ok(Variant::BstrNull),
            ("Errors" | "CellControl", Get) if !rect.is_cell() => Err(OmError::app_defined()),
            ("CommentThreaded", Get) => {
                if rect.is_cell() {
                    Ok(Variant::Dispatch(None))
                } else {
                    Err(OmError::exception(HResult::E_INVALIDARG.0, None))
                }
            }
            ("MDX", Get) => {
                if rect.is_cell() {
                    Ok(Variant::Bstr(String::new()))
                } else {
                    Err(OmError::app_defined())
                }
            }
            ("ListHeaderRows" | "LinkedDataTypeState", Get) => Ok(Variant::I4(0)),
            ("AllowEdit", Get) => Ok(Variant::Bool(true)),
            ("HasRichDataType" | "SavedAsArray", Get) => Ok(Variant::Bool(false)),

            ("PageBreak", Get) if rect.is_cell() || !(rect.rows() == 1 && rect.cols() == MAX_COLS || rect.cols() == 1 && rect.rows() == MAX_ROWS) => {
                Err(OmError::app_defined())
            }
            ("PageBreak", Get) => {
                let rows = rect.cols() == MAX_COLS;
                let at = if rows { rect.r0 } else { rect.c0 };
                let has = self.book(book)?.wb.page_breaks(sheet, rows).contains(&at);
                Ok(Variant::I4(if has { -4135 } else { -4142 }))
            }
            ("ID", Get | Put) => Err(OmError::app_defined()),
            ("PageBreak", Put) => match Application::put_value(a).get(0) {
                Some(Variant::Bool(_)) => Ok(Variant::Empty),
                Some(Variant::I4(0 | 1)) => Ok(Variant::Empty),
                _ => Err(OmError::app_defined()),
            },
            ("UseStandardWidth", Get) => {
                let wb = &self.book(book)?.wb;
                let std = wb.sheet_standard_width(sheet);
                Ok(Variant::Bool(
                    (rect.c0..=rect.c1).all(|c| wb.column_width(sheet, c) == std),
                ))
            }
            ("UseStandardWidth", Put) => {
                if matches!(a.put, Some(Variant::Bstr(_))) {
                    return Err(OmError::app_defined());
                }
                if self.put_bool(a)? {
                    let wb = &mut self.book_mut(book)?.wb;
                    let std = wb.sheet_standard_width(sheet);
                    wb.set_column_width(sheet, rect.c0, rect.c1, std);
                }
                Ok(Variant::Empty)
            }
            ("UseStandardHeight", Get) => {
                let wb = &self.book(book)?.wb;
                let (mut all, mut none) = (true, true);
                for row in rect.r0..=rect.r1 {
                    if !wb.row_hidden(sheet, row) && wb.row_custom_h8(sheet, row).is_none() {
                        none = false;
                    } else {
                        all = false;
                    }
                }
                Ok(if all {
                    Variant::Bool(true)
                } else if none {
                    Variant::Bool(false)
                } else {
                    Variant::Null
                })
            }
            ("UseStandardHeight", Put) => {
                if matches!(a.put, Some(Variant::Bstr(_))) {
                    return Err(OmError::app_defined());
                }
                let standard = self.put_bool(a)?;
                let wb = &mut self.book_mut(book)?.wb;
                if standard {
                    wb.autofit_rows(sheet, rect.r0, rect.r1);
                } else {
                    for row in rect.r0..=rect.r1 {
                        let h = wb.row_height(sheet, row);
                        let _ = wb.set_row_height(sheet, row, row, h);
                    }
                }
                Ok(Variant::Empty)
            }
            ("Range", Get) => {
                let Some(Variant::Bstr(text)) = a.get(0) else {
                    return Err(OmError::app_defined());
                };
                let wb = &self.book(book)?.wb;
                let (_, rel) =
                    parse_range_text(wb, sheet, text).ok_or_else(OmError::app_defined)?;
                let moved = Rect::new(
                    rect.r0 + rel.r0,
                    rect.c0 + rel.c0,
                    rect.r0 + rel.r1,
                    rect.c0 + rel.c1,
                );
                if moved.r1 >= MAX_ROWS || moved.c1 >= MAX_COLS {
                    return Err(OmError::app_defined());
                }
                self.sub_range(r, sheet, moved, Mode::Cells)
            }

            ("NumberFormatLocal", Get) => self.same_or_null(book, sheet, rect, |wb, x| {
                Variant::Bstr(com_format(wb, wb.number_format(x), 0))
            }),
            ("Text", Get) => {
                self.same_or_null(book, sheet, rect, |wb, x| {
                    Variant::Bstr(cell_text_capped(wb, x))
                })
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
            ("FormulaR1C1" | "FormulaR1C1Local", Put) => {
                self.put_formulas(book, sheet, rect, a, Entry::R1C1, false)
            }
            ("Formula2R1C1" | "Formula2R1C1Local", Put) => {
                self.put_formulas(book, sheet, rect, a, Entry::DynamicR1C1, false)
            }
            ("NumberFormat" | "NumberFormatLocal", Put) => {
                let lcid = if m.name == "NumberFormat" { a.lcid } else { 0 };

                let value = a.put.clone().unwrap_or(Variant::Empty);
                let (rows, cols, items) = array_cells(&value).ok_or_else(OmError::app_defined)?;
                let mut codes = Vec::with_capacity(items.len());
                for item in &items {
                    if matches!(item, Variant::Bool(_)) {
                        return Err(OmError::unable_to_set(m.name, "Range"));
                    }
                    let one = Args {
                        values: Vec::new(),
                        put: Some(item.clone()),
                        lcid,
                    };

                    let fmt = self.put_string(&one)?.replace("\"$\"", "$");

                    if xlcalc::number_format_refused(&fmt) {
                        return Err(OmError::unable_to_set("NumberFormat", "Range"));
                    }
                    codes.push(fmt);
                }
                let wb = &mut self.book_mut(book)?.wb;
                let engine = |wb: &Workbook, code: &str| {
                    if us_names(lcid) {
                        engine_format(wb, code)
                    } else {
                        code.to_string()
                    }
                };
                if let Some(c) = codes.first() {
                    let first = engine(wb, c);
                    wb.note_number_format(Some(&first));
                }
                let host = |wb: &Workbook, code: &str| us_names(lcid) && host_spelled(wb, code);
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
            ("FormulaArray", Put) if !matches!(Application::put_value(a).get(0), Some(Variant::Bstr(t)) if t.starts_with('=')) =>
            {

                self.put_formulas(book, sheet, rect, a, Entry::Legacy, false)
            }
            ("FormulaArray", Put) => {
                let text = self.put_string(a)?;
                let wb = &mut self.book_mut(book)?.wb;
                let locale = input_locale(a.lcid, wb.locale());
                wb.with_input_locale(&locale, |wb| wb.set_array_formula(sheet, rect, &text))
                    .map_err(edit_error)?;
                Ok(Variant::Empty)
            }
            ("CellControl", Get) => Err(OmError::app_defined_with(
                "Excel VBA Object Model for Cell Control is not supported in your version of Excel.",
            )),
            ("Errors", Get) => Err(OmError::app_defined()),
            ("Previous", Get) if rect.c0 > 0 => self.sub_range(
                r,
                sheet,
                Rect::cell(rect.r0, rect.c0 - 1),
                Mode::Cells,
            ),
            ("ID", _) => Err(OmError::app_defined()),
            ("Group" | "Ungroup" | "ShowDetail", _) => {
                self.range_outline_member(r, sheet, rect, m, a)
            }
            ("CreateNames", Func) => {
                let flag = |i| a.bool(i).ok().flatten().unwrap_or(false);
                let sides = [flag(0), flag(1), flag(2), flag(3)];
                self.range_create_names(book, sheet, rect, sides)?;
                Ok(Variant::Bool(true))
            }
            ("ClearOutline", Func) => {
                self.book_mut(book)?.wb.clear_outline(sheet, rect);
                Ok(Variant::Bool(true))
            }
            ("Justify", Func) => {
                let book = self.book(r.book)?;
                let bad = (rect.r0..=rect.r1)
                    .flat_map(|row| (rect.c0..=rect.c1).map(move |col| (row, col)))
                    .any(|(row, col)| {
                        let x = xlcalc::CellAddr::new(sheet, row, col);
                        book.wb.formula_text(x).is_some()
                            || matches!(book.wb.value(x), xlcalc::Value::Number(_))
                    });
                if bad {
                    return Err(OmError::app_defined_with(
                        "Cannot justify cells containing numbers or formulas.",
                    ));
                }
                Ok(Variant::Bool(true))
            }
            ("ClearFormats", Func) | ("Clear", Func) => {
                let wb = &mut self.book_mut(book)?.wb;
                if m.name == "Clear" {
                    wb.hl_clear(sheet, &rect);
                    wb.notes_clear(sheet, &rect);
                    wb.spark_clear(sheet, &rect);
                    wb.clear_range(sheet, rect).map_err(edit_error)?;

                    wb.reset_number_format(sheet, rect);
                } else {
                    wb.clear_formats(sheet, rect);
                }

                if m.name == "Clear" && self.clear_keeps_locked(book, sheet) {
                    self.book_mut(book)?.wb.set_style_with(
                        sheet,
                        rect,
                        &|s: &xlcalc::style::Style| {
                            let mut s = s.clone();
                            s.locked = false;
                            s
                        },
                    );
                }
                Ok(Variant::Bool(true))
            }
            ("MergeCells", Put) => {
                let on = match &a.put {
                    None | Some(Variant::Empty | Variant::Null) => false,
                    _ => self
                        .put_bool(a)
                        .map_err(|_| OmError::unable_to_set("MergeCells", "Range"))?,
                };
                if on && self.merge_keeps_top_left_only(book, sheet) {
                    let wb = &mut self.book_mut(book)?.wb;
                    if rect.r1 > rect.r0 {
                        let below = Rect::new(rect.r0 + 1, rect.c0, rect.r1, rect.c1);
                        wb.clear_range(sheet, below).map_err(edit_error)?;
                    }
                    if rect.c1 > rect.c0 {
                        let right = Rect::new(rect.r0, rect.c0 + 1, rect.r0, rect.c1);
                        wb.clear_range(sheet, right).map_err(edit_error)?;
                    }
                }
                let before: Vec<(u32, u32, xlcalc::style::Style)> =
                    if on && u64::from(rect.rows()) * u64::from(rect.cols()) <= 1 << 16 {
                        let wb = &self.book(book)?.wb;
                        (rect.r0..=rect.r1)
                            .flat_map(|row| (rect.c0..=rect.c1).map(move |col| (row, col)))
                            .map(|(row, col)| {
                                (row, col, wb.cell_style(CellAddr::new(sheet, row, col)).clone())
                            })
                            .collect()
                    } else {
                        Vec::new()
                    };
                let v = self.range_view_member(r, sheet, rect, m, a)?;
                self.merge_resets_others(book, sheet, rect, &before)?;

                if on && self.contents_protected(book, sheet) {
                    let rest: Vec<Rect> = (rect.r0..=rect.r1)
                        .flat_map(|row| (rect.c0..=rect.c1).map(move |col| (row, col)))
                        .filter(|&(row, col)| (row, col) != (rect.r0, rect.c0))
                        .map(|(row, col)| Rect::new(row, col, row, col))
                        .collect();
                    let wb = &mut self.book_mut(book)?.wb;
                    for cell in rest {
                        wb.set_style_with(sheet, cell, &|st: &xlcalc::style::Style| {
                            let mut st = st.clone();
                            st.locked = true;
                            st
                        });
                    }
                }
                Ok(v)
            }
            (
                "Hidden" | "AutoFilter" | "Merge" | "UnMerge" | "ColumnWidth" | "Width" | "AutoFit"
                | "RowHeight" | "Height" | "Left" | "Top",
                _,
            ) => self.range_view_member(r, sheet, rect, m, a),
            ("OutlineLevel", _) => self.range_outline_member(r, sheet, rect, m, a),
            ("SparklineGroups", Get) => Ok(self.alloc(Object::Spark(
                book,
                sheet,
                crate::sparklines::SparkPart::Groups(rect),
            ))),
            ("FormatConditions", Get) => Ok(self.alloc(Object::FormatConditions(*r))),
            ("Validation", Get) => Ok(self.alloc(Object::Validation(*r))),
            ("DisplayFormat", Get) => Ok(self.alloc(Object::DisplayFormat(*r))),
            ("Hyperlinks", Get) => Ok(self.alloc(Object::Hyperlinks(book, sheet, rect))),
            ("ClearHyperlinks", Func) => {
                self.book_mut(book)?.wb.hl_clear(sheet, &rect);
                Ok(Variant::Empty)
            }
            ("Characters", _) => {
                let span = crate::rangeparts::CharSpan {
                    start: a.i32(0)?,
                    len: a.i32(1)?,
                };
                Ok(self.alloc(Object::Characters(*r, span)))
            }
            ("Phonetic", Get) => Ok(self.alloc(Object::Phonetic(*r))),

            ("Phonetics", Get) => Ok(Variant::Dispatch(None)),

            ("Precedents" | "DirectPrecedents" | "Dependents" | "DirectDependents", Get)
                if self.view.is_some() =>
            {
                Ok(self.alloc(Object::Range(*r)))
            }
            ("Precedents" | "DirectPrecedents" | "Dependents" | "DirectDependents", Get) => {
                self.trace_range(book, sheet, &[rect], m.name)
            }
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

    fn trace_range(
        &mut self,
        book: BookId,
        sheet: SheetId,
        rects: &[Rect],
        name: &str,
    ) -> Result<Variant, OmError> {
        let wb = &self.book(book)?.wb;
        let direct = name.starts_with("Direct");
        let found = if name.ends_with("Precedents") {
            wb.trace_precedents(sheet, rects, direct)
        } else {
            wb.trace_dependents(sheet, rects, direct)
        };
        if found.is_empty() {
            let mut e = OmError::app_defined_with("No cells were found.");
            e.error_info = true;
            return Err(e);
        }
        self.new_multi(book, sheet, &found, Mode::Cells)
    }

    fn multi_copy(
        &mut self,
        book: BookId,
        sheet: SheetId,
        rects: &[Rect],
        a: &Args,
    ) -> Result<Variant, OmError> {
        let stacked = rects
            .iter()
            .all(|r| r.c0 == rects[0].c0 && r.c1 == rects[0].c1);
        let side = rects
            .iter()
            .all(|r| r.r0 == rects[0].r0 && r.r1 == rects[0].r1);
        if !stacked && !side {
            self.clip = None;
            self.clip_more.0.clear();
            return Err(OmError::app_defined_with(
                "This action won't work on multiple selections.",
            ));
        }
        self.clip = Some((book, sheet, rects[0]));
        self.clip_more = (rects[1..].to_vec(), stacked);
        let Some(Variant::Dispatch(Some(id))) = a.get(0) else {
            return Ok(Variant::Bool(true));
        };
        let Some(Object::Range(dest)) = self.arena.get(*id) else {
            return Err(OmError::app_defined());
        };
        let (ds, dr) = self.resolve(&dest.clone())?;
        let mut off = 0;
        for src in rects {
            let at = if stacked {
                Rect::new(dr.r0 + off, dr.c0, dr.r0 + off, dr.c0)
            } else {
                Rect::new(dr.r0, dr.c0 + off, dr.r0, dr.c0 + off)
            };
            self.paste_copy(book, (sheet, *src), (ds, at), false)?;
            off += if stacked { src.rows() } else { src.cols() };
        }
        Ok(Variant::Bool(true))
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

            ("PasteSpecial", Func) => {
                for area in &areas {
                    self.range_member(area, m, a)?;
                }
                Ok(Variant::Bool(true))
            }
            ("Copy", Func) => self.multi_copy(book, sheet, &rects, a),
            ("Precedents" | "DirectPrecedents" | "Dependents" | "DirectDependents", Get)
                if self.view.is_none() =>
            {
                self.trace_range(book, sheet, &rects, m.name)
            }
            ("Find" | "FindNext" | "FindPrevious" | "End", _) => {
                self.range_search(book, sheet, &rects, m, a)
            }
            ("SpecialCells", Func) => self.special_cells_member(book, sheet, &rects, m, a),
            ("CurrentRegion", Get) => self.range_member(&first, m, a),
            ("DisplayFormat", Get) => Ok(self.alloc(Object::DisplayFormat(first))),
            ("Phonetic", Get) => Ok(self.alloc(Object::Phonetic(first))),
            ("Phonetics", Get) => Ok(Variant::Dispatch(None)),
            ("Hyperlinks", Get) => Ok(self.alloc(Object::Hyperlinks(book, sheet, rects[0]))),
            ("UseStandardHeight" | "UseStandardWidth", _) | ("Summary", Get) => {
                self.use_standard_member(book, sheet, &rects, m, a)
            }
            ("TextToColumns", Func) => Err(OmError::app_defined_with(
                "This can't be done on a multiple range selection.\n\nSelect a single range and try again.",
            )),

            ("FillDown" | "FillUp" | "FillLeft" | "FillRight", Func) => {
                for area in &areas {
                    self.range_member(area, m, a)?;
                }
                Ok(Variant::Bool(true))
            }
            ("Group" | "Ungroup" | "ShowDetail" | "OutlineLevel", _) => {
                Err(OmError::app_defined())
            }

            ("AutoFit", Func) if first.mode == Mode::Cells && first.axis.is_none() => {
                Err(OmError::app_defined_with(
                    "AutoFit method of Range class failed",
                ))
            }
            ("Comment", Get) => Ok(Variant::Dispatch(None)),
            ("AddComment", Func) => Err(OmError::exception(HResult::E_INVALIDARG.0, None)),
            ("NoteText", Func) => self.range_member(&first, m, a),
            ("ClearComments" | "ClearNotes", Func) => {
                self.range_clear_notes(book, sheet, &rects, m)
            }
            ("Address" | "AddressLocal", Get) => self.address_text(book, &parts, a),
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
            ("Resize" | "Item" | "ID", Get) => Err(OmError::app_defined()),
            ("Previous", Get) => Err(OmError::app_defined_with(
                "Unable to get the Previous property of the Range class",
            )),
            ("SavedAsArray", Get) => Err(OmError::app_defined()),

            ("Validation", Get) => {
                if self.validation_exists(&first) {
                    return Err(OmError::app_defined());
                }
                self.range_member(&first, m, a)
            }
            (
                "Left" | "Top" | "Next" | "ListHeaderRows" | "LinkedDataTypeState"
                | "HasRichDataType" | "AllowEdit" | "PivotTable" | "PivotField" | "PivotItem"
                | "LocationInTable" | "QueryTable" | "PivotCell" | "ServerActions" | "CellControl"
                | "MDX" | "Errors" | "XPath" | "CommentThreaded" | "Style",
                Get,
            ) => self.range_member(&first, m, a),
            ("FormulaArray", Get) => Ok(Variant::Null),

            ("HorizontalAlignment" | "VerticalAlignment", Get) => Ok(Variant::Null),
            ("ReadingOrder" | "ListObject", Get) => self.range_member(&first, m, a),
            ("HasSpill" | "SpillingToRange" | "SpillParent", Get) => Err(OmError::app_defined()),
            ("CountLarge", Get) => {
                let n: u64 = rects
                    .iter()
                    .map(|r| match mode {
                        Mode::Cells => r.cell_count(),
                        Mode::Rows => u64::from(r.rows()),
                        Mode::Columns => u64::from(r.cols()),
                    })
                    .sum();
                Ok(Variant::I8(n as i64))
            }
            ("Hidden" | "MergeArea" | "Name" | "CurrentArray", Get) => Err(OmError::app_defined()),
            (
                "AddIndent" | "Borders" | "Characters" | "ColumnWidth" | "Font" | "FormulaHidden"
                | "Height" | "IndentLevel" | "Interior" | "Locked" | "NumberFormatLocal"
                | "Orientation" | "PrefixCharacter" | "RowHeight" | "ShrinkToFit" | "Width"
                | "WrapText",
                Get,
            ) => self.range_member(&first, m, a),
            ("Range", Get) if a.get(0).is_some() => Err(OmError::app_defined()),
            ("PageBreak", Get) => Err(OmError::app_defined_with(
                "Unable to get the PageBreak property of the Range class",
            )),
            (
                "Value" | "_Default" | "Value2" | "Formula" | "Formula2" | "FormulaLocal"
                | "Formula2Local" | "FormulaR1C1" | "FormulaR1C1Local" | "Formula2R1C1"
                | "Formula2R1C1Local" | "Row" | "Column" | "Worksheet" | "Calculate",
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
            ("FormulaR1C1" | "FormulaR1C1Local", Put) => {
                self.put_formulas_in(book, &parts, a, Entry::R1C1, false)
            }
            ("Formula2R1C1" | "Formula2R1C1Local", Put) => {
                self.put_formulas_in(book, &parts, a, Entry::DynamicR1C1, false)
            }

            ("Value" | "Value2" | "_Default" | "NumberFormat", Put)
            | ("ClearContents" | "ClearFormats" | "Clear", Func) => {
                let mut last = Variant::Empty;
                let from = self.book_mut(book)?.wb.notices().len();
                for area in &areas {
                    last = self.range_member(area, m, a)?;
                }
                if m.name != "NumberFormat" && m.kind == Put {
                    let wb = &mut self.book_mut(book)?.wb;
                    merge_changes(wb, from, sheet, &rects);
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
        self.put_values_in(r, sheet, rect, a, flavour, false)
    }

    fn put_values_in(
        &mut self,
        r: &RangeRef,
        sheet: SheetId,
        rect: Rect,
        a: &Args,
        flavour: Flavour,
        skip_locked: bool,
    ) -> Result<Variant, OmError> {
        let value = a.put.clone().unwrap_or(Variant::Empty);
        let value = self.range_object_value(value)?;
        let (rows, cols, items) = array_cells(&value).ok_or_else(OmError::app_defined)?;
        let scalar = !matches!(value, Variant::Array(_));

        let target = rect;
        let lcid = a.lcid;
        let wb = &mut self.book_mut(r.book)?.wb;
        let first_notice = wb.notices().len();
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
                if skip_locked && wb.cell_style(addr).locked {
                    continue;
                }
                match item {
                    Some(v) => put_cell(wb, addr, v, flavour, lcid)?,
                    None => {
                        wb.set_value(addr, Value::Error(XlError::NA));
                    }
                }
            }
        }
        merge_changes(wb, first_notice, sheet, &[target]);
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
        let from = wb.notices().len();
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
        let rects: Vec<Rect> = parts.iter().map(|p| p.1).collect();
        merge_formula_changes(wb, from, sheet, &rects);
        Ok(Variant::Empty)
    }
}

#[derive(Clone, Copy, PartialEq, Eq)]
enum Entry {
    Legacy,

    Dynamic,

    R1C1,

    DynamicR1C1,
}

pub(crate) fn enter_text(
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
            let text = if matches!(entry, Entry::R1C1 | Entry::DynamicR1C1) && t.starts_with('=') {
                r1c1_to_a1(t, addr.row, addr.col).ok_or_else(OmError::app_defined)?
            } else {
                t.clone()
            };
            enter_text(
                wb,
                addr,
                &text,
                matches!(entry, Entry::Legacy | Entry::R1C1),
                lcid,
            )
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

pub(crate) fn engine_format(wb: &Workbook, code: &str) -> String {
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

        Variant::Bstr(t) if t.starts_with('\'') => {
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
                wb.set_number_format_stored(addr.sheet, Rect::cell(addr.row, addr.col), &fmt);
            }
        }
        Variant::Cy(c) if flavour == Flavour::Value => {
            let rounded = xlcalc::round_worksheet(*c as f64 / 10_000.0, 2);
            if wb.number_format(addr) == "@" {
                let text = render_us(CURRENCY_FORMAT, rounded, wb.settings().date_system);
                wb.set_value(addr, Value::Text(text.into()));
                wb.set_number_format_stored(
                    addr.sheet,
                    Rect::cell(addr.row, addr.col),
                    CURRENCY_FORMAT,
                );
                return Ok(());
            }
            number(wb, rounded);
            if format_replaced(wb.number_format(addr), false, true) {
                wb.set_number_format_stored(
                    addr.sheet,
                    Rect::cell(addr.row, addr.col),
                    CURRENCY_FORMAT,
                );
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

fn merge_changes(wb: &mut Workbook, from: usize, sheet: SheetId, rects: &[Rect]) {
    if rects.len() == 1 && rects[0].is_cell() {
        return;
    }
    let log = wb.notices_mut();
    let mut first = None;
    let mut i = from;
    while i < log.len() {
        if matches!(&log[i], Notice::Changed { sheet: s, .. } if *s == sheet) {
            if first.is_none() {
                first = Some(i);
                i += 1;
            } else {
                log.remove(i);
            }
        } else {
            i += 1;
        }
    }
    if let Some(i) = first {
        log[i] = changed_notice(sheet, rects);
    }
}

fn changed_notice(sheet: SheetId, rects: &[Rect]) -> Notice {
    let areas = rects.to_vec();
    if areas.len() > 1 {
        Notice::ChangedAsOne { sheet, areas }
    } else {
        Notice::Changed { sheet, areas }
    }
}

fn merge_formula_changes(wb: &mut Workbook, from: usize, sheet: SheetId, rects: &[Rect]) {
    if rects.len() == 1 && rects[0].is_cell() {
        return;
    }
    let log = wb.notices_mut();
    let tail = log.split_off(from.min(log.len()));
    let (mut calc, mut after, mut changed) = (false, false, false);
    for n in tail {
        match n {
            Notice::Changed { sheet: s, .. } if s == sheet => changed = true,
            Notice::Calculated { .. } if calc => {}
            Notice::Calculated { .. } => {
                calc = true;
                log.push(n);
            }
            Notice::AfterCalculate if after => {}
            Notice::AfterCalculate => {
                after = true;
                log.push(n);
            }
            n => log.push(n),
        }
    }
    if changed {
        log.push(changed_notice(sheet, rects));
    }
}

const XPATH_RANGE_INVALID: &str = "The specified range is invalid due to one or more of the following reasons:\n\n\u{2022}The range refers to cells in more than one column.\n\u{2022}The range contains mapped cells from more than one XML map.\n\u{2022}The range contains both mapped and unmapped cells.\n\u{2022}The range contains both table and non-table cells.\n\u{2022}The range is not contiguous.";

impl Application<'_> {
    fn merge_resets_others(
        &mut self,
        book: BookId,
        sheet: SheetId,
        rect: Rect,
        before: &[(u32, u32, xlcalc::style::Style)],
    ) -> Result<(), OmError> {
        let Some((_, _, anchor)) = before.first() else {
            return Ok(());
        };
        let anchor_alignment = anchor.alignment.clone();
        let wb = &mut self.book_mut(book)?.wb;
        for (i, (row, col, old)) in before.iter().enumerate() {
            let mut new = if i == 0 {
                old.clone()
            } else {
                let mut s = xlcalc::style::Style::default();
                if old.alignment == anchor_alignment {
                    s.alignment = old.alignment.clone();
                }
                s
            };
            let none = xlcalc::style::Border::default();
            new.borders.left = if *col == rect.c0 {
                old.borders.left
            } else {
                none
            };
            new.borders.right = if *col == rect.c1 {
                old.borders.right
            } else {
                none
            };
            new.borders.top = if *row == rect.r0 {
                old.borders.top
            } else {
                none
            };
            new.borders.bottom = if *row == rect.r1 {
                old.borders.bottom
            } else {
                none
            };
            if i == 0 {
                new.borders.diagonal = old.borders.diagonal;
            }
            wb.set_style_with(sheet, Rect::cell(*row, *col), &|_| new.clone());
        }
        Ok(())
    }
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
