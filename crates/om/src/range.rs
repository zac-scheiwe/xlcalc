use std::sync::Arc;
use xlcalc::style::Style;
use xlcalc::{CellAddr, DateSystem, EditError, Notice, Rect, SheetId, Value, Workbook, XlError};
use xlcalc_variant::{change_type, HResult, Locale, VarType, Variant};

use crate::addr::{
    external_prefix, max_cols, max_rows, parse_areas_text, parse_range_text, range_text,
    AddressStyle,
};
use crate::app::{unsupported, Application, BookId, Mode, Object, RangeRef};
use crate::cmdargs::Reader;
use crate::conv::{
    array_cells, array_variant, cell_variant, currency_overflow, date_overflow, input_locale,
    is_missing, scode_error, variant_locale, Args, Flavour, HOST_LCID,
};
use crate::error::{vba_scode, OmError, DISP_E_TYPEMISMATCH};
use crate::objects::{edit_error, whole_sheet};
use crate::style::StyleOwner;
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

fn header_class(wb: &Workbook, at: CellAddr) -> Option<(u8, Style)> {
    let kind = match wb.value(at) {
        Value::Blank => return None,
        Value::Number(_) => 1,
        Value::Text(_) => 2,
        Value::Bool(_) => 3,
        Value::Error(_) => 4,
        Value::Array(_) | Value::Lambda(_) => 5,
    };
    let mut st = wb.cell_style(at).clone();
    st.locked = true;
    st.formula_hidden = false;
    st.named = Arc::from("");
    st.custom_code = false;
    st.implicit_general = false;

    st.borders = xlcalc::style::Borders {
        bottom: wb.shown_border(at, xlcalc::style::Edge::Bottom),
        ..Default::default()
    };
    Some((kind, st))
}

fn header_row_count(wb: &Workbook, sheet: SheetId, rect: Rect) -> i32 {
    if rect.r0 == rect.r1 || (rect.c0 == 0 && rect.c1 == max_cols() - 1) {
        return 0;
    }
    let row = |r: u32| -> Vec<Option<(u8, Style)>> {
        (rect.c0..=rect.c1)
            .map(|c| header_class(wb, CellAddr::new(sheet, r, c)))
            .collect()
    };
    let mut reference: Vec<Option<(u8, Style)>> = vec![None; (rect.c1 - rect.c0 + 1) as usize];
    let mut seen = false;
    let mut run = 0;
    let mut r = rect.r0;
    while r < max_rows() && run < 3 {
        let cur = row(r);
        if cur.iter().all(Option::is_none) {
            if seen {
                return 0;
            }
        } else {
            let mut differs = false;
            for (have, new) in reference.iter_mut().zip(cur) {
                match (&*have, new) {
                    (_, None) => {}
                    (None, n) => *have = n,
                    (Some(h), Some(n)) => differs |= *h != n,
                }
            }
            if differs && seen {
                if run == 2 && rect.r0 > 0 {
                    let above = row(rect.r0 - 1);
                    let alike = above
                        .iter()
                        .zip(reference.iter())
                        .all(|(x, y)| match (x, y) {
                            (Some(x), Some(y)) => x == y,
                            _ => true,
                        });
                    if !alike && above.iter().any(Option::is_some) {
                        return 0;
                    }
                }
                return run;
            }
            seen = true;
        }
        run += 1;
        r += 1;
    }
    0
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

            Some(Part::Abs(n)) if n > i64::from(max) && max == max_cols() => Err(()),
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
        Ok(resolve(p, row, max_rows())?
            .map(|(n, abs)| format!("{}{}", if abs { "$" } else { "" }, n + 1)))
    };
    let col_txt = |p: Option<Part>| -> Result<Option<String>, ()> {
        Ok(resolve(p, col, max_cols())?.map(|(n, abs)| {
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
    pub(crate) fn convert_formula(&mut self, a: &Args) -> Result<Variant, OmError> {
        use crate::convertformula::{convert, Style};
        let err = || Ok(Variant::Error(-2_146_826_273));

        let given = |i: usize| match a.get(i) {
            None | Some(Variant::Null | Variant::Empty) => None,
            Some(v) => Some(v),
        };
        let formula = match a.get(0) {
            Some(Variant::Bstr(s)) => s.clone(),
            Some(Variant::Bool(true)) => "TRUE".to_string(),
            Some(Variant::Bool(false)) => "FALSE".to_string(),
            Some(v @ (Variant::I2(_) | Variant::I4(_) | Variant::R4(_) | Variant::R8(_))) => {
                match change_type(v, VarType::Bstr, Locale::EN_US, 0) {
                    Ok(Variant::Bstr(s)) => s,
                    _ => return err(),
                }
            }
            _ => return err(),
        };
        let style_of = |v: &Variant| -> Option<Style> {
            match v {
                Variant::Bstr(_) | Variant::Dispatch(_) | Variant::Array(_) => None,
                v => match change_type(v, VarType::I4, Locale::EN_US, 0) {
                    Ok(Variant::I4(-4150)) => Some(Style::R1c1),
                    Ok(_) => Some(Style::A1),
                    Err(_) => None,
                },
            }
        };

        let from = match given(1) {
            None => Style::R1c1,
            Some(v) => match style_of(v) {
                Some(s) => s,
                None => return err(),
            },
        };
        let to = match given(2) {
            None => None,
            Some(v) => match style_of(v) {
                Some(s) => Some(s),
                None => return err(),
            },
        };
        let mode = match given(3) {
            None => None,
            Some(v @ (Variant::I2(_) | Variant::I4(_) | Variant::Bool(_))) => {
                match change_type(v, VarType::I4, Locale::EN_US, 0) {
                    Ok(Variant::I4(-1 | 1)) => Some(1),
                    Ok(Variant::I4(0 | 4)) => Some(4),
                    Ok(Variant::I4(n @ (2 | 3))) => Some(n),
                    _ => return err(),
                }
            }
            Some(_) => return err(),
        };

        if a.get(1).is_none() && to.is_none() && mode.is_none() {
            return err();
        }
        let (row, col) = match a.get(4) {
            None => (1, 1),
            Some(v @ Variant::Dispatch(_)) => match self.range_areas(Some(v)) {
                Ok((_, _, rects)) => (rects[0].r0 + 1, rects[0].c0 + 1),
                Err(_) => return err(),
            },
            Some(_) => return err(),
        };

        if to.is_none() && mode.is_none() {
            return Ok(Variant::Bstr(formula));
        }
        let book = self.active_book()?.name.clone();
        match convert(
            &formula,
            from,
            to.unwrap_or(Style::R1c1),
            mode,
            &book,
            row,
            col,
        ) {
            Some(t) => Ok(Variant::Bstr(t)),
            None => err(),
        }
    }

    fn rect_cells(sheet: SheetId, rect: Rect) -> impl Iterator<Item = CellAddr> {
        (rect.r0..=rect.r1)
            .flat_map(move |r| (rect.c0..=rect.c1).map(move |c| CellAddr::new(sheet, r, c)))
    }

    pub(crate) fn read_cells(
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

    pub(crate) fn value_variant(wb: &Workbook, a: CellAddr, flavour: Flavour) -> Variant {
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

    fn same_format_or_null(
        &self,
        book: BookId,
        parts: &[(SheetId, Rect)],
        f: impl Fn(&Workbook, &Style) -> Variant,
    ) -> Result<Variant, OmError> {
        let wb = &self.book(book)?.wb;
        let mut values = parts
            .iter()
            .flat_map(|&(sheet, rect)| wb.range_styles(sheet, rect))
            .map(|s| f(wb, s));
        let first = values.next().unwrap_or(Variant::Null);
        Ok(if values.all(|v| v == first) {
            first
        } else {
            Variant::Null
        })
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

    fn variant_operand(&self, v: &Variant) -> Result<Variant, OmError> {
        let Variant::Dispatch(Some(_)) = v else {
            return Ok(v.clone());
        };
        let (book, sheet, rects) = self.range_areas(Some(v))?;
        match rects.as_slice() {
            [rect] if rect.is_cell() => {
                let wb = &self.book(book)?.wb;
                Ok(Self::value_variant(
                    wb,
                    CellAddr::new(sheet, rect.r0, rect.c0),
                    Flavour::Value,
                ))
            }
            _ => Err(OmError::type_mismatch()),
        }
    }

    fn coerce_variant_to_int(
        &self,
        a: &Args,
        i: usize,
        lo: i64,
        hi: i64,
        default: i64,
    ) -> Result<i64, OmError> {
        let Some(v) = a.get(i).filter(|v| !is_missing(v)) else {
            return Ok(default);
        };
        let v = self.variant_operand(v)?;
        match change_type(&v, VarType::I4, variant_locale(a.lcid), 0) {
            Ok(Variant::I4(n)) if (lo..hi).contains(&i64::from(n)) => Ok(i64::from(n)),
            Ok(_) => Err(OmError::app_defined()),
            Err(HResult::DISP_E_OVERFLOW) => {
                Err(OmError::exception(HResult::DISP_E_OVERFLOW.0, None))
            }
            Err(_) => Err(OmError::type_mismatch()),
        }
    }

    fn coerce_variant_to_number(&self, a: &Args, i: usize) -> Result<f64, OmError> {
        let invalid = || OmError::exception(HResult::E_INVALIDARG.0, None);
        let Some(v) = a.get(i) else {
            return Err(invalid());
        };
        match self.variant_operand(v)? {
            Variant::Empty => Ok(0.0),
            Variant::I2(n) => Ok(f64::from(n)),
            Variant::I4(n) | Variant::Int(n) => Ok(f64::from(n)),
            Variant::I8(n) => Ok(n as f64),
            Variant::Ui1(n) => Ok(f64::from(n)),
            Variant::R4(x) => Ok(f64::from(x)),
            Variant::R8(x) | Variant::Date(x) => Ok(x),
            Variant::Cy(n) => Ok(n as f64 / 10_000.0),
            Variant::Bool(b) => Ok(if b { -1.0 } else { 0.0 }),
            _ => Err(invalid()),
        }
    }

    fn item(
        &mut self,
        r: &RangeRef,
        sheet: SheetId,
        rect: Rect,
        a: &Args,
    ) -> Result<Variant, OmError> {
        let (rw_max, col_max) = (i64::from(max_rows()), i64::from(max_cols()));
        let given = |i: usize| a.get(i).is_some_and(|v| !is_missing(v));
        let (r0, c0) = (i64::from(rect.r0), i64::from(rect.c0));
        let (r1, c1) = (i64::from(rect.r1), i64::from(rect.c1));
        let text_ref = |i: usize| match a.get(i) {
            Some(Variant::Bstr(t)) => {
                crate::addr::sz_to_ref(t, true).and_then(crate::addr::fit_grid)
            }
            _ => None,
        };
        let whole_rows = |f: &Rect| f.c0 == 0 && i64::from(f.c1) == col_max - 1;
        let whole_cols = |f: &Rect| f.r0 == 0 && i64::from(f.r1) == rw_max - 1;

        let (mut height, mut width) = (r1 - r0, c1 - c0);
        let (ro, co);
        let mode;
        if given(1) {
            if r.mode != Mode::Cells {
                return Err(OmError::app_defined());
            }
            ro = self.coerce_variant_to_int(a, 0, -rw_max, rw_max + 1, 1)? - 1;
            co = match text_ref(1) {
                Some(f) => {
                    if !whole_cols(&f) || f.c0 != f.c1 {
                        return Err(OmError::app_defined());
                    }
                    i64::from(f.c0)
                }
                None => self.coerce_variant_to_int(a, 1, -col_max, col_max + 1, 1)? - 1,
            };
            (height, width) = (0, 0);
            mode = Mode::Cells;
        } else {
            mode = r.mode;
            match r.mode {
                Mode::Cells => {
                    let columns = width + 1;
                    match self.coerce_variant_to_int(a, 0, -0x7fff_ffff, 0x7fff_ffff, 1) {
                        Ok(n) => {
                            ro = (n - 1) / columns;
                            co = (n - 1) % columns;
                        }
                        Err(_) => {
                            let x = self.coerce_variant_to_number(a, 0)?;
                            if x.abs() > (rw_max * col_max) as f64 {
                                return Err(OmError::app_defined());
                            }
                            let q = ((x - 1.0) / columns as f64).trunc();
                            ro = q as i64;
                            co = (x - 1.0 - q * columns as f64).trunc() as i64;
                        }
                    }
                    (height, width) = (0, 0);
                }
                Mode::Rows => {
                    co = 0;
                    match text_ref(0) {
                        Some(f) => {
                            if !whole_rows(&f) {
                                return Err(OmError::app_defined());
                            }
                            ro = i64::from(f.r0);
                            height = i64::from(f.r1 - f.r0);
                        }
                        None => {
                            ro = self.coerce_variant_to_int(a, 0, -rw_max, rw_max + 1, 1)? - 1;
                            height = 0;
                        }
                    }
                }
                Mode::Columns => {
                    ro = 0;
                    match text_ref(0) {
                        Some(f) => {
                            if !whole_cols(&f) {
                                return Err(OmError::app_defined());
                            }
                            co = i64::from(f.c0);
                            width = i64::from(f.c1 - f.c0);
                        }
                        None => {
                            co = self.coerce_variant_to_int(a, 0, -col_max, col_max + 1, 1)? - 1;
                            width = 0;
                        }
                    }
                }
            }

            if r0 + height < 0 || r0 + height >= rw_max || c0 + width < 0 || c0 + width >= col_max {
                return Err(OmError::app_defined());
            }
        }

        let found = grid_rect(r0 + ro, c0 + co, r0 + ro + height, c0 + co + width)
            .ok_or_else(OmError::app_defined)?;
        self.sub_range(r, sheet, found, mode)
    }

    fn range_shift(
        &mut self,
        book: BookId,
        at: (SheetId, Rect),
        insert: bool,
        shift: Option<i32>,
        below: bool,
    ) -> Result<Variant, OmError> {
        let (sheet, rect) = at;
        let whole_rows = shift == Some(3) || rect.c0 == 0 && rect.c1 == max_cols() - 1;
        let whole_cols =
            !whole_rows && (shift == Some(4) || rect.r0 == 0 && rect.r1 == max_rows() - 1);
        if !(whole_rows || whole_cols) && shift.is_some_and(|s| !(1..=2).contains(&s)) {
            return Err(OmError::app_defined());
        }
        let wb = &mut self.book_mut(book)?.wb;
        wb.insert_format_below = below;
        let band = if whole_rows {
            Some((true, rect.r0, rect.rows()))
        } else if whole_cols {
            Some((false, rect.c0, rect.cols()))
        } else {
            None
        };
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
            let down = match shift {
                Some(s) => s == 2,
                None => match wb.data_beside(sheet, rect, insert) {
                    (true, false) => false,
                    (false, true) => true,
                    (true, true) => rect.cols() >= rect.rows(),
                    (false, false) => insert,
                },
            };
            if insert {
                wb.insert_cells(sheet, rect, down)
            } else {
                wb.delete_cells(sheet, rect, down)
            }
        };
        wb.insert_format_below = false;
        done.map_err(edit_error)?;
        if let Some((rows, at, n)) = band {
            self.follow_source_shift(book, sheet, rows, at, n, insert);
        }
        Ok(Variant::Bool(true))
    }

    pub(crate) fn range_member(
        &mut self,
        r: &RangeRef,
        m: &'static Member,
        a: &Args,
    ) -> Result<Variant, OmError> {
        use InvokeKind::*;

        let empties = matches!(
            (m.name, m.kind),
            (
                "Value" | "Value2" | "_Default" | "Formula" | "Formula2",
                Put
            ) | ("ClearContents", Func)
        );
        let held: Vec<(SheetId, u32, u32)> = if empties {
            self.resolve(r)
                .ok()
                .and_then(|(sheet, rect)| {
                    let sh = self.book(r.book).ok()?.wb.sheet(sheet)?;
                    Some(
                        sh.entries_in(rect)
                            .map(|(row, col, _)| (sheet, row, col))
                            .collect(),
                    )
                })
                .unwrap_or_default()
        } else {
            Vec::new()
        };

        if matches!((m.name, m.kind), ("Clear", Func)) && !self.emptied.is_empty() {
            if let Ok((sheet, rect)) = self.resolve(r) {
                let book = r.book;
                self.emptied.retain(|&(b, s, row, col)| {
                    !(b == book && s == sheet && rect.contains(row, col))
                });
            }
        }
        let out = self.range_member_inner(r, m, a);
        if !held.is_empty() {
            if let Ok(b) = self.book(r.book) {
                let gone: Vec<_> = held
                    .into_iter()
                    .filter(|&(s, row, col)| {
                        matches!(b.wb.value(CellAddr::new(s, row, col)), Value::Blank)
                    })
                    .collect();
                self.emptied
                    .extend(gone.into_iter().map(|(s, row, col)| (r.book, s, row, col)));
            }
        }
        out
    }

    fn range_member_inner(
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

        if let Ok(b) = self.book_mut(book) {
            match vifmt_effect(m.name, m.kind) {
                Vifmt::Exact => b.wb.note_exact_cell_format(top_left),
                Vifmt::Reset => b.wb.note_number_format(None),
                Vifmt::Leaves => {}
            }
        }
        if (m.name, m.kind) == ("Value", Put) {
            self.value_put_kind(a, 1)?;
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
                    return self.item(&moded, sheet, rect, a);
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

                if matches!(a.get(0), Some(Variant::Null)) {
                    return Err(OmError::type_mismatch());
                }
                let mut operands = self
                    .range_operands(m.dispid, a)
                    .map_err(|_| OmError::app_defined())?;

                let reader = Reader {
                    wb: &self.book(book)?.wb,
                    lcid: a.lcid,
                };
                let origin = reader
                    .int(operands.get(1))
                    .map_err(|_| OmError::type_mismatch())?;
                operands.take(1);
                let below = insert && origin == Some(1);
                let refused = OmError::type_mismatch();
                self.run_command(book, a.lcid, operands, &refused, |app, o| {
                    let reader = Reader {
                        wb: &app.book(book)?.wb,
                        lcid: a.lcid,
                    };
                    let shift = reader
                        .int(o.get(0))
                        .map_err(|_| OmError::type_mismatch())?;
                    app.range_shift(book, (sheet, rect), insert, shift, below)
                })
            }
            ("Cut", Func) | ("Copy", Func) => {

                let dest = match self.paste_destination(book, sheet, m, a) {
                    Ok(dest) => dest,
                    Err(e) => {
                        self.clip = None;
                        self.clip_more.0.clear();
                        return Err(e);
                    }
                };
                let Some((ds, dr)) = dest else {

                    self.clip = Some((book, sheet, rect));
                    self.clip_cut = m.name == "Cut";
                    self.clip_more.0.clear();
                    self.book_mut(book)?.wb.mark_copy(Some((sheet, rect)));
                    return Ok(Variant::Bool(true));
                };

                self.clip = None;
                self.clip_more.0.clear();
                self.paste_copy(book, (sheet, rect), (ds, dr), m.name == "Cut", true)?;
                Ok(Variant::Bool(true))
            }
            ("Dirty", Func) => {
                self.book_mut(book)?.wb.dirty_range(sheet, rect);
                Ok(Variant::Empty)
            }
            ("FillDown" | "FillUp" | "FillRight" | "FillLeft", Func)
                if !fill_enabled(m.name, rect) =>
            {
                Err(fill_failed(m.name))
            }

            ("FillDown" | "FillUp" | "FillRight" | "FillLeft", Func)
                if (rect.r1 > rect.r0 || matches!(m.name, "FillRight" | "FillLeft"))
                    && self.book(book).is_ok_and(|b| {
                        b.wb.filter_mode(sheet)
                            && (rect.r0..=rect.r1).any(|r| !b.wb.row_hidden(sheet, r))
                    }) =>
            {
                let name = m.name;
                let book_ref = self.book_mut(book)?;
                let visible: Vec<u32> = (rect.r0..=rect.r1)
                    .filter(|&r| !book_ref.wb.row_hidden(sheet, r))
                    .collect();
                let vcols: Vec<u32> = (rect.c0..=rect.c1)
                    .filter(|&c| !book_ref.wb.col_hidden(sheet, c))
                    .collect();
                let mut jobs: Vec<(Rect, Rect)> = Vec::new();
                match name {
                    "FillDown" | "FillUp" => {
                        if let (Some(&first), Some(&last)) = (visible.first(), visible.last()) {
                            let s = if name == "FillDown" { first } else { last };
                            for &r in visible.iter().filter(|&&r| r != s) {
                                jobs.push((
                                    Rect::new(s, rect.c0, s, rect.c1),
                                    Rect::new(r, rect.c0, r, rect.c1),
                                ));
                            }
                        }
                    }
                    _ => {

                        if let (Some(&first), Some(&last)) = (vcols.first(), vcols.last()) {
                            let s = if name == "FillRight" { first } else { last };
                            for &r in &visible {
                                for &c in vcols.iter().filter(|&&c| c != s) {
                                    jobs.push((Rect::new(r, s, r, s), Rect::new(r, c, r, c)));
                                }
                            }
                        }
                    }
                }
                book_ref
                    .wb
                    .command(xlcalc::Command::Fill, |wb| {
                        wb.note_changed(sheet, &[rect]);

                        wb.creating_formulas(|wb| {
                            for (src, dst) in &jobs {
                                wb.fill(sheet, *src, *dst)?;
                            }
                            Ok(())
                        })
                    })
                    .map_err(edit_error)?;
                for (src, dst) in jobs {
                    self.carry_objects(book, (sheet, src), (sheet, dst), false)?;
                }
                Ok(Variant::Bool(true))
            }
            ("FillUp" | "FillLeft", Func) => {
                let up = m.name == "FillUp";
                let (src, dst) = if up {
                    let single = rect.r1 == rect.r0;
                    let s = if single { rect.r1 + 1 } else { rect.r1 };
                    (
                        Rect::new(s, rect.c0, s, rect.c1),
                        Rect::new(rect.r0, rect.c0, rect.r1.min(s - 1), rect.c1),
                    )
                } else {
                    let single = rect.c1 == rect.c0;
                    let s = if single { rect.c1 + 1 } else { rect.c1 };
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
                self.carry_objects(book, (sheet, src), (sheet, dst), false)?;
                Ok(Variant::Bool(true))
            }
            ("FillDown" | "FillRight", Func)
                if (m.name == "FillDown" && rect.r1 == rect.r0)
                    || (m.name == "FillRight" && rect.c1 == rect.c0) =>
            {
                let src = if m.name == "FillDown" {
                    Rect::new(rect.r0 - 1, rect.c0, rect.r0 - 1, rect.c1)
                } else {
                    Rect::new(rect.r0, rect.c0 - 1, rect.r1, rect.c0 - 1)
                };
                self.book_mut(book)?
                    .wb
                    .command(xlcalc::Command::Fill, |wb| {
                        wb.note_changed(sheet, &[rect]);
                        wb.fill(sheet, src, rect)
                    })
                    .map_err(edit_error)?;
                self.carry_objects(book, (sheet, src), (sheet, rect), false)?;
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
                    self.carry_objects(book, (sheet, src), (sheet, dst), false)?;
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
                let whole = Rect::new(rect.r0, 0, rect.r1, max_cols() - 1);
                self.sub_range(r, sheet, whole, Mode::Rows)
            }
            ("EntireColumn", Get) => {
                let whole = Rect::new(0, rect.c0, max_rows() - 1, rect.c1);
                self.sub_range(r, sheet, whole, Mode::Columns)
            }
            ("Worksheet" | "Parent", Get) => Ok(self.alloc(Object::Worksheet(book, sheet))),
            ("Application", Get) => Ok(Variant::Dispatch(Some(self.handle()))),
            ("Comment", Get) => self.range_comment(book, sheet, rect),
            ("AddComment", Func) => self.range_add_comment(book, sheet, rect, a),
            ("AddCommentThreaded", Func) => self.range_add_thread(book, sheet, rect, a),
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

            ("Next" | "Previous", Get)
                if !rect.is_cell() && self.sheet_prot(book, sheet).contents =>
            {
                self.sub_range(r, sheet, Rect::cell(rect.r0, rect.c0), Mode::Cells)
            }
            ("Next", Get) if rect.c0 + 1 < max_cols() => {
                self.sub_range(r, sheet, Rect::cell(rect.r0, rect.c0 + 1), Mode::Cells)
            }
            ("Offset", Get) => {
                let moved = self.offset_areas(book, sheet, &[rect], a)?;
                self.sub_range(r, sheet, moved[0], r.mode)
            }

            ("Resize", Get) => {
                let rows = self
                    .coerce_variant_to_int(a, 0, 0, i64::from(max_rows()) + 1, i64::from(rect.rows()))
                    .map_err(|_| OmError::app_defined())?;
                let cols = self
                    .coerce_variant_to_int(a, 1, 0, i64::from(max_cols()) + 1, i64::from(rect.cols()))
                    .map_err(|_| OmError::app_defined())?;
                let r1 = i64::from(rect.r0) + rows - 1;
                let c1 = i64::from(rect.c0) + cols - 1;
                if rows < 1 || cols < 1 || r1 >= i64::from(max_rows()) || c1 >= i64::from(max_cols()) {
                    return Err(OmError::app_defined());
                }
                let resized = Rect::new(rect.r0, rect.c0, r1 as u32, c1 as u32);
                self.sub_range(r, sheet, resized, r.mode)
            }
            ("Item" | "_Default", Get) if a.get(0).is_some() || a.get(1).is_some() => {
                self.item(r, sheet, rect, a)
            }
            ("Item", Get) if r.mode != Mode::Cells && a.get(0).is_none() && a.get(1).is_none() => {
                Err(OmError::exception(0x8007_000E_u32 as i32, None))
            }

            ("Value" | "Item" | "_Default", Get) => {

                if m.name == "Value" {
                    match self.value_index(a)? {
                        10 => {}
                        kind @ (11 | 12) => {
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
                        _ => return Err(OmError::app_defined()),
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

            ("FormulaArray", Get)
                if !rect.is_cell()
                    && self.book(book)?.wb.current_array(top_left) != Some(rect) =>
            {
                self.same_or_null(book, sheet, rect, |wb, x| {
                    Application::formula_variant(wb, x, false, true)
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
                    .ok_or_else(crate::rangefind::no_cells)?;
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
            ("NumberFormat", Get) => {
                self.same_format_or_null(book, &[(sheet, rect)], |wb, s| {
                    let code = &s.number_format;
                    if s.custom_code && host_spelled(wb, code) {
                        Variant::Bstr(code.to_string())
                    } else {
                        Variant::Bstr(com_format(wb, code, a.lcid))
                    }
                })
            }
            (
                "PivotField" | "PivotItem" | "PivotTable" | "PivotCell" | "QueryTable"
                | "ServerActions" | "Summary" | "LocationInTable",
                Get,
            ) => Err(OmError::app_defined()),
            ("XPath", Get) if rect.c0 == rect.c1 => Ok(self.alloc(Object::Stub("XPath"))),
            ("XPath", Get) => Err(OmError::exception(
                HResult::E_INVALIDARG.0,
                Some(XPATH_RANGE_INVALID),
            )),
            ("Errors", Get) if rect.is_cell() => Ok(self.alloc(Object::Stub("Errors"))),
            ("ID", Get) if rect.r0 == rect.r1 => Ok(self
                .range_ids
                .get(&(book, sheet, rect.r0, rect.c0))
                .map_or(Variant::BstrNull, |t| Variant::Bstr(t.clone()))),
            ("ID", Put) if rect.r0 == rect.r1 => {
                let t = self.put_string_alpha(a)?;
                self.range_ids.insert((book, sheet, rect.r0, rect.c0), t);
                Ok(Variant::Empty)
            }
            ("FormulaLabel", Get | Put) => Err(OmError::app_defined()),
            ("Errors" | "CellControl", Get) if !rect.is_cell() => Err(OmError::app_defined()),
            ("CommentThreaded", Get) => self.range_thread(book, sheet, rect),
            ("MDX", Get) => {
                if rect.is_cell() {
                    Ok(Variant::Bstr(String::new()))
                } else {
                    Err(OmError::app_defined())
                }
            }
            ("ListHeaderRows", Get) => {
                let wb = &self.book(book)?.wb;
                Ok(Variant::I4(header_row_count(wb, sheet, rect)))
            }
            ("LinkedDataTypeState", Get) => Ok(Variant::I4(0)),
            ("AllowEdit", Get) => Ok(Variant::Bool(
                !(self.sheet_prot(book, sheet).contents && self.any_locked(book, sheet, rect)),
            )),
            ("HasRichDataType" | "SavedAsArray", Get) => Ok(Variant::Bool(false)),

            ("PageBreak", Get) if r.eff_mode() == Mode::Cells => Err(OmError::app_defined()),
            ("PageBreak", Get) => {
                let rows = match r.eff_mode() {
                    Mode::Rows => true,
                    Mode::Columns => false,
                    Mode::Cells => unreachable!("refused above"),
                };
                let at = if rows { rect.r0 } else { rect.c0 };
                let has = self
                    .book(book)?
                    .wb
                    .all_page_breaks(sheet, rows)
                    .contains(&at);
                Ok(Variant::I4(if has { -4135 } else { -4142 }))
            }
            ("ID", Get | Put) => Err(OmError::app_defined()),

            ("PageBreak", Put) => {
                let manual = match Application::put_value(a).get(0) {
                    Some(Variant::Bool(b)) => *b,
                    Some(Variant::Empty) | None => false,
                    Some(v) => {
                        let n = match v {
                            Variant::I2(n) => Some(f64::from(*n)),
                            Variant::I4(n) => Some(f64::from(*n)),
                            Variant::R4(n) => Some(f64::from(*n)),
                            Variant::R8(n) => Some(*n),

                            Variant::Bstr(t) => t.trim().parse::<f64>().ok().filter(|n| *n >= 0.0),
                            _ => None,
                        };
                        match n {
                            Some(-4142.0) => false,
                            Some(-4135.0) => true,
                            Some(n) if n >= 0.0 && n.trunc() == 0.0 => false,
                            Some(n) if n >= 0.0 && n.trunc() == 1.0 => true,
                            _ => return Err(OmError::app_defined()),
                        }
                    }
                };
                let mode = r.eff_mode();
                let wb = &mut self.book_mut(book)?.wb;
                if !manual && rect.is_full_rows() && rect.is_full_columns() {
                    for at in wb.all_page_breaks(sheet, true) {
                        wb.delete_page_break(sheet, true, at);
                    }
                    return Ok(Variant::Empty);
                }
                let (rows, cols) = match (manual, mode) {
                    (true, Mode::Rows) => (true, false),
                    (true, Mode::Columns) => (false, true),
                    _ => (true, true),
                };
                let sides: Vec<(bool, u32)> = [(true, rect.r0, rows), (false, rect.c0, cols)]
                    .into_iter()
                    .filter(|(_, at, on)| *on && *at > 0)
                    .map(|(is_rows, at, _)| (is_rows, at))
                    .collect();
                if sides.is_empty() || (rect.r0 == 0 && rect.c0 == 0) {
                    return Err(OmError::app_defined());
                }
                for (is_rows, at) in sides {
                    if manual {
                        wb.add_page_break(sheet, is_rows, at);
                    } else {
                        wb.delete_page_break(sheet, is_rows, at);
                    }
                }
                Ok(Variant::Empty)
            }
            ("UseStandardWidth", Get) => {
                let wb = &self.book(book)?.wb;

                let first = wb.col_standard(sheet, rect.c0);
                Ok(if used_area_differs(wb, sheet, rect, |c| wb.col_standard(sheet, c)) {
                    Variant::Null
                } else {
                    Variant::Bool(first)
                })
            }
            ("UseStandardWidth", Put) => {
                if matches!(a.put, Some(Variant::Bstr(_))) {
                    return Err(OmError::app_defined());
                }
                if self.put_bool(a)? {
                    let wb = &mut self.book_mut(book)?.wb;
                    wb.use_standard_width(sheet, rect.c0, rect.c1);
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
                let wb = &self.book(book)?.wb;
                let moved_by = |rel: Rect| -> Result<Rect, OmError> {
                    let moved = Rect::new(
                        rect.r0 + rel.r0,
                        rect.c0 + rel.c0,
                        rect.r0 + rel.r1,
                        rect.c0 + rel.c1,
                    );
                    if moved.r1 >= max_rows() || moved.c1 >= max_cols() {
                        return Err(OmError::app_defined());
                    }
                    Ok(moved)
                };
                let given = |i: usize| a.get(i).filter(|v| !is_missing(v));
                let Some(first) = given(0) else {
                    return Err(OmError::app_defined());
                };
                if let Some(second) = given(1) {
                    let part = |v: &Variant| -> Result<(Rect, Mode, SheetId), OmError> {
                        match v {
                            Variant::Bstr(t) => crate::addr::sz_to_ref(t, false)
                                .map(|b| (b, Mode::Cells, sheet))
                                .ok_or_else(OmError::app_defined),
                            Variant::Dispatch(Some(id)) => {
                                let mode = match self.arena.get(*id) {
                                    Some(Object::Range(x)) => x.mode,
                                    _ => Mode::Cells,
                                };
                                let (_, s, rects) = self.range_areas(Some(v))?;
                                Ok((rects[0], mode, s))
                            }
                            _ => Err(OmError::app_defined()),
                        }
                    };
                    let (b1, m1, s1) = part(first)?;
                    let (b2, m2, s2) = part(second)?;
                    if s1 != s2 || s1 != sheet {
                        return Err(OmError::app_defined());
                    }
                    let both = moved_by(b1.union_bounds(&b2))?;
                    return self.sub_range(r, sheet, both, if m1 == m2 { m1 } else { Mode::Cells });
                }
                let Variant::Bstr(text) = first else {
                    return Err(OmError::app_defined());
                };
                if text.chars().count() > 255 {
                    return Err(OmError::app_defined());
                }
                let text = text.strip_prefix('=').unwrap_or(text);
                let (found, rels) =
                    parse_areas_text(wb, sheet, text).ok_or_else(OmError::app_defined)?;
                if found != sheet {
                    return Err(OmError::app_defined());
                }
                let moved = rels
                    .into_iter()
                    .map(moved_by)
                    .collect::<Result<Vec<_>, _>>()?;
                match moved.as_slice() {
                    [one] => self.sub_range(r, sheet, *one, Mode::Cells),
                    _ => self.new_multi(book, sheet, &moved, Mode::Cells),
                }
            }

            ("NumberFormatLocal", Get) => {
                self.same_format_or_null(book, &[(sheet, rect)], |wb, s| {
                    Variant::Bstr(com_format(wb, &s.number_format, 0))
                })
            }
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

            ("Item", Put) if a.get(0).is_some() || a.get(1).is_some() => {
                let Variant::Dispatch(Some(id)) = self.item(r, sheet, rect, a)? else {
                    return Err(OmError::app_defined());
                };
                let Some(Object::Range(cell)) = self.arena.get(id) else {
                    return Err(OmError::app_defined());
                };
                let cell = *cell;
                let (cs, crect) = self.resolve(&cell)?;
                self.put_values(&cell, cs, crect, a, Flavour::Value)
            }
            ("Value", Put) | ("_Default", Put) => {
                let out = self.put_values(r, sheet, rect, a, Flavour::Value)?;
                self.hyperlink_formula_style(book, sheet, rect, a)?;
                Ok(out)
            }
            ("Value2", Put) => {
                let out = self.put_values(r, sheet, rect, a, Flavour::Value2)?;
                self.hyperlink_formula_style(book, sheet, rect, a)?;
                Ok(out)
            }
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

                if matches!(value, Variant::Null) {
                    return Ok(Variant::Empty);
                }
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
                let ampm_lcid = if self.command_format_put { 0 } else { lcid };
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
                    wb.set_number_format_lcid(sheet, rect, &fmt, ampm_lcid);
                    let h = host(wb, &codes[0]);
                    wb.mark_custom_code(sheet, rect, &fmt, h);
                } else {
                    for (i, row) in (rect.r0..=rect.r1).enumerate() {
                        for (j, col) in (rect.c0..=rect.c1).enumerate() {
                            let k = (i % rows as usize) * cols as usize + j % cols as usize;
                            let fmt = engine(wb, &codes[k]);
                            wb.set_number_format_lcid(sheet, Rect::cell(row, col), &fmt, ampm_lcid);
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

                let operands = self
                    .range_operands(m.dispid, a)
                    .map_err(|_| OmError::app_defined())?;
                let refused = OmError::app_defined();
                self.run_command(book, a.lcid, operands, &refused, |app, o| {
                    let reader = Reader {
                        wb: &app.book(book)?.wb,
                        lcid: a.lcid,
                    };
                    let mut sides = [false; 4];
                    for (i, side) in sides.iter_mut().enumerate() {
                        let flag = reader.flag(o.get(i)).map_err(|_| OmError::app_defined())?;
                        *side = flag.unwrap_or(false);
                    }
                    app.range_create_names(book, sheet, rect, sides)?;
                    Ok(Variant::Bool(true))
                })
            }
            ("ClearOutline", Func) => {

                let rect = if rect.is_cell() { whole_sheet() } else { rect };
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
                wb.cfv_clear(sheet, rect, m.name == "Clear");
                if m.name == "Clear" {
                    wb.hl_clear(sheet, &rect);
                    wb.notes_clear(sheet, &rect);
                    wb.rich_clear(sheet, &rect);
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

                let rows = (on && rect.is_full_rows() && !rect.is_full_columns())
                    .then(|| self.whole_row_plan(book, sheet, rect))
                    .transpose()?
                    .flatten();
                let before: Vec<(u32, u32, xlcalc::style::Style)> = if on
                    && rows.is_none()
                    && u64::from(rect.rows()) * u64::from(rect.cols()) <= 1 << 16
                {
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
                let whole = if on && before.is_empty() && rows.is_none() {
                    self.whole_line_edges(book, sheet, rect)?
                } else {
                    None
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
                let outline = if before.is_empty() {
                    None
                } else {
                    Some(self.merge_outline(book, sheet, rect)?)
                };
                let v = self.range_view_member(r, sheet, rect, m, a)?;
                self.merge_resets_others(book, sheet, rect, &before, outline)?;
                if let Some(whole) = whole {
                    self.merge_resets_whole_lines(book, sheet, rect, &whole)?;
                }
                if let Some((area, plan)) = rows {
                    self.merge_resets_whole_rows(book, sheet, area, &plan)?;
                }

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

    fn value_index(&self, a: &Args) -> Result<i64, OmError> {
        if matches!(a.get(0), Some(Variant::Empty)) {
            return Ok(10);
        }
        self.coerce_variant_to_int(a, 0, 10, 13, 10)
            .map_err(|mut e| {
                if e.scode == crate::error::SCODE_TYPE_MISMATCH {
                    e.detail = Some(crate::app::INDEX_MISMATCH.into());
                }
                e
            })
    }

    fn value_put_kind(&self, a: &Args, areas: usize) -> Result<i64, OmError> {
        let kind = self.value_index(a)?;
        match kind {
            10 => Ok(10),
            11 => {
                if areas != 1 {
                    return Err(OmError::app_defined());
                }
                match &a.put {
                    None => Err(OmError::exception(0x8007_0057_u32 as i32, None)),
                    Some(v) if is_missing(v) => {
                        Err(OmError::exception(0x8007_0057_u32 as i32, None))
                    }
                    Some(Variant::Bstr(t)) if t.contains("<Workbook") => Err(OmError::exception(
                        crate::error::E_NOTIMPL.0,
                        Some("Range.Value(xlRangeValueXMLSpreadsheet) of a workbook is not ported"),
                    )),
                    Some(_) => Err(OmError::app_defined()),
                }
            }
            12 => Err(OmError::exception(crate::error::E_NOTIMPL.0, None)),
            _ => Err(OmError::app_defined()),
        }
    }

    fn offset_areas(
        &self,
        book: BookId,
        sheet: SheetId,
        areas: &[Rect],
        a: &Args,
    ) -> Result<Vec<Rect>, OmError> {
        let (rows, cols) = (i64::from(max_rows()), i64::from(max_cols()));
        let mut dr = self.coerce_variant_to_int(a, 0, -rows, rows, 0)?;
        let mut dc = self.coerce_variant_to_int(a, 1, -cols, cols, 0)?;
        let mut areas = areas.to_vec();
        let first = areas[0];
        let wb = &self.book(book)?.wb;
        if let Some(mc) = wb.merge_at(CellAddr::new(sheet, first.r0, first.c0)) {
            if first.is_cell() || first == mc {
                let (mut r0, mut c0) = (first.r0, first.c0);
                if dc > 0 {
                    c0 = mc.c1 + 1;
                    dc -= 1;
                }
                if dr > 0 {
                    r0 = mc.r1 + 1;
                    dr -= 1;
                }
                areas[0] = Rect::new(r0, c0, r0, c0);
            }
        }
        areas
            .iter()
            .map(|rect| {
                let r0 = u32::try_from(i64::from(rect.r0) + dr).ok()?;
                let r1 = u32::try_from(i64::from(rect.r1) + dr).ok()?;
                let c0 = u32::try_from(i64::from(rect.c0) + dc).ok()?;
                let c1 = u32::try_from(i64::from(rect.c1) + dc).ok()?;
                (r1 < max_rows() && c1 < max_cols()).then(|| Rect::new(r0, c0, r1, c1))
            })
            .collect::<Option<Vec<_>>>()
            .ok_or_else(OmError::app_defined)
    }

    fn address_text(
        &self,
        book: BookId,
        parts: &[(SheetId, Rect)],
        a: &Args,
    ) -> Result<Variant, OmError> {
        let flag = |i| a.bool(i).ok().flatten().unwrap_or(true);
        let r1c1 = a.i32(2)?.unwrap_or(1) != 1;
        let external = a.bool(3).ok().flatten().unwrap_or(false);
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

        let mut prefix = String::new();
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
            prefix = external_prefix(&b.name, &name);
        }
        let mut room = 257usize.saturating_sub(prefix.chars().count());
        let mut text = String::new();
        for (i, &(_, rect)) in parts.iter().enumerate() {
            let one: String = range_text(rect, &style).chars().take(255).collect();
            let need = one.chars().count() + usize::from(i > 0);
            if need > room {
                break;
            }
            room -= need;
            if i > 0 {
                text.push(',');
            }
            text.push_str(&one);
        }
        text.insert_str(0, &prefix);
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

    pub(crate) fn paste_destination(
        &self,
        book: BookId,
        sheet: SheetId,
        m: &'static Member,
        a: &Args,
    ) -> Result<Option<(SheetId, Rect)>, OmError> {
        let refused =
            || OmError::app_defined_with(&format!("{} method of Range class failed", m.name));
        match a.get(0) {
            None | Some(Variant::Null) => Ok(None),
            Some(Variant::Dispatch(Some(id))) => match self.arena.get(*id) {
                Some(Object::Range(dest)) => self.resolve(dest).map(Some),
                _ => Err(OmError::app_defined()),
            },
            Some(Variant::Bstr(text)) => text_destination(&self.book(book)?.wb, sheet, text)
                .map(Some)
                .ok_or_else(refused),
            Some(_) => Err(refused()),
        }
    }

    fn multi_copy(
        &mut self,
        book: BookId,
        sheet: SheetId,
        rects: &[Rect],
        m: &'static Member,
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
        self.clip_cut = false;
        self.clip_more = (rects[1..].to_vec(), stacked);
        let (ds, dr) = match self.paste_destination(book, sheet, m, a) {
            Ok(Some(dest)) => dest,
            Ok(None) => return Ok(Variant::Bool(true)),
            Err(e) => {
                self.clip = None;
                self.clip_more.0.clear();
                return Err(e);
            }
        };
        let mut off = 0;
        for src in rects {
            let at = if stacked {
                Rect::new(dr.r0 + off, dr.c0, dr.r0 + off, dr.c0)
            } else {
                Rect::new(dr.r0, dr.c0 + off, dr.r0, dr.c0 + off)
            };
            self.paste_copy(book, (sheet, *src), (ds, at), false, true)?;
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

            ("Insert" | "Delete", Func) => {
                let mut order: Vec<(RangeRef, Rect)> =
                    areas.iter().copied().zip(rects.iter().copied()).collect();
                order.sort_by_key(|(_, r)| std::cmp::Reverse((r.r0, r.c0)));
                let mut res = Variant::Bool(true);
                for (area, _) in &order {
                    res = self.range_member(area, m, a)?;
                }
                Ok(res)
            }

            ("PasteSpecial", Func) => {
                for area in &areas {
                    self.range_member(area, m, a)?;
                }
                Ok(Variant::Bool(true))
            }
            ("Copy", Func) => self.multi_copy(book, sheet, &rects, m, a),
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
            ("XPath", Get) => Err(OmError::exception(
                HResult::E_INVALIDARG.0,
                Some(XPATH_RANGE_INVALID),
            )),

            ("Font", Get) => Ok(self.alloc(Object::Font(StyleOwner::Areas(areas.clone())))),
            ("Interior", Get) => Ok(self.alloc(Object::Interior(StyleOwner::Areas(areas.clone())))),
            ("Borders", Get) if !indexed => {
                Ok(self.alloc(Object::Borders(StyleOwner::Areas(areas.clone()))))
            }
            ("Borders", Get) => {
                let i = a.i32(0)?.ok_or_else(OmError::app_defined)?;
                Ok(self.alloc(Object::Border(StyleOwner::Areas(areas.clone()), i)))
            }
            ("DisplayFormat", Get) => Ok(self.alloc(Object::DisplayFormat(first))),
            ("Phonetic", Get) => Ok(self.alloc(Object::Phonetic(first))),
            ("Phonetics", Get) => Ok(Variant::Dispatch(None)),
            ("Hyperlinks", Get) => Ok(self.alloc(Object::Hyperlinks(book, sheet, rects[0]))),
            ("UseStandardHeight" | "UseStandardWidth", _) | ("Summary", Get) => {
                self.use_standard_member(book, sheet, &rects, m, a)
            }
            ("TextToColumns" | "Sort", Func) => Err(OmError::app_defined_with(
                "This can't be done on a multiple range selection.\n\nSelect a single range and try again.",
            )),

            ("RemoveDuplicates", Func) => Err(OmError::app_defined()),

            ("Merge", Func) => {
                self.merge_across(a)?;
                for area in &areas {
                    self.range_member(area, m, a)?;
                }
                Ok(Variant::Empty)
            }
            ("FillDown" | "FillUp" | "FillLeft" | "FillRight", Func) => {
                if rects.first().is_some_and(|a| !fill_enabled(m.name, *a)) {
                    return Err(fill_failed(m.name));
                }

                if self.book(book)?.wb.filter_mode(sheet) {
                    for area in &areas {
                        self.range_member(area, m, a)?;
                    }
                    return Ok(Variant::Bool(true));
                }
                let (sorted, jobs) = multi_fill(m.name, &rects);
                self.book_mut(book)?
                    .wb
                    .command(xlcalc::Command::Fill, |wb| {

                        wb.note_changed(sheet, &sorted);
                        wb.creating_formulas(|wb| {
                            for (src, dst) in &jobs {
                                wb.fill(sheet, *src, *dst)?;
                            }
                            Ok(())
                        })
                    })
                    .map_err(edit_error)?;
                for (src, dst) in jobs {
                    self.carry_objects(book, (sheet, src), (sheet, dst), false)?;
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
            ("AddComment" | "AddCommentThreaded", Func) => {
                Err(OmError::exception(HResult::E_INVALIDARG.0, None))
            }
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
                            Rect::new(r.r0, 0, r.r1, max_cols() - 1)
                        } else {
                            Rect::new(0, r.c0, max_rows() - 1, r.c1)
                        }
                    })
                    .collect();
                let to = if rows { Mode::Rows } else { Mode::Columns };
                self.new_multi(book, sheet, &whole, to)
            }
            ("Offset", Get) => {
                let moved = self.offset_areas(book, sheet, &rects, a)?;
                self.new_multi(book, sheet, &moved, mode)
            }

            ("Item", Get) if a.get(0).is_none() && a.get(1).is_none() => {
                self.range_member(&first, m, a)
            }
            ("Resize" | "Item" | "ID", Get) => Err(OmError::app_defined()),
            ("Previous", Get) => Err(OmError::app_defined_with(
                "Unable to get the Previous property of the Range class",
            )),
            ("SavedAsArray", Get) => Err(OmError::app_defined()),

            ("FormatConditions", Get) => Ok(self.alloc(Object::FormatConditionsMulti(areas))),

            ("Validation", Get) => {
                if self.validation_exists(&first) {
                    return Ok(self.alloc(Object::ValidationMulti(first)));
                }
                self.range_member(&first, m, a)
            }
            (
                "Left" | "Top" | "Next" | "ListHeaderRows" | "LinkedDataTypeState"
                | "HasRichDataType" | "AllowEdit" | "PivotTable" | "PivotField" | "PivotItem"
                | "LocationInTable" | "QueryTable" | "PivotCell" | "ServerActions" | "CellControl"
                | "MDX" | "CommentThreaded",
                Get,
            ) => self.range_member(&first, m, a),

            ("Style", Get) => {
                let mut name: Option<Option<String>> = None;
                let mut v = Variant::Dispatch(None);
                for area in &areas {
                    let got = self.range_member(area, m, a)?;
                    let n = match &got {
                        Variant::Dispatch(Some(id)) => match self.arena.get(*id) {
                            Some(Object::Style(_, n)) => Some(n.to_string()),
                            _ => None,
                        },
                        _ => None,
                    };
                    if n.is_none() || name.as_ref().is_some_and(|p| *p != n) {
                        return Ok(Variant::Dispatch(None));
                    }
                    if name.is_none() {
                        v = got;
                    }
                    name = Some(n);
                }
                Ok(v)
            }
            ("Errors" | "FormulaLabel", Get) => Err(OmError::app_defined()),
            ("FormulaArray", Get) => Ok(Variant::Null),

            (
                "ColumnWidth" | "HorizontalAlignment" | "VerticalAlignment" | "Locked" | "FormulaHidden"
                | "NumberFormatLocal" | "IndentLevel" | "Orientation" | "ShrinkToFit" | "WrapText"
                | "RowHeight",
                Get,
            ) => {
                let mut seen: Option<Variant> = None;
                for area in &areas {
                    let v = self.range_member(area, m, a)?;
                    match &seen {
                        None => seen = Some(v),
                        Some(p) if *p == v => {}
                        Some(_) => return Ok(Variant::Null),
                    }
                }
                Ok(seen.unwrap_or(Variant::Null))
            }
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
                "AddIndent" | "Characters" | "Height" | "PrefixCharacter" | "Width",
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

            ("MergeCells", Get) => {
                let got = self.same_or_null_in(book, &parts, |wb, x| match wb.merge_at(x) {
                    Some(m) => Variant::Bstr(format!("{m:?}")),
                    None => Variant::Bool(false),
                })?;
                Ok(match got {
                    Variant::Bstr(_) => Variant::Bool(true),
                    v => v,
                })
            }
            ("HasFormula", Get) => self.same_or_null_in(book, &parts, |wb, x| {
                Variant::Bool(wb.formula_text(x).is_some())
            }),
            ("NumberFormat", Get) => self.same_format_or_null(book, &parts, |wb, s| {
                Variant::Bstr(com_format(wb, &s.number_format, a.lcid))
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

            ("Value" | "Value2" | "_Default", Put)
                if matches!(&a.put, Some(Variant::Bstr(t)) if t.starts_with('=')) =>
            {
                self.put_formulas_in(book, &parts, a, Entry::Legacy, false)
            }
            ("Value" | "Value2" | "_Default" | "NumberFormat", Put)
            | ("ClearContents" | "ClearFormats" | "Clear", Func) => {
                if (m.name, m.kind) == ("Value", Put) {
                    self.value_put_kind(a, areas.len())?;
                }
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

            ("PageBreak", Put) => self.range_member(&first, m, a),

            (
                "AddIndent" | "HorizontalAlignment" | "ID" | "IndentLevel" | "NumberFormatLocal"
                | "Orientation" | "ReadingOrder" | "ShrinkToFit" | "VerticalAlignment"
                | "WrapText" | "Style" | "Locked" | "FormulaHidden" | "MergeCells" | "RowHeight"
                | "ColumnWidth" | "Hidden",
                Put,
            ) => {

                if let ("Style", Some(Variant::Bstr(name))) = (m.name, &a.put) {
                    if self.book(book)?.wb.style_table().get(name).is_none() {
                        return Err(OmError::app_defined_with(&format!(
                            "Style '{name}' not found."
                        )));
                    }
                }
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

        if a.get(0).is_none() {
            return Err(failed());
        }
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

        if let Variant::Bstr(t) = &value {
            if !skip_locked && t.starts_with('=') && rect.cell_count() > 1 {
                return self.put_formulas_in(r.book, &[(sheet, rect)], a, Entry::Legacy, false);
            }
        }
        let (rows, cols, items) = array_cells(&value).ok_or_else(OmError::app_defined)?;
        let scalar = !matches!(value, Variant::Array(_));

        if !scalar && items.is_empty() {
            return Ok(Variant::Empty);
        }

        let target = rect;
        let lcid = a.lcid;

        let skipped: std::collections::HashSet<(u32, u32)> = if skip_locked {
            (target.r0..=target.r1)
                .flat_map(|row| (target.c0..=target.c1).map(move |col| (row, col)))
                .filter(|&(row, col)| self.any_locked(r.book, sheet, Rect::new(row, col, row, col)))
                .collect()
        } else {
            std::collections::HashSet::new()
        };
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
                if skipped.contains(&(row, col)) {
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
        let out = self.put_formulas_in(book, &[(sheet, rect)], a, entry, local)?;
        self.hyperlink_formula_style(book, sheet, rect, a)?;
        Ok(out)
    }

    fn hyperlink_formula_style(
        &mut self,
        book: BookId,
        sheet: SheetId,
        rect: Rect,
        a: &Args,
    ) -> Result<(), OmError> {
        let Some(Variant::Bstr(t)) = &a.put else {
            return Ok(());
        };
        let head: String = t
            .chars()
            .filter(|c| !c.is_whitespace())
            .take(11)
            .collect::<String>()
            .to_ascii_uppercase();
        if head == "=HYPERLINK(" {
            self.hyperlink_style(book, sheet, rect)?;
        }
        Ok(())
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

        if !scalar && items.is_empty() {
            return Ok(Variant::Empty);
        }
        let lcid = if local { HOST_LCID } else { a.lcid };
        let wb = &mut self.book_mut(book)?.wb;
        let &(sheet, _) = parts.first().ok_or_else(OmError::app_defined)?;
        let from = wb.notices().len();

        let several = parts.len() > 1 || parts.iter().any(|p| !p.1.is_cell());
        let put = |wb: &mut Workbook| {
            wb.creating_formulas(|wb| {
                put_formula_cells(wb, parts, &items, (rows, cols), scalar, entry, lcid)
            })
        };
        if several {
            wb.with_range_entry(put)?;
        } else {
            put(wb)?;
        }
        let rects: Vec<Rect> = parts.iter().map(|p| p.1).collect();
        merge_formula_changes(wb, from, sheet, &rects);
        Ok(Variant::Empty)
    }
}

pub(crate) fn used_area_differs<T: PartialEq>(
    wb: &Workbook,
    sheet: SheetId,
    rect: Rect,
    value: impl Fn(u32) -> T,
) -> bool {
    let used = |rows: bool| {
        let set = wb.populated_lines(sheet, rows);
        Some((*set.first()?, *set.last()?))
    };
    let ((r0, r1), (c0, c1)) = match (used(true), used(false)) {
        (Some(rows), Some(cols)) => (rows, cols),
        _ => return false,
    };
    let (cr0, cr1) = (rect.r0.max(r0), rect.r1.min(r1));
    let (cc0, cc1) = (rect.c0.max(c0), rect.c1.min(c1));
    if cr0 > cr1 || cc0 > cc1 || (cr0 == cr1 && cc0 == cc1) {
        return false;
    }
    let first = value(rect.c0);
    (cc0..=cc1).any(|c| value(c) != first)
}

fn put_formula_cells(
    wb: &mut Workbook,
    parts: &[(SheetId, Rect)],
    items: &[Variant],
    (rows, cols): (u32, u32),
    scalar: bool,
    entry: Entry,
    lcid: u32,
) -> Result<(), OmError> {
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
                    let before = wb.cell_style(addr).clone();
                    wb.copy_range(sheet, src, addr).map_err(edit_error)?;
                    let copied = wb.cell_style(addr).clone();
                    let kept = xlcalc::style::Style {
                        number_format: copied.number_format,
                        custom_code: copied.custom_code,
                        implicit_general: copied.implicit_general,
                        ..before
                    };
                    wb.set_style_with(sheet, Rect::cell(addr.row, addr.col), &|_| kept.clone());
                } else {
                    put_formula_cell(wb, addr, &items[0], entry, lcid)?;
                }
            }
        }
    } else {
        wb.bulk_entry(|wb| -> Result<(), OmError> {
            for &(sheet, rect) in parts {
                for (i, row) in (rect.r0..rect.r0 + rows).enumerate() {
                    for (j, col) in (rect.c0..rect.c0 + cols).enumerate() {
                        if row > rect.r1 || col > rect.c1 {
                            continue;
                        }
                        let addr = CellAddr::new(sheet, row, col);
                        let item = &items[i * cols as usize + j];

                        let blank = matches!(item, Variant::Bstr(s) if s.is_empty())
                            && !wb.has_own_entry(addr);
                        if blank {
                            continue;
                        }
                        put_formula_cell(wb, addr, item, entry, lcid)?;
                    }
                }
            }
            Ok(())
        })?;
    }
    Ok(())
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
            if t.starts_with('=') && t.chars().count() > 8192 {
                return Err(OmError::exception(crate::error::vba_scode(7), None));
            }
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

        Variant::Cy(_) => put_cell(wb, addr, v, Flavour::Value, lcid),
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

pub(crate) fn date_format(d: f64) -> &'static str {
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

pub(crate) fn host_spelled(wb: &Workbook, code: &str) -> bool {
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

pub(crate) fn com_format(wb: &Workbook, code: &str, lcid: u32) -> String {
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
    let built_in = ["h:mm AM/PM", "h:mm:ss AM/PM"].contains(&code);
    if built_in {
        code.replace("AM/PM", "am/pm")
    } else {
        code.to_string()
    }
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
        wb.set_value_entered(addr, Value::Number(n));
    };
    match v {
        Variant::Empty | Variant::Null => {
            wb.clear(addr);
        }
        Variant::Bool(b) => {
            wb.set_value_entered(addr, Value::Bool(*b));
        }

        Variant::Bstr(t) if t.starts_with('\'') => {
            wb.set_text_prefixed(addr, &t[1..])
                .map_err(|e| OmError::app_defined().with_detail(e.to_string()))?;
        }

        Variant::Bstr(t) => enter_text(wb, addr, t, true, lcid)?,
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
                let fmt = engine_format(wb, CURRENCY_FORMAT);
                wb.set_number_format_stored(addr.sheet, Rect::cell(addr.row, addr.col), &fmt);
            }
        }
        Variant::Cy(c) => number(wb, *c as f64 / 10_000.0),
        Variant::Error(code) => match scode_error(*code) {
            Some(e) => {
                wb.set_value(addr, Value::Error(e));
            }
            None => return Err(OmError::app_defined()),
        },

        Variant::Array(_) => {
            wb.clear(addr);
        }
        Variant::Dispatch(_) | Variant::Unknown(_) => return Err(OmError::app_defined()),
        other => match change_type(other, VarType::R8, Locale::EN_US, 0) {
            Ok(Variant::R8(n)) => number(wb, n),
            _ => return Err(OmError::type_mismatch()),
        },
    }
    Ok(())
}

fn grid_rect(r0: i64, c0: i64, r1: i64, c1: i64) -> Option<Rect> {
    let rows = 0..i64::from(max_rows());
    let cols = 0..i64::from(max_cols());
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

const TEXT_CAP: usize = 0x201e - 1;

fn cell_text_capped(wb: &xlcalc::Workbook, at: CellAddr) -> String {
    let mut s = wb.shown_text(at);

    wb.note_text_read(at);
    if let Some((i, _)) = s.char_indices().nth(TEXT_CAP) {
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

struct WholeLineEdges {
    anchor: xlcalc::style::Style,
    cells: Vec<(u32, u32, xlcalc::style::Style)>,
    line_diagonals: Vec<Option<(xlcalc::style::Border, bool, bool)>>,

    last_row: Option<u32>,
}

struct WholeRowPlan {
    anchor: xlcalc::style::Style,

    extent: u32,
    diagonal: Option<(xlcalc::style::Border, bool, bool)>,

    edge: Vec<(u32, u32, xlcalc::style::Style)>,

    keep_right: bool,
}

impl Application<'_> {
    fn whole_line_edges(
        &self,
        book: BookId,
        sheet: SheetId,
        rect: Rect,
    ) -> Result<Option<WholeLineEdges>, OmError> {
        if rect.is_full_columns() == rect.is_full_rows() {
            return Ok(None);
        }
        let wb = &self.book(book)?.wb;
        let mut at: Vec<(u32, u32)> = wb
            .formatted_cells(sheet)
            .into_iter()
            .filter(|&(r, c)| rect.contains(r, c))
            .collect();
        if let Some(sh) = wb.sheet(sheet) {
            at.extend(
                sh.entries()
                    .map(|(r, c, _)| (r, c))
                    .filter(|&(r, c)| rect.contains(r, c)),
            );
        }
        at.sort_unstable();
        at.dedup();
        let last_row = wb
            .used_range_with_formats(sheet)
            .map(|u| u.r1)
            .into_iter()
            .chain(at.iter().map(|&(r, _)| r))
            .max();
        let cells = at
            .into_iter()
            .map(|(r, c)| (r, c, wb.cell_style(CellAddr::new(sheet, r, c)).clone()))
            .collect();
        let line_diagonals = if rect.is_full_columns() {
            (rect.c0..=rect.c1)
                .map(|c| {
                    let b = &wb.line_style(sheet, false, c).borders;
                    (b.diagonal.line != xlcalc::style::BorderLine::None).then_some((
                        b.diagonal,
                        b.diagonal_down,
                        b.diagonal_up,
                    ))
                })
                .collect()
        } else {
            Vec::new()
        };
        Ok(Some(WholeLineEdges {
            anchor: wb
                .cell_style(CellAddr::new(sheet, rect.r0, rect.c0))
                .clone(),
            cells,
            line_diagonals,
            last_row,
        }))
    }

    fn merge_resets_whole_lines(
        &mut self,
        book: BookId,
        sheet: SheetId,
        rect: Rect,
        whole: &WholeLineEdges,
    ) -> Result<(), OmError> {
        use xlcalc::style::{Border, BorderLine, Style};
        let cols = rect.is_full_columns();
        let wb = &mut self.book_mut(book)?.wb;
        let anchor_alignment = whole.anchor.alignment.clone();

        let line_diagonal = |col: u32| {
            whole
                .line_diagonals
                .get((col - rect.c0) as usize)
                .copied()
                .flatten()
        };
        let diagonal_uniform = whole.anchor.borders.diagonal.line != BorderLine::None
            && whole.cells.iter().all(|(_, _, s)| {
                s.borders.diagonal.line == BorderLine::None
                    || (s.borders.diagonal == whole.anchor.borders.diagonal
                        && s.borders.diagonal_down == whole.anchor.borders.diagonal_down
                        && s.borders.diagonal_up == whole.anchor.borders.diagonal_up)
            });
        let lines = if cols {
            rect.c0..=rect.c1
        } else {
            rect.r0..=rect.r1
        };
        for i in lines {
            let slice = if cols {
                Rect::new(rect.r0, i, rect.r1, i)
            } else {
                Rect::new(i, rect.c0, i, rect.c1)
            };
            wb.set_style_with(sheet, slice, &|old: &Style| {
                let mut s = Style::default();
                if old.alignment == anchor_alignment {
                    s.alignment = old.alignment.clone();
                }
                s
            });
        }
        if let Some(last) = whole.last_row.filter(|&l| cols && l >= rect.r0) {
            for c in rect.c0..=rect.c1 {
                if let Some((d, down, up)) = line_diagonal(c) {
                    wb.set_style_with(sheet, Rect::new(rect.r0, c, last, c), &|s: &Style| {
                        let mut s = s.clone();
                        s.borders.diagonal = d;
                        s.borders.diagonal_down = down;
                        s.borders.diagonal_up = up;
                        s
                    });
                }
            }
        }
        for (row, col, old) in &whole.cells {
            let none = Border::default();
            let keep = |edge: bool, b: Border| if edge { b } else { none };
            let side_line = [
                old.borders.left,
                old.borders.right,
                old.borders.top,
                old.borders.bottom,
            ]
            .iter()
            .any(|b| b.line != BorderLine::None);
            wb.set_style_with(sheet, Rect::cell(*row, *col), &|s: &Style| {
                let mut s = s.clone();
                if let Some((d, down, up)) = line_diagonal(*col) {
                    s.borders.diagonal = d;
                    s.borders.diagonal_down = down;
                    s.borders.diagonal_up = up;
                } else if diagonal_uniform && side_line {
                    s.borders.diagonal = old.borders.diagonal;
                    s.borders.diagonal_down = old.borders.diagonal_down;
                    s.borders.diagonal_up = old.borders.diagonal_up;
                }
                s.borders.left = keep(*col == rect.c0, old.borders.left);
                s.borders.right = keep(*col == rect.c1, old.borders.right);
                s.borders.top = keep(*row == rect.r0, old.borders.top);
                s.borders.bottom = keep(*row == rect.r1, old.borders.bottom);
                s
            });
        }

        let mut anchor = whole.anchor.clone();
        let anchor_side = [
            anchor.borders.left,
            anchor.borders.right,
            anchor.borders.top,
            anchor.borders.bottom,
        ]
        .iter()
        .any(|b| b.line != BorderLine::None);
        if let Some((d, down, up)) = line_diagonal(rect.c0) {
            anchor.borders.diagonal = d;
            anchor.borders.diagonal_down = down;
            anchor.borders.diagonal_up = up;
        } else if !(diagonal_uniform && anchor_side) {
            anchor.borders.diagonal = Border::default();
            anchor.borders.diagonal_down = false;
            anchor.borders.diagonal_up = false;
        }
        if rect.c1 > rect.c0 {
            anchor.borders.right = Border::default();
        }
        if rect.r1 > rect.r0 {
            anchor.borders.bottom = Border::default();
        }
        wb.set_style_with(sheet, Rect::cell(rect.r0, rect.c0), &|_| anchor.clone());
        Ok(())
    }

    fn whole_row_plan(
        &self,
        book: BookId,
        sheet: SheetId,
        rect: Rect,
    ) -> Result<Option<(Rect, WholeRowPlan)>, OmError> {
        use xlcalc::style::BorderLine;
        let wb = &self.book(book)?.wb;
        let mut grown = rect;
        loop {
            let next = wb
                .merges(sheet)
                .into_iter()
                .filter(|m| m.intersects(&grown))
                .fold(grown, |x, m| x.union_bounds(&m));
            if next == grown {
                break;
            }
            grown = next;
        }
        if grown != rect {
            return Ok(grown.is_full_columns().then(|| {
                (
                    grown,
                    WholeRowPlan {
                        anchor: wb.cell_style(CellAddr::new(sheet, 0, 0)).clone(),
                        extent: 0,
                        diagonal: None,
                        edge: Vec::new(),
                        keep_right: false,
                    },
                )
            }));
        }
        let (b0, b1) = (rect.r0 / 16 * 16, rect.r1 / 16 * 16 + 15);
        let mut records: Vec<(u32, u32)> = wb
            .formatted_cells(sheet)
            .into_iter()
            .filter(|&(r, _)| (b0..=b1).contains(&r))
            .collect();
        let mut has_value = false;
        records.extend(
            self.emptied
                .iter()
                .filter(|&&(b, s, r, _)| b == book && s == sheet && (b0..=b1).contains(&r))
                .map(|&(_, _, r, c)| (r, c)),
        );
        if let Some(sh) = wb.sheet(sheet) {
            for (r, c, _) in sh.entries() {
                if (b0..=b1).contains(&r) {
                    records.push((r, c));
                    has_value |= (rect.r0..=rect.r1).contains(&r);
                }
            }
        }
        let extent = if has_value {
            records.iter().map(|&(_, c)| c).max().unwrap_or(0)
        } else {
            0
        };
        let diag = |b: &xlcalc::style::Borders| (b.diagonal, b.diagonal_down, b.diagonal_up);
        let line = diag(&wb.line_style(sheet, true, rect.r0).borders);
        let uniform = line.0.line != BorderLine::None
            && (rect.r0..=rect.r1).all(|r| diag(&wb.line_style(sheet, true, r).borders) == line)
            && records
                .iter()
                .filter(|&&(r, _)| (rect.r0..=rect.r1).contains(&r))
                .all(|&(r, c)| {
                    let b = &wb.cell_style(CellAddr::new(sheet, r, c)).borders;
                    b.diagonal.line == BorderLine::None || diag(b) == line
                });
        let style = |r: u32, c: u32| (r, c, wb.cell_style(CellAddr::new(sheet, r, c)).clone());
        let mut edge: Vec<_> = (rect.r0..=rect.r1)
            .flat_map(|r| [style(r, 0), style(r, extent)])
            .chain((0..=extent).flat_map(|c| [style(rect.r0, c), style(rect.r1, c)]))
            .collect();
        edge.sort_by_key(|&(r, c, _)| (r, c));
        edge.dedup_by_key(|e| (e.0, e.1));
        Ok(Some((
            rect,
            WholeRowPlan {
                anchor: wb.cell_style(CellAddr::new(sheet, rect.r0, 0)).clone(),
                extent,
                diagonal: uniform.then_some(line),
                edge,
                keep_right: wb.ever_used(sheet).is_none_or(|u| u.c1 <= extent),
            },
        )))
    }

    fn merge_resets_whole_rows(
        &mut self,
        book: BookId,
        sheet: SheetId,
        rect: Rect,
        plan: &WholeRowPlan,
    ) -> Result<(), OmError> {
        use xlcalc::style::{Border, Style};
        let wb = &mut self.book_mut(book)?.wb;
        let align = plan.anchor.alignment.clone();
        let reset = |old: &Style| {
            let mut s = Style::default();
            if old.alignment == align {
                s.alignment = old.alignment.clone();
            }
            s
        };
        wb.set_style_with_merged(sheet, rect, &reset);
        if let Some((d, down, up)) = plan.diagonal {
            wb.set_style_with(
                sheet,
                Rect::new(rect.r0, 0, rect.r1, plan.extent),
                &|s: &Style| {
                    let mut s = s.clone();
                    s.borders.diagonal = d;
                    s.borders.diagonal_down = down;
                    s.borders.diagonal_up = up;
                    s
                },
            );
        }
        for (r, c, old) in &plan.edge {
            wb.set_style_with(sheet, Rect::cell(*r, *c), &|s: &Style| {
                let mut s = s.clone();
                if *c == 0 {
                    s.borders.left = old.borders.left;
                }
                if *c == plan.extent && plan.keep_right {
                    s.borders.right = old.borders.right;
                }
                if *r == rect.r0 {
                    s.borders.top = old.borders.top;
                }
                if *r == rect.r1 {
                    s.borders.bottom = old.borders.bottom;
                }
                s
            });
        }
        let mut anchor = plan.anchor.clone();
        anchor.borders.right = Border::default();
        if rect.r1 > rect.r0 {
            anchor.borders.bottom = Border::default();
        }
        let (d, down, up) = plan.diagonal.unwrap_or_default();
        anchor.borders.diagonal = d;
        anchor.borders.diagonal_down = down;
        anchor.borders.diagonal_up = up;
        wb.set_style_with(sheet, Rect::cell(rect.r0, 0), &|_| anchor.clone());
        Ok(())
    }

    fn merge_outline(
        &self,
        book: BookId,
        sheet: SheetId,
        rect: Rect,
    ) -> Result<[xlcalc::style::Border; 4], OmError> {
        use xlcalc::style::{Border, Edge};
        let wb = &self.book(book)?.wb;
        let side = |edge: Edge, cells: &mut dyn Iterator<Item = (u32, u32)>| -> Border {
            let mut lines = cells.map(|(r, c)| wb.shown_border(CellAddr::new(sheet, r, c), edge));
            let first = lines.next().unwrap_or_default();
            if lines.all(|b| b == first) {
                first
            } else {
                Border::default()
            }
        };
        Ok([
            side(Edge::Left, &mut (rect.r0..=rect.r1).map(|r| (r, rect.c0))),
            side(Edge::Right, &mut (rect.r0..=rect.r1).map(|r| (r, rect.c1))),
            side(Edge::Top, &mut (rect.c0..=rect.c1).map(|c| (rect.r0, c))),
            side(Edge::Bottom, &mut (rect.c0..=rect.c1).map(|c| (rect.r1, c))),
        ])
    }

    fn merge_resets_others(
        &mut self,
        book: BookId,
        sheet: SheetId,
        rect: Rect,
        before: &[(u32, u32, xlcalc::style::Style)],
        outline: Option<[xlcalc::style::Border; 4]>,
    ) -> Result<(), OmError> {
        let Some((_, _, anchor)) = before.first() else {
            return Ok(());
        };
        let anchor_alignment = anchor.alignment.clone();
        let diagonal_uniform = anchor.borders.diagonal.line != xlcalc::style::BorderLine::None
            && before.iter().all(|(_, _, s)| {
                s.borders.diagonal == anchor.borders.diagonal
                    && s.borders.diagonal_down == anchor.borders.diagonal_down
                    && s.borders.diagonal_up == anchor.borders.diagonal_up
            });

        let has_side_line: Vec<bool> = {
            use xlcalc::style::Edge;
            let wb = &self.book(book)?.wb;
            before
                .iter()
                .map(|(row, col, _)| {
                    [Edge::Left, Edge::Right, Edge::Top, Edge::Bottom]
                        .into_iter()
                        .any(|e| {
                            wb.shown_border(CellAddr::new(sheet, *row, *col), e).line
                                != xlcalc::style::BorderLine::None
                        })
                })
                .collect()
        };
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

            let [out_l, out_r, out_t, out_b] = outline.unwrap_or([
                old.borders.left,
                old.borders.right,
                old.borders.top,
                old.borders.bottom,
            ]);
            new.borders.left = if *col == rect.c0 { out_l } else { none };
            new.borders.right = if *col == rect.c1 { out_r } else { none };
            new.borders.top = if *row == rect.r0 { out_t } else { none };
            new.borders.bottom = if *row == rect.r1 { out_b } else { none };
            if i == 0 {
                new.borders.diagonal = old.borders.diagonal;
            } else if diagonal_uniform && has_side_line[i] {
                new.borders.diagonal = old.borders.diagonal;
                new.borders.diagonal_down = old.borders.diagonal_down;
                new.borders.diagonal_up = old.borders.diagonal_up;
            }
            wb.set_style_with(sheet, Rect::cell(*row, *col), &|_| new.clone());
        }
        Ok(())
    }
}

pub(crate) enum Vifmt {
    Exact,

    Reset,
    Leaves,
}

pub(crate) fn vifmt_effect(name: &str, kind: InvokeKind) -> Vifmt {
    use InvokeKind::*;
    match (name, kind) {
        (
            "NumberFormat"
            | "NumberFormatLocal"
            | "HorizontalAlignment"
            | "VerticalAlignment"
            | "WrapText"
            | "IndentLevel"
            | "Orientation"
            | "ShrinkToFit"
            | "Locked"
            | "FormulaHidden"
            | "MergeCells"
            | "Style",
            Get,
        )
        | (
            "HorizontalAlignment"
            | "VerticalAlignment"
            | "WrapText"
            | "IndentLevel"
            | "Orientation"
            | "ShrinkToFit"
            | "Locked"
            | "FormulaHidden",
            Put,
        ) => Vifmt::Exact,

        (
            "Replace" | "TextToColumns" | "GoalSeek" | "AutoFilter" | "FormulaArray" | "Insert",
            _,
        ) => Vifmt::Reset,
        _ => Vifmt::Leaves,
    }
}

impl Application<'_> {
    pub(crate) fn note_style_owner_format(&mut self, o: &crate::style::StyleOwner) {
        let r = match o {
            crate::style::StyleOwner::Range(r) => r,
            crate::style::StyleOwner::Areas(rs) => match rs.first() {
                Some(r) => r,
                None => return,
            },
            _ => return,
        };
        if let Ok((sheet, rect)) = self.resolve(r) {
            if let Ok(b) = self.book_mut(r.book) {
                b.wb.note_exact_cell_format(CellAddr::new(sheet, rect.r0, rect.c0));
            }
        }
    }
}

fn text_destination(wb: &Workbook, sheet: SheetId, text: &str) -> Option<(SheetId, Rect)> {
    let (row, col) = wb.active_cell_of(sheet);
    let body = text.strip_prefix('=').unwrap_or(text);
    let a1 = r1c1_to_a1(&format!("={body}"), row, col)?;
    parse_range_text(wb, sheet, a1.strip_prefix('=')?)
}

fn fill_enabled(name: &str, first: Rect) -> bool {
    let (from, to, edge) = match name {
        "FillDown" => (first.r0, first.r1, 0),
        "FillUp" => (first.r0, first.r1, max_rows() - 1),
        "FillRight" => (first.c0, first.c1, 0),
        _ => (first.c0, first.c1, max_cols() - 1),
    };
    from != edge || to != edge
}

fn fill_failed(name: &str) -> OmError {
    OmError::app_defined_with(&format!("{name} method of Range class failed"))
}

fn multi_fill(name: &str, rects: &[Rect]) -> (Vec<Rect>, Vec<(Rect, Rect)>) {
    let vertical = matches!(name, "FillDown" | "FillUp");
    let backwards = matches!(name, "FillUp" | "FillLeft");
    let mut areas = rects.to_vec();
    areas.sort_by_key(|a| (a.r0, a.c0));

    let split = |a: Rect| -> Option<(Rect, Rect)> {
        let (lo, hi) = if vertical { (a.r0, a.r1) } else { (a.c0, a.c1) };
        if lo == hi {
            return None;
        }
        let (s, from, to) = if backwards {
            (hi, lo, hi - 1)
        } else {
            (lo, lo + 1, hi)
        };
        Some(if vertical {
            (Rect::new(s, a.c0, s, a.c1), Rect::new(from, a.c0, to, a.c1))
        } else {
            (Rect::new(a.r0, s, a.r1, s), Rect::new(a.r0, from, a.r1, to))
        })
    };

    let first = areas[0];
    let mut across = 1;
    while across < areas.len()
        && areas[across].r0 == first.r0
        && areas[across].r1 == first.r1
        && areas[across].c0 > areas[across - 1].c1
    {
        across += 1;
    }
    let grid = areas.len().is_multiple_of(across)
        && areas.chunks(across).all(|row| {
            row.iter().zip(&areas).all(|(a, top)| {
                (a.c0, a.c1) == (top.c0, top.c1) && (a.r0, a.r1) == (row[0].r0, row[0].r1)
            })
        })
        && areas
            .chunks(across)
            .zip(areas.chunks(across).skip(1))
            .all(|(above, below)| below[0].r0 > above[0].r1);
    if !grid {
        let jobs = areas.iter().filter_map(|&a| split(a)).collect();
        return (areas, jobs);
    }
    let lines: Vec<Vec<Rect>> = if vertical {
        (0..across)
            .map(|i| areas.iter().skip(i).step_by(across).copied().collect())
            .collect()
    } else {
        areas.chunks(across).map(<[Rect]>::to_vec).collect()
    };
    let mut jobs = Vec::new();
    for mut line in lines {
        if backwards {
            line.reverse();
        }
        let src = match split(line[0]) {
            Some((src, rest)) => {
                jobs.push((src, rest));
                src
            }
            None => line[0],
        };
        jobs.extend(line[1..].iter().map(|&a| (src, a)));
    }
    (areas, jobs)
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

    #[test]
    fn fill_is_disabled_on_the_edge_it_would_fill_from() {
        let (last_r, last_c) = (max_rows() - 1, max_cols() - 1);
        let r = Rect::new;
        assert!(!fill_enabled("FillDown", r(0, 0, 0, 3)));
        assert!(fill_enabled("FillDown", r(0, 0, 1, 0)));
        assert!(fill_enabled("FillDown", r(1, 0, 1, 0)));
        assert!(!fill_enabled("FillRight", r(0, 0, 3, 0)));
        assert!(fill_enabled("FillRight", r(0, 0, 0, 1)));
        assert!(!fill_enabled("FillUp", r(last_r, 0, last_r, 0)));
        assert!(fill_enabled("FillUp", r(last_r - 1, 0, last_r, 0)));
        assert!(!fill_enabled("FillLeft", r(0, last_c, 0, last_c)));
        assert!(fill_enabled("FillLeft", r(0, 0, 0, 0)));
    }

    #[test]
    fn fill_of_a_grid_of_areas_runs_from_its_first_area() {
        let c = Rect::cell;
        let jobs = |name, areas: &[Rect]| multi_fill(name, areas).1;

        assert_eq!(
            jobs("FillDown", &[c(4, 0), c(0, 0), c(2, 0)]),
            [(c(0, 0), c(2, 0)), (c(0, 0), c(4, 0))]
        );
        assert_eq!(jobs("FillUp", &[c(0, 0), c(2, 0)]), [(c(2, 0), c(0, 0))]);

        let (a, b) = (Rect::new(0, 0, 1, 0), Rect::new(3, 0, 4, 0));
        assert_eq!(
            jobs("FillDown", &[a, b]),
            [(c(0, 0), c(1, 0)), (c(0, 0), b)]
        );

        assert_eq!(jobs("FillDown", &[c(1, 0), c(3, 2)]), []);
        assert_eq!(jobs("FillDown", &[a, c(4, 2)]), [(c(0, 0), c(1, 0))]);
    }
}
