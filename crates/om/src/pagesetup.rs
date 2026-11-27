use std::collections::HashMap;

use xlcalc::{CellAddr, Evaluated, Rect, SheetId};
use xlcalc_variant::{SafeArray, VarType, Variant};

use crate::addr::{parse_areas_text, quote_sheet, range_text, AddressStyle};
use crate::app::{unsupported, Application, BookId, Object};
use crate::conv::Args;
use crate::error::OmError;
use crate::typelib::{InvokeKind, Member};

const CREATOR: i32 = 0x5843_454C;
const AUTOMATIC: i32 = -4105;
const NO_COMMENTS: i32 = -4142;

#[derive(Clone, Debug, PartialEq)]
pub(crate) struct PageSetup {
    pub align_margins: bool,
    pub black_and_white: bool,
    pub bottom: f64,

    pub footer: String,
    pub header: String,
    pub center_horizontally: bool,
    pub center_vertically: bool,
    pub different_first: bool,
    pub draft: bool,
    pub first_page: Option<f64>,
    pub fit_tall: Option<i32>,
    pub fit_wide: Option<i32>,
    pub footer_margin: f64,
    pub header_margin: f64,
    pub left: f64,
    pub odd_even: bool,
    pub order: f64,
    pub orientation: f64,
    pub paper_size: f64,
    pub comments: i32,
    pub errors: i32,
    pub gridlines: bool,
    pub headings: bool,
    pub right: f64,
    pub scale_with_doc: bool,
    pub top: f64,
    pub zoom: Option<i32>,

    pub list_zoom: i32,

    pub list_sig: u64,

    pub list_win: Option<(u32, u32)>,

    pub slv_cached: bool,

    pub slv_zoomed: bool,
}

impl Default for PageSetup {
    fn default() -> Self {
        PageSetup {
            align_margins: true,
            black_and_white: false,
            bottom: 54.0,
            footer: String::new(),
            header: String::new(),
            center_horizontally: false,
            center_vertically: false,
            different_first: false,
            draft: false,
            first_page: None,
            fit_tall: Some(1),
            fit_wide: Some(1),
            footer_margin: 0.3 * 72.0,
            header_margin: 0.3 * 72.0,
            left: 50.4,
            odd_even: false,
            order: 1.0,
            orientation: 1.0,
            paper_size: 1.0,
            comments: NO_COMMENTS,
            errors: 0,
            gridlines: false,
            headings: false,
            right: 50.4,
            scale_with_doc: true,
            top: 54.0,
            zoom: Some(100),
            list_zoom: 100,
            list_sig: 0,
            list_win: None,
            slv_cached: false,
            slv_zoomed: false,
        }
    }
}

pub(crate) type BookPageSetup = HashMap<SheetId, PageSetup>;

const PRINT_AREA: &str = "PRINT_AREA";
const PRINT_TITLES: &str = "PRINT_TITLES";

pub(crate) fn paper_ok(n: i32) -> bool {
    matches!(n, 1 | 3 | 5..=9 | 11..=13 | 19 | 20 | 24..=28 | 30 | 34 | 37 | 43 | 66 | 70..=74 | 91 | 119..=131)
}

fn enum_put(a: &Args, allowed: &[i32]) -> Result<Option<f64>, OmError> {
    let v = match a.get(0) {
        None => return Ok(None),
        Some(v) if crate::conv::is_missing(v) || matches!(v, Variant::Null) => return Ok(None),
        Some(Variant::Empty) => Variant::R8(0.0),
        Some(v) => v.clone(),
    };
    let one = Args {
        values: vec![Some(v)],
        put: None,
        lcid: a.lcid,
    };
    let n = one.f64(0)?.ok_or_else(OmError::type_mismatch)?.floor();
    if !allowed.is_empty() && !allowed.contains(&(n as i32)) {
        return Err(OmError::app_defined());
    }
    Ok(Some(n))
}

fn first_element(a: &Args) -> Args {
    let mut v = a.get(0);
    while let Some(Variant::Array(arr)) = v {
        match arr.data.first() {
            Some(e) => v = Some(e),
            None => break,
        }
    }
    Args {
        values: vec![v.cloned()],
        put: None,
        lcid: a.lcid,
    }
}

fn flag(a: &Args, draft: bool, out: &mut bool) -> Result<(), ()> {
    match a.get(0) {
        None | Some(Variant::Empty) => *out = false,
        Some(v) if crate::conv::is_missing(v) => *out = false,
        Some(Variant::Null) => {}
        Some(Variant::Bool(b)) => *out = *b,
        Some(Variant::Bstr(t)) => {
            *out = if draft {
                match t.to_ascii_lowercase().as_str() {
                    "true" => true,
                    "false" => false,
                    _ => return Err(()),
                }
            } else {
                t.trim().parse::<f64>().map_err(|_| ())? != 0.0
            }
        }
        Some(Variant::Cy(_) | Variant::Date(_)) if draft => return Err(()),
        Some(v) => {
            let one = Args {
                values: vec![Some(v.clone())],
                put: None,
                lcid: a.lcid,
            };
            match one.f64(0) {
                Ok(Some(x)) => *out = x != 0.0,
                _ => return Err(()),
            }
        }
    }
    Ok(())
}

fn titles_text(
    a: &Args,
    string: &dyn Fn(&Args) -> Result<String, OmError>,
    fail: &dyn Fn() -> OmError,
) -> Result<String, OmError> {
    match a.get(0) {
        None | Some(Variant::Null | Variant::Empty | Variant::Bool(false)) => Ok(String::new()),
        Some(v) if crate::conv::is_missing(v) => Ok(String::new()),
        Some(Variant::Bool(true)) => Err(fail()),
        _ => string(a),
    }
}

fn abs_style() -> AddressStyle {
    AddressStyle {
        row_abs: true,
        col_abs: true,
        r1c1: false,
        relative_to: (0, 0),
    }
}

const MAX_AREA_TEXT: usize = 255;

fn minimize_sqref(mut v: Vec<Rect>) -> Vec<Rect> {
    let inside = |x: Rect, y: Rect| x.r0 <= y.r0 && x.r1 >= y.r1 && x.c0 <= y.c0 && x.c1 >= y.c1;
    let mut i = v.len();
    while i > 0 {
        i -= 1;
        let mut j = v.len();
        while j > 0 {
            j -= 1;
            if j == i {
                continue;
            }
            let (a, o) = (v[i], v[j]);
            let side = a.r0 == o.r0 && a.r1 == o.r1 && o.c1 + 1 >= a.c0 && a.c1 + 1 >= o.c0;
            let stack = a.c0 == o.c0 && a.c1 == o.c1 && o.r1 + 1 >= a.r0 && a.r1 + 1 >= o.r0;
            if side || stack || inside(a, o) || inside(o, a) {
                v[i] = Rect::new(
                    a.r0.min(o.r0),
                    a.c0.min(o.c0),
                    a.r1.max(o.r1),
                    a.c1.max(o.c1),
                );
                v.remove(j);
                if j < i {
                    i -= 1;
                }

                j = v.len();
            }
        }
    }
    v
}

fn stored_points(v: f64) -> f64 {
    let r = v / 72.0 * 72.0;
    if r.is_finite() {
        r
    } else {
        0.0
    }
}

fn hf_marker(c: u16) -> Option<usize> {
    match u8::try_from(c).ok()?.to_ascii_uppercase() {
        b'L' => Some(0),
        b'C' => Some(1),
        b'R' => Some(2),
        _ => None,
    }
}

const HF_ROOM: usize = 255;

fn hf_munge(src: &[u16], which: usize, replace: Option<&[u16]>) -> Option<Vec<u16>> {
    const AMP: u16 = b'&' as u16;
    let set = replace.is_some();

    let mut copy = if which == 1 { !set } else { set };
    let mut out: Vec<u16> = Vec::new();

    let mut escape_ok = true;
    let mut pos = 0;
    while pos < src.len() && out.len() < HF_ROOM {
        let c = src[pos];
        let mut plain = true;
        if let Some(n) = src.get(pos + 1).copied().filter(|_| escape_ok && c == AMP) {
            if n == AMP {
                escape_ok = false;
                plain = false;
            } else if let Some(k) = hf_marker(n) {
                if set {
                    copy = k != which;
                } else {
                    copy = false;
                    if k == which {
                        pos += 2;
                        copy = true;
                    }
                }
            }
        }
        if plain {
            escape_ok = true;
        }
        if copy && pos < src.len() {
            out.push(src[pos]);
        }
        pos += 1;
    }
    if pos < src.len() {
        return Some(src.to_vec());
    }
    if let Some(new) = replace.filter(|n| !n.is_empty()) {
        if HF_ROOM - out.len() < new.len() + 2 {
            return None;
        }
        out.push(AMP);
        out.push(u16::from(b"LCR"[which]));
        out.extend_from_slice(new);
    }
    Some(out)
}

fn hf_part(stored: &str, which: usize) -> String {
    let src: Vec<u16> = xlcalc::wide::units(stored).collect();
    hf_munge(&src, which, None).map_or_else(String::new, |v| xlcalc::wide::from_units(&v))
}

fn hf_replace(stored: &str, which: usize, text: &str) -> Option<String> {
    let new: Vec<u16> = xlcalc::wide::units(text).collect();
    if new.len() > HF_ROOM {
        return None;
    }
    let src: Vec<u16> = xlcalc::wide::units(stored).collect();
    hf_munge(&src, which, Some(&new)).map(|v| xlcalc::wide::from_units(&v))
}

fn hf_text(a: &Args) -> Result<String, OmError> {
    match a.get(0) {
        None | Some(Variant::Null | Variant::Empty) => Ok(String::new()),
        Some(v) if crate::conv::is_missing(v) => Ok(String::new()),
        Some(Variant::Array(arr)) => match arr.data.first() {
            Some(v) => hf_scalar(a, v),
            None => Ok(String::new()),
        },
        Some(v) => hf_scalar(a, v),
    }
}

fn hf_scalar(a: &Args, v: &Variant) -> Result<String, OmError> {
    match v {
        Variant::Cy(c) => {
            let sign = if *c < 0 { "-" } else { "" };
            let cents = (c.unsigned_abs() + 50) / 100;
            Ok(format!("{sign}${}.{:02} ", cents / 100, cents % 100))
        }
        v => a.variant_string(v)?.ok_or_else(OmError::type_mismatch),
    }
}

pub(crate) fn first_page_value(n: f64) -> Option<f64> {
    if n >= 2_147_483_647.0 {
        Some(2_147_483_647.0)
    } else if n.abs() == 32_768.0 {
        None
    } else {
        Some(n)
    }
}

fn set_fail(prop: &str) -> OmError {
    OmError::unable_to_set(prop, "PageSetup")
}

impl Application<'_> {
    pub(crate) fn page_setup(&self, b: BookId, s: SheetId) -> PageSetup {
        let Ok(book) = self.book(b) else {
            return PageSetup::default();
        };
        let mut p = book.page_setup.get(&s).cloned().unwrap_or_default();

        let sig = crate::pagegeom::layout_signature(&book.wb, s);
        if p.list_sig != sig {
            p.list_zoom = p.zoom.unwrap_or(0);
            p.list_sig = sig;
            p.list_win = Some(self.break_window(b, s));
        }
        p
    }

    fn name_parts(&self, b: BookId, s: SheetId, key: &str) -> Vec<String> {
        let Ok(book) = self.book(b) else {
            return Vec::new();
        };
        let Some(def) = book.wb.names().get(key, Some(s)).cloned() else {
            return Vec::new();
        };
        book.wb
            .name_refers_to(&def)
            .split(',')
            .map(|p| p.rsplit_once('!').map_or(p, |x| x.1).to_string())
            .collect()
    }

    pub(crate) fn refresh_break_list(&mut self, b: BookId, s: SheetId) {
        let win = self.break_window(b, s);
        if let Some(p) = self.book_mut(b).ok().and_then(|b| b.page_setup.get_mut(&s)) {
            p.list_zoom = p.zoom.unwrap_or(0);
            p.list_win = Some(win);
        }
    }

    fn set_name_parts(&mut self, b: BookId, s: SheetId, key: &str, parts: &[String]) {
        self.refresh_break_list(b, s);
        let Ok(book) = self.book_mut(b) else {
            return;
        };
        if parts.is_empty() {
            book.wb.delete_name(key, Some(s));
            return;
        }
        let Some(sheet) = book.wb.sheet(s) else {
            return;
        };
        let prefix = quote_sheet(&sheet.name);
        let refers = parts
            .iter()
            .map(|p| format!("{prefix}!{p}"))
            .collect::<Vec<_>>()
            .join(",");
        let display = if key == PRINT_AREA {
            "Print_Area"
        } else {
            "Print_Titles"
        };
        let _ = book.wb.define_name(display, &format!("={refers}"), Some(s));
    }

    fn title_part(&self, b: BookId, s: SheetId, rows: bool) -> String {
        self.name_parts(b, s, PRINT_TITLES)
            .into_iter()
            .find(|p| {
                p.trim_start_matches('$')
                    .starts_with(|c: char| c.is_ascii_digit())
                    == rows
            })
            .unwrap_or_default()
    }

    pub(crate) fn put_titles(
        &mut self,
        b: BookId,
        s: SheetId,
        rows: bool,
        text: &str,
    ) -> Result<(), OmError> {
        let prop = if rows {
            "PrintTitleRows"
        } else {
            "PrintTitleColumns"
        };
        if text.chars().count() > MAX_AREA_TEXT {
            return Err(set_fail(prop));
        }
        let text = text.strip_prefix('=').unwrap_or(text);
        let mut mine = Vec::new();
        if !text.is_empty() {
            let wb = &self.book(b)?.wb;
            let Some((sheet, areas)) = parse_areas_text(wb, s, text) else {
                return Err(set_fail(prop));
            };
            if sheet != s || areas.len() != 1 {
                return Err(set_fail(prop));
            }
            let r = areas[0];
            let all_cols = r.c0 == 0 && r.c1 + 1 >= crate::addr::max_cols();
            let all_rows = r.r0 == 0 && r.r1 + 1 >= crate::addr::max_rows();

            if all_cols && all_rows {
                return Err(set_fail(prop));
            }

            let r = if rows {
                if all_rows && !all_cols {
                    return Err(set_fail(prop));
                }
                Rect::new(r.r0, 0, r.r1, crate::addr::max_cols() - 1)
            } else {
                if all_cols && !all_rows {
                    return Err(set_fail(prop));
                }
                Rect::new(0, r.c0, crate::addr::max_rows() - 1, r.c1)
            };
            mine.push(range_text(r, &abs_style()));
        }
        let other = self.title_part(b, s, !rows);
        let mut parts = Vec::new();

        if rows {
            if !other.is_empty() {
                parts.push(other);
            }
            parts.extend(mine);
        } else {
            parts.extend(mine);
            if !other.is_empty() {
                parts.push(other);
            }
        }
        self.set_name_parts(b, s, PRINT_TITLES, &parts);
        Ok(())
    }

    pub(crate) fn put_print_area(
        &mut self,
        b: BookId,
        s: SheetId,
        text: &str,
    ) -> Result<(), OmError> {
        if text.chars().count() > MAX_AREA_TEXT {
            return Err(set_fail("PrintArea"));
        }
        let text = text.strip_prefix('=').unwrap_or(text);
        if text.is_empty() {
            self.set_name_parts(b, s, PRINT_AREA, &[]);
            return Ok(());
        }
        let bad = || {
            OmError::app_defined_with(
                "This formula is missing a range reference or a defined name. ",
            )
        };
        let wb = &self.book(b)?.wb;
        let mut dot_name = false;
        let areas = if let Some((sheet, areas)) = parse_areas_text(wb, s, text) {
            if sheet != s {
                return Err(bad());
            }
            areas
        } else if text.starts_with('=') {
            return Err(bad());
        } else {
            let (r, c) = wb.active_cell_of(s);
            match wb.evaluate_text(text, CellAddr::new(s, r, c)) {
                Evaluated::Ref(areas)
                    if !areas.is_empty() && areas.iter().all(|x| x.sheet == s) =>
                {
                    areas.iter().map(|x| x.rect).collect()
                }

                _ => match wb.dot_notation_range(text, CellAddr::new(s, r, c)) {
                    Some(areas) if !areas.is_empty() && areas.iter().all(|x| x.sheet == s) => {
                        dot_name = true;
                        areas.iter().map(|x| x.rect).collect()
                    }
                    _ => return Err(bad()),
                },
            }
        };
        if dot_name {
            self.book_mut(b)?.wb.note_dot_notation_name(text);
        }
        let areas = minimize_sqref(areas);
        let parts: Vec<String> = areas.iter().map(|r| range_text(*r, &abs_style())).collect();
        self.set_name_parts(b, s, PRINT_AREA, &parts);
        Ok(())
    }

    pub(crate) fn header_footer_text(&self, b: BookId, s: SheetId, footer: bool) -> String {
        let p = self.page_setup(b, s);
        if footer {
            p.footer
        } else {
            p.header
        }
    }

    pub(crate) fn print_area_parts(&self, b: BookId, s: SheetId) -> Vec<String> {
        self.name_parts(b, s, PRINT_AREA)
    }

    pub(crate) fn print_title_rows(&self, b: BookId, s: SheetId) -> String {
        self.title_part(b, s, true)
    }

    pub(crate) fn print_title_columns(&self, b: BookId, s: SheetId) -> String {
        self.title_part(b, s, false)
    }

    pub(crate) fn pages_member(
        &mut self,
        b: BookId,
        s: SheetId,
        m: &'static Member,
    ) -> Result<Variant, OmError> {
        use InvokeKind::*;
        match (m.name, m.kind) {
            ("Count", Get) => {
                let used = self.book(b)?.wb.used_range(s).is_some();
                Ok(Variant::I4(i32::from(used)))
            }
            ("_NewEnum", Get) => Ok(self.alloc_enumerator(Vec::new())),
            ("Count", _) => Err(OmError::hresult(xlcalc_variant::HResult(
                0x8002_0003_u32 as i32,
            ))),
            ("Application", Get) => Ok(Variant::Dispatch(Some(self.handle()))),
            ("Creator", Get) => Ok(Variant::I4(CREATOR)),
            ("Parent", Get) => Ok(self.alloc(Object::PageSetup(b, s))),
            _ => Err(unsupported("Pages", m)),
        }
    }

    pub(crate) fn page_setup_member(
        &mut self,
        b: BookId,
        s: SheetId,
        m: &'static Member,
        a: &Args,
    ) -> Result<Variant, OmError> {
        let by_type = matches!(
            m.name,
            "OddAndEvenPagesHeaderFooter"
                | "DifferentFirstPageHeaderFooter"
                | "ScaleWithDocHeaderFooter"
                | "AlignMarginsHeaderFooter"
                | "PrintComments"
                | "PrintErrors"
        );
        if by_type && m.kind == InvokeKind::Put {
            self.coerce_wb_put(m, a)?;
            if m.name == "PrintErrors" && matches!(a.put, Some(Variant::Bool(true))) {
                return Err(OmError::exception(0x8007_0057_u32 as i32, None));
            }
        }
        self.page_setup_member_inner(b, s, m, a).map_err(|e| {
            let bad = e.scode == crate::error::SCODE_TYPE_MISMATCH
                || e.hresult == xlcalc_variant::HResult::DISP_E_OVERFLOW;
            if m.kind == InvokeKind::Put && bad {
                OmError::app_defined()
            } else {
                e
            }
        })
    }

    fn page_setup_member_inner(
        &mut self,
        b: BookId,
        s: SheetId,
        m: &'static Member,
        a: &Args,
    ) -> Result<Variant, OmError> {
        use InvokeKind::*;
        if m.name == "Pages" && m.kind == Get {
            return Ok(self.alloc(Object::Pages(b, s)));
        }
        let mut p = self.page_setup(b, s);
        let text = |v: &str| Variant::Bstr(v.to_string());
        if m.kind == Get {
            return Ok(match m.name {
                "AlignMarginsHeaderFooter" => Variant::Bool(p.align_margins),
                "BlackAndWhite" => Variant::Bool(p.black_and_white),
                "BottomMargin" => Variant::R8(p.bottom),
                "CenterFooter" => text(&hf_part(&p.footer, 1)),
                "CenterHeader" => text(&hf_part(&p.header, 1)),
                "CenterHorizontally" => Variant::Bool(p.center_horizontally),
                "CenterVertically" => Variant::Bool(p.center_vertically),
                "DifferentFirstPageHeaderFooter" => Variant::Bool(p.different_first),
                "Draft" => Variant::Bool(p.draft),
                "FirstPageNumber" => match p.first_page {
                    Some(n) => Variant::R8(n),
                    None => Variant::I4(AUTOMATIC),
                },
                "FitToPagesTall" => p.fit_tall.map_or(Variant::Bool(false), Variant::I4),
                "FitToPagesWide" => p.fit_wide.map_or(Variant::Bool(false), Variant::I4),
                "FooterMargin" => Variant::R8(p.footer_margin),
                "HeaderMargin" => Variant::R8(p.header_margin),
                "LeftFooter" => text(&hf_part(&p.footer, 0)),
                "LeftHeader" => text(&hf_part(&p.header, 0)),
                "LeftMargin" => Variant::R8(p.left),
                "OddAndEvenPagesHeaderFooter" => Variant::Bool(p.odd_even),
                "Order" => Variant::R8(p.order),
                "Orientation" => Variant::R8(p.orientation),
                "PaperSize" => Variant::R8(p.paper_size),
                "PrintArea" => {
                    let parts = self.name_parts(b, s, PRINT_AREA);
                    if parts.is_empty() {
                        Variant::BstrNull
                    } else {
                        Variant::Bstr(parts.join(","))
                    }
                }
                "PrintComments" => Variant::I4(p.comments),
                "PrintErrors" => Variant::I4(p.errors),
                "PrintGridlines" => Variant::Bool(p.gridlines),
                "PrintHeadings" => Variant::Bool(p.headings),
                "PrintNotes" => Variant::Bool(p.comments != NO_COMMENTS),
                "PrintQuality" => Variant::Array(Box::new(SafeArray {
                    elem: VarType::Variant,
                    bounds: vec![(1, 2)],
                    data: vec![Variant::R8(600.0), Variant::R8(600.0)],
                })),
                "PrintTitleColumns" => Variant::Bstr(self.title_part(b, s, false)),
                "PrintTitleRows" => Variant::Bstr(self.title_part(b, s, true)),
                "RightFooter" => text(&hf_part(&p.footer, 2)),
                "RightHeader" => text(&hf_part(&p.header, 2)),
                "RightMargin" => Variant::R8(p.right),
                "ScaleWithDocHeaderFooter" => Variant::Bool(p.scale_with_doc),
                "TopMargin" => Variant::R8(p.top),
                "Zoom" => p.zoom.map_or(Variant::Bool(false), Variant::I4),
                "ChartSize" => Variant::Error(0x800A_07FA_u32 as i32),
                "Application" => Variant::Dispatch(Some(self.handle())),
                "Creator" => Variant::I4(CREATOR),
                "Parent" => self.alloc(Object::Worksheet(b, s)),
                _ => return Err(unsupported("PageSetup", m)),
            });
        }
        if !matches!(m.kind, Put) {
            return Err(unsupported("PageSetup", m));
        }

        let mut value = Application::put_value(a);
        let name = m.name;

        if matches!(
            name,
            "Order"
                | "Orientation"
                | "PaperSize"
                | "FirstPageNumber"
                | "TopMargin"
                | "BottomMargin"
                | "LeftMargin"
                | "RightMargin"
                | "HeaderMargin"
                | "FooterMargin"
        ) {
            if let Some(Some(Variant::Bool(t))) = value.values.first() {
                value.values[0] = Some(Variant::I4(i32::from(*t)));
            }
        }

        if name.ends_with("Header") || name.ends_with("Footer") {
            if let Some(Some(Variant::Bool(t))) = value.values.first() {
                let text = if *t { "TRUE" } else { "FALSE" };
                value.values[0] = Some(Variant::Bstr(text.to_string()));
            }
        }
        let a = &value;
        let fail = || set_fail(name);
        let boolean = |a: &Args| a.bool(0)?.ok_or_else(OmError::type_mismatch);
        let real = |a: &Args| a.f64(0)?.ok_or_else(OmError::type_mismatch);

        let margin = |a: &Args, cur: f64| -> Result<f64, OmError> {
            match a.get(0) {
                None => Ok(cur),
                Some(v) if crate::conv::is_missing(v) || matches!(v, Variant::Null) => Ok(cur),
                Some(Variant::Bstr(t)) if t.contains(',') => Err(fail()),

                Some(Variant::Array(arr)) => {
                    let mut v = arr.data.first();
                    while let Some(Variant::Array(inner)) = v {
                        v = inner.data.first();
                    }
                    match v {
                        None | Some(Variant::Null) => Err(OmError::arg(
                            xlcalc_variant::HResult::DISP_E_TYPEMISMATCH,
                            0,
                        )),
                        Some(Variant::Bstr(t)) if t.is_empty() => Err(fail()),
                        Some(Variant::Bool(x)) => Ok(f64::from(u8::from(*x))),
                        Some(v) => a.variant_f64(v).map(stored_points),
                    }
                }
                _ => real(a).map(stored_points),
            }
        };
        let string = |a: &Args| a.string(0)?.ok_or_else(OmError::type_mismatch);

        let counted = |a: &Args, lo: i32, hi: i32| -> Result<Option<i32>, OmError> {
            match a.get(0) {
                Some(Variant::Bool(false)) => Ok(None),

                Some(
                    Variant::I2(_)
                    | Variant::I4(_)
                    | Variant::R4(_)
                    | Variant::R8(_)
                    | Variant::Ui1(_)
                    | Variant::I8(_)
                    | Variant::Int(_),
                ) => {
                    let n = a.f64(0)?.ok_or_else(OmError::type_mismatch)?.trunc();
                    if n < f64::from(lo) || n > f64::from(hi) {
                        Err(fail())
                    } else {
                        Ok(Some(n as i32))
                    }
                }
                _ => Err(fail()),
            }
        };

        let broken = p.fit_tall.is_some_and(|n| n > 65535) || p.fit_wide.is_some_and(|n| n > 65535);
        if broken
            && matches!(
                name,
                "BlackAndWhite"
                    | "BottomMargin"
                    | "CenterFooter"
                    | "CenterHeader"
                    | "CenterHorizontally"
                    | "CenterVertically"
                    | "Draft"
                    | "FirstPageNumber"
                    | "FooterMargin"
                    | "HeaderMargin"
                    | "LeftFooter"
                    | "LeftHeader"
                    | "LeftMargin"
                    | "Order"
                    | "Orientation"
                    | "PaperSize"
                    | "PrintGridlines"
                    | "PrintHeadings"
                    | "PrintNotes"
                    | "PrintQuality"
                    | "RightFooter"
                    | "RightHeader"
                    | "RightMargin"
                    | "TopMargin"
            )
        {
            return Err(fail());
        }
        match name {
            "AlignMarginsHeaderFooter" => p.align_margins = boolean(a)?,
            "BlackAndWhite" => flag(a, false, &mut p.black_and_white).map_err(|_| fail())?,
            "BottomMargin" => p.bottom = margin(a, p.bottom)?,
            "CenterFooter" => {
                p.footer = hf_replace(&p.footer, 1, &hf_text(a)?).ok_or_else(fail)?;
            }
            "CenterHeader" => {
                p.header = hf_replace(&p.header, 1, &hf_text(a)?).ok_or_else(fail)?;
            }
            "CenterHorizontally" => {
                flag(a, false, &mut p.center_horizontally).map_err(|_| fail())?
            }
            "CenterVertically" => flag(a, false, &mut p.center_vertically).map_err(|_| fail())?,
            "DifferentFirstPageHeaderFooter" => p.different_first = boolean(a)?,
            "ChartSize" => return Err(fail()),
            "Draft" => flag(a, true, &mut p.draft).map_err(|_| fail())?,
            "FirstPageNumber" => {
                let first = first_element(a);
                let a = &first;

                p.first_page = match a.get(0) {
                    Some(v) if !matches!(v, Variant::Bstr(_) | Variant::BstrNull) => {
                        match enum_put(a, &[])? {
                            Some(-4105.0) => None,
                            Some(n) => first_page_value(n),
                            None => Some(0.0),
                        }
                    }
                    _ => first_page_value(real(a)?.floor()),
                };
            }
            "FitToPagesTall" => p.fit_tall = counted(a, 1, i32::MAX)?,
            "FitToPagesWide" => p.fit_wide = counted(a, 1, i32::MAX)?,
            "FooterMargin" => p.footer_margin = margin(a, p.footer_margin)?,
            "HeaderMargin" => p.header_margin = margin(a, p.header_margin)?,
            "LeftFooter" => {
                p.footer = hf_replace(&p.footer, 0, &hf_text(a)?).ok_or_else(fail)?;
            }
            "LeftHeader" => {
                p.header = hf_replace(&p.header, 0, &hf_text(a)?).ok_or_else(fail)?;
            }
            "LeftMargin" => p.left = margin(a, p.left)?,
            "OddAndEvenPagesHeaderFooter" => p.odd_even = boolean(a)?,
            "Order" => {
                if let Some(n) = enum_put(&first_element(a), &[1, 2])? {
                    p.order = n;
                }
            }
            "Orientation" => {
                if let Some(n) = enum_put(&first_element(a), &[1, 2])? {
                    p.orientation = n;
                }
            }
            "PaperSize" => {
                if let Some(n) = enum_put(a, &[])? {
                    if !paper_ok(n as i32) {
                        return Err(fail());
                    }
                    p.paper_size = n;
                }
            }
            "PrintArea" => {
                if let Some(v @ Variant::Dispatch(Some(_))) = a.get(0) {
                    if let Ok((rs, r)) = self.range_arg(Some(v)) {
                        let blank = r.r0 == r.r1
                            && r.c0 == r.c1
                            && self.book(b).is_ok_and(|bk| {
                                matches!(
                                    bk.wb.value(CellAddr::new(rs, r.r0, r.c0)),
                                    xlcalc::Value::Blank
                                )
                            });
                        if blank {
                            return self.put_print_area(b, s, "").map(|_| Variant::Empty);
                        }
                    }
                }
                let t = titles_text(a, &string, &fail)?;
                if t.is_empty() {
                    return self.put_print_area(b, s, "").map(|_| Variant::Empty);
                }
                return self.put_print_area(b, s, &t).map(|_| Variant::Empty);
            }
            "PrintComments" => match a.i32(0)? {
                Some(n @ (NO_COMMENTS | 1 | 16)) => p.comments = n,
                _ => {
                    return Err(OmError::app_defined_with(
                        "Invalid data value for parameter.",
                    ));
                }
            },
            "PrintErrors" => match a.i32(0)? {
                Some(n @ 0..=3) => p.errors = n,
                _ => return Err(OmError::exception(0x8007_0057_u32 as i32, None)),
            },
            "PrintGridlines" => p.gridlines = boolean(a)?,
            "PrintHeadings" => p.headings = boolean(a)?,
            "PrintNotes" => {
                p.comments = if boolean(a)? { 1 } else { NO_COMMENTS };
            }
            "PrintQuality" => match a.i32(0)? {
                Some(600) => {}
                _ => return Err(fail()),
            },
            "PrintTitleColumns" => {
                return self
                    .put_titles(b, s, false, &titles_text(a, &string, &fail)?)
                    .map(|_| Variant::Empty);
            }
            "PrintTitleRows" => {
                return self
                    .put_titles(b, s, true, &titles_text(a, &string, &fail)?)
                    .map(|_| Variant::Empty);
            }
            "RightFooter" => {
                p.footer = hf_replace(&p.footer, 2, &hf_text(a)?).ok_or_else(fail)?;
            }
            "RightHeader" => {
                p.header = hf_replace(&p.header, 2, &hf_text(a)?).ok_or_else(fail)?;
            }
            "RightMargin" => p.right = margin(a, p.right)?,
            "ScaleWithDocHeaderFooter" => p.scale_with_doc = boolean(a)?,
            "TopMargin" => p.top = margin(a, p.top)?,
            "Zoom" => p.zoom = counted(a, 10, 400)?,
            _ => return Err(unsupported("PageSetup", m)),
        }
        if !matches!(name, "Zoom" | "FitToPagesWide" | "FitToPagesTall") {
            p.list_zoom = p.zoom.unwrap_or(0);
            p.list_win = Some(self.break_window(b, s));
        }
        self.book_mut(b)?.page_setup.insert(s, p);
        Ok(Variant::Empty)
    }
}
