use std::collections::HashMap;

use xlcalc::{Rect, SheetId};
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
    pub center_footer: String,
    pub center_header: String,
    pub center_horizontally: bool,
    pub center_vertically: bool,
    pub different_first: bool,
    pub draft: bool,
    pub first_page: Option<f64>,
    pub fit_tall: Option<i32>,
    pub fit_wide: Option<i32>,
    pub footer_margin: f64,
    pub header_margin: f64,
    pub left_footer: String,
    pub left_header: String,
    pub left: f64,
    pub odd_even: bool,
    pub order: f64,
    pub orientation: f64,
    pub paper_size: f64,
    pub comments: i32,
    pub errors: i32,
    pub gridlines: bool,
    pub headings: bool,
    pub right_footer: String,
    pub right_header: String,
    pub right: f64,
    pub scale_with_doc: bool,
    pub top: f64,
    pub zoom: Option<i32>,

    pub header_order: Vec<char>,
    pub footer_order: Vec<char>,
}

impl Default for PageSetup {
    fn default() -> Self {
        PageSetup {
            align_margins: true,
            black_and_white: false,
            bottom: 54.0,
            center_footer: String::new(),
            center_header: String::new(),
            center_horizontally: false,
            center_vertically: false,
            different_first: false,
            draft: false,
            first_page: None,
            fit_tall: Some(1),
            fit_wide: Some(1),
            footer_margin: 0.3 * 72.0,
            header_margin: 0.3 * 72.0,
            left_footer: String::new(),
            left_header: String::new(),
            left: 50.4,
            odd_even: false,
            order: 1.0,
            orientation: 1.0,
            paper_size: 1.0,
            comments: NO_COMMENTS,
            errors: 0,
            gridlines: false,
            headings: false,
            right_footer: String::new(),
            right_header: String::new(),
            right: 50.4,
            scale_with_doc: true,
            top: 54.0,
            zoom: Some(100),
            header_order: Vec::new(),
            footer_order: Vec::new(),
        }
    }
}

pub(crate) type BookPageSetup = HashMap<SheetId, PageSetup>;

const PRINT_AREA: &str = "PRINT_AREA";
const PRINT_TITLES: &str = "PRINT_TITLES";

fn paper_ok(n: i32) -> bool {
    matches!(n, 1 | 3 | 5..=9 | 11..=13 | 19 | 20 | 24..=28 | 30 | 34 | 37 | 43 | 66 | 70..=74 | 91 | 119..=130)
}

fn abs_style() -> AddressStyle {
    AddressStyle {
        row_abs: true,
        col_abs: true,
        r1c1: false,
        relative_to: (0, 0),
    }
}

fn touch(order: &mut Vec<char>, part: char, set: bool) {
    order.retain(|c| *c != part);
    if set {
        order.push(part);
    }
}

fn set_fail(prop: &str) -> OmError {
    OmError::unable_to_set(prop, "PageSetup")
}

impl Application<'_> {
    pub(crate) fn page_setup(&self, b: BookId, s: SheetId) -> PageSetup {
        self.book(b)
            .ok()
            .and_then(|b| b.page_setup.get(&s).cloned())
            .unwrap_or_default()
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

    fn set_name_parts(&mut self, b: BookId, s: SheetId, key: &str, parts: &[String]) {
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

    fn put_titles(&mut self, b: BookId, s: SheetId, rows: bool, text: &str) -> Result<(), OmError> {
        let text = text.strip_prefix('=').unwrap_or(text);
        let prop = if rows {
            "PrintTitleRows"
        } else {
            "PrintTitleColumns"
        };
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
            let all_cols = r.c0 == 0 && r.c1 + 1 >= crate::addr::MAX_COLS;
            let all_rows = r.r0 == 0 && r.r1 + 1 >= crate::addr::MAX_ROWS;

            let r = if rows {
                if all_rows && !all_cols {
                    return Err(set_fail(prop));
                }
                Rect::new(r.r0, 0, r.r1, crate::addr::MAX_COLS - 1)
            } else {
                if all_cols && !all_rows {
                    return Err(set_fail(prop));
                }
                Rect::new(0, r.c0, crate::addr::MAX_ROWS - 1, r.c1)
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

    fn put_print_area(&mut self, b: BookId, s: SheetId, text: &str) -> Result<(), OmError> {
        let text = text.strip_prefix('=').unwrap_or(text);
        if text.is_empty() {
            self.set_name_parts(b, s, PRINT_AREA, &[]);
            return Ok(());
        }
        let wb = &self.book(b)?.wb;
        let bad = || {
            OmError::app_defined_with(
                "This formula is missing a range reference or a defined name. ",
            )
        };
        let Some((sheet, areas)) = parse_areas_text(wb, s, text) else {
            return Err(bad());
        };
        if sheet != s {
            return Err(bad());
        }
        let parts: Vec<String> = areas.iter().map(|r| range_text(*r, &abs_style())).collect();
        self.set_name_parts(b, s, PRINT_AREA, &parts);
        Ok(())
    }

    pub(crate) fn header_footer_text(&self, b: BookId, s: SheetId, footer: bool) -> String {
        let p = self.page_setup(b, s);
        let (order, l, c, r) = if footer {
            (
                &p.footer_order,
                &p.left_footer,
                &p.center_footer,
                &p.right_footer,
            )
        } else {
            (
                &p.header_order,
                &p.left_header,
                &p.center_header,
                &p.right_header,
            )
        };
        order
            .iter()
            .map(|k| {
                let t = match k {
                    'L' => l,
                    'C' => c,
                    _ => r,
                };
                format!("&{k}{t}")
            })
            .collect()
    }

    pub(crate) fn print_area_parts(&self, b: BookId, s: SheetId) -> Vec<String> {
        self.name_parts(b, s, PRINT_AREA)
    }

    pub(crate) fn print_title_rows(&self, b: BookId, s: SheetId) -> String {
        self.title_part(b, s, true)
    }

    pub(crate) fn pages_member(
        &mut self,
        b: BookId,
        s: SheetId,
        m: &'static Member,
    ) -> Result<Variant, OmError> {
        use InvokeKind::*;
        match (m.name, m.kind) {
            ("Count", Get) => Ok(Variant::I4(1)),
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
            if m.name == "PrintErrors" && matches!(a.put, Some(Variant::Bool(_))) {
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
                "CenterFooter" => text(&p.center_footer),
                "CenterHeader" => text(&p.center_header),
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
                "LeftFooter" => text(&p.left_footer),
                "LeftHeader" => text(&p.left_header),
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
                "RightFooter" => text(&p.right_footer),
                "RightHeader" => text(&p.right_header),
                "RightMargin" => Variant::R8(p.right),
                "ScaleWithDocHeaderFooter" => Variant::Bool(p.scale_with_doc),
                "TopMargin" => Variant::R8(p.top),
                "Zoom" => p.zoom.map_or(Variant::Bool(false), Variant::I4),
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
            "Order" | "Orientation" | "PaperSize" | "FirstPageNumber"
        ) {
            if let Some(Some(Variant::Bool(t))) = value.values.first() {
                value.values[0] = Some(Variant::I4(i32::from(*t)));
            }
        }
        let a = &value;
        let fail = || set_fail(name);
        let boolean = |a: &Args| a.bool(0)?.ok_or_else(OmError::type_mismatch);
        let real = |a: &Args| a.f64(0)?.ok_or_else(OmError::type_mismatch);
        let string = |a: &Args| a.string(0)?.ok_or_else(OmError::type_mismatch);

        let hf = |a: &Args| -> Result<String, OmError> {
            let t = a.string(0)?.ok_or_else(OmError::type_mismatch)?;
            if t.encode_utf16().count() > 255 {
                Err(fail())
            } else {
                Ok(t)
            }
        };

        let counted = |a: &Args, lo: i32, hi: i32| -> Result<Option<i32>, OmError> {
            match a.get(0) {
                Some(Variant::Bool(false)) => Ok(None),
                Some(Variant::Bool(true)) => Err(fail()),
                _ => {
                    let n = a.f64(0)?.ok_or_else(OmError::type_mismatch)?.trunc();
                    if n < f64::from(lo) || n > f64::from(hi) {
                        Err(fail())
                    } else {
                        Ok(Some(n as i32))
                    }
                }
            }
        };
        match name {
            "AlignMarginsHeaderFooter" => p.align_margins = boolean(a)?,
            "BlackAndWhite" => p.black_and_white = boolean(a)?,
            "BottomMargin" => p.bottom = real(a)?,
            "CenterFooter" => {
                p.center_footer = hf(a)?;
                touch(&mut p.footer_order, 'C', !p.center_footer.is_empty());
            }
            "CenterHeader" => {
                p.center_header = hf(a)?;
                touch(&mut p.header_order, 'C', !p.center_header.is_empty());
            }
            "CenterHorizontally" => p.center_horizontally = boolean(a)?,
            "CenterVertically" => p.center_vertically = boolean(a)?,
            "DifferentFirstPageHeaderFooter" => p.different_first = boolean(a)?,
            "Draft" => p.draft = boolean(a)?,
            "FirstPageNumber" => {
                let n = real(a)?.trunc();

                p.first_page = if n >= 2_147_483_647.0 {
                    Some(2_147_483_647.0)
                } else if n.abs() == 32_768.0 {
                    None
                } else {
                    Some(n)
                };
            }
            "FitToPagesTall" => p.fit_tall = counted(a, 1, 65535)?,
            "FitToPagesWide" => p.fit_wide = counted(a, 1, 65535)?,
            "FooterMargin" => p.footer_margin = real(a)?,
            "HeaderMargin" => p.header_margin = real(a)?,
            "LeftFooter" => {
                p.left_footer = hf(a)?;
                touch(&mut p.footer_order, 'L', !p.left_footer.is_empty());
            }
            "LeftHeader" => {
                p.left_header = hf(a)?;
                touch(&mut p.header_order, 'L', !p.left_header.is_empty());
            }
            "LeftMargin" => p.left = real(a)?,
            "OddAndEvenPagesHeaderFooter" => p.odd_even = boolean(a)?,
            "Order" => match a.i32(0)? {
                Some(n @ (1 | 2)) => p.order = f64::from(n),
                _ => return Err(fail()),
            },
            "Orientation" => match a.i32(0)? {
                Some(n @ (1 | 2)) => p.orientation = f64::from(n),
                _ => return Err(fail()),
            },
            "PaperSize" => match a.i32(0)? {
                Some(n) if paper_ok(n) => p.paper_size = f64::from(n),
                _ => return Err(fail()),
            },
            "PrintArea" => {
                if matches!(a.get(0), Some(Variant::Bool(false))) {
                    return self.put_print_area(b, s, "").map(|_| Variant::Empty);
                }
                return self
                    .put_print_area(b, s, &string(a)?)
                    .map(|_| Variant::Empty);
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
                _ => {
                    return Err(OmError::app_defined_with(
                        "Invalid data value for parameter.",
                    ));
                }
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
                    .put_titles(b, s, false, &string(a)?)
                    .map(|_| Variant::Empty);
            }
            "PrintTitleRows" => {
                return self
                    .put_titles(b, s, true, &string(a)?)
                    .map(|_| Variant::Empty);
            }
            "RightFooter" => {
                p.right_footer = hf(a)?;
                touch(&mut p.footer_order, 'R', !p.right_footer.is_empty());
            }
            "RightHeader" => {
                p.right_header = hf(a)?;
                touch(&mut p.header_order, 'R', !p.right_header.is_empty());
            }
            "RightMargin" => p.right = real(a)?,
            "ScaleWithDocHeaderFooter" => p.scale_with_doc = boolean(a)?,
            "TopMargin" => p.top = real(a)?,
            "Zoom" => p.zoom = counted(a, 10, 400)?,
            _ => return Err(unsupported("PageSetup", m)),
        }
        self.book_mut(b)?.page_setup.insert(s, p);
        Ok(Variant::Empty)
    }
}
