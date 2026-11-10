use std::collections::HashMap;

use xlcalc::style::{nearest_color_index, PALETTE};
use xlcalc::{Rect, SheetId};
use xlcalc_variant::Variant;

use crate::app::{unsupported, Application, BookId, Mode, Object, SheetsKind};
use crate::conv::Args;
use crate::error::OmError;
use crate::typelib::{InvokeKind, Member};

#[derive(Clone, Debug)]
pub(crate) struct SheetView {
    pub zoom: f64,
    pub gridlines: bool,
    pub headings: bool,
    pub formulas: bool,
    pub zeros: bool,

    pub split_row: u32,
    pub split_col: u32,
    pub frozen: bool,
    pub scroll_row: u32,
    pub scroll_col: u32,

    pub color: u32,
    pub color_set: bool,
    pub color_index: i32,
}

impl Default for SheetView {
    fn default() -> Self {
        SheetView {
            zoom: 100.0,
            gridlines: true,
            headings: true,
            formulas: false,
            zeros: true,
            split_row: 0,
            split_col: 0,
            frozen: false,
            scroll_row: 1,
            scroll_col: 1,
            color: 0,
            color_set: false,
            color_index: -4105,
        }
    }
}

#[derive(Clone, Debug)]
pub(crate) struct WindowState {
    pub views: HashMap<SheetId, SheetView>,
    pub view_mode: i32,
    pub tab_ratio: f64,
    pub caption: Option<String>,

    pub group: (Option<SheetId>, Vec<SheetId>),

    pub flags: Vec<(&'static str, bool)>,
}

impl Default for WindowState {
    fn default() -> Self {
        WindowState {
            views: HashMap::new(),
            view_mode: 1,
            tab_ratio: 0.6,
            caption: None,
            group: (None, Vec::new()),
            flags: vec![
                ("DisplayOutline", true),
                ("DisplayHorizontalScrollBar", true),
                ("DisplayVerticalScrollBar", true),
                ("DisplayWorkbookTabs", true),
                ("EnableResize", true),
                ("Visible", true),
                ("DisplayRuler", true),
                ("DisplayRightToLeft", false),
            ],
        }
    }
}

fn unable(m: &Member, set: bool) -> OmError {
    if set {
        OmError::unable_to_set(m.name, "Window")
    } else {
        OmError::app_defined_with(&format!(
            "Unable to get the {} property of the Window class",
            m.name
        ))
    }
}

pub(crate) fn palette_bgr(index: i32) -> u32 {
    let rgb = PALETTE[(index - 1) as usize];
    ((rgb & 0xFF) << 16) | (rgb & 0xFF00) | (rgb >> 16)
}

impl Application<'_> {
    fn view_of(&self, b: BookId) -> Result<(SheetId, SheetView), OmError> {
        let book = self.book(b)?;
        let sheet = book.wb.active_sheet();
        Ok((
            sheet,
            book.window.views.get(&sheet).cloned().unwrap_or_default(),
        ))
    }

    fn set_view(&mut self, b: BookId, f: impl FnOnce(&mut SheetView)) -> Result<(), OmError> {
        let book = self.book_mut(b)?;
        let sheet = book.wb.active_sheet();
        f(book.window.views.entry(sheet).or_default());
        Ok(())
    }

    pub(crate) fn selected_sheets(&self, b: BookId) -> Result<Vec<SheetId>, OmError> {
        let book = self.book(b)?;
        let active = book.wb.active_sheet();
        let (was, list) = &book.window.group;
        let mut ids: Vec<SheetId> = if *was == Some(active) {
            list.clone()
        } else {
            Vec::new()
        };
        if !ids.contains(&active) {
            ids.push(active);
        }
        Ok(book
            .wb
            .sheets()
            .map(|s| s.id)
            .filter(|s| ids.contains(s))
            .collect())
    }

    pub(crate) fn group_select(
        &mut self,
        b: BookId,
        sheet: SheetId,
        add: bool,
    ) -> Result<(), OmError> {
        let mut ids = if add {
            self.selected_sheets(b)?
        } else {
            Vec::new()
        };
        if !ids.contains(&sheet) {
            ids.push(sheet);
        }
        let book = self.book_mut(b)?;
        if !add {
            book.wb
                .set_active_sheet(sheet)
                .map_err(crate::objects::edit_error)?;
        }
        let active = book.wb.active_sheet();
        book.window.group = (Some(active), ids);
        Ok(())
    }

    pub(crate) fn window_view_member(
        &mut self,
        b: BookId,
        m: &'static Member,
        a: &Args,
    ) -> Result<Variant, OmError> {
        use InvokeKind::*;
        let set = m.kind == Put;
        match (m.name, m.kind) {
            ("Caption", Put) => {
                let c = self.put_string(a)?;
                self.book_mut(b)?.window.caption = Some(c);
                Ok(Variant::Empty)
            }
            ("Type", Get) => Ok(Variant::I4(1)),
            ("WindowState", Get) => Ok(Variant::I4(-4143)),
            ("Zoom", Get) => Ok(Variant::R8(self.view_of(b)?.1.zoom)),
            ("Zoom", Put) => {
                if matches!(a.put, Some(Variant::Bool(_))) {
                    return Ok(Variant::Empty);
                }
                let z = self.put_f64(a)?.trunc();
                if !(10.0..=400.0).contains(&z) {
                    return Err(unable(m, true));
                }
                self.set_view(b, |v| v.zoom = z)?;
                Ok(Variant::Empty)
            }
            ("DisplayGridlines" | "DisplayHeadings" | "DisplayFormulas" | "DisplayZeros", Get) => {
                let v = self.view_of(b)?.1;
                Ok(Variant::Bool(match m.name {
                    "DisplayGridlines" => v.gridlines,
                    "DisplayHeadings" => v.headings,
                    "DisplayFormulas" => v.formulas,
                    _ => v.zeros,
                }))
            }
            ("DisplayGridlines" | "DisplayHeadings" | "DisplayFormulas" | "DisplayZeros", Put) => {
                let on = self.put_bool(a)?;
                let name = m.name;
                self.set_view(b, |v| match name {
                    "DisplayGridlines" => v.gridlines = on,
                    "DisplayHeadings" => v.headings = on,
                    "DisplayFormulas" => v.formulas = on,
                    _ => v.zeros = on,
                })?;
                Ok(Variant::Empty)
            }
            ("GridlineColor", Get) => Ok(Variant::R8(f64::from(self.view_of(b)?.1.color))),
            ("GridlineColor", Put) => {
                let c = self.put_f64(a)?;
                if !(0.0..=16_777_215.0).contains(&c) {
                    return Err(unable(m, true));
                }
                let c = c as u32;
                let rgb = ((c & 0xFF) << 16) | (c & 0xFF00) | (c >> 16);
                let idx = i32::from(nearest_color_index(rgb));
                self.set_view(b, |v| {
                    v.color = c;
                    v.color_set = true;
                    v.color_index = idx;
                })?;
                Ok(Variant::Empty)
            }
            ("GridlineColorIndex", Get) => {
                let v = self.view_of(b)?.1;
                Ok(if v.color_set {
                    Variant::R8(f64::from(v.color_index))
                } else {
                    Variant::I4(-4105)
                })
            }
            ("GridlineColorIndex", Put) => {
                let i = self.put_i32(a)?;
                if i == -4105 {
                    self.set_view(b, |v| {
                        *v = SheetView {
                            color_set: false,
                            color: 0,
                            color_index: -4105,
                            ..v.clone()
                        }
                    })?;
                    return Ok(Variant::Empty);
                }
                if !(1..=56).contains(&i) {
                    return Err(unable(m, true));
                }
                self.set_view(b, |v| {
                    v.color = palette_bgr(i);
                    v.color_set = true;
                    v.color_index = i;
                })?;
                Ok(Variant::Empty)
            }
            ("ScrollRow" | "ScrollColumn", Get) => {
                let v = self.view_of(b)?.1;
                Ok(Variant::R8(f64::from(if m.name == "ScrollRow" {
                    v.scroll_row
                } else {
                    v.scroll_col
                })))
            }
            ("ScrollRow" | "ScrollColumn", Put) => {
                let n = self.put_f64(a)?.trunc();
                let max = if m.name == "ScrollRow" {
                    1_048_576.0
                } else {
                    16_384.0
                };
                if !(1.0..=max).contains(&n) {
                    return Err(unable(m, true));
                }
                let (row, n) = (m.name == "ScrollRow", n as u32);
                self.set_view(b, |v| {
                    if row {
                        v.scroll_row = n;
                    } else {
                        v.scroll_col = n;
                    }
                })?;
                Ok(Variant::Empty)
            }
            ("SplitRow" | "SplitColumn", Get) => {
                let v = self.view_of(b)?.1;
                let n = if m.name == "SplitRow" {
                    v.split_row
                } else {
                    v.split_col
                };
                Ok(if n == 0 {
                    Variant::I4(0)
                } else {
                    Variant::R8(f64::from(n))
                })
            }
            ("SplitRow" | "SplitColumn", Put) => {
                let n = self.put_f64(a)?.trunc();
                let max = if m.name == "SplitRow" {
                    1_048_576.0
                } else {
                    16_384.0
                };
                if !(0.0..=max).contains(&n) {
                    return Err(unable(m, true));
                }
                let (row, n) = (m.name == "SplitRow", n as u32);
                self.set_view(b, |v| {
                    if row {
                        v.split_row = n;
                    } else {
                        v.split_col = n;
                    }
                })?;
                Ok(Variant::Empty)
            }
            ("SplitHorizontal" | "SplitVertical", Get) => Ok(Variant::I4(0)),
            ("Split", Get) => {
                let v = self.view_of(b)?.1;
                Ok(Variant::Bool(v.split_row > 0 || v.split_col > 0))
            }
            ("Split", Put) => {
                if !self.put_bool(a)? {
                    self.set_view(b, |v| {
                        v.split_row = 0;
                        v.split_col = 0;
                        v.frozen = false;
                    })?;
                }
                Ok(Variant::Empty)
            }
            ("FreezePanes", Get) => Ok(Variant::Bool(self.view_of(b)?.1.frozen)),
            ("FreezePanes", Put) => {
                let on = self.put_bool(a)?;
                let (r, c) = self.book(b)?.wb.active_cell();
                self.set_view(b, |v| {
                    if on && v.split_row == 0 && v.split_col == 0 {
                        v.split_row = r;
                        v.split_col = c;
                    }
                    v.frozen = on && (v.split_row > 0 || v.split_col > 0);
                })?;
                Ok(Variant::Empty)
            }
            ("View", Get) => Ok(Variant::I4(self.book(b)?.window.view_mode)),
            ("View", Put) => {
                let v = self.put_i32(a)?;
                if !(1..=3).contains(&v) {
                    return Err(OmError::app_defined());
                }
                self.book_mut(b)?.window.view_mode = v;
                Ok(Variant::Empty)
            }
            ("TabRatio", Get) => Ok(Variant::R8(self.book(b)?.window.tab_ratio)),
            ("TabRatio", Put) => {
                let r = self.put_f64(a)?;
                if !(0.0..=1.0).contains(&r) {
                    return Err(unable(m, true));
                }
                self.book_mut(b)?.window.tab_ratio = r;
                Ok(Variant::Empty)
            }
            (
                "DisplayOutline"
                | "DisplayHorizontalScrollBar"
                | "DisplayVerticalScrollBar"
                | "DisplayWorkbookTabs"
                | "EnableResize"
                | "Visible"
                | "DisplayRuler"
                | "DisplayRightToLeft",
                _,
            ) => {
                let name = m.name;
                if set {
                    let on = self.put_bool(a)?;
                    if let Some(f) = self
                        .book_mut(b)?
                        .window
                        .flags
                        .iter_mut()
                        .find(|f| f.0 == name)
                    {
                        f.1 = on;
                    }
                    Ok(Variant::Empty)
                } else {
                    let on = self
                        .book(b)?
                        .window
                        .flags
                        .iter()
                        .find(|f| f.0 == name)
                        .is_some_and(|f| f.1);
                    Ok(Variant::Bool(on))
                }
            }
            ("ActiveSheet", Get) => {
                let book = self.book(b)?;
                let s = book.wb.active_sheet();
                Ok(self.sheet_object(b, s))
            }
            ("SelectedSheets", Get) => Ok(self.alloc(Object::Sheets(b, SheetsKind::Selected))),
            ("ActiveCell", Get) => {
                let book = self.book(b)?;
                let sheet = book.wb.active_sheet();
                let (r, c) = book.wb.active_cell();
                self.new_range(b, sheet, Rect::cell(r, c), Mode::Cells)
            }
            ("RangeSelection", Get) => {
                let book = self.book(b)?;
                let sheet = book.wb.active_sheet();
                let sel = book.wb.selection_of(sheet);
                let first = sel.areas.first().copied().unwrap_or(Rect::cell(0, 0));
                self.new_range(b, sheet, first, Mode::Cells)
            }
            ("ScrollIntoView", Func) => Ok(Variant::Empty),
            ("SmallScroll" | "LargeScroll", Func) => {
                let page = m.name == "LargeScroll";
                let n = |i: usize| a.i32(i).map(|v| i64::from(v.unwrap_or(0)));
                let (down, up, right, left) = (n(0)?, n(1)?, n(2)?, n(3)?);
                let (rows, cols) = if page { (46, 15) } else { (1, 1) };
                self.set_view(b, |v| {
                    let r = i64::from(v.scroll_row) + (down - up) * rows;
                    let c = i64::from(v.scroll_col) + (right - left) * cols;
                    v.scroll_row = r.clamp(1, 1_048_576) as u32;
                    v.scroll_col = c.clamp(1, 16_384) as u32;
                })?;
                Ok(Variant::Bool(true))
            }

            ("ScrollWorkbookTabs", _) if a.get(0).is_none() && a.get(1).is_none() => {
                Err(unable(m, false))
            }
            ("ScrollWorkbookTabs", _) => Ok(Variant::Bool(true)),
            _ => Err(unsupported("Window", m)),
        }
    }
}

impl Application<'_> {
    fn pane_count(&mut self, b: BookId) -> Result<u32, OmError> {
        let v = self.view_of(b)?.1;
        Ok((1 + u32::from(v.split_row > 0)) * (1 + u32::from(v.split_col > 0)))
    }

    pub(crate) fn panes_member(
        &mut self,
        b: BookId,
        n: u32,
        m: &'static Member,
        a: &Args,
    ) -> Result<Variant, OmError> {
        let count = self.pane_count(b)?;
        match m.name {
            "Count" => Ok(Variant::I4(count as i32)),
            "Item" | "_Default" => {
                let i = Args {
                    values: vec![a.get(0).cloned()],
                    put: None,
                    lcid: a.lcid,
                }
                .i32(0)?
                .unwrap_or(0);
                match u32::try_from(i) {
                    Ok(i) if (1..=count).contains(&i) => Ok(self.alloc(Object::Pane(b, n, i))),
                    _ => Err(OmError::bad_index()),
                }
            }
            "Parent" | "Application" => Err(unsupported("Panes", m)),
            _ => Err(unsupported("Panes", m)),
        }
    }

    pub(crate) fn pane_member(
        &mut self,
        b: BookId,
        n: u32,
        i: u32,
        m: &'static Member,
        a: &Args,
    ) -> Result<Variant, OmError> {
        match (m.name, m.kind) {
            ("Index", InvokeKind::Get) => Ok(Variant::I4(i as i32)),
            ("ScrollRow" | "ScrollColumn", InvokeKind::Get) => self.window_view_member(b, m, a),
            ("ScrollRow" | "ScrollColumn", InvokeKind::Put) => {
                self.window_view_member(b, m, a).map_err(|e| {
                    if e.scode == crate::error::vba_scode(1004) {
                        OmError::app_defined()
                    } else {
                        e
                    }
                })
            }
            ("Parent", InvokeKind::Get) => Ok(self.alloc(Object::Window(b, n))),
            _ => Err(unsupported("Pane", m)),
        }
    }
}
