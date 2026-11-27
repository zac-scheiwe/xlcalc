use std::collections::HashMap;

use xlcalc::style::{nearest_color_index, PALETTE};
use xlcalc::{Rect, SheetId};
use xlcalc_variant::Variant;

use crate::app::{unsupported, Application, BookId, Mode, Object, SheetsKind};
use crate::conv::Args;
use crate::error::OmError;
use crate::panes::{freeze_point, Lines};
use crate::typelib::{InvokeKind, Member};

#[derive(Clone, Debug)]
pub(crate) struct SheetView {
    pub zoom: f64,
    pub gridlines: bool,
    pub headings: bool,
    pub formulas: bool,
    pub zeros: bool,

    pub split_row: f64,
    pub split_col: f64,

    pub split_by_freeze: bool,
    pub frozen: bool,
    pub scroll_row: u32,
    pub scroll_col: u32,

    pub pane_row: u32,
    pub pane_col: u32,

    pub color: u32,
    pub color_set: bool,

    pub color_rgb: bool,

    pub pane4: bool,
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
            split_row: 0.0,
            split_col: 0.0,
            split_by_freeze: false,
            frozen: false,
            scroll_row: 1,
            scroll_col: 1,
            pane_row: 1,
            pane_col: 1,
            color: 0,
            color_set: false,
            color_rgb: false,
            pane4: false,
            color_index: -4105,
        }
    }
}

#[derive(Clone, Debug)]
pub(crate) struct WindowState {
    pub views: HashMap<SheetId, SheetView>,
    pub view_mode: i32,

    pub pbp_zoom: f64,

    pub pl_zoom: f64,
    pub tab_ratio: f64,

    pub captions: HashMap<u32, String>,

    pub before_min: i32,

    pub state: i32,

    pub group: (Option<SheetId>, Vec<SheetId>),

    pub flags: Vec<(&'static str, bool)>,

    pub n: u32,

    pub others: HashMap<u32, WindowState>,
}

impl Default for WindowState {
    fn default() -> Self {
        WindowState {
            views: HashMap::new(),
            view_mode: 1,
            pbp_zoom: 60.0,
            pl_zoom: 100.0,
            tab_ratio: 0.6,
            captions: HashMap::new(),
            before_min: -4143,
            state: -4143,
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
                ("_DisplayRightToLeft", false),
                ("DisplayWhitespace", true),
                ("AutoFilterDateGrouping", true),
            ],
            n: 1,
            others: HashMap::new(),
        }
    }
}

impl WindowState {
    fn swap_data(&mut self, o: &mut WindowState) {
        use std::mem::swap;
        swap(&mut self.views, &mut o.views);
        swap(&mut self.view_mode, &mut o.view_mode);
        swap(&mut self.pbp_zoom, &mut o.pbp_zoom);
        swap(&mut self.pl_zoom, &mut o.pl_zoom);
        swap(&mut self.tab_ratio, &mut o.tab_ratio);
        swap(&mut self.before_min, &mut o.before_min);
        swap(&mut self.state, &mut o.state);
        swap(&mut self.group, &mut o.group);
        swap(&mut self.flags, &mut o.flags);
    }

    pub(crate) fn focus(&mut self, n: u32) {
        if self.n == n {
            return;
        }
        let mut theirs = self.others.remove(&n).unwrap_or_default();
        self.swap_data(&mut theirs);
        self.others.insert(self.n, theirs);
        self.n = n;
    }

    pub(crate) fn copy_window(&mut self, from: u32, new: u32) {
        self.focus(from);
        let copy = WindowState {
            views: self.views.clone(),
            view_mode: self.view_mode,
            pbp_zoom: self.pbp_zoom,
            pl_zoom: self.pl_zoom,
            tab_ratio: self.tab_ratio,
            before_min: self.before_min,
            state: self.state,
            group: self.group.clone(),
            flags: self.flags.clone(),
            ..WindowState::default()
        };
        self.others.insert(new, copy);
    }

    pub(crate) fn drop_window(&mut self, n: u32) {
        self.others.remove(&n);
    }
}

const WINDOW_WIDTH: f64 = 1409.0;
const WINDOW_HEIGHT: f64 = 673.5;
const ROW_POINTS: f64 = 14.5;
const COL_POINTS: f64 = 48.0;

fn col_points(v: &SheetView) -> f64 {
    if v.formulas {
        COL_POINTS * 2.0
    } else {
        COL_POINTS
    }
}

fn geometry(v: &SheetView, name: &str) -> (bool, f64, bool) {
    let rows = name.ends_with("Row") || name.ends_with("Vertical");
    let unit = if rows { ROW_POINTS } else { col_points(v) } * v.zoom / 100.0;
    (
        rows,
        unit,
        name.ends_with("Horizontal") || name.ends_with("Vertical"),
    )
}

fn half_window(v: &SheetView) -> (f64, f64) {
    let rows = (WINDOW_HEIGHT / (ROW_POINTS * v.zoom / 100.0)).ceil();
    let cols = (WINDOW_WIDTH / (col_points(v) * v.zoom / 100.0)).ceil();
    ((rows / 2.0).floor(), (cols / 2.0).floor())
}

enum WindowPut {
    Skip,

    Value(Variant),

    Pass,
}

impl Application<'_> {
    fn window_put_arg(&self, b: BookId, m: &Member, a: &Args) -> Result<WindowPut, OmError> {
        use xlcalc_variant::{change_type, VarType};
        const BOOLS: [&str; 10] = [
            "DisplayGridlines",
            "DisplayHeadings",
            "DisplayFormulas",
            "DisplayZeros",
            "DisplayOutline",
            "DisplayHorizontalScrollBar",
            "DisplayVerticalScrollBar",
            "DisplayWorkbookTabs",
            "Split",
            "FreezePanes",
        ];
        const NUMS: [&str; 8] = [
            "Zoom",
            "ScrollRow",
            "ScrollColumn",
            "SplitRow",
            "SplitColumn",
            "SplitHorizontal",
            "SplitVertical",
            "WindowState",
        ];
        let name = m.name;
        let is_bool = BOOLS.contains(&name);
        if name == "View" {
            return match &a.put {
                Some(v) if crate::conv::is_missing(v) => {
                    Err(OmError::hresult(crate::error::DISP_E_PARAMNOTOPTIONAL))
                }
                Some(Variant::Null | Variant::Array(_)) => {
                    Err(OmError::arg(crate::error::DISP_E_TYPEMISMATCH, 0))
                }
                Some(Variant::Bstr(t)) if t.trim().parse::<f64>().is_err() => {
                    Err(OmError::arg(crate::error::DISP_E_TYPEMISMATCH, 0))
                }
                _ => Ok(WindowPut::Pass),
            };
        }
        if !is_bool && !NUMS.contains(&name) {
            if let Some(Variant::Bstr(t)) = &a.put {
                if t.trim().parse::<f64>().is_err()
                    && !matches!(name, "Caption" | "AutoFilterDateGrouping")
                {
                    return Err(OmError::app_defined());
                }
            }
            return Ok(WindowPut::Pass);
        }
        let refused = || unable(m, true);
        let mut v = a.put.clone().unwrap_or(Variant::Empty);
        let mut from_array = false;
        if let Variant::Array(arr) = &v {
            match arr.data.as_slice() {
                [one] => {
                    v = one.clone();
                    from_array = true;
                }
                _ => return Err(refused()),
            }
        }
        if matches!(v, Variant::Null) || crate::conv::is_missing(&v) {
            return match name {
                "SplitHorizontal" | "SplitVertical" => Ok(WindowPut::Value(Variant::R8(0.0))),
                "Zoom" | "WindowState" => Err(refused()),

                "Split" => Ok(WindowPut::Value(Variant::Bool(false))),
                "FreezePanes" => {
                    let frozen = self.view_of(b)?.1.frozen;
                    Ok(WindowPut::Value(Variant::Bool(!frozen)))
                }
                _ => Ok(WindowPut::Skip),
            };
        }
        let lcid = crate::conv::variant_locale(a.lcid);
        if is_bool {
            return Ok(WindowPut::Value(Variant::Bool(match v {
                Variant::Bool(b) => b,
                Variant::Empty => false,
                Variant::Bstr(t) => match t.to_ascii_lowercase().as_str() {
                    "true" => true,
                    "false" => false,
                    _ => return Err(refused()),
                },
                Variant::Cy(_) | Variant::Date(_) => return Err(refused()),
                v => match change_type(&v, VarType::R8, lcid, 0) {
                    Ok(Variant::R8(x)) => x != 0.0,
                    _ => return Err(refused()),
                },
            })));
        }

        if name == "Zoom" && matches!(v, Variant::Bool(_)) {
            return Ok(WindowPut::Value(v));
        }

        let text = matches!(v, Variant::Bstr(_)) || from_array;
        let n = match v {
            Variant::Bool(b) => f64::from(u8::from(b)),
            Variant::Empty => 0.0,
            Variant::Cy(_) | Variant::Date(_) if name == "WindowState" => {
                return Err(refused());
            }
            v => match change_type(&v, VarType::R8, lcid, 0) {
                Ok(Variant::R8(x)) => x,
                _ => return Err(refused()),
            },
        };
        if name == "WindowState" {
            let t = n.trunc();
            return Ok(WindowPut::Value(Variant::I4(match t as i32 {
                s @ (-4143 | -4137 | -4140) if n == t && !text => s,
                1 => -4140,
                2 => -4137,
                3 => -4143,
                _ => return Err(refused()),
            })));
        }
        if n < 0.0 && name.starts_with("Split") {
            return Err(refused());
        }
        Ok(WindowPut::Value(Variant::R8(n)))
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
    bgr_of(PALETTE[(index - 1) as usize])
}

pub(crate) fn bgr_of(rgb: u32) -> u32 {
    ((rgb & 0xFF) << 16) | (rgb & 0xFF00) | (rgb >> 16)
}

impl Application<'_> {
    pub(crate) fn break_window(&self, b: BookId, s: SheetId) -> (u32, u32) {
        self.break_window_at(b, s, None, false)
    }

    pub(crate) fn break_window_idle(&self, b: BookId, s: SheetId) -> (u32, u32) {
        self.break_window_at(b, s, None, true)
    }

    fn break_window_at(&self, b: BookId, s: SheetId, zoom: Option<f64>, exact: bool) -> (u32, u32) {
        let Ok(book) = self.book(b) else {
            return (u32::MAX, u32::MAX);
        };
        if zoom.is_none() && matches!(book.window.view_mode, 2 | 3) {
            return (u32::MAX, u32::MAX);
        }
        let v = book.window.views.get(&s).cloned().unwrap_or_default();
        let z = zoom.unwrap_or(v.zoom);

        let wb = &book.wb;
        let first = v.scroll_row.saturating_sub(1);
        let rows =
            if !exact || (wb.row_custom_heights(s).is_empty() && wb.hidden_rows(s).is_empty()) {
                xlcalc::window_rows(z)
            } else {
                xlcalc::window_rows_with(z, &|r| {
                    let row = first + r;
                    if wb.row_hidden(s, row) {
                        0
                    } else {
                        (wb.row_h8(s, row) + 2) / 4
                    }
                })
            };
        let cols = if !exact || v.formulas || wb.column_state(s).is_empty() {
            (WINDOW_WIDTH / (col_points(&v) * z / 100.0)).ceil() as u32
        } else {
            let first = v.scroll_col.saturating_sub(1);
            let (avail, zi) = ((WINDOW_WIDTH * 2.0) as i64, z.round() as i64);
            let (mut acc, mut n) = (0_i64, 0_u32);
            while acc < avail && first + n < 16_384 {
                let c = first + n;
                if !wb.col_hidden(s, c) {
                    acc += (i64::from(wb.col_px(s, c)) * zi + 50) / 100;
                }
                n += 1;
            }
            n.saturating_sub(1)
        };
        (
            v.scroll_row.saturating_sub(1) + 2 * rows + 2,
            v.scroll_col.saturating_sub(1) + 2 * cols + 2,
        )
    }

    pub(crate) fn refresh_break_list_over(&mut self, b: BookId, s: SheetId, win: (u32, u32)) {
        let mut p = self.page_setup(b, s);
        p.list_zoom = p.zoom.unwrap_or(0);
        p.list_win = Some(win);
        if let Ok(bk) = self.book_mut(b) {
            bk.page_setup.insert(s, p);
        }
    }

    fn palette_bgr_in(&self, b: BookId, i: i32) -> Result<u32, OmError> {
        let theme = &self.book(b)?.wb.style_table().theme;
        Ok(theme
            .indexed(u8::try_from(i + 7).unwrap_or(0))
            .map_or_else(|| palette_bgr(i), bgr_of))
    }

    fn view_of(&self, b: BookId) -> Result<(SheetId, SheetView), OmError> {
        let book = self.book(b)?;
        let sheet = book.wb.active_sheet();
        let fresh = SheetView {
            formulas: book.wb.sheet(sheet).is_some_and(|s| s.kind.is_macro()),
            ..SheetView::default()
        };
        Ok((
            sheet,
            book.window.views.get(&sheet).cloned().unwrap_or(fresh),
        ))
    }

    fn set_view(&mut self, b: BookId, f: impl FnOnce(&mut SheetView)) -> Result<(), OmError> {
        let book = self.book_mut(b)?;
        let sheet = book.wb.active_sheet();
        let macro_sheet = book.wb.sheet(sheet).is_some_and(|s| s.kind.is_macro());
        f(book.window.views.entry(sheet).or_insert_with(|| SheetView {
            formulas: macro_sheet,
            ..SheetView::default()
        }));
        Ok(())
    }

    fn set_panes(
        &mut self,
        b: BookId,
        f: impl FnOnce(&mut SheetView, &Lines, &Lines),
    ) -> Result<(), OmError> {
        let (sheet, mut view) = self.view_of(b)?;
        let book = self.book(b)?;
        f(
            &mut view,
            &Lines::rows(&book.wb, sheet),
            &Lines::cols(&book.wb, sheet),
        );
        self.book_mut(b)?.window.views.insert(sheet, view);
        Ok(())
    }

    fn set_view_selected(&mut self, b: BookId, f: impl Fn(&mut SheetView)) -> Result<(), OmError> {
        let sheets = self.selected_sheets(b)?;
        let book = self.book_mut(b)?;
        for sheet in sheets {
            let macro_sheet = book.wb.sheet(sheet).is_some_and(|s| s.kind.is_macro());
            f(book.window.views.entry(sheet).or_insert_with(|| SheetView {
                formulas: macro_sheet,
                ..SheetView::default()
            }));
        }
        Ok(())
    }

    pub(crate) fn freeze_at(&mut self, b: BookId, rows: u32, cols: u32) -> Result<(), OmError> {
        self.set_panes(b, |v, rl, cl| {
            let was_frozen = v.frozen;
            v.unfreeze(rl, cl);
            let at = |l: &Lines, scroll: u32, n: u32| l.position(l.first_shown(scroll), n.into());
            v.split_row = at(rl, v.scroll_row, rows);
            v.split_col = at(cl, v.scroll_col, cols);
            v.split_by_freeze = false;
            v.pane4 = false;
            if rows == 0 && cols == 0 && !was_frozen {
                (v.scroll_row, v.scroll_col) = (1, 1);
            }
            v.freeze(rl, cl);
        })
    }

    pub(crate) fn pane_flags(&self, b: BookId) -> Result<(f64, bool, bool), OmError> {
        let v = self.view_of(b)?.1;
        let (row, col) = (v.split_row > 0.0, v.split_col > 0.0);
        Ok(match (v.frozen, row, col) {
            (true, true, false) => (3.0, false, true),
            (true, true, true) if v.pane4 => (4.0, true, true),
            (true, _, true) => (2.0, true, false),
            (false, r, c) => (1.0, c, r),
            (true, false, false) => (1.0, false, false),
        })
    }

    pub(crate) fn zoom_to_selection(&mut self, b: BookId) -> Result<(), OmError> {
        let book = self.book(b)?;
        let sheet = book.wb.active_sheet();
        let sel = book.wb.selection_of(sheet);
        let Some(first) = sel.areas.first().copied() else {
            return Ok(());
        };
        let bound = sel.areas.iter().fold(first, |a, r| {
            Rect::new(
                a.r0.min(r.r0),
                a.c0.min(r.c0),
                a.r1.max(r.r1),
                a.c1.max(r.c1),
            )
        });
        let wb = &book.wb;
        let whole = bound.r0 == 0 && bound.r1 >= 1_048_575 && bound.c0 == 0 && bound.c1 >= 16_383;
        let z = if whole {
            10
        } else {
            let row_hp = |r: u32| -> i64 {
                if wb.row_hidden(sheet, r) {
                    0
                } else {
                    (i64::from(wb.row_h8(sheet, r)) + 2) / 4
                }
            };
            let col_hp = |c: u32| -> i64 {
                if wb.col_hidden(sheet, c) {
                    0
                } else {
                    i64::from(wb.col_px(sheet, c))
                }
            };
            let at = |z: i64, f: &dyn Fn(u32) -> i64, lo: u32, hi: u32| -> i64 {
                (lo..=hi).map(|i| (f(i) * z * 2 + 100) / 200).sum()
            };
            let header = |z: i64| xlcalc::window_header_hp(z as u32);
            let (avail_h, avail_w) = (1376_i64, (WINDOW_WIDTH * 2.0) as i64);
            let rows = |z: i64| at(z, &row_hp, bound.r0, bound.r1) + header(z);
            let cols = |z: i64| at(z, &col_hp, bound.c0, bound.c1);
            let est = |avail: i64, extent: i64| {
                if extent > 0 {
                    (100 * avail + extent / 2) / extent
                } else {
                    i64::from(i32::MAX)
                }
            };
            let mut z = est(avail_h, rows(100)).min(est(avail_w, cols(100)));
            while z < 400 && z > 10 && !(rows(z) <= avail_h && cols(z) <= avail_w) {
                z -= 1;
            }
            z.clamp(10, 400)
        };
        match self.book(b)?.window.view_mode {
            2 => self.book_mut(b)?.window.pbp_zoom = z as f64,
            3 => self.book_mut(b)?.window.pl_zoom = z as f64,
            _ => return self.set_view(b, |v| v.zoom = z as f64),
        }
        Ok(())
    }

    pub(crate) fn set_window_state(&mut self, b: BookId, s: i32) -> Result<(), OmError> {
        let w = &mut self.book_mut(b)?.window;
        if s == -4140 && w.state != -4140 {
            w.before_min = w.state;
        }
        w.state = s;
        Ok(())
    }

    pub(crate) fn restore_window(&mut self, b: BookId) -> Result<(), OmError> {
        let w = &mut self.book_mut(b)?.window;
        w.state = if w.state == -4140 {
            w.before_min
        } else {
            -4143
        };
        Ok(())
    }

    pub(crate) fn set_gridline_auto(&mut self, b: BookId) -> Result<(), OmError> {
        self.set_view(b, |v| {
            v.color = 0;
            v.color_set = false;
            v.color_rgb = false;
            v.color_index = -4105;
        })
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
        let norm;
        let a = if set {
            match self.window_put_arg(b, m, a)? {
                WindowPut::Skip => return Ok(Variant::Empty),
                WindowPut::Value(v) => {
                    norm = Args {
                        values: a.values.clone(),
                        put: Some(v),
                        lcid: a.lcid,
                    };
                    &norm
                }
                WindowPut::Pass => a,
            }
        } else {
            a
        };
        match (m.name, m.kind) {
            ("Type", Get) => Ok(Variant::I4(1)),
            ("WindowState", Get) => Ok(Variant::I4(self.book(b)?.window.state)),
            ("WindowState", Put) => {
                let s = self.put_i32(a)?;
                self.set_window_state(b, s)?;
                Ok(Variant::Empty)
            }
            ("Zoom", Get) if self.book(b)?.window.view_mode == 2 => {
                Ok(Variant::R8(self.book(b)?.window.pbp_zoom))
            }
            ("Zoom", Get) if self.book(b)?.window.view_mode == 3 => {
                Ok(Variant::R8(self.book(b)?.window.pl_zoom))
            }
            ("Zoom", Get) => Ok(Variant::R8(self.view_of(b)?.1.zoom)),
            ("Zoom", Put) => {
                if let Some(Variant::Bool(fit)) = a.put {
                    if fit {
                        self.zoom_to_selection(b)?;
                    }
                    return Ok(Variant::Empty);
                }
                let z = self.put_f64(a)?.trunc();
                if !(10.0..=400.0).contains(&z) {
                    return Err(unable(m, true));
                }
                match self.book(b)?.window.view_mode {
                    2 => self.book_mut(b)?.window.pbp_zoom = z,
                    3 => self.book_mut(b)?.window.pl_zoom = z,
                    _ => self.set_view_selected(b, |v| v.zoom = z)?,
                }
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
                let set = |v: &mut SheetView| match name {
                    "DisplayGridlines" => v.gridlines = on,
                    "DisplayHeadings" => v.headings = on,
                    "DisplayFormulas" => v.formulas = on,
                    _ => v.zeros = on,
                };

                if name == "DisplayGridlines" {
                    self.set_view_selected(b, set)?;
                } else {
                    self.set_view(b, set)?;
                }
                Ok(Variant::Empty)
            }
            ("GridlineColor", Get) => Ok(Variant::R8(f64::from(self.view_of(b)?.1.color))),
            ("GridlineColor", Put) => {
                let c = self.put_f64(a)?;

                if !(0.0..=4_294_967_295.0).contains(&c) {
                    return Err(unable(m, true));
                }
                let c = (c as u32) & 0xFF_FFFF;
                let rgb = ((c & 0xFF) << 16) | (c & 0xFF00) | (c >> 16);

                let idx = i32::from(nearest_color_index(rgb));
                let c = self.palette_bgr_in(b, idx)?;
                self.set_view(b, |v| {
                    v.color = c;
                    v.color_set = true;
                    v.color_rgb = true;
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
                let i = if matches!(a.put, Some(Variant::Null | Variant::Empty) | None) {
                    0
                } else {
                    self.put_i32(a)?
                };
                if i == 0 {
                    self.set_gridline_auto(b)?;
                    return Ok(Variant::Empty);
                }

                if i == -4105 && self.view_of(b)?.1.color_rgb {
                    self.set_gridline_auto(b)?;
                    return Ok(Variant::Empty);
                }
                if !(1..=56).contains(&i) {
                    return Err(unable(m, true));
                }
                let c = self.palette_bgr_in(b, i)?;
                self.set_view(b, |v| {
                    v.color = c;
                    v.color_set = true;
                    v.color_rgb = false;
                    v.color_index = i;
                })?;
                Ok(Variant::Empty)
            }
            ("ScrollRow" | "ScrollColumn", Get) => {
                let (sheet, v) = self.view_of(b)?;
                let wb = &self.book(b)?.wb;
                let rows = m.name == "ScrollRow";
                let lines = if rows {
                    Lines::rows(wb, sheet)
                } else {
                    Lines::cols(wb, sheet)
                };
                Ok(Variant::R8(f64::from(v.shown_scroll(rows, &lines))))
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
                self.set_view(b, |v| v.scroll_to(row, n))?;
                Ok(Variant::Empty)
            }
            ("SplitRow" | "SplitColumn" | "SplitHorizontal" | "SplitVertical", Get) => {
                let (sheet, v) = self.view_of(b)?;
                let (rows, unit, points) = geometry(&v, m.name);
                let wb = &self.book(b)?.wb;
                let lines = if rows {
                    Lines::rows(wb, sheet)
                } else {
                    Lines::cols(wb, sheet)
                };
                let (count, size) = v.split_read(rows, &lines);
                let split = if rows { v.split_row } else { v.split_col };
                Ok(if split <= 0.0 {
                    Variant::I4(0)
                } else if points {
                    Variant::R8((size * unit * 1e6).round() / 1e6)
                } else {
                    Variant::R8(count)
                })
            }
            ("SplitRow" | "SplitColumn" | "SplitHorizontal" | "SplitVertical", Put) => {
                let n = self.put_f64(a)?;
                let (sheet, v) = self.view_of(b)?;
                let (rows, unit, points) = geometry(&v, m.name);
                let max = match m.name {
                    "SplitRow" => 1_048_576.0,
                    "SplitColumn" => 16_384.0,
                    _ => f64::from(i32::MAX),
                };
                if !(0.0..=max).contains(&n.trunc()) {
                    return Err(unable(m, true));
                }

                let count = if points {
                    0.0
                } else {
                    let wb = &self.book(b)?.wb;
                    let lines = if rows {
                        Lines::rows(wb, sheet)
                    } else {
                        Lines::cols(wb, sheet)
                    };
                    let first = match (v.frozen, rows) {
                        (true, true) if v.split_row > 0.0 => v.pane_row,
                        (true, false) if v.split_col > 0.0 => v.pane_col,
                        (_, true) => v.scroll_row,
                        (_, false) => v.scroll_col,
                    };
                    lines.position(lines.first_shown(first), n)
                };

                let half = if points {
                    (n * 2.0).floor()
                } else {
                    (count * unit * 2.0 - 1e-9).ceil()
                };
                let pos = if !points && count.fract() == 0.0 {
                    count
                } else {
                    half / 2.0 / unit
                };
                let edge = if rows { WINDOW_HEIGHT } else { WINDOW_WIDTH } / unit;
                let far = pos * unit > 16384.0;
                let pos = if half < 1.0 || far {
                    0.0
                } else {
                    if pos > edge - 1.0 / unit {
                        edge
                    } else {
                        pos
                    }
                };
                self.set_panes(b, |v, rl, cl| {
                    v.unfreeze(rl, cl);
                    if rows {
                        v.split_row = pos;
                    } else {
                        v.split_col = pos;
                    }
                    v.split_by_freeze = false;
                })?;
                Ok(Variant::Empty)
            }
            ("Split", Get) => {
                let v = self.view_of(b)?.1;
                Ok(Variant::Bool(v.split_row > 0.0 || v.split_col > 0.0))
            }
            ("Split", Put) => {
                let on = self.put_bool(a)?;
                let v = self.view_of(b)?.1;
                if !on {
                    if v.frozen && v.split_by_freeze {
                        return self
                            .set_panes(b, |v, rl, cl| {
                                v.unfreeze(rl, cl);
                                v.split_by_freeze = false;
                            })
                            .map(|()| Variant::Empty);
                    }
                    self.set_panes(b, |v, rl, cl| {
                        if v.frozen {
                            v.unfreeze(rl, cl);
                        } else {
                            (v.scroll_row, v.scroll_col) = (1, 1);
                        }
                        v.split_row = 0.0;
                        v.split_col = 0.0;
                        v.split_by_freeze = false;
                    })?;
                } else if v.split_row <= 0.0 && v.split_col <= 0.0 {
                    let (rows, cols) = half_window(&v);
                    self.set_view(b, |v| {
                        v.split_row = rows;
                        v.split_col = cols;
                    })?;
                }
                Ok(Variant::Empty)
            }
            ("FreezePanes", Get) => Ok(Variant::Bool(self.view_of(b)?.1.frozen)),
            ("FreezePanes", Put) => {
                if self.book(b)?.window.state == -4140 {
                    return Err(unable(m, true));
                }

                if self.book(b)?.window.view_mode == 3 {
                    return Err(unable(m, true));
                }
                let on = self.put_bool(a)?;
                let (r, c) = self.book(b)?.wb.active_cell();
                self.set_panes(b, |v, rl, cl| {
                    let at_cell = v.split_row <= 0.0 && v.split_col <= 0.0;
                    if !on && v.frozen {
                        v.unfreeze(rl, cl);
                        if v.split_by_freeze {
                            v.split_row = 0.0;
                            v.split_col = 0.0;
                            v.split_by_freeze = false;
                        }
                    }
                    if on && !v.frozen {
                        if at_cell {
                            let to = |l: &Lines, scroll: u32, cell: u32| {
                                let first = l.first_shown(scroll);
                                if cell > first {
                                    l.position(first, f64::from(cell - first))
                                } else {
                                    0.0
                                }
                            };
                            let scale = v.zoom / 100.0;
                            (v.split_row, v.split_col) = freeze_point(
                                (to(rl, v.scroll_row, r + 1), to(cl, v.scroll_col, c + 1)),
                                (
                                    WINDOW_HEIGHT / (ROW_POINTS * scale),
                                    WINDOW_WIDTH / (col_points(v) * scale),
                                ),
                                half_window(v),
                            );
                            v.split_by_freeze = true;
                        }
                        v.freeze(rl, cl);
                    }
                    v.pane4 = on && v.frozen && !at_cell && v.split_row > 0.0 && v.split_col > 0.0;
                })?;
                Ok(Variant::Empty)
            }
            ("View", Get) => Ok(Variant::I4(self.book(b)?.window.view_mode)),
            ("View", Put) => {
                let v = self.put_i32(a)?;
                if !(1..=3).contains(&v) {
                    return Err(OmError::app_defined());
                }
                let (old, sheet) = {
                    let bk = self.book(b)?;
                    (bk.window.view_mode, bk.wb.active_sheet())
                };

                let leaving = if old == 2 && v == 1 {
                    Some(self.book(b)?.window.pbp_zoom)
                } else {
                    None
                };
                self.book_mut(b)?.window.view_mode = v;
                if old != v {
                    let win = self.break_window_at(b, sheet, leaving, false);
                    self.refresh_break_list_over(b, sheet, win);
                }
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

            ("DisplayRightToLeft" | "_DisplayRightToLeft", _) => {
                let sheet = self.book(b)?.wb.active_sheet();
                self.sheet_stored_option(b, sheet, m, a)
                    .unwrap_or_else(|| Err(unsupported("Window", m)))
            }
            (
                "DisplayOutline"
                | "DisplayHorizontalScrollBar"
                | "DisplayVerticalScrollBar"
                | "DisplayWorkbookTabs"
                | "EnableResize"
                | "Visible"
                | "DisplayRuler"
                | "DisplayWhitespace"
                | "AutoFilterDateGrouping",
                _,
            ) => {
                let name = if m.name == "_DisplayRightToLeft" {
                    "DisplayRightToLeft"
                } else {
                    m.name
                };
                if set {
                    let on = match self.put_bool(a) {
                        Ok(on) => on,
                        Err(_) if name == "AutoFilterDateGrouping" => {
                            return Err(OmError::arg(crate::error::DISP_E_TYPEMISMATCH, 0));
                        }
                        Err(e) => return Err(e),
                    };
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
        Ok((1 + u32::from(v.split_row > 0.0)) * (1 + u32::from(v.split_col > 0.0)))
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

                    _ => Err(OmError::app_defined()),
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
            ("VisibleRange", InvokeKind::Get) => self.window_member(b, n, m, a),
            ("ScrollRow" | "ScrollColumn", InvokeKind::Get) => {
                let v = self.view_of(b)?.1;
                let rows = v.split_row > 0.0;
                let cols = v.split_col > 0.0;
                let (top, left) = match (rows, cols, i) {
                    (true, true, 1) => (true, true),
                    (true, true, 2) => (true, false),
                    (true, true, 3) => (false, true),
                    (true, false, 1) => (true, false),
                    (false, true, 1) => (false, true),
                    _ => (false, false),
                };
                if v.frozen && m.name == "ScrollRow" && top {
                    return Ok(Variant::R8(v.pane_row.into()));
                }
                if v.frozen && m.name == "ScrollColumn" && left {
                    return Ok(Variant::R8(v.pane_col.into()));
                }
                self.window_view_member(b, m, a)
            }
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

impl SheetView {
    pub(crate) fn visible_rect(&self, rows: &Lines, cols: &Lines) -> xlcalc::Rect {
        let total_r = xlcalc::window_rows(self.zoom);
        let total_c = (WINDOW_WIDTH / (col_points(self) * self.zoom / 100.0)).ceil() as u32;
        let (r0, r1) = self.visible_lines(true, total_r, rows);
        let (c0, c1) = self.visible_lines(false, total_c, cols);
        xlcalc::Rect::new(r0, c0, r1.min(1_048_575), c1.min(16_383))
    }
}
