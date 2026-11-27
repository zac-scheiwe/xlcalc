use xlcalc::style::CellColor;
use xlcalc::{SheetId, SheetKind, Workbook};
use xlcalc_variant::Variant;
use xlcalc_xlsx::{Package, PageUi, SheetUi, TabColor, ViewUi};

use crate::addins::{doc_props, DocProps};
use crate::app::{Application, BookId};
use crate::pagesetup::PageSetup;
use crate::panes::Lines;
use crate::window::{palette_bgr, SheetView};

const AUTOMATIC: i32 = -4105;
const COMMENTS: [i32; 3] = [-4142, 1, 16];

const OUTLINE_BUTTON: u32 = 53;

struct SplitPixels<'a> {
    before: (f64, f64),
    rows: Lines<'a>,
    cols: Lines<'a>,
}

struct Shown {
    zoom: u32,
    headings: bool,
    formulas: bool,
    outline: bool,
    scroll_row: u32,
}

fn split_pixels(wb: &Workbook, sheet: SheetId, shown: Shown) -> SplitPixels<'_> {
    let zoom = shown.zoom.clamp(10, 400);
    let scaled = move |px: u32| (px * zoom * 2 + 100) / 200;

    let row = move |r: u32| {
        if wb.row_hidden(sheet, r) {
            0
        } else {
            (wb.row_h8(sheet, r) + 2) / 4
        }
    };

    let first = Lines::rows(wb, sheet).first_shown(shown.scroll_row) - 1;
    let count = xlcalc::window_rows_with(f64::from(zoom), &|r| row(first + r));
    let (header_x, header_y) = wb.window_headers_px(zoom, first + count - 1);

    let std_row = scaled((wb.standard_height(sheet) * 2.0).round() as u32);
    let button = std_row.min(header_y).saturating_sub(1).min(OUTLINE_BUTTON);
    let gutter = |rows: bool| {
        let deepest = wb.outlined_lines(sheet, rows).iter().map(|l| l.1).max();
        match deepest {
            Some(level) if shown.outline && level > 1 => level * button + 5,
            _ => 0,
        }
    };
    let heading = |px: u32| if shown.headings { px } else { 0 };
    let widen = if shown.formulas { 2 } else { 1 };
    SplitPixels {
        before: (
            f64::from(gutter(false) + heading(header_y)),
            f64::from(gutter(true) + heading(header_x)),
        ),
        rows: Lines::sized(move |n| f64::from(scaled(row(n - 1))), 1_048_576),
        cols: Lines::sized(
            move |n| {
                if wb.col_hidden(sheet, n - 1) {
                    0.0
                } else {
                    f64::from(scaled(widen * wb.col_px(sheet, n - 1)))
                }
            },
            16_384,
        ),
    }
}

fn split_from_px(px: f64, before: f64, scroll: u32, lines: &Lines, pixels: &Lines) -> f64 {
    let first = lines.first_shown(scroll);
    lines.position(first, pixels.count_at(first, px - before))
}

fn split_to_px(split: f64, before: f64, scroll: u32, lines: &Lines, pixels: &Lines) -> f64 {
    if split <= 0.0 {
        return 0.0;
    }
    let first = lines.first_shown(scroll);

    before + (pixels.position(first, lines.count_at(first, split)) - 1e-9).ceil()
}

fn view_in(v: &ViewUi, wb: &Workbook, sheet: SheetId) -> SheetView {
    let (mut split_row, mut split_col) = (v.split_row, v.split_col);
    if !v.frozen && (split_row > 0.0 || split_col > 0.0) {
        let shown = Shown {
            zoom: v.zoom,
            headings: v.headings,
            formulas: v.formulas,
            outline: v.outline_symbols != Some(false),
            scroll_row: v.scroll_row,
        };
        let px = split_pixels(wb, sheet, shown);
        let (rows, cols) = (Lines::rows(wb, sheet), Lines::cols(wb, sheet));
        split_row = split_from_px(split_row, px.before.0, v.scroll_row, &rows, &px.rows);
        split_col = split_from_px(split_col, px.before.1, v.scroll_col, &cols, &px.cols);
    }
    let mut out = SheetView {
        zoom: f64::from(v.zoom),
        gridlines: v.gridlines,
        headings: v.headings,
        formulas: v.formulas,
        zeros: v.zeros,
        split_row,
        split_col,
        split_by_freeze: v.frozen,
        frozen: v.frozen,
        scroll_row: v.scroll_row,
        scroll_col: v.scroll_col,
        pane_row: v.pane_row,
        pane_col: v.pane_col,
        ..SheetView::default()
    };
    if let Some(i) = v.grid_color.and_then(|c| i32::try_from(c).ok()) {
        if (8..=63).contains(&i) {
            out.color = palette_bgr(i - 7);
            out.color_set = true;
            out.color_index = i - 7;
        }
    }
    out
}

fn view_out(v: &SheetView, wb: &Workbook, sheet: SheetId, outline: bool) -> ViewUi {
    let (rows, cols) = (&Lines::rows(wb, sheet), &Lines::cols(wb, sheet));

    let axis = |split: f64, scroll: u32, pane: u32, lines: &Lines| {
        if split <= 0.0 {
            let at = if v.frozen {
                lines.first_shown(scroll)
            } else {
                scroll
            };
            (at, 1, 0)
        } else if v.frozen {
            (scroll, pane, 0)
        } else {
            let first = lines.first_shown(scroll);
            (first, 1, lines.next_pane(first, split))
        }
    };
    let (scroll_row, pane_row, next_row) = axis(v.split_row, v.scroll_row, v.pane_row, rows);
    let (scroll_col, pane_col, next_col) = axis(v.split_col, v.scroll_col, v.pane_col, cols);
    let (mut split_row, mut split_col) = (v.split_row, v.split_col);
    if !v.frozen && (split_row > 0.0 || split_col > 0.0) {
        let shown = Shown {
            zoom: v.zoom.round() as u32,
            headings: v.headings,
            formulas: v.formulas,
            outline,
            scroll_row,
        };
        let px = split_pixels(wb, sheet, shown);
        split_row = split_to_px(split_row, px.before.0, scroll_row, rows, &px.rows);
        split_col = split_to_px(split_col, px.before.1, scroll_col, cols, &px.cols);
    }
    ViewUi {
        zoom: v.zoom.round() as u32,
        mode: None,
        mode_zoom: None,
        right_to_left: None,
        outline_symbols: None,
        gridlines: v.gridlines,
        headings: v.headings,
        formulas: v.formulas,
        zeros: v.zeros,
        grid_color: (v.color_index != AUTOMATIC).then(|| (v.color_index + 7) as u32),
        split_row,
        split_col,
        frozen: v.frozen,
        scroll_row,
        scroll_col,
        pane_row,
        pane_col,
        split_next: (next_row, next_col),
    }
}

fn page_in(p: &PageUi) -> PageSetup {
    let [left, right, top, bottom, header_margin, footer_margin] = p.margins;
    let int = |v: Option<u32>| v.and_then(|v| i32::try_from(v).ok());
    PageSetup {
        left,
        right,
        top,
        bottom,
        header_margin,
        footer_margin,
        orientation: f64::from(p.orientation),
        paper_size: f64::from(p.paper_size),
        zoom: int(p.zoom),
        fit_wide: int(p.fit_wide),
        fit_tall: int(p.fit_tall),
        first_page: p.first_page.map(|v| v as f64),
        order: if p.over_then_down { 2.0 } else { 1.0 },
        black_and_white: p.black_and_white,
        draft: p.draft,
        comments: COMMENTS
            .get(p.comments as usize)
            .map_or(COMMENTS[0], |c| *c),
        errors: p.errors as i32,
        center_horizontally: p.center_horizontally,
        center_vertically: p.center_vertically,
        gridlines: p.gridlines,
        headings: p.headings,
        header: p.header.clone(),
        footer: p.footer.clone(),
        odd_even: p.odd_even,
        different_first: p.different_first,
        scale_with_doc: p.scale_with_doc,
        align_margins: p.align_margins,
        list_zoom: int(p.zoom).unwrap_or(0),
        ..PageSetup::default()
    }
}

fn page_out(p: &PageSetup) -> PageUi {
    let int = |v: Option<i32>| v.and_then(|v| u32::try_from(v).ok());
    PageUi {
        margins: [
            p.left,
            p.right,
            p.top,
            p.bottom,
            p.header_margin,
            p.footer_margin,
        ],
        orientation: p.orientation as u32,
        paper_size: p.paper_size as u32,
        zoom: int(p.zoom),
        fit_wide: int(p.fit_wide),
        fit_tall: int(p.fit_tall),
        first_page: p.first_page.map(|v| v as i64),
        over_then_down: p.order as i32 == 2,
        black_and_white: p.black_and_white,
        draft: p.draft,
        comments: COMMENTS.iter().position(|c| *c == p.comments).unwrap_or(0) as u32,
        errors: p.errors.max(0) as u32,
        center_horizontally: p.center_horizontally,
        center_vertically: p.center_vertically,
        gridlines: p.gridlines,
        headings: p.headings,
        header: p.header.clone(),
        footer: p.footer.clone(),
        odd_even: p.odd_even,
        different_first: p.different_first,
        scale_with_doc: p.scale_with_doc,
        align_margins: p.align_margins,
    }
}

fn tab_keys(b: BookId, s: SheetId) -> [String; 3] {
    ["TabRgb", "TabTheme", "TabTint"].map(|k| format!("ws{}:{}:{k}", b.0, s.0))
}

impl Application<'_> {
    pub(crate) fn load_ui(&mut self, id: BookId, package: &Package) {
        let props = doc_props(package);
        if let Ok(book) = self.book_mut(id) {
            book.props = props;
        }
        let Ok(book) = self.book(id) else {
            return;
        };
        let theme = book.wb.style_table().theme.clone();
        let active = book.wb.active_sheet();
        let sheets: Vec<(SheetId, bool)> = book
            .wb
            .sheets()
            .filter(|s| s.kind != SheetKind::Chart)
            .map(|s| (s.id, s.kind.is_macro()))
            .collect();
        for (sheet, is_macro) in sheets {
            let Some(ui) = xlcalc_xlsx::read_sheet_ui(package, sheet) else {
                continue;
            };
            if let Some(tab) = &ui.tab {
                let rgb = match (tab.rgb, tab.theme, tab.indexed) {
                    (Some(v), _, _) => Some(v),
                    (None, Some(t), _) => u8::try_from(t)
                        .ok()
                        .map(|t| theme.rgb_cv(CellColor::theme(t, tab.tint), 0)),
                    (None, None, Some(i)) => u8::try_from(i).ok().and_then(|i| theme.indexed(i)),
                    _ => None,
                };
                if let Some(rgb) = rgb {
                    let [key, tkey, skey] = tab_keys(id, sheet);
                    self.opts.insert(key, Variant::I4(rgb as i32));
                    if let Some(t) = tab.theme {
                        self.opts.insert(tkey, Variant::I4(t as i32 + 1));
                    }
                    if tab.tint != 0.0 {
                        self.opts.insert(skey, Variant::R8(tab.tint));
                    }
                }
            }
            let Ok(book) = self.book_mut(id) else {
                return;
            };
            let plain = ViewUi {
                formulas: is_macro,
                ..ViewUi::default()
            };
            if ui.view != plain {
                let view = view_in(&ui.view, &book.wb, sheet);
                book.window.views.insert(sheet, view);
            }
            if sheet == active {
                let mode = ui.view.mode.unwrap_or(1);
                book.window.view_mode = i32::from(mode);
                match (mode, ui.view.mode_zoom) {
                    (2, Some(z)) => book.window.pbp_zoom = f64::from(z),
                    (3, Some(z)) => book.window.pl_zoom = f64::from(z),
                    _ => {}
                }
                for (name, on) in &mut book.window.flags {
                    match *name {
                        "DisplayRightToLeft" => *on = ui.view.right_to_left == Some(true),
                        "DisplayOutline" => *on = ui.view.outline_symbols != Some(false),
                        _ => {}
                    }
                }
            }
            if ui.page != PageUi::default() {
                book.page_setup.insert(sheet, page_in(&ui.page));
            }
        }
    }

    pub(crate) fn ui_for_save(&self, b: BookId) -> Vec<(SheetId, SheetUi)> {
        let Ok(book) = self.book(b) else {
            return Vec::new();
        };
        let active = book.wb.active_sheet();
        let window = &book.window;
        let on = |name: &str| window.flags.iter().any(|f| f.0 == name && f.1);
        book.wb
            .sheets()
            .filter(|s| s.kind != SheetKind::Chart)
            .map(|s| {
                let [key, tkey, skey] = tab_keys(b, s.id);
                let tint = match self.opts.get(&skey) {
                    Some(Variant::R8(t)) => *t,
                    _ => 0.0,
                };
                let tab = match self.opts.get(&key) {
                    Some(Variant::I4(rgb)) => Some(match self.opts.get(&tkey) {
                        Some(Variant::I4(t)) => TabColor {
                            rgb: None,
                            theme: Some((*t - 1).max(0) as u32),
                            indexed: None,
                            tint,
                        },
                        _ => TabColor {
                            rgb: Some(*rgb as u32),
                            theme: None,
                            indexed: None,
                            tint: 0.0,
                        },
                    }),
                    _ => None,
                };
                let shown = window
                    .views
                    .get(&s.id)
                    .cloned()
                    .unwrap_or_else(|| SheetView {
                        formulas: s.kind.is_macro(),
                        ..SheetView::default()
                    });
                let outline = s.id != active || on("DisplayOutline");
                let mut view = view_out(&shown, &book.wb, s.id, outline);
                if s.id == active {
                    let mode = window.view_mode.clamp(1, 3) as u8;
                    view.mode = Some(mode);
                    view.mode_zoom = match mode {
                        2 => Some(window.pbp_zoom.round() as u32),
                        3 => Some(window.pl_zoom.round() as u32),
                        _ => None,
                    };
                    view.right_to_left = Some(on("DisplayRightToLeft"));
                    view.outline_symbols = Some(on("DisplayOutline"));
                }
                let page = book
                    .page_setup
                    .get(&s.id)
                    .map_or_else(PageUi::default, page_out);
                (s.id, SheetUi { tab, view, page })
            })
            .collect()
    }

    pub(crate) fn properties_for_save(&self, b: BookId) -> Option<[String; 5]> {
        let book = self.book(b).ok()?;
        let p = &book.props;
        let mut out = [
            p.title.clone(),
            p.subject.clone(),
            p.author.clone(),
            p.keywords.clone(),
            p.comments.clone(),
        ];
        if book.package.is_none() {
            let d = DocProps::default();
            let fresh = [d.title, d.subject, d.author, d.keywords, d.comments];
            for (v, was) in out.iter_mut().zip(fresh) {
                if *v == was {
                    v.clear();
                }
            }
        }
        Some(out)
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    fn shown(zoom: u32, scroll_row: u32) -> Shown {
        Shown {
            zoom,
            headings: true,
            formulas: false,
            outline: true,
            scroll_row,
        }
    }

    fn book() -> (Workbook, SheetId) {
        let wb = Workbook::new();
        let sheet = wb.active_sheet();
        (wb, sheet)
    }

    #[test]
    fn the_pixels_before_the_cells() {
        let (mut wb, s) = book();
        let before = |wb: &Workbook, shown| split_pixels(wb, s, shown).before;
        assert_eq!(before(&wb, shown(100, 1)), (29.0, 40.0));

        assert_eq!(before(&wb, shown(10, 1)), (7.0, 13.0));
        assert_eq!(before(&wb, shown(25, 1)), (9.0, 16.0));
        assert_eq!(before(&wb, shown(50, 1)), (14.0, 27.0));
        assert_eq!(before(&wb, shown(60, 1)), (18.0, 26.0));
        assert_eq!(before(&wb, shown(400, 1)), (115.0, 160.0));
        assert_eq!(before(&wb, shown(80, 1)), (24.0, 34.0));
        assert_eq!(before(&wb, shown(200, 1)), (57.0, 79.0));

        assert_eq!(before(&wb, shown(100, 953)), (29.0, 40.0));
        assert_eq!(before(&wb, shown(100, 954)), (29.0, 51.0));
        let bare = Shown {
            headings: false,
            ..shown(100, 1)
        };
        assert_eq!(before(&wb, bare), (0.0, 0.0));

        wb.set_outline_levels(s, true, 1, 2, 2);
        assert_eq!(before(&wb, shown(100, 1)), (29.0, 101.0));
        assert_eq!(before(&wb, shown(80, 1)), (24.0, 83.0));
        assert_eq!(before(&wb, shown(200, 1)), (57.0, 190.0));
        wb.set_outline_levels(s, false, 1, 2, 2);
        assert_eq!(before(&wb, shown(100, 1)), (90.0, 101.0));
        let hidden = Shown {
            outline: false,
            ..shown(100, 1)
        };
        assert_eq!(before(&wb, hidden), (29.0, 40.0));
    }

    #[test]
    fn splits_to_pixels_and_back() {
        let (wb, s) = book();
        let px = split_pixels(&wb, s, shown(100, 1));
        let (rows, cols) = (Lines::rows(&wb, s), Lines::cols(&wb, s));
        assert_eq!(split_to_px(3.0, px.before.1, 1, &cols, &px.cols), 328.0);
        assert_eq!(split_to_px(5.0, px.before.0, 1, &rows, &px.rows), 174.0);
        assert_eq!(split_from_px(328.0, px.before.1, 1, &cols, &px.cols), 3.0);
        assert_eq!(split_from_px(174.0, px.before.0, 1, &rows, &px.rows), 5.0);

        assert_eq!(split_to_px(2.5, 40.0, 1, &cols, &px.cols), 280.0);
        assert_eq!(split_to_px(100.0 / 48.0, 40.0, 1, &cols, &px.cols), 240.0);

        assert_eq!(split_from_px(40.0, 40.0, 1, &cols, &px.cols), 0.0);
        assert_eq!(split_to_px(0.0, 40.0, 1, &cols, &px.cols), 0.0);
    }
}
