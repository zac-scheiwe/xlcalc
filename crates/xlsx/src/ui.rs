use xlcalc::SheetId;

use crate::package::Package;
use crate::rawxml::{self, Split, Tag};
use crate::xml::{self, Node};

#[derive(Clone, Debug, Default, PartialEq)]
pub struct SheetUi {
    pub tab: Option<TabColor>,
    pub view: ViewUi,
    pub page: PageUi,
}

#[derive(Clone, Debug, PartialEq)]
pub struct TabColor {
    pub rgb: Option<u32>,
    pub theme: Option<u32>,
    pub indexed: Option<u32>,
    pub tint: f64,
}

#[derive(Clone, Debug, PartialEq)]
pub struct ViewUi {
    pub zoom: u32,

    pub mode: Option<u8>,

    pub mode_zoom: Option<u32>,
    pub right_to_left: Option<bool>,
    pub outline_symbols: Option<bool>,
    pub gridlines: bool,
    pub headings: bool,
    pub formulas: bool,
    pub zeros: bool,

    pub grid_color: Option<u32>,

    pub split_row: f64,
    pub split_col: f64,
    pub frozen: bool,

    pub scroll_row: u32,
    pub scroll_col: u32,

    pub pane_row: u32,
    pub pane_col: u32,

    pub split_next: (u32, u32),
}

impl Default for ViewUi {
    fn default() -> Self {
        ViewUi {
            zoom: 100,
            mode: Some(1),
            mode_zoom: None,
            right_to_left: Some(false),
            outline_symbols: Some(true),
            gridlines: true,
            headings: true,
            formulas: false,
            zeros: true,
            grid_color: None,
            split_row: 0.0,
            split_col: 0.0,
            frozen: false,
            scroll_row: 1,
            scroll_col: 1,
            pane_row: 1,
            pane_col: 1,
            split_next: (0, 0),
        }
    }
}

#[derive(Clone, Debug, PartialEq)]
pub struct PageUi {
    pub margins: [f64; 6],

    pub orientation: u32,
    pub paper_size: u32,

    pub zoom: Option<u32>,
    pub fit_wide: Option<u32>,
    pub fit_tall: Option<u32>,
    pub first_page: Option<i64>,
    pub over_then_down: bool,
    pub black_and_white: bool,
    pub draft: bool,

    pub comments: u32,

    pub errors: u32,
    pub center_horizontally: bool,
    pub center_vertically: bool,
    pub gridlines: bool,
    pub headings: bool,
    pub header: String,
    pub footer: String,
    pub odd_even: bool,
    pub different_first: bool,
    pub scale_with_doc: bool,
    pub align_margins: bool,
}

impl Default for PageUi {
    fn default() -> Self {
        PageUi {
            margins: [50.4, 50.4, 54.0, 54.0, 0.3 * 72.0, 0.3 * 72.0],
            orientation: 1,
            paper_size: 1,
            zoom: Some(100),
            fit_wide: Some(1),
            fit_tall: Some(1),
            first_page: None,
            over_then_down: false,
            black_and_white: false,
            draft: false,
            comments: 0,
            errors: 0,
            center_horizontally: false,
            center_vertically: false,
            gridlines: false,
            headings: false,
            header: String::new(),
            footer: String::new(),
            odd_even: false,
            different_first: false,
            scale_with_doc: true,
            align_margins: true,
        }
    }
}

const MARGINS: [&str; 6] = ["left", "right", "top", "bottom", "header", "footer"];
const MODES: [&str; 3] = ["normal", "pageBreakPreview", "pageLayout"];
const COMMENTS: [&str; 3] = ["none", "atEnd", "asDisplayed"];
const ERRORS: [&str; 4] = ["displayed", "blank", "dash", "NA"];

fn flag(n: Option<&Node>, key: &str, default: bool) -> bool {
    match n.and_then(|n| n.attr(key)) {
        Some("1" | "true") => true,
        Some("0" | "false") => false,
        _ => default,
    }
}

fn number<T: std::str::FromStr>(n: Option<&Node>, key: &str) -> Option<T> {
    n.and_then(|n| n.attr(key)).and_then(|v| v.parse().ok())
}

fn cell_at(text: Option<&str>) -> Option<(u32, u32)> {
    text.and_then(crate::parse_cell)
}

const DPI: f32 = 144.0;

fn split_px(twips: f64) -> f64 {
    (f64::from((twips.trunc() / 1440.0) as f32) * f64::from(DPI)).round()
}

fn split_twips(px: f64) -> f64 {
    (f64::from(px as f32 / DPI) * 1440.0).round().max(1.0)
}

fn read_view(sheet: &Node) -> ViewUi {
    let mut out = ViewUi::default();
    let Some(v) = sheet.child("sheetViews").and_then(|s| s.child("sheetView")) else {
        return out;
    };
    let v = Some(v);
    let mode = v
        .and_then(|v| v.attr("view"))
        .and_then(|m| MODES.iter().position(|x| *x == m))
        .map_or(1, |i| i as u8 + 1);
    out.mode = Some(mode);
    let scale = number::<u32>(v, "zoomScale").unwrap_or(100);
    if mode == 1 {
        out.zoom = scale;
    } else {
        out.zoom = number::<u32>(v, "zoomScaleNormal")
            .filter(|z| *z > 0)
            .unwrap_or(100);
        out.mode_zoom = Some(scale);
    }
    out.right_to_left = Some(flag(v, "rightToLeft", false));
    out.outline_symbols = Some(flag(v, "showOutlineSymbols", true));
    out.gridlines = flag(v, "showGridLines", true);
    out.headings = flag(v, "showRowColHeaders", true);
    out.formulas = flag(v, "showFormulas", false);
    out.zeros = flag(v, "showZeros", true);
    if !flag(v, "defaultGridColor", true) {
        out.grid_color = number(v, "colorId");
    }
    let (r, c) = cell_at(v.and_then(|v| v.attr("topLeftCell"))).unwrap_or((0, 0));
    (out.scroll_row, out.scroll_col) = (r + 1, c + 1);
    let Some(p) = v.and_then(|v| v.child("pane")) else {
        return out;
    };
    let p = Some(p);
    let x = number::<f64>(p, "xSplit").unwrap_or(0.0);
    let y = number::<f64>(p, "ySplit").unwrap_or(0.0);
    out.frozen = matches!(
        p.and_then(|p| p.attr("state")),
        Some("frozen" | "frozenSplit")
    );
    if out.frozen {
        (out.split_row, out.split_col) = (y, x);

        let lower = cell_at(p.and_then(|p| p.attr("topLeftCell")));
        if y > 0.0 {
            out.pane_row = out.scroll_row;
            out.scroll_row = lower.map_or(out.scroll_row, |(r, _)| r + 1);
        }
        if x > 0.0 {
            out.pane_col = out.scroll_col;
            out.scroll_col = lower.map_or(out.scroll_col, |(_, c)| c + 1);
        }
    } else {
        (out.split_row, out.split_col) = (split_px(y.max(0.0)), split_px(x.max(0.0)));
    }
    out
}

fn read_page(sheet: &Node) -> PageUi {
    let mut out = PageUi::default();
    let margins = sheet.child("pageMargins");
    for (i, k) in MARGINS.iter().enumerate() {
        if let Some(v) = number::<f64>(margins, k) {
            out.margins[i] = v * 72.0;
        }
    }
    let setup = sheet.child("pageSetup");
    let word = |k: &str| setup.and_then(|s| s.attr(k));
    let one_of = |k: &str, words: &[&str]| word(k).and_then(|v| words.iter().position(|x| *x == v));
    out.orientation = if word("orientation") == Some("landscape") {
        2
    } else {
        1
    };
    out.paper_size = number(setup, "paperSize").unwrap_or(1);
    let fit = flag(
        sheet.child("sheetPr").and_then(|p| p.child("pageSetUpPr")),
        "fitToPage",
        false,
    );
    out.zoom = (!fit).then(|| number(setup, "scale").unwrap_or(100));
    let pages = |k: &str| Some(number::<u32>(setup, k).unwrap_or(1)).filter(|n| *n > 0);
    (out.fit_wide, out.fit_tall) = (pages("fitToWidth"), pages("fitToHeight"));
    if flag(setup, "useFirstPageNumber", false) {
        out.first_page = number::<i64>(setup, "firstPageNumber");
    }
    out.over_then_down = word("pageOrder") == Some("overThenDown");
    out.black_and_white = flag(setup, "blackAndWhite", false);
    out.draft = flag(setup, "draft", false);
    out.comments = one_of("cellComments", &COMMENTS).unwrap_or(0) as u32;
    out.errors = one_of("errors", &ERRORS).unwrap_or(0) as u32;
    let print = sheet.child("printOptions");
    out.center_horizontally = flag(print, "horizontalCentered", false);
    out.center_vertically = flag(print, "verticalCentered", false);
    out.gridlines = flag(print, "gridLines", false);
    out.headings = flag(print, "headings", false);
    let hf = sheet.child("headerFooter");
    let text = |k: &str| {
        hf.and_then(|h| h.child(k))
            .map_or_else(String::new, |t| t.text.clone())
    };
    (out.header, out.footer) = (text("oddHeader"), text("oddFooter"));
    out.odd_even = flag(hf, "differentOddEven", false);
    out.different_first = flag(hf, "differentFirst", false);
    out.scale_with_doc = flag(hf, "scaleWithDoc", true);
    out.align_margins = flag(hf, "alignWithMargins", true);
    out
}

fn read_tab(sheet: &Node) -> Option<TabColor> {
    let t = sheet.child("sheetPr")?.child("tabColor");
    t?;
    Some(TabColor {
        rgb: t
            .and_then(|t| t.attr("rgb"))
            .and_then(|v| u32::from_str_radix(v, 16).ok())
            .map(|v| v & 0x00FF_FFFF),
        theme: number(t, "theme"),
        indexed: number(t, "indexed"),
        tint: number(t, "tint").unwrap_or(0.0),
    })
}

fn read_node(sheet: &Node) -> SheetUi {
    SheetUi {
        tab: read_tab(sheet),
        view: read_view(sheet),
        page: read_page(sheet),
    }
}

pub fn read_sheet_ui(pkg: &Package, sheet: SheetId) -> Option<SheetUi> {
    let part = &pkg.source_sheet(sheet)?.part;
    xml::parse(pkg.part(part)?).ok().map(|n| read_node(&n))
}

pub(crate) fn read_split(split: &Split) -> SheetUi {
    xml::parse(&split.write()).map_or_else(|_| SheetUi::default(), |n| read_node(&n))
}

fn open_element(raw: Option<&str>, name: &str) -> Result<(Tag, Vec<rawxml::Kid>), String> {
    match raw {
        Some(raw) => {
            let s = rawxml::split(raw.as_bytes())?;
            Ok((s.root, s.kids))
        }
        None => Ok((Tag::new(name), Vec::new())),
    }
}

fn close_element(tag: &Tag, kids: &[rawxml::Kid]) -> String {
    if kids.is_empty() {
        tag.empty()
    } else {
        let body: String = kids.iter().map(|k| k.raw.as_str()).collect();
        format!("{}{body}</{}>", tag.open(), tag.name)
    }
}

fn set_if<T: PartialEq>(tag: &mut Tag, key: &str, now: &T, was: &T, value: Option<String>) {
    if now != was {
        tag.set(key, value.as_deref());
    }
}

fn one(v: bool) -> Option<String> {
    v.then(|| "1".to_string())
}

fn zero(v: bool) -> Option<String> {
    (!v).then(|| "0".to_string())
}

const SHEET_PR_ORDER: &[&str] = &["tabColor", "outlinePr", "pageSetUpPr"];

pub(crate) fn put_sheet_pr(
    split: &mut Split,
    order: &[&str],
    name: &str,
    child: Option<String>,
) -> Result<(), String> {
    let raw = split.kid("sheetPr").map(|k| k.raw.clone());
    let (tag, kids) = open_element(raw.as_deref(), "sheetPr")?;
    let mut inner = Split { root: tag, kids };
    inner.put(name, child, SHEET_PR_ORDER);
    if raw.is_some() || !inner.kids.is_empty() {
        split.put(
            "sheetPr",
            Some(close_element(&inner.root, &inner.kids)),
            order,
        );
    }
    Ok(())
}

pub(crate) fn sheet_pr_child(split: &Split, name: &str) -> Result<Option<Tag>, String> {
    let raw = split.kid("sheetPr").map(|k| k.raw.as_str());
    let (_, kids) = open_element(raw, "sheetPr")?;
    kids.iter()
        .find(|k| k.name == name)
        .map(|k| Tag::parse(&k.raw[..rawxml::head_len(&k.raw).unwrap_or(k.raw.len())]))
        .transpose()
}

pub(crate) fn write_tab(
    split: &mut Split,
    order: &[&str],
    now: &SheetUi,
    was: &SheetUi,
) -> Result<(), String> {
    if now.tab == was.tab {
        return Ok(());
    }
    let child = now.tab.as_ref().map(|c| {
        let mut t = Tag::new("tabColor");
        t.set("rgb", c.rgb.map(|v| format!("FF{v:06X}")).as_deref());
        t.set("indexed", c.indexed.map(|v| v.to_string()).as_deref());
        t.set("theme", c.theme.map(|v| v.to_string()).as_deref());
        t.set(
            "tint",
            (c.tint != 0.0).then(|| c.tint.to_string()).as_deref(),
        );
        t.empty()
    });
    put_sheet_pr(split, order, "tabColor", child)
}

pub(crate) fn write_page(
    split: &mut Split,
    order: &[&str],
    now: &SheetUi,
    was: &SheetUi,
) -> Result<(), String> {
    let (n, w) = (&now.page, &was.page);
    if n == w {
        return Ok(());
    }
    let raw = |split: &Split, name: &str| split.kid(name).map(|k| k.raw.clone());

    let (mut t, kids) = open_element(raw(split, "printOptions").as_deref(), "printOptions")?;
    for (key, now, was) in [
        (
            "horizontalCentered",
            n.center_horizontally,
            w.center_horizontally,
        ),
        ("verticalCentered", n.center_vertically, w.center_vertically),
        ("headings", n.headings, w.headings),
        ("gridLines", n.gridlines, w.gridlines),
    ] {
        set_if(&mut t, key, &now, &was, one(now));
    }
    let print = (!t.attrs.is_empty()).then(|| close_element(&t, &kids));
    split.put("printOptions", print, order);

    if n.margins != w.margins || split.kid("pageMargins").is_none() {
        let (mut t, kids) = open_element(raw(split, "pageMargins").as_deref(), "pageMargins")?;
        for (i, key) in MARGINS.iter().enumerate() {
            if n.margins[i] != w.margins[i] || t.get(key).is_none() {
                t.set(key, Some(&(n.margins[i] / 72.0).to_string()));
            }
        }
        split.put("pageMargins", Some(close_element(&t, &kids)), order);
    }

    let (mut t, kids) = open_element(raw(split, "pageSetup").as_deref(), "pageSetup")?;
    let other = |v: u32, d: u32| (v != d).then(|| v.to_string());
    set_if(
        &mut t,
        "paperSize",
        &n.paper_size,
        &w.paper_size,
        other(n.paper_size, 1),
    );
    if let Some(z) = n.zoom {
        set_if(&mut t, "scale", &n.zoom, &w.zoom, other(z, 100));
    }
    set_if(
        &mut t,
        "firstPageNumber",
        &n.first_page,
        &w.first_page,
        n.first_page.map(|v| v.to_string()),
    );
    set_if(
        &mut t,
        "useFirstPageNumber",
        &n.first_page,
        &w.first_page,
        one(n.first_page.is_some()),
    );
    for (key, now, was) in [
        ("fitToWidth", n.fit_wide, w.fit_wide),
        ("fitToHeight", n.fit_tall, w.fit_tall),
    ] {
        set_if(&mut t, key, &now, &was, other(now.unwrap_or(0), 1));
    }
    set_if(
        &mut t,
        "pageOrder",
        &n.over_then_down,
        &w.over_then_down,
        n.over_then_down.then(|| "overThenDown".to_string()),
    );
    set_if(
        &mut t,
        "orientation",
        &n.orientation,
        &w.orientation,
        Some(if n.orientation == 2 {
            "landscape".to_string()
        } else {
            "portrait".to_string()
        }),
    );
    for (key, now, was) in [
        ("blackAndWhite", n.black_and_white, w.black_and_white),
        ("draft", n.draft, w.draft),
    ] {
        set_if(&mut t, key, &now, &was, one(now));
    }
    let named = |v: u32, words: &[&str]| {
        words
            .get(v as usize)
            .filter(|_| v > 0)
            .map(|s| s.to_string())
    };
    set_if(
        &mut t,
        "cellComments",
        &n.comments,
        &w.comments,
        named(n.comments, &COMMENTS),
    );
    set_if(
        &mut t,
        "errors",
        &n.errors,
        &w.errors,
        named(n.errors, &ERRORS),
    );
    let setup = (!t.attrs.is_empty()).then(|| close_element(&t, &kids));
    split.put("pageSetup", setup, order);

    if n.zoom.is_none() != w.zoom.is_none() {
        let mut t =
            sheet_pr_child(split, "pageSetUpPr")?.unwrap_or_else(|| Tag::new("pageSetUpPr"));
        t.set("fitToPage", one(n.zoom.is_none()).as_deref());
        let child = (!t.attrs.is_empty()).then(|| t.empty());
        put_sheet_pr(split, order, "pageSetUpPr", child)?;
    }

    let (mut t, kids) = open_element(raw(split, "headerFooter").as_deref(), "headerFooter")?;
    for (key, now, was) in [
        ("differentOddEven", n.odd_even, w.odd_even),
        ("differentFirst", n.different_first, w.different_first),
    ] {
        set_if(&mut t, key, &now, &was, one(now));
    }
    for (key, now, was) in [
        ("scaleWithDoc", n.scale_with_doc, w.scale_with_doc),
        ("alignWithMargins", n.align_margins, w.align_margins),
    ] {
        set_if(&mut t, key, &now, &was, zero(now));
    }
    let mut inner = Split { root: t, kids };
    for (name, now, was) in [
        ("oddHeader", &n.header, &w.header),
        ("oddFooter", &n.footer, &w.footer),
    ] {
        if now != was {
            let child =
                (!now.is_empty()).then(|| format!("<{name}>{}</{name}>", rawxml::escape_text(now)));
            inner.put(
                name,
                child,
                &[
                    "oddHeader",
                    "oddFooter",
                    "evenHeader",
                    "evenFooter",
                    "firstHeader",
                    "firstFooter",
                ],
            );
        }
    }
    let footer = (!inner.root.attrs.is_empty() || !inner.kids.is_empty())
        .then(|| close_element(&inner.root, &inner.kids));
    split.put("headerFooter", footer, order);
    Ok(())
}

pub(crate) struct SelectionText {
    pub active: String,

    pub active_area: usize,
    pub sqref: String,
}

fn selection_xml(pane: Option<&str>, sel: &SelectionText) -> String {
    let mut t = Tag::new("selection");
    t.set("pane", pane);
    t.set("activeCell", Some(&sel.active));
    if sel.active_area > 0 {
        t.set("activeCellId", Some(&sel.active_area.to_string()));
    }
    t.set("sqref", Some(&sel.sqref));
    t.empty()
}

const VIEW_ATTRS: &[&str] = &[
    "windowProtection",
    "showFormulas",
    "showGridLines",
    "showRowColHeaders",
    "showZeros",
    "rightToLeft",
    "tabSelected",
    "showRuler",
    "showOutlineSymbols",
    "defaultGridColor",
    "view",
    "topLeftCell",
    "colorId",
    "zoomScale",
    "zoomScaleNormal",
    "zoomScaleSheetLayoutView",
    "zoomScalePageLayoutView",
    "workbookViewId",
];

pub(crate) struct ViewWrite<'a> {
    pub ui: Option<(&'a SheetUi, &'a SheetUi)>,

    pub selected: Option<bool>,

    pub selection: Option<SelectionText>,
}

fn a1(row: u32, col: u32) -> String {
    format!("{}{row}", xlcalc::col_to_letters(col - 1))
}

pub(crate) fn write_view(
    split: &mut Split,
    order: &[&str],
    w: ViewWrite<'_>,
) -> Result<(), String> {
    let changed = w.ui.and_then(|(now, was)| {
        let (mut n, o) = (now.view.clone(), &was.view);
        n.mode = n.mode.or(o.mode);
        n.mode_zoom = n.mode_zoom.filter(|_| n.mode != Some(1)).or(o.mode_zoom);
        n.right_to_left = n.right_to_left.or(o.right_to_left);
        n.outline_symbols = n.outline_symbols.or(o.outline_symbols);
        (n != *o).then_some((n, o))
    });
    let had = split.kid("sheetViews").map(|k| k.raw.clone());
    if changed.is_none() && w.selection.is_none() && w.selected != Some(true) && had.is_none() {
        return Ok(());
    }
    let (views_tag, mut views) = open_element(had.as_deref(), "sheetViews")?;
    if !views.iter().any(|k| k.name == "sheetView") {
        views.insert(
            0,
            rawxml::Kid {
                name: "sheetView".into(),
                raw: "<sheetView workbookViewId=\"0\"/>".into(),
            },
        );
    }
    let Some(at) = views.iter().position(|k| k.name == "sheetView") else {
        return Ok(());
    };
    let (mut tag, kids) = open_element(Some(&views[at].raw), "sheetView")?;
    let mut inner = Split {
        root: Tag::new("sheetView"),
        kids,
    };
    if let Some(on) = w.selected {
        tag.set("tabSelected", one(on).as_deref());
    }
    if let Some((n, o)) = &changed {
        for (key, now, was) in [
            ("showGridLines", n.gridlines, o.gridlines),
            ("showRowColHeaders", n.headings, o.headings),
            ("showZeros", n.zeros, o.zeros),
        ] {
            set_if(&mut tag, key, &now, &was, zero(now));
        }
        set_if(
            &mut tag,
            "showFormulas",
            &n.formulas,
            &o.formulas,
            one(n.formulas),
        );
        if let Some(v) = n.right_to_left {
            set_if(&mut tag, "rightToLeft", &Some(v), &o.right_to_left, one(v));
        }
        if let Some(v) = n.outline_symbols {
            set_if(
                &mut tag,
                "showOutlineSymbols",
                &Some(v),
                &o.outline_symbols,
                zero(v),
            );
        }
        if n.grid_color != o.grid_color {
            tag.set("defaultGridColor", zero(n.grid_color.is_none()).as_deref());
            tag.set("colorId", n.grid_color.map(|c| c.to_string()).as_deref());
        }
        let mode = n.mode.or(o.mode).unwrap_or(1);
        let mode_zoom = n.mode_zoom.or(o.mode_zoom);
        if (Some(mode), n.zoom, mode_zoom) != (o.mode, o.zoom, o.mode_zoom) {
            let name = MODES.get(usize::from(mode.saturating_sub(1)));
            tag.set("view", name.filter(|_| mode > 1).copied());
            let shown = if mode == 1 {
                n.zoom
            } else {
                mode_zoom.unwrap_or(100)
            };
            let normal = (mode > 1 || n.zoom != 100).then(|| n.zoom.to_string());
            tag.set(
                "zoomScale",
                (shown != 100).then(|| shown.to_string()).as_deref(),
            );
            tag.set("zoomScaleNormal", normal.as_deref());
        }
        let layout = |v: &ViewUi| (v.split_row, v.split_col, v.frozen, v.pane_row, v.pane_col);
        let scroll = |v: &ViewUi| (v.scroll_row, v.scroll_col);
        let moved = scroll(n) != scroll(o);
        if layout(n) != layout(o) || (moved && inner.kid("pane").is_some()) {
            let top = |first: u32, pane: u32, split: f64| {
                if n.frozen && split > 0.0 {
                    pane.max(1)
                } else {
                    first.max(1)
                }
            };
            let corner = (
                top(n.scroll_row, n.pane_row, n.split_row),
                top(n.scroll_col, n.pane_col, n.split_col),
            );
            tag.set(
                "topLeftCell",
                (corner != (1, 1))
                    .then(|| a1(corner.0, corner.1))
                    .as_deref(),
            );
            let kept: Vec<String> = inner
                .kids
                .iter()
                .filter(|k| k.name == "selection")
                .map(|k| k.raw.clone())
                .collect();
            inner
                .kids
                .retain(|k| k.name != "pane" && k.name != "selection");
            let (rows, cols) = (n.split_row > 0.0, n.split_col > 0.0);
            if rows || cols {
                let active = match (rows, cols) {
                    (true, true) => "bottomRight",
                    (true, false) => "bottomLeft",
                    _ => "topRight",
                };
                let mut p = Tag::new("pane");
                let (x, y) = if n.frozen {
                    (n.split_col, n.split_row)
                } else {
                    (split_twips(n.split_col), split_twips(n.split_row))
                };
                p.set("xSplit", cols.then(|| x.to_string()).as_deref());
                p.set("ySplit", rows.then(|| y.to_string()).as_deref());

                let lower = |split: f64, frozen_at: u32, corner: u32, next: u32| {
                    if n.frozen {
                        if split > 0.0 {
                            frozen_at.max(1)
                        } else {
                            1
                        }
                    } else if next > 0 {
                        next
                    } else {
                        corner
                    }
                };
                let first = (
                    lower(n.split_row, n.scroll_row, corner.0, n.split_next.0),
                    lower(n.split_col, n.scroll_col, corner.1, n.split_next.1),
                );
                p.set("topLeftCell", Some(&a1(first.0, first.1)));
                p.set("activePane", n.frozen.then_some(active));
                p.set("state", n.frozen.then_some("frozen"));
                inner.kids.push(rawxml::Kid {
                    name: "pane".into(),
                    raw: p.empty(),
                });

                let here = a1(first.0, first.1);
                let start = SelectionText {
                    active: here.clone(),
                    active_area: 0,
                    sqref: here,
                };
                if n.frozen && rows && cols {
                    for pane in ["topRight", "bottomLeft"] {
                        inner.kids.push(rawxml::Kid {
                            name: "selection".into(),
                            raw: selection_xml(Some(pane), &start),
                        });
                    }
                }
                inner.kids.push(rawxml::Kid {
                    name: "selection".into(),
                    raw: selection_xml(Some(active), w.selection.as_ref().unwrap_or(&start)),
                });
            } else if let Some(last) = kept.last() {
                let mut t = Tag::parse(last)?;
                t.set("pane", None);
                inner.kids.push(rawxml::Kid {
                    name: "selection".into(),
                    raw: t.empty(),
                });
            }
        } else if moved {
            tag.set(
                "topLeftCell",
                (scroll(n) != (1, 1))
                    .then(|| a1(n.scroll_row, n.scroll_col))
                    .as_deref(),
            );
        }
    }
    if let Some(sel) = &w.selection {
        let pane = inner
            .kid("pane")
            .map(|k| Tag::parse(&k.raw))
            .transpose()?
            .and_then(|p| p.value("activePane"))
            .filter(|p| p != "topLeft");
        let xml = selection_xml(pane.as_deref(), sel);
        let found = inner.kids.iter().position(|k| {
            k.name == "selection"
                && Tag::parse(&k.raw)
                    .is_ok_and(|t| t.value("pane").filter(|p| p != "topLeft") == pane)
        });
        let kid = rawxml::Kid {
            name: "selection".into(),
            raw: xml,
        };
        match found {
            Some(i) => inner.kids[i] = kid,
            None => {
                let end = inner
                    .kids
                    .iter()
                    .rposition(|k| k.name == "pane" || k.name == "selection")
                    .map_or(0, |i| i + 1);
                inner.kids.insert(end, kid);
            }
        }
    }
    tag.attrs.sort_by_key(|(k, _)| {
        VIEW_ATTRS
            .iter()
            .position(|a| a == k)
            .unwrap_or(VIEW_ATTRS.len())
    });
    views[at].raw = close_element(&tag, &inner.kids);
    split.put("sheetViews", Some(close_element(&views_tag, &views)), order);
    Ok(())
}

pub const CORE_PROPERTIES: [&str; 5] = [
    "dc:title",
    "dc:subject",
    "dc:creator",
    "cp:keywords",
    "dc:description",
];

pub fn core_property(core: &str, tag: &str) -> String {
    let Some(at) = core.find(&format!("<{tag}")) else {
        return String::new();
    };
    let Some(open) = core[at..].find('>').map(|i| at + i + 1) else {
        return String::new();
    };
    if core[..open].ends_with("/>") {
        return String::new();
    }
    core[open..]
        .find(&format!("</{tag}>"))
        .map_or_else(String::new, |end| rawxml::unescape(&core[open..open + end]))
}

pub(crate) fn core_xml(core: &[u8], values: &[String; 5]) -> Result<Option<Vec<u8>>, String> {
    let text = std::str::from_utf8(core).map_err(|e| e.to_string())?;
    let changed: Vec<(&str, &String)> = CORE_PROPERTIES
        .iter()
        .zip(values)
        .filter(|(tag, v)| core_property(text, tag) != **v)
        .map(|(tag, v)| (*tag, v))
        .collect();
    if changed.is_empty() {
        return Ok(None);
    }
    let mut split = rawxml::split(core)?;
    for (tag, value) in changed {
        let local = tag.rsplit(':').next().unwrap_or(tag);
        let child =
            (!value.is_empty()).then(|| format!("<{tag}>{}</{tag}>", rawxml::escape_text(value)));
        split.put(local, child, &[]);
    }
    Ok(Some(split.write()))
}

#[cfg(test)]
mod split_tests {
    use super::*;

    #[test]
    fn twips_and_pixels_of_a_split() {
        assert_eq!(split_twips(328.0), 3280.0);
        assert_eq!(split_px(3280.0), 328.0);

        for (read, written) in [
            (401.0, 400.0),
            (404.0, 400.0),
            (405.0, 410.0),
            (415.0, 420.0),
            (295.0, 290.0),
            (296.0, 300.0),
            (5565.0, 5560.0),
            (1745.0, 1750.0),
            (3280.9, 3280.0),
            (1000.5, 1000.0),
        ] {
            assert_eq!(split_twips(split_px(read)), written, "{read}");
        }
        assert_eq!(split_twips(0.0), 1.0);
    }
}
