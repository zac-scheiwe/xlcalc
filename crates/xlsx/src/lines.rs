use std::collections::{BTreeMap, BTreeSet, HashMap};

use xlcalc::style::Style;
use xlcalc::{ColWidth, Rect, Selection, SheetId, Workbook};

use crate::rawxml::{self, Split, Tag};
use crate::styles::StyleWriter;
use crate::ui::{self, SelectionText};
use crate::xml::Node;

const MAX_COL: u32 = 16_383;

fn on(v: Option<&str>) -> bool {
    matches!(v, Some("1" | "true"))
}

pub(crate) struct LineState {
    col_levels: Vec<(u32, u32, u32)>,
    col_tags: Vec<(u32, u32)>,
    row_levels: Vec<(u32, u32)>,
    row_tags: Vec<u32>,

    outline: Option<(bool, bool, bool)>,

    breaks: (Vec<u32>, Vec<u32>),

    selection: Option<Selection>,
}

impl LineState {
    pub(crate) fn read(sheet: &Node) -> LineState {
        let mut out = LineState {
            breaks: (breaks(sheet, "rowBreaks"), breaks(sheet, "colBreaks")),
            selection: selection(sheet),
            col_levels: Vec::new(),
            col_tags: Vec::new(),
            row_levels: Vec::new(),
            row_tags: Vec::new(),
            outline: sheet
                .child("sheetPr")
                .and_then(|p| p.child("outlinePr"))
                .map(|o| {
                    let below = |k: &str| matches!(o.attr(k), Some("0" | "false"));
                    (
                        below("summaryBelow"),
                        below("summaryRight"),
                        on(o.attr("applyStyles")),
                    )
                }),
        };
        let level = |v: Option<&str>| v.and_then(|v| v.parse::<u32>().ok()).unwrap_or(0).min(7);
        if let Some(cols) = sheet.child("cols") {
            for c in cols.children("col") {
                let num = |k: &str| c.attr(k).and_then(|v| v.parse::<u32>().ok());
                let (Some(min), Some(max)) = (num("min"), num("max")) else {
                    continue;
                };
                if min < 1 || max < min || min > MAX_COL + 1 {
                    continue;
                }
                let (c0, c1) = (min - 1, (max - 1).min(MAX_COL));
                out.col_tags.push((c0, c1));
                let l = level(c.attr("outlineLevel"));
                if l > 0 {
                    out.col_levels.push((c0, c1, l));
                }
            }
        }
        let mut next = 0u32;
        if let Some(data) = sheet.child("sheetData") {
            for row in data.children("row") {
                next = match row.attr("r") {
                    Some(r) => match r.parse::<u32>() {
                        Ok(r) if r > 0 => r,
                        _ => continue,
                    },
                    None => next + 1,
                };
                out.row_tags.push(next - 1);
                let l = level(row.attr("outlineLevel"));
                if l > 0 {
                    out.row_levels.push((next - 1, l));
                }
            }
        }
        out
    }

    pub(crate) fn apply(&self, wb: &mut Workbook, sheet: SheetId) {
        if let Some((above, left, styles)) = self.outline {
            wb.set_outline_settings(sheet, above, left, styles);
        }
        for &(c0, c1, l) in &self.col_levels {
            wb.set_outline_levels(sheet, false, c0, c1 - c0 + 1, l + 1);
        }
        for &(r, l) in &self.row_levels {
            wb.set_outline_levels(sheet, true, r, 1, l + 1);
        }
        for &(c0, c1) in &self.col_tags {
            for c in c0..=c1 {
                wb.set_line_origin(sheet, false, c, c);
            }
        }
        for &r in &self.row_tags {
            wb.set_line_origin(sheet, true, r, r);
        }
        for (rows, list) in [(true, &self.breaks.0), (false, &self.breaks.1)] {
            for &at in list {
                wb.add_page_break(sheet, rows, at);
            }
        }
        if let Some(s) = &self.selection {
            let _ = wb.set_selection(sheet, s.clone());
        }
    }
}

struct SourceCol {
    c0: u32,
    c1: u32,
    tag: Tag,
}

fn source_cols(raw: &str) -> Result<Vec<SourceCol>, String> {
    let mut out = Vec::new();
    for kid in rawxml::split(raw.as_bytes())?.kids {
        if kid.name != "col" {
            continue;
        }
        let tag = Tag::parse(&kid.raw)?;
        let num = |k: &str| tag.get(k).and_then(|v| v.parse::<u32>().ok());
        let (Some(min), Some(max)) = (num("min"), num("max")) else {
            continue;
        };
        if min >= 1 && max >= min {
            out.push(SourceCol {
                c0: min - 1,
                c1: max - 1,
                tag,
            });
        }
    }
    out.sort_by_key(|c| c.c0);
    Ok(out)
}

#[derive(PartialEq)]
struct Col {
    width: String,
    style: u32,
    hidden: bool,
    best_fit: bool,
    custom: bool,
    phonetic: bool,
    level: u32,
    collapsed: bool,

    own_width: bool,
}

pub(crate) fn cols_xml(
    wb: &Workbook,
    sheet: SheetId,
    src: Option<&str>,
    styles: &mut StyleWriter,
) -> Result<Option<String>, String> {
    let source = match src {
        Some(raw) => source_cols(raw)?,
        None => Vec::new(),
    };
    let from_source = |orig: u32| {
        let i = source.partition_point(|c| c.c1 < orig);
        source.get(i).filter(|c| c.c0 <= orig)
    };
    let std_px = wb.sheet_std_px(sheet);
    let state: BTreeMap<u32, (ColWidth, bool)> = wb
        .column_state(sheet)
        .into_iter()
        .map(|(c, w, hidden)| (c, (w, hidden)))
        .collect();
    let origins: HashMap<u32, u32> = wb.line_origins(sheet, false).into_iter().collect();
    let levels: HashMap<u32, u32> = wb.outlined_lines(sheet, false).into_iter().collect();
    let collapsed: BTreeSet<u32> = wb.collapsed_lines(sheet, false).into_iter().collect();
    let formats: BTreeMap<u32, Style> = wb.column_formats(sheet).into_iter().collect();
    let mut cols: BTreeSet<u32> = state.keys().copied().collect();
    cols.extend(origins.keys());
    cols.extend(levels.keys());
    cols.extend(&collapsed);
    cols.extend(formats.keys());

    let mut runs: Vec<(u32, u32, Col)> = Vec::new();
    for c in cols {
        let standard = ColWidth {
            px: std_px,
            ..ColWidth::default()
        };
        let (w, hidden) = state.get(&c).copied().unwrap_or((standard, false));

        let own_width = w.px != 0 && state.contains_key(&c) && (w.user || w.px != std_px);
        let px = if w.px == 0 { std_px } else { w.px };
        let kept = origins
            .get(&c)
            .and_then(|&o| from_source(o))
            .map(|s| &s.tag);
        let attr = |k: &str| kept.and_then(|t| t.get(k));
        let src_width = attr("width").filter(|w| {
            w.parse::<f64>()
                .is_ok_and(|w| xlcalc::file_width_to_px(w) == px)
        });

        let style = match formats.get(&c) {
            Some(f) => styles.xf(f, false, attr("style").and_then(|s| s.parse::<u32>().ok())),
            None => 0,
        };

        let width = match src_width {
            Some(w) if own_width || !hidden => w.to_string(),
            _ if hidden && !own_width => "0".to_string(),
            _ => xlcalc::px_to_file_width(px).to_string(),
        };
        let col = Col {
            width,
            style,
            hidden,
            best_fit: w.best,
            custom: w.user,
            phonetic: on(attr("phonetic")),
            level: levels.get(&c).map_or(0, |l| l - 1),
            collapsed: collapsed.contains(&c),
            own_width,
        };

        let plain = col.style == 0
            && !col.hidden
            && !col.custom
            && px == std_px
            && !col.phonetic
            && col.level == 0
            && !col.collapsed;
        if plain {
            continue;
        }
        match runs.last_mut() {
            Some((_, last, run)) if *last + 1 == c && *run == col => *last = c,
            _ => runs.push((c, c, col)),
        }
    }
    if runs.is_empty() {
        return Ok(None);
    }
    let mut out = String::from("<cols>");
    for (c0, c1, mut col) in runs {
        if c1 == MAX_COL && c1 > c0 && col.hidden && !col.own_width {
            col.width = xlcalc::px_to_file_width(std_px).to_string();
            col.custom = false;
        }
        let mut t = Tag::new("col");
        t.set("min", Some(&(c0 + 1).to_string()))
            .set("max", Some(&(c1 + 1).to_string()))
            .set("width", Some(&col.width));
        if col.style != 0 {
            t.set("style", Some(&col.style.to_string()));
        }
        let flag = |t: &mut Tag, k: &str, v: bool| {
            if v {
                t.set(k, Some("1"));
            }
        };
        flag(&mut t, "hidden", col.hidden);
        flag(&mut t, "bestFit", col.best_fit);
        flag(&mut t, "customWidth", col.custom);
        flag(&mut t, "phonetic", col.phonetic);
        if col.level > 0 {
            t.set("outlineLevel", Some(&col.level.to_string()));
        }
        flag(&mut t, "collapsed", col.collapsed);
        out.push_str(&t.empty());
    }
    out.push_str("</cols>");
    Ok(Some(out))
}

pub(crate) fn outline_depths(
    wb: &Workbook,
    sheet: SheetId,
    raw: Option<&str>,
) -> Result<Option<String>, String> {
    let depth = |rows: bool| wb.outlined_lines(sheet, rows).iter().map(|l| l.1 - 1).max();
    let (rows, cols) = (depth(true), depth(false));
    let mut tag = match raw {
        Some(raw) => Tag::parse(raw)?,
        None if rows.is_none() && cols.is_none() => return Ok(None),
        None => {
            let mut t = Tag::new("sheetFormatPr");
            t.empty = true;
            t.set("defaultRowHeight", Some("15"));
            t
        }
    };
    let before = tag.attrs.clone();
    tag.set("outlineLevelRow", rows.map(|l| l.to_string()).as_deref());
    tag.set("outlineLevelCol", cols.map(|l| l.to_string()).as_deref());
    Ok((raw.is_none() || tag.attrs != before).then(|| tag.empty()))
}

fn breaks(sheet: &Node, name: &str) -> Vec<u32> {
    let Some(list) = sheet.child(name) else {
        return Vec::new();
    };
    list.children("brk")
        .filter(|b| on(b.attr("man")))
        .filter_map(|b| b.attr("id").and_then(|v| v.parse::<u32>().ok()))
        .filter(|&at| at > 0)
        .collect()
}

fn area(text: &str) -> Option<Rect> {
    let (a, b) = text.split_once(':').unwrap_or((text, text));
    let (a, b) = (crate::parse_cell(a)?, crate::parse_cell(b)?);
    Some(Rect::new(a.0, a.1, b.0, b.1))
}

fn selection(sheet: &Node) -> Option<Selection> {
    let view = sheet.child("sheetViews")?.child("sheetView")?;
    let named = |p: Option<&str>| p.unwrap_or("topLeft").to_string();
    let active = named(view.child("pane").and_then(|p| p.attr("activePane")));
    let sel = view
        .children("selection")
        .find(|s| named(s.attr("pane")) == active)?;
    let cell = sel.attr("activeCell").and_then(crate::parse_cell);
    let areas: Vec<Rect> = match sel.attr("sqref") {
        Some(list) => list.split_whitespace().filter_map(area).collect(),
        None => cell
            .map(|(r, c)| Rect::new(r, c, r, c))
            .into_iter()
            .collect(),
    };
    let first = areas.first()?;
    let active = cell.unwrap_or((first.r0, first.c0));
    Some(Selection { areas, active })
}

fn a1(row: u32, col: u32) -> String {
    format!("{}{}", xlcalc::col_to_letters(col), row + 1)
}

pub(crate) fn changed_selection(
    wb: &Workbook,
    sheet: SheetId,
    part: &Split,
) -> Option<SelectionText> {
    let now = wb.selection_of(sheet);
    let was = crate::xml::parse(&part.write())
        .ok()
        .and_then(|n| selection(&n))
        .unwrap_or_else(|| Selection::cell(0, 0));
    if now == was {
        return None;
    }
    let text = |a: &Rect| {
        if (a.r0, a.c0) == (a.r1, a.c1) {
            a1(a.r0, a.c0)
        } else {
            format!("{}:{}", a1(a.r0, a.c0), a1(a.r1, a.c1))
        }
    };
    let (row, col) = now.active;
    Some(SelectionText {
        active: a1(row, col),
        active_area: now
            .areas
            .iter()
            .position(|a| a.contains(row, col))
            .unwrap_or(0),
        sqref: now.areas.iter().map(text).collect::<Vec<_>>().join(" "),
    })
}

pub(crate) fn write_breaks(
    wb: &Workbook,
    sheet: SheetId,
    split: &mut Split,
    order: &[&str],
) -> Result<(), String> {
    for (name, rows, last) in [
        ("rowBreaks", true, MAX_COL),
        ("colBreaks", false, 1_048_575),
    ] {
        let raw = split.kid(name).map(|k| k.raw.clone());
        let mut was = Vec::new();
        if let Some(raw) = &raw {
            let node = crate::xml::parse(raw.as_bytes())?;
            was = node
                .children("brk")
                .filter(|b| on(b.attr("man")))
                .filter_map(|b| b.attr("id").and_then(|v| v.parse::<u32>().ok()))
                .filter(|&at| at > 0)
                .collect();
        }
        was.sort_unstable();

        let now = wb.all_page_breaks(sheet, rows);
        if now == was {
            continue;
        }
        let list: String = now
            .iter()
            .map(|at| format!("<brk id=\"{at}\" max=\"{last}\" man=\"1\"/>"))
            .collect();
        let n = now.len();
        let xml = (n > 0)
            .then(|| format!("<{name} count=\"{n}\" manualBreakCount=\"{n}\">{list}</{name}>"));
        split.put(name, xml, order);
    }
    Ok(())
}

pub(crate) fn write_outline_pr(
    wb: &Workbook,
    sheet: SheetId,
    split: &mut Split,
    order: &[&str],
) -> Result<(), String> {
    let (above, left, styles) = wb.outline_settings(sheet);
    let had = ui::sheet_pr_child(split, "outlinePr")?;
    let mut tag = had.clone().unwrap_or_else(|| Tag::new("outlinePr"));
    let off = |t: &Tag, k: &str| matches!(t.get(k), Some("0" | "false"));
    let was = (
        off(&tag, "summaryBelow"),
        off(&tag, "summaryRight"),
        on(tag.get("applyStyles")),
    );
    if was == (above, left, styles) {
        return Ok(());
    }
    tag.set("applyStyles", styles.then_some("1"));
    tag.set("summaryBelow", above.then_some("0"));
    tag.set("summaryRight", left.then_some("0"));
    let child = (!tag.attrs.is_empty()).then(|| tag.empty());
    ui::put_sheet_pr(split, order, "outlinePr", child)
}
