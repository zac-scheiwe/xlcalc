use xlcalc::{CellAddr, ColWidth, Rect, SheetId, Value, Workbook, XlError};
use xlcalc_variant::Variant;

use crate::addr::{max_cols, max_rows};
use crate::app::{Application, BookId};
use crate::conv::Args;
use crate::error::OmError;

#[derive(Clone, Debug, Default)]
pub(crate) struct ConsolidateState {
    pub sources: Vec<(SheetId, Rect)>,
    pub top: bool,
    pub left: bool,
}

#[derive(Clone, Copy, PartialEq)]
enum Agg {
    Sum,
    Count,
    Average,
    Max,
    Min,
    Product,
    StDev,
    StDevP,
    Var,
    VarP,
}

fn agg_of(code: i32) -> Option<Agg> {
    Some(match code {
        -4157 => Agg::Sum,
        -4112 | -4113 => Agg::Count,
        -4106 => Agg::Average,
        -4136 => Agg::Max,
        -4139 => Agg::Min,
        -4149 => Agg::Product,
        -4155 => Agg::StDev,
        -4156 => Agg::StDevP,
        -4164 => Agg::Var,
        -4165 => Agg::VarP,
        _ => return None,
    })
}

fn aggregate(f: Agg, xs: &[f64]) -> Value {
    if xs.is_empty() {
        return Value::Blank;
    }
    let n = xs.len() as f64;
    let mean = xs.iter().sum::<f64>() / n;
    let ss = xs.iter().map(|x| (x - mean) * (x - mean)).sum::<f64>();
    match f {
        Agg::Sum => Value::Number(xs.iter().sum()),
        Agg::Count => Value::Number(n),
        Agg::Average => Value::Number(mean),
        Agg::Max => Value::Number(xs.iter().copied().fold(f64::MIN, f64::max)),
        Agg::Min => Value::Number(xs.iter().copied().fold(f64::MAX, f64::min)),
        Agg::Product => Value::Number(xs.iter().product()),
        Agg::StDev | Agg::Var if xs.len() < 2 => Value::Error(XlError::DIV0),
        Agg::StDev => Value::Number((ss / (n - 1.0)).sqrt()),
        Agg::Var => Value::Number(ss / (n - 1.0)),
        Agg::StDevP => Value::Number((ss / n).sqrt()),
        Agg::VarP => Value::Number(ss / n),
    }
}

fn result_of(f: Agg, counta: bool, values: &[Value]) -> Value {
    if f == Agg::Count && counta {
        return Value::Number(values.len() as f64);
    }
    if f != Agg::Count {
        if let Some(e) = values.iter().find(|v| matches!(v, Value::Error(_))) {
            return e.clone();
        }
    }
    let xs: Vec<f64> = values
        .iter()
        .filter_map(|v| match v {
            Value::Number(x) => Some(*x),
            _ => None,
        })
        .collect();
    match (f, xs.is_empty()) {
        (Agg::Count, true) => Value::Number(0.0),
        _ => aggregate(f, &xs),
    }
}

fn takes_part(v: &Value, counta: bool) -> bool {
    match v {
        Value::Number(_) | Value::Error(_) => true,
        Value::Blank => false,
        _ => counta,
    }
}

#[derive(Default)]
struct Keys {
    labels: Vec<(String, Value)>,
}

impl Keys {
    fn merge(&mut self, src: &[(String, Value)], append: bool) {
        let earlier: Vec<String> = self.labels.iter().map(|x| x.0.to_lowercase()).collect();
        for (i, l) in src.iter().enumerate() {
            if l.0.is_empty() || self.find(&l.0).is_some() {
                continue;
            }
            let at = if append {
                self.labels.len()
            } else {
                src[i + 1..]
                    .iter()
                    .filter(|n| earlier.contains(&n.0.to_lowercase()))
                    .find_map(|n| self.find(&n.0))
                    .unwrap_or(self.labels.len())
            };
            self.labels.insert(at, l.clone());
        }
    }

    fn find(&self, l: &str) -> Option<usize> {
        let l = l.to_lowercase();
        self.labels.iter().position(|x| x.0.to_lowercase() == l)
    }
}

fn label_of(v: &Value) -> (String, Value) {
    (label_key(v), v.clone())
}

fn label_key(v: &Value) -> String {
    match v {
        Value::Text(t) => t.to_string(),
        Value::Number(n) => format!("{n}"),
        Value::Bool(b) => if *b { "TRUE" } else { "FALSE" }.to_string(),
        _ => String::new(),
    }
}

struct Src {
    sheet: SheetId,
    rect: Rect,
    col_labels: Vec<(String, Value)>,
    row_labels: Vec<(String, Value)>,
}

struct Item {
    src: usize,
    at: CellAddr,
    value: Value,
}

struct Plan {
    cols: Keys,
    rows: Keys,

    nr: u32,
    nc: u32,

    cells: Vec<Vec<Vec<Item>>>,
    top: bool,
    left: bool,

    own_label: bool,
}

impl Plan {
    fn is_empty(&self) -> bool {
        self.cells.iter().all(|r| r.iter().all(Vec::is_empty))
    }

    fn labels(&self, rect: Rect, extra: u32) -> Vec<(u32, u32, Value)> {
        let (h, w) = (u32::from(self.top), u32::from(self.left));
        let mut out = Vec::new();
        if self.own_label {
            return out;
        }
        if self.top {
            for (j, l) in (0..self.nc).zip(&self.cols.labels) {
                out.push((rect.r0, rect.c0 + w + extra + j, l.1.clone()));
            }
        }
        if self.left {
            for (i, l) in (0..self.nr).zip(&self.rows.labels) {
                out.push((rect.r0 + h + i, rect.c0, l.1.clone()));
            }
        }
        out
    }

    fn area(&self, rect: Rect, extra: u32) -> Option<Rect> {
        let (h, w) = (u32::from(self.top), u32::from(self.left));
        (self.nr > 0 && self.nc > 0).then(|| {
            Rect::new(
                rect.r0,
                rect.c0,
                rect.r0 + h + self.nr - 1,
                rect.c0 + w + extra + self.nc - 1,
            )
        })
    }
}

struct Links {
    func: Agg,

    counta: bool,
}

struct Group {
    row: u32,
    blocks: Vec<(usize, u32)>,
}

impl Group {
    fn len(&self) -> u32 {
        self.blocks.iter().map(|b| b.1).sum()
    }
}

fn link_groups(plan: &Plan, sources: usize) -> Vec<Group> {
    (0u32..)
        .zip(&plan.cells)
        .filter_map(|(row, cols)| {
            let blocks: Vec<(usize, u32)> = (0..sources)
                .filter_map(|k| {
                    let n = cols
                        .iter()
                        .map(|c| c.iter().filter(|x| x.src == k).count() as u32)
                        .max()
                        .unwrap_or(0);
                    (n > 0).then_some((k, n))
                })
                .collect();
            (!blocks.is_empty()).then_some(Group { row, blocks })
        })
        .collect()
}

const NAME_SPLIT_PX: u32 = 40;

const LABEL_PX: u32 = 20;

const MAX_LEVEL: u8 = 7;

fn fn_name(f: Agg, counta: bool) -> &'static str {
    match f {
        Agg::Sum => "SUM",
        Agg::Count if counta => "COUNTA",
        Agg::Count => "COUNT",
        Agg::Average => "AVERAGE",
        Agg::Max => "MAX",
        Agg::Min => "MIN",
        Agg::Product => "PRODUCT",
        Agg::StDev => "STDEV",
        Agg::StDevP => "STDEVP",
        Agg::Var => "VAR",
        Agg::VarP => "VARP",
    }
}

fn quote_sheet(n: &str) -> String {
    if !n.is_empty()
        && n.chars().all(|c| c.is_alphanumeric() || c == '_')
        && !n.starts_with(|c: char| c.is_ascii_digit())
    {
        n.to_string()
    } else {
        format!("'{}'", n.replace('\'', "''"))
    }
}

fn copy_format(wb: &mut Workbook, addr: CellAddr, fmt: &str) {
    if wb.number_format(addr) != fmt {
        let cell = Rect::cell(addr.row, addr.col);
        wb.set_number_format_copy(addr.sheet, cell, fmt);
    }
}

fn has_contents(wb: &Workbook, addr: CellAddr) -> bool {
    wb.formula_text(addr).is_some()
        || (!matches!(wb.value(addr), Value::Blank) && wb.spill_anchor(addr).is_none())
}

fn blank_area(
    wb: &mut Workbook,
    sheet: SheetId,
    area: Rect,
    plan: &Plan,
    formulas: bool,
) -> Result<(), OmError> {
    let keeps_corner = plan.own_label || (plan.top && plan.left);
    for r in area.r0..=area.r1 {
        for c in area.c0..=area.c1 {
            if keeps_corner && (r, c) == (area.r0, area.c0) {
                continue;
            }
            let addr = CellAddr::new(sheet, r, c);
            if !has_contents(wb, addr) || (!formulas && wb.formula_text(addr).is_some()) {
                continue;
            }
            match wb.array_at(addr) {
                Some((_, block)) if !block.is_cell() => {
                    wb.clear_range(sheet, block)
                        .map_err(crate::objects::edit_error)?;
                }
                _ => {
                    wb.set_value(addr, Value::Blank);
                }
            }
        }
    }
    Ok(())
}

fn insertion_refused(
    wb: &Workbook,
    sheet: SheetId,
    area: Rect,
    plan: &Plan,
    groups: &[Group],
) -> bool {
    let Some(used) = wb.used_range(sheet) else {
        return false;
    };
    let added: u32 = groups.iter().map(Group::len).sum();
    if used.r1.saturating_add(added) >= max_rows() || (plan.left && used.c1 + 1 >= max_cols()) {
        return true;
    }
    let at_rows: Vec<u32> = groups
        .iter()
        .map(|g| area.r0 + u32::from(plan.top) + g.row)
        .collect();
    let at_col = plan.left.then(|| area.c0 + 1);
    let cuts = |b: Rect| {
        let outside = b.r0 < area.r0 || b.r1 > area.r1 || b.c0 < area.c0 || b.c1 > area.c1;
        (b.intersects(&area) && outside)
            || at_rows.iter().any(|&r| b.r0 < r && r <= b.r1)
            || at_col.is_some_and(|c| b.c0 < c && c <= b.c1)
    };

    let array_cut = |r: u32, c: u32| {
        wb.array_at(CellAddr::new(sheet, r, c))
            .is_some_and(|(_, b)| !b.is_cell() && cuts(b))
    };
    let mut edge_rows = at_rows.iter().copied().chain([area.r0, area.r1]);
    let mut edge_cols = [area.c0, area.c1].into_iter().chain(at_col);
    edge_rows.any(|r| (used.c0..=used.c1).any(|c| array_cut(r, c)))
        || edge_cols.any(|c| (used.r0..=used.r1).any(|r| array_cut(r, c)))
        || wb
            .pivots()
            .tables
            .iter()
            .any(|t| t.sheet == sheet && t.written.is_some_and(cuts))
        || wb
            .tables()
            .iter()
            .any(|t| t.sheet == sheet && at_rows.first().is_some_and(|&r| t.rect.r1 >= r))
}

impl Application<'_> {
    fn consolidate_links(
        &mut self,
        book: BookId,
        sheet: SheetId,
        rect: Rect,
        o: &Links,
        plan: &Plan,
        sources: usize,
    ) -> Result<(), OmError> {
        let (h, w) = (u32::from(plan.top), u32::from(plan.left));
        let groups = link_groups(plan, sources);
        let name = self.book(book)?.name.clone();
        let wb = &mut self.book_mut(book)?.wb;
        if let Some(area) = plan.area(rect, 0) {
            if insertion_refused(wb, sheet, area, plan, &groups) {
                return Ok(());
            }
        }

        wb.note_number_format(None);

        let extra = w;
        if plan.left {
            wb.insert_cols(sheet, rect.c0 + 1, 1)
                .map_err(crate::objects::edit_error)?;
        }
        if let Some(area) = plan.area(rect, extra) {
            blank_area(wb, sheet, area, plan, true)?;
        }
        for (r, c, v) in plan.labels(rect, extra) {
            wb.set_value(CellAddr::new(sheet, r, c), v);
        }

        let level = groups
            .iter()
            .map(|g| wb.row_outline_level(sheet, rect.r0 + h + g.row))
            .max()
            .map_or(1, |l| u8::try_from(l).unwrap_or(MAX_LEVEL))
            .clamp(1, MAX_LEVEL);
        let above = wb.outline_settings(sheet).0;

        let mut placed: Vec<(u32, u32)> = Vec::new();
        let mut added = 0;
        for g in &groups {
            let row = rect.r0 + h + g.row + added;
            let n = g.len();
            let first = row + u32::from(above);
            wb.insert_rows(sheet, first, n)
                .map_err(crate::objects::edit_error)?;
            placed.push((first, if above { row } else { row + n }));
            added += n;
        }
        let data_col = rect.c0 + w + extra;
        let f = fn_name(o.func, o.counta);
        for (g, &(first, total)) in groups.iter().zip(&placed) {
            let cols = &plan.cells[g.row as usize];
            let formats: Vec<Option<String>> = cols
                .iter()
                .map(|c| c.first().map(|x| wb.number_format(x.at).to_string()))
                .collect();
            let mut row = first;
            for &(k, n) in &g.blocks {
                for ((col, items), fmt) in (data_col..).zip(cols).zip(&formats) {
                    let Some(fmt) = fmt else { continue };
                    for (i, x) in (0..).zip(items.iter().filter(|x| x.src == k)) {
                        let source = wb
                            .sheet(x.at.sheet)
                            .map_or(String::new(), |s| s.name.to_string());
                        let text = format!(
                            "={}!${}${}",
                            quote_sheet(&source),
                            xlcalc::col_to_letters(x.at.col),
                            x.at.row + 1
                        );
                        let addr = CellAddr::new(sheet, row + i, col);
                        wb.set_formula(addr, &text)
                            .map_err(crate::objects::edit_error)?;
                        copy_format(wb, addr, fmt);
                    }
                }
                if plan.left {
                    let addr = CellAddr::new(sheet, row, rect.c0 + 1);
                    wb.set_value(addr, Value::Text(name.as_str().into()));
                }
                row += n;
            }
            let end = first + g.len() - 1;
            wb.set_subtotal_levels(sheet, first, end, Some(level));
            wb.set_rows_hidden(sheet, first, g.len(), true)
                .map_err(crate::objects::edit_error)?;
            for (col, fmt) in (data_col..).zip(&formats) {
                let Some(fmt) = fmt else { continue };
                let letters = xlcalc::col_to_letters(col);
                let range = if first == end {
                    format!("{letters}{}", first + 1)
                } else {
                    format!("{letters}{}:{letters}{}", first + 1, end + 1)
                };
                let addr = CellAddr::new(sheet, total, col);
                wb.set_formula(addr, &format!("={f}({range})"))
                    .map_err(crate::objects::edit_error)?;
                copy_format(wb, addr, fmt);
            }
        }
        if plan.left && !wb.col_hidden(sheet, rect.c0) {
            let px = wb.col_px(sheet, rect.c0);
            if px > NAME_SPLIT_PX {
                let set = |px| ColWidth {
                    px,
                    user: true,
                    best: false,
                };
                wb.set_column_record(sheet, rect.c0, rect.c0, set(LABEL_PX), false);
                wb.set_column_record(sheet, rect.c0 + 1, rect.c0 + 1, set(px - LABEL_PX), false);
            }
        }
        Ok(())
    }

    fn consolidate_values(
        &mut self,
        book: BookId,
        sheet: SheetId,
        rect: Rect,
        func: Agg,
        counta: bool,
        plan: &Plan,
    ) -> Result<(), OmError> {
        let (h, w) = (u32::from(plan.top), u32::from(plan.left));
        let wb = &mut self.book_mut(book)?.wb;
        if let Some(area) = plan.area(rect, 0) {
            blank_area(wb, sheet, area, plan, false)?;
        }
        for (r, c, v) in plan.labels(rect, 0) {
            wb.set_value(CellAddr::new(sheet, r, c), v);
        }
        for (i, row) in (0..).zip(&plan.cells) {
            for (j, items) in (0..).zip(row) {
                let Some(first) = items.first() else { continue };
                let addr = CellAddr::new(sheet, rect.r0 + h + i, rect.c0 + w + j);
                if has_contents(wb, addr) {
                    continue;
                }
                let fmt = wb.number_format(first.at).to_string();
                let values: Vec<Value> = items.iter().map(|x| x.value.clone()).collect();
                wb.set_value(addr, result_of(func, counta, &values));
                copy_format(wb, addr, &fmt);
            }
        }
        Ok(())
    }
}

impl Application<'_> {
    fn consolidate_sources(
        &self,
        book: BookId,
        sheet: SheetId,
        v: &Variant,
    ) -> Result<Vec<(SheetId, Rect)>, OmError> {
        let texts: Vec<String> = match v {
            Variant::Bstr(s) => vec![s.to_string()],
            Variant::Array(sa) => sa
                .data
                .iter()
                .map(|e| match e {
                    Variant::Bstr(s) => Ok(s.to_string()),
                    _ => Err(OmError::type_mismatch()),
                })
                .collect::<Result<_, _>>()?,
            _ => return Err(OmError::type_mismatch()),
        };
        let wb = &self.book(book)?.wb;
        let mut out: Vec<(SheetId, Rect)> = Vec::new();
        for t in &texts {
            match crate::pivot::parse_r1c1(wb, sheet, t) {
                Some(src) => {
                    if out.contains(&src) {
                        return Err(OmError::app_defined_with(
                            "Cannot add duplicate source reference.",
                        ));
                    }
                    out.push(src);
                }

                None if t.split_once('!').is_some_and(|(n, _)| {
                    let n = n.trim_matches('\'');
                    !n.is_empty() && wb.sheet_by_name(n).is_none()
                }) => {}
                None => return Err(OmError::app_defined_with("Reference isn't valid.")),
            }
        }
        Ok(out)
    }

    pub(crate) fn consolidate(
        &mut self,
        book: BookId,
        sheet: SheetId,
        rect: Rect,
        a: &Args,
    ) -> Result<Variant, OmError> {
        let func = match a.i32(1)? {
            None => Agg::Sum,
            Some(c) => agg_of(c).ok_or_else(|| {
                OmError::app_defined_with("Consolidate method of Range class failed")
            })?,
        };
        let old = self
            .book(book)?
            .consolidate
            .get(&sheet)
            .cloned()
            .unwrap_or_default();
        let sources = match a.get(0) {
            Some(v) => self.consolidate_sources(book, sheet, v)?,
            None if old.sources.is_empty() => {
                return Err(OmError::app_defined_with(
                    "No source references specified for consolidation.",
                ))
            }
            None => old.sources.clone(),
        };
        let top = a.bool(2)?.unwrap_or(old.top);
        let left = a.bool(3)?.unwrap_or(old.left);
        let mut create_links = false;
        if a.bool(4)?.unwrap_or(false) {
            if sources.iter().any(|&(s, _)| s == sheet) {
                return Err(OmError::app_defined_with(
                    "Cannot create links to consolidation sheet.",
                ));
            }
            create_links = true;
        }
        let counta = a.i32(1)? == Some(-4112);
        let (h, w) = (u32::from(top), u32::from(left));
        let wb = &self.book(book)?.wb;
        let mut cols = Keys::default();
        let mut rows = Keys::default();
        let mut srcs = Vec::new();

        let here = wb.value(CellAddr::new(sheet, rect.r0, rect.c0));
        let own_label = rect.is_cell() && top != left && !matches!(here, Value::Blank);
        if own_label {
            let keys = if top { &mut cols } else { &mut rows };
            keys.labels.push(label_of(&here));
        }
        for &(s, r) in &sources {
            let val = |i: u32, j: u32| wb.value(CellAddr::new(s, r.r0 + i, r.c0 + j));
            let col_labels: Vec<(String, Value)> = if top {
                (w..r.cols()).map(|j| label_of(&val(0, j))).collect()
            } else {
                Vec::new()
            };
            let row_labels: Vec<(String, Value)> = if left {
                (h..r.rows()).map(|i| label_of(&val(i, 0))).collect()
            } else {
                Vec::new()
            };
            if !(own_label && top) {
                cols.merge(&col_labels, !rect.is_cell());
            }
            if !(own_label && left) {
                rows.merge(&row_labels, !rect.is_cell());
            }
            srcs.push(Src {
                sheet: s,
                rect: r,
                col_labels,
                row_labels,
            });
        }
        let (mut nr, mut nc) = (rows.labels.len() as u32, cols.labels.len() as u32);
        for src in &srcs {
            if !top {
                nc = nc.max(src.rect.cols().saturating_sub(w));
            }
            if !left {
                nr = nr.max(src.rect.rows().saturating_sub(h));
            }
        }

        if !rect.is_cell() {
            nr = nr.min(rect.rows().saturating_sub(h));
            nc = nc.min(rect.cols().saturating_sub(w));
        }
        let mut cells: Vec<Vec<Vec<Item>>> = (0..nr)
            .map(|_| (0..nc).map(|_| Vec::new()).collect())
            .collect();
        for (k, src) in srcs.iter().enumerate() {
            let r = src.rect;
            for i in h..r.rows() {
                for j in w..r.cols() {
                    let ri = if left {
                        rows.find(&src.row_labels[(i - h) as usize].0)
                    } else {
                        Some((i - h) as usize)
                    };
                    let ci = if top {
                        cols.find(&src.col_labels[(j - w) as usize].0)
                    } else {
                        Some((j - w) as usize)
                    };
                    let (Some(ri), Some(ci)) = (ri, ci) else {
                        continue;
                    };
                    let at = CellAddr::new(src.sheet, r.r0 + i, r.c0 + j);
                    let value = wb.value(at);
                    if !takes_part(&value, counta) {
                        continue;
                    }
                    if let Some(cell) = cells.get_mut(ri).and_then(|x| x.get_mut(ci)) {
                        cell.push(Item { src: k, at, value });
                    }
                }
            }
        }
        let plan = Plan {
            cols,
            rows,
            nr,
            nc,
            cells,
            top,
            left,
            own_label,
        };
        if plan.is_empty() {
            if let Some(area) = plan.area(rect, 0) {
                blank_area(&mut self.book_mut(book)?.wb, sheet, area, &plan, false)?;
            }
        } else if create_links {
            let links = Links { func, counta };
            self.consolidate_links(book, sheet, rect, &links, &plan, sources.len())?;
        } else {
            self.consolidate_values(book, sheet, rect, func, counta, &plan)?;
        }
        self.book_mut(book)?
            .consolidate
            .insert(sheet, ConsolidateState { sources, top, left });
        Ok(Variant::Bool(true))
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn errors_and_text_take_part_by_function() {
        let err = Value::Error(XlError::DIV0);
        let text = Value::Text("x".into());
        assert!(takes_part(&err, false));
        assert!(!takes_part(&text, false));
        assert!(takes_part(&text, true));
        assert!(!takes_part(&Value::Blank, true));
        let with_error = [Value::Number(2.0), err.clone()];
        let sum = result_of(Agg::Sum, false, &with_error);
        assert!(matches!(sum, Value::Error(XlError::DIV0)));

        let nums = result_of(Agg::Count, false, &with_error);
        assert!(matches!(nums, Value::Number(n) if n == 1.0));
        let all = result_of(Agg::Count, true, &with_error);
        assert!(matches!(all, Value::Number(n) if n == 2.0));
        let none = result_of(Agg::Count, false, std::slice::from_ref(&err));
        assert!(matches!(none, Value::Number(n) if n == 0.0));
    }

    #[test]
    fn a_source_gets_as_many_link_rows_as_its_fullest_column() {
        let item = |src| Item {
            src,
            at: CellAddr::new(SheetId(0), 0, 0),
            value: Value::Number(1.0),
        };
        let plan = Plan {
            cols: Keys::default(),
            rows: Keys::default(),
            nr: 2,
            nc: 2,
            cells: vec![
                vec![vec![item(0), item(1), item(1)], vec![item(1)]],
                vec![Vec::new(), Vec::new()],
            ],
            top: true,
            left: true,
            own_label: false,
        };
        let groups = link_groups(&plan, 2);

        assert_eq!(groups.len(), 1);
        assert_eq!(groups[0].blocks, [(0, 1), (1, 2)]);
        assert_eq!(groups[0].len(), 3);
    }
}
