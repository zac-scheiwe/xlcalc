use crate::workbook::Command;
use std::cmp::Ordering;
use std::sync::Arc;

use crate::address::{CellAddr, Rect, SheetId, MAX_COLS, MAX_ROWS};
use crate::calc::{self, RecalcReport};
use crate::formula::{CRef, Formula, Rc, SheetRef};
use crate::formula_print::print_node;
use crate::functions::arrays::collate;
use crate::structure::map_refs;
use crate::style::{CellColor, Pattern};
use crate::value::Value;
use crate::workbook::tables::TableKey;
use crate::workbook::{CalcMode, EditError, Entry, Selection, Workbook};

pub(crate) type MovedCell = Result<Value, (Arc<Formula>, String, Value, Option<(u32, u32)>)>;

#[derive(Clone, Copy, Debug)]
pub struct SortKey {
    pub index: u32,

    pub descending: bool,

    pub color: Option<SortColor>,
}

#[derive(Clone, Copy, Debug)]
pub struct SortColor {
    pub font: bool,

    pub rgb: Option<u32>,
}

#[derive(Clone, Debug, Default)]
pub struct SortSpec {
    pub keys: Vec<SortKey>,

    pub header: bool,

    pub by_columns: bool,

    pub match_case: bool,

    pub custom: Vec<Vec<String>>,

    pub text_as_number: Vec<bool>,
}

fn rank(v: &Value) -> u8 {
    match v {
        Value::Number(_) => 0,
        Value::Text(_) => 1,
        Value::Bool(_) => 2,
        Value::Error(_) => 3,

        Value::Array(_) => 4,
        _ => 5,
    }
}

fn cmp_asc(a: &Value, b: &Value, match_case: bool) -> Ordering {
    match (a, b) {
        (Value::Number(x), Value::Number(y)) => x.partial_cmp(y).unwrap_or(Ordering::Equal),
        (Value::Text(x), Value::Text(y)) => {
            if match_case {
                collate::compare_match_case(x, y)
            } else {
                collate::compare(x, y)
            }
        }
        (Value::Bool(x), Value::Bool(y)) => x.cmp(y),
        _ => rank(a).cmp(&rank(b)),
    }
}

fn list_pos(v: &Value, list: &[String], match_case: bool) -> Option<usize> {
    let Value::Text(t) = v else {
        return None;
    };
    list.iter().position(|e| {
        if match_case {
            e == t.as_ref()
        } else {
            e.to_lowercase() == t.to_lowercase()
        }
    })
}

fn cmp_key(a: &Value, b: &Value, desc: bool, match_case: bool, list: &[String]) -> Ordering {
    let (ab, bb) = (rank(a) == 5, rank(b) == 5);
    if ab || bb {
        return ab.cmp(&bb);
    }
    let o = if list.is_empty() {
        cmp_asc(a, b, match_case)
    } else {
        match (list_pos(a, list, match_case), list_pos(b, list, match_case)) {
            (Some(x), Some(y)) => x.cmp(&y),
            (Some(_), None) => Ordering::Less,
            (None, Some(_)) => Ordering::Greater,
            (None, None) => cmp_asc(a, b, match_case),
        }
    };
    if desc {
        o.reverse()
    } else {
        o
    }
}

fn move_axis(a: Rc, b: Rc, host: u32, max: u32) -> Option<(Rc, Rc)> {
    let pos = |x: Rc| {
        if x.abs {
            x.v as i64
        } else {
            host as i64 + x.v as i64
        }
    };
    let (pa, pb) = (pos(a), pos(b));
    let (lo, hi) = (pa.min(pb), pa.max(pb));
    if hi < 0 || lo >= max as i64 {
        return None;
    }
    let enc = |p: i64, x: Rc| {
        if x.abs {
            Rc {
                v: p as i32,
                abs: true,
            }
        } else {
            Rc {
                v: (p - host as i64) as i32,
                abs: false,
            }
        }
    };
    Some((enc(lo.max(0), a), enc(hi.min(max as i64 - 1), b)))
}

pub(crate) fn move_ref(cref: &CRef, to: CellAddr) -> Option<CRef> {
    Some(match *cref {
        CRef::Cell { row, col } => {
            let (r, _) = move_axis(row, row, to.row, MAX_ROWS)?;
            let (c, _) = move_axis(col, col, to.col, MAX_COLS)?;
            let pos = |x: Rc, h: u32| {
                if x.abs {
                    x.v as i64
                } else {
                    h as i64 + x.v as i64
                }
            };

            if pos(row, to.row) < 0
                || pos(row, to.row) >= MAX_ROWS as i64
                || pos(col, to.col) < 0
                || pos(col, to.col) >= MAX_COLS as i64
            {
                return None;
            }
            CRef::Cell { row: r, col: c }
        }
        CRef::Area { r0, c0, r1, c1 } => {
            let (r0, r1) = move_axis(r0, r1, to.row, MAX_ROWS)?;
            let (c0, c1) = move_axis(c0, c1, to.col, MAX_COLS)?;
            CRef::Area { r0, c0, r1, c1 }
        }
        CRef::Cols { c0, c1 } => {
            let (c0, c1) = move_axis(c0, c1, to.col, MAX_COLS)?;
            CRef::Cols { c0, c1 }
        }
        CRef::Rows { r0, r1 } => {
            let (r0, r1) = move_axis(r0, r1, to.row, MAX_ROWS)?;
            CRef::Rows { r0, r1 }
        }
    })
}

pub(crate) fn retarget(cref: &CRef, dr: i64, dc: i64) -> CRef {
    let mv = |x: Rc, d: i64| {
        if x.abs {
            x
        } else {
            Rc {
                v: (x.v as i64 - d) as i32,
                abs: false,
            }
        }
    };
    match *cref {
        CRef::Cell { row, col } => CRef::Cell {
            row: mv(row, dr),
            col: mv(col, dc),
        },
        CRef::Area { r0, c0, r1, c1 } => CRef::Area {
            r0: mv(r0, dr),
            c0: mv(c0, dc),
            r1: mv(r1, dr),
            c1: mv(c1, dc),
        },
        CRef::Cols { c0, c1 } => CRef::Cols {
            c0: mv(c0, dc),
            c1: mv(c1, dc),
        },
        CRef::Rows { r0, r1 } => CRef::Rows {
            r0: mv(r0, dr),
            r1: mv(r1, dr),
        },
    }
}

struct OutlineScan {
    r0: i64,
    r1: i64,
    low: u32,
    differs: bool,
    subtotal: bool,
}

impl Workbook {
    pub fn sort_order(&self, sheet: SheetId, rect: Rect, spec: &SortSpec) -> Vec<u32> {
        let (lines, skip) = Self::sort_lines(rect, spec);
        let n = lines.saturating_sub(skip);
        let at = |line: u32, key: u32| -> Value {
            let (r, c) = if spec.by_columns {
                (rect.r0 + key, rect.c0 + line)
            } else {
                (rect.r0 + line, rect.c0 + key)
            };
            let a = CellAddr::new(sheet, r, c);
            if let (Some(f), Some(sh)) = (self.formula_at(a), self.sheet(sheet)) {
                if sh.spills.is_blocked(f) {
                    return Value::Array(std::sync::Arc::new(crate::value::Array::new(
                        1,
                        1,
                        vec![Value::Blank],
                    )));
                }
            }
            self.value(a)
        };
        let theme = &self.style_table().theme;

        let keyval = |line: u32, k: &SortKey| -> Value {
            let Some(sc) = k.color else {
                return at(line, k.index);
            };
            let (r, c) = if spec.by_columns {
                (rect.r0 + k.index, rect.c0 + line)
            } else {
                (rect.r0 + line, rect.c0 + k.index)
            };
            let st = self.cell_style(CellAddr::new(sheet, r, c));
            let has = if sc.font {
                match st.font.color {
                    CellColor::Auto => None,
                    c => Some(theme.rgb(c, 0)),
                }
            } else if st.fill.pattern == Pattern::None {
                None
            } else {
                Some(theme.rgb(st.fill.color, 0xFF_FFFF))
            };
            Value::Number(if has == sc.rgb { 0.0 } else { 1.0 })
        };
        let keyvals: Vec<Vec<Value>> = (0..n)
            .map(|i| {
                spec.keys
                    .iter()
                    .enumerate()
                    .map(|(ki, k)| match keyval(skip + i, k) {
                        Value::Text(t) if spec.text_as_number.get(ki) == Some(&true) => {
                            crate::coerce::parse_number_text(&t, self.input_env())
                                .map_or(Value::Text(t), |p| Value::Number(p.value))
                        }
                        v => v,
                    })
                    .collect::<Vec<_>>()
            })
            .collect();
        let mut order: Vec<u32> = (0..n).collect();
        order.sort_by(|&a, &b| {
            for (ki, k) in spec.keys.iter().enumerate() {
                let o = cmp_key(
                    &keyvals[a as usize][ki],
                    &keyvals[b as usize][ki],
                    k.descending,
                    spec.match_case,
                    spec.custom.get(ki).map_or(&[][..], |l| l.as_slice()),
                );
                if o != Ordering::Equal {
                    return o;
                }
            }
            Ordering::Equal
        });
        order
    }

    fn sort_lines(rect: Rect, spec: &SortSpec) -> (u32, u32) {
        let lines = if spec.by_columns {
            rect.cols()
        } else {
            rect.rows()
        };

        (lines, u32::from(spec.header && !spec.by_columns))
    }

    fn sort_row_level(&self, sheet: SheetId, r: i64, rw_mac: i64) -> u32 {
        if r < 0 || r >= rw_mac {
            0
        } else {
            self.row_level(sheet, r as u32) - 1
        }
    }

    fn sort_rw_mac(&self, sheet: SheetId) -> i64 {
        self.view_used_r1(sheet).map_or(0, |r| i64::from(r) + 1)
    }

    fn sort_outline_scan(&self, sheet: SheetId, sel: Rect) -> OutlineScan {
        let rw_mac = self.sort_rw_mac(sheet);
        let lv = |r: i64| self.sort_row_level(sheet, r, rw_mac);
        let (mut r0, mut r1) = (i64::from(sel.r0), i64::from(sel.r1));
        let (mut low, mut at_low, mut differs) = (lv(r0), 1u32, false);
        let mut r = r0 + 1;
        while r <= r1 && (low > 0 || at_low < 2) {
            let l = lv(r);
            if l == low {
                at_low += 1;
            } else if !differs || l < low {
                differs = true;
                if l < low {
                    low = l;
                    at_low = 1;
                }
            }
            r += 1;
        }
        if !differs {
            return OutlineScan {
                r0,
                r1,
                low,
                differs,
                subtotal: false,
            };
        }
        let test = if self.outline_settings(sheet).0 {
            while r1 + 1 < rw_mac && lv(r1 + 1) > low {
                r1 += 1;
            }
            while r0 < rw_mac && lv(r0) > low {
                r0 += 1;
            }
            (r0 < r1).then(|| {
                if at_low == 1 && lv(r0 + 1) == low + 1 {
                    r0 += 1;
                    low += 1;
                }
                (r0, r0 + 1)
            })
        } else {
            while r0 > 0 && lv(r0 - 1) > low {
                r0 -= 1;
            }
            while r1 >= 0 && lv(r1) > low {
                r1 -= 1;
            }
            (r0 < r1).then(|| {
                if at_low == 1 && lv(r1 - 1) == low + 1 {
                    r1 -= 1;
                    low += 1;
                }
                (r1, r1 - 1)
            })
        };
        let subtotal = test.is_some_and(|(row, next)| {
            let one = Rect {
                r0: row as u32,
                r1: row as u32,
                ..sel
            };
            !self.subtotal_rows(sheet, one).is_empty() && !self.row_hidden(sheet, next as u32)
        });
        OutlineScan {
            r0,
            r1,
            low,
            differs,
            subtotal,
        }
    }

    fn sort_outline_refused(&self, sheet: SheetId, cols: bool, r0: u32, r1: u32, low: u32) -> bool {
        let lv = |r: u32| self.line_level0(sheet, cols, r);
        let hidden = |r: u32| self.line_hidden(sheet, cols, r);
        let above = self.summary_first(sheet, cols);
        (r0..=r1).any(|r| {
            if above {
                r < r1 && lv(r) <= low && hidden(r) && lv(r + 1) > low
            } else {
                r > r0 && lv(r - 1) > low && lv(r) <= low && hidden(r)
            }
        })
    }

    fn line_level0(&self, sheet: SheetId, cols: bool, i: u32) -> u32 {
        if cols {
            self.col_outline_level(sheet, i) - 1
        } else {
            self.row_level(sheet, i) - 1
        }
    }

    fn line_hidden(&self, sheet: SheetId, cols: bool, i: u32) -> bool {
        if cols {
            self.col_hidden(sheet, i)
        } else {
            self.row_hidden(sheet, i)
        }
    }

    fn summary_first(&self, sheet: SheetId, cols: bool) -> bool {
        let s = self.outline_settings(sheet);
        if cols {
            s.1
        } else {
            s.0
        }
    }

    fn sort_cl_mac(&self, sheet: SheetId) -> i64 {
        let cells = self
            .sheet(sheet)
            .and_then(|sh| sh.cells.sticky_bounds())
            .map(|b| i64::from(b.c1) + 1);
        let v = self.views.get(&sheet);
        let lv = v.and_then(|v| v.col_levels.keys().next_back().map(|&c| i64::from(c) + 1));
        let hid = v.and_then(|v| v.hidden_cols.iter().next_back().map(|&c| i64::from(c) + 1));
        [cells, lv, hid].into_iter().flatten().max().unwrap_or(0)
    }

    fn sort_outline_scan_cols(&self, sheet: SheetId, sel: Rect) -> OutlineScan {
        let mac = self.sort_cl_mac(sheet);
        let lv = |c: i64| {
            if c < 0 || c >= mac {
                0
            } else {
                self.line_level0(sheet, true, c as u32)
            }
        };
        let (mut c0, mut c1) = (i64::from(sel.c0), i64::from(sel.c1));
        let first = lv(c0);
        let (mut low, mut differs) = (first, false);
        for c in c0 + 1..=c1.min(mac - 1) {
            let l = lv(c);
            if !differs {
                if l != first {
                    differs = true;
                    low = first.min(l);
                    if low == 0 {
                        break;
                    }
                }
            } else {
                low = low.min(l);
            }
        }
        if self.summary_first(sheet, true) {
            while c1 + 1 < mac && lv(c1 + 1) > low {
                c1 += 1;
            }
            while c0 < mac && lv(c0) > low {
                c0 += 1;
            }
        } else {
            while c0 > 0 && lv(c0 - 1) > low {
                c0 -= 1;
            }
            while c1 >= 0 && lv(c1) > low {
                c1 -= 1;
            }
        }
        OutlineScan {
            r0: c0,
            r1: c1,
            low,
            differs,
            subtotal: false,
        }
    }

    fn adjust_sort_cols(&self, sheet: SheetId, rect: Rect, mode: Option<u32>) -> Option<Rect> {
        let (mut c0, mut c1) = (i64::from(rect.c0), i64::from(rect.c1));
        let skip = |c: i64| {
            self.col_hidden(sheet, c as u32)
                && mode.is_none_or(|m| self.line_level0(sheet, true, c as u32) > m)
        };
        if c0 < c1 {
            while c0 < c1 && skip(c0) {
                c0 += 1;
            }
            while c0 < c1 && skip(c1) {
                c1 -= 1;
            }
        }
        (c0 <= c1 && c1 >= 0).then_some(Rect {
            c0: c0 as u32,
            c1: c1 as u32,
            ..rect
        })
    }

    fn adjust_sort_rows(&self, sheet: SheetId, rect: Rect, mode: Option<u32>) -> Option<Rect> {
        let rw_mac = self.sort_rw_mac(sheet);
        let (mut r0, mut r1) = (i64::from(rect.r0), i64::from(rect.r1).min(rw_mac - 1));
        let lv = |r: i64| self.sort_row_level(sheet, r, rw_mac);
        let hidden = |r: i64| self.row_hidden(sheet, r as u32);
        if r0 < r1 {
            while r0 < r1 && hidden(r0) && mode.is_none_or(|m| lv(r0) <= m) {
                r0 += 1;
            }
            while r0 < r1 && hidden(r1) && mode.is_none_or(|m| lv(r1) <= m) {
                r1 -= 1;
            }
        }
        (r0 <= r1 && r1 >= 0).then_some(Rect {
            r0: r0 as u32,
            r1: r1 as u32,
            ..rect
        })
    }

    fn outline_order(
        &self,
        sheet: SheetId,
        cols: bool,
        base: u32,
        sorted: &[u32],
        last: u32,
        top: u32,
    ) -> Vec<u32> {
        let (fl, ll) = (0, (last - base).min(sorted.len() as u32 - 1));
        let mut rank = vec![0usize; sorted.len()];
        for (i, &s) in sorted.iter().enumerate() {
            rank[s as usize] = i;
        }
        let deep = |j: u32| self.line_level0(sheet, cols, base + j) > top;
        let hidden = |j: u32| !deep(j) && self.line_hidden(sheet, cols, base + j);
        let mut keys: Vec<u32> = (fl..=ll).filter(|&j| !deep(j) && !hidden(j)).collect();
        keys.sort_by_key(|&j| rank[j as usize]);
        let above = self.summary_first(sheet, cols);
        let mut order: Vec<u32> = (0..sorted.len() as u32).collect();
        let (mut cursor, mut slot) = (fl, fl);
        for key in keys {
            while cursor <= ll && deep(cursor) {
                cursor += 1;
            }
            while slot <= ll && cursor <= ll && hidden(cursor) {
                order[slot as usize] = cursor;
                slot += 1;
                cursor += 1;
            }
            if slot > ll {
                break;
            }
            let mut block: Vec<u32> = Vec::new();
            if above {
                block.push(key);
                let mut j = key + 1;
                while j <= ll && deep(j) {
                    block.push(j);
                    j += 1;
                }
            } else {
                let mut j = key;
                while j > fl && deep(j - 1) {
                    j -= 1;
                }
                block.extend(j..=key);
            }
            for row in block {
                if slot <= ll {
                    order[slot as usize] = row;
                    slot += 1;
                }
            }
            cursor += 1;
        }

        while slot <= ll {
            while cursor <= ll && deep(cursor) {
                cursor += 1;
            }
            if cursor > ll || !hidden(cursor) {
                break;
            }
            order[slot as usize] = cursor;
            slot += 1;
            cursor += 1;
        }
        order
    }

    pub fn sort_table<'a>(
        &mut self,
        key: impl Into<TableKey<'a>>,
        spec: &SortSpec,
    ) -> Result<RecalcReport, EditError> {
        let key = key.into();
        self.command(Command::Sort, |wb| wb.sort_table_run(key, spec))
    }

    pub fn sort_filter(
        &mut self,
        sheet: SheetId,
        rect: Rect,
        spec: &SortSpec,
    ) -> Result<RecalcReport, EditError> {
        let r = self.sort_range(sheet, rect, spec);
        if r.is_ok() {
            self.view_mut(sheet).filter_sorted = true;
        }
        r
    }

    fn sort_table_run(
        &mut self,
        key: TableKey<'_>,
        spec: &SortSpec,
    ) -> Result<RecalcReport, EditError> {
        let ti = self.table_position(key)?;
        let t = &self.tables[ti];
        let (sheet, mut rect) = (t.sheet, t.rect);
        if t.headers {
            rect.r0 += 1;
        }
        if t.totals_row().is_some() {
            rect.r1 -= 1;
        }
        let r = self.sort_range(sheet, rect, spec);

        if r.is_err() {
            self.view_sorted(sheet, rect);
        }

        if sheet != self.session.active {
            self.session.selection.insert(sheet, Selection::of(&[rect]));
        }
        if r.is_ok() {
            if let Ok(ti) = self.table_position(key) {
                let t = &mut self.tables[ti];
                t.sort_state = t.headers && t.show_filter;
            }
        }
        r
    }

    pub fn sort_range(
        &mut self,
        sheet: SheetId,
        rect: Rect,
        spec: &SortSpec,
    ) -> Result<RecalcReport, EditError> {
        self.command(Command::Sort, |wb| {
            let touch = wb.sort_touch_row(sheet, rect);
            let r = wb.sort_range_run(sheet, rect, spec);
            if r.is_ok() {
                wb.drawing_sorted(sheet, rect);
                if let Some(t) = touch {
                    wb.view_touch_row_pub(sheet, t);
                }
            }
            r
        })
    }

    pub fn sort_auto_select(&self, sheet: SheetId, rect: Rect) -> Option<Rect> {
        if let Some(t) = self
            .table_at(CellAddr::new(sheet, rect.r0, rect.c0))
            .filter(|t| t.rect.contains(rect.r1, rect.c1))
        {
            return Some(Rect {
                r1: t.rect.r1 - u32::from(t.totals && t.rect.r1 > t.rect.r0),
                ..t.rect
            });
        }
        if rect.is_cell() || self.merge_area_exact(sheet, rect) {
            return self.select_for_single_cell(sheet, rect);
        }
        Some(rect)
    }

    pub fn sort_straddles_table(&self, sheet: SheetId, rect: Rect) -> bool {
        self.tables.iter().any(|t| {
            t.sheet == sheet
                && t.rect.intersects(&rect)
                && !(t.rect.r0 <= rect.r0
                    && rect.r1 <= t.rect.r1
                    && t.rect.c0 <= rect.c0
                    && rect.c1 <= t.rect.c1)
        })
    }

    fn sort_touch_row(&self, sheet: SheetId, rect: Rect) -> Option<u32> {
        let sh = self.sheet(sheet)?;
        let top = rect.r1.min(self.view_used_r1(sheet)?);
        let last = (rect.r0..=top)
            .rev()
            .find(|&r| (rect.c0..=rect.c1).any(|c| sh.cells.get(r, c).is_some()))?;
        let vis = (rect.r0..=last)
            .rev()
            .find(|&r| !self.row_hidden(sheet, r))?;
        Some(vis + 1)
    }

    fn sort_range_run(
        &mut self,
        sheet: SheetId,
        rect: Rect,
        spec: &SortSpec,
    ) -> Result<RecalcReport, EditError> {
        if let Some(sh) = self.sheet_mut(sheet) {
            sh.spills.note_rows(sh.cells.sticky_bounds().map(|b| b.r1));
            sh.spills.relayout();
            let ids = sh.spills.active_ids();
            crate::calc::snapshot_holes(self, sheet, &ids);
        }

        if self
            .data_tables
            .iter()
            .any(|t| t.sheet == sheet && t.interior().intersects(&rect))
        {
            return Err(EditError::Compile(crate::datatable::PART_ERROR.into()));
        }

        self.note_cell_format(CellAddr::new(sheet, rect.r0, rect.c0));
        if self.tables.iter().any(|t| t.sheet == sheet && t.totals) {
            self.snap_totals_before_sort(sheet, rect);
        }
        let r = self.sort_range_impl(sheet, rect, spec);

        if r.is_ok() {
            let (dr, dc) = match (spec.header, spec.by_columns) {
                (true, false) => (1, 0),
                (true, true) => (0, 1),
                _ => (0, 0),
            };
            if sheet != self.session.active {
                self.session
                    .fill_selected
                    .insert(sheet, (rect.r0 + dr, rect.c0 + dc));
            }

            self.notify_list_select(sheet, rect.r0 + dr, rect.c0 + dc);
        }
        r
    }

    fn sort_range_impl(
        &mut self,
        sheet: SheetId,
        rect: Rect,
        spec: &SortSpec,
    ) -> Result<RecalcReport, EditError> {
        let sh = self.sheet(sheet).ok_or(EditError::NoSuchSheet)?;

        if !sh.spills.intersecting(&rect).is_empty() {
            return Err(EditError::Compile(
                "You can't change part of an array.".into(),
            ));
        }
        crate::structure::range_too_large(&rect)?;

        if self
            .merges(sheet)
            .iter()
            .any(|m| crate::cse::inside(&rect, m))
        {
            return Ok(RecalcReport::default());
        }

        let mut merge_rect = Some(rect);
        if !spec.by_columns {
            let first = rect.r0 + u32::from(spec.header);

            if spec.header && rect.r1 > rect.r0 {
                merge_rect = Some(Rect::new(first, rect.c0, rect.r1, rect.c1));
            }
            if (first..=rect.r1).any(|r| self.row_hidden(sheet, r)) {
                let vis = |r: &u32| !self.row_hidden(sheet, *r);
                merge_rect = (first..=rect.r1)
                    .find(vis)
                    .zip((first..=rect.r1).rev().find(vis))
                    .map(|(a, b)| Rect::new(a, rect.c0, b, rect.c1));
            }
        }

        let mut merge_err = None;
        if let Some(m) = merge_rect {
            if let Err(e) = self.guard_merges_sort(sheet, m) {
                let outlined = !spec.by_columns
                    && (rect.r0..=rect.r1).any(|r| self.row_outline_level(sheet, r) > 1);
                if !outlined {
                    return Err(e);
                }
                merge_err = Some(e);
            }
        }
        let width = if spec.by_columns {
            rect.rows()
        } else {
            rect.cols()
        };
        if spec.keys.is_empty() || spec.keys.iter().any(|k| k.index >= width) {
            return Err(EditError::Compile("sort key is outside the range".into()));
        }
        let (_, skip) = Self::sort_lines(rect, spec);

        let mut mode = None;
        let given = rect;
        let (mut rect, mut skip) = (rect, skip);
        let outlined = self.views.get(&sheet).is_some_and(|v| !v.levels.is_empty());
        let col_outlined = self
            .views
            .get(&sheet)
            .is_some_and(|v| !v.col_levels.is_empty());
        if spec.by_columns && col_outlined {
            let scan = self.sort_outline_scan_cols(sheet, rect);
            let moved = scan.r0 != i64::from(rect.c0) || scan.r1 != i64::from(rect.c1);
            if scan.differs || moved {
                if scan.differs
                    && scan.r0 <= scan.r1
                    && scan.r1 >= 0
                    && self.sort_outline_refused(
                        sheet,
                        true,
                        scan.r0 as u32,
                        scan.r1 as u32,
                        scan.low,
                    )
                {
                    return Err(EditError::Compile(
                        "Sorry, we can't sort because this list is filtered and grouped".into(),
                    ));
                }
                mode = Some(scan.low);
                if scan.r0 <= scan.r1 && scan.r1 >= 0 {
                    rect = Rect {
                        c0: scan.r0 as u32,
                        c1: scan.r1 as u32,
                        ..rect
                    };
                } else {
                    self.view_sorted(sheet, given);
                    return Ok(RecalcReport::default());
                }
            }
        }
        if !spec.by_columns && rect.rows() > skip && outlined {
            let sel = Rect {
                r0: rect.r0 + skip,
                ..rect
            };
            let scan = self.sort_outline_scan(sheet, sel);
            if scan.subtotal {
                self.remove_subtotal_rows(sheet, sel, false)?;
                if let Some(e) = merge_err.take() {
                    return Err(e);
                }
            }
            if scan.differs {
                if scan.r1 < scan.r0 || scan.r1 < 0 {
                    self.view_sorted(sheet, given);
                    return Ok(RecalcReport::default());
                }
                let (r0, r1) = (scan.r0 as u32, scan.r1 as u32);
                if self.sort_outline_refused(sheet, false, r0, r1, scan.low) {
                    return Err(EditError::Compile(
                        "Sorry, we can't sort because this list is filtered and grouped".into(),
                    ));
                }
                mode = Some(scan.low);
                rect = Rect { r0, r1, ..rect };
                skip = 0;
            }
        }
        if !spec.by_columns {
            let data = Rect {
                r0: rect.r0 + skip,
                ..rect
            };
            match self.adjust_sort_rows(sheet, data, mode) {
                Some(d) => {
                    rect = d;
                    skip = 0;
                }
                None => {
                    self.view_sorted(sheet, given);
                    return Ok(RecalcReport::default());
                }
            }
        }
        if spec.by_columns {
            match self.adjust_sort_cols(sheet, rect, mode) {
                Some(d) => rect = d,
                None => {
                    self.view_sorted(sheet, given);
                    return Ok(RecalcReport::default());
                }
            }
        }
        let unheaded;
        let spec = if skip == 0 && spec.header {
            unheaded = SortSpec {
                header: false,
                ..spec.clone()
            };
            &unheaded
        } else {
            spec
        };
        let mut order = self.sort_order(sheet, rect, spec);

        let hidden_line = |line: u32| {
            if spec.by_columns {
                self.col_hidden(sheet, rect.c0 + line)
            } else {
                self.row_hidden(sheet, rect.r0 + line)
            }
        };
        if let Some(e) = merge_err {
            return if mode.is_some() {
                Ok(RecalcReport::default())
            } else {
                Err(e)
            };
        }
        let mut row_moves: Vec<(u32, u32)> = Vec::new();
        let mut col_moves: Vec<(u32, u32)> = Vec::new();
        if let Some(top) = mode.filter(|_| !order.is_empty()) {
            let base = if spec.by_columns {
                rect.c0
            } else {
                rect.r0 + skip
            };
            let last = base + order.len() as u32 - 1;
            let cols = spec.by_columns;
            order = self.outline_order(sheet, cols, base, &order, last, top);
            let moves: Vec<(u32, u32)> = order
                .iter()
                .enumerate()
                .filter(|&(j, &s)| j as u32 != s)
                .map(|(j, &s)| (base + s, base + j as u32))
                .collect();
            if cols {
                col_moves = moves;
            } else {
                row_moves = moves;
            }
        } else if (0..order.len() as u32).any(|j| hidden_line(skip + j)) {
            let mut vis = order
                .iter()
                .copied()
                .filter(|&s| !hidden_line(skip + s))
                .collect::<Vec<_>>()
                .into_iter();
            order = (0..order.len() as u32)
                .map(|j| {
                    if hidden_line(skip + j) {
                        j
                    } else {
                        vis.next().unwrap()
                    }
                })
                .collect();
        }
        let entries: Vec<(u32, u32, Entry)> = self
            .sheet(sheet)
            .unwrap()
            .cells
            .iter_rect(rect)
            .map(|(r, c, e)| (r, c, e.clone()))
            .collect();
        let by_pos: rustc_hash::FxHashMap<(u32, u32), usize> = entries
            .iter()
            .enumerate()
            .map(|(i, e)| ((e.0, e.1), i))
            .collect();
        let mut plan: Vec<(CellAddr, MovedCell)> = Vec::new();

        let mut sources: rustc_hash::FxHashMap<CellAddr, (crate::workbook::FormulaId, i64)> =
            Default::default();
        let mut fmt_pairs: Vec<(CellAddr, CellAddr)> = Vec::new();
        for (j, &src_line) in order.iter().enumerate() {
            let (dst_line, src_line) = (skip + j as u32, skip + src_line);

            for k in 0..width {
                let pos = |line: u32| {
                    if spec.by_columns {
                        (rect.r0 + k, rect.c0 + line)
                    } else {
                        (rect.r0 + line, rect.c0 + k)
                    }
                };
                let (sr, sc) = pos(src_line);
                let (dr, dc) = pos(dst_line);
                let to = CellAddr::new(sheet, dr, dc);
                fmt_pairs.push((CellAddr::new(sheet, sr, sc), to));
                match by_pos.get(&(sr, sc)).map(|&i| &entries[i].2) {
                    None => plan.push((to, Ok(Value::Blank))),
                    Some(Entry::Const(v)) => plan.push((to, Ok(v.clone()))),
                    Some(Entry::Formula(fid)) => {
                        let f = self.formula(*fid);
                        let (dr, dc) = (
                            to.row as i64 - f.addr.row as i64,
                            to.col as i64 - f.addr.col as i64,
                        );
                        let root = map_refs(&f.formula.root, &mut |sheet, cref| {
                            if matches!(sheet, SheetRef::Current | SheetRef::Own) {
                                Some(move_ref(cref, to))
                            } else {
                                Some(Some(retarget(cref, dr, dc)))
                            }
                        });
                        let text = print_node(self, &root, to);
                        sources.insert(to, (*fid, f.seq));
                        plan.push((
                            to,
                            Err((
                                Arc::new(Formula {
                                    root,
                                    volatile: f.formula.volatile,
                                    dynamic: f.formula.dynamic.clone(),
                                }),
                                text,
                                f.value_at(f.addr.row, f.addr.col),
                                f.array,
                            )),
                        ));
                    }
                }
            }
        }
        self.fmt_sort_pairs(&fmt_pairs);
        self.notes_sort_pairs(&fmt_pairs);
        self.hl_sort_pairs(&fmt_pairs);
        self.view_sorted(sheet, given);
        if !row_moves.is_empty() {
            self.view_move_rows(sheet, &row_moves);
        }
        if !col_moves.is_empty() {
            self.view_move_cols(sheet, &col_moves);
        }
        let mut report = RecalcReport::default();
        let mode = self.settings.mode;

        let used = self
            .sheet(sheet)
            .unwrap()
            .cells
            .sticky_bounds()
            .and_then(|u| u.intersection(&rect))
            .map_or(0, |i| i.rows() as u64 * i.cols() as u64);
        let any_cells = used >= 2;
        self.settings.mode = CalcMode::Manual;
        let epoch = self.calc.chain_epoch;
        let keep_walk = self.calc.walk.in_use();
        self.calc.hold_free = true;
        for (to, what) in plan {
            match what {
                Ok(v) => {
                    self.set_value_raw(to, v);
                }
                Err((formula, text, shown, array)) => {
                    self.set_compiled_formula(to, formula, text);

                    if let (Some(&(o, seq)), Some(n)) = (sources.get(&to), self.formula_at(to)) {
                        self.chain_set_seq(n, seq);
                        if o != n && self.calc.walk.tracking() {
                            self.calc.walk.renamed.push((o, n));
                        }
                    }
                    if let Some(fid) = self.formula_at(to) {
                        self.formula_mut(fid).value = shown;
                        if array == Some((1, 1)) {
                            self.formula_mut(fid).array = array;
                        }
                    }
                }
            }
            report.changed.push(to);
        }
        self.release_held_slots();
        self.settings.mode = mode;

        if keep_walk {
            self.calc.chain_epoch = epoch;
        }
        if any_cells {
            let r = self.recalc_point();
            report.evaluations = r.evaluations;
            report.circular = r.circular;
            report.changed.extend(r.changed);
        }
        Ok(calc::finish_report(report))
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    fn outlined() -> (Workbook, SheetId) {
        let mut wb = Workbook::new();
        let s = wb.sheet_at(0).id;
        for r in 0..8 {
            wb.set_value(CellAddr::new(s, r, 0), Value::Number(f64::from(r)));
        }
        for (r, l) in [(1, 2), (2, 2), (3, 1), (4, 2), (5, 2), (6, 1)] {
            wb.set_subtotal_levels(s, r, r, Some(l));
        }
        (wb, s)
    }

    #[test]
    fn scan_below_leaves_out_a_lone_grand_total() {
        let (wb, s) = outlined();
        let sc = wb.sort_outline_scan(s, Rect::new(1, 0, 7, 0));
        assert!(sc.differs);

        assert_eq!((sc.r0, sc.r1, sc.low), (1, 6, 1));
        assert!(!sc.subtotal);
    }

    #[test]
    fn scan_below_grows_up_over_deeper_rows() {
        let (wb, s) = outlined();
        let sc = wb.sort_outline_scan(s, Rect::new(2, 0, 6, 0));
        assert_eq!((sc.r0, sc.r1, sc.low), (1, 6, 1));
        let flat = wb.sort_outline_scan(s, Rect::new(4, 0, 5, 0));
        assert!(!flat.differs);
    }

    #[test]
    fn scan_above_grows_down_and_starts_at_a_key_row() {
        let (mut wb, s) = outlined();
        wb.set_outline_settings(s, true, false, false);
        let sc = wb.sort_outline_scan(s, Rect::new(2, 0, 4, 0));

        assert_eq!((sc.r0, sc.r1, sc.low), (4, 5, 2));
    }

    #[test]
    fn hidden_key_row_after_detail_is_refused() {
        let (mut wb, s) = outlined();
        assert!(!wb.sort_outline_refused(s, false, 1, 6, 1));
        wb.set_rows_hidden(s, 3, 1, true).unwrap();
        assert!(wb.sort_outline_refused(s, false, 1, 6, 1));
    }

    #[test]
    fn adjust_skips_hidden_ends_but_not_deep_ones_in_outline_mode() {
        let (mut wb, s) = outlined();
        wb.set_rows_hidden(s, 1, 1, true).unwrap();
        wb.set_rows_hidden(s, 6, 2, true).unwrap();
        let r = Rect::new(1, 0, 20, 0);
        assert_eq!(wb.adjust_sort_rows(s, r, None), Some(Rect::new(2, 0, 5, 0)));

        assert_eq!(
            wb.adjust_sort_rows(s, r, Some(1)),
            Some(Rect::new(1, 0, 5, 0))
        );
    }

    fn col_outlined() -> (Workbook, SheetId) {
        let mut wb = Workbook::new();
        let s = wb.sheet_at(0).id;
        for c in 0..8 {
            wb.set_value(CellAddr::new(s, 0, c), Value::Number(f64::from(c)));
        }
        wb.set_outline_levels(s, false, 1, 3, 2);
        (wb, s)
    }

    #[test]
    fn column_scan_reads_to_the_range_end_and_grows_over_deeper_columns() {
        let (mut wb, s) = col_outlined();

        let sc = wb.sort_outline_scan_cols(s, Rect::new(0, 0, 0, 7));
        assert!((sc.differs, sc.low, sc.r0, sc.r1) == (true, 0, 0, 7));

        let sc = wb.sort_outline_scan_cols(s, Rect::new(0, 1, 0, 3));
        assert!(!sc.differs && (sc.r0, sc.r1) == (1, 3));

        wb.set_outline_settings(s, false, true, false);
        let sc = wb.sort_outline_scan_cols(s, Rect::new(0, 0, 0, 2));
        assert!(sc.differs && (sc.r0, sc.r1) == (0, 3));
    }

    #[test]
    fn adjust_trims_hidden_columns_deeper_than_the_key_level_only_in_outline_mode() {
        let (mut wb, s) = col_outlined();
        wb.set_cols_hidden(s, 3, 1, true);
        wb.set_cols_hidden(s, 7, 1, true);
        let r = Rect::new(0, 0, 0, 7);
        assert_eq!(wb.adjust_sort_cols(s, r, None), Some(Rect::new(0, 0, 0, 6)));

        let r = Rect::new(0, 0, 0, 3);
        assert_eq!(
            wb.adjust_sort_cols(s, r, Some(0)),
            Some(Rect::new(0, 0, 0, 2))
        );
    }
}
