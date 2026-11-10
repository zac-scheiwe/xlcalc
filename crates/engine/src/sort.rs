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
use crate::workbook::{CalcMode, EditError, Entry, Workbook};

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
                    .map(|k| keyval(skip + i, k))
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

    fn outline_sort_level(
        &self,
        sheet: SheetId,
        r0: u32,
        r1: u32,
    ) -> Result<Option<(u32, u32, u32)>, EditError> {
        let lv = |r: u32| self.row_level(sheet, r) - 1;

        let (mut low, mut at_low, mut differs) = (lv(r0), 1u32, false);
        for r in r0 + 1..=r1 {
            if low == 0 && at_low > 1 {
                break;
            }
            let l = lv(r);
            if l < low {
                low = l;
                at_low = 1;
                differs = true;
            } else if l == low {
                at_low += 1;
            } else {
                differs = true;
            }
        }
        if !differs {
            return Ok(None);
        }

        let mut last = r1;
        while last > r0 && lv(last) > low {
            last -= 1;
        }

        if last > r0 && at_low == 1 && lv(last - 1) == low + 1 {
            last -= 1;
            low += 1;
        }
        for r in r0 + 1..=last {
            if lv(r - 1) > low && lv(r) <= low && self.row_hidden(sheet, r) {
                return Err(EditError::Compile(
                    "Sorry, we can't sort because this list is filtered and grouped".into(),
                ));
            }
        }
        Ok(Some((r0, last, low)))
    }

    fn sort_drop_subtotals(
        &mut self,
        sheet: SheetId,
        rect: Rect,
        spec: &SortSpec,
        skip: u32,
    ) -> Result<(Rect, Option<u8>, bool), EditError> {
        let n = rect.rows().saturating_sub(skip);
        if spec.by_columns || n < 2 || rect.is_cell() {
            return Ok((rect, None, false));
        }
        let (r0, r1) = (rect.r0 + skip, rect.r1);
        if (r0..=r1).any(|r| self.row_hidden(sheet, r)) {
            return Ok((rect, None, false));
        }
        let Some((_, last, _)) = self.outline_sort_level(sheet, r0, r1)? else {
            return Ok((rect, None, false));
        };
        let row = Rect {
            r0: last,
            r1: last,
            ..rect
        };
        if self.subtotal_rows(sheet, row).is_empty() {
            return Ok((rect, None, false));
        }

        let header_level = (skip > 0)
            .then(|| self.row_outline_level(sheet, rect.r0).saturating_sub(1) as u8)
            .filter(|&l| l > 0);
        let removed = self.subtotal_rows(sheet, rect).len() as u32;

        let first = Rect { r0, r1: r0, ..rect };
        let start_moves = !self.subtotal_rows(sheet, first).is_empty();
        let end_row = Rect {
            r0: rect.r1,
            ..rect
        };
        let end_deleted = u32::from(!self.subtotal_rows(sheet, end_row).is_empty());
        self.remove_subtotal_run(sheet, rect)?;
        let rect = Rect {
            r0: rect.r0 + u32::from(start_moves),
            ..rect
        };

        let r1 = (rect.r1 + end_deleted).saturating_sub(removed).max(rect.r0);
        Ok((Rect { r1, ..rect }, header_level, true))
    }

    fn outline_order(
        &self,
        sheet: SheetId,
        base: u32,
        sorted: &[u32],
        first: u32,
        last: u32,
        top: u32,
    ) -> Vec<u32> {
        let (fl, ll) = (first - base, last - base);
        let mut rank = vec![0usize; sorted.len()];
        for (i, &s) in sorted.iter().enumerate() {
            rank[s as usize] = i;
        }
        let lv = |j: u32| self.row_level(sheet, base + j) - 1;
        let fixed = |j: u32| lv(j) <= top && self.row_hidden(sheet, base + j);
        let mut blocks: Vec<(usize, Vec<u32>)> = Vec::new();
        let mut pending: Vec<u32> = Vec::new();
        let mut slots: Vec<u32> = Vec::new();
        for j in fl..=ll {
            if fixed(j) {
                continue;
            }
            slots.push(j);
            pending.push(j);
            if lv(j) <= top {
                blocks.push((rank[j as usize], std::mem::take(&mut pending)));
            }
        }
        if let Some(&k) = pending.last() {
            blocks.push((rank[k as usize], pending));
        }
        blocks.sort_by_key(|b| b.0);
        let mut order: Vec<u32> = (0..sorted.len() as u32).collect();
        for (slot, src) in slots.into_iter().zip(blocks.into_iter().flat_map(|b| b.1)) {
            order[slot as usize] = src;
        }
        order
    }

    pub fn sort_table(&mut self, name: &str, spec: &SortSpec) -> Result<RecalcReport, EditError> {
        self.command(Command::Sort, |wb| wb.sort_table_run(name, spec))
    }

    fn sort_table_run(&mut self, name: &str, spec: &SortSpec) -> Result<RecalcReport, EditError> {
        let ti = self.table_position(name)?;
        let t = &self.tables[ti];
        let (sheet, mut rect) = (t.sheet, t.rect);
        if t.headers {
            rect.r0 += 1;
        }
        if t.totals_row().is_some() {
            rect.r1 -= 1;
        }
        let r = self.sort_range(sheet, rect, spec);
        if r.is_ok() {
            if let Ok(ti) = self.table_position(name) {
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
                if let Some(t) = touch {
                    wb.view_touch_row_pub(sheet, t);
                }
            }
            r
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
        self.guard_merges_sort(sheet, rect)?;
        let width = if spec.by_columns {
            rect.rows()
        } else {
            rect.cols()
        };
        if spec.keys.is_empty() || spec.keys.iter().any(|k| k.index >= width) {
            return Err(EditError::Compile("sort key is outside the range".into()));
        }
        let (_, skip) = Self::sort_lines(rect, spec);
        let (rect, header_level, dropped) = self.sort_drop_subtotals(sheet, rect, spec, skip)?;
        let mut order = self.sort_order(sheet, rect, spec);
        let mut skip = skip;
        if header_level.is_some() && !order.is_empty() {
            let m = order.len() as u32 + 1;
            let mut perm = vec![0; m as usize];
            perm[1] = 0;
            for (j, &o) in order.iter().enumerate() {
                let slot = if j == 0 { 0 } else { j + 1 };
                perm[slot] = 1 + o;
            }
            order = perm;
            skip = 0;
        }

        let hidden_line = |line: u32| {
            if spec.by_columns {
                self.col_hidden(sheet, rect.c0 + line)
            } else {
                self.row_hidden(sheet, rect.r0 + line)
            }
        };
        let outline = if spec.by_columns || order.is_empty() {
            None
        } else {
            self.outline_sort_level(
                sheet,
                rect.r0 + skip,
                rect.r0 + skip + order.len() as u32 - 1,
            )?
        };
        let mut row_moves: Vec<(u32, u32)> = Vec::new();
        if let Some((first, last, top)) = outline {
            order = self.outline_order(sheet, rect.r0 + skip, &order, first, last, top);
            row_moves = order
                .iter()
                .enumerate()
                .filter(|&(j, &s)| j as u32 != s)
                .map(|(j, &s)| (rect.r0 + skip + s, rect.r0 + skip + j as u32))
                .collect();
        } else if dropped {
            row_moves = order
                .iter()
                .enumerate()
                .filter(|&(j, &s)| j as u32 != s)
                .map(|(j, &s)| (rect.r0 + skip + s, rect.r0 + skip + j as u32))
                .collect();
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
        let mut fmt_pairs: Vec<(CellAddr, CellAddr)> = Vec::new();
        for (j, &src_line) in order.iter().enumerate() {
            let (dst_line, src_line) = (skip + j as u32, skip + src_line);
            if dst_line == src_line {
                continue;
            }
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
                        plan.push((
                            to,
                            Err((
                                Arc::new(Formula {
                                    root,
                                    volatile: f.formula.volatile,
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
        self.view_sorted(sheet, rect);
        if !row_moves.is_empty() {
            self.view_move_rows(sheet, &row_moves);
        }
        if let Some(l) = header_level.filter(|&l| l > 0) {
            self.set_subtotal_levels(sheet, rect.r0 + 1, rect.r0 + 1, Some(l));
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
        for (to, what) in plan {
            match what {
                Ok(v) => {
                    self.set_value_raw(to, v);
                }
                Err((formula, text, shown, array)) => {
                    self.set_compiled_formula(to, formula, text);
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
        self.settings.mode = mode;
        if any_cells {
            let r = self.recalc_point();
            report.evaluations = r.evaluations;
            report.circular = r.circular;
            report.changed.extend(r.changed);
        }
        Ok(calc::finish_report(report))
    }
}
