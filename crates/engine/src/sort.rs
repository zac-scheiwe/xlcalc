use std::cmp::Ordering;
use std::sync::Arc;

use crate::address::{CellAddr, Rect, SheetId, MAX_COLS, MAX_ROWS};
use crate::calc::{self, RecalcReport};
use crate::formula::{CRef, Formula, Rc, SheetRef};
use crate::formula_print::print_node;
use crate::functions::arrays::collate;
use crate::structure::map_refs;
use crate::value::Value;
use crate::workbook::{CalcMode, EditError, Entry, Workbook};

pub(crate) type MovedCell = Result<Value, (Arc<Formula>, String, Value, Option<(u32, u32)>)>;

#[derive(Clone, Copy, Debug)]
pub struct SortKey {
    pub index: u32,

    pub descending: bool,
}

#[derive(Clone, Debug, Default)]
pub struct SortSpec {
    pub keys: Vec<SortKey>,

    pub header: bool,

    pub by_columns: bool,

    pub match_case: bool,
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

fn cmp_key(a: &Value, b: &Value, desc: bool, match_case: bool) -> Ordering {
    let (ab, bb) = (rank(a) == 5, rank(b) == 5);
    if ab || bb {
        return ab.cmp(&bb);
    }
    let o = cmp_asc(a, b, match_case);
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
        let keyvals: Vec<Vec<Value>> = (0..n)
            .map(|i| {
                spec.keys
                    .iter()
                    .map(|k| at(skip + i, k.index))
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

    pub fn sort_table(&mut self, name: &str, spec: &SortSpec) -> Result<RecalcReport, EditError> {
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
        self.guard_filtered(sheet, rect, !spec.by_columns, spec.by_columns)?;

        if self
            .data_tables
            .iter()
            .any(|t| t.sheet == sheet && t.interior().intersects(&rect))
        {
            return Err(EditError::Compile(crate::datatable::PART_ERROR.into()));
        }
        if self.tables.iter().any(|t| t.sheet == sheet && t.totals) {
            self.snap_totals_before_sort(sheet, rect);
        }
        self.sort_range_impl(sheet, rect, spec)
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
        if self.merge_intersects(sheet, &rect) {
            return Err(EditError::OutOfScope("sort over merged cells"));
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
        let mut order = self.sort_order(sheet, rect, spec);
        if !spec.by_columns && !order.is_empty() {
            let r0 = rect.r0 + skip;
            self.outline_sort_guard(sheet, r0, r0 + order.len() as u32 - 1)?;
        }

        let hidden_line = |line: u32| {
            if spec.by_columns {
                self.col_hidden(sheet, rect.c0 + line)
            } else {
                self.row_hidden(sheet, rect.r0 + line)
            }
        };
        if (0..order.len() as u32).any(|j| hidden_line(skip + j)) {
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
                            if *sheet == SheetRef::Current {
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
        self.fmt_move_pairs(&fmt_pairs);
        let mut report = RecalcReport::default();
        let mode = self.settings.mode;
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
        if mode == CalcMode::Automatic {
            let r = calc::recalc(self);
            report.evaluations = r.evaluations;
            report.circular = r.circular;
            report.changed.extend(r.changed);
        }
        Ok(calc::finish_report(report))
    }
}
