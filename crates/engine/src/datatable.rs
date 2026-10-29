use std::sync::{Arc, OnceLock};

use crate::address::{CellAddr, Rect, SheetId, MAX_COLS, MAX_ROWS};
use crate::calc::{self, RecalcReport};
use crate::formula::{CRef, Formula, Node, Rc, SheetRef};
use crate::formula_print::print_node;
use crate::functions::{self, FuncId};
use crate::value::Value;
use crate::workbook::{CalcMode, EditError, Entry, FormulaId, Op, Workbook};

pub const TABLE_FN: &str = "\u{1}TABLE";

pub const PART_ERROR: &str = "Can't change part of a data table.";

pub const INPUT_ERROR: &str = "Input cell reference is not valid.";

#[derive(Clone, Debug)]
pub struct DataTable {
    pub sheet: SheetId,

    pub rect: Rect,

    pub dirty: bool,
}

impl DataTable {
    pub fn interior(&self) -> Rect {
        Rect::new(
            self.rect.r0 + 1,
            self.rect.c0 + 1,
            self.rect.r1,
            self.rect.c1,
        )
    }

    pub fn contains_interior(&self, a: CellAddr) -> bool {
        a.sheet == self.sheet && self.interior().contains(a.row, a.col)
    }
}

fn table_id() -> FuncId {
    static ID: OnceLock<FuncId> = OnceLock::new();
    *ID.get_or_init(|| functions::lookup(TABLE_FN).expect("TABLE registered"))
}

pub fn is_table_formula(f: &Formula) -> bool {
    matches!(&f.root, Node::Func { id, .. } if *id == table_id())
}

fn rel(target: CellAddr, host: CellAddr) -> Node {
    Node::Ref(
        SheetRef::Current,
        CRef::Cell {
            row: Rc {
                v: target.row as i32 - host.row as i32,
                abs: false,
            },
            col: Rc {
                v: target.col as i32 - host.col as i32,
                abs: false,
            },
        },
    )
}

fn arg_cell(node: &Node, host: CellAddr) -> Option<CellAddr> {
    match node.unwrap_ws() {
        Node::Ref(SheetRef::Current, CRef::Cell { row, col }) => Some(CellAddr::new(
            host.sheet,
            row.resolve(host.row, MAX_ROWS, false)?,
            col.resolve(host.col, MAX_COLS, false)?,
        )),
        _ => None,
    }
}

type Inputs = [(bool, Option<CellAddr>); 2];

fn inputs(wb: &Workbook, t: &DataTable) -> Option<Inputs> {
    let int = t.interior();
    let host = CellAddr::new(t.sheet, int.r0, int.c0);
    let fid = wb.formula_at(host)?;
    let f = wb.formula(fid);
    let Node::Func { id, args } = &f.formula.root else {
        return None;
    };
    if *id != table_id() || args.len() < 2 {
        return None;
    }
    let one = |n: &Node| (!matches!(n, Node::Missing), arg_cell(n, host));
    Some([one(&args[0]), one(&args[1])])
}

impl Workbook {
    pub(crate) fn drop_sheet_data_tables(&mut self, sheet: SheetId) {
        self.data_tables.retain(|t| t.sheet != sheet);
    }

    pub(crate) fn copy_sheet_data_tables(&mut self, src: SheetId, new: SheetId) {
        let copies: Vec<DataTable> = self
            .data_tables
            .iter()
            .filter(|t| t.sheet == src)
            .map(|t| DataTable {
                sheet: new,
                ..t.clone()
            })
            .collect();
        self.data_tables.extend(copies);
    }

    pub(crate) fn move_data_tables(
        &mut self,
        moving: &[usize],
        doomed: &[usize],
        sheet: SheetId,
        dr: i64,
        dc: i64,
    ) {
        for &i in moving {
            let t = &mut self.data_tables[i];
            t.rect = Rect::new(
                (t.rect.r0 as i64 + dr) as u32,
                (t.rect.c0 as i64 + dc) as u32,
                (t.rect.r1 as i64 + dr) as u32,
                (t.rect.c1 as i64 + dc) as u32,
            );
            t.sheet = sheet;
        }
        let mut i = 0;
        self.data_tables.retain(|_| {
            i += 1;
            !doomed.contains(&(i - 1))
        });
    }

    pub fn add_data_table(
        &mut self,
        sheet: SheetId,
        rect: Rect,
        row_input: Option<CellAddr>,
        col_input: Option<CellAddr>,
    ) -> Result<RecalcReport, EditError> {
        if self.sheet(sheet).is_none() {
            return Err(EditError::NoSuchSheet);
        }
        if rect.rows() < 2 || rect.cols() < 2 {
            return Err(EditError::Compile("This selection isn't valid.".into()));
        }

        if rect.cell_count() > 1 << 20 {
            return Err(EditError::Compile("This selection isn't valid.".into()));
        }
        let both = row_input.is_some() && col_input.is_some();
        if row_input.is_none() && col_input.is_none() {
            return Err(EditError::Compile(INPUT_ERROR.into()));
        }
        for a in [row_input, col_input].into_iter().flatten() {
            let corner = a.row == rect.r0 && a.col == rect.c0;
            if a.sheet != sheet || (rect.contains(a.row, a.col) && (both || !corner)) {
                return Err(EditError::Compile(INPUT_ERROR.into()));
            }
        }

        let int = Rect::new(rect.r0 + 1, rect.c0 + 1, rect.r1, rect.c1);
        let inner: Vec<Rect> = self
            .array_blocks(sheet)
            .into_iter()
            .map(|(_, b)| b)
            .filter(|b| b.intersects(&int))
            .collect();
        if inner.iter().any(|b| b.intersection(&int) != Some(*b)) {
            return Err(crate::cse::part_of_array());
        }
        if self.merge_intersects(sheet, &int) {
            return Err(EditError::OutOfScope("data table over merged cells"));
        }

        let mut doomed = Vec::new();
        for (i, t) in self.data_tables.iter().enumerate() {
            let old = t.interior();
            if t.sheet != sheet || !old.intersects(&int) {
                continue;
            }
            if int.intersection(&old) == Some(old) {
                doomed.push(i);
            } else {
                return Err(EditError::Compile(PART_ERROR.into()));
            }
        }
        for i in doomed.into_iter().rev() {
            self.data_tables.remove(i);
        }
        for b in inner {
            self.clear_range(sheet, b)?;
        }
        self.data_tables.push(DataTable {
            sheet,
            rect,
            dirty: true,
        });
        for r in int.r0..=int.r1 {
            for c in int.c0..=int.c1 {
                let host = CellAddr::new(sheet, r, c);
                let at = |r, c| CellAddr::new(sheet, r, c);
                let input = |a: Option<CellAddr>| a.map_or(Node::Missing, |a| rel(a, host));
                let (model, h1, h2) = if both {
                    (at(rect.r0, rect.c0), at(rect.r0, c), Some(at(r, rect.c0)))
                } else if col_input.is_some() {
                    (at(rect.r0, c), at(r, rect.c0), None)
                } else {
                    (at(r, rect.c0), at(rect.r0, c), None)
                };
                let root = Node::Func {
                    id: table_id(),
                    args: vec![
                        input(row_input),
                        input(col_input),
                        rel(model, host),
                        rel(h1, host),
                        h2.map_or(Node::Missing, |h| rel(h, host)),
                    ],
                };
                let text = print_node(self, &root, host);
                self.set_compiled_formula(
                    host,
                    Arc::new(Formula {
                        root,
                        volatile: false,
                    }),
                    text,
                );
                self.data_tables.last_mut().unwrap().dirty = true;
            }
        }
        if self.settings.mode == CalcMode::Automatic {
            Ok(calc::recalc(self))
        } else {
            Ok(RecalcReport::default())
        }
    }

    pub(crate) fn data_table_ignores(&mut self, addr: CellAddr) -> bool {
        let Some(i) = self
            .data_tables
            .iter()
            .position(|t| t.contains_interior(addr))
        else {
            return false;
        };
        if self.data_tables[i].interior().cell_count() > 1 {
            return true;
        }
        self.data_tables.remove(i);
        false
    }

    pub(crate) fn data_tables_overwritten(
        &mut self,
        sheet: SheetId,
        rect: Rect,
    ) -> Result<(), EditError> {
        let mut doomed = Vec::new();
        for (i, t) in self.data_tables.iter().enumerate() {
            let int = t.interior();
            if t.sheet != sheet || !int.intersects(&rect) {
                continue;
            }
            if rect.intersection(&int) == Some(int) {
                doomed.push(i);
            } else {
                return Err(EditError::Compile(PART_ERROR.into()));
            }
        }
        for i in doomed.into_iter().rev() {
            self.data_tables.remove(i);
        }
        Ok(())
    }

    pub fn set_semiautomatic(&mut self) -> RecalcReport {
        self.settings.mode = CalcMode::Automatic;
        self.settings.except_tables = true;
        calc::recalc(self)
    }
}

pub(crate) fn table_cell_dirtied(wb: &mut Workbook, addr: CellAddr) {
    if wb.op_on(Op::DtBlock) {
        return;
    }
    for t in &mut wb.data_tables {
        if t.contains_interior(addr) {
            t.dirty = true;
        }
    }
}

fn reads_results(wb: &Workbook, t: &DataTable, u: &DataTable) -> bool {
    let target = u.interior();
    let mut seen: rustc_hash::FxHashSet<FormulaId> = Default::default();
    let mut work: Vec<FormulaId> = Vec::new();
    let start = Rect::new(t.rect.r0, t.rect.c0, t.rect.r1, t.rect.c1);
    let push_rect = |sheet: SheetId, rect: Rect, work: &mut Vec<FormulaId>| {
        if let Some(sh) = wb.sheet(sheet) {
            for (_, _, e) in sh.cells.iter_rect(rect) {
                if let Entry::Formula(f) = e {
                    work.push(*f);
                }
            }
        }
    };

    push_rect(
        t.sheet,
        Rect::new(start.r0, start.c0, start.r0, start.c1),
        &mut work,
    );
    push_rect(
        t.sheet,
        Rect::new(start.r0, start.c0, start.r1, start.c0),
        &mut work,
    );
    while let Some(f) = work.pop() {
        if !seen.insert(f) {
            continue;
        }
        let cell = wb.formula(f);
        if is_table_formula(&cell.formula) {
            continue;
        }
        for p in &cell.precedents {
            let (s, rect) = match p {
                crate::graph::Prec::Cell(s, r, c) => (*s, Rect::cell(*r, *c)),
                crate::graph::Prec::Range(s, rect) => (*s, *rect),
                _ => continue,
            };
            if s == u.sheet && rect.intersects(&target) {
                return true;
            }
            push_rect(s, rect, &mut work);
        }
    }
    false
}

pub(crate) fn table_pass(wb: &mut Workbook, report: &mut RecalcReport) {
    if !wb.data_tables.iter().any(|t| t.dirty) {
        return;
    }
    wb.with_op(Op::DtBlock, true, |wb| loop {
        let dirty: Vec<usize> = (0..wb.data_tables.len())
            .filter(|&i| wb.data_tables[i].dirty)
            .collect();
        let Some(&first) = dirty.first() else {
            break;
        };
        let pick = dirty
            .iter()
            .copied()
            .find(|&i| {
                !dirty
                    .iter()
                    .any(|&j| j != i && reads_results(wb, &wb.data_tables[i], &wb.data_tables[j]))
            })
            .unwrap_or(first);
        wb.data_tables[pick].dirty = false;
        let t = wb.data_tables[pick].clone();
        compute(wb, &t, report);
    });
}

fn put(wb: &mut Workbook, a: CellAddr, e: Option<Entry>) {
    let sh = wb.sheet_mut(a.sheet).unwrap();
    match e {
        Some(e) => {
            sh.cells.set(a.row, a.col, e);
        }
        None => {
            sh.cells.remove(a.row, a.col);
        }
    }
    calc::cell_changed(wb, a);
}

fn compute(wb: &mut Workbook, t: &DataTable, report: &mut RecalcReport) {
    let Some([(has_row, row_in), (has_col, col_in)]) = inputs(wb, t) else {
        return;
    };
    let int = t.interior();

    let spills = |a: Option<CellAddr>| {
        a.and_then(|a| wb.formula_at(a)).is_some_and(|fid| {
            wb.formula(fid).spill.is_some()
                || wb
                    .sheet(t.sheet)
                    .is_some_and(|sh| sh.spills.is_blocked(fid))
        })
    };
    if (has_row && row_in.is_none())
        || (has_col && col_in.is_none())
        || spills(row_in)
        || spills(col_in)
    {
        let v = Value::Error(crate::XlError::REF);
        for r in int.r0..=int.r1 {
            for c in int.c0..=int.c1 {
                let dst = CellAddr::new(t.sheet, r, c);
                if let Some(fid) = wb.formula_at(dst) {
                    if is_table_formula(&wb.formula(fid).formula) {
                        if !wb.formula(fid).value.same_as(&v) {
                            report.changed.push(dst);
                        }
                        debug_assert!(!crate::eval::contains_circ(&v));
                        wb.formula_mut(fid).value = v.clone();
                    }
                }
            }
        }
        calc::rect_changed(wb, t.sheet, int);
        let r = calc::recalc_cells(wb);
        report.evaluations += r.evaluations;
        report.changed.extend(r.changed);
        return;
    }
    let at = |r, c| CellAddr::new(t.sheet, r, c);

    type Unit = (Vec<(CellAddr, CellAddr)>, Vec<(CellAddr, CellAddr)>);
    let mut units: Vec<Unit> = Vec::new();
    let (r0, c0) = (t.rect.r0, t.rect.c0);
    match (row_in, col_in) {
        (Some(ri), Some(ci)) => {
            for r in int.r0..=int.r1 {
                for c in int.c0..=int.c1 {
                    units.push((
                        vec![(ri, at(r0, c)), (ci, at(r, c0))],
                        vec![(at(r, c), at(r0, c0))],
                    ));
                }
            }
        }
        (None, Some(ci)) => {
            for r in int.r0..=int.r1 {
                units.push((
                    vec![(ci, at(r, c0))],
                    (int.c0..=int.c1).map(|c| (at(r, c), at(r0, c))).collect(),
                ));
            }
        }
        (Some(ri), None) => {
            for c in int.c0..=int.c1 {
                units.push((
                    vec![(ri, at(r0, c))],
                    (int.r0..=int.r1).map(|r| (at(r, c), at(r, c0))).collect(),
                ));
            }
        }
        (None, None) => return,
    }
    let mut saved: Vec<(CellAddr, Option<Entry>)> = Vec::new();
    for a in [row_in, col_in].into_iter().flatten() {
        if !saved.iter().any(|(s, _)| *s == a) {
            let e = wb
                .sheet(a.sheet)
                .and_then(|s| s.cells.get(a.row, a.col).cloned());
            saved.push((a, e));
        }
    }
    let absorb = |report: &mut RecalcReport, r: RecalcReport| {
        report.evaluations += r.evaluations;
        report.changed.extend(r.changed);
    };
    for (subs, writes) in units {
        let vals: Vec<(CellAddr, Value)> = subs.iter().map(|&(i, h)| (i, wb.value(h))).collect();
        for (i, v) in vals {
            put(wb, i, (!v.is_blank()).then_some(Entry::Const(v)));
        }
        let r = calc::recalc_cells(wb);
        absorb(report, r);
        let results: Vec<(CellAddr, Value)> = writes
            .iter()
            .map(|&(dst, m)| {
                let v = wb.value(m);
                (dst, if v.is_blank() { Value::Number(0.0) } else { v })
            })
            .collect();
        for (dst, v) in results {
            let Some(fid) = wb.formula_at(dst) else {
                continue;
            };
            if !is_table_formula(&wb.formula(fid).formula) {
                continue;
            }
            if !wb.formula(fid).value.same_as(&v) {
                report.changed.push(dst);
            }
            debug_assert!(!crate::eval::contains_circ(&v));
            wb.formula_mut(fid).value = v;
        }
    }
    for (a, e) in saved {
        put(wb, a, e);
    }
    calc::rect_changed(wb, t.sheet, int);
    let r = calc::recalc_cells(wb);
    absorb(report, r);
}
