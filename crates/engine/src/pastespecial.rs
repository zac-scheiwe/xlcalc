use std::sync::Arc;

use crate::address::{CellAddr, Rect, SheetId, MAX_COLS, MAX_ROWS};
use crate::calc::{self, RecalcReport};
use crate::coerce;
use crate::formula::{CRef, Formula, Node, Rc, SheetRef};
use crate::formula_print::print_node;
use crate::parser::ast::{BinOp, UnaryOp};
use crate::value::{Value, XlError};
use crate::workbook::{CalcMode, EditError, Entry, Workbook};

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum PasteWhat {
    All,

    Values,

    Formulas,

    Formats,
}

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum PasteOp {
    None,

    Add,

    Sub,

    Mul,

    Div,
}

#[derive(Clone, Copy, Debug)]
pub struct PasteSpecial {
    pub what: PasteWhat,

    pub op: PasteOp,

    pub skip_blanks: bool,

    pub transpose: bool,
}

impl Default for PasteSpecial {
    fn default() -> Self {
        PasteSpecial {
            what: PasteWhat::All,
            op: PasteOp::None,
            skip_blanks: false,
            transpose: false,
        }
    }
}

enum Act {
    Skip,
    Set(Value),

    SetCalc(Value),
    Formula(Formula, Option<Value>),
}

fn pos(x: Rc, host: u32) -> i64 {
    if x.abs {
        x.v as i64
    } else {
        host as i64 + x.v as i64
    }
}

fn retarget(cref: &CRef, dr: i64, dc: i64) -> CRef {
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

fn transpose_corner(row: Rc, col: Rc, from: CellAddr, to: CellAddr) -> (Rc, Rc) {
    let (off_r, off_c) = (
        pos(row, from.row) - from.row as i64,
        pos(col, from.col) - from.col as i64,
    );

    let new_row = if col.abs {
        Rc {
            v: (to.row as i64 + off_c) as i32,
            abs: true,
        }
    } else {
        Rc {
            v: off_c as i32,
            abs: false,
        }
    };
    let new_col = if row.abs {
        Rc {
            v: (to.col as i64 + off_r) as i32,
            abs: true,
        }
    } else {
        Rc {
            v: off_r as i32,
            abs: false,
        }
    };
    (new_row, new_col)
}

fn on_grid(r: Rc, host: u32, max: u32) -> bool {
    (0..max as i64).contains(&pos(r, host))
}

fn clip(r: Rc, host: u32, max: u32) -> Rc {
    let p = pos(r, host).clamp(0, max as i64 - 1);
    if r.abs {
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
}

fn transpose_ref(
    cref: &CRef,
    sheet: &SheetRef,
    from: CellAddr,
    to: CellAddr,
    src: Rect,
) -> Option<CRef> {
    let (dr, dc) = (
        to.row as i64 - from.row as i64,
        to.col as i64 - from.col as i64,
    );
    let area = |r0: Rc, c0: Rc, r1: Rc, c1: Rc| -> Option<CRef> {
        let (nr0, nc0) = transpose_corner(r0, c0, from, to);
        let (nr1, nc1) = transpose_corner(r1, c1, from, to);
        let ok = [nr0, nr1].iter().all(|r| on_grid(*r, to.row, MAX_ROWS))
            && [nc0, nc1].iter().all(|c| on_grid(*c, to.col, MAX_COLS));
        ok.then_some(CRef::Area {
            r0: nr0,
            c0: nc0,
            r1: nr1,
            c1: nc1,
        })
    };

    let in_block = || -> bool {
        let same = match sheet {
            SheetRef::Current => true,
            SheetRef::Sheet(id) => *id == from.sheet,
            _ => false,
        };
        let (r, c) = match *cref {
            CRef::Rows { r0, r1 } => (pos(r0, from.row).min(pos(r1, from.row)), 0),
            CRef::Cols { c0, c1 } => (0, pos(c0, from.col).min(pos(c1, from.col))),
            _ => return false,
        };
        same && r >= 0 && c >= 0 && src.contains(r as u32, c as u32)
    };
    match *cref {
        CRef::Cell { row, col } => {
            if row.abs || col.abs {
                Some(retarget(cref, dr, dc))
            } else {
                let (nr, nc) = transpose_corner(row, col, from, to);
                (on_grid(nr, to.row, MAX_ROWS) && on_grid(nc, to.col, MAX_COLS))
                    .then_some(CRef::Cell { row: nr, col: nc })
            }
        }
        CRef::Area { r0, c0, r1, c1 } => {
            if (r0.abs || c0.abs) && (r1.abs || c1.abs) {
                return Some(retarget(cref, dr, dc));
            }
            area(r0, c0, r1, c1)
        }
        CRef::Cols { c0, c1 } => {
            if (c0.abs || c1.abs) && !in_block() {
                return Some(retarget(cref, dr, dc));
            }
            let abs = |v: u32| Rc {
                v: v as i32,
                abs: true,
            };
            let (nr0, nc0) = transpose_corner(abs(0), c0, from, to);
            let (nr1, nc1) = transpose_corner(abs(MAX_ROWS - 1), c1, from, to);

            let ok = [nr0, nr1].iter().all(|r| on_grid(*r, to.row, MAX_ROWS))
                && on_grid(nc0, to.col, MAX_COLS);
            ok.then(|| CRef::Area {
                r0: nr0,
                c0: nc0,
                r1: nr1,
                c1: clip(nc1, to.col, MAX_COLS),
            })
        }
        CRef::Rows { r0, r1 } => {
            if (r0.abs || r1.abs) && !in_block() {
                return Some(retarget(cref, dr, dc));
            }
            let abs = |v: u32| Rc {
                v: v as i32,
                abs: true,
            };
            let (nr0, nc0) = transpose_corner(r0, abs(0), from, to);
            let (_, nc1) = transpose_corner(r1, abs(MAX_COLS - 1), from, to);
            let ok = [nc0, nc1].iter().all(|c| on_grid(*c, to.col, MAX_COLS))
                && on_grid(nr0, to.row, MAX_ROWS);
            ok.then(|| CRef::Area {
                r0: nr0,
                c0: nc0,

                r1: Rc {
                    v: MAX_ROWS as i32 - 1,
                    abs: true,
                },
                c1: nc1,
            })
        }
    }
}

impl Workbook {
    pub(crate) fn moved_formula(&self, fid: u32, to: CellAddr, transpose: Option<Rect>) -> Formula {
        let f = self.formula(fid);
        let from = f.addr;
        let root = crate::structure::edit_refs(&f.formula.root, &mut |sh, cref| {
            if let Some(src) = transpose {
                match transpose_ref(cref, sh, from, to, src) {
                    Some(c) => crate::structure::RefEdit::Set(sh.clone(), c),
                    None => crate::structure::RefEdit::Dead(sh.clone()),
                }
            } else {
                self.copy_ref_edit(sh, cref, from, to)
            }
        });
        let root = if transpose.is_some() {
            root
        } else {
            self.copy_struct_cols(&root, from, to, true)
        };
        Formula {
            root,
            volatile: f.formula.volatile,
        }
    }

    pub fn paste_special(
        &mut self,
        src_sheet: SheetId,
        src: Rect,
        dst_sheet: SheetId,
        dst: Rect,
        spec: &PasteSpecial,
    ) -> Result<RecalcReport, EditError> {
        self.paste_special_body(src_sheet, src, dst_sheet, dst, spec)
    }

    fn paste_special_body(
        &mut self,
        src_sheet: SheetId,
        src: Rect,
        dst_sheet: SheetId,
        dst: Rect,
        spec: &PasteSpecial,
    ) -> Result<RecalcReport, EditError> {
        self.guard_filtered(src_sheet, src, true, true)?;
        let reach = Rect::new(
            dst.r0,
            dst.c0,
            dst.r1.max(dst.r0.saturating_add(src.rows() - 1)),
            dst.c1.max(dst.c0.saturating_add(src.cols() - 1)),
        );
        self.guard_filtered(dst_sheet, reach, true, true)?;
        if self.merge_intersects(src_sheet, &src) || self.merge_intersects(dst_sheet, &reach) {
            return Err(EditError::OutOfScope("paste special over merged cells"));
        }
        let pending = self.totals_pending(src_sheet, src) || self.totals_pending(dst_sheet, dst);
        let r = self.with_paste_block((dst_sheet, dst), |this| {
            this.paste_special_one(src_sheet, src, dst_sheet, dst, spec)
        });
        if r.is_ok()
            && matches!(
                spec.what,
                PasteWhat::All | PasteWhat::Formulas | PasteWhat::Values
            )
        {
            let (lr, lc) = if spec.transpose {
                (src.cols(), src.rows())
            } else {
                (src.rows(), src.cols())
            };
            let reach = Rect::new(
                dst.r0,
                dst.c0,
                dst.r1.max(dst.r0 + lr - 1),
                dst.c1.max(dst.c0 + lc - 1),
            );
            self.paste_ends_calc(dst_sheet, reach);
        }
        if pending {
            self.restore_totals(src_sheet, src, None);
            self.restore_totals(dst_sheet, dst, Some(dst));
        }
        r
    }

    pub(crate) fn paste_special_one(
        &mut self,
        src_sheet: SheetId,
        src: Rect,
        dst_sheet: SheetId,
        dst: Rect,
        spec: &PasteSpecial,
    ) -> Result<RecalcReport, EditError> {
        if matches!(spec.what, PasteWhat::All | PasteWhat::Formulas) {
            self.names_follow(src_sheet, src, dst_sheet, true);
        }
        self.paste_special_impl(src_sheet, src, dst_sheet, dst, spec)
    }

    fn paste_special_impl(
        &mut self,
        src_sheet: SheetId,
        src: Rect,
        dst_sheet: SheetId,
        dst: Rect,
        spec: &PasteSpecial,
    ) -> Result<RecalcReport, EditError> {
        self.sheet(src_sheet).ok_or(EditError::NoSuchSheet)?;
        self.sheet(dst_sheet).ok_or(EditError::NoSuchSheet)?;
        let (lr, lc) = if spec.transpose {
            (src.cols(), src.rows())
        } else {
            (src.rows(), src.cols())
        };

        let ok_r = dst.rows().is_multiple_of(lr) || dst.rows() == 1;
        let ok_c = dst.cols().is_multiple_of(lc) || dst.cols() == 1;
        let (reps_r, reps_c) = if ok_r && ok_c {
            ((dst.rows() / lr).max(1), (dst.cols() / lc).max(1))
        } else {
            (1, 1)
        };
        if dst.r0 as u64 + (reps_r * lr) as u64 > MAX_ROWS as u64
            || dst.c0 as u64 + (reps_c * lc) as u64 > MAX_COLS as u64
        {
            return Err(EditError::Compile(
                "paste area does not fit on the sheet".into(),
            ));
        }

        if (reps_r as u64 * lr as u64) * (reps_c as u64 * lc as u64)
            > crate::workbook::MAX_ARRAY_CELLS
            && spec.what != PasteWhat::Formats
        {
            return Err(EditError::Compile("The paste area is too large.".into()));
        }
        let only_formats = spec.what == PasteWhat::Formats;

        if spec.transpose
            && src_sheet == dst_sheet
            && dst.r0 <= src.r1
            && src.r0
                < dst.r0
                    + if only_formats {
                        if dst.is_cell() {
                            lr
                        } else {
                            dst.rows()
                        }
                    } else {
                        reps_r * lr
                    }
            && dst.c0 <= src.c1
            && src.c0
                < dst.c0
                    + if only_formats {
                        if dst.is_cell() {
                            lc
                        } else {
                            dst.cols()
                        }
                    } else {
                        reps_c * lc
                    }
        {
            return Err(EditError::Compile(
                "This selection isn't valid. Make sure the copy and paste areas don't overlap unless they are the same size and shape."
                    .into(),
            ));
        }

        let tile_is_src = |a0: u32, s0: u32, l: u32, reps: u32| {
            s0 >= a0 && (s0 - a0).is_multiple_of(l) && (s0 - a0) / l < reps
        };
        if !spec.transpose
            && reps_r * reps_c > 1
            && src_sheet == dst_sheet
            && src.intersects(&Rect::new(
                dst.r0,
                dst.c0,
                dst.r0 + reps_r * lr - 1,
                dst.c0 + reps_c * lc - 1,
            ))
            && !(tile_is_src(dst.r0, src.r0, lr, reps_r) && tile_is_src(dst.c0, src.c0, lc, reps_c))
        {
            return Err(EditError::Compile(
                "This selection isn't valid. Make sure the copy and paste areas don't overlap unless they are the same size and shape."
                    .into(),
            ));
        }

        let target = Rect::new(
            dst.r0,
            dst.c0,
            dst.r0 + reps_r * lr - 1,
            dst.c0 + reps_c * lc - 1,
        );

        if spec.op != PasteOp::None && spec.what != PasteWhat::Formats {
            let hit = self.formula_ids().any(|f| {
                let c = self.formula(f);
                c.addr.sheet == dst_sheet
                    && (c
                        .spill
                        .is_some_and(|s| !s.is_cell() && s.intersects(&target))
                        || (target.contains(c.addr.row, c.addr.col)
                            && self
                                .sheet(dst_sheet)
                                .is_some_and(|s| s.spills.is_blocked(f))))
            });
            if hit {
                return Err(crate::cse::part_of_array());
            }
        }
        let orig_tables: Vec<Rect> = self.tables.iter().map(|t| t.rect).collect();
        let arrays = self.paste_special_arrays(src_sheet, src, dst_sheet, target, spec)?;
        let mut plan: Vec<(CellAddr, Act)> = Vec::new();
        let mut fmt_pairs: Vec<(CellAddr, CellAddr)> = Vec::new();
        for rr in 0..reps_r {
            for rc in 0..reps_c {
                for i in 0..lr {
                    for j in 0..lc {
                        let (sr, sc) = if spec.transpose {
                            (src.r0 + j, src.c0 + i)
                        } else {
                            (src.r0 + i, src.c0 + j)
                        };
                        let to =
                            CellAddr::new(dst_sheet, dst.r0 + rr * lr + i, dst.c0 + rc * lc + j);
                        let from = CellAddr::new(src_sheet, sr, sc);
                        let act = self.paste_cell(from, to, spec, src);

                        let skipped = spec.skip_blanks && self.src_blank(from);
                        if spec.what != PasteWhat::Values
                            && spec.what != PasteWhat::Formulas
                            && !skipped
                        {
                            fmt_pairs.push((from, to));
                        }
                        plan.push((to, act));
                    }
                }
            }
        }
        if spec.what == PasteWhat::Formats && (dst.rows() > 1 || dst.cols() > 1) {
            let cells = dst.rows() as u64 * dst.cols() as u64;
            if cells <= (1 << 20) {
                fmt_pairs.clear();
                for r in 0..dst.rows() {
                    for c in 0..dst.cols() {
                        let (i, j) = (r % lr, c % lc);
                        let (sr, sc) = if spec.transpose {
                            (src.r0 + j, src.c0 + i)
                        } else {
                            (src.r0 + i, src.c0 + j)
                        };
                        let from = CellAddr::new(src_sheet, sr, sc);
                        if spec.skip_blanks && self.src_blank(from) {
                            continue;
                        }
                        fmt_pairs.push((from, CellAddr::new(dst_sheet, dst.r0 + r, dst.c0 + c)));
                    }
                }
            }
        }
        self.fmt_copy_pairs(&fmt_pairs);
        let mut report = RecalcReport::default();
        let mode = self.settings.mode;
        self.settings.mode = CalcMode::Manual;
        for (to, act) in plan {
            match act {
                Act::Skip => continue,
                Act::SetCalc(v) => {
                    self.set_value(to, v);
                }
                Act::Set(v) => {
                    if spec.op == PasteOp::None {
                        self.set_value_exact(to, v);
                    } else {
                        self.set_value(to, v);
                    }
                }
                Act::Formula(formula, shown) => {
                    let text = print_node(self, &formula.root, to);
                    self.set_compiled_formula(to, Arc::new(formula), text);
                    if let (Some(fid), Some(shown)) = (self.formula_at(to), shown) {
                        self.formula_mut(fid).value = shown;
                    }
                }
            }
            report.changed.push(to);
        }
        self.paste_arrays(arrays);
        if self
            .tables
            .iter()
            .map(|t| t.rect)
            .ne(orig_tables.iter().copied())
        {
            self.fmt_copy_pairs(&fmt_pairs);
        }
        self.settings.mode = mode;
        if mode == CalcMode::Automatic {
            let r = calc::recalc(self);
            report.evaluations = r.evaluations;
            report.circular = r.circular;
            report.changed.extend(r.changed);
        }

        let headers: Vec<CellAddr> = report
            .changed
            .iter()
            .copied()
            .filter(|a| {
                self.tables.iter().any(|t| {
                    t.sheet == a.sheet
                        && t.header_row() == Some(a.row)
                        && (t.rect.c0..=t.rect.c1).contains(&a.col)
                }) && self.formula_at(*a).is_some()
            })
            .collect();
        for a in headers {
            let v = self.value(a);
            let text = self.header_text(a, &v);
            self.set_value(a, Value::from(text.as_str()));
        }

        if spec.what != PasteWhat::Formats {
            if let Some((ti, rect, rows, edge)) = self.paste_growth(
                dst_sheet,
                target,
                None,
                Some(&orig_tables),
                Some((src_sheet, src)),
                true,
            ) {
                let info = crate::moves::PasteInfo {
                    block: target,
                    carries_fmt: matches!(spec.what, PasteWhat::All | PasteWhat::Formats),
                    protect_blanks: !edge,
                };
                self.grow_table_by_paste(ti, rect, rows, Some(info));
            }
        }
        if spec.what == PasteWhat::All
            && spec.op == PasteOp::None
            && !spec.skip_blanks
            && !spec.transpose
        {
            self.paste_copies_table(src_sheet, src, dst_sheet, target);
        }
        Ok(calc::finish_report(report))
    }

    fn src_blank(&self, from: CellAddr) -> bool {
        self.sheet(from.sheet)
            .is_none_or(|s| s.cells.get(from.row, from.col).is_none())
    }

    fn paste_cell(&self, from: CellAddr, to: CellAddr, spec: &PasteSpecial, src: Rect) -> Act {
        let entry = self
            .sheet(from.sheet)
            .and_then(|s| s.cells.get(from.row, from.col))
            .cloned();
        if spec.what == PasteWhat::Formats {
            return Act::Skip;
        }
        let values_only = spec.what == PasteWhat::Values;

        let value = || self.value(from);
        let blank = match &entry {
            None => !values_only || matches!(value(), Value::Blank),
            Some(_) => false,
        };
        if blank && spec.skip_blanks {
            return Act::Skip;
        }
        if spec.op == PasteOp::None {
            return match entry {
                _ if blank => Act::Set(Value::Blank),
                Some(Entry::Formula(_)) if values_only => Act::SetCalc(value()),
                _ if values_only => Act::Set(value()),
                Some(Entry::Const(v)) => Act::Set(v),
                Some(Entry::Formula(fid)) => {
                    let shown = self.formula(fid).value_at(from.row, from.col);
                    Act::Formula(
                        self.moved_formula(fid, to, spec.transpose.then_some(src)),
                        Some(shown),
                    )
                }
                None => Act::Set(Value::Blank),
            };
        }
        let env = self.input_env();
        let num = |v: &Value| -> Option<f64> {
            match v {
                Value::Number(n) => Some(*n),
                Value::Text(_) => coerce::to_number(v, env).ok(),
                _ => None,
            }
        };

        let dest_is_formula = matches!(
            self.sheet(to.sheet)
                .and_then(|s| s.cells.get(to.row, to.col)),
            Some(Entry::Formula(_))
        );

        enum Operand {
            Lit(f64),
            Formula(Formula),
        }
        let operand = if blank {
            Operand::Lit(0.0)
        } else {
            match &entry {
                Some(Entry::Formula(fid)) if !values_only => Operand::Formula(self.moved_formula(
                    *fid,
                    to,
                    (spec.transpose && !dest_is_formula).then_some(src),
                )),
                _ => {
                    let v = match &entry {
                        Some(Entry::Const(v)) if !values_only => v.clone(),
                        _ => value(),
                    };
                    match num(&v) {
                        Some(n) => Operand::Lit(n),
                        None => return Act::Skip,
                    }
                }
            }
        };
        enum Dest {
            Num(f64),
            Formula(Formula),
        }
        let dest_entry = self
            .sheet(to.sheet)
            .and_then(|s| s.cells.get(to.row, to.col))
            .cloned();
        let dest = match dest_entry {
            None => {
                if blank {
                    return Act::Skip;
                }
                Dest::Num(0.0)
            }
            Some(Entry::Const(v)) => match num(&v) {
                Some(n) => Dest::Num(n),
                None => return Act::Skip,
            },
            Some(Entry::Formula(fid)) => {
                let f = self.formula(fid);
                Dest::Formula(Formula {
                    root: f.formula.root.clone(),
                    volatile: f.formula.volatile,
                })
            }
        };
        let op = match spec.op {
            PasteOp::Add => BinOp::Add,
            PasteOp::Sub => BinOp::Sub,
            PasteOp::Mul => BinOp::Mul,
            _ => BinOp::Div,
        };
        let dest_formula = matches!(dest, Dest::Formula(_));

        let lit_operand = matches!(operand, Operand::Lit(_));
        match (dest, operand) {
            (Dest::Num(n), Operand::Lit(s)) => Act::Set(match spec.op {
                PasteOp::Add => finite(n + s),
                PasteOp::Sub => finite(n - s),
                PasteOp::Mul => finite(n * s),
                _ if s == 0.0 => Value::Error(XlError::DIV0),
                _ => finite(n / s),
            }),
            (dest, operand) => {
                let (left, lv) = match dest {
                    Dest::Num(n) => (Node::Lit(Value::Number(n)), false),
                    Dest::Formula(f) => (Node::Paren(Box::new(f.root)), f.volatile),
                };
                let (right, rv) = match operand {
                    Operand::Lit(s) => (Node::Lit(Value::Number(s)), false),
                    Operand::Formula(f) => (Node::Paren(Box::new(f.root)), f.volatile),
                };
                let mut formula = Formula {
                    root: Node::Binary(op, Box::new(left), Box::new(right)),
                    volatile: lv || rv,
                };
                if dest_formula && lit_operand {
                } else if dest_formula {
                    let text = print_node(self, &formula.root, to);
                    if let Some((f, _)) = crate::legacy::legacy_quiet_expr(&text)
                        .and_then(|e| self.compile_expr_normalized(&e, to).ok())
                    {
                        formula = f;
                    }
                } else {
                    formula.root = Node::Unary(UnaryOp::ArrayZero, Box::new(formula.root));
                }
                Act::Formula(
                    formula,
                    Some(if dest_is_formula {
                        self.value(to)
                    } else {
                        Value::Number(0.0)
                    }),
                )
            }
        }
    }
}

fn finite(x: f64) -> Value {
    if x.is_finite() {
        Value::Number(x)
    } else {
        Value::Error(XlError::NUM)
    }
}
