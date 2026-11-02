use std::sync::Arc;

use crate::address::CellAddr;
use crate::calc::{self, RecalcReport};
use crate::formula::Formula;
use crate::value::Value;

use super::{CalcMode, EditError, Entry, Op, Workbook};

pub(crate) struct Edit {
    pub addr: CellAddr,
    pub kind: EditKind,

    pub replace_entry: bool,
}

pub(crate) enum EditKind {
    Value(Value),

    Formula(String),

    Clear,
}

enum Checked {
    Go,

    Ignore,
}

enum Plan {
    Value(Value),
    Formula(Arc<Formula>, String),
    Clear,
}

impl Edit {
    pub fn value(wb: &Workbook, addr: CellAddr, v: Value) -> Edit {
        let kind = if v.is_blank() {
            EditKind::Clear
        } else {
            EditKind::Value(v)
        };
        Edit::new(wb, addr, kind)
    }

    pub fn formula(wb: &Workbook, addr: CellAddr, text: &str) -> Edit {
        Edit::new(wb, addr, EditKind::Formula(text.to_string()))
    }

    pub fn clear(wb: &Workbook, addr: CellAddr) -> Edit {
        Edit::new(wb, addr, EditKind::Clear)
    }

    fn new(wb: &Workbook, addr: CellAddr, kind: EditKind) -> Edit {
        Edit {
            addr,
            kind,
            replace_entry: wb.op_on(Op::ReplaceEntry),
        }
    }
}

impl Workbook {
    pub(crate) fn run_edit(&mut self, edit: Edit) -> Result<RecalcReport, EditError> {
        if let Checked::Ignore = self.edit_check(&edit)? {
            return Ok(RecalcReport::default());
        }
        let plan = self.edit_plan(&edit)?;

        if let Some(sh) = self.sheet_mut(edit.addr.sheet) {
            sh.spills.touch();
        }
        let before: Vec<crate::address::Rect> = self.tables.iter().map(|t| t.rect).collect();
        let cleared = matches!(edit.kind, EditKind::Clear);
        self.edit_apply(edit.addr, plan);
        if cleared {
            self.sbr_unknown();
        } else {
            self.sbr_edit(edit.addr, &before);
        }
        Ok(self.edit_after(&edit))
    }

    fn edit_check(&mut self, edit: &Edit) -> Result<Checked, EditError> {
        let addr = edit.addr;
        match edit.kind {
            EditKind::Value(_) => {
                if self.sheet(addr.sheet).is_none()
                    || self.in_multi_cell_array(addr)
                    || self.merged_hidden(addr)
                    || self.data_table_ignores(addr)
                {
                    return Ok(Checked::Ignore);
                }
            }
            EditKind::Formula(_) => {
                if self.sheet(addr.sheet).is_none() {
                    return Err(EditError::NoSuchSheet);
                }
                if self.data_table_ignores(addr) {
                    return Ok(Checked::Ignore);
                }
                if self.in_multi_cell_array(addr) || self.merged_hidden(addr) {
                    return Err(EditError::Locked);
                }
            }
            EditKind::Clear => {
                if !self.clear_allowed(addr) {
                    return Ok(Checked::Ignore);
                }
            }
        }
        Ok(Checked::Go)
    }

    fn clear_allowed(&mut self, addr: CellAddr) -> bool {
        self.sheet(addr.sheet).is_some()
            && !self.data_table_ignores(addr)
            && !self.in_multi_cell_array(addr)
    }

    fn edit_plan(&mut self, edit: &Edit) -> Result<Plan, EditError> {
        let addr = edit.addr;
        Ok(match &edit.kind {
            EditKind::Value(v) => {
                let v = match v {
                    Value::Number(x) if !x.is_finite() => Value::Error(crate::value::XlError::NUM),
                    v => v.clone(),
                };
                Plan::Value(self.precision_const(addr, v))
            }
            EditKind::Formula(text) => {
                let (formula, body) = self.plan_formula(addr, text)?;
                Plan::Formula(formula, body)
            }
            EditKind::Clear => Plan::Clear,
        })
    }

    fn plan_formula(
        &mut self,
        addr: CellAddr,
        text: &str,
    ) -> Result<(Arc<Formula>, String), EditError> {
        let body = crate::input::auto_close_call(text.strip_prefix('=').unwrap_or(text));
        let body = body.as_ref();

        if body.starts_with('=') {
            return Err(EditError::Parse(crate::parser::ParseError {
                pos: 0,
                message: "unexpected '='".into(),
            }));
        }

        let widened = self
            .table_expansion(addr, true)
            .map(|(ti, rect, _)| (ti, std::mem::replace(&mut self.tables[ti].rect, rect)));
        let compiled = self.compile_normalized(body, addr);
        if let Some((ti, old)) = widened {
            self.tables[ti].rect = old;
        }
        if let Err(EditError::Parse(e)) = &compiled {
            self.note_rejected_names(body, e.pos, addr);
        }
        let (formula, body) = compiled?;

        let formula = match crate::legacy::scalar_entry_expr(self, &body, addr)
            .and_then(|e| self.compile_expr_normalized(&e, addr).ok())
        {
            Some((f, _)) => f,
            None => formula,
        };
        self.note_placeholders(&formula, addr);
        Ok((Arc::new(formula), body))
    }

    fn edit_apply(&mut self, addr: CellAddr, plan: Plan) {
        let refill = self.tables.iter().position(|t| {
            t.emptied && t.sheet == addr.sheet && t.rect.contains(addr.row, addr.col)
        });
        if let Some(ti) = refill {
            self.tables[ti].emptied = false;
        }

        if self.calc.stale_tables && matches!(plan, Plan::Value(_)) {
            let names: Vec<String> = self
                .tables
                .iter()
                .filter(|t| {
                    t.emptied
                        && t.sheet == addr.sheet
                        && addr.row == t.rect.r1 + 1
                        && (t.rect.c0..=t.rect.c1).contains(&addr.col)
                })
                .map(|t| t.name.clone())
                .collect();
            self.refresh_tables_named(names);
        }
        match plan {
            Plan::Value(v) => {
                self.remove_cell(addr);
                self.sheet_mut(addr.sheet)
                    .unwrap()
                    .cells
                    .set(addr.row, addr.col, Entry::Const(v));
                calc::cell_changed(self, addr);

                if !self.tables.is_empty() && self.merge_at(addr).is_none() {
                    self.table_auto_expand(addr, false);
                    self.header_edited(addr);
                }

                if let Some(ti) = refill {
                    if addr.row == self.tables[ti].data_start() {
                        self.table_fill_new_rows(ti, (addr.row, addr.row), &[], true);
                    }
                }
            }
            Plan::Formula(formula, body) => self.apply_formula(addr, formula, body),
            Plan::Clear => self.apply_clear(addr),
        }
    }

    fn apply_formula(&mut self, addr: CellAddr, formula: Arc<Formula>, body: String) {
        self.remove_cell(addr);
        let fid = self.alloc_formula(addr, formula, body);
        self.note_hidden_names(fid);
        self.sheet_mut(addr.sheet)
            .unwrap()
            .cells
            .set(addr.row, addr.col, Entry::Formula(fid));
        self.link_formula(fid);
        {
            let root = self.formula(fid).formula.root.clone();
            if let Some(f) = self.infer_format(&root, addr) {
                if !self.cellfmt_has(addr) || self.auto_replaces(self.number_format(addr), &f) {
                    self.store_format(addr, &f);
                }
            }
        }
        calc::cell_changed(self, addr);
        calc::mark_dirty(self, fid);
        if self.calc.stale_tables {
            self.refresh_stale_tables(fid);
        }
        if !self.tables.is_empty() {
            self.table_auto_expand(addr, true);

            let header = self.tables.iter().any(|t| {
                t.sheet == addr.sheet
                    && t.header_row() == Some(addr.row)
                    && (t.rect.c0..=t.rect.c1).contains(&addr.col)
            });

            if header && self.formula_at(addr).is_some() {
                self.remove_cell(addr);
                self.sheet_mut(addr.sheet).unwrap().cells.set(
                    addr.row,
                    addr.col,
                    Entry::Const(Value::Number(0.0)),
                );
            }
            self.header_edited(addr);
            self.table_formula_under(addr);
        }
    }

    fn apply_clear(&mut self, addr: CellAddr) {
        let had = self.remove_cell(addr);
        if !had {
            self.clear_circular_spill(addr);
        }

        if had
            || self
                .ever_used(addr.sheet)
                .is_some_and(|u| u.contains(addr.row, addr.col))
        {
            calc::cell_changed(self, addr);
        }
        if !had && self.settings.mode == CalcMode::Manual {
            calc::block_spill_at(self, addr);
        }
        if !self.tables.is_empty() {
            self.header_edited(addr);
        }
    }

    fn clear_circular_spill(&mut self, addr: CellAddr) {
        let owner = self.sheet(addr.sheet).and_then(|sh| {
            let g = sh.spills.owner_of(addr.row, addr.col)?;
            let f = self.formula(g);
            (f.array.is_none() && f.addr != addr && self.calc.circular.contains(&g)).then_some(g)
        });
        let Some(g) = owner else {
            return;
        };
        let Some(rect) = self.sheet_mut(addr.sheet).unwrap().spills.remove(g) else {
            return;
        };
        self.formula_mut(g).spill = None;
        calc::rect_changed(self, addr.sheet, rect);
        calc::mark_dirty(self, g);
    }

    fn edit_after(&mut self, edit: &Edit) -> RecalcReport {
        let report = self.after_edit(vec![edit.addr]);

        if matches!(edit.kind, EditKind::Formula(_))
            && !self.tables.is_empty()
            && !edit.replace_entry
        {
            self.table_calc_fill(edit.addr);
        }
        report
    }

    pub(crate) fn clear_core(&mut self, addr: CellAddr) -> bool {
        if !self.clear_allowed(addr) {
            return false;
        }
        self.apply_clear(addr);
        true
    }
}
