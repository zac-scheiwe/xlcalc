use super::tables::{Spec, Table};
use crate::address::{CellAddr, Rect};
use crate::calc::{self, RecalcReport};
use crate::formula::{map_node, CRef, Formula, Node, Rc, SheetRef};
use crate::formula_print::print_node;
use crate::value::Value;
use crate::workbook::{CalcMode, EditError, Entry, Workbook};
use std::sync::Arc;

fn rc(v: i64, abs: bool) -> Rc {
    Rc { v: v as i32, abs }
}

impl Table {
    fn plain_ref(&self, spec: &Spec, host: CellAddr) -> Node {
        let cols = match &spec.cols {
            Some((a, b)) => match (self.column_index(a), self.column_index(b)) {
                (Some(a), Some(b)) => Some((a, b)),
                _ => return Node::DeadRef(SheetRef::Current),
            },
            None => None,
        };
        let (d0, d1) = self.data_rows();
        let (rect, relative) = match self.resolve(spec.items, cols, host.row) {
            Some(Ok(r)) => (r, spec.items.this_row),
            Some(Err(())) => (Rect::new(d0, self.rect.c0, d1, self.rect.c1), true),
            None => (self.rect, false),
        };
        let h = host.row as i64;
        let (r0, r1) = if relative {
            (rc(rect.r0 as i64 - h, false), rc(rect.r1 as i64 - h, false))
        } else {
            (rc(rect.r0 as i64, true), rc(rect.r1 as i64, true))
        };
        let (c0, c1) = (rc(rect.c0 as i64, true), rc(rect.c1 as i64, true));
        let cref = if rect.is_cell() {
            CRef::Cell { row: r0, col: c0 }
        } else {
            CRef::Area { r0, c0, r1, c1 }
        };
        Node::Ref(SheetRef::Sheet(self.sheet), cref)
    }
}

impl Workbook {
    fn table_position(&self, name: &str) -> Result<usize, EditError> {
        self.tables
            .iter()
            .position(|t| crate::text::eq_ordinal_ci(&t.name, name))
            .ok_or_else(|| EditError::Compile(format!("no such table '{name}'")))
    }

    fn rewrite_nodes(&mut self, f: &mut dyn FnMut(&Node, CellAddr) -> Option<Node>) -> bool {
        let mut any = false;
        let ids: Vec<_> = self.formula_ids().collect();
        for fid in ids {
            let (host, formula) = {
                let c = self.formula(fid);
                (c.addr, c.formula.clone())
            };
            let mut changed = false;
            let root = map_node(&formula.root, &mut |n| {
                let r = f(n, host);
                changed |= r.is_some();
                r
            });
            if changed {
                any = true;
                let text = print_node(self, &root, host);
                let c = self.formula_mut(fid);
                c.formula = Arc::new(Formula {
                    root,
                    volatile: formula.volatile,
                });
                c.text = text;
            }
        }
        let names: Vec<_> = self.names.iter().cloned().collect();
        for mut def in names {
            let host = def.host();
            let mut changed = false;
            let root = map_node(&def.formula.root, &mut |n| {
                let r = f(n, host);
                changed |= r.is_some();
                r
            });
            if changed {
                any = true;
                def.refers_to = print_node(self, &root, host);
                def.formula = Arc::new(Formula {
                    root,
                    volatile: def.formula.volatile,
                });
                self.names.insert(def);
            }
        }
        any
    }

    pub fn unlist_table(&mut self, name: &str) -> Result<RecalcReport, EditError> {
        let ti = self.table_position(name)?;
        let t = self.tables[ti].clone();

        let mut stale: Vec<(crate::workbook::FormulaId, Value)> = Vec::new();
        for fid in self.formula_ids().collect::<Vec<_>>() {
            let f = self.formula(fid);
            let mut root = &f.formula.root;
            while let Node::Paren(x) | Node::Ws { node: x, .. } = root {
                root = x;
            }
            if let Node::StructRef { table, spec, .. } = root {
                let one = spec.cols.as_ref().is_some_and(|(a, b)| a == b);
                let cols = spec
                    .cols
                    .as_ref()
                    .and_then(|(a, b)| Some((t.column_index(a)?, t.column_index(b)?)));
                let missing = !matches!(t.resolve(spec.items, cols, f.addr.row), Some(Ok(_)));
                if one && missing && crate::text::eq_ordinal_ci(table, &t.name) && !f.dirty {
                    stale.push((fid, f.value.clone()));
                }
            }
        }
        self.rewrite_nodes(&mut |n, host| match n {
            Node::StructRef { table, spec, .. } if crate::text::eq_ordinal_ci(table, &t.name) => {
                Some(t.plain_ref(spec, host))
            }
            _ => None,
        });
        self.tables.remove(ti);
        let report = self.relink_and_recalc();
        for (fid, v) in stale {
            if self.formulas.get(fid as usize).is_some_and(|f| f.is_some())
                && !matches!(v, Value::Array(_))
            {
                self.formula_mut(fid).value = v;
            }
        }
        Ok(report)
    }

    pub fn delete_table(&mut self, name: &str) -> Result<RecalcReport, EditError> {
        let ti = self.table_position(name)?;
        let t = self.tables[ti].clone();
        self.guard_arrays_rect(t.sheet, t.rect)?;
        self.rewrite_nodes(&mut |n, _| match n {
            Node::StructRef { table, .. } if crate::text::eq_ordinal_ci(table, &t.name) => {
                Some(Node::DeadRef(SheetRef::Current))
            }
            _ => None,
        });
        self.tables.remove(ti);
        self.relink_and_recalc();
        self.clear_range(t.sheet, t.rect)
    }

    pub fn rename_table(&mut self, name: &str, new: &str) -> Result<RecalcReport, EditError> {
        let ti = self.table_position(name)?;
        if crate::text::eq_ordinal_ci(&self.tables[ti].name, new) {
            return Ok(RecalcReport::default());
        }
        let sheet = self.tables[ti].sheet;
        if !crate::parser::is_valid_name(new)
            || self
                .tables
                .iter()
                .any(|o| o.sheet == sheet && crate::text::eq_ordinal_ci(&o.name, new))
        {
            return Err(EditError::BadName(new.to_string()));
        }
        let old = self.tables[ti].name.clone();
        self.tables[ti].name = new.to_string();
        let new_text: crate::value::Text = new.into();
        self.rewrite_nodes(&mut |n, _| match n {
            Node::StructRef {
                table,
                implicit,
                spec,
            } if crate::text::eq_ordinal_ci(table, &old) => Some(Node::StructRef {
                table: new_text.clone(),
                implicit: *implicit,
                spec: spec.clone(),
            }),
            _ => None,
        });

        for fid in self.formula_ids().collect::<Vec<_>>() {
            self.unlink_formula(fid);
            self.link_formula(fid);
        }
        Ok(RecalcReport::default())
    }

    pub fn set_table_headers(&mut self, name: &str, show: bool) -> Result<RecalcReport, EditError> {
        let ti = self.table_position(name)?;
        let t = self.tables[ti].clone();
        if t.headers == show {
            return Ok(RecalcReport::default());
        }
        let mode = self.settings.mode;
        let (c0, c1) = (t.rect.c0, t.rect.c1);
        if !show {
            self.settings.mode = CalcMode::Manual;
            self.tables[ti].rect.r0 += 1;
            self.tables[ti].headers = false;
            for c in c0..=c1 {
                self.clear(CellAddr::new(t.sheet, t.rect.r0, c));
            }
        } else {
            let free = t.rect.r0 > 0
                && (c0..=c1).all(|c| {
                    self.sheet(t.sheet)
                        .is_some_and(|s| s.cells.get(t.rect.r0 - 1, c).is_none())
                });
            self.settings.mode = CalcMode::Manual;
            let row = if free {
                t.rect.r0 - 1
            } else {
                let shift = crate::structure::Shift {
                    sheet: t.sheet,
                    axis: crate::structure::Axis::Rows,
                    at: t.rect.r0,
                    count: 1,
                    insert: true,
                    limit: Some((c0, c1)),
                    cells: false,
                };
                if let Err(e) = self.guard_arrays_shift(&shift) {
                    self.settings.mode = mode;
                    return Err(e);
                }
                self.tables.remove(ti);
                let _ = self.shift(shift);
                self.tables.insert(ti, t.clone());
                self.tables[ti].rect.r1 += 1;
                t.rect.r0
            };
            self.tables[ti].rect.r0 = row;
            self.tables[ti].headers = true;
            for (i, n) in t.columns.iter().enumerate() {
                let a = CellAddr::new(t.sheet, row, c0 + i as u32);
                if let Some(sh) = self.sheet_mut(t.sheet) {
                    sh.cells
                        .set(a.row, a.col, Entry::Const(Value::from(n.as_str())));
                }
                calc::cell_changed(self, a);
            }
        }
        self.settings.mode = mode;
        let name = self.tables[ti].name.clone();
        Ok(self.table_changed(&name))
    }

    fn new_resized_column(&mut self, t: &Table, col: u32, taken: &[String]) -> String {
        let key = |s: &str| s.chars().map(crate::text::fold_upper).collect::<String>();
        let used = |n: &str| taken.iter().any(|x| key(x) == key(n));
        let addr = CellAddr::new(t.sheet, t.rect.r0, col);
        let given = if t.headers {
            match self.value(addr) {
                Value::Blank => None,
                v => Some(self.header_text(addr, &v)),
            }
        } else {
            None
        };
        let name = match given {
            Some(mut n) => {
                if used(&n) {
                    let mut k = 2;
                    while used(&format!("{n}{k}")) {
                        k += 1;
                    }
                    n = format!("{n}{k}");
                }
                n
            }
            None => {
                let mut k = 1;
                while used(&format!("Column{k}")) {
                    k += 1;
                }
                format!("Column{k}")
            }
        };
        if t.headers && !matches!(self.value(addr), Value::Text(ref x) if **x == *name) {
            if self.formula_at(addr).is_some() {
                self.remove_cell(addr);
            }
            if let Some(sh) = self.sheet_mut(t.sheet) {
                sh.cells
                    .set(addr.row, addr.col, Entry::Const(Value::from(name.as_str())));
            }
            calc::cell_changed(self, addr);
        }
        name
    }

    pub fn resize_table(&mut self, name: &str, rect: Rect) -> Result<RecalcReport, EditError> {
        let ti = self.table_position(name)?;
        let t = self.tables[ti].clone();
        let invalid = || {
            EditError::Compile(
                "the specified range is invalid: the top of the table must remain in the same row, and the result must overlap the original table".into(),
            )
        };
        if rect.r0 != t.rect.r0 || !rect.intersects(&t.rect) || rect.c0 > rect.c1 {
            return Err(invalid());
        }
        let new_end = if t.totals && rect.r1 < t.rect.r1 {
            rect.r1 + 1
        } else {
            rect.r1
        };
        let data_end = new_end - u32::from(t.totals);
        if data_end < t.data_start() {
            return Err(invalid());
        }
        let newrect = Rect::new(rect.r0, rect.c0, new_end, rect.c1);
        crate::structure::range_too_large(&newrect)?;
        if self
            .tables
            .iter()
            .enumerate()
            .any(|(i, o)| i != ti && o.sheet == t.sheet && o.rect.intersects(&newrect))
        {
            return Err(EditError::Compile(
                "cannot resize over cells inside and outside a table, or over several tables"
                    .into(),
            ));
        }
        if self
            .array_blocks(t.sheet)
            .iter()
            .any(|(_, b)| b.intersects(&newrect) && !t.rect.contains(b.r0, b.c0))
        {
            return Err(EditError::Compile(
                "multi-cell array formulas aren't allowed in tables".into(),
            ));
        }
        if newrect == t.rect {
            return Ok(RecalcReport::default());
        }
        let mode = self.settings.mode;
        self.settings.mode = CalcMode::Manual;

        let old_de = t.data_rows().1;
        let shrunk = data_end < old_de;
        if shrunk {
            let (d0, sheet) = (t.data_start(), t.sheet);
            let ids: Vec<crate::workbook::FormulaId> = self.formula_ids().collect();
            for fid in ids {
                let (host, formula) = {
                    let f = self.formula(fid);
                    (f.addr, f.formula.clone())
                };
                let mut changed = false;
                let root = crate::structure::edit_refs(&formula.root, &mut |sr, cref| {
                    let tsheet = match sr {
                        SheetRef::Current => host.sheet,
                        SheetRef::Sheet(id) => *id,
                        _ => return crate::structure::RefEdit::Keep,
                    };
                    if tsheet != sheet || !matches!(cref, CRef::Area { .. }) {
                        return crate::structure::RefEdit::Keep;
                    }
                    match crate::moves::cref_rect(cref, host) {
                        Some(r)
                            if r.r0 >= d0
                                && r.r0 <= data_end
                                && r.r1 > data_end
                                && r.r1 <= old_de
                                && t.rect.c0 <= r.c0
                                && r.c1 <= t.rect.c1 =>
                        {
                            changed = true;
                            crate::structure::RefEdit::Set(
                                sr.clone(),
                                crate::moves::encode_rect(
                                    cref,
                                    Rect::new(r.r0, r.c0, data_end, r.c1),
                                    host,
                                ),
                            )
                        }
                        _ => crate::structure::RefEdit::Keep,
                    }
                });
                if changed {
                    let text = print_node(self, &root, host);
                    let f = self.formula_mut(fid);
                    f.formula = Arc::new(Formula {
                        root,
                        volatile: formula.volatile,
                    });
                    f.text = text;
                }
            }
        }

        let (oc0, oc1) = (t.rect.c0, t.rect.c1);

        let kc1 = oc1.min(rect.c1);
        let mut tn = t.clone();
        if kc1 < oc1 {
            let w = (kc1 - oc0 + 1) as usize;
            tn.rect.c1 = kc1;
            tn.columns.truncate(w);
            tn.calc.truncate(w);
            self.tables[ti] = tn.clone();
        }
        let old_t = t.rect.r1;
        let mut rows_grew = None;
        if t.totals && new_end != old_t {
            self.tables.remove(ti);
            let sheet = t.sheet;
            let tmp = crate::address::MAX_ROWS - 1;
            let row_shift = |at: u32, insert: bool| crate::structure::Shift {
                sheet,
                axis: crate::structure::Axis::Rows,
                at,
                count: 1,
                insert,
                limit: Some((oc0, kc1)),
                cells: false,
            };
            let _ = self.move_range(
                sheet,
                Rect::new(old_t, oc0, old_t, kc1),
                CellAddr::new(sheet, tmp, oc0),
            );
            let _ = self.shift(row_shift(old_t, false));
            let _ = self.shift(row_shift(new_end, true));
            let _ = self.move_range(
                sheet,
                Rect::new(tmp, oc0, tmp, kc1),
                CellAddr::new(sheet, new_end, oc0),
            );
            self.tables.insert(ti, tn.clone());
            if new_end > old_t {
                rows_grew = Some((old_t, new_end - 1));
            }
        } else if !t.totals && new_end > old_t {
            rows_grew = Some((old_t + 1, new_end));
        }

        if let Some((_, last)) = rows_grew {
            let mut r = tn.rect;
            r.r1 = if t.totals { old_t - 1 } else { old_t };
            self.tables[ti].rect = r;
            self.tables[ti].totals = false;
            let mut grown = r;
            grown.r1 = last;
            self.grow_table_by_paste(ti, grown, true);
            self.tables[ti].totals = t.totals;

            for row in r.r1 + 1..=last {
                let mut g = r;
                g.r1 = row;
                self.extend_row_formats(t.sheet, g);
            }
        }
        self.tables[ti].rect.r1 = new_end;

        let mut columns: Vec<String> = Vec::new();
        let mut calc: Vec<bool> = Vec::new();
        for c in rect.c0..=rect.c1 {
            if c >= oc0 && c <= oc1 {
                let i = (c - oc0) as usize;
                columns.push(t.columns[i].clone());
                calc.push(t.calc[i]);
            } else {
                columns.push(String::new());
                calc.push(false);
            }
        }
        let kept: Vec<String> = columns.iter().filter(|c| !c.is_empty()).cloned().collect();
        let mut taken = kept;
        let mut shown = self.tables[ti].clone();
        shown.rect = newrect;
        for (i, c) in (rect.c0..=rect.c1).enumerate() {
            if columns[i].is_empty() {
                let n = self.new_resized_column(&shown, c, &taken);
                taken.push(n.clone());
                columns[i] = n;
            }
        }

        let gone: Vec<String> = t
            .columns
            .iter()
            .filter(|c| !columns.iter().any(|k| crate::text::eq_ordinal_ci(k, c)))
            .cloned()
            .collect();
        if !gone.is_empty() {
            self.rewrite_nodes(&mut |n, _| match n {
                Node::StructRef { table, spec, .. }
                    if crate::text::eq_ordinal_ci(table, &t.name) =>
                {
                    let (a, b) = spec.cols.as_ref()?;
                    let dead = gone.iter().any(|g| {
                        crate::text::eq_ordinal_ci(g, a) || crate::text::eq_ordinal_ci(g, b)
                    });
                    dead.then(|| Node::DeadRef(SheetRef::Current))
                }
                _ => None,
            });
        }
        let live: Vec<String> = columns.iter().map(|c| c.to_lowercase()).collect();
        let tb = &mut self.tables[ti];
        tb.rect = newrect;
        tb.columns = columns;
        tb.calc = calc;
        tb.calc_key.retain(|(k, _)| live.contains(k));
        self.settings.mode = mode;
        let name = self.tables[ti].name.clone();
        if shrunk {
            return Ok(self.relink_and_recalc());
        }
        Ok(self.table_changed(&name))
    }
}
