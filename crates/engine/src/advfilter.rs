use crate::address::{CellAddr, Rect, SheetId, MAX_ROWS};
use crate::calc::RecalcReport;
use crate::eval::{Area, EvalCtx};
use crate::functions::criteria::db::matching_records;
use crate::text::eq_ci;
use crate::value::Value;
use crate::workbook::{CalcMode, Command, EditError, Workbook};

#[derive(Clone, Debug)]
pub struct AdvancedFilterSpec {
    pub list: (SheetId, Rect),

    pub criteria: Option<(SheetId, Rect)>,

    pub copy_to: Option<(SheetId, Rect)>,

    pub unique: bool,
}

fn header_name(v: &Value) -> Option<String> {
    match v {
        Value::Text(t) if !t.is_empty() => Some(t.to_string()),
        Value::Number(n) => Some(crate::number::format_general(*n)),
        Value::Bool(b) => Some(if *b { "TRUE" } else { "FALSE" }.to_string()),
        _ => None,
    }
}

fn row_key(vals: &[Value]) -> Vec<String> {
    vals.iter()
        .map(|v| match v {
            Value::Text(t) => format!("t{}", t.to_lowercase()),
            Value::Blank => "t".to_string(),
            other => format!("{other:?}"),
        })
        .collect()
}

impl Workbook {
    fn drop_range_filter(&mut self, sheet: SheetId) -> Vec<u32> {
        let old: Vec<crate::view::Filter> = self
            .view_mut(sheet)
            .filters
            .iter()
            .filter(|f| f.table.is_none() && !f.crit.is_empty())
            .cloned()
            .collect();
        let mut changed = Vec::new();
        for f in &old {
            if let Some(rect) = self.filter_rect(f) {
                let last = if f.total_row { rect.r1 - 1 } else { rect.r1 };
                let v = self.view_mut(sheet);
                for r in rect.r0 + 1..=last {
                    if v.hidden_rows.remove(&r) {
                        changed.push(r);
                    }
                }
            }
        }
        self.view_mut(sheet).filters.retain(|f| f.table.is_some());
        changed
    }

    pub fn advanced_filter(
        &mut self,
        spec: &AdvancedFilterSpec,
    ) -> Result<RecalcReport, EditError> {
        self.command(Command::View, |wb| wb.advanced_filter_run(spec))
    }

    fn advanced_filter_run(
        &mut self,
        spec: &AdvancedFilterSpec,
    ) -> Result<RecalcReport, EditError> {
        let bad = || EditError::Compile("AdvancedFilter method of Range class failed".into());
        let (sheet, list) = spec.list;
        self.sheet(sheet).ok_or(EditError::NoSuchSheet)?;
        if list.rows() < 2 {
            return Err(EditError::Compile(
                "This command requires at least two rows of source data".into(),
            ));
        }

        let pass: Vec<bool> = match spec.criteria {
            None => vec![true; (list.rows() - 1) as usize],
            Some((cs, crect)) => {
                let host = CellAddr::new(sheet, list.r0, list.c0);
                let mut ctx = EvalCtx::new(self, host);
                let db = Area { sheet, rect: list };
                let crit = Area {
                    sheet: cs,
                    rect: crect,
                };
                matching_records(&mut ctx, &db, &crit).map_err(|_| bad())?
            }
        };
        let rows: Vec<u32> = (list.r0 + 1..=list.r1).collect();
        let row_vals = |wb: &Workbook, r: u32, cols: &[u32]| -> Vec<Value> {
            cols.iter()
                .map(|&c| wb.value(CellAddr::new(sheet, r, c)))
                .collect()
        };
        match spec.copy_to {
            None => {
                let all: Vec<u32> = (list.c0..=list.c1).collect();
                let mut seen = rustc_hash::FxHashSet::default();
                let keep: Vec<bool> = rows
                    .iter()
                    .zip(&pass)
                    .map(|(&r, &p)| {
                        p && (!spec.unique || seen.insert(row_key(&row_vals(self, r, &all))))
                    })
                    .collect();

                let mut changed = self.drop_range_filter(sheet);
                let v = self.view_mut(sheet);
                for (&r, &k) in rows.iter().zip(&keep) {
                    if v.hidden_rows.contains(&r) == !k {
                        continue;
                    }
                    changed.push(r);
                    if k {
                        v.hidden_rows.remove(&r);
                    } else {
                        v.hidden_rows.insert(r);
                    }
                }
                v.adv = keep.contains(&false).then_some(list);
                for &r in changed
                    .iter()
                    .filter(|&&r| r > list.r0 && r <= list.r1 && !keep[(r - list.r0 - 1) as usize])
                {
                    self.view_touch_row_pub(sheet, r);
                }
                changed.sort_unstable();
                changed.dedup();
                self.rows_visibility_changed(sheet, &changed);
                Ok(self.view_settle())
            }
            Some((ds, drect)) => {
                let shown = self.drop_range_filter(sheet);
                self.rows_visibility_changed(sheet, &shown);
                self.sheet(ds).ok_or(EditError::NoSuchSheet)?;
                if ds == sheet
                    && drect.r0 <= list.r1
                    && drect.r1 >= list.r0
                    && drect.c0 <= list.c1
                    && drect.c1 >= list.c0
                {
                    return Err(bad());
                }
                let names: Vec<Option<String>> = (list.c0..=list.c1)
                    .map(|c| header_name(&self.value(CellAddr::new(sheet, list.r0, c))))
                    .collect();

                let first = self.value(CellAddr::new(ds, drect.r0, drect.c0));
                let own_headers = drect.is_cell() && first.is_blank();
                let mut cols: Vec<(u32, u32)> = Vec::new();
                if own_headers {
                    cols.extend((0..list.cols()).map(|j| {
                        let at = names[j as usize].as_deref().and_then(|n| {
                            names
                                .iter()
                                .position(|m| m.as_deref().is_some_and(|m| eq_ci(m, n)))
                        });
                        (list.c0 + at.map_or(j, |a| a as u32), drect.c0 + j)
                    }));
                } else {
                    let width = if drect.is_cell() { 1 } else { drect.cols() };
                    for k in 0..width {
                        let h = header_name(&self.value(CellAddr::new(ds, drect.r0, drect.c0 + k)));
                        let at = h.and_then(|h| {
                            names
                                .iter()
                                .position(|n| n.as_deref().is_some_and(|n| eq_ci(n, &h)))
                        });
                        let Some(at) = at else {
                            return Err(EditError::Compile(
                                "The extract range has a missing or invalid field name.".into(),
                            ));
                        };
                        cols.push((list.c0 + at as u32, drect.c0 + k));
                    }
                }
                let srcs: Vec<u32> = cols.iter().map(|c| c.0).collect();
                let mut seen = rustc_hash::FxHashSet::default();
                let out: Vec<u32> = rows
                    .iter()
                    .zip(&pass)
                    .filter(|&(&r, &p)| {
                        p && (!spec.unique || seen.insert(row_key(&row_vals(self, r, &srcs))))
                    })
                    .map(|(&r, _)| r)
                    .collect();

                let (lo, hi) = (cols[0].1, cols[cols.len() - 1].1);
                let below = Rect::new((drect.r0 + 1).min(MAX_ROWS - 1), lo, MAX_ROWS - 1, hi);
                let old: Vec<(u32, u32)> = self
                    .sheet(ds)
                    .map(|s| s.cells.iter_rect(below).map(|(r, c, _)| (r, c)).collect())
                    .unwrap_or_default();
                let mut report = RecalcReport::default();
                for (r, c) in old {
                    let to = CellAddr::new(ds, r, c);
                    report.changed.push(to);
                    self.set_value_raw(to, Value::Blank);
                }
                let mut pairs = Vec::new();
                let mut writes = Vec::new();
                for (k, &r) in out.iter().enumerate() {
                    for &(sc, dc) in &cols {
                        let from = CellAddr::new(sheet, r, sc);
                        let to = CellAddr::new(ds, drect.r0 + 1 + k as u32, dc);
                        if to.row >= MAX_ROWS {
                            break;
                        }
                        pairs.push((from, to));
                        writes.push((to, self.value(from)));
                    }
                }

                let bad_header = own_headers
                    && (list.c0..=list.c1).any(|c| {
                        let a = CellAddr::new(sheet, list.r0, c);
                        self.formula_at(a).is_some()
                            && !matches!(self.value(a), Value::Text(_) | Value::Blank)
                    });

                if own_headers {
                    let head = Rect::new(list.r0, list.c0, list.r0, list.c1);
                    let dst = CellAddr::new(ds, drect.r0, drect.c0);
                    let r = self.copy_range_with(sheet, head, dst, false)?;
                    report.changed.extend(r.changed);
                }
                if bad_header {
                    return Err(bad());
                }
                self.fmt_move_pairs(&pairs);
                for (to, v) in writes {
                    report.changed.push(to);
                    self.set_value_raw(to, v);
                }
                if self.settings.mode == CalcMode::Automatic {
                    let r = crate::calc::recalc(self);
                    report.evaluations += r.evaluations;
                    report.add_sheets(&r);
                    report.changed.extend(r.changed);
                }
                Ok(report)
            }
        }
    }
}
