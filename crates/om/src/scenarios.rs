use xlcalc::scenarios::{cells_of, Scenario, ScenarioCell, MAX_CHANGING_CELLS};
use xlcalc::{CellAddr, Rect, SheetId, Value};
use xlcalc_variant::{SafeArray, VarType, Variant};

use crate::app::{unsupported, Application, BookId, Mode, Object};
use crate::conv::{array_cells, Args};
use crate::error::OmError;
use crate::typelib::{InvokeKind, Member};

const MAX_COMMENT: usize = 255;

fn default_comment() -> String {
    format!("Created by {} on {}", crate::notesobj::user_name(), today())
}

fn today() -> String {
    let serial = match crate::fsobj::ole_date(Ok(std::time::SystemTime::now())) {
        Variant::Date(d) => d.floor() as i64,
        _ => 0,
    };

    let z = serial - 25569 + 719_468;
    let era = z.div_euclid(146_097);
    let doe = z.rem_euclid(146_097);
    let yoe = (doe - doe / 1460 + doe / 36_524 - doe / 146_096) / 365;
    let doy = doe - (365 * yoe + yoe / 4 - yoe / 100);
    let mp = (5 * doy + 2) / 153;
    let d = doy - (153 * mp + 2) / 5 + 1;
    let m = if mp < 10 { mp + 3 } else { mp - 9 };
    let y = yoe + era * 400 + i64::from(m <= 2);
    format!("{m}/{d}/{y}")
}

fn add_failed() -> OmError {
    OmError::app_defined_with("Add method of Scenarios class failed")
}

fn unable_to_get(property: &str, class: &str) -> OmError {
    OmError::app_defined_with(&format!(
        "Unable to get the {property} property of the {class} class"
    ))
}

fn same_name(a: &str, b: &str) -> bool {
    a.to_lowercase() == b.to_lowercase()
}

fn value_text(v: &Variant) -> Option<String> {
    match v {
        Variant::Bstr(s) if s.starts_with('=') => None,
        Variant::Bstr(s) => Some(s.clone()),
        Variant::Empty => Some(String::new()),
        Variant::I2(n) => Some(n.to_string()),
        Variant::I4(n) => Some(n.to_string()),
        Variant::R4(n) => Some(n.to_string()),
        Variant::R8(n) => Some(n.to_string()),
        Variant::Bool(b) => Some(if *b { "TRUE" } else { "FALSE" }.into()),
        _ => None,
    }
}

fn bounds(cells: &[ScenarioCell]) -> Option<Rect> {
    let first = cells.first()?;
    let mut r = Rect::cell(first.row, first.col);
    for c in cells {
        r = r.union_bounds(&Rect::cell(c.row, c.col));
    }
    Some(r)
}

impl Application<'_> {
    fn scenario_cells(
        &self,
        b: BookId,
        sheet: SheetId,
        cells: Option<&Variant>,
        values: Option<&Variant>,
    ) -> Option<(Vec<Rect>, Vec<ScenarioCell>)> {
        let (_, s, rects) = self.range_areas(cells).ok()?;
        if s != sheet {
            return None;
        }
        let list: Vec<(u32, u32)> = rects.iter().flat_map(|r| cells_of(*r)).collect();
        if list.len() > MAX_CHANGING_CELLS {
            return None;
        }
        let given = match values {
            Some(v) => array_cells(v)?.2,
            None => Vec::new(),
        };
        let wb = &self.book(b).ok()?.wb;
        let cells = list
            .into_iter()
            .enumerate()
            .map(|(i, (row, col))| {
                let value = match given.get(i) {
                    Some(v) => value_text(v)?,
                    None => wb.scenario_text(CellAddr::new(sheet, row, col)),
                };
                Some(ScenarioCell { row, col, value })
            })
            .collect::<Option<Vec<_>>>()?;
        Some((rects, cells))
    }

    fn scenario_list(&self, b: BookId, s: SheetId) -> Result<Vec<Scenario>, OmError> {
        Ok(self
            .book(b)?
            .wb
            .scenarios(s)
            .map(|x| x.list.clone())
            .unwrap_or_default())
    }

    pub(crate) fn sheet_scenarios(
        &mut self,
        b: BookId,
        s: SheetId,
        a: &Args,
    ) -> Result<Variant, OmError> {
        let Some(index) = a.get(0) else {
            return Ok(self.alloc(Object::Scenarios(b, s)));
        };
        let list = self.scenario_list(b, s)?;
        let i = self
            .item_index(Some(index), a.lcid, &list, |x| x.name.as_str())
            .map_err(|_| unable_to_get("Scenarios", "Worksheet"))?;
        Ok(self.alloc(Object::Scenario(b, s, list[i].name.clone())))
    }

    pub(crate) fn scenarios_member(
        &mut self,
        b: BookId,
        s: SheetId,
        m: &'static Member,
        a: &Args,
    ) -> Result<Variant, OmError> {
        let list = self.scenario_list(b, s)?;
        match m.name {
            "Count" => Ok(Variant::I4(list.len() as i32)),
            "Item" | "_Default" => {
                let i = self
                    .item_index(a.get(0), a.lcid, &list, |x| x.name.as_str())
                    .map_err(|_| unable_to_get("Item", "Scenarios"))?;
                Ok(self.alloc(Object::Scenario(b, s, list[i].name.clone())))
            }
            "_NewEnum" => {
                let items = list
                    .iter()
                    .map(|x| self.alloc(Object::Scenario(b, s, x.name.clone())))
                    .collect();
                Ok(self.alloc_enumerator(items))
            }
            "Parent" => Ok(self.alloc(Object::Worksheet(b, s))),
            "Application" => Ok(Variant::Dispatch(Some(self.handle()))),
            "Add" => {
                let name = a.string(0).map_err(|_| add_failed())?.unwrap_or_default();
                if name.is_empty() || name.chars().count() > 255 {
                    return Err(add_failed());
                }
                if list.iter().any(|x| same_name(&x.name, &name)) {
                    return Err(add_failed());
                }
                let (areas, cells) = self
                    .scenario_cells(b, s, a.get(1), a.get(2))
                    .ok_or_else(add_failed)?;
                let flag = |i: usize, default: bool| -> Result<bool, OmError> {
                    Ok(a.bool(i).map_err(|_| add_failed())?.unwrap_or(default))
                };
                let scenario = Scenario {
                    name: name.clone(),
                    locked: flag(4, true)?,
                    hidden: flag(5, false)?,
                    user: String::new(),

                    comment: a
                        .string(3)
                        .map_err(|_| add_failed())?
                        .unwrap_or_else(default_comment),
                    cells,
                    areas,
                };
                self.book_mut(b)?.wb.scenarios_mut(s).list.push(scenario);
                Ok(self.alloc(Object::Scenario(b, s, name)))
            }
            "Merge" => {
                let src = self.merge_source(b, a.get(0))?;
                let from = self.scenario_list(b, src)?;
                let stamp = today();
                let sheet = self.book_mut(b)?.wb.scenarios_mut(s);
                for mut sc in from {
                    if sheet.list.iter().any(|x| same_name(&x.name, &sc.name)) {
                        sc.name = format!("{} {stamp}", sc.name);
                    }
                    sheet.list.push(sc);
                }
                Ok(Variant::Bool(true))
            }
            "CreateSummary" => self.create_summary(b, s, a),
            _ => Err(unsupported("Scenarios", m)),
        }
    }

    fn merge_source(&self, b: BookId, v: Option<&Variant>) -> Result<SheetId, OmError> {
        let failed = || OmError::app_defined_with("Merge method of Scenarios class failed");
        match v {
            Some(Variant::Bstr(name)) => self
                .book(b)?
                .wb
                .sheet_by_name(name)
                .map(|x| x.id)
                .ok_or_else(failed),
            Some(Variant::Dispatch(Some(id))) => match self.arena.get(*id) {
                Some(Object::Worksheet(ob, os)) if *ob == b => Ok(*os),
                _ => Err(failed()),
            },
            _ => Err(failed()),
        }
    }

    fn summary_label(&self, b: BookId, at: CellAddr) -> Result<String, OmError> {
        let wb = &self.book(b)?.wb;
        let abs = format!("${}${}", xlcalc::col_to_letters(at.col), at.row + 1);
        let names_cell = |d: &xlcalc::NameDef| {
            let text = wb.name_refers_to(d);
            let default = d.scope.unwrap_or(at.sheet);
            matches!(crate::addr::parse_areas_text(wb, default, &text),
                Some((sh, rects)) if sh == at.sheet && rects == [Rect::cell(at.row, at.col)])
        };
        Ok(wb
            .names()
            .iter()
            .find(|d| d.scope.is_none_or(|x| x == at.sheet) && names_cell(d))
            .map_or(abs, |d| d.name.clone()))
    }

    fn create_summary(&mut self, b: BookId, s: SheetId, a: &Args) -> Result<Variant, OmError> {
        let failed = || OmError::app_defined_with("CreateSummary method of Scenarios class failed");
        let kind = a.i32(0).map_err(|_| failed())?.unwrap_or(1);
        let results: Vec<CellAddr> = match a.get(1) {
            None | Some(Variant::Empty) => Vec::new(),
            v => {
                let (_, rs, rects) = self.range_areas(v).map_err(|_| failed())?;
                rects
                    .iter()
                    .flat_map(|r| cells_of(*r))
                    .map(|(row, col)| CellAddr::new(rs, row, col))
                    .collect()
            }
        };
        if kind == 2 {
            if results.is_empty() {
                self.book_mut(b)?
                    .wb
                    .set_active_sheet(s)
                    .map_err(crate::objects::edit_error)?;
                return Ok(self.alloc(Object::Worksheet(b, s)));
            }
            return self
                .create_pivot_summary(b, s, &results)
                .unwrap_or_else(|| Err(failed()));
        }
        let run = self
            .book_mut(b)?
            .wb
            .run_scenario_summary(s, &results)
            .ok_or_else(failed)?;
        let list = self.scenario_list(b, s)?;
        let mut grid: Vec<(u32, u32, Value)> = vec![(1, 1, Value::text("Scenario Summary"))];
        grid.push((2, 3, Value::text("Current Values:")));
        for (i, sc) in list.iter().enumerate() {
            let c = 4 + i as u32;
            grid.push((2, c, Value::text(sc.name.as_str())));
            if !sc.comment.is_empty() {
                grid.push((3, c, Value::text(sc.comment.as_str())));
            }
        }
        grid.push((4, 1, Value::text("Changing Cells:")));
        let mut row = 5;
        let put_rows = |grid: &mut Vec<(u32, u32, Value)>,
                        row: &mut u32,
                        labels: Vec<String>,
                        current: &[Value],
                        per: &[Vec<Value>]| {
            for (k, l) in labels.into_iter().enumerate() {
                grid.push((*row, 2, Value::text(l.as_str())));
                grid.push((*row, 3, current[k].clone()));
                for (i, vals) in per.iter().enumerate() {
                    grid.push((*row, 4 + i as u32, vals[k].clone()));
                }
                *row += 1;
            }
        };
        let labels = run
            .cells
            .iter()
            .map(|&(r, c)| self.summary_label(b, CellAddr::new(s, r, c)))
            .collect::<Result<Vec<_>, _>>()?;
        put_rows(&mut grid, &mut row, labels, &run.current, &run.values);
        if results.is_empty() {
            row += 1;
        } else {
            grid.push((row, 1, Value::text("Result Cells:")));
            row += 1;
            let labels = results
                .iter()
                .map(|at| self.summary_label(b, *at))
                .collect::<Result<Vec<_>, _>>()?;
            put_rows(
                &mut grid,
                &mut row,
                labels,
                &run.current_results,
                &run.results,
            );
        }
        for note in [
            "Notes:  Current Values column represents values of changing cells at",
            "time Scenario Summary Report was created.  Changing cells for each",
            "scenario are highlighted in gray.",
        ] {
            grid.push((row, 1, Value::text(note)));
            row += 1;
        }
        let book = self.book_mut(b)?;
        let mut name = "Scenario Summary".to_string();
        let mut n = 1;
        while book.wb.sheet_by_name(&name).is_some() {
            n += 1;
            name = format!("Scenario Summary {n}");
        }
        let id = book
            .wb
            .add_sheet_before(&name, Some(s))
            .map_err(crate::objects::edit_error)?;
        for (r, c, v) in grid {
            if !v.is_blank() {
                book.wb.set_value(CellAddr::new(id, r, c), v);
            }
        }

        book.wb.set_style_with(id, Rect::cell(0, 1), &|st| {
            let mut st = st.clone();
            st.font.bold = true;
            st
        });
        book.wb
            .set_active_sheet(id)
            .map_err(crate::objects::edit_error)?;
        Ok(self.alloc(Object::Worksheet(b, s)))
    }

    fn create_pivot_summary(
        &mut self,
        b: BookId,
        s: SheetId,
        results: &[CellAddr],
    ) -> Option<Result<Variant, OmError>> {
        use xlcalc::pivot::{Ax, DataField, Orient, PivotCache, PivotTable};
        let run = self.book_mut(b).ok()?.wb.run_scenario_pivot(s, results)?;
        let n = run.fields.len();
        let captions: Vec<String> = results
            .iter()
            .map(|a| format!("${}${}", xlcalc::col_to_letters(a.col), a.row + 1))
            .collect();

        let mut names: Vec<String> = run.fields.clone();
        names.extend(
            run.fields
                .iter()
                .map(|f| f.trim_end_matches(" by").to_string()),
        );
        names.extend(captions.iter().cloned());
        let rows: Vec<Vec<Value>> = run
            .records
            .iter()
            .map(|(labels, vals)| {
                let l = labels.iter().map(|&i| Value::text(run.names[i].as_str()));
                l.clone().chain(l).chain(vals.iter().cloned()).collect()
            })
            .collect();
        let cache = PivotCache::from_records("", s, names, rows, |v| match v {
            Value::Text(t) => t.to_string(),
            Value::Number(x) => xlcalc::format_general(*x),
            _ => String::new(),
        });
        let funcs: Vec<_> = (0..results.len())
            .map(|k| crate::pivot::default_func(&cache, 2 * n + k))
            .collect();
        let nfields = cache.fields.len();
        let book = match self.book_mut(b) {
            Ok(x) => x,
            Err(e) => return Some(Err(e)),
        };
        let mut sheet_name = "Scenario PivotTable".to_string();
        let mut k = 1;
        while book.wb.sheet_by_name(&sheet_name).is_some() {
            k += 1;
            sheet_name = format!("Scenario PivotTable {k}");
        }
        let id = match book.wb.add_sheet_before(&sheet_name, Some(s)) {
            Ok(id) => id,
            Err(e) => return Some(Err(crate::objects::edit_error(e))),
        };
        let pv = book.wb.pivots_mut();
        pv.caches.push(cache);
        let ci = pv.caches.len() - 1;
        let mut tname = "PivotTable1".to_string();
        let mut k = 1;
        while pv
            .tables
            .iter()
            .any(|t| t.name.eq_ignore_ascii_case(&tname))
        {
            k += 1;
            tname = format!("PivotTable{k}");
        }
        let mut t = PivotTable::new(&tname, ci, nfields, id, n as u32 + 1, 0);
        for g in 0..n {
            t.fields[g].orient = Orient::Page;
            t.pages.push(g);
            t.fields[n + g].orient = Orient::Row;
            t.fields[n + g].subtotals = false;
            t.rows.push(Ax::Field(n + g));
        }
        for (k, (caption, func)) in captions.into_iter().zip(funcs).enumerate() {
            t.fields[2 * n + k].orient = Orient::Data;
            t.add_data(DataField {
                src: 2 * n + k,
                caption,
                func,

                numfmt: Some("General".into()),
                show: Default::default(),
            });
        }
        t.row_grand = false;
        t.col_grand = false;
        pv.tables.push(t);
        let ti = pv.tables.len() - 1;
        let done = book
            .wb
            .pivot_update(ti)
            .and_then(|()| book.wb.set_active_sheet(id))
            .map_err(crate::objects::edit_error);
        Some(done.map(|()| self.alloc(Object::Worksheet(b, s))))
    }

    fn scenario_renamed(&mut self, b: BookId, s: SheetId, old: &str, new: &str) {
        self.arena.for_each_mut(|o| {
            if let Object::Scenario(ob, os, n) = o {
                if *ob == b && *os == s && same_name(n, old) {
                    *n = new.to_string();
                }
            }
        });
    }

    fn edit_scenario(
        &mut self,
        b: BookId,
        s: SheetId,
        i: usize,
        f: impl FnOnce(&mut Scenario),
    ) -> Result<Variant, OmError> {
        if let Some(x) = self.book_mut(b)?.wb.scenarios_mut(s).list.get_mut(i) {
            f(x);
        }
        Ok(Variant::Empty)
    }

    pub(crate) fn scenario_member(
        &mut self,
        b: BookId,
        s: SheetId,
        name: &str,
        m: &'static Member,
        a: &Args,
    ) -> Result<Variant, OmError> {
        let class = "Scenario";
        let list = self.scenario_list(b, s)?;
        let i = list
            .iter()
            .position(|x| same_name(&x.name, name))
            .ok_or_else(OmError::object_required)?;
        let sc = &list[i];
        let put = m.kind == InvokeKind::Put;
        match (m.name, put) {
            ("Name", false) => Ok(Variant::Bstr(sc.name.clone())),
            ("Name", true) => {
                let new = self
                    .put_string(a)
                    .map_err(|_| OmError::unable_to_set("Name", class))?;

                if new.is_empty() {
                    return Ok(Variant::Empty);
                }
                let taken = list
                    .iter()
                    .enumerate()
                    .any(|(j, x)| j != i && same_name(&x.name, &new));
                if new.chars().count() > 255 || taken {
                    return Err(OmError::unable_to_set("Name", class));
                }
                self.scenario_renamed(b, s, name, &new);
                self.edit_scenario(b, s, i, |x| x.name = new)
            }
            ("Comment", false) => Ok(Variant::Bstr(sc.comment.clone())),
            ("Comment", true) => {
                let text = self
                    .put_string(a)
                    .map_err(|_| OmError::unable_to_set("Comment", class))?;
                if text.chars().count() > MAX_COMMENT {
                    return Err(OmError::unable_to_set("Comment", class));
                }
                self.edit_scenario(b, s, i, |x| x.comment = text)
            }
            ("Locked", false) => Ok(Variant::Bool(sc.locked)),
            ("Locked", true) => {
                let v = self
                    .put_bool(a)
                    .map_err(|_| OmError::unable_to_set("Locked", class))?;
                self.edit_scenario(b, s, i, |x| x.locked = v)
            }
            ("Hidden", false) => Ok(Variant::Bool(sc.hidden)),
            ("Hidden", true) => {
                let v = self
                    .put_bool(a)
                    .map_err(|_| OmError::unable_to_set("Hidden", class))?;
                self.edit_scenario(b, s, i, |x| x.hidden = v)
            }
            ("Index", _) => Ok(Variant::I4(i as i32 + 1)),
            ("Parent", _) => Ok(self.alloc(Object::Worksheet(b, s))),
            ("Application", _) => Ok(Variant::Dispatch(Some(self.handle()))),
            ("ChangingCells", _) => {
                if sc.areas.is_empty() {
                    let rect = bounds(&sc.cells).ok_or_else(OmError::object_required)?;
                    return self.new_range(b, s, rect, Mode::Cells);
                }
                let areas = sc.areas.clone();
                self.new_multi(b, s, &areas, Mode::Cells)
            }
            ("Values", _) => {
                let texts: Vec<Variant> = sc
                    .cells
                    .iter()
                    .map(|c| Variant::Bstr(c.value.clone()))
                    .collect();
                let Some(index) = a.get(0) else {
                    return Ok(Variant::Array(Box::new(SafeArray {
                        elem: VarType::Variant,
                        bounds: vec![(1, texts.len() as u32)],
                        data: texts,
                    })));
                };
                let n = Args {
                    values: vec![Some(index.clone())],
                    put: None,
                    lcid: a.lcid,
                }
                .i32(0)
                .map_err(|_| unable_to_get("Values", class))?
                .unwrap_or(0);
                usize::try_from(n - 1)
                    .ok()
                    .and_then(|k| texts.get(k).cloned())
                    .ok_or_else(|| unable_to_get("Values", class))
            }
            ("Show", _) => {
                self.book_mut(b)?.wb.show_scenario(s, i);
                Ok(Variant::Bool(true))
            }
            ("Delete", _) => {
                let sheet = self.book_mut(b)?.wb.scenarios_mut(s);
                sheet.list.remove(i);
                let gone = u32::try_from(i).unwrap_or(u32::MAX);
                for slot in [&mut sheet.current, &mut sheet.show] {
                    match *slot {
                        Some(x) if x == gone => *slot = None,
                        Some(x) if x > gone => *slot = Some(x - 1),
                        _ => {}
                    }
                }
                Ok(Variant::Bool(true))
            }
            ("ChangeScenario", _) => {
                let (areas, cells) =
                    self.scenario_cells(b, s, a.get(0), a.get(1))
                        .ok_or_else(|| {
                            OmError::app_defined_with(
                                "ChangeScenario method of Scenario class failed",
                            )
                        })?;
                self.edit_scenario(b, s, i, |x| {
                    x.cells = cells;
                    x.areas = areas;
                })?;
                Ok(Variant::Bool(true))
            }
            _ => Err(unsupported(class, m)),
        }
    }
}

impl Application<'_> {
    pub(crate) fn ole_objects_member(
        &mut self,
        m: &'static Member,
        a: &Args,
    ) -> Result<Variant, OmError> {
        use InvokeKind::*;
        match (m.name, m.kind) {
            ("Count", Get) => Ok(Variant::I4(0)),
            ("Application", Get) => Ok(Variant::Dispatch(Some(self.handle()))),
            ("Creator", Get) => Ok(Variant::I4(0x5843_454C)),
            ("Add", Func) => Err(OmError::app_defined_with("Cannot insert object.")),
            ("_NewEnum", Get) => Ok(self.alloc_enumerator(Vec::new())),
            ("ShapeRange", Get) => Err(OmError::exception(0x8007_000E_u32 as i32, None)),
            ("SourceName", _) => Err(OmError::app_defined()),
            ("Border", Get) => Ok(self.alloc(Object::Stub("Border"))),
            ("Interior", Get) => Ok(self.alloc(Object::Stub("Interior"))),

            ("Shadow", Put)
                if matches!(
                    a.put,
                    Some(Variant::Bool(true) | Variant::I4(1..) | Variant::I4(..=-1))
                ) =>
            {
                Ok(Variant::Empty)
            }
            (name, Put | PutRef) => Err(OmError::app_defined_with(&format!(
                "Unable to set the {name} property of the OLEObjects class"
            ))),
            (name, _) => Err(OmError::app_defined_with(&format!(
                "Unable to get the {name} property of the OLEObjects class"
            ))),
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn formulas_and_objects_are_refused() {
        assert_eq!(value_text(&Variant::Bstr("=1+1".into())), None);
        assert_eq!(value_text(&Variant::I4(2)).as_deref(), Some("2"));
        assert_eq!(value_text(&Variant::R8(0.5)).as_deref(), Some("0.5"));
        assert_eq!(value_text(&Variant::Dispatch(None)), None);
    }
}
