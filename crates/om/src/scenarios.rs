use xlcalc::scenarios::{cells_of, Scenario, ScenarioCell, MAX_CHANGING_CELLS};
use xlcalc::{CellAddr, Rect, SheetId};
use xlcalc_variant::{SafeArray, VarType, Variant};

use crate::app::{unsupported, Application, BookId, Mode, Object};
use crate::conv::{array_cells, Args};
use crate::error::OmError;
use crate::typelib::{InvokeKind, Member};

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
                    comment: a.string(3).map_err(|_| add_failed())?.unwrap_or_default(),
                    cells,
                    areas,
                };
                self.book_mut(b)?.wb.scenarios_mut(s).list.push(scenario);
                Ok(self.alloc(Object::Scenario(b, s, name)))
            }
            _ => Err(unsupported("Scenarios", m)),
        }
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
                let taken = list
                    .iter()
                    .enumerate()
                    .any(|(j, x)| j != i && same_name(&x.name, &new));
                if new.is_empty() || new.chars().count() > 255 || taken {
                    return Err(OmError::unable_to_set("Name", class));
                }
                self.scenario_renamed(b, s, name, &new);
                self.edit_scenario(b, s, i, |x| x.name = new)
            }
            ("Comment", false) => Ok(Variant::Bstr(sc.comment.clone())),
            ("Comment", true) => {
                let text = self.put_string(a)?;
                self.edit_scenario(b, s, i, |x| x.comment = text)
            }
            ("Locked", false) => Ok(Variant::Bool(sc.locked)),
            ("Locked", true) => {
                let v = self.put_bool(a)?;
                self.edit_scenario(b, s, i, |x| x.locked = v)
            }
            ("Hidden", false) => Ok(Variant::Bool(sc.hidden)),
            ("Hidden", true) => {
                let v = self.put_bool(a)?;
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
