use crate::Command;
use crate::{CellAddr, Rect, SheetId, Value, Workbook};

pub const MAX_CHANGING_CELLS: usize = 32;

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct ScenarioCell {
    pub row: u32,

    pub col: u32,

    pub value: String,
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct Scenario {
    pub name: String,

    pub locked: bool,

    pub hidden: bool,

    pub user: String,

    pub comment: String,

    pub cells: Vec<ScenarioCell>,

    pub areas: Vec<Rect>,
}

#[derive(Clone, Debug, Default, PartialEq, Eq)]
pub struct SheetScenarios {
    pub list: Vec<Scenario>,

    pub current: Option<u32>,

    pub show: Option<u32>,

    pub sqref: Option<String>,
}

impl SheetScenarios {
    pub fn position(&self, name: &str) -> Option<usize> {
        self.list
            .iter()
            .position(|s| crate::text::eq_ordinal_ci(&s.name, name))
    }
}

fn constant(text: &str) -> Value {
    match text.trim().parse::<f64>() {
        Ok(n) if n.is_finite() => Value::Number(n),
        _ => Value::text(text),
    }
}

impl Workbook {
    pub fn scenarios(&self, sheet: SheetId) -> Option<&SheetScenarios> {
        self.scenarios.get(&sheet)
    }

    pub fn scenarios_mut(&mut self, sheet: SheetId) -> &mut SheetScenarios {
        self.scenarios.entry(sheet).or_default()
    }

    pub fn set_scenarios(&mut self, sheet: SheetId, s: SheetScenarios) {
        if s.list.is_empty() {
            self.scenarios.remove(&sheet);
        } else {
            self.scenarios.insert(sheet, s);
        }
    }

    pub fn scenario_text(&self, addr: CellAddr) -> String {
        match self.value(addr) {
            Value::Number(n) => n.to_string(),
            Value::Text(t) => t.to_string(),
            Value::Bool(true) => "TRUE".into(),
            Value::Bool(false) => "FALSE".into(),
            _ => String::new(),
        }
    }

    pub fn show_scenario(&mut self, sheet: SheetId, index: usize) -> bool {
        let Some(sc) = self
            .scenarios
            .get(&sheet)
            .and_then(|s| s.list.get(index))
            .cloned()
        else {
            return false;
        };

        self.command(Command::Scenario, |wb| {
            wb.one_pass(|wb| {
                for c in &sc.cells {
                    wb.set_value_run(CellAddr::new(sheet, c.row, c.col), constant(&c.value));
                }
            })
        });
        let s = self.scenarios_mut(sheet);
        s.current = u32::try_from(index).ok();
        s.show = s.current;
        true
    }

    pub(crate) fn drop_sheet_scenarios(&mut self, sheet: SheetId) {
        self.scenarios.remove(&sheet);
    }

    pub(crate) fn copy_sheet_scenarios(&mut self, src: SheetId, new: SheetId) {
        if let Some(s) = self.scenarios.get(&src).cloned() {
            self.scenarios.insert(new, s);
        }
    }
}

pub fn cells_of(r: Rect) -> impl Iterator<Item = (u32, u32)> {
    (r.r0..=r.r1).flat_map(move |row| (r.c0..=r.c1).map(move |col| (row, col)))
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::Notice;

    #[test]
    fn show_writes_constants_and_gives_no_change() {
        let mut wb = Workbook::new();
        let s = wb.sheet_at(0).id;
        let at = |r, c| CellAddr::new(s, r, c);
        wb.set_input(at(0, 1), "=A1*2").unwrap();
        wb.set_input(at(0, 0), "=1+1").unwrap();
        let cell = |row, col, value: &str| ScenarioCell {
            row,
            col,
            value: value.into(),
        };
        wb.scenarios_mut(s).list.push(Scenario {
            name: "One".into(),
            locked: true,
            hidden: false,
            user: String::new(),
            comment: String::new(),
            cells: vec![cell(0, 0, "5"), cell(1, 0, "x")],
            areas: Vec::new(),
        });
        wb.take_notices();
        assert!(wb.show_scenario(s, 0));
        assert!(matches!(wb.value(at(0, 1)), Value::Number(n) if n == 10.0));
        assert!(matches!(wb.value(at(1, 0)), Value::Text(t) if &*t == "x"));
        assert!(wb.formula_at(at(0, 0)).is_none());
        let n = wb.take_notices();
        assert_eq!(
            n,
            [
                Notice::Calculated { sheets: vec![s] },
                Notice::AfterCalculate
            ]
        );
        assert_eq!(wb.scenarios(s).and_then(|x| x.show), Some(0));
        assert!(!wb.show_scenario(s, 3));
    }
}
