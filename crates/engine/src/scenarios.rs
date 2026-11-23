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
            Value::Number(n) => crate::number::format_general(n),
            Value::Text(t) => t.to_string(),
            Value::Bool(true) => "TRUE".into(),
            Value::Bool(false) => "FALSE".into(),
            _ => String::new(),
        }
    }

    pub(crate) fn scenario_constant(&self, text: &str) -> Value {
        match crate::input::interpret(text, &self.input_env()) {
            crate::input::Entered::Value(v) => v,
            crate::input::Entered::Clear => Value::Blank,
            crate::input::Entered::Formula(_) => Value::text(text),
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
                    let v = wb.scenario_constant(&c.value);
                    wb.set_value_run(CellAddr::new(sheet, c.row, c.col), v);
                }
            })
        });
        let s = self.scenarios_mut(sheet);
        s.current = u32::try_from(index).ok();
        s.show = s.current;
        true
    }

    fn write_scenario_texts(&mut self, sheet: SheetId, cells: &[ScenarioCell]) {
        self.command(Command::Scenario, |wb| {
            wb.one_pass(|wb| {
                for c in cells {
                    let v = wb.scenario_constant(&c.value);
                    wb.set_value_run(CellAddr::new(sheet, c.row, c.col), v);
                }
            })
        });
    }

    pub fn run_scenario_summary(
        &mut self,
        sheet: SheetId,
        results: &[CellAddr],
    ) -> Option<ScenarioSummary> {
        let list = self.scenarios.get(&sheet)?.list.clone();
        if list.is_empty() {
            return None;
        }
        let mut cells: Vec<(u32, u32)> = Vec::new();
        for sc in &list {
            for c in &sc.cells {
                if !cells.contains(&(c.row, c.col)) {
                    cells.push((c.row, c.col));
                }
            }
        }
        let current: Vec<ScenarioCell> = cells
            .iter()
            .map(|&(row, col)| ScenarioCell {
                row,
                col,
                value: self.scenario_text(CellAddr::new(sheet, row, col)),
            })
            .collect();
        let read = |wb: &Workbook| results.iter().map(|a| wb.value(*a)).collect::<Vec<_>>();

        let constants: Vec<ScenarioCell> = current
            .iter()
            .filter(|c| {
                self.formula_at(CellAddr::new(sheet, c.row, c.col))
                    .is_none()
            })
            .cloned()
            .collect();
        self.write_scenario_texts(sheet, &constants);
        let current_results = read(self);
        let mut values = Vec::new();
        let mut shown = Vec::new();
        for sc in &list {
            values.push(
                current
                    .iter()
                    .map(|c| {
                        let own = sc.cells.iter().find(|x| x.row == c.row && x.col == c.col);
                        self.scenario_constant(&own.unwrap_or(c).value)
                    })
                    .collect(),
            );
            self.write_scenario_texts(sheet, &sc.cells);
            shown.push(read(self));
            self.write_scenario_texts(sheet, &current);
        }
        Some(ScenarioSummary {
            cells,
            current: current
                .iter()
                .map(|c| self.scenario_constant(&c.value))
                .collect(),
            values,
            current_results,
            results: shown,
        })
    }

    pub fn run_scenario_pivot(
        &mut self,
        sheet: SheetId,
        results: &[CellAddr],
    ) -> Option<ScenarioPivot> {
        let list = self.scenarios.get(&sheet)?.list.clone();
        if list.is_empty() {
            return None;
        }
        let mut groups: Vec<(String, Vec<usize>)> = Vec::new();
        for (i, sc) in list.iter().enumerate() {
            let key = scenario_field_name(sc);
            match groups.iter_mut().find(|g| g.0 == key) {
                Some(g) => g.1.push(i),
                None => groups.push((key, vec![i])),
            }
        }
        let n = groups.len();
        let read = |wb: &Workbook| results.iter().map(|a| wb.value(*a)).collect::<Vec<_>>();
        let show = |wb: &mut Workbook, s: usize| wb.write_scenario_texts(sheet, &list[s].cells);

        let mut cur = vec![0usize; n];
        let mut label: Vec<usize> = (0..n).collect();
        for g in &groups {
            show(self, g.1[0]);
        }
        show(self, groups[n - 1].1[0]);
        label[n - 1] = groups[n - 1].1[0];
        let mut records = vec![(label.clone(), read(self))];
        loop {
            let mut g = n;
            let advanced = loop {
                if g == 0 {
                    break false;
                }
                g -= 1;
                let items = &groups[g].1;
                if cur[g] + 1 < items.len() {
                    cur[g] += 1;
                    label[g] = items[cur[g]];
                    show(self, label[g]);
                    break true;
                }
                cur[g] = 0;
                label[g] = items[0];
                show(self, label[g]);
            };
            if !advanced {
                break;
            }
            records.push((label.clone(), read(self)));
        }
        Some(ScenarioPivot {
            fields: groups.into_iter().map(|g| g.0).collect(),
            names: list.into_iter().map(|s| s.name).collect(),
            records,
        })
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

#[derive(Clone, Debug)]
pub struct ScenarioSummary {
    pub cells: Vec<(u32, u32)>,

    pub current: Vec<Value>,

    pub values: Vec<Vec<Value>>,

    pub current_results: Vec<Value>,

    pub results: Vec<Vec<Value>>,
}

#[derive(Clone, Debug)]
pub struct ScenarioPivot {
    pub fields: Vec<String>,

    pub names: Vec<String>,

    pub records: Vec<(Vec<usize>, Vec<Value>)>,
}

fn scenario_field_name(sc: &Scenario) -> String {
    let cell = |r: u32, c: u32| format!("${}${}", crate::address::col_to_letters(c), r + 1);
    let areas: Vec<Rect> = if sc.areas.is_empty() {
        let rows = sc.cells.iter().map(|c| c.row);
        let cols = sc.cells.iter().map(|c| c.col);
        match (
            rows.clone().min(),
            rows.max(),
            cols.clone().min(),
            cols.max(),
        ) {
            (Some(r0), Some(r1), Some(c0), Some(c1)) => vec![Rect::new(r0, c0, r1, c1)],
            _ => Vec::new(),
        }
    } else {
        sc.areas.clone()
    };
    let addr: Vec<String> = areas
        .iter()
        .map(|r| {
            if r.r0 == r.r1 && r.c0 == r.c1 {
                cell(r.r0, r.c0)
            } else {
                format!("{}:{}", cell(r.r0, r.c0), cell(r.r1, r.c1))
            }
        })
        .collect();
    let mut name = format!("{} by", addr.join(","));
    if !sc.user.is_empty() {
        name.push(' ');
        name.push_str(&sc.user);
    }
    name
}

pub fn cells_of(r: Rect) -> impl Iterator<Item = (u32, u32)> {
    (r.r0..=r.r1).flat_map(move |row| (r.c0..=r.c1).map(move |col| (row, col)))
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::Notice;

    #[test]
    fn pivot_walk_turns_the_last_group_fastest() {
        let mut wb = Workbook::new();
        let s = wb.sheet_at(0).id;
        let at = |r, c| CellAddr::new(s, r, c);
        wb.set_input(at(0, 5), "=A1*1000+A2*100+A3*10").unwrap();
        for (i, name) in ["a1", "a2", "b1", "b2", "c1", "c2"].into_iter().enumerate() {
            let row = (i / 2) as u32;
            wb.scenarios_mut(s).list.push(Scenario {
                name: name.into(),
                locked: true,
                hidden: false,
                user: String::new(),
                comment: String::new(),
                cells: vec![ScenarioCell {
                    row,
                    col: 0,
                    value: format!("{}", i % 2 + 1),
                }],
                areas: vec![Rect::cell(row, 0)],
            });
        }
        let run = wb.run_scenario_pivot(s, &[at(0, 5)]).unwrap();
        assert_eq!(run.fields, ["$A$1 by", "$A$2 by", "$A$3 by"]);
        let got: Vec<(Vec<usize>, f64)> = run
            .records
            .iter()
            .map(|(l, v)| match v[0] {
                Value::Number(n) => (l.clone(), n),
                _ => panic!("{v:?}"),
            })
            .collect();
        let want: Vec<(Vec<usize>, f64)> = vec![
            (vec![0, 1, 4], 1110.0),
            (vec![0, 1, 5], 1120.0),
            (vec![0, 3, 4], 1210.0),
            (vec![0, 3, 5], 1220.0),
            (vec![1, 2, 4], 2110.0),
            (vec![1, 2, 5], 2120.0),
            (vec![1, 3, 4], 2210.0),
            (vec![1, 3, 5], 2220.0),
        ];
        assert_eq!(got, want);

        for r in 0..3 {
            assert!(matches!(wb.value(at(r, 0)), Value::Number(n) if n == 1.0));
        }
    }

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
