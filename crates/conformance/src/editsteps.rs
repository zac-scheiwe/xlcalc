use serde_json::Value as J;
use xlcalc::sort::{SortKey, SortSpec};
use xlcalc::{parse_cell_ref, parse_range_ref, SheetId, Workbook};

use super::{Outcome, Tally};

pub fn run(
    wb: &mut Workbook,
    op: &str,
    step: &J,
    default_sheet: SheetId,
    rec: &J,
    n: usize,
    tally: &mut Tally,
) -> bool {
    let excel_err = !rec["error"].is_null();
    let mut verdict = |tally: &mut Tally, err: Option<String>| {
        let ok = err.is_some() == excel_err;
        tally.add(if ok { Outcome::Exact } else { Outcome::Wrong }, || {
            format!(
                "step {n}: {op} — Excel error {:?}, engine {err:?}",
                rec["error"]
            )
        });
    };
    match op {
        "data_table" => {
            let (s, rect) =
                parse_range_ref(wb, step["range"].as_str().unwrap(), default_sheet).unwrap();

            let mut bad = false;
            let mut input = |k: &str| {
                let (is, r) = parse_range_ref(wb, step[k].as_str()?, default_sheet)?;
                bad |= !r.is_cell();
                Some(xlcalc::CellAddr::new(is, r.r0, r.c0))
            };
            let (ri, ci) = (input("row_input"), input("col_input"));
            let err = if bad {
                Some("Input cell reference is not valid.".to_string())
            } else {
                wb.add_data_table(s, rect, ri, ci)
                    .err()
                    .map(|e| e.to_string())
            };
            verdict(tally, err);
        }
        "goal_seek" => {
            let cell = |k: &str| {
                let (s, r) = parse_range_ref(wb, step[k].as_str().unwrap(), default_sheet).unwrap();
                xlcalc::CellAddr::new(s, r.r0, r.c0)
            };
            let (target, changing) = (cell("cell"), cell("changing"));
            let goal = step["goal"].as_f64().unwrap();
            match wb.goal_seek(target, goal, changing) {
                Ok(got) if !excel_err => {
                    let want = rec["result"].as_bool();
                    tally.add(
                        if want == Some(got) {
                            Outcome::Exact
                        } else {
                            Outcome::Wrong
                        },
                        || format!("step {n}: goal_seek — Excel {want:?}, engine {got}"),
                    );
                }
                r => verdict(tally, r.err().map(|e| e.to_string())),
            }
        }
        "sort" => {
            let (s, rect) =
                parse_range_ref(wb, step["range"].as_str().unwrap(), default_sheet).unwrap();
            let spec = SortSpec {
                keys: step["keys"]
                    .as_array()
                    .unwrap()
                    .iter()
                    .map(|k| SortKey {
                        index: k["key"].as_u64().unwrap() as u32 - 1,
                        descending: k["order"].as_str() == Some("desc"),
                    })
                    .collect(),
                header: step["header"].as_bool().unwrap_or(false),
                by_columns: step["orient"].as_str() == Some("cols"),
                match_case: step["matchcase"].as_bool().unwrap_or(false),
            };
            let err = wb.sort_range(s, rect, &spec).err().map(|e| e.to_string());
            verdict(tally, err);
        }
        "table_sort" => {
            let Some(t) = wb.table_by_name(step["name"].as_str().unwrap()) else {
                verdict(tally, Some("no such table".into()));
                return true;
            };
            let (s, mut rect) = (t.sheet, t.rect);
            rect.r0 += 1;
            if t.totals_row().is_some() {
                rect.r1 -= 1;
            }
            let spec = SortSpec {
                keys: step["keys"]
                    .as_array()
                    .unwrap()
                    .iter()
                    .map(|k| SortKey {
                        index: k["key"].as_u64().unwrap() as u32 - 1,
                        descending: k["order"].as_str() == Some("desc"),
                    })
                    .collect(),
                header: false,
                by_columns: false,
                match_case: step["matchcase"].as_bool().unwrap_or(false),
            };
            let err = wb.sort_range(s, rect, &spec).err().map(|e| e.to_string());
            verdict(tally, err);
        }
        "paste_special" => {
            use xlcalc::pastespecial::{PasteOp, PasteSpecial, PasteWhat};
            let (ss, src) =
                parse_range_ref(wb, step["from"].as_str().unwrap(), default_sheet).unwrap();
            let (ds, dst) =
                parse_range_ref(wb, step["to"].as_str().unwrap(), default_sheet).unwrap();
            let spec = PasteSpecial {
                what: match step["paste"].as_str().unwrap_or("all") {
                    "values" => PasteWhat::Values,
                    "formulas" => PasteWhat::Formulas,
                    "formats" => PasteWhat::Formats,
                    _ => PasteWhat::All,
                },
                op: match step["operation"].as_str().unwrap_or("none") {
                    "add" => PasteOp::Add,
                    "sub" => PasteOp::Sub,
                    "mul" => PasteOp::Mul,
                    "div" => PasteOp::Div,
                    _ => PasteOp::None,
                },
                skip_blanks: step["skip_blanks"].as_bool().unwrap_or(false),
                transpose: step["transpose"].as_bool().unwrap_or(false),
            };
            let err = wb
                .paste_special(ss, src, ds, dst, &spec)
                .err()
                .map(|e| e.to_string());
            verdict(tally, err);
        }
        "format" => {
            let (s, rect) =
                parse_range_ref(wb, step["range"].as_str().unwrap(), default_sheet).unwrap();
            wb.set_number_format(s, rect, step["fmt"].as_str().unwrap());
        }
        "autofill" => {
            use xlcalc::autofill::FillType;
            let (ss, src) =
                parse_range_ref(wb, step["from"].as_str().unwrap(), default_sheet).unwrap();
            let (_, dst) =
                parse_range_ref(wb, step["to"].as_str().unwrap(), default_sheet).unwrap();
            let ty = Some(
                FillType::parse(step["type"].as_str().unwrap_or("default"))
                    .unwrap_or(FillType::Default),
            );
            let err = match ty {
                Some(ty) => wb.autofill(ss, src, dst, ty).err().map(|e| e.to_string()),
                None => Some("bad fill type".into()),
            };
            verdict(tally, err);
        }
        "dataseries" => {
            use xlcalc::fillseries::{DateUnit, SeriesSpec, SeriesType};
            let (s, rect) =
                parse_range_ref(wb, step["range"].as_str().unwrap(), default_sheet).unwrap();
            let spec = SeriesSpec {
                rows: step["rowcol"].as_str().map(|r| r == "rows"),
                ty: step["type"].as_str().and_then(SeriesType::parse),
                unit: step["date"].as_str().and_then(DateUnit::parse),
                step: step["step"].as_f64(),
                stop: step["stop"].as_f64(),
                trend: step["trend"].as_bool().unwrap_or(false),
            };
            let err = wb.data_series(s, rect, &spec).err().map(|e| e.to_string());
            verdict(tally, err);
        }
        "precision" => {
            wb.set_precision_as_displayed(step["value"].as_bool().unwrap_or(true));
        }
        _ => {
            let _ = (parse_cell_ref, &mut verdict);
            return false;
        }
    }
    true
}
