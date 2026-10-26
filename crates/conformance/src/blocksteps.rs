use serde_json::Value as J;
use xlcalc::{parse_cell_ref, parse_range_ref, CellAddr, SheetId, Workbook};

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
    let verdict = |tally: &mut Tally, err: Option<String>| {
        let ok = err.is_some() == excel_err;
        tally.add(if ok { Outcome::Exact } else { Outcome::Wrong }, || {
            format!(
                "step {n}: {op} — Excel error {:?}, engine {err:?}",
                rec["error"]
            )
        });
    };
    let range = |wb: &Workbook, k: &str| {
        parse_range_ref(wb, step[k].as_str().unwrap(), default_sheet).unwrap()
    };
    match op {
        "array" => {
            let (s, rect) = range(wb, "range");
            let err = wb
                .set_array_formula(s, rect, step["input"].as_str().unwrap())
                .err()
                .map(|e| e.to_string());
            verdict(tally, err);
        }
        "setl" => {
            let a = parse_cell_ref(wb, step["cell"].as_str().unwrap(), default_sheet).unwrap();

            let err = match wb.set_legacy_input(a, step["input"].as_str().unwrap()) {
                Err(xlcalc::EditError::Locked) if !excel_err => None,
                r => r.err().map(|e| e.to_string()),
            };
            verdict(tally, err);
        }
        "merge" => {
            let (s, rect) = range(wb, "range");
            let across = step["across"].as_bool().unwrap_or(false);
            let err = wb.merge_cells(s, rect, across).err().map(|e| e.to_string());
            verdict(tally, err);
        }
        "unmerge" => {
            let (s, rect) = range(wb, "range");
            wb.unmerge_cells(s, rect);
        }
        "read_text" => {
            let (s, _) = range(wb, "range");
            for c in rec["cells"].as_array().into_iter().flatten() {
                let a1 = c["cell"].as_str().unwrap();
                let addr = parse_cell_ref(wb, a1, s).unwrap();

                let constant = || match (wb.formula_at(addr), wb.value(addr)) {
                    (None, xlcalc::Value::Text(t)) => t.to_string(),
                    _ => String::new(),
                };
                let got2 = wb.formula_text(addr).unwrap_or_else(constant);
                let got1 = wb.legacy_formula_text(addr).unwrap_or_else(constant);
                for (what, want, got) in [
                    ("Formula2", c["formula2"].as_str().unwrap_or(""), got2),
                    ("Formula", c["formula"].as_str().unwrap_or(""), got1),
                ] {
                    if !want.starts_with('=') && !got.starts_with('=') {
                        continue;
                    }
                    let want = super::sheetsteps::norm_book(want);
                    let got = super::sheetsteps::norm_book(&got);
                    let o = if want == got {
                        Outcome::Exact
                    } else {
                        Outcome::Wrong
                    };
                    tally.add(o, || {
                        format!("step {n}: {a1} {what} expected {want:?}, got {got:?}")
                    });
                }
                check_blocks(wb, c, addr, n, tally);
            }
        }
        _ => return false,
    }
    true
}

pub fn check_blocks(wb: &Workbook, c: &J, addr: CellAddr, n: usize, tally: &mut Tally) {
    let a1 = c["cell"].as_str().unwrap_or("");
    let want_a = c["array"].as_str().map(str::to_string);
    let got_a = wb.current_array(addr).map(|b| b.a1());
    if want_a.is_some() || got_a.is_some() {
        let o = if want_a == got_a {
            Outcome::Exact
        } else {
            Outcome::Wrong
        };
        tally.add(o, || {
            format!("step {n}: {a1} array expected {want_a:?}, got {got_a:?}")
        });
    }
    let want_m = c["merge"].as_str().map(str::to_string);
    let got_m = wb.merge_at(addr).map(|m| m.a1());
    if want_m.is_some() || got_m.is_some() {
        let o = if want_m == got_m {
            Outcome::Exact
        } else {
            Outcome::Wrong
        };
        tally.add(o, || {
            format!("step {n}: {a1} merge expected {want_m:?}, got {got_m:?}")
        });
    }
}
