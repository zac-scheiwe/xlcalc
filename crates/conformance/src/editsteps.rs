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
            let name = step["name"].as_str().unwrap();
            if wb.table_by_name(name).is_none() {
                verdict(tally, Some("no such table".into()));
                return true;
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
            let err = wb.sort_table(name, &spec).err().map(|e| e.to_string());
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
        "delete_name" => {
            let scope = step["scope"]
                .as_str()
                .and_then(|n| wb.sheet_by_name(n).map(|x| x.id));
            let missing = step["scope"].as_str().is_some() && scope.is_none();
            let err = if missing
                || wb
                    .delete_name_active(step["name"].as_str().unwrap(), scope)
                    .is_none()
            {
                Some("no such name".to_string())
            } else {
                None
            };
            verdict(tally, err);
        }
        "clear_formats" | "clear_all" => {
            let (s, rect) =
                parse_range_ref(wb, step["range"].as_str().unwrap(), default_sheet).unwrap();
            let mut err = None;
            if op == "clear_all" {
                err = wb.clear_range(s, rect).err().map(|e| e.to_string());
            }
            if err.is_none() {
                if op == "clear_formats" {
                    wb.clear_formats(s, rect);
                } else {
                    wb.reset_number_format(s, rect);
                }
            }
            verdict(tally, err);
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
        "hide_rows" | "unhide_rows" | "hide_cols" | "unhide_cols" | "group_rows"
        | "ungroup_rows" | "outline_show" | "autofilter" | "autofilter_clear"
        | "autofilter_reapply" | "autofilter_off" => {
            let Some(sh) = step["sheet"]
                .as_str()
                .or_else(|| {
                    step["range"]
                        .as_str()
                        .and_then(|r| r.split_once('!'))
                        .map(|x| x.0)
                })
                .or_else(|| {
                    step["table"]
                        .as_str()
                        .and_then(|t| wb.table_by_name(t))
                        .map(|_| "")
                })
            else {
                verdict(tally, Some("no sheet".into()));
                return true;
            };
            let sid = if sh.is_empty() {
                wb.table_by_name(step["table"].as_str().unwrap())
                    .map(|t| t.sheet)
            } else {
                wb.sheet_by_name(sh.trim_matches('\'')).map(|s| s.id)
            };
            let Some(sid) = sid else {
                verdict(tally, Some("no such sheet".into()));
                return true;
            };
            let at = step["at"].as_u64().unwrap_or(1) as u32 - 1;
            let count = step["count"].as_u64().unwrap_or(1) as u32;
            let r = match op {
                "hide_rows" => wb.set_rows_hidden(sid, at, count, true).map(|_| ()),
                "unhide_rows" => wb.set_rows_hidden(sid, at, count, false).map(|_| ()),
                "hide_cols" => Ok(wb.set_cols_hidden(sid, at, count, true)),
                "unhide_cols" => Ok(wb.set_cols_hidden(sid, at, count, false)),
                "group_rows" => wb.group_rows(sid, at, count, true).map(|_| ()),
                "ungroup_rows" => wb.group_rows(sid, at, count, false).map(|_| ()),
                "outline_show" => {
                    wb.outline_show(sid, step["level"].as_u64().unwrap_or(1) as u32);
                    Ok(())
                }
                "autofilter" => {
                    let table = step["table"].as_str();
                    let rect = match step["range"].as_str() {
                        Some(r) => parse_range_ref(wb, r, default_sheet).map(|x| x.1),
                        None => Some(xlcalc::Rect::new(0, 0, 0, 0)),
                    };
                    let joined: Option<String> = match &step["criteria1"] {
                        serde_json::Value::Array(a) => Some(
                            a.iter()
                                .filter_map(|x| x.as_str())
                                .collect::<Vec<_>>()
                                .join(&xlcalc::view::VALUES_SEP.to_string()),
                        ),
                        x => x.as_str().map(str::to_string),
                    };
                    match (rect, step["field"].as_u64()) {
                        (Some(rect), Some(f)) => wb
                            .autofilter(
                                sid,
                                rect,
                                table,
                                f as u32,
                                joined.as_deref(),
                                step["operator"].as_str(),
                                step["criteria2"].as_str(),
                            )
                            .map(|_| ()),
                        (Some(rect), None) if table.is_none() => {
                            wb.autofilter_toggle(sid, rect).map(|_| ())
                        }
                        _ => Err(xlcalc::EditError::Compile("bad autofilter".into())),
                    }
                }
                "autofilter_clear" => wb.show_all_data(sid).map(|_| ()),
                "autofilter_reapply" => wb.reapply_filter(sid).map(|_| ()),
                _ => wb.autofilter_off(sid, step["table"].as_str()).map(|_| ()),
            };
            verdict(tally, r.err().map(|e| e.to_string()));
        }
        "rowstate" => {
            let Some(sid) = step["sheet"]
                .as_str()
                .and_then(|n| wb.sheet_by_name(n))
                .map(|s| s.id)
            else {
                return true;
            };
            let rows = step["rows"].as_u64().unwrap_or(20) as u32;
            let ints = |k: &str| -> Vec<u32> {
                rec[k]
                    .as_array()
                    .into_iter()
                    .flatten()
                    .filter_map(|v| v.as_u64())
                    .map(|v| v as u32)
                    .collect()
            };
            let hid: Vec<u32> = wb
                .hidden_rows(sid)
                .into_iter()
                .filter(|r| *r < rows)
                .map(|r| r + 1)
                .collect();
            let hc: Vec<u32> = wb
                .hidden_cols(sid)
                .into_iter()
                .filter(|c| *c < 26)
                .map(|c| c + 1)
                .collect();
            let lv: Vec<u32> = (0..rows).map(|r| wb.row_level(sid, r)).collect();
            let checks: [(&str, bool, String); 4] = [
                (
                    "hidden rows",
                    hid == ints("hidden_rows"),
                    format!("{:?} vs Excel {:?}", hid, ints("hidden_rows")),
                ),
                (
                    "hidden cols",
                    hc == ints("hidden_cols"),
                    format!("{:?} vs Excel {:?}", hc, ints("hidden_cols")),
                ),
                (
                    "outline levels",
                    lv == ints("levels"),
                    format!("{:?} vs Excel {:?}", lv, ints("levels")),
                ),
                (
                    "filter mode",
                    wb.filter_mode(sid) == rec["filter_mode"].as_bool().unwrap_or(false),
                    format!("{} vs Excel {}", wb.filter_mode(sid), rec["filter_mode"]),
                ),
            ];
            for (what, ok, msg) in checks {
                tally.add(if ok { Outcome::Exact } else { Outcome::Wrong }, || {
                    format!("step {n}: rowstate {what}: {msg}")
                });
            }
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
