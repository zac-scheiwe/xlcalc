use serde_json::Value as J;
use xlcalc::Workbook;
use xlcalc_om::{flags, OmError};
use xlcalc_variant::{ObjectId, SafeArray, VarType, Variant};

use super::omsteps::{self, OmSession};
use super::tablesteps::find_table;
use super::{Outcome, Tally};

fn message(e: &OmError) -> String {
    e.detail.clone().unwrap_or_else(|| e.to_string())
}

fn obj(v: Variant) -> Result<ObjectId, OmError> {
    match v {
        Variant::Dispatch(Some(id)) | Variant::Unknown(Some(id)) => Ok(id),
        _ => Err(OmError::object_required()),
    }
}

fn worksheet(s: &mut OmSession, name: &str) -> Result<ObjectId, OmError> {
    let app = s.app_handle();
    s.object(app, "Worksheets", &[Variant::Bstr(name.to_string())])
}

fn split_ref(r: &str) -> Option<(String, String)> {
    let i = r.rfind('!')?;
    Some((
        r[..i].trim_matches('\'').replace("''", "'"),
        r[i + 1..].to_string(),
    ))
}

fn range_ref(s: &mut OmSession, r: &str) -> Result<ObjectId, OmError> {
    let (sheet, a1) = split_ref(r).ok_or_else(|| OmError::app_defined().with_detail("no sheet"))?;
    s.range(&sheet, &a1)
}

fn step_sheet(s: &mut OmSession, step: &J) -> Result<ObjectId, OmError> {
    let name = step["sheet"]
        .as_str()
        .or_else(|| {
            step["range"]
                .as_str()
                .and_then(|r| r.split_once('!'))
                .map(|x| x.0)
        })
        .ok_or_else(|| OmError::app_defined().with_detail("no sheet"))?;
    worksheet(s, name.trim_matches('\''))
}

fn span(s: &mut OmSession, step: &J, rows: bool) -> Result<ObjectId, OmError> {
    let ws = step_sheet(s, step)?;
    let at = step["at"].as_i64().unwrap_or(1);
    let last = at + step["count"].as_i64().unwrap_or(1) - 1;
    if rows {
        s.object(ws, "Rows", &[Variant::Bstr(format!("{at}:{last}"))])
    } else {
        let cell = |s: &mut OmSession, c: i64| {
            s.object(ws, "Cells", &[Variant::I4(1), Variant::I4(c as i32)])
        };
        let (a, b) = (cell(s, at)?, cell(s, last)?);
        let range = s.object(
            ws,
            "Range",
            &[Variant::Dispatch(Some(a)), Variant::Dispatch(Some(b))],
        )?;
        s.object(range, "EntireColumn", &[])
    }
}

fn criteria1(step: &J) -> Result<Variant, String> {
    let missing = Variant::Error(xlcalc_variant::HResult::DISP_E_PARAMNOTFOUND.0);
    let op = step["operator"].as_str().unwrap_or("");
    Ok(match &step["criteria1"] {
        J::Array(a) => {
            let data: Vec<Variant> = a
                .iter()
                .filter_map(|x| x.as_str())
                .map(|x| Variant::Bstr(x.to_string()))
                .collect();
            Variant::Array(Box::new(SafeArray {
                elem: VarType::Bstr,
                bounds: vec![(0, data.len() as u32)],
                data,
            }))
        }
        J::Null => missing,
        J::String(t) if matches!(op, "top10" | "bottom10" | "top10pct" | "bottom10pct") => {
            Variant::R8(t.trim().parse().map_err(|_| "bad number".to_string())?)
        }
        J::String(t) if op == "dynamic" => {
            Variant::I4(t.trim().parse().map_err(|_| "bad number".to_string())?)
        }
        J::String(t) => Variant::Bstr(t.clone()),
        J::Number(n) => Variant::R8(n.as_f64().unwrap_or(0.0)),
        _ => missing,
    })
}

fn autofilter(s: &mut OmSession, step: &J) -> Result<(), OmError> {
    let rng = match step["table"].as_str() {
        Some(t) => {
            let lo = find_table(s, t, None)?;
            obj(s.get(lo, "Range")?)?
        }
        None => range_ref(s, step["range"].as_str().unwrap_or(""))?,
    };
    let Some(field) = step["field"].as_i64() else {
        return s.method(rng, "AutoFilter").map(|_| ());
    };
    let missing = Variant::Error(xlcalc_variant::HResult::DISP_E_PARAMNOTFOUND.0);
    let c1 = criteria1(step).map_err(|e| OmError::app_defined().with_detail(e))?;
    let op = match step["operator"].as_str() {
        Some(o) => Variant::I4(match o {
            "and" => 1,
            "or" => 2,
            "top10" => 3,
            "bottom10" => 4,
            "top10pct" => 5,
            "bottom10pct" => 6,
            "values" => 7,
            "dynamic" => 11,
            _ => return Err(OmError::app_defined().with_detail("bad operator")),
        }),
        None => missing.clone(),
    };
    let c2 = match step["criteria2"].as_str() {
        Some(t) => Variant::Bstr(t.to_string()),
        None if step.get("criteria2").is_some() => Variant::Bstr(String::new()),
        None => missing,
    };
    s.call(
        rng,
        "AutoFilter",
        flags::METHOD,
        &[Variant::I4(field as i32), c1, op, c2],
    )
    .map(|_| ())
}

type RowState = (Vec<u32>, Vec<u32>, bool, Vec<u32>);

fn rowstate(s: &mut OmSession, step: &J) -> Result<RowState, OmError> {
    let ws = step_sheet(s, step)?;
    let rows_coll = obj(s.get(ws, "Rows")?)?;
    let cols_coll = obj(s.get(ws, "Columns")?)?;
    let nrows = step["rows"].as_u64().unwrap_or(20) as i32;
    let mut hr = Vec::new();
    let mut lv = Vec::new();
    for r in 1..=nrows {
        let row = s.object(rows_coll, "Item", &[Variant::I4(r)])?;
        if s.get(row, "Hidden")? == Variant::Bool(true) {
            hr.push(r as u32);
        }
        if let Variant::I4(l) = s.get(row, "OutlineLevel")? {
            lv.push(l as u32);
        }
    }
    let mut hc = Vec::new();
    for c in 1..=26 {
        let col = s.object(cols_coll, "Item", &[Variant::I4(c)])?;
        if s.get(col, "Hidden")? == Variant::Bool(true) {
            hc.push(c as u32);
        }
    }
    let filter = s.get(ws, "FilterMode")? == Variant::Bool(true);
    s.get(ws, "AutoFilterMode")?;
    Ok((hr, hc, filter, lv))
}

pub fn selinfo(om: &mut Option<OmSession>, wb: &mut Workbook) {
    let session = om.get_or_insert_with(|| OmSession::new(wb));
    session
        .with(wb, |s| {
            let wbk = s.workbook()?;
            let sheet = obj(s.get(wbk, "ActiveSheet")?)?;
            s.get(sheet, "Name")?;
            let app = s.app_handle();
            for member in ["ActiveCell", "Selection"] {
                if let Ok(r) = obj(s.get(app, member)?) {
                    let _ = s.address(r);
                }
            }
            Ok::<(), OmError>(())
        })
        .ok();
}

pub fn run(
    om: &mut Option<OmSession>,
    wb: &mut Workbook,
    op: &str,
    step: &J,
    rec: &J,
    n: usize,
    tally: &mut Tally,
) -> bool {
    let session = om.get_or_insert_with(|| OmSession::new(wb));
    let on = |v: bool| Variant::Bool(v);
    let err: Option<String> = match op {
        "hide_rows" | "unhide_rows" | "hide_cols" | "unhide_cols" => session
            .with(wb, |s| {
                let target = span(s, step, op.ends_with("rows"))?;
                s.put(target, "Hidden", on(op.starts_with("hide")))
            })
            .err()
            .map(|e| message(&e)),
        "autofilter" => session
            .with(wb, |s| autofilter(s, step))
            .err()
            .map(|e| message(&e)),
        "autofilter_clear" => session
            .with(wb, |s| {
                let ws = step_sheet(s, step)?;
                s.method(ws, "ShowAllData").map(|_| ())
            })
            .err()
            .map(|e| message(&e)),
        "autofilter_reapply" => session
            .with(wb, |s| {
                let af = match step["table"].as_str() {
                    Some(t) => {
                        let lo = find_table(s, t, None)?;
                        s.get(lo, "AutoFilter")?
                    }
                    None => {
                        let ws = step_sheet(s, step)?;
                        s.get(ws, "AutoFilter")?
                    }
                };

                let af = obj(af).map_err(|_| {
                    OmError::object_required()
                        .with_detail("You cannot call a method on a null-valued expression.")
                })?;
                s.method(af, "ApplyFilter").map(|_| ())
            })
            .err()
            .map(|e| message(&e)),
        "autofilter_off" => session
            .with(wb, |s| match step["table"].as_str() {
                Some(t) => {
                    let lo = find_table(s, t, None)?;
                    s.put(lo, "ShowAutoFilter", on(false))
                }
                None => {
                    let ws = step_sheet(s, step)?;
                    s.put(ws, "AutoFilterMode", on(false))
                }
            })
            .err()
            .map(|e| message(&e)),
        "merge" => session
            .with(wb, |s| {
                let rng = range_ref(s, step["range"].as_str().unwrap_or(""))?;
                let across = step["across"].as_bool().unwrap_or(false);
                s.call(rng, "Merge", flags::METHOD, &[on(across)])
                    .map(|_| ())
            })
            .err()
            .map(|e| message(&e)),
        "unmerge" => {
            let _ = session.with(wb, |s| {
                let rng = range_ref(s, step["range"].as_str().unwrap_or(""))?;
                s.method(rng, "UnMerge").map(|_| ())
            });
            return true;
        }
        "rowstate" => {
            let Ok((hid, hc, filter, lvls)) = session.with(wb, |s| rowstate(s, step)) else {
                return true;
            };
            let ints = |k: &str| -> Vec<u32> {
                rec[k]
                    .as_array()
                    .into_iter()
                    .flatten()
                    .filter_map(|v| v.as_u64())
                    .map(|v| v as u32)
                    .collect()
            };
            let checks: [(&str, bool, String); 4] = [
                (
                    "outline levels",
                    rec["levels"].is_null() || lvls == ints("levels"),
                    format!("{:?} vs Excel {}", lvls, rec["levels"]),
                ),
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
                    "filter mode",
                    filter == rec["filter_mode"].as_bool().unwrap_or(false),
                    format!("{} vs Excel {}", filter, rec["filter_mode"]),
                ),
            ];
            for (what, ok, msg) in checks {
                tally.add(if ok { Outcome::Exact } else { Outcome::Wrong }, || {
                    format!("step {n}: rowstate {what}: {msg}")
                });
            }
            return true;
        }
        _ => return false,
    };
    omsteps::verdict(tally, op, rec, n, err);
    true
}
