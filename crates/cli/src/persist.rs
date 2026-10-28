use serde_json::{json, Value as J};
use xlcalc::{CalcMode, CellAddr, Entry, Value, Workbook, XlError};

fn typed(v: &Value) -> J {
    match v {
        Value::Number(n) => json!({"t": "n", "v": n}),
        Value::Text(s) => json!({"t": "s", "v": &**s}),
        Value::Bool(b) => json!({"t": "b", "v": b}),
        Value::Error(e) => json!({"t": "e", "v": e.as_str()}),
        _ => json!({"t": "blank"}),
    }
}

pub fn save(wb: &Workbook) -> J {
    let sheets: Vec<J> = wb
        .sheets()
        .map(|s| {
            let cells: Vec<J> = s
                .entries()
                .map(|(r, c, e)| {
                    let a = CellAddr::new(s.id, r, c);
                    let v = match e {
                        Entry::Const(v) => typed(v),
                        Entry::Formula(_) => {
                            json!({"t": "f", "v": wb.formula_text(a).unwrap_or_default()})
                        }
                    };
                    json!([a.a1(), v])
                })
                .collect();
            json!({"name": s.name, "cells": cells})
        })
        .collect();
    let name_of = |id| wb.sheet(id).map(|s| s.name.clone());
    let names: Vec<J> = wb
        .names()
        .iter()
        .map(|n| json!({"name": n.name, "refers_to": format!("={}", n.refers_to), "scope": n.scope.and_then(name_of)}))
        .collect();
    let tables: Vec<J> = wb
        .tables()
        .iter()
        .map(|t| json!({"name": t.name, "sheet": name_of(t.sheet), "range": t.rect.a1(), "totals": t.totals, "headers": t.headers, "columns": t.columns}))
        .collect();
    let s = wb.settings();
    json!({
        "sheets": sheets,
        "names": names,
        "tables": tables,
        "settings": {"manual": s.mode == CalcMode::Manual, "iteration": s.iteration, "max_iterations": s.max_iterations, "max_change": s.max_change},
    })
}

pub fn load(doc: &J) -> Result<Workbook, String> {
    let mut wb = Workbook::empty();
    wb.set_mode(CalcMode::Manual);
    let sheets = doc["sheets"].as_array().ok_or("missing sheets")?;
    for s in sheets {
        wb.add_sheet(s["name"].as_str().ok_or("sheet without a name")?)
            .map_err(|e| e.to_string())?;
    }
    let mut formulas = Vec::new();
    for s in sheets {
        let id = wb.sheet_by_name(s["name"].as_str().unwrap()).unwrap().id;
        for pair in s["cells"].as_array().into_iter().flatten() {
            let a = xlcalc::parse_cell_ref(&wb, pair[0].as_str().unwrap_or(""), id)
                .ok_or("bad cell address")?;
            let t = &pair[1];
            let v = match t["t"].as_str().unwrap_or("") {
                "n" => Value::Number(t["v"].as_f64().unwrap_or(0.0)),
                "s" => Value::from(t["v"].as_str().unwrap_or("")),
                "b" => Value::Bool(t["v"].as_bool().unwrap_or(false)),
                "e" => Value::Error(
                    XlError::parse(t["v"].as_str().unwrap_or("")).unwrap_or(XlError::Value),
                ),
                "f" => {
                    formulas.push((a, t["v"].as_str().unwrap_or("").to_string()));
                    continue;
                }
                _ => continue,
            };
            wb.set_value(a, v);
        }
    }

    for t in doc["tables"].as_array().into_iter().flatten() {
        let sheet = wb
            .sheet_by_name(t["sheet"].as_str().unwrap_or(""))
            .ok_or("table on unknown sheet")?
            .id;
        let rect = xlcalc::parse_range_ref(&wb, t["range"].as_str().unwrap_or(""), sheet)
            .ok_or("bad table range")?
            .1;
        let name = t["name"].as_str().unwrap_or("");
        let totals = t["totals"].as_bool().unwrap_or(false);
        let mut data = if totals {
            xlcalc::Rect::new(rect.r0, rect.c0, rect.r1 - 1, rect.c1)
        } else {
            rect
        };
        let headers = t["headers"].as_bool().unwrap_or(true);

        let mut above = Vec::new();
        if !headers {
            if data.r0 == 0 {
                return Err("cannot restore a table without headers in row 1".into());
            }
            data.r0 -= 1;
            for (i, c) in t["columns"].as_array().into_iter().flatten().enumerate() {
                let a = xlcalc::CellAddr::new(sheet, data.r0, data.c0 + i as u32);
                above.push((a, wb.value(a)));
                wb.set_value(a, xlcalc::Value::from(c.as_str().unwrap_or("")));
            }
        }
        wb.add_table(name, sheet, data).map_err(|e| e.to_string())?;
        if !headers {
            wb.set_table_headers(name, false)
                .map_err(|e| e.to_string())?;
            for (a, v) in above {
                wb.set_value(a, v);
            }
        }
        if totals {
            wb.restore_table_totals(name, rect);
        }
    }
    for n in doc["names"].as_array().into_iter().flatten() {
        let scope = n["scope"]
            .as_str()
            .and_then(|s| wb.sheet_by_name(s))
            .map(|s| s.id);
        wb.define_name(
            n["name"].as_str().unwrap_or(""),
            n["refers_to"].as_str().unwrap_or(""),
            scope,
        )
        .map_err(|e| e.to_string())?;
    }
    for (a, f) in formulas {
        wb.set_formula(a, &f)
            .map_err(|e| format!("{}: {e}", a.a1()))?;
    }
    let st = &doc["settings"];
    let mut settings = wb.settings().clone();
    settings.iteration = st["iteration"].as_bool().unwrap_or(false);
    if let Some(n) = st["max_iterations"].as_u64() {
        settings.max_iterations = n as u32;
    }
    if let Some(x) = st["max_change"].as_f64() {
        settings.max_change = x;
    }
    settings.mode = if st["manual"].as_bool().unwrap_or(false) {
        CalcMode::Manual
    } else {
        CalcMode::Automatic
    };
    wb.restore_settings(settings);
    Ok(wb)
}
