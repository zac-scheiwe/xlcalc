use serde_json::Value as J;
use xlcalc::parser::print::sheet_name_text;
use xlcalc::{SheetId, Workbook};

use super::{Outcome, Tally};

pub fn norm_book(s: &str) -> String {
    let mut out = String::new();
    let mut rest = s;
    while let Some(i) = rest.find("Book") {
        out.push_str(&rest[..i]);
        let tail = &rest[i + 4..];
        let digits = tail.chars().take_while(|c| c.is_ascii_digit()).count();
        if digits > 0 && (tail[digits..].starts_with('!') || tail[digits..].starts_with(']')) {
            out.push_str("Book1");
            rest = &tail[digits..];
        } else {
            out.push_str("Book");
            rest = tail;
        }
    }
    out.push_str(rest);
    out
}

fn id(wb: &Workbook, name: &str) -> Option<SheetId> {
    wb.sheet_by_name(name).map(|s| s.id)
}

fn before_of(wb: &Workbook, step: &J) -> Option<Option<SheetId>> {
    if let Some(b) = step["before"].as_str() {
        return id(wb, b).map(Some);
    }
    let a = id(wb, step["after"].as_str()?)?;
    let p = wb.sheet_position(a)?;
    Some((p + 1 < wb.sheet_count()).then(|| wb.sheet_at(p + 1).id))
}

fn verdict(tally: &mut Tally, ok: bool, what: impl FnOnce() -> String) {
    tally.add(if ok { Outcome::Exact } else { Outcome::Wrong }, what);
}

fn strings(v: &J, f: impl Fn(&J) -> String) -> Vec<String> {
    v.as_array()
        .map(|a| a.iter().map(f).collect())
        .unwrap_or_default()
}

pub fn run(wb: &mut Workbook, op: &str, step: &J, rec: &J, n: usize, tally: &mut Tally) -> bool {
    let excel_err = !rec["error"].is_null();
    match op {
        "rename_sheet" | "delete_sheet" | "move_sheet" | "copy_sheet" => {
            let Some(sid) = id(wb, step["sheet"].as_str().unwrap()) else {
                verdict(tally, false, || format!("step {n}: no sheet for {op}"));
                return true;
            };
            let mut err: Option<String> = None;
            match op {
                "rename_sheet" => {
                    err = wb
                        .rename_sheet(sid, step["name"].as_str().unwrap())
                        .err()
                        .map(|e| e.to_string());
                }
                "delete_sheet" => {
                    err = wb.delete_sheet(sid).err().map(|e| e.to_string());
                }
                "move_sheet" => match before_of(wb, step) {
                    Some(b) => err = wb.move_sheet(sid, b).err().map(|e| e.to_string()),
                    None => err = Some("no target sheet".into()),
                },
                _ => match before_of(wb, step) {
                    Some(b) => match wb.copy_sheet(sid, b) {
                        Ok(new) => {
                            let auto = wb.sheet(new).unwrap().name.clone();
                            let want = rec["new_name"].as_str().unwrap_or("");
                            verdict(tally, auto == want, || {
                                format!("step {n}: copy named {auto:?}, Excel {want:?}")
                            });
                            if let Some(nm) = step["name"].as_str() {
                                err = wb.rename_sheet(new, nm).err().map(|e| e.to_string());
                            }
                        }
                        Err(e) => err = Some(e.to_string()),
                    },
                    None => err = Some("no target sheet".into()),
                },
            }
            verdict(tally, err.is_some() == excel_err, || {
                format!(
                    "step {n}: {op} {} — Excel error {:?}, engine {err:?}",
                    step["sheet"], rec["error"]
                )
            });
        }
        "sheets" => {
            let got: Vec<String> = wb.sheets().map(|s| s.name.clone()).collect();
            let want = strings(&rec["sheets"], |x| x.as_str().unwrap_or("").to_string());
            verdict(tally, got == want, || {
                format!("step {n}: sheets expected {want:?}, got {got:?}")
            });
        }
        "names" => {
            if std::env::var("CONF_ACTIVE").is_ok() {
                let s = wb.active_sheet();
                eprintln!(
                    "step {n} active {:?} cell {:?}",
                    wb.sheet(s).map(|x| &x.name),
                    wb.active_cell()
                );
            }
            let mut got: Vec<String> = wb
                .names
                .iter()
                .map(|d| {
                    let nm = match d.scope {
                        Some(s) => {
                            format!("{}!{}", sheet_name_text(&wb.sheet(s).unwrap().name), d.name)
                        }
                        None => d.name.clone(),
                    };
                    format!("{nm} {}", norm_book(&format!("={}", wb.name_refers_to(d))))
                })
                .collect();
            got.sort();
            let mut want = strings(&rec["names"], |x| {
                if x["name"].as_str().is_some_and(|s| {
                    s.starts_with("_xlpm.")
                        || s.starts_with("_xlop.")
                        || (s.starts_with("_xlfn.") && x["visible"] == false)
                }) {
                    return String::new();
                }
                format!(
                    "{} {}",
                    x["name"].as_str().unwrap(),
                    norm_book(x["refers_to"].as_str().unwrap())
                )
            });
            want.retain(|w| !w.is_empty());
            want.sort();
            verdict(tally, got == want, || {
                let extra: Vec<_> = got.iter().filter(|g| !want.contains(g)).collect();
                let missing: Vec<_> = want.iter().filter(|w| !got.contains(w)).collect();
                format!("step {n}: names differ: engine only {extra:?}, Excel only {missing:?}")
            });
        }
        "tables" => {
            let rich = rec["tables"]
                .as_array()
                .is_some_and(|a| a.iter().any(|x| x["headers"].is_boolean()));
            let mut got: Vec<String> = wb
                .tables
                .iter()
                .map(|t| {
                    let extra = if rich {
                        format!(
                            " h={} t={} cols={}",
                            u8::from(t.headers),
                            u8::from(t.totals),
                            t.columns.join("|")
                        )
                    } else {
                        String::new()
                    };
                    format!(
                        "{} {} {}{}",
                        t.name,
                        wb.sheet(t.sheet).unwrap().name,
                        t.rect.a1(),
                        extra
                    )
                })
                .collect();
            got.sort();
            let mut want = strings(&rec["tables"], |x| {
                let extra = match (x["headers"].as_bool(), x["totals"].as_bool()) {
                    (Some(h), Some(t)) => format!(
                        " h={} t={} cols={}",
                        u8::from(h),
                        u8::from(t),
                        x["columns"]
                            .as_array()
                            .map(|a| a
                                .iter()
                                .filter_map(|c| c.as_str())
                                .collect::<Vec<_>>()
                                .join("|"))
                            .unwrap_or_default()
                    ),
                    _ => String::new(),
                };
                format!(
                    "{} {} {}{}",
                    x["name"].as_str().unwrap(),
                    x["sheet"].as_str().unwrap(),
                    x["range"].as_str().unwrap(),
                    extra
                )
            });
            want.sort();
            verdict(tally, got == want, || {
                format!("step {n}: tables expected {want:?}, got {got:?}")
            });
        }
        _ => return false,
    }
    true
}
