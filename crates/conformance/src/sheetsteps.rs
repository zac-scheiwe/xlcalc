use serde_json::Value as J;
use xlcalc::Workbook;
use xlcalc_om::flags;
use xlcalc_variant::Variant;

use super::omsteps::{self, Arg, OmSession};
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

fn verdict(tally: &mut Tally, ok: bool, what: impl FnOnce() -> String) {
    tally.add(if ok { Outcome::Exact } else { Outcome::Wrong }, what);
}

fn strings(v: &J, f: impl Fn(&J) -> String) -> Vec<String> {
    v.as_array()
        .map(|a| a.iter().map(f).collect())
        .unwrap_or_default()
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
    let sheet_path = |name: &str| format!("Worksheets({})", omsteps::quote(name));
    match op {
        "rename_sheet" | "delete_sheet" | "move_sheet" | "copy_sheet" => {
            let path = sheet_path(step["sheet"].as_str().unwrap());
            let mut err: Option<String> = None;

            let place: Vec<Arg> = match step["before"].as_str() {
                Some(b) => vec![Arg::Path(sheet_path(b))],
                None => vec![
                    Arg::Missing,
                    Arg::Path(sheet_path(step["after"].as_str().unwrap_or(""))),
                ],
            };
            match op {
                "rename_sheet" => {
                    let name = Variant::Bstr(step["name"].as_str().unwrap().to_string());
                    err = omsteps::call(om, wb, &path, "Name", flags::PUT, &[name]).err();
                }
                "delete_sheet" => {
                    err = omsteps::call(om, wb, &path, "Delete", flags::METHOD, &[]).err();
                }
                "move_sheet" => {
                    err = omsteps::call_args(om, wb, &path, "Move", flags::METHOD, &place).err();
                }
                _ => match omsteps::call_args(om, wb, &path, "Copy", flags::METHOD, &place) {
                    Ok(_) => {
                        let auto =
                            match omsteps::call(om, wb, "ActiveSheet", "Name", flags::GET, &[]) {
                                Ok(Variant::Bstr(s)) => s,
                                _ => String::new(),
                            };
                        let want = rec["new_name"].as_str().unwrap_or("");
                        verdict(tally, auto == want, || {
                            format!("step {n}: copy named {auto:?}, Excel {want:?}")
                        });
                        if let Some(nm) = step["name"].as_str() {
                            let name = Variant::Bstr(nm.to_string());
                            err = omsteps::call(om, wb, "ActiveSheet", "Name", flags::PUT, &[name])
                                .err();
                        }
                    }
                    Err(e) => err = Some(e),
                },
            }
            omsteps::verdict(tally, op, rec, n, err);
        }
        "sheets" => {
            let count = match omsteps::call(om, wb, "Worksheets", "Count", flags::GET, &[]) {
                Ok(Variant::I4(c)) => c,
                _ => 0,
            };
            let got: Vec<String> = (1..=count)
                .map(|i| {
                    let path = format!("Worksheets({i})");
                    match omsteps::call(om, wb, &path, "Name", flags::GET, &[]) {
                        Ok(Variant::Bstr(s)) => s,
                        _ => String::new(),
                    }
                })
                .collect();
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

            let count = match omsteps::call(om, wb, "Names", "Count", flags::GET, &[]) {
                Ok(Variant::I4(c)) => c,
                _ => 0,
            };
            let mut got: Vec<String> = (1..=count)
                .map(|i| {
                    let path = format!("Names.Item({i})");
                    let mut text = |member: &str| match omsteps::call(
                        om,
                        wb,
                        &path,
                        member,
                        flags::GET,
                        &[],
                    ) {
                        Ok(Variant::Bstr(s)) => s,
                        _ => String::new(),
                    };
                    let (name, refers) = (text("Name"), text("RefersTo"));
                    format!("{name} {}", norm_book(&refers))
                })
                .collect();
            got.sort();
            let mut want = strings(&rec["names"], |x| {
                if x["name"].as_str().is_some_and(|s| s.starts_with("_xlop.")) {
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
        _ => return false,
    }
    true
}
