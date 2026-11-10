use std::collections::BTreeMap;
use std::fs;
use std::path::{Path, PathBuf};
use std::process::{Command, ExitCode};
use std::sync::atomic::{AtomicUsize, Ordering};
use std::sync::Mutex;

mod cellsteps;
mod cmdsteps;
mod comsteps;
mod datasteps;
mod editsteps;
mod filecmds;
mod filesteps;
mod golden;
mod minimize;
mod omsteps;
mod sheetsteps;
mod tablesteps;
mod viewsteps;
mod wbsuite;

use golden::{compact_golden, expand_golden, is_compact, load_golden, load_golden_text};
use minimize::SuiteSel;
use serde_json::{json, Value as J};
use xlcalc::{parse_cell_ref, parse_range_ref, CellAddr, Value, Workbook, XlError};
use xlcalc_om::flags;
use xlcalc_variant::Variant;

fn root() -> PathBuf {
    let p = Path::new(env!("CARGO_MANIFEST_DIR"))
        .join("..")
        .join("..")
        .canonicalize()
        .expect("repo root");

    match p.to_str().and_then(|s| s.strip_prefix(r"\\?\")) {
        Some(s) => PathBuf::from(s),
        None => p,
    }
}

fn conf_dir() -> PathBuf {
    root().join("conformance")
}

#[derive(Clone)]
struct Case {
    formula: String,
    nondeterministic: bool,
}

fn read_cases(path: &Path) -> Vec<Case> {
    let text = fs::read_to_string(path).unwrap_or_else(|e| panic!("{}: {e}", path.display()));
    text.lines()
        .map(str::trim_end)
        .filter(|l| !l.trim().is_empty() && !l.trim_start().starts_with('#'))
        .map(|l| {
            let l = l.trim_start();
            match l.strip_prefix('~') {
                Some(f) => Case {
                    formula: f.to_string(),
                    nondeterministic: true,
                },
                None => Case {
                    formula: l.to_string(),
                    nondeterministic: false,
                },
            }
        })
        .collect()
}

fn read_directives(path: &Path) -> Vec<String> {
    let text = fs::read_to_string(path).unwrap_or_default();
    text.lines()
        .filter_map(|l| l.trim().strip_prefix("#!"))
        .map(|d| d.trim().to_string())
        .collect()
}

fn suites() -> BTreeMap<String, (&'static str, PathBuf)> {
    let mut out = BTreeMap::new();
    for (dir, ext, kind) in [
        ("cases", "txt", "formulas"),
        ("scenarios", "json", "scenario"),
        ("workbooks", "xlsx", "workbook"),
        ("workbooks", "xlsb", "workbook"),
        ("workbooks", "xls", "workbook"),
    ] {
        let Ok(rd) = fs::read_dir(conf_dir().join(dir)) else {
            continue;
        };
        for e in rd.flatten() {
            let p = e.path();
            if p.extension().and_then(|x| x.to_str()) == Some(ext) {
                let name = p.file_stem().unwrap().to_string_lossy().to_string();
                out.insert(name, (kind, p));
            }
        }
    }
    out
}

fn selected(args: &[String]) -> Vec<String> {
    let all = suites();
    let names: Vec<String> = args
        .iter()
        .filter(|a| !a.starts_with('-'))
        .cloned()
        .collect();
    if names.is_empty() {
        return all.keys().cloned().collect();
    }
    for n in &names {
        if !all.contains_key(n) {
            eprintln!(
                "unknown suite '{n}' (have: {})",
                all.keys().cloned().collect::<Vec<_>>().join(", ")
            );
            std::process::exit(2);
        }
    }
    names
}

fn workbook_path(suite: &str) -> PathBuf {
    let dir = conf_dir().join("workbooks");
    for ext in ["xlsb", "xls"] {
        let b = dir.join(format!("{suite}.{ext}"));
        if b.exists() {
            return b;
        }
    }
    dir.join(format!("{suite}.xlsx"))
}

fn record(args: &[String]) -> ExitCode {
    let names = selected(args);
    let all = suites();
    let (wb_names, names): (Vec<String>, Vec<String>) =
        names.into_iter().partition(|n| all[n].0 == "workbook");
    if !wb_names.is_empty() {
        let st = record_workbooks(&wb_names, &all);
        if st != ExitCode::SUCCESS || names.is_empty() {
            return st;
        }
    }
    let fixture: J =
        serde_json::from_str(&fs::read_to_string(conf_dir().join("fixture.json")).unwrap())
            .unwrap();
    let mut batches = Vec::new();
    let mut flags: BTreeMap<String, Vec<bool>> = BTreeMap::new();
    let mut scenario_steps: BTreeMap<String, J> = BTreeMap::new();
    for n in &names {
        let (kind, path) = &all[n];
        if *kind == "formulas" {
            let cases = read_cases(path);
            flags.insert(
                n.clone(),
                cases.iter().map(|c| c.nondeterministic).collect(),
            );
            let date1904 = read_directives(path).iter().any(|d| d == "date1904");
            batches.push(json!({
                "kind": "formulas",
                "id": n,
                "date1904": date1904,
                "formulas": cases.iter().map(|c| c.formula.clone()).collect::<Vec<_>>(),
            }));
        } else {
            let mut sc: J = serde_json::from_str(&fs::read_to_string(path).unwrap()).unwrap();
            sc["kind"] = json!("scenario");
            sc["id"] = json!(n);
            scenario_steps.insert(n.clone(), sc.clone());
            batches.push(sc);
        }
    }
    let tmp = std::env::var_os("CONFORM_ORACLE_TMP")
        .map(PathBuf::from)
        .unwrap_or_else(|| conf_dir().join(".oracle-tmp"));
    fs::create_dir_all(&tmp).unwrap();
    let req_path = tmp.join("request.json");
    let resp_path = tmp.join("response.json");
    fs::write(
        &req_path,
        serde_json::to_string(&json!({ "fixture": fixture, "batches": batches })).unwrap(),
    )
    .unwrap();
    let _ = fs::remove_file(&resp_path);
    let status = Command::new("pwsh")
        .args(["-NoProfile", "-File"])
        .arg(root().join("oracle").join("excel_oracle.ps1"))
        .arg("-In")
        .arg(&req_path)
        .arg("-Out")
        .arg(&resp_path)
        .args(
            std::env::var("CONFORM_PROGID")
                .map(|p| vec!["-ProgId".to_string(), p])
                .unwrap_or_default(),
        )
        .status()
        .expect("run pwsh");
    if !status.success() {
        eprintln!("oracle failed: {status}");
        return ExitCode::FAILURE;
    }
    let text = fs::read_to_string(&resp_path).unwrap();
    let text = text.trim_start_matches('\u{feff}');
    let resp: J = serde_json::from_str(text).expect("oracle response JSON");

    let golden_dir = std::env::var_os("CONFORM_GOLDEN_OUT")
        .map(PathBuf::from)
        .unwrap_or_else(|| conf_dir().join("golden"));
    fs::create_dir_all(&golden_dir).unwrap();
    for n in &names {
        let r = &resp["results"][n];
        let mut items = r["items"].clone();
        if let (Some(f), Some(arr)) = (flags.get(n), items.as_array_mut()) {
            for (it, nd) in arr.iter_mut().zip(f) {
                if *nd {
                    it["nondeterministic"] = json!(true);
                }
            }
        }
        let mut golden = json!({
            "suite": n,
            "kind": r["kind"],
            "date1904": batches.iter().find(|b| b["id"] == json!(n)).map_or(json!(false), |b| b["date1904"].clone()),
            "excel": resp["excel"],
            "international": resp["international"],
            "items": items,
        });
        if let Some(sc) = scenario_steps.get(n) {
            golden["scenario"] = sc.clone();
        }
        golden::tokenize_root(&mut golden);
        let out = golden_dir.join(format!("{n}.json"));
        fs::write(
            &out,
            serde_json::to_string(&compact_golden(&golden)).unwrap(),
        )
        .unwrap();
        println!(
            "recorded {n} ({} items)",
            golden["items"].as_array().map_or(0, |a| a.len())
        );
    }
    ExitCode::SUCCESS
}

fn record_workbooks(names: &[String], all: &BTreeMap<String, (&'static str, PathBuf)>) -> ExitCode {
    let tmp = std::env::var_os("CONFORM_ORACLE_TMP")
        .map(PathBuf::from)
        .unwrap_or_else(|| conf_dir().join(".oracle-tmp"));
    fs::create_dir_all(&tmp).unwrap();
    let req_path = tmp.join("xlsx_request.json");
    let resp_path = tmp.join("xlsx_response.json");
    let list: Vec<J> = names
        .iter()

        .map(|n| json!({ "id": n, "path": all[n].1.to_string_lossy(), "opened": n.starts_with("lib_") }))
        .collect();
    fs::write(
        &req_path,
        serde_json::to_string(&json!({ "workbooks": list })).unwrap(),
    )
    .unwrap();
    let _ = fs::remove_file(&resp_path);
    let status = Command::new("pwsh")
        .args(["-NoProfile", "-File"])
        .arg(root().join("oracle").join("xlsx_oracle.ps1"))
        .arg("-In")
        .arg(&req_path)
        .arg("-Out")
        .arg(&resp_path)
        .args(
            std::env::var("CONFORM_PROGID")
                .map(|p| vec!["-ProgId".to_string(), p])
                .unwrap_or_default(),
        )
        .status()
        .expect("run pwsh");
    if !status.success() {
        eprintln!("workbook oracle failed: {status}");
        return ExitCode::FAILURE;
    }
    let text = fs::read_to_string(&resp_path).unwrap();
    let resp: J = serde_json::from_str(text.trim_start_matches('\u{feff}')).expect("oracle JSON");
    let golden_dir = std::env::var_os("CONFORM_GOLDEN_OUT")
        .map(PathBuf::from)
        .unwrap_or_else(|| conf_dir().join("golden"));
    fs::create_dir_all(&golden_dir).unwrap();
    for n in names {
        let r = &resp["results"][n];
        let mut golden = json!({
            "suite": n,
            "kind": "workbook",
            "date1904": r["date1904"],
            "excel": resp["excel"],
            "items": r["items"],
        });
        for k in ["opened", "repaired", "repairLog", "error"] {
            if let Some(v) = r.get(k) {
                golden[k] = v.clone();
            }
        }
        golden::tokenize_root(&mut golden);
        fs::write(
            golden_dir.join(format!("{n}.json")),
            serde_json::to_string(&compact_golden(&golden)).unwrap(),
        )
        .unwrap();
        let cells: usize = golden["items"].as_array().map_or(0, |a| {
            a.iter()
                .map(|i| i["cells"].as_array().map_or(0, |c| c.len()))
                .sum()
        });
        println!("recorded {n} ({cells} cells)");
    }
    ExitCode::SUCCESS
}

fn typed_value(t: &J) -> Value {
    match t["t"].as_str().unwrap_or("") {
        "n" => Value::Number(num_of(t)),
        "s" => Value::from(t["v"].as_str().unwrap()),
        "b" => Value::Bool(t["v"].as_bool().unwrap()),
        "e" => Value::Error(XlError::parse(t["v"].as_str().unwrap()).unwrap_or(XlError::VALUE)),
        _ => Value::Blank,
    }
}

fn apply_typed(wb: &mut Workbook, addr: CellAddr, t: &J) -> Result<(), String> {
    match t["t"].as_str().unwrap_or("") {
        "f" | "in" => wb
            .set_input(addr, t["v"].as_str().unwrap())
            .map(|_| ())
            .map_err(|e| e.to_string()),
        "blank" => {
            wb.clear(addr);
            Ok(())
        }
        "s" => wb
            .set_text_prefixed(addr, t["v"].as_str().unwrap())
            .map(|_| ())
            .map_err(|e| e.to_string()),
        _ => {
            wb.set_value(addr, typed_value(t));
            Ok(())
        }
    }
}

fn load_fixture(wb: &mut Workbook) {
    let fixture: J =
        serde_json::from_str(&fs::read_to_string(conf_dir().join("fixture.json")).unwrap())
            .unwrap();
    for sheet in fixture["sheets"].as_array().unwrap() {
        let id = wb.add_sheet(sheet["name"].as_str().unwrap()).unwrap();
        for pair in sheet["cells"].as_array().unwrap() {
            let addr = parse_cell_ref(wb, pair[0].as_str().unwrap(), id).unwrap();
            apply_typed(wb, addr, &pair[1]).unwrap();
        }
    }
}

#[derive(Debug, PartialEq)]
enum Outcome {
    Exact,
    Close,

    Near,
    Wrong,
}

fn num_of(t: &J) -> f64 {
    t["bits"]
        .as_str()
        .and_then(|b| u64::from_str_radix(b, 16).ok())
        .map(f64::from_bits)
        .unwrap_or_else(|| t["v"].as_f64().unwrap())
}

fn show_expected(t: &J) -> String {
    match t["t"].as_str().unwrap_or("?") {
        "n" => format!("{}", num_of(t)),
        "s" => format!("{:?}", t["v"].as_str().unwrap()),
        "b" => if t["v"].as_bool().unwrap() {
            "TRUE"
        } else {
            "FALSE"
        }
        .to_string(),
        "e" => t["v"].as_str().unwrap().to_string(),
        "blank" => "(blank)".to_string(),
        "a" => {
            let rows: Vec<String> = t["v"]
                .as_array()
                .unwrap()
                .iter()
                .map(|r| {
                    r.as_array()
                        .unwrap()
                        .iter()
                        .map(show_expected)
                        .collect::<Vec<_>>()
                        .join(",")
                })
                .collect();
            format!("{{{}}}", rows.join(";"))
        }
        other => format!("<{other}>"),
    }
}

fn show_value(v: &Value) -> String {
    match v {
        Value::Blank => "(blank)".into(),
        Value::Number(n) => format!("{n}"),
        Value::Text(s) => format!("{:?}", &**s),
        Value::Bool(b) => if *b { "TRUE" } else { "FALSE" }.into(),
        Value::Error(e) => e.to_string(),
        Value::Array(a) => {
            let rows: Vec<String> = (0..a.rows)
                .map(|r| {
                    (0..a.cols)
                        .map(|c| show_value(a.get(r, c)))
                        .collect::<Vec<_>>()
                        .join(",")
                })
                .collect();
            format!("{{{}}}", rows.join(";"))
        }
        Value::Lambda(_) => "<lambda>".into(),
    }
}

fn compare_scalar(t: &J, v: &Value) -> Outcome {
    match (t["t"].as_str().unwrap_or(""), v) {
        ("n", Value::Number(n)) => {
            let bits = t["bits"]
                .as_str()
                .and_then(|b| u64::from_str_radix(b, 16).ok());
            let want = bits
                .map(f64::from_bits)
                .unwrap_or_else(|| t["v"].as_f64().unwrap());
            if want.to_bits() == n.to_bits() || (want == 0.0 && *n == 0.0) {
                Outcome::Exact
            } else if (want - n).abs() <= 1e-12 * want.abs().max(n.abs()) {
                Outcome::Close
            } else if (want - n).abs() <= 1e-6 * want.abs().max(n.abs()) {
                Outcome::Near
            } else {
                Outcome::Wrong
            }
        }
        ("s", Value::Text(s))
            if t["v"].as_str().map(sheetsteps::norm_book).as_deref()
                == Some(&sheetsteps::norm_book(s)) =>
        {
            Outcome::Exact
        }
        ("b", Value::Bool(b)) if t["v"].as_bool() == Some(*b) => Outcome::Exact,
        ("e", Value::Error(e)) if t["v"].as_str() == Some(e.as_str()) => Outcome::Exact,
        ("blank", Value::Blank) => Outcome::Exact,
        _ => Outcome::Wrong,
    }
}

fn compare_value(t: &J, v: &Value) -> Outcome {
    if t["t"].as_str() == Some("a") {
        let Value::Array(a) = v else {
            return Outcome::Wrong;
        };
        let rows = t["v"].as_array().unwrap();
        if rows.len() != a.rows as usize || rows[0].as_array().unwrap().len() != a.cols as usize {
            return Outcome::Wrong;
        }
        let mut worst = Outcome::Exact;
        for (r, row) in rows.iter().enumerate() {
            for (c, cell) in row.as_array().unwrap().iter().enumerate() {
                match compare_scalar(cell, a.get(r as u32, c as u32)) {
                    Outcome::Wrong => return Outcome::Wrong,
                    Outcome::Near => worst = Outcome::Near,
                    Outcome::Close if worst == Outcome::Exact => worst = Outcome::Close,
                    Outcome::Close => {}
                    Outcome::Exact => {}
                }
            }
        }
        return worst;
    }
    compare_scalar(t, v)
}

fn same_type(t: &J, v: &Value) -> bool {
    matches!(
        (t["t"].as_str().unwrap_or(""), v),
        ("n", Value::Number(_))
            | ("s", Value::Text(_))
            | ("b", Value::Bool(_))
            | ("e", Value::Error(_))
            | ("a", Value::Array(_))
    )
}

fn pinned_to_clock(wb: &mut Workbook, addr: CellAddr, want: &J, got: &Value) -> Option<Value> {
    if want["t"].as_str() != Some("n")
        || !matches!(
            compare_value(want, got),
            Outcome::Wrong | Outcome::Near | Outcome::Close
        )
    {
        return None;
    }
    let now = num_of(want);
    if !(1.0..2_958_466.0).contains(&now) {
        return None;
    }

    if wb.settings().iteration {
        return None;
    }

    xlcalc::testing::set_fixed_now(wb, Some(now));
    xlcalc::testing::recalc(wb);
    let v = cell_result(wb, addr);
    xlcalc::testing::set_fixed_now(wb, None);
    xlcalc::testing::recalc(wb);
    matches!(compare_value(want, &v), Outcome::Exact).then_some(v)
}

fn cell_result(wb: &Workbook, addr: CellAddr) -> Value {
    if let Some(fid) = wb.formula_at(addr) {
        let f = wb.formula(fid);

        if f.spill.is_some() && !f.is_legacy_array() {
            return f.value.clone();
        }
    }
    wb.value(addr)
}

struct Tally {
    exact: usize,
    close: usize,
    near: usize,
    wrong: usize,

    out_of_scope: usize,
    failures: Vec<String>,

    pending_oos: Option<String>,

    truncated: Option<(usize, String)>,
    command_tier: bool,

    ran: std::collections::HashSet<String>,
    secs: f64,
    load_secs: f64,

    partial: bool,
}

fn oos_what(s: &str) -> Option<String> {
    if let Some(i) = s.find("out of scope: ") {
        let rest = &s[i + "out of scope: ".len()..];
        let end = rest.find(['"', ')']).unwrap_or(rest.len());
        return Some(rest[..end].to_string());
    }
    let i = s.find("OutOfScope(\"")?;
    let rest = &s[i + "OutOfScope(\"".len()..];
    Some(rest[..rest.find('"').unwrap_or(rest.len())].to_string())
}

fn is_command_op(op: &str) -> bool {
    matches!(
        op,
        "sort"
            | "table_sort"
            | "autofill"
            | "dataseries"
            | "paste_special"
            | "data_table"
            | "goal_seek"
            | "table_resize"
            | "table_unlist"
            | "table_delete"
            | "table_rename"
            | "table_headers"
            | "table_totals"
            | "merge"
            | "unmerge"
            | "replace"
            | "remove_dups"
            | "text_to_columns"
            | "subtotal"
            | "remove_subtotal"
    ) || op.starts_with("autofilter")
        || op.starts_with("hide_")
        || op.starts_with("unhide_")
}

impl Tally {
    fn new() -> Tally {
        Tally {
            exact: 0,
            close: 0,
            near: 0,
            wrong: 0,
            out_of_scope: 0,
            failures: Vec::new(),
            pending_oos: None,
            truncated: None,
            command_tier: false,
            ran: Default::default(),
            secs: 0.0,
            load_secs: 0.0,
            partial: false,
        }
    }

    fn add(&mut self, o: Outcome, describe: impl FnOnce() -> String) {
        if self.pending_oos.is_some() {
            return;
        }
        if o == Outcome::Exact {
            self.exact += 1;
            return;
        }
        let s = describe();
        if let Some(w) = oos_what(&s) {
            self.pending_oos = Some(w);
            return;
        }
        let s = if s.chars().count() > 240 && std::env::var_os("CONFORM_FULL").is_none() {
            format!("{}…", s.chars().take(240).collect::<String>())
        } else {
            s
        };
        match o {
            Outcome::Exact => {}
            Outcome::Close => {
                self.close += 1;
                self.failures.push(format!("~ {s}"));
            }
            Outcome::Near => {
                self.near += 1;
                self.failures.push(format!("≈ {s}"));
            }
            Outcome::Wrong => {
                self.wrong += 1;
                self.failures.push(format!("✗ {s}"));
            }
        }
    }
}

fn run_formulas(golden: &J, grep: Option<&str>, check_text: bool, sel: &SuiteSel) -> Tally {
    let mut wb = Workbook::empty();
    xlcalc::testing::set_fixed_now(&mut wb, None);
    load_fixture(&mut wb);
    let t_id = wb.add_sheet("T").unwrap();
    wb.add_sheet("S").unwrap();
    if golden["date1904"].as_bool().unwrap_or(false) {
        wb.set_date_system(xlcalc::DateSystem::D1904);
    }

    wb.set_mode(xlcalc::CalcMode::Manual);
    let mut tally = Tally::new();
    let mut occ = minimize::OccCounter::default();
    for (idx, it) in golden["items"].as_array().unwrap().iter().enumerate() {
        let formula = it["formula"].as_str().unwrap();
        if grep.is_some_and(|g| !formula.to_lowercase().contains(&g.to_lowercase())) {
            continue;
        }
        if let SuiteSel::Items(keep) = sel {
            if !keep.contains(&occ.next(it)) {
                continue;
            }
            tally.ran.insert(formula.to_string());
        }
        minimize::item_begin();
        let bad_before = tally.close + tally.near + tally.wrong;
        let host = parse_cell_ref(&wb, it["host"].as_str().unwrap(), t_id).unwrap();
        let invalid = it["invalid"].as_bool().unwrap_or(false);

        match if !formula.starts_with('=') || formula == "=" {
            wb.set_input(host, formula)
        } else {
            wb.set_formula(host, formula)
        } {
            Err(e) => {
                let o = if invalid {
                    Outcome::Exact
                } else {
                    Outcome::Wrong
                };
                tally.add(o, || {
                    format!(
                        "{formula}  → engine rejected ({e}), Excel accepted: {}",
                        show_expected(&it["value"])
                    )
                });
            }
            Ok(_) if invalid => {
                let got = cell_result(&wb, host);
                tally.add(Outcome::Wrong, || {
                    format!(
                        "{formula}  → Excel rejects this formula, engine gave {}",
                        show_value(&got)
                    )
                });
            }
            Ok(_) if check_text => {
                let want = it["normalized"].as_str().map(str::to_string);
                let got = wb.formula_text(host);
                let o = if want.is_none() || want == got {
                    Outcome::Exact
                } else {
                    Outcome::Wrong
                };
                tally.add(o, || {
                    format!("{formula}  → Excel text {want:?}, engine {got:?}")
                });
            }
            Ok(_) => {
                let got = cell_result(&wb, host);
                let o = if it["nondeterministic"].as_bool().unwrap_or(false) {
                    if same_type(&it["value"], &got) {
                        Outcome::Exact
                    } else {
                        Outcome::Wrong
                    }
                } else {
                    compare_value(&it["value"], &got)
                };
                tally.add(o, || {
                    format!(
                        "{formula}  → expected {}, got {}",
                        show_expected(&it["value"]),
                        show_value(&got)
                    )
                });
            }
        }
        wb.clear(host);
        minimize::item_end(it, idx, tally.close + tally.near + tally.wrong > bad_before);
    }
    tally
}

fn truncate_at(tally: &mut Tally, n: usize, results: &[J], steps: &J) {
    let what = tally.pending_oos.take().unwrap_or_default();
    for (j, st) in steps.as_array().unwrap().iter().enumerate().skip(n - 1) {
        if st["op"].as_str() == Some("read") {
            tally.out_of_scope += results[j]["cells"].as_array().map_or(0, |c| c.len());
        }
    }
    tally.truncated = Some((n, what));
}

fn run_scenario(golden: &J, suite: &str) -> Tally {
    let sc = &golden["scenario"];
    let mut wb = Workbook::empty();
    if sc["use_fixture"].as_bool().unwrap_or(false) {
        load_fixture(&mut wb);
    }
    let sheets: Vec<String> = match sc["sheets"].as_array() {
        Some(a) => a.iter().map(|s| s.as_str().unwrap().to_string()).collect(),
        None => vec!["Sheet1".into()],
    };
    for s in &sheets {
        wb.add_sheet(s).unwrap();
    }
    if sc["date1904"].as_bool().unwrap_or(false) {
        wb.set_date_system(xlcalc::DateSystem::D1904);
    }
    let default_sheet = wb.sheet_by_name(&sheets[0]).unwrap().id;
    let mut tally = Tally::new();
    let check_format = sc["check_format"].as_bool().unwrap_or(false);
    let check_empty_formats = sc["check_empty_formats"].as_bool().unwrap_or(false);
    let check_all_formats =
        sc["check_all_formats"].as_bool().unwrap_or(false) || check_empty_formats;
    let steps_text = sc["steps"].to_string().to_ascii_uppercase();
    let uses_clock = steps_text.contains("NOW(") || steps_text.contains("TODAY(");
    let mut typed: BTreeMap<String, String> = BTreeMap::new();
    omsteps::OmSession::set_sheets_in_new(sc["sheets_in_new_workbook"].as_u64().unwrap_or(1) as u32);
    let mut om: Option<omsteps::OmSession> = None;
    if sc["fresh_excel"].as_bool().unwrap_or(false) {
        om = Some(omsteps::OmSession::fresh(&mut wb));
    }
    tally.command_tier = sc["steps"]
        .as_array()
        .unwrap()
        .iter()
        .any(|s| s["op"].as_str().is_some_and(is_command_op));
    let results = golden["items"].as_array().unwrap();
    for (i, step) in sc["steps"].as_array().unwrap().iter().enumerate() {
        let rec = &results[i];
        if std::env::var("CONF_TRACE").is_ok() {
            eprintln!("step {i}");
        }
        if std::env::var("CONF_CHAIN").is_ok() && i > 0 {
            eprintln!("CHAIN step {i}: {}", wb.debug_chain());
        }
        if let Ok(r) = std::env::var("CONF_FMT") {
            if let Some((s, rect)) = parse_range_ref(&wb, &r, default_sheet) {
                let mut out = Vec::new();
                for row in rect.r0..=rect.r1 {
                    for col in rect.c0..=rect.c1 {
                        let a = xlcalc::CellAddr::new(s, row, col);
                        out.push(format!("{}={}", a.a1(), wb.debug_format(a)));
                    }
                }
                eprintln!("step {i} fmt {}", out.join(" | "));
            }
        }
        if std::env::var("CONF_MERGES").is_ok() {
            eprintln!("step {i} merges {:?}", wb.merges(default_sheet));
        }
        if std::env::var("CONF_TABLES").is_ok() {
            for t in wb.tables() {
                eprintln!(
                    "step {i} table {} {} totals={} calc={:?} key={:?} snap={:?}",
                    t.name,
                    t.rect,
                    t.totals,
                    t.calc,
                    t.calc_key,
                    t.calc_snap_columns()
                );
            }
        }
        if let Ok(w) = std::env::var("CONF_WATCH") {
            for a1 in w.split(',') {
                if let Some(ad) = parse_cell_ref(&wb, a1, default_sheet) {
                    eprintln!(
                        "  [{}] {a1}: {:?} = {}",
                        i + 1,
                        wb.formula_text(ad),
                        show_value(&cell_result(&wb, ad))
                    );
                }
            }
        }
        let op = step["op"].as_str().unwrap();
        if tally.pending_oos.is_some() {
            truncate_at(&mut tally, i + 1, results, &sc["steps"]);
            break;
        }

        if !rec["error"].is_null() {
            if let Some((sn, _)) = step["cell"]
                .as_str()
                .or(step["range"].as_str())
                .and_then(|c| c.split_once('!'))
            {
                if wb.sheet_by_name(sn.trim_matches('\'')).is_none() {
                    tally.add(Outcome::Exact, String::new);
                    continue;
                }
            }
        }
        match op {
            "set" | "typel" | "setl" | "array" | "setv" | "clear" | "clear_formats"
            | "clear_all" | "format" | "precision" | "read" | "read_text" => {
                let session = om.get_or_insert_with(|| omsteps::OmSession::new(&mut wb));
                let cx = cellsteps::Ctx {
                    op,
                    step,
                    rec,
                    n: i + 1,
                    default_sheet,
                    flags: cellsteps::ReadFlags {
                        check_format,
                        check_all_formats,
                        check_empty_formats,
                        uses_clock,
                    },
                };
                cellsteps::run(session, &mut wb, &cx, &mut typed, &mut tally);
            }
            "calc" => {
                let _ = omsteps::call(&mut om, &mut wb, "", "Calculate", flags::METHOD, &[]);
            }
            "fullcalc" => {
                let _ = omsteps::call(&mut om, &mut wb, "", "CalculateFull", flags::METHOD, &[]);
            }
            "rebuild" => {
                let _ = omsteps::call(
                    &mut om,
                    &mut wb,
                    "",
                    "CalculateFullRebuild",
                    flags::METHOD,
                    &[],
                );
            }
            "mode" => {
                let value = match step["value"].as_str() {
                    Some("manual") => -4135,
                    Some("semi") => 2,
                    _ => -4105,
                };
                let _ = omsteps::call(
                    &mut om,
                    &mut wb,
                    "",
                    "Calculation",
                    flags::PUT,
                    &[Variant::I4(value)],
                );
            }
            "iteration" => {
                let enabled = Variant::Bool(step["enabled"].as_bool().unwrap_or(false));
                let _ = omsteps::call(&mut om, &mut wb, "", "Iteration", flags::PUT, &[enabled]);
                if let Some(n) = step["max_iter"].as_i64() {
                    let n = Variant::I4(n as i32);
                    let _ = omsteps::call(&mut om, &mut wb, "", "MaxIterations", flags::PUT, &[n]);
                }
                if let Some(x) = step["max_change"].as_f64() {
                    let x = Variant::R8(x);
                    let _ = omsteps::call(&mut om, &mut wb, "", "MaxChange", flags::PUT, &[x]);
                }
            }
            "name" => {
                let path = match step["scope"].as_str() {
                    Some(sc) => format!("Worksheets({}).Names", omsteps::quote(sc)),
                    None => "Names".to_string(),
                };
                let args = [
                    Variant::Bstr(step["name"].as_str().unwrap().to_string()),
                    Variant::Bstr(step["refers_to"].as_str().unwrap().to_string()),
                ];
                let r = omsteps::call(&mut om, &mut wb, &path, "Add", flags::METHOD, &args);
                let excel_rejected = !rec["error"].is_null();
                match r {
                    Err(e) if !excel_rejected => tally.add(Outcome::Wrong, || {
                        format!("step {}: name {} rejected: {e}", i + 1, step["name"])
                    }),
                    Ok(_) if excel_rejected => tally.add(Outcome::Wrong, || {
                        format!(
                            "step {}: name {} accepted, but Excel rejected it",
                            i + 1,
                            step["name"]
                        )
                    }),
                    _ => tally.add(Outcome::Exact, String::new),
                }
            }
            "delete_name" => {
                let mut path = match step["scope"].as_str() {
                    Some(sc) => format!("Worksheets({}).Names", omsteps::quote(sc)),
                    None => "Names".to_string(),
                };
                path.push_str(&format!(
                    ".Item({})",
                    omsteps::quote(step["name"].as_str().unwrap())
                ));
                let r = omsteps::call(&mut om, &mut wb, &path, "Delete", flags::METHOD, &[]);
                omsteps::verdict(
                    &mut tally,
                    op,
                    rec,
                    i + 1,
                    r.err().map(|_| "no such name".into()),
                );
            }
            "copy" => {
                let r = omsteps::call_args(
                    &mut om,
                    &mut wb,
                    &omsteps::ref_path(step["from"].as_str().unwrap()),
                    "Copy",
                    flags::METHOD,
                    &[omsteps::Arg::Path(omsteps::ref_path(
                        step["to"].as_str().unwrap(),
                    ))],
                );
                let r = r.map(|_| ());
                if let Some(w) = r.as_ref().err().and_then(|e| oos_what(e)) {
                    tally.pending_oos = Some(w);
                }
                let excel_err = !rec["error"].is_null();
                match r {
                    Err(e) if !excel_err => tally.add(Outcome::Wrong, || {
                        format!("step {}: copy failed: {e}", i + 1)
                    }),
                    Ok(()) if excel_err => tally.add(Outcome::Wrong, || {
                        format!(
                            "step {}: copy accepted, but Excel refused: {:?}",
                            i + 1,
                            rec["error"]
                        )
                    }),
                    Err(_) => tally.add(Outcome::Exact, String::new),
                    Ok(()) => {}
                }
            }
            "cut" => {
                let r = omsteps::call_args(
                    &mut om,
                    &mut wb,
                    &omsteps::ref_path(step["from"].as_str().unwrap()),
                    "Cut",
                    flags::METHOD,
                    &[omsteps::Arg::Path(omsteps::ref_path(
                        step["to"].as_str().unwrap(),
                    ))],
                );

                let _ = omsteps::call(
                    &mut om,
                    &mut wb,
                    "",
                    "CutCopyMode",
                    flags::PUT,
                    &[Variant::Bool(false)],
                );
                let r = r.and_then(|v| match v {
                    Variant::Bool(false) => Err("cut refused".to_string()),
                    _ => Ok(()),
                });
                omsteps::verdict(&mut tally, op, rec, i + 1, r.err());
            }
            "insert_cells" | "delete_cells" => {
                let shift = step["shift"].as_str().unwrap_or("");
                let code = match (op, shift) {
                    ("insert_cells", "right") => -4161,
                    ("insert_cells", _) => -4121,
                    (_, "left") => -4159,
                    _ => -4162,
                };
                let member = if op == "insert_cells" {
                    "Insert"
                } else {
                    "Delete"
                };
                let r = omsteps::call(
                    &mut om,
                    &mut wb,
                    &omsteps::ref_path(step["range"].as_str().unwrap()),
                    member,
                    flags::METHOD,
                    &[Variant::I4(code)],
                );
                omsteps::verdict(&mut tally, op, rec, i + 1, r.err());
            }
            "insert_rows" | "delete_rows" | "insert_cols" | "delete_cols" => {
                let sheet = omsteps::quote(step["sheet"].as_str().unwrap());
                let at = step["at"].as_u64().unwrap();
                let last = at + step["count"].as_u64().unwrap() - 1;
                let path = if op.ends_with("rows") {
                    format!(
                        "Worksheets({sheet}).Rows({})",
                        omsteps::quote(&format!("{at}:{last}"))
                    )
                } else {
                    let letters = |n: u64| {
                        u32::try_from(n - 1).map_or("ZZZZ".to_string(), xlcalc::col_to_letters)
                    };
                    let text = format!("{}1:{}1", letters(at), letters(last));
                    format!(
                        "Worksheets({sheet}).Range({}).EntireColumn",
                        omsteps::quote(&text)
                    )
                };
                let member = if op.starts_with("insert") {
                    "Insert"
                } else {
                    "Delete"
                };
                let r = omsteps::call(&mut om, &mut wb, &path, member, flags::METHOD, &[]);
                omsteps::verdict(&mut tally, op, rec, i + 1, r.err());
            }

            "selinfo" => viewsteps::selinfo(&mut om, &mut wb),
            "circ" => {
                let want = rec["circular"].as_str().map(str::to_string);

                let path = format!(
                    "Worksheets({})",
                    omsteps::quote(step["sheet"].as_str().unwrap_or("Sheet1"))
                );
                let got = match omsteps::call(
                    &mut om,
                    &mut wb,
                    &path,
                    "CircularReference",
                    flags::GET,
                    &[],
                ) {
                    Ok(Variant::Dispatch(Some(_))) => Some(()),
                    _ => None,
                };

                let o = if want.is_some() == got.is_some() {
                    Outcome::Exact
                } else {
                    Outcome::Wrong
                };
                tally.add(o, || {
                    format!(
                        "step {}: circular reference expected {want:?}, got {got:?}",
                        i + 1
                    )
                });
            }

            "vba_module" | "vba_doc_module" | "run" => {
                let session = om.get_or_insert_with(|| omsteps::OmSession::new(&mut wb));
                let check = !matches!(
                    sc["steps"].get(i + 1).and_then(|s| s["op"].as_str()),
                    Some("vba_module" | "vba_doc_module")
                );
                let r = match op {
                    "run" => session.run_macro(&mut wb, step, rec),
                    "vba_doc_module" => session.vba_doc_module(&mut wb, step, rec, check),
                    _ => session.vba_module(&mut wb, step, rec, check),
                };
                match r {
                    Ok(()) => tally.add(Outcome::Exact, String::new),
                    Err(why) => tally.add(Outcome::Wrong, || {
                        format!(
                            "step {}: {op} {}: {why}",
                            i + 1,
                            step["name"]
                                .as_str()
                                .or(step["file"].as_str())
                                .unwrap_or("")
                        )
                    }),
                }
            }
            other @ ("com_advise" | "com_events") => {
                let session = om.get_or_insert_with(|| omsteps::OmSession::new(&mut wb));
                comsteps::run(session, &mut wb, other, step, rec, i + 1, &mut tally);
            }
            other if viewsteps::run(&mut om, &mut wb, other, step, rec, i + 1, &mut tally) => {}
            other if cmdsteps::run(&mut om, &mut wb, other, step, rec, i + 1, &mut tally) => {}
            other
                if datasteps::run(
                    &mut om,
                    &mut wb,
                    &datasteps::Ctx {
                        op: other,
                        step,
                        rec,
                        n: i + 1,
                    },
                    &mut tally,
                ) => {}
            other if editsteps::run(&mut wb, other, step, rec, i + 1, &mut tally) => {}
            other if sheetsteps::run(&mut om, &mut wb, other, step, rec, i + 1, &mut tally) => {}
            other
                if tablesteps::run(
                    &mut om,
                    &mut wb,
                    &tablesteps::Ctx {
                        op: other,
                        step,
                        rec,
                        n: i + 1,
                        default_sheet,
                    },
                    &mut tally,
                ) => {}
            other
                if om
                    .get_or_insert_with(|| omsteps::OmSession::new(&mut wb))
                    .run(&mut wb, other, step, rec, i + 1, &mut tally) => {}
            other
                if om
                    .get_or_insert_with(|| omsteps::OmSession::new(&mut wb))
                    .run_file_step(&mut wb, other, step, rec, i + 1, &mut tally) => {}
            other => {
                tally.add(Outcome::Wrong, || {
                    format!("step {}: unsupported op '{other}'", i + 1)
                });
            }
        }
        if tally.pending_oos.is_some() {
            truncate_at(&mut tally, i + 1, results, &sc["steps"]);
            break;
        }

        assert!(
            xlcalc::testing::ops_idle(&wb),
            "step {}: operation state left set",
            i + 1
        );
        if let Ok(w) = std::env::var("CONF_WATCH") {
            if let Some(a) = parse_cell_ref(&wb, &w, default_sheet) {
                eprintln!(
                    "watch step {} {w} = {} {:?}",
                    i + 1,
                    show_value(&cell_result(&wb, a)),
                    wb.formula_text(a)
                );
            }
        }
    }

    if suite.starts_with("style_") {
        drop(om);
        wbsuite::saved_round_trip(&wb, &mut tally);
    }
    tally
}

fn run_suite(
    n: &str,
    grep: Option<&str>,
    check_text: bool,
    man: Option<&minimize::Manifest>,
) -> Option<Tally> {
    let path = conf_dir().join("golden").join(format!("{n}.json"));
    let bytes = fs::read(&path).ok()?;
    let sel = match man {
        Some(m) => m.select(n, minimize::hash_bytes(&bytes)),
        None => SuiteSel::All,
    };
    let t0 = std::time::Instant::now();
    let mut t = run_suite_bytes(n, &path, &bytes, grep, check_text, &sel, false);
    if let Some(t) = t.as_mut() {
        t.secs = t0.elapsed().as_secs_f64();
    }
    t
}

fn run_suite_bytes(
    n: &str,
    path: &Path,
    bytes: &[u8],
    grep: Option<&str>,
    check_text: bool,
    sel: &SuiteSel,
    record: bool,
) -> Option<Tally> {
    if matches!(sel, SuiteSel::Skip) {
        let mut t = Tally::new();
        t.partial = true;
        return Some(t);
    }
    let tl = std::time::Instant::now();
    let golden: J = load_golden_text(path, std::str::from_utf8(bytes).ok()?);
    let load_secs = tl.elapsed().as_secs_f64();
    let kind = golden["kind"].as_str().unwrap_or("formulas").to_string();
    if record && kind != "formulas" {
        minimize::suite_begin();
    }
    let t = if kind == "scenario" {
        run_scenario(&golden, n)
    } else if kind == "workbook" {
        wbsuite::run_workbook(&golden, &workbook_path(n), check_text)
    } else {
        run_formulas(&golden, grep, check_text, sel)
    };
    if record && kind != "formulas" {
        minimize::suite_end(&kind, t.close + t.near + t.wrong > 0);
    }
    let mut t = t;
    t.load_secs = load_secs;
    t.partial = !matches!(sel, SuiteSel::All);
    Some(t)
}

fn run_suites(
    names: &[String],
    jobs: usize,
    grep: Option<&str>,
    check_text: bool,
    man: Option<&minimize::Manifest>,
) -> Vec<Option<Tally>> {
    let next = AtomicUsize::new(0);
    let slots: Vec<Mutex<Option<Tally>>> = names.iter().map(|_| Mutex::new(None)).collect();
    std::thread::scope(|sc| {
        let workers: Vec<_> = (0..jobs.clamp(1, names.len().max(1)))
            .map(|_| {
                std::thread::Builder::new()
                    .stack_size(256 * 1024 * 1024)
                    .spawn_scoped(sc, || loop {
                        let i = next.fetch_add(1, Ordering::Relaxed);
                        let Some(n) = names.get(i) else { break };
                        let t = run_suite(n, grep, check_text, man);
                        *slots[i].lock().unwrap() = t;
                    })
                    .unwrap()
            })
            .collect();
        for w in workers {
            if let Err(e) = w.join() {
                std::panic::resume_unwind(e);
            }
        }
    });
    slots.into_iter().map(|m| m.into_inner().unwrap()).collect()
}

fn changed_suites(base: &str) -> Vec<String> {
    let git = |args: &[&str]| -> Vec<String> {
        let out = Command::new("git")
            .args(args)
            .current_dir(root())
            .output()
            .unwrap_or_else(|e| panic!("git: {e}"));
        if !out.status.success() {
            eprintln!(
                "git {}: {}",
                args.join(" "),
                String::from_utf8_lossy(&out.stderr)
            );
            std::process::exit(2);
        }
        String::from_utf8_lossy(&out.stdout)
            .lines()
            .map(str::to_string)
            .collect()
    };
    let dirs = ["conformance/cases", "conformance/golden"];
    let mut files = git(&[&["diff", "--name-only", base, "--"][..], &dirs].concat());
    files.extend(git(&[
        &["ls-files", "--others", "--exclude-standard", "--"][..],
        &dirs,
    ]
    .concat()));
    let mut out: Vec<String> = files
        .iter()
        .filter_map(|f| {
            let rest = f
                .strip_prefix("conformance/cases/")
                .or_else(|| f.strip_prefix("conformance/golden/"))?;
            Path::new(rest)
                .file_stem()
                .map(|s| s.to_string_lossy().to_string())
        })
        .collect();
    out.sort();
    out.dedup();
    out
}

fn run(args: &[String]) -> ExitCode {
    let verbose = args.iter().any(|a| a == "-v" || a == "--verbose");

    let check_text = args.iter().any(|a| a == "--text");
    let flag = |name: &str| {
        args.iter()
            .position(|a| a == name)
            .and_then(|i| args.get(i + 1))
            .cloned()
    };
    let grep = flag("--grep");
    let full = args.iter().any(|a| a == "--full");
    let times = args.iter().any(|a| a == "--times");
    let jobs: usize = flag("--jobs")
        .and_then(|s| s.parse().ok())
        .unwrap_or_else(|| {
            let cores = std::thread::available_parallelism().map_or(1, |n| n.get());
            (cores / 2).clamp(1, 6)
        });

    let changed = args
        .iter()
        .position(|a| a == "--changed")
        .map(|i| match args.get(i + 1) {
            Some(b) if !b.starts_with('-') && !suites().contains_key(b) => b.clone(),
            _ => "HEAD".to_string(),
        });
    let limit: usize = flag("--limit").and_then(|s| s.parse().ok()).unwrap_or(40);
    let skip_values: Vec<String> = [
        grep.clone(),
        flag("--limit"),
        flag("--jobs"),
        changed.clone(),
    ]
    .into_iter()
    .flatten()
    .collect();
    let mut names: Vec<String> = selected(
        &args
            .iter()
            .filter(|a| !skip_values.contains(a))
            .cloned()
            .collect::<Vec<_>>(),
    );

    let explicit = args
        .iter()
        .any(|a| !a.starts_with('-') && !skip_values.contains(a))
        || changed.is_some();
    let man = if full || (explicit && !args.iter().any(|a| a == "--min")) {
        None
    } else {
        minimize::Manifest::load()
    };
    println!(
        "mode: {}",
        if man.is_some() {
            "minimized (conformance/minimal.json; `--full` replays everything)"
        } else {
            "full"
        }
    );
    if let Some(base) = &changed {
        let ch = changed_suites(base);
        names.retain(|n| ch.contains(n));
        if names.is_empty() {
            println!("no suites changed since {base}");
            return ExitCode::SUCCESS;
        }
    }
    let mut known = read_out_of_scope();
    let mut todo: Vec<String> = Vec::new();
    let mut ran: Vec<String> = Vec::new();
    let mut partial: std::collections::BTreeSet<String> = Default::default();
    let mut ran_formulas: BTreeMap<String, std::collections::HashSet<String>> = BTreeMap::new();
    let (mut te, mut tc, mut tn, mut tw) = (0, 0, 0, 0);

    let mut tiers = [[0usize; 5]; 2];
    println!(
        "{:<20} {:>7} {:>7} {:>7} {:>7} {:>8}",
        "suite", "exact", "close", "near", "wrong", "score"
    );
    let results = run_suites(&names, jobs, grep.as_deref(), check_text, man.as_ref());
    for (n, tally) in names.iter().zip(results) {
        let Some(tally) = tally else {
            println!("{n:<20} (no golden — run `conform record {n}`)");
            continue;
        };
        ran.push(n.clone());
        let total = tally.exact + tally.close + tally.near + tally.wrong;
        println!(
            "{:<20} {:>7} {:>7} {:>7} {:>7} {:>7.1}%",
            n,
            tally.exact,
            tally.close,
            tally.near,
            tally.wrong,
            if total > 0 {
                100.0 * tally.exact as f64 / total as f64
            } else {
                100.0
            }
        );
        if times {
            println!(
                "{:<20} time {:>9.2}s (golden load {:.2}s)",
                "", tally.secs, tally.load_secs
            );
        }
        if tally.out_of_scope > 0 || tally.truncated.is_some() {
            println!("{:<20} out_of_scope {:>5}", "", tally.out_of_scope);
        }
        if tally.partial {
            partial.insert(n.clone());
            ran_formulas.insert(n.clone(), tally.ran.clone());
        }
        if verbose {
            if let Some((n, what)) = &tally.truncated {
                println!("    truncated at step {n}: out of scope: {what}");
            }
        }
        let reasons: Vec<Option<usize>> = tally
            .failures
            .iter()
            .map(|f| match_known(&mut known, n, f))
            .collect();
        for (f, r) in tally.failures.iter().zip(&reasons) {
            if r.is_none() {
                todo.push(format!("{n} | {}", f.replace(['\r', '\n'], " ")));
            }
        }
        if verbose {
            for (f, r) in tally.failures.iter().zip(&reasons).take(limit) {
                match r {
                    Some(i) => println!("    {f}  (out of scope: {})", known[*i].reason),
                    None => println!("    {f}"),
                }
            }
            if tally.failures.len() > limit {
                println!("    … {} more", tally.failures.len() - limit);
            }
        }
        te += tally.exact;
        tc += tally.close;
        tn += tally.near;
        tw += tally.wrong;
        let t = &mut tiers[tally.command_tier as usize];
        t[0] += tally.exact;
        t[1] += tally.close;
        t[2] += tally.near;
        t[3] += tally.wrong;
        t[4] += tally.out_of_scope;
    }
    let total = te + tc + tn + tw;
    println!(
        "{:<20} {:>7} {:>7} {:>7} {:>7} {:>7.1}%",
        "TOTAL",
        te,
        tc,
        tn,
        tw,
        if total > 0 {
            100.0 * te as f64 / total as f64
        } else {
            100.0
        }
    );
    for (name, t) in ["TOTAL core", "TOTAL command"].iter().zip(&tiers) {
        println!(
            "{:<20} {:>7} {:>7} {:>7} {:>7} {:>7.1}%  out_of_scope {}",
            name,
            t[0],
            t[1],
            t[2],
            t[3],
            if t[..4].iter().sum::<usize>() > 0 {
                100.0 * t[0] as f64 / t[..4].iter().sum::<usize>() as f64
            } else {
                100.0
            },
            t[4]
        );
    }

    let stale: Vec<&OutOfScope> = known
        .iter()
        .filter(|k| {
            k.hits == 0
                && names.contains(&k.suite)
                && !partial.contains(&k.suite)
                && !k.pattern.starts_with(COM_ONLY)
        })
        .collect();
    println!(
        "OUT_OF_SCOPE {} listed, {} matched, {} stale",
        known.len(),
        known.iter().map(|k| k.hits).sum::<usize>(),
        stale.len()
    );
    for k in &stale {
        println!("  stale: {} | {}", k.suite, k.pattern);
    }
    todo.sort();
    println!("TODO {}", todo.len());
    if grep.is_none() {
        write_todo(&ran, &todo, &partial, &ran_formulas);
    }
    if !todo.is_empty() {
        ExitCode::FAILURE
    } else {
        ExitCode::SUCCESS
    }
}

fn write_todo(
    ran: &[String],
    new: &[String],
    partial: &std::collections::BTreeSet<String>,
    ran_formulas: &BTreeMap<String, std::collections::HashSet<String>>,
) {
    let path = conf_dir().join("todo.txt");
    let old = fs::read_to_string(&path).unwrap_or_default();
    let new_keys: std::collections::HashSet<&str> = new
        .iter()
        .map(|n| n.split("  → ").next().unwrap_or(""))
        .collect();
    let mut lines: Vec<String> = old
        .lines()
        .filter(|l| !l.trim().is_empty())
        .filter(|l| {
            let mut parts = l.splitn(2, " | ");
            let suite = parts.next().unwrap_or("");

            if partial.contains(suite) {
                let f = parts
                    .next()
                    .unwrap_or("")
                    .split("  → ")
                    .next()
                    .unwrap_or("");
                let replayed = ran_formulas.get(suite).is_some_and(|r| r.contains(f));
                let still = new_keys.contains(format!("{suite} | {f}").as_str());
                return conf_dir()
                    .join("golden")
                    .join(format!("{suite}.json"))
                    .exists()
                    && !(replayed && !still);
            }
            !ran.iter().any(|r| r == suite)
                && conf_dir()
                    .join("golden")
                    .join(format!("{suite}.json"))
                    .exists()
        })
        .map(str::to_string)
        .collect();
    lines.extend(new.iter().cloned());
    lines.sort();
    lines.dedup();
    let mut text = lines.join("\n");
    if !text.is_empty() {
        text.push('\n');
    }
    if text != old {
        if let Err(e) = fs::write(&path, text) {
            eprintln!("{}: {e}", path.display());
        }
    }
}

struct OutOfScope {
    suite: String,
    pattern: String,
    reason: String,
    hits: usize,
}

const COM_ONLY: &str = "COM: ";

fn read_out_of_scope() -> Vec<OutOfScope> {
    let path = conf_dir().join("out_of_scope.txt");
    let Ok(text) = fs::read_to_string(&path) else {
        return Vec::new();
    };
    let mut out = Vec::new();
    for (i, line) in text.lines().enumerate() {
        let line = line.trim();
        if line.is_empty() || line.starts_with('#') {
            continue;
        }
        let parts: Vec<&str> = line.splitn(3, " | ").collect();
        if parts.len() != 3 || parts.iter().any(|p| p.trim().is_empty()) {
            eprintln!(
                "{}:{}: expected `suite | pattern | reason`",
                path.display(),
                i + 1
            );
            continue;
        }
        out.push(OutOfScope {
            suite: parts[0].trim().to_string(),
            pattern: parts[1].trim().to_string(),
            reason: parts[2].trim().to_string(),
            hits: 0,
        });
    }
    out
}

fn out_of_scope_reason(suite: &str, failure: &str) -> Option<String> {
    let mut known = read_out_of_scope();
    match_known(&mut known, suite, failure).map(|i| known[i].reason.clone())
}

fn match_known(known: &mut [OutOfScope], suite: &str, failure: &str) -> Option<usize> {
    let i = known
        .iter()
        .position(|k| k.suite == suite && (k.pattern == "*" || failure.contains(&k.pattern)))?;
    known[i].hits += 1;
    Some(i)
}

fn golden_files(args: &[String]) -> Vec<PathBuf> {
    let dir = conf_dir().join("golden");
    let mut v: Vec<PathBuf> = fs::read_dir(&dir)
        .unwrap()
        .map(|e| e.unwrap().path())
        .filter(|p| p.extension().is_some_and(|x| x == "json"))
        .filter(|p| {
            let stem = p.file_stem().unwrap().to_string_lossy();
            args.is_empty() || args.iter().any(|a| *a == stem)
        })
        .collect();
    v.sort();
    v
}

fn compact_cmd(args: &[String]) -> ExitCode {
    let (mut before, mut after, mut kept) = (0u64, 0u64, 0);
    for p in golden_files(args) {
        let raw = fs::read_to_string(&p).unwrap();
        let g = expand_golden(&serde_json::from_str(&raw).unwrap()).unwrap();
        let c = compact_golden(&g);
        if !is_compact(&c) {
            kept += 1;
            eprintln!(
                "{}: not compactable losslessly, minified original encoding",
                p.display()
            );
        }
        let out = serde_json::to_string(&c).unwrap();
        before += raw.len() as u64;
        after += out.len() as u64;
        if out != raw {
            fs::write(&p, &out).unwrap();
        }
    }
    println!("compact: {before} -> {after} bytes; {kept} files kept in original encoding");
    ExitCode::SUCCESS
}

fn verify_golden(args: &[String]) -> ExitCode {
    let against = args
        .iter()
        .position(|a| a == "--against")
        .and_then(|i| args.get(i + 1))
        .map(PathBuf::from);
    let suites: Vec<String> = args
        .iter()
        .enumerate()
        .filter(|(i, a)| !a.starts_with("--") && (*i == 0 || args[*i - 1] != "--against"))
        .map(|(_, a)| a.clone())
        .collect();
    let (mut ok, mut bad, mut compact) = (0, 0, 0);
    for p in golden_files(&suites) {
        let raw = fs::read_to_string(&p).unwrap();
        let v: J = serde_json::from_str(&raw).unwrap();
        let mut good = true;
        match expand_golden(&v) {
            Err(e) => {
                eprintln!("{}: {e}", p.display());
                good = false;
            }
            Ok(g) => {
                if is_compact(&v) {
                    compact += 1;
                }
                if serde_json::to_string(&compact_golden(&g)).unwrap() != raw {
                    eprintln!("{}: does not re-compact to identical bytes", p.display());
                    good = false;
                }
                if let Some(dir) = &against {
                    let o: J = serde_json::from_str(
                        &fs::read_to_string(dir.join(p.file_name().unwrap())).unwrap(),
                    )
                    .unwrap();
                    if o != g {
                        eprintln!("{}: expansion differs from {}", p.display(), dir.display());
                        good = false;
                    }
                }
            }
        }
        if good {
            ok += 1;
        } else {
            bad += 1;
        }
    }
    println!("verify-golden: {ok} ok, {bad} failed ({compact} compact encoding)");
    if bad > 0 {
        ExitCode::FAILURE
    } else {
        ExitCode::SUCCESS
    }
}

fn short(v: &J) -> String {
    let s = if v.get("t").and_then(J::as_str) == Some("n") {
        match v.get("bits").and_then(J::as_str) {
            Some(b) => format!("n:{b}"),
            None => v.to_string(),
        }
    } else {
        v.to_string()
    };
    if s.chars().count() > 90 {
        format!("{}…", s.chars().take(90).collect::<String>())
    } else {
        s
    }
}

fn vba_code(text: &str) -> String {
    let mut kept: Vec<String> = Vec::new();
    for line in text.lines() {
        let st = line.trim_start();
        let is_rem = st.len() >= 3
            && st.is_char_boundary(3)
            && st[..3].eq_ignore_ascii_case("rem")
            && !st[3..]
                .chars()
                .next()
                .is_some_and(|c| c.is_alphanumeric() || c == '_');
        if st.starts_with('\'') || is_rem {
            continue;
        }
        let mut line = line;
        if !line.contains('"') {
            let mut prev_space = false;
            for (i, c) in line.char_indices() {
                if c == '\'' && prev_space {
                    line = line[..i].trim_end();
                    break;
                }
                prev_space = c.is_whitespace();
            }
        }
        let line = line.trim_end_matches([' ', '\t', '\r']);
        if !line.is_empty() {
            kept.push(line.to_string());
        }
    }
    kept.join("\n")
}

fn diff_json(path: &str, g: &J, f: &J, out: &mut Vec<String>) {
    match (g, f) {
        (J::Object(go), J::Object(fo)) => {
            if go.get("nondeterministic").and_then(J::as_bool) == Some(true)
                && go.contains_key("value")
            {
                let (gt, ft) = (
                    go["value"].get("t"),
                    fo.get("value").and_then(|v| v.get("t")),
                );
                if gt != ft {
                    out.push(format!(
                        "{path}.value.t(nondet): golden {gt:?} fresh {ft:?}"
                    ));
                }
                for (k, gv) in go {
                    if matches!(k.as_str(), "value" | "spill" | "nondeterministic") {
                        continue;
                    }
                    match fo.get(k) {
                        Some(fv) => diff_json(&format!("{path}.{k}"), gv, fv, out),
                        None => {
                            out.push(format!("{path}.{k}: golden {} fresh <absent>", short(gv)))
                        }
                    }
                }
                return;
            }
            if go.get("t").and_then(J::as_str) == Some("n")
                && fo.get("t").and_then(J::as_str) == Some("n")
            {
                let gb = go.get("bits").and_then(J::as_str);
                let fb = fo.get("bits").and_then(J::as_str);
                let same = match (gb, fb) {
                    (Some(a), Some(b)) => a == b,
                    _ => num_of(g).to_bits() == num_of(f).to_bits(),
                };
                if !same {
                    out.push(format!("{path}: golden {} fresh {}", short(g), short(f)));
                }
                return;
            }
            for (k, gv) in go {
                let p = if path.is_empty() {
                    k.clone()
                } else {
                    format!("{path}.{k}")
                };
                match fo.get(k) {
                    Some(fv) => diff_json(&p, gv, fv, out),
                    None => out.push(format!("{p}: golden {} fresh <absent>", short(gv))),
                }
            }
        }
        (J::Array(ga), J::Array(fa)) => {
            if ga.len() != fa.len() {
                out.push(format!(
                    "{path}: golden len {} fresh len {}",
                    ga.len(),
                    fa.len()
                ));
            }
            for (i, (a, b)) in ga.iter().zip(fa).enumerate() {
                diff_json(&format!("{path}[{i}]"), a, b, out);
            }
        }
        (J::String(a), J::String(b)) => {
            if sheetsteps::norm_book(a) != sheetsteps::norm_book(b) {
                out.push(format!("{path}: golden {} fresh {}", short(g), short(f)));
            }
        }
        _ => {
            if g != f {
                out.push(format!("{path}: golden {} fresh {}", short(g), short(f)));
            }
        }
    }
}

fn item_label(i: usize, it: &J) -> String {
    let mut s = format!("#{i}");
    if let Some(st) = it.get("step") {
        s.push_str(&format!(" step={st}"));
    }
    for k in ["cell", "range", "op", "host"] {
        if let Some(v) = it.get(k).and_then(J::as_str) {
            s.push_str(&format!(" {k}={v}"));
        }
    }
    if let Some(f) = it.get("formula").and_then(J::as_str) {
        let f: String = f.chars().take(70).collect();
        s.push_str(&format!(" f={f}"));
    }
    s
}

fn compare_golden(args: &[String]) -> ExitCode {
    let Some(dir) = args.first().map(PathBuf::from) else {
        eprintln!("usage: conform compare-golden <dir> [suite…]");
        return ExitCode::from(2);
    };
    let base = args
        .iter()
        .position(|a| a == "--base")
        .and_then(|i| args.get(i + 1))
        .map(PathBuf::from);
    let names: Vec<String> = args[1..]
        .iter()
        .enumerate()
        .filter(|(i, a)| !a.starts_with('-') && (*i == 0 || args[*i] != "--base"))
        .map(|(_, a)| a.clone())
        .collect();
    let scored = args.iter().any(|a| a == "--scored");
    let known = if scored {
        read_out_of_scope()
    } else {
        Vec::new()
    };
    let todo = if scored {
        fs::read_to_string(conf_dir().join("todo.txt")).unwrap_or_default()
    } else {
        String::new()
    };
    let listed = |name: &str| {
        known.iter().any(|k| k.suite == name)
            || todo.lines().any(|l| l.split(" | ").next() == Some(name))
    };
    let mut listed_diffs = Vec::new();
    let (mut nsuites, mut ndiff_suites, mut nitems, mut nmissing) = (0, 0, 0usize, 0);
    let mut ncovered = 0usize;
    for p in golden_files(&names) {
        let fp = dir.join(p.file_name().unwrap());
        let name = p.file_stem().unwrap().to_string_lossy().to_string();
        if !fp.exists() {
            nmissing += 1;
            continue;
        }
        let gp = base
            .as_ref()
            .map_or(p.clone(), |b| b.join(p.file_name().unwrap()));
        if !gp.exists() {
            nmissing += 1;
            continue;
        }
        if scored && known.iter().any(|k| k.suite == name && k.pattern == "*") {
            continue;
        }
        nsuites += 1;
        let g = load_golden(&gp);
        let f = load_golden(&fp);
        let (mut ga, mut fa) = (
            g["items"].as_array().cloned().unwrap_or_default(),
            f["items"].as_array().cloned().unwrap_or_default(),
        );
        if scored {
            (ga, fa) = scored_items(&g["scenario"], ga, fa);
        }
        let mut lines = Vec::new();

        let mut pairs: Vec<(usize, usize)> = Vec::new();
        if ga.len() == fa.len() {
            pairs.extend((0..ga.len()).map(|i| (i, i)));
        } else {
            lines.push(format!(
                "{name}: item count golden {} fresh {} (aligning by formula)",
                ga.len(),
                fa.len()
            ));
            let mut j = 0;
            for (i, it) in ga.iter().enumerate() {
                let Some(fm) = it.get("formula") else {
                    continue;
                };
                if let Some(off) = fa[j..].iter().position(|x| x.get("formula") == Some(fm)) {
                    pairs.push((i, j + off));
                    j += off + 1;
                }
            }
        }
        for (i, j) in pairs {
            let (gi, fi) = (&ga[i], &fa[j]);
            let mut d = Vec::new();
            diff_json("", gi, fi, &mut d);
            if !d.is_empty() {
                nitems += 1;
                let more = if d.len() > 4 {
                    format!(" (+{} more)", d.len() - 4)
                } else {
                    String::new()
                };
                d.truncate(4);
                let line = format!("{name} {} | {}{more}", item_label(i, gi), d.join(" | "));
                if scored
                    && known.iter().any(|k| {
                        k.suite == name
                            && k.pattern
                                .strip_prefix(COM_ONLY)
                                .is_some_and(|p| line.contains(p))
                    })
                {
                    ncovered += 1;
                    nitems -= 1;
                    continue;
                }
                lines.push(line);
            }
        }
        if !lines.is_empty() {
            ndiff_suites += 1;
            if scored && listed(&name) {
                listed_diffs.push(name.clone());
            }
            for l in lines {
                println!("{l}");
            }
        }
    }
    if scored {
        println!(
            "compare-golden --scored: {} of the differing suites have entries in todo.txt or out_of_scope.txt: {} ({ncovered} items covered by `COM:` entries)",
            listed_diffs.len(),
            listed_diffs.join(" ")
        );
    }
    println!("compare-golden: {nsuites} suites compared, {ndiff_suites} with differences, {nitems} differing items ({nmissing} without recording in {})", dir.display());
    if ndiff_suites > 0 {
        ExitCode::from(1)
    } else {
        ExitCode::SUCCESS
    }
}

fn scored_items(sc: &J, golden: Vec<J>, fresh: Vec<J>) -> (Vec<J>, Vec<J>) {
    let all_formats = sc["check_all_formats"] == true || sc["check_empty_formats"] == true;
    let typed_formats = sc["check_format"] == true;
    let stop = fresh
        .iter()
        .position(|it| {
            it["error"]
                .as_str()
                .is_some_and(|e| e.starts_with("out of scope"))
        })
        .unwrap_or(usize::MAX);
    let keep = |items: Vec<J>| -> Vec<J> {
        items
            .into_iter()
            .take(stop)
            .map(|mut it| {
                if it["op"] == "circ" {
                    it["circular"] = json!(!it["circular"].is_null());
                }
                if it["op"] == "file_bytes" {
                    it["parts"] = json!(it["parts"].is_array());
                }
                if it["op"] == "unmerge" {
                    if let Some(o) = it.as_object_mut() {
                        o.remove("error");
                    }
                } else if it["error"].is_string() {
                    it["error"] = json!(true);
                }
                let it_op = it["op"].as_str().unwrap_or("").to_string();
                for c in it
                    .get_mut("cells")
                    .and_then(J::as_array_mut)
                    .into_iter()
                    .flatten()
                {
                    let constant = |k: &str| c[k].as_str().is_some_and(|t| !t.starts_with('='));
                    let constants: Vec<&str> = ["formula", "formula2"]
                        .into_iter()
                        .filter(|k| it_op == "read_text" && constant(k))
                        .collect();
                    let scored = all_formats || (typed_formats && c["value"]["t"] == "n");
                    let Some(cell) = c.as_object_mut() else {
                        continue;
                    };
                    for k in constants {
                        cell.remove(k);
                    }
                    if scored {
                        let f = cell.get("format").and_then(J::as_str).unwrap_or("General");
                        let f = f.to_ascii_lowercase();
                        cell.insert("format".into(), json!(f));
                    } else {
                        cell.remove("format");
                    }
                }
                if let Some(names) = it.get_mut("names").and_then(J::as_array_mut) {
                    names.retain(|x| {
                        let s = x["name"].as_str().unwrap_or("");
                        !(s.starts_with("_xlpm.")
                            || s.starts_with("_xlop.")
                            || (s.starts_with("_xlfn.") && x["visible"] == false))
                    });
                    for x in names.iter_mut() {
                        let refers = sheetsteps::norm_book(x["refers_to"].as_str().unwrap_or(""));
                        *x = json!({ "name": x["name"], "refers_to": refers });
                    }
                    names.sort_by_key(|x| x.to_string());
                }
                it
            })
            .collect()
    };
    let nondet: Vec<bool> = golden
        .iter()
        .map(|it| it["nondeterministic"] == true)
        .collect();
    let (g, f) = (keep(golden), keep(fresh));
    let drop = |v: Vec<J>| -> Vec<J> {
        v.into_iter()
            .enumerate()
            .filter(|(i, _)| !nondet.get(*i).copied().unwrap_or(false))
            .map(|(_, x)| x)
            .collect()
    };
    (drop(g), drop(f))
}

fn main() -> ExitCode {
    let args: Vec<String> = std::env::args().skip(1).collect();

    let child = std::thread::Builder::new()
        .stack_size(256 * 1024 * 1024)
        .spawn(move || match args.first().map(String::as_str) {
            Some("record") => record(&args[1..]),
            Some("run") => run(&args[1..]),
            Some("sigs") => minimize::sigs_cmd(&args[1..]),
            Some("minimize") => minimize::minimize_cmd(&args[1..]),
            Some("compact") => compact_cmd(&args[1..]),
            Some("verify-golden") => verify_golden(&args[1..]),
            Some("compare-golden") => compare_golden(&args[1..]),
            Some("eval") => eval_file(&args[1]),
            Some("pkgdiff") => filecmds::pkgdiff(&args[1..]),
            Some("crossread") => filecmds::crossread(&args[1..]),
            Some("crossread-files") => filesteps::crossread_files(&args[1..]),
            Some("list") => {
                for (n, (k, _)) in suites() {
                    println!("{n:<20} {k}");
                }
                ExitCode::SUCCESS
            }
            _ => {
                eprintln!("usage: conform record|run|compact|verify-golden|compare-golden|pkgdiff|crossread|list [suite…] [-v] [--grep TEXT] [--limit N] [--jobs N] [--changed [BASE]]");
                ExitCode::from(2)
            }
        })
        .unwrap();
    child.join().unwrap()
}

fn eval_file(path: &str) -> ExitCode {
    let mut wb = Workbook::empty();
    xlcalc::testing::set_fixed_now(&mut wb, None);
    load_fixture(&mut wb);
    let t_id = wb.add_sheet("T").unwrap();
    wb.add_sheet("S").unwrap();
    let host = parse_cell_ref(&wb, "A1", t_id).unwrap();
    let cur = std::sync::Arc::new(std::sync::Mutex::new((
        String::new(),
        std::time::Instant::now(),
    )));
    let cur2 = cur.clone();
    std::thread::spawn(move || loop {
        std::thread::sleep(std::time::Duration::from_millis(200));
        let g = cur2.lock().unwrap();
        if !g.0.is_empty() && g.1.elapsed().as_secs() >= 6 {
            println!("HANG	{}", g.0);
            std::process::exit(3);
        }
    });
    for c in read_cases(Path::new(path)) {
        *cur.lock().unwrap() = (c.formula.clone(), std::time::Instant::now());
        match wb.set_formula(host, &c.formula) {
            Err(e) => println!("REJECT {e}\t{}", c.formula),
            Ok(_) => println!("{}\t{}", show_value(&cell_result(&wb, host)), c.formula),
        }
    }
    ExitCode::SUCCESS
}

#[cfg(test)]
mod vba_code_tests {
    use super::vba_code;

    #[test]
    fn comments_blank_lines_and_trailing_spaces_do_not_count() {
        let with = "Option Explicit\r\n' a comment\r\n\r\nDim x As Long  ' trailing\r\n    Rem old style\r\nx = 1   \r\nMsg = \"it's\" ' kept: the line has a quote\r\n";
        let without =
            "Option Explicit\nDim x As Long\nx = 1\nMsg = \"it's\" ' kept: the line has a quote";
        assert_eq!(vba_code(with), vba_code(without));
        assert_eq!(vba_code(without), without);
        assert_eq!(vba_code("Remark = 1"), "Remark = 1");
    }
}
