use std::collections::BTreeMap;
use std::fs;
use std::path::{Path, PathBuf};
use std::process::{Command, ExitCode};

mod blocksteps;
mod editsteps;
mod golden;
mod sheetsteps;

use golden::{compact_golden, expand_golden, is_compact, load_golden};
use serde_json::{json, Value as J};
use xlcalc::{parse_cell_ref, parse_range_ref, CalcMode, CellAddr, Value, Workbook, XlError};

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

fn record(args: &[String]) -> ExitCode {
    let names = selected(args);
    let all = suites();
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

fn typed_value(t: &J) -> Value {
    match t["t"].as_str().unwrap_or("") {
        "n" => Value::Number(num_of(t)),
        "s" => Value::from(t["v"].as_str().unwrap()),
        "b" => Value::Bool(t["v"].as_bool().unwrap()),
        "e" => Value::Error(XlError::parse(t["v"].as_str().unwrap()).unwrap_or(XlError::Value)),
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

    if wb.settings.iteration {
        return None;
    }

    wb.calc.fixed_now = Some(now);
    xlcalc::calc::recalc(wb);
    let v = cell_result(wb, addr);
    wb.calc.fixed_now = None;
    xlcalc::calc::recalc(wb);
    matches!(compare_value(want, &v), Outcome::Exact).then_some(v)
}

fn cell_result(wb: &Workbook, addr: CellAddr) -> Value {
    if let Some(fid) = wb.formula_at(addr) {
        let f = wb.formula(fid);

        if f.spill.is_some() && f.array.is_none() {
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
    failures: Vec<String>,
}

impl Tally {
    fn new() -> Tally {
        Tally {
            exact: 0,
            close: 0,
            near: 0,
            wrong: 0,
            failures: Vec::new(),
        }
    }

    fn add(&mut self, o: Outcome, describe: impl FnOnce() -> String) {
        let describe = || {
            let s = describe();
            if s.chars().count() > 240 {
                format!("{}…", s.chars().take(240).collect::<String>())
            } else {
                s
            }
        };
        match o {
            Outcome::Exact => self.exact += 1,
            Outcome::Close => {
                self.close += 1;
                self.failures.push(format!("~ {}", describe()));
            }
            Outcome::Near => {
                self.near += 1;
                self.failures.push(format!("≈ {}", describe()));
            }
            Outcome::Wrong => {
                self.wrong += 1;
                self.failures.push(format!("✗ {}", describe()));
            }
        }
    }
}

fn run_formulas(golden: &J, grep: Option<&str>, check_text: bool) -> Tally {
    let mut wb = Workbook::empty();
    wb.calc.fixed_now = None;
    load_fixture(&mut wb);
    let t_id = wb.add_sheet("T").unwrap();
    wb.add_sheet("S").unwrap();
    if golden["date1904"].as_bool().unwrap_or(false) {
        wb.settings.date_system = xlcalc::datetime::DateSystem::D1904;
    }
    let mut tally = Tally::new();
    for it in golden["items"].as_array().unwrap() {
        let formula = it["formula"].as_str().unwrap();
        if grep.is_some_and(|g| !formula.to_lowercase().contains(&g.to_lowercase())) {
            continue;
        }
        let host = parse_cell_ref(&wb, it["host"].as_str().unwrap(), t_id).unwrap();
        let invalid = it["invalid"].as_bool().unwrap_or(false);
        match if formula == "=" {
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
    }
    tally
}

fn run_scenario(golden: &J) -> Tally {
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
        wb.settings.date_system = xlcalc::datetime::DateSystem::D1904;
    }
    let default_sheet = wb.sheet_by_name(&sheets[0]).unwrap().id;
    let mut tally = Tally::new();
    let check_format = sc["check_format"].as_bool().unwrap_or(false);
    let check_empty_formats = sc["check_empty_formats"].as_bool().unwrap_or(false);
    let check_all_formats =
        sc["check_all_formats"].as_bool().unwrap_or(false) || check_empty_formats;
    let mut typed: BTreeMap<String, String> = BTreeMap::new();
    let results = golden["items"].as_array().unwrap();
    for (i, step) in sc["steps"].as_array().unwrap().iter().enumerate() {
        let rec = &results[i];
        if std::env::var("CONF_TRACE").is_ok() {
            eprintln!("step {i}");
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
            for t in &wb.tables {
                eprintln!(
                    "step {i} table {} {} totals={} calc={:?} key={:?} snap={:?}",
                    t.name,
                    t.rect,
                    t.totals,
                    t.calc,
                    t.calc_key,
                    t.calc_snap.iter().map(|(n, _)| n).collect::<Vec<_>>()
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
        let cell = |k: &str| parse_cell_ref(&wb, step[k].as_str().unwrap(), default_sheet).unwrap();

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
            "set" | "typel" => {
                let a = cell("cell");
                let excel_rejected = !rec["error"].is_null();
                typed.insert(
                    step["cell"].as_str().unwrap().to_string(),
                    step["input"].as_str().unwrap().to_string(),
                );
                let input = &*wb
                    .typed_text(a, step["input"].as_str().unwrap())
                    .into_owned();

                let lotus = !input.starts_with('=')
                    && matches!(
                        xlcalc::input::interpret(input, &wb.input_env()),
                        xlcalc::input::Entered::Formula(_)
                    );
                if lotus {
                    wb.set_value(a, Value::from(input));
                    continue;
                }
                match wb.set_input(a, input) {
                    Err(xlcalc::EditError::Locked) if !excel_rejected => {}
                    Err(e) if !excel_rejected => tally.add(Outcome::Wrong, || {
                        format!("step {}: set {} rejected: {e}", i + 1, step["cell"])
                    }),
                    Ok(_) if excel_rejected => tally.add(Outcome::Wrong, || {
                        format!(
                            "step {}: set {} accepted, but Excel rejected it",
                            i + 1,
                            step["cell"]
                        )
                    }),
                    Err(_) => tally.add(Outcome::Exact, String::new),
                    Ok(_) => {}
                }
            }
            "setv" => {
                let a = cell("cell");
                let _ = apply_typed(&mut wb, a, &step["value"]);
            }
            "clear" => {
                let (s, rect) =
                    parse_range_ref(&wb, step["range"].as_str().unwrap(), default_sheet).unwrap();

                let scored = !wb.data_tables.is_empty();
                let r = wb.clear_range(s, rect);
                let excel_err = !rec["error"].is_null();
                if r.is_err() != excel_err {
                    tally.add(Outcome::Wrong, || {
                        format!(
                            "step {}: clear — Excel error {:?}, engine {:?}",
                            i + 1,
                            rec["error"],
                            r.err()
                        )
                    });
                } else if excel_err || scored {
                    tally.add(Outcome::Exact, String::new);
                }
            }
            "calc" => {
                wb.recalc();
            }
            "fullcalc" => {
                wb.full_recalc();
            }
            "rebuild" => {
                wb.rebuild();
            }
            "mode" if step["value"].as_str() == Some("semi") => {
                wb.set_semiautomatic();
            }
            "mode" => {
                wb.set_mode(if step["value"].as_str() == Some("manual") {
                    CalcMode::Manual
                } else {
                    CalcMode::Automatic
                });
            }
            "iteration" => {
                wb.set_iteration(
                    step["enabled"].as_bool().unwrap_or(false),
                    step["max_iter"].as_u64().map(|n| n as u32),
                    step["max_change"].as_f64(),
                );
            }
            "name" => {
                let scope = step["scope"]
                    .as_str()
                    .map(|n| wb.sheet_by_name(n).unwrap().id);
                let excel_rejected = !rec["error"].is_null();
                match wb.define_name_active(
                    step["name"].as_str().unwrap(),
                    step["refers_to"].as_str().unwrap(),
                    scope,
                ) {
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
            "table" => {
                let (s, rect) =
                    parse_range_ref(&wb, step["range"].as_str().unwrap(), default_sheet).unwrap();
                match wb.add_table(step["name"].as_str().unwrap(), s, rect) {
                    Ok(_) => {
                        if step["totals"].as_bool().unwrap_or(false) {
                            let _ = wb.set_table_totals(step["name"].as_str().unwrap(), true);
                        }
                    }

                    Err(xlcalc::EditError::BadName(_)) if !rec["error"].is_null() => {
                        let mut k = wb.tables.len() + 1;
                        while wb.table_by_name(&format!("Table{k}")).is_some() {
                            k += 1;
                        }
                        let _ = wb.add_table(&format!("Table{k}"), s, rect);
                        tally.add(Outcome::Exact, String::new);
                    }
                    Err(_) if !rec["error"].is_null() => tally.add(Outcome::Exact, String::new),
                    Err(e) => tally.add(Outcome::Wrong, || {
                        format!("step {}: table failed: {e}", i + 1)
                    }),
                }
            }
            "table_totals" => {
                let r = wb.set_table_totals(
                    step["name"].as_str().unwrap(),
                    step["value"].as_bool().unwrap_or(false),
                );
                if r.is_err() != !rec["error"].is_null() {
                    tally.add(Outcome::Wrong, || {
                        format!(
                            "step {}: table_totals — Excel error {:?}, engine {:?}",
                            i + 1,
                            rec["error"],
                            r.err()
                        )
                    });
                }
            }
            "table_unlist" | "table_delete" | "table_resize" | "table_rename" | "table_headers" => {
                let name = step["name"].as_str().unwrap();
                let op = step["op"].as_str().unwrap();
                let r = match op {
                    "table_unlist" => wb.unlist_table(name),
                    "table_delete" => wb.delete_table(name),
                    "table_resize" => {
                        let (_, rect) =
                            parse_range_ref(&wb, step["range"].as_str().unwrap(), default_sheet)
                                .unwrap();
                        wb.resize_table(name, rect)
                    }
                    "table_rename" => wb.rename_table(name, step["to"].as_str().unwrap()),
                    _ => wb.set_table_headers(name, step["value"].as_bool().unwrap_or(false)),
                };
                if r.is_err() != !rec["error"].is_null() {
                    tally.add(Outcome::Wrong, || {
                        format!(
                            "step {}: {op} — Excel error {:?}, engine {:?}",
                            i + 1,
                            rec["error"],
                            r.err()
                        )
                    });
                }
            }
            "copy" => {
                let (ss, src) =
                    parse_range_ref(&wb, step["from"].as_str().unwrap(), default_sheet).unwrap();

                let (ds, dr) =
                    parse_range_ref(&wb, step["to"].as_str().unwrap(), default_sheet).unwrap();
                let res = wb.copy_to_range(ss, src, ds, dr).map(|_| ());
                let excel_err = !rec["error"].is_null();
                match res {
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
            "cut" | "insert_cells" | "delete_cells" => {
                let r = if op == "cut" {
                    let (ss, src) =
                        parse_range_ref(&wb, step["from"].as_str().unwrap(), default_sheet)
                            .unwrap();
                    wb.move_range(ss, src, cell("to"))
                } else {
                    let (s, rect) =
                        parse_range_ref(&wb, step["range"].as_str().unwrap(), default_sheet)
                            .unwrap();
                    let shift = step["shift"].as_str().unwrap_or("");
                    if op == "insert_cells" {
                        wb.insert_cells(s, rect, shift != "right")
                    } else {
                        wb.delete_cells(s, rect, shift != "left")
                    }
                };
                let ok = r.is_ok() == rec["error"].is_null();
                tally.add(if ok { Outcome::Exact } else { Outcome::Wrong }, || {
                    format!(
                        "step {}: {op} — Excel error {:?}, engine {:?}",
                        i + 1,
                        rec["error"],
                        r.err()
                    )
                });
            }
            "insert_rows" | "delete_rows" | "insert_cols" | "delete_cols" => {
                let sheet = wb
                    .sheet_by_name(step["sheet"].as_str().unwrap())
                    .unwrap()
                    .id;
                let at = step["at"].as_u64().unwrap() as u32 - 1;
                let count = step["count"].as_u64().unwrap() as u32;
                let r = match op {
                    "insert_rows" => wb.insert_rows(sheet, at, count),
                    "delete_rows" => wb.delete_rows(sheet, at, count),
                    "insert_cols" => wb.insert_cols(sheet, at, count),
                    _ => wb.delete_cols(sheet, at, count),
                };
                let ok = r.is_ok() == rec["error"].is_null();
                tally.add(if ok { Outcome::Exact } else { Outcome::Wrong }, || {
                    format!(
                        "step {}: {op} — Excel error {:?}, engine {:?}",
                        i + 1,
                        rec["error"],
                        r.err()
                    )
                });
            }

            "selinfo" => {}
            "circ" => {
                let want = rec["circular"].as_str().map(str::to_string);

                let got = if wb.calc.circ_iterating {
                    None
                } else {
                    wb.calc.circular_ref.map(|a| a.a1())
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
            "read" => {
                for c in rec["cells"].as_array().into_iter().flatten() {
                    let a1 = c["cell"].as_str().unwrap();
                    let (s, _) =
                        parse_range_ref(&wb, step["range"].as_str().unwrap(), default_sheet)
                            .unwrap();
                    let addr = parse_cell_ref(&wb, a1, s).unwrap();
                    let mut got = cell_result(&wb, addr);
                    if let Some(g) = pinned_to_clock(&mut wb, addr, &c["value"], &got) {
                        got = g;
                    }
                    if std::env::var("CONF_DUMP").is_ok_and(|v| v == (i + 1).to_string()) {
                        eprintln!(
                            "dump {a1} = {} {:?} fmt={:?}",
                            show_value(&got),
                            wb.formula_text(addr),
                            wb.number_format(addr)
                        );
                    }
                    blocksteps::check_blocks(&wb, c, addr, i + 1, &mut tally);
                    let o = compare_value(&c["value"], &got);
                    tally.add(o, || {
                        format!(
                            "step {}: {} {a1} expected {}, got {}",
                            i + 1,
                            step["range"].as_str().unwrap().split('!').next().unwrap(),
                            show_expected(&c["value"]),
                            show_value(&got)
                        )
                    });
                    if check_format && c["value"].is_number() {
                        let sheet = step["range"].as_str().unwrap().split('!').next().unwrap();
                        if let Some(input) = typed.get(&format!("{sheet}!{a1}")) {
                            let want = c["format"].as_str().unwrap_or("General");
                            let got = wb
                                .typed_format(input)
                                .unwrap_or_else(|| "General".to_string());
                            let o = if want == got {
                                Outcome::Exact
                            } else {
                                Outcome::Wrong
                            };
                            tally.add(o, || {
                                format!(
                                    "step {}: {a1} {input:?} format expected {want:?}, got {got:?}",
                                    i + 1
                                )
                            });
                        }
                    }
                    if check_all_formats
                        && (check_empty_formats
                            || !c["value"].is_null()
                            || c["formula"].is_string())
                    {
                        let want = c["format"].as_str().unwrap_or("General");
                        let got = wb.number_format(addr).to_string();
                        let o = if want.eq_ignore_ascii_case(&got) {
                            Outcome::Exact
                        } else {
                            Outcome::Wrong
                        };
                        tally.add(o, || {
                            format!(
                                "step {}: {a1} number format expected {want:?}, got {got:?}",
                                i + 1
                            )
                        });
                    }

                    let want_f = c["formula"].as_str().map(sheetsteps::norm_book);
                    let got_f = wb.formula_text(addr).map(|f| sheetsteps::norm_book(&f));
                    if want_f.is_some() || got_f.is_some() {
                        let o = if want_f == got_f {
                            Outcome::Exact
                        } else {
                            Outcome::Wrong
                        };
                        tally.add(o, || {
                            format!(
                                "step {}: {a1} formula expected {want_f:?}, got {got_f:?}",
                                i + 1
                            )
                        });
                    }
                }
            }
            other
                if blocksteps::run(&mut wb, other, step, default_sheet, rec, i + 1, &mut tally) => {
            }
            other
                if editsteps::run(&mut wb, other, step, default_sheet, rec, i + 1, &mut tally) => {}
            other if sheetsteps::run(&mut wb, other, step, rec, i + 1, &mut tally) => {}
            other => {
                tally.add(Outcome::Wrong, || {
                    format!("step {}: unsupported op '{other}'", i + 1)
                });
            }
        }
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
    tally
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
    let limit: usize = flag("--limit").and_then(|s| s.parse().ok()).unwrap_or(40);
    let skip_values: Vec<String> = [grep.clone(), flag("--limit")]
        .into_iter()
        .flatten()
        .collect();
    let names: Vec<String> = selected(
        &args
            .iter()
            .filter(|a| !skip_values.contains(a))
            .cloned()
            .collect::<Vec<_>>(),
    );
    let (mut te, mut tc, mut tn, mut tw) = (0, 0, 0, 0);
    println!(
        "{:<20} {:>7} {:>7} {:>7} {:>7} {:>8}",
        "suite", "exact", "close", "near", "wrong", "score"
    );
    for n in &names {
        let path = conf_dir().join("golden").join(format!("{n}.json"));
        if !path.exists() {
            println!("{n:<20} (no golden — run `conform record {n}`)");
            continue;
        }
        let golden: J = load_golden(&path);
        let tally = if golden["kind"] == "scenario" {
            run_scenario(&golden)
        } else {
            run_formulas(&golden, grep.as_deref(), check_text)
        };
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
        if verbose {
            for f in tally.failures.iter().take(limit) {
                println!("    {f}");
            }
            if tally.failures.len() > limit {
                println!("    … {} more", tally.failures.len() - limit);
            }
        }
        te += tally.exact;
        tc += tally.close;
        tn += tally.near;
        tw += tally.wrong;
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
    if tn + tw > 0 {
        ExitCode::FAILURE
    } else {
        ExitCode::SUCCESS
    }
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

fn main() -> ExitCode {
    let args: Vec<String> = std::env::args().skip(1).collect();

    let child = std::thread::Builder::new()
        .stack_size(256 * 1024 * 1024)
        .spawn(move || match args.first().map(String::as_str) {
            Some("record") => record(&args[1..]),
            Some("run") => run(&args[1..]),
            Some("compact") => compact_cmd(&args[1..]),
            Some("verify-golden") => verify_golden(&args[1..]),
            Some("eval") => eval_file(&args[1]),
            Some("list") => {
                for (n, (k, _)) in suites() {
                    println!("{n:<20} {k}");
                }
                ExitCode::SUCCESS
            }
            _ => {
                eprintln!("usage: conform record|run|compact|verify-golden|list [suite…] [-v] [--grep TEXT] [--limit N]");
                ExitCode::from(2)
            }
        })
        .unwrap();
    child.join().unwrap()
}

fn eval_file(path: &str) -> ExitCode {
    let mut wb = Workbook::empty();
    wb.calc.fixed_now = None;
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
