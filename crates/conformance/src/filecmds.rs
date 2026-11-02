use crate::{conf_dir, root};
use serde_json::{json, Value as J};
use std::collections::BTreeMap;
use std::fs;
use std::path::{Path, PathBuf};
use std::process::{Command, ExitCode};
use xlcalc::{col_to_letters, CellAddr, Value, Workbook};
use xlcalc_xlsx::{canonical_xml, check_package, save, FileKind, Package, SaveOptions};

pub fn pkgdiff(args: &[String]) -> ExitCode {
    let [a, b] = args else {
        eprintln!("usage: conform pkgdiff <a.xlsx> <b.xlsx>");
        return ExitCode::from(2);
    };
    let read = |p: &String| {
        fs::read(p)
            .map_err(|e| e.to_string())
            .and_then(|b| Package::read(&b))
            .unwrap_or_else(|e| panic!("{p}: {e}"))
    };
    let (pa, pb) = (read(a), read(b));
    let mut names: BTreeMap<String, ()> = BTreeMap::new();
    for n in pa.part_names().chain(pb.part_names()) {
        names.insert(n.to_string(), ());
    }
    let mut differ = 0;
    for n in names.keys() {
        let line = match (pa.part(n), pb.part(n)) {
            (Some(_), None) => format!("only in a     {n}"),
            (None, Some(_)) => format!("only in b     {n}"),
            (Some(x), Some(y)) if x == y => {
                if pa.content_type(n) == pb.content_type(n) {
                    continue;
                }
                format!("content type  {n}")
            }
            (Some(x), Some(y)) => match (canonical_xml(x), canonical_xml(y)) {
                (Some(cx), Some(cy)) if cx == cy => format!("same (xml)    {n}"),
                (Some(cx), Some(cy)) => {
                    let at = cx
                        .lines()
                        .zip(cy.lines())
                        .position(|(l, r)| l != r)
                        .unwrap_or(cx.lines().count().min(cy.lines().count()));
                    let l = cx.lines().nth(at).unwrap_or("<end>").trim();
                    let r = cy.lines().nth(at).unwrap_or("<end>").trim();
                    differ += 1;
                    format!("differs       {n}\n    a: {}\n    b: {}", cut(l), cut(r))
                }
                _ => {
                    differ += 1;
                    format!("differs       {n} ({} vs {} bytes)", x.len(), y.len())
                }
            },
            (None, None) => continue,
        };
        if line.starts_with("only") || line.starts_with("content") {
            differ += 1;
        }
        println!("{line}");
    }
    println!("{differ} part(s) differ");
    if differ == 0 {
        ExitCode::SUCCESS
    } else {
        ExitCode::FAILURE
    }
}

fn cut(s: &str) -> String {
    if s.chars().count() > 160 {
        format!("{}…", s.chars().take(160).collect::<String>())
    } else {
        s.to_string()
    }
}

struct Input {
    id: String,
    src: PathBuf,
    golden: Option<PathBuf>,

    new: Option<String>,

    fresh: bool,
}

fn new_book(name: &str) -> Option<Workbook> {
    let mut wb = Workbook::new();
    let s = wb.sheet_at(0).id;
    let mut put = |r: u32, c: u32, t: &str| {
        wb.set_input(CellAddr::new(s, r, c), t)
            .unwrap_or_else(|e| panic!("{t}: {e:?}"));
    };
    match name {
        "numbers" => {
            let xs: [f64; 16] = [
                0.1,
                0.1 + 0.2,
                1.0 / 3.0,
                2.0 / 3.0,
                0.085,
                1e-300,
                2.225_073_858_507_201_4e-308,
                2.225_073_858_507_202e-308,
                1.797_693_134_862_315_7e308,
                9_007_199_254_740_993.0,
                123_456_789_012_345.67,
                -0.000_123_456_789,
                1e15,
                1e16,
                123_456_789.123_456_79,
                std::f64::consts::PI,
            ];
            for (i, x) in xs.iter().enumerate() {
                let i = i as u32;
                wb.set_value(CellAddr::new(s, i, 0), Value::Number(*x));
                wb.set_value(CellAddr::new(s, i, 1), Value::Number(-x * 7.0));
            }
        }

        "basic" => {
            put(0, 0, "1.5");
            put(0, 1, "text with \"quotes\" & <angles>");
            put(0, 2, "TRUE");
            put(0, 3, "#N/A");
            put(1, 0, "=A1*2");
            put(2, 0, "=SEQUENCE(3)");
            put(2, 1, "=SUM(A3#)");
            put(3, 1, "=XLOOKUP(2,A3:A5,A3:A5)");
            put(4, 1, "=LET(x,A1,x*x)");
            put(5, 1, "12/03/2024");
            put(6, 1, "12.5%");
            put(7, 1, "=TEXT(B6,\"yyyy-mm-dd\")");
            put(8, 1, "  leading space");
            put(9, 1, "=1/0");
        }
        _ => return None,
    }
    Some(wb)
}

fn inputs(args: &[String]) -> Vec<Input> {
    let wb_dir = conf_dir().join("workbooks");
    let names: Vec<String> = if args.is_empty() {
        let mut v: Vec<String> = fs::read_dir(&wb_dir)
            .map(|d| {
                d.flatten()
                    .filter_map(|e| {
                        let p = e.path();
                        (p.extension()?.to_str()? == "xlsx")
                            .then(|| p.file_stem()?.to_str().map(str::to_string))?
                    })
                    .collect()
            })
            .unwrap_or_default();
        v.sort();
        v
    } else {
        args.to_vec()
    };
    names
        .into_iter()
        .map(|n| {
            let (n, fresh) = match n.strip_prefix("fresh:") {
                Some(rest) => (rest.to_string(), true),
                None => (n, false),
            };
            let mut inp = input(n);
            if fresh {
                inp.fresh = true;
                inp.id.push_str("_fresh");
            }
            inp
        })
        .collect()
}

fn input(n: String) -> Input {
    let p = PathBuf::from(&n);
    if let Some(b) = n.strip_prefix("new:") {
        Input {
            id: format!("new_{b}"),
            src: PathBuf::new(),
            golden: None,
            new: Some(b.to_string()),
            fresh: false,
        }
    } else if p.extension().is_some() && p.exists() {
        Input {
            id: p.file_stem().unwrap().to_string_lossy().to_string(),
            src: fs::canonicalize(&p).map_or(p, strip_verbatim),
            golden: None,
            new: None,
            fresh: false,
        }
    } else {
        let g = conf_dir().join("golden").join(format!("{n}.json"));
        Input {
            src: conf_dir().join("workbooks").join(format!("{n}.xlsx")),
            golden: g.exists().then_some(g),
            new: None,
            fresh: false,
            id: n,
        }
    }
}

fn strip_verbatim(p: PathBuf) -> PathBuf {
    match p.to_str().and_then(|s| s.strip_prefix(r"\\?\")) {
        Some(s) => PathBuf::from(s),
        None => p,
    }
}

const SDK_FALSE_POSITIVES: &[(&str, &str, &str)] = &[
    ("/calcChain.xml", "Sem_MissingIndexedElement", ""),
    ("/charts/chart", "Sch_UndeclaredAttribute", "'uri'"),
    (
        "/charts/chart",
        "Sch_InvalidElementContentExpectingComplex",
        "dataDisplayOptions16",
    ),
];

pub fn openxml_validate(paths: &[PathBuf]) -> Option<Vec<(String, String)>> {
    if paths.is_empty() {
        return Some(Vec::new());
    }
    let out = Command::new("pwsh")
        .args(["-NoProfile", "-File"])
        .arg(root().join("oracle").join("openxml_validate.ps1"))
        .args(paths)
        .output()
        .expect("run pwsh");
    if !out.status.success() {
        return None;
    }
    let mut errs = Vec::new();
    for line in String::from_utf8_lossy(&out.stdout).lines() {
        let f: Vec<&str> = line.splitn(5, '\t').collect();
        if f.len() < 5 {
            continue;
        }
        let (path, part, xpath, id, desc) = (f[0], f[1], f[2], f[3], f[4]);
        if SDK_FALSE_POSITIVES
            .iter()
            .any(|(p, i, d)| part.contains(p) && id == *i && desc.contains(d))
        {
            continue;
        }
        errs.push((
            path.to_string(),
            format!("Open XML SDK: {part} {xpath} {id}: {desc}"),
        ));
    }
    Some(errs)
}

pub fn crossread(args: &[String]) -> ExitCode {
    let mut out_dir = std::env::var_os("CONFORM_ORACLE_TMP")
        .map(PathBuf::from)
        .unwrap_or_else(|| conf_dir().join(".oracle-tmp"))
        .join("crossread");
    let mut rest = Vec::new();
    let mut it = args.iter();
    while let Some(a) = it.next() {
        if a == "--out" {
            out_dir = PathBuf::from(it.next().expect("--out DIR"));
        } else {
            rest.push(a.clone());
        }
    }
    fs::create_dir_all(&out_dir).unwrap();
    let mut failed = 0;
    let mut files = Vec::new();
    let mut checked = Vec::new();
    let mut saved_paths: Vec<PathBuf> = Vec::new();
    for inp in inputs(&rest) {
        match save_one(&inp, &out_dir) {
            Ok((saved, warnings)) => {
                for w in warnings {
                    println!("{}: save warning: {w}", inp.id);
                }
                if inp.new.is_none() {
                    files.push(
                        json!({ "id": format!("{}.src", inp.id), "path": inp.src.to_string_lossy() }),
                    );
                }
                files.push(
                    json!({ "id": format!("{}.saved", inp.id), "path": saved.to_string_lossy() }),
                );
                saved_paths.push(saved);
                checked.push(inp);
            }
            Err(e) => {
                println!("FAIL {}: {e}", inp.id);
                failed += 1;
            }
        }
    }

    match openxml_validate(&saved_paths) {
        None => println!(
            "note: Open XML SDK not installed (oracle/fetch_openxml_sdk.ps1): not validated"
        ),
        Some(errs) => {
            let bad: std::collections::BTreeSet<&str> =
                errs.iter().map(|(p, _)| p.as_str()).collect();
            for (p, e) in errs.iter().take(40) {
                println!("FAIL {p}: {e}");
            }
            let mut i = 0;
            checked.retain(|inp| {
                let p = &saved_paths[i];
                i += 1;
                let keep = !bad.contains(p.to_string_lossy().as_ref());
                if !keep {
                    failed += 1;
                    println!(
                        "FAIL {}: Open XML SDK validation ({} error(s))",
                        inp.id,
                        errs.iter()
                            .filter(|(q, _)| q.as_str() == p.to_string_lossy())
                            .count()
                    );
                }
                keep
            });
            files.retain(|f| {
                let id = f["id"].as_str().unwrap_or("");
                match id.strip_suffix(".saved") {
                    Some(base) => checked.iter().any(|c| c.id == base),
                    None => id
                        .strip_suffix(".src")
                        .is_none_or(|b| checked.iter().any(|c| c.id == b)),
                }
            });
        }
    }
    if files.is_empty() {
        return ExitCode::FAILURE;
    }
    let req = out_dir.join("crossread_request.json");
    let resp_path = out_dir.join("crossread_response.json");
    fs::write(&req, json!({ "files": files, "timeout": 1800 }).to_string()).unwrap();
    let _ = fs::remove_file(&resp_path);
    let status = Command::new("pwsh")
        .args(["-NoProfile", "-File"])
        .arg(root().join("oracle").join("xlsx_crossread.ps1"))
        .arg("-In")
        .arg(&req)
        .arg("-Out")
        .arg(&resp_path)
        .status()
        .expect("run pwsh");
    if !status.success() {
        eprintln!("crossread oracle failed: {status}");
        return ExitCode::FAILURE;
    }
    let text = fs::read_to_string(&resp_path).unwrap();
    let resp: J = serde_json::from_str(text.trim_start_matches('\u{feff}')).expect("oracle JSON");
    for inp in &checked {
        let saved = &resp["results"][format!("{}.saved", inp.id)];
        let expected;
        let src = match &inp.new {
            Some(b) => {
                println!("     {}: Excel reads {}", inp.id, saved["book"]);
                expected = engine_record(&new_book(b).unwrap());
                &expected
            }
            None => &resp["results"][format!("{}.src", inp.id)],
        };
        let diffs = compare(inp, src, saved);
        if diffs.is_empty() {
            let cells: usize = saved["calc"].as_array().map_or(0, |a| {
                a.iter()
                    .map(|i| i["cells"].as_array().map_or(0, Vec::len))
                    .sum()
            });
            println!("ok   {} ({cells} cells)", inp.id);
        } else {
            failed += 1;
            println!("FAIL {}: {} difference(s)", inp.id, diffs.len());
            for d in diffs.iter().take(12) {
                println!("    {d}");
            }
        }
    }
    println!("crossread: {} input(s), {failed} failed", checked.len());
    if failed == 0 {
        ExitCode::SUCCESS
    } else {
        ExitCode::FAILURE
    }
}

fn save_one(inp: &Input, out_dir: &Path) -> Result<(PathBuf, Vec<String>), String> {
    if let Some(b) = &inp.new {
        let wb = new_book(b).ok_or_else(|| format!("no new book {b:?} (numbers, basic)"))?;
        let saved = save(&wb, None, &SaveOptions::default()).map_err(|e| format!("save: {e}"))?;
        let problems = check_package(&saved.bytes);
        if !problems.is_empty() {
            return Err(format!("package check (Excel not run): {problems:?}"));
        }
        let path = out_dir.join(format!("{}.saved.xlsx", inp.id));
        fs::write(&path, &saved.bytes).map_err(|e| e.to_string())?;
        return Ok((
            fs::canonicalize(&path).map_or(path, strip_verbatim),
            saved.warnings,
        ));
    }
    let bytes = fs::read(&inp.src).map_err(|e| format!("{}: {e}", inp.src.display()))?;
    let imp = xlcalc_xlsx::import_bytes(bytes).map_err(|e| format!("import: {e}"))?;
    let ext = inp
        .src
        .extension()
        .and_then(|e| e.to_str())
        .unwrap_or("xlsx")
        .to_lowercase();
    let kind = if ext == "xlsm" {
        FileKind::Xlsm
    } else {
        FileKind::Xlsx
    };
    let opts = SaveOptions {
        kind: Some(kind),
        fresh_styles: inp.fresh,
        ..SaveOptions::default()
    };
    let saved = save(&imp.workbook, Some(&imp.package), &opts).map_err(|e| format!("save: {e}"))?;
    let problems = check_package(&saved.bytes);
    if !problems.is_empty() {
        return Err(format!("package check (Excel not run): {problems:?}"));
    }
    let path = out_dir.join(format!("{}.saved.{ext}", inp.id));
    fs::write(&path, &saved.bytes).map_err(|e| e.to_string())?;
    Ok((
        fs::canonicalize(&path).map_or(path, strip_verbatim),
        saved.warnings,
    ))
}

fn engine_record(wb: &Workbook) -> J {
    let mut items = Vec::new();
    for sh in wb.sheets() {
        let mut cells = Vec::new();
        if let Some(u) = wb.used_range(sh.id) {
            for r in u.r0..=u.r1 {
                for c in u.c0..=u.c1 {
                    let a = CellAddr::new(sh.id, r, c);
                    let v = wb.value(a);
                    let f = wb.formula_text(a);
                    if matches!(v, Value::Blank) && f.is_none() {
                        continue;
                    }
                    let value = match &v {
                        Value::Number(n) => {
                            json!({ "t": "n", "v": n, "bits": format!("{:016X}", n.to_bits()) })
                        }
                        Value::Text(t) => json!({ "t": "s", "v": t.to_string() }),
                        Value::Bool(b) => json!({ "t": "b", "v": b }),
                        Value::Error(e) => json!({ "t": "e", "v": e.to_string() }),
                        _ => json!({ "t": "blank" }),
                    };
                    let mut o = json!({ "cell": format!("{}{}", col_to_letters(c), r + 1), "value": value });
                    if let Some(f) = f {
                        o["formula"] = J::from(f);
                    }
                    cells.push(o);
                }
            }
        }
        items.push(json!({ "op": "read", "sheet": sh.name.to_string(), "cells": cells }));
    }
    json!({ "open": items, "calc": items })
}

fn cells(items: &J) -> BTreeMap<String, J> {
    let mut m = BTreeMap::new();
    for it in items.as_array().into_iter().flatten() {
        let sheet = it["sheet"].as_str().unwrap_or("");
        for c in it["cells"].as_array().into_iter().flatten() {
            m.insert(
                format!("{sheet}!{}", c["cell"].as_str().unwrap_or("")),
                c.clone(),
            );
        }
    }
    m
}

fn fresh_open(src: &J) -> J {
    let calc = cells(&src["calc"]);
    let mut open = src["open"].clone();
    for it in open.as_array_mut().into_iter().flatten() {
        let sheet = it["sheet"].as_str().unwrap_or("").to_string();
        for c in it["cells"].as_array_mut().into_iter().flatten() {
            let k = format!("{sheet}!{}", c["cell"].as_str().unwrap_or(""));
            if let Some(f) = calc.get(&k) {
                if c.get("formula").is_some() && f["value"] != c["value"] {
                    *c = f.clone();
                }
            }
        }
    }
    open
}

fn diff_cells(what: &str, a: &J, b: &J, values_only: bool, out: &mut Vec<String>) {
    let (ca, mut cb) = (cells(a), cells(b));
    if values_only {
        for c in cb.values_mut() {
            if let Some(o) = c.as_object_mut() {
                o.remove("text");
                o.remove("format");
                if let Some(v) = o.get_mut("value").and_then(J::as_object_mut) {
                    if v.get("t").and_then(J::as_str) == Some("n") {
                        v.remove("v");
                    }
                }
            }
        }
    }
    let ca: BTreeMap<String, J> = if values_only {
        ca.into_iter()
            .map(|(k, mut c)| {
                if let Some(v) = c["value"].as_object_mut() {
                    if v.get("t").and_then(J::as_str) == Some("n") {
                        v.remove("v");
                    }
                }
                (k, c)
            })
            .collect()
    } else {
        ca
    };
    for (k, va) in &ca {
        match cb.get(k) {
            Some(vb) if vb == va => {}
            Some(vb) => out.push(format!("{what} {k}: source {va} | saved {vb}")),
            None => out.push(format!("{what} {k}: missing in saved ({va})")),
        }
    }
    for (k, vb) in &cb {
        if !ca.contains_key(k) {
            out.push(format!("{what} {k}: only in saved ({vb})"));
        }
    }
}

fn style_cells(rec: &J) -> BTreeMap<String, String> {
    let mut m = BTreeMap::new();
    for sh in rec.as_array().into_iter().flatten() {
        let sheet = sh["sheet"].as_str().unwrap_or("");
        let sigs = sh["sigs"].as_array().cloned().unwrap_or_default();
        for b in sh["blocks"].as_array().into_iter().flatten() {
            let n = |i: usize| b[i].as_u64().unwrap_or(0) as u32;
            let sig = sigs
                .get(n(4) as usize)
                .map_or_else(String::new, J::to_string);
            for r in n(0)..n(0) + n(2) {
                for c in n(1)..n(1) + n(3) {
                    m.insert(format!("{sheet}!{}{r}", col_to_letters(c - 1)), sig.clone());
                }
            }
        }
    }
    m
}

fn sig_diff(x: Option<&String>, y: Option<&String>) -> String {
    let parse = |s: Option<&String>| s.and_then(|s| serde_json::from_str::<J>(s).ok());
    let (Some(a), Some(b)) = (parse(x), parse(y)) else {
        return format!("source {x:?} | saved {y:?}");
    };
    let mut parts = Vec::new();
    for g in ["props", "borders"] {
        match (a[g].as_object(), b[g].as_object()) {
            (Some(oa), Some(ob)) => {
                for (k, va) in oa {
                    let vb = ob.get(k).unwrap_or(&J::Null);
                    if va != vb {
                        parts.push(format!("{k}: {va} | {vb}"));
                    }
                }
            }
            _ if a[g] != b[g] => parts.push(format!("{g}: {} | {}", a[g], b[g])),
            _ => {}
        }
    }
    cut(&parts.join("; "))
}

fn diff_styles(a: &J, b: &J, out: &mut Vec<String>) {
    if a.is_null() {
        return;
    }
    let (ma, mb) = (style_cells(a), style_cells(b));
    let mut n = 0;
    let mut first = Vec::new();
    for k in ma.keys().chain(mb.keys().filter(|k| !ma.contains_key(*k))) {
        let (x, y) = (ma.get(k), mb.get(k));
        if x != y {
            n += 1;
            if first.len() < 4 {
                first.push(format!("style {k}: {}", sig_diff(x, y)));
            }
        }
    }
    if n > first.len() {
        first.push(format!("style: {n} cell(s) differ in all"));
    }
    out.extend(first);
}

fn compare(inp: &Input, src: &J, saved: &J) -> Vec<String> {
    let mut d = Vec::new();

    let renamed: J = serde_json::from_str(&saved.to_string().replace(
        &format!("{}.saved.", inp.id),
        &format!("{}.", inp.id.trim_end_matches("_fresh")),
    ))
    .unwrap_or_else(|_| saved.clone());
    let saved = &renamed;
    if let Some(e) = saved["error"].as_str() {
        d.push(format!("Excel could not open the saved file: {e}"));
        return d;
    }
    if let Some(e) = src["error"].as_str() {
        d.push(format!("Excel could not open the source: {e}"));
        return d;
    }
    if saved["repaired"].as_bool() == Some(true) {
        d.push(format!(
            "Excel repaired the saved file: {}",
            cut(saved["repairLog"].as_str().unwrap_or(""))
        ));
    }
    let (ba, bb) = (&src["book"], &saved["book"]);
    if let (Some(oa), Some(ob)) = (ba.as_object(), bb.as_object()) {
        for (k, va) in oa {
            let vb = ob.get(k).unwrap_or(&J::Null);
            if va != vb {
                d.push(format!(
                    "book {k}: source {} | saved {}",
                    cut(&va.to_string()),
                    cut(&vb.to_string())
                ));
            }
        }
    }
    diff_styles(&src["styles"], &saved["styles"], &mut d);
    let new = inp.new.is_some();

    let iterative = src["book"]["iteration"].as_bool() == Some(true);
    let saved_after = if iterative { "open" } else { "calc" };
    if iterative {
        diff_cells("open/calc", &src["calc"], &saved["open"], new, &mut d);
    } else {
        diff_cells("open", &fresh_open(src), &saved["open"], new, &mut d);
        diff_cells("calc", &src["calc"], &saved["calc"], new, &mut d);
    }

    if let Some(g) = &inp.golden {
        let golden = crate::golden::load_golden(g);
        let want = cells(&golden["items"]);
        let got = cells(&saved[saved_after]);
        for (k, w) in &want {
            let v = got.get(k).map(|c| &c["value"]);
            if v != Some(&w["value"]) {
                d.push(format!(
                    "golden {k}: {} | saved {}",
                    w["value"],
                    v.unwrap_or(&J::Null)
                ));
            }
        }
    }
    d
}
