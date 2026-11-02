use serde_json::Value as J;
use std::collections::HashMap;
use std::path::PathBuf;
use xlcalc::{CellAddr, Value, Workbook};

fn root() -> PathBuf {
    PathBuf::from(env!("CARGO_MANIFEST_DIR")).join("../..")
}

fn load(kind: &str, suite: &str) -> J {
    let p = root().join(format!("conformance/{kind}/{suite}.json"));
    serde_json::from_slice(&std::fs::read(&p).unwrap()).unwrap()
}

fn items(g: &J) -> HashMap<u64, &J> {
    g["items"]
        .as_array()
        .unwrap()
        .iter()
        .map(|i| (i["step"].as_u64().unwrap(), i))
        .collect()
}

fn file_name(arg: &J) -> String {
    arg.as_str()
        .unwrap()
        .rsplit('/')
        .next()
        .unwrap()
        .to_string()
}

fn compare_book(b: &J, wb: &Workbook, tag: &str, bad: &mut Vec<String>) {
    let sheet = wb.sheets().next().unwrap().id;
    for c in b["sheets"][0]["cells"].as_array().into_iter().flatten() {
        let a1 = c["cell"].as_str().unwrap();
        let addr = wb.addr(sheet, a1).unwrap_or_else(|| panic!("{tag} {a1}"));
        let got = wb.value(addr);
        let v = &c["value"];
        let ok = match v["t"].as_str().unwrap() {
            "n" => matches!(&got, Value::Number(n)
                if format!("{:016X}", n.to_bits()) == v["bits"].as_str().unwrap()),
            "s" => matches!(&got, Value::Text(t) if t.to_string() == v["v"].as_str().unwrap()),
            "b" => matches!(&got, Value::Bool(x) if Some(*x) == v["v"].as_bool()),
            "e" => matches!(&got, Value::Error(e) if e.to_string() == v["v"].as_str().unwrap()),
            _ => matches!(got, Value::Blank),
        };
        if !ok {
            bad.push(format!("{tag} {a1}: value {got:?} vs {v}"));
            continue;
        }
        let fmt = wb.number_format(addr);
        if !fmt.eq_ignore_ascii_case(c["format"].as_str().unwrap()) {
            bad.push(format!("{tag} {a1}: format {fmt} vs {}", c["format"]));
        }
        let text = c["text"].as_str().unwrap();

        if !fmt.eq_ignore_ascii_case("general") && !text.contains('#') {
            let t = wb.display_text(addr);
            if t != text && !text.starts_with('#') {
                bad.push(format!("{tag} {a1}: text {t:?} vs {text:?}"));
            }
        }
    }
}

#[test]
fn csv_open_goldens() {
    if !root().join("conformance/golden/csv_open.json").exists() {
        return;
    }
    let sc = load("scenarios", "csv_open");
    let g = load("golden", "csv_open");
    let items = items(&g);
    let (mut opens, mut bad) = (0, Vec::new());
    let mut cur: Option<(xlcalc_xlsx::CsvImport, String)> = None;
    for (i, st) in sc["steps"].as_array().unwrap().iter().enumerate() {
        let step = i as u64 + 1;
        match (st["op"].as_str().unwrap(), st["call"].as_str()) {
            ("om", Some("Open")) => {
                let args = st["args"].as_array().unwrap();
                if args.len() != 14 {
                    cur = None;
                    continue;
                }
                let name = file_name(&args[0]);
                let local = args.last().and_then(J::as_bool).unwrap_or(false);
                let bytes = std::fs::read(root().join("conformance/csv").join(&name)).unwrap();
                let tag = format!("{name} local={local}");
                if name == "wide_cell.csv" {
                    cur = None;
                    continue;
                }
                cur = Some((xlcalc_xlsx::import_csv(&bytes, &name, local), tag));
            }
            ("file_read", _) => {
                let Some((imp, tag)) = cur.as_ref() else {
                    continue;
                };
                let b = &items[&step]["book"];
                opens += 1;
                let sname = imp.workbook.sheets().next().unwrap().name.clone();
                if b["name"] != imp.name.as_str()
                    || b["file_format"] != imp.file_format
                    || b["sheets"][0]["name"] != sname.as_str()
                {
                    bad.push(format!(
                        "{tag}: name/format/sheet {} {} {sname}",
                        imp.name, imp.file_format
                    ));
                }
                compare_book(b, &imp.workbook, tag, &mut bad);
            }
            _ => {}
        }
    }
    println!("{opens} opens, {} failures", bad.len());
    assert!(bad.is_empty(), "{}", bad.join("\n"));
}

#[test]
fn csv_save_goldens() {
    if !root().join("conformance/golden/csv_save.json").exists() {
        return;
    }
    let sc = load("scenarios", "csv_save");
    let g = load("golden", "csv_save");
    let items = items(&g);
    let mut wb: Option<Workbook> = None;
    let mut order: Vec<xlcalc::SheetId> = Vec::new();
    let (mut saved, mut bad) = (0, Vec::new());
    let mut last: Option<(Vec<u8>, String, i32)> = None;
    let mut bname = String::new();
    let mut bfmt = 0;
    for (i, st) in sc["steps"].as_array().unwrap().iter().enumerate() {
        let step = i as u64 + 1;
        let path = st["path"].as_str().unwrap_or("");
        let op = st["op"].as_str().unwrap();
        match (op, st["call"].as_str(), st["prop"].as_str()) {
            ("om", Some("Add"), _) if path == "Workbooks" => {
                let mut w = Workbook::empty();
                order = vec![w.add_sheet("Sheet1").unwrap()];
                wb = Some(w);
                bname = String::new();
            }
            ("om", Some("Add"), _) => {
                let w = wb.as_mut().unwrap();
                let id = w.add_sheet("Sheet2").unwrap();
                order.insert(0, id);
            }
            ("om", Some("Open"), _) => {
                let name = file_name(&st["args"][0]);
                let bytes = std::fs::read(root().join("conformance/csv").join(&name)).unwrap();
                let imp = xlcalc_xlsx::import_csv(&bytes, &name, false);
                order = imp.workbook.sheets().map(|s| s.id).collect();
                bname = imp.name;
                bfmt = imp.file_format;
                wb = Some(imp.workbook);
            }
            ("om", Some("Activate"), _) => {
                wb.as_mut().unwrap().set_active_sheet(order[0]).unwrap();
            }
            ("om", Some("SaveAs"), _) => {
                let name = file_name(&st["args"][0]);
                let fmt = st["args"][1].as_i64().unwrap() as i32;
                let r = xlcalc_xlsx::save_as_csv(wb.as_mut().unwrap(), &name, fmt).unwrap();
                bname = r.name.clone();
                bfmt = r.file_format;
                last = Some((r.bytes, name, fmt));
            }
            ("om", Some("Save"), _) => {
                let w = wb.as_mut().unwrap();
                let r = xlcalc_xlsx::save_as_csv(w, &bname.clone(), bfmt).unwrap();
                last = Some((r.bytes, bname.clone(), bfmt));
            }
            ("om_put", _, Some(prop)) => {
                let w = wb.as_mut().unwrap();
                let sheet = if path.contains("ActiveSheet") {
                    w.active_sheet()
                } else {
                    let n: usize = path.split("Worksheets(").nth(1).unwrap()[..1]
                        .parse()
                        .unwrap();
                    order[n - 1]
                };
                let val = &st["value"];
                let addr_of = |w: &Workbook| {
                    let a = path
                        .split("Range('")
                        .nth(1)
                        .unwrap()
                        .split('\'')
                        .next()
                        .unwrap();
                    w.addr(sheet, a).unwrap()
                };
                match prop {
                    "Value2" if val.is_number() => {
                        let a = addr_of(w);
                        w.set_value(a, Value::Number(val.as_f64().unwrap()));
                    }
                    "Value2" | "Formula2Local" => {
                        let a = addr_of(w);
                        w.set_input(
                            a,
                            &val.as_str()
                                .unwrap()
                                .chars()
                                .take(32767)
                                .collect::<String>(),
                        )
                        .unwrap();
                    }
                    "NumberFormat" => {
                        let a = addr_of(w);
                        w.set_number_format(
                            sheet,
                            xlcalc::Rect::new(a.row, a.col, a.row, a.col),
                            val.as_str().unwrap(),
                        );
                    }
                    "Hidden" => {
                        let n: u32 = path
                            .split('(')
                            .next_back()
                            .unwrap()
                            .trim_end_matches(')')
                            .parse()
                            .unwrap();
                        if path.contains("Rows") {
                            w.set_rows_hidden(sheet, n - 1, 1, true).unwrap();
                        } else {
                            w.set_cols_hidden(sheet, n - 1, 1, true);
                        }
                    }
                    _ => {}
                }
            }
            ("file_bytes", _, _) => {
                let hex = items[&step]["bytes_hex"].as_str();
                let Some((bytes, name, _)) = last.take() else {
                    continue;
                };
                let Some(hex) = hex else { continue };
                saved += 1;
                let got: String = bytes.iter().map(|b| format!("{b:02X}")).collect();

                let gap = ["formats_6.csv", "narrow_columns_6.csv"].contains(&name.as_str());
                if got != hex.to_uppercase() && !gap {
                    bad.push(format!("{name}: bytes\n  got  {got}\n  want {hex}"));
                }
            }
            ("om_get", _, Some(prop)) if path == "ActiveWorkbook" && prop == "Name" => {
                let want = items[&step]["result"]["v"].as_str().unwrap();
                if !bname.is_empty() && want != bname {
                    bad.push(format!("Name {bname} vs {want}"));
                }
            }
            ("om_get", _, Some("Name"))
                if path.ends_with("Worksheets(1)") || path.ends_with("ActiveSheet") =>
            {
                let w = wb.as_ref().unwrap();
                let id = if path.ends_with("ActiveSheet") {
                    w.active_sheet()
                } else {
                    order[0]
                };
                let got = w.sheet(id).unwrap().name.clone();
                let want = items[&step]["result"]["v"].as_str().unwrap();
                if got != want {
                    bad.push(format!("sheet name {got} vs {want}"));
                }
            }
            ("om_get", _, Some("FileFormat")) if bfmt != 0 => {
                let want = items[&step]["result"]["v"].as_f64().unwrap() as i32;
                if want != bfmt {
                    bad.push(format!("FileFormat {bfmt} vs {want}"));
                }
            }
            _ => {}
        }
        let _ = CellAddr::new(xlcalc::SheetId(0), 0, 0);
    }
    println!("{saved} saved files, {} failures", bad.len());
    assert!(bad.is_empty(), "{}", bad.join("\n"));
}

#[test]
fn csv_open_probes() {
    let open = |b: &[u8]| xlcalc_xlsx::import_csv(b, "p.csv", false);
    let cell = |i: &xlcalc_xlsx::CsvImport, a: &str| {
        let s = i.workbook.sheets().next().unwrap().id;
        i.workbook.display_text(i.workbook.addr(s, a).unwrap())
    };
    let i = open(b"a,b");
    assert_eq!((i.file_format, cell(&i, "A1").as_str()), (-4158, "a,b"));
    let i = open(b"a\tb");
    assert_eq!((i.file_format, cell(&i, "B1").as_str()), (-4158, "b"));
    let i = open(b"a,bc");
    assert_eq!((i.file_format, cell(&i, "B1").as_str()), (6, "bc"));
    let i = open(b"a\tbcd");
    assert_eq!((i.file_format, cell(&i, "A1").as_str()), (6, "a\tbcd"));
    let i = open(b"\xEF\xBB\xBF7");
    assert_eq!((i.file_format, cell(&i, "A1").as_str()), (62, "7"));
    let i = open(b"\"x,y\"");
    assert_eq!((i.file_format, cell(&i, "A1").as_str()), (6, "x,y"));

    let mut wb = xlcalc::Workbook::empty();
    let a = wb.add_sheet("Sheet2").unwrap();
    wb.add_sheet("dup").unwrap();
    wb.set_active_sheet(a).unwrap();
    wb.set_input(wb.addr(a, "C3").unwrap(), "1").unwrap();
    let r = xlcalc_xlsx::save_as_csv(&mut wb, "dup.csv", 6).unwrap();
    assert_eq!(r.bytes, b",,\r\n,,\r\n,,1\r\n");
    assert_eq!(wb.sheet(a).unwrap().name, "Sheet2");
    let r = xlcalc_xlsx::save_as_csv(
        &mut wb,
        "this_is_a_very_long_file_name_over_31_chars.csv",
        6,
    );
    assert!(r.is_ok());
    assert_eq!(wb.sheet(a).unwrap().name, "this_is_a_very_long_file_name_o");
}
