use std::path::PathBuf;
use xlcalc_vbaproject::ModuleKind;

fn data(name: &str) -> PathBuf {
    PathBuf::from(env!("CARGO_MANIFEST_DIR"))
        .join("../vbaproject/tests/data")
        .join(name)
}

fn check(fixture: &str) {
    let imp = xlcalc_xlsx::import_file(&data(&format!("{fixture}.xlsm"))).unwrap();
    let vba = imp.vba.expect("xl/vbaProject.bin kept");
    let project = vba.project.as_ref().unwrap();
    let json: serde_json::Value = serde_json::from_str(
        &std::fs::read_to_string(data(&format!("{fixture}.vbe.json"))).unwrap(),
    )
    .unwrap();
    let want = json["modules"].as_array().unwrap();
    assert_eq!(project.modules.len(), want.len(), "{fixture}: module count");
    assert_eq!(project.code_page, 1252);
    for w in want {
        let name = w["name"].as_str().unwrap();
        let m = project
            .modules
            .iter()
            .find(|m| m.name == name)
            .unwrap_or_else(|| panic!("{fixture}: module {name} missing"));
        let kind = match w["type"].as_i64().unwrap() {
            1 => ModuleKind::Standard,
            2 => ModuleKind::Class,
            100 => ModuleKind::Document,
            t => panic!("type {t}"),
        };
        assert_eq!(m.kind, kind, "{fixture}/{name}: kind");

        let got = m.code().unwrap_or_else(|e| panic!("{fixture}/{name}: {e}"));
        let got = got.strip_suffix("\r\n").unwrap_or(&got);
        let text = w["text"].as_str().unwrap();
        if let Some(i) = got.bytes().zip(text.bytes()).position(|(a, b)| a != b) {
            panic!(
                "{fixture}/{name}: first difference at byte {i} of {}/{}",
                got.len(),
                text.len()
            );
        }
        assert_eq!(got, text, "{fixture}/{name}: text");
        assert_eq!(
            got.split("\r\n").count() - usize::from(got.is_empty()),
            w["lines"].as_u64().unwrap() as usize,
            "{fixture}/{name}: line count"
        );
    }

    let wb_cn = vba.workbook_code_name.as_deref().unwrap();
    assert_eq!(wb_cn, json["workbookCodeName"].as_str().unwrap());
    assert!(project.document_module(wb_cn).is_some());
    for (s, w) in vba
        .sheet_code_names
        .iter()
        .zip(json["sheets"].as_array().unwrap())
    {
        assert_eq!(s.0, w["name"].as_str().unwrap());
        assert_eq!(s.1.as_deref(), w["codeName"].as_str());
        assert!(project.document_module(s.1.as_deref().unwrap()).is_some());
    }
}

#[test]
fn basic() {
    check("basic");
}
#[test]
fn lines() {
    check("lines");
}
#[test]
fn cp1252() {
    check("cp1252");
}
#[test]
fn many() {
    check("many");
}
#[test]
fn big() {
    check("big");
}

#[test]
fn big_modules_cross_chunk_boundaries() {
    let imp = xlcalc_xlsx::import_file(&data("big.xlsm")).unwrap();
    let project = imp.vba.unwrap().project.unwrap();
    for m in &project.modules {
        if m.name.starts_with("Big") {
            let n = m.source.as_ref().unwrap().len();
            assert!(n > 3 * 4096, "{}: {n} bytes", m.name);
        }
    }
}

#[test]
fn incompressible_module_is_an_explicit_error() {
    let imp = xlcalc_xlsx::import_file(&data("noise.xlsm")).unwrap();
    let project = imp.vba.unwrap().project.unwrap();
    let m = project
        .modules
        .iter()
        .find(|m| m.name == "BigNoise")
        .unwrap();
    assert!(m.source.is_err());
    assert!(project.modules.iter().any(|m| m.source.is_ok()));
}

#[test]
fn xlsx_has_no_vba() {
    let p =
        PathBuf::from(env!("CARGO_MANIFEST_DIR")).join("../../conformance/workbooks/budget.xlsx");
    assert!(xlcalc_xlsx::import_file(&p).unwrap().vba.is_none());
}
