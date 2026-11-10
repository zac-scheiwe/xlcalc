#![cfg(windows)]

use std::sync::Arc;

use xlcalc::{CellAddr, SheetId, Value, Workbook};
use xlcalc_xll::XllHost;

fn built() -> Option<String> {
    let dir = std::env::current_exe()
        .ok()?
        .parent()?
        .parent()?
        .to_path_buf();
    let stub = dir.join("xlcall32.dll");
    let dll = dir.join("xlc_testxll.dll");
    (stub.exists() && dll.exists()).then(|| dll.to_string_lossy().into_owned())
}

fn at(sheet: SheetId, a1: &str) -> CellAddr {
    let letters: String = a1.chars().take_while(char::is_ascii_alphabetic).collect();
    let row: u32 = a1[letters.len()..].parse().expect("row");
    let col = letters
        .bytes()
        .fold(0u32, |n, b| n * 26 + u32::from(b - b'A' + 1));
    CellAddr {
        sheet,
        row: row - 1,
        col: col - 1,
    }
}

#[test]
fn old_callbacks_follow_excel() {
    let Some(path) = built() else {
        eprintln!("skipped: xlc_testxll.dll or xlcall32.dll is not built");
        return;
    };
    let host = XllHost::load(&path).expect("the test XLL loads");
    assert!(
        host.function_names().iter().any(|n| n == "XLC4CALL"),
        "registered through Excel4v: {:?}",
        host.function_names()
    );
    let golden = include_str!("../../../conformance/com/xll_probe6.golden.txt");
    let mut bad = Vec::new();
    let mut n = 0;
    for line in golden.lines() {
        let Some(cell) = line.split(' ').next() else {
            continue;
        };
        let (formula, want) = match line
            .split_once(' ')
            .and_then(|(_, r)| r.split_once(" -> ["))
        {
            Some((f, w)) => (f, w.strip_suffix(']').unwrap_or(w)),
            None => continue,
        };

        if cell.contains(':') || !formula.starts_with('=') {
            continue;
        }
        let mut wb = Workbook::new();
        wb.set_function_host(Some(Arc::new(host.clone())));
        let s1 = wb.active_sheet();
        let s2 = wb.add_sheet("Other").expect("a second sheet");
        wb.set_value(at(s2, "A1"), Value::Number(9.0));
        wb.set_value(at(s2, "B2"), Value::Number(5.0));
        wb.set_value(at(s1, "B1"), Value::Number(5.0));
        wb.set_value(at(s1, "B2"), Value::Text("txt".into()));
        wb.set_value(at(s1, "B3"), Value::Bool(true));
        wb.set_formula(at(s1, "C1"), "=B1*2").unwrap();
        wb.set_value(at(s1, "C2"), Value::Number(7.0));
        wb.set_formula(at(s1, "C3"), "=1/0").unwrap();
        wb.set_value(at(s1, "E1"), Value::Number(7.0));
        wb.set_value(at(s1, "E2"), Value::Number(2.5));
        let target = at(s1, cell);
        n += 1;
        if let Err(e) = wb.set_legacy_input(target, formula) {
            bad.push(format!("{cell} {formula}: error {e:?}, Excel [{want}]"));
            continue;
        }
        let mut got = wb.display_text(target);

        let mut want = want.to_string();
        if formula.contains("(16393)") {
            got = got
                .rsplit(std::path::MAIN_SEPARATOR)
                .next()
                .unwrap_or_default()
                .to_string();
            want = want
                .rsplit(std::path::MAIN_SEPARATOR)
                .next()
                .unwrap_or_default()
                .to_string();
        }
        if got != want {
            bad.push(format!("{cell} {formula}: got [{got}], Excel [{want}]"));
        }
    }
    assert!(n > 200, "the golden has its lines ({n})");
    assert!(bad.is_empty(), "{} differ:\n{}", bad.len(), bad.join("\n"));
}
