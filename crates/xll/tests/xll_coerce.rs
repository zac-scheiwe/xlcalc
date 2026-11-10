#![cfg(windows)]

use std::sync::Arc;

use xlcalc::{CellAddr, SheetId, Value, Workbook};
use xlcalc_xll::XllHost;

fn testxll() -> Option<String> {
    let exe = std::env::current_exe().ok()?;
    let dll = exe.parent()?.parent()?.join("xlc_testxll.dll");
    dll.exists().then(|| dll.to_string_lossy().into_owned())
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

fn workbook(host: &XllHost) -> (Workbook, SheetId) {
    let mut wb = Workbook::new();
    wb.set_function_host(Some(Arc::new(host.clone())));
    let s1 = wb.active_sheet();
    let s2 = wb.add_sheet("Sheet2").expect("a second sheet");
    wb.set_value(at(s2, "A1"), Value::Number(42.0));
    wb.set_value(at(s1, "H1"), Value::Number(1.5));
    wb.set_value(at(s1, "H2"), Value::Text("abc".into()));
    wb.set_value(at(s1, "H3"), Value::Bool(true));
    wb.set_formula(at(s1, "H4"), "=1/0").unwrap();
    wb.set_value(at(s1, "H6"), Value::Number(3.0));
    wb.set_value(at(s1, "H7"), Value::Number(2.5));
    wb.set_value(at(s1, "H8"), Value::Number(-2.5));
    wb.set_formula(at(s1, "H9"), "=NA()").unwrap();
    wb.set_formula(at(s1, "H10"), "=\"\"").unwrap();
    wb.set_value(at(s1, "H11"), Value::Text("x".repeat(300).into()));
    wb.set_value(at(s1, "H12"), Value::Text("y".repeat(32767).into()));
    let accents = format!("{}{}", "\u{e9}".repeat(40), "\u{20ac}".repeat(5));
    wb.set_value(at(s1, "H13"), Value::Text(accents.into()));
    wb.set_value(at(s1, "J1"), Value::Number(1.0));
    wb.set_value(at(s1, "K1"), Value::Number(2.0));
    wb.set_value(at(s1, "J2"), Value::Number(3.0));
    wb.set_value(at(s1, "K2"), Value::Text("x".into()));
    wb.set_value(at(s1, "M1"), Value::Text("a".into()));
    wb.set_value(at(s1, "M3"), Value::Number(5.0));
    wb.set_value(at(s1, "N1"), Value::Bool(true));
    wb.set_value(at(s1, "N2"), Value::Number(1.0));
    wb.set_value(at(s1, "P1"), Value::Number(3.0));
    wb.set_value(at(s1, "P2"), Value::Number(2.0));
    (wb, s1)
}

#[test]
fn argument_coercion_follows_excel() {
    let Some(path) = testxll() else {
        eprintln!("skipped: xlc_testxll.dll is not built");
        return;
    };
    let host = XllHost::load(&path).expect("the test XLL loads");
    let golden = include_str!("../../../conformance/com/xll_probe5.golden.txt");
    let only = std::env::var("XLL_COERCE_ONLY").ok();
    let mut bad = Vec::new();
    let mut n = 0;
    for line in golden.lines() {
        let Some((formula, rest)) = line.split_once(" -> [") else {
            continue;
        };
        let Some((want, _)) = rest.split_once("] v2=[") else {
            continue;
        };
        if only.as_deref().is_some_and(|o| !formula.contains(o)) {
            continue;
        }
        let (mut wb, s1) = workbook(&host);
        let cell = at(s1, "Z50");
        n += 1;
        if let Err(e) = wb.set_legacy_input(cell, formula) {
            bad.push(format!("{formula}: error {e:?}, Excel [{want}]"));
            continue;
        }
        let got = wb.display_text(cell);
        if got != want {
            bad.push(format!("{formula}: got [{got}], Excel [{want}]"));
        }
    }
    assert!(only.is_some() || n > 1000, "the golden has its lines ({n})");
    assert!(bad.is_empty(), "{} differ:\n{}", bad.len(), bad.join("\n"));
}

#[test]
fn reference_kinds_follow_excel() {
    let Some(path) = testxll() else {
        eprintln!("skipped: xlc_testxll.dll is not built");
        return;
    };
    let host = XllHost::load(&path).expect("the test XLL loads");
    let golden = include_str!("../../../conformance/com/xll_probe7.golden.txt");
    let mut bad = Vec::new();
    let mut n = 0;
    for line in golden.lines() {
        let Some((form, rest)) = line.split_once(' ') else {
            continue;
        };
        let Some((formula, want)) = rest.split_once(" -> [") else {
            continue;
        };
        let want = want.strip_suffix(']').unwrap_or(want);
        let (mut wb, s1) = workbook(&host);
        wb.define_name("nm", "=Sheet1!$J$1:$K$2", None)
            .expect("a name");
        wb.define_name("nmone", "=Sheet1!$J$1", None)
            .expect("a name");
        let cell = at(s1, "Z50");
        n += 1;
        let entered = match form {
            "F" => wb.set_legacy_input(cell, formula),
            "F2" => wb.set_formula(cell, formula),
            _ => continue,
        };
        if let Err(e) = entered {
            bad.push(format!("{form} {formula}: error {e:?}, Excel [{want}]"));
            continue;
        }
        let got = wb.display_text(cell);
        if got != want {
            bad.push(format!("{form} {formula}: got [{got}], Excel [{want}]"));
        }
    }
    assert!(n > 60, "the golden has its lines ({n})");
    assert!(
        bad.is_empty(),
        "{} differ:
{}",
        bad.len(),
        bad.join(
            "
"
        )
    );
}
