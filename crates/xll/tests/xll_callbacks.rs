#![cfg(windows)]

use std::sync::Arc;

use xlcalc::{CellAddr, SheetId, SheetKind, Value, Workbook};
use xlcalc_xll::XllHost;

fn testxll() -> Option<String> {
    let exe = std::env::current_exe().ok()?;
    let dll = exe.parent()?.parent()?.join("xlc_testxll.dll");
    dll.exists().then(|| dll.to_string_lossy().into_owned())
}

fn cell(sheet: SheetId, row: u32, col: u32) -> CellAddr {
    CellAddr { sheet, row, col }
}

fn book(host: &XllHost) -> (Workbook, SheetId) {
    let mut wb = Workbook::new();
    wb.set_function_host(Some(Arc::new(host.clone())));
    let s1 = wb.active_sheet();
    let s2 = wb.add_sheet("Sheet2").expect("Sheet2");
    for (name, kind) in [
        ("MacroS", SheetKind::Macro),
        ("IntlS", SheetKind::MacroIntl),
        ("DlgS", SheetKind::Dialog),
        ("ChartS", SheetKind::Chart),
    ] {
        wb.add_sheet_of_kind(name, kind).expect(name);
    }
    wb.set_value(cell(s2, 0, 1), Value::Number(9.0));
    wb.set_value(cell(s1, 0, 1), Value::Number(1.5));
    wb.set_value(cell(s1, 1, 1), Value::Text("7".into()));
    wb.set_value(cell(s1, 2, 1), Value::Bool(true));
    wb.set_value(cell(s1, 4, 1), Value::Text("abc".into()));
    wb.set_formula(cell(s1, 5, 1), "=1/0").unwrap();
    wb.set_value(cell(s1, 6, 1), Value::Number(3_000_000_000.0));
    wb.set_value(cell(s1, 7, 1), Value::Number(-2.5));
    wb.set_value(cell(s1, 8, 1), Value::Number(7.0));
    wb.set_value(cell(s1, 0, 2), Value::Number(2.0));
    wb.set_value(cell(s1, 1, 2), Value::Text("x".into()));
    (wb, s1)
}

#[test]
fn callbacks_follow_excel() {
    let Some(path) = testxll() else {
        eprintln!("skipped: xlc_testxll.dll is not built");
        return;
    };
    let host = XllHost::load(&path).expect("the test XLL loads");
    let (mut wb, s1) = book(&host);
    let (mut wb2, s1b) = book(&host);
    let golden = include_str!("../../../conformance/com/xll_probe18.golden.txt");
    let mut bad = Vec::new();
    let mut seen = 0;

    let mut second = 0;
    for line in golden.lines() {
        let Some((formula, want)) = line.split_once(" -> [") else {
            continue;
        };
        let want = want.strip_suffix(']').unwrap_or(want);

        if !formula.starts_with('=') {
            continue;
        }
        let (book, home) = if second > 0 {
            second -= 1;
            (&mut wb2, s1b)
        } else {
            (&mut wb, s1)
        };
        let at = cell(home, 0, 0);
        seen += 1;
        book.set_formula(at, formula)
            .unwrap_or_else(|e| panic!("{formula}: {e:?}"));
        let got = book.display_text(at);
        if got != want {
            bad.push(format!("{formula}: got [{got}], Excel [{want}]"));
        }
        if formula == "=XlcBin(\"def\",\"n1\",\"again\")" {
            second = 3;
        }
    }
    assert!(seen > 250, "the golden is read: {seen} lines");
    assert!(bad.is_empty(), "{} differ:\n{}", bad.len(), bad.join("\n"));
}

#[test]
fn single_value_of_an_array_follows_excel() {
    let Some(path) = testxll() else {
        eprintln!("skipped: xlc_testxll.dll is not built");
        return;
    };
    let host = XllHost::load(&path).expect("the test XLL loads");
    let (mut wb, s1) = book(&host);
    for n in 0..24 {
        wb.set_value(cell(s1, 20 + n / 4, n % 4), Value::Number(f64::from(n + 1)));
    }
    let golden = include_str!("../../../conformance/com/xll_probe19.golden.txt");
    let mut seen = 0;
    for line in golden.lines() {
        let Some((what, want)) = line.split_once(" -> [") else {
            continue;
        };
        let want = want.strip_suffix(']').unwrap_or(want);
        let words: Vec<&str> = what.split(' ').collect();
        let (addr, op) = match words[..] {
            ["array", _, op] => (cell(s1, 2, 2), op),
            [at, "legacy", op] | [at, op] => {
                let b = at.as_bytes();
                let row = at[1..].parse::<u32>().expect("a row") - 1;
                (cell(s1, row, u32::from(b[0] - b'A')), op)
            }
            _ => continue,
        };
        seen += 1;
        let formula = format!("=XlcCoerce({op},1)");
        let rect = xlcalc::Rect::new(2, 2, 5, 4);
        if words[0] == "array" {
            wb.set_array_formula(s1, rect, &formula).expect("CSE");
            let first = want.split("] [").next().expect("a value");
            for at in [cell(s1, 2, 2), cell(s1, 3, 3), cell(s1, 5, 4)] {
                assert_eq!(wb.display_text(at), first, "{what}");
            }
        } else if words.get(1) == Some(&"legacy") {
            wb.set_legacy_input(addr, &formula).expect("legacy entry");
            assert_eq!(wb.display_text(addr), want, "{what}");
        } else {
            wb.set_formula(addr, &formula).expect("entry");
            assert_eq!(wb.display_text(addr), want, "{what}");
        }
        wb.clear_range(s1, rect).expect("clear");
        wb.clear_range(s1, xlcalc::Rect::cell(addr.row, addr.col))
            .expect("clear");
    }
    assert!(seen > 50, "the golden is read: {seen} lines");
}
