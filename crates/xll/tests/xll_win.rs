#![cfg(windows)]

use std::sync::Arc;

use xlcalc::{CellAddr, SheetId, Value, Workbook};
use xlcalc_xll::XllHost;

fn testxll() -> Option<String> {
    let exe = std::env::current_exe().ok()?;
    let dll = exe.parent()?.parent()?.join("xlc_testxll.dll");
    dll.exists().then(|| dll.to_string_lossy().into_owned())
}

fn golden() -> Vec<String> {
    let path = concat!(
        env!("CARGO_MANIFEST_DIR"),
        "/../../conformance/com/xll_probe.golden.txt"
    );
    std::fs::read_to_string(path)
        .unwrap_or_default()
        .lines()
        .map(str::to_string)
        .collect()
}

fn parse(line: &str) -> Option<(String, String)> {
    let (formula, rest) = line.split_once(" -> [")?;
    let (text, _) = rest.split_once("] v2=[")?;
    formula
        .starts_with('=')
        .then(|| (formula.to_string(), text.to_string()))
}

fn a1(sheet: u32) -> CellAddr {
    CellAddr {
        sheet: SheetId(sheet),
        row: 0,
        col: 0,
    }
}

fn shown(wb: &mut Workbook, at: CellAddr, formula: &str) -> String {
    wb.set_formula(at, formula)
        .unwrap_or_else(|e| panic!("{formula}: {e:?}"));
    wb.display_text(at)
}

const OPEN: &[&str] = &["=XlcArr()"];

#[test]
fn functions_follow_excel() {
    let Some(path) = testxll() else {
        eprintln!("skipped: xlc_testxll.dll is not built");
        return;
    };
    let host = XllHost::load(&path).expect("the test XLL loads");
    let mut wb = Workbook::new();
    wb.set_function_host(Some(Arc::new(host.clone())));
    let first = wb.active_sheet();
    let at = a1(first.0);
    wb.set_value(
        CellAddr {
            sheet: first,
            row: 0,
            col: 1,
        },
        Value::Number(1.5),
    );
    wb.set_value(
        CellAddr {
            sheet: first,
            row: 1,
            col: 1,
        },
        Value::Text("txt".into()),
    );
    wb.set_value(
        CellAddr {
            sheet: first,
            row: 2,
            col: 1,
        },
        Value::Bool(true),
    );
    wb.set_formula(
        CellAddr {
            sheet: first,
            row: 0,
            col: 2,
        },
        "=1/0",
    )
    .unwrap();
    wb.set_value(
        CellAddr {
            sheet: first,
            row: 1,
            col: 2,
        },
        Value::Number(7.0),
    );
    let lines = golden();
    let split = lines
        .iter()
        .position(|l| l.starts_with("caller on Sheet2"))
        .expect("the golden has the sheet change");
    let mut failures = Vec::new();
    for line in &lines[..split] {
        let Some((formula, expected)) = parse(line) else {
            continue;
        };
        if OPEN.contains(&formula.as_str()) {
            continue;
        }
        let got = shown(&mut wb, at, &formula);
        if got != expected {
            failures.push(format!("{formula}: got [{got}], Excel [{expected}]"));
        }
    }

    let mut wb = Workbook::new();
    wb.set_function_host(Some(Arc::new(host.clone())));
    let second = wb.add_sheet("Sheet2").expect("a second sheet");
    for line in &lines[split + 1..] {
        let Some((formula, expected)) = parse(line) else {
            continue;
        };
        if OPEN.contains(&formula.as_str()) {
            continue;
        }
        let got = shown(&mut wb, a1(second.0), &formula);
        if got != expected {
            failures.push(format!("{formula}: got [{got}], Excel [{expected}]"));
        }
    }
    assert!(failures.is_empty(), "{}", failures.join("\n"));
    registration_events_and_volatility(&host);
}

fn registration_events_and_volatility(host: &XllHost) {
    assert_eq!(host.manager_info().as_deref(), Some("Xlc Test XLL"));
    let names = host.function_names();
    assert_eq!(names.len(), 57);
    assert!(names.iter().any(|n| n == "XLCTYPE"));
    let mut wb = Workbook::new();
    wb.set_function_host(Some(Arc::new(host.clone())));
    let at = a1(wb.active_sheet().0);
    let row = |r: u32| CellAddr { row: r, ..at };
    wb.set_formula(row(0), "=XlcVol()").unwrap();
    wb.set_formula(row(1), "=XlcType(1)").unwrap();
    wb.set_formula(row(2), "=XlcCalcEnds()").unwrap();
    let before = wb.display_text(row(0)).parse::<f64>().unwrap();
    wb.recalc();
    let after = wb.display_text(row(0)).parse::<f64>().unwrap();
    assert_eq!(after, before + 1.0, "a volatile function is called again");
    assert_eq!(wb.display_text(row(1)), "num:1");
    let ends = wb.display_text(row(2)).parse::<f64>().unwrap();
    host.fire_event(1);
    wb.set_formula(row(2), "=XlcCalcEnds()").unwrap();
    let ends_after = wb.display_text(row(2)).parse::<f64>().unwrap();
    assert!(
        ends_after > ends,
        "the calculation-ended event reaches the library"
    );
    host.close();
    assert!(host.function_names().is_empty());
}

const VOLATILE_IDS: &[u32] = &[63, 74, 221, 464];

const OPEN_IDS: &[u32] = &[429];

#[test]
fn function_numbers_follow_excel() {
    let Some(path) = testxll() else {
        eprintln!("skipped: xlc_testxll.dll is not built");
        return;
    };
    let host = XllHost::load(&path).expect("the test XLL loads");
    let mut wb = Workbook::new();
    wb.set_function_host(Some(Arc::new(host.clone())));
    let first = wb.active_sheet();
    let at = a1(first.0);
    for (row, col, v) in [
        (0, 1, Value::Number(1.5)),
        (1, 1, Value::Number(4.0)),
        (0, 2, Value::Number(10.0)),
        (1, 2, Value::Text("x".into())),
    ] {
        wb.set_value(
            CellAddr {
                sheet: first,
                row,
                col,
            },
            v,
        );
    }
    let path = concat!(
        env!("CARGO_MANIFEST_DIR"),
        "/../../conformance/com/xll_probe3.golden.txt"
    );
    let text = std::fs::read_to_string(path).unwrap_or_default();

    let mut lines: Vec<String> = Vec::new();
    for l in text.lines() {
        match lines.last_mut() {
            Some(prev) if !l.starts_with("=XlcCallN(") => {
                prev.push('\n');
                prev.push_str(l);
            }
            _ => lines.push(l.to_string()),
        }
    }
    let mut bad = Vec::new();
    let mut seen = 0;
    for line in lines.iter().filter(|l| l.starts_with("=XlcCallN(")) {
        let Some((formula, want)) = line.split_once(" -> [") else {
            continue;
        };
        let id: u32 = formula["=XlcCallN(".len()..]
            .split([',', ')'])
            .next()
            .and_then(|n| n.parse().ok())
            .unwrap_or(0);
        if VOLATILE_IDS.contains(&id) || OPEN_IDS.contains(&id) {
            continue;
        }
        let want = want.strip_suffix(']').unwrap_or(want);
        seen += 1;

        let got: String = shown(&mut wb, at, formula)
            .chars()
            .map(|c| if c.is_ascii() { c } else { '?' })
            .collect();
        if got != want {
            bad.push(format!("{formula}: got [{got}], Excel [{want}]"));
        }
    }
    assert!(seen > 4000, "the golden is read: {seen} lines");
    eprintln!("{} of {seen} differ", bad.len());
    for b in &bad {
        eprintln!("{b}");
    }
    assert!(
        bad.is_empty(),
        "{} function-number answers differ",
        bad.len()
    );
}

#[test]
fn returned_references_add_no_dependency() {
    let Some(path) = testxll() else {
        eprintln!("skipped: xlc_testxll.dll is not built");
        return;
    };
    let host = XllHost::load(&path).expect("the test XLL loads");
    let golden = include_str!("../../../conformance/com/xll_probe4.golden.txt");
    let mut bad = Vec::new();
    for line in golden.lines().skip(1) {
        let Some((formula, rest)) = line.split_once(' ').and_then(|(_, r)| r.split_once(" -> "))
        else {
            continue;
        };
        let field = |name: &str| {
            rest.split_once(&format!("{name}=["))
                .and_then(|(_, t)| t.split_once(']'))
                .map(|(v, _)| v.to_string())
                .unwrap_or_default()
        };
        let mut wb = Workbook::new();
        wb.set_function_host(Some(Arc::new(host.clone())));
        let s1 = wb.active_sheet();
        let other = wb.add_sheet("Other").unwrap();
        let cell = |sheet: SheetId, row: u32, col: u32| CellAddr { sheet, row, col };
        wb.set_value(cell(s1, 0, 1), Value::Number(5.0));
        wb.set_value(cell(s1, 1, 1), Value::Number(6.0));
        wb.set_value(cell(other, 0, 1), Value::Number(50.0));
        let at = cell(s1, 0, 0);
        wb.set_legacy_input(at, formula).unwrap();
        let before = wb.display_text(at);
        let (target, new) = if formula.contains("\"Other\"") {
            (cell(other, 0, 1), 70.0)
        } else if formula.contains("(1,1)") {
            (cell(s1, 1, 1), 9.0)
        } else {
            (cell(s1, 0, 1), 7.0)
        };
        wb.set_value(target, Value::Number(new));
        let edited = wb.display_text(at);
        wb.full_recalc();
        let full = wb.display_text(at);
        for (got, want, what) in [
            (before, field("before"), "before"),
            (edited, field("afterEdit"), "after the edit"),
            (full, field("afterFull"), "after a full calculation"),
        ] {
            if got != want {
                bad.push(format!("{formula} {what}: got [{got}], Excel [{want}]"));
            }
        }
    }
    assert!(bad.is_empty(), "{}", bad.join("\n"));
}

#[test]
fn other_threads_follow_excel() {
    let Some(path) = testxll() else {
        eprintln!("skipped: xlc_testxll.dll is not built");
        return;
    };
    let host = XllHost::load(&path).expect("the test XLL loads");
    let golden = include_str!("../../../conformance/com/xll_probe8.golden.txt");
    let mut bad = Vec::new();
    let mut wb = Workbook::new();
    wb.set_function_host(Some(Arc::new(host)));
    let at = a1(wb.active_sheet().0);
    let mut check = |wb: &mut Workbook, formula: &str, want: &str| {
        let got = shown(wb, at, formula);
        if got != want {
            bad.push(format!("{formula}: got [{got}], Excel [{want}]"));
        }
    };
    let mut threads = 0;
    for line in golden.lines() {
        if let Some((formula, rest)) = line.split_once(" -> [") {
            threads += 1;
            check(&mut wb, formula, rest.trim_end_matches(']'));
        }
    }
    assert_eq!(threads, 7);
    for delay in [0, 100, 600] {
        check(&mut wb, &format!("=XlcAsyncLater({delay},21)"), "42");
    }
    let c1 = CellAddr {
        row: 0,
        col: 2,
        ..at
    };
    let c2 = CellAddr {
        row: 1,
        col: 2,
        ..at
    };
    wb.set_formula(c1, "=XlcAsyncLater(300,5)").unwrap();
    wb.set_formula(c2, "=C1+1").unwrap();
    assert_eq!(wb.display_text(c2), "11");
    assert!(bad.is_empty(), "{}", bad.join("\n"));
}

#[test]
fn macro_numbers_with_operands_follow_excel() {
    let Some(path) = testxll() else {
        eprintln!("skipped: xlc_testxll.dll is not built");
        return;
    };
    let host = XllHost::load(&path).expect("the test XLL loads");
    let mut wb = Workbook::new();
    wb.set_function_host(Some(Arc::new(host.clone())));
    let first = wb.active_sheet();
    let at = a1(first.0);
    for (row, col, v) in [
        (0, 1, Value::Number(1.5)),
        (1, 1, Value::Number(4.0)),
        (0, 2, Value::Number(10.0)),
        (1, 2, Value::Text("x".into())),
    ] {
        wb.set_value(
            CellAddr {
                sheet: first,
                row,
                col,
            },
            v,
        );
    }
    let golden = include_str!("../../../conformance/com/xll_probe10.golden.txt");
    let mut bad = Vec::new();
    let mut seen = 0;
    for line in golden.lines().filter(|l| l.starts_with("=XlcCallNM(")) {
        let Some((formula, want)) = line.split_once(" -> [") else {
            continue;
        };

        if formula.starts_with("=XlcCallNM(187,4)") || formula.starts_with("=XlcCallNM(258,") {
            continue;
        }
        let want = want.strip_suffix(']').unwrap_or(want);
        seen += 1;
        let got = shown(&mut wb, at, formula);
        if got != want {
            bad.push(format!("{formula}: got [{got}], Excel [{want}]"));
        }
    }
    assert!(seen > 400, "the golden is read: {seen} lines");
    eprintln!("{} of {seen} differ", bad.len());
    for b in &bad {
        eprintln!("{b}");
    }
    assert!(bad.is_empty(), "{} macro answers differ", bad.len());
}

#[test]
fn macro_answers_use_the_workbook_name() {
    let Some(path) = testxll() else {
        eprintln!("skipped: xlc_testxll.dll is not built");
        return;
    };
    let host = XllHost::load(&path).expect("the test XLL loads");
    let mut wb = Workbook::new();
    wb.set_function_host(Some(Arc::new(host.clone())));
    let first = wb.active_sheet();
    let at = a1(first.0);
    let golden = include_str!("../../../conformance/com/xll_probe12.golden.txt");
    let mut bad = Vec::new();
    let mut seen = 0;
    for line in golden.lines().filter(|l| l.contains(" =XlcCallNM(")) {
        let Some((head, want)) = line.split_once(" -> [") else {
            continue;
        };
        let (label, formula) = head.split_once(' ').unwrap();
        match label {
            "new" => wb.set_book_name("Book2"),
            "saved" => wb.set_book_name("pbook.xlsx"),
            "xlsm" => wb.set_book_name("other book.xlsm"),
            "renamed" => {
                wb.rename_sheet(first, "My Sheet").unwrap();
            }
            "second" => {
                wb.rename_sheet(first, "Sheet1").unwrap();
                wb.set_book_name("Book3");
                wb.set_session_books(
                    vec!["other book.xlsm".into(), "Book3".into()],
                    vec!["Book3".into(), "other book.xlsm".into()],
                );
            }
            _ => unreachable!("{label}"),
        }
        let want = want.strip_suffix(']').unwrap_or(want);
        seen += 1;
        let got = shown(&mut wb, at, formula);
        if got != want {
            bad.push(format!("{label} {formula}: got [{got}], Excel [{want}]"));
        }
    }
    assert!(seen > 30, "the golden is read: {seen} lines");
    for b in &bad {
        eprintln!("{b}");
    }
    assert!(bad.is_empty(), "{} macro answers differ", bad.len());
}

#[test]
fn macro_functions_list_the_open_books() {
    let Some(path) = testxll() else {
        eprintln!("skipped: xlc_testxll.dll is not built");
        return;
    };
    let host = XllHost::load(&path).expect("the test XLL loads");
    let mut wb = Workbook::new();
    wb.set_function_host(Some(Arc::new(host.clone())));
    let at = a1(wb.active_sheet().0);
    let golden = include_str!("../../../conformance/com/xll_probe13.golden.txt");
    let mut bad = Vec::new();
    let mut seen = 0;
    for line in golden.lines().filter(|l| l.contains(" =XlcCallNM(")) {
        let Some((head, want)) = line.split_once(" -> [") else {
            continue;
        };
        let (label, formula) = head.split_once(' ').unwrap();
        let (docs, windows): (&[&str], &[&str]) = match label {
            "one" | "closed" => (&["Book2"], &["Book2"]),
            "two-first" | "two-second" => (&["Book2", "Book3"], &["Book3", "Book2"]),
            "two-saved" => (&["Book2", "second one.xlsx"], &["second one.xlsx", "Book2"]),
            _ => continue,
        };
        let own = if label == "two-first" || label == "one" || label == "closed" {
            "Book2"
        } else if label == "two-saved" {
            "second one.xlsx"
        } else {
            "Book3"
        };
        wb.set_book_name(own);
        let strings = |v: &[&str]| v.iter().map(|s| s.to_string()).collect::<Vec<_>>();
        wb.set_session_books(strings(docs), strings(windows));

        let want = want.strip_suffix(']').unwrap_or(want).trim_end_matches('|');
        seen += 1;
        let got = shown(&mut wb, at, formula);
        if got != want {
            bad.push(format!("{label} {formula}: got [{got}], Excel [{want}]"));
        }
    }
    assert!(seen > 30, "the golden is read: {seen} lines");
    for b in &bad {
        eprintln!("{b}");
    }
    assert!(bad.is_empty(), "{} macro answers differ", bad.len());
}
