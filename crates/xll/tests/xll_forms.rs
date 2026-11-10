#![cfg(windows)]

use std::sync::Arc;

use xlcalc::{CellAddr, Rect, SheetId, Value, Workbook};
use xlcalc_xll::XllHost;

fn testxll() -> Option<String> {
    let exe = std::env::current_exe().ok()?;
    let dll = exe.parent()?.parent()?.join("xlc_testxll.dll");
    dll.exists().then(|| dll.to_string_lossy().into_owned())
}

fn cell(a: &str) -> (u32, u32) {
    let letters: String = a.chars().take_while(char::is_ascii_alphabetic).collect();
    let row: u32 = a[letters.len()..].parse().expect("row");
    let col = letters.bytes().fold(0u32, |n, b| {
        n * 26 + u32::from(b.to_ascii_uppercase() - b'A' + 1)
    });
    (row - 1, col - 1)
}

fn seed(wb: &mut Workbook, sheet: SheetId) {
    for (r, c, v) in [
        (0, 1, Value::Number(1.5)),
        (1, 1, Value::Number(4.0)),
        (0, 2, Value::Number(10.0)),
        (0, 3, Value::Number(20.0)),
        (2, 1, Value::Text("x".into())),
    ] {
        wb.set_value(
            CellAddr {
                sheet,
                row: r,
                col: c,
            },
            v,
        );
    }
}

#[test]
fn formula_forms_follow_excel() {
    let Some(path) = testxll() else {
        eprintln!("skipped: xlc_testxll.dll is not built");
        return;
    };
    let host = XllHost::load(&path).expect("the test XLL loads");
    let golden = include_str!("../../../conformance/com/xll_probe2.golden.txt");
    let mut bad = Vec::new();
    for line in golden.lines() {
        let mut it = line.splitn(3, ' ');
        let (Some(mode), Some(at), Some(rest)) = (it.next(), it.next(), it.next()) else {
            continue;
        };
        let Some((formula, want)) = rest.split_once(" -> [") else {
            continue;
        };
        let want = want.strip_suffix(']').unwrap_or(want);
        if !matches!(mode, "F" | "F2" | "FA") || want.starts_with("ERR") {
            continue;
        }
        let mut wb = Workbook::new();
        wb.set_function_host(Some(Arc::new(host.clone())));
        let sheet = wb.active_sheet();
        seed(&mut wb, sheet);
        let (a, b) = at.split_once(':').unwrap_or((at, at));
        let ((r0, c0), (r1, c1)) = (cell(a), cell(b));
        let anchor = CellAddr {
            sheet,
            row: r0,
            col: c0,
        };
        let res = match mode {
            "F" => wb.set_legacy_input(anchor, formula),
            "F2" => wb.set_formula(anchor, formula),
            _ => wb.set_array_formula(sheet, Rect::new(r0, c0, r1, c1), formula),
        };
        if let Err(e) = res {
            bad.push(format!("{line}\n    error {e:?}"));
            continue;
        }
        let mut shown = Vec::new();
        for row in 0..40 {
            for col in 0..30 {
                let x = CellAddr { sheet, row, col };
                let inside = mode == "FA" && (r0..=r1).contains(&row) && (c0..=c1).contains(&col);
                let spilled = mode == "F2" && (x == anchor || wb.spill_anchor(x) == Some(anchor));
                if inside || spilled || (mode == "F" && x == anchor) {
                    shown.push(wb.display_text(x));
                }
            }
        }
        let got = shown.join(",");
        if got != want {
            bad.push(format!("{line}\n    got [{got}]"));
        }
    }
    assert!(bad.is_empty(), "{} differ:\n{}", bad.len(), bad.join("\n"));
}
