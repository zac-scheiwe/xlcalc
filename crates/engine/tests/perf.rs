use std::time::Instant;

use xlcalc::{CalcMode, CellAddr, Value, Workbook};

fn setup(n: u32, formulas: bool) -> (Workbook, xlcalc::SheetId) {
    let mut wb = Workbook::new();
    let s = wb.sheet_at(0).id;
    wb.set_mode(CalcMode::Manual);
    for r in 0..n {
        wb.set_value(CellAddr::new(s, r, 0), Value::Number(r as f64 * 0.5));
        if formulas {
            wb.set_formula(CellAddr::new(s, r, 1), &format!("=A{}*2", r + 1))
                .unwrap();
        }
    }
    wb.set_mode(CalcMode::Automatic);
    (wb, s)
}

fn time(label: &str, n: u32, wb: &mut Workbook, cell: CellAddr, formula: &str) {
    wb.set_formula(cell, formula).unwrap();
    let reps = 20;
    let t = Instant::now();
    for _ in 0..reps {
        wb.set_formula(cell, formula).unwrap();
    }
    let per = t.elapsed() / reps;
    println!(
        "{label:<40} {per:>10.2?}  {:6.1} ns/cell",
        per.as_nanos() as f64 / n as f64
    );
}

#[test]
#[ignore]
fn hot_paths() {
    let n = 200_000;
    let (mut wb, s) = setup(n, true);
    let d = |r| CellAddr::new(s, r, 3);
    time("SUM over constants", n, &mut wb, d(0), "=SUM(A:A)");
    time("SUM over formula results", n, &mut wb, d(1), "=SUM(B:B)");
    time("COUNT over constants", n, &mut wb, d(2), "=COUNT(A:A)");
    time(
        "MATCH exact (last)",
        n,
        &mut wb,
        d(3),
        &format!("=MATCH({},A:A,0)", (n - 1) as f64 * 0.5),
    );
    time(
        "VLOOKUP exact (last)",
        n,
        &mut wb,
        d(4),
        &format!("=VLOOKUP({},A:B,2,FALSE)", (n - 1) as f64 * 0.5),
    );
    time(
        "SUMPRODUCT A*B",
        n,
        &mut wb,
        d(5),
        "=SUMPRODUCT(A1:A200000,B1:B200000)",
    );
    time("SUM(A*2) array", n, &mut wb, d(6), "=SUM(A1:A200000*2)");

    let rect = xlcalc::Rect::new(0, 0, n - 1, 0);
    let t = Instant::now();
    let mut acc = 0.0;
    for _ in 0..20 {
        acc += xlcalc::testing::raw_sum(&wb, s, rect);
    }
    let per = t.elapsed() / 20;
    println!(
        "{:<40} {per:>10.2?}  {:6.1} ns/cell  ({acc})",
        "raw iter_rect",
        per.as_nanos() as f64 / n as f64
    );
    let t = Instant::now();
    let mut acc = 0.0;
    for _ in 0..20 {
        acc += xlcalc::testing::visit_sum(&wb, CellAddr::new(s, 0, 5), s, rect);
    }
    let per = t.elapsed() / 20;
    println!(
        "{:<40} {per:>10.2?}  {:6.1} ns/cell  ({acc})",
        "visit_area",
        per.as_nanos() as f64 / n as f64
    );
}
