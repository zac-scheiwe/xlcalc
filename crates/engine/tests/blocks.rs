use xlcalc::{parse_cell_ref, parse_range_ref, CellAddr, Rect, SheetId, Value, Workbook};

fn wb() -> Workbook {
    Workbook::new()
}

fn at(wb: &Workbook, a1: &str) -> CellAddr {
    parse_cell_ref(wb, a1, wb.sheet_at(0).id).unwrap()
}

fn rng(wb: &Workbook, a1: &str) -> (SheetId, Rect) {
    parse_range_ref(wb, a1, wb.sheet_at(0).id).unwrap()
}

fn set(wb: &mut Workbook, a1: &str, input: &str) {
    let a = at(wb, a1);
    wb.set_input(a, input).unwrap();
}

fn val(wb: &Workbook, a1: &str) -> Value {
    wb.value(at(wb, a1))
}

#[test]
fn array_block_fits_result() {
    let mut w = wb();
    for (c, v) in [("A1", "1"), ("A2", "2"), ("A3", "3")] {
        set(&mut w, c, v);
    }
    let (s, r) = rng(&w, "C1:D4");
    w.set_array_formula(s, r, "=A1:A3*2").unwrap();
    assert!(matches!(val(&w, "C2"), Value::Number(n) if n == 4.0));

    assert!(matches!(val(&w, "D3"), Value::Number(n) if n == 6.0));
    assert!(matches!(val(&w, "C4"), Value::Error(_)));
    assert_eq!(w.formula_text(at(&w, "D2")).as_deref(), Some("=A1:A3*2"));

    let _ = w.set_input(at(&w, "C2"), "5");
    assert!(matches!(val(&w, "C2"), Value::Number(n) if n == 4.0));
    let (s, part) = rng(&w, "C1:C2");
    assert!(w.clear_range(s, part).is_err());
    let (s, all) = rng(&w, "B1:D4");
    w.clear_range(s, all).unwrap();
    assert!(matches!(val(&w, "C2"), Value::Blank));
}

#[test]
fn circular_block_shows_zero() {
    let mut w = wb();
    let (s, r) = rng(&w, "A8:C10");
    w.set_array_formula(s, r, "=MAX(A5:A7)+A10").unwrap();
    assert!(matches!(val(&w, "B9"), Value::Number(n) if n == 0.0));
}

#[test]
fn merge_keeps_first_value() {
    let mut w = wb();
    set(&mut w, "B1", "7");
    set(&mut w, "A2", "8");
    set(&mut w, "D1", "=B1+1");
    let (s, r) = rng(&w, "A1:B2");
    w.merge_cells(s, r, false).unwrap();

    assert!(matches!(val(&w, "A1"), Value::Number(n) if n == 7.0));
    assert!(matches!(val(&w, "A2"), Value::Blank));
    assert_eq!(w.formula_text(at(&w, "D1")).as_deref(), Some("=A1+1"));
    assert_eq!(w.merge_at(at(&w, "B2")), Some(r));

    let (s, m) = rng(&w, "D5:E6");
    w.merge_cells(s, m, false).unwrap();
    set(&mut w, "C4", "=SEQUENCE(3,3)");
    assert!(matches!(val(&w, "C4"), Value::Error(_)));
    w.unmerge_cells(s, m);
    assert!(matches!(val(&w, "E6"), Value::Number(n) if n == 9.0));
}

#[test]
fn legacy_entry_inserts_at() {
    let mut w = wb();
    let a = at(&w, "B1");
    w.set_legacy_input(a, "=SUM(IF(A1:A3>1,A1:A3))").unwrap();
    assert_eq!(
        w.formula_text(a).as_deref(),
        Some("=SUM(IF(@A1:A3>1,A1:A3))")
    );
    assert_eq!(
        w.legacy_formula_text(a).as_deref(),
        Some("=SUM(IF(A1:A3>1,A1:A3))")
    );
    let b = at(&w, "B2");
    w.set_legacy_input(b, "=SUMPRODUCT(A1:A3*2)+INDEX(A1:A3,0)")
        .unwrap();
    assert_eq!(
        w.formula_text(b).as_deref(),
        Some("=SUMPRODUCT(A1:A3*2)+@INDEX(A1:A3,0)")
    );
}
