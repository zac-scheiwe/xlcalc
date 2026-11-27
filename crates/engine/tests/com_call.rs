use xlcalc::{Area, Arg, CellAddr, Evaluated, Rect, Value, Workbook};

fn book() -> (Workbook, CellAddr, CellAddr, Vec<Area>) {
    let mut wb = Workbook::new();
    let s = wb.active_sheet();
    for (r, c, t) in [(0, 0, "1"), (0, 1, "2"), (1, 0, "3"), (1, 1, "4")] {
        wb.set_input(CellAddr::new(s, r, c), t).unwrap();
    }
    let formula = CellAddr::new(s, 3, 3);
    wb.set_formula(formula, "=1+1").unwrap();
    let empty = CellAddr::new(s, 7, 7);
    let Evaluated::Ref(range) = wb.evaluate_text("A1:B2", empty) else {
        panic!("A1:B2 is a reference");
    };
    (wb, formula, empty, range)
}

fn numbers(v: Value) -> (u32, u32, Vec<f64>) {
    let Value::Array(a) = v else {
        panic!("not an array: {v:?}");
    };
    let data = a
        .data
        .iter()
        .map(|x| match x {
            Value::Number(n) => *n,
            other => panic!("not a number: {other:?}"),
        })
        .collect();
    (a.rows, a.cols, data)
}

#[test]
fn a_com_call_keeps_its_array_in_a_formula_cell() {
    let (wb, formula, empty, range) = book();
    for at in [formula, empty] {
        let call = |name| {
            wb.call_function(at, name, &[Arg::Ref(range.clone())])
                .unwrap()
        };
        assert_eq!(numbers(call("TRANSPOSE")), (2, 2, vec![1.0, 3.0, 2.0, 4.0]));
        assert_eq!(numbers(call("ROW")), (2, 1, vec![1.0, 2.0]));
    }
}

#[test]
fn evaluate_ignores_the_formula_in_the_host_cell() {
    let (mut wb, plain, empty, _) = book();
    let block = CellAddr::new(plain.sheet, 5, 0);
    wb.set_array_formula(plain.sheet, Rect::new(5, 0, 5, 1), "=A1:B1*1")
        .unwrap();
    let dynamic = CellAddr::new(plain.sheet, 6, 3);
    wb.set_formula(dynamic, "=SEQUENCE(1)").unwrap();
    for at in [plain, block, dynamic, empty] {
        let eval = |text| match wb.evaluate_text(text, at) {
            Evaluated::Value(v) => v,
            Evaluated::Ref(_) => panic!("{text} gave a reference"),
        };
        assert_eq!(
            numbers(eval("TRANSPOSE(A1:B2)")),
            (2, 2, vec![1.0, 3.0, 2.0, 4.0])
        );
        assert_eq!(numbers(eval("ROW(A1:A3)")), (3, 1, vec![1.0, 2.0, 3.0]));
        assert_eq!(
            numbers(eval("A1:B2*10")),
            (2, 2, vec![10.0, 20.0, 30.0, 40.0])
        );
        assert!(matches!(eval("SUM(ABS(A1:B2))"), Value::Number(n) if n == 10.0));
    }
}

#[test]
fn an_empty_cell_comes_back_empty_from_a_com_call() {
    let (wb, formula, _, mut range) = book();

    range[0].rect.r1 = 0;
    range[0].rect.c1 = 2;
    let out = wb
        .call_function(formula, "TRANSPOSE", &[Arg::Ref(range.clone())])
        .unwrap();
    let Value::Array(a) = out else {
        panic!("not an array: {out:?}");
    };
    assert!(matches!(a.data[2], Value::Blank));
    let lookup = [
        Arg::Value(Value::Number(1.0)),
        Arg::Ref(range),
        Arg::Value(Value::Number(3.0)),
        Arg::Value(Value::Bool(false)),
    ];
    assert!(matches!(
        wb.call_function(formula, "VLOOKUP", &lookup),
        Some(Value::Blank)
    ));
}

fn area(at: CellAddr, r0: u32, c0: u32, r1: u32, c1: u32) -> Area {
    Area {
        sheet: at.sheet,
        rect: Rect::new(r0, c0, r1, c1),
    }
}

#[test]
fn a_com_call_keeps_a_reference_result() {
    let (wb, _, empty, range) = book();
    let en = xlcalc::Locale::en_us();
    let call = |at, name, args: &[Arg]| wb.call_function_in(at, name, args, &en).unwrap();
    let areas = |at, name, args: &[Arg]| match call(at, name, args) {
        Evaluated::Ref(a) => a,
        Evaluated::Value(v) => panic!("{name} gave the value {v:?}"),
    };
    let whole = Arg::Ref(range.clone());
    let n = |x| Arg::Value(Value::Number(x));
    assert_eq!(
        areas(empty, "INDEX", &[whole.clone(), n(0.0), n(2.0)]),
        vec![area(empty, 0, 1, 1, 1)]
    );
    assert_eq!(
        areas(empty, "CHOOSE", &[n(1.0), whole.clone()]),
        vec![range[0]]
    );

    assert!(matches!(
        call(empty, "INDEX", &[whole.clone(), n(5.0), n(1.0)]),
        Evaluated::Value(Value::Error(_))
    ));
    assert!(matches!(
        call(empty, "IFERROR", &[whole.clone(), n(0.0)]),
        Evaluated::Value(Value::Array(_))
    ));

    let union = vec![area(empty, 0, 0, 1, 0), area(empty, 0, 1, 0, 1)];
    assert!(matches!(
        call(empty, "SUM", &[Arg::Ref(union.clone())]),
        Evaluated::Value(Value::Number(s)) if s == 6.0
    ));
    assert_eq!(
        areas(empty, "INDEX", &[Arg::Ref(union), n(1.0), n(1.0), n(2.0)]),
        vec![area(empty, 0, 1, 0, 1)]
    );

    let second_row = CellAddr::new(empty.sheet, 1, 3);
    let first_column = [Arg::Ref(vec![area(empty, 0, 0, 1, 0)])];
    assert_eq!(
        areas(second_row, "SINGLE", &first_column),
        vec![area(empty, 1, 0, 1, 0)]
    );
    assert!(matches!(
        call(empty, "SINGLE", &first_column),
        Evaluated::Value(Value::Error(_))
    ));

    let read = wb.call_function(empty, "INDEX", &[whole, n(0.0), n(2.0)]);
    assert_eq!(numbers(read.unwrap()), (2, 1, vec![2.0, 4.0]));
}
