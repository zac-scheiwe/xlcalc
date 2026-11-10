use xlcalc::cfv::{CfKind, CfRule, Dxf};
use xlcalc::{parse_cell_ref, CellAddr, Rect, Value, Workbook};

fn setup() -> (Workbook, xlcalc::SheetId) {
    let mut wb = Workbook::new();
    let s = wb.sheet_at(0).id;
    for r in 0..6u32 {
        wb.set_value(CellAddr::new(s, r, 0), Value::Number(f64::from(r + 1)));
        wb.set_value(CellAddr::new(s, r, 1), Value::Number(f64::from(6 - r)));
    }
    (wb, s)
}

fn rule(wb: &mut Workbook, s: xlcalc::SheetId, kind: CfKind, f1: &str) {
    let anchor = parse_cell_ref(wb, "A1", s).unwrap();
    let f1 = wb.cfv_to_relative(f1, anchor).unwrap();
    wb.cfv_sheet_mut(s).add_rule(CfRule {
        id: 0,
        kind,
        applies: vec![Rect::new(0, 0, 5, 1)],
        prio: 0,
        seq: 0,
        stop: true,
        f1: Some(f1),
        f2: None,
        dxf: Dxf::default(),
    });
}

#[test]
fn expression_rule_holds_per_cell() {
    let (mut wb, s) = setup();
    rule(&mut wb, s, CfKind::Expression, "=$A1>$B1");
    let holds = |wb: &Workbook, r| wb.cf_holds(s, 0, r, 0);
    assert!(!holds(&wb, 0));
    assert!(!holds(&wb, 2));
    assert!(holds(&wb, 3));
    assert!(holds(&wb, 5));
}
