use std::sync::Arc;

use xlcalc::{CellAddr, RtdSource, Value, Workbook};

struct Source;

impl RtdSource for Source {
    fn topic(&self, prog_id: &str, server: &str, topics: &[String]) -> Option<Value> {
        (prog_id == "Xlc.TestRtd")
            .then(|| Value::Text(format!("{server}|{}", topics.join("|")).into()))
    }
}

fn eval(wb: &mut Workbook, formula: &str) -> String {
    let at = CellAddr::new(wb.active_sheet(), 0, 0);
    wb.set_formula(at, formula).unwrap();
    wb.display_text(at)
}

#[test]
fn without_a_source_everything_is_na() {
    let mut wb = Workbook::new();
    assert_eq!(eval(&mut wb, "=RTD(\"Xlc.TestRtd\",,\"a\")"), "#N/A");
}

#[test]
fn source_values_topics_and_errors() {
    let mut wb = Workbook::new();
    wb.set_rtd_source(Some(Arc::new(Source)));
    assert_eq!(
        eval(&mut wb, "=RTD(\"Xlc.TestRtd\",,\"const\",\"x\")"),
        "|const|x"
    );
    assert_eq!(
        eval(&mut wb, "=RTD(\"Xlc.TestRtd\",\"S\",5,TRUE)"),
        "S|5|TRUE"
    );
    assert_eq!(eval(&mut wb, "=RTD(\"No.Such\",,\"a\")"), "#N/A");
    assert_eq!(eval(&mut wb, "=RTD(1,,\"a\")"), "#N/A");
    assert_eq!(eval(&mut wb, "=RTD(\"Xlc.TestRtd\",,1/0)"), "#DIV/0!");
}
