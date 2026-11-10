use std::sync::{Arc, Mutex};

use xlcalc::{
    parse_cell_ref, Arg, Array, CallContext, CellAddr, FunctionHost, ParamKind, RuntimeError,
    Signature, Value, Workbook, XlError,
};

#[derive(Default)]
struct TestHost {
    log: Mutex<Vec<String>>,
}

impl TestHost {
    fn calls(&self, name: &str) -> usize {
        let log = self.log.lock().unwrap();
        log.iter()
            .filter(|l| l.split(' ').next() == Some(name))
            .count()
    }
}

fn sig(params: &[ParamKind], volatile: bool) -> Signature {
    Signature {
        params: params.to_vec(),
        rest: ParamKind::Value,
        volatile,
    }
}

fn describe(a: &Arg, cx: &CallContext) -> String {
    match a {
        Arg::Missing => "missing".into(),
        Arg::Value(Value::Number(n)) => format!("num:{n}"),
        Arg::Value(Value::Array(a)) => format!("array:{}x{}", a.rows, a.cols),
        Arg::Value(v) => format!("value:{v:?}"),
        Arg::Ref(areas) | Arg::XRef(areas) => {
            let mut sum = 0.0;
            for a in areas {
                for r in a.rect.r0..=a.rect.r1 {
                    for c in a.rect.c0..=a.rect.c1 {
                        if let Value::Number(n) = cx.view().value(CellAddr::new(a.sheet, r, c)) {
                            sum += n;
                        }
                    }
                }
            }
            format!("ref:{}:{sum}", areas.len())
        }
    }
}

impl FunctionHost for TestHost {
    fn signature(&self, name: &str) -> Option<Signature> {
        use ParamKind::{Reference as R, Value as V};
        Some(match name {
            "SUM" => sig(&[V], false),

            "SHADOW" | "DOUBLEIT" => sig(&[V], false),
            "ARGS" => sig(&[R, R], false),
            "READA1" => sig(&[], false),
            "VOLREAD" => sig(&[], true),
            "SETVOL" => sig(&[V], false),
            "FAIL" => sig(&[], false),
            "CVERR" => sig(&[], false),
            "TRIPLE" => sig(&[], false),
            _ => return None,
        })
    }

    fn call(
        &self,
        name: &str,
        args: &[Arg],
        cx: &mut CallContext<'_>,
    ) -> Result<Value, RuntimeError> {
        let line = std::iter::once(name.to_string())
            .chain(args.iter().map(|a| describe(a, cx)))
            .collect::<Vec<_>>()
            .join(" ");
        self.log.lock().unwrap().push(line.clone());
        let a1 = CellAddr::new(cx.view().caller().sheet, 0, 0);
        Ok(match name {
            "SUM" => Value::Number(-1.0),
            "SHADOW" => Value::Text("host".into()),
            "DOUBLEIT" => match args.first() {
                Some(Arg::Value(Value::Number(n))) => Value::Number(n * 2.0),
                _ => return Err(RuntimeError),
            },
            "ARGS" => Value::Text(line.into()),
            "READA1" | "VOLREAD" => cx.view().value(a1),
            "SETVOL" => {
                let on = matches!(args.first(), Some(Arg::Value(Value::Bool(true))));
                cx.set_volatile(on);
                Value::Number(self.calls("SETVOL") as f64)
            }
            "FAIL" => return Err(RuntimeError),
            "CVERR" => Value::Error(XlError::NA),
            "TRIPLE" => Array::new(
                1,
                3,
                vec![Value::Number(1.0), Value::Number(2.0), Value::Number(3.0)],
            )
            .into_value(),
            _ => unreachable!(),
        })
    }
}

struct T {
    wb: Workbook,
    host: Arc<TestHost>,
}

impl T {
    fn new() -> T {
        let mut wb = Workbook::new();
        let host = Arc::new(TestHost::default());
        wb.set_function_host(Some(host.clone()));
        T { wb, host }
    }

    fn at(&self, a1: &str) -> CellAddr {
        let s = self.wb.sheet_at(0).id;
        parse_cell_ref(&self.wb, a1, s).unwrap()
    }

    fn set(&mut self, a1: &str, input: &str) {
        let a = self.at(a1);
        self.wb
            .set_input(a, input)
            .unwrap_or_else(|e| panic!("{a1} := {input}: {e}"));
    }

    fn get(&self, a1: &str) -> Value {
        self.wb.value(self.at(a1))
    }

    fn num(&self, a1: &str) -> f64 {
        match self.get(a1) {
            Value::Number(n) => n,
            other => panic!("{a1} = {other:?}, expected a number"),
        }
    }

    fn text(&self, a1: &str) -> String {
        match self.get(a1) {
            Value::Text(t) => t.to_string(),
            other => panic!("{a1} = {other:?}, expected text"),
        }
    }
}

#[test]
fn builtin_then_defined_name_then_host() {
    let mut t = T::new();
    t.set("A1", "=SUM(2,3)");
    assert_eq!(t.num("A1"), 5.0);
    t.set("A2", "=SHADOW(1)");
    assert_eq!(t.text("A2"), "host");

    t.wb.define_name("SHADOW", "=LAMBDA(x,x+100)", None)
        .unwrap();
    assert_eq!(t.num("A2"), 101.0);
    t.wb.delete_name("SHADOW", None).unwrap();
    assert_eq!(t.text("A2"), "host");
    t.set("A3", "=NOSUCH(1)");
    assert!(matches!(t.get("A3"), Value::Error(XlError::NAME)));
    assert_eq!(t.host.calls("SUM"), 0);
}

#[test]
fn value_and_reference_parameters() {
    let mut t = T::new();
    t.set("A1", "3");
    t.set("A2", "4");
    t.set("B1", "=DOUBLEIT(A1)");
    assert_eq!(t.num("B1"), 6.0);
    t.set("B2", "=ARGS(A1:A2,7)");
    assert_eq!(t.text("B2"), "ARGS ref:1:7 num:7");
    t.set("B3", "=ARGS((A1,A2),)");
    assert_eq!(t.text("B3"), "ARGS ref:2:7 missing");

    t.set("B4", "=ARGS(1,{1,2})");
    assert_eq!(t.text("B4"), "ARGS num:1 array:1x2");

    t.set("A2", "10");
    assert_eq!(t.text("B2"), "ARGS ref:1:13 num:7");
}

#[test]
fn uncalculated_reference_argument_reads_empty_then_again() {
    let mut t = T::new();
    t.set("A3", "1");
    t.set("A2", "=A3*2");
    t.wb.set_mode(xlcalc::CalcMode::Manual);
    t.set("A3", "5");

    t.set("B1", "=ARGS(OFFSET(A1,1,0),0)");
    assert_eq!(t.text("B1"), "ARGS ref:1:2 num:0");
    let before = t.host.log.lock().unwrap().len();
    t.wb.recalc();
    assert_eq!(t.num("A2"), 10.0);
    assert_eq!(t.text("B1"), "ARGS ref:1:10 num:0");

    let log = t.host.log.lock().unwrap();
    assert_eq!(
        log[before..],
        [
            "ARGS ref:1:0 num:0".to_string(),
            "ARGS ref:1:10 num:0".to_string()
        ]
    );
}

#[test]
fn reads_through_the_view_go_stale_until_full_recalc() {
    let mut t = T::new();
    t.set("A1", "1");
    t.set("B1", "=READA1()");
    t.set("C1", "=VOLREAD()");
    assert_eq!(t.num("B1"), 1.0);
    t.set("A1", "2");
    assert_eq!(t.num("B1"), 1.0, "not a dependency");
    assert_eq!(t.num("C1"), 2.0, "volatile");
    t.wb.recalc();
    assert_eq!(t.num("B1"), 1.0);
    t.wb.full_recalc();
    assert_eq!(t.num("B1"), 2.0);
}

#[test]
fn volatility_is_set_per_call() {
    let mut t = T::new();
    t.set("A5", "TRUE");
    t.set("B1", "=SETVOL(A5)");
    let n = t.host.calls("SETVOL");
    t.wb.recalc();
    t.wb.recalc();
    assert_eq!(t.host.calls("SETVOL"), n + 2);

    t.set("A5", "FALSE");
    let n = t.host.calls("SETVOL");
    t.wb.recalc();
    t.wb.recalc();
    assert_eq!(t.host.calls("SETVOL"), n);
}

#[test]
fn runtime_errors_and_error_values() {
    let mut t = T::new();
    t.set("A1", "=FAIL()");
    assert!(matches!(t.get("A1"), Value::Error(XlError::VALUE)));
    t.set("A2", "=CVERR()");
    assert!(matches!(t.get("A2"), Value::Error(XlError::NA)));
    t.set("A3", "=DOUBLEIT(\"x\")");
    assert!(matches!(t.get("A3"), Value::Error(XlError::VALUE)));
}

#[test]
fn array_results_spill() {
    let mut t = T::new();
    t.set("A1", "=TRIPLE()");
    assert_eq!(t.num("A1"), 1.0);
    assert_eq!(t.num("B1"), 2.0);
    assert_eq!(t.num("C1"), 3.0);
    t.set("A2", "=SUM(A1#)");
    assert_eq!(t.num("A2"), 6.0);
}

#[test]
fn setting_the_host_later_recalculates_its_callers() {
    let mut wb = Workbook::new();
    let s = wb.sheet_at(0).id;
    let a1 = CellAddr::new(s, 0, 0);
    let a2 = CellAddr::new(s, 1, 0);
    wb.set_input(a1, "=DOUBLEIT(21)").unwrap();
    wb.set_input(a2, "=LET(x,DOUBLEIT(1),x)").unwrap();
    assert!(matches!(wb.value(a1), Value::Error(XlError::NAME)));
    wb.set_function_host(Some(Arc::new(TestHost::default())));
    wb.recalc();
    assert!(matches!(wb.value(a1), Value::Number(n) if n == 42.0));
    assert!(matches!(wb.value(a2), Value::Number(n) if n == 2.0));
    wb.set_function_host(None);
    wb.recalc();
    assert!(matches!(wb.value(a1), Value::Error(XlError::NAME)));
}

#[test]
fn host_calls_leave_the_parallel_phase() {
    let mut t = T::new();
    let s = t.wb.sheet_at(0).id;
    for r in 0..3000 {
        t.wb.set_input(CellAddr::new(s, r, 0), &(r + 1).to_string())
            .unwrap();
        t.wb.set_input(CellAddr::new(s, r, 1), "=A1").unwrap();
    }
    for r in 0..3000 {
        let f = format!("=DOUBLEIT(A{})+B{}", r + 1, r + 1);
        t.wb.set_input(CellAddr::new(s, r, 2), &f).unwrap();
    }
    let n = t.host.calls("DOUBLEIT");
    t.wb.full_recalc();

    assert_eq!(t.host.calls("DOUBLEIT"), n + 3000);
    for r in [0u32, 1500, 2999] {
        let want = 2.0 * (r + 1) as f64 + 1.0;
        assert!(
            matches!(t.wb.value(CellAddr::new(s, r, 2)), Value::Number(n) if n == want),
            "row {r}"
        );
    }
}

#[test]
fn volatility_is_recorded_on_iterative_sweeps() {
    let mut t = T::new();
    t.wb.set_iteration_enabled(true);
    t.set("A5", "TRUE");

    t.set("A1", "=SETVOL(A5)+B1*0");
    t.set("B1", "=A1");
    let a1 = t.at("A1");
    assert_eq!(xlcalc::testing::host_volatile_cells(&t.wb), vec![a1]);
    t.set("A5", "FALSE");
    assert!(xlcalc::testing::host_volatile_cells(&t.wb).is_empty());
}

#[test]
fn volatility_is_recorded_for_an_array_settled_element_by_element() {
    let mut t = T::new();
    let s = t.wb.sheet_at(0).id;
    t.set("A5", "TRUE");
    t.set("B1", "1");
    t.set("B2", "2");

    let rect = xlcalc::Rect::new(4, 8, 5, 8);
    t.wb.set_array_formula(s, rect, "=B1:B2+I6:I7+SETVOL(A5)*0")
        .unwrap();
    let i5 = t.at("I5");
    assert_eq!(xlcalc::testing::host_volatile_cells(&t.wb), vec![i5]);
    t.set("A5", "FALSE");
    assert!(xlcalc::testing::host_volatile_cells(&t.wb).is_empty());
}
