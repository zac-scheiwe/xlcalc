use xlcalc::{parse_cell_ref, CellAddr, Value, Workbook, XlError};

struct T {
    wb: Workbook,
}

impl T {
    fn new() -> T {
        T {
            wb: Workbook::new(),
        }
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
}

#[test]
fn chain_updates() {
    let mut t = T::new();
    t.set("A1", "1");
    t.set("A2", "=A1+1");
    t.set("A3", "=A2*10");
    t.set("B1", "=SUM(A1:A3)");
    assert_eq!(t.num("B1"), 1.0 + 2.0 + 20.0);
    t.set("A1", "5");
    assert_eq!(t.num("A3"), 60.0);
    assert_eq!(t.num("B1"), 5.0 + 6.0 + 60.0);
}

#[test]
fn long_chain_does_not_overflow() {
    let mut t = T::new();
    t.set("A1", "1");
    for r in 2..=20_000 {
        t.set(&format!("A{r}"), &format!("=A{}+1", r - 1));
    }
    t.set("A1", "10");
    assert_eq!(t.num("A20000"), 20_009.0);
}

#[test]
fn cycle_basic() {
    let mut t = T::new();
    t.set("C1", "5");
    t.set("A1", "=B1+1");
    t.set("B1", "=A1+C1");
    assert_eq!(t.num("A1"), 1.0);
    assert_eq!(t.num("B1"), 0.0);
    assert_eq!(t.wb.circular_ref(), Some(t.at("B1")));
    t.set("C1", "7");
    assert_eq!(t.num("A1"), 1.0);
    assert_eq!(t.num("B1"), 0.0);
}

#[test]
fn cycle_through_block_completes_in_one_pass() {
    let mut t = T::new();
    for (a1, input) in [
        ("G1", "0"),
        ("H1", "=G1*10-5"),
        ("A1", "=H1+0"),
        ("A2", "5"),
    ] {
        t.set(a1, input);
    }
    let s = t.wb.sheet_at(0).id;
    let (sheet, rect) = xlcalc::parse_range_ref(&t.wb, "B1:B2", s).unwrap();
    t.wb.set_array_formula(sheet, rect, "=IF(A1:A2>2,1,0)")
        .unwrap();
    assert_eq!((t.num("B1"), t.num("B2")), (0.0, 1.0));
    t.set("G1", "=B2");
    assert_eq!(t.num("G1"), 1.0);
    assert_eq!(t.num("H1"), 5.0);
    assert_eq!(t.num("A1"), 5.0);
    assert_eq!(t.num("B1"), 1.0);
}

#[test]
fn cycle_prior_value() {
    let mut t = T::new();
    t.set("A1", "10");
    t.set("B1", "=A1*2");
    assert_eq!(t.num("B1"), 20.0);
    t.set("A1", "=B1+1");
    assert_eq!(t.num("A1"), 0.0);
    assert_eq!(t.num("B1"), 20.0);
    t.set("D1", "=A1+B1");
    assert_eq!(t.num("A1"), 0.0);
    assert_eq!(t.num("B1"), 20.0);
    assert_eq!(t.num("D1"), 0.0);
}

#[test]
fn runtime_cycle_detection() {
    let mut t = T::new();
    t.set("A1", "=ROW(A1)");
    t.set("A2", "=IF(FALSE,A2,1)");
    t.set("A3", "=ROWS(A1:A5)");
    t.set("A5", "=ISREF(A5)");
    assert_eq!(t.num("A1"), 1.0);
    assert_eq!(t.num("A2"), 1.0);
    assert_eq!(t.num("A3"), 5.0);
    assert!(matches!(t.get("A5"), Value::Bool(true)));
    assert_eq!(t.wb.circular_ref(), None);
    t.set("A6", "=SUM(A1:A3)+A6*0");
    assert_eq!(t.num("A6"), 0.0);
    assert_eq!(t.wb.circular_ref(), Some(t.at("A6")));
}

#[test]
fn iterative_calculation() {
    let mut t = T::new();
    t.wb.set_iteration(true, Some(100), Some(0.001));
    t.set("A1", "=B1/2+1");
    t.set("B1", "=A1");
    assert_eq!(t.num("A1"), 1.9990234375);
    assert_eq!(t.num("B1"), 1.9990234375);
    t.set("C1", "=C1+1");
    assert_eq!(t.num("C1"), 100.0);
    assert_eq!(t.num("A1"), 2.0);
    assert_eq!(t.num("B1"), 2.0);
    t.wb.recalc();
    assert_eq!(t.num("C1"), 200.0);
}

#[test]
fn spill_dynamics() {
    let mut t = T::new();
    t.set("A1", "3");
    t.set("B1", "=SEQUENCE(A1)");
    t.set("C1", "=B3");
    t.set("C2", "=B1#");
    t.set("D1", "=SUM(B1#)");
    t.set("E1", "=ISBLANK(B2)");
    assert_eq!(t.num("B2"), 2.0);
    assert_eq!(t.num("C1"), 3.0);
    assert_eq!(t.num("C4"), 3.0);
    assert_eq!(t.num("D1"), 6.0);
    assert!(matches!(t.get("E1"), Value::Bool(false)));

    t.set("B3", "x");
    assert!(matches!(t.get("B1"), Value::Error(XlError::SPILL)));
    assert!(matches!(t.get("C1"), Value::Text(ref s) if &**s == "x"));
    assert!(matches!(t.get("D1"), Value::Error(XlError::SPILL)));
    assert!(matches!(t.get("C2"), Value::Error(XlError::SPILL)));
    assert!(matches!(t.get("E1"), Value::Bool(true)));
    assert!(t.get("B2").is_blank());

    let b3 = t.at("B3");
    t.wb.clear(b3);
    t.set("A1", "2");
    assert_eq!(t.num("B2"), 2.0);
    assert!(t.get("B3").is_blank());
    assert_eq!(t.num("C1"), 0.0);
    assert_eq!(t.num("D1"), 3.0);
    assert!(matches!(t.get("E1"), Value::Bool(false)));
    assert_eq!(t.num("C3"), 2.0);
    assert!(t.get("C4").is_blank());
}

#[test]
fn blocked_by_formula() {
    let mut t = T::new();
    t.set("F1", "=SEQUENCE(2,2)");
    t.set("G1", "=SEQUENCE(2)");
    assert!(matches!(t.get("F1"), Value::Error(XlError::SPILL)));
    assert_eq!(t.num("G2"), 2.0);
}

#[test]
fn names_and_lambdas() {
    let mut t = T::new();
    t.set("A1", "4");
    t.wb.define_name("rate", "=Sheet1!$A$1", None).unwrap();
    t.set("B1", "=rate*2");
    assert_eq!(t.num("B1"), 8.0);
    t.set("A1", "5");
    assert_eq!(t.num("B1"), 10.0);
    t.wb.define_name("SQ", "=LAMBDA(x,x*x)", None).unwrap();
    t.set("C1", "=SQ(3)");
    assert_eq!(t.num("C1"), 9.0);
    t.set("D1", "=LET(a,2,b,a+1,a*b)");
    assert_eq!(t.num("D1"), 6.0);
    t.set("E1", "=LET(f,LAMBDA(n,n+1),f(f(1)))");
    assert_eq!(t.num("E1"), 3.0);
    t.set("F1", "=MAP(SEQUENCE(3),LAMBDA(v,v*10))");
    assert_eq!(t.num("F3"), 30.0);
    t.set("G1", "=REDUCE(0,SEQUENCE(4),LAMBDA(a,v,a+v))");
    assert_eq!(t.num("G1"), 10.0);
    t.set("H1", "=undefined_name+1");
    assert!(matches!(t.get("H1"), Value::Error(XlError::NAME)));
    t.wb.define_name("undefined_name", "=41", None).unwrap();
    assert_eq!(t.num("H1"), 42.0);
}

#[test]
fn dynamic_references() {
    let mut t = T::new();
    t.set("A1", "1");
    t.set("A2", "2");
    t.set("A3", "=A1+A2");
    t.set("B1", "=SUM(INDIRECT(\"A1:A3\"))");
    t.set("B2", "=SUM(OFFSET(A1,0,0,3,1))");
    t.set("B3", "=SUM(A1:INDEX(A:A,3))");
    assert_eq!(t.num("B1"), 6.0);
    assert_eq!(t.num("B2"), 6.0);
    assert_eq!(t.num("B3"), 6.0);
    t.set("A1", "10");
    assert_eq!(t.num("B1"), 24.0);
    assert_eq!(t.num("B2"), 24.0);
    assert_eq!(t.num("B3"), 24.0);
}

#[test]
fn parallel_matches_sequential() {
    let build = |threads: usize| {
        let mut t = T::new();
        t.wb.set_threads(threads);
        t.wb.set_mode(xlcalc::CalcMode::Manual);
        for r in 1..=3000 {
            t.set(&format!("A{r}"), &format!("{}", r % 17));
            t.set(
                &format!("B{r}"),
                &format!("=A{r}*2+INDIRECT(\"A\"&MAX(1,ROW()-1))"),
            );
            t.set(
                &format!("C{r}"),
                &format!("=SUM(B$1:B{})+OFFSET(A1,{},0)", (r % 50) + 1, r % 7),
            );
        }
        t.set("E1", "=SEQUENCE(100)");
        t.set("F1", "=SUM(E1#)+C3000");
        t.set("F2", "=E50*2");
        t.set("G1", "=G2+1");
        t.set("G2", "=G1+A1");
        t.wb.set_mode(xlcalc::CalcMode::Automatic);
        t.wb.full_recalc();
        t.set("A1", "100");
        t.set("E1", "=SEQUENCE(60)");
        t
    };
    let par = build(0);
    let seq = build(1);
    for r in 1..=3000 {
        for c in ["B", "C"] {
            let a = format!("{c}{r}");
            assert!(
                par.get(&a).same_as(&seq.get(&a)),
                "{a}: {:?} vs {:?}",
                par.get(&a),
                seq.get(&a)
            );
        }
    }
    for a in ["F1", "F2", "G1", "G2", "E60"] {
        assert!(
            par.get(a).same_as(&seq.get(a)),
            "{a}: {:?} vs {:?}",
            par.get(a),
            seq.get(a)
        );
    }
    assert_eq!(par.wb.circular_ref(), seq.wb.circular_ref());
}

#[test]
fn sumif_resized_sum_range_dependency() {
    let mut t = T::new();
    for (a, v) in [
        ("A1", "x"),
        ("A2", "x"),
        ("A3", "y"),
        ("B1", "1"),
        ("B2", "2"),
        ("B3", "4"),
    ] {
        t.set(a, v);
    }
    t.set("C1", "=SUMIF(A1:A3,\"x\",B1)");
    assert_eq!(t.num("C1"), 3.0);
    t.set("B2", "20");
    assert_eq!(t.num("C1"), 21.0);
}

#[test]
fn manual_mode() {
    let mut t = T::new();
    t.wb.set_mode(xlcalc::CalcMode::Manual);
    t.set("A1", "1");
    t.set("B1", "=A1*2");
    assert_eq!(t.num("B1"), 2.0, "a newly entered formula is calculated");
    t.set("A1", "5");
    assert_eq!(t.num("B1"), 2.0, "dependents wait for recalc");
    t.wb.recalc();
    assert_eq!(t.num("B1"), 10.0);
}

#[test]
fn amordegrc_huge_period_terminates() {
    let mut t = T::new();
    t.set(
        "A1",
        "=AMORDEGRC(2400,DATE(2008,8,19),DATE(2008,12,31),300,1E+300,0.15,1)",
    );
    assert_eq!(t.num("A1"), 0.0);
}

#[test]
fn amorlinc_huge_life_terminates() {
    let start = std::time::Instant::now();
    let mut t = T::new();
    t.set("A1", "=AMORLINC(1000,44196,44561,100,9E+8,1E-9,1)");
    t.set("A3", "=AMORLINC(1E+300,44196,44561,0,1E+300,1E-300,1)");
    t.set("A4", "=AMORLINC(1000,44196,44561,100,1E+300,1E-12,1)");
    assert_eq!(t.num("A1"), 1.0000000000000002e-6);
    assert_eq!(t.num("A3"), 0.0);
    assert_eq!(t.num("A4"), 0.0);
    assert!(start.elapsed().as_secs() < 10, "AMORLINC must not loop");
}

#[test]
fn gap_audit_al_extremes() {
    let mut t = T::new();

    t.set(
        "A1",
        "=ACCRINT(DATE(2019,1,1),60,DATE(2019,4,1),0.05,1000,2,0,TRUE)",
    );
    assert_eq!(t.num("A1"), 12.135225885225886);

    t.set("B1", "=CHIINV(0.05,1E+300)/1E+300");
    assert_eq!(t.num("B1"), 1.0002658235505755);
    t.set("B2", "=CHISQ.INV.RT(0.05,1E+11)/1E+11");
    assert_eq!(t.num("B2"), 1.0002731807118597);
    t.set("B3", "=CHIINV(0.05,1E+10)");
    assert!(matches!(t.get("B3"), Value::Error(XlError::NUM)));
    t.set("B4", "=CHIINV(0.05,6E+307)");
    assert!(matches!(t.get("B4"), Value::Error(XlError::DIV0)));

    t.set(
        "C1",
        "=FORECAST.ETS(2147483657,{1,2,3,4,5},{6,7,8,9,10},1,1,1)",
    );
    t.set(
        "C2",
        "=FORECAST.ETS(2147483658,{1,2,3,4,5},{6,7,8,9,10},1,1,1)",
    );
    t.set("C3", "=FORECAST.ETS(1E+300,{1,2,3,4,5},{6,7,8,9,10},1,1,1)");
    assert!(matches!(t.get("C2"), Value::Error(XlError::NUM)));
    assert!(matches!(t.get("C3"), Value::Error(XlError::NUM)));
    assert!(!matches!(t.get("C1"), Value::Error(_)));
}

#[test]
fn gap_audit_al_edges() {
    let mut t = T::new();
    t.set("A1", "=DB(1000,100,0.5,1,6)");
    assert!((t.num("A1") - 249.975).abs() < 1e-9);
    t.set("A2", "=CUBEKPIMEMBER(\"a\",\"b\",1)");
    assert!(matches!(t.get("A2"), Value::Error(XlError::NA)));
    t.set("A3", "=CUBEKPIMEMBER(1,\"b\",\"c\")");
    assert!(matches!(t.get("A3"), Value::Error(XlError::VALUE)));
    t.set("A4", "=CONVERT(1,REPT(\"a\",256),\"ft\")");
    assert!(matches!(t.get("A4"), Value::Error(XlError::VALUE)));
    t.set(
        "A5",
        "=DATEDIF(DATE(2020,1,1),DATE(2021,3,5),REPT(\"d\",256))",
    );
    assert!(matches!(t.get("A5"), Value::Error(XlError::VALUE)));
    t.set("A6", "=HYPERLINK(REPT(\"a\",256),\"n\")");
    assert!(matches!(t.get("A6"), Value::Error(XlError::VALUE)));
    t.set(
        "A7",
        "=ACCRINT(DATE(2019,1,1),DATE(2019,7,1),DATE(2019,4,1),0.05,1000,2,0,\"TRUE\")",
    );
    assert!(matches!(t.get("A7"), Value::Error(XlError::VALUE)));
    t.set(
        "A8",
        "=ACCRINT(60,DATE(2019,7,1),DATE(2019,4,1),0.05,1000,2,0,TRUE)",
    );
    assert!((t.num("A8") - 5954.444444444444).abs() < 1e-9);
}

#[test]
fn gap_audit_al3_edges() {
    let mut t = T::new();
    for (cell, f, e) in [
        ("A1", "=IRR(,0.1)", XlError::VALUE),
        ("A2", "=TREND({1;2;3},{1;2;3},{1,#N/A})", XlError::VALUE),
        ("A3", "=GROWTH({1;2;3},{1;2;3},{1,#N/A})", XlError::VALUE),
        (
            "A4",
            "=FORECAST.ETS(\"x\",{1;2;3;4;5},{1;2;3;4;5},#N/A)",
            XlError::VALUE,
        ),
        (
            "A5",
            "=FORECAST.ETS.STAT({1;2;3;4;5},{1;2;3;4;5},)",
            XlError::NUM,
        ),
    ] {
        t.set(cell, f);
        assert!(matches!(t.get(cell), Value::Error(x) if x == e), "{f}");
    }
}

#[test]
fn seeded_random_is_deterministic_across_threads() {
    let run = |threads: usize| {
        let mut t = T::new();
        t.wb.set_threads(threads);
        t.wb.set_mode(xlcalc::CalcMode::Manual);
        t.wb.seed_random(12345);
        for r in 1..=3000 {
            t.set(&format!("A{r}"), &format!("{}", r % 13));
            t.set(&format!("B{r}"), &format!("=A{r}*2+1"));
            t.set(&format!("C{r}"), &format!("=RAND()+B{r}"));
            t.set(&format!("D{r}"), &format!("=RANDBETWEEN(1,1000000)+A{r}"));
        }
        t.set("F1", "=RANDARRAY(20,3)");
        t.set("J1", "=SUM(F1#)+C3000");
        t.wb.set_mode(xlcalc::CalcMode::Automatic);
        t.wb.full_recalc();
        t.wb.full_recalc();
        t
    };
    let reference = run(1);
    for threads in [0, 0, 0, 1, 0, 4] {
        let other = run(threads);
        for r in 1..=3000 {
            for c in ["C", "D"] {
                let a = format!("{c}{r}");
                assert!(
                    reference.get(&a).same_as(&other.get(&a)),
                    "{a} threads={threads}: {:?} vs {:?}",
                    reference.get(&a),
                    other.get(&a)
                );
            }
        }
        for a in ["F1", "H20", "J1"] {
            assert!(
                reference.get(a).same_as(&other.get(a)),
                "{a} threads={threads}"
            );
        }
    }
}
