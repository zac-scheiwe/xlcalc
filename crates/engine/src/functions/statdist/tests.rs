use super::dd::DD;
use super::{hp, special};
use crate::{CellAddr, Value, Workbook};

fn eval(formula: &str) -> Value {
    let mut wb = Workbook::new();
    let s = wb.sheet_at(0).id;
    let a = CellAddr::new(s, 0, 0);
    wb.set_input(a, formula).unwrap();
    wb.value(a)
}

fn num(formula: &str) -> f64 {
    match eval(formula) {
        Value::Number(n) => n,
        other => panic!("{formula}: expected a number, got {other:?}"),
    }
}

#[test]
fn double_double_basics() {
    let x = DD::new(2.0).ln();
    assert_eq!(x.hi(), std::f64::consts::LN_2);
    assert_eq!(DD::new(1.0).exp().hi(), std::f64::consts::E);

    assert!((DD::new(1e-310).ln().hi() + 713.8).abs() < 0.1);
    assert!(DD::new(709.5).exp().hi().is_finite());
}

#[test]
fn log_gamma_and_gamma() {
    assert_eq!(special::lgamma(1.0), 0.0);
    assert_eq!(special::lgamma(2.0), 0.0);
    assert_eq!(special::gamma(5.0), 24.0);
    assert_eq!(special::gamma(0.5), 1.772453850905516);
    assert!(special::gamma(-1.0).is_nan());
    assert!((special::lgamma(0.999999) - 5.772164873855652e-7).abs() < 1e-21);
}

#[test]
fn incomplete_gamma_and_beta() {
    let (p, q) = hp::gamma_pq(1.0, 2.0);
    assert!((p - (1.0 - (-2.0f64).exp())).abs() < 1e-16);
    assert!((q - (-2.0f64).exp()).abs() < 1e-17);
    assert_eq!(hp::beta_inc(2.0, 3.0, 0.5, 0.5), 0.6875);
    assert_eq!(hp::beta_inc(1.0, 1.0, 0.25, 0.75), 0.25);
}

#[test]
fn worksheet_values() {
    assert_eq!(num("=NORM.S.DIST(0,TRUE)"), 0.5);
    assert_eq!(num("=ERF(0)"), 0.0);
    assert_eq!(num("=T.DIST(0,5,TRUE)"), 0.5);
    assert_eq!(num("=CORREL({1,2,3},{1,2,3})"), 1.0);
    assert_eq!(num("=PERCENTILE({1,2,3,4},0.5)"), 2.5);
    assert!((num("=CHISQ.INV(0.5,2)") - 2.0 * std::f64::consts::LN_2).abs() < 1e-15);
    assert!(matches!(eval("=NORM.S.INV(0)"), Value::Error(_)));
}

#[test]
fn extreme_arguments_terminate() {
    for f in [
        "=CHISQ.DIST(5E+9,1E+10,TRUE)",
        "=GAMMA.DIST(1E+6,1E+6,1,TRUE)",
        "=BETA.DIST(0.5,1E+6,1E+6,TRUE)",
        "=T.DIST(1,1E+10,TRUE)",
        "=F.DIST(1,1E+10,1E+10,TRUE)",
        "=BINOM.DIST(500000,1000000,0.5,TRUE)",
        "=BINOM.INV(1000000,0.5,0.5)",
        "=POISSON.DIST(1E+6,1E+6,TRUE)",
        "=T.INV(1E-300,1)",
        "=CHISQ.INV.RT(1E-300,3)",
        "=GAMMA.INV(0.999999999,1E+6,1)",
        "=BETA.INV(1E-300,0.01,100)",
    ] {
        let _ = eval(f);
    }
}
