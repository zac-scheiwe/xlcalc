use crate::address::SheetId;
use crate::value::{Value, XlError};
use crate::{parse_cell_ref, CellAddr, Workbook};

enum C<'a> {
    N(f64),
    S(&'a str),
    B(bool),
    E(XlError),
    F(&'a str),
}
use C::*;

struct Book {
    wb: Workbook,
    t: SheetId,
}

impl Book {
    fn new(cells: &[(&str, C)]) -> Book {
        Book::with(cells, &[])
    }

    fn with(p: &[(&str, C)], q: &[(&str, C)]) -> Book {
        let mut wb = Workbook::empty();
        let ps = wb.add_sheet("P").unwrap();
        let qs = wb.add_sheet("Q").unwrap();
        let t = wb.add_sheet("T").unwrap();
        for (sheet, cells) in [(ps, p), (qs, q)] {
            for (a1, c) in cells {
                let a = parse_cell_ref(&wb, a1, sheet).unwrap();
                match c {
                    N(n) => {
                        wb.set_value(a, Value::Number(*n));
                    }
                    S(s) => {
                        wb.set_value(a, Value::from(*s));
                    }
                    B(b) => {
                        wb.set_value(a, Value::Bool(*b));
                    }
                    E(e) => {
                        wb.set_value(a, Value::Error(*e));
                    }
                    F(f) => {
                        wb.set_formula(a, f)
                            .unwrap_or_else(|e| panic!("{a1}: {f}: {e}"));
                    }
                }
            }
        }
        Book { wb, t }
    }

    fn eval(&mut self, formula: &str) -> Value {
        let a = CellAddr::new(self.t, 0, 0);
        self.wb
            .set_formula(a, formula)
            .unwrap_or_else(|e| panic!("{formula}: {e}"));
        let fid = self.wb.formula_at(a).unwrap();
        let v = self.wb.formula(fid).value.clone();
        self.wb.clear(a);
        v
    }

    fn num(&mut self, formula: &str) -> f64 {
        match self.eval(formula) {
            Value::Number(n) => n,
            other => panic!("{formula} = {other:?}, expected a number"),
        }
    }

    fn check(&mut self, cases: &[(&str, f64)]) {
        for (f, want) in cases {
            let got = self.num(f);
            assert_eq!(
                got.to_bits(),
                want.to_bits(),
                "{f}: expected {want}, got {got}"
            );
        }
    }

    fn check_err(&mut self, cases: &[(&str, XlError)]) {
        for (f, want) in cases {
            match self.eval(f) {
                Value::Error(e) if e == *want => {}
                other => panic!("{f}: expected {want}, got {other:?}"),
            }
        }
    }
}

fn mixed() -> Book {
    let a: Vec<(String, C)> = vec![
        ("A1".into(), N(5.0)),
        ("A2".into(), S("5")),
        ("A3".into(), N(5.0000000000001)),
        ("A4".into(), B(true)),
        ("A5".into(), S("TRUE")),
        ("A7".into(), F("=\"\"")),
        ("A8".into(), E(XlError::NA)),
        ("A9".into(), S("#N/A")),
        ("A10".into(), S("apple")),
        ("A11".into(), S("Apple pie")),
        ("A12".into(), N(43831.0)),
        ("A13".into(), S("1/1/2020")),
        ("A14".into(), N(0.0)),
        ("A15".into(), B(false)),
        ("A16".into(), S("a*b")),
        ("A17".into(), S("  5")),
        ("A18".into(), S("5%")),
        ("A19".into(), N(0.5)),
        ("A20".into(), S("=5")),
    ];
    let mut cells: Vec<(String, C)> = a;
    for r in 1..=20 {
        cells.push((format!("B{r}"), N((1u32 << (r - 1)) as f64)));
    }
    let refs: Vec<(&str, C)> = cells
        .iter()
        .map(|(k, c)| (k.as_str(), clone_c(c)))
        .collect();
    Book::new(&refs)
}

fn clone_c<'a>(c: &C<'a>) -> C<'a> {
    match c {
        N(n) => N(*n),
        S(s) => S(s),
        B(b) => B(*b),
        E(e) => E(*e),
        F(f) => F(f),
    }
}

#[test]
fn criteria_language_on_mixed_types() {
    let mut b = mixed();
    let cases: &[(&str, f64)] = &[
        ("5", 65539.0),
        ("\"5\"", 65539.0),
        ("\"=5\"", 65539.0),
        ("\">4\"", 2053.0),
        ("\"<5\"", 270336.0),
        ("\"<>5\"", 1048574.0),
        ("\"\"", 96.0),
        ("\"=\"", 32.0),
        ("\"<>\"", 1048543.0),
        ("TRUE", 8.0),
        ("\"TRUE\"", 8.0),
        ("\">FALSE\"", 8.0),
        ("\"<TRUE\"", 16384.0),
        ("#N/A", 128.0),
        ("\"#N/A\"", 128.0),
        ("\"<>#N/A\"", 1048447.0),
        ("\"APPLE*\"", 1536.0),
        ("\"*\"", 759634.0),
        ("\"?*\"", 759570.0),
        ("\"<>*\"", 288941.0),
        ("\">a\"", 34320.0),
        ("\"<a\"", 725314.0),
        ("43831", 6144.0),
        ("\"1/1/2020\"", 6144.0),
        ("0", 8192.0),
        ("P!A6", 8192.0),
        ("P!A7", 96.0),
        ("\"a~*b\"", 32768.0),
        ("\"5%\"", 131072.0),
        ("\"==5\"", 524288.0),
        ("\"= 5\"", 65539.0),
        ("\"12:00\"", 262144.0),
        ("\"=5.0000000000001\"", 4.0),
        ("\"<>1/1/2020\"", 1046527.0),
        ("\"<>0\"", 1040383.0),
        ("\" TRUE \"", 0.0),
        ("\"$5\"", 65539.0),
        ("\"*5\"", 589826.0),
        ("\"5*\"", 131074.0),
        ("\"?\"", 2.0),
        ("\">=0\"", 272389.0),
        ("\">=\"\"\"", 694034.0),
        ("\"> \"", 759570.0),
        ("\"< \"", 64.0),
        ("\"=  \"", 0.0),
        ("\"=TRUE*\"", 16.0),
        ("\"#N/?\"", 256.0),
        ("\"*/*\"", 4352.0),
        ("\">\"", 0.0),
        ("\"<=\"", 0.0),
    ];
    for (crit, want) in cases {
        let f = format!("=SUMIF(P!A1:A20,{crit},P!B1:B20)");
        let got = b.num(&f);
        assert_eq!(got, *want, "{f}");
    }
}

#[test]
fn numeric_criteria_use_fifteen_digit_equality() {
    let mut b = Book::new(&[
        ("A1", F("=0.1+0.2")),
        ("A2", F("=1+2^-48")),
        ("A3", F("=1+2^-46")),
        ("A4", F("=1-2^-48")),
        ("A5", F("=1-2^-46")),
        ("A6", F("=1+2^-52")),
        ("A7", F("=1+1E-14")),
        ("A8", F("=1+1E-15")),
        ("A9", F("=2^-1000")),
        ("A10", N(0.0)),
        ("A11", F("=1+5E-15")),
        ("A12", F("=1+3E-15")),
        ("A13", F("=123456789012345+0.4")),
        ("A14", F("=123456789012345+0.6")),
        ("A15", S("0.3")),
        ("A16", F("=-0.1-0.2")),
        ("B1", N(1.0)),
        ("B2", N(2.0)),
        ("B3", N(4.0)),
        ("B4", N(8.0)),
        ("B5", N(16.0)),
        ("B6", N(32.0)),
        ("B7", N(64.0)),
        ("B8", N(128.0)),
        ("B9", N(256.0)),
        ("B10", N(512.0)),
        ("B11", N(1024.0)),
        ("B12", N(2048.0)),
        ("B13", N(4096.0)),
        ("B14", N(8192.0)),
        ("B15", N(16384.0)),
        ("B16", N(32768.0)),
    ]);
    b.check(&[
        ("=SUMIF(P!A1:A16,0.3,P!B1:B16)", 16385.0),
        ("=SUMIF(P!A1:A16,1,P!B1:B16)", 2210.0),
        ("=SUMIF(P!A1:A16,\">1\",P!B1:B16)", 13380.0),
        ("=SUMIF(P!A1:A16,\"<1\",P!B1:B16)", 33561.0),
        ("=SUMIF(P!A1:A16,\"<>1\",P!B1:B16)", 63325.0),
        ("=SUMIF(P!A1:A16,0,P!B1:B16)", 512.0),
        ("=SUMIF(P!A1:A16,\">0\",P!B1:B16)", 15871.0),
        ("=SUMIF(P!A1:A16,123456789012345,P!B1:B16)", 4096.0),
        ("=SUMIF(P!A1:A16,\">123456789012345\",P!B1:B16)", 8192.0),
        ("=SUMIF(P!A1:A16,-0.3,P!B1:B16)", 32768.0),
        ("=SUMIF(P!A1:A16,\"<-0.3\",P!B1:B16)", 0.0),
        ("=COUNTIF(P!A1:A16,P!A7)", 3.0),
        ("=COUNTIF(P!A1:A16,\">\"&P!A9)", 12.0),
    ]);
}

#[test]
fn only_classic_error_literals_are_criteria() {
    let mut b = Book::new(&[
        ("A1", E(XlError::Spill)),
        ("A2", N(1.0)),
        ("A3", E(XlError::Calc)),
        ("A4", E(XlError::Null)),
        ("A5", E(XlError::Field)),
        ("B1", N(1.0)),
        ("B2", N(2.0)),
        ("B3", N(4.0)),
        ("B4", N(8.0)),
        ("B5", N(16.0)),
    ]);
    b.check(&[
        ("=SUMIF(P!A1:A5,\"#SPILL!\",P!B1:B5)", 0.0),
        ("=SUMIF(P!A1:A5,\"#CALC!\",P!B1:B5)", 0.0),
        ("=SUMIF(P!A1:A5,\"#NULL!\",P!B1:B5)", 8.0),
        ("=SUMIF(P!A1:A5,\"#FIELD!\",P!B1:B5)", 0.0),
        ("=SUMIF(P!A1:A5,\"<>#SPILL!\",P!B1:B5)", 31.0),
        ("=SUMIF(P!A1:A5,\">#N/A\",P!B1:B5)", 21.0),
        ("=SUMIF(P!A1:A5,\"<#N/A\",P!B1:B5)", 8.0),
        ("=SUMIF(P!A1:A5,P!A1,P!B1:B5)", 1.0),
        ("=SUMIF(P!A1:A5,P!A3,P!B1:B5)", 4.0),
        ("=COUNTIF(P!B1:B5,LAMBDA(x,x))", 0.0),
    ]);
}

#[test]
fn tilde_escapes_only_in_wildcard_patterns() {
    let texts = [
        "~x", "~~x", "x", "a~b", "ab", "a~", "~", "a", "*", "?", "~*", "a*",
    ];
    let mut cells: Vec<(String, C)> = Vec::new();
    for (i, t) in texts.iter().enumerate() {
        cells.push((format!("A{}", i + 1), S(t)));
        cells.push((format!("B{}", i + 1), N((1u32 << i) as f64)));
    }
    let refs: Vec<(&str, C)> = cells
        .iter()
        .map(|(k, c)| (k.as_str(), clone_c(c)))
        .collect();
    let mut b = Book::new(&refs);
    let cases: &[(&str, f64)] = &[
        ("~x", 1.0),
        ("~~x", 2.0),
        ("~x*", 4.0),
        ("~~x*", 1.0),
        ("~", 64.0),
        ("~~", 0.0),
        ("~~*", 1091.0),
        ("~*", 256.0),
        ("*~", 4095.0),
        ("a~b", 8.0),
        ("a~b*", 16.0),
        ("a~*", 2048.0),
        ("a*~", 2232.0),
        ("~?", 512.0),
        ("*~~*", 1131.0),
        ("~~~*", 1024.0),
        ("?~", 964.0),
        ("<>~~x", 4093.0),
        ("a*~~", 32.0),
    ];
    for (crit, want) in cases {
        let f = format!("=SUMIF(P!A1:A12,\"{crit}\",P!B1:B12)");
        assert_eq!(b.num(&f), *want, "{f}");
    }
}

#[test]
fn sums_are_plain_and_row_major() {
    let mut b = Book::new(&[
        ("C1", N(1e16)),
        ("D1", N(1.0)),
        ("C2", N(-1e16)),
        ("D2", N(1.0)),
        ("E1", S("k")),
        ("F1", S("k")),
        ("E2", S("k")),
        ("F2", S("k")),
        ("G1", N(0.1)),
        ("G2", N(0.2)),
        ("G3", N(-0.3)),
        ("H1", S("k")),
        ("H2", S("k")),
        ("H3", S("k")),
    ]);
    let tiny = 0.1 + 0.2 - 0.3;
    b.check(&[
        ("=SUMIF(P!E1:F2,\"k\",P!C1:D2)", 1.0),
        ("=SUMIF(P!E1:F2,\"k\",P!C1)", 1.0),
        ("=SUMIFS(P!C1:D2,P!E1:F2,\"k\")", 1.0),
        ("=SUMIF(P!H1:H3,\"k\",P!G1:G3)", tiny),
        ("=SUMIF(P!G1:G3,\"<>0\")", tiny),
        ("=SUMIFS(P!G1:G3,P!H1:H3,\"k\")", tiny),
        ("=AVERAGEIF(P!H1:H3,\"k\",P!G1:G3)", tiny / 3.0),
        ("=SUBTOTAL(9,P!G1:G3)", 0.0),
        ("=SUBTOTAL(1,P!G1:G3)", 0.0),
        ("=AGGREGATE(9,4,P!G1:G3)", 0.0),
    ]);
}

#[test]
fn subtotal_skips_nested_subtotals() {
    let mut b = Book::new(&[
        ("A1", N(1.0)),
        ("A2", F("=SUBTOTAL(9,P!B1)")),
        ("A3", F("=1000+SUBTOTAL(9,P!B1)")),
        ("A4", F("=AGGREGATE(9,0,P!B1)")),
        ("A5", F("=IF(FALSE,SUBTOTAL(9,P!B1),10000)")),
        ("A6", F("=SUM(P!B1)")),
        ("A7", F("=AGGREGATE(14,6,P!B1,1)")),
        ("A8", F("=SUBTOTAL(109,P!B1)")),
        ("A9", N(100000.0)),
        ("A10", F("=1/0")),
        ("A11", F("=AGGREGATE(14,6,{1,2},1)")),
        ("B1", N(10.0)),
        ("D1", F("=SEQUENCE(3)")),
        ("K1", F("=SUBTOTAL(9,P!B1)*{1;2}")),
        ("F1", N(1.0)),
        ("G1", F("=SUBTOTAL(9,P!F1)")),
        ("G2", F("=AGGREGATE(9,0,P!F1)")),
        ("G3", F("=1000+SUBTOTAL(9,P!F1)")),
        ("G4", N(5000.0)),
    ]);
    b.check(&[
        ("=SUBTOTAL(9,P!A1:A9)", 100011.0),
        ("=SUBTOTAL(109,P!A1:A9)", 100011.0),
        ("=SUBTOTAL(2,P!A1:A9)", 3.0),
        ("=SUBTOTAL(3,P!A1:A9)", 3.0),
        ("=SUM(P!A1:A9)", 111061.0),
        ("=AGGREGATE(9,0,P!A1:A9)", 100011.0),
        ("=AGGREGATE(9,1,P!A1:A9)", 100011.0),
        ("=AGGREGATE(9,2,P!A1:A10)", 100011.0),
        ("=AGGREGATE(9,3,P!A1:A10)", 100011.0),
        ("=AGGREGATE(9,4,P!A1:A9)", 111061.0),
        ("=AGGREGATE(9,5,P!A1:A9)", 111061.0),
        ("=AGGREGATE(9,6,P!A1:A10)", 111061.0),
        ("=AGGREGATE(9,7,P!A1:A10)", 111061.0),
        ("=AGGREGATE(9,,P!A1:A9)", 100011.0),
        ("=AGGREGATE(14,0,P!A1:A9,1)", 100000.0),
        ("=AGGREGATE(14,4,P!A1:A9,1)", 100000.0),
        ("=AGGREGATE(14,6,P!A1:A10,1)", 100000.0),
        ("=AGGREGATE(9,0,P!A11)", 0.0),
        ("=SUBTOTAL(9,P!A11)", 0.0),
        ("=SUBTOTAL(9,P!D1:D3)", 6.0),
        ("=SUBTOTAL(9,P!D1#)", 6.0),
        ("=SUBTOTAL(9,P!K1:K2)", 0.0),
        ("=SUM(P!K1:K2)", 30.0),
        ("=SUBTOTAL(9,P!A1,P!A2,P!A9)", 100001.0),
        ("=AGGREGATE(15,4,P!G1:G3,1)", 1.0),
        ("=AGGREGATE(14,0,P!G1:G4,1)", 5000.0),
        ("=AGGREGATE(15,0,P!G1:G4,1)", 5000.0),
        ("=AGGREGATE(15,0,(P!G1:G3,P!F1),1)", 1.0),
        ("=AGGREGATE(9,0,P!F1:G4)", 5001.0),
        ("=AGGREGATE(2,0,P!F1:G4)", 2.0),
        ("=AGGREGATE(3,4,P!F1:G4)", 5.0),
    ]);
    b.check_err(&[
        ("=SUBTOTAL(9,P!A1:A10)", XlError::Div0),
        ("=AGGREGATE(9,4,P!A1:A10)", XlError::Div0),
        ("=AGGREGATE(9,0,P!A1:A10)", XlError::Div0),
        ("=AGGREGATE(14,0,P!A1:A10,1)", XlError::Div0),
        ("=AGGREGATE(15,0,P!A1:A11,1)", XlError::Div0),
        ("=AGGREGATE(15,0,P!G1:G3,1)", XlError::Num),
    ]);
}

fn db_cells() -> Vec<(&'static str, C<'static>)> {
    vec![
        ("A1", S("Name")),
        ("B1", S("Qty")),
        ("C1", S("Date")),
        ("D1", S("Bit")),
        ("A2", S("apple")),
        ("B2", N(5.0)),
        ("C2", N(43831.0)),
        ("D2", N(1.0)),
        ("A3", S("apricot")),
        ("B3", N(10.0)),
        ("C3", N(43832.0)),
        ("D3", N(2.0)),
        ("A4", S("banana")),
        ("B4", N(15.0)),
        ("C4", N(43833.0)),
        ("D4", N(4.0)),
        ("A5", S("Apple pie")),
        ("B5", N(20.0)),
        ("D5", N(8.0)),
        ("B6", N(25.0)),
        ("C6", N(43835.0)),
        ("D6", N(16.0)),
        ("A7", S("app")),
        ("B7", S("30")),
        ("C7", N(43836.0)),
        ("D7", N(32.0)),
    ]
}

#[test]
fn computed_criteria_move_with_each_record() {
    let formulas: &[(&str, f64)] = &[
        ("=B2>12", 60.0),
        ("=$B2>12", 60.0),
        ("=B3>12", 30.0),
        ("=B$2>12", 0.0),
        ("=ROW()=2", 63.0),
        ("=COLUMN()>0", 63.0),
        ("=ROW(A2)=4", 4.0),
        ("=ROW(A1)>0", 62.0),
        ("=ROW(B1)<6", 30.0),
        ("=D3>0", 31.0),
        ("=ISBLANK(D3)", 0.0),
        ("=NOT(ISERROR(D3))", 31.0),
        ("=ERROR.TYPE(D3)=4", 32.0),
        ("=E2=1", 0.0),
        ("=$E3=1", 0.0),
        ("=E$2=1", 63.0),
        ("=ROWS(D3:D4)=2", 15.0),
        ("=COUNTA(A2:D2)=4", 39.0),
        ("=COUNTA(A2:E2)=5", 0.0),
        ("=COUNTA(A$2:D2)>0", 63.0),
        ("=SUM($B$2:B2)>20", 60.0),
        ("=SUM(B2:C2)>43840", 22.0),
        ("=B2:B3>7", 62.0),
        ("=ISTEXT(B2)", 32.0),
        ("=N(B2)>12", 28.0),
        ("=B2*1>12", 60.0),
        ("=B2>B3", 0.0),
        ("=A2=\"apple\"", 1.0),
        ("=LEFT(A2,2)=\"ap\"", 43.0),
        ("=AND(B2>5,B2<25)", 14.0),
        ("=OR(A2=\"app\",D2=1)", 33.0),
        ("=C2>43832", 52.0),
        ("=LET(x,B2,x>12)", 60.0),
        ("=SUMPRODUCT(--(A2:A3=\"apple\"))>0", 1.0),
        ("=IFERROR(D3,1)>0", 63.0),
        ("=OFFSET(D2,1,0)>0", 31.0),
        ("=INDIRECT(\"D3\")>0", 63.0),
        ("=$A$9+B2>0", 63.0),
        ("=Q!H1=1", 63.0),
        ("=B2", 31.0),
        ("=A2", 0.0),
        ("=ISBLANK(A2)", 16.0),
        ("=\"\"", 0.0),
        ("={TRUE,FALSE}", 63.0),
    ];
    let mut cells = db_cells();
    cells.push(("A9", N(100.0)));
    cells.push(("E2", N(1.0)));
    cells.push(("E3", N(1.0)));
    let cols: Vec<String> = (0..formulas.len())
        .map(|i| crate::address::col_to_letters(10 + 2 * i as u32))
        .collect();
    let keys: Vec<(String, String)> = cols
        .iter()
        .map(|c| (format!("{c}1"), format!("{c}2")))
        .collect();
    for (i, (h, v)) in keys.iter().enumerate() {
        cells.push((Box::leak(h.clone().into_boxed_str()), S("Foo")));
        cells.push((Box::leak(v.clone().into_boxed_str()), F(formulas[i].0)));
    }
    let q: Vec<(String, C)> = (1..=6).map(|r| (format!("H{r}"), N(r as f64))).collect();
    let q: Vec<(&str, C)> = q.iter().map(|(k, c)| (k.as_str(), clone_c(c))).collect();
    let mut b = Book::with(&cells, &q);
    for (i, (f, want)) in formulas.iter().enumerate() {
        let c = &cols[i];
        let got = b.num(&format!("=DSUM(P!A1:D7,\"Bit\",P!{c}1:{c}2)"));
        assert_eq!(got, *want, "computed criterion {f}");
    }
}

#[test]
fn computed_criteria_layouts() {
    let mut cells = db_cells();
    cells.extend([
        ("F1", S("Foo")),
        ("F2", B(true)),
        ("G1", S("Foo")),
        ("G2", B(false)),
        ("H1", S("Foo")),
        ("H2", N(0.0)),
        ("I1", S("Foo")),
        ("I2", S("TRUE")),
        ("J1", S("Foo")),
        ("J2", S("true")),
        ("K1", S("Foo")),
        ("K2", S(" TRUE")),
        ("L1", S("Foo")),
        ("L2", S("1")),
        ("M1", S("Foo")),
        ("M2", N(-1.0)),
        ("N1", S("Foo")),
        ("N2", E(XlError::NA)),
        ("O2", S("apple")),
        ("P2", N(7.0)),
        ("Q1", S("Name")),
        ("Q2", F("=A2=\"apple\"")),
        ("X10", S("Foo")),
        ("X11", F("=B2>12")),
        ("Y10", S("Foo")),
        ("Y11", F("=B2>22")),
        ("Y12", F("=B2<6")),
        ("Z10", S("Foo")),
        ("AA10", S("Name")),
        ("Z11", F("=B2>8")),
        ("AA11", S("ap")),
        ("AB10", S("Name")),
        ("AB11", S("ap")),
        ("AC11", S("zzz")),
    ]);
    let q = [
        ("A1", S("Foo")),
        ("A2", F("=P!B2>12")),
        ("C10", S("Foo")),
        ("C11", F("=P!B2>12")),
        ("D1", S("Foo")),
        ("D2", F("=ROW(P!B2)=2")),
    ];
    let mut b = Book::with(&cells, &q);
    b.check(&[
        ("=DSUM(P!A1:D7,\"Bit\",P!F1:F2)", 63.0),
        ("=DSUM(P!A1:D7,\"Bit\",P!G1:G2)", 0.0),
        ("=DSUM(P!A1:D7,\"Bit\",P!H1:H2)", 0.0),
        ("=DSUM(P!A1:D7,\"Bit\",P!I1:I2)", 63.0),
        ("=DSUM(P!A1:D7,\"Bit\",P!J1:J2)", 63.0),
        ("=DSUM(P!A1:D7,\"Bit\",P!K1:K2)", 0.0),
        ("=DSUM(P!A1:D7,\"Bit\",P!L1:L2)", 0.0),
        ("=DSUM(P!A1:D7,\"Bit\",P!M1:M2)", 63.0),
        ("=DSUM(P!A1:D7,\"Bit\",P!N1:N2)", 0.0),
        ("=DSUM(P!A1:D7,\"Bit\",P!O1:O2)", 0.0),
        ("=DSUM(P!A1:D7,\"Bit\",P!P1:P2)", 63.0),
        ("=DSUM(P!A1:D7,\"Bit\",P!Q1:Q2)", 0.0),
        ("=DSUM(P!A1:D7,\"Bit\",P!X10:X11)", 60.0),
        ("=DSUM(P!A1:D7,\"Bit\",P!Y10:Y12)", 49.0),
        ("=DSUM(P!A1:D7,\"Bit\",P!Z10:AA11)", 42.0),
        ("=DSUM(P!A1:D7,\"Bit\",P!AB10:AC11)", 0.0),
        ("=DSUM(P!A1:D7,\"Bit\",Q!A1:A2)", 60.0),
        ("=DSUM(P!A1:D7,\"Bit\",Q!C10:C11)", 60.0),
        ("=DSUM(P!A1:D7,\"Bit\",Q!D1:D2)", 1.0),
        ("=DSUM(P!A1:D5,\"Bit\",Q!D1:D2)", 1.0),
    ]);
}

#[test]
fn database_text_criteria_are_prefixes() {
    let mut cells = db_cells();
    cells.extend([
        ("A8", F("=\"\"")),
        ("B8", B(true)),
        ("C8", E(XlError::NA)),
        ("D8", N(64.0)),
        ("A9", S("TRUE")),
        ("B9", S("5")),
        ("C9", S("abc")),
        ("D9", N(128.0)),
        ("A10", B(true)),
        ("B10", N(5.0)),
        ("C10", N(43831.0)),
        ("D10", N(256.0)),
    ]);
    let name: &[(&str, f64)] = &[
        ("ap", 43.0),
        ("=ap", 0.0),
        ("<>ap", 511.0),
        ("<>apple", 510.0),
        (">=ap", 175.0),
        ("a?", 43.0),
        ("a?p", 41.0),
        ("TRUE", 256.0),
        ("=TRUE", 256.0),
        ("*", 239.0),
        ("?*", 175.0),
        ("~*", 0.0),
        ("<>", 495.0),
        ("=", 16.0),
        ("apple pie", 8.0),
        (" apple", 0.0),
        ("app*", 41.0),
        ("<>app*", 470.0),
        (">a", 175.0),
        ("<=apple", 97.0),
        ("A", 43.0),
        ("=a*", 43.0),
        ("b", 4.0),
    ];
    let qty: &[(&str, f64)] = &[
        ("5", 385.0),
        ("=5", 385.0),
        ("<>5", 254.0),
        ("TRUE", 64.0),
        ("30", 32.0),
        (">4", 287.0),
        ("<>", 511.0),
        (">=30", 0.0),
        ("<>30", 511.0),
    ];
    let mut extra: Vec<(String, String, String)> = Vec::new();

    let mut col = 5u32;
    for (field, list) in [("Name", name), ("Qty", qty)] {
        for (crit, _) in list {
            let c = crate::address::col_to_letters(col);
            extra.push((c.clone(), field.to_string(), crit.to_string()));
            col += 1;
        }
    }

    let ce = crate::address::col_to_letters(col);
    let cf = crate::address::col_to_letters(col + 1);
    let leaked: Vec<(&'static str, C<'static>)> = extra
        .iter()
        .flat_map(|(c, field, crit)| {
            let h: &'static str = Box::leak(format!("{c}1").into_boxed_str());
            let v: &'static str = Box::leak(format!("{c}2").into_boxed_str());
            let f: &'static str = Box::leak(field.clone().into_boxed_str());
            let k: &'static str = Box::leak(crit.clone().into_boxed_str());
            [(h, S(f)), (v, S(k))]
        })
        .collect();
    cells.extend(leaked);
    let (e1, e2, f1, f2) = (
        format!("{ce}1"),
        format!("{ce}2"),
        format!("{cf}1"),
        format!("{cf}2"),
    );
    cells.extend([
        (&*Box::leak(e1.into_boxed_str()), S("Name")),
        (&*Box::leak(e2.into_boxed_str()), F("=\"\"")),
        (&*Box::leak(f1.into_boxed_str()), S("Qty")),
        (&*Box::leak(f2.into_boxed_str()), F("=\"\"")),
    ]);
    let mut b = Book::new(&cells);
    for ((c, field, crit), want) in extra.iter().zip(name.iter().chain(qty.iter()).map(|x| x.1)) {
        let got = b.num(&format!("=DSUM(P!A1:D10,\"Bit\",P!{c}1:{c}2)"));
        assert_eq!(got, want, "{field} {crit}");
    }
    b.check(&[
        (&format!("=DSUM(P!A1:D10,\"Bit\",P!{ce}1:{ce}2)"), 239.0),
        (&format!("=DSUM(P!A1:D10,\"Bit\",P!{cf}1:{cf}2)"), 160.0),
        ("=DPRODUCT(P!A1:D10,\"Qty\",P!F1:F2)", 1000.0),
        ("=DAVERAGE(P!A1:D10,\"Qty\",P!F1:F2)", 35.0 / 3.0),
        ("=DMAX(P!A1:D10,\"Date\",P!F1:F2)", 43836.0),
        ("=DMIN(P!A1:D10,\"Date\",P!F1:F2)", 43831.0),
        ("=DSUM(P!A1:D10,\"Date\",P!F1:F2)", 131499.0),
        ("=DCOUNT(P!A1:D10,\"Date\",P!F1:F2)", 3.0),
        ("=DCOUNTA(P!A1:D10,\"Date\",P!F1:F2)", 3.0),
        ("=DCOUNTA(P!A1:D10,\"Name\",P!F1:F2)", 4.0),
        ("=DVAR(P!A1:D10,\"Bit\",P!F1:F2)", 210.25),
        ("=DSUM(P!A1:D10,\"Bit\",P!A9:A10)", 511.0),
        ("=DSUM(P!A1:D10,\"Bit\",P!B9:B10)", 511.0),
        ("=DSUM(P!A1:D10,\"Bit\",P!C7:C8)", 0.0),
        ("=DSUM(P!A1:D10,\"Bit\",P!C1:C2)", 257.0),
        ("=DSUM(P!A1:D10,\"Bit\",P!C1:C3)", 259.0),
        ("=DSUM(P!A1:D10,\"Bit\",P!B1:B2)", 385.0),
    ]);
    let sd = b.num("=STDEV(5,10,20)");
    b.check(&[("=DSTDEV(P!A1:D10,\"Qty\",P!F1:F2)", sd)]);
    b.check_err(&[
        ("=DGET(P!A1:D10,\"Name\",P!F1:F2)", XlError::Num),
        ("=DGET(P!A1:D10,\"Date\",P!G1:G2)", XlError::Value),
        ("=DMAX(P!A1:D10,\"Date\",P!A1:A10)", XlError::NA),
        ("=DSUM(P!A1:D10,\"Bit\",P!A1:A1)", XlError::Value),
    ]);
}

#[test]
fn database_headers_and_dget() {
    let mut cells = db_cells();
    cells.extend([
        ("F1", N(10.0)),
        ("G1", B(true)),
        ("E2", S("e1")),
        ("E3", S("e2")),
        ("E4", S("e3")),
        ("F2", N(1.0)),
        ("F3", N(2.0)),
        ("F4", N(3.0)),
        ("G2", N(7.0)),
        ("G3", N(8.0)),
        ("G4", N(9.0)),
        ("J2", S("e1")),
        ("K1", N(10.0)),
        ("K2", N(2.0)),
        ("L1", N(10.0)),
        ("L2", S("2")),
        ("M1", B(true)),
        ("M2", N(8.0)),
        ("N1", S("true")),
        ("N2", N(8.0)),
        ("O1", S("Bit ")),
        ("O2", N(1.0)),
        ("P1", S("Name")),
        ("P2", S("Apple p")),
        ("Q1", S("Qty")),
        ("Q2", N(25.0)),
        ("R1", S("Name")),
        ("R2", S("=apple")),
        ("R3", S("=apple")),
        ("S1", S("Name")),
        ("S2", S("=")),
    ]);
    let mut b = Book::new(&cells);
    b.check(&[
        ("=DSUM(P!A1:G7,\"Bit\",P!J1:J2)", 0.0),
        ("=DSUM(P!A1:G7,\"Bit\",P!K1:K2)", 2.0),
        ("=DSUM(P!A1:G7,\"Bit\",P!L1:L2)", 2.0),
        ("=DSUM(P!A1:G7,\"Bit\",P!M1:M2)", 2.0),
        ("=DSUM(P!A1:G7,\"Bit\",P!N1:N2)", 2.0),
        ("=DSUM(P!A1:G7,\"Bit\",P!O1:O2)", 63.0),
        ("=DSUM(P!A1:G7,\"10\",P!A1:A2)", 1.0),
        ("=DSUM(P!A1:G7,\"TRUE\",P!A1:A2)", 7.0),
        ("=DSUM(P!A1:G7,TRUE,P!A1:A2)", 0.0),
        ("=DSUM(P!A1:G7,\"\",P!A1:A2)", 0.0),
        ("=DSUM(P!A1:G7,5,P!A1:A2)", 0.0),
        ("=DSUM(P!A1:G7,7.9,P!A1:A2)", 7.0),
        ("=DSUM(P!A1:G7,P!G1,P!A1:A2)", 0.0),
        ("=DGET(P!A1:G7,\"Date\",P!A1:A2)", 43831.0),
        ("=DGET(P!A1:G7,\"Qty\",P!R1:R3)", 5.0),
        ("=DCOUNT(P!A1:G7,\"Date\",P!A1:A2)", 1.0),
        ("=DCOUNTA(P!A1:G7,\"Date\",P!A1:A2)", 1.0),
        ("=DPRODUCT(P!A1:G7,\"Date\",P!A1:A2)", 43831.0),
        ("=DSUM(P!A1:G7,\"Bit\",P!A1:D2)", 1.0),
    ]);
    b.check_err(&[
        ("=DSUM(P!A1:G7,10,P!A1:A2)", XlError::Value),
        ("=DSUM(P!A1:G7,8,P!A1:A2)", XlError::Value),
        ("=DSUM(P!A1:G7,-1,P!A1:A2)", XlError::Value),
        ("=DSUM(P!A1:G7,\"10.0\",P!A1:A2)", XlError::Value),
        ("=DSUM(P!A1:G7,\"1E1\",P!A1:A2)", XlError::Value),
        ("=DSUM(P!A1:G7,P!F1,P!A1:A2)", XlError::Value),
        ("=DSUM(P!A1:G7,P!H1,P!A1:A2)", XlError::Value),
        ("=DSUM(P!A1:G7,\"Bit\",P!A1:B1)", XlError::Value),
        ("=DGET(P!A1:G7,\"Date\",P!P1:P2)", XlError::Value),
        ("=DGET(P!A1:G7,\"Name\",P!Q1:Q2)", XlError::Value),
        ("=DGET(P!A1:G7,\"Name\",P!S1:S2)", XlError::Value),
        ("=DGET(P!A1:G7,\"Date\",P!J1:J2)", XlError::Value),
    ]);
}

#[test]
fn database_rows_stop_at_the_used_range() {
    let mut b = Book::new(&[
        ("A1", S("Key")),
        ("A2", S("a")),
        ("A3", S("b")),
        ("B1", S("Val")),
        ("B2", N(1.0)),
        ("B3", N(2.0)),
        ("C5", N(0.0)),
    ]);
    b.check(&[
        ("=DCOUNTA(P!A:B,,P!E1:E2)", 4.0),
        ("=DCOUNTA(P!A1:B100,,P!E1:E2)", 4.0),
        ("=DCOUNTA(P!X1:Y100,,P!E1:E2)", 4.0),
        ("=DCOUNTA(P!A6:B100,,P!E1:E2)", 0.0),
        ("=DSUM(P!A1:B5,\"Val\",P!A1:A100)", 3.0),
        ("=DSUM(P!A1:B5,\"Val\",P!A1:A3)", 3.0),
        ("=DSUM(P!A1:B5,\"Val\",P!A1:A2)", 1.0),
    ]);
}
