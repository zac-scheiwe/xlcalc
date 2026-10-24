use super::*;

fn t(x: f64, code: &str) -> String {
    let loc = Locale::en_nz();
    match NumberFormat::parse(code) {
        Ok(f) => match f.format_number(x, &loc, DateSystem::D1900) {
            Ok(r) => r.text,
            Err(e) => e.to_string(),
        },
        Err(e) => e.to_string(),
    }
}

fn ts(s: &str, code: &str) -> String {
    let loc = Locale::en_nz();
    match NumberFormat::parse(code) {
        Ok(f) => f
            .format_text(s, &loc)
            .map(|r| r.text)
            .unwrap_or_else(|e| e.to_string()),
        Err(e) => e.to_string(),
    }
}

#[test]
fn numbers() {
    assert_eq!(t(1234.5678, "0.00"), "1234.57");
    assert_eq!(t(1.005, "0.00"), "1.01");
    assert_eq!(t(-0.001, "0"), "0");
    assert_eq!(t(-0.001, "0;(0)"), "(0)");
    assert_eq!(t(1e20, "0"), "100000000000000000000");
    assert_eq!(t(1234567.0, "#,##0,"), "1,235");
    assert_eq!(t(1234567.0, "0.0,,"), "1.2");
    assert_eq!(t(0.5, "0%%"), "5000%%");
    assert_eq!(t(1.5, "??.??"), " 1.5 ");
    assert_eq!(t(0.0, "?.??"), " .  ");
    assert_eq!(t(5551234567.0, "(000) 000-0000"), "(555) 123-4567");
    assert_eq!(t(12345678.0, "#,##0 x 0"), "12,345,67 x 8");
    assert_eq!(t(5.0, "?,???"), "    5");
    assert_eq!(t(12.5, "x.00"), "x12.50");
    assert_eq!(t(1.0, "0.0.0"), "1.0.0");
}

#[test]
fn scientific() {
    assert_eq!(t(12345.0, "0.00E+00"), "1.23E+04");
    assert_eq!(t(12345.0, "##0.0E+0"), "12.3E+3");
    assert_eq!(t(12345.0, "000.0E+00"), "012.3E+03");
    assert_eq!(t(0.00012345, "0.00E-00"), "1.23E-04");
    assert_eq!(t(12345.0, "0.00E-00"), "1.23E04");
    assert_eq!(t(9.999, "0.00E+00"), "1.00E+01");
    assert_eq!(t(12345.0, "0.00E+?0"), "1.23E+ 4");
}

#[test]
fn fractions() {
    assert_eq!(t(1.5, "# ?/?"), "1 1/2");
    assert_eq!(t(0.333, "# ?/?"), " 1/3");
    assert_eq!(t(0.0, "# ?/?"), "0    ");
    assert_eq!(t(1.375, "?/8"), "11/8");
    assert_eq!(t(1.4, "# ??/16"), "1  6/16");
    assert_eq!(t(std::f64::consts::PI, "# ???/???"), "3  16/113");
    assert_eq!(t(0.91, "# ??/??"), " 81/89");
    assert_eq!(t(0.123, "# ???/???"), "  23/187");
    assert_eq!(t(5.0, "# #/#"), "5");
    assert_eq!(t(-0.5, "# ?/?"), "- 1/2");
    assert_eq!(t(-0.04, "?/?"), "0/1");
    assert_eq!(t(1.5, "# ?/0?"), "1 1/02");
    assert_eq!(t(1.5, "# ?/?0"), "1 1/20");
}

#[test]
fn sections_and_conditions() {
    assert_eq!(t(-5.0, "[<0]0;0"), "5");
    assert_eq!(t(-2.0, "[>3]\"big\";\"small\""), "-small");
    assert_eq!(t(-15.0, "[>-10]0;0"), "15");
    assert_eq!(t(-5.0, "[>-10]0;0"), "-5");
    assert_eq!(t(5.0, "[>10]0;[<-20]0"), "#VALUE!");
    assert_eq!(t(-5.0, "\"x\"0"), "-x5");
    assert_eq!(t(-5.0, ""), "-");
    assert_eq!(ts("abc", "0;(0);\"zero\";\"<\"@\">\""), "<abc>");
    assert_eq!(ts("x", "0;0;0;"), "");
    assert_eq!(ts("x", ";;"), "x");
    assert_eq!(t(1.0, "@;0"), "#VALUE!");
}

#[test]
fn general() {
    assert_eq!(t(1234567890123.0, "General"), "1.23457E+12");
    assert_eq!(t(-1.5, "General;General"), "1.5");
    assert_eq!(t(1.5, "General%"), "1.5%");
    assert_eq!(t(1.0 / 3.0, "@"), "0.333333333");
    assert_eq!(t(1.5, "0 General"), "#VALUE!");
}

#[test]
fn dates() {
    assert_eq!(
        t(45000.75, "dd/mm/yyyy hh:mm:ss AM/PM"),
        "15/03/2023 06:00:00 pm"
    );
    assert_eq!(t(45000.75, "ddd mmmmm"), "Wed M");
    assert_eq!(t(60.0, "dd/mm/yyyy"), "29/02/1900");
    assert_eq!(t(0.0, "dd/mm/yyyy dddd"), "00/01/1900 Saturday");
    assert_eq!(t(1.75, "[h]:mm:ss"), "42:00:00");
    assert_eq!(t(0.123456, "hh:mm:ss.000"), "02:57:46.598");
    assert_eq!(t(0.99999999, "hh:mm:ss"), "00:00:00");
    assert_eq!(t(45000.6789, "h d m"), "16 15 17");
    assert_eq!(t(45000.6789, "m d s"), "3 15 37");
    assert_eq!(t(45000.5, "h:mm mm"), "12:00 03");
    assert_eq!(t(-1.0, "dd/mm/yyyy"), "#VALUE!");
    assert_eq!(t(2958466.0, "yyyy"), "#VALUE!");
    assert_eq!(t(2958466.0, "[h]"), "71003184");
    assert_eq!(t(45000.6789, "b bbbb e"), "66 2566 2023");
    assert_eq!(t(1.5, "[h]:mm AM/PM"), "#VALUE!");
    assert_eq!(t(45000.0, "[$-407]dddd mmmm"), "Mittwoch März");
    assert_eq!(t(45000.0, "[$-F800]dddd"), "Wednesday, 15 March 2023");
}

#[test]
fn invalid_codes() {
    for code in [
        "0.00e+00",
        "[Red",
        "0;0;0;0;0",
        "0 abc",
        "\"x",
        "0\\",
        "0_",
        "0*",
        "[Purple]0",
        "[Red][Blue]0",
        "d0",
        "0d",
        "ss.0000",
        "# ?/?/?",
        "/?",
        "[ >1]0",
        "Gen",
    ] {
        assert!(
            NumberFormat::parse(code).is_err(),
            "{code} should be invalid"
        );
    }
}
