use super::{builtin, RtContext};
use xlcalc_variant::Variant;

fn call(name: &str, args: &[Variant]) -> Result<Variant, i32> {
    let b = builtin(name).unwrap_or_else(|| panic!("no built-in {name}"));
    assert!(args.len() >= usize::from(b.min_args) && args.len() <= usize::from(b.max_args));
    (b.call)(&RtContext::default(), args).map_err(|e| e.number)
}

fn s(x: &str) -> Variant {
    Variant::Bstr(x.to_string())
}

fn txt(name: &str, args: &[Variant]) -> String {
    match call(name, args) {
        Ok(Variant::Bstr(t)) => t,
        other => panic!("{name}: {other:?}"),
    }
}

fn rows(data: &str) -> impl Iterator<Item = Vec<&str>> {
    data.lines().map(|l| l.split('\t').collect())
}

fn bits(h: &str) -> f64 {
    f64::from_bits(u64::from_str_radix(h, 16).expect("hex"))
}

#[test]
fn cstr_double_matches_excel() {
    for r in rows(include_str!("testdata/cstr.tsv")) {
        let x = bits(r[0]);
        assert_eq!(txt("CStr", &[Variant::R8(x)]), r[1], "{x:e}");
    }
}

#[test]
fn format_matches_excel() {
    for r in rows(include_str!("testdata/format.tsv")) {
        let x = bits(r[0]);
        let got = call("Format", &[Variant::R8(x), s(r[1])]);
        if r[2] == "<e>" {
            assert!(got.is_err(), "{x:e} {}", r[1]);
        } else {
            assert_eq!(got, Ok(s(r[2])), "{x:e} {}", r[1]);
        }
    }
}

fn n(v: i32) -> Variant {
    Variant::I4(v)
}

fn clock() -> f64 {
    45366.0 + (13.0 * 3600.0 + 45.0 * 60.0 + 30.0) / 86_400.0
}

fn with_clock(name: &str, args: &[Variant]) -> Variant {
    let ctx = RtContext {
        clock,
        ..RtContext::default()
    };
    (builtin(name).expect("built-in").call)(&ctx, args).expect("ok")
}

#[test]
fn conversions() {
    assert_eq!(call("CLng", &[Variant::R8(2.5)]), Ok(n(2)));
    assert_eq!(call("CLng", &[Variant::R8(3.5)]), Ok(n(4)));
    assert_eq!(call("CInt", &[Variant::R8(40000.0)]), Err(6));
    assert_eq!(call("CLng", &[s("abc")]), Err(13));
    assert_eq!(call("CLng", &[Variant::Null]), Err(94));
    assert_eq!(call("CBool", &[n(5)]), Ok(Variant::Bool(true)));
    assert_eq!(call("CByte", &[n(256)]), Err(6));
    assert_eq!(
        call("CVErr", &[n(2007)]),
        Ok(Variant::Error(0x800A_07D7_u32 as i32))
    );
    assert_eq!(txt("CStr", &[Variant::Bool(true)]), "True");
    assert_eq!(txt("Str", &[n(5)]), " 5");
    assert_eq!(txt("Str", &[n(-5)]), "-5");
    assert_eq!(txt("Hex", &[n(255)]), "FF");
    assert_eq!(txt("Hex", &[n(-1)]), "FFFFFFFF");
    assert_eq!(txt("Hex", &[Variant::I2(-1)]), "FFFF");
    assert_eq!(txt("Oct", &[n(8)]), "10");
    assert_eq!(call("Val", &[s(" 12 34abc")]), Ok(Variant::R8(1234.0)));
    assert_eq!(call("Val", &[s("&HFF")]), Ok(Variant::R8(255.0)));
    assert_eq!(call("Val", &[s("&HFFFF")]), Ok(Variant::R8(-1.0)));
    assert_eq!(call("Val", &[s("1.5e2x")]), Ok(Variant::R8(150.0)));
    assert_eq!(call("Val", &[s("abc")]), Ok(Variant::R8(0.0)));
    assert_eq!(call("Round", &[Variant::R8(2.5)]), Ok(Variant::R8(2.0)));
    assert_eq!(
        call("Round", &[Variant::R8(2.675), n(2)]),
        Ok(Variant::R8(2.68))
    );
    assert_eq!(call("Fix", &[Variant::R8(-2.5)]), Ok(Variant::R8(-2.0)));
    assert_eq!(call("Int", &[Variant::R8(-2.5)]), Ok(Variant::R8(-3.0)));
    assert_eq!(call("Abs", &[n(-3)]), Ok(n(3)));
    assert_eq!(call("Sgn", &[Variant::R8(-0.5)]), Ok(Variant::I2(-1)));
}

#[test]
fn maths() {
    assert_eq!(call("Sqr", &[n(-1)]), Err(5));
    assert_eq!(call("Log", &[n(0)]), Err(5));
    assert_eq!(call("Sin", &[Variant::R8(1e19)]), Err(5));
    assert_eq!(call("Exp", &[n(1000)]), Err(6));
    assert_eq!(call("Sqr", &[n(4)]), Ok(Variant::R8(2.0)));
    assert_eq!(call("Atn", &[n(0)]), Ok(Variant::R8(0.0)));
    assert_eq!(call("Cos", &[n(0)]), Ok(Variant::R8(1.0)));
}

#[test]
fn strings() {
    assert_eq!(txt("Left", &[s("hello"), n(2)]), "he");
    assert_eq!(txt("Right", &[s("hello"), n(9)]), "hello");
    assert_eq!(txt("Mid", &[s("hello"), n(2), n(3)]), "ell");
    assert_eq!(txt("Mid", &[s("hello"), n(4)]), "lo");
    assert_eq!(call("Left", &[s("x"), n(-1)]), Err(5));
    assert_eq!(call("Left", &[Variant::Null, n(1)]), Ok(Variant::Null));
    assert_eq!(call("Left$", &[Variant::Null, n(1)]), Err(94));
    assert_eq!(call("Len", &[s("abc")]), Ok(n(3)));
    assert_eq!(call("InStr", &[s("abcabc"), s("c")]), Ok(n(3)));
    assert_eq!(call("InStr", &[n(4), s("abcabc"), s("c")]), Ok(n(6)));
    assert_eq!(call("InStr", &[s("abc"), s("B")]), Ok(n(0)));

    assert_eq!(call("InStr", &[s("abc"), s("B"), n(1)]), Err(13));
    assert_eq!(call("InStrRev", &[s("abcabc"), s("b")]), Ok(n(5)));
    assert_eq!(
        txt("Replace", &[s("aaa"), s("a"), s("bb"), n(2), n(1)]),
        "bba"
    );
    assert_eq!(txt("UCase", &[s("abc")]), "ABC");
    assert_eq!(txt("Trim", &[s("  a b  ")]), "a b");
    assert_eq!(txt("Space", &[n(3)]), "   ");
    assert_eq!(txt("String", &[n(3), s("xy")]), "xxx");
    assert_eq!(txt("String", &[n(2), n(65)]), "AA");
    assert_eq!(call("StrComp", &[s("a"), s("B")]), Ok(Variant::I2(1)));
    assert_eq!(
        call("StrComp", &[s("a"), s("B"), n(1)]),
        Ok(Variant::I2(-1))
    );
    assert_eq!(txt("StrReverse", &[s("abc")]), "cba");
    assert_eq!(call("Asc", &[s("A")]), Ok(Variant::I2(65)));
    assert_eq!(call("Asc", &[s("\u{20ac}")]), Ok(Variant::I2(128)));
    assert_eq!(txt("Chr", &[n(65)]), "A");
    assert_eq!(call("Chr", &[n(256)]), Err(5));
    assert_eq!(txt("ChrW", &[n(0x20ac)]), "\u{20ac}");
}

#[test]
fn split_and_join() {
    let Ok(Variant::Array(a)) = call("Split", &[s("a,b,,c"), s(",")]) else {
        panic!("array");
    };
    assert_eq!(a.bounds, vec![(0, 4)]);
    assert_eq!(a.data[2], s(""));
    let parts = call("Split", &[s("a,b,c"), s(","), n(2)]).expect("ok");
    assert_eq!(txt("Join", &[parts, s("-")]), "a-b,c");
    let Ok(Variant::Array(e)) = call("Split", &[s("")]) else {
        panic!("array");
    };
    assert_eq!(e.bounds, vec![(0, 0)]);
}

#[test]
fn information() {
    assert_eq!(call("IsNumeric", &[s("1,234")]), Ok(Variant::Bool(true)));
    assert_eq!(call("IsNumeric", &[s("x")]), Ok(Variant::Bool(false)));
    assert_eq!(call("IsDate", &[s("15/03/2024")]), Ok(Variant::Bool(true)));
    assert_eq!(call("IsDate", &[s("31/02/2024")]), Ok(Variant::Bool(false)));
    assert_eq!(call("IsDate", &[n(5)]), Ok(Variant::Bool(false)));
    assert_eq!(call("VarType", &[s("")]), Ok(Variant::I4(8)));
    assert_eq!(txt("TypeName", &[Variant::R8(1.0)]), "Double");
    assert_eq!(txt("TypeName", &[Variant::Null]), "Null");
}

#[test]
fn dates() {
    let ser = |y, m, d| call("DateSerial", &[n(y), n(m), n(d)]);
    assert_eq!(ser(2024, 3, 15), Ok(Variant::Date(45366.0)));
    assert_eq!(ser(2024, 14, 1), Ok(Variant::Date(45689.0)));
    assert_eq!(ser(1899, 12, 30), Ok(Variant::Date(0.0)));
    assert_eq!(ser(99, 1, 1), Ok(Variant::Date(36161.0)));
    let d = Variant::Date(45366.5);
    assert_eq!(
        call("Year", std::slice::from_ref(&d)),
        Ok(Variant::I2(2024))
    );
    assert_eq!(call("Month", std::slice::from_ref(&d)), Ok(Variant::I2(3)));
    assert_eq!(call("Day", std::slice::from_ref(&d)), Ok(Variant::I2(15)));
    assert_eq!(call("Hour", std::slice::from_ref(&d)), Ok(Variant::I2(12)));
    assert_eq!(
        call("Weekday", std::slice::from_ref(&d)),
        Ok(Variant::I2(6))
    );
    assert_eq!(call("Weekday", &[d, n(2)]), Ok(Variant::I2(5)));
    assert_eq!(call("Year", &[s("15 Mar 2024")]), Ok(Variant::I2(2024)));
    assert_eq!(call("Minute", &[s("1:30 PM")]), Ok(Variant::I2(30)));
    assert_eq!(call("Hour", &[s("1:30 PM")]), Ok(Variant::I2(13)));
    assert_eq!(
        call("CDate", &[s("15/03/2024 06:00")]),
        Ok(Variant::Date(45366.25))
    );
    assert_eq!(
        call("TimeSerial", &[n(25), n(0), n(0)]),
        Ok(Variant::Date(25.0 / 24.0))
    );
    assert_eq!(
        call("DateAdd", &[s("m"), n(1), Variant::Date(45688.0)]),
        Ok(Variant::Date(45716.0))
    );
    assert_eq!(
        call("DateAdd", &[s("d"), n(-1), Variant::Date(45366.0)]),
        Ok(Variant::Date(45365.0))
    );
    let diff = |u, a, b| call("DateDiff", &[s(u), Variant::Date(a), Variant::Date(b)]);
    assert_eq!(diff("d", 45366.0, 45370.5), Ok(n(4)));
    assert_eq!(diff("m", 45688.0, 45716.0), Ok(n(1)));
    assert_eq!(diff("x", 1.0, 2.0), Err(5));
}

#[test]
fn clock_functions() {
    assert_eq!(with_clock("Date", &[]), Variant::Date(45366.0));
    assert_eq!(with_clock("Now", &[]), Variant::Date(clock()));
    assert_eq!(with_clock("Timer", &[]), Variant::R4(49530.0));
    assert_eq!(with_clock("Time$", &[]), s("13:45:30"));
    assert_eq!(with_clock("Date$", &[]), s("03-15-2024"));
}

#[test]
fn format_named() {
    let f = |x: Variant, code: &str| txt("Format", &[x, s(code)]);
    assert_eq!(f(Variant::R8(1234.567), "Fixed"), "1234.57");
    assert_eq!(f(Variant::R8(1234.567), "Standard"), "1,234.57");
    assert_eq!(f(Variant::R8(0.256), "Percent"), "25.60%");
    assert_eq!(f(Variant::R8(1234.5), "Scientific"), "1.23E+03");
    assert_eq!(f(n(0), "Yes/No"), "No");
    assert_eq!(f(n(2), "On/Off"), "On");
    assert_eq!(f(Variant::Bool(true), "True/False"), "True");
    assert_eq!(f(Variant::Date(45366.5), "Short Date"), "15/03/2024");
    assert_eq!(f(Variant::Date(45366.5), "Long Time"), "12:00:00 pm");
    assert_eq!(
        f(Variant::Date(45366.5), "yyyy-mm-dd hh:mm"),
        "2024-03-15 12:00"
    );
    assert_eq!(
        f(Variant::Date(45366.5), "General Date"),
        "15/03/2024 12:00:00 pm"
    );
    assert_eq!(f(Variant::Date(0.25), "General Date"), "6:00:00 am");
    assert_eq!(f(s("5"), "0.00"), "5.00");
    assert_eq!(f(s("abc"), ">"), "ABC");
    assert_eq!(txt("Format", &[Variant::R8(0.5)]), "0.5");
    assert_eq!(call("Format", &[Variant::Null]), Ok(Variant::BstrNull));
    assert_eq!(call("Format$", &[Variant::Null]), Ok(Variant::BstrNull));
}

#[test]
fn registry() {
    assert!(builtin("cstr").is_some());
    assert!(builtin("Nope").is_none());
}

#[test]
fn format_text_sections() {
    let f = |x: &str, code: &str| txt("Format", &[s(x), s(code)]);
    assert_eq!(f("abc", ">"), "ABC");
    assert_eq!(f("ABC", "<"), "abc");
    assert_eq!(f("ab", "@@@@"), "  ab");
    assert_eq!(f("ab", "&&&&"), "ab");
    assert_eq!(f("abcdef", "(@@)"), "(ab)cdef");
}

#[test]
fn builtins_declare_their_return_type() {
    use crate::types::VType;
    let ret = |n: &str| builtin(n).expect("built-in").ret.clone();
    assert_eq!(ret("Len"), VType::Long);
    assert_eq!(ret("Left$"), VType::String);
    assert_eq!(ret("Left"), VType::Variant);
    assert_eq!(ret("IIf"), VType::Variant);
    assert_eq!(ret("CDate"), VType::Date);
    assert!(format!("{:?}", builtin("Asc").expect("Asc")).contains("Integer"));
}

#[test]
fn iif_picks_a_part() {
    assert_eq!(
        call("IIf", &[Variant::Bool(true), n(1), n(2)]),
        Ok(Variant::I4(1))
    );
    assert_eq!(call("IIf", &[s("0"), n(1), n(2)]), Ok(Variant::I4(2)));
    assert_eq!(call("IIf", &[s("abc"), n(1), n(2)]), Err(13));
}
