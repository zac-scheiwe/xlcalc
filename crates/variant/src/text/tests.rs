use crate::oleaut::{differences, Rng};
use crate::{flags, Decimal, Locale, SafeArray, VarType, Variant};

const LOCALES: [Locale; 2] = [Locale::EN_US, Locale::EN_NZ];
const FLAGS: [u16; 4] = [
    0,
    flags::NOUSEROVERRIDE,
    flags::ALPHABOOL,
    flags::LOCALBOOL | flags::NOUSEROVERRIDE,
];

fn check(cases: &[Variant], to: VarType) {
    for locale in LOCALES {
        for flags in FLAGS {
            let diffs = differences(cases.iter().cloned(), to, locale, flags);
            let shown: Vec<_> = diffs.iter().take(20).collect();
            assert!(
                diffs.is_empty(),
                "{} differences ({locale:?}, flags {flags:#x}): {shown:#?}",
                diffs.len()
            );
        }
    }
}

fn check_once(cases: Vec<Variant>, to: VarType) {
    let diffs = differences(cases, to, Locale::EN_US, 0);
    let shown: Vec<_> = diffs.iter().take(20).collect();
    assert!(diffs.is_empty(), "{} differences: {shown:#?}", diffs.len());
}

fn texts(strings: &[&str]) -> Vec<Variant> {
    strings
        .iter()
        .map(|s| Variant::Bstr(s.to_string()))
        .collect()
}

fn pick<'a>(rng: &mut Rng, items: &[&'a str]) -> &'a str {
    items[rng.below(items.len() as u64) as usize]
}

fn edge_doubles() -> Vec<f64> {
    let mut xs = vec![
        0.0,
        -0.0,
        0.5,
        0.1,
        999999999999999.5,
        999999999999999.4,
        99999999999999.95,
        0.99999999999999995,
        1.000000000000005,
        1.000000000000015,
        2.5e-16,
        1.25e-7,
        f64::MAX,
        f64::MIN_POSITIVE,
        5e-324,
        f64::INFINITY,
        f64::NEG_INFINITY,
        f64::NAN,
        -f64::NAN,
        f64::from_bits(0x7FF0_0000_0000_0001),
        f64::from_bits(0xFFF0_0000_0000_0001),
        f64::from_bits(0xFFF8_0000_0000_0001),
        9999999.5,
        0.012345678901234567,
        123456789012345.67,
    ];
    for e in -324..=308 {
        let p: f64 = format!("1e{e}").parse().unwrap();
        for x in [p, p.next_up(), p.next_down(), 5.0 * p, 0.95 * p, 0.995 * p] {
            xs.push(x);
            xs.push(-x);
        }
    }

    for k in [
        1u64,
        7,
        123,
        99_999,
        123_456_789_012_345,
        999_999_999_999_999,
    ] {
        for s in 0..20 {
            xs.push((k as f64 + 0.5) / f64::powi(2.0, s));
        }
    }
    xs
}

#[test]
fn r8_to_text() {
    let mut rng = Rng(11);
    let mut xs = edge_doubles();
    for _ in 0..100_000 {
        xs.push(rng.double(-1074, 1023));
    }
    for _ in 0..2000 {
        xs.push(rng.any_double());
    }

    for _ in 0..5000 {
        let digits = rng.below(10_000_000_000_000_000) as f64;
        xs.push(digits / f64::powi(10.0, rng.below(40) as i32 - 10));
    }
    check_once(xs.iter().map(|&x| Variant::R8(x)).collect(), VarType::Bstr);
    let edges: Vec<_> = edge_doubles().into_iter().map(Variant::R8).collect();
    check(&edges, VarType::Bstr);
}

#[test]
fn r4_to_text() {
    let mut rng = Rng(12);
    let mut xs: Vec<f32> = edge_doubles().iter().map(|&x| x as f32).collect();
    xs.extend([
        f32::MAX,
        f32::MIN_POSITIVE,
        1e-45,
        16777216.0,
        9999999.0,
        0.1,
    ]);
    xs.extend([f32::from_bits(0x7F80_0001), f32::from_bits(0xFFC0_0000)]);
    for _ in 0..30_000 {
        xs.push(f32::from_bits(rng.next() as u32));
    }
    check_once(xs.iter().map(|&x| Variant::R4(x)).collect(), VarType::Bstr);
}

#[test]
fn others_to_text() {
    let mut rng = Rng(13);
    let mut cases = vec![
        Variant::Empty,
        Variant::Null,
        Variant::Error(0x800A_07FA_u32 as i32),
        Variant::Error(0x8002_0004_u32 as i32),
        Variant::Dispatch(None),
        Variant::Unknown(None),
        Variant::Bool(true),
        Variant::Bool(false),
        Variant::Array(Box::new(SafeArray {
            elem: VarType::Variant,
            bounds: vec![(1, 2)],
            data: vec![Variant::I4(1), Variant::Bstr("a".into())],
        })),
    ];
    for v in [
        0i64,
        1,
        -1,
        5,
        50,
        500,
        5000,
        10_000,
        15_000,
        i64::MAX,
        i64::MIN,
    ] {
        cases.extend([Variant::Cy(v), Variant::I8(v)]);
    }
    for v in [0i32, 1, -1, i32::MAX, i32::MIN] {
        cases.push(Variant::I4(v));
    }
    for v in [0i16, -1, i16::MAX, i16::MIN] {
        cases.push(Variant::I2(v));
    }
    cases.extend([Variant::Ui1(0), Variant::Ui1(255)]);
    for negative in [false, true] {
        for scale in [0, 1, 5, 27, 28] {
            for mantissa in [0, 1, 10, 12345, 1000, (1 << 96) - 1] {
                cases.push(Variant::Decimal(Decimal {
                    negative,
                    scale,
                    mantissa,
                }));
            }
        }
    }
    for _ in 0..2000 {
        let x = rng.next();
        cases.push(Variant::Cy(x as i64 >> rng.below(64)));
        cases.push(Variant::I8(x as i64 >> rng.below(64)));
        cases.push(Variant::I4(x as i32));
        cases.push(Variant::I2(x as i16));
        cases.push(Variant::Ui1(x as u8));
        cases.push(Variant::Decimal(Decimal {
            negative: x & 1 == 1,
            scale: rng.below(29) as u8,
            mantissa: (u128::from(rng.next()) << 32 | u128::from(x >> 32)) >> rng.below(96),
        }));
    }
    check(&cases, VarType::Bstr);
}

fn grammar(rng: &mut Rng) -> String {
    let mut s = String::new();
    for _ in 0..rng.below(4).saturating_sub(1) {
        s += pick(rng, &["+", "-", "(", "$", " ", "＄", "\t", "\u{3000}"]);
    }
    let zeros = rng.below(5) == 0;
    let digits = |rng: &mut Rng, n: u64, s: &mut String| {
        for _ in 0..n {
            let d = if zeros && rng.below(2) == 0 {
                0
            } else {
                rng.below(10)
            };
            s.push(char::from(b'0' + d as u8));
            if rng.below(20) == 0 {
                s.push(',');
            }
        }
    };
    let int_len = [0, 1, 1, 2, 3, 5, 8, 15, 16, 17, 19, 20, 21, 25, 31, 40];
    let n = int_len[rng.below(int_len.len() as u64) as usize];
    digits(rng, n, &mut s);
    if rng.below(3) > 0 {
        s.push('.');
        let frac_len = [0, 1, 2, 3, 5, 10, 15, 20, 28, 30, 35];
        let n = frac_len[rng.below(frac_len.len() as u64) as usize];
        digits(rng, n, &mut s);
    }
    if rng.below(5) < 2 {
        s += pick(rng, &["e", "E", "d", "D"]);
        s += pick(rng, &["", "", "+", "-"]);
        let e = match rng.below(10) {
            0..=5 => rng.below(31),
            6..=8 => rng.below(401),
            _ => {
                let places = rng.below(12) as u32 + 1;
                rng.below(10u64.pow(places))
            }
        };
        if rng.below(20) > 0 {
            s += &e.to_string();
        }
    }
    for _ in 0..rng.below(5).saturating_sub(2) {
        s += pick(rng, &["+", "-", ")", "$", " ", "\t", "＄"]);
    }
    if rng.below(10) == 0 {
        let junk = [
            "x", "a", "%", "/", "#", "&", "(", ")", "$", "⁒", ",", ".", "+", "-", " ", "e", "E",
            "d", "D", "H", "0", "٠", "１", "€", "\0",
        ];
        let at = s
            .char_indices()
            .map(|(i, _)| i)
            .nth(rng.below(s.chars().count() as u64 + 1) as usize)
            .unwrap_or(s.len());
        s.insert_str(at, pick(rng, &junk));
    }
    s
}

fn radix(rng: &mut Rng) -> String {
    let mut s = pick(rng, &["", " ", "  "]).to_string();
    s += "&";
    s += pick(rng, &["H", "h", "O", "o", "", "B"]);
    let lens = [0, 1, 2, 4, 8, 11, 15, 16, 17, 20, 21, 22, 23];
    let alphabet: &[u8] = if rng.below(2) == 0 {
        b"0123456789abcdefABCDEF"
    } else {
        b"01234567"
    };
    for _ in 0..lens[rng.below(lens.len() as u64) as usize] {
        s.push(char::from(
            alphabet[rng.below(alphabet.len() as u64) as usize],
        ));
    }
    s += pick(rng, &["", "", " ", "x", "-", ")"]);
    s
}

fn written(rng: &mut Rng) -> String {
    let lo = [-1074, -60, -20][rng.below(3) as usize];
    let hi = [1023, 60, 20][rng.below(3) as usize];
    let x = rng.double(lo, hi);
    match rng.below(5) {
        0 => format!("{x:?}"),
        1 => format!("{x:.16e}"),
        2 => format!("{x:.14e}"),
        3 => format!("{x:.*e}", rng.below(40) as usize + 1),
        _ => super::real::real_text(x, super::real::R8_DIGITS),
    }
}

fn any_string(rng: &mut Rng) -> String {
    match rng.below(20) {
        0..=10 => grammar(rng),
        11..=17 => written(rng),
        _ => radix(rng),
    }
}

const EDGE_STRINGS: &[&str] = &[
    "",
    " ",
    "1",
    " 1",
    "1 ",
    "+1",
    "-1",
    "- 1",
    "1-",
    "(1)",
    "$1",
    "-$1",
    "$-1",
    "1,000",
    "1,00",
    ",1",
    "1,",
    "1.5",
    ".5",
    "5.",
    "1e3",
    "1E+3",
    "1e-3",
    "1d3",
    "1e",
    "e3",
    "&H10",
    "&O10",
    "&B10",
    "&17",
    "&HFFFF",
    "&HFFFFFFFF",
    "&H80000000",
    "&H7FFFFFFF",
    "&H100000000",
    "&HFFFF8000",
    "&HFFFF7FFF",
    "&H8000000000000000",
    "&HFFFFFFFFFFFFFFFF",
    "&H1FFFFFFFFFFFFFFFF",
    "&O1777777777777777777777",
    "&O37777777777",
    "&O40000000000",
    "&H",
    "&",
    "&0",
    "0x10",
    "1 2",
    "1.2.3",
    "1e3e3",
    "1e400",
    "1e-400",
    "1.234,5",
    "1$",
    "$1$",
    "($1)",
    "(-1)",
    "--1",
    "(1",
    "1)",
    "1\u{0}2",
    "1E+",
    "1.e3",
    ".e3",
    ".",
    "-",
    "$",
    "00001",
    "1e999999999",
    "1e9999999999",
    "0e9999999999",
    "0e-300",
    "0e300",
    "1.7976931348623157e308",
    "1.79769313486232e308",
    "2.2250738585072014e-308",
    "4.9e-324",
    "2.5e-324",
    "123456789012345678901234567890",
    "9007199254740993",
    "1e23",
    "1e-320",
    "True",
    "true",
    "TRUE",
    "False",
    "fAlSe",
    "#TRUE#",
    "#FALSE#",
    "#true#",
    " True",
    "True ",
    "Wahr",
    "yes",
    "True\u{0}x",
    "Tru",
    "0",
    "-0",
    "0.0",
    "2",
    "Inf",
    "NaN",
    "１",
    "0.5",
    "1.5",
    "2.5",
    "-0.5",
    "-1.5",
    "0.05",
    "0.49999999999999999",
    "0.500000000000000000001",
    "32767",
    "32768",
    "-32768",
    "-32769",
    "32767.5",
    "-32768.5",
    "255",
    "256",
    "255.5",
    "254.5",
    "-0.4",
    "-0.6",
    "2147483647",
    "2147483648",
    "-2147483648",
    "-2147483649",
    "2147483647.5",
    "2147483646.5",
    "-2147483648.5",
    "4294967295",
    "9999999999",
    "2999999999",
    "9223372036854775807",
    "9223372036854775808",
    "-9223372036854775808",
    "9223372036854775807.5",
    "4000000000.5",
    "922337203685477.5807",
    "922337203685477.58075",
    "922337203685477.5808",
    "-922337203685477.5808",
    "-922337203685477.58085",
    "0.00005",
    "0.00015",
    "0.000050000000000000000001",
    "1e-30",
    "3.4028235e38",
    "3.40282356779e38",
    "3.40282356780e38",
    "1e39",
    "1.4e-45",
    "7e-46",
    "1e28",
    "1e29",
    "79228162514264337593543950335",
    "79228162514264337593543950336",
    "7.9228162514264337593543950335",
    "7.9228162514264337593543950336",
    "0.12345678901234567890123456785",
    "0.12345678901234567890123456775",
    "0.000000000000000000000000000049",
    "0.00000000000000000000000000005",
    "0.00000000000000000000000000015",
    "0.00000000000000000000000000025",
    "1e-28",
    "1e-29",
    "0.0000000000000000000000000000",
    "0.00000000000000000000000000000",
    "-0.0e-1",
    "12345678901234567890e-48",
    "12345678901234567890e-49",
    "9999999999999999999999999999.5",
    "7922816251426433759354395033.55",
    "7922816251426433759354395033.45",
    "0.1234567890123456789012345678901234567890",
    "1234567890123456789012345678901234567890",
];

fn string_cases(seed: u64, n: usize) -> Vec<Variant> {
    let mut rng = Rng(seed);
    let mut cases = texts(EDGE_STRINGS);
    cases.extend((0..n).map(|_| Variant::Bstr(any_string(&mut rng))));
    cases
}

#[test]
fn text_to_r8() {
    check_once(string_cases(21, 100_000), VarType::R8);
    check(&texts(EDGE_STRINGS), VarType::R8);
}

fn decimal_carry_bug(s: &str) -> bool {
    super::parse::parse(s, 30).is_ok_and(|p| p.shift == 0 && p.pwr10 + p.digits.len() as i64 == 29)
}

#[test]
fn text_to_other_numbers() {
    let all = string_cases(22, 20_000);
    for to in [
        VarType::I2,
        VarType::I4,
        VarType::I8,
        VarType::Ui1,
        VarType::R4,
        VarType::Cy,
        VarType::Decimal,
        VarType::Bool,
    ] {
        let kept = |v: &&Variant| match v {
            Variant::Bstr(s) => to != VarType::Decimal || !decimal_carry_bug(s),
            _ => true,
        };
        check_once(all.iter().filter(kept).cloned().collect(), to);
        let edges: Vec<_> = texts(EDGE_STRINGS).iter().filter(kept).cloned().collect();
        check(&edges, to);
    }
}

#[test]
fn text_to_non_numbers() {
    let cases = texts(&["", "1", "abc", "0", "True", "#TRUE#"]);
    for to in [
        VarType::Empty,
        VarType::Null,
        VarType::Error,
        VarType::Dispatch,
        VarType::Unknown,
        VarType::Bstr,
    ] {
        check(&cases, to);
    }
}
