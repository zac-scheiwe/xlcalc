use super::*;
use crate::oleaut::{self, same, Rng};
use crate::Decimal;

fn dec(m: u128, s: u8, n: bool) -> Variant {
    Variant::Decimal(Decimal {
        negative: n,
        scale: s,
        mantissa: m,
    })
}

fn values() -> Vec<Variant> {
    let mut v = vec![
        Variant::Empty,
        Variant::Null,
        Variant::Error(5),
        Variant::Dispatch(None),
        Variant::Bool(true),
        Variant::Bool(false),
    ];
    v.extend([0u8, 1, 2, 7, 127, 128, 200, 255].map(Variant::Ui1));
    v.extend([0i16, 1, -1, 2, 7, 300, 32767, -32768, 255, 256].map(Variant::I2));
    v.extend([0i32, 1, -1, 3, 70000, -70000, i32::MAX, i32::MIN, 46341].map(Variant::I4));
    v.extend(
        [
            0i64,
            1,
            -1,
            5,
            1 << 40,
            -(1 << 40),
            i64::MAX,
            i64::MIN,
            3_037_000_500,
        ]
        .map(Variant::I8),
    );
    v.extend(
        [
            0.0f32, -0.0, 1.0, 2.5, -2.5, 7.5, 0.1, 3e38, -3e38, 1e-40, 16777217.0,
        ]
        .map(Variant::R4),
    );
    v.extend(
        [
            0.0,
            -0.0,
            1.0,
            -1.0,
            0.5,
            1.5,
            2.5,
            3.5,
            -2.5,
            7.5,
            0.1,
            1e10,
            -1e10,
            2147483647.5,
            2147483648.0,
            1e15,
            1e300,
            1e308,
            -1e308,
            1e-300,
            4.5e18,
            9.3e18,
            0.3,
            100.0,
            8.5,
        ]
        .map(Variant::R8),
    );
    v.extend(
        [
            0i64,
            1,
            -1,
            10000,
            15000,
            -15000,
            25000,
            12345678,
            i64::MAX,
            i64::MIN,
            1_000_000_000_000,
        ]
        .map(Variant::Cy),
    );
    v.extend(
        [
            0.0, 1.0, -1.0, 2.5, 36526.5, 2958465.0, -657434.0, 2958465.99,
        ]
        .map(Variant::Date),
    );
    v.extend([
        dec(0, 0, false),
        dec(1, 0, false),
        dec(3, 0, true),
        dec(75, 1, false),
        dec(12345, 2, true),
        dec(1, 28, false),
        dec(u128::from(u64::MAX), 0, false),
        dec((1 << 96) - 1, 0, false),
        dec((1 << 96) - 1, 28, true),
        dec((1 << 96) - 1, 10, false),
        dec(10, 1, false),
        dec(0, 5, true),
    ]);
    for s in [
        "",
        "7",
        "abc",
        "2.5",
        " 12 ",
        "1e3",
        "-4",
        "True",
        "false",
        "#TRUE#",
        "1e400",
        "&H10",
        "$5",
        "0",
        "1,5",
        "123456789012345678",
    ] {
        v.push(Variant::Bstr(s.to_string()));
    }
    v
}

fn random_values(n: usize) -> Vec<Variant> {
    let mut rng = Rng(0xBEEF);
    let mut v = Vec::new();
    for _ in 0..n {
        v.push(Variant::R8(rng.double(-40, 70)));
        v.push(Variant::R4(rng.double(-20, 40) as f32));
        v.push(Variant::I4(rng.next() as i32 >> rng.below(30)));
        v.push(Variant::I2(rng.next() as i16 >> rng.below(15)));
        v.push(Variant::I8(rng.next() as i64 >> rng.below(60)));
        v.push(Variant::Cy(rng.next() as i64 >> rng.below(60)));
        v.push(Variant::Date(rng.double(-5, 22)));
        v.push(dec(
            u128::from(rng.next()) << rng.below(33) | u128::from(rng.next() >> 40),
            rng.below(29) as u8,
            rng.below(2) == 0,
        ));
    }
    v
}

fn agree(a: &Result<Variant, HResult>, b: &Result<Variant, HResult>) -> bool {
    match (a, b) {
        (Ok(x), Ok(y)) => same(x, y),
        (x, y) => x == y,
    }
}

fn binary_diffs(op: Binary, xs: &[Variant], ys: &[Variant]) -> Vec<String> {
    let mut out = Vec::new();
    for a in xs {
        for b in ys {
            let ours = binary(op, a, b);
            let theirs = oleaut::binary(op, a, b);
            if !agree(&ours, &theirs) {
                out.push(format!(
                    "{op:?} {a:?} {b:?}: ours {ours:?}, oleaut32 {theirs:?}"
                ));
            }
        }
    }
    out
}

fn report(what: &str, found: Vec<String>) {
    assert!(
        found.is_empty(),
        "{what}: {} differences, the first ones:\n{}",
        found.len(),
        found[..found.len().min(25)].join("\n")
    );
}

fn big() -> usize {
    std::env::var("XLCALC_BIG")
        .ok()
        .and_then(|s| s.parse().ok())
        .unwrap_or(0)
}

fn binary_test(ops: &[Binary]) {
    let v = values();
    let r = random_values(big());
    for &op in ops {
        report(&format!("{op:?}"), binary_diffs(op, &v, &v));
        if !r.is_empty() {
            let mut all = v.clone();
            all.extend(r.iter().cloned());
            report(&format!("{op:?} random"), binary_diffs(op, &all, &r));
        }
    }
}

#[test]
fn add() {
    binary_test(&[Binary::Add]);
}
#[test]
fn sub() {
    binary_test(&[Binary::Sub]);
}
#[test]
fn mul() {
    binary_test(&[Binary::Mul]);
}
#[test]
fn div() {
    binary_test(&[Binary::Div]);
}
#[test]
fn idiv() {
    binary_test(&[Binary::Idiv]);
}
#[test]
fn modulo() {
    binary_test(&[Binary::Mod]);
}
#[test]
fn pow() {
    binary_test(&[Binary::Pow]);
}

#[test]
fn pow_doubles() {
    let mut x = vec![
        0.0,
        -0.0,
        1.0,
        -1.0,
        0.5,
        -0.5,
        2.0,
        -2.0,
        -8.0,
        10.0,
        f64::INFINITY,
        f64::NEG_INFINITY,
        f64::NAN,
        f64::from_bits(0x7ff0_0000_0000_0001),
        f64::MAX,
        5e-324,
        1e-310,
        -1e300,
        1e300,
        1.0000000000000002,
    ];
    let mut rng = Rng(0x50E);
    for _ in 0..2000 + big() * 100 {
        x.push(rng.double(-30, 30));
        x.push(rng.any_double());
        x.push((rng.below(400) as f64 - 200.0) / 4.0);
    }
    let v: Vec<Variant> = x.into_iter().map(Variant::R8).collect();
    report("pow doubles", binary_diffs(Binary::Pow, &v[..20], &v));
    report("pow doubles", binary_diffs(Binary::Pow, &v, &v[..20]));
    let r: Vec<Variant> = v[20..].to_vec();
    report(
        "pow doubles random",
        binary_diffs(
            Binary::Pow,
            &r[..r.len().min(3000)],
            &r[..r.len().min(3000)],
        ),
    );
}

#[test]
fn cat() {
    binary_test(&[Binary::Cat]);
}
#[test]
fn logic() {
    binary_test(&[
        Binary::And,
        Binary::Or,
        Binary::Xor,
        Binary::Eqv,
        Binary::Imp,
    ]);
}

#[test]
fn unary_ops() {
    let mut v = values();
    v.extend(random_values(big()));
    let mut found = Vec::new();
    for op in [Unary::Neg, Unary::Not, Unary::Abs, Unary::Fix, Unary::Int] {
        for a in &v {
            let (ours, theirs) = (unary(op, a), oleaut::unary(op, a));
            if !agree(&ours, &theirs) {
                found.push(format!("{op:?} {a:?}: ours {ours:?}, oleaut32 {theirs:?}"));
            }
        }
    }
    report("unary", found);
}

#[test]
fn rounding() {
    let mut v = values();
    v.extend(random_values(big()));
    let mut found = Vec::new();
    for d in [
        0, 1, 2, 3, 4, 6, 10, 15, 19, 20, 21, 22, 23, 24, 26, 27, 28, 29, 30, -1,
    ] {
        for a in &v {
            let (ours, theirs) = (round(a, d), oleaut::round(a, d));
            if !agree(&ours, &theirs) {
                found.push(format!(
                    "Round({a:?}, {d}): ours {ours:?}, oleaut32 {theirs:?}"
                ));
            }
        }
    }
    report("round", found);
}

#[test]
fn rounding_floats() {
    let mut rng = Rng(0x0DD5);
    let mut found = Vec::new();
    let n = 60_000 + big() * 1000;
    for i in 0..n {
        let d = rng.below(24) as i32;
        let x = match i % 6 {
            0 => rng.double(-30, 60),
            1 => {
                let k = (rng.next() >> rng.below(64)) as f64 + 0.5;
                let x = k / 10f64.powi(d);
                f64::from_bits(x.to_bits().wrapping_add(rng.below(5).wrapping_sub(2)))
            }
            2 => 1e16 * (1.0 - rng.below(1 << 20) as f64 * f64::EPSILON) * [1.0, -1.0][i / 6 % 2],
            3 => rng.any_double(),
            4 => (rng.below(2000) as f64 - 1000.0) / 8.0,
            _ => rng.double(-60, 1100),
        };
        let mut cases = vec![Variant::R8(x), Variant::Date(x)];
        if i % 3 == 0 {
            cases.push(Variant::R4(x as f32));
        }
        for a in cases {
            let (ours, theirs) = (round(&a, d), oleaut::round(&a, d));
            if !agree(&ours, &theirs) && found.len() < 50 {
                found.push(format!(
                    "Round({a:?}, {d}): ours {ours:?}, oleaut32 {theirs:?}"
                ));
            }
        }
    }
    report("round floats", found);
}

#[test]
fn date_to_text() {
    let mut rng = Rng(77);
    let mut found = Vec::new();
    let n = 20_000 + big() * 20;
    for i in 0..n {
        let x = match i % 5 {
            4 => [
                0.999999999,
                -0.999999999,
                -0.5,
                -1.5,
                -1.0000001,
                -1.999999999,
                -2.9999999999,
                0.0000001,
                -0.0000001,
            ][i / 5 % 9],
            0 => rng.double(-5, 22),
            1 => (rng.below(5_000_000) as f64 - 650_000.0) + rng.below(86_401) as f64 / 86_400.0,
            2 => {
                (rng.below(300_000) as f64 - 5.0)
                    + (rng.below(86_400 * 1000) as f64 + 0.5) / 86_400_000.0
            }
            _ => (rng.below(100_000) as f64) + 1.0 - rng.below(3) as f64 * 1e-9,
        };
        let v = Variant::Date(x);
        for locale in [Locale::EN_NZ, Locale::EN_US] {
            let ours = crate::change_type(&v, VarType::Bstr, locale, 0);
            let theirs = oleaut::change_type(&v, VarType::Bstr, locale, 0);
            if ours != theirs && found.len() < 30 {
                found.push(format!(
                    "{x:?} {locale:?}: ours {ours:?}, oleaut32 {theirs:?}"
                ));
            }
        }
    }
    report("date text", found);
}

#[test]
fn comparing() {
    let mut v = values();
    v.extend(random_values(big()));
    v.extend(
        [
            "", "a", "A", "b", "B", "abc", "ABD", "a b", "a-b", "ab", "é", "e", "E", "10", "9",
            "-1", "z", "Z", "_", "1a",
        ]
        .map(|s| Variant::Bstr(s.into())),
    );
    let mut found = Vec::new();
    for locale in [Locale::EN_US, Locale::EN_NZ] {
        for flags in [0, 1] {
            for a in &v {
                for b in &v {
                    let ours = compare(a, b, locale, flags);
                    let theirs = oleaut::compare(a, b, locale, flags);
                    if ours != theirs {
                        found.push(format!(
                            "Cmp({a:?}, {b:?}, {locale:?}, {flags}): ours {ours:?}, oleaut32 {theirs:?}"
                        ));
                    }
                }
            }
        }
    }
    report("compare", found);
}

#[test]
fn comparing_strings() {
    let alphabet: Vec<char> = "aAbBzZ09 -_.'#$\u{e9}\u{c9}\u{e8}\u{f1}".chars().collect();
    let mut rng = Rng(5);
    let mut words = vec![String::new()];
    for _ in 0..200 + big() {
        let n = 1 + rng.below(4) as usize;
        words.push(
            (0..n)
                .map(|_| alphabet[rng.below(alphabet.len() as u64) as usize])
                .collect(),
        );
    }
    let mut found = Vec::new();
    for flags in [0, 1] {
        for a in &words {
            for b in &words {
                let (x, y) = (Variant::Bstr(a.clone()), Variant::Bstr(b.clone()));
                let ours = compare(&x, &y, Locale::EN_US, flags);
                let theirs = oleaut::compare(&x, &y, Locale::EN_US, flags);
                if ours != theirs {
                    found.push(format!(
                        "Cmp({a:?}, {b:?}, {flags}): ours {ours:?}, oleaut32 {theirs:?}"
                    ));
                }
            }
        }
    }
    report("compare strings", found);
}
