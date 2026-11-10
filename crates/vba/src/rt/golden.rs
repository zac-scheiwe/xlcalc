use super::{builtin, RtContext};
use std::fmt::Write;
use xlcalc_variant::{HResult, SafeArray, VarType, Variant};

fn unesc(t: &str) -> String {
    let mut out = String::new();
    let mut rest = t;
    while let Some(i) = rest.find("\\u{") {
        out.push_str(&rest[..i]);
        let end = rest[i..].find('}').expect("closing brace") + i;
        let cp = u32::from_str_radix(&rest[i + 3..end], 16).expect("hex");
        out.push(char::from_u32(cp).expect("scalar value"));
        rest = &rest[end + 1..];
    }
    out.push_str(rest);
    out
}

fn esc(t: &str) -> String {
    const SAFE: &str = " .,:_-+/()[]=#$%&*'?@!;<>~^";
    let mut out = String::new();
    for c in t.chars() {
        if c.is_ascii_alphanumeric() || SAFE.contains(c) {
            out.push(c);
        } else {
            write!(out, "\\u{{{:X}}}", c as u32).expect("write");
        }
    }
    out
}

fn arg(tok: &str) -> Variant {
    let (kind, v) = tok.split_once(':').unwrap_or((tok, ""));
    let bits64 = || f64::from_bits(u64::from_str_radix(v, 16).expect("bits"));
    match kind {
        "s" => Variant::Bstr(unesc(v)),
        "i2" => Variant::I2(v.parse().expect("i2")),
        "i4" => Variant::I4(v.parse().expect("i4")),
        "u1" => Variant::Ui1(v.parse().expect("u1")),
        "r8" => Variant::R8(bits64()),
        "r4" => Variant::R4(f32::from_bits(u32::from_str_radix(v, 16).expect("bits"))),
        "cy" => Variant::Cy(v.parse().expect("cy")),
        "d" => Variant::Date(bits64()),
        "b" => Variant::Bool(v == "1"),
        "null" => Variant::Null,
        "empty" => Variant::Empty,
        "miss" => Variant::Error(HResult::DISP_E_PARAMNOTFOUND.0),
        "err" => Variant::Error(0x800A_0000_u32 as i32 + v.parse::<i32>().expect("code")),
        _ => panic!("argument {tok}"),
    }
}

fn run(ctx: &RtContext, name: &str, args: &[Variant]) -> Result<Variant, i32> {
    let call = |n: &str, a: &[Variant]| {
        let b = builtin(n).unwrap_or_else(|| panic!("no built-in {n}"));
        (b.call)(ctx, a).map_err(|e| e.number)
    };
    match name {
        "Echo" => Ok(args[0].clone()),
        "Join_split" => {
            let parts = call("Split", &args[..2])?;
            let mut a = vec![parts];
            a.extend_from_slice(&args[2..]);
            call("Join", &a)
        }
        "Join_arr" => {
            let n = args.len() - 1;
            let arr = Variant::Array(Box::new(SafeArray {
                elem: VarType::Variant,
                bounds: vec![(0, u32::try_from(n).expect("small"))],
                data: args[..n].to_vec(),
            }));
            call("Join", &[arr, args[n].clone()])
        }
        _ => call(name, args),
    }
}

fn dump(v: &Variant) -> String {
    let Variant::Array(a) = v else {
        return format!("not an array: {v:?}");
    };
    let (lb, n) = a.bounds[0];
    let items: Vec<String> = a
        .data
        .iter()
        .map(|e| match e {
            Variant::Bstr(s) => s.clone(),
            other => format!("{other:?}"),
        })
        .collect();
    let n = i64::from(n);
    format!("A{}:{}:{}", lb, i64::from(lb) + n - 1, items.join("|"))
}

fn canon(r: Result<Variant, i32>, post: &str) -> String {
    let v = match r {
        Ok(v) => v,
        Err(n) => return format!("ERR:{n}"),
    };
    match v {
        Variant::Bstr(s) => format!("S:{}", esc(&super::host_text(&s))),
        Variant::BstrNull => "SN:".to_string(),
        Variant::I2(n) => format!("I2:{n}"),
        Variant::I4(n) => format!("I4:{n}"),
        Variant::Ui1(n) => format!("UI1:{n}"),
        Variant::R8(x) => format!("R8:{:016X}", x.to_bits()),
        Variant::R4(x) => format!("R4:{:08X}", x.to_bits()),
        Variant::Date(x) => format!("DATE:{:016X}", x.to_bits()),
        Variant::Cy(n) => format!("CY:{n}"),
        Variant::Bool(b) => format!("BOOL:{}", i32::from(b)),
        Variant::Empty => "EMPTY".to_string(),
        Variant::Null => "NULL".to_string(),
        Variant::Error(e) => format!("ERROR:{:08X}", e as u32),
        Variant::Decimal(d) => format!("DEC:{}:{}:{}", u8::from(d.negative), d.scale, d.mantissa),
        Variant::Array(_) if post == "dump" => format!("S:{}", esc(&dump(&v))),
        other => format!("OTHER:{other:?}"),
    }
}

fn check(data: &str) -> (usize, Vec<String>) {
    let mut total = 0;
    let mut bad = Vec::new();
    for line in data.lines() {
        let f: Vec<&str> = line.split('\t').collect();
        let ctx = RtContext {
            compare_text: f[0] == "T",
            ..RtContext::default()
        };
        let args: Vec<Variant> = f[4..].iter().map(|t| arg(t)).collect();
        let got = canon(run(&ctx, f[1], &args), f[2]);
        total += 1;
        if got != f[3] {
            bad.push(format!(
                "{} {} {:?} -> {got}, Excel {}",
                f[0],
                f[1],
                &f[4..],
                f[3]
            ));
        }
    }
    (total, bad)
}

fn report(name: &str, data: &str) {
    let (total, bad) = check(data);
    if let Ok(path) = std::env::var("RT_GOLDEN_REPORT") {
        std::fs::write(format!("{path}/{name}.txt"), bad.join("\n")).expect("write report");
    }
    assert!(
        bad.is_empty(),
        "{name}: {} of {total} rows differ from Excel:\n{}",
        bad.len(),
        bad.iter().take(40).cloned().collect::<Vec<_>>().join("\n")
    );
}

#[test]
fn text_matches_excel() {
    report("text", include_str!("testdata/vba_rt_text.tsv"));
}

#[test]
fn conversions_match_excel() {
    report("conv", include_str!("testdata/vba_rt_conv.tsv"));
}

#[test]
fn dates_match_excel() {
    report("date", include_str!("testdata/vba_rt_date.tsv"));
}

#[test]
fn named_formats_match_excel() {
    report("format", include_str!("testdata/vba_rt_format_named.tsv"));
}
