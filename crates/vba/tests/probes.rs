use std::fs;
use std::path::PathBuf;
use xlcalc_variant::Variant;
use xlcalc_vba::{ExcelNames, ModuleKind, NoHost, Project, SourceModule};

fn function(block: &str) -> &'static str {
    match block {
        "EXP" => "VExp",
        "LN" => "VLog",
        "SQRT" => "VSqr",
        "ATAN" => "VAtn",
        "SIN" => "VSin",
        "COS" => "VCos",
        "TAN" => "VTan",
        "POW" => "VPow",
        "MUL" => "DMul",
        "DIV" => "DDiv",
        "ADD" => "DAdd",
        "MULADD" => "DMulAdd",
        "MULADD_STORED" => "DMulAddStored",
        "VAR_MULADD" => "VarMulAdd",
        "VAR_MUL" => "VarMul",
        "STR" => "VStr",
        other => panic!("block {other}"),
    }
}

#[test]
fn probe2_sample() {
    let dir = PathBuf::from(env!("CARGO_MANIFEST_DIR"));
    let source = fs::read_to_string(dir.join("../../conformance/vba/vba_maths.bas")).unwrap();
    let module = SourceModule {
        name: "vba_maths".to_string(),
        kind: ModuleKind::Standard,
        source,
    };
    let project = Project::compile(&[module], &ExcelNames);
    assert!(project.errors().is_empty(), "{:?}", project.errors());
    let csv = fs::read_to_string(dir.join("tests/data/probe2_sample.csv")).unwrap();
    let mut failures = Vec::new();
    let mut n = 0;
    for line in csv.lines().filter(|l| !l.starts_with('#')).skip(1) {
        let c: Vec<&str> = line.split(',').collect();
        let args: Vec<Variant> = c[1..4]
            .iter()
            .filter(|s| !s.is_empty())
            .map(|s| Variant::R8(f64::from_bits(u64::from_str_radix(s, 16).unwrap())))
            .collect();
        let got = project.call(function(c[0]), &args, &mut NoHost);
        let ok = match &got {
            Ok(Variant::Bstr(s)) => c[0] == "STR" && s == c[5],
            Ok(Variant::R8(x)) => format!("{:016X}", x.to_bits()) == c[5],
            _ => false,
        };
        n += 1;
        if !ok {
            failures.push(format!("{line}: got {got:?}"));
        }
    }
    assert!(n > 3000, "only {n} rows");
    assert!(failures.is_empty(), "{}", failures.join("\n"));
}
