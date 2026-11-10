use xlcalc_variant::Variant;
use xlcalc_vba::{ExcelNames, ModuleKind, NoHost, Project, SourceModule};

fn run(source: &str) -> Variant {
    let m = SourceModule {
        name: "M".to_string(),
        kind: ModuleKind::Standard,
        source: source.to_string(),
    };
    let p = Project::compile(&[m], &ExcelNames);
    assert!(p.errors().is_empty(), "{:?}", p.errors());
    p.call("F", &[], &mut NoHost)
        .unwrap_or_else(|e| panic!("{e:?}"))
}

fn s(v: &str) -> Variant {
    Variant::Bstr(v.to_string())
}

#[test]
fn write_read_round_trip() {
    let src = r#"
Function F() As String
    Dim p As String, a As String, n As Long, t As String
    p = Environ$("TEMP") & "\xlcalc_files_t1.txt"
    Open p For Output As #1
    Write #1, "x", 5
    Print #1, "line two"
    Close #1
    Open p For Input As #1
    Input #1, a, n
    Line Input #1, t
    Close #1
    Kill p
    F = a & n & "|" & t
End Function"#;
    assert_eq!(run(src), s("x5|line two"));
}

#[test]
fn binary_and_random() {
    let src = r#"
Function F() As String
    Dim p As String, x As Long, y As Long
    p = Environ$("TEMP") & "\xlcalc_files_t2.bin"
    Open p For Random As #1 Len = 8
    x = 7: Put #1, 2, x
    Get #1, 2, y
    F = y & "|" & LOF(1) & "|" & Loc(1)
    Close #1
    Kill p
End Function"#;
    assert_eq!(run(src), s("7|12|2"));
}

#[test]
fn array_get() {
    let src = r#"
Function F() As String
    Dim p As String
    p = Environ$("TEMP") & "\xlcalc_files_t3.bin"
    Open p For Binary As #1
    Dim a(1 To 3) As Byte
    Put #1, , 65
    Put #1, , 66
    Put #1, , 67
    Get #1, 1, a
    F = a(1) & a(2) & a(3)
    Close #1
    Kill p
End Function"#;
    assert_eq!(run(src), s("65066"));
}
