use xlcalc_variant::{ObjectId, SafeArray, VarType, Variant};
use xlcalc_vba::{
    ExcelNames, InvokeKind, ModuleKind, NoHost, ObjectHost, Project, SourceModule, VbaError,
};

fn project(source: &str) -> Project {
    let p = Project::compile(&[module("M", source)], &ExcelNames);
    assert!(p.errors().is_empty(), "{:?}", p.errors());
    p
}

fn module(name: &str, source: &str) -> SourceModule {
    SourceModule {
        name: name.to_string(),
        kind: ModuleKind::Standard,
        source: source.to_string(),
    }
}

fn run(source: &str, f: &str, args: &[Variant]) -> Result<Variant, VbaError> {
    project(source).call(f, args, &mut NoHost)
}

fn ok(source: &str, f: &str, args: &[Variant]) -> Variant {
    run(source, f, args).unwrap_or_else(|e| panic!("{f}: {e:?}"))
}

fn err(source: &str, f: &str, args: &[Variant]) -> i32 {
    match run(source, f, args) {
        Err(e) => e.number,
        Ok(v) => panic!("{f}: expected an error, got {v:?}"),
    }
}

fn s(v: &str) -> Variant {
    Variant::Bstr(v.to_string())
}

#[test]
fn if_and_elseif() {
    let src = r#"
Function F(x As Long) As String
    If x < 0 Then
        F = "neg"
    ElseIf x = 0 Then
        F = "zero"
    Else
        F = "pos"
    End If
End Function
Function G(x As Long) As Long
    If x > 1 Then G = 1 Else G = 2
End Function"#;
    assert_eq!(ok(src, "F", &[Variant::I4(-3)]), s("neg"));
    assert_eq!(ok(src, "F", &[Variant::I4(0)]), s("zero"));
    assert_eq!(ok(src, "F", &[Variant::I4(5)]), s("pos"));
    assert_eq!(ok(src, "G", &[Variant::I4(5)]), Variant::I4(1));
    assert_eq!(ok(src, "G", &[Variant::I4(0)]), Variant::I4(2));
}

#[test]
fn select_case() {
    let src = r#"
Function F(x As Variant) As String
    Select Case x
        Case 1, 2: F = "small"
        Case 3 To 5: F = "mid"
        Case "a": F = "letter"
        Case Is > 100: F = "big"
        Case Else: F = "other"
    End Select
End Function"#;
    assert_eq!(ok(src, "F", &[Variant::I4(2)]), s("small"));
    assert_eq!(ok(src, "F", &[Variant::R8(4.5)]), s("mid"));
    assert_eq!(ok(src, "F", &[Variant::I4(101)]), s("big"));
    assert_eq!(ok(src, "F", &[s("a")]), s("letter"));
    assert_eq!(ok(src, "F", &[Variant::I4(50)]), s("other"));

    assert_eq!(ok(src, "F", &[s("b")]), s("big"));
}

#[test]
fn for_loops() {
    let src = r#"
Function Total(n As Long) As Long
    Dim i As Long
    For i = 1 To n: Total = Total + i: Next
End Function
Function Down() As String
    Dim i As Integer
    For i = 10 To 1 Step -3
        Down = Down & i & ","
    Next i
    Down = Down & "end=" & i
End Function
Function Early() As Long
    Dim i As Long
    For i = 1 To 100
        If i * i > 50 Then Exit For
    Next
    Early = i
End Function
Function Frac() As Long
    Dim x As Double
    For x = 0 To 1 Step 0.25
        Frac = Frac + 1
    Next
End Function"#;
    assert_eq!(ok(src, "Total", &[Variant::I4(10)]), Variant::I4(55));
    assert_eq!(ok(src, "Total", &[Variant::I4(0)]), Variant::I4(0));
    assert_eq!(ok(src, "Down", &[]), s("10,7,4,1,end=-2"));
    assert_eq!(ok(src, "Early", &[]), Variant::I4(8));
    assert_eq!(ok(src, "Frac", &[]), Variant::I4(5));
}

#[test]
fn for_each_over_arrays() {
    let src = r#"
Function F() As Double
    Dim v As Variant
    For Each v In Array(1, 2.5, "3")
        F = F + v
    Next
End Function
Function G() As Long
    Dim a(1 To 2, 1 To 2) As Long, v As Variant
    a(1, 1) = 1: a(2, 1) = 2: a(1, 2) = 3: a(2, 2) = 4
    For Each v In a
        G = G * 10 + v
    Next
End Function"#;
    assert_eq!(ok(src, "F", &[]), Variant::R8(6.5));

    assert_eq!(ok(src, "G", &[]), Variant::I4(1234));
}

#[test]
fn do_and_while_loops() {
    let src = r#"
Function A() As Long
    Do While A < 5: A = A + 1: Loop
End Function
Function B() As Long
    Do
        B = B + 1
    Loop Until B >= 3
End Function
Function C() As Long
    Do Until C = 4
        C = C + 2
    Loop
End Function
Function D() As Long
    Do
        D = D + 1
        If D = 7 Then Exit Do
    Loop
End Function
Function E() As Long
    While E < 9
        E = E + 4
    Wend
End Function"#;
    assert_eq!(ok(src, "A", &[]), Variant::I4(5));
    assert_eq!(ok(src, "B", &[]), Variant::I4(3));
    assert_eq!(ok(src, "C", &[]), Variant::I4(4));
    assert_eq!(ok(src, "D", &[]), Variant::I4(7));
    assert_eq!(ok(src, "E", &[]), Variant::I4(12));
}

#[test]
fn goto_and_labels() {
    let src = r#"
Function F() As String
    Dim i As Long
again:
    i = i + 1
    If i < 3 Then GoTo again
    F = "i=" & i
    GoTo 100
    F = "skipped"
100 End Function"#;
    assert_eq!(ok(src, "F", &[]), s("i=3"));
}

#[test]
fn on_error_resume_next() {
    let src = r#"
Function F() As String
    Dim x As Long
    On Error Resume Next
    x = 1 / 0
    F = "after:" & Err.Number & ":" & x
End Function
Function G() As Long
    On Error Resume Next
    Err.Raise 13
    G = Err.Number
    Err.Clear
    G = G * 100 + Err.Number
End Function"#;
    assert_eq!(ok(src, "F", &[]), s("after:11:0"));
    assert_eq!(ok(src, "G", &[]), Variant::I4(1300));
}

#[test]
fn on_error_goto_and_resume() {
    let src = r#"
Function F() As String
    On Error GoTo handler
    Dim d As Long
    F = "q=" & (10 / d)
    Exit Function
handler:
    F = F & "[" & Err.Number & "]"
    d = 2
    Resume
End Function
Function G() As String
    On Error GoTo h
    Err.Raise 5
    G = G & "next"
    Exit Function
h:
    G = "h" & Err.Number & ","
    Resume Next
End Function
Function H() As Long
    On Error GoTo h
    Err.Raise 9
h:
    H = Err.Number
End Function
Function Unhandled() As Long
    On Error GoTo 0
    Unhandled = 1 / 0
End Function"#;

    assert_eq!(ok(src, "F", &[]), s("q=5"));
    assert_eq!(ok(src, "G", &[]), s("h5,next"));
    assert_eq!(ok(src, "H", &[]), Variant::I4(9));
    assert_eq!(err(src, "Unhandled", &[]), 11);
}

#[test]
fn errors_propagate_to_the_caller_handler() {
    let src = r#"
Function Inner() As Long
    Inner = CLng("x")
End Function
Function Outer() As String
    On Error GoTo h
    Outer = "v" & Inner()
    Exit Function
h:
    Outer = "caught " & Err.Number & " " & Err.Description
End Function"#;
    assert_eq!(ok(src, "Outer", &[]), s("caught 13 Type mismatch"));
}

#[test]
fn byref_and_byval() {
    let src = r#"
Sub Bump(ByRef n As Long, ByVal m As Long)
    n = n + 1
    m = m + 1
End Sub
Sub BumpDefault(n As Long)
    n = n * 2
End Sub
Function F() As String
    Dim a As Long, b As Long
    a = 1: b = 1
    Bump a, b
    BumpDefault a
    BumpDefault (b)
    F = a & "," & b
End Function
Sub SetElem(x As Variant)
    x = 9
End Sub
Function G() As Long
    Dim arr(2) As Variant
    SetElem arr(1)
    G = arr(1)
End Function"#;

    assert_eq!(ok(src, "F", &[]), s("4,1"));
    assert_eq!(ok(src, "G", &[]), Variant::I4(9));
}

#[test]
fn optional_and_param_array() {
    let src = r#"
Function Opt(a As Long, Optional b As Long = 10, Optional c As Variant) As String
    Opt = a + b & "/" & IsMissing(c)
End Function
Function Sum(ParamArray xs() As Variant) As Double
    Dim x As Variant
    For Each x In xs
        Sum = Sum + x
    Next
End Function
Function Count(ParamArray xs() As Variant) As Long
    Count = UBound(xs) - LBound(xs) + 1
End Function
Function Caller() As String
    Caller = Opt(1) & " " & Opt(1, 2, 3) & " " & Sum(1, 2, 3.5) & " " & Count()
End Function"#;
    assert_eq!(ok(src, "Caller", &[]), s("11/True 3/False 6.5 0"));
    assert_eq!(err(src, "Opt", &[]), 449);
}

#[test]
fn arrays_and_bounds() {
    let src = r#"
Function F() As String
    Dim a(1 To 3) As Integer, b() As String, m(2, 3) As Double
    a(2) = 7
    ReDim b(0 To 1)
    b(1) = "x"
    F = a(2) & a(1) & b(1) & "|" & LBound(a) & UBound(a) & "|" & UBound(m, 2)
End Function
Function OutOfRange() As Long
    Dim a(1 To 3) As Long
    OutOfRange = a(4)
End Function
Function Ret() As Long()
    Dim a(1) As Long
    a(1) = 5
    Ret = a
End Function"#;
    assert_eq!(ok(src, "F", &[]), s("70x|13|3"));
    assert_eq!(err(src, "OutOfRange", &[]), 9);
    assert_eq!(
        ok(src, "Ret", &[]),
        Variant::Array(Box::new(SafeArray {
            elem: VarType::I4,
            bounds: vec![(0, 2)],
            data: vec![Variant::I4(0), Variant::I4(5)],
        }))
    );
}

#[test]
fn redim_preserve() {
    let src = r#"
Function F() As String
    Dim a() As Long, i As Long
    ReDim a(1 To 2)
    a(1) = 1: a(2) = 2
    ReDim Preserve a(1 To 4)
    a(4) = 4
    For i = 1 To 4: F = F & a(i): Next
    ReDim a(1 To 2)
    F = F & "|" & a(1)
End Function
Function WrongDims() As Long
    Dim a() As Long
    ReDim a(1, 1)
    ReDim Preserve a(2, 2)
End Function"#;
    assert_eq!(ok(src, "F", &[]), s("1204|0"));

    let fixed = "Function F()\n    Dim a(3) As Long\n    ReDim a(5)\nEnd Function";
    assert!(!Project::compile(&[module("M", fixed)], &ExcelNames)
        .errors()
        .is_empty());
    assert_eq!(err(src, "WrongDims", &[]), 9);
}

#[test]
fn with_blocks_reach_the_host() {
    struct Host(Vec<String>);
    impl ObjectHost for Host {
        fn global(&mut self) -> Result<ObjectId, VbaError> {
            Ok(ObjectId(1))
        }
        fn invoke(
            &mut self,
            obj: ObjectId,
            member: &str,
            kind: InvokeKind,
            args: &[Variant],
            _: &[(&str, Variant)],
        ) -> Result<Variant, VbaError> {
            self.0.push(format!("{}.{member} {kind:?} {args:?}", obj.0));
            match member.to_ascii_lowercase().as_str() {
                "application" => Ok(Variant::Dispatch(Some(ObjectId(2)))),
                "name" => Ok(Variant::Bstr("App".to_string())),
                _ => Ok(Variant::Empty),
            }
        }
    }
    let src = r#"
Function F() As String
    With Application
        F = .Name
        .Calculation = 3
    End With
End Function"#;
    let mut host = Host(Vec::new());
    let r = project(src).call("F", &[], &mut host).unwrap();
    assert_eq!(r, s("App"));
    assert_eq!(host.0.len(), 3, "{:?}", host.0);
    assert!(host.0[2].starts_with("2.Calculation Put"), "{:?}", host.0);
}

#[test]
fn objects_without_a_host() {
    let src = r#"
Function F() As String
    On Error Resume Next
    Application.Caller.Value2 = 1
    F = "err " & Err.Number
End Function
Function G() As Boolean
    Dim o As Object
    G = o Is Nothing
End Function
Function H() As Long
    Dim o As Object
    H = o.Value
End Function"#;
    assert_eq!(ok(src, "F", &[]), s("err 424"));
    assert_eq!(ok(src, "G", &[]), Variant::Bool(true));
    assert_eq!(err(src, "H", &[]), 91);
}

#[test]
fn enums_and_consts() {
    let src = r#"
Private Const BASE As Long = 100
Public Const NAME_ As String = "n" & "m"
Enum Colour
    Red
    Green = 5
    Blue
End Enum
Function F() As String
    Const LOCAL_ = BASE * 2
    F = Red & Green & Blue & Colour.Blue & NAME_ & LOCAL_ & vbCrLf
End Function"#;
    assert_eq!(ok(src, "F", &[]), s("0566nm200\r\n"));
}

#[test]
fn statics_and_module_state() {
    let src = r#"
Dim calls As Long
Function Counter() As Long
    Static n As Long
    n = n + 1
    calls = calls + 1
    Counter = n * 100 + calls
End Function
Static Function AllStatic() As Long
    Dim k As Long
    k = k + 1
    AllStatic = k
End Function"#;
    let p = project(src);
    assert_eq!(
        p.call("Counter", &[], &mut NoHost).unwrap(),
        Variant::I4(101)
    );
    assert_eq!(
        p.call("Counter", &[], &mut NoHost).unwrap(),
        Variant::I4(202)
    );
    assert_eq!(
        p.call("AllStatic", &[], &mut NoHost).unwrap(),
        Variant::I4(1)
    );
    assert_eq!(
        p.call("AllStatic", &[], &mut NoHost).unwrap(),
        Variant::I4(2)
    );
}

#[test]
fn modules_see_each_others_publics() {
    let a = "Public Total As Long\nPrivate Function Hidden() As Long\nHidden = 1\nEnd Function\nPublic Sub Add(n As Long)\nTotal = Total + n\nEnd Sub";
    let b = "Function F() As Long\nAdd 3\nF = Total + A.Total\nEnd Function";
    let p = Project::compile(&[module("A", a), module("B", b)], &ExcelNames);
    assert!(p.errors().is_empty(), "{:?}", p.errors());
    assert_eq!(p.call("F", &[], &mut NoHost).unwrap(), Variant::I4(6));
    assert_eq!(p.call("B.F", &[], &mut NoHost).unwrap(), Variant::I4(12));
    assert!(p.signature("A.Hidden").is_some_and(|s| !s.public));

    let c = "Function G() As Long\nA.Add 4\nG = A.Total\nEnd Function";
    let q = Project::compile(&[module("A", a), module("C", c)], &ExcelNames);
    assert!(q.errors().is_empty(), "{:?}", q.errors());
    assert_eq!(q.call("G", &[], &mut NoHost).unwrap(), Variant::I4(4));
    assert_eq!(p.call("Nope", &[], &mut NoHost).unwrap_err().number, 35);
}

#[test]
fn signatures() {
    let src = r#"
Function F(ByVal a As Long, b, Optional c As String = "x", ParamArray d() As Variant) As Double
    Application.Volatile
End Function
Private Sub S()
End Sub"#;
    let p = project(src);
    let f = p.signature("F").unwrap();
    assert!(f.volatile && f.public);
    assert_eq!(f.params.len(), 4);
    assert!(f.params[0].by_val && !f.params[0].takes_reference());
    assert!(f.params[1].takes_reference());
    assert!(f.params[2].optional);
    assert!(f.params[3].param_array);
    assert!(!p.signature("S").unwrap().public);
}

#[test]
fn typed_arithmetic() {
    let src = r#"
Function IntOverflow() As Integer
    Dim a As Integer
    a = 32767
    IntOverflow = a + 1
End Function
Function LongMix() As Variant
    Dim a As Integer, b As Long
    a = 2: b = 3
    LongMix = a * b
End Function
Function Bank() As Long
    Bank = CLng(2.5) + CLng(3.5)
End Function
Function IntDiv() As String
    IntDiv = (7 \ 2) & " " & (-7 Mod 3) & " " & (7.6 \ 2)
End Function
Function Cur() As Currency
    Dim c As Currency
    c = 1.23456
    Cur = c * 3
End Function
Function Cat() As String
    Cat = 1 & 2.5 & True & Null
End Function
Function VarNull() As Variant
    VarNull = Null + 1
End Function"#;
    assert_eq!(err(src, "IntOverflow", &[]), 6);
    assert_eq!(ok(src, "LongMix", &[]), Variant::I4(6));
    assert_eq!(ok(src, "Bank", &[]), Variant::I4(6));
    assert_eq!(ok(src, "IntDiv", &[]), s("3 -1 4"));

    assert_eq!(ok(src, "Cur", &[]), Variant::Cy(37038));
    assert_eq!(ok(src, "Cat", &[]), s("12.5True"));
    assert_eq!(ok(src, "VarNull", &[]), Variant::Null);
}

#[test]
fn strings_and_comparison() {
    let src = r#"
Function F() As String
    F = ("a" < "b") & ("B" < "a") & ("abc" Like "a*") & (1 = 1#) & ("10" > 9)
End Function"#;
    let text = "Option Compare Text\nFunction G() As Boolean\nG = \"ABC\" = \"abc\"\nEnd Function";
    assert_eq!(ok(src, "F", &[]), s("TrueTrueTrueTrueTrue"));
    assert_eq!(ok(text, "G", &[]), Variant::Bool(true));
}

#[test]
fn recursion_and_depth() {
    std::thread::Builder::new()
        .stack_size(8 << 20)
        .spawn(recursion)
        .unwrap()
        .join()
        .unwrap();
}

fn recursion() {
    let src = r#"
Function Fact(n As Long) As Double
    If n <= 1 Then Fact = 1 Else Fact = n * Fact(n - 1)
End Function
Function Forever(n As Long) As Long
    Forever = Forever(n + 1)
End Function"#;
    assert_eq!(
        ok(src, "Fact", &[Variant::I4(10)]),
        Variant::R8(3_628_800.0)
    );
    assert_eq!(err(src, "Forever", &[Variant::I4(0)]), 28);
}

#[test]
fn arguments_are_coerced_to_parameter_types() {
    let src = "Function F(x As Long) As Long\nF = x\nEnd Function";
    assert_eq!(ok(src, "F", &[s("41")]), Variant::I4(41));
    assert_eq!(ok(src, "F", &[Variant::R8(2.5)]), Variant::I4(2));
    assert_eq!(err(src, "F", &[s("abc")]), 13);
    assert_eq!(err(src, "F", &[Variant::I4(1), Variant::I4(2)]), 450);
}

#[test]
fn compile_errors_have_positions() {
    let p = Project::compile(
        &[module("M", "Function F()\n    F = (1 +\nEnd Function")],
        &ExcelNames,
    );
    let e = &p.errors()[0];
    assert_eq!((e.module.as_str(), e.line), ("M", 2), "{e}");
    let p = Project::compile(
        &[module(
            "M",
            "Function F()\n    F = Undeclared\nEnd Function\nGarbage here",
        )],
        &ExcelNames,
    );
    assert!(!p.errors().is_empty());
}

#[test]
fn explicit_requires_declarations() {
    let p = Project::compile(
        &[module(
            "M",
            "Option Explicit\nFunction F()\n    y = 1\n    F = y\nEnd Function",
        )],
        &ExcelNames,
    );
    let e = &p.errors()[0];
    assert_eq!((e.line, e.col), (3, 5), "{e}");

    assert_eq!(
        ok("Function F()\n    y = 1\n    F = y\nEnd Function", "F", &[]),
        Variant::I2(1)
    );
}

#[test]
fn unsupported_constructs_are_compile_errors() {
    for src in [
        "Function F()\n    Open \"x\" For Input As #1\nEnd Function",
        "Declare PtrSafe Function GetTickCount Lib \"kernel32\" () As Long",
    ] {
        let p = Project::compile(&[module("M", src)], &ExcelNames);
        assert!(!p.errors().is_empty(), "{src}");
    }
    let class = SourceModule {
        name: "C".to_string(),
        kind: ModuleKind::Class,
        source: String::new(),
    };
    let p = Project::compile(
        &[class, module("M", "Function F()\nF = 1\nEnd Function")],
        &ExcelNames,
    );

    assert!(p.errors().is_empty(), "{:?}", p.errors());
    assert!(p.signature("F").is_some());
}
