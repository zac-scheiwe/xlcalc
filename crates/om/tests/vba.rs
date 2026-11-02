use xlcalc::{Value, Workbook};
use xlcalc_om::{flags, Application, BookId, Call};
use xlcalc_variant::Variant;
use xlcalc_vba::{ModuleKind, SourceModule};

fn setup(src: &str) -> (Application<'static>, BookId) {
    let mut app = Application::new();
    let book = app.add_workbook(Workbook::new());
    let module = SourceModule {
        name: "Module1".into(),
        kind: ModuleKind::Standard,
        source: src.into(),
    };
    let errors = app.add_vba_module(book, module).unwrap();
    assert!(errors.is_empty(), "{errors:?}");
    (app, book)
}

fn range(app: &mut Application, a1: &str) -> xlcalc_variant::ObjectId {
    let h = app.handle();
    match app.invoke(h, "Range", Call::VB, &[Variant::Bstr(a1.into())], &[]) {
        Ok(Variant::Dispatch(Some(id))) => id,
        other => panic!("Range({a1}): {other:?}"),
    }
}

fn set(app: &mut Application, a1: &str, formula: &str) {
    let r = range(app, a1);
    app.invoke(
        r,
        "Formula",
        Call::PUT,
        &[Variant::Bstr(formula.into())],
        &[],
    )
    .unwrap();
    app.release(r);
}

fn value(app: &Application, book: BookId, a1: &str) -> String {
    let wb = app.engine(book).unwrap();
    let s = wb.sheet_at(0).id;
    format!("{:?}", wb.value(xlcalc::parse_cell_ref(wb, a1, s).unwrap()))
}

fn run(app: &mut Application, args: &[Variant]) -> Result<Variant, xlcalc_om::OmError> {
    let h = app.handle();
    let call = Call {
        flags: flags::METHOD,
        lcid: 0x0409,
    };
    app.invoke(h, "Run", call, args, &[])
}

#[test]
fn objects_from_a_cell() {
    let (mut app, book) = setup(
        "Function Cap() As Variant\n    Cap = Application.Caller.Address\nEnd Function\n\
         Function Rd() As Variant\n    Rd = Application.Caller.Worksheet.Range(\"A1\").Value2\nEnd Function\n\
         Function Wr() As Variant\n    On Error Resume Next\n    Range(\"Z1\").Value2 = 1\n    Wr = Err.Number\nEnd Function\n\
         Function Wf() As Variant\n    On Error Resume Next\n    Wf = Application.WorksheetFunction.Match(2, Array(1, 2, 3), 0)\n    If Err.Number <> 0 Then Wf = Err.Number\nEnd Function\n",
    );
    set(&mut app, "A1", "7");
    set(&mut app, "B1", "=Cap()");
    set(&mut app, "B2", "=Rd()");
    set(&mut app, "B3", "=Wr()");
    set(&mut app, "B4", "=Wf()");
    assert_eq!(
        value(&app, book, "B1"),
        format!("{:?}", Value::Text("$B$1".into()))
    );
    assert_eq!(value(&app, book, "B2"), format!("{:?}", Value::Number(7.0)));
    assert_eq!(
        value(&app, book, "B3"),
        format!("{:?}", Value::Number(1004.0))
    );
    assert_eq!(value(&app, book, "B4"), format!("{:?}", Value::Number(2.0)));
    assert_eq!(value(&app, book, "Z1"), format!("{:?}", Value::Blank));
}

#[test]
fn run_finds_defined_names_first() {
    let (mut app, book) = setup(
        "Function MyF(x) As Double: MyF = x * 100: End Function
         Function MyG(x) As Double: MyG = x * 100: End Function
         Function Twice(x As Long) As Long: Twice = x * 2: End Function
",
    );
    let h = app.handle();
    let names = match app.invoke(h, "Names", Call::VB, &[], &[]) {
        Ok(Variant::Dispatch(Some(id))) => id,
        other => panic!("{other:?}"),
    };
    for (n, r) in [("MyF", "=LAMBDA(x,x+1)"), ("MyG", "=5")] {
        let args = [Variant::Bstr(n.into()), Variant::Bstr(r.into())];
        app.invoke(names, "Add", Call::VB, &args, &[]).unwrap();
    }
    let e = run(&mut app, &[Variant::Bstr("MyF".into()), Variant::I4(2)]).unwrap_err();
    assert_eq!(e.number, 1004);
    let v = run(&mut app, &[Variant::Bstr("MyG".into()), Variant::I4(2)]).unwrap();
    assert_eq!(v, Variant::Error(0x800A_07DF_u32 as i32));
    let v = run(&mut app, &[Variant::Bstr("Twice".into()), Variant::I4(21)]).unwrap();
    assert_eq!(v, Variant::I4(42));
    let e = run(&mut app, &[Variant::Bstr("Nope".into())]).unwrap_err();
    assert_eq!(e.number, 1004);

    set(&mut app, "A1", "=Twice(4)");
    assert_eq!(value(&app, book, "A1"), format!("{:?}", Value::Number(8.0)));
}

#[test]
fn deep_recursion_from_a_cell() {
    let (mut app, book) = setup(
        "Function Depth(n As Long) As Long
    If n <= 1 Then Depth = 1 Else Depth = 1 + Depth(n - 1)
End Function
",
    );

    set(&mut app, "A1", "=Depth(190)");
    set(&mut app, "A2", "=Depth(5000)");
    assert_eq!(
        value(&app, book, "A1"),
        format!("{:?}", Value::Number(190.0))
    );
    assert_eq!(
        value(&app, book, "A2"),
        format!("{:?}", Value::Error(xlcalc::XlError::VALUE))
    );

    std::thread::Builder::new()
        .stack_size(256 * 1024)
        .spawn(move || {
            set(&mut app, "A3", "=Depth(195)");
            assert_eq!(
                value(&app, book, "A3"),
                format!("{:?}", Value::Number(195.0))
            );
        })
        .unwrap()
        .join()
        .unwrap();
}

#[test]
fn volatile_and_stale_reads() {
    let (mut app, book) = setup(
        "Public Calls As Long
         Function ReadA1() As Variant
    Calls = Calls + 1
    ReadA1 = Range(\"A1\").Value2
End Function
         Function ReadA1Vol() As Variant
    Application.Volatile
    ReadA1Vol = Range(\"A1\").Value2
End Function
",
    );
    set(&mut app, "A1", "1");
    set(&mut app, "B1", "=ReadA1()");
    set(&mut app, "B2", "=ReadA1Vol()");
    set(&mut app, "A1", "2");
    assert_eq!(value(&app, book, "B1"), format!("{:?}", Value::Number(1.0)));
    assert_eq!(value(&app, book, "B2"), format!("{:?}", Value::Number(2.0)));
}

fn open(app: &mut Application, file: &str) -> BookId {
    let path = std::path::Path::new(env!("CARGO_MANIFEST_DIR"))
        .join("../vbaproject/tests/data")
        .join(file);
    let h = app.handle();
    let books = match app.invoke(h, "Workbooks", Call::VB, &[], &[]) {
        Ok(Variant::Dispatch(Some(id))) => id,
        other => panic!("{other:?}"),
    };
    let arg = [Variant::Bstr(path.to_string_lossy().into())];
    app.invoke(books, "Open", Call::VB, &arg, &[])
        .unwrap_or_else(|e| panic!("open {file}: {e}"));
    app.active_workbook().unwrap()
}

#[test]
fn open_compiles_the_project() {
    let mut app = Application::new();
    let book = open(&mut app, "basic.xlsm");
    let host = app.vba_host(book).expect("a project");
    assert!(host.function_names().contains(&"ADD2".to_string()));

    assert!(!host.function_names().contains(&"SHEETFN".to_string()));

    let w = app.import_warnings(book);
    assert!(w.is_empty(), "{w:?}");

    let args = [
        Variant::Bstr("Add2".into()),
        Variant::R8(1.5),
        Variant::I4(2),
    ];
    assert_eq!(run(&mut app, &args).unwrap(), Variant::R8(3.5));

    let args = [Variant::Bstr("Sheet1.SheetFn".into())];

    assert_eq!(run(&mut app, &args).unwrap(), Variant::Empty);
    assert!(run(&mut app, &[Variant::Bstr("SheetFn".into())]).is_err());
}

#[test]
fn open_warns_about_a_module_it_cannot_read() {
    let mut app = Application::new();
    let book = open(&mut app, "noise.xlsm");
    let w = app.import_warnings(book);
    assert!(
        w.iter()
            .any(|l| l.contains("BigNoise") && l.contains("#NAME?")),
        "{w:?}"
    );
}
