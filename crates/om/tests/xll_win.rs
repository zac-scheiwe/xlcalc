#![cfg(windows)]

use xlcalc_om::{flags, Application, Call};
use xlcalc_variant::{ObjectId, Variant};

fn call(app: &mut Application, o: ObjectId, name: &str, f: u16, args: &[Variant]) -> Variant {
    app.invoke(
        o,
        name,
        Call {
            flags: f,
            lcid: 0x0409,
        },
        args,
        &[],
    )
    .unwrap_or_else(|e| panic!("{name}: {e:?}"))
}

fn s(v: &str) -> Variant {
    Variant::Bstr(v.into())
}

#[test]
fn register_xll_and_call() {
    let exe = std::env::current_exe().unwrap();
    let dll = exe
        .parent()
        .unwrap()
        .parent()
        .unwrap()
        .join("xlc_testxll.dll");
    if !dll.exists() {
        eprintln!("skipped: xlc_testxll.dll is not built");
        return;
    }
    let mut app = Application::new();
    app.add_workbook(xlcalc::Workbook::new());
    let root = app.handle();
    let get = flags::GET | flags::METHOD;
    let sheet = match call(&mut app, root, "ActiveSheet", get, &[]) {
        Variant::Dispatch(Some(id)) => id,
        other => panic!("{other:?}"),
    };
    let a1 = match call(&mut app, sheet, "Range", get, &[s("A1")]) {
        Variant::Dispatch(Some(id)) => id,
        other => panic!("{other:?}"),
    };
    call(&mut app, a1, "Formula", flags::PUT, &[s("=XlcDbl(3)")]);
    assert_eq!(call(&mut app, a1, "Text", flags::GET, &[]), s("#NAME?"));
    let ok = call(
        &mut app,
        root,
        "RegisterXLL",
        get,
        &[s(&dll.to_string_lossy())],
    );
    assert_eq!(ok, Variant::Bool(true));
    assert_eq!(call(&mut app, a1, "Text", flags::GET, &[]), s("6"));
    assert_eq!(
        call(&mut app, root, "Run", get, &[s("XlcDbl"), Variant::R8(5.0)]),
        Variant::R8(10.0)
    );
    let missing = s(r"C:\no\such.xll");
    assert_eq!(
        call(&mut app, root, "RegisterXLL", get, &[missing]),
        Variant::Bool(false)
    );
}
