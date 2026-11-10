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

fn obj(v: Variant) -> ObjectId {
    match v {
        Variant::Dispatch(Some(id)) => id,
        other => panic!("not an object: {other:?}"),
    }
}

fn s(v: &str) -> Variant {
    Variant::Bstr(v.into())
}

#[test]
fn rtd_follows_excel() {
    if xlcalc_winbridge::create("Xlc.TestRtd").is_err() {
        eprintln!("skipped: Xlc.TestRtd is not registered");
        return;
    }
    let mut app = Application::new();
    app.add_workbook(xlcalc::Workbook::new());
    let root = app.handle();
    let get = flags::GET | flags::METHOD;
    let sheet = obj(call(&mut app, root, "ActiveSheet", get, &[]));
    let cell = |app: &mut Application, a: &str| obj(call(app, sheet, "Range", get, &[s(a)]));
    let text = |app: &mut Application, a: &str, f: &str| {
        let c = cell(app, a);
        call(app, c, "Formula", flags::PUT, &[s(f)]);
        call(app, c, "Text", flags::GET, &[])
    };

    assert_eq!(
        text(&mut app, "A9", "=RTD(\"Xlc.TestRtd\",,\"const\",\"x\")"),
        s("#N/A")
    );
    app.enable_rtd();
    assert_eq!(
        text(&mut app, "A1", "=RTD(\"Xlc.TestRtd\",,\"const\",\"x\")"),
        s("c:x")
    );
    assert_eq!(
        text(
            &mut app,
            "A2",
            "=RTD(\"Xlc.TestRtd\",,\"echo\",\"p\",\"q\")"
        ),
        s("echo|p|q")
    );
    assert_eq!(
        text(&mut app, "A3", "=RTD(\"Xlc.TestRtd\",,\"err\")"),
        s("#N/A")
    );
    assert_eq!(
        text(&mut app, "A4", "=RTD(\"No.Such.Server\",,\"a\")"),
        s("#N/A")
    );
    assert_eq!(text(&mut app, "A5", "=RTD(1,,\"a\")"), s("#N/A"));
    assert_eq!(
        text(&mut app, "A6", "=RTD(\"Xlc.TestRtd\",,\"count\")"),
        s("0")
    );
    assert!(app.refresh_rtd());
    let c = cell(&mut app, "A6");
    assert_eq!(call(&mut app, c, "Text", flags::GET, &[]), s("1"));
    app.stop_rtd();
}

#[test]
fn xll_rtd_follows_excel() {
    let dll = std::env::current_exe()
        .ok()
        .and_then(|e| Some(e.parent()?.parent()?.join("xlc_testxll.dll")))
        .filter(|p| p.exists());
    let Some(dll) = dll else {
        eprintln!("skipped: xlc_testxll.dll is not built");
        return;
    };
    if xlcalc_winbridge::create("Xlc.TestRtd").is_err() {
        eprintln!("skipped: Xlc.TestRtd is not registered");
        return;
    }
    let mut app = Application::new();
    app.add_workbook(xlcalc::Workbook::new());
    let root = app.handle();
    let get = flags::GET | flags::METHOD;
    call(
        &mut app,
        root,
        "RegisterXLL",
        flags::METHOD,
        &[s(dll.to_string_lossy().as_ref())],
    );
    app.enable_rtd();
    let sheet = obj(call(&mut app, root, "ActiveSheet", get, &[]));
    let cell = obj(call(&mut app, sheet, "Range", get, &[s("A1")]));
    let golden = include_str!("../../../conformance/com/xll_probe9.golden.txt");
    let mut bad = Vec::new();
    let mut n = 0;
    for line in golden.lines() {
        let Some((head, want)) = line.split_once(" -> [") else {
            continue;
        };
        let Some(formula) = head.strip_suffix(" now") else {
            continue;
        };
        n += 1;
        call(&mut app, cell, "Formula", flags::PUT, &[s(formula)]);
        let got = call(&mut app, cell, "Text", flags::GET, &[]);
        if got != s(want.trim_end_matches(']')) {
            bad.push(format!("{formula}: got {got:?}, Excel [{want}"));
        }
    }
    assert!(n >= 10, "the golden has its lines ({n})");
    let counter = obj(call(&mut app, sheet, "Range", get, &[s("B1")]));
    call(
        &mut app,
        counter,
        "Formula",
        flags::PUT,
        &[s("=XlcRtdValue(\"Xlc.TestRtd\",,\"count\")")],
    );
    for want in ["0", "1", "2"] {
        let got = call(&mut app, counter, "Text", flags::GET, &[]);
        if got != s(want) {
            bad.push(format!("count: got {got:?}, Excel [{want}]"));
        }
        app.refresh_rtd();
    }
    app.stop_rtd();
    assert!(bad.is_empty(), "{}", bad.join("\n"));
}
