#![cfg(windows)]

use xlcalc_om::{
    flags, Application, AutomationServer, Call, ComBridge, ExtCall, RegisteredAddin, WindowsBridge,
};
use xlcalc_variant::{ObjectId, Variant};
use xlcalc_vba::{InvokeKind, VbaError};

static SERIAL: std::sync::Mutex<()> = std::sync::Mutex::new(());

struct Injected(WindowsBridge);

impl ComBridge for Injected {
    fn registered_addins(&mut self) -> Vec<RegisteredAddin> {
        Vec::new()
    }
    fn create(&mut self, p: &str) -> Result<u64, VbaError> {
        self.0.create(p)
    }
    fn invoke(
        &mut self,
        h: u64,
        m: &str,
        k: InvokeKind,
        a: &[Variant],
    ) -> Result<Variant, VbaError> {
        self.0.invoke(h, m, k, a)
    }
    fn extensibility(&mut self, h: u64, c: ExtCall) -> Result<bool, VbaError> {
        self.0.extensibility(h, c)
    }
    fn release(&mut self, h: u64) {
        self.0.release(h)
    }
    fn automation_servers(&mut self) -> Vec<AutomationServer> {
        vec![AutomationServer {
            prog_id: "Xlc.TestFuncs".into(),
            file: "mscoree.dll".into(),
            title: "XlcTest.TestFuncs".into(),
        }]
    }
    fn has_member(&mut self, h: u64, n: &str) -> bool {
        self.0.has_member(h, n)
    }
    fn call_function(
        &mut self,
        h: u64,
        n: &str,
        a: &[Variant],
        cb: &mut dyn xlcalc_om::ObjectCallback,
    ) -> Result<Variant, xlcalc_om::CallFailure> {
        self.0.call_function(h, n, a, cb)
    }
}

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

fn text(app: &mut Application, formula: &str) -> String {
    let root = app.handle();
    let sheet = obj(call(
        app,
        root,
        "ActiveSheet",
        flags::GET | flags::METHOD,
        &[],
    ));
    let a1 = obj(call(
        app,
        sheet,
        "Range",
        flags::GET | flags::METHOD,
        &[Variant::Bstr("A1".into())],
    ));
    call(
        app,
        a1,
        "Formula",
        flags::PUT,
        &[Variant::Bstr(formula.into())],
    );
    match call(app, a1, "Text", flags::GET, &[]) {
        Variant::Bstr(s) => s.to_string(),
        other => panic!("{other:?}"),
    }
}

#[test]
fn automation_functions_follow_excel() {
    let _serial = SERIAL
        .lock()
        .unwrap_or_else(std::sync::PoisonError::into_inner);
    if xlcalc_winbridge::create("Xlc.TestFuncs").is_err() {
        eprintln!("skipped: Xlc.TestFuncs is not registered");
        return;
    }
    let mut app = Application::new();
    app.set_com_bridge(Box::new(Injected(WindowsBridge::new())));
    app.add_workbook(xlcalc::Workbook::new());
    let root = app.handle();
    assert_eq!(text(&mut app, "=Twice(4)"), "#NAME?");
    let list = obj(call(
        &mut app,
        root,
        "AddIns",
        flags::GET | flags::METHOD,
        &[],
    ));
    let Variant::I4(count) = call(&mut app, list, "Count", flags::GET, &[]) else {
        panic!("Count");
    };
    let mut found = None;
    for i in 1..=count {
        let item = obj(call(
            &mut app,
            list,
            "Item",
            flags::GET | flags::METHOD,
            &[Variant::I4(i)],
        ));
        if call(&mut app, item, "Title", flags::GET, &[])
            == Variant::Bstr("XlcTest.TestFuncs".into())
        {
            found = Some(item);
        }
    }
    let found = found.expect("the automation add-in is listed");
    assert_eq!(
        call(&mut app, found, "Name", flags::GET, &[]),
        Variant::Bstr("mscoree.dll".into())
    );
    call(
        &mut app,
        found,
        "Installed",
        flags::PUT,
        &[Variant::Bool(true)],
    );
    for (formula, shown) in [
        ("=Twice(4)", "8"),
        ("=twice(4)", "8"),
        ("=Greet(\"a\")", "Hi a"),
        ("=Greet(1)", "Hi 1"),
        ("=Plus(1,2)", "3"),
        ("=Plus(\"3\",2)", "5"),
        ("=Plus(TRUE,2)", "3"),
        ("=Kind(1)", "Double"),
        ("=Kind(\"s\")", "String"),
        ("=Kind(TRUE)", "Boolean"),
        ("=Kind(1/0)", "Int32"),
        ("=Err()", "#DIV/0!"),
        ("=Boom()", "#VALUE!"),
        ("=Whole(2.5)", "3"),
        ("=Whole(1E+20)", "#NUM!"),
        ("=Flag(1)", "FALSE"),
        ("=Flag(\"x\")", "#VALUE!"),
        ("=Twice(\"x\")", "#VALUE!"),
        ("=Twice()", "#VALUE!"),
        ("=Twice(1,2)", "#VALUE!"),
        ("=Twice(TRUE)", "-2"),
        ("=SUM(Twice(2),1)", "5"),
        ("=Opt()", "missing"),
        ("=Opt(1)", "given"),
    ] {
        assert_eq!(text(&mut app, formula), shown, "{formula}");
    }
}

fn put(app: &mut Application, cell: &str, prop: &str, value: Variant) {
    let root = app.handle();
    let sheet = obj(call(
        app,
        root,
        "ActiveSheet",
        flags::GET | flags::METHOD,
        &[],
    ));
    let r = obj(call(
        app,
        sheet,
        "Range",
        flags::GET | flags::METHOD,
        &[Variant::Bstr(cell.into())],
    ));
    call(app, r, prop, flags::PUT, &[value]);
}

fn read(app: &mut Application, cell: &str) -> String {
    let root = app.handle();
    let sheet = obj(call(
        app,
        root,
        "ActiveSheet",
        flags::GET | flags::METHOD,
        &[],
    ));
    let r = obj(call(
        app,
        sheet,
        "Range",
        flags::GET | flags::METHOD,
        &[Variant::Bstr(cell.into())],
    ));
    match call(app, r, "Text", flags::GET, &[]) {
        Variant::Bstr(s) => s.to_string(),
        other => panic!("{other:?}"),
    }
}

#[test]
fn automation_ranges_and_spills_follow_excel() {
    let _serial = SERIAL
        .lock()
        .unwrap_or_else(std::sync::PoisonError::into_inner);
    if xlcalc_winbridge::create("Xlc.TestFuncs").is_err() {
        eprintln!("skipped: Xlc.TestFuncs is not registered");
        return;
    }
    let mut app = Application::new();
    app.set_com_bridge(Box::new(Injected(WindowsBridge::new())));
    app.add_workbook(xlcalc::Workbook::new());
    let root = app.handle();
    let list = obj(call(
        &mut app,
        root,
        "AddIns",
        flags::GET | flags::METHOD,
        &[],
    ));
    let Variant::I4(count) = call(&mut app, list, "Count", flags::GET, &[]) else {
        panic!("Count");
    };
    for i in 1..=count {
        let item = obj(call(
            &mut app,
            list,
            "Item",
            flags::GET | flags::METHOD,
            &[Variant::I4(i)],
        ));
        if call(&mut app, item, "Title", flags::GET, &[])
            == Variant::Bstr("XlcTest.TestFuncs".into())
        {
            call(
                &mut app,
                item,
                "Installed",
                flags::PUT,
                &[Variant::Bool(true)],
            );
        }
    }
    put(&mut app, "B1", "Value2", Variant::R8(1.5));
    put(&mut app, "B2", "Value2", Variant::Bstr("txt".into()));
    put(&mut app, "B3", "Value2", Variant::Bool(true));
    put(&mut app, "C1", "Formula", Variant::Bstr("=B1*2".into()));
    put(&mut app, "C2", "Value2", Variant::R8(7.0));
    put(&mut app, "C3", "Formula", Variant::Bstr("=1/0".into()));
    let mut bad = Vec::new();
    for (formula, shown) in [
        ("=Prop(B1,\"Address\")", "$B$1"),
        ("=Prop(B1,\"Row\")", "1"),
        ("=Prop(B1,\"Column\")", "2"),
        ("=Prop(B1,\"Count\")", "1"),
        ("=Prop(B1,\"Value2\")", "1.5"),
        ("=Prop(B1,\"Formula\")", "1.5"),
        ("=Prop(B1,\"HasFormula\")", "False"),
        ("=Prop(B1:C3,\"Address\")", "$B$1:$C$3"),
        ("=Prop(B1:C3,\"Count\")", "6"),
        ("=Prop(B1:C3,\"Value2\")", "arr3x2"),
        ("=Prop((B1,C2),\"Address\")", "$B$1,$C$2"),
        ("=Prop(C1,\"Value2\")", "3"),
        ("=Prop(C3,\"Value2\")", "-2146826281"),
        ("=Prop(B2,\"Value2\")", "txt"),
        ("=Prop(A9,\"Value2\")", "null"),
        ("=Prop(A:A,\"Address\")", "$A:$A"),
        ("=Prop(1:1,\"Address\")", "$1:$1"),
        ("=Prop(A:A,\"Row\")", "1"),
        ("=Prop(A1:A9,\"Count\")", "9"),
        ("=Kind(A:A)", "__ComObject"),
        ("=Prop(5,\"Address\")", "ERR MissingMethodException"),
        ("=Prop(B1+0,\"Address\")", "ERR MissingMethodException"),
        ("=Prop(INDEX(B1:C3,2,1),\"Address\")", "$B$2"),
        ("=Prop(OFFSET(B1,1,1),\"Address\")", "$C$2"),
        ("=Put(D1,\"Value2\",5)", "ERR COMException"),
        ("=Self(B1)", "1.5"),
        ("=Self(B2)", "txt"),
        ("=Self(5)", "5"),
        ("=Twice(B1)", "3"),
        ("=Twice(C1)", "6"),
        ("=Twice(B1:B1)", "3"),
        ("=Twice(B1:C1)", "#VALUE!"),
        ("=Same(B1)", "1.5"),
        ("=Same(B2)", "txt"),
        ("=Kind(B1)", "__ComObject"),
        ("=SUM(Self(B1:C1))", "4.5"),
    ] {
        put(&mut app, "Z50", "Formula", Variant::Bstr(formula.into()));
        let got = read(&mut app, "Z50");
        if got != shown {
            bad.push(format!("{formula}: got {got:?}, want {shown:?}"));
        }
    }
    assert!(bad.is_empty(), "{bad:#?}");
    assert_eq!(read(&mut app, "D1"), "");

    put(&mut app, "A5", "Formula2", Variant::Bstr("=Arr()".into()));
    let got: Vec<String> = ["A5", "B5", "C5", "A6", "B6", "A7"]
        .iter()
        .map(|c| read(&mut app, c))
        .collect();
    assert_eq!(got, ["1", "2", "", "3", "4", ""]);
    put(
        &mut app,
        "E1",
        "Formula2",
        Variant::Bstr("=Self(B1:C3)".into()),
    );
    assert_eq!(read(&mut app, "E1"), "1.5");
    assert_eq!(read(&mut app, "F3"), "#DIV/0!");
    put(&mut app, "H1", "Formula2", Variant::Bstr("=Wide()".into()));
    assert_eq!(read(&mut app, "J1"), "3");
    put(&mut app, "H3", "Formula2", Variant::Bstr("=Col()".into()));
    assert_eq!(read(&mut app, "H5"), "3");
    put(&mut app, "A20", "Formula", Variant::Bstr("=Arr()".into()));
    assert_eq!(read(&mut app, "A20"), "1");
    assert_eq!(read(&mut app, "B20"), "");
    put(&mut app, "A22", "Formula", Variant::Bstr("=Grid()".into()));
    assert_eq!(read(&mut app, "A22"), "#VALUE!");
}

fn cell(app: &mut Application, a1: &str) -> ObjectId {
    let root = app.handle();
    let sheet = obj(call(
        app,
        root,
        "ActiveSheet",
        flags::GET | flags::METHOD,
        &[],
    ));
    obj(call(
        app,
        sheet,
        "Range",
        flags::GET | flags::METHOD,
        &[Variant::Bstr(a1.into())],
    ))
}

fn prop_text(app: &mut Application, a1: &str, name: &str, args: &[Variant]) -> String {
    let r = cell(app, a1);
    match call(app, r, name, flags::GET | flags::METHOD, args) {
        Variant::Bstr(s) => s.to_string(),
        Variant::Bool(b) => if b { "True" } else { "False" }.to_string(),
        other => panic!("{name}: {other:?}"),
    }
}

#[test]
fn automation_probe4_follows_excel() {
    let _serial = SERIAL
        .lock()
        .unwrap_or_else(std::sync::PoisonError::into_inner);
    if xlcalc_winbridge::create("Xlc.TestFuncs").is_err() {
        eprintln!("skipped: Xlc.TestFuncs is not registered");
        return;
    }
    let mut app = Application::new();
    app.set_com_bridge(Box::new(Injected(WindowsBridge::new())));
    app.add_workbook(xlcalc::Workbook::new());
    let root = app.handle();
    let list = obj(call(
        &mut app,
        root,
        "AddIns",
        flags::GET | flags::METHOD,
        &[],
    ));
    let Variant::I4(count) = call(&mut app, list, "Count", flags::GET, &[]) else {
        panic!("Count");
    };
    for i in 1..=count {
        let item = obj(call(
            &mut app,
            list,
            "Item",
            flags::GET | flags::METHOD,
            &[Variant::I4(i)],
        ));
        if call(&mut app, item, "Title", flags::GET, &[])
            == Variant::Bstr("XlcTest.TestFuncs".into())
        {
            call(
                &mut app,
                item,
                "Installed",
                flags::PUT,
                &[Variant::Bool(true)],
            );
        }
    }
    put(&mut app, "B1", "Value2", Variant::R8(1.5));
    put(&mut app, "B2", "Value2", Variant::Bstr("txt".into()));
    put(&mut app, "B3", "Value2", Variant::Bool(true));
    put(&mut app, "C1", "Formula", Variant::Bstr("=B1*2".into()));
    put(&mut app, "C2", "Value2", Variant::R8(7.0));
    put(&mut app, "C3", "Formula", Variant::Bstr("=1/0".into()));
    let golden = [
        include_str!("../../../conformance/com/autoaddin_probe4.golden.txt"),
        include_str!("../../../conformance/com/autoaddin_probe5.golden.txt"),
    ]
    .concat();
    let mut bad = Vec::new();
    for line in golden.lines() {
        let Some((head, want)) = line.split_once(" -> ") else {
            continue;
        };
        let mut parts = head.splitn(3, ' ');
        let (kind, at, formula) = (
            parts.next().unwrap_or(""),
            parts.next().unwrap_or(""),
            parts.next().unwrap_or(""),
        );
        let prop = match kind {
            "L" => "Formula",
            "S" => "Formula2",
            _ => continue,
        };
        put(&mut app, at, prop, Variant::Bstr(formula.into()));
        let spill = if prop_text(&mut app, at, "HasSpill", &[]) == "True" {
            let r = cell(&mut app, at);
            let sp = obj(call(&mut app, r, "SpillingToRange", flags::GET, &[]));
            match call(
                &mut app,
                sp,
                "Address",
                flags::GET | flags::METHOD,
                &[Variant::Bool(false), Variant::Bool(false)],
            ) {
                Variant::Bstr(s) => s.to_string(),
                other => panic!("{other:?}"),
            }
        } else {
            "-".to_string()
        };
        let got = format!(
            "[{}] hasSpill={} spill={} f=[{}] f2=[{}]",
            prop_text(&mut app, at, "Text", &[]),
            prop_text(&mut app, at, "HasSpill", &[]),
            spill,
            prop_text(&mut app, at, "Formula", &[]),
            prop_text(&mut app, at, "Formula2", &[]),
        );
        if got != want {
            bad.push(format!("{line}\n    got {got}"));
        }
        let r = cell(&mut app, at);
        call(&mut app, r, "Clear", flags::GET | flags::METHOD, &[]);
    }
    assert!(bad.is_empty(), "{} differ:\n{}", bad.len(), bad.join("\n"));
}

#[test]
fn automation_type_names_follow_excel() {
    let _serial = SERIAL
        .lock()
        .unwrap_or_else(std::sync::PoisonError::into_inner);
    if xlcalc_winbridge::create("Xlc.TestFuncs").is_err() {
        eprintln!("skipped: Xlc.TestFuncs is not registered");
        return;
    }
    let mut app = Application::new();
    app.set_com_bridge(Box::new(Injected(WindowsBridge::new())));
    app.add_workbook(xlcalc::Workbook::new());
    let root = app.handle();
    let list = obj(call(
        &mut app,
        root,
        "AddIns",
        flags::GET | flags::METHOD,
        &[],
    ));
    let Variant::I4(count) = call(&mut app, list, "Count", flags::GET, &[]) else {
        panic!("Count");
    };
    for i in 1..=count {
        let item = obj(call(
            &mut app,
            list,
            "Item",
            flags::GET | flags::METHOD,
            &[Variant::I4(i)],
        ));
        if call(&mut app, item, "Title", flags::GET, &[])
            == Variant::Bstr("XlcTest.TestFuncs".into())
        {
            call(
                &mut app,
                item,
                "Installed",
                flags::PUT,
                &[Variant::Bool(true)],
            );
        }
    }
    put(&mut app, "B1", "Value2", Variant::R8(1.5));
    put(&mut app, "B2", "Value2", Variant::Bstr("txt".into()));
    put(&mut app, "C1", "Formula", Variant::Bstr("=B1*2".into()));
    put(
        &mut app,
        "C2",
        "Formula",
        Variant::Bstr("=SUM(B1:B2,C1)".into()),
    );
    put(&mut app, "D2", "Formula", Variant::Bstr("=C2+B1".into()));
    let golden = [
        include_str!("../../../conformance/com/autoaddin_probe6.golden.txt"),
        include_str!("../../../conformance/com/autoaddin_probe7.golden.txt"),
    ]
    .concat();
    let mut bad = Vec::new();
    for line in golden.lines() {
        let Some((formula, want)) = line.split_once(" -> [") else {
            continue;
        };
        let want = want.trim_end_matches(']');
        put(&mut app, "A1", "Formula", Variant::Bstr(formula.into()));
        let got = prop_text(&mut app, "A1", "Text", &[]);
        if got != want {
            bad.push(format!("{formula}: got [{got}], Excel [{want}]"));
        }
    }
    assert!(
        bad.is_empty(),
        "{} differ:
{}",
        bad.len(),
        bad.join(
            "
"
        )
    );
}

fn show(app: &mut Application, v: &Variant) -> String {
    match v {
        Variant::Empty | Variant::Null => "null".into(),
        Variant::R8(n) => format!("Double:{}", g15(*n)),
        Variant::I4(n) | Variant::Error(n) => format!("Int32:{n}"),
        Variant::Bool(b) => format!("Boolean:{}", if *b { "True" } else { "False" }),
        Variant::Bstr(s) => format!("String:{s}"),
        Variant::Dispatch(Some(id)) => {
            let addr = call(
                app,
                *id,
                "Address",
                flags::GET | flags::METHOD,
                &[Variant::Bool(false), Variant::Bool(false)],
            );
            let ws = obj(call(app, *id, "Worksheet", flags::GET, &[]));
            let name = call(app, ws, "Name", flags::GET, &[]);
            let (Variant::Bstr(addr), Variant::Bstr(name)) = (addr, name) else {
                panic!("address");
            };
            format!("com:Range {addr} sheet={name}")
        }
        Variant::Array(a) => {
            let rank = a.bounds.len();
            let (r, c) = match a.bounds.as_slice() {
                [(_, n)] => (*n, 1),
                [(_, r), (_, c)] => (*r, *c),
                _ => panic!("rank"),
            };
            let mut parts = Vec::new();
            for i in 0..r as usize {
                for j in 0..c as usize {
                    let e = a.data[j * r as usize + i].clone();
                    parts.push(show(app, &e));
                }
            }
            format!(
                "arr{rank}d {r}x{c} lb={} {{{}}}",
                a.bounds[0].0,
                parts.join(";")
            )
        }
        other => format!("{other:?}"),
    }
}

fn g15(n: f64) -> String {
    if n == n.trunc() && n.abs() < 1e15 {
        return format!("{}", n as i64);
    }
    let s = format!("{n:.14e}");
    let (m, e) = s.split_once('e').unwrap_or((&s, "0"));
    let e: i32 = e.parse().unwrap_or(0);
    let digits: String = m.replace('.', "").trim_start_matches('-').to_string();
    let digits = digits.trim_end_matches('0');
    let neg = if n < 0.0 { "-" } else { "" };
    if (-5..15).contains(&e) {
        if e >= 0 {
            let int = e as usize + 1;
            let padded = format!("{digits:0<int$}");
            let (a, b) = padded.split_at(int);
            if b.is_empty() {
                format!("{neg}{a}")
            } else {
                format!("{neg}{a}.{b}")
            }
        } else {
            format!("{neg}0.{}{digits}", "0".repeat((-e - 1) as usize))
        }
    } else {
        format!("{neg}{digits}E{e:+}")
    }
}

#[test]
fn evaluate_follows_excel() {
    let _serial = SERIAL
        .lock()
        .unwrap_or_else(std::sync::PoisonError::into_inner);
    if xlcalc_winbridge::create("Xlc.TestFuncs").is_err() {
        eprintln!("skipped: Xlc.TestFuncs is not registered");
        return;
    }
    let mut app = Application::new();
    app.set_com_bridge(Box::new(Injected(WindowsBridge::new())));
    app.add_workbook(xlcalc::Workbook::new());
    let root = app.handle();
    let list = obj(call(
        &mut app,
        root,
        "AddIns",
        flags::GET | flags::METHOD,
        &[],
    ));
    let Variant::I4(count) = call(&mut app, list, "Count", flags::GET, &[]) else {
        panic!("Count");
    };
    for i in 1..=count {
        let item = obj(call(
            &mut app,
            list,
            "Item",
            flags::GET | flags::METHOD,
            &[Variant::I4(i)],
        ));
        if call(&mut app, item, "Title", flags::GET, &[])
            == Variant::Bstr("XlcTest.TestFuncs".into())
        {
            call(
                &mut app,
                item,
                "Installed",
                flags::PUT,
                &[Variant::Bool(true)],
            );
        }
    }
    let book = obj(call(&mut app, root, "ActiveWorkbook", flags::GET, &[]));
    let sheets = obj(call(&mut app, book, "Worksheets", flags::GET, &[]));
    let ws1 = obj(call(
        &mut app,
        sheets,
        "Item",
        flags::GET | flags::METHOD,
        &[Variant::I4(1)],
    ));
    let ws2 = obj(call(
        &mut app,
        sheets,
        "Add",
        flags::METHOD,
        &[
            Variant::Error(0x80020004u32 as i32),
            Variant::Dispatch(Some(ws1)),
        ],
    ));
    call(
        &mut app,
        ws2,
        "Name",
        flags::PUT,
        &[Variant::Bstr("Other".into())],
    );
    call(&mut app, ws1, "Activate", flags::METHOD, &[]);
    put(&mut app, "B1", "Value2", Variant::R8(1.5));
    put(&mut app, "B2", "Value2", Variant::Bstr("txt".into()));
    put(&mut app, "B3", "Value2", Variant::Bool(true));
    put(&mut app, "C1", "Formula", Variant::Bstr("=B1*2".into()));
    put(&mut app, "C2", "Value2", Variant::R8(7.0));
    put(&mut app, "C3", "Formula", Variant::Bstr("=1/0".into()));
    for (a1, v) in [("A1", 9.0), ("B2", 5.0)] {
        let r = obj(call(
            &mut app,
            ws2,
            "Range",
            flags::GET | flags::METHOD,
            &[Variant::Bstr(a1.into())],
        ));
        call(&mut app, r, "Value2", flags::PUT, &[Variant::R8(v)]);
    }
    for (name, refers) in [("nm", "=Sheet1!$B$1:$C$2"), ("k", "=42")] {
        let names = obj(call(&mut app, ws1, "Names", flags::GET, &[]));
        call(
            &mut app,
            names,
            "Add",
            flags::METHOD,
            &[Variant::Bstr(name.into()), Variant::Bstr(refers.into())],
        );
    }
    let d4 = cell(&mut app, "D4");
    call(&mut app, d4, "Select", flags::METHOD, &[]);
    let dll = std::env::current_exe()
        .ok()
        .and_then(|e| Some(e.parent()?.parent()?.join("xlc_testxll.dll")))
        .filter(|p| p.exists());
    let golden = include_str!("../../../conformance/com/evaluate_probe.golden.txt");
    let mut bad = Vec::new();
    let mut other_active = false;
    for line in golden.lines() {
        let Some((head, want)) = line.split_once(" => ") else {
            continue;
        };
        if want.starts_with("ERR ") {
            continue;
        }
        if line.starts_with("RegisterXLL") {
            if let Some(p) = &dll {
                call(
                    &mut app,
                    root,
                    "RegisterXLL",
                    flags::METHOD,
                    &[Variant::Bstr(p.to_string_lossy().as_ref().into())],
                );
            }
            continue;
        }
        if head.contains("Xlc") && dll.is_none() {
            continue;
        }
        let wants_other = head.contains("active=Other");
        if wants_other != other_active {
            let target = if wants_other { ws2 } else { ws1 };
            call(&mut app, target, "Activate", flags::METHOD, &[]);
            other_active = wants_other;
        }
        let Some(open) = head.find(" [") else {
            continue;
        };
        let who = &head[..open];
        let inner = &head[open + 2..];
        let text = inner.rsplit_once(']').map_or(inner, |x| x.0).to_string();
        let text = match text.as_str() {
            "255" => format!("{}1", "1+".repeat(100)),
            "256 chars" => format!("\"{}\"", "a".repeat(253)),
            "300 chars" => format!("\"{}\"", "a".repeat(298)),
            "missing" => continue,
            "number" => "5".to_string(),
            "empty" => String::new(),
            _ => text,
        };
        let (target, member) = match who {
            "App.Evaluate" => (root, "Evaluate"),
            "Ws1.Evaluate" => (ws1, "Evaluate"),
            "Ws2.Evaluate" => (ws2, "Evaluate"),
            "X4M" => (root, "ExecuteExcel4Macro"),
            _ => continue,
        };
        let r = app.invoke(
            target,
            member,
            Call {
                flags: flags::METHOD | flags::GET,
                lcid: 0x0409,
            },
            &[Variant::Bstr(text.as_str().into())],
            &[],
        );
        let got = match r {
            Ok(v) => show(&mut app, &v),
            Err(e) => format!("ERR {e:?}"),
        };
        if got.replace("[Book1]", "[BookN]") != want.replace("[Book1]", "[BookN]") {
            bad.push(format!("{line}\n    got {got}"));
        }
    }
    assert!(bad.is_empty(), "{} differ:\n{}", bad.len(), bad.join("\n"));
}

#[test]
fn evaluate_with_vba_calls_follows_excel() {
    let _serial = SERIAL
        .lock()
        .unwrap_or_else(std::sync::PoisonError::into_inner);
    if xlcalc_winbridge::create("Xlc.TestFuncs").is_err() {
        eprintln!("skipped: Xlc.TestFuncs is not registered");
        return;
    }
    let mut app = Application::new();
    app.set_com_bridge(Box::new(Injected(WindowsBridge::new())));
    app.add_workbook(xlcalc::Workbook::new());
    let root = app.handle();
    let list = obj(call(
        &mut app,
        root,
        "AddIns",
        flags::GET | flags::METHOD,
        &[],
    ));
    let Variant::I4(count) = call(&mut app, list, "Count", flags::GET, &[]) else {
        panic!("Count");
    };
    for i in 1..=count {
        let item = obj(call(
            &mut app,
            list,
            "Item",
            flags::GET | flags::METHOD,
            &[Variant::I4(i)],
        ));
        if call(&mut app, item, "Title", flags::GET, &[])
            == Variant::Bstr("XlcTest.TestFuncs".into())
        {
            call(
                &mut app,
                item,
                "Installed",
                flags::PUT,
                &[Variant::Bool(true)],
            );
        }
    }
    let dll = std::env::current_exe()
        .ok()
        .and_then(|e| Some(e.parent()?.parent()?.join("xlc_testxll.dll")))
        .filter(|p| p.exists());
    if let Some(p) = &dll {
        call(
            &mut app,
            root,
            "RegisterXLL",
            flags::METHOD,
            &[Variant::Bstr(p.to_string_lossy().as_ref().into())],
        );
    }
    let book = obj(call(&mut app, root, "ActiveWorkbook", flags::GET, &[]));
    let project = obj(call(&mut app, book, "VBProject", flags::GET, &[]));
    let comps = obj(call(&mut app, project, "VBComponents", flags::GET, &[]));
    let module = obj(call(
        &mut app,
        comps,
        "Add",
        flags::GET | flags::METHOD,
        &[Variant::I4(1)],
    ));
    let code = obj(call(&mut app, module, "CodeModule", flags::GET, &[]));
    call(
        &mut app,
        code,
        "AddFromString",
        flags::METHOD,
        &[Variant::Bstr(
            "Public Function Dbl(x)
  Dbl = x * 2
End Function
"
            .into(),
        )],
    );
    put(&mut app, "B1", "Value2", Variant::R8(5.0));
    put(&mut app, "B2", "Value2", Variant::R8(6.0));
    let golden = include_str!("../../../conformance/com/evaluate_probe2.golden.txt");
    let mut bad = Vec::new();
    for line in golden.lines() {
        let Some((head, want)) = line.split_once(" => ") else {
            continue;
        };
        let Some(open) = head.find(" [") else {
            continue;
        };
        let (who, text) = (&head[..open], head[open + 2..].trim_end_matches(']'));
        if text.contains("Xlc") && dll.is_none() || text.contains("EUROCONVERT") {
            continue;
        }
        let got = match who {
            "App.Evaluate" | "Ws.Evaluate" => {
                let target = if who == "App.Evaluate" {
                    root
                } else {
                    obj(call(&mut app, root, "ActiveSheet", flags::GET, &[]))
                };
                match app.invoke(
                    target,
                    "Evaluate",
                    Call {
                        flags: flags::METHOD | flags::GET,
                        lcid: 0x0409,
                    },
                    &[Variant::Bstr(text.into())],
                    &[],
                ) {
                    Ok(v) => show(&mut app, &v),
                    Err(e) => format!("ERR {e:?}"),
                }
            }
            "cell" => {
                put(&mut app, "D1", "Formula", Variant::Bstr(format!("={text}")));
                format!("String:{}", read(&mut app, "D1"))
            }
            _ => continue,
        };
        if got != want {
            bad.push(format!(
                "{line}
    got {got}"
            ));
        }
    }
    assert!(
        bad.is_empty(),
        "{} differ:
{}",
        bad.len(),
        bad.join(
            "
"
        )
    );
}

#[test]
fn xll_addins_follow_excel() {
    let _serial = SERIAL
        .lock()
        .unwrap_or_else(std::sync::PoisonError::into_inner);
    let Some(dll) = std::env::current_exe()
        .ok()
        .and_then(|e| Some(e.parent()?.parent()?.join("xlc_testxll.dll")))
        .filter(|p| p.exists())
    else {
        eprintln!("skipped: xlc_testxll.dll is not built");
        return;
    };
    let mut app = Application::new();
    app.add_workbook(xlcalc::Workbook::new());
    let root = app.handle();
    let list = obj(call(
        &mut app,
        root,
        "AddIns",
        flags::GET | flags::METHOD,
        &[],
    ));
    let count = |app: &mut Application| call(app, list, "Count", flags::GET, &[]);
    let before = count(&mut app);
    let path = Variant::Bstr(dll.to_string_lossy().as_ref().into());
    assert_eq!(
        call(
            &mut app,
            root,
            "RegisterXLL",
            flags::METHOD,
            std::slice::from_ref(&path)
        ),
        Variant::Bool(true)
    );
    assert_eq!(count(&mut app), before, "RegisterXLL lists nothing");
    assert_eq!(text(&mut app, "=XlcDbl(2)"), "4");
    let a = obj(call(
        &mut app,
        list,
        "Add",
        flags::METHOD,
        std::slice::from_ref(&path),
    ));
    assert_eq!(
        call(&mut app, a, "Title", flags::GET, &[]),
        Variant::Bstr("Xlc_Testxll".into())
    );
    assert_eq!(
        call(&mut app, a, "Installed", flags::GET, &[]),
        Variant::Bool(false)
    );
    let Variant::I4(n) = before else {
        panic!("count")
    };
    assert_eq!(count(&mut app), Variant::I4(n + 1));
    call(&mut app, a, "Installed", flags::PUT, &[Variant::Bool(true)]);
    assert_eq!(text(&mut app, "=XlcDbl(3)"), "6");
    call(
        &mut app,
        a,
        "Installed",
        flags::PUT,
        &[Variant::Bool(false)],
    );
    assert_eq!(text(&mut app, "=XlcDbl(5)"), "#VALUE!");
    call(&mut app, a, "Installed", flags::PUT, &[Variant::Bool(true)]);
    assert_eq!(text(&mut app, "=XlcDbl(6)"), "12");
}
