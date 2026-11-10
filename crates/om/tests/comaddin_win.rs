#![cfg(windows)]

use xlcalc_om::{flags, Application, Call, ComBridge, ExtCall, RegisteredAddin, WindowsBridge};
use xlcalc_variant::ObjectId;
use xlcalc_variant::Variant;
use xlcalc_vba::{InvokeKind, VbaError};

struct Injected(WindowsBridge, Vec<RegisteredAddin>);

impl ComBridge for Injected {
    fn registered_addins(&mut self) -> Vec<RegisteredAddin> {
        self.1.clone()
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
    fn automation_servers(&mut self) -> Vec<xlcalc_om::AutomationServer> {
        Vec::new()
    }
    fn has_member(&mut self, h: u64, n: &str) -> bool {
        self.0.has_member(h, n)
    }
}

fn reg(prog_id: &str, friendly: &str, behavior: u32) -> RegisteredAddin {
    RegisteredAddin {
        prog_id: prog_id.into(),
        friendly_name: friendly.into(),
        description: String::new(),
        guid: String::new(),
        load_behavior: behavior,
    }
}

static SERIAL: std::sync::Mutex<()> = std::sync::Mutex::new(());

fn log_path() -> std::path::PathBuf {
    std::env::temp_dir().join("xlc_testaddin.log")
}

fn log_since(from: usize) -> Vec<String> {
    let pid = format!("{} ", std::process::id());
    let text = std::fs::read_to_string(log_path()).unwrap_or_default();
    text.lines()
        .filter_map(|l| l.strip_prefix(&pid))
        .filter(|l| !l.contains(" set "))
        .skip(from)
        .map(|l| {
            let mut w = l.split_whitespace();
            let (class, ev) = (w.next().unwrap_or(""), w.next().unwrap_or(""));
            match w.next().filter(|x| x.starts_with("mode=")) {
                Some(m) => format!("{class} {ev} {m}"),
                None => format!("{class} {ev}"),
            }
        })
        .collect()
}

fn count_mine() -> usize {
    log_since(0).len()
}

fn fixture_present() -> bool {
    xlcalc_winbridge::create("Xlc.TestAddin").is_ok()
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

fn get(app: &mut Application, o: ObjectId, name: &str) -> Variant {
    call(app, o, name, flags::GET | flags::METHOD, &[])
}

fn obj(v: Variant) -> ObjectId {
    match v {
        Variant::Dispatch(Some(id)) => id,
        other => panic!("not an object: {other:?}"),
    }
}

fn started() -> Option<(
    Application<'static>,
    usize,
    std::sync::MutexGuard<'static, ()>,
)> {
    let guard = SERIAL.lock().unwrap_or_else(|e| e.into_inner());
    if !fixture_present() {
        eprintln!("skipped: Xlc.TestAddin is not registered");
        return None;
    }
    let base = count_mine();
    let mut app = Application::new();
    app.set_com_bridge(Box::new(Injected(
        WindowsBridge::new(),
        vec![
            reg("Xlc.TestAddin", "Xlc Test Add-in", 3),
            reg("Xlc.TestAddin2", "Xlc Test Add-in 2", 3),
        ],
    )));
    Some((app, base, guard))
}

#[test]
fn startup_and_shutdown_follow_excel() {
    let Some((mut app, base, _g)) = started() else {
        return;
    };
    assert_eq!(
        log_since(base),
        [
            "TestAddin ctor",
            "TestAddin OnConnection mode=1",
            "TestAddin OnAddInsUpdate",
            "TestAddin2 ctor",
            "TestAddin2 OnConnection mode=1",
            "TestAddin OnAddInsUpdate",
            "TestAddin2 OnAddInsUpdate",
            "TestAddin OnStartupComplete",
            "TestAddin2 OnStartupComplete",
        ]
    );
    let n = log_since(0).len();
    let root = app.handle();
    call(&mut app, root, "Quit", flags::METHOD, &[]);
    assert_eq!(
        log_since(n),
        [
            "TestAddin OnBeginShutdown",
            "TestAddin2 OnBeginShutdown",
            "TestAddin OnDisconnection mode=0",
            "TestAddin2 OnDisconnection mode=0",
        ]
    );
}

#[test]
fn collection_and_connect_follow_excel() {
    let Some((mut app, _, _g)) = started() else {
        return;
    };
    let root = app.handle();
    let coll = obj(get(&mut app, root, "COMAddIns"));
    assert_eq!(get(&mut app, coll, "Count"), Variant::I4(2));
    let item = |app: &mut Application, k: Variant| {
        app.invoke(
            coll,
            "Item",
            Call {
                flags: flags::METHOD | flags::GET,
                lcid: 0x0409,
            },
            &[k],
            &[],
        )
    };

    let a = obj(item(&mut app, Variant::Bstr("XLC.TESTADDIN".into())).unwrap());
    assert_eq!(
        get(&mut app, a, "ProgId"),
        Variant::Bstr("Xlc.TestAddin".into())
    );
    assert_eq!(
        get(&mut app, a, "Description"),
        Variant::Bstr("Xlc Test Add-in".into())
    );
    for bad in [
        Variant::I4(0),
        Variant::I4(-1),
        Variant::I4(3),
        Variant::Bstr("x".into()),
    ] {
        let e = item(&mut app, bad).unwrap_err();
        assert_eq!(e.hresult.0, 0x8002_000B_u32 as i32);
    }
    assert!(item(&mut app, Variant::R8(1.5)).is_ok());
    assert_eq!(get(&mut app, a, "Connect"), Variant::Bool(true));
    let n = log_since(0).len();
    let put =
        |app: &mut Application, v: bool| call(app, a, "Connect", flags::PUT, &[Variant::Bool(v)]);
    put(&mut app, true);
    assert_eq!(log_since(n), Vec::<String>::new());
    put(&mut app, false);
    assert_eq!(log_since(n), ["TestAddin OnDisconnection mode=1"]);
    assert_eq!(get(&mut app, a, "Connect"), Variant::Bool(false));
    let n = log_since(0).len();
    put(&mut app, false);
    assert_eq!(
        log_since(n),
        ["TestAddin ctor", "TestAddin OnDisconnection mode=1"]
    );
    let n = log_since(0).len();
    put(&mut app, true);
    assert_eq!(
        log_since(n),
        [
            "TestAddin ctor",
            "TestAddin OnConnection mode=0",
            "TestAddin OnAddInsUpdate",
            "TestAddin2 OnAddInsUpdate",
        ]
    );

    let n = log_since(0).len();
    call(&mut app, coll, "Update", flags::METHOD, &[]);
    assert_eq!(log_since(n), Vec::<String>::new());
}

#[test]
fn create_object_reaches_a_real_server() {
    let Some((mut app, _, _g)) = started() else {
        return;
    };
    let book = app.add_workbook(xlcalc::Workbook::new());
    let code = "Public Function F()\r\n  Dim o As Object\r\n  Set o = CreateObject(\"Xlc.TestAddin\")\r\n  F = o.Twice(21) & \"/\" & o.Hello(\"x\")\r\nEnd Function\r\n\
                Public Function G()\r\n  On Error Resume Next\r\n  CreateObject \"No.Such.Class\"\r\n  G = Err.Number\r\nEnd Function\r\n\
                Public Function H()\r\n  On Error Resume Next\r\n  Dim o As Object\r\n  Set o = CreateObject(\"Xlc.TestAddin\")\r\n  o.Nope\r\n  H = Err.Number\r\nEnd Function\r\n";
    app.add_vba_module(
        book,
        xlcalc_vba::SourceModule {
            name: "Module1".into(),
            kind: xlcalc_vba::ModuleKind::Standard,
            source: code.into(),
        },
    )
    .unwrap();
    let root = app.handle();
    let run = |app: &mut Application, f: &str| {
        call(app, root, "Run", flags::METHOD, &[Variant::Bstr(f.into())])
    };
    assert_eq!(run(&mut app, "F"), Variant::Bstr("42/hello x".into()));
    assert_eq!(run(&mut app, "G"), Variant::I4(429));
    assert_eq!(run(&mut app, "H"), Variant::I4(438));
}
