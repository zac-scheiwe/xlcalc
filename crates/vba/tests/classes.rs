use std::collections::HashMap;
use std::fs;
use std::path::Path;
use xlcalc_variant::{ObjectId, Variant};
use xlcalc_vba::{
    DebugItem, ExcelNames, InvokeKind, MacroLookup, ModuleKind, NoHost, ObjectHost, Project,
    SourceModule, VbaError,
};

const SHEET: ObjectId = ObjectId(1);
const BOOK: ObjectId = ObjectId(2);

#[derive(Default)]
struct Host {
    cells: HashMap<String, Variant>,
    ranges: Vec<String>,
    printed: Vec<Vec<DebugItem>>,
    asserts: usize,
}

impl ObjectHost for Host {
    fn global(&mut self) -> Result<ObjectId, VbaError> {
        Err(VbaError::new(424))
    }

    fn invoke(
        &mut self,
        obj: ObjectId,
        member: &str,
        kind: InvokeKind,
        positional: &[Variant],
        _: &[(&str, Variant)],
    ) -> Result<Variant, VbaError> {
        let m = member.to_ascii_lowercase();
        match (obj, m.as_str(), kind) {
            (SHEET, "name", InvokeKind::Get) => Ok(Variant::Bstr("Sheet1".to_string())),
            (BOOK, "name", InvokeKind::Get) => Ok(Variant::Bstr("Book3".to_string())),
            (SHEET, "range", InvokeKind::Get) => {
                let Some(Variant::Bstr(a)) = positional.first() else {
                    return Err(VbaError::new(13));
                };
                self.ranges.push(a.to_ascii_uppercase());
                Ok(Variant::Dispatch(Some(ObjectId(
                    100 + self.ranges.len() as u64,
                ))))
            }
            (ObjectId(n), "value" | "", k) if n > 100 => {
                let a = self.ranges[(n - 101) as usize].clone();
                match k {
                    InvokeKind::Get => Ok(self.cells.get(&a).cloned().unwrap_or(Variant::Empty)),
                    _ => {
                        let v = positional.last().cloned().unwrap_or(Variant::Empty);
                        self.cells.insert(a, v);
                        Ok(Variant::Empty)
                    }
                }
            }
            _ => Err(VbaError::new(438)),
        }
    }

    fn type_name(&mut self, obj: ObjectId) -> String {
        match obj {
            SHEET => "Worksheet",
            BOOK => "Workbook",
            _ => "Range",
        }
        .to_string()
    }

    fn document(&mut self, module: &str) -> Result<ObjectId, VbaError> {
        match module {
            "Sheet1" => Ok(SHEET),
            "ThisWorkbook" => Ok(BOOK),
            _ => Err(VbaError::new(424)),
        }
    }

    fn debug_print(&mut self, items: &[DebugItem]) {
        self.printed.push(items.to_vec());
    }

    fn debug_assert(&mut self) {
        self.asserts += 1;
    }
}

fn root() -> &'static Path {
    Path::new(env!("CARGO_MANIFEST_DIR"))
}

fn probe_modules() -> Vec<SourceModule> {
    let ps = fs::read_to_string(root().join("../../oracle/prog/m6_vba_probe.ps1")).unwrap();
    let mut out = Vec::new();
    let mut cur: Option<(String, ModuleKind, String)> = None;
    for line in ps.lines() {
        if let Some((name, kind, src)) = &mut cur {
            if line.starts_with("\"@") {
                out.push(SourceModule {
                    name: name.clone(),
                    kind: *kind,
                    source: std::mem::take(src),
                });
                cur = None;
            } else {
                src.push_str(line);
                src.push('\n');
            }
            continue;
        }
        let t = line.trim();
        let quoted = |t: &str| t.split('\'').nth(1).unwrap_or_default().to_string();
        cur = if t.starts_with("Add-Std ") {
            Some((quoted(t), ModuleKind::Standard, String::new()))
        } else if t.starts_with("Add-Cls ") {
            Some((quoted(t), ModuleKind::Class, String::new()))
        } else if t.starts_with("$cls = @\"") {
            Some(("CV".to_string(), ModuleKind::Class, String::new()))
        } else if t.starts_with("$vbc.Item($ws.CodeName)") {
            Some(("Sheet1".to_string(), ModuleKind::Document, String::new()))
        } else if t.starts_with("$vbc.Item($wb.CodeName)") {
            Some((
                "ThisWorkbook".to_string(),
                ModuleKind::Document,
                String::new(),
            ))
        } else {
            None
        };
    }
    out
}

fn fmt(v: &Variant) -> String {
    match v {
        Variant::Empty => "<null>".to_string(),
        Variant::Bstr(s) => format!("String:{s}"),
        Variant::BstrNull => "String:".to_string(),
        Variant::I4(n) => format!("Int32:{n}"),
        v => format!("{v:?}"),
    }
}

fn run_error(name: &str, e: Option<&VbaError>) -> String {
    match e {
        None => format!(
            " ERR 800A03EC Cannot run the macro '{name}'. The macro may not be available in this workbook or all macros may be disabled."
        ),
        Some(e) => {
            let h = 0x800A_0000u32 | e.number as u32;
            let msg = if e.description.is_empty() {
                format!("0x{h:08X}")
            } else {
                e.description.clone()
            };
            format!(" ERR {h:08X} {msg}")
        }
    }
}

#[test]
fn probe_steps_match_excel() {
    if !root().join("../../oracle/prog/m6_vba_probe.ps1").exists() {
        return;
    }
    let modules = probe_modules();
    assert_eq!(modules.len(), 11);
    let p = Project::compile(&modules, &ExcelNames);
    assert!(p.errors().is_empty(), "{:?}", p.errors());
    let mut host = Host::default();
    let recorded = fs::read_to_string(root().join("tests/data/m6_vba_probe.txt")).unwrap();
    let get_log = |host: &mut Host| match p.call("GetLog", &[], host) {
        Ok(Variant::Bstr(s)) => s,
        Ok(Variant::BstrNull) => String::new(),
        r => panic!("GetLog: {r:?}"),
    };
    get_log(&mut host);
    let i = Variant::I4;
    let mut ran = 0;
    for line in recorded.lines() {
        let (label, want) = line.split_once(" => ").unwrap();
        let (name, mut args): (&str, Vec<Variant>) = match label.strip_prefix("run ") {
            None => (label, Vec::new()),
            Some("quoted book module" | "bare book module" | "bare book") => continue,
            Some("lower case") => ("onlym2", Vec::new()),
            Some("spaced") => (" OnlyM2", Vec::new()),
            Some("CA.Fire") => ("CA.Fire", vec![i(1)]),
            Some("Echo 0") => ("Echo", Vec::new()),
            Some("Echo 3") => (
                "Echo",
                vec![i(1), Variant::Bstr("b".to_string()), Variant::R8(2.5)],
            ),
            Some("Echo 30") => ("Echo", (1..=30).map(i).collect()),
            Some("Typed \"5\"") => ("Typed", vec![Variant::Bstr("5".to_string())]),
            Some("Typed 2.5") => ("Typed", vec![Variant::R8(2.5)]),
            Some(n) => (n.split(' ').next().unwrap(), Vec::new()),
        };
        let r = match p.lookup_macro(name) {
            MacroLookup::Found(_) => p.run_macro(name, &mut args, &mut host),
            MacroLookup::NotFound | MacroLookup::Ambiguous => Err(VbaError::new(1004)),
        };
        let log = get_log(&mut host);
        let got = match &r {
            Ok(v) => format!("ret={} log={log}", fmt(v)),
            Err(e) if e.number == 1004 => format!("ret=<null> log={log}{}", run_error(name, None)),
            Err(e) => format!("ret=<null> log={log}{}", run_error(name, Some(e))),
        };

        let want = want.replace("log=LOGERR", "log=");
        assert_eq!(got, want, "{label}");
        ran += 1;
    }
    assert_eq!(ran, 47);
}

fn project(modules: &[(&str, ModuleKind, &str)]) -> Project {
    let m: Vec<SourceModule> = modules
        .iter()
        .map(|&(name, kind, source)| SourceModule {
            name: name.to_string(),
            kind,
            source: source.to_string(),
        })
        .collect();
    let p = Project::compile(&m, &ExcelNames);
    assert!(p.errors().is_empty(), "{:?}", p.errors());
    p
}

#[test]
fn debug_print_and_assert_go_to_the_host() {
    let p = project(&[(
        "M",
        ModuleKind::Standard,
        "Sub S()\nDebug.Print 1, \"a\"; 2\nDebug.Print\nDebug.Assert 1 = 2\nDebug.Assert True\nEnd Sub",
    )]);
    let mut host = Host::default();
    p.run_macro("S", &mut [], &mut host).unwrap();
    use DebugItem::*;
    let s = |x: &str| Value(Variant::Bstr(x.to_string()));
    assert_eq!(
        host.printed,
        vec![
            vec![
                Value(Variant::I2(1)),
                Zone,
                s("a"),
                Join,
                Value(Variant::I2(2))
            ],
            vec![]
        ]
    );
    assert_eq!(host.asserts, 1);
}

#[test]
fn stop_is_an_error() {
    let p = project(&[("M", ModuleKind::Standard, "Sub S()\nStop\nEnd Sub")]);
    let e = p.run_macro("S", &mut [], &mut NoHost).unwrap_err();
    assert_eq!(e.number, 438);
}

#[test]
fn friend_members_are_early_bound_only() {
    let p = project(&[
        (
            "C",
            ModuleKind::Class,
            "Friend Function F() As Long\nF = 7\nEnd Function",
        ),
        (
            "M",
            ModuleKind::Standard,
            "Function Early() As Long\nDim c As New C\nEarly = c.F\nEnd Function\nFunction Late() As Long\nDim o As Object\nSet o = New C\nLate = o.F\nEnd Function",
        ),
    ]);
    assert_eq!(p.call("Early", &[], &mut NoHost).unwrap(), Variant::I4(7));
    assert_eq!(p.call("Late", &[], &mut NoHost).unwrap_err().number, 438);
}

#[test]
fn byref_arguments_of_members_and_macros() {
    let p = project(&[
        (
            "C",
            ModuleKind::Class,
            "Public Sub Bump(n As Long)\nn = n + 1\nEnd Sub",
        ),
        (
            "M",
            ModuleKind::Standard,
            "Function F() As Long\nDim o As Object, n As Long\nSet o = New C\nn = 1\no.Bump n\nF = n\nEnd Function\nSub G(x As Variant)\nx = x * 2\nEnd Sub",
        ),
    ]);
    assert_eq!(p.call("F", &[], &mut NoHost).unwrap(), Variant::I4(2));
    let mut args = [Variant::I4(4)];
    p.run_macro("G", &mut args, &mut NoHost).unwrap();
    assert_eq!(args[0], Variant::I4(8));
}

struct Reentrant<'a> {
    p: Option<&'a Project>,
}

impl ObjectHost for Reentrant<'_> {
    fn global(&mut self) -> Result<ObjectId, VbaError> {
        Ok(ObjectId(9))
    }

    fn invoke(
        &mut self,
        _: ObjectId,
        _: &str,
        _: InvokeKind,
        _: &[Variant],
        _: &[(&str, Variant)],
    ) -> Result<Variant, VbaError> {
        let p = self.p.take().ok_or_else(|| VbaError::new(28))?;
        let r = p.call("Inner", &[], self);
        self.p = Some(p);
        r
    }
}

#[test]
fn the_host_may_call_back_in() {
    let p = project(&[(
        "M",
        ModuleKind::Standard,
        "Public N As Long\nFunction Inner() As Long\nN = N + 10\nInner = N\nEnd Function\nFunction Outer() As Long\nN = 1\nOuter = Range(\"A1\") + N\nEnd Function",
    )]);
    let mut host = Reentrant { p: Some(&p) };
    assert_eq!(p.call("Outer", &[], &mut host).unwrap(), Variant::I4(22));
}

#[test]
fn sinks_of_host_objects() {
    let p = project(&[
        (
            "C",
            ModuleKind::Class,
            "Public WithEvents App As Object\nPublic Sub Hook(o As Object)\nSet App = o\nEnd Sub\nPrivate Sub App_Change(ByVal n As Long, Cancel As Boolean)\nCancel = n > 1\nEnd Sub",
        ),
        (
            "M",
            ModuleKind::Standard,
            "Public K As C\nSub Attach()\nSet K = New C\nK.Hook Application\nEnd Sub",
        ),
    ]);
    struct App;
    impl ObjectHost for App {
        fn global(&mut self) -> Result<ObjectId, VbaError> {
            Ok(ObjectId(5))
        }
        fn invoke(
            &mut self,
            _: ObjectId,
            _: &str,
            _: InvokeKind,
            _: &[Variant],
            _: &[(&str, Variant)],
        ) -> Result<Variant, VbaError> {
            Ok(Variant::Dispatch(Some(ObjectId(7))))
        }
    }
    p.run_macro("Attach", &mut [], &mut App).unwrap();
    assert_eq!(p.held_objects(), vec![ObjectId(7)]);
    let sinks = p.sinks(ObjectId(7), "Change");
    assert_eq!(sinks.len(), 1);
    assert_eq!(sinks[0].procedure, "C.App_Change");
    let mut args = [Variant::I4(2), Variant::Bool(false)];
    p.call_sink(&sinks[0], &mut args, &mut App).unwrap();
    assert_eq!(args[1], Variant::Bool(true));
}
