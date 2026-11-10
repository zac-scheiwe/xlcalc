use serde_json::Value as Json;
use std::fs;
use std::path::PathBuf;
use xlcalc_variant::{Decimal, ObjectId, SafeArray, VarType, Variant};
use xlcalc_vba::{ExcelNames, InvokeKind, ModuleKind, ObjectHost, Project, SourceModule, VbaError};

fn root() -> PathBuf {
    PathBuf::from(env!("CARGO_MANIFEST_DIR")).join("../../conformance")
}

fn read_json(path: PathBuf) -> Json {
    serde_json::from_str(&fs::read_to_string(&path).unwrap()).unwrap()
}

struct RunHost;

const GLOBAL: ObjectId = ObjectId(1);
const APPLICATION: ObjectId = ObjectId(2);
const WORKSHEET_FUNCTION: ObjectId = ObjectId(3);

impl ObjectHost for RunHost {
    fn global(&mut self) -> Result<ObjectId, VbaError> {
        Ok(GLOBAL)
    }

    fn invoke(
        &mut self,
        obj: ObjectId,
        member: &str,
        _: InvokeKind,
        _: &[Variant],
        _: &[(&str, Variant)],
    ) -> Result<Variant, VbaError> {
        let m = member.to_ascii_lowercase();
        match (obj, m.as_str()) {
            (GLOBAL, "application") => Ok(Variant::Dispatch(Some(APPLICATION))),
            (APPLICATION, "caller") => Ok(Variant::Error(0x800A_07E7_u32 as i32)),
            (APPLICATION, "worksheetfunction") => Ok(Variant::Dispatch(Some(WORKSHEET_FUNCTION))),
            (APPLICATION, "match") => Ok(Variant::Error(0x800A_07FA_u32 as i32)),
            (WORKSHEET_FUNCTION, "match") => Err(VbaError {
                number: 1004,
                description: "Unable to get the Match property of the WorksheetFunction class"
                    .to_string(),
            }),
            _ => Err(VbaError::new(438)),
        }
    }
}

fn hex(s: &str) -> u64 {
    u64::from_str_radix(s, 16).unwrap()
}

fn variant(j: &Json) -> Variant {
    let obj = match j {
        Json::Null => return Variant::Empty,
        Json::Bool(b) => return Variant::Bool(*b),
        Json::String(s) => return Variant::Bstr(s.clone()),
        Json::Number(n) => {
            return match n.as_i64().and_then(|i| i32::try_from(i).ok()) {
                Some(i) => Variant::I4(i),
                None => Variant::R8(n.as_f64().unwrap()),
            }
        }
        Json::Object(o) => o,
        Json::Array(_) => panic!("array argument"),
    };
    let vt = obj["vt"].as_str().unwrap();
    let v = obj.get("v").cloned().unwrap_or(Json::Null);
    let bits = obj.get("bits").and_then(Json::as_str).map(hex);
    let float = |v: &Json| bits.map_or_else(|| v.as_f64().unwrap(), f64::from_bits);
    match vt {
        "EMPTY" => Variant::Empty,
        "NULL" => Variant::Null,
        "I2" => Variant::I2(v.as_i64().unwrap() as i16),
        "I4" => Variant::I4(v.as_i64().unwrap() as i32),
        "UI1" => Variant::Ui1(v.as_i64().unwrap() as u8),
        "BOOL" => Variant::Bool(v.as_bool().unwrap()),
        "BSTR" if v.is_null() => Variant::BstrNull,
        "BSTR" => Variant::Bstr(v.as_str().unwrap().to_string()),
        "ERROR" => Variant::Error(hex(v.as_str().unwrap()) as u32 as i32),
        "R4" => Variant::R4(match bits {
            Some(b) => f32::from_bits(b as u32),
            None => v.as_f64().unwrap() as f32,
        }),
        "R8" => Variant::R8(float(&v)),
        "DATE" => Variant::Date(float(&v)),
        "CY" => {
            let s = v.as_str().unwrap();
            Variant::Cy(match s.split_once('.') {
                Some((i, f)) => {
                    let f = format!("{f:0<4}");
                    i.parse::<i64>().unwrap() * 10_000 + f.parse::<i64>().unwrap()
                }
                None => s.parse().unwrap(),
            })
        }
        "DECIMAL" => Variant::Decimal(Decimal {
            negative: obj["negative"].as_bool().unwrap(),
            scale: obj["scale"].as_u64().unwrap() as u8,
            mantissa: obj["mantissa"].as_str().unwrap().parse().unwrap(),
        }),
        _ if vt.starts_with("ARRAY|") => {
            let elem = match &vt[6..] {
                "VARIANT" => VarType::Variant,
                "5" => VarType::R8,
                other => panic!("array of {other}"),
            };
            let bounds = obj["bounds"]
                .as_array()
                .unwrap()
                .iter()
                .map(|b| (b[0].as_i64().unwrap() as i32, b[1].as_u64().unwrap() as u32))
                .collect();
            let data = obj["data"]
                .as_array()
                .unwrap()
                .iter()
                .map(variant)
                .collect();
            Variant::Array(Box::new(SafeArray { elem, bounds, data }))
        }
        other => panic!("vt {other}"),
    }
}

fn same(a: &Variant, b: &Variant) -> bool {
    match (a, b) {
        (Variant::R8(x), Variant::R8(y)) | (Variant::Date(x), Variant::Date(y)) => {
            x.to_bits() == y.to_bits()
        }
        (Variant::R4(x), Variant::R4(y)) => x.to_bits() == y.to_bits(),
        (Variant::Array(x), Variant::Array(y)) => {
            x.elem == y.elem
                && x.bounds == y.bounds
                && x.data.len() == y.data.len()
                && x.data.iter().zip(&y.data).all(|(p, q)| same(p, q))
        }
        _ => a == b,
    }
}

const SKIP: &[&str] = &["GetCallCount", "MyG"];

const KNOWN_RT: &[(&str, &str)] = &[];

fn suite(name: &str) -> (usize, Vec<String>) {
    if !root().join(format!("golden/{name}.json")).exists() || !root().join("vba").is_dir() {
        return (usize::MAX, Vec::new());
    }
    let scenario = read_json(root().join(format!("scenarios/{name}.json")));
    let golden = read_json(root().join(format!("golden/{name}.json")));
    let steps = scenario["steps"].as_array().unwrap();
    let mut modules = Vec::new();
    for s in steps {
        if s["op"] == "vba_module" {
            let file = s["file"].as_str().unwrap();
            let kind = if file.ends_with(".cls") {
                ModuleKind::Class
            } else {
                ModuleKind::Standard
            };
            modules.push(SourceModule {
                name: file
                    .trim_end_matches(".bas")
                    .trim_end_matches(".cls")
                    .to_string(),
                kind,
                source: fs::read_to_string(root().join("vba").join(file)).unwrap(),
            });
        }
    }
    let project = Project::compile(&modules, &ExcelNames);
    assert!(project.errors().is_empty(), "{:?}", project.errors());
    let mut failures = Vec::new();
    let mut checked = 0;
    for item in golden["items"].as_array().unwrap() {
        if item["op"] != "run" || item.get("fail").is_some() {
            continue;
        }
        let step = &steps[item["step"].as_u64().unwrap() as usize - 1];
        let proc = step["name"].as_str().unwrap();
        if SKIP.contains(&proc) {
            continue;
        }
        let args: Vec<Variant> = step
            .get("args")
            .and_then(Json::as_array)
            .map(|a| a.iter().map(variant).collect())
            .unwrap_or_default();
        let want = variant(&item["result"]);
        checked += 1;
        if std::env::var_os("VBA_TRACE").is_some() {
            eprintln!("{proc}{args:?}");
        }

        let got = project.call(proc, &args, &mut RunHost).map(|v| match v {
            Variant::Bstr(s) => Variant::Bstr(xlcalc_vba::rt::host_text(&s)),
            v => v,
        });
        let ok = matches!(&got, Ok(v) if same(v, &want));
        let known = KNOWN_RT.iter().find(|k| k.0 == proc);
        match known {
            Some(&(_, was)) if !ok && format!("{got:?}") == was => {}
            Some(_) if ok => failures.push(format!("{proc}: passes now; remove it from KNOWN_RT")),
            _ if ok => {}
            _ => failures.push(format!("{proc}{args:?}: got {got:?}, want {want:?}")),
        }
    }
    (checked, failures)
}

#[test]
fn vba_maths_runs() {
    let (n, failures) = suite("vba_maths");
    assert!(n >= 20, "only {n} steps");
    assert!(failures.is_empty(), "{}", failures.join("\n"));
}

#[test]
fn vba_boundary_runs() {
    let (n, failures) = suite("vba_boundary");
    assert!(n >= 40, "only {n} steps");
    assert!(failures.is_empty(), "{}", failures.join("\n"));
}

#[test]
fn vba_lang_runs() {
    let (n, failures) = std::thread::Builder::new()
        .stack_size(256 << 20)
        .spawn(|| suite("vba_lang"))
        .unwrap()
        .join()
        .unwrap();
    assert!(n >= 1000, "only {n} steps");
    assert!(failures.is_empty(), "{}", failures.join("\n"));
}

#[test]
fn vba_lang2_runs() {
    let (n, failures) = std::thread::Builder::new()
        .stack_size(256 << 20)
        .spawn(|| suite("vba_lang2"))
        .unwrap()
        .join()
        .unwrap();
    assert!(n >= 300, "only {n} steps");
    assert!(
        failures.is_empty(),
        "{}",
        failures.join(
            "
"
        )
    );
}

#[test]
fn vba_io_runs() {
    let (n, failures) = std::thread::Builder::new()
        .stack_size(256 << 20)
        .spawn(|| suite("vba_io"))
        .unwrap()
        .join()
        .unwrap();
    assert!(n >= 300, "only {n} steps");
    assert!(
        failures.is_empty(),
        "{}",
        failures.join(
            "
"
        )
    );
}
