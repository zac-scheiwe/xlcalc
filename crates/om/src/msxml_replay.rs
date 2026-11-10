use std::collections::{HashMap, HashSet};
use std::fs;
use std::path::{Path, PathBuf};

use xlcalc_variant::{HResult, ObjectId, Variant};
use xlcalc_vba::{InvokeKind, VbaError};

use crate::adodb_replay::{byte_array, dec, dotnet_r, enc, from_hex, split_top, to_hex};
use crate::Application;

const ROOTS: [(&str, &str); 7] = [
    ("d", "MSXML2.DOMDocument.6.0"),
    ("e", "MSXML2.DOMDocument.6.0"),
    ("d3", "MSXML2.DOMDocument.3.0"),
    ("d0", "MSXML2.DOMDocument"),
    ("dx", "MSXML.DOMDocument"),
    ("dm", "Microsoft.XMLDOM"),
    ("dt", "MSXML2.FreeThreadedDOMDocument.6.0"),
];

const NULL_CALL: i32 = 0x8013_1501_u32 as i32;
const NO_METHOD: i32 = 0x8013_1512_u32 as i32;

struct World<'a> {
    app: Application<'a>,
    dir: String,
    vars: HashMap<String, Variant>,
    roots: HashMap<String, ObjectId>,
}

impl World<'_> {
    fn show(&mut self, v: &Variant) -> String {
        match v {
            Variant::Empty | Variant::BstrNull | Variant::Dispatch(None) => "-".into(),
            Variant::Null => "Null".into(),
            Variant::Bool(b) => if *b { "True" } else { "False" }.into(),
            Variant::Bstr(s) => {
                let s = s.replace(&self.dir.replace('\\', "/"), &self.dir);
                format!("'{}'", enc(&s, &self.dir))
            }
            Variant::I2(n) => format!("I2:{n}"),
            Variant::I4(n) => format!("I4:{n}"),
            Variant::R8(x) => format!("R8:{}", dotnet_r(*x)),
            Variant::Dispatch(Some(id)) => format!("obj:{}", self.app.com_type_name(*id)),
            other => format!("?{other:?}"),
        }
    }

    fn arg(&mut self, tok: &str) -> Variant {
        let tok = tok.trim();
        if let Some(inner) = tok.strip_prefix('\'') {
            let s = dec(&inner[..inner.len() - 1], &self.dir);
            return Variant::Bstr(s.replace("''", "'"));
        }
        if tok.starts_with('$') {
            return self.vars.get(tok).cloned().unwrap_or(Variant::Empty);
        }
        if let Some(r) = tok.strip_prefix('@') {
            return self.root(r);
        }
        match tok {
            "T" => return Variant::Bool(true),
            "F" => return Variant::Bool(false),
            "null" => return Variant::Null,
            "empty" => return Variant::Empty,
            "nothing" => return Variant::Dispatch(None),
            "miss" => return Variant::Error(HResult::DISP_E_PARAMNOTFOUND.0),
            _ => {}
        }
        if let Some(x) = tok.strip_prefix("i2:") {
            return Variant::I2(x.parse().unwrap());
        }
        if let Some(x) = tok.strip_prefix("x:") {
            return byte_array(&from_hex(x));
        }
        if let Ok(n) = tok.parse::<i32>() {
            return Variant::I4(n);
        }
        Variant::R8(tok.parse().unwrap())
    }

    fn elem(&mut self, e: &str) -> (String, Vec<Variant>) {
        let Some(i) = e.find('(') else {
            return (e.to_string(), Vec::new());
        };
        let name = e[..i].to_string();
        let inner = &e[i + 1..e.len() - 1];
        let args = if inner.trim().is_empty() {
            Vec::new()
        } else {
            split_top(inner, ',').iter().map(|p| self.arg(p)).collect()
        };
        (name, args)
    }

    fn root(&mut self, n: &str) -> Variant {
        if let Some((_, prog)) = ROOTS.iter().find(|(r, _)| *r == n) {
            if !self.roots.contains_key(n) {
                let id = self.app.create_com_object(prog).unwrap();
                self.roots.insert(n.to_string(), id);
            }
            return Variant::Dispatch(Some(self.roots[n]));
        }
        self.vars.get(n).cloned().unwrap_or(Variant::Empty)
    }

    fn eval(&mut self, path: &str, put: Option<Variant>) -> Result<Variant, VbaError> {
        let els = split_top(path, '.');
        let mut cur = self.root(&els[0]);
        for (k, e) in els.iter().enumerate().skip(1) {
            let (mut name, mut args) = self.elem(e);
            if name == "@" {
                name.clear();
            }
            let Variant::Dispatch(Some(id)) = cur else {
                return Err(VbaError {
                    number: if cur == Variant::Null {
                        NO_METHOD
                    } else {
                        NULL_CALL
                    },
                    description: String::new(),
                });
            };
            if k == els.len() - 1 {
                if let Some(v) = put {
                    args.push(v);
                    self.app.com_invoke(id, &name, InvokeKind::Put, &args)?;
                    return Ok(Variant::Empty);
                }
            }
            cur = self.app.com_invoke(id, &name, InvokeKind::Get, &args)?;
        }
        Ok(cur)
    }

    fn error_text(&self, e: &VbaError) -> String {
        let n = e.number as u32;
        match n {
            13 => format!(
                "!0x80020005 {}",
                enc(
                    "Type mismatch. (Exception from HRESULT: 0x80020005 (DISP_E_TYPEMISMATCH))",
                    &self.dir
                )
            ),
            438 => format!(
                "!0x80020006 {}",
                enc(
                    "Unknown name. (Exception from HRESULT: 0x80020006 (DISP_E_UNKNOWNNAME))",
                    &self.dir
                )
            ),
            450 => "!N:0x8002000E".into(),
            0x8007_0057 | 0x8007_0003 | 0x8013_1501 | 0x8013_1512 => format!("!N:0x{n:08X}"),
            n if n >= 0x8000_0000 => format!("!0x{n:08X} {}", enc(&e.description, &self.dir)),
            n => format!("!VBA{n}"),
        }
    }

    fn each(&mut self, p: &str, mode: char) -> Result<String, VbaError> {
        let v = self.eval(p.trim(), None)?;
        if matches!(v, Variant::Empty | Variant::Dispatch(None)) {
            return Ok("{}".into());
        }
        let Variant::Dispatch(Some(id)) = v else {
            return Err(VbaError {
                number: NULL_CALL,
                description: String::new(),
            });
        };
        let items = self.app.com_enumerate(id)?;
        let mut shown = Vec::new();
        for x in &items {
            let member = match mode {
                'n' => "nodeName",
                'x' => "xml",
                _ => "",
            };
            shown.push(match x {
                Variant::Dispatch(Some(i)) if !member.is_empty() => {
                    let v = self.app.com_invoke(*i, member, InvokeKind::Get, &[])?;
                    self.show(&v)
                }
                _ => self.show(x),
            });
        }
        Ok(format!("{{{}}}", shown.join(",")))
    }

    fn run_step(&mut self, st: &str) -> Result<String, VbaError> {
        for (prefix, mode) in [("eachn ", 'n'), ("eachx ", 'x'), ("each ", ' ')] {
            if let Some(p) = st.strip_prefix(prefix) {
                return self.each(p, mode);
            }
        }
        let (mut var, mut body) = (None, st);
        if let Some(rest) = st.strip_prefix('$') {
            if let Some((n, r)) = rest.split_once('=') {
                if n.chars().all(|c| c.is_alphanumeric() || c == '_') {
                    var = Some(format!("${n}"));
                    body = r;
                }
            }
        }
        let parts = split_top(body, '=');
        if parts.len() > 1 {
            let v = self.arg(&parts[1..].join("="));
            self.eval(&parts[0], Some(v))?;
            return Ok(".".into());
        }
        let res = self.eval(body, None)?;
        if let Some(n) = var {
            self.vars.insert(n.clone(), res);
            return Ok(format!(".{n}"));
        }
        Ok(self.show(&res))
    }

    fn step(&mut self, st: &str) -> String {
        let st = st.trim();
        if let Some(p) = st.strip_prefix("put ") {
            let (name, hex) = p.split_once(' ').unwrap_or((p, ""));
            fs::write(Path::new(&self.dir).join(name), from_hex(hex)).unwrap();
            return ".".into();
        }
        if let Some(p) = st.strip_prefix("putt ") {
            let (name, text) = p.split_once(' ').unwrap();
            let Variant::Bstr(t) = self.arg(text) else {
                unreachable!()
            };
            fs::write(Path::new(&self.dir).join(name), t.as_bytes()).unwrap();
            return ".".into();
        }
        if let Some(n) = st.strip_prefix("hex ") {
            return match fs::read(Path::new(&self.dir).join(n.trim())) {
                Ok(b) => format!("F:{}", to_hex(&b)),
                Err(_) => "F:<none>".into(),
            };
        }
        if let Some(n) = st.strip_prefix("rm ") {
            let _ = fs::remove_file(Path::new(&self.dir).join(n.trim()));
            return ".".into();
        }
        match self.run_step(st) {
            Ok(s) => s,
            Err(e) => self.error_text(&e),
        }
    }

    fn run_case(&mut self, case: &str) -> String {
        self.vars.clear();
        self.roots.clear();
        self.app = Application::new();
        if let Ok(rd) = fs::read_dir(&self.dir) {
            for f in rd.flatten() {
                let _ = fs::remove_file(f.path());
            }
        }
        split_top(case, ';')
            .iter()
            .filter(|s| !s.trim().is_empty())
            .map(|s| self.step(s))
            .collect::<Vec<_>>()
            .join(" | ")
    }
}

fn conformance(name: &str) -> PathBuf {
    PathBuf::from(env!("CARGO_MANIFEST_DIR"))
        .join("../../conformance/com")
        .join(name)
}

#[test]
fn msxml_golden() {
    let dir = std::env::temp_dir().join(format!("msxmlreplay_{}", std::process::id()));
    fs::create_dir_all(&dir).unwrap();
    let mut w = World {
        app: Application::new(),
        dir: dir.to_string_lossy().to_string(),
        vars: HashMap::new(),
        roots: HashMap::new(),
    };
    let text = fs::read_to_string(conformance("msxml_probe.golden.txt")).unwrap();
    let open_text = fs::read_to_string(conformance("msxml_probe.open.txt")).unwrap_or_default();
    let open: HashSet<&str> = open_text.lines().filter(|l| !l.is_empty()).collect();
    let (mut total, mut bad, mut now_ok) = (0, Vec::new(), 0);
    let mut failing = Vec::new();
    for line in text.lines().filter(|l| !l.is_empty()) {
        let (case, want) = line.split_once(" => ").unwrap();
        total += 1;
        if std::env::var("MSXML_TRACE").is_ok() {
            eprintln!("{case}");
        }
        let got = w.run_case(case);
        if got != want {
            failing.push(case.to_string());
            if !open.contains(case) {
                bad.push(format!("{case}\n  want {want}\n  got  {got}"));
            }
        } else if open.contains(case) {
            now_ok += 1;
        }
    }
    let _ = fs::remove_dir_all(&dir);
    if std::env::var("MSXML_WRITE_OPEN").is_ok() {
        fs::write(
            conformance("msxml_probe.open.txt"),
            failing.join("\n") + "\n",
        )
        .unwrap();
        return;
    }
    if let Ok(path) = std::env::var("MSXML_DUMP") {
        fs::write(path, bad.join("\n")).unwrap();
    }
    let shown = std::env::var("MSXML_SHOW")
        .ok()
        .and_then(|s| s.parse().ok())
        .unwrap_or(12);
    assert!(
        bad.is_empty() && now_ok == 0,
        "{} of {total} golden cases differ ({now_ok} open cases now pass):\n{}",
        bad.len(),
        bad.iter()
            .take(shown)
            .cloned()
            .collect::<Vec<_>>()
            .join("\n")
    );
}
