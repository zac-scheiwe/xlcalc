use std::collections::HashMap;
use std::fs;
use std::path::{Path, PathBuf};

use xlcalc_variant::{HResult, ObjectId, SafeArray, VarType, Variant};
use xlcalc_vba::{InvokeKind, VbaError};

use crate::Application;

pub(crate) fn split_top(s: &str, sep: char) -> Vec<String> {
    let mut out = Vec::new();
    let mut cur = String::new();
    let (mut quote, mut depth) = (false, 0i32);
    for c in s.chars() {
        if c == '\'' {
            quote = !quote;
        } else if !quote {
            if c == '(' || c == '[' {
                depth += 1;
            } else if c == ')' || c == ']' {
                depth -= 1;
            }
        }
        if !quote && depth == 0 && c == sep {
            out.push(std::mem::take(&mut cur));
        } else {
            cur.push(c);
        }
    }
    out.push(cur);
    out
}

pub(crate) fn dec(s: &str, dir: &str) -> String {
    let u: Vec<char> = s.chars().collect();
    let mut out: Vec<u16> = Vec::new();
    let mut i = 0;
    let hex = |a: &[char]| -> Option<u16> {
        a.iter()
            .try_fold(0u16, |n, c| c.to_digit(16).map(|d| n * 16 + d as u16))
    };
    while i < u.len() {
        if u[i] == '%' {
            let long = u.get(i + 1) == Some(&'u');
            let (n, len) = if long { (4, 6) } else { (2, 3) };
            if let Some(v) = u.get(i + len - n..i + len).and_then(hex) {
                out.push(v);
                i += len;
                continue;
            }
        }
        let mut b = [0u16; 2];
        out.extend_from_slice(u[i].encode_utf16(&mut b));
        i += 1;
    }
    String::from_utf16_lossy(&out).replace("<T>", dir)
}

pub(crate) fn enc(s: &str, dir: &str) -> String {
    let s = s.replace(dir, "<T>");
    let mut out = String::new();
    for c in s.encode_utf16() {
        if (32..=126).contains(&c) && c != 0x25 && c != 0x27 {
            out.push(c as u8 as char);
        } else if c < 256 {
            out.push_str(&format!("%{c:02X}"));
        } else {
            out.push_str(&format!("%u{c:04X}"));
        }
    }
    out
}

pub(crate) fn from_hex(h: &str) -> Vec<u8> {
    (0..h.len() / 2)
        .map(|i| u8::from_str_radix(&h[2 * i..2 * i + 2], 16).unwrap())
        .collect()
}

pub(crate) fn to_hex(b: &[u8]) -> String {
    b.iter().map(|x| format!("{x:02X}")).collect()
}

fn variant_array(items: Vec<Variant>) -> Variant {
    Variant::Array(Box::new(SafeArray {
        elem: VarType::Variant,
        bounds: vec![(0, items.len() as u32)],
        data: items,
    }))
}

pub(crate) fn byte_array(b: &[u8]) -> Variant {
    Variant::Array(Box::new(SafeArray {
        elem: VarType::Ui1,
        bounds: vec![(0, b.len() as u32)],
        data: b.iter().map(|&x| Variant::Ui1(x)).collect(),
    }))
}

pub(crate) fn dotnet_r(v: f64) -> String {
    if v == 0.0 {
        return "0".into();
    }
    dotnet_e(&format!("{v:e}"), 15)
}

fn dotnet_r32(v: f32) -> String {
    if v == 0.0 {
        return "0".into();
    }
    dotnet_e(&format!("{v:e}"), 9)
}

fn dotnet_e(e: &str, wide: i32) -> String {
    let (mant, exp) = e.split_once('e').unwrap();
    let exp: i32 = exp.parse().unwrap();
    let neg = mant.starts_with('-');
    let digits: String = mant.chars().filter(char::is_ascii_digit).collect();
    let sign = if neg { "-" } else { "" };
    if !(-5..wide).contains(&exp) {
        let frac = &digits[1..];
        let dot = if frac.is_empty() {
            String::new()
        } else {
            format!(".{frac}")
        };
        let es = if exp < 0 { '-' } else { '+' };
        return format!("{sign}{}{dot}E{es}{:02}", &digits[..1], exp.abs());
    }
    if exp >= 0 {
        let int_len = exp as usize + 1;
        if digits.len() <= int_len {
            format!("{sign}{digits}{}", "0".repeat(int_len - digits.len()))
        } else {
            format!("{sign}{}.{}", &digits[..int_len], &digits[int_len..])
        }
    } else {
        format!("{sign}0.{}{digits}", "0".repeat((-exp - 1) as usize))
    }
}

fn decimal_text(negative: bool, scaled: u128, scale: u32) -> String {
    let mut s = scaled.to_string();
    if scale > 0 {
        while s.len() <= scale as usize {
            s.insert(0, '0');
        }
        s.insert(s.len() - scale as usize, '.');
        let t = s.trim_end_matches('0').trim_end_matches('.');
        s = t.to_string();
    }
    if negative && s != "0" {
        s.insert(0, '-');
    }
    s
}

struct World<'a> {
    app: Application<'a>,
    dir: String,
    vars: HashMap<String, Variant>,
    roots: HashMap<&'static str, ObjectId>,
}

impl World<'_> {
    fn show(&mut self, v: &Variant) -> String {
        match v {
            Variant::Empty => "-".into(),
            Variant::Null => "Null".into(),
            Variant::Bool(b) => if *b { "True" } else { "False" }.into(),
            Variant::Bstr(s) => format!("'{}'", enc(s, &self.dir)),
            Variant::BstrNull => "''".into(),
            Variant::I2(n) => format!("I2:{n}"),
            Variant::I4(n) => format!("I4:{n}"),
            Variant::I8(n) => format!("I8:{n}"),
            Variant::Ui1(n) => format!("[U1:{n}]"),
            Variant::R8(x) => format!("R8:{}", dotnet_r(*x)),
            Variant::R4(x) => format!("R4:{}", dotnet_r32(*x)),
            Variant::Cy(n) => format!("Dec:{}", decimal_text(*n < 0, n.unsigned_abs().into(), 4)),
            Variant::Decimal(d) => format!(
                "Dec:{}",
                decimal_text(d.negative, d.mantissa, d.scale.into())
            ),
            Variant::Date(x) => format!("Date:{}", crate::adors::r8_text(*x)),
            Variant::Dispatch(Some(id)) => format!("obj:{}", self.app.com_type_name(*id)),
            Variant::Array(a) => {
                if a.elem == VarType::Ui1 && a.bounds.len() == 1 {
                    let b: Vec<u8> = a
                        .data
                        .iter()
                        .map(|x| if let Variant::Ui1(b) = x { *b } else { 0 })
                        .collect();
                    return format!("B:{}", to_hex(&b));
                }
                if a.bounds.len() == 2 {
                    let (nf, nr) = (a.bounds[0].1 as usize, a.bounds[1].1 as usize);
                    let rows: Vec<String> = (0..nf)
                        .map(|f| {
                            (0..nr)
                                .map(|r| self.show(&a.data[f + nf * r]))
                                .collect::<Vec<_>>()
                                .join(",")
                        })
                        .collect();
                    return format!("[{}]", rows.join(";"));
                }
                let items: Vec<String> = a.data.iter().map(|x| self.show(x)).collect();
                format!("[{}]", items.join("|"))
            }
            other => format!("?{other:?}"),
        }
    }

    fn arg(&mut self, tok: &str) -> Variant {
        let tok = tok.trim();
        if let Some(inner) = tok.strip_prefix('\'') {
            return Variant::Bstr(dec(&inner[..inner.len() - 1], &self.dir));
        }
        if tok.starts_with('$') {
            return self.vars[tok].clone();
        }
        if let Some(r) = tok.strip_prefix('@') {
            return Variant::Dispatch(Some(self.roots[r]));
        }
        if let Some(inner) = tok.strip_prefix('[') {
            let inner = &inner[..inner.len() - 1];
            let items = if inner.is_empty() {
                Vec::new()
            } else {
                split_top(inner, '|').iter().map(|p| self.arg(p)).collect()
            };
            return variant_array(items);
        }
        match tok {
            "T" => return Variant::Bool(true),
            "F" => return Variant::Bool(false),
            "null" => return Variant::Null,
            "empty" => return Variant::Empty,
            "miss" => return Variant::Error(HResult::DISP_E_PARAMNOTFOUND.0),
            _ => {}
        }
        if let Some(x) = tok.strip_prefix("i2:") {
            return Variant::I2(x.parse().unwrap());
        }
        if let Some(x) = tok.strip_prefix("r4:") {
            return Variant::R4(x.parse().unwrap());
        }
        if let Some(x) = tok.strip_prefix("cy:") {
            return decimal_from_str(x);
        }
        if let Some(x) = tok.strip_prefix("dt:") {
            return Variant::Date(x.parse().unwrap());
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
        match self.roots.get(n) {
            Some(id) => Variant::Dispatch(Some(*id)),
            None => self.vars[n].clone(),
        }
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
                return Err(VbaError::new(91));
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
        let (code, text) = match e.number {
            13 => (
                0x8002_0005_u32,
                "Type mismatch. (Exception from HRESULT: 0x80020005 (DISP_E_TYPEMISMATCH))"
                    .to_string(),
            ),
            438 => (
                0x8002_0006,
                "Unknown name. (Exception from HRESULT: 0x80020006 (DISP_E_UNKNOWNNAME))"
                    .to_string(),
            ),
            450 => return "!N:0x8002000E".into(),
            n if (n as u32) >= 0x8000_0000 => (n as u32, e.description.clone()),
            n => return format!("!VBA{n}"),
        };
        format!("!0x{code:08X} {}", enc(&text, &self.dir))
    }

    fn step(&mut self, st: &str) -> String {
        let st = st.trim();
        if let Some(p) = st.strip_prefix("put ") {
            let (name, hex) = p.split_once(' ').unwrap_or((p, ""));
            fs::write(Path::new(&self.dir).join(name), from_hex(hex)).unwrap();
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
        let run = |w: &mut Self| -> Result<String, VbaError> {
            if let Some(p) = st.strip_prefix("each ") {
                let v = w.eval(p.trim(), None)?;
                let Variant::Dispatch(Some(id)) = v else {
                    return Err(VbaError::new(438));
                };
                let items = w.app.com_enumerate(id)?;
                let shown: Vec<String> = items.iter().map(|x| w.show(x)).collect();
                return Ok(format!("{{{}}}", shown.join(",")));
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
                let v = w.arg(&parts[1..].join("="));
                w.eval(&parts[0], Some(v))?;
                return Ok(".".into());
            }
            let res = w.eval(body, None)?;
            if let Some(n) = var {
                w.vars.insert(n.clone(), res);
                return Ok(format!(".{n}"));
            }
            Ok(w.show(&res))
        };
        match run(self) {
            Ok(s) => s,
            Err(e) => self.error_text(&e),
        }
    }

    fn run_case(&mut self, case: &str) -> String {
        self.vars.clear();
        for (n, prog) in [
            ("s", "ADODB.Stream"),
            ("t", "ADODB.Stream"),
            ("r", "ADODB.Recordset"),
            ("r2", "ADODB.Recordset"),
        ] {
            let id = self.app.create_com_object(prog).unwrap();
            self.roots.insert(n, id);
        }
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

fn golden() -> String {
    let p = PathBuf::from(env!("CARGO_MANIFEST_DIR"))
        .join("../../conformance/com/adodb_probe.golden.txt");
    fs::read_to_string(p).unwrap()
}

#[test]
fn adodb_golden() {
    let dir = std::env::temp_dir().join(format!("adoreplay_{}", std::process::id()));
    fs::create_dir_all(&dir).unwrap();
    let dir_s = dir.to_string_lossy().to_string();
    let mut w = World {
        app: Application::new(),
        dir: dir_s,
        vars: HashMap::new(),
        roots: HashMap::new(),
    };
    let text = golden();
    let (mut total, mut bad) = (0, Vec::new());
    for line in text.lines().filter(|l| !l.is_empty()) {
        let (case, want) = line.split_once(" => ").unwrap();
        total += 1;
        if std::env::var("ADO_TRACE").is_ok() {
            eprintln!("{case}");
        }
        let got = w.run_case(case);
        if got != want {
            bad.push(format!("{case}\n  want {want}\n  got  {got}"));
        }
    }
    let _ = fs::remove_dir_all(&dir);
    let shown: Vec<&String> = bad
        .iter()
        .take(
            std::env::var("ADO_SHOW")
                .ok()
                .and_then(|s| s.parse().ok())
                .unwrap_or(12),
        )
        .collect();
    assert!(
        bad.is_empty(),
        "{} of {total} golden cases differ:\n{}",
        bad.len(),
        shown
            .iter()
            .map(|s| s.as_str())
            .collect::<Vec<_>>()
            .join("\n")
    );
}

fn decimal_from_str(s: &str) -> Variant {
    let negative = s.starts_with('-');
    let t = s.trim_start_matches('-');
    let (int, frac) = t.split_once('.').unwrap_or((t, ""));
    Variant::Decimal(xlcalc_variant::Decimal {
        negative,
        scale: frac.len() as u8,
        mantissa: format!("{int}{frac}").parse().unwrap(),
    })
}
