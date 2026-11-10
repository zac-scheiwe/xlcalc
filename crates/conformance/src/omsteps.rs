use std::collections::HashMap;

use serde_json::Value as J;
use xlcalc::Workbook;
use xlcalc_om::{flags, Application, BookId, Call, OmError};
use xlcalc_variant::{Decimal, HResult, ObjectId, SafeArray, VarType, Variant};

use super::{Outcome, Tally};

thread_local! {
    static SHEETS_IN_NEW: std::cell::Cell<u32> = const { std::cell::Cell::new(1) };
}

pub struct OmSession {
    pub(crate) app: Application<'static>,
    pub(crate) book: BookId,
    held: HashMap<String, ObjectId>,

    pub(crate) temps: Vec<ObjectId>,

    pub(crate) out: std::path::PathBuf,

    pub(crate) sink_log: super::comsteps::SinkLog,
}

impl Drop for OmSession {
    fn drop(&mut self) {
        super::filesteps::remove_out(&self.out);
    }
}

struct Fail {
    hr: i32,

    scode: i32,
    arg_err: Option<u32>,
}

impl From<OmError> for Fail {
    fn from(e: OmError) -> Fail {
        Fail {
            hr: e.hresult.0,
            scode: e.scode,
            arg_err: e.arg_err,
        }
    }
}

impl Fail {
    fn hr(hr: u32) -> Fail {
        Fail {
            hr: hr as i32,
            scode: 0,
            arg_err: None,
        }
    }
}

fn hex(s: &str) -> Option<u64> {
    u64::from_str_radix(s.trim_start_matches("0x"), 16).ok()
}

fn hex_i32(j: &J) -> Option<i32> {
    match j {
        J::String(s) => hex(s).map(|v| v as u32 as i32),
        J::Number(n) => n.as_i64().map(|v| v as i32),
        _ => None,
    }
}

fn decode(j: &J, session: &mut OmSession) -> Result<Variant, Fail> {
    match j {
        J::Null => Ok(Variant::Empty),
        J::Bool(b) => Ok(Variant::Bool(*b)),
        J::Number(n) => Ok(
            match n
                .as_i64()
                .filter(|_| !n.is_f64())
                .and_then(|i| i32::try_from(i).ok())
            {
                Some(i) => Variant::I4(i),
                None => Variant::R8(n.as_f64().unwrap_or(0.0)),
            },
        ),

        J::String(s) => Ok(Variant::Bstr(match s.strip_prefix("@repo/") {
            Some(rest) => super::root().join(rest).to_string_lossy().to_string(),
            None => s.clone(),
        })),
        J::Array(_) => Err(Fail::hr(0x8002_0006)),
        J::Object(o) => {
            if o.get("missing").is_some() {
                return Ok(Variant::Error(HResult::DISP_E_PARAMNOTFOUND.0));
            }
            if let Some(p) = o.get("path").and_then(J::as_str) {
                return session.eval_path(p).map(|id| Variant::Dispatch(Some(id)));
            }
            if let Some(rows) = o.get("array").and_then(J::as_array) {
                let nrows = rows.len() as u32;
                let ncols = rows.first().and_then(J::as_array).map_or(0, Vec::len) as u32;
                let mut data = Vec::new();
                for c in 0..ncols as usize {
                    for r in rows {
                        let cell = &r[c];
                        data.push(decode(cell, session)?);
                    }
                }
                return Ok(Variant::Array(Box::new(SafeArray {
                    elem: VarType::Variant,
                    bounds: vec![(1, nrows), (1, ncols)],
                    data,
                })));
            }
            let vt = o.get("vt").and_then(J::as_str).unwrap_or("");
            let v = o.get("v").unwrap_or(&J::Null);
            let int = |v: &J| -> i64 {
                v.as_i64()
                    .or_else(|| v.as_str().and_then(|s| s.parse().ok()))
                    .unwrap_or(0)
            };
            let bits = o.get("bits").and_then(J::as_str).and_then(hex);
            Ok(match vt {
                "EMPTY" => Variant::Empty,
                "NULL" => Variant::Null,
                "I2" => Variant::I2(int(v) as i16),
                "I4" => Variant::I4(int(v) as i32),
                "UI1" => Variant::Ui1(int(v) as u8),
                "I8" => Variant::I8(int(v)),
                "R4" => Variant::R4(match bits {
                    Some(b) => f32::from_bits(b as u32),
                    None => v.as_f64().unwrap_or(0.0) as f32,
                }),
                "R8" => Variant::R8(match bits {
                    Some(b) => f64::from_bits(b),
                    None => v.as_f64().unwrap_or(0.0),
                }),
                "DATE" => Variant::Date(match bits {
                    Some(b) => f64::from_bits(b),
                    None => v.as_f64().unwrap_or(0.0),
                }),
                "CY" => Variant::Cy(match v {
                    J::String(s) if s.contains('.') => {
                        let x: f64 = s.parse().unwrap_or(0.0);
                        (x * 10_000.0).round() as i64
                    }
                    _ if o.contains_key("text") => int(v),

                    other => int(other) * if bits.is_some() { 1 } else { 10_000 },
                }),
                "BOOL" => Variant::Bool(v.as_bool().unwrap_or(false)),
                "BSTR" if v.is_null() => Variant::BstrNull,
                "BSTR" => Variant::Bstr(v.as_str().unwrap_or("").to_string()),
                "ERROR" => Variant::Error(hex_i32(v).unwrap_or(0)),
                "DECIMAL" if v.is_string() => {
                    let text = v.as_str().unwrap_or("0");
                    let (negative, body) = match text.strip_prefix('-') {
                        Some(b) => (true, b),
                        None => (false, text),
                    };
                    let (whole, frac) = body.split_once('.').unwrap_or((body, ""));
                    Variant::Decimal(Decimal {
                        negative,
                        scale: frac.len() as u8,
                        mantissa: format!("{whole}{frac}").parse().unwrap_or(0),
                    })
                }
                "DECIMAL" => Variant::Decimal(Decimal {
                    negative: o.get("negative").and_then(J::as_bool).unwrap_or(false),
                    scale: int(o.get("scale").unwrap_or(&J::Null)) as u8,
                    mantissa: o
                        .get("mantissa")
                        .map(|m| match m {
                            J::String(s) => s.parse().unwrap_or(0),
                            other => other.as_u64().unwrap_or(0).into(),
                        })
                        .unwrap_or(0),
                }),
                a if a == "ARRAY" || a.starts_with("ARRAY|") => {
                    let elem = match &a[a.len().min(6)..] {
                        "" | "VARIANT" => VarType::Variant,
                        n => [
                            VarType::I2,
                            VarType::I4,
                            VarType::R4,
                            VarType::R8,
                            VarType::Cy,
                            VarType::Date,
                            VarType::Bstr,
                            VarType::Error,
                            VarType::Bool,
                            VarType::Decimal,
                            VarType::Ui1,
                            VarType::I8,
                        ]
                        .into_iter()
                        .find(|&t| n.parse::<u16>().ok() == Some(t as u16))
                        .ok_or_else(|| Fail::hr(0x8002_0006))?,
                    };
                    let bounds = o
                        .get("bounds")
                        .and_then(J::as_array)
                        .map(|b| {
                            b.iter()
                                .map(|p| {
                                    (
                                        p[0].as_i64().unwrap_or(1) as i32,
                                        p[1].as_u64().unwrap_or(0) as u32,
                                    )
                                })
                                .collect()
                        })
                        .unwrap_or_default();
                    let mut data = Vec::new();
                    for item in o.get("data").and_then(J::as_array).into_iter().flatten() {
                        data.push(decode(item, session)?);
                    }
                    Variant::Array(Box::new(SafeArray { elem, bounds, data }))
                }
                _ => return Err(Fail::hr(0x8002_0006)),
            })
        }
    }
}

fn split_segments(path: &str) -> Vec<String> {
    let (mut out, mut cur) = (Vec::new(), String::new());
    let (mut depth, mut quoted) = (0i32, false);
    for c in path.chars() {
        match c {
            '\'' => {
                quoted = !quoted;
                cur.push(c);
            }
            '(' | '[' if !quoted => {
                depth += 1;
                cur.push(c);
            }
            ')' | ']' if !quoted => {
                depth -= 1;
                cur.push(c);
            }
            '.' if !quoted && depth == 0 => out.push(std::mem::take(&mut cur)),
            _ => cur.push(c),
        }
    }
    out.push(cur);
    out
}

fn parse_segment(seg: &str) -> Option<(String, Vec<Variant>)> {
    let Some(open) = seg.find('(') else {
        return Some((seg.to_string(), Vec::new()));
    };
    let body = seg[open..].strip_prefix('(')?.strip_suffix(')')?;
    Some((seg[..open].to_string(), parse_args(body)?))
}

fn parse_args(body: &str) -> Option<Vec<Variant>> {
    let mut args = Vec::new();
    let chars: Vec<char> = body.chars().collect();
    let mut i = 0;
    while i < chars.len() {
        while chars.get(i) == Some(&' ') {
            i += 1;
        }
        if i >= chars.len() {
            break;
        }
        let start = i;
        if chars[i] == '\'' {
            i += 1;
            let mut s = String::new();
            loop {
                match chars.get(i) {
                    None => return None,
                    Some('\'') if chars.get(i + 1) == Some(&'\'') => {
                        s.push('\'');
                        i += 2;
                    }
                    Some('\'') => {
                        i += 1;
                        break;
                    }
                    Some(c) => {
                        s.push(*c);
                        i += 1;
                    }
                }
            }
            args.push(Variant::Bstr(s));
        } else if chars[i] == '[' {
            let close = body.rfind(']').filter(|_| chars.last() == Some(&']'))?;
            let inner: String = chars[i + 1..close.min(chars.len())].iter().collect();
            let items = parse_args(&inner)?;
            args.push(Variant::Array(Box::new(SafeArray {
                elem: VarType::Variant,
                bounds: vec![(0, items.len() as u32)],
                data: items,
            })));
            i = chars.len();
        } else {
            while i < chars.len() && chars[i] != ',' {
                i += 1;
            }
            let lit: String = chars[start..i]
                .iter()
                .collect::<String>()
                .trim()
                .to_string();
            args.push(match lit.as_str() {
                "True" => Variant::Bool(true),
                "False" => Variant::Bool(false),
                _ => match lit.parse::<i32>() {
                    Ok(n) => Variant::I4(n),
                    Err(_) => Variant::R8(lit.parse().ok()?),
                },
            });
        }
        while chars.get(i) == Some(&' ') {
            i += 1;
        }
        match chars.get(i) {
            Some(',') => i += 1,
            None => {}
            Some(_) => return None,
        }
    }
    Some(args)
}

impl OmSession {
    pub fn set_sheets_in_new(n: u32) {
        SHEETS_IN_NEW.with(|c| c.set(n));
    }

    pub fn new(wb: &mut Workbook) -> OmSession {
        OmSession::with_numbers(wb, &[], false)
    }

    pub fn fresh(wb: &mut Workbook) -> OmSession {
        OmSession::with_numbers(wb, &Application::AUTOMATION_BOOK_NUMBERS, true)
    }

    fn with_numbers(wb: &mut Workbook, reserved: &[u32], fresh: bool) -> OmSession {
        let sheets = wb.sheet_count() as u32;
        let mut app = Application::new();
        let out = super::filesteps::new_out_dir();
        app.set_file_system(Box::new(super::filesteps::OutFs::new(out.clone())));
        app.reserve_book_numbers(reserved);
        app.set_host(Box::new(RunnerHost));
        let book = app.add_workbook(std::mem::replace(wb, Workbook::empty()));
        app.swap_engine(book, wb);

        if fresh && sheets >= 2 {
            app.set_next_sheet_number(book, sheets + 2);
        }

        app.set_sheets_in_new_workbook(SHEETS_IN_NEW.with(std::cell::Cell::get));

        for name in ["DisplayAlerts", "ScreenUpdating", "EnableEvents"] {
            let _ = app.invoke(app.handle(), name, Call::PUT, &[Variant::Bool(false)], &[]);
        }

        let _ = app.invoke(
            app.handle(),
            "DefaultFilePath",
            Call::PUT,
            &[Variant::Bstr("@out".into())],
            &[],
        );
        if let Ok(Variant::Dispatch(Some(b))) =
            app.invoke(app.handle(), "ActiveWorkbook", Call::GET, &[], &[])
        {
            let _ = app.invoke(b, "Saved", Call::PUT, &[Variant::Bool(false)], &[]);
            app.release(b);
        }
        OmSession {
            app,
            book,
            held: HashMap::new(),
            temps: Vec::new(),
            out,
            sink_log: Default::default(),
        }
    }

    fn eval_path(&mut self, path: &str) -> Result<ObjectId, Fail> {
        let mut segs = split_segments(path).into_iter().peekable();
        let mut cur = self.app.handle();
        if path.is_empty() {
            return Ok(cur);
        }
        if let Some(first) = path.strip_prefix('$') {
            let name = first.split('.').next().unwrap_or("");
            cur = *self.held.get(name).ok_or_else(|| Fail::hr(0x8002_0006))?;
            segs.next();
        }
        for seg in segs {
            let (name, args) = parse_segment(&seg).ok_or_else(|| Fail::hr(0x8002_0006))?;

            let call = Call {
                flags: flags::METHOD | flags::GET,
                lcid: 1033,
            };
            let got = self
                .app
                .invoke(cur, &name, call, &args, &[])
                .map_err(Fail::from)?;
            match got {
                Variant::Dispatch(Some(id)) | Variant::Unknown(Some(id)) => {
                    self.temps.push(id);
                    cur = id;
                }
                _ => {
                    return Err(Fail {
                        hr: 0x800A_01A8_u32 as i32,
                        scode: 0,
                        arg_err: None,
                    })
                }
            }
        }
        Ok(cur)
    }
}

impl OmSession {
    pub(crate) fn eval_handle(&mut self, path: &str) -> Option<ObjectId> {
        self.eval_path(path).ok()
    }
}

pub fn quote(s: &str) -> String {
    format!("'{}'", s.replace('\'', "''"))
}

pub fn ref_path(r: &str) -> String {
    match r.rfind('!') {
        Some(i) => {
            let sheet = r[..i].trim_matches('\'').replace("''", "'");
            format!(
                "Worksheets({}).Range({})",
                quote(&sheet),
                quote(&r[i + 1..])
            )
        }
        None => format!("Range({})", quote(r)),
    }
}

pub enum Arg {
    Val(Variant),
    Path(String),

    Missing,
}

pub fn call(
    om: &mut Option<OmSession>,
    wb: &mut Workbook,
    path: &str,
    member: &str,
    call_flags: u16,
    args: &[Variant],
) -> Result<Variant, String> {
    let args: Vec<Arg> = args.iter().cloned().map(Arg::Val).collect();
    call_args(om, wb, path, member, call_flags, &args)
}

pub fn call_args(
    om: &mut Option<OmSession>,
    wb: &mut Workbook,
    path: &str,
    member: &str,
    call_flags: u16,
    args: &[Arg],
) -> Result<Variant, String> {
    let session = om.get_or_insert_with(|| OmSession::new(wb));
    session.app.swap_engine(session.book, wb);
    let result = (|| {
        let target = session
            .eval_path(path)
            .map_err(|f| format!("path {path}: {}", hr_text(f.hr)))?;
        let mut values = Vec::new();
        for a in args {
            values.push(match a {
                Arg::Val(v) => v.clone(),
                Arg::Missing => Variant::Error(HResult::DISP_E_PARAMNOTFOUND.0),
                Arg::Path(p) => session
                    .eval_path(p)
                    .map(|id| Variant::Dispatch(Some(id)))
                    .map_err(|f| format!("path {p}: {}", hr_text(f.hr)))?,
            });
        }
        let call = Call {
            flags: call_flags,
            lcid: 1033,
        };
        session
            .app
            .invoke(target, member, call, &values, &[])
            .map_err(|e| e.detail.clone().unwrap_or_else(|| e.to_string()))
    })();
    for id in std::mem::take(&mut session.temps) {
        session.app.release(id);
    }
    if let Ok(Variant::Dispatch(Some(id)) | Variant::Unknown(Some(id))) = &result {
        session.app.release(*id);
    }
    session.app.swap_engine(session.book, wb);
    result
}

pub fn verdict(tally: &mut Tally, op: &str, rec: &J, n: usize, err: Option<String>) {
    if let Some(w) = err.as_deref().and_then(super::oos_what) {
        tally.pending_oos = Some(w);
        return;
    }
    let ok = err.is_some() == !rec["error"].is_null();
    tally.add(if ok { Outcome::Exact } else { Outcome::Wrong }, || {
        format!(
            "step {n}: {op} — Excel error {:?}, engine {err:?}",
            rec["error"]
        )
    });
}

fn hr_text(hr: i32) -> String {
    format!("{:08X}", hr as u32)
}

impl OmSession {
    pub fn run(
        &mut self,
        wb: &mut Workbook,
        op: &str,
        step: &J,
        rec: &J,
        n: usize,
        tally: &mut Tally,
    ) -> bool {
        if !matches!(
            op,
            "om" | "om_get" | "om_put" | "om_hold" | "om_enum" | "om_release"
        ) {
            return false;
        }
        if op == "om_release" {
            self.app.swap_engine(self.book, wb);
            if let Some(id) = step["name"].as_str().and_then(|n| self.held.remove(n)) {
                self.app.release(id);
            }
            self.app.swap_engine(self.book, wb);
            tally.add(Outcome::Exact, String::new);
            return true;
        }
        self.app.swap_engine(self.book, wb);
        let outcome = self.step(op, step, rec);
        for id in std::mem::take(&mut self.temps) {
            self.app.release(id);
        }
        self.app.swap_engine(self.book, wb);
        match outcome {
            Ok(()) => tally.add(Outcome::Exact, String::new),
            Err(why) => tally.add(Outcome::Wrong, || {
                format!("step {n}: {op} {}: {why}", describe(step))
            }),
        }
        true
    }

    fn step(&mut self, op: &str, step: &J, rec: &J) -> Result<(), String> {
        let lcid = step["lcid"].as_u64().unwrap_or(1033) as u32;
        let path = step["path"].as_str().unwrap_or("");
        if op == "om_enum" {
            return self.enumerate(path, lcid, rec);
        }
        let expected_fail = rec.get("fail").filter(|f| !f.is_null());
        let result: Result<Variant, Fail> = (|| {
            let target = self.eval_path(path)?;
            if op == "om_hold" {
                let name = step["name"].as_str().unwrap_or("").to_string();
                self.temps.retain(|&t| t != target);
                self.held.insert(name, target);
                return Ok(Variant::Dispatch(Some(target)));
            }
            let (member, default_flags) = match op {
                "om" => (step["call"].as_str().unwrap_or(""), flags::METHOD),
                "om_get" => (step["prop"].as_str().unwrap_or(""), flags::GET),
                _ => (step["prop"].as_str().unwrap_or(""), flags::PUT),
            };
            let call = Call {
                flags: step["flags"].as_u64().map_or(default_flags, |f| f as u16),
                lcid,
            };
            let mut positional = Vec::new();
            for a in step["args"].as_array().into_iter().flatten() {
                positional.push(decode(a, self)?);
            }
            if op == "om_put" {
                positional.push(decode(&step["value"], self)?);
            }
            let mut named = Vec::new();
            for (k, v) in step["named"].as_object().into_iter().flatten() {
                named.push((k.as_str(), decode(v, self)?));
            }
            self.app
                .invoke(target, member, call, &positional, &named)
                .map_err(Fail::from)
        })();
        match (result, expected_fail) {
            (Ok(v), None) => {
                let ok = self.check_result(&v, rec, op);
                if let Variant::Dispatch(Some(id)) | Variant::Unknown(Some(id)) = v {
                    if op != "om_hold" {
                        self.app.release(id);
                    }
                }
                ok
            }
            (Ok(_), Some(f)) => Err(format!("Excel failed ({}), xlcalc succeeded", f["hr"])),
            (Err(e), None) => Err(format!(
                "Excel returned {}, xlcalc failed {} scode {}",
                short_json(&rec["result"]),
                hr_text(e.hr),
                hr_text(e.scode)
            )),
            (Err(e), Some(f)) => {
                let want_hr = hex_i32(&f["hr"]).unwrap_or(0);
                let want_scode = hex_i32(&f["excep"]["scode"]).unwrap_or(0);
                let want_arg = f["arg_err"].as_u64().map(|a| a as u32);
                if e.hr == want_hr
                    && e.scode == want_scode
                    && (want_arg.is_none() || e.arg_err == want_arg)
                {
                    Ok(())
                } else {
                    Err(format!(
                        "Excel failed {} scode {} arg {:?}, xlcalc failed {} scode {} arg {:?}",
                        hr_text(want_hr),
                        hr_text(want_scode),
                        want_arg,
                        hr_text(e.hr),
                        hr_text(e.scode),
                        e.arg_err
                    ))
                }
            }
        }
    }

    fn enumerate(&mut self, path: &str, lcid: u32, rec: &J) -> Result<(), String> {
        let got = (|| {
            let target = self.eval_path(path)?;
            let call = Call {
                flags: flags::METHOD | flags::GET,
                lcid,
            };
            match self.app.invoke(target, "_NewEnum", call, &[], &[])? {
                Variant::Unknown(Some(e)) | Variant::Dispatch(Some(e)) => {
                    let items = self.app.enum_next(e, usize::MAX);
                    self.app.release(e);
                    Ok(items)
                }
                _ => Err(Fail::from(OmError::app_defined())),
            }
        })();
        let (items, want) = match (got, rec["items"].as_array()) {
            (Ok(items), Some(want)) => (items, want),
            (Ok(items), None) => {
                self.release_all(&items);
                return Err(format!(
                    "Excel failed ({}), xlcalc succeeded",
                    rec["fail"]["hr"]
                ));
            }
            (Err(e), Some(_)) => return Err(format!("xlcalc failed {}", hr_text(e.hr))),
            (Err(_), None) => return Ok(()),
        };
        let mut result = if items.len() == want.len() {
            Ok(())
        } else {
            Err(format!(
                "Excel enumerated {} items, xlcalc {}",
                want.len(),
                items.len()
            ))
        };
        if result.is_ok() {
            for (i, (g, w)) in items.iter().zip(want).enumerate() {
                if let Err(e) = self.check_object(g, w) {
                    result = Err(format!("item {}: {e}", i + 1));
                    break;
                }
            }
        }
        self.release_all(&items);
        result
    }

    fn release_all(&mut self, items: &[Variant]) {
        for v in items {
            if let Variant::Dispatch(Some(id)) | Variant::Unknown(Some(id)) = v {
                self.app.release(*id);
            }
        }
    }

    fn check_result(&mut self, got: &Variant, rec: &J, op: &str) -> Result<(), String> {
        if op == "om_hold" {
            return Ok(());
        }
        let want = &rec["result"];
        if want["vt"] == "DISPATCH" {
            return self.check_object(got, want);
        }

        if want["vt"] == "UNKNOWN" {
            return match (got, want["v"].is_null()) {
                (Variant::Unknown(Some(_)), false) | (Variant::Unknown(None), true) => Ok(()),
                _ => Err(format!("expected {want}, got {got:?}")),
            };
        }
        let expected = decode(want, self).map_err(|_| "unreadable recorded result".to_string())?;

        let stripped;
        let got = match got {
            Variant::Bstr(s) if rec["op"] == "run" => {
                stripped = Variant::Bstr(xlcalc_vba::rt::host_text(s));
                &stripped
            }
            g => g,
        };
        if *got == expected {
            Ok(())
        } else {
            Err(format!("expected {expected:?}, got {got:?}"))
        }
    }

    fn check_object(&mut self, got: &Variant, want: &J) -> Result<(), String> {
        let id = match got {
            Variant::Dispatch(Some(id)) | Variant::Unknown(Some(id)) => *id,
            Variant::Dispatch(None) | Variant::Unknown(None) if want["v"].is_null() => {
                return Ok(())
            }
            other => return Err(format!("expected an object, got {other:?}")),
        };
        if want["v"].is_null() && want.get("type").is_none() {
            return Err("expected Nothing, got an object".to_string());
        }
        let ty = self.app.type_name(id).unwrap_or("?");
        if let Some(t) = want["type"].as_str() {
            if t != ty {
                return Err(format!("expected a {t}, got a {ty}"));
            }
        }
        let ask = |this: &mut OmSession, member: &str, args: &[Variant]| match this.app.invoke(
            id,
            member,
            Call::GET,
            args,
            &[],
        ) {
            Ok(Variant::Bstr(s)) => Ok(s),
            other => Err(format!("{member} gave {other:?}")),
        };
        if let Some(a) = want["address"].as_str() {
            let args = [
                Variant::Bool(true),
                Variant::Bool(true),
                Variant::I4(1),
                Variant::Bool(true),
            ];
            let text = ask(self, "Address", &args)?;
            let normalized = match text.strip_prefix('[') {
                Some(rest) => format!("[wb]{}", rest.split_once(']').map_or(rest, |x| x.1)),
                None => text.clone(),
            };
            if normalized != a {
                return Err(format!("expected address {a}, got {text}"));
            }
        }
        if let Some(name) = want["name"].as_str() {
            let got = ask(self, "Name", &[])?;
            if got != name {
                return Err(format!("expected name {name}, got {got}"));
            }
        }
        Ok(())
    }
}

fn short_json(j: &J) -> String {
    let s = j.to_string();
    if s.chars().count() > 120 {
        format!("{}...", s.chars().take(120).collect::<String>())
    } else {
        s
    }
}

fn describe(step: &J) -> String {
    let member = step["call"]
        .as_str()
        .or(step["prop"].as_str())
        .unwrap_or("");
    format!("{}.{member}", step["path"].as_str().unwrap_or(""))
}

pub const MACHINE_LCID: u32 = 0x1409;

impl OmSession {
    pub fn with<R>(&mut self, wb: &mut Workbook, f: impl FnOnce(&mut OmSession) -> R) -> R {
        self.app.swap_engine(self.book, wb);
        let r = f(self);
        for id in std::mem::take(&mut self.temps) {
            self.app.release(id);
        }
        self.app.swap_engine(self.book, wb);
        r
    }

    pub fn call(
        &mut self,
        target: ObjectId,
        member: &str,
        flags: u16,
        args: &[Variant],
    ) -> Result<Variant, OmError> {
        let call = Call {
            flags,
            lcid: MACHINE_LCID,
        };
        let got = self.app.invoke(target, member, call, args, &[])?;
        if let Variant::Dispatch(Some(id)) | Variant::Unknown(Some(id)) = got {
            self.temps.push(id);
        }
        Ok(got)
    }

    pub fn app_handle(&self) -> ObjectId {
        self.app.handle()
    }

    pub fn object(
        &mut self,
        target: ObjectId,
        member: &str,
        args: &[Variant],
    ) -> Result<ObjectId, OmError> {
        match self.call(target, member, flags::METHOD | flags::GET, args)? {
            Variant::Dispatch(Some(id)) => Ok(id),
            _ => Err(OmError::object_required()),
        }
    }

    pub fn range(&mut self, sheet: &str, a1: &str) -> Result<ObjectId, OmError> {
        let app = self.app.handle();
        let ws = self.object(app, "Worksheets", &[Variant::Bstr(sheet.to_string())])?;
        self.object(ws, "Range", &[Variant::Bstr(a1.to_string())])
    }

    pub fn workbook(&mut self) -> Result<ObjectId, OmError> {
        let app = self.app.handle();
        self.object(app, "ActiveWorkbook", &[])
    }

    pub fn put(&mut self, target: ObjectId, member: &str, value: Variant) -> Result<(), OmError> {
        self.call(target, member, flags::PUT, &[value]).map(|_| ())
    }

    pub fn get(&mut self, target: ObjectId, member: &str) -> Result<Variant, OmError> {
        self.call(target, member, flags::GET, &[])
    }

    pub fn method(&mut self, target: ObjectId, member: &str) -> Result<Variant, OmError> {
        self.call(target, member, flags::METHOD, &[])
    }

    pub fn address(&mut self, target: ObjectId) -> Result<String, OmError> {
        let args = [Variant::Bool(false), Variant::Bool(false)];
        match self.call(target, "Address", flags::GET | flags::METHOD, &args)? {
            Variant::Bstr(s) => Ok(s),
            _ => Err(OmError::object_required()),
        }
    }
}

struct RunnerHost;

impl xlcalc_om::Host for RunnerHost {
    fn run_open_macros(&self) -> bool {
        true
    }

    fn debug_print(&mut self, line: &str) {
        eprintln!("host: Debug.Print {line}");
    }

    fn on_time(&mut self, request: &xlcalc_om::OnTimeRequest, schedule: bool) {
        eprintln!("host: OnTime {request:?} schedule={schedule}");
    }

    fn unhandled_error(&mut self, error: &xlcalc_om::HandlerError) {
        eprintln!(
            "host: unhandled error {} in {}: {}",
            error.number, error.procedure, error.description
        );
    }
}

impl OmSession {
    pub fn vba_module(
        &mut self,
        wb: &mut Workbook,
        step: &J,
        rec: &J,
        check: bool,
    ) -> Result<(), String> {
        let file = step["file"].as_str().unwrap_or("");
        let path = super::root().join("conformance/vba").join(file);
        let source = std::fs::read_to_string(&path).map_err(|e| format!("{file}: {e}"))?;
        let name = source
            .lines()
            .find_map(|l| {
                let rest = l.strip_prefix("Attribute VB_Name")?;
                Some(rest.trim().trim_start_matches('=').trim().trim_matches('"'))
            })
            .unwrap_or("Module1")
            .to_string();
        if let Some(want) = rec["module"].as_str() {
            if want != name {
                return Err(format!("module named {name}, Excel named it {want}"));
            }
        }
        let kind = if file.to_ascii_lowercase().ends_with(".cls") {
            xlcalc_vba::ModuleKind::Class
        } else {
            xlcalc_vba::ModuleKind::Standard
        };
        let module = xlcalc_vba::SourceModule { name, kind, source };
        let book = self.book;
        let errors = self
            .with(wb, |s| s.app.add_vba_module(book, module))
            .map_err(|e| e.to_string())?;
        match errors.first().filter(|_| check) {
            None => Ok(()),
            Some(e) => Err(format!("does not compile: {e}")),
        }
    }

    pub fn vba_doc_module(
        &mut self,
        wb: &mut Workbook,
        step: &J,
        rec: &J,
        check: bool,
    ) -> Result<(), String> {
        let file = step["file"].as_str().unwrap_or("");
        let module = step["module"].as_str().unwrap_or("");
        let path = super::root().join("conformance/vba").join(file);
        let source = std::fs::read_to_string(&path).map_err(|e| format!("{file}: {e}"))?;
        let book = self.book;
        let (name, errors) = self
            .with(wb, |s| s.app.add_document_module(book, module, &source))
            .map_err(|e| e.to_string())?;
        if let Some(want) = rec["module"].as_str() {
            if !want.eq_ignore_ascii_case(&name) {
                return Err(format!("module named {name}, Excel named it {want}"));
            }
        }
        match errors.first().filter(|_| check) {
            None => Ok(()),
            Some(e) => Err(format!("does not compile: {e}")),
        }
    }

    pub fn run_macro(&mut self, wb: &mut Workbook, step: &J, rec: &J) -> Result<(), String> {
        let mut args = vec![step["name"].clone()];
        args.extend(step["args"].as_array().into_iter().flatten().cloned());
        let call = serde_json::json!({ "path": "", "call": "Run", "args": args, "flags": 1 });
        self.app.swap_engine(self.book, wb);
        let outcome = self.step("om", &call, rec);
        for id in std::mem::take(&mut self.temps) {
            self.app.release(id);
        }
        self.app.swap_engine(self.book, wb);
        outcome
    }
}
