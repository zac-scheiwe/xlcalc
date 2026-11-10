use std::sync::Arc;

use xlcalc_variant::{change_type, flags, Locale, VarType, Variant};
use xlcalc_vba::{InvokeKind, VbaError};

use crate::comobj::ComObj;
use crate::vbregex::{Match, Regex};

#[derive(Debug, Default)]
pub(crate) struct RegExp {
    pattern: String,
    global: bool,
    ignore_case: bool,
    multi_line: bool,
}

#[derive(Debug)]
pub(crate) struct MatchSet {
    input: Vec<u16>,
    matches: Vec<Match>,
}

impl MatchSet {
    fn text(&self, from: usize, to: usize) -> Variant {
        Variant::Bstr(String::from_utf16_lossy(&self.input[from..to]))
    }
}

fn text_arg(v: &Variant) -> Result<String, VbaError> {
    match v.clone().canon() {
        Variant::Bstr(s) => Ok(s),
        Variant::Empty => Ok(String::new()),
        Variant::Null => Err(VbaError::new(13)),
        other => match change_type(&other, VarType::Bstr, Locale::EN_US, flags::ALPHABOOL) {
            Ok(Variant::Bstr(s)) => Ok(s),
            _ => Err(VbaError::new(13)),
        },
    }
}

fn bool_arg(v: &Variant) -> Result<bool, VbaError> {
    match v.clone().canon() {
        Variant::Empty => Ok(false),
        Variant::Null => Err(VbaError::new(13)),
        other => match change_type(&other, VarType::Bool, Locale::EN_US, flags::ALPHABOOL) {
            Ok(Variant::Bool(b)) => Ok(b),
            _ => Err(VbaError::new(13)),
        },
    }
}

fn index_arg(v: &Variant, len: usize) -> Result<usize, VbaError> {
    match change_type(
        &v.clone().canon(),
        VarType::I4,
        Locale::EN_US,
        flags::ALPHABOOL,
    ) {
        Ok(Variant::I4(n)) => usize::try_from(n)
            .ok()
            .filter(|&n| n < len)
            .ok_or_else(|| VbaError::new(5)),
        _ => Err(VbaError::new(13)),
    }
}

fn arg_count(args: &[Variant], n: usize) -> Result<(), VbaError> {
    if args.len() == n {
        Ok(())
    } else {
        Err(VbaError::new(450))
    }
}

impl RegExp {
    fn compile(&self) -> Result<Regex, VbaError> {
        let pat: Vec<u16> = self.pattern.encode_utf16().collect();
        Regex::new(&pat, self.ignore_case, self.multi_line).map_err(|e| VbaError::new(e.number()))
    }

    fn matches(&self, subject: &Variant) -> Result<MatchSet, VbaError> {
        let input: Vec<u16> = text_arg(subject)?.encode_utf16().collect();
        let re = self.compile()?;
        let matches = re.exec(&input, self.global);
        Ok(MatchSet { input, matches })
    }
}

impl crate::Application<'_> {
    pub(crate) fn regexp_call(
        &mut self,
        i: usize,
        member: &str,
        kind: InvokeKind,
        args: &[Variant],
    ) -> Result<Variant, VbaError> {
        let Some(ComObj::RegExp(re)) = self.com.get_mut(i) else {
            return Err(VbaError::new(438));
        };
        let put = matches!(kind, InvokeKind::Put | InvokeKind::PutRef);
        let name = member.to_ascii_lowercase();
        match name.as_str() {
            "pattern" | "global" | "ignorecase" | "multiline" if put => {
                arg_count(args, 1)?;
                match name.as_str() {
                    "pattern" => re.pattern = text_arg(&args[0])?,
                    "global" => re.global = bool_arg(&args[0])?,
                    "ignorecase" => re.ignore_case = bool_arg(&args[0])?,
                    _ => re.multi_line = bool_arg(&args[0])?,
                }
                Ok(Variant::Empty)
            }
            "pattern" => arg_count(args, 0).map(|()| Variant::Bstr(re.pattern.clone())),
            "global" => arg_count(args, 0).map(|()| Variant::Bool(re.global)),
            "ignorecase" => arg_count(args, 0).map(|()| Variant::Bool(re.ignore_case)),
            "multiline" => arg_count(args, 0).map(|()| Variant::Bool(re.multi_line)),
            "test" => {
                arg_count(args, 1)?;
                let input: Vec<u16> = text_arg(&args[0])?.encode_utf16().collect();
                Ok(Variant::Bool(re.compile()?.find_at(&input, 0).is_some()))
            }
            "execute" => {
                arg_count(args, 1)?;
                let set = re.matches(&args[0])?;
                Ok(self.new_com(ComObj::Matches(Arc::new(set))))
            }
            "replace" => {
                arg_count(args, 2)?;
                let input: Vec<u16> = text_arg(&args[0])?.encode_utf16().collect();
                let repl: Vec<u16> = text_arg(&args[1])?.encode_utf16().collect();
                let out = re.compile()?.replace(&input, &repl, re.global);
                Ok(Variant::Bstr(String::from_utf16_lossy(&out)))
            }
            _ => Err(VbaError::new(438)),
        }
    }

    pub(crate) fn matches_call(
        &mut self,
        set: &Arc<MatchSet>,
        member: &str,
        args: &[Variant],
    ) -> Result<Variant, VbaError> {
        match member.to_ascii_lowercase().as_str() {
            "count" => arg_count(args, 0).map(|()| Variant::I4(set.matches.len() as i32)),
            "item" | "" => {
                arg_count(args, 1)?;
                let n = index_arg(&args[0], set.matches.len())?;
                Ok(self.new_com(ComObj::Match(set.clone(), n)))
            }
            _ => Err(VbaError::new(438)),
        }
    }

    pub(crate) fn match_call(
        &mut self,
        set: &Arc<MatchSet>,
        n: usize,
        member: &str,
        args: &[Variant],
    ) -> Result<Variant, VbaError> {
        let m = &set.matches[n];
        match member.to_ascii_lowercase().as_str() {
            "value" | "" => arg_count(args, 0).map(|()| set.text(m.start, m.end)),
            "firstindex" => arg_count(args, 0).map(|()| Variant::I4(m.start as i32)),
            "length" => arg_count(args, 0).map(|()| Variant::I4((m.end - m.start) as i32)),
            "submatches" => {
                arg_count(args, 0)?;
                Ok(self.new_com(ComObj::SubMatches(set.clone(), n)))
            }
            _ => Err(VbaError::new(438)),
        }
    }
}

pub(crate) fn submatch(set: &MatchSet, n: usize, g: usize) -> Variant {
    match set.matches[n].groups[g] {
        Some((a, b)) => set.text(a, b),
        None => Variant::Empty,
    }
}

pub(crate) fn submatches_call(
    set: &MatchSet,
    n: usize,
    member: &str,
    args: &[Variant],
) -> Result<Variant, VbaError> {
    let groups = set.matches[n].groups.len();
    match member.to_ascii_lowercase().as_str() {
        "count" => arg_count(args, 0).map(|()| Variant::I4(groups as i32)),
        "item" | "" => {
            arg_count(args, 1)?;
            Ok(submatch(set, n, index_arg(&args[0], groups)?))
        }
        _ => Err(VbaError::new(438)),
    }
}

impl MatchSet {
    pub(crate) fn len(&self) -> usize {
        self.matches.len()
    }

    pub(crate) fn groups(&self, n: usize) -> usize {
        self.matches[n].groups.len()
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::vbregex::tests::{dec, enc};
    use crate::Application;
    use xlcalc_variant::ObjectId;

    fn arg(s: &str) -> Variant {
        if let Some(n) = s.strip_prefix("#i:") {
            Variant::I4(n.parse().unwrap())
        } else if let Some(x) = s.strip_prefix("#d:") {
            Variant::R8(x.parse().unwrap())
        } else if let Some(b) = s.strip_prefix("#b:") {
            Variant::Bool(b != "0")
        } else if s == "#n" {
            Variant::Null
        } else if s == "#u" {
            Variant::Empty
        } else {
            Variant::Bstr(String::from_utf16_lossy(&dec(s)))
        }
    }

    fn show(r: Result<Variant, VbaError>) -> String {
        match r {
            Err(e) => match e.number {
                13 => "!0x80020005".to_string(),
                450 => "!0x8002000E".to_string(),
                438 => "!0x80020006".to_string(),
                5 => "!0x80070057".to_string(),
                n => format!("!{n}"),
            },
            Ok(Variant::Bool(b)) => if b { "True" } else { "False" }.to_string(),
            Ok(Variant::Bstr(s)) => {
                let u: Vec<u16> = s.encode_utf16().collect();
                format!("'{}'", enc(&u))
            }
            Ok(Variant::Empty) => "<null>".to_string(),
            Ok(Variant::Dispatch(_)) => "obj".to_string(),
            Ok(other) => format!("{other:?}"),
        }
    }

    fn obj(v: Variant) -> ObjectId {
        match v {
            Variant::Dispatch(Some(id)) => id,
            other => panic!("{other:?}"),
        }
    }

    struct Rig {
        app: Application<'static>,
        re: ObjectId,
    }

    impl Rig {
        fn new() -> Rig {
            let mut app = Application::new();
            let re = app.create_com_object("VBScript.RegExp").unwrap();
            Rig { app, re }
        }

        fn get(&mut self, id: ObjectId, m: &str, a: &[Variant]) -> Result<Variant, VbaError> {
            self.app.com_invoke(id, m, InvokeKind::Get, a)
        }

        fn set(&mut self, name: &str, v: Variant) -> Result<Variant, VbaError> {
            self.app.com_invoke(self.re, name, InvokeKind::Put, &[v])
        }

        fn call(&mut self, name: &str, a: &[Variant]) -> Result<Variant, VbaError> {
            self.get(self.re, name, a)
        }

        fn execute(&mut self, f: &[&str], input: &str) -> Result<ObjectId, VbaError> {
            self.set("Pattern", arg(f[2])).unwrap();
            self.set("Global", Variant::Bool(f[1].contains('g')))
                .unwrap();
            self.call("Execute", &[arg(input)]).map(obj)
        }

        fn run(&mut self, case: &str) -> Option<String> {
            let mut f: Vec<&str> = case.split("@@").collect();
            while f.len() < 5 {
                f.push("");
            }
            Some(match f[0] {
                "D" => {
                    let mut out = String::from("Pattern=''");
                    for n in ["Global", "IgnoreCase", "MultiLine"] {
                        let v = show(self.call(n, &[]));
                        out.push_str(&format!(" {n}={v}"));
                    }
                    out
                }
                op @ ("PP" | "PG" | "PI" | "PM") => {
                    let name = match op {
                        "PP" => "Pattern",
                        "PG" => "Global",
                        "PI" => "IgnoreCase",
                        _ => "MultiLine",
                    };
                    if let Err(e) = self.set(name, arg(f[3])) {
                        return Some(show(Err(e)));
                    }

                    match self.call(name, &[]) {
                        Ok(Variant::Bstr(s)) if s.is_empty() && f[3] == "#u" => "<null>".into(),
                        r => show(r),
                    }
                }
                "N" => {
                    self.set("Pattern", Variant::Bstr("a".into())).unwrap();
                    let a: Vec<Variant> = (0..f[3].parse::<usize>().unwrap())
                        .map(|_| Variant::Bstr("a".into()))
                        .collect();
                    show(self.call(f[2], &a))
                }
                "TA" | "RA" => {
                    self.set("Pattern", arg(f[2])).unwrap();
                    self.set("Global", Variant::Bool(f[1].contains('g')))
                        .unwrap();
                    if f[0] == "TA" {
                        show(self.call("Test", &[arg(f[3])]))
                    } else {
                        show(self.call("Replace", &[arg(f[3]), arg(f[4])]))
                    }
                }
                "XA" => {
                    let ms = match self.execute(&f, f[3]) {
                        Ok(ms) => ms,
                        Err(e) => return Some(show(Err(e))),
                    };
                    let Variant::I4(n) = self.get(ms, "Count", &[]).unwrap() else {
                        panic!("Count is not a Long")
                    };
                    let mut out = format!("n={n}");
                    if n > 0 {
                        let m = obj(self.get(ms, "Item", &[Variant::I4(0)]).unwrap());
                        let v = match self.get(m, "Value", &[]).unwrap() {
                            Variant::Bstr(s) if s.is_empty() => Variant::Empty,
                            v => v,
                        };
                        out.push_str(&format!(" first={}", show(Ok(v))));
                    }
                    out
                }
                op @ ("I" | "S") => {
                    let ms = self.execute(&f, f[3]).unwrap();
                    let ix: Vec<i32> = f[4].split(',').map(|x| x.parse().unwrap()).collect();
                    let m = match self.get(ms, "Item", &[Variant::I4(ix[0])]) {
                        Ok(v) => obj(v),
                        Err(e) => return Some(show(Err(e))),
                    };
                    if op == "I" {
                        return Some(show(self.get(m, "Value", &[])));
                    }
                    let sm = obj(self.get(m, "SubMatches", &[]).unwrap());
                    show(self.get(sm, "Item", &[Variant::I4(ix[1])]))
                }
                _ => return None,
            })
        }
    }

    #[test]
    fn golden_typed_and_collection_ops_replay() {
        let path = concat!(
            env!("CARGO_MANIFEST_DIR"),
            "/../../conformance/com/regexp_probe.golden.txt"
        );
        let text = std::fs::read_to_string(path).unwrap();
        let (mut run, mut failures) = (0, Vec::new());
        for line in text.lines() {
            let Some((case, want)) = line.split_once(" => ") else {
                continue;
            };
            let Some(got) = Rig::new().run(case) else {
                continue;
            };
            run += 1;
            if got != want {
                failures.push(format!("{case}\n  want {want}\n  got  {got}"));
            }
        }
        assert!(run > 60, "only {run} cases ran");
        assert!(
            failures.is_empty(),
            "{} of {run} cases differ:\n{}",
            failures.len(),
            failures.join("\n")
        );
    }

    #[test]
    fn execute_enumerates_matches_and_submatches() {
        let mut rig = Rig::new();
        rig.set("Pattern", Variant::Bstr("(a)(b)?".into())).unwrap();
        rig.set("Global", Variant::Bool(true)).unwrap();
        let ms = obj(rig.call("Execute", &[Variant::Bstr("aab".into())]).unwrap());
        let items = rig.app.com_enumerate(ms).unwrap();
        assert_eq!(items.len(), 2);
        let m = obj(items[1].clone());
        assert_eq!(rig.app.com_type_name(m), "Match");
        let sm = obj(rig.get(m, "SubMatches", &[]).unwrap());
        let subs = rig.app.com_enumerate(sm).unwrap();
        assert_eq!(
            subs,
            vec![Variant::Bstr("a".into()), Variant::Bstr("b".into())]
        );
        assert_eq!(rig.app.com_type_name(rig.re), "RegExp");
    }
}
