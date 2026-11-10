use std::path::Path;

use xlcalc_variant::Variant;
use xlcalc_vba::{InvokeKind, VbaError};

fn is_sep(c: char) -> bool {
    c == '\\' || c == '/'
}

fn drive_name(p: &str) -> String {
    let c: Vec<char> = p.chars().collect();
    if c.len() >= 2 && c[1] == ':' {
        return c[..2].iter().collect();
    }
    if c.len() > 2 && c[0] == '\\' && c[1] == '\\' {
        let rest: String = c[2..].iter().collect();
        let mut parts = rest.split(is_sep);
        if let (Some(s), Some(sh)) = (parts.next(), parts.next()) {
            return format!(r"\\{s}\{sh}");
        }
    }
    String::new()
}

fn split_last(p: &str) -> (String, String) {
    let t = p.trim_end_matches(is_sep);
    let drive = drive_name(t).len();
    if t.len() <= drive {
        return (String::new(), String::new());
    }
    match t[drive..].rfind(is_sep) {
        Some(i) => {
            let i = drive + i;
            let name = t[i + 1..].to_string();
            let parent = &t[..i];

            let parent = if parent.len() == drive && parent.ends_with(':') {
                format!("{parent}\\")
            } else {
                parent.to_string()
            };
            (parent, name)
        }
        None => (t[..drive].to_string(), t[drive..].to_string()),
    }
}

fn base_and_ext(name: &str) -> (String, String) {
    match name.rfind('.') {
        Some(i) => (name[..i].to_string(), name[i + 1..].to_string()),
        None => (name.to_string(), String::new()),
    }
}

fn absolute(p: &str) -> String {
    let drive = drive_name(p);
    let mut out: Vec<&str> = Vec::new();
    for part in p[drive.len()..].split(is_sep).filter(|s| !s.is_empty()) {
        match part {
            "." => {}
            ".." => {
                out.pop();
            }
            x => out.push(x),
        }
    }
    if out.is_empty() {
        return format!("{drive}\\");
    }
    format!("{drive}\\{}", out.join("\\"))
}

impl crate::Application<'_> {
    pub(crate) fn fso_call(
        &mut self,
        name: &str,
        kind: InvokeKind,
        args: &[Variant],
    ) -> Result<Variant, VbaError> {
        if kind != InvokeKind::Get {
            return Err(VbaError::new(438));
        }
        let text = |i: usize| match args.get(i).cloned().map(Variant::canon) {
            Some(Variant::Bstr(s)) => Ok(s),
            Some(Variant::Empty) => Ok(String::new()),
            Some(_) => Err(VbaError::new(13)),
            None => Err(VbaError::new(450)),
        };
        let one = |n: usize| {
            if args.len() == n {
                Ok(())
            } else {
                Err(VbaError::new(450))
            }
        };
        let s = |x: String| Ok(Variant::Bstr(x));
        let lower = name.to_ascii_lowercase();
        if let Some(v) = self.fso_files_call(&lower, args)? {
            return Ok(v);
        }
        match lower.as_str() {
            "getfilename" => {
                one(1)?;
                s(split_last(&text(0)?).1)
            }
            "getbasename" => {
                one(1)?;
                s(base_and_ext(&split_last(&text(0)?).1).0)
            }
            "getextensionname" => {
                one(1)?;
                s(base_and_ext(&split_last(&text(0)?).1).1)
            }
            "getparentfoldername" => {
                one(1)?;
                s(split_last(&text(0)?).0)
            }
            "getdrivename" => {
                one(1)?;
                s(drive_name(&text(0)?))
            }
            "buildpath" => {
                one(2)?;
                let (a, b) = (text(0)?, text(1)?);
                if a.is_empty() {
                    return s(b);
                }
                if b.is_empty() {
                    return s(a);
                }
                s(match (a.ends_with(is_sep), b.starts_with(is_sep)) {
                    (false, false) if a.ends_with(':') => format!("{a}{b}"),
                    (true, true) => format!("{a}{}", &b[1..]),
                    (false, false) => format!("{a}\\{b}"),
                    _ => format!("{a}{b}"),
                })
            }
            "fileexists" => {
                one(1)?;
                Ok(Variant::Bool(Path::new(&text(0)?).is_file()))
            }
            "folderexists" => {
                one(1)?;
                Ok(Variant::Bool(Path::new(&text(0)?).is_dir()))
            }
            "driveexists" => {
                one(1)?;
                let spec = text(0)?;
                let t = spec.trim_end_matches(is_sep);
                let mut cs = t.chars();
                Ok(Variant::Bool(match cs.next() {
                    Some(l) if l.is_ascii_alphabetic() && matches!(cs.as_str(), "" | ":") => {
                        Path::new(&format!("{l}:\\")).exists()
                    }
                    _ => false,
                }))
            }
            "getabsolutepathname" => {
                one(1)?;
                s(absolute(&text(0)?))
            }
            "gettempname" => {
                one(0)?;
                s("rad00000.tmp".to_string())
            }
            _ => Err(VbaError::new(438)),
        }
    }
}
