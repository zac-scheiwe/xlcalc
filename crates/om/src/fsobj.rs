use std::fs::{self, File, OpenOptions};
use std::io::{self, Read, Write};
use std::path::Path;

use xlcalc_variant::{change_type, flags, Locale, VarType, Variant};
use xlcalc_vba::{InvokeKind, VbaError};

use crate::comobj::ComObj;

const E_INVALIDARG: i32 = 0x8007_0057_u32 as i32;

const PARAM_NOT_FOUND: i32 = 0x8002_0004_u32 as i32;

pub(crate) fn invalid_arg() -> VbaError {
    VbaError {
        number: E_INVALIDARG,
        description: "The parameter is incorrect.".to_string(),
    }
}

fn io_err(e: &io::Error) -> VbaError {
    VbaError::new(match e.raw_os_error() {
        Some(2) => 53,
        Some(3 | 267) => 76,
        Some(80 | 183) => 58,
        Some(123 | 161) => 52,
        Some(_) => 70,
        None if e.kind() == io::ErrorKind::NotFound => 53,
        None => 70,
    })
}

fn is_sep(c: char) -> bool {
    c == '\\' || c == '/'
}

fn has_wild(s: &str) -> bool {
    s.contains(['*', '?'])
}

fn wild(pat: &str, name: &str) -> bool {
    fn go(p: &[char], n: &[char]) -> bool {
        match p.split_first() {
            None => n.is_empty(),
            Some(('*', rest)) => (0..=n.len()).any(|i| go(rest, &n[i..])),
            Some(('?', rest)) => !n.is_empty() && go(rest, &n[1..]),
            Some((c, rest)) => n.first().is_some_and(|x| x == c) && go(rest, &n[1..]),
        }
    }
    let pat = if pat == "*.*" { "*" } else { pat };
    let p: Vec<char> = pat.to_lowercase().chars().collect();
    let n: Vec<char> = name.to_lowercase().chars().collect();
    go(&p, &n)
}

fn strip_verbatim(s: String) -> String {
    match s.strip_prefix(r"\\?\UNC\") {
        Some(rest) => format!(r"\\{rest}"),
        None => s.strip_prefix(r"\\?\").map_or(s.clone(), str::to_string),
    }
}

fn canonical(p: &str) -> io::Result<String> {
    let t = p.trim_end_matches(is_sep);
    let root;
    let t = if t.len() == 2 && t.ends_with(':') && t.len() != p.len() {
        root = format!("{t}\\");
        root.as_str()
    } else {
        t
    };
    fs::canonicalize(t).map(|x| strip_verbatim(x.to_string_lossy().to_string()))
}

fn is_root(p: &str) -> bool {
    p.len() == 3 && p.ends_with(":\\")
}

fn leaf(p: &str) -> String {
    if is_root(p) {
        return String::new();
    }
    p.rsplit(is_sep).next().unwrap_or("").to_string()
}

fn parent(p: &str) -> Option<String> {
    if is_root(p) {
        return None;
    }
    let i = p.rfind(is_sep)?;
    let head = &p[..i];
    Some(if head.len() == 2 && head.ends_with(':') {
        format!("{head}\\")
    } else {
        head.to_string()
    })
}

fn join(dir: &str, name: &str) -> String {
    if dir.ends_with(is_sep) {
        format!("{dir}{name}")
    } else {
        format!("{dir}\\{name}")
    }
}

fn split_pattern(p: &str) -> (String, String) {
    match p.rfind(is_sep) {
        Some(i) => {
            let head = &p[..i];
            let head = if head.len() == 2 && head.ends_with(':') {
                format!("{head}\\")
            } else {
                head.to_string()
            };
            (head, p[i + 1..].to_string())
        }
        None => (".".to_string(), p.to_string()),
    }
}

fn norm_key(p: &str) -> String {
    p.replace('/', "\\").trim_end_matches('\\').to_lowercase()
}

fn inside(folder: &str, target: &str) -> bool {
    let (f, t) = (norm_key(folder), norm_key(target));
    t == f || t.starts_with(&format!("{f}\\"))
}

fn list(dir: &str, dirs: bool) -> io::Result<Vec<String>> {
    let mut v = Vec::new();
    for e in fs::read_dir(dir)? {
        let e = e?;
        if e.file_type().map(|t| t.is_dir()).unwrap_or(false) == dirs {
            v.push(e.file_name().to_string_lossy().to_string());
        }
    }
    v.sort_by_key(|n| n.to_lowercase());
    Ok(v)
}

fn valid_leaf(n: &str) -> bool {
    !n.is_empty() && !n.contains(['\\', '/', '*', '?', '<', '>', '|', '"', ':'])
}

fn is_missing(v: &Variant) -> bool {
    matches!(v, Variant::Empty) || matches!(v, Variant::Error(e) if *e == PARAM_NOT_FOUND)
}

fn count(args: &[Variant], min: usize, max: usize) -> Result<(), VbaError> {
    if (min..=max).contains(&args.len()) {
        Ok(())
    } else {
        Err(VbaError::new(450))
    }
}

fn to_text(v: &Variant) -> Result<String, VbaError> {
    match v.clone().canon() {
        Variant::Bstr(s) => Ok(s),
        Variant::Empty | Variant::Null => Ok(String::new()),
        other => match change_type(&other, VarType::Bstr, Locale::EN_US, flags::ALPHABOOL) {
            Ok(Variant::Bstr(s)) => Ok(s),
            _ => Err(VbaError::new(13)),
        },
    }
}

fn text(args: &[Variant], i: usize) -> Result<String, VbaError> {
    to_text(args.get(i).ok_or_else(|| VbaError::new(450))?)
}

fn opt_long(args: &[Variant], i: usize, default: i32) -> Result<i32, VbaError> {
    match args.get(i) {
        Some(v) if !is_missing(v) => match change_type(v, VarType::I4, Locale::EN_US, 0) {
            Ok(Variant::I4(n)) => Ok(n),
            _ => Err(VbaError::new(13)),
        },
        _ => Ok(default),
    }
}

fn opt_bool(args: &[Variant], i: usize, default: bool) -> Result<bool, VbaError> {
    match args.get(i) {
        Some(v) if !is_missing(v) => match change_type(v, VarType::Bool, Locale::EN_US, 0) {
            Ok(Variant::Bool(b)) => Ok(b),
            _ => Err(VbaError::new(13)),
        },
        _ => Ok(default),
    }
}

fn long_result(n: u64) -> Variant {
    i32::try_from(n).map_or(Variant::R8(n as f64), Variant::I4)
}

const CP1252: [u16; 32] = [
    0x20AC, 0x0081, 0x201A, 0x0192, 0x201E, 0x2026, 0x2020, 0x2021, 0x02C6, 0x2030, 0x0160, 0x2039,
    0x0152, 0x008D, 0x017D, 0x008F, 0x0090, 0x2018, 0x2019, 0x201C, 0x201D, 0x2022, 0x2013, 0x2014,
    0x02DC, 0x2122, 0x0161, 0x203A, 0x0153, 0x009D, 0x017E, 0x0178,
];

fn ansi_char(b: u8) -> char {
    if (0x80..0xA0).contains(&b) {
        char::from_u32(u32::from(CP1252[usize::from(b - 0x80)])).unwrap_or('?')
    } else {
        char::from(b)
    }
}

fn ansi_byte(c: char) -> Option<u8> {
    let n = u32::from(c);
    if n < 0x80 || (0xA0..0x100).contains(&n) {
        return u8::try_from(n).ok();
    }
    let i = CP1252.iter().position(|&x| u32::from(x) == n)?;
    u8::try_from(0x80 + i).ok()
}

fn decode(bytes: &[u8], unicode: bool) -> Vec<char> {
    if !unicode {
        return bytes.iter().map(|&b| ansi_char(b)).collect();
    }
    let (body, be) = match bytes {
        [0xFF, 0xFE, rest @ ..] => (rest, false),
        [0xFE, 0xFF, rest @ ..] => (rest, true),
        _ => (bytes, false),
    };
    let units: Vec<u16> = body
        .as_chunks::<2>()
        .0
        .iter()
        .map(|c| {
            if be {
                u16::from_be_bytes(*c)
            } else {
                u16::from_le_bytes(*c)
            }
        })
        .collect();
    String::from_utf16_lossy(&units).chars().collect()
}

fn looks_unicode(bytes: &[u8]) -> bool {
    if bytes.starts_with(&[0xFF, 0xFE]) {
        return true;
    }
    bytes.len() >= 2
        && bytes.len().is_multiple_of(2)
        && bytes.iter().skip(1).step_by(2).all(|&b| b == 0)
        && bytes.iter().step_by(2).any(|&b| b != 0)
}

fn encode(s: &str, unicode: bool) -> Result<Vec<u8>, VbaError> {
    if unicode {
        return Ok(s.encode_utf16().flat_map(u16::to_le_bytes).collect());
    }
    s.chars()
        .map(|c| ansi_byte(c).ok_or_else(|| VbaError::new(5)))
        .collect()
}

fn advance(line: &mut i32, col: &mut i32, chars: &[char]) {
    for &c in chars {
        if c == '\n' {
            *line += 1;
            *col = 1;
        } else {
            *col += 1;
        }
    }
}

#[derive(Debug)]
pub(crate) struct Stream {
    file: Option<File>,
    readable: bool,
    unicode: bool,
    chars: Vec<char>,
    pos: usize,
    line: i32,
    col: i32,
}

#[cfg(windows)]
fn share(o: &mut OpenOptions, mode: u32) {
    use std::os::windows::fs::OpenOptionsExt;
    o.share_mode(mode);
}

#[cfg(not(windows))]
fn share(_: &mut OpenOptions, _: u32) {}

const SHARE_READ: u32 = 1;
const SHARE_READ_WRITE: u32 = 3;

fn open_read(path: &str, format: i32) -> Result<Stream, VbaError> {
    let mut o = OpenOptions::new();
    o.read(true);
    share(&mut o, SHARE_READ_WRITE);
    let mut f = o.open(path).map_err(|e| io_err(&e))?;
    let mut bytes = Vec::new();
    f.read_to_end(&mut bytes).map_err(|e| io_err(&e))?;
    let unicode = format == -1 || (format == -2 && looks_unicode(&bytes));
    Ok(Stream {
        file: Some(f),
        readable: true,
        unicode,
        chars: decode(&bytes, unicode),
        pos: 0,
        line: 1,
        col: 1,
    })
}
impl Stream {
    fn put(&mut self, s: &str) -> Result<(), VbaError> {
        let bytes = encode(s, self.unicode)?;
        let f = self.file.as_mut().ok_or_else(|| VbaError::new(91))?;
        f.write_all(&bytes).map_err(|e| io_err(&e))?;
        let chars: Vec<char> = s.chars().collect();
        advance(&mut self.line, &mut self.col, &chars);
        Ok(())
    }

    fn consume(&mut self, n: usize) {
        let end = self.pos + n;
        advance(&mut self.line, &mut self.col, &self.chars[self.pos..end]);
        self.pos = end;
    }

    fn at_end(&self) -> bool {
        self.pos >= self.chars.len()
    }

    fn at_end_of_line(&self) -> bool {
        match self.chars.get(self.pos) {
            None | Some('\n') => true,
            Some('\r') => self.chars.get(self.pos + 1) == Some(&'\n'),
            Some(_) => false,
        }
    }

    fn next_line(&self) -> (String, usize) {
        let rest = &self.chars[self.pos..];
        match rest.iter().position(|&c| c == '\n') {
            Some(i) => {
                let body = if i > 0 && rest[i - 1] == '\r' {
                    &rest[..i - 1]
                } else {
                    &rest[..i]
                };
                (body.iter().collect(), i + 1)
            }
            None => (rest.iter().collect(), rest.len()),
        }
    }

    fn need_read(&self) -> Result<(), VbaError> {
        if self.readable {
            Ok(())
        } else {
            Err(VbaError::new(54))
        }
    }

    fn need_write(&self) -> Result<(), VbaError> {
        if self.readable {
            Err(VbaError::new(54))
        } else {
            Ok(())
        }
    }

    pub(crate) fn call(
        &mut self,
        name: &str,
        kind: InvokeKind,
        args: &[Variant],
    ) -> Result<Variant, VbaError> {
        if kind != InvokeKind::Get {
            return Err(VbaError::new(438));
        }
        let name = name.to_ascii_lowercase();
        if name == "close" {
            count(args, 0, 0)?;
            self.file = None;
            return Ok(Variant::Empty);
        }
        let known = [
            "line",
            "column",
            "atendofstream",
            "atendofline",
            "read",
            "readline",
            "readall",
            "skip",
            "skipline",
            "write",
            "writeline",
            "writeblanklines",
        ];
        if !known.contains(&name.as_str()) {
            return Err(VbaError::new(438));
        }
        if self.file.is_none() {
            return Err(VbaError::new(91));
        }
        match name.as_str() {
            "line" => {
                count(args, 0, 0)?;
                Ok(Variant::I4(self.line))
            }
            "column" => {
                count(args, 0, 0)?;
                Ok(Variant::I4(self.col))
            }
            "atendofstream" => {
                count(args, 0, 0)?;
                self.need_read()?;
                Ok(Variant::Bool(self.at_end()))
            }
            "atendofline" => {
                count(args, 0, 0)?;
                self.need_read()?;
                Ok(Variant::Bool(self.at_end_of_line()))
            }
            "readall" => {
                count(args, 0, 0)?;
                self.need_read()?;
                if self.at_end() {
                    return Err(VbaError::new(62));
                }
                let n = self.chars.len() - self.pos;
                let s: String = self.chars[self.pos..].iter().collect();
                self.consume(n);
                Ok(Variant::Bstr(s))
            }
            "readline" => {
                count(args, 0, 0)?;
                self.need_read()?;
                if self.at_end() {
                    return Err(VbaError::new(62));
                }
                let (s, n) = self.next_line();
                self.consume(n);
                Ok(Variant::Bstr(s))
            }
            "skipline" => {
                count(args, 0, 0)?;
                self.need_read()?;
                if self.at_end() {
                    return Err(VbaError::new(62));
                }
                let (_, n) = self.next_line();
                self.consume(n);
                Ok(Variant::Empty)
            }
            "read" | "skip" => {
                count(args, 1, 1)?;
                self.need_read()?;
                let n = opt_long(args, 0, 0)?;
                let n = usize::try_from(n).map_err(|_| invalid_arg())?;
                if n == 0 {
                    return Ok(if name == "read" {
                        Variant::Bstr(String::new())
                    } else {
                        Variant::Empty
                    });
                }
                if self.at_end() {
                    return Err(VbaError::new(62));
                }
                let n = n.min(self.chars.len() - self.pos);
                let s: String = self.chars[self.pos..self.pos + n].iter().collect();
                self.consume(n);
                Ok(if name == "read" {
                    Variant::Bstr(s)
                } else {
                    Variant::Empty
                })
            }
            "write" => {
                count(args, 1, 1)?;
                self.need_write()?;
                let s = text(args, 0)?;
                self.put(&s)?;
                Ok(Variant::Empty)
            }
            "writeline" => {
                count(args, 0, 1)?;
                self.need_write()?;
                let s = if args.is_empty() {
                    String::new()
                } else {
                    text(args, 0)?
                };
                self.put(&format!("{s}\r\n"))?;
                Ok(Variant::Empty)
            }
            _ => {
                count(args, 1, 1)?;
                self.need_write()?;
                let n = opt_long(args, 0, 0)?;
                let n = usize::try_from(n).map_err(|_| invalid_arg())?;
                self.put(&"\r\n".repeat(n))?;
                Ok(Variant::Empty)
            }
        }
    }
}

fn create_text_file(path: &str, overwrite: bool, unicode: bool) -> Result<Stream, VbaError> {
    if path.is_empty() {
        return Err(invalid_arg());
    }
    let mut o = OpenOptions::new();
    o.write(true);
    if overwrite {
        o.create(true).truncate(true);
    } else {
        o.create_new(true);
    }
    share(&mut o, SHARE_READ);
    let f = o.open(path).map_err(|e| io_err(&e))?;
    let mut s = Stream {
        file: Some(f),
        readable: false,
        unicode,
        chars: Vec::new(),
        pos: 0,
        line: 1,
        col: 1,
    };
    if unicode {
        if let Some(f) = s.file.as_mut() {
            f.write_all(&[0xFF, 0xFE]).map_err(|e| io_err(&e))?;
        }
    }
    Ok(s)
}

fn open_text_file(path: &str, mode: i32, create: bool, format: i32) -> Result<Stream, VbaError> {
    if path.is_empty() || !matches!(mode, 1 | 2 | 8) || !matches!(format, -2..=0) {
        return Err(invalid_arg());
    }
    if mode == 1 {
        if create {
            drop(OpenOptions::new().append(true).create(true).open(path));
        }
        return open_read(path, format);
    }
    let mut o = OpenOptions::new();
    if mode == 2 {
        o.write(true).truncate(true).create(create);
    } else {
        o.read(true).append(true).create(create);
    }
    share(&mut o, SHARE_READ);
    let mut f = o.open(path).map_err(|e| io_err(&e))?;
    let mut s = Stream {
        file: None,
        readable: false,
        unicode: format == -1,
        chars: Vec::new(),
        pos: 0,
        line: 1,
        col: 1,
    };
    if mode == 8 {
        let mut old = Vec::new();
        f.read_to_end(&mut old).map_err(|e| io_err(&e))?;
        s.unicode = format == -1 || (format == -2 && looks_unicode(&old));
        if s.unicode {
            if old.is_empty() {
                f.write_all(&[0xFF, 0xFE]).map_err(|e| io_err(&e))?;
            }
        } else {
            let text = decode(&old, false);
            advance(&mut s.line, &mut s.col, &text);
        }
    } else if format == -1 {
        f.write_all(&[0xFF, 0xFE]).map_err(|e| io_err(&e))?;
    }
    s.file = Some(f);
    Ok(s)
}

#[cfg(windows)]
fn attrs_of(md: &fs::Metadata) -> i32 {
    use std::os::windows::fs::MetadataExt;
    (md.file_attributes() & 0x37) as i32
}

#[cfg(not(windows))]
fn attrs_of(md: &fs::Metadata) -> i32 {
    let kind = if md.is_dir() { 16 } else { 32 };
    kind | i32::from(md.permissions().readonly())
}

#[cfg(windows)]
fn set_attrs(path: &str, attrs: i32) -> Result<(), VbaError> {
    use windows::core::HSTRING;
    use windows::Win32::Storage::FileSystem::{SetFileAttributesW, FILE_FLAGS_AND_ATTRIBUTES};
    let bits = (attrs & 0x27) as u32;
    let bits = if bits == 0 { 0x80 } else { bits };

    unsafe { SetFileAttributesW(&HSTRING::from(path), FILE_FLAGS_AND_ATTRIBUTES(bits)) }
        .map_err(|_| VbaError::new(70))
}

#[cfg(not(windows))]
fn set_attrs(path: &str, attrs: i32) -> Result<(), VbaError> {
    let md = fs::metadata(path).map_err(|e| io_err(&e))?;
    let mut p = md.permissions();
    p.set_readonly(attrs & 1 != 0);
    fs::set_permissions(path, p).map_err(|e| io_err(&e))
}

#[cfg(windows)]
fn ole_date(t: io::Result<std::time::SystemTime>) -> Variant {
    use windows::Win32::Foundation::{FILETIME, SYSTEMTIME};
    use windows::Win32::System::Time::{FileTimeToSystemTime, SystemTimeToTzSpecificLocalTime};
    let Ok(t) = t else {
        return Variant::Date(0.0);
    };
    let Ok(since) = t.duration_since(std::time::UNIX_EPOCH) else {
        return Variant::Date(0.0);
    };
    let ticks =
        (since.as_secs() + 11_644_473_600) * 10_000_000 + u64::from(since.subsec_nanos() / 100);
    let ft = FILETIME {
        dwLowDateTime: ticks as u32,
        dwHighDateTime: (ticks >> 32) as u32,
    };
    let (mut utc, mut local) = (SYSTEMTIME::default(), SYSTEMTIME::default());

    let ok = unsafe {
        FileTimeToSystemTime(&ft, &mut utc).is_ok()
            && SystemTimeToTzSpecificLocalTime(None, &utc, &mut local).is_ok()
    };
    if !ok {
        return Variant::Date(0.0);
    }
    let days = days_from_civil(
        i64::from(local.wYear),
        i64::from(local.wMonth),
        i64::from(local.wDay),
    ) + 25_569;
    let secs = f64::from(local.wHour) * 3600.0
        + f64::from(local.wMinute) * 60.0
        + f64::from(local.wSecond)
        + f64::from(local.wMilliseconds) / 1000.0;
    Variant::Date(days as f64 + secs / 86_400.0)
}

#[cfg(not(windows))]
fn ole_date(t: io::Result<std::time::SystemTime>) -> Variant {
    let secs = t
        .ok()
        .and_then(|t| t.duration_since(std::time::UNIX_EPOCH).ok())
        .map_or(0.0, |d| d.as_secs_f64());
    Variant::Date(25_569.0 + secs / 86_400.0)
}

#[cfg(windows)]
fn days_from_civil(y: i64, m: i64, d: i64) -> i64 {
    let y = if m <= 2 { y - 1 } else { y };
    let era = y.div_euclid(400);
    let yoe = y - era * 400;
    let doy = (153 * (if m > 2 { m - 3 } else { m + 9 }) + 2) / 5 + d - 1;
    let doe = yoe * 365 + yoe / 4 - yoe / 100 + doy;
    era * 146_097 + doe - 719_468
}

fn clear_readonly(path: &Path) {
    if let Ok(md) = fs::metadata(path) {
        let mut p = md.permissions();
        if p.readonly() {
            #[allow(clippy::permissions_set_readonly_false)]
            p.set_readonly(false);
            drop(fs::set_permissions(path, p));
        }
    }
}

fn any_readonly(path: &Path) -> bool {
    let Ok(md) = fs::metadata(path) else {
        return false;
    };
    if md.permissions().readonly() {
        return true;
    }
    md.is_dir()
        && fs::read_dir(path)
            .map(|d| d.flatten().any(|e| any_readonly(&e.path())))
            .unwrap_or(false)
}

fn clear_readonly_tree(path: &Path) {
    clear_readonly(path);
    if let Ok(d) = fs::read_dir(path) {
        for e in d.flatten() {
            if e.file_type().map(|t| t.is_dir()).unwrap_or(false) {
                clear_readonly_tree(&e.path());
            } else {
                clear_readonly(&e.path());
            }
        }
    }
}

fn dir_size(path: &str) -> u64 {
    let Ok(d) = fs::read_dir(path) else {
        return 0;
    };
    d.flatten()
        .map(|e| match e.file_type() {
            Ok(t) if t.is_dir() => dir_size(&e.path().to_string_lossy()),
            _ => e.metadata().map_or(0, |m| m.len()),
        })
        .sum()
}

fn short_name(name: &str) -> String {
    let (base, ext) = match name.rfind('.') {
        Some(i) if i > 0 => (&name[..i], &name[i + 1..]),
        _ => (name, ""),
    };
    let fits = base.len() <= 8
        && ext.len() <= 3
        && !base.contains([' ', '.'])
        && !ext.contains([' ', '.']);
    if fits {
        return name.to_string();
    }
    let mut b: String = base
        .chars()
        .filter(|c| c.is_ascii_alphanumeric())
        .take(6)
        .collect::<String>()
        .to_uppercase();
    b.push_str("~1");
    let e: String = ext
        .chars()
        .filter(|c| c.is_ascii_alphanumeric())
        .take(3)
        .collect::<String>()
        .to_uppercase();
    if e.is_empty() {
        b
    } else {
        format!("{b}.{e}")
    }
}

fn type_name(file: &str) -> String {
    let name = leaf(file);
    let ext = match name.rfind('.') {
        Some(i) if i + 1 < name.len() => name[i + 1..].to_lowercase(),
        _ => return "File".to_string(),
    };
    match ext.as_str() {
        "txt" | "log" => "Text Document".to_string(),
        "xlsx" => "Microsoft Excel Worksheet".to_string(),
        "dll" => "Application extension".to_string(),
        _ => format!("{} File", ext.to_uppercase()),
    }
}

fn target_of(dest: &str, name: &str) -> Result<String, VbaError> {
    if dest.is_empty() || has_wild(dest) {
        return Err(invalid_arg());
    }
    if dest.ends_with(is_sep) {
        let folder = dest.trim_end_matches(is_sep);
        if !Path::new(folder).is_dir() {
            return Err(VbaError::new(76));
        }
        return Ok(join(folder, name));
    }
    Ok(dest.to_string())
}

fn same_path(a: &str, b: &str) -> bool {
    match (canonical(a), canonical(b)) {
        (Ok(x), Ok(y)) => norm_key(&x) == norm_key(&y),
        _ => false,
    }
}

fn copy_file_one(src: &str, target: &str, overwrite: bool) -> Result<(), VbaError> {
    if same_path(src, target) {
        return Ok(());
    }
    let t = Path::new(target);
    if t.is_dir() {
        return Err(VbaError::new(70));
    }
    if t.exists() && !overwrite {
        return Err(VbaError::new(58));
    }
    fs::copy(src, target).map_err(|e| io_err(&e))?;
    Ok(())
}

fn move_one(src: &str, target: &str) -> Result<(), VbaError> {
    if same_path(src, target) {
        return Ok(());
    }
    if Path::new(target).exists() {
        return Err(VbaError::new(58));
    }
    fs::rename(src, target).map_err(|e| io_err(&e))
}

fn copy_tree(src: &str, dst: &str, overwrite: bool) -> Result<(), VbaError> {
    if !Path::new(dst).is_dir() {
        fs::create_dir(dst).map_err(|e| io_err(&e))?;
    }
    for n in list(src, false).map_err(|e| io_err(&e))? {
        let t = join(dst, &n);
        if Path::new(&t).exists() && !overwrite {
            return Err(VbaError::new(58));
        }
        fs::copy(join(src, &n), &t).map_err(|e| io_err(&e))?;
    }
    for n in list(src, true).map_err(|e| io_err(&e))? {
        copy_tree(&join(src, &n), &join(dst, &n), overwrite)?;
    }
    Ok(())
}

fn delete_file_path(path: &str, force: bool) -> Result<(), VbaError> {
    let p = Path::new(path);
    if p.is_dir() {
        return Ok(());
    }
    if !p.is_file() {
        return Err(VbaError::new(53));
    }
    if fs::metadata(p).is_ok_and(|m| m.permissions().readonly()) {
        if !force {
            return Err(VbaError::new(70));
        }
        clear_readonly(p);
    }
    fs::remove_file(p).map_err(|e| io_err(&e))
}

fn delete_folder_path(path: &str, force: bool) -> Result<(), VbaError> {
    let p = Path::new(path);
    if p.is_file() {
        return Ok(());
    }
    if !p.is_dir() {
        return Err(VbaError::new(76));
    }
    if force {
        clear_readonly_tree(p);
    } else if any_readonly(p) {
        return Err(VbaError::new(70));
    }
    fs::remove_dir_all(p).map_err(|e| io_err(&e))
}

impl crate::Application<'_> {
    pub(crate) fn new_com(&mut self, obj: ComObj) -> Variant {
        self.com.push(obj);
        Variant::Dispatch(Some(crate::comobj::com_id(self.com.len() - 1)))
    }

    fn file_object(&mut self, path: &str) -> Result<Variant, VbaError> {
        let c = canonical(path).map_err(|_| VbaError::new(53))?;
        if !Path::new(&c).is_file() {
            return Err(VbaError::new(53));
        }
        Ok(self.new_com(ComObj::File(c)))
    }

    fn folder_object(&mut self, path: &str) -> Result<Variant, VbaError> {
        let c = canonical(path).map_err(|_| VbaError::new(76))?;
        if !Path::new(&c).is_dir() {
            return Err(VbaError::new(76));
        }
        Ok(self.new_com(ComObj::Folder(c)))
    }

    fn drive_object(&mut self, spec: &str) -> Result<Variant, VbaError> {
        let t = spec.trim_end_matches(is_sep);
        let mut cs = t.chars();
        let letter = cs.next();
        let rest: String = cs.collect();
        match letter {
            Some(l) if l.is_ascii_alphabetic() && (rest.is_empty() || rest == ":") => {
                let l = l.to_ascii_uppercase();
                if !Path::new(&format!("{l}:\\")).exists() {
                    return Err(VbaError::new(68));
                }
                Ok(self.new_com(ComObj::Drive(l)))
            }
            _ if spec.starts_with(r"\\") => Err(VbaError::new(76)),
            _ => Err(invalid_arg()),
        }
    }

    fn stream_object(&mut self, s: Result<Stream, VbaError>) -> Result<Variant, VbaError> {
        Ok(self.new_com(ComObj::TextStream(Box::new(s?))))
    }

    pub(crate) fn fso_files_call(
        &mut self,
        name: &str,
        args: &[Variant],
    ) -> Result<Option<Variant>, VbaError> {
        let v = match name {
            "createtextfile" => {
                count(args, 1, 3)?;
                let s = create_text_file(
                    &text(args, 0)?,
                    opt_bool(args, 1, true)?,
                    opt_bool(args, 2, false)?,
                );
                self.stream_object(s)?
            }
            "opentextfile" => {
                count(args, 1, 4)?;
                let s = open_text_file(
                    &text(args, 0)?,
                    opt_long(args, 1, 1)?,
                    opt_bool(args, 2, false)?,
                    opt_long(args, 3, 0)?,
                );
                self.stream_object(s)?
            }
            "getfile" | "getfolder" => {
                count(args, 1, 1)?;
                let p = text(args, 0)?;
                if p.is_empty() {
                    return Err(invalid_arg());
                }
                if name == "getfile" {
                    self.file_object(&p)?
                } else {
                    self.folder_object(&p)?
                }
            }
            "getdrive" => {
                count(args, 1, 1)?;
                self.drive_object(&text(args, 0)?)?
            }
            "getspecialfolder" => {
                count(args, 1, 1)?;
                let root = std::env::var("SystemRoot").unwrap_or_else(|_| r"C:\Windows".into());
                let p = match opt_long(args, 0, -1)? {
                    0 => root,
                    1 => join(&root, "System32"),
                    2 => std::env::temp_dir().to_string_lossy().to_string(),
                    _ => return Err(invalid_arg()),
                };
                self.folder_object(&p)?
            }
            "createfolder" => {
                count(args, 1, 1)?;
                let p = text(args, 0)?;
                if p.is_empty() {
                    return Err(invalid_arg());
                }
                fs::create_dir(p.trim_end_matches(is_sep)).map_err(|e| io_err(&e))?;
                self.folder_object(&p)?
            }
            "getfileversion" => {
                count(args, 1, 1)?;
                if !Path::new(&text(args, 0)?).is_file() {
                    return Err(VbaError::new(53));
                }
                Variant::Bstr(String::new())
            }
            "deletefile" => {
                count(args, 1, 2)?;
                delete_files(&text(args, 0)?, opt_bool(args, 1, false)?)?
            }
            "deletefolder" => {
                count(args, 1, 2)?;
                delete_folders(&text(args, 0)?, opt_bool(args, 1, false)?)?
            }
            "copyfile" | "movefile" => {
                count(args, 2, if name == "copyfile" { 3 } else { 2 })?;
                let overwrite = opt_bool(args, 2, true)?;
                transfer_files(
                    &text(args, 0)?,
                    &text(args, 1)?,
                    name == "copyfile",
                    overwrite,
                )?
            }
            "copyfolder" | "movefolder" => {
                count(args, 2, if name == "copyfolder" { 3 } else { 2 })?;
                let overwrite = opt_bool(args, 2, true)?;
                transfer_folders(
                    &text(args, 0)?,
                    &text(args, 1)?,
                    name == "copyfolder",
                    overwrite,
                )?
            }
            _ => return Ok(None),
        };
        Ok(Some(v))
    }

    pub(crate) fn file_call(
        &mut self,
        idx: usize,
        path: &str,
        name: &str,
        kind: InvokeKind,
        args: &[Variant],
    ) -> Result<Variant, VbaError> {
        let name = name.to_ascii_lowercase();
        let md = fs::metadata(path)
            .ok()
            .filter(fs::Metadata::is_file)
            .ok_or_else(|| VbaError::new(53))?;
        let put = matches!(kind, InvokeKind::Put | InvokeKind::PutRef);
        match (name.as_str(), put) {
            ("name", false) => {
                count(args, 0, 0)?;
                Ok(Variant::Bstr(leaf(path)))
            }
            ("path", false) => {
                count(args, 0, 0)?;
                Ok(Variant::Bstr(path.to_string()))
            }
            ("shortname", false) => Ok(Variant::Bstr(short_name(&leaf(path)))),
            ("shortpath", false) => Ok(Variant::Bstr(join(
                &parent(path).unwrap_or_default(),
                &short_name(&leaf(path)),
            ))),
            ("size", false) => Ok(long_result(md.len())),
            ("type", false) => Ok(Variant::Bstr(type_name(path))),
            ("attributes", false) => Ok(Variant::I4(attrs_of(&md))),
            ("datecreated", false) => Ok(ole_date(md.created())),
            ("datelastmodified", false) => Ok(ole_date(md.modified())),
            ("datelastaccessed", false) => Ok(ole_date(md.accessed())),
            ("parentfolder", false) => self.folder_object(&parent(path).unwrap_or_default()),
            ("drive", false) => self.drive_object(&path[..2]),
            ("attributes", true) => {
                set_attrs(path, opt_long(args, 0, 0)?)?;
                Ok(Variant::Empty)
            }
            ("name", true) => {
                let target = self.rename_target(path, &text(args, 0)?)?;
                fs::rename(path, &target).map_err(|e| io_err(&e))?;
                self.com[idx] = ComObj::File(canonical(&target).map_err(|e| io_err(&e))?);
                Ok(Variant::Empty)
            }
            ("copy", false) => {
                count(args, 1, 2)?;
                let target = target_of(&text(args, 0)?, &leaf(path))?;
                copy_file_one(path, &target, opt_bool(args, 1, true)?)?;
                Ok(Variant::Empty)
            }
            ("move", false) => {
                count(args, 1, 1)?;
                let target = target_of(&text(args, 0)?, &leaf(path))?;
                move_one(path, &target)?;
                if let Ok(c) = canonical(&target) {
                    self.com[idx] = ComObj::File(c);
                }
                Ok(Variant::Empty)
            }
            ("delete", false) => {
                count(args, 0, 1)?;
                delete_file_path(path, opt_bool(args, 0, false)?)?;
                Ok(Variant::Empty)
            }
            ("openastextstream", false) => {
                count(args, 0, 2)?;
                let s = open_text_file(path, opt_long(args, 0, 1)?, false, opt_long(args, 1, 0)?);
                self.stream_object(s)
            }
            (
                "path" | "size" | "type" | "datecreated" | "datelastmodified" | "datelastaccessed"
                | "shortname" | "shortpath" | "parentfolder" | "drive",
                true,
            ) => Err(VbaError::new(450)),
            _ => Err(VbaError::new(438)),
        }
    }

    fn rename_target(&self, path: &str, new: &str) -> Result<String, VbaError> {
        if !valid_leaf(new) {
            return Err(invalid_arg());
        }
        let target = join(&parent(path).unwrap_or_default(), new);
        if Path::new(&target).exists() {
            return Err(VbaError::new(58));
        }
        Ok(target)
    }

    pub(crate) fn folder_call(
        &mut self,
        idx: usize,
        path: &str,
        name: &str,
        kind: InvokeKind,
        args: &[Variant],
    ) -> Result<Variant, VbaError> {
        let name = name.to_ascii_lowercase();
        let md = fs::metadata(path)
            .ok()
            .filter(fs::Metadata::is_dir)
            .ok_or_else(|| VbaError::new(76))?;
        let put = matches!(kind, InvokeKind::Put | InvokeKind::PutRef);
        let root = is_root(path);
        match (name.as_str(), put) {
            ("name", false) => Ok(Variant::Bstr(leaf(path))),
            ("path", false) => Ok(Variant::Bstr(path.to_string())),
            ("shortname", false) => Ok(Variant::Bstr(short_name(&leaf(path)))),
            ("shortpath", false) => Ok(Variant::Bstr(path.to_string())),
            ("type", false) => Ok(Variant::Bstr(
                if root { "Local Disk" } else { "File folder" }.to_string(),
            )),
            ("attributes", false) => Ok(Variant::I4(attrs_of(&md))),
            ("isrootfolder", false) => Ok(Variant::Bool(root)),
            ("size", false) => Ok(long_result(dir_size(path))),
            ("datecreated", false) => Ok(ole_date(md.created())),
            ("datelastmodified", false) => Ok(ole_date(md.modified())),
            ("datelastaccessed", false) => Ok(ole_date(md.accessed())),
            ("parentfolder", false) => match parent(path) {
                Some(p) => self.folder_object(&p),
                None => Ok(Variant::Dispatch(None)),
            },
            ("drive", false) => self.drive_object(&path[..2]),

            ("files" | "subfolders", false) if !args.is_empty() => {
                self.collection_call(path, name == "files", "item", args)
            }
            ("files", false) => Ok(self.new_com(ComObj::Files(path.to_string()))),
            ("subfolders", false) => Ok(self.new_com(ComObj::SubFolders(path.to_string()))),
            ("attributes", true) => {
                set_attrs(path, opt_long(args, 0, 0)?)?;
                Ok(Variant::Empty)
            }
            ("name", true) if !root => {
                let target = self.rename_target(path, &text(args, 0)?)?;
                fs::rename(path, &target).map_err(|e| io_err(&e))?;
                self.com[idx] = ComObj::Folder(canonical(&target).map_err(|e| io_err(&e))?);
                Ok(Variant::Empty)
            }
            ("copy", false) => {
                count(args, 1, 2)?;
                transfer_folders(path, &text(args, 0)?, true, opt_bool(args, 1, true)?)?;
                Ok(Variant::Empty)
            }
            ("move", false) => {
                count(args, 1, 1)?;
                let dest = text(args, 0)?;
                transfer_folders(path, &dest, false, true)?;
                let target = target_of(&dest, &leaf(path))?;
                if let Ok(c) = canonical(&target) {
                    self.com[idx] = ComObj::Folder(c);
                }
                Ok(Variant::Empty)
            }
            ("delete", false) => {
                count(args, 0, 1)?;
                delete_folder_path(path, opt_bool(args, 0, false)?)?;
                Ok(Variant::Empty)
            }
            ("createtextfile", false) => {
                count(args, 1, 3)?;
                let s = create_text_file(
                    &join(path, &text(args, 0)?),
                    opt_bool(args, 1, true)?,
                    opt_bool(args, 2, false)?,
                );
                self.stream_object(s)
            }
            _ => Err(VbaError::new(438)),
        }
    }

    pub(crate) fn drive_call(
        &mut self,
        letter: char,
        name: &str,
        kind: InvokeKind,
    ) -> Result<Variant, VbaError> {
        if kind != InvokeKind::Get {
            return Err(VbaError::new(450));
        }
        match name.to_ascii_lowercase().as_str() {
            "driveletter" => Ok(Variant::Bstr(letter.to_string())),
            "path" => Ok(Variant::Bstr(format!("{letter}:"))),
            "drivetype" => Ok(Variant::I4(2)),
            "isready" => Ok(Variant::Bool(true)),
            "filesystem" => Ok(Variant::Bstr("NTFS".to_string())),
            "sharename" => Ok(Variant::Bstr(String::new())),
            "rootfolder" => self.folder_object(&format!("{letter}:\\")),
            _ => Err(VbaError::new(438)),
        }
    }

    pub(crate) fn collection_call(
        &mut self,
        dir: &str,
        files: bool,
        name: &str,
        args: &[Variant],
    ) -> Result<Variant, VbaError> {
        let name = if name.is_empty() { "item" } else { name }.to_ascii_lowercase();
        match name.as_str() {
            "count" => {
                count(args, 0, 0)?;
                let n = list(dir, !files).map_err(|e| io_err(&e))?.len();
                Ok(Variant::I4(i32::try_from(n).unwrap_or(i32::MAX)))
            }
            "item" => {
                count(args, 1, 1)?;
                let key = match args[0].clone().canon() {
                    Variant::Bstr(s) => s,
                    _ => return Err(invalid_arg()),
                };
                let found = list(dir, !files)
                    .map_err(|e| io_err(&e))?
                    .into_iter()
                    .find(|n| n.to_lowercase() == key.to_lowercase());
                match (found, files) {
                    (Some(n), true) => self.file_object(&join(dir, &n)),
                    (Some(n), false) => self.folder_object(&join(dir, &n)),
                    (None, true) => Err(VbaError::new(53)),
                    (None, false) => Err(VbaError::new(76)),
                }
            }
            "add" if !files => {
                count(args, 1, 1)?;
                let n = text(args, 0)?;
                if !valid_leaf(&n) {
                    return Err(invalid_arg());
                }
                let p = join(dir, &n);
                fs::create_dir(&p).map_err(|e| io_err(&e))?;
                self.folder_object(&p)
            }
            _ => Err(VbaError::new(438)),
        }
    }

    pub(crate) fn fs_collection_items(
        &mut self,
        dir: &str,
        files: bool,
    ) -> Result<Vec<Variant>, VbaError> {
        let mut out = Vec::new();
        for n in list(dir, !files).map_err(|e| io_err(&e))? {
            let p = join(dir, &n);
            out.push(if files {
                self.file_object(&p)?
            } else {
                self.folder_object(&p)?
            });
        }
        Ok(out)
    }
}

fn matches_of(path: &str, dirs: bool, missing_dir: i32) -> Result<Vec<String>, VbaError> {
    if path.is_empty() {
        return Err(invalid_arg());
    }
    let (dir, pat) = split_pattern(path.trim_end_matches(is_sep));
    if !has_wild(&pat) {
        return Ok(vec![path.trim_end_matches(is_sep).to_string()]);
    }
    if !Path::new(&dir).is_dir() {
        return Err(VbaError::new(missing_dir));
    }
    let names = list(&dir, dirs).map_err(|e| io_err(&e))?;
    Ok(names
        .into_iter()
        .filter(|n| wild(&pat, n))
        .map(|n| join(&dir, &n))
        .collect())
}

fn delete_files(path: &str, force: bool) -> Result<Variant, VbaError> {
    let wildcard = has_wild(&split_pattern(path).1);
    let found = matches_of(path, false, 53)?;
    if wildcard && found.is_empty() {
        return Err(VbaError::new(53));
    }
    for f in found {
        delete_file_path(&f, force)?;
    }
    Ok(Variant::Empty)
}

fn delete_folders(path: &str, force: bool) -> Result<Variant, VbaError> {
    if path.ends_with(is_sep) {
        return Err(VbaError::new(76));
    }
    let wildcard = has_wild(&split_pattern(path).1);
    let found = matches_of(path, true, 76)?;
    if wildcard && found.is_empty() {
        return Err(VbaError::new(76));
    }
    for f in found {
        delete_folder_path(&f, force)?;
    }
    Ok(Variant::Empty)
}

fn transfer_files(src: &str, dest: &str, copy: bool, overwrite: bool) -> Result<Variant, VbaError> {
    let (dir, pat) = split_pattern(src);
    if src.is_empty() || pat.is_empty() {
        return Err(invalid_arg());
    }
    if !has_wild(&pat) {
        if !Path::new(&dir).is_dir() {
            return Err(VbaError::new(76));
        }
        if !Path::new(src).is_file() {
            return Err(VbaError::new(53));
        }
        let target = target_of(dest, &pat)?;
        if copy {
            copy_file_one(src, &target, overwrite)?;
        } else {
            move_one(src, &target)?;
        }
        return Ok(Variant::Empty);
    }
    let found = matches_of(src, false, 76)?;
    if found.is_empty() {
        return Err(VbaError::new(53));
    }
    let folder = dest.trim_end_matches(is_sep);
    if dest.is_empty() || has_wild(dest) || !Path::new(folder).is_dir() {
        return Err(VbaError::new(76));
    }
    for f in found {
        let target = join(folder, &leaf(&f));
        if copy {
            copy_file_one(&f, &target, overwrite)?;
        } else {
            move_one(&f, &target)?;
        }
    }
    Ok(Variant::Empty)
}

fn transfer_folders(
    src: &str,
    dest: &str,
    copy: bool,
    overwrite: bool,
) -> Result<Variant, VbaError> {
    if src.is_empty() || dest.is_empty() || has_wild(dest) {
        return Err(invalid_arg());
    }
    let wildcard = has_wild(&split_pattern(src.trim_end_matches(is_sep)).1);
    let found = matches_of(src, true, 76)?;
    if wildcard && found.is_empty() {
        return Err(VbaError::new(76));
    }
    for s in found {
        if !Path::new(&s).is_dir() {
            return Err(VbaError::new(76));
        }
        let target = if wildcard || dest.ends_with(is_sep) {
            join(dest.trim_end_matches(is_sep), &leaf(&s))
        } else {
            dest.to_string()
        };
        if same_path(&s, &target) {
            continue;
        }
        if inside(&s, &target) {
            return Err(VbaError::new(5));
        }
        if copy {
            copy_tree(&s, &target, overwrite)?;
        } else {
            move_one(&s, &target)?;
        }
    }
    Ok(Variant::Empty)
}
