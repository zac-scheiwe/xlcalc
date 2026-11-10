use super::strings::{ansi_code, ansi_unit};
use super::util::{convert, is_null, long, text, text_out, units, R};
use super::RtContext;
use crate::VbaError;
use std::cell::RefCell;
use std::collections::BTreeMap;
use std::fs::{self, File, OpenOptions};
use std::io::{Read, Seek, SeekFrom, Write};
use std::path::{Path, PathBuf};
use xlcalc_variant::{SafeArray, VarType, Variant};

#[derive(Clone, Copy, PartialEq, Eq, Debug)]
enum Mode {
    Input,
    Output,
    Append,
    Binary,
    Random,
}

struct Open {
    key: String,
    file: File,
    mode: Mode,

    access: i32,
    reclen: u64,

    pos: u64,

    col: usize,

    data: Vec<u8>,

    eof_hit: bool,

    wbuf: Vec<u8>,

    wopen: bool,

    rec_start: u64,
}

impl Open {
    fn flush(&mut self) {
        if !self.wbuf.is_empty() {
            let _ = self.file.write_all(&self.wbuf);
            self.wbuf.clear();
        }
    }

    fn len(&self) -> u64 {
        self.file.metadata().map_or(0, |m| m.len()) + self.wbuf.len() as u64
    }
}

impl Drop for Open {
    fn drop(&mut self) {
        self.flush();
    }
}

#[derive(Default)]
struct State {
    files: BTreeMap<i32, Open>,

    dir: Option<std::vec::IntoIter<String>>,

    attrs: BTreeMap<String, i32>,
}

thread_local! {
    static STATE: RefCell<State> = RefCell::new(State::default());
}

fn with<T>(f: impl FnOnce(&mut State) -> T) -> T {
    STATE.with(|s| f(&mut s.borrow_mut()))
}

fn err(n: i32) -> VbaError {
    VbaError::new(n)
}

fn io_error(e: &std::io::Error, path: &Path) -> VbaError {
    use std::io::ErrorKind::{AlreadyExists, NotFound, PermissionDenied};
    match e.kind() {
        NotFound => {
            let parent = path.parent().filter(|p| !p.as_os_str().is_empty());
            if parent.is_some_and(|p| !p.exists()) {
                err(76)
            } else {
                err(53)
            }
        }
        PermissionDenied => err(75),
        AlreadyExists => err(58),
        _ => match e.raw_os_error() {
            Some(3) => err(76),
            Some(32 | 33) => err(70),
            Some(80) => err(58),
            Some(112) => err(61),
            _ => err(75),
        },
    }
}

fn full_path(p: &str) -> PathBuf {
    let path = PathBuf::from(p);
    if path.is_absolute() {
        path
    } else {
        std::env::current_dir().unwrap_or_default().join(path)
    }
}

fn key_of(p: &Path) -> String {
    p.to_string_lossy().replace('/', "\\").to_lowercase()
}

fn is_open(st: &State, key: &str) -> bool {
    st.files.values().any(|f| f.key == key)
}

fn file_no(ctx: &RtContext, v: &Variant) -> Result<i32, VbaError> {
    let n = long(ctx, v).map_err(|_| err(52))?;
    if (1..=511).contains(&n) {
        Ok(n)
    } else {
        Err(err(52))
    }
}

fn get_file(st: &mut State, n: i32) -> Result<&mut Open, VbaError> {
    st.files.get_mut(&n).ok_or_else(|| err(52))
}

pub(super) fn to_ansi(s: &str) -> Vec<u8> {
    units(s).iter().map(|&u| ansi_code(u) as u8).collect()
}

pub(super) fn from_ansi(b: &[u8]) -> String {
    let u: Vec<u16> = b.iter().map(|&c| ansi_unit(u16::from(c))).collect();
    String::from_utf16_lossy(&u)
}

pub(super) fn open(ctx: &RtContext, a: &[Variant]) -> R {
    let path_text = text(ctx, &a[0])?;
    let mode = match long(ctx, &a[1])? {
        1 => Mode::Input,
        2 => Mode::Output,
        3 => Mode::Append,
        4 => Mode::Binary,
        _ => Mode::Random,
    };
    let num = file_no(ctx, &a[2])?;
    let len = long(ctx, &a[3])?;
    let access = long(ctx, &a[4])?;
    if with(|st| st.files.contains_key(&num)) {
        return Err(err(55));
    }
    if path_text.is_empty() {
        return Err(err(75));
    }
    if path_text.chars().any(|c| matches!(c, '?' | '*') || c < ' ') {
        return Err(err(52));
    }
    let path = full_path(&path_text);
    let key = key_of(&path);
    let want = match mode {
        Mode::Input => 1,
        Mode::Output | Mode::Append => 2,
        Mode::Binary | Mode::Random => {
            if access == 0 {
                3
            } else {
                access
            }
        }
    };
    if access != 0 && access != want && mode != Mode::Binary && mode != Mode::Random {
        return Err(err(54));
    }
    if with(|st| {
        st.files.values().any(|f| {
            f.key == key
                && (matches!(f.mode, Mode::Output | Mode::Append)
                    || matches!(mode, Mode::Output | Mode::Append))
        })
    }) {
        return Err(err(55));
    }
    let reclen = match len {
        -1 => 128,
        1..=32767 => len as u64,
        32768.. => return Err(err(6)),
        _ => return Err(err(5)),
    };
    if path.is_dir() {
        return Err(err(75));
    }
    let mut o = OpenOptions::new();
    match mode {
        Mode::Input => o.read(true),
        Mode::Output => o.write(true).create(true).truncate(true),
        Mode::Append => o.append(true).create(true),
        Mode::Binary | Mode::Random => o
            .read(want & 1 != 0)
            .write(want & 2 != 0)
            .create(want & 2 != 0),
    };
    let mut file = o.open(&path).map_err(|e| io_error(&e, &path))?;
    let mut data = Vec::new();
    if mode == Mode::Input {
        file.read_to_end(&mut data)
            .map_err(|e| io_error(&e, &path))?;

        if let Some(z) = data.iter().position(|&c| c == 0x1A) {
            data.truncate(z);
        }
    }
    let pos = 0;
    with(|st| {
        st.files.insert(
            num,
            Open {
                key,
                file,
                mode,
                access: want,
                reclen,
                pos,
                col: 0,
                data,
                eof_hit: false,
                wbuf: Vec::new(),
                wopen: false,
                rec_start: 0,
            },
        )
    });
    Ok(Variant::Empty)
}

pub(super) fn close(ctx: &RtContext, a: &[Variant]) -> R {
    let mut nums = Vec::new();
    for v in a {
        nums.push(long(ctx, v)?);
    }
    with(|st| {
        if nums.is_empty() {
            st.files.clear();
        } else {
            for n in nums {
                st.files.remove(&n);
            }
        }
    });
    Ok(Variant::Empty)
}

pub(super) fn freefile(ctx: &RtContext, a: &[Variant]) -> R {
    let range = match a.first() {
        Some(v) if !matches!(v, Variant::Error(_)) => long(ctx, v)?,
        _ => 0,
    };
    let (lo, hi) = if range == 0 { (1, 255) } else { (256, 511) };
    let n = with(|st| (lo..=hi).find(|n| !st.files.contains_key(n)));
    match n {
        Some(n) => Ok(Variant::I2(n as i16)),
        None => Err(err(67)),
    }
}

fn with_file<T>(
    ctx: &RtContext,
    v: &Variant,
    f: impl FnOnce(&mut Open) -> Result<T, VbaError>,
) -> Result<T, VbaError> {
    let n = long(ctx, v).map_err(|_| err(52))?;
    with(|st| f(get_file(st, n)?))
}

pub(super) fn eof(ctx: &RtContext, a: &[Variant]) -> R {
    with_file(ctx, &a[0], |f| {
        Ok(Variant::Bool(match f.mode {
            Mode::Input => f.pos >= f.data.len() as u64,
            Mode::Output | Mode::Append => true,
            _ => f.eof_hit,
        }))
    })
}

pub(super) fn lof(ctx: &RtContext, a: &[Variant]) -> R {
    with_file(ctx, &a[0], |f| {
        let n = f.len();
        i32::try_from(n).map(Variant::I4).map_err(|_| err(6))
    })
}

pub(super) fn loc(ctx: &RtContext, a: &[Variant]) -> R {
    with_file(ctx, &a[0], |f| {
        let n = match f.mode {
            Mode::Random => f.pos / f.reclen,
            Mode::Binary => f.pos,
            Mode::Input => f.pos.div_ceil(128).max(1),
            Mode::Output | Mode::Append => f.pos / 128,
        };
        Ok(Variant::I4(n as i32))
    })
}

pub(super) fn seek_fn(ctx: &RtContext, a: &[Variant]) -> R {
    with_file(ctx, &a[0], |f| {
        let n = match f.mode {
            Mode::Random => f.pos / f.reclen + 1,
            _ => f.pos + 1,
        };
        Ok(Variant::I4(n as i32))
    })
}

pub(super) fn seek_stmt(ctx: &RtContext, a: &[Variant]) -> R {
    let to = long(ctx, &a[1])?;
    with_file(ctx, &a[0], |f| {
        if to < 1 {
            return Err(err(63));
        }
        let to = to as u64 - 1;
        f.eof_hit = false;
        f.pos = if f.mode == Mode::Random {
            to * f.reclen
        } else {
            to
        };
        Ok(Variant::Empty)
    })
}

fn civil(days: i64) -> (i64, i64, i64) {
    let z = days - 25_569 + 719_468;
    let era = z.div_euclid(146_097);
    let doe = z.rem_euclid(146_097);
    let yoe = (doe - doe / 1460 + doe / 36524 - doe / 146_096) / 365;
    let doy = doe - (365 * yoe + yoe / 4 - yoe / 100);
    let mp = (5 * doy + 2) / 153;
    let d = doy - (153 * mp + 2) / 5 + 1;
    let m = if mp < 10 { mp + 3 } else { mp - 9 };
    (yoe + era * 400 + i64::from(m <= 2), m, d)
}

fn write_date(d: f64) -> String {
    let days = d.floor();
    let secs = ((d - days) * 86400.0).round() as i64;
    let (y, m, dd) = civil(days as i64);
    let date = format!("{y:04}-{m:02}-{dd:02}");
    let time = format!("{:02}:{:02}:{:02}", secs / 3600, secs / 60 % 60, secs % 60);
    if secs == 0 {
        format!("#{date}#")
    } else {
        format!("#{date} {time}#")
    }
}

fn print_text(ctx: &RtContext, v: &Variant) -> Result<String, VbaError> {
    Ok(match v {
        Variant::Bstr(s) => s.clone(),
        Variant::BstrNull | Variant::Empty => String::new(),
        Variant::Null => "Null".to_string(),
        Variant::Bool(b) => if *b { "True" } else { "False" }.to_string(),
        Variant::Error(e) => format!("Error {}", *e as u32 & 0xFFFF),
        Variant::Date(_) => format!("{} ", text(ctx, v)?),
        Variant::Array(_) | Variant::Dispatch(_) | Variant::Unknown(_) => return Err(err(13)),
        n => {
            let t = text(ctx, n)?;
            if t.starts_with('-') {
                format!("{t} ")
            } else {
                format!(" {t} ")
            }
        }
    })
}

fn write_text(ctx: &RtContext, v: &Variant) -> Result<String, VbaError> {
    Ok(match v {
        Variant::Bstr(s) => format!("\"{}\"", s.replace('"', "\"\"")),
        Variant::BstrNull => "\"\"".to_string(),
        Variant::Empty => String::new(),
        Variant::Null => "#NULL#".to_string(),
        Variant::Bool(b) => if *b { "#TRUE#" } else { "#FALSE#" }.to_string(),
        Variant::Error(e) => format!("#ERROR {}#", *e as u32 & 0xFFFF),
        Variant::Date(d) => write_date(*d),
        Variant::Array(_) | Variant::Dispatch(_) | Variant::Unknown(_) => return Err(err(13)),
        n => {
            let t = text(ctx, n)?;
            if let Some(r) = t.strip_prefix("0.") {
                format!(".{r}")
            } else if let Some(r) = t.strip_prefix("-0.") {
                format!("-.{r}")
            } else {
                t
            }
        }
    })
}

fn put_bytes(f: &mut Open, bytes: &[u8]) -> Result<(), VbaError> {
    if matches!(f.mode, Mode::Output | Mode::Append) {
        f.wbuf.extend_from_slice(bytes);
    } else {
        f.file.seek(SeekFrom::Start(f.pos)).map_err(|_| err(75))?;
        f.file
            .write_all(bytes)
            .map_err(|e| match e.raw_os_error() {
                Some(112) => err(61),
                _ => err(75),
            })?;
    }
    f.pos += bytes.len() as u64;
    Ok(())
}

fn text_file(ctx: &RtContext, v: &Variant) -> Result<i32, VbaError> {
    file_no(ctx, v)
}

pub(super) fn print(ctx: &RtContext, a: &[Variant]) -> R {
    let n = text_file(ctx, &a[0])?;
    with(|st| {
        let f = get_file(st, n)?;
        if !matches!(f.mode, Mode::Output | Mode::Append) {
            return Err(err(54));
        }
        let mut out = String::new();
        let mut col = f.col;
        let mut newline = true;
        let mut i = 1;
        while i < a.len() {
            let marker = text(ctx, &a[i])?;
            i += 1;
            newline = true;
            match marker.as_str() {
                ";" => newline = false,
                "," => {
                    let pad = 14 - col % 14;
                    out.push_str(&" ".repeat(pad));
                    col += pad;
                    newline = false;
                }
                "v" => {
                    let t = print_text(ctx, &a[i])?;
                    i += 1;
                    col += units(&t).len();
                    out.push_str(&t);
                }
                "s" => {
                    let k = long(ctx, &a[i])?.max(0) as usize;
                    i += 1;
                    out.push_str(&" ".repeat(k));
                    col += k;
                }
                _ => {
                    let target = if matches!(a[i], Variant::Empty) {
                        col + 14 - col % 14
                    } else {
                        (long(ctx, &a[i])?.max(1) as usize) - 1
                    };
                    i += 1;
                    if target < col {
                        out.push_str("\r\n");
                        col = 0;
                    }
                    if target > col {
                        out.push_str(&" ".repeat(target - col));
                        col = target;
                    }
                }
            }
        }
        if newline {
            out.push_str("\r\n");
            col = 0;
        }
        f.col = col;
        put_bytes(f, &to_ansi(&out))?;
        Ok(Variant::Empty)
    })
}

pub(super) fn write(ctx: &RtContext, a: &[Variant]) -> R {
    let n = text_file(ctx, &a[0])?;
    with(|st| {
        let f = get_file(st, n)?;
        if !matches!(f.mode, Mode::Output | Mode::Append) {
            return Err(err(54));
        }
        let mut fields = Vec::new();
        let mut slot: Option<String> = None;
        let mut trailing = false;
        let mut i = 1;
        while i < a.len() {
            let marker = text(ctx, &a[i])?;
            i += 1;
            trailing = false;
            match marker.as_str() {
                "v" => {
                    slot = Some(write_text(ctx, &a[i])?);
                    i += 1;
                }
                "s" => {
                    let k = long(ctx, &a[i])?.max(0) as usize;
                    slot = Some(" ".repeat(k));
                    i += 1;
                }
                "t" => i += 1,
                _ => {
                    fields.push(slot.take().unwrap_or_default());
                    trailing = true;
                }
            }
        }
        if let Some(x) = slot {
            fields.push(x);
        }
        let mut line = String::new();
        if f.wopen {
            line.push(',');
        }
        line += &fields.join(",");
        if !trailing {
            line += "\r\n";
        }
        f.wopen = trailing;
        f.col = 0;
        put_bytes(f, &to_ansi(&line))?;
        Ok(Variant::Empty)
    })
}

pub(super) fn line_input(ctx: &RtContext, a: &[Variant]) -> R {
    let _ = ctx;
    with_file(ctx, &a[0], |f| {
        if f.mode != Mode::Input {
            return Err(err(54));
        }
        let start = f.pos as usize;
        if start >= f.data.len() {
            return Err(err(62));
        }
        let rest = &f.data[start..];
        let end = rest.iter().position(|&c| c == b'\r').unwrap_or(rest.len());
        let line = from_ansi(&rest[..end]).replace('\0', "");
        let mut next = end;
        if end < rest.len() {
            next += 1;
            if rest.get(end + 1) == Some(&b'\n') {
                next += 1;
            }
        }
        f.pos += next as u64;
        text_out(line)
    })
}

fn number_of(t: &str) -> Option<Variant> {
    let t = t.trim();
    let b = t.as_bytes();
    let mut i = 0;
    let neg = match b.first() {
        Some(b'-') => {
            i = 1;
            true
        }
        Some(b'+') => {
            i = 1;
            false
        }
        _ => false,
    };
    if b.get(i) == Some(&b'&') {
        let (radix, skip) = match b.get(i + 1).map(u8::to_ascii_lowercase) {
            Some(b'h') => (16, 2),
            Some(b'o') => (8, 2),
            _ => (8, 1),
        };
        let digits: String = t[i + skip..]
            .chars()
            .take_while(|c| c.is_digit(radix))
            .collect();
        let v = i64::from_str_radix(&digits, radix).unwrap_or(0);
        let v = if neg { -v } else { v };
        return Some(if (-32768..=65535).contains(&v) {
            Variant::I2(v as i16)
        } else {
            Variant::I4(v as i32)
        });
    }
    let start = i;
    while b.get(i).is_some_and(u8::is_ascii_digit) {
        i += 1;
    }
    let int_digits = i - start;
    let mut frac_digits = 0;
    if b.get(i) == Some(&b'.') {
        i += 1;
        let f0 = i;
        while b.get(i).is_some_and(u8::is_ascii_digit) {
            i += 1;
        }
        frac_digits = i - f0;
    }
    if int_digits + frac_digits == 0 {
        return None;
    }
    let mut has_exp = false;
    if matches!(b.get(i), Some(b'e' | b'E' | b'd' | b'D')) {
        let mut j = i + 1;
        if matches!(b.get(j), Some(b'+' | b'-')) {
            j += 1;
        }
        let e0 = j;
        while b.get(j).is_some_and(u8::is_ascii_digit) {
            j += 1;
        }
        if j > e0 {
            has_exp = true;
            i = j;
        }
    }
    let num: String = t[..i].replace(['d', 'D'], "e");
    let v: f64 = num.parse().ok()?;
    if has_exp {
        return Some(Variant::R8(v));
    }
    if frac_digits > 0 {
        let scaled = (v * 10000.0).round();
        if frac_digits <= 4 && (v * 10000.0 - scaled).abs() < 1e-6 && scaled.abs() < 9.2e18 {
            return Some(Variant::Cy(scaled as i64));
        }
        return Some(Variant::R8(v));
    }
    Some(if (-32768.0..=32767.0).contains(&v) {
        Variant::I2(v as i16)
    } else if (-2147483648.0..=2147483647.0).contains(&v) {
        Variant::I4(v as i32)
    } else {
        Variant::R8(v)
    })
}

fn days_of(y: i64, m: i64, d: i64) -> i64 {
    let y = if m <= 2 { y - 1 } else { y };
    let era = y.div_euclid(400);
    let yoe = y.rem_euclid(400);
    let mp = (m + 9) % 12;
    let doy = (153 * mp + 2) / 5 + d - 1;
    let doe = yoe * 365 + yoe / 4 - yoe / 100 + doy;
    era * 146_097 + doe - 719_468 + 25_569
}

fn hash_date(t: &str) -> Option<f64> {
    let t = t.trim();
    let (date, time) = match t.split_once(' ') {
        Some((d, tm)) => (Some(d), Some(tm)),
        None if t.contains(':') => (None, Some(t)),
        None => (Some(t), None),
    };
    let mut days = 0.0;
    if let Some(d) = date {
        let mut it = d.split('-');
        let y: i64 = it.next()?.parse().ok()?;
        let m: i64 = it.next()?.parse().ok()?;
        let dd: i64 = it.next()?.parse().ok()?;
        if it.next().is_some() || !(1..=12).contains(&m) || !(1..=31).contains(&dd) {
            return None;
        }
        days = days_of(y, m, dd) as f64;
    }
    if let Some(tm) = time {
        let mut it = tm.split(':');
        let h: f64 = it.next()?.parse().ok()?;
        let mi: f64 = it.next()?.parse().ok()?;
        let sec: f64 = it.next().map_or(Some(0.0), |x| x.parse().ok())?;
        days += (h * 3600.0 + mi * 60.0 + sec) / 86400.0;
    }
    Some(days)
}

fn field_variant(raw: &str) -> Variant {
    if let Some(inner) = raw.strip_prefix('#').and_then(|r| r.strip_suffix('#')) {
        match inner.to_ascii_uppercase().as_str() {
            "TRUE" => return Variant::Bool(true),
            "FALSE" => return Variant::Bool(false),
            "NULL" => return Variant::Null,
            u => {
                if let Some(code) = u.strip_prefix("ERROR ") {
                    if let Ok(n) = code.trim().parse::<i32>() {
                        return Variant::Error(0x800A_0000_u32 as i32 | n);
                    }
                }
                if let Some(d) = hash_date(inner) {
                    return Variant::Date(d);
                }
            }
        }
    }
    number_of(raw).unwrap_or_else(|| Variant::Bstr(raw.to_string()))
}

pub(super) fn input_field(ctx: &RtContext, a: &[Variant]) -> R {
    let code = long(ctx, &a[2])?;
    let (raw, quoted) = with_file(ctx, &a[0], |f| {
        if f.mode != Mode::Input {
            return Err(err(if f.mode == Mode::Binary && f.len() == 0 {
                62
            } else {
                54
            }));
        }
        let d = &f.data;
        let mut i = f.pos as usize;
        if i >= d.len() {
            return Err(err(62));
        }
        while i < d.len() && (d[i] == b' ' || d[i] == b'\t') {
            i += 1;
        }
        let mut out = Vec::new();
        let mut quoted = false;
        if d.get(i) == Some(&b'"') {
            quoted = true;
            i += 1;
            while i < d.len() {
                if d[i] == b'"' {
                    if d.get(i + 1) == Some(&b'"') {
                        out.push(b'"');
                        i += 2;
                        continue;
                    }
                    i += 1;
                    break;
                }
                out.push(d[i]);
                i += 1;
            }
            while i < d.len() && (d[i] == b' ' || d[i] == b'\t') {
                i += 1;
            }
        } else {
            while i < d.len() && !matches!(d[i], b',' | b'\r') {
                out.push(d[i]);
                i += 1;
            }
            while out.last() == Some(&b' ') {
                out.pop();
            }
        }
        match d.get(i) {
            Some(b',') => i += 1,
            Some(b'\r') if d.get(i + 1) == Some(&b'\n') => i += 2,
            Some(b'\r') => i += 1,
            _ => {}
        }
        f.pos = i as u64;
        Ok((from_ansi(&out), quoted))
    })?;
    if quoted || code == VarType::Bstr as i32 {
        let v = Variant::Bstr(raw);
        return if code == 12 || code == 8 {
            Ok(v)
        } else {
            convert(ctx, &v, to_var_type(code)?)
        };
    }
    if code == 12 && raw.is_empty() {
        return Ok(Variant::Empty);
    }
    let v = field_variant(&raw);
    if code == 12 {
        return Ok(v);
    }
    if matches!(v, Variant::Bstr(_)) {
        return if code == 7 {
            Err(err(6))
        } else {
            Ok(zero_of(code))
        };
    }
    if code == 7 && !matches!(v, Variant::Date(_)) && raw.trim().parse::<f64>().is_err() {
        return Err(err(6));
    }
    if code == 11 && !matches!(v, Variant::Bool(_)) {
        return Err(err(6));
    }
    convert(ctx, &v, to_var_type(code)?)
}

fn to_var_type(code: i32) -> Result<VarType, VbaError> {
    Ok(match code {
        2 => VarType::I2,
        3 => VarType::I4,
        4 => VarType::R4,
        5 => VarType::R8,
        6 => VarType::Cy,
        7 => VarType::Date,
        8 => VarType::Bstr,
        11 => VarType::Bool,
        12 => VarType::Variant,
        14 => VarType::Decimal,
        17 => VarType::Ui1,
        20 => VarType::I8,
        _ => return Err(err(13)),
    })
}

fn zero_of(code: i32) -> Variant {
    match code {
        2 => Variant::I2(0),
        3 => Variant::I4(0),
        4 => Variant::R4(0.0),
        5 => Variant::R8(0.0),
        11 => Variant::Bool(false),
        17 => Variant::Ui1(0),
        20 => Variant::I8(0),
        _ => Variant::Empty,
    }
}

fn size_of(code: i32) -> Option<usize> {
    Some(match code {
        2 | 11 => 2,
        3 | 4 => 4,
        5 | 6 | 7 | 20 => 8,
        14 => 16,
        17 => 1,
        _ => return None,
    })
}

fn scalar_bytes(code: i32, v: &Variant) -> Vec<u8> {
    match (code, v) {
        (_, Variant::I2(x)) => x.to_le_bytes().to_vec(),
        (_, Variant::I4(x)) => x.to_le_bytes().to_vec(),
        (_, Variant::R4(x)) => x.to_le_bytes().to_vec(),
        (_, Variant::R8(x) | Variant::Date(x)) => x.to_le_bytes().to_vec(),
        (_, Variant::Cy(x)) => x.to_le_bytes().to_vec(),
        (_, Variant::I8(x)) => x.to_le_bytes().to_vec(),
        (_, Variant::Ui1(x)) => vec![*x],
        (_, Variant::Decimal(d)) => {
            let mut b = vec![14, 0, d.scale, if d.negative { 0x80 } else { 0 }];
            b.extend(((d.mantissa >> 64) as u32).to_le_bytes());
            b.extend((d.mantissa as u64).to_le_bytes());
            b
        }
        (_, Variant::Bool(x)) => (if *x { -1i16 } else { 0 }).to_le_bytes().to_vec(),
        _ => Vec::new(),
    }
}

fn encode(ctx: &RtContext, mode: Mode, code: i32, v: &Variant) -> Result<Vec<u8>, VbaError> {
    if code & 0x2000 != 0 {
        let Variant::Array(arr) = v else {
            return Err(err(13));
        };
        let mut out = Vec::new();
        for e in &arr.data {
            out.extend(encode(ctx, mode, code & 0xFFF, e)?);
        }
        return Ok(out);
    }
    if code == 12 {
        let tag = match v {
            Variant::Empty => 0,
            Variant::Null => 1,
            Variant::I2(_) => 2,
            Variant::I4(_) => 3,
            Variant::R4(_) => 4,
            Variant::R8(_) => 5,
            Variant::Cy(_) => 6,
            Variant::Date(_) => 7,
            Variant::Bstr(_) | Variant::BstrNull => 8,
            Variant::Bool(_) => 11,
            Variant::Ui1(_) => 17,
            Variant::I8(_) => 20,
            Variant::Decimal(_) => 14,
            Variant::Array(arr) => {
                let elem = if arr.elem == VarType::Variant {
                    12
                } else {
                    arr.elem as i32
                };
                let mut out = ((0x2000 | elem) as u16).to_le_bytes().to_vec();
                out.extend((arr.bounds.len() as u16).to_le_bytes());
                for (lo, n) in &arr.bounds {
                    out.extend(n.to_le_bytes());
                    out.extend(lo.to_le_bytes());
                }
                for e in &arr.data {
                    out.extend(encode(ctx, mode, elem, e)?);
                }
                return Ok(out);
            }
            _ => return Err(err(13)),
        };
        let mut out = (tag as u16).to_le_bytes().to_vec();
        match v {
            Variant::Bstr(_) | Variant::BstrNull => out.extend(encode(ctx, Mode::Random, 8, v)?),
            v => out.extend(scalar_bytes(tag, v)),
        }
        return Ok(out);
    }
    if code == 8 {
        let s = match v {
            Variant::BstrNull => Vec::new(),
            v => to_ansi(&text(ctx, v)?),
        };

        return Ok(if mode == Mode::Random {
            let mut out = (s.len() as u16).to_le_bytes().to_vec();
            out.extend(s);
            out
        } else {
            s
        });
    }
    let t = to_var_type(code)?;
    let v = convert(ctx, v, t)?;
    Ok(scalar_bytes(code, &v))
}

fn record_arg(ctx: &RtContext, v: &Variant) -> Result<Option<i32>, VbaError> {
    if matches!(v, Variant::Empty) {
        Ok(None)
    } else {
        long(ctx, v).map(Some)
    }
}

fn piece_arg(ctx: &RtContext, v: &Variant) -> Result<(Option<i32>, u8), VbaError> {
    match v {
        Variant::Ui1(p) => Ok((None, *p)),
        v => Ok((record_arg(ctx, v)?, 0)),
    }
}

pub(super) fn put(ctx: &RtContext, a: &[Variant]) -> R {
    let (rec, mut piece) = piece_arg(ctx, &a[1])?;
    let mut code = long(ctx, &a[3])?;
    if code & 0x1000 != 0 {
        code &= !0x1000;
        piece |= 32;
    }
    with_file(ctx, &a[0], |f| {
        if !matches!(f.mode, Mode::Binary | Mode::Random) {
            return Err(err(54));
        }
        if f.access & 2 == 0 {
            return Err(err(75));
        }

        let mut bytes = if piece & 32 != 0 && code == 8 {
            encode(ctx, Mode::Binary, code, &a[2])?
        } else {
            encode(ctx, f.mode, code, &a[2])?
        };
        if piece & 16 != 0 && code == 8 && f.mode == Mode::Binary {
            let n = (bytes.len() as u16).to_le_bytes();
            bytes.splice(0..0, n);
        }
        let kind = piece & 15;
        seek_record(f, rec)?;
        f.eof_hit = false;
        if matches!(kind, 0 | 1 | 4) {
            f.rec_start = f.pos;
        }
        if f.mode == Mode::Random && f.pos - f.rec_start + bytes.len() as u64 > f.reclen {
            return Err(err(59));
        }
        put_bytes(f, &bytes)?;
        if f.mode == Mode::Random && matches!(kind, 0 | 3 | 4) {
            f.pos = f.rec_start + f.reclen;
        }
        Ok(Variant::Empty)
    })
}

fn seek_record(f: &mut Open, rec: Option<i32>) -> Result<(), VbaError> {
    match rec {
        None => Ok(()),
        Some(r) if r >= 1 => {
            f.pos = if f.mode == Mode::Random {
                (r as u64 - 1) * f.reclen
            } else {
                r as u64 - 1
            };
            Ok(())
        }
        Some(_) => Err(err(63)),
    }
}

fn read_exact(f: &mut Open, n: usize) -> Result<Vec<u8>, VbaError> {
    let len = f.len();
    let mut buf = vec![0; n];
    if f.pos < len {
        f.file.seek(SeekFrom::Start(f.pos)).map_err(|_| err(75))?;
        let mut got = 0;
        while got < n {
            match f.file.read(&mut buf[got..]) {
                Ok(0) | Err(_) => break,
                Ok(k) => got += k,
            }
        }
    }
    if f.pos + n as u64 > len {
        f.eof_hit = true;
    }
    f.pos += n as u64;
    Ok(buf)
}

fn decode_scalar(code: i32, b: &[u8]) -> Variant {
    match code {
        2 => Variant::I2(i16::from_le_bytes([b[0], b[1]])),
        3 => Variant::I4(i32::from_le_bytes([b[0], b[1], b[2], b[3]])),
        4 => Variant::R4(f32::from_le_bytes([b[0], b[1], b[2], b[3]])),
        5 => Variant::R8(f64::from_le_bytes(b[..8].try_into().unwrap_or_default())),
        6 => Variant::Cy(i64::from_le_bytes(b[..8].try_into().unwrap_or_default())),
        7 => Variant::Date(f64::from_le_bytes(b[..8].try_into().unwrap_or_default())),
        11 => Variant::Bool(i16::from_le_bytes([b[0], b[1]]) != 0),
        14 => {
            let hi = u32::from_le_bytes(b[4..8].try_into().unwrap_or_default());
            let lo = u64::from_le_bytes(b[8..16].try_into().unwrap_or_default());
            Variant::Decimal(xlcalc_variant::Decimal {
                negative: b[3] & 0x80 != 0,
                scale: b[2],
                mantissa: (u128::from(hi) << 64) | u128::from(lo),
            })
        }
        17 => Variant::Ui1(b[0]),
        20 => Variant::I8(i64::from_le_bytes(b[..8].try_into().unwrap_or_default())),
        _ => Variant::Empty,
    }
}

fn decode(f: &mut Open, code: i32, hint: &Variant) -> Result<Variant, VbaError> {
    if code == 12 {
        let tag = i32::from(u16::from_le_bytes(
            read_exact(f, 2)?.try_into().unwrap_or_default(),
        ));
        if tag & 0x2000 != 0 {
            let elem = tag & 0xFFF;
            let dims = u16::from_le_bytes(read_exact(f, 2)?.try_into().unwrap_or_default());
            let mut bounds = Vec::new();
            let mut total = 1usize;
            for _ in 0..dims {
                let b = read_exact(f, 8)?;
                let n = u32::from_le_bytes(b[..4].try_into().unwrap_or_default());
                let lo = i32::from_le_bytes(b[4..].try_into().unwrap_or_default());
                total = total.saturating_mul(n as usize);
                bounds.push((lo, n));
            }
            if total > 1 << 24 {
                return Err(err(7));
            }
            let mut data = Vec::new();
            for _ in 0..total {
                data.push(decode(f, elem, &Variant::Empty)?);
            }
            return Ok(Variant::Array(Box::new(SafeArray {
                elem: to_var_type(elem)?,
                bounds,
                data,
            })));
        }
        return match tag {
            0 => Ok(Variant::Empty),
            1 => Ok(Variant::Null),
            8 => decode(f, 8, &Variant::Empty).map(|v| match v {
                Variant::Bstr(s) if s.is_empty() => Variant::BstrNull,
                v => v,
            }),
            t => decode(f, t, hint),
        };
    }
    if code == 8 {
        let n = if f.mode == Mode::Random || matches!(hint, Variant::Empty) {
            u16::from_le_bytes(read_exact(f, 2)?.try_into().unwrap_or_default()) as usize
        } else {
            match hint {
                Variant::Bstr(s) => units(s).len(),
                _ => 0,
            }
        };
        let bytes = read_exact(f, n)?;
        return Ok(Variant::Bstr(from_ansi(&bytes)));
    }
    let n = size_of(code).ok_or_else(|| err(13))?;
    let b = read_exact(f, n)?;
    Ok(decode_scalar(code, &b))
}

pub(super) fn get(ctx: &RtContext, a: &[Variant]) -> R {
    let (rec, mut piece) = piece_arg(ctx, &a[1])?;
    let mut code = long(ctx, &a[3])?;
    if code & 0x1000 != 0 {
        code &= !0x1000;
        piece |= 32;
    }
    with_file(ctx, &a[0], |f| {
        if !matches!(f.mode, Mode::Binary | Mode::Random) {
            return Err(err(54));
        }
        if f.access & 1 == 0 {
            return Err(err(75));
        }
        let kind = piece & 15;
        seek_record(f, rec)?;
        f.eof_hit = false;
        if matches!(kind, 0 | 1 | 4) {
            f.rec_start = f.pos;
        }
        let prefixed = piece & 16 != 0 && code == 8 && f.mode == Mode::Binary;
        let v = if piece & 32 != 0 && code == 8 {
            let n = match &a[2] {
                Variant::Bstr(s) => units(s).len(),
                _ => 0,
            };
            Variant::Bstr(from_ansi(&read_exact(f, n)?))
        } else if prefixed {
            let n = u16::from_le_bytes(read_exact(f, 2)?.try_into().unwrap_or_default());
            let hint = Variant::Bstr("\0".repeat(usize::from(n)));
            decode(f, 8, &hint)?
        } else if code & 0x2000 != 0 {
            let Variant::Array(arr) = &a[2] else {
                return Err(err(13));
            };
            let mut arr: SafeArray = (**arr).clone();
            let elem = code & 0xFFF;
            for e in &mut arr.data {
                *e = decode(f, elem, e)?;
            }
            Variant::Array(Box::new(arr))
        } else {
            decode(f, code, &a[2])?
        };
        if f.mode == Mode::Random && matches!(kind, 0 | 3 | 4) {
            f.pos = f.rec_start + f.reclen;
        }
        Ok(v)
    })
}

fn matches_pattern(name: &str, pat: &str) -> bool {
    fn go(n: &[char], p: &[char]) -> bool {
        match p.split_first() {
            None => n.is_empty(),
            Some(('*', rest)) => (0..=n.len()).any(|i| go(&n[i..], rest)),
            Some(('?', rest)) => !n.is_empty() && go(&n[1..], rest),
            Some((c, rest)) => n.first().is_some_and(|x| x == c) && go(&n[1..], rest),
        }
    }
    let n: Vec<char> = name.to_lowercase().chars().collect();
    let p: Vec<char> = pat.to_lowercase().chars().collect();
    go(&n, &p)
}

fn split_pattern(p: &str) -> (PathBuf, String) {
    let path = full_path(p);
    let name = path
        .file_name()
        .map(|n| n.to_string_lossy().into_owned())
        .unwrap_or_default();
    (
        path.parent().map(Path::to_path_buf).unwrap_or_default(),
        name,
    )
}

pub(super) fn kill(ctx: &RtContext, a: &[Variant]) -> R {
    let p = text(ctx, &a[0])?;
    if p.is_empty() {
        return Err(err(53));
    }
    let (dir, name) = split_pattern(&p);
    let mut hits = Vec::new();
    if name.contains(['*', '?']) {
        let rd = fs::read_dir(&dir).map_err(|_| err(53))?;
        for e in rd.flatten() {
            let n = e.file_name().to_string_lossy().into_owned();
            if e.path().is_file() && matches_pattern(&n, &name) {
                hits.push(e.path());
            }
        }
    } else {
        let path = dir.join(&name);
        if path.is_dir() {
            return Err(err(53));
        }
        hits.push(path);
    }
    if hits.is_empty() {
        return Err(err(53));
    }
    for path in hits {
        if with(|st| is_open(st, &key_of(&path))) {
            return Err(err(55));
        }
        let meta = fs::metadata(&path).map_err(|_| err(53))?;
        if meta.permissions().readonly() {
            return Err(err(75));
        }
        fs::remove_file(&path).map_err(|e| match e.kind() {
            std::io::ErrorKind::NotFound => err(53),
            _ => io_error(&e, &path),
        })?;
        with(|st| st.attrs.remove(&key_of(&path)));
    }
    Ok(Variant::Empty)
}

pub(super) fn name(ctx: &RtContext, a: &[Variant]) -> R {
    let (old, new) = (full_path(&text(ctx, &a[0])?), full_path(&text(ctx, &a[1])?));
    if !old.exists() {
        return Err(err(53));
    }
    if key_of(&old) == key_of(&new) {
        return Ok(Variant::Empty);
    }
    if new.exists() {
        return Err(err(58));
    }
    if with(|st| is_open(st, &key_of(&old))) {
        return Err(err(55));
    }
    if new.parent().is_some_and(|d| !d.exists()) {
        return Err(err(53));
    }
    fs::rename(&old, &new).map_err(|e| io_error(&e, &new))?;
    with(|st| st.attrs.remove(&key_of(&old)));
    Ok(Variant::Empty)
}

pub(super) fn file_copy(ctx: &RtContext, a: &[Variant]) -> R {
    let (src, dst) = (full_path(&text(ctx, &a[0])?), full_path(&text(ctx, &a[1])?));
    if with(|st| is_open(st, &key_of(&dst))) {
        return Err(err(55));
    }
    fs::copy(&src, &dst).map_err(|e| io_error(&e, &src))?;
    Ok(Variant::Empty)
}

pub(super) fn mkdir(ctx: &RtContext, a: &[Variant]) -> R {
    let p = text(ctx, &a[0])?;
    if p.is_empty() {
        return Err(err(76));
    }
    let path = full_path(&p);
    if path.exists() {
        return Err(err(75));
    }
    fs::create_dir(&path).map_err(|e| io_error(&e, &path))?;
    Ok(Variant::Empty)
}

pub(super) fn rmdir(ctx: &RtContext, a: &[Variant]) -> R {
    let p = text(ctx, &a[0])?;
    let path = full_path(&p);
    if !path.is_dir() {
        return Err(err(76));
    }
    fs::remove_dir(&path).map_err(|e| match e.raw_os_error() {
        Some(145) => err(75),
        _ => io_error(&e, &path),
    })?;
    Ok(Variant::Empty)
}

pub(super) fn file_len(ctx: &RtContext, a: &[Variant]) -> R {
    let p = text(ctx, &a[0])?;
    if p.is_empty() {
        return Err(err(53));
    }
    let path = full_path(&p);
    let m = fs::metadata(&path).map_err(|e| io_error(&e, &path))?;
    i32::try_from(m.len()).map(Variant::I4).map_err(|_| err(6))
}

pub(super) fn get_attr(ctx: &RtContext, a: &[Variant]) -> R {
    let p = text(ctx, &a[0])?;
    let path = full_path(&p);
    let m = fs::metadata(&path).map_err(|e| io_error(&e, &path))?;
    if let Some(n) = with(|st| st.attrs.get(&key_of(&path)).copied()) {
        return Ok(Variant::I2((n | if m.is_dir() { 16 } else { 0 }) as i16));
    }
    let mut n = 0;
    if m.permissions().readonly() {
        n |= 1;
    }
    n |= if m.is_dir() { 16 } else { 32 };
    Ok(Variant::I2(n))
}

pub(super) fn set_attr(ctx: &RtContext, a: &[Variant]) -> R {
    let p = text(ctx, &a[0])?;
    let attr = long(ctx, &a[1])?;
    let path = full_path(&p);
    let m = fs::metadata(&path).map_err(|e| io_error(&e, &path))?;
    let mut perm = m.permissions();
    perm.set_readonly(attr & 1 != 0);
    fs::set_permissions(&path, perm).map_err(|e| io_error(&e, &path))?;
    with(|st| st.attrs.insert(key_of(&path), attr & 0x27));
    Ok(Variant::Empty)
}

pub(super) fn dir(ctx: &RtContext, a: &[Variant]) -> R {
    let given = match a.first() {
        Some(v) if !matches!(v, Variant::Error(_)) => Some(text(ctx, v)?),
        _ => None,
    };
    let attr = match a.get(1) {
        Some(v) if !matches!(v, Variant::Error(_)) => long(ctx, v)?,
        _ => 0,
    };
    if attr & !0x3F != 0 {
        return Err(err(5));
    }
    if let Some(p) = given {
        let names = list(&p, attr);
        with(|st| st.dir = Some(names.into_iter()));
    }
    let next = with(|st| st.dir.as_mut().map(Iterator::next));
    match next {
        None => Err(err(5)),
        Some(Some(n)) => text_out(n),
        Some(None) => {
            with(|st| st.dir = None);
            text_out(String::new())
        }
    }
}

fn dir_visible(path: &Path, is_dir: bool, attr: i32) -> bool {
    let set = with(|st| st.attrs.get(&key_of(path)).copied()).unwrap_or(0);
    let own = (set & 6) | if is_dir { 16 } else { 0 };
    own & !attr == 0
}

fn list(p: &str, attr: i32) -> Vec<String> {
    if p.is_empty() {
        return Vec::new();
    }
    let (dir, pat) = split_pattern(p);
    let mut out = Vec::new();
    if !pat.contains(['*', '?']) {
        let path = dir.join(&pat);
        if let Ok(m) = fs::metadata(&path) {
            if dir_visible(&path, m.is_dir(), attr) {
                let actual = path
                    .file_name()
                    .map(|n| n.to_string_lossy().into_owned())
                    .unwrap_or(pat);
                out.push(actual);
            }
        }
        return out;
    }
    if let Ok(rd) = fs::read_dir(&dir) {
        for e in rd.flatten() {
            let n = e.file_name().to_string_lossy().into_owned();
            let path = e.path();
            if dir_visible(&path, path.is_dir(), attr) && matches_pattern(&n, &pat) {
                out.push(n);
            }
        }
        if attr & 16 != 0 && dir.parent().is_some() {
            for n in [".", ".."] {
                if matches_pattern(n, &pat) {
                    out.push(n.to_string());
                }
            }
        }
    }
    out.sort_by_key(|n| n.to_lowercase());
    out
}

pub(super) fn cur_dir(_: &RtContext, _: &[Variant]) -> R {
    let d = std::env::current_dir().unwrap_or_default();
    text_out(d.to_string_lossy().into_owned())
}

pub(super) fn environ(ctx: &RtContext, a: &[Variant]) -> R {
    if is_null(&a[0]) {
        return Err(err(94));
    }
    let v = match &a[0] {
        Variant::Bstr(s) => std::env::var(s).unwrap_or_default(),
        n => {
            let k = long(ctx, n)?;
            if k < 1 {
                return Err(err(5));
            }
            match std::env::vars().nth(k as usize - 1) {
                Some((name, val)) => format!("{name}={val}"),
                None => String::new(),
            }
        }
    };
    text_out(v)
}

pub(super) fn input_chars(ctx: &RtContext, a: &[Variant]) -> R {
    let n = long(ctx, &a[0])?;
    if n < 1 {
        return Err(err(5));
    }
    with_file(ctx, &a[1], |f| {
        if f.mode != Mode::Input {
            return Err(err(if f.mode == Mode::Binary && f.len() == 0 {
                62
            } else {
                54
            }));
        }

        let mut out = Vec::new();
        let mut i = f.pos as usize;
        while out.len() < n as usize && i < f.data.len() {
            if f.data[i] != 0 {
                out.push(f.data[i]);
            }
            i += 1;
        }
        f.pos = i as u64;
        if out.len() < n as usize {
            return Err(err(62));
        }
        text_out(from_ansi(&out))
    })
}
