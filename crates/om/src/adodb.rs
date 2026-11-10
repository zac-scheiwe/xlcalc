use std::fs;

use xlcalc_variant::{change_type, HResult, Locale, SafeArray, VarType, Variant};
use xlcalc_vba::{InvokeKind, VbaError};

use crate::adodb_tables::{ASCII_ENCODE, CHARSETS, L1_ENCODE, L9_ENCODE, W1252_ENCODE};
use crate::comobj::{index, is_com, ComObj};

pub(crate) fn ado_error(code: u32, text: &str) -> VbaError {
    VbaError {
        number: (0x800A_0000 | code) as i32,
        description: text.to_string(),
    }
}

pub(crate) fn closed() -> VbaError {
    ado_error(
        0x0E78,
        "Operation is not allowed when the object is closed.",
    )
}

pub(crate) fn opened() -> VbaError {
    ado_error(0x0E79, "Operation is not allowed when the object is open.")
}

pub(crate) fn bad_arg() -> VbaError {
    ado_error(
        0x0BB9,
        "Arguments are of the wrong type, are out of acceptable range, or are in conflict with one another.",
    )
}

pub(crate) fn context() -> VbaError {
    ado_error(0x0C93, "Operation is not allowed in this context.")
}

pub(crate) fn missing(v: &Variant) -> bool {
    matches!(v, Variant::Error(e) if *e == HResult::DISP_E_PARAMNOTFOUND.0)
}

pub(crate) fn opt(args: &[Variant], i: usize) -> Option<&Variant> {
    args.get(i).filter(|v| !missing(v))
}

pub(crate) fn type_mismatch() -> VbaError {
    VbaError {
        number: 0x8002_0005_u32 as i32,
        description: "Type mismatch.".into(),
    }
}

pub(crate) fn long_arg(v: &Variant) -> Result<i32, VbaError> {
    match change_type(v, VarType::I4, Locale::EN_US, 0) {
        Ok(Variant::I4(n)) => Ok(n),
        _ => Err(type_mismatch()),
    }
}

pub(crate) fn text_arg(v: &Variant) -> Result<String, VbaError> {
    match v.clone().canon() {
        Variant::Bstr(s) => Ok(s),
        Variant::Empty => Ok(String::new()),
        Variant::Array(a) if a.elem == VarType::Ui1 && a.bounds.len() == 1 => {
            if a.data.is_empty() {
                return Err(bad_arg());
            }
            let bytes: Vec<u8> = a
                .data
                .iter()
                .map(|x| if let Variant::Ui1(b) = x { *b } else { 0 })
                .collect();
            let units: Vec<u16> = bytes
                .as_chunks::<2>()
                .0
                .iter()
                .map(|c| u16::from_le_bytes(*c))
                .collect();
            Ok(String::from_utf16_lossy(&units))
        }
        Variant::Null | Variant::Array(_) => Err(type_mismatch()),
        other => match change_type(&other, VarType::Bstr, Locale::EN_US, 0) {
            Ok(Variant::Bstr(s)) => Ok(s),
            _ => Err(type_mismatch()),
        },
    }
}

pub(crate) fn arity(args: &[Variant], lo: usize, hi: usize) -> Result<(), VbaError> {
    if (lo..=hi).contains(&args.len()) {
        Ok(())
    } else {
        Err(VbaError::new(450))
    }
}

#[derive(Clone, Copy, PartialEq, Eq, Debug)]
enum Codec {
    Utf8,
    Utf16Le,
    Utf16Be,

    Utf16Feff,
    Ascii,
    W1252,
    L1,
    L9,
    Other,
}

const CP1252_HIGH: [u16; 32] = [
    0x20AC, 0x81, 0x201A, 0x0192, 0x201E, 0x2026, 0x2020, 0x2021, 0x02C6, 0x2030, 0x0160, 0x2039,
    0x0152, 0x8D, 0x017D, 0x8F, 0x90, 0x2018, 0x2019, 0x201C, 0x201D, 0x2022, 0x2013, 0x2014,
    0x02DC, 0x2122, 0x0161, 0x203A, 0x0153, 0x9D, 0x017E, 0x0178,
];

const LATIN9: [(u8, u16); 8] = [
    (0xA4, 0x20AC),
    (0xA6, 0x0160),
    (0xA8, 0x0161),
    (0xB4, 0x017D),
    (0xB8, 0x017E),
    (0xBC, 0x0152),
    (0xBD, 0x0153),
    (0xBE, 0x0178),
];

#[derive(Clone, Debug)]
pub(crate) struct Stream {
    open: bool,
    ty: i32,
    mode: i32,
    charset: String,
    line_sep: i32,
    data: Vec<u8>,
    pos: usize,
}

impl Default for Stream {
    fn default() -> Stream {
        Stream {
            open: false,
            ty: 2,
            mode: 0,
            charset: "Unicode".into(),
            line_sep: -1,
            data: Vec::new(),
            pos: 0,
        }
    }
}

fn valid_mode(m: i32) -> bool {
    (0..=0x1F).contains(&m) || m == 0x0040_0000 || m == 0x0080_0000
}

fn canonical_charset(name: &str) -> String {
    match name.to_ascii_lowercase().as_str() {
        "unicode" | "utf-16" | "utf-16le" | "unicodefffe" => "Unicode".into(),
        "unicodefeff" => "UnicodeFEFF".into(),
        _ => name.to_string(),
    }
}

fn io_error() -> VbaError {
    ado_error(0x0BBC, "Write to file failed.")
}

impl Stream {
    fn codec(&self) -> Codec {
        match self.charset.to_ascii_lowercase().as_str() {
            "utf-8" => Codec::Utf8,
            "unicode" => Codec::Utf16Le,
            "utf-16be" => Codec::Utf16Be,
            "unicodefeff" => Codec::Utf16Feff,
            "ascii" | "us-ascii" => Codec::Ascii,
            "windows-1252" | "x-user-defined" => Codec::W1252,
            "iso-8859-1" | "latin1" => Codec::L1,
            "iso-8859-15" => Codec::L9,
            _ => Codec::Other,
        }
    }

    fn bom(&self) -> &'static [u8] {
        match self.charset.to_ascii_lowercase().as_str() {
            "utf-8" => &[0xEF, 0xBB, 0xBF],
            "unicode" => &[0xFF, 0xFE],
            "unicodefeff" => &[0xFE, 0xFF],
            _ => &[],
        }
    }

    fn check_open(&self) -> Result<(), VbaError> {
        if self.open {
            Ok(())
        } else {
            Err(closed())
        }
    }

    fn can_read(&self) -> bool {
        self.mode & 3 != 2
    }

    fn encode(&self, units: &[u16]) -> Result<Vec<u8>, VbaError> {
        let single = |table: &[(u16, u8)]| -> Vec<u8> {
            units
                .iter()
                .map(|&u| {
                    if u < 0x80 {
                        u as u8
                    } else {
                        table
                            .iter()
                            .find(|(c, _)| *c == u)
                            .map_or(b'?', |(_, b)| *b)
                    }
                })
                .collect()
        };
        Ok(match self.codec() {
            Codec::Utf8 => String::from_utf16_lossy(units).into_bytes(),
            Codec::Utf16Le => units.iter().flat_map(|u| u.to_le_bytes()).collect(),
            Codec::Utf16Be | Codec::Utf16Feff => {
                units.iter().flat_map(|u| u.to_be_bytes()).collect()
            }
            Codec::Ascii => single(ASCII_ENCODE),
            Codec::W1252 => single(W1252_ENCODE),
            Codec::L1 => single(L1_ENCODE),
            Codec::L9 => single(L9_ENCODE),
            Codec::Other => return Err(bad_arg()),
        })
    }

    fn decode(&self, bytes: &[u8], be: Option<bool>) -> Result<Vec<u16>, VbaError> {
        Ok(match self.codec() {
            Codec::Utf8 => String::from_utf8_lossy(bytes).encode_utf16().collect(),
            Codec::Utf16Le | Codec::Utf16Be | Codec::Utf16Feff => bytes
                .as_chunks::<2>()
                .0
                .iter()
                .map(|&c| {
                    if be.unwrap_or(self.codec() != Codec::Utf16Le) {
                        u16::from_be_bytes(c)
                    } else {
                        u16::from_le_bytes(c)
                    }
                })
                .collect(),
            Codec::Ascii => bytes.iter().map(|&b| u16::from(b & 0x7F)).collect(),
            Codec::W1252 | Codec::L1 => bytes
                .iter()
                .map(|&b| {
                    if (0x80..0xA0).contains(&b) {
                        CP1252_HIGH[usize::from(b - 0x80)]
                    } else {
                        u16::from(b)
                    }
                })
                .collect(),
            Codec::L9 => bytes
                .iter()
                .map(|&b| {
                    LATIN9
                        .iter()
                        .find(|(c, _)| *c == b)
                        .map_or(u16::from(b), |(_, u)| *u)
                })
                .collect(),
            Codec::Other => return Err(bad_arg()),
        })
    }

    fn encoded_len(&self, units: &[u16]) -> usize {
        match self.codec() {
            Codec::Utf8 => String::from_utf16_lossy(units).len(),
            Codec::Utf16Le | Codec::Utf16Be | Codec::Utf16Feff => 2 * units.len(),
            _ => units.len(),
        }
    }

    fn put_bytes(&mut self, at: usize, bytes: &[u8]) {
        if self.data.len() < at {
            self.data.resize(at, 0);
        }
        let end = at + bytes.len();
        if self.data.len() < end {
            self.data.resize(end, 0);
        }
        self.data[at..end].copy_from_slice(bytes);
    }

    fn write_units(&mut self, units: &[u16], tail: &[u8]) -> Result<(), VbaError> {
        let mut bytes = self.encode(units)?;
        bytes.extend_from_slice(tail);
        let bom = self.bom();
        let mut start = self.pos;
        if self.pos == 0 && !bom.is_empty() {
            if !self.data.starts_with(bom) {
                self.put_bytes(0, bom);
            }
            start = bom.len();
        }
        self.put_bytes(start, &bytes);
        self.pos = start + bytes.len();
        Ok(())
    }

    fn remaining_units(&self) -> Result<(Vec<u16>, usize), VbaError> {
        let bom = self.bom();
        let mut start = self.pos;
        let mut be = None;
        if self.pos == 0 {
            if matches!(self.codec(), Codec::Utf16Le | Codec::Utf16Feff) {
                if self.data.starts_with(&[0xFF, 0xFE]) {
                    start = 2;
                    be = Some(false);
                } else if self.data.starts_with(&[0xFE, 0xFF]) {
                    start = 2;
                    be = Some(true);
                }
            } else if !bom.is_empty() && self.data.starts_with(bom) {
                start = bom.len();
            }
        }
        Ok((
            self.decode(&self.data[start.min(self.data.len())..], be)?,
            start,
        ))
    }

    fn read_units(&mut self, n: i32) -> Result<Vec<u16>, VbaError> {
        let (units, start) = self.remaining_units()?;
        if n == 0 {
            return Ok(Vec::new());
        }
        let (take, consumed) = match n {
            -1 => (units.len(), None),
            -2 => {
                let sep: &[u16] = match self.line_sep {
                    13 => &[13],
                    10 => &[10],
                    _ => &[13, 10],
                };
                match units.windows(sep.len()).position(|w| w == sep) {
                    Some(i) => (i, Some(i + sep.len())),
                    None => (units.len(), None),
                }
            }
            n => {
                let k = (n as usize).min(units.len());
                (k, Some(k))
            }
        };
        self.pos = match consumed {
            Some(c) => start + self.encoded_len(&units[..c]),
            None => self.data.len(),
        };
        Ok(units[..take].to_vec())
    }

    fn units_to_string(units: &[u16]) -> Variant {
        Variant::Bstr(String::from_utf16_lossy(units))
    }

    fn open_call(&mut self, args: &[Variant]) -> Result<Variant, VbaError> {
        arity(args, 0, 5)?;
        if self.open {
            return Err(opened());
        }
        match opt(args, 0) {
            None => {}
            Some(Variant::Bstr(s)) if s.to_ascii_lowercase().starts_with("url=") => {
                return Err(VbaError {
                    number: 0x8004_0E19_u32 as i32,
                    description: "Object or data matching the name, range, or selection criteria was not found within the scope of this operation.".into(),
                });
            }
            Some(_) => return Err(bad_arg()),
        }
        if let Some(m) = opt(args, 1) {
            if long_arg(m)? != 0 {
                return Err(bad_arg());
            }
        }
        if let Some(o) = opt(args, 2) {
            if long_arg(o)? != -1 {
                return Err(bad_arg());
            }
        }
        self.open = true;
        self.data.clear();
        self.pos = 0;
        Ok(Variant::Empty)
    }

    fn save_to_file(&mut self, args: &[Variant]) -> Result<Variant, VbaError> {
        arity(args, 1, 2)?;
        self.check_open()?;
        if !self.can_read() {
            return Err(context());
        }
        let path = text_arg(&args[0])?;
        let how = opt(args, 1).map_or(Ok(1), long_arg)?;
        if path.is_empty() || !(1..=2).contains(&how) {
            return Err(bad_arg());
        }
        if how == 1 && fs::metadata(&path).is_ok() {
            return Err(io_error());
        }
        fs::write(&path, &self.data).map_err(|_| io_error())?;
        self.pos = 0;
        Ok(Variant::Empty)
    }

    fn load_from_file(&mut self, args: &[Variant]) -> Result<Variant, VbaError> {
        arity(args, 1, 1)?;
        self.check_open()?;
        if self.mode & 3 == 1 {
            return Err(context());
        }
        let path = text_arg(&args[0])?;
        if path.is_empty() {
            return Err(bad_arg());
        }
        let mut bytes =
            fs::read(&path).map_err(|_| ado_error(0x0BBA, "File could not be opened."))?;
        let bom = self.bom();
        if self.ty == 2 && !bom.is_empty() && !bytes.starts_with(bom) {
            let mut with = bom.to_vec();
            with.append(&mut bytes);
            bytes = with;
        }
        self.data = bytes;
        self.pos = 0;
        Ok(Variant::Empty)
    }

    fn line_call(&mut self, args: &[Variant], skip: bool) -> Result<Variant, VbaError> {
        self.check_open()?;
        if self.ty != 2 || !self.can_read() {
            return Err(context());
        }
        if skip {
            arity(args, 0, 0)?;
            self.read_units(-2)?;
            return Ok(Variant::Empty);
        }
        arity(args, 0, 1)?;
        let n = opt(args, 0).map_or(Ok(-1), long_arg)?;
        if n < -2 {
            return Err(bad_arg());
        }
        Ok(Self::units_to_string(&self.read_units(n)?))
    }

    fn read_call(&mut self, args: &[Variant]) -> Result<Variant, VbaError> {
        arity(args, 0, 1)?;
        self.check_open()?;
        if self.ty != 1 || !self.can_read() {
            return Err(context());
        }
        let n = opt(args, 0).map_or(Ok(-1), long_arg)?;
        if n < -1 {
            return Err(bad_arg());
        }
        let rest = self.data.len() - self.pos;
        let take = if n == -1 {
            rest
        } else {
            (n as usize).min(rest)
        };
        if take == 0 {
            return Ok(Variant::Null);
        }
        let bytes = self.data[self.pos..self.pos + take].to_vec();
        self.pos += take;
        Ok(byte_array(&bytes))
    }

    fn write_call(&mut self, args: &[Variant]) -> Result<Variant, VbaError> {
        arity(args, 1, 1)?;
        self.check_open()?;
        if self.ty != 1 || self.mode & 3 == 1 {
            return Err(context());
        }
        let Some(bytes) = array_bytes(&args[0]) else {
            return Err(bad_arg());
        };
        let at = self.pos;
        self.put_bytes(at, &bytes);
        self.pos += bytes.len();
        Ok(Variant::Empty)
    }

    fn write_text_call(&mut self, args: &[Variant]) -> Result<Variant, VbaError> {
        arity(args, 1, 2)?;
        self.check_open()?;
        if self.ty != 2 {
            return Err(context());
        }
        if self.mode & 3 == 1 {
            return Err(VbaError {
                number: 0x8003_0005_u32 as i32,
                description: "Access Denied.".into(),
            });
        }
        let text = text_arg(&args[0])?;
        let how = opt(args, 1).map_or(Ok(0), long_arg)?;
        let mut units: Vec<u16> = text.encode_utf16().collect();
        let mut tail: Vec<u8> = Vec::new();
        match how {
            0 => {}
            1 => {
                let sep: &[u8] = match self.line_sep {
                    13 => b"\r",
                    10 => b"\n",
                    _ => b"\r\n",
                };

                if matches!(self.codec(), Codec::Utf16Le | Codec::Utf16Feff) {
                    units.extend(sep.iter().map(|&b| u16::from(b)));
                } else {
                    tail.extend_from_slice(sep);
                }
            }
            _ => return Err(bad_arg()),
        }
        self.write_units(&units, &tail)?;
        Ok(Variant::Empty)
    }

    fn put_type(&mut self, v: &Variant) -> Result<(), VbaError> {
        let t = long_arg(v)?;
        if t != 1 && t != 2 {
            return Err(bad_arg());
        }
        if self.open && self.pos != 0 {
            return Err(context());
        }
        self.ty = t;
        Ok(())
    }

    fn put_charset(&mut self, v: &Variant) -> Result<(), VbaError> {
        let name = text_arg(v)?;
        if self.ty != 2 {
            return Err(context());
        }
        if !CHARSETS.contains(&name.to_ascii_lowercase().as_str()) {
            return Err(bad_arg());
        }
        if self.open && self.pos != 0 {
            return Err(context());
        }
        self.charset = canonical_charset(&name);
        Ok(())
    }

    pub(crate) fn call(
        &mut self,
        name: &str,
        kind: InvokeKind,
        args: &[Variant],
    ) -> Result<Variant, VbaError> {
        let name = name.to_ascii_lowercase();
        if matches!(kind, InvokeKind::Put | InvokeKind::PutRef) {
            if args.len() != 1 {
                return Err(VbaError::new(450));
            }
            let v = &args[0];
            match name.as_str() {
                "type" => self.put_type(v)?,
                "charset" => self.put_charset(v)?,
                "mode" => {
                    if self.open {
                        return Err(opened());
                    }
                    let m = long_arg(v)?;
                    if !valid_mode(m) {
                        return Err(bad_arg());
                    }
                    self.mode = m;
                }
                "lineseparator" => {
                    let n = long_arg(v)?;
                    if ![-1, 10, 13].contains(&n) {
                        return Err(bad_arg());
                    }
                    self.line_sep = n;
                }
                "position" => {
                    self.check_open()?;
                    let n = long_arg(v)?;
                    if n < 0 {
                        return Err(bad_arg());
                    }
                    if n as usize > self.data.len() {
                        return Err(VbaError {
                            number: 0x8007_0057_u32 as i32,
                            description: "The parameter is incorrect.".into(),
                        });
                    }
                    self.pos = n as usize;
                }
                "size" | "eos" | "state" => return Err(VbaError::new(438)),
                _ => return Err(VbaError::new(438)),
            }
            return Ok(Variant::Empty);
        }
        match name.as_str() {
            "type" => {
                arity(args, 0, 0)?;
                Ok(Variant::I4(self.ty))
            }
            "state" => {
                arity(args, 0, 0)?;
                Ok(Variant::I4(i32::from(self.open)))
            }
            "mode" => {
                arity(args, 0, 0)?;
                Ok(Variant::I4(self.mode))
            }
            "lineseparator" => {
                arity(args, 0, 0)?;
                Ok(Variant::I4(self.line_sep))
            }
            "charset" => {
                arity(args, 0, 0)?;
                if self.ty != 2 {
                    return Err(context());
                }
                Ok(Variant::Bstr(self.charset.clone()))
            }
            "size" => {
                arity(args, 0, 0)?;
                self.check_open()?;
                Ok(Variant::I4(self.data.len() as i32))
            }
            "position" => {
                arity(args, 0, 0)?;
                self.check_open()?;
                Ok(Variant::I4(self.pos as i32))
            }
            "eos" => {
                arity(args, 0, 0)?;
                self.check_open()?;
                Ok(Variant::Bool(self.pos >= self.data.len()))
            }
            "open" => self.open_call(args),
            "close" => {
                arity(args, 0, 0)?;
                self.check_open()?;
                self.open = false;
                self.data.clear();
                self.pos = 0;
                Ok(Variant::Empty)
            }
            "flush" | "cancel" => {
                arity(args, 0, 0)?;
                self.check_open()?;
                Ok(Variant::Empty)
            }
            "seteos" => {
                arity(args, 0, 0)?;
                self.check_open()?;
                self.data.truncate(self.pos);
                Ok(Variant::Empty)
            }
            "read" => self.read_call(args),
            "readtext" => self.line_call(args, false),
            "skipline" => self.line_call(args, true),
            "write" => self.write_call(args),
            "writetext" => self.write_text_call(args),
            "loadfromfile" => self.load_from_file(args),
            "savetofile" => self.save_to_file(args),
            _ => Err(VbaError::new(438)),
        }
    }
}

pub(crate) fn byte_array(bytes: &[u8]) -> Variant {
    Variant::Array(Box::new(SafeArray {
        elem: VarType::Ui1,
        bounds: vec![(0, bytes.len() as u32)],
        data: bytes.iter().map(|&b| Variant::Ui1(b)).collect(),
    }))
}

fn array_bytes(v: &Variant) -> Option<Vec<u8>> {
    match v {
        Variant::Array(a) if a.elem == VarType::Ui1 && a.bounds.len() == 1 => a
            .data
            .iter()
            .map(|x| match x {
                Variant::Ui1(b) => Some(*b),
                _ => None,
            })
            .collect(),
        _ => None,
    }
}

impl crate::Application<'_> {
    pub(crate) fn stream_call(
        &mut self,
        i: usize,
        name: &str,
        kind: InvokeKind,
        args: &[Variant],
    ) -> Result<Variant, VbaError> {
        if kind == InvokeKind::Get && name.eq_ignore_ascii_case("copyto") {
            return self.stream_copy_to(i, args);
        }
        match self.com.get_mut(i) {
            Some(ComObj::AdoStream(s)) => s.call(name, kind, args),
            _ => Err(VbaError::new(424)),
        }
    }

    fn stream_copy_to(&mut self, i: usize, args: &[Variant]) -> Result<Variant, VbaError> {
        arity(args, 1, 2)?;
        let Some(ComObj::AdoStream(src)) = self.com.get(i) else {
            return Err(VbaError::new(424));
        };
        src.check_open()?;
        let dest = match &args[0] {
            Variant::Dispatch(Some(id)) | Variant::Unknown(Some(id)) if is_com(*id) => index(*id),
            Variant::Dispatch(_) | Variant::Unknown(_) => return Err(VbaError::new(91)),
            _ => return Err(VbaError::new(13)),
        };
        let Some(ComObj::AdoStream(d)) = self.com.get(dest) else {
            return Err(bad_arg());
        };
        if !d.open {
            return Err(bad_arg());
        }
        let n = opt(args, 1).map_or(Ok(-1), long_arg)?;
        if n < -1 {
            return Err(bad_arg());
        }
        let (src_ty, dest_ty) = (src.ty, d.ty);
        if !src.can_read() || (src_ty == 1 && dest_ty == 2) {
            return Err(context());
        }
        let Some(ComObj::AdoStream(src)) = self.com.get_mut(i) else {
            return Err(VbaError::new(424));
        };
        if src_ty == 2 && dest_ty == 2 {
            let units = src.read_units(if n < 0 { -1 } else { n })?;
            if !units.is_empty() {
                let Some(ComObj::AdoStream(d)) = self.com.get_mut(dest) else {
                    return Err(VbaError::new(424));
                };
                d.write_units(&units, &[])?;
            }
        } else {
            let rest = src.data.len() - src.pos;
            let take = if n < 0 { rest } else { (n as usize).min(rest) };
            let bytes = src.data[src.pos..src.pos + take].to_vec();
            src.pos += take;
            if !bytes.is_empty() {
                let Some(ComObj::AdoStream(d)) = self.com.get_mut(dest) else {
                    return Err(VbaError::new(424));
                };
                let at = d.pos;
                d.put_bytes(at, &bytes);
                d.pos += bytes.len();
            }
        }
        Ok(Variant::Empty)
    }
}
