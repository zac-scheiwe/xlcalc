pub mod cfb;
pub mod ovba;
pub mod write;

use std::fmt;

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum VbaError {
    Cfb(String),

    MissingStream(String),

    Decompress(String),

    Dir(String),

    UnsupportedCodePage(u16),

    BadText(String),
}

impl fmt::Display for VbaError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            VbaError::Cfb(m) => write!(f, "compound file: {m}"),
            VbaError::MissingStream(s) => write!(f, "stream {s} not found"),
            VbaError::Decompress(m) => write!(f, "decompression: {m}"),
            VbaError::Dir(m) => write!(f, "dir stream: {m}"),
            VbaError::UnsupportedCodePage(c) => write!(f, "unsupported code page {c}"),
            VbaError::BadText(m) => write!(f, "text: {m}"),
        }
    }
}

impl std::error::Error for VbaError {}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum ModuleKind {
    Standard,

    Class,

    Document,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Module {
    pub name: String,

    pub stream_name: String,
    pub kind: ModuleKind,

    pub text_offset: u32,

    pub source: Result<String, VbaError>,
}

impl Module {
    pub fn code(&self) -> Result<String, VbaError> {
        let source = self.source.as_ref().map_err(Clone::clone)?;
        let mut out = String::new();
        for line in source.split_inclusive("\r\n") {
            if !is_attribute_line(line) {
                out.push_str(line);
            }
        }
        Ok(out)
    }
}

fn is_attribute_line(line: &str) -> bool {
    line.get(..10)
        .is_some_and(|p| p.eq_ignore_ascii_case("Attribute "))
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct VbaProject {
    pub name: String,

    pub code_page: u16,

    pub modules: Vec<Module>,
}

impl VbaProject {
    pub fn parse(bytes: Vec<u8>) -> Result<VbaProject, VbaError> {
        let cfb = cfb::Cfb::new(bytes)?;
        let dir = ovba::decompress(&cfb.read_stream("VBA/dir")?)?;
        let info = parse_dir(&dir)?;
        let kinds = match cfb.read_stream("PROJECT") {
            Ok(p) => project_kinds(&p, info.code_page)?,
            Err(VbaError::MissingStream(_)) => Vec::new(),
            Err(e) => return Err(e),
        };
        let mut modules = Vec::new();
        for r in info.modules {
            let stream = cfb.read_stream(&format!("VBA/{}", r.stream_name))?;
            let source = read_source(&stream, r.offset, info.code_page, &r.name);
            let kind = if r.procedural {
                ModuleKind::Standard
            } else {
                match kinds
                    .iter()
                    .find(|(n, _)| n.eq_ignore_ascii_case(&r.name))
                    .map(|k| k.1)
                {
                    Some(ModuleKind::Document) => ModuleKind::Document,
                    _ => ModuleKind::Class,
                }
            };
            modules.push(Module {
                source,
                name: r.name,
                stream_name: r.stream_name,
                kind,
                text_offset: r.offset,
            });
        }
        Ok(VbaProject {
            name: info.name,
            code_page: info.code_page,
            modules,
        })
    }

    pub fn document_module(&self, code_name: &str) -> Option<&Module> {
        self.modules
            .iter()
            .find(|m| m.kind == ModuleKind::Document && m.name.eq_ignore_ascii_case(code_name))
    }
}

fn read_source(stream: &[u8], offset: u32, code_page: u16, name: &str) -> Result<String, VbaError> {
    let packed = stream
        .get(offset as usize..)
        .ok_or_else(|| VbaError::Dir(format!("module {name}: text offset past its stream")))?;
    let raw = if packed.is_empty() {
        Vec::new()
    } else {
        ovba::decompress(packed)?
    };
    decode(code_page, &raw)
}

pub fn decode(code_page: u16, bytes: &[u8]) -> Result<String, VbaError> {
    match code_page {
        1252 => Ok(bytes.iter().map(|&b| cp1252(b)).collect()),
        65001 => String::from_utf8(bytes.to_vec()).map_err(|e| VbaError::BadText(e.to_string())),
        other => Err(VbaError::UnsupportedCodePage(other)),
    }
}

fn cp1252(b: u8) -> char {
    const HIGH: [char; 32] = [
        '\u{20AC}', '\u{81}', '\u{201A}', '\u{0192}', '\u{201E}', '\u{2026}', '\u{2020}',
        '\u{2021}', '\u{02C6}', '\u{2030}', '\u{0160}', '\u{2039}', '\u{0152}', '\u{8D}',
        '\u{017D}', '\u{8F}', '\u{90}', '\u{2018}', '\u{2019}', '\u{201C}', '\u{201D}', '\u{2022}',
        '\u{2013}', '\u{2014}', '\u{02DC}', '\u{2122}', '\u{0161}', '\u{203A}', '\u{0153}',
        '\u{9D}', '\u{017E}', '\u{0178}',
    ];
    match b {
        0x80..=0x9F => HIGH[usize::from(b - 0x80)],
        _ => char::from(b),
    }
}

struct ModRec {
    name: String,
    stream_name: String,
    offset: u32,
    procedural: bool,
}

struct DirInfo {
    name: String,
    code_page: u16,
    modules: Vec<ModRec>,
}

fn dir_err(m: &str) -> VbaError {
    VbaError::Dir(m.to_string())
}

fn utf16(b: &[u8]) -> String {
    String::from_utf16_lossy(&cfb::words16(b))
}

fn parse_dir(dir: &[u8]) -> Result<DirInfo, VbaError> {
    let mut pos = 0usize;
    let mut code_page: Option<u16> = None;
    let mut name = Vec::new();
    let mut modules: Vec<ModRec> = Vec::new();

    let mut raw_names: Vec<(Vec<u8>, Vec<u8>)> = Vec::new();
    let mut cur: Option<(ModRec, Vec<u8>, Vec<u8>, bool)> = None;
    let mut in_modules = false;
    let mut declared = 0usize;
    while pos < dir.len() {
        let id = dir
            .get(pos..pos + 2)
            .map(|b| u16::from_le_bytes([b[0], b[1]]))
            .ok_or_else(|| dir_err("truncated record id"))?;
        let size = dir
            .get(pos + 2..pos + 6)
            .map(|b| u32::from_le_bytes([b[0], b[1], b[2], b[3]]))
            .ok_or_else(|| dir_err("truncated record size"))? as usize;
        pos += 6;
        let data = dir
            .get(pos..pos + size)
            .ok_or_else(|| dir_err("record runs past the end"))?;
        pos += size;
        let u32d = || {
            data.get(..4)
                .map(|b| u32::from_le_bytes([b[0], b[1], b[2], b[3]]))
                .ok_or_else(|| dir_err("short record"))
        };
        match id {
            0x0009 => pos += 2,
            0x0003 if !in_modules => {
                code_page = Some(
                    data.get(..2)
                        .map(|b| u16::from_le_bytes([b[0], b[1]]))
                        .ok_or_else(|| dir_err("short code page"))?,
                );
            }
            0x0004 if !in_modules => name = data.to_vec(),
            0x000F => {
                in_modules = true;
                declared = usize::from(
                    data.get(..2)
                        .map(|b| u16::from_le_bytes([b[0], b[1]]))
                        .ok_or_else(|| dir_err("short module count"))?,
                );
            }

            0x0019 if in_modules => {
                if let Some((m, n, s, _)) = cur.take() {
                    modules.push(m);
                    raw_names.push((n, s));
                }
                cur = Some((
                    ModRec {
                        name: String::new(),
                        stream_name: String::new(),
                        offset: 0,
                        procedural: false,
                    },
                    data.to_vec(),
                    Vec::new(),
                    false,
                ));
            }
            _ if in_modules && cur.is_some() => {
                let Some((m, _, s, us)) = cur.as_mut() else {
                    continue;
                };
                match id {
                    0x0047 => m.name = utf16(data),
                    0x001A => {
                        if !*us {
                            *s = data.to_vec();
                        }
                    }
                    0x0032 => {
                        m.stream_name = utf16(data);
                        *us = true;
                    }
                    0x0031 => m.offset = u32d()?,
                    0x0021 => m.procedural = true,
                    _ => {}
                }
            }
            _ => {}
        }
    }
    if let Some((m, n, s, _)) = cur.take() {
        modules.push(m);
        raw_names.push((n, s));
    }
    let code_page = code_page.ok_or_else(|| dir_err("no PROJECTCODEPAGE"))?;
    if modules.len() != declared {
        return Err(VbaError::Dir(format!(
            "{declared} modules declared, {} found",
            modules.len()
        )));
    }
    for (m, (n, s)) in modules.iter_mut().zip(raw_names) {
        if m.name.is_empty() {
            m.name = decode(code_page, &n)?;
        }
        if m.stream_name.is_empty() {
            m.stream_name = decode(code_page, &s)?;
        }
    }
    Ok(DirInfo {
        name: decode(code_page, &name)?,
        code_page,
        modules,
    })
}

fn project_kinds(stream: &[u8], code_page: u16) -> Result<Vec<(String, ModuleKind)>, VbaError> {
    let text = decode(code_page, stream)?;
    let mut out = Vec::new();
    for line in text.lines() {
        let Some((key, val)) = line.split_once('=') else {
            continue;
        };
        let kind = match key {
            "Document" => ModuleKind::Document,
            "Class" => ModuleKind::Class,
            "Module" => ModuleKind::Standard,
            _ => continue,
        };

        let name = val.split('/').next().unwrap_or(val);
        out.push((name.to_string(), kind));
    }
    Ok(out)
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn code_pages() {
        assert_eq!(
            decode(1252, b"a\x80\xe9\x9f").unwrap(),
            "a\u{20AC}\u{e9}\u{178}"
        );
        assert_eq!(
            decode(932, b"x").unwrap_err(),
            VbaError::UnsupportedCodePage(932)
        );
    }

    #[test]
    fn attribute_lines_hidden() {
        let m = Module {
            name: "M".into(),
            stream_name: "M".into(),
            kind: ModuleKind::Standard,
            text_offset: 0,
            source: Ok("Attribute VB_Name = \"M\"\r\nSub A()\r\nEnd Sub\r\n".into()),
        };
        assert_eq!(m.code().unwrap(), "Sub A()\r\nEnd Sub\r\n");
    }

    #[test]
    fn rejects_non_cfb() {
        assert!(VbaProject::parse(b"PK\x03\x04".to_vec()).is_err());
    }
}
