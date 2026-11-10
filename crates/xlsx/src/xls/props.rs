use crate::xlsb::esc;
use crate::xml::Node;
use std::collections::BTreeMap;
use std::fmt::Write;

pub(super) const SUMMARY: &str = "\u{5}SummaryInformation";
pub(super) const DOC_SUMMARY: &str = "\u{5}DocumentSummaryInformation";

const FMT_SUMMARY: [u8; 16] = [
    0xE0, 0x85, 0x9F, 0xF2, 0xF9, 0x4F, 0x68, 0x10, 0xAB, 0x91, 0x08, 0x00, 0x2B, 0x27, 0xB3, 0xD9,
];
const FMT_DOC: [u8; 16] = [
    0x02, 0xD5, 0xCD, 0xD5, 0x9C, 0x2E, 0x1B, 0x10, 0x93, 0x97, 0x08, 0x00, 0x2B, 0x2C, 0xF9, 0xAE,
];
const FMT_CUSTOM: [u8; 16] = [
    0x05, 0xD5, 0xCD, 0xD5, 0x9C, 0x2E, 0x1B, 0x10, 0x93, 0x97, 0x08, 0x00, 0x2B, 0x2C, 0xF9, 0xAE,
];

#[derive(Clone, Debug, PartialEq)]
pub(super) enum Val {
    Str(String),
    I4(i32),
    U4(u32),
    R8(f64),
    Bool(bool),
    Time(u64),
}

struct Section {
    fmt: [u8; 16],
    props: BTreeMap<u32, Val>,
    names: BTreeMap<u32, String>,
}

fn u16at(b: &[u8], i: usize) -> Option<u16> {
    Some(u16::from_le_bytes(b.get(i..i + 2)?.try_into().ok()?))
}

fn u32at(b: &[u8], i: usize) -> Option<u32> {
    Some(u32::from_le_bytes(b.get(i..i + 4)?.try_into().ok()?))
}

fn decode_ansi(b: &[u8], cp: u16) -> String {
    let b = b.split(|&c| c == 0).next().unwrap_or(b);
    if cp == 65001 {
        String::from_utf8_lossy(b).into_owned()
    } else {
        b.iter().map(|&c| char::from(c)).collect()
    }
}

fn sections(b: &[u8]) -> Vec<Section> {
    let mut out = Vec::new();
    let Some(n) = u32at(b, 24) else { return out };
    for i in 0..n.min(4) as usize {
        let at = 28 + i * 20;
        let (Some(off), Some(fmt)) = (u32at(b, at + 16), b.get(at..at + 16)) else {
            break;
        };
        let off = off as usize;
        let Some(count) = u32at(b, off + 4) else {
            break;
        };
        let mut s = Section {
            fmt: fmt.try_into().unwrap_or([0; 16]),
            props: BTreeMap::new(),
            names: BTreeMap::new(),
        };
        let mut cp = 1252u16;
        let mut list: Vec<(u32, usize)> = Vec::new();
        for k in 0..count.min(256) as usize {
            let (Some(id), Some(po)) = (u32at(b, off + 8 + k * 8), u32at(b, off + 12 + k * 8))
            else {
                break;
            };
            list.push((id, off + po as usize));
        }

        for &(id, p) in &list {
            if id == 1 {
                if let Some(v) = u16at(b, p + 4) {
                    cp = v;
                }
            }
        }
        for &(id, p) in &list {
            if id == 0 {
                let Some(cnt) = u32at(b, p) else { continue };
                let mut q = p + 4;
                for _ in 0..cnt.min(1024) {
                    let (Some(pid), Some(len)) = (u32at(b, q), u32at(b, q + 4)) else {
                        break;
                    };
                    let len = len as usize;
                    q += 8;
                    if cp == 1200 {
                        let Some(raw) = b.get(q..q + len * 2) else {
                            break;
                        };
                        let u: Vec<u16> = raw
                            .as_chunks::<2>()
                            .0
                            .iter()
                            .map(|c| u16::from_le_bytes(*c))
                            .take_while(|&c| c != 0)
                            .collect();
                        s.names.insert(pid, String::from_utf16_lossy(&u));
                        q += (len * 2).div_ceil(4) * 4;
                    } else {
                        let Some(raw) = b.get(q..q + len) else { break };
                        s.names.insert(pid, decode_ansi(raw, cp));
                        q += len;
                    }
                }
                continue;
            }
            let Some(ty) = u16at(b, p) else { continue };
            let v = match ty & 0xFFF {
                0x02 => u16at(b, p + 4).map(|v| Val::I4(i32::from(v as i16))),
                0x03 => u32at(b, p + 4).map(|v| Val::I4(v as i32)),
                0x13 => u32at(b, p + 4).map(Val::U4),
                0x05 => b
                    .get(p + 4..p + 12)
                    .and_then(|x| x.try_into().ok())
                    .map(|x| Val::R8(f64::from_le_bytes(x))),
                0x0B => u16at(b, p + 4).map(|v| Val::Bool(v != 0)),
                0x40 => b
                    .get(p + 4..p + 12)
                    .and_then(|x| x.try_into().ok())
                    .map(|x| Val::Time(u64::from_le_bytes(x))),
                0x1E => {
                    let len = u32at(b, p + 4).unwrap_or(0) as usize;
                    b.get(p + 8..p + 8 + len)
                        .map(|r| Val::Str(decode_ansi(r, cp)))
                }
                0x1F => {
                    let len = u32at(b, p + 4).unwrap_or(0) as usize;
                    b.get(p + 8..p + 8 + len * 2).map(|raw| {
                        let u: Vec<u16> = raw
                            .as_chunks::<2>()
                            .0
                            .iter()
                            .map(|c| u16::from_le_bytes(*c))
                            .take_while(|&c| c != 0)
                            .collect();
                        Val::Str(String::from_utf16_lossy(&u))
                    })
                }
                _ => None,
            };
            if let Some(v) = v {
                s.props.insert(id, v);
            }
        }
        out.push(s);
    }
    out
}

fn w3c(ft: u64) -> String {
    let secs = (ft / 10_000_000) as i64 - 11_644_473_600;
    let days = secs.div_euclid(86_400);
    let rem = secs.rem_euclid(86_400);

    let z = days + 719_468;
    let era = z.div_euclid(146_097);
    let doe = z.rem_euclid(146_097);
    let yoe = (doe - doe / 1460 + doe / 36_524 - doe / 146_096) / 365;
    let y = yoe + era * 400;
    let doy = doe - (365 * yoe + yoe / 4 - yoe / 100);
    let mp = (5 * doy + 2) / 153;
    let d = doy - (153 * mp + 2) / 5 + 1;
    let m = if mp < 10 { mp + 3 } else { mp - 9 };
    let y = if m <= 2 { y + 1 } else { y };
    format!(
        "{y:04}-{m:02}-{d:02}T{:02}:{:02}:{:02}Z",
        rem / 3600,
        rem % 3600 / 60,
        rem % 60
    )
}

fn filetime(s: &str) -> Option<u64> {
    let s = s.trim().trim_end_matches('Z');
    let (date, time) = s.split_once('T').unwrap_or((s, "00:00:00"));
    let mut d = date.split('-').map(|x| x.parse::<i64>().ok());
    let (y, m, day) = (d.next()??, d.next()??, d.next()??);
    let mut t = time.split(':').map(|x| x.parse::<i64>().ok());
    let (hh, mm, ss) = (
        t.next()??,
        t.next().flatten().unwrap_or(0),
        t.next().flatten().unwrap_or(0),
    );
    let y2 = if m <= 2 { y - 1 } else { y };
    let era = y2.div_euclid(400);
    let yoe = y2.rem_euclid(400);
    let mp = if m > 2 { m - 3 } else { m + 9 };
    let doy = (153 * mp + 2) / 5 + day - 1;
    let doe = yoe * 365 + yoe / 4 - yoe / 100 + doy;
    let days = era * 146_097 + doe - 719_468;
    let secs = days * 86_400 + hh * 3600 + mm * 60 + ss + 11_644_473_600;
    u64::try_from(secs).ok().map(|v| v * 10_000_000)
}

pub(super) fn to_parts(summary: Option<&[u8]>, doc: Option<&[u8]>) -> Vec<(&'static str, String)> {
    let mut core = String::new();
    let mut app = String::new();
    let mut custom = String::new();
    let text = |v: &Val| match v {
        Val::Str(s) => Some(s.clone()),
        _ => None,
    };
    if let Some(b) = summary {
        for s in sections(b).iter().filter(|s| s.fmt == FMT_SUMMARY) {
            for (id, tag) in [
                (2u32, "dc:title"),
                (3, "dc:subject"),
                (4, "dc:creator"),
                (5, "cp:keywords"),
                (6, "dc:description"),
                (8, "cp:lastModifiedBy"),
            ] {
                if let Some(t) = s.props.get(&id).and_then(text) {
                    let _ = write!(core, "<{tag}>{}</{tag}>", esc(&t));
                }
            }
            for (id, tag) in [(12u32, "dcterms:created"), (13, "dcterms:modified")] {
                if let Some(Val::Time(t)) = s.props.get(&id) {
                    if *t > 0 {
                        let _ = write!(
                            core,
                            "<{tag} xsi:type=\"dcterms:W3CDTF\">{}</{tag}>",
                            w3c(*t)
                        );
                    }
                }
            }
        }
    }
    if let Some(b) = doc {
        let secs = sections(b);
        for s in secs.iter().filter(|s| s.fmt == FMT_DOC) {
            if let Some(t) = s.props.get(&2).and_then(text) {
                let _ = write!(core, "<cp:category>{}</cp:category>", esc(&t));
            }
            for (id, tag) in [(14u32, "Manager"), (15, "Company")] {
                if let Some(t) = s.props.get(&id).and_then(text) {
                    let _ = write!(app, "<{tag}>{}</{tag}>", esc(&t));
                }
            }
        }
        let mut pid = 2;
        for s in secs.iter().filter(|s| s.fmt == FMT_CUSTOM) {
            for (id, v) in &s.props {
                let Some(name) = s.names.get(id) else {
                    continue;
                };
                let body = match v {
                    Val::Str(t) => format!("<vt:lpwstr>{}</vt:lpwstr>", esc(t)),
                    Val::I4(n) => format!("<vt:i4>{n}</vt:i4>"),
                    Val::U4(n) => format!("<vt:ui4>{n}</vt:ui4>"),
                    Val::R8(n) => format!("<vt:r8>{n}</vt:r8>"),
                    Val::Bool(n) => format!("<vt:bool>{}</vt:bool>", u8::from(*n)),
                    Val::Time(t) => format!("<vt:filetime>{}</vt:filetime>", w3c(*t)),
                };
                let _ = write!(
                    custom,
                    "<property fmtid=\"{{D5CDD505-2E9C-101B-9397-08002B2CF9AE}}\" pid=\"{pid}\" name=\"{}\">{body}</property>",
                    esc(name)
                );
                pid += 1;
            }
        }
    }
    let mut out = Vec::new();
    if !core.is_empty() {
        out.push((
            "docProps/core.xml",
            format!("<?xml version=\"1.0\" encoding=\"UTF-8\" standalone=\"yes\"?>\n<cp:coreProperties xmlns:cp=\"http://schemas.openxmlformats.org/package/2006/metadata/core-properties\" xmlns:dc=\"http://purl.org/dc/elements/1.1/\" xmlns:dcterms=\"http://purl.org/dc/terms/\" xmlns:dcmitype=\"http://purl.org/dc/dcmitype/\" xmlns:xsi=\"http://www.w3.org/2001/XMLSchema-instance\">{core}</cp:coreProperties>"),
        ));
    }
    if !app.is_empty() {
        out.push((
            "docProps/app.xml",
            format!("<?xml version=\"1.0\" encoding=\"UTF-8\" standalone=\"yes\"?>\n<Properties xmlns=\"http://schemas.openxmlformats.org/officeDocument/2006/extended-properties\" xmlns:vt=\"http://schemas.openxmlformats.org/officeDocument/2006/docPropsVTypes\">{app}</Properties>"),
        ));
    }
    if !custom.is_empty() {
        out.push((
            "docProps/custom.xml",
            format!("<?xml version=\"1.0\" encoding=\"UTF-8\" standalone=\"yes\"?>\n<Properties xmlns=\"http://schemas.openxmlformats.org/officeDocument/2006/custom-properties\" xmlns:vt=\"http://schemas.openxmlformats.org/officeDocument/2006/docPropsVTypes\">{custom}</Properties>"),
        ));
    }
    out
}

fn pad4(v: &mut Vec<u8>) {
    while !v.len().is_multiple_of(4) {
        v.push(0);
    }
}

fn put_val(v: &mut Vec<u8>, val: &Val, unicode: bool) {
    match val {
        Val::Str(s) => {
            if unicode {
                v.extend(0x1Fu32.to_le_bytes());
                let u: Vec<u16> = s.encode_utf16().chain([0]).collect();
                v.extend((u.len() as u32).to_le_bytes());
                for c in u {
                    v.extend(c.to_le_bytes());
                }
            } else {
                v.extend(0x1Eu32.to_le_bytes());
                v.extend((s.len() as u32 + 1).to_le_bytes());
                v.extend(s.chars().map(|c| c as u8));
                v.push(0);
            }
        }
        Val::I4(n) => {
            v.extend(3u32.to_le_bytes());
            v.extend(n.to_le_bytes());
        }
        Val::U4(n) => {
            v.extend(0x13u32.to_le_bytes());
            v.extend(n.to_le_bytes());
        }
        Val::R8(n) => {
            v.extend(5u32.to_le_bytes());
            v.extend(n.to_le_bytes());
        }
        Val::Bool(b) => {
            v.extend(0x0Bu32.to_le_bytes());
            v.extend((if *b { 0xFFFFu32 } else { 0 }).to_le_bytes());
        }
        Val::Time(t) => {
            v.extend(0x40u32.to_le_bytes());
            v.extend(t.to_le_bytes());
        }
    }
    pad4(v);
}

fn section(props: &[(u32, Val)], dict: &[(u32, String)], unicode: bool) -> Vec<u8> {
    let mut items: Vec<(u32, Vec<u8>)> = Vec::new();
    if !dict.is_empty() {
        let mut d = Vec::new();
        d.extend((dict.len() as u32).to_le_bytes());
        for (id, name) in dict {
            d.extend(id.to_le_bytes());
            if unicode {
                let u: Vec<u16> = name.encode_utf16().chain([0]).collect();
                d.extend((u.len() as u32).to_le_bytes());
                for c in u {
                    d.extend(c.to_le_bytes());
                }
                pad4(&mut d);
            } else {
                d.extend((name.len() as u32 + 1).to_le_bytes());
                d.extend(name.chars().map(|c| c as u8));
                d.push(0);
            }
        }
        pad4(&mut d);
        items.push((0, d));
    }
    let mut cp = Vec::new();
    cp.extend(2u32.to_le_bytes());
    cp.extend((if unicode { 1200u32 } else { 1252 }).to_le_bytes());
    items.push((1, cp));
    for (id, v) in props {
        let mut b = Vec::new();
        put_val(&mut b, v, unicode);
        items.push((*id, b));
    }
    let head = 8 + items.len() * 8;
    let mut out = Vec::new();
    out.extend(0u32.to_le_bytes());
    out.extend((items.len() as u32).to_le_bytes());
    let mut off = head;
    for (id, b) in &items {
        out.extend(id.to_le_bytes());
        out.extend((off as u32).to_le_bytes());
        off += b.len();
    }
    for (_, b) in &items {
        out.extend(b);
    }
    let size = out.len() as u32;
    out[..4].copy_from_slice(&size.to_le_bytes());
    out
}

fn stream(sections: &[([u8; 16], Vec<u8>)]) -> Vec<u8> {
    let mut v = vec![0xFE, 0xFF, 0, 0, 0x06, 0x00, 0x02, 0x00];
    v.extend([0u8; 16]);
    v.extend((sections.len() as u32).to_le_bytes());
    let mut off = 28 + sections.len() * 20;
    for (fmt, body) in sections {
        v.extend(fmt);
        v.extend((off as u32).to_le_bytes());
        off += body.len();
    }
    for (_, body) in sections {
        v.extend(body);
    }
    v
}

fn child_text(n: &Node, k: &str) -> Option<String> {
    n.child(k).map(|c| c.text.clone())
}

pub(super) fn from_parts(
    core: Option<&Node>,
    app: Option<&Node>,
    custom: Option<&Node>,
) -> (Option<Vec<u8>>, Option<Vec<u8>>) {
    let mut sum: Vec<(u32, Val)> = Vec::new();
    if let Some(c) = core {
        for (id, k) in [
            (2u32, "title"),
            (3, "subject"),
            (4, "creator"),
            (5, "keywords"),
            (6, "description"),
            (8, "lastModifiedBy"),
        ] {
            if let Some(t) = child_text(c, k) {
                sum.push((id, Val::Str(t)));
            }
        }
        for (id, k) in [(12u32, "created"), (13, "modified")] {
            if let Some(t) = child_text(c, k).and_then(|t| filetime(&t)) {
                sum.push((id, Val::Time(t)));
            }
        }
    }
    let unicode = |vals: &[(u32, Val)]| {
        vals.iter()
            .any(|(_, v)| matches!(v, Val::Str(s) if !s.chars().all(|c| (c as u32) < 256)))
    };
    let summary = (!sum.is_empty()).then(|| {
        let u = unicode(&sum);
        stream(&[(FMT_SUMMARY, section(&sum, &[], u))])
    });
    let mut doc: Vec<(u32, Val)> = Vec::new();
    if let Some(t) = core.and_then(|c| child_text(c, "category")) {
        doc.push((2, Val::Str(t)));
    }
    if let Some(a) = app {
        for (id, k) in [(14u32, "Manager"), (15, "Company")] {
            if let Some(t) = child_text(a, k) {
                doc.push((id, Val::Str(t)));
            }
        }
    }
    let mut cust: Vec<(u32, Val)> = Vec::new();
    let mut dict: Vec<(u32, String)> = Vec::new();
    if let Some(c) = custom {
        for (i, p) in c.children("property").enumerate() {
            let Some(k) = p.kids.first() else { continue };
            let v = match k.name.as_str() {
                "i4" | "i2" | "int" => k.text.trim().parse().ok().map(Val::I4),
                "ui4" | "uint" => k.text.trim().parse().ok().map(Val::U4),
                "r8" | "r4" => k.text.trim().parse().ok().map(Val::R8),
                "bool" => Some(Val::Bool(matches!(k.text.trim(), "1" | "true"))),
                "filetime" => filetime(&k.text).map(Val::Time),
                _ => Some(Val::Str(k.text.clone())),
            };
            if let (Some(v), Some(name)) = (v, p.attr("name")) {
                let id = i as u32 + 2;
                dict.push((id, name.to_string()));
                cust.push((id, v));
            }
        }
    }
    if doc.is_empty() && cust.is_empty() {
        return (summary, None);
    }
    let mut secs = vec![(FMT_DOC, section(&doc, &[], unicode(&doc)))];
    if !cust.is_empty() {
        let u = unicode(&cust) || dict.iter().any(|d| !d.1.is_ascii());
        secs.push((FMT_CUSTOM, section(&cust, &dict, u)));
    }
    (summary, Some(stream(&secs)))
}
