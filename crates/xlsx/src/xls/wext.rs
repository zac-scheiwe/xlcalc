use super::ptg_out::put_chars;
use super::write::{rd_xml, Out};
use crate::ext_in::read_link;
use crate::package::Package;
use crate::xlsb_enc::{error_code, ExtBookEnt};
use xlcalc::Value;

pub(super) struct ExtOut {
    pub ent: ExtBookEnt,
    pub records: Vec<(u16, Vec<u8>)>,
}

fn encode_url(target: &str, rooted: bool) -> String {
    let mut out = String::from("\u{1}");
    let mut path = target.replace('/', "\\");
    if rooted {
        let b = path.as_bytes();
        if b.len() >= 2 && b[1] == b':' && b[0].is_ascii_alphabetic() {
            out.push('\u{2}');
            path = path[2..].trim_start_matches('\\').to_string();
        }
    }
    let parts: Vec<&str> = path.split('\\').collect();
    for (i, part) in parts.iter().enumerate() {
        if *part == ".." {
            out.push('\u{4}');
        } else {
            out.push_str(part);
            if i + 1 < parts.len() {
                out.push('\u{3}');
            }
        }
    }
    out
}

fn name_rgce(sheets: &[String], n: &xlcalc::ExtName) -> Vec<u8> {
    let Some(s) = sheets
        .iter()
        .position(|x| x.eq_ignore_ascii_case(&n.sheet))
        .map(|i| i as u16)
    else {
        return vec![0x1C, 0x17];
    };
    let r = n.rect;
    let (Ok(r0), Ok(r1), Ok(c0), Ok(c1)) = (
        u16::try_from(r.r0),
        u16::try_from(r.r1),
        u16::try_from(r.c0),
        u16::try_from(r.c1),
    ) else {
        return vec![0x1C, 0x17];
    };
    let mut v = Vec::new();
    if r.is_cell() {
        v.push(0x3A);

        for x in [s, s, r0, c0] {
            v.extend(x.to_le_bytes());
        }
    } else {
        v.push(0x3B);
        for x in [s, s, r0, r1, c0, c1] {
            v.extend(x.to_le_bytes());
        }
    }
    v
}

pub(super) fn read_all(
    pkg: &Package,
    book: &crate::xml::Node,
    target_of: &dyn Fn(&str) -> Option<String>,
) -> Result<Vec<ExtOut>, String> {
    let mut out = Vec::new();
    let Some(refs) = book.child("externalReferences") else {
        return Ok(out);
    };
    for (i, r) in refs.children("externalReference").enumerate() {
        let n = i + 1;
        let part = target_of(r.attr("id").unwrap_or(""))
            .ok_or_else(|| format!("external link {n}: no part"))?;
        let root = rd_xml(pkg, &part)?;
        let raw: Vec<(String, String, String)> = pkg
            .rels(&part)
            .iter()
            .map(|r| (r.id.clone(), r.ty.clone(), r.target.clone()))
            .collect();
        let link = read_link(&part, &root, &raw).ok_or_else(|| format!("external link {n}"))?;
        let sheets: Vec<String> = link.sheets.iter().map(|s| s.name.clone()).collect();
        let mut records: Vec<(u16, Vec<u8>)> = Vec::new();
        let url = encode_url(&link.target, link.rooted);
        let mut d = Vec::new();
        d.extend((sheets.len() as u16).to_le_bytes());
        d.extend((url.encode_utf16().count() as u16).to_le_bytes());
        put_chars(&mut d, &url);
        for s in &sheets {
            d.extend((s.encode_utf16().count() as u16).to_le_bytes());
            put_chars(&mut d, s);
        }
        records.push((0x1AE, d));
        for nm in &link.names {
            let mut d = vec![0u8; 6];
            d.push(nm.name.encode_utf16().count().min(255) as u8);
            put_chars(&mut d, &nm.name);
            let rgce = name_rgce(&sheets, nm);
            d.extend((rgce.len() as u16).to_le_bytes());
            d.extend(rgce);
            records.push((0x23, d));
        }
        for (si, s) in link.sheets.iter().enumerate() {
            let mut cells: Vec<_> = s
                .cells
                .iter()
                .filter(|c| {
                    c.1 < 256
                        && c.0 < 65536
                        && matches!(
                            c.2,
                            Value::Number(_) | Value::Bool(_) | Value::Error(_) | Value::Text(_)
                        )
                })
                .collect();
            if cells.is_empty() {
                continue;
            }
            cells.sort_by_key(|c| (c.0, c.1));

            let mut runs: Vec<Vec<&(u32, u32, Value)>> = Vec::new();
            for c in cells {
                match runs.last_mut() {
                    Some(r) if r[0].0 == c.0 && r[r.len() - 1].1 + 1 == c.1 => r.push(c),
                    _ => runs.push(vec![c]),
                }
            }
            let mut x = Vec::new();
            x.extend((runs.len() as u16).to_le_bytes());
            x.extend((si as u16).to_le_bytes());
            records.push((0x59, x));
            for run in runs {
                let mut d = vec![run[run.len() - 1].1 as u8, run[0].1 as u8];
                d.extend((run[0].0 as u16).to_le_bytes());
                for (_, _, v) in run {
                    match v {
                        Value::Number(x) => {
                            d.push(1);
                            d.extend(x.to_le_bytes());
                        }
                        Value::Text(t) => {
                            d.push(2);
                            d.extend((t.encode_utf16().count().min(255) as u16).to_le_bytes());
                            put_chars(&mut d, t);
                        }
                        Value::Bool(b) => {
                            d.push(4);
                            d.push(u8::from(*b));
                            d.extend([0u8; 7]);
                        }
                        Value::Error(e) => {
                            d.push(0x10);
                            d.push(error_code(&e.to_string()).unwrap_or(0x0F));
                            d.extend([0u8; 7]);
                        }
                        _ => {}
                    }
                }
                records.push((0x5A, d));
            }
        }
        out.push(ExtOut {
            ent: ExtBookEnt {
                sheets,
                names: link.names.iter().map(|n| n.name.clone()).collect(),
            },
            records,
        });
    }
    Ok(out)
}

pub(super) fn write(o: &mut Out, exts: &[ExtOut]) {
    for e in exts {
        for (id, d) in &e.records {
            o.rec(*id, d);
        }
    }
}
