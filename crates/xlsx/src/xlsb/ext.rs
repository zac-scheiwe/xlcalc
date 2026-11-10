use super::{error_name, esc, recs, ref_text, xstr, Rd};
use crate::{part, Zip};
use std::fmt::Write as _;

pub(crate) struct ExtBook {
    pub sheets: Vec<String>,
    pub names: Vec<String>,
    pub xml: String,

    pub rels: Option<Vec<u8>>,
}

fn name_ref(sheets: &[String], rgce: &[u8]) -> String {
    let u16_at = |i: usize| {
        rgce.get(i..i + 2)
            .map(|b| u32::from(u16::from_le_bytes([b[0], b[1]])))
    };
    let sheet = |s: u32| sheets.get(s as usize).map(|n| n.replace('\'', "''"));
    match rgce.first().map(|p| p & 0x1F | 0x20) {
        Some(0x3A) if rgce.len() >= 9 => {
            let (Some(s), Some(r), Some(c)) = (u16_at(3).and_then(sheet), u16_at(5), u16_at(7))
            else {
                return "#REF!".into();
            };
            format!("='{s}'!{}", ref_text(r, c, false, false))
        }
        Some(0x3B) if rgce.len() >= 13 => {
            let (Some(s), Some(r0), Some(r1), Some(c0), Some(c1)) = (
                u16_at(3).and_then(sheet),
                u16_at(5),
                u16_at(7),
                u16_at(9),
                u16_at(11),
            ) else {
                return "#REF!".into();
            };
            format!(
                "='{s}'!{}:{}",
                ref_text(r0, c0, false, false),
                ref_text(r1, c1, false, false)
            )
        }
        _ => "#REF!".into(),
    }
}

pub(super) fn read(zip: &mut Zip, path: &str) -> Result<ExtBook, String> {
    let data = part(zip, path).ok_or_else(|| format!("missing {path}"))?;
    let (mut main, mut alt) = (String::from("rId1"), None);
    let mut sheets: Vec<String> = Vec::new();
    let mut names: Vec<(String, String)> = Vec::new();
    let mut pending = String::new();

    let mut cells: Vec<(u32, u32, u32, &'static str, String)> = Vec::new();
    let (mut sheet, mut row) = (0u32, 0u32);
    for (id, d) in recs(&data) {
        let mut rd = Rd::new(d);
        match id {
            360 => {
                rd.u16()?;
                main = rd.wstr()?;
            }
            5108 => {
                rd.take(8)?;
                alt = Some(rd.wstr()?);
            }
            359 => {
                for _ in 0..rd.u32()? {
                    sheets.push(rd.wstr()?);
                }
            }
            577 => pending = rd.wstr()?,
            585 => {
                let cb = rd.u32()? as usize;
                let rgce = rd.take(cb)?;
                names.push((std::mem::take(&mut pending), name_ref(&sheets, rgce)));
            }
            363 => sheet = rd.u32()?,
            366 => row = rd.u32()?,
            368 => {
                let c = rd.u32()?;
                cells.push((sheet, row, c, "n", format!("{}", rd.f64()?)));
            }
            369 => {
                let c = rd.u32()?;
                cells.push((sheet, row, c, "b", rd.u8()?.to_string()));
            }
            370 => {
                let c = rd.u32()?;
                let e = rd.u8()?;
                let name = error_name(e).ok_or(format!("error code {e:#x}"))?;
                cells.push((sheet, row, c, "e", name.to_string()));
            }
            371 => {
                let c = rd.u32()?;
                cells.push((sheet, row, c, "str", xstr(&rd.wstr()?)));
            }
            _ => {}
        }
    }
    let x = build(&main, alt.as_deref(), &sheets, &names, &cells);
    let rels = path
        .rsplit_once('/')
        .and_then(|(dir, file)| part(zip, &format!("{dir}/_rels/{file}.rels")));
    Ok(ExtBook {
        names: names.into_iter().map(|n| n.0).collect(),
        sheets,
        xml: x,
        rels,
    })
}

pub(crate) fn build(
    main: &str,
    alt: Option<&str>,
    sheets: &[String],
    names: &[(String, String)],
    cells: &[(u32, u32, u32, &'static str, String)],
) -> String {
    let mut x = String::from(
        "<?xml version=\"1.0\" encoding=\"UTF-8\"?>\n<externalLink xmlns=\"http://schemas.openxmlformats.org/spreadsheetml/2006/main\" xmlns:xxl21=\"http://schemas.microsoft.com/office/spreadsheetml/2021/extlinks2021\">",
    );
    let _ = write!(
        x,
        "<externalBook xmlns:r=\"{}\" r:id=\"{}\">",
        super::REL,
        esc(main)
    );
    if let Some(a) = alt {
        let _ = write!(
            x,
            "<xxl21:alternateUrls><xxl21:absoluteUrl r:id=\"{}\"/></xxl21:alternateUrls>",
            esc(a)
        );
    }
    x.push_str("<sheetNames>");
    for s in sheets {
        let _ = write!(x, "<sheetName val=\"{}\"/>", esc(s));
    }
    x.push_str("</sheetNames>");
    if !names.is_empty() {
        x.push_str("<definedNames>");
        for (n, r) in names {
            let _ = write!(
                x,
                "<definedName name=\"{}\" refersTo=\"{}\"/>",
                esc(n),
                esc(r)
            );
        }
        x.push_str("</definedNames>");
    }
    x.push_str("<sheetDataSet>");
    for i in 0..sheets.len() as u32 {
        let mut mine: Vec<_> = cells.iter().filter(|c| c.0 == i).collect();
        if mine.is_empty() {
            continue;
        }
        mine.sort_by_key(|c| (c.1, c.2));
        let _ = write!(x, "<sheetData sheetId=\"{i}\">");
        let mut open = None;
        for (_, r, c, t, v) in mine {
            if open != Some(*r) {
                if open.is_some() {
                    x.push_str("</row>");
                }
                let _ = write!(x, "<row r=\"{}\">", r + 1);
                open = Some(*r);
            }
            let t = if *t == "n" {
                String::new()
            } else {
                format!(" t=\"{t}\"")
            };
            let _ = write!(
                x,
                "<cell r=\"{}\"{t}><v>{v}</v></cell>",
                ref_text(*r, *c, true, true)
            );
        }
        x.push_str("</row></sheetData>");
    }
    x.push_str("</sheetDataSet></externalBook></externalLink>");
    x
}
