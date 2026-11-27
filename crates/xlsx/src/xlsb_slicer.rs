use crate::rawxml::escape_attr;
use crate::slicers_xml::{all, NS, NS15};
use crate::xlsb::{recs, Rd};
use crate::xlsb_rec::{Recs, P};
use crate::xml::Node;
use std::fmt::Write as _;

pub(crate) const CT_CACHE_BIN: &str = "application/vnd.ms-excel.slicerCache";
pub(crate) const CT_SLICER_BIN: &str = "application/vnd.ms-excel.slicer";

const FRT14: [u8; 4] = [0x02, 0x0e, 0, 0];
const FRT15: [u8; 4] = [0x03, 0x0f, 0, 0];
const FRT_BEGIN: u32 = 35;
const FRT_END: u32 = 36;

const BEGIN_CACHE: u32 = 1075;
const END_CACHE: u32 = 1076;
const CACHE_DEF: u32 = 1077;
const END_CACHE_DEF: u32 = 1078;
const PIVOT_TABLES: u32 = 1085;
const TABULAR: u32 = 1100;
const END_TABULAR: u32 = 1101;
const ITEMS: u32 = 1102;
const TABLE_CACHE: u32 = 2077;
const END_TABLE_CACHE: u32 = 2078;
const BEGIN_SLICERS: u32 = 1115;
const END_SLICERS: u32 = 1116;
const BEGIN_SLICER: u32 = 1083;
const END_SLICER: u32 = 1084;
const BEGIN_TL_CACHES: u32 = 2083;
const END_TL_CACHES: u32 = 2084;

pub(crate) const TL_CACHE_ID: u32 = 2085;
const END_TL_CACHE_ID: u32 = 2086;
const BEGIN_TL_REFS: u32 = 2087;
const END_TL_REFS: u32 = 2088;
const TL_REF: u32 = 2089;
const END_TL_REF: u32 = 2090;

fn number(n: &Node, a: &str) -> Option<u32> {
    n.attr(a).and_then(|x| x.parse().ok())
}

pub(crate) fn is_pivot(root: &Node) -> bool {
    !all(root, "tabular").is_empty()
}

fn cache_flags(n: &Node) -> u8 {
    let sort = if n.attr("sortOrder") == Some("descending") {
        2
    } else {
        1
    };
    let cross = match n.attr("crossFilter") {
        Some("none") => 0,
        Some("showItemsWithNoData") => 2,
        _ => 1,
    };
    let lists = u8::from(n.attr("customListSort") != Some("0"));
    sort | cross << 2 | lists << 4
}

pub(crate) fn cache_bin(root: &Node) -> Vec<u8> {
    let mut r = Recs::default();
    r.empty(BEGIN_CACHE);
    r.with(CACHE_DEF, |p| {
        p.wstr(root.attr("name").unwrap_or(""))
            .wstr(root.attr("sourceName").unwrap_or(""));
    });
    let tables = all(root, "pivotTable");
    r.with(PIVOT_TABLES, |p| {
        p.u32(tables.len() as u32);
        for t in &tables {
            p.u32(number(t, "tabId").unwrap_or(0))
                .wstr(t.attr("name").unwrap_or(""));
        }
    });
    if let Some(t) = all(root, "tabular").first() {
        r.with(TABULAR, |p| {
            p.u32(0)
                .u32(number(t, "pivotCacheId").unwrap_or(0))
                .u8(cache_flags(t) | 0x20);
        });
        let items = all(t, "i");
        r.with(ITEMS, |p| {
            p.u32(items.len() as u32);
            for i in &items {
                let flags =
                    u8::from(i.attr("s") == Some("1")) | u8::from(i.attr("nd") == Some("1")) << 1;
                p.u32(number(i, "x").unwrap_or(0)).u8(flags);
            }
        });
        r.empty(END_TABULAR);
    }
    if let Some(t) = all(root, "tableSlicerCache").first() {
        r.rec(FRT_BEGIN, &FRT15);
        r.with(TABLE_CACHE, |p| {
            p.u32(0)
                .u32(number(t, "column").unwrap_or(1))
                .u32(number(t, "tableId").unwrap_or(1))
                .u8(cache_flags(t));
        });
        r.empty(END_TABLE_CACHE);
        r.empty(FRT_END);
    }
    r.empty(END_CACHE_DEF);
    r.empty(END_CACHE);
    r.bytes
}

pub(crate) fn slicers_bin(list: &[&Node]) -> Vec<u8> {
    let mut r = Recs::default();
    r.empty(BEGIN_SLICERS);
    for s in list {
        let name = s.attr("name").unwrap_or("");
        let caption = s.attr("caption");
        let style = s.attr("style");
        r.with(BEGIN_SLICER, |p| {
            let flags = u8::from(s.attr("showCaption") != Some("0"))
                | u8::from(caption.is_some()) << 1
                | u8::from(style.is_some()) << 2
                | u8::from(s.attr("lockedPosition") == Some("1")) << 3;
            p.u8(flags)
                .u32(number(s, "startItem").unwrap_or(0))
                .u32(number(s, "columnCount").unwrap_or(1))
                .u32(number(s, "level").unwrap_or(0))
                .u32(number(s, "rowHeight").unwrap_or(0))
                .wstr(name)
                .wstr(s.attr("cache").unwrap_or(""));
            for v in [caption, style].into_iter().flatten() {
                p.wstr(v);
            }
        });
        r.empty(END_SLICER);
    }
    r.empty(END_SLICERS);
    r.bytes
}

fn related(p: &mut P, rid: &str) {
    let units: Vec<u16> = xlcalc::wide::units(rid).collect();
    p.u32(8).u16(units.len() as u16);
    for u in units {
        p.u16(u);
    }
}

pub(crate) fn related_id(d: &[u8]) -> Result<String, String> {
    let mut rd = Rd::new(d);
    rd.u32()?;
    let mut units = Vec::new();
    for _ in 0..rd.u16()? {
        units.push(rd.u16()?);
    }
    Ok(String::from_utf16_lossy(&units))
}

pub(crate) fn book_records(r: &mut Recs, pivot: &[String], table: &[String]) {
    if !pivot.is_empty() {
        r.rec(FRT_BEGIN, &FRT14);
        r.empty(1070);
        for rid in pivot {
            r.with(1072, |p| related(p, rid));
            r.empty(1073);
        }
        r.empty(1071);
        r.empty(FRT_END);
    }
    if !table.is_empty() {
        r.rec(FRT_BEGIN, &FRT15);
        r.empty(2075);
        for rid in table {
            r.with(2076, |p| related(p, rid));
        }
        r.empty(FRT_END);
    }
}

pub(crate) fn book_timeline_records(r: &mut Recs, rids: &[String]) {
    if rids.is_empty() {
        return;
    }
    r.rec(FRT_BEGIN, &FRT15);
    r.empty(BEGIN_TL_CACHES);
    for rid in rids {
        r.with(TL_CACHE_ID, |p| related(p, rid));
        r.empty(END_TL_CACHE_ID);
    }
    r.empty(END_TL_CACHES);
    r.empty(FRT_END);
}

pub(crate) fn sheet_timeline_records(r: &mut Recs, rid: &str) {
    r.rec(FRT_BEGIN, &FRT15);
    r.empty(BEGIN_TL_REFS);
    r.with(TL_REF, |p| related(p, rid));
    r.empty(END_TL_REF);
    r.empty(END_TL_REFS);
    r.empty(FRT_END);
}

pub(crate) fn sheet_records(r: &mut Recs, rid: &str, pivot: bool) {
    r.rec(FRT_BEGIN, if pivot { &FRT14 } else { &FRT15 });
    r.empty(1079);
    r.with(1081, |p| related(p, rid));
    r.empty(1082);
    r.empty(1080);
    r.empty(FRT_END);
}

fn flag_attrs(flags: u8) -> String {
    let mut s = String::new();
    if flags & 3 == 2 {
        s.push_str(" sortOrder=\"descending\"");
    }
    if flags & 0x10 == 0 {
        s.push_str(" customListSort=\"0\"");
    }
    match flags >> 2 & 3 {
        0 => s.push_str(" crossFilter=\"none\""),
        2 => s.push_str(" crossFilter=\"showItemsWithNoData\""),
        _ => {}
    }
    s
}

pub(crate) fn cache_xml(bin: &[u8]) -> Result<String, String> {
    let mut name = None;
    let mut tables = String::new();
    let mut body = String::new();
    for (id, d) in recs(bin) {
        let mut rd = Rd::new(d);
        match id {
            CACHE_DEF => name = Some((rd.wstr()?, rd.wstr()?)),
            PIVOT_TABLES => {
                for _ in 0..rd.u32()? {
                    let tab = rd.u32()?;
                    let n = rd.wstr()?;
                    let _ = write!(
                        tables,
                        "<pivotTable tabId=\"{tab}\" name=\"{}\"/>",
                        escape_attr(&n)
                    );
                }
            }
            TABULAR => {
                rd.u32()?;
                let cache = rd.u32()?;
                let flags = rd.u8()?;
                let _ = write!(
                    body,
                    "<data><tabular pivotCacheId=\"{cache}\"{}>",
                    flag_attrs(flags)
                );
            }
            ITEMS => {
                let n = rd.u32()?;
                let _ = write!(body, "<items count=\"{n}\">");
                for _ in 0..n {
                    let x = rd.u32()?;
                    let flags = rd.u8()?;
                    let _ = write!(body, "<i x=\"{x}\"");
                    if flags & 1 != 0 {
                        body.push_str(" s=\"1\"");
                    }
                    if flags & 2 != 0 {
                        body.push_str(" nd=\"1\"");
                    }
                    body.push_str("/>");
                }
                body.push_str("</items>");
            }
            END_TABULAR => body.push_str("</tabular></data>"),
            TABLE_CACHE => {
                rd.u32()?;
                let column = rd.u32()?;
                let table = rd.u32()?;
                let flags = rd.u8()?;
                let _ = write!(
                    body,
                    "<extLst><ext xmlns:x15=\"{NS15}\"><x15:tableSlicerCache tableId=\"{table}\" column=\"{column}\"{}/></ext></extLst>",
                    flag_attrs(flags)
                );
            }
            _ => {}
        }
    }
    let (name, source) = name.ok_or("no slicer cache definition")?;
    if !tables.is_empty() {
        tables = format!("<pivotTables>{tables}</pivotTables>");
    }
    Ok(format!(
        "<slicerCacheDefinition xmlns=\"{NS}\" name=\"{}\" sourceName=\"{}\">{tables}{body}</slicerCacheDefinition>",
        escape_attr(&name),
        escape_attr(&source)
    ))
}

pub(crate) fn slicers_xml(bin: &[u8]) -> Result<String, String> {
    let mut s = format!("<slicers xmlns=\"{NS}\">");
    for (id, d) in recs(bin) {
        if id != BEGIN_SLICER {
            continue;
        }
        let mut rd = Rd::new(d);
        let flags = rd.u8()?;
        rd.u32()?;
        let columns = rd.u32()?;
        rd.u32()?;
        let row_height = rd.u32()?;
        let name = rd.wstr()?;
        let cache = rd.wstr()?;
        let _ = write!(
            s,
            "<slicer name=\"{}\" cache=\"{}\" columnCount=\"{columns}\" rowHeight=\"{row_height}\"",
            escape_attr(&name),
            escape_attr(&cache)
        );
        if flags & 2 != 0 {
            let _ = write!(s, " caption=\"{}\"", escape_attr(&rd.wstr()?));
        }
        if flags & 4 != 0 {
            let _ = write!(s, " style=\"{}\"", escape_attr(&rd.wstr()?));
        }
        if flags & 1 == 0 {
            s.push_str(" showCaption=\"0\"");
        }
        if flags & 8 != 0 {
            s.push_str(" lockedPosition=\"1\"");
        }
        s.push_str("/>");
    }
    s.push_str("</slicers>");
    Ok(s)
}
