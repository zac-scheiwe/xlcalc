use crate::rawxml::escape_attr;
use crate::xml::Node;
use std::collections::BTreeMap;
use xlcalc::slicer::{Slicer, SlicerCache};
use xlcalc::{SheetId, Workbook};

pub(crate) const NS: &str = "http://schemas.microsoft.com/office/spreadsheetml/2009/9/main";
pub(crate) const NS15: &str = "http://schemas.microsoft.com/office/spreadsheetml/2010/11/main";
pub(crate) const URI_BOOK14: &str = "{BBE1A952-AA13-448e-AADC-164F8A28A991}";
pub(crate) const URI_BOOK15: &str = "{46BE6895-7355-4a93-B00E-2C351335B9C9}";
pub(crate) const URI_LIST: &str = "{A8765BA9-456A-4dab-B4F3-ACF838C121DE}";
const URI_TABLE: &str = "{2F2917AC-EB37-4324-AD4E-5DD8C200BD13}";
pub(crate) const CT_CACHE: &str = "application/vnd.ms-excel.slicerCache+xml";
pub(crate) const CT_SLICER: &str = "application/vnd.ms-excel.slicer+xml";
pub(crate) const REL_CACHE: &str =
    "http://schemas.microsoft.com/office/2007/relationships/slicerCache";
pub(crate) const REL_SLICER: &str = "http://schemas.microsoft.com/office/2007/relationships/slicer";
pub(crate) const NS_SLE: &str = "http://schemas.microsoft.com/office/drawing/2010/slicer";

const EMU_PT: f64 = 12700.0;

pub(crate) const URI_TL_SHEET: &str = "{7E03D99C-DC04-49d9-9315-930204A7B6E9}";
pub(crate) const URI_TL_BOOK: &str = "{D0CA8CA8-9F24-4464-BF8E-62219DCF47F9}";
pub(crate) const CT_TL_CACHE: &str = "application/vnd.ms-excel.timelineCache+xml";
pub(crate) const CT_TL: &str = "application/vnd.ms-excel.timeline+xml";
pub(crate) const REL_TL_CACHE: &str =
    "http://schemas.microsoft.com/office/2011/relationships/timelineCache";
pub(crate) const REL_TL: &str = "http://schemas.microsoft.com/office/2011/relationships/timeline";
pub(crate) const NS_TSLE: &str = "http://schemas.microsoft.com/office/drawing/2012/timeslicer";

pub(crate) fn iso(serial: f64) -> String {
    let z = serial.floor() as i64 - 25569 + 719_468;
    let era = z.div_euclid(146_097);
    let doe = z.rem_euclid(146_097);
    let yoe = (doe - doe / 1460 + doe / 36_524 - doe / 146_096) / 365;
    let doy = doe - (365 * yoe + yoe / 4 - yoe / 100);
    let mp = (5 * doy + 2) / 153;
    let d = doy - (153 * mp + 2) / 5 + 1;
    let m = if mp < 10 { mp + 3 } else { mp - 9 };
    let y = yoe + era * 400 + i64::from(m <= 2);
    format!("{y:04}-{m:02}-{d:02}T00:00:00")
}

fn year_start(serial: f64) -> f64 {
    let y: i64 = iso(serial)[0..4].parse().unwrap_or(1900);
    self::serial(&format!("{y:04}-01-01")).unwrap_or(serial)
}

fn year_len(serial: f64) -> f64 {
    let y: i64 = iso(serial)[0..4].parse().unwrap_or(1900);
    if (y % 4 == 0 && y % 100 != 0) || y % 400 == 0 {
        366.0
    } else {
        365.0
    }
}

pub(crate) fn serial(s: &str) -> Option<f64> {
    let y: i64 = s.get(0..4)?.parse().ok()?;
    let m: i64 = s.get(5..7)?.parse().ok()?;
    let d: i64 = s.get(8..10)?.parse().ok()?;
    let y = y - i64::from(m <= 2);
    let era = y.div_euclid(400);
    let yoe = y.rem_euclid(400);
    let doy = (153 * (if m > 2 { m - 3 } else { m + 9 }) + 2) / 5 + d - 1;
    let doe = yoe * 365 + yoe / 4 - yoe / 100 + doy;
    Some((era * 146_097 + doe - 719_468 + 25569) as f64)
}

pub(crate) fn cache_ok(wb: &Workbook, c: &SlicerCache) -> bool {
    if c.pivot {
        wb.pivots()
            .tables
            .iter()
            .any(|t| t.name.eq_ignore_ascii_case(&c.table))
    } else {
        wb.tables()
            .iter()
            .any(|t| t.name.eq_ignore_ascii_case(&c.table))
    }
}

pub(crate) fn live(wb: &Workbook) -> Vec<(usize, &SlicerCache)> {
    wb.slicer_caches()
        .iter()
        .enumerate()
        .filter(|(_, c)| cache_ok(wb, c))
        .collect()
}

fn on_sheet(wb: &Workbook, sheet: SheetId, timeline: bool) -> Vec<(&SlicerCache, &Slicer)> {
    live(wb)
        .into_iter()
        .filter(|(_, c)| c.timeline.is_some() == timeline)
        .flat_map(|(_, c)| c.slicers.iter().map(move |s| (c, s)))
        .filter(|(_, s)| s.sheet == sheet)
        .collect()
}

pub(crate) fn sheet_part(wb: &Workbook, sheet: SheetId) -> Option<String> {
    let list = on_sheet(wb, sheet, false);
    if list.is_empty() {
        return None;
    }
    let mut s = format!("<slicers xmlns=\"{NS}\">");
    for (c, sl) in list {
        s.push_str(&format!(
            "<slicer name=\"{}\" cache=\"{}\" caption=\"{}\"",
            escape_attr(&sl.name),
            escape_attr(&c.name),
            escape_attr(&sl.caption)
        ));
        if sl.columns > 1 {
            s.push_str(&format!(" columnCount=\"{}\"", sl.columns));
        }
        if !sl.header {
            s.push_str(" showCaption=\"0\"");
        }
        if !sl.style.is_empty() && sl.style != "SlicerStyleLight1" {
            s.push_str(&format!(" style=\"{}\"", escape_attr(&sl.style)));
        }
        if !sl.locked {
            s.push_str(" lockedPosition=\"0\"");
        }
        s.push_str(&format!(
            " rowHeight=\"{}\"/>",
            (sl.row_h * EMU_PT).round() as i64
        ));
    }
    s.push_str("</slicers>");
    Some(s)
}

pub(crate) fn timeline_part(wb: &Workbook, sheet: SheetId) -> Option<String> {
    let list = on_sheet(wb, sheet, true);
    if list.is_empty() {
        return None;
    }
    let mut s = format!(
        "<timelines xmlns=\"{NS15}\" xmlns:mc=\"http://schemas.openxmlformats.org/markup-compatibility/2006\" mc:Ignorable=\"x\" xmlns:x=\"http://schemas.openxmlformats.org/spreadsheetml/2006/main\">"
    );
    for (c, sl) in list {
        s.push_str(&format!(
            "<timeline name=\"{}\" cache=\"{}\" caption=\"{}\" level=\"{}\" selectionLevel=\"{}\" scrollPosition=\"2000-01-01T00:00:00\" style=\"{}\"",
            escape_attr(&sl.name),
            escape_attr(&c.name),
            escape_attr(&sl.caption),
            sl.tv.level,
            sl.tv.level,
            escape_attr(&sl.style)
        ));
        if !sl.tv.header {
            s.push_str(" showHeader=\"0\"");
        }
        if !sl.tv.hscroll {
            s.push_str(" showHorizontalScrollbar=\"0\"");
        }
        if !sl.tv.label {
            s.push_str(" showSelectionLabel=\"0\"");
        }
        if !sl.tv.time_level {
            s.push_str(" showTimeLevel=\"0\"");
        }
        s.push_str("/>");
    }
    s.push_str("</timelines>");
    Some(s)
}

pub(crate) fn timeline_ext(rid: &str, rp: &str) -> String {
    format!(
        "<ext uri=\"{URI_TL_SHEET}\" xmlns:x15=\"{NS15}\"><x15:timelineRefs><x15:timelineRef {rp}:id=\"{rid}\"/></x15:timelineRefs></ext>"
    )
}

pub(crate) fn sheet_ext(rid: &str, rp: &str) -> String {
    format!(
        "<ext uri=\"{URI_LIST}\" xmlns:x14=\"{NS}\"><x14:slicerList><x14:slicer {rp}:id=\"{rid}\"/></x14:slicerList></ext>"
    )
}

pub(crate) struct Ids<'a> {
    pub tables: &'a BTreeMap<String, u32>,

    pub sheets: &'a BTreeMap<SheetId, u32>,
}

pub(crate) fn cache_part(wb: &Workbook, i: usize, ids: &Ids) -> String {
    let c = &wb.slicer_caches()[i];
    if c.timeline.is_some() {
        return timeline_cache_part(wb, c, ids);
    }
    let mut s = format!(
        "<slicerCacheDefinition xmlns=\"{NS}\" xmlns:mc=\"http://schemas.openxmlformats.org/markup-compatibility/2006\" mc:Ignorable=\"x\" xmlns:x=\"http://schemas.openxmlformats.org/spreadsheetml/2006/main\" name=\"{}\" sourceName=\"{}\">",
        escape_attr(&c.name),
        escape_attr(&c.field)
    );
    let mut flags = String::new();
    if c.sort == 3 {
        flags.push_str(" sortOrder=\"descending\"");
    }
    if !c.custom_lists {
        flags.push_str(" customListSort=\"0\"");
    }
    match c.cross {
        1 => flags.push_str(" crossFilter=\"none\""),
        3 => flags.push_str(" crossFilter=\"showItemsWithNoData\""),
        _ => {}
    }
    if c.pivot {
        let pv = wb.pivots();
        let t = pv
            .tables
            .iter()
            .find(|t| t.name.eq_ignore_ascii_case(&c.table));
        if let Some(t) = t {
            let tab = ids.sheets.get(&t.sheet).copied().unwrap_or(0);
            let _ = tab;
            s.push_str("<pivotTables>");
            for p in &c.pivots {
                if let Some(pt) = pv.tables.iter().find(|x| x.name.eq_ignore_ascii_case(p)) {
                    let tab = ids.sheets.get(&pt.sheet).copied().unwrap_or(0);
                    s.push_str(&format!(
                        "<pivotTable tabId=\"{tab}\" name=\"{}\"/>",
                        escape_attr(&pt.name)
                    ));
                }
            }
            s.push_str("</pivotTables>");
            let cache = &pv.caches[t.cache];
            let fi = cache
                .fields
                .iter()
                .position(|f| f.name.eq_ignore_ascii_case(&c.field));
            s.push_str(&format!(
                "<data><tabular pivotCacheId=\"{}\"{flags}>",
                t.cache + 1
            ));
            let items = fi.map_or(&[][..], |f| &cache.fields[f].items[..]);
            s.push_str(&format!("<items count=\"{}\">", items.len()));
            for (k, it) in items.iter().enumerate() {
                if c.off.iter().any(|o| o.eq_ignore_ascii_case(&it.label)) {
                    s.push_str(&format!("<i x=\"{k}\"/>"));
                } else {
                    s.push_str(&format!("<i x=\"{k}\" s=\"1\"/>"));
                }
            }
            s.push_str("</items></tabular></data>");
        }
    } else {
        let tid = ids
            .tables
            .get(&c.table.to_lowercase())
            .copied()
            .unwrap_or(1);
        let col = wb
            .tables()
            .iter()
            .find(|t| t.name.eq_ignore_ascii_case(&c.table))
            .and_then(|t| {
                t.columns
                    .iter()
                    .position(|x| x.eq_ignore_ascii_case(&c.field))
            })
            .map_or(1, |p| p + 1);
        s.push_str(&format!(
            "<extLst><x:ext uri=\"{URI_TABLE}\" xmlns:x15=\"{NS15}\"><x15:tableSlicerCache tableId=\"{tid}\" column=\"{col}\"{flags}/></x:ext></extLst>"
        ));
    }
    s.push_str("</slicerCacheDefinition>");
    s
}

fn timeline_cache_part(wb: &Workbook, c: &SlicerCache, ids: &Ids) -> String {
    let mut s = format!(
        "<timelineCacheDefinition xmlns=\"{NS15}\" xmlns:mc=\"http://schemas.openxmlformats.org/markup-compatibility/2006\" mc:Ignorable=\"x\" xmlns:x=\"http://schemas.openxmlformats.org/spreadsheetml/2006/main\" name=\"{}\" sourceName=\"{}\">",
        escape_attr(&c.name),
        escape_attr(&c.field)
    );
    let pv = wb.pivots();
    let Some(t) = pv
        .tables
        .iter()
        .find(|t| t.name.eq_ignore_ascii_case(&c.table))
    else {
        s.push_str("</timelineCacheDefinition>");
        return s;
    };
    s.push_str("<pivotTables>");
    for p in &c.pivots {
        if let Some(pt) = pv.tables.iter().find(|x| x.name.eq_ignore_ascii_case(p)) {
            let tab = ids.sheets.get(&pt.sheet).copied().unwrap_or(0);
            s.push_str(&format!(
                "<pivotTable tabId=\"{tab}\" name=\"{}\"/>",
                escape_attr(&pt.name)
            ));
        }
    }
    s.push_str("</pivotTables>");
    let nums: Vec<f64> = pv.caches[t.cache]
        .fields
        .iter()
        .find(|f| f.name.eq_ignore_ascii_case(&c.field))
        .map(|f| {
            f.items
                .iter()
                .filter_map(|i| match i.value {
                    xlcalc::Value::Number(n) => Some(n),
                    _ => None,
                })
                .collect()
        })
        .unwrap_or_default();
    if nums.is_empty() {
        s.push_str("</timelineCacheDefinition>");
        return s;
    }
    let lo = nums.iter().copied().fold(f64::INFINITY, f64::min);
    let hi = nums.iter().copied().fold(f64::NEG_INFINITY, f64::max);
    let range = c.timeline.as_ref().and_then(|t| t.range);
    let (sa, sb) = range.unwrap_or((lo, hi));
    s.push_str(&format!(
        "<state minimalRefreshVersion=\"6\" lastRefreshVersion=\"6\" pivotCacheId=\"{}\" filterType=\"{}\"><selection startDate=\"{}\" endDate=\"{}\"/><bounds startDate=\"{}\" endDate=\"{}\"/></state></timelineCacheDefinition>",
        t.cache + 1,
        match range {
            Some((a, b)) if a == b => "dateEqual",
            Some(_) => "dateBetween",
            None => "unknown",
        },
        iso(sa),
        iso(sb),
        iso(year_start(lo)),
        iso(year_start(hi) + if year_start(hi) < hi { year_len(hi) } else { 0.0 })
    ));
    s
}

pub(crate) fn book_exts(
    wb: &Workbook,
    rids: &[(usize, String)],
    rp: &str,
) -> (Option<String>, Option<String>, Option<String>) {
    let (mut p, mut t, mut tl) = (String::new(), String::new(), String::new());
    for (i, rid) in rids {
        let tag = format!("<x14:slicerCache {rp}:id=\"{rid}\"/>");
        if wb.slicer_caches()[*i].timeline.is_some() {
            tl.push_str(&format!("<x15:timelineCacheRef {rp}:id=\"{rid}\"/>"));
        } else if wb.slicer_caches()[*i].pivot {
            p.push_str(&tag);
        } else {
            t.push_str(&tag);
        }
    }
    (
        (!p.is_empty()).then(|| {
            format!(
                "<ext uri=\"{URI_BOOK14}\" xmlns:x14=\"{NS}\"><x14:slicerCaches>{p}</x14:slicerCaches></ext>"
            )
        }),
        (!t.is_empty()).then(|| {
            format!(
                "<ext uri=\"{URI_BOOK15}\" xmlns:x15=\"{NS15}\"><x15:slicerCaches xmlns:x14=\"{NS}\">{t}</x15:slicerCaches></ext>"
            )
        }),
        (!tl.is_empty()).then(|| {
            format!(
                "<ext uri=\"{URI_TL_BOOK}\" xmlns:x15=\"{NS15}\"><x15:timelineCacheRefs>{tl}</x15:timelineCacheRefs></ext>"
            )
        }),
    )
}

pub(crate) fn names(wb: &Workbook, old: Option<String>) -> Option<String> {
    let mut inner = String::new();
    if let Some(o) = &old {
        if let (Some(a), Some(b)) = (o.find('>'), o.rfind("</")) {
            inner = o[a + 1..b].to_string();
        }
    }

    for prefix in [
        "<definedName name=\"Slicer_",
        "<definedName name=\"NativeTimeline_",
    ] {
        while let Some(at) = inner.find(prefix) {
            let end = inner[at..]
                .find("</definedName>")
                .map_or(inner.len(), |e| at + e + "</definedName>".len());
            inner.replace_range(at..end, "");
        }
    }
    for (_, c) in live(wb) {
        inner.push_str(&format!(
            "<definedName name=\"{}\">#N/A</definedName>",
            escape_attr(&c.name)
        ));
    }
    (!inner.is_empty()).then(|| format!("<definedNames>{inner}</definedNames>"))
}

pub(crate) fn frame(name: &str, cnv: &str, x: i64, y: i64, w: i64, h: i64) -> String {
    frame_of(false, name, cnv, x, y, w, h)
}

pub(crate) fn timeline_frame(name: &str, cnv: &str, x: i64, y: i64, w: i64, h: i64) -> String {
    frame_of(true, name, cnv, x, y, w, h)
}

fn frame_of(tl: bool, name: &str, cnv: &str, x: i64, y: i64, w: i64, h: i64) -> String {
    let n = escape_attr(name);
    let (pfx, ns, el, fallback) = if tl {
        (
            "tsle",
            NS_TSLE,
            "timeslicer",
            "Timeline: Works in Excel 2013 or higher. Do not move or resize.",
        )
    } else {
        (
            "sle",
            NS_SLE,
            "slicer",
            "This shape represents a slicer. Slicers are supported in Excel 2010 or later.

If the shape was modified in an earlier version of Excel, or if the workbook was saved in Excel 2003 or earlier, the slicer cannot be used.",
        )
    };
    format!(
        "<mc:AlternateContent xmlns:mc=\"http://schemas.openxmlformats.org/markup-compatibility/2006\"><mc:Choice xmlns:{pfx}=\"{ns}\" Requires=\"{pfx}\"><xdr:graphicFrame macro=\"\"><xdr:nvGraphicFramePr>{cnv}<xdr:cNvGraphicFramePr/></xdr:nvGraphicFramePr><xdr:xfrm><a:off x=\"0\" y=\"0\"/><a:ext cx=\"0\" cy=\"0\"/></xdr:xfrm><a:graphic><a:graphicData uri=\"{ns}\"><{pfx}:{el} xmlns:{pfx}=\"{ns}\" name=\"{n}\"/></a:graphicData></a:graphic></xdr:graphicFrame></mc:Choice><mc:Fallback><xdr:sp macro=\"\" textlink=\"\"><xdr:nvSpPr>{cnv}<xdr:cNvSpPr><a:spLocks noTextEdit=\"1\"/></xdr:cNvSpPr></xdr:nvSpPr><xdr:spPr><a:xfrm><a:off x=\"{x}\" y=\"{y}\"/><a:ext cx=\"{w}\" cy=\"{h}\"/></a:xfrm><a:prstGeom prst=\"rect\"><a:avLst/></a:prstGeom><a:solidFill><a:prstClr val=\"white\"/></a:solidFill><a:ln w=\"1\"><a:solidFill><a:prstClr val=\"green\"/></a:solidFill></a:ln></xdr:spPr><xdr:txBody><a:bodyPr vertOverflow=\"clip\" horzOverflow=\"clip\"/><a:lstStyle/><a:p><a:r><a:rPr lang=\"en-US\" sz=\"1100\"/><a:t>{fallback}\n\nIf the shape was modified in an earlier version of Excel, or if the workbook was saved in Excel 2003 or earlier, the slicer cannot be used.</a:t></a:r></a:p></xdr:txBody></xdr:sp></mc:Fallback></mc:AlternateContent>"
    )
}

fn walk<'a>(n: &'a Node, name: &str, out: &mut Vec<&'a Node>) {
    for k in &n.kids {
        if k.name == name {
            out.push(k);
        }
        walk(k, name, out);
    }
}

pub(crate) fn all<'a>(n: &'a Node, name: &str) -> Vec<&'a Node> {
    let mut v = Vec::new();
    walk(n, name, &mut v);
    v
}

pub(crate) struct CacheDef {
    pub name: String,
    pub field: String,
    pub table_id: Option<u32>,
    pub column: Option<usize>,
    pub pivot: Option<String>,

    pub off_idx: Vec<usize>,
    pub sort: i32,
    pub cross: i32,
    pub custom_lists: bool,

    pub timeline: Option<Option<(f64, f64)>>,

    pub pivots: Vec<String>,
}

pub(crate) fn read_timeline_cache(root: &Node) -> Option<CacheDef> {
    let range = all(root, "selection")
        .first()
        .and_then(|s| Some((serial(s.attr("startDate")?)?, serial(s.attr("endDate")?)?)));
    let filtered = all(root, "state")
        .first()
        .is_some_and(|s| matches!(s.attr("filterType"), Some("dateBetween" | "dateEqual")));
    Some(CacheDef {
        name: root.attr("name")?.to_string(),
        field: root.attr("sourceName")?.to_string(),
        table_id: None,
        column: None,
        pivot: Some(all(root, "pivotTable").first()?.attr("name")?.to_string()),
        off_idx: Vec::new(),
        sort: 2,
        cross: 2,
        custom_lists: true,
        timeline: Some(range.filter(|_| filtered)),
        pivots: all(root, "pivotTable")
            .iter()
            .filter_map(|p| p.attr("name").map(str::to_string))
            .collect(),
    })
}

pub(crate) fn read_cache(root: &Node) -> Option<CacheDef> {
    let name = root.attr("name")?.to_string();
    let field = root.attr("sourceName")?.to_string();
    let mut d = CacheDef {
        name,
        field,
        table_id: None,
        column: None,
        pivot: None,
        off_idx: Vec::new(),
        sort: 2,
        cross: 2,
        custom_lists: true,
        timeline: None,
        pivots: all(root, "pivotTable")
            .iter()
            .filter_map(|p| p.attr("name").map(str::to_string))
            .collect(),
    };
    let flags = |d: &mut CacheDef, n: &Node| {
        if n.attr("sortOrder") == Some("descending") {
            d.sort = 3;
        }
        if n.attr("customListSort") == Some("0") {
            d.custom_lists = false;
        }
        d.cross = match n.attr("crossFilter") {
            Some("none") => 1,
            Some("showItemsWithNoData") => 3,
            _ => 2,
        };
    };
    if let Some(p) = all(root, "pivotTable").first() {
        d.pivot = p.attr("name").map(str::to_string);
    }
    if let Some(t) = all(root, "tabular").first() {
        flags(&mut d, t);
        for (k, i) in all(t, "i").into_iter().enumerate() {
            if i.attr("s") != Some("1") {
                d.off_idx
                    .push(i.attr("x").and_then(|x| x.parse().ok()).unwrap_or(k));
            }
        }
    }
    if let Some(t) = all(root, "tableSlicerCache").first() {
        flags(&mut d, t);
        d.table_id = t.attr("tableId").and_then(|x| x.parse().ok());
        d.column = t.attr("column").and_then(|x| x.parse().ok());
    }
    (d.pivot.is_some() || d.table_id.is_some()).then_some(d)
}

pub(crate) fn read_timelines(root: &Node, sheet: SheetId) -> Vec<(String, Slicer)> {
    all(root, "timeline")
        .into_iter()
        .filter_map(|n| {
            let name = n.attr("name")?.to_string();
            let cache = n.attr("cache")?.to_string();
            let flag = |a: &str| n.attr(a) != Some("0");
            Some((
                cache,
                Slicer {
                    caption: n.attr("caption").unwrap_or(&name).to_string(),
                    name,
                    sheet,
                    shape: 0,
                    columns: 1,
                    style: n
                        .attr("style")
                        .unwrap_or("TimeSlicerStyleLight1")
                        .to_string(),
                    header: true,
                    locked: true,
                    col_w: 130.5,
                    row_h: 19.833_307_086_614_173,
                    tv: xlcalc::slicer::TimelineView {
                        level: n.attr("level").and_then(|x| x.parse().ok()).unwrap_or(2),
                        header: flag("showHeader"),
                        hscroll: flag("showHorizontalScrollbar"),
                        label: flag("showSelectionLabel"),
                        time_level: flag("showTimeLevel"),
                    },
                },
            ))
        })
        .collect()
}

pub(crate) fn read_slicers(root: &Node, sheet: SheetId) -> Vec<(String, Slicer)> {
    all(root, "slicer")
        .into_iter()
        .filter_map(|n| {
            let name = n.attr("name")?.to_string();
            let cache = n.attr("cache")?.to_string();
            Some((
                cache,
                Slicer {
                    caption: n.attr("caption").unwrap_or(&name).to_string(),
                    name,
                    sheet,
                    shape: 0,
                    columns: n
                        .attr("columnCount")
                        .and_then(|x| x.parse().ok())
                        .unwrap_or(1),
                    style: n.attr("style").unwrap_or("SlicerStyleLight1").to_string(),
                    header: n.attr("showCaption") != Some("0"),
                    locked: n.attr("lockedPosition") != Some("0"),
                    col_w: 130.5,
                    row_h: n
                        .attr("rowHeight")
                        .and_then(|x| x.parse::<f64>().ok())
                        .map_or(19.833_307_086_614_173, |e| e / EMU_PT),
                    tv: Default::default(),
                },
            ))
        })
        .collect()
}

pub(crate) fn adopt(
    wb: &mut Workbook,
    ids: &[SheetId],
    defs: &[CacheDef],
    slicers: &[(usize, Node)],
    table_ids: &[(u32, String)],
    pivots: &[crate::pivot::PivotDef],
    filters: &[TableValues],
) {
    for (table, col, vals) in filters {
        wb.table_values_set(table, *col, vals);
    }
    let mut made: Vec<(String, usize)> = Vec::new();
    for d in defs {
        if let (Some(range), Some(table)) = (d.timeline, &d.pivot) {
            let Ok(i) = wb.timeline_cache_add(table, &d.field) else {
                continue;
            };
            let _ = wb.slicer_cache_rename(i, &d.name);
            for p in &d.pivots {
                let _ = wb.slicer_pivot_add(i, p);
            }
            if let Some((a, b)) = range {
                wb.timeline_set(i, Some(a), Some(b));
            }
            made.push((d.name.clone(), i));
            continue;
        }
        let (table, pivot) = match (&d.pivot, d.table_id) {
            (Some(p), _) => (p.clone(), true),
            (None, Some(id)) => match table_ids.iter().find(|t| t.0 == id) {
                Some(t) => (t.1.clone(), false),
                None => continue,
            },
            _ => continue,
        };
        let Some(i) = wb.slicer_cache_import(&d.name, &table, &d.field, pivot) else {
            continue;
        };
        for p in &d.pivots {
            let _ = wb.slicer_pivot_add(i, p);
        }
        if let Some(c) = wb.slicer_cache_mut(i) {
            c.sort = d.sort;
            c.cross = d.cross;
            c.custom_lists = d.custom_lists;
        }
        if pivot {
            let labels = pivots
                .iter()
                .find(|p| {
                    p.table
                        .attr("name")
                        .is_some_and(|n| n.eq_ignore_ascii_case(&table))
                })
                .and_then(|p| {
                    let cf = all(&p.cache, "cacheField").into_iter().find(|f| {
                        f.attr("name")
                            .is_some_and(|n| n.eq_ignore_ascii_case(&d.field))
                    })?;
                    let shared = cf.child("sharedItems")?;
                    Some(
                        d.off_idx
                            .iter()
                            .filter_map(|x| {
                                shared
                                    .kids
                                    .get(*x)
                                    .and_then(|k| k.attr("v"))
                                    .map(str::to_string)
                            })
                            .collect::<Vec<_>>(),
                    )
                })
                .unwrap_or_default();

            let shown = wb.slicer_items(i);
            let off: Vec<String> = labels
                .into_iter()
                .map(|l| {
                    let num = l.parse::<f64>().ok();
                    shown
                        .iter()
                        .find(|s| {
                            s.eq_ignore_ascii_case(&l)
                                || num.is_some_and(|n| s.parse::<f64>().ok() == Some(n))
                        })
                        .cloned()
                        .unwrap_or(l)
                })
                .collect();
            if let Some(c) = wb.slicer_cache_mut(i) {
                c.off = off;
            }
            let _ = wb.slicer_apply(i);
        } else {
            wb.slicer_sync_off(i);
        }
        made.push((d.name.clone(), i));
    }
    for (idx, root) in slicers {
        let Some(&sheet) = ids.get(*idx) else {
            continue;
        };
        let list = if root.name == "timelines" {
            read_timelines(root, sheet)
        } else {
            read_slicers(root, sheet)
        };
        for (cache, s) in list {
            if let Some((_, i)) = made.iter().find(|(n, _)| n.eq_ignore_ascii_case(&cache)) {
                wb.slicer_attach(*i, s, [100.0, 100.0, 144.0, 206.25]);
            }
        }
    }
}

pub(crate) type TableValues = (String, u32, Vec<Option<String>>);

pub(crate) fn table_values(t: &Node, name: &str) -> Vec<TableValues> {
    let Some(af) = t.child("autoFilter") else {
        return Vec::new();
    };
    af.children("filterColumn")
        .filter_map(|fc| {
            let col = fc.attr("colId")?.parse().ok()?;
            let f = fc.child("filters")?;
            let mut vals: Vec<Option<String>> = f
                .children("filter")
                .filter_map(|x| x.attr("val").map(|v| Some(v.to_string())))
                .collect();
            if f.attr("blank") == Some("1") {
                vals.push(None);
            }
            Some((name.to_string(), col, vals))
        })
        .collect()
}
