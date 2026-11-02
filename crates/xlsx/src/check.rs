use crate::package::{dir_of, parse_rels, resolve, Package};
use crate::rawxml::{self, Tag};
use quick_xml::events::Event;
use std::collections::{BTreeSet, HashMap};

pub fn check_package(bytes: &[u8]) -> Vec<String> {
    let mut errs = Vec::new();
    let pkg = match Package::read(bytes) {
        Ok(p) => p,
        Err(e) => return vec![format!("not a zip package: {e}")],
    };
    let mut seen = BTreeSet::new();
    let names: Vec<String> = pkg.part_names().map(str::to_string).collect();
    for n in &names {
        if !seen.insert(n.to_lowercase()) {
            errs.push(format!("{n}: duplicate part name"));
        }
        if n.split('/').any(|s| s.is_empty() || s.ends_with('.'))
            || n.chars()
                .any(|c| matches!(c, '\\' | '%' | '?' | '#' | '<' | '>' | '"' | '|' | '*'))
        {
            errs.push(format!("{n}: invalid part name"));
        }
    }
    if pkg.part("[Content_Types].xml").is_none() {
        errs.push("no [Content_Types].xml".into());
        return errs;
    }
    for n in &names {
        if n != "[Content_Types].xml" && pkg.content_type(n).is_none() {
            errs.push(format!("{n}: no content type"));
        }
        let lower = n.to_lowercase();
        if lower.ends_with(".xml") || lower.ends_with(".rels") || lower.ends_with(".vml") {
            if let Err(e) = well_formed(pkg.part(n).unwrap_or_default()) {
                errs.push(format!("{n}: not well-formed XML: {e}"));
            }
        }
    }

    for n in names.iter().filter(|n| n.ends_with(".rels")) {
        let owner = match n.rsplit_once("_rels/") {
            Some((d, f)) => format!("{d}{}", f.trim_end_matches(".rels")),
            None => continue,
        };
        if !owner.is_empty() && pkg.part(&owner).is_none() {
            errs.push(format!("{n}: relationships of a missing part"));
        }
        let rels = match parse_rels(pkg.part(n).unwrap_or_default()) {
            Ok(r) => r,
            Err(e) => {
                errs.push(format!("{n}: {e}"));
                continue;
            }
        };
        let mut ids = BTreeSet::new();
        for r in &rels {
            if !ids.insert(r.id.clone()) {
                errs.push(format!("{n}: duplicate id {}", r.id));
            }
            if !r.external && pkg.part(&resolve(dir_of(&owner), &r.target)).is_none() {
                errs.push(format!("{n}: {} points at missing {}", r.id, r.target));
            }
        }
    }
    let root = pkg.rels("");
    let root = if root.is_empty() {
        parse_rels(pkg.part("_rels/.rels").unwrap_or_default()).unwrap_or_default()
    } else {
        root
    };
    let Some(book) = root
        .iter()
        .find(|r| r.kind() == "officeDocument")
        .map(|r| resolve("", &r.target))
    else {
        errs.push("no officeDocument relationship".into());
        return errs;
    };
    check_book(&pkg, &book, &mut errs);
    errs
}

fn well_formed(b: &[u8]) -> Result<(), String> {
    let mut rd = quick_xml::Reader::from_reader(b);
    rd.config_mut().check_end_names = true;
    let mut buf = Vec::new();
    let mut depth = 0i64;
    loop {
        match rd.read_event_into(&mut buf).map_err(|e| e.to_string())? {
            Event::Start(_) => depth += 1,
            Event::End(_) => depth -= 1,
            Event::Eof => break,
            _ => {}
        }
        buf.clear();
    }
    if depth != 0 {
        return Err("unclosed elements".into());
    }
    Ok(())
}

fn used_rel_ids(bytes: &[u8]) -> Vec<String> {
    let mut out = Vec::new();
    let Ok(s) = std::str::from_utf8(bytes) else {
        return out;
    };
    let mut rd = quick_xml::Reader::from_str(s);
    let mut prefixes: Vec<String> = Vec::new();
    loop {
        let e = match rd.read_event() {
            Ok(Event::Start(e)) | Ok(Event::Empty(e)) => e,
            Ok(Event::Eof) | Err(_) => break,
            _ => continue,
        };
        for a in e.attributes().flatten() {
            let k = a.key.as_ref().to_string();
            if let Some(p) = k.strip_prefix("xmlns:") {
                if a.value.contains("officeDocument/2006/relationships") {
                    prefixes.push(p.to_string());
                }
            } else if let Some((p, _)) = k.split_once(':') {
                if prefixes.iter().any(|x| x == p) {
                    out.push(a.value.to_string());
                }
            }
        }
    }
    out
}

const SHEET_ORDER: &[&str] = &[
    "sheetPr",
    "dimension",
    "sheetViews",
    "sheetFormatPr",
    "cols",
    "sheetData",
    "sheetCalcPr",
    "sheetProtection",
    "protectedRanges",
    "scenarios",
    "autoFilter",
    "sortState",
    "dataConsolidate",
    "customSheetViews",
    "mergeCells",
    "phoneticPr",
    "conditionalFormatting",
    "dataValidations",
    "hyperlinks",
    "printOptions",
    "pageMargins",
    "pageSetup",
    "headerFooter",
    "rowBreaks",
    "colBreaks",
    "customProperties",
    "cellWatches",
    "ignoredErrors",
    "smartTags",
    "drawing",
    "legacyDrawing",
    "legacyDrawingHF",
    "drawingHF",
    "picture",
    "oleObjects",
    "controls",
    "webPublishItems",
    "tableParts",
    "extLst",
];

fn parse_cell(s: &str) -> Option<(u32, u32)> {
    let letters = s.chars().take_while(|c| c.is_ascii_alphabetic()).count();
    let col = xlcalc::letters_to_col(&s[..letters])?;
    let row: u32 = s[letters..].parse().ok()?;
    (col < 16384 && (1..=1_048_576).contains(&row)).then_some((row - 1, col))
}

fn check_book(pkg: &Package, book: &str, errs: &mut Vec<String>) {
    let Some(bytes) = pkg.part(book) else {
        errs.push(format!("{book}: missing"));
        return;
    };
    let rels = pkg.rels(book);
    for id in used_rel_ids(bytes) {
        if !rels.iter().any(|r| r.id == id) {
            errs.push(format!("{book}: r:id {id} has no relationship"));
        }
    }
    let Ok(split) = rawxml::split(bytes) else {
        return;
    };
    let dir = dir_of(book);

    let part_of = |kind: &str| {
        rels.iter()
            .find(|r| r.kind() == kind && !r.external)
            .map(|r| resolve(dir, &r.target))
    };
    let xf_count = part_of("styles")
        .and_then(|p| pkg.part(&p).and_then(|b| rawxml::split(b).ok()))
        .and_then(|s| {
            s.kid("cellXfs")
                .and_then(|k| rawxml::split(k.raw.as_bytes()).ok())
        })
        .map_or(1, |s| s.kids.len());
    let strings: Vec<String> = part_of("sharedStrings")
        .and_then(|p| pkg.part(&p).and_then(|b| crate::xml::parse(b).ok()))
        .map(|n| n.children("si").map(crate::si_text).collect())
        .unwrap_or_default();
    let cm_count = part_of("sheetMetadata")
        .and_then(|p| pkg.part(&p).and_then(|b| crate::xml::parse(b).ok()))
        .and_then(|n| n.child("cellMetadata").map(|c| c.children("bk").count()))
        .unwrap_or(0);

    let mut sheet_names = BTreeSet::new();
    let mut sheet_ids = BTreeSet::new();
    let mut n_sheets = 0;
    let mut bk = Book {
        xf_count,
        strings,
        cm_count,
        table_names: BTreeSet::new(),
        table_ids: BTreeSet::new(),
    };
    if let Some(k) = split.kid("sheets") {
        for s in rawxml::split(k.raw.as_bytes())
            .map(|s| s.kids)
            .unwrap_or_default()
        {
            n_sheets += 1;
            let Ok(t) = Tag::parse(&s.raw) else { continue };
            let name = t.value("name").unwrap_or_default();
            if name.is_empty()
                || name.chars().count() > 31
                || name.contains(['[', ']', ':', '*', '?', '/', '\\'])
                || name.starts_with('\'')
                || name.ends_with('\'')
                || !sheet_names.insert(name.to_lowercase())
            {
                errs.push(format!("sheet name {name:?} invalid or repeated"));
            }
            if !sheet_ids.insert(t.get("sheetId").unwrap_or("").to_string()) {
                errs.push(format!("sheet {name}: repeated sheetId"));
            }
            let rid = t
                .attrs
                .iter()
                .find(|(k, _)| k.ends_with(":id"))
                .map(|(_, v)| v.clone())
                .unwrap_or_default();
            let Some(rel) = rels.iter().find(|r| r.id == rid) else {
                continue;
            };
            let part = resolve(dir, &rel.target);
            check_sheet(pkg, &part, &name, &mut bk, errs);
        }
    }
    if n_sheets == 0 {
        errs.push("workbook has no sheets".into());
    }
    if let Some(k) = split.kid("definedNames") {
        for d in rawxml::split(k.raw.as_bytes())
            .map(|s| s.kids)
            .unwrap_or_default()
        {
            let Ok(t) = Tag::parse(&d.raw) else { continue };
            if t.get("localSheetId")
                .and_then(|i| i.parse::<usize>().ok())
                .is_some_and(|i| i >= n_sheets)
            {
                errs.push(format!(
                    "defined name {}: localSheetId out of range",
                    t.value("name").unwrap_or_default()
                ));
            }
        }
    }
}

struct Book {
    xf_count: usize,
    strings: Vec<String>,
    cm_count: usize,
    table_names: BTreeSet<String>,
    table_ids: BTreeSet<String>,
}

fn check_sheet(pkg: &Package, part: &str, name: &str, bk: &mut Book, errs: &mut Vec<String>) {
    let (xf_count, cm_count) = (bk.xf_count, bk.cm_count);
    let strings = &bk.strings;
    let Some(bytes) = pkg.part(part) else {
        errs.push(format!("{name}: part {part} missing"));
        return;
    };
    let rels = pkg.rels(part);
    for id in used_rel_ids(bytes) {
        if !rels.iter().any(|r| r.id == id) {
            errs.push(format!("{name}: r:id {id} has no relationship"));
        }
    }
    let Ok(split) = rawxml::split(bytes) else {
        return;
    };
    let mut last = 0;
    for k in &split.kids {
        if let Some(i) = SHEET_ORDER.iter().position(|n| *n == k.name) {
            if i < last {
                errs.push(format!("{name}: <{}> out of schema order", k.name));
            }
            last = i;
        }
    }

    let mut texts: HashMap<(u32, u32), String> = HashMap::new();
    if let Some(sd) = split.kid("sheetData") {
        let mut rd = quick_xml::Reader::from_str(&sd.raw);
        let mut last_row: Option<u32> = None;
        let mut last_col: Option<u32> = None;
        let mut cur: Option<(u32, u32, String)> = None;
        let mut in_v = false;
        loop {
            let ev = match rd.read_event() {
                Ok(e) => e,
                Err(e) => {
                    errs.push(format!("{name}: {e}"));
                    break;
                }
            };
            let empty = matches!(ev, Event::Empty(_));
            match ev {
                Event::Start(e) | Event::Empty(e) => match e.local_name().as_ref() {
                    "row" => {
                        let r = e
                            .try_get_attribute("r")
                            .ok()
                            .flatten()
                            .and_then(|a| a.value.parse::<u32>().ok());
                        match r {
                            Some(r) if (1..=1_048_576).contains(&r) => {
                                if last_row.is_some_and(|l| r <= l) {
                                    errs.push(format!("{name}: row {r} out of order"));
                                }
                                last_row = Some(r);
                            }
                            _ => errs.push(format!("{name}: row without a valid r")),
                        }
                        last_col = None;
                    }
                    "c" => {
                        let mut at = None;
                        let mut t = String::from("n");
                        for a in e.attributes().flatten() {
                            match a.key.as_ref() {
                                "r" => at = parse_cell(&a.value),
                                "t" => t = a.value.to_string(),
                                "s" if a.value.parse::<usize>().map_or(true, |s| s >= xf_count) => {
                                    errs.push(format!("{name}: style {} out of range", a.value));
                                }
                                "cm" if a
                                    .value
                                    .parse::<usize>()
                                    .map_or(true, |c| c == 0 || c > cm_count) =>
                                {
                                    errs.push(format!("{name}: cm {} out of range", a.value));
                                }
                                _ => {}
                            }
                        }
                        match at {
                            Some((r, c)) => {
                                if last_row != Some(r + 1) {
                                    errs.push(format!("{name}: cell outside its row"));
                                }
                                if last_col.is_some_and(|l| c <= l) {
                                    errs.push(format!(
                                        "{name}: cell out of order in row {}",
                                        r + 1
                                    ));
                                }
                                last_col = Some(c);
                                cur = (!empty).then_some((r, c, t));
                            }
                            None => errs.push(format!("{name}: cell without a valid r")),
                        }
                    }
                    "v" => in_v = !empty,
                    _ => {}
                },
                Event::Text(x) => {
                    if in_v {
                        if let Some((r, c, t)) = &cur {
                            let v = x.xml10_content().to_string();
                            let text = match t.as_str() {
                                "s" => match v
                                    .trim()
                                    .parse::<usize>()
                                    .ok()
                                    .and_then(|i| strings.get(i))
                                {
                                    Some(s) => Some(s.clone()),
                                    None => {
                                        errs.push(format!(
                                            "{name}: shared string {v} out of range"
                                        ));
                                        None
                                    }
                                },
                                "str" | "inlineStr" => Some(v),
                                "n" => {
                                    if v.trim().parse::<f64>().is_err() {
                                        errs.push(format!("{name}: number {v:?} unreadable"));
                                    }
                                    None
                                }
                                _ => None,
                            };
                            if let Some(s) = text {
                                texts.insert((*r, *c), s);
                            }
                        }
                    }
                }
                Event::End(e) if e.local_name().as_ref() == "v" => in_v = false,
                Event::Eof => break,
                _ => {}
            }
        }
    }

    for r in rels.iter().filter(|r| r.kind() == "table" && !r.external) {
        let tp = resolve(dir_of(part), &r.target);
        let Some(tn) = pkg.part(&tp).and_then(|b| crate::xml::parse(b).ok()) else {
            continue;
        };
        let tname = tn.attr("displayName").unwrap_or("").to_lowercase();
        if crate::write::is_cell_like(&tname) {
            errs.push(format!(
                "{tp}: table display name {tname} reads as a cell reference"
            ));
        }
        if !bk.table_names.insert(tname.clone()) {
            errs.push(format!("{tp}: table name {tname} repeated"));
        }
        if !bk.table_ids.insert(tn.attr("id").unwrap_or("").to_string()) {
            errs.push(format!("{tp}: table id repeated"));
        }
        let rect = tn.attr("ref").and_then(|r| {
            let (a, b) = r.split_once(':').unwrap_or((r, r));
            Some((parse_cell(a)?, parse_cell(b)?))
        });
        let Some(((r0, c0), (_, c1))) = rect else {
            errs.push(format!("{tp}: bad ref"));
            continue;
        };
        let cols: Vec<String> = tn
            .child("tableColumns")
            .map(|c| {
                c.children("tableColumn")
                    .map(|x| crate::ftext::unescape_xstring(x.attr("name").unwrap_or("")))
                    .collect()
            })
            .unwrap_or_default();
        if cols.len() as u32 != c1 - c0 + 1 {
            errs.push(format!(
                "{tp}: {} columns for a ref {} wide",
                cols.len(),
                c1 - c0 + 1
            ));
        }
        if tn.attr("headerRowCount") != Some("0") {
            for (i, n) in cols.iter().enumerate() {
                let have = texts.get(&(r0, c0 + i as u32));
                if have.map(String::as_str) != Some(n.as_str()) {
                    errs.push(format!("{tp}: header cell of column {n:?} reads {have:?}"));
                }
            }
        }
    }
}

pub fn canonical_xml(bytes: &[u8]) -> Option<String> {
    let s = std::str::from_utf8(bytes).ok()?;
    let mut rd = quick_xml::Reader::from_str(s.trim_start_matches('\u{feff}'));
    let mut out = String::new();
    let mut depth = 0usize;
    let mut masked = false;
    let tag = |e: &quick_xml::events::BytesStart, out: &mut String, depth: usize, close: bool| {
        let name = e.name().as_ref().to_string();
        let mut attrs: Vec<(String, String)> = e
            .attributes()
            .flatten()
            .map(|a| {
                let k = a.key.as_ref().to_string();
                let v = match (name.rsplit(':').next(), k.as_str()) {
                    (Some("revisionPtr"), "documentId") | (Some("absPath"), "url") => "*".into(),
                    _ => rawxml::unescape(&a.value),
                };
                (k, v)
            })
            .collect();
        attrs.sort();
        out.push_str(&"  ".repeat(depth));
        out.push('<');
        out.push_str(&name);
        for (k, v) in attrs {
            out.push_str(&format!(" {k}={v:?}"));
        }
        out.push_str(if close { "/>\n" } else { ">\n" });
        name
    };
    loop {
        match rd.read_event().ok()? {
            Event::Start(e) => {
                let name = tag(&e, &mut out, depth, false);
                masked = name == "dcterms:modified";
                depth += 1;
            }
            Event::Empty(e) => {
                tag(&e, &mut out, depth, true);
            }
            Event::End(e) => {
                depth = depth.saturating_sub(1);
                out.push_str(&"  ".repeat(depth));
                out.push_str(&format!("</{}>\n", e.name().as_ref()));
                masked = false;
            }
            Event::Text(t) => {
                let t = t.xml10_content().to_string();
                if !t.trim().is_empty() {
                    out.push_str(&"  ".repeat(depth));
                    out.push_str(if masked { "*" } else { &t });
                    out.push('\n');
                }
            }
            Event::GeneralRef(r) => {
                out.push_str(&"  ".repeat(depth));
                out.push_str(&format!("&{};\n", r.as_ref()));
            }
            Event::CData(c) => {
                out.push_str(&format!("<![CDATA[{}]]>\n", c.as_ref()));
            }
            Event::Eof => break,
            _ => {}
        }
    }
    (depth == 0 && !out.is_empty()).then_some(out)
}
