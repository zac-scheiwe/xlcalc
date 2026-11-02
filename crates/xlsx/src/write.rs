use crate::package::{dir_of, parse_rels, relative, rels_name, resolve, write_rels, Package, Rel};
use crate::rawxml::{self, escape_attr, escape_text, xstring, Tag};
use crate::scenarios;
use crate::styles::StyleWriter;
use std::collections::{BTreeMap, BTreeSet, HashMap};
use std::io::Write;
use xlcalc::{
    col_to_letters, CalcMode, CellAddr, FileFormulaKind, Rect, SheetId, SheetState, Value, Workbook,
};

const NS_MAIN: &str = "http://schemas.openxmlformats.org/spreadsheetml/2006/main";
const NS_REL: &str = "http://schemas.openxmlformats.org/officeDocument/2006/relationships";
const REL: &str = "http://schemas.openxmlformats.org/officeDocument/2006/relationships/";
const REL_PKG: &str = "http://schemas.openxmlformats.org/package/2006/relationships/";
const CT: &str = "application/vnd.openxmlformats-officedocument.spreadsheetml.";

pub const CALC_ID: &str = "191029";

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum FileKind {
    Xlsx,

    Xlsm,

    Xltx,
}

#[derive(Clone, Debug, Default)]
pub struct SaveOptions {
    pub kind: Option<FileKind>,

    pub created: Option<String>,

    pub fresh_styles: bool,

    pub vba: Option<VbaWrite>,

    pub controls: Vec<(SheetId, Vec<crate::ctrlprops::Control>)>,
}

#[derive(Clone, Debug)]
pub struct VbaWrite {
    pub project: Vec<u8>,

    pub workbook_code_name: String,

    pub sheet_code_names: Vec<String>,
}

pub struct Saved {
    pub bytes: Vec<u8>,

    pub warnings: Vec<String>,
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

const POSITIONAL: &[&str] = &[
    "autoFilter",
    "sortState",
    "mergeCells",
    "conditionalFormatting",
    "dataValidations",
    "hyperlinks",
    "rowBreaks",
    "colBreaks",
    "ignoredErrors",
    "cols",
    "drawing",
    "legacyDrawing",
    "protectedRanges",
    "scenarios",
];

const BOOK_ORDER: &[&str] = &[
    "fileVersion",
    "fileSharing",
    "workbookPr",
    "AlternateContent",
    "revisionPtr",
    "workbookProtection",
    "bookViews",
    "sheets",
    "functionGroups",
    "externalReferences",
    "definedNames",
    "calcPr",
    "oleSize",
    "customWorkbookViews",
    "pivotCaches",
    "smartTagPr",
    "smartTagTypes",
    "webPublishing",
    "fileRecoveryPr",
    "webPublishObjects",
    "extLst",
];

const TABLE_ORDER: &[&str] = &[
    "autoFilter",
    "sortState",
    "tableColumns",
    "tableStyleInfo",
    "extLst",
];

const NEW_BOOK: &str = concat!(
    "<workbook xmlns=\"http://schemas.openxmlformats.org/spreadsheetml/2006/main\" ",
    "xmlns:r=\"http://schemas.openxmlformats.org/officeDocument/2006/relationships\">",
    "<workbookPr/><bookViews><workbookView/></bookViews><sheets/></workbook>"
);

const NEW_SHEET: &str = concat!(
    "<worksheet xmlns=\"http://schemas.openxmlformats.org/spreadsheetml/2006/main\" ",
    "xmlns:r=\"http://schemas.openxmlformats.org/officeDocument/2006/relationships\">",
    "<sheetViews><sheetView workbookViewId=\"0\"/></sheetViews>",
    "<sheetFormatPr defaultRowHeight=\"15\"/>",
    "<pageMargins left=\"0.7\" right=\"0.7\" top=\"0.75\" bottom=\"0.75\" header=\"0.3\" footer=\"0.3\"/>",
    "</worksheet>"
);

const METADATA: &str = concat!(
    "<?xml version=\"1.0\" encoding=\"UTF-8\" standalone=\"yes\"?>\r\n",
    "<metadata xmlns=\"http://schemas.openxmlformats.org/spreadsheetml/2006/main\" ",
    "xmlns:xda=\"http://schemas.microsoft.com/office/spreadsheetml/2017/dynamicarray\">",
    "<metadataTypes count=\"1\"><metadataType name=\"XLDAPR\" minSupportedVersion=\"120000\" ",
    "copy=\"1\" pasteAll=\"1\" pasteValues=\"1\" merge=\"1\" splitFirst=\"1\" rowColShift=\"1\" ",
    "clearFormats=\"1\" clearComments=\"1\" assign=\"1\" coerce=\"1\" cellMeta=\"1\"/></metadataTypes>",
    "<futureMetadata name=\"XLDAPR\" count=\"1\"><bk><extLst>",
    "<ext uri=\"{bdbb8cdc-fa1e-496e-a857-3c3f30c029c3}\">",
    "<xda:dynamicArrayProperties fDynamic=\"1\" fCollapsed=\"0\"/></ext></extLst></bk></futureMetadata>",
    "<cellMetadata count=\"1\"><bk><rc t=\"1\" v=\"0\"/></bk></cellMetadata></metadata>"
);

pub(crate) fn number_text(n: f64) -> String {
    let plain = format!("{n}");
    let exp = format!("{n:e}").replace('e', "E");
    if exp.len() < plain.len() {
        exp
    } else {
        plain
    }
}

fn a1(r: u32, c: u32) -> String {
    format!("{}{}", col_to_letters(c), r + 1)
}

fn rect_text(r: &Rect) -> String {
    if r.r0 == r.r1 && r.c0 == r.c1 {
        a1(r.r0, r.c0)
    } else {
        format!("{}:{}", a1(r.r0, r.c0), a1(r.r1, r.c1))
    }
}

fn formula_xml(text: &str) -> String {
    escape_text(
        &text
            .replace(crate::ftext::ALIAS, "[0]!")
            .replace(crate::ftext::BARE_TABLE, ""),
    )
}

struct Out {
    parts: BTreeMap<String, Vec<u8>>,

    types: HashMap<String, String>,

    copied: BTreeSet<String>,
    warnings: Vec<String>,
}

impl Out {
    fn put(&mut self, name: &str, bytes: Vec<u8>, ty: Option<&str>) {
        self.parts.insert(name.to_string(), bytes);
        if let Some(t) = ty {
            self.types.insert(name.to_string(), t.to_string());
        }
    }
}

fn fresh_id(used: &mut BTreeSet<String>) -> String {
    let mut i = 1;
    loop {
        let id = format!("rId{i}");
        if used.insert(id.clone()) {
            return id;
        }
        i += 1;
    }
}

fn fresh_part(out: &Out, src: &Package, pattern: &str) -> String {
    let mut i = 1;
    loop {
        let p = pattern.replace('#', &i.to_string());
        if !out.parts.contains_key(&p) && src.part(&p).is_none() {
            return p;
        }
        i += 1;
    }
}

fn col_state_xml(wb: &Workbook, sheet: SheetId) -> Option<String> {
    let state = wb.column_state(sheet);
    if state.is_empty() {
        return None;
    }
    let mut out = String::from("<cols>");
    for (c, px, hand, hidden) in state {
        out.push_str(&format!(
            "<col min=\"{n}\" max=\"{n}\" width=\"{}\"{}{}/>",
            xlcalc::px_to_file_width(px),
            if hidden { " hidden=\"1\"" } else { "" },
            if hand || px != wb.standard_px() {
                " customWidth=\"1\""
            } else {
                ""
            },
            n = c + 1,
        ));
    }
    out.push_str("</cols>");
    Some(out)
}

fn map_col_styles(raw: &str, styles: &mut StyleWriter) -> Result<String, String> {
    let sp = rawxml::split(raw.as_bytes())?;
    let mut out = sp.root.open();
    for k in &sp.kids {
        let n = rawxml::head_len(&k.raw).unwrap_or(k.raw.len());
        let mut t = Tag::parse(&k.raw[..n])?;
        match t.get("style").and_then(|v| v.parse::<u32>().ok()) {
            Some(i) => {
                let x = styles.map_source(i).to_string();
                t.set("style", Some(&x));
                out.push_str(&t.write());
                out.push_str(&k.raw[n..]);
            }
            None => out.push_str(&k.raw),
        }
    }
    out.push_str(&format!("</{}>", sp.root.name));
    Ok(out)
}

#[derive(Default)]
struct SourceCells {
    style: HashMap<(u32, u32), u32>,

    rows: BTreeMap<u32, Vec<(String, String)>>,
}

fn parse_cell(s: &str) -> Option<(u32, u32)> {
    let letters = s.chars().take_while(|c| c.is_ascii_alphabetic()).count();
    let col = xlcalc::letters_to_col(&s[..letters])?;
    let row: u32 = s[letters..].parse().ok()?;
    Some((row.checked_sub(1)?, col))
}

fn source_cells(sheet_data: &str) -> SourceCells {
    use quick_xml::events::Event;
    let mut out = SourceCells::default();
    let mut rd = quick_xml::Reader::from_str(sheet_data);
    let mut row = 0u32;
    let mut next_col = 0u32;
    loop {
        let e = match rd.read_event() {
            Ok(Event::Start(e)) | Ok(Event::Empty(e)) => e,
            Ok(Event::Eof) | Err(_) => break,
            _ => continue,
        };
        match e.local_name().as_ref() {
            "row" => {
                let mut attrs = Vec::new();
                let mut r = None;
                for a in e.attributes().flatten() {
                    let k = a.key.as_ref().to_string();
                    match k.as_str() {
                        "r" => r = a.value.parse::<u32>().ok(),
                        "spans" | "hidden" | "ht" | "customHeight" => {}
                        _ => attrs.push((k, a.value.to_string())),
                    }
                }
                row = r.unwrap_or(row + 1);
                next_col = 0;
                if !attrs.is_empty() {
                    out.rows.insert(row - 1, attrs);
                }
            }
            "c" => {
                let mut at = None;
                let mut s = None;
                for a in e.attributes().flatten() {
                    match a.key.as_ref() {
                        "r" => at = parse_cell(&a.value),
                        "s" => s = a.value.parse::<u32>().ok(),
                        _ => {}
                    }
                }
                let (r, c) = at.unwrap_or((row.saturating_sub(1), next_col));
                next_col = c + 1;
                if let Some(s) = s.filter(|&s| s != 0) {
                    out.style.insert((r, c), s);
                }
            }
            _ => {}
        }
    }
    out
}

struct Strings {
    index: HashMap<String, u32>,
    items: Vec<String>,
    source: HashMap<String, String>,
}

impl Strings {
    fn new(src: Option<&[u8]>) -> Strings {
        let mut source = HashMap::new();
        if let Some(split) = src.and_then(|b| rawxml::split(b).ok()) {
            for k in split.kids.iter().filter(|k| k.name == "si") {
                if let Ok(n) = crate::xml::parse(k.raw.as_bytes()) {
                    source
                        .entry(crate::si_text(&n))
                        .or_insert_with(|| k.raw.clone());
                }
            }
        }
        Strings {
            index: HashMap::new(),
            items: Vec::new(),
            source,
        }
    }

    fn add_inline(&mut self, sheet: &[u8]) {
        let Ok(t) = std::str::from_utf8(sheet) else {
            return;
        };
        let mut at = 0;
        while let Some(i) = t[at..].find("<is>") {
            let start = at + i;
            let Some(j) = t[start..].find("</is>") else {
                break;
            };
            let inner = &t[start + 4..start + j];
            if inner.contains("<r>") || inner.contains("<r ") {
                let raw = format!("<si>{inner}</si>");
                if let Ok(n) = crate::xml::parse(raw.as_bytes()) {
                    self.source.entry(crate::si_text(&n)).or_insert(raw);
                }
            }
            at = start + j + 5;
        }
    }

    fn get(&mut self, s: &str) -> u32 {
        if let Some(&i) = self.index.get(s) {
            return i;
        }
        let raw = self.source.get(s).cloned().unwrap_or_else(|| {
            let space = s.starts_with(char::is_whitespace)
                || s.ends_with(char::is_whitespace)
                || s.contains('\n');
            format!(
                "<si><t{}>{}</t></si>",
                if space { " xml:space=\"preserve\"" } else { "" },
                escape_text(&xstring(s))
            )
        });
        let i = self.items.len() as u32;
        self.items.push(raw);
        self.index.insert(s.to_string(), i);
        i
    }
}

struct SourceTable {
    sheet_part: String,
    rel_id: String,
    part: String,
    bytes: Vec<u8>,
    id: u32,
}

pub fn save(wb: &Workbook, pkg: Option<&Package>, opts: &SaveOptions) -> Result<Saved, String> {
    let empty = Package::default();
    let src = pkg.unwrap_or(&empty);
    let new_book = pkg.is_none();
    let book_part = "xl/workbook.xml";
    let src_book_rels = src.rels(book_part);
    let src_vba = src_book_rels.iter().any(|r| r.kind() == "vbaProject");
    let kind = opts.kind.unwrap_or(if src_vba {
        FileKind::Xlsm
    } else {
        FileKind::Xlsx
    });
    let mut out = Out {
        parts: BTreeMap::new(),
        types: HashMap::new(),
        copied: BTreeSet::new(),
        warnings: Vec::new(),
    };

    let mut owned: BTreeSet<String> = BTreeSet::new();
    for p in [
        "[Content_Types].xml",
        book_part,
        "docProps/app.xml",
        "_rels/.rels",
    ] {
        owned.insert(p.to_lowercase());
    }
    owned.insert(rels_name(book_part).to_lowercase());
    for r in &src_book_rels {
        if matches!(
            r.kind(),
            "worksheet" | "sharedStrings" | "styles" | "calcChain" | "sheetMetadata"
        ) && !r.external
        {
            let p = resolve("xl", &r.target);
            owned.insert(rels_name(&p).to_lowercase());
            owned.insert(p.to_lowercase());
        }
    }
    let src_part = |kind: &str| -> Option<String> {
        src_book_rels
            .iter()
            .find(|r| r.kind() == kind && !r.external)
            .map(|r| resolve("xl", &r.target))
    };

    let mut src_tables: HashMap<String, SourceTable> = HashMap::new();
    let mut max_table_id = 0;
    for s in &src.sheets {
        for r in src.rels(&s.part) {
            if r.kind() != "table" || r.external {
                continue;
            }
            let p = resolve(dir_of(&s.part), &r.target);
            owned.insert(p.to_lowercase());
            let Some(b) = src.part(&p) else { continue };
            let Ok(sp) = rawxml::split(b) else { continue };
            let name = sp
                .root
                .value("displayName")
                .or(sp.root.value("name"))
                .unwrap_or_default();
            let id = sp.root.get("id").and_then(|i| i.parse().ok()).unwrap_or(0);
            max_table_id = max_table_id.max(id);
            src_tables.insert(
                name.to_lowercase(),
                SourceTable {
                    sheet_part: s.part.clone(),
                    rel_id: r.id.clone(),
                    part: p,
                    bytes: b.to_vec(),
                    id,
                },
            );
        }
    }

    let mut used_ids: BTreeSet<String> = src_book_rels.iter().map(|r| r.id.clone()).collect();
    let mut book_rels: Vec<Rel> = src_book_rels
        .iter()
        .filter(|r| match r.kind() {
            "worksheet" | "sharedStrings" | "styles" | "calcChain" | "sheetMetadata" => r.external,
            "vbaProject" => kind == FileKind::Xlsm,
            _ => true,
        })
        .cloned()
        .collect();

    if let Some(v) = opts.vba.as_ref().filter(|_| kind == FileKind::Xlsm) {
        {
            let existing = src_book_rels
                .iter()
                .find(|r| r.kind() == "vbaProject" && !r.external);
            let name = match existing {
                Some(r) => resolve("xl", &r.target),
                None => fresh_part(&out, src, "xl/vbaProject.bin"),
            };
            if existing.is_none() {
                book_rels.push(Rel {
                    id: fresh_id(&mut used_ids),
                    ty: "http://schemas.microsoft.com/office/2006/relationships/vbaProject".into(),
                    target: relative("xl", &name),
                    external: false,
                });
            }
            owned.insert(name.to_lowercase());
            out.put(
                &name,
                v.project.clone(),
                Some("application/vnd.ms-office.vbaProject"),
            );
        }
    }

    let mut styles = StyleWriter::new(
        src_part("styles").and_then(|p| src.part(&p)),
        wb,
        opts.fresh_styles,
    )?;
    let mut strings = Strings::new(src_part("sharedStrings").and_then(|p| src.part(&p)));
    for sh in &src.sheets {
        if let Some(b) = src.part(&sh.part) {
            strings.add_inline(b);
        }
    }
    let dyn_cm = 1u32;
    let mut any_dynamic = false;

    let mut sheet_ids: BTreeSet<u32> = src.sheets.iter().map(|s| s.sheet_id).collect();
    let mut sheet_entries: Vec<String> = Vec::new();
    let r_prefix = |t: &Tag| t.prefix_of(NS_REL);
    let mut used_tables: BTreeSet<String> = BTreeSet::new();
    let mut table_ids: BTreeSet<u32> = src_tables.values().map(|t| t.id).collect();
    for (sheet_pos, sheet) in wb.sheets().enumerate() {
        let id = sheet.id;
        let source = src.source_sheet(id);
        let (part, rel_id, sheet_id) = match source {
            Some(s) => (s.part.clone(), s.rel_id.clone(), s.sheet_id),
            None => {
                let p = fresh_part(&out, src, "xl/worksheets/sheet#.xml");
                let rid = fresh_id(&mut used_ids);
                let mut n = 1;
                while sheet_ids.contains(&n) {
                    n += 1;
                }
                sheet_ids.insert(n);
                (p, rid, n)
            }
        };
        book_rels.push(Rel {
            id: rel_id.clone(),
            ty: format!("{REL}worksheet"),
            target: relative("xl", &part),
            external: false,
        });
        let mut st = Tag::new("sheet");
        st.set("name", Some(&sheet.name))
            .set("sheetId", Some(&sheet_id.to_string()));
        match sheet.state {
            SheetState::Visible => {}
            SheetState::Hidden => {
                st.set("state", Some("hidden"));
            }
            SheetState::VeryHidden => {
                st.set("state", Some("veryHidden"));
            }
        }
        st.set("r:id", Some(&rel_id));
        sheet_entries.push(st.empty());

        let src_bytes = source.and_then(|s| src.part(&s.part));
        let mut split = rawxml::split(src_bytes.unwrap_or(NEW_SHEET.as_bytes()))?;
        let cells = split
            .kid("sheetData")
            .map(|k| source_cells(&k.raw))
            .unwrap_or_default();

        if let Some(cols) = split.kid("cols").map(|k| k.raw.clone()) {
            split.put(
                "cols",
                Some(map_col_styles(&cols, &mut styles)?),
                SHEET_ORDER,
            );
        } else if let Some(cols) = col_state_xml(wb, id) {
            split.put("cols", Some(cols), SHEET_ORDER);
        }
        if let Some(name) = opts
            .vba
            .as_ref()
            .and_then(|v| v.sheet_code_names.get(sheet_pos))
        {
            let raw = split.kid("sheetPr").map(|k| k.raw.clone());
            split.put(
                "sheetPr",
                Some(sheet_pr(raw.as_deref(), name)?),
                SHEET_ORDER,
            );
        }
        let rp = match r_prefix(&split.root) {
            Some(p) if !p.is_empty() => p,
            _ => {
                split.root.set("xmlns:r", Some(NS_REL));
                "r".to_string()
            }
        };

        let src_rels = source.map(|s| src.rels(&s.part)).unwrap_or_default();
        let mut sheet_used: BTreeSet<String> = src_rels.iter().map(|r| r.id.clone()).collect();
        let mut sheet_rels: Vec<Rel> = src_rels
            .iter()
            .filter(|r| r.kind() != "table" || r.external)
            .cloned()
            .collect();
        let mut table_parts: Vec<String> = Vec::new();
        for t in wb.tables().iter().filter(|t| t.sheet == id) {
            let matched = src_tables
                .get(&t.name.to_lowercase())
                .filter(|s| !used_tables.contains(&s.part));
            let (tpart, trel, tid, base) = match matched {
                Some(s) => {
                    used_tables.insert(s.part.clone());
                    let rid =
                        if s.sheet_part == part && !sheet_rels.iter().any(|r| r.id == s.rel_id) {
                            s.rel_id.clone()
                        } else {
                            fresh_id(&mut sheet_used)
                        };
                    (s.part.clone(), rid, s.id, Some(s.bytes.as_slice()))
                }
                None => {
                    let p = fresh_part(&out, src, "xl/tables/table#.xml");
                    let mut n = max_table_id + 1;
                    while table_ids.contains(&n) {
                        n += 1;
                    }
                    table_ids.insert(n);
                    (p, fresh_id(&mut sheet_used), n, None)
                }
            };
            let bytes = table_xml(t, tid, base)?;
            out.put(&tpart, bytes, Some(&format!("{CT}table+xml")));
            sheet_rels.push(Rel {
                id: trel.clone(),
                ty: format!("{REL}table"),
                target: relative(dir_of(&part), &tpart),
                external: false,
            });
            table_parts.push(format!("<tablePart {rp}:id=\"{}\"/>", escape_attr(&trel)));
        }
        let tp = (!table_parts.is_empty()).then(|| {
            format!(
                "<tableParts count=\"{}\">{}</tableParts>",
                table_parts.len(),
                table_parts.concat()
            )
        });
        split.put("tableParts", tp, SHEET_ORDER);
        split.put("scenarios", scenarios::write(wb.scenarios(id)), SHEET_ORDER);
        if let Some((_, list)) = opts.controls.iter().find(|(s, _)| *s == id) {
            let env = crate::ctrlprops::Env {
                src,
                source_part: source.map(|s| s.part.as_str()),
                part: &part,
                rp: &rp,
            };
            let taken = |p: &str| out.parts.contains_key(p) || src.part(p).is_some();
            let new = crate::ctrlprops::write_sheet(
                &env,
                list,
                &mut split,
                &mut sheet_rels,
                &mut sheet_used,
                &taken,
                SHEET_ORDER,
            )?;
            for (name, bytes, ty) in new {
                out.put(&name, bytes, Some(ty));
            }
        }

        let (data, dim) = sheet_data(
            wb,
            id,
            &cells,
            &mut styles,
            &mut strings,
            dyn_cm,
            &mut any_dynamic,
        );
        split.put("sheetData", Some(data), SHEET_ORDER);
        split.put(
            "dimension",
            Some(format!("<dimension ref=\"{dim}\"/>")),
            SHEET_ORDER,
        );
        if pkg.is_some_and(|p| p.structurally_edited(id)) {
            let stale: Vec<&str> = split
                .kids
                .iter()
                .map(|k| k.name.as_str())
                .filter(|n| POSITIONAL.contains(n))
                .collect();
            if !stale.is_empty() {
                out.warnings.push(format!(
                    "{}: kept as it was in the source file, references not adjusted after the structural edit: {}",
                    sheet.name,
                    stale.join(", ")
                ));
            }
        }
        if wb.has_line_formats(id) {
            out.warnings.push(format!(
                "{}: whole-row, whole-column and sheet styles are not written",
                sheet.name
            ));
        }
        out.put(&part, split.write(), Some(&format!("{CT}worksheet+xml")));
        if !sheet_rels.is_empty() {
            out.put(&rels_name(&part), write_rels(&sheet_rels), None);
        }
    }

    let sst_part = src_part("sharedStrings").unwrap_or("xl/sharedStrings.xml".into());
    if !strings.items.is_empty() {
        let mut s = String::from(rawxml::DECL);
        s.push_str(&format!(
            "<sst xmlns=\"{NS_MAIN}\" count=\"{}\" uniqueCount=\"{}\">",
            strings.items.len(),
            strings.items.len()
        ));
        s.push_str(&strings.items.concat());
        s.push_str("</sst>");
        out.put(
            &sst_part,
            s.into_bytes(),
            Some(&format!("{CT}sharedStrings+xml")),
        );
        book_rels.push(owned_rel(
            &src_book_rels,
            &mut used_ids,
            "sharedStrings",
            &sst_part,
        ));
    }
    let styles_part = src_part("styles").unwrap_or("xl/styles.xml".into());
    out.put(
        &styles_part,
        styles.write(),
        Some(&format!("{CT}styles+xml")),
    );
    book_rels.push(owned_rel(
        &src_book_rels,
        &mut used_ids,
        "styles",
        &styles_part,
    ));
    if any_dynamic {
        let p = src_part("sheetMetadata").unwrap_or("xl/metadata.xml".into());
        out.put(
            &p,
            METADATA.as_bytes().to_vec(),
            Some(&format!("{CT}sheetMetadata+xml")),
        );
        book_rels.push(owned_rel(
            &src_book_rels,
            &mut used_ids,
            "sheetMetadata",
            &p,
        ));
    }

    let mut book = rawxml::split(src.part(book_part).unwrap_or(NEW_BOOK.as_bytes()))?;
    let rp = match r_prefix(&book.root) {
        Some(p) if !p.is_empty() => p,
        _ => {
            book.root.set("xmlns:r", Some(NS_REL));
            "r".to_string()
        }
    };
    let sheets_xml = format!(
        "<sheets>{}</sheets>",
        sheet_entries
            .iter()
            .map(|s| s.replacen(" r:id=", &format!(" {rp}:id="), 1))
            .collect::<String>()
    );
    book.put("sheets", Some(sheets_xml), BOOK_ORDER);
    let pr = workbook_pr(
        wb,
        book.kid("workbookPr").map(|k| k.raw.as_str()),
        opts.vba.as_ref().map(|v| v.workbook_code_name.as_str()),
    )?;
    book.put("workbookPr", Some(pr), BOOK_ORDER);
    if let Some(bv) = book.kid("bookViews").map(|k| k.raw.clone()) {
        book.put("bookViews", Some(book_views(wb, src, &bv)?), BOOK_ORDER);
    }
    let names = defined_names(wb, src, book.kid("definedNames").map(|k| k.raw.as_str()))?;
    book.put("definedNames", names, BOOK_ORDER);
    let calc = calc_pr(wb, book.kid("calcPr").map(|k| k.raw.as_str()))?;
    book.put("calcPr", Some(calc), BOOK_ORDER);
    let main = match kind {
        FileKind::Xlsx => format!("{CT}sheet.main+xml"),
        FileKind::Xlsm => "application/vnd.ms-excel.sheet.macroEnabled.main+xml".to_string(),
        FileKind::Xltx => format!("{CT}template.main+xml"),
    };
    out.put(book_part, book.write(), Some(&main));
    out.put(&rels_name(book_part), write_rels(&book_rels), None);

    let mut root_rels = src.rels("");
    if root_rels.is_empty() {
        root_rels = parse_rels(src.part("_rels/.rels").unwrap_or_default()).unwrap_or_default();
    }
    if !root_rels.iter().any(|r| r.kind() == "officeDocument") {
        root_rels.push(Rel {
            id: "rId1".into(),
            ty: format!("{REL}officeDocument"),
            target: book_part.into(),
            external: false,
        });
    }
    if new_book {
        root_rels.push(Rel {
            id: "rId2".into(),
            ty: format!("{REL_PKG}metadata/core-properties"),
            target: "docProps/core.xml".into(),
            external: false,
        });
        root_rels.push(Rel {
            id: "rId3".into(),
            ty: format!("{REL}extended-properties"),
            target: "docProps/app.xml".into(),
            external: false,
        });
        out.put(
            "docProps/core.xml",
            core_xml(opts.created.as_deref()),
            Some("application/vnd.openxmlformats-package.core-properties+xml"),
        );
    }
    out.put("_rels/.rels", write_rels(&root_rels), None);
    if new_book || src.part("docProps/app.xml").is_some() {
        out.put(
            "docProps/app.xml",
            app_xml(wb, src.part("docProps/app.xml"))?,
            Some("application/vnd.openxmlformats-officedocument.extended-properties+xml"),
        );
    }

    for name in src.part_names() {
        if owned.contains(&name.to_lowercase()) || out.parts.contains_key(name) {
            continue;
        }
        if let Some(b) = src.part(name) {
            out.parts.insert(name.to_string(), b.to_vec());
            out.copied.insert(name.to_string());
        }
    }
    collect_garbage(&mut out);

    let ct = content_types(&out, src);
    out.parts.remove("[Content_Types].xml");
    let order = part_order(&out, src);
    let mut zw = zip::ZipWriter::new(std::io::Cursor::new(Vec::new()));
    let o = zip::write::SimpleFileOptions::default()
        .compression_method(zip::CompressionMethod::Deflated);
    zw.start_file("[Content_Types].xml", o)
        .map_err(|e| e.to_string())?;
    zw.write_all(&ct).map_err(|e| e.to_string())?;
    for name in order {
        zw.start_file(name.as_str(), o).map_err(|e| e.to_string())?;
        zw.write_all(&out.parts[&name]).map_err(|e| e.to_string())?;
    }
    let bytes = zw.finish().map_err(|e| e.to_string())?.into_inner();
    Ok(Saved {
        bytes,
        warnings: out.warnings,
    })
}

fn owned_rel(src: &[Rel], used: &mut BTreeSet<String>, kind: &str, part: &str) -> Rel {
    let id = src
        .iter()
        .find(|r| r.kind() == kind && !r.external)
        .map(|r| r.id.clone())
        .unwrap_or_else(|| fresh_id(used));
    Rel {
        id,
        ty: format!("{REL}{kind}"),
        target: relative("xl", part),
        external: false,
    }
}

fn collect_garbage(out: &mut Out) {
    let mut reached: BTreeSet<String> = BTreeSet::new();
    let mut queue = vec![String::new()];
    while let Some(p) = queue.pop() {
        let rn = rels_name(&p);
        let rn = if p.is_empty() {
            "_rels/.rels".into()
        } else {
            rn
        };
        let Some(b) = out.parts.get(&rn) else {
            continue;
        };
        reached.insert(rn.clone());
        for r in parse_rels(b).unwrap_or_default() {
            if r.external {
                continue;
            }
            let t = resolve(dir_of(&p), &r.target);
            let t = out
                .parts
                .keys()
                .find(|k| k.eq_ignore_ascii_case(&t))
                .cloned()
                .unwrap_or(t);
            if reached.insert(t.clone()) {
                queue.push(t);
            }
        }
    }
    let drop: Vec<String> = out
        .copied
        .iter()
        .filter(|p| !reached.contains(*p))
        .cloned()
        .collect();
    for p in drop {
        out.parts.remove(&p);
        out.copied.remove(&p);
    }
}

fn content_types(out: &Out, src: &Package) -> Vec<u8> {
    let mut defaults: BTreeMap<String, String> = src.defaults().clone();
    defaults
        .entry("rels".into())
        .or_insert_with(|| "application/vnd.openxmlformats-package.relationships+xml".into());
    defaults
        .entry("xml".into())
        .or_insert_with(|| "application/xml".into());
    let mut overrides: Vec<(String, String)> = Vec::new();
    for name in out.parts.keys() {
        let ty = match out.types.get(name) {
            Some(t) => Some(t.clone()),
            None => src.content_type(name).map(str::to_string),
        };
        let ext = name
            .rsplit_once('.')
            .map(|(_, e)| e.to_lowercase())
            .unwrap_or_default();
        match (ty, defaults.get(&ext)) {
            (Some(t), Some(d)) if t == *d => {}
            (Some(t), _) => overrides.push((name.clone(), t)),
            (None, _) => {}
        }
    }
    let mut s = String::from(rawxml::DECL);
    s.push_str("<Types xmlns=\"http://schemas.openxmlformats.org/package/2006/content-types\">");
    for (e, t) in &defaults {
        s.push_str(&format!(
            "<Default Extension=\"{}\" ContentType=\"{}\"/>",
            escape_attr(e),
            escape_attr(t)
        ));
    }
    for (p, t) in &overrides {
        s.push_str(&format!(
            "<Override PartName=\"/{}\" ContentType=\"{}\"/>",
            escape_attr(p),
            escape_attr(t)
        ));
    }
    s.push_str("</Types>");
    s.into_bytes()
}

fn part_order(out: &Out, src: &Package) -> Vec<String> {
    let mut order: Vec<String> = Vec::new();
    if out.parts.contains_key("_rels/.rels") {
        order.push("_rels/.rels".into());
    }
    for n in src.part_names() {
        if out.parts.contains_key(n) && !order.iter().any(|o| o == n) {
            order.push(n.to_string());
        }
    }
    for n in out.parts.keys() {
        if !order.contains(n) {
            order.push(n.clone());
        }
    }
    order
}

fn sheet_data(
    wb: &Workbook,
    id: SheetId,
    src: &SourceCells,
    styles: &mut StyleWriter,
    strings: &mut Strings,
    dyn_cm: u32,
    any_dynamic: &mut bool,
) -> (String, String) {
    let Some(sheet) = wb.sheet(id) else {
        return ("<sheetData/>".into(), "A1".into());
    };

    let mut cells: BTreeMap<(u32, u32), bool> = BTreeMap::new();
    let mut formulas: HashMap<(u32, u32), String> = HashMap::new();
    let mut cm: BTreeSet<(u32, u32)> = BTreeSet::new();
    for (r, c, e) in sheet.entries() {
        let addr = CellAddr::new(id, r, c);
        match e {
            xlcalc::Entry::Const(_) => {
                cells.insert((r, c), true);
            }
            xlcalc::Entry::Formula(_) => {
                cells.entry((r, c)).or_insert(false);
                let Some(f) = wb.file_formula(addr) else {
                    continue;
                };
                let body = formula_xml(&f.text);
                let (xml, block) = match f.kind {
                    FileFormulaKind::Plain => (format!("<f>{body}</f>"), None),
                    FileFormulaKind::Dynamic { spill } => {
                        cm.insert((r, c));
                        *any_dynamic = true;
                        (
                            format!("<f t=\"array\" ref=\"{}\">{body}</f>", rect_text(&spill)),
                            Some(spill),
                        )
                    }
                    FileFormulaKind::Array(rect) => (
                        format!("<f t=\"array\" ref=\"{}\">{body}</f>", rect_text(&rect)),
                        Some(rect),
                    ),
                };
                formulas.insert((r, c), xml);
                if let Some(b) = block {
                    for rr in b.r0..=b.r1 {
                        for cc in b.c0..=b.c1 {
                            cells.entry((rr, cc)).or_insert(false);
                        }
                    }
                }
            }
        }
    }
    for t in wb.data_tables().iter().filter(|t| t.sheet == id) {
        let i = t.interior();
        let top = CellAddr::new(id, i.r0, i.c0);
        let args = wb
            .formula_text(top)
            .and_then(|f| {
                f.strip_prefix("=TABLE(")
                    .and_then(|x| x.strip_suffix(')'))
                    .map(str::to_string)
            })
            .unwrap_or_default();
        let (row_in, col_in) = args.split_once(',').unwrap_or((&args, ""));
        let mut tag = Tag::new("f");
        tag.set("t", Some("dataTable"))
            .set("ref", Some(&rect_text(&i)));
        match (row_in.is_empty(), col_in.is_empty()) {
            (false, false) => {
                tag.set("dt2D", Some("1"))
                    .set("dtr", Some("1"))
                    .set("r1", Some(row_in))
                    .set("r2", Some(col_in));
            }
            (false, true) => {
                tag.set("dt2D", Some("0"))
                    .set("dtr", Some("1"))
                    .set("r1", Some(row_in));
            }
            _ => {
                tag.set("dt2D", Some("0"))
                    .set("dtr", Some("0"))
                    .set("r1", Some(col_in));
            }
        }
        formulas.insert((i.r0, i.c0), tag.empty());
        for rr in i.r0..=i.r1 {
            for cc in i.c0..=i.c1 {
                cells.entry((rr, cc)).or_insert(false);
            }
        }
    }
    for (r, c) in wb.formatted_cells(id) {
        cells.entry((r, c)).or_insert(false);
    }
    for &(r, c) in src.style.keys() {
        cells.entry((r, c)).or_insert(false);
    }
    let hidden: BTreeSet<u32> = wb.hidden_rows(id).into_iter().collect();
    let mut rows: BTreeSet<u32> = cells.keys().map(|k| k.0).collect();
    rows.extend(&hidden);
    rows.extend(src.rows.keys());
    let heights: std::collections::BTreeMap<u32, u32> =
        wb.row_custom_heights(id).into_iter().collect();
    rows.extend(heights.keys());

    let mut s = String::from("<sheetData>");
    let mut bounds: Option<Rect> = None;
    let mut it = cells.iter().peekable();
    for r in rows {
        let mut tag = Tag::new("row");
        tag.set("r", Some(&(r + 1).to_string()));
        if let Some(a) = src.rows.get(&r) {
            for (k, v) in a {
                let v = match (k.as_str(), v.parse::<u32>()) {
                    ("s", Ok(i)) => styles.map_source(i).to_string(),
                    _ => v.clone(),
                };
                tag.attrs.push((k.clone(), v));
            }
        }
        if let Some(h8) = heights.get(&r) {
            tag.set("ht", Some(&(f64::from(*h8) / 8.0).to_string()));
            tag.set("customHeight", Some("1"));
        }
        if hidden.contains(&r) {
            tag.set("hidden", Some("1"));
        }
        let mut body = String::new();
        while let Some(&(&(cr, cc), &is_const)) = it.peek() {
            if cr != r {
                break;
            }
            it.next();
            let addr = CellAddr::new(id, cr, cc);
            let xf = styles.xf(
                wb.cell_style(addr),
                wb.has_apostrophe(addr),
                src.style.get(&(cr, cc)).copied(),
            );
            let value = wb.value(addr);
            let f = formulas.get(&(cr, cc));
            if f.is_none() && matches!(value, Value::Blank) && xf == 0 {
                continue;
            }
            let b = Rect::new(cr, cc, cr, cc);
            bounds = Some(bounds.map_or(b, |x| x.union_bounds(&b)));
            body.push_str(&format!("<c r=\"{}\"", a1(cr, cc)));
            if xf != 0 {
                body.push_str(&format!(" s=\"{xf}\""));
            }
            let (t, v) = match &value {
                Value::Number(n) => (None, Some(number_text(*n))),
                Value::Text(x) if is_const => (Some("s"), Some(strings.get(x).to_string())),
                Value::Text(x) => (Some("str"), Some(escape_text(&xstring(x)))),
                Value::Bool(b) => (Some("b"), Some(if *b { "1" } else { "0" }.to_string())),
                Value::Error(e) => (Some("e"), Some(e.to_string())),
                Value::Blank => (None, None),
                _ => (Some("e"), Some("#CALC!".to_string())),
            };
            if let Some(t) = t {
                body.push_str(&format!(" t=\"{t}\""));
            }
            if cm.contains(&(cr, cc)) {
                body.push_str(&format!(" cm=\"{dyn_cm}\""));
            }
            if f.is_none() && v.is_none() {
                body.push_str("/>");
                continue;
            }
            body.push('>');
            if let Some(f) = f {
                body.push_str(f);
            }
            if let Some(v) = v {
                let space = v.starts_with(char::is_whitespace) || v.ends_with(char::is_whitespace);
                let attr = if space { " xml:space=\"preserve\"" } else { "" };
                body.push_str(&format!("<v{attr}>{v}</v>"));
            }
            body.push_str("</c>");
        }
        if body.is_empty() && tag.attrs.len() == 1 {
            continue;
        }
        if body.is_empty() {
            s.push_str(&tag.empty());
        } else {
            s.push_str(&tag.open());
            s.push_str(&body);
            s.push_str("</row>");
        }
    }
    s.push_str("</sheetData>");
    (s, bounds.map_or("A1".into(), |b| rect_text(&b)))
}

fn table_xml(t: &xlcalc::Table, id: u32, base: Option<&[u8]>) -> Result<Vec<u8>, String> {
    let rect = rect_text(&t.rect);
    let mut split = match base {
        Some(b) => rawxml::split(b)?,
        None => {
            let mut s = rawxml::split(
                format!(
                    "<table xmlns=\"{NS_MAIN}\"><tableStyleInfo name=\"TableStyleMedium2\" showFirstColumn=\"0\" showLastColumn=\"0\" showRowStripes=\"1\" showColumnStripes=\"0\"/></table>"
                )
                .as_bytes(),
            )?;
            s.root.set("id", Some(&id.to_string()));
            s
        }
    };
    let src_cols: Vec<(String, String)> = split
        .kid("tableColumns")
        .and_then(|k| rawxml::split(k.raw.as_bytes()).ok())
        .map(|s| {
            s.kids
                .into_iter()
                .filter_map(|k| {
                    let name = Tag::parse(&k.raw).ok()?.value("name").unwrap_or_default();
                    Some((crate::ftext::unescape_xstring(&name), k.raw))
                })
                .collect()
        })
        .unwrap_or_default();
    let header = if t.headers { None } else { Some("0") };
    let totals = if t.totals { Some("1") } else { None };
    let display = display_name(&t.name);
    let unchanged = base.is_some()
        && split.root.get("ref") == Some(rect.as_str())
        && split.root.get("headerRowCount") == header
        && split.root.get("totalsRowCount") == totals
        && split.root.value("displayName").as_deref() == Some(display.as_str())
        && src_cols.len() == t.columns.len()
        && src_cols.iter().zip(&t.columns).all(|(a, b)| a.0 == *b);
    if unchanged {
        return Ok(base.unwrap_or_default().to_vec());
    }
    split
        .root
        .set("name", Some(&t.name))
        .set("displayName", Some(&display))
        .set("ref", Some(&rect))
        .set("headerRowCount", header)
        .set("totalsRowCount", totals);
    if !t.totals && split.root.get("totalsRowShown").is_none() {
        split.root.set("totalsRowShown", Some("0"));
    }
    let mut filter_rect = t.rect;
    if t.totals {
        filter_rect.r1 -= 1;
    }
    let af = if t.headers {
        Some(match split.kid("autoFilter") {
            Some(k) => {
                let mut kk = rawxml::split(k.raw.as_bytes())?;
                kk.root.set("ref", Some(&rect_text(&filter_rect)));
                let body: String = kk.kids.iter().map(|x| x.raw.as_str()).collect();
                if body.is_empty() {
                    kk.root.empty()
                } else {
                    format!("{}{body}</{}>", kk.root.open(), kk.root.name)
                }
            }
            None => format!("<autoFilter ref=\"{}\"/>", rect_text(&filter_rect)),
        })
    } else {
        None
    };
    split.put("autoFilter", af, TABLE_ORDER);
    let mut next_id = src_cols
        .iter()
        .filter_map(|(_, raw)| Tag::parse(raw).ok()?.get("id")?.parse::<u32>().ok())
        .max()
        .unwrap_or(0);
    let mut cols = String::new();
    for name in &t.columns {
        let found = src_cols
            .iter()
            .find(|(n, _)| n.eq_ignore_ascii_case(name))
            .map(|(_, raw)| raw.clone());
        let attr = escape_attr(&xstring(name));
        match found {
            Some(raw) => {
                let head_end = rawxml::head_len(&raw).unwrap_or(raw.len());
                let mut tag = Tag::parse(&raw)?;
                tag.attrs
                    .iter_mut()
                    .filter(|(k, _)| k == "name")
                    .for_each(|(_, v)| *v = attr.clone());
                cols.push_str(&tag.write());
                cols.push_str(&raw[head_end..]);
            }
            None => {
                next_id += 1;
                cols.push_str(&format!("<tableColumn id=\"{next_id}\" name=\"{attr}\"/>"));
            }
        }
    }
    split.put(
        "tableColumns",
        Some(format!(
            "<tableColumns count=\"{}\">{cols}</tableColumns>",
            t.columns.len()
        )),
        TABLE_ORDER,
    );
    Ok(split.write())
}

fn sheet_pr(raw: Option<&str>, code_name: &str) -> Result<String, String> {
    let (mut tag, rest) = match raw {
        Some(r) => {
            let s = rawxml::split(r.as_bytes())?;
            let body: String = s.kids.iter().map(|k| k.raw.as_str()).collect();
            (s.root, body)
        }
        None => (Tag::new("sheetPr"), String::new()),
    };
    tag.set("codeName", Some(code_name));
    Ok(if rest.is_empty() {
        tag.empty()
    } else {
        format!("{}{rest}</{}>", tag.open(), tag.name)
    })
}

fn workbook_pr(
    wb: &Workbook,
    raw: Option<&str>,
    code_name: Option<&str>,
) -> Result<String, String> {
    let (mut tag, rest) = match raw {
        Some(r) => {
            let s = rawxml::split(r.as_bytes())?;
            let body: String = s.kids.iter().map(|k| k.raw.as_str()).collect();
            (s.root, body)
        }
        None => (Tag::new("workbookPr"), String::new()),
    };
    let d1904 = matches!(wb.settings().date_system, xlcalc::DateSystem::D1904);
    tag.set("date1904", d1904.then_some("1"));
    if code_name.is_some() {
        tag.set("codeName", code_name);
    }
    Ok(if rest.is_empty() {
        tag.empty()
    } else {
        format!("{}{rest}</{}>", tag.open(), tag.name)
    })
}

fn book_views(wb: &Workbook, src: &Package, raw: &str) -> Result<String, String> {
    let mut s = rawxml::split(raw.as_bytes())?;

    let tab = src.active_sheet(wb.active_sheet());
    let tab = match wb.sheet_state(tab) {
        Some(SheetState::Visible) => tab,
        _ => wb.active_sheet(),
    };
    let active = wb.sheet_position(tab).unwrap_or(0);
    let n = wb.sheet_count();
    let mut out = s.root.open();
    for k in &mut s.kids {
        if k.name != "workbookView" {
            out.push_str(&k.raw);
            continue;
        }
        let v = rawxml::split(k.raw.as_bytes())?;
        let mut t = v.root;
        let s_active = (active != 0).then(|| active.to_string());
        t.set("activeTab", s_active.as_deref());
        if t.get("firstSheet")
            .and_then(|f| f.parse::<usize>().ok())
            .is_some_and(|f| f >= n)
        {
            t.set("firstSheet", None);
        }
        let body: String = v.kids.iter().map(|x| x.raw.as_str()).collect();
        if body.is_empty() {
            out.push_str(&t.empty());
        } else {
            out.push_str(&format!("{}{body}</{}>", t.open(), t.name));
        }
    }
    out.push_str(&format!("</{}>", s.root.name));
    Ok(out)
}

fn defined_names(
    wb: &Workbook,
    src: &Package,
    raw: Option<&str>,
) -> Result<Option<String>, String> {
    let mut source: Vec<(String, Option<SheetId>, Tag, String)> = Vec::new();
    if let Some(r) = raw {
        for k in rawxml::split(r.as_bytes())?.kids {
            let s = rawxml::split(k.raw.as_bytes())?;
            let text: String = s.kids.iter().map(|x| x.raw.as_str()).collect();
            let text = if text.is_empty() {
                let open = k.raw.find('>').map_or(0, |i| i + 1);
                let close = k.raw.rfind("</").unwrap_or(k.raw.len());
                if open <= close {
                    k.raw[open..close].to_string()
                } else {
                    String::new()
                }
            } else {
                text
            };
            let scope = s
                .root
                .get("localSheetId")
                .and_then(|i| i.parse::<usize>().ok())
                .map(|i| src.sheets.get(i).map(|x| x.id));
            let scope = match scope {
                Some(None) => continue,
                Some(Some(id)) => Some(id),
                None => None,
            };
            let name = s.root.value("name").unwrap_or_default();
            source.push((name.to_lowercase(), scope, s.root, text));
        }
    }
    let mut items: Vec<(String, String)> = Vec::new();
    let local = |scope: Option<SheetId>| scope.and_then(|id| wb.sheet_position(id));
    for d in wb.names().iter() {
        if d.name.starts_with(crate::ftext::ALIAS) {
            continue;
        }
        if d.scope.is_some() && local(d.scope).is_none() {
            continue;
        }
        let mut tag = source
            .iter()
            .find(|(n, s, _, _)| *n == d.name.to_lowercase() && *s == d.scope)
            .map(|x| x.2.clone())
            .unwrap_or_else(|| Tag::new("definedName"));
        tag.empty = false;
        tag.set("name", Some(&d.name));
        let ls = local(d.scope).map(|i| i.to_string());
        tag.set("localSheetId", ls.as_deref());
        let text = formula_xml(&wb.file_name_formula(d));
        items.push((
            d.name.to_lowercase(),
            format!("{}{text}</definedName>", tag.open()),
        ));
    }
    for (n, scope, tag, text) in &source {
        if !n.starts_with("_xlnm.") || items.iter().any(|(m, _)| m == n && scope.is_none()) {
            continue;
        }
        if scope.is_some() && local(*scope).is_none() {
            continue;
        }
        let mut tag = tag.clone();
        tag.empty = false;
        let ls = local(*scope).map(|i| i.to_string());
        tag.set("localSheetId", ls.as_deref());
        items.push((n.clone(), format!("{}{text}</definedName>", tag.open())));
    }
    if items.is_empty() {
        return Ok(None);
    }
    items.sort_by(|a, b| a.0.cmp(&b.0));
    Ok(Some(format!(
        "<definedNames>{}</definedNames>",
        items.into_iter().map(|x| x.1).collect::<String>()
    )))
}

fn calc_pr(wb: &Workbook, raw: Option<&str>) -> Result<String, String> {
    let mut t = match raw {
        Some(r) => rawxml::split(r.as_bytes())?.root,
        None => Tag::new("calcPr"),
    };
    let s = wb.settings();
    let mode = if s.mode == CalcMode::Manual {
        Some("manual")
    } else if s.except_tables {
        Some("autoNoTable")
    } else {
        None
    };
    let count = s.max_iterations.to_string();
    let delta = number_text(s.max_change);
    t.set("calcId", Some(CALC_ID))
        .set("calcMode", mode)
        .set("fullCalcOnLoad", None)
        .set("iterate", s.iteration.then_some("1"))
        .set(
            "iterateCount",
            (s.max_iterations != 100).then_some(count.as_str()),
        )
        .set(
            "iterateDelta",
            (s.max_change != 0.001).then_some(delta.as_str()),
        )
        .set("fullPrecision", wb.precision_as_displayed().then_some("0"));
    Ok(t.empty())
}

fn app_xml(wb: &Workbook, src: Option<&[u8]>) -> Result<Vec<u8>, String> {
    let mut split = rawxml::split(src.unwrap_or(
        concat!(
            "<Properties xmlns=\"http://schemas.openxmlformats.org/officeDocument/2006/extended-properties\" ",
            "xmlns:vt=\"http://schemas.openxmlformats.org/officeDocument/2006/docPropsVTypes\">",
            "<Application>xlcalc</Application><DocSecurity>0</DocSecurity><ScaleCrop>false</ScaleCrop>",
            "<LinksUpToDate>false</LinksUpToDate><SharedDoc>false</SharedDoc>",
            "<HyperlinksChanged>false</HyperlinksChanged></Properties>"
        )
        .as_bytes(),
    ))?;
    let vt = split
        .root
        .prefix_of("http://schemas.openxmlformats.org/officeDocument/2006/docPropsVTypes")
        .unwrap_or_else(|| {
            split.root.set(
                "xmlns:vt",
                Some("http://schemas.openxmlformats.org/officeDocument/2006/docPropsVTypes"),
            );
            "vt".into()
        });
    let names: Vec<String> = wb.sheets().map(|s| s.name.clone()).collect();
    let order = [
        "Template",
        "Manager",
        "Company",
        "Pages",
        "Words",
        "Characters",
        "PresentationFormat",
        "Lines",
        "Paragraphs",
        "Slides",
        "Notes",
        "TotalTime",
        "HiddenSlides",
        "MMClips",
        "ScaleCrop",
        "HeadingPairs",
        "TitlesOfParts",
    ];

    split.put(
        "HeadingPairs",
        Some(format!(
            "<HeadingPairs><{vt}:vector size=\"2\" baseType=\"variant\"><{vt}:variant><{vt}:lpstr>Worksheets</{vt}:lpstr></{vt}:variant><{vt}:variant><{vt}:i4>{}</{vt}:i4></{vt}:variant></{vt}:vector></HeadingPairs>",
            names.len()
        )),
        &order,
    );
    let titles: String = names
        .iter()
        .map(|n| format!("<{vt}:lpstr>{}</{vt}:lpstr>", escape_text(n)))
        .collect();
    split.put(
        "TitlesOfParts",
        Some(format!(
            "<TitlesOfParts><{vt}:vector size=\"{}\" baseType=\"lpstr\">{titles}</{vt}:vector></TitlesOfParts>",
            names.len()
        )),
        &order,
    );
    Ok(split.write())
}

fn core_xml(created: Option<&str>) -> Vec<u8> {
    let mut s = String::from(rawxml::DECL);
    s.push_str(concat!(
        "<cp:coreProperties xmlns:cp=\"http://schemas.openxmlformats.org/package/2006/metadata/core-properties\" ",
        "xmlns:dc=\"http://purl.org/dc/elements/1.1/\" xmlns:dcterms=\"http://purl.org/dc/terms/\" ",
        "xmlns:dcmitype=\"http://purl.org/dc/dcmitype/\" xmlns:xsi=\"http://www.w3.org/2001/XMLSchema-instance\">"
    ));
    if let Some(t) = created {
        let t = escape_text(t);
        s.push_str(&format!(
            "<dcterms:created xsi:type=\"dcterms:W3CDTF\">{t}</dcterms:created><dcterms:modified xsi:type=\"dcterms:W3CDTF\">{t}</dcterms:modified>"
        ));
    }
    s.push_str("</cp:coreProperties>");
    s.into_bytes()
}

#[cfg(test)]
mod tests {
    use super::number_text;

    #[test]
    fn numbers() {
        assert_eq!(number_text(0.085), "0.085");
        assert_eq!(number_text(1e-7), "1E-7");
        assert_eq!(number_text(1e21), "1E21");
        assert_eq!(number_text(123456.0), "123456");
        assert_eq!(number_text(-0.0), "-0");
        for x in [
            0.1 + 0.2,
            1.0 / 3.0,
            5e-324,
            f64::MAX,
            2.0f64.powi(60) + 1.0,
        ] {
            assert_eq!(
                number_text(x).parse::<f64>().unwrap().to_bits(),
                x.to_bits()
            );
        }
    }
}

pub(crate) fn display_name(name: &str) -> String {
    if is_cell_like(name) {
        format!("{name}_")
    } else {
        name.to_string()
    }
}

pub(crate) fn is_cell_like(name: &str) -> bool {
    let u = name.to_ascii_uppercase();
    let letters = u.bytes().take_while(u8::is_ascii_alphabetic).count();
    let digits = &u[letters..];
    if (1..=3).contains(&letters)
        && !digits.is_empty()
        && digits.bytes().all(|b| b.is_ascii_digit())
    {
        let col = u[..letters]
            .bytes()
            .fold(0u32, |c, b| c * 26 + u32::from(b - b'A' + 1));
        let row: u64 = digits.parse().unwrap_or(0);
        if col <= 16384 && (1..=1_048_576).contains(&row) {
            return true;
        }
    }

    let rc = |s: &str, k: char| -> Option<usize> {
        let rest = s.strip_prefix(k)?;
        Some(rest.bytes().take_while(u8::is_ascii_digit).count() + 1)
    };
    let mut at = 0;
    if let Some(n) = rc(&u, 'R') {
        at = n;
    }
    if let Some(n) = rc(&u[at..], 'C') {
        at += n;
    }
    at > 0 && at == u.len()
}

#[cfg(test)]
mod display_name_tests {
    use super::{display_name, is_cell_like};

    #[test]
    fn cell_like_names_get_an_underscore() {
        for n in ["Tbl1", "a1", "XFD1048576", "R", "c", "R1C1", "r2", "C3"] {
            assert!(is_cell_like(n), "{n}");
        }
        for n in ["Table1", "XFE1", "A0", "A1048577", "Rate", "R1C1x", "Tbl"] {
            assert!(!is_cell_like(n), "{n}");
        }
        assert_eq!(display_name("Tbl1"), "Tbl1_");
        assert_eq!(display_name("Sales"), "Sales");
    }
}
