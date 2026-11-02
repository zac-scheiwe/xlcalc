mod check;
mod csv;
mod ctrlprops;
mod ftext;
mod package;
mod rawxml;
mod scenarios;
mod styles;
mod write;
mod xlsb;
mod xlsb_enc;
mod xlsb_ftab;
mod xlsb_rec;
mod xlsb_styles;
mod xlsb_write;
mod xml;

pub use check::{canonical_xml, check_package};
pub use ctrlprops::{read_controls, Control, ControlKind};
pub use package::{Package, Rel, SourceSheet};
pub use write::{save, FileKind, SaveOptions, Saved, VbaWrite};
pub use xlsb_write::xlsx_to_xlsb;

use package::{rels_name, resolve};
use std::collections::HashMap;
use std::io::Read;
use xlcalc::style::Style;
use xlcalc::{CalcMode, CellAddr, DateSystem, Rect, SheetId, SheetState, Value, Workbook, XlError};
use xml::Node;

pub use csv::{export_csv, import_csv, import_csv_unsplit, save_as_csv, CsvImport, CsvSaved};

pub struct Import {
    pub workbook: Workbook,

    pub opened_uncalculated: bool,

    pub warnings: Vec<String>,

    pub vba: Option<VbaPart>,

    pub sheet_code_names: Vec<(String, Option<String>)>,

    pub package: Package,
}

#[derive(Debug)]
pub struct VbaPart {
    pub raw: Vec<u8>,

    pub project: Result<xlcalc_vbaproject::VbaProject, xlcalc_vbaproject::VbaError>,

    pub workbook_code_name: Option<String>,

    pub sheet_code_names: Vec<(String, Option<String>)>,
}

type Zip = zip::ZipArchive<std::io::Cursor<Vec<u8>>>;

fn part(zip: &mut Zip, name: &str) -> Option<Vec<u8>> {
    let mut f = zip.by_name(name.trim_start_matches('/')).ok()?;
    let mut v = Vec::new();
    f.read_to_end(&mut v).ok()?;
    Some(v)
}

fn tree(zip: &mut Zip, name: &str) -> Result<Option<Node>, String> {
    match part(zip, name) {
        Some(b) => xml::parse(&b).map(Some).map_err(|e| format!("{name}: {e}")),
        None => Ok(None),
    }
}

fn rels(zip: &mut Zip, owner: &str) -> Result<Vec<(String, String, String)>, String> {
    let dir = owner.rsplit_once('/').map_or("", |(d, _)| d).to_string();
    let Some(n) = tree(zip, &rels_name(owner))? else {
        return Ok(Vec::new());
    };
    Ok(n.children("Relationship")
        .map(|r| {
            (
                r.attr("Id").unwrap_or("").to_string(),
                r.attr("Type").unwrap_or("").to_string(),
                resolve(&dir, r.attr("Target").unwrap_or("")),
            )
        })
        .collect())
}

fn iso_serial(t: &str, date1904: bool) -> Option<f64> {
    let (d, tm) = match t.split_once('T') {
        Some((d, tm)) => (Some(d), Some(tm)),
        None if t.contains(':') => (None, Some(t)),
        None => (Some(t), None),
    };
    let mut days = 0.0;
    if let Some(d) = d {
        let mut it = d.split('-');
        let y: i64 = it.next()?.parse().ok()?;
        let m: i64 = it.next()?.parse().ok()?;
        let dd: i64 = it.next()?.parse().ok()?;

        let (yy, mm) = if m <= 2 { (y - 1, m + 9) } else { (y, m - 3) };
        let era = yy.div_euclid(400);
        let yoe = yy - era * 400;
        let doy = (153 * mm + 2) / 5 + dd - 1;
        let doe = yoe * 365 + yoe / 4 - yoe / 100 + doy;
        let unix = era * 146_097 + doe - 719_468;
        days = (unix + 25_569) as f64;
        if date1904 {
            days -= 1462.0;
        } else if (1.0..61.0).contains(&days) {
            days -= 1.0;
        }
    }
    if let Some(tm) = tm {
        let tm = tm.trim_end_matches('Z');
        let mut it = tm.split(':');
        let h: f64 = it.next()?.parse().ok()?;
        let mi: f64 = it.next().map_or(Some(0.0), |x| x.parse().ok())?;
        let se: f64 = it.next().map_or(Some(0.0), |x| x.parse().ok())?;
        days += (h * 3600.0 + mi * 60.0 + se) / 86400.0;
    }
    Some(days)
}

pub(crate) fn file_number(t: &str) -> Option<Value> {
    let x = t.trim().parse::<f64>().ok()?;
    Some(if x.is_infinite() {
        Value::Error(XlError::NULL)
    } else if x != 0.0 && x.abs() < f64::MIN_POSITIVE {
        Value::Number(0.0)
    } else {
        Value::Number(x)
    })
}

fn parse_cell(s: &str) -> Option<(u32, u32)> {
    let letters = s.chars().take_while(|c| c.is_ascii_alphabetic()).count();
    let col = xlcalc::letters_to_col(&s[..letters])?;
    let row: u32 = s[letters..].parse().ok()?;
    Some((row.checked_sub(1)?, col))
}

fn parse_rect(s: &str) -> Option<Rect> {
    let (a, b) = s.split_once(':').unwrap_or((s, s));
    let (r0, c0) = parse_cell(a)?;
    let (r1, c1) = parse_cell(b)?;
    Some(Rect::new(r0, c0, r1, c1))
}

pub(crate) fn si_text(n: &Node) -> String {
    let mut s = String::new();
    for k in &n.kids {
        match k.name.as_str() {
            "t" => s.push_str(&k.text),
            "r" => {
                if let Some(t) = k.child("t") {
                    s.push_str(&t.text);
                }
            }
            _ => {}
        }
    }
    ftext::unescape_xstring(&s)
}

#[derive(Clone)]
enum Form {
    Plain(String),

    Dynamic(String),

    Array(Rect, String),
}

type DataTableDef = (Rect, Option<(u32, u32)>, Option<(u32, u32)>);

#[derive(Default)]
struct SheetData {
    name: String,
    consts: Vec<(u32, u32, Value)>,
    forms: Vec<(u32, u32, Form)>,
    styles: Vec<(u32, u32, u32)>,
    hidden_rows: Vec<u32>,

    row_heights: Vec<(u32, u32)>,

    cols: Vec<(u32, u32, Option<u32>, bool, bool)>,

    cached: Vec<(u32, u32, Value)>,

    uncached_arrays: Vec<(u32, u32)>,

    data_tables: Vec<DataTableDef>,

    scenarios: Option<xlcalc::scenarios::SheetScenarios>,
}

struct TableDef {
    name: String,
    sheet: usize,
    rect: Rect,
    totals: bool,

    columns: Vec<String>,
    headers: bool,
}

fn read_sheet(
    date1904: bool,
    n: &Node,
    shared: &[String],
    dynamic_cm: &[bool],
    warnings: &mut Vec<String>,
    name: &str,
) -> SheetData {
    let mut sd = SheetData {
        name: name.to_string(),
        ..SheetData::default()
    };
    sd.scenarios = scenarios::read(n);
    if let Some(cols) = n.child("cols") {
        for c in cols.children("col") {
            let num = |k: &str| c.attr(k).and_then(|v| v.parse::<f64>().ok());
            let (Some(min), Some(max)) = (num("min"), num("max")) else {
                continue;
            };
            if min < 1.0 || max < min {
                continue;
            }
            let on = |k: &str| matches!(c.attr(k), Some("1" | "true"));
            sd.cols.push((
                min as u32 - 1,
                (max as u32).min(16_384) - 1,
                num("width").map(xlcalc::file_width_to_px),
                on("customWidth"),
                on("hidden"),
            ));
        }
    }
    let mut masters: HashMap<String, (u32, u32, String)> = HashMap::new();
    let mut array_rects: Vec<Rect> = Vec::new();
    let mut raw: Vec<(u32, u32, Value)> = Vec::new();
    let Some(data) = n.child("sheetData") else {
        return sd;
    };

    let mut row_ix: u32 = 0;
    for row in data.children("row") {
        row_ix = match row.attr("r") {
            Some(r) => match r.parse::<u32>() {
                Ok(r) if r > 0 => r,
                _ => continue,
            },
            None => row_ix + 1,
        };
        if row.attr("hidden") == Some("1") {
            sd.hidden_rows.push(row_ix - 1);
        }
        if matches!(row.attr("customHeight"), Some("1" | "true")) {
            if let Some(h) = row.attr("ht").and_then(|h| h.parse::<f64>().ok()) {
                if (0.0..=409.5).contains(&h) {
                    sd.row_heights.push((row_ix - 1, (h * 8.0).round() as u32));
                }
            }
        }
        let mut next_col: u32 = 0;
        for c in row.children("c") {
            let at = match c.attr("r") {
                Some(a) => parse_cell(a),
                None => Some((row_ix - 1, next_col)),
            };
            let Some((r, col)) = at else {
                continue;
            };
            next_col = col + 1;
            if let Some(s) = c.attr("s").and_then(|s| s.parse::<u32>().ok()) {
                if s != 0 {
                    sd.styles.push((r, col, s));
                }
            }
            let ty = c.attr("t").unwrap_or("n");
            let v = c.child("v").map(|v| v.text.as_str());
            let value = match (ty, v) {
                ("s", Some(v)) => v
                    .trim()
                    .parse::<usize>()
                    .ok()
                    .and_then(|i| shared.get(i))
                    .map(|s| Value::from(s.as_str())),
                ("inlineStr", _) => c.child("is").map(|i| Value::from(si_text(i).as_str())),
                ("str", Some(v)) => Some(Value::from(ftext::unescape_xstring(v).as_str())),
                ("b", Some(v)) => Some(Value::Bool(matches!(v.trim(), "1" | "true"))),
                ("e", Some(v)) => Some(Value::Error(
                    XlError::parse(v.trim()).unwrap_or(XlError::VALUE),
                )),
                ("d", Some(v)) => {
                    let serial = iso_serial(v.trim(), date1904);
                    if serial.is_none() {
                        warnings.push(format!(
                            "{name}: ISO date cell {} unreadable",
                            c.attr("r").unwrap_or("")
                        ));
                    }
                    serial.map(Value::Number)
                }
                (_, Some(v)) => file_number(v),
                _ => None,
            };
            if let Some(f) = c.child("f") {
                let at = f.attr("t").unwrap_or("normal");
                let body = f.text.clone();
                match at {
                    "shared" => {
                        let si = f.attr("si").unwrap_or("").to_string();
                        if !body.is_empty() {
                            masters.insert(si.clone(), (r, col, body.clone()));
                        }
                        let text = match masters.get(&si) {
                            Some((mr, mc, mt)) => ftext::shift_formula(
                                mt,
                                i64::from(r) - i64::from(*mr),
                                i64::from(col) - i64::from(*mc),
                            ),
                            None => {
                                warnings.push(format!("{name}: shared formula {si} has no master"));
                                continue;
                            }
                        };
                        if let Some(v) = value {
                            sd.cached.push((r, col, v));
                        }
                        sd.forms.push((r, col, Form::Plain(text)));
                    }
                    "array" => {
                        let rect = f
                            .attr("ref")
                            .and_then(parse_rect)
                            .unwrap_or(Rect::new(r, col, r, col));
                        let cm = c
                            .attr("cm")
                            .and_then(|s| s.parse::<usize>().ok())
                            .is_some_and(|i| dynamic_cm.get(i).copied().unwrap_or(false));
                        array_rects.push(rect);
                        if value.is_none() {
                            sd.uncached_arrays.push((r, col));
                        }
                        sd.forms.push((
                            r,
                            col,
                            if cm {
                                Form::Dynamic(body)
                            } else {
                                Form::Array(rect, body)
                            },
                        ));
                    }
                    "dataTable" => {
                        let rect = f.attr("ref").and_then(parse_rect);
                        let r1 = f.attr("r1").and_then(parse_cell);
                        let r2 = f.attr("r2").and_then(parse_cell);
                        let two = f.attr("dt2D") == Some("1");
                        let row_table = f.attr("dtr") == Some("1");
                        match (rect, r1) {
                            (Some(rect), Some(a)) => {
                                array_rects.push(rect);
                                let (ri, ci) = if two {
                                    (Some(a), r2)
                                } else if row_table {
                                    (Some(a), None)
                                } else {
                                    (None, Some(a))
                                };
                                sd.data_tables.push((rect, ri, ci));
                            }
                            _ => warnings.push(format!(
                                "{name}: data table at {} unreadable",
                                c.attr("r").unwrap_or("")
                            )),
                        }
                    }
                    _ => {
                        if let Some(v) = value {
                            sd.cached.push((r, col, v));
                        }
                        sd.forms.push((r, col, Form::Plain(body)));
                    }
                }
            } else if let Some(v) = value {
                raw.push((r, col, v));
            }
        }
    }

    for (r, c, v) in raw {
        if !array_rects.iter().any(|a| a.contains(r, c)) {
            sd.consts.push((r, c, v));
        }
    }
    sd
}

fn load(bytes: Vec<u8>) -> Result<Import, String> {
    let mut zip: Zip =
        zip::ZipArchive::new(std::io::Cursor::new(bytes)).map_err(|e| e.to_string())?;

    let mut warnings: Vec<String> = Vec::new();
    if xlsb::is_xlsb(&mut zip) {
        let (converted, w) = xlsb::to_xlsx(&mut zip)?;
        warnings = w;
        zip = zip::ZipArchive::new(std::io::Cursor::new(converted)).map_err(|e| e.to_string())?;
    }
    let bytes = zip.into_inner().into_inner();
    let mut package = Package::read(&bytes)?;
    let mut zip: Zip =
        zip::ZipArchive::new(std::io::Cursor::new(bytes)).map_err(|e| e.to_string())?;
    let wbx = tree(&mut zip, "xl/workbook.xml")?.ok_or("no xl/workbook.xml")?;
    let wb_rels = rels(&mut zip, "xl/workbook.xml")?;
    let by_id: HashMap<&str, &str> = wb_rels
        .iter()
        .map(|(i, _, t)| (i.as_str(), t.as_str()))
        .collect();

    let shared: Vec<String> = match wb_rels.iter().find(|r| r.1.ends_with("/sharedStrings")) {
        Some((_, _, p)) => tree(&mut zip, p)?
            .map(|n| n.children("si").map(si_text).collect())
            .unwrap_or_default(),
        None => Vec::new(),
    };
    let mut styles = match wb_rels.iter().find(|r| r.1.ends_with("/styles")) {
        Some((_, _, p)) => tree(&mut zip, p)?
            .map(|n| styles::Styles::read(&n))
            .unwrap_or_default(),
        None => styles::Styles::default(),
    };
    if let Some((_, _, p)) = wb_rels.iter().find(|r| r.1.ends_with("/theme")) {
        if let Some(n) = tree(&mut zip, p)? {
            styles.read_theme(&n);
        }
    }
    warnings.append(&mut styles.warnings);

    let mut dynamic_cm: Vec<bool> = vec![false];
    if let Some((_, _, p)) = wb_rels.iter().find(|r| r.1.ends_with("/sheetMetadata")) {
        if let Some(m) = tree(&mut zip, p)? {
            let types: Vec<bool> = m
                .child("metadataTypes")
                .map(|t| {
                    t.children("metadataType")
                        .map(|x| x.attr("name") == Some("XLDAPR"))
                        .collect()
                })
                .unwrap_or_default();
            if let Some(cm) = m.child("cellMetadata") {
                for bk in cm.children("bk") {
                    let dynamic = bk.child("rc").is_some_and(|rc| {
                        rc.attr("t")
                            .and_then(|t| t.parse::<usize>().ok())
                            .and_then(|t| types.get(t.wrapping_sub(1)))
                            .copied()
                            .unwrap_or(false)
                    });
                    dynamic_cm.push(dynamic);
                }
            }
        }
    }

    let wbpr = wbx.child("workbookPr");
    let date1904 = wbpr.is_some_and(|p| matches!(p.attr("date1904"), Some("1") | Some("true")));
    let calc = wbx.child("calcPr");

    let mut sheet_code_names: Vec<(String, Option<String>)> = Vec::new();
    let mut sheets: Vec<SheetData> = Vec::new();

    let mut sources: Vec<(String, String, u32, Option<String>)> = Vec::new();
    let mut tables: Vec<TableDef> = Vec::new();
    if let Some(ss) = wbx.child("sheets") {
        for s in ss.children("sheet") {
            let name = s.attr("name").unwrap_or("Sheet").to_string();
            let Some(path) = s.attr("id").and_then(|i| by_id.get(i)) else {
                warnings.push(format!("sheet {name}: no part"));
                continue;
            };
            let Some(sx) = tree(&mut zip, path)? else {
                warnings.push(format!("sheet {name}: missing part {path}"));
                continue;
            };
            sheet_code_names.push((
                name.clone(),
                sx.child("sheetPr")
                    .and_then(|p| p.attr("codeName"))
                    .map(str::to_string),
            ));
            let idx = sheets.len();
            sources.push((
                path.to_string(),
                s.attr("id").unwrap_or("").to_string(),
                s.attr("sheetId").and_then(|i| i.parse().ok()).unwrap_or(0),
                s.attr("state")
                    .filter(|st| *st != "visible")
                    .map(str::to_string),
            ));
            sheets.push(read_sheet(
                date1904,
                &sx,
                &shared,
                &dynamic_cm,
                &mut warnings,
                &name,
            ));
            for (_, ty, tp) in rels(&mut zip, path)? {
                if !ty.ends_with("/table") {
                    continue;
                }
                let Some(t) = tree(&mut zip, &tp)? else {
                    continue;
                };
                let Some(rect) = t.attr("ref").and_then(parse_rect) else {
                    continue;
                };

                let tname = match (t.attr("displayName"), t.attr("name")) {
                    (Some(d), Some(n)) if write::is_cell_like(n) && d == write::display_name(n) => {
                        n.to_string()
                    }
                    (d, n) => d.or(n).unwrap_or("Table").to_string(),
                };
                let columns: Vec<String> = t
                    .child("tableColumns")
                    .map(|c| {
                        c.children("tableColumn")
                            .map(|x| x.attr("name").unwrap_or("").to_string())
                            .collect()
                    })
                    .unwrap_or_default();
                tables.push(TableDef {
                    columns,
                    headers: t.attr("headerRowCount") != Some("0"),
                    name: tname,
                    sheet: idx,
                    rect,
                    totals: t.attr("totalsRowCount").is_some_and(|c| c != "0"),
                });
            }
        }
    }

    let mut wb = Workbook::empty();
    wb.set_mode(CalcMode::Manual);
    if date1904 {
        wb.set_date_system(DateSystem::D1904);
    }

    let mut aliases: std::collections::BTreeSet<String> = std::collections::BTreeSet::new();
    let table_names: Vec<String> = tables.iter().map(|t| t.name.clone()).collect();
    for sd in &mut sheets {
        for (_, _, f) in &mut sd.forms {
            match f {
                Form::Plain(t) | Form::Dynamic(t) | Form::Array(_, t) => {
                    *t = ftext::file_meaning(&ftext::prepare(t, &mut aliases), &table_names)
                }
            }
        }
    }
    let mut ids: Vec<SheetId> = Vec::new();
    let mut sheet_states: Vec<(SheetId, SheetState)> = Vec::new();
    for s in &sheets {
        ids.push(
            wb.add_sheet(&s.name)
                .map_err(|e| format!("sheet {}: {e}", s.name))?,
        );
    }
    for (&id, (part, rel_id, sheet_id, state)) in ids.iter().zip(sources) {
        let state = match state.as_deref() {
            Some("hidden") => SheetState::Hidden,
            Some("veryHidden") => SheetState::VeryHidden,
            _ => SheetState::Visible,
        };
        package.sheets.push(SourceSheet {
            id,
            part,
            rel_id,
            sheet_id,
        });
        sheet_states.push((id, state));
    }

    for (sd, &id) in sheets.iter().zip(&ids) {
        for (r, c, v) in &sd.consts {
            wb.set_value(CellAddr::new(id, *r, *c), v.clone());
        }
    }
    for t in &tables {
        let id = ids[t.sheet];
        let mut rect = t.rect;
        if t.totals {
            rect.r1 -= 1;
        }
        if !t.headers {
            let sd = &sheets[t.sheet];
            let free = rect.r0 > 0
                && t.columns.len() as u32 == rect.cols()
                && (rect.c0..=rect.c1).all(|c| {
                    !sd.consts
                        .iter()
                        .any(|(r, cc, _)| *r == rect.r0 - 1 && *cc == c)
                        && !sd
                            .forms
                            .iter()
                            .any(|(r, cc, _)| *r == rect.r0 - 1 && *cc == c)
                });
            if !free {
                warnings.push(format!(
                    "table {}: no header row and no free row above it, skipped",
                    t.name
                ));
                continue;
            }
            rect.r0 -= 1;
            for (i, n) in t.columns.iter().enumerate() {
                wb.set_value(
                    CellAddr::new(id, rect.r0, rect.c0 + i as u32),
                    Value::from(n.as_str()),
                );
            }
        }
        if let Err(e) = wb.add_table(&t.name, id, rect) {
            warnings.push(format!("table {}: {e}", t.name));
            continue;
        }
        if !t.headers {
            if let Err(e) = wb.set_table_headers(&t.name, false) {
                warnings.push(format!("table {}: hiding headers: {e}", t.name));
            }
        }
        if t.totals {
            wb.restore_table_totals(&t.name, t.rect);
        }
    }
    if let Some(dn) = wbx.child("definedNames") {
        for d in dn.children("definedName") {
            let name = d.attr("name").unwrap_or("");
            if name.starts_with("_xlnm.") {
                continue;
            }
            let scope = d
                .attr("localSheetId")
                .and_then(|s| s.parse::<usize>().ok())
                .and_then(|i| ids.get(i).copied());
            let refers = format!(
                "={}",
                ftext::file_meaning(&ftext::prepare(&d.text, &mut aliases), &table_names)
            );
            if let Err(e) = wb.define_name(name, &refers, scope) {
                warnings.push(format!("name {name}: {e} ({refers})"));
            }
        }
    }
    if !aliases.is_empty() {
        let keep = wb.active_sheet();
        if let Ok(scratch) = wb.add_sheet("xlcalc scratch") {
            if let Err(e) = wb.set_active_sheet(scratch) {
                warnings.push(format!("scratch sheet: {e}"));
            }
            for name in &aliases {
                let alias = format!("{}{name}", ftext::ALIAS);
                if let Err(e) = wb.define_name(&alias, &format!("={name}"), None) {
                    warnings.push(format!("name {alias}: {e}"));
                }
            }
            if let Err(e) = wb.set_active_sheet(keep) {
                warnings.push(format!("active sheet: {e}"));
            }
            if let Err(e) = wb.delete_sheet(scratch) {
                warnings.push(format!("scratch sheet: {e}"));
            }
        }
    }

    *wb.style_table_mut() = styles.table().clone();
    if let Some(base) = styles.base() {
        wb.set_base_style(base);
    }
    for (sd, &id) in sheets.iter().zip(&ids) {
        for (r, c, f) in &sd.forms {
            let addr = CellAddr::new(id, *r, *c);
            let res = match f {
                Form::Plain(t) => wb.set_legacy_input(addr, &format!("={t}")),
                Form::Dynamic(t) => wb.set_formula(addr, &format!("={t}")),
                Form::Array(rect, t) => wb.set_array_formula(id, *rect, &format!("={t}")),
            };
            if let Err(e) = res {
                warnings.push(format!(
                    "{}!{}{}: formula refused: {e}",
                    sd.name,
                    xlcalc::col_to_letters(*c),
                    r + 1
                ));
            }
        }
        for (rect, ri, ci) in &sd.data_tables {
            let whole = Rect::new(
                rect.r0.saturating_sub(1),
                rect.c0.saturating_sub(1),
                rect.r1,
                rect.c1,
            );
            let at = |x: &Option<(u32, u32)>| x.map(|(r, c)| CellAddr::new(id, r, c));
            if let Err(e) = wb.add_data_table(id, whole, at(ri), at(ci)) {
                warnings.push(format!("{}: data table: {e}", sd.name));
            }
        }
        if let Some(sc) = &sd.scenarios {
            wb.set_scenarios(id, sc.clone());
        }
        for &(r, h8) in &sd.row_heights {
            wb.set_row_h8(id, r, h8);
        }
        if !sd.hidden_rows.is_empty() {
            for &r in &sd.hidden_rows {
                let _ = wb.set_rows_hidden(id, r, 1, true);
            }
        }
        for &(c0, c1, px, hand, hidden) in &sd.cols {
            if let Some(px) = px.filter(|&p| p != wb.standard_px()) {
                wb.set_column_px(id, c0, c1, px, hand);
            }
            if hidden {
                wb.set_cols_hidden(id, c0, c1 - c0 + 1, true);
            }
        }
    }
    for (sd, &id) in sheets.iter().zip(&ids) {
        let mut cells = sd.styles.clone();
        cells.sort_unstable();
        let mut i = 0;
        while i < cells.len() {
            let (r, c, s) = cells[i];
            let mut j = i;
            while j + 1 < cells.len()
                && cells[j + 1].0 == r
                && cells[j + 1].1 == cells[j].1 + 1
                && cells[j + 1].2 == s
            {
                j += 1;
            }
            let rect = Rect::new(r, c, r, cells[j].1);

            if let Some(st) = styles.style_of(s) {
                if st.with_number_format("General") != *wb.base_style() {
                    wb.set_style_with(id, rect, &|x| Style {
                        number_format: x.number_format.clone(),
                        ..st.clone()
                    });
                }
            }
            if let Some(code) = styles.format_of(s) {
                wb.set_number_format(id, rect, &code);
            }
            i = j + 1;
        }

        for &(r, c, s) in &sd.styles {
            if styles.quote_prefix(s) {
                wb.mark_apostrophe(CellAddr::new(id, r, c));
            }
        }

        let styled: std::collections::HashSet<(u32, u32)> = sd
            .styles
            .iter()
            .filter(|(_, _, s)| styles.format_of(*s).is_some())
            .map(|&(r, c, _)| (r, c))
            .collect();
        for (r, c, _) in &sd.forms {
            let a = CellAddr::new(id, *r, *c);
            if !styled.contains(&(*r, *c)) && wb.number_format(a) != "General" {
                wb.set_number_format(id, Rect::new(*r, *c, *r, *c), "General");
            }
        }
    }
    if let Some(c) = calc {
        if c.attr("iterate") == Some("1") {
            let count = c
                .attr("iterateCount")
                .and_then(|s| s.parse().ok())
                .unwrap_or(100);
            let delta = c
                .attr("iterateDelta")
                .and_then(|s| s.parse().ok())
                .unwrap_or(0.001);
            wb.set_iteration(true, Some(count), Some(delta));
        }
        if c.attr("fullPrecision") == Some("0") {
            wb.set_precision_as_displayed(true);
        }
    }

    for (sd, &id) in sheets.iter().zip(&ids) {
        for (r, c, v) in &sd.cached {
            xlcalc::testing::seed_value(&mut wb, CellAddr::new(id, *r, *c), v.clone());
        }
    }
    let manual = calc.is_some_and(|c| c.attr("calcMode") == Some("manual"));
    let mut opened_uncalculated = false;
    wb.full_recalc();
    if manual {
        for (sd, &id) in sheets.iter().zip(&ids) {
            for (r, c, v) in &sd.cached {
                xlcalc::testing::restore_saved_value(&mut wb, CellAddr::new(id, *r, *c), v.clone());
            }
        }
    } else {
        wb.set_mode(CalcMode::Automatic);

        let current = calc
            .and_then(|c| c.attr("calcId"))
            .and_then(|s| s.parse::<u32>().ok())
            .is_some_and(|id| id >= 191029);
        let forced = calc.is_some_and(|c| matches!(c.attr("fullCalcOnLoad"), Some("1" | "true")));
        opened_uncalculated = current && !forced;
        if opened_uncalculated {
            let mut saved = Vec::new();
            let mut blank = Vec::new();
            let mut recalc = Vec::new();
            for (sd, &id) in sheets.iter().zip(&ids) {
                for (r, c, v) in &sd.cached {
                    saved.push((CellAddr::new(id, *r, *c), v.clone()));
                }
                for (r, c) in &sd.uncached_arrays {
                    blank.push(CellAddr::new(id, *r, *c));
                }

                for (r, c, f) in &sd.forms {
                    if matches!(f, Form::Plain(t) if t.contains("]:[")) {
                        recalc.push(CellAddr::new(id, *r, *c));
                    }
                }
            }
            xlcalc::testing::open_like_excel(&mut wb, &saved, &blank, &recalc);
        }
    }
    let tab = wbx
        .child("bookViews")
        .and_then(|b| b.child("workbookView"))
        .and_then(|v| v.attr("activeTab"))
        .and_then(|t| t.parse::<usize>().ok())
        .unwrap_or(0);

    for &(id, state) in &sheet_states {
        if ids.get(tab) == Some(&id) && state != SheetState::Visible {
            continue;
        }
        wb.set_sheet_state(id, state)
            .map_err(|e| format!("sheet state: {e}"))?;
    }
    if let Some(&tab) = ids.get(tab) {
        package.active = Some((wb.active_sheet(), tab));
    }
    let vba = part(&mut zip, "xl/vbaProject.bin").map(|raw| VbaPart {
        project: xlcalc_vbaproject::VbaProject::parse(raw.clone()),
        raw,
        workbook_code_name: wbpr.and_then(|p| p.attr("codeName")).map(str::to_string),
        sheet_code_names: sheet_code_names.clone(),
    });
    Ok(Import {
        opened_uncalculated,
        workbook: wb,
        warnings,
        vba,
        sheet_code_names,
        package,
    })
}

pub fn display_formula(text: &str, book: &str) -> String {
    text.replace(ftext::ALIAS, &format!("{book}!"))
        .replace(ftext::UDF, "")
        .replace(ftext::BARE_TABLE, "")
}

pub fn import_file(path: &std::path::Path) -> Result<Import, String> {
    let bytes = std::fs::read(path).map_err(|e| format!("{}: {e}", path.display()))?;
    load(bytes)
}

pub fn import_bytes(bytes: Vec<u8>) -> Result<Import, String> {
    load(bytes)
}

#[cfg(test)]
mod tests {
    use super::{file_number, iso_serial};
    use xlcalc::{Value, XlError};

    #[test]
    fn file_numbers() {
        let n = |t: &str| match file_number(t) {
            Some(Value::Number(x)) => Some(x.to_bits()),
            Some(Value::Error(e)) if e == XlError::NULL => Some(u64::MAX),
            _ => None,
        };
        assert_eq!(n(" 2.5 "), Some(2.5f64.to_bits()));
        assert_eq!(n("1.797693134862316e+308"), Some(u64::MAX));
        assert_eq!(n("2.225073858507201e-308"), Some(0));
        assert_eq!(
            n("2.2250738585072014e-308"),
            Some(f64::MIN_POSITIVE.to_bits())
        );
        assert_eq!(n("x"), None);
    }

    #[test]
    fn iso_dates() {
        assert_eq!(iso_serial("2026-10-30", false), Some(46_325.0));
        assert_eq!(iso_serial("1900-03-01", false), Some(61.0));
        assert_eq!(iso_serial("1900-02-28", false), Some(59.0));
        assert_eq!(iso_serial("1900-01-01", false), Some(1.0));
        assert_eq!(iso_serial("2026-10-30", true), Some(46_325.0 - 1462.0));
        assert_eq!(iso_serial("2026-10-30T12:00:00", false), Some(46_325.5));
        assert_eq!(iso_serial("06:00:00", false), Some(0.25));
        assert_eq!(iso_serial("nonsense", false), None);
    }
}
