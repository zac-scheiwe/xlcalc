mod cfs;
mod check;
mod csv;
mod ctrlprops;
mod drawings;
mod drawings_out;
mod dvs;
mod ext_in;
mod ext_out;
mod extras;
mod ftext;
mod hyperlinks;
mod merges;
mod notesxml;
mod package;
mod pivot;
mod rawxml;
mod richerr;
mod scenarios;
mod serfmt_xml;
mod slicers_xml;
mod sparks;
mod styles;
mod write;
mod xls;
mod xlsb;
mod xlsb_cfdv;
mod xlsb_dxf;
mod xlsb_enc;
mod xlsb_filter;
mod xlsb_ftab;
mod xlsb_pivot;
mod xlsb_rec;
mod xlsb_sheet;
mod xlsb_spark;
mod xlsb_styles;
mod xlsb_write;
mod xml;

pub use check::{canonical_xml, check_package};

pub fn function_name(id: u16) -> Option<&'static str> {
    xlsb_ftab::ftab(id).map(|(name, _)| name)
}
pub use ctrlprops::{read_controls, Control, ControlKind};
pub use ext_in::FileLink;
pub use extras::extras_summary;
pub use package::{Package, Rel, SourceSheet};
pub use write::{save, FileKind, SaveOptions, Saved, VbaWrite};
pub use xls::xlsx_to_xls;
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

    pub ext_links: Vec<FileLink>,
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

fn read_notes(zip: &mut Zip, path: &str, sheet: &Node) -> Vec<notesxml::NoteDef> {
    let Ok(all) = rels(zip, path) else {
        return Vec::new();
    };
    let Some(comments) = all
        .iter()
        .find(|r| r.1.ends_with("/comments"))
        .and_then(|r| tree(zip, &r.2).ok().flatten())
    else {
        return Vec::new();
    };
    let vml_id = sheet.child("legacyDrawing").and_then(|l| l.attr("id"));
    let vml = vml_id
        .and_then(|id| all.iter().find(|r| r.0 == id))
        .and_then(|r| part(zip, &r.2))
        .map(|b| String::from_utf8_lossy(&b).into_owned());
    notesxml::parse(&comments, vml.as_deref())
}

fn external_targets(zip: &mut Zip, owner: &str) -> HashMap<String, String> {
    let Ok(Some(n)) = tree(zip, &rels_name(owner)) else {
        return HashMap::new();
    };
    n.children("Relationship")
        .filter(|r| r.attr("TargetMode") == Some("External"))
        .filter_map(|r| Some((r.attr("Id")?.to_string(), r.attr("Target")?.to_string())))
        .collect()
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

    chart: bool,

    macro_kind: u8,
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

    sparks: Option<xml::Node>,

    links: Vec<hyperlinks::LinkDef>,

    merged: Vec<xlcalc::Rect>,

    dvs: Vec<dvs::DvDef>,

    cfs: Vec<cfs::CfDef>,

    notes: Vec<notesxml::NoteDef>,
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
    sd.sparks = sparks::find(n);
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
    let mut warnings: Vec<String> = Vec::new();
    let bytes = if xls::is_cfb(&bytes) {
        let (converted, w) = xls::to_xlsx(&bytes)?;
        warnings = w;
        converted
    } else {
        bytes
    };
    let mut zip: Zip =
        zip::ZipArchive::new(std::io::Cursor::new(bytes)).map_err(|e| e.to_string())?;

    if xlsb::is_xlsb(&mut zip) {
        let (converted, w) = xlsb::to_xlsx(&mut zip)?;
        warnings.extend(w);
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
    let mut ext_links: Vec<FileLink> = Vec::new();
    if let Some(refs) = wbx.child("externalReferences") {
        for er in refs.children("externalReference") {
            let Some(part) = er
                .attr("id")
                .and_then(|id| by_id.get(id))
                .map(|p| p.to_string())
            else {
                continue;
            };
            let Some(root) = tree(&mut zip, &part)? else {
                continue;
            };
            let raw: Vec<(String, String, String)> = match tree(&mut zip, &rels_name(&part))? {
                Some(n) => n
                    .children("Relationship")
                    .map(|r| {
                        (
                            r.attr("Id").unwrap_or("").to_string(),
                            r.attr("Type").unwrap_or("").to_string(),
                            r.attr("Target").unwrap_or("").to_string(),
                        )
                    })
                    .collect(),
                None => Vec::new(),
            };
            if let Some(l) = ext_in::read_link(&part, &root, &raw) {
                ext_links.push(l);
            }
        }
    }
    let ext_keys: Vec<String> = ext_links.iter().map(FileLink::key).collect();

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
    let mut pivot_defs: Vec<pivot::PivotDef> = Vec::new();

    let mut table_ids: Vec<(u32, String)> = Vec::new();
    let mut slicer_parts: Vec<(usize, String)> = Vec::new();
    let mut table_filters: Vec<slicers_xml::TableValues> = Vec::new();
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
            let mut sd = read_sheet(date1904, &sx, &shared, &dynamic_cm, &mut warnings, &name);
            sd.links = hyperlinks::read(&sx, &external_targets(&mut zip, path));
            sd.merged = merges::read(&sx);
            sd.dvs = dvs::read(&sx);
            sd.cfs = cfs::read(&sx);
            sd.notes = read_notes(&mut zip, path, &sx);
            sd.chart = sx.name == "chartsheet";
            sd.macro_kind = match sx.name.as_str() {
                "macrosheet" => 1,
                "intlmacrosheet" => 2,
                _ => 0,
            };
            sheets.push(sd);
            for (_, ty, tp) in rels(&mut zip, path)? {
                if ty.ends_with("/slicer") || ty.ends_with("/timeline") {
                    slicer_parts.push((idx, tp));
                }
            }
            for (_, ty, tp) in rels(&mut zip, path)? {
                if !ty.ends_with("/pivotTable") {
                    continue;
                }
                let Some(table) = tree(&mut zip, &tp)? else {
                    continue;
                };
                let cp = rels(&mut zip, &tp)?
                    .into_iter()
                    .find(|r| r.1.ends_with("/pivotCacheDefinition"))
                    .map(|r| r.2);
                let Some(cp) = cp else { continue };
                let Some(cache) = tree(&mut zip, &cp)? else {
                    continue;
                };
                pivot_defs.push(pivot::PivotDef {
                    sheet: idx,
                    table,
                    cache,
                    cache_part: cp,
                });
            }
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
                table_filters.extend(slicers_xml::table_values(&t, &tname));
                if let Some(i) = t.attr("id").and_then(|i| i.parse().ok()) {
                    table_ids.push((i, tname.clone()));
                }
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

    let mut cache_defs: Vec<slicers_xml::CacheDef> = Vec::new();
    for sc in slicers_xml::all(&wbx, "slicerCache") {
        let Some(path) = sc.attr("id").and_then(|i| by_id.get(i)) else {
            continue;
        };
        if let Some(d) = tree(&mut zip, path)?
            .as_ref()
            .and_then(slicers_xml::read_cache)
        {
            cache_defs.push(d);
        }
    }
    for sc in slicers_xml::all(&wbx, "timelineCacheRef") {
        let Some(path) = sc.attr("id").and_then(|i| by_id.get(i)) else {
            continue;
        };
        if let Some(d) = tree(&mut zip, path)?
            .as_ref()
            .and_then(slicers_xml::read_timeline_cache)
        {
            cache_defs.push(d);
        }
    }
    let mut slicer_trees: Vec<(usize, Node)> = Vec::new();
    for (idx, p) in &slicer_parts {
        if let Some(n) = tree(&mut zip, p)? {
            slicer_trees.push((*idx, n));
        }
    }

    let mut wb = Workbook::empty();
    for fl in &ext_links {
        let l = wb.ext_link(&fl.key());
        wb.ext_set_link(l, fl.absolute.as_deref().unwrap_or(&fl.target), false);
        wb.ext_set_data(l, fl.sheets.clone());
        wb.ext_set_names(l, fl.names.clone());
    }
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
                    *t = ftext::file_meaning(
                        &ftext::prepare(&ftext::ext_index(t, &ext_keys), &mut aliases),
                        &table_names,
                    )
                }
            }
        }
    }
    let mut ids: Vec<SheetId> = Vec::new();
    let mut sheet_states: Vec<(SheetId, SheetState)> = Vec::new();
    for s in &sheets {
        let added = if s.chart {
            wb.add_chart_sheet_before(&s.name, None)
        } else if s.macro_kind > 0 {
            let intl = s.macro_kind == 2;
            wb.add_macro_sheet_before(&s.name, intl, None)
        } else {
            wb.add_sheet(&s.name)
        };
        ids.push(added.map_err(|e| format!("sheet {}: {e}", s.name))?);
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
            let name = match name.strip_prefix("_xlnm.") {
                Some(n @ ("Auto_Open" | "Auto_Close" | "Auto_Activate" | "Auto_Deactivate")) => n,
                Some(_) => continue,
                None => name,
            };
            let scope = d
                .attr("localSheetId")
                .and_then(|s| s.parse::<usize>().ok())
                .and_then(|i| ids.get(i).copied());
            let refers = format!(
                "={}",
                ftext::file_meaning(
                    &ftext::prepare(&ftext::ext_index(&d.text, &ext_keys), &mut aliases),
                    &table_names,
                )
            );
            if let Err(e) = wb.define_name(name, &refers, scope) {
                warnings.push(format!("name {name}: {e} ({refers})"));
            } else if matches!(d.attr("xlm"), Some("1") | Some("true")) {
                let function = matches!(d.attr("function"), Some("1") | Some("true"));
                wb.set_name_macro_type(
                    &name.to_uppercase(),
                    scope,
                    Some(if function { 1 } else { 2 }),
                );
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
        if let Some(sp) = &sd.sparks {
            sparks::read(&mut wb, id, sp);
        }
        if let Some(sc) = &sd.scenarios {
            wb.set_scenarios(id, sc.clone());
        }
        merges::apply(&mut wb, id, &sd.merged);
        dvs::apply(&mut wb, id, &sd.dvs);
        cfs::apply(&mut wb, id, &sd.cfs);
        hyperlinks::apply(&mut wb, id, &sd.links);
        notesxml::apply(&mut wb, id, &sd.notes);
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
                wb.set_number_format_stored(id, rect, &code);
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

    for (sd, &id) in sheets.iter().zip(&ids) {
        if sd.macro_kind > 0 {
            for (r, c, v) in &sd.cached {
                wb.xlm_store(CellAddr::new(id, *r, *c), v.clone());
            }
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
    pivot::adopt(&mut wb, &ids, &pivot_defs, &mut warnings);
    for s in &package.sheets {
        if let Some(d) = drawings::read_sheet_drawing(&wb, s.id, &package, &s.part) {
            wb.set_drawing(s.id, d);
        }
    }
    slicers_xml::adopt(
        &mut wb,
        &ids,
        &cache_defs,
        &slicer_trees,
        &table_ids,
        &pivot_defs,
        &table_filters,
    );
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
        ext_links,
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

    #[test]
    fn groups_are_written_as_group_shapes() {
        use xlcalc::drawing::ShapeKind;
        let mut wb = xlcalc::Workbook::empty();
        let s = wb.add_sheet("G").unwrap();
        let a = wb.add_shape(s, ShapeKind::Auto(1), "Rectangle", [10.0, 10.0, 50.0, 40.0]);
        let b = wb.add_shape(s, ShapeKind::Auto(9), "Oval", [100.0, 60.0, 50.0, 40.0]);
        wb.add_shape(s, ShapeKind::TextBox, "TextBox", [30.0, 120.0, 80.0, 30.0]);
        wb.group_shapes(s, &[a, b]).unwrap();
        let saved = crate::write::save(&wb, None, &crate::write::SaveOptions::default()).unwrap();
        let back = super::import_bytes(saved.bytes).unwrap();
        let s2 = back.workbook.sheet_by_name("G").unwrap().id;

        let d = back.workbook.drawing(s2).unwrap();
        assert_eq!(d.shapes.len(), 4);
        assert!(matches!(d.shapes[0].kind, ShapeKind::Group(_)));
        assert_eq!(d.shapes[1].parent, Some(d.shapes[0].id));
        assert_eq!(d.shapes[2].parent, Some(d.shapes[0].id));
        assert_eq!(d.shapes[3].kind, ShapeKind::TextBox);
        assert_eq!(d.shapes[3].parent, None);

        let id = d.shapes[2].id;
        let mut m = back.workbook;
        m.set_shape_rect(s2, id, (3_000_000, 1_000_000, 900_000, 400_000));
        let saved = crate::write::save(&m, None, &crate::write::SaveOptions::default()).unwrap();
        let again = super::import_bytes(saved.bytes).unwrap().workbook;
        let s3 = again.sheet_by_name("G").unwrap().id;
        let d3 = again.drawing(s3).unwrap();
        assert_eq!(d3.shapes.len(), 4);
        assert_eq!(
            again.shape_rect_emu(s3, &d3.shapes[2].pos),
            (3_000_000, 1_000_000, 900_000, 400_000)
        );
    }

    #[test]
    fn series_formatting_round_trip() {
        use xlcalc::drawing::{
            Chart, DataLabels, ErrorBars, Part, Series, SeriesFmt, ShapeKind, Trendline,
        };
        use xlcalc::{CellAddr, Rect, Workbook};
        let mut wb = Workbook::empty();
        let s = wb.add_sheet("Data").unwrap();
        for i in 0..3u32 {
            wb.set_input(CellAddr::new(s, i, 0), &(i + 1).to_string())
                .unwrap();
        }
        let c = wb.add_shape(s, ShapeKind::Chart, "Chart", [200.0, 20.0, 300.0, 200.0]);
        let mk = |t: i32, fmt: SeriesFmt| Series {
            name: Part::Text("S".into()),
            x: Part::None,
            values: Part::Refs(vec![(s, Rect::new(0, 0, 2, 0))]),
            chart_type: t,
            axis_group: 1,
            fmt,
        };
        let fmt = SeriesFmt {
            marker_style: Some(8),
            marker_size: Some(9),
            marker_bg: Some(255),
            marker_fg: Some(-2),
            line_rgb: Some(0x00FF00),
            border_rgb: Some(0x00FF00),
            line_weight: Some(2.25),
            line_visible: Some(true),
            dash: Some(4),
            smooth: Some(true),
            labels: Some(DataLabels {
                position: Some(0),
                ..DataLabels::default()
            }),
            point_labels: vec![2],
            trends: vec![Trendline {
                kind: -4132,
                order: 2,
                period: 2,
                forward: 1.5,
                backward: 0.0,
                name: Some("T".into()),
                intercept: None,
                display_equation: true,
                display_r2: false,
            }],
            error_bars: Some(ErrorBars::default()),
            ..SeriesFmt::default()
        };
        let mut ch = Chart::new();
        ch.chart_type = 65;
        ch.series.push(mk(65, fmt.clone()));
        wb.drawing_mut(s).get_mut(c).unwrap().chart = Some(ch);
        let saved = crate::write::save(&wb, None, &crate::write::SaveOptions::default()).unwrap();
        let back = super::import_bytes(saved.bytes).unwrap();
        let s2 = back.workbook.sheet_by_name("Data").unwrap().id;
        let got = &back.workbook.drawing(s2).unwrap().shapes[0]
            .chart
            .as_ref()
            .unwrap()
            .series[0]
            .fmt;
        assert_eq!(got, &fmt);
    }

    #[test]
    fn linked_pictures_keep_their_link() {
        use xlcalc::drawing::{Image, ShapeKind};
        let path = concat!(
            env!("CARGO_MANIFEST_DIR"),
            "/../../conformance/files/pics/a.png"
        )
        .replace('/', "\\");
        let png = include_bytes!("../../../conformance/files/pics/a.png").to_vec();
        let mut wb = xlcalc::Workbook::empty();
        let s = wb.add_sheet("P").unwrap();
        for (i, embedded) in [false, true].into_iter().enumerate() {
            let id = wb.add_shape(
                s,
                ShapeKind::Picture,
                "Picture",
                [10.0 + 40.0 * i as f64, 20.0, 30.0, 15.0],
            );
            wb.drawing_mut(s).get_mut(id).unwrap().image = Some(Image {
                ext: "png".into(),
                bytes: png.clone(),
                linked: true,
                source: path.clone(),
                embedded,
            });
        }
        let saved = crate::write::save(&wb, None, &crate::write::SaveOptions::default()).unwrap();
        let pkg = crate::Package::read(&saved.bytes).unwrap();
        let rels = String::from_utf8(
            pkg.part("xl/drawings/_rels/drawing1.xml.rels")
                .unwrap()
                .to_vec(),
        )
        .unwrap();
        assert!(rels.contains("TargetMode=\"External\""), "{rels}");
        assert!(rels.contains("file:///"), "{rels}");
        assert!(pkg.part("xl/media/image1.png").is_some());
        assert!(pkg.part("xl/media/image2.png").is_none());
        let back = super::import_bytes(saved.bytes).unwrap();
        let s2 = back.workbook.sheet_by_name("P").unwrap().id;
        let d = back.workbook.drawing(s2).unwrap();
        let imgs: Vec<_> = d.shapes.iter().map(|x| x.image.clone().unwrap()).collect();
        assert!(imgs.iter().all(|i| i.linked && i.source == path));
        assert_eq!(
            imgs.iter().map(|i| i.embedded).collect::<Vec<_>>(),
            vec![false, true]
        );
        assert!(imgs.iter().all(|i| i.bytes == png));
    }

    #[test]
    fn macro_sheet_round_trip() {
        use xlcalc::{CellAddr, SheetKind, Workbook};
        let mut wb = Workbook::empty();
        wb.add_sheet("Sheet1").unwrap();
        let m = wb.add_macro_sheet_before("Macro1", false, None).unwrap();
        wb.set_formula(CellAddr::new(m, 0, 0), "=RETURN(7)")
            .unwrap();
        wb.set_formula(CellAddr::new(m, 1, 0), "=SET.VALUE(B1,5)")
            .unwrap();
        wb.define_name("go", "=Macro1!$A$1", None).unwrap();
        wb.set_name_macro_type("GO", None, Some(2));
        wb.define_name("addm", "=Macro1!$A$1", None).unwrap();
        wb.set_name_macro_type("ADDM", None, Some(1));
        wb.define_name("Auto_Open", "=Macro1!$A$1", None).unwrap();
        let opts = crate::write::SaveOptions {
            kind: Some(crate::write::FileKind::Xlsm),
            ..Default::default()
        };
        let saved = crate::write::save(&wb, None, &opts).unwrap();

        let mut again = super::import_bytes(saved.bytes).unwrap().workbook;
        let d = again.sheet_by_name("Macro1").unwrap().id;
        again.xlm_store(CellAddr::new(d, 0, 0), xlcalc::Value::Bool(true));
        let saved = crate::write::save(&again, None, &opts).unwrap();
        let direct = super::import_bytes(saved.bytes.clone()).unwrap();
        assert!(matches!(
            direct.workbook.value(CellAddr::new(d, 0, 0)),
            xlcalc::Value::Bool(true)
        ));
        let (bin, warnings) = crate::xlsx_to_xlsb(&saved.bytes).unwrap();
        assert!(warnings.is_empty());
        let saved = super::import_bytes(bin).unwrap();
        let back = saved;
        let w2 = &back.workbook;
        assert!(back.warnings.is_empty(), "{:?}", back.warnings);
        let s = w2.sheet_by_name("Macro1").unwrap();
        assert_eq!(s.kind, SheetKind::Macro);
        assert_eq!(
            w2.formula_text(CellAddr::new(s.id, 1, 0)).as_deref(),
            Some("=SET.VALUE(B1,5)")
        );
        assert!(matches!(
            w2.value(CellAddr::new(s.id, 0, 0)),
            xlcalc::Value::Bool(true)
        ));
        assert_eq!(w2.names().macro_type("GO", None), Some(2));
        assert_eq!(w2.names().macro_type("ADDM", None), Some(1));
        assert!(w2.names().iter().any(|n| n.name == "Auto_Open"));
    }

    #[test]
    fn sparklines_round_trip() {
        use xlcalc::sparkline::{SparkGroup, Sparkline};
        let mut wb = xlcalc::Workbook::empty();
        let s = wb.add_sheet("S p").unwrap();
        let d = wb.add_sheet("D").unwrap();
        let id = wb.next_spark_id();
        let mut g = SparkGroup::new(id, 2, true);
        g.line_weight = 1.5;
        g.blanks = 2;
        g.points[2].visible = true;
        g.points[2].color = 255;
        g.min_type = 3;
        g.min_value = 2.0;
        g.series_color = 0x00FF00;
        for i in 0..2 {
            g.lines.push(Sparkline {
                row: i,
                col: 5,
                src_sheet: d,
                src: xlcalc::Rect::new(i, 0, i, 2),
            });
        }
        wb.spark_groups_mut(s).push(g.clone());
        let saved = crate::write::save(&wb, None, &crate::write::SaveOptions::default()).unwrap();
        let back = super::import_bytes(saved.bytes).unwrap();
        let s2 = back.workbook.sheet_by_name("S p").unwrap().id;
        let got = back.workbook.spark_groups(s2);
        assert_eq!(got.len(), 1);
        let d2 = back.workbook.sheet_by_name("D").unwrap().id;
        let mut want = g;
        want.id = got[0].id;
        for l in &mut want.lines {
            l.src_sheet = d2;
        }
        assert_eq!(got[0], want);
    }

    #[test]
    fn shapes_and_charts_round_trip() {
        use xlcalc::drawing::{Chart, Part, Series, ShapeKind};
        use xlcalc::{CellAddr, Rect, Workbook};
        let mut wb = Workbook::empty();
        let s = wb.add_sheet("Data").unwrap();
        for (i, v) in ["1", "2", "3"].iter().enumerate() {
            wb.set_input(CellAddr::new(s, i as u32, 0), v).unwrap();
        }
        let a = wb.add_shape(s, ShapeKind::Auto(9), "Oval", [10.0, 20.0, 100.0, 50.0]);
        wb.drawing_mut(s).get_mut(a).unwrap().text = "hi".into();
        let c = wb.add_shape(s, ShapeKind::Chart, "Chart", [200.0, 20.0, 300.0, 200.0]);
        let mut ch = Chart::new();
        ch.chart_type = 51;
        ch.has_title = true;
        ch.title = "T".into();
        ch.series.push(Series {
            name: Part::Text("S1".into()),
            x: Part::None,
            values: Part::Refs(vec![(s, Rect::new(0, 0, 2, 0))]),
            chart_type: 51,
            axis_group: 1,
            fmt: Default::default(),
        });
        wb.drawing_mut(s).get_mut(c).unwrap().chart = Some(ch.clone());
        let cs = wb.add_chart_sheet_before("CS", None).unwrap();
        let k = wb.add_shape(cs, ShapeKind::Chart, "Chart", [0.0, 0.0, 480.0, 288.0]);
        wb.drawing_mut(cs).get_mut(k).unwrap().chart = Some(ch);
        wb.drawing_mut(cs).get_mut(k).unwrap().pos = xlcalc::drawing::Pos::Abs {
            x: 0,
            y: 0,
            w: 6_096_000,
            h: 3_657_600,
        };
        let saved = crate::write::save(&wb, None, &crate::write::SaveOptions::default()).unwrap();
        let back = super::import_bytes(saved.bytes.clone()).unwrap();
        let w2 = &back.workbook;
        let s2 = w2.sheet_by_name("Data").unwrap().id;
        let d = w2.drawing(s2).unwrap();
        assert_eq!(d.shapes.len(), 2);
        assert_eq!(d.shapes[0].text, "hi");
        assert_eq!(d.shapes[0].kind, ShapeKind::Auto(9));
        let c2 = d.shapes[1].chart.as_ref().unwrap();
        assert_eq!(c2.chart_type, 51);
        assert_eq!(c2.title, "T");
        assert_eq!(
            c2.series[0].values,
            Part::Refs(vec![(s2, Rect::new(0, 0, 2, 0))])
        );
        let cs2 = w2.sheet_by_name("CS").unwrap();
        assert_eq!(cs2.kind, xlcalc::SheetKind::Chart);
        let cd = w2.drawing(cs2.id).unwrap();
        assert_eq!(cd.shapes.len(), 1);
        assert_eq!(cd.shapes[0].chart.as_ref().unwrap().series.len(), 1);

        let (bin, warns) = crate::xlsb_write::xlsx_to_xlsb(&saved.bytes).unwrap();
        assert!(warns.is_empty(), "{warns:?}");
        let b2 = super::import_bytes(bin).unwrap();
        let bs = b2.workbook.sheet_by_name("Data").unwrap().id;
        assert_eq!(b2.workbook.drawing(bs).unwrap().shapes.len(), 2);
        let bc = b2.workbook.sheet_by_name("CS").unwrap();
        assert_eq!(bc.kind, xlcalc::SheetKind::Chart);

        let again = crate::write::save(
            w2,
            Some(&back.package),
            &crate::write::SaveOptions::default(),
        )
        .unwrap();
        let p1 = crate::Package::read(&saved.bytes).unwrap();
        let p2 = crate::Package::read(&again.bytes).unwrap();
        for n in ["xl/drawings/drawing1.xml", "xl/charts/chart1.xml"] {
            assert_eq!(p1.part(n), p2.part(n), "{n}");
        }
    }

    #[test]
    fn picture_bytes_round_trip() {
        use xlcalc::drawing::{Image, ShapeKind};
        let png = include_bytes!("../../../conformance/files/pics/a.png").to_vec();
        let mut wb = xlcalc::Workbook::empty();
        let s = wb.add_sheet("P").unwrap();
        let id = wb.add_shape(s, ShapeKind::Picture, "Picture", [10.0, 20.0, 30.0, 15.0]);
        {
            let sh = wb.drawing_mut(s).get_mut(id).unwrap();
            sh.image = Some(Image {
                ext: "png".into(),
                bytes: png.clone(),
                linked: false,
                source: String::new(),
                embedded: true,
            });
            sh.lock_aspect = true;
            sh.alt_text = "logo".into();
        }
        let saved = crate::write::save(&wb, None, &crate::write::SaveOptions::default()).unwrap();
        let pkg = crate::Package::read(&saved.bytes).unwrap();
        assert_eq!(pkg.part("xl/media/image1.png"), Some(&png[..]));
        let back = super::import_bytes(saved.bytes.clone()).unwrap();
        let s2 = back.workbook.sheet_by_name("P").unwrap().id;
        let d = back.workbook.drawing(s2).unwrap();
        assert_eq!(d.shapes.len(), 1);
        let sh = &d.shapes[0];
        assert_eq!(sh.kind, ShapeKind::Picture);
        assert_eq!(sh.image.as_ref().map(|i| &i.bytes[..]), Some(&png[..]));
        assert!(sh.lock_aspect);
        assert_eq!(sh.alt_text, "logo");

        let (bin, _) = crate::xlsb_write::xlsx_to_xlsb(&saved.bytes).unwrap();
        let b2 = super::import_bytes(bin).unwrap();
        let bs = b2.workbook.sheet_by_name("P").unwrap().id;
        let bsh = &b2.workbook.drawing(bs).unwrap().shapes[0];
        assert_eq!(bsh.image.as_ref().map(|i| &i.bytes[..]), Some(&png[..]));

        let again = crate::write::save(
            &back.workbook,
            Some(&back.package),
            &crate::write::SaveOptions::default(),
        )
        .unwrap();
        let p2 = crate::Package::read(&again.bytes).unwrap();
        assert_eq!(
            pkg.part("xl/drawings/drawing1.xml"),
            p2.part("xl/drawings/drawing1.xml")
        );
        assert_eq!(p2.part("xl/media/image1.png"), Some(&png[..]));
    }

    #[test]
    fn moved_shape_keeps_its_style_and_extlst() {
        use std::io::{Read, Write};
        use xlcalc::drawing::ShapeKind;
        let mut wb = xlcalc::Workbook::empty();
        let s = wb.add_sheet("S").unwrap();
        wb.add_shape(s, ShapeKind::Auto(1), "Rectangle", [10.0, 10.0, 50.0, 40.0]);
        let saved = crate::write::save(&wb, None, &crate::write::SaveOptions::default()).unwrap();

        let mut zr = zip::ZipArchive::new(std::io::Cursor::new(saved.bytes.clone())).unwrap();
        let mut zw = zip::ZipWriter::new(std::io::Cursor::new(Vec::new()));
        for i in 0..zr.len() {
            let mut f = zr.by_index(i).unwrap();
            let name = f.name().unwrap().to_string();
            let mut data = Vec::new();
            f.read_to_end(&mut data).unwrap();
            if name == "xl/drawings/drawing1.xml" {
                let x = String::from_utf8(data).unwrap();
                let x = x.replace(
                    "</xdr:spPr>",
                    "<a:effectLst><a:outerShdw blurRad=\"50800\"/></a:effectLst><a:extLst><a:ext uri=\"{X}\"/></a:extLst></xdr:spPr><xdr:style><a:lnRef idx=\"2\"/></xdr:style>",
                );
                data = x.into_bytes();
            }
            zw.start_file(name, zip::write::SimpleFileOptions::default())
                .unwrap();
            zw.write_all(&data).unwrap();
        }
        let bytes = zw.finish().unwrap().into_inner();
        let mut back = super::import_bytes(bytes).unwrap();
        let s2 = back.workbook.sheet_by_name("S").unwrap().id;
        let id = back.workbook.drawing(s2).unwrap().shapes[0].id;
        back.workbook
            .set_shape_rect(s2, id, (1_270_000, 635_000, 635_000, 635_000));
        back.workbook.drawing_mut(s2).get_mut(id).unwrap().name = "Moved".into();
        let out = crate::write::save(
            &back.workbook,
            Some(&back.package),
            &crate::write::SaveOptions::default(),
        )
        .unwrap();
        let pkg = crate::Package::read(&out.bytes).unwrap();
        let x = String::from_utf8(pkg.part("xl/drawings/drawing1.xml").unwrap().to_vec()).unwrap();
        assert!(
            x.contains("outerShdw") && x.contains("{X}") && x.contains("lnRef"),
            "{x}"
        );
        assert!(x.contains("name=\"Moved\""), "{x}");
        assert!(x.contains("<a:off x=\"1270000\" y=\"635000\"/>"), "{x}");
    }
}
