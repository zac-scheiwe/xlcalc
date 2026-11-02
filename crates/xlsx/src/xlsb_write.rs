use crate::package::{dir_of, resolve, Package};
use crate::xlsb_enc::{self as enc, Cls, NameEnt, State, TableEnt};
use crate::xlsb_rec::{Recs, P};
use crate::xml::{self, Node};
use crate::{parse_cell, parse_rect, si_text};
use std::io::Write as _;

const REL: &str = "http://schemas.openxmlformats.org/officeDocument/2006/relationships";
const PKG_REL: &str = "http://schemas.openxmlformats.org/package/2006/relationships";

struct SheetIn {
    name: String,
    sheet_id: u32,
    state: u32,
    node: Node,

    tables: Vec<usize>,
}

struct TableIn {
    node: Node,
    ent: TableEnt,
}

fn rd_xml(pkg: &Package, name: &str) -> Result<Node, String> {
    let b = pkg
        .part(name)
        .ok_or_else(|| format!("missing part {name}"))?;
    xml::parse(b).map_err(|e| format!("{name}: {e}"))
}

fn num(n: &Node, key: &str) -> Option<f64> {
    n.attr(key).and_then(|v| v.parse().ok())
}

fn flag(n: &Node, key: &str) -> bool {
    matches!(n.attr(key), Some("1" | "true"))
}

fn xf_ixfe(c: &Node) -> u32 {
    c.attr("s").and_then(|v| v.parse().ok()).unwrap_or(0)
}

enum V {
    None,
    Num(f64),
    Bool(bool),
    Err(u8),
    Str(String),
    Sst(u32),
}

fn cell_value(c: &Node, warnings: &mut Vec<String>) -> V {
    let v = c.child("v").map(|v| v.text.as_str());
    match c.attr("t") {
        Some("s") => v
            .and_then(|v| v.trim().parse().ok())
            .map_or(V::None, V::Sst),
        Some("str") => V::Str(v.unwrap_or("").to_string()),
        Some("inlineStr") => V::Str(c.child("is").map(si_text).unwrap_or_default()),
        Some("b") => V::Bool(v.is_some_and(|v| v.trim() == "1")),
        Some("e") => match v.and_then(|v| enc::error_byte(v.trim())) {
            Some(b) => V::Err(b),
            None => {
                warnings.push(format!(
                    "cell {}: error value {:?} not carried",
                    c.attr("r").unwrap_or("?"),
                    v
                ));
                V::None
            }
        },
        _ => v
            .and_then(|v| v.trim().parse::<f64>().ok())
            .map_or(V::None, V::Num),
    }
}

struct Ctx<'a> {
    sheets: &'a [String],
    tables: &'a [TableEnt],
}

fn cell_head(p: &mut P, col: u32, xf: u32) {
    p.u32(col).u32(xf);
}

struct Cx<'a> {
    ctx: &'a Ctx<'a>,
    st: &'a mut State,
    sheet: u32,
    warnings: &'a mut Vec<String>,

    members: std::collections::HashMap<(u32, u32), (u8, u32, u32)>,
}

impl Cx<'_> {
    fn cover(&mut self, rc: crate::Rect, ptg: u8, row: u32, col: u32) {
        for r in rc.r0..=rc.r1 {
            for c in rc.c0..=rc.c1 {
                if (r, c) != (row, col) {
                    self.members.insert((r, c), (ptg, row, col));
                }
            }
        }
    }
}

fn write_cell(r: &mut Recs, cx: &mut Cx<'_>, c: &Node, row: u32, col: u32) {
    let (ctx, sheet) = (cx.ctx, cx.sheet);
    let xf = xf_ixfe(c);
    let val = cell_value(c, cx.warnings);
    let f = c.child("f");
    if let Some(cm) = c.attr("cm").and_then(|v| v.parse::<u32>().ok()) {
        r.with(49, |p| {
            p.u32(cm);
        });
    }
    let mut formula: Option<enc::Rgce> = None;
    let mut after: Option<(u32, Vec<u8>)> = None;
    if let Some(f) = f {
        let kind = f.attr("t").unwrap_or("normal");
        let text = f.text.trim();
        let here = format!("{}{}", xlcalc::col_to_letters(col), row + 1);
        match kind {
            "array" if !text.is_empty() => {
                match enc::encode(ctx.sheets, ctx.tables, cx.st, Some(sheet), text, Cls::Value) {
                    Ok(body) => {
                        let rect = f.attr("ref").and_then(parse_rect);
                        if let Some(rc) = rect {
                            let mut p = P::default();
                            p.u32(rc.r0).u32(rc.r1).u32(rc.c0).u32(rc.c1).u8(0x08);
                            p.raw(&body.parsed());
                            after = Some((426, p.0));
                            cx.cover(rc, 0x01, row, col);
                            let mut rgce = vec![0x01];
                            rgce.extend(row.to_le_bytes());
                            formula = Some(enc::Rgce {
                                rgce,
                                rgcb: col.to_le_bytes().to_vec(),
                            });
                        }
                    }
                    Err(e) => cx
                        .warnings
                        .push(format!("{here}: formula not carried: {e}")),
                }
            }
            "dataTable" => {
                let rect = f.attr("ref").and_then(parse_rect);
                let r1 = f.attr("r1").and_then(parse_cell);
                if let (Some(rc), Some(r1)) = (rect, r1) {
                    let r2 = f.attr("r2").and_then(parse_cell).unwrap_or((0, 0));
                    let mut fl = 0u8;
                    if flag(f, "dtr") {
                        fl |= 1;
                    }
                    if flag(f, "dt2D") {
                        fl |= 2;
                    }
                    let mut p = P::default();
                    p.u32(rc.r0).u32(rc.r1).u32(rc.c0).u32(rc.c1);
                    p.u32(r1.0).u32(r1.1).u32(r2.0).u32(r2.1).u8(fl);

                    r.rec(428, &p.0);
                }
            }
            "normal" if !text.is_empty() => {
                match enc::encode(ctx.sheets, ctx.tables, cx.st, Some(sheet), text, Cls::Value) {
                    Ok(x) => formula = Some(x),
                    Err(e) => cx
                        .warnings
                        .push(format!("{here}: formula not carried: {e}")),
                }
            }
            _ => {}
        }
    } else if let Some(&(ptg, r0, c0)) = cx.members.get(&(row, col)) {
        let mut rgce = vec![ptg];
        rgce.extend(r0.to_le_bytes());
        formula = Some(enc::Rgce {
            rgce,
            rgcb: c0.to_le_bytes().to_vec(),
        });
    }
    match (&val, formula) {
        (v, Some(fm)) => {
            let id = match v {
                V::Str(_) => 8,
                V::Bool(_) => 10,
                V::Err(_) => 11,
                _ => 9,
            };
            r.with(id, |p| {
                cell_head(p, col, xf);
                match v {
                    V::Str(s) => {
                        p.wstr(s);
                    }
                    V::Bool(b) => {
                        p.u8(u8::from(*b));
                    }
                    V::Err(e) => {
                        p.u8(*e);
                    }
                    V::Num(n) => {
                        p.f64(*n);
                    }
                    V::Sst(_) | V::None => {
                        p.f64(0.0);
                    }
                }
                p.u16(0).raw(&fm.parsed());
            });
        }
        (V::None, None) => r.with(1, |p| cell_head(p, col, xf)),
        (V::Num(n), None) => r.with(5, |p| {
            cell_head(p, col, xf);
            p.f64(*n);
        }),
        (V::Bool(b), None) => r.with(4, |p| {
            cell_head(p, col, xf);
            p.u8(u8::from(*b));
        }),
        (V::Err(e), None) => r.with(3, |p| {
            cell_head(p, col, xf);
            p.u8(*e);
        }),
        (V::Str(s), None) => r.with(6, |p| {
            cell_head(p, col, xf);
            p.wstr(s);
        }),
        (V::Sst(i), None) => r.with(7, |p| {
            cell_head(p, col, xf);
            p.u32(*i);
        }),
    }
    if let Some((id, data)) = after {
        r.rec(id, &data);
    }
}

struct SheetOut {
    bin: Vec<u8>,
}

fn write_sheet(cx: &mut Cx<'_>, sh: &SheetIn, table_rids: &[String]) -> SheetOut {
    let root = &sh.node;
    let mut r = Recs::default();
    let code_name = root
        .child("sheetPr")
        .and_then(|p| p.attr("codeName"))
        .map(str::to_string);
    r.empty(129);
    r.with(147, |p| {
        p.raw(&[
            0xC9, 0x04, 0x02, 0x00, 0x40, 0, 0, 0, 0, 0, 0, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF,
            0xFF, 0xFF,
        ])
        .wstr(code_name.as_deref().unwrap_or(""));
    });
    let dim = root
        .child("dimension")
        .and_then(|d| d.attr("ref"))
        .and_then(parse_rect);
    r.with(148, |p| {
        let (r0, r1, c0, c1) = dim.map_or((0, 0, 0, 0), |d| (d.r0, d.r1, d.c0, d.c1));
        p.u32(r0).u32(r1).u32(c0).u32(c1);
    });
    let view = root.child("sheetViews").and_then(|v| v.child("sheetView"));
    r.empty(133);
    r.with(137, |p| {
        let mut flags: u16 = 0x039C;
        if view.is_some_and(|v| flag(v, "tabSelected")) {
            flags |= 0x40;
        }
        if view.is_some_and(|v| v.attr("showGridLines") == Some("0")) {
            flags &= !0x04;
        }
        let zoom = view.and_then(|v| num(v, "zoomScale")).unwrap_or(100.0) as u16;
        p.u16(flags).raw(&[0; 12]).raw(&[0x40, 0, 0, 0]).u16(zoom);
        p.raw(&[0; 10]);
    });
    r.with(152, |p| {
        p.u32(3).u32(0).u32(0).u32(0).u32(1);
        p.u32(0).u32(0).u32(0).u32(0);
    });
    r.empty(138);
    r.empty(134);
    let def_h = root
        .child("sheetFormatPr")
        .and_then(|f| num(f, "defaultRowHeight"))
        .unwrap_or(15.0);
    r.with(485, |p| {
        p.u32(0xFFFF_FFFF)
            .u16(8)
            .u16((def_h * 20.0).round() as u16)
            .u32(0);
    });
    if let Some(cols) = root.child("cols") {
        let list: Vec<&Node> = cols.children("col").collect();
        if !list.is_empty() {
            r.empty(390);
            for c in list {
                let min = num(c, "min").unwrap_or(1.0) as u32 - 1;
                let max = num(c, "max").unwrap_or(1.0) as u32 - 1;
                let width = num(c, "width").unwrap_or(8.43);
                let mut fl = 0u16;
                if flag(c, "hidden") {
                    fl |= 1;
                }
                if flag(c, "customWidth") {
                    fl |= 2;
                }
                r.with(60, |p| {
                    p.u32(min)
                        .u32(max)
                        .u32((width * 256.0).round() as u32)
                        .u32(num(c, "style").unwrap_or(0.0) as u32)
                        .u16(fl);
                });
            }
            r.empty(391);
        }
    }
    r.empty(145);
    if let Some(data) = root.child("sheetData") {
        for row in data.children("row") {
            let rw = num(row, "r").map_or(0, |v| v as u32 - 1);
            let cells: Vec<(u32, &Node)> = row
                .children("c")
                .filter_map(|c| {
                    let (_, col) = parse_cell(c.attr("r")?)?;
                    Some((col, c))
                })
                .collect();
            let ht = num(row, "ht").unwrap_or(def_h);
            r.with(0, |p| {
                p.u32(rw).u32(0).u16((ht * 20.0).round() as u16);
                let custom = if flag(row, "customHeight") { 0x2000 } else { 0 };
                p.u16(custom | if flag(row, "hidden") { 0x1000 } else { 0 });
                p.u8(0);
                match (cells.first(), cells.last()) {
                    (Some(a), Some(b)) => {
                        p.u32(1).u32(a.0).u32(b.0);
                    }
                    _ => {
                        p.u32(0);
                    }
                }
            });
            for (col, c) in cells {
                write_cell(&mut r, cx, c, rw, col);
            }
        }
    }
    r.empty(146);
    if let Some(m) = root.child("pageMargins") {
        r.with(476, |p| {
            for k in ["left", "right", "top", "bottom", "header", "footer"] {
                p.f64(num(m, k).unwrap_or(0.7));
            }
        });
    }
    if !table_rids.is_empty() {
        r.with(660, |p| {
            p.u32(table_rids.len() as u32);
        });
        for id in table_rids {
            r.with(661, |p| {
                p.wstr(id);
            });
        }
        r.empty(662);
    }
    for k in [
        "mergeCells",
        "hyperlinks",
        "conditionalFormatting",
        "dataValidations",
        "drawing",
        "legacyDrawing",
    ] {
        if root.child(k).is_some() {
            cx.warnings
                .push(format!("sheet {}: {k} not carried", sh.name));
        }
    }
    r.empty(130);
    SheetOut { bin: r.bytes }
}

fn table_bin(t: &TableIn) -> Vec<u8> {
    let n = &t.node;
    let rc = n.attr("ref").and_then(parse_rect);
    let (r0, r1, c0, c1) = rc.map_or((0, 0, 0, 0), |d| (d.r0, d.r1, d.c0, d.c1));
    let headers = num(n, "headerRowCount").unwrap_or(1.0) as u32;
    let totals = num(n, "totalsRowCount").unwrap_or(0.0) as u32;
    let mut r = Recs::default();
    r.with(343, |p| {
        p.u32(r0).u32(r1).u32(c0).u32(c1);
        p.u32(0).u32(t.ent.id).u32(headers).u32(totals).u32(0);
        for _ in 0..6 {
            p.u32(0xFFFF_FFFF);
        }
        p.u32(0);
        p.wstr(&t.ent.names[0]).wstr(&t.ent.names[1]).wstr("");
        for _ in 0..3 {
            p.null_str();
        }
    });
    r.with(161, |p| {
        p.u32(r0).u32(r1).u32(c0).u32(c1);
    });
    r.empty(162);
    r.with(345, |p| {
        p.u32(t.ent.columns.len() as u32);
    });
    for (i, c) in t.ent.columns.iter().enumerate() {
        r.with(347, |p| {
            p.u32(i as u32 + 1).u32(0);
            p.u32(0xFFFF_FFFF).u32(0xFFFF_FFFF).u32(0xFFFF_FFFF);
            p.u32(0).u32(0xFFFF_FFFF);
            p.wstr(c);
            for _ in 0..4 {
                p.null_str();
            }
        });
        r.empty(348);
    }
    r.empty(346);
    if let Some(si) = n.child("tableStyleInfo") {
        let mut fl = 0u16;
        if flag(si, "showFirstColumn") {
            fl |= 1;
        }
        if flag(si, "showLastColumn") {
            fl |= 2;
        }
        if flag(si, "showRowStripes") {
            fl |= 4;
        }
        if flag(si, "showColumnStripes") {
            fl |= 8;
        }
        r.with(513, |p| {
            p.u16(fl).wstr(si.attr("name").unwrap_or(""));
        });
    }
    r.empty(344);
    r.bytes
}

fn shared_strings(pkg: &Package, refs: u32) -> Result<Vec<u8>, String> {
    let mut items = Vec::new();
    if let Some(b) = pkg.part("xl/sharedStrings.xml") {
        let root = xml::parse(b)?;
        for si in root.children("si") {
            items.push(si_text(si));
        }
    }
    let mut r = Recs::default();
    r.with(159, |p| {
        p.u32(refs).u32(items.len() as u32);
    });
    for s in &items {
        r.with(19, |p| {
            p.u8(0).wstr(s);
        });
    }
    r.empty(160);
    Ok(r.bytes)
}

fn metadata_bin() -> Vec<u8> {
    let mut r = Recs::default();
    r.empty(332);
    r.rec(334, &[1, 0, 0, 0]);
    r.with(335, |p| {
        p.raw(&[0xB0, 0xC0, 0x6A, 0xD8, 0xC0, 0xD4, 0x01, 0x00])
            .wstr("XLDAPR");
    });
    r.empty(336);
    r.with(339, |p| {
        p.u32(1).wstr("XLDAPR");
    });
    r.empty(52);
    r.rec(35, &[2, 0, 2, 0]);
    r.empty(4096);
    r.rec(4097, &[1, 0]);
    r.empty(36);
    r.empty(53);
    r.empty(340);
    r.rec(337, &[1, 0, 0, 0, 1, 0, 0, 0]);
    r.rec(51, &[1, 0, 0, 0, 1, 0, 0, 0, 0, 0, 0, 0]);
    r.empty(338);
    r.empty(333);
    r.bytes
}

fn name_record(p: &mut P, n: &NameEnt) {
    p.u32(n.flags).u8(0).u32(n.scope.unwrap_or(0xFFFF_FFFF));
    p.wstr(&n.name);
    p.u32(n.rgce.len() as u32).raw(&n.rgce);
    p.u32(n.rgcb.len() as u32).raw(&n.rgcb);
    p.null_str();
    if n.flags & 2 != 0 {
        for _ in 0..4 {
            p.null_str();
        }
    }
}

pub fn xlsx_to_xlsb(bytes: &[u8]) -> Result<(Vec<u8>, Vec<String>), String> {
    let pkg = Package::read(bytes)?;
    let mut warnings = Vec::new();
    let book_part = "xl/workbook.xml";
    let book = rd_xml(&pkg, book_part)?;
    let book_rels = pkg.rels(book_part);
    let target_of = |rid: &str| -> Option<String> {
        book_rels
            .iter()
            .find(|r| r.id == rid)
            .map(|r| resolve(dir_of(book_part), &r.target))
    };

    let mut sheets: Vec<SheetIn> = Vec::new();
    let mut tables: Vec<TableIn> = Vec::new();
    let mut sheet_parts: Vec<String> = Vec::new();
    for s in book
        .child("sheets")
        .map(|n| n.children("sheet").collect::<Vec<_>>())
        .unwrap_or_default()
    {
        let rid = s.attr("id").unwrap_or("");
        let part = target_of(rid).ok_or_else(|| format!("sheet {rid}: no part"))?;
        let node = rd_xml(&pkg, &part)?;
        let mut mine = Vec::new();
        for rel in pkg.rels(&part) {
            if rel.kind() == "table" {
                let tp = resolve(dir_of(&part), &rel.target);
                let tn = rd_xml(&pkg, &tp)?;
                let columns: Vec<String> = tn
                    .child("tableColumns")
                    .map(|c| {
                        c.children("tableColumn")
                            .map(|x| x.attr("name").unwrap_or("").to_string())
                            .collect()
                    })
                    .unwrap_or_default();
                let name = tn.attr("name").unwrap_or("").to_string();
                let display = tn.attr("displayName").unwrap_or(&name).to_string();
                mine.push(tables.len());
                tables.push(TableIn {
                    ent: TableEnt {
                        id: num(&tn, "id").unwrap_or(1.0) as u32,
                        names: [name, display],
                        columns,
                    },
                    node: tn,
                });
            } else if !matches!(rel.kind(), "printerSettings") {
                warnings.push(format!(
                    "sheet {}: relationship {} not carried",
                    s.attr("name").unwrap_or(""),
                    rel.kind()
                ));
            }
        }
        sheets.push(SheetIn {
            name: s.attr("name").unwrap_or("").to_string(),
            sheet_id: num(s, "sheetId").unwrap_or(1.0) as u32,
            state: match s.attr("state") {
                Some("hidden") => 1,
                Some("veryHidden") => 2,
                _ => 0,
            },
            node,
            tables: mine,
        });
        sheet_parts.push(part);
    }
    let sheet_names: Vec<String> = sheets.iter().map(|s| s.name.clone()).collect();
    let table_ents: Vec<TableEnt> = tables
        .iter()
        .map(|t| TableEnt {
            id: t.ent.id,
            names: t.ent.names.clone(),
            columns: t.ent.columns.clone(),
        })
        .collect();

    let mut st = State::default();
    if !sheets.is_empty() {
        st.xti.push((0, 0, 0));
    }
    let mut name_src: Vec<(usize, String)> = Vec::new();
    if let Some(dn) = book.child("definedNames") {
        for n in dn.children("definedName") {
            let name = n.attr("name").unwrap_or("").to_string();
            if name.starts_with("_xlnm.") {
                warnings.push(format!("name {name} not carried"));
                continue;
            }
            name_src.push((st.names.len(), n.text.trim().to_string()));
            st.names.push(NameEnt {
                name,
                scope: n.attr("localSheetId").and_then(|v| v.parse().ok()),
                flags: u32::from(flag(n, "hidden")),
                rgce: vec![0x1C, 0x1D],
                rgcb: Vec::new(),
            });
        }
    }
    let ctx = Ctx {
        sheets: &sheet_names,
        tables: &table_ents,
    };
    for (i, text) in name_src {
        let scope = st.names[i].scope;
        match enc::encode(ctx.sheets, ctx.tables, &mut st, scope, &text, Cls::Ref) {
            Ok(f) => {
                st.names[i].rgce = f.rgce;
                st.names[i].rgcb = f.rgcb;
            }
            Err(e) => warnings.push(format!("name {}: {e}", st.names[i].name)),
        }
    }

    let mut parts: Vec<(String, Vec<u8>)> = Vec::new();
    let mut sheet_rels: Vec<Vec<(String, String)>> = Vec::new();
    let mut sst_refs = 0u32;
    let mut outs: Vec<SheetOut> = Vec::new();
    for (i, sh) in sheets.iter().enumerate() {
        if let Some(d) = sh.node.child("sheetData") {
            sst_refs += d
                .children("row")
                .flat_map(|r| r.children("c"))
                .filter(|c| c.attr("t") == Some("s"))
                .count() as u32;
        }
        let rids: Vec<String> = (1..=sh.tables.len()).map(|k| format!("rId{k}")).collect();
        let mut cx = Cx {
            ctx: &ctx,
            st: &mut st,
            sheet: i as u32,
            warnings: &mut warnings,
            members: std::collections::HashMap::new(),
        };
        let out = write_sheet(&mut cx, sh, &rids);
        sheet_rels.push(
            sh.tables
                .iter()
                .enumerate()
                .map(|(k, &t)| {
                    (
                        format!("rId{}", k + 1),
                        format!("../tables/table{}.bin", t + 1),
                    )
                })
                .collect(),
        );
        outs.push(out);
    }

    let mut r = Recs::default();
    r.empty(131);
    r.rec(128, &{
        let mut p = P::default();
        p.raw(&[0; 16]).wstr("xl").wstr("7").wstr("7").wstr("30527");
        p.0
    });
    let wbpr = book.child("workbookPr");
    r.with(153, |p| {
        let mut f: u32 = 0x0001_0020;
        if wbpr.is_some_and(|w| flag(w, "date1904")) {
            f |= 1;
        }
        p.u32(f)
            .u32(0x0003_163C)
            .wstr(wbpr.and_then(|w| w.attr("codeName")).unwrap_or(""));
    });
    r.empty(135);
    let active = book
        .child("bookViews")
        .and_then(|b| b.child("workbookView"))
        .and_then(|v| num(v, "activeTab"))
        .unwrap_or(0.0) as u32;
    r.with(158, |p| {
        p.u32(1900)
            .u32(1900)
            .u32(0x7080)
            .u32(0x3C0A)
            .u32(600)
            .u32(0)
            .u32(active)
            .u8(0x78);
    });
    r.empty(136);
    r.empty(143);
    for (i, sh) in sheets.iter().enumerate() {
        r.with(156, |p| {
            p.u32(sh.state)
                .u32(sh.sheet_id)
                .wstr(&format!("rId{}", i + 1))
                .wstr(&sh.name);
        });
    }
    r.empty(144);
    let cp = book.child("calcPr");
    r.with(157, |p| {
        let get = |k: &str| cp.and_then(|c| num(c, k));
        let on = |k: &str| cp.is_some_and(|c| flag(c, k));
        let mut flags: u16 = 0x0062;
        if on("fullCalcOnLoad") {
            flags |= 1;
        }
        if on("iterate") {
            flags |= 4;
        }
        if cp.and_then(|c| c.attr("fullPrecision")) != Some("0") {
            flags |= 8;
        }
        p.u32(get("calcId").unwrap_or(191029.0) as u32)
            .u32(u32::from(
                cp.and_then(|c| c.attr("calcMode")) != Some("manual"),
            ))
            .u32(get("iterateCount").unwrap_or(100.0) as u32)
            .f64(get("iterateDelta").unwrap_or(0.001))
            .u32(1)
            .u16(flags);
    });
    r.empty(353);
    r.empty(357);
    r.with(362, |p| {
        p.u32(st.xti.len() as u32);
        for &(b, a, z) in &st.xti {
            p.u32(b).u32(a as u32).u32(z as u32);
        }
    });
    r.empty(354);
    for n in &st.names {
        r.with(39, |p| name_record(p, n));
    }
    r.empty(132);
    let workbook_bin = r.bytes;

    let mut rel_items: Vec<(String, String, String)> = Vec::new();
    let mut ct: Vec<(String, &str)> = Vec::new();
    for (i, o) in outs.into_iter().enumerate() {
        let n = i + 1;
        parts.push((format!("xl/worksheets/sheet{n}.bin"), o.bin));
        ct.push((
            format!("/xl/worksheets/sheet{n}.bin"),
            "application/vnd.ms-excel.worksheet",
        ));
        rel_items.push((
            format!("rId{n}"),
            format!("{REL}/worksheet"),
            format!("worksheets/sheet{n}.bin"),
        ));
        if !sheet_rels[i].is_empty() {
            parts.push((
                format!("xl/worksheets/_rels/sheet{n}.bin.rels"),
                rels_xml(
                    &sheet_rels[i]
                        .iter()
                        .map(|(id, t)| (id.clone(), format!("{REL}/table"), t.clone()))
                        .collect::<Vec<_>>(),
                ),
            ));
        }
    }
    for (i, t) in tables.iter().enumerate() {
        parts.push((format!("xl/tables/table{}.bin", i + 1), table_bin(t)));
        ct.push((
            format!("/xl/tables/table{}.bin", i + 1),
            "application/vnd.ms-excel.table",
        ));
    }
    let mut next = sheets.len() + 1;
    let mut add_rel = |ty: &str, target: &str| {
        rel_items.push((format!("rId{next}"), ty.to_string(), target.to_string()));
        next += 1;
    };
    parts.push((
        "xl/sharedStrings.bin".into(),
        shared_strings(&pkg, sst_refs)?,
    ));
    ct.push((
        "/xl/sharedStrings.bin".into(),
        "application/vnd.ms-excel.sharedStrings",
    ));
    add_rel(&format!("{REL}/sharedStrings"), "sharedStrings.bin");
    if let Some(s) = pkg.part("xl/styles.xml") {
        parts.push(("xl/styles.bin".into(), crate::xlsb_styles::to_bin(s)?));
        ct.push(("/xl/styles.bin".into(), "application/vnd.ms-excel.styles"));
        add_rel(&format!("{REL}/styles"), "styles.bin");
    }
    if pkg.part("xl/metadata.xml").is_some() {
        parts.push(("xl/metadata.bin".into(), metadata_bin()));
        ct.push((
            "/xl/metadata.bin".into(),
            "application/vnd.ms-excel.sheetMetadata",
        ));
        add_rel(&format!("{REL}/sheetMetadata"), "metadata.bin");
    }
    if let Some(v) = pkg.part("xl/vbaProject.bin") {
        parts.push(("xl/vbaProject.bin".into(), v.to_vec()));
        ct.push((
            "/xl/vbaProject.bin".into(),
            "application/vnd.ms-office.vbaProject",
        ));
        add_rel(
            "http://schemas.microsoft.com/office/2006/relationships/vbaProject",
            "vbaProject.bin",
        );
    }
    parts.push(("xl/workbook.bin".into(), workbook_bin));
    parts.push(("xl/_rels/workbook.bin.rels".into(), rels_xml(&rel_items)));
    let mut root_rels = vec![(
        "rId1".to_string(),
        format!("{REL}/officeDocument"),
        "xl/workbook.bin".to_string(),
    )];
    for (name, ty, ctype) in [
        (
            "docProps/core.xml",
            "http://schemas.openxmlformats.org/package/2006/relationships/metadata/core-properties",
            "application/vnd.openxmlformats-package.core-properties+xml",
        ),
        (
            "docProps/app.xml",
            "http://schemas.openxmlformats.org/officeDocument/2006/relationships/extended-properties",
            "application/vnd.openxmlformats-officedocument.extended-properties+xml",
        ),
    ] {
        if let Some(b) = pkg.part(name) {
            parts.push((name.to_string(), b.to_vec()));
            ct.push((format!("/{name}"), ctype));
            root_rels.push((format!("rId{}", root_rels.len() + 1), ty.to_string(), name.to_string()));
        }
    }
    parts.push(("_rels/.rels".into(), rels_xml(&root_rels)));
    let mut cts = String::from(
        "<?xml version=\"1.0\" encoding=\"UTF-8\" standalone=\"yes\"?>\n<Types xmlns=\"http://schemas.openxmlformats.org/package/2006/content-types\"><Default Extension=\"bin\" ContentType=\"application/vnd.ms-excel.sheet.binary.macroEnabled.main\"/><Default Extension=\"rels\" ContentType=\"application/vnd.openxmlformats-package.relationships+xml\"/><Default Extension=\"xml\" ContentType=\"application/xml\"/>",
    );
    for (p, t) in &ct {
        cts.push_str(&format!("<Override PartName=\"{p}\" ContentType=\"{t}\"/>"));
    }
    cts.push_str("</Types>");

    let mut out = std::io::Cursor::new(Vec::new());
    {
        let mut zw = zip::ZipWriter::new(&mut out);
        let opts = zip::write::SimpleFileOptions::default();
        zw.start_file("[Content_Types].xml", opts)
            .map_err(|e| e.to_string())?;
        zw.write_all(cts.as_bytes()).map_err(|e| e.to_string())?;
        for (name, bytes) in &parts {
            zw.start_file(name, opts).map_err(|e| e.to_string())?;
            zw.write_all(bytes).map_err(|e| e.to_string())?;
        }
        zw.finish().map_err(|e| e.to_string())?;
    }
    Ok((out.into_inner(), warnings))
}

fn rels_xml(items: &[(String, String, String)]) -> Vec<u8> {
    let mut s = format!(
        "<?xml version=\"1.0\" encoding=\"UTF-8\" standalone=\"yes\"?>\n<Relationships xmlns=\"{PKG_REL}\">"
    );
    for (id, ty, target) in items {
        s.push_str(&format!(
            "<Relationship Id=\"{id}\" Type=\"{ty}\" Target=\"{target}\"/>"
        ));
    }
    s.push_str("</Relationships>");
    s.into_bytes()
}
