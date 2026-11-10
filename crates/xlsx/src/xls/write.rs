use super::ptg_out::{from_biff12, put_chars};
use super::wsheet::{self, SheetIn};
use super::wstyles::{self, StyleOut};
use crate::package::{dir_of, resolve, Package};
use crate::xlsb_enc::{self as enc, Cls, NameEnt, State, TableArea, TableEnt};
use crate::xml::{self, Node};
use xlcalc_vbaproject::cfb::{Cfb, CfbBuilder};

pub(super) const MAX_RECORD: usize = 8224;

#[derive(Default)]
pub(crate) struct Out {
    pub b: Vec<u8>,
}

impl Out {
    pub fn rec(&mut self, id: u16, d: &[u8]) {
        let mut d = d;
        let mut id = id;

        loop {
            let n = d.len().min(MAX_RECORD);
            self.b.extend(id.to_le_bytes());
            self.b.extend((n as u16).to_le_bytes());
            self.b.extend_from_slice(&d[..n]);
            d = &d[n..];
            if d.is_empty() {
                break;
            }
            id = 0x3C;
        }
    }
    pub fn pos(&self) -> usize {
        self.b.len()
    }
}

pub(super) fn rd_xml(pkg: &Package, name: &str) -> Result<Node, String> {
    let b = pkg
        .part(name)
        .ok_or_else(|| format!("missing part {name}"))?;
    xml::parse(b).map_err(|e| format!("{name}: {e}"))
}

pub(super) fn num(n: &Node, key: &str) -> Option<f64> {
    n.attr(key).and_then(|v| v.parse().ok())
}

pub(super) fn flag(n: &Node, key: &str) -> bool {
    matches!(n.attr(key), Some("1" | "true"))
}

pub(super) fn bof(o: &mut Out, dt: u16) {
    let mut d = Vec::new();
    d.extend(0x0600u16.to_le_bytes());
    d.extend(dt.to_le_bytes());
    d.extend(0x4F5Au16.to_le_bytes());
    d.extend(0x07CDu16.to_le_bytes());
    d.extend(0x00C1u32.to_le_bytes());
    d.extend(0x0806u32.to_le_bytes());
    o.rec(0x809, &d);
}

fn short_name(d: &mut Vec<u8>, s: &str) {
    d.push(s.encode_utf16().count().min(255) as u8);
    put_chars(d, s);
}

fn sst_records(o: &mut Out, strings: &[String], total_refs: u32) {
    let mut recs: Vec<Vec<u8>> = Vec::new();
    let mut cur: Vec<u8> = Vec::new();
    cur.extend(total_refs.to_le_bytes());
    cur.extend((strings.len() as u32).to_le_bytes());
    for s in strings {
        let u: Vec<u16> = s.encode_utf16().collect();
        let u = &u[..u.len().min(32767)];
        let wide = u.iter().any(|&x| x >= 0x100);
        let w = if wide { 2 } else { 1 };

        if cur.len() + 3 + w > MAX_RECORD {
            recs.push(std::mem::take(&mut cur));
        }
        cur.extend((u.len() as u16).to_le_bytes());
        cur.push(u8::from(wide));
        let mut i = 0;
        while i < u.len() {
            let room = (MAX_RECORD - cur.len()) / w;
            if room == 0 {
                recs.push(std::mem::take(&mut cur));
                cur.push(u8::from(wide));
                continue;
            }
            let n = room.min(u.len() - i);
            for &x in &u[i..i + n] {
                if wide {
                    cur.extend(x.to_le_bytes());
                } else {
                    cur.push(x as u8);
                }
            }
            i += n;
            if i < u.len() {
                recs.push(std::mem::take(&mut cur));
                cur.push(u8::from(wide));
            }
        }
    }
    recs.push(cur);
    let mut first = true;
    for r in recs {
        let id: u16 = if first { 0xFC } else { 0x3C };
        first = false;
        o.b.extend(id.to_le_bytes());
        o.b.extend((r.len() as u16).to_le_bytes());
        o.b.extend(r);
    }
}

fn builtin_code(name: &str) -> Option<u8> {
    let n = name.strip_prefix("_xlnm.")?;
    super::read::BUILTIN_NAMES
        .iter()
        .position(|b| *b == n)
        .map(|i| i as u8)
}

pub fn xlsx_to_xls(bytes: &[u8]) -> Result<(Vec<u8>, Vec<String>), String> {
    let pkg = Package::read(bytes)?;
    let mut warnings: Vec<String> = Vec::new();
    let book_part = "xl/workbook.xml";
    let book = rd_xml(&pkg, book_part)?;
    let book_rels = pkg.rels(book_part);
    let target_of = |rid: &str| -> Option<String> {
        book_rels
            .iter()
            .find(|r| r.id == rid)
            .map(|r| resolve(dir_of(book_part), &r.target))
    };
    let part_of_kind = |kind: &str| -> Option<String> {
        book_rels
            .iter()
            .find(|r| r.kind() == kind)
            .map(|r| resolve(dir_of(book_part), &r.target))
    };

    let styles_node = match part_of_kind("styles") {
        Some(p) => rd_xml(&pkg, &p)?,
        None => Node::default(),
    };
    let theme_node = part_of_kind("theme").and_then(|p| rd_xml(&pkg, &p).ok());
    let theme_rgb = wstyles::theme_colors(theme_node.as_ref());
    let st_out: StyleOut = wstyles::convert(&styles_node, theme_node.as_ref());

    let mut sst: Vec<String> = Vec::new();
    if let Some(p) = part_of_kind("sharedStrings") {
        let n = rd_xml(&pkg, &p)?;
        for si in n.children("si") {
            sst.push(crate::si_text(si));
        }
    }

    let mut sheets: Vec<SheetIn> = Vec::new();
    let mut pivot_caches: Vec<super::wpivot::CacheIn> = Vec::new();
    let mut tables: Vec<TableEnt> = Vec::new();
    for (sheet_no, s) in book
        .child("sheets")
        .map(|n| n.children("sheet").collect::<Vec<_>>())
        .unwrap_or_default()
        .into_iter()
        .enumerate()
    {
        let rid = s.attr("id").unwrap_or("");
        let part = target_of(rid).ok_or_else(|| format!("sheet {rid}: no part"))?;
        let node = rd_xml(&pkg, &part)?;
        let name = s.attr("name").unwrap_or("").to_string();
        if name.encode_utf16().count() > 31 {
            warnings.push(format!("sheet name {name} is longer than 31 characters"));
        }
        let mut links = std::collections::HashMap::new();
        let mut comments_part = None;
        let mut vml_part = None;
        let mut drawing_part = None;
        let mut sheet_tables: Vec<Node> = Vec::new();
        let mut sheet_pivots: Vec<super::wpivot::PivotIn> = Vec::new();
        for rel in pkg.rels(&part) {
            if rel.kind() == "hyperlink" {
                links.insert(rel.id.clone(), rel.target.clone());
            } else if rel.kind() == "comments" {
                comments_part = Some(resolve(dir_of(&part), &rel.target));
            } else if rel.kind() == "vmlDrawing" {
                vml_part = Some(resolve(dir_of(&part), &rel.target));
            } else if rel.kind() == "drawing" {
                drawing_part = Some(resolve(dir_of(&part), &rel.target));
            } else if rel.kind() == "pivotTable" {
                let tp = resolve(dir_of(&part), &rel.target);
                match super::wpivot::read_table(&pkg, &tp, &mut pivot_caches) {
                    Ok(p) => sheet_pivots.push(p),
                    Err(e) => warnings.push(format!("sheet {name}: pivot table not written: {e}")),
                }
            } else if rel.kind() == "table" {
                let tp = resolve(dir_of(&part), &rel.target);
                let t = rd_xml(&pkg, &tp)?;
                sheet_tables.push(t.clone());
                if let Some(rc) = t.attr("ref").and_then(crate::parse_rect) {
                    let name = t.attr("name").unwrap_or("").to_string();
                    let display = t.attr("displayName").unwrap_or(&name).to_string();
                    tables.push(TableEnt {
                        id: num(&t, "id").unwrap_or(1.0) as u32,
                        names: [name, display],
                        columns: t
                            .child("tableColumns")
                            .map(|c| {
                                c.children("tableColumn")
                                    .map(|x| x.attr("name").unwrap_or("").to_string())
                                    .collect()
                            })
                            .unwrap_or_default(),
                        sheet: sheet_no as u32,
                        area: Some(TableArea {
                            sheet: sheet_no as u32,
                            r0: rc.r0,
                            c0: rc.c0,
                            r1: rc.r1,
                            header: num(&t, "headerRowCount") != Some(0.0),
                            totals: num(&t, "totalsRowCount").unwrap_or(0.0) > 0.0,
                        }),
                    });
                }
            } else if !matches!(rel.kind(), "printerSettings" | "comments" | "vmlDrawing") {
                warnings.push(format!(
                    "sheet {name}: relationship {} not carried",
                    rel.kind()
                ));
            }
        }
        let notes = match comments_part {
            Some(p) => {
                let c = rd_xml(&pkg, &p)?;
                let vml = vml_part
                    .as_ref()
                    .and_then(|v| pkg.part(v))
                    .map(|b| String::from_utf8_lossy(b).into_owned());
                super::wcomments::read(&c, vml.as_deref(), &super::wdraw::Dims::of(&node))
            }
            None => Vec::new(),
        };
        let shapes = match drawing_part {
            Some(p) => {
                let d = rd_xml(&pkg, &p)?;
                let rels = pkg.rels(&p);
                let chart_of = |rid: &str| -> Option<Node> {
                    let rel = rels.iter().find(|r| r.id == rid)?;
                    rd_xml(&pkg, &resolve(dir_of(&p), &rel.target)).ok()
                };
                let image_of = |rid: &str| -> Option<super::wdraw::Image> {
                    let rel = rels.iter().find(|r| r.id == rid)?;
                    let part = resolve(dir_of(&p), &rel.target);
                    let bytes = pkg.part(&part)?.to_vec();
                    let ext = part.rsplit_once('.')?.1.to_lowercase();
                    Some((ext, bytes))
                };
                let dims = super::wdraw::Dims::of(&node);
                super::wdraw::read(
                    &d,
                    &super::wdraw::Source {
                        dims: &dims,
                        sheet: &name,
                        chart_of: &chart_of,
                        image_of: &image_of,
                        theme: &theme_rgb,
                    },
                    &mut warnings,
                )
            }
            None => Vec::new(),
        };
        sheets.push(SheetIn {
            name,
            links,
            notes,
            shapes,
            state: match s.attr("state") {
                Some("hidden") => 1,
                Some("veryHidden") => 2,
                _ => 0,
            },
            chart: node.name == "chartsheet",
            tables: sheet_tables,
            pivots: sheet_pivots,
            node,
        });
    }
    let sheet_names: Vec<String> = sheets.iter().map(|s| s.name.clone()).collect();

    let ext_out = super::wext::read_all(&pkg, &book, &target_of)?;

    let mut seed: Option<(Vec<NameEnt>, Vec<usize>)> = None;
    let mut dg: Vec<(u32, u32)> = Vec::new();
    let mut blips: Vec<super::wdraw::BlipOut> = Vec::new();
    let (state, sheet_streams) = loop {
        let warned = warnings.len();
        dg.clear();
        blips.clear();
        let mut state = State {
            legacy: true,
            ext: ext_out.iter().map(|e| e.ent.clone()).collect(),
            ..State::default()
        };
        let mut name_src: Vec<(usize, String)> = Vec::new();
        let mut at: Vec<usize> = Vec::new();
        let seeded = seed.is_some();
        if let Some((names, pos)) = seed.take() {
            state.names = names;
            at = pos;
        }
        if let Some(dn) = book.child("definedNames") {
            for (k, n) in dn.children("definedName").enumerate() {
                let name = n.attr("name").unwrap_or("").to_string();
                if seeded {
                    name_src.push((at[k], n.text.trim().to_string()));
                    continue;
                }
                at.push(state.names.len());
                name_src.push((state.names.len(), n.text.trim().to_string()));
                state.names.push(NameEnt {
                    name,
                    scope: n.attr("localSheetId").and_then(|v| v.parse().ok()),
                    flags: u32::from(flag(n, "hidden")),
                    rgce: vec![0x1C, 0x1D],
                    rgcb: Vec::new(),
                });
            }
        }
        for (i, text) in name_src {
            let scope = state.names[i].scope;
            match enc::encode(&sheet_names, &tables, &mut state, scope, &text, Cls::Ref) {
                Ok(f) => {
                    state.names[i].rgce = f.rgce;
                    state.names[i].rgcb = f.rgcb;
                }

                Err(e) if e.starts_with("LET is") || e.starts_with("LAMBDA is") => {
                    state.names[i].rgce = vec![0x1C, 0x0F];
                    state.names[i].flags |= 0x10;
                    warnings.push(format!(
                        "name {}: {e}; the file holds #VALUE!",
                        state.names[i].name
                    ));
                }
                Err(e) => warnings.push(format!("name {}: {e}", state.names[i].name)),
            }
        }

        let dxfs: Vec<Node> = styles_node
            .child("dxfs")
            .map(|d| d.children("dxf").cloned().collect())
            .unwrap_or_default();
        let sheet_streams: Vec<wsheet::SheetOut> = {
            let mut v = Vec::new();
            let mut cx = wsheet::Cx {
                names: &sheet_names,
                tables: &tables,
                st: &mut state,
                styles: &st_out,
                warnings: &mut warnings,
                sst: &sst,
                dg: &mut dg,
                blips: &mut blips,
                dxfs: &dxfs,
                calc: book.child("calcPr"),
                caches: &pivot_caches,
            };
            for (i, sh) in sheets.iter().enumerate() {
                v.push(wsheet::write_sheet(&mut cx, sh, i as u32)?);
            }
            v
        };
        let group = |n: &str| match n.get(..6) {
            Some("_xlfn." | "_xlws.") => 0,
            Some("_xlpm.") => 1,
            _ => 2,
        };
        let mut order: Vec<usize> = (0..state.names.len()).collect();
        order.sort_by_cached_key(|&i| {
            let n = &state.names[i].name;
            (
                group(n),
                if group(n) < 2 {
                    n.to_lowercase()
                } else {
                    String::new()
                },
            )
        });
        if !seeded && order.iter().enumerate().any(|(a, &b)| a != b) {
            let mut by_old: Vec<Option<NameEnt>> = std::mem::take(&mut state.names)
                .into_iter()
                .map(Some)
                .collect();
            let mut new_at = vec![0; order.len()];
            let mut names = Vec::new();
            for (new, &old) in order.iter().enumerate() {
                new_at[old] = new;
                if let Some(mut e) = by_old[old].take() {
                    e.rgce = vec![0x1C, 0x1D];
                    e.rgcb = Vec::new();
                    names.push(e);
                }
            }
            seed = Some((names, at.iter().map(|&o| new_at[o]).collect()));
            warnings.truncate(warned);
            continue;
        }
        break (state, sheet_streams);
    };
    let sst_refs: u32 = sheets
        .iter()
        .filter_map(|s| s.node.child("sheetData"))
        .flat_map(|d| d.children("row"))
        .flat_map(|r| r.children("c"))
        .filter(|c| c.attr("t") == Some("s"))
        .count() as u32;

    let mut o = Out::default();
    bof(&mut o, 0x05);
    o.rec(0xE1, &0x04B0u16.to_le_bytes());
    o.rec(0xC1, &[0, 0]);
    o.rec(0xE2, &[]);
    {
        let mut d = vec![0x06, 0x00, 0x00];
        d.extend(b"xlcalc");
        d.resize(112, b' ');
        o.rec(0x5C, &d);
    }
    o.rec(0x42, &0x04B0u16.to_le_bytes());
    o.rec(0x161, &[0, 0]);
    o.rec(0x1C0, &[]);
    {
        let mut d = Vec::new();
        d.extend((sheets.len() as u16).to_le_bytes());
        for i in 1..=sheets.len() {
            d.extend((i as u16).to_le_bytes());
        }
        o.rec(0x13D, &d);
    }
    o.rec(0x9C, &[0x11, 0]);
    {
        let prot = book.child("workbookProtection");
        let on = |k: &str| u16::from(prot.is_some_and(|p| flag(p, k)));
        let pwd = prot
            .and_then(|p| p.attr("workbookPassword"))
            .and_then(|h| u16::from_str_radix(h, 16).ok())
            .unwrap_or(0);
        o.rec(0x19, &on("lockWindows").to_le_bytes());
        o.rec(0x12, &on("lockStructure").to_le_bytes());
        o.rec(0x13, &pwd.to_le_bytes());
        o.rec(0x1AF, &on("lockRevision").to_le_bytes());
        o.rec(0x1BC, &[0, 0]);
    }
    {
        let view = book
            .child("bookViews")
            .and_then(|v| v.child("workbookView"));
        let g = |k: &str, d: f64| view.and_then(|v| num(v, k)).unwrap_or(d) as u16;
        let mut d = Vec::new();
        d.extend(g("xWindow", 0.0).to_le_bytes());
        d.extend(g("yWindow", 0.0).to_le_bytes());
        d.extend(g("windowWidth", 16000.0).to_le_bytes());
        d.extend(g("windowHeight", 9000.0).to_le_bytes());
        let mut fl = 0x38u16;
        if view.is_some_and(|v| v.attr("showHorizontalScroll") == Some("0")) {
            fl &= !0x08;
        }
        if view.is_some_and(|v| v.attr("showVerticalScroll") == Some("0")) {
            fl &= !0x10;
        }
        if view.is_some_and(|v| v.attr("showSheetTabs") == Some("0")) {
            fl &= !0x20;
        }
        d.extend(fl.to_le_bytes());
        d.extend(g("activeTab", 0.0).to_le_bytes());
        d.extend(g("firstSheet", 0.0).to_le_bytes());
        d.extend(1u16.to_le_bytes());
        d.extend(600u16.to_le_bytes());
        o.rec(0x3D, &d);
    }
    o.rec(0x40, &[0, 0]);
    o.rec(0x8D, &[0, 0]);
    let wbpr = book.child("workbookPr");
    o.rec(
        0x22,
        &[u8::from(wbpr.is_some_and(|w| flag(w, "date1904"))), 0],
    );
    let calc = book.child("calcPr");
    o.rec(
        0x0E,
        &[
            u8::from(calc.is_none_or(|c| c.attr("fullPrecision") != Some("0"))),
            0,
        ],
    );
    o.rec(0x1B7, &[0, 0]);
    o.rec(0xDA, &[0, 0]);
    for (id, d) in st_out
        .fonts
        .iter()
        .chain(st_out.formats.iter())
        .chain(st_out.xfs.iter())
        .chain(st_out.xfext.iter())
        .chain(st_out.dxfs.iter())
        .chain(st_out.styles.iter())
        .chain(st_out.table_styles.iter())
    {
        o.rec(*id, d);
    }
    let pv_tables: Vec<Vec<&Node>> = (0..pivot_caches.len())
        .map(|c| {
            sheets
                .iter()
                .flat_map(|s| s.pivots.iter())
                .filter(|p| p.cache == c)
                .map(|p| &p.table)
                .collect()
        })
        .collect();
    let pv_out = super::wpivot::write_caches(&mut o, &pivot_caches, &pv_tables, &mut warnings);
    o.rec(0x160, &[0, 0]);

    let mut bound_at: Vec<usize> = Vec::new();
    for sh in &sheets {
        bound_at.push(o.pos());
        let mut d = vec![0u8; 4];
        d.push(sh.state);
        d.push(if sh.chart { 2 } else { 0 });
        short_name(&mut d, &sh.name);
        o.rec(0x85, &d);
    }
    o.rec(0x8C, &[1, 0, 0x40, 0]);

    {
        let addin = state.xti.iter().any(|x| x.0 == enc::ADDIN_BOOK);
        if addin {
            o.rec(0x1AE, &[0x01, 0x00, 0x01, 0x3A]);
            for a in &state.addins {
                let mut d = vec![0u8; 6];
                short_name(&mut d, a);
                d.extend(2u16.to_le_bytes());
                d.extend([0x1C, 0x17]);
                o.rec(0x23, &d);
            }
        }
        let mut d = Vec::new();
        d.extend((sheets.len() as u16).to_le_bytes());
        d.extend(0x0401u16.to_le_bytes());
        o.rec(0x1AE, &d);
        super::wext::write(&mut o, &ext_out);
        if !state.xti.is_empty() {
            let mut d = Vec::new();
            d.extend((state.xti.len() as u16).to_le_bytes());
            for (book, a, b) in &state.xti {
                let sb = match *book {
                    enc::ADDIN_BOOK => 0,
                    0 => u16::from(addin),
                    n => n as u16 + u16::from(addin),
                };
                d.extend(sb.to_le_bytes());
                d.extend((*a as i16).to_le_bytes());
                d.extend((*b as i16).to_le_bytes());
            }
            o.rec(0x17, &d);
        }
    }
    for n in &state.names {
        let (rgce, extra) = match from_biff12(&n.rgce, &n.rgcb) {
            Ok(x) => x,
            Err(e) => {
                warnings.push(format!("name {}: {e}", n.name));
                (vec![0x1C, 0x1D], Vec::new())
            }
        };
        let code = builtin_code(&n.name);
        let mut flags = (n.flags & 0xFFFF) as u16;
        if code.is_some() {
            flags |= 0x20;
        }
        let mut d = Vec::new();
        d.extend(flags.to_le_bytes());
        d.push(0);
        let mut nm = Vec::new();
        match code {
            Some(c) => {
                d.push(1);
                nm.push(0);
                nm.push(c);
            }
            None => {
                d.push(n.name.encode_utf16().count().min(255) as u8);
                put_chars(&mut nm, &n.name);
            }
        }
        d.extend((rgce.len() as u16).to_le_bytes());
        d.extend(0u16.to_le_bytes());
        d.extend(n.scope.map_or(0, |s| s as u16 + 1).to_le_bytes());
        d.extend([0u8; 4]);
        d.extend(nm);
        d.extend(rgce);
        d.extend(extra);
        o.rec(0x18, &d);
    }

    o.rec(0x1C1, &[0xC1, 0x01, 0, 0, 0x35, 0xEA, 0x0E, 0]);
    if !dg.is_empty() {
        o.rec(0xEB, &super::wcomments::group_record(&dg, &blips));
    }
    sst_records(&mut o, &sst, sst_refs);
    o.rec(0x0A, &[]);

    let mut stream = o.b;
    for (i, so) in sheet_streams.iter().enumerate() {
        let start = stream.len() as u32;
        let at = bound_at[i] + 4;
        stream[at..at + 4].copy_from_slice(&start.to_le_bytes());
        let mut bytes = so.bytes.clone();
        for &p in &so.patch {
            let v = u32::from_le_bytes([bytes[p], bytes[p + 1], bytes[p + 2], bytes[p + 3]]);
            bytes[p..p + 4].copy_from_slice(&(v + start).to_le_bytes());
        }
        stream.extend(bytes);
    }

    let mut cfb = CfbBuilder::new();
    cfb.add_stream("Workbook", stream);
    for c in pv_out {
        cfb.add_stream(&c.name, c.stream);
    }
    {
        let node = |n: &str| rd_xml(&pkg, n).ok();
        let (s, d) = super::props::from_parts(
            node("docProps/core.xml").as_ref(),
            node("docProps/app.xml").as_ref(),
            node("docProps/custom.xml").as_ref(),
        );
        if let Some(s) = s {
            cfb.add_stream(super::props::SUMMARY, s);
        }
        if let Some(d) = d {
            cfb.add_stream(super::props::DOC_SUMMARY, d);
        }
    }
    if let Some(vba) = pkg.part("xl/vbaProject.bin") {
        let src = Cfb::new(vba.to_vec()).map_err(|e| e.to_string())?;
        for s in src.streams() {
            let data = src.read_stream(&s).map_err(|e| e.to_string())?;
            cfb.add_stream(&format!("_VBA_PROJECT_CUR/{s}"), data);
        }
    }
    let out = cfb.finish().map_err(|e| e.to_string())?;
    Ok((out, warnings))
}
