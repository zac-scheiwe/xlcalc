use super::ptg_out::{from_biff12, put_chars};
use super::write::{bof, flag, num, Out};
use super::wstyles::StyleOut;
use crate::xlsb_enc::{self as enc, Cls, State, TableEnt};
use crate::xml::Node;
use crate::{parse_cell, parse_rect, si_text};
use std::collections::HashMap;

pub(super) struct SheetIn {
    pub name: String,

    pub links: HashMap<String, String>,
    pub notes: Vec<super::wcomments::NoteIn>,
    pub shapes: Vec<super::wdraw::ShapeIn>,
    pub state: u8,
    pub chart: bool,

    pub tables: Vec<Node>,

    pub pivots: Vec<super::wpivot::PivotIn>,
    pub node: Node,
}

pub(super) struct SheetOut {
    pub bytes: Vec<u8>,

    pub patch: Vec<usize>,
}

pub(super) struct Cx<'a> {
    pub names: &'a [String],
    pub tables: &'a [TableEnt],
    pub st: &'a mut State,
    pub styles: &'a StyleOut,
    pub warnings: &'a mut Vec<String>,
    pub sst: &'a [String],

    pub dg: &'a mut Vec<(u32, u32)>,

    pub blips: &'a mut Vec<super::wdraw::BlipOut>,
    pub dxfs: &'a [Node],
    pub calc: Option<&'a Node>,

    pub caches: &'a [super::wpivot::CacheIn],
}

type RowIn<'a> = (u32, &'a Node, Vec<(u32, &'a Node)>);

enum V {
    None,
    Num(f64),
    Bool(bool),
    Err(u8),
    Str(String),
    Sst(u32),
}

fn cell_value(c: &Node) -> V {
    let v = c.child("v").map(|v| v.text.as_str());
    match c.attr("t") {
        Some("s") => v
            .and_then(|v| v.trim().parse().ok())
            .map_or(V::None, V::Sst),
        Some("str") => V::Str(v.unwrap_or("").to_string()),
        Some("inlineStr") => V::Str(c.child("is").map(si_text).unwrap_or_default()),
        Some("b") => V::Bool(v.is_some_and(|v| v.trim() == "1")),
        Some("e") => v
            .and_then(|v| enc::error_byte(v.trim()))
            .map_or(V::None, V::Err),
        _ => v
            .and_then(|v| v.trim().parse::<f64>().ok())
            .map_or(V::None, V::Num),
    }
}

fn rk(v: f64) -> Option<u32> {
    if !v.is_finite() || (v == 0.0 && v.is_sign_negative()) {
        return None;
    }
    for (mul, tag) in [(1.0, 2u32), (100.0, 3)] {
        let x = v * mul;
        if x.fract() == 0.0 && x.abs() < 536_870_912.0 && (x / mul == v) {
            return Some(((x as i32 as u32) << 2) | tag);
        }
    }
    None
}

fn ref8(d: &mut Vec<u8>, r0: u32, r1: u32, c0: u32, c1: u32) {
    d.extend((r0 as u16).to_le_bytes());
    d.extend((r1 as u16).to_le_bytes());
    d.push(c0 as u8);
    d.push(c1 as u8);
}

fn xl_string16(d: &mut Vec<u8>, s: &str) {
    d.extend((s.encode_utf16().count().min(32767) as u16).to_le_bytes());
    put_chars(d, s);
}

pub(super) fn write_sheet(cx: &mut Cx<'_>, sh: &SheetIn, idx: u32) -> Result<SheetOut, String> {
    let mut o = Out::default();
    if sh.chart {
        let Some(s) = sh.shapes.iter().find(|s| s.chart.is_some()) else {
            cx.warnings.push(format!(
                "sheet {}: chart sheet without a chart written empty",
                sh.name
            ));
            bof(&mut o, 0x20);
            o.rec(0x0A, &[]);
            return Ok(SheetOut {
                bytes: o.b,
                patch: Vec::new(),
            });
        };
        let Some(c) = s.chart.as_ref() else {
            return Err("chart sheet".into());
        };
        let dgid = cx.dg.len() as u32 + 1;
        let mut enc_ref = |f: &str| -> Option<Vec<u8>> {
            let r = enc::encode(cx.names, cx.tables, cx.st, None, f, Cls::Ref).ok()?;
            from_biff12(&r.rgce, &r.rgcb).ok().map(|x| x.0)
        };
        let bytes = super::wchart::substream(c, &mut enc_ref, dgid, s.size_pt);
        cx.dg.push((dgid, 1));
        return Ok(SheetOut {
            bytes,
            patch: Vec::new(),
        });
    }
    bof(&mut o, 0x10);
    let root = &sh.node;
    let xf_base = cx.styles.xf_base;

    let calc = cx.calc;
    let mode = match calc.and_then(|c| c.attr("calcMode")) {
        Some("manual") => 0u16,
        Some("autoNoTable") => 0xFFFF,
        _ => 1,
    };
    o.rec(0x0D, &mode.to_le_bytes());
    let count = calc.and_then(|c| num(c, "iterateCount")).unwrap_or(100.0) as u16;
    o.rec(0x0C, &count.to_le_bytes());
    o.rec(0x0F, &[1, 0]);
    o.rec(
        0x11,
        &[u8::from(calc.is_some_and(|c| flag(c, "iterate"))), 0],
    );
    let delta = calc.and_then(|c| num(c, "iterateDelta")).unwrap_or(0.001);
    o.rec(0x10, &delta.to_le_bytes());
    o.rec(
        0x5F,
        &[
            u8::from(calc.is_none_or(|c| c.attr("calcOnSave") != Some("0"))),
            0,
        ],
    );
    let opts = root.child("printOptions");
    o.rec(
        0x2A,
        &[u8::from(opts.is_some_and(|p| flag(p, "headings"))), 0],
    );
    o.rec(
        0x2B,
        &[u8::from(opts.is_some_and(|p| flag(p, "gridLines"))), 0],
    );
    o.rec(0x82, &[1, 0]);
    o.rec(0x80, &[0; 8]);
    let fmt = root.child("sheetFormatPr");
    let def_h = fmt
        .and_then(|f| num(f, "defaultRowHeight"))
        .unwrap_or(12.75);
    {
        let mut fl = 0u16;
        if fmt.is_some_and(|f| flag(f, "customHeight")) {
            fl |= 1;
        }
        if fmt.is_some_and(|f| flag(f, "zeroHeight")) {
            fl |= 2;
        }
        let mut d = Vec::new();
        d.extend(fl.to_le_bytes());
        d.extend(((def_h * 20.0).round() as u16).to_le_bytes());
        o.rec(0x225, &d);
    }
    let pr = root.child("sheetPr");
    let outline = pr.and_then(|p| p.child("outlinePr"));
    let mut wsbool = 0x0401u16;
    if outline.is_none_or(|p| p.attr("summaryBelow") != Some("0")) {
        wsbool |= 0x40;
    }
    if outline.is_none_or(|p| p.attr("summaryRight") != Some("0")) {
        wsbool |= 0x80;
    }
    if pr
        .and_then(|p| p.child("pageSetUpPr"))
        .is_some_and(|p| flag(p, "fitToPage"))
    {
        wsbool |= 0x100;
    }
    o.rec(0x81, &wsbool.to_le_bytes());

    let hf = root.child("headerFooter");
    for (id, key) in [(0x14u16, "oddHeader"), (0x15, "oddFooter")] {
        let t = hf
            .and_then(|h| h.child(key))
            .map(|n| n.text.clone())
            .unwrap_or_default();
        if t.is_empty() {
            o.rec(id, &[]);
        } else {
            let mut d = Vec::new();
            xl_string16(&mut d, &t);
            o.rec(id, &d);
        }
    }
    let po = root.child("printOptions");
    o.rec(
        0x83,
        &[
            u8::from(po.is_some_and(|p| flag(p, "horizontalCentered"))),
            0,
        ],
    );
    o.rec(
        0x84,
        &[u8::from(po.is_some_and(|p| flag(p, "verticalCentered"))), 0],
    );
    let pm = root.child("pageMargins");
    for (id, key, d) in [
        (0x26u16, "left", 0.7),
        (0x27, "right", 0.7),
        (0x28, "top", 0.75),
        (0x29, "bottom", 0.75),
    ] {
        let v = pm.and_then(|m| num(m, key)).unwrap_or(d);
        o.rec(id, &v.to_le_bytes());
    }
    {
        let ps = root.child("pageSetup");
        let g = |k: &str, d: f64| ps.and_then(|p| num(p, k)).unwrap_or(d);
        let mut d = Vec::new();
        d.extend((g("paperSize", 1.0) as u16).to_le_bytes());
        d.extend((g("scale", 100.0) as u16).to_le_bytes());
        d.extend((g("firstPageNumber", 1.0) as u16).to_le_bytes());
        d.extend((g("fitToWidth", 1.0) as u16).to_le_bytes());
        d.extend((g("fitToHeight", 1.0) as u16).to_le_bytes());
        let mut fl = 0u16;
        if ps.is_some_and(|p| p.attr("pageOrder") == Some("overThenDown")) {
            fl |= 1;
        }
        if ps.is_none_or(|p| p.attr("orientation") != Some("landscape")) {
            fl |= 2;
        }
        if ps.is_some_and(|p| p.attr("orientation").is_none()) {
            fl |= 0;
        }
        d.extend(fl.to_le_bytes());
        d.extend((g("horizontalDpi", 600.0) as u16).to_le_bytes());
        d.extend((g("verticalDpi", 600.0) as u16).to_le_bytes());
        d.extend(
            pm.and_then(|m| num(m, "header"))
                .unwrap_or(0.3)
                .to_le_bytes(),
        );
        d.extend(
            pm.and_then(|m| num(m, "footer"))
                .unwrap_or(0.3)
                .to_le_bytes(),
        );
        d.extend((g("copies", 1.0) as u16).to_le_bytes());
        o.rec(0xA1, &d);
    }

    let dcw = fmt
        .and_then(|f| num(f, "baseColWidth"))
        .map_or(8, |v| v as u16);
    o.rec(0x55, &dcw.to_le_bytes());
    if let Some(cols) = root.child("cols") {
        for c in cols.children("col") {
            let min = num(c, "min").unwrap_or(1.0) as u32 - 1;
            let max = (num(c, "max").unwrap_or(1.0) as u32 - 1).min(255);
            if min > 255 {
                continue;
            }
            let width = num(c, "width").unwrap_or(8.43);
            let xf = xf_base + num(c, "style").unwrap_or(0.0) as u32;
            let mut fl = 0u16;
            if flag(c, "hidden") {
                fl |= 1;
            }
            if flag(c, "customWidth") {
                fl |= 2;
            }
            fl |= (num(c, "outlineLevel").unwrap_or(0.0) as u16 & 7) << 8;
            if flag(c, "collapsed") {
                fl |= 0x1000;
            }
            let mut d = Vec::new();
            d.extend((min as u16).to_le_bytes());
            d.extend((max as u16).to_le_bytes());
            d.extend(((width * 256.0).round() as u16).to_le_bytes());
            d.extend((xf as u16).to_le_bytes());
            d.extend(fl.to_le_bytes());
            d.extend(0u16.to_le_bytes());
            o.rec(0x7D, &d);
        }
    }

    let mut rows: Vec<RowIn<'_>> = Vec::new();
    let mut too_big = false;
    if let Some(data) = root.child("sheetData") {
        for row in data.children("row") {
            let rw = num(row, "r").map_or(0, |v| v as u32 - 1);
            if rw > 0xFFFF {
                too_big = true;
                continue;
            }
            let mut cells = Vec::new();
            for c in row.children("c") {
                if let Some((_, col)) = c.attr("r").and_then(parse_cell) {
                    if col > 255 {
                        too_big = true;
                    } else {
                        cells.push((col, c));
                    }
                }
            }
            rows.push((rw, row, cells));
        }
    }
    if too_big {
        cx.warnings.push(format!(
            "sheet {}: cells beyond row 65536 or column IV are dropped",
            sh.name
        ));
    }

    {
        let r0 = rows.first().map_or(0, |r| r.0);
        let r1 = rows.last().map_or(0, |r| r.0 + 1);
        let all = rows.iter().flat_map(|r| r.2.iter().map(|c| c.0));
        let c0 = all.clone().min().unwrap_or(0);
        let c1 = all.max().map_or(0, |c| c + 1);
        let mut d = Vec::new();
        d.extend(r0.to_le_bytes());
        d.extend(r1.to_le_bytes());
        d.extend((c0 as u16).to_le_bytes());
        d.extend((c1 as u16).to_le_bytes());
        d.extend(0u16.to_le_bytes());
        o.rec(0x200, &d);
    }

    let mut walk = Walk {
        sh,
        idx,
        members: HashMap::new(),
        kept: std::collections::HashSet::new(),
    };
    let mut i = 0;
    while i < rows.len() {
        let block = rows[i].0 / 32;
        let mut j = i;
        while j < rows.len() && rows[j].0 / 32 == block {
            j += 1;
        }
        for (rw, row, cells) in &rows[i..j] {
            let ht = num(row, "ht");
            let mut fl = 0x100u32;
            fl |= (num(row, "outlineLevel").unwrap_or(0.0) as u32) & 7;
            if flag(row, "collapsed") {
                fl |= 0x10;
            }
            if flag(row, "hidden") {
                fl |= 0x20;
            }
            if flag(row, "customHeight") {
                fl |= 0x40;
            }
            if flag(row, "customFormat") {
                fl |= 0x80 | ((xf_base + num(row, "s").unwrap_or(0.0) as u32) << 16);
            }
            let mut d = Vec::new();
            d.extend((*rw as u16).to_le_bytes());
            d.extend((cells.first().map_or(0, |c| c.0) as u16).to_le_bytes());
            d.extend((cells.last().map_or(0, |c| c.0 + 1) as u16).to_le_bytes());
            d.extend(
                ht.map_or(0x8000 | (def_h * 20.0) as u16, |h| {
                    (h * 20.0).round() as u16
                })
                .to_le_bytes(),
            );
            d.extend([0u8; 4]);
            d.extend(fl.to_le_bytes());
            o.rec(0x208, &d);
        }
        for (rw, _, cells) in &rows[i..j] {
            for (col, c) in cells {
                write_cell(&mut o, cx, &mut walk, c, *rw, *col);
            }
        }
        i = j;
    }
    if !sh.notes.is_empty() || !sh.shapes.is_empty() {
        let dgid = cx.dg.len() as u32 + 1;
        let flat = super::wdraw::flatten(&sh.shapes);
        let ns = flat.len() as u32;
        let mut items: Vec<super::wdraw::Item> = Vec::new();
        let mut chart_dgs = 0;
        for (i, s) in flat.iter().enumerate() {
            let mut enc_ref = |f: &str| -> Option<Vec<u8>> {
                let r = enc::encode(cx.names, cx.tables, cx.st, None, f, Cls::Ref).ok()?;
                from_biff12(&r.rgce, &r.rgcb).ok().map(|x| x.0)
            };
            if s.chart.is_some() {
                chart_dgs += 1;
            }
            let pib = s.pic.as_ref().map_or(0, |(ext, bytes)| {
                super::wdraw::blip_index(cx.blips, ext, bytes, cx.warnings)
            });
            items.push(super::wdraw::item(
                dgid,
                i as u32,
                s,
                &mut enc_ref,
                dgid + chart_dgs,
                pib,
            ));
        }

        for (g, s) in flat.iter().enumerate() {
            if s.group {
                let members = super::wdraw::descendants(s);
                let own = items[g].sp.len() as u32 - 8;
                let total = own
                    + items[g + 1..=g + members]
                        .iter()
                        .map(super::wdraw::Item::declared)
                        .sum::<u32>();
                items[g].sp[4..8].copy_from_slice(&total.to_le_bytes());
            }
        }
        for (i, n) in sh.notes.iter().enumerate() {
            items.push(super::wcomments::item(dgid, ns + i as u32, n));
        }
        let used = super::wdraw::emit(&mut o, &items, dgid);
        super::wcomments::write(&mut o, &sh.notes, ns + 1);
        cx.dg.push((dgid, used));
        for k in 1..=chart_dgs {
            cx.dg.push((dgid + k, 1));
        }
    }
    for p in &sh.pivots {
        if let Some(c) = cx.caches.get(p.cache) {
            super::wpivot::write_view(&mut o, p, c, cx.warnings);
        }
    }

    let view = root.child("sheetViews").and_then(|v| v.child("sheetView"));
    {
        let mut fl = 0x00B6u16;
        if view.is_some_and(|v| v.attr("showGridLines") == Some("0")) {
            fl &= !0x02;
        }
        if view.is_some_and(|v| v.attr("showRowColHeaders") == Some("0")) {
            fl &= !0x04;
        }
        if view.is_some_and(|v| v.attr("showZeros") == Some("0")) {
            fl &= !0x10;
        }
        if view.is_some_and(|v| flag(v, "rightToLeft")) {
            fl |= 0x40;
        }
        if view.is_some_and(|v| flag(v, "tabSelected")) {
            fl |= 0x600;
        }
        let pane = view.and_then(|v| v.child("pane"));
        let frozen =
            pane.is_some_and(|p| matches!(p.attr("state"), Some("frozen" | "frozenSplit")));
        if frozen {
            fl |= 0x108;
        }
        let mut d = Vec::new();
        d.extend(fl.to_le_bytes());
        d.extend(0u16.to_le_bytes());
        d.extend(0u16.to_le_bytes());
        d.extend(0x40u32.to_le_bytes());
        d.extend(0u16.to_le_bytes());
        d.extend(0u16.to_le_bytes());
        d.extend(0u32.to_le_bytes());
        o.rec(0x23E, &d);
        if let Some(z) = view.and_then(|v| num(v, "zoomScale")) {
            if z != 100.0 {
                let mut d = Vec::new();
                d.extend((z as u16).to_le_bytes());
                d.extend(100u16.to_le_bytes());
                o.rec(0xA0, &d);
            }
        }

        if let Some(p) = pane.filter(|p| {
            frozen || num(p, "xSplit").unwrap_or(0.0) > 0.0 || num(p, "ySplit").unwrap_or(0.0) > 0.0
        }) {
            let px = num(p, "xSplit").unwrap_or(0.0) as u16;
            let py = num(p, "ySplit").unwrap_or(0.0) as u16;
            let (top, left) = p
                .attr("topLeftCell")
                .and_then(parse_cell)
                .map_or((py, px), |(r, c)| (r as u16, c as u16));
            let active = match p.attr("activePane") {
                Some("topRight") => 1u8,
                Some("bottomLeft") => 2,
                Some("topLeft") => 3,
                Some("bottomRight") => 0,
                _ => match (px > 0, py > 0) {
                    (true, true) => 0u8,
                    (true, false) => 1,
                    _ => 2,
                },
            };
            let mut d = Vec::new();
            d.extend(px.to_le_bytes());
            d.extend(py.to_le_bytes());
            d.extend(top.to_le_bytes());
            d.extend(left.to_le_bytes());
            d.extend(u16::from(active).to_le_bytes());
            o.rec(0x41, &d);
        }

        let sels: Vec<&Node> = view
            .map(|v| v.children("selection").collect())
            .unwrap_or_default();
        for sel in sels {
            let pnn = match sel.attr("pane") {
                Some("bottomRight") => 0u8,
                Some("topRight") => 1,
                Some("bottomLeft") => 2,
                _ => 3,
            };
            let act = sel
                .attr("activeCell")
                .and_then(parse_cell)
                .unwrap_or((0, 0));
            let rc = sel
                .attr("sqref")
                .and_then(|q| q.split(' ').next())
                .and_then(parse_rect)
                .unwrap_or(xlcalc::Rect::new(act.0, act.1, act.0, act.1));
            let mut d = vec![pnn];
            d.extend((act.0 as u16).to_le_bytes());
            d.extend((act.1 as u16).to_le_bytes());
            d.extend(0u16.to_le_bytes());
            d.extend(1u16.to_le_bytes());
            d.extend((rc.r0 as u16).to_le_bytes());
            d.extend((rc.r1 as u16).to_le_bytes());
            d.push(rc.c0 as u8);
            d.push(rc.c1 as u8);
            o.rec(0x1D, &d);
        }
    }

    if let Some(m) = root.child("mergeCells") {
        let list: Vec<_> = m
            .children("mergeCell")
            .filter_map(|c| c.attr("ref").and_then(parse_rect))
            .filter(|r| r.r1 <= 0xFFFF && r.c1 <= 255)
            .collect();
        for chunk in list.chunks(1026) {
            let mut d = Vec::new();
            d.extend((chunk.len() as u16).to_le_bytes());
            for r in chunk {
                for v in [r.r0, r.r1, r.c0, r.c1] {
                    d.extend((v as u16).to_le_bytes());
                }
            }
            o.rec(0xE5, &d);
        }
    }
    {
        let sst = cx.sst;
        let text_of = |r: u32, c: u32| super::wextras::cell_text(sh, sst, r, c);
        super::wextras::hyperlinks(&mut o, cx, sh, &text_of);
    }
    {
        let mut ordinal = 0u16;
        super::wcf::write(&mut o, cx, root, idx, &mut ordinal);
    }
    super::wextras::validations(&mut o, cx, sh, idx);
    if let Some(cn) = pr.and_then(|p| p.attr("codeName")) {
        let mut d = Vec::new();
        xl_string16(&mut d, cn);
        o.rec(0x1BA, &d);
    }
    super::wtable::write(&mut o, cx, sh, idx);
    o.rec(0x0A, &[]);
    Ok(SheetOut {
        bytes: o.b,
        patch: Vec::new(),
    })
}

struct Walk<'a> {
    sh: &'a SheetIn,
    idx: u32,
    members: HashMap<(u32, u32), (u8, u32, u32)>,

    kept: std::collections::HashSet<(u32, u32)>,
}

fn write_cell(o: &mut Out, cx: &mut Cx<'_>, w: &mut Walk<'_>, c: &Node, row: u32, col: u32) {
    let (sh, idx) = (w.sh, w.idx);
    let xf =
        (cx.styles.xf_base + c.attr("s").and_then(|v| v.parse::<u32>().ok()).unwrap_or(0)) as u16;
    let val = cell_value(c);
    let here = format!("{}{}", xlcalc::col_to_letters(col), row + 1);
    cx.st.row = row;
    let mut head = Vec::new();
    head.extend((row as u16).to_le_bytes());
    head.extend((col as u16).to_le_bytes());
    head.extend(xf.to_le_bytes());

    let mut formula: Option<enc::Rgce> = None;
    let mut keep = c.attr("cm").is_some();
    let mut unflagged = false;
    let volatile = c
        .child("f")
        .is_some_and(|f| flag(f, "ca") || flag(f, "aca"))
        || (c.attr("vm").is_some() && matches!(val, V::Err(_)));
    let mut after: Vec<(u16, Vec<u8>)> = Vec::new();
    if let Some(f) = c.child("f") {
        let text = f.text.trim();
        match f.attr("t").unwrap_or("normal") {
            "array" if !text.is_empty() => {
                let encoded = enc::encode(cx.names, cx.tables, cx.st, Some(idx), text, Cls::Value);

                let encoded = match encoded {
                    Err(e) if e.starts_with("LET is") || e.starts_with("LAMBDA is") => {
                        keep = true;
                        cx.warnings
                            .push(format!("{} {here}: {e}; the file holds #VALUE!", sh.name));
                        Ok(enc::Rgce {
                            rgce: vec![0x1C, 0x0F],
                            rgcb: Vec::new(),
                        })
                    }
                    other => other,
                };
                match encoded {
                    Ok(body) => {
                        if let Some(rc) = f.attr("ref").and_then(parse_rect) {
                            match from_biff12(&body.rgce, &body.rgcb) {
                                Ok((rgce, extra)) => {
                                    let mut d = Vec::new();
                                    ref8(&mut d, rc.r0, rc.r1, rc.c0, rc.c1);
                                    d.extend(u16::from(volatile).to_le_bytes());

                                    if rc.r1 > rc.r0 && rc.c1 > rc.c0 {
                                        unflagged = true;
                                    }
                                    d.extend(0u32.to_le_bytes());
                                    d.extend((rgce.len() as u16).to_le_bytes());
                                    d.extend(rgce);
                                    d.extend(extra);
                                    after.push((0x221, d));
                                    for r in rc.r0..=rc.r1 {
                                        for cc in rc.c0..=rc.c1 {
                                            if (r, cc) != (row, col) {
                                                w.members.insert((r, cc), (0x01, row, col));
                                            }
                                        }
                                    }
                                    formula = Some(enc::Rgce {
                                        rgce: {
                                            let mut v = vec![0x01];
                                            v.extend(row.to_le_bytes());
                                            v
                                        },
                                        rgcb: col.to_le_bytes().to_vec(),
                                    });
                                }
                                Err(e) => cx
                                    .warnings
                                    .push(format!("{} {here}: formula not carried: {e}", sh.name)),
                            }
                        }
                    }
                    Err(e) => cx
                        .warnings
                        .push(format!("{} {here}: formula not carried: {e}", sh.name)),
                }
            }
            "dataTable" => {
                let rect = f.attr("ref").and_then(parse_rect);
                let r1 = f.attr("r1").and_then(parse_cell);
                if let (Some(rc), Some(r1)) = (rect, r1) {
                    let r2 = f.attr("r2").and_then(parse_cell).unwrap_or((0, 0));
                    let mut fl = 0u16;
                    if flag(f, "dtr") {
                        fl |= 4;
                    }
                    if flag(f, "dt2D") {
                        fl |= 8;
                    }
                    let mut d = Vec::new();
                    ref8(&mut d, rc.r0, rc.r1, rc.c0, rc.c1);
                    d.extend(fl.to_le_bytes());
                    for v in [r1.0, r1.1, r2.0, r2.1] {
                        d.extend((v as u16).to_le_bytes());
                    }
                    after.push((0x236, d));
                    for r in rc.r0..=rc.r1 {
                        for cc in rc.c0..=rc.c1 {
                            if (r, cc) != (row, col) {
                                w.members.insert((r, cc), (0x02, row, col));
                            }
                        }
                    }
                    formula = Some(enc::Rgce {
                        rgce: {
                            let mut v = vec![0x02];
                            v.extend(row.to_le_bytes());
                            v
                        },
                        rgcb: col.to_le_bytes().to_vec(),
                    });
                }
            }
            "normal" if !text.is_empty() => {
                match enc::encode(cx.names, cx.tables, cx.st, Some(idx), text, Cls::Value) {
                    Ok(x) => formula = Some(x),

                    Err(e) if e.starts_with("LET is") || e.starts_with("LAMBDA is") => {
                        keep = true;
                        formula = Some(enc::Rgce {
                            rgce: vec![0x1C, 0x0F],
                            rgcb: Vec::new(),
                        });
                        cx.warnings
                            .push(format!("{} {here}: {e}; the file holds #VALUE!", sh.name));
                    }
                    Err(e) => cx
                        .warnings
                        .push(format!("{} {here}: formula not carried: {e}", sh.name)),
                }
            }
            _ => {}
        }
    } else if let Some(&(ptg, r0, c0)) = w.members.get(&(row, col)) {
        let mut rgce = vec![ptg];
        rgce.extend(r0.to_le_bytes());
        formula = Some(enc::Rgce {
            rgce,
            rgcb: c0.to_le_bytes().to_vec(),
        });
    }
    if keep {
        w.kept.insert((row, col));
    }
    let keep = keep
        || w.members
            .get(&(row, col))
            .is_some_and(|m| w.kept.contains(&(m.1, m.2)));
    let Some(fm) = formula else {
        plain_cell(o, head, &val);
        return;
    };
    let (rgce, extra) = match from_biff12(&fm.rgce, &fm.rgcb) {
        Ok(x) => x,
        Err(e) => {
            cx.warnings
                .push(format!("{} {here}: formula not carried: {e}", sh.name));

            (vec![0x1C, 0x0F], Vec::new())
        }
    };
    let mut d = head;
    let mut string: Option<&str> = None;
    match &val {
        V::Num(n) => d.extend(n.to_le_bytes()),
        V::Str(s) => {
            d.extend([0, 0, 0, 0, 0, 0, 0xFF, 0xFF]);
            string = Some(s);
        }
        V::Bool(b) => d.extend([1, 0, u8::from(*b), 0, 0, 0, 0xFF, 0xFF]),
        V::Err(e) => d.extend([2, 0, *e, 0, 0, 0, 0xFF, 0xFF]),
        V::None | V::Sst(_) => d.extend([3, 0, 0, 0, 0, 0, 0xFF, 0xFF]),
    }

    let bits = if keep && !unflagged { 0x20u16 } else { 0 } | u16::from(volatile);
    d.extend(bits.to_le_bytes());
    d.extend(0u32.to_le_bytes());
    d.extend((rgce.len() as u16).to_le_bytes());
    d.extend(rgce);
    d.extend(extra);
    o.rec(0x06, &d);
    for (id, data) in after {
        o.rec(id, &data);
    }
    if let Some(s) = string {
        let mut d = Vec::new();
        xl_string16(&mut d, s);
        o.rec(0x207, &d);
    }
}

fn plain_cell(o: &mut Out, mut head: Vec<u8>, val: &V) {
    match val {
        V::None => o.rec(0x201, &head),
        V::Num(n) => match rk(*n) {
            Some(r) => {
                head.extend(r.to_le_bytes());
                o.rec(0x27E, &head);
            }
            None => {
                head.extend(n.to_le_bytes());
                o.rec(0x203, &head);
            }
        },
        V::Bool(b) => {
            head.extend([u8::from(*b), 0]);
            o.rec(0x205, &head);
        }
        V::Err(e) => {
            head.extend([*e, 1]);
            o.rec(0x205, &head);
        }
        V::Str(s) => {
            xl_string16(&mut head, s);
            o.rec(0x204, &head);
        }
        V::Sst(i) => {
            head.extend(i.to_le_bytes());
            o.rec(0xFD, &head);
        }
    }
}
