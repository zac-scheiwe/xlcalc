use super::ptg_out::{from_biff12, put_chars};
use super::rtable::FN_NAMES;
use super::write::Out;
use super::wsheet::{Cx, SheetIn};
use crate::parse_rect;
use crate::xlsb_enc as enc;
use crate::xml::Node;
use xlcalc::FormulaContext;

fn long_str(d: &mut Vec<u8>, s: &str) {
    d.extend((xlcalc::wide::units(s).count().min(32767) as u16).to_le_bytes());
    put_chars(d, s);
}

fn ref8u(d: &mut Vec<u8>, r: (u32, u32, u32, u32)) {
    for v in [r.0, r.2, r.1, r.3] {
        d.extend((v as u16).to_le_bytes());
    }
}

fn frt(d: &mut Vec<u8>, rt: u16, grbit: u16) {
    d.extend(rt.to_le_bytes());
    d.extend(grbit.to_le_bytes());
}

pub(super) fn next_id(tables: &[crate::xlsb_enc::TableEnt]) -> u32 {
    tables.iter().map(|t| t.id).max().unwrap_or(0) + 1
}

pub(super) fn write(o: &mut Out, cx: &mut Cx<'_>, sh: &SheetIn, idx: u32) {
    if sh.tables.is_empty() {
        return;
    }
    let mut d = Vec::new();
    frt(&mut d, 0x871, 0);
    d.extend([0u8; 8]);
    d.extend(5u16.to_le_bytes());
    d.push(1);
    d.extend(0xFFFF_FFFFu32.to_le_bytes());
    d.extend(0xFFFF_FFFFu32.to_le_bytes());
    d.extend(next_id(cx.tables).to_le_bytes());
    d.extend(0u16.to_le_bytes());
    o.rec(0x871, &d);
    let mut order: Vec<&Node> = sh.tables.iter().collect();
    order.sort_by_key(|t| {
        t.attr("id")
            .and_then(|v| v.parse::<u32>().ok())
            .unwrap_or(0)
    });
    for t in order {
        table(o, cx, t, idx);
    }
}

fn table(o: &mut Out, cx: &mut Cx<'_>, t: &Node, idx: u32) {
    let Some(rc) = t.attr("ref").and_then(parse_rect) else {
        return;
    };
    let rect = (rc.r0, rc.c0, rc.r1, rc.c1);
    let id: u32 = t.attr("id").and_then(|v| v.parse().ok()).unwrap_or(1);
    let name = t.attr("name").unwrap_or("");
    let display = t.attr("displayName").unwrap_or(name);
    let header = t.attr("headerRowCount") != Some("0");
    let totals = t
        .attr("totalsRowCount")
        .and_then(|v| v.parse::<u32>().ok())
        .unwrap_or(0)
        .min(1);
    let af = t.child("autoFilter").is_some();
    let cols: Vec<&Node> = t
        .child("tableColumns")
        .map(|c| c.children("tableColumn").collect())
        .unwrap_or_default();
    struct F {
        id: u32,
        name: String,
        ilta: u32,
        flags: u32,
        label: Option<String>,
        formula: Option<Vec<u8>>,

        agg: Vec<u8>,
        data: Vec<u8>,
    }
    let mut fields = Vec::new();
    for c in &cols {
        let mut f = F {
            id: c.attr("id").and_then(|v| v.parse().ok()).unwrap_or(0),
            name: c.attr("name").unwrap_or("").to_string(),
            ilta: 0,
            flags: u32::from(af),
            label: None,
            formula: None,
            agg: Vec::new(),
            data: Vec::new(),
        };
        let dxfn = |key: &str| {
            c.attr(key)
                .and_then(|i| i.parse::<usize>().ok())
                .and_then(|i| cx.dxfs.get(i))
                .map_or_else(Vec::new, |d| super::wcf::dxfn_list(d, cx))
        };
        f.agg = dxfn("totalsRowDxfId");
        f.data = dxfn("dataDxfId");
        if let Some(func) = c.attr("totalsRowFunction") {
            f.ilta = FN_NAMES.iter().position(|n| *n == func).unwrap_or(0) as u32;
        }
        if c.child("calculatedColumnFormula").is_some() {
            f.flags |= 0x800;
        }
        if let Some(l) = c.attr("totalsRowLabel") {
            f.flags |= 0x400;
            f.label = Some(l.to_string());
        }
        if f.ilta == 9 {
            if let Some(ft) = c.child("totalsRowFormula") {
                (cx.st.row, cx.st.col) = (rc.r1, rc.c0);
                let r = enc::encode(
                    cx.names,
                    cx.tables,
                    cx.st,
                    Some(idx),
                    ft.text.trim(),
                    FormulaContext::Cell,
                )
                .and_then(|e| from_biff12(&e.rgce, &e.rgcb));
                match r {
                    Ok((rgce, _)) => {
                        f.flags |= 0x80;
                        f.formula = Some(rgce);
                    }
                    Err(e) => cx
                        .warnings
                        .push(format!("table {name}: totals formula not written: {e}")),
                }
            }
        }
        fields.push(f);
    }
    let v12 = !header || fields.iter().any(|f| f.flags & 0x480 != 0);
    let rt: u16 = if v12 { 0x878 } else { 0x872 };
    let mut d = Vec::new();
    frt(&mut d, rt, 1);
    ref8u(&mut d, rect);
    d.extend(5u16.to_le_bytes());
    d.push(0);
    d.extend(0u32.to_le_bytes());
    d.extend(1u16.to_le_bytes());
    d.extend(0u32.to_le_bytes());
    d.extend(0u16.to_le_bytes());
    ref8u(&mut d, rect);
    d.extend(0u32.to_le_bytes());
    d.extend(id.to_le_bytes());
    d.extend(u32::from(header).to_le_bytes());
    d.extend(totals.to_le_bytes());
    let next = fields.iter().map(|f| f.id).max().unwrap_or(0) + 1;
    d.extend(next.to_le_bytes());
    d.extend(0x40u32.to_le_bytes());
    d.extend(0x773Fu16.to_le_bytes());
    d.extend(0u16.to_le_bytes());
    let flags = 0x00DF_0800u32 | if af { 0x06 } else { 0 } | if totals > 0 { 0x40 } else { 0 };
    d.extend(flags.to_le_bytes());
    d.extend([0u8; 32]);
    long_str(&mut d, name);
    d.extend((fields.len() as u16).to_le_bytes());
    long_str(&mut d, &id.to_string());
    for (i, f) in fields.iter().enumerate() {
        d.extend(f.id.to_le_bytes());
        d.extend([0u8; 8]);
        d.extend(f.ilta.to_le_bytes());
        d.extend((f.agg.len() as u32).to_le_bytes());
        d.extend(0xFFFF_FFFFu32.to_le_bytes());
        d.extend(f.flags.to_le_bytes());
        d.extend((f.data.len() as u32).to_le_bytes());
        d.extend(0xFFFF_FFFFu32.to_le_bytes());
        long_str(&mut d, &(i + 1).to_string());
        long_str(&mut d, &f.name);
        d.extend(&f.agg);
        d.extend(&f.data);
        if header && f.flags & 1 != 0 {
            d.extend([0, 0, 0, 0, 0xFF, 0xFF]);
        }
        if let Some(l) = &f.label {
            long_str(&mut d, l);
        }
        if let Some(g) = &f.formula {
            d.extend((g.len() as u16).to_le_bytes());
            d.extend(g);
        }
        if !header {
            d.extend([0u8; 4]);
        }
    }
    o.rec(rt, &d);

    let mut d = Vec::new();
    frt(&mut d, 0x877, 0);
    d.extend([0u8; 8]);
    d.extend(0u16.to_le_bytes());
    d.extend(id.to_le_bytes());
    for _ in 0..3 {
        d.extend(0u32.to_le_bytes());
        d.extend(0xFFFF_FFFFu32.to_le_bytes());
    }
    d.extend([0u8; 12]);
    o.rec(0x877, &d);
    let mut d = Vec::new();
    frt(&mut d, 0x877, 0);
    d.extend([0u8; 8]);
    d.extend(1u16.to_le_bytes());
    d.extend(id.to_le_bytes());
    let st = t.child("tableStyleInfo");
    let sname = st.and_then(|s| s.attr("name")).unwrap_or("");
    let mut fl = if sname.is_empty() { 0u16 } else { 0x40 };
    if let Some(s) = st {
        for (bit, key) in [
            (1u16, "showFirstColumn"),
            (2, "showLastColumn"),
            (4, "showRowStripes"),
            (8, "showColumnStripes"),
        ] {
            if matches!(s.attr(key), Some("1" | "true")) {
                fl |= bit;
            }
        }
    }
    d.extend(fl.to_le_bytes());
    long_str(&mut d, sname);
    o.rec(0x877, &d);
    let mut d = Vec::new();
    frt(&mut d, 0x877, 0);
    d.extend([0u8; 8]);
    d.extend(2u16.to_le_bytes());
    d.extend(id.to_le_bytes());
    long_str(&mut d, display);
    long_str(&mut d, "");
    o.rec(0x877, &d);
}
