use super::formula::{self, Name};
use super::*;
use crate::slicers_xml::serial;
use crate::xlsb_enc::error_byte;
use crate::xlsb_rec::{Recs, P};
use crate::xml::Node;

fn on(n: &Node, key: &str, default: bool) -> bool {
    match n.attr(key) {
        Some("0" | "false") => false,
        Some(_) => true,
        None => default,
    }
}

fn int(n: &Node, key: &str, default: i64) -> i64 {
    n.attr(key)
        .and_then(|v| v.parse::<i64>().ok())
        .unwrap_or(default)
}

fn float(n: &Node, key: &str) -> Option<f64> {
    n.attr(key).and_then(|v| v.parse().ok())
}

fn date_parts(s: &str) -> Option<(u16, u16, u8, u8, u8, u8)> {
    let (d, t) = s.split_once('T').unwrap_or((s, "00:00:00"));
    let mut dp = d.split('-');
    let y = dp.next()?.parse().ok()?;
    let m = dp.next()?.parse().ok()?;
    let day = dp.next()?.parse().ok()?;
    let mut tp = t.split(':');
    let h = tp.next()?.parse().ok()?;
    let mi = tp.next().map_or(Some(0), |v| v.parse().ok())?;
    let sec = tp
        .next()
        .map_or(Some(0), |v| v.split('.').next()?.parse().ok())?;
    Some((y, m, day, h, mi, sec))
}

fn date_bytes(p: &mut P, s: &str) {
    let (y, m, d, h, mi, sec) = date_parts(s).unwrap_or((1900, 1, 1, 0, 0, 0));
    p.u16(y).u16(m).u8(d).u8(h).u8(mi).u8(sec);
}

fn item_kind(n: &Node) -> Option<(u32, Option<u16>)> {
    Some(match n.name.as_str() {
        "m" => (I_MISSING, None),
        "n" => (I_NUM, Some(1)),
        "b" => (I_BOOL, None),
        "e" => (I_ERR, Some(0x10)),
        "s" if n.attr("f").is_some_and(|v| v != "0") => (I_CALC, None),
        "s" => (I_STR, Some(2)),
        "d" => (I_DATE, Some(0x20)),
        _ => return None,
    })
}

fn item_value(p: &mut P, id: u32, n: &Node) {
    let v = n.attr("v").unwrap_or("");
    match id {
        I_NUM => {
            p.f64(v.parse().unwrap_or(0.0));
        }
        I_BOOL => {
            p.u8(u8::from(v == "1" || v.eq_ignore_ascii_case("true")));
        }
        I_ERR => {
            p.u8(error_byte(v).unwrap_or(0x2A));
        }
        I_STR => {
            p.wstr(v);
        }
        I_DATE => date_bytes(p, v),
        I_CALC => {
            p.wstr(v).raw(&[2, 0, 0, 0, 0, 0]);
        }
        _ => {}
    }
}

fn items(r: &mut Recs, kids: &[Node]) {
    let mut i = 0;
    while i < kids.len() {
        let Some((id, arr)) = item_kind(&kids[i]) else {
            i += 1;
            continue;
        };
        let mut j = i;
        if let Some(ty) = arr {
            while j < kids.len() && item_kind(&kids[j]).and_then(|k| k.1) == Some(ty) {
                j += 1;
            }
            if j - i >= 4 {
                r.with(R_ARRAY, |p| {
                    p.u16(ty).u32((j - i) as u32);
                    for k in &kids[i..j] {
                        item_value(p, item_kind(k).map_or(0, |x| x.0), k);
                    }
                });
                r.empty(R_ARRAY_END);
                i = j;
                continue;
            }
        }
        r.with(id, |p| item_value(p, id, &kids[i]));
        i += 1;
    }
}

fn label(n: &Node) -> String {
    match n.name.as_str() {
        "b" => {
            if n.attr("v") == Some("1") {
                "TRUE".into()
            } else {
                "FALSE".into()
            }
        }
        "m" => String::new(),
        _ => n.attr("v").unwrap_or("").to_string(),
    }
}

fn shared_flags(s: &Node) -> (u16, Option<(f64, f64)>) {
    let mut f = 0x400u16;
    for (key, bit, default) in [
        ("containsSemiMixedTypes", 1u16, true),
        ("containsNonDate", 2, true),
        ("containsDate", 4, false),
        ("containsString", 8, true),
        ("containsBlank", 0x10, false),
        ("containsMixedTypes", 0x20, false),
        ("containsNumber", 0x40, false),
        ("containsInteger", 0x80, false),
        ("longText", 0x200, false),
    ] {
        if on(s, key, default) {
            f |= bit;
        }
    }
    let mm = match (s.attr("minDate"), s.attr("maxDate")) {
        (Some(a), Some(b)) => serial(a).zip(serial(b)),
        _ => float(s, "minValue").zip(float(s, "maxValue")),
    };
    if mm.is_some() {
        f |= 0x100;
    }
    (f, mm)
}

fn group_base(name: &str) -> u8 {
    match name {
        "seconds" => 1,
        "minutes" => 2,
        "hours" => 3,
        "days" => 4,
        "months" => 5,
        "quarters" => 6,
        "years" => 7,
        _ => 0,
    }
}

pub(crate) fn cache_def_bin(def: &Node, rid: &str, warnings: &mut Vec<String>) -> Vec<u8> {
    let mut r = Recs::default();
    let fields: Vec<&Node> = def
        .child("cacheFields")
        .map_or_else(Vec::new, |c| c.children("cacheField").collect());
    let by = def.attr("refreshedBy");
    r.with(R_DEF, |p| {
        let mut f1 = 0u8;
        if on(def, "saveData", true) {
            f1 |= 1;
        }
        if on(def, "refreshOnLoad", false) {
            f1 |= 4;
        }
        if on(def, "optimizeMemory", false) {
            f1 |= 8;
        }
        if on(def, "enableRefresh", true) {
            f1 |= 0x10;
        }
        if on(def, "backgroundQuery", false) {
            f1 |= 0x20;
        }
        p.u8(int(def, "refreshedVersion", 8) as u8)
            .u8(int(def, "minRefreshableVersion", 3) as u8)
            .u8(int(def, "createdVersion", 8) as u8)
            .u8(f1)
            .raw(&(-1i32).to_le_bytes())
            .f64(float(def, "refreshedDate").unwrap_or(0.0))
            .u8(2 | u8::from(by.is_some()))
            .u32(int(def, "recordCount", 0) as u32);
        if let Some(b) = by {
            p.wstr(b);
        }
        p.wstr(rid);
    });
    r.with(R_SRC, |p| {
        p.u32(0).u32(0);
    });
    match def
        .child("cacheSource")
        .and_then(|s| s.child("worksheetSource"))
    {
        Some(ws) if ws.attr("name").is_some() && ws.attr("ref").is_none() => {
            r.with(R_SHEETSRC, |p| {
                p.u8(1).u8(0).u8(0).wstr(ws.attr("name").unwrap_or(""));
            });
        }
        Some(ws) => {
            let rect = ws.attr("ref").and_then(crate::parse_rect);
            if rect.is_none() {
                warnings.push("pivot cache: source range not understood".into());
            }
            let rc = rect.unwrap_or(xlcalc::Rect::new(0, 0, 0, 0));
            r.with(R_SHEETSRC, |p| {
                p.u8(0).u8(0).u8(2).wstr(ws.attr("sheet").unwrap_or(""));
                p.u32(rc.r0).u32(rc.r1).u32(rc.c0).u32(rc.c1);
            });
        }
        None => warnings.push("pivot cache: no worksheet source".into()),
    }
    r.empty(R_SHEETSRC_END);
    r.empty(R_SRC_END);

    let names: Vec<&str> = fields
        .iter()
        .map(|f| f.attr("name").unwrap_or(""))
        .collect();
    r.with(R_FIELDS, |p| {
        p.u32(fields.len() as u32);
    });

    let shared: Vec<Vec<String>> = fields
        .iter()
        .map(|f| {
            f.child("sharedItems")
                .map_or_else(Vec::new, |s| s.kids.iter().map(label).collect())
        })
        .collect();
    for f in &fields {
        let has_formula = f.attr("formula").is_some();
        let name = f.attr("name").unwrap_or("");
        let group = f.child("fieldGroup");
        let mut flags = 0u16;
        if on(f, "databaseField", true) {
            flags |= 4;
        }
        if f.attr("caption").is_some() {
            flags |= 8;
        }
        let compiled = if has_formula {
            flags |= 0x100;
            let text = f.attr("formula").unwrap_or("");
            let res = |n: &str, item: Option<&str>| {
                if item.is_some() {
                    return None;
                }
                names.iter().position(|x| *x == n).map(Name::Field)
            };
            match formula::encode(text, &res) {
                Ok(c) => Some(c),
                Err(e) => {
                    warnings.push(format!("pivot calculated field {name}: {e}"));
                    flags &= !0x100;
                    None
                }
            }
        } else {
            None
        };
        r.with(R_FIELD, |p| {
            p.u16(flags)
                .u32(int(f, "numFmtId", 0) as u32)
                .u16(0)
                .u32(0)
                .u32(0)
                .u32(0)
                .wstr(name);
            if let Some(c) = f.attr("caption") {
                p.wstr(c);
            }
            if let Some((rgce, _)) = &compiled {
                p.u32(rgce.len() as u32).raw(rgce).u32(0);
            }
        });
        if let Some(s) = f.child("sharedItems") {
            let (fl, mm) = shared_flags(s);
            r.with(R_SHARED, |p| {
                p.u16(fl).u32(s.kids.len() as u32);
                if let Some((lo, hi)) = mm {
                    p.f64(lo).f64(hi);
                }
            });
            items(&mut r, &s.kids);
            r.empty(R_SHARED_END);
        }
        if let Some(g) = group {
            r.with(R_GROUP, |p| {
                p.u32(int(g, "par", -1) as u32)
                    .u32(int(g, "base", -1) as u32);
            });
            if let Some(rp) = g.child("rangePr") {
                let by = group_base(rp.attr("groupBy").unwrap_or("range"));
                let (lo, hi) = if by > 0 {
                    (
                        rp.attr("startDate").and_then(serial).unwrap_or(0.0),
                        rp.attr("endDate").and_then(serial).unwrap_or(0.0),
                    )
                } else {
                    (
                        float(rp, "startNum").unwrap_or(0.0),
                        float(rp, "endNum").unwrap_or(0.0),
                    )
                };
                let mut fl = 0u8;
                if on(rp, "autoStart", true) {
                    fl |= 1;
                }
                if on(rp, "autoEnd", true) {
                    fl |= 2;
                }
                if by > 0 {
                    fl |= 4;
                }
                r.with(R_RANGEPR, |p| {
                    p.u8(by)
                        .u8(fl)
                        .f64(lo)
                        .f64(hi)
                        .f64(float(rp, "groupInterval").unwrap_or(1.0));
                });
                r.empty(R_RANGEPR_END);
            }
            if let Some(dp) = g.child("discretePr") {
                r.with(R_DISCRETE, |p| {
                    p.u32(dp.kids.len() as u32);
                });
                for x in &dp.kids {
                    r.with(R_DISCRETE_X, |p| {
                        p.u32(int(x, "v", 0) as u32);
                    });
                }
                r.empty(R_DISCRETE_END);
            }
            if let Some(gi) = g.child("groupItems") {
                r.with(R_GITEMS, |p| {
                    p.u32(gi.kids.len() as u32);
                });
                items(&mut r, &gi.kids);
                r.empty(R_GITEMS_END);
            }
            r.empty(R_GROUP_END);
        }
        if let Some((_, used)) = &compiled {
            r.with(R_TUPLES, |p| {
                p.u32(used.len() as u32);
            });
            for u in used {
                if let Name::Field(i) = u {
                    r.with(R_TUPLE, |p| {
                        p.u32(*i as u32).u16(1);
                    });
                    r.empty(R_TUPLE_END);
                }
            }
            r.empty(R_TUPLES_END);
        }
        r.empty(R_FIELD_END);
    }
    r.empty(R_FIELDS_END);

    let mut calc = Recs::default();
    let mut ncalc = 0u32;
    if let Some(ci) = def.child("calculatedItems") {
        for item in ci.children("calculatedItem") {
            let text = item.attr("formula").unwrap_or("");
            let area = item
                .child("pivotArea")
                .and_then(|a| a.child("references"))
                .and_then(|a| a.child("reference"));
            let fld = area.map(|a| int(a, "field", 0) as usize);
            let x = area
                .and_then(|a| a.child("x"))
                .map(|x| int(x, "v", 0) as u32);
            let (Some(fld), Some(x)) = (fld, x) else {
                warnings.push("pivot calculated item: no target".into());
                continue;
            };
            let res = |n: &str, item: Option<&str>| {
                let f = names.iter().position(|x| *x == n)?;
                let it = shared[f].iter().position(|l| Some(l.as_str()) == item)?;
                Some(Name::Item(f, it))
            };
            let (rgce, used) = match formula::encode(text, &res) {
                Ok(c) => c,
                Err(e) => {
                    warnings.push(format!("pivot calculated item: {e}"));
                    continue;
                }
            };
            ncalc += 1;
            calc.with(R_CALCMEM, |p| {
                p.u32(0xFFFF_FFFF).u32(rgce.len() as u32).raw(&rgce).u32(0);
            });
            calc.rec(R_AREA, &[0xFF, 0xFF, 0xFF, 0xFF, 1, 0x11, 0, 0]);
            calc.with(R_REFS, |p| {
                p.u32(1);
            });
            calc.with(R_REF, |p| {
                p.u32(fld as u32).u32(1).raw(&[1, 0, 1]);
            });
            calc.with(R_REF_X, |p| {
                p.u32(x);
            });
            calc.empty(R_REF_X_END);
            calc.empty(R_REF_END);
            calc.empty(R_REFS_END);
            calc.empty(R_AREA_END);
            calc.with(R_TUPLES, |p| {
                p.u32(used.len() as u32);
            });
            for u in &used {
                if let Name::Item(f, i) = u {
                    calc.rec(R_TUPLE, &[0xFF, 0xFF, 0xFF, 0xFF, 1, 0]);
                    calc.with(R_MEMBERS, |p| {
                        p.u32(1);
                    });
                    calc.with(R_TMEMBER, |p| {
                        p.u8(0).u32(*f as u32).u32(*i as u32);
                    });
                    calc.empty(R_TMEMBER_END);
                    calc.empty(R_MEMBERS_END);
                    calc.empty(R_TUPLE_END);
                }
            }
            calc.empty(R_TUPLES_END);
            calc.empty(R_CALCMEM_END);
        }
    }
    if ncalc > 0 {
        r.with(R_CALCS, |p| {
            p.u32(ncalc);
        });
        r.bytes.extend(calc.bytes);
        r.empty(R_CALCS_END);
    }

    r.rec(FRT_BEGIN, &[2, 0x0E, 0, 0]);
    r.rec(CACHE_FRT, &[0; 9]);
    r.empty(CACHE_FRT_END);
    r.empty(FRT_END);
    r.rec(FRT_BEGIN, &[0x54, 0, 2, 0]);
    r.rec(CACHE_CLEAN, &[1]);
    r.empty(FRT_END);
    r.empty(R_DEF_END);
    r.bytes
}

pub(crate) fn cache_records_bin(records: &Node, def: &Node) -> Vec<u8> {
    let fields: Vec<&Node> = def
        .child("cacheFields")
        .map_or_else(Vec::new, |c| c.children("cacheField").collect());

    let columns: Vec<(bool, u32)> = fields
        .iter()
        .filter(|f| on(f, "databaseField", true) && f.attr("formula").is_none())
        .map(|f| {
            let s = f.child("sharedItems");
            let shared = s.is_some_and(|s| s.attr("count").is_some() || !s.kids.is_empty());
            let kind = match s {
                Some(s) if on(s, "containsNumber", false) => I_NUM,
                Some(s) if on(s, "containsDate", false) => I_DATE,
                _ => I_STR,
            };
            (shared, kind)
        })
        .collect();
    let mut r = Recs::default();
    let rows: Vec<&Node> = records.children("r").collect();
    r.with(REC_BEGIN, |p| {
        p.u32(rows.len() as u32);
    });
    for row in rows {
        let typed = row.kids.iter().enumerate().any(|(i, v)| {
            v.name != "x"
                && item_kind(v).is_some_and(|(id, _)| columns.get(i).is_some_and(|c| c.1 != id))
        });
        if typed {
            r.empty(REC_TYPED_ROW);
            for v in &row.kids {
                if v.name == "x" {
                    r.with(R_INDEX, |p| {
                        p.u32(int(v, "v", 0) as u32);
                    });
                } else if let Some((id, _)) = item_kind(v) {
                    r.with(id, |p| item_value(p, id, v));
                }
            }
            continue;
        }
        r.with(REC_ROW, |p| {
            for v in &row.kids {
                if v.name == "x" {
                    p.u32(int(v, "v", 0) as u32);
                } else if let Some((id, _)) = item_kind(v) {
                    item_value(p, id, v);
                }
            }
        });
    }
    r.empty(REC_END);
    r.bytes
}

fn item_type(t: &str) -> u8 {
    ITEM_TYPES
        .iter()
        .position(|x| *x == t)
        .map_or(0, |i| i as u8)
}

pub(crate) fn table_bin(t: &Node, cache_id: u32, warnings: &mut Vec<String>) -> Vec<u8> {
    let mut r = Recs::default();
    let (loc_r0, loc_c0) = t
        .child("location")
        .and_then(|l| l.attr("ref"))
        .and_then(crate::parse_rect)
        .map_or((0, 0), |x| (x.r0, x.c0));
    r.with(T_DEF, |p| {
        let f1 = 0x0040_4108u32 | ((int(t, "indent", 0) as u32 & 0x7F) << 24);
        let mut f2 = 0x200B_65F0u32;
        let mut f3 = 0x2DFu32;
        if !on(t, "rowGrandTotals", true) {
            f2 &= !0x2000;
        }
        if !on(t, "colGrandTotals", true) {
            f2 &= !0x4000;
        }
        if !on(t, "useAutoFormatting", false) {
            f2 &= !0x100;
        }
        if !on(t, "itemPrintTitles", false) {
            f2 &= !0x2_0000;
        }
        if !on(t, "outline", false) {
            f2 &= !0x1_0000;
        }
        if !on(t, "compact", true) {
            f3 &= !0x8;
        }
        if !on(t, "compactData", true) {
            f3 &= !0x1;
        }
        if !on(t, "outlineData", false) {
            f3 &= !0x6;
        }
        let strings = [
            ("dataCaption", 2, 0x8_0000u32),
            ("grandTotalCaption", 2, 0x10_0000),
            ("errorCaption", 3, 0x40),
            ("missingCaption", 3, 0x80),
            ("pageStyle", 2, 0x20_0000),
            ("pivotTableStyle", 2, 0x40_0000),
            ("vacatedStyle", 2, 0x80_0000),
            ("tag", 2, 0x4000_0000),
            ("colHeaderCaption", 3, 0x800),
            ("rowHeaderCaption", 3, 0x400),
        ];

        for (key, word, bit) in strings {
            let has = t.attr(key).is_some();
            let inverted = word == 3 && (bit == 0x40 || bit == 0x80);
            let target = if word == 2 { &mut f2 } else { &mut f3 };
            if has != inverted {
                *target |= bit;
            } else {
                *target &= !bit;
            }
        }
        p.u32(f1)
            .u32(f2)
            .u32(f3)
            .u8(if on(t, "dataOnRows", false) { 1 } else { 2 })
            .u8(0)
            .u8(int(t, "updatedVersion", 8) as u8)
            .u8(int(t, "minRefreshableVersion", 3) as u8)
            .raw(&(-1i32).to_le_bytes())
            .u16(1)
            .u16(0)
            .u32(0)
            .u32(cache_id)
            .wstr(t.attr("name").unwrap_or(""));
        for (key, _, _) in strings {
            if let Some(s) = t.attr(key) {
                p.wstr(s);
            }
        }
    });
    if let Some(l) = t.child("location") {
        let rc = l
            .attr("ref")
            .and_then(crate::parse_rect)
            .unwrap_or(xlcalc::Rect::new(0, 0, 0, 0));
        r.with(T_LOC, |p| {
            p.u32(rc.r0)
                .u32(rc.r1)
                .u32(rc.c0)
                .u32(rc.c1)
                .u32(loc_r0 + int(l, "firstHeaderRow", 0) as u32)
                .u32(loc_r0 + int(l, "firstDataRow", 0) as u32)
                .u32(loc_c0 + int(l, "firstDataCol", 0) as u32)
                .u32(int(l, "rowPageCount", 0) as u32)
                .u32(int(l, "colPageCount", 0) as u32);
        });
        r.empty(T_LOC_END);
    }
    let pfs: Vec<&Node> = t
        .child("pivotFields")
        .map_or_else(Vec::new, |c| c.children("pivotField").collect());
    r.with(T_FIELDS, |p| {
        p.u32(pfs.len() as u32);
    });
    for f in pfs {
        let mut f1 = match f.attr("axis") {
            Some("axisRow") => 1u32,
            Some("axisCol") => 2,
            Some("axisPage") => 4,
            Some("axisValues") => 3,
            _ => 0,
        };
        if on(f, "dataField", false) {
            f1 |= 8;
        }
        if on(f, "compact", true) {
            f1 |= 0x1000_0000;
        }
        let mut custom = false;
        for (i, a) in SUBTOTAL_ATTRS.iter().enumerate() {
            if on(f, a, false) {
                f1 |= 0x200 << i;
                custom = true;
            }
        }

        if on(f, "defaultSubtotal", true) && !custom {
            f1 |= 0x100;
        }
        if f.attr("name").is_some() {
            f1 |= 0x2000_0000;
        }
        let mut f2 = 0x04_A15Fu32;
        for (key, bit) in [("dragToRow", 1u32), ("dragToCol", 2), ("dragToPage", 4)] {
            if !on(f, key, true) {
                f2 &= !bit;
            }
        }
        if on(f, "showAll", true) {
            f2 |= 0x20;
        }
        if !on(f, "outline", true) {
            f2 &= !0x40;
        }
        if on(f, "insertBlankRow", false) {
            f2 |= 0x80;
        }
        if !on(f, "subtotalTop", true) {
            f2 &= !0x100;
        }
        match f.attr("sortType") {
            Some("ascending") => f2 |= 0x1000,
            Some("descending") => {
                f2 |= 0x1000;
                f2 &= !0x2000;
            }
            _ => {}
        }
        if on(f, "multipleItemSelectionAllowed", false) {
            f2 |= 0x8_0000;
        }
        r.with(T_FIELD, |p| {
            p.u32(f1)
                .u32(int(f, "numFmtId", 0) as u32)
                .u32(f2)
                .u32(10)
                .raw(&(-1i32).to_le_bytes());
            if let Some(n) = f.attr("name") {
                p.wstr(n);
            }
        });
        if let Some(its) = f.child("items") {
            r.with(T_ITEMS, |p| {
                p.u32(its.kids.len() as u32);
            });
            for it in its.children("item") {
                let ty = item_type(it.attr("t").unwrap_or("data"));
                let mut fl = 0u16;
                if on(it, "h", false) {
                    fl |= 1;
                }
                if !on(it, "sd", true) {
                    fl |= 2;
                }
                if on(it, "f", false) {
                    fl |= 4;
                }
                if it.attr("n").is_some() {
                    fl |= 0x10;
                }
                let x = if ty == 0 {
                    int(it, "x", 0) as u32
                } else {
                    u32::MAX
                };
                r.with(T_ITEM, |p| {
                    p.u8(ty).u16(fl).u32(x);
                    if let Some(n) = it.attr("n") {
                        p.wstr(n);
                    }
                });
                r.empty(T_ITEM_END);
            }
            r.empty(T_ITEMS_END);
        }
        if let Some(sort) = f.child("autoSortScope") {
            let d = sort
                .child("pivotArea")
                .and_then(|a| a.child("references"))
                .and_then(|a| a.child("reference"))
                .and_then(|a| a.child("x"))
                .map_or(0, |x| int(x, "v", 0) as u32);
            r.empty(T_SORT);
            r.rec(R_AREA, &[0xFF, 0xFF, 0xFF, 0xFF, 1, 0, 0, 0]);
            r.with(R_REFS, |p| {
                p.u32(1);
            });
            r.rec(R_REF, &[0xFE, 0xFF, 0xFF, 0xFF, 1, 0, 0, 0, 1, 0, 0]);
            r.with(R_REF_X, |p| {
                p.u32(d);
            });
            r.empty(R_REF_X_END);
            r.empty(R_REF_END);
            r.empty(R_REFS_END);
            r.empty(R_AREA_END);
            r.empty(T_SORT_END);
        }
        if on(f, "fillDownLabels", false) {
            r.rec(FRT_BEGIN, &[2, 0x0E, 0, 0]);
            r.with(T_FILL, |p| {
                p.u32(0).u32(1).u32(0);
            });
            r.empty(FRT_END);
        }
        r.empty(T_FIELD_END);
    }
    r.empty(T_FIELDS_END);

    let axis = |r: &mut Recs, tag: &str, begin: u32, end: u32| {
        if let Some(n) = t.child(tag) {
            r.with(begin, |p| {
                p.u32(n.kids.len() as u32);
                for f in n.children("field") {
                    p.u32(int(f, "x", 0) as u32);
                }
            });
            r.empty(end);
        }
    };
    let lines = |r: &mut Recs, tag: &str, begin: u32, end: u32| {
        if let Some(n) = t.child(tag) {
            r.with(begin, |p| {
                p.u32(n.kids.len() as u32);
            });
            for i in n.children("i") {
                r.with(T_LINE, |p| {
                    p.u16(int(i, "r", 0) as u16)
                        .u16(u16::from(item_type(i.attr("t").unwrap_or("data"))))
                        .u32(i.kids.len() as u32)
                        .u32(int(i, "i", 0) as u32);
                });
                if !i.kids.is_empty() {
                    r.with(T_LINE_X, |p| {
                        for x in i.children("x") {
                            p.u32(int(x, "v", 0) as u32);
                        }
                    });
                    r.empty(T_LINE_X_END);
                }
                r.empty(T_LINE_END);
            }
            r.empty(end);
        }
    };
    axis(&mut r, "rowFields", T_ROWFIELDS, T_ROWFIELDS_END);
    lines(&mut r, "rowItems", T_ROWITEMS, T_ROWITEMS_END);
    axis(&mut r, "colFields", T_COLFIELDS, T_COLFIELDS_END);
    lines(&mut r, "colItems", T_COLITEMS, T_COLITEMS_END);
    if let Some(pf) = t.child("pageFields") {
        r.with(T_PAGES, |p| {
            p.u32(pf.kids.len() as u32);
        });
        for f in pf.children("pageField") {
            r.with(T_PAGE, |p| {
                p.u32(int(f, "fld", 0) as u32)
                    .u32(
                        f.attr("item")
                            .map_or(PAGE_ALL, |v| v.parse().unwrap_or(PAGE_ALL)),
                    )
                    .u32(int(f, "hier", -1) as u32)
                    .u8(u8::from(f.attr("name").is_some()));
                if let Some(n) = f.attr("name") {
                    p.wstr(n);
                }
            });
            r.empty(T_PAGE_END);
        }
        r.empty(T_PAGES_END);
    }
    if let Some(df) = t.child("dataFields") {
        r.with(T_DATAS, |p| {
            p.u32(df.kids.len() as u32);
        });
        for f in df.children("dataField") {
            let sub = f.attr("subtotal").unwrap_or("sum");
            let show = f.attr("showDataAs").unwrap_or("normal");
            let s = SUBTOTALS.iter().position(|x| *x == sub).unwrap_or(0);
            let sh = SHOW_AS.iter().position(|x| *x == show).unwrap_or_else(|| {
                warnings.push(format!("pivot data field show-as {show} not written"));
                0
            });
            r.with(T_DATA, |p| {
                p.u32(int(f, "fld", 0) as u32)
                    .u32(s as u32)
                    .u32(sh as u32)
                    .u32(int(f, "baseField", 0) as u32)
                    .u32(int(f, "baseItem", 0) as u32)
                    .u32(int(f, "numFmtId", 0) as u32)
                    .u8(u8::from(f.attr("name").is_some()));
                if let Some(n) = f.attr("name") {
                    p.wstr(n);
                }
            });
            r.empty(T_DATA_END);
        }
        r.empty(T_DATAS_END);
    }
    if let Some(st) = t.child("pivotTableStyleInfo") {
        let mut fl = 1u16;
        for (key, bit) in [
            ("showRowHeaders", 0x10u16),
            ("showColHeaders", 0x20),
            ("showRowStripes", 4),
            ("showColStripes", 8),
            ("showLastColumn", 2),
        ] {
            if on(st, key, false) {
                fl |= bit;
            }
        }
        r.with(T_STYLE, |p| {
            p.u16(fl).wstr(st.attr("name").unwrap_or(""));
        });
    }

    r.rec(FRT_BEGIN, &[2, 0x0E, 0, 0]);
    let mut ext = [0xFFu8; 21];
    ext[..9].fill(0);
    r.rec(TABLE_FRT, &ext);
    r.empty(TABLE_FRT_END);
    r.empty(FRT_END);
    r.rec(FRT_BEGIN, &[3, 0x11, 0, 0]);
    r.rec(TABLE_FRT2, &[1, 1, 0]);
    r.empty(TABLE_FRT2_END);
    r.empty(FRT_END);
    r.empty(T_DEF_END);
    r.bytes
}
