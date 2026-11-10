use super::rd_xml;
use crate::ext_in::read_link;
use crate::package::{write_rels, Package, Rel};
use crate::xlsb_enc::{error_code, ExtBookEnt};
use crate::xlsb_rec::Recs;
use xlcalc::Value;

pub(super) struct ExtOut {
    pub ent: ExtBookEnt,
    pub bin: Vec<u8>,
    pub rels: Vec<u8>,
}

fn name_rgce(sheets: &[String], n: &xlcalc::ExtName) -> Vec<u8> {
    let Some(s) = sheets
        .iter()
        .position(|x| x.eq_ignore_ascii_case(&n.sheet))
        .map(|i| i as u16)
    else {
        return vec![0x1C, 0x17];
    };
    let r = n.rect;
    let (Ok(r0), Ok(r1), Ok(c0), Ok(c1)) = (
        u16::try_from(r.r0),
        u16::try_from(r.r1),
        u16::try_from(r.c0),
        u16::try_from(r.c1),
    ) else {
        return vec![0x1C, 0x17];
    };
    let mut v = Vec::new();
    if r.is_cell() {
        v.push(0x3A);
        for x in [s, s, r0, c0] {
            v.extend(x.to_le_bytes());
        }
    } else {
        v.push(0x3B);
        for x in [s, s, r0, r1, c0, c1] {
            v.extend(x.to_le_bytes());
        }
    }
    v
}

pub(super) fn read_all(
    pkg: &Package,
    book: &crate::xml::Node,
    target_of: &dyn Fn(&str) -> Option<String>,
) -> Result<Vec<ExtOut>, String> {
    let mut out = Vec::new();
    let Some(refs) = book.child("externalReferences") else {
        return Ok(out);
    };
    for (i, r) in refs.children("externalReference").enumerate() {
        let n = i + 1;
        let part = target_of(r.attr("id").unwrap_or(""))
            .ok_or_else(|| format!("external link {n}: no part"))?;
        let root = rd_xml(pkg, &part)?;
        let rels = pkg.rels(&part);
        let raw: Vec<(String, String, String)> = rels
            .iter()
            .map(|r| (r.id.clone(), r.ty.clone(), r.target.clone()))
            .collect();
        let link = read_link(&part, &root, &raw).ok_or_else(|| format!("external link {n}"))?;
        let book_node = root.child("externalBook");
        let main = book_node.and_then(|b| b.attr("id")).unwrap_or("rId1");
        let alt = book_node
            .and_then(|b| b.child("alternateUrls"))
            .and_then(|a| a.child("absoluteUrl"))
            .and_then(|a| a.attr("id"));
        let sheets: Vec<String> = link.sheets.iter().map(|s| s.name.clone()).collect();
        let mut r = Recs::default();
        r.with(360, |p| {
            p.u16(0).wstr(main).u32(0xFFFF_FFFF);
        });
        r.rec(37, &[0x01, 0x00, 0x06, 0x11, 0x00, 0x80]);
        if let Some(a) = alt {
            r.with(5108, |p| {
                p.u32(0).u32(0).wstr(a).u32(0);
            });
        }
        r.empty(38);
        r.with(359, |p| {
            p.u32(sheets.len() as u32);
            for s in &sheets {
                p.wstr(s);
            }
        });
        let mut names = link.names.clone();
        names.sort_by_key(|a| a.name.to_lowercase());
        for nm in &names {
            r.with(577, |p| {
                p.wstr(&nm.name);
            });
            let rgce = name_rgce(&sheets, nm);
            r.with(585, |p| {
                p.u32(rgce.len() as u32).raw(&rgce);
            });
            r.rec(586, &[0x40, 0, 0, 0, 0, 0, 0]);
            r.empty(587);
        }
        for (si, s) in link.sheets.iter().enumerate() {
            let mut cells: Vec<_> = s
                .cells
                .iter()
                .filter(|c| {
                    matches!(
                        c.2,
                        Value::Number(_) | Value::Bool(_) | Value::Error(_) | Value::Text(_)
                    )
                })
                .collect();
            if cells.is_empty() {
                continue;
            }
            cells.sort_by_key(|c| (c.0, c.1));
            r.with(363, |p| {
                p.u32(si as u32).u8(0);
            });
            let mut row = None;
            for (rw, col, v) in cells {
                if row != Some(*rw) {
                    r.with(366, |p| {
                        p.u32(*rw);
                    });
                    row = Some(*rw);
                }
                match v {
                    Value::Number(x) => r.with(368, |p| {
                        p.u32(*col).f64(*x);
                    }),
                    Value::Bool(b) => r.with(369, |p| {
                        p.u32(*col).u8(u8::from(*b));
                    }),
                    Value::Error(e) => {
                        let code = error_code(&e.to_string()).unwrap_or(0x0F);
                        r.with(370, |p| {
                            p.u32(*col).u8(code);
                        });
                    }
                    Value::Text(t) => r.with(371, |p| {
                        p.u32(*col).wstr(t);
                    }),
                    _ => {}
                }
            }
            r.empty(364);
        }
        r.empty(588);
        let rels_out: Vec<Rel> = rels;
        out.push(ExtOut {
            ent: ExtBookEnt {
                sheets,
                names: names.iter().map(|n| n.name.clone()).collect(),
            },
            bin: r.bytes,
            rels: write_rels(&rels_out),
        });
    }
    Ok(out)
}
