use super::write::Out;
use crate::xml::Node;

pub(super) struct NoteIn {
    pub row: u32,
    pub col: u32,
    pub author: String,
    pub text: String,
    pub visible: bool,

    pub anchor: Option<[u16; 8]>,
}

pub(super) fn read(comments: &Node, vml: Option<&str>, dims: &super::wdraw::Dims) -> Vec<NoteIn> {
    let authors: Vec<String> = comments
        .child("authors")
        .map(|a| a.children("author").map(|x| x.text.clone()).collect())
        .unwrap_or_default();

    let mut shown: Vec<(u32, u32, bool, Option<[u16; 8]>)> = Vec::new();
    if let Some(v) = vml {
        let chunks: Vec<&str> = v.split("<x:ClientData").collect();
        for (ci, part) in chunks.iter().enumerate().skip(1) {
            let kept: Option<Vec<u16>> = chunks[ci - 1].rfind("alt=\"xlanchor:").and_then(|i| {
                let s = &chunks[ci - 1][i + 14..];
                let w: Vec<u16> = s
                    .split('"')
                    .next()?
                    .split(',')
                    .filter_map(|x| x.parse().ok())
                    .collect();
                (w.len() == 8).then_some(w)
            });
            let body = part.split("</x:ClientData>").next().unwrap_or("");
            if !body.contains("ObjectType=\"Note\"") {
                continue;
            }
            let get = |tag: &str| -> Option<u32> {
                let open = format!("<x:{tag}>");
                let at = body.find(&open)? + open.len();
                body[at..].split('<').next()?.trim().parse().ok()
            };
            let visible = body.find("<x:Visible").is_some_and(|i| {
                let rest = &body[i..];
                !rest.starts_with("<x:Visible>False") && !rest.starts_with("<x:Visible>false")
            });

            let anchor = body.find("<x:Anchor>").and_then(|i| {
                let t = body[i + 10..].split('<').next()?;
                let v: Vec<f64> = t.split(',').filter_map(|x| x.trim().parse().ok()).collect();
                (v.len() == 8).then(|| {
                    let w = |c: f64, px: f64| {
                        (px / dims.col_px(c as u32).max(1.0) * 1024.0)
                            .round()
                            .clamp(0.0, 1023.0) as u16
                    };
                    let h = |r: f64, px: f64| {
                        (px / dims.row_px(r as u32).max(1.0) * 256.0)
                            .round()
                            .clamp(0.0, 255.0) as u16
                    };
                    let computed = [
                        v[0] as u16,
                        w(v[0], v[1]),
                        v[2] as u16,
                        h(v[2], v[3]),
                        v[4] as u16,
                        w(v[4], v[5]),
                        v[6] as u16,
                        h(v[6], v[7]),
                    ];

                    match kept {
                        Some(k) if [0, 2, 4, 6].iter().all(|&j| f64::from(k[j]) == v[j]) => {
                            [k[0], k[1], k[2], k[3], k[4], k[5], k[6], k[7]]
                        }
                        _ => computed,
                    }
                })
            });
            if let (Some(r), Some(c)) = (get("Row"), get("Column")) {
                shown.push((r, c, visible, anchor));
            }
        }
    }
    let mut out = Vec::new();
    let Some(list) = comments.child("commentList") else {
        return out;
    };
    for n in list.children("comment") {
        let Some(rc) = n.attr("ref").and_then(crate::parse_cell) else {
            continue;
        };
        let author = n
            .attr("authorId")
            .and_then(|i| i.parse::<usize>().ok())
            .and_then(|i| authors.get(i))
            .cloned()
            .unwrap_or_default();
        let found = shown.iter().find(|k| k.0 == rc.0 && k.1 == rc.1);
        let visible = found.is_some_and(|k| k.2);
        out.push(NoteIn {
            row: rc.0,
            col: rc.1,
            author,
            text: n.child("text").map(crate::si_text).unwrap_or_default(),
            visible,
            anchor: found.and_then(|k| k.3),
        });
    }
    out
}

use super::wdraw::{escher, opt, sp_container, Item};

pub(super) fn item(dgid: u32, slot: u32, note: &NoteIn) -> Item {
    let spid = dgid * 1024 + 1 + slot;
    let mut sp = Vec::new();
    sp.extend(spid.to_le_bytes());
    sp.extend(0x0A00u32.to_le_bytes());
    let mut props = vec![
        (0x80u16, 0x4100_0000 + spid * 0x100),
        (0x8B, 2),
        (0xBF, 0x0008_0008),
        (0x158, 0),
        (0x181, 0x0800_0050),
        (0x183, 0x0800_0050),
        (0x185, 0x1000_00F4),
        (0x1C0, 0x0800_0051),
        (0x1C3, 0x1000_00F4),
        (0x201, 0x0800_0051),
        (0x203, 0x1000_00F4),
        (0x23F, 0x0003_0001),
        (
            0x3BF,
            if note.visible {
                0x0002_0000
            } else {
                0x0002_0002
            },
        ),
    ];
    let c1 = (note.col + 1).min(0x3FF0) as u16;
    let r1 = note.row.saturating_sub(1) as u16;
    let mut anchor = Vec::new();
    match note.anchor {
        Some(a) => {
            anchor.extend(3u16.to_le_bytes());
            for v in a {
                anchor.extend(v.to_le_bytes());
            }
        }
        None => {
            for v in [3u16, c1, 0xA0, r1, 9, c1 + 2, 0x140, r1 + 4, 0x4F] {
                anchor.extend(v.to_le_bytes());
            }
        }
    }
    let mut body = Vec::new();
    body.extend(escher(2, 202, 0xF00A, &sp));
    body.extend(opt(&mut props, &[]));
    body.extend(escher(0, 0, 0xF010, &anchor));
    body.extend(escher(0, 0, 0xF011, &[]));

    let mut obj = vec![0x15, 0, 0x12, 0, 0x19, 0];
    obj.extend(((slot + 1) as u16).to_le_bytes());
    obj.extend(0x4011u16.to_le_bytes());
    obj.extend([0u8; 12]);
    obj.extend([0x0D, 0, 0x16, 0]);
    let seed = ((u64::from(dgid) << 32) | u64::from(slot + 1)).wrapping_mul(0x9E37_79B9_7F4A_7C15);
    for k in 0..16u32 {
        obj.push((seed.rotate_left(k * 5) >> (k % 8)) as u8 ^ (k as u8).wrapping_mul(29));
    }
    obj.extend([0, 0, 0x10, 0, 0, 0]);
    obj.extend([0, 0, 0, 0]);
    Item {
        sp: sp_container(&body, true),
        obj,
        text: Some(note.text.clone()),
        tail: Vec::new(),
    }
}

pub(super) fn write(o: &mut Out, notes: &[NoteIn], first_id: u32) {
    for (i, note) in notes.iter().enumerate() {
        let mut d = Vec::new();
        d.extend((note.row as u16).to_le_bytes());
        d.extend((note.col as u16).to_le_bytes());
        d.extend((if note.visible { 2u16 } else { 0 }).to_le_bytes());
        d.extend(((first_id + i as u32) as u16).to_le_bytes());
        let units: Vec<u16> = note.author.encode_utf16().take(54).collect();
        let wide = !note.author.is_ascii();
        d.extend((units.len() as u16).to_le_bytes());
        d.push(u8::from(wide));
        for &u in &units {
            if wide {
                d.extend(u.to_le_bytes());
            } else {
                d.push(u as u8);
            }
        }
        if !wide {
            d.push(0);
        }
        o.rec(0x1C, &d);
    }
}

pub(super) fn group_record(groups: &[(u32, u32)], blips: &[super::wdraw::BlipOut]) -> Vec<u8> {
    let shapes: u32 = groups.iter().map(|g| g.1).sum();
    let spid_max = groups
        .iter()
        .map(|g| g.0 * 1024 + g.1)
        .max()
        .unwrap_or(1024);
    let mut dgg = Vec::new();
    dgg.extend(spid_max.to_le_bytes());
    dgg.extend((groups.len() as u32 + 1).to_le_bytes());
    dgg.extend(shapes.to_le_bytes());
    dgg.extend((groups.len() as u32).to_le_bytes());
    for g in groups {
        dgg.extend(g.0.to_le_bytes());
        dgg.extend(g.1.to_le_bytes());
    }
    let mut opt = Vec::new();
    for (id, v) in [
        (0xBFu16, 0x0008_0008u32),
        (0x181, 0x0800_0041),
        (0x4186, 0),
        (0x1C0, 0x0800_0040),
        (0x41C5, 0),
    ] {
        opt.extend(id.to_le_bytes());
        opt.extend(v.to_le_bytes());
    }
    let mut split = Vec::new();
    for c in [0x0800_000Du32, 0x0800_000C, 0x0800_0017, 0x1000_00F7] {
        split.extend(c.to_le_bytes());
    }
    let mut body = Vec::new();
    body.extend(escher(0, 0, 0xF006, &dgg));
    if !blips.is_empty() {
        let mut store = Vec::new();
        for b in blips {
            let mut f = vec![b.kind, b.kind];
            f.extend(b.uid);
            f.extend(0xFFu16.to_le_bytes());
            f.extend((b.record.len() as u32).to_le_bytes());
            f.extend(b.refs.to_le_bytes());
            f.extend([0u8; 4]);
            f.extend([0u8; 4]);
            f.extend(&b.record);
            store.extend(escher(2, u16::from(b.kind), 0xF007, &f));
        }
        body.extend(escher(15, blips.len() as u16, 0xF001, &store));
    }
    body.extend(escher(3, 5, 0xF00B, &opt));
    body.extend(escher(0, 4, 0xF11E, &split));
    escher(15, 0, 0xF000, &body)
}
