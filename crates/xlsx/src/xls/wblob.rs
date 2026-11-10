use super::wdraw::{Corner, ShapeIn};
use std::collections::BTreeMap;
use std::fmt::Write as _;
use std::io::Write as _;

pub(super) fn md4(data: &[u8]) -> [u8; 16] {
    let mut msg = data.to_vec();
    msg.push(0x80);
    while msg.len() % 64 != 56 {
        msg.push(0);
    }
    msg.extend(((data.len() as u64) * 8).to_le_bytes());
    let mut s = [0x6745_2301u32, 0xefcd_ab89, 0x98ba_dcfe, 0x1032_5476];
    for blk in msg.as_chunks::<64>().0 {
        let mut x = [0u32; 16];
        for (i, w) in x.iter_mut().enumerate() {
            *w = u32::from_le_bytes([blk[4 * i], blk[4 * i + 1], blk[4 * i + 2], blk[4 * i + 3]]);
        }
        let [mut a, mut b, mut c, mut d] = s;
        for k in 0..16 {
            let f = (b & c) | (!b & d);
            let r = [3, 7, 11, 19][k % 4];
            let t = a.wrapping_add(f).wrapping_add(x[k]).rotate_left(r);
            (a, b, c, d) = (d, t, b, c);
        }
        const G: [usize; 16] = [0, 4, 8, 12, 1, 5, 9, 13, 2, 6, 10, 14, 3, 7, 11, 15];
        for k in 0..16 {
            let g = (b & c) | (b & d) | (c & d);
            let r = [3, 5, 9, 13][k % 4];
            let t = a
                .wrapping_add(g)
                .wrapping_add(x[G[k]])
                .wrapping_add(0x5a82_7999)
                .rotate_left(r);
            (a, b, c, d) = (d, t, b, c);
        }
        const H: [usize; 16] = [0, 8, 4, 12, 2, 10, 6, 14, 1, 9, 5, 13, 3, 11, 7, 15];
        for k in 0..16 {
            let h = b ^ c ^ d;
            let r = [3, 9, 11, 15][k % 4];
            let t = a
                .wrapping_add(h)
                .wrapping_add(x[H[k]])
                .wrapping_add(0x6ed9_eba1)
                .rotate_left(r);
            (a, b, c, d) = (d, t, b, c);
        }
        s = [
            s[0].wrapping_add(a),
            s[1].wrapping_add(b),
            s[2].wrapping_add(c),
            s[3].wrapping_add(d),
        ];
    }
    let mut out = [0u8; 16];
    for (i, w) in s.iter().enumerate() {
        out[4 * i..4 * i + 4].copy_from_slice(&w.to_le_bytes());
    }
    out
}

const BOOL_GROUPS: [u16; 14] = [
    0x3F, 0x7F, 0xBF, 0x13F, 0x17F, 0x1BF, 0x1FF, 0x23F, 0x27F, 0x2BF, 0x33F, 0x37F, 0x3BF, 0x43F,
];

const LEFT_OUT: [u16; 19] = [
    0x80, 0x105, 0x106, 0x145, 0x146, 0x151, 0x152, 0x157, 0x187, 0x191, 0x192, 0x193, 0x194,
    0x197, 0x1BE, 0x21C, 0x301, 0x380, 0x504,
];

fn default_of(pid: u16) -> u32 {
    match pid {
        0x81 | 0x83 | 0x8D => 0x1_6530,
        0x82 | 0x84 => 0xB298,
        0x142 | 0x143 => 0x5460,
        0x181 => 0xFF_FFFF,
        0x182 | 0x1C1 => 0x1_0000,
        0x1CB => 0x2535,
        0x1CC => 0x8_0000,
        0x1D6 | 0x1D7 => 2,
        0x144 | 0x158 | 0xBB | 0x1BB | 0x1FC | 0x17B | 0x17C | 0x17F | 0x8C => 1,
        _ => 0,
    }
}

pub(super) fn shape_checksum(spt: u16, props: &[(u16, u32)]) -> [u8; 16] {
    let mut flat: BTreeMap<u16, u32> = BTreeMap::new();
    for &(id, v) in props {
        let pid = id & 0x3FFF;
        if BOOL_GROUPS.contains(&pid) {
            for i in 0..16u16 {
                if v >> (16 + i) & 1 != 0 {
                    flat.insert(pid - i, v >> i & 1);
                }
            }
        } else {
            flat.insert(pid, v);
        }
    }
    let mut buf = Vec::new();
    for (&pid, &v) in &flat {
        if !LEFT_OUT.contains(&pid) && v != default_of(pid) {
            buf.extend(u32::from(pid).to_le_bytes());
            buf.extend(v.to_le_bytes());
        }
    }
    buf.extend(u32::from(spt).to_le_bytes());
    md4(&buf)
}

fn b64(d: &[u8]) -> String {
    const A: &[u8; 64] = b"ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789+/";
    let mut s = String::new();
    for c in d.chunks(3) {
        let n = u32::from(c[0]) << 16
            | u32::from(c.get(1).copied().unwrap_or(0)) << 8
            | u32::from(c.get(2).copied().unwrap_or(0));
        for k in 0..4 {
            if k <= c.len() {
                s.push(A[(n >> (18 - 6 * k) & 63) as usize] as char);
            } else {
                s.push('=');
            }
        }
    }
    s + "&#xA;"
}

fn esc(s: &str) -> String {
    s.replace('&', "&amp;")
        .replace('<', "&lt;")
        .replace('>', "&gt;")
        .replace('"', "&quot;")
}

const XDR: &str = "http://schemas.openxmlformats.org/drawingml/2006/spreadsheetDrawing";
const A: &str = "http://schemas.openxmlformats.org/drawingml/2006/main";

pub(super) fn blob(s: &ShapeIn, spid: u32, sum: &[u8; 16]) -> Option<Vec<u8>> {
    let prst = s.preset.as_deref()?;
    let (from, to): (Corner, Corner) = s.corners?;
    let [x0, y0, x1, y1] = s.rect;
    let mut sp = String::new();
    let _ = write!(
        sp,
        "<?xml version=\"1.0\" encoding=\"UTF-8\" standalone=\"yes\"?>\n<xdr:anchor xmlns:xdr=\"{XDR}\"><xdr:twoCellAnchor>"
    );
    for (tag, c) in [("from", from), ("to", to)] {
        let _ = write!(
            sp,
            "<xdr:{tag}><xdr:col>{}</xdr:col><xdr:colOff>{}</xdr:colOff><xdr:row>{}</xdr:row><xdr:rowOff>{}</xdr:rowOff></xdr:{tag}>",
            c.0, c.1, c.2, c.3
        );
    }
    let _ = write!(
        sp,
        "<xdr:sp macro=\"\" textlink=\"\"><xdr:nvSpPr><xdr:cNvPr id=\"{spid}\" name=\"{}\"/><xdr:cNvSpPr/></xdr:nvSpPr><xdr:spPr>",
        esc(&s.name)
    );
    let flips = format!(
        "{}{}",
        if s.flip_h { " flipH=\"1\"" } else { "" },
        if s.flip_v { " flipV=\"1\"" } else { "" }
    );
    let _ = write!(
        sp,
        "<a:xfrm xmlns:a=\"{A}\"{flips}><a:off x=\"{x0}\" y=\"{y0}\"/><a:ext cx=\"{}\" cy=\"{}\"/></a:xfrm><a:prstGeom xmlns:a=\"{A}\" prst=\"{}\"><a:avLst/></a:prstGeom>",
        x1 - x0,
        y1 - y0,
        esc(prst)
    );
    let rgb = |c: u32| {
        format!(
            "{:02X}{:02X}{:02X}",
            c & 0xFF,
            c >> 8 & 0xFF,
            c >> 16 & 0xFF
        )
    };
    if s.no_fill {
        let _ = write!(sp, "<a:noFill xmlns:a=\"{A}\"/>");
    } else if let Some(c) = s.fill {
        let _ = write!(
            sp,
            "<a:solidFill xmlns:a=\"{A}\"><a:srgbClr val=\"{}\"/></a:solidFill>",
            rgb(c)
        );
    }
    if s.no_line || s.line.is_some() || s.line_emu.is_some() {
        let w = s.line_emu.map_or(String::new(), |w| format!(" w=\"{w}\""));
        let _ = write!(sp, "<a:ln xmlns:a=\"{A}\"{w}>");
        if s.no_line {
            let _ = write!(sp, "<a:noFill/>");
        } else if let Some(c) = s.line {
            let _ = write!(
                sp,
                "<a:solidFill><a:srgbClr val=\"{}\"/></a:solidFill>",
                rgb(c)
            );
        }
        sp.push_str("</a:ln>");
    }
    sp.push_str("</xdr:spPr></xdr:sp><xdr:clientData/></xdr:twoCellAnchor></xdr:anchor>");
    let down = format!(
        "<?xml version=\"1.0\" encoding=\"UTF-8\" standalone=\"yes\"?>\n<a:downRevStg xmlns:a=\"{A}\" shapeCheckSum=\"{}\" textCheckSum=\"\" shapeId=\"{spid}\" fHybridRaster=\"0\" ver=\"1\"><a:xlAnchor><a:from row=\"{}\" col=\"{}\" rowOffset=\"{}\" colOffset=\"{}\"/><a:to row=\"{}\" col=\"{}\" rowOffset=\"{}\" colOffset=\"{}\"/></a:xlAnchor></a:downRevStg>",
        b64(sum),
        from.2, from.0, from.3, from.1, to.2, to.0, to.3, to.1
    );
    let ct = "<?xml version=\"1.0\" encoding=\"UTF-8\" standalone=\"yes\"?>\n<Types xmlns=\"http://schemas.openxmlformats.org/package/2006/content-types\"><Default Extension=\"rels\" ContentType=\"application/vnd.openxmlformats-package.relationships+xml\"/><Default Extension=\"xml\" ContentType=\"application/xml\"/><Override PartName=\"/drs/shapexml.xml\" ContentType=\"application/vnd.ms-office.DrsShape+xml\"/><Override PartName=\"/drs/downrev.xml\" ContentType=\"application/vnd.ms-office.DrsDownRev+xml\"/></Types>";
    let rels = "<?xml version=\"1.0\" encoding=\"UTF-8\" standalone=\"yes\"?>\n<Relationships xmlns=\"http://schemas.openxmlformats.org/package/2006/relationships\"><Relationship Id=\"rId2\" Type=\"http://schemas.microsoft.com/office/2006/relationships/downRev\" Target=\"drs/downrev.xml\"/><Relationship Id=\"rId1\" Type=\"http://schemas.microsoft.com/office/2006/relationships/shapeXml\" Target=\"drs/shapexml.xml\"/></Relationships>";
    let mut zw = zip::ZipWriter::new(std::io::Cursor::new(Vec::new()));
    let o = zip::write::SimpleFileOptions::default()
        .compression_method(zip::CompressionMethod::Deflated);
    for (name, body) in [
        ("[Content_Types].xml", ct),
        ("_rels/.rels", rels),
        ("drs/shapexml.xml", sp.as_str()),
        ("drs/downrev.xml", down.as_str()),
    ] {
        zw.start_file(name, o).ok()?;
        zw.write_all(body.as_bytes()).ok()?;
    }
    Some(zw.finish().ok()?.into_inner())
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn md4_vectors() {
        let hex = |d: [u8; 16]| d.iter().map(|b| format!("{b:02x}")).collect::<String>();
        assert_eq!(hex(md4(b"")), "31d6cfe0d16ae931b73c59d7e0c089c0");
        assert_eq!(hex(md4(b"abc")), "a448017aaf21d8525fc10ae87aa6729d");
        assert_eq!(
            hex(md4(
                b"12345678901234567890123456789012345678901234567890123456789012345678901234567890"
            )),
            "e33b4ddc9c38f2199c3e7b164fcc0536"
        );
    }

    #[test]
    fn checksum_matches_excel() {
        let props = [
            (0xBF, 0x1F_0018),
            (0x181, 0xAD_BBF2),
            (0x1BF, 0x10_0010),
            (0x1C0, 0xFF_FFFF),
            (0x1CB, 0x4A6A),
            (0x1FF, 0x8_0008),
            (0x3BF, 0x2_0000),
        ];
        let sum = shape_checksum(1, &props);
        assert_eq!(
            sum.iter().map(|b| format!("{b:02x}")).collect::<String>(),
            "3fb2fd22232233994697b906f712ded7"
        );
        assert_eq!(b64(&sum), "P7L9IiMiM5lGl7kG9xLe1w==&#xA;");
    }
}
