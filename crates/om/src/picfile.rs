pub(crate) fn probe(b: &[u8]) -> Option<(&'static str, i64, i64)> {
    let be32 = |o: usize| {
        b.get(o..o + 4)
            .map(|x| u32::from_be_bytes([x[0], x[1], x[2], x[3]]))
    };
    let le32 = |o: usize| {
        b.get(o..o + 4)
            .map(|x| u32::from_le_bytes([x[0], x[1], x[2], x[3]]))
    };
    let be16 = |o: usize| b.get(o..o + 2).map(|x| u16::from_be_bytes([x[0], x[1]]));

    let emu = |px: u32, per_m: u32| -> i64 {
        if per_m == 0 {
            i64::from(px) * 9525
        } else {
            (f64::from(px) * 36_000_000.0 / f64::from(per_m)).round() as i64
        }
    };
    if b.starts_with(&[0x89, b'P', b'N', b'G']) {
        let (w, h) = (be32(16)?, be32(20)?);
        let (mut px, mut py) = (0, 0);
        let mut o = 8;
        while let (Some(len), Some(ty)) = (be32(o), b.get(o + 4..o + 8)) {
            if ty == b"pHYs" && b.get(o + 16) == Some(&1) {
                px = be32(o + 8)?;
                py = be32(o + 12)?;
            }
            if ty == b"IDAT" {
                break;
            }
            o += 12 + len as usize;
        }
        return Some(("png", emu(w, px), emu(h, py)));
    }
    if b.starts_with(&[0xFF, 0xD8]) {
        let (mut dx, mut dy) = (0u32, 0u32);
        let (mut dpi_x, mut dpi_y) = (0u32, 0u32);
        let mut o = 2;
        while b.get(o) == Some(&0xFF) {
            let m = *b.get(o + 1)?;
            let len = usize::from(be16(o + 2)?);
            if m == 0xE0 && b.get(o + 4..o + 9) == Some(b"JFIF\0") {
                let unit = *b.get(o + 11)?;
                let (x, y) = (u32::from(be16(o + 12)?), u32::from(be16(o + 14)?));
                match unit {
                    1 => (dpi_x, dpi_y) = (x, y),
                    2 => (dx, dy) = (x * 100, y * 100),
                    _ => {}
                }
            }
            if matches!(m, 0xC0..=0xC3 | 0xC5..=0xC7 | 0xC9..=0xCB | 0xCD..=0xCF) {
                let (h, w) = (u32::from(be16(o + 5)?), u32::from(be16(o + 7)?));
                let by_dpi = |px: u32, dpi: u32| i64::from(px) * 914_400 / i64::from(dpi);
                if dpi_x > 0 && dpi_y > 0 {
                    return Some(("jpeg", by_dpi(w, dpi_x), by_dpi(h, dpi_y)));
                }
                return Some(("jpeg", emu(w, dx), emu(h, dy)));
            }
            o += 2 + len;
        }
        return None;
    }
    if b.starts_with(b"GIF8") {
        let w = u32::from(u16::from_le_bytes([*b.get(6)?, *b.get(7)?]));
        let h = u32::from(u16::from_le_bytes([*b.get(8)?, *b.get(9)?]));
        return Some(("gif", emu(w, 0), emu(h, 0)));
    }
    if b.starts_with(b"BM") {
        let (w, h) = (le32(18)?, le32(22)? as i32);
        return Some(("bmp", emu(w, le32(38)?), emu(h.unsigned_abs(), le32(42)?)));
    }
    None
}
