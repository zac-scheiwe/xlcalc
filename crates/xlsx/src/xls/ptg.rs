use super::cur::Cur;

pub(crate) const MAX_ROW8: u32 = 0xFFFF;
pub(crate) const MAX_COL8: u32 = 0xFF;
const MAX_ROW12: u32 = 0xF_FFFF;
const MAX_COL12: u32 = 0x3FFF;

fn base_ptg(p: u8) -> u8 {
    if p >= 0x20 {
        (p & 0x1F) | 0x20
    } else {
        p
    }
}

fn ref_in(row: u16, col: u16, n: bool, base: (u32, u32)) -> (u32, u16) {
    let (row_rel, col_rel) = (col & 0x8000 != 0, col & 0x4000 != 0);
    let mut r = u32::from(row);
    let mut c = u32::from(col & 0xFF);
    if n {
        if row_rel {
            r = (i64::from(base.0) + i64::from(row as i16)).rem_euclid(65536) as u32;
        }
        if col_rel {
            c = (i64::from(base.1) + i64::from((col & 0xFF) as u8 as i8)).rem_euclid(256) as u32;
        }
    }
    (r, c as u16 | (col & 0xC000))
}

fn put_str16(out: &mut Vec<u8>, s: &str) {
    let u: Vec<u16> = s.encode_utf16().collect();
    out.extend((u.len() as u16).to_le_bytes());
    for x in u {
        out.extend(x.to_le_bytes());
    }
}

pub(crate) fn to_biff12(
    rgce: &[u8],
    extra: &[u8],
    base: (u32, u32),
    cp: u16,
    namex: &mut dyn FnMut(u16, u16) -> Option<u32>,
) -> Result<(Vec<u8>, Vec<u8>), String> {
    let mut c = Cur::new(rgce);
    let mut ex = Cur::new(extra);
    let mut out: Vec<u8> = Vec::new();
    let mut cb: Vec<u8> = Vec::new();
    while !c.done() {
        let ptg = c.u8()?;
        let b = base_ptg(ptg);
        out.push(ptg);
        match b {
            0x01 | 0x02 => {
                let row = c.u16()?;
                let col = c.u16()?;
                out.extend(u32::from(row).to_le_bytes());
                cb.extend(u32::from(col).to_le_bytes());
            }
            0x03..=0x16 => {}
            0x17 => {
                let n = usize::from(c.u8()?);
                let s = c.xl_chars(n, cp)?;
                out.pop();
                out.push(0x17);
                put_str16(&mut out, &s);
            }
            0x18 => return Err("PtgExtend".into()),
            0x19 => {
                let ty = c.u8()?;
                out.push(ty);
                match ty {
                    0x04 => {
                        let n = c.u16()?;
                        out.extend(n.to_le_bytes());
                        out.extend(c.take((usize::from(n) + 1) * 2)?);
                    }
                    _ => out.extend(c.take(2)?),
                }
            }
            0x1C | 0x1D => out.push(c.u8()?),
            0x1E => out.extend(c.take(2)?),
            0x1F => out.extend(c.take(8)?),
            0x20 => {
                c.take(7)?;
                out.extend([0u8; 14]);
                let cols = match ex.u8()? {
                    0 => 256u32,
                    n => u32::from(n),
                };
                let rows = u32::from(ex.u16()?) + 1;
                cb.extend(rows.to_le_bytes());
                cb.extend(cols.to_le_bytes());
                for _ in 0..rows * cols {
                    match ex.u8()? {
                        0 => {
                            ex.take(8)?;
                            cb.push(1);
                            cb.extend(0u16.to_le_bytes());
                        }
                        1 => {
                            cb.push(0);
                            cb.extend(ex.take(8)?);
                        }
                        2 => {
                            let n = usize::from(ex.u16()?);
                            let s = ex.xl_chars(n, cp)?;
                            cb.push(1);
                            put_str16(&mut cb, &s);
                        }
                        4 => {
                            cb.extend([2, ex.u8()?]);
                            ex.take(7)?;
                        }
                        0x10 => {
                            cb.extend([4, ex.u8()?, 0, 0, 0]);
                            ex.take(7)?;
                        }
                        t => return Err(format!("array element type {t}")),
                    }
                }
            }
            0x21 => out.extend(c.take(2)?),
            0x22 => out.extend(c.take(3)?),
            0x23 => out.extend(c.take(4)?),
            0x24 | 0x2C => {
                let (row, col) = (c.u16()?, c.u16()?);
                let (r, w) = ref_in(row, col, b == 0x2C, base);
                out.extend(r.to_le_bytes());
                out.extend(w.to_le_bytes());
                if b == 0x2C {
                    let at = ptg_pos(&out);
                    out[at] = ptg & 0xE0 | 0x04;
                }
            }
            0x25 | 0x2D => area_in(&mut c, &mut out, b == 0x2D, base, ptg)?,
            0x26 => {
                c.take(4)?;
                let cce = c.take(2)?;
                out.extend([0u8; 4]);
                out.extend(cce);
                let n = usize::from(ex.u16()?);
                cb.extend((n as u32).to_le_bytes());
                for _ in 0..n {
                    let (r0, r1, c0, c1) = (ex.u16()?, ex.u16()?, ex.u16()?, ex.u16()?);
                    cb.extend(u32::from(r0).to_le_bytes());
                    cb.extend(u32::from(r1).to_le_bytes());
                    cb.extend(u32::from(c0).to_le_bytes());
                    cb.extend(u32::from(c1).to_le_bytes());
                }
            }
            0x27 | 0x28 => out.extend(c.take(6)?),
            0x29 => out.extend(c.take(2)?),
            0x2A => {
                c.take(4)?;
                out.extend([0u8; 6]);
            }
            0x2B => {
                c.take(8)?;
                out.extend([0u8; 12]);
            }
            0x39 => {
                let ixti = c.u16()?;
                let idx = c.u16()?;
                c.take(2)?;
                if let Some(n) = namex(ixti, idx) {
                    out.pop();
                    out.push(ptg & 0xE0 | 0x03);
                    out.extend(n.to_le_bytes());
                } else {
                    out.extend(ixti.to_le_bytes());
                    out.extend(u32::from(idx).to_le_bytes());
                }
            }
            0x3A => {
                out.extend(c.take(2)?);
                let (row, col) = (c.u16()?, c.u16()?);
                let (r, w) = ref_in(row, col, false, base);
                out.extend(r.to_le_bytes());
                out.extend(w.to_le_bytes());
            }
            0x3B => {
                out.extend(c.take(2)?);
                area_in(&mut c, &mut out, false, base, 0xFF)?;
            }
            0x3C => {
                out.extend(c.take(2)?);
                c.take(4)?;
                out.extend([0u8; 6]);
            }
            0x3D => {
                out.extend(c.take(2)?);
                c.take(8)?;
                out.extend([0u8; 12]);
            }
            _ => return Err(format!("formula token {ptg:#x}")),
        }
    }
    Ok((out, cb))
}

fn ptg_pos(out: &[u8]) -> usize {
    out.len() - 7
}

fn area_in(
    c: &mut Cur<'_>,
    out: &mut Vec<u8>,
    n: bool,
    base: (u32, u32),
    ptg: u8,
) -> Result<(), String> {
    let (r0, r1, c0, c1) = (c.u16()?, c.u16()?, c.u16()?, c.u16()?);
    let (a, w0) = ref_in(r0, c0, n, base);
    let (mut b, w1) = ref_in(r1, c1, n, base);
    let mut w1 = w1;
    let (ca, cb) = (w0 & 0xFF, w1 & 0xFF);

    if a == 0 && b == MAX_ROW8 {
        b = MAX_ROW12;
    }
    if ca == 0 && cb == MAX_COL8 as u16 {
        w1 = (w1 & 0xC000) | MAX_COL12 as u16;
    }
    out.extend(a.to_le_bytes());
    out.extend(b.to_le_bytes());
    out.extend(w0.to_le_bytes());
    out.extend(w1.to_le_bytes());
    if n && ptg != 0xFF {
        let at = out.len() - 12 - 1;
        out[at] = ptg & 0xE0 | 0x05;
    }
    Ok(())
}
