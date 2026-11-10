use super::cur::Cur;
use std::collections::HashMap;

const MAX_RGCE: usize = 1800;
const MAX_ROW8: u32 = 0xFFFF;
const MAX_COL8: u32 = 0xFF;
const MAX_ROW12: u32 = 0xF_FFFF;
const MAX_COL12: u32 = 0x3FFF;

fn base_ptg(p: u8) -> u8 {
    if p >= 0x20 {
        (p & 0x1F) | 0x20
    } else {
        p
    }
}

pub(crate) fn put_chars(out: &mut Vec<u8>, s: &str) {
    let u: Vec<u16> = s.encode_utf16().collect();
    if u.iter().all(|&x| x < 0x100) {
        out.push(0);
        out.extend(u.iter().map(|&x| x as u8));
    } else {
        out.push(1);
        for x in u {
            out.extend(x.to_le_bytes());
        }
    }
}

fn utf16_of(t: &[u8]) -> String {
    let u: Vec<u16> = t
        .as_chunks::<2>()
        .0
        .iter()
        .map(|x| u16::from_le_bytes(*x))
        .collect();
    String::from_utf16_lossy(&u)
}

fn ref_out(row: u32, col: u16) -> Result<(u16, u16), String> {
    let c = u32::from(col & 0x3FFF);
    if row > MAX_ROW8 || c > MAX_COL8 {
        return Err(format!(
            "reference beyond the .xls limits (row {}, column {})",
            row + 1,
            c + 1
        ));
    }
    Ok((row as u16, c as u16 | (col & 0xC000)))
}

fn refn_out(row: u32, col: u16, base: (u32, u32)) -> Result<(u16, u16), String> {
    let c = u32::from(col & 0x3FFF);
    let rw = if col & 0x8000 != 0 {
        (i64::from(row) - i64::from(base.0)) as i16 as u16
    } else if row > MAX_ROW8 {
        return Err("reference beyond the .xls limits".into());
    } else {
        row as u16
    };
    let cw = if col & 0x4000 != 0 {
        u16::from((i64::from(c) - i64::from(base.1)) as i8 as u8)
    } else if c > MAX_COL8 {
        return Err("reference beyond the .xls limits".into());
    } else {
        c as u16
    };
    Ok((rw, cw | (col & 0xC000)))
}

fn area_out(c: &mut Cur<'_>, out: &mut Vec<u8>, rel: Option<(u32, u32)>) -> Result<(), String> {
    let (r0, r1) = (c.u32()?, c.u32()?);
    let (c0, c1) = (c.u16()?, c.u16()?);

    let r1 = if r0 == 0 && r1 == MAX_ROW12 {
        MAX_ROW8
    } else {
        r1
    };
    let c1 = if c0 & 0x3FFF == 0 && u32::from(c1 & 0x3FFF) == MAX_COL12 {
        (c1 & 0xC000) | MAX_COL8 as u16
    } else {
        c1
    };
    let relative = (c0 | c1) & 0xC000 != 0;
    let (a, ca, b, cb) = match rel {
        Some(base) if relative => {
            let last = out.len() - 1;
            out[last] |= 0x08;
            let (a, ca) = refn_out(r0, c0, base)?;
            let (b, cb) = refn_out(r1, c1, base)?;
            (a, ca, b, cb)
        }
        _ => {
            let (a, ca) = ref_out(r0, c0)?;
            let (b, cb) = ref_out(r1, c1)?;
            (a, ca, b, cb)
        }
    };
    out.extend(a.to_le_bytes());
    out.extend(b.to_le_bytes());
    out.extend(ca.to_le_bytes());
    out.extend(cb.to_le_bytes());
    Ok(())
}

pub(crate) fn from_biff12(rgce: &[u8], rgcb: &[u8]) -> Result<(Vec<u8>, Vec<u8>), String> {
    from_biff12_at(rgce, rgcb, None)
}

pub(crate) fn from_biff12_at(
    rgce: &[u8],
    rgcb: &[u8],
    rel: Option<(u32, u32)>,
) -> Result<(Vec<u8>, Vec<u8>), String> {
    let mut c = Cur::new(rgce);
    let mut ex = Cur::new(rgcb);
    let mut out: Vec<u8> = Vec::new();
    let mut extra: Vec<u8> = Vec::new();

    let mut starts: HashMap<usize, usize> = HashMap::new();
    let mut jumps: Vec<Jump> = Vec::new();
    while !c.done() {
        starts.insert(c.p, out.len());
        let ptg = c.u8()?;
        let b = base_ptg(ptg);
        out.push(ptg);
        match b {
            0x01 | 0x02 => {
                let row = c.u32()?;
                let col = ex.u32()?;
                if row > MAX_ROW8 || col > MAX_COL8 {
                    return Err("shared/array/table reference beyond the .xls limits".into());
                }
                out.extend((row as u16).to_le_bytes());
                out.extend((col as u16).to_le_bytes());
            }
            0x03..=0x16 => {}
            0x17 => {
                let n = usize::from(c.u16()?);
                if n > 255 {
                    return Err("text constant longer than 255 characters".into());
                }
                let s = utf16_of(c.take(n * 2)?);
                out.push(n as u8);
                put_chars(&mut out, &s);
            }
            0x18 => return Err("structured reference".into()),
            0x19 => {
                let ty = c.u8()?;
                out.push(ty);
                match ty {
                    0x04 => {
                        let n = c.u16()?;
                        out.extend(n.to_le_bytes());
                        let (base, new_base) = (c.p, out.len());
                        for _ in 0..=n {
                            let off = usize::from(c.u16()?);
                            jumps.push(Jump {
                                at: out.len(),
                                new_base,
                                old_target: base + off,
                                adj: 0,
                            });
                            out.extend([0, 0]);
                        }
                    }
                    0x02 | 0x08 => {
                        let off = usize::from(c.u16()?);
                        let adj = usize::from(ty == 0x08);
                        jumps.push(Jump {
                            at: out.len(),
                            new_base: out.len() + 2,
                            old_target: c.p + off + adj,
                            adj,
                        });
                        out.extend([0, 0]);
                    }
                    0x80 => return Err("IFERROR jump".into()),
                    _ => out.extend(c.take(2)?),
                }
            }
            0x1C | 0x1D => out.push(c.u8()?),
            0x1E => out.extend(c.take(2)?),
            0x1F => out.extend(c.take(8)?),
            0x20 => {
                c.take(14)?;
                out.extend([0u8; 7]);
                let rows = ex.u32()?;
                let cols = ex.u32()?;
                if rows == 0 || cols == 0 || rows > 65536 || cols > 256 {
                    return Err("array constant beyond the .xls limits".into());
                }
                extra.push(if cols == 256 { 0 } else { cols as u8 });
                extra.extend(((rows - 1) as u16).to_le_bytes());
                for _ in 0..rows * cols {
                    match ex.u8()? {
                        0 => {
                            extra.push(1);
                            extra.extend(ex.take(8)?);
                        }
                        1 => {
                            let n = usize::from(ex.u16()?);
                            let s = utf16_of(ex.take(n * 2)?);
                            extra.push(2);
                            extra.extend((n as u16).to_le_bytes());
                            put_chars(&mut extra, &s);
                        }
                        2 => {
                            extra.extend([4, ex.u8()?]);
                            extra.extend([0u8; 7]);
                        }
                        4 => {
                            extra.extend([0x10, ex.u8()?]);
                            ex.take(3)?;
                            extra.extend([0u8; 7]);
                        }
                        t => return Err(format!("array element type {t}")),
                    }
                }
            }
            0x21 => out.extend(c.take(2)?),
            0x22 => {
                let t = c.take(3)?;
                let (argc, fid) = (t[0] & 0x7F, u16::from_le_bytes([t[1], t[2]]));

                if argc > 30 + u8::from(fid == 255) {
                    return Err(format!("{argc} arguments (the .xls limit is 30)"));
                }
                out.extend(t);
            }
            0x23 => out.extend(c.take(4)?),
            0x24 => {
                let row = c.u32()?;
                let col = c.u16()?;
                let (r, w) = match rel {
                    Some(b) if col & 0xC000 != 0 => {
                        let last = out.len() - 1;
                        out[last] = ptg | 0x08;
                        refn_out(row, col, b)?
                    }
                    _ => ref_out(row, col)?,
                };
                out.extend(r.to_le_bytes());
                out.extend(w.to_le_bytes());
            }
            0x25 => area_out(&mut c, &mut out, rel)?,
            0x26 => {
                c.take(4)?;
                let cce = usize::from(c.u16()?);
                out.extend([0u8; 4]);
                jumps.push(Jump {
                    at: out.len(),
                    new_base: out.len() + 2,
                    old_target: c.p + cce,
                    adj: 0,
                });
                out.extend([0, 0]);
                let n = ex.u32()?;
                extra.extend((n as u16).to_le_bytes());
                for _ in 0..n {
                    let (r0, r1, c0, c1) = (ex.u32()?, ex.u32()?, ex.u32()?, ex.u32()?);
                    for v in [r0, r1, c0, c1] {
                        extra.extend((v.min(0xFFFF) as u16).to_le_bytes());
                    }
                }
            }
            0x27 | 0x28 => {
                out.extend(c.take(4)?);
                let cce = usize::from(c.u16()?);
                jumps.push(Jump {
                    at: out.len(),
                    new_base: out.len() + 2,
                    old_target: c.p + cce,
                    adj: 0,
                });
                out.extend([0, 0]);
            }
            0x29 => {
                let cce = usize::from(c.u16()?);
                jumps.push(Jump {
                    at: out.len(),
                    new_base: out.len() + 2,
                    old_target: c.p + cce,
                    adj: 0,
                });
                out.extend([0, 0]);
            }
            0x2A => {
                c.take(6)?;
                out.extend([0u8; 4]);
            }
            0x2B => {
                c.take(12)?;
                out.extend([0u8; 8]);
            }
            0x39 => {
                out.extend(c.take(2)?);
                let idx = c.u32()?;
                out.extend((idx as u16).to_le_bytes());
                out.extend([0u8; 2]);
            }
            0x3A => {
                out.extend(c.take(2)?);
                let row = c.u32()?;
                let col = c.u16()?;
                let (r, w) = ref_out(row, col)?;
                out.extend(r.to_le_bytes());
                out.extend(w.to_le_bytes());
            }
            0x3B => {
                out.extend(c.take(2)?);
                area_out(&mut c, &mut out, None)?;
            }
            0x3C => {
                out.extend(c.take(2)?);
                c.take(6)?;
                out.extend([0u8; 4]);
            }
            0x3D => {
                out.extend(c.take(2)?);
                c.take(12)?;
                out.extend([0u8; 8]);
            }
            _ => return Err(format!("formula token {ptg:#x}")),
        }
    }
    if out.len() > MAX_RGCE {
        return Err(format!(
            "{} bytes of tokens (the .xls limit is {MAX_RGCE})",
            out.len()
        ));
    }
    starts.insert(c.p, out.len());
    for j in jumps {
        let target = starts
            .get(&j.old_target)
            .ok_or("a jump in the formula lands inside a token")?;
        let new_off = target
            .checked_sub(j.new_base + j.adj)
            .ok_or("a jump in the formula runs backwards")?;
        out[j.at..j.at + 2].copy_from_slice(&(new_off as u16).to_le_bytes());
    }
    Ok((out, extra))
}

struct Jump {
    at: usize,
    new_base: usize,
    old_target: usize,
    adj: usize,
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn refs_and_strings_narrow() {
        let rgce = [0x44, 0, 0, 0, 0, 0, 0, 0x17, 2, 0, b'h', 0, b'i', 0, 0x03];
        let (b8, ex) = from_biff12(&rgce, &[]).unwrap();
        assert_eq!(b8, [0x44, 0, 0, 0, 0, 0x17, 2, 0, b'h', b'i', 0x03]);
        assert!(ex.is_empty());
    }

    #[test]
    fn formula_limits() {
        let sum = |n: usize| {
            let mut t = vec![0x1E, 1, 0];
            for _ in 1..n {
                t.extend([0x1E, 1, 0, 0x03]);
            }
            t
        };
        assert!(from_biff12(&sum(450), &[]).is_ok());
        assert!(from_biff12(&sum(451), &[]).is_err());

        let mut t = Vec::new();
        for _ in 0..31 {
            t.extend([0x1E, 1, 0]);
        }
        t.extend([0x22, 31, 4, 0]);
        assert!(from_biff12(&t, &[]).is_err());
    }

    #[test]
    fn rows_beyond_65536_are_refused() {
        let rgce = [0x44, 0, 0, 1, 0, 0, 0];
        assert!(from_biff12(&rgce, &[]).is_err());
    }

    #[test]
    fn whole_columns_narrow() {
        let mut rgce = vec![0x45];
        rgce.extend(0u32.to_le_bytes());
        rgce.extend(0xF_FFFFu32.to_le_bytes());
        rgce.extend(0u16.to_le_bytes());
        rgce.extend(0u16.to_le_bytes());
        let (b8, _) = from_biff12(&rgce, &[]).unwrap();
        assert_eq!(b8, [0x45, 0, 0, 0xFF, 0xFF, 0, 0, 0, 0]);
    }
}
