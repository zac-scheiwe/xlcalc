pub(crate) struct Cur<'a> {
    pub b: &'a [u8],
    pub p: usize,
}

impl<'a> Cur<'a> {
    pub fn new(b: &'a [u8]) -> Self {
        Cur { b, p: 0 }
    }
    pub fn done(&self) -> bool {
        self.p >= self.b.len()
    }
    pub fn rest(&self) -> &'a [u8] {
        &self.b[self.p.min(self.b.len())..]
    }
    pub fn take(&mut self, n: usize) -> Result<&'a [u8], String> {
        let end = self.p.checked_add(n).ok_or("length overflow")?;
        let s = self.b.get(self.p..end).ok_or("record too short")?;
        self.p = end;
        Ok(s)
    }
    pub fn u8(&mut self) -> Result<u8, String> {
        Ok(self.take(1)?[0])
    }
    pub fn u16(&mut self) -> Result<u16, String> {
        let t = self.take(2)?;
        Ok(u16::from_le_bytes([t[0], t[1]]))
    }
    pub fn i16(&mut self) -> Result<i16, String> {
        Ok(self.u16()? as i16)
    }
    pub fn u32(&mut self) -> Result<u32, String> {
        let t = self.take(4)?;
        Ok(u32::from_le_bytes([t[0], t[1], t[2], t[3]]))
    }
    pub fn f64(&mut self) -> Result<f64, String> {
        let t = self.take(8)?;
        let mut a = [0u8; 8];
        a.copy_from_slice(t);
        Ok(f64::from_le_bytes(a))
    }

    pub fn xl_chars(&mut self, n: usize, cp: u16) -> Result<String, String> {
        let flags = self.u8()?;
        self.chars(n, flags & 1 != 0, cp)
    }

    pub fn chars(&mut self, n: usize, wide: bool, cp: u16) -> Result<String, String> {
        if wide {
            let t = self.take(n * 2)?;
            let u: Vec<u16> = t
                .as_chunks::<2>()
                .0
                .iter()
                .map(|c| u16::from_le_bytes(*c))
                .collect();
            Ok(String::from_utf16_lossy(&u))
        } else {
            let t = self.take(n)?;
            let _ = cp;
            Ok(t.iter().map(|&x| char::from(x)).collect())
        }
    }

    pub fn short_str(&mut self, cp: u16) -> Result<String, String> {
        let n = usize::from(self.u8()?);
        self.xl_chars(n, cp)
    }

    pub fn long_str(&mut self, cp: u16) -> Result<String, String> {
        let n = usize::from(self.u16()?);
        let flags = self.u8()?;
        let runs = if flags & 8 != 0 { self.u16()? } else { 0 };
        let ext = if flags & 4 != 0 { self.u32()? } else { 0 };
        let s = self.chars(n, flags & 1 != 0, cp)?;
        self.take(usize::from(runs) * 4 + ext as usize)?;
        Ok(s)
    }
}
