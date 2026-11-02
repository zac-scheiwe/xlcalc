#[derive(Default)]
pub(crate) struct Recs {
    pub bytes: Vec<u8>,
}

impl Recs {
    pub fn rec(&mut self, id: u32, data: &[u8]) {
        if id < 0x80 {
            self.bytes.push(id as u8);
        } else {
            self.bytes.push((id & 0x7F) as u8 | 0x80);
            self.bytes.push((id >> 7) as u8);
        }
        let mut n = data.len();
        loop {
            let b = (n & 0x7F) as u8;
            n >>= 7;
            if n == 0 {
                self.bytes.push(b);
                break;
            }
            self.bytes.push(b | 0x80);
        }
        self.bytes.extend_from_slice(data);
    }

    pub fn with(&mut self, id: u32, f: impl FnOnce(&mut P)) {
        let mut p = P::default();
        f(&mut p);
        self.rec(id, &p.0);
    }

    pub fn empty(&mut self, id: u32) {
        self.rec(id, &[]);
    }
}

#[derive(Default)]
pub(crate) struct P(pub Vec<u8>);

impl P {
    pub fn u8(&mut self, v: u8) -> &mut Self {
        self.0.push(v);
        self
    }
    pub fn u16(&mut self, v: u16) -> &mut Self {
        self.0.extend(v.to_le_bytes());
        self
    }
    pub fn u32(&mut self, v: u32) -> &mut Self {
        self.0.extend(v.to_le_bytes());
        self
    }
    pub fn f64(&mut self, v: f64) -> &mut Self {
        self.0.extend(v.to_le_bytes());
        self
    }
    pub fn raw(&mut self, v: &[u8]) -> &mut Self {
        self.0.extend_from_slice(v);
        self
    }

    pub fn wstr(&mut self, s: &str) -> &mut Self {
        let units: Vec<u16> = s.encode_utf16().collect();
        self.u32(units.len() as u32);
        for u in units {
            self.u16(u);
        }
        self
    }

    pub fn null_str(&mut self) -> &mut Self {
        self.u32(0xFFFF_FFFF)
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn ids_and_lengths_are_variable() {
        let mut r = Recs::default();
        r.rec(0x25, &[1, 2]);
        r.rec(0x1000, &[0; 200]);
        assert_eq!(&r.bytes[..4], &[0x25, 2, 1, 2]);
        assert_eq!(&r.bytes[4..8], &[0x80, 0x20, 0xC8, 0x01]);
    }
}
