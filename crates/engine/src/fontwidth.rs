static METRICS: &[u8] = include_bytes!("colwidth_big.bin");

const FONTS: [&str; 15] = [
    "Aptos Narrow",
    "Calibri",
    "Arial",
    "Times New Roman",
    "Courier New",
    "Segoe UI",
    "Verdana",
    "Tahoma",
    "Consolas",
    "Cambria",
    "Georgia",
    "Aptos",
    "Arial Narrow",
    "Arial Black",
    "Aptos Display",
];

const PPEM: (u32, u32) = (12, 819);
const ROWS: usize = FONTS.len() * 4;
const DESIGN_BYTES: usize = ROWS * 95 * 2;
const ASC_BYTES: usize = ROWS * (PPEM.1 - PPEM.0 + 1) as usize * 2;

fn u16_at(at: usize) -> Option<u32> {
    let b = METRICS.get(at..at + 2)?;
    Some(u32::from(u16::from_le_bytes([b[0], b[1]])))
}

fn advance(row: usize, ppem: u32, ch: char) -> Option<u32> {
    let n = ch as u32;
    if !(32..=126).contains(&n) || !(PPEM.0..=PPEM.1).contains(&ppem) {
        return None;
    }
    let design = u16_at((row * 95 + (n - 32) as usize) * 2)?;
    let scaled = (design * ppem + 1024) / 2048;

    let base = DESIGN_BYTES + ASC_BYTES;
    let count = METRICS
        .get(base..base + 4)
        .map(|b| u32::from_le_bytes([b[0], b[1], b[2], b[3]]) as usize)?;
    let entry = |i: usize| -> Option<((usize, u32, u32), i64)> {
        let e = METRICS.get(base + 4 + i * 5..base + 9 + i * 5)?;
        let key = (
            usize::from(e[0]),
            u32::from(u16::from_le_bytes([e[1], e[2]])),
            u32::from(e[3]),
        );
        Some((key, i64::from(e[4] as i8)))
    };
    let key = (row, ppem, n);
    let (mut lo, mut hi) = (0, count);
    while lo < hi {
        let mid = (lo + hi) / 2;
        let (k, d) = entry(mid)?;
        match k.cmp(&key) {
            std::cmp::Ordering::Less => lo = mid + 1,
            std::cmp::Ordering::Greater => hi = mid,
            std::cmp::Ordering::Equal => return u32::try_from(i64::from(scaled) + d).ok(),
        }
    }
    Some(scaled)
}

fn ascent(row: usize, ppem: u32) -> Option<u32> {
    let i = row * (PPEM.1 - PPEM.0 + 1) as usize + (ppem - PPEM.0) as usize;
    u16_at(DESIGN_BYTES + i * 2)
}

pub(crate) const CCH_COL_DFT: u32 = 8;

const BEST_FIT_MIN: u32 = 2;

pub(crate) fn best_fit_growth(w: u32, normal_d: u32) -> u32 {
    (BEST_FIT_MIN * w / (CCH_COL_DFT * normal_d.max(1))).max(BEST_FIT_MIN)
}

pub(crate) fn best_fit_room(w: u32, normal_d: u32) -> u32 {
    (BEST_FIT_MIN * w / (CCH_COL_DFT * normal_d.max(1) + BEST_FIT_MIN)).max(BEST_FIT_MIN)
}

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub(crate) struct FontMeasure {
    row: usize,

    ppem: u32,

    pub digit: u32,

    pub(crate) g: u32,

    pub c: u32,
}

impl FontMeasure {
    pub(crate) fn new(f: &crate::style::Font) -> Option<FontMeasure> {
        let font = FONTS.iter().position(|n| n.eq_ignore_ascii_case(&f.name))?;
        let row = font * 4 + usize::from(f.italic) + 2 * usize::from(f.bold);
        let full = (f.size_twips + 5) / 10;
        let ppem = if f.superscript || f.subscript {
            (2 * full + 1) / 3
        } else {
            full
        };
        if ppem < PPEM.0 || full > PPEM.1 {
            return None;
        }
        let c = if f.italic { ascent(row, full)? / 5 } else { 0 };
        let digit = advance(row, ppem, '0')?;
        Some(FontMeasure {
            row,
            ppem,
            digit,
            g: digit.div_ceil(4),
            c,
        })
    }

    pub(crate) fn advance(&self, ch: char) -> Option<u32> {
        advance(self.row, self.ppem, ch)
    }

    pub(crate) fn fit(&self, w: u32, normal_d: u32) -> u32 {
        w + best_fit_growth(w, normal_d) + 2 * self.g + self.c + 1
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::style::Font;

    fn font(name: &str, pt2: u32, bold: bool, italic: bool) -> Font {
        Font {
            name: name.into(),
            size_twips: pt2 * 10,
            bold,
            italic,
            ..crate::style::Style::default().font
        }
    }

    #[test]
    fn best_fit_follows_the_normal_font() {
        assert_eq!(
            (
                best_fit_growth(0, 11),
                best_fit_growth(87, 11),
                best_fit_growth(88, 11),
                best_fit_growth(132, 11)
            ),
            (2, 2, 2, 3)
        );

        assert_eq!((best_fit_growth(224, 14), best_fit_growth(223, 14)), (4, 3));

        assert_eq!(best_fit_growth(560, 7), 20);
    }

    #[test]
    fn traced_fits() {
        let m = FontMeasure::new(&font("Aptos Narrow", 22, false, false)).unwrap();
        assert_eq!((m.digit, m.c), (11, 0));
        assert_eq!(m.fit(11 + 204, 11), 226);
        let m = FontMeasure::new(&font("Aptos Narrow", 819, false, true)).unwrap();
        assert_eq!((m.digit, m.c), (415, 165));
        assert_eq!(m.fit(415 + 165 + 204, 11), 1175);
        let m = FontMeasure::new(&font("Aptos Narrow", 22, true, true)).unwrap();
        assert_eq!(m.fit(11 + 4 + 204, 11), 234);
        let mut f = font("Calibri", 200, false, false);
        f.superscript = true;
        let m = FontMeasure::new(&f).unwrap();
        assert_eq!(m.digit, 67);
        assert_eq!(m.fit(67 + 204, 11), 312);

        let mut f = font("Courier New", 144, true, true);
        f.superscript = true;
        let m = FontMeasure::new(&f).unwrap();
        assert_eq!((m.digit, m.c), (58, 24));
        assert_eq!(m.fit(58 + 24 + 204, 11), 347);
        let m = FontMeasure::new(&font("Arial Black", 40, false, false)).unwrap();
        assert_eq!(m.fit(27 + 204, 11), 251);
        let m = FontMeasure::new(&font("Aptos Display", 40, true, true)).unwrap();
        assert_eq!((m.digit, m.c), (22, 8));
        assert_eq!(m.fit(22 + 8 + 204, 11), 260);
    }
}
