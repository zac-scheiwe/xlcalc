use std::sync::OnceLock;

static DATA: &[u8] = include_bytes!("gdi_metrics.bin");
const ENTRY: usize = 10;
const FIRST_PPEM: u32 = 2;

struct FontRows {
    name: String,
    pmax: u32,

    at: Option<usize>,
}

fn index() -> &'static [FontRows] {
    static INDEX: OnceLock<Vec<FontRows>> = OnceLock::new();
    INDEX.get_or_init(|| {
        let mut p = 0;
        let n = usize::from(u16::from_le_bytes([DATA[0], DATA[1]]));
        p += 2;
        let mut fonts = Vec::new();
        for _ in 0..n {
            let len = usize::from(DATA[p]);
            let name = String::from_utf8_lossy(&DATA[p + 1..p + 1 + len]).to_lowercase();
            p += 1 + len;
            let present = DATA[p] != 0;
            let pmax = u32::from(u16::from_le_bytes([DATA[p + 1], DATA[p + 2]]));
            p += 3;
            fonts.push(FontRows {
                name,
                pmax,
                at: present.then_some(0),
            });
        }

        for f in &mut fonts {
            if f.at.is_some() {
                f.at = Some(p);
                p += 4 * (f.pmax - FIRST_PPEM + 1) as usize * ENTRY;
            }
        }
        fonts
    })
}

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub(crate) struct Tm {
    pub ascent: i32,
    pub descent: i32,
    pub internal_leading: i32,
    pub external_leading: i32,

    pub digit: i32,
}

fn row(font: &FontRows, face: usize, ppem: u32) -> Option<Tm> {
    let base = font.at?;
    let i = (face * (font.pmax - FIRST_PPEM + 1) as usize + (ppem - FIRST_PPEM) as usize) * ENTRY;
    let e = DATA.get(base + i..base + i + ENTRY)?;
    let s = |k: usize| i32::from(i16::from_le_bytes([e[k], e[k + 1]]));
    Some(Tm {
        ascent: s(0),
        descent: s(2),
        internal_leading: s(4),
        external_leading: i32::from(u16::from_le_bytes([e[6], e[7]])),
        digit: i32::from(u16::from_le_bytes([e[8], e[9]])),
    })
}

pub(crate) fn tm(name: &str, bold: bool, italic: bool, ppem: u32) -> Tm {
    let n = name.to_lowercase();
    let idx = index();
    let font = idx
        .iter()
        .find(|f| f.name == n && f.at.is_some())
        .or_else(|| idx.iter().find(|f| f.name == "arial"))
        .expect("Arial is in the table");
    let face = usize::from(italic) + 2 * usize::from(bold);
    row(font, face, ppem.clamp(FIRST_PPEM, font.pmax)).expect("row in range")
}

pub(crate) fn ascent_descent(t: &Tm) -> (u32, u32) {
    let el = t.external_leading;
    let k = (t.internal_leading + el / 2)
        .max((el - el / 2 - 1).max(0) + t.descent)
        .max(2);
    let asc = (k - t.internal_leading + t.ascent).max(0);
    (asc as u32, (k + 1) as u32)
}

const SMALL_ITALIC_OVERHANG: [u32; 10] = [1, 1, 2, 2, 2, 3, 4, 5, 5, 6];

pub(crate) fn overhang(name: &str, bold: bool, italic: bool, ppem: u32) -> u32 {
    if ppem >= 12 || SYMBOL_FONTS.iter().any(|f| f.eq_ignore_ascii_case(name)) {
        return 0;
    }
    let slant = if italic {
        SMALL_ITALIC_OVERHANG[(ppem.max(2) - 2) as usize]
    } else {
        0
    };
    slant + u32::from(bold)
}

const SYMBOL_FONTS: [&str; 3] = ["Symbol", "Wingdings", "Webdings"];

pub(crate) fn digit_width(name: &str, bold: bool, italic: bool, ppem: u32) -> u32 {
    tm(name, bold, italic, ppem).digit.max(1) as u32
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn small_fonts_replace_every_font_under_six_points() {
        for name in ["Comic Sans MS", "Calibri", "Nonexistent"] {
            let t = tm(name, false, true, 8);
            assert_eq!((t.ascent, t.descent, t.digit), (8, 2, 5));
            assert_eq!(overhang(name, false, true, 8), 4);
            assert_eq!(overhang(name, true, true, 10), 6);
            assert_eq!(overhang(name, false, false, 10), 0);
        }
        assert_eq!(overhang("Symbol", false, true, 8), 0);
        assert_eq!(overhang("Calibri", false, true, 12), 0);
    }

    #[test]
    fn default_font_row() {
        let (a, d) = ascent_descent(&tm("Aptos Narrow", false, false, 22));
        assert_eq!((a, d), (22, 7));
    }

    #[test]
    fn ascent_descent_branches() {
        let t = |il, el, desc| Tm {
            ascent: 20,
            descent: desc,
            internal_leading: il,
            external_leading: el,
            digit: 10,
        };

        assert_eq!(ascent_descent(&t(3, 0, 5)), (22, 6));

        assert_eq!(ascent_descent(&t(3, 2, 5)), (22, 6));
        assert_eq!(ascent_descent(&t(3, 3, 5)), (23, 7));

        assert_eq!(ascent_descent(&t(8, 0, 4)), (20, 9));

        assert_eq!(ascent_descent(&t(0, 0, 0)), (22, 3));
    }

    #[test]
    fn digit_widths_match_the_fitted_rows() {
        assert_eq!(digit_width("Aptos Narrow", false, false, 22), 11);
        assert_eq!(digit_width("Arial", false, false, 22), 12);
        assert_eq!(digit_width("Arial", false, false, 11), 6);
        assert_eq!(digit_width("Courier New", false, false, 10), 6);
    }

    #[test]
    fn digit_width_agrees_with_the_advance_table() {
        let fonts = [
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
            "Arial Black",
            "Aptos Display",
        ];
        for name in fonts {
            for (bold, italic) in [(false, false), (false, true), (true, false), (true, true)] {
                for ppem in (12..=200).step_by(7).chain([819]) {
                    let font = crate::style::Font {
                        name: name.into(),
                        size_twips: ppem * 10,
                        bold,
                        italic,
                        ..crate::style::Style::default().font
                    };
                    let m = crate::fontwidth::FontMeasure::new(&font).unwrap();
                    let widest = "0123456789"
                        .chars()
                        .filter_map(|c| m.advance(c))
                        .max()
                        .unwrap();
                    assert_eq!(
                        digit_width(name, bold, italic, ppem),
                        widest,
                        "{name} {ppem} {bold} {italic}"
                    );
                }
            }
        }
    }

    #[test]
    fn unknown_font_uses_arial() {
        assert_eq!(
            tm("No Such Font", false, false, 22),
            tm("Arial", false, false, 22)
        );
    }
}
