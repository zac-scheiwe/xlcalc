use crate::chartfont_tbl as tbl;
use std::cell::Cell;

#[derive(Clone, Copy, PartialEq, Eq, Debug)]
pub(crate) enum Family {
    AptosNarrow,
    Calibri,
    Arial,
    TimesNewRoman,
    SegoeUi,
    Consolas,
}

#[derive(Clone, Copy, PartialEq, Debug)]
pub(crate) struct TextFont {
    pub family: Family,
    pub size: f64,

    pub rich: bool,
}

impl TextFont {
    pub(crate) const DEFAULT: TextFont = TextFont {
        family: Family::AptosNarrow,
        size: 10.0,
        rich: false,
    };

    pub(crate) fn new(name: &str, size: f64) -> TextFont {
        let family = match name.to_ascii_lowercase().as_str() {
            "calibri" => Family::Calibri,
            "arial" => Family::Arial,
            "times new roman" => Family::TimesNewRoman,
            "segoe ui" => Family::SegoeUi,
            "consolas" => Family::Consolas,
            _ => Family::AptosNarrow,
        };
        TextFont {
            family,
            size,
            rich: false,
        }
    }
}

#[derive(Clone, Copy, PartialEq, Debug)]
pub(crate) struct Fonts {
    pub cat: TextFont,
    pub val: TextFont,
    pub legend: TextFont,
}

impl Fonts {
    pub(crate) const DEFAULT: Fonts = Fonts {
        cat: TextFont::DEFAULT,
        val: TextFont::DEFAULT,
        legend: TextFont::DEFAULT,
    };
}

thread_local! {
    static FONTS: Cell<Fonts> = const { Cell::new(Fonts::DEFAULT) };
}

pub(crate) struct FontScope(Fonts);

impl Drop for FontScope {
    fn drop(&mut self) {
        FONTS.with(|f| f.set(self.0));
    }
}

pub(crate) fn font_scope(f: Fonts) -> FontScope {
    FontScope(FONTS.with(|c| c.replace(f)))
}

pub(crate) fn fonts() -> Fonts {
    FONTS.with(Cell::get)
}

fn rnd(x: f64) -> i64 {
    (x + 0.5).floor() as i64
}

fn table(family: Family) -> Option<(&'static [u16; 224], (i64, i64))> {
    Some(match family {
        Family::AptosNarrow => return None,
        Family::Calibri => (&tbl::CALIBRI_ADVANCE, tbl::CALIBRI_WIN),
        Family::Arial => (&tbl::ARIAL_ADVANCE, tbl::ARIAL_WIN),
        Family::TimesNewRoman => (&tbl::TIMES_ADVANCE, tbl::TIMES_WIN),
        Family::SegoeUi => (&tbl::SEGOE_UI_ADVANCE, tbl::SEGOE_UI_WIN),
        Family::Consolas => (&tbl::CONSOLAS_ADVANCE, tbl::CONSOLAS_WIN),
    })
}

pub(crate) fn glyph_units(f: TextFont, ch: char) -> i64 {
    match table(f.family) {
        None => crate::chartparts::aptos_glyph_units(ch, f.size),
        Some((adv, _)) => {
            let cp = ch as usize;
            let a = if (32..256).contains(&cp) {
                adv[cp - 32]
            } else {
                0
            };

            let a = if a == 0 {
                adv[usize::from(b'0' - 32)]
            } else {
                a
            };
            rnd(f64::from(a) * 200.0 * f.size / 2048.0)
        }
    }
}

pub(crate) fn text_emu(f: TextFont, s: &str) -> i64 {
    if s.is_empty() {
        return 0;
    }

    if f.rich || s.chars().any(|ch| ch as u32 > 0x17F) {
        let in_arial = table(f.family).is_none()
            && s.chars().any(|c| matches!(c as u32, 0x1A0 | 0x1A1 | 0x1AF));
        let mut units: i64 = s
            .chars()
            .map(|ch| {
                in_arial
                    .then(|| crate::chartparts::arial_glyph_units(ch, f.size))
                    .flatten()
                    .unwrap_or_else(|| glyph_units(f, ch))
            })
            .sum();

        if s.ends_with(' ') {
            units += 1;
        }
        return ((127 * units + 1) / 2).max(1);
    }
    s.chars()
        .map(|ch| (127 * glyph_units(f, ch) + 1) / 2)
        .sum::<i64>()
        .max(1)
}

fn win_metrics(f: TextFont) -> (i64, i64) {
    table(f.family).map_or((1923, 577), |t| t.1)
}

pub(crate) fn asc_desc_units(f: TextFont) -> (i64, i64) {
    let (asc, desc) = win_metrics(f);
    let u = |m: i64| rnd(m as f64 * 200.0 * f.size / 2048.0);
    (u(asc), u(desc))
}

pub(crate) fn emu(units: i64) -> i64 {
    (127 * units + 1) / 2
}

fn asc_desc_emu(f: TextFont) -> (i64, i64) {
    let (asc, desc) = win_metrics(f);
    let u = |m: i64| emu(rnd(m as f64 * 200.0 * f.size / 2048.0));
    (u(asc), u(desc))
}

pub(crate) fn line_emu(f: TextFont) -> i64 {
    let (a, d) = asc_desc_emu(f);
    a + d
}

pub(crate) fn legend_line_emu(f: TextFont) -> i64 {
    (1.1 * legend_key_emu(f) as f64) as i64
}

pub(crate) fn legend_key_emu(f: TextFont) -> i64 {
    (0.9 * line_emu(f) as f64).floor() as i64
}

pub(crate) fn label_offset_emu(f: TextFont) -> i64 {
    asc_desc_emu(f).0 / 3
}

pub(crate) fn side_gap_emu(f: TextFont) -> i64 {
    let (a, d) = asc_desc_emu(f);
    ((a + d) >> 1) + label_offset_emu(f) - 57_150
}

pub(crate) fn foot_emu(f: TextFont) -> i64 {
    let (a, d) = asc_desc_emu(f);
    a + d + 2 * label_offset_emu(f) - 57_150
}

pub(crate) fn half_tick_emu(f: TextFont) -> i64 {
    ((line_emu(f) - 152_400) >> 1).max(0)
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn defaults_match_the_traced_aptos_narrow_numbers() {
        let d = TextFont::DEFAULT;
        assert_eq!(line_emu(d), 155_004);
        assert_eq!(legend_line_emu(d), 153_453);
        assert_eq!(legend_key_emu(d), 139_503);
    }

    #[test]
    fn reserves_follow_the_font_metrics() {
        let d = TextFont::DEFAULT;
        assert_eq!(side_gap_emu(d), 60_103);
        assert_eq!(foot_emu(d), 177_356);
        assert_eq!(half_tick_emu(d), 1_302);
        let c = TextFont::new("Calibri", 12.0);
        assert_eq!(side_gap_emu(c), 84_244);
        assert_eq!(half_tick_emu(c), 16_828);
        assert_eq!(foot_emu(c), 225_638);
        let a = TextFont::new("Arial", 24.0);
        assert_eq!(foot_emu(a), 467_276);
    }

    #[test]
    fn legend_line_and_key_are_scaled_integers() {
        let c = TextFont::new("Calibri", 12.0);
        assert_eq!(legend_key_emu(c), 167_450);
        assert_eq!(legend_line_emu(c), 184_195);

        assert_eq!(legend_line_emu(TextFont::new("Calibri", 14.0)), 214_871);
    }

    #[test]
    fn calibri_legend_widths_follow_the_font() {
        let f = TextFont::new("Calibri", 12.0);
        assert_eq!(text_emu(f, "Wi") - text_emu(f, "x"), 104_585);
        assert_eq!(line_emu(TextFont::new("Arial", 10.0)), 141_923);
    }
}
