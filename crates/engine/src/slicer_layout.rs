use crate::drawing::Emu4;
use crate::style::Font;

const SCREEN_DPI: i64 = 144;
const EMU_PER_INCH: i64 = 914_400;

const INSET: i64 = 85_725;

const BELOW_CAPTION: i64 = 47_625;

const GAP: i64 = 28_575;

const SCROLL_BAR: i64 = 209_550;

pub fn header_ascent(base: &Font) -> i64 {
    let ppem = (i64::from(base.size_twips) * SCREEN_DPI + 720) / 1440;
    let ppem = u32::try_from(ppem).unwrap_or(u32::MAX);
    i64::from(crate::gdi_metrics::tm(&base.name, true, base.italic, ppem).ascent)
}

fn caption_height(ascent_px: i64) -> i64 {
    let ascent = (ascent_px.max(1) * EMU_PER_INCH + SCREEN_DPI / 2) / SCREEN_DPI;
    (ascent * 5 / 3 + 38_100).max(219_075)
}

fn round(x: f64) -> i64 {
    (x + if x < 0.0 { -0.5 } else { 0.5 }) as i64
}

fn scales() -> (f64, f64) {
    let s = SCREEN_DPI as f64 / EMU_PER_INCH as f64;
    (s, s / (s * s))
}

fn scaled(v: i64, s: f64) -> f64 {
    let (low, high) = ((-2_147_483_648.0 / s) as i64, (2_147_483_647.0 / s) as i64);
    v.clamp(low, high) as f64 * s
}

fn rect_through_screen(top: i64, bottom: i64) -> (i64, i64) {
    let (s, inv) = scales();
    let back = |px: f64| round(f64::from(px as i32) * inv);
    (back(scaled(top, s).floor()), back(scaled(bottom, s).ceil()))
}

fn through_screen(v: i64) -> i64 {
    let (s, inv) = scales();
    round(f64::from(round(scaled(v, s)) as i32) * inv)
}

pub struct SlicerBox {
    pub rect: Emu4,
    pub header: bool,

    pub ascent: i64,
    pub columns: i64,
    pub items: i64,
    pub row_height: i64,
}

impl SlicerBox {
    fn item_rows(&self) -> (i64, i64) {
        let (_, y, _, h) = self.rect;
        let end = y.saturating_add(h);
        let top = if self.header {
            y.saturating_add(caption_height(self.ascent))
                .min(end)
                .saturating_add(BELOW_CAPTION)
        } else {
            y.saturating_add(INSET)
        };
        let top = top.min(end).max(y);
        let bottom = end.saturating_sub(INSET).min(end).max(top);
        rect_through_screen(top, bottom)
    }

    fn scroll_bar(&self) -> bool {
        let (top, bottom) = self.item_rows();
        let area = bottom.saturating_sub(top);
        let item = through_screen(self.row_height);
        let gap = through_screen(GAP);
        let rows = (self.items - 1) / self.columns + 1;
        let (visible, partial) = if area < item {
            (1, true)
        } else {
            let step = (item + gap).max(1);
            let whole = ((area - item) / step).clamp(i64::from(i32::MIN), i64::from(i32::MAX));
            if area - item - whole * step > gap {
                (whole + 2, true)
            } else {
                (whole + 1, false)
            }
        };
        visible < rows || (visible == rows && partial && rows != 1)
    }

    pub fn item_width(&self) -> i64 {
        let (x, _, w, _) = self.rect;
        let end = x.saturating_add(w);
        let left = x.saturating_add(INSET).min(end).max(x);
        let right = end.saturating_sub(INSET).min(end).max(left);
        let bar = if self.scroll_bar() { SCROLL_BAR } else { 0 };
        let between = (self.columns - 1).saturating_mul(GAP);
        ((right - left - bar - between) / self.columns).max(0)
    }
}

#[cfg(test)]
mod tests {
    use super::SlicerBox;

    fn ten(width: i64, height_pt: f64, header: bool, columns: i64, row_height: i64) -> i64 {
        SlicerBox {
            rect: (8_159_750, 3_048_000, width, (height_pt * 12_700.0) as i64),
            header,
            ascent: 22,
            columns,
            items: 10,
            row_height,
        }
        .item_width()
    }

    const DEFAULT_ROW: i64 = 251_883;

    #[test]
    fn scroll_bar_edges() {
        let (full, bar) = (1_733_550, 1_524_000);
        for (height, header, row, want) in [
            (233.0, false, DEFAULT_ROW, bar),
            (235.8, false, DEFAULT_ROW, full),
            (185.0, false, 190_500, bar),
            (187.75, false, 190_500, full),
            (203.75, true, 190_500, bar),
            (204.0, true, 190_500, full),
            (235.0, false, 256_540, bar),
            (235.5, false, 256_540, full),
        ] {
            assert_eq!(
                ten(1_905_000, height, header, 1, row),
                want,
                "{height} {header} {row}"
            );
        }
    }

    #[test]
    fn columns_and_clamps() {
        assert_eq!(ten(1_905_000, 80.25, false, 3, 190_500), 488_950);
        assert_eq!(ten(1_905_000, 80.3, false, 3, 190_500), 558_800);

        assert_eq!(ten(1_905_000, 20.0, false, 10, 190_500), 147_637);

        assert_eq!(ten(0, 206.0, false, 1, DEFAULT_ROW), 0);
        assert_eq!(ten(63_500, 206.0, false, 4, DEFAULT_ROW), 0);
    }
}
