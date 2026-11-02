use crate::address::{CellAddr, SheetId, MAX_COLS, MAX_ROWS};
use crate::colwidth::char_px;
use crate::rowheight_data::FONT_ROWS;
use crate::style::{BorderLine, Style};
use crate::value::Value;
use crate::workbook::Workbook;

const WRAP_PAD_PX: u32 = 9;

pub const MAX_H8: u32 = 3276;

fn font_tenths(name: &str, size_twips: u32) -> u32 {
    let table = FONT_ROWS
        .iter()
        .find(|(n, _)| n.eq_ignore_ascii_case(name))
        .or_else(|| FONT_ROWS.iter().find(|(n, _)| *n == "Arial"))
        .map(|(_, t)| *t)
        .unwrap_or(&[]);

    let halves = (size_twips + 5) / 10;
    let halves = halves.max(2);
    let idx = if halves <= 144 {
        (halves - 2) as usize
    } else {
        143 + (halves / 2).saturating_sub(73) as usize
    };
    match table.get(idx) {
        Some(&v) => u32::from(v),
        None => {
            let n = table.len();
            if n < 2 {
                return 145;
            }
            let (a, b) = (u32::from(table[n - 2]), u32::from(table[n - 1]));
            b + (b - a) * (idx + 1 - n) as u32
        }
    }
}

pub fn h8_to_points(h8: u32) -> f64 {
    ((f64::from(h8) * 1.25 - 1e-9).ceil()) / 10.0
}

pub fn points_to_h8(h: f64) -> Option<u32> {
    if h.is_nan() || !(0.0..=409.5).contains(&h) {
        return None;
    }
    let twentieths =
        (h * 20.0 + 1e-9).floor() as i64 + i64::from(((h * 20.0 + 1e-9).fract()) >= 0.5);
    let eighths = (twentieths as f64 * 0.4 + 1e-9).floor() as i64
        + i64::from(((twentieths as f64 * 0.4 + 1e-9).fract()) >= 0.5);
    Some((eighths as u32).min(MAX_H8))
}

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub struct RowHt {
    pub custom: Option<u32>,

    pub frozen: Option<u32>,
}

fn font_h(s: &Style) -> u32 {
    font_tenths(&s.font.name, s.font.size_twips)
}

fn border_extra(top: BorderLine, bottom: BorderLine) -> u32 {
    5 * (u32::from(line_px(bottom) >= 2) + u32::from(line_px(top) >= 3))
}

fn line_px(l: BorderLine) -> u32 {
    match l {
        BorderLine::None => 0,
        BorderLine::Thin
        | BorderLine::Dashed
        | BorderLine::Dotted
        | BorderLine::Hair
        | BorderLine::DashDot
        | BorderLine::DashDotDot => 1,
        BorderLine::Medium
        | BorderLine::MediumDashed
        | BorderLine::MediumDashDot
        | BorderLine::MediumDashDotDot
        | BorderLine::SlantDashDot => 2,
        BorderLine::Thick | BorderLine::Double => 3,
    }
}

fn wrap_lines(text: &str, avail: u32, wrap: bool, scale: f64) -> u32 {
    let mut lines = 0;
    for seg in text.split('\n') {
        lines += if wrap {
            seg_lines(seg, avail, scale)
        } else {
            1
        };
    }
    lines.max(1)
}

fn seg_lines(seg: &str, avail: u32, scale: f64) -> u32 {
    let px = |c: char| (f64::from(char_px(c)) * scale).round() as u32;
    let space = px(' ');
    let mut lines = 1;
    let mut x = 0u32;
    for (i, word) in seg.split(' ').enumerate() {
        let w: u32 = word.chars().map(px).sum();
        if i > 0 && x + space + w <= avail {
            x += space + w;
        } else if i == 0 && w <= avail {
            x = w;
        } else {
            if i > 0 {
                lines += 1;
            }

            x = 0;
            for ch in word.chars() {
                let cw = px(ch);
                if x + cw > avail && x > 0 {
                    lines += 1;
                    x = 0;
                }
                x += cw;
            }
        }
    }
    lines
}

impl Workbook {
    pub fn row_height(&self, sheet: SheetId, row: u32) -> f64 {
        if self.row_hidden(sheet, row) {
            return 0.0;
        }
        match self.row_custom_h8(sheet, row) {
            Some(h8) => h8_to_points(h8),
            None => f64::from(self.row_auto_tenths(sheet, row)) / 10.0,
        }
    }

    pub fn row_h8(&self, sheet: SheetId, row: u32) -> u32 {
        if let Some(h) = self.row_custom_h8(sheet, row) {
            return h;
        }
        self.row_auto_h8(sheet, row)
    }

    pub fn row_custom_h8(&self, sheet: SheetId, row: u32) -> Option<u32> {
        self.views
            .get(&sheet)
            .and_then(|v| v.row_h.get(&row))
            .and_then(|h| h.custom)
    }

    fn normal_tenths(&self) -> u32 {
        font_h(self.base_style())
    }

    pub fn standard_height(&self, sheet: SheetId) -> f64 {
        f64::from(self.standard_tenths(sheet)) / 10.0
    }

    fn standard_tenths(&self, sheet: SheetId) -> u32 {
        let mut t = self.normal_tenths();
        for ((s, _), (_, sty)) in &self.cellfmt.cols {
            if *s == sheet {
                t = t.max(font_h(sty));
            }
        }
        if let Some((_, sty)) = self.cellfmt.sheets.get(&sheet) {
            t = t.max(font_h(sty));
        }
        t
    }

    pub fn row_auto_h8(&self, sheet: SheetId, row: u32) -> u32 {
        (f64::from(self.row_auto_tenths(sheet, row)) * 0.8).round() as u32
    }

    pub fn row_auto_tenths(&self, sheet: SheetId, row: u32) -> u32 {
        let frozen = self
            .views
            .get(&sheet)
            .and_then(|v| v.row_h.get(&row))
            .and_then(|h| h.frozen);
        frozen.unwrap_or_else(|| self.row_fit_tenths(sheet, row))
    }

    pub fn auto_snapshot(&self, sheet: SheetId) -> Vec<(u32, u32)> {
        let Some(used) = self.used_range(sheet) else {
            return Vec::new();
        };
        if u64::from(used.r1 + 1) * u64::from(used.c1 + 1) > 20_000 {
            return Vec::new();
        }
        (used.r0..=used.r1)
            .filter(|&r| self.row_custom_h8(sheet, r).is_none())
            .map(|r| (r, self.row_auto_tenths(sheet, r)))
            .collect()
    }

    pub fn auto_settle(&mut self, sheet: SheetId, snap: &[(u32, u32)]) {
        for &(r, t) in snap {
            if self.row_auto_tenths(sheet, r) != t {
                self.view_mut(sheet).row_h.insert(
                    r,
                    RowHt {
                        custom: None,
                        frozen: Some(t),
                    },
                );
            }
        }
    }

    pub fn auto_refit(&mut self, sheet: SheetId, r0: u32, r1: u32) {
        if let Some(v) = self.views.get_mut(&sheet) {
            for (_, h) in v.row_h.range_mut(r0..=r1) {
                h.frozen = None;
            }
        }
    }

    pub fn holds_wrapped_text(&self, sheet: SheetId, rect: crate::Rect) -> bool {
        let rect = self
            .used_range(sheet)
            .and_then(|u| rect.intersection(&u))
            .unwrap_or(rect);
        if u64::from(rect.r1 - rect.r0 + 1) * u64::from(rect.c1 - rect.c0 + 1) > 10_000 {
            return false;
        }
        (rect.r0..=rect.r1).any(|r| {
            (rect.c0..=rect.c1).any(|c| {
                let a = CellAddr::new(sheet, r, c);
                matches!(self.value(a), Value::Text(t) if t.contains('\n') || self.cell_style(a).alignment.wrap)
            })
        })
    }

    fn row_fit_tenths(&self, sheet: SheetId, row: u32) -> u32 {
        let floor = self.standard_tenths(sheet);
        let mut tenths = floor;
        if let Some((_, sty)) = self.cellfmt.rows.get(&(sheet, row)) {
            tenths = tenths.max(font_h(sty));
        }
        let last = self.row_last_col(sheet);
        let mut c = 0;
        while c <= last {
            let a = CellAddr::new(sheet, row, c);
            c += 1;
            if self.merge_at(a).is_some() {
                continue;
            }
            let v = self.value(a);
            let own = self.own_style(a).is_some();
            if !own && v.is_blank() {
                continue;
            }
            let sty = self.cell_style(a);
            let above = if a.row > 0 {
                self.cell_style(CellAddr::new(sheet, a.row - 1, a.col))
                    .borders
                    .bottom
                    .line
            } else {
                BorderLine::None
            };
            let below = if a.row + 1 < MAX_ROWS {
                self.cell_style(CellAddr::new(sheet, a.row + 1, a.col))
                    .borders
                    .top
                    .line
            } else {
                BorderLine::None
            };
            let top = if line_px(above) > line_px(sty.borders.top.line) {
                above
            } else {
                sty.borders.top.line
            };
            let bottom = if line_px(below) > line_px(sty.borders.bottom.line) {
                below
            } else {
                sty.borders.bottom.line
            };
            let one = font_h(sty) + border_extra(top, bottom);
            let lines = match &v {
                Value::Text(t) if sty.alignment.wrap || t.contains('\n') => {
                    let avail = self.col_px(sheet, a.col).saturating_sub(WRAP_PAD_PX);
                    let scale = f64::from(sty.font.size_twips) / 220.0;
                    wrap_lines(t, avail, sty.alignment.wrap, scale)
                }
                _ => 1,
            };

            let cell = if lines > 1 {
                let first = one.max(sty.font.size_twips / 2 + 35);
                (first + one * (lines - 1)).min(4095)
            } else {
                one
            };
            tenths = tenths.max(cell);
        }
        tenths
    }

    fn row_last_col(&self, sheet: SheetId) -> u32 {
        let mut last = 0;
        if let Some(r) = self.used_range(sheet) {
            last = r.c1;
        }
        if let Some(r) = self.ever_used(sheet) {
            last = last.max(r.c1);
        }
        last.min(MAX_COLS - 1)
    }

    pub fn set_row_height(&mut self, sheet: SheetId, r0: u32, r1: u32, h: f64) -> Option<()> {
        let h8 = points_to_h8(h)?;
        if h8 == 0 {
            if let Some(v) = self.views.get_mut(&sheet) {
                for r in r0..=r1 {
                    v.row_h.remove(&r);
                }
            }
            self.set_rows_hidden(sheet, r0, r1 - r0 + 1, true).ok()?;
            return Some(());
        }
        if (r0..=r1).any(|r| self.row_hidden(sheet, r)) {
            self.set_rows_hidden(sheet, r0, r1 - r0 + 1, false).ok()?;
        }
        let v = self.view_mut(sheet);
        for r in r0..=r1 {
            v.row_h.insert(
                r,
                RowHt {
                    custom: Some(h8),
                    frozen: None,
                },
            );
        }
        Some(())
    }

    pub fn autofit_rows(&mut self, sheet: SheetId, r0: u32, r1: u32) {
        if let Some(v) = self.views.get_mut(&sheet) {
            let keys: Vec<u32> = v.row_h.range(r0..=r1).map(|(k, _)| *k).collect();
            for k in keys {
                v.row_h.remove(&k);
            }
        }
    }

    pub fn row_custom_heights(&self, sheet: SheetId) -> Vec<(u32, u32)> {
        self.views
            .get(&sheet)
            .map(|v| {
                v.row_h
                    .iter()
                    .filter_map(|(r, h)| h.custom.map(|c| (*r, c)))
                    .collect()
            })
            .unwrap_or_default()
    }

    pub fn set_row_h8(&mut self, sheet: SheetId, row: u32, h8: u32) {
        self.view_mut(sheet).row_h.insert(
            row,
            RowHt {
                custom: Some(h8.min(MAX_H8)),
                frozen: None,
            },
        );
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn heights_quantise() {
        for (set, read) in [
            (0.1, 0.2),
            (12.7, 12.8),
            (14.4, 14.4),
            (14.6, 14.7),
            (20.25, 20.3),
            (50.33, 50.4),
            (24.07, 24.0),
            (409.5, 409.5),
        ] {
            assert_eq!(h8_to_points(points_to_h8(set).unwrap()), read, "{set}");
        }
        assert!(points_to_h8(410.0).is_none());
    }

    #[test]
    fn font_table() {
        assert_eq!(font_tenths("Aptos Narrow", 220), 145);
        assert_eq!(font_tenths("Aptos Narrow", 240), 160);
        assert_eq!(font_tenths("Arial", 400), 250);
    }
}
