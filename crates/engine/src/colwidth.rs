use crate::address::{CellAddr, Rect, SheetId};
use crate::numfmt::{
    format_general_cell, NumberFormat, FILL_MARK, GENERAL_MARK, SPACE_MARK, TEXT_GENERAL_WIDTH,
};
use crate::value::Value;
use crate::workbook::Workbook;

pub const STANDARD_PX: u32 = 96;

const DIGIT_PX: u32 = 11;
const PAD_PX: u32 = 7;

const WIDEN_PX: u32 = 9;

pub(crate) fn fit_px(s: u32) -> u32 {
    (s + WIDEN_PX).max(s * 45 / 44 + PAD_PX)
}

pub fn digit_px(name: &str, size_twips: u32) -> u32 {
    use crate::colwidth_data::DIGIT_ROWS;
    let table = DIGIT_ROWS
        .iter()
        .find(|(n, _)| n.eq_ignore_ascii_case(name))
        .or_else(|| DIGIT_ROWS.iter().find(|(n, _)| *n == "Arial"))
        .map_or(&[][..], |(_, t)| *t);

    let halves = ((size_twips + 5) / 10).max(10) as usize;
    let i = halves - 10;
    match table.get(i) {
        Some(&d) => u32::from(d),
        None => {
            let last = table.last().map_or(11, |&d| u32::from(d));
            last + ((i + 1 - table.len()) / 2) as u32
        }
    }
}

fn pad_for(d: u32) -> u32 {
    2 * d.div_ceil(4) + 1
}

pub fn standard_px_for(d: u32) -> u32 {
    (8 * d + pad_for(d)).div_ceil(8) * 8
}

pub fn px_to_width_d(px: u32, d: u32) -> f64 {
    if d == DIGIT_PX {
        return px_to_width(px);
    }
    let pad = pad_for(d);
    let w = if px >= d + pad {
        f64::from(px - pad) / f64::from(d)
    } else {
        f64::from(px) / f64::from(d + pad)
    };
    (w * 100.0).round() / 100.0
}

pub fn width_to_px_d(cw: f64, d: u32) -> u32 {
    if d == DIGIT_PX {
        return width_to_px(cw);
    }
    let pad = pad_for(d);
    let px = if cw >= 1.0 {
        (cw * f64::from(d) + f64::from(pad) + 0.5).floor()
    } else {
        (cw * f64::from(d + pad) + 0.5).floor()
    };
    (px.max(0.0) as u32).min(255 * d + pad)
}

const MAX_PX: u32 = 2812;

const ASCII_PX: [u8; 95] = [
    4, 7, 7, 11, 11, 17, 13, 4, 7, 7, 11, 11, 6, 7, 6, 7, 11, 11, 11, 11, 11, 11, 11, 11, 11, 11,
    6, 6, 11, 11, 11, 10, 18, 12, 12, 14, 14, 11, 11, 14, 14, 5, 7, 11, 10, 16, 14, 15, 12, 15, 12,
    11, 10, 14, 12, 18, 11, 11, 10, 6, 7, 6, 11, 9, 11, 11, 11, 11, 11, 11, 6, 10, 11, 5, 5, 10, 5,
    17, 11, 11, 11, 11, 7, 10, 7, 11, 9, 14, 9, 9, 9, 6, 10, 6, 11,
];

pub fn char_px(c: char) -> u32 {
    match c as u32 {
        n @ 32..=126 => u32::from(ASCII_PX[(n - 32) as usize]),
        _ => DIGIT_PX,
    }
}

pub fn text_px(s: &str) -> u32 {
    s.chars().map(char_px).sum()
}

pub fn file_width_to_px(w: f64) -> u32 {
    ((w * f64::from(DIGIT_PX) + f64::from(DIGIT_PX * DIGIT_PX) / 256.0)
        .floor()
        .max(0.0) as u32)
        .min(MAX_PX)
}

pub fn px_to_file_width(px: u32) -> f64 {
    (f64::from(px) * 256.0 / f64::from(DIGIT_PX)).round() / 256.0
}

pub fn width_to_px(cw: f64) -> u32 {
    let px = if cw >= 1.0 {
        (cw * f64::from(DIGIT_PX) + f64::from(PAD_PX) + 0.5).floor()
    } else {
        (cw * 18.0 + 0.5).floor()
    };
    (px.max(0.0) as u32).min(MAX_PX)
}

pub fn px_to_width(px: u32) -> f64 {
    let w = if px >= 18 {
        f64::from(px - PAD_PX) / f64::from(DIGIT_PX)
    } else {
        f64::from(px) / 18.0
    };
    (w * 100.0).round() / 100.0
}

enum Part {
    Char(char),

    Space(char),
    Fill(char),
    General,
}

fn parts(s: &str) -> Vec<Part> {
    let mut out = Vec::new();
    let mut it = s.chars();
    while let Some(c) = it.next() {
        out.push(match c {
            SPACE_MARK => Part::Space(it.next().unwrap_or(' ')),
            FILL_MARK => Part::Fill(it.next().unwrap_or(' ')),
            GENERAL_MARK => Part::General,
            c => Part::Char(c),
        });
    }
    out
}

fn hashes(avail: u32) -> String {
    "#".repeat((avail / DIGIT_PX) as usize)
}

impl Workbook {
    pub fn col_px(&self, sheet: SheetId, col: u32) -> u32 {
        self.views
            .get(&sheet)
            .and_then(|v| v.col_px.get(&col))
            .map_or(self.standard_px(), |w| w.0)
    }

    pub fn digit_px(&self) -> u32 {
        let f = &self.base_style().font;
        digit_px(&f.name, f.size_twips)
    }

    pub fn standard_px(&self) -> u32 {
        standard_px_for(self.digit_px())
    }

    pub fn standard_width(&self) -> f64 {
        px_to_width_d(self.standard_px(), self.digit_px())
    }

    pub fn column_width(&self, sheet: SheetId, col: u32) -> f64 {
        if self.col_hidden(sheet, col) {
            0.0
        } else {
            px_to_width_d(self.col_px(sheet, col), self.digit_px())
        }
    }

    pub fn column_points(&self, sheet: SheetId, col: u32) -> f64 {
        if self.col_hidden(sheet, col) {
            0.0
        } else {
            f64::from(self.col_px(sheet, col)) / 2.0
        }
    }

    pub fn cell_width(&self, sheet: SheetId, col: u32) -> (f64, bool) {
        if self.col_hidden(sheet, col) {
            return (0.0, false);
        }
        let custom = self
            .views
            .get(&sheet)
            .is_some_and(|v| v.col_px.contains_key(&col));
        ((self.column_width(sheet, col) + 0.5).floor(), !custom)
    }

    pub fn set_column_width(&mut self, sheet: SheetId, c0: u32, c1: u32, cw: f64) {
        if cw <= 0.0 {
            self.set_cols_hidden(sheet, c0, c1 - c0 + 1, true);
            return;
        }
        if (c0..=c1).any(|c| self.col_hidden(sheet, c)) {
            self.set_cols_hidden(sheet, c0, c1 - c0 + 1, false);
        }
        let px = width_to_px_d(cw, self.digit_px());
        let std = self.standard_px();
        let v = self.view_mut(sheet);
        for c in c0..=c1 {
            if px == std {
                v.col_px.remove(&c);
            } else {
                v.col_px.insert(c, (px, true));
            }
        }
    }

    pub fn column_state(&self, sheet: SheetId) -> Vec<(u32, u32, bool, bool)> {
        let Some(v) = self.views.get(&sheet) else {
            return Vec::new();
        };
        let std = self.standard_px();
        let mut cols: Vec<u32> = v.col_px.keys().chain(&v.hidden_cols).copied().collect();
        cols.sort_unstable();
        cols.dedup();
        cols.into_iter()
            .map(|c| {
                let (px, hand) = v.col_px.get(&c).copied().unwrap_or((std, false));
                (c, px, hand, v.hidden_cols.contains(&c))
            })
            .collect()
    }

    pub fn set_column_px(&mut self, sheet: SheetId, c0: u32, c1: u32, px: u32, by_hand: bool) {
        let v = self.view_mut(sheet);
        for c in c0..=c1 {
            v.col_px.insert(c, (px.min(MAX_PX), by_hand));
        }
    }

    pub fn widen_for_format(&mut self, sheet: SheetId, rect: Rect) {
        let Some(sh) = self.sheet(sheet) else {
            return;
        };
        let cells: Vec<(u32, u32)> = sh.cells.iter_rect(rect).map(|(r, c, _)| (r, c)).collect();
        let mut need: Vec<(u32, u32)> = Vec::new();
        for (r, c) in cells {
            let addr = CellAddr::new(sheet, r, c);
            if let Some(px) = self.text_px_for_widen(addr) {
                need.push((c, fit_px(px)));
            }
        }
        for (c, px) in need {
            let set = self
                .views
                .get(&sheet)
                .and_then(|v| v.col_px.get(&c))
                .copied();
            if self.col_hidden(sheet, c) || set.is_some_and(|s| s.1) {
                continue;
            }
            if px > set.map_or(self.standard_px(), |s| s.0) {
                self.view_mut(sheet)
                    .col_px
                    .insert(c, (px.min(MAX_PX), false));
            }
        }
    }

    pub fn autofit_columns(&mut self, sheet: SheetId, rect: Rect) {
        let Some(sh) = self.sheet(sheet) else {
            return;
        };
        let cells: Vec<(u32, u32)> = sh.cells.iter_rect(rect).map(|(r, c, _)| (r, c)).collect();
        let mut widest: std::collections::BTreeMap<u32, u32> = Default::default();
        for (r, c) in cells {
            if let Some(px) = self.full_text_px(CellAddr::new(sheet, r, c)) {
                let w = widest.entry(c).or_default();
                *w = (*w).max(fit_px(px));
            }
        }
        for (c, px) in widest {
            if !self.col_hidden(sheet, c) {
                self.view_mut(sheet)
                    .col_px
                    .insert(c, (px.min(MAX_PX), false));
            }
        }
    }

    fn full_text_px(&self, addr: CellAddr) -> Option<u32> {
        let x = match self.value(addr) {
            Value::Number(x) => x,
            Value::Blank => return None,
            _ => return Some(text_px(&self.display_text(addr))),
        };
        let f = NumberFormat::parse(self.number_format(addr)).ok()?;
        let out = f
            .format_cell_number(x, &self.locale, self.settings.date_system)
            .ok()?;
        let mut px = 0;
        for p in parts(&out.text) {
            px += match p {
                Part::Char(c) | Part::Space(c) => char_px(c),
                Part::Fill(_) => 0,

                Part::General => {
                    let g = format_general_cell(x.abs(), TEXT_GENERAL_WIDTH).unwrap_or_default();
                    g.chars().count() as u32 * DIGIT_PX
                }
            };
        }
        Some(px)
    }

    fn text_px_for_widen(&self, addr: CellAddr) -> Option<u32> {
        let x = match self.value(addr) {
            Value::Number(x) => x,
            Value::Bool(_) => return Some(text_px(&self.display_text(addr))),
            _ => return None,
        };
        let f = NumberFormat::parse(self.number_format(addr)).ok()?;
        let out = f
            .format_cell_number(x, &self.locale, self.settings.date_system)
            .ok()?;
        let parts = parts(&out.text);
        let mut px = 0;
        let mut general = false;
        for p in &parts {
            match p {
                Part::Char(c) | Part::Space(c) => px += char_px(*c),
                Part::Fill(_) => {}
                Part::General => general = true,
            }
        }
        if general {
            let bare = matches!(
                parts.as_slice(),
                [Part::General] | [Part::Char('-'), Part::General]
            );
            if bare || !self.shown_text(addr).bytes().all(|b| b == b'#') {
                return None;
            }
            let g = (1..=TEXT_GENERAL_WIDTH).find_map(|k| format_general_cell(x.abs(), k))?;
            px += text_px(&g);
        }
        Some(px)
    }

    pub(crate) fn filter_text(&self, addr: CellAddr) -> String {
        let s = self.shown_text(addr);
        if !s.is_empty() && s.bytes().all(|b| b == b'#') {
            self.display_text(addr)
        } else {
            s
        }
    }

    fn cell_room(&self, addr: CellAddr) -> u32 {
        let (c0, c1) = self
            .merge_at(addr)
            .map_or((addr.col, addr.col), |m| (m.c0, m.c1));
        let px: u32 = (c0..=c1)
            .filter(|&c| !self.col_hidden(addr.sheet, c))
            .map(|c| self.col_px(addr.sheet, c))
            .sum();
        px.saturating_sub(PAD_PX)
    }

    pub fn shown_text(&self, addr: CellAddr) -> String {
        let Value::Number(x) = self.value(addr) else {
            return self.display_text(addr);
        };
        if self.col_hidden(addr.sheet, addr.col) {
            return self.display_text(addr);
        }
        let avail = self.cell_room(addr);
        let code = self.number_format(addr);
        let Some(f) = NumberFormat::parse(code).ok() else {
            return self.display_text(addr);
        };
        let Ok(out) = f.format_cell_number(x, &self.locale, self.settings.date_system) else {
            return hashes(avail);
        };
        let parts = parts(&out.text);

        match parts.as_slice() {
            [Part::General] | [Part::Char('-'), Part::General] => {
                let neg = parts.len() == 2;
                let g = x.abs() * if neg { -1.0 } else { 1.0 };
                let chars = if neg && avail >= char_px('-') {
                    (avail - char_px('-')) / DIGIT_PX + 1
                } else {
                    avail / DIGIT_PX
                };
                return format_general_cell(g, chars as usize).unwrap_or_else(|| hashes(avail));
            }
            _ => {}
        }
        let mut used = 0;
        let mut fill = None;
        for p in &parts {
            match p {
                Part::Char(c) => used += char_px(*c),
                Part::Space(_) => used += char_px(' '),
                Part::Fill(c) if fill.is_none() => fill = Some(*c),
                _ => {}
            }
        }
        let general = if parts.iter().any(|p| matches!(p, Part::General)) {
            let room = avail.saturating_sub(used) / DIGIT_PX;
            match format_general_cell(x.abs(), room as usize) {
                Some(g) => {
                    used += text_px(&g);
                    g
                }
                None => return hashes(avail),
            }
        } else {
            String::new()
        };
        if used > avail {
            return hashes(avail);
        }
        let mut s = String::new();
        let mut filled = false;
        for p in parts {
            match p {
                Part::Char(c) => s.push(c),
                Part::Space(_) => s.push(' '),
                Part::General => s.push_str(&general),
                Part::Fill(c) if !filled => {
                    filled = true;
                    let n = (avail - used) / char_px(c).max(1);
                    s.extend(std::iter::repeat_n(c, n as usize));
                }
                Part::Fill(_) => {}
            }
        }
        s
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn widths_round_trip() {
        assert_eq!(width_to_px(8.43), 100);
        assert_eq!(px_to_width(100), 8.45);
        assert_eq!(width_to_px(0.4), 7);
        assert_eq!(px_to_width(7), 0.39);
        assert_eq!(px_to_width(STANDARD_PX), 8.09);
        for px in [1, 17, 18, 96, 111, 2812] {
            assert_eq!(file_width_to_px(px_to_file_width(px)), px);
        }
    }

    #[test]
    fn format_put_widens_and_text_fits() {
        let mut wb = Workbook::new();
        let s = wb.sheet_at(0).id;
        let a = |r, c| CellAddr::new(s, r, c);
        wb.set_formula(a(0, 4), "=DATE(2024,2,29)").unwrap();
        wb.set_number_format(s, Rect::cell(0, 4), "d-mmm-yy");
        wb.widen_for_format(s, Rect::cell(0, 4));
        assert_eq!(wb.column_width(s, 4), 8.45);
        assert_eq!(wb.cell_width(s, 4), (8.0, false));
        wb.set_value(a(0, 1), Value::Number(1234.5678));
        assert_eq!(wb.shown_text(a(0, 1)), "1234.568");
        wb.set_column_width(s, 1, 1, 2.0);
        assert_eq!(wb.shown_text(a(0, 1)), "##");
        wb.set_number_format(s, Rect::cell(0, 1), "* 0");
        wb.set_column_width(s, 1, 1, 8.09);
        assert_eq!(wb.shown_text(a(0, 1)), format!("{}1235", " ".repeat(11)));
    }
}
