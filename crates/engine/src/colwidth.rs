use crate::address::{CellAddr, Rect, SheetId};
use crate::fontwidth::FontMeasure;
use crate::numfmt::{
    format_general_cell, format_general_cell_by, general_best_fit, general_display_flag,
    NumberFormat, FILL_MARK, GENERAL_MARK, SPACE_MARK, TEXT_GENERAL_WIDTH,
};
use crate::style::HAlign;
use crate::value::Value;
use crate::workbook::Workbook;

pub const STANDARD_PX: u32 = 96;

const DIGIT_PX: u32 = 11;
const PAD_PX: u32 = 7;

const INDENT_PX: u32 = 12;

fn indent_px(level: u8, h: HAlign) -> u32 {
    let one = INDENT_PX * u32::from(level);
    if h == HAlign::Distributed {
        2 * one
    } else {
        one
    }
}

pub(crate) fn fit_px(s: u32, normal_d: u32) -> u32 {
    s + crate::fontwidth::best_fit_growth(s, normal_d) + PAD_PX
}

pub fn digit_px(name: &str, size_twips: u32, bold: bool, italic: bool) -> u32 {
    let ppem = (size_twips + 5) / 10;
    crate::gdi_metrics::digit_width(name, bold, italic, ppem)
}

fn pad_for(d: u32) -> u32 {
    2 * d.div_ceil(4) + 1
}

pub fn standard_px_for(d: u32) -> u32 {
    (8 * d + pad_for(d)).div_ceil(8) * 8
}

pub fn px_to_width_d(px: u32, d: u32) -> f64 {
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

const BOLD_ASCII_PX: [u8; 95] = [
    4, 7, 8, 11, 11, 17, 13, 5, 7, 7, 11, 11, 6, 7, 6, 8, 11, 11, 11, 11, 11, 11, 11, 11, 11, 11,
    6, 6, 11, 11, 11, 10, 17, 12, 12, 14, 14, 11, 11, 14, 14, 6, 7, 12, 10, 16, 14, 14, 12, 14, 13,
    12, 10, 14, 12, 18, 12, 12, 11, 7, 8, 7, 11, 9, 11, 11, 12, 11, 12, 11, 7, 10, 12, 6, 6, 11, 6,
    18, 12, 11, 12, 12, 7, 10, 7, 12, 10, 15, 10, 10, 9, 7, 10, 7, 11,
];

fn bold_char_px(c: char) -> u32 {
    match c as u32 {
        n @ 32..=126 => u32::from(BOLD_ASCII_PX[(n - 32) as usize]),
        _ => DIGIT_PX,
    }
}

const FILTER_BUTTON_PX: u32 = 25;

struct HeaderFit {
    row: u32,
    c0: u32,
    c1: u32,
    bold: bool,
    button: u32,
}

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub(crate) struct PivotFit {
    pub row: u32,
    pub col: u32,
    pub bold: bool,
    pub extra: u32,
}

impl PivotFit {
    pub(crate) fn header(row: u32, col: u32, extra: u32) -> PivotFit {
        PivotFit {
            row,
            col,
            bold: true,
            extra,
        }
    }
}

pub(crate) struct ColFit {
    pub compact: bool,
    pub left_cols: u32,
    pub levels: Vec<u8>,
}

impl ColFit {
    fn groups(
        &self,
        c0: u32,
        c1: u32,
        widest: &std::collections::BTreeMap<u32, u32>,
    ) -> Vec<(u32, u32)> {
        let outline = !self.compact || self.left_cols == 0;
        let lv = |c: u32| self.levels.get((c - c0) as usize).copied().unwrap_or(1);
        let top = (c0..=c1).map(lv).max().unwrap_or(1);
        let flat = (c0..=c1).map(lv).min().unwrap_or(1) == top;
        let mut out = Vec::new();
        let mut c = c0;
        while c <= c1 {
            let own = widest.get(&c).copied();
            if !outline {
                out.extend(own.map(|w| (c, w)));
            } else if c + 1 < c0 + self.left_cols {
            } else if c + 1 == c0 + self.left_cols || flat || lv(c) != top {
                out.extend(own.map(|w| (c, w)));
            } else {
                let mut end = c;
                while end < c1 && lv(end + 1) == top {
                    end += 1;
                }
                let w = (c..=end).filter_map(|k| widest.get(&k).copied()).max();
                out.extend(w.map(|w| (c, w)));
                out.extend((c + 1..=end).filter_map(|k| w.map(|w| (k, w))));
                c = end;
            }
            c += 1;
        }
        out
    }
}

pub fn char_px(c: char) -> u32 {
    match c as u32 {
        n @ 32..=126 => u32::from(ASCII_PX[(n - 32) as usize]),
        _ => DIGIT_PX,
    }
}

fn char_run(c: char, m: Option<&FontMeasure>) -> Option<u32> {
    match m {
        Some(m) => m.advance(c),
        None => Some(char_px(c)),
    }
}

fn general_run(text: &str, m: Option<&FontMeasure>) -> Option<u32> {
    let digit = m.map_or(DIGIT_PX, |m| m.digit);
    let mut px = m.map_or(0, |m| m.c);
    let body = match text.strip_prefix('-') {
        Some(rest) => {
            px += char_run('-', m)?;
            rest
        }
        None => text,
    };
    let n = body.chars().count() as u32;
    px += n * digit;
    if body.contains('E') {
        px = (px + char_run('E', m)? + char_run('+', m)?).saturating_sub(2 * digit);
    }
    Some(px)
}

fn text_run(s: &str, m: Option<&FontMeasure>) -> Option<u32> {
    let mut px = m.map_or(0, |m| m.c);
    for c in s.chars() {
        px += char_run(c, m)?;
    }
    Some(px)
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

struct ShownFont<'a>(Option<&'a FontMeasure>);

impl ShownFont<'_> {
    fn digit(&self) -> u32 {
        self.0.map_or(DIGIT_PX, |m| m.digit)
    }

    fn room(&self, px: u32) -> u32 {
        match self.0 {
            Some(m) => px.saturating_sub(2 * m.g + 1 + m.c),
            None => px.saturating_sub(PAD_PX),
        }
    }

    fn char_px(&self, c: char) -> u32 {
        char_run(c, self.0).unwrap_or_else(|| self.digit())
    }

    fn general_px(&self, s: &str) -> u32 {
        s.chars()
            .map(|c| {
                if c.is_ascii_digit() || c == '.' {
                    self.digit()
                } else {
                    self.char_px(c)
                }
            })
            .sum()
    }

    fn hashes(&self, room: u32) -> String {
        let w = self
            .0
            .map_or(DIGIT_PX, |m| m.advance('#').unwrap_or(m.digit));
        "#".repeat((room / w.max(1)) as usize)
    }
}

impl Workbook {
    pub fn col_px(&self, sheet: SheetId, col: u32) -> u32 {
        self.views
            .get(&sheet)
            .and_then(|v| v.col_px.get(&col))
            .map_or_else(|| self.sheet_std_px(sheet), |w| w.0)
    }

    pub fn sheet_std_px(&self, sheet: SheetId) -> u32 {
        self.views.get(&sheet).and_then(|v| v.std_cw).map_or_else(
            || self.standard_px(),
            |cw| width_to_px_d(cw, self.digit_px()),
        )
    }

    pub fn set_sheet_standard_width(&mut self, sheet: SheetId, cw: f64) {
        let px = width_to_px_d(cw, self.digit_px());
        let default = self.standard_px();

        let kept = px_to_width_d(px, self.digit_px());
        self.view_mut(sheet).std_cw = (px != default).then_some(kept);
    }

    pub fn sheet_standard_width(&self, sheet: SheetId) -> f64 {
        px_to_width_d(self.sheet_std_px(sheet), self.digit_px())
    }

    pub fn digit_px(&self) -> u32 {
        let f = &self.base_style().font;
        digit_px(&f.name, f.size_twips, f.bold, f.italic)
    }

    pub fn max_px(&self) -> u32 {
        let d = self.digit_px();
        255 * d + pad_for(d)
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
        let std = self.sheet_std_px(sheet);
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
        let std = self.sheet_std_px(sheet);
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
        let max_px = self.max_px();
        let v = self.view_mut(sheet);
        for c in c0..=c1 {
            v.col_px.insert(c, (px.min(max_px), by_hand));
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
            let al = &self.cell_style(addr).alignment;
            let indent = indent_px(al.indent, al.horizontal);
            if let Some(px) = self.fit_cell(addr, indent, &|m| self.text_px_for_widen(addr, m)) {
                need.push((c, px));
            }
        }

        let mut fitted = Vec::new();
        for (c, px) in need {
            let set = self
                .views
                .get(&sheet)
                .and_then(|v| v.col_px.get(&c))
                .copied();
            if self.col_hidden(sheet, c) || set.is_some_and(|s| s.1) {
                continue;
            }
            if px > set.map_or(self.sheet_std_px(sheet), |s| s.0) {
                if fitted.is_empty() {
                    fitted = (rect.r0..=rect.r1)
                        .filter(|&r| self.row_custom_h8(sheet, r).is_none())
                        .map(|r| (r, self.row_fit_tenths(sheet, r)))
                        .collect();
                }
                let max_px = self.max_px();
                self.view_mut(sheet)
                    .col_px
                    .insert(c, (px.min(max_px), false));
            }
        }
    }

    pub fn widen_for_font(&mut self, sheet: SheetId, rect: Rect) {
        let Some(sh) = self.sheet(sheet) else {
            return;
        };
        let cells: Vec<(u32, u32)> = sh.cells.iter_rect(rect).map(|(r, c, _)| (r, c)).collect();
        let mut flag = self.general_flag.load(std::sync::atomic::Ordering::Relaxed);
        for (r, c) in cells {
            let addr = CellAddr::new(sheet, r, c);
            let Value::Number(x) = self.value(addr) else {
                continue;
            };
            if self.cell_style(addr).alignment.indent != 0 {
                continue;
            }
            let Ok(f) = NumberFormat::parse(self.number_format(addr)) else {
                continue;
            };
            let Ok(out) = f.format_cell_number(x, &self.locale, self.settings.date_system) else {
                continue;
            };
            let (need, px) = if matches!(
                parts(&out.text).as_slice(),
                [Part::General] | [Part::Char('-'), Part::General]
            ) {
                let m = self.cell_measure(addr);
                let sf = ShownFont(m.as_ref());

                let px = self.cell_room_px(addr);
                let avail = sf
                    .room(px.saturating_sub(crate::fontwidth::best_fit_room(px, self.digit_px())));
                let start = if x < 0.0 {
                    avail.saturating_sub(sf.char_px('-')) / sf.digit()
                } else {
                    avail / sf.digit()
                };
                let text = general_best_fit(x, start as i32, &mut flag);

                match self.fit_cell(addr, 0, &|m| general_run(&text, m)) {
                    Some(px) => (px - 1, px),
                    None => continue,
                }
            } else {
                match self.fit_cell(addr, 0, &|m| self.text_px_for_widen(addr, m)) {
                    Some(px) => (px, px),
                    None => continue,
                }
            };
            let set = self
                .views
                .get(&sheet)
                .and_then(|v| v.col_px.get(&c))
                .copied();
            if self.col_hidden(sheet, c) || set.is_some_and(|s| s.1) {
                continue;
            }
            if need > set.map_or(self.sheet_std_px(sheet), |s| s.0) {
                let max_px = self.max_px();
                self.view_mut(sheet)
                    .col_px
                    .insert(c, (px.min(max_px), false));
            }
        }
    }

    pub fn note_text_read(&self, addr: CellAddr) {
        let Value::Number(x) = self.value(addr) else {
            return;
        };
        if self.col_hidden(addr.sheet, addr.col) {
            return;
        }
        let Ok(f) = NumberFormat::parse(self.number_format(addr)) else {
            return;
        };
        let Ok(out) = f.format_cell_number(x, &self.locale, self.settings.date_system) else {
            return;
        };
        if !matches!(
            parts(&out.text).as_slice(),
            [Part::General] | [Part::Char('-'), Part::General]
        ) {
            return;
        }
        let m = self.cell_measure(addr);
        let sf = ShownFont(m.as_ref());
        let avail = sf.room(self.cell_room_px(addr));
        let w = if x < 0.0 {
            avail.checked_sub(sf.char_px('-')).map(|a| a / sf.digit())
        } else {
            Some(avail / sf.digit())
        };
        let Some(w) = w else {
            return;
        };
        let mut flag = self.general_flag.load(std::sync::atomic::Ordering::Relaxed);
        general_display_flag(x, w as i32, &mut flag);
        self.general_flag
            .store(flag, std::sync::atomic::Ordering::Relaxed);
    }

    pub fn widen_for_indent(&mut self, sheet: SheetId, rect: Rect) -> Vec<(u32, u32)> {
        let Some(sh) = self.sheet(sheet) else {
            return Vec::new();
        };
        let cells: Vec<(u32, u32)> = sh.cells.iter_rect(rect).map(|(r, c, _)| (r, c)).collect();
        let mut need: Vec<(u32, u32)> = Vec::new();
        for (r, c) in cells {
            let addr = CellAddr::new(sheet, r, c);
            let al = &self.cell_style(addr).alignment;
            let n = u32::from(al.indent);

            if n == 0
                || al.shrink
                || al.rotation != 0
                || !matches!(self.value(addr), Value::Number(_))
            {
                continue;
            }
            let indent = indent_px(al.indent, al.horizontal);
            if let Some(px) = self.fit_cell(addr, indent, &|m| self.full_text_px(addr, m)) {
                need.push((c, px));
            }
        }

        let mut fitted = Vec::new();
        for (c, px) in need {
            let set = self
                .views
                .get(&sheet)
                .and_then(|v| v.col_px.get(&c))
                .copied();
            if self.col_hidden(sheet, c) || set.is_some_and(|s| s.1) {
                continue;
            }
            if px > set.map_or(self.sheet_std_px(sheet), |s| s.0) {
                if fitted.is_empty() {
                    fitted = (rect.r0..=rect.r1)
                        .filter(|&r| self.row_custom_h8(sheet, r).is_none())
                        .map(|r| (r, self.row_fit_tenths(sheet, r)))
                        .collect();
                }
                let max_px = self.max_px();
                self.view_mut(sheet)
                    .col_px
                    .insert(c, (px.min(max_px), false));
            }
        }
        fitted
    }

    pub(crate) fn widen_for_new_table(&mut self, sheet: SheetId, rect: Rect) {
        for c in rect.c0..=rect.c1 {
            let Value::Text(t) = self.value(CellAddr::new(sheet, rect.r0, c)) else {
                continue;
            };

            let px = t.chars().map(bold_char_px).sum::<u32>() + FILTER_BUTTON_PX + 6;
            let set = self
                .views
                .get(&sheet)
                .and_then(|v| v.col_px.get(&c))
                .copied();
            if self.col_hidden(sheet, c) || set.is_some_and(|s| s.1) {
                continue;
            }
            if px > set.map_or(self.sheet_std_px(sheet), |s| s.0) {
                let max_px = self.max_px();
                self.view_mut(sheet)
                    .col_px
                    .insert(c, (px.min(max_px), false));
            }
        }
    }

    pub fn autofit_columns(&mut self, sheet: SheetId, rect: Rect) {
        let Some(sh) = self.sheet(sheet) else {
            return;
        };
        let cells: Vec<(u32, u32)> = sh.cells.iter_rect(rect).map(|(r, c, _)| (r, c)).collect();
        let headers = self.filter_headers(sheet);
        let mut widest: std::collections::BTreeMap<u32, u32> = Default::default();
        for (r, c) in cells {
            let addr = CellAddr::new(sheet, r, c);
            let hdr = headers
                .iter()
                .find(|h| h.row == r && (h.c0..=h.c1).contains(&c));
            let button = hdr.map_or(0, |h| h.button);
            let px = match (self.value(addr), hdr) {
                (Value::Text(t), Some(h)) if h.bold => Some(fit_px(
                    t.chars().map(bold_char_px).sum::<u32>() + button,
                    self.digit_px(),
                )),
                _ => {
                    let al = &self.cell_style(addr).alignment;
                    let indent = if al.shrink || al.rotation != 0 {
                        0
                    } else {
                        indent_px(al.indent, al.horizontal)
                    };
                    self.fit_cell(addr, indent + button, &|m| self.full_text_px(addr, m))
                }
            };
            if let Some(px) = px {
                let w = widest.entry(c).or_default();
                *w = (*w).max(px);
            }
        }
        for (c, px) in widest {
            if self.col_hidden(sheet, c) {
                self.set_cols_hidden(sheet, c, 1, false);
            }
            let max_px = self.max_px();
            self.view_mut(sheet)
                .col_px
                .insert(c, (px.min(max_px), false));
        }
    }

    fn filter_headers(&self, sheet: SheetId) -> Vec<HeaderFit> {
        let mut out = Vec::new();
        if let Some(v) = self.views.get(&sheet) {
            for f in v.filters.iter().filter(|f| f.table.is_none()) {
                if let Some(r) = self.filter_rect(f) {
                    out.push(HeaderFit {
                        row: r.r0,
                        c0: r.c0,
                        c1: r.c1,
                        bold: false,
                        button: FILTER_BUTTON_PX,
                    });
                }
            }
        }
        for t in self
            .tables()
            .iter()
            .filter(|t| t.sheet == sheet && t.headers)
        {
            out.push(HeaderFit {
                row: t.rect.r0,
                c0: t.rect.c0,
                c1: t.rect.c1,
                bold: true,
                button: if t.show_filter { FILTER_BUTTON_PX } else { 0 },
            });
        }
        out
    }

    pub(crate) fn autofit_pivot(
        &mut self,
        sheet: SheetId,
        rect: Rect,
        attrs: &[PivotFit],
        plan: &ColFit,
    ) {
        let Some(sh) = self.sheet(sheet) else {
            return;
        };
        let cells: Vec<(u32, u32)> = sh.cells.iter_rect(rect).map(|(r, c, _)| (r, c)).collect();
        let by_cell: std::collections::HashMap<(u32, u32), &PivotFit> =
            attrs.iter().map(|a| ((a.row, a.col), a)).collect();
        let mut widest: std::collections::BTreeMap<u32, u32> = Default::default();
        for (r, c) in cells {
            let addr = CellAddr::new(sheet, r, c);
            let at = by_cell.get(&(r, c)).copied();
            let px = match (self.value(addr), at) {
                (Value::Text(t), Some(a)) if a.bold => Some(t.chars().map(bold_char_px).sum()),
                _ => self.full_text_px(addr, None),
            };
            if let Some(px) = px {
                let w = widest.entry(c).or_default();
                *w = (*w).max(fit_px(px, self.digit_px()) + at.map_or(0, |a| a.extra));
            }
        }
        for (c, px) in plan.groups(rect.c0, rect.c1, &widest) {
            if !self.col_hidden(sheet, c) {
                let max_px = self.max_px();
                self.view_mut(sheet)
                    .col_px
                    .insert(c, (px.min(max_px), false));
            }
        }
    }

    fn cell_measure(&self, addr: CellAddr) -> Option<FontMeasure> {
        let f = &self.cell_style(addr).font;
        let reference = f.name.eq_ignore_ascii_case("Aptos Narrow")
            && f.size_twips == 220
            && !f.bold
            && !f.italic
            && !f.superscript
            && !f.subscript;
        if reference {
            None
        } else {
            FontMeasure::new(f)
        }
    }

    fn fit_cell(
        &self,
        addr: CellAddr,
        indent: u32,
        measure: &dyn Fn(Option<&FontMeasure>) -> Option<u32>,
    ) -> Option<u32> {
        if let Some(m) = self.cell_measure(addr) {
            if let Some(run) = measure(Some(&m)) {
                return Some(m.fit(run + indent, self.digit_px()));
            }
        }
        measure(None).map(|run| fit_px(run + indent, self.digit_px()))
    }

    fn full_text_px(&self, addr: CellAddr, m: Option<&FontMeasure>) -> Option<u32> {
        let x = match self.value(addr) {
            Value::Number(x) => x,
            Value::Blank => return None,
            _ => return text_run(&self.display_text(addr), m),
        };
        let f = NumberFormat::parse(self.number_format(addr)).ok()?;
        let out = f
            .format_cell_number(x, &self.locale, self.settings.date_system)
            .ok()?;
        let mut px = m.map_or(0, |m| m.c);
        for p in parts(&out.text) {
            px += match p {
                Part::Char(c) | Part::Space(c) => char_run(c, m)?,
                Part::Fill(_) => 0,

                Part::General => {
                    let g = format_general_cell(x.abs(), TEXT_GENERAL_WIDTH).unwrap_or_default();
                    let n = g.chars().count() as u32;
                    match m {
                        None => n * DIGIT_PX,
                        Some(m) if g.contains('E') => {
                            (n * m.digit + m.advance('E')? + m.advance('+')?)
                                .saturating_sub(2 * m.digit)
                        }
                        Some(m) => n * m.digit,
                    }
                }
            };
        }
        Some(px)
    }

    fn text_px_for_widen(&self, addr: CellAddr, m: Option<&FontMeasure>) -> Option<u32> {
        let x = match self.value(addr) {
            Value::Number(x) => x,
            Value::Bool(_) => return text_run(&self.display_text(addr), m),
            _ => return None,
        };
        let f = NumberFormat::parse(self.number_format(addr)).ok()?;
        let out = f
            .format_cell_number(x, &self.locale, self.settings.date_system)
            .ok()?;
        let parts = parts(&out.text);
        let mut px = m.map_or(0, |m| m.c);
        let mut general = false;
        for p in &parts {
            match p {
                Part::Char(c) | Part::Space(c) => px += char_run(*c, m)?,
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
            px += text_run(&g, None)?;
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
        self.cell_room_px(addr).saturating_sub(PAD_PX)
    }

    fn cell_room_px(&self, addr: CellAddr) -> u32 {
        let (c0, c1) = self
            .merge_at(addr)
            .map_or((addr.col, addr.col), |m| (m.c0, m.c1));
        let px: u32 = (c0..=c1)
            .filter(|&c| !self.col_hidden(addr.sheet, c))
            .map(|c| self.col_px(addr.sheet, c))
            .sum();
        px
    }

    fn filled_text(&self, addr: CellAddr) -> String {
        let shown = self.display_text(addr);
        let Value::Text(t) = self.value(addr) else {
            return shown;
        };
        if self.col_hidden(addr.sheet, addr.col) {
            return shown;
        }
        let Ok(f) = NumberFormat::parse(self.number_format(addr)) else {
            return shown;
        };
        let Ok(out) = f.format_cell_text(&t, &self.locale) else {
            return shown;
        };
        let parts = parts(&out.text);
        if !parts.iter().any(|p| matches!(p, Part::Fill(_))) {
            return shown;
        }
        let avail = self.cell_room(addr);
        let used: u32 = parts
            .iter()
            .map(|p| match p {
                Part::Char(c) => char_px(*c),
                Part::Space(_) => char_px(' '),
                _ => 0,
            })
            .sum();
        let mut s = String::new();
        let mut filled = false;
        for p in parts {
            match p {
                Part::Char(c) => s.push(c),
                Part::Space(_) => s.push(' '),
                Part::Fill(c) if !filled => {
                    filled = true;
                    let n = avail.saturating_sub(used) / char_px(c).max(1);
                    s.extend(std::iter::repeat_n(c, n as usize));
                }
                _ => {}
            }
        }
        s
    }

    pub fn shown_text(&self, addr: CellAddr) -> String {
        let Value::Number(x) = self.value(addr) else {
            return self.filled_text(addr);
        };
        if self.col_hidden(addr.sheet, addr.col) {
            return self.display_text(addr);
        }
        let m = self.cell_measure(addr);
        let sf = ShownFont(m.as_ref());
        let avail = sf.room(self.cell_room_px(addr));
        let code = self.number_format(addr);
        let Some(f) = NumberFormat::parse(code).ok() else {
            return self.display_text(addr);
        };
        let Ok(out) = f.format_cell_number(x, &self.locale, self.settings.date_system) else {
            return sf.hashes(avail);
        };
        let parts = parts(&out.text);

        match parts.as_slice() {
            [Part::General] | [Part::Char('-'), Part::General] => {
                let neg = parts.len() == 2;
                let g = x.abs() * if neg { -1.0 } else { 1.0 };
                let dash = sf.char_px('-');
                let chars = if neg && avail >= dash {
                    (avail - dash) / sf.digit() + 1
                } else {
                    avail / sf.digit()
                };
                let fits = |s: &str| sf.general_px(s) <= avail;
                return format_general_cell_by(g, chars as usize, Some(&fits))
                    .map(|t| f.digits_of_tag(&t))
                    .unwrap_or_else(|| sf.hashes(avail));
            }
            _ => {}
        }
        let mut used = 0;
        let mut fill = None;
        for p in &parts {
            match p {
                Part::Char(c) => used += sf.char_px(*c),
                Part::Space(_) => used += sf.char_px(' '),
                Part::Fill(c) if fill.is_none() => fill = Some(*c),
                _ => {}
            }
        }
        let general = if parts.iter().any(|p| matches!(p, Part::General)) {
            let room = avail.saturating_sub(used) / sf.digit();
            match format_general_cell(x.abs(), room as usize) {
                Some(g) => {
                    used += sf.general_px(&g);
                    f.digits_of_tag(&g)
                }
                None => return sf.hashes(avail),
            }
        } else {
            String::new()
        };
        if used > avail {
            return sf.hashes(avail);
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
                    let n = (avail - used) / sf.char_px(c).max(1);
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
    fn standard_width_put_keeps_the_read_back_value() {
        let mut wb = Workbook::new();
        let s = wb.sheet_at(0).id;
        wb.set_sheet_standard_width(s, 12.5);
        assert_eq!(wb.views.get(&s).and_then(|v| v.std_cw), Some(12.55));
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
