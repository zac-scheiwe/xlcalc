use crate::address::{CellAddr, SheetId, MAX_COLS, MAX_ROWS};
use crate::colwidth::char_px;
use crate::gdi_metrics;

static TURN_ADVANCES: &[u8] = include_bytes!("rowheight_adv.bin");

static TURN_EXT: &[u8] = include_bytes!("rowheight_adv_ext.bin");
use crate::style::{Alignment, BorderLine, HAlign, Style};
use crate::value::Value;
use crate::workbook::Workbook;

const WRAP_PAD_PX: u32 = 9;

pub const MAX_H8: u32 = 3276;

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

    pub stale: Option<u32>,
}

fn turn_params(f: &crate::style::Font) -> (u32, u32, u32) {
    let ppem = ((f.size_twips + 5) / 10).max(2);
    let (a, d) = font_ad(f);
    let digit = gdi_metrics::digit_width(&f.name, f.bold, f.italic, ppem);
    let c = match gdi_metrics::overhang(&f.name, f.bold, f.italic, ppem) {
        0 if f.italic => {
            (gdi_metrics::tm(&f.name, f.bold, f.italic, ppem)
                .ascent
                .max(0)
                / 5) as u32
        }
        oh => oh,
    };
    (digit.div_ceil(4), a + d, c)
}

const MAX_ONE_LINE: u32 = 4096;

fn font_ad(f: &crate::style::Font) -> (u32, u32) {
    let ppem = ((f.size_twips + 5) / 10).max(2);
    let tm = gdi_metrics::tm(&f.name, f.bold, f.italic, ppem);
    gdi_metrics::ascent_descent(&tm)
}

fn font_h(s: &Style) -> u32 {
    let (a, d) = font_ad(&s.font);
    (5 * (a + d)).min(MAX_ONE_LINE)
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

fn indent_px(al: &Alignment) -> u32 {
    let one = u32::from(al.indent) * 12;
    match al.horizontal {
        HAlign::General | HAlign::Left | HAlign::Right => one,
        HAlign::Distributed => one * 2,
        _ => 0,
    }
}

fn wrap_run_max(limit: u32, g: u32, c: u32) -> u32 {
    let first = limit.saturating_sub(2 * g + 3);
    let second = limit.saturating_sub(2 * g) * 44 / 45;
    first.min(second).saturating_sub(c)
}

fn wrap_lines(text: &str, avail: u32, wrap: bool, px: &dyn Fn(char) -> u32) -> u32 {
    let mut lines = 0;
    for seg in text.split('\n') {
        lines += if wrap { seg_lines(seg, avail, px) } else { 1 };
    }
    lines.max(1)
}

fn seg_lines(seg: &str, avail: u32, px: &dyn Fn(char) -> u32) -> u32 {
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

const SIN15: [i64; 91] = [
    0, 572, 1144, 1715, 2286, 2856, 3425, 3993, 4560, 5126, 5690, 6252, 6813, 7371, 7927, 8481,
    9032, 9580, 10126, 10668, 11207, 11743, 12275, 12803, 13328, 13848, 14364, 14876, 15383, 15886,
    16384, 16876, 17364, 17846, 18323, 18794, 19260, 19720, 20173, 20621, 21062, 21497, 21925,
    22347, 22762, 23170, 23571, 23964, 24351, 24730, 25101, 25465, 25821, 26169, 26509, 26841,
    27165, 27481, 27788, 28087, 28377, 28659, 28932, 29196, 29451, 29697, 29934, 30162, 30381,
    30591, 30791, 30982, 31163, 31335, 31498, 31650, 31794, 31927, 32051, 32165, 32269, 32364,
    32448, 32523, 32587, 32642, 32687, 32722, 32747, 32762, 32767,
];

fn mul_div(a: i64, b: i64, c: i64) -> i64 {
    (a * b + c / 2) / c
}

fn stacked_half_points(text: &str, line: u32) -> u32 {
    let n = text.chars().count() as u32;
    (line * n + 2).min(819)
}

const TURN_FONTS: [&str; 56] = [
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
    "Comic Sans MS",
    "Calibri Light",
    "Trebuchet MS",
    "Lucida Console",
    "Microsoft Sans Serif",
    "Segoe UI Semibold",
    "MS Gothic",
    "Bahnschrift",
    "Book Antiqua",
    "Candara",
    "Cascadia Code",
    "Cascadia Mono",
    "Century",
    "Century Gothic",
    "Constantia",
    "Corbel",
    "Dubai",
    "Ebrima",
    "Franklin Gothic Medium",
    "Gabriola",
    "Gadugi",
    "Impact",
    "Ink Free",
    "JetBrains Mono",
    "Leelawadee UI",
    "Lucida Sans Unicode",
    "Malgun Gothic",
    "Microsoft YaHei",
    "MS PGothic",
    "MS Reference Sans Serif",
    "Nirmala UI",
    "Palatino Linotype",
    "Segoe UI Light",
    "Segoe Print",
    "Sitka Text",
    "Sylfaen",
    "Ubuntu Mono",
    "Yu Gothic",
    "SimSun",
    "Aptos Serif",
    "Aptos Mono",
];

const EXT_FONTS: usize = 13;

const ADV_PPEM: (u32, u32) = (2, 160);

const EXT_PPEM0: u32 = 12;

const EXT_RANGES: [(u32, u32); 5] = [
    (0xA0, 0x17F),
    (0x391, 0x3C9),
    (0x410, 0x44F),
    (0x621, 0x64A),
    (0x2013, 0x2014),
];
const EXT_PPEM1: u32 = 56;

fn turn_advance(font: usize, face: usize, ppem: u32, c: char) -> Option<i64> {
    let n = c as u32;
    let row = font * 4 + face;

    let in_table = (ADV_PPEM.0..=ADV_PPEM.1).contains(&ppem);
    if (32..=126).contains(&n) {
        if !in_table {
            return None;
        }
        let at = (row * (ADV_PPEM.1 - ADV_PPEM.0 + 1) as usize + (ppem - ADV_PPEM.0) as usize) * 95;
        return TURN_ADVANCES
            .get(at + (n - 32) as usize)
            .map(|&b| i64::from(b));
    }

    let bold = i64::from(face & 2 != 0);
    if matches!(n, 0x3040..=0x30FF | 0x3400..=0x4DBF | 0x4E00..=0x9FFF | 0xAC00..=0xD7A3 | 0xFF01..=0xFF60)
    {
        return Some(i64::from(ppem) + bold);
    }
    if (0x1F600..=0x1F64F).contains(&n) {
        let adv = if ppem == EXT_PPEM0 {
            15
        } else {
            (11 * ppem + 3) / 8
        };
        return Some(i64::from(adv) + bold);
    }
    if (EXT_PPEM0..=EXT_PPEM1).contains(&ppem) && font < EXT_FONTS {
        let mut skip = 0;
        for (lo, hi) in EXT_RANGES {
            if (lo..=hi).contains(&n) {
                let chars: u32 = EXT_RANGES.iter().map(|(l, h)| h - l + 1).sum();
                let at = (row * (EXT_PPEM1 - EXT_PPEM0 + 1) as usize + (ppem - EXT_PPEM0) as usize)
                    * chars as usize
                    + (skip + n - lo) as usize;
                return TURN_EXT.get(at).map(|&b| i64::from(b));
            }
            skip += hi - lo + 1;
        }
    }
    None
}

fn turned_half_points(text: &str, f: &crate::style::Font, deg: u32, number: bool) -> Option<u32> {
    let font = TURN_FONTS
        .iter()
        .position(|n| n.eq_ignore_ascii_case(&f.name))?;
    if !f.size_twips.is_multiple_of(10) || !(1..=90).contains(&deg) {
        return None;
    }
    let ppem = f.size_twips / 10;
    if ppem < ADV_PPEM.0 || ppem > ADV_PPEM.1 {
        return None;
    }
    let face = usize::from(f.italic) + 2 * usize::from(f.bold);
    let (g, h, c) = turn_params(f);
    let (g, h, c) = (i64::from(g), i64::from(h), i64::from(c));
    let mut w = c;
    for ch in text.chars() {
        w += match turn_advance(font, face, ppem, ch) {
            Some(a) => a,

            None if font == 0 => i64::from(char_px(ch) * ppem / 22),
            None => return None,
        };
    }
    let v = if deg == 90 {
        (w + 2 * g + 3 + c).max(45 * w / 44 + 2 * g + 1 + c)
    } else {
        let d = deg as usize;
        let (s, co) = (SIN15[d], SIN15[90 - d]);
        mul_div(s * w + co * h, 1, 32767) + 2 * mul_div(g * s, 1, 32767) + 1
    };
    let v = v + if number && deg != 90 { 2 } else { 0 };
    Some((v as u32).min(819))
}

fn break_lines(text: &str, fits: &dyn Fn(u32) -> bool, unit: &dyn Fn(char) -> u32) -> Vec<u32> {
    let space = unit(' ');
    let mut lines = Vec::new();
    let mut x = 0u32;
    for (i, word) in text.split(' ').enumerate() {
        let w: u32 = word.chars().map(unit).sum();
        if i > 0 && fits(x + space + w) {
            x += space + w;
        } else if i == 0 && fits(w) {
            x = w;
        } else {
            if i > 0 {
                lines.push(x);
            }
            x = 0;
            for ch in word.chars() {
                let cw = unit(ch);
                if x > 0 && !fits(x + cw) {
                    lines.push(x);
                    x = 0;
                }
                x += cw;
            }
        }
    }
    lines.push(x);
    lines
}

fn best_fit_allowance(x: u32, grow: bool, d: u32) -> i64 {
    let (x, d) = (i64::from(x), i64::from(d.max(1)));
    if grow {
        (2 * x / d).max(2)
    } else {
        -(2 * x / (d + 2)).max(2)
    }
}

fn turned_wrapped_half_points(
    text: &str,
    col_px: u32,
    row_hp: u32,
    (line, g, c): (u32, u32, u32),
    d: u32,
    px: &dyn Fn(char) -> u32,
) -> u32 {
    const MAX: u32 = 819;
    const UNLIMITED: u32 = 0x7fff;
    let across = col_px.saturating_sub(2 * g + 1);
    let along = row_hp.saturating_sub(2 * g + 1);
    let w0 = (i64::from(along) + best_fit_allowance(along, false, d)).max(0) as u32;

    let measure = |w: u32| {
        let avail = w.saturating_sub(c);
        let lines = break_lines(text, &|x| x <= avail, px);
        (
            lines.iter().max().copied().unwrap_or(0) + c,
            lines.len() as u32 * line,
        )
    };
    let (mut w, extent) = measure(w0);
    if across < extent {
        let (mut lo, mut hi) = (w, measure(UNLIMITED).0 + 1);
        while lo + 1 < hi {
            let mid = (hi + lo) / 2;
            if measure(mid).1 <= across {
                w = mid;
                hi = mid;
            } else {
                lo = mid;
            }
        }
    }
    let h = i64::from(w) + best_fit_allowance(w, true, d) + 1 + i64::from(2 * g + c);
    (h as u32).clamp(line, MAX)
}

fn wrapped_stacked_half_points(text: &str, col_px: u32, start: u32, (line, g): (u32, u32)) -> u32 {
    const MAX: u32 = 819;
    if text.chars().count() == 1 {
        return line;
    }

    let slots = (col_px.saturating_sub(2 * g + 1 + 2) / line).max(1) as usize;
    let lines_at = |len: u32| {
        let cap = (len.saturating_sub(2) / line).max(1);
        break_lines(text, &|x| x <= cap, &|_| 1)
    };
    let start = start.min(MAX);
    let mut lines = lines_at(start);
    if lines.len() > slots {
        if lines_at(MAX).len() > slots {
            return MAX;
        }
        let (mut lo, mut hi) = (start + 1, MAX);
        while lo < hi {
            let mid = (lo + hi) / 2;
            if lines_at(mid).len() <= slots {
                hi = mid;
            } else {
                lo = mid + 1;
            }
        }
        lines = lines_at(lo);
    }
    let longest = lines.into_iter().max().unwrap_or(0);
    (line * longest + 2).clamp(line, MAX)
}

fn column_header_height(name: &str, bold: bool, italic: bool, size_twips: u32, zoom: u32) -> i64 {
    let ppem0 = ((size_twips + 5) / 10).max(2);
    let ppem = (ppem0 * zoom + 50) / 100;
    let (a, d) = gdi_metrics::ascent_descent(&gdi_metrics::tm(name, bold, italic, ppem.max(2)));
    i64::from(a + d)
}

fn window_last_row(header: i64, zoom: u32, avail: i64, hp: &dyn Fn(u32) -> u32) -> u32 {
    let scale = |h: u32| (i64::from(h) * i64::from(zoom) * 2 + 100) / 200;
    let mut acc = header;
    let mut r = 0;
    while r < MAX_ROWS {
        let u = scale(hp(r));
        if acc + u > avail {
            return r;
        }
        acc += u;
        r += 1;
        if acc == avail {
            return r - 1;
        }
    }
    MAX_ROWS - 1
}

const AVAIL: i64 = 1376;

pub fn window_rows(zoom: f64) -> u32 {
    let z = zoom.round().clamp(10.0, 400.0) as u32;
    window_last_row(
        column_header_height("Aptos Narrow", false, false, 220, z),
        z,
        AVAIL,
        &|_| 29,
    ) + 1
}

impl Workbook {
    pub fn set_normal_style(&mut self, s: Style, zoom: &dyn Fn(SheetId) -> f64) {
        let ids: Vec<SheetId> = self.sheets().map(|sh| sh.id).collect();
        let old: Vec<u32> = ids.iter().map(|&id| self.standard_tenths(id)).collect();
        self.set_base_style(s);
        for (id, o) in ids.into_iter().zip(old) {
            let new = self.live_standard_tenths(id);
            let rows: Vec<u32> = self.populated_lines(id, true).into_iter().collect();
            let follows = if new == o {
                true
            } else if let Some(&last) = rows.last() {
                let z = zoom(id).round().clamp(10.0, 400.0) as u32;
                let header = {
                    let f = &self.base_style().font;
                    column_header_height(&f.name, f.bold, f.italic, f.size_twips, z)
                };
                let mut done = std::collections::BTreeSet::new();
                let mut ok = true;
                for &row in rows.iter().chain(std::iter::once(&(last + 1))) {
                    let hp = |r: u32| -> u32 {
                        if self.row_hidden(id, r) {
                            0
                        } else if let Some(h8) = self.row_custom_h8(id, r) {
                            h8 / 4
                        } else if done.contains(&r) {
                            new / 5
                        } else {
                            o / 5
                        }
                    };
                    let shown = window_last_row(header, z, AVAIL, &hp);
                    if row > shown {
                        ok = row != last + 1;
                    } else {
                        done.insert(row);
                    }
                }
                ok
            } else {
                true
            };
            self.view_mut(id).std_frozen = if follows { None } else { Some(o) };
        }
    }

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

    pub fn standard_height(&self, sheet: SheetId) -> f64 {
        f64::from(self.standard_tenths(sheet)) / 10.0
    }

    fn standard_tenths(&self, sheet: SheetId) -> u32 {
        if let Some(t) = self.views.get(&sheet).and_then(|v| v.std_frozen) {
            return t;
        }
        self.live_standard_tenths(sheet)
    }

    fn row_in_table(&self, sheet: SheetId, row: u32) -> bool {
        if self
            .views
            .get(&sheet)
            .is_none_or(|v| v.std_frozen.is_none())
        {
            return false;
        }
        self.cellfmt.rows.contains_key(&(sheet, row))
            || self.sheet(sheet).is_some_and(|sh| {
                sh.cells
                    .iter_rect(crate::Rect::new(row, 0, row, MAX_COLS - 1))
                    .next()
                    .is_some()
            })
            || self.cellfmt.map.keys().any(|k| k.0 == sheet && k.1 == row)
    }

    fn live_standard_tenths(&self, sheet: SheetId) -> u32 {
        let (a, d) = self.gap_maxima(sheet, None);
        (5 * (a + d)).min(MAX_ONE_LINE)
    }

    fn gap_fonts(&self, sheet: SheetId, row: Option<u32>) -> Vec<(u32, u32)> {
        let cols = self.line_format_indices(sheet, false);
        let free = |c: u32| {
            row.is_none_or(|r| {
                let a = CellAddr::new(sheet, r, c);
                self.merge_at(a).is_none()
                    && self.own_style(a).is_none()
                    && self.value(a).is_blank()
            })
        };
        let style_at = |c: u32| -> &Style {
            match row {
                Some(r) => self.cell_style(CellAddr::new(sheet, r, c)),
                None => self.line_style(sheet, false, c),
            }
        };
        let mut seen: Vec<*const Style> = Vec::new();
        let mut out: Vec<(u32, u32)> = Vec::new();
        let mut take = |sty: &Style| {
            let p: *const Style = sty;
            if !seen.contains(&p) {
                seen.push(p);
                let ad = font_ad(&sty.font);
                if !out.contains(&ad) {
                    out.push(ad);
                }
            }
        };
        for &c in &cols {
            if free(c) {
                take(style_at(c));
            }
        }

        let mut c = 0;
        while c < MAX_COLS && (cols.binary_search(&c).is_ok() || !free(c)) {
            c += 1;
        }
        if c < MAX_COLS {
            take(style_at(c));
        }
        out
    }

    fn gap_maxima(&self, sheet: SheetId, row: Option<u32>) -> (u32, u32) {
        let (a, d) = self
            .gap_fonts(sheet, row)
            .into_iter()
            .fold((0, 0), |(a, d), (x, y)| (a.max(x), d.max(y)));
        if d == 0 {
            font_ad(&self.base_style().font)
        } else {
            (a, d)
        }
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
                        stale: None,
                    },
                );
            }
        }
    }

    pub fn auto_keep(&mut self, sheet: SheetId, fitted: &[(u32, u32)]) {
        for &(r, t) in fitted {
            self.view_mut(sheet).row_h.insert(
                r,
                RowHt {
                    custom: None,
                    frozen: Some(t),
                    stale: None,
                },
            );
        }
    }

    pub fn auto_refit(&mut self, sheet: SheetId, r0: u32, r1: u32) {
        if let Some(v) = self.views.get_mut(&sheet) {
            for (_, h) in v.row_h.range_mut(r0..=r1) {
                if h.frozen.is_some() {
                    h.stale = h.frozen;
                }
                h.frozen = None;
            }
        }
    }

    pub fn post_scan_refit(&mut self, sheet: SheetId, r0: u32, r1: u32) {
        let hidden = self.hidden_rows(sheet);
        let mut lo = r0;
        while lo > 0 {
            lo -= 1;
            if !hidden.contains(&lo) {
                break;
            }
        }
        let mut hi = r1;
        while hi + 1 < MAX_ROWS {
            hi += 1;
            if !hidden.contains(&hi) {
                break;
            }
        }
        self.auto_refit(sheet, lo, hi);
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

    pub fn holds_text(&self, sheet: SheetId, rect: crate::Rect) -> bool {
        self.rect_any(sheet, rect, |wb, a| matches!(wb.value(a), Value::Text(_)))
    }

    pub fn heavy_horizontal_edge(&self, sheet: SheetId, rect: crate::Rect) -> bool {
        self.rect_any(sheet, rect, |wb, a| {
            let sty = wb.cell_style(a);
            let above = if a.row > 0 {
                line_px(
                    wb.cell_style(CellAddr::new(sheet, a.row - 1, a.col))
                        .borders
                        .bottom
                        .line,
                )
            } else {
                0
            };
            let below = if a.row + 1 < MAX_ROWS {
                line_px(
                    wb.cell_style(CellAddr::new(sheet, a.row + 1, a.col))
                        .borders
                        .top
                        .line,
                )
            } else {
                0
            };
            line_px(sty.borders.top.line).max(above) >= 2
                || line_px(sty.borders.bottom.line).max(below) >= 2
        })
    }

    fn rect_any(
        &self,
        sheet: SheetId,
        rect: crate::Rect,
        f: impl Fn(&Self, CellAddr) -> bool,
    ) -> bool {
        let rect = self
            .used_range(sheet)
            .and_then(|u| rect.intersection(&u))
            .unwrap_or(rect);
        if u64::from(rect.r1 - rect.r0 + 1) * u64::from(rect.c1 - rect.c0 + 1) > 10_000 {
            return false;
        }
        (rect.r0..=rect.r1)
            .any(|r| (rect.c0..=rect.c1).any(|c| f(self, CellAddr::new(sheet, r, c))))
    }

    pub(crate) fn row_fit_tenths(&self, sheet: SheetId, row: u32) -> u32 {
        let frozen = self
            .views
            .get(&sheet)
            .is_some_and(|v| v.std_frozen.is_some());
        let mut tenths = if frozen && !self.row_in_table(sheet, row) {
            self.standard_tenths(sheet)
        } else {
            0
        };
        if let Some((_, sty)) = self.cellfmt.rows.get(&(sheet, row)) {
            tenths = tenths.max(font_h(sty));
        }
        let mut extra = 0;
        let mut single = 0;
        let mut multi = false;

        let (mut asc, mut desc) = (0, 0);
        let last = self.row_last_col(sheet);

        let across = |r: Option<u32>, bottom: bool| {
            let mut best = BorderLine::None;
            if let Some(r) = r.filter(|r| *r < MAX_ROWS) {
                for c in 0..=last {
                    let b = &self.cell_style(CellAddr::new(sheet, r, c)).borders;
                    let l = if bottom { b.bottom.line } else { b.top.line };
                    if line_px(l) > line_px(best) {
                        best = l;
                    }
                }
            }
            best
        };
        let above = across(row.checked_sub(1), true);
        let below = across(row.checked_add(1), false);
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

            let shifted = sty.font.superscript || sty.font.subscript;
            let one = font_h(sty) + if shifted { 20 } else { 0 };
            extra = extra.max(border_extra(top, bottom));
            let lines = match &v {
                Value::Text(t) if sty.alignment.wrap || t.contains('\n') => {
                    let limit = self
                        .col_px(sheet, a.col)
                        .saturating_sub(indent_px(&sty.alignment));
                    let scale =
                        f64::from(sty.font.size_twips) / 220.0 * if shifted { 0.75 } else { 1.0 };
                    let font = TURN_FONTS
                        .iter()
                        .position(|f| f.eq_ignore_ascii_case(&sty.font.name));
                    let face = usize::from(sty.font.italic) + 2 * usize::from(sty.font.bold);
                    let ppem = sty.font.size_twips / 10;

                    let avail = if shifted {
                        limit.saturating_sub(WRAP_PAD_PX)
                    } else {
                        let (g, _, c) = turn_params(&sty.font);
                        wrap_run_max(limit, g, c)
                    };
                    let px = |c: char| {
                        let own = match font {
                            Some(f) if !shifted && sty.font.size_twips.is_multiple_of(10) => {
                                turn_advance(f, face, ppem, c)
                            }
                            _ => None,
                        };
                        own.map_or_else(
                            || (f64::from(char_px(c)) * scale).round() as u32,
                            |a| a as u32,
                        )
                    };
                    wrap_lines(t, avail, sty.alignment.wrap, &px)
                }
                _ => 1,
            };

            let shown = match &v {
                Value::Number(_) | Value::Bool(_) | Value::Error(_)
                    if sty.alignment.rotation != 0 =>
                {
                    Some(self.display_text(a))
                }
                Value::Text(t) => Some(t.to_string()),
                _ => None,
            };
            let plain_number = matches!(v, Value::Number(_))
                && !crate::NumberFormat::parse(&sty.number_format).is_ok_and(|n| n.is_date());
            let turn_deg = match sty.alignment.rotation {
                r @ 1..=90 => Some(u32::from(r)),
                r @ 91..=180 => Some(u32::from(r) - 90),
                _ => None,
            };
            let rotated = match &shown {
                Some(t)
                    if !sty.alignment.wrap
                        && !shifted
                        && !t.contains('\n')
                        && turn_deg.is_some() =>
                {
                    turned_half_points(t, &sty.font, turn_deg.unwrap_or(0), plain_number)
                }
                _ => None,
            }
            .or(match &shown {
                Some(t)
                    if sty.alignment.rotation != 0
                        && sty.alignment.wrap
                        && !t.contains('\n')
                        && !shifted =>
                {
                    let px = self.col_px(sheet, a.col);

                    let stale = self
                        .views
                        .get(&sheet)
                        .and_then(|v| v.row_h.get(&row))
                        .and_then(|h| h.stale)
                        .map(|t| (t.saturating_sub(150) * 2 + 145) / 290 + 1);
                    let font = TURN_FONTS
                        .iter()
                        .position(|f| f.eq_ignore_ascii_case(&sty.font.name));
                    let face = usize::from(sty.font.italic) + 2 * usize::from(sty.font.bold);
                    let ppem = sty.font.size_twips / 10;
                    match (font, sty.font.size_twips % 10) {
                        (Some(f), 0) => {
                            let (g, h, c) = turn_params(&sty.font);
                            let adv = |ch: char| {
                                turn_advance(f, face, ppem, ch)
                                    .map_or_else(|| char_px(ch) * ppem / 22, |a| a as u32)
                            };

                            let flat = stale.unwrap_or_else(|| {
                                let lim = wrap_run_max(px, g, c);
                                break_lines(t, &|x| x <= lim, &adv).len() as u32
                            });
                            let first = h.max((sty.font.size_twips / 2 + 35) / 5);
                            let start = if stale.is_some() {
                                h * flat
                            } else {
                                first + (flat - 1) * h
                            }
                            .min(819);
                            match sty.alignment.rotation {
                                90 | 180 => Some(turned_wrapped_half_points(
                                    t,
                                    px,
                                    start,
                                    (h, g, c),
                                    8 * self.digit_px(),
                                    &adv,
                                )),
                                255 => Some(wrapped_stacked_half_points(t, px, start, (h, g))),
                                _ => None,
                            }
                        }
                        _ => None,
                    }
                }
                Some(t)
                    if sty.alignment.rotation == 255
                        && !sty.alignment.wrap
                        && !t.contains('\n')
                        && !shifted =>
                {
                    Some(stacked_half_points(t, turn_params(&sty.font).1))
                }
                _ => None,
            });
            if let Some(h2) = rotated {
                single = single.max(h2 * 5);
                continue;
            }

            let (a, d) = font_ad(&sty.font);
            let (a, d) = (5 * a, 5 * d);
            if lines > 1 {
                multi = true;
                let total = one * lines;
                asc = asc.max(a.max(total.saturating_sub(d)));
                desc = desc.max(d);
            } else {
                single = single.max(one);
                if !shifted {
                    asc = asc.max(a);
                    desc = desc.max(d);
                }
            }
        }

        let (ga, gd) = self.gap_maxima(sheet, Some(row));
        asc = asc.max(5 * ga);
        desc = desc.max(5 * gd);
        let cap = if multi { 4095 } else { MAX_ONE_LINE };
        tenths = tenths.max(single).max((asc + desc).min(cap));
        tenths + extra
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
        self.mark_rows(sheet, r0, r1, true);
        let v = self.view_mut(sheet);
        for r in r0..=r1 {
            v.row_h.insert(
                r,
                RowHt {
                    custom: Some(h8),
                    frozen: None,
                    stale: None,
                },
            );
        }
        Some(())
    }

    pub fn autofit_rows(&mut self, sheet: SheetId, r0: u32, r1: u32) {
        if (r0..=r1).any(|r| self.row_hidden(sheet, r)) {
            let _ = self.set_rows_hidden(sheet, r0, r1 - r0 + 1, false);
        }
        if let Some(v) = self.views.get_mut(&sheet) {
            let keys: Vec<u32> = v.row_h.range(r0..=r1).map(|(k, _)| *k).collect();
            for k in keys {
                v.row_h.remove(&k);
            }
        }
    }

    pub fn populated_lines(&self, sheet: SheetId, rows: bool) -> std::collections::BTreeSet<u32> {
        let mut set = std::collections::BTreeSet::new();
        if let Some(sh) = self.sheet(sheet) {
            set.extend(sh.cells.iter().map(|(r, c, _)| if rows { r } else { c }));
        }
        set.extend(
            self.formatted_cells(sheet)
                .into_iter()
                .map(|(r, c)| if rows { r } else { c }),
        );
        if rows {
            set.extend(self.row_custom_heights(sheet).into_iter().map(|h| h.0));
            if let Some(v) = self.views.get(&sheet) {
                set.extend(v.table_rows.iter().copied());
            }
        }
        set
    }

    pub fn note_table_rows(&mut self, sheet: SheetId, rect: crate::Rect) {
        let rows: Vec<u32> = (rect.r0..=rect.r1.min(rect.r0 + 4096))
            .filter(|&r| {
                self.sheet(sheet).is_some_and(|sh| {
                    sh.cells
                        .iter_rect(crate::Rect::new(r, 0, r, MAX_COLS - 1))
                        .next()
                        .is_some()
                })
            })
            .collect();
        if !rows.is_empty() {
            self.view_mut(sheet).table_rows.extend(rows);
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
                stale: None,
            },
        );
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn flat_wrap_width_is_the_traced_pchlastfit_avail() {
        assert_eq!(wrap_run_max(546, 3, 0), 528);
        assert_eq!(wrap_run_max(557, 3, 0), 538);
    }

    #[test]
    fn window_walk_shows_the_traced_rows() {
        let rows: Vec<u32> = [50.0, 75.0, 100.0, 150.0, 200.0]
            .iter()
            .map(|z| window_rows(*z))
            .collect();
        assert_eq!(rows, [91, 62, 47, 31, 23]);
    }

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
        let f = |name: &str, twips: u32| crate::style::Font {
            name: name.into(),
            size_twips: twips,
            ..crate::style::Style::default().font
        };
        assert_eq!(font_ad(&f("Aptos Narrow", 220)), (22, 7));
        assert_eq!(
            font_h(&Style {
                font: f("Aptos Narrow", 220),
                ..Style::default()
            }),
            145
        );
        assert_eq!(
            font_h(&Style {
                font: f("Aptos Narrow", 240),
                ..Style::default()
            }),
            160
        );
        assert_eq!(
            font_h(&Style {
                font: f("Arial", 400),
                ..Style::default()
            }),
            250
        );

        assert_eq!(
            font_h(&Style {
                font: f("Arial", 100),
                ..Style::default()
            }),
            65
        );
    }

    #[test]
    fn one_line_edges() {
        let h = |name: &str, twips: u32| {
            font_h(&Style {
                font: crate::style::Font {
                    name: name.into(),
                    size_twips: twips,
                    ..Style::default().font
                },
                ..Style::default()
            })
        };

        assert_eq!(h("Arial", 8180), 4096);

        assert!(h("Arial", 110) <= h("Arial", 120));

        assert_eq!(h("No Such Font", 220), h("Arial", 220));
        assert_eq!(h("No Such Font", 100), h("Arial", 100));
    }

    #[test]
    fn turned_wrap_growth_search_follows_the_routines() {
        let f = crate::style::Font {
            name: "Palatino Linotype".into(),
            size_twips: 220,
            italic: true,
            ..crate::style::Style::default().font
        };
        let (g, h, c) = turn_params(&f);
        assert_eq!((g, h, c), (3, 31, 4));
        let font = TURN_FONTS
            .iter()
            .position(|n| n.eq_ignore_ascii_case(&f.name))
            .unwrap();
        let adv = |ch: char| turn_advance(font, 1, 22, ch).unwrap() as u32;
        assert_eq!(
            turned_wrapped_half_points("alpha beta gamma delta", 100, 124, (h, g, c), 88, &adv),
            108
        );
        assert_eq!(best_fit_allowance(95, true, 88), 2);
        assert_eq!(best_fit_allowance(117, false, 88), -2);
    }

    #[test]
    fn turn_parameters_follow_the_font_routine() {
        let f = |name: &str, twips: u32, italic: bool| crate::style::Font {
            name: name.into(),
            size_twips: twips,
            italic,
            ..crate::style::Style::default().font
        };

        assert_eq!(turn_params(&f("Aptos Narrow", 220, false)), (3, 29, 0));
        assert_eq!(turn_params(&f("Aptos Narrow", 220, true)), (3, 29, 4));
        assert_eq!(turn_params(&f("Aptos Narrow", 280, false)), (4, 37, 0));

        let (g, h, c) = turn_params(&f("Comic Sans MS", 110, true));
        assert!(g >= 1 && h > 0 && c > 0);

        assert_eq!(stacked_half_points("Wag", 29), 89);
        assert_eq!(stacked_half_points(&"a".repeat(100), 29), 819);
    }
}
