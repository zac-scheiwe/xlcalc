use crate::pagesetup::PageSetup;
use xlcalc::{SheetId, Workbook};

pub(crate) struct Printer {
    pub dpi: f64,

    pub row_num: i64,
    pub row_den: i64,

    pub digit: f64,
    pub col_pad: f64,

    pub screen_digit: i64,
    pub std_screen: i64,

    pub slack: f64,

    pub head_h: f64,
    pub head_w: f64,
}

pub(crate) const PDF600: Printer = Printer {
    dpi: 600.0,
    row_num: 127,
    row_den: 29,
    digit: 47.0,
    col_pad: 30.0,
    screen_digit: 11,
    std_screen: 96,
    slack: 8.0,
    head_h: 127.0,
    head_w: 173.0,
};

impl Printer {
    fn paper(&self, size: f64) -> (f64, f64) {
        let mm = |w: f64, h: f64| ((w / 25.4 * self.dpi).round(), (h / 25.4 * self.dpi).round());
        let inch = |w: f64, h: f64| (w * self.dpi, h * self.dpi);
        match size as i32 {
            3 => inch(11.0, 17.0),
            5 => inch(8.5, 14.0),
            6 => inch(5.5, 8.5),
            7 => inch(7.25, 10.5),
            8 => mm(297.0, 420.0),
            9 => mm(210.0, 297.0),
            11 => mm(148.0, 210.0),
            12 => mm(257.0, 364.0),
            13 => mm(182.0, 257.0),
            19 => inch(3.875, 8.875),
            20 => inch(4.125, 9.5),
            24 => inch(17.0, 22.0),
            25 => inch(22.0, 34.0),
            26 => inch(34.0, 44.0),
            27 => mm(110.0, 220.0),
            28 => mm(162.0, 229.0),
            30 => mm(229.0, 324.0),
            34 => mm(176.0, 250.0),
            37 => inch(3.875, 7.5),
            43 => mm(100.0, 148.0),
            66 => mm(420.0, 594.0),
            70 => mm(105.0, 148.0),
            71 => mm(240.0, 332.0),
            72 => mm(216.0, 277.0),
            73 => mm(120.0, 235.0),
            74 => mm(90.0, 205.0),
            91 => mm(105.0, 235.0),
            119 => inch(28.0, 40.0),
            120 => inch(9.0, 12.0),
            121 => inch(12.0, 18.0),
            122 => inch(18.0, 24.0),
            123 => inch(24.0, 36.0),
            124 => inch(30.0, 42.0),
            125 => inch(36.0, 48.0),
            126 => inch(2.0, 3.5),
            129 => inch(14.0, 17.0),
            130 => inch(3.5, 5.0),
            _ => inch(8.5, 11.0),
        }
    }

    fn capacity(&self, ps: &PageSetup) -> (f64, f64) {
        let (mut w, mut h) = self.paper(ps.paper_size);
        if ps.orientation == 2.0 {
            std::mem::swap(&mut w, &mut h);
        }
        let m = |pt: f64| (pt / 72.0 * self.dpi).trunc();
        let (mut cw, mut ch) = (
            w - m(ps.left) - m(ps.right) - self.slack,
            h - m(ps.top) - m(ps.bottom) - self.slack,
        );
        if ps.headings {
            let z = ps.zoom.unwrap_or(100);
            cw -= scaled(self.head_w, z);
            ch -= scaled(self.head_h, z);
        }
        (cw, ch)
    }

    fn row_px(&self, h8: u32) -> f64 {
        let v = (i64::from(h8) * self.row_num * 2 + self.row_den) / (self.row_den * 2);
        (v >> 2) as f64
    }

    fn col_px(&self, wb: &Workbook, sheet: SheetId, col: u32, cap: f64) -> f64 {
        if wb.col_px(sheet, col) == wb.sheet_std_px(sheet) {
            let (n, d) = (self.digit as i64, self.screen_digit);
            return ((self.std_screen * n + d / 2) / d) as f64;
        }
        (self.digit * wb.column_width(sheet, col) + self.col_pad)
            .round()
            .min(cap)
    }
}

fn scaled(px: f64, zoom: i32) -> f64 {
    ((px * f64::from(zoom) + 50.0) / 100.0).floor()
}

struct Span {
    rows: (u32, u32),
    cols: (u32, u32),
    whole: bool,
}

fn col_num(s: &str) -> Option<u32> {
    if s.is_empty() || !s.bytes().all(|c| c.is_ascii_alphabetic()) {
        return None;
    }
    Some(s.bytes().fold(0, |n, c| {
        n * 26 + u32::from(c.to_ascii_uppercase() - b'A' + 1)
    }))
}

fn part(s: &str) -> Option<(Option<u32>, Option<u32>)> {
    let s = s.replace('$', "");
    let i = s.find(|c: char| c.is_ascii_digit()).unwrap_or(s.len());
    let (c, r) = s.split_at(i);
    let col = if c.is_empty() {
        None
    } else {
        Some(col_num(c)?)
    };
    let row = if r.is_empty() {
        None
    } else {
        Some(r.parse().ok()?)
    };
    (col.is_some() || row.is_some()).then_some((col, row))
}

fn parse(text: &str) -> Option<Span> {
    let (a, b) = text.split_once(':').unwrap_or((text, text));
    let (a, b) = (part(a)?, part(b)?);
    let whole = a.0.is_none() || a.1.is_none();
    Some(Span {
        rows: (a.1.unwrap_or(1), b.1.unwrap_or(1_048_576)),
        cols: (a.0.unwrap_or(1), b.0.unwrap_or(16_384)),
        whole,
    })
}

struct Walk<'a> {
    limit: f64,
    window: u32,
    breaks: &'a [u32],

    titles: Option<(u32, u32)>,
}

fn walk_breaks((first, last): (u32, u32), w: &Walk, size: &dyn Fn(u32) -> f64) -> Vec<u32> {
    let title: f64 = w
        .titles
        .map_or(0.0, |(a, b)| (a..=b.min(a + 4000)).map(size).sum());
    let (mut used, mut base, mut out) = (0.0, 0.0, Vec::new());
    for i in first..=last.min(first.saturating_add(w.window - 1)) {
        let h = size(i);
        let manual = i > first && w.breaks.contains(&i);
        if manual || (used + h > w.limit && used > base) {
            out.push(i);
            base = w.titles.filter(|t| i > t.1).map_or(0.0, |_| title);
            used = base;
        }
        used += h;
    }
    out
}

fn paginate(span: (u32, u32), w: &Walk, size: &dyn Fn(u32) -> f64) -> (u32, u32) {
    let b = walk_breaks(span, w, size);
    (b.first().copied().unwrap_or(span.1 + 1), b.len() as u32 + 1)
}

struct Axes<'a> {
    wb: &'a Workbook,
    sheet: SheetId,
    pr: &'a Printer,
    cap: (f64, f64),
    headings: bool,
}

impl Axes<'_> {
    fn row(&self, r: u32) -> f64 {
        if self.wb.row_hidden(self.sheet, r - 1) {
            0.0
        } else {
            self.pr.row_px(self.wb.row_h8(self.sheet, r - 1))
        }
    }

    fn col(&self, c: u32) -> f64 {
        if self.wb.col_hidden(self.sheet, c - 1) {
            0.0
        } else {
            self.pr.col_px(self.wb, self.sheet, c - 1, self.cap.0)
        }
    }

    fn ldxy(
        &self,
        (first, last): (u32, u32),
        cap: i64,
        scale: i64,
        f: &dyn Fn(u32) -> f64,
    ) -> Ldxy {
        let mut o = Ldxy::default();
        let mut page = 0;
        for i in first..=last.min(first.saturating_add(4000)) {
            let e = scaled(f(i), scale as i32) as i64;
            if page + e >= cap {
                o.count += 1;
                o.waste += cap - page;
                page = e;
            } else {
                page += e;
            }
            o.total += e;
        }
        o
    }

    fn fit_zoom(&self, sp: &Span, titles: &Titles, wide: Option<i32>, tall: Option<i32>) -> i32 {
        let muldiv = |a: i64, b: i64, c: i64| {
            if c == 0 {
                -1
            } else {
                (a * b + c / 2) / c
            }
        };
        let (wide, tall) = (i64::from(wide.unwrap_or(0)), i64::from(tall.unwrap_or(0)));
        let (hw, hh) = if self.headings {
            (self.pr.head_w as i64, self.pr.head_h as i64)
        } else {
            (0, 0)
        };
        let (cw, ch) = (self.cap.0 as i64 + hw, self.cap.1 as i64 + hh);

        let spread = |m: &Ldxy, n: i64| {
            if m.count > 0 {
                m.total + (m.waste / m.count) * (n - 1)
            } else {
                m.total
            }
        };
        let (mut scale, mut dir) = (100_i64, 0_i64);
        for _ in 0..2000 {
            let tc = titles
                .cols
                .map(|t| self.ldxy(t, cw, scale, &|c| self.col(c)));
            let tr = titles
                .rows
                .map(|t| self.ldxy(t, ch, scale, &|r| self.row(r)));
            let ac = self.ldxy(sp.cols, cw, scale, &|c| self.col(c));
            let ar = self.ldxy(sp.rows, ch, scale, &|r| self.row(r));
            let (tcw, tcx) = tc.map_or((0, 0), |m| (m.total, m.waste));
            let (trw, trx) = tr.map_or((0, 0), |m| (m.total, m.waste));
            let tcs = tc.map_or(0, |m| spread(&m, wide));
            let trs = tr.map_or(0, |m| spread(&m, tall));
            let need_w = tcw * wide + ac.waste + ac.total + tcx + hw;
            let need_h = trw * tall + ar.waste + ar.total + trx + hh;
            if ((wide == 0 || need_w <= cw * wide) && (tall == 0 || need_h <= ch * tall))
                || need_w == 0
                || need_h == 0
            {
                break;
            }
            let rw = muldiv(cw * 100, wide, tcs * wide + spread(&ac, wide) + hw);
            let rh = muldiv(ch * 100, tall, trs * tall + spread(&ar, tall) + hh);
            let (rw, rh) = match (wide, tall) {
                (0, _) => (rh, rh),
                (_, 0) => (rw, rw),
                _ => (rw, rh),
            };
            let mut next = muldiv(rw.min(rh), scale, 100).clamp(10, 400);
            if dir == 0 {
                dir = if next <= scale { -1 } else { 1 };
            } else if next * dir < scale * dir {
                next = scale;
            }
            if next == scale {
                next += dir;
            }
            scale = next.clamp(10, 400);
            if scale == 10 || scale == 400 {
                break;
            }
        }
        scale as i32
    }
}

#[derive(Default, Clone, Copy)]
struct Ldxy {
    total: i64,
    count: i64,
    waste: i64,
}

fn zoom_of(ax: &Axes, ps: &PageSetup, sp: &Span, titles: &Titles) -> i32 {
    ps.zoom
        .unwrap_or_else(|| ax.fit_zoom(sp, titles, ps.fit_wide, ps.fit_tall))
}

fn breaks_of(wb: &Workbook, sheet: SheetId, rows: bool) -> Vec<u32> {
    wb.all_page_breaks(sheet, rows)
        .iter()
        .map(|b| b + 1)
        .collect()
}

#[derive(Default)]
pub(crate) struct Titles {
    pub rows: Option<(u32, u32)>,
    pub cols: Option<(u32, u32)>,
}

impl Titles {
    pub(crate) fn parse(rows: &str, cols: &str) -> Titles {
        Titles {
            rows: parse(rows).map(|s| s.rows),
            cols: parse(cols).map(|s| s.cols),
        }
    }
}

pub(crate) fn layout_signature(wb: &Workbook, sheet: SheetId) -> u64 {
    use std::hash::{Hash, Hasher};
    let mut h = std::collections::hash_map::DefaultHasher::new();
    wb.row_custom_heights(sheet).hash(&mut h);
    wb.hidden_rows(sheet).hash(&mut h);
    wb.column_state(sheet).hash(&mut h);
    wb.sheet_std_px(sheet).hash(&mut h);
    wb.all_page_breaks(sheet, true).hash(&mut h);
    wb.all_page_breaks(sheet, false).hash(&mut h);
    h.finish() | 1
}

pub(crate) type Window = (u32, u32);

fn layout_at(
    ax: &Axes,
    ps: &PageSetup,
    zoom: Option<i32>,
    parts: &[String],
    titles: &Titles,
    win: Window,
) -> (u32, Option<(u32, u32)>) {
    let wb = ax.wb;
    let (hb, vb) = (
        breaks_of(wb, ax.sheet, true),
        breaks_of(wb, ax.sheet, false),
    );
    let mut total = 0;
    let mut single = None;
    for p in parts {
        let Some(sp) = parse(p) else { continue };
        let z = zoom.unwrap_or_else(|| zoom_of(ax, ps, &sp, titles));
        let rows = Walk {
            limit: ax.cap.1,
            window: win.0,
            breaks: &hb,
            titles: titles.rows,
        };
        let cols = Walk {
            limit: ax.cap.0,
            window: win.1,
            breaks: &vb,
            titles: titles.cols,
        };
        let (rb, rp) = paginate(sp.rows, &rows, &|r| scaled(ax.row(r), z));
        let (cb, cp) = paginate(sp.cols, &cols, &|c| scaled(ax.col(c), z));
        if !sp.whole {
            total += rp * cp;
        }
        if parts.len() == 1 {
            let v = |first: u32, b: u32| if first > 1 { first } else { b };
            single = Some((v(sp.rows.0, rb), v(sp.cols.0, cb)));
        }
    }
    (total, single)
}

pub(crate) fn layout(
    wb: &Workbook,
    sheet: SheetId,
    ps: &PageSetup,
    parts: &[String],
    titles: &Titles,
    win: Window,
) -> (u32, Option<(u32, u32)>) {
    let pr = &PDF600;
    let ax = Axes {
        wb,
        sheet,
        pr,
        cap: pr.capacity(ps),
        headings: ps.headings,
    };

    let now = ps.zoom.unwrap_or(100);
    let fresh = layout_at(&ax, ps, Some(now), parts, titles, win);
    let listed = (ps.list_zoom > 0).then_some(ps.list_zoom);
    if listed == Some(now) && ps.list_win.is_none_or(|w| w == win) {
        return fresh;
    }
    let cached = layout_at(&ax, ps, listed, parts, titles, ps.list_win.unwrap_or(win));
    let first = match (cached.1, fresh.1) {
        (Some(a), Some(b)) => Some((a.0.min(b.0), a.1.min(b.1))),
        (a, b) => a.or(b),
    };
    (cached.0, first)
}

pub(crate) type BreakRun = Vec<(u32, bool)>;

pub(crate) fn cached_breaks(
    wb: &Workbook,
    sheet: SheetId,
    ps: &PageSetup,
    parts: &[String],
    titles: &Titles,
    win: Window,
) -> (BreakRun, BreakRun) {
    let pr = &PDF600;
    let ax = Axes {
        wb,
        sheet,
        pr,
        cap: pr.capacity(ps),
        headings: ps.headings,
    };
    let (hb, vb) = (breaks_of(wb, sheet, true), breaks_of(wb, sheet, false));
    let win = ps.list_win.unwrap_or(win);
    let listed = (ps.list_zoom > 0).then_some(ps.list_zoom);
    let (mut rows, mut cols) = (Vec::new(), Vec::new());
    for p in parts {
        let Some(sp) = parse(p) else { continue };
        if sp.whole {
            continue;
        }
        let z = listed.unwrap_or_else(|| zoom_of(&ax, ps, &sp, titles));
        let rw = Walk {
            limit: ax.cap.1,
            window: win.0,
            breaks: &hb,
            titles: titles.rows,
        };
        let cw = Walk {
            limit: ax.cap.0,
            window: win.1,
            breaks: &vb,
            titles: titles.cols,
        };
        let tag = |v: Vec<u32>, m: &[u32]| {
            v.into_iter()
                .map(|i| (i, m.contains(&i)))
                .collect::<Vec<_>>()
        };
        rows.extend(tag(
            walk_breaks(sp.rows, &rw, &|r| scaled(ax.row(r), z)),
            &hb,
        ));
        cols.extend(tag(
            walk_breaks(sp.cols, &cw, &|c| scaled(ax.col(c), z)),
            &vb,
        ));
    }
    (rows, cols)
}

pub(crate) fn used_layout(
    wb: &Workbook,
    sheet: SheetId,
    ps: &PageSetup,
    last_row: u32,
    last_col: u32,
    win: Window,
) -> (Option<u32>, Option<u32>) {
    let pr = &PDF600;
    let ax = Axes {
        wb,
        sheet,
        pr,
        cap: pr.capacity(ps),
        headings: ps.headings,
    };
    let sp = Span {
        rows: (1, last_row + 1),
        cols: (1, last_col + 1),
        whole: false,
    };
    let breaks = (breaks_of(wb, sheet, true), breaks_of(wb, sheet, false));
    let at = |z: i32, win: Window| {
        let rows = Walk {
            limit: ax.cap.1,
            window: win.0,
            breaks: &breaks.0,
            titles: None,
        };
        let cols = Walk {
            limit: ax.cap.0,
            window: win.1,
            breaks: &breaks.1,
            titles: None,
        };
        let (rb, _) = paginate(sp.rows, &rows, &|r| scaled(ax.row(r), z));
        let (cb, _) = paginate(sp.cols, &cols, &|c| scaled(ax.col(c), z));
        (
            (rb <= last_row + 1).then_some(rb),
            (cb <= last_col + 1).then_some(cb),
        )
    };
    let now = ps.zoom.unwrap_or(100);
    let (r, c) = at(now, win);
    let listed = if ps.list_zoom > 0 {
        ps.list_zoom
    } else {
        zoom_of(&ax, ps, &sp, &Titles::default())
    };
    if listed == now {
        return (r, c);
    }
    let (r2, c2) = at(listed, ps.list_win.unwrap_or(win));
    let first = |a: Option<u32>, b: Option<u32>| match (a, b) {
        (Some(a), Some(b)) => Some(a.min(b)),
        (a, b) => a.or(b),
    };
    (first(r, r2), first(c, c2))
}

pub(crate) struct UsedBreaks {
    pub cached: (Vec<u32>, Vec<u32>),

    pub fresh: (Vec<u32>, Vec<u32>),
}

pub(crate) fn used_breaks(
    wb: &Workbook,
    sheet: SheetId,
    ps: &PageSetup,
    (last_row, last_col): (u32, u32),
    win: Window,
    primed: bool,
) -> UsedBreaks {
    let pr = &PDF600;
    let ax = Axes {
        wb,
        sheet,
        pr,
        cap: pr.capacity(ps),
        headings: ps.headings,
    };
    let sp = Span {
        rows: (1, last_row + 1),
        cols: (1, last_col + 1),
        whole: false,
    };
    let (hb, vb) = (breaks_of(wb, sheet, true), breaks_of(wb, sheet, false));
    let now = zoom_of(&ax, ps, &sp, &Titles::default());
    let listed = if ps.list_zoom > 0 { ps.list_zoom } else { now };
    let lwin = ps.list_win.unwrap_or(win);
    let walk = |z: i32, limit: (u32, u32), hm: &[u32], vm: &[u32]| {
        let rows = Walk {
            limit: ax.cap.1,
            window: limit.0,
            breaks: hm,
            titles: None,
        };
        let cols = Walk {
            limit: ax.cap.0,
            window: limit.1,
            breaks: vm,
            titles: None,
        };
        (
            walk_breaks(sp.rows, &rows, &|r| scaled(ax.row(r), z)),
            walk_breaks(sp.cols, &cols, &|c| scaled(ax.col(c), z)),
        )
    };
    let cached = walk(listed, lwin, &hb, &vb);
    let merge = |a: &[u32], b: &[u32]| {
        let mut m: Vec<u32> = a.iter().chain(b).copied().collect();
        m.sort_unstable();
        m.dedup();
        m
    };

    let none: (Vec<u32>, Vec<u32>) = (Vec::new(), Vec::new());
    let forced = if primed { &cached } else { &none };
    let fresh = walk(
        now,
        (u32::MAX, u32::MAX),
        &merge(&hb, &forced.0),
        &merge(&vb, &forced.1),
    );
    UsedBreaks { cached, fresh }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn capacity_of_the_default_page() {
        let pr = &PDF600;
        let ps = PageSetup::default();

        assert_eq!(pr.capacity(&ps), (4252.0, 5692.0));
        let land = PageSetup {
            orientation: 2.0,
            ..PageSetup::default()
        };
        assert_eq!(
            pr.capacity(&land),
            (6600.0 - 840.0 - 8.0, 5100.0 - 900.0 - 8.0)
        );
    }

    #[test]
    fn rows_of_the_default_height_fit_44_to_a_page() {
        let pr = &PDF600;
        assert_eq!(pr.row_px(116), 127.0);

        let w = Walk {
            limit: 5692.0,
            window: 96,
            breaks: &[],
            titles: None,
        };
        assert_eq!(paginate((1, 200), &w, &|_| 127.0), (45, 3));

        let w = Walk {
            titles: Some((1, 2)),
            ..w
        };
        assert_eq!(paginate((1, 200), &w, &|_| 127.0), (45, 3));

        let w = Walk {
            limit: 4252.0,
            window: u32::MAX,
            breaks: &[],
            titles: Some((1, 2)),
        };
        assert_eq!(paginate((1, 52), &w, &|_| 410.0), (11, 7));
    }

    #[test]
    fn standard_column_is_410_px() {
        let pr = &PDF600;
        assert_eq!((pr.std_screen * 47 + 5) / 11, 410);

        assert_eq!((117 * 47 + 5) / 11, 500);
        assert_eq!((227 * 47 + 5) / 11, 970);
    }

    #[test]
    fn paper_sizes_traced_at_600_dpi() {
        let pr = &PDF600;

        for (n, w, h) in [
            (1, 5100.0, 6600.0),
            (6, 3300.0, 5100.0),
            (7, 4350.0, 6300.0),
            (8, 7016.0, 9921.0),
            (9, 4961.0, 7016.0),
            (66, 9921.0, 14031.0),
            (19, 2325.0, 5325.0),
            (71, 5669.0, 7843.0),
            (126, 1200.0, 2100.0),
        ] {
            assert_eq!(pr.paper(n.into()), (w, h), "paper {n}");
        }
    }

    #[test]
    fn zoom_rounds_half_up() {
        assert_eq!(scaled(127.0, 50), 64.0);
        assert_eq!(scaled(127.0, 100), 127.0);
    }

    #[test]
    fn fit_to_pages_iterates_to_a_scale_that_fits() {
        let mut wb = Workbook::new();
        let sheet = wb.add_sheet("S").unwrap();
        let pr = &PDF600;
        let ps = PageSetup::default();
        let ax = Axes {
            wb: &wb,
            sheet,
            pr,
            cap: pr.capacity(&ps),
            headings: false,
        };
        let sp = parse("A1:AZ300").unwrap();
        let t = Titles::default();

        for (w, h) in [(1, 1), (2, 3), (5, 5)] {
            let z = ax.fit_zoom(&sp, &t, Some(w), Some(h));
            let at = |z: i32| {
                let c = ax.ldxy(sp.cols, 4252, i64::from(z), &|c| ax.col(c));
                let r = ax.ldxy(sp.rows, 5692, i64::from(z), &|r| ax.row(r));
                (
                    c.total + c.waste <= 4252 * i64::from(w),
                    r.total + r.waste <= 5692 * i64::from(h),
                )
            };
            assert_eq!(at(z), (true, true), "{w}x{h} at {z}");
            assert_ne!(at(z + 1), (true, true), "{w}x{h} at {}", z + 1);
        }

        assert_eq!(ax.fit_zoom(&sp, &t, Some(20), Some(20)), 100);

        let z = ax.fit_zoom(&sp, &t, Some(1), None);
        assert!(z < 100);
    }
}
