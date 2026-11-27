use xlcalc::drawing::{BagVal, Chart, ChartAxis, Part, TITLE_AUTO, TITLE_DELETED, TITLE_TYPED};
use xlcalc_variant::Variant;

use crate::app::{unsupported, Application};
use crate::chart_text::{link_formula, link_text, parse_link};
use crate::charts::{parse_part, part_values};
use crate::conv::Args;
use crate::error::OmError;
use crate::shapes::{DrawKind, DrawRef};
use crate::typelib::{InvokeKind, Member};
use xlcalc::chart_src::series_from_used;

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub(crate) enum Sub {
    Title,
    TitleChars,
    Legend,
    Entries,
    Axes,
    Axis(u8),
    AxisTitle(u8),
    TickLabels(u8),

    Font(u8),
    ChartArea,
    PlotArea,
    SerFormat(u8),
    SerFill(u8),
    SerLine(u8),
    SerFore(u8, bool),
    SerInterior(u8),
    SerBorder(u8),
    SerLabels(u8),
    SerLabel(u8, u16),
    SerPoint(u8, u16),
    TrendColl(u8),
    Trend(u8, u8),
    ErrBars(u8),
    Groups,
    Group(u8),
    AreaFormat,
    AreaFill,
    AreaLine,
    DataTable,
}

impl Sub {
    pub(crate) fn interface_name(self) -> &'static str {
        match self {
            Sub::Title => "ChartTitle",
            Sub::TitleChars => "Characters",
            Sub::Legend => "Legend",
            Sub::Entries => "LegendEntries",
            Sub::Axes => "Axes",
            Sub::Axis(_) => "Axis",
            Sub::AxisTitle(_) => "AxisTitle",
            Sub::TickLabels(_) => "TickLabels",
            Sub::Font(_) => "Font",
            Sub::ChartArea => "ChartArea",
            Sub::PlotArea => "PlotArea",
            Sub::SerFormat(_) => "ChartFormat",
            Sub::SerFill(_) => "FillFormat",
            Sub::SerLine(_) => "LineFormat",
            Sub::SerFore(..) => "ColorFormat",
            Sub::SerInterior(_) => "Interior",
            Sub::SerBorder(_) => "Border",
            Sub::SerLabels(_) => "DataLabels",
            Sub::SerLabel(..) => "DataLabel",
            Sub::SerPoint(..) => "Point",
            Sub::TrendColl(_) => "Trendlines",
            Sub::Trend(..) => "Trendline",
            Sub::ErrBars(_) => "ErrorBars",
            Sub::Groups => "ChartGroups",
            Sub::Group(_) => "ChartGroup",
            Sub::AreaFormat => "ChartFormat",
            Sub::AreaFill => "FillFormat",
            Sub::AreaLine => "LineFormat",
            Sub::DataTable => "DataTable",
        }
    }
    pub(crate) fn series(self) -> Option<u8> {
        match self {
            Sub::SerFormat(k)
            | Sub::SerFill(k)
            | Sub::SerLine(k)
            | Sub::SerFore(k, _)
            | Sub::SerInterior(k)
            | Sub::SerBorder(k)
            | Sub::SerLabels(k)
            | Sub::SerLabel(k, _)
            | Sub::SerPoint(k, _)
            | Sub::TrendColl(k)
            | Sub::Trend(k, _)
            | Sub::ErrBars(k) => Some(k),
            _ => None,
        }
    }
}

impl Application<'_> {
    fn chart_bag(
        &mut self,
        r: &DrawRef,
        key: &str,
        slot: &str,
        m: &'static Member,
        a: &Args,
    ) -> Result<Variant, OmError> {
        match self.bag_member(crate::bag::Store::Chart(r), key, slot, m, a) {
            Some(res) => res,
            None => Err(unsupported(r.kind.interface_name(), m)),
        }
    }

    fn axes_item(&mut self, r: &DrawRef, a: &Args) -> Result<Variant, OmError> {
        let t = match a.get(0) {
            Some(Variant::Bstr(_)) => return Err(OmError::exception(0x8002_0005_u32 as i32, None)),
            Some(Variant::Null | Variant::Bool(_)) | None => return Err(e_fail()),
            Some(_) => a.i32(0)?.unwrap_or(0),
        };
        let mut group = 1;
        match a.get(1) {
            None => {}
            Some(Variant::Null | Variant::Bool(_)) => return Err(e_fail()),
            Some(Variant::Bstr(_)) => {
                return Err(OmError::arg(crate::error::DISP_E_TYPEMISMATCH, 0))
            }
            Some(_) => group = a.i32(1)?.unwrap_or(0),
        }
        if group != 1 {
            let has = self.chart_of(r)?.series.iter().any(|s| s.axis_group == 2);
            if group == 2 && t == 2 && has {
                return Ok(self.sub_obj(r, Sub::Axis(2)));
            }

            if group == 2 && t == 1 && has {
                return Ok(self.sub_obj(r, Sub::Axis(SECONDARY_CAT)));
            }
            return Err(e_fail());
        }
        match t {
            1 | 2 => Ok(self.sub_obj(r, Sub::Axis(t as u8 - 1))),
            3 => Err(OmError::exception(0x8007_0057_u32 as i32, None)),
            _ => Err(e_fail()),
        }
    }
}

fn e_fail() -> OmError {
    OmError::exception(0x8000_4005_u32 as i32, None)
}

pub(crate) fn range_err(desc: &str) -> OmError {
    OmError::exception(0x800A_03EC_u32 as i32, Some(desc))
}

pub(crate) fn auto_title(wb: &xlcalc::Workbook, ch: &Chart) -> String {
    ch.lone_series()
        .and_then(|s| match &s.name {
            Part::Text(t) if !t.starts_with('{') => Some(t.clone()).filter(|t| !t.is_empty()),
            name => match crate::charts::part_values(wb, name).first() {
                Some(Variant::Bstr(t)) if !t.is_empty() => Some(t.clone()),
                Some(Variant::R8(n)) => Some(xlcalc::format_general(*n)),
                _ => None,
            },
        })
        .unwrap_or_else(|| "Chart Title".into())
}

pub(crate) fn ensure_auto_title(wb: &xlcalc::Workbook, ch: &mut Chart) {
    if ch.auto_title_series().is_none() {
        if ch.bag.remove(TITLE_AUTO).is_some() {
            ch.has_title = false;
        }
        return;
    }
    if !ch.has_title {
        new_title(wb, ch);
    }
    if !ch.bag.contains_key(TITLE_TYPED) {
        ch.bag.insert(TITLE_AUTO.into(), BagVal::I(1));
        ch.title = auto_title(wb, ch);
    }
}

fn put_has_title(wb: &xlcalc::Workbook, ch: &mut Chart, v: bool) {
    if v && !ch.has_title {
        new_title(wb, ch);
    } else if !v && ch.has_title {
        delete_title(ch);
    }
}

fn new_title(wb: &xlcalc::Workbook, ch: &mut Chart) {
    ch.has_title = true;
    ch.title = auto_title(wb, ch);
    ch.title_ref = None;
    for key in [TITLE_DELETED, TITLE_AUTO, TITLE_TYPED] {
        ch.bag.remove(key);
    }
}

fn delete_title(ch: &mut Chart) {
    ch.has_title = false;
    ch.bag.remove(TITLE_AUTO);
    ch.bag.remove(TITLE_TYPED);
    ch.bag.insert(TITLE_DELETED.into(), BagVal::I(1));
}

fn typed_title(ch: &mut Chart) {
    ch.bag.remove(TITLE_AUTO);
    ch.bag.insert(TITLE_TYPED.into(), BagVal::I(1));
}

fn is_bubble(t: i32) -> bool {
    matches!(t, 15 | 87 | 88)
}

fn is_pie(t: i32) -> bool {
    matches!(t, 5 | 68 | 69 | 70 | 71 | 80 | -4102 | -4120)
}

pub(crate) fn put_bool(a: &Args) -> Result<bool, OmError> {
    match a.put.as_ref() {
        Some(Variant::Bool(x)) => Ok(*x),
        _ => Err(OmError::type_mismatch()),
    }
}

pub(crate) fn put_text(a: &Args) -> Result<String, OmError> {
    match a.put.as_ref() {
        Some(Variant::Bstr(t)) => Ok(t.clone()),
        _ => Err(OmError::type_mismatch()),
    }
}

pub(crate) fn put_num(a: &Args) -> Result<f64, OmError> {
    a.put
        .as_ref()
        .map(|v| crate::controls::num_arg(v, a.lcid))
        .transpose()?
        .ok_or_else(OmError::type_mismatch)
}

pub(crate) const FONT_AREA: u8 = 0;
pub(crate) const FONT_LEGEND: u8 = 1;
pub(crate) const FONT_TICKS: u8 = 2;

pub(crate) fn font_name(c: &Chart, p: u8) -> String {
    for q in [p, FONT_AREA] {
        if let Some(BagVal::S(s)) = c.bag.get(&format!("Font{q}.Name")) {
            return s.clone();
        }
    }
    "Aptos Narrow".to_string()
}

pub(crate) fn font_size(c: &Chart, p: u8) -> f64 {
    for q in [p, FONT_AREA] {
        if let Some(BagVal::D(v)) = c.bag.get(&format!("Font{q}.Size")) {
            return *v;
        }
    }
    10.0
}

fn font_rich(c: &Chart, p: u8) -> bool {
    let u = matches!(c.bag.get(&format!("Font{p}.Underline")), Some(BagVal::I(v)) if *v != -4142);
    let s = matches!(c.bag.get(&format!("Font{p}.Strike")), Some(BagVal::B(true)));
    u || s
}

pub(crate) fn chart_fonts(c: &Chart) -> crate::chartfont::Fonts {
    let f = |p: u8| {
        let mut t = crate::chartfont::TextFont::new(&font_name(c, p), font_size(c, p));
        t.rich = font_rich(c, p);
        t
    };
    crate::chartfont::Fonts {
        cat: f(FONT_TICKS),
        val: f(FONT_TICKS + 1),
        legend: f(FONT_LEGEND),
    }
}

pub(crate) const SCALED: &str = "#scaled";

pub(crate) const SCALE_LEN: &str = "#scalelen";

fn auto_scale(
    vals: &[f64],
    intervals: &dyn Fn(f64) -> f64,
    extra: f64,
    pad: bool,
    bubbles: usize,
    fixed: (Option<f64>, Option<f64>, Option<f64>),
) -> (f64, f64, f64, f64) {
    let (fmin, fmax, fmajor) = fixed;
    let (mut lo0, mut hi0) = (f64::MAX, f64::MIN);
    for &v in vals {
        lo0 = lo0.min(v);
        hi0 = hi0.max(v);
    }
    if vals.is_empty() || (lo0 == 0.0 && hi0 == 0.0) {
        (lo0, hi0) = (0.0, 0.0);
    }
    let (mut lo, mut hi) = (fmin.unwrap_or(lo0), fmax.unwrap_or(hi0));
    let (min_auto, max_auto) = (fmin.is_none(), fmax.is_none());
    let range = hi - lo;
    if range == 0.0 && min_auto && max_auto {
        if lo == 0.0 {
            hi = 1.0;
        } else if lo < 0.0 {
            hi = 0.0;
        } else {
            lo = 0.0;
        }
    } else if range > 0.0 {
        let above = if min_auto && lo > 0.0 {
            Some(true)
        } else if max_auto && hi < 0.0 {
            Some(false)
        } else {
            None
        };
        if let Some(above) = above {
            let base = if above { hi } else { lo.abs() };
            if base / 6.0 - range < 0.0 {
                if above {
                    lo = 0.0;
                } else {
                    hi = 0.0;
                }
            } else if above {
                lo -= range * 0.5;
            } else {
                hi += range * 0.5;
            }
        }
    }

    let r = (hi - lo) / 20.0;
    if pad && max_auto && hi != 0.0 && hi - hi0 - r < 0.0 {
        hi = hi0 + r;
    }
    if pad && min_auto && lo != 0.0 && lo - lo0 + r > 0.0 {
        lo = lo0 - r;
    }

    for _ in 0..bubbles {
        let room = 100.0 * 3.0 / 1000.0 * ((hi - lo) * 1.1) * 0.5;
        let over = room - (hi - hi0);
        if over > 0.0 && max_auto {
            hi += over;
        }
        let under = room - (lo0 - lo);
        if under > 0.0 && min_auto {
            lo -= under;
        }
    }
    let (mut bottom, mut top) = (lo, hi);
    if top <= bottom {
        if fmax.is_some() && min_auto {
            bottom = top - top.abs().max(1.0);
        } else {
            top = bottom + bottom.abs().max(1.0);
        }
    }
    let span = top - bottom;
    let tol = |x: f64| 1e-9 * x.abs();

    let grid = |step: f64| {
        let min = fmin.unwrap_or_else(|| {
            let o = fmax.unwrap_or(0.0);
            let m = o + ((bottom - o) / step).round() * step;
            (if m - bottom > tol(bottom) {
                m - step
            } else if (m - bottom).abs() <= tol(bottom) {
                bottom
            } else {
                m
            }) + 0.0
        });
        let max = match fmax {
            Some(m) => m,
            None => {
                let o = fmin.unwrap_or(0.0);
                let m = o + ((top - o) / step).round() * step;
                (if top - m > tol(top) {
                    m + step
                } else if (top - m).abs() <= tol(top) {
                    top
                } else {
                    m
                }) + 0.0
            }
        };
        (min, max)
    };

    let step = match fmajor {
        Some(m) if m > 0.0 => m,
        _ => {
            let room = intervals(top);
            let mut p = pow10(span.log10().ceil() as i32 - 1);
            let v = (span / room / p * 100.0).ceil();
            let table = [10.0, 20.0, 50.0, 100.0, 200.0, 500.0, 1000.0];
            let mut k = table.iter().position(|&m| v <= m).unwrap_or(6);
            loop {
                let step = (table[k] / 100.0) * p;
                let (min, max) = grid(step);
                if (max - min) / step <= room + extra + 1e-9 || p.is_infinite() {
                    break step;
                }
                if k == 6 {
                    k = 3;
                    p *= 10.0;
                } else {
                    k += 1;
                }
            }
        }
    };
    let (min, max) = grid(step);
    (min, max, step, step / 5.0)
}

const APTOS_NARROW_ADVANCE: [u16; 95] = [
    383, 667, 695, 1020, 1038, 1558, 1204, 394, 623, 623, 1020, 1038, 533, 627, 533, 641, 1038,
    1038, 1038, 1038, 1038, 1038, 1038, 1038, 1038, 1038, 533, 533, 1038, 1038, 1038, 932, 1651,
    1094, 1132, 1296, 1284, 1043, 983, 1323, 1323, 498, 625, 1069, 935, 1479, 1320, 1367, 1083,
    1367, 1137, 1063, 893, 1271, 1087, 1665, 1035, 1009, 963, 554, 641, 554, 1038, 852, 1022, 998,
    1053, 982, 1053, 994, 570, 921, 1038, 459, 459, 917, 497, 1606, 1038, 1034, 1053, 1053, 633,
    907, 607, 1045, 848, 1344, 831, 848, 818, 554, 946, 554, 1038,
];

const FONT_UNITS_PER_EM: i64 = 2048;

pub(crate) fn aptos_glyph_units(ch: char, size: f64) -> i64 {
    use crate::aptos_adv::{fullwidth, FALLBACK_UNITS};
    let rnd = |x: f64| (x + 0.5).floor() as i64;
    if fullwidth(ch as u32) {
        return rnd(2000.0 * size / 10.0);
    }
    if matches!(ch as u32, 0x300..=0x36f) {
        return 0;
    }
    if let Ok(i) = FALLBACK_UNITS.binary_search_by_key(&(ch as u32), |&(c, _)| u32::from(c)) {
        return rnd(f64::from(FALLBACK_UNITS[i].1) * size / 10.0);
    }

    if ch == '\t' {
        return rnd(873.0 * 200.0 * size / FONT_UNITS_PER_EM as f64);
    }

    if matches!(ch as u32, 0xFF61..=0xFF9F) {
        return rnd(1000.0 * size / 10.0);
    }

    if let Some(a) = script_advance(ch as u32) {
        return rnd(f64::from(a) * 200.0 * size / FONT_UNITS_PER_EM as f64);
    }
    let adv = match ch as u32 {
        32..=126 => i64::from(APTOS_NARROW_ADVANCE[ch as usize - 32]),
        cp => match crate::aptos_adv::APTOS_NARROW_EXTRA
            .binary_search_by_key(&cp, |&(c, _)| u32::from(c))
        {
            Ok(i) => i64::from(crate::aptos_adv::APTOS_NARROW_EXTRA[i].1),

            Err(_) => i64::from(APTOS_NARROW_ADVANCE[usize::from(b'0' - 32)]),
        },
    };
    rnd(adv as f64 * 200.0 * size / FONT_UNITS_PER_EM as f64)
}

fn script_advance(cp: u32) -> Option<u16> {
    use crate::chart_script_adv::{ARIAL_ADVANCE, TAHOMA_ADVANCE};
    let (table, lo, hi) = match cp {
        0x590..=0x77F => (ARIAL_ADVANCE, 0x590, 0x77F),
        0xE01..=0xE5B => (TAHOMA_ADVANCE, 0xE01, 0xE5B),
        _ => return None,
    };
    debug_assert!(lo <= hi);
    table
        .binary_search_by_key(&cp, |&(c, _)| u32::from(c))
        .ok()
        .map(|i| table[i].1)
}

pub(crate) fn arial_glyph_units(ch: char, size: f64) -> Option<i64> {
    let table = crate::chart_script_adv::ARIAL_ADVANCE;
    let i = table
        .binary_search_by_key(&(ch as u32), |&(c, _)| u32::from(c))
        .ok()?;
    Some((f64::from(table[i].1) * 200.0 * size / FONT_UNITS_PER_EM as f64 + 0.5).floor() as i64)
}

pub(crate) fn series_name_text(
    wb: &xlcalc::Workbook,
    k: usize,
    s: &xlcalc::drawing::Series,
) -> String {
    let name = match &s.name {
        Part::Name(_) => match wb.resolve_part(&s.name) {
            p @ std::borrow::Cow::Owned(Part::Refs(_)) => p,
            _ => std::borrow::Cow::Owned(Part::None),
        },
        p => std::borrow::Cow::Borrowed(p),
    };
    match &*name {
        Part::None => format!("Series{}", s.number(k)),
        Part::Text(t) => t.clone(),
        Part::Refs(v) => {
            let mut parts = Vec::new();
            for (sh, r) in v {
                for row in r.r0..=r.r1 {
                    for col in r.c0..=r.c1 {
                        let addr = xlcalc::CellAddr::new(*sh, row, col);
                        parts.push(match crate::charts::cell_variant(&wb.value(addr)) {
                            Variant::Bstr(t) => t,
                            Variant::R8(n) => crate::charts::number_cell_text(wb, addr, n),
                            _ => String::new(),
                        });
                    }
                }
            }
            parts.join(" ")
        }
        p => part_values(wb, p)
            .first()
            .map_or(String::new(), |v| match v {
                Variant::Bstr(s) => s.clone(),
                Variant::R8(n) => xlcalc::format_general(*n),
                _ => String::new(),
            }),
    }
}

pub(crate) fn series_name_emu(wb: &xlcalc::Workbook, k: usize, s: &xlcalc::drawing::Series) -> i64 {
    legend_font_emu(&series_name_text(wb, k, s))
}

pub(crate) fn wrap_legend_text(s: &str, avail: i64) -> (i64, i64) {
    let whole = legend_font_emu(s);
    if whole <= avail {
        return (whole, 1);
    }
    let space = legend_font_emu(" ");
    let (mut widest, mut lines, mut cur) = (0, 1, -1);
    for word in s.split(' ') {
        let w = legend_font_emu(word);
        if cur >= 0 && cur + space + w > avail {
            widest = widest.max(cur);
            lines += 1;
            cur = w;
        } else {
            cur = if cur < 0 { w } else { cur + space + w };
        }
    }
    (widest.max(cur), lines)
}

pub(crate) fn legend_text_emu(s: &str) -> i64 {
    crate::chartfont::text_emu(crate::chartfont::fonts().cat, s)
}

pub(crate) fn legend_font_emu(s: &str) -> i64 {
    crate::chartfont::text_emu(crate::chartfont::fonts().legend, s)
}

pub(crate) fn val_text_emu(s: &str) -> i64 {
    crate::chartfont::text_emu(crate::chartfont::fonts().val, s)
}

pub(crate) fn digit_emu() -> i64 {
    val_text_emu("0").max(val_text_emu("8"))
}

fn val_line_emu() -> i64 {
    crate::chartfont::line_emu(crate::chartfont::fonts().val)
}

struct AxisGeometry {
    len: i64,
    horizontal: bool,
    percent: bool,

    radar: Option<(i64, i64)>,
}

impl AxisGeometry {
    fn intervals(&self, top: f64) -> f64 {
        let n = if self.horizontal {
            let digits = ((top.abs().log10() + 1.0) as i64).max(6);
            let dw = digit_emu();
            self.len / ((dw + (dw + 3) / 4) * digits)
        } else if let Some((w, h)) = self.radar {
            let r = (w.min(h) / 2).max(1);
            r * 9 / (val_line_emu() * 10)
        } else {
            self.len / val_line_emu() - 2
        };
        let n = if self.percent { n.max(4) } else { n };
        n.clamp(1, 10) as f64
    }
}

pub(crate) fn is_3d_column(t: i32) -> bool {
    matches!(t, -4100 | 54..=56 | 60..=62)
}

const SECONDARY_CAT: u8 = 3;

pub(crate) fn is_xy(t: i32) -> bool {
    matches!(t, -4169 | 72..=75 | 15 | 87 | 88)
}

pub(crate) const KEPT_CAT: &str = "#cat_type";

pub(crate) fn kept_cat(c: &Chart) -> bool {
    is_xy(c.chart_type)
        && c.bag.contains_key(KEPT_CAT)
        && c.series
            .iter()
            .all(|s| s.axis_group != 1 || is_xy(s.chart_type))
}

fn bag_is_time(c: &Chart) -> bool {
    matches!(c.bag.get("Axis1.CategoryType"), Some(BagVal::I(3)))
}

pub(crate) fn axis_type(c: &Chart) -> i32 {
    if !is_xy(c.chart_type) {
        return c.chart_type;
    }
    match c
        .series
        .iter()
        .find(|s| s.axis_group == 1 && !is_xy(s.chart_type))
    {
        Some(s) => s.chart_type,
        None => match c.bag.get(KEPT_CAT) {
            Some(BagVal::I(t)) => *t,
            _ => c.chart_type,
        },
    }
}

fn pow10(e: i32) -> f64 {
    format!("1e{e}").parse().unwrap_or(1.0)
}
impl Application<'_> {
    fn axis_geometry(&self, r: &DrawRef, c: &Chart, i: usize) -> Result<AxisGeometry, OmError> {
        let sh = self.shape_clone(r)?;
        let (_, _, w, h) = self.book(r.book)?.wb.shape_rect_emu(r.sheet, &sh.pos);
        let bar = matches!(c.chart_type, 57..=62);
        let horizontal = if is_xy(axis_type(c)) { i == 0 } else { bar };
        let wb = &self.book(r.book)?.wb;
        let side = c.has_legend && matches!(c.legend_pos, -4152 | -4131 | 2);
        let level = c.has_legend && matches!(c.legend_pos, -4160 | -4107);

        let three = match (is_3d_column(c.chart_type), self.outer_plot(r, c)?) {
            (true, Some(o)) => {
                let cats = c
                    .series
                    .iter()
                    .map(|s| crate::charts::part_values(wb, &s.values).len())
                    .max()
                    .unwrap_or(1);
                let at_max =
                    matches!(c.bag.get("Axis1.Crosses"), Some(BagVal::I(2))) != c.axes[0].reverse;
                Some(
                    crate::chart_3d::View3d::of(c, c.series.len(), cats)
                        .value_axis(o[2], o[3], at_max),
                )
            }
            _ => None,
        };
        let horizontal = three.map_or(horizontal, |t| t.1);
        let len = if let Some((len, _)) = three {
            len
        } else if horizontal {
            let names = c
                .series
                .iter()
                .enumerate()
                .map(|(k, s)| series_name_emu(wb, k, s))
                .max()
                .unwrap_or(0);
            let legend = if side {
                152_400 + crate::chartfont::legend_key_emu(crate::chartfont::fonts().legend) + names
            } else {
                0
            };
            let beside = if is_xy(axis_type(c)) {
                c.axes[1].has_title
            } else {
                c.axes[0].has_title
            };
            w - 279_400 - legend - if beside { 193_104 } else { 0 }
        } else if let Some(BagVal::I(n)) = c.bag.get(SCALE_LEN) {
            i64::from(*n)
        } else {
            h - 279_400
                - if c.has_title { 393_319 } else { 0 }
                - if level {
                    152_400 + crate::chartfont::legend_line_emu(crate::chartfont::fonts().legend)
                } else {
                    0
                }
                - if c.axes[0].has_title { 193_104 } else { 0 }
        };
        Ok(AxisGeometry {
            len,
            horizontal,

            percent: stack_kind(c.chart_type) >= 3 && !is_3d_column(c.chart_type),
            radar: if is_radar(c.chart_type) {
                self.radar_plot_size(r, c)?
            } else {
                None
            },
        })
    }

    pub(crate) fn axis_scale(
        &self,
        r: &DrawRef,
        c: &Chart,
        i: usize,
    ) -> Result<(f64, f64, f64, f64), OmError> {
        let _fonts = crate::chartfont::font_scope(crate::chartparts::chart_fonts(c));
        let ax = &c.axes[i];
        let geom = self.axis_geometry(r, c, i.min(1))?;
        let group = if i == 2 { 2 } else { 1 };
        let vals = self.chart_values(r, c, i == 0, group)?;
        let run = |major: Option<f64>| {
            auto_scale(
                &vals,
                &|top| geom.intervals(top),
                if geom.horizontal { 0.0 } else { 1.0 },
                stack_kind(c.chart_type) <= 2
                    && !is_radar(c.chart_type)
                    && !is_bubble(c.chart_type)
                    && !is_3d_column(c.chart_type),
                if is_bubble(c.chart_type) { 1 } else { 0 },
                (ax.min, ax.max, major),
            )
        };
        let mut scale = run(ax.major);

        if let (None, Some(minor)) = (ax.major, ax.minor) {
            if minor > scale.2 && !ax.log {
                scale = run(Some(minor));
            }
        }
        if ax.log {
            let vals: Vec<f64> = self
                .chart_values(r, c, i == 0, group)?
                .into_iter()
                .filter(|v| *v > 0.0)
                .collect();

            let lo = if stack_kind(c.chart_type) == 2 || is_3d_column(c.chart_type) {
                1.0
            } else {
                vals.iter().copied().fold(f64::MAX, f64::min)
            };
            let hi = vals.iter().copied().fold(f64::MIN, f64::max);
            if !vals.is_empty() {
                let b = ax.log_base;
                let pw = |e: i32| if b == 10.0 { pow10(e) } else { b.powi(e) };
                scale = (
                    pw((lo.ln() / b.ln()).floor() as i32),
                    pw((hi.ln() / b.ln()).ceil() as i32),
                    b,
                    b,
                );
            }
        }
        if i == 1 && stack_kind(c.chart_type) >= 3 {
            scale.0 = scale.0.max(-1.0);
            scale.1 = scale.1.min(1.0);
        }
        Ok(scale)
    }

    fn chart_values(
        &self,
        r: &DrawRef,
        c: &Chart,
        x: bool,
        group: i32,
    ) -> Result<Vec<f64>, OmError> {
        let wb = &self.book(r.book)?.wb;

        let hv = crate::charts::HideView::new(wb, c);
        let pv = |p: &Part| crate::charts::part_values_vis(&hv, p, c.visible_only);
        let mut series: Vec<&xlcalc::drawing::Series> =
            c.series.iter().filter(|s| s.axis_group == group).collect();
        if series.is_empty() {
            series = c.series.iter().collect();
        }
        let mut out = Vec::new();
        let stacked = if x { 0 } else { stack_kind(c.chart_type) };
        if stacked != 0 {
            let cols: Vec<Vec<f64>> = series
                .iter()
                .map(|s| {
                    pv(&s.values)
                        .into_iter()
                        .map(|v| if let Variant::R8(n) = v { n } else { 0.0 })
                        .collect()
                })
                .collect();
            let n = cols.iter().map(Vec::len).max().unwrap_or(0);
            let mut out = Vec::new();
            for k in 0..n {
                let at = |s: &Vec<f64>| s.get(k).copied().unwrap_or(0.0);
                let total: f64 = cols.iter().map(|s| at(s).abs()).sum();
                let (mut pos, mut neg, mut run) = (0.0, 0.0, 0.0);
                for s in &cols {
                    let mut v = at(s);
                    if stacked >= 3 {
                        v = if total == 0.0 { 0.0 } else { v / total };
                    }
                    run += v;
                    if v >= 0.0 {
                        pos += v
                    } else {
                        neg += v
                    }
                    if stacked % 2 == 1 {
                        out.push(run);
                    }
                }
                if stacked % 2 == 0 {
                    out.extend([pos, neg]);
                }
            }
            return Ok(out);
        }

        let cats = c
            .series
            .iter()
            .find(|s| s.axis_group == 1 && !is_xy(s.chart_type))
            .filter(|_| !x && is_xy(c.chart_type))
            .map(|s| match s.x {
                Part::None => pv(&s.values).len(),
                _ => pv(&s.x).len(),
            });
        for s in series {
            let mut vs = pv(if x { &s.x } else { &s.values });
            if let (Some(n), true) = (cats, is_xy(s.chart_type)) {
                vs.truncate(n);
            }

            if x && vs.iter().any(|v| matches!(v, Variant::Bstr(_))) {
                out.extend((1..=vs.len()).map(|k| k as f64));
                continue;
            }

            if x && matches!(s.x, Part::None) {
                let n = pv(&s.values).len();
                out.extend((1..=n).map(|k| k as f64));
                continue;
            }
            for v in vs {
                if let Variant::R8(n) = v {
                    out.push(n);
                }
            }
        }
        Ok(out)
    }

    fn sub_obj(&mut self, r: &DrawRef, k: Sub) -> Variant {
        self.alloc(crate::app::Object::Draw(DrawRef {
            kind: DrawKind::Sub(k),
            ..*r
        }))
    }

    fn chart_title(&mut self, r: &DrawRef, c: &Chart) -> Result<Variant, OmError> {
        if c.auto_title_series().is_some() {
            self.layout_title(r)?;
        }
        Ok(self.sub_obj(r, Sub::Title))
    }

    pub(crate) fn titles_for_xls(&mut self, b: crate::app::BookId) -> Result<(), OmError> {
        let wb = &self.book(b)?.wb;
        let charts: Vec<DrawRef> = wb
            .sheets()
            .filter_map(|s| Some((s.id, wb.drawing(s.id)?)))
            .flat_map(|(sheet, d)| {
                d.shapes.iter().filter_map(move |s| {
                    let c = s.chart.as_ref()?;
                    (c.auto_title_series().is_some() != c.has_title).then_some(DrawRef {
                        book: b,
                        sheet,
                        id: s.id,
                        kind: DrawKind::ChartObject,
                    })
                })
            })
            .collect();
        for r in charts {
            self.layout_title(&r)?;
        }
        Ok(())
    }

    pub(crate) fn layout_title(&mut self, r: &DrawRef) -> Result<(), OmError> {
        let c = self.chart_of(r)?;
        let mut next = c.clone();
        ensure_auto_title(&self.book(r.book)?.wb, &mut next);
        if next == c {
            return Ok(());
        }
        self.chart_edit(r, |wb, ch| {
            ensure_auto_title(wb, ch);
            Ok(())
        })
    }

    pub(crate) fn chart_extra(
        &mut self,
        r: &DrawRef,
        m: &'static Member,
        a: &Args,
        c: &Chart,
    ) -> Option<Result<Variant, OmError>> {
        let put = m.kind == InvokeKind::Put || m.kind == InvokeKind::PutRef;
        Some(match (m.name, put) {
            ("ApplyDataLabels", false) => self.apply_data_labels(r, a),
            ("ChartTitle", false) => self.chart_title(r, c),
            ("Legend", false) => {
                if c.has_legend {
                    Ok(self.sub_obj(r, Sub::Legend))
                } else {
                    Err(range_err("This object doesn't have a legend."))
                }
            }
            ("ChartGroups", _) => match a.get(0) {
                Some(_) if m.kind == InvokeKind::Func => match a.i32(0) {
                    Ok(Some(i)) if i >= 1 && i <= chart_group_count(c) => {
                        Ok(self.sub_obj(r, Sub::Group(i as u8)))
                    }
                    Ok(_) => Err(OmError::bad_index()),
                    Err(e) => Err(e),
                },
                _ => Ok(self.sub_obj(r, Sub::Groups)),
            },
            ("ChartArea", false) => Ok(self.sub_obj(r, Sub::ChartArea)),
            ("PlotArea", false) => Ok(self.sub_obj(r, Sub::PlotArea)),
            ("Axes", false) => match a.get(0) {
                None => Ok(self.sub_obj(r, Sub::Axes)),
                Some(_) => self.axes_item(r, a),
            },
            ("HasAxis", false) => {
                let t = match a.i32(0) {
                    Ok(Some(t)) => t,
                    _ => return Some(Err(e_fail())),
                };
                let g = a.i32(1).ok().flatten().unwrap_or(1);

                let group = match g {
                    1 => true,
                    2 => t == 2 && c.series.iter().any(|s| s.axis_group == 2),
                    _ => false,
                };
                Ok(Variant::Bool(
                    (t == 1 || t == 2)
                        && group
                        && !is_pie(c.chart_type)
                        && !c.axis_hidden[(t - 1) as usize],
                ))
            }
            ("HasAxis", true) => Err(e_fail()),
            ("ChartType", true) => {
                let t = match crate::controls::variant_i4(
                    a.put.as_ref().unwrap_or(&Variant::Empty),
                    a.lcid,
                ) {
                    Ok(t) => t,
                    Err(e) => return Some(Err(e)),
                };

                if let Some(hr) = crate::chart_types::chart_type_put_error(t, c.series.len()) {
                    return Some(Err(OmError::exception(hr as i32, None)));
                }
                return None;
            }
            ("PlotBy", true) => {
                let by = match put_num(a) {
                    Ok(t) => t as i32,
                    Err(e) => return Some(Err(e)),
                };
                if by != 1 && by != 2 {
                    return Some(Err(OmError::exception(0x800A_03EC_u32 as i32, None)));
                }
                self.chart_edit(r, |wb, ch| {
                    if ch.plot_by == by {
                    } else if let Some((s, rect)) = ch.source {
                        let (by, series) =
                            series_from_used(wb, s, rect, Some(by), ch.chart_type, &ch.series);
                        ch.series = series;
                        ch.plot_by = by;
                    } else if ch
                        .series
                        .iter()
                        .any(|s| matches!(s.name, Part::Dead) || matches!(s.x, Part::Dead))
                    {
                        return Err(e_fail());
                    } else {
                        ch.plot_by = by;
                    }
                    Ok(Variant::Empty)
                })
            }
            ("HasTitle", true) => {
                let v = match put_bool(a) {
                    Ok(v) => v,
                    Err(e) => return Some(Err(e)),
                };
                self.chart_edit(r, |wb, ch| {
                    put_has_title(wb, ch, v);
                    Ok(Variant::Empty)
                })
            }
            ("HasLegend", true) => {
                let v = match put_bool(a) {
                    Ok(v) => v,
                    Err(e) => return Some(Err(e)),
                };
                self.chart_edit(r, |_, ch| {
                    if v && !ch.has_legend {
                        ch.legend_pos = -4152;
                    }
                    ch.has_legend = v;
                    Ok(Variant::Empty)
                })
            }
            ("SetElement", _) => {
                let e = a.i32(0).ok().flatten().unwrap_or(-1);
                if !matches!(e, 0..=2 | 100..=106 | 200..=205 | 211 | 300..=306 | 310 | 311 | 330..=335 | 350..=356 | 500..=502 | 600)
                {
                    return Some(Err(if matches!(e, 601 | 602) {
                        range_err("Parameter not valid")
                    } else {
                        e_fail()
                    }));
                }
                self.chart_edit(r, |wb, ch| {
                    match e {
                        0..=2 => put_has_title(wb, ch, e != 0),
                        100 => ch.has_legend = false,
                        101..=106 => {
                            ch.has_legend = true;
                            ch.legend_pos = match e {
                                102 => -4160,
                                103 | 106 => -4131,
                                104 => -4107,
                                _ => -4152,
                            };
                        }
                        201..=205 | 211 => {
                            for s in &mut ch.series {
                                s.fmt.labels.get_or_insert_with(Default::default);
                            }
                        }
                        301..=305 | 310 | 311 => {
                            let ax = &mut ch.axes[usize::from(e >= 310)];
                            if !ax.has_title {
                                ax.has_title = true;
                                ax.title = "Axis Title".into();
                                ax.title_ref = None;
                            }
                        }
                        331 => ch.axes[1].grid_minor = true,
                        333 => ch.axes[0].grid_minor = true,
                        334 => ch.axes[0].grid_major = true,
                        335 => {
                            ch.axes[0].grid_major = true;
                            ch.axes[0].grid_minor = true;
                        }
                        352 => ch.axis_hidden[1] = true,
                        _ => {}
                    }
                    Ok(Variant::Empty)
                })
            }
            ("ApplyLayout", _) => {
                let layout = match a.get(0) {
                    Some(Variant::Null | Variant::Bool(_)) => None,
                    Some(_) => a.i32(0).ok().flatten(),
                    None => None,
                };
                if !layout.is_some_and(|l| (1..=11).contains(&l)) {
                    return Some(Err(OmError::exception(0x8007_0057_u32 as i32, None)));
                }
                match a.get(1) {
                    None => {}
                    Some(Variant::Null | Variant::Bool(_)) => return Some(Err(e_fail())),
                    Some(Variant::Bstr(_)) => {
                        return Some(Err(OmError::exception(0x8002_0005_u32 as i32, None)))
                    }
                    Some(_)
                        if !a
                            .i32(1)
                            .ok()
                            .flatten()
                            .is_some_and(|t| crate::chart_types::owc_type(t).is_some()) =>
                    {
                        return Some(Err(e_fail()))
                    }
                    Some(_) => {}
                }
                self.chart_edit(r, |wb, ch| {
                    put_has_title(wb, ch, true);
                    Ok(Variant::Empty)
                })
            }
            ("Location", _) => {
                let place = a.i32(0).ok().flatten().unwrap_or(0);
                let name = match a.get(1) {
                    Some(Variant::Bstr(t)) => Some(t.clone()),
                    _ => None,
                };
                self.chart_location(r, place, name)
            }
            ("Delete", _) => Err(range_err("Delete method of Chart class failed")),

            ("DataTable", false)
                if !matches!(
                    c.bag.get("Chart.HasDataTable"),
                    Some(xlcalc::drawing::BagVal::B(true))
                ) =>
            {
                Err(OmError::exception(
                    0x800A_03EC_u32 as i32,
                    Some("Object required."),
                ))
            }
            ("DataTable", false) => Ok(self.sub_obj(r, Sub::DataTable)),

            (lv @ ("SeriesNameLevel" | "CategoryLabelLevel"), false) => {
                let key = format!("Chart.{lv}");
                Ok(Variant::I4(match c.bag.get(&key) {
                    Some(xlcalc::drawing::BagVal::I(n)) => *n,
                    _ if lv == "SeriesNameLevel" => series_name_level(c),
                    _ if c.chart_type == 15 || c.chart_type == 87 || c.chart_type == 88 => -3,
                    _ => 0,
                }))
            }
            (lv @ ("SeriesNameLevel" | "CategoryLabelLevel"), true) => {
                let v = match put_num(a) {
                    Ok(v) => v.round_ties_even() as i32,
                    Err(e) => return Some(Err(e)),
                };
                let stored = match v {
                    -3 => -3,
                    0 | -1 if lv != "SeriesNameLevel" || series_name_level(c) != -3 => 0,
                    -2 => return Some(Err(e_fail())),
                    _ => {
                        return Some(Err(OmError::exception(0x800A_03EC_u32 as i32, None)));
                    }
                };
                let key = format!("Chart.{lv}");
                self.chart_edit(r, |_, ch| {
                    ch.bag.insert(key, xlcalc::drawing::BagVal::I(stored));
                    Ok(Variant::Empty)
                })
            }
            ("Name", false) => {
                let wb = &self.book(r.book).ok()?.wb;

                let owner = wb.sheet(r.sheet);
                let shown = if owner.is_some_and(|s| s.kind == xlcalc::SheetKind::Chart) {
                    r.sheet
                } else {
                    wb.active_sheet()
                };
                let sheet = wb
                    .sheet(shown)
                    .map(|s| s.name.to_string())
                    .unwrap_or_default();
                let co = self.shape_clone(r).ok()?.name;
                Ok(Variant::Bstr(format!("{sheet} {co}")))
            }

            ("ChartStyle", false) => Ok(Variant::I4(if c.style == 0 { 2 } else { c.style })),
            ("ChartColor", false) => Ok(Variant::I4(2)),
            ("ChartColor", true) if matches!(a.put, Some(Variant::Bstr(_))) => {
                Err(range_err("This property only accepts integer values."))
            }
            ("DisplayBlanksAs", false) => Ok(Variant::I4(c.blanks)),
            ("PlotVisibleOnly", false) => Ok(Variant::Bool(c.visible_only)),
            ("DisplayBlanksAs", true) => {
                let v = match put_num(a) {
                    Ok(v) => v as i32,
                    Err(e) => return Some(Err(e)),
                };

                self.chart_edit(r, |_, ch| {
                    ch.blanks = v;
                    Ok(Variant::Empty)
                })
            }
            ("PlotVisibleOnly", true) => {
                let v = match put_bool(a) {
                    Ok(v) => v,
                    Err(e) => return Some(Err(e)),
                };
                self.chart_edit(r, |_, ch| {
                    ch.visible_only = v;
                    Ok(Variant::Empty)
                })
            }
            ("BarShape", false) => Ok(Variant::I4(0)),
            _ => return None,
        })
    }

    fn title_text(&self, r: &DrawRef, c: &Chart) -> Result<String, OmError> {
        Ok(match &c.title_ref {
            Some(p) => link_text(&self.book(r.book)?.wb, p),
            None => c.title.clone(),
        })
    }

    fn title_put_link(&self, r: &DrawRef, t: &str) -> Result<Option<Part>, OmError> {
        if t.encode_utf16().count() > 255 {
            return Err(e_fail());
        }
        let book = self.book(r.book)?;
        Ok(t.strip_prefix('=')
            .map(|f| parse_link(&book.wb, r.sheet, f).unwrap_or(Part::None)))
    }

    fn title_formula_put(
        &self,
        r: &DrawRef,
        member: &str,
        f: String,
    ) -> Result<(String, Option<Part>), OmError> {
        if f.encode_utf16().count() > 255 {
            return Err(if member == "Formula" {
                e_fail()
            } else {
                OmError::app_defined()
            });
        }
        let book = self.book(r.book)?;
        match f.strip_prefix('=') {
            Some(rest) => {
                let link = parse_link(&book.wb, r.sheet, rest).ok_or_else(OmError::app_defined)?;
                Ok((String::new(), Some(link)))
            }
            None => Ok((f, None)),
        }
    }

    fn title_formula(
        &self,
        r: &DrawRef,
        member: &str,
        text: &str,
        link: Option<&Part>,
    ) -> Result<Variant, OmError> {
        Ok(Variant::Bstr(match link {
            Some(p) => link_formula(&self.book(r.book)?.wb, p, member.contains("R1C1"))
                .ok_or_else(e_fail)?,
            None => text.to_string(),
        }))
    }

    pub(crate) fn sub_member(
        &mut self,
        r: &DrawRef,
        k: Sub,
        m: &'static Member,
        a: &Args,
    ) -> Result<Variant, OmError> {
        let put = m.kind == InvokeKind::Put || m.kind == InvokeKind::PutRef;
        let c = self.chart_of(r)?;
        match k {
            Sub::Title | Sub::TitleChars => {
                if !c.has_title {
                    return Err(OmError::exception(
                        0x8007_0057_u32 as i32,
                        Some("This object has no title."),
                    ));
                }
                match (m.name, put) {
                    ("Characters", false) if k == Sub::Title => {
                        Ok(self.sub_obj(r, Sub::TitleChars))
                    }
                    ("Text" | "Caption", false) => Ok(Variant::Bstr(self.title_text(r, &c)?)),
                    ("Text" | "Caption", true) => {
                        let t = put_text(a)?;

                        let link = if k == Sub::Title {
                            self.title_put_link(r, &t)?
                        } else {
                            None
                        };
                        self.chart_edit(r, |_, ch| {
                            if t.is_empty() {
                                delete_title(ch);
                            } else {
                                typed_title(ch);
                            }
                            ch.title = if link.is_some() { String::new() } else { t };
                            ch.title_ref = link;
                            Ok(Variant::Empty)
                        })
                    }
                    ("Formula" | "FormulaLocal" | "FormulaR1C1" | "FormulaR1C1Local", false)
                        if k == Sub::Title =>
                    {
                        self.title_formula(r, m.name, &c.title, c.title_ref.as_ref())
                    }
                    ("Formula" | "FormulaLocal" | "FormulaR1C1" | "FormulaR1C1Local", true)
                        if k == Sub::Title =>
                    {
                        let (t, link) = self.title_formula_put(r, m.name, put_text(a)?)?;
                        self.chart_edit(r, |_, ch| {
                            typed_title(ch);
                            ch.title = t;
                            ch.title_ref = link;
                            Ok(Variant::Empty)
                        })
                    }
                    ("Position", true) if k == Sub::Title => {
                        let v = put_num(a)?.round_ties_even() as i32;
                        self.chart_edit(r, |_, ch| {
                            ch.bag.insert(
                                "ChartTitle.Position".into(),
                                xlcalc::drawing::BagVal::I(if v == -4105 { -4105 } else { -4114 }),
                            );
                            Ok(Variant::Empty)
                        })
                    }
                    _ => self.chart_bag(r, "ChartTitle", "ChartTitle", m, a),
                }
            }
            Sub::Legend => match (m.name, put) {
                ("Position", false) => Ok(Variant::I4(c.legend_pos)),
                ("Position", true) => {
                    let p = put_num(a)? as i32;
                    if !matches!(p, -4160 | -4131 | -4152 | -4107 | 2) {
                        return Err(e_fail());
                    }
                    self.chart_edit(r, |_, ch| {
                        ch.legend_pos = p;
                        Ok(Variant::Empty)
                    })
                }
                ("LegendEntries", _) => Ok(self.sub_obj(r, Sub::Entries)),
                ("Font", false) => Ok(self.sub_obj(r, Sub::Font(FONT_LEGEND))),
                _ if !put => match self.geom_get(r, "Legend", &c, m)? {
                    Some(v) => Ok(v),
                    None => self.chart_bag(r, "Legend", "Legend", m, a),
                },
                _ => self.chart_bag(r, "Legend", "Legend", m, a),
            },
            Sub::Axes => match m.name {
                "Count" => Ok(Variant::I4(if is_pie(c.chart_type) { 0 } else { 2 })),
                "Item" | "_Default" => self.axes_item(r, a),
                _ => Err(unsupported("Axes", m)),
            },
            Sub::Entries => match m.name {
                "Count" => Ok(Variant::I4(c.series.len() as i32)),
                _ => Err(unsupported("LegendEntries", m)),
            },
            Sub::ChartArea => match m.name {
                "Name" => Ok(Variant::Bstr("Chart".into())),
                "Format" if !put => Ok(self.sub_obj(r, Sub::AreaFormat)),
                "Font" if !put => Ok(self.sub_obj(r, Sub::Font(FONT_AREA))),
                "Width" | "Height" if !put => {
                    let sh = self.shape_clone(r)?;
                    let (_, _, w, h) = self.book(r.book)?.wb.shape_rect_emu(r.sheet, &sh.pos);
                    self.chart_edit(r, |_, ch| {
                        ch.bag.insert("#laid".into(), xlcalc::drawing::BagVal::I(1));
                        Ok(())
                    })?;
                    let v = if m.name == "Width" { w } else { h };

                    Ok(Variant::R8((v - 76_200).max(0) as f64 / 12_700.0))
                }

                "Left" | "Top" if !put => {
                    let sh = self.shape_clone(r)?;
                    let (x, y, _, _) = self.book(r.book)?.wb.shape_rect_emu(r.sheet, &sh.pos);
                    let v = if m.name == "Left" { x } else { y };
                    Ok(Variant::R8(v as f64 / 12_700.0))
                }
                _ => self.chart_bag(r, "ChartArea", "ChartArea", m, a),
            },
            Sub::PlotArea => match m.name {
                "Name" => Ok(Variant::Bstr("Plot".into())),
                _ if !put => match self.geom_get(r, "PlotArea", &c, m)? {
                    Some(v) => Ok(v),
                    None => self.chart_bag(r, "PlotArea", "PlotArea", m, a),
                },
                _ => {
                    let res = self.chart_bag(r, "PlotArea", "PlotArea", m, a)?;
                    self.geom_put(r, &c, m, a)?;
                    Ok(res)
                }
            },
            Sub::Axis(SECONDARY_CAT) => self.secondary_cat_member(r, &c, m, put),
            Sub::Axis(i) => self.axis_member(r, i as usize, &c, m, a, put),
            Sub::Groups => match m.name {
                "Count" => Ok(Variant::I4(chart_group_count(&c))),
                "Item" | "_Default" => {
                    let i = a.i32(0)?.unwrap_or(0);
                    if i >= 1 && i <= chart_group_count(&c) {
                        Ok(self.sub_obj(r, Sub::Group(i as u8)))
                    } else {
                        Err(OmError::bad_index())
                    }
                }
                _ => Err(unsupported("ChartGroups", m)),
            },
            Sub::Group(i) => self.chart_bag(r, "ChartGroup", &format!("ChartGroup{i}"), m, a),
            Sub::AreaFormat => match (m.name, put) {
                ("Fill", false) => Ok(self.sub_obj(r, Sub::AreaFill)),
                ("Line", false) => Ok(self.sub_obj(r, Sub::AreaLine)),
                _ => self.chart_bag(r, "ChartFormat", "ChartFormat", m, a),
            },
            Sub::AreaFill => self.chart_bag(r, "AreaFill", "AreaFill", m, a),
            Sub::AreaLine => self.chart_bag(r, "AreaLine", "AreaLine", m, a),
            Sub::DataTable => self.chart_bag(r, "DataTable", "DataTable", m, a),
            Sub::AxisTitle(i) => {
                let ax = &c.axes[i as usize];
                let slot = format!("AxisTitle{i}");
                match (m.name, put) {
                    (
                        "Text" | "Caption" | "Formula" | "FormulaLocal" | "FormulaR1C1"
                        | "FormulaR1C1Local",
                        _,
                    ) if !ax.has_title => Err(OmError::object_required()),
                    ("Text" | "Caption", false) => Ok(Variant::Bstr(match &ax.title_ref {
                        Some(p) => link_text(&self.book(r.book)?.wb, p),
                        None => ax.title.clone(),
                    })),
                    ("Text" | "Caption", true) => {
                        let t = put_text(a)?;
                        let link = self.title_put_link(r, &t)?;

                        self.axis_edit(r, i as usize, |x| {
                            x.has_title = !t.is_empty();
                            x.title = if link.is_some() { String::new() } else { t };
                            x.title_ref = link;
                        })
                    }
                    ("Formula" | "FormulaLocal" | "FormulaR1C1" | "FormulaR1C1Local", false) => {
                        self.title_formula(r, m.name, &ax.title, ax.title_ref.as_ref())
                    }
                    ("Formula" | "FormulaLocal" | "FormulaR1C1" | "FormulaR1C1Local", true) => {
                        let (t, link) = self.title_formula_put(r, m.name, put_text(a)?)?;
                        self.axis_edit(r, i as usize, |x| {
                            x.title = t;
                            x.title_ref = link;
                        })
                    }

                    ("Orientation", false)
                        if i != 0 && !c.bag.contains_key(&format!("{slot}.Orientation")) =>
                    {
                        Ok(Variant::I4(-4171))
                    }
                    _ => self.chart_bag(r, "AxisTitle", &slot, m, a),
                }
            }
            Sub::TickLabels(i) => {
                let ax = &c.axes[i as usize];
                match (m.name, put) {
                    ("NumberFormat", false) => Ok(Variant::Bstr(
                        ax.tick_nf.clone().unwrap_or_else(|| "General".into()),
                    )),
                    ("NumberFormatLinked", false) => Ok(Variant::Bool(ax.tick_nf.is_none())),
                    ("NumberFormat", true) => {
                        let t = put_text(a)?;
                        self.axis_edit(r, i as usize, |x| x.tick_nf = Some(t))
                    }
                    ("NumberFormatLinked", true) => {
                        let v = put_bool(a)?;
                        self.axis_edit(r, i as usize, |x| {
                            if v {
                                x.tick_nf = None;
                            } else if x.tick_nf.is_none() {
                                x.tick_nf = Some("General".into());
                            }
                        })
                    }
                    ("Font", false) => Ok(self.sub_obj(r, Sub::Font(FONT_TICKS + i))),
                    _ => Err(unsupported("TickLabels", m)),
                }
            }
            Sub::Font(p) => {
                let key = |what: &str| format!("Font{p}.{what}");
                match (m.name, put) {
                    ("Name", false) => Ok(Variant::Bstr(font_name(&c, p))),
                    ("Size", false) => Ok(Variant::R8(font_size(&c, p))),
                    ("Name", true) => {
                        let t = put_text(a)?;
                        self.chart_edit(r, |_, ch| {
                            ch.bag.insert(key("Name"), xlcalc::drawing::BagVal::S(t));
                            Ok(Variant::Empty)
                        })
                    }
                    ("Size", true) => {
                        let v = put_num(a)?;
                        self.chart_edit(r, |_, ch| {
                            ch.bag.insert(key("Size"), xlcalc::drawing::BagVal::D(v));
                            Ok(Variant::Empty)
                        })
                    }
                    ("Underline", false) => Ok(Variant::I4(match c.bag.get(&key("Underline")) {
                        Some(BagVal::I(v)) => *v,
                        _ => -4142,
                    })),
                    ("Strikethrough", false) => Ok(Variant::Bool(matches!(
                        c.bag.get(&key("Strike")),
                        Some(BagVal::B(true))
                    ))),
                    ("Underline", true) => {
                        let v = put_num(a)? as i32;
                        self.chart_edit(r, |_, ch| {
                            ch.bag
                                .insert(key("Underline"), xlcalc::drawing::BagVal::I(v));
                            Ok(Variant::Empty)
                        })
                    }
                    ("Strikethrough", true) => {
                        let v = put_bool(a)?;
                        self.chart_edit(r, |_, ch| {
                            ch.bag.insert(key("Strike"), xlcalc::drawing::BagVal::B(v));
                            Ok(Variant::Empty)
                        })
                    }
                    _ => Err(unsupported("Font", m)),
                }
            }
            _ => self.serfmt_member(r, k, m, a),
        }
    }

    fn mark_scaled(&mut self, r: &DrawRef) -> Result<(), OmError> {
        self.chart_edit(r, |_, ch| {
            ch.bag.insert(SCALED.into(), BagVal::I(1));
            Ok(())
        })
    }

    fn axis_edit(
        &mut self,
        r: &DrawRef,
        i: usize,
        f: impl FnOnce(&mut ChartAxis),
    ) -> Result<Variant, OmError> {
        self.chart_edit(r, |_, ch| {
            f(&mut ch.axes[i]);
            Ok(Variant::Empty)
        })
    }

    fn secondary_cat_member(
        &mut self,
        r: &DrawRef,
        c: &Chart,
        m: &'static Member,
        put: bool,
    ) -> Result<Variant, OmError> {
        let mut sec = c.clone();
        sec.series.retain(|s| s.axis_group == 2);
        let d = if put { None } else { self.date_axis(r, &sec)? };
        let Some(d) = d.filter(|d| d.base.is_some()) else {
            return Err(e_fail());
        };
        Ok(match m.name {
            "MinimumScale" => Variant::R8(d.min),
            "MaximumScale" => Variant::R8(d.max),
            "MajorUnit" => Variant::R8(d.major),
            "MinorUnit" => Variant::R8(1.0),
            "MinimumScaleIsAuto" | "MaximumScaleIsAuto" | "MajorUnitIsAuto" | "MinorUnitIsAuto" => {
                Variant::Bool(true)
            }
            _ => return Err(e_fail()),
        })
    }

    fn axis_member(
        &mut self,
        r: &DrawRef,
        i: usize,
        c: &Chart,
        m: &'static Member,
        a: &Args,
        put: bool,
    ) -> Result<Variant, OmError> {
        if is_pie(c.chart_type) {
            if m.name == "AxisTitle" && !put {
                return Ok(Variant::Dispatch(None));
            }

            if matches!(
                m.name,
                "MajorGridlines"
                    | "MinorGridlines"
                    | "DisplayUnitLabel"
                    | "Format"
                    | "CategorySortOrder"
            ) {
                let key = if i == 0 { "Axis1" } else { "Axis2" };
                return self.chart_bag(r, key, key, m, a);
            }

            if put && matches!(m.name, "TickLabelSpacing" | "TickMarkSpacing") {
                let mut e = OmError::exception(
                    0x8000_4005_u32 as i32,
                    Some("This object is no longer valid."),
                );
                e.error_info = true;
                return Err(e);
            }

            let bad_put = put
                && match m.name {
                    "TickLabelPosition" => !matches!(put_num(a)? as i32, -4142 | -4134 | -4127 | 4),
                    "MajorTickMark" | "MinorTickMark" => {
                        !matches!(put_num(a)? as i32, -4142 | 2 | 3 | 4)
                    }
                    _ => false,
                };
            return Err(if bad_put || (m.name == "MajorTickMark" && !put) {
                e_fail()
            } else {
                OmError::exception(
                    0x8000_4003_u32 as i32,
                    Some("This object is no longer valid."),
                )
            });
        }
        let ax = &c.axes[i];
        let value_axis = i >= 1 || is_xy(axis_type(c));
        let scale = self.axis_scale(r, c, i)?;

        let scale_read = value_axis
            && ax.log
            && !put
            && matches!(
                m.name,
                "MinimumScale" | "MaximumScale" | "MajorUnit" | "MinorUnit"
            );
        let scaled = scale_read || matches!(c.bag.get(SCALED), Some(BagVal::I(1)));

        if value_axis
            && !put
            && matches!(
                m.name,
                "MinimumScale" | "MaximumScale" | "MajorUnit" | "MinorUnit"
            )
        {
            self.layout_title(r)?;
        }
        if scale_read && !matches!(c.bag.get(SCALED), Some(BagVal::I(1))) {
            self.mark_scaled(r)?;
        }

        let log_min = |v: f64| if ax.log && scaled && v <= 0.0 { 1.0 } else { v };

        let date_read = i == 0
            && !value_axis
            && !put
            && matches!(
                m.name,
                "MinimumScale"
                    | "MaximumScale"
                    | "MajorUnit"
                    | "MinorUnit"
                    | "MinimumScaleIsAuto"
                    | "MaximumScaleIsAuto"
                    | "MajorUnitIsAuto"
                    | "MinorUnitIsAuto"
                    | "MajorUnitScale"
                    | "MinorUnitScale"
                    | "BaseUnit"
                    | "CrossesAt"
            );
        if let Some(d) = date_read
            .then(|| self.date_axis(r, c))
            .transpose()?
            .flatten()
        {
            let v = match m.name {
                "MinimumScale" => Some(Variant::R8(d.min)),
                "MaximumScale" => Some(Variant::R8(d.max)),
                "MajorUnit" => Some(Variant::R8(d.major)),
                "MinorUnit" => Some(Variant::R8(ax.minor.unwrap_or(1.0))),
                "MinimumScaleIsAuto" => Some(Variant::Bool(ax.min.is_none())),
                "MaximumScaleIsAuto" => Some(Variant::Bool(ax.max.is_none())),
                "MajorUnitIsAuto" => Some(Variant::Bool(ax.major.is_none())),
                "MinorUnitIsAuto" => Some(Variant::Bool(ax.minor.is_none())),
                "MajorUnitScale" | "MinorUnitScale" => match d.scale {
                    Some(s) => Some(Variant::I4(s)),
                    None => return Err(e_fail()),
                },
                "BaseUnit" => d.base.map(Variant::I4),
                "CrossesAt" if !matches!(c.bag.get("Axis1.Crosses"), Some(BagVal::I(-4114))) => {
                    Some(Variant::R8(if Self::scatter_first(c) {
                        0.0
                    } else {
                        d.min
                    }))
                }
                _ => None,
            };
            if let Some(v) = v {
                return Ok(v);
            }
        } else if date_read
            && m.name == "BaseUnit"
            && !matches!(c.bag.get("Axis1.BaseUnitIsAuto"), Some(BagVal::B(false)))
        {
            if let Some(BagVal::I(b)) = c.bag.get(crate::chart_date::KEPT_BASE) {
                return Ok(Variant::I4(*b));
            }
        }
        match (m.name, put) {
            ("Type", false) => Ok(Variant::I4(if i == 2 { 2 } else { i as i32 + 1 })),
            ("AxisGroup", false) => Ok(Variant::I4(if i == 2 { 2 } else { 1 })),

            ("AxisTitle", false) if i == 0 && is_radar(c.chart_type) => Ok(Variant::Dispatch(None)),
            ("HasTitle", true) if i == 0 && is_radar(c.chart_type) && put_bool(a)? => Err(
                OmError::exception(0x8000_4005_u32 as i32, Some("This object has no title.")),
            ),
            ("HasTitle", false) => Ok(Variant::Bool(ax.has_title)),
            ("HasTitle", true) => {
                let v = put_bool(a)?;

                let flag = format!("Axis{}.#ht", i + 1);
                self.chart_edit(r, |_, ch| {
                    ch.bag.insert(flag, xlcalc::drawing::BagVal::I(1));
                    Ok(())
                })?;
                self.axis_edit(r, i, |x| {
                    if v && !x.has_title {
                        x.title = "Axis Title".into();
                        x.title_ref = None;
                    }
                    x.has_title = v;
                })
            }

            ("AxisTitle", false) => Ok(self.sub_obj(r, Sub::AxisTitle(i as u8))),
            ("CategoryNames", false) if i == 0 && !is_xy(axis_type(c)) => {
                let wb = &self.book(r.book)?.wb;
                Ok(c.series
                    .first()
                    .map_or(Variant::Empty, |s| crate::charts::series_x_values(wb, c, s)))
            }
            ("CategoryNames", true) if i == 0 && !is_xy(axis_type(c)) => {
                let part = match a.put.as_ref() {
                    Some(Variant::Bstr(t)) => {
                        let wb = &self.book(r.book)?.wb;
                        parse_part(wb, r.sheet, t.trim_start_matches('='))
                            .map_err(|_| OmError::exception(0x800A_03EC_u32 as i32, None))?
                    }
                    Some(Variant::R8(_) | Variant::I4(_)) => {
                        Part::Text(format!("{{{}}}", xlcalc::format_general(put_num(a)?)))
                    }
                    _ => return Err(OmError::exception(0x800A_03EC_u32 as i32, None)),
                };
                self.chart_edit(r, |_, ch| {
                    for s in &mut ch.series {
                        s.x = part.clone();
                    }
                    Ok(Variant::Empty)
                })
            }
            ("TickLabels", false) => Ok(self.sub_obj(r, Sub::TickLabels(i as u8))),
            ("HasMajorGridlines", false) => Ok(Variant::Bool(ax.grid_major)),
            ("HasMinorGridlines", false) => Ok(Variant::Bool(ax.grid_minor)),
            ("HasMajorGridlines", true) => {
                let v = put_bool(a)?;
                self.axis_edit(r, i, |x| x.grid_major = v)
            }
            ("HasMinorGridlines", true) => {
                let v = put_bool(a)?;
                self.axis_edit(r, i, |x| x.grid_minor = v)
            }
            ("ReversePlotOrder", false) => Ok(Variant::Bool(ax.reverse)),
            ("ReversePlotOrder", true) => {
                let v = put_bool(a)?;
                self.axis_edit(r, i, |x| x.reverse = v)
            }
            ("TickLabelPosition", false) => Ok(Variant::I4(ax.tick_pos)),
            ("MajorTickMark", false) => Ok(Variant::I4(ax.major_tick)),
            ("MinorTickMark", false) => Ok(Variant::I4(ax.minor_tick)),
            ("MajorTickMark" | "MinorTickMark" | "TickLabelPosition", true) => {
                let v = put_num(a)? as i32;
                let ok: &[i32] = if m.name == "TickLabelPosition" {
                    &[-4142, -4134, -4127, 4]
                } else {
                    &[-4142, 2, 3, 4]
                };
                if !ok.contains(&v) {
                    return Err(e_fail());
                }
                let which = m.name;
                self.axis_edit(r, i, |x| match which {
                    "MajorTickMark" => x.major_tick = v,
                    "MinorTickMark" => x.minor_tick = v,
                    _ => x.tick_pos = v,
                })
            }
            (
                "ScaleType" | "MinimumScale" | "MaximumScale" | "MajorUnit" | "MinorUnit"
                | "MinimumScaleIsAuto" | "MaximumScaleIsAuto" | "MajorUnitIsAuto"
                | "MinorUnitIsAuto",
                _,
            ) if !value_axis
                && !(m.name == "ScaleType" && !put && (is_radar(c.chart_type) || kept_cat(c))) =>
            {
                Err(e_fail())
            }

            ("CrossesAt", false)
                if value_axis
                    && ax.log
                    && !matches!(c.bag.get("Axis2.Crosses"), Some(BagVal::I(-4114))) =>
            {
                let lo = ax.min.map_or(scale.0, log_min);
                let hi = ax.max.unwrap_or(scale.1);
                let v = if scaled {
                    1.0f64.min(hi).max(lo)
                } else if let Some(v) = ax.min.filter(|v| *v > 1.0) {
                    v
                } else {
                    ax.max.filter(|v| *v < 1.0).unwrap_or(10.0)
                };

                self.mark_scaled(r)?;
                Ok(Variant::R8(v))
            }
            ("CrossesAt", true) if value_axis && ax.log && put_num(a).is_ok_and(|v| v <= 0.0) => {
                self.mark_scaled(r)?;
                Err(e_fail())
            }
            ("ScaleType", false) if value_axis => {
                Ok(Variant::I4(if ax.log { -4133 } else { -4132 }))
            }
            ("ScaleType", false) if is_radar(c.chart_type) || kept_cat(c) => Ok(Variant::I4(-4132)),
            ("ScaleType", true) if value_axis => {
                let log = match a.put.as_ref() {
                    Some(Variant::Null | Variant::Empty) => false,
                    _ => {
                        let v = put_num(a)?;
                        v != 0.0 && v != -4132.0
                    }
                };
                let crosses = match c.bag.get("Axis2.CrossesAt") {
                    Some(BagVal::D(d)) => *d,
                    _ => 0.0,
                };
                if log && crosses < 0.0 {
                    return Err(e_fail());
                }

                let snap = log
                    && crosses > 0.0
                    && matches!(c.bag.get("Axis2.Crosses"), Some(BagVal::I(-4114)));
                if snap {
                    self.chart_edit(r, |_, ch| {
                        ch.bag.insert("Axis2.CrossesAt".into(), BagVal::D(1.0));
                        Ok(())
                    })?;
                }
                self.chart_edit(r, |_, ch| {
                    ch.bag.remove(SCALED);
                    Ok(())
                })?;
                self.axis_edit(r, i, |x| {
                    x.log = log;
                    x.major = None;
                    x.minor = None;
                })
            }
            ("LogBase", false) if value_axis && !ax.log => Err(OmError::exception(
                0x800A_03EC_u32 as i32,
                Some("This scale has no log base."),
            )),
            ("LogBase", false) if value_axis && ax.log => Ok(Variant::R8(ax.log_base)),
            ("LogBase", true) if value_axis && !is_xy(c.chart_type) => {
                let v = put_num(a)?;

                if !(2.0..=1000.0).contains(&v) {
                    return Err(range_err("Parameter not valid"));
                }
                self.axis_edit(r, i, |x| {
                    x.log = true;
                    x.log_base = v;
                    x.major = None;
                    x.minor = None;
                })
            }

            ("MajorUnit" | "MinorUnit", true)
                if value_axis && {
                    let v = put_num(a)?;
                    v.is_nan() || v <= 0.0 || (ax.log && v < ax.log_base)
                } =>
            {
                Err(OmError::exception(
                    0x8000_4005_u32 as i32,
                    Some("Parameter not valid"),
                ))
            }
            ("MinimumScale", true) if value_axis && ax.log && put_num(a)? <= 0.0 => Err(range_err(
                "Excel can't plot negative or zero values on the logarithmic scale of a chart.",
            )),
            ("MinimumScale", false) if value_axis => {
                Ok(Variant::R8(ax.min.map_or(scale.0, log_min)))
            }
            ("MaximumScale", false) if value_axis => Ok(Variant::R8(ax.max.unwrap_or(scale.1))),
            ("MajorUnit", false) if value_axis => Ok(Variant::R8(ax.major.unwrap_or(scale.2))),
            ("MinorUnit", false) if value_axis => Ok(Variant::R8(
                ax.minor.unwrap_or(
                    ax.major
                        .map_or(scale.3, |m| if ax.log { m } else { m / 5.0 }),
                ),
            )),
            ("MinorUnitIsAuto", false) if value_axis => Ok(Variant::Bool(ax.minor.is_none())),
            ("MinorUnit", true) if value_axis => {
                let v = put_num(a)?;
                self.axis_edit(r, i, |x| x.minor = Some(v))
            }
            ("MinimumScaleIsAuto", false) if value_axis => Ok(Variant::Bool(ax.min.is_none())),
            ("MaximumScaleIsAuto", false) if value_axis => Ok(Variant::Bool(ax.max.is_none())),
            ("MajorUnitIsAuto", false) if value_axis => Ok(Variant::Bool(ax.major.is_none())),
            ("MinimumScale", true) if value_axis => {
                let v = put_num(a)?;
                self.axis_edit(r, i, |x| x.min = Some(v))
            }
            ("MaximumScale", true) if value_axis => {
                let v = put_num(a)?;
                self.axis_edit(r, i, |x| x.max = Some(v))
            }
            ("MajorUnit", true) if value_axis => {
                let v = put_num(a)?;
                self.axis_edit(r, i, |x| x.major = Some(v))
            }
            ("MinorUnitIsAuto", true) if value_axis => {
                let v = put_bool(a)?;
                let cur = ax
                    .major
                    .map_or(scale.3, |m| if ax.log { m } else { m / 5.0 });
                self.axis_edit(r, i, |x| x.minor = if v { None } else { Some(cur) })
            }
            ("MinimumScaleIsAuto" | "MaximumScaleIsAuto" | "MajorUnitIsAuto", true)
                if value_axis =>
            {
                let v = put_bool(a)?;
                let which = m.name;

                let key = format!("Axis{}.#last{which}", i + 1);
                let last = match c.bag.get(&key) {
                    Some(xlcalc::drawing::BagVal::D(x)) => Some(*x),
                    _ => None,
                };
                let held = match which {
                    "MinimumScaleIsAuto" => ax.min,
                    "MaximumScaleIsAuto" => ax.max,
                    _ => ax.major,
                };
                self.chart_edit(r, |_, ch| {
                    if let (true, Some(x)) = (v, held) {
                        ch.bag.insert(key, xlcalc::drawing::BagVal::D(x));
                    }
                    let x = &mut ch.axes[i];
                    let (slot, cur) = match which {
                        "MinimumScaleIsAuto" => (&mut x.min, scale.0),
                        "MaximumScaleIsAuto" => (&mut x.max, scale.1),
                        _ => (&mut x.major, scale.2),
                    };
                    *slot = if v { None } else { Some(last.unwrap_or(cur)) };
                    Ok(Variant::Empty)
                })
            }
            ("Crosses", true) => match put_num(a)? as i32 {
                v @ (2 | 4 | -4105) => {
                    let key = format!("{}.Crosses", axis_slot(i));
                    self.chart_edit(r, |_, ch| {
                        if v == -4105 {
                            ch.bag.remove(&key);
                        } else {
                            ch.bag.insert(key, BagVal::I(v));
                        }
                        Ok(Variant::Empty)
                    })
                }
                -4114 => Ok(Variant::Empty),
                _ => Err(e_fail()),
            },

            ("CrossesAt", false) if i == 0 && !value_axis && !is_radar(c.chart_type) => {
                match c.bag.get("Axis1.Crosses") {
                    Some(BagVal::I(2)) => Ok(Variant::R8(category_count(self, r, c)? as f64)),
                    Some(BagVal::I(4)) => Ok(Variant::R8(1.0)),
                    None if !c.bag.contains_key(crate::chart_geom::CAT_CROSS) => {
                        Ok(Variant::R8(0.0))
                    }
                    _ => self.chart_bag(r, "Axis1", "Axis1", m, a),
                }
            }

            ("CrossesAt", false)
                if i >= 1
                    && !ax.log
                    && !c.bag.contains_key(&format!("{}.Crosses", axis_slot(i)))
                    && !is_radar(c.chart_type) =>
            {
                let lo = ax.min.unwrap_or(scale.0);
                let hi = ax.max.unwrap_or(scale.1);
                Ok(Variant::R8(
                    if i == 2 && !c.bag.contains_key(xlcalc::drawing::SECONDARY_ZERO) {
                        lo
                    } else {
                        0.0f64.min(hi).max(lo)
                    },
                ))
            }

            ("CrossesAt", true) if i == 0 && !value_axis && !is_radar(c.chart_type) => {
                let v = put_num(a)?;
                let time = bag_is_time(c) || self.date_axis(r, c)?.is_some();
                let mut cut = a.clone();
                if v >= 1.0 && !time {
                    cut.put = Some(Variant::R8(v.trunc()));
                }
                self.chart_bag(r, "Axis1", axis_slot(i), m, &cut)
            }
            _ => {
                let key = if i == 0 { "Axis1" } else { "Axis2" };
                self.chart_bag(r, key, axis_slot(i), m, a)
            }
        }
    }
}

fn category_count(app: &Application<'_>, r: &DrawRef, c: &Chart) -> Result<usize, OmError> {
    let wb = &app.book(r.book)?.wb;
    Ok(c.series
        .iter()
        .map(|s| crate::charts::part_values(wb, &s.values).len())
        .max()
        .unwrap_or(0))
}

fn axis_slot(i: usize) -> &'static str {
    match i {
        0 => "Axis1",
        1 => "Axis2",
        _ => "Axis3",
    }
}

fn series_name_level(c: &Chart) -> i32 {
    if !c.series.is_empty() && c.series.iter().all(|s| matches!(s.name, Part::None)) {
        -3
    } else {
        0
    }
}

pub(crate) fn chart_group_count(c: &Chart) -> i32 {
    let mut seen: Vec<(i32, i32)> = Vec::new();
    for s in &c.series {
        let kind = (s.axis_group, group_family(s.chart_type));
        if !seen.contains(&kind) {
            seen.push(kind);
        }
    }
    seen.len() as i32
}

pub(crate) fn group_family(t: i32) -> i32 {
    match t {
        51..=54 | 98..=100 | 102..=104 => 51,
        57..=62 => 57,
        4 | 63..=67 => 4,
        -4169 | 72..=75 => -4169,
        1 | 76..=79 => 1,
        5 | 68..=71 | 80 | -4102 | -4120 => 5,
        other => other,
    }
}

pub(crate) fn stack_kind(t: i32) -> u8 {
    match t {
        63 | 66 | 76 | 78 => 1,
        64 | 67 | 77 | 79 => 3,
        52 | 55 | 58 | 61 => 2,
        53 | 56 | 59 | 62 => 4,
        _ => 0,
    }
}

pub(crate) fn is_radar(t: i32) -> bool {
    matches!(t, -4151 | 81 | 82)
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn single_glyphs_match_legend_probes() {
        for (s, emu) in [
            (" ", 23_749),
            ("0", 64_389),
            ("G", 82_042),
            ("w", 83_376),
            ("e", 61_659),
            ("[", 34_354),
            ("%", 96_584),
            ("'", 24_448),
        ] {
            assert_eq!(legend_text_emu(s), emu, "{s:?}");
        }
        assert_eq!(digit_emu(), 64_389);
    }

    #[test]
    fn glyphs_round_each() {
        assert_eq!(legend_text_emu(""), 0);
        assert_eq!(legend_text_emu("ee"), 123_318);
        assert_eq!(legend_text_emu("eee"), 184_977);
        assert_eq!(legend_text_emu("4500"), 257_556);

        assert_eq!(legend_text_emu("\u{e9}"), 61_659);

        assert_eq!(legend_text_emu("\u{4e2d}"), 127_000);
    }
}
