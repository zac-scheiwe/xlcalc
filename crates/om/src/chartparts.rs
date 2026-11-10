use xlcalc::drawing::{Chart, ChartAxis, Part};
use xlcalc_variant::Variant;

use crate::app::{unsupported, Application};
use crate::charts::{parse_part, part_text, part_values, series_from_block};
use crate::conv::Args;
use crate::error::OmError;
use crate::shapes::{DrawKind, DrawRef};
use crate::typelib::{InvokeKind, Member};

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub(crate) enum Sub {
    Title,
    TitleChars,
    Legend,
    Entries,
    Axis(u8),
    AxisTitle(u8),
    TickLabels(u8),
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
}

impl Sub {
    pub(crate) fn interface_name(self) -> &'static str {
        match self {
            Sub::Title => "ChartTitle",
            Sub::TitleChars => "Characters",
            Sub::Legend => "Legend",
            Sub::Entries => "LegendEntries",
            Sub::Axis(_) => "Axis",
            Sub::AxisTitle(_) => "AxisTitle",
            Sub::TickLabels(_) => "TickLabels",
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

fn e_fail() -> OmError {
    OmError::exception(0x8000_4005_u32 as i32, None)
}

pub(crate) fn range_err(desc: &str) -> OmError {
    OmError::exception(0x800A_03EC_u32 as i32, Some(desc))
}

fn valid_type(t: i32) -> bool {
    matches!(
        t,
        1 | 4 | 5 | 15 | 51..=115 | -4098 | -4102..=-4100 | -4120 | -4151 | -4169
    )
}

fn auto_title(wb: &xlcalc::Workbook, ch: &Chart) -> String {
    let one = if is_pie(ch.chart_type) || ch.series.len() == 1 {
        ch.series.first()
    } else {
        None
    };
    one.and_then(|s| match crate::charts::part_values(wb, &s.name).first() {
        Some(Variant::Bstr(t)) if !t.is_empty() => Some(t.clone()),
        Some(Variant::R8(n)) => Some(xlcalc::format_general(*n)),
        _ => None,
    })
    .unwrap_or_else(|| "Chart Title".into())
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

fn put_text(a: &Args) -> Result<String, OmError> {
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

fn auto_scale(
    vals: &[f64],
    intervals: &dyn Fn(f64) -> f64,
    extra: f64,
    pad: bool,
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
    let (bottom, mut top) = (lo, hi);
    if top <= bottom {
        top = bottom + bottom.abs().max(1.0);
    }
    let span = top - bottom;
    let tol = |x: f64| 1e-9 * x.abs();

    let grid = |step: f64| {
        let min = fmin.unwrap_or_else(|| {
            let m = (bottom / step).round() * step;
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

const LEGEND_CHAR_EMU: [i64; 95] = [
    23749, 41339, 43117, 63246, 64389, 96584, 74676, 22003, 38608, 38608, 63246, 64389, 33084,
    38862, 33084, 39751, 64389, 64389, 64389, 64389, 64389, 64389, 64389, 64389, 64389, 64389,
    33084, 33084, 64389, 64389, 64389, 57785, 102362, 67818, 70168, 80391, 79629, 64707, 60960,
    82042, 82042, 30861, 38735, 66294, 57976, 91694, 81852, 84773, 67183, 84773, 70485, 65913,
    55372, 78804, 67437, 103251, 64199, 62548, 59690, 34354, 39751, 34354, 64389, 52832, 63373,
    61913, 65278, 60897, 65278, 61659, 35370, 57087, 64389, 28448, 28448, 56896, 30798, 99568,
    64389, 64135, 65278, 65278, 39243, 56261, 37656, 64834, 52578, 83376, 51562, 52578, 50737,
    34354, 58674, 34354, 64389,
];

fn legend_text_emu(s: &str) -> i64 {
    s.chars()
        .map(|ch| match ch as u32 {
            32..=126 => LEGEND_CHAR_EMU[ch as usize - 32],
            _ => 64389,
        })
        .sum()
}

struct AxisGeometry {
    len: i64,
    horizontal: bool,
    percent: bool,
    radar: bool,
}

impl AxisGeometry {
    fn intervals(&self, top: f64) -> f64 {
        let n = if self.horizontal {
            let digits = ((top.abs().log10() + 1.0) as i64).max(6);
            let dw = 64389;
            self.len / ((dw + (dw + 3) / 4) * digits)
        } else if self.radar {
            self.len / 2 / 155_004 - 2
        } else {
            self.len / 155_004 - 2
        };
        let n = if self.percent { n.max(4) } else { n };
        n.clamp(1, 10) as f64
    }
}

pub(crate) fn is_xy(t: i32) -> bool {
    matches!(t, -4169 | 72..=75 | 15 | 87 | 88)
}

fn pow10(e: i32) -> f64 {
    format!("1e{e}").parse().unwrap_or(1.0)
}
impl Application<'_> {
    fn axis_geometry(&self, r: &DrawRef, c: &Chart, i: usize) -> Result<AxisGeometry, OmError> {
        let sh = self.shape_clone(r)?;
        let (_, _, w, h) = self.book(r.book)?.wb.shape_rect_emu(r.sheet, &sh.pos);
        let bar = matches!(c.chart_type, 57..=62);
        let horizontal = if is_xy(c.chart_type) { i == 0 } else { bar };
        let wb = &self.book(r.book)?.wb;
        let side = c.has_legend && matches!(c.legend_pos, -4152 | -4131 | 2);
        let level = c.has_legend && matches!(c.legend_pos, -4160 | -4107);
        let len = if horizontal {
            let names = c
                .series
                .iter()
                .enumerate()
                .map(|(k, s)| match &s.name {
                    Part::None => legend_text_emu(&format!("Series{}", k + 1)),
                    Part::Text(t) => legend_text_emu(t),
                    p => part_values(wb, p).first().map_or(0, |v| match v {
                        Variant::Bstr(s) => legend_text_emu(s),
                        Variant::R8(n) => legend_text_emu(&xlcalc::format_general(*n)),
                        _ => 0,
                    }),
                })
                .max()
                .unwrap_or(0);
            let legend = if side { 291_903 + names } else { 0 };
            let beside = if is_xy(c.chart_type) {
                c.axes[1].has_title
            } else {
                c.axes[0].has_title
            };
            w - 279_400 - legend - if beside { 193_104 } else { 0 }
        } else {
            h - 279_400
                - if c.has_title { 393_319 } else { 0 }
                - if level { 305_853 } else { 0 }
                - if c.axes[0].has_title { 193_104 } else { 0 }
        };
        Ok(AxisGeometry {
            len,
            horizontal,
            percent: stack_kind(c.chart_type) >= 3,
            radar: is_radar(c.chart_type),
        })
    }
    fn chart_values(&self, r: &DrawRef, c: &Chart, x: bool) -> Result<Vec<f64>, OmError> {
        let wb = &self.book(r.book)?.wb;
        let mut out = Vec::new();
        let stacked = if x { 0 } else { stack_kind(c.chart_type) };
        if stacked != 0 {
            let cols: Vec<Vec<f64>> = c
                .series
                .iter()
                .map(|s| {
                    part_values(wb, &s.values)
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
        for s in &c.series {
            let vs = part_values(wb, if x { &s.x } else { &s.values });

            if x && vs.iter().any(|v| matches!(v, Variant::Bstr(_))) {
                out.extend((1..=vs.len()).map(|k| k as f64));
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
            ("ChartTitle", false) => Ok(self.sub_obj(r, Sub::Title)),
            ("Legend", false) => {
                if c.has_legend {
                    Ok(self.sub_obj(r, Sub::Legend))
                } else {
                    Err(range_err("This object doesn't have a legend."))
                }
            }
            ("ChartArea", false) => Ok(self.sub_obj(r, Sub::ChartArea)),
            ("PlotArea", false) => Ok(self.sub_obj(r, Sub::PlotArea)),
            ("Axes", false) => {
                let t = match a.i32(0) {
                    Ok(Some(t)) => t,
                    _ => return Some(Err(OmError::app_defined())),
                };
                if t == 1 || t == 2 {
                    Ok(self.sub_obj(r, Sub::Axis(t as u8 - 1)))
                } else {
                    Err(OmError::exception(0x8007_0057_u32 as i32, None))
                }
            }
            ("HasAxis", false) => {
                let t = match a.i32(0) {
                    Ok(Some(t)) => t,
                    _ => return Some(Err(e_fail())),
                };
                let g = a.i32(1).ok().flatten().unwrap_or(1);
                Ok(Variant::Bool(
                    (t == 1 || t == 2)
                        && g == 1
                        && !is_pie(c.chart_type)
                        && !c.axis_hidden[(t - 1) as usize],
                ))
            }
            ("HasAxis", true) => Err(e_fail()),
            ("ChartType", true) => {
                let t = match put_num(a) {
                    Ok(t) => t as i32,
                    Err(e) => return Some(Err(e)),
                };
                if !valid_type(t) {
                    return Some(Err(e_fail()));
                }
                return None;
            }
            ("PlotBy", true) => {
                let by = match put_num(a) {
                    Ok(t) => t as i32,
                    Err(e) => return Some(Err(e)),
                };
                if by != 1 && by != 2 {
                    return Some(Err(e_fail()));
                }
                self.chart_edit(r, |wb, ch| {
                    if ch.plot_by == by {
                    } else if let Some((s, rect)) = ch.source {
                        let (by, series) =
                            series_from_block(wb, s, rect, Some(by), ch.chart_type, &[]);
                        ch.series = series;
                        ch.plot_by = by;
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
                    if v && !ch.has_title {
                        ch.title = auto_title(wb, ch);
                        ch.title_ref = None;
                    }
                    ch.has_title = v;
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
                        0 => ch.has_title = false,
                        1 | 2 if !ch.has_title => {
                            ch.has_title = true;
                            ch.title = auto_title(wb, ch);
                            ch.title_ref = None;
                        }
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
            ("ApplyLayout", _) => self.chart_edit(r, |wb, ch| {
                if !ch.has_title {
                    ch.has_title = true;
                    ch.title = auto_title(wb, ch);
                    ch.title_ref = None;
                }
                Ok(Variant::Empty)
            }),
            ("Location", _) => {
                let place = a.i32(0).ok().flatten().unwrap_or(0);
                let name = match a.get(1) {
                    Some(Variant::Bstr(t)) => Some(t.clone()),
                    _ => None,
                };
                self.chart_location(r, place, name)
            }
            ("Delete", _) => Err(range_err("Delete method of Chart class failed")),
            ("Name", false) => {
                let wb = &self.book(r.book).ok()?.wb;
                let sheet = wb
                    .sheet(r.sheet)
                    .map(|s| s.name.to_string())
                    .unwrap_or_default();
                let co = self.shape_clone(r).ok()?.name;
                Ok(Variant::Bstr(format!("{sheet} {co}")))
            }
            ("ChartStyle" | "ChartColor", false) => Ok(Variant::I4(2)),
            ("DisplayBlanksAs", false) => Ok(Variant::I4(c.blanks)),
            ("PlotVisibleOnly", false) => Ok(Variant::Bool(c.visible_only)),
            ("DisplayBlanksAs", true) => {
                let v = match put_num(a) {
                    Ok(v) => v as i32,
                    Err(e) => return Some(Err(e)),
                };
                if !(1..=3).contains(&v) {
                    return Some(Err(e_fail()));
                }
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
            ("HasDataTable", false) => Ok(Variant::Bool(false)),
            ("BarShape", false) => Ok(Variant::I4(0)),
            ("Rotation", false) => Ok(Variant::I4(20)),
            ("Elevation", false) => Ok(Variant::I4(15)),
            _ => return None,
        })
    }

    fn title_text(&self, r: &DrawRef, c: &Chart) -> Result<String, OmError> {
        if let Some(p) = &c.title_ref {
            let wb = &self.book(r.book)?.wb;
            return Ok(match part_values(wb, p).first() {
                Some(Variant::Bstr(s)) => s.clone(),
                Some(Variant::R8(n)) => xlcalc::format_general(*n),
                _ => String::new(),
            });
        }
        Ok(c.title.clone())
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
                    ("Text" | "Caption", true) if k == Sub::Title => {
                        let t = put_text(a)?;
                        self.chart_edit(r, |_, ch| {
                            ch.title = t;
                            ch.title_ref = None;
                            Ok(Variant::Empty)
                        })
                    }
                    ("Formula", false) if k == Sub::Title => match &c.title_ref {
                        Some(p) => {
                            let wb = &self.book(r.book)?.wb;
                            Ok(Variant::Bstr(format!("={}", part_text(wb, p, false))))
                        }
                        None => Ok(Variant::Bstr(c.title.clone())),
                    },
                    ("Formula", true) if k == Sub::Title => {
                        let f = put_text(a)?;
                        let sheet = r.sheet;
                        self.chart_edit(r, |wb, ch| {
                            match f.strip_prefix('=') {
                                Some(rest) => match parse_part(wb, sheet, rest)? {
                                    Part::Refs(v) => ch.title_ref = Some(Part::Refs(v)),
                                    Part::Text(t) => {
                                        ch.title = t;
                                        ch.title_ref = None;
                                    }
                                    _ => return Err(OmError::app_defined()),
                                },
                                None => {
                                    ch.title = f;
                                    ch.title_ref = None;
                                }
                            }
                            Ok(Variant::Empty)
                        })
                    }
                    _ => Err(unsupported(k.interface_name(), m)),
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
                ("IncludeInLayout", false) => Ok(Variant::Bool(true)),
                ("LegendEntries", _) => Ok(self.sub_obj(r, Sub::Entries)),
                _ => Err(unsupported("Legend", m)),
            },
            Sub::Entries => match m.name {
                "Count" => Ok(Variant::I4(c.series.len() as i32)),
                _ => Err(unsupported("LegendEntries", m)),
            },
            Sub::ChartArea => match m.name {
                "Name" => Ok(Variant::Bstr("Chart".into())),
                _ => Err(unsupported("ChartArea", m)),
            },
            Sub::PlotArea => match m.name {
                "Name" => Ok(Variant::Bstr("Plot".into())),
                _ => Err(unsupported("PlotArea", m)),
            },
            Sub::Axis(i) => self.axis_member(r, i as usize, &c, m, a, put),
            Sub::AxisTitle(i) => {
                let ax = &c.axes[i as usize];
                match (m.name, put) {
                    ("Text" | "Caption", false) => Ok(Variant::Bstr(ax.title.clone())),
                    ("Text" | "Caption", true) => {
                        let t = put_text(a)?;
                        self.axis_edit(r, i as usize, |x| x.title = t)
                    }
                    ("Orientation", false) => Ok(Variant::I4(if i == 0 { -4128 } else { -4171 })),
                    _ => Err(unsupported("AxisTitle", m)),
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
                    _ => Err(unsupported("TickLabels", m)),
                }
            }
            _ => self.serfmt_member(r, k, m, a),
        }
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
        let value_axis = i == 1 || is_xy(c.chart_type);
        let geom = self.axis_geometry(r, c, i)?;
        let mut scale = auto_scale(
            &self.chart_values(r, c, i == 0)?,
            &|top| geom.intervals(top),
            if geom.horizontal { 0.0 } else { 1.0 },
            stack_kind(c.chart_type) <= 2,
            (ax.min, ax.max, ax.major),
        );
        if ax.log {
            let vals: Vec<f64> = self
                .chart_values(r, c, i == 0)?
                .into_iter()
                .filter(|v| *v > 0.0)
                .collect();

            let lo = if stack_kind(c.chart_type) == 2 {
                1.0
            } else {
                vals.iter().copied().fold(f64::MAX, f64::min)
            };
            let hi = vals.iter().copied().fold(f64::MIN, f64::max);
            if !vals.is_empty() {
                scale = (
                    pow10(lo.log10().floor() as i32),
                    pow10(hi.log10().ceil() as i32),
                    10.0,
                    10.0,
                );
            }
        }
        if i == 1 && stack_kind(c.chart_type) >= 3 {
            scale.0 = scale.0.max(-1.0);
            scale.1 = scale.1.min(1.0);
        }
        match (m.name, put) {
            ("Type", false) => Ok(Variant::I4(i as i32 + 1)),
            ("AxisGroup", false) => Ok(Variant::I4(1)),

            ("AxisTitle", false) if i == 0 && is_radar(c.chart_type) => Ok(Variant::Dispatch(None)),
            ("HasTitle", true) if i == 0 && is_radar(c.chart_type) && put_bool(a)? => Err(
                OmError::exception(0x8000_4005_u32 as i32, Some("This object has no title.")),
            ),
            ("HasTitle", false) => Ok(Variant::Bool(ax.has_title)),
            ("HasTitle", true) => {
                let v = put_bool(a)?;
                self.axis_edit(r, i, |x| {
                    if v && !x.has_title {
                        x.title = "Axis Title".into();
                    }
                    x.has_title = v;
                })
            }
            ("AxisTitle", false) => {
                if ax.has_title {
                    Ok(self.sub_obj(r, Sub::AxisTitle(i as u8)))
                } else {
                    Err(OmError::exception(0x800A_03EC_u32 as i32, None))
                }
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
            ("Crosses" | "CategoryType", false) => Ok(Variant::I4(-4105)),
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
            ) if !value_axis && !(m.name == "ScaleType" && !put && is_radar(c.chart_type)) => {
                Err(e_fail())
            }
            ("TickLabelSpacing", false) if !value_axis => Ok(Variant::I4(1)),
            ("AxisBetweenCategories", false) if !value_axis => Ok(Variant::Bool(true)),
            ("ScaleType", false) if value_axis => {
                Ok(Variant::I4(if ax.log { -4133 } else { -4132 }))
            }
            ("ScaleType", false) if is_radar(c.chart_type) => Ok(Variant::I4(-4132)),
            ("ScaleType", true) if value_axis => {
                let v = put_num(a)? as i32;
                if v != -4132 && v != -4133 {
                    return Err(e_fail());
                }
                self.axis_edit(r, i, |x| x.log = v == -4133)
            }
            ("MinimumScale", false) if value_axis => Ok(Variant::R8(ax.min.unwrap_or(scale.0))),
            ("MaximumScale", false) if value_axis => Ok(Variant::R8(ax.max.unwrap_or(scale.1))),
            ("MajorUnit", false) if value_axis => Ok(Variant::R8(ax.major.unwrap_or(scale.2))),
            ("MinorUnit", false) if value_axis => Ok(Variant::R8(
                ax.minor.unwrap_or(ax.major.map_or(scale.3, |m| m / 5.0)),
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
                let cur = ax.major.map_or(scale.3, |m| m / 5.0);
                self.axis_edit(r, i, |x| x.minor = if v { None } else { Some(cur) })
            }
            ("MinimumScaleIsAuto" | "MaximumScaleIsAuto" | "MajorUnitIsAuto", true)
                if value_axis =>
            {
                let v = put_bool(a)?;
                let which = m.name;
                self.axis_edit(r, i, |x| {
                    let (slot, cur) = match which {
                        "MinimumScaleIsAuto" => (&mut x.min, scale.0),
                        "MaximumScaleIsAuto" => (&mut x.max, scale.1),
                        _ => (&mut x.major, scale.2),
                    };
                    *slot = if v { None } else { Some(cur) };
                })
            }
            ("Crosses", true) => match put_num(a)? as i32 {
                -4105 | -4114 | 2 | 4 => Ok(Variant::Empty),
                _ => Err(e_fail()),
            },
            _ => Err(unsupported("Axis", m)),
        }
    }
}

fn stack_kind(t: i32) -> u8 {
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
