use xlcalc::drawing::{BagVal, Chart, Part};
use xlcalc_variant::Variant;

use crate::app::Application;
use crate::chart_layout::{
    auto_layout, cat_overhang, cat_text_h, half_tick, inside_out_room, label_extent,
    legend_wrap_room, outer_axis_layout, reserves, side_label_step, table_head_at, table_head_pad,
    table_label_lines, LayoutIn, LegendAt, LegendIn, TITLE_EXTRA,
};
use crate::chart_lobj::OUTSIDE;
use crate::chartparts::{
    is_3d_column, is_xy, legend_font_emu, legend_text_emu, series_name_emu, series_name_text,
    val_text_emu, wrap_legend_text, SCALE_LEN,
};
use crate::charts::part_values;
use crate::conv::Args;
use crate::error::OmError;
use crate::shapes::DrawRef;
use crate::typelib::{InvokeKind, Member};

const EMU: f64 = 12_700.0;

fn value_cross_pos(c: &Chart, dates: Option<(i64, i32, i64)>, cats: i64) -> Option<f64> {
    let cross = match (c.bag.get("Axis1.Crosses"), c.bag.get("Axis1.CrossesAt")) {
        (Some(BagVal::I(-4114)), Some(BagVal::D(v))) => *v,
        (None, _) if Application::scatter_first(c) => 0.0,
        _ => return None,
    };
    let between = crate::charts::between_categories(c);
    let x = if between { cross - 0.5 } else { cross };
    let (v, n) = match dates {
        Some((start, unit, bins)) => (crate::chart_date::bin_number(start, unit, x) as f64, bins),
        None => (x, cats),
    };
    let (lo, hi) = if between {
        (0.5, n as f64 + 0.5)
    } else {
        (1.0, n as f64)
    };
    Some((v - lo) / (hi - lo))
}
const MANUAL: &str = "PlotArea.#ml";
const LAID: &str = "#laid";
pub(crate) use xlcalc::drawing::CAT_CROSS;

const HGT_PUT: &str = "PlotArea.#hput";

const HGT_REQ: &str = "PlotArea.#hreq";

const OUTER: &str = "PlotArea.#outer";

const RERUN: &str = "PlotArea.#rerun";

const MIN_INNER: i64 = 10;

const LAID_SIZE: &str = "#lsz";

const PRINT_EARLY: &str = "#poe";

const PRINT_EARLY_WIDTH: i64 = 2_705;

const PASS2_SHRINK: i64 = 200_692;

const DEPTH_LABEL_PAD: i64 = 20_352;

fn side_axis_pad(font: crate::chartfont::TextFont) -> i64 {
    crate::chartfont::line_emu(font) / 2
}

const SIDE_AXIS_H: i64 = 174_054;

fn layout_family(t: i32) -> i32 {
    match t {
        52 | 53 => 51,
        58..=62 => 57,
        63 | 64 => 4,
        66 | 67 => 65,
        76 | 77 => 1,
        72..=75 => -4169,

        54..=56 => -4100,
        _ => t,
    }
}

fn no_less_than_min(mut plot: [i64; 4], mut inside: [i64; 4]) -> ([i64; 4], [i64; 4]) {
    for k in 2..4 {
        if plot[k] < MIN_INNER {
            plot[k] = MIN_INNER;
            inside[k] = MIN_INNER;
        }
    }
    (plot, inside)
}

fn rect_of(c: &Chart, key: &str) -> Option<[i64; 4]> {
    match c.bag.get(key) {
        Some(BagVal::S(s)) => {
            let v: Vec<i64> = s.split(',').filter_map(|x| x.parse().ok()).collect();
            (v.len() == 4).then(|| [v[0], v[1], v[2], v[3]])
        }
        _ => None,
    }
}

fn texts(wb: &xlcalc::Workbook, p: &Part) -> Vec<String> {
    part_values(wb, p)
        .into_iter()
        .map(|v| match v {
            Variant::Bstr(s) => s,
            Variant::R8(n) => xlcalc::format_general(n),
            _ => String::new(),
        })
        .collect()
}

pub(crate) struct Geom {
    pub plot: [i64; 4],
    pub inside: [i64; 4],
    pub legend: Option<[i64; 4]>,

    pub res: [i64; 4],

    pub table: bool,
    pub title: bool,

    pub early: bool,
}

impl Application<'_> {
    fn layout_input(&self, r: &DrawRef, c: &Chart) -> Result<Option<LayoutIn>, OmError> {
        self.layout_input_with(r, c, true)
    }

    fn layout_input_with(
        &self,
        r: &DrawRef,
        c: &Chart,
        dated: bool,
    ) -> Result<Option<LayoutIn>, OmError> {
        self.layout_input_core(r, c, dated, true)
    }

    pub(crate) fn radar_plot_size(
        &self,
        r: &DrawRef,
        c: &Chart,
    ) -> Result<Option<(i64, i64)>, OmError> {
        let _fonts = crate::chartfont::font_scope(crate::chartparts::chart_fonts(c));
        Ok(self
            .layout_input_core(r, c, false, false)?
            .filter(|li| li.radar.is_some())
            .map(|li| {
                let s = crate::chart_layout::plot_frame(&li).0;
                (s[2] + 2 * TITLE_EXTRA, s[3] + 2 * TITLE_EXTRA)
            }))
    }

    pub(crate) fn outer_plot(&self, r: &DrawRef, c: &Chart) -> Result<Option<[i64; 4]>, OmError> {
        let _fonts = crate::chartfont::font_scope(crate::chartparts::chart_fonts(c));
        if let Some(o) = self.target_rect(r, c, OUTER)? {
            return Ok(Some(o));
        }
        Ok(self
            .layout_input_core(r, c, false, false)?
            .map(|li| auto_layout(&li).plot))
    }

    fn layout_input_core(
        &self,
        r: &DrawRef,
        c: &Chart,
        dated: bool,
        scaled: bool,
    ) -> Result<Option<LayoutIn>, OmError> {
        let _fonts = crate::chartfont::font_scope(crate::chartparts::chart_fonts(c));

        let t = layout_family(crate::chartparts::axis_type(c));
        let percent = matches!(c.chart_type, 53 | 59 | 64 | 67 | 77 | 56);
        if !(matches!(t, 51 | 57 | 4 | 65 | 1 | -4169 | 5 | 15 | -4100)
            || crate::chartparts::is_radar(t))
            || c.series.is_empty()
        {
            return Ok(None);
        }

        if c.axes.iter().any(|a| a.tick_nf.is_some()) && !crate::chartparts::is_radar(t) {
            return Ok(None);
        }
        let sh = self.shape_clone(r)?;
        let wb = &self.book(r.book)?.wb;
        let (_, _, w, h) = wb.shape_rect_emu(r.sheet, &sh.pos);
        let pie = t == 5;

        let date = if dated && !pie {
            self.date_axis(r, c)?
        } else {
            None
        };
        let date_bins = date
            .as_ref()
            .and_then(|d| d.base.map(|u| (d.start, u, d.bins)));
        let (date_labels, cat_bins) = match date {
            Some(d) if d.base.is_some() => {
                let k = if d.major >= 1.0 { d.major as usize } else { 1 };
                (Some(d.labels.into_iter().step_by(k).collect()), d.bins)
            }
            _ => (None, 0),
        };
        let cat_pad = date_labels.is_some();
        let cats = if let Some(l) = date_labels {
            l
        } else {
            let s = &c.series[0];
            let v = texts(wb, &s.x);
            if v.is_empty() {
                let n = part_values(wb, &s.values).len();
                (1..=n).map(|i| i.to_string()).collect()
            } else {
                v
            }
        };
        let legend = if c.has_legend {
            let at = match c.legend_pos {
                -4152 => LegendAt::Right,
                -4131 => LegendAt::Left,
                -4160 => LegendAt::Top,
                -4107 => LegendAt::Bottom,
                _ => return Ok(None),
            };

            let key = match c.chart_type {
                4 | 63..=67 | 72..=75 | -4151 | 81 => 304_800,
                _ => crate::chartfont::legend_key_emu(crate::chartfont::fonts().legend),
            };
            let (texts, lines): (Vec<i64>, Vec<i64>) = if pie {
                cats.iter().map(|s| (legend_font_emu(s), 1)).unzip()
            } else if matches!(at, LegendAt::Right | LegendAt::Left) {
                let avail = legend_wrap_room(w, key);
                c.series
                    .iter()
                    .enumerate()
                    .map(|(k, s)| wrap_legend_text(&series_name_text(wb, k, s), avail))
                    .unzip()
            } else {
                c.series
                    .iter()
                    .enumerate()
                    .map(|(k, s)| (series_name_emu(wb, k, s), 1))
                    .unzip()
            };
            Some(LegendIn {
                at,
                texts,
                lines,
                key,
            })
        } else {
            None
        };
        let title = c.has_title
            || ((pie || c.series.len() == 1)
                && !matches!(c.bag.get("#title_off"), Some(BagVal::I(1))));
        let mut li = LayoutIn {
            w,
            h,
            pie,
            radar: None,
            bar: t == 57,
            right_overhang: 0,
            left_overhang: 0,
            between: crate::charts::between_categories(c),
            legend,
            title,
            side_title: false,
            foot_title: false,
            val_labels: Vec::new(),
            sec_labels: Vec::new(),
            val_labels_on: false,
            cat_width: 0,
            cat_labels_on: false,
            cat_labels: Vec::new(),
            cat_pad,
            cat_bins,
            crosses_inside: false,
            cross_frac: None,
            val_cross: None,
            table: None,
            far_x: false,
            far_y: false,
            title_top: false,
            val_rev: false,
        };
        if pie {
            return Ok(Some(li));
        }
        if crate::chartparts::is_radar(t) {
            let on = c.axes[0].tick_pos != -4142 && !c.axis_hidden[0];
            li.radar = Some(if on { cats.clone() } else { Vec::new() });
            return Ok(Some(li));
        }
        let (vt, ct) = (&c.axes[1], &c.axes[0]);
        li.side_title = if li.bar { ct.has_title } else { vt.has_title };
        li.foot_title = if li.bar { vt.has_title } else { ct.has_title };
        li.val_labels_on = vt.tick_pos != -4142 && !c.axis_hidden[1];
        li.cat_labels_on = ct.tick_pos != -4142 && !c.axis_hidden[0];
        if !scaled {
            return Ok(Some(li));
        }
        let (lo, hi, major, _) = self.axis_scale(r, c, 1)?;
        if c.axes[1].log && lo > 0.0 && hi >= lo {
            let b = c.axes[1].log_base;
            let n = ((hi / lo).ln() / b.ln() + 0.5).floor() as i32;
            li.val_labels = (0..=n)
                .map(|k| val_text_emu(&xlcalc::format_general(lo * b.powi(k))))
                .collect();
        } else if major > 0.0 && hi >= lo {
            let n = ((hi - lo) / major + 0.5).floor() as i64;
            li.val_labels = (0..=n)
                .map(|k| {
                    let v = lo + k as f64 * major;

                    val_text_emu(&if percent {
                        format!("{}%", (v * 100.0).round())
                    } else {
                        xlcalc::format_general(v)
                    })
                })
                .collect();
        }
        if !is_xy(t)
            && c.series.iter().any(|s| s.axis_group == 2)
            && c.axes[2].tick_pos != -4142
            && !c.axes[2].log
        {
            let (lo, hi, major, _) = self.axis_scale(r, c, 2)?;
            if major > 0.0 && hi >= lo {
                let n = ((hi - lo) / major + 0.5).floor() as i64;
                li.sec_labels = (0..=n)
                    .map(|k| val_text_emu(&xlcalc::format_general(lo + k as f64 * major)))
                    .collect();
            }
        }

        let unscaled_log = c.axes[1].log
            && !matches!(c.bag.get(crate::chartparts::SCALED), Some(BagVal::I(1)))
            && !matches!(c.bag.get("Axis2.Crosses"), Some(BagVal::I(-4114)));

        let cross2 = match c.bag.get("Axis2.Crosses") {
            Some(BagVal::I(v)) => *v,
            _ => -4105,
        };
        let span_ok = hi > lo && !c.axes[1].log;
        let f_at = |at: f64| (at.clamp(lo, hi) - lo) / (hi - lo);
        let f = match (cross2, c.bag.get("Axis2.CrossesAt")) {
            _ if !span_ok => 0.0,
            (2, _) => 1.0,
            (4, _) => 0.0,
            (-4114, Some(BagVal::D(v))) => f_at(*v),
            _ => {
                if lo < 0.0 {
                    f_at(0.0)
                } else {
                    0.0
                }
            }
        };

        let p = if c.axes[1].reverse { 1.0 - f } else { f };
        let edge_labels = matches!(ct.tick_pos, -4127 | -4134);

        let auto_inside = cross2 == -4105 && (lo < 0.0 || unscaled_log);
        let custom_inside = cross2 == -4114 && span_ok && ((p > 0.0 && p < 1.0) || lo < 0.0);
        let inside = !edge_labels && (auto_inside || custom_inside);
        li.crosses_inside = inside;
        li.cross_frac = (inside && span_ok).then_some(p);
        li.val_rev = c.axes[1].reverse;
        let flips = matches!(t, 51 | 57 | 4 | 65 | 1 | -4100);
        if flips {
            let (cat_rev, val_rev) = (c.axes[0].reverse, c.axes[1].reverse);
            let far_cat = match ct.tick_pos {
                -4127 => !val_rev,
                -4134 => val_rev,
                _ => !inside && p >= 1.0,
            };
            let cross1 = match c.bag.get("Axis1.Crosses") {
                Some(BagVal::I(v)) => *v,
                _ => -4105,
            };
            let q = match cross1 {
                2 => 1.0,
                -4114 => match value_cross_pos(c, date_bins, cats.len() as i64) {
                    Some(r) if r > 0.0 => 0.5,
                    _ => 0.0,
                },
                _ => 0.0,
            };
            let far_val = match vt.tick_pos {
                -4127 => !cat_rev,
                -4134 => cat_rev,
                _ => (if cat_rev { 1.0 - q } else { q }) >= 1.0,
            };
            (li.far_x, li.far_y) = if li.bar {
                (far_cat, far_val)
            } else {
                (far_val, far_cat)
            };

            li.title_top = li.far_y || (!li.bar && inside && val_rev);
        }
        if matches!(t, 51 | 4 | 65)
            && matches!(c.bag.get("Chart.HasDataTable"), Some(BagVal::B(true)))
        {
            let key = if matches!(c.bag.get("DataTable.ShowLegendKey"), Some(BagVal::B(false))) {
                crate::chart_lobj::border().0 / 4
            } else if t == 51 {
                crate::chartfont::legend_key_emu(crate::chartfont::fonts().legend)
            } else {
                304_800
            };
            let widest = c
                .series
                .iter()
                .enumerate()
                .map(|(k, s)| series_name_emu(wb, k, s))
                .max()
                .unwrap_or(0);
            li.table = Some((key + widest + table_head_pad(), c.series.len() as i64));
        }
        if is_xy(t) {
            let (xlo, xhi, xmajor, _) = self.axis_scale(r, c, 0)?;
            if xmajor > 0.0 && xhi >= xlo {
                let n = ((xhi - xlo) / xmajor + 0.5).floor() as i64;
                let last = xlo + n as f64 * xmajor;
                li.right_overhang = legend_text_emu(&xlcalc::format_general(last)) / 2;
                li.left_overhang = legend_text_emu(&xlcalc::format_general(xlo)) / 2;
            }
        } else {
            li.cat_width = cats.iter().map(|s| legend_text_emu(s)).max().unwrap_or(0);
            if !li.bar && li.cat_labels_on && li.cat_width > 0 {
                li.cat_labels = cats.clone();
                if c.axes[0].reverse {
                    li.cat_labels.reverse();
                }
            }

            if t == 1 && !crate::charts::between_categories(c) {
                let end = if c.axes[0].reverse {
                    cats.first()
                } else {
                    cats.last()
                };
                li.right_overhang = end.map_or(0, |s| legend_text_emu(s) / 2);
            }
            if !li.bar && li.table.is_none() {
                li.val_cross = value_cross_pos(c, date_bins, cats.len() as i64)
                    .filter(|f| *f > 0.0 && !li.far_x && !c.axes[0].reverse);
            }
        }
        Ok(Some(li))
    }

    fn target_rect(&self, r: &DrawRef, c: &Chart, key: &str) -> Result<Option<[i64; 4]>, OmError> {
        let Some(mut t) = rect_of(c, key) else {
            return Ok(None);
        };
        let Some(BagVal::S(sz)) = c.bag.get(LAID_SIZE) else {
            return Ok(Some(t));
        };
        let old: Vec<i64> = sz.split(',').filter_map(|x| x.parse().ok()).collect();
        let sh = self.shape_clone(r)?;
        let (_, _, w, h) = self.book(r.book)?.wb.shape_rect_emu(r.sheet, &sh.pos);
        if let [ow, oh] = old[..] {
            if ow > 0 && oh > 0 && (ow != w || oh != h) {
                let sc = |v: i64, a: i64, b: i64| (v as f64 * b as f64 / a as f64).round() as i64;
                t[0] = sc(t[0] + OUTSIDE, ow, w) - OUTSIDE;
                t[1] = sc(t[1] + OUTSIDE, oh, h) - OUTSIDE;
                t[2] = sc(t[2], ow, w);
                t[3] = sc(t[3], oh, h);
            }
        }
        Ok(Some(t))
    }

    pub(crate) fn chart_geom(&self, r: &DrawRef, c: &Chart) -> Result<Option<Geom>, OmError> {
        self.chart_geom_with(r, c, true)
    }

    pub(crate) fn chart_geom_with(
        &self,
        r: &DrawRef,
        c: &Chart,
        dated: bool,
    ) -> Result<Option<Geom>, OmError> {
        let _fonts = crate::chartfont::font_scope(crate::chartparts::chart_fonts(c));
        let Some(mut li) = self.layout_input_with(r, c, dated)? else {
            return Ok(None);
        };
        let lay = auto_layout(&li);
        let li_titles = (li.side_title, li.foot_title);
        let early = li.table.is_some() && matches!(c.bag.get(PRINT_EARLY), Some(BagVal::I(1)));

        let (outer, manual) = (
            self.target_rect(r, c, OUTER)?,
            self.target_rect(r, c, MANUAL)?,
        );

        let applied = match outer {
            Some(_) if c.bag.contains_key(RERUN) => None,
            Some(o) => Some(o),
            None => manual,
        };
        if let (Some((head, _)), Some(x)) = (&mut li.table, applied.map(|o| o[0])) {
            if !crate::bag::is_3d_type(c.chart_type) {
                *head = table_head_at(*head, x);
            }
        }
        (li.side_title, li.foot_title) = (false, false);
        let mut res = reserves(&li);
        (li.side_title, li.foot_title) = li_titles;
        res[0] += TITLE_EXTRA;
        res[1] += TITLE_EXTRA;
        res[3] += TITLE_EXTRA;
        res[2] = 0;
        let (plot, inside) = match (outer, manual) {
            (Some(o), _) if !li.pie => {
                let ins = outer_axis_layout(&li, o);
                res = [
                    ins[0] - o[0],
                    ins[1] - o[1],
                    o[0] + o[2] - ins[0] - ins[2],
                    o[1] + o[3] - ins[1] - ins[3],
                ];
                (o, ins)
            }
            (_, Some(m)) if !li.pie => {
                let room = inside_out_room(li.h, m);
                let (l, r) = cat_overhang(&li, m[2], m[2], room);
                res[0] = res[0].max(l);
                res[2] = if early {
                    (r - PRINT_EARLY_WIDTH).max(0)
                } else {
                    r
                };
                if li.cat_labels_on && !li.bar && !li.crosses_inside && li.table.is_none() {
                    res[3] += label_extent(&li, m[2], room) - cat_text_h();
                }

                let bottom = if li.table.is_some() && m[3] == 0 {
                    half_tick() + TITLE_EXTRA
                } else {
                    res[3]
                };

                if li.table.is_some() && !li.cat_labels.is_empty() {
                    let (lines1, _, last1) = table_label_lines(&li.cat_labels, m[2] - res[0]);
                    if lines1 > 1 {
                        let n = li.cat_labels.len() as i64;
                        res[2] = (76_200 + last1 / 2 - m[2] / n / 2).max(0);
                        let a2 = m[2] + res[2] - PASS2_SHRINK;
                        let (lines2, rot, _) = table_label_lines(&li.cat_labels, a2);
                        let r2 = if rot {
                            0
                        } else {
                            cat_overhang(&li, m[2] + res[2], m[2] + res[2], room).1
                        };
                        let iw = m[2] + res[2] - r2;
                        let oh = m[3] + res[1] + bottom + (lines1 - 1) * cat_text_h();
                        let top = li.h - OUTSIDE - oh;
                        let ih = oh - res[1] - bottom - (lines2 - 1) * cat_text_h();
                        res[2] = r2;
                        return Ok(Some(Geom {
                            plot: [m[0] - res[0], top, iw + res[0] + r2, oh],
                            inside: [m[0], top + res[1], iw, ih],
                            legend: lay.legend,
                            res,
                            table: true,
                            title: li.title,
                            early,
                        }));
                    }
                }
                (
                    [
                        m[0] - res[0],
                        m[1] - res[1],
                        m[2] + res[0] + res[2],
                        m[3] + res[1] + bottom,
                    ],
                    m,
                )
            }
            _ if early => {
                let (mut plot, mut inside) = (lay.plot, lay.inside);
                plot[2] += PRINT_EARLY_WIDTH;
                inside[2] += PRINT_EARLY_WIDTH;
                (plot, inside)
            }
            _ if is_3d_column(c.chart_type) && !matches!(c.chart_type, 60..=62) => {
                no_less_than_min(lay.plot, self.inside_3d(r, c, &li, lay.plot)?)
            }
            _ => {
                let mut inside = lay.inside;
                if let Some(d) = self.bar_3d_shift(r, c, &li, lay.plot)? {
                    inside[0] += d[0];
                    inside[2] -= d[0] + d[1];
                    inside[3] -= d[2];
                }
                no_less_than_min(lay.plot, inside)
            }
        };
        Ok(Some(Geom {
            plot,
            inside,
            legend: lay.legend,
            res,
            table: li.table.is_some(),
            title: li.title,
            early,
        }))
    }

    fn bar_3d_shift(
        &self,
        r: &DrawRef,
        c: &Chart,
        li: &LayoutIn,
        plot: [i64; 4],
    ) -> Result<Option<[i64; 3]>, OmError> {
        if !matches!(c.chart_type, 60..=62)
            || li.far_x
            || li.far_y
            || li.side_title
            || li.crosses_inside
            || li.cross_frac.is_some()
            || li.val_rev
            || !li.val_labels_on
            || matches!(c.bag.get("Chart.RightAngleAxes"), Some(BagVal::B(true)))
        {
            return Ok(None);
        }
        let rot = match c.bag.get("Chart.Rotation") {
            Some(BagVal::I(n)) => n.rem_euclid(360),
            _ => 20,
        };
        let quarter = rot / 90;
        let at_max = matches!(c.bag.get("Axis1.Crosses"), Some(BagVal::I(2))) != c.axes[0].reverse;
        let wb = &self.book(r.book)?.wb;
        let cats = c
            .series
            .iter()
            .map(|s| part_values(wb, &s.values).len())
            .max()
            .unwrap_or(1);
        let view = crate::chart_3d::View3d::of(c, c.series.len(), cats);
        let (_, horizontal) = view.value_axis(plot[2], plot[3], at_max);
        let [l, t, w, h] = plot;
        let ins = auto_layout(li).inside;
        let old = [
            ins[0] - l,
            ins[1] - t,
            l + w - ins[0] - ins[2],
            t + h - ins[1] - ins[3],
        ];
        let widest = li.val_labels.iter().copied().max().unwrap_or(0);
        let first = li.val_labels.first().copied().unwrap_or(0) / 2;
        let cat_left = quarter == 0 || quarter == 3;
        let (left, right, bottom) = if horizontal {
            (if cat_left { old[0] } else { first }, old[2], old[3])
        } else {
            let vw = widest + side_axis_pad(crate::chartfont::fonts().val);
            match quarter {
                0 => (old[0].max(vw), 0, 0),
                1 => (0, vw, 0),
                2 => (vw, 0, 0),
                _ => (old[0], vw, 0),
            }
        };
        let d = [left - old[0], right - old[2], bottom - old[3]];
        Ok((d != [0; 3]).then_some(d))
    }

    fn inside_3d(
        &self,
        r: &DrawRef,
        c: &Chart,
        li: &LayoutIn,
        o: [i64; 4],
    ) -> Result<[i64; 4], OmError> {
        let wb = &self.book(r.book)?.wb;
        let [left, top, _, bottom] = reserves(li);
        let rot = match c.bag.get("Chart.Rotation") {
            Some(BagVal::I(n)) => *n,
            _ => 20,
        };
        let flat = matches!(c.bag.get("Chart.Elevation"), Some(BagVal::I(0)));
        let depth_names = || {
            c.series
                .iter()
                .enumerate()
                .map(|(k, s)| series_name_emu(wb, k, s))
                .max()
                .unwrap_or(0)
                + DEPTH_LABEL_PAD
        };

        if (90..270).contains(&rot) {
            let (right, bottom) = if rot < 180 {
                let widths: Vec<i64> = li.cat_labels.iter().map(|s| legend_text_emu(s)).collect();

                let n = widths.len() as i64;
                let view = crate::chart_3d::View3d::of(c, c.series.len(), widths.len());
                let step = view
                    .category_axis_rise(o[2], o[3], li.far_y)
                    .map_or(1, |rise| side_label_step(n, rise, cat_text_h()));
                let widest = widths
                    .iter()
                    .step_by(step as usize)
                    .copied()
                    .max()
                    .unwrap_or(0);
                let series = if c.chart_type == -4100 && !flat {
                    SIDE_AXIS_H
                } else {
                    0
                };
                (
                    widest + side_axis_pad(crate::chartfont::fonts().cat),
                    half_tick() + series,
                )
            } else if c.chart_type == -4100 && !flat {
                (depth_names(), bottom)
            } else {
                (0, bottom)
            };
            return Ok([
                o[0],
                o[1] + half_tick(),
                o[2] - right,
                o[3] - half_tick() - bottom,
            ]);
        }
        let depth = if c.chart_type == -4100 && (0..45).contains(&rot) && !flat {
            depth_names()
        } else {
            0
        };

        let (left, right) = if li.far_x {
            (0, left.max(depth))
        } else {
            (left, depth)
        };
        let (top, bottom) = if li.far_y {
            (bottom, top)
        } else {
            (top, bottom)
        };
        Ok([
            o[0] + left,
            o[1] + top,
            o[2] - left - right,
            o[3] - top - bottom,
        ])
    }

    pub(crate) fn mark_print_early(&mut self, r: &DrawRef) -> Result<(), OmError> {
        let cr = DrawRef {
            kind: crate::shapes::DrawKind::Chart,
            ..*r
        };
        self.chart_edit(&cr, |_, c| {
            if !matches!(c.bag.get("Chart.HasDataTable"), Some(BagVal::B(true))) {
                c.bag.insert(PRINT_EARLY.into(), BagVal::I(1));
                c.bag.insert(LAID.into(), BagVal::I(1));
            }
            Ok(())
        })
    }

    pub(crate) fn scatter_first(c: &Chart) -> bool {
        is_xy(c.chart_type)
            && c.series
                .iter()
                .find(|s| s.axis_group == 1)
                .is_some_and(|s| is_xy(s.chart_type))
    }

    pub(crate) fn mark_cat_cross(&mut self, r: &DrawRef, c: &Chart) -> Result<(), OmError> {
        let cat = !is_xy(crate::chartparts::axis_type(c)) && !Self::scatter_first(c);
        if c.bag.contains_key(CAT_CROSS) == cat {
            return Ok(());
        }
        self.chart_edit(r, |_, ch| {
            if cat {
                ch.bag.insert(CAT_CROSS.into(), BagVal::I(1));
            } else {
                ch.bag.remove(CAT_CROSS);
            }
            Ok(())
        })
    }

    fn mark_laid(&mut self, r: &DrawRef) -> Result<(), OmError> {
        self.chart_edit(r, |wb, c| {
            c.bag.insert(LAID.into(), BagVal::I(1));
            c.bag.insert(crate::chartparts::SCALED.into(), BagVal::I(1));

            if !c.has_title
                && c.series.len() == 1
                && !matches!(c.bag.get("#title_off"), Some(BagVal::I(1)))
            {
                c.title = crate::chartparts::auto_title(wb, c);
                c.title_ref = None;
                c.has_title = true;
            }
            Ok(())
        })
    }

    pub(crate) fn geom_get(
        &mut self,
        r: &DrawRef,
        part: &str,
        c: &Chart,
        m: &'static Member,
    ) -> Result<Option<Variant>, OmError> {
        let _fonts = crate::chartfont::font_scope(crate::chartparts::chart_fonts(c));
        let idx = |n: &str| {
            ["Left", "Top", "Width", "Height"]
                .iter()
                .position(|x| *x == n)
        };
        let name = m.name.trim_start_matches('_');
        let (inner, base) = match name.strip_prefix("Inside") {
            Some(b) if part == "PlotArea" => (true, b),
            _ => (false, name),
        };
        let Some(k) = idx(base) else {
            return Ok(None);
        };
        if matches!(part, "PlotArea" | "Legend") {
            self.mark_cat_cross(r, c)?;
        }

        let mut fresh;
        let mut c = c;
        if matches!(c.bag.get(PRINT_EARLY), Some(BagVal::I(1)))
            && !c.bag.contains_key(MANUAL)
            && !c.bag.contains_key(OUTER)
        {
            fresh = c.clone();
            fresh.bag.remove(PRINT_EARLY);
            c = &fresh;
            self.chart_edit(r, |_, ch| {
                ch.bag.remove(PRINT_EARLY);
                Ok(())
            })?;
        }
        let Some(g) = self.chart_geom(r, c)? else {
            return Ok(None);
        };
        let rect = match part {
            "PlotArea" if inner => g.inside,
            "PlotArea" => g.plot,
            "Legend" => match g.legend {
                Some(l) => l,
                None => return Ok(None),
            },
            _ => return Ok(None),
        };
        self.mark_laid(r)?;
        if k < 2 {
            return Ok(Some(Variant::R8((rect[k] + OUTSIDE) as f64 / EMU - 4.0)));
        }
        Ok(Some(Variant::R8(rect[k] as f64 / EMU)))
    }

    pub(crate) fn geom_put(
        &mut self,
        r: &DrawRef,
        c: &Chart,
        m: &'static Member,
        a: &Args,
    ) -> Result<(), OmError> {
        if m.kind != InvokeKind::Put {
            return Ok(());
        }
        let _fonts = crate::chartfont::font_scope(crate::chartparts::chart_fonts(c));
        let name = m.name.trim_start_matches('_');
        let (inner, base) = match name.strip_prefix("Inside") {
            Some(b) => (true, b),
            None => (false, name),
        };
        let Some(k) = ["Left", "Top", "Width", "Height"]
            .iter()
            .position(|x| *x == base)
        else {
            return Ok(());
        };

        if !matches!(c.bag.get(LAID), Some(BagVal::I(1))) {
            if k >= 2 && inner && a.put.is_some() {
                return Err(OmError::exception(0x8000_4005_u32 as i32, None));
            }
            return Ok(());
        }
        let Some(g) = self.chart_geom(r, c)? else {
            return Ok(());
        };
        let Some(v) = a.put.as_ref().and_then(|v| match v {
            Variant::R8(x) => Some(*x),
            Variant::I4(x) => Some(f64::from(*x)),
            _ => None,
        }) else {
            return Ok(());
        };
        let sh = self.shape_clone(r)?;
        let (_, _, w, h) = self.book(r.book)?.wb.shape_rect_emu(r.sheet, &sh.pos);
        let emu = v * EMU;
        let (max_x, max_y) = (w - OUTSIDE, h - OUTSIDE);
        let pos = if emu.is_finite() {
            emu.clamp(-1e15, 1e15) as i64
        } else {
            0
        };

        let huge = v > f64::from(i32::MAX);
        let whole = if huge || emu < 0.0 || !emu.is_finite() {
            0
        } else {
            (v.floor() as i64) * 12_700
        };
        if !inner {
            let mut o = g.plot;
            match k {
                0 => o[0] = pos.max(-OUTSIDE).min(max_x - o[2]),
                1 => o[1] = pos.max(-OUTSIDE).min(max_y - o[3]),
                2 => o[2] = whole.min(max_x - o[0]),
                _ => o[3] = whole.min(max_y - o[1]),
            }
            let s = format!("{},{},{},{}", o[0], o[1], o[2], o[3]);
            let len = i32::try_from(o[3]).unwrap_or(i32::MAX);
            let size = format!("{w},{h}");
            return self.chart_edit(r, |_, c| {
                c.bag.insert(LAID_SIZE.into(), BagVal::S(size));
                c.bag.insert(OUTER.into(), BagVal::S(s));
                c.bag.insert(SCALE_LEN.into(), BagVal::I(len));
                c.bag.remove(MANUAL);
                c.bag.remove(RERUN);
                Ok(())
            });
        }
        let mut ins = g.inside;
        let mut asked = None;
        let [rl, rt, _, rb] = g.res;
        match k {
            0 => ins[0] = pos.max(-OUTSIDE + rl).min(max_x - ins[2]),
            1 => {
                let asked = match c.bag.get(HGT_REQ) {
                    Some(BagVal::I(a)) if g.table => i64::from(*a),
                    _ => 0,
                };
                if asked > 0 && max_y - rb - asked < -OUTSIDE + rt {
                    ins[1] = -OUTSIDE + rt;
                    ins[3] = max_y - rb - ins[1];
                } else {
                    let below = if g.table && ins[3] == 0 {
                        half_tick() + TITLE_EXTRA
                    } else {
                        rb
                    };
                    ins[1] = pos.max(-OUTSIDE + rt).min(max_y - below - ins[3]);
                }
            }
            2 if huge => {
                ins[2] = 0;
                let (l, rr) = match self.layout_input(r, c)? {
                    Some(li) => {
                        let (l, rr) = cat_overhang(&li, 0, 0, inside_out_room(li.h, ins));
                        (rl.max(l), rr)
                    }
                    None => (rl, 0),
                };
                if ins[0] - l < -OUTSIDE || c.bag.contains_key(HGT_REQ) {
                    let o = [-OUTSIDE, ins[1] - rt, l + rr, ins[3] + rt + rb];
                    let s = format!("{},{},{},{}", o[0], o[1], o[2], o[3]);
                    let len = i32::try_from(o[3]).unwrap_or(i32::MAX);
                    let size = format!("{w},{h}");
                    return self.chart_edit(r, |_, c| {
                        c.bag.insert(LAID_SIZE.into(), BagVal::S(size));
                        c.bag.insert(OUTER.into(), BagVal::S(s));
                        c.bag.insert(RERUN.into(), BagVal::I(1));
                        c.bag.insert(SCALE_LEN.into(), BagVal::I(len));
                        c.bag.remove(MANUAL);
                        Ok(())
                    });
                }
            }
            2 => ins[2] = whole.min(max_x - ins[0]),
            _ => {
                let room = max_y - rb - ins[1];
                if g.table
                    && (g.inside[3] == 0 || (g.early && g.inside[3] == MIN_INNER))
                    && whole > room
                {
                    ins[1] = rt
                        + if g.title && !g.early {
                            29_339
                        } else {
                            -OUTSIDE
                        };
                    ins[3] = max_y - rb - ins[1];
                    asked = Some(i32::try_from(whole).unwrap_or(i32::MAX));
                } else if g.early && g.table && whole == 0 {
                    ins[3] = MIN_INNER;
                } else {
                    ins[3] = whole.min(room);
                }
            }
        }
        let s = format!("{},{},{},{}", ins[0], ins[1], ins[2], ins[3]);
        let size = format!("{w},{h}");
        self.chart_edit(r, |_, c| {
            c.bag.insert(LAID_SIZE.into(), BagVal::S(size));
            c.bag.insert(MANUAL.into(), BagVal::S(s));
            if k == 3 {
                c.bag.insert(HGT_PUT.into(), BagVal::I(1));
                match asked {
                    Some(a) => c.bag.insert(HGT_REQ.into(), BagVal::I(a)),
                    None => c.bag.remove(HGT_REQ),
                };
            }
            c.bag.remove(OUTER);
            c.bag.remove(RERUN);
            c.bag.remove(SCALE_LEN);
            Ok(())
        })
    }
}
