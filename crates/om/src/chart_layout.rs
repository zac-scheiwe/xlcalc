use crate::chart_lobj::{Dock, LObj, OUTSIDE};
use crate::chartparts::legend_text_emu;

#[derive(Clone, Copy, PartialEq, Debug)]
pub(crate) enum LegendAt {
    Right,
    Left,
    Top,
    Bottom,
}

pub(crate) struct LegendIn {
    pub at: LegendAt,
    pub texts: Vec<i64>,

    pub lines: Vec<i64>,
    pub key: i64,
}

pub(crate) struct LayoutIn {
    pub w: i64,
    pub h: i64,
    pub pie: bool,

    pub radar: Option<Vec<String>>,

    pub bar: bool,

    pub right_overhang: i64,

    pub left_overhang: i64,

    pub between: bool,
    pub legend: Option<LegendIn>,
    pub title: bool,

    pub side_title: bool,
    pub foot_title: bool,

    pub val_labels: Vec<i64>,

    pub sec_labels: Vec<i64>,
    pub val_labels_on: bool,

    pub cat_width: i64,
    pub cat_labels_on: bool,

    pub cat_labels: Vec<String>,

    pub cat_pad: bool,

    pub cat_bins: i64,

    pub crosses_inside: bool,

    pub cross_frac: Option<f64>,

    pub val_cross: Option<f64>,

    pub table: Option<(i64, i64)>,

    pub far_x: bool,
    pub far_y: bool,

    pub title_top: bool,

    pub val_rev: bool,
}

pub(crate) type Rect = [i64; 4];

pub(crate) struct Layout {
    pub plot: Rect,
    pub inside: Rect,
    pub legend: Option<Rect>,
}

const TITLE_H: i64 = 317_119;

const PLOT_INSET: i64 = 63_500;
fn legend_entry_h() -> i64 {
    76_200 + legend_line_h()
}

fn legend_line_h() -> i64 {
    crate::chartfont::legend_line_emu(crate::chartfont::fonts().legend)
}

fn legend_pad() -> i64 {
    crate::chart_lobj::border().0 / 4
}

fn side_legend_room(chart_w: i64) -> i64 {
    chart_w / 3
}

pub(crate) fn legend_wrap_room(chart_w: i64, key: i64) -> i64 {
    side_legend_room(chart_w) - key - legend_pad()
}
pub(crate) const AXIS_TITLE: i64 = 193_104;

pub(crate) const TITLE_EXTRA: i64 = 76_200;

pub(crate) fn label_gap() -> i64 {
    crate::chartfont::side_gap_emu(crate::chartfont::fonts().val)
}

pub(crate) fn cat_label_gap() -> i64 {
    crate::chartfont::side_gap_emu(crate::chartfont::fonts().cat)
}

pub(crate) fn cat_foot() -> i64 {
    crate::chartfont::foot_emu(crate::chartfont::fonts().cat)
}

fn val_foot() -> i64 {
    crate::chartfont::foot_emu(crate::chartfont::fonts().val)
}
pub(crate) fn half_tick() -> i64 {
    crate::chartfont::half_tick_emu(crate::chartfont::fonts().val)
}

fn table_base() -> i64 {
    let (_, fy) = crate::chart_lobj::border();
    cat_text_h() + crate::chartfont::label_offset_emu(crate::chartfont::fonts().cat) - fy
}

fn table_row() -> i64 {
    let (bx, by) = crate::chart_lobj::border();
    let px = (914_400.0_f64 / 96.0) as i64;
    (bx / 4) * 2 + cat_text_h() - 1 + px.max(by / 4) / 4
}

pub(crate) fn table_head_pad() -> i64 {
    let (bx, _) = crate::chart_lobj::border();
    (bx / 4) * 2 + bx
}

pub(crate) fn table_head_at(head: i64, x: i64) -> i64 {
    let (bx, _) = crate::chart_lobj::border();
    (head - bx).min(x + crate::chart_lobj::OUTSIDE) + bx
}

fn half(x: i64) -> i64 {
    x / 2
}

fn legend_size(l: &LegendIn, chart_w: i64, chart_h: i64) -> (i64, i64) {
    let key = l.key;
    let n = l.texts.len() as i64;
    let pad = legend_pad();
    let line = legend_line_h();
    let dy = crate::chart_lobj::border().1;
    match l.at {
        LegendAt::Right | LegendAt::Left => {
            let widest = l.texts.iter().copied().max().unwrap_or(0);
            let lines = l.lines.iter().copied().max().unwrap_or(1).max(1);

            let room = (chart_h as f64 * 0.9) as i64;
            let single = l.lines.last() == Some(&1);
            let (mut y, mut bottom) = (dy / 2, dy / 2);
            for _ in 0..n {
                let mut b = y + lines * line;
                if b >= room {
                    b = y + line;
                    if !single || b > room {
                        break;
                    }
                }
                bottom = b;
                y = b + dy;
            }
            (
                (widest + key + 2 * pad).min(side_legend_room(chart_w)) + 2 * pad,
                bottom + dy / 2,
            )
        }
        LegendAt::Top | LegendAt::Bottom => {
            let room = (chart_w as f64 * 0.9) as i64;
            let entries: Vec<i64> = l.texts.iter().map(|t| t + key).collect();
            let sum: i64 = entries.iter().sum();
            if sum < room {
                return ((sum as f64 * 1.2).min(room as f64) as i64, legend_entry_h());
            }
            let widest = entries.iter().copied().max().unwrap_or(1).clamp(1, room);
            let mut cols = ((room - pad) / widest).min(n).max(1);
            let rows = (n + cols - 1) / cols;
            if rows > 1 {
                cols -= (rows * cols - n) / rows;
            }
            let max_h = chart_h / 2;
            let fit = (max_h / line).min(rows);
            let squeezed = (max_h / (((line as f64 * 0.88) as i64).max(1))).min(rows);
            let fit = fit.max(squeezed).max(1);
            let mut shown = if cols == 1 && line != 1 { fit } else { n };
            if fit * cols < n {
                shown = fit * cols;
            }
            let row = line + dy;

            let mut y = 0;
            for i in 0..n {
                y = row * (i / cols) + dy / 2;
                if y > max_h || i == shown {
                    break;
                }
            }
            (cols * widest, dy / 4 + line + y)
        }
    }
}

pub(crate) fn cat_text_h() -> i64 {
    crate::chartfont::line_emu(crate::chartfont::fonts().cat)
}

#[derive(Clone, Copy, PartialEq, Debug)]
pub(crate) struct LabelPlan {
    pub angle: i32,
    pub step: i64,
}

pub(crate) fn label_plan(labels: &[String], w: i64, room: i64, pad: bool) -> LabelPlan {
    let named = labels.iter().filter(|s| !s.is_empty()).count();
    let n = if named > 0 { named } else { labels.len() } as i64;
    let plain = LabelPlan { angle: 0, step: 1 };
    if n < 2 {
        return plain;
    }
    let grow = |x: i64| if pad { (x as f64 * 1.2) as i64 } else { x };
    let sampled = || {
        let every = if n > 100 { n as usize / 101 + 1 } else { 1 };
        labels.iter().step_by(every)
    };

    let widest = sampled()
        .flat_map(|s| s.split(char::is_whitespace))
        .map(legend_text_emu)
        .max()
        .unwrap_or(0);
    let widest = grow(widest).max(1);
    let h = grow(cat_text_h());
    let r12 = (w / widest).clamp(1, n);
    if r12 >= n {
        return plain;
    }
    let limit = if pad { i64::MAX } else { limit_45(w, room) };
    let sin45 = sin45();

    let first = sampled().map(|t| first_line(t, limit)).max().unwrap_or(0);
    let ext_raw = if pad {
        grow(((first + cat_text_h()) as f64 * sin45) as i64) as f64
    } else {
        (first + h) as f64 * sin45
    };
    let slot = w / n;
    let rows = match (sin45 * slot as f64) as i64 {
        0 => n,
        e => h / e,
    };
    let extent = w.min(ext_raw.round() as i64).max(1);
    let r15 = ((n + 1) / (rows.max(0) + 1)).max(w / extent).clamp(1, n);
    if r15 >= n {
        return LabelPlan { angle: 45, step: 1 };
    }
    let r14 = (w / h).clamp(1, n);
    let p12 = (n - 1) / r12;
    let p15 = (n - 1) / r15;
    let fit90 = (n - 1) / r14;
    let both = p12.min(p15);
    if fit90 < both {
        LabelPlan {
            angle: 90,
            step: fit90 + 1,
        }
    } else {
        LabelPlan {
            angle: if p12 > p15 { 45 } else { 0 },
            step: both + 1,
        }
    }
}

#[allow(
    clippy::approx_constant,
    reason = "Excel's rotation uses the single precision value of pi, which must not be PI"
)]
fn max_rotated_box(angle: f64, w: i64, h: i64, line: i64) -> (i64, i64) {
    if (angle + 90.0).abs() <= 1.5e-14 || (angle - 90.0).abs() <= 1.5e-14 {
        return (h, w);
    }
    if angle.abs() <= 1.5e-14 || (w < 1 && h < 1) {
        return (w, h);
    }
    let rad = angle / 180.0 * 3.1415927;
    let (cos, sin) = (rad.cos(), rad.sin());
    let tan = sin / cos;
    let (wf, hf) = (w as f64, h as f64);
    if h < 1 {
        return ((wf / cos) as i64, (wf / sin.abs()) as i64);
    }
    if w < 1 {
        return ((hf / sin.abs()) as i64, (hf / cos) as i64);
    }
    let limit_angle = hf.atan2(wf) * 180.0 / 3.1415927;

    let (d5, d6, d7, d8, d9, d10) = if line < 1 {
        let d6 = (hf / cos - wf / sin.abs()) / (tan.abs() - 1.0 / tan.abs());
        (
            (hf - sin.abs() * d6) / cos,
            d6,
            wf / (2.0 * sin.abs()),
            hf / (2.0 * cos),
            hf / (2.0 * sin.abs()),
            wf / (2.0 * cos),
        )
    } else {
        let l = line as f64;
        (
            l,
            (hf - l * cos) / sin.abs(),
            l,
            l,
            hf / sin.abs() - l / tan.abs(),
            wf / cos - l * tan.abs(),
        )
    };
    let by_height = (d9 as i64, d8 as i64);
    if angle <= limit_angle {
        if angle < -limit_angle || limit_angle < angle {
            if limit_angle <= angle + 90.0 || d8 <= d5 {
                return by_height;
            }
            if d6 < d10 {
                return (d6 as i64, d5 as i64);
            }
        } else {
            let a90 = angle + 90.0;
            if a90 < 180.0 - limit_angle && limit_angle < a90 {
                if d9 <= d6 {
                    return by_height;
                }
                if d5 < d7 {
                    return (d6 as i64, d5 as i64);
                }
            }
        }
        return (d10 as i64, d7 as i64);
    }
    if 180.0 - limit_angle < angle + 90.0 && d5 < d8 {
        return if d10 <= d6 {
            (d10 as i64, d7 as i64)
        } else {
            (d6 as i64, d5 as i64)
        };
    }
    by_height
}

fn limit_45(w: i64, room: i64) -> i64 {
    max_rotated_box(-45.0, w, room, cat_text_h()).0
}

fn flat_limit(n: i64, w: i64, step: i64) -> i64 {
    w / ((n - 1) / step.max(1) + 1)
}

fn flat_layouts(labels: &[String], which: &[usize], limit: i64, room: i64) -> Vec<Lines> {
    let lay = |w: i64| -> (Vec<Lines>, bool) {
        let w = if w < 1 { i64::from(i32::MAX) } else { w };
        let all: Vec<(Lines, bool)> = which
            .iter()
            .map(|&k| art_layout(&labels[k], w, room))
            .collect();
        let cut = all.iter().any(|l| l.1);
        (all.into_iter().map(|l| l.0).collect(), cut)
    };
    let (lines, cut) = lay(limit);
    if cut {
        lay(limit * 5).0
    } else {
        lines
    }
}

fn sin45() -> f64 {
    (0.25 * f64::from(std::f32::consts::PI)).sin()
}

fn cos45() -> f64 {
    (0.25 * f64::from(std::f32::consts::PI)).cos()
}

pub(crate) fn side_label_step(n: i64, len: i64, size: i64) -> i64 {
    if n < 2 {
        return 1;
    }
    let fit = (len * 9 / 10 / size.max(1)).clamp(1, n);
    (n - 1) / fit + 1
}

fn wrapped_lines(text: &str, limit: i64) -> (i64, i64) {
    let l = wrapped_line_widths(text, limit);
    (l.len() as i64, l.iter().copied().max().unwrap_or(0))
}

fn wrapped_line_widths(text: &str, limit: i64) -> Vec<i64> {
    let widest_char = text
        .chars()
        .map(|c| legend_text_emu(c.encode_utf8(&mut [0; 4])))
        .max()
        .unwrap_or(0);
    if widest_char > limit {
        let mut w: Vec<i64> = text
            .chars()
            .take(3)
            .map(|c| legend_text_emu(c.encode_utf8(&mut [0; 4])))
            .collect();
        w.resize(3, 0);
        return w;
    }
    let space = legend_text_emu(" ");
    let mut done: Vec<i64> = Vec::new();
    let mut cur = -1;
    let put = |piece: &str, cur: &mut i64, done: &mut Vec<i64>| {
        let w = legend_text_emu(piece);
        let next = if *cur < 0 { w } else { *cur + space + w };
        if *cur >= 0 && next > limit {
            done.push(*cur);
            *cur = w;
        } else {
            *cur = next;
        }
    };
    for word in text.split(' ') {
        if legend_text_emu(word) <= limit {
            put(word, &mut cur, &mut done);
            continue;
        }

        if cur >= 0 {
            done.push(cur);
            cur = -1;
        }
        let mut part = String::new();
        for ch in word.chars() {
            let mut next = part.clone();
            next.push(ch);
            if !part.is_empty() && legend_text_emu(&next) > limit {
                put(&part, &mut cur, &mut done);
                done.push(cur);
                cur = -1;
                part = ch.to_string();
            } else {
                part = next;
            }
        }
        put(&part, &mut cur, &mut done);
    }
    done.push(cur.max(0));
    done
}

pub(crate) fn table_label_lines(labels: &[String], avail: i64) -> (i64, bool, i64) {
    let n = labels.len() as i64;
    let last_full = labels.last().map_or(0, |s| legend_text_emu(s));
    if avail < 0 || n < 1 {
        return (1, false, last_full);
    }
    let widest = labels.iter().map(|s| legend_text_emu(s)).max().unwrap_or(0);
    if avail >= n * widest {
        return (1, false, last_full);
    }
    let limit = cat_text_h().max(avail / n);
    let wrapped: Vec<(i64, i64)> = labels.iter().map(|s| wrapped_lines(s, limit)).collect();
    let line = wrapped.iter().map(|w| w.1).max().unwrap_or(1).max(1);
    let fit = |w: i64| (avail / w.max(1)).min(n).max(1);
    if fit(line) > fit(widest) {
        let lines = wrapped.iter().map(|w| w.0).max().unwrap_or(1);
        return (lines, true, wrapped.last().map_or(0, |w| w.1));
    }
    let wrapped: Vec<(i64, i64)> = labels.iter().map(|s| wrapped_lines(s, avail * 5)).collect();
    let lines = wrapped.iter().map(|w| w.0).max().unwrap_or(1);
    (lines, false, wrapped.last().map_or(0, |w| w.1))
}

pub(crate) fn inside_out_room(chart_h: i64, inner: Rect) -> i64 {
    let (_, fy) = crate::chart_lobj::border();
    let below = chart_h - (inner[1] + inner[3] + OUTSIDE);
    below.min(chart_h / 2) - (fy + cat_foot() - cat_text_h())
}

fn first_line(text: &str, limit: i64) -> i64 {
    first_line_size(text, limit).0
}

fn first_line_size(text: &str, limit: i64) -> (i64, i64) {
    let lay = art_layout(text, limit, cat_text_h()).0;
    (
        lay.widths.first().copied().unwrap_or(0),
        lay.heights.first().copied().unwrap_or(cat_text_h()),
    )
}

pub(crate) fn label_extent(i: &LayoutIn, w: i64, room: i64) -> i64 {
    labels_extent(&i.cat_labels, i.cat_pad, w, room)
}

pub(crate) fn labels_extent(labels: &[String], cat_pad: bool, w: i64, room: i64) -> i64 {
    let n = labels.len();
    if n < 2 {
        return cat_text_h();
    }
    let plan = label_plan(labels, w, room, cat_pad);
    let sin45 = sin45();
    let limit = match plan.angle {
        0 if cat_pad => return cat_text_h(),
        0 => {
            let limit = flat_limit(n as i64, w, plan.step);
            let which: Vec<usize> = (0..n).step_by(plan.step as usize).collect();
            let lay = flat_layouts(labels, &which, limit, room);
            return lay
                .iter()
                .map(|l| l.heights.iter().sum::<i64>())
                .max()
                .unwrap_or(cat_text_h());
        }
        _ if cat_pad => i64::MAX,
        45 => limit_45(w, room),
        _ => room,
    };
    (0..n)
        .step_by(plan.step as usize)
        .map(|k| {
            let (fw, pitch) = first_line_size(&labels[k], limit);
            if plan.angle == 45 {
                ((fw + pitch) as f64 * sin45) as i64
            } else {
                fw
            }
        })
        .max()
        .unwrap_or(cat_text_h())
}

pub(crate) fn cat_overhang(i: &LayoutIn, w: i64, plan_w: i64, room: i64) -> (i64, i64) {
    let n = i.cat_labels.len() as i64;
    if n < 1 {
        return (0, 0);
    }
    let widths: Vec<i64> = i.cat_labels.iter().map(|s| legend_text_emu(s)).collect();

    let plan = if i.table.is_some() {
        LabelPlan { angle: 0, step: 1 }
    } else {
        label_plan(&i.cat_labels, plan_w, room, i.cat_pad)
    };
    let slot = w / n;
    let flat_width = |k: usize| {
        if i.table.is_some() {
            return widths[k];
        }
        let limit = flat_limit(n, plan_w, plan.step);
        let lay = flat_layouts(&i.cat_labels, &[k], limit, room);
        lay[0].widths.iter().copied().max().unwrap_or(0)
    };
    let half_width = |k: usize| flat_width(k) / 2;
    let pad = |e: i64| (76_200 + e - slot / 2).max(0);
    let last = ((n - 1) / plan.step * plan.step) as usize;
    match plan.angle {
        0 => (pad(half_width(0)), pad(half_width(last))),
        90 => (0, pad(0)),
        _ => {
            let sin45 = sin45();
            let limit = (plan_w.min(room) as f64 / sin45 - cat_text_h() as f64) as i64;
            let fw = first_line(&i.cat_labels[0], limit);
            (
                pad((sin45 * fw as f64) as i64),
                pad((sin45 * cat_text_h() as f64) as i64),
            )
        }
    }
}

pub(crate) fn reserves(i: &LayoutIn) -> [i64; 4] {
    if i.pie {
        return [0; 4];
    }
    let last = i.val_labels.last().copied().unwrap_or(0);
    let first = i.val_labels.first().copied().unwrap_or(0);
    let widest = i.val_labels.iter().copied().max().unwrap_or(0);
    if i.bar {
        let (lend, rend) = if i.val_rev {
            (last, first)
        } else {
            (first, last)
        };
        let (hl, hr) = if i.val_labels_on {
            (half(lend), half(rend))
        } else {
            (0, 0)
        };
        let cat = i.cat_width + cat_label_gap();
        let near = i.cat_labels_on && !i.crosses_inside;
        let title = if i.side_title { TITLE_EXTRA } else { 0 };

        let (left, right) = match (near, i.far_x, i.side_title) {
            (true, false, _) => (cat + title, hr),
            (true, true, _) => (hl, hr.max(cat) + title),
            (false, false, true) => (0, hr),
            (false, true, true) => (hl, 0),
            (false, _, false) => (hl, hr),
        };
        let bottom = if i.val_labels_on { val_foot() } else { 0 };
        let foot = if i.foot_title { TITLE_EXTRA } else { 0 };
        return [left, 0, right, bottom + foot];
    }
    let mut left = if i.val_labels_on {
        widest + label_gap()
    } else {
        0
    };
    if let Some((head, _)) = i.table {
        left = left.max(head);
    }
    let left = left + if i.side_title { TITLE_EXTRA } else { 0 };
    let top = if i.val_labels_on { half_tick() } else { 0 };
    let bottom = if let Some((_, rows)) = i.table {
        table_base() + table_row() * rows
    } else if i.cat_labels_on && !i.crosses_inside {
        cat_foot()
    } else if i.val_labels_on {
        half_tick()
    } else {
        0
    } + if i.foot_title { TITLE_EXTRA } else { 0 };

    let right = match i.sec_labels.iter().copied().max() {
        Some(w) => i.right_overhang.max(w + label_gap()),
        None => i.right_overhang,
    };
    [left, top, right, bottom]
}

pub(crate) fn inside_cross_reserve(ext: i64, frac: f64, plot_h: i64) -> i64 {
    let d = (frac * plot_h as f64 + 0.5) as i64;
    if d >= ext || frac >= 1.0 {
        return 0;
    }
    let f = d as f64 / plot_h as f64;
    ((ext as f64 - plot_h as f64 * f) / (1.0 - f)) as i64
}

pub(crate) fn value_cross_width(w: i64, frac: f64, len: i64) -> i64 {
    let d = (frac * len as f64 + 0.5) as i64;
    if d >= w {
        return 0;
    }
    if len <= 0 {
        return w;
    }
    let f = d as f64 / len as f64;
    ((w as f64 - len as f64 * f) / (1.0 - f)) as i64
}

pub(crate) fn plot_frame(i: &LayoutIn) -> (Rect, Option<Rect>) {
    let mut root = LObj::new([-OUTSIDE, -OUTSIDE, i.w, i.h]);
    if i.title {
        root.dock([-1, TITLE_H], Dock::Top, 0, [0; 4], true, true);
    }
    let leg = i.legend.as_ref().map(|l| {
        let size = legend_size(l, root.hard[2], root.hard[3]);
        let dock = match l.at {
            LegendAt::Right => Dock::Right,
            LegendAt::Left => Dock::Left,
            LegendAt::Top => Dock::Top,
            LegendAt::Bottom => Dock::Bottom,
        };
        root.dock([size.0, size.1], dock, 0, [0; 4], true, true)
    });
    let mut chart = LObj::new(root.hard);
    let mut plot = LObj::new(chart.dock([-1, -1], Dock::Fill, 0, [0; 4], true, true));
    plot.inset_soft_bounds(PLOT_INSET, PLOT_INSET);
    (plot.soft, leg)
}

pub(crate) fn auto_layout(i: &LayoutIn) -> Layout {
    let (psoft, leg) = plot_frame(i);

    let [mut l, mut t, mut w, mut h] = psoft;
    let side = i.side_title.then_some(AXIS_TITLE);
    let foot = i.foot_title.then_some(AXIS_TITLE);
    if let Some(s) = side {
        w -= s;
        if !i.far_x {
            l += s;
        }
    }
    if let Some(s) = foot {
        h -= s;
        if i.title_top {
            t += s;
        }
    }
    finish(i, [l, t, w, h], psoft, [side, foot], leg)
}

pub(crate) fn axis_layout(i: &LayoutIn, soft: Rect, titles: [Option<i64>; 2]) -> Rect {
    let (fx, fy) = crate::chart_lobj::border();
    let s = [
        soft[0] - fx,
        soft[1] - fy,
        soft[2] + 2 * fx,
        soft[3] + 2 * fy,
    ];
    let mut o = LObj::new(s);
    o.slots[0][1] += fy;
    o.slots[0][3] -= 2 * fy;
    o.slots[1][0] += fx;
    o.slots[1][2] -= 2 * fx;
    o.shrink_to_slots();

    let tv = if i.far_x { Dock::Right } else { Dock::Left };
    let th = if i.far_y || i.title_top {
        Dock::Top
    } else {
        Dock::Bottom
    };
    if let Some(t) = titles[0] {
        o.dock([t, -1], tv, 0, [0; 4], true, true);
    }
    if let Some(t) = titles[1] {
        o.dock([-1, t], th, 1, [0; 4], true, true);
    }
    lay_axes(i, &mut o, s)
}

pub(crate) fn outer_axis_layout(i: &LayoutIn, outer: Rect) -> Rect {
    let mut o = LObj::new(outer);
    lay_axes(i, &mut o, outer)
}

fn inner_axis_layout(i: &LayoutIn, inner: Rect) -> Rect {
    let mut o = LObj::new(inner);
    o.grow = true;
    lay_axes(i, &mut o, inner)
}

pub(crate) fn inner_target_layout(i: &LayoutIn, inner: Rect) -> (Rect, Rect) {
    let g = inner_axis_layout(i, inner);
    let (x, y, w, h) = (g[0] + OUTSIDE, g[1] + OUTSIDE, g[2], g[3]);
    let (mut nx, mut nw) = (x.max(0), w);
    let mut flag = x < 0;
    if i.w < nx + nw {
        nx = i.w - nw;
        flag = true;
    }
    if nx < 0 {
        nw = i.w;
        flag = true;
        nx = 0;
    }
    let ny = y.max(0);
    let mut redo = y < 0 || flag;
    let mut nh = h;
    if i.h < ny + nh {
        redo = true;
    }
    let mut ty = if ny + nh <= i.h { ny } else { i.h - nh };
    if ty < 0 {
        nh = i.h;
        redo = true;
        ty = 0;
    }
    if !redo {
        return (g, inner);
    }
    let held = [nx - OUTSIDE, ty - OUTSIDE, nw, nh];
    (held, outer_axis_layout(i, held))
}

fn lay_axes(i: &LayoutIn, o: &mut LObj, s: Rect) -> Rect {
    let vdock = if i.far_x { Dock::Right } else { Dock::Left };
    let hdock = if i.far_y { Dock::Top } else { Dock::Bottom };
    if i.bar {
        bar_axes(i, o, s, vdock, hdock);
    } else {
        column_axes(i, o, s, vdock, hdock);
    }
    o.hard
}

pub(crate) fn value_box(i: &LayoutIn) -> i64 {
    if !i.val_labels_on {
        return 0;
    }
    let (fx, _) = crate::chart_lobj::border();
    i.val_labels.iter().copied().max().unwrap_or(0) + label_gap() + fx
}

fn column_axes(i: &LayoutIn, o: &mut LObj, s: Rect, vdock: Dock, hdock: Dock) {
    if o.grow {
        column_cat(i, o, s, hdock);
        column_values(i, o, s, vdock);
    } else {
        column_values(i, o, s, vdock);
        column_cat(i, o, s, hdock);
    }
}

fn column_values(i: &LayoutIn, o: &mut LObj, s: Rect, vdock: Dock) {
    let (fx, _) = crate::chart_lobj::border();

    let vpad = if i.val_labels_on {
        cat_line_of(crate::chartfont::fonts().val) / 2
    } else {
        0
    };
    let vbox = value_box(i);
    match i.val_cross {
        Some(frac) => {
            let w = value_cross_width(vbox, frac, s[2]);
            o.dock([w, -1], vdock, 0, [0, 0, vpad, vpad], false, true);
        }
        None => {
            o.dock([vbox, -1], vdock, 0, [0, 0, vpad, vpad], false, true);
        }
    }
    if let Some(w) = i.sec_labels.iter().copied().max() {
        o.dock(
            [w + label_gap() + fx, -1],
            Dock::Right,
            0,
            [0, 0, vpad, vpad],
            false,
            true,
        );
    }
}

fn column_cat(i: &LayoutIn, o: &mut LObj, s: Rect, hdock: Dock) {
    let (_, fy) = crate::chart_lobj::border();

    let plan_w = o.hard[2];

    let room = if o.grow {
        inside_out_room(i.h, s)
    } else {
        o.hard[3] / 2
    };
    let (pl, pr) = cat_pads(i, plan_w, s[2], room);
    if let Some((_, rows)) = i.table {
        let lines = table_label_lines(&i.cat_labels, plan_w - pl).0.max(1);
        let h = table_base() + table_row() * rows + fy + (lines - 1) * cat_text_h();

        let dock_h = if o.grow { h.min(o.hard[3]) } else { h };
        o.dock([-1, dock_h], hdock, 1, [pl, pr, 0, 0], false, true);
        return;
    }
    let h = if i.cat_labels_on {
        let extent = if i.cat_labels.is_empty() {
            cat_text_h()
        } else {
            label_extent(i, plan_w, room).min(room)
        };
        cat_foot() + extent - cat_text_h() + fy
    } else {
        0
    };
    if !i.crosses_inside {
        o.dock([-1, h], hdock, 1, [pl, pr, 0, 0], false, true);
        return;
    }

    let grown = match i.cross_frac {
        Some(frac) if i.cat_labels_on => inside_cross_reserve(h, frac, s[3]),
        _ => 0,
    };
    let size = if grown > 0 { grown } else { h };
    o.dock([-1, size], hdock, 1, [pl, pr, 0, 0], false, grown > 0);
}

fn bar_axes(i: &LayoutIn, o: &mut LObj, s: Rect, vdock: Dock, hdock: Dock) {
    if o.grow {
        bar_cat(i, o, s, vdock);
        bar_values(i, o, hdock);
    } else {
        bar_values(i, o, hdock);
        bar_cat(i, o, s, vdock);
    }
}

fn bar_values(i: &LayoutIn, o: &mut LObj, hdock: Dock) {
    let (fx, fy) = crate::chart_lobj::border();

    let first = i.val_labels.first().copied().unwrap_or(0);
    let last = i.val_labels.last().copied().unwrap_or(0);
    let (lend, rend) = if i.val_rev {
        (last, first)
    } else {
        (first, last)
    };
    let (pl, pr) = if i.val_labels_on {
        (half(lend) + fx, half(rend) + fx)
    } else {
        (0, 0)
    };
    let hbox = if i.val_labels_on { val_foot() + fy } else { 0 };
    o.dock([-1, hbox], hdock, 1, [pl, pr, 0, 0], false, true);
}

fn bar_cat(i: &LayoutIn, o: &mut LObj, s: Rect, vdock: Dock) {
    let (fx, fy) = crate::chart_lobj::border();
    if !i.cat_labels_on {
        return;
    }
    let vbox = i.cat_width + cat_label_gap() + fx;

    let n = i.cat_labels.len().max(1) as i64;
    let vpad = if i.between {
        (fy + cat_text_h() / 2 - s[2] / n / 2).max(0)
    } else {
        fy + cat_text_h() / 2
    };
    let pads = [0, 0, vpad, vpad];
    if !i.crosses_inside {
        o.dock([vbox, -1], vdock, 0, pads, false, true);
        return;
    }
    let grown = match i.cross_frac {
        Some(p) if !i.far_x && !i.side_title => value_cross_width(vbox, p, s[2]),
        _ => 0,
    };
    o.dock([grown.max(0), -1], vdock, 0, pads, false, true);
}

pub(crate) fn cat_pads(i: &LayoutIn, plan_w: i64, len: i64, room: i64) -> (i64, i64) {
    let (fx, _) = crate::chart_lobj::border();
    if let Some((head, _)) = i.table {
        let Some(last) = i.cat_labels.last() else {
            return (fx + head, fx + i.right_overhang);
        };
        let n = i.cat_labels.len() as i64;
        let adj = if i.between { len / n / 2 } else { 0 };

        let (_, _, held) = table_label_lines(&i.cat_labels, plan_w - head - fx);
        let _ = last;
        return (fx + head, (fx + held / 2 - adj).max(0));
    }
    if !i.cat_labels_on {
        return (0, 0);
    }
    if i.cat_labels.is_empty() {
        return (fx + i.left_overhang, fx + i.right_overhang);
    }
    let n = i.cat_labels.len() as i64;
    let plan = label_plan(&i.cat_labels, plan_w, room, i.cat_pad);
    let last = ((n - 1) / plan.step * plan.step) as usize;
    let (el, er) = match plan.angle {
        0 => {
            let limit = flat_limit(n, plan_w, plan.step);
            if i.cat_pad {
                (
                    legend_text_emu(&i.cat_labels[0]) / 2,
                    legend_text_emu(&i.cat_labels[last]) / 2,
                )
            } else {
                let which: Vec<usize> = (0..n as usize).step_by(plan.step as usize).collect();
                let lay = flat_layouts(&i.cat_labels, &which, limit, room);
                let widest = |k: usize| {
                    let at = which.iter().position(|&x| x == k).unwrap_or(0);
                    lay[at].widths.iter().copied().max().unwrap_or(0)
                };
                (widest(0) / 2, widest(last) / 2)
            }
        }
        90 => (0, 0),
        _ => {
            let limit = if i.cat_pad {
                i64::MAX
            } else {
                limit_45(plan_w, room)
            };
            let fw = first_line(&i.cat_labels[0], limit);
            (
                (cos45() * fw as f64) as i64,
                (cos45() * cat_text_h() as f64) as i64,
            )
        }
    };
    let adj = if !i.between {
        0
    } else {
        if matches!(plan.angle, 0 | 90) {
            len / n / 2
        } else {
            let units = if i.cat_bins > 0 { i.cat_bins } else { n };
            (len as f64 * 0.5 / units as f64) as i64
        }
    };
    ((fx + el - adj).max(0), (fx + er - adj).max(0))
}

fn cat_line_of(f: crate::chartfont::TextFont) -> i64 {
    crate::chartfont::line_emu(f)
}

fn square([x, y, w, h]: Rect) -> Rect {
    let side = w.min(h).max(0);
    [
        x + ((w >> 1) - (side >> 1)),
        y + ((h >> 1) - (side >> 1)),
        side,
        side,
    ]
}

pub(crate) fn radar_layout(soft: Rect, labels: (i64, i64)) -> (Rect, Rect) {
    let b = TITLE_EXTRA;
    let r0 = soft;
    let (dx, dy) = ((labels.0 - b).max(0), (labels.1 - b).max(0));
    let (w, h) = (r0[2] - 2 * dx, r0[3] - 2 * dy);
    let [x, y, side, _] = square([r0[0] + dx, r0[1] + dy, w, h]);
    let outer = r0[2].min(r0[3]);
    ([x, y, outer, outer], [x, y, side, side])
}

pub(crate) fn radar_label_box(labels: &[String], hard: (i64, i64)) -> (i64, i64) {
    if labels.is_empty() {
        return (0, 0);
    }
    let fh = cat_text_h();
    let pad_and_spacer = 2 * crate::chartfont::label_offset_emu(crate::chartfont::fonts().cat)
        + crate::chart_lobj::border().1 / 4;
    let line_box = fh + pad_and_spacer / 2;
    let limit = hard.0 / 4;

    let left = hard.1 - 2 * (line_box - TITLE_EXTRA).max(0);
    let height = (2 * left / labels.len() as i64).max(fh);
    let widest = labels
        .iter()
        .map(|t| art_lines(t, limit, height).into_iter().max().unwrap_or(0))
        .max()
        .unwrap_or(0);
    (widest + 76_851, line_box)
}

pub(crate) fn art_lines(text: &str, w: i64, h: i64) -> Vec<i64> {
    art_layout(text, w, h).0.widths
}

fn symbol_face(cp: u32) -> Option<u8> {
    let t = crate::chart_symbol_faces::SYMBOL_FACES;
    match t.get(t.partition_point(|r| r.1 < cp)) {
        Some(&(first, _, f)) if first <= cp => Some(f),
        _ => None,
    }
}

fn script_rule(cp: u32) -> Option<u8> {
    let t = crate::chart_script_faces::SCRIPT_FACES;
    match t.get(t.partition_point(|r| r.1 < cp)) {
        Some(&(first, _, r)) if first <= cp => Some(r),
        _ => None,
    }
}

fn text_pitch(text: &str) -> i64 {
    let cat = crate::chartfont::fonts().cat;

    type Face = (i64, i64, i64, i64, bool);
    const YU: Face = (2017, 619, 0, 2048, true);
    const MALGUN: Face = (2229, 495, 0, 2048, true);
    const JHENGHEI: Face = (2203, 521, 0, 2048, true);
    const YAHEI: Face = (2167, 536, 0, 2048, true);
    const MS_GOTHIC: Face = (220, 36, 0, 256, true);
    const ARIAL: Face = (1854, 434, 67, 2048, false);
    const TAHOMA: Face = (2049, 423, 0, 2048, false);

    const CAMBRIA_MATH: Face = (1946, 455, 0, 2048, false);
    const SEGOE_SYMBOL: Face = (2210, 514, 0, 2048, false);

    const MANGAL: Face = (2542, 898, 0, 2048, false);
    const NIRMALA: Face = (2210, 514, 0, 2048, false);
    const SYLFAEN: Face = (2062, 635, 0, 2048, false);
    const EBRIMA: Face = (2210, 514, 57, 2048, false);
    const CALIBRI: Face = (1950, 550, 0, 2048, false);
    const MONGOLIAN_BAITI: Face = (1729, 449, 184, 2048, false);
    type Rule = (&'static [Face], bool);
    const RULES: [Rule; 9] = [
        (&[MANGAL], false),
        (&[NIRMALA], false),
        (&[SYLFAEN], true),
        (&[ARIAL], false),
        (&[EBRIMA, CALIBRI], false),
        (&[SEGOE_SYMBOL], true),
        (&[MONGOLIAN_BAITI], false),
        (&[MS_GOTHIC], false),
        (&[CALIBRI], true),
    ];
    let units = |face: Face| {
        let (asc, desc, gap, upm, fe) = face;
        let u = |m: i64| (m as f64 * 200.0 * cat.size / upm as f64 + 0.5).floor() as i64;
        let (a, d) = (u(asc), u(desc));
        if fe {
            let m = (15 * (a + d) + 50) / 100;
            (a + m, d + m)
        } else {
            (a + u(gap), d)
        }
    };
    let (mut latin, mut other, mut asc, mut desc) = (false, false, 0i64, 0i64);
    for c in text.chars() {
        let cp = c as u32;
        let face = match cp {
            _ if script_rule(cp).is_some() => {
                let (faces, joins) = RULES[usize::from(script_rule(cp).unwrap_or(0))];
                for &f in faces {
                    let (a, d) = units(f);
                    asc = asc.max(a);
                    desc = desc.max(d);
                }
                latin |= joins;
                other = true;
                continue;
            }
            0x0590..=0x08FF | 0xFB1D..=0xFDFF | 0xFE70..=0xFEFF => ARIAL,
            0x0E00..=0x0E7F => TAHOMA,
            0xFF01..=0xFF5E => MS_GOTHIC,
            0x1100..=0x11FF
            | 0x2E80..=0x2FDF
            | 0x3000..=0x30FF
            | 0x3130..=0x318F
            | 0x31F0..=0x33FF
            | 0x3400..=0x4DBF
            | 0x4E00..=0x9FFF
            | 0xAC00..=0xD7AF
            | 0xF900..=0xFAFF
            | 0xFE30..=0xFE4F
            | 0xFF00..=0xFFEF => {
                let table = crate::chart_ea_faces::EA_FALLBACK;
                match table.get(table.partition_point(|r| r.1 < cp)) {
                    Some(&(first, _, f)) if first <= cp => {
                        [YU, MALGUN, JHENGHEI, YAHEI][f as usize]
                    }
                    _ => YU,
                }
            }
            _ if symbol_face(cp).is_some() => {
                let face = [CAMBRIA_MATH, SEGOE_SYMBOL][usize::from(symbol_face(cp).unwrap_or(0))];
                let (a, d) = units(face);
                asc = asc.max(a);
                desc = desc.max(d);
                latin = true;
                other = true;
                continue;
            }

            _ if c.is_whitespace() || c == '\u{200b}' || (cp < 0x80 && !c.is_alphanumeric()) => {
                continue
            }
            _ => {
                latin = true;
                continue;
            }
        };
        let (a, d) = units(face);
        asc = asc.max(a);
        desc = desc.max(d);
        other = true;
    }
    if !other {
        return cat_text_h();
    }
    if latin {
        let (a, d) = crate::chartfont::asc_desc_units(cat);
        asc = asc.max(a);
        desc = desc.max(d);
    }
    crate::chartfont::emu(asc + desc)
}

struct Lines {
    widths: Vec<i64>,
    heights: Vec<i64>,
}

fn art_layout(text: &str, w: i64, h: i64) -> (Lines, bool) {
    let chars: Vec<char> = text.chars().collect();
    let wid: Vec<i64> = chars
        .iter()
        .map(|c| legend_text_emu(c.encode_utf8(&mut [0; 4])))
        .collect();
    let sum_rounded = chars.iter().any(|&c| c as u32 > 0x17F);
    let n = chars.len() + 1;
    let adv = |i: usize| wid.get(i).copied().unwrap_or(0);
    let is_nl = |i: usize| i < chars.len() && (chars[i] == '\r' || chars[i] == '\n');
    let is_ws = |i: usize| i < chars.len() && chars[i].is_whitespace();
    let ell = legend_text_emu("\u{2026}");
    let (mut lines, mut heights, mut used, mut cut) = (Vec::new(), Vec::new(), 0, false);
    let (mut start, mut i, mut width, mut cap, mut ellipsis) = (0usize, 0usize, 0i64, w, 0i64);
    while i < n {
        let mut next = i + 1;
        width += adv(i);
        if !is_nl(i) && next < n {
            i = next;
            if cap >= width {
                continue;
            }
            while start < next {
                let k = next - 1;
                if is_ws(k) || (width <= cap && chars[k] == '-') {
                    break;
                }
                width -= adv(k);
                next -= 1;
            }
            while next < n && is_ws(next) {
                width += adv(next);
                next += 1;
            }
            if next == start {
                cut = true;
                while next < n && width <= cap {
                    width += adv(next);
                    next += 1;
                }
                if start + 1 < next {
                    next -= 1;
                    width -= adv(next);
                }
            }
        }

        let pitch = |a: usize, b: usize| text_pitch(&chars[a.min(b)..b].iter().collect::<String>());
        let own = pitch(start, next.min(chars.len()));
        let after = pitch(next.min(chars.len()), chars.len());
        if ellipsis == 0 && used + own + after > h && next + 1 < n {
            ellipsis = ell;
            cap = (cap - ell).max(0);
            width = 0;
            i = start;
            continue;
        }
        let mut shown = next;
        while start < shown && is_ws(shown - 1) {
            width -= adv(shown - 1);
            shown -= 1;
        }

        let end = shown.min(chars.len());
        if sum_rounded && start < end {
            let line: String = chars[start..end].iter().collect();
            width = legend_text_emu(&line);
        }
        lines.push(width);
        heights.push(own);
        used += own;
        if used + after > h {
            cut = ellipsis != 0 || next < n;
            break;
        }
        width = 0;
        start = next;
        i = next;
    }

    let cut = cut || (lines.len() > 1 && lines.last() == Some(&0));
    (
        Lines {
            widths: lines,
            heights,
        },
        cut,
    )
}

fn finish(
    i: &LayoutIn,
    [l, t, w, h]: Rect,
    soft: Rect,
    titles: [Option<i64>; 2],
    legend: Option<Rect>,
) -> Layout {
    if let Some(labels) = &i.radar {
        let p = radar_layout(soft, radar_label_box(labels, (soft[2], soft[3])));
        return Layout {
            plot: p.0,
            inside: p.1,
            legend,
        };
    }
    if i.pie {
        let p = square([l, t, w, h]);
        return Layout {
            plot: p,
            inside: p,
            legend,
        };
    }
    Layout {
        plot: [l, t, w, h],
        inside: axis_layout(i, soft, titles),
        legend,
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn art_first_line_keeps_sup() {
        let t = "Sup Supercalifragilistic x";
        assert_eq!(first_line(t, 1_163_521), legend_text_emu("Sup"));
        assert!(first_line(t, 1_238_637) > legend_text_emu("Sup Supercalifragilistic") - 10);
    }

    #[test]
    fn art_layout_cut_flag_and_remeasure() {
        assert_eq!(art_layout("Qa", 12_700, 2_000_000).0.widths.len(), 3);
        assert!(art_layout("Qa", 12_700, 2_000_000).1);
        assert!(art_layout("Qa", 12_700, 400_000).1);
        assert_eq!(art_layout("Qa", 63_500, 400_000).0.widths.len(), 2);
        let labels = vec!["Qa".to_string()];
        let lay = flat_layouts(&labels, &[0], 12_700, 400_000);
        assert_eq!(lay[0].widths.len(), 2);
    }

    #[test]
    fn square_centres_either_way() {
        assert_eq!(
            square([88_900, 88_900, 2_260_600, 4_800_600]),
            [88_900, 1_358_900, 2_260_600, 2_260_600]
        );
        assert_eq!(
            square([88_900, 482_219, 2_260_600, 4_407_281])[1],
            1_555_559
        );
        assert_eq!(square([0, 0, 7, 4]), [1, 0, 4, 4]);
    }

    #[test]
    fn art_lines_traced() {
        let cases: [(&str, &[i64]); 5] = [
            ("Supercalifragilistic", &[257684, 100140]),
            ("Incomprehensibilities", &[220282, 99568]),
            ("Internationalization", &[233808, 126302]),
            ("aa bb cc dd", &[123826, 130556]),
            ("Antidisestablishment", &[198311, 149987]),
        ];
        for (t, want) in cases {
            assert_eq!(art_lines(t, 260_422, 405_749), want, "{t}");
        }
    }

    #[test]
    fn label_plan_long_names() {
        let labels: Vec<String> = [
            "Northern region",
            "Southern region",
            "Eastern region",
            "Western region",
            "Central region",
            "Overseas region",
        ]
        .iter()
        .map(|s| s.to_string())
        .collect();
        let p = label_plan(&labels, 2_117_889, 2_003_996 / 2, false);
        assert_eq!((p.angle, p.step), (45, 1));
        let p = label_plan(&labels, 4_611_719, 2_892_996 / 2, false);
        assert_eq!((p.angle, p.step), (0, 1));
        let limit = flat_limit(6, 4_611_719, 1);
        assert_eq!(limit, 768_619);
        let all: Vec<usize> = (0..labels.len()).collect();
        let lay = flat_layouts(&labels, &all, limit, 1_446_498);
        let lines = lay.iter().map(|l| l.widths.len()).max();
        let widest = lay.iter().flat_map(|l| l.widths.iter().copied()).max();
        assert_eq!((lines, widest), (Some(2), Some(762_828)));
        assert_eq!(legend_text_emu("Overseas"), 474_347);
    }

    #[test]
    fn label_plan_wrapped_labels_stay_at_45() {
        let labels: Vec<String> = [
            "Internationalization day",
            "Fig Internationalization",
            "Pneumonoultra micro",
            "Sup Supercalifragilistic x",
            "a Incomprehensibilities",
            "Electroencephalography x y",
        ]
        .iter()
        .map(|s| s.to_string())
        .collect();
        let p = label_plan(&labels, 3_341_719, 932_338, false);
        assert_eq!((p.angle, p.step), (45, 1));
    }

    #[test]
    fn label_plan_counts_named_labels_and_samples_many() {
        let mut labels = vec![String::new(); 10];
        for k in [0, 2, 4, 6] {
            labels[k] = "Southern".to_string();
        }
        let wide = legend_text_emu("Southern");
        let p = label_plan(&labels, 4 * wide, 1_000_000, false);
        assert_eq!((p.angle, p.step), (0, 1));
        let p = label_plan(&labels, 4 * wide - 1, 1_000_000, false);
        assert_ne!((p.angle, p.step), (0, 1));

        for (n, skipped) in [(100, false), (101, true)] {
            let mut labels = vec!["a".to_string(); n];
            labels[3] = "Wwwwwwwwwwwwwwwwwwww".to_string();
            let w = n as i64 * legend_text_emu("a");
            let p = label_plan(&labels, w, 1_500_000, false);
            assert_eq!((p.angle, p.step) == (0, 1), skipped, "{n}");
        }
    }

    #[test]
    fn label_plan_matches_traced_widths() {
        let labels: Vec<String> = ["Apple", "Pear", "Fig", "Kiwi", "Plum"]
            .iter()
            .map(|s| s.to_string())
            .collect();
        let want = [
            (0, 0, 5),
            (20, 0, 5),
            (30, 45, 3),
            (40, 90, 2),
            (50, 45, 2),
            (60, 45, 2),
            (70, 90, 1),
            (80, 90, 1),
            (100, 45, 1),
        ];
        for (pt, angle, step) in want {
            let p = label_plan(&labels, pt * 12_700, 218_504, false);
            assert_eq!((p.angle, p.step), (angle, step), "{pt} pt");
        }
    }

    #[test]
    fn first_line_matches_traced_wraps() {
        assert_eq!(first_line("Apple", 154_007), 67_818);
        assert_eq!(first_line("Fig", 154_007), 146_495);
        assert_eq!(first_line("Plum", 154_007), 67_183);
        assert_eq!(first_line("Plum", 218_504), 97_981);
        assert_eq!(first_line("Kiwi", 218_504), 206_566);
    }

    #[test]
    fn side_labels_step_when_the_axis_is_short() {
        assert_eq!(side_label_step(5, 746_503, 155_004), 2);

        assert_eq!(side_label_step(3, 712_448, 155_004), 1);

        assert_eq!(side_label_step(5, 0, 155_004), 5);
        assert_eq!(side_label_step(1, 0, 155_004), 1);

        assert_eq!(side_label_step(4, 400_000, 90_000), 1);
        assert_eq!(side_label_step(4, 399_999, 90_000), 2);
    }

    #[test]
    fn value_cross_width_traced() {
        let len = 2_631_186;
        let frac = 0.5 / 1519.0;
        assert_eq!((frac * len as f64 + 0.5) as i64, 866);
        assert_eq!(value_cross_width(393_859, frac, len), 393_122);

        assert_eq!(value_cross_width(393_859, 0.0, len), 393_859);
        assert_eq!(value_cross_width(393_859, 0.5, len), 0);

        let f = 393_858.0 / len as f64;
        assert_eq!(value_cross_width(393_859, f, len), 1);
    }

    #[test]
    fn legend_rows_when_entries_pass_the_room() {
        let texts = vec![743_207, 752_859, 676_469, 714_759, 376_049, 684_024];
        let line = legend_line_h();
        let l = |w: i64| {
            let li = LegendIn {
                at: LegendAt::Top,
                lines: vec![1; texts.len()],
                texts: texts.clone(),
                key: 139_503,
            };
            legend_size(&li, w * 12_700, 200 * 12_700)
        };
        assert_eq!(l(180), (2 * 892_362, 2 * (line + 76_200) + line + 57_150));
        assert_eq!(l(260), (3 * 892_362, (line + 76_200) + line + 57_150));
    }

    #[test]
    fn rotated_box_bounds() {
        assert_eq!(
            max_rotated_box(-45.0, 3_342_743, 932_338, 155_004),
            (1_163_521, 155_004)
        );
        assert_eq!(
            max_rotated_box(45.0, 3_342_743, 932_338, 155_004),
            (1_163_521, 155_004)
        );
        assert_eq!(limit_45(372_653, 600_000), 372_006);
        assert_eq!(
            max_rotated_box(-45.0, 3_342_743, 932_338, -1),
            (659_262, 659_262)
        );
        assert_eq!(max_rotated_box(-45.0, 100, -1, 5), (141, 141));
        assert_eq!(max_rotated_box(0.0, 100, 50, 5), (100, 50));
        assert_eq!(max_rotated_box(90.0, 100, 50, 5), (50, 100));
    }
}
