use crate::chart_3d::View3d;
use crate::chart_layout::{
    cat_foot, cat_label_gap, cat_text_h, label_extent, label_gap, side_label_step,
    value_cross_width, LayoutIn, Rect,
};
use crate::chart_lobj::{border, Dock, LObj};
use crate::chartparts::legend_text_emu;

pub(crate) struct Names {
    pub series_width: i64,

    pub series_labels: Vec<String>,
}

pub(crate) fn axis_layout_3d(
    i: &LayoutIn,
    view: &View3d,
    soft: Rect,
    titles: [Option<i64>; 2],
    names: &Names,
) -> Rect {
    let (fx, fy) = border();
    let s = [
        soft[0] - fx,
        soft[1] - fy,
        soft[2] + 2 * fx,
        soft[3] + 2 * fy,
    ];
    let vol = (s[2], s[3]);
    let mut o = LObj::new(s);

    o.slots[0][1] += fy;
    o.slots[0][3] -= 2 * fy;
    o.slots[1][0] += fx;
    o.slots[1][2] -= 2 * fx;
    o.shrink_to_slots();

    let hard0 = (o.hard[2], o.hard[3]);

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
    let val_font = crate::chartfont::fonts().val;
    let cat_font = crate::chartfont::fonts().cat;
    let default_pos = view.kind1_pos(vol);
    let near = default_pos == 0x14;
    let widest_val = i.val_labels.iter().copied().max().unwrap_or(0);
    let vpad = if i.val_labels_on {
        crate::chartfont::line_emu(val_font) / 2
    } else {
        0
    };
    let half_cat = crate::chartfont::line_emu(cat_font) / 2;
    let vert_sides = view.vertical_sides();
    let vertical = |dx: i64, dy: i64| dx * 2 / 3 <= dy;
    let dock_side = |pos: i32| if pos == 0x14 { Dock::Left } else { Dock::Right };

    if view.bar {
        if i.cat_labels_on {
            let pos = if near != i.far_x { 0x14 } else { 0x12 };
            let vbox = i.cat_width + cat_label_gap() + fx;

            let vbox = if i.crosses_inside {
                match i.cross_frac {
                    Some(p) if !i.far_x && !i.side_title => value_cross_width(vbox, p, s[2]),
                    _ => 0,
                }
                .max(0)
            } else {
                vbox
            };
            let n = i.cat_labels.len().max(1) as i64;
            let pad = if i.between {
                (fy + cat_text_h() / 2 - s[2] / n / 2).max(0)
            } else {
                fy + cat_text_h() / 2
            };
            o.dock([vbox, -1], dock_side(pos), 0, [0, 0, pad, pad], false, near);
        }

        let pos = if i.far_y { 0x11 } else { 0x13 };
        let (dx, dy) = view.edge_box(2, pos, vol, hard0.0, hard0.1);
        if i.val_labels_on {
            if vertical(dx, dy) {
                let w = widest_val + crate::chartfont::line_emu(val_font) / 2 + fx;

                o.dock([w, -1], dock_side(vert_sides.0), 3, [0; 4], false, true);
            } else {
                let first = i.val_labels.first().copied().unwrap_or(0);
                let last = i.val_labels.last().copied().unwrap_or(0);

                let (first, last) = if i.val_rev {
                    (last, first)
                } else {
                    (first, last)
                };
                let h = crate::chartfont::foot_emu(val_font) + fy;
                let dock = if i.far_y { Dock::Top } else { Dock::Bottom };
                o.dock(
                    [-1, h],
                    dock,
                    1,
                    [first / 2 + fx, last / 2 + fx, 0, 0],
                    false,
                    true,
                );
            }
        }
        return o.hard;
    }

    let dock_value = |o: &mut LObj| {
        if i.val_labels_on {
            let pos = if near != i.far_x { 0x14 } else { 0x12 };
            let vbox = widest_val + label_gap() + fx;
            o.dock(
                [vbox, -1],
                dock_side(pos),
                0,
                [0, 0, vpad, vpad],
                false,
                near,
            );
        }
    };

    if view.depth_axis {
        dock_value(&mut o);

        let pos = 0x13;
        let (dx, dy) = view.edge_box(3, pos, vol, hard0.0, hard0.1);
        if vertical(dx, dy) {
            let w = names.series_width + half_cat + fy / 4;

            let side = dock_side(vert_sides.1);
            o.dock([w, -1], side, 4, [0; 4], false, true);
        } else {
            let (hdx, _) = view.edge_box(3, pos, vol, o.hard[2], o.hard[3]);
            let plan_w = hdx * 9 / 10;
            let room = o.hard[3] / 2;
            let extent = if names.series_labels.is_empty() {
                cat_text_h()
            } else {
                crate::chart_layout::labels_extent(&names.series_labels, false, plan_w, room)
                    .min(room)
            };
            let h =
                extent + (half_cat + fy / 4 + crate::chartfont::line_emu(cat_font) - cat_text_h());
            o.dock([-1, h], Dock::Bottom, 4, [0; 4], false, true);
        }
    }

    let pos = if i.far_y { 0x11 } else { 0x13 };
    let (dx, dy) = view.edge_box(2, pos, vol, hard0.0, hard0.1);

    let (dx_now, dy_now) = view.edge_box(2, pos, vol, o.hard[2], o.hard[3]);
    if vertical(dx, dy) {
        let widths: Vec<i64> = i.cat_labels.iter().map(|t| legend_text_emu(t)).collect();
        let step = side_label_step(widths.len() as i64, dy_now, cat_text_h());
        let widest = widths
            .iter()
            .step_by(step as usize)
            .copied()
            .max()
            .unwrap_or(0);
        o.dock(
            [widest + half_cat + fx, -1],
            dock_side(vert_sides.0),
            3,
            [0, 0, 0, 0],
            false,
            true,
        );
    } else {
        let plan_w = dx_now * 9 / 10;
        let room = o.hard[3] / 2;
        let h = if i.cat_labels_on && !i.crosses_inside {
            let extent = if i.cat_labels.is_empty() {
                cat_text_h()
            } else {
                label_extent(i, plan_w, room).min(room)
            };
            cat_foot() + extent - cat_text_h() + fy
        } else if i.val_labels_on {
            fy + crate::chart_layout::half_tick()
        } else {
            0
        };
        let dock = if i.far_y { Dock::Top } else { Dock::Bottom };

        let (pl, pr) = crate::chart_layout::cat_pads(i, plan_w, s[2], room);
        o.dock([-1, h], dock, 1, [pl, pr, 0, 0], false, true);
    }
    if !view.depth_axis {
        dock_value(&mut o);
    }
    o.hard
}
