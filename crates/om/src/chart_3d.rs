use xlcalc::drawing::{BagVal, Chart};

const PAD: f64 = 0.014;

const PI: f64 = 31_415_927.0 / 10_000_000.0;

struct Walls {
    left_x: f64,
    left_z: f64,
    right_x: f64,
    right_z: f64,
    z_end: f64,
    z_off: f64,
    pos_value: i32,
}

pub(crate) struct View3d {
    pub rotation: f64,
    pub elevation: f64,

    pub perspective: f64,
    pub depth_percent: f64,

    pub right_angle: bool,

    pub depth_axis: bool,

    pub bar: bool,
    pub series: usize,
    pub categories: usize,
}

fn num(c: &Chart, k: &str, def: f64) -> f64 {
    match c.bag.get(k) {
        Some(BagVal::I(n)) => f64::from(*n),
        Some(BagVal::D(v)) => *v,
        _ => def,
    }
}

impl View3d {
    pub(crate) fn of(c: &Chart, series: usize, categories: usize) -> View3d {
        View3d {
            rotation: num(c, "Chart.Rotation", 20.0),
            elevation: num(c, "Chart.Elevation", 15.0),
            perspective: num(c, "Chart.Perspective", 30.0),
            depth_percent: num(c, "Chart.DepthPercent", 100.0),
            right_angle: matches!(c.bag.get("Chart.RightAngleAxes"), Some(BagVal::B(true))),
            depth_axis: c.chart_type == -4100,
            bar: matches!(c.chart_type, 60..=62),

            series: if matches!(c.chart_type, 55 | 56 | 61 | 62) {
                1
            } else {
                series.max(1)
            },
            categories: categories.max(1),
        }
    }

    fn volume(&self, w: i64, h: i64) -> (f64, f64, f64) {
        let (s, n) = (self.series as f64, self.categories as i32);
        let (depth, rows) = if self.depth_axis {
            (s * 100.0 * 0.5, (n + self.series as i32) / 2)
        } else {
            let slot = 10_000.0 / (100.0 * s + 150.0);
            ((1.5 * slot + slot) * 0.5, n)
        };
        let span = f64::from(n) * 50.0;

        let aspect = f64::from((h + 152_400) as f32 / (w + 152_400) as f32);
        let height = f64::from(rows) * 50.0 / span * 100.0 * aspect;
        let mut z = depth / span * 100.0 * self.depth_percent / 100.0;
        if self.bar {
            z = height * z / 100.0;
        }
        (100.0, height, z.max(1.0))
    }

    fn walls(&self, z: f64) -> Walls {
        let x = 100.0;
        let rot = self.rotation;
        let quarter = if !(90.0..360.0).contains(&rot) {
            0
        } else if rot < 180.0 {
            1
        } else if rot < 270.0 {
            2
        } else {
            3
        };
        let pad = PAD;
        match quarter {
            0 => Walls {
                left_x: -pad,
                left_z: z,
                right_x: x,
                right_z: -pad,
                z_end: -pad,
                z_off: 0.0,
                pos_value: 0x14,
            },
            1 => Walls {
                left_x: x,
                left_z: z + pad,
                right_x: -pad,
                right_z: 0.0,
                z_end: z + pad,
                z_off: z,
                pos_value: 0x12,
            },
            2 => Walls {
                left_x: x + pad,
                left_z: 0.0,
                right_x: 0.0,
                right_z: z + pad,
                z_end: z + pad,
                z_off: z,
                pos_value: 0x12,
            },
            _ => Walls {
                left_x: 0.0,
                left_z: -pad,
                right_x: x + pad,
                right_z: z,
                z_end: -pad,
                z_off: 0.0,
                pos_value: 0x14,
            },
        }
    }

    fn fov(&self) -> f64 {
        if self.perspective == 0.0 {
            0.1
        } else {
            self.perspective / 2.0
        }
    }

    fn screen(&self, w: i64, h: i64) -> impl Fn(f64, f64, f64) -> (i64, i64) {
        let f32r = |x: f64| f64::from(x as f32);
        let (x, y, z) = self.volume(w, h);
        let walls = self.walls(z);
        let c = (x / 2.0, y / 2.0, z / 2.0);
        let mut verts = Vec::with_capacity(8);

        for vz in [walls.right_z, walls.left_z] {
            for vy in [y + PAD, 0.0] {
                for vx in [walls.right_x, walls.left_x] {
                    verts.push((f32r(vx - c.0), f32r(vy - c.1), f32r(vz - c.2)));
                }
            }
        }
        let (rot, elev, fov) = (self.rotation, self.elevation, self.fov());

        let (s1, c1) = (-rot / 360.0 * 2.0 * PI).sin_cos();
        let (s2, c2) = (elev / 360.0 * 2.0 * PI).sin_cos();
        let ty = (fov / 360.0 * 2.0 * 0.5 * PI).tan();
        let tx = ty * w as f64 / h as f64;
        let (mut top, mut right) = (f64::MIN, f64::MIN);
        let (mut bottom, mut left) = (f64::MAX, f64::MAX);
        for &(vx, vy, vz) in &verts {
            let zp = f32r(vz * c1 - vx * s1);
            let xp = f32r(vz * s1 + vx * c1);
            let zz = f32r(zp * c2 - vy * s2);
            let yy = f32r(zp * s2 + vy * c2);
            top = top.max(yy + zz * ty);
            bottom = bottom.min(yy - zz * ty);
            right = right.max(xp + zz * tx);
            left = left.min(xp - zz * tx);
        }
        let (my, mx) = ((top - bottom) * PAD, (right - left) * PAD);
        bottom -= my;
        top += my;
        left -= mx;
        right += mx;
        let dy = (top - bottom) / (2.0 * ty);
        let dx = (right - left) / (2.0 * tx);
        let eye = (
            f32r(dx * tx + left),
            f32r(dy * ty + bottom),
            f32r(dy.max(dx)),
        );
        let (r, e) = (rot.to_radians(), elev.to_radians());
        let scale = (h as f64 / 914_400.0 / 2.0) / (fov / 2.0).to_radians().tan();
        let emu = |v: f64| ((v * 914_400.0 + 1e7 + 0.5) as i64) - 10_000_000;
        move |vx: f64, vy: f64, vz: f64| {
            let (px, py, pz) = (vx - c.0, vy - c.1, vz - c.2);
            let qx = px * r.cos() - pz * r.sin();
            let qy = px * r.sin() * e.sin() + py * e.cos() + pz * r.cos() * e.sin();
            let qz = px * r.sin() * e.cos() - py * e.sin() + pz * r.cos() * e.cos();
            let d = eye.2 - qz;
            (emu(scale * (qx - eye.0) / d), emu(scale * (qy - eye.1) / d))
        }
    }

    fn value_axis_box(&self, w: i64, h: i64, at_max: bool) -> (i64, i64) {
        if self.bar {
            return self.x_edge_box(w, h, at_max);
        }
        let (_, y, z) = self.volume(w, h);
        let walls = self.walls(z);
        let screen = self.screen(w, h);

        let pos = if at_max {
            0x14 + 0x12 - walls.pos_value
        } else {
            walls.pos_value
        };
        let (ax, az) = if pos == 0x14 {
            (walls.left_x, walls.left_z)
        } else {
            (walls.right_x, walls.right_z)
        };
        let (pa, pb) = (screen(ax, 0.0, az), screen(ax, y, az));
        ((pb.0 - pa.0).abs(), (pb.1 - pa.1).abs())
    }

    pub(crate) fn value_axis(&self, w: i64, h: i64, at_max: bool) -> (i64, bool) {
        if self.right_angle {
            return (self.oblique_value_axis_len(w, h), self.bar);
        }
        let (dx, dy) = self.value_axis_box(w, h, at_max);
        if dx * 2 / 3 <= dy {
            (dy, false)
        } else {
            (dx, true)
        }
    }

    fn x_edge_box(&self, w: i64, h: i64, at_max: bool) -> (i64, i64) {
        let (x, y, z) = self.volume(w, h);
        let walls = self.walls(z);
        let screen = self.screen(w, h);
        let (yy, zz) = if at_max {
            (0.0, walls.z_end)
        } else {
            (y + PAD, z - walls.z_off)
        };
        let (pa, pb) = (screen(0.0, yy, zz), screen(x, yy, zz));
        ((pb.0 - pa.0).abs(), (pb.1 - pa.1).abs())
    }

    pub(crate) fn category_axis_rise(&self, w: i64, h: i64, at_max: bool) -> Option<i64> {
        if self.right_angle {
            return None;
        }
        Some(self.x_edge_box(w, h, at_max).1)
    }
}

impl View3d {
    fn oblique_value_axis_len(&self, w: i64, h: i64) -> i64 {
        let f32r = |x: f64| f64::from(x as f32);
        let (x, y, z) = self.volume(w, h);
        let (wf, hf) = (w as f64, h as f64);

        let ppu = f32r(f32r(wf / 100.0) * 72.0 / 914_400.0);
        let pad = 2.0 / ppu;
        let face_w = x + PAD + 2.0 * pad;
        let face_h = y + PAD + 2.0 * pad;
        let depth = z + PAD + 0.2 + 2.0 * pad;
        let turn = |deg: f64| (deg / 360.0 * 6.283_185_4).sin();
        let (across, up) = (turn(self.rotation), turn(self.elevation));
        let half_w = ((depth * across).abs() + face_w) / 2.0;
        let half_h = ((depth * up).abs() + face_h) / 2.0;

        let fit = if hf / wf <= half_h / half_w {
            hf / (2.0 * half_h)
        } else {
            wf / (2.0 * half_w)
        };
        ((if self.bar { x } else { y }) * fit).round() as i64
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    fn view(rot: f64, elev: f64, depth_axis: bool, series: usize, categories: usize) -> View3d {
        View3d {
            rotation: rot,
            elevation: elev,
            perspective: 30.0,
            depth_percent: 100.0,
            right_angle: false,
            depth_axis,
            bar: false,
            series,
            categories,
        }
    }

    #[test]
    fn oblique_axis_keeps_the_height() {
        let mut v = view(20.0, 15.0, false, 2, 5);
        v.right_angle = true;
        let len = v.value_axis(3_092_266, 2_260_600, false).0;
        assert!(len / 155_004 - 2 >= 10, "{len}");

        assert!(len < 2_260_600);
        let mut d = view(20.0, 15.0, true, 2, 5);
        d.right_angle = true;
        let n = d.value_axis(3_092_266, 2_260_600, false).0 / 155_004 - 2;
        assert!((5..10).contains(&n), "{n}");

        let mut b = view(200.0, 15.0, false, 2, 5);
        b.right_angle = true;
        let mut f = view(-20.0, 15.0, false, 2, 5);
        f.right_angle = true;
        assert_eq!(
            b.value_axis(3_092_266, 2_260_600, false).0,
            view_len(160.0, 15.0)
        );
        assert_eq!(f.value_axis(3_092_266, 2_260_600, false).0, len);
    }

    #[test]
    fn bar_value_axis_matches_traced_edges() {
        let near = |got: i64, want: i64| assert!((got - want).abs() <= 3, "{got} vs {want}");
        let bar = |rot: f64, elev: f64| {
            let mut v = view(rot, elev, false, 3, 3);
            v.bar = true;
            v
        };
        let (w, h) = (2_947_866, 2_260_600);
        let (len, horizontal) = bar(20.0, 15.0).value_axis(w, h, false);
        near(len, 2_331_437);
        assert!(horizontal);
        let (len, horizontal) = bar(100.0, 15.0).value_axis(w + 12_700, h, false);
        near(len, 716_727);
        assert!(!horizontal);
        near(
            bar(20.0, 0.0).value_axis(w + 4 * 12_700, h, false).0,
            2_541_656,
        );
        near(
            bar(20.0, 80.0).value_axis(w + 5 * 12_700, h, false).0,
            2_211_594,
        );
        near(
            bar(20.0, 15.0).value_axis(w + 7 * 12_700, h, true).0,
            2_453_670,
        );
    }

    #[test]
    fn category_axis_matches_traced_box() {
        let v = view(100.0, 15.0, false, 2, 3);
        let (w, h) = (2_941_770, 2_260_600);
        assert_eq!(v.x_edge_box(w, h, false), (274_195, 712_448));
        assert_eq!(v.category_axis_rise(w, h, false), Some(712_448));
    }

    fn view_len(rot: f64, elev: f64) -> i64 {
        let mut v = view(rot, elev, false, 2, 5);
        v.right_angle = true;
        v.value_axis(3_092_266, 2_260_600, false).0
    }

    #[test]
    fn value_axis_matches_traced_lengths() {
        let near = |got: i64, want: i64| assert!((got - want).abs() <= 3, "{got} vs {want}");
        near(
            view(20.0, 80.0, true, 1, 3)
                .value_axis(3_186_119, 1_867_281, false)
                .0,
            176_897,
        );
        near(
            view(20.0, 15.0, false, 1, 3)
                .value_axis(3_186_119, 1_867_281, false)
                .0,
            1_406_355,
        );
        near(
            view(0.0, 15.0, false, 2, 5)
                .value_axis(3_092_266, 2_260_600, false)
                .0,
            2_084_809,
        );
        near(
            view(20.0, 15.0, false, 2, 5)
                .value_axis(5_378_266, 4_292_600, false)
                .0,
            3_517_003,
        );
        near(
            view(20.0, 15.0, true, 1, 3)
                .value_axis(5_472_119, 3_899_281, false)
                .0,
            2_291_034,
        );
    }
}
