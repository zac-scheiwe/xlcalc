use crate::chart_layout::Rect;

pub(crate) const OUTSIDE: i64 = 50_800;

pub(crate) fn border() -> (i64, i64) {
    const PX: f64 = 8.0;
    const EMU_PER_INCH: f64 = 914_400.0;
    (
        (PX / 96.0 * (EMU_PER_INCH + 0.5)) as i64,
        (PX / 96.0 * EMU_PER_INCH) as i64,
    )
}

const MIN_HARD: i64 = 10;

#[derive(Clone, Copy, PartialEq, Debug)]
pub(crate) enum Dock {
    Top,
    Right,
    Bottom,
    Left,

    Fill,
}

#[derive(Clone, Debug)]
pub(crate) struct LObj {
    pub soft: Rect,
    pub hard: Rect,
    pub slots: [Rect; 5],
}

impl LObj {
    pub(crate) fn new(soft: Rect) -> LObj {
        LObj {
            soft,
            hard: soft,
            slots: [soft; 5],
        }
    }

    pub(crate) fn inset_soft_bounds(&mut self, dx: i64, dy: i64) {
        self.soft[0] += dx;
        self.soft[1] += dy;
        self.soft[2] -= 2 * dx;
        self.soft[3] -= 2 * dy;
    }

    pub(crate) fn shrink_to_slots(&mut self) {
        let [mut l, mut t, w, h] = self.hard;
        let (mut r, mut b) = (l + w, t + h);
        for s in &self.slots {
            l = l.max(s[0]);
            t = t.max(s[1]);
            r = r.min(s[0] + s[2]);
            b = b.min(s[1] + s[3]);
        }
        self.hard = [l, t, (r - l).max(MIN_HARD), (b - t).max(MIN_HARD)];
    }

    pub(crate) fn dock(
        &mut self,
        size: [i64; 2],
        dock: Dock,
        space: usize,
        pad: [i64; 4],
        border: bool,
        consume: bool,
    ) -> Rect {
        let (bx, by) = if border { self::border() } else { (0, 0) };
        let [sx, sy, sw, sh] = self.slots[space];
        let [pl, pr, pt, pb] = pad;
        let [mut cw, mut ch] = size;
        let soft = self.soft;
        let centre = |len: &mut i64, at: i64, room: i64| -> i64 {
            if *len == -1 {
                *len = room;
                at
            } else {
                (room - *len) / 2 + at
            }
        };

        let across = |at: i64, room: i64, lo: i64, len: i64, plo: i64, phi: i64| -> (i64, i64) {
            let a = (lo + plo).max(at);
            let e = (lo - phi + len).min(a + room);
            (a, e - a)
        };
        let child;
        match dock {
            Dock::Top | Dock::Bottom => {
                let (y, rest_y) = if dock == Dock::Top {
                    (sy + by, sy + by + ch)
                } else {
                    (sy + sh - ch - by, sy)
                };
                let x = centre(&mut cw, sx, sw);
                child = [x, y, cw, ch];
                let (ax, aw) = across(sx, sw, soft[0], soft[2], pl, pr);
                self.slots[space][0] = ax;
                self.slots[space][2] = aw;
                if consume {
                    self.slots[space][1] = rest_y;
                    self.slots[space][3] = sh - ch - by;
                }
            }
            Dock::Left | Dock::Right => {
                let (x, rest_x) = if dock == Dock::Left {
                    (sx + bx, sx + bx + cw)
                } else {
                    (sx + sw - cw - bx, sx)
                };
                let y = centre(&mut ch, sy, sh);
                child = [x, y, cw, ch];
                let (ay, ah) = across(sy, sh, soft[1], soft[3], pt, pb);
                self.slots[space][1] = ay;
                self.slots[space][3] = ah;
                if consume {
                    self.slots[space][0] = rest_x;
                    self.slots[space][2] = sw - cw - bx;
                }
            }
            Dock::Fill => {
                child = [sx + bx, sy + by, sw - 2 * bx, sh - 2 * by];
                if consume {
                    self.slots[space] = [sx, sy, 0, 0];
                }
            }
        }
        self.shrink_to_slots();
        child
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn border_is_eight_pixels() {
        assert_eq!(border(), (76_200, 76_200));
    }

    #[test]
    fn docks_consume_their_slot() {
        let mut o = LObj::new([0, 0, 1_000_000, 800_000]);
        let t = o.dock([-1, 300_000], Dock::Top, 0, [0; 4], true, true);
        assert_eq!(t, [0, 76_200, 1_000_000, 300_000]);
        assert_eq!(o.slots[0], [0, 376_200, 1_000_000, 423_800]);
        let r = o.dock([200_000, 100_000], Dock::Right, 0, [0; 4], true, true);
        assert_eq!(
            r,
            [723_800, 376_200 + (423_800 - 100_000) / 2, 200_000, 100_000]
        );
        assert_eq!(o.hard, [0, 376_200, 723_800, 423_800]);
        let b = o.dock([100_000, 50_000], Dock::Bottom, 0, [0; 4], false, true);
        assert_eq!(b[1], 750_000);
        let l = o.dock([10_000, 10_000], Dock::Left, 0, [0; 4], true, true);
        assert_eq!(l[0], 76_200);
        assert_eq!(o.hard, [86_200, 376_200, 637_600, 373_800]);
    }

    #[test]
    fn slots_bound_the_same_edge_with_the_nearer_one() {
        let mut o = LObj::new([0, 0, 1_000_000, 1_000_000]);
        o.dock([100_000, -1], Dock::Right, 0, [0; 4], false, true);
        o.dock([300_000, -1], Dock::Right, 1, [0; 4], false, true);
        assert_eq!(o.hard[2], 700_000);
    }

    #[test]
    fn pads_hold_the_slot_inside_the_soft_rect() {
        let mut o = LObj::new([0, 0, 1_000_000, 1_000_000]);
        o.dock(
            [-1, 100],
            Dock::Bottom,
            0,
            [20_000, 30_000, 0, 0],
            false,
            false,
        );
        assert_eq!(o.slots[0], [20_000, 0, 950_000, 1_000_000]);
    }

    #[test]
    fn hard_rect_keeps_a_minimum() {
        let mut o = LObj::new([0, 0, 100, 100]);
        o.dock([-1, -1], Dock::Fill, 0, [0; 4], false, true);
        assert_eq!(o.hard, [0, 0, MIN_HARD, MIN_HARD]);
    }
}
