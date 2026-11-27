use crate::address::{Rect, MAX_COLS, MAX_ROWS};
use crate::structure::{Axis, Shift};

impl AdjustContext {
    pub(crate) fn of_shift(s: &Shift) -> AdjustContext {
        let rows = s.axis == Axis::Rows;
        let (l0, l1) = s
            .limit
            .unwrap_or((0, if rows { MAX_COLS - 1 } else { MAX_ROWS - 1 }));
        let first = if s.insert { s.at } else { s.at + s.count };
        let delta = if s.insert {
            s.count as i64
        } else {
            -(s.count as i64)
        };

        let rect = if rows {
            Rect {
                r0: first,
                c0: l0,
                r1: MAX_ROWS - 1,
                c1: l1,
            }
        } else {
            Rect {
                r0: l0,
                c0: first,
                r1: l1,
                c1: MAX_COLS - 1,
            }
        };
        AdjustContext {
            kind: if s.insert { Kind::Insert } else { Kind::Delete },
            rect,
            dr: if rows { delta } else { 0 },
            dc: if rows { 0 } else { delta },
            dest: rect,
        }
    }

    pub(crate) fn area(&self, a: &Rect) -> Option<Rect> {
        let mut touched = false;
        let r = adjust_area(
            self,
            a,
            MAX_ROWS as i64,
            MAX_COLS as i64,
            &|_| false,
            &mut touched,
        );
        match r.code {
            1 | 2 | 5 => None,
            _ => Some(r.rect),
        }
    }
}

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub(crate) enum Kind {
    Delete,

    Insert,

    Move,

    MoveSource,

    Overwrite,
}

#[derive(Clone, Copy, Debug)]
pub(crate) struct AdjustContext {
    pub kind: Kind,

    pub rect: Rect,

    pub dr: i64,
    pub dc: i64,

    pub dest: Rect,
}

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub(crate) struct Adjusted {
    pub code: u32,
    pub rect: Rect,
}

#[derive(Clone, Copy)]
struct Cell {
    code: u32,
    row: i64,
    col: i64,
}

fn cell(x: &AdjustContext, row: i64, col: i64, maxr: i64, maxc: i64) -> Cell {
    let i = |v: u32| v as i64;
    let (r0, r1, c0, c1) = (i(x.rect.r0), i(x.rect.r1), i(x.rect.c0), i(x.rect.c1));
    let same = |code| Cell { code, row, col };
    let d = &x.dest;
    let in_dest = i(d.r0) <= row && row <= i(d.r1) && i(d.c0) <= col && col <= i(d.c1);
    let row_in = r0 <= row && row <= r1;
    let col_in = c0 <= col && col <= c1;
    let both = row_in && col_in;
    let shifted = |code: u32| {
        if row + x.dr < maxr && col + x.dc < maxc {
            Cell {
                code,
                row: row + x.dr,
                col: col + x.dc,
            }
        } else {
            same(2)
        }
    };
    match x.kind {
        Kind::Delete => {
            if (col_in && row < r0 && r0 + x.dr <= row) || (row_in && col < c0 && c0 + x.dc <= col)
            {
                same(1)
            } else if both {
                shifted(0)
            } else {
                same(8)
            }
        }
        Kind::Insert => {
            if both {
                shifted(0)
            } else {
                same(8)
            }
        }
        Kind::Move => {
            if both {
                shifted(0)
            } else if in_dest {
                same(1)
            } else {
                same(8)
            }
        }
        Kind::MoveSource => {
            if both {
                shifted(5)
            } else {
                same(8)
            }
        }
        Kind::Overwrite => same(if in_dest { 1 } else { 8 }),
    }
}

type Cand = (i64, i64, i64, i64);

pub(crate) fn adjust_area(
    x: &AdjustContext,
    a: &Rect,
    maxr: i64,
    maxc: i64,
    entire_table: &dyn Fn(&Rect) -> bool,
    touched: &mut bool,
) -> Adjusted {
    let i = |v: u32| v as i64;
    let (r0, r1, c0, c1) = (i(a.r0), i(a.r1), i(a.c0), i(a.c1));
    let mv = matches!(x.kind, Kind::Move | Kind::Overwrite);

    let (er, ec) = if x.kind == Kind::MoveSource {
        (0, 0)
    } else {
        (x.dr, x.dc)
    };
    let (lr, lc) = (maxr - 1, maxc - 1);
    let (h, w) = (r1 - r0, c1 - c0);
    let (xr0, xr1, xc0, xc1) = (i(x.rect.r0), i(x.rect.r1), i(x.rect.c0), i(x.rect.c1));
    let (dr0, dr1, dc0, dc1) = (i(x.dest.r0), i(x.dest.r1), i(x.dest.c0), i(x.dest.c1));
    let out = |code, row0: i64, row1: i64, col0: i64, col1: i64| Adjusted {
        code,
        rect: Rect::new(row0 as u32, col0 as u32, row1 as u32, col1 as u32),
    };
    if entire_table(a) {
        return Adjusted { code: 8, rect: *a };
    }
    let tl = cell(x, r0, c0, maxr, maxc);
    let tr = cell(x, r0, c1, maxr, maxc);
    let bl = cell(x, r1, c0, maxr, maxc);
    let br = cell(x, r1, c1, maxr, maxc);
    if tl.code == 8 && br.code == 8 {
        return Adjusted { code: 8, rect: *a };
    }
    if tl.code & 7 != 0 && br.code & 7 != 0 {
        return out(tl.code & 7, tl.row, br.row, tl.col, br.col);
    }
    if bl.code & 7 != 0 && tr.code & 7 != 0 {
        return out(bl.code & 7, tl.row, br.row, tl.col, br.col);
    }
    let (ctl, ctr, cbl, cbr) = (tl.code & 3, tr.code & 3, bl.code & 3, br.code & 3);
    let (mut row0, mut row1, mut col0) = (tl.row, br.row, tl.col);

    let mut edge = false;

    if ctl == 1 && ctr == 1 {
        col0 = bl.col;
        if h != lr {
            edge = true;
            if !mv {
                row0 = if er == 0 { xr1 + 1 } else { er + xr0 };
            } else if br.row == r1 {
                row0 = r0;
                if dr0 != r0 {
                    row0 = dr1 + 1;
                }
            } else {
                row0 = dr0;
                if br.col == c1 && br.row < xr0 {
                    row1 = xr0 - 1;
                }
            }
        }
    }

    let mut col1 = br.col;
    if ctr != 0 && cbr != 0 {
        row1 = bl.row;
        if ctr == 2 {
            edge = true;
            col1 = lc;
        } else if w != lc {
            edge = true;
            if !mv {
                col1 = xc0 - 1 + ec;
            } else if col0 == c0 {
                col1 = c1;
                if dc1 != c1 {
                    col1 = dc0 - 1;
                }
            } else {
                col1 = dc1;
                if row0 == r0 && xc1 < col0 {
                    col0 = xc1 + 1;
                }
            }
        }
    }

    if cbr != 0 && cbl != 0 {
        col1 = tr.col;
        if cbr == 2 {
            edge = true;
            row1 = lr;
        } else if h != lr {
            edge = true;
            if !mv {
                row1 = er - 1 + xr0;
            } else if row0 == r0 {
                row1 = r1;
                if dr1 != r1 {
                    row1 = dr0 - 1;
                }
            } else {
                row1 = dr1;
                if col0 == c0 && xr1 < row0 {
                    row0 = xr1 + 1;
                }
            }
        }
    }

    if cbl == 1 && ctl == 1 {
        row0 = tr.row;
        if w != lc {
            edge = true;
            if !mv {
                col0 = if ec == 0 { xc1 + 1 } else { xc0 + ec };
            } else if col1 == c1 {
                col0 = c0;
                if dc0 != c0 {
                    col0 = dc1 + 1;
                }
            } else {
                col0 = dc0;
                if row1 == r1 && col1 < xc0 {
                    col1 = xc0 - 1;
                }
            }
        }
    }
    let cand: Cand = (col0, row0, col1, row1);
    let orig: Cand = (c0, r0, c1, r1);
    let n = cbr + cbl + ctr + ctl;
    let mut code = 0;
    let p: Cand = if n == 0 {
        let m_tl = tl.row != r0 || tl.col != c0;
        let m_tr = !(tr.row == r0 && tr.col == c1);
        let m_br = !(br.row == r1 && br.col == c1);
        let m_bl = !(bl.row == r1 && bl.col == c0);
        let cnt = m_tl as i32 + m_tr as i32 + m_br as i32 + m_bl as i32;
        if cnt == 1 {
            orig
        } else if cnt != 2 {
            cand
        } else {
            let b3 = tl.row == tr.row && tl.col == bl.col;
            if b3 && !mv {
                if br.col == lc || br.row == lr {
                    edge = true;
                }
                code = 0x10;
                cand
            } else {
                let mask =
                    (m_tl as u32) << 3 | (m_tr as u32) << 2 | (m_br as u32) << 1 | m_bl as u32;
                let mut p = orig;
                match mask {
                    3 => {
                        if b3 && row0 <= row1 {
                            p = cand;
                            edge = true;
                            if xr1 == r1 && row1 < xr0 {
                                p.3 = xr0 - 1;
                            }
                        }
                    }
                    6 => {
                        if b3 && col0 <= col1 {
                            p = cand;
                            edge = true;
                            if xc1 == c1 && col1 < xc0 {
                                p.2 = xc0 - 1;
                            }
                        }
                    }
                    9 => {
                        if b3 && col0 <= col1 {
                            p = cand;
                            edge = true;
                            if xc0 == c0 && xc1 < col0 {
                                p.0 = xc1 + 1;
                            }
                        }
                    }
                    12 => {
                        if b3 && row0 <= row1 {
                            p = cand;
                            edge = true;
                            if xr0 == r0 && xr1 < row0 {
                                p.1 = xr1 + 1;
                            }
                        }
                    }
                    _ => edge = true,
                }
                p
            }
        }
    } else if n == 1 {
        orig
    } else {
        cand
    };
    finish(x, a, p, code, edge, touched, (maxr, maxc))
}

fn finish(
    x: &AdjustContext,
    a: &Rect,
    p: Cand,
    mut code: u32,
    mut edge: bool,
    touched: &mut bool,
    (maxr, maxc): (i64, i64),
) -> Adjusted {
    let (lr, lc) = (maxr - 1, maxc - 1);
    let (r0, r1, c0, c1) = (a.r0 as i64, a.r1 as i64, a.c0 as i64, a.c1 as i64);
    let (mut col0, mut row0, mut col1, mut row1) = p;
    if r1 - r0 == lr {
        edge = true;
        row0 = 0;
        row1 = lr;
    } else if r1 == lr && x.kind == Kind::Delete {
        edge = true;
        *touched = true;
        row1 = lr;
    }
    let clip = if c1 - c0 == lc {
        col0 = 0;
        col1 = lc;
        true
    } else {
        if c1 == lc && x.kind == Kind::Delete {
            edge = true;
            *touched = true;
            col1 = lc;
        }
        edge
    };
    if clip {
        code = 3;
    }
    Adjusted {
        code,
        rect: Rect::new(
            row0.min(row1) as u32,
            col0.min(col1) as u32,
            row0.max(row1) as u32,
            col0.max(col1) as u32,
        ),
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    type Case = (i32, [i64; 10], bool, [i64; 4], [i64; 4], u32);

    #[test]
    fn traced_contexts() {
        let cases: &[Case] = &[
            (
                1,
                [6, 1048575, 0, 2, 2, 0, 0, 0, 0, 0],
                false,
                [0, 5, 0, 1],
                [0, 5, 0, 1],
                8,
            ),
            (
                1,
                [6, 1048575, 0, 2, 2, 0, 0, 0, 0, 0],
                false,
                [1, 7, 0, 0],
                [1, 9, 0, 0],
                16,
            ),
            (
                1,
                [6, 1048575, 0, 2, 2, 0, 0, 0, 0, 0],
                false,
                [1, 5, 0, 0],
                [1, 5, 0, 0],
                8,
            ),
            (
                1,
                [6, 1048575, 0, 2, 2, 0, 0, 0, 0, 0],
                false,
                [6, 1048575, 0, 2],
                [8, 1048575, 0, 2],
                3,
            ),
            (
                1,
                [6, 1048575, 0, 2, 2, 0, 0, 0, 0, 0],
                false,
                [0, 6, 0, 2],
                [0, 8, 0, 2],
                16,
            ),
            (
                1,
                [6, 1048575, 0, 2, 2, 0, 0, 0, 0, 0],
                false,
                [1, 5, 0, 2],
                [1, 5, 0, 2],
                8,
            ),
            (
                1,
                [6, 1048575, 0, 2, 2, 0, 0, 0, 0, 0],
                false,
                [0, 5, 0, 2],
                [0, 5, 0, 2],
                8,
            ),
            (
                1,
                [6, 1048575, 0, 2, 2, 0, 0, 0, 0, 0],
                false,
                [0, 7, 0, 2],
                [0, 9, 0, 2],
                16,
            ),
            (
                1,
                [6, 1048575, 0, 2, 2, 0, 0, 0, 0, 0],
                false,
                [0, 1048575, 0, 0],
                [0, 1048575, 0, 0],
                3,
            ),
            (
                1,
                [6, 1048575, 0, 2, 2, 0, 0, 0, 0, 0],
                false,
                [5, 5, 0, 0],
                [5, 5, 0, 0],
                8,
            ),
            (
                1,
                [6, 1048575, 0, 2, 2, 0, 0, 0, 0, 0],
                false,
                [1, 100, 0, 0],
                [1, 102, 0, 0],
                16,
            ),
            (
                1,
                [6, 1048575, 0, 2, 2, 0, 0, 0, 0, 0],
                false,
                [1, 8, 0, 0],
                [1, 10, 0, 0],
                16,
            ),
            (
                1,
                [6, 1048575, 0, 2, 2, 0, 0, 0, 0, 0],
                false,
                [1, 5, 0, 1],
                [1, 5, 0, 1],
                8,
            ),
            (
                1,
                [6, 1048575, 0, 2, 2, 0, 0, 0, 0, 0],
                false,
                [2, 5, 0, 0],
                [2, 5, 0, 0],
                8,
            ),
            (
                1,
                [6, 1048575, 0, 2, 2, 0, 0, 0, 0, 0],
                false,
                [1, 4, 0, 0],
                [1, 4, 0, 0],
                8,
            ),
            (
                1,
                [6, 1048575, 0, 2, 2, 0, 0, 0, 0, 0],
                false,
                [0, 7, 0, 0],
                [0, 9, 0, 0],
                16,
            ),
            (
                1,
                [6, 1048575, 0, 2, 2, 0, 0, 0, 0, 0],
                false,
                [0, 5, 0, 0],
                [0, 5, 0, 0],
                8,
            ),
            (
                5,
                [9, 10, 0, 2, -3, 0, 6, 7, 0, 2],
                false,
                [0, 7, 0, 1],
                [0, 7, 0, 1],
                8,
            ),
            (
                5,
                [9, 10, 0, 2, -3, 0, 6, 7, 0, 2],
                false,
                [1, 9, 0, 0],
                [1, 6, 0, 0],
                3,
            ),
            (
                5,
                [9, 10, 0, 2, -3, 0, 6, 7, 0, 2],
                false,
                [1, 7, 0, 0],
                [1, 7, 0, 0],
                8,
            ),
            (
                5,
                [9, 10, 0, 2, -3, 0, 6, 7, 0, 2],
                false,
                [0, 7, 0, 0],
                [0, 7, 0, 0],
                8,
            ),
            (
                5,
                [9, 10, 0, 2, -3, 0, 6, 7, 0, 2],
                false,
                [0, 9, 0, 0],
                [0, 6, 0, 0],
                3,
            ),
            (
                5,
                [9, 10, 0, 2, -3, 0, 6, 7, 0, 2],
                false,
                [1, 4, 0, 0],
                [1, 4, 0, 0],
                8,
            ),
            (
                5,
                [9, 10, 0, 2, -3, 0, 6, 7, 0, 2],
                false,
                [2, 5, 0, 0],
                [2, 5, 0, 0],
                8,
            ),
            (
                5,
                [9, 10, 0, 2, -3, 0, 6, 7, 0, 2],
                false,
                [1, 7, 0, 1],
                [1, 7, 0, 1],
                8,
            ),
            (
                5,
                [9, 10, 0, 2, -3, 0, 6, 7, 0, 2],
                false,
                [1, 10, 0, 0],
                [1, 8, 0, 0],
                3,
            ),
            (
                5,
                [9, 10, 0, 2, -3, 0, 6, 7, 0, 2],
                false,
                [1, 102, 0, 0],
                [1, 102, 0, 0],
                8,
            ),
            (
                5,
                [9, 10, 0, 2, -3, 0, 6, 7, 0, 2],
                false,
                [5, 5, 0, 0],
                [5, 5, 0, 0],
                8,
            ),
            (
                5,
                [9, 10, 0, 2, -3, 0, 6, 7, 0, 2],
                false,
                [0, 1048575, 0, 0],
                [0, 1048575, 0, 0],
                8,
            ),
            (
                5,
                [9, 10, 0, 2, -3, 0, 6, 7, 0, 2],
                false,
                [0, 9, 0, 2],
                [0, 6, 0, 2],
                3,
            ),
            (
                5,
                [9, 10, 0, 2, -3, 0, 6, 7, 0, 2],
                false,
                [0, 7, 0, 2],
                [0, 7, 0, 2],
                8,
            ),
            (
                5,
                [9, 10, 0, 2, -3, 0, 6, 7, 0, 2],
                false,
                [1, 7, 0, 2],
                [1, 7, 0, 2],
                8,
            ),
            (
                0,
                [11, 1048575, 0, 2, -2, 0, 6, 7, 0, 2],
                false,
                [0, 7, 0, 1],
                [0, 7, 0, 1],
                8,
            ),
            (
                0,
                [11, 1048575, 0, 2, -2, 0, 6, 7, 0, 2],
                false,
                [1, 6, 0, 0],
                [1, 6, 0, 0],
                8,
            ),
            (
                0,
                [11, 1048575, 0, 2, -2, 0, 6, 7, 0, 2],
                false,
                [1, 7, 0, 0],
                [1, 7, 0, 0],
                8,
            ),
            (
                0,
                [11, 1048575, 0, 2, -2, 0, 6, 7, 0, 2],
                false,
                [11, 1048575, 0, 2],
                [9, 1048575, 0, 2],
                3,
            ),
            (
                0,
                [11, 1048575, 0, 2, -2, 0, 6, 7, 0, 2],
                true,
                [1, 7, 0, 0],
                [1, 7, 0, 0],
                8,
            ),
            (
                0,
                [11, 1048575, 0, 2, -2, 0, 6, 7, 0, 2],
                true,
                [0, 7, 0, 0],
                [0, 7, 0, 0],
                8,
            ),
            (
                0,
                [11, 1048575, 0, 2, -2, 0, 6, 7, 0, 2],
                true,
                [1, 6, 0, 0],
                [1, 6, 0, 0],
                8,
            ),
            (
                0,
                [11, 1048575, 0, 2, -2, 0, 6, 7, 0, 2],
                true,
                [0, 6, 0, 0],
                [0, 6, 0, 0],
                8,
            ),
            (
                0,
                [11, 1048575, 0, 2, -2, 0, 6, 7, 0, 2],
                true,
                [1, 4, 0, 0],
                [1, 4, 0, 0],
                8,
            ),
            (
                0,
                [11, 1048575, 0, 2, -2, 0, 6, 7, 0, 2],
                true,
                [2, 5, 0, 0],
                [2, 5, 0, 0],
                8,
            ),
            (
                0,
                [11, 1048575, 0, 2, -2, 0, 6, 7, 0, 2],
                true,
                [1, 7, 0, 1],
                [1, 7, 0, 1],
                8,
            ),
            (
                0,
                [11, 1048575, 0, 2, -2, 0, 6, 7, 0, 2],
                true,
                [1, 8, 0, 0],
                [1, 8, 0, 0],
                8,
            ),
            (
                0,
                [11, 1048575, 0, 2, -2, 0, 6, 7, 0, 2],
                true,
                [1, 102, 0, 0],
                [1, 100, 0, 0],
                16,
            ),
            (
                0,
                [11, 1048575, 0, 2, -2, 0, 6, 7, 0, 2],
                true,
                [5, 5, 0, 0],
                [5, 5, 0, 0],
                8,
            ),
            (
                0,
                [11, 1048575, 0, 2, -2, 0, 6, 7, 0, 2],
                true,
                [0, 1048575, 0, 0],
                [0, 1048575, 0, 0],
                3,
            ),
            (
                0,
                [11, 1048575, 0, 2, -2, 0, 6, 7, 0, 2],
                true,
                [0, 6, 0, 2],
                [0, 6, 0, 2],
                8,
            ),
            (
                0,
                [11, 1048575, 0, 2, -2, 0, 6, 7, 0, 2],
                true,
                [0, 7, 0, 2],
                [0, 7, 0, 2],
                8,
            ),
            (
                0,
                [11, 1048575, 0, 2, -2, 0, 6, 7, 0, 2],
                true,
                [1, 7, 0, 2],
                [1, 7, 0, 2],
                8,
            ),
        ];
        let (maxr, maxc) = (1_048_576, 16_384);
        let rc = |a: i64, b: i64, c: i64, d: i64| Rect::new(a as u32, c as u32, b as u32, d as u32);
        let mut bad = Vec::new();
        for (k, c, _c8, i, o, ret) in cases {
            let kind = match k {
                0 => Kind::Delete,
                1 => Kind::Insert,
                _ => Kind::Move,
            };
            let x = AdjustContext {
                kind,
                rect: rc(c[0], c[1], c[2], c[3]),
                dr: c[4],
                dc: c[5],
                dest: rc(c[6], c[7], c[8], c[9]),
            };
            let mut t = false;

            let table = Rect::new(0, 0, 8, 2);
            let entire = |a: &Rect| {
                kind == Kind::Move
                    && table.contains(a.r0, a.c0)
                    && (a.r0 > table.r0 || a.r1 < table.r1 || a.c0 > table.c0 || a.c1 < table.c1)
                    && a.r0 <= table.r0 + 1
                    && a.r1 + 1 >= table.r1
                    && a.r1 <= table.r1
                    && a.c1 <= table.c1
            };
            let r = adjust_area(&x, &rc(i[0], i[1], i[2], i[3]), maxr, maxc, &entire, &mut t);
            let want = rc(o[0], o[1], o[2], o[3]);

            if r.code != *ret || (r.code != 8 && r.rect != want) {
                bad.push(format!(
                    "{k} {c:?} in {i:?}: got {} {:?} want {ret} {o:?}",
                    r.code, r.rect
                ));
            }
        }
        assert!(bad.is_empty(), "{}", bad.join("\n"));
    }

    #[test]
    fn traced_cross_sheet() {
        let a = |s: &str| {
            let (x, y) = s.split_once(':').unwrap_or((s, s));
            let p = |t: &str| {
                let (col, row) = t.split_at(1);
                (
                    row.parse::<u32>().unwrap() - 1,
                    u32::from(col.as_bytes()[0] - b'A'),
                )
            };
            Rect::new(p(x).0, p(x).1, p(y).0, p(y).1)
        };
        let src = a("C3:F8");
        let ctx = |kind| AdjustContext {
            kind,
            rect: src,
            dr: 7,
            dc: 5,
            dest: a("H10:K15"),
        };
        let cases = [
            (Kind::MoveSource, "C1:F4", "C1:F2", 3),
            (Kind::MoveSource, "C6:F10", "C9:F10", 3),
            (Kind::MoveSource, "A4:D6", "A4:B6", 3),
            (Kind::MoveSource, "E4:H6", "G4:H6", 3),
            (Kind::MoveSource, "A1:C3", "A1:C3", 0),
            (Kind::MoveSource, "E7:H10", "E7:H10", 0),
            (Kind::MoveSource, "A5:H5", "A5:H5", 8),
            (Kind::MoveSource, "A3:H5", "A3:H5", 8),
            (Kind::MoveSource, "D4:E5", "I11:J12", 5),
            (Kind::MoveSource, "C3:F8", "H10:K15", 5),
            (Kind::Overwrite, "H8:K11", "H8:K9", 3),
            (Kind::Overwrite, "H12:K18", "H16:K18", 3),
            (Kind::Overwrite, "F11:I13", "F11:G13", 3),
            (Kind::Overwrite, "J11:M13", "L11:M13", 3),
            (Kind::Overwrite, "F8:H10", "F8:H10", 0),
            (Kind::Overwrite, "I11:J12", "I11:J12", 1),
            (Kind::Overwrite, "H8:K18", "H8:K18", 8),
            (Kind::Overwrite, "F12:M12", "F12:M12", 8),
        ];
        for (kind, from, to, code) in cases {
            let mut t = false;
            let r = adjust_area(&ctx(kind), &a(from), 1_048_576, 16_384, &|_| false, &mut t);
            assert_eq!((r.code, r.rect), (code, a(to)), "{kind:?} {from}");
        }
    }
}
