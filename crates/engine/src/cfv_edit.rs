use crate::address::{CellAddr, Rect, SheetId, MAX_COLS, MAX_ROWS};
use crate::cfv::{CfKind, CfRule, Dv, Dxf};
use crate::cfv_hosed::{minimize, split_hosed, End, Tok, R4};
use crate::formula::{CRef, Node, Rc, SheetRef};
use crate::formula_print::{print_node, print_node_r1c1};
use crate::structure::{
    encode, map_refs, resolve, shift_cref, targets_sheet, within_limit, Axis, Shift,
};
use crate::workbook::Workbook;

#[derive(Clone)]
pub(crate) struct Core {
    pub applies: Vec<Rect>,
    pub f: [Option<String>; 2],
}

#[derive(Clone, Copy)]
pub(crate) struct Piece {
    pub old: Rect,
    pub new: Rect,

    pub sheet: Option<SheetId>,

    pub area: usize,

    pub fresh: bool,

    pub read: Option<(u32, u32, u32, u32)>,

    pub unit: Option<usize>,
}

pub(crate) struct Group {
    pub sheet: SheetId,
    pub applies: Vec<Rect>,
    pub f: [Option<String>; 2],

    pub first: usize,

    pub orig: bool,
}

struct Built {
    first: usize,
    sheet: SheetId,
    rects: Vec<(usize, Rect)>,
    roots: [Option<Node>; 2],
    lit: [Option<String>; 2],
}

type Outcome = Option<(Vec<Group>, Option<Vec<i32>>)>;

type Bases = Vec<((usize, usize), Vec<(usize, Vec<u8>)>)>;

fn rel_sig(root: &Node, abs: bool) -> Vec<Option<i32>> {
    let mut out = Vec::new();
    map_refs(root, &mut |_: &SheetRef, cref: &CRef| {
        let (rcs, ext) = match *cref {
            CRef::Cell { row, col } => (vec![row, col], vec![]),
            CRef::Area { r0, c0, r1, c1 } => (vec![r0, c0, r1, c1], vec![(r0, r1), (c0, c1)]),
            CRef::Cols { c0, c1 } => (vec![c0, c1], vec![(c0, c1)]),
            CRef::Rows { r0, r1 } => (vec![r0, r1], vec![(r0, r1)]),
        };
        out.extend(
            rcs.into_iter()
                .flat_map(|rc| [Some(i32::from(rc.abs)), (!rc.abs || abs).then_some(rc.v)]),
        );
        out.extend(
            ext.into_iter()
                .map(|(a, b)| (a.abs && b.abs).then_some(b.v - a.v)),
        );
        None
    });
    out
}

pub(crate) fn bbox_tl(applies: &[Rect]) -> (u32, u32) {
    (
        applies.iter().map(|r| r.r0).min().unwrap_or(0),
        applies.iter().map(|r| r.c0).min().unwrap_or(0),
    )
}

pub(crate) fn coalesce(mut v: Vec<Rect>) -> Vec<Rect> {
    'again: loop {
        for i in 0..v.len() {
            for j in (i + 1)..v.len() {
                let (a, b) = (v[i], v[j]);
                let same_cols = a.c0 == b.c0 && a.c1 == b.c1;
                let same_rows = a.r0 == b.r0 && a.r1 == b.r1;
                let joins =
                    (same_cols && a.r0 <= b.r1.saturating_add(1) && b.r0 <= a.r1.saturating_add(1))
                        || (same_rows
                            && a.c0 <= b.c1.saturating_add(1)
                            && b.c0 <= a.c1.saturating_add(1))
                        || a.contains_rect(&b)
                        || b.contains_rect(&a);
                if joins {
                    v[i] = a.union_bounds(&b);
                    v.remove(j);
                    continue 'again;
                }
            }
        }
        return v;
    }
}

pub(crate) fn shift_pieces(s: &Shift, r: Rect, area: usize) -> Vec<Piece> {
    let same = |r: Rect| Piece {
        old: r,
        new: r,
        sheet: None,
        area,
        fresh: false,
        read: None,
        unit: None,
    };
    let (p, q, lo, hi) = match s.axis {
        Axis::Rows => (r.r0, r.r1, r.c0, r.c1),
        Axis::Cols => (r.c0, r.c1, r.r0, r.r1),
    };
    let mk = |a: u32, b: u32, c: u32, d: u32| match s.axis {
        Axis::Rows => Rect::new(a, c, b, d),
        Axis::Cols => Rect::new(c, a, d, b),
    };
    let (b0, b1) = match s.limit {
        Some((l0, l1)) => (lo.max(l0), hi.min(l1)),
        None => (lo, hi),
    };
    if b0 > b1 {
        return vec![same(r)];
    }

    if (b0, b1) != (lo, hi) {
        return cell_shift_pieces(s, r, area, (p, q, b0, b1));
    }
    let mut keep: Vec<Rect> = Vec::new();
    let mut moved: Vec<(Rect, Rect)> = Vec::new();
    if lo < b0 {
        keep.push(mk(p, q, lo, b0 - 1));
    }
    if hi > b1 {
        keep.push(mk(p, q, b1 + 1, hi));
    }
    let (at, n) = (s.at, s.count);
    if s.insert {
        if at <= p {
            moved.push((mk(p, q, b0, b1), mk(p + n, q + n, b0, b1)));
        } else if at <= q + 1 {
            moved.push((mk(p, q, b0, b1), mk(p, q + n, b0, b1)));
        } else {
            keep.push(mk(p, q, b0, b1));
        }
    } else {
        let end = at + n - 1;
        if p < at {
            keep.push(mk(p, q.min(at - 1), b0, b1));
        }
        if q > end {
            let a = p.max(end + 1);
            moved.push((mk(a, q, b0, b1), mk(a - n, q - n, b0, b1)));
        }
    }
    let mut out: Vec<Piece> = keep.into_iter().map(same).collect();
    out.extend(moved.into_iter().map(|(old, new)| Piece {
        old,
        new,
        sheet: None,
        area,
        fresh: false,
        read: None,
        unit: None,
    }));
    out
}

fn cell_shift_pieces(
    s: &Shift,
    r: Rect,
    area: usize,
    (p, q, b0, b1): (u32, u32, u32, u32),
) -> Vec<Piece> {
    let mk = |a: u32, b: u32| match s.axis {
        Axis::Rows => Rect::new(a, b0, b, b1),
        Axis::Cols => Rect::new(b0, a, b1, b),
    };
    let piece = |old: Rect, new: Rect, fresh: bool| Piece {
        old,
        new,
        sheet: None,
        area,
        fresh,
        read: None,
        unit: None,
    };
    let (at, n) = (s.at, s.count);
    let mut out = Vec::new();
    let band = (at <= q).then(|| mk(p.max(at), q));
    match band {
        Some(b) => out.extend(r.subtract(&b).into_iter().map(|x| piece(x, x, false))),
        None => out.push(piece(r, r, false)),
    }
    if s.insert {
        if at > p && at <= q + 1 {
            let f = mk(at, at + n - 1);
            out.push(piece(f, f, true));
        }
        if let Some(b) = band {
            let (a, e) = (p.max(at), q);
            out.push(piece(b, mk(a + n, e + n), false));
        }
    } else {
        let end = at + n - 1;
        if q > end {
            let a = p.max(end + 1);
            out.push(piece(mk(a, q), mk(a - n, q - n), false));
        }
    }
    out
}

fn split_line_insert(s: &Shift, raw: Vec<Piece>) -> Vec<Piece> {
    let (at, n) = (s.at, s.count);
    let span = |r: &Rect| match s.axis {
        Axis::Rows => (r.r0, r.r1),
        Axis::Cols => (r.c0, r.c1),
    };
    let mk = |r: &Rect, a: u32, b: u32| match s.axis {
        Axis::Rows => Rect::new(a, r.c0, b, r.c1),
        Axis::Cols => Rect::new(r.r0, a, r.r1, b),
    };
    let mut out = Vec::new();
    for p in raw {
        let (p0, q0) = span(&p.old);
        if p.old == p.new || p0 >= at || q0 < at || p.sheet.is_some() {
            out.push(p);
            continue;
        }
        out.push(Piece {
            old: mk(&p.old, p0, at - 1),
            new: mk(&p.old, p0, at + n - 1),
            ..p
        });
        out.push(Piece {
            old: mk(&p.old, at, q0),
            new: mk(&p.old, at + n, q0 + n),
            ..p
        });
    }
    out
}

fn minimize_rects(v: Vec<Rect>) -> Vec<Rect> {
    let mut r4: Vec<R4> = v
        .iter()
        .map(|r| [r.r0.into(), r.r1.into(), r.c0.into(), r.c1.into()])
        .collect();
    minimize(&mut r4);
    r4.iter()
        .map(|x| Rect::new(x[0] as u32, x[2] as u32, x[1] as u32, x[3] as u32))
        .collect()
}

fn join_areas(into: &mut Vec<Rect>, more: Vec<Rect>) {
    into.extend(more);
    *into = minimize_rects(std::mem::take(into));
}

fn delete_regions(s: &Shift) -> Option<(Rect, Rect, Rect)> {
    let (l0, l1) = s.limit?;
    if s.insert {
        return None;
    }
    let (last, band_last) = match s.axis {
        Axis::Rows => (MAX_ROWS - 1, MAX_COLS - 1),
        Axis::Cols => (MAX_COLS - 1, MAX_ROWS - 1),
    };
    let end = s.at + s.count - 1;
    if end >= last {
        return None;
    }
    let mk = |a: u32, b: u32| match s.axis {
        Axis::Rows => Rect::new(a, l0, b, l1.min(band_last)),
        Axis::Cols => Rect::new(l0, a, l1.min(band_last), b),
    };
    Some((mk(s.at, end), mk(end + 1, last), mk(s.at, last - s.count)))
}

fn ref_span(cref: &CRef, h: &Rect) -> Option<Rect> {
    let at = |rc: Rc, base: u32, max: u32| {
        let v = if rc.abs {
            i64::from(rc.v)
        } else {
            i64::from(base) + i64::from(rc.v)
        };
        u32::try_from(v).ok().filter(|&v| v < max)
    };
    let (r0, c0, r1, c1) = match *cref {
        CRef::Cell { row, col } => (
            at(row, h.r0, MAX_ROWS)?,
            at(col, h.c0, MAX_COLS)?,
            at(row, h.r1, MAX_ROWS)?,
            at(col, h.c1, MAX_COLS)?,
        ),
        CRef::Area { r0, c0, r1, c1 } => (
            at(r0, h.r0, MAX_ROWS)?,
            at(c0, h.c0, MAX_COLS)?,
            at(r1, h.r1, MAX_ROWS)?,
            at(c1, h.c1, MAX_COLS)?,
        ),
        _ => return None,
    };
    (r0 <= r1 && c0 <= c1).then(|| Rect::new(r0, c0, r1, c1))
}

fn hosed_tok(cref: &CRef) -> Tok {
    let row = |rc: Rc| End {
        v: i64::from((rc.v as u16) as i16),
        rel: !rc.abs,
    };
    let col = |rc: Rc| End {
        v: i64::from(rc.v & 0x3fff),
        rel: !rc.abs,
    };
    let fixed = |v: u32| End {
        v: v.into(),
        rel: false,
    };
    match *cref {
        CRef::Cell { row: r, col: c } => Tok {
            row: [row(r), row(r)],
            col: [col(c), col(c)],
        },
        CRef::Area { r0, c0, r1, c1 } => Tok {
            row: [row(r0), row(r1)],
            col: [col(c0), col(c1)],
        },
        CRef::Rows { r0, r1 } => Tok {
            row: [row(r0), row(r1)],
            col: [fixed(0), fixed(MAX_COLS - 1)],
        },
        CRef::Cols { c0, c1 } => Tok {
            row: [fixed(0), fixed(MAX_ROWS - 1)],
            col: [col(c0), col(c1)],
        },
    }
}

fn r4(r: &Rect) -> R4 {
    [r.r0.into(), r.r1.into(), r.c0.into(), r.c1.into()]
}

pub(crate) fn move_pieces(
    src: Rect,
    dr: i64,
    dc: i64,
    dst_sheet: SheetId,
    r: Rect,
    area: usize,
) -> Vec<Piece> {
    let mut out: Vec<Piece> = r
        .subtract(&src)
        .into_iter()
        .map(|x| Piece {
            old: x,
            new: x,
            sheet: None,
            area,
            fresh: false,
            read: None,
            unit: None,
        })
        .collect();
    if let Some(i) = r.intersection(&src) {
        let sh = |v: u32, d: i64| (i64::from(v) + d).clamp(0, i64::from(u32::MAX)) as u32;
        out.push(Piece {
            old: i,
            new: Rect::new(sh(i.r0, dr), sh(i.c0, dc), sh(i.r1, dr), sh(i.c1, dc)),
            sheet: Some(dst_sheet),
            area,
            fresh: false,
            read: None,
            unit: None,
        });
    }
    out
}

fn cut_cref(cref: &CRef, old: CellAddr, new: CellAddr, src: Rect, d: (i64, i64)) -> Option<CRef> {
    let mv = |r: u32, c: u32, inside: bool| -> (u32, u32) {
        if inside {
            (
                (i64::from(r) + d.0).clamp(0, i64::from(MAX_ROWS) - 1) as u32,
                (i64::from(c) + d.1).clamp(0, i64::from(MAX_COLS) - 1) as u32,
            )
        } else {
            (r, c)
        }
    };
    match *cref {
        CRef::Cell { row, col } => {
            let r = resolve(row, old.row, MAX_ROWS)?;
            let c = resolve(col, old.col, MAX_COLS)?;
            let (nr, nc) = mv(r, c, src.contains(r, c));
            Some(CRef::Cell {
                row: encode(nr, row.abs, new.row),
                col: encode(nc, col.abs, new.col),
            })
        }
        CRef::Area { r0, c0, r1, c1 } => {
            let (a, b) = (
                resolve(r0, old.row, MAX_ROWS)?,
                resolve(c0, old.col, MAX_COLS)?,
            );
            let (x, y) = (
                resolve(r1, old.row, MAX_ROWS)?,
                resolve(c1, old.col, MAX_COLS)?,
            );
            let inside = src.contains(a, b) && src.contains(x, y);
            let (na, nb) = mv(a, b, inside);
            let (nx, ny) = mv(x, y, inside);
            Some(CRef::Area {
                r0: encode(na, r0.abs, new.row),
                c0: encode(nb, c0.abs, new.col),
                r1: encode(nx, r1.abs, new.row),
                c1: encode(ny, c1.abs, new.col),
            })
        }
        other => Some(other),
    }
}

#[derive(Clone, Copy)]
pub(crate) enum Rewrite {
    Shift(Shift),

    Cut(SheetId, Rect, (i64, i64)),

    Copy,
}

impl Workbook {
    pub(crate) fn cfv_groups(
        &self,
        home: SheetId,
        core: &Core,
        pieces: &[Piece],
        how: Rewrite,
    ) -> Vec<Group> {
        self.cfv_groups_in(home, core, pieces, how, None)
    }

    fn cfv_groups_in(
        &self,
        home: SheetId,
        core: &Core,
        pieces: &[Piece],
        how: Rewrite,
        class: Option<&[usize]>,
    ) -> Vec<Group> {
        let (ar, ac) = bbox_tl(&core.applies);
        let anchor = CellAddr::new(home, ar, ac);
        let compiled: [Option<Node>; 2] = std::array::from_fn(|i| {
            let t = core.f[i].as_deref()?.strip_prefix('=')?;
            self.compile_normalized(t, anchor)
                .ok()
                .map(|(f, _)| f.root.clone())
        });
        let hosed: Vec<usize> = pieces
            .iter()
            .filter(|p| p.unit.is_some())
            .map(|p| p.area)
            .collect();
        let grown: Vec<usize> = pieces.iter().filter(|p| p.fresh).map(|p| p.area).collect();
        let mut built: Vec<Built> = Vec::new();
        for (k, p) in pieces.iter().enumerate() {
            let sheet = p.sheet.unwrap_or(home);
            let (or, oc, nr, nc) = p.read.unwrap_or((p.old.r0, p.old.c0, p.new.r0, p.new.c0));
            let old = CellAddr::new(home, or, oc);
            let new = CellAddr::new(sheet, nr, nc);
            let roots: [Option<Node>; 2] = std::array::from_fn(|i| {
                let root = compiled[i].as_ref()?;
                Some(match how {
                    _ if p.fresh => root.clone(),
                    Rewrite::Copy => root.clone(),
                    Rewrite::Shift(s) => map_refs(root, &mut |sr: &SheetRef, cref: &CRef| {
                        if targets_sheet(sr, old.sheet, s.sheet) && within_limit(cref, old, &s) {
                            Some(shift_cref(cref, old, new, &s).ok())
                        } else if let Some(c) = targets_sheet(sr, old.sheet, s.sheet)
                            .then(|| s.straddle(cref, old, new))
                            .flatten()
                        {
                            Some(Some(c))
                        } else if old != new {
                            Some(shift_cref(cref, old, new, &Shift { count: 0, ..s }).ok())
                        } else {
                            None
                        }
                    }),
                    Rewrite::Cut(src_sheet, src, d) => {
                        map_refs(root, &mut |sr: &SheetRef, cref: &CRef| {
                            if targets_sheet(sr, old.sheet, src_sheet) {
                                Some(cut_cref(cref, old, new, src, d))
                            } else if old != new {
                                Some(cut_cref(cref, old, new, Rect::new(1, 1, 0, 0), d))
                            } else {
                                None
                            }
                        })
                    }
                })
            });
            let lit: [Option<String>; 2] = std::array::from_fn(|i| {
                core.f[i]
                    .clone()
                    .filter(|t| compiled[i].is_none() || !t.starts_with('='))
            });

            let joins = |b: &Built| {
                class.is_some_and(|c| c[b.first] == c[k]) && p.fresh && !pieces[b.first].fresh
            };
            let same = |b: &Built| {
                joins(b)
                    || b.sheet == sheet
                        && class.is_none_or(|c| c[b.first] == c[k])
                        && (0..2).all(|i| match (&b.roots[i], &roots[i]) {
                            (Some(x), Some(y)) => {
                                print_node(self, x, new) == print_node(self, y, new)
                            }
                            (None, None) => true,
                            _ => false,
                        })
                        && b.lit == lit
            };
            let rect = (p.area, p.new);
            if let Some(b) = built.iter_mut().find(|b| same(b)) {
                b.rects.push(rect);
            } else {
                built.push(Built {
                    first: k,
                    sheet,
                    rects: vec![rect],
                    roots,
                    lit,
                });
            }
        }
        built
            .into_iter()
            .map(|b| {
                let areas: Vec<Rect> = if b.rects.iter().any(|x| hosed.contains(&x.0)) {
                    let (kept, moved): (Vec<&(usize, Rect)>, Vec<_>) =
                        b.rects.iter().partition(|x| {
                            !hosed.contains(&x.0) && core.applies.get(x.0) == Some(&x.1)
                        });
                    minimize_rects(kept.iter().chain(&moved).map(|x| x.1).collect())
                } else {
                    let mut by: Vec<usize> = b.rects.iter().map(|x| x.0).collect();
                    by.dedup();
                    let mut all = Vec::new();

                    let deleting =
                        matches!(how, Rewrite::Shift(s) if !s.insert && s.limit.is_none());
                    let mut classed: Vec<(u8, Vec<Rect>)> = Vec::new();
                    for a in by {
                        let rects: Vec<Rect> =
                            b.rects.iter().filter(|x| x.0 == a).map(|x| x.1).collect();
                        let class = match (core.applies.get(a), rects.as_slice()) {
                            (Some(o), [r]) if r == o => 0,
                            (Some(o), [r]) if r.rows() == o.rows() && r.cols() == o.cols() => 1,
                            _ => 2,
                        };

                        classed.push((
                            class,
                            if grown.contains(&a) {
                                minimize_rects(rects)
                            } else {
                                coalesce(rects)
                            },
                        ));
                    }
                    if deleting && classed.iter().any(|c| c.0 > 0) {
                        classed.sort_by_key(|c| c.0);
                        all = minimize_rects(classed.into_iter().flat_map(|c| c.1).collect());
                    } else {
                        all.extend(classed.into_iter().flat_map(|c| c.1));
                    }
                    all
                };
                let (nr, nc) = bbox_tl(&areas);
                let host = CellAddr::new(b.sheet, nr, nc);
                let f = std::array::from_fn(|i| match &b.roots[i] {
                    Some(root) => Some(format!("={}", print_node(self, root, host))),
                    None => b.lit[i].clone(),
                });
                let abs = matches!(how, Rewrite::Cut(..));
                let orig = b.sheet == home
                    && (0..2).all(|i| match (&compiled[i], &b.roots[i]) {
                        (Some(x), Some(y)) => rel_sig(x, abs) == rel_sig(y, abs),
                        (None, None) => true,
                        _ => false,
                    });
                Group {
                    sheet: b.sheet,
                    applies: areas,
                    f,
                    first: b.first,
                    orig,
                }
            })
            .collect()
    }

    fn dv_refs_straddle(&self, home: SheetId, core: &Core, s: &Shift) -> bool {
        if s.limit.is_none() && s.insert {
            return false;
        }
        let (ar, ac) = bbox_tl(&core.applies);
        let anchor = CellAddr::new(home, ar, ac);
        let mut bounds: Option<Rect> = None;
        let mut each: Vec<Rect> = Vec::new();
        for t in core.f.iter().flatten() {
            let Some(t) = t.strip_prefix('=') else {
                continue;
            };
            let Ok((f, _)) = self.compile_normalized(t, anchor) else {
                continue;
            };

            for h in core
                .applies
                .iter()
                .flat_map(|a| [(a.r0, a.c0), (a.r1, a.c1)])
            {
                map_refs(&f.root, &mut |sr: &SheetRef, cref: &CRef| {
                    if targets_sheet(sr, home, s.sheet) {
                        let rect = match *cref {
                            CRef::Cell { row, col } => resolve(row, h.0, MAX_ROWS)
                                .zip(resolve(col, h.1, MAX_COLS))
                                .map(|(r, c)| Rect::new(r, c, r, c)),
                            CRef::Area { r0, c0, r1, c1 } => {
                                let r =
                                    |a, b| resolve(a, h.0, MAX_ROWS).zip(resolve(b, h.0, MAX_ROWS));
                                let c =
                                    |a, b| resolve(a, h.1, MAX_COLS).zip(resolve(b, h.1, MAX_COLS));
                                r(r0, r1).zip(c(c0, c1)).map(|((a, b), (x, y))| {
                                    Rect::new(a.min(b), x.min(y), a.max(b), x.max(y))
                                })
                            }
                            _ => None,
                        };
                        if let Some(rect) = rect {
                            bounds = Some(bounds.map_or(rect, |b| b.union_bounds(&rect)));
                            each.push(rect);
                        }
                    }
                    None
                });
            }
        }
        let Some(b) = bounds else {
            return false;
        };
        let (l0, l1) = s.limit.unwrap_or((0, u32::MAX));
        let moved = match s.axis {
            Axis::Rows => Rect::new(s.at, l0, MAX_ROWS - 1, l1.min(MAX_COLS - 1)),
            Axis::Cols => Rect::new(l0, s.at, l1.min(MAX_ROWS - 1), MAX_COLS - 1),
        };

        let gone = (!s.insert).then(|| match s.axis {
            Axis::Rows => Rect::new(s.at, l0, s.at + s.count - 1, l1.min(MAX_COLS - 1)),
            Axis::Cols => Rect::new(l0, s.at, l1.min(MAX_ROWS - 1), s.at + s.count - 1),
        });
        b.intersection(&moved).is_some() && !moved.contains_rect(&b)
            || gone.is_some_and(|g| each.iter().any(|r| r.intersection(&g).is_some()))
    }

    fn edit_regions(how: Rewrite) -> Option<(SheetId, Vec<Rect>)> {
        match how {
            Rewrite::Copy => None,
            Rewrite::Cut(sheet, src, _) => Some((sheet, vec![src])),
            Rewrite::Shift(s) => {
                let (l0, l1) = s.limit.unwrap_or((0, u32::MAX));
                let mk = |a: u32, b: u32| match s.axis {
                    Axis::Rows => Rect::new(a, l0, b, l1.min(MAX_COLS - 1)),
                    Axis::Cols => Rect::new(l0, a, l1.min(MAX_ROWS - 1), b),
                };
                let last = match s.axis {
                    Axis::Rows => MAX_ROWS - 1,
                    Axis::Cols => MAX_COLS - 1,
                };
                let end = s.at + s.count - 1;
                Some((
                    s.sheet,
                    if s.insert {
                        vec![mk(s.at, last)]
                    } else if end < last {
                        vec![mk(s.at, end), mk(end + 1, last)]
                    } else {
                        vec![mk(s.at, end)]
                    },
                ))
            }
        }
    }

    fn cfv_subpieces(
        &self,
        home: SheetId,
        cores: &[Core],
        pieces: &[Piece],
        how: Rewrite,
    ) -> Vec<Vec<(Piece, Vec<u8>)>> {
        let whole = |p: &Piece| vec![(*p, Vec::new())];
        let Some((rsheet, regions)) = Self::edit_regions(how) else {
            return pieces.iter().map(whole).collect();
        };
        let mut refs: Vec<CRef> = Vec::new();
        for core in cores {
            let (ar, ac) = bbox_tl(&core.applies);
            let anchor = CellAddr::new(home, ar, ac);
            for t in core.f.iter().flatten() {
                let Some(t) = t.strip_prefix('=') else {
                    continue;
                };
                let Ok((f, _)) = self.compile_normalized(t, anchor) else {
                    continue;
                };
                map_refs(&f.root, &mut |sr: &SheetRef, cref: &CRef| {
                    if targets_sheet(sr, home, rsheet) {
                        refs.push(*cref);
                    }
                    None
                });
            }
        }
        let ends = |c: &CRef| -> Vec<(Option<Rc>, Option<Rc>)> {
            match *c {
                CRef::Cell { row, col } => vec![(Some(row), Some(col))],
                CRef::Area { r0, c0, r1, c1 } => vec![(Some(r0), Some(c0)), (Some(r1), Some(c1))],
                CRef::Rows { r0, r1 } => vec![(Some(r0), None), (Some(r1), None)],
                CRef::Cols { c0, c1 } => vec![(None, Some(c0)), (None, Some(c1))],
            }
        };
        let sig = |r: u32, c: u32| -> Vec<u8> {
            let host = CellAddr::new(home, r, c);
            let mut out = Vec::new();
            for cref in &refs {
                let on = match how {
                    Rewrite::Shift(s) => within_limit(cref, host, &s),
                    _ => true,
                };
                for (er, ec) in ends(cref) {
                    let rr = er.map(|x| resolve(x, r, MAX_ROWS));
                    let cc = ec.map(|x| resolve(x, c, MAX_COLS));
                    let k = regions.iter().position(|g| {
                        on && match (rr, cc) {
                            (Some(Some(a)), Some(Some(b))) => g.contains(a, b),
                            (Some(Some(a)), None) => {
                                g.c0 == 0 && g.c1 >= MAX_COLS - 1 && (g.r0..=g.r1).contains(&a)
                            }
                            (None, Some(Some(b))) => {
                                g.r0 == 0 && g.r1 >= MAX_ROWS - 1 && (g.c0..=g.c1).contains(&b)
                            }
                            _ => false,
                        }
                    });
                    out.push(k.map_or(0, |k| k as u8 + 1));
                }
            }
            out
        };
        let mut out = Vec::new();
        for p in pieces {
            let size = |r: &Rect| (r.r1 - r.r0, r.c1 - r.c0);
            if p.fresh || size(&p.old) != size(&p.new) {
                out.push(whole(p));
                continue;
            }
            let o = p.old;
            let (mut rl, mut cl): (Vec<u32>, Vec<u32>) = (Vec::new(), Vec::new());
            for cref in &refs {
                for (er, ec) in ends(cref) {
                    for g in &regions {
                        for (rc, lines, lo, hi, a, b) in [
                            (er, &mut rl, o.r0, o.r1, g.r0, g.r1),
                            (ec, &mut cl, o.c0, o.c1, g.c0, g.c1),
                        ] {
                            let Some(rc) = rc.filter(|x| !x.abs) else {
                                continue;
                            };
                            for e in [i64::from(a), i64::from(b) + 1] {
                                let x = e - i64::from(rc.v);
                                if x > i64::from(lo) && x <= i64::from(hi) {
                                    lines.push(x as u32);
                                }
                            }
                        }
                    }
                }
            }
            for l in [&mut rl, &mut cl] {
                l.sort_unstable();
                l.dedup();
            }
            let starts = |lo: u32, l: &[u32]| -> Vec<u32> {
                std::iter::once(lo).chain(l.iter().copied()).collect()
            };

            let (rs, cs) = (starts(o.r0, &rl), starts(o.c0, &cl));
            let rl: Vec<u32> = rl
                .iter()
                .copied()
                .filter(|&x| cs.iter().any(|&c| sig(x - 1, c) != sig(x, c)))
                .collect();
            let cl: Vec<u32> = cl
                .iter()
                .copied()
                .filter(|&x| rs.iter().any(|&r| sig(r, x - 1) != sig(r, x)))
                .collect();
            let bounds = |lo: u32, hi: u32, l: &[u32]| -> Vec<(u32, u32)> {
                let st = starts(lo, l);
                st.iter()
                    .enumerate()
                    .map(|(i, &a)| (a, st.get(i + 1).map_or(hi, |&b| b - 1)))
                    .collect()
            };
            let (dr, dc) = (
                i64::from(p.new.r0) - i64::from(o.r0),
                i64::from(p.new.c0) - i64::from(o.c0),
            );
            let sh = |v: u32, d: i64| (i64::from(v) + d) as u32;
            let mut subs = Vec::new();
            for (r0, r1) in bounds(o.r0, o.r1, &rl) {
                for (c0, c1) in bounds(o.c0, o.c1, &cl) {
                    let old = Rect::new(r0, c0, r1, c1);
                    let new = Rect::new(sh(r0, dr), sh(c0, dc), sh(r1, dr), sh(c1, dc));
                    subs.push((Piece { old, new, ..*p }, sig(r0, c0)));
                }
            }
            out.push(subs);
        }
        out
    }

    fn cf_hosed(
        &self,
        home: SheetId,
        cores: &[Core],
        how: Rewrite,
        (area, r): (usize, Rect),
        first: usize,
    ) -> Option<Vec<Piece>> {
        let (gone, moved, dest, (dr, dc)) = match how {
            Rewrite::Shift(s) if s.sheet == home => {
                let (gone, moved, dest) = delete_regions(&s)?;
                let d = match s.axis {
                    Axis::Rows => (-i64::from(s.count), 0),
                    Axis::Cols => (0, -i64::from(s.count)),
                };
                (Some(gone), moved, dest, d)
            }

            Rewrite::Cut(sheet, src, d) if sheet == home => {
                let at = |v: u32, d: i64| u32::try_from(i64::from(v) + d).ok();
                let dest = Rect::new(
                    at(src.r0, d.0)?,
                    at(src.c0, d.1)?,
                    at(src.r1, d.0)?,
                    at(src.c1, d.1)?,
                );
                (None, src, dest, d)
            }
            _ => return None,
        };
        let mut toks: Vec<Tok> = Vec::new();

        let mut lands = false;
        for core in cores {
            let (ar, ac) = bbox_tl(&core.applies);
            let anchor = CellAddr::new(home, ar, ac);
            for t in core.f.iter().flatten() {
                let Some(t) = t.strip_prefix('=') else {
                    continue;
                };
                let Ok((f, _)) = self.compile_normalized(t, anchor) else {
                    continue;
                };
                map_refs(&f.root, &mut |sr: &SheetRef, cref: &CRef| {
                    if targets_sheet(sr, home, home) {
                        toks.push(hosed_tok(cref));
                        lands |= ref_span(cref, &r)
                            .is_some_and(|x| x.intersects(&moved) && !moved.contains_rect(&x));
                    }
                    None
                });
            }
        }

        let touched = gone.is_some_and(|g| r.intersects(&g)) || r.intersects(&moved);
        let inside = gone.is_some_and(|g| g.contains_rect(&r)) || moved.contains_rect(&r);
        if !(touched && !inside || lands && !inside) {
            return None;
        }
        let shift = |v: u32, d: i64| (i64::from(v) + d) as u32;
        let mut out = Vec::new();
        let mut unit = first;
        for piece in split_hosed(r4(&r), &toks, &r4(&moved), &r4(&dest)) {
            let rects: Vec<(Rect, Rect)> = piece
                .iter()
                .map(|x| Rect::new(x[0] as u32, x[2] as u32, x[1] as u32, x[3] as u32))
                .filter(|x| !gone.is_some_and(|g| g.contains_rect(x)))
                .map(|x| {
                    let to = if moved.contains_rect(&x) {
                        Rect::new(
                            shift(x.r0, dr),
                            shift(x.c0, dc),
                            shift(x.r1, dr),
                            shift(x.c1, dc),
                        )
                    } else {
                        x
                    };
                    (x, to)
                })
                .collect();
            let Some(&(o, n)) = rects.first() else {
                continue;
            };
            let read = Some((o.r0, o.c0, n.r0, n.c0));
            out.extend(rects.into_iter().map(|(old, new)| Piece {
                old,
                new,
                sheet: None,
                area,
                fresh: false,
                read,
                unit: Some(unit),
            }));
            unit += 1;
        }
        Some(out)
    }

    fn cf_area_hidden(&self, sheet: SheetId, rule: &CfRule, how: Rewrite) -> bool {
        let Rewrite::Shift(s) = how else {
            return false;
        };
        if s.sheet != sheet {
            return false;
        }
        let size = |r: &Rect| (r.r1 - r.r0, r.c1 - r.c0);
        matches!(
            rule.kind,
            CfKind::AboveAverage { .. } | CfKind::Top10 { .. }
        ) && rule
            .applies
            .iter()
            .any(|r| s.adjust_rect(r).is_none_or(|x| size(&x) != size(r)))
    }

    fn cf_line_insert_hosed(&self, home: SheetId, cores: &[Core], s: &Shift) -> bool {
        if !s.insert || s.limit.is_some() || s.sheet != home {
            return false;
        }
        let mut hit = false;
        for core in cores {
            let (ar, ac) = bbox_tl(&core.applies);
            let anchor = CellAddr::new(home, ar, ac);
            for t in core.f.iter().flatten() {
                let Some(t) = t.strip_prefix('=') else {
                    continue;
                };
                let Ok((f, _)) = self.compile_normalized(t, anchor) else {
                    continue;
                };
                map_refs(&f.root, &mut |_: &SheetRef, cref: &CRef| {
                    let rcs = match (*cref, s.axis) {
                        (CRef::Cell { row, .. }, Axis::Rows) => vec![row],
                        (CRef::Cell { col, .. }, Axis::Cols) => vec![col],
                        (CRef::Area { r0, r1, .. }, Axis::Rows) => vec![r0, r1],
                        (CRef::Area { c0, c1, .. }, Axis::Cols) => vec![c0, c1],
                        (CRef::Rows { r0, r1 }, Axis::Rows) => vec![r0, r1],
                        (CRef::Cols { c0, c1 }, Axis::Cols) => vec![c0, c1],
                        _ => vec![],
                    };
                    hit |= rcs.iter().any(|rc| !rc.abs && rc.v != 0);
                    None
                });
            }
        }
        hit
    }

    fn cf_same_rule(&self, sheet: SheetId, a: &CfRule, b: &CfRule) -> bool {
        if a.kind != b.kind || a.stop != b.stop || a.dxf != b.dxf {
            return false;
        }
        let at = |r: &CfRule| {
            let (row, col) = r.anchor();
            CellAddr::new(sheet, row, col)
        };
        let (ha, hb) = (at(a), at(b));
        let read = |t: &str, host: CellAddr| {
            let f = t.strip_prefix('=')?;
            let (f, _) = self.compile_normalized(f, host).ok()?;
            Some(print_node_r1c1(self, &f.root, host))
        };
        [(&a.f1, &b.f1), (&a.f2, &b.f2)]
            .into_iter()
            .all(|(x, y)| match (x, y) {
                (Some(x), Some(y)) => match (read(x, ha), read(y, hb)) {
                    (Some(p), Some(q)) => p == q,
                    _ => x == y,
                },
                (None, None) => true,
                _ => false,
            })
    }

    fn cfv_rewrite_sheet(
        &mut self,
        sheet: SheetId,
        mut pieces_of: impl FnMut(&Core, usize, Rect) -> Vec<Piece>,
        how: Rewrite,
    ) {
        let Some(sh) = self.cfv.get(&sheet) else {
            return;
        };
        let rules = sh.rules.clone();
        let dvs = sh.dvs.clone();
        let mut next_seq = rules.iter().map(|r| r.seq).max().unwrap_or(0) + 1;
        let mut grown_dvs: Vec<Dv> = Vec::new();
        let mut out_rules: Vec<CfRule> = Vec::new();
        let mut extra: Vec<(SheetId, CfRule)> = Vec::new();
        let mut placed: Vec<(CfRule, i32)> = Vec::new();

        let mut wholes: Vec<(i32, i32, i32)> = Vec::new();
        let core_of = |r: &CfRule| Core {
            applies: r.applies.clone(),
            f: [r.f1.clone(), r.f2.clone()],
        };

        let mut blocks: Vec<Vec<usize>> = Vec::new();
        for (i, r) in rules.iter().enumerate() {
            match blocks.iter_mut().find(|b| rules[b[0]].applies == r.applies) {
                Some(b) => b.push(i),
                None => blocks.push(vec![i]),
            }
        }
        for b in &mut blocks {
            b.sort_by_key(|&i| rules[i].prio);
        }
        blocks.sort_by_key(|b| b.iter().map(|&i| rules[i].seq).min());
        let mut top = rules.iter().map(|r| r.prio).max().unwrap_or(0);

        let mut done: Vec<Outcome> = (0..rules.len()).map(|_| None).collect();
        for b in &blocks {
            let cores: Vec<Core> = b.iter().map(|&i| core_of(&rules[i])).collect();

            let mut raw: Vec<Piece> = Vec::new();
            let mut units = 0;
            for (i, r) in cores[0].applies.iter().enumerate() {
                let emu = match how {
                    Rewrite::Shift(_) | Rewrite::Cut(..) => {
                        self.cf_hosed(sheet, &cores, how, (i, *r), units)
                    }
                    Rewrite::Copy => None,
                };
                match emu {
                    Some(ps) => {
                        units = ps
                            .iter()
                            .filter_map(|p| p.unit)
                            .max()
                            .map_or(units, |u| u + 1);
                        raw.extend(ps);
                    }
                    None => raw.extend(pieces_of(&cores[0], i, *r)),
                }
            }

            let hosed =
                matches!(how, Rewrite::Shift(s) if self.cf_line_insert_hosed(sheet, &cores, &s));
            let raw = match how {
                Rewrite::Shift(s) if hosed => split_line_insert(&s, raw),
                _ => raw,
            };
            if matches!(how, Rewrite::Copy) {
                for &i in b {
                    done[i] = Some((self.cfv_groups(sheet, &cores[0], &raw, how), None));
                }
                continue;
            }
            let subs: Vec<Vec<(Piece, Vec<u8>)>> = if hosed {
                raw.iter().map(|p| vec![(*p, Vec::new())]).collect()
            } else {
                let subs = self.cfv_subpieces(sheet, &cores, &raw, how);
                raw.iter()
                    .zip(subs)
                    .map(|(p, sub)| match p.unit {
                        Some(_) => vec![(*p, Vec::new())],
                        None => sub,
                    })
                    .collect()
            };

            let moved = |p: &Piece| !p.fresh && (p.old != p.new || p.sheet.is_some());
            let mut flat: Vec<Piece> = Vec::new();
            let mut bases: Bases = Vec::new();
            for (i, (p, sub)) in raw.iter().zip(&subs).enumerate() {
                let key = match p.unit {
                    Some(u) => (u, 2),
                    None if hosed => (i, 1),
                    None => (p.area, usize::from(moved(p))),
                };
                let k = match bases.iter().position(|x| x.0 == key) {
                    Some(k) => k,
                    None => {
                        bases.push((key, Vec::new()));
                        bases.len() - 1
                    }
                };
                for (q, sig) in sub {
                    bases[k].1.push((flat.len(), sig.clone()));
                    flat.push(*q);
                }
            }
            let mut slot = vec![0usize; flat.len()];
            let mut slots = bases.len();
            for (k, (_, members)) in bases.iter().enumerate() {
                let calm = |s: &[u8]| s.iter().all(|&x| x == 0);

                let any_calm = members.iter().any(|m| !flat[m.0].fresh && calm(&m.1));
                let mut own: Option<&[u8]> = None;
                let mut extra: Vec<(&[u8], usize)> = Vec::new();
                for (i, sig) in members {
                    let sig = sig.as_slice();
                    slot[*i] = if flat[*i].fresh || (any_calm && calm(sig)) || own == Some(sig) {
                        k
                    } else if !any_calm && own.is_none() {
                        own = Some(sig);
                        k
                    } else if let Some(&(_, s)) = extra.iter().find(|e| e.0 == sig) {
                        s
                    } else {
                        extra.push((sig, slots));
                        slots += 1;
                        slots - 1
                    };
                }
            }

            let whole = b.len() > 1
                || b.iter()
                    .any(|&i| self.cf_area_hidden(sheet, &rules[i], how));
            let class: Option<Vec<usize>> = whole.then(|| {
                let mut fold = vec![true; slots];
                for (j, c) in cores.iter().enumerate() {
                    let hidden = self.cf_area_hidden(sheet, &rules[b[j]], how);
                    for g in self.cfv_groups_in(sheet, c, &flat, how, Some(&slot)) {
                        if !g.orig || hidden {
                            fold[slot[g.first]] = false;
                        }
                    }
                }
                slot.iter()
                    .map(|&s| if fold[s] { usize::MAX } else { s })
                    .collect()
            });
            let groups: Vec<Vec<Group>> = cores
                .iter()
                .map(|c| {
                    let mut gs = self.cfv_groups_in(sheet, c, &flat, how, class.as_deref());
                    if let Some(cl) = &class {
                        for g in &mut gs {
                            g.orig &= cl[g.first] == usize::MAX;
                        }
                    }
                    gs
                })
                .collect();
            let size = |r: &Rect| (r.r1 - r.r0, r.c1 - r.c0);

            let cell_grown = raw
                .iter()
                .filter(|p| match how {
                    Rewrite::Shift(s) if s.limit.is_some() => {
                        let end = match s.axis {
                            Axis::Rows => p.old.r1,
                            Axis::Cols => p.old.c1,
                        };
                        size(&p.old) != size(&p.new) && s.at <= end
                    }
                    _ => false,
                })
                .count();
            slots += cell_grown;
            let split = units > 0
                || flat.len() != raw.len()
                || raw.len() != cores[0].applies.len()
                || cell_grown > 0
                || raw
                    .iter()
                    .any(|p| p.fresh || size(&p.old) != size(&cores[0].applies[p.area]));

            let lands = |x: u8| match how {
                Rewrite::Cut(..) => x != 0,
                Rewrite::Shift(s) => !s.insert && x == 1 && s.limit.is_some(),
                Rewrite::Copy => false,
            };
            let landed = subs
                .iter()
                .flatten()
                .any(|(_, sig)| sig.iter().any(|&x| lands(x)));
            let affected = split || landed;
            let n = b.len() as i32;
            for ((j, &i), g) in b.iter().enumerate().zip(groups) {
                let prio = affected.then(|| {
                    g.iter()
                        .map(|g| top + 1 + slot[g.first] as i32 * n + j as i32)
                        .collect()
                });
                done[i] = Some((g, prio));
            }

            let still = if units > 0 {
                bases.iter().filter(|x| x.0 .1 == 0).count()
            } else {
                0
            };
            let slots = slots - still;
            if affected {
                if whole {
                    wholes.push((top + 1, top + slots as i32 * n, n));
                }
                top += slots as i32 * n;
            }
        }

        let mut block_of = vec![0usize; rules.len()];
        for (bi, b) in blocks.iter().enumerate() {
            for &i in b {
                block_of[i] = bi;
            }
        }
        let mut out_block: Vec<(usize, bool)> = Vec::new();
        for (ri, rule) in rules.into_iter().enumerate() {
            let Some((groups, prio)) = done[ri].take() else {
                continue;
            };
            for (k, g) in groups.into_iter().enumerate() {
                let mut r = rule.clone();

                let line = matches!(how, Rewrite::Shift(s) if s.limit.is_none());
                if !line && grew(&rule.applies, &g.applies) {
                    r.seq = next_seq;
                    next_seq += 1;
                }
                r.applies = g.applies;
                r.f1 = g.f[0].clone();
                r.f2 = g.f[1].clone();
                match &prio {
                    Some(_) if g.orig => {
                        out_rules.push(r);
                        out_block.push((block_of[ri], true));
                    }
                    Some(p) if g.sheet == sheet => placed.push((r, p[k])),
                    None if k == 0 && g.sheet == sheet => {
                        out_rules.push(r);
                        out_block.push((block_of[ri], false));
                    }
                    _ => extra.push((g.sheet, r)),
                }
            }
        }

        let mut keep = vec![true; out_rules.len()];
        for bi in 0..blocks.len() {
            let of = |b: usize, keep: &[bool]| -> Vec<usize> {
                let mut v: Vec<usize> = (0..out_block.len())
                    .filter(|&k| keep[k] && out_block[k].0 == b)
                    .collect();
                v.sort_by_key(|&k| out_rules[k].prio);
                v
            };
            let mine = of(bi, &keep);
            if mine.is_empty() || !mine.iter().all(|&k| out_block[k].1) {
                continue;
            }
            let into = (0..bi).map(|ai| of(ai, &keep)).find(|theirs| {
                theirs.len() == mine.len()
                    && theirs
                        .iter()
                        .zip(&mine)
                        .all(|(&t, &m)| self.cf_same_rule(sheet, &out_rules[t], &out_rules[m]))
            });
            if let Some(theirs) = into {
                for (&t, &m) in theirs.iter().zip(&mine) {
                    let more = std::mem::take(&mut out_rules[m].applies);
                    join_areas(&mut out_rules[t].applies, more);
                    keep[m] = false;
                }
            }
        }
        let mut k = 0;
        out_rules.retain(|_| {
            k += 1;
            keep[k - 1]
        });

        placed.sort_by_key(|x| x.1);
        let unit_of = |p: i32| {
            wholes
                .iter()
                .find(|w| w.0 <= p && p <= w.1)
                .map(|w| w.0 + (p - w.0) / w.2 * w.2)
        };
        let mut units: Vec<(i32, Vec<usize>)> = Vec::new();
        let mut merged: Vec<(CfRule, i32)> = Vec::new();
        let mut copies: Vec<(i32, Vec<(CfRule, i32)>)> = Vec::new();
        for (r, p) in placed {
            match unit_of(p) {
                Some(u) => match copies.iter_mut().find(|c| c.0 == u) {
                    Some(c) => match c.1.iter_mut().find(|m| self.cf_same_rule(sheet, &m.0, &r)) {
                        Some(m) => join_areas(&mut m.0.applies, r.applies),
                        None => c.1.push((r, p)),
                    },
                    None => copies.push((u, vec![(r, p)])),
                },
                None => match merged.iter_mut().enumerate().find(|(k, m)| {
                    !units.iter().any(|u| u.1.contains(k)) && self.cf_same_rule(sheet, &m.0, &r)
                }) {
                    Some((_, m)) => join_areas(&mut m.0.applies, r.applies),
                    None => merged.push((r, p)),
                },
            }
        }
        for (u, copy) in copies {
            let into = units.iter().find(|x| {
                x.1.len() == copy.len()
                    && x.1
                        .iter()
                        .zip(&copy)
                        .all(|(&k, c)| self.cf_same_rule(sheet, &merged[k].0, &c.0))
            });
            match into {
                Some(x) => {
                    for (&k, (r, _)) in x.1.clone().iter().zip(copy) {
                        join_areas(&mut merged[k].0.applies, r.applies);
                    }
                }
                None => {
                    let ks = (merged.len()..merged.len() + copy.len()).collect();
                    merged.extend(copy);
                    units.push((u, ks));
                }
            }
        }
        merged.sort_by_key(|x| x.1);
        let placed = merged;
        let mut out_dvs: Vec<Dv> = Vec::new();
        let mut extra_dv: Vec<(SheetId, Dv)> = Vec::new();
        for dv in dvs {
            let core = Core {
                applies: dv.applies.clone(),
                f: [dv.f1.clone(), dv.f2.clone()],
            };
            let pieces: Vec<Piece> = core
                .applies
                .iter()
                .enumerate()
                .flat_map(|(i, r)| pieces_of(&core, i, *r))
                .collect();
            let groups = self.cfv_groups(sheet, &core, &pieces, how);
            let all: Vec<Rect> = groups.iter().flat_map(|g| g.applies.clone()).collect();

            let moved_only = pieces.len() == dv.applies.len()
                && pieces.iter().all(|p| {
                    !p.fresh
                        && p.sheet.is_none()
                        && p.old == dv.applies[p.area]
                        && p.new.rows() == p.old.rows()
                        && p.new.cols() == p.old.cols()
                });
            let rereg = match how {
                Rewrite::Shift(s) if s.insert && s.limit.is_none() => {
                    grew(&dv.applies, &all)
                        && self.cf_line_insert_hosed(sheet, std::slice::from_ref(&core), &s)
                }
                Rewrite::Shift(s) => !moved_only || self.dv_refs_straddle(sheet, &core, &s),
                _ => false,
            };
            for (k, g) in groups.into_iter().enumerate() {
                let rereg =
                    rereg || !matches!(how, Rewrite::Shift(_)) && grew(&dv.applies, &g.applies);
                let mut d = dv.clone();
                d.applies = g.applies;
                d.f1 = g.f[0].clone();
                d.f2 = g.f[1].clone();
                if g.sheet == sheet && (k == 0 || rereg && matches!(how, Rewrite::Shift(_))) {
                    if rereg {
                        grown_dvs.push(d);
                    } else {
                        out_dvs.push(d);
                    }
                } else {
                    extra_dv.push((g.sheet, d));
                }
            }
        }
        let sh = self.cfv_sheet_mut(sheet);
        sh.rules = out_rules;
        out_dvs.extend(grown_dvs);
        sh.dvs = out_dvs;
        for (r, p) in placed {
            sh.add_rule_at(r, p);
        }
        sh.rules.sort_by_key(|r| r.prio);
        for (to, r) in extra {
            self.cfv_sheet_mut(to).add_rule_skipping(r, 2);
        }
        for (to, d) in extra_dv {
            self.cfv_sheet_mut(to).dvs.push(d);
        }
    }

    pub(crate) fn cfv_shift_all(&mut self, s: &Shift) {
        let sheets: Vec<SheetId> = self.cfv.keys().copied().collect();
        for sheet in sheets {
            let s = *s;
            self.cfv_rewrite_sheet(
                sheet,
                |_, area, r| {
                    if sheet == s.sheet {
                        shift_pieces(&s, r, area)
                    } else {
                        vec![Piece {
                            old: r,
                            new: r,
                            sheet: None,
                            area,
                            fresh: false,
                            read: None,
                            unit: None,
                        }]
                    }
                },
                Rewrite::Shift(s),
            );
        }
        let sh = s.sheet;
        if let Some(c) = self.cfv.get_mut(&sh) {
            c.rules.retain(|r| !r.applies.is_empty());
            c.dvs.retain(|d| !d.applies.is_empty());
        }
    }

    pub fn cfv_clear(&mut self, sheet: SheetId, hole: Rect, with_dv: bool) {
        let Some(sh) = self.cfv.get(&sheet) else {
            return;
        };
        let kept = (!with_dv).then(|| sh.dvs.clone());
        self.cfv_rewrite_sheet(
            sheet,
            |_, area, r| {
                r.subtract(&hole)
                    .into_iter()
                    .map(|p| Piece {
                        old: p,
                        new: p,
                        sheet: None,
                        area,
                        fresh: false,
                        read: None,
                        unit: None,
                    })
                    .collect()
            },
            Rewrite::Copy,
        );
        let sh = self.cfv_sheet_mut(sheet);
        if let Some(d) = kept {
            sh.dvs = d;
        }
    }

    pub(crate) fn cfv_cut(&mut self, src_sheet: SheetId, src: Rect, dst: CellAddr) {
        let d = (
            i64::from(dst.row) - i64::from(src.r0),
            i64::from(dst.col) - i64::from(src.c0),
        );
        let dst_rect = Rect::new(
            dst.row,
            dst.col,
            dst.row.saturating_add(src.rows() - 1),
            dst.col.saturating_add(src.cols() - 1),
        );

        if let Some(sh) = self.cfv.get_mut(&dst.sheet) {
            for r in &mut sh.rules {
                r.applies = r
                    .applies
                    .iter()
                    .flat_map(|a| a.subtract(&dst_rect))
                    .collect();
            }
            for v in &mut sh.dvs {
                v.applies = v
                    .applies
                    .iter()
                    .flat_map(|a| a.subtract(&dst_rect))
                    .collect();
            }
        }
        let others: Vec<SheetId> = self.cfv.keys().copied().collect();

        for sheet in others {
            if sheet == src_sheet {
                self.cfv_rewrite_sheet(
                    sheet,
                    |_, area, r| move_pieces(src, d.0, d.1, dst.sheet, r, area),
                    Rewrite::Cut(src_sheet, src, d),
                );
            } else {
                self.cfv_rewrite_sheet(
                    sheet,
                    |_, area, r| {
                        vec![Piece {
                            old: r,
                            new: r,
                            sheet: None,
                            area,
                            fresh: false,
                            read: None,
                            unit: None,
                        }]
                    },
                    Rewrite::Cut(src_sheet, src, d),
                );
            }
        }
        for sh in self.cfv.values_mut() {
            sh.rules.retain(|r| !r.applies.is_empty());
            sh.dvs.retain(|d| !d.applies.is_empty());
        }
    }

    pub(crate) fn cfv_copy(
        &mut self,
        src_sheet: SheetId,
        src: Rect,
        dst_sheet: SheetId,
        dst_rect: Rect,
        fill: bool,
        with_dv: bool,
    ) {
        let Some(sh) = self.cfv.get(&src_sheet) else {
            return;
        };
        let rules = sh.rules.clone();
        let dvs = if with_dv { sh.dvs.clone() } else { Vec::new() };

        let mut tiles: Vec<(Rect, Rect)> = Vec::new();
        let mut r = dst_rect.r0;
        while r <= dst_rect.r1 {
            let mut c = dst_rect.c0;
            while c <= dst_rect.c1 {
                let h = src.rows().min(dst_rect.r1 - r + 1);
                let w = src.cols().min(dst_rect.c1 - c + 1);
                tiles.push((
                    Rect::new(src.r0, src.c0, src.r0 + h - 1, src.c0 + w - 1),
                    Rect::new(r, c, r + h - 1, c + w - 1),
                ));
                c += src.cols();
            }
            r += src.rows();
        }
        let make = |wb: &Workbook, applies: &[Rect], f: [Option<String>; 2]| -> Vec<Group> {
            let core = Core {
                applies: applies.to_vec(),
                f,
            };
            let mut pieces = Vec::new();
            for (t_src, t_dst) in &tiles {
                for (i, a) in applies.iter().enumerate() {
                    if let Some(x) = a.intersection(t_src) {
                        let dr = i64::from(t_dst.r0) - i64::from(t_src.r0);
                        let dc = i64::from(t_dst.c0) - i64::from(t_src.c0);
                        let sh = |v: u32, d: i64| (i64::from(v) + d) as u32;
                        pieces.push(Piece {
                            old: x,
                            new: Rect::new(sh(x.r0, dr), sh(x.c0, dc), sh(x.r1, dr), sh(x.c1, dc)),
                            sheet: Some(dst_sheet),
                            area: i,
                            fresh: false,
                            read: None,
                            unit: None,
                        });
                    }
                }
            }
            wb.cfv_groups(src_sheet, &core, &pieces, Rewrite::Copy)
        };
        let mut new_rules: Vec<CfRule> = Vec::new();
        let mut fills: Vec<(usize, Vec<Rect>)> = Vec::new();
        for (i, rule) in rules.iter().enumerate() {
            let groups = make(self, &rule.applies, [rule.f1.clone(), rule.f2.clone()]);
            for g in groups {
                if fill && src_sheet == dst_sheet {
                    fills.push((i, g.applies));
                } else {
                    let mut n = rule.clone();
                    n.applies = g.applies;
                    n.f1 = g.f[0].clone();
                    n.f2 = g.f[1].clone();
                    new_rules.push(n);
                }
            }
        }
        let mut new_dvs: Vec<Dv> = Vec::new();
        let mut dv_fills: Vec<(usize, Vec<Rect>)> = Vec::new();
        for (i, dv) in dvs.iter().enumerate() {
            let groups = make(self, &dv.applies, [dv.f1.clone(), dv.f2.clone()]);
            for g in groups {
                if fill && src_sheet == dst_sheet {
                    dv_fills.push((i, g.applies));
                } else {
                    let mut n = dv.clone();
                    n.applies = g.applies;
                    n.f1 = g.f[0].clone();
                    n.f2 = g.f[1].clone();
                    new_dvs.push(n);
                }
            }
        }
        if fill && src_sheet == dst_sheet {
            let sh = self.cfv_sheet_mut(dst_sheet);

            for (i, rects) in fills {
                let id = rules[i].id;
                for r in &mut sh.rules {
                    if r.id != id {
                        r.applies = r
                            .applies
                            .iter()
                            .flat_map(|a| subtract_all(a, &rects))
                            .collect();
                    }
                }
                if let Some(r) = sh.rules.iter_mut().find(|r| r.id == id) {
                    r.applies.extend(rects);
                    r.applies = coalesce(std::mem::take(&mut r.applies));
                }
            }
            for (i, rects) in dv_fills {
                let key = dvs[i].applies.clone();
                for d in &mut sh.dvs {
                    if d.applies != key {
                        d.applies = d
                            .applies
                            .iter()
                            .flat_map(|a| subtract_all(a, &rects))
                            .collect();
                    }
                }
                if let Some(d) = sh.dvs.iter_mut().find(|d| d.applies == key) {
                    d.applies.extend(rects);
                    d.applies = coalesce(std::mem::take(&mut d.applies));
                }
            }
            sh.rules.retain(|r| !r.applies.is_empty());
            sh.dvs.retain(|d| !d.applies.is_empty());
            return;
        }
        let sh = self.cfv_sheet_mut(dst_sheet);
        for r in &mut sh.rules {
            r.applies = r
                .applies
                .iter()
                .flat_map(|a| a.subtract(&dst_rect))
                .collect();
        }
        sh.rules.retain(|r| !r.applies.is_empty());
        for d in &mut sh.dvs {
            d.applies = d
                .applies
                .iter()
                .flat_map(|a| a.subtract(&dst_rect))
                .collect();
        }
        sh.dvs.retain(|d| !d.applies.is_empty());

        let n = new_rules.len() as i32;
        for r in &mut sh.rules {
            r.prio += n;
        }
        for (k, r) in (1..).zip(new_rules) {
            let id = sh.add_rule_at(r, k);
            let pos = sh.index_of(id).unwrap_or(0);
            let rule = sh.rules.remove(pos);
            sh.rules.insert(k as usize - 1, rule);
        }

        for n in new_dvs {
            let into = self
                .cfv
                .get(&dst_sheet)
                .and_then(|sh| sh.dvs.iter().position(|d| self.dv_same(dst_sheet, d, &n)));
            match into {
                Some(k) => {
                    let mut d = self.cfv_sheet_mut(dst_sheet).dvs[k].clone();
                    let (r0, c0) = d.anchor();
                    join_areas(&mut d.applies, n.applies);
                    let (r1, c1) = d.anchor();
                    if (r0, c0) != (r1, c1) {
                        let (from, to) = (
                            CellAddr::new(dst_sheet, r0, c0),
                            CellAddr::new(dst_sheet, r1, c1),
                        );
                        for f in [&mut d.f1, &mut d.f2] {
                            if let Some(t) = f.as_deref().filter(|t| t.starts_with('=')) {
                                if let Some(x) = self.cfv_retarget(t, from, to) {
                                    *f = Some(x);
                                }
                            }
                        }
                    }
                    self.cfv_sheet_mut(dst_sheet).dvs[k] = d;
                }
                None => self.cfv_sheet_mut(dst_sheet).dvs.push(n),
            }
        }
    }

    fn dv_same(&self, sheet: SheetId, a: &Dv, b: &Dv) -> bool {
        let (ar, ac) = a.anchor();
        let (br, bc) = b.anchor();
        let (ha, hb) = (CellAddr::new(sheet, ar, ac), CellAddr::new(sheet, br, bc));
        let mut x = b.clone();
        x.applies.clone_from(&a.applies);
        for f in [&mut x.f1, &mut x.f2] {
            if let Some(t) = f.as_deref().filter(|t| t.starts_with('=')) {
                if let Some(n) = self.cfv_retarget(t, hb, ha) {
                    *f = Some(n);
                }
            }
        }
        let mut y = a.clone();
        for f in [&mut y.f1, &mut y.f2] {
            if let Some(t) = f.as_deref().filter(|t| t.starts_with('=')) {
                if let Some(n) = self.cfv_retarget(t, ha, ha) {
                    *f = Some(n);
                }
            }
        }
        x == y
    }
}

fn subtract_all(a: &Rect, holes: &[Rect]) -> Vec<Rect> {
    let mut cur = vec![*a];
    for h in holes {
        cur = cur.iter().flat_map(|x| x.subtract(h)).collect();
    }
    cur
}

fn grew(old: &[Rect], new: &[Rect]) -> bool {
    let cells = |v: &[Rect]| -> u64 {
        v.iter()
            .map(|r| u64::from(r.rows()) * u64::from(r.cols()))
            .sum()
    };
    cells(new) > cells(old)
}

impl Workbook {
    pub fn cfv_special_cells(&self, sheet: SheetId, rects: &[Rect], ty: i32) -> Vec<Rect> {
        let Some(sh) = self.cfv.get(&sheet) else {
            return Vec::new();
        };
        let Some(first) = rects.first() else {
            return Vec::new();
        };
        let single = rects.len() == 1 && first.rows() == 1 && first.cols() == 1;
        let within = |r: &Rect| -> Vec<Rect> {
            if single {
                vec![*r]
            } else {
                rects.iter().filter_map(|x| r.intersection(x)).collect()
            }
        };
        let (ref_r, ref_c) = (first.r0, first.c0);
        let mut found: Vec<Rect> = Vec::new();
        let mut take = |applies: &[Rect], limit: bool| {
            let mut mine: Vec<Rect> = Vec::new();
            for a in applies {
                let pieces = if limit { within(a) } else { vec![*a] };
                for p in pieces {
                    for x in subtract_all(&p, &found) {
                        mine.push(x);
                    }
                }
            }
            found.extend(coalesce(mine));
        };
        match ty {
            -4172 => {
                let mut rules: Vec<&CfRule> = sh.rules.iter().collect();
                rules.sort_by_key(|r| r.seq);
                for r in rules {
                    take(&r.applies, true);
                }
            }
            -4174 => {
                let mut all: Vec<R4> = Vec::new();
                for d in &sh.dvs {
                    for a in &d.applies {
                        for p in within(a) {
                            all.push([p.r0.into(), p.r1.into(), p.c0.into(), p.c1.into()]);
                        }
                    }
                }
                minimize(&mut all);
                found.extend(
                    all.iter()
                        .map(|x| Rect::new(x[0] as u32, x[2] as u32, x[1] as u32, x[3] as u32)),
                );
            }
            -4173 => {
                let hits: Vec<&CfRule> = sh
                    .rules
                    .iter()
                    .filter(|r| r.applies.iter().any(|a| a.contains(ref_r, ref_c)))
                    .collect();
                if let [one] = hits[..] {
                    if one.dxf == Dxf::default() {
                        take(&one.applies, false);
                    }
                }
            }
            _ => {
                let at = CellAddr::new(sheet, ref_r, ref_c);
                if let Some(me) = sh
                    .dvs
                    .iter()
                    .find(|d| d.applies.iter().any(|a| a.contains(ref_r, ref_c)))
                {
                    let norm = |d: &Dv| -> Dv {
                        let (ar, ac) = d.anchor();
                        let from = CellAddr::new(sheet, ar, ac);
                        let mut n = d.clone();
                        n.applies = Vec::new();
                        for f in [&mut n.f1, &mut n.f2] {
                            if let Some(t) = f.as_deref().filter(|t| t.starts_with('=')) {
                                *f = self.cfv_retarget(t, from, at);
                            }
                        }
                        n
                    };
                    let mine = norm(me);
                    for d in &sh.dvs {
                        if norm(d) == mine {
                            take(&d.applies, true);
                        }
                    }
                }
            }
        }
        found
    }
}

pub(crate) struct CfvRoots(Vec<RootSlot>);

struct RootSlot {
    sheet: SheetId,
    dv: bool,
    index: usize,
    slot: usize,
    anchor: CellAddr,
    root: Node,
}

impl Workbook {
    pub(crate) fn cfv_roots(&self) -> CfvRoots {
        let mut out = Vec::new();
        for (&sheet, sh) in &self.cfv {
            let mut add = |dv: bool, index: usize, applies: &[Rect], f: [&Option<String>; 2]| {
                let (r, c) = bbox_tl(applies);
                let anchor = CellAddr::new(sheet, r, c);
                for (slot, t) in f.into_iter().enumerate() {
                    let Some(t) = t.as_deref().and_then(|t| t.strip_prefix('=')) else {
                        continue;
                    };
                    if let Ok((f, _)) = self.compile_normalized(t, anchor) {
                        out.push(RootSlot {
                            sheet,
                            dv,
                            index,
                            slot,
                            anchor,
                            root: f.root.clone(),
                        });
                    }
                }
            };
            for (i, r) in sh.rules.iter().enumerate() {
                add(false, i, &r.applies, [&r.f1, &r.f2]);
            }
            for (i, d) in sh.dvs.iter().enumerate() {
                add(true, i, &d.applies, [&d.f1, &d.f2]);
            }
        }
        CfvRoots(out)
    }

    pub(crate) fn cfv_apply_roots(&mut self, roots: CfvRoots, map: &mut dyn FnMut(&Node) -> Node) {
        for s in roots.0 {
            let text = format!("={}", print_node(self, &map(&s.root), s.anchor));
            let Some(sh) = self.cfv.get_mut(&s.sheet) else {
                continue;
            };
            let target = if s.dv {
                sh.dvs.get_mut(s.index).map(|d| [&mut d.f1, &mut d.f2])
            } else {
                sh.rules.get_mut(s.index).map(|r| [&mut r.f1, &mut r.f2])
            };
            if let Some(t) = target {
                *t[s.slot] = Some(text);
            }
        }
    }
}

impl Workbook {
    pub fn paste_validation(
        &mut self,
        src_sheet: SheetId,
        src: Rect,
        dst_sheet: SheetId,
        dst: Rect,
    ) {
        let rules = self.cfv.get(&dst_sheet).map(|s| s.rules.clone());
        let to = Rect::new(
            dst.r0,
            dst.c0,
            dst.r1.max(dst.r0 + src.rows() - 1),
            dst.c1.max(dst.c0 + src.cols() - 1),
        );
        if let Some(sh) = self.cfv.get_mut(&dst_sheet) {
            for d in &mut sh.dvs {
                d.applies = d
                    .applies
                    .iter()
                    .flat_map(|x| subtract_all(x, &[to]))
                    .collect();
            }
            sh.dvs.retain(|d| !d.applies.is_empty());
        }
        self.cfv_copy(src_sheet, src, dst_sheet, to, false, true);
        if let (Some(rules), Some(sh)) = (rules, self.cfv.get_mut(&dst_sheet)) {
            sh.rules = rules;
        }
    }
}
