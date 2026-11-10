use crate::address::{CellAddr, Rect, SheetId, MAX_COLS, MAX_ROWS};
use crate::cfv::{CfRule, Dv, Dxf};
use crate::formula::{CRef, Node, SheetRef};
use crate::formula_print::print_node;
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
}

pub(crate) struct Group {
    pub sheet: SheetId,
    pub applies: Vec<Rect>,
    pub f: [Option<String>; 2],
}

struct Built {
    sheet: SheetId,
    rects: Vec<(usize, Rect)>,
    roots: [Option<Node>; 2],
    lit: [Option<String>; 2],
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
    }));
    out
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
        })
        .collect();
    if let Some(i) = r.intersection(&src) {
        let sh = |v: u32, d: i64| (i64::from(v) + d).clamp(0, i64::from(u32::MAX)) as u32;
        out.push(Piece {
            old: i,
            new: Rect::new(sh(i.r0, dr), sh(i.c0, dc), sh(i.r1, dr), sh(i.c1, dc)),
            sheet: Some(dst_sheet),
            area,
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
        let (ar, ac) = bbox_tl(&core.applies);
        let anchor = CellAddr::new(home, ar, ac);
        let compiled: [Option<Node>; 2] = std::array::from_fn(|i| {
            let t = core.f[i].as_deref()?.strip_prefix('=')?;
            self.compile_normalized(t, anchor)
                .ok()
                .map(|(f, _)| f.root.clone())
        });
        let mut built: Vec<Built> = Vec::new();
        for p in pieces {
            let sheet = p.sheet.unwrap_or(home);
            let old = CellAddr::new(home, p.old.r0, p.old.c0);
            let new = CellAddr::new(sheet, p.new.r0, p.new.c0);
            let roots: [Option<Node>; 2] = std::array::from_fn(|i| {
                let root = compiled[i].as_ref()?;
                Some(match how {
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

            let same = |b: &Built| {
                b.sheet == sheet
                    && (0..2).all(|i| match (&b.roots[i], &roots[i]) {
                        (Some(x), Some(y)) => print_node(self, x, new) == print_node(self, y, new),
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
                let areas: Vec<Rect> = {
                    let mut by: Vec<usize> = b.rects.iter().map(|x| x.0).collect();
                    by.dedup();
                    let mut all = Vec::new();
                    for a in by {
                        all.extend(coalesce(
                            b.rects.iter().filter(|x| x.0 == a).map(|x| x.1).collect(),
                        ));
                    }
                    all
                };
                let (nr, nc) = bbox_tl(&areas);
                let host = CellAddr::new(b.sheet, nr, nc);
                let f = std::array::from_fn(|i| match &b.roots[i] {
                    Some(root) => Some(format!("={}", print_node(self, root, host))),
                    None => b.lit[i].clone(),
                });
                Group {
                    sheet: b.sheet,
                    applies: areas,
                    f,
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
        for t in core.f.iter().flatten() {
            let Some(t) = t.strip_prefix('=') else {
                continue;
            };
            let Ok((f, _)) = self.compile_normalized(t, anchor) else {
                continue;
            };
            map_refs(&f.root, &mut |sr: &SheetRef, cref: &CRef| {
                if targets_sheet(sr, home, s.sheet) {
                    let rect = match *cref {
                        CRef::Cell { row, col } => resolve(row, anchor.row, MAX_ROWS)
                            .zip(resolve(col, anchor.col, MAX_COLS))
                            .map(|(r, c)| Rect::new(r, c, r, c)),
                        CRef::Area { r0, c0, r1, c1 } => {
                            let r = |a, b| {
                                resolve(a, anchor.row, MAX_ROWS)
                                    .zip(resolve(b, anchor.row, MAX_ROWS))
                            };
                            let c = |a, b| {
                                resolve(a, anchor.col, MAX_COLS)
                                    .zip(resolve(b, anchor.col, MAX_COLS))
                            };
                            r(r0, r1).zip(c(c0, c1)).map(|((a, b), (x, y))| {
                                Rect::new(a.min(b), x.min(y), a.max(b), x.max(y))
                            })
                        }
                        _ => None,
                    };
                    if let Some(rect) = rect {
                        bounds = Some(bounds.map_or(rect, |b| b.union_bounds(&rect)));
                    }
                }
                None
            });
        }
        let Some(b) = bounds else {
            return false;
        };
        let (l0, l1) = s.limit.unwrap_or((0, u32::MAX));
        let moved = match s.axis {
            Axis::Rows => Rect::new(s.at, l0, MAX_ROWS - 1, l1.min(MAX_COLS - 1)),
            Axis::Cols => Rect::new(l0, s.at, l1.min(MAX_ROWS - 1), MAX_COLS - 1),
        };
        b.intersection(&moved).is_some() && !moved.contains_rect(&b)
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
        for rule in rules {
            let core = Core {
                applies: rule.applies.clone(),
                f: [rule.f1.clone(), rule.f2.clone()],
            };
            let pieces: Vec<Piece> = core
                .applies
                .iter()
                .enumerate()
                .flat_map(|(i, r)| pieces_of(&core, i, *r))
                .collect();
            let groups = self.cfv_groups(sheet, &core, &pieces, how);
            for (k, g) in groups.into_iter().enumerate() {
                let mut r = rule.clone();
                if grew(&rule.applies, &g.applies) {
                    r.seq = next_seq;
                    next_seq += 1;
                }
                r.applies = g.applies;
                r.f1 = g.f[0].clone();
                r.f2 = g.f[1].clone();
                if k == 0 && g.sheet == sheet {
                    out_rules.push(r);
                } else {
                    extra.push((g.sheet, r));
                }
            }
        }
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
            for (k, g) in groups.into_iter().enumerate() {
                let grown = grew(&dv.applies, &g.applies);
                let mut d = dv.clone();
                d.applies = g.applies;
                d.f1 = g.f[0].clone();
                d.f2 = g.f[1].clone();
                d.touched |=
                    matches!(how, Rewrite::Shift(s) if self.dv_refs_straddle(sheet, &core, &s));
                if k == 0 && g.sheet == sheet {
                    if grown {
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
        for (k, r) in new_rules.into_iter().enumerate() {
            let id = sh.add_rule(r);

            let pos = sh.index_of(id).unwrap_or(0);
            let rule = sh.rules.remove(pos);
            sh.rules.insert(k, rule);
        }
        sh.renumber();
        sh.dvs.extend(new_dvs);
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
                for touched in [false, true] {
                    for d in sh.dvs.iter().filter(|d| d.touched == touched) {
                        take(&d.applies, true);
                    }
                }
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
                        n.touched = false;
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
