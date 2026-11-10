use crate::structure::{Axis, Shift};
use crate::{Rect, SheetId, Workbook};

pub const POINT_KINDS: [&str; 6] = [
    "Negative",
    "Markers",
    "Highpoint",
    "Lowpoint",
    "Firstpoint",
    "Lastpoint",
];

#[derive(Clone, Debug, PartialEq)]
pub struct Sparkline {
    pub row: u32,
    pub col: u32,
    pub src_sheet: SheetId,
    pub src: Rect,
}

#[derive(Clone, Copy, Debug, PartialEq)]
pub struct PointFmt {
    pub visible: bool,

    pub color: i32,
}

#[derive(Clone, Debug, PartialEq)]
pub struct SparkGroup {
    pub id: u32,

    pub slot: u32,

    pub kind: i32,

    pub abs: bool,

    pub src: Option<(SheetId, Rect)>,
    pub lines: Vec<Sparkline>,

    pub plot_by: i32,
    pub line_weight: f64,

    pub blanks: i32,
    pub hidden: bool,
    pub series_color: i32,
    pub points: [PointFmt; 6],

    pub min_type: i32,
    pub max_type: i32,
    pub min_value: f64,
    pub max_value: f64,
    pub right_to_left: bool,
    pub axis_visible: bool,
    pub axis_color: i32,
}

impl SparkGroup {
    pub fn new(id: u32, kind: i32, abs: bool) -> SparkGroup {
        SparkGroup {
            id,
            slot: id,
            kind,
            abs,
            src: None,
            lines: Vec::new(),
            plot_by: 0,
            line_weight: 0.75,
            blanks: 1,
            hidden: false,
            series_color: 9_592_887,
            points: [PointFmt {
                visible: false,
                color: 208,
            }; 6],
            min_type: 2,
            max_type: 2,
            min_value: 0.0,
            max_value: 0.0,
            right_to_left: false,
            axis_visible: false,
            axis_color: 0,
        }
    }

    pub fn location(&self) -> Option<Rect> {
        let f = self.lines.first()?;
        let mut r = Rect::cell(f.row, f.col);
        for l in &self.lines {
            r = r.union_bounds(&Rect::cell(l.row, l.col));
        }
        Some(r)
    }

    pub fn sources_tile(&self) -> bool {
        let Some(f) = self.lines.first() else {
            return true;
        };
        let mut bound = f.src;
        let mut area = 0u64;
        for l in &self.lines {
            bound = bound.union_bounds(&l.src);
            area += u64::from(l.src.rows()) * u64::from(l.src.cols());
        }
        self.lines.iter().all(|l| l.src_sheet == f.src_sheet)
            && u64::from(bound.rows()) * u64::from(bound.cols()) == area
            && (self.lines.len() > 4096
                || self.lines.iter().enumerate().all(|(i, a)| {
                    self.lines[i + 1..]
                        .iter()
                        .all(|b| a.src.intersection(&b.src).is_none())
                }))
    }

    pub fn cells_fill(&self) -> bool {
        self.location()
            .is_some_and(|r| u64::from(r.rows()) * u64::from(r.cols()) == self.lines.len() as u64)
    }

    pub fn touches(&self, r: &Rect) -> bool {
        self.lines.iter().any(|l| r.contains(l.row, l.col))
    }
}

impl Workbook {
    pub fn spark_groups(&self, sheet: SheetId) -> &[SparkGroup] {
        self.sparklines.get(&sheet).map_or(&[], Vec::as_slice)
    }

    pub fn spark_groups_mut(&mut self, sheet: SheetId) -> &mut Vec<SparkGroup> {
        self.sparklines.entry(sheet).or_default()
    }

    pub fn next_spark_id(&mut self) -> u32 {
        self.spark_next += 1;
        self.spark_next
    }

    pub(crate) fn spark_shift(&mut self, s: &Shift) {
        let rows = s.axis == Axis::Rows;
        let mapspan = |a: u32, b: u32| s.map_span(a, b);
        let across = |r: &Rect| if rows { (r.c0, r.c1) } else { (r.r0, r.r1) };
        let in_limit = |r: &Rect| match s.limit {
            None => true,
            Some((p0, p1)) => {
                let (a, b) = across(r);
                a >= p0 && b <= p1
            }
        };
        let shift_rect = |r: Rect| -> Option<Rect> {
            if !in_limit(&r) {
                return Some(r);
            }
            let (a, b) = if rows { (r.r0, r.r1) } else { (r.c0, r.c1) };
            let (na, nb) = mapspan(a, b)?;
            let mut n = r;
            if rows {
                n.r0 = na;
                n.r1 = nb;
            } else {
                n.c0 = na;
                n.c1 = nb;
            }
            Some(n)
        };
        let sheets: Vec<SheetId> = self.sparklines.keys().copied().collect();
        for sheet in sheets {
            let Some(groups) = self.sparklines.get_mut(&sheet) else {
                continue;
            };
            for g in groups.iter_mut() {
                if let Some((ss, r)) = &mut g.src {
                    if *ss == s.sheet {
                        if let Some(n) = shift_rect(*r) {
                            *r = n;
                        }
                    }
                }
                for l in &mut g.lines {
                    if l.src_sheet == s.sheet {
                        if let Some(n) = shift_rect(l.src) {
                            l.src = n;
                        }
                    }
                }
                if sheet == s.sheet {
                    g.lines
                        .retain_mut(|l| match shift_rect(Rect::cell(l.row, l.col)) {
                            Some(n) => {
                                l.row = n.r0;
                                l.col = n.c0;
                                true
                            }
                            None => false,
                        });
                    if s.insert && s.at > 0 {
                        Self::spark_grow(g, s);
                    }
                }
            }
            for g in groups.iter_mut() {
                if g.src.is_some() && !g.sources_tile() {
                    g.src = None;
                }
            }
            let mut gone: Vec<u32> = groups
                .iter()
                .filter(|g| g.lines.is_empty())
                .map(|g| g.slot)
                .collect();
            gone.sort_unstable_by(|a, b| b.cmp(a));
            groups.retain(|g| !g.lines.is_empty());
            self.spark_free.entry(sheet).or_default().extend(gone);
        }
    }

    fn spark_grow(g: &mut SparkGroup, s: &Shift) {
        let rows = s.axis == Axis::Rows;
        let pos = |l: &Sparkline| if rows { (l.row, l.col) } else { (l.col, l.row) };
        let mut grown = false;
        let mut out: Vec<Sparkline> = Vec::with_capacity(g.lines.len());
        for l in &g.lines {
            out.push(l.clone());
            let (along, cross) = pos(l);
            let in_limit = s.limit.is_none_or(|(p0, p1)| cross >= p0 && cross <= p1);
            if along + 1 != s.at || !in_limit {
                continue;
            }
            for d in 1..=s.count {
                let mut n = l.clone();
                if rows {
                    n.row += d;
                    n.src.r0 += d;
                    n.src.r1 += d;
                } else {
                    n.col += d;
                    n.src.c0 += d;
                    n.src.c1 += d;
                }
                out.push(n);
                grown = true;
            }
        }
        if grown {
            g.lines = out;
            g.src = None;
        }
    }

    pub(crate) fn spark_drop_sheet(&mut self, id: SheetId) {
        self.sparklines.remove(&id);
    }

    pub(crate) fn spark_copy_sheet(&mut self, src: SheetId, new: SheetId) {
        if let Some(mut gs) = self.sparklines.get(&src).cloned() {
            gs.sort_by_key(|g| std::cmp::Reverse(g.slot));
            for g in &mut gs {
                g.id = self.next_spark_id();
                g.slot = g.id;
                g.src = None;
                for l in &mut g.lines {
                    if l.src_sheet == src {
                        l.src_sheet = new;
                    }
                }
            }
            self.sparklines.insert(new, gs);
        }
    }
}

impl Workbook {
    pub fn spark_clear(&mut self, sheet: SheetId, r: &Rect) {
        if let Some(gs) = self.sparklines.get_mut(&sheet) {
            for g in gs.iter_mut() {
                let n = g.lines.len();
                g.lines.retain(|l| !r.contains(l.row, l.col));
                if g.lines.len() != n {
                    g.src = None;
                }
            }
        }
        self.spark_remove(sheet, |g| g.lines.is_empty());
    }

    pub fn spark_remove(&mut self, sheet: SheetId, f: impl Fn(&SparkGroup) -> bool) {
        let Some(gs) = self.sparklines.get_mut(&sheet) else {
            return;
        };
        let mut gone: Vec<u32> = gs.iter().filter(|g| f(g)).map(|g| g.slot).collect();
        gone.sort_unstable_by(|a, b| b.cmp(a));
        gs.retain(|g| !f(g));
        self.spark_free.entry(sheet).or_default().extend(gone);
    }

    pub fn spark_take_slot(&mut self, sheet: SheetId, id: u32) -> u32 {
        self.spark_free
            .get_mut(&sheet)
            .and_then(Vec::pop)
            .unwrap_or(id)
    }
}
