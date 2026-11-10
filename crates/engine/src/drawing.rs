use rustc_hash::FxHashMap;

use crate::address::{CellAddr, Rect, SheetId};
use crate::structure::{Axis, Shift};
use crate::workbook::Workbook;

pub const EMU_PT: f64 = 12700.0;

pub fn pt_to_emu(pt: f64) -> i64 {
    (pt * EMU_PT).round() as i64
}

#[derive(Clone, Copy, Debug, PartialEq, Eq, Default)]
pub struct Anchor {
    pub row: u32,
    pub col: u32,
    pub row_off: i64,
    pub col_off: i64,
}

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum Pos {
    Both { from: Anchor, to: Anchor },

    One { from: Anchor, w: i64, h: i64 },

    Abs { x: i64, y: i64, w: i64, h: i64 },
}

impl Pos {
    pub fn placement(&self) -> i32 {
        match self {
            Pos::Both { .. } => 1,
            Pos::One { .. } => 2,
            Pos::Abs { .. } => 3,
        }
    }
}

#[derive(Clone, Debug, PartialEq)]
pub enum ShapeKind {
    Auto(i32),
    TextBox,
    Line,
    Picture,
    Chart,

    Slicer,

    Timeline,

    Group(Vec<u32>),
}

#[derive(Clone, Debug, PartialEq)]
pub enum Part {
    None,
    Refs(Vec<(SheetId, Rect)>),

    Dead,

    External(String),
    Text(String),
}

#[derive(Clone, Debug, PartialEq)]
pub struct Series {
    pub name: Part,
    pub x: Part,
    pub values: Part,
    pub chart_type: i32,
    pub axis_group: i32,

    pub fmt: SeriesFmt,
}

#[derive(Clone, Debug, PartialEq, Default)]
pub struct SeriesFmt {
    pub marker_style: Option<i32>,
    pub marker_size: Option<i32>,
    pub smooth: Option<bool>,
    pub marker_bg: Option<i32>,
    pub marker_fg: Option<i32>,
    pub fill_rgb: Option<i32>,
    pub fill_visible: Option<bool>,
    pub line_rgb: Option<i32>,
    pub line_weight: Option<f32>,
    pub line_visible: Option<bool>,
    pub dash: Option<i32>,
    pub interior: Option<i32>,
    pub border_rgb: Option<i32>,
    pub border_style: Option<i32>,
    pub labels: Option<DataLabels>,

    pub point_labels: Vec<u16>,
    pub trends: Vec<Trendline>,
    pub error_bars: Option<ErrorBars>,
}

#[derive(Clone, Debug, PartialEq)]
pub struct DataLabels {
    pub show_value: bool,
    pub show_series: bool,
    pub show_category: bool,
    pub show_percent: bool,
    pub show_legend_key: bool,
    pub show_bubble: bool,

    pub position: Option<i32>,
    pub number_format: Option<String>,
}

impl Default for DataLabels {
    fn default() -> DataLabels {
        DataLabels {
            show_value: true,
            show_series: false,
            show_category: false,
            show_percent: false,
            show_legend_key: false,
            show_bubble: false,
            position: None,
            number_format: None,
        }
    }
}

#[derive(Clone, Debug, PartialEq)]
pub struct Trendline {
    pub kind: i32,
    pub order: i32,
    pub period: i32,
    pub forward: f64,
    pub backward: f64,
    pub name: Option<String>,
    pub intercept: Option<f64>,
    pub display_equation: bool,
    pub display_r2: bool,
}

#[derive(Clone, Copy, Debug, PartialEq)]
pub struct ErrorBars {
    pub end_style: i32,

    pub calls: u8,

    pub x_calls: u8,
}

impl Default for ErrorBars {
    fn default() -> ErrorBars {
        ErrorBars {
            end_style: 1,
            calls: 0,
            x_calls: 0,
        }
    }
}

#[derive(Clone, Debug, PartialEq)]
pub struct Chart {
    pub chart_type: i32,

    pub plot_by: i32,
    pub series: Vec<Series>,
    pub has_title: bool,
    pub title: String,

    pub title_ref: Option<Part>,
    pub has_legend: bool,

    pub legend_pos: i32,

    pub axis_hidden: [bool; 2],

    pub blanks: i32,
    pub visible_only: bool,

    pub source: Option<(SheetId, Rect)>,

    pub axes: [ChartAxis; 2],
}

#[derive(Clone, Debug, PartialEq)]
pub struct ChartAxis {
    pub has_title: bool,
    pub title: String,
    pub grid_major: bool,
    pub grid_minor: bool,

    pub min: Option<f64>,
    pub max: Option<f64>,
    pub major: Option<f64>,
    pub minor: Option<f64>,
    pub reverse: bool,
    pub major_tick: i32,
    pub minor_tick: i32,
    pub tick_pos: i32,

    pub tick_nf: Option<String>,

    pub log: bool,
}

impl ChartAxis {
    pub fn new(grid_major: bool) -> ChartAxis {
        ChartAxis {
            has_title: false,
            title: String::new(),
            grid_major,
            grid_minor: false,
            min: None,
            max: None,
            major: None,
            minor: None,
            reverse: false,
            major_tick: 3,
            minor_tick: -4142,
            tick_pos: 4,
            tick_nf: None,
            log: false,
        }
    }
}

impl Chart {
    pub fn parts_mut(&mut self) -> Vec<&mut Part> {
        let mut v: Vec<&mut Part> = Vec::new();
        for s in &mut self.series {
            v.push(&mut s.name);
            v.push(&mut s.x);
            v.push(&mut s.values);
        }
        if let Some(t) = &mut self.title_ref {
            v.push(t);
        }
        v
    }

    pub fn new() -> Chart {
        Chart {
            chart_type: 51,
            plot_by: 0,
            series: Vec::new(),
            has_title: false,
            title: String::new(),
            title_ref: None,
            has_legend: true,
            legend_pos: -4152,
            axis_hidden: [false; 2],
            blanks: 1,
            visible_only: true,
            source: None,
            axes: [ChartAxis::new(false), ChartAxis::new(true)],
        }
    }
}

impl Default for Chart {
    fn default() -> Chart {
        Chart::new()
    }
}

#[derive(Clone, Debug, PartialEq)]
pub struct Shape {
    pub id: u32,
    pub name: String,
    pub kind: ShapeKind,
    pub pos: Pos,
    pub visible: bool,
    pub text: String,
    pub chart: Option<Chart>,
    pub flip_h: bool,
    pub flip_v: bool,
    pub rotation: f64,
    pub locked: bool,
    pub alt_text: String,

    pub parent: Option<u32>,

    pub lock_aspect: bool,

    pub image: Option<Image>,

    pub props: ShapeProps,
}

#[derive(Clone, Debug, PartialEq)]
pub struct ShapeProps {
    pub black_white: i32,

    pub shape_style: Option<i32>,

    pub background_style: i32,
    pub decorative: bool,
    pub on_action: String,
    pub title: String,

    pub text_auto: i32,
}

impl Default for ShapeProps {
    fn default() -> ShapeProps {
        ShapeProps {
            black_white: 1,
            shape_style: None,
            background_style: 0,
            decorative: false,
            on_action: String::new(),
            title: String::new(),
            text_auto: 1,
        }
    }
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct Image {
    pub ext: String,
    pub bytes: Vec<u8>,

    pub linked: bool,

    pub source: String,

    pub embedded: bool,
}

#[derive(Clone, Debug, Default, PartialEq)]
pub struct SheetDrawing {
    pub shapes: Vec<Shape>,

    pub next_id: u32,

    pub listing: std::collections::BTreeMap<u32, Vec<u32>>,
}

impl SheetDrawing {
    pub fn renumber_in_z_order(&mut self) {
        let map: Vec<(u32, u32)> = self
            .shapes
            .iter()
            .enumerate()
            .map(|(i, s)| (s.id, i as u32 + 2))
            .collect();
        let new_id = |o: u32| map.iter().find(|m| m.0 == o).map(|m| m.1);
        for sh in &mut self.shapes {
            let old = sh.id;
            let Some(n) = new_id(old) else { continue };
            let suffix = format!(" {}", old.saturating_sub(1));
            if let Some(base) = sh.name.strip_suffix(&suffix) {
                if !base.is_empty() && !base.ends_with(|c: char| c.is_ascii_digit()) {
                    sh.name = format!("{base} {}", n - 1);
                }
            }
            sh.id = n;
            sh.parent = sh.parent.and_then(new_id);
            if let ShapeKind::Group(k) = &mut sh.kind {
                *k = k.iter().filter_map(|x| new_id(*x)).collect();
            }
        }
        self.next_id = self.shapes.len() as u32 + 2;
    }

    pub fn index_of(&self, id: u32) -> Option<usize> {
        self.shapes.iter().position(|s| s.id == id)
    }

    pub fn get(&self, id: u32) -> Option<&Shape> {
        self.shapes.iter().find(|s| s.id == id)
    }

    pub fn get_mut(&mut self, id: u32) -> Option<&mut Shape> {
        self.shapes.iter_mut().find(|s| s.id == id)
    }
}

pub type Emu4 = (i64, i64, i64, i64);

impl Workbook {
    pub fn drawing(&self, sheet: SheetId) -> Option<&SheetDrawing> {
        self.drawings.get(&sheet)
    }

    pub fn drawing_mut(&mut self, sheet: SheetId) -> &mut SheetDrawing {
        let d = self.drawings.entry(sheet).or_default();
        if d.next_id == 0 {
            d.next_id = 2;
        }
        d
    }

    fn row_h_emu(&self, sheet: SheetId, row: u32) -> i64 {
        pt_to_emu(self.row_height(sheet, row))
    }

    fn col_w_emu(&self, sheet: SheetId, col: u32) -> i64 {
        pt_to_emu(self.column_points(sheet, col))
    }

    pub fn row_top_emu(&self, sheet: SheetId, row: u32) -> i64 {
        (0..row).map(|r| self.row_h_emu(sheet, r)).sum()
    }

    pub fn col_left_emu(&self, sheet: SheetId, col: u32) -> i64 {
        (0..col).map(|c| self.col_w_emu(sheet, c)).sum()
    }

    pub fn anchor_xy(&self, sheet: SheetId, a: &Anchor) -> (i64, i64) {
        let x = self.col_left_emu(sheet, a.col) + a.col_off.clamp(0, self.col_w_emu(sheet, a.col));
        let y = self.row_top_emu(sheet, a.row) + a.row_off.clamp(0, self.row_h_emu(sheet, a.row));
        (x, y)
    }

    pub fn xy_anchor(&self, sheet: SheetId, x: i64, y: i64) -> Anchor {
        let find = |p: i64, size: &dyn Fn(u32) -> i64, max: u32| -> (u32, i64) {
            let mut acc = 0i64;
            let mut last_visible = 0;
            for i in 0..max {
                let s = size(i);
                if s > 0 {
                    last_visible = i;
                    if p < acc + s {
                        return (i, p - acc);
                    }
                }
                acc += s;
            }
            (last_visible, p - acc)
        };
        let (row, row_off) = find(
            y.max(0),
            &|r| self.row_h_emu(sheet, r),
            crate::address::MAX_ROWS,
        );
        let (col, col_off) = find(
            x.max(0),
            &|c| self.col_w_emu(sheet, c),
            crate::address::MAX_COLS,
        );
        Anchor {
            row,
            col,
            row_off,
            col_off,
        }
    }

    pub fn shape_rect_emu(&self, sheet: SheetId, pos: &Pos) -> Emu4 {
        match pos {
            Pos::Both { from, to } => {
                let (x1, y1) = self.anchor_xy(sheet, from);
                let (x2, y2) = self.anchor_xy(sheet, to);
                (x1, y1, (x2 - x1).max(0), (y2 - y1).max(0))
            }
            Pos::One { from, w, h } => {
                let (x, y) = self.anchor_xy(sheet, from);
                (x, y, *w, *h)
            }
            Pos::Abs { x, y, w, h } => (*x, *y, *w, *h),
        }
    }

    pub fn shape_cells(&self, sheet: SheetId, pos: &Pos) -> ((u32, u32), (u32, u32)) {
        let (x, y, w, h) = self.shape_rect_emu(sheet, pos);
        let a = match pos {
            Pos::Both { from, .. } | Pos::One { from, .. } => *from,
            Pos::Abs { .. } => self.xy_anchor(sheet, x, y),
        };
        let (b_row, b_col) = match pos {
            Pos::Both { to, .. } => (to.row, to.col),
            _ => {
                let b = self.xy_anchor(sheet, x + w, y + h);
                let (bx, by) = self.anchor_xy(sheet, &b);
                let c = self.xy_anchor(sheet, bx, by);
                (c.row, c.col)
            }
        };
        ((a.row, a.col), (b_row, b_col))
    }

    pub fn pos_for(&self, sheet: SheetId, placement: i32, r: Emu4) -> Pos {
        let (x, y, w, h) = r;
        match placement {
            1 => Pos::Both {
                from: self.xy_anchor(sheet, x, y),
                to: self.xy_anchor(sheet, x + w, y + h),
            },
            2 => Pos::One {
                from: self.xy_anchor(sheet, x, y),
                w,
                h,
            },
            _ => Pos::Abs { x, y, w, h },
        }
    }

    pub fn add_shape(
        &mut self,
        sheet: SheetId,
        kind: ShapeKind,
        base_name: &str,
        rect_pt: [f64; 4],
    ) -> u32 {
        let r = (
            pt_to_emu(rect_pt[0]),
            pt_to_emu(rect_pt[1]),
            pt_to_emu(rect_pt[2]),
            pt_to_emu(rect_pt[3]),
        );
        let pos = self.pos_for(sheet, 1, r);
        let d = self.drawing_mut(sheet);
        let id = d.next_id;
        d.next_id += 1;
        d.shapes.push(Shape {
            id,
            name: format!("{base_name} {}", id - 1),
            kind,
            pos,
            visible: true,
            text: String::new(),
            chart: None,
            flip_h: false,
            flip_v: false,
            rotation: 0.0,
            locked: true,
            alt_text: String::new(),
            parent: None,
            lock_aspect: false,
            image: None,
            props: ShapeProps::default(),
        });
        id
    }

    pub fn duplicate_shape(&mut self, sheet: SheetId, id: u32, base: &str) -> Option<u32> {
        let src = self.drawing(sheet)?.get(id)?.clone();
        let (x, y, w, h) = self.shape_rect_emu(sheet, &src.pos);
        let off = pt_to_emu(12.0);
        let pos = self.pos_for(sheet, src.pos.placement(), (x + off, y + off, w, h));
        let d = self.drawing_mut(sheet);
        let nid = d.next_id;
        d.next_id += 1;
        let mut copy = src.clone();
        copy.id = nid;
        copy.pos = pos;
        if src.name == format!("{base} {}", src.id - 1) {
            copy.name = format!("{base} {}", nid - 1);
        }

        if let Some(p) = src.parent {
            if let Some(at) = d.index_of(p) {
                let end = at + d.block(p).len();
                d.shapes.insert(end, copy);
                if let Some(ShapeKind::Group(kids)) = d.get_mut(p).map(|g| &mut g.kind) {
                    kids.push(nid);
                }
                self.refresh_groups(sheet);
                return Some(nid);
            }
        }
        d.shapes.push(copy);
        Some(nid)
    }

    pub fn set_shape_rect(&mut self, sheet: SheetId, id: u32, r: Emu4) {
        let Some(s) = self.drawing(sheet).and_then(|d| d.get(id)) else {
            return;
        };
        let pos = self.pos_for(sheet, s.pos.placement(), r);
        if let Some(s) = self.drawing_mut(sheet).get_mut(id) {
            s.pos = pos;
        }
    }

    pub fn set_shape_placement(&mut self, sheet: SheetId, id: u32, placement: i32) {
        let Some(s) = self.drawing(sheet).and_then(|d| d.get(id)) else {
            return;
        };
        let r = self.shape_rect_emu(sheet, &s.pos);
        let pos = self.pos_for(sheet, placement, r);
        if let Some(s) = self.drawing_mut(sheet).get_mut(id) {
            s.pos = pos;
        }
    }

    pub(crate) fn drawing_shift(&mut self, s: &Shift) {
        let rows = s.axis == Axis::Rows;
        let map = |a: &Anchor| -> Anchor {
            let mut a = *a;
            let i = if rows { a.row } else { a.col };
            let n = match s.map_index(i) {
                Some(n) => n,
                None => {
                    if rows {
                        a.row_off = 0;
                    } else {
                        a.col_off = 0;
                    }
                    s.at
                }
            };
            if rows {
                a.row = n;
            } else {
                a.col = n;
            }
            a
        };
        let in_block = |a: &Anchor| {
            let i = if rows { a.row } else { a.col };
            !s.insert && i >= s.at && i < s.at + s.count
        };

        let across = |a: &Anchor| if rows { a.col } else { a.row };
        let reached = |pos: &Pos| match (s.limit, pos) {
            (None, _) => true,
            (Some((p0, p1)), Pos::Both { from, to }) => {
                (p0..=p1).contains(&across(from)) && across(to) <= p1 + 1
            }
            (Some((p0, p1)), Pos::One { from, .. }) => (p0..=p1).contains(&across(from)),
            (Some(_), Pos::Abs { .. }) => false,
        };
        let sheets: Vec<SheetId> = self.drawings.keys().copied().collect();
        for sheet in sheets {
            let Some(d) = self.drawings.get_mut(&sheet) else {
                continue;
            };
            if sheet == s.sheet {
                d.shapes.retain(|sh| match sh.pos {
                    Pos::Both { from, to } => {
                        let flat = from == to;
                        !(reached(&sh.pos) && in_block(&from) && in_block(&to) && !flat)
                    }
                    _ => true,
                });
                for sh in &mut d.shapes {
                    if !reached(&sh.pos) {
                        continue;
                    }
                    match &mut sh.pos {
                        Pos::Both { from, to } => {
                            *from = map(from);
                            *to = map(to);
                        }
                        Pos::One { from, .. } => *from = map(from),
                        Pos::Abs { .. } => {}
                    }
                }
            }
            for sh in &mut d.shapes {
                if let Some(c) = &mut sh.chart {
                    for p in c.parts_mut() {
                        shift_part(p, s);
                    }
                }
            }
        }
    }

    pub(crate) fn drawing_drop_sheet(&mut self, id: SheetId) {
        self.drawings.remove(&id);
        for d in self.drawings.values_mut() {
            for sh in &mut d.shapes {
                if let Some(c) = &mut sh.chart {
                    for p in c.parts_mut() {
                        if let Part::Refs(v) = p {
                            v.retain(|(s, _)| *s != id);
                            if v.is_empty() {
                                *p = Part::Dead;
                            }
                        }
                    }
                }
            }
        }
    }

    pub(crate) fn drawing_copy_sheet(&mut self, src: SheetId, new: SheetId) {
        if let Some(mut d) = self.drawings.get(&src).cloned() {
            for sh in &mut d.shapes {
                if let Some(c) = &mut sh.chart {
                    for p in c.parts_mut() {
                        if let Part::Refs(v) = p {
                            for e in v.iter_mut().filter(|e| e.0 == src) {
                                e.0 = new;
                            }
                        }
                    }
                }
            }
            d.listing.clear();
            d.renumber_in_z_order();
            self.drawings.insert(new, d);
        }
    }
}

impl Workbook {
    pub(crate) fn drawing_cut(&mut self, src_sheet: SheetId, src: Rect, dst: CellAddr) {
        let dr = dst.row as i64 - src.r0 as i64;
        let dc = dst.col as i64 - src.c0 as i64;
        for d in self.drawings.values_mut() {
            for sh in &mut d.shapes {
                let Some(c) = &mut sh.chart else { continue };
                for p in c.parts_mut() {
                    let Part::Refs(v) = p else { continue };
                    for e in v.iter_mut() {
                        let r = e.1;
                        if e.0 == src_sheet
                            && r.r0 >= src.r0
                            && r.r1 <= src.r1
                            && r.c0 >= src.c0
                            && r.c1 <= src.c1
                        {
                            e.0 = dst.sheet;
                            e.1 = Rect::new(
                                (r.r0 as i64 + dr) as u32,
                                (r.c0 as i64 + dc) as u32,
                                (r.r1 as i64 + dr) as u32,
                                (r.c1 as i64 + dc) as u32,
                            );
                        }
                    }
                }
            }
        }
    }
}

fn shift_part(p: &mut Part, s: &Shift) {
    let Part::Refs(v) = p else {
        return;
    };
    let rows = s.axis == Axis::Rows;
    let mut out = Vec::new();
    for (sheet, r) in v.iter() {
        if *sheet != s.sheet {
            out.push((*sheet, *r));
            continue;
        }
        if let Some((a, b)) = s.limit {
            let (lo, hi) = if rows { (r.c0, r.c1) } else { (r.r0, r.r1) };
            if lo < a || hi > b {
                out.push((*sheet, *r));
                continue;
            }
        }
        let (lo, hi) = if rows { (r.r0, r.r1) } else { (r.c0, r.c1) };
        if let Some((na, nb)) = s.map_span(lo, hi) {
            let mut n = *r;
            if rows {
                n.r0 = na;
                n.r1 = nb;
            } else {
                n.c0 = na;
                n.c1 = nb;
            }
            out.push((*sheet, n));
        }
    }
    *p = if out.is_empty() {
        Part::Dead
    } else {
        Part::Refs(out)
    };
}

pub type Drawings = FxHashMap<SheetId, SheetDrawing>;

impl Workbook {
    pub fn set_drawing(&mut self, sheet: SheetId, mut d: SheetDrawing) {
        d.next_id = d
            .shapes
            .iter()
            .map(|s| s.id + 1)
            .max()
            .unwrap_or(2)
            .max(d.next_id)
            .max(2);
        self.drawings.insert(sheet, d);
    }
}
