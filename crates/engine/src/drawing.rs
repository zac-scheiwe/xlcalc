use std::borrow::Cow;

use rustc_hash::FxHashMap;

use crate::address::{CellAddr, Rect, SheetId};
use crate::host::Evaluated;
use crate::structure::{Axis, Shift};
use crate::value::Value;
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

    Name(NameLink),
}

#[derive(Clone, Debug, PartialEq)]
pub struct NameLink {
    pub scope: Option<SheetId>,

    pub name: String,
}

#[derive(Clone, Debug)]
pub enum NameRead {
    Refs(Vec<(SheetId, Rect)>),

    Value(Value),
    Undefined,
}

impl Workbook {
    pub fn read_name_link(&self, n: &NameLink) -> NameRead {
        if self.names().get(&n.name, n.scope).is_none() {
            return NameRead::Undefined;
        }
        let host = n.scope.or_else(|| {
            let shadowed = |s: SheetId| self.names().get(&n.name, Some(s)).is_some();
            self.sheets().map(|s| s.id).find(|&s| !shadowed(s))
        });
        let Some(host) = host else {
            return NameRead::Undefined;
        };
        match self.evaluate_text(&n.name, CellAddr::new(host, 0, 0)) {
            Evaluated::Ref(a) => NameRead::Refs(a.iter().map(|a| (a.sheet, a.rect)).collect()),
            Evaluated::Value(v) => NameRead::Value(v),
        }
    }

    pub fn resolve_part<'a>(&self, p: &'a Part) -> Cow<'a, Part> {
        let Part::Name(n) = p else {
            return Cow::Borrowed(p);
        };
        let item = |v: &Value| match v {
            Value::Number(x) => x.to_string(),
            Value::Text(t) => quoted_item(t),
            Value::Bool(b) => quoted_item(if *b { "TRUE" } else { "FALSE" }),
            _ => "0".to_string(),
        };
        Cow::Owned(match self.read_name_link(n) {
            NameRead::Refs(v) => Part::Refs(v),
            NameRead::Value(Value::Error(_)) | NameRead::Undefined => Part::Dead,
            NameRead::Value(Value::Array(a)) => {
                let items: Vec<String> = a.data.iter().map(item).collect();
                Part::Text(format!("{{{}}}", items.join(",")))
            }
            NameRead::Value(v) => Part::Text(format!("{{{}}}", item(&v))),
        })
    }

    pub(crate) fn drawing_respell_name(&mut self, key: &str, scope: Option<SheetId>, name: &str) {
        let key = crate::text::name_key(key);
        for d in self.drawings.values_mut() {
            for c in d.shapes.iter_mut().filter_map(|s| s.chart.as_mut()) {
                for p in c.parts_mut() {
                    if let Part::Name(n) = p {
                        if n.scope == scope && crate::text::name_key(&n.name) == key {
                            n.name = name.to_string();
                        }
                    }
                }
            }
        }
    }
}

pub fn literal_items(t: &str) -> Vec<(&str, &str)> {
    let inner = t.strip_prefix('{').unwrap_or(t);
    let inner = inner.strip_suffix('}').unwrap_or(inner);
    let mut out = Vec::new();
    let (mut start, mut quoted) = (0, false);
    for (i, c) in inner.char_indices() {
        match c {
            '"' => quoted = !quoted,
            ',' | ';' if !quoted => {
                out.push((inner[start..i].trim(), &inner[i..=i]));
                start = i + 1;
            }
            _ => {}
        }
    }
    out.push((inner[start..].trim(), ""));
    out
}

pub fn unquoted_item(item: &str) -> Option<String> {
    let inner = item.strip_prefix('"')?.strip_suffix('"')?;
    Some(inner.replace("\"\"", "\""))
}

pub fn quoted_item(s: &str) -> String {
    format!("\"{}\"", s.replace('"', "\"\""))
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
    pub values_ranged: bool,

    pub filtered: bool,
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

    pub label_texts: Vec<PointText>,

    pub label_cells: Vec<(u16, Option<CellAddr>)>,

    pub held_labels: Vec<HeldLabel>,

    pub name_data: bool,
    pub trends: Vec<Trendline>,
    pub error_bars: Option<ErrorBars>,

    pub bubbles: Option<Part>,

    pub plot_rank: u32,

    pub number: Option<u32>,
}

#[derive(Clone, Debug, PartialEq)]
pub struct PointText {
    pub point: u16,

    pub text: String,

    pub link: Option<Part>,

    pub auto: bool,
}

#[derive(Clone, Debug, PartialEq)]
pub struct HeldLabel {
    pub cell: Option<CellAddr>,

    pub own: bool,
    pub text: Option<PointText>,

    pub format: Vec<(String, BagVal)>,
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

#[derive(Clone, Debug, Default, PartialEq)]
pub enum HideTrack {
    #[default]
    Live,
    Armed,

    Frozen(std::collections::BTreeSet<(SheetId, bool, u32)>),
}

#[derive(Clone, Debug, PartialEq)]
pub enum BagVal {
    I(i32),
    D(f64),
    B(bool),
    S(String),
}

pub const SECONDARY_ZERO: &str = "Axis3.#zero";

pub const CAT_CROSS: &str = "Axis1.#cx";

pub type Bag = std::collections::BTreeMap<String, BagVal>;

pub const TITLE_DELETED: &str = "#title_off";

pub const TITLE_AUTO: &str = "#title_auto";

pub const TITLE_TYPED: &str = "#title_typed";

#[derive(Clone, Debug, PartialEq)]
pub struct Chart {
    pub chart_type: i32,

    pub plot_by: i32,

    pub pb_stale: bool,

    pub pb_pending: bool,

    pub hide_track: HideTrack,
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

    pub pivot: Option<String>,

    pub axes: [ChartAxis; 3],

    pub style: i32,

    pub bag: Bag,

    pub undo: ChartUndo,
}

#[derive(Clone, Debug, Default, PartialEq)]
pub struct ChartUndo {
    pub open: bool,
    pub snap: Option<Box<Chart>>,
}

#[derive(Clone, Debug, PartialEq)]
pub struct ChartAxis {
    pub has_title: bool,
    pub title: String,

    pub title_ref: Option<Part>,
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

    pub log_base: f64,
}

impl ChartAxis {
    pub fn new(grid_major: bool) -> ChartAxis {
        ChartAxis {
            has_title: false,
            title: String::new(),
            title_ref: None,
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
            log_base: 10.0,
        }
    }
}

impl Series {
    pub fn number(&self, k: usize) -> usize {
        self.fmt.number.map_or(k + 1, |n| n as usize)
    }
}

impl Chart {
    pub fn auto_title_series(&self) -> Option<&Series> {
        if self.bag.contains_key(TITLE_DELETED)
            || self.bag.get("Chart.SeriesNameLevel") == Some(&BagVal::I(-3))
        {
            return None;
        }
        self.lone_series().filter(|s| s.name != Part::None)
    }

    pub fn lone_series(&self) -> Option<&Series> {
        let mut pies: Vec<i32> = Vec::new();
        let mut count = 0;
        let mut first = None;
        for s in self.series.iter().filter(|s| !s.fmt.filtered) {
            if matches!(s.chart_type, 5 | 68..=71 | -4102) {
                if pies.contains(&s.axis_group) {
                    continue;
                }
                pies.push(s.axis_group);
            }
            count += 1;
            first = first.or(Some(s));
        }
        first.filter(|_| count == 1)
    }

    pub fn free_series_number(&self) -> u32 {
        let used: std::collections::BTreeSet<usize> = self
            .series
            .iter()
            .enumerate()
            .map(|(k, s)| s.number(k))
            .collect();
        (1..).find(|n| !used.contains(&(*n as usize))).unwrap_or(1)
    }

    pub fn parts_mut(&mut self) -> Vec<&mut Part> {
        let (mut v, texts) = self.parts_split_mut();
        v.extend(texts);
        v
    }

    pub fn parts_split_mut(&mut self) -> (Vec<&mut Part>, Vec<&mut Part>) {
        let mut v: Vec<&mut Part> = Vec::new();
        let mut texts: Vec<&mut Part> = Vec::new();
        for s in &mut self.series {
            v.push(&mut s.name);
            v.push(&mut s.x);
            v.push(&mut s.values);
            if let Some(b) = &mut s.fmt.bubbles {
                v.push(b);
            }
            texts.extend(s.fmt.label_texts.iter_mut().filter_map(|t| t.link.as_mut()));
            let held = s.fmt.held_labels.iter_mut().filter_map(|h| h.text.as_mut());
            texts.extend(held.filter_map(|t| t.link.as_mut()));
        }
        texts.extend(self.title_ref.as_mut());
        texts.extend(self.axes.iter_mut().filter_map(|a| a.title_ref.as_mut()));
        (v, texts)
    }

    fn label_cells_mut(&mut self) -> impl Iterator<Item = &mut Option<CellAddr>> {
        self.series.iter_mut().flat_map(|s| {
            let held = s.fmt.held_labels.iter_mut().map(|h| &mut h.cell);
            s.fmt.label_cells.iter_mut().map(|c| &mut c.1).chain(held)
        })
    }

    pub fn carry_label_cells(&mut self, from: Option<(SheetId, SheetId)>) {
        let kept = |c: &Option<CellAddr>| from.is_some_and(|f| c.is_some_and(|c| c.sheet == f.0));
        for s in &mut self.series {
            s.fmt.label_cells.retain(|c| kept(&c.1));
            s.fmt.held_labels.retain(|h| kept(&h.cell));
        }
        if let Some((_, to)) = from {
            for c in self.label_cells_mut().flatten() {
                c.sheet = to;
            }
        }
    }

    pub fn new() -> Chart {
        Chart {
            chart_type: 51,
            plot_by: 0,
            pb_stale: false,
            pb_pending: false,
            hide_track: HideTrack::Live,
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
            pivot: None,
            axes: [
                ChartAxis::new(false),
                ChartAxis::new(true),
                ChartAxis::new(true),
            ],
            style: 0,
            bag: Bag::new(),
            undo: ChartUndo::default(),
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

    pub tf: crate::drawing_text::TextFrame,

    pub hlink: Option<ShapeLink>,

    pub bag: Bag,
}

#[derive(Clone, Debug, Eq)]
pub struct ShapeLink {
    pub address: String,
    pub sub_address: String,
    pub screen_tip: Option<String>,

    pub rid: String,
}

impl PartialEq for ShapeLink {
    fn eq(&self, o: &ShapeLink) -> bool {
        self.address == o.address
            && self.sub_address == o.sub_address
            && self.screen_tip == o.screen_tip
    }
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
            tf: crate::drawing_text::TextFrame::default(),
            hlink: None,
            bag: Bag::new(),
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

        let (x, y) = (
            x.clamp(0, self.col_left_emu(sheet, crate::address::MAX_COLS) - 9525),
            y.clamp(0, self.row_top_emu(sheet, crate::address::MAX_ROWS) - 9525),
        );
        match placement {
            1 => {
                let from = self.xy_anchor(sheet, x, y);
                let to = self.xy_anchor(sheet, x + w, y + h);
                let (fx, fy) = self.anchor_xy(sheet, &from);
                let (tx, ty) = self.anchor_xy(sheet, &to);
                if tx - fx == w && ty - fy == h {
                    Pos::Both { from, to }
                } else {
                    Pos::One { from, w, h }
                }
            }
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

    pub fn cell_corner_emu(&self, sheet: SheetId, row: u32, col: u32) -> (i64, i64) {
        (self.col_left_emu(sheet, col), self.row_top_emu(sheet, row))
    }

    pub fn paste_shape(
        &mut self,
        sheet: SheetId,
        src: &Shape,
        base: &str,
        at: (i64, i64, i64, i64),
    ) -> u32 {
        let (x, y, w, h) = at;
        let pos = self.pos_for(sheet, src.pos.placement(), (x, y, w, h));
        let d = self.drawing_mut(sheet);
        let nid = d.next_id;
        d.next_id += 1;
        let mut copy = src.clone();
        copy.id = nid;
        copy.pos = pos;
        copy.parent = None;
        if src.name == format!("{base} {}", src.id - 1) {
            copy.name = format!("{base} {}", nid - 1);
        }
        d.shapes.push(copy);
        nid
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

    pub(crate) fn drawing_shift(&mut self, s: &Shift) -> Vec<(SheetId, u32, bool)> {
        self.drawing_hide_live();
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

        let past_hidden = s.limit.is_some_and(|(_, p1)| {
            if rows {
                self.column_points(s.sheet, p1 + 1) == 0.0
            } else {
                self.row_height(s.sheet, p1 + 1) == 0.0
            }
        });
        let across = |a: &Anchor| if rows { a.col } else { a.row };
        let reached = |pos: &Pos| match (s.limit, pos) {
            (None, _) => true,
            (Some((p0, _)), Pos::Both { from, .. } | Pos::One { from, .. }) if past_hidden => {
                across(from) >= p0
            }
            (Some((p0, p1)), Pos::Both { from, to }) => {
                (p0..=p1).contains(&across(from)) && across(to) <= p1 + 1
            }
            (Some((p0, p1)), Pos::One { from, .. }) => (p0..=p1).contains(&across(from)),
            (Some(_), Pos::Abs { .. }) => false,
        };

        let tabs: Vec<Rect> = self
            .tables
            .iter()
            .filter(|t| t.sheet == s.sheet)
            .map(|t| t.rect)
            .collect();
        let in_table = |r: &Rect| {
            tabs.iter()
                .any(|t| t.r0 <= r.r0 && r.r1 <= t.r1 && t.c0 <= r.c0 && r.c1 <= t.c1)
        };

        self.control_anchors.retain(|&(sheet, _), pos| {
            if sheet != s.sheet || !reached(pos) {
                return true;
            }
            match pos {
                Pos::Both { from, to } => {
                    if in_block(from) && in_block(to) && from != to {
                        return false;
                    }
                    *from = map(from);
                    *to = map(to);
                }
                Pos::One { from, .. } => *from = map(from),
                Pos::Abs { .. } => {}
            }
            true
        });
        let mut resplit = Vec::new();
        let shifted = self
            .sheet(s.sheet)
            .map(|x| crate::parser::print::sheet_name_text(&x.name));
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
                    let dead = |c: &Chart| {
                        c.series
                            .iter()
                            .filter(|x| matches!(x.values, Part::Dead))
                            .count()
                    };
                    if std::mem::take(&mut c.pb_pending) {
                        c.plot_by = 0;
                        c.source = None;
                    }
                    let dead0 = dead(c);
                    let linked = c.plot_by != 0
                        && c.pivot.is_none()
                        && c.source.is_some_and(|(ss, r)| {
                            let lo = if s.axis == Axis::Rows { r.r0 } else { r.c0 };
                            let gone =
                                !s.insert && s.limit.is_none() && s.at <= lo && lo < s.at + s.count;
                            ss == s.sheet && in_table(&r) && !gone
                        });
                    let mut trimmed = false;
                    let named: Vec<bool> = c
                        .series
                        .iter()
                        .map(|x| matches!(x.name, Part::Refs(_)))
                        .collect();
                    for cell in c.label_cells_mut() {
                        let Some(at) = *cell else { continue };
                        let one = Rect::new(at.row, at.col, at.row, at.col);
                        let mut p = Part::Refs(vec![(at.sheet, one)]);
                        shift_part(&mut p, s);
                        *cell = match p {
                            Part::Refs(v) => v.first().map(|&(x, r)| CellAddr::new(x, r.r0, r.c0)),
                            _ => None,
                        };
                    }
                    let (parts, texts) = c.parts_split_mut();
                    for p in parts {
                        trimmed |= shift_part(p, s);
                    }

                    for p in texts {
                        if shift_part(p, s) && *p == Part::Dead {
                            if let Some(name) = &shifted {
                                *p = Part::External(format!("{name}!#REF!"));
                            }
                        }
                    }

                    if s.limit.is_some() {
                        for (x, was) in c.series.iter_mut().zip(named) {
                            if was && x.name == Part::Dead && x.values != Part::Dead {
                                x.name = Part::Text("#REF!".into());
                            }
                        }
                    }

                    let only_parts = s.limit.is_none() && trimmed && dead(c) == dead0;
                    if s.limit.is_none() {
                        trimmed = false;
                    }
                    trimmed |= dead(c) > dead0;
                    if only_parts && !linked {
                        c.pb_pending = true;
                    }

                    if trimmed && !linked {
                        c.pb_stale = true;
                        c.source = None;

                        let combo = c
                            .series
                            .windows(2)
                            .any(|w| w[0].chart_type != w[1].chart_type);
                        if s.limit.is_none() && combo && dead(c) > dead0 {
                            c.plot_by = 0;
                        }
                    }

                    if let (true, Some((ss, r)), None) = (linked, c.source, &c.pivot) {
                        {
                            let shrinks = s.limit.is_some() && !s.insert;
                            let n = if s.limit.is_some() && !shrinks {
                                s.adjust_rect(&r)
                            } else {
                                s.adjust_rect_whole(&r)
                            };
                            if let Some(n) = n {
                                c.source = Some((ss, n));
                                resplit.push((sheet, sh.id, s.limit.is_some() && !shrinks));
                            }
                        }
                    }
                }
            }
        }
        resplit
    }

    pub(crate) fn drawing_resplit(&mut self, list: &[(SheetId, u32, bool)]) {
        for &(sheet, id, check) in list {
            let Some(c) = self.drawing(sheet).and_then(|d| d.get(id)?.chart.as_ref()) else {
                continue;
            };
            let Some((ss, r)) = c.source else { continue };
            let (by, mut series) = crate::chart_src::series_from_block(
                self,
                ss,
                r,
                Some(c.plot_by),
                c.chart_type,
                &[],
            );

            if check
                && (series.len() != c.series.len()
                    || series
                        .iter()
                        .zip(&c.series)
                        .any(|(n, o)| n.name != o.name || n.x != o.x || n.values != o.values))
            {
                if let Some(c) = self
                    .drawing_mut(sheet)
                    .get_mut(id)
                    .and_then(|s| s.chart.as_mut())
                {
                    c.plot_by = 0;
                    c.source = None;
                }
                continue;
            }

            for n in &mut series {
                if let Some(o) = c.series.iter().find(|o| o.values == n.values) {
                    n.fmt = o.fmt.clone();
                    n.chart_type = o.chart_type;
                    n.axis_group = o.axis_group;
                }
            }
            if let Some(c) = self
                .drawing_mut(sheet)
                .get_mut(id)
                .and_then(|s| s.chart.as_mut())
            {
                c.series = series;
                c.plot_by = by;
            }
        }
    }

    pub(crate) fn drawing_drop_sheet(&mut self, id: SheetId) {
        self.drawings.remove(&id);
        for d in self.drawings.values_mut() {
            for sh in &mut d.shapes {
                if let Some(c) = &mut sh.chart {
                    for cell in c.label_cells_mut() {
                        if cell.is_some_and(|at| at.sheet == id) {
                            *cell = None;
                        }
                    }
                    for p in c.parts_mut() {
                        match p {
                            Part::Refs(v) => {
                                v.retain(|(s, _)| *s != id);
                                if v.is_empty() {
                                    *p = Part::Dead;
                                }
                            }
                            Part::Name(n) if n.scope == Some(id) => *p = Part::Dead,
                            _ => {}
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
                    for at in c.label_cells_mut().flatten() {
                        if at.sheet == src {
                            at.sheet = new;
                        }
                    }
                    for p in c.parts_mut() {
                        match p {
                            Part::Refs(v) => {
                                for e in v.iter_mut().filter(|e| e.0 == src) {
                                    e.0 = new;
                                }
                            }

                            Part::Name(n) if n.scope == Some(src) => n.scope = Some(new),
                            _ => {}
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
    pub(crate) fn drawing_sorted(&mut self, sheet: SheetId, rect: Rect) {
        for d in self.drawings.values_mut() {
            for sh in &mut d.shapes {
                let Some(c) = &mut sh.chart else { continue };
                let hit = c.series.iter().any(|x| {
                    [&x.name, &x.x, &x.values].into_iter().any(|p| {
                        matches!(p, Part::Refs(v) if v.iter().any(|(s, r)| *s == sheet && r.intersects(&rect)))
                    })
                });
                if hit {
                    c.hide_track = HideTrack::Armed;
                }
            }
        }
    }

    pub(crate) fn drawing_hide_live(&mut self) {
        for d in self.drawings.values_mut() {
            for sh in &mut d.shapes {
                if let Some(c) = &mut sh.chart {
                    c.hide_track = HideTrack::Live;
                }
            }
        }
    }

    pub(crate) fn drawing_cut(&mut self, src_sheet: SheetId, src: Rect, dst: CellAddr) {
        let dr = dst.row as i64 - src.r0 as i64;
        let dc = dst.col as i64 - src.c0 as i64;
        for d in self.drawings.values_mut() {
            for sh in &mut d.shapes {
                let Some(c) = &mut sh.chart else { continue };
                if std::mem::take(&mut c.pb_pending) {
                    c.plot_by = 0;
                    c.source = None;
                }
                let mut moved = false;

                let holds = |r: Rect, at: CellAddr| {
                    (r.r0..=r.r1).contains(&at.row) && (r.c0..=r.c1).contains(&at.col)
                };
                let onto = |v: u32, d: i64| (i64::from(v) + d) as u32;
                let pasted = Rect::new(
                    onto(src.r0, dr),
                    onto(src.c0, dc),
                    onto(src.r1, dr),
                    onto(src.c1, dc),
                );
                for cell in c.label_cells_mut() {
                    let Some(at) = *cell else { continue };
                    if at.sheet == src_sheet && holds(src, at) {
                        let to = CellAddr::new(dst.sheet, onto(at.row, dr), onto(at.col, dc));
                        *cell = Some(to);
                    } else if at.sheet == dst.sheet && holds(pasted, at) {
                        *cell = None;
                    }
                }
                let before: Vec<Part> = c.series.iter().map(|s| s.values.clone()).collect();

                let dest = Rect::new(
                    (src.r0 as i64 + dr) as u32,
                    (src.c0 as i64 + dc) as u32,
                    (src.r1 as i64 + dr) as u32,
                    (src.c1 as i64 + dc) as u32,
                );
                for p in c.parts_mut() {
                    let Part::Refs(v) = p else { continue };
                    for e in v.iter_mut() {
                        let r = e.1;
                        let inside = e.0 == src_sheet
                            && r.r0 >= src.r0
                            && r.r1 <= src.r1
                            && r.c0 >= src.c0
                            && r.c1 <= src.c1;
                        if !inside && e.0 == dst.sheet {
                            let hit = dest.r0 <= r.r1
                                && r.r0 <= dest.r1
                                && dest.c0 <= r.c1
                                && r.c0 <= dest.c1;
                            let rows_all = dest.r0 <= r.r0 && r.r1 <= dest.r1;
                            let cols_all = dest.c0 <= r.c0 && r.c1 <= dest.c1;
                            if hit && rows_all && cols_all {
                                e.0 = SheetId(u32::MAX);
                            } else if hit && rows_all && !cols_all {
                                if dest.c0 <= r.c0 {
                                    e.1.c0 = dest.c1 + 1;
                                } else if dest.c1 >= r.c1 {
                                    e.1.c1 = dest.c0 - 1;
                                }
                            } else if hit && cols_all && !rows_all {
                                if dest.r0 <= r.r0 {
                                    e.1.r0 = dest.r1 + 1;
                                } else if dest.r1 >= r.r1 {
                                    e.1.r1 = dest.r0 - 1;
                                }
                            }
                        }
                        if e.0 == src_sheet
                            && r.r0 >= src.r0
                            && r.r1 <= src.r1
                            && r.c0 >= src.c0
                            && r.c1 <= src.c1
                        {
                            moved = true;
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
                for p in c.parts_mut() {
                    if let Part::Refs(v) = p {
                        v.retain(|e| e.0 != SheetId(u32::MAX));
                        if v.is_empty() {
                            *p = Part::Dead;
                        }
                    }
                }
                for (s, b) in c.series.iter_mut().zip(&before) {
                    if let (Part::Refs(n), Part::Refs(o)) = (&s.values, b) {
                        let cells = |v: &Vec<(SheetId, Rect)>| {
                            v.iter()
                                .map(|(_, r)| (r.r1 - r.r0 + 1) * (r.c1 - r.c0 + 1))
                                .sum::<u32>()
                        };
                        if cells(n) == 1 && cells(o) > 1 {
                            s.fmt.values_ranged = true;
                        }
                    }
                }
                if moved {
                    c.plot_by = 0;
                }
            }
        }
    }
}

fn shift_part(p: &mut Part, s: &Shift) -> bool {
    let Part::Refs(v) = p else {
        return false;
    };
    let mut trimmed = false;
    let rows = s.axis == Axis::Rows;
    let mut out = Vec::new();
    for (sheet, r) in v.iter() {
        if *sheet != s.sheet {
            out.push((*sheet, *r));
            continue;
        }
        if s.limit.is_some() {
            if let Some(n) = s.adjust_rect(r) {
                trimmed |= s.trims(r);
                out.push((*sheet, n));
            }
            continue;
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
    let lost = out.is_empty();
    *p = if lost { Part::Dead } else { Part::Refs(out) };
    trimmed || lost
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

#[cfg(test)]
mod tests {
    use super::*;

    fn chart(series: &[(i32, bool)]) -> Chart {
        let mut c = Chart::new();
        for &(chart_type, named) in series {
            c.series.push(Series {
                name: if named {
                    Part::Text("n".into())
                } else {
                    Part::None
                },
                x: Part::None,
                values: Part::None,
                chart_type,
                axis_group: 1,
                fmt: SeriesFmt::default(),
            });
        }
        c
    }

    #[test]
    fn array_literal_items() {
        let items: Vec<&str> = literal_items("{1,\"a,b\",\"s\"\"t\"}")
            .into_iter()
            .map(|i| i.0)
            .collect();
        assert_eq!(items, ["1", "\"a,b\"", "\"s\"\"t\""]);
        assert_eq!(unquoted_item(items[2]).as_deref(), Some("s\"t"));
        assert_eq!(quoted_item("s\"t"), items[2]);
        assert_eq!(unquoted_item("1"), None);
    }

    #[test]
    fn automatic_title_series() {
        let has = |c: &Chart| c.auto_title_series().is_some();
        assert!(!has(&chart(&[])));
        assert!(has(&chart(&[(51, true)])));
        assert!(!has(&chart(&[(51, false)])));
        assert!(!has(&chart(&[(51, true), (51, true)])));

        assert!(has(&chart(&[(5, true), (5, true)])));
        assert!(!has(&chart(&[(5, false), (5, true)])));
        assert!(!has(&chart(&[(-4120, true), (-4120, true)])));
        let mut filtered = chart(&[(51, true), (51, true)]);
        filtered.series[0].fmt.filtered = true;
        assert!(has(&filtered));
        let mut deleted = chart(&[(51, true)]);
        deleted.bag.insert(TITLE_DELETED.into(), BagVal::I(1));
        assert!(!has(&deleted));
        let mut none = chart(&[(51, true)]);
        none.bag
            .insert("Chart.SeriesNameLevel".into(), BagVal::I(-3));
        assert!(!has(&none));
    }
}
