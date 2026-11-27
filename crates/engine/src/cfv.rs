use std::sync::Arc;

use crate::address::{CellAddr, Rect, SheetId};
pub use crate::cfblock::{CfBlock, Placed};
use crate::cfv_edit::bbox_tl;
use crate::dfrtree::DfRtree;
use crate::formula::{CRef, Node};
use crate::parser::ast::BinOp;
use crate::structure::Shift;
use crate::style::Style;
use crate::value::Value;
use crate::workbook::Workbook;
use crate::x87;

#[derive(Clone, Copy, Debug, Default, PartialEq, Eq)]
pub struct DxfMask {
    pub number_format: bool,
    pub bold: bool,
    pub italic: bool,
    pub underline: bool,
    pub strike: bool,
    pub font_color: bool,
    pub pattern: bool,

    pub pattern_read: bool,
    pub fill_color: bool,
    pub pattern_color: bool,

    pub theme_font: bool,

    pub fill_none: bool,

    pub pattern_color_none: bool,
    pub left: bool,
    pub right: bool,
    pub top: bool,
    pub bottom: bool,
}

#[derive(Clone, Copy, Debug, Default)]
pub struct DxfMade {
    pub stamp: u64,

    pub slot: Option<usize>,

    pub source: Option<usize>,
}

impl PartialEq for DxfMade {
    fn eq(&self, _: &DxfMade) -> bool {
        true
    }
}

impl Eq for DxfMade {}

#[derive(Clone, Debug, Default, PartialEq, Eq)]
pub struct Dxf {
    pub style: Style,
    pub mask: DxfMask,
    pub made: DxfMade,
}

impl Dxf {
    pub fn apply_over(&self, base: &mut Style, taken: &mut DxfMask) {
        let (m, s) = (&self.mask, &self.style);
        if m.number_format && !taken.number_format {
            base.number_format = s.number_format.clone();
            taken.number_format = true;
        }
        if m.bold && !taken.bold {
            base.font.bold = s.font.bold;
            taken.bold = true;
        }
        if m.italic && !taken.italic {
            base.font.italic = s.font.italic;
            taken.italic = true;
        }
        if m.underline && !taken.underline {
            base.font.underline = s.font.underline;
            taken.underline = true;
        }
        if m.strike && !taken.strike {
            base.font.strikethrough = s.font.strikethrough;
            taken.strike = true;
        }
        if m.font_color && !taken.font_color {
            base.font.color = s.font.color;
            taken.font_color = true;
        }
        if m.pattern && !taken.pattern {
            base.fill.pattern = s.fill.pattern;
            base.fill.cleared = false;
            taken.pattern = true;
        }
        if m.fill_color && !taken.fill_color {
            base.fill.color = s.fill.color;
            taken.fill_color = true;
        }
        if m.pattern_color && !taken.pattern_color {
            base.fill.pattern_color = s.fill.pattern_color;
            taken.pattern_color = true;
        }
        if m.left && !taken.left {
            base.borders.left = s.borders.left;
            taken.left = true;
        }
        if m.right && !taken.right {
            base.borders.right = s.borders.right;
            taken.right = true;
        }
        if m.top && !taken.top {
            base.borders.top = s.borders.top;
            taken.top = true;
        }
        if m.bottom && !taken.bottom {
            base.borders.bottom = s.borders.bottom;
            taken.bottom = true;
        }
    }
}

#[derive(Clone, Debug, PartialEq)]
pub struct ScaleCrit {
    pub ty: i32,
    pub value: f64,

    pub formula: Option<String>,

    pub color: u32,
}

#[derive(Clone, Debug, PartialEq)]
pub struct CondValue {
    pub ty: i32,
    pub value: f64,
    pub formula: Option<String>,
}

#[derive(Clone, Copy, Debug, PartialEq)]
pub struct BarColor {
    pub rgb: u32,
    pub tint: f64,
}

impl BarColor {
    pub const fn new(rgb: u32) -> BarColor {
        BarColor { rgb, tint: 0.0 }
    }

    pub fn shown(&self) -> u32 {
        crate::style::apply_tint_cv(self.rgb, self.tint)
    }
}

#[derive(Clone, Debug, PartialEq)]
pub struct DataBarSpec {
    pub min: CondValue,
    pub max: CondValue,
    pub show_value: bool,

    pub fill_type: i32,

    pub direction: i32,

    pub axis_position: i32,
    pub bar: BarColor,
    pub axis: BarColor,

    pub border_type: i32,
    pub border: BarColor,

    pub neg_color_type: i32,
    pub neg: BarColor,
    pub neg_border_type: i32,
    pub neg_border: BarColor,

    pub percent_min: i32,
    pub percent_max: i32,
}

impl Default for DataBarSpec {
    fn default() -> Self {
        DataBarSpec {
            min: CondValue {
                ty: 1,
                value: 0.0,
                formula: None,
            },
            max: CondValue {
                ty: 2,
                value: 0.0,
                formula: None,
            },
            show_value: true,
            fill_type: 1,
            direction: -5002,
            axis_position: 2,
            bar: BarColor::new(0x638EC6),
            axis: BarColor::new(0),
            border_type: 0,
            border: BarColor::new(0),
            neg_color_type: 1,
            neg: BarColor::new(0x638EC6),
            neg_border_type: 1,
            neg_border: BarColor::new(0),
            percent_min: 0,
            percent_max: 100,
        }
    }
}

#[derive(Clone, Debug, PartialEq)]
pub struct IconCrit {
    pub ty: i32,
    pub value: f64,

    pub op: i32,

    pub icon: i32,
}

#[derive(Clone, Debug, PartialEq)]
pub struct IconSetSpec {
    pub id: i32,
    pub reverse: bool,
    pub icon_only: bool,
    pub percentile: bool,

    pub custom: bool,
    pub crits: Vec<IconCrit>,

    pub formulas: Vec<Option<String>>,
}

const SET_ICONS: [&[i32]; 20] = [
    &[3, 2, 1],
    &[6, 5, 4],
    &[9, 8, 7],
    &[12, 11, 10],
    &[16, 15, 14],
    &[18, 17, 10],
    &[21, 20, 19],
    &[24, 23, 22],
    &[3, 26, 25, 1],
    &[6, 28, 27, 4],
    &[32, 31, 30, 29],
    &[38, 39, 40, 41],
    &[13, 12, 11, 10],
    &[3, 26, 2, 25, 1],
    &[6, 28, 5, 27, 4],
    &[37, 38, 39, 40, 41],
    &[36, 35, 34, 33, 32],
    &[44, 43, 42],
    &[47, 46, 45],
    &[52, 51, 50, 49, 48],
];

impl IconSetSpec {
    pub fn new(id: i32, reverse: bool) -> Option<IconSetSpec> {
        let icons = usize::try_from(id - 1)
            .ok()
            .and_then(|i| SET_ICONS.get(i))?;
        let n = icons.len();
        let crits = (0..n)
            .map(|k| IconCrit {
                ty: 3,
                value: Self::default_value(n, k),
                op: 7,
                icon: if reverse { icons[n - 1 - k] } else { icons[k] },
            })
            .collect();
        Some(IconSetSpec {
            id,
            reverse,
            icon_only: false,
            percentile: false,
            custom: false,
            crits,
            formulas: vec![None; n],
        })
    }

    pub fn default_value(n: usize, k: usize) -> f64 {
        if n == 3 {
            [0.0, 33.0, 67.0][k.min(2)]
        } else {
            (100.0 / n as f64 * k as f64).round()
        }
    }
}

#[derive(Clone, Debug, PartialEq)]
pub enum CfKind {
    CellValue(i32),

    Expression,

    Top10 {
        bottom: bool,
        percent: bool,
        rank: u32,
    },

    Unique {
        duplicate: bool,
    },

    Text {
        op: i32,
        text: String,
    },
    Blanks,
    NoBlanks,
    Errors,
    NoErrors,

    DataBar(Box<DataBarSpec>),

    IconSet(IconSetSpec),

    TimePeriod(i32),

    ColorScale(Vec<ScaleCrit>),
    AboveAverage {
        below: bool,
        equal: bool,

        std: bool,
        num_std: u32,
    },
}

impl CfKind {
    pub fn type_code(&self) -> i32 {
        match self {
            CfKind::CellValue(_) => 1,
            CfKind::Expression => 2,
            CfKind::ColorScale(_) => 3,
            CfKind::DataBar(_) => 4,
            CfKind::IconSet(_) => 6,
            CfKind::Top10 { .. } => 5,
            CfKind::Unique { .. } => 8,
            CfKind::Text { .. } => 9,
            CfKind::Blanks => 10,
            CfKind::TimePeriod(_) => 11,
            CfKind::AboveAverage { .. } => 12,
            CfKind::NoBlanks => 13,
            CfKind::Errors => 16,
            CfKind::NoErrors => 17,
        }
    }
}

#[derive(Clone, Debug, PartialEq)]
pub struct CfRule {
    pub id: u32,
    pub kind: CfKind,
    pub stop: bool,

    pub prio: i32,

    pub f1: Option<String>,
    pub f2: Option<String>,
    pub dxf: Dxf,
}

pub fn load_order(rules: &mut [Placed]) {
    for i in (0..rules.len()).rev() {
        if !optimizable(&rules[i].rule.kind) {
            continue;
        }
        let mut rest = join_areas(rules[i].areas.clone());
        rest.sort_by_key(|r| (r.r0, r.r1, r.c0, r.c1));
        let mut out = Vec::with_capacity(rest.len());
        for j in (0..i).rev() {
            if !load_conflict(&rules[i].rule, &rules[j].rule) {
                continue;
            }
            let g = &rules[j].areas;
            let (hit, keep): (Vec<Rect>, Vec<Rect>) = rest
                .into_iter()
                .partition(|r| g.iter().any(|x| x.intersects(r)));
            out.extend(hit);
            rest = keep;
        }
        out.extend(rest);
        rules[i].areas = out;
    }
}

fn optimizable(k: &CfKind) -> bool {
    matches!(
        k,
        CfKind::CellValue(_)
            | CfKind::Expression
            | CfKind::Text { .. }
            | CfKind::Blanks
            | CfKind::NoBlanks
            | CfKind::Errors
            | CfKind::NoErrors
            | CfKind::TimePeriod(_)
    )
}

fn load_conflict(f: &CfRule, g: &CfRule) -> bool {
    if f.kind == g.kind && f.f1 == g.f1 && f.f2 == g.f2 && f.stop == g.stop && f.dxf == g.dxf {
        return false;
    }
    if f.stop || g.stop {
        return true;
    }
    let formats = match (&f.kind, &g.kind) {
        (CfKind::CellValue(a), CfKind::CellValue(b)) => values_meet(
            *a,
            f.f1.as_deref(),
            f.f2.as_deref(),
            *b,
            g.f1.as_deref(),
            g.f2.as_deref(),
        ),
        _ => true,
    };
    if formats && masks_meet(&f.dxf.mask, &g.dxf.mask) && f.dxf != g.dxf {
        return true;
    }
    let m = &f.dxf.mask;
    matches!(g.kind, CfKind::ColorScale(_)) && (m.fill_color || m.pattern || m.pattern_color)
}

fn masks_meet(a: &DxfMask, b: &DxfMask) -> bool {
    (a.number_format && b.number_format)
        || (a.bold && b.bold)
        || (a.italic && b.italic)
        || (a.underline && b.underline)
        || (a.strike && b.strike)
        || (a.font_color && b.font_color)
        || (a.pattern && b.pattern)
        || (a.fill_color && b.fill_color)
        || (a.pattern_color && b.pattern_color)
        || (a.left && b.left)
        || (a.right && b.right)
        || (a.top && b.top)
        || (a.bottom && b.bottom)
}

fn values_meet(
    a: i32,
    a1: Option<&str>,
    a2: Option<&str>,
    b: i32,
    b1: Option<&str>,
    b2: Option<&str>,
) -> bool {
    let (Some(x), Some(y)) = (value_set(a, a1, a2), value_set(b, b1, b2)) else {
        return true;
    };
    x.iter().any(|p| y.iter().any(|q| p.meets(q)))
}

#[derive(Clone, Copy)]
struct Run {
    lo: f64,
    lo_in: bool,
    hi: f64,
    hi_in: bool,
}

impl Run {
    fn meets(&self, o: &Run) -> bool {
        let (lo, lo_in) = if self.lo > o.lo || (self.lo == o.lo && !self.lo_in) {
            (self.lo, self.lo_in)
        } else {
            (o.lo, o.lo_in)
        };
        let (hi, hi_in) = if self.hi < o.hi || (self.hi == o.hi && !self.hi_in) {
            (self.hi, self.hi_in)
        } else {
            (o.hi, o.hi_in)
        };
        lo < hi || (lo == hi && lo_in && hi_in)
    }
}

fn value_set(op: i32, f1: Option<&str>, f2: Option<&str>) -> Option<Vec<Run>> {
    let num = |f: Option<&str>| f?.trim_start_matches('=').trim().parse::<f64>().ok();
    let a = num(f1)?;
    let inf = f64::INFINITY;
    let run = |lo, lo_in, hi, hi_in| Run {
        lo,
        lo_in,
        hi,
        hi_in,
    };
    Some(match op {
        1 | 2 => {
            let b = num(f2)?;
            let (lo, hi) = (a.min(b), a.max(b));
            if op == 1 {
                vec![run(lo, true, hi, true)]
            } else {
                vec![run(-inf, false, lo, false), run(hi, false, inf, false)]
            }
        }
        3 => vec![run(a, true, a, true)],
        4 => vec![run(-inf, false, a, false), run(a, false, inf, false)],
        5 => vec![run(a, false, inf, false)],
        6 => vec![run(-inf, false, a, false)],
        7 => vec![run(a, true, inf, false)],
        8 => vec![run(-inf, false, a, true)],
        _ => return None,
    })
}

fn join_areas(mut v: Vec<Rect>) -> Vec<Rect> {
    loop {
        let mut joined = false;
        for vertical in [true, false] {
            'pass: loop {
                for i in 0..v.len() {
                    for j in 0..v.len() {
                        let (a, b) = (v[i], v[j]);
                        let ok = i != j
                            && if vertical {
                                a.c0 == b.c0 && a.c1 == b.c1 && b.r0 <= a.r1 + 1 && a.r0 <= b.r0
                            } else {
                                a.r0 == b.r0 && a.r1 == b.r1 && b.c0 <= a.c1 + 1 && a.c0 <= b.c0
                            };
                        if ok {
                            v[i] = Rect {
                                r0: a.r0,
                                c0: a.c0,
                                r1: a.r1.max(b.r1),
                                c1: a.c1.max(b.c1),
                            };
                            v.remove(j);
                            joined = true;
                            continue 'pass;
                        }
                    }
                }
                break;
            }
        }
        if !joined {
            return v;
        }
    }
}

#[derive(Clone, Debug, PartialEq)]
pub struct Dv {
    pub applies: Vec<Rect>,

    pub kind: i32,
    pub alert: i32,
    pub op: i32,

    pub f1: Option<String>,
    pub f2: Option<String>,
    pub ignore_blank: bool,
    pub dropdown: bool,
    pub show_input: bool,
    pub show_error: bool,
    pub input_title: String,
    pub input_message: String,
    pub error_title: String,
    pub error_message: String,
}

impl Dv {
    pub fn anchor(&self) -> (u32, u32) {
        crate::cfv_edit::bbox_tl(&self.applies)
    }
}

#[derive(Clone, Debug, Default)]
pub struct SheetCfv {
    pub next_id: u32,
    pub rules: Vec<CfRule>,
    pub blocks: Vec<CfBlock>,

    pub tree: DfRtree<u32>,
    pub next_uid: u32,
    pub dvs: Vec<Dv>,

    pub retired: Vec<CfRule>,
}

#[derive(Clone, Debug, Default, PartialEq)]
pub struct CfOutcome {
    pub style: Option<Style>,
}

fn a1_union(applies: &[Rect]) -> String {
    let parts: Vec<String> = applies
        .iter()
        .map(|r| {
            format!(
                "${}${}:${}${}",
                crate::address::col_to_letters(r.c0),
                r.r0 + 1,
                crate::address::col_to_letters(r.c1),
                r.r1 + 1
            )
        })
        .collect();
    if parts.len() == 1 {
        parts[0].clone()
    } else {
        format!("({})", parts.join(","))
    }
}

fn quote(s: &str) -> String {
    format!("\"{}\"", s.replace('"', "\"\""))
}

impl Workbook {
    pub fn cfv_sheet(&self, sheet: SheetId) -> Option<&SheetCfv> {
        self.cfv.get(&sheet)
    }

    pub fn cfv_sheet_mut(&mut self, sheet: SheetId) -> &mut SheetCfv {
        self.cfv.entry(sheet).or_default()
    }

    pub fn cfv_to_relative(&self, text: &str, anchor: CellAddr) -> Result<String, String> {
        let (_, norm) = self
            .compile_normalized(text.trim_start_matches('='), anchor)
            .map_err(|e| e.to_string())?;
        Ok(format!("={norm}"))
    }

    pub fn cfv_bare_area(&self, text: &str, anchor: CellAddr) -> bool {
        self.compile_normalized(text.trim_start_matches('='), anchor)
            .is_ok_and(|(f, _)| {
                matches!(
                    f.root,
                    Node::Ref(_, CRef::Area { .. } | CRef::Cols { .. } | CRef::Rows { .. })
                )
            })
    }

    pub fn cfv_threshold_formula(&self, text: &str, anchor: CellAddr) -> Option<String> {
        let (f, norm) = self
            .compile_normalized(text.trim_start_matches('='), anchor)
            .ok()?;
        let mut relative = false;
        crate::formula::walk(&f.root, &mut |n| {
            if let Node::Ref(_, c) = n {
                relative |= match *c {
                    CRef::Cell { row, col } => !row.abs || !col.abs,
                    CRef::Area { r0, c0, r1, c1 } => !(r0.abs && c0.abs && r1.abs && c1.abs),
                    CRef::Cols { c0, c1 } => !(c0.abs && c1.abs),
                    CRef::Rows { r0, r1 } => !(r0.abs && r1.abs),
                };
            }
        });
        (!relative).then(|| format!("={norm}"))
    }

    pub fn cfv_retarget(&self, text: &str, from: CellAddr, to: CellAddr) -> Option<String> {
        let (f, _) = self
            .compile_normalized(text.trim_start_matches('='), from)
            .ok()?;

        let wrap = |rc: crate::formula::Rc, host: u32, max: u32| {
            if rc.abs {
                rc
            } else {
                let x = (i64::from(host) + i64::from(rc.v)).rem_euclid(i64::from(max));
                crate::formula::Rc {
                    v: (x - i64::from(host)) as i32,
                    abs: false,
                }
            }
        };
        let (mr, mc) = (crate::address::MAX_ROWS, crate::address::MAX_COLS);
        let root = crate::structure::map_refs(&f.root, &mut |_, c| {
            Some(Some(match *c {
                CRef::Cell { row, col } => CRef::Cell {
                    row: wrap(row, to.row, mr),
                    col: wrap(col, to.col, mc),
                },
                CRef::Area { r0, c0, r1, c1 } => CRef::Area {
                    r0: wrap(r0, to.row, mr),
                    c0: wrap(c0, to.col, mc),
                    r1: wrap(r1, to.row, mr),
                    c1: wrap(c1, to.col, mc),
                },
                CRef::Cols { c0, c1 } => CRef::Cols {
                    c0: wrap(c0, to.col, mc),
                    c1: wrap(c1, to.col, mc),
                },
                CRef::Rows { r0, r1 } => CRef::Rows {
                    r0: wrap(r0, to.row, mr),
                    r1: wrap(r1, to.row, mr),
                },
            }))
        });
        Some(format!(
            "={}",
            crate::formula_print::print_node(self, &root, to)
        ))
    }

    pub fn cfv_from_relative(&self, text: &str, _anchor: CellAddr) -> Option<String> {
        Some(text.to_string())
    }

    pub fn dv_list_source_ok(&self, text: &str, anchor: CellAddr) -> bool {
        let Ok((f, _)) = self.compile_normalized(text.trim_start_matches('='), anchor) else {
            return false;
        };
        let mut has_op = false;
        crate::formula::walk(&f.root, &mut |n| {
            has_op |= matches!(
                n,
                Node::Binary(BinOp::Range | BinOp::Intersect | BinOp::Union, ..)
            );
        });
        if has_op {
            return false;
        }
        let mut ctx = crate::eval::EvalCtx::new(self, anchor);
        match ctx.eval(&f.root) {
            crate::eval::Operand::Ref(crate::eval::Reference::Area(a)) => {
                a.rect.r0 == a.rect.r1 || a.rect.c0 == a.rect.c1
            }
            _ => false,
        }
    }

    pub fn cfv_eval(&self, text: &str, anchor: CellAddr, host: CellAddr) -> Value {
        let Ok((f, _)) = self.compile_normalized(text.trim_start_matches('='), anchor) else {
            return Value::Error(crate::value::XlError::VALUE);
        };
        let mut ctx = crate::eval::EvalCtx::new(self, host);
        crate::eval::eval_formula(&mut ctx, &f.root)
    }

    pub fn eval_function_values(
        &self,
        name: &str,
        args: &[Option<Value>],
        refs: &[Option<String>],
        anchor: CellAddr,
        host: CellAddr,
    ) -> Option<Value> {
        let by_value = |i: usize| {
            !matches!(refs.get(i), Some(Some(_))) && matches!(args.get(i), Some(Some(_)))
        };
        let mut text = format!("{name}(");
        for (i, _) in args.iter().enumerate() {
            if i > 0 {
                text.push(',');
            }
            if let Some(Some(r)) = refs.get(i) {
                text.push_str(r);
            } else if by_value(i) {
                text.push('0');
            }
        }
        text.push(')');
        let (mut f, _) = self.compile_normalized(&text, anchor).ok()?;
        let mut root = &mut f.root;
        while let Node::Ws { node, .. } = root {
            root = node;
        }
        let Node::Func { args: nodes, .. } = &mut *root else {
            return None;
        };
        if nodes.len() != args.len() {
            return None;
        }
        for (i, n) in nodes.iter_mut().enumerate() {
            if by_value(i) {
                if let Some(Some(v)) = args.get(i) {
                    *n = Node::Lit(v.clone());
                }
            }
        }
        let mut ctx = crate::eval::EvalCtx::new(self, host);
        Some(crate::eval::finalize_keeping_nil(ctx.call_back(root)))
    }
    fn cfv_truthy(&self, text: &str, anchor: CellAddr, host: CellAddr) -> bool {
        match self.cfv_eval(text, anchor, host) {
            Value::Bool(b) => b,
            Value::Number(n) => n != 0.0,
            _ => false,
        }
    }

    pub fn cf_condition_text(&self, sheet: SheetId, rule: &CfRule) -> String {
        let areas = self.cfv.get(&sheet).map_or(&[][..], |s| s.areas(rule.id));
        self.cf_condition(sheet, rule, areas)
    }

    fn cf_condition(&self, sheet: SheetId, rule: &CfRule, areas: &[Rect]) -> String {
        let (r0, c0) = bbox_tl(areas);
        let cell = CellAddr::new(sheet, r0, c0).a1();
        self.cf_condition_raw(rule, areas).replace("@@", &cell)
    }

    fn cf_condition_raw(&self, rule: &CfRule, areas: &[Rect]) -> String {
        let rng = a1_union(areas);
        let f1 = rule.f1.clone().unwrap_or_default();
        let f2 = rule.f2.clone().unwrap_or_default();
        let strip = |s: &str| s.trim_start_matches('=').to_string();
        match &rule.kind {
            CfKind::CellValue(op) => {
                let a = strip(&f1);
                let b = strip(&f2);
                match op {
                    1 => format!("=AND(@@>=({a}),@@<=({b}))"),
                    2 => format!("=NOT(AND(@@>=({a}),@@<=({b})))"),
                    3 => format!("=@@=({a})"),
                    4 => format!("=@@<>({a})"),
                    5 => format!("=@@>({a})"),
                    6 => format!("=@@<({a})"),
                    7 => format!("=@@>=({a})"),
                    _ => format!("=@@<=({a})"),
                }
            }
            CfKind::Expression => f1,
            CfKind::ColorScale(_) => "=FALSE".into(),
            CfKind::Top10 {
                bottom,
                percent,
                rank,
            } => {
                let n = if *percent {
                    format!("MAX(1,INT(COUNT({rng})*{rank}/100))")
                } else {
                    format!("MIN({rank},COUNT({rng}))")
                };
                let (f, c) = if *bottom { (15, "<=") } else { (14, ">=") };

                format!("=AND(ISNUMBER(@@),@@{c}AGGREGATE({f},6,{rng},{n}))")
            }
            CfKind::Unique { duplicate } => {
                let count = areas
                    .iter()
                    .map(|r| format!("COUNTIF({},@@)", a1_union(std::slice::from_ref(r))))
                    .collect::<Vec<_>>()
                    .join("+");
                let op = if *duplicate { ">1" } else { "=1" };
                format!("=AND(NOT(ISBLANK(@@)),({count}){op})")
            }

            CfKind::Text { text, .. } if text.is_empty() => "=FALSE".into(),
            CfKind::Text { op, text } => {
                let t = quote(text);
                match op {
                    0 => format!("=NOT(ISERROR(SEARCH({t},@@)))"),
                    1 => format!("=ISERROR(SEARCH({t},@@))"),
                    2 => format!("=LEFT(@@,LEN({t}))={t}"),
                    _ => format!("=RIGHT(@@,LEN({t}))={t}"),
                }
            }
            CfKind::DataBar(_) | CfKind::IconSet(_) => "=FALSE".into(),
            CfKind::Blanks => "=LEN(TRIM(@@))=0".into(),
            CfKind::NoBlanks => "=LEN(TRIM(@@))>0".into(),
            CfKind::Errors => "=ISERROR(@@)".into(),
            CfKind::NoErrors => "=NOT(ISERROR(@@))".into(),
            CfKind::TimePeriod(p) => {
                let d = "INT(@@)";
                let wk = |shift: i32| {
                    format!(
                        "AND(ISNUMBER(@@),{d}-TODAY()+WEEKDAY(TODAY())-({shift})>=1,{d}-TODAY()+WEEKDAY(TODAY())-({shift})<=7)"
                    )
                };
                let mo = |m: i32| {
                    format!(
                        "AND(ISNUMBER(@@),YEAR({d})*12+MONTH({d})=YEAR(TODAY())*12+MONTH(TODAY())+({m}))"
                    )
                };
                match p {
                    0 => "=FLOOR(@@,1)=TODAY()".to_string(),
                    1 => format!("=AND(ISNUMBER(@@),{d}=TODAY()-1)"),
                    2 => format!("=AND(ISNUMBER(@@),{d}<=TODAY(),{d}>TODAY()-7)"),
                    3 => format!("={}", wk(0)),
                    4 => format!("={}", wk(-7)),
                    5 => format!("={}", mo(-1)),
                    6 => format!("=AND(ISNUMBER(@@),{d}=TODAY()+1)"),
                    7 => format!("={}", wk(7)),
                    8 => format!("={}", mo(1)),
                    _ => format!("={}", mo(0)),
                }
            }
            CfKind::AboveAverage {
                below,
                equal,
                std,
                num_std,
            } => {
                let c = match (below, equal) {
                    (false, false) => ">",
                    (false, true) => ">=",
                    (true, false) => "<",
                    (true, true) => "<=",
                };
                let sign = if *below { "-" } else { "+" };
                let sd = if *std {
                    format!("{sign}{num_std}*STDEV({rng})")
                } else {
                    String::new()
                };
                format!("=AND(ISNUMBER(@@),@@{c}AGGREGATE(1,6,{rng}){sd})")
            }
        }
    }

    pub fn cf_holds(&self, sheet: SheetId, index: usize, row: u32, col: u32) -> bool {
        let Some(sh) = self.cfv.get(&sheet) else {
            return false;
        };
        let Some(rule) = sh.rules.get(index) else {
            return false;
        };
        if !sh.covers(rule.id, row, col) {
            return false;
        }
        let host = CellAddr::new(sheet, row, col);
        let areas = sh.areas(rule.id);
        let mut cond = self.cf_condition(sheet, rule, areas);
        let (r0, c0) = bbox_tl(areas);
        if rule.kind == CfKind::NoBlanks {
            let cell = CellAddr::new(sheet, r0, c0).a1();
            cond = format!("=IF(ISERROR({cell}),TRUE,{})", cond.trim_start_matches('='));
        }
        self.cfv_truthy(&cond, CellAddr::new(sheet, r0, c0), host)
    }

    fn scale_color(&self, addr: CellAddr, areas: &[Rect], crits: &[ScaleCrit]) -> Option<u32> {
        let Value::Number(v) = self.value(addr) else {
            return None;
        };
        let mut nums: Vec<f64> = Vec::new();
        let used = self.used_range(addr.sheet);
        for r in areas {
            let r = match used {
                Some(u) => Rect {
                    r0: r.r0.max(u.r0),
                    c0: r.c0.max(u.c0),
                    r1: r.r1.min(u.r1),
                    c1: r.c1.min(u.c1),
                },
                None => continue,
            };
            if r.r0 > r.r1 || r.c0 > r.c1 {
                continue;
            }
            for row in r.r0..=r.r1 {
                for col in r.c0..=r.c1 {
                    if let Value::Number(n) = self.value(CellAddr::new(addr.sheet, row, col)) {
                        nums.push(n);
                    }
                }
            }
        }
        if nums.is_empty() {
            return None;
        }
        let lo = nums.iter().copied().fold(f64::INFINITY, f64::min);
        let hi = nums.iter().copied().fold(f64::NEG_INFINITY, f64::max);
        let (r0, c0) = bbox_tl(areas);
        let anchor = CellAddr::new(addr.sheet, r0, c0);

        let mut at: Vec<f64> = Vec::with_capacity(crits.len());
        for c in crits {
            let val = match c
                .formula
                .as_deref()
                .map(|f| self.cfv_eval(f, anchor, anchor))
            {
                Some(Value::Number(n)) => n,
                Some(_) => return None,
                None => c.value,
            };
            at.push(match c.ty {
                1 => lo,
                2 => hi,
                3 => x87::add(x87::div(x87::mul(x87::sub(hi, lo), val), 100.0), lo),
                5 => crate::functions::criteria::stats::percentile_inc(&nums, x87::div(val, 100.0))
                    .ok()?,
                _ => val,
            });
        }
        let colors: Vec<u32> = crits.iter().map(|c| c.color).collect();
        Some(crate::cfgradient::scale_color(&at, &colors, v))
    }

    pub fn display_style(&self, addr: CellAddr) -> Style {
        let mut style = self.cell_style(addr).clone();
        let Some(sh) = self.cfv.get(&addr.sheet) else {
            return style;
        };

        let mut blocks: Vec<(i32, Style, DxfMask)> = Vec::new();
        let mut seen: Vec<usize> = Vec::new();
        for rule in &sh.rules {
            let Some(b) = sh.block_of(rule.id) else {
                continue;
            };
            let block = &sh.blocks[b];
            let covers = block.areas.iter().any(|a| a.contains(addr.row, addr.col));
            if !covers || seen.contains(&b) {
                continue;
            }
            seen.push(b);
            let mut first = None;
            let mut s = Style::default();
            let mut taken = DxfMask::default();
            for (i, r) in sh.rules.iter().enumerate() {
                if !block.rules.contains(&r.id) {
                    continue;
                }
                let dxf = match &r.kind {
                    CfKind::ColorScale(crits) => {
                        match self.scale_color(addr, &block.areas, crits) {
                            Some(rgb) => {
                                let mut d = Dxf::default();
                                d.style.fill.pattern = crate::style::Pattern::Solid;
                                d.style.fill.color = crate::style::CellColor::Rgb(rgb);
                                d.mask.pattern = true;
                                d.mask.fill_color = true;
                                d
                            }
                            None => continue,
                        }
                    }
                    _ if self.cf_holds(addr.sheet, i, addr.row, addr.col) => r.dxf.clone(),
                    _ => continue,
                };
                first.get_or_insert(r.prio);
                dxf.apply_over(&mut s, &mut taken);
                if r.stop && !matches!(r.kind, CfKind::ColorScale(_)) {
                    break;
                }
            }
            if let Some(p) = first {
                blocks.push((p, s, taken));
            }
        }
        blocks.sort_by_key(|b| b.0);
        let mut got = DxfMask::default();
        for (_, s, m) in blocks {
            let font = m.bold || m.italic || m.underline || m.strike || m.font_color;
            if font && !got.bold {
                got.bold = true;
                let d = Dxf {
                    style: s.clone(),
                    mask: DxfMask {
                        bold: m.bold,
                        italic: m.italic,
                        underline: m.underline,
                        strike: m.strike,
                        font_color: m.font_color,
                        ..DxfMask::default()
                    },
                    ..Dxf::default()
                };
                d.apply_over(&mut style, &mut DxfMask::default());
            }
            let fill = m.pattern || m.fill_color || m.pattern_color;
            if fill && !got.pattern {
                got.pattern = true;
                let d = Dxf {
                    style: s.clone(),
                    mask: DxfMask {
                        pattern: m.pattern,
                        fill_color: m.fill_color,
                        pattern_color: m.pattern_color,
                        ..DxfMask::default()
                    },
                    ..Dxf::default()
                };
                d.apply_over(&mut style, &mut DxfMask::default());
            }
            let border = m.left || m.right || m.top || m.bottom;
            if border && !got.left {
                got.left = true;
                let d = Dxf {
                    style: s.clone(),
                    mask: DxfMask {
                        left: m.left,
                        right: m.right,
                        top: m.top,
                        bottom: m.bottom,
                        ..DxfMask::default()
                    },
                    ..Dxf::default()
                };
                d.apply_over(&mut style, &mut DxfMask::default());
            }
            if m.number_format && !got.number_format {
                got.number_format = true;
                style.number_format = s.number_format.clone();
            }
        }
        style
    }

    pub fn dv_at(&self, addr: CellAddr) -> Option<&Dv> {
        self.cfv
            .get(&addr.sheet)?
            .dvs
            .iter()
            .find(|d| d.applies.iter().any(|r| r.contains(addr.row, addr.col)))
    }

    pub fn dv_value(&self, addr: CellAddr) -> Option<bool> {
        let dv = self.dv_at(addr)?;
        let v = self.value(addr);
        if dv.kind == 0 {
            return Some(true);
        }
        if v.is_blank() && dv.ignore_blank {
            return Some(true);
        }
        let (ar, ac) = dv.anchor();
        let anchor = CellAddr::new(addr.sheet, ar, ac);
        let operand = |f: &Option<String>| -> Option<Value> {
            let f = f.as_ref()?;
            if f.starts_with('=') {
                Some(self.cfv_eval(f, anchor, addr))
            } else {
                Some(
                    f.trim()
                        .parse::<f64>()
                        .map(Value::Number)
                        .unwrap_or_else(|_| Value::Text(Arc::from(f.as_str()))),
                )
            }
        };

        let num = |v: &Value| match v {
            Value::Number(n) => Some((0u8, *n)),
            Value::Bool(b) => Some((2u8, f64::from(u8::from(*b)))),
            Value::Blank => Some((0u8, 0.0)),
            _ => None,
        };
        let cmp = |x: (u8, f64)| -> bool {
            let limit = |v: &Value| match v {
                Value::Text(_) => Some((1u8, 0.0)),
                v => num(v),
            };
            let a = operand(&dv.f1).as_ref().and_then(limit);
            let b = operand(&dv.f2).as_ref().and_then(limit);
            let (Some(a), bb) = (a, b) else { return false };
            let ge = |p: (u8, f64), q: (u8, f64)| p.0 > q.0 || (p.0 == q.0 && p.1 >= q.1);
            let gt = |p: (u8, f64), q: (u8, f64)| p.0 > q.0 || (p.0 == q.0 && p.1 > q.1);
            match dv.op {
                1 => bb.is_some_and(|b| ge(x, a) && ge(b, x)),
                2 => bb.is_some_and(|b| !(ge(x, a) && ge(b, x))),
                3 => x == a,
                4 => x != a,
                5 => gt(x, a),
                6 => gt(a, x),
                7 => ge(x, a),
                8 => ge(a, x),
                _ => false,
            }
        };
        Some(match dv.kind {
            1 => num(&v).is_some_and(|x| (x.0 == 2 || x.1 == x.1.trunc()) && cmp(x)),
            2 | 4 | 5 => num(&v).is_some_and(cmp),
            6 => {
                let len = match &v {
                    Value::Text(t) => t.encode_utf16().count() as f64,
                    Value::Blank => 0.0,
                    Value::Error(_) => return Some(false),
                    other => self.display_text_of(other).encode_utf16().count() as f64,
                };
                cmp((0, len))
            }
            7 => dv
                .f1
                .as_ref()
                .is_some_and(|f| self.cfv_truthy(f, anchor, addr)),
            _ => self.dv_list_has(dv, addr, &v),
        })
    }

    pub fn dv_limit_ok(&self, kind: i32, text: &str, anchor: CellAddr) -> bool {
        let n = if text.starts_with('=') {
            match self.cfv_eval(text, anchor, anchor) {
                Value::Number(n) => n,
                Value::Blank => 0.0,
                _ => return true,
            }
        } else if let Some(n) = self.dv_literal(kind, text) {
            n
        } else {
            return true;
        };
        match kind {
            1 if !text.starts_with('=') => n == n.trunc(),
            4 => (1.0..2_958_466.0).contains(&n),
            5 => (0.0..1.0).contains(&n),
            6 => n >= 0.0 && n == n.trunc(),
            _ => true,
        }
    }

    pub fn dv_literal(&self, kind: i32, text: &str) -> Option<f64> {
        if let Ok(n) = text.trim().parse::<f64>() {
            return Some(n);
        }
        if !matches!(kind, 4 | 5) {
            return None;
        }
        let f = format!("=VALUE({})", quote(text));
        match self.cfv_eval(
            &f,
            CellAddr::new(SheetId(0), 0, 0),
            CellAddr::new(SheetId(0), 0, 0),
        ) {
            Value::Number(n) => Some(n),
            _ => None,
        }
    }

    pub fn dv_literal_shown(&self, kind: i32, n: f64) -> String {
        let at = CellAddr::new(SheetId(0), 0, 0);
        let fmt = if kind == 4 {
            "d/mm/yyyy"
        } else {
            "h:mm:ss AM/PM"
        };
        let f = format!("=TEXT({n},\"{fmt}\")");
        match self.cfv_eval(&f, at, at) {
            Value::Text(t) => {
                if kind == 4 {
                    t.to_string()
                } else {
                    t.to_lowercase()
                }
            }
            _ => String::new(),
        }
    }

    fn display_text_of(&self, v: &Value) -> String {
        match v {
            Value::Number(n) => crate::number::format_general(*n),
            Value::Bool(b) => if *b { "TRUE" } else { "FALSE" }.into(),
            Value::Text(t) => t.to_string(),
            _ => String::new(),
        }
    }

    fn dv_list_has(&self, dv: &Dv, addr: CellAddr, v: &Value) -> bool {
        let Some(f1) = dv.f1.as_ref() else {
            return false;
        };
        let shown = self.display_text_of(v);
        if let Some(f) = f1.strip_prefix('=') {
            let _ = f;

            let src = self.cfv_eval(f1, dv_anchor(dv, addr), addr);
            let mut items = Vec::new();
            match src {
                Value::Array(a) => {
                    for x in a.data.iter() {
                        items.push(x.clone());
                    }
                }
                other => items.push(other),
            }
            return items.iter().any(|x| match (x, v) {
                (Value::Number(a), Value::Number(b)) => a == b,
                (Value::Text(a), Value::Text(b)) => crate::text::eq_ci(a, b),
                (Value::Bool(a), Value::Bool(b)) => a == b,
                _ => false,
            });
        }
        if matches!(v, Value::Error(_)) {
            return false;
        }
        let shown = shown.trim();
        f1.split(',').any(|item| item.trim() == shown)
    }

    pub(crate) fn cfv_shift(&mut self, s: &Shift) {
        self.cfv_shift_all(s);
    }
}

impl SheetCfv {
    pub fn set_priority(&mut self, pos: usize, p: i32) {
        for (i, r) in self.rules.iter_mut().enumerate() {
            if i != pos && r.prio >= p {
                r.prio += 1;
            }
        }
        self.rules[pos].prio = p;
        self.rules.sort_by_key(|r| r.prio);
    }

    pub fn index_of(&self, id: u32) -> Option<usize> {
        self.rules.iter().position(|r| r.id == id)
    }

    pub fn add_rule(&mut self, rule: CfRule, areas: Vec<Rect>) -> u32 {
        self.add_rule_skipping(rule, 1, areas)
    }

    pub fn add_rule_skipping(&mut self, rule: CfRule, step: i32, areas: Vec<Rect>) -> u32 {
        let prio = self.rules.iter().map(|r| r.prio).max().unwrap_or(0) + step;
        self.add_rule_at(rule, prio, areas)
    }
}

fn dv_anchor(dv: &Dv, at: CellAddr) -> CellAddr {
    let (r, c) = dv.anchor();
    CellAddr::new(at.sheet, r, c)
}

#[cfg(test)]
mod tests {
    use super::*;

    fn rule(kind: CfKind, prio: i32, areas: &[(u32, u32, u32, u32)], stop: bool) -> Placed {
        Placed {
            block: 0,
            rule: CfRule {
                id: 0,
                kind,
                stop,
                prio,
                f1: Some(format!("={prio}")),
                f2: None,
                dxf: Dxf::default(),
            },
            areas: areas
                .iter()
                .map(|&(a, b, c, d)| Rect::new(a, b, c, d))
                .collect(),
            seq: 0,
        }
    }

    fn areas(v: &[Rect]) -> Vec<(u32, u32, u32, u32)> {
        v.iter().map(|r| (r.r0, r.c0, r.r1, r.c1)).collect()
    }

    #[test]
    fn load_order_moves_areas_under_a_conflicting_rule_first() {
        let top = CfKind::Top10 {
            bottom: false,
            percent: false,
            rank: 10,
        };
        let mut v = vec![
            rule(top, 1, &[(6, 0, 7, 0), (6, 3, 9, 3)], false),
            rule(CfKind::NoBlanks, 2, &[(0, 0, 1, 1), (6, 3, 9, 4)], true),
            rule(CfKind::Blanks, 4, &[(6, 0, 7, 0), (6, 3, 9, 3)], true),
            rule(
                CfKind::CellValue(7),
                5,
                &[(6, 0, 11, 0), (7, 3, 9, 3)],
                true,
            ),
        ];
        load_order(&mut v);
        assert_eq!(areas(&v[0].areas), [(6, 0, 7, 0), (6, 3, 9, 3)]);
        assert_eq!(areas(&v[1].areas), [(6, 3, 9, 4), (0, 0, 1, 1)]);
        assert_eq!(areas(&v[2].areas), [(6, 3, 9, 3), (6, 0, 7, 0)]);
        assert_eq!(areas(&v[3].areas), [(6, 0, 11, 0), (7, 3, 9, 3)]);
    }

    #[test]
    fn load_order_joins_and_sorts_formula_rules_only() {
        let mut v = vec![
            rule(
                CfKind::Expression,
                1,
                &[(0, 1, 0, 1), (0, 0, 0, 0), (1, 0, 1, 0)],
                false,
            ),
            rule(
                CfKind::Top10 {
                    bottom: false,
                    percent: false,
                    rank: 10,
                },
                2,
                &[(0, 2, 0, 2), (0, 0, 0, 0)],
                false,
            ),
        ];
        load_order(&mut v);
        assert_eq!(areas(&v[0].areas), [(0, 1, 0, 1), (0, 0, 1, 0)]);
        assert_eq!(areas(&v[1].areas), [(0, 2, 0, 2), (0, 0, 0, 0)]);
    }

    #[test]
    fn constant_cell_value_conditions_that_cannot_meet_do_not_conflict() {
        assert!(!values_meet(3, Some("=1"), None, 3, Some("=2"), None));
        assert!(values_meet(3, Some("=1"), None, 3, Some("=1"), None));
        assert!(values_meet(5, Some("=3"), None, 1, Some("=1"), Some("=5")));
        assert!(!values_meet(6, Some("=1"), None, 5, Some("=5"), None));
        assert!(values_meet(3, Some("=R1C1"), None, 3, Some("=2"), None));
    }
}
