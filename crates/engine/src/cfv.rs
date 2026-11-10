use std::sync::Arc;

use crate::address::{CellAddr, Rect, SheetId};
use crate::formula::Node;
use crate::parser::ast::BinOp;
use crate::structure::Shift;
use crate::style::Style;
use crate::value::Value;
use crate::workbook::Workbook;

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
    pub left: bool,
    pub right: bool,
    pub top: bool,
    pub bottom: bool,
}

#[derive(Clone, Debug, Default, PartialEq, Eq)]
pub struct Dxf {
    pub style: Style,
    pub mask: DxfMask,
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
        let step = if n == 3 {
            100.0 / 3.0
        } else {
            100.0 / n as f64
        };
        let crits = (0..n)
            .map(|k| IconCrit {
                ty: 3,
                value: if n == 3 {
                    [0.0, 33.0, 67.0][k]
                } else {
                    (step * k as f64).round()
                },
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
        })
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

    pub applies: Vec<Rect>,
    pub stop: bool,

    pub prio: i32,

    pub seq: u32,

    pub f1: Option<String>,
    pub f2: Option<String>,
    pub dxf: Dxf,
}

impl CfRule {
    pub fn anchor(&self) -> (u32, u32) {
        crate::cfv_edit::bbox_tl(&self.applies)
    }
    pub fn covers(&self, row: u32, col: u32) -> bool {
        self.applies.iter().any(|r| r.contains(row, col))
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

    pub touched: bool,
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
    pub dvs: Vec<Dv>,
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

    pub fn cfv_retarget(&self, text: &str, from: CellAddr, to: CellAddr) -> Option<String> {
        let (f, _) = self
            .compile_normalized(text.trim_start_matches('='), from)
            .ok()?;
        Some(format!(
            "={}",
            crate::formula_print::print_node(self, &f.root, to)
        ))
    }

    pub fn cfv_from_relative(&self, text: &str, _anchor: CellAddr) -> Option<String> {
        Some(text.to_string())
    }

    pub fn dv_list_source_ok(&self, text: &str, anchor: CellAddr) -> bool {
        let Ok((f, _)) = self.compile_normalized(text.trim_start_matches('='), anchor) else {
            return false;
        };
        let mut root = f.root.unwrap_ws();
        while let Node::Paren(inner) = root {
            root = inner.unwrap_ws();
        }
        if matches!(
            root,
            Node::Binary(BinOp::Range | BinOp::Intersect | BinOp::Union, ..)
        ) {
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
    fn cfv_truthy(&self, text: &str, anchor: CellAddr, host: CellAddr) -> bool {
        match self.cfv_eval(text, anchor, host) {
            Value::Bool(b) => b,
            Value::Number(n) => n != 0.0,
            _ => false,
        }
    }

    pub fn cf_condition_text(&self, sheet: SheetId, rule: &CfRule) -> String {
        self.cf_condition(sheet, rule)
    }

    fn cf_condition(&self, sheet: SheetId, rule: &CfRule) -> String {
        let (r0, c0) = rule.anchor();
        let cell = CellAddr::new(sheet, r0, c0).a1();
        self.cf_condition_raw(rule).replace("@@", &cell)
    }

    fn cf_condition_raw(&self, rule: &CfRule) -> String {
        let rng = a1_union(&rule.applies);
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
                let first = a1_union(&rule.applies[..1]);
                let op = if *duplicate { ">1" } else { "=1" };
                format!("=AND(NOT(ISBLANK(@@)),COUNTIF({first},@@){op})")
            }
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
                    0 => format!("=AND(ISNUMBER(@@),{d}=TODAY())"),
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
        let Some(rule) = self.cfv.get(&sheet).and_then(|s| s.rules.get(index)) else {
            return false;
        };
        if !rule.covers(row, col) {
            return false;
        }
        let host = CellAddr::new(sheet, row, col);
        let mut cond = self.cf_condition(sheet, rule);
        let (r0, c0) = rule.anchor();
        if rule.kind == CfKind::NoBlanks {
            let cell = CellAddr::new(sheet, r0, c0).a1();
            cond = format!("=IF(ISERROR({cell}),TRUE,{})", cond.trim_start_matches('='));
        }
        self.cfv_truthy(&cond, CellAddr::new(sheet, r0, c0), host)
    }

    fn scale_color(&self, addr: CellAddr, rule: &CfRule, crits: &[ScaleCrit]) -> Option<u32> {
        let Value::Number(v) = self.value(addr) else {
            return None;
        };
        let mut nums: Vec<f64> = Vec::new();
        let used = self.used_range(addr.sheet);
        for r in &rule.applies {
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
        nums.sort_by(f64::total_cmp);
        let (lo, hi) = (nums[0], nums[nums.len() - 1]);
        let (r0, c0) = rule.anchor();
        let anchor = CellAddr::new(addr.sheet, r0, c0);
        let at: Vec<f64> = crits
            .iter()
            .map(|c| match c.ty {
                1 => lo,
                2 => hi,
                3 => lo + (hi - lo) * c.value / 100.0,
                5 => {
                    let k = (nums.len() - 1) as f64 * c.value / 100.0;
                    let i = k.floor() as usize;
                    let f = k - i as f64;
                    if i + 1 < nums.len() {
                        nums[i] + f * (nums[i + 1] - nums[i])
                    } else {
                        nums[i]
                    }
                }
                4 => match c
                    .formula
                    .as_deref()
                    .map(|f| self.cfv_eval(f, anchor, anchor))
                {
                    Some(Value::Number(n)) => n,
                    _ => c.value,
                },
                _ => c.value,
            })
            .collect();
        let n = crits.len();
        if v <= at[0] {
            return Some(crits[0].color);
        }
        if v >= at[n - 1] {
            return Some(crits[n - 1].color);
        }
        let i = (1..n).find(|&i| v <= at[i])?;
        let (a, b) = (at[i - 1], at[i]);
        let t = if b > a { (v - a) / (b - a) } else { 1.0 };
        let (c1, c2) = (crits[i - 1].color, crits[i].color);
        let mut out = 0u32;
        for shift in [16, 8, 0] {
            let x = f64::from((c1 >> shift) & 255);
            let y = f64::from((c2 >> shift) & 255);
            let ch = (x + ((y - x) * t).trunc()) as u32;
            out |= ch << shift;
        }
        Some(out)
    }

    pub fn display_style(&self, addr: CellAddr) -> Style {
        let mut style = self.cell_style(addr).clone();
        let Some(sh) = self.cfv.get(&addr.sheet) else {
            return style;
        };
        let mut taken = DxfMask::default();
        for (i, rule) in sh.rules.iter().enumerate() {
            if let CfKind::ColorScale(crits) = &rule.kind {
                if rule.covers(addr.row, addr.col) && !taken.fill_color {
                    if let Some(rgb) = self.scale_color(addr, rule, crits) {
                        let mut d = Dxf::default();
                        d.style.fill.pattern = crate::style::Pattern::Solid;
                        d.style.fill.color = crate::style::CellColor::Rgb(rgb);
                        d.mask.pattern = true;
                        d.mask.fill_color = true;
                        d.apply_over(&mut style, &mut taken);
                    }
                }
                continue;
            }
            if !rule.covers(addr.row, addr.col) || !self.cf_holds(addr.sheet, i, addr.row, addr.col)
            {
                continue;
            }
            rule.dxf.apply_over(&mut style, &mut taken);
            if rule.stop {
                break;
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
            Value::Bool(b) => Some((1u8, f64::from(u8::from(*b)))),
            Value::Blank => Some((0u8, 0.0)),
            _ => None,
        };
        let cmp = |x: (u8, f64)| -> bool {
            let a = operand(&dv.f1).as_ref().and_then(num);
            let b = operand(&dv.f2).as_ref().and_then(num);
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
            1 => num(&v).is_some_and(|x| (x.0 == 1 || x.1 == x.1.trunc()) && cmp(x)),
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
    pub fn renumber(&mut self) {
        for (i, r) in self.rules.iter_mut().enumerate() {
            r.prio = i as i32 + 1;
        }
    }

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

    pub fn add_rule(&mut self, rule: CfRule) -> u32 {
        self.add_rule_skipping(rule, 1)
    }

    pub fn add_rule_skipping(&mut self, mut rule: CfRule, step: i32) -> u32 {
        rule.prio = self.rules.iter().map(|r| r.prio).max().unwrap_or(0) + step;
        rule.seq = self.rules.iter().map(|r| r.seq).max().unwrap_or(0) + 1;
        self.next_id += 1;
        rule.id = self.next_id;
        self.rules.push(rule);
        self.next_id
    }
}

fn dv_anchor(dv: &Dv, at: CellAddr) -> CellAddr {
    let (r, c) = dv.anchor();
    CellAddr::new(at.sheet, r, c)
}
