use crate::x87;
use std::sync::Arc;

use crate::address::{CellAddr, Rect, SheetId};
use crate::calc::{self, RecalcReport};
use crate::datetime::{days_in_month, serial_from_ymd, weekday_from_serial, ymd_from_serial};
use crate::value::Value;
use crate::workbook::{CalcMode, EditError, Entry, Workbook};

#[derive(Clone, Copy, PartialEq, Eq, Debug)]
pub enum FillType {
    Default,
    Copy,
    Series,
    Formats,
    Values,
    Days,
    Weekdays,
    Months,
    Years,
    Linear,
    Growth,
}

impl FillType {
    pub fn parse(s: &str) -> Option<FillType> {
        Some(match s.to_ascii_lowercase().as_str() {
            "default" | "" => FillType::Default,
            "copy" => FillType::Copy,
            "series" => FillType::Series,
            "formats" => FillType::Formats,
            "values" => FillType::Values,
            "days" => FillType::Days,
            "weekdays" => FillType::Weekdays,
            "months" => FillType::Months,
            "years" => FillType::Years,
            "linear" => FillType::Linear,
            "growth" => FillType::Growth,
            _ => return None,
        })
    }
}

#[derive(Clone, Copy, PartialEq, Eq, Debug)]
enum Dir {
    Down,
    Up,
    Right,
    Left,
}

impl Dir {
    fn sign(self) -> i64 {
        match self {
            Dir::Down | Dir::Right => 1,
            _ => -1,
        }
    }
    fn vertical(self) -> bool {
        matches!(self, Dir::Down | Dir::Up)
    }
}

const DAYS_SHORT: [&str; 7] = ["Mon", "Tue", "Wed", "Thu", "Fri", "Sat", "Sun"];
const DAYS_LONG: [&str; 7] = [
    "Monday",
    "Tuesday",
    "Wednesday",
    "Thursday",
    "Friday",
    "Saturday",
    "Sunday",
];
const MONTHS_SHORT_NZ: [&str; 12] = [
    "Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sept", "Oct", "Nov", "Dec",
];
const MONTHS_SHORT_EN: [&str; 12] = [
    "Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep", "Oct", "Nov", "Dec",
];
const MONTHS_LONG: [&str; 12] = [
    "January",
    "February",
    "March",
    "April",
    "May",
    "June",
    "July",
    "August",
    "September",
    "October",
    "November",
    "December",
];
const Q_QTR: [&str; 4] = ["Qtr1", "Qtr2", "Qtr3", "Qtr4"];
const Q_QTR_SP: [&str; 4] = ["Qtr 1", "Qtr 2", "Qtr 3", "Qtr 4"];
const Q_QUARTER_SP: [&str; 4] = ["Quarter 1", "Quarter 2", "Quarter 3", "Quarter 4"];
const Q_QUARTER: [&str; 4] = ["Quarter1", "Quarter2", "Quarter3", "Quarter4"];
const Q_Q: [&str; 4] = ["Q1", "Q2", "Q3", "Q4"];
const Q_Q_SP: [&str; 4] = ["Q 1", "Q 2", "Q 3", "Q 4"];

struct Family {
    kind: u8,
    lists: &'static [&'static [&'static str]],
}

static FAMILIES: [Family; 10] = [
    Family {
        kind: 0,
        lists: &[&DAYS_SHORT],
    },
    Family {
        kind: 0,
        lists: &[&DAYS_LONG],
    },
    Family {
        kind: 1,
        lists: &[&MONTHS_LONG],
    },
    Family {
        kind: 1,
        lists: &[&MONTHS_SHORT_NZ, &MONTHS_SHORT_EN],
    },
    Family {
        kind: 2,
        lists: &[&Q_QTR],
    },
    Family {
        kind: 2,
        lists: &[&Q_QTR_SP],
    },
    Family {
        kind: 2,
        lists: &[&Q_QUARTER_SP],
    },
    Family {
        kind: 2,
        lists: &[&Q_QUARTER],
    },
    Family {
        kind: 2,
        lists: &[&Q_Q],
    },
    Family {
        kind: 2,
        lists: &[&Q_Q_SP],
    },
];

#[derive(Clone, Copy, PartialEq, Eq, Debug)]
enum Style {
    Lower,
    Upper,
    Title,
}

#[derive(Clone, Debug)]
struct ListItem {
    cands: Vec<(usize, usize, u8)>,
    lead: String,
    style: Style,
}

fn list_item(s: &str) -> Option<ListItem> {
    let t = s.trim_start();
    let lead = s[..s.len() - t.len()].to_string();
    let core = t.trim_end_matches([' ', '.']);
    if core.is_empty() {
        return None;
    }
    let mut cands = Vec::new();
    for (fi, f) in FAMILIES.iter().enumerate() {
        let n = f.lists[0].len();
        for idx in 0..n {
            let mut mask = 0u8;
            for (li, l) in f.lists.iter().enumerate() {
                if l[idx].eq_ignore_ascii_case(core) {
                    mask |= 1 << li;
                }
            }
            if mask != 0 {
                cands.push((fi, idx, mask));
            }
        }
    }
    if cands.is_empty() {
        return None;
    }
    let mut cs = core.chars();
    let c0 = cs.next().unwrap();
    let c1 = cs.next();
    let style = if c0.is_lowercase() {
        Style::Lower
    } else if c1.is_some_and(|c| c.is_uppercase()) {
        Style::Upper
    } else {
        Style::Title
    };
    Some(ListItem { cands, lead, style })
}

fn styled(name: &str, st: Style) -> String {
    match st {
        Style::Lower => name.to_lowercase(),
        Style::Upper => name.to_uppercase(),
        Style::Title => name.to_string(),
    }
}

#[derive(Clone, Debug)]
struct TextNum {
    prefix: String,
    num: u64,
    width: usize,
    suffix: String,
    ordinal: bool,
}

fn text_num(s: &str) -> Option<TextNum> {
    let b = s.as_bytes();
    let lead = b.iter().take_while(|c| c.is_ascii_digit()).count();
    let mk = |a: usize, z: usize, ordinal: bool| -> Option<TextNum> {
        let digits = &s[a..z];
        if digits.len() > 15 {
            return None;
        }
        Some(TextNum {
            prefix: s[..a].to_string(),
            num: digits.parse().ok()?,
            width: digits.len(),
            suffix: if ordinal {
                String::new()
            } else {
                s[z..].to_string()
            },
            ordinal,
        })
    };
    if lead > 0 {
        let rest = &s[lead..];
        if rest.is_empty() {
            return mk(0, lead, false);
        }
        let first = rest.chars().next().unwrap();
        if first.is_alphabetic() {
            let lower = rest.to_ascii_lowercase();
            if matches!(lower.as_str(), "st" | "nd" | "rd" | "th") {
                return mk(0, lead, true);
            }
        } else {
            return mk(0, lead, false);
        }
    }
    let tail = b.iter().rev().take_while(|c| c.is_ascii_digit()).count();
    if tail > 0 {
        return mk(s.len() - tail, s.len(), false);
    }
    None
}

fn ordinal_suffix(n: u64) -> &'static str {
    match (n % 100, n % 10) {
        (11..=13, _) => "th",
        (_, 1) => "st",
        (_, 2) => "nd",
        (_, 3) => "rd",
        _ => "th",
    }
}

#[derive(Clone, Copy, PartialEq, Eq, Debug)]
enum FmtKind {
    Number,
    Date { time: bool },
    Time,
}

fn fmt_kind(fmt: &str) -> FmtKind {
    let f = fmt.to_ascii_lowercase();

    let mut sect = String::new();
    let mut in_q = false;
    let mut chars = f.chars().peekable();

    let mut toks: Vec<char> = Vec::new();
    while let Some(c) = chars.next() {
        if in_q {
            if c == '"' {
                in_q = false;
            }
            continue;
        }
        match c {
            '"' => in_q = true,
            ';' => break,
            '\\' | '_' | '*' => {
                chars.next();
            }
            '[' => {
                let mut inner = String::new();
                for d in chars.by_ref() {
                    if d == ']' {
                        break;
                    }
                    inner.push(d);
                }
                let t = inner.trim_start_matches(['h', 'm', 's']);
                if t.is_empty() && !inner.is_empty() {
                    toks.push(inner.chars().next().unwrap());
                    sect.push('x');
                }
            }
            'a' | 'p' => {
                let rest: String = std::iter::once(c).chain(chars.clone()).take(5).collect();
                if rest.starts_with("am/pm") {
                    for _ in 0..4 {
                        chars.next();
                    }
                } else if rest.starts_with("a/p") {
                    for _ in 0..2 {
                        chars.next();
                    }
                }
            }
            'd' | 'y' | 'm' | 'h' | 's' => {
                if toks.last() != Some(&c) || sect.ends_with(|x: char| !x.is_alphabetic()) {
                    toks.push(c);
                }
                sect.push(c);
            }
            _ => sect.push(' '),
        }
    }

    let mut has_date = false;
    let mut has_time = false;
    let mut i = 0;

    let runs: Vec<char> = {
        let mut r = Vec::new();
        let mut prev = ' ';
        for c in sect.chars() {
            if "dymhs".contains(c) {
                if prev != c {
                    r.push(c);
                }
            } else if c == 'x' {
                r.push('x');
            }
            prev = c;
        }
        r
    };
    while i < runs.len() {
        match runs[i] {
            'd' | 'y' => has_date = true,
            'h' | 's' | 'x' => has_time = true,
            'm' => {
                let after_h = i > 0 && matches!(runs[i - 1], 'h' | 'x');
                let before_s = i + 1 < runs.len() && runs[i + 1] == 's';
                if after_h || before_s {
                    has_time = true;
                } else {
                    has_date = true;
                }
            }
            _ => {}
        }
        i += 1;
    }
    if has_date {
        FmtKind::Date { time: has_time }
    } else if has_time {
        FmtKind::Time
    } else {
        FmtKind::Number
    }
}

fn num_class(fmt: &str) -> u8 {
    let f = fmt.trim();
    if f.eq_ignore_ascii_case("general") {
        return 0;
    }
    let first = f.split(';').next().unwrap_or("");
    if first.to_ascii_lowercase().ends_with("]general") && first.starts_with("[$-") {
        return 8;
    }

    let mut plain = String::new();
    let mut currency = false;
    let mut chars = first.chars().peekable();
    while let Some(c) = chars.next() {
        match c {
            '"' => {
                for d in chars.by_ref() {
                    if d == '"' {
                        break;
                    }
                }
            }
            '\\' | '_' => {
                chars.next();
            }
            '[' => {
                let mut tag = String::new();
                for d in chars.by_ref() {
                    if d == ']' {
                        break;
                    }
                    tag.push(d);
                }
                if let Some(rest) = tag.strip_prefix('$') {
                    let sym = rest.split('-').next().unwrap_or("");
                    if !sym.is_empty() {
                        currency = true;
                    }
                }
            }
            c => plain.push(c),
        }
    }
    let lower = plain.to_ascii_lowercase();
    if plain.contains('*') {
        7
    } else if plain.contains('%') {
        2
    } else if lower.contains("e+") || lower.contains("e-") {
        4
    } else if plain.contains('/') {
        5
    } else if currency || plain.contains(['$', '\u{20ac}', '\u{a3}', '\u{a5}']) {
        3
    } else if plain.contains('@') {
        6
    } else {
        1
    }
}

fn is_month_fmt(fmt: &str) -> bool {
    let f = fmt.to_ascii_lowercase();
    f == "mmm-yy" || f == "mmm-yyyy"
}

#[derive(Clone, Debug)]
enum Kind {
    Blank,

    Other,

    Num(f64, u8),
    Date { v: f64, time: bool, months: bool },
    Time(f64),
    Text(TextNum),
    List(ListItem),
}

fn same_group(a: &Kind, b: &Kind) -> bool {
    match (a, b) {
        (Kind::Num(..), Kind::Num(..)) => true,
        (Kind::Date { .. }, Kind::Date { .. }) => true,
        (Kind::Time(_), Kind::Time(_)) => true,
        (Kind::Text(x), Kind::Text(y)) => {
            x.prefix == y.prefix && x.suffix == y.suffix && x.ordinal == y.ordinal
        }
        (Kind::List(x), Kind::List(y)) => {
            x.cands.iter().any(|a| y.cands.iter().any(|b| a.0 == b.0))
        }
        _ => false,
    }
}

pub(crate) fn round15(x: f64) -> f64 {
    if !x.is_finite() || x == 0.0 {
        return x;
    }
    format!("{:.14e}", x).parse().unwrap_or(x)
}

pub(crate) struct Fit {
    x0: f64,
    base: f64,
    slope: f64,
}

pub(crate) fn fit(xs: &[f64], ys: &[f64]) -> Fit {
    let (m, b) = crate::functions::statdist::regress::fit_line(xs, ys, false).unwrap_or((0.0, 0.0));
    Fit {
        x0: xs[0],
        base: x87::add(b, x87::mul(m, xs[0])),
        slope: m,
    }
}

impl Fit {
    pub(crate) fn at(&self, x: f64) -> f64 {
        x87::add(self.base, round15(x87::mul(self.slope, x - self.x0)))
    }
}

pub(crate) struct GrowthFit {
    x0: f64,
    base: f64,
    r: f64,
}

pub(crate) fn growth_fit(xs: &[f64], ys: &[f64]) -> GrowthFit {
    let (m, b) = crate::functions::statdist::regress::fit_line(xs, ys, true).unwrap_or((0.0, 0.0));
    let r = x87::exp(m);
    GrowthFit {
        x0: xs[0],
        base: x87::mul(x87::exp(b), powi87(r, xs[0])),
        r,
    }
}

impl GrowthFit {
    pub(crate) fn at(&self, x: f64) -> f64 {
        let n = x - self.x0;
        let p = powi87(self.r, n.abs());

        let q = if n < 0.0 { x87::div(1.0, p) } else { p };
        x87::mul(self.base, round15(q))
    }
}

fn sheet_frame(sign: i64, xs: &[f64], ys: &[f64], x: f64, t: f64) -> (Vec<f64>, Vec<f64>, f64) {
    if sign >= 0 {
        return (xs.to_vec(), ys.to_vec(), x);
    }
    let xs2 = xs.iter().rev().map(|v| t + 1.0 - v).collect();
    let ys2 = ys.iter().rev().copied().collect();
    (xs2, ys2, t + 1.0 - x)
}

pub(crate) fn powi87(r: f64, n: f64) -> f64 {
    let mut e = n as u64;
    let (mut sq, mut acc) = (r, 1.0);
    while e > 0 {
        if e & 1 == 1 {
            acc = x87::mul(acc, sq);
        }
        e >>= 1;
        if e > 0 {
            sq = x87::mul(sq, sq);
        }
    }
    acc
}

#[derive(Clone, Debug)]
enum Act {
    Copy(usize),
    Set(usize, Value),
    FormatOnly(usize),
}

struct Ctx {
    ty: FillType,
    sign: i64,
    len: usize,

    shared: Vec<bool>,
    nlines: usize,
    sys: crate::datetime::DateSystem,
}

struct Coords {
    xs: Vec<f64>,
    period: f64,
}

fn coords(slots: &[usize]) -> Coords {
    let m = slots.len();
    if m == 2 {
        Coords {
            xs: vec![0.0, 1.0],
            period: 2.0,
        }
    } else {
        let s0 = slots[0];
        Coords {
            xs: slots.iter().map(|s| (s - s0) as f64).collect(),
            period: (slots[m - 1] - s0 + 1) as f64,
        }
    }
}

fn linear_in(xs: &[f64], ys: &[f64]) -> bool {
    let n = xs.len();
    let dx = xs[n - 1] - xs[0];
    let dy = ys[n - 1] - ys[0];
    (0..n).all(|k| (ys[k] - ys[0]) * dx == dy * (xs[k] - xs[0]))
}

fn mod_floor(a: i64, m: i64) -> i64 {
    a.rem_euclid(m)
}

fn add_weekdays(sys: crate::datetime::DateSystem, d: i64, n: i64) -> i64 {
    let wd = |x: i64| weekday_from_serial(sys, x);
    let mut cur = d;
    if n >= 0 {
        let mut left = n;
        while left > 0 {
            cur += 1;
            while matches!(wd(cur), 0 | 6) {
                cur += 1;
            }
            left -= 1;
        }
    } else {
        let mut left = -n;
        while left > 0 {
            cur -= 1;
            while matches!(wd(cur), 0 | 6) {
                cur -= 1;
            }
            left -= 1;
        }
    }
    cur
}

pub(crate) fn add_weekdays_fast(sys: crate::datetime::DateSystem, d: i64, n: i64) -> i64 {
    let base = if n >= 0 {
        roll_back_weekend(sys, d)
    } else {
        match weekday_from_serial(sys, d) {
            6 => d + 2,
            0 => d + 1,
            _ => d,
        }
    };
    let weeks = n / 5;
    add_weekdays(sys, base + weeks * 7, n - weeks * 5)
}

fn roll_back_weekend(sys: crate::datetime::DateSystem, d: i64) -> i64 {
    match weekday_from_serial(sys, d) {
        6 => d - 1,
        0 => d - 2,
        _ => d,
    }
}

fn dim(sys: crate::datetime::DateSystem, y: i64, m: u32) -> u32 {
    if sys == crate::datetime::DateSystem::D1900 && y == 1900 && m == 2 {
        29
    } else {
        days_in_month(y, m)
    }
}

fn is_eom(sys: crate::datetime::DateSystem, d: i64) -> bool {
    let (y, m, dd) = ymd_from_serial(sys, d);
    dd == dim(sys, y, m)
}

fn month_index(sys: crate::datetime::DateSystem, d: i64) -> i64 {
    let (y, m, _) = ymd_from_serial(sys, d);
    y * 12 + m as i64 - 1
}

pub(crate) fn date_in_month(sys: crate::datetime::DateSystem, mi: i64, dom: u32, eom: bool) -> i64 {
    let y = mi.div_euclid(12);
    let m = mi.rem_euclid(12) as u32 + 1;
    let dim = dim(sys, y, m);
    let d = if eom { dim } else { dom.min(dim) };
    if sys == crate::datetime::DateSystem::D1900
        && y == 1900
        && ((m == 2 && d == 29) || (m == 3 && d == 0))
    {
        return 60;
    }
    serial_from_ymd(sys, y, m as i64, d as i64)
}

impl Ctx {
    fn increments_single(&self, slot: usize) -> bool {
        !matches!(self.ty, FillType::Default | FillType::Values)
            || ((self.len > 1 || self.nlines > 1)
                && !self.shared.get(slot).copied().unwrap_or(false))
    }
}

fn group_value(ctx: &Ctx, items: &[Kind], slots: &[usize], k: usize, p: i64) -> Option<Value> {
    let m = slots.len();
    let ty = ctx.ty;
    match &items[slots[0]] {
        Kind::Num(..) => {
            let ys: Vec<f64> = slots
                .iter()
                .map(|&s| match items[s] {
                    Kind::Num(v, _) => v,
                    _ => 0.0,
                })
                .collect();
            numeric_value(ctx, slots, &ys, k, p, false)
        }
        Kind::Time(_) => {
            let ys: Vec<f64> = slots
                .iter()
                .map(|&s| match items[s] {
                    Kind::Time(v) => v,
                    _ => 0.0,
                })
                .collect();
            if m == 1 {
                if matches!(
                    ty,
                    FillType::Default | FillType::Series | FillType::Values | FillType::Linear
                ) {
                    return Some(Value::Number(round15(ys[0] + (ctx.sign * p) as f64 / 24.0)));
                }
                return None;
            }
            if ys.iter().all(|v| v.abs() < 1.0) {
                numeric_value(ctx, slots, &ys, k, p, true)
            } else {
                let fr: Vec<f64> = ys.iter().map(|v| v - v.floor()).collect();
                match numeric_value(ctx, slots, &fr, k, p, false)? {
                    Value::Number(n) => Some(Value::Number(round15(ys[0] - fr[0] + n))),
                    v => Some(v),
                }
            }
        }
        Kind::Date { .. } => date_value(ctx, items, slots, k, p),
        Kind::Text(_) => text_value(ctx, items, slots, k, p),
        Kind::List(_) => list_value(ctx, items, slots, k, p),
        _ => None,
    }
}

fn numeric_value(
    ctx: &Ctx,
    slots: &[usize],
    ys: &[f64],
    k: usize,
    p: i64,
    time: bool,
) -> Option<Value> {
    let fin = |v: f64| round15(if time && v < 0.0 { v - v.floor() } else { v });
    let m = slots.len();
    let ty = ctx.ty;
    if matches!(
        ty,
        FillType::Days | FillType::Weekdays | FillType::Months | FillType::Years
    ) {
        return None;
    }
    if m == 1 {
        if ty == FillType::Growth || !ctx.increments_single(slots[0]) {
            return None;
        }
        return Some(Value::Number(round15(ys[0] + (ctx.sign * p) as f64)));
    }
    let c = coords(slots);
    let xs: Vec<f64> = if m == 2 {
        vec![1.0, 2.0]
    } else {
        c.xs.iter().map(|x| x + 1.0).collect()
    };
    let x = xs[k] + c.period * p as f64;
    if ty == FillType::Growth {
        if ys.iter().any(|&y| y <= 0.0) {
            return Some(Value::Number(0.0));
        }
        let (xs, ys, x) = sheet_frame(ctx.sign, &xs, ys, x, xs[m - 1]);
        return Some(Value::Number(round15(growth_fit(&xs, &ys).at(x))));
    }
    if time && !linear_in(&xs, ys) {
        return None;
    }
    if linear_in(&xs, ys) {
        let dx = xs[m - 1] - xs[0];
        let slope = (ys[m - 1] - ys[0]) / dx;

        let t = round15(x87::mul(slope, x - xs[0]));
        return Some(Value::Number(fin(x87::add(ys[0], t))));
    }
    let (xs, ys, x) = sheet_frame(ctx.sign, &xs, ys, x, xs[m - 1]);
    Some(Value::Number(fin(fit(&xs, &ys).at(x))))
}

fn text_value(ctx: &Ctx, items: &[Kind], slots: &[usize], k: usize, p: i64) -> Option<Value> {
    if !matches!(
        ctx.ty,
        FillType::Default | FillType::Series | FillType::Values
    ) {
        return None;
    }
    let m = slots.len();
    let tn: Vec<&TextNum> = slots
        .iter()
        .map(|&s| match &items[s] {
            Kind::Text(t) => t,
            _ => unreachable!(),
        })
        .collect();
    let nums: Vec<i128> = tn.iter().map(|t| t.num as i128).collect();
    let new: i128 = if m == 1 {
        nums[0] + (ctx.sign * p) as i128
    } else {
        let c = coords(slots);
        let xs: Vec<i128> = c.xs.iter().map(|&x| x as i128).collect();
        let dx = xs[m - 1] - xs[0];
        let dn = nums[m - 1] - nums[0];
        if dx == 0 || !(0..m).all(|j| (nums[j] - nums[0]) * dx == dn * (xs[j] - xs[0])) {
            return None;
        }
        let x_new = xs[k] + (c.period as i128) * p as i128;
        let off = dn * x_new;
        if off % dx != 0 {
            return None;
        }
        nums[0] + off / dx
    };
    let n = new.unsigned_abs();
    if n > 999_999_999_999_999 {
        return None;
    }
    let t = tn[0];
    let digits = format!("{:0w$}", n, w = t.width);
    let suffix = if t.ordinal {
        ordinal_suffix(n as u64).to_string()
    } else {
        t.suffix.clone()
    };
    Some(Value::text(format!("{}{}{}", t.prefix, digits, suffix)))
}

fn cand(l: &ListItem, fam: usize) -> Option<(usize, usize, u8)> {
    l.cands.iter().find(|c| c.0 == fam).copied()
}

fn pick_list(items: &[Kind], slots: &[usize]) -> Option<(usize, usize)> {
    let Kind::List(first) = &items[slots[0]] else {
        return None;
    };
    for &(fam, _, _) in &first.cands {
        let mut mask = 0xffu8;
        let mut ok = true;
        for &s in slots {
            match &items[s] {
                Kind::List(l) => match cand(l, fam) {
                    Some(c) => mask &= c.2,
                    None => ok = false,
                },
                _ => ok = false,
            }
        }
        if !ok {
            continue;
        }
        if mask == 0 {
            mask = cand(first, fam)?.2;
        }
        let li = (0..8).find(|b| mask & (1 << b) != 0)?;
        return Some((fam, li));
    }
    None
}

fn list_value(ctx: &Ctx, items: &[Kind], slots: &[usize], k: usize, p: i64) -> Option<Value> {
    let m = slots.len();
    let (fam, li) = pick_list(items, slots)?;
    let family = &FAMILIES[fam];
    let list = family.lists[li];
    let l = list.len() as i64;
    let first = match &items[slots[0]] {
        Kind::List(x) => x.clone(),
        _ => return None,
    };
    let idxs: Vec<i64> = slots
        .iter()
        .map(|&s| match &items[s] {
            Kind::List(x) => cand(x, fam).map_or(0, |c| c.1 as i64),
            _ => 0,
        })
        .collect();
    let ty = ctx.ty;
    let ok = match ty {
        FillType::Default | FillType::Series | FillType::Values => true,
        FillType::Days | FillType::Weekdays => family.kind == 0,
        FillType::Months => family.kind == 1,
        _ => false,
    };
    if !ok {
        return None;
    }
    let out = |i: i64| -> Value {
        let name = styled(list[mod_floor(i, l) as usize], first.style);
        Value::text(format!("{}{}", first.lead, name))
    };
    if ty == FillType::Weekdays {
        let w: Vec<i64> = idxs
            .iter()
            .map(|&i| {
                if i >= 5 {
                    if ctx.sign > 0 {
                        4
                    } else {
                        5
                    }
                } else {
                    i
                }
            })
            .collect();
        let widx = |x: i64| -> i64 { mod_floor(x, 5) };
        if m == 1 {
            return Some(out(widx(w[0] + ctx.sign * p)));
        }
        let c = coords(slots);
        let xs: Vec<i64> = c.xs.iter().map(|&x| x as i64).collect();
        let step = find_step(&xs, &w, 5)?;
        let x_new = xs[k] + c.period as i64 * p;
        return Some(out(widx(w[0] + step * x_new)));
    }
    if m == 1 {
        return Some(out(idxs[0] + ctx.sign * p));
    }
    let c = coords(slots);
    let xs: Vec<i64> = c.xs.iter().map(|&x| x as i64).collect();
    let step = find_step(&xs, &idxs, l)?;
    let x_new = xs[k] + c.period as i64 * p;
    Some(out(idxs[0] + step * x_new))
}

fn find_step(xs: &[i64], vs: &[i64], l: i64) -> Option<i64> {
    (0..l).find(|&s| {
        (0..xs.len()).all(|j| mod_floor(vs[0] + s * (xs[j] - xs[0]), l) == mod_floor(vs[j], l))
    })
}

#[derive(Debug)]
enum DMode {
    Cycle,
    Days { slope: f64 },
    Months { slope: i64, eom: bool, dom: u32 },
    Series { step: i64 },
    ShiftDays(i64),
    ShiftMonths(i64),
}

fn date_value(ctx: &Ctx, items: &[Kind], slots: &[usize], k: usize, p: i64) -> Option<Value> {
    let sys = ctx.sys;
    let m = slots.len();
    let ty = ctx.ty;
    if matches!(ty, FillType::Linear | FillType::Growth) {
        return None;
    }
    let (time, months_fmt) = match &items[slots[0]] {
        Kind::Date { time, months, .. } => (*time, *months),
        _ => return None,
    };
    let raw: Vec<f64> = slots
        .iter()
        .map(|&s| match items[s] {
            Kind::Date { v, .. } => v,
            _ => 0.0,
        })
        .collect();
    let ints: Vec<i64> = raw.iter().map(|v| v.floor() as i64).collect();
    let dv: Vec<f64> = if time {
        raw.clone()
    } else {
        ints.iter().map(|&i| i as f64).collect()
    };
    let ymd: Vec<(i64, u32, u32)> = ints.iter().map(|&i| ymd_from_serial(sys, i)).collect();
    let all_eom = ints.iter().all(|&i| is_eom(sys, i));
    let same_dom = ymd.iter().all(|a| a.2 == ymd[0].2);
    let aligned = all_eom || same_dom;
    let mi: Vec<i64> = ints.iter().map(|&i| month_index(sys, i)).collect();
    let dom0 = ymd[0].2;

    let mode = if m == 1 {
        let step = ctx.sign;
        match ty {
            FillType::Default | FillType::Series | FillType::Values => {
                if months_fmt {
                    DMode::Months {
                        slope: step,
                        eom: all_eom,
                        dom: dom0,
                    }
                } else {
                    DMode::Days { slope: step as f64 }
                }
            }
            FillType::Days => DMode::Days { slope: step as f64 },
            FillType::Months => DMode::Months {
                slope: step,
                eom: all_eom,
                dom: dom0,
            },
            FillType::Years => DMode::Months {
                slope: 12 * step,
                eom: all_eom,
                dom: dom0,
            },
            FillType::Weekdays => DMode::Series { step },
            _ => DMode::Cycle,
        }
    } else {
        let c = coords(slots);
        let xs = &c.xs;
        let days_linear = linear_in(xs, &dv);
        let mif: Vec<f64> = mi.iter().map(|&x| x as f64).collect();
        let months_linear = linear_in(xs, &mif);
        let dx = xs[m - 1] - xs[0];
        let days_slope = (dv[m - 1] - dv[0]) / dx;
        let months_slope = (mi[m - 1] - mi[0]) as f64 / dx;
        let months_mode = |per: i64| -> DMode {
            let s = months_slope;
            if s.fract() != 0.0 || !months_linear {
                return DMode::Cycle;
            }
            if s == 0.0 && matches!(ty, FillType::Months | FillType::Years) {
                return DMode::ShiftMonths(per * ctx.sign);
            }
            DMode::Months {
                slope: s as i64,
                eom: all_eom,
                dom: dom0,
            }
        };
        match ty {
            FillType::Default | FillType::Series | FillType::Values => {
                if aligned && mi[m - 1] != mi[0] {
                    months_mode(1)
                } else if days_linear {
                    DMode::Days { slope: days_slope }
                } else {
                    DMode::Cycle
                }
            }
            FillType::Days => {
                if m >= 3
                    && aligned
                    && mi[m - 1] != mi[0]
                    && months_linear
                    && months_slope.fract() == 0.0
                {
                    months_mode(1)
                } else if days_linear {
                    DMode::Days { slope: days_slope }
                } else {
                    DMode::Cycle
                }
            }
            FillType::Months => {
                if aligned {
                    months_mode(1)
                } else if m >= 3 && !days_linear {
                    DMode::Cycle
                } else {
                    DMode::ShiftMonths(ctx.sign)
                }
            }
            FillType::Years => {
                let aligned_years = aligned && mi.iter().all(|x| (x - mi[0]) % 12 == 0);
                if aligned_years {
                    months_mode(12)
                } else if m >= 3 && !days_linear {
                    DMode::Cycle
                } else {
                    DMode::ShiftMonths(12 * ctx.sign)
                }
            }
            FillType::Weekdays => {
                let consecutive = |d: i64| (0..m - 1).all(|j| ints[j + 1] - ints[j] == d);
                if consecutive(1) {
                    DMode::Series { step: 1 }
                } else if consecutive(-1) {
                    DMode::Series { step: -1 }
                } else if aligned
                    && mi[m - 1] != mi[0]
                    && months_linear
                    && months_slope.fract() == 0.0
                {
                    months_mode(1)
                } else if !days_linear || ints[m - 1] == ints[0] {
                    DMode::Cycle
                } else if (ints[m - 1] - ints[0]).abs() <= 7 {
                    DMode::ShiftDays(7 * ctx.sign)
                } else {
                    DMode::ShiftMonths(ctx.sign)
                }
            }
            _ => DMode::Cycle,
        }
    };

    let (xs0, period): (f64, f64) = if m == 1 {
        (0.0, 1.0)
    } else {
        let c = coords(slots);
        (c.xs[k], c.period)
    };
    let pf = p as f64;
    let x_new = if m == 1 { pf } else { xs0 + period * pf };
    let v = match mode {
        DMode::Cycle => return None,
        DMode::Days { slope } => {
            if m == 1 {
                dv[0] + slope * pf
            } else {
                dv[0] + slope * x_new
            }
        }
        DMode::Months { slope, eom, dom } => {
            let target = mi[0] + slope * (if m == 1 { p } else { x_new as i64 });
            let d = date_in_month(sys, target, dom, eom);
            if ty == FillType::Weekdays {
                roll_back_weekend(sys, d) as f64
            } else {
                d as f64
            }
        }
        DMode::Series { step } => {
            if m == 1 {
                add_weekdays_fast(sys, ints[0], step * p) as f64
            } else {
                if step < 0 {
                    add_weekdays_fast(
                        sys,
                        ints[m - 1],
                        step * (k as i64 + p * m as i64 - (m as i64 - 1)),
                    ) as f64
                } else {
                    add_weekdays_fast(sys, ints[0], step * (k as i64 + p * m as i64)) as f64
                }
            }
        }
        DMode::ShiftDays(n) => roll_back_weekend(sys, ints[k] + n * p) as f64,
        DMode::ShiftMonths(n) => {
            let (y0, m0, d0) = ymd[0];
            let t = (y0 * 12 + m0 as i64 - 1) + n * p;
            let (y, m) = (t.div_euclid(12), t.rem_euclid(12) + 1);
            let first = serial_from_ymd(sys, y, m, d0 as i64);

            let d = if k == 0 && ty != FillType::Weekdays {
                date_in_month(sys, t, d0, false)
            } else {
                first + (ints[k] - ints[0])
            };
            if ty == FillType::Weekdays {
                roll_back_weekend(sys, d) as f64
            } else {
                d as f64
            }
        }
    };
    Some(Value::Number(v))
}

fn interior_gap_inexact(items: &[Kind]) -> bool {
    let mut xs = Vec::new();
    let mut ys = Vec::new();
    for (i, k) in items.iter().enumerate() {
        match k {
            Kind::Num(v, _) => {
                xs.push(i as f64);
                ys.push(*v);
            }
            Kind::Blank => {}
            _ => return false,
        }
    }
    let n = xs.len();
    n >= 3 && (xs[n - 1] - xs[0]) as usize + 1 > n && !linear_in(&xs, &ys)
}

fn plan_line(ctx: &Ctx, items: &[Kind], count: usize) -> Vec<Act> {
    let len = items.len();
    let ty = ctx.ty;
    let all = |f: &dyn Fn(usize) -> Act| (0..count).map(|j| f((len + j) % len)).collect::<Vec<_>>();
    match ty {
        FillType::Copy => return all(&Act::Copy),
        FillType::Formats => return all(&Act::FormatOnly),
        _ => {}
    }

    let trendish = matches!(ty, FillType::Linear | FillType::Growth)
        || (matches!(ty, FillType::Default | FillType::Series | FillType::Values)
            && interior_gap_inexact(items));
    if trendish {
        let nums: Vec<(usize, f64)> = items
            .iter()
            .enumerate()
            .filter_map(|(i, k)| match k {
                Kind::Num(v, _) => Some((i, *v)),
                _ => None,
            })
            .collect();
        let others = items
            .iter()
            .filter(|k| !matches!(k, Kind::Num(..) | Kind::Blank))
            .count();
        if nums.len() >= 2 && others == 0 {
            let xs: Vec<f64> = nums.iter().map(|(i, _)| (*i + 1) as f64).collect();
            let ys: Vec<f64> = nums.iter().map(|(_, v)| *v).collect();
            let growth = ty == FillType::Growth;
            let zero = growth && ys.iter().any(|&y| y <= 0.0);
            let t = len as f64;
            let (fx, fy, _) = sheet_frame(ctx.sign, &xs, &ys, 0.0, t);
            let f = fit(&fx, &fy);
            let g = if growth && !zero {
                Some(growth_fit(&fx, &fy))
            } else {
                None
            };
            return (0..count)
                .map(|j| {
                    let x = (len + j + 1) as f64;
                    let x = if ctx.sign < 0 { t + 1.0 - x } else { x };
                    let v = if zero {
                        0.0
                    } else if let Some(g) = &g {
                        round15(g.at(x))
                    } else {
                        round15(f.at(x))
                    };
                    Act::Set((len + j) % len, Value::Number(v))
                })
                .collect();
        }
    }

    let mut groups: Vec<Vec<usize>> = Vec::new();
    let mut group_of: Vec<Option<(usize, usize)>> = vec![None; len];
    let mut group_class: Vec<Option<u8>> = Vec::new();
    let mut cur: Option<usize> = None;
    for (s, kind) in items.iter().enumerate() {
        match kind {
            Kind::Blank => continue,
            Kind::Other => {
                cur = None;
                continue;
            }
            _ => {}
        }

        let join = cur.is_some_and(|g| match (&items[groups[g][0]], kind) {
            (Kind::Num(..), Kind::Num(_, c)) => {
                let ok = *c == 0 || group_class[g].is_none_or(|gc| gc == *c);
                if ok && *c != 0 && group_class[g].is_none() {
                    group_class[g] = Some(*c);
                }
                ok
            }
            (a, b) => same_group(a, b),
        });
        if !join {
            groups.push(Vec::new());
            group_class.push(match kind {
                Kind::Num(_, c) if *c != 0 => Some(*c),
                _ => None,
            });
            cur = Some(groups.len() - 1);
        }
        let g = cur.unwrap();
        group_of[s] = Some((g, groups[g].len()));
        groups[g].push(s);
    }
    (0..count)
        .map(|j| {
            let q = len + j;
            let (s, p) = (q % len, (q / len) as i64);
            match group_of[s] {
                Some((g, k)) => match group_value(ctx, items, &groups[g], k, p) {
                    Some(v) => Act::Set(s, v),
                    None => Act::Copy(s),
                },
                None => Act::Copy(s),
            }
        })
        .collect()
}

impl Workbook {
    fn fill_kind(&self, addr: CellAddr) -> Kind {
        let Some(sh) = self.sheet(addr.sheet) else {
            return Kind::Blank;
        };
        match sh.cells.get(addr.row, addr.col) {
            None => Kind::Blank,
            Some(Entry::Formula(_)) => Kind::Other,
            Some(Entry::Const(v)) => match v {
                Value::Blank => Kind::Blank,
                Value::Number(n) => {
                    let fmt = self.number_format(addr);
                    match fmt_kind(fmt) {
                        FmtKind::Number => Kind::Num(*n, num_class(fmt)),
                        FmtKind::Time => Kind::Time(*n),
                        FmtKind::Date { time } => Kind::Date {
                            v: *n,
                            time,
                            months: is_month_fmt(fmt),
                        },
                    }
                }
                Value::Text(t) => {
                    if let Some(l) = list_item(t) {
                        Kind::List(l)
                    } else if let Some(tn) = text_num(t) {
                        Kind::Text(tn)
                    } else {
                        Kind::Other
                    }
                }
                _ => Kind::Other,
            },
        }
    }

    pub fn autofill(
        &mut self,
        sheet: SheetId,
        src: Rect,
        dst: Rect,
        ty: FillType,
    ) -> Result<RecalcReport, EditError> {
        self.sheet(sheet).ok_or(EditError::NoSuchSheet)?;
        let bad = || EditError::Compile("cannot fill that range".into());
        let dir = if dst.c0 == src.c0 && dst.c1 == src.c1 {
            if dst.r0 == src.r0 && dst.r1 > src.r1 {
                Dir::Down
            } else if dst.r1 == src.r1 && dst.r0 < src.r0 {
                Dir::Up
            } else {
                return Err(bad());
            }
        } else if dst.r0 == src.r0 && dst.r1 == src.r1 {
            if dst.c0 == src.c0 && dst.c1 > src.c1 {
                Dir::Right
            } else if dst.c1 == src.c1 && dst.c0 < src.c0 {
                Dir::Left
            } else {
                return Err(bad());
            }
        } else {
            return Err(bad());
        };
        crate::structure::range_too_large(&dst)?;
        let (len, count, nlines) = if dir.vertical() {
            (src.rows(), dst.rows() - src.rows(), src.cols())
        } else {
            (src.cols(), dst.cols() - src.cols(), src.rows())
        };
        let dest_part = match dir {
            Dir::Down => Rect::new(src.r1 + 1, dst.c0, dst.r1, dst.c1),
            Dir::Up => Rect::new(dst.r0, dst.c0, src.r0 - 1, dst.c1),
            Dir::Right => Rect::new(dst.r0, src.c1 + 1, dst.r1, dst.c1),
            Dir::Left => Rect::new(dst.r0, dst.c0, dst.r1, src.c0 - 1),
        };
        self.guard_merges_rect(sheet, dst)?;

        {
            let ms: Vec<Rect> = self
                .merges(sheet)
                .into_iter()
                .filter(|m| m.intersects(&src))
                .collect();
            if let Some(first) = ms.first() {
                let (h, w) = (first.rows(), first.cols());
                let area: u64 = ms.iter().map(|m| m.rows() as u64 * m.cols() as u64).sum();
                if ms.iter().any(|m| m.rows() != h || m.cols() != w)
                    || area != src.rows() as u64 * src.cols() as u64
                {
                    return Err(EditError::Compile(
                        "To do this, all the merged cells need to be the same size.".into(),
                    ));
                }
            }
        }
        self.guard_arrays_rect(sheet, dest_part)?;
        self.data_tables_overwritten(sheet, dest_part)?;
        let numeric_at = |l: u32, s: u32| -> bool {
            let (r, c) = match dir {
                Dir::Down => (src.r0 + s, src.c0 + l),
                Dir::Up => (src.r1 - s, src.c0 + l),
                Dir::Right => (src.r0 + l, src.c0 + s),
                Dir::Left => (src.r0 + l, src.c1 - s),
            };
            matches!(
                self.fill_kind(CellAddr::new(sheet, r, c)),
                Kind::Num(..) | Kind::Date { .. } | Kind::Time(_)
            )
        };
        let shared: Vec<bool> = (0..len)
            .map(|s| (0..nlines).filter(|&l| numeric_at(l, s)).count() > 1)
            .collect();
        let ctx = Ctx {
            ty,
            sign: dir.sign(),
            len: len as usize,
            shared,
            nlines: nlines as usize,
            sys: self.settings.date_system,
        };

        let at = |l: u32, s: u32| -> (u32, u32) {
            match dir {
                Dir::Down => (src.r0 + s, src.c0 + l),
                Dir::Up => (src.r1 - s, src.c0 + l),
                Dir::Right => (src.r0 + l, src.c0 + s),
                Dir::Left => (src.r0 + l, src.c1 - s),
            }
        };
        let mut plans: Vec<Vec<Act>> = Vec::new();
        for l in 0..nlines {
            let items: Vec<Kind> = (0..len)
                .map(|s| {
                    let (r, c) = at(l, s);
                    self.fill_kind(CellAddr::new(sheet, r, c))
                })
                .collect();
            plans.push(plan_line(&ctx, &items, count as usize));
        }
        if dir == Dir::Down {
            self.fill_grow_tables(sheet, dest_part, &[dest_part]);
        }
        let mode = self.settings.mode;
        self.settings.mode = CalcMode::Manual;
        let mut report = RecalcReport::default();
        let mut failed = None;
        'outer: for (l, plan) in plans.iter().enumerate() {
            for (j, act) in plan.iter().enumerate() {
                let (slot, _) = match act {
                    Act::Copy(s) | Act::FormatOnly(s) | Act::Set(s, _) => (*s as u32, ()),
                };
                let (sr, sc) = at(l as u32, slot);
                let (dr, dc) = at(l as u32, len + j as u32);
                let to = CellAddr::new(sheet, dr, dc);
                let from = Rect::cell(sr, sc);
                match act {
                    Act::FormatOnly(_) => self.fmt_copy_block(sheet, from, to),
                    Act::Copy(_) | Act::Set(..) => {
                        let old_fmt =
                            (ty == FillType::Values).then(|| self.number_format(to).to_string());
                        match self.copy_range(sheet, from, to) {
                            Ok(r) => report.changed.extend(r.changed),
                            Err(e) => {
                                failed = Some(e);
                                break 'outer;
                            }
                        }
                        if let Act::Set(_, v) = act {
                            self.set_value_exact(to, v.clone());
                        }
                        if let Some(f) = old_fmt {
                            self.set_number_format(sheet, Rect::cell(dr, dc), &f);
                        }
                    }
                }
            }
        }
        self.settings.mode = mode;
        if mode == CalcMode::Automatic {
            let r = calc::recalc(self);
            report.evaluations = r.evaluations;
            report.circular = r.circular;
            report.changed.extend(r.changed);
        }
        let _ = Arc::new(0);
        match failed {
            Some(e) => Err(e),
            None => Ok(calc::finish_report(report)),
        }
    }
}
