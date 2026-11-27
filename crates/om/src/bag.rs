use std::collections::HashMap;
use std::sync::OnceLock;

use xlcalc::drawing::{Bag, BagVal};
use xlcalc_variant::Variant;

use crate::app::{coerce_put, Application};
use crate::conv::Args;
use crate::error::OmError;
use crate::shapes::DrawRef;
use crate::typelib::{InvokeKind, Member, Ty};

type Fail = (i32, Option<String>);

#[derive(Debug, Default)]
pub(crate) struct Spec {
    def: Option<BagVal>,
    nothing: bool,
    object: Option<&'static str>,
    range: Option<(f64, f64, Fail)>,
    get_fail: Option<Fail>,
    put_fail: Option<Fail>,
    ignore: bool,

    unable: Option<&'static str>,

    tri: u8,

    only: Option<(bool, Vec<i32>)>,

    fam_put: Vec<(String, u8)>,

    fam: Vec<(String, Result<BagVal, Fail>)>,
}

fn table_style_show_default(style: &str, member: &str) -> Option<bool> {
    let want = match member {
        "ShowAsAvailableTableStyle" => "TableStyle",
        "ShowAsAvailablePivotTableStyle" => "PivotStyle",
        "ShowAsAvailableSlicerStyle" => "SlicerStyle",
        "ShowAsAvailableTimelineStyle" => "TimeSlicerStyle",
        _ => return None,
    };
    let family = ["TableStyle", "PivotStyle", "SlicerStyle", "TimeSlicerStyle"]
        .into_iter()
        .find(|f| style.starts_with(f))?;
    Some(family == want)
}

pub(crate) fn is_3d_type(t: i32) -> bool {
    matches!(
        t,
        -4100 | 54..=56 | 60..=62 | -4098 | 78 | 79 | -4101 | -4102 | 70 | 83..=86 | 98..=110
    )
}

pub(crate) fn family(t: i32) -> &'static str {
    match t {
        51..=54 | 98..=104 | 92..=97 | 105..=110 => "col",
        57..=62 => "bar",
        4 | 63..=67 => "line",
        5 | 68..=71 | 80 | -4102 | -4120 => "pie",
        -4169 | 72..=75 => "xy",
        15 | 87 | 88 => "bubble",
        1 | 76..=79 => "area",
        -4151 | 81 | 82 => "radar",
        _ => "other",
    }
}

fn chart_view_default(t: i32, member: &str) -> Option<i32> {
    match (t, member) {
        (-4102 | 70, "Elevation") => Some(15),
        (85 | 86, "Elevation") => Some(90),
        (85 | 86, "Perspective" | "Rotation") => Some(0),
        _ => None,
    }
}

fn text_put_rule(member: &str, bv: &BagVal) -> Option<Result<Option<BagVal>, Fail>> {
    let x = match bv {
        BagVal::I(n) => f64::from(*n),
        BagVal::D(x) => *x,
        BagVal::B(b) => -f64::from(u8::from(*b)),

        BagVal::S(_) if member == "Orientation" => {
            return Some(Err((0x8002_0005_u32 as i32, None)));
        }
        BagVal::S(_) if matches!(member, "HorizontalAlignment" | "ReadingOrder") => {
            return Some(Ok(None));
        }
        BagVal::S(_) => return None,
    };
    let n = x.round_ties_even() as i32;
    let e_fail = || Err((0x8000_4005_u32 as i32, None));
    match member {
        "Orientation" => {
            let rot = match n {
                -4128 => 0,
                -4171 => 90,
                -4170 => -90,
                -4166 => 2000,
                n => n,
            };
            if !((-90..=90).contains(&rot) || rot == 2000) {
                return Some(e_fail());
            }
            let xl = match rot {
                0 => -4128,
                90 => -4171,
                -90 => -4170,
                2000 => -4166,
                r => r,
            };
            Some(Ok(Some(BagVal::I(xl))))
        }
        "HorizontalAlignment" => {
            let v = match n {
                -4152 => -4152,
                -4131 | 1 => -4131,
                -4130 => -4130,
                -4117 | 5 => -4117,
                -4108 | 7 => -4108,
                _ => return Some(e_fail()),
            };
            Some(Ok(Some(BagVal::I(v))))
        }
        "ReadingOrder" => match n {
            -5004..=-5002 => Some(Ok(Some(BagVal::I(n)))),
            _ => Some(e_fail()),
        },
        _ => None,
    }
}

fn rotation_is_first_slice(c: &xlcalc::drawing::Chart) -> bool {
    crate::chartparts::chart_group_count(c) == 1
        && c.series
            .iter()
            .all(|s| crate::chart_types::is_pie_kind(s.chart_type))
}

fn rotation_of(orientation: Option<&BagVal>) -> i32 {
    match orientation {
        Some(BagVal::I(-4171)) => 90,
        Some(BagVal::I(-4170)) => -90,
        Some(BagVal::I(-4166)) => 2000,
        Some(BagVal::I(n)) if *n != -4128 => *n,
        _ => 0,
    }
}

fn vertical_alignment_put(
    rotation: i32,
    v: &xlcalc_variant::Variant,
    lcid: u32,
) -> Result<Option<i32>, OmError> {
    if !matches!(rotation, 90 | -90 | 2000) {
        return Ok(None);
    }
    let n = crate::controls::variant_i4(v, lcid)?;
    let first_up = rotation != 90;
    Ok(Some(match n {
        -4160 => {
            if first_up {
                0
            } else {
                2
            }
        }
        -4107 => {
            if first_up {
                2
            } else {
                0
            }
        }
        -4108 => 1,
        -4130 => 3,
        -4117 => 4,
        _ => return Err(OmError::exception(0x8000_4005_u32 as i32, None)),
    }))
}

fn vertical_alignment_get(rotation: i32, align: i32) -> i32 {
    let first_up = match rotation {
        -90 | 2000 => true,
        90 => false,
        _ => return -4108,
    };
    match (align, first_up) {
        (0, true) | (2, false) => -4160,
        (2, true) | (0, false) => -4107,
        _ => -4108,
    }
}

fn not_valid_here() -> Fail {
    (
        0x800A_03EC_u32 as i32,
        Some("This property is not valid for the current chart type.".to_string()),
    )
}

fn parameter_not_valid() -> Fail {
    (
        0x800A_03EC_u32 as i32,
        Some("Parameter not valid".to_string()),
    )
}

fn chart_put_rule(
    t: i32,
    right_angle: bool,
    member: &str,
    bv: &BagVal,
) -> Option<Result<Option<BagVal>, Fail>> {
    let x = match bv {
        BagVal::I(n) => f64::from(*n),
        BagVal::D(x) => *x,
        BagVal::B(b) => f64::from(u8::from(*b)),
        BagVal::S(_) => return None,
    };
    let view = crate::chart_types::is_3d(t);
    let pie_3d = crate::chart_types::is_3d_pie(t);
    let int = |v: f64| Some(Ok(Some(BagVal::I(v as i32))));
    match member {
        "Rotation" => {
            if !(0.0..=360.0).contains(&x) {
                return Some(Err(parameter_not_valid()));
            }
            int(x.trunc())
        }
        "Elevation" => {
            if !view {
                return Some(Err(not_valid_here()));
            }
            if (pie_3d && x < 0.0) || !(-90.0..=90.0).contains(&x) {
                return Some(Err(parameter_not_valid()));
            }
            int(x)
        }
        "Perspective" => {
            if !view {
                return Some(Err(not_valid_here()));
            }
            if right_angle {
                return Some(Ok(None));
            }
            if !(0.0..=200.0).contains(&x) {
                return Some(Err(parameter_not_valid()));
            }
            int(x)
        }
        "RightAngleAxes" => {
            if !view {
                return Some(Err(not_valid_here()));
            }
            Some(Ok(Some(BagVal::B(x != 0.0))))
        }
        "HeightPercent" => {
            if !view {
                return Some(Err(not_valid_here()));
            }
            if !(5.0..=500.0).contains(&x) {
                return Some(Err(parameter_not_valid()));
            }
            int(x)
        }
        "DepthPercent" => {
            if !(20.0..=2000.0).contains(&x) {
                return Some(Err((
                    0x800A_03EC_u32 as i32,
                    Some("Chart depth must be between 1 and 500.".to_string()),
                )));
            }
            int(x)
        }
        "GapDepth" => {
            if crate::chart_types::refuses_gap_depth(t) {
                return Some(Err(not_valid_here()));
            }
            if !view || pie_3d {
                return Some(Ok(None));
            }
            if !(0.0..=500.0).contains(&x) {
                return Some(Err((
                    0x800A_03EC_u32 as i32,
                    Some("Gap depth must be between 0 and 500.".to_string()),
                )));
            }
            int(x)
        }
        "ChartStyle" => {
            let n = x.round_ties_even();
            if (1.0..=48.0).contains(&n) || (201.0..=497.0).contains(&n) {
                int(n)
            } else {
                Some(Err((0x8007_0057_u32 as i32, None)))
            }
        }
        _ => None,
    }
}

fn fam_is(token: &str, fam: &str) -> bool {
    token == fam || (token == "pie" && fam == "doughnut")
}

fn parse_fail(tok: &str) -> Fail {
    let (code, desc) = match tok.split_once('"') {
        Some((c, d)) => (c, Some(d.trim_end_matches('"').to_string())),
        None => (tok, None),
    };
    (
        u32::from_str_radix(code, 16).expect("bag_spec scode") as i32,
        desc,
    )
}

fn tokens(line: &str) -> Vec<String> {
    let mut out = Vec::new();
    let mut cur = String::new();
    let mut quoted = false;
    for ch in line.chars() {
        match ch {
            '"' => {
                quoted = !quoted;
                cur.push(ch);
            }
            ' ' if !quoted => {
                if !cur.is_empty() {
                    out.push(std::mem::take(&mut cur));
                }
            }
            _ => cur.push(ch),
        }
    }
    if !cur.is_empty() {
        out.push(cur);
    }
    out
}

type ShadowPreset = (i32, f64, f64, f64, i32, f64, i32, f32);

const SHADOW_PRESETS: [ShadowPreset; 35] = [
    (9, 5.0, 107763.0, -8100000.0, -2, 100.0, 0, 0.5_f32),
    (10, 5.0, 107763.0, -8100000.0, -2, 100.0, 0, 0.5_f32),
    (11, 5.0, 0.0, 0.0, -2, 100.0, 0, 0.5_f32),
    (12, 5.0, 0.0, 0.0, -2, 100.0, 0, 0.5_f32),
    (13, 5.0, 53882.0, -8100000.0, -2, 100.0, 0, 0.5_f32),
    (14, 5.0, 35921.0, 2700000.0, -2, 100.0, 0, 0.5_f32),
    (15, 5.0, 0.0, 0.0, -2, 100.0, 0, 0.5_f32),
    (16, 5.0, 0.0, 0.0, -2, 100.0, 0, 0.5_f32),
    (17, 5.0, 17961.0, 2700000.0, -2, 100.0, 0, 0.5_f32),
    (18, 5.0, 17961.0, -8100000.0, -2, 100.0, 0, 0.5_f32),
    (19, 5.0, 0.0, 0.0, -2, 100.0, 0, 0.5_f32),
    (20, 5.0, 0.0, 0.0, -2, 100.0, 0, 0.5_f32),
    (21, 4.0, 38100.0, 2700000.0, -1, 100.0, 2, 0.6_f32),
    (22, 4.0, 38100.0, 5400000.0, -1, 100.0, 2, 0.6_f32),
    (23, 4.0, 38100.0, 8100000.0, -1, 100.0, 2, 0.6_f32),
    (24, 4.0, 38100.0, 0.0, -1, 100.0, 2, 0.6_f32),
    (25, 5.0, 0.0, 0.0, -1, 102.0, 2, 0.6_f32),
    (26, 4.0, 38100.0, 10800000.0, -1, 100.0, 2, 0.6_f32),
    (27, 4.0, 38100.0, -2700000.0, -1, 100.0, 2, 0.6_f32),
    (28, 4.0, 38100.0, 16200000.0, -1, 100.0, 2, 0.6_f32),
    (29, 4.0, 38100.0, -8100000.0, -1, 100.0, 2, 0.6_f32),
    (30, 5.0, 50800.0, -8100000.0, -2, 100.0, 1, 0.5_f32),
    (31, 5.0, 50800.0, 16200000.0, -2, 100.0, 1, 0.5_f32),
    (32, 5.0, 50800.0, -2700000.0, -2, 100.0, 1, 0.5_f32),
    (33, 5.0, 50800.0, 10800000.0, -2, 100.0, 1, 0.5_f32),
    (34, 9.0, 0.0, 0.0, -2, 100.0, 1, 0.0_f32),
    (35, 5.0, 50800.0, 0.0, -2, 100.0, 1, 0.5_f32),
    (36, 5.0, 50800.0, 8100000.0, -2, 100.0, 1, 0.5_f32),
    (37, 5.0, 50800.0, 5400000.0, -2, 100.0, 1, 0.5_f32),
    (38, 5.0, 50800.0, 2700000.0, -2, 100.0, 1, 0.5_f32),
    (39, 6.0, 0.0, -8100000.0, -1, 100.0, 2, 0.8_f32),
    (40, 6.0, 0.0, -2700000.0, -1, 100.0, 2, 0.8_f32),
    (41, 12.0, 317500.0, 5400000.0, -1, 90.0, 2, 0.85_f32),
    (42, 6.0, 12700.0, 8100000.0, -1, 100.0, 2, 0.8_f32),
    (43, 6.0, 12700.0, 2700000.0, -1, 100.0, 2, 0.8_f32),
];

fn materialized() -> &'static HashMap<String, Vec<(String, BagVal)>> {
    static T: OnceLock<HashMap<String, Vec<(String, BagVal)>>> = OnceLock::new();
    T.get_or_init(|| {
        let mut map = HashMap::new();
        for line in include_str!("bag_spec.txt").lines() {
            let t = tokens(line.trim());
            if t.len() < 2 || t[1] != "#mat" {
                continue;
            }
            let vals = t[2..]
                .iter()
                .map(|x| {
                    let (k, v) = x.split_once('=').expect("bag_spec #mat");
                    let bv = match v.split_at(1) {
                        ("i", n) => BagVal::I(n.parse().expect("int")),
                        ("d", n) => BagVal::D(n.parse().expect("double")),
                        ("b", n) => BagVal::B(n == "1"),
                        _ => panic!("bag_spec #mat value {v}"),
                    };
                    (k.to_string(), bv)
                })
                .collect();
            map.insert(t[0].clone(), vals);
        }
        map
    })
}

fn table() -> &'static HashMap<(String, String), Spec> {
    static T: OnceLock<HashMap<(String, String), Spec>> = OnceLock::new();
    T.get_or_init(|| {
        let mut map: HashMap<(String, String), Spec> = HashMap::new();
        for line in include_str!("bag_spec.txt").lines() {
            let line = line.trim();
            if line.is_empty() || line.starts_with('#') {
                continue;
            }
            let t = tokens(line);
            if t[1] == "#mat" {
                continue;
            }
            let mut sp = Spec::default();
            let mut range: Option<(f64, f64)> = None;
            let mut rerr: Fail = (0x800A_03EC_u32 as i32, None);
            for tok in &t[2..] {
                let (head, rest) = tok.split_at(1);
                if head == "@" {
                    let (f, v) = rest.split_once(':').expect("bag_spec family");
                    let d = match v.split_at(1) {
                        ("i", n) => Ok(BagVal::I(n.parse().expect("int"))),
                        ("d", n) => Ok(BagVal::D(n.parse().expect("double"))),
                        ("b", n) => Ok(BagVal::B(n == "1")),
                        ("s", n) => Ok(BagVal::S(n.trim_matches('"').to_string())),
                        ("F", n) => Err(parse_fail(n.trim_start_matches('g'))),
                        _ => panic!("bag_spec family token {tok}"),
                    };
                    sp.fam.push((f.to_string(), d));
                    continue;
                }
                match head {
                    "%" => {
                        let (fams, mode) = rest.split_once('/').expect("bag_spec %");
                        for f in fams.split(',') {
                            let mode = match mode {
                                "ignore" => 0,
                                "store" => 1,
                                "store-r" => 2,
                                _ => panic!("bag_spec % mode {mode}"),
                            };
                            sp.fam_put.push((f.to_string(), mode));
                        }
                    }
                    "i" => sp.def = Some(BagVal::I(rest.parse().expect("bag_spec int"))),
                    "d" => sp.def = Some(BagVal::D(rest.parse().expect("bag_spec double"))),
                    "b" => sp.def = Some(BagVal::B(rest == "1")),
                    "s" => sp.def = Some(BagVal::S(rest.trim_matches('"').to_string())),
                    "n" => sp.nothing = true,
                    "o" => sp.object = Some(Box::leak(rest.to_string().into_boxed_str())),
                    "r" => {
                        let (lo, hi) = rest.split_once("..").expect("bag_spec range");
                        range = Some((lo.parse().expect("lo"), hi.parse().expect("hi")));
                    }
                    "!" => rerr = parse_fail(rest),
                    "=" => sp.ignore = true,
                    "S" | "R" => {
                        let list = rest
                            .split(',')
                            .map(|n| n.parse().expect("bag_spec S"))
                            .collect();
                        sp.only = Some((head == "R", list));
                    }
                    "K" => sp.tri = rest.parse().expect("bag_spec K"),
                    "U" => {
                        sp.unable = Some(Box::leak(
                            rest.trim_matches('"').to_string().into_boxed_str(),
                        ));
                    }
                    "F" => {
                        if let Some(r) = rest.strip_prefix('g') {
                            sp.get_fail = Some(parse_fail(r));
                        } else if let Some(r) = rest.strip_prefix('p') {
                            sp.put_fail = Some(parse_fail(r));
                        } else {
                            sp.get_fail = Some(parse_fail(rest));
                            sp.put_fail = sp.get_fail.clone();
                        }
                    }
                    _ => panic!("bag_spec token {tok}"),
                }
            }
            if let Some((lo, hi)) = range {
                sp.range = Some((lo, hi, rerr));
            }
            map.insert((t[0].clone(), t[1].clone()), sp);
        }
        map
    })
}

fn fail(f: &Fail) -> OmError {
    OmError::exception(f.0, f.1.as_deref())
}

fn to_variant(v: &BagVal, ret: Ty) -> Variant {
    match (v, ret) {
        (BagVal::B(b), _) => Variant::Bool(*b),
        (BagVal::I(n), Ty::Bool) => Variant::Bool(*n != 0),
        (BagVal::S(s), _) => Variant::Bstr(s.clone()),
        (BagVal::I(n), Ty::R8) => Variant::R8(f64::from(*n)),
        (BagVal::I(n), Ty::R4) => Variant::R4(*n as f32),
        (BagVal::I(n), _) => Variant::I4(*n),
        (BagVal::D(x), Ty::R4) => Variant::R4(*x as f32),
        (BagVal::D(x), Ty::I4 | Ty::Int | Ty::Enum(_)) => Variant::I4(*x as i32),
        (BagVal::D(x), _) => Variant::R8(*x),
    }
}

fn from_variant(v: &Variant) -> Option<BagVal> {
    Some(match v {
        Variant::Bool(b) => BagVal::B(*b),
        Variant::I4(n) => BagVal::I(*n),
        Variant::R8(x) => BagVal::D(*x),
        Variant::R4(x) => BagVal::D(f64::from(*x)),
        Variant::Bstr(s) => BagVal::S(s.clone()),
        Variant::Empty => BagVal::I(0),
        _ => return None,
    })
}

pub(crate) enum Store<'a> {
    Chart(&'a DrawRef),
    Shape(&'a DrawRef),
    App,
}

impl Application<'_> {
    fn bag_read(&self, st: &Store, name: &str) -> Result<Option<BagVal>, OmError> {
        Ok(match st {
            Store::Chart(r) => self.chart_of(r)?.bag.get(name).cloned(),
            Store::Shape(r) => self.shape_clone(r)?.props.bag.get(name).cloned(),
            Store::App => self.stub_bag.get(name).cloned(),
        })
    }

    fn bag_write(&mut self, st: &Store, f: impl FnOnce(&mut Bag)) -> Result<(), OmError> {
        match st {
            Store::Chart(r) => self.chart_edit(r, |_, c| {
                f(&mut c.bag);
                Ok(())
            }),
            Store::Shape(r) => {
                let wb = &mut self.book_mut(r.book)?.wb;
                if let Some(s) = wb.drawing_mut(r.sheet).get_mut(r.id) {
                    f(&mut s.props.bag);
                }
                Ok(())
            }
            Store::App => {
                f(&mut self.stub_bag);
                Ok(())
            }
        }
    }

    fn shadow_offset_get(&self, st: &Store, slot: &str, x: bool) -> Result<Option<f64>, OmError> {
        let (Some(BagVal::D(dist)), Some(BagVal::D(dir))) = (
            self.bag_read(st, &format!("{slot}.#dist"))?,
            self.bag_read(st, &format!("{slot}.#dir"))?,
        ) else {
            return Ok(None);
        };
        let rad = dir / 60000.0 * std::f64::consts::PI / 180.0;
        let c = if x { rad.cos() } else { rad.sin() };
        Ok(Some(dist * c / 12700.0))
    }

    fn shadow_offset_put(
        &mut self,
        st: &Store,
        slot: &str,
        x: bool,
        bv: &BagVal,
    ) -> Result<(), OmError> {
        let new = match bv {
            BagVal::D(v) => *v,
            BagVal::I(v) => f64::from(*v),
            _ => return Ok(()),
        };
        let cur = |this: &Self, ax: bool| -> Result<f64, OmError> {
            if let Some(v) = this.shadow_offset_get(st, slot, ax)? {
                return Ok(v);
            }
            let n = format!("{slot}.{}", if ax { "OffsetX" } else { "OffsetY" });
            Ok(match this.bag_read(st, &n)? {
                Some(BagVal::D(v)) => v,
                _ => 2.079951763153076,
            })
        };
        let (px, py) = if x {
            (new, cur(self, false)?)
        } else {
            (cur(self, true)?, new)
        };
        let dist = (px.hypot(py) * 12700.0).round();
        let dir = (py.atan2(px) * 180.0 / std::f64::consts::PI * 60000.0).round();
        self.bag_write(st, |b| {
            b.insert(format!("{slot}.#dist"), BagVal::D(dist));
            b.insert(format!("{slot}.#dir"), BagVal::D(dir));
        })
    }

    pub(crate) fn bag_member(
        &mut self,
        st: Store,
        key: &str,
        slot: &str,
        m: &'static Member,
        a: &Args,
    ) -> Option<Result<Variant, OmError>> {
        let sp = table().get(&(key.to_string(), m.name.to_string()))?;
        Some(self.bag_run(&st, key, slot, sp, m, a))
    }

    fn family_of(&self, st: &Store, key: &str, slot: &str) -> Result<&'static str, OmError> {
        Ok(match st {
            Store::Chart(r) => {
                let c = self.chart_of(r)?;

                let own = if key == "Series" {
                    slot.strip_prefix("Series")
                        .and_then(|k| k.parse::<usize>().ok())
                        .and_then(|k| c.series.get(k))
                        .map(|x| x.chart_type)
                } else if key.starts_with("Axis") {
                    Some(crate::chartparts::axis_type(&c))
                } else {
                    None
                };
                let t = own.filter(|t| *t != 0).unwrap_or(c.chart_type);
                if matches!(key, "ChartGroup" | "Series") && matches!(t, -4120 | 80) {
                    "doughnut"
                } else {
                    family(t)
                }
            }
            Store::Shape(r) => match self.shape_clone(r)?.kind {
                xlcalc::drawing::ShapeKind::TextBox => "textbox",
                xlcalc::drawing::ShapeKind::Line => "line",
                xlcalc::drawing::ShapeKind::Chart => "chart",
                _ => "col",
            },
            Store::App => "",
        })
    }

    fn bag_run(
        &mut self,
        st: &Store,
        key: &str,
        slot: &str,
        sp: &Spec,
        m: &'static Member,
        a: &Args,
    ) -> Result<Variant, OmError> {
        let name = format!("{slot}.{}", m.name);
        if m.kind == InvokeKind::Get {
            let fam_ok = !sp.fam.is_empty() && {
                let fam = self.family_of(st, key, slot)?;
                sp.fam.iter().any(|(f, d)| fam_is(f, fam) && d.is_ok())
            };
            if let (Some(f), false) = (&sp.get_fail, fam_ok) {
                return Err(fail(f));
            }
            if matches!(key, "ChartTitle" | "AxisTitle" | "DataLabels")
                && m.name == "VerticalAlignment"
            {
                if let Some(BagVal::I(align)) = self.bag_read(st, &format!("{slot}.#valign"))? {
                    let rot =
                        rotation_of(self.bag_read(st, &format!("{slot}.Orientation"))?.as_ref());
                    return Ok(to_variant(
                        &BagVal::I(vertical_alignment_get(rot, align)),
                        m.ret,
                    ));
                }
            }
            if let (Store::Chart(r), "Chart", "Rotation") = (st, key, m.name) {
                let c = self.chart_of(r)?;
                if rotation_is_first_slice(&c) {
                    let angle = match self.bag_read(st, "ChartGroup1.FirstSliceAngle")? {
                        Some(BagVal::I(n)) => n,
                        _ => 0,
                    };
                    return Ok(to_variant(&BagVal::I(angle), m.ret));
                }
                if self.bag_read(st, &name)?.is_none()
                    && crate::chart_types::is_pie_kind(c.chart_type)
                {
                    return Ok(to_variant(&BagVal::I(20), m.ret));
                }
            }
            if key == "ShadowFormat" && matches!(m.name, "OffsetX" | "OffsetY") {
                if let Some(v) = self.shadow_offset_get(st, slot, m.name == "OffsetX")? {
                    return Ok(to_variant(&BagVal::D(v), m.ret));
                }
            }
            if let Some(v) = self.bag_read(st, &name)? {
                if key == "ShadowFormat" && m.name == "Style" && v == BagVal::I(0) {
                    return Err(OmError::exception(
                        0x8007_0057_u32 as i32,
                        Some("The specified value is out of range."),
                    ));
                }
                return Ok(to_variant(&v, m.ret));
            }
            if key == "TableStyle" {
                if let Some(style) = slot.strip_prefix("TableStyle:") {
                    if let Some(on) = table_style_show_default(style, m.name) {
                        return Ok(Variant::Bool(on));
                    }
                }
            }

            if key == "Axis1" && m.name == "AxisBetweenCategories" {
                if let Some(BagVal::I(n)) = self.bag_read(st, &format!("{slot}.#between"))? {
                    return Ok(Variant::Bool(n != 0));
                }
            }
            if let Store::Chart(r) = st {
                if key == "Chart" {
                    if let Some(n) = chart_view_default(self.chart_of(r)?.chart_type, m.name) {
                        return Ok(to_variant(&BagVal::I(n), m.ret));
                    }
                }
            }
            if !sp.fam.is_empty() {
                let fam = self.family_of(st, key, slot)?;
                if let Some((_, d)) = sp.fam.iter().find(|(f, _)| fam_is(f, fam)) {
                    return match d {
                        Ok(v) => Ok(to_variant(v, m.ret)),
                        Err(f) => Err(fail(f)),
                    };
                }
            }
            if let Some(n) = sp.object {
                return Ok(self.alloc(crate::app::Object::Stub(n)));
            }
            if sp.nothing {
                return Ok(Variant::Dispatch(None));
            }
            return match &sp.def {
                Some(d) => Ok(to_variant(d, m.ret)),
                None => Err(OmError::unsupported(&name)),
            };
        }
        if sp.object.is_some() || sp.nothing {
            return Err(OmError::arg(crate::error::DISP_E_TYPEMISMATCH, 0));
        }
        if matches!(key, "ChartTitle" | "AxisTitle" | "DataLabels") && m.name == "VerticalAlignment"
        {
            let rot = rotation_of(self.bag_read(st, &format!("{slot}.Orientation"))?.as_ref());
            let v = a.put.clone().unwrap_or(Variant::Empty);
            if let Some(align) = vertical_alignment_put(rot, &v, a.lcid)? {
                self.bag_write(st, |b| {
                    b.insert(format!("{slot}.#valign"), BagVal::I(align));
                })?;
            }
            return Ok(Variant::Empty);
        }
        let v = a.put.clone().unwrap_or(Variant::Empty);
        let v = match (coerce_put(m, v, a.lcid), sp.unable) {
            (Err(_), Some(class)) => return Err(OmError::unable_to_set(m.name, class)),
            (r, _) => r?,
        };
        let Some(bv) = from_variant(&v) else {
            return Err(OmError::type_mismatch());
        };

        let bv = match (&sp.def, bv) {
            (Some(BagVal::D(_)), BagVal::I(n)) => BagVal::D(f64::from(n)),
            (Some(BagVal::D(_)), BagVal::B(_))
                if m.params.last().is_some_and(|p| matches!(p.ty, Ty::Variant)) =>
            {
                return Err(OmError::exception(0x8000_4005_u32 as i32, None));
            }
            (_, bv) => bv,
        };
        if matches!(key, "ChartTitle" | "AxisTitle" | "DataLabels") {
            if let Some(rule) = text_put_rule(m.name, &bv) {
                if let Some(v) = rule.map_err(|f| fail(&f))? {
                    self.bag_write(st, |b| {
                        b.insert(name, v);
                    })?;
                }
                return Ok(Variant::Empty);
            }
        }
        if let (Store::Chart(r), "Chart") = (st, key) {
            let c = self.chart_of(r)?;
            let right_angle = matches!(c.bag.get("Chart.RightAngleAxes"), Some(BagVal::B(true)));
            let first_slice = m.name == "Rotation" && rotation_is_first_slice(&c);
            if let Some(rule) = chart_put_rule(c.chart_type, right_angle, m.name, &bv) {
                return match rule.map_err(|f| fail(&f))? {
                    Some(v) if first_slice => {
                        self.bag_write(st, |b| {
                            b.insert(name, v.clone());
                            b.insert("ChartGroup1.FirstSliceAngle".into(), v);
                        })?;
                        Ok(Variant::Empty)
                    }
                    Some(v) => {
                        if let (BagVal::I(n), "ChartStyle") = (&v, m.name) {
                            let n = *n;
                            self.chart_edit(r, |_, ch| {
                                ch.style = n;
                                Ok(())
                            })?;
                        }
                        self.bag_write(st, |b| {
                            b.insert(name, v);
                        })?;
                        Ok(Variant::Empty)
                    }
                    None => Ok(Variant::Empty),
                };
            }
        }

        let exempt = if sp.fam_put.is_empty() {
            None
        } else {
            let fam = self.family_of(st, key, slot)?;
            sp.fam_put.iter().find(|(f, _)| f == fam).map(|(_, md)| *md)
        };
        if exempt == Some(0) {
            return Ok(Variant::Empty);
        }
        let skip_range =
            sp.fam_put.iter().any(|(_, md)| *md == 1) && sp.put_fail.is_some() && exempt.is_none();
        if let (Some((lo, hi, e)), BagVal::I(_) | BagVal::D(_), false) =
            (&sp.range, &bv, skip_range)
        {
            let x = match &bv {
                BagVal::I(n) => f64::from(*n),
                BagVal::D(x) => *x,
                _ => 0.0,
            };
            if x < *lo || x > *hi || x.is_nan() {
                return Err(fail(e));
            }
        }

        if let (Some(f), None) = (&sp.put_fail, exempt) {
            return Err(fail(f));
        }
        let bv = match (&sp.only, &bv) {
            (Some((first, list)), BagVal::I(_) | BagVal::D(_)) => {
                let n = match &bv {
                    BagVal::I(n) => f64::from(*n),
                    BagVal::D(x) => x.round_ties_even(),
                    _ => 0.0,
                } as i32;
                if list.contains(&n) {
                    BagVal::I(n)
                } else if *first {
                    BagVal::I(list[0])
                } else {
                    return Ok(Variant::Empty);
                }
            }
            (Some(_), _) if !matches!(bv, BagVal::B(_)) => return Ok(Variant::Empty),
            _ => bv,
        };

        let bv = match (key, m.name, &bv) {
            ("Series", "BubbleSizes", BagVal::S(s)) => {
                let s = s.trim_start_matches('=');
                BagVal::S(if s.is_empty() { "{1,1,1}" } else { s }.to_string())
            }
            ("Series", "BubbleSizes", BagVal::I(n)) => BagVal::S(format!("{{{n}}}")),
            ("Series", "BubbleSizes", BagVal::D(x)) => BagVal::S(format!("{{{x}}}")),
            _ => bv,
        };
        let bv = match (sp.tri, &bv) {
            (1, BagVal::I(n)) => BagVal::I(if *n != 0 { -1 } else { 0 }),
            (2, BagVal::I(n)) => BagVal::I(if *n == -1 { -1 } else { 0 }),
            _ => bv,
        };
        if key == "ShadowFormat" && m.name == "Type" {
            if let BagVal::I(t) = bv {
                if let Some(p) = SHADOW_PRESETS.iter().find(|p| p.0 == t) {
                    let p = *p;
                    self.bag_write(st, |b| {
                        let mut put = |k: &str, v: BagVal| {
                            b.insert(format!("{slot}.{k}"), v);
                        };
                        put("Type", BagVal::I(t));
                        put("Blur", BagVal::D(p.1));
                        put("#dist", BagVal::D(p.2));
                        put("#dir", BagVal::D(p.3));
                        put("RotateWithShape", BagVal::I(p.4));
                        put("Size", BagVal::D(p.5));
                        put("Style", BagVal::I(p.6));
                        put("Transparency", BagVal::D(f64::from(p.7)));
                        put("Visible", BagVal::I(-1));
                    })?;
                }
            }
            return Ok(Variant::Empty);
        }

        let bv = match (key, m.name, &bv) {
            ("PlotArea", "Position", BagVal::I(n)) if *n != -4105 => BagVal::I(-4114),

            ("Series", "InvertColor", BagVal::I(n)) if *n < 0 => BagVal::I(0),
            _ => bv,
        };

        if key == "Axis1" && m.name == "CategoryType" && !matches!(bv, BagVal::I(2 | 3 | -4105)) {
            return Err(OmError::exception(
                0x800A_03EC_u32 as i32,
                Some("Parameter not valid"),
            ));
        }

        if key == "Axis1"
            && m.name == "BaseUnit"
            && matches!(
                self.bag_read(st, &format!("{slot}.CategoryType"))?,
                Some(BagVal::I(2))
            )
        {
            self.bag_write(st, |b| {
                b.insert(format!("{slot}.CategoryType"), BagVal::I(-4105));
            })?;
        }

        if matches!(key, "Axis1") && m.name == "BaseUnitIsAuto" && bv == BagVal::B(true) {
            return Ok(Variant::Empty);
        }

        let linked: &[(&str, BagVal)] = match (key, m.name) {
            ("Axis1", "BaseUnit") => &[("BaseUnitIsAuto", BagVal::B(false))],

            ("Axis1", "CategoryType") if bv == BagVal::I(3) => {
                &[("BaseUnitIsAuto", BagVal::B(false))]
            }
            ("Axis1", "TickLabelSpacing") => &[("TickLabelSpacingIsAuto", BagVal::B(false))],
            ("Axis1" | "Axis2", "CrossesAt") => &[("Crosses", BagVal::I(-4114))],
            ("Axis1" | "Axis2", "DisplayUnitCustom") => &[("DisplayUnit", BagVal::I(-4114))],
            _ => &[],
        };
        if !linked.is_empty() {
            self.bag_write(st, |b| {
                for (k, v) in linked {
                    b.insert(format!("{slot}.{k}"), v.clone());
                }
            })?;
        }
        if key == "ShadowFormat" && matches!(m.name, "OffsetX" | "OffsetY") {
            self.shadow_offset_put(st, slot, m.name == "OffsetX", &bv)?;
        } else if !sp.ignore {
            self.bag_write(st, |b| {
                b.insert(name, bv);
            })?;
        }

        if let Some(vals) = materialized().get(key) {
            let flag = format!("{slot}.#mat");
            if self.bag_read(st, &flag)?.is_none() {
                self.bag_write(st, |b| {
                    b.insert(flag, BagVal::I(1));
                    for (k, v) in vals {
                        b.entry(format!("{slot}.{k}")).or_insert_with(|| v.clone());
                    }
                })?;
            }
        }
        Ok(Variant::Empty)
    }
}
