use xlcalc_variant::{change_type, Locale, SafeArray, VarType, Variant};

use crate::app::{Application, Object};
use crate::conv::{is_missing, variant_locale, Args, Flavour};
use crate::error::{OmError, DISP_E_TYPEMISMATCH};

type Elems = (Vec<Variant>, Vec<(i32, u32)>);

pub(crate) struct AfCall {
    pub field: i32,
    pub c1: Option<String>,
    pub c2: Option<String>,
}

pub(crate) struct AfArgs {
    pub operator: &'static str,
    pub op_code: i32,
    pub rich: bool,
    pub calls: Vec<AfCall>,

    pub shape: Option<Vec<(i32, u32)>>,

    pub values: Option<Vec<String>>,
}

pub(crate) struct AfFail {
    pub err: OmError,
    pub arrows: bool,
}

fn trunc_int(v: &Variant, lcid: u32) -> Option<i32> {
    match v {
        Variant::Bool(_) | Variant::Error(_) | Variant::Empty | Variant::Null => None,
        Variant::I2(_) | Variant::I4(_) | Variant::Ui1(_) | Variant::I8(_) | Variant::Int(_) => {
            let Ok(Variant::R8(x)) = change_type(v, VarType::R8, variant_locale(lcid), 0) else {
                return None;
            };
            to_i32(x)
        }
        Variant::R4(_)
        | Variant::R8(_)
        | Variant::Cy(_)
        | Variant::Date(_)
        | Variant::Decimal(_)
        | Variant::Bstr(_) => {
            let Ok(Variant::R8(x)) = change_type(v, VarType::R8, variant_locale(lcid), 0) else {
                return None;
            };
            to_i32(x)
        }
        _ => None,
    }
}

fn to_i32(x: f64) -> Option<i32> {
    let t = x.trunc();
    (t.abs() < 2_147_483_648.0).then_some(t as i32)
}

fn is_number(v: &Variant) -> bool {
    matches!(
        v,
        Variant::I2(_)
            | Variant::I4(_)
            | Variant::Ui1(_)
            | Variant::I8(_)
            | Variant::Int(_)
            | Variant::R4(_)
            | Variant::R8(_)
            | Variant::Cy(_)
            | Variant::Decimal(_)
    )
}

fn split_sign(s: &str) -> (&str, &str) {
    for sign in ["<=", ">=", "<>", "<", ">", "="] {
        if let Some(rest) = s.strip_prefix(sign) {
            return (sign, rest);
        }
    }
    ("", s)
}

pub(crate) fn clean_criterion(s: &str) -> String {
    let (sign, rest) = split_sign(s.trim_matches(' '));
    let operand: String = rest.chars().take(255).collect();
    let out = format!("{sign}{operand}");

    if out.is_empty() && !s.is_empty() {
        "=".to_string()
    } else {
        out
    }
}

impl Application<'_> {
    fn criterion_text(
        &self,
        v: &Variant,
        lcid: u32,
        in_array: bool,
        arg_index: u32,
    ) -> Result<Option<String>, OmError> {
        let mismatch = || OmError::arg(DISP_E_TYPEMISMATCH, arg_index);
        let locale = if in_array {
            Locale::EN_NZ
        } else {
            variant_locale(lcid)
        };
        Ok(match v {
            Variant::Null => None,
            Variant::Empty | Variant::BstrNull => Some("=".to_string()),
            Variant::Bool(b) => Some(if *b { "TRUE" } else { "FALSE" }.to_string()),
            Variant::Bstr(s) => Some(clean_criterion(s)),
            Variant::R4(x) => Some(xlcalc::format_general(f64::from(*x))),
            Variant::R8(x) => Some(xlcalc::format_general(*x)),
            Variant::I2(_)
            | Variant::I4(_)
            | Variant::Ui1(_)
            | Variant::I8(_)
            | Variant::Int(_) => match change_type(v, VarType::R8, locale, 0) {
                Ok(Variant::R8(x)) => Some(xlcalc::format_general(x)),
                _ => return Err(mismatch()),
            },
            Variant::Decimal(_) => match change_type(v, VarType::R8, locale, 0) {
                Ok(Variant::R8(x)) => Some(xlcalc::format_general(x)),
                _ => return Err(mismatch()),
            },

            Variant::Cy(_) => match change_type(v, VarType::R8, locale, 0) {
                Ok(Variant::R8(x)) => {
                    let fmt = xlcalc::NumberFormat::parse("$#,##0.00_);($#,##0.00)")
                        .map_err(|_| mismatch())?;
                    let shown = fmt
                        .format_number(x, &xlcalc::Locale::en_us(), xlcalc::DateSystem::D1900)
                        .map_err(|_| mismatch())?;
                    Some(clean_criterion(&shown.text))
                }
                _ => return Err(mismatch()),
            },
            Variant::Date(_) => match change_type(v, VarType::Bstr, locale, 0) {
                Ok(Variant::Bstr(s)) => Some(match s.split_once(' ') {
                    Some((d, t)) if s.contains(':') && d.contains('/') => format!("{d}  {t}"),
                    _ => s,
                }),
                _ => return Err(mismatch()),
            },
            _ => return Err(mismatch()),
        })
    }

    fn array_arg(&self, v: &Variant) -> Result<Option<Elems>, OmError> {
        match v {
            Variant::Array(arr) => {
                let mut bounds: Vec<(i32, u32)> = arr.bounds.iter().map(|&(_, n)| (1, n)).collect();

                if bounds.len() == 2 && bounds[0].1 == 1 {
                    bounds.remove(0);
                }
                Ok(Some((arr.data.clone(), bounds)))
            }
            Variant::Dispatch(Some(id)) => {
                let Some(Object::Range(r)) = self.arena.get(*id) else {
                    return Err(OmError::app_defined());
                };
                let (sheet, rect) = self.resolve(r)?;
                let got = self.read_cells(r.book, sheet, rect, |wb, x| {
                    Application::value_variant(wb, x, Flavour::Value)
                })?;
                Ok(match got {
                    Variant::Array(arr) => {
                        let bounds = arr.bounds.clone();
                        Some((arr.data, bounds))
                    }
                    _ => None,
                })
            }
            _ => Ok(None),
        }
    }

    fn scalar_of(&self, v: &Variant) -> Result<Variant, OmError> {
        match v {
            Variant::Dispatch(Some(id)) => {
                let Some(Object::Range(r)) = self.arena.get(*id) else {
                    return Err(OmError::app_defined());
                };
                let (sheet, rect) = self.resolve(r)?;
                match self.read_cells(r.book, sheet, rect, |wb, x| {
                    Application::value_variant(wb, x, Flavour::Value)
                })? {
                    Variant::Array(_) => Err(OmError::app_defined()),
                    other => Ok(other),
                }
            }
            other => Ok(other.clone()),
        }
    }

    fn af_operator(&self, a: &Args) -> Result<Option<i32>, OmError> {
        let first;
        let v = match a.get(2) {
            None => return Ok(None),
            Some(Variant::Null | Variant::Empty) => return Err(OmError::app_defined()),
            Some(Variant::Bool(true)) => return Ok(Some(1)),
            Some(Variant::Array(arr)) => match arr.data.first() {
                Some(e) => {
                    first = e.clone();
                    &first
                }
                None => return Err(OmError::app_defined()),
            },
            Some(v) => v,
        };
        match trunc_int(v, a.lcid) {
            Some(n) => Ok(Some(n)),
            None => a.i32(2),
        }
    }

    fn af_vd_ok(&self, a: &Args) -> bool {
        let vd_ok = |v: &Variant| -> bool {
            match v {
                Variant::Empty | Variant::Bool(_) => true,
                Variant::Bstr(_) => matches!(
                    change_type(v, VarType::Bool, variant_locale(a.lcid), 0),
                    Ok(Variant::Bool(_))
                ),
                x => is_number(x) && !matches!(x, Variant::Cy(_)),
            }
        };
        let Some(v) = a.get(4) else { return true };
        match v {
            Variant::Array(arr) => arr.data.iter().all(vd_ok),
            other => vd_ok(other),
        }
    }

    pub(crate) fn af_args(&self, a: &Args) -> Result<AfArgs, Box<AfFail>> {
        self.af_args_inner(a).map_err(|err| {
            let arrows = err.arg_err.is_none()
                && self.af_vd_ok(a)
                && matches!(self.af_operator(a), Ok(Some(7..=9)));
            Box::new(AfFail { err, arrows })
        })
    }

    fn af_args_inner(&self, a: &Args) -> Result<AfArgs, OmError> {
        let idx = |_pos: usize| 0u32;
        if !self.af_vd_ok(a) {
            return Err(OmError::app_defined());
        }
        let rich = a.get(5).is_some();

        let op_arg = self.af_operator(a)?;
        let op_code = op_arg.unwrap_or(0);
        let operator = match op_arg {
            None | Some(1) => "and",
            Some(2) => "or",
            Some(3) => "top10",
            Some(4) => "bottom10",
            Some(5) => "top10pct",
            Some(6) => "bottom10pct",
            Some(7) => "values",
            Some(8) => "cellcolor",
            Some(9) => "fontcolor",

            Some(10) if a.get(1).is_none() => "icon",
            Some(11) => "dynamic",
            Some(12) => "nofill",
            Some(13) => "autofont",
            Some(_) => return Err(OmError::app_defined()),
        };

        let mut shape: Option<Vec<(i32, u32)>> = None;
        let mut lane = |pos: usize, allow_range: bool| -> Result<Vec<Variant>, OmError> {
            let Some(v) = a.get(pos) else {
                return Ok(Vec::new());
            };
            if !allow_range && matches!(v, Variant::Dispatch(_)) {
                return Err(OmError::app_defined());
            }
            if let Some((items, bounds)) = self.array_arg(v)? {
                if items.is_empty() {
                    return Err(OmError::arg(DISP_E_TYPEMISMATCH, idx(pos)));
                }

                if items.len() > 1 && items.iter().any(|e| matches!(e, Variant::Array(_))) {
                    return Err(OmError::arg(DISP_E_TYPEMISMATCH, idx(pos)));
                }
                shape.get_or_insert(bounds);
                return Ok(items);
            }
            Ok(vec![self.scalar_of(v)?])
        };
        let fields = lane(0, false)?;
        let items1 = lane(1, true)?;
        let items2 = lane(3, true)?;

        for pos in [4, 5] {
            if let Some(Variant::Array(arr)) = a.get(pos) {
                shape.get_or_insert(arr.bounds.iter().map(|&(_, n)| (1, n)).collect());
            }
        }
        let in_array1 = matches!(a.get(1), Some(Variant::Array(_)))
            || items1.len() > 1
            || matches!(a.get(1), Some(Variant::Dispatch(_)));
        let in_array2 = matches!(a.get(3), Some(Variant::Array(_)))
            || items2.len() > 1
            || matches!(a.get(3), Some(Variant::Dispatch(_)));

        let values_list = operator == "values" && matches!(a.get(1), Some(Variant::Array(_)));
        let mut values = None;
        let n = fields
            .len()
            .max(if values_list { 1 } else { items1.len() })
            .max(items2.len())
            .max(1);
        let pick = |v: &[Variant], i: usize| v.get(i % v.len().max(1)).cloned();

        let flat = |v: Variant| -> Result<Variant, OmError> {
            let mut v = v;
            loop {
                match v {
                    Variant::Array(arr) if arr.data.len() == 1 => v = arr.data[0].clone(),
                    Variant::Array(_) => return Err(OmError::arg(DISP_E_TYPEMISMATCH, 0)),
                    other => return Ok(other),
                }
            }
        };
        if values_list {
            let mut list: Vec<String> = Vec::new();

            let width = match a.get(1) {
                Some(Variant::Array(arr)) if arr.bounds.len() == 2 => arr.bounds[1].1 as usize,
                _ => items1.len(),
            };
            for e in items1.iter().take(width) {
                let e = flat(e.clone())?;
                let text = match &e {
                    Variant::Bstr(s) => Some(clean_criterion(s)),

                    Variant::BstrNull => Some("=".to_string()),
                    Variant::Date(_) => self.criterion_text(&e, a.lcid, true, idx(1))?,
                    Variant::Error(_) => return Err(OmError::arg(DISP_E_TYPEMISMATCH, idx(1))),
                    _ => None,
                };
                if let Some(t) = text {
                    if t.starts_with(['<', '>']) {
                        return Err(OmError::app_defined());
                    }
                    if !list.contains(&t) {
                        list.push(t);
                    }
                }
            }
            values = Some(list);
        }
        let mut calls = Vec::with_capacity(n);
        for i in 0..n {
            let field = match pick(&fields, i) {
                Some(f) => {
                    let f = flat(f).map_err(|_| OmError::app_defined())?;
                    match trunc_int(&f, a.lcid) {
                        Some(n) => n,
                        None => {
                            let one = Args {
                                values: vec![Some(f)],
                                put: None,
                                lcid: a.lcid,
                            };
                            one.i32(0)?.unwrap_or(0)
                        }
                    }
                }
                None => 0,
            };
            let c1 = if values_list {
                None
            } else {
                match pick(&items1, i) {
                    None => None,
                    Some(e) => {
                        let e = flat(e)?;
                        if is_missing(&e) {
                            return Err(OmError::arg(DISP_E_TYPEMISMATCH, idx(1)));
                        }
                        self.af_criterion1(&e, operator, a.lcid, in_array1, idx(1))?
                    }
                }
            };
            let c2 = match pick(&items2, i) {
                _ if values_list => None,
                None => None,
                Some(e) => {
                    let e = flat(e)?;
                    if is_missing(&e) {
                        return Err(OmError::arg(DISP_E_TYPEMISMATCH, idx(3)));
                    }
                    self.criterion_text(&e, a.lcid, in_array2, idx(3))?
                }
            };
            calls.push(AfCall { field, c1, c2 });
        }

        if matches!(operator, "top10" | "bottom10" | "top10pct" | "bottom10pct")
            && a.get(1).is_none()
        {
            for c in &mut calls {
                c.c1 = Some("10".to_string());
            }
        }

        if operator == "dynamic" && a.get(1).is_none() {
            return Err(OmError::app_defined());
        }
        if matches!(operator, "cellcolor" | "fontcolor") && a.get(1).is_none() {
            let dflt = if operator == "fontcolor" { "12" } else { "0" };
            for c in &mut calls {
                c.c1 = Some(dflt.to_string());
            }
        }
        Ok(AfArgs {
            operator,
            op_code,
            rich,
            calls,
            shape,
            values,
        })
    }

    fn af_criterion1(
        &self,
        v: &Variant,
        operator: &str,
        lcid: u32,
        in_array: bool,
        arg_index: u32,
    ) -> Result<Option<String>, OmError> {
        match operator {
            "top10" | "bottom10" | "top10pct" | "bottom10pct" => match v {
                Variant::Empty | Variant::Null | Variant::BstrNull => Err(OmError::app_defined()),
                Variant::Bstr(s) => Ok(Some(s.clone())),
                Variant::Bool(_) => Err(OmError::app_defined()),
                other => match trunc_int(other, lcid) {
                    Some(n) => Ok(Some(n.to_string())),
                    None if is_number(other) => Err(OmError::app_defined()),
                    None => Err(OmError::arg(DISP_E_TYPEMISMATCH, arg_index)),
                },
            },
            "cellcolor" | "fontcolor" => match v {
                Variant::Bool(_) | Variant::Empty | Variant::Null | Variant::BstrNull => {
                    Err(OmError::app_defined())
                }
                other => match trunc_int(other, lcid) {
                    Some(n) => Ok(Some((n & 0x00FF_FFFF).to_string())),
                    None => Err(OmError::app_defined()),
                },
            },
            "nofill" | "autofont" => Ok(Some("0".to_string())),

            "dynamic" => match v {
                Variant::I2(_) | Variant::I4(_) => match trunc_int(v, lcid) {
                    Some(n @ 1..=34) => Ok(Some(n.to_string())),
                    _ => Err(OmError::app_defined()),
                },
                _ => Err(OmError::app_defined()),
            },
            _ => self.criterion_text(v, lcid, in_array, arg_index),
        }
    }

    pub(crate) fn af_answer(shape: &Option<Vec<(i32, u32)>>, n: usize) -> Variant {
        match shape {
            Some(bounds) => Variant::Array(Box::new(SafeArray {
                elem: VarType::Variant,
                bounds: bounds.clone(),
                data: vec![Variant::Bool(true); n],
            })),
            None => Variant::Bool(true),
        }
    }
}

#[cfg(test)]
mod tests {
    use super::clean_criterion;

    #[test]
    fn spaces_sign_and_literals() {
        assert_eq!(clean_criterion(" >5"), ">5");
        assert_eq!(clean_criterion("> 5"), "> 5");
        assert_eq!(clean_criterion(" <> x "), "<> x");
        assert_eq!(clean_criterion(">  1/1/2024"), ">  1/1/2024");
        assert_eq!(clean_criterion("=  5"), "=  5");
        assert_eq!(clean_criterion(">5 "), ">5");
        assert_eq!(clean_criterion("   "), "=");
        assert_eq!(clean_criterion(""), "");
        assert_eq!(clean_criterion(">  "), ">");
        assert_eq!(clean_criterion(">TRUE"), ">TRUE");
        assert_eq!(clean_criterion("=TRUE"), "=TRUE");
        assert_eq!(clean_criterion("<>#N/A"), "<>#N/A");
        assert_eq!(clean_criterion("=#N/A"), "=#N/A");
        assert_eq!(clean_criterion(&format!("={}", "x".repeat(300))).len(), 256);
        assert_eq!(clean_criterion(&"x".repeat(300)).len(), 255);
    }
}
