use xlcalc::{Value, Workbook, XlError};
use xlcalc_variant::{change_type, HResult, Locale, VarType, Variant};

use crate::error::OmError;
use crate::typelib::enumeration;

#[derive(Clone)]
pub(crate) struct Args {
    pub values: Vec<Option<Variant>>,

    pub put: Option<Variant>,
    pub lcid: u32,
}

pub(crate) fn is_missing(v: &Variant) -> bool {
    *v == Variant::Error(HResult::DISP_E_PARAMNOTFOUND.0)
}

pub(crate) const HOST_LCID: u32 = Locale::EN_NZ.0;

pub(crate) fn variant_locale(lcid: u32) -> Locale {
    if lcid == Locale::EN_US.0 || lcid == 0 {
        Locale::EN_US
    } else {
        Locale::EN_NZ
    }
}

pub(crate) fn input_locale(lcid: u32, host: &xlcalc::Locale) -> xlcalc::Locale {
    let mut locale = host.clone();
    if lcid == Locale::EN_US.0 || lcid == 0 {
        locale.date_order = xlcalc::Locale::en_us().date_order;
    }
    locale
}

pub(crate) fn command_number(
    v: Option<&Variant>,
    lcid: u32,
    wb: &Workbook,
) -> Result<Option<f64>, ()> {
    match v {
        None | Some(Variant::Null) => Ok(None),
        Some(v) if is_missing(v) => Ok(None),
        Some(Variant::Empty) => Ok(Some(0.0)),

        Some(Variant::Bool(t)) => Ok(Some(f64::from(u8::from(*t)))),
        Some(Variant::Array(sa)) => match sa.data.first() {
            Some(first) => command_number(Some(first), lcid, wb),
            None => Err(()),
        },
        Some(Variant::Bstr(s)) => wb
            .number_in_text(s, &input_locale(lcid, wb.locale()))
            .map(Some)
            .ok_or(()),
        Some(
            Variant::BstrNull | Variant::Dispatch(_) | Variant::Unknown(_) | Variant::Error(_),
        ) => Err(()),
        Some(v) => match change_type(v, VarType::R8, variant_locale(lcid), 0) {
            Ok(Variant::R8(x)) => Ok(Some(x)),
            _ => Err(()),
        },
    }
}

pub(crate) fn command_int(
    v: Option<&Variant>,
    lcid: u32,
    wb: &Workbook,
) -> Result<Option<i32>, ()> {
    Ok(command_number(v, lcid, wb)?.map(sfix))
}

pub(crate) fn sfix(x: f64) -> i32 {
    (i64::from((x + 2_147_483_648.0) as u32) - 2_147_483_648) as i32
}

impl Args {
    pub fn get(&self, i: usize) -> Option<&Variant> {
        self.values.get(i)?.as_ref()
    }

    fn convert(&self, i: usize, to: VarType) -> Result<Option<Variant>, OmError> {
        let Some(v) = self.get(i) else {
            return Ok(None);
        };
        match change_type(v, to, variant_locale(self.lcid), 0) {
            Ok(v) => Ok(Some(v)),
            Err(_) => Err(OmError::type_mismatch()),
        }
    }

    pub fn i32(&self, i: usize) -> Result<Option<i32>, OmError> {
        Ok(match self.convert(i, VarType::I4)? {
            Some(Variant::I4(n)) => Some(n),
            _ => None,
        })
    }

    pub fn i32_ovf(&self, i: usize) -> Result<Option<i32>, OmError> {
        if let Some(v) = self.get(i) {
            if !matches!(v, Variant::Null | Variant::Empty | Variant::Bool(_)) {
                if let Ok(Variant::R8(x)) =
                    change_type(v, VarType::R8, variant_locale(self.lcid), 0)
                {
                    if !(-2147483648.5..2147483647.5).contains(&x) {
                        return Err(OmError::exception(
                            xlcalc_variant::HResult::DISP_E_OVERFLOW.0,
                            None,
                        ));
                    }
                }
            }
        }
        self.i32(i)
    }

    pub fn bool(&self, i: usize) -> Result<Option<bool>, OmError> {
        Ok(match self.convert(i, VarType::Bool)? {
            Some(Variant::Bool(b)) => Some(b),
            _ => None,
        })
    }

    pub fn f64(&self, i: usize) -> Result<Option<f64>, OmError> {
        Ok(match self.convert(i, VarType::R8)? {
            Some(Variant::R8(x)) => Some(x),
            _ => None,
        })
    }

    pub(crate) fn variant_f64(&self, v: &Variant) -> Result<f64, OmError> {
        match change_type(v, VarType::R8, variant_locale(self.lcid), 0) {
            Ok(Variant::R8(x)) => Ok(x),
            _ => Err(OmError::type_mismatch()),
        }
    }

    pub(crate) fn variant_string(&self, v: &Variant) -> Result<Option<String>, OmError> {
        match change_type(v, VarType::Bstr, variant_locale(self.lcid), 0) {
            Ok(Variant::Bstr(s)) => Ok(Some(s)),
            Ok(_) => Ok(None),
            Err(_) => Err(OmError::type_mismatch()),
        }
    }

    pub fn string(&self, i: usize) -> Result<Option<String>, OmError> {
        Ok(match self.convert(i, VarType::Bstr)? {
            Some(Variant::Bstr(s)) => Some(s),
            _ => None,
        })
    }
}

pub(crate) fn error_scode(e: XlError) -> i32 {
    let name = match e {
        XlError::NULL => "xlErrNull",
        XlError::DIV0 => "xlErrDiv0",
        XlError::VALUE => "xlErrValue",
        XlError::REF => "xlErrRef",
        XlError::NAME => "xlErrName",
        XlError::NUM => "xlErrNum",
        XlError::NA => "xlErrNA",
        XlError::GETTING_DATA => "xlErrGettingData",
        XlError::SPILL => "xlErrSpill",
        XlError::CONNECT => "xlErrConnect",
        XlError::BLOCKED => "xlErrBlocked",
        XlError::FIELD => "xlErrField",
        XlError::CALC => "xlErrCalc",
        _ => "xlErrUnknown",
    };
    let code = enumeration("XlCVError")
        .and_then(|e| e.constants.iter().find(|c| c.0 == name))
        .map_or(2048, |c| c.1);
    crate::error::vba_scode(code)
}

pub fn scode_error(scode: i32) -> Option<XlError> {
    XlError::ALL.into_iter().find(|&e| error_scode(e) == scode)
}

#[derive(Clone, Copy, PartialEq, Eq)]
pub(crate) enum Flavour {
    Value,
    Value2,
}

pub(crate) fn cell_variant(v: &Value, format: &str, flavour: Flavour, d1904: bool) -> Variant {
    match v {
        Value::Blank => Variant::Empty,
        Value::Number(n) => {
            let n = &if *n == 0.0 { 0.0 } else { *n };
            if flavour == Flavour::Value {
                if xlcalc::NumberFormat::parse(format).is_ok_and(|f| f.is_date())
                    && has_date_part(format)
                {
                    return Variant::Date(if d1904 { n + 1462.0 } else { *n });
                }
                if has_bare_dollar(format) {
                    if let Ok(Variant::Cy(c)) =
                        change_type(&Variant::R8(*n), VarType::Cy, Locale::EN_US, 0)
                    {
                        return Variant::Cy(c);
                    }
                }
            }
            Variant::R8(*n)
        }
        Value::Text(t) => Variant::Bstr(t.to_string()),
        Value::Bool(b) => Variant::Bool(*b),
        Value::Error(e) => Variant::Error(error_scode(*e)),
        Value::Array(_) | Value::Lambda(_) => Variant::Error(error_scode(XlError::VALUE)),
    }
}

pub(crate) fn currency_overflow(v: &Value, format: &str) -> bool {
    matches!(v, Value::Number(n) if has_bare_dollar(format)
        && !(xlcalc::NumberFormat::parse(format).is_ok_and(|f| f.is_date()) && has_date_part(format))
        && change_type(&Variant::R8(*n), VarType::Cy, Locale::EN_US, 0).is_err())
}

pub(crate) fn date_overflow(v: &Value, format: &str) -> bool {
    matches!(v, Value::Number(n) if *n >= 2_958_466.0
        && xlcalc::NumberFormat::parse(format).is_ok_and(|f| f.is_date())
        && has_date_part(format))
}

pub(crate) fn array_variant(rows: u32, cols: u32, row_major: Vec<Variant>) -> Variant {
    let mut data = Vec::with_capacity(row_major.len());
    for c in 0..cols as usize {
        for r in 0..rows as usize {
            data.push(row_major[r * cols as usize + c].clone());
        }
    }
    Variant::Array(Box::new(xlcalc_variant::SafeArray {
        elem: VarType::Variant,
        bounds: vec![(1, rows), (1, cols)],
        data,
    }))
}

pub(crate) fn array_cells(v: &Variant) -> Option<(u32, u32, Vec<Variant>)> {
    match v {
        Variant::Array(a) => {
            let (rows, cols) = match a.bounds.as_slice() {
                [(_, r), (_, c)] => (*r, *c),
                [(_, c)] => (1, *c),
                _ => return None,
            };
            let mut out = Vec::with_capacity(a.data.len());
            for r in 0..rows as usize {
                for c in 0..cols as usize {
                    out.push(a.data.get(c * rows as usize + r)?.clone());
                }
            }
            Some((rows, cols, out))
        }
        other => Some((1, 1, vec![other.clone()])),
    }
}

fn code_chars(code: &str) -> Vec<char> {
    let mut out = Vec::new();
    let mut chars = code.chars();
    while let Some(c) = chars.next() {
        match c {
            '"' => {
                for q in chars.by_ref() {
                    if q == '"' {
                        break;
                    }
                }
            }
            '[' => {
                let mut inner = String::new();
                for q in chars.by_ref() {
                    if q == ']' {
                        break;
                    }
                    inner.push(q);
                }
                let lower = inner.to_ascii_lowercase();
                if !lower.is_empty() && lower.chars().all(|c| matches!(c, 'h' | 'm' | 's')) {
                    out.extend(lower.chars());
                }
            }
            '\\' | '_' | '*' => {
                chars.next();
            }
            c => out.push(c),
        }
    }
    out
}

pub(crate) fn has_date_part(code: &str) -> bool {
    let c: Vec<char> = code_chars(
        &code
            .replace("AM/PM", "")
            .replace("am/pm", "")
            .replace("A/P", ""),
    )
    .into_iter()
    .map(|c| c.to_ascii_lowercase())
    .collect();
    c.iter().enumerate().any(|(i, &ch)| match ch {
        'd' | 'y' | 'e' => true,
        'm' => {
            let before = c[..i]
                .iter()
                .rev()
                .find(|&&x| x != ':' && x != ' ' && x != 'm');
            let after = c[i + 1..]
                .iter()
                .find(|&&x| x != ':' && x != ' ' && x != 'm');
            before != Some(&'h') && after != Some(&'s')
        }
        _ => false,
    })
}

pub(crate) fn is_time_only(code: &str) -> bool {
    !has_date_part(code) && xlcalc::NumberFormat::parse(code).is_ok_and(|f| f.is_date())
}

fn has_bare_dollar(code: &str) -> bool {
    code_chars(code).contains(&'$')
}

#[cfg(test)]
mod sfix_tests {
    use super::sfix;

    #[test]
    fn floors_and_saturates() {
        assert_eq!(sfix(-0.5), -1);
        assert_eq!(sfix(-1.5), -2);
        assert_eq!(sfix(2.5), 2);
        assert_eq!(sfix(-1e300), i32::MIN);
        assert_eq!(sfix(1e300), i32::MAX);
    }
}

#[cfg(test)]
mod command_arg_tests {
    use super::command_int;
    use xlcalc::Workbook;
    use xlcalc_variant::Variant;

    #[test]
    fn integers_floor() {
        let wb = Workbook::new();
        let int = |v: Variant| command_int(Some(&v), 0, &wb);
        assert_eq!(int(Variant::R8(-4160.5)), Ok(Some(-4161)));
        assert_eq!(int(Variant::R8(2.9)), Ok(Some(2)));
        assert_eq!(int(Variant::Bool(true)), Ok(Some(1)));
        assert_eq!(int(Variant::Null), Ok(None));
        assert_eq!(int(Variant::Bstr("x".into())), Err(()));
    }
}
