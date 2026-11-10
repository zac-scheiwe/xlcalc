use xlcalc::{Arg, CallContext, FunctionHost, ParamKind, RuntimeError, Signature, Value, XlError};
use xlcalc_variant::{change_type, Locale, VarType, Variant};

const CURRENCIES: &[(&str, f64, i32)] = &[
    ("ATS", 13.7603, 2),
    ("BEF", 40.3399, 0),
    ("DEM", 1.95583, 2),
    ("ESP", 166.386, 0),
    ("EUR", 1.0, 2),
    ("FIM", 5.94573, 2),
    ("FRF", 6.55957, 2),
    ("GRD", 340.75, 0),
    ("IEP", 0.787564, 2),
    ("ITL", 1936.27, 0),
    ("LUF", 40.3399, 0),
    ("NLG", 2.20371, 2),
    ("PTE", 200.482, 0),
    ("SIT", 239.64, 2),
    ("CYP", 0.585274, 2),
    ("MTL", 0.4293, 2),
    ("SKK", 30.126, 2),
    ("EEK", 15.6466, 2),
];

pub(crate) struct EuroHost;

fn currency(v: &Value) -> Option<(&'static str, f64, i32)> {
    let Value::Text(t) = v else {
        return None;
    };
    CURRENCIES
        .iter()
        .find(|(code, ..)| code.eq_ignore_ascii_case(t))
        .copied()
}

fn to_double(v: &Value) -> Option<f64> {
    let var = match v {
        Value::Blank => return Some(0.0),
        Value::Number(n) => return Some(*n),
        Value::Bool(b) => Variant::Bool(*b),
        Value::Text(t) => Variant::Bstr(t.to_string()),
        _ => return None,
    };
    match change_type(&var, VarType::R8, Locale::EN_NZ, 0) {
        Ok(Variant::R8(x)) => Some(x),
        _ => None,
    }
}

fn to_bool(v: &Value) -> Option<bool> {
    let var = match v {
        Value::Blank => return Some(false),
        Value::Bool(b) => return Some(*b),
        Value::Number(n) => Variant::R8(*n),
        Value::Text(t) => Variant::Bstr(t.to_string()),
        _ => return None,
    };
    match change_type(&var, VarType::Bool, Locale::EN_NZ, 0) {
        Ok(Variant::Bool(b)) => Some(b),
        _ => None,
    }
}

fn to_int(x: f64) -> i64 {
    x.round_ties_even() as i64
}

fn round_sig15(x: f64) -> f64 {
    format!("{x:.14e}").parse().unwrap_or(x)
}

fn arg_value(a: &Arg) -> Option<&Value> {
    match a {
        Arg::Value(v) => Some(v),
        _ => None,
    }
}

fn convert(args: &[Arg]) -> Option<f64> {
    if !(3..=5).contains(&args.len()) {
        return None;
    }
    let get = |i: usize| args.get(i).and_then(arg_value);
    let number = to_double(get(0)?)?;
    let (_, from_rate, _) = currency(get(1)?)?;
    let (to_code, to_rate, to_decimals) = currency(get(2)?)?;
    let from_code = currency(get(1)?)?.0;
    let full = match get(3) {
        Some(v) => to_bool(v)?,
        None => false,
    };
    let triangulation = match get(4) {
        Some(v) => {
            let digits = to_int(to_double(v)?);
            if digits < 3 {
                return None;
            }
            Some(i32::try_from(digits).ok()?)
        }
        None => None,
    };
    if from_code == to_code {
        return Some(number);
    }
    let mut euro = if from_code == "EUR" {
        number
    } else {
        round_sig15(number / from_rate)
    };
    if let (Some(digits), false) = (triangulation, from_code == "EUR") {
        euro = xlcalc::round_worksheet(euro, digits);
    }
    let result = if to_code == "EUR" {
        euro
    } else {
        euro * to_rate
    };
    Some(if full {
        result
    } else {
        xlcalc::round_worksheet(result, to_decimals)
    })
}

impl FunctionHost for EuroHost {
    fn signature(&self, name: &str) -> Option<Signature> {
        (name == "EUROCONVERT").then(|| Signature {
            params: vec![ParamKind::Value; 5],
            rest: ParamKind::Value,
            volatile: false,
        })
    }

    fn call(
        &self,
        _name: &str,
        args: &[Arg],
        _cx: &mut CallContext<'_>,
    ) -> Result<Value, RuntimeError> {
        Ok(match convert(args) {
            Some(x) if x.is_finite() => Value::Number(x),
            _ => Value::Error(XlError::VALUE),
        })
    }
}
