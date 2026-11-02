use crate::coerce::{parse_number_text, NumberKind, ParseEnv};
use crate::value::{Value, XlError};

#[derive(Clone, Debug)]
pub enum Entered {
    Formula(String),

    Value(Value),

    Clear,
}

pub fn interpret(s: &str, env: &ParseEnv) -> Entered {
    if s.is_empty() {
        return Entered::Clear;
    }
    if let Some(rest) = s.strip_prefix('=') {
        if !rest.is_empty() {
            return Entered::Formula(rest.trim_end_matches(' ').to_string());
        }
        return Entered::Value(Value::from(s));
    }
    if let Some(rest) = s.strip_prefix('\'') {
        return Entered::Value(Value::from(rest));
    }
    if s.eq_ignore_ascii_case("TRUE") {
        return Entered::Value(Value::Bool(true));
    }
    if s.eq_ignore_ascii_case("FALSE") {
        return Entered::Value(Value::Bool(false));
    }

    if let Some(e) = XlError::parse(s).filter(|e| {
        matches!(
            *e,
            XlError::NULL
                | XlError::DIV0
                | XlError::VALUE
                | XlError::REF
                | XlError::NAME
                | XlError::NUM
                | XlError::NA
                | XlError::GETTING_DATA
        )
    }) {
        return Entered::Value(Value::Error(e));
    }
    if let Some(p) = parse_number_text(s, *env) {
        let when = matches!(
            p.kind,
            NumberKind::Date | NumberKind::DateTime | NumberKind::Time { .. }
        );
        if !(when && s.starts_with(' ')) {
            return Entered::Value(Value::Number(p.value));
        }
    }

    let point_ok = env.locale.decimal_sep == '.' || !s.contains('.');
    if (s.starts_with('+') || s.starts_with('-'))
        && s.len() > 1
        && point_ok
        && crate::parser::parse(s).is_ok()
    {
        return Entered::Formula(s.to_string());
    }
    Entered::Value(Value::from(s))
}

pub fn auto_format(kind: &NumberKind, env: &ParseEnv) -> Option<String> {
    match kind {
        NumberKind::Plain
        | NumberKind::Thousands
        | NumberKind::Scientific
        | NumberKind::Currency
        | NumberKind::Percent => None,
        NumberKind::Date => Some(env.locale.short_date_format.clone()),
        NumberKind::Time { seconds, ampm } => Some(
            match (seconds, ampm) {
                (false, false) => "h:mm",
                (true, false) => "h:mm:ss",
                (false, true) => "h:mm AM/PM",
                (true, true) => "h:mm:ss AM/PM",
            }
            .to_string(),
        ),
        NumberKind::DateTime => Some(format!("{} h:mm", env.locale.short_date_format)),
    }
}

pub(crate) fn auto_close_call(body: &str) -> std::borrow::Cow<'_, str> {
    let t = body.trim_start_matches(['+', '-', '@', ' ']);
    let name_len = t
        .find(|c: char| !(c.is_alphanumeric() || c == '.' || c == '_'))
        .unwrap_or(t.len());
    if name_len == 0 || !t[name_len..].starts_with('(') {
        return body.into();
    }
    let mut quote: Option<char> = None;
    for c in t[name_len + 1..].chars() {
        match (quote, c) {
            (Some(q), c) if c == q => quote = None,
            (Some(_), _) => {}
            (None, '"' | '\'') => quote = Some(c),
            (None, '(' | ')') => return body.into(),
            _ => {}
        }
    }
    if quote.is_some() {
        return body.into();
    }
    format!("{body})").into()
}

pub fn is_overflow_scientific(s: &str, env: &ParseEnv) -> bool {
    let dec = env.locale.decimal_sep;
    let Some(epos) = s.find(['e', 'E']) else {
        return false;
    };
    let thousands = env.locale.thousands_sep;
    let mant: String = s[..epos].chars().filter(|&c| c != thousands).collect();
    let (m, e) = (mant.as_str(), s[epos + 1..].trim_start_matches(['+', '-']));
    let digits = |t: &str| !t.is_empty() && t.chars().all(|c| c.is_ascii_digit());
    let mant_ok = match m.split_once(dec) {
        Some((a, b)) => (digits(a) || a.is_empty()) && (digits(b) || b.is_empty()) && m.len() > 1,
        None => digits(m),
    };

    let grouped = s[..epos].contains(thousands);
    let overflows = || {
        let t = format!("{}e{}", m.replace(dec, "."), &s[epos + 1..]);
        t.parse::<f64>().is_ok_and(f64::is_infinite)
    };
    mant_ok && digits(e) && (!grouped || overflows()) && parse_number_text(s, *env).is_none()
}
