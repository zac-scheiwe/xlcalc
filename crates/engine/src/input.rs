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
            e,
            XlError::Null
                | XlError::Div0
                | XlError::Value
                | XlError::Ref
                | XlError::Name
                | XlError::Num
                | XlError::NA
                | XlError::GettingData
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
    if (s.starts_with('+') || s.starts_with('-')) && s.len() > 1 && crate::parser::parse(s).is_ok()
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
