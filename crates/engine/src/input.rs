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
    if let Some(v) = wrapped_exponent(s) {
        return Entered::Value(Value::Number(v));
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

pub fn wrapped_exponent(s: &str) -> Option<f64> {
    let t = s.trim_end_matches(' ');
    let (neg, t) = match t.strip_prefix('-') {
        Some(r) => (true, r),
        None => (false, t),
    };
    let epos = t.find(['e', 'E'])?;
    let (mant, ex) = (&t[..epos], &t[epos + 1..]);
    let ex = ex.strip_prefix('+').unwrap_or(ex);
    if ex.is_empty()
        || !ex.bytes().all(|b| b.is_ascii_digit())
        || ex.trim_start_matches('0') != "2147483647"
    {
        return None;
    }
    let (int, frac) = mant.split_once('.').unwrap_or((mant, ""));
    if int.len() != 1
        || !(b'1'..=b'9').contains(&int.as_bytes()[0])
        || !frac.bytes().all(|b| b.is_ascii_digit())
    {
        return None;
    }
    let v: f64 = mant.parse().ok()?;
    let v = v / 10.0;
    Some(if neg { -v } else { v })
}

pub fn is_overflow_scientific(s: &str, env: &ParseEnv) -> bool {
    let Some(epos) = s.find(['e', 'E']) else {
        return false;
    };
    let after = &s[epos + 1..];
    let (sign, rest) = match after.as_bytes().first() {
        Some(b'+') => ("", &after[1..]),
        Some(b'-') => ("-", &after[1..]),
        _ => ("", after),
    };
    let ndig = rest.bytes().take_while(u8::is_ascii_digit).count();
    if ndig == 0 {
        return false;
    }
    let exp = rest[..ndig].trim_start_matches('0');
    if exp.len() > 10 || exp.parse::<u64>().unwrap_or(0) > i32::MAX as u64 {
        return false;
    }
    let head = s[..epos].trim_start_matches(' ');
    let (head, paren) = match head.strip_prefix('(') {
        Some(h) => (h, true),
        None => (head, false),
    };
    if paren && head.starts_with(['+', '-']) {
        return false;
    }

    parse_number_text(&format!("{head}e1"), *env).is_some()
        && parse_number_text(&format!("{head}e{sign}{exp}"), *env).is_none()
}
