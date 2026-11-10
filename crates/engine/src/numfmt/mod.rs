mod ampm_elapsed;
mod calendars;
mod calendars_cn;
mod calendars_he;
mod codelen;
mod dbnum;
pub(crate) mod decimal;
mod displayed;
pub(crate) use displayed::EditClass;
mod general;
mod locales;
mod locales_more;
mod parse;
mod readback;
mod render;

pub use general::{format_general_cell, format_general_width, TEXT_GENERAL_WIDTH};
pub(crate) use readback::{
    digits_with_date, format_key, quote_ampm, readback_spelling, spelled_for_parse,
    typed_elapsed_ok,
};
pub(crate) use render::{FILL_MARK, GENERAL_MARK, SPACE_MARK};

use crate::datetime::DateSystem;
use crate::locale::Locale;
use crate::value::{Value, XlError};

pub fn fixed_text(x: f64, decimals: i32, grouping: bool, loc: &Locale) -> (String, bool) {
    let d = decimal::Dec::from_f64(x).round_to(decimals);
    let mut int = d.int_digits();
    if int.is_empty() {
        int.push(0);
    }
    let mut out = String::with_capacity(int.len() * 4 / 3 + decimals.max(0) as usize + 2);
    for (i, &digit) in int.iter().enumerate() {
        out.push((b'0' + digit) as char);
        let pos = int.len() - 1 - i;
        if grouping && pos > 0 && pos.is_multiple_of(3) {
            out.push(loc.thousands_sep);
        }
    }
    if decimals > 0 {
        out.push(loc.decimal_sep);
        for f in d.frac_digits(decimals as usize) {
            out.push((b'0' + f) as char);
        }
    }
    (out, d.is_zero())
}

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum Color {
    Black,
    Blue,
    Cyan,
    Green,
    Magenta,
    Red,
    White,
    Yellow,

    Indexed(u8),
}

#[derive(Clone, Debug, PartialEq)]
pub struct Rendered {
    pub text: String,

    pub color: Option<Color>,
}

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub(crate) enum Cmp {
    Lt,
    Le,
    Gt,
    Ge,
    Eq,
    Ne,
}

impl Cmp {
    fn test(self, x: f64, b: f64) -> bool {
        match self {
            Cmp::Lt => x < b,
            Cmp::Le => x <= b,
            Cmp::Gt => x > b,
            Cmp::Ge => x >= b,
            Cmp::Eq => x == b,
            Cmp::Ne => x != b,
        }
    }
}

#[derive(Clone, Debug)]
pub struct NumberFormat {
    pub(crate) sections: Vec<parse::Section>,

    pub(crate) text_section: Option<usize>,

    pub(crate) numeric: usize,

    pub(crate) code_lens: Vec<Option<u32>>,
}

impl NumberFormat {
    pub fn parse(code: &str) -> Result<NumberFormat, XlError> {
        parse::parse_format(code).ok_or(XlError::VALUE)
    }

    pub fn general() -> NumberFormat {
        NumberFormat::parse("General").expect("General parses")
    }

    pub fn format(&self, v: &Value, loc: &Locale, ds: DateSystem) -> Result<Rendered, XlError> {
        match v {
            Value::Number(n) => self.format_number(*n, loc, ds),
            Value::Blank => self.format_number(0.0, loc, ds),
            Value::Text(s) => self.format_text(s, loc),
            Value::Bool(b) => self.format_text(if *b { "TRUE" } else { "FALSE" }, loc),
            Value::Error(e) => Err(*e),
            Value::Array(_) => Err(XlError::VALUE),
            Value::Lambda(_) => Err(XlError::CALC),
        }
    }

    pub fn format_number(&self, x: f64, loc: &Locale, ds: DateSystem) -> Result<Rendered, XlError> {
        render::render_number(self, x, loc, ds)
    }

    pub(crate) fn digits_of_tag(&self, text: &str) -> String {
        render::digits_of_tag(self, text)
    }

    pub(crate) fn format_cell_number(
        &self,
        x: f64,
        loc: &Locale,
        ds: DateSystem,
    ) -> Result<Rendered, XlError> {
        render::with_cell_marks(|| render::render_number(self, x, loc, ds))
    }

    pub fn format_vba(&self, x: f64, loc: &Locale) -> Result<Rendered, XlError> {
        render::render_number(self, x, loc, DateSystem::Vba)
    }

    pub fn format_text(&self, s: &str, loc: &Locale) -> Result<Rendered, XlError> {
        render::render_text(self, s, loc)
    }

    pub fn is_date(&self) -> bool {
        self.sections
            .first()
            .is_some_and(|s| matches!(s.body, parse::Body::Date(_)))
    }
}

#[cfg(test)]
mod tests;
