mod ampm_elapsed;
mod calendars;
mod calendars_cn;
mod calendars_he;
mod codelen;
mod compare;
pub(crate) use compare::{compile_for_compare, Compiled};
mod dbnum;
pub(crate) mod decimal;
mod displayed;
mod fracden;
pub(crate) use displayed::{EditClass, LimPrec};
pub(crate) mod fillfmt;
mod general;
mod locales;
mod locales_more;
mod parse;
mod readback;
mod render;

pub use general::{
    format_general_cell, format_general_cell_by, format_general_width, general_best_fit,
    general_display_flag, TEXT_GENERAL_WIDTH,
};
pub(crate) use readback::{
    designators, digits_with_date, quote_ampm, readback_spelling, spelled_for_parse,
    typed_elapsed_ok, Designators,
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

pub(crate) struct NumberHeader {
    pub(crate) scientific: bool,

    pub(crate) frac_digits: usize,

    pub(crate) grouping: bool,

    pub(crate) scale: i32,
}

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum ElapsedUnit {
    Hours,
    Minutes,
    Seconds,
}

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum StrictDate {
    Plain,

    Elapsed(ElapsedUnit),
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

    pub fn parse_stored(code: &str) -> Result<NumberFormat, XlError> {
        parse::parse_stored(code).ok_or(XlError::VALUE)
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

    pub(crate) fn number_header(&self) -> Option<NumberHeader> {
        match &self.sections.first()?.body {
            parse::Body::Number(l) if l.n_int + l.n_frac > 0 => Some(NumberHeader {
                scientific: false,
                frac_digits: l.n_frac,
                grouping: l.grouping,
                scale: l.scale,
            }),
            parse::Body::Sci(l) if l.n_int + l.n_frac > 0 => Some(NumberHeader {
                scientific: true,
                frac_digits: l.n_frac,
                grouping: l.grouping,
                scale: 0,
            }),
            _ => None,
        }
    }

    pub(crate) fn date_op_classes(&self) -> Option<Vec<u8>> {
        use parse::DateTok as D;
        let section = self.sections.iter().find(|s| match &s.body {
            parse::Body::Literal(items) => !items.is_empty(),
            _ => true,
        })?;
        let parse::Body::Date(layout) = &section.body else {
            return None;
        };
        let marks_at = |k: usize| layout.day_marks.iter().filter(move |&&m| m == k);
        let mut classes = Vec::new();
        for k in 0..=layout.items.len() {
            classes.extend(marks_at(k).map(|_| 13));
            let class = match layout.items.get(k) {
                Some(parse::Item::Date(d)) => match d {
                    D::Day(_) => 13,
                    D::Month(_) => 17,
                    D::Hour(_) | D::Hour12(_) => 21,
                    D::Minute(_) => 23,
                    D::Second(_) => 25,
                    D::Year(_) | D::Buddhist(_) => 27,
                    D::AmPm(_) => 34,
                    D::EYear(_) | D::Era(_) => 53,
                    D::SubSec(_)
                    | D::ElapsedH(_)
                    | D::ElapsedM(_)
                    | D::ElapsedS(_)
                    | D::LeftH(_)
                    | D::LeftM(_)
                    | D::LeftS(_)
                    | D::General(_)
                    | D::AmPmCjk => continue,
                },
                _ => continue,
            };
            classes.push(class);
        }
        classes.truncate(8);
        Some(classes)
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

    pub(crate) fn format_cell_text(&self, s: &str, loc: &Locale) -> Result<Rendered, XlError> {
        render::with_cell_marks(|| render::render_text(self, s, loc))
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

    pub fn strict_date(&self) -> Option<StrictDate> {
        let mut any = false;
        for s in &self.sections {
            let parse::Body::Date(d) = &s.body else {
                continue;
            };
            any = true;
            for it in &d.items {
                match it {
                    parse::Item::Date(parse::DateTok::ElapsedH(_)) => {
                        return Some(StrictDate::Elapsed(ElapsedUnit::Hours))
                    }
                    parse::Item::Date(parse::DateTok::ElapsedM(_)) => {
                        return Some(StrictDate::Elapsed(ElapsedUnit::Minutes))
                    }
                    parse::Item::Date(parse::DateTok::ElapsedS(_)) => {
                        return Some(StrictDate::Elapsed(ElapsedUnit::Seconds))
                    }
                    _ => {}
                }
            }
        }
        any.then_some(StrictDate::Plain)
    }
}

#[cfg(test)]
mod tests;
