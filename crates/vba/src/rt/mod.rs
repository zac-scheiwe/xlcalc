mod conv;
mod dates;
mod format;
mod info;
mod maths;
mod strings;
mod util;

use crate::types::VType;
use crate::VbaError;
use xlcalc_variant::{Locale, Variant};

#[derive(Clone, Copy, Debug)]
pub struct RtContext {
    pub compare_text: bool,

    pub locale: Locale,

    pub clock: fn() -> f64,
}

impl Default for RtContext {
    fn default() -> RtContext {
        RtContext {
            compare_text: false,
            locale: Locale::EN_NZ,
            clock: dates::system_clock,
        }
    }
}

#[derive(Debug)]
pub struct Builtin {
    pub name: &'static str,

    pub ret: VType,
    pub min_args: u8,
    pub max_args: u8,
    pub call: fn(&RtContext, &[Variant]) -> Result<Variant, VbaError>,
}

pub fn builtin(name: &str) -> Option<&'static Builtin> {
    BUILTINS.iter().find(|b| b.name.eq_ignore_ascii_case(name))
}

type Call = fn(&RtContext, &[Variant]) -> Result<Variant, VbaError>;

const fn b(name: &'static str, ret: VType, min_args: u8, max_args: u8, call: Call) -> Builtin {
    Builtin {
        name,
        ret,
        min_args,
        max_args,
        call,
    }
}

static BUILTINS: &[Builtin] = &[
    b("CStr", VType::String, 1, 1, conv::cstr),
    b("CDbl", VType::Double, 1, 1, conv::cdbl),
    b("CLng", VType::Long, 1, 1, conv::clng),
    b("CInt", VType::Integer, 1, 1, conv::cint),
    b("CByte", VType::Byte, 1, 1, conv::cbyte),
    b("CBool", VType::Boolean, 1, 1, conv::cbool),
    b("CCur", VType::Currency, 1, 1, conv::ccur),
    b("CDate", VType::Date, 1, 1, conv::cdate),
    b("CDec", VType::Variant, 1, 1, conv::cdec),
    b("CSng", VType::Single, 1, 1, conv::csng),
    b("CVar", VType::Variant, 1, 1, conv::cvar),
    b("CVErr", VType::Variant, 1, 1, conv::cverr),
    b("Val", VType::Double, 1, 1, conv::val),
    b("Str", VType::Variant, 1, 1, conv::str),
    b("Str$", VType::String, 1, 1, conv::str_d),
    b("Hex", VType::Variant, 1, 1, conv::hex),
    b("Hex$", VType::String, 1, 1, conv::hex_d),
    b("Oct", VType::Variant, 1, 1, conv::oct),
    b("Oct$", VType::String, 1, 1, conv::oct_d),
    b("Fix", VType::Variant, 1, 1, conv::fix),
    b("Int", VType::Variant, 1, 1, conv::int),
    b("Abs", VType::Variant, 1, 1, conv::abs),
    b("Sgn", VType::Integer, 1, 1, conv::sgn),
    b("Round", VType::Variant, 1, 2, conv::round),
    b("Exp", VType::Double, 1, 1, maths::exp),
    b("Log", VType::Double, 1, 1, maths::log),
    b("Sqr", VType::Double, 1, 1, maths::sqr),
    b("Atn", VType::Double, 1, 1, maths::atn),
    b("Sin", VType::Double, 1, 1, maths::sin),
    b("Cos", VType::Double, 1, 1, maths::cos),
    b("Tan", VType::Double, 1, 1, maths::tan),
    b("Len", VType::Long, 1, 1, strings::len),
    b("Left", VType::Variant, 2, 2, strings::left),
    b("Left$", VType::String, 2, 2, strings::left_d),
    b("Right", VType::Variant, 2, 2, strings::right),
    b("Right$", VType::String, 2, 2, strings::right_d),
    b("Mid", VType::Variant, 2, 3, strings::mid),
    b("Mid$", VType::String, 2, 3, strings::mid_d),
    b("InStr", VType::Variant, 2, 4, strings::instr),
    b("InStrRev", VType::Variant, 2, 4, strings::instrrev),
    b("Replace", VType::Variant, 3, 6, strings::replace),
    b("LCase", VType::Variant, 1, 1, strings::lcase),
    b("LCase$", VType::String, 1, 1, strings::lcase_d),
    b("UCase", VType::Variant, 1, 1, strings::ucase),
    b("UCase$", VType::String, 1, 1, strings::ucase_d),
    b("Trim", VType::Variant, 1, 1, strings::trim),
    b("Trim$", VType::String, 1, 1, strings::trim_d),
    b("LTrim", VType::Variant, 1, 1, strings::ltrim),
    b("LTrim$", VType::String, 1, 1, strings::ltrim_d),
    b("RTrim", VType::Variant, 1, 1, strings::rtrim),
    b("RTrim$", VType::String, 1, 1, strings::rtrim_d),
    b("Space", VType::Variant, 1, 1, strings::space),
    b("Space$", VType::String, 1, 1, strings::space),
    b("String", VType::Variant, 2, 2, strings::string),
    b("String$", VType::String, 2, 2, strings::string_d),
    b("StrComp", VType::Variant, 2, 3, strings::strcomp),
    b("StrReverse", VType::Variant, 1, 1, strings::strreverse),
    b("Asc", VType::Integer, 1, 1, strings::asc),
    b("AscW", VType::Variant, 1, 1, strings::ascw),
    b("Chr", VType::Variant, 1, 1, strings::chr),
    b("Chr$", VType::String, 1, 1, strings::chr),
    b("ChrW", VType::Variant, 1, 1, strings::chrw),
    b("ChrW$", VType::String, 1, 1, strings::chrw),
    b("Split", VType::Variant, 1, 4, strings::split),
    b("Join", VType::Variant, 1, 2, strings::join),
    b("IIf", VType::Variant, 3, 3, info::iif),
    b("DoEvents", VType::Integer, 0, 0, info::do_events),
    b("IsNumeric", VType::Boolean, 1, 1, info::is_numeric),
    b("IsDate", VType::Boolean, 1, 1, info::is_date),
    b("IsEmpty", VType::Boolean, 1, 1, info::is_empty),
    b("IsNull", VType::Boolean, 1, 1, info::is_null),
    b("IsError", VType::Boolean, 1, 1, info::is_error),
    b("IsArray", VType::Boolean, 1, 1, info::is_array),
    b("VarType", VType::Long, 1, 1, info::var_type),
    b("TypeName", VType::String, 1, 1, info::type_name),
    b("DateSerial", VType::Date, 3, 3, dates::date_serial),
    b("TimeSerial", VType::Date, 3, 3, dates::time_serial),
    b("DateValue", VType::Date, 1, 1, dates::date_value),
    b("TimeValue", VType::Date, 1, 1, dates::time_value),
    b("Year", VType::Variant, 1, 1, dates::year),
    b("Month", VType::Variant, 1, 1, dates::month),
    b("Day", VType::Variant, 1, 1, dates::day),
    b("Hour", VType::Variant, 1, 1, dates::hour),
    b("Minute", VType::Variant, 1, 1, dates::minute),
    b("Second", VType::Variant, 1, 1, dates::second),
    b("Weekday", VType::Variant, 1, 2, dates::weekday),
    b("DateAdd", VType::Variant, 3, 3, dates::date_add),
    b("DateDiff", VType::Variant, 3, 5, dates::date_diff),
    b("Now", VType::Date, 0, 0, dates::now),
    b("Date", VType::Date, 0, 0, dates::date),
    b("Date$", VType::String, 0, 0, dates::date_text),
    b("Time", VType::Date, 0, 0, dates::time),
    b("Time$", VType::String, 0, 0, dates::time_text),
    b("Timer", VType::Single, 0, 0, dates::timer),
    b("Format", VType::Variant, 1, 4, format::format),
    b("Format$", VType::String, 1, 4, format::format_d),
];

#[cfg(test)]
mod golden;
#[cfg(test)]
mod tests;
