mod casetable;
mod conv;
mod dates;
mod files;
mod format;
mod info;
mod maths;
mod strings;
mod util;
pub(crate) use strings::map_unit;

pub(crate) fn let_date(locale: Locale, v: &Variant) -> Result<Variant, crate::VbaError> {
    let ctx = RtContext {
        locale,
        ..RtContext::default()
    };
    dates::to_date(&ctx, v).map(Variant::Date)
}
pub use strings::ansi_bytes;
pub use util::host_text;
pub(crate) use util::{bytes_of, from_bytes, join, split_odd};

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
    b("CLngLng", VType::LongLong, 1, 1, conv::clnglng),
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
    b("Abs<typed>", VType::Variant, 1, 1, conv::abs_typed),
    b("Select<test>", VType::Boolean, 1, 1, conv::select_test),
    b("CDate<typed>", VType::Date, 1, 1, conv::cdate_typed),
    b("Rnd", VType::Single, 0, 1, maths::rnd),
    b("Randomize", VType::Variant, 0, 1, maths::randomize),
    b("Error", VType::Variant, 0, 1, info::error_text),
    b("Error$", VType::String, 0, 1, info::error_text),
    b("LenB", VType::Long, 1, 1, strings::lenb),
    b("AscB", VType::Byte, 1, 1, strings::ascb),
    b("ChrB", VType::Variant, 1, 1, strings::chrb),
    b("ChrB$", VType::String, 1, 1, strings::chrb),
    b("MidB", VType::Variant, 2, 3, strings::midb),
    b("MidB$", VType::String, 2, 3, strings::midb),
    b("LeftB", VType::Variant, 2, 2, strings::leftb),
    b("LeftB$", VType::String, 2, 2, strings::leftb),
    b("RightB", VType::Variant, 2, 2, strings::rightb),
    b("RightB$", VType::String, 2, 2, strings::rightb),
    b("InStrB", VType::Variant, 2, 4, strings::instrb),
    b("Filter", VType::Variant, 2, 4, strings::filter),
    b("StrConv", VType::Variant, 2, 3, strings::strconv),
    b("Left", VType::Variant, 2, 2, strings::left),
    b("Left$", VType::String, 2, 2, strings::left_d),
    b("Right", VType::Variant, 2, 2, strings::right),
    b("Right$", VType::String, 2, 2, strings::right_d),
    b("Mid", VType::Variant, 2, 3, strings::mid),
    b("MidAssign", VType::String, 4, 4, strings::mid_assign),
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
    b("Switch", VType::Variant, 2, 255, info::switch),
    b("Choose", VType::Variant, 2, 255, info::choose),
    b("Partition", VType::Variant, 4, 4, info::partition),
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
    b("DatePart", VType::Variant, 2, 4, dates::date_part),
    b("MonthName", VType::String, 1, 2, dates::month_name_fn),
    b("WeekdayName", VType::String, 1, 3, dates::weekday_name),
    b("Now", VType::Date, 0, 0, dates::now),
    b("Date", VType::Date, 0, 0, dates::date),
    b("Date$", VType::String, 0, 0, dates::date_text),
    b("Time", VType::Date, 0, 0, dates::time),
    b("Time$", VType::String, 0, 0, dates::time_text),
    b("Timer", VType::Single, 0, 0, dates::timer),
    b("FileOpen<>", VType::Variant, 6, 6, files::open),
    b("FileClose<>", VType::Variant, 0, 255, files::close),
    b("FilePrint<>", VType::Variant, 1, 255, files::print),
    b("FileWrite<>", VType::Variant, 1, 255, files::write),
    b("LineInput<>", VType::String, 2, 2, files::line_input),
    b("Input", VType::String, 2, 2, files::input_chars),
    b("InputField<>", VType::Variant, 2, 2, files::input_field),
    b("FilePut<>", VType::Variant, 3, 3, files::put),
    b("FileGet<>", VType::Variant, 3, 3, files::get),
    b("FileSeek<>", VType::Variant, 2, 2, files::seek_stmt),
    b("Name<>", VType::Variant, 2, 2, files::name),
    b("EOF", VType::Boolean, 1, 1, files::eof),
    b("LOF", VType::Long, 1, 1, files::lof),
    b("Loc", VType::Long, 1, 1, files::loc),
    b("Seek", VType::Long, 1, 1, files::seek_fn),
    b("FreeFile", VType::Integer, 0, 1, files::freefile),
    b("Kill", VType::Variant, 1, 1, files::kill),
    b("FileCopy", VType::Variant, 2, 2, files::file_copy),
    b("MkDir", VType::Variant, 1, 1, files::mkdir),
    b("RmDir", VType::Variant, 1, 1, files::rmdir),
    b("FileLen", VType::Long, 1, 1, files::file_len),
    b("GetAttr", VType::Integer, 1, 1, files::get_attr),
    b("SetAttr", VType::Variant, 2, 2, files::set_attr),
    b("Dir", VType::String, 0, 2, files::dir),
    b("Dir$", VType::String, 0, 2, files::dir),
    b("CurDir", VType::Variant, 0, 1, files::cur_dir),
    b("CurDir$", VType::String, 0, 1, files::cur_dir),
    b("Environ", VType::Variant, 1, 1, files::environ),
    b("Environ$", VType::String, 1, 1, files::environ),
    b("Format", VType::Variant, 1, 4, format::format),
    b("Format$", VType::String, 1, 4, format::format_d),
    b("FormatNumber", VType::String, 1, 5, format::format_number),
    b("FormatPercent", VType::String, 1, 5, format::format_percent),
    b(
        "FormatCurrency",
        VType::String,
        1,
        5,
        format::format_currency,
    ),
    b(
        "FormatDateTime",
        VType::String,
        1,
        2,
        format::format_date_time,
    ),
];

#[cfg(test)]
mod golden;
#[cfg(test)]
mod tests;
