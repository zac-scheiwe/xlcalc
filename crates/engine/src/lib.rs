#[inline]
pub(crate) fn command_stack<R>(f: impl FnOnce() -> R) -> R {
    stacker::maybe_grow(4 * 1024 * 1024, 64 * 1024 * 1024, f)
}

#[inline]
pub(crate) fn deep<R>(f: impl FnOnce() -> R) -> R {
    stacker::maybe_grow(64 * 1024, 1024 * 1024, f)
}

pub(crate) mod address;
pub(crate) mod advfilter;
pub mod atp;
pub(crate) mod autofill;
pub(crate) mod calc;
pub(crate) mod cellfmt;
pub mod cfv;
mod cfv_edit;
pub(crate) mod coerce;
pub(crate) mod colwidth;
mod colwidth_data;
pub mod crt;
pub(crate) mod cse;
pub(crate) mod datatable;
pub(crate) mod datetime;
pub(crate) mod deadref;
pub(crate) mod dedup;
pub mod drawing;
mod drawing_groups;
pub(crate) mod editcell;
pub(crate) mod eval;
pub mod f80;
pub(crate) mod fileform;
pub(crate) mod fillseries;
pub mod findcells;
mod fmtmove;
pub(crate) mod formula;
pub(crate) mod formula_print;
pub(crate) mod functions;
pub(crate) mod goalseek;
pub(crate) mod graph;
pub(crate) mod host;
pub mod hyperlink;
pub(crate) mod input;
pub(crate) mod legacy;
mod legacy_classes;
pub(crate) mod locale;
pub(crate) mod merge;
pub(crate) mod moves;
pub mod notes;
pub(crate) mod number;
pub(crate) mod numfmt;
mod pagebreak;
pub mod parser;
pub(crate) mod pastespecial;
pub mod pivot;
pub(crate) mod replace;
pub(crate) mod rowheight;
mod rowheight_data;
pub mod scenarios;
pub(crate) mod share;
pub mod slicer;
pub mod solver;
pub(crate) mod sort;
pub mod sparkline;
pub(crate) mod structure;
pub mod style;
pub(crate) mod subtotal;
pub(crate) mod text;
mod trace;
pub(crate) mod ttc;
pub(crate) mod value;
pub(crate) mod view;
pub(crate) mod walk;
pub(crate) mod workbook;
pub(crate) mod x87;
pub(crate) mod xmath;

#[doc(hidden)]
pub mod testing;

pub use address::{col_to_letters, letters_to_col, parse_a1_rect, CellAddr, Rect, SheetId};
pub use advfilter::AdvancedFilterSpec;
pub use autofill::FillType;
pub use calc::RecalcReport;
pub use cellfmt::format_code_as_read;
pub use cellfmt::number_format_refused;
pub use colwidth::{
    file_width_to_px, px_to_file_width, px_to_width, width_to_px, STANDARD_PX as STANDARD_COLUMN_PX,
};
pub use datatable::DataTable;
pub use datetime::DateSystem;
pub use dedup::DedupSpec;
pub use eval::lambda::LambdaValue;
pub use eval::Area;
pub use fileform::{file_prefix, FileFormula, FileFormulaKind};
pub use fillseries::{DateUnit, SeriesSpec, SeriesType};
pub use findcells::{EndDir, FindSpec, LookIn, SpecialKind};
pub use functions::names as function_names;
pub use host::{
    Arg, CallContext, Evaluated, FunctionHost, ParamKind, ReadView, RtdSource, RuntimeError,
    Signature,
};
pub use locale::{DateOrder, Locale};
pub use number::format_general;
pub use numfmt::{Color, NumberFormat, Rendered};
pub use parser::{is_valid_name, ParseError};
pub use pastespecial::{PasteOp, PasteSpecial, PasteWhat};
pub use replace::ReplaceSpec;
pub use sort::{SortColor, SortKey, SortSpec};
pub use subtotal::{SubFunc, SubtotalSpec};
pub use text::{collate_compare, collate_compare_match_case};
pub use ttc::TtcSpec;
pub use value::{Array, Value, XlError};
pub mod xlm;
pub use view::{ColView, FilterSpec, FilterView};
pub use workbook::ext::{split_path, ExtLink, ExtName, ExtSheetData, OpenBook};
pub use workbook::tables::Table;
pub use workbook::{
    paste_area, CalcMode, Command, EditError, Entry, FormulaCell, FormulaId, NameDef, Names,
    Notice, Outcome, Selection, Settings, Sheet, SheetKind, SheetState, TrackId, Workbook,
};

#[derive(Clone, Copy, Debug)]
pub struct FunctionInfo {
    pub min_args: u8,
    pub max_args: u8,
    pub volatile: bool,

    pub params: &'static str,
}

pub fn argument_class(upper_name: &str, i: usize) -> Option<u8> {
    let classes = legacy_classes::legacy_classes(upper_name)?;
    Some(legacy_classes::class_at(upper_name, classes, i))
}

pub fn function_info(upper_name: &str) -> Option<FunctionInfo> {
    let def = functions::def(functions::lookup(upper_name)?);
    Some(FunctionInfo {
        min_args: def.min_args,
        max_args: def.max_args,
        volatile: def.volatile,
        params: def.params,
    })
}

pub fn parse_cell_ref(wb: &Workbook, text: &str, default_sheet: SheetId) -> Option<CellAddr> {
    let (sheet, cell) = match text.rfind('!') {
        Some(i) => {
            let name = text[..i].trim_matches('\'').replace("''", "'");
            (wb.sheet_by_name(&name)?.id, &text[i + 1..])
        }
        None => (default_sheet, text),
    };
    let (row, col, _, _) = address::parse_a1_cell(cell.trim())?;
    Some(CellAddr::new(sheet, row, col))
}

pub fn round_worksheet(x: f64, digits: i32) -> f64 {
    functions::math::round_decimal(x, digits, functions::math::RoundMode::HalfAway)
}

pub fn parse_range_ref(
    wb: &Workbook,
    text: &str,
    default_sheet: SheetId,
) -> Option<(SheetId, Rect)> {
    let (sheet, body) = match text.rfind('!') {
        Some(i) => {
            let name = text[..i].trim_matches('\'').replace("''", "'");
            (wb.sheet_by_name(&name)?.id, &text[i + 1..])
        }
        None => (default_sheet, text),
    };
    Some((
        sheet,
        address::parse_a1_rect(&body.trim().replace('$', ""))?,
    ))
}
