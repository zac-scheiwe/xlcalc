#[inline]
pub(crate) fn deep<R>(f: impl FnOnce() -> R) -> R {
    stacker::maybe_grow(64 * 1024, 1024 * 1024, f)
}

pub(crate) mod address;
pub(crate) mod autofill;
pub(crate) mod calc;
pub(crate) mod cellfmt;
pub(crate) mod coerce;
pub(crate) mod cse;
pub(crate) mod datatable;
pub(crate) mod datetime;
pub(crate) mod dedup;
pub(crate) mod editcell;
pub(crate) mod eval;
pub(crate) mod fillseries;
mod fmtmove;
pub(crate) mod formula;
pub(crate) mod formula_print;
pub(crate) mod functions;
pub(crate) mod goalseek;
pub(crate) mod graph;
pub(crate) mod input;
pub(crate) mod legacy;
mod legacy_classes;
pub(crate) mod locale;
pub(crate) mod merge;
pub(crate) mod moves;
pub(crate) mod number;
pub(crate) mod numfmt;
pub(crate) mod parser;
pub(crate) mod pastespecial;
pub(crate) mod replace;
pub(crate) mod sort;
pub(crate) mod structure;
pub(crate) mod subtotal;
pub(crate) mod text;
pub(crate) mod ttc;
pub(crate) mod value;
pub(crate) mod view;
pub(crate) mod workbook;
pub(crate) mod x87;
pub(crate) mod xmath;

#[doc(hidden)]
pub mod testing;

pub use address::{col_to_letters, letters_to_col, CellAddr, Rect, SheetId};
pub use autofill::FillType;
pub use calc::RecalcReport;
pub use datatable::DataTable;
pub use datetime::DateSystem;
pub use dedup::DedupSpec;
pub use eval::lambda::LambdaValue;
pub use fillseries::{DateUnit, SeriesSpec, SeriesType};
pub use functions::names as function_names;
pub use locale::{DateOrder, Locale};
pub use number::format_general;
pub use numfmt::{Color, NumberFormat, Rendered};
pub use parser::ParseError;
pub use pastespecial::{PasteOp, PasteSpecial, PasteWhat};
pub use replace::ReplaceSpec;
pub use sort::{SortKey, SortSpec};
pub use subtotal::{SubFunc, SubtotalSpec};
pub use ttc::TtcSpec;
pub use value::{Array, Value, XlError};
pub use view::FilterSpec;
pub use workbook::tables::Table;
pub use workbook::{
    CalcMode, EditError, Entry, FormulaCell, FormulaId, NameDef, Names, Settings, Sheet, Workbook,
};

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
