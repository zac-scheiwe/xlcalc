#[inline]
pub(crate) fn deep<R>(f: impl FnOnce() -> R) -> R {
    stacker::maybe_grow(64 * 1024, 1024 * 1024, f)
}

pub mod address;
pub mod autofill;
pub mod calc;
pub mod cellfmt;
pub mod coerce;
pub mod cse;
pub mod datatable;
pub mod datetime;
pub mod eval;
pub mod fillseries;
mod fmtmove;
pub mod formula;
pub mod formula_print;
pub mod functions;
pub mod goalseek;
pub mod graph;
pub mod input;
pub mod legacy;
pub mod locale;
pub mod merge;
pub mod moves;
pub mod number;
pub mod numfmt;
pub mod parser;
pub mod pastespecial;
pub mod sort;
pub mod structure;
pub mod text;
pub mod value;
pub mod workbook;
pub mod x87;
pub mod xmath;

pub use address::{CellAddr, Rect, SheetId};
pub use calc::RecalcReport;
pub use value::{Array, Value, XlError};
pub use workbook::{CalcMode, EditError, Settings, Workbook};

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
