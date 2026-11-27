#[inline]
pub(crate) fn command_stack<R>(f: impl FnOnce() -> R) -> R {
    xlcalc_stack::maybe_grow(4 * 1024 * 1024, 64 * 1024 * 1024, f)
}

#[inline]
pub(crate) fn deep<R>(f: impl FnOnce() -> R) -> R {
    xlcalc_stack::maybe_grow(64 * 1024, 1024 * 1024, f)
}

pub(crate) mod address;
pub(crate) mod adjust;
pub(crate) mod advfilter;
pub mod atp;
pub(crate) mod autofill;
pub(crate) mod calc;
mod carry;
pub(crate) mod cellfmt;
mod cfblock;
mod cfgradient;
pub mod cfv;
mod cfv_edit;
mod cfv_hosed;
pub mod chart_src;
pub(crate) mod coerce;
pub(crate) mod colwidth;
pub mod crt;
pub(crate) mod cse;
pub(crate) mod datatable;
pub(crate) mod datetime;
pub(crate) mod deadref;
pub(crate) mod dedup;
pub mod dfrtree;
pub mod drawing;
mod drawing_groups;
pub mod drawing_text;
pub(crate) mod editcell;
pub(crate) mod eval;
pub mod f80;
pub(crate) mod fileform;
pub(crate) mod fillseries;
pub mod findcells;
mod fmtmove;
pub(crate) mod fontwidth;
pub(crate) mod formula;
pub(crate) mod formula_print;
pub(crate) mod functions;
pub(crate) mod gdi_metrics;
pub(crate) mod goalseek;
pub(crate) mod graph;
pub(crate) mod host;
pub mod hyperlink;
mod ifmt_auto;
mod ifmt_slots;
#[doc(hidden)]
pub mod ifmt_tables;
pub(crate) mod input;
pub(crate) mod legacy;
mod legacy_classes;
mod legacy_ftab;
mod legacy_grbit;
mod legacy_pass;
mod legacy_single;
pub(crate) mod locale;
pub(crate) mod merge;
pub(crate) mod moves;
pub mod notes;
pub(crate) mod number;
pub(crate) mod numfmt;
pub mod objfmla;
mod pagebreak;
pub mod parser;
pub use parser::print::stored_formula_text;
pub(crate) mod pastespecial;
pub mod pivot;
pub mod refit;
pub(crate) mod replace;
pub mod rich;
pub(crate) mod rowheight;
pub mod scenarios;
pub(crate) mod share;
pub mod slicer;
mod slicer_layout;
pub mod solver;
pub(crate) mod sort;
pub mod sparkline;
pub mod spid;
pub(crate) mod structure;
pub mod style;
pub(crate) mod subtotal;
pub(crate) mod text;
mod token_classes;
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
pub use cellfmt::number_format_refused;
pub use cellfmt::{format_code_as_read, format_code_normalized, CfNumberFormat};
pub use colwidth::{
    coldx_to_dx, coldx_to_px, digit_px, file_width_to_px, px_to_coldx, px_to_file_width,
    px_to_width, standard_px_for, width_to_px, STANDARD_PX as STANDARD_COLUMN_PX,
};
pub use datatable::{DataTable, TableInput};
pub use datetime::DateSystem;
pub use dedup::DedupSpec;
pub use eval::lambda::LambdaValue;
pub use eval::Area;
pub use fileform::{file_prefix, FileFormula, FileFormulaKind};
pub use fillseries::{DateUnit, SeriesSpec, SeriesType};
pub use findcells::{EndDir, FindSpec, LookIn, SpecialKind};
pub use formula::name_ch_xls;
pub use functions::arity as function_arity;
pub use functions::names as function_names;
pub use host::{
    Arg, CallContext, Evaluated, FunctionHost, ParamKind, ReadView, RtdSource, RuntimeError,
    Signature,
};
pub use ifmt_slots::{builtin_code_of_file_id, builtin_file_id_of_code, NumberFormatTable};
pub use locale::{DateOrder, Locale};
pub use number::{format_general, format_literal};
pub use numfmt::{Color, ElapsedUnit, NumberFormat, Rendered, StrictDate};
pub use parser::{is_valid_name, name_from_label, ParseError};
pub use pastespecial::{PasteOp, PasteSpecial, PasteWhat};
pub use replace::ReplaceSpec;
pub use rowheight::window_rows;

pub fn window_header_hp(zoom: u32) -> i64 {
    let z = zoom.clamp(10, 400);
    let ppem = ((220 + 5) / 10 * z + 50) / 100;
    let (a, d) =
        gdi_metrics::ascent_descent(&gdi_metrics::tm("Aptos Narrow", false, false, ppem.max(2)));
    i64::from(a + d)
}

pub fn window_rows_with(zoom: f64, hp: &dyn Fn(u32) -> u32) -> u32 {
    let z = i64::from(zoom.round().clamp(10.0, 400.0) as u32);
    let avail = 1376_i64;
    let mut acc = window_header_hp(z as u32);
    let mut r = 0_u32;
    while r < 1_048_576 {
        let u = (i64::from(hp(r)) * z * 2 + 100) / 200;
        if acc + u > avail {
            return r + 1;
        }
        acc += u;
        r += 1;
        if acc == avail {
            return r;
        }
    }
    1_048_576
}
pub use sort::{SortColor, SortKey, SortSpec};
pub use subtotal::{SubFunc, SubtotalSpec};
pub use text::wide;
pub use text::{collate_compare, collate_compare_match_case, name_key};
pub use token_classes::{token_classes, FormulaContext, TokenClass, TokenClasses};
pub use ttc::TtcSpec;
pub use value::{Array, Value, XlError};
pub mod xlm;
pub use view::filterfile::{Attrs, FileColumn, FileCrit, FileSort, RangeFilterFile};
pub use view::{ColView, ColWidth, FilterSpec, FilterView};
pub use workbook::ext::{split_path, ExtLink, ExtName, ExtSheetData, OpenBook};
pub use workbook::tables::{table_display_name, ColumnId, Table, TableId, TableKey};
pub use workbook::LastFormat;
pub use workbook::{
    paste_area, CalcMode, Command, EditError, Entry, FormulaCell, FormulaId, NameDef, Names,
    Notice, Outcome, Selection, Settings, Sheet, SheetKind, SheetState, TrackId, Workbook,
};

#[derive(Clone, Copy, Debug)]
pub struct FunctionInfo {
    pub min_args: u8,
    pub max_args: u8,

    pub volatile: bool,
}

pub fn function_info(upper_name: &str) -> Option<FunctionInfo> {
    let def = functions::def(functions::lookup(upper_name)?);
    Some(FunctionInfo {
        min_args: def.min_args,
        max_args: def.max_args,
        volatile: def.volatile_record(),
    })
}

pub fn ifunc_name(ifunc: u16) -> Option<&'static str> {
    legacy_ftab::FTAB.get(usize::from(ifunc)).map(|f| f.name)
}

pub fn ifunc_arg_mask(ifunc: u16, arg: usize) -> Option<u32> {
    let m = legacy_ftab::FTAB.get(usize::from(ifunc))?.masks;
    m.get(arg).or_else(|| m.last()).copied()
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
