use xlcalc::{CellAddr, Evaluated, SheetId, Value};
use xlcalc_variant::{SafeArray, VarType, Variant};

use crate::app::{Application, BookId, Mode};
use crate::conv::{array_variant, Args};
use crate::error::OmError;
use crate::vba::value_variant;

impl Application<'_> {
    pub(crate) fn evaluate_member(
        &mut self,
        book: Option<BookId>,
        sheet: Option<SheetId>,
        a: &Args,
    ) -> Result<Variant, OmError> {
        let text = a.string(0)?.ok_or_else(OmError::app_defined)?;
        let (book, host) = {
            let b = match book {
                Some(id) => self.book(id)?,
                None => self.active_book()?,
            };
            let active = b.wb.active_sheet();
            let (r, c) = b.wb.active_cell_of(active);
            (b.id, CellAddr::new(sheet.unwrap_or(active), r, c))
        };
        let wrap = {
            let t = text.trim_start_matches('=').trim().to_ascii_uppercase();
            t.starts_with("ROW(") || t.starts_with("COLUMN(")
        };
        let out = self.book(book)?.wb.evaluate_text(&text, host);
        match out {
            Evaluated::Ref(areas) => {
                let Some(first) = areas.first() else {
                    return Ok(Variant::Error(crate::conv::error_scode(
                        xlcalc::XlError::REF,
                    )));
                };
                let rects: Vec<_> = areas
                    .iter()
                    .filter(|x| x.sheet == first.sheet)
                    .map(|x| x.rect)
                    .collect();
                self.new_multi(book, first.sheet, &rects, Mode::Cells)
            }
            Evaluated::Value(Value::Array(arr)) => {
                let cells: Vec<Variant> = arr.data.iter().map(value_variant).collect();
                if arr.rows == 1 {
                    Ok(one_row(cells))
                } else {
                    Ok(array_variant(arr.rows, arr.cols, cells))
                }
            }
            Evaluated::Value(v) if wrap && !matches!(v, Value::Error(_)) => {
                Ok(one_row(vec![value_variant(&v)]))
            }
            Evaluated::Value(v) => Ok(value_variant(&v)),
        }
    }
}

fn one_row(cells: Vec<Variant>) -> Variant {
    Variant::Array(Box::new(SafeArray {
        elem: VarType::Variant,
        bounds: vec![(1, cells.len() as u32)],
        data: cells,
    }))
}
