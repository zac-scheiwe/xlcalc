use crate::{CellAddr, RecalcReport, Rect, SheetId, Value, Workbook};

pub const VALUES_SEP: char = crate::view::VALUES_SEP;

pub fn recalc(wb: &mut Workbook) -> RecalcReport {
    crate::calc::recalc(wb)
}

pub fn ops_idle(wb: &Workbook) -> bool {
    wb.ops_idle()
}

pub fn set_fixed_now(wb: &mut Workbook, now: Option<f64>) {
    wb.calc.fixed_now = now;
}

pub fn is_formula_entry(wb: &Workbook, text: &str) -> bool {
    matches!(
        crate::input::interpret(text, &wb.input_env()),
        crate::input::Entered::Formula(_)
    )
}

pub fn sheet_name_text(name: &str) -> String {
    crate::parser::print::sheet_name_text(name)
}

pub fn function_signatures() -> Vec<String> {
    crate::functions::names()
        .into_iter()
        .map(|n| {
            let d = crate::functions::def(crate::functions::lookup(n).unwrap());
            format!("{} {} {} {:?}", n, d.min_args, d.max_args, d.params)
        })
        .collect()
}

pub fn raw_sum(wb: &Workbook, sheet: SheetId, rect: Rect) -> f64 {
    let mut acc = 0.0;
    if let Some(sh) = wb.sheet(sheet) {
        for (_, _, e) in sh.cells.iter_rect(rect) {
            if let crate::Entry::Const(Value::Number(x)) = e {
                acc += x;
            }
        }
    }
    acc
}

pub fn visit_sum(wb: &Workbook, host: CellAddr, sheet: SheetId, rect: Rect) -> f64 {
    let mut acc = 0.0;
    let mut ctx = crate::eval::EvalCtx::new(wb, host);
    let area = crate::eval::Area { sheet, rect };
    ctx.visit_area(&area, |_, _, v| {
        if let Value::Number(x) = v {
            acc += x
        }
    });
    acc
}
