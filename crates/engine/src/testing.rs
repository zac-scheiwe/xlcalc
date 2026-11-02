use crate::{CellAddr, RecalcReport, Rect, SheetId, Value, Workbook};

pub const VALUES_SEP: char = crate::view::VALUES_SEP;

pub fn recalc(wb: &mut Workbook) -> RecalcReport {
    wb.command(crate::Command::Calculate, crate::calc::recalc)
}

pub fn seed_value(wb: &mut Workbook, addr: CellAddr, v: Value) -> bool {
    match wb.formula_at(addr) {
        Some(fid) => {
            wb.formula_mut(fid).value = v;
            true
        }
        None => false,
    }
}

pub fn restore_saved_value(wb: &mut Workbook, addr: CellAddr, v: Value) -> bool {
    match wb.formula_at(addr) {
        Some(fid) if !wb.formula(fid).value.is_array() && !v.is_array() => {
            wb.formula_mut(fid).value = v;
            true
        }
        _ => false,
    }
}

pub fn open_like_excel(
    wb: &mut Workbook,
    saved: &[(CellAddr, Value)],
    blank: &[CellAddr],
    recalc: &[CellAddr],
) {
    wb.command(crate::Command::Calculate, |wb| {
        let volatile: Vec<crate::workbook::FormulaId> = wb
            .calc
            .volatile
            .iter()
            .chain(&wb.calc.host_volatile)
            .copied()
            .collect();
        for fid in volatile {
            crate::calc::mark_dirty(wb, fid);
        }
        for &addr in recalc {
            if let Some(fid) = wb.formula_at(addr) {
                crate::calc::mark_dirty(wb, fid);
            }
        }
        let names: Vec<String> = wb
            .names
            .iter()
            .filter(|d| d.formula.volatile)
            .map(|d| d.name.to_uppercase())
            .collect();
        let mut users = Vec::new();
        for key in names {
            users.clear();
            wb.graph.dependents_of_name(&key, &mut users);
            for fid in users.clone() {
                crate::calc::mark_dirty(wb, fid);
            }
        }
        let calculated: rustc_hash::FxHashSet<crate::workbook::FormulaId> =
            wb.formula_ids().filter(|&f| wb.formula(f).dirty).collect();
        let items = saved
            .iter()
            .map(|(a, v)| (*a, v.clone()))
            .chain(blank.iter().map(|a| (*a, Value::from(""))));
        for (addr, v) in items {
            if let Some(fid) = wb.formula_at(addr) {
                if !calculated.contains(&fid) && !wb.formula(fid).value.is_array() && !v.is_array()
                {
                    wb.formula_mut(fid).value = v;
                }
            }
        }
        crate::calc::recalc_with(wb, true)
    });
}

pub fn host_volatile_cells(wb: &Workbook) -> Vec<CellAddr> {
    let mut cells: Vec<CellAddr> = wb
        .calc
        .host_volatile
        .iter()
        .map(|&f| wb.formula(f).addr)
        .collect();
    cells.sort_unstable();
    cells
}

pub fn ops_idle(wb: &Workbook) -> bool {
    wb.ops_idle() && wb.notes.idle()
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
