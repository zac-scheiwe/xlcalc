use xlcalc::{CellAddr, Workbook};

fn on_small_stack(f: impl FnOnce() + Send + 'static) {
    std::thread::Builder::new()
        .stack_size(1 << 20)
        .spawn(f)
        .unwrap()
        .join()
        .expect("overflow or panic");
}

fn shapes(n: usize) -> Vec<(&'static str, String)> {
    vec![
        ("parens", format!("={}1{}", "(".repeat(n), ")".repeat(n))),
        ("unary minus", format!("={}1", "-".repeat(n))),
        ("unary minus A1", format!("={}A1", "-".repeat(n))),
        ("unary plus", format!("={}A1", "+".repeat(n))),
        ("binary chain", format!("=1{}", "+1".repeat(n))),
        ("concat chain", format!("=A1{}", "&A1".repeat(n))),
        ("pow chain", format!("=1{}", "^1".repeat(n))),
        (
            "IF nest",
            format!("={}1{}", "IF(TRUE,".repeat(n), ",0)".repeat(n)),
        ),
        (
            "SUM nest",
            format!("={}A1{}", "SUM(".repeat(n), ")".repeat(n)),
        ),
        ("percent", format!("=1{}", "%".repeat(n))),
        ("array", format!("={}1{}", "{".repeat(n), "}".repeat(n))),
        ("at", format!("={}A1", "@".repeat(n))),
        ("spill", format!("=A1{}", "#".repeat(n))),
        (
            "lambda",
            format!("={}1{}", "LET(x,".repeat(n), ",x)".repeat(n)),
        ),
        (
            "mixed",
            format!("={}-(1{})", "-(".repeat(n), ")+1".repeat(n)),
        ),
    ]
}

#[test]
fn deep_formulas_do_not_overflow_the_stack() {
    on_small_stack(|| {
        for n in [50usize, 255, 500, 1000, 2000, 3000, 4000, 20000] {
            for (_name, text) in shapes(n) {
                let mut wb = Workbook::new();
                let s = wb.sheet_at(0).id;
                let a = CellAddr::new(s, 5, 5);

                let _ = wb.set_input(a, &text);
                let _ = wb.display_text(a);
                let _ = wb.formula_text(a);
                let _ = wb.insert_rows(s, 0, 1);
                let _ = wb.copy_range(s, xlcalc::Rect::new(6, 5, 6, 5), CellAddr::new(s, 9, 9));
                let _ = wb.full_recalc();
            }
        }
    });
}
