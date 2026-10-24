use xlcalc::{CalcMode, CellAddr, Rect, SheetId, Value, Workbook};

const MAXR: u32 = 1_048_575;
const MAXC: u32 = 16_383;

fn wb() -> (Workbook, SheetId) {
    let wb = Workbook::new();
    let s = wb.sheet_at(0).id;
    (wb, s)
}

fn a(s: SheetId, r: u32, c: u32) -> CellAddr {
    CellAddr::new(s, r, c)
}

#[test]
fn corner_cells_work() {
    let (mut wb, s) = wb();
    wb.set_input(a(s, MAXR, MAXC), "7").unwrap();
    wb.set_input(a(s, 0, 0), "=XFD1048576*2").unwrap();
    assert!(matches!(wb.value(a(s, 0, 0)), Value::Number(n) if n == 14.0));
    assert_eq!(wb.display_text(a(s, MAXR, MAXC)), "7");
    assert_eq!(wb.used_range(s), Some(Rect::new(0, 0, MAXR, MAXC)));
    assert_eq!(wb.addr(s, "XFD1048576"), Some(a(s, MAXR, MAXC)));
    assert_eq!(wb.addr(s, "XFE1"), None);
    assert_eq!(wb.addr(s, "A1048577"), None);

    wb.set_input(a(s, 1, 0), "=XFD1048576+1").unwrap();
    let r = wb.set_input(a(s, MAXR, 0), "=A1048576:XFD1048576");
    assert!(r.is_ok() || r.is_err());
    wb.set_input(a(s, 2, 0), "=OFFSET(A1,1048575,16383)")
        .unwrap();
    assert!(matches!(wb.value(a(s, 2, 0)), Value::Number(n) if n == 7.0));
    wb.set_input(a(s, 3, 0), "=OFFSET(A1,1048576,0)").unwrap();
    assert!(wb.value(a(s, 3, 0)).is_error());
    wb.set_input(a(s, 4, 0), "=SUM(A:XFD)").unwrap();
    wb.set_input(a(s, 5, 0), "=ROWS(A:A)+COLUMNS(1:1)").unwrap();
    assert!(matches!(wb.value(a(s, 5, 0)), Value::Number(n) if n == 1_048_576.0 + 16_384.0));
}

#[test]
fn out_of_grid_addresses_do_not_panic() {
    let (mut wb, s) = wb();
    for &(r, c) in &[
        (MAXR + 1, 0),
        (0, MAXC + 1),
        (u32::MAX, u32::MAX),
        (u32::MAX, 0),
    ] {
        let ad = a(s, r, c);
        let _ = wb.set_input(ad, "1");
        let _ = wb.set_input(ad, "=1+1");
        let _ = wb.set_formula(ad, "=A1");
        let _ = wb.set_formula(ad, "=R[1]C[1]");
        let _ = wb.set_value(ad, Value::Number(1.0));
        let _ = wb.clear(ad);
        let _ = wb.value(ad);
        let _ = wb.display_text(ad);
        let _ = wb.formula_text(ad);
        let _ = wb.dependents(ad);
        let _ = wb.precedents(ad);
        let _ = wb.number_format(ad);
        let _ = wb.spill_anchor(ad);
        let _ = wb.merge_at(ad);
        let _ = wb.table_at(ad);
        let _ = wb.set_legacy_input(ad, "=A1:A3");
        let _ = wb.goal_seek(ad, 1.0, ad);
    }
}

#[test]
fn structural_edits_at_the_edges() {
    let (mut wb, s) = wb();
    wb.set_input(a(s, MAXR, MAXC), "1").unwrap();
    wb.set_input(a(s, 0, 0), "=XFD1048576").unwrap();

    let _ = wb.insert_rows(s, 0, 1);
    let _ = wb.insert_cols(s, 0, 1);
    let _ = wb.insert_rows(s, MAXR, 1);
    let _ = wb.insert_cols(s, MAXC, 1);
    let _ = wb.insert_rows(s, MAXR + 1, 1);
    let _ = wb.insert_rows(s, 0, u32::MAX);
    let _ = wb.insert_cols(s, 0, u32::MAX);
    let _ = wb.insert_rows(s, u32::MAX, u32::MAX);
    let _ = wb.delete_rows(s, MAXR, 1);
    let _ = wb.delete_rows(s, MAXR, u32::MAX);
    let _ = wb.delete_rows(s, u32::MAX, u32::MAX);
    let _ = wb.delete_cols(s, MAXC, 5);
    let _ = wb.delete_cols(s, u32::MAX, 1);
    let _ = wb.delete_rows(s, 0, 0);
    let _ = wb.insert_cells(s, Rect::new(0, 0, MAXR, MAXC), true);
    let _ = wb.insert_cells(s, Rect::new(0, 0, MAXR, 0), false);
    let _ = wb.delete_cells(s, Rect::new(0, 0, MAXR, MAXC), false);
    let _ = wb.delete_cells(s, Rect::new(MAXR, MAXC, MAXR, MAXC), true);

    let wild = Rect::new(0, 0, u32::MAX - 1, u32::MAX - 1);
    let _ = wb.insert_cells(s, wild, true);
    let _ = wb.delete_cells(s, wild, true);
    let _ = wb.clear_range(s, wild);
}

#[test]
fn huge_ranges_finish_quickly() {
    let (mut wb, s) = wb();
    wb.set_input(a(s, 0, 0), "1").unwrap();
    wb.set_input(a(s, 5, 5), "=A1+1").unwrap();
    let whole = Rect::new(0, 0, MAXR, MAXC);
    let t = std::time::Instant::now();
    let _ = wb.copy_range(s, whole, a(s, 0, 0));
    let _ = wb.copy_range(s, whole, a(s, 1, 1));
    let _ = wb.copy_range(s, Rect::new(0, 0, 0, 0), a(s, MAXR, MAXC));
    let _ = wb.copy_range(s, Rect::new(0, 0, 1, 1), a(s, MAXR, MAXC));
    let _ = wb.move_range(s, whole, a(s, 0, 0));
    let _ = wb.move_range(s, Rect::new(0, 0, 1, 1), a(s, MAXR, MAXC));
    let _ = wb.sort_range(s, whole, &Default::default());
    let _ = wb.merge_cells(s, whole, false);
    let _ = wb.unmerge_cells(s, whole);
    let _ = wb.set_number_format(s, whole, "0.00");
    let _ = wb.set_number_format(s, Rect::new(0, 0, MAXR, 3), "0.00");
    let _ = wb.paste_special(s, whole, s, whole, &Default::default());
    let _ = wb.paste_special(s, Rect::new(0, 0, 0, 0), s, whole, &Default::default());
    let _ = wb.set_array_formula(s, whole, "=A1");
    let _ = wb.add_table("T", s, Rect::new(0, 0, MAXR, 40));
    let _ = wb.add_data_table(s, whole, Some(a(s, 0, 0)), None);
    let r = wb.clear_range(s, whole);
    assert!(r.is_ok());
    assert!(
        t.elapsed().as_secs() < 60,
        "whole-sheet operations took {:?}",
        t.elapsed()
    );
}

#[test]
fn clearing_the_whole_sheet_empties_it() {
    let (mut wb, s) = wb();
    wb.set_input(a(s, 0, 0), "1").unwrap();
    wb.set_input(a(s, 1, 0), "=A1+1").unwrap();
    wb.set_input(a(s, MAXR, MAXC), "x").unwrap();
    wb.clear_range(s, Rect::new(0, 0, MAXR, MAXC)).unwrap();
    assert_eq!(wb.used_range(s), None);
    assert_eq!(wb.formula_count(), 0);
}

#[test]
fn whole_sheet_array_formula_is_refused() {
    let (mut wb, s) = wb();
    assert!(wb
        .set_array_formula(s, Rect::new(0, 0, MAXR, MAXC), "=1")
        .is_err());
}

#[test]
fn bad_arguments_are_errors_not_panics() {
    let (mut wb, s) = wb();
    let ghost = SheetId(9999);
    assert!(wb.set_input(a(ghost, 0, 0), "1").is_err() || wb.value(a(ghost, 0, 0)).is_blank());
    let _ = wb.set_formula(a(ghost, 0, 0), "=1");
    let _ = wb.insert_rows(ghost, 0, 1);
    let _ = wb.delete_cols(ghost, 0, 1);
    let _ = wb.rename_sheet(ghost, "x");
    let _ = wb.delete_sheet(ghost);
    let _ = wb.move_sheet(ghost, None);
    let _ = wb.copy_sheet(ghost, None);
    let _ = wb.move_sheet(s, Some(ghost));
    let _ = wb.copy_sheet(s, Some(ghost));
    let _ = wb.copy_range(ghost, Rect::new(0, 0, 1, 1), a(s, 0, 0));
    let _ = wb.copy_range(s, Rect::new(0, 0, 1, 1), a(ghost, 0, 0));
    let _ = wb.fill(ghost, Rect::new(0, 0, 0, 0), Rect::new(0, 0, 5, 0));
    let _ = wb.sort_range(ghost, Rect::new(0, 0, 3, 3), &Default::default());
    let _ = wb.goal_seek(a(s, 0, 0), 1.0, a(s, 1, 1));
    let _ = wb.add_sheet("");
    let _ = wb.add_sheet(&"x".repeat(100));
    let _ = wb.add_sheet("a/b[c]");
    let _ = wb.add_sheet("History");
    let _ = wb.define_name("", "=1", None);
    let _ = wb.define_name("A1", "=1", None);
    let _ = wb.define_name("x", "=", None);
    let _ = wb.define_name("x", "=((", Some(ghost));
    let _ = wb.delete_name("nothing", None);
    let _ = wb.delete_name("x", Some(ghost));
    let _ = wb.add_table("", s, Rect::new(0, 0, 3, 3));
    let _ = wb.set_table_totals("nope", true);
    let _ = wb.add_data_table(s, Rect::new(0, 0, 0, 0), None, None);
    for text in [
        "=",
        "=(",
        "=)",
        "=\"",
        "='",
        "=A1:",
        "=[",
        "=A1[",
        "={",
        "={1,",
        "=1+",
        "=@",
        "=#",
        "=_xlfn.",
        "=LET(",
        "=LAMBDA(",
        "\u{0}",
        "=\u{1F600}",
        "=Sheet9!A1",
        "=R[99999999]C",
        "=RC[99999999]",
        "=R99999999C1",
    ] {
        let _ = wb.set_input(a(s, 0, 0), text);
        let _ = wb.set_formula(a(s, 0, 0), text);
        let _ = wb.set_legacy_input(a(s, 0, 0), text);
        let _ = wb.set_array_formula(s, Rect::new(0, 0, 2, 2), text);
        let _ = wb.define_name("n", text, None);
    }

    let _ = std::panic::catch_unwind(|| ());
}

#[test]
fn calc_modes_and_iteration_edges() {
    let (mut wb, s) = wb();
    wb.set_input(a(s, 0, 0), "=B1+1").unwrap();
    wb.set_input(a(s, 0, 1), "=A1+1").unwrap();
    wb.set_iteration(true, Some(0), Some(f64::NAN));
    wb.set_iteration(true, Some(u32::MAX), Some(-1.0));
    wb.set_iteration(true, Some(1), Some(f64::INFINITY));
    wb.set_mode(CalcMode::Manual);
    wb.recalc();
    wb.full_recalc();
    wb.set_semiautomatic();
    assert!(wb.is_semiautomatic());
    wb.set_mode(CalcMode::Automatic);
    assert!(!wb.is_semiautomatic());
    wb.rebuild();
}
