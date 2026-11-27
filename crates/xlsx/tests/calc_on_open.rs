use xlcalc::{CellAddr, Workbook};

#[test]
fn saved_results_are_kept_on_open() {
    let mut wb = Workbook::new();
    let s = wb.sheet_at(0).id;
    wb.set_input(CellAddr::new(s, 0, 0), "=1+1").unwrap();
    let xlsx = xlcalc_xlsx::save(&wb, None, &Default::default())
        .unwrap()
        .bytes;
    let (xlsb, _) = xlcalc_xlsx::xlsx_to_xlsb(&xlsx).unwrap();
    for (format, bytes) in [("xlsx", xlsx), ("xlsb", xlsb)] {
        let imp = xlcalc_xlsx::import_bytes(bytes).unwrap();
        assert!(imp.opened_uncalculated, "{format}");
        let again = xlcalc_xlsx::save(&imp.workbook, Some(&imp.package), &Default::default())
            .unwrap()
            .bytes;
        let back = xlcalc_xlsx::import_bytes(again).unwrap();
        assert!(back.opened_uncalculated, "{format} resaved");
    }
}

fn calc_of(bytes: &[u8]) -> (String, usize) {
    let imp = xlcalc_xlsx::import_bytes(bytes.to_vec()).unwrap();
    let part = imp.package.part("xl/workbook.xml").unwrap();
    let text = String::from_utf8(part.to_vec()).unwrap();
    let from = text.find("<calcPr").unwrap();
    let to = from + text[from..].find('>').unwrap();
    (
        text[from..to].to_string(),
        text.matches("<xcalcf:feature ").count(),
    )
}

#[test]
fn pending_manual_book() {
    let dir = concat!(env!("CARGO_MANIFEST_DIR"), "/../../conformance/files");
    for (name, id, features) in [("old", 191_027, 4), ("version", 0, 0)] {
        let bytes = std::fs::read(format!("{dir}/calcopen_{name}_off.xlsx")).unwrap();
        let mut imp = xlcalc_xlsx::import_bytes(bytes).unwrap();
        let package = imp.package;
        let save = |wb: &Workbook| {
            xlcalc_xlsx::save(wb, Some(&package), &Default::default())
                .unwrap()
                .bytes
        };
        assert_eq!(imp.workbook.load_pending(), Some(id), "{name}");
        let kept = save(&imp.workbook);
        let (xlsb, _) = xlcalc_xlsx::xlsx_to_xlsb(&kept).unwrap();
        for (format, bytes) in [("xlsx", kept), ("xlsb", xlsb)] {
            let back = xlcalc_xlsx::import_bytes(bytes.clone()).unwrap();
            assert!(back.workbook.load_pending().is_some(), "{name} {format}");
            assert!(!back.workbook.settings().calc_on_save, "{name} {format}");
            if format == "xlsx" {
                let (calc, count) = calc_of(&bytes);
                for want in [
                    format!("calcId=\"{id}\""),
                    "calcCompleted=\"0\"".into(),
                    "calcOnSave=\"0\"".into(),
                ] {
                    assert!(calc.contains(&want), "{name}: {calc}");
                }
                assert_eq!(count, features, "{name}");
            }
        }
        imp.workbook.recalc();
        assert_eq!(imp.workbook.load_pending(), None, "{name}");
        let (calc, count) = calc_of(&save(&imp.workbook));
        assert!(calc.contains("calcId=\"191029\""), "{name}: {calc}");
        assert!(!calc.contains("calcCompleted"), "{name}: {calc}");
        assert_eq!(count, 7, "{name}");
    }
}

#[test]
fn xls_calc_settings() {
    let dir = concat!(env!("CARGO_MANIFEST_DIR"), "/../../conformance/xls");
    let bytes = std::fs::read(format!("{dir}/budget.xls")).unwrap();
    let imp = xlcalc_xlsx::import_bytes(bytes).unwrap();
    let saved = xlcalc_xlsx::save(&imp.workbook, Some(&imp.package), &Default::default())
        .unwrap()
        .bytes;
    let (calc, _) = calc_of(&saved);
    assert!(!calc.contains("calcOnSave"), "{calc}");

    let mut wb = Workbook::new();
    wb.set_mode(xlcalc::CalcMode::Manual);
    wb.set_calc_on_save(false);
    let (xls, _) = xlcalc_xlsx::xlsx_to_xls_of(
        &xlcalc_xlsx::save(&wb, None, &Default::default())
            .unwrap()
            .bytes,
        &wb,
        false,
    )
    .unwrap();
    let back = xlcalc_xlsx::import_bytes(xls).unwrap().workbook;
    assert_eq!(back.settings().mode, xlcalc::CalcMode::Manual);
    assert!(!back.settings().calc_on_save);
}
