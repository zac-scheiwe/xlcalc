use std::path::PathBuf;

fn fixture(name: &str) -> PathBuf {
    PathBuf::from(env!("CARGO_MANIFEST_DIR"))
        .join("../../conformance/workbooks")
        .join(name)
}

#[test]
fn xlsb_reads_like_its_xlsx_twin() {
    for name in ["budget", "tblcalc", "datatables", "letlambda", "names_prec"] {
        let a = xlcalc_xlsx::import_file(&fixture(&format!("{name}.xlsx"))).unwrap();
        let b = xlcalc_xlsx::import_file(&fixture(&format!("xlsb_{name}.xlsb"))).unwrap();
        assert!(b.warnings.is_empty(), "{name}: {:?}", b.warnings);
        let sheets: Vec<String> = a.workbook.sheets().map(|s| s.name.clone()).collect();
        let other: Vec<String> = b.workbook.sheets().map(|s| s.name.clone()).collect();
        assert_eq!(sheets, other, "{name}");
        for s in &sheets {
            let (sa, sb) = (
                a.workbook.sheet_by_name(s).unwrap(),
                b.workbook.sheet_by_name(s).unwrap(),
            );
            let used = a.workbook.used_range(sa.id);
            let Some(r) = used else { continue };
            for row in r.r0..=r.r1 {
                for col in r.c0..=r.c1 {
                    let va = a.workbook.value(xlcalc::CellAddr::new(sa.id, row, col));
                    let vb = b.workbook.value(xlcalc::CellAddr::new(sb.id, row, col));
                    assert_eq!(
                        format!("{va:?}"),
                        format!("{vb:?}"),
                        "{name}!{s} {row},{col}"
                    );
                }
            }
        }
    }
}

#[test]
fn xlsb_keeps_the_vba_project() {
    let b = xlcalc_xlsx::import_file(&fixture("xlsb_vba.xlsb")).unwrap();
    let vba = b.vba.expect("vbaProject.bin");
    assert!(vba.project.is_ok());
    assert_eq!(vba.workbook_code_name.as_deref(), Some("ThisWorkbook"));
    assert_eq!(vba.sheet_code_names[0].1.as_deref(), Some("Sheet1"));
}

#[test]
fn a_file_that_is_not_a_package_is_refused() {
    assert!(xlcalc_xlsx::import_bytes(b"not a zip".to_vec()).is_err());
}
