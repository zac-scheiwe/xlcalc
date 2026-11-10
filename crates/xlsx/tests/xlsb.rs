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

#[test]
fn xlsb_external_links_read_like_the_xlsx_twin() {
    let a = xlcalc_xlsx::import_file(&fixture("extlinks.xlsx")).unwrap();
    let b = xlcalc_xlsx::import_file(&fixture("xlsb_extlinks.xlsb")).unwrap();
    assert!(b.warnings.is_empty(), "{:?}", b.warnings);
    let (sa, sb) = (
        a.workbook.sheets().next().unwrap().id,
        b.workbook.sheets().next().unwrap().id,
    );
    for row in 0..8 {
        let (ca, cb) = (
            xlcalc::CellAddr::new(sa, row, 0),
            xlcalc::CellAddr::new(sb, row, 0),
        );
        assert_eq!(
            format!("{:?}", a.workbook.value(ca)),
            format!("{:?}", b.workbook.value(cb)),
            "value A{}",
            row + 1
        );
        assert_eq!(
            a.workbook.formula_text(ca),
            b.workbook.formula_text(cb),
            "formula A{}",
            row + 1
        );
    }
    assert_eq!(a.workbook.ext_links(), b.workbook.ext_links());
    assert_eq!(a.workbook.ext_names(0), b.workbook.ext_names(0));
}

fn zip_part(bytes: &[u8], name: &str) -> Vec<u8> {
    use std::io::Read;
    let mut z = zip::ZipArchive::new(std::io::Cursor::new(bytes)).unwrap();
    let mut v = Vec::new();
    z.by_name(name).unwrap().read_to_end(&mut v).unwrap();
    v
}

#[test]
fn xlsb_writer_matches_excel_for_external_links() {
    let xlsx = std::fs::read(fixture("extlinks.xlsx")).unwrap();
    let theirs = std::fs::read(fixture("xlsb_extlinks.xlsb")).unwrap();
    let (ours, warnings) = xlcalc_xlsx::xlsx_to_xlsb(&xlsx).unwrap();
    for n in 1..=2 {
        let part = format!("xl/externalLinks/externalLink{n}.bin");
        assert_eq!(
            zip_part(&ours, &part),
            zip_part(&theirs, &part),
            "link {n} (warnings {warnings:?})"
        );
    }

    let a = xlcalc_xlsx::import_bytes(xlsx).unwrap();
    let b = xlcalc_xlsx::import_bytes(ours).unwrap();
    assert!(b.warnings.is_empty(), "{:?}", b.warnings);
    assert_eq!(a.workbook.ext_links(), b.workbook.ext_links());
    let s = a.workbook.sheets().next().unwrap().id;
    let t = b.workbook.sheets().next().unwrap().id;
    for row in 0..8 {
        assert_eq!(
            a.workbook.formula_text(xlcalc::CellAddr::new(s, row, 0)),
            b.workbook.formula_text(xlcalc::CellAddr::new(t, row, 0))
        );
    }
}

#[test]
fn xlsb_writer_matches_excel_for_external_3d_spans() {
    let xlsx = std::fs::read(fixture("extspan.xlsx")).unwrap();
    let theirs = std::fs::read(fixture("xlsb_extspan.xlsb")).unwrap();
    let (ours, warnings) = xlcalc_xlsx::xlsx_to_xlsb(&xlsx).unwrap();

    let part = "xl/externalLinks/externalLink1.bin";
    assert_eq!(
        zip_part(&ours, part),
        zip_part(&theirs, part),
        "{part} (warnings {warnings:?})"
    );
    let a = xlcalc_xlsx::import_bytes(xlsx).unwrap();
    let b = xlcalc_xlsx::import_bytes(ours).unwrap();
    let c = xlcalc_xlsx::import_file(&fixture("xlsb_extspan.xlsb")).unwrap();
    let s = a.workbook.sheets().next().unwrap().id;
    let t = b.workbook.sheets().next().unwrap().id;
    let u = c.workbook.sheets().next().unwrap().id;
    for row in 0..3 {
        assert_eq!(
            a.workbook.formula_text(xlcalc::CellAddr::new(s, row, 0)),
            c.workbook.formula_text(xlcalc::CellAddr::new(u, row, 0))
        );
        assert_eq!(
            a.workbook.formula_text(xlcalc::CellAddr::new(s, row, 0)),
            b.workbook.formula_text(xlcalc::CellAddr::new(t, row, 0))
        );
    }
}
