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

fn with_part(bytes: &[u8], name: &str, edit: impl Fn(&str) -> String) -> Vec<u8> {
    use std::io::{Read, Write};
    let mut from = zip::ZipArchive::new(std::io::Cursor::new(bytes)).unwrap();
    let mut to = zip::ZipWriter::new(std::io::Cursor::new(Vec::new()));
    for i in 0..from.len() {
        let mut part = from.by_index(i).unwrap();
        let this = part.name().unwrap().into_owned();
        let mut data = Vec::new();
        part.read_to_end(&mut data).unwrap();
        if this == name {
            data = edit(std::str::from_utf8(&data).unwrap()).into_bytes();
        }
        to.start_file(this, zip::write::SimpleFileOptions::default())
            .unwrap();
        to.write_all(&data).unwrap();
    }
    to.finish().unwrap().into_inner()
}

#[test]
fn view_state_survives_the_binary_formats() {
    let mut wb = xlcalc::Workbook::new();
    let sheet = wb.add_sheet("S").unwrap();
    wb.set_value(
        xlcalc::CellAddr::new(sheet, 0, 0),
        xlcalc::Value::Number(1.0),
    );
    let plain = xlcalc_xlsx::save(&wb, None, &Default::default())
        .unwrap()
        .bytes;
    let view = "<sheetViews><sheetView tabSelected=\"1\" view=\"pageBreakPreview\" \
        topLeftCell=\"B3\" zoomScale=\"80\" workbookViewId=\"0\"><selection activeCell=\"F7\" \
        activeCellId=\"1\" sqref=\"C4 F7:G8\"/></sheetView></sheetViews>";
    let source = with_part(&plain, "xl/worksheets/sheet1.xml", |x| {
        let from = x.find("<sheetData").unwrap();
        let to = x.find("</sheetData>").map_or_else(
            || from + x[from..].find("/>").unwrap() + 2,
            |at| at + "</sheetData>".len(),
        );
        let open = x.find("<worksheet").unwrap();
        let head = &x[..open + x[open..].find('>').unwrap() + 1];
        format!(
            "{head}<sheetPr><tabColor rgb=\"FF123456\"/></sheetPr>{view}\
             <cols><col min=\"2\" max=\"2\" width=\"12\" style=\"1\" customWidth=\"1\"/></cols>\
             {}<rowBreaks count=\"1\" manualBreakCount=\"1\"><brk id=\"7\" max=\"16383\" \
             man=\"1\"/></rowBreaks></worksheet>",
            &x[from..to]
        )
    });
    let source = with_part(&source, "xl/styles.xml", |x| {
        let from = x.find("<cellXfs").unwrap();
        let to = x.find("</cellXfs>").unwrap();
        let xf = "<xf numFmtId=\"2\" fontId=\"0\" fillId=\"0\" borderId=\"0\" xfId=\"0\" \
            applyNumberFormat=\"1\"/>";
        let open = from + x[from..].find('>').unwrap() + 1;
        let first = &x[open..to];
        format!("{}<cellXfs count=\"2\">{first}{xf}{}", &x[..from], &x[to..])
    });
    let (xlsb, _) = xlcalc_xlsx::xlsx_to_xlsb(&source).unwrap();
    let (xls, _) = xlcalc_xlsx::xlsx_to_xls(&source).unwrap();
    for (format, bytes) in [("xlsb", xlsb), ("xls", xls)] {
        let imp = xlcalc_xlsx::import_bytes(bytes).unwrap();
        let out = xlcalc_xlsx::save(&imp.workbook, Some(&imp.package), &Default::default())
            .unwrap()
            .bytes;
        let xml = String::from_utf8(zip_part(&out, "xl/worksheets/sheet1.xml")).unwrap();
        for want in [
            "rgb=\"FF123456\"",
            "view=\"pageBreakPreview\"",
            "topLeftCell=\"B3\"",
            "zoomScale=\"80\"",
            "activeCell=\"F7\" activeCellId=\"1\" sqref=\"C4 F7:G8\"",
            "<brk id=\"7\"",
            "style=\"",
        ] {
            assert!(xml.contains(want), "{format}: no {want} in {xml}");
        }
    }
}
