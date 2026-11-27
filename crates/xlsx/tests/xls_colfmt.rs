use std::collections::BTreeSet;
use std::path::{Path, PathBuf};
use xlcalc::style::Style;
use xlcalc::{CellAddr, Workbook};

const SHEETS: [&str; 3] = ["S", "T", "U"];

fn root() -> PathBuf {
    PathBuf::from(env!("CARGO_MANIFEST_DIR")).join("../../conformance")
}

fn styles(wb: &Workbook) -> Vec<(String, Style)> {
    let mut out = Vec::new();
    for name in SHEETS {
        let sheet = wb.sheet_by_name(name).unwrap().id;
        for row in 0..20u32 {
            for col in 0..256u32 {
                let a = CellAddr::new(sheet, row, col);
                out.push((format!("{name}!{row},{col}"), wb.cell_style(a).clone()));
            }
        }
    }
    out
}

fn same_styles(twin: &Path, other: &Path) {
    let a = xlcalc_xlsx::import_file(twin).unwrap();
    let b = xlcalc_xlsx::import_file(other).unwrap();
    let (sa, sb) = (styles(&a.workbook), styles(&b.workbook));

    let distinct: BTreeSet<&str> = sa.iter().map(|(_, s)| &*s.number_format).collect();
    assert!(distinct.len() > 3, "probe is vacuous: {distinct:?}");
    let diff: Vec<String> = sa
        .iter()
        .zip(&sb)
        .filter(|(x, y)| x.1 != y.1)
        .map(|(x, y)| format!("{}: {:?} vs {:?}", x.0, x.1, y.1))
        .collect();
    assert!(
        diff.is_empty(),
        "{} differences: {:#?}",
        diff.len(),
        &diff[..diff.len().min(12)]
    );
}

#[test]
fn xls_excel_colfmt_reads_like_xlsx() {
    same_styles(
        &root().join("files/colfmt1.xlsx"),
        &root().join("xls/colfmt1.xls"),
    );
}

#[test]
fn xls_written_colfmt_reads_back() {
    let twin = root().join("files/colfmt1.xlsx");
    let imp = xlcalc_xlsx::import_file(&twin).unwrap();
    let saved = xlcalc_xlsx::save(&imp.workbook, Some(&imp.package), &Default::default()).unwrap();
    let (xls, warnings) = xlcalc_xlsx::xlsx_to_xls(&saved.bytes).unwrap();
    assert!(warnings.is_empty(), "write warnings: {warnings:?}");
    let path = PathBuf::from(env!("CARGO_TARGET_TMPDIR")).join("colfmt1.xls");
    std::fs::write(&path, xls).unwrap();
    same_styles(&twin, &path);
}
