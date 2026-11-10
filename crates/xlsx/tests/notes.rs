use xlcalc::{CellAddr, Value, Workbook};
use xlcalc_xlsx::{check_package, import_bytes, save, SaveOptions};

fn notes(wb: &Workbook) -> Vec<(u32, u32, String, String, bool)> {
    let s = wb.active_sheet();
    wb.notes(s)
        .map(|((r, c), n)| (r, c, n.text.clone(), n.author.clone(), n.visible))
        .collect()
}

#[test]
fn notes_round_trip() {
    let mut wb = Workbook::new();
    let s = wb.active_sheet();
    wb.set_value(CellAddr::new(s, 0, 0), Value::Number(1.0));
    wb.note_add(CellAddr::new(s, 2, 1), "first & <b>\nline", "zac", false);
    wb.note_add(CellAddr::new(s, 0, 3), "shown", "ann", true);
    let saved = save(&wb, None, &SaveOptions::default()).unwrap();
    let problems = check_package(&saved.bytes);
    assert!(problems.is_empty(), "{problems:#?}");
    let back = import_bytes(saved.bytes.clone()).unwrap();
    assert_eq!(notes(&back.workbook), notes(&wb));

    let mut wb2 = back.workbook;
    let s2 = wb2.active_sheet();
    let id = wb2.note_at(s2, 2, 1).unwrap().id;
    wb2.note_remove(s2, id);
    let saved2 = save(&wb2, None, &SaveOptions::default()).unwrap();
    let back2 = import_bytes(saved2.bytes).unwrap();
    assert_eq!(notes(&back2.workbook), notes(&wb2));
}

fn all_notes(wb: &Workbook) -> Vec<(String, u32, u32, String, String, bool)> {
    let mut v = Vec::new();
    for s in wb.sheets() {
        let id = s.id;
        for ((r, c), n) in wb.notes(id) {
            v.push((
                s.name.clone(),
                r,
                c,
                n.text.clone(),
                n.author.clone(),
                n.visible,
            ));
        }
    }
    v
}

#[test]
fn excel_files() {
    let dir = concat!(env!("CARGO_MANIFEST_DIR"), "/tests/data/");
    let load = |n: &str| import_bytes(std::fs::read(format!("{dir}{n}")).unwrap()).unwrap();
    let x = load("notes.xlsx");
    let want = all_notes(&x.workbook);
    assert_eq!(want.len(), 3, "{want:?}");
    for f in ["notes.xlsb", "notes.xls"] {
        let b = load(f);
        assert_eq!(all_notes(&b.workbook), want, "{f}: {:?}", b.warnings);
    }
}

#[test]
fn binary_formats() {
    let mut wb = Workbook::new();
    let s = wb.active_sheet();
    wb.set_value(CellAddr::new(s, 0, 0), Value::Number(1.0));
    wb.note_add(CellAddr::new(s, 2, 1), "first & <b>\nline", "zac", false);
    wb.note_add(CellAddr::new(s, 0, 3), "shown", "ann", true);
    let saved = save(&wb, None, &SaveOptions::default()).unwrap();
    let (xlsb, w) = xlcalc_xlsx::xlsx_to_xlsb(&saved.bytes).unwrap();
    assert!(w.is_empty(), "{w:?}");
    let (xls, w) = xlcalc_xlsx::xlsx_to_xls(&saved.bytes).unwrap();
    assert!(w.is_empty(), "{w:?}");
    if let Ok(d) = std::env::var("NOTES_OUT") {
        std::fs::write(format!("{d}/w.xlsb"), &xlsb).unwrap();
        std::fs::write(format!("{d}/w.xls"), &xls).unwrap();
    }
    for bytes in [xlsb, xls] {
        let back = import_bytes(bytes).unwrap();
        assert_eq!(notes(&back.workbook), notes(&wb), "{:?}", back.warnings);
    }
}
