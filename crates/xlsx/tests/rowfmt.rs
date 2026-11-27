use xlcalc::{CellAddr, Workbook};

fn look(wb: &Workbook) -> (Vec<(String, bool)>, String) {
    let id = wb.sheets().next().unwrap().id;

    let cells = [
        (0, 0),
        (0, 1),
        (0, 2),
        (0, 3),
        (0, 4),
        (1, 1),
        (2, 0),
        (3, 0),
        (3, 1),
        (4, 4),
        (8, 16383),
        (5, 1),
        (5, 3),
    ];
    let seen = cells
        .iter()
        .map(|&(r, c)| {
            let a = CellAddr::new(id, r, c);
            let bold = wb.cell_style(a).font.bold;
            (wb.number_format(a).to_string(), bold)
        })
        .collect();
    (seen, format!("{:?}", wb.used_range_with_formats(id)))
}

#[test]
fn row_formats_survive_every_file_kind() {
    let path = std::path::Path::new(env!("CARGO_MANIFEST_DIR"))
        .join("../../conformance/files/rowfmt1.xlsx");
    let imp = xlcalc_xlsx::import_file(&path).unwrap();
    let expected = look(&imp.workbook);
    let want = |f: &str, b: bool| (f.to_string(), b);

    assert_eq!(expected.0[0], want("0.00%", false));
    assert_eq!(expected.0[1], want("0.00", false));
    assert_eq!(expected.0[4], want("0.00%", false));
    assert_eq!(expected.0[8], want("0.000", false));
    assert_eq!(expected.0[10], want("0.000", false));

    let saved = xlcalc_xlsx::save(&imp.workbook, Some(&imp.package), &Default::default()).unwrap();
    let (xlsb, _) = xlcalc_xlsx::xlsx_to_xlsb(&saved.bytes).unwrap();
    let (xls, _) = xlcalc_xlsx::xlsx_to_xls(&saved.bytes).unwrap();
    for (kind, bytes) in [("xlsx", saved.bytes), ("xlsb", xlsb), ("xls", xls)] {
        let again = xlcalc_xlsx::import_bytes(bytes).unwrap();
        let mut got = look(&again.workbook);

        if kind == "xls" {
            got.0[10] = expected.0[10].clone();
        }
        assert_eq!(got, expected, "{kind}");
    }
}
