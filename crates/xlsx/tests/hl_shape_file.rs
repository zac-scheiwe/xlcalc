use std::path::PathBuf;
use xlcalc::Workbook;

type Row = (u32, String, String, Option<String>);

fn shapes(wb: &Workbook) -> Vec<Row> {
    let s = wb.sheets().next().unwrap().id;
    let mut v: Vec<Row> = wb
        .hyperlinks(s)
        .iter()
        .filter_map(|h| {
            Some((
                h.shape?,
                h.address.clone(),
                h.sub_address.clone(),
                h.screen_tip.clone(),
            ))
        })
        .collect();
    v.sort();
    v
}

#[test]
fn shape_links_round_trip() {
    let path =
        PathBuf::from(env!("CARGO_MANIFEST_DIR")).join("../../conformance/files/hl_shape.xlsx");
    let want: Vec<Row> = vec![
        (2, String::new(), "Sheet1!B3".into(), Some("tip1".into())),
        (
            3,
            "http://x.org/".into(),
            String::new(),
            Some("tip2".into()),
        ),
    ];
    let imp = xlcalc_xlsx::import_file(&path).unwrap();
    assert_eq!(shapes(&imp.workbook), want);

    let saved = xlcalc_xlsx::save(&imp.workbook, Some(&imp.package), &Default::default()).unwrap();
    let back = xlcalc_xlsx::import_bytes(saved.bytes).unwrap();
    assert_eq!(shapes(&back.workbook), want);

    let mut wb = imp.workbook;
    let s = wb.sheets().next().unwrap().id;
    let id = wb
        .hyperlinks(s)
        .iter()
        .find(|h| h.shape == Some(3))
        .unwrap()
        .id;
    wb.hyperlink_remove(s, id);
    wb.hyperlink_add(
        s,
        xlcalc::hyperlink::Hyperlink {
            id: 0,
            rect: xlcalc::Rect::cell(0, 0),
            address: "http://y.org/".into(),
            sub_address: String::new(),
            screen_tip: Some("new".into()),
            name: None,
            shape: Some(3),
        },
    );
    let saved = xlcalc_xlsx::save(&wb, Some(&imp.package), &Default::default()).unwrap();
    let back = xlcalc_xlsx::import_bytes(saved.bytes).unwrap();
    let got = shapes(&back.workbook);
    assert_eq!(got[0], want[0]);
    assert_eq!(
        got[1],
        (3, "http://y.org/".into(), String::new(), Some("new".into()))
    );
}

#[test]
fn shape_links_in_every_format() {
    let want: Vec<Row> = vec![
        (2, String::new(), "Sheet1!B3".into(), Some("tip1".into())),
        (
            3,
            "http://x.org/".into(),
            String::new(),
            Some("tip2".into()),
        ),
    ];
    for ext in ["xlsx", "xlsb", "xls"] {
        let path = PathBuf::from(env!("CARGO_MANIFEST_DIR"))
            .join(format!("../../conformance/files/hl_shape.{ext}"));
        let imp = xlcalc_xlsx::import_file(&path).unwrap();
        assert_eq!(shapes(&imp.workbook), want, "{ext} read");
        let mut wb = imp.workbook;
        let saved = xlcalc_xlsx::save(&wb, Some(&imp.package), &Default::default()).unwrap();
        let as_b = xlcalc_xlsx::xlsx_to_xlsb(&saved.bytes).unwrap().0;
        let as_s = xlcalc_xlsx::xlsx_to_xls(&saved.bytes).unwrap().0;
        for (tag, bytes) in [("xlsx", saved.bytes), ("xlsb", as_b), ("xls", as_s)] {
            let back = xlcalc_xlsx::import_bytes(bytes).unwrap();
            assert_eq!(shapes(&back.workbook), want, "{ext} saved as {tag}");
        }

        let s = wb.sheets().next().unwrap().id;
        let id = wb
            .hyperlinks(s)
            .iter()
            .find(|h| h.shape == Some(3))
            .unwrap()
            .id;
        wb.hyperlink_remove(s, id);
        wb.hyperlink_add(
            s,
            xlcalc::hyperlink::Hyperlink {
                id: 0,
                rect: xlcalc::Rect::cell(0, 0),
                address: "http://y.org/".into(),
                sub_address: "top".into(),
                screen_tip: Some("new".into()),
                name: None,
                shape: Some(3),
            },
        );
        let saved = xlcalc_xlsx::save(&wb, Some(&imp.package), &Default::default()).unwrap();
        let as_b = xlcalc_xlsx::xlsx_to_xlsb(&saved.bytes).unwrap().0;
        let as_s = xlcalc_xlsx::xlsx_to_xls(&saved.bytes).unwrap().0;
        for (tag, bytes) in [("xlsx", saved.bytes), ("xlsb", as_b), ("xls", as_s)] {
            let back = xlcalc_xlsx::import_bytes(bytes).unwrap();
            let got = shapes(&back.workbook);
            assert_eq!(got[0], want[0], "{ext} new link as {tag}");
            let new = (
                3,
                "http://y.org/".to_string(),
                "top".to_string(),
                Some("new".to_string()),
            );
            assert_eq!(got[1], new, "{ext} new link as {tag}");
        }
    }
}

#[test]
#[ignore = "writes files for Excel"]
fn write_files_for_excel() {
    let dir = PathBuf::from(std::env::var("HL_OUT").unwrap());
    let path =
        PathBuf::from(env!("CARGO_MANIFEST_DIR")).join("../../conformance/files/hl_shape.xls");
    let imp = xlcalc_xlsx::import_file(&path).unwrap();
    let mut wb = imp.workbook;
    let s = wb.sheets().next().unwrap().id;
    let id = wb
        .hyperlinks(s)
        .iter()
        .find(|h| h.shape == Some(3))
        .unwrap()
        .id;
    wb.hyperlink_remove(s, id);
    wb.hyperlink_add(
        s,
        xlcalc::hyperlink::Hyperlink {
            id: 0,
            rect: xlcalc::Rect::cell(0, 0),
            address: "http://y.org/p".into(),
            sub_address: "top".into(),
            screen_tip: Some("new".into()),
            name: None,
            shape: Some(3),
        },
    );
    let saved = xlcalc_xlsx::save(&wb, Some(&imp.package), &Default::default()).unwrap();
    std::fs::write(dir.join("mine.xlsx"), &saved.bytes).unwrap();
    let b = xlcalc_xlsx::xlsx_to_xlsb(&saved.bytes).unwrap().0;
    std::fs::write(dir.join("mine.xlsb"), b).unwrap();
    let x = xlcalc_xlsx::xlsx_to_xls(&saved.bytes).unwrap().0;
    std::fs::write(dir.join("mine.xls"), x).unwrap();
}
