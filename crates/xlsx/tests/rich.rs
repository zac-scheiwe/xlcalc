use std::path::Path;
use xlcalc::{CellAddr, Workbook};
use xlcalc_xlsx::{import_bytes, save, xlsx_to_xls, xlsx_to_xlsb, SaveOptions};

fn pieces(wb: &Workbook, row: u32) -> Vec<(String, String)> {
    let at = CellAddr::new(wb.sheet_at(0).id, row - 1, 0);
    let show = |f: &xlcalc::rich::RunFont| {
        let x = &f.font;
        let flags = [
            x.bold,
            x.italic,
            x.strikethrough,
            x.superscript,
            x.subscript,
            x.outline,
            x.shadow,
            f.extra.condense,
            f.extra.extend,
        ];
        let flags: String = flags.map(|b| if b { '1' } else { '0' }).iter().collect();
        format!(
            "{} {} {flags} {:?} {:?}",
            x.name, x.size_twips, x.underline, f.extra.charset
        )
    };
    wb.rich_segments(at)
        .unwrap_or_default()
        .into_iter()
        .map(|(text, font)| (text, font.as_ref().map_or_else(String::new, show)))
        .collect()
}

type Readings = (Vec<(String, u32, u32)>, u8, u8);

fn phonetic(wb: &Workbook, row: u32) -> Option<Readings> {
    let at = CellAddr::new(wb.sheet_at(0).id, row - 1, 0);
    let p = wb.rich_text(at)?.phonetic.as_ref()?;
    let runs = p.runs.iter().map(|r| (r.text.to_string(), r.from, r.to));
    Some((runs.collect(), p.kind, p.align))
}

fn texts(p: &[(String, String)]) -> Vec<&str> {
    p.iter().map(|x| x.0.as_str()).collect()
}

fn check(wb: &Workbook, what: &str) {
    assert_eq!(texts(&pieces(wb, 1)), ["Hello", " ", "World"], "{what}");

    assert_eq!(texts(&pieces(wb, 7)), ["ab\u{1F600}", "cd", "ef"], "{what}");
    let outline = pieces(wb, 8);

    let cell = wb.cell_style(CellAddr::new(wb.sheet_at(0).id, 7, 0));
    assert!(cell.font.outline && outline[0].1.is_empty(), "{what}");
    assert!(
        outline.iter().any(|p| p.1.contains(" 000000100 ")),
        "{what}: {outline:?}"
    );
    let legacy = pieces(wb, 13);
    assert_eq!(texts(&legacy), ["symbol", " ", "greek"], "{what}");
    assert!(
        legacy[0].1.starts_with("Symbol ") && legacy[0].1.ends_with("000000011 None Some(2)"),
        "{what}: {legacy:?}"
    );
    assert!(legacy[2].1.ends_with("Some(128)"), "{what}: {legacy:?}");
    let plain = (
        vec![
            ("トウキョウ".to_string(), 0, 2),
            ("トチョウ".to_string(), 2, 4),
        ],
        1,
        1,
    );
    assert_eq!(phonetic(wb, 14), Some(plain), "{what}");
    let rich = (vec![("とうきょう".to_string(), 0, 2)], 2, 3);
    assert_eq!(phonetic(wb, 15), Some(rich), "{what}");
    assert_eq!(texts(&pieces(wb, 15)), ["東京", "都庁 x"], "{what}");
    assert_eq!(phonetic(wb, 1), None, "{what}");
}

#[test]
fn excel_files_read_and_write_back() {
    let data = Path::new(env!("CARGO_MANIFEST_DIR")).join("tests/data");
    let mut first: Option<Vec<_>> = None;
    for ext in ["xlsx", "xlsb", "xls"] {
        let bytes = std::fs::read(data.join(format!("rich_excel.{ext}"))).unwrap();
        let imported = import_bytes(bytes).unwrap();
        check(&imported.workbook, ext);

        let all: Vec<_> = (1..=18).map(|r| pieces(&imported.workbook, r)).collect();
        match &first {
            Some(f) => assert_eq!(&all, f, "{ext}"),
            None => first = Some(all.clone()),
        }

        let saved = save(
            &imported.workbook,
            Some(&imported.package),
            &SaveOptions::default(),
        )
        .unwrap();
        let outs = [
            ("xlsx", saved.bytes.clone()),
            ("xlsb", xlsx_to_xlsb(&saved.bytes).unwrap().0),
            ("xls", xlsx_to_xls(&saved.bytes).unwrap().0),
        ];
        for (to, bytes) in outs {
            let what = format!("{ext} saved as {to}");
            let again = import_bytes(bytes).unwrap();
            check(&again.workbook, &what);
            let read: Vec<_> = (1..=18).map(|r| pieces(&again.workbook, r)).collect();
            assert_eq!(read, all, "{what}");
        }
    }
}

#[test]
fn long_rich_strings_split_over_records() {
    let mut wb = Workbook::new();
    let sheet = wb.sheet_at(0).id;
    let mut font = wb.cell_style(CellAddr::new(sheet, 0, 0)).font.clone();
    font.bold = true;
    let bold = xlcalc::rich::RunFont {
        font,
        extra: xlcalc::rich::FontExtra::default(),
    };
    for row in 0..12 {
        let at = CellAddr::new(sheet, row, 0);
        let text: String = format!("{row:02}") + &"abcdefgh".repeat(350);
        wb.set_value(at, xlcalc::Value::from(text.as_str()));
        let len = text.len();
        let runs: Vec<_> = (0..len)
            .step_by(7)
            .enumerate()
            .map(|(i, from)| {
                let font = (i % 2 == 1).then(|| bold.clone());
                (from, (from + 7).min(len), font)
            })
            .collect();
        let phonetic = xlcalc::rich::Phonetic {
            runs: (0..200)
                .map(|i| xlcalc::rich::PhoneticRun {
                    text: "アイウ".into(),
                    from: i * 3,
                    to: i * 3 + 3,
                })
                .collect(),
            font: bold.clone(),
            font_id: 0,
            kind: 1,
            align: 1,
        };
        wb.rich_load(at, &runs, Some(phonetic));
    }
    let saved = save(&wb, None, &SaveOptions::default()).unwrap();
    for bytes in [
        xlsx_to_xls(&saved.bytes).unwrap().0,
        xlsx_to_xlsb(&saved.bytes).unwrap().0,
    ] {
        let again = import_bytes(bytes).unwrap().workbook;
        for row in 1..=12 {
            assert_eq!(pieces(&again, row), pieces(&wb, row), "row {row}");
            assert_eq!(phonetic(&again, row), phonetic(&wb, row), "row {row}");
        }
    }
}

#[test]
fn surrogate_halves_survive_the_formats() {
    let mut wb = Workbook::new();
    let sheet = wb.sheet_at(0).id;
    let texts: [&[u16]; 3] = [&[0xD83D], &[0x78, 0xDE00, 0x79], &[0x61, 0xD83D, 0xDE00]];
    for (row, units) in (0..).zip(texts) {
        let text = xlcalc::wide::from_units(units);
        wb.set_value(CellAddr::new(sheet, row, 0), text.as_str().into());
    }
    let saved = save(&wb, None, &SaveOptions::default()).unwrap();
    for (what, bytes) in [
        ("xlsx", saved.bytes.clone()),
        ("xlsb", xlsx_to_xlsb(&saved.bytes).unwrap().0),
        ("xls", xlsx_to_xls(&saved.bytes).unwrap().0),
    ] {
        let again = import_bytes(bytes).unwrap().workbook;
        let sheet = again.sheet_at(0).id;
        for (row, units) in (0..).zip(texts) {
            let xlcalc::Value::Text(t) = again.value(CellAddr::new(sheet, row, 0)) else {
                panic!("{what}: row {row} is not text");
            };
            let read: Vec<u16> = xlcalc::wide::units(&t).collect();
            assert_eq!(read, units, "{what}: row {row}");
        }
    }
}
