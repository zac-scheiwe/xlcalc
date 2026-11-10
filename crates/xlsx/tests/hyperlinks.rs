use std::path::PathBuf;
use xlcalc::{CellAddr, Value, Workbook};

fn fixture(name: &str) -> PathBuf {
    PathBuf::from(env!("CARGO_MANIFEST_DIR"))
        .join("../../conformance/files")
        .join(name)
}

fn null(s: &str) -> String {
    if s.is_empty() { "<null>" } else { s }.to_string()
}

fn loose(address: &str, loose: bool) -> String {
    if !loose {
        return address.to_string();
    }
    let a = address.replace("%20", " ");
    if a.contains(':') {
        a
    } else {
        a.replace('/', "\\")
    }
}

fn dump(wb: &Workbook, spaces: bool) -> Vec<String> {
    let mut out = Vec::new();
    for s in wb.sheets() {
        for h in wb.hyperlinks(s.id) {
            let cell = match wb.value(CellAddr::new(s.id, h.rect.r0, h.rect.c0)) {
                Value::Text(t) => t.to_string(),
                Value::Number(n) => n.to_string(),
                _ => String::new(),
            };
            out.push(format!(
                "{} {} addr={} sub={} tip={} name={} cell={}",
                s.name,
                xlcalc_xlsx_rect(&h.rect),
                null(&loose(&h.address, spaces)),
                null(&h.sub_address),
                h.screen_tip
                    .as_deref()
                    .map_or("<null>".into(), str::to_string),
                h.display_name().unwrap_or_default(),
                cell
            ));
        }
    }
    out
}

fn xlcalc_xlsx_rect(r: &xlcalc::Rect) -> String {
    let a1 = |row: u32, col: u32| format!("{}{}", xlcalc::col_to_letters(col), row + 1);
    if r.r0 == r.r1 && r.c0 == r.c1 {
        a1(r.r0, r.c0)
    } else {
        format!("{}:{}", a1(r.r0, r.c0), a1(r.r1, r.c1))
    }
}

fn golden(name: &str) -> Vec<String> {
    let text = std::fs::read_to_string(fixture(&format!("{name}.txt"))).unwrap();
    text.lines()
        .map(|l| {
            let mut keep = Vec::new();
            for part in l
                .split(" text=")
                .take(1)
                .chain(l.split(" name=").skip(1).take(1))
            {
                keep.push(part.to_string());
            }
            let head = &keep[0];
            let tail = keep[1].split(" hasf=").next().unwrap();
            format!("{head} name={tail}")
        })
        .collect()
}

fn import(name: &str) -> Workbook {
    let imp = xlcalc_xlsx::import_file(&fixture(name)).unwrap();
    assert!(imp.warnings.is_empty(), "{name}: {:?}", imp.warnings);
    imp.workbook
}

#[test]
fn files_saved_by_excel_read_like_excel_reads_them() {
    for name in ["hl_book.xlsx", "hl_book.xlsb", "hl_book.xls"] {
        assert_eq!(dump(&import(name), false), golden(name), "{name}");
    }
}

#[test]
fn saved_files_read_back_the_same() {
    for name in ["hl_book.xlsx", "hl_book.xlsb", "hl_book.xls"] {
        let imp = xlcalc_xlsx::import_file(&fixture(name)).unwrap();
        let before = dump(&imp.workbook, true);
        let saved =
            xlcalc_xlsx::save(&imp.workbook, Some(&imp.package), &Default::default()).unwrap();
        let back = xlcalc_xlsx::import_bytes(saved.bytes.clone()).unwrap();
        assert_eq!(dump(&back.workbook, true), before, "{name} as xlsx");
        let (b, _) = xlcalc_xlsx::xlsx_to_xlsb(&saved.bytes).unwrap();
        let back = xlcalc_xlsx::import_bytes(b).unwrap();
        assert_eq!(dump(&back.workbook, true), before, "{name} as xlsb");
        let (x, _) = xlcalc_xlsx::xlsx_to_xls(&saved.bytes).unwrap();
        let back = xlcalc_xlsx::import_bytes(x).unwrap();
        assert_eq!(dump(&back.workbook, true), before, "{name} as xls");
    }
}

#[test]
#[ignore = "writes files for the Excel check"]
fn write_files_for_excel() {
    let dir = PathBuf::from(std::env::var("HL_OUT").expect("HL_OUT"));
    let imp = xlcalc_xlsx::import_file(&fixture("hl_book.xlsx")).unwrap();
    let mut wb = Workbook::new();
    let s = wb.sheets().next().unwrap().id;
    let link = |r0, c0, r1, c1, a: &str, sub: &str, tip: Option<&str>, name: Option<&str>| {
        xlcalc::hyperlink::Hyperlink {
            id: 0,
            rect: xlcalc::Rect::new(r0, c0, r1, c1),
            address: a.into(),
            sub_address: sub.into(),
            screen_tip: tip.map(str::to_string),
            name: name.map(str::to_string),
        }
    };
    wb.set_value(CellAddr::new(s, 0, 0), Value::Text("Web".into()));
    wb.hyperlink_add(
        s,
        link(
            0,
            0,
            0,
            0,
            "https://example.com/x y",
            "",
            Some("tip"),
            Some("Web"),
        ),
    );
    wb.set_value(CellAddr::new(s, 1, 0), Value::Text("Here".into()));
    wb.hyperlink_add(s, link(1, 0, 1, 0, "", "Sheet1!B2", None, Some("Here")));
    wb.set_value(CellAddr::new(s, 2, 0), Value::Text("File".into()));
    wb.hyperlink_add(
        s,
        link(2, 0, 2, 0, r"docs\a b.xlsx", "Data!C3", Some("a & b"), None),
    );
    wb.hyperlink_add(
        s,
        link(
            3,
            0,
            4,
            1,
            "mailto:x@y.z?subject=Hi",
            "",
            None,
            Some("Block"),
        ),
    );
    for (tag, wb, pkg) in [
        ("mine", &imp.workbook, Some(&imp.package)),
        ("built", &wb, None),
    ] {
        let saved = xlcalc_xlsx::save(wb, pkg, &Default::default()).unwrap();
        std::fs::write(dir.join(format!("{tag}.xlsx")), &saved.bytes).unwrap();
        std::fs::write(
            dir.join(format!("{tag}.xlsb")),
            xlcalc_xlsx::xlsx_to_xlsb(&saved.bytes).unwrap().0,
        )
        .unwrap();
        std::fs::write(
            dir.join(format!("{tag}.xls")),
            xlcalc_xlsx::xlsx_to_xls(&saved.bytes).unwrap().0,
        )
        .unwrap();
    }
}
