use std::collections::BTreeSet;
use std::path::PathBuf;
use xlcalc::{CellAddr, Value};
use xlcalc_xlsx::{import_bytes, save, Package, SaveOptions};

fn file(name: &str) -> Vec<u8> {
    std::fs::read(
        PathBuf::from(env!("CARGO_MANIFEST_DIR"))
            .join("../../conformance/files")
            .join(name),
    )
    .unwrap()
}

fn cells(xml: &str) -> Vec<(String, String, String, String)> {
    let mut out = Vec::new();
    let mut rest = xml;
    while let Some(i) = rest.find("<c r=\"") {
        rest = &rest[i + 6..];
        let r = &rest[..rest.find('"').unwrap()];
        let tag_end = rest.find('>').unwrap();
        let tag = &rest[..tag_end];
        let attr = |n: &str| {
            tag.find(&format!(" {n}=\""))
                .map(|k| {
                    let v = &tag[k + n.len() + 3..];
                    v[..v.find('"').unwrap()].to_string()
                })
                .unwrap_or_default()
        };
        let t = attr("t");
        let body = if tag.ends_with('/') {
            ""
        } else {
            &rest[tag_end + 1..rest.find("</c>").unwrap()]
        };
        let inner = |n: &str| {
            body.find(&format!("<{n}>"))
                .map(|k| {
                    let v = &body[k + n.len() + 2..];
                    v[..v.find(&format!("</{n}>")).unwrap()].to_string()
                })
                .unwrap_or_default()
        };
        out.push((r.to_string(), t, inner("f"), inner("v")));
    }
    out
}

fn types(rels: &str) -> BTreeSet<String> {
    rels.split("Type=\"")
        .skip(1)
        .map(|s| s[..s.find('"').unwrap()].to_string())
        .collect()
}

fn namespaces(xml: &str) -> BTreeSet<String> {
    let head = &xml[..xml.find('>').map_or(xml.len(), |i| {
        xml[i + 1..].find('>').map_or(i, |j| i + 1 + j)
    })];
    head.split("xmlns")
        .skip(1)
        .filter_map(|s| {
            let s = &s[s.find('"')? + 1..];
            Some(s[..s.find('"')?].to_string())
        })
        .filter(|u| u.contains("purl.oclc.org") || u.contains("schemas.openxmlformats.org"))
        .collect()
}

#[test]
fn strict_save_matches_excel() {
    let mut imp = import_bytes(file("strict_book.xlsx")).unwrap();
    assert!(imp.workbook.date_compat());
    imp.workbook.set_date_compat(false);
    let ours = save(&imp.workbook, Some(&imp.package), &SaveOptions::default()).unwrap();
    let ours = Package::read(&ours.bytes).unwrap();

    let raw = save(&imp.workbook, Some(&imp.package), &SaveOptions::default()).unwrap();
    let raw_part = |name: &str| -> String {
        let mut z = zip::ZipArchive::new(std::io::Cursor::new(&raw.bytes)).unwrap();
        let mut s = String::new();
        std::io::Read::read_to_string(&mut z.by_name(name).unwrap(), &mut s).unwrap();
        s
    };
    let theirs_bytes = file("strict_book_strict.xlsx");
    let theirs_raw = |name: &str| -> String {
        let mut z = zip::ZipArchive::new(std::io::Cursor::new(&theirs_bytes)).unwrap();
        let mut s = String::new();
        std::io::Read::read_to_string(&mut z.by_name(name).unwrap(), &mut s).unwrap();
        s
    };
    assert!(ours.part("xl/workbook.xml").is_some());

    let wb = raw_part("xl/workbook.xml");
    assert!(wb.contains("conformance=\"strict\""), "{wb}");
    assert_eq!(
        namespaces(&wb),
        namespaces(&theirs_raw("xl/workbook.xml")),
        "workbook namespaces"
    );

    let a = cells(&raw_part("xl/worksheets/sheet1.xml"));
    let b = cells(&theirs_raw("xl/worksheets/sheet1.xml"));
    assert_eq!(a.len(), b.len());
    for (x, y) in a.iter().zip(&b) {
        assert_eq!(x, y);
    }

    for part in [
        "xl/worksheets/sheet1.xml",
        "xl/styles.xml",
        "docProps/app.xml",
        "xl/sharedStrings.xml",
    ] {
        assert_eq!(
            namespaces(&raw_part(part)),
            namespaces(&theirs_raw(part)),
            "{part}"
        );
    }
    for part in ["_rels/.rels", "xl/_rels/workbook.xml.rels"] {
        let mine = types(&raw_part(part));
        let excel = types(&theirs_raw(part));

        assert!(mine.is_subset(&excel), "{part}: {mine:?} vs {excel:?}");
        assert!(
            mine.iter().all(|t| !t.contains("2006/relationships/")
                || t.contains("package/2006")
                || t.ends_with("vmlDrawing")),
            "{part}: {mine:?}"
        );
    }

    let st = raw_part("xl/styles.xml");
    assert!(st.contains("<start") && st.contains("<end"), "{st}");
    assert!(!st.contains("<left") && !st.contains("<right"), "{st}");
}

fn shape_skeleton(xml: &str) -> String {
    fn cut(s: &mut String, open: &str, close: &str) {
        while let Some(i) = s.find(open) {
            let from = i + open.len();
            let j = s[from..]
                .find(close)
                .map_or(s.len(), |j| from + j + close.len());
            s.replace_range(i..j, "");
        }
    }
    let mut s = xml.to_string();
    cut(&mut s, "<?xml", "?>");

    while let Some(i) = s.find(" xmlns") {
        let q = s[i..].find("=\"").map_or(s.len(), |q| i + q + 2);
        let j = s[q..].find('"').map_or(s.len(), |j| q + j + 1);
        s.replace_range(i..j, "");
    }
    cut(&mut s, "<a:extLst><a:ext uri=\"{FF2B5EF4", "</a:extLst>");
    cut(&mut s, " xr:uid=\"", "\"");
    cut(&mut s, " lang=\"", "\"");
    let mut s = s.replace("\r\n", "\n").replace("></xdr:cNvPr>", "/>");
    for tag in ["rowOff", "colOff"] {
        let open = format!("<xdr:{tag}>");
        let close = format!("</xdr:{tag}>");
        let mut o = String::new();
        let mut rest = s.as_str();
        while let Some(i) = rest.find(&open) {
            o.push_str(&rest[..i + open.len()]);
            rest = &rest[rest[i..].find(&close).unwrap() + i..];
        }
        o.push_str(rest);
        s = o;
    }
    s
}

#[test]
fn strict_save_writes_note_shapes_like_excel() {
    let mut imp = import_bytes(file("strict_book.xlsx")).unwrap();
    imp.workbook.set_date_compat(false);
    let raw = save(&imp.workbook, Some(&imp.package), &SaveOptions::default()).unwrap();
    let part = |bytes: &[u8], name: &str| -> String {
        let mut z = zip::ZipArchive::new(std::io::Cursor::new(bytes)).unwrap();
        let mut s = String::new();
        std::io::Read::read_to_string(&mut z.by_name(name).unwrap(), &mut s).unwrap();
        s
    };
    let theirs = file("strict_book_strict.xlsx");

    let ours_d = shape_skeleton(&part(&raw.bytes, "xl/drawings/drawing1.xml"));
    let excel_d = shape_skeleton(&part(&theirs, "xl/drawings/drawing1.xml"));
    assert_eq!(ours_d, excel_d);

    let ours_c = shape_skeleton(&part(&raw.bytes, "xl/comments1.xml"));
    let excel_c = shape_skeleton(&part(&theirs, "xl/comments1.xml"));
    assert_eq!(ours_c, excel_c);

    let sheet = part(&raw.bytes, "xl/worksheets/sheet1.xml");
    let d = sheet.find("<drawing ").expect("drawing element");
    assert!(d < sheet.find("<legacyDrawing").unwrap());
    let rels = part(&raw.bytes, "xl/worksheets/_rels/sheet1.xml.rels");
    assert!(rels.contains("http://purl.oclc.org/ooxml/officeDocument/relationships/drawing\""));
    let ct = part(&raw.bytes, "[Content_Types].xml");
    assert!(ct.contains("/xl/drawings/drawing1.xml"), "{ct}");

    let mut again = import_bytes(theirs.clone()).unwrap();
    again.workbook.set_date_compat(false);
    let saved = save(
        &again.workbook,
        Some(&again.package),
        &SaveOptions::default(),
    )
    .unwrap();
    let d2 = part(&saved.bytes, "xl/drawings/drawing1.xml");
    assert_eq!(d2.matches("name=\"Comment ").count(), 1, "{d2}");
}

#[test]
fn strict_file_reads_back() {
    let imp = import_bytes(file("strict_book_strict.xlsx")).unwrap();
    let wb = &imp.workbook;
    assert!(!wb.date_compat());
    let s = wb.sheet_by_name("S").unwrap().id;
    let want: [f64; 18] = [
        0.0,
        45000.5,
        0.5,
        1.5,
        45000.0,
        0.123456789,
        45000.00001,
        2958466.0,
        2958465.0,
        45000.0,
        90.0,
        1.5,
        45000.0,
        45000.0,
        0.3333333333333333,
        0.00001157,
        -1.0,
        1.25,
    ];
    for (i, x) in want.iter().enumerate() {
        let v = wb.value(CellAddr::new(s, i as u32, 0));
        assert!(
            matches!(v, Value::Number(n) if n.to_bits() == x.to_bits()),
            "A{}: {v:?}",
            i + 1
        );
    }

    let a = save(wb, Some(&imp.package), &SaveOptions::default()).unwrap();
    let again = import_bytes(a.bytes.clone()).unwrap();
    let b = save(
        &again.workbook,
        Some(&again.package),
        &SaveOptions::default(),
    )
    .unwrap();
    let mut za = zip::ZipArchive::new(std::io::Cursor::new(&a.bytes)).unwrap();
    let mut zb = zip::ZipArchive::new(std::io::Cursor::new(&b.bytes)).unwrap();
    let mut sa = String::new();
    let mut sb = String::new();
    std::io::Read::read_to_string(
        &mut za.by_name("xl/worksheets/sheet1.xml").unwrap(),
        &mut sa,
    )
    .unwrap();
    std::io::Read::read_to_string(
        &mut zb.by_name("xl/worksheets/sheet1.xml").unwrap(),
        &mut sb,
    )
    .unwrap();
    assert_eq!(sa, sb);
    assert!(sa.contains("t=\"d\""));

    let mut back = import_bytes(a.bytes).unwrap();
    back.workbook.set_date_compat(true);
    let t = save(&back.workbook, Some(&back.package), &SaveOptions::default()).unwrap();
    let mut zt = zip::ZipArchive::new(std::io::Cursor::new(&t.bytes)).unwrap();
    let mut st = String::new();
    std::io::Read::read_to_string(
        &mut zt.by_name("xl/worksheets/sheet1.xml").unwrap(),
        &mut st,
    )
    .unwrap();
    assert!(
        !st.contains("t=\"d\"") && !st.contains("purl.oclc.org"),
        "{}",
        &st[..400]
    );
    let mut ws = String::new();
    std::io::Read::read_to_string(&mut zt.by_name("xl/workbook.xml").unwrap(), &mut ws).unwrap();
    assert!(
        !ws.contains("conformance") && !ws.contains("purl.oclc.org"),
        "{ws}"
    );
}
