use std::io::Read;
use xlcalc_xlsx::{import_bytes, save, xlsx_to_xls, xlsx_to_xlsb};

fn fixture(n: &str) -> Vec<u8> {
    std::fs::read(format!(
        "{}/tests/data/filefuzz_k.{n}",
        env!("CARGO_MANIFEST_DIR")
    ))
    .unwrap()
}

fn part(bytes: &[u8], name: &str) -> Vec<u8> {
    let mut z = zip::ZipArchive::new(std::io::Cursor::new(bytes)).unwrap();
    let mut out = Vec::new();
    z.by_name(name).unwrap().read_to_end(&mut out).unwrap();
    out
}

fn biff12(b: &[u8]) -> Vec<(u16, Vec<u8>)> {
    let (mut p, mut out) = (0, Vec::new());
    while p < b.len() {
        let mut id = u16::from(b[p]);
        p += 1;
        if id & 0x80 != 0 {
            id = (id & 0x7F) | (u16::from(b[p]) << 7);
            p += 1;
        }
        let (mut n, mut sh) = (0usize, 0);
        loop {
            let c = b[p];
            p += 1;
            n |= usize::from(c & 0x7F) << sh;
            sh += 7;
            if c & 0x80 == 0 {
                break;
            }
        }
        out.push((id, b[p..p + n].to_vec()));
        p += n;
    }
    out
}

fn saved_xlsx(file: &str) -> Vec<u8> {
    let imp = import_bytes(fixture(file)).unwrap();
    save(&imp.workbook, Some(&imp.package), &Default::default())
        .unwrap()
        .bytes
}

#[test]
fn newer_errors_are_rich_values() {
    let x = saved_xlsx("xlsx");
    let sheet = String::from_utf8(part(&x, "xl/worksheets/sheet1.xml")).unwrap();
    assert!(!sheet.contains("#SPILL!"), "{sheet}");
    let a30 = sheet.split("<c r=\"A30\"").nth(1).unwrap();
    assert!(a30.starts_with(" t=\"e\" cm=\"1\" vm=\"1\">"), "{a30}");
    assert!(a30.contains("<v>#VALUE!</v>"));
    let meta = String::from_utf8(part(&x, "xl/metadata.xml")).unwrap();
    assert!(meta.contains("XLRICHVALUE") && meta.contains("<valueMetadata count=\"1\">"));
    let rv = String::from_utf8(part(&x, "xl/richData/rdrichvalue.xml")).unwrap();
    assert!(rv.contains("<rv s=\"0\"><v>0</v><v>8</v><v>"), "{rv}");
    let st = String::from_utf8(part(&x, "xl/richData/rdrichvaluestructure.xml")).unwrap();
    assert!(st.contains("rwOffset") && st.contains("errorType"));
    let rels = String::from_utf8(part(&x, "xl/_rels/workbook.xml.rels")).unwrap();
    assert!(rels.matches("RichValue").count() >= 3);

    import_bytes(x).unwrap();
}

#[test]
fn table_totals_from_xlsb() {
    let x = saved_xlsx("xlsb");
    let t = String::from_utf8(part(&x, "xl/tables/table1.xml")).unwrap();
    assert!(t.contains("totalsRowLabel=\"Total\""), "{t}");
    assert!(t.contains("totalsRowFunction=\"sum\""), "{t}");
    assert!(
        t.contains("<tableColumn id=\"1\"") && t.contains("<tableColumn id=\"2\""),
        "{t}"
    );
}

#[test]
fn xlsb_table_totals() {
    let (b, _) = xlsx_to_xlsb(&saved_xlsx("xlsx")).unwrap();
    let recs = biff12(&part(&b, "xl/tables/table1.bin"));
    let list = &recs.iter().find(|r| r.0 == 0x157).unwrap().1;
    assert_eq!(
        u32::from_le_bytes(list[28..32].try_into().unwrap()),
        1,
        "crwTotals"
    );
    assert_eq!(
        u32::from_le_bytes(list[32..36].try_into().unwrap()),
        1,
        "flags"
    );
    let af = &recs.iter().find(|r| r.0 == 0xA1).unwrap().1;
    assert_eq!(
        u32::from_le_bytes(af[4..8].try_into().unwrap()),
        4,
        "filter last row"
    );
    let cols: Vec<_> = recs.iter().filter(|r| r.0 == 0x15B).collect();
    assert_eq!(
        u32::from_le_bytes(cols[1].1[4..8].try_into().unwrap()),
        6,
        "sum"
    );
    assert!(cols[0]
        .1
        .windows(9)
        .any(|w| w == "T\0o\0t\0a\0l".as_bytes()));
}

#[test]
fn xls_pane() {
    let (x, _) = xlsx_to_xls(&saved_xlsx("xlsx")).unwrap();
    let cfb = xlcalc_vbaproject::cfb::Cfb::new(x).unwrap();
    let d = cfb.read_stream("Workbook").unwrap();
    let mut p = 0;
    let mut panes = Vec::new();
    while p + 4 <= d.len() {
        let id = u16::from_le_bytes([d[p], d[p + 1]]);
        let n = usize::from(u16::from_le_bytes([d[p + 2], d[p + 3]]));
        if id == 0x41 {
            panes.push(d[p + 4..p + 4 + n].to_vec());
        }
        p += 4 + n;
    }
    assert_eq!(panes.len(), 1);
    assert_eq!(panes[0], [2, 0, 3, 0, 3, 0, 2, 0, 0, 0]);
}

fn sheet1(x: &[u8]) -> String {
    String::from_utf8(part(x, "xl/worksheets/sheet1.xml")).unwrap()
}

#[test]
fn xls_print_setup() {
    let s = sheet1(&saved_xlsx("xls"));
    assert!(
        s.contains("<printOptions gridLines=\"1\" horizontalCentered=\"1\"/>"),
        "{s}"
    );
    assert!(
        s.contains("header=\"0.1388888888888889\"") || s.contains("header=\"0.13"),
        "{s}"
    );
    assert!(s.contains("pageOrder=\"overThenDown\""), "{s}");
    assert!(s.contains("blackAndWhite=\"1\""), "{s}");
    assert!(
        s.contains("<rowBreaks") && s.contains("<brk id=\"24\""),
        "{s}"
    );
    assert!(
        s.contains("<colBreaks") && s.contains("<brk id=\"6\""),
        "{s}"
    );
}

#[test]
fn xlsb_sheet_state_round_trips() {
    let (b, w) = xlsx_to_xlsb(&saved_xlsx("xlsx")).unwrap();
    assert!(
        w.iter()
            .all(|m| !m.contains("mergeCells") && !m.contains("LET")),
        "{w:?}"
    );
    let back = import_bytes(b).unwrap();
    let x = save(&back.workbook, Some(&back.package), &Default::default())
        .unwrap()
        .bytes;
    let s = sheet1(&x);
    assert!(s.contains("<mergeCell ref=\"E1:F2\"/>"), "{s}");
    assert!(s.contains("outlineLevel=\"2\""), "{s}");
    assert!(s.contains("summaryBelow=\"0\""), "{s}");
    assert!(
        s.contains("xSplit=\"2\"") && s.contains("state=\"frozenSplit\""),
        "{s}"
    );
    assert!(s.contains("showGridLines=\"0\""), "{s}");
    assert!(s.contains("<brk id=\"24\""), "{s}");
    assert!(
        s.contains("<f>_xlfn.LET(_xlpm.x,2,_xlpm.x*3)</f>") || s.contains("LET("),
        "{s}"
    );
    let wbx = String::from_utf8(part(&x, "xl/workbook.xml")).unwrap();
    assert!(wbx.contains("lockStructure=\"1\""), "{wbx}");
    assert!(wbx.contains("_xlnm.Print_Area"), "{wbx}");
    let prot = String::from_utf8(part(&x, "xl/worksheets/sheet4.xml")).unwrap();
    assert!(prot.contains("<sheetProtection sheet=\"1\""), "{prot}");
}
