use std::io::Read;

use xlcalc::{CellAddr, FilterSpec, Rect, SheetId, Workbook};
use xlcalc_xlsx::{import_bytes, save, xlsx_to_xlsb, SaveOptions};

const AREA: Rect = Rect {
    r0: 0,
    c0: 0,
    r1: 5,
    c1: 1,
};

fn book() -> (Workbook, SheetId) {
    let mut wb = Workbook::new();
    let s = wb.active_sheet();
    let rows = [
        ("name", "qty"),
        ("Apple", "1"),
        ("Banana", "5"),
        ("Cherry", "3"),
        ("Date", "10"),
        ("Elder", "8"),
    ];
    for (r, (a, b)) in rows.iter().enumerate() {
        wb.set_formula(CellAddr::new(s, r as u32, 0), a).unwrap();
        wb.set_formula(CellAddr::new(s, r as u32, 1), b).unwrap();
    }
    (wb, s)
}

fn filter(wb: &mut Workbook, s: SheetId, field: u32, c1: &str, op: &str, code: i32) {
    wb.autofilter(
        s,
        AREA,
        None,
        FilterSpec {
            field,
            criteria1: Some(c1),
            operator: Some(op),
            criteria2: None,
            op_code: code,
            rich: false,
        },
    )
    .unwrap();
}

fn part(bytes: &[u8], name: &str) -> String {
    let mut z = zip::ZipArchive::new(std::io::Cursor::new(bytes)).unwrap();
    let mut s = String::new();
    z.by_name(name).unwrap().read_to_string(&mut s).unwrap();
    s
}

fn hidden(wb: &Workbook, s: SheetId) -> Vec<u32> {
    (0..6).filter(|&r| wb.row_hidden(s, r)).collect()
}

fn bytes_of(wb: &Workbook) -> Vec<u8> {
    save(wb, None, &SaveOptions::default()).unwrap().bytes
}

#[test]
fn criteria_survive_a_save() {
    for (field, c1, op, code, xml) in [
        (
            1,
            "Apple",
            "and",
            0,
            "<filters><filter val=\"Apple\"/></filters>",
        ),
        (1, "=", "and", 0, "<filters blank=\"1\"/>"),
        (
            1,
            "<>",
            "and",
            0,
            "<customFilters><customFilter operator=\"notEqual\" val=\" \"/></customFilters>",
        ),
        (
            1,
            "=*an*",
            "and",
            0,
            "<customFilters><customFilter val=\"*an*\"/></customFilters>",
        ),
        (
            2,
            ">4",
            "and",
            0,
            "<customFilters><customFilter operator=\"greaterThan\" val=\"4\"/></customFilters>",
        ),
        (2, "3", "top10", 3, "<top10 val=\"3\" filterVal=\"5\"/>"),
        (
            2,
            "50",
            "bottom10pct",
            6,
            "<top10 top=\"0\" percent=\"1\" val=\"50\" filterVal=\"3\"/>",
        ),
        (
            2,
            "33",
            "dynamic",
            11,
            "<dynamicFilter type=\"aboveAverage\" val=\"5.4\"/>",
        ),
        (2, "255", "cellcolor", 8, "<colorFilter dxfId=\"0\"/>"),
    ] {
        let (mut wb, s) = book();
        filter(&mut wb, s, field, c1, op, code);
        let before = (hidden(&wb, s), format!("{:?}", wb.autofilter_view(s, None)));
        let bytes = bytes_of(&wb);
        let sheet = part(&bytes, "xl/worksheets/sheet1.xml");
        let want = format!(
            "<autoFilter ref=\"A1:B6\"><filterColumn colId=\"{}\">{xml}</filterColumn></autoFilter>",
            field - 1
        );
        assert!(sheet.contains(&want), "{c1} {op}: {sheet}");
        assert!(sheet.contains("<sheetPr filterMode=\"1\"/>"), "{sheet}");
        let names = part(&bytes, "xl/workbook.xml");
        assert!(
            names.contains("name=\"_xlnm._FilterDatabase\" localSheetId=\"0\" hidden=\"1\""),
            "{names}"
        );
        let back = import_bytes(bytes).unwrap();
        assert!(back.warnings.is_empty(), "{:?}", back.warnings);
        let wb = back.workbook;
        let s = wb.sheet_by_name("Sheet1").unwrap().id;
        assert!(wb.autofilter_mode(s) && wb.filter_mode(s), "{c1} {op}");
        let after = (hidden(&wb, s), format!("{:?}", wb.autofilter_view(s, None)));
        assert_eq!(after, before, "{c1} {op}");
        let again = part(&bytes_of(&wb), "xl/worksheets/sheet1.xml");
        assert!(again.contains(&want), "{c1} {op}: {again}");
    }
}

#[test]
fn buttons_alone_are_no_filter_mode() {
    let (mut wb, s) = book();
    wb.autofilter_toggle(s, AREA).unwrap();
    let bytes = bytes_of(&wb);
    let sheet = part(&bytes, "xl/worksheets/sheet1.xml");
    assert!(sheet.contains("<autoFilter ref=\"A1:B6\"/>"), "{sheet}");
    assert!(!sheet.contains("filterMode"), "{sheet}");
    let wb = import_bytes(bytes).unwrap().workbook;
    let s = wb.sheet_by_name("Sheet1").unwrap().id;
    assert!(wb.autofilter_mode(s) && !wb.filter_mode(s));
}

#[test]
fn removed_filter_is_not_written() {
    let (mut wb, s) = book();
    filter(&mut wb, s, 1, "Apple", "and", 0);
    let loaded = import_bytes(bytes_of(&wb)).unwrap();
    let mut wb = loaded.workbook;
    let s = wb.sheet_by_name("Sheet1").unwrap().id;
    wb.autofilter_off(s, None).unwrap();
    let bytes = save(&wb, Some(&loaded.package), &SaveOptions::default())
        .unwrap()
        .bytes;
    let sheet = part(&bytes, "xl/worksheets/sheet1.xml");
    assert!(!sheet.contains("autoFilter"), "{sheet}");
    assert!(!sheet.contains("filterMode"), "{sheet}");
}

fn table_book() -> (Workbook, SheetId) {
    let (mut wb, s) = book();
    for (r, name) in ["Apple", "Banana", "Cherry", "Date", "Elder"]
        .iter()
        .enumerate()
    {
        wb.set_input(CellAddr::new(s, r as u32 + 1, 0), name)
            .unwrap();
    }
    wb.add_table("T", s, AREA).unwrap();
    (wb, s)
}

#[test]
fn table_criteria_survive_a_save() {
    for (field, c1, op, code, xml) in [
        (
            1,
            "Apple",
            "and",
            0,
            "<filters><filter val=\"Apple\"/></filters>",
        ),
        (
            2,
            ">4",
            "and",
            0,
            "<customFilters><customFilter operator=\"greaterThan\" val=\"4\"/></customFilters>",
        ),
        (2, "3", "top10", 3, "<top10 val=\"3\" filterVal=\"5\"/>"),
        (
            2,
            "33",
            "dynamic",
            11,
            "<dynamicFilter type=\"aboveAverage\" val=\"5.4\"/>",
        ),
        (2, "255", "cellcolor", 8, "<colorFilter dxfId=\"0\"/>"),
    ] {
        let (mut wb, s) = table_book();
        wb.autofilter(
            s,
            AREA,
            Some("T"),
            FilterSpec {
                field,
                criteria1: Some(c1),
                operator: Some(op),
                criteria2: None,
                op_code: code,
                rich: false,
            },
        )
        .unwrap();
        let before = hidden(&wb, s);
        let bytes = bytes_of(&wb);
        let table = part(&bytes, "xl/tables/table1.xml");
        let want = format!(
            "<autoFilter ref=\"A1:B6\"><filterColumn colId=\"{}\">{xml}</filterColumn></autoFilter>",
            field - 1
        );
        assert!(table.contains(&want), "{c1} {op}: {table}");
        let (binary, warnings) = xlsx_to_xlsb(&bytes).unwrap();
        assert!(warnings.is_empty(), "{warnings:?}");
        let from_binary = import_bytes(binary).unwrap().workbook;
        let again = part(&bytes_of(&from_binary), "xl/tables/table1.xml");
        assert!(again.contains(&want), "xlsb {c1} {op}: {again}");
        let sheet = part(&bytes, "xl/worksheets/sheet1.xml");
        assert!(!sheet.contains("filterMode"), "{sheet}");
        assert!(!sheet.contains("<autoFilter"), "{sheet}");
        let loaded = import_bytes(bytes).unwrap();
        assert!(loaded.warnings.is_empty(), "{:?}", loaded.warnings);
        let wb = loaded.workbook;
        let s = wb.sheet_by_name("Sheet1").unwrap().id;
        assert!(wb.autofilter_object_mode(s, Some("T")), "{c1} {op}");
        assert_eq!(hidden(&wb, s), before, "{c1} {op}");
        assert!(wb.autofilter_view(s, Some("T")).unwrap().fields[field as usize - 1].is_some());
        for source in [None, Some(&loaded.package)] {
            let bytes = save(&wb, source, &SaveOptions::default()).unwrap().bytes;
            let again = part(&bytes, "xl/tables/table1.xml");
            assert!(again.contains(&want), "{c1} {op}: {again}");
        }
    }
}

#[test]
fn table_filter_cleared_or_removed() {
    let (mut wb, s) = table_book();
    wb.autofilter(
        s,
        AREA,
        Some("T"),
        FilterSpec {
            field: 1,
            criteria1: Some("Apple"),
            ..FilterSpec::default()
        },
    )
    .unwrap();
    let loaded = import_bytes(bytes_of(&wb)).unwrap();
    let mut wb = loaded.workbook;
    let s = wb.sheet_by_name("Sheet1").unwrap().id;
    let clear = FilterSpec {
        field: 1,
        ..FilterSpec::default()
    };
    wb.autofilter(s, AREA, Some("T"), clear).unwrap();
    let table = |wb: &Workbook| {
        let bytes = save(wb, Some(&loaded.package), &SaveOptions::default())
            .unwrap()
            .bytes;
        (part(&bytes, "xl/tables/table1.xml"), bytes)
    };
    let (xml, _) = table(&wb);
    assert!(xml.contains("<autoFilter ref=\"A1:B6\"/>"), "{xml}");
    wb.autofilter_off(s, Some("T")).unwrap();
    let (xml, bytes) = table(&wb);
    assert!(!xml.contains("autoFilter"), "{xml}");
    let wb = import_bytes(bytes).unwrap().workbook;
    assert!(!wb.table_by_name("T").unwrap().show_filter);
}
