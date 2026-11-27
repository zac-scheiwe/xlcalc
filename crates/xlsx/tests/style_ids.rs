use xlcalc::cfv::{CfKind, CfRule, Dxf};
use xlcalc::{CellAddr, Rect, SheetId, Workbook};

fn cell(r: u32, c: u32) -> Rect {
    Rect::new(r, c, r, c)
}

fn part(wb: &Workbook, name: &str) -> String {
    let saved = xlcalc_xlsx::save(wb, None, &Default::default()).unwrap();
    part_of(&saved.bytes, name)
}

fn part_of(bytes: &[u8], name: &str) -> String {
    let mut zip = zip::ZipArchive::new(std::io::Cursor::new(bytes)).unwrap();
    let mut out = String::new();
    std::io::Read::read_to_string(&mut zip.by_name(name).unwrap(), &mut out).unwrap();
    out
}

fn num_fmts(styles: &str) -> Vec<(u32, String)> {
    styles
        .split("<numFmt ")
        .skip(1)
        .map(|t| {
            let attr = |k: &str| t.split(&format!("{k}=\"")).nth(1).unwrap();
            let value = |k: &str| attr(k).split('"').next().unwrap().replace("&quot;", "\"");
            (value("numFmtId").parse().unwrap(), value("formatCode"))
        })
        .collect()
}

fn xf_formats(styles: &str) -> Vec<u32> {
    let xfs = styles.split("<cellXfs").nth(1).unwrap();
    let xfs = xfs.split("</cellXfs>").next().unwrap();
    xfs.split("<xf ")
        .skip(1)
        .map(|t| {
            let v = t.split("numFmtId=\"").nth(1).unwrap();
            v.split('"').next().unwrap().parse().unwrap()
        })
        .collect()
}

fn probe_book() -> (Workbook, SheetId) {
    let mut wb = Workbook::new();
    let first = wb.sheets().next().map(|s| s.id);
    let s = first.unwrap_or_else(|| wb.add_sheet("Sheet1").unwrap());
    for (r, c, f) in [
        (0, 0, "0.000"),
        (0, 0, "0.0000"),
        (0, 1, "0.00000"),
        (0, 2, "0.000000"),
        (0, 2, "General"),
        (2, 0, "yyyy"),
        (1, 0, "yyyy-mm"),
        (5, 0, "$#,##0.00"),
        (6, 0, "$#,##0;[Red]-$#,##0"),
        (
            7,
            0,
            r#"_($* #,##0.00_);_($* (#,##0.00);_($* "-"??_);_(@_)"#,
        ),
        (8, 0, r#"_-$* #,##0.00_-;-$* #,##0.00_-;_-$* "-"??_-;_-@_-"#),
        (9, 0, "YYYY"),
        (10, 0, r#"0.0"x""#),
        (11, 0, r"0.0\x"),
    ] {
        wb.set_number_format(s, cell(r, c), f);
    }
    (wb, s)
}

#[test]
fn ids_are_slots() {
    let (wb, _) = probe_book();
    let styles = part(&wb, "xl/styles.xml");
    let ids: Vec<u32> = num_fmts(&styles).iter().map(|f| f.0).collect();

    assert_eq!(ids, [6, 44, 165, 166, 168, 169, 170, 171, 172, 173]);

    let xfs = xf_formats(&styles);
    let at = |id| xfs.iter().position(|&x| x == id).unwrap();
    assert!(at(165) < at(166) && at(168) < at(169) && at(169) < at(170));
    assert!(!xfs.contains(&164) && !xfs.contains(&167));
}

#[test]
fn reload_renumbers_and_xls_keeps_all() {
    let (wb, _) = probe_book();
    let saved = xlcalc_xlsx::save(&wb, None, &Default::default()).unwrap();
    let again = xlcalc_xlsx::import_bytes(saved.bytes.clone()).unwrap();
    let resaved =
        xlcalc_xlsx::save(&again.workbook, Some(&again.package), &Default::default()).unwrap();
    let styles = part_of(&resaved.bytes, "xl/styles.xml");
    let ids: Vec<u32> = num_fmts(&styles).iter().map(|f| f.0).collect();
    assert_eq!(ids, [6, 44, 164, 165, 166, 167, 168, 169, 170, 171]);
    assert!(xf_formats(&styles)
        .iter()
        .all(|id| *id < 164 || ids.contains(id)));

    let (xls, _) = xlcalc_xlsx::xlsx_to_xls_of(&saved.bytes, &wb, false).unwrap();
    let back = xlcalc_xlsx::import_bytes(xls).unwrap();
    let table = back.workbook.number_format_table();
    assert_eq!(table.codes().len(), 10, "{:?}", table.codes());
    assert_eq!(table.codes()[0], "0.000");
    assert_eq!(table.codes()[3], "0.000000");
    for (r, c) in [(0, 1), (1, 0), (2, 0), (11, 0)] {
        let at = CellAddr::new(back.workbook.sheets().next().unwrap().id, r, c);
        let first = again.workbook.sheets().next().unwrap().id;
        assert_eq!(
            back.workbook.number_format(at),
            again
                .workbook
                .number_format(CellAddr { sheet: first, ..at }),
        );
    }
}

fn rule(wb: &mut Workbook, s: SheetId, at: Rect) -> u32 {
    let rule = CfRule {
        id: 0,
        kind: CfKind::Expression,
        prio: 0,
        stop: true,
        f1: None,
        f2: None,
        dxf: Dxf::default(),
    };
    wb.cfv_sheet_mut(s).add_on_range(rule, &[at])
}

fn dxf_codes(styles: &str) -> Vec<String> {
    let dxfs = styles.split("<dxfs").nth(1).unwrap();
    let dxfs = dxfs.split("</dxfs>").next().unwrap();
    dxfs.split("formatCode=\"")
        .skip(1)
        .map(|t| t.split('"').next().unwrap().to_string())
        .collect()
}

#[test]
fn dxfs_are_newest_first() {
    let mut wb = Workbook::new();
    let s1 = wb.sheet_at(0).id;
    let s2 = wb.add_sheet("Sheet2").unwrap();
    let a = rule(&mut wb, s1, cell(0, 0));
    let b = rule(&mut wb, s1, cell(0, 1));
    wb.cfv_put_number_format(s1, a, "0.0");
    wb.cfv_put_number_format(s1, b, "0.00");
    let c = rule(&mut wb, s2, cell(0, 0));
    wb.cfv_put_number_format(s2, c, "0.000");

    wb.cfv_put_number_format(s1, a, "0.0000");
    let saved = xlcalc_xlsx::save(&wb, None, &Default::default()).unwrap();
    let styles = part_of(&saved.bytes, "xl/styles.xml");
    assert_eq!(dxf_codes(&styles), ["0.0000", "0.000", "0.00"]);

    let again = xlcalc_xlsx::import_bytes(saved.bytes).unwrap();
    let resaved =
        xlcalc_xlsx::save(&again.workbook, Some(&again.package), &Default::default()).unwrap();
    let styles = part_of(&resaved.bytes, "xl/styles.xml");
    assert_eq!(dxf_codes(&styles), ["0.000", "0.00", "0.0000"]);
}

#[test]
fn unused_cell_formats_are_dropped() {
    let (wb, _) = probe_book();
    let saved = xlcalc_xlsx::save(&wb, None, &Default::default()).unwrap();
    let before = xf_formats(&part_of(&saved.bytes, "xl/styles.xml"));
    let mut again = xlcalc_xlsx::import_bytes(saved.bytes).unwrap();
    let first = again.workbook.sheet_at(0).id;
    again
        .workbook
        .set_number_format(first, cell(0, 0), "General");
    let resaved =
        xlcalc_xlsx::save(&again.workbook, Some(&again.package), &Default::default()).unwrap();
    let after = xf_formats(&part_of(&resaved.bytes, "xl/styles.xml"));
    assert_eq!(after.len(), before.len() - 1);
    let reread = xlcalc_xlsx::import_bytes(resaved.bytes).unwrap();
    let sheet = reread.workbook.sheet_at(0).id;
    for (r, c) in [(0, 1), (1, 0), (2, 0), (5, 0), (10, 0), (11, 0)] {
        assert_eq!(
            reread.workbook.number_format(CellAddr::new(sheet, r, c)),
            again.workbook.number_format(CellAddr::new(first, r, c)),
        );
    }
}
