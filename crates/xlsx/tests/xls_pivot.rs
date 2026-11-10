use xlcalc_vbaproject::cfb::Cfb;

fn data(name: &str) -> Vec<u8> {
    let p = format!("{}/tests/data/xls_pivot/{name}", env!("CARGO_MANIFEST_DIR"));
    std::fs::read(&p).unwrap_or_else(|e| panic!("{p}: {e}"))
}

fn records(d: &[u8]) -> Vec<(u16, Vec<u8>)> {
    let mut out = Vec::new();
    let mut p = 0;
    while p + 4 <= d.len() {
        let id = u16::from_le_bytes([d[p], d[p + 1]]);
        let len = usize::from(u16::from_le_bytes([d[p + 2], d[p + 3]]));
        out.push((id, d[p + 4..p + 4 + len].to_vec()));
        p += 4 + len;
    }
    out
}

const PIVOT_IDS: [u16; 31] = [
    0xB0, 0xB1, 0xB2, 0xB4, 0xB5, 0xB6, 0xC5, 0xF1, 0x100, 0x802, 0x810, 0xD5, 0xE3, 0x51, 0x864,
    0x103, 0x52, 0xD8, 0x122, 0xF2, 0xF6, 0xF7, 0xF8, 0xF9, 0xFB, 0x80E, 0x80F, 0x1BB, 0xC6, 0xC7,
    0x1B5,
];

fn pivot_records(xls: &[u8]) -> Vec<(String, u16, Vec<u8>)> {
    let cfb = Cfb::new(xls.to_vec()).unwrap();
    let mut out = Vec::new();
    for (id, d) in records(&cfb.read_stream("Workbook").unwrap()) {
        if PIVOT_IDS.contains(&id) && id != 0xC6 && id != 0xC7 && id != 0x1BB {
            out.push(("Workbook".to_string(), id, d));
        }
    }
    let mut names: Vec<String> = cfb
        .streams()
        .into_iter()
        .filter(|s| s.starts_with("_SX_DB_CUR/"))
        .collect();
    names.sort();
    for n in names {
        for (id, d) in records(&cfb.read_stream(&n).unwrap()) {
            out.push((n.clone(), id, d));
        }
    }
    out
}

fn hex(d: &[u8]) -> String {
    d.iter()
        .map(|b| format!("{b:02x}"))
        .collect::<Vec<_>>()
        .join(" ")
}

fn compare(case: &str) -> Result<(), String> {
    let (ours, warnings) = xlcalc_xlsx::xlsx_to_xls(&data(&format!("{case}.xlsx"))).unwrap();
    let ours = pivot_records(&ours);
    let excel = pivot_records(&data(&format!("{case}.xls")));
    for i in 0..ours.len().max(excel.len()) {
        if ours.get(i) != excel.get(i) {
            let show = |r: Option<&(String, u16, Vec<u8>)>| {
                r.map_or("(none)".to_string(), |r| {
                    format!("{} {:04x} [{}] {}", r.0, r.1, r.2.len(), hex(&r.2))
                })
            };
            return Err(format!(
                "{case}: record {i} of {}/{} differs ({warnings:?})\n ours : {}\n excel: {}",
                ours.len(),
                excel.len(),
                show(ours.get(i)),
                show(excel.get(i))
            ));
        }
    }
    Ok(())
}

#[test]
fn written_pivot_records_equal_excels() {
    let cases = [
        "xp_one",
        "xp_rc",
        "xp_multi",
        "xp_page",
        "xp_sub",
        "xp_v1",
        "xp_v2",
        "xp_v3",
        "xp_t1",
        "xp_t2",
        "xp_calc",
        "xp_calc2",
        "xp_calc3",
        "xp_calc4",
        "xp_named",
        "xp_table",
        "xp_ext",
        "xp_numgroup",
        "xp_dategroup",
        "xp_dg2",
        "xp_dg3",
        "xp_dg4",
        "xp_ng2",
    ];
    let errs: Vec<String> = cases.iter().filter_map(|c| compare(c).err()).collect();
    assert!(errs.is_empty(), "{}", errs.join("\n"));
}

const CASES: [&str; 16] = [
    "xp_one", "xp_rc", "xp_multi", "xp_page", "xp_sub", "xp_v1", "xp_v2", "xp_v3", "xp_t1",
    "xp_t2", "xp_calc", "xp_calc2", "xp_calc3", "xp_calc4", "xp_named", "xp_table",
];

#[test]
fn read_xls_pivots_match_the_xlsx_twin() {
    for case in CASES {
        let a = xlcalc_xlsx::import_bytes(data(&format!("{case}.xlsx"))).unwrap();
        let b = xlcalc_xlsx::import_bytes(data(&format!("{case}.xls"))).unwrap();
        let (pa, pb) = (a.workbook.pivots(), b.workbook.pivots());
        assert_eq!(pa.tables.len(), 1, "{case}: {:?}", a.warnings);
        assert_eq!(pb.tables.len(), 1, "{case}: {:?}", b.warnings);
        assert_eq!(
            format!("{:?}", pa.tables),
            format!("{:?}", pb.tables),
            "{case}"
        );
        assert_eq!(
            format!("{:?}", pa.caches),
            format!("{:?}", pb.caches),
            "{case}"
        );
    }
}

#[test]
fn pivots_survive_a_trip_through_xls() {
    for case in CASES {
        let a = xlcalc_xlsx::import_bytes(data(&format!("{case}.xlsx"))).unwrap();
        let saved = xlcalc_xlsx::save(&a.workbook, Some(&a.package), &Default::default()).unwrap();
        let (xls, warnings) = xlcalc_xlsx::xlsx_to_xls(&saved.bytes).unwrap();
        assert!(warnings.is_empty(), "{case}: {warnings:?}");
        let b = xlcalc_xlsx::import_bytes(xls).unwrap();
        assert_eq!(
            format!("{:?}", a.workbook.pivots().tables),
            format!("{:?}", b.workbook.pivots().tables),
            "{case}"
        );
    }
}

#[test]
fn selection_grouped_pivots_survive_a_trip_through_xls() {
    let case = "xp_selgroup";
    let a = xlcalc_xlsx::import_bytes(data(&format!("{case}.xlsx"))).unwrap();
    let saved = xlcalc_xlsx::save(&a.workbook, Some(&a.package), &Default::default()).unwrap();
    let (xls, warnings) = xlcalc_xlsx::xlsx_to_xls(&saved.bytes).unwrap();
    assert!(warnings.is_empty(), "{case}: {warnings:?}");
    assert!(!pivot_records(&xls).is_empty(), "{case}");
    let b = xlcalc_xlsx::import_bytes(xls).unwrap();
    assert_eq!(
        format!("{:?}", a.workbook.pivots().tables),
        format!("{:?}", b.workbook.pivots().tables),
        "{case}"
    );
}

#[test]
#[ignore]
fn dump_pivot_case() {
    let case = std::env::var("XP_CASE").unwrap();
    let ours = xlcalc_xlsx::xlsx_to_xls(&data(&format!("{case}.xlsx")))
        .unwrap()
        .0;
    if let Ok(dir) = std::env::var("XP_OUT") {
        std::fs::write(format!("{dir}/{case}_ours.xls"), &ours).unwrap();
    }
    for (tag, x) in [("excel", data(&format!("{case}.xls"))), ("ours", ours)] {
        for (s, id, d) in pivot_records(&x) {
            println!("{tag} {s} {id:04x} [{}] {}", d.len(), hex(&d));
        }
    }
}
