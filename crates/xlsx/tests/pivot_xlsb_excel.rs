use std::io::Read;
use xlcalc_xlsx::{import_bytes, xlsx_to_xlsb};

const CASES: [&str; 9] = [
    "calcitem2",
    "calcitem3",
    "numgroup",
    "dategroup_my",
    "selgroup",
    "lay_tabular",
    "pages",
    "sort_data",
    "datafields",
];

fn fixture(name: &str) -> Vec<u8> {
    let p = format!(
        "{}/tests/data/pivot_xlsb/{name}",
        env!("CARGO_MANIFEST_DIR")
    );
    std::fs::read(&p).unwrap_or_else(|e| panic!("{p}: {e}"))
}

fn parts(b: &[u8]) -> Vec<(String, Vec<u8>)> {
    let mut z = zip::ZipArchive::new(std::io::Cursor::new(b)).unwrap();
    let mut v = Vec::new();
    for i in 0..z.len() {
        let mut f = z.by_index(i).unwrap();
        let mut d = Vec::new();
        f.read_to_end(&mut d).unwrap();
        v.push((f.name().unwrap().to_string(), d));
    }
    v
}

fn records(b: &[u8]) -> Vec<(u32, Vec<u8>)> {
    let mut p = 0;
    let mut out = Vec::new();
    while p < b.len() {
        let mut id = u32::from(b[p]);
        p += 1;
        if id & 0x80 != 0 {
            id = (id & 0x7F) | (u32::from(b[p]) << 7);
            p += 1;
        }
        let (mut len, mut shift) = (0usize, 0);
        loop {
            let x = usize::from(b[p]);
            p += 1;
            len |= (x & 0x7F) << shift;
            shift += 7;
            if x & 0x80 == 0 {
                break;
            }
        }
        out.push((id, b[p..p + len].to_vec()));
        p += len;
    }
    while out.first().is_some_and(|r| matches!(r.0, 37 | 38 | 3072)) {
        out.remove(0);
    }
    out
}

#[test]
fn written_records_match_excel() {
    for case in CASES {
        let (mine, warnings) = xlsx_to_xlsb(&fixture(&format!("{case}.xlsx"))).unwrap();
        assert!(warnings.is_empty(), "{case}: {warnings:?}");
        let (mine, excel) = (parts(&mine), parts(&fixture(&format!("{case}.xlsb"))));
        let mut seen = 0;
        for (name, e) in &excel {
            let pivot =
                name.contains("pivotCache/pivotCache") || name.contains("pivotTables/pivot");
            if !pivot || name.ends_with(".rels") {
                continue;
            }
            let (_, m) = mine
                .iter()
                .find(|(n, _)| n == name)
                .unwrap_or_else(|| panic!("{case}: {name} not written"));
            let (e, m) = (records(e), records(m));
            for i in 0..e.len().max(m.len()) {
                assert_eq!(e.get(i), m.get(i), "{case} {name} record {i}");
            }
            seen += 1;
        }
        assert_eq!(
            seen, 3,
            "{case}: cache definition, records and table expected"
        );
    }
}

#[test]
fn read_pivots_match_the_xlsx() {
    for case in CASES {
        if case == "calcitem2" {
            continue;
        }
        let a = import_bytes(fixture(&format!("{case}.xlsx"))).unwrap();
        let b = import_bytes(fixture(&format!("{case}.xlsb"))).unwrap();
        assert!(b.warnings.is_empty(), "{case}: {:?}", b.warnings);
        let (pa, pb) = (a.workbook.pivots(), b.workbook.pivots());
        assert_eq!(pa.tables.len(), 1, "{case}");
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
