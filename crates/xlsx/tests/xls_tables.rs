use std::io::Read;
use xlcalc_xlsx::{import_bytes, save};

fn data(name: &str) -> Vec<u8> {
    std::fs::read(format!("{}/tests/data/{name}", env!("CARGO_MANIFEST_DIR"))).unwrap()
}

fn tables(bytes: &[u8]) -> Vec<(String, String)> {
    let mut z = zip::ZipArchive::new(std::io::Cursor::new(bytes)).unwrap();
    let names: Vec<String> = z
        .file_names()
        .filter_map(|r| r.ok())
        .map(|c| c.to_string())
        .filter(|n| n.starts_with("xl/tables/table") && n.ends_with(".xml"))
        .collect();
    let mut out = Vec::new();
    for n in names {
        let mut s = String::new();
        z.by_name(&n).unwrap().read_to_string(&mut s).unwrap();
        let g = |k: &str| {
            let i = s.find(&format!(" {k}=\"")).unwrap() + k.len() + 3;
            s[i..i + s[i..].find('"').unwrap()].to_string()
        };
        let cols: Vec<String> = s
            .split("<tableColumn ")
            .skip(1)
            .map(|c| {
                let tag = c.split('>').next().unwrap();
                let a = |k: &str| {
                    tag.find(&format!(" {k}=\""))
                        .map(|i| {
                            let i = i + k.len() + 3;
                            tag[i..i + tag[i..].find('"').unwrap()].to_string()
                        })
                        .unwrap_or_default()
                };
                format!(
                    "{}/{}/{}",
                    a("name"),
                    a("totalsRowFunction"),
                    a("totalsRowLabel")
                )
            })
            .collect();
        out.push((
            format!(
                "{} {} {}",
                g("displayName"),
                g("ref"),
                s.contains("<autoFilter")
            ),
            cols.join(","),
        ));
    }
    out.sort();
    out
}

fn through(bytes: Vec<u8>) -> Vec<u8> {
    let imp = import_bytes(bytes).unwrap();
    save(&imp.workbook, Some(&imp.package), &Default::default())
        .unwrap()
        .bytes
}

#[test]
fn xls_tables_match_the_xlsx_twin() {
    for f in ["xlstbl_a", "xlstbl_b", "xlstbl_c"] {
        let x = tables(&through(data(&format!("{f}.xlsx"))));
        let l = tables(&through(data(&format!("{f}.xls"))));
        assert!(!x.is_empty(), "{f}: no tables in the xlsx");
        assert_eq!(x, l, "{f}");
    }
}

fn table_records(xls: &[u8]) -> Vec<Vec<u8>> {
    let cfb = xlcalc_vbaproject::cfb::Cfb::new(xls.to_vec()).unwrap();
    let d = cfb.read_stream("Workbook").unwrap();
    let mut out = Vec::new();
    let mut p = 0;
    while p + 4 <= d.len() {
        let id = u16::from_le_bytes([d[p], d[p + 1]]);
        let len = u16::from_le_bytes([d[p + 2], d[p + 3]]) as usize;
        if matches!(id, 0x871 | 0x872 | 0x877 | 0x878) {
            out.push(d[p + 4..p + 4 + len].to_vec());
        }
        p += 4 + len;
    }
    out
}

#[test]
fn written_xls_tables_equal_excels() {
    for f in ["xlstbl_a", "xlstbl_b", "xlstbl_c"] {
        let saved = through(data(&format!("{f}.xlsx")));
        let (ours, _) = xlcalc_xlsx::xlsx_to_xls(&saved).unwrap();
        let excel = data(&format!("{f}.xls"));
        assert_eq!(table_records(&ours), table_records(&excel), "{f}");
        assert_eq!(tables(&through(ours)), tables(&saved), "{f} read back");
    }
}

fn calc_records(xls: &[u8]) -> Vec<(u16, Vec<u8>)> {
    let cfb = xlcalc_vbaproject::cfb::Cfb::new(xls.to_vec()).unwrap();
    let d = cfb.read_stream("Workbook").unwrap();
    let mut out = Vec::new();
    let mut p = 0;
    while p + 4 <= d.len() {
        let id = u16::from_le_bytes([d[p], d[p + 1]]);
        let len = u16::from_le_bytes([d[p + 2], d[p + 3]]) as usize;
        let mut r = d[p + 4..p + 4 + len].to_vec();
        match id {
            0x06 => {
                r[16..20].fill(0);

                if r[12..14] == [0xFF, 0xFF] {
                    r[11] = 0;
                }
            }
            0x221 => r[8..12].fill(0),
            0x18 => {}
            _ => {
                p += 4 + len;
                continue;
            }
        }
        out.push((id, r));
        p += 4 + len;
    }
    out
}

#[test]
fn written_xls_lambda_and_spills_equal_excels() {
    let saved = through(data("xlslam.xlsx"));
    let (ours, _) = xlcalc_xlsx::xlsx_to_xls(&saved).unwrap();
    assert_eq!(calc_records(&ours), calc_records(&data("xlslam.xls")));
}
