use std::io::Read;
use xlcalc_xlsx::{import_bytes, save, xlsx_to_xlsb};

fn fixture(ext: &str) -> Vec<u8> {
    std::fs::read(format!(
        "{}/tests/data/filefuzz_l.{ext}",
        env!("CARGO_MANIFEST_DIR")
    ))
    .unwrap()
}

fn names(bytes: &[u8]) -> Vec<String> {
    let z = zip::ZipArchive::new(std::io::Cursor::new(bytes)).unwrap();
    z.file_names()
        .filter_map(|r| r.ok())
        .map(|c| c.to_string())
        .collect()
}

fn part(bytes: &[u8], name: &str) -> String {
    let mut z = zip::ZipArchive::new(std::io::Cursor::new(bytes)).unwrap();
    let mut out = String::new();
    z.by_name(name)
        .unwrap_or_else(|_| panic!("no part {name}"))
        .read_to_string(&mut out)
        .unwrap();
    out
}

fn resaved(bytes: Vec<u8>) -> Vec<u8> {
    let imp = import_bytes(bytes).unwrap();
    save(&imp.workbook, Some(&imp.package), &Default::default())
        .unwrap()
        .bytes
}

#[test]
fn xlsb_keeps_theme_and_properties() {
    let (b, _) = xlsx_to_xlsb(&fixture("xlsx")).unwrap();
    let n = names(&b);
    for want in [
        "xl/theme/theme1.xml",
        "docProps/core.xml",
        "docProps/app.xml",
        "docProps/custom.xml",
    ] {
        assert!(n.iter().any(|x| x == want), "{want} missing from {n:?}");
    }
    for bytes in [resaved(fixture("xlsb")), resaved(b)] {
        let n = names(&bytes);
        for want in [
            "xl/theme/theme1.xml",
            "docProps/core.xml",
            "docProps/custom.xml",
        ] {
            assert!(n.iter().any(|x| x == want), "{want} missing from {n:?}");
        }
        assert!(part(&bytes, "docProps/custom.xml").contains("<property"));
    }
}

fn data(name: &str) -> Vec<u8> {
    std::fs::read(format!("{}/tests/data/{name}", env!("CARGO_MANIFEST_DIR"))).unwrap()
}

fn autofilters(bytes: &[u8]) -> Vec<String> {
    let mut out = Vec::new();
    for i in 1..=12 {
        let mut z = zip::ZipArchive::new(std::io::Cursor::new(bytes)).unwrap();
        let Ok(mut f) = z.by_name(&format!("xl/worksheets/sheet{i}.xml")) else {
            continue;
        };
        let mut s = String::new();
        f.read_to_string(&mut s).unwrap();
        let Some(a) = s.find("<autoFilter") else {
            out.push(String::new());
            continue;
        };
        let e = s[a..].find("</autoFilter>").map_or(s.len(), |e| a + e + 13);
        let mut t = s[a..e].to_string();
        if let Some(u) = t.find(" xr:uid=\"") {
            let q = t[u + 9..].find('"').unwrap();
            t.replace_range(u..u + 9 + q + 1, "");
        }
        let t = t.replace(
            " xmlns:xlrd2=\"http://schemas.microsoft.com/office/spreadsheetml/2017/richdata2\"",
            "",
        );
        out.push(t.replace("\"></filterColumn>", "\"/>"));
    }
    out
}

#[test]
fn xlsb_autofilter() {
    let x = data("filefuzz_af.xlsx");
    let want = autofilters(&x);
    assert!(want.iter().any(|a| a.contains("<top10")), "{want:?}");
    let (b, w) = xlsx_to_xlsb(&x).unwrap();
    assert!(!w.iter().any(|m| m.contains("colour")), "{w:?}");
    let from_excel = autofilters(&resaved(data("filefuzz_af.xlsb")));
    let from_us = autofilters(&resaved(b));
    for (i, a) in want.iter().enumerate() {
        assert_eq!(&from_excel[i], a, "Excel's xlsb, sheet {}", i + 1);
        assert_eq!(&from_us[i], a, "our xlsb, sheet {}", i + 1);
    }
}

#[test]
fn xlsb_icon_and_date_group_filters() {
    let x = data("filefuzz_af3.xlsx");
    let want = autofilters(&x);
    assert!(
        want[0].contains("<iconFilter iconSet=\"3Flags\""),
        "{want:?}"
    );
    assert!(want[1].contains("dateTimeGrouping=\"second\""), "{want:?}");
    assert!(
        want[2].contains("x14:iconFilter iconSet=\"5Boxes\""),
        "{want:?}"
    );
    let (b, w) = xlsx_to_xlsb(&x).unwrap();
    assert!(!w.iter().any(|m| m.contains("filter")), "{w:?}");
    let excel = data("filefuzz_af3.xlsb");
    let (from_excel, from_us) = (
        autofilters(&resaved(excel.clone())),
        autofilters(&resaved(b.clone())),
    );
    for (i, a) in want.iter().enumerate().filter(|(_, a)| !a.is_empty()) {
        let norm = |s: &str| {
            s.replace(
                " xmlns:x14=\"http://schemas.microsoft.com/office/spreadsheetml/2009/9/main\"",
                "",
            )
        };
        assert_eq!(
            norm(&from_excel[i]),
            norm(a),
            "Excel's xlsb, sheet {}",
            i + 1
        );
        assert_eq!(norm(&from_us[i]), norm(a), "our xlsb, sheet {}", i + 1);
        let part = format!("xl/worksheets/sheet{}.bin", i + 1);
        let filt = |bytes: &[u8]| -> Vec<(u16, Vec<u8>)> {
            let recs = biff12(&bin_part(bytes, &part));
            let from = recs.iter().position(|r| r.0 == 161).unwrap();
            let to = recs.iter().position(|r| r.0 == 162).unwrap();
            let mut out = recs[from + 1..to].to_vec();

            for (id, d) in &mut out {
                if *id == 5094 {
                    let g = d[24];
                    if g < 1 {
                        d[6..8].fill(0);
                    }
                    if g < 2 {
                        d[8..10].fill(0);
                        d[18..20].fill(0);
                    }
                }
            }
            out
        };
        assert_eq!(filt(&b), filt(&excel), "records of sheet {}", i + 1);
    }
}

#[test]
fn xlsb_dynamic_filters() {
    let x = data("filefuzz_af2.xlsx");
    let want = &autofilters(&x)[0];
    assert!(want.contains("type=\"M12\"") && want.contains("type=\"yearToDate\""));
    assert_eq!(&autofilters(&resaved(data("filefuzz_af2.xlsb")))[0], want);
    let (b, _) = xlsx_to_xlsb(&x).unwrap();
    assert_eq!(&autofilters(&resaved(b))[0], want);
}

fn sheet_xml(bytes: &[u8], n: u32) -> String {
    part(bytes, &format!("xl/worksheets/sheet{n}.xml"))
}

#[test]
fn xlsb_lambda() {
    let (b, w) = xlsx_to_xlsb(&fixture("xlsx")).unwrap();
    assert!(w.iter().all(|m| !m.contains("computed function")), "{w:?}");
    let call = "_xlfn.LAMBDA(_xlpm.x,_xlpm.x+1)(2)";
    for bytes in [resaved(b), resaved(fixture("xlsb"))] {
        let s = sheet_xml(&bytes, 3);
        assert!(s.contains(call), "{s}");
    }
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

fn bin_part(bytes: &[u8], name: &str) -> Vec<u8> {
    let mut z = zip::ZipArchive::new(std::io::Cursor::new(bytes)).unwrap();
    let mut out = Vec::new();
    z.by_name(name).unwrap().read_to_end(&mut out).unwrap();
    out
}

fn sparks(sheet: &str) -> Vec<String> {
    let mut out = Vec::new();
    for tag in [
        "<x14:sparklineGroup ",
        "<x14:sparklineGroup>",
        "<x14:sparkline>",
    ] {
        let mut rest = sheet;
        while let Some(i) = rest.find(tag) {
            let end = rest[i..].find('>').unwrap() + i;
            let t = &rest[i..end];
            let mut attrs: Vec<&str> = t
                .split(' ')
                .skip(1)
                .filter(|a| !a.starts_with("xr2:") && *a != "lineWeight=\"0.75\"")
                .collect();
            attrs.sort_unstable();
            let body = if tag == "<x14:sparkline>" {
                let e = rest[i..].find("</x14:sparkline>").unwrap() + i;
                rest[end + 1..e].to_string()
            } else {
                String::new()
            };
            out.push(format!("{tag}{attrs:?}{body}"));
            rest = &rest[end..];
        }
    }
    out.sort();
    out
}

#[test]
fn xlsb_sparklines() {
    for name in ["sp", "sp2"] {
        let x = data(&format!("filefuzz_{name}.xlsx"));
        let excel = data(&format!("filefuzz_{name}.xlsb"));
        let (ours, w) = xlsx_to_xlsb(&x).unwrap();
        assert!(w.iter().all(|m| !m.contains("sparkline")), "{w:?}");
        let rec = |b: &[u8], ids: &[u16]| -> Vec<(u16, Vec<u8>)> {
            biff12(&bin_part(b, "xl/worksheets/sheet1.bin"))
                .into_iter()
                .filter(|r| ids.contains(&r.0))
                .collect()
        };

        let groups = |v: Vec<(u16, Vec<u8>)>| -> Vec<Vec<u8>> {
            v.into_iter()
                .map(|(_, mut d)| {
                    for k in 0..8 {
                        let c = 6 + k * 8;
                        if d[c] >> 1 == 3 {
                            d[c + 4..c + 7].fill(0);
                        }
                    }
                    d
                })
                .collect()
        };
        assert_eq!(
            groups(rec(&ours, &[1041])),
            groups(rec(&excel, &[1041])),
            "{name}: groups"
        );

        let strip = |v: Vec<(u16, Vec<u8>)>| -> Vec<Vec<u8>> {
            v.into_iter()
                .map(|(_, mut d)| {
                    d[49] = 0;
                    d[50] = 0;
                    d
                })
                .collect()
        };
        assert_eq!(strip(rec(&ours, &[1043])), strip(rec(&excel, &[1043])));
        let want = sparks(&sheet_xml(&x, 1));
        assert!(want.len() > 3, "{want:?}");
        assert_eq!(
            sparks(&sheet_xml(&resaved(excel), 1)),
            want,
            "{name}: Excel's xlsb"
        );
        assert_eq!(
            sparks(&sheet_xml(&resaved(ours), 1)),
            want,
            "{name}: our xlsb"
        );
    }
}

#[test]
fn xls_series_empty_name() {
    let tx = |b: Vec<u8>| {
        let r = resaved(b);
        let mut z = zip::ZipArchive::new(std::io::Cursor::new(&r)).unwrap();
        let n = (0..z.len())
            .map(|i| z.by_index(i).unwrap().name().unwrap().to_string())
            .find(|n| n.starts_with("xl/charts/chart"))
            .unwrap();
        part(&r, &n).contains("<c:tx><c:v></c:v>")
    };
    assert!(tx(data("filefuzz_ch2.xls")));
    assert!(!tx(data("filefuzz_ch.xls")));
    for (n, want) in [("filefuzz_ch2", true), ("filefuzz_ch", false)] {
        let (x, _) = xlcalc_xlsx::xlsx_to_xls(&data(&format!("{n}.xlsx"))).unwrap();
        assert_eq!(tx(x), want, "{n} via our xls");
    }
}

#[test]
fn xls_precision_as_displayed() {
    let wb = |b: Vec<u8>| part(&resaved(b), "xl/workbook.xml");
    assert!(wb(data("filefuzz_pr.xls")).contains("fullPrecision=\"0\""));
    let (x, _) = xlcalc_xlsx::xlsx_to_xls(&data("filefuzz_pr.xlsx")).unwrap();
    assert!(wb(x).contains("fullPrecision=\"0\""));
}

fn cf_dv(bytes: &[u8], sheet: u32) -> String {
    let s = part(bytes, &format!("xl/worksheets/sheet{sheet}.xml"));
    let mut out = String::new();
    for (open, close) in [
        ("<conditionalFormatting ", "</conditionalFormatting>"),
        ("<dataValidations", "</dataValidations>"),
    ] {
        let mut at = 0;
        while let Some(i) = s[at..].find(open) {
            let a = at + i;
            let e = s[a..].find(close).map_or(s.len(), |e| a + e + close.len());
            out.push_str(&s[a..e]);
            out.push('\n');
            at = e;
        }
    }
    while let Some(a) = out.find("<extLst>") {
        let e = out[a..].find("</extLst>").unwrap() + a + 9;
        out.replace_range(a..e, "");
    }
    let mut out = out.replace("&quot;", "\"").replace("\"></cfRule>", "\"/>");
    while let Some(a) = out.find(" xr:uid=\"") {
        let e = out[a + 9..].find('"').unwrap() + a + 10;
        out.replace_range(a..e, "");
    }
    while let Some(a) = out.find(" dxfId=\"") {
        let e = out[a + 8..].find('"').unwrap() + a + 9;
        out.replace_range(a..e, "");
    }
    out.replace(" iconSet=\"3TrafficLights1\"", "")
}

fn dxf_set(bytes: &[u8]) -> Vec<String> {
    let s = part(bytes, "xl/styles.xml");
    let a = s.find("<dxfs").unwrap_or(0);
    let e = s.find("</dxfs>").unwrap_or(0);
    let mut v: Vec<String> = s[a..e.max(a)]
        .split("<dxf>")
        .skip(1)
        .map(|d| {
            let mut d = d.to_string();
            while let Some(i) = d.find(" tint=\"") {
                let e = d[i + 7..].find('"').unwrap() + i + 8;
                d.replace_range(i..e, "");
            }
            while let Some(i) = d.find(" numFmtId=\"") {
                let e = d[i + 11..].find('"').unwrap() + i + 12;
                d.replace_range(i..e, "");
            }
            d
        })
        .collect();
    v.sort();
    v.dedup();
    v
}

#[test]
fn xlsb_conditional_formats_and_validation() {
    for (name, sheets) in [
        ("cfdv", vec![2, 3]),
        ("cf2", vec![1]),
        ("cf3", vec![1]),
        ("cf4", vec![1]),
        ("dxf", vec![1]),
    ] {
        let x = data(&format!("filefuzz_{name}.xlsx"));
        let from_excel = resaved(data(&format!("filefuzz_{name}.xlsb")));
        let (b, w) = xlsx_to_xlsb(&x).unwrap();
        assert!(
            !w.iter().any(|m| m.contains("conditional")),
            "{name}: {w:?}"
        );
        let from_us = resaved(b);
        for s in sheets {
            let want = cf_dv(&x, s);
            assert!(!want.is_empty(), "{name} sheet {s}");
            same(
                &cf_dv(&from_excel, s),
                &want,
                &format!("{name}: Excel's xlsb, sheet {s}"),
            );
            same(
                &cf_dv(&from_us, s),
                &want,
                &format!("{name}: our xlsb, sheet {s}"),
            );
        }
        assert_eq!(
            dxf_set(&from_excel),
            dxf_set(&x),
            "{name}: dxfs from Excel's xlsb"
        );
        assert_eq!(dxf_set(&from_us), dxf_set(&x), "{name}: our dxfs");
    }
}

#[test]
fn xlsb_tab_colour() {
    let x = data("filefuzz_cfdv.xlsx");
    let (b, _) = xlsx_to_xlsb(&x).unwrap();
    for bytes in [resaved(data("filefuzz_cfdv.xlsb")), resaved(b)] {
        assert!(part(&bytes, "xl/worksheets/sheet1.xml").contains("<tabColor rgb=\"FFFF0000\"/>"));
        let s4 = part(&bytes, "xl/worksheets/sheet4.xml");
        assert!(s4.contains("<tabColor theme=\"4\" tint=\"0.39997"), "{s4}");
    }
}

fn same(got: &str, want: &str, what: &str) {
    for (g, w) in got.lines().zip(want.lines()) {
        assert_eq!(g, w, "{what}");
    }
    assert_eq!(
        got.lines().count(),
        want.lines().count(),
        "{what}: line count"
    );
}

#[test]
fn xlsb_date_group_plain_form() {
    use std::io::Write;
    let x = data("filefuzz_af3.xlsx");
    let mut z = zip::ZipArchive::new(std::io::Cursor::new(&x)).unwrap();
    let mut out = zip::ZipWriter::new(std::io::Cursor::new(Vec::new()));
    for i in 0..z.len() {
        let mut f = z.by_index(i).unwrap();
        let name = f.name().unwrap().to_string();
        let mut b = Vec::new();
        f.read_to_end(&mut b).unwrap();
        if name == "xl/worksheets/sheet2.xml" {
            let s = String::from_utf8(b).unwrap();
            let a = s.find("<autoFilter").unwrap();
            let e = s.find("</autoFilter>").unwrap() + 13;
            let af = "<autoFilter ref=\"A1:F40\"><filterColumn colId=\"1\"><filters><dateGroupItem year=\"2023\" month=\"3\" dateTimeGrouping=\"month\"/></filters></filterColumn><filterColumn colId=\"3\"><filters><dateGroupItem year=\"2023\" month=\"3\" day=\"15\" hour=\"14\" dateTimeGrouping=\"hour\"/></filters></filterColumn></autoFilter>";
            b = format!("{}{}{}", &s[..a], af, &s[e..]).into_bytes();
        }
        out.start_file(name, zip::write::SimpleFileOptions::default())
            .unwrap();
        out.write_all(&b).unwrap();
    }
    let x2 = out.finish().unwrap().into_inner();
    let (b, w) = xlsx_to_xlsb(&x2).unwrap();
    assert!(w.iter().all(|m| !m.contains("filter")), "{w:?}");
    let af = &autofilters(&resaved(b))[1];
    assert!(
        af.contains("dateGroupItem year=\"2023\" month=\"3\" dateTimeGrouping=\"month\""),
        "{af}"
    );
    assert!(
        af.contains("day=\"15\" hour=\"14\" dateTimeGrouping=\"hour\""),
        "{af}"
    );
}
