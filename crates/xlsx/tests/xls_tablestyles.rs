use std::io::Read;
use xlcalc_xlsx::{import_bytes, save, xlsx_to_xls};

fn styles_xml(bytes: &[u8]) -> String {
    let mut z = zip::ZipArchive::new(std::io::Cursor::new(bytes)).unwrap();
    let mut s = String::new();
    z.by_name("xl/styles.xml")
        .unwrap()
        .read_to_string(&mut s)
        .unwrap();
    s
}

fn lines(s: &str) -> Vec<String> {
    let dxfs: Vec<&str> = s
        .split("<dxf>")
        .skip(1)
        .map(|d| d.split("</dxf>").next().unwrap())
        .collect();
    let ts = s.split("<tableStyles").nth(1).unwrap();
    let attr = |tag: &str, k: &str| -> String {
        tag.find(&format!(" {k}=\""))
            .map(|i| {
                let i = i + k.len() + 3;
                tag[i..i + tag[i..].find('"').unwrap()].to_string()
            })
            .unwrap_or_default()
    };
    let mut out = vec![format!(
        "default {} {}",
        attr(ts, "defaultTableStyle"),
        attr(ts, "defaultPivotStyle")
    )];
    for st in ts.split("<tableStyle ").skip(1) {
        let head = st.split('>').next().unwrap();
        let flags = format!(
            "{}{}",
            if attr(head, "pivot") == "0" { 0 } else { 1 },
            if attr(head, "table") == "0" { 0 } else { 1 }
        );
        let name = attr(head, "name");
        for el in st.split("<tableStyleElement ").skip(1) {
            let tag = el.split("/>").next().unwrap();
            let dx = attr(tag, "dxfId")
                .parse::<usize>()
                .ok()
                .map(|i| dxfs[i])
                .unwrap_or("");

            let mut dx = dx.to_string();
            while let Some(i) = dx.find("tint=\"") {
                let j = i + 6 + dx[i + 6..].find('"').unwrap();
                let v: f64 = dx[i + 6..j].parse().unwrap();
                dx.replace_range(i..=j, &format!("tint~{v:.3}"));
            }
            out.push(format!(
                "{name}|{flags}|{}|{}|{dx}",
                attr(tag, "type"),
                attr(tag, "size")
            ));
        }
    }
    out
}

#[test]
fn table_styles_survive_xls() {
    let src = std::fs::read(format!(
        "{}/tests/data/xls_tablestyles.xlsx",
        env!("CARGO_MANIFEST_DIR")
    ))
    .unwrap();
    let want = lines(&styles_xml(&src));
    assert!(want.len() > 30);
    let imp = import_bytes(src).unwrap();
    let saved = save(&imp.workbook, Some(&imp.package), &Default::default()).unwrap();
    let (xls, _) = xlsx_to_xls(&saved.bytes).unwrap();
    let back = import_bytes(xls).unwrap();
    let again = save(&back.workbook, Some(&back.package), &Default::default()).unwrap();
    assert_eq!(lines(&styles_xml(&again.bytes)), want);
}
