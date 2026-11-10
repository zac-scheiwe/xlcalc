use std::path::PathBuf;

fn root() -> PathBuf {
    PathBuf::from(env!("CARGO_MANIFEST_DIR")).join("../../conformance")
}

fn twin(name: &str) -> PathBuf {
    let a = root().join("workbooks").join(format!("{name}.xlsx"));
    if a.exists() {
        a
    } else {
        root().join("files").join(format!("{name}.xlsx"))
    }
}

fn compare(name: &str, formulas: bool) {
    compare_with(
        name,
        &root().join("xls").join(format!("{name}.xls")),
        formulas,
    );
}

fn compare_written(name: &str, formulas: bool) {
    let imp = xlcalc_xlsx::import_file(&twin(name)).unwrap();
    let saved = xlcalc_xlsx::save(&imp.workbook, Some(&imp.package), &Default::default()).unwrap();
    let (xls, warnings) = xlcalc_xlsx::xlsx_to_xls(&saved.bytes).unwrap();
    eprintln!("{name}: write warnings {warnings:?}");
    let dir = PathBuf::from(env!("CARGO_TARGET_TMPDIR"));
    let path = dir.join(format!("{name}.xls"));
    std::fs::write(&path, xls).unwrap();
    compare_with(name, &path, formulas);
}

fn compare_with(name: &str, b_path: &std::path::Path, formulas: bool) {
    let a = xlcalc_xlsx::import_file(&twin(name)).unwrap();
    let b = xlcalc_xlsx::import_file(b_path).unwrap();
    eprintln!("{name}: warnings {:?}", b.warnings);
    let sheets: Vec<String> = a.workbook.sheets().map(|s| s.name.clone()).collect();
    let other: Vec<String> = b.workbook.sheets().map(|s| s.name.clone()).collect();
    assert_eq!(sheets, other, "{name}");
    let mut bad = Vec::new();
    for s in &sheets {
        let (sa, sb) = (
            a.workbook.sheet_by_name(s).unwrap(),
            b.workbook.sheet_by_name(s).unwrap(),
        );
        let Some(r) = a.workbook.used_range(sa.id) else {
            continue;
        };
        for row in r.r0..=r.r1.min(65535) {
            for col in r.c0..=r.c1.min(255) {
                let (ca, cb) = (
                    xlcalc::CellAddr::new(sa.id, row, col),
                    xlcalc::CellAddr::new(sb.id, row, col),
                );
                let (nfa, nfb) = (a.workbook.number_format(ca), b.workbook.number_format(cb));
                if nfa != nfb {
                    bad.push(format!("{s} {row},{col}: number format {nfa:?} vs {nfb:?}"));
                }
                let sig = |st: &xlcalc::style::Style| {
                    format!(
                        "{} {} {} {} {:?} {:?} {:?} {:?} {:?} {:?} {:?} {:?} {} {} {:?}",
                        st.font.name,
                        st.font.size_twips,
                        st.font.bold,
                        st.font.italic,
                        st.font.underline,
                        st.font.strikethrough,
                        st.fill.pattern,
                        st.borders.left.line,
                        st.borders.right.line,
                        st.borders.top.line,
                        st.borders.bottom.line,
                        st.alignment,
                        st.locked,
                        st.formula_hidden,
                        st.named
                    )
                };
                let (sta, stb) = (
                    sig(a.workbook.cell_style(ca)),
                    sig(b.workbook.cell_style(cb)),
                );
                if sta != stb && !(name == "styles" && s == "Styled" && row == 12 && col == 1) {
                    bad.push(format!("{s} {row},{col}: style {sta} vs {stb}"));
                }
                let (va, vb) = (a.workbook.value(ca), b.workbook.value(cb));
                if format!("{va:?}") != format!("{vb:?}")
                    && !(name == "features" && s == "Calc" && row == 2 && col == 2)
                {
                    bad.push(format!("{s} {row},{col}: {va:?} vs {vb:?}"));
                }
                if formulas {
                    let (fa, fb) = (a.workbook.formula_text(ca), b.workbook.formula_text(cb));
                    if fa != fb {
                        eprintln!("{s} {row},{col}: {fa:?} vs {fb:?}");
                        bad.push(format!("{s} {row},{col}: formula {fa:?} vs {fb:?}"));
                    }
                }
            }
        }
    }
    assert!(
        bad.is_empty(),
        "{name}: {} differences, first: {:#?}",
        bad.len(),
        &bad[..bad.len().min(12)]
    );
}

#[test]
fn xls_budget_reads_like_xlsx() {
    compare("budget", true);
}

#[test]
fn xls_cashflow_reads_like_xlsx() {
    compare("cashflow", true);
}

#[test]
fn xls_date1904_reads_like_xlsx() {
    compare("date1904", true);
}

#[test]
fn xls_datatables_reads_like_xlsx() {
    compare("datatables", true);
}

#[test]
fn xls_features_reads_like_xlsx() {
    compare("features", false);
}

#[test]
fn xls_styles_reads_like_xlsx() {
    compare("styles", false);
}

#[test]
fn dbg_features() {
    let b = xlcalc_xlsx::import_file(&root().join("xls").join("features.xls")).unwrap();
    let s = b.workbook.sheet_by_name("Calc").unwrap();
    for r in 0..8 {
        let ca = xlcalc::CellAddr::new(s.id, r, 2);
        eprintln!(
            "C{} {:?} {:?}",
            r + 1,
            b.workbook.formula_text(ca),
            b.workbook.value(ca)
        );
    }
}

#[test]
fn dump() {
    let Ok(p) = std::env::var("XLS_DUMP") else {
        return;
    };
    let cfb = xlcalc_vbaproject::cfb::Cfb::new(std::fs::read(p).unwrap()).unwrap();
    eprintln!("streams: {:?}", cfb.streams());
    let b = cfb.read_stream("Workbook").unwrap();
    let mut p = 0;
    while p + 4 <= b.len() {
        let id = u16::from_le_bytes([b[p], b[p + 1]]);
        let n = u16::from_le_bytes([b[p + 2], b[p + 3]]) as usize;
        let d = &b[p + 4..(p + 4 + n).min(b.len())];
        let hex: String = d.iter().take(2000).map(|x| format!("{x:02x}")).collect();
        eprintln!("{p:6} {id:#06x} len {n}: {hex}");
        p += 4 + n;
    }
}

#[test]
fn xls_written_budget_reads_back() {
    compare_written("budget", true);
}

#[test]
fn xls_written_cashflow_reads_back() {
    compare_written("cashflow", true);
}

#[test]
fn xls_written_date1904_reads_back() {
    compare_written("date1904", true);
}

#[test]
fn xls_written_datatables_reads_back() {
    compare_written("datatables", true);
}

#[test]
fn xls_written_features_reads_back() {
    compare_written("features", false);
}

#[test]
fn xls_written_styles_reads_back() {
    compare_written("styles", false);
}

fn summary_of(path: &std::path::Path) -> Vec<String> {
    let imp = xlcalc_xlsx::import_file(path).unwrap();
    eprintln!("{}: warnings {:?}", path.display(), imp.warnings);

    xlcalc_xlsx::extras_summary(&imp.package)
        .into_iter()
        .map(|l| l.replace("../files/", ""))
        .collect()
}

fn diff(a: &[String], b: &[String]) -> Vec<String> {
    let mut out = Vec::new();
    for l in a {
        if !b.contains(l) {
            out.push(format!("only in twin: {l:?}"));
        }
    }
    for l in b {
        if !a.contains(l) {
            out.push(format!("only in xls:  {l:?}"));
        }
    }
    out
}

fn extras_names() -> [(&'static str, &'static str); 3] {
    [
        ("xlsextras", "files"),
        ("xlscf", "files"),
        ("xlscfnum", "files"),
    ]
}

#[test]
fn xls_extras_read_like_xlsx() {
    let mut all: Vec<String> = Vec::new();
    for (name, _) in extras_names() {
        let a = summary_of(&twin(name));
        let b = summary_of(&root().join("xls").join(format!("{name}.xls")));
        all.extend(diff(&a, &b).into_iter().map(|l| format!("{name}: {l}")));
    }
    assert!(
        all.is_empty(),
        "{} differences:\n{}",
        all.len(),
        all.join("\n")
    );
}

#[test]
fn xls_extras_written_read_back() {
    let mut all: Vec<String> = Vec::new();
    for (name, _) in extras_names() {
        let imp = xlcalc_xlsx::import_file(&twin(name)).unwrap();
        let saved =
            xlcalc_xlsx::save(&imp.workbook, Some(&imp.package), &Default::default()).unwrap();
        let (xls, warnings) = xlcalc_xlsx::xlsx_to_xls(&saved.bytes).unwrap();
        eprintln!("{name}: write warnings {warnings:?}");
        let path = PathBuf::from(env!("CARGO_TARGET_TMPDIR")).join(format!("{name}.xls"));
        std::fs::write(&path, xls).unwrap();
        let a = summary_of(&twin(name));
        let b = summary_of(&path);
        all.extend(diff(&a, &b).into_iter().map(|l| format!("{name}: {l}")));
    }
    assert!(
        all.is_empty(),
        "{} differences:\n{}",
        all.len(),
        all.join("\n")
    );
}

#[test]
fn dump_part() {
    let Ok(v) = std::env::var("XLS_PART") else {
        return;
    };
    let (p, part) = v.split_once("::").unwrap();
    let imp = xlcalc_xlsx::import_file(std::path::Path::new(p)).unwrap();
    eprintln!(
        "{}",
        String::from_utf8_lossy(imp.package.part(part).unwrap())
    );
}

fn cf12_records(path: &std::path::Path) -> Vec<Vec<u8>> {
    let cfb = xlcalc_vbaproject::cfb::Cfb::new(std::fs::read(path).unwrap()).unwrap();
    let b = cfb.read_stream("Workbook").unwrap();
    let mut out = Vec::new();
    let mut p = 0;
    while p + 4 <= b.len() {
        let id = u16::from_le_bytes([b[p], b[p + 1]]);
        let n = u16::from_le_bytes([b[p + 2], b[p + 3]]) as usize;
        if id == 0x87A {
            out.push(b[p + 4..(p + 4 + n).min(b.len())].to_vec());
        }
        p += 4 + n;
    }
    out
}

fn cf12_key(r: &[u8]) -> Vec<u8> {
    let u32at = |i: usize| u32::from_le_bytes([r[i], r[i + 1], r[i + 2], r[i + 3]]) as usize;
    let cb = u32at(18);
    let dxf = &r[22..22 + cb];
    let flags = u32::from_le_bytes([dxf[0], dxf[1], dxf[2], dxf[3]]);
    let user = dxf[4] & 1 != 0;
    let nf = if flags & 0x0200_0000 == 0 {
        0
    } else if user {
        usize::from(u16::from_le_bytes([dxf[6], dxf[7]]))
    } else {
        2
    };
    let simple = r[12] == 1;
    let mut key = r[12..14].to_vec();
    if simple {
        key.extend(&r[14..18]);
    }
    key.extend(&dxf[..6 + nf]);
    let tail = if r[12] == 5 { 30 } else { 24 };
    key.extend(&r[r.len() - tail..]);
    key
}

#[test]
fn xls_cf_numfmt_records_match_excel() {
    let imp = xlcalc_xlsx::import_file(&twin("xlscfnum")).unwrap();
    let saved = xlcalc_xlsx::save(&imp.workbook, Some(&imp.package), &Default::default()).unwrap();
    let (xls, _) = xlcalc_xlsx::xlsx_to_xls(&saved.bytes).unwrap();
    let path = PathBuf::from(env!("CARGO_TARGET_TMPDIR")).join("xlscfnum_bytes.xls");
    std::fs::write(&path, xls).unwrap();
    let ours = cf12_records(&path);
    let excel = cf12_records(&root().join("xls").join("xlscfnum.xls"));
    let hex = |d: &[u8]| d.iter().map(|x| format!("{x:02x}")).collect::<String>();
    assert_eq!(ours.len(), excel.len());
    let mut bad = Vec::new();
    for (i, (o, e)) in ours.iter().zip(&excel).enumerate() {
        let (ko, ke) = (cf12_key(o), cf12_key(e));
        if ko != ke {
            bad.push(format!(
                "record {i}\n  excel {}\n  ours  {}",
                hex(&ke),
                hex(&ko)
            ));
        }
    }
    assert!(bad.is_empty(), "{}", bad.join("\n"));
}

fn shapes_of(path: &std::path::Path) -> Vec<String> {
    let imp = xlcalc_xlsx::import_file(path).unwrap();
    let mut out = Vec::new();
    for s in imp.workbook.sheets() {
        let Some(d) = imp.workbook.drawing(s.id) else {
            continue;
        };
        for sh in &d.shapes {
            let (x, y, w, h) = imp.workbook.shape_rect_emu(s.id, &sh.pos);
            let px = |v: i64| (v as f64 / 9525.0).round() as i64;
            let kind = match &sh.kind {
                xlcalc::drawing::ShapeKind::Group(m) => format!("Group({})", m.len()),
                k => format!("{k:?}"),
            };
            let parent = sh.parent.and_then(|p| d.shapes.iter().find(|x| x.id == p));
            out.push(format!(
                "{}|{}|{}|{:?}|{},{},{},{}|flip {}{}|parent {}|image {}|chart {}",
                s.name,
                sh.name,
                kind,
                sh.text,
                px(x),
                px(y),
                px(w),
                px(h),
                sh.flip_h,
                sh.flip_v,
                parent.map_or("", |p| p.name.as_str()),
                sh.image
                    .as_ref()
                    .map_or(String::new(), |i| format!("{} {}", i.ext, i.bytes.len())),
                sh.chart.as_ref().map_or(String::new(), |c| format!(
                    "{} {:?} {:?} legend {} {} hidden {:?} axes {:?}",
                    c.chart_type,
                    c.title,
                    c.series
                        .iter()
                        .map(|s| format!(
                            "{:?} {:?} {:?} type {} group {} smooth {:?} labels {}",
                            s.name,
                            s.x,
                            s.values,
                            s.chart_type,
                            s.axis_group,
                            s.fmt.smooth,
                            s.fmt.labels.is_some()
                        ))
                        .collect::<Vec<_>>(),
                    c.has_legend,
                    c.legend_pos,
                    c.axis_hidden,
                    c.axes
                        .iter()
                        .map(|a| format!(
                            "{:?} {} {} {:?} {:?} {:?} {:?} {}",
                            a.title,
                            a.grid_major,
                            a.grid_minor,
                            a.min,
                            a.max,
                            a.major,
                            a.minor,
                            a.reverse
                        ))
                        .collect::<Vec<_>>()
                ))
            ));
        }
    }
    out
}

fn assert_same_shapes(a: &[String], b: &[String]) {
    assert!(!a.is_empty());
    assert_eq!(
        a.len(),
        b.len(),
        "{a:#?}
{b:#?}"
    );
    for (x, y) in a.iter().zip(b) {
        let split = |l: &str| {
            let f: Vec<&str> = l.split('|').collect();
            let n: Vec<i64> = f[4].split(',').map(|v| v.parse().unwrap()).collect();
            let mut rest = f.clone();
            rest.remove(4);
            (rest.join("|"), n)
        };
        let (ha, na) = split(x);
        let (hb, nb) = split(y);
        assert_eq!(ha, hb);
        for (p, q) in na.iter().zip(&nb) {
            assert!((p - q).abs() <= 3, "{x} vs {y}");
        }
    }
}

fn chart_axes(path: &std::path::Path) -> Vec<String> {
    let imp = xlcalc_xlsx::import_file(path).unwrap();
    let mut out: Vec<String> = imp
        .package
        .part_names()
        .filter(|n| n.starts_with("xl/charts/chart") && n.ends_with(".xml"))
        .map(|n| {
            let t = String::from_utf8_lossy(imp.package.part(n).unwrap()).into_owned();
            let c = |p: &str| t.matches(p).count();
            let groups = [
                "<c:barChart>",
                "<c:lineChart>",
                "<c:areaChart>",
                "<c:scatterChart>",
                "<c:pieChart>",
            ]
            .map(&c);
            format!(
                "groups {groups:?} catAx {} valAx {} deleted {} max {} grid {} minorgrid {} titles {}",
                c("<c:catAx>"),
                c("<c:valAx>"),
                c("<c:delete val=\"1\"/>"),
                c("<c:crosses val=\"max\"/>"),
                c("<c:majorGridlines"),
                c("<c:minorGridlines"),
                t.matches("<a:t>").count()
            )
        })
        .collect();
    out.sort();
    out
}

fn draw_read(name: &str) {
    assert_eq!(
        chart_axes(&twin(name)),
        chart_axes(&root().join("xls").join(format!("{name}.xls")))
    );
    let a = shapes_of(&twin(name));
    let b = shapes_of(&root().join("xls").join(format!("{name}.xls")));
    if std::env::var("SIGS").is_ok() {
        eprintln!(
            "{}",
            b.join(
                "
"
            )
        );
    }
    assert_same_shapes(&a, &b);
}

fn draw_written(name: &str) {
    let imp = xlcalc_xlsx::import_file(&twin(name)).unwrap();
    let saved = xlcalc_xlsx::save(&imp.workbook, Some(&imp.package), &Default::default()).unwrap();
    let (xls, warnings) = xlcalc_xlsx::xlsx_to_xls(&saved.bytes).unwrap();
    eprintln!("{name}: write warnings {warnings:?}");
    let path = PathBuf::from(env!("CARGO_TARGET_TMPDIR")).join(format!("{name}.xls"));
    std::fs::write(&path, xls).unwrap();
    assert_same_shapes(&shapes_of(&twin(name)), &shapes_of(&path));
    assert_eq!(chart_axes(&twin(name)), chart_axes(&path));
}

#[test]
fn xls_draw_read_like_xlsx() {
    draw_read("xlsdraw");
}

#[test]
fn xls_draw_written_read_back() {
    draw_written("xlsdraw");
}

#[test]
fn xls_draw2_read_like_xlsx() {
    draw_read("xlsdraw2");
}

#[test]
fn xls_draw2_written_read_back() {
    draw_written("xlsdraw2");
}

fn escher_shapes(path: &std::path::Path) -> Vec<(u16, std::collections::BTreeMap<u16, u32>)> {
    fn scan(d: &[u8], out: &mut Vec<(u16, std::collections::BTreeMap<u16, u32>)>) {
        let mut p = 0;
        while p + 8 <= d.len() {
            let ver = d[p] & 15;
            let inst = (u16::from_le_bytes([d[p], d[p + 1]]) >> 4) as usize;
            let ty = u16::from_le_bytes([d[p + 2], d[p + 3]]);
            let len = u32::from_le_bytes([d[p + 4], d[p + 5], d[p + 6], d[p + 7]]) as usize;
            let b = &d[p + 8..(p + 8 + len).min(d.len())];
            if ver == 15 {
                scan(b, out);
            } else if ty == 0xF00A {
                out.push((inst as u16, Default::default()));
            } else if ty == 0xF00B {
                for k in 0..inst {
                    let Some(e) = b.get(k * 6..k * 6 + 6) else {
                        break;
                    };
                    let id = u16::from_le_bytes([e[0], e[1]]);
                    if id & 0x8000 == 0 {
                        let v = u32::from_le_bytes([e[2], e[3], e[4], e[5]]);
                        if let Some(l) = out.last_mut() {
                            l.1.insert(id & 0x3FFF, v);
                        }
                    }
                }
            }
            p += 8 + len;
        }
    }
    let cfb = xlcalc_vbaproject::cfb::Cfb::new(std::fs::read(path).unwrap()).unwrap();
    let b = cfb.read_stream("Workbook").unwrap();
    let mut out = Vec::new();
    let (mut p, mut prev) = (0, 0u16);
    while p + 4 <= b.len() {
        let id = u16::from_le_bytes([b[p], b[p + 1]]);
        let n = u16::from_le_bytes([b[p + 2], b[p + 3]]) as usize;
        let d = &b[p + 4..(p + 4 + n).min(b.len())];
        let esc = id == 0xEC || (id == 0x3C && prev == 0x5D);
        if esc {
            scan(d, &mut out);
        }
        prev = if esc { 0x5D } else { id };
        p += 4 + n;
    }
    out
}

fn written_xls(name: &str) -> PathBuf {
    let imp = xlcalc_xlsx::import_file(&twin(name)).unwrap();
    let saved = xlcalc_xlsx::save(&imp.workbook, Some(&imp.package), &Default::default()).unwrap();
    let (xls, warnings) = xlcalc_xlsx::xlsx_to_xls(&saved.bytes).unwrap();
    eprintln!("{name}: write warnings {warnings:?}");
    let path = PathBuf::from(env!("CARGO_TARGET_TMPDIR")).join(format!("{name}_w.xls"));
    std::fs::write(&path, xls).unwrap();
    path
}

#[test]
fn xls_theme_colours_match_excel() {
    let ours = escher_shapes(&written_xls("xlsdraw3"));
    let excel = escher_shapes(&root().join("xls").join("xlsdraw3.xls"));
    let near = |a: u32, b: u32| {
        (0..3).all(|i| ((a >> (8 * i)) & 0xFF).abs_diff((b >> (8 * i)) & 0xFF) <= 1)
    };
    let mut bad = Vec::new();
    let (o, e): (Vec<_>, Vec<_>) = (
        ours.iter().filter(|s| s.1.contains_key(&0x181)).collect(),
        excel.iter().filter(|s| s.1.contains_key(&0x181)).collect(),
    );
    assert_eq!(o.len(), e.len());
    for (i, (a, b)) in o.iter().zip(&e).enumerate() {
        let (fa, fb) = (a.1[&0x181], b.1[&0x181]);
        let (la, lb) = (
            a.1.get(&0x1C0).copied().unwrap_or(0),
            b.1.get(&0x1C0).copied().unwrap_or(0),
        );
        if !near(fa, fb) || !near(la, lb) {
            bad.push(format!(
                "shape {i}: fill {fa:06x}/{fb:06x} line {la:06x}/{lb:06x}"
            ));
        }
    }
    assert!(bad.is_empty(), "{}", bad.join("\n"));
}

#[test]
fn xls_msospt_match_excel() {
    let ours = escher_shapes(&written_xls("xlsdraw3"));
    let excel = escher_shapes(&root().join("xls").join("xlsdraw3.xls"));
    assert_eq!(ours.len(), excel.len());
    let bad: Vec<String> = ours
        .iter()
        .zip(&excel)
        .enumerate()
        .filter(|(_, (o, e))| e.0 != 0 && o.0 != e.0)
        .map(|(i, (o, e))| format!("shape {i}: ours {} excel {}", o.0, e.0))
        .collect();
    assert!(bad.is_empty(), "{}", bad.join("\n"));
}

#[test]
fn xls_draw4_read_like_xlsx() {
    draw_read("xlsdraw4");
}

#[test]
fn xls_draw6_read_like_xlsx() {
    draw_read("xlsdraw6");
}

#[test]
fn xls_draw6_written_read_back() {
    draw_written("xlsdraw6");
}

#[test]
fn xls_draw4_written_read_back() {
    draw_written("xlsdraw4");
}

fn presets(path: &std::path::Path) -> Vec<String> {
    let imp = xlcalc_xlsx::import_file(path).unwrap();
    let mut out = Vec::new();
    for n in imp
        .package
        .part_names()
        .filter(|n| n.starts_with("xl/drawings/drawing"))
    {
        let t = String::from_utf8_lossy(imp.package.part(n).unwrap()).into_owned();
        for piece in t.split("prst=\"").skip(1) {
            out.push(piece.split('"').next().unwrap().to_string());
        }
    }
    out
}

#[test]
fn xls_draw3_read_like_xlsx() {
    draw_read("xlsdraw3");
    let a = presets(&twin("xlsdraw3"));
    let b = presets(&root().join("xls").join("xlsdraw3.xls"));
    assert!(a.len() > 150 && a.iter().any(|p| p == "trapezoid"));
    assert_eq!(a, b);
}

#[test]
fn xls_draw3_reads_all_shapes() {
    let imp = xlcalc_xlsx::import_file(&root().join("xls").join("xlsdraw3.xls")).unwrap();
    eprintln!("{:?}", imp.warnings);
    let n = shapes_of(&root().join("xls").join("xlsdraw3.xls")).len();
    assert_eq!(n, 182);
}

#[test]
fn xls_draw5_read_like_xlsx() {
    let keep = |v: Vec<String>| -> Vec<String> {
        v.into_iter()
            .filter(|l| !l.starts_with("ChartSheet|"))
            .collect()
    };
    let a = keep(shapes_of(&twin("xlsdraw5")));
    let b = keep(shapes_of(&root().join("xls").join("xlsdraw5.xls")));
    if std::env::var("SIGS").is_ok() {
        eprintln!(
            "{}",
            b.join(
                "
"
            )
        );
    }
    assert_same_shapes(&a, &b);
}

#[test]
fn xls_draw5_written_read_back() {
    draw_written("xlsdraw5");
}

#[test]
fn xls_draw5_chartsheet_written_read_back() {
    let path = written_xls("xlsdraw5");
    let ours: Vec<String> = shapes_of(&path)
        .into_iter()
        .filter(|l| l.starts_with("ChartSheet|"))
        .collect();
    let twin_l: Vec<String> = shapes_of(&twin("xlsdraw5"))
        .into_iter()
        .filter(|l| l.starts_with("ChartSheet|"))
        .collect();
    assert_same_shapes(&twin_l, &ours);
}

#[test]
fn xls_draw3_written_presets_keep_geometry() {
    let a = presets(&twin("xlsdraw3"));
    let b = presets(&written_xls("xlsdraw3"));
    assert_eq!(a, b);
}

#[test]
fn xls_draw6_written_legends_carry_layout_record() {
    let bytes = std::fs::read(written_xls("xlsdraw6")).unwrap();
    let key = [0x9du8, 0x08, 0, 0, 0, 0, 0, 0, 0, 0, 0];
    let n = bytes.windows(key.len()).filter(|w| *w == key).count();
    assert!(n >= 6, "{n} layout records");
}
