use std::path::{Path, PathBuf};
use xlcalc::{CellAddr, Value, Workbook};
use xlcalc_xlsx::{
    check_package, import_bytes, read_controls, save, Control, ControlKind, Package, SaveOptions,
};

fn root() -> PathBuf {
    PathBuf::from(env!("CARGO_MANIFEST_DIR")).join("../..")
}

fn show(v: &Value) -> String {
    match v {
        Value::Number(n) => format!("n:{:016X}", n.to_bits()),
        v => format!("{v:?}"),
    }
}

pub fn snapshot(wb: &Workbook) -> Vec<String> {
    let values = !wb.settings().iteration;
    let mut out = Vec::new();
    let s = wb.settings();
    out.push(format!(
        "settings {:?} it={} {} {} ds={:?} pad={}",
        s.mode,
        s.iteration,
        s.max_iterations,
        s.max_change,
        s.date_system,
        wb.precision_as_displayed()
    ));
    out.push(format!("base style {:?}", wb.base_style()));
    let t = wb.style_table();
    out.push(format!("theme {:?}", t.theme));
    for n in &t.named {
        out.push(format!("named style {n:?}"));
    }
    for d in &t.dxfs {
        out.push(format!("dxf {d}"));
    }
    for sh in wb.sheets() {
        out.push(format!("sheet {}", sh.name));
        out.push(format!("hidden rows {:?}", wb.hidden_rows(sh.id)));
        out.push(format!("row heights {:?}", wb.row_custom_heights(sh.id)));
        let Some(used) = wb.used_range(sh.id) else {
            continue;
        };
        for r in used.r0..=used.r1 {
            for c in used.c0..=used.c1 {
                let a = CellAddr::new(sh.id, r, c);
                let v = wb.value(a);
                let f = wb.formula_text(a);
                let fmt = wb.number_format(a);
                let style = wb.cell_style(a);
                if matches!(v, Value::Blank) && f.is_none() && style == wb.base_style() {
                    continue;
                }
                out.push(format!(
                    "{}!R{r}C{c} {} {:?} {fmt:?} {:?}",
                    sh.name,
                    if values { show(&v) } else { String::new() },
                    f,
                    if values {
                        wb.display_text(a)
                    } else {
                        String::new()
                    }
                ));
                if style != wb.base_style() {
                    out.push(format!("{}!R{r}C{c} style {style:?}", sh.name));
                }
            }
        }
    }
    let mut names: Vec<String> = wb
        .names()
        .iter()
        .map(|n| {
            format!(
                "name {} {:?} {}",
                n.name,
                n.scope.and_then(|s| wb.sheet_position(s)),
                n.refers_to
            )
        })
        .collect();
    names.sort();
    out.extend(names);
    for t in wb.tables() {
        out.push(format!(
            "table {} {:?} {:?} h={} t={} {:?}",
            t.name,
            wb.sheet_position(t.sheet),
            t.rect,
            t.headers,
            t.totals,
            t.columns
        ));
    }
    out
}

fn same_lines(a: &[String], b: &[String], what: &str) {
    if a == b {
        return;
    }
    let diffs: Vec<String> = a
        .iter()
        .zip(b)
        .filter(|(x, y)| x != y)
        .take(8)
        .map(|(x, y)| format!("  - {x}\n  + {y}"))
        .collect();
    panic!(
        "{what}: {} vs {} lines, first differences:\n{}",
        a.len(),
        b.len(),
        diffs.join("\n")
    );
}

fn round_trip(path: &Path) -> (Package, Package) {
    let name = path.display().to_string();
    let bytes = std::fs::read(path).unwrap();
    let first = import_bytes(bytes).unwrap();
    let saved = save(
        &first.workbook,
        Some(&first.package),
        &SaveOptions::default(),
    )
    .unwrap_or_else(|e| panic!("{name}: save: {e}"));
    assert!(saved.warnings.is_empty(), "{name}: {:?}", saved.warnings);
    let problems = check_package(&saved.bytes);
    assert!(problems.is_empty(), "{name}: {problems:#?}");
    let second = import_bytes(saved.bytes.clone()).unwrap_or_else(|e| panic!("{name}: {e}"));
    assert_eq!(first.warnings, second.warnings, "{name}: warnings");
    same_lines(
        &snapshot(&first.workbook),
        &snapshot(&second.workbook),
        &name,
    );

    if !second.workbook.settings().iteration {
        let again = save(
            &second.workbook,
            Some(&second.package),
            &SaveOptions::default(),
        )
        .unwrap();
        assert!(again.bytes == saved.bytes, "{name}: second save differs");
    }
    (first.package, second.package)
}

const OWNED: &[&str] = &[
    "[Content_Types].xml",
    "_rels/.rels",
    "xl/workbook.xml",
    "xl/_rels/workbook.xml.rels",
    "xl/styles.xml",
    "xl/sharedStrings.xml",
    "xl/metadata.xml",
    "docProps/app.xml",
];

fn owned(p: &str) -> bool {
    OWNED.contains(&p)
        || p.starts_with("xl/worksheets/")
        || p.starts_with("xl/tables/")
        || p == "xl/calcChain.xml"
}

fn referenced(a: &Package, p: &str) -> bool {
    let file = p.rsplit('/').next().unwrap_or(p);
    a.part_names().filter(|n| n.ends_with(".rels")).any(|r| {
        a.part(r)
            .is_some_and(|b| String::from_utf8_lossy(b).contains(&format!("{file}\"")))
    })
}

fn preserved_parts_identical(a: &Package, b: &Package, name: &str) {
    for p in a.part_names().filter(|p| !owned(p)) {
        if b.part(p).is_none() && !referenced(a, p) {
            continue;
        }
        assert_eq!(a.part(p), b.part(p), "{name}: part {p} changed");
        assert_eq!(a.content_type(p), b.content_type(p), "{name}: type of {p}");
    }
}

#[test]
fn workbook_suites_round_trip() {
    let dir = root().join("conformance/workbooks");
    let mut n = 0;
    let mut failed = Vec::new();
    for e in std::fs::read_dir(&dir).unwrap().flatten() {
        let p = e.path();
        if p.extension().and_then(|x| x.to_str()) != Some("xlsx") {
            continue;
        }
        n += 1;
        let r = std::panic::catch_unwind(|| {
            let (a, b) = round_trip(&p);
            preserved_parts_identical(&a, &b, &p.display().to_string());
            assert!(b.part("xl/calcChain.xml").is_none());
        });
        if r.is_err() {
            failed.push(p.file_name().unwrap().to_string_lossy().to_string());
        }
    }
    assert!(failed.is_empty(), "failed: {failed:?}");
    assert!(n >= 26);
}

#[test]
fn vba_fixtures_keep_their_project() {
    let dir = root().join("crates/vbaproject/tests/data");
    for e in std::fs::read_dir(&dir).unwrap().flatten() {
        let p = e.path();
        if p.extension().and_then(|x| x.to_str()) != Some("xlsm") {
            continue;
        }
        let (a, b) = round_trip(&p);
        let name = p.display().to_string();
        preserved_parts_identical(&a, &b, &name);
        assert!(b.part("xl/vbaProject.bin").is_some(), "{name}");
        assert_eq!(
            b.content_type("xl/workbook.xml"),
            Some("application/vnd.ms-excel.sheet.macroEnabled.main+xml")
        );
    }
}

#[test]
fn xlsm_saved_as_xlsx_drops_the_project() {
    let p = root().join("crates/vbaproject/tests/data/basic.xlsm");
    let imp = import_bytes(std::fs::read(p).unwrap()).unwrap();
    let opts = SaveOptions {
        kind: Some(xlcalc_xlsx::FileKind::Xlsx),
        ..SaveOptions::default()
    };
    let saved = save(&imp.workbook, Some(&imp.package), &opts).unwrap();
    assert!(check_package(&saved.bytes).is_empty());
    let back = import_bytes(saved.bytes).unwrap();
    assert!(back.vba.is_none());
    assert_eq!(
        back.package.content_type("xl/workbook.xml"),
        Some("application/vnd.openxmlformats-officedocument.spreadsheetml.sheet.main+xml")
    );
}

#[test]
fn new_workbook() {
    let mut wb = Workbook::new();
    let s = wb.sheet_at(0).id;
    wb.set_input(CellAddr::new(s, 0, 0), "1.5").unwrap();
    wb.set_input(CellAddr::new(s, 0, 1), "text").unwrap();
    wb.set_input(CellAddr::new(s, 1, 0), "=A1*2").unwrap();
    wb.set_input(CellAddr::new(s, 2, 0), "=SEQUENCE(3)")
        .unwrap();
    wb.set_input(CellAddr::new(s, 1, 1), "12/03/2024").unwrap();
    let opts = SaveOptions {
        created: Some("2026-11-01T00:00:00Z".into()),
        ..SaveOptions::default()
    };
    let saved = save(&wb, None, &opts).unwrap();
    assert!(
        check_package(&saved.bytes).is_empty(),
        "{:#?}",
        check_package(&saved.bytes)
    );
    let back = import_bytes(saved.bytes).unwrap();
    assert!(back.warnings.is_empty(), "{:?}", back.warnings);
    same_lines(&snapshot(&wb), &snapshot(&back.workbook), "new book");
}

#[test]
fn scenarios_round_trip() {
    use xlcalc::scenarios::{Scenario, ScenarioCell};
    let mut wb = Workbook::new();
    let s = wb.sheet_at(0).id;
    let cell = |row, col, value: &str| ScenarioCell {
        row,
        col,
        value: value.into(),
    };
    let sc = wb.scenarios_mut(s);
    sc.list.push(Scenario {
        name: "Base & <one>".into(),
        locked: true,
        hidden: false,
        user: String::new(),
        comment: "first \"comment\"".into(),
        cells: vec![cell(0, 0, "1"), cell(1, 0, "x y")],
        areas: Vec::new(),
    });
    sc.list.push(Scenario {
        name: "Alt".into(),
        locked: false,
        hidden: true,
        user: "me".into(),
        comment: String::new(),
        cells: vec![cell(0, 27, "0.5")],
        areas: Vec::new(),
    });
    sc.current = Some(1);
    sc.show = Some(1);
    let saved = save(&wb, None, &SaveOptions::default()).unwrap();
    assert!(check_package(&saved.bytes).is_empty());
    let back = import_bytes(saved.bytes).unwrap();
    let id = back.workbook.sheet_at(0).id;
    assert_eq!(back.workbook.scenarios(id), wb.scenarios(s));
    let again = save(&back.workbook, Some(&back.package), &SaveOptions::default()).unwrap();
    let back2 = import_bytes(again.bytes).unwrap();
    let id2 = back2.workbook.sheet_at(0).id;
    assert_eq!(back2.workbook.scenarios(id2), wb.scenarios(s));
}

#[test]
fn feature_fixture_keeps_what_it_does_not_model() {
    let p = root().join("conformance/files/features.xlsx");
    let (a, b) = round_trip(&p);
    preserved_parts_identical(&a, &b, "features");
    for part in [
        "xl/drawings/drawing1.xml",
        "xl/charts/chart1.xml",
        "xl/comments1.xml",
        "xl/drawings/vmlDrawing1.vml",
    ] {
        assert!(b.part(part).is_some(), "{part} kept");
    }
    let sheet = std::str::from_utf8(b.part("xl/worksheets/sheet1.xml").unwrap()).unwrap();
    for el in [
        "<conditionalFormatting",
        "<dataValidations",
        "<hyperlinks",
        "<mergeCells",
        "<pageSetup",
        "<drawing ",
        "<legacyDrawing ",
        "<tableParts",
    ] {
        assert!(sheet.contains(el), "{el} kept");
    }
    let book = std::str::from_utf8(b.part("xl/workbook.xml").unwrap()).unwrap();
    assert!(book.contains("state=\"hidden\""));
    assert!(book.contains("activeTab=\"1\""));
}

#[test]
fn structural_edit_warns_about_preserved_elements() {
    let p = root().join("conformance/files/features.xlsx");
    let mut imp = import_bytes(std::fs::read(p).unwrap()).unwrap();
    let data = imp.workbook.sheet_by_name("Data").unwrap().id;
    imp.package.mark_structural_edit(data);
    let saved = save(&imp.workbook, Some(&imp.package), &SaveOptions::default()).unwrap();
    assert_eq!(saved.warnings.len(), 1, "{:?}", saved.warnings);
    let w = &saved.warnings[0];
    assert!(
        w.starts_with("Data: ") && w.contains("conditionalFormatting"),
        "{w}"
    );
}

#[test]
fn edited_styles_round_trip() {
    use std::sync::Arc;
    use xlcalc::style::{
        BorderLine, CellColor, FontScheme, HAlign, NamedStyle, Pattern, Style, Underline, VAlign,
    };
    let p = root().join("conformance/files/features.xlsx");
    let mut imp = import_bytes(std::fs::read(p).unwrap()).unwrap();
    let wb = &mut imp.workbook;
    assert!(!wb.style_table().dxfs.is_empty());
    let s = wb.sheet_at(0).id;
    let rect = |r: u32, c: u32| xlcalc::Rect::new(r, c, r, c + 1);
    wb.set_style_with(s, rect(30, 0), &|x| {
        let mut x = x.clone();
        x.font.bold = true;
        x.font.underline = Underline::DoubleAccounting;
        x.font.size_twips = 210;
        x.font.name = Arc::from("Courier New");
        x.font.scheme = FontScheme::None;
        x.font.color = CellColor::Rgb(0x12_34_56);
        x.fill.pattern = Pattern::Solid;
        x.fill.color = CellColor::theme(5, -0.249_977_111_117_893);
        x
    });
    wb.set_style_with(s, rect(31, 0), &|x| {
        let mut x = x.clone();
        x.fill.pattern = Pattern::LightTrellis;
        x.fill.color = CellColor::Indexed(13);
        x.fill.pattern_color = CellColor::Rgb(0xFF_00_00);
        x.borders.left.line = BorderLine::MediumDashDot;
        x.borders.left.color = CellColor::theme(4, 0.0);
        x.borders.diagonal.line = BorderLine::Thin;
        x.borders.diagonal_up = true;
        x.alignment.horizontal = HAlign::CenterAcrossSelection;
        x.alignment.vertical = VAlign::Top;
        x.alignment.wrap = true;
        x.alignment.rotation = 255;
        x.locked = false;
        x.formula_hidden = true;
        x.number_format = Arc::from("$#,##0.00;[Red]-$#,##0.00");
        x
    });
    let good = wb.style_table().get("Good").unwrap().style.clone();
    wb.set_style_with(s, rect(32, 0), &|_| good.clone());
    let mine = Style {
        named: Arc::from("Mine"),
        number_format: Arc::from("0.000"),
        ..Style::default()
    };
    wb.style_table_mut().insert(NamedStyle {
        name: Arc::from("Mine"),
        builtin_id: None,
        style: mine.clone(),
        include_number: true,
        include_font: false,
        include_alignment: false,
        include_border: false,
        include_patterns: false,
        include_protection: true,
    });
    wb.set_style_with(s, rect(33, 0), &|_| mine.clone());
    wb.set_text_prefixed(CellAddr::new(s, 34, 0), "007")
        .unwrap();
    let saved = save(wb, Some(&imp.package), &SaveOptions::default()).unwrap();
    let problems = check_package(&saved.bytes);
    assert!(problems.is_empty(), "{problems:#?}");
    let back = import_bytes(saved.bytes.clone()).unwrap();
    assert_eq!(imp.warnings, back.warnings);
    same_lines(&snapshot(wb), &snapshot(&back.workbook), "edited styles");
    assert!(back.workbook.has_apostrophe(CellAddr::new(s, 34, 0)));
    let again = save(&back.workbook, Some(&back.package), &SaveOptions::default()).unwrap();
    assert!(again.bytes == saved.bytes, "second save differs");
}

#[test]
fn styles_fixture_round_trips() {
    let p = root().join("conformance/files/styles.xlsx");
    let (a, b) = round_trip(&p);
    preserved_parts_identical(&a, &b, "styles");
    let imp = import_bytes(std::fs::read(&p).unwrap()).unwrap();
    let opts = SaveOptions {
        fresh_styles: true,
        ..SaveOptions::default()
    };
    let saved = save(&imp.workbook, Some(&imp.package), &opts).unwrap();
    let problems = check_package(&saved.bytes);
    assert!(problems.is_empty(), "{problems:#?}");
    let back = import_bytes(saved.bytes).unwrap();
    assert_eq!(imp.warnings, back.warnings);
    same_lines(
        &snapshot(&imp.workbook),
        &snapshot(&back.workbook),
        "fresh styles",
    );
}

fn all_controls() -> Vec<Control> {
    use ControlKind::*;
    let kinds = [
        Button, CheckBox, Radio, Spin, Scroll, Drop, List, GBox, Label,
    ];
    let mut v: Vec<Control> = kinds
        .iter()
        .enumerate()
        .map(|(i, k)| {
            let name = format!("Ctl {}", i + 1);
            Control::new(*k, &name, [10.0, 10.0 + 30.0 * i as f64, 60.5, 20.0])
        })
        .collect();
    v[0].on_action = "Fn".into();
    v[1].fmla_link = "E1".into();
    v[1].checked = 2;
    v[2].caption = "two
lines & <more>"
        .into();
    v[3].fmla_link = "'S2'!B2".into();
    (v[3].val, v[3].min, v[3].max, v[3].inc) = (7, 1, 10, 2);
    v[4].horiz = true;
    v[5].fmla_range = "G1:G3".into();
    (v[5].sel, v[5].drop_lines) = (2, 4);
    v[6].fmla_range = "G1:G3".into();
    v[7].locked = false;
    v
}

#[test]
fn form_controls_round_trip() {
    let wb = Workbook::new();
    let s = wb.sheet_at(0).id;
    let mut list = all_controls();
    let opts = SaveOptions {
        controls: vec![(s, list.clone())],
        ..SaveOptions::default()
    };
    let saved = save(&wb, None, &opts).unwrap();
    assert!(
        check_package(&saved.bytes).is_empty(),
        "{:#?}",
        check_package(&saved.bytes)
    );
    let back = import_bytes(saved.bytes.clone()).unwrap();
    let got = read_controls(&back.package, back.workbook.sheet_at(0).id);
    assert_eq!(got.len(), list.len());
    for (i, (g, w)) in got.iter().zip(list.iter_mut()).enumerate() {
        assert_eq!(g.shape_id, 1025 + i as u32);
        w.shape_id = g.shape_id;
        assert_eq!(g, w, "control {i}");
    }

    let mut next = got.clone();
    next[0].on_action = "Other".into();
    next[0].left = 33.5;
    next[3].val = 9;
    next[3].fmla_link = String::new();
    next[2].caption = "plain".into();
    next.remove(8);
    next.push(Control::new(
        ControlKind::CheckBox,
        "Late",
        [1.0, 2.0, 3.0, 4.0],
    ));
    let opts = SaveOptions {
        controls: vec![(s, next.clone())],
        ..SaveOptions::default()
    };
    let saved2 = save(&back.workbook, Some(&back.package), &opts).unwrap();
    assert!(
        check_package(&saved2.bytes).is_empty(),
        "{:#?}",
        check_package(&saved2.bytes)
    );
    let back2 = import_bytes(saved2.bytes).unwrap();
    let got2 = read_controls(&back2.package, back2.workbook.sheet_at(0).id);
    assert_eq!(got2.len(), next.len());
    let last = next.len() - 1;
    next[last].shape_id = got2[last].shape_id;
    assert_eq!(got2[last].shape_id, 1034);
    assert_eq!(got2, next);

    let opts = SaveOptions {
        controls: vec![(s, got2.clone())],
        ..SaveOptions::default()
    };
    let saved3 = save(&back2.workbook, Some(&back2.package), &opts).unwrap();
    let back3 = import_bytes(saved3.bytes).unwrap();
    for part in back2.package.part_names() {
        if part.contains("ctrlProp") || part.contains("vmlDrawing") {
            assert_eq!(back2.package.part(part), back3.package.part(part), "{part}");
        }
    }
}

#[test]
fn row_heights_round_trip() {
    let mut wb = Workbook::new();
    let s = wb.sheet_at(0).id;
    wb.set_input(CellAddr::new(s, 0, 0), "x").unwrap();
    wb.set_row_height(s, 2, 2, 33.0).unwrap();
    wb.set_row_height(s, 4, 4, 12.7).unwrap();
    wb.set_row_height(s, 6, 6, 0.0).unwrap();
    let saved = save(&wb, None, &SaveOptions::default()).unwrap();
    assert!(check_package(&saved.bytes).is_empty());
    let back = import_bytes(saved.bytes).unwrap();
    assert_eq!(
        wb.row_custom_heights(s),
        back.workbook.row_custom_heights(s)
    );
    assert_eq!(
        back.workbook.row_custom_heights(s),
        vec![(2, 264), (4, 102)]
    );
    assert!(back.workbook.row_hidden(s, 6));
}
