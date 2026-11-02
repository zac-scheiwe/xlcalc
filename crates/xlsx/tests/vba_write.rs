use std::path::PathBuf;
use xlcalc::Workbook;
use xlcalc_vbaproject::write::{build, header, ModuleSource};
use xlcalc_vbaproject::ModuleKind;
use xlcalc_xlsx::{check_package, import_bytes, save, FileKind, SaveOptions, VbaWrite};

fn data(name: &str) -> PathBuf {
    PathBuf::from(env!("CARGO_MANIFEST_DIR"))
        .join("../vbaproject/tests/data")
        .join(name)
}

fn xlsm() -> SaveOptions {
    SaveOptions {
        kind: Some(FileKind::Xlsm),
        created: Some("2026-11-01T00:00:00Z".into()),
        ..SaveOptions::default()
    }
}

#[test]
fn new_workbook_with_a_project() {
    let wb = Workbook::new();
    let doc = |name: &str, base: &str| ModuleSource {
        name: name.into(),
        kind: ModuleKind::Document,
        source: header(ModuleKind::Document, name, Some(base)),
    };
    let modules = vec![
        doc("ThisWorkbook", "{00020819-0000-0000-C000-000000000046}"),
        doc("Sheet1", "{00020820-0000-0000-C000-000000000046}"),
        ModuleSource {
            name: "Mod1".into(),
            kind: ModuleKind::Standard,
            source: format!(
                "{}Option Explicit\r\nPublic Function Triple(x As Double) As Double\r\n    Triple = x * 3\r\nEnd Function\r\n",
                header(ModuleKind::Standard, "Mod1", None)
            ),
        },
    ];
    let opts = SaveOptions {
        vba: Some(VbaWrite {
            project: build("VBAProject", &modules).unwrap(),
            workbook_code_name: "ThisWorkbook".into(),
            sheet_code_names: vec!["Sheet1".into()],
        }),
        ..xlsm()
    };
    let saved = save(&wb, None, &opts).unwrap();
    assert!(
        check_package(&saved.bytes).is_empty(),
        "{:#?}",
        check_package(&saved.bytes)
    );
    let back = import_bytes(saved.bytes).unwrap();
    let vba = back.vba.expect("a vbaProject.bin");
    assert_eq!(vba.workbook_code_name.as_deref(), Some("ThisWorkbook"));
    assert_eq!(
        vba.sheet_code_names,
        vec![("Sheet1".to_string(), Some("Sheet1".to_string()))]
    );
    let project = vba.project.unwrap();
    assert_eq!(project.modules.len(), 3);
    for (m, w) in project.modules.iter().zip(&modules) {
        assert_eq!((&m.name, m.kind), (&w.name, w.kind));
        assert_eq!(m.source.as_ref().unwrap(), &w.source);
    }
    assert_eq!(
        back.package.content_type("xl/vbaProject.bin"),
        Some("application/vnd.ms-office.vbaProject")
    );
    assert_eq!(
        back.package.content_type("xl/workbook.xml"),
        Some("application/vnd.ms-excel.sheet.macroEnabled.main+xml")
    );
}

#[test]
fn excel_projects_rewrite_from_their_source() {
    for name in ["basic", "cp1252", "lines", "many", "big", "noise"] {
        let imp = import_bytes(std::fs::read(data(&format!("{name}.xlsm"))).unwrap()).unwrap();
        let vba = imp.vba.as_ref().unwrap();
        let project = vba.project.as_ref().unwrap();

        if project.modules.iter().any(|m| m.source.is_err()) {
            continue;
        }
        let modules: Vec<ModuleSource> = project
            .modules
            .iter()
            .map(|m| ModuleSource {
                name: m.name.clone(),
                kind: m.kind,
                source: m.source.clone().unwrap(),
            })
            .collect();
        let opts = SaveOptions {
            vba: Some(VbaWrite {
                project: build(&project.name, &modules).unwrap(),
                workbook_code_name: vba.workbook_code_name.clone().unwrap(),
                sheet_code_names: vba
                    .sheet_code_names
                    .iter()
                    .map(|s| s.1.clone().unwrap())
                    .collect(),
            }),
            ..xlsm()
        };
        let saved = save(&imp.workbook, Some(&imp.package), &opts).unwrap();
        assert!(check_package(&saved.bytes).is_empty(), "{name}");
        let back = import_bytes(saved.bytes).unwrap();
        let again = back.vba.unwrap().project.unwrap();
        assert_eq!(again.name, project.name, "{name}");
        assert_eq!(again.modules.len(), project.modules.len(), "{name}");
        for (a, b) in again.modules.iter().zip(&project.modules) {
            assert_eq!(
                (&a.name, a.kind, &a.source),
                (&b.name, b.kind, &b.source),
                "{name}"
            );
        }
    }
}
