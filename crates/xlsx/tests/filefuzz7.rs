use xlcalc_xlsx::{import_bytes, save, xlsx_to_xls, xlsx_to_xlsb, FileKind, SaveOptions};

fn resave(bytes: Vec<u8>, ext: &str) -> Result<(Vec<u8>, Vec<String>), String> {
    let imp = import_bytes(bytes)?;
    let kind = if ext == "xlsm" {
        FileKind::Xlsm
    } else {
        FileKind::Xlsx
    };
    let opts = SaveOptions {
        kind: Some(kind),
        ..Default::default()
    };
    let saved = save(&imp.workbook, Some(&imp.package), &opts)?;
    if let Ok(p) = std::env::var("FUZZ_KEEP") {
        std::fs::write(p, &saved.bytes).map_err(|e| e.to_string())?;
    }
    match ext {
        "xls" => xlsx_to_xls(&saved.bytes),
        "xlsb" => xlsx_to_xlsb(&saved.bytes),
        _ => Ok((saved.bytes, Vec::new())),
    }
}

#[test]
#[ignore]
fn resave_fuzz_dir() {
    let dir = std::env::var("FUZZ_DIR").expect("FUZZ_DIR");
    let mut log = String::new();
    for e in std::fs::read_dir(&dir).unwrap() {
        let p = e.unwrap().path();
        let ext = p.extension().and_then(|e| e.to_str()).unwrap_or("");
        let stem = p.file_stem().and_then(|s| s.to_str()).unwrap_or("");
        if !["xlsx", "xlsm", "xlsb", "xls"].contains(&ext) || stem.ends_with("_ours") {
            continue;
        }

        let ext = match (ext, std::env::var("FUZZ_TO")) {
            ("xlsx", Ok(to)) => to,
            _ => ext.to_string(),
        };
        let ext = ext.as_str();
        let out = p.with_file_name(format!("{stem}_ours.{ext}"));
        match resave(std::fs::read(&p).unwrap(), ext) {
            Ok((b, w)) => {
                std::fs::write(&out, b).unwrap();
                log.push_str(&format!("OK {} {w:?}\n", p.display()));
            }
            Err(e) => log.push_str(&format!("FAIL {} {e}\n", p.display())),
        }
    }
    println!("{log}");
}

#[test]
#[ignore]
fn list_xls_globals() {
    let cfb = xlcalc_vbaproject::cfb::Cfb::new(
        std::fs::read(std::env::var("XLS_FILE").unwrap()).unwrap(),
    )
    .unwrap();
    let d = cfb.read_stream("Workbook").unwrap();
    let mut p = 0;
    let mut out = String::new();
    while p + 4 <= d.len() {
        let id = u16::from_le_bytes([d[p], d[p + 1]]);
        let len = usize::from(u16::from_le_bytes([d[p + 2], d[p + 3]]));
        out.push_str(&format!("{id:04x}[{len}] "));
        if std::env::var("XLS_HEX").is_ok_and(|v| v.contains(&format!("{id:04x}"))) {
            out.push_str(&format!(
                "{{{}}} ",
                d[p + 4..p + 4 + len]
                    .iter()
                    .map(|b| format!("{b:02x}"))
                    .collect::<Vec<_>>()
                    .join("")
            ));
        }
        if id == 0x85 && std::env::var("XLS_ALL").is_err() {
            break;
        }
        p += 4 + len;
    }
    println!("{out}");
}

#[test]
#[ignore]
fn resave_matrix() {
    let dir = std::env::var("FUZZ_DIR").expect("FUZZ_DIR");
    let full = std::env::var("FUZZ_FULL").is_ok();
    let only = std::env::var("FUZZ_ONLY").ok();
    let mut log = String::new();
    let mut names: Vec<_> = std::fs::read_dir(&dir)
        .unwrap()
        .map(|e| e.unwrap().path())
        .collect();
    names.sort();
    for p in names {
        let ext = p.extension().and_then(|e| e.to_str()).unwrap_or("");
        let stem = p.file_stem().and_then(|s| s.to_str()).unwrap_or("");
        if !["xlsx", "xlsm", "xlsb", "xls"].contains(&ext) || stem.contains('_') {
            continue;
        }
        if only.as_ref().is_some_and(|o| !stem.starts_with(o.as_str())) {
            continue;
        }
        let bytes = std::fs::read(&p).unwrap();
        for tgt in ["xlsx", "xlsm", "xlsb", "xls"] {
            let wanted = full
                || tgt == ext
                || matches!(
                    (ext, tgt),
                    ("xlsx", "xlsb") | ("xlsx", "xls") | ("xls", "xlsx") | ("xlsb", "xlsx")
                );
            if !wanted {
                continue;
            }
            let out = p.with_file_name(format!("{stem}_{ext}2{tgt}.{tgt}"));
            let r = std::panic::catch_unwind(|| resave(bytes.clone(), tgt));
            match r {
                Ok(Ok((b, w))) => {
                    std::fs::write(&out, b).unwrap();
                    log.push_str(&format!("OK {} {w:?}\n", out.display()));
                }
                Ok(Err(e)) => log.push_str(&format!("FAIL {} {e}\n", out.display())),
                Err(_) => log.push_str(&format!("PANIC {}\n", out.display())),
            }
        }
    }
    println!("{log}");
}
