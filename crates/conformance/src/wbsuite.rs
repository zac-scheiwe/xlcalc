use crate::{compare_value, show_expected, show_value, Outcome, Tally};
use serde_json::Value as J;
use std::path::Path;
use xlcalc::parse_cell_ref;

pub fn run_workbook(golden: &J, xlsx: &Path, check_text: bool) -> Tally {
    let mut tally = Tally::new();
    let mut imp = match xlcalc_xlsx::import_file(xlsx) {
        Ok(i) => i,
        Err(e) => {
            tally.add(Outcome::Wrong, || format!("import failed: {e}"));
            return tally;
        }
    };
    for w in &imp.warnings {
        tally.add(Outcome::Wrong, || format!("import: {w}"));
    }

    compare_opened(golden, &imp.workbook, &mut tally);
    if imp.opened_uncalculated {
        imp.workbook.full_recalc();
    }
    compare_book(golden, xlsx, &imp.workbook, check_text, "", &mut tally);

    if !imp.workbook.settings().iteration {
        let saved = xlcalc_xlsx::save(
            &imp.workbook,
            Some(&imp.package),
            &xlcalc_xlsx::SaveOptions::default(),
        )
        .and_then(|s| {
            let problems = xlcalc_xlsx::check_package(&s.bytes);
            if problems.is_empty() {
                xlcalc_xlsx::import_bytes(s.bytes)
            } else {
                Err(format!("package check: {problems:?}"))
            }
        });
        match saved {
            Ok(back) => {
                for w in &back.warnings {
                    tally.add(Outcome::Wrong, || format!("saved: import: {w}"));
                }
                compare_book(
                    golden,
                    xlsx,
                    &back.workbook,
                    check_text,
                    "saved: ",
                    &mut tally,
                );
            }
            Err(e) => tally.add(Outcome::Wrong, || format!("saved: {e}")),
        }
    }
    tally
}

fn compare_opened(golden: &J, wb: &xlcalc::Workbook, tally: &mut Tally) {
    let Some(items) = golden["opened"].as_array() else {
        return;
    };
    let mut differ = Vec::new();
    let mut same = 0;
    for it in items {
        let sheet = it["sheet"].as_str().unwrap_or("");
        let Some(sh) = wb.sheet_by_name(sheet) else {
            differ.push(format!("{sheet}: sheet missing"));
            continue;
        };
        for c in it["cells"].as_array().into_iter().flatten() {
            let name = c["cell"].as_str().unwrap_or("");
            let Some(addr) = parse_cell_ref(wb, name, sh.id) else {
                continue;
            };
            let got = wb.value(addr);
            if compare_value(&c["value"], &got) == Outcome::Exact {
                same += 1;
            } else {
                differ.push(format!(
                    "{sheet}!{name} expected {}, got {}",
                    show_expected(&c["value"]),
                    show_value(&got)
                ));
            }
        }
    }
    for _ in 0..same {
        tally.add(Outcome::Exact, String::new);
    }
    if !differ.is_empty() {
        tally.add(Outcome::Wrong, || {
            format!(
                "opened: {} cell(s) as Excel shows them before calculating, first {}",
                differ.len(),
                differ[0]
            )
        });
    }
}

fn compare_book(
    golden: &J,
    xlsx: &Path,
    wb: &xlcalc::Workbook,
    check_text: bool,
    tag: &str,
    tally: &mut Tally,
) {
    for it in golden["items"].as_array().unwrap() {
        let sheet = it["sheet"].as_str().unwrap();
        let Some(sh) = wb.sheet_by_name(sheet) else {
            tally.add(Outcome::Wrong, || format!("{tag}{sheet}: sheet missing"));
            continue;
        };
        let id = sh.id;
        for c in it["cells"].as_array().unwrap() {
            let name = c["cell"].as_str().unwrap();
            let addr = parse_cell_ref(wb, name, id).unwrap();
            let got = wb.value(addr);
            let want = &c["value"];
            let o = compare_value(want, &got);
            let formula = c["formula"].as_str().unwrap_or("");
            tally.add(o, || {
                format!(
                    "{tag}{sheet}!{name} {formula} → expected {}, got {}",
                    show_expected(want),
                    show_value(&got)
                )
            });
            if let Some(text) = c["text"].as_str() {
                let t = wb.display_text(addr);
                let o = if t == text {
                    Outcome::Exact
                } else {
                    Outcome::Wrong
                };
                tally.add(o, || {
                    format!(
                        "{tag}{sheet}!{name} text of {} ({}) → expected {text:?}, got {t:?}",
                        show_value(&got),
                        c["format"].as_str().unwrap_or("")
                    )
                });
            }
            if check_text && !formula.is_empty() {
                let book = xlsx.file_name().and_then(|n| n.to_str()).unwrap_or("");
                let got = wb
                    .formula_text(addr)
                    .map(|t| xlcalc_xlsx::display_formula(&t, book));
                let o = if got.as_deref() == Some(formula) {
                    Outcome::Exact
                } else {
                    Outcome::Wrong
                };
                tally.add(o, || {
                    format!("{tag}{sheet}!{name} formula → Excel {formula:?}, engine {got:?}")
                });
            }
        }
    }
}

fn style_snapshot(wb: &xlcalc::Workbook) -> Vec<String> {
    let mut out = vec![format!("base style {:?}", wb.base_style())];
    let t = wb.style_table();
    out.push(format!("theme {:?}", t.theme));
    out.extend(t.named.iter().map(|n| format!("named style {n:?}")));
    out.extend(t.dxfs.iter().map(|d| format!("dxf {d}")));
    for sh in wb.sheets() {
        out.push(format!("sheet {}", sh.name));
        let Some(u) = wb.used_range(sh.id) else {
            continue;
        };
        for r in u.r0..=u.r1 {
            for c in u.c0..=u.c1 {
                let a = xlcalc::CellAddr::new(sh.id, r, c);
                let v = wb.value(a);
                let f = wb.formula_text(a);
                let st = wb.cell_style(a);
                if matches!(v, xlcalc::Value::Blank) && f.is_none() && st == wb.base_style() {
                    continue;
                }
                out.push(format!("{} {} {f:?}", a.a1(), show_value(&v)));
                if st != wb.base_style() {
                    out.push(format!("{} style {st:?}", a.a1()));
                }
            }
        }
    }
    out
}

pub fn saved_round_trip(wb: &xlcalc::Workbook, tally: &mut Tally) {
    let saved = xlcalc_xlsx::save(wb, None, &xlcalc_xlsx::SaveOptions::default()).and_then(|s| {
        let problems = xlcalc_xlsx::check_package(&s.bytes);
        if problems.is_empty() {
            xlcalc_xlsx::import_bytes(s.bytes)
        } else {
            Err(format!("package check: {problems:?}"))
        }
    });
    let back = match saved {
        Ok(b) => b,
        Err(e) => return tally.add(Outcome::Wrong, || format!("saved: {e}")),
    };
    for w in &back.warnings {
        tally.add(Outcome::Wrong, || format!("saved: import: {w}"));
    }
    let (a, b) = (style_snapshot(wb), style_snapshot(&back.workbook));
    let diffs: Vec<String> = a
        .iter()
        .filter(|l| !b.contains(l))
        .map(|l| format!("- {l}"))
        .chain(
            b.iter()
                .filter(|l| !a.contains(l))
                .map(|l| format!("+ {l}")),
        )
        .collect();
    let o = if diffs.is_empty() {
        Outcome::Exact
    } else {
        Outcome::Wrong
    };
    tally.add(o, || {
        format!(
            "saved: reimport differs in {} line(s): {}",
            diffs.len(),
            diffs.into_iter().take(4).collect::<Vec<_>>().join(" | ")
        )
    });
}
