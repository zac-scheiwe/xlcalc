use std::collections::BTreeMap;

use serde_json::Value as J;
use xlcalc::{parse_cell_ref, parse_range_ref, SheetId, Value, Workbook};
use xlcalc_om::{scode_error, OmError};
use xlcalc_variant::{ObjectId, Variant};

use super::omsteps::OmSession;
use super::{
    cell_result, compare_value, num_of, oos_what, pinned_to_clock, sheetsteps, show_expected,
    show_value, Outcome, Tally,
};

#[derive(Clone, Copy)]
pub struct ReadFlags {
    pub check_format: bool,
    pub check_all_formats: bool,
    pub check_empty_formats: bool,

    pub uses_clock: bool,
}

fn message(e: &OmError) -> String {
    e.detail.clone().unwrap_or_else(|| e.to_string())
}

fn note_oos(tally: &mut Tally, e: &OmError) {
    if let Some(what) = e
        .description
        .as_deref()
        .and_then(|d| d.strip_prefix("out of scope: "))
    {
        tally.pending_oos = Some(what.to_string());
    }
}

fn split_ref(r: &str, default: &str) -> (String, String) {
    match r.rfind('!') {
        Some(i) => (
            r[..i].trim_matches('\'').replace("''", "'"),
            r[i + 1..].to_string(),
        ),
        None => (default.to_string(), r.to_string()),
    }
}

fn verdict(tally: &mut Tally, op: &str, n: usize, rec: &J, err: Option<String>) {
    if let Some(w) = err.as_deref().and_then(oos_what) {
        tally.pending_oos = Some(w);
        return;
    }
    let ok = err.is_some() == !rec["error"].is_null();
    tally.add(if ok { Outcome::Exact } else { Outcome::Wrong }, || {
        format!(
            "step {n}: {op} — Excel error {:?}, engine {err:?}",
            rec["error"]
        )
    });
}

fn to_value(v: &Variant) -> Value {
    match v {
        Variant::Empty | Variant::Null => Value::Blank,
        Variant::R8(n) => Value::Number(*n),
        Variant::I4(n) => Value::Number(f64::from(*n)),
        Variant::Bstr(s) => Value::from(s.as_str()),
        Variant::Bool(b) => Value::Bool(*b),
        Variant::Error(code) => Value::Error(scode_error(*code).unwrap_or(xlcalc::XlError::VALUE)),
        Variant::Array(sa) => {
            let (rows, cols) = match sa.bounds.as_slice() {
                [(_, r), (_, c)] => (*r, *c),
                _ => (1, sa.data.len() as u32),
            };
            let data = (0..rows as usize)
                .flat_map(|r| (0..cols as usize).map(move |c| (r, c)))
                .map(|(r, c)| to_value(&sa.data[c * rows as usize + r]))
                .collect();
            Value::Array(std::sync::Arc::new(xlcalc::Array::new(rows, cols, data)))
        }
        _ => Value::Error(xlcalc::XlError::VALUE),
    }
}

struct CellData {
    value: Value,
    formula: Option<String>,

    raw2: String,
    legacy: String,
    format: String,
    array: Option<String>,
    merge: Option<String>,
}

fn text(v: Variant) -> String {
    match v {
        Variant::Bstr(s) => s,
        _ => String::new(),
    }
}

fn flag(s: &mut OmSession, id: ObjectId, member: &str) -> Result<bool, OmError> {
    Ok(s.get(id, member)? == Variant::Bool(true))
}

fn read_cell(s: &mut OmSession, sheet: &str, a1: &str, scored: bool) -> Result<CellData, OmError> {
    let cell = s.range(sheet, a1)?;
    let has_formula = flag(s, cell, "HasFormula")?;
    let formula = if has_formula {
        Some(text(s.get(cell, "Formula2")?))
    } else {
        None
    };
    let (mut value, mut raw2, mut legacy, mut format) = (
        s.get(cell, "Value2")?,
        String::new(),
        String::new(),
        String::new(),
    );
    if scored {
        if has_formula && flag(s, cell, "HasSpill")? {
            if let Variant::Dispatch(Some(spill)) = s.get(cell, "SpillingToRange")? {
                let same = |s: &mut OmSession, m: &str| -> Result<bool, OmError> {
                    Ok(s.get(spill, m)? == s.get(cell, m)?)
                };
                if same(s, "Row")? && same(s, "Column")? {
                    value = s.get(spill, "Value2")?;
                }
            }
        }
        format = text(s.get(cell, "NumberFormat")?);
    } else {
        raw2 = text(s.get(cell, "Formula2")?);
        legacy = text(s.get(cell, "Formula")?);
    }
    let array = if flag(s, cell, "HasArray")? {
        match s.get(cell, "CurrentArray")? {
            Variant::Dispatch(Some(r)) => Some(s.address(r)?),
            _ => None,
        }
    } else {
        None
    };
    let merge = if flag(s, cell, "MergeCells")? {
        match s.get(cell, "MergeArea")? {
            Variant::Dispatch(Some(r)) => Some(s.address(r)?),
            _ => None,
        }
    } else {
        None
    };
    Ok(CellData {
        value: to_value(&value),
        formula,
        raw2,
        legacy,
        format,
        array,
        merge,
    })
}

fn read_cells(
    om: &mut OmSession,
    wb: &mut Workbook,
    range: &str,
    default: &str,
    cells: &[&str],
    scored: bool,
) -> Vec<Result<CellData, OmError>> {
    let (sheet, _) = split_ref(range, default);
    om.with(wb, |s| {
        cells
            .iter()
            .map(|a1| read_cell(s, &sheet, a1, scored))
            .collect()
    })
}

fn check_blocks(c: &J, d: &CellData, n: usize, tally: &mut Tally) {
    let a1 = c["cell"].as_str().unwrap_or("");
    for (what, want, got) in [
        ("array", c["array"].as_str().map(str::to_string), &d.array),
        ("merge", c["merge"].as_str().map(str::to_string), &d.merge),
    ] {
        if want.is_some() || got.is_some() {
            let o = if want == *got {
                Outcome::Exact
            } else {
                Outcome::Wrong
            };
            tally.add(o, || {
                format!("step {n}: {a1} {what} expected {want:?}, got {got:?}")
            });
        }
    }
}

pub struct Ctx<'a> {
    pub op: &'a str,
    pub step: &'a J,
    pub rec: &'a J,

    pub n: usize,
    pub default_sheet: SheetId,
    pub flags: ReadFlags,
}

pub fn run(
    om: &mut OmSession,
    wb: &mut Workbook,
    cx: &Ctx,
    typed: &mut BTreeMap<String, String>,
    tally: &mut Tally,
) -> bool {
    let Ctx {
        op,
        step,
        rec,
        n,
        default_sheet,
        flags: _,
    } = *cx;
    let excel_rejected = !rec["error"].is_null();
    let default = wb
        .sheet(default_sheet)
        .map(|s| s.name.clone())
        .unwrap_or_default();
    let place = |k: &str| split_ref(step[k].as_str().unwrap(), &default);
    match op {
        "set" | "typel" => {
            let (sheet, a1) = place("cell");
            typed.insert(
                step["cell"].as_str().unwrap().to_string(),
                step["input"].as_str().unwrap().to_string(),
            );
            let input = step["input"].as_str().unwrap().to_string();
            let r = om.with(wb, |s| {
                let cell = s.range(&sheet, &a1)?;
                s.put(cell, "Formula2Local", Variant::Bstr(input))
            });
            if let Err(e) = &r {
                note_oos(tally, e);
            }
            match r {
                Err(e) if !excel_rejected => tally.add(Outcome::Wrong, || {
                    format!("step {n}: set {} rejected: {}", step["cell"], message(&e))
                }),
                Ok(()) if excel_rejected => tally.add(Outcome::Wrong, || {
                    format!(
                        "step {n}: set {} accepted, but Excel rejected it",
                        step["cell"]
                    )
                }),
                Err(_) => tally.add(Outcome::Exact, String::new),
                Ok(()) => {}
            }
        }
        "setl" => {
            let (sheet, a1) = place("cell");
            let input = step["input"].as_str().unwrap().to_string();
            let r = om.with(wb, |s| {
                let cell = s.range(&sheet, &a1)?;
                s.put(cell, "Formula", Variant::Bstr(input))
            });
            verdict(tally, op, n, rec, r.err().map(|e| message(&e)));
        }
        "array" => {
            let (sheet, a1) = place("range");
            let input = step["input"].as_str().unwrap().to_string();
            let r = om.with(wb, |s| {
                let range = s.range(&sheet, &a1)?;
                s.put(range, "FormulaArray", Variant::Bstr(input))
            });
            verdict(tally, op, n, rec, r.err().map(|e| message(&e)));
        }
        "setv" => {
            let (sheet, a1) = place("cell");
            let t = &step["value"];

            let _ = om.with(wb, |s| {
                let cell = s.range(&sheet, &a1)?;
                let v = t["v"].as_str().unwrap_or("").to_string();
                match t["t"].as_str().unwrap_or("") {
                    "n" => s.put(cell, "Value2", Variant::R8(num_of(t))),
                    "s" => s.put(cell, "Value2", Variant::Bstr(format!("'{v}"))),
                    "b" => s.put(cell, "Value2", Variant::Bool(t["v"].as_bool().unwrap())),
                    "e" | "f" => s.put(cell, "Formula2", Variant::Bstr(v)),
                    "in" => s.put(cell, "Formula2Local", Variant::Bstr(v)),
                    _ => s.method(cell, "ClearContents").map(|_| ()),
                }
            });
        }
        "clear" => {
            let (sheet, a1) = place("range");

            let scored = wb.has_data_tables();
            let r = om.with(wb, |s| {
                let range = s.range(&sheet, &a1)?;
                s.method(range, "ClearContents")
            });
            if let Err(e) = &r {
                note_oos(tally, e);
            }
            let excel_err = excel_rejected;
            if r.is_err() != excel_err {
                tally.add(Outcome::Wrong, || {
                    format!(
                        "step {n}: clear — Excel error {:?}, engine {:?}",
                        rec["error"],
                        r.err().map(|e| message(&e))
                    )
                });
            } else if excel_err || scored {
                tally.add(Outcome::Exact, String::new);
            }
        }
        "clear_formats" | "clear_all" => {
            let (sheet, a1) = place("range");
            let member = if op == "clear_all" {
                "Clear"
            } else {
                "ClearFormats"
            };
            let r = om.with(wb, |s| {
                let range = s.range(&sheet, &a1)?;
                s.method(range, member)
            });
            verdict(tally, op, n, rec, r.err().map(|e| message(&e)));
        }
        "format" => {
            let (sheet, a1) = place("range");
            let fmt = step["fmt"].as_str().unwrap().to_string();
            let _ = om.with(wb, |s| {
                let range = s.range(&sheet, &a1)?;
                s.put(range, "NumberFormat", Variant::Bstr(fmt))
            });
        }
        "precision" => {
            let on = step["value"].as_bool().unwrap_or(true);
            let _ = om.with(wb, |s| {
                let book = s.workbook()?;
                s.put(book, "PrecisionAsDisplayed", Variant::Bool(on))
            });
        }
        "read" => read(om, wb, cx, &default, typed, tally),
        "read_text" => read_text(om, wb, step, &default, rec, n, tally),
        _ => return false,
    }
    true
}

fn read_text(
    om: &mut OmSession,
    wb: &mut Workbook,
    step: &J,
    default: &str,
    rec: &J,
    n: usize,
    tally: &mut Tally,
) {
    let cells: Vec<&J> = rec["cells"].as_array().into_iter().flatten().collect();
    let names: Vec<&str> = cells.iter().map(|c| c["cell"].as_str().unwrap()).collect();
    let got = read_cells(
        om,
        wb,
        step["range"].as_str().unwrap(),
        default,
        &names,
        false,
    );
    for (c, d) in cells.iter().zip(got) {
        let a1 = c["cell"].as_str().unwrap();
        let d = match d {
            Ok(d) => d,
            Err(e) => {
                tally.add(Outcome::Wrong, || {
                    format!("step {n}: {a1} read failed: {e}")
                });
                continue;
            }
        };

        let (got2, got1) = (d.raw2.clone(), d.legacy.clone());
        for (what, want, got) in [
            ("Formula2", c["formula2"].as_str().unwrap_or(""), got2),
            ("Formula", c["formula"].as_str().unwrap_or(""), got1),
        ] {
            if !want.starts_with('=') && !got.starts_with('=') {
                continue;
            }
            let want = sheetsteps::norm_book(want);
            let got = sheetsteps::norm_book(&got);
            let o = if want == got {
                Outcome::Exact
            } else {
                Outcome::Wrong
            };
            tally.add(o, || {
                format!("step {n}: {a1} {what} expected {want:?}, got {got:?}")
            });
        }
        check_blocks(c, &d, n, tally);
    }
}

fn read(
    om: &mut OmSession,
    wb: &mut Workbook,
    cx: &Ctx,
    default: &str,
    typed: &BTreeMap<String, String>,
    tally: &mut Tally,
) {
    let Ctx {
        step,
        rec,
        n,
        default_sheet,
        flags,
        ..
    } = *cx;
    let range = step["range"].as_str().unwrap();
    let cells: Vec<&J> = rec["cells"].as_array().into_iter().flatten().collect();
    let Some((s, _)) = parse_range_ref(wb, range, default_sheet) else {
        verdict(
            tally,
            "read",
            n,
            rec,
            Some(format!("no such sheet in {range}")),
        );
        return;
    };
    let (sheet, _) = split_ref(range, default);
    for c in &cells {
        let a1 = c["cell"].as_str().unwrap();

        let d = match om.with(wb, |s| read_cell(s, &sheet, a1, true)) {
            Ok(d) => d,
            Err(e) => {
                tally.add(Outcome::Wrong, || {
                    format!("step {n}: {a1} read failed: {e}")
                });
                continue;
            }
        };

        let addr = parse_cell_ref(wb, a1, s).unwrap();
        let mut got = d.value.clone();
        if let Some(g) = flags
            .uses_clock
            .then(|| pinned_to_clock(wb, addr, &c["value"], &got))
            .flatten()
        {
            got = g;
        }
        if std::env::var("CONF_DUMP").is_ok_and(|v| v == n.to_string()) {
            eprintln!(
                "dump {a1} = {} {:?} fmt={:?} engine={}",
                show_value(&got),
                wb.formula_text(addr),
                wb.number_format(addr),
                show_value(&cell_result(wb, addr))
            );
        }
        check_blocks(c, &d, n, tally);
        let o = compare_value(&c["value"], &got);
        tally.add(o, || {
            format!(
                "step {n}: {} {a1} expected {}, got {}",
                range.split('!').next().unwrap(),
                show_expected(&c["value"]),
                show_value(&got)
            )
        });

        if flags.check_format && c["value"]["t"] == "n" {
            let sheet = range.split('!').next().unwrap();
            if let Some(input) = typed.get(&format!("{sheet}!{a1}")) {
                let want = c["format"].as_str().unwrap_or("General");

                let got = wb
                    .typed_format(input)
                    .unwrap_or_else(|| "General".to_string());
                let o = if want.eq_ignore_ascii_case(&got) {
                    Outcome::Exact
                } else {
                    Outcome::Wrong
                };
                tally.add(o, || {
                    format!("step {n}: {a1} {input:?} format expected {want:?}, got {got:?}")
                });
            }
        }
        if flags.check_all_formats
            && (flags.check_empty_formats || !c["value"].is_null() || c["formula"].is_string())
        {
            let want = c["format"].as_str().unwrap_or("General");
            let got = &d.format;
            let o = if want.eq_ignore_ascii_case(got) {
                Outcome::Exact
            } else {
                Outcome::Wrong
            };
            tally.add(o, || {
                format!("step {n}: {a1} number format expected {want:?}, got {got:?}")
            });
        }

        let want_f = c["formula"].as_str().map(sheetsteps::norm_book);
        let got_f = d.formula.as_deref().map(sheetsteps::norm_book);
        if want_f.is_some() || got_f.is_some() {
            let o = if want_f == got_f {
                Outcome::Exact
            } else {
                Outcome::Wrong
            };
            tally.add(o, || {
                format!("step {n}: {a1} formula expected {want_f:?}, got {got_f:?}")
            });
        }
    }
}
