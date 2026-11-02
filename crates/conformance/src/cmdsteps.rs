use serde_json::Value as J;
use xlcalc::Workbook;
use xlcalc_om::flags;
use xlcalc_variant::Variant;

use super::omsteps::{self, Arg, OmSession};
use super::Tally;

fn opt(v: Option<Variant>) -> Arg {
    v.map_or(Arg::Missing, Arg::Val)
}

pub fn run(
    om: &mut Option<OmSession>,
    wb: &mut Workbook,
    op: &str,
    step: &J,
    rec: &J,
    n: usize,
    tally: &mut Tally,
) -> bool {
    let text = |k: &str| step[k].as_str();
    let err = match op {
        "sort" => sort_step(om, wb, step).err(),
        "autofill" => {
            let ty = match text("type").unwrap_or("default") {
                "copy" => 1,
                "series" => 2,
                "formats" => 3,
                "values" => 4,
                "days" => 5,
                "weekdays" => 6,
                "months" => 7,
                "years" => 8,
                "linear" => 9,
                "growth" => 10,
                _ => 0,
            };
            omsteps::call_args(
                om,
                wb,
                &omsteps::ref_path(text("from").unwrap()),
                "AutoFill",
                flags::METHOD,
                &[
                    Arg::Path(omsteps::ref_path(text("to").unwrap())),
                    Arg::Val(Variant::I4(ty)),
                ],
            )
            .err()
        }
        "dataseries" => {
            let rowcol = text("rowcol").map(|r| Variant::I4(if r == "rows" { 1 } else { 2 }));
            let ty = text("type").and_then(|t| {
                Some(Variant::I4(match t {
                    "linear" => -4132,
                    "growth" => 2,
                    "date" => 3,
                    "autofill" => 4,
                    _ => return None,
                }))
            });
            let unit = text("date").and_then(|t| {
                Some(Variant::I4(match t {
                    "day" => 1,
                    "weekday" => 2,
                    "month" => 3,
                    "year" => 4,
                    _ => return None,
                }))
            });
            let args = [
                opt(rowcol),
                opt(ty),
                opt(unit),
                opt(step["step"].as_f64().map(Variant::R8)),
                opt(step["stop"].as_f64().map(Variant::R8)),
                opt(step["trend"].as_bool().map(Variant::Bool)),
            ];
            omsteps::call_args(
                om,
                wb,
                &omsteps::ref_path(text("range").unwrap()),
                "DataSeries",
                flags::METHOD,
                &args,
            )
            .err()
        }
        "paste_special" => {
            let paste = match text("paste").unwrap_or("all") {
                "values" => -4163,
                "formulas" => -4123,
                "formats" => -4122,
                _ => -4104,
            };
            let operation = match text("operation").unwrap_or("none") {
                "add" => 2,
                "sub" => 3,
                "mul" => 4,
                "div" => 5,
                _ => -4142,
            };
            let flag = |k: &str| Variant::Bool(step[k].as_bool().unwrap_or(false));

            let copied = omsteps::call(
                om,
                wb,
                &omsteps::ref_path(text("from").unwrap()),
                "Copy",
                flags::METHOD,
                &[],
            );
            let r = copied.and_then(|_| {
                omsteps::call(
                    om,
                    wb,
                    &omsteps::ref_path(text("to").unwrap()),
                    "PasteSpecial",
                    flags::METHOD,
                    &[
                        Variant::I4(paste),
                        Variant::I4(operation),
                        flag("skip_blanks"),
                        flag("transpose"),
                    ],
                )
            });
            let _ = omsteps::call(
                om,
                wb,
                "",
                "CutCopyMode",
                flags::PUT,
                &[Variant::Bool(false)],
            );
            r.err()
        }
        "replace" => {
            let lookat = if text("lookat") == Some("whole") {
                1
            } else {
                2
            };
            let order = if text("order") == Some("cols") { 2 } else { 1 };

            omsteps::call(
                om,
                wb,
                &omsteps::ref_path(text("range").unwrap()),
                "Replace",
                flags::METHOD,
                &[
                    Variant::Bstr(text("what").unwrap_or("").to_string()),
                    Variant::Bstr(text("with").unwrap_or("").to_string()),
                    Variant::I4(lookat),
                    Variant::I4(order),
                    Variant::Bool(step["matchcase"].as_bool() == Some(true)),
                ],
            )
            .err()
        }
        _ => return false,
    };
    omsteps::verdict(tally, op, rec, n, err);
    true
}

fn sort_step(om: &mut Option<OmSession>, wb: &mut Workbook, step: &J) -> Result<(), String> {
    let range = omsteps::ref_path(step["range"].as_str().unwrap());
    let sort = format!("{range}.Worksheet.Sort");
    let fields = format!("{sort}.SortFields");
    let cols = step["orient"].as_str() == Some("cols");
    omsteps::call(om, wb, &fields, "Clear", flags::METHOD, &[])?;
    for k in step["keys"].as_array().unwrap() {
        let item = if cols { "Rows" } else { "Columns" };
        let key = format!("{range}.{item}.Item({})", k["key"].as_u64().unwrap());
        let order = if k["order"].as_str() == Some("desc") {
            2
        } else {
            1
        };
        omsteps::call_args(
            om,
            wb,
            &fields,
            "Add",
            flags::METHOD,
            &[
                Arg::Path(key),
                Arg::Val(Variant::I4(0)),
                Arg::Val(Variant::I4(order)),
            ],
        )?;
    }
    omsteps::call_args(
        om,
        wb,
        &sort,
        "SetRange",
        flags::METHOD,
        &[Arg::Path(range)],
    )?;
    let header = step["header"].as_bool().unwrap_or(false);
    let match_case = step["matchcase"].as_bool() == Some(true);
    let puts = [
        ("Orientation", Variant::I4(if cols { 2 } else { 1 })),
        ("Header", Variant::I4(if header { 1 } else { 2 })),
        ("MatchCase", Variant::Bool(match_case)),
    ];
    for (member, v) in puts {
        omsteps::call(om, wb, &sort, member, flags::PUT, &[v])?;
    }
    omsteps::call(om, wb, &sort, "Apply", flags::METHOD, &[])?;
    Ok(())
}
