use serde_json::Value as J;
use xlcalc::Workbook;
use xlcalc_om::flags;
use xlcalc_variant::{SafeArray, VarType, Variant};

use super::omsteps::{self, ref_path, Arg, OmSession};
use super::{Outcome, Tally};

pub struct Ctx<'a> {
    pub op: &'a str,
    pub step: &'a J,
    pub rec: &'a J,
    pub n: usize,
}

fn array(items: Vec<Variant>) -> Variant {
    Variant::Array(Box::new(SafeArray {
        elem: VarType::Variant,
        bounds: vec![(0, items.len() as u32)],
        data: items,
    }))
}

fn ints(j: &J) -> Arg {
    Arg::Val(array(
        j.as_array()
            .map(|a| {
                a.iter()
                    .map(|c| Variant::I4(c.as_i64().unwrap_or(0) as i32))
                    .collect()
            })
            .unwrap_or_default(),
    ))
}

fn val(v: Variant) -> Arg {
    Arg::Val(v)
}

fn flag(step: &J, k: &str) -> Arg {
    val(Variant::Bool(step[k].as_bool().unwrap_or(false)))
}

fn text_or_missing(step: &J, k: &str) -> Arg {
    match step.get(k) {
        Some(_) => val(Variant::Bstr(step[k].as_str().unwrap_or("").to_string())),
        None => Arg::Missing,
    }
}

fn subtotal_code(name: &str) -> i32 {
    match name {
        "count" => -4112,
        "average" => -4106,
        "max" => -4136,
        "min" => -4139,
        "product" => -4149,
        "countnums" => -4113,
        "stdev" => -4155,
        "stdevp" => -4156,
        "var" => -4164,
        "varp" => -4165,
        _ => -4157,
    }
}

fn text_to_columns_args(step: &J) -> Vec<Arg> {
    let optional_ref = |k: &str| match step.get(k) {
        Some(_) => Arg::Path(ref_path(step[k].as_str().unwrap_or(""))),
        None => Arg::Missing,
    };
    let qualifier = match step["qualifier"].as_str() {
        Some("single") => 2,
        Some("none") => -4142,
        _ => 1,
    };
    let fieldinfo = match step["fieldinfo"].as_array() {
        Some(a) => val(array(
            a.iter()
                .map(|p| {
                    array(vec![
                        Variant::I4(p[0].as_i64().unwrap_or(0) as i32),
                        Variant::I4(p[1].as_i64().unwrap_or(0) as i32),
                    ])
                })
                .collect(),
        )),
        None => Arg::Missing,
    };
    let fixed = step["type"].as_str() == Some("fixed");
    vec![
        optional_ref("dest"),
        val(Variant::I4(if fixed { 2 } else { 1 })),
        val(Variant::I4(qualifier)),
        flag(step, "consecutive"),
        flag(step, "tab"),
        flag(step, "semicolon"),
        flag(step, "comma"),
        flag(step, "space"),
        val(Variant::Bool(step.get("other").is_some())),
        text_or_missing(step, "other"),
        fieldinfo,
        text_or_missing(step, "decimal"),
        text_or_missing(step, "thousands"),
        match step.get("trailing_minus") {
            Some(_) => flag(step, "trailing_minus"),
            None => Arg::Missing,
        },
    ]
}

pub fn run(om: &mut Option<OmSession>, wb: &mut Workbook, cx: &Ctx, tally: &mut Tally) -> bool {
    let (op, step, rec, n) = (cx.op, cx.step, cx.rec, cx.n);
    let refer = |k: &str| ref_path(step[k].as_str().unwrap_or(""));
    let path = match op {
        "goal_seek" => refer("cell"),
        "remove_dups" | "text_to_columns" | "subtotal" | "remove_subtotal" | "data_table" => {
            refer("range")
        }
        _ => return false,
    };
    let optional_ref = |k: &str| match step.get(k) {
        Some(_) => Arg::Path(refer(k)),
        None => Arg::Missing,
    };
    let (member, args) = match op {
        "remove_dups" => {
            let header = if step["header"].as_bool().unwrap_or(false) {
                1
            } else {
                2
            };
            (
                "RemoveDuplicates",
                vec![ints(&step["columns"]), val(Variant::I4(header))],
            )
        }
        "text_to_columns" => ("TextToColumns", text_to_columns_args(step)),
        "subtotal" => {
            let below = step["below"].as_bool().unwrap_or(true);
            let function = step["function"].as_str().unwrap_or("sum");
            (
                "Subtotal",
                vec![
                    val(Variant::I4(step["group"].as_i64().unwrap_or(1) as i32)),
                    val(Variant::I4(subtotal_code(function))),
                    ints(&step["totals"]),
                    val(Variant::Bool(step["replace"].as_bool().unwrap_or(true))),
                    flag(step, "pagebreaks"),
                    val(Variant::I4(i32::from(below))),
                ],
            )
        }
        "remove_subtotal" => ("RemoveSubtotal", Vec::new()),
        "data_table" => (
            "Table",
            vec![optional_ref("row_input"), optional_ref("col_input")],
        ),
        _ => (
            "GoalSeek",
            vec![
                val(Variant::R8(step["goal"].as_f64().unwrap())),
                Arg::Path(refer("changing")),
            ],
        ),
    };
    let result = omsteps::call_args(om, wb, &path, member, flags::METHOD, &args);
    if let (Ok(Variant::Bool(got)), "goal_seek", true) = (&result, op, rec["error"].is_null()) {
        let want = rec["result"].as_bool();
        tally.add(
            if want == Some(*got) {
                Outcome::Exact
            } else {
                Outcome::Wrong
            },
            || format!("step {n}: goal_seek — Excel {want:?}, engine {got}"),
        );
        return true;
    }
    omsteps::verdict(tally, op, rec, n, result.err());
    true
}
