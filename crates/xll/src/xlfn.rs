use crate::macro_table::{Code, MACRO_ANSWERS};
use crate::xlfn_ftab::{CETAB, FTAB};
use xlcalc::{CellAddr, Value, Workbook, XlError};

const XLF_USER: i32 = 255;

pub enum Outcome {
    Value(Value),

    Invalid,

    BadOperand,

    BadCount,

    Failed(i32),

    Flow,
}

fn literal(v: &Value, out: &mut String) {
    match v {
        Value::Blank => {}
        Value::Number(n) => out.push_str(&format!("{n:?}")),
        Value::Text(t) => {
            out.push('"');
            out.push_str(&t.replace('"', "\"\""));
            out.push('"');
        }
        Value::Bool(b) => out.push_str(if *b { "TRUE" } else { "FALSE" }),
        Value::Error(e) => out.push_str(e.as_str()),
        Value::Array(a) => {
            out.push('{');
            for r in 0..a.rows {
                if r > 0 {
                    out.push(';');
                }
                for c in 0..a.cols {
                    if c > 0 {
                        out.push(',');
                    }
                    literal(a.get(r, c), out);
                }
            }
            out.push('}');
        }
        Value::Lambda(_) => out.push_str("#VALUE!"),
    }
}

const ARRAY_RESULT: &[u16] = &[
    8, 9, 29, 50, 661, 52, 164, 165, 497, 582, 614, 615, 616, 625, 626, 630, 632, 634, 635, 636,
    650, 651, 652, 660,
];

const MOVES_OPERAND: &[u16] = &[615, 616];

fn arg_mask(id: u16, i: usize) -> Option<u32> {
    if matches!(id, 480 | 590) {
        Some(0x800017)
    } else {
        xlcalc::ifunc_arg_mask(id, i)
    }
}

fn operand_bit(v: &Value) -> u32 {
    match v {
        Value::Number(_) => 1,
        Value::Text(_) => 2,
        Value::Bool(_) => 4,
        _ => 0,
    }
}

fn coerces_to(v: &Value) -> u32 {
    match v {
        Value::Number(_) => 2 | 0x800 | 4 | 0x40,
        Value::Bool(_) => 1 | 2 | 0x800 | 0x40,
        _ => 1 | 0x800 | 4 | 0x40,
    }
}

fn eval_one(
    wb: &Workbook,
    caller: CellAddr,
    id: u16,
    name: &str,
    args: &[Option<Value>],
    refs: &[Option<String>],
) -> Value {
    let mut coerced: Option<Vec<Option<Value>>> = None;
    for (i, a) in args.iter().enumerate() {
        let Some(mask) = arg_mask(id, i) else {
            continue;
        };
        match a {
            Some(Value::Error(e)) if mask & 0x10 == 0 => return Value::Error(*e),

            Some(v @ (Value::Number(_) | Value::Bool(_) | Value::Text(_)))
                if !matches!(refs.get(i), Some(Some(_)))
                    && mask & operand_bit(v) == 0
                    && mask & coerces_to(v) == 0 =>
            {
                return Value::Error(XlError::VALUE);
            }
            Some(Value::Blank) if mask & 0x100 == 0 && !matches!(refs.get(i), Some(Some(_))) => {
                let v = if mask & 0x801 != 0 {
                    Value::Number(0.0)
                } else if mask & 2 != 0 {
                    Value::Text("".into())
                } else if mask & 4 != 0 {
                    Value::Bool(false)
                } else {
                    return Value::Error(XlError::VALUE);
                };
                coerced.get_or_insert_with(|| args.to_vec())[i] = Some(v);
            }
            _ => {}
        }
    }
    eval(wb, caller, name, coerced.as_deref().unwrap_or(args), refs)
}

fn id_of(xlfn: i32) -> Option<u16> {
    u16::try_from(xlfn).ok()
}

fn eval(
    wb: &Workbook,
    caller: CellAddr,
    name: &str,
    args: &[Option<Value>],
    refs: &[Option<String>],
) -> Value {
    let v = wb
        .eval_function_values(name, args, refs, caller, caller)
        .unwrap_or_else(|| {
            let mut text = format!("{name}(");
            for (i, a) in args.iter().enumerate() {
                if i > 0 {
                    text.push(',');
                }
                if let Some(Some(r)) = refs.get(i) {
                    text.push_str(r);
                } else if let Some(Value::Blank) = a {
                    text.push_str("XFD1048576");
                } else if let Some(v) = a {
                    literal(v, &mut text);
                }
            }
            text.push(')');
            wb.cfv_eval(&text, caller, caller)
        });
    match v {
        Value::Lambda(_) => Value::Error(XlError::VALUE),
        v => v,
    }
}

fn macro_call(
    wb: &Workbook,
    caller: CellAddr,
    xlfn: i32,
    args: &[Option<Value>],
    refs: &[Option<String>],
) -> Outcome {
    let Some(name) = u16::try_from(xlfn)
        .ok()
        .and_then(xlcalc_xlsx::function_name)
    else {
        return Outcome::Invalid;
    };
    let kinds = args
        .iter()
        .map(|a| match a {
            Some(Value::Number(n)) => format!("{}", n.trunc()),
            Some(Value::Text(_)) => "s".to_string(),
            _ => "o".to_string(),
        })
        .collect::<Vec<_>>()
        .join(",");
    if let Some((_, _, code)) = MACRO_ANSWERS
        .iter()
        .find(|(n, k, _)| i32::from(*n) == xlfn && *k == kinds)
    {
        return match *code {
            Code::Invalid => Outcome::Invalid,
            Code::Failed(e) => Outcome::Failed(e),
            Code::Err(e) => Outcome::Value(Value::Error(crate::xloper::error_from_code(e))),
            Code::Bool(b) => Outcome::Value(Value::Bool(b)),
            Code::Flow => Outcome::Flow,
        };
    }
    if let Some(v) = macro_value(wb, caller, xlfn, args, &kinds) {
        return Outcome::Value(v);
    }
    let mut text = format!("{name}(");
    for (i, a) in args.iter().enumerate() {
        if i > 0 {
            text.push(',');
        }
        if let Some(Some(r)) = refs.get(i) {
            text.push_str(r);
        } else if let Some(v) = a {
            literal(v, &mut text);
        }
    }
    text.push(')');
    let mut env = xlcalc::xlm::ReadOnlyEnv::new(wb);
    match xlcalc::xlm::eval_text(&mut env, &text, caller) {
        Err(_) => Outcome::BadCount,
        Ok(Err(_)) => Outcome::Failed(15),
        Ok(Ok(v)) => Outcome::Value(v),
    }
}

fn macro_value(
    wb: &Workbook,
    caller: CellAddr,
    xlfn: i32,
    args: &[Option<Value>],
    kinds: &str,
) -> Option<Value> {
    let sheet = wb.sheet(caller.sheet).map(|s| s.name.to_string())?;
    let book = wb.book_name();
    let full = format!("[{book}]{sheet}");
    let text = |s: String| Value::Text(s.into());
    let list = |items: &[&str]| {
        xlcalc::Array::new(
            1,
            items.len() as u32,
            items.iter().map(|s| text((*s).to_string())).collect(),
        )
        .into_value()
    };
    match (xlfn, kinds) {
        (91 | 93, "" | "1" | "3") => {
            let (docs, windows) = wb.session_books();
            let names = if xlfn == 91 { windows } else { docs };
            Some(list(&names.iter().map(String::as_str).collect::<Vec<_>>()))
        }
        (91, "2") | (93, "2" | "4") => Some(Value::Error(XlError::NA)),
        (107, "s") => Some(text("=#N/A".to_string())),
        (185, "1") => {
            let at = wb.cfv_eval(
                &format!("ADDRESS({},{})", caller.row + 1, caller.col + 1),
                caller,
                caller,
            );
            let Value::Text(at) = at else {
                return None;
            };

            let plain = |n: &str| {
                n.chars()
                    .all(|c| c.is_alphanumeric() || c == '_' || c == '.')
            };
            let head = if plain(book) && plain(&sheet) {
                full
            } else {
                format!("'{}'", full.replace('\'', "''"))
            };
            Some(text(format!("{head}!{at}")))
        }
        (186, "1") => Some(text("Windows (64-bit) NT 10.00".to_string())),
        (187 | 188, "1") => Some(text(full)),

        (188, "1,s") => match args.get(1) {
            Some(Some(Value::Text(t))) if t.eq_ignore_ascii_case(&sheet) => Some(text(full)),
            _ => Some(Value::Error(XlError::NA)),
        },
        (187, "1,s") => Some(text(full)),
        (241, "4,2" | "4,2,3" | "s,2" | "1,4") => {
            let first = match args.first() {
                Some(Some(Value::Text(t))) => t.to_string(),
                Some(Some(Value::Number(n))) => format!("{n}"),
                _ => String::new(),
            };
            Some(text(first))
        }
        (268, "4") => Some(Value::Number(wb.sheet_count() as f64)),
        (268, "1") => Some(
            xlcalc::Array::new(
                1,
                wb.sheet_count() as u32,
                wb.sheet_names()
                    .into_iter()
                    .map(|s| text(format!("[{book}]{s}")))
                    .collect(),
            )
            .into_value(),
        ),
        (349, "4") => Some(list(&[
            "January",
            "February",
            "March",
            "April",
            "May",
            "June",
            "July",
            "August",
            "September",
            "October",
            "November",
            "December",
        ])),
        (349, "1") => Some(list(&["Mon", "Tue", "Wed", "Thu", "Fri", "Sat", "Sun"])),
        _ => None,
    }
}

const FLIGHTED_ON: &[u16] = &[
    609, 611, 612, 613, 614, 615, 616, 623, 624, 625, 626, 627, 628, 629, 630, 631, 632, 633, 634,
    635, 636, 637, 638, 639, 640, 641, 647, 648, 650, 651, 652, 653, 654, 655, 656, 657, 658, 659,
    660, 661, 664, 686,
];

fn gate(ifunc: u16, macro_sheet: bool) -> Option<&'static (u16, u16, u8, u8)> {
    let row = FTAB.get(usize::from(ifunc))?;

    if ifunc >= 0x2c9 || (ifunc >= 0x25b && row.3 & 0x10 == 0) {
        return None;
    }
    if ifunc >= 0x25b && row.3 & 4 != 0 && !FLIGHTED_ON.contains(&ifunc) {
        return None;
    }
    if row.1 & 0x400 != 0 {
        return None;
    }
    if row.1 & 0x800 != 0 && !macro_sheet && ifunc != 89 && ifunc != 257 {
        return None;
    }
    Some(row)
}

pub fn call(
    wb: &Workbook,
    caller: CellAddr,
    xlfn: i32,
    args: &[Option<Value>],
    refs: &[Option<String>],
    macro_sheet: bool,
) -> Outcome {
    if xlfn == XLF_USER {
        return match args.first() {
            None => Outcome::BadCount,
            Some(Some(Value::Text(_) | Value::Error(_))) => Outcome::BadOperand,
            Some(_) => Outcome::Value(Value::Error(XlError::VALUE)),
        };
    }

    if args.len() >= 0x100 {
        return Outcome::BadCount;
    }
    if xlfn & 0x8000 != 0 {
        let idx = usize::try_from(xlfn & 0xfff).unwrap_or(usize::MAX);
        return match CETAB.get(idx) {
            Some(&max1) if macro_sheet && idx <= 0x5fb => {
                if args.len() >= usize::from(max1) {
                    Outcome::BadCount
                } else {
                    Outcome::Invalid
                }
            }
            _ => Outcome::Invalid,
        };
    }

    let xlfn = xlfn & !0x2000;
    let Some(&(max1, flags, min, _)) = u16::try_from(xlfn).ok().and_then(|n| gate(n, macro_sheet))
    else {
        return Outcome::Invalid;
    };
    if args.len() < usize::from(min) || args.len() >= usize::from(max1) {
        return Outcome::BadCount;
    }
    if flags & 0x800 != 0 && xlfn != 89 && xlfn != 257 {
        return if xlfn == 149 {
            Outcome::Value(Value::Error(XlError::VALUE))
        } else if xlfn == 256 {
            Outcome::Value(Value::Bool(false))
        } else {
            macro_call(wb, caller, xlfn, args, refs)
        };
    }
    if macro_sheet && xlfn == 1 && args.len() == 1 {
        return Outcome::Invalid;
    }
    let Some(name) =
        xlcalc_xlsx::function_name(xlfn as u16).or_else(|| xlcalc::ifunc_name(xlfn as u16))
    else {
        return Outcome::Invalid;
    };

    if matches!(id_of(xlfn), Some(639 | 640)) {
        return Outcome::Value(Value::Error(XlError::NA));
    }

    if id_of(xlfn) == Some(609) {
        return Outcome::Value(match args.first() {
            Some(Some(Value::Array(a))) => a.data.first().cloned().unwrap_or(Value::Blank),
            Some(Some(v)) => v.clone(),
            _ => Value::Blank,
        });
    }
    let id = xlfn as u16;

    if id == 1 && args.len() == 1 {
        return Outcome::Value(Value::Error(XlError::VALUE));
    }
    let is_ref = |i: usize| matches!(refs.get(i), Some(Some(_)));

    let if_lift = id == 1 && matches!(args.first(), Some(Some(Value::Array(_)))) && !is_ref(0);
    let lifted = |i: usize, v: &Option<Value>| {
        matches!(v, Some(Value::Array(_)))
            && !is_ref(i)
            && (if_lift || arg_mask(id, i).is_some_and(|m| m & 0x40 == 0))
    };
    let (mut rows, mut cols) = (0, 0);
    for (i, v) in args.iter().enumerate() {
        if let (true, Some(Value::Array(a))) = (lifted(i, v), v) {
            rows = rows.max(a.rows);
            cols = cols.max(a.cols);
        }
    }
    let v = if rows > 0 {
        let mut data = Vec::new();
        for r in 0..rows {
            for c in 0..cols {
                let one: Vec<Option<Value>> = args
                    .iter()
                    .enumerate()
                    .map(|(i, v)| match v {
                        Some(Value::Array(a)) if lifted(i, v) => {
                            let rr = if a.rows == 1 { 0 } else { r };
                            let cc = if a.cols == 1 { 0 } else { c };
                            Some(if rr < a.rows && cc < a.cols {
                                a.get(rr, cc).clone()
                            } else {
                                Value::Error(XlError::NA)
                            })
                        }
                        other => other.clone(),
                    })
                    .collect();

                if MOVES_OPERAND.contains(&id)
                    && (r, c) != (0, 0)
                    && !is_ref(0)
                    && args[0].is_some()
                {
                    data.push(
                        one.iter()
                            .skip(1)
                            .find_map(|o| match o {
                                Some(Value::Error(e)) => Some(Value::Error(*e)),
                                Some(Value::Text(t))
                                    if !t.eq_ignore_ascii_case("true")
                                        && !t.eq_ignore_ascii_case("false") =>
                                {
                                    Some(Value::Error(XlError::VALUE))
                                }
                                _ => None,
                            })
                            .unwrap_or(Value::Blank),
                    );
                    continue;
                }

                data.push(match eval_one(wb, caller, id, name, &one, refs) {
                    Value::Array(a) => a.data.first().cloned().unwrap_or(Value::Blank),
                    v => v,
                });
            }
        }
        xlcalc::Array::new(rows, cols, data).into_value()
    } else {
        match eval_one(wb, caller, id, name, args, refs) {
            Value::Array(a) if id == 1 => a.data.first().cloned().unwrap_or(Value::Blank),
            Value::Error(XlError::VALUE)
                if id == 1 && matches!(args.first(), Some(Some(Value::Text(_)))) =>
            {
                xlcalc::Array::new(1, 1, vec![Value::Error(XlError::VALUE)]).into_value()
            }
            v => v,
        }
    };
    match v {
        v @ Value::Array(_) => Outcome::Value(v),
        v if ARRAY_RESULT.contains(&id) && !matches!(v, Value::Error(_)) => {
            Outcome::Value(xlcalc::Array::new(1, 1, vec![v]).into_value())
        }
        v => Outcome::Value(v),
    }
}
