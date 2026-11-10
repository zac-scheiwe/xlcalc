use crate::macro_table::{Code, MACRO_ANSWERS};
use xlcalc::{CellAddr, Value, Workbook, XlError};

const MACRO_ONLY: &[u16] = &[
    53, 54, 55, 79, 80, 81, 84, 85, 87, 88, 90, 91, 92, 93, 94, 95, 96, 103, 104, 106, 107, 108,
    110, 122, 123, 132, 133, 134, 135, 136, 137, 138, 139, 145, 146, 147, 149, 150, 151, 152, 153,
    154, 155, 156, 157, 158, 159, 160, 161, 166, 170, 171, 172, 173, 174, 175, 176, 177, 178, 179,
    180, 181, 182, 185, 186, 187, 188, 191, 192, 200, 201, 202, 203, 217, 218, 223, 224, 225, 226,
    236, 237, 238, 239, 240, 241, 242, 243, 245, 246, 248, 249, 250, 251, 253, 254, 256, 258, 259,
    260, 262, 263, 264, 265, 266, 267, 268, 333, 334, 335, 338, 339, 340, 341, 348, 349, 355, 356,
    357,
];

const XLF_USER: i32 = 255;

const MACRO_ZERO_OK: &[i32] = &[
    54, 81, 84, 85, 87, 91, 93, 94, 95, 96, 103, 122, 123, 151, 157, 166, 170, 180, 181, 182, 191,
    192, 237, 238, 239, 240, 245, 248, 251,
];

const MACRO_COUNTS: &[(i32, u8)] = &[
    (53, 0b00010),
    (54, 0b00010),
    (55, 0b00010),
    (79, 0b00100),
    (80, 0b00100),
    (81, 0b01110),
    (84, 0b00110),
    (85, 0b00000),
    (87, 0b00010),
    (90, 0b00010),
    (91, 0b00111),
    (93, 0b00111),
    (94, 0b00000),
    (95, 0b00000),
    (96, 0b00010),
    (103, 0b00110),
    (106, 0b00010),
    (107, 0b00110),
    (122, 0b01110),
    (123, 0b00010),
    (145, 0b01110),
    (146, 0b00110),
    (147, 0b00110),
    (166, 0b00110),
    (170, 0b00110),
    (171, 0b11000),
    (172, 0b00010),
    (173, 0b00000),
    (174, 0b00000),
    (180, 0b00010),
    (182, 0b11110),
    (185, 0b00110),
    (186, 0b00010),
    (187, 0b00110),
    (188, 0b00110),
    (191, 0b01110),
    (223, 0b00000),
    (224, 0b00010),
    (225, 0b00000),
    (226, 0b01110),
    (237, 0b00010),
    (238, 0b00000),
    (241, 0b11100),
    (242, 0b11100),
    (245, 0b00000),
    (246, 0b11110),
    (248, 0b00010),
    (251, 0b00010),
    (258, 0b00110),
    (259, 0b01110),
    (262, 0b00010),
    (263, 0b00010),
    (268, 0b00110),
    (333, 0b11110),
    (335, 0b01100),
    (339, 0b00110),
    (340, 0b01110),
    (341, 0b11110),
    (348, 0b00110),
    (349, 0b00010),
    (357, 0b00110),
];

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

const LIFTED: &[u16] = &[8, 9, 75, 125, 216, 344, 345, 346, 347, 358, 360, 480];

const ARRAY_RESULT: &[u16] = &[8, 9, 50, 52, 164, 165];

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
    match wb.cfv_eval(&text, caller, caller) {
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
    if let Some((_, mask)) = MACRO_COUNTS.iter().find(|(n, _)| *n == xlfn) {
        if args.len() <= 4 && mask >> args.len() & 1 == 0 {
            return Outcome::BadCount;
        }
    }
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

fn call_if(args: &[Option<Value>]) -> Outcome {
    let first = |v: &Option<Value>| match v {
        Some(Value::Array(a)) => a.data.first().cloned().unwrap_or(Value::Blank),
        Some(v) => v.clone(),
        None => Value::Blank,
    };
    let err = Value::Error(XlError::VALUE);
    Outcome::Value(match args {
        [] => return Outcome::BadCount,
        [_] => err,
        [cond, rest @ ..] => match first(cond) {
            Value::Number(_) | Value::Bool(_) | Value::Blank => {
                let truthy = match first(cond) {
                    Value::Number(n) => n != 0.0,
                    Value::Bool(b) => b,
                    _ => false,
                };
                match (truthy, rest) {
                    (true, [yes, ..]) => first(yes),
                    (false, [_, no, ..]) => first(no),
                    (false, _) => Value::Bool(false),
                    (true, []) => err,
                }
            }
            _ => xlcalc::Array::new(1, 1, vec![err]).into_value(),
        },
    })
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
            Some(Some(Value::Text(_))) => Outcome::BadOperand,
            Some(_) => Outcome::Value(Value::Error(XlError::VALUE)),
        };
    }
    if macro_sheet && u16::try_from(xlfn).is_ok_and(|n| MACRO_ONLY.contains(&n)) {
        return if args.is_empty() && !MACRO_ZERO_OK.contains(&xlfn) {
            Outcome::BadCount
        } else if xlfn == 149 {
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
    let Some(name) = u16::try_from(xlfn)
        .ok()
        .filter(|n| !MACRO_ONLY.contains(n))
        .and_then(xlcalc_xlsx::function_name)
    else {
        return Outcome::Invalid;
    };
    if id_of(xlfn) == Some(1) {
        return call_if(args);
    }
    let Some(info) = xlcalc::function_info(name) else {
        return Outcome::Invalid;
    };
    if args.len() < usize::from(info.min_args) || args.len() > usize::from(info.max_args) {
        return Outcome::BadCount;
    }
    let id = xlfn as u16;
    let is_ref = |i: usize| matches!(refs.get(i), Some(Some(_)));
    let array_arg = args
        .iter()
        .enumerate()
        .filter(|(i, _)| !is_ref(*i))
        .filter_map(|(_, v)| v.as_ref())
        .find(|v| matches!(v, Value::Array(_)));
    let v = match array_arg {
        Some(Value::Array(shape)) if LIFTED.contains(&id) => {
            let (rows, cols) = (shape.rows, shape.cols);
            let mut data = Vec::new();
            for r in 0..rows {
                for c in 0..cols {
                    let at = |v: &Option<Value>| match v {
                        Some(Value::Array(a)) if a.rows == rows && a.cols == cols => {
                            Some(a.get(r, c).clone())
                        }
                        other => other.clone(),
                    };
                    let one: Vec<Option<Value>> = args.iter().map(at).collect();
                    data.push(eval(wb, caller, name, &one, refs));
                }
            }
            xlcalc::Array::new(rows, cols, data).into_value()
        }
        _ => eval(wb, caller, name, args, refs),
    };
    match v {
        v @ Value::Array(_) => Outcome::Value(v),
        v if ARRAY_RESULT.contains(&id) && !matches!(v, Value::Error(_)) => {
            Outcome::Value(xlcalc::Array::new(1, 1, vec![v]).into_value())
        }
        v => Outcome::Value(v),
    }
}
