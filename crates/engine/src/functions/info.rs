use super::args::*;
use super::{args, lazy, scalar, volatile, FnDef};
use crate::eval::{EvalCtx, Operand};
use crate::value::{Value, XlError};
use crate::workbook::Entry;

pub const FIELD_FN: &str = "\u{1}FIELD";

pub const DEFS: &[FnDef] = &[
    lazy(crate::datatable::TABLE_FN, 0, 5, |ctx, _| {
        Operand::Val(ctx.wb.value(ctx.host))
    }),
    scalar("ISBLANK", 1, 1, |_, a| Value::Bool(a[0].is_blank())),
    scalar("ISNUMBER", 1, 1, |_, a| {
        Value::Bool(matches!(a[0], Value::Number(_)))
    }),
    scalar("ISTEXT", 1, 1, |_, a| {
        Value::Bool(matches!(a[0], Value::Text(_)))
    }),
    scalar("ISNONTEXT", 1, 1, |_, a| {
        Value::Bool(!matches!(a[0], Value::Text(_)))
    }),
    scalar("ISLOGICAL", 1, 1, |_, a| {
        Value::Bool(matches!(a[0], Value::Bool(_)))
    }),
    scalar("ISERROR", 1, 1, |_, a| Value::Bool(a[0].is_error())),
    scalar("ISERR", 1, 1, |_, a| {
        Value::Bool(matches!(a[0], Value::Error(e) if e != XlError::NA))
    }),
    scalar("ISNA", 1, 1, |_, a| {
        Value::Bool(matches!(a[0], Value::Error(XlError::NA)))
    }),
    args("ISEVEN", 1, 1, "R", |c, o| parity_op(c, &o[0], true)),
    args("ISODD", 1, 1, "R", |c, o| parity_op(c, &o[0], false)),
    args("ISREF", 1, 1, "R", |_, o| {
        Operand::Val(Value::Bool(matches!(
            o[0],
            Operand::Ref(ref r) if !matches!(r, crate::eval::Reference::Grid(_))
        )))
    }),
    args("ISFORMULA", 1, 1, "Q", isformula),
    args(FIELD_FN, 2, 3, "RRR", |_, o| {
        if o.len() == 3 {
            return Operand::err(XlError::Name);
        }

        let (rows, cols) = match &o[0] {
            Operand::Val(Value::Array(a)) => (a.rows, a.cols),
            Operand::Ref(r) => r
                .single()
                .map_or((1, 1), |a| (a.rect.rows(), a.rect.cols())),
            _ => (1, 1),
        };
        let e = Value::Error(XlError::Field);
        if rows * cols <= 1 {
            Operand::Val(e)
        } else {
            Operand::Val(crate::value::Array::filled(rows, cols, e).into_value())
        }
    }),
    scalar("NA", 0, 0, |_, _| err(XlError::NA)),
    scalar("ERROR.TYPE", 1, 1, |_, a| match a[0] {
        Value::Error(e) => Value::Number(e.type_number() as f64),
        _ => err(XlError::NA),
    }),
    args("N", 1, 1, "R", |c, o| {
        first_cell_or_lift(c, &o[0], |v| match v {
            Value::Number(n) => Value::Number(*n),
            Value::Bool(b) => Value::Number(if *b { 1.0 } else { 0.0 }),
            Value::Error(e) => Value::Error(*e),
            Value::Lambda(_) => Value::Error(XlError::Value),
            _ => Value::Number(0.0),
        })
    }),
    args("TYPE", 1, 1, "R", type_fn),
    args("SHEET", 0, 1, "R", sheet_fn),
    args("SHEETS", 0, 1, "R", sheets_fn),
    volatile(args("CELL", 1, 2, "VQ", cell_fn)),
    volatile(scalar("INFO", 1, 1, info_fn)),
    scalar("SINGLE", 1, 1, |_, a| a[0].clone()),
    scalar("WEBSERVICE", 1, 1, |_, a| fixed_err(a, XlError::Value)),
    scalar("CUBEVALUE", 1, super::MANY, |_, a| {
        fixed_err(a, XlError::NA)
    }),
    scalar("CUBEMEMBER", 2, 3, |_, a| fixed_err(a, XlError::NA)),
    scalar("CUBEKPIMEMBER", 3, 4, |_, a| fixed_err(a, XlError::Value)),
    scalar("CUBEMEMBERPROPERTY", 3, 3, |_, a| fixed_err(a, XlError::NA)),
    scalar("CUBERANKEDMEMBER", 3, 4, |c, a| cube_numeric(c, a, 2)),
    scalar("CUBESET", 2, 5, |c, a| cube_numeric(c, a, 3)),
    scalar("CUBESETCOUNT", 1, 1, |_, a| fixed_err(a, XlError::NA)),
    scalar("RTD", 3, super::MANY, |_, a| fixed_err(a, XlError::NA)),
    scalar("FIELDVALUE", 2, 2, |_, a| fixed_err(a, XlError::Field)),
    scalar("STOCKHISTORY", 2, 8, |_, a| fixed_err(a, XlError::Blocked)),
    scalar("IMAGE", 1, 5, |_, a| {
        a.iter()
            .find_map(|v| {
                if let Value::Error(e) = v {
                    Some(Value::Error(*e))
                } else {
                    None
                }
            })
            .unwrap_or_else(|| err(XlError::Value))
    }),
    args("GETPIVOTDATA", 2, super::MANY, "R", |_, _| {
        Operand::err(XlError::Ref)
    }),
    args("PHONETIC", 1, 1, "Q", |_, o| {
        Operand::err(match &o[0] {
            Operand::Val(Value::Error(XlError::Name)) => XlError::Name,
            Operand::Val(Value::Error(XlError::Ref)) => XlError::Ref,
            _ => XlError::NA,
        })
    }),
];

fn info_fn(ctx: &mut EvalCtx, a: &[Value]) -> Value {
    val((|| {
        let kind = text(&a[0])?.to_ascii_lowercase();
        Ok(match kind.as_str() {
            "system" => Value::from("pcdos"),
            "release" => Value::from("16.0"),
            "osversion" => Value::from("Windows (64-bit) NT 10.00"),
            "origin" => Value::from("$A:$A$1"),
            "recalc" => Value::from(match ctx.wb.settings.mode {
                crate::workbook::CalcMode::Automatic => "Automatic",
                crate::workbook::CalcMode::Manual => "Manual",
            }),

            "numfile" => Value::num(ctx.wb.sheets().count() as f64),
            "memavail" | "memused" | "totmem" => err(XlError::NA),
            _ => err(XlError::Value),
        })
    })())
}

fn cell_fn(ctx: &mut EvalCtx, ops: &[Operand]) -> Operand {
    let kind = match &ops[0] {
        Operand::Val(Value::Text(s)) => s.to_ascii_lowercase(),
        Operand::Val(Value::Error(e)) => return Operand::err(*e),
        _ => return Operand::err(XlError::Value),
    };
    let (sheet, row, col) = match ops.get(1) {
        None | Some(Operand::Missing) => (ctx.host.sheet, ctx.host.row, ctx.host.col),
        Some(Operand::Ref(r)) => {
            let a = r.areas()[0];
            (a.sheet, a.rect.r0, a.rect.c0)
        }
        Some(Operand::Val(Value::Error(e))) => return Operand::err(*e),
        Some(_) => return Operand::err(XlError::Value),
    };

    let extra: Vec<String> = match ops.get(1) {
        Some(Operand::Ref(r)) => r
            .areas()
            .iter()
            .skip(1)
            .map(|a| {
                let (r, col) = (a.rect, crate::address::col_to_letters);
                if r.is_full_rows() {
                    format!("${}:${}", r.r0 + 1, r.r1 + 1)
                } else if r.is_full_columns() {
                    format!("${}:${}", col(r.c0), col(r.c1))
                } else if r.is_cell() {
                    format!("${}${}", col(r.c0), r.r0 + 1)
                } else {
                    format!("${}${}:${}${}", col(r.c0), r.r0 + 1, col(r.c1), r.r1 + 1)
                }
            })
            .collect(),
        _ => Vec::new(),
    };
    let value = || -> Value { ctx.wb.value(crate::address::CellAddr::new(sheet, row, col)) };
    let v = match kind.as_str() {
        "row" => Value::Number(row as f64 + 1.0),
        "col" => Value::Number(col as f64 + 1.0),
        "address" => {
            let mut a = format!("${}${}", crate::address::col_to_letters(col), row + 1);
            for e in &extra {
                a.push(',');
                a.push_str(e);
            }
            if sheet == ctx.host.sheet {
                Value::from(a)
            } else {
                let name = ctx
                    .wb
                    .sheet(sheet)
                    .map(|s| s.name.clone())
                    .unwrap_or_default();

                let t = crate::parser::print::sheet_name_text(&name);
                let t = match t.strip_prefix('\x27') {
                    Some(rest) => format!("'[Book1]{rest}"),
                    None => format!("[Book1]{t}"),
                };
                Value::from(format!("{t}!{a}"))
            }
        }
        "contents" => {
            let v = ctx.cell_value(sheet, row, col);
            if v.is_blank() {
                Value::Number(0.0)
            } else {
                v
            }
        }
        "type" => Value::from(match value() {
            Value::Blank => "b",
            Value::Text(_) => "l",
            _ => "v",
        }),
        "prefix" => Value::from(match value() {
            Value::Text(_) => "'",
            _ => "",
        }),
        "protect" => Value::Number(1.0),
        "color" | "parentheses" => Value::Number(0.0),
        "format" => Value::from(cell_format_code(
            ctx.wb
                .number_format(crate::address::CellAddr::new(sheet, row, col)),
        )),
        "filename" => Value::from(""),

        "width" => array_or_scalar(1, 2, |_, c| {
            if c == 0 {
                Value::Number(8.0)
            } else {
                Value::Bool(true)
            }
        }),
        "sheet" => match ctx.wb.sheet_position(sheet) {
            Some(p) => Value::Number(p as f64 + 1.0),
            None => return Operand::err(XlError::Ref),
        },
        "coord" => Value::from(format!(
            "$A:${}${}",
            crate::address::col_to_letters(col),
            row + 1
        )),
        _ => return Operand::err(XlError::Value),
    };
    Operand::Val(v)
}

fn parity_op(ctx: &mut EvalCtx, op: &Operand, even: bool) -> Operand {
    let v = match op {
        Operand::Ref(r) => match r.areas() {
            [a] if a.rect.r0 == a.rect.r1 && a.rect.c0 == a.rect.c1 => {
                ctx.cell_value(a.sheet, a.rect.r0, a.rect.c0)
            }
            _ => return Operand::err(XlError::Value),
        },
        Operand::Val(v) => v.clone(),
        Operand::Missing => return Operand::err(XlError::NA),
    };
    Operand::Val(match v {
        Value::Array(a) => crate::value::Array::new(
            a.rows,
            a.cols,
            a.data.iter().map(|x| parity(ctx, x, even)).collect(),
        )
        .into_value(),
        v => parity(ctx, &v, even),
    })
}

fn parity(ctx: &EvalCtx, v: &Value, even: bool) -> Value {
    if matches!(v, Value::Bool(_)) {
        return err(XlError::Value);
    }
    match ctx.to_number(v) {
        Ok(n) => {
            let n = n.trunc();
            Value::Bool((n % 2.0 == 0.0) == even)
        }
        Err(e) => err(e),
    }
}

fn isformula(ctx: &mut EvalCtx, ops: &[Operand]) -> Operand {
    match &ops[0] {
        Operand::Ref(r) => {
            let a = r.areas()[0];
            let sheet = ctx.wb.sheet(a.sheet);
            let is = |row: u32, col: u32| {
                sheet
                    .and_then(|s| s.cells.get(row, col))
                    .is_some_and(|e| matches!(e, Entry::Formula(_)))
            };

            Operand::Val(super::args::array_or_scalar(
                a.rect.rows(),
                a.rect.cols(),
                |r, c| Value::Bool(is(a.rect.r0 + r, a.rect.c0 + c)),
            ))
        }
        Operand::Val(Value::Error(e)) => Operand::err(*e),
        _ => Operand::err(XlError::Value),
    }
}

fn type_fn(ctx: &mut EvalCtx, ops: &[Operand]) -> Operand {
    let v = match &ops[0] {
        Operand::Ref(crate::eval::Reference::Grid(_)) => return Operand::Val(Value::Number(64.0)),
        Operand::Ref(r) => match r.single() {
            Some(a) if a.rect.is_cell() => {
                let a = *a;
                ctx.cell_value(a.sheet, a.rect.r0, a.rect.c0)
            }
            Some(_) => return Operand::Val(Value::Number(64.0)),
            None => return Operand::Val(Value::Number(16.0)),
        },
        Operand::Val(v) => v.clone(),
        Operand::Missing => Value::Blank,
    };
    Operand::Val(Value::Number(match v {
        Value::Number(_) | Value::Blank => 1.0,
        Value::Text(_) => 2.0,
        Value::Bool(_) => 4.0,
        Value::Error(_) => 16.0,
        Value::Array(_) => 64.0,
        Value::Lambda(_) => 128.0,
    }))
}

fn sheet_fn(ctx: &mut EvalCtx, ops: &[Operand]) -> Operand {
    let id = match ops.first() {
        None | Some(Operand::Missing) => ctx.host.sheet,
        Some(Operand::Ref(r)) => r.areas()[0].sheet,
        Some(Operand::Val(Value::Text(name))) => match ctx.wb.sheet_by_name(name) {
            Some(s) => s.id,
            None => return Operand::err(XlError::NA),
        },
        Some(Operand::Val(Value::Error(e))) => return Operand::err(*e),
        Some(_) => return Operand::err(XlError::NA),
    };
    match ctx.wb.sheet_position(id) {
        Some(p) => Operand::Val(Value::Number(p as f64 + 1.0)),
        None => Operand::err(XlError::Ref),
    }
}

fn sheets_fn(ctx: &mut EvalCtx, ops: &[Operand]) -> Operand {
    match ops.first() {
        None | Some(Operand::Missing) => Operand::Val(Value::Number(ctx.wb.sheet_count() as f64)),
        Some(Operand::Ref(r)) => {
            let mut ids: Vec<_> = r.areas().iter().map(|a| a.sheet).collect();
            ids.dedup();
            Operand::Val(Value::Number(ids.len() as f64))
        }
        Some(Operand::Val(Value::Error(e))) => Operand::err(*e),
        Some(_) => Operand::err(XlError::NA),
    }
}

fn cube_numeric(c: &mut EvalCtx, a: &[Value], idx: usize) -> Value {
    if let Some(Value::Error(e)) = a.iter().find(|v| matches!(v, Value::Error(_))) {
        return Value::Error(*e);
    }
    match a.get(idx) {
        Some(v) => match c.to_number(v) {
            Ok(_) => err(XlError::NA),
            Err(e) => err(e),
        },
        None => err(XlError::NA),
    }
}
fn fixed_err(a: &[Value], e: XlError) -> Value {
    a.iter()
        .find_map(|v| match v {
            Value::Error(x) => Some(Value::Error(*x)),
            _ => None,
        })
        .unwrap_or(Value::Error(e))
}

fn cell_format_code(fmt: &str) -> String {
    let mut sections = fmt.split(';');
    let pos = sections.next().unwrap_or("");
    let neg = sections.next().unwrap_or("");

    let mut clean = String::new();
    let mut chars = pos.chars();
    while let Some(c) = chars.next() {
        match c {
            '"' => {
                for d in chars.by_ref() {
                    if d == '"' {
                        break;
                    }
                }
            }
            '\x5c' | '_' | '*' => {
                chars.next();
            }
            '[' => {
                for d in chars.by_ref() {
                    if d == ']' {
                        break;
                    }
                }
            }
            c => clean.push(c),
        }
    }
    let lower = clean.to_ascii_lowercase();
    let is_date = lower
        .chars()
        .any(|c| matches!(c, 'd' | 'm' | 'y' | 'h' | 's'))
        && !lower.contains("general")
        && !lower.contains("e+")
        && !lower.contains("e-");
    let mut code = if lower.trim() == "general" || lower.trim().is_empty() {
        "G".to_string()
    } else if is_date {
        match lower.trim() {
            "d-mmm-yy" | "dd/mm/yyyy" | "d/mm/yyyy" => "D1",
            "d-mmm" => "D2",
            "mmm-yy" => "D3",
            "m/d/yy" | "mm/dd/yy" | "m/d/yyyy" | "m/d/yy h:mm" | "mmmm d, yyyy" => "D4",
            "mm/dd" => "D5",
            "h:mm:ss am/pm" => "D6",
            "h:mm am/pm" => "D7",
            "h:mm:ss" => "D8",
            "h:mm" => "D9",
            _ => "G",
        }
        .to_string()
    } else if lower.contains('@') || lower.contains('?') {
        "G".to_string()
    } else {
        let dec = match lower.find('.') {
            Some(i) => lower[i + 1..]
                .chars()
                .take_while(|c| matches!(c, '0' | '#' | '?'))
                .count(),
            None => 0,
        };
        let b = lower.as_bytes();
        let grouped = (1..b.len().saturating_sub(1)).any(|i| {
            b[i] == b',' && matches!(b[i - 1], b'0' | b'#') && matches!(b[i + 1], b'0' | b'#')
        });
        let kind = if lower.contains('%') {
            'P'
        } else if lower.contains("e+") || lower.contains("e-") {
            'S'
        } else if lower.contains(['$', '€', '£', '¥']) {
            'C'
        } else if grouped {
            ','
        } else {
            'F'
        };
        format!("{kind}{dec}")
    };
    if code != "G" && neg.to_ascii_lowercase().contains("[red]") {
        code.push('-');
    }
    code
}
