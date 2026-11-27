use super::args::*;
use super::cellformat::{cell_format_code, cell_format_info};
use super::{args, lazy, scalar, scalar_p, FnDef};
use crate::eval::{EvalCtx, Operand};
use crate::value::{Value, XlError};
use crate::workbook::Entry;
use crate::x87;

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
    args("ISFORMULA", 1, 1, "R", isformula),
    args(FIELD_FN, 2, 3, "RRR", |_, o| {
        if o.len() == 3 {
            return Operand::err(XlError::NAME);
        }

        let (rows, cols) = match &o[0] {
            Operand::Val(Value::Array(a)) => (a.rows, a.cols),
            Operand::Ref(r) => r
                .single()
                .map_or((1, 1), |a| (a.rect.rows(), a.rect.cols())),
            _ => (1, 1),
        };
        let e = Value::Error(XlError::FIELD);
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
            Value::Lambda(_) => Value::Error(XlError::VALUE),
            _ => Value::Number(0.0),
        })
    }),
    args("TYPE", 1, 1, "R", type_fn),
    args("SHEET", 0, 1, "R", sheet_fn),
    args("SHEETS", 0, 1, "R", sheets_fn),
    args("CELL", 1, 2, "VR", cell_fn),
    scalar("INFO", 1, 1, info_fn),
    scalar("SINGLE", 1, 1, |_, a| a[0].clone()),
    scalar("WEBSERVICE", 1, 1, |_, a| fixed_err(a, XlError::VALUE)),
    scalar_p("CUBEVALUE", 1, super::MANY, "VR", |_, a| {
        fixed_err(a, XlError::NA)
    }),
    scalar_p("CUBEMEMBER", 2, 3, "VRV", |_, a| fixed_err(a, XlError::NA)),
    scalar("CUBEKPIMEMBER", 3, 4, |_, a| {
        let kind = if matches!(a[2], Value::Text(_)) {
            XlError::VALUE
        } else {
            XlError::NA
        };
        fixed_err(a, kind)
    }),
    scalar("CUBEMEMBERPROPERTY", 3, 3, |_, a| fixed_err(a, XlError::NA)),
    scalar("CUBERANKEDMEMBER", 3, 4, |c, a| cube_numeric(c, a, 2)),
    scalar_p("CUBESET", 2, 5, "VRV", |c, a| cube_numeric(c, a, 3)),
    scalar("CUBESETCOUNT", 1, 1, |_, a| fixed_err(a, XlError::NA)),
    scalar("RTD", 3, super::MANY, rtd),
    scalar("FIELDVALUE", 2, 2, |_, a| match a[1] {
        Value::Error(e) => Value::Error(e),
        _ => err(XlError::FIELD),
    }),
    scalar("STOCKHISTORY", 2, super::MANY254, |_, a| {
        let kind = if matches!(&a[0], Value::Text(t) if !t.is_empty()) {
            XlError::BLOCKED
        } else {
            XlError::VALUE
        };
        fixed_err(a, kind)
    }),
    scalar("IMAGE", 1, 5, |_, a| {
        a.iter()
            .find_map(|v| {
                if let Value::Error(e) = v {
                    Some(Value::Error(*e))
                } else {
                    None
                }
            })
            .unwrap_or_else(|| err(XlError::VALUE))
    }),
    super::checked(
        args("GETPIVOTDATA", 2, super::MANY, "R", |ctx, o| {
            for (i, op) in o.iter().enumerate() {
                let name_slot = i < 2 || i % 2 == 0;
                let multi =
                    matches!(op, Operand::Ref(r) if r.single().is_none_or(|a| !a.rect.is_cell()));

                if multi && i >= 2 && name_slot {
                    if let Operand::Ref(r) = op {
                        if let Some(a) = r.single() {
                            let (rows, cols) =
                                (a.rect.r1 - a.rect.r0 + 1, a.rect.c1 - a.rect.c0 + 1);
                            let e = Value::Error(XlError::REF);
                            return Operand::Val(
                                crate::value::Array::filled(rows, cols, e).into_value(),
                            );
                        }
                    }
                }
                if name_slot && !multi {
                    if let Operand::Val(Value::Error(e)) = op {
                        return Operand::err(*e);
                    }
                }
            }

            if let Operand::Ref(r0) = &o[0] {
                if let Some(a0) = r0.single() {
                    if let Some(ti) = ctx.wb.pivot_meeting(a0.sheet, &a0.rect) {
                        if o.len() != 2 {
                            return Operand::err(XlError::REF);
                        }
                        let text = match ctx.deref(o[1].clone()) {
                            Value::Text(s) => s.to_string(),
                            Value::Blank => String::new(),
                            v @ (Value::Number(_) | Value::Bool(_)) => crate::coerce::to_text(&v)
                                .map(|t| t.to_string())
                                .unwrap_or_default(),
                            _ => return Operand::err(XlError::REF),
                        };
                        return Operand::Val(ctx.wb.get_pivot_data_name(ti, &text));
                    }
                }
            }
            let Operand::Ref(r) = &o[1] else {
                return Operand::err(XlError::REF);
            };
            let Some(a) = r.single() else {
                return Operand::err(XlError::REF);
            };
            let anchor = crate::address::CellAddr::new(a.sheet, a.rect.r0, a.rect.c0);
            let data = ctx.deref(o[0].clone());
            let mut pairs = Vec::new();
            for ch in o[2..].chunks(2) {
                if ch.len() < 2 {
                    return Operand::err(XlError::REF);
                }
                let f = ctx.deref(ch[0].clone());
                let i = ctx.deref(ch[1].clone());
                pairs.push((f, i));
            }
            Operand::Val(
                ctx.wb
                    .get_pivot_data(&data, anchor, a.rect.is_cell(), &pairs),
            )
        }),
        |a| {
            if a.len() < 5 || a.len() % 2 == 0 {
                Ok(())
            } else {
                Err("field and item come in pairs")
            }
        },
    ),
    args("PHONETIC", 1, 1, "R", |_, o| {
        Operand::err(match &o[0] {
            Operand::Val(Value::Error(XlError::NAME)) => XlError::NAME,
            Operand::Val(Value::Error(XlError::REF)) => XlError::REF,
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
            _ => err(XlError::VALUE),
        })
    })())
}

fn cell_fn(ctx: &mut EvalCtx, ops: &[Operand]) -> Operand {
    let kind = match &ops[0] {
        Operand::Val(Value::Text(s)) => s.to_ascii_lowercase(),
        Operand::Val(Value::Error(e)) => return Operand::err(*e),
        _ => return Operand::err(XlError::VALUE),
    };
    let (sheet, row, col) = match ops.get(1) {
        None | Some(Operand::Missing) => {
            let s = ctx.wb.active_sheet();
            let (r, c) = ctx.wb.active_cell_of(s);
            (s, r, c)
        }
        Some(Operand::Ref(r)) => {
            let a = r.areas()[0];
            (a.sheet, a.rect.r0, a.rect.c0)
        }
        Some(Operand::Val(Value::Error(e))) => return Operand::err(*e),
        Some(_) => return Operand::err(XlError::VALUE),
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
            } else if let Some(p) = ctx.wb.ext_prefix(sheet) {
                Value::from(format!("{p}{a}"))
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

        "prefix" => {
            let at = crate::address::CellAddr::new(sheet, row, col);
            Value::from(match value() {
                Value::Text(_) if ctx.wb.formula_at(at).is_none() => {
                    use crate::style::HAlign;
                    match ctx.wb.cell_style(at).alignment.horizontal {
                        HAlign::Right => "\"",
                        HAlign::Center | HAlign::CenterAcrossSelection => "^",
                        HAlign::Fill => "\\",
                        _ => "'",
                    }
                }
                _ => "",
            })
        }
        "protect" => {
            let at = crate::address::CellAddr::new(sheet, row, col);
            Value::Number(if ctx.wb.cell_style(at).locked {
                1.0
            } else {
                0.0
            })
        }
        "color" | "parentheses" => {
            let at = crate::address::CellAddr::new(sheet, row, col);
            let i = cell_format_info(
                ctx.wb.number_format(at),
                ctx.wb.cell_style(at).implicit_general,
            );
            Value::Number(f64::from(u8::from(if kind.as_str() == "color" {
                i.color
            } else {
                i.parens
            })))
        }
        "format" => {
            let at = crate::address::CellAddr::new(sheet, row, col);
            Value::from(cell_format_code(
                ctx.wb.number_format(at),
                ctx.wb.cell_style(at).implicit_general,
            ))
        }
        "filename" => Value::from(""),

        "width" => {
            let (w, standard) = ctx.wb.cell_width(sheet, col);
            array_or_scalar(1, 2, |_, c| {
                if c == 0 {
                    Value::Number(w)
                } else {
                    Value::Bool(standard)
                }
            })
        }

        "sheet" => Value::Number(1.0),
        "coord" => Value::from(format!(
            "$A:${}${}",
            crate::address::col_to_letters(col),
            row + 1
        )),
        _ => return Operand::err(XlError::VALUE),
    };
    Operand::Val(v)
}

fn parity_op(ctx: &mut EvalCtx, op: &Operand, even: bool) -> Operand {
    let v = match op {
        Operand::Ref(r) => match r.areas() {
            [a] if a.rect.r0 == a.rect.r1 && a.rect.c0 == a.rect.c1 => {
                ctx.cell_value(a.sheet, a.rect.r0, a.rect.c0)
            }
            _ => return Operand::err(XlError::VALUE),
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
        return err(XlError::VALUE);
    }
    match ctx.to_number(v) {
        Ok(n) => {
            let a = x87::add(n.abs(), 1e-10).floor();
            let h = x87::div(a, 2.0);
            Value::Bool((h == h.floor()) == even)
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
        _ => Operand::err(XlError::VALUE),
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
        Some(Operand::Ref(r)) => {
            let id = r.areas()[0].sheet;
            if id.0 >= crate::workbook::ext::EXT_BASE {
                return Operand::err(XlError::NA);
            }
            id
        }
        Some(Operand::Val(Value::Text(name))) => match ctx.wb.sheet_by_name(name) {
            Some(s) => s.id,
            None => return Operand::err(XlError::NA),
        },
        Some(Operand::Val(Value::Error(e))) => return Operand::err(*e),
        Some(_) => return Operand::err(XlError::NA),
    };
    match ctx.wb.sheet_position(id) {
        Some(p) => Operand::Val(Value::Number(p as f64 + 1.0)),
        None => Operand::err(XlError::REF),
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

fn rtd(c: &mut EvalCtx, a: &[Value]) -> Value {
    let (Value::Text(prog), Some(source)) = (&a[0], c.wb.rtd_source.as_ref()) else {
        return fixed_err(a, XlError::NA);
    };
    if let Some(Value::Error(e)) = a.iter().find(|v| matches!(v, Value::Error(_))) {
        return Value::Error(*e);
    }
    let text = |v: &Value| match v {
        Value::Text(t) => t.to_string(),
        Value::Number(n) => crate::number::format_general(*n),
        Value::Bool(b) => (if *b { "TRUE" } else { "FALSE" }).to_string(),
        _ => String::new(),
    };
    let topics: Vec<String> = a[2..].iter().map(text).collect();
    source
        .topic(prog, &text(&a[1]), &topics)
        .unwrap_or(Value::Error(XlError::NA))
}

fn fixed_err(a: &[Value], e: XlError) -> Value {
    a.iter()
        .find_map(|v| match v {
            Value::Error(x) => Some(Value::Error(*x)),
            _ => None,
        })
        .unwrap_or(Value::Error(e))
}
