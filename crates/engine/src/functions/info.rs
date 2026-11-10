use super::args::*;
use super::{args, lazy, scalar, scalar_p, volatile, FnDef};
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
    volatile(args("CELL", 1, 2, "VQ", cell_fn)),
    volatile(scalar("INFO", 1, 1, info_fn)),
    scalar("SINGLE", 1, 1, |_, a| a[0].clone()),
    scalar("WEBSERVICE", 1, 1, |_, a| fixed_err(a, XlError::VALUE)),
    scalar_p("CUBEVALUE", 1, super::MANY, "VR", |_, a| {
        fixed_err(a, XlError::NA)
    }),
    scalar_p("CUBEMEMBER", 2, 3, "VRV", |_, a| fixed_err(a, XlError::NA)),
    scalar("CUBEKPIMEMBER", 3, 4, |_, a| {
        let kind = if matches!(a[0], Value::Text(_)) {
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
    volatile(scalar("RTD", 3, super::MANY, rtd)),
    scalar("FIELDVALUE", 2, 2, |_, a| match a[1] {
        Value::Error(e) => Value::Error(e),
        _ => err(XlError::FIELD),
    }),
    scalar("STOCKHISTORY", 2, 8, |_, a| {
        let kind = if matches!(a[0], Value::Text(_)) {
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
        super::volatile(args("GETPIVOTDATA", 2, super::MANY, "R", |ctx, o| {
            for (i, op) in o.iter().enumerate() {
                let name_slot = i < 2 || i % 2 == 0;
                let multi =
                    matches!(op, Operand::Ref(r) if r.single().is_none_or(|a| !a.rect.is_cell()));
                if multi && i >= 2 && name_slot && ctx.legacy_form {
                    return Operand::err(XlError::VALUE);
                }

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
            Operand::Val(ctx.wb.get_pivot_data(&data, anchor, &pairs))
        })),
        |a| {
            if a.len() < 5 || a.len() % 2 == 0 {
                Ok(())
            } else {
                Err("field and item come in pairs")
            }
        },
    ),
    args("PHONETIC", 1, 1, "Q", |_, o| {
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
            let r = n.round();
            let n = if (n - r).abs() < 1.1642e-10 {
                r
            } else {
                n.trunc()
            };
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

struct CellFmtInfo {
    code: String,
    color: bool,
    parens: bool,
}

const LOCALE_MARK: char = '\u{1}';

const A_MARK: char = '\u{2}';

const ELAPSED_MARK: char = '\u{4}';

fn strip_section(sec: &str) -> (String, bool, bool) {
    let mut clean = String::new();
    let (mut elapsed, mut locale) = (false, false);
    let mut chars = sec.chars();
    while let Some(c) = chars.next() {
        match c {
            '"' => {
                let mut q = String::new();
                for d in chars.by_ref() {
                    if d == '"' {
                        break;
                    }
                    q.push(d);
                }

                if q == "$" {
                    clean.push('$');
                }
            }
            '\\' => {
                if let Some(d) = chars.next() {
                    if d.eq_ignore_ascii_case(&'a') {
                        clean.push(A_MARK);
                    }
                }
            }
            '_' | '*' => {
                chars.next();
            }
            '[' => {
                let mut b = String::new();
                for d in chars.by_ref() {
                    if d == ']' {
                        break;
                    }
                    b.push(d);
                }
                let l = b.to_ascii_lowercase();
                if !l.is_empty() && l.chars().all(|c| matches!(c, 'h' | 'm' | 's')) {
                    elapsed = true;
                    clean.push(ELAPSED_MARK);
                }

                if l.starts_with('$') && l.contains('-') {
                    locale = true;
                    clean.push(LOCALE_MARK);
                }
            }
            c => clean.push(c),
        }
    }
    (clean.to_ascii_lowercase(), elapsed, locale)
}

fn date_code(l: &str) -> &'static str {
    let l = l
        .replace("am/pm", "p")
        .replace(&format!("{A_MARK}/p"), "q")
        .replace("a/p", "q")
        .replace("/p", "")
        .replace("b1", &LOCALE_MARK.to_string());
    let mut toks: Vec<(char, usize)> = Vec::new();

    let mut spans: Vec<(usize, usize)> = Vec::new();
    let mut prev = ' ';
    for (at, c) in l.chars().enumerate() {
        if c == LOCALE_MARK {
            toks.push(('L', 1));
            spans.push((at, at));
        } else if c == ELAPSED_MARK {
            toks.push(('E', 1));
            spans.push((at, at));
        } else if matches!(c, 'd' | 'm' | 'y' | 'h' | 's' | 'p' | 'q' | 'e' | 'g' | 'a') {
            match toks.last_mut() {
                Some((t, n)) if *t == c && prev == c && c != 'p' && c != 'q' => {
                    *n += 1;
                    if let Some(sp) = spans.last_mut() {
                        sp.1 = at;
                    }
                }
                _ => {
                    toks.push((c, 1));
                    spans.push((at, at));
                }
            }
        }
        prev = c;
    }
    let chars: Vec<char> = l.chars().collect();

    let letter_between = |i: usize| {
        spans.get(i + 1).is_some_and(|next| {
            chars[spans[i].1 + 1..next.0]
                .iter()
                .any(|c| c.is_alphabetic())
        })
    };

    let mut kinds: Vec<char> = Vec::new();
    let mut elapsed = false;
    for (i, &(t, n)) in toks.iter().enumerate() {
        match t {
            'E' => {
                elapsed = true;

                if !kinds.is_empty() {
                    kinds.push('E');
                }
            }
            'm' => {
                let after_h = i > 0 && toks[i - 1].0 == 'h';
                let before_s = toks.get(i + 1).is_some_and(|x| x.0 == 's') && !letter_between(i);
                kinds.push(if (after_h || before_s || elapsed) && n <= 2 {
                    'n'
                } else if n <= 5 {
                    'm'
                } else {
                    'x'
                });
            }
            'L' => kinds.push('d'),
            'a' if n >= 3 => kinds.push('d'),
            'a' => {}
            t => kinds.push(t),
        }
    }
    let lead: String = kinds
        .iter()
        .take_while(|k| matches!(k, 'd' | 'm' | 'y'))
        .collect();
    for (shape, code) in [
        ("dmy", "D1"),
        ("dm", "D2"),
        ("my", "D3"),
        ("mdy", "D4"),
        ("md", "D5"),
    ] {
        if lead.starts_with(shape) {
            return code;
        }
    }
    let has = |c: char| kinds.contains(&c);
    if kinds.first() == Some(&'h') && !elapsed && has('n') {
        return match (has('s'), has('p')) {
            (true, true) => "D6",
            (false, true) => "D7",
            (true, false) => "D8",
            (false, false) => "D9",
        };
    }
    "G"
}

fn cell_format_info(fmt: &str, implicit_general: bool) -> CellFmtInfo {
    let mut sections = split_sections(fmt).into_iter();
    let pos = sections.next().unwrap_or("");
    let neg = sections.next();

    let has_color = |s: &str| {
        let l = s.to_ascii_lowercase();
        [
            "[red]",
            "[blue]",
            "[green]",
            "[black]",
            "[white]",
            "[yellow]",
            "[cyan]",
            "[magenta]",
            "[color",
        ]
        .iter()
        .any(|c| l.contains(c))
    };

    let neg_counts = neg.is_some_and(|n| !n.contains('@') && !(n == "General" && implicit_general));
    let color = neg_counts && (has_color(pos) || neg.is_some_and(has_color));
    let (lower, _, _) = strip_section(pos);

    let parens = {
        let mut it = pos.chars();
        let mut found = false;
        while let Some(c) = it.next() {
            match c {
                '"' => {
                    for d in it.by_ref() {
                        if d == '"' {
                            break;
                        }
                    }
                }
                '[' => {
                    for d in it.by_ref() {
                        if d == ']' {
                            break;
                        }
                    }
                }
                '\\' | '_' | '*' => {
                    it.next();
                }
                '(' => found = true,
                _ => {}
            }
        }
        found
    };
    let digits = lower.contains(['0', '#', '?']);
    let is_sci = (lower.contains("e+") || lower.contains("e-")) && digits;
    let has_dt = (lower.contains(['d', 'm', 'y', 'h', 's', ELAPSED_MARK])
        || lower.starts_with("b1")
        || lower.starts_with("b2"))
        && !lower.contains("general");
    let mut code = if fmt.contains(';') && pos.is_empty() {
        "H".to_string()
    } else if lower.trim().is_empty() || lower.contains("general") || lower.contains('@') {
        "G".to_string()
    } else if has_dt && !is_sci {
        date_code(&lower).to_string()
    } else if !digits || is_fraction(&lower) {
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
            b[i] == b','
                && matches!(b[i - 1], b'0' | b'#' | b'?')
                && matches!(b[i + 1], b'0' | b'#' | b'?')
        });
        let kind = if lower.contains('$') {
            'C'
        } else if is_sci {
            'S'
        } else if lower.contains('%') {
            'P'
        } else if grouped {
            ','
        } else {
            'F'
        };
        format!("{kind}{}", dec.min(15))
    };
    if code != "H" {
        if color {
            code.push('-');
        }
        if parens && code != "G" {
            code.push_str("()");
        }
    }
    CellFmtInfo {
        code,
        color,
        parens,
    }
}

fn split_sections(fmt: &str) -> Vec<&str> {
    let mut out = Vec::new();
    let (mut start, mut quote, mut bracket, mut skip) = (0, false, false, false);
    for (i, c) in fmt.char_indices() {
        if skip {
            skip = false;
            continue;
        }
        match c {
            '"' if !bracket => quote = !quote,
            '[' if !quote => bracket = true,
            ']' if !quote => bracket = false,
            '\\' | '_' | '*' if !quote && !bracket => skip = true,
            ';' if !quote && !bracket => {
                out.push(&fmt[start..i]);
                start = i + 1;
            }
            _ => {}
        }
    }
    out.push(&fmt[start..]);
    out
}

fn is_fraction(lower: &str) -> bool {
    let b = lower.as_bytes();
    (1..b.len().saturating_sub(1)).any(|i| {
        b[i] == b'/'
            && matches!(b[i - 1], b'0' | b'#' | b'?')
            && matches!(b[i + 1], b'0'..=b'9' | b'#' | b'?')
    })
}

fn cell_format_code(fmt: &str, implicit_general: bool) -> String {
    cell_format_info(fmt, implicit_general).code
}
