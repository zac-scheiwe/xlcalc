pub(crate) mod collate;
mod lookup;
mod matrix;
mod shape;

use std::cmp::Ordering;
use std::sync::Arc;

use super::args::*;
use super::{args, checked, FnDef, MANY254};
use crate::address::Rect;
use crate::eval::{Area, EvalCtx, Operand, Reference};
use crate::value::{Array, Value, XlError};
use crate::workbook::MAX_ARRAY_CELLS;

pub const DEFS: &[FnDef] = &[
    args("XLOOKUP", 3, 6, "R", lookup::xlookup),
    args("XMATCH", 2, 4, "R", lookup::xmatch),
    args("LOOKUP", 2, 3, "R", lookup::lookup),
    args("SORT", 1, 4, "ERRR", shape::sort),
    checked(args("SORTBY", 2, MANY254, "R", shape::sortby), sortby_pairs),
    args("UNIQUE", 1, 3, "RRR", shape::unique),
    args("TAKE", 2, 3, "RRR", |c, o| shape::take_drop(c, o, true)),
    args("DROP", 2, 3, "RRR", |c, o| shape::take_drop(c, o, false)),
    args("CHOOSEROWS", 2, MANY254, "MR", |c, o| {
        shape::choose_lines(c, o, true)
    }),
    args("CHOOSECOLS", 2, MANY254, "MR", |c, o| {
        shape::choose_lines(c, o, false)
    }),
    args("EXPAND", 2, 4, "RVVR", shape::expand),
    args("TOCOL", 1, 3, "MVV", |c, o| shape::to_line(c, o, true)),
    args("TOROW", 1, 3, "MVV", |c, o| shape::to_line(c, o, false)),
    args("WRAPROWS", 2, 3, "MVV", |c, o| shape::wrap(c, o, true)),
    args("WRAPCOLS", 2, 3, "MVV", |c, o| shape::wrap(c, o, false)),
    args("MMULT", 2, 2, "RR", matrix::mmult),
    args("MINVERSE", 1, 1, "R", matrix::minverse),
    args("MDETERM", 1, 1, "R", matrix::mdeterm),
    args("MUNIT", 1, 1, "V", matrix::munit),
    args("FORMULATEXT", 1, 1, "Q", formulatext),
];

fn sortby_pairs(a: &[crate::parser::ast::Expr]) -> Result<(), &'static str> {
    if a.len() == 2 || a.len() % 2 == 1 {
        Ok(())
    } else {
        Err("needs by_array/sort_order pairs")
    }
}

fn type_rank(v: &Value) -> u8 {
    match v {
        Value::Number(_) => 0,
        Value::Text(_) => 1,
        Value::Bool(_) => 2,
        Value::Error(_) => 3,
        Value::Blank => 4,
        Value::Array(_) | Value::Lambda(_) => 5,
    }
}

pub(crate) fn xl_cmp(a: &Value, b: &Value) -> Ordering {
    match (a, b) {
        (Value::Number(x), Value::Number(y)) => x.partial_cmp(y).unwrap_or(Ordering::Equal),
        (Value::Text(x), Value::Text(y)) => collate::compare(x, y),
        (Value::Bool(x), Value::Bool(y)) => x.cmp(y),
        (Value::Error(x), Value::Error(y)) => x.cmp(y),
        _ => type_rank(a).cmp(&type_rank(b)),
    }
}

pub(crate) fn xl_eq(a: &Value, b: &Value) -> bool {
    match (a, b) {
        (Value::Number(x), Value::Number(y)) => x == y,
        (Value::Text(x), Value::Text(y)) => collate::equal(x, y),
        (Value::Bool(x), Value::Bool(y)) => x == y,
        (Value::Error(x), Value::Error(y)) => x == y,
        (Value::Blank, Value::Blank) => true,
        _ => false,
    }
}

fn opt_number(ctx: &EvalCtx, op: Option<&Operand>) -> R<Option<f64>> {
    match op {
        None | Some(Operand::Missing) => Ok(None),
        Some(Operand::Val(v)) => ctx.to_number(v).map(Some),
        Some(Operand::Ref(_)) => Err(XlError::Value),
    }
}

fn arg_error(ops: &[Operand]) -> R<()> {
    for op in ops {
        if let Operand::Val(Value::Error(e)) = op {
            return Err(*e);
        }
    }
    Ok(())
}

fn arg_error_typed(ctx: &EvalCtx, ops: &[Operand], kinds: &str) -> R<()> {
    for (op, k) in ops.iter().zip(kinds.chars()) {
        match op {
            Operand::Val(Value::Error(e)) => return Err(*e),
            Operand::Val(v @ Value::Text(_)) => {
                if k == 'n' {
                    ctx.to_number(v)?;
                } else {
                    boolean(v)?;
                }
            }
            _ => {}
        }
    }
    Ok(())
}

fn opt_flag(op: Option<&Operand>) -> R<bool> {
    match op {
        None | Some(Operand::Missing) => Ok(false),
        Some(Operand::Val(v)) => boolean(v),
        Some(Operand::Ref(_)) => Err(XlError::Value),
    }
}

enum Src {
    Area(Area),
    Array(Arc<Array>),
}

impl Src {
    fn from_operand_3d(op: &Operand) -> R<Src> {
        match op {
            Operand::Ref(r) if r.is_span() && r.single().is_none() => Err(XlError::Ref),
            _ => Src::from_operand(op),
        }
    }

    fn from_operand(op: &Operand) -> R<Src> {
        Ok(match op {
            Operand::Ref(r) => Src::Area(*r.single().ok_or(XlError::Value)?),
            Operand::Val(Value::Error(e)) => return Err(*e),
            Operand::Val(Value::Array(a)) => Src::Array(a.clone()),
            Operand::Val(v) => Src::Array(Arc::new(Array::new(1, 1, vec![v.clone()]))),
            Operand::Missing => Src::Array(Arc::new(Array::new(1, 1, vec![Value::Blank]))),
        })
    }

    fn rows(&self) -> u32 {
        match self {
            Src::Area(a) => a.rect.rows(),
            Src::Array(a) => a.rows,
        }
    }

    fn cols(&self) -> u32 {
        match self {
            Src::Area(a) => a.rect.cols(),
            Src::Array(a) => a.cols,
        }
    }

    fn to_array(&self, ctx: &mut EvalCtx) -> R<Arc<Array>> {
        match self {
            Src::Array(a) => Ok(a.clone()),
            Src::Area(area) => {
                if area.rect.rows() as u64 * area.rect.cols() as u64 > MAX_ARRAY_CELLS {
                    return Err(XlError::Num);
                }
                Ok(match ctx.area_array(area) {
                    Value::Array(a) => a,
                    v => Arc::new(Array::new(1, 1, vec![v])),
                })
            }
        }
    }

    fn slice(&self, r0: u32, r1: u32, c0: u32, c1: u32) -> Operand {
        match self {
            Src::Area(a) => Operand::Ref(Reference::Area(Area {
                sheet: a.sheet,
                rect: Rect::new(
                    a.rect.r0 + r0,
                    a.rect.c0 + c0,
                    a.rect.r0 + r1,
                    a.rect.c0 + c1,
                ),
            })),
            Src::Array(a) => Operand::Val(array_or_scalar(r1 - r0 + 1, c1 - c0 + 1, |r, c| {
                a.get(r0 + r, c0 + c).clone()
            })),
        }
    }
}

fn lift_values(
    ctx: &mut EvalCtx,
    ops: &[Operand],
    positions: &[usize],
    mut body: impl FnMut(&mut EvalCtx, &[Operand]) -> Operand,
) -> Operand {
    let mut ops: Vec<Operand> = ops.to_vec();
    let mut shape: Option<(u32, u32)> = None;
    let mut lifted = Vec::new();
    for &i in positions {
        if i >= ops.len() || ops[i].is_missing() {
            continue;
        }
        let v = ctx.deref(std::mem::replace(&mut ops[i], Operand::Missing));
        if let Value::Array(a) = &v {
            let (r, c) = shape.unwrap_or((1, 1));
            shape = Some((r.max(a.rows), c.max(a.cols)));
            lifted.push(i);
        }
        ops[i] = Operand::Val(v);
    }
    if ctx.interrupted() {
        return Operand::err(XlError::Calc);
    }
    let Some((rows, cols)) = shape else {
        return body(ctx, &ops);
    };
    let mut data = Vec::with_capacity(rows as usize * cols as usize);
    let mut elem = ops.clone();
    for r in 0..rows {
        for c in 0..cols {
            for &i in &lifted {
                if let Operand::Val(Value::Array(a)) = &ops[i] {
                    elem[i] = Operand::Val(a.broadcast_get(r, c));
                }
            }
            let out = body(ctx, &elem);
            data.push(match ctx.deref_marked(out) {
                Value::Array(a) => a.data[0].clone(),
                other => other,
            });
            if ctx.interrupted() {
                return Operand::err(XlError::Calc);
            }
        }
    }
    Operand::Val(Array::new(rows, cols, data).into_value())
}

fn array_value(rows: u32, cols: u32, data: Vec<Value>) -> Value {
    Array::new(rows, cols, data).into_value()
}

fn check_result_size(rows: u64, cols: u64) -> R<()> {
    if rows * cols > MAX_ARRAY_CELLS {
        Err(XlError::Num)
    } else {
        Ok(())
    }
}

fn formulatext(ctx: &mut EvalCtx, ops: &[Operand]) -> Operand {
    let area = match &ops[0] {
        Operand::Ref(r) => r.areas()[0],
        Operand::Val(Value::Error(e)) => return Operand::err(*e),
        _ => return Operand::err(XlError::NA),
    };
    let wb = ctx.wb;
    let text_at = |row: u32, col: u32| -> Value {
        let addr = crate::address::CellAddr::new(area.sheet, row, col);
        match wb.formula_text(addr) {
            Some(t) => Value::from(normalize_formula(ctx_sheets(wb), &t)),
            None => Value::Error(XlError::NA),
        }
    };
    let rect = area.rect;
    if rect.is_cell() {
        return Operand::Val(text_at(rect.r0, rect.c0));
    }

    let (rows, cols) = (rect.rows(), rect.cols());
    if check_result_size(rows as u64, cols as u64).is_err() {
        return Operand::err(XlError::Num);
    }
    let used = wb.sheet(area.sheet).and_then(|s| s.bounded(&rect));
    Operand::Val(array_or_scalar(rows, cols, |r, c| {
        let (row, col) = (rect.r0 + r, rect.c0 + c);
        match used {
            Some(u) if u.contains(row, col) => text_at(row, col),
            _ => Value::Error(XlError::NA),
        }
    }))
}

fn formula_number(x: f64) -> String {
    let g = crate::number::format_general(x);
    let Some((mant, exp)) = g.split_once('E') else {
        return g;
    };
    let exp: i32 = exp.parse().unwrap_or(0);
    let (neg, mant) = match mant.strip_prefix('-') {
        Some(m) => (true, m),
        None => (false, mant),
    };
    let digits: String = mant.chars().filter(|c| c.is_ascii_digit()).collect();
    let n = digits.len() as i32;
    let fixed = if exp >= 0 {
        if exp + 1 >= n {
            format!("{}{}", digits, "0".repeat((exp + 1 - n) as usize))
        } else {
            format!(
                "{}.{}",
                &digits[..(exp + 1) as usize],
                &digits[(exp + 1) as usize..]
            )
        }
    } else {
        format!("0.{}{}", "0".repeat((-exp - 1) as usize), digits)
    };
    if fixed.len() > 21 {
        g
    } else if neg {
        format!("-{fixed}")
    } else {
        fixed
    }
}

fn ctx_sheets(wb: &crate::workbook::Workbook) -> impl Fn(&str) -> Option<String> + '_ {
    move |name: &str| wb.sheet_by_name(name).map(|s| s.name.clone())
}

fn normalize_formula(sheet_name: impl Fn(&str) -> Option<String>, text: &str) -> String {
    use crate::parser::ast::SheetSpec;
    use crate::parser::lexer::{Lexer, Tok};
    let body = text.strip_prefix('=').unwrap_or(text);
    let Ok(tokens) = Lexer::new(body).tokenize() else {
        return text.to_string();
    };
    let chars: Vec<char> = body.chars().collect();
    let is_ws = |c: char| matches!(c, ' ' | '\n' | '\r' | '\t' | '\u{a0}');
    let mut out = String::from("=");
    let mut prev_end = 0usize;
    for (i, t) in tokens.iter().enumerate() {
        let end = match tokens.get(i + 1) {
            Some(next) => {
                let mut e = next.pos;
                while e > t.pos && is_ws(chars[e - 1]) {
                    e -= 1;
                }
                e
            }
            None => chars.len(),
        };
        let gap: String = chars[prev_end.min(t.pos)..t.pos].iter().collect();
        let raw: String = chars[t.pos..end.max(t.pos)].iter().collect();
        prev_end = end.max(t.pos);
        let drop_gap = matches!(t.tok, Tok::Op(",") | Tok::Op(";") | Tok::End);
        if !drop_gap {
            out.push_str(&gap);
        }
        match &t.tok {
            Tok::End => {}
            Tok::Number { value, .. } => out.push_str(&formula_number(*value)),
            Tok::Error(e) => out.push_str(e.as_str()),
            Tok::Ident(s) => {
                let next_is_paren = matches!(tokens.get(i + 1).map(|n| &n.tok), Some(Tok::Op("(")));
                let near_colon = matches!(tokens.get(i + 1).map(|n| &n.tok), Some(Tok::Op(":")))
                    || (i > 0 && matches!(tokens[i - 1].tok, Tok::Op(":")));
                let upper = s.to_uppercase();
                let bare = s.replace('$', "");
                let is_ref = crate::address::parse_a1_cell(s).is_some()
                    || (near_colon
                        && !bare.is_empty()
                        && bare.chars().all(|c| c.is_ascii_alphabetic())
                        && crate::address::letters_to_col(&bare).is_some());
                if next_is_paren || is_ref || upper == "TRUE" || upper == "FALSE" {
                    out.push_str(&upper);
                } else {
                    out.push_str(s);
                }
            }
            Tok::Sheet(SheetSpec::Single(n)) => {
                let name = sheet_name(n).unwrap_or_else(|| n.clone());
                out.push_str(&crate::parser::print::sheet_name_text(&name));
                out.push('!');
            }
            _ => out.push_str(&raw),
        }
    }
    out
}
