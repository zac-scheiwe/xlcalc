use std::sync::Arc;

use super::crit::{truthy, Criterion, Flavor};
use super::ifs::{area_of, Window};
use super::stats;
use crate::address::{CellAddr, Rect, MAX_COLS};
use crate::coerce;
use crate::eval::refs::resolve_rect;
use crate::eval::{Area, EvalCtx, Operand};
use crate::formula::{walk, CRef, Node, Rc, SheetRef};
use crate::text::eq_ci;
use crate::value::{Array, Text, Value, XlError};

type R<T> = Result<T, XlError>;

#[derive(Clone, Copy, PartialEq, Eq, Debug)]
pub enum DKind {
    Sum,
    Average,
    Count,
    CountA,
    Get,
    Max,
    Min,
    Product,
    StDev,
    StDevP,
    Var,
    VarP,
}

pub fn dfunc(ctx: &mut EvalCtx, ops: &[Operand], kind: DKind) -> Operand {
    if let Some(Operand::Val(Value::Array(a))) = ops.get(2) {
        return Operand::Val(
            Array::filled(a.rows, a.cols, Value::Error(XlError::VALUE)).into_value(),
        );
    }

    let mut ops = ops.to_vec();
    let field = std::mem::replace(&mut ops[1], Operand::Missing);
    ops[1] = match field {
        Operand::Ref(r) => match r.single() {
            Some(a) if a.rect.is_cell() => match ctx.cell_value(a.sheet, a.rect.r0, a.rect.c0) {
                Value::Error(_) => Operand::err(XlError::VALUE),
                v => Operand::Val(v),
            },
            _ => Operand::err(XlError::VALUE),
        },
        other => other,
    };
    for op in &ops {
        if let Operand::Val(Value::Error(e)) = op {
            return Operand::err(*e);
        }
    }
    if let Operand::Val(Value::Array(fields)) = &ops[1] {
        let data = fields
            .data
            .iter()
            .map(|f| {
                let mut one = ops.clone();
                one[1] = Operand::Val(f.clone());
                run(ctx, &one, kind).unwrap_or_else(Value::Error)
            })
            .collect();
        return Operand::Val(Array::new(fields.rows, fields.cols, data).into_value());
    }
    Operand::Val(run(ctx, &ops, kind).unwrap_or_else(Value::Error))
}

fn header_key(v: &Value) -> Option<Text> {
    match v {
        Value::Blank | Value::Array(_) | Value::Lambda(_) => None,
        Value::Error(e) => Some(Text::from(e.as_str())),
        v => coerce::to_text(v).ok(),
    }
}

fn find_header(headers: &[Value], key: &str) -> Option<usize> {
    headers
        .iter()
        .position(|h| header_key(h).is_some_and(|k| eq_ci(&k, key)))
}

fn resolve_field(v: &Value, headers: &[Value]) -> R<usize> {
    match v {
        Value::Number(_) | Value::Bool(_) => {
            let n = match v {
                Value::Number(n) => n.trunc(),
                _ => matches!(v, Value::Bool(true)) as u8 as f64,
            };
            if n < 1.0 || n > headers.len() as f64 {
                Err(XlError::VALUE)
            } else {
                Ok(n as usize - 1)
            }
        }
        Value::Text(s) if s.is_empty() => headers
            .iter()
            .position(|h| matches!(h, Value::Blank) || matches!(h, Value::Text(t) if t.is_empty()))
            .ok_or(XlError::VALUE),
        Value::Text(s) => find_header(headers, s).ok_or(XlError::VALUE),
        Value::Error(e) => Err(*e),
        _ => Err(XlError::VALUE),
    }
}

enum Cond {
    Field(usize, Criterion),

    Computed {
        addr: CellAddr,
        root: Arc<crate::formula::Formula>,
    },
    Never,
}

fn run(ctx: &mut EvalCtx, ops: &[Operand], kind: DKind) -> R<Value> {
    let db = area_of(&ops[0])?;
    if db.rect.rows() < 2 {
        return Err(XlError::VALUE);
    }
    let ncols = db.rect.cols();
    let headers: Vec<Value> = (0..ncols)
        .map(|c| ctx.cell_value(db.sheet, db.rect.r0, db.rect.c0 + c))
        .collect();
    let field = match &ops[1] {
        Operand::Missing => None,
        Operand::Val(v) => Some(resolve_field(v, &headers)?),
        Operand::Ref(_) => return Err(XlError::VALUE),
    };
    if field.is_none() && !matches!(kind, DKind::Count | DKind::CountA) {
        return Err(XlError::VALUE);
    }
    let crit = match &ops[2] {
        Operand::Ref(r) => *r.single().ok_or(XlError::VALUE)?,
        Operand::Val(Value::Error(e)) => return Err(*e),
        _ => return Err(XlError::VALUE),
    };
    if crit.rect.rows() < 2 {
        return Err(XlError::VALUE);
    }
    let rows = conditions(ctx, &crit, &db, &headers)?;
    if ctx.interrupted() {
        return Err(XlError::CALC);
    }

    let wb = ctx.wb;
    let sh = wb.sheet(db.sheet).ok_or(XlError::REF)?;
    let all_cols = Rect {
        r0: db.rect.r0,
        c0: 0,
        r1: db.rect.r1,
        c1: MAX_COLS - 1,
    };
    let used_last = sh.bounded(&all_cols).map_or(db.rect.r0, |b| b.r1);
    let nrec = used_last.saturating_sub(db.rect.r0).min(db.rect.rows() - 1);
    let records = Window::new(
        ctx,
        db.sheet,
        db.rect.r0 + 1,
        db.rect.c0,
        nrec.max(1),
        ncols,
    )?;
    let blank = Value::Blank;
    let mut matched: Vec<u32> = Vec::new();
    for k in 0..nrec {
        if record_matches(ctx, &rows, &records, &db, k) {
            matched.push(k);
        }
        if ctx.interrupted() {
            return Err(XlError::CALC);
        }
    }
    let Some(col) = field else {
        return Ok(Value::Number(matched.len() as f64));
    };
    let vals: Vec<&Value> = matched
        .iter()
        .map(|&k| records.get(k, col as u32).unwrap_or(&blank))
        .collect();
    aggregate(kind, &vals)
}

fn aggregate(kind: DKind, vals: &[&Value]) -> R<Value> {
    match kind {
        DKind::Count => Ok(Value::Number(
            vals.iter()
                .filter(|v| matches!(v, Value::Number(_)))
                .count() as f64,
        )),
        DKind::CountA => Ok(Value::Number(
            vals.iter().filter(|v| !v.is_blank()).count() as f64
        )),
        DKind::Get => {
            let mut it = vals.iter().filter(|v| !v.is_blank());
            match (it.next(), it.next()) {
                (None, _) => Err(XlError::VALUE),
                (Some(v), None) => Ok((*v).clone()),
                _ => Err(XlError::NUM),
            }
        }
        _ => {
            let mut nums = Vec::with_capacity(vals.len());
            for v in vals {
                match v {
                    Value::Number(n) => nums.push(*n),
                    Value::Error(e) => return Err(*e),
                    _ => {}
                }
            }
            let r = match kind {
                DKind::Sum => nums.iter().fold(0.0, |a, x| a + x),
                DKind::Average => {
                    if nums.is_empty() {
                        return Err(XlError::DIV0);
                    }
                    nums.iter().fold(0.0, |a, x| a + x) / nums.len() as f64
                }
                DKind::Max => stats::max(&nums),
                DKind::Min => stats::min(&nums),
                DKind::Product => {
                    if nums.is_empty() {
                        0.0
                    } else {
                        nums.iter().fold(1.0, |a, x| a * x)
                    }
                }
                DKind::StDev => stats::stdev(&nums, true)?,
                DKind::StDevP => stats::stdev(&nums, false)?,
                DKind::Var => stats::var(&nums, true)?,
                DKind::VarP => stats::var(&nums, false)?,
                DKind::Count | DKind::CountA | DKind::Get => unreachable!(),
            };
            Ok(Value::num(r))
        }
    }
}

fn conditions(ctx: &mut EvalCtx, crit: &Area, db: &Area, headers: &[Value]) -> R<Vec<Vec<Cond>>> {
    let wb = ctx.wb;
    let sh = wb.sheet(crit.sheet).ok_or(XlError::REF)?;
    let Some(used) = sh.bounded(&crit.rect) else {
        return Ok(vec![Vec::new()]);
    };
    let last_row = used.r1.max(crit.rect.r0);
    let fields: Vec<Option<usize>> = (used.c0..=used.c1)
        .map(|c| {
            let h = ctx.cell_value(crit.sheet, crit.rect.r0, c);
            header_key(&h).and_then(|k| {
                if k.is_empty() {
                    None
                } else {
                    find_header(headers, &k)
                }
            })
        })
        .collect();
    let mut rows = Vec::new();
    for r in crit.rect.r0 + 1..=last_row {
        let mut conds = Vec::new();
        for (j, c) in (used.c0..=used.c1).enumerate() {
            let v = ctx.cell_value(crit.sheet, r, c);
            match fields[j] {
                Some(col) => {
                    if !v.is_blank() {
                        conds.push(Cond::Field(
                            col,
                            Criterion::parse(ctx, &v, Flavor::Database)?,
                        ));
                    }
                }
                None => {
                    let addr = CellAddr::new(crit.sheet, r, c);
                    if let Some(fid) = wb.formula_at(addr) {
                        let f = wb.formula(fid).formula.clone();
                        if needs_rebase(&f.root, addr, db) {
                            conds.push(Cond::Computed { addr, root: f });
                        } else if !eval_criterion(ctx, &f.root, addr) {
                            conds.push(Cond::Never);
                        }
                    } else if !v.is_blank() && !truthy(&v) {
                        conds.push(Cond::Never);
                    }
                }
            }
        }
        rows.push(conds);
    }
    if crit.rect.r1 > last_row {
        rows.push(Vec::new());
    }
    Ok(rows)
}

fn record_matches(
    ctx: &mut EvalCtx,
    rows: &[Vec<Cond>],
    records: &Window,
    db: &Area,
    k: u32,
) -> bool {
    let blank = Value::Blank;
    rows.iter().any(|conds| {
        conds.iter().all(|cond| match cond {
            Cond::Field(col, crit) => {
                crit.matches(ctx, records.get(k, *col as u32).unwrap_or(&blank))
            }
            Cond::Computed { addr, root } => {
                let node = rebase(&root.root, *addr, db, k as i32);
                eval_criterion(ctx, &node, *addr)
            }
            Cond::Never => false,
        })
    })
}

fn eval_criterion(ctx: &mut EvalCtx, node: &Node, addr: CellAddr) -> bool {
    let host = std::mem::replace(&mut ctx.host, addr);
    let env = std::mem::take(&mut ctx.env);
    let wrap = std::mem::replace(&mut ctx.wrap, false);
    let out = ctx.eval(node);
    let v = ctx.deref(out);
    ctx.host = host;
    ctx.env = env;
    ctx.wrap = wrap;
    truthy(&v)
}

fn row_relative(c: &CRef) -> bool {
    match c {
        CRef::Cell { row, .. } => !row.abs,
        CRef::Area { r0, r1, .. } | CRef::Rows { r0, r1 } => !r0.abs || !r1.abs,
        CRef::Cols { .. } => false,
    }
}

fn targets_db(sheet: &SheetRef, _host: CellAddr, db: &Area) -> bool {
    match sheet {
        SheetRef::Current => true,
        SheetRef::Sheet(id) => *id == db.sheet,
        _ => false,
    }
}

fn needs_rebase(root: &Node, host: CellAddr, db: &Area) -> bool {
    let mut any = false;
    walk(root, &mut |n| {
        if let Node::Ref(sheet, cref) = n {
            if row_relative(cref) && targets_db(sheet, host, db) {
                any = true;
            }
        }
    });
    any
}

fn shift_rows(c: &CRef, k: i32) -> CRef {
    let s = |rc: Rc| {
        if rc.abs {
            rc
        } else {
            Rc {
                v: rc.v + k,
                abs: false,
            }
        }
    };
    match *c {
        CRef::Cell { row, col } => CRef::Cell { row: s(row), col },
        CRef::Area { r0, c0, r1, c1 } => CRef::Area {
            r0: s(r0),
            c0,
            r1: s(r1),
            c1,
        },
        CRef::Rows { r0, r1 } => CRef::Rows {
            r0: s(r0),
            r1: s(r1),
        },
        cols @ CRef::Cols { .. } => cols,
    }
}

fn rebase(node: &Node, host: CellAddr, db: &Area, k: i32) -> Node {
    crate::formula::map_node(node, &mut |n| match n {
        Node::Ref(sheet, cref) if row_relative(cref) && targets_db(sheet, host, db) => {
            let records = Rect {
                r0: db.rect.r0 + 1,
                c0: db.rect.c0,
                r1: db.rect.r1,
                c1: db.rect.c1,
            };
            Some(
                match resolve_rect(&shift_rows(cref, k), host, false)
                    .and_then(|r| r.intersection(&records))
                {
                    Some(i) => {
                        let a = |v: u32| Rc {
                            v: v as i32,
                            abs: true,
                        };
                        Node::Ref(
                            SheetRef::Sheet(db.sheet),
                            CRef::Area {
                                r0: a(i.r0),
                                c0: a(i.c0),
                                r1: a(i.r1),
                                c1: a(i.c1),
                            },
                        )
                    }
                    None => Node::Lit(Value::Error(XlError::REF)),
                },
            )
        }
        _ => None,
    })
}
