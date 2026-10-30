use super::{Area, EvalCtx, Operand, Reference};
use crate::address::{CellAddr, Rect, SheetId, MAX_COLS, MAX_ROWS};
use crate::formula::{CRef, SheetRef};
use crate::parser::ast::BinOp;
use crate::value::{Value, XlError};
use crate::workbook::Entry;

pub fn resolve_rect(cref: &CRef, host: CellAddr, wrap: bool) -> Option<Rect> {
    let row = |rc: crate::formula::Rc| rc.resolve(host.row, MAX_ROWS, wrap);
    let col = |rc: crate::formula::Rc| rc.resolve(host.col, MAX_COLS, wrap);
    Some(match *cref {
        CRef::Cell { row: r, col: c } => Rect::cell(row(r)?, col(c)?),
        CRef::Area { r0, c0, r1, c1 } => Rect::new(row(r0)?, col(c0)?, row(r1)?, col(c1)?),
        CRef::Cols { c0, c1 } => Rect::new(0, col(c0)?, MAX_ROWS - 1, col(c1)?),
        CRef::Rows { r0, r1 } => Rect::new(row(r0)?, 0, row(r1)?, MAX_COLS - 1),
    })
}

pub fn resolve_sheets(ctx: &EvalCtx, sheet: &SheetRef) -> Vec<SheetId> {
    let wb = ctx.wb;
    match sheet {
        SheetRef::Current => vec![ctx.host.sheet],
        SheetRef::Sheet(id) => {
            if wb.sheet(*id).is_some() {
                vec![*id]
            } else {
                vec![]
            }
        }
        SheetRef::Span(a, b) => match (wb.sheet_position(*a), wb.sheet_position(*b)) {
            (Some(x), Some(y)) => {
                let (lo, hi) = (x.min(y), x.max(y));
                (lo..=hi).map(|i| wb.sheet_at(i).id).collect()
            }
            _ => vec![],
        },
        SheetRef::Missing(_) | SheetRef::External(_) => vec![],
    }
}

pub fn resolve(ctx: &mut EvalCtx, sheet: &SheetRef, cref: &CRef) -> Operand {
    let Some(rect) = resolve_rect(cref, ctx.host, ctx.wrap) else {
        return Operand::err(XlError::REF);
    };
    let sheets = resolve_sheets(ctx, sheet);
    match sheets.len() {
        0 if matches!(sheet, SheetRef::Sheet(_) | SheetRef::Span(..)) => Operand::err(XlError::REF),

        0 => {
            let n = rect.cell_count() as usize;
            if n > 1 && n <= 65536 {
                let (rows, cols) = (rect.rows(), rect.cols());
                Operand::Val(
                    crate::value::Array::new(rows, cols, vec![Value::Error(XlError::REF); n])
                        .into_value(),
                )
            } else {
                Operand::err(XlError::REF)
            }
        }
        1 => Operand::Ref(Reference::Area(Area {
            sheet: sheets[0],
            rect,
        })),
        _ => Operand::Ref(Reference::Span(
            sheets
                .into_iter()
                .map(|s| Area { sheet: s, rect })
                .collect(),
        )),
    }
}

pub fn eval_name(ctx: &mut EvalCtx, key: &str, sheet: Option<&SheetRef>) -> Operand {
    let scope = match sheet {
        None => Some(ctx.host.sheet),

        Some(SheetRef::Sheet(id)) if ctx.wb.sheet(*id).is_none() => None,

        Some(SheetRef::Missing(_)) => return Operand::err(XlError::NAME),
        Some(s) => match resolve_sheets(ctx, s).as_slice() {
            [id] => Some(*id),
            _ => return Operand::err(XlError::REF),
        },
    };
    let wb = ctx.wb;
    let def = match sheet {
        Some(_) if scope.is_some_and(|s| wb.names.was_deleted(key, s)) => wb.names.get(key, scope),
        Some(_) => wb.names.get(key, scope).or_else(|| wb.names.get(key, None)),

        None => match ctx.name_scope {
            Some(Some(sid))
                if ctx
                    .name_key
                    .as_ref()
                    .is_some_and(|n| wb.names.bound_global(sid, n, key)) =>
            {
                wb.names.get(key, None)
            }
            Some(Some(sid)) => wb.names.lookup(key, sid),
            Some(None) => wb.names.get(key, None),
            None => wb.names.lookup(key, ctx.host.sheet),
        },
    };
    let Some(def) = def else {
        return Operand::err(if sheet.is_some() && scope.is_none() {
            XlError::REF
        } else {
            XlError::NAME
        });
    };
    if ctx.depth > 64 {
        return Operand::err(XlError::NAME);
    }
    let formula = def.formula.clone();
    let env = std::mem::take(&mut ctx.env);
    let wrap = std::mem::replace(&mut ctx.wrap, true);
    ctx.depth += 1;
    let outer = ctx.name_scope.replace(def.scope);
    let outer_key = ctx
        .name_key
        .replace(crate::value::Text::from(def.name.to_uppercase()));
    let out = ctx.eval(&formula.root);
    ctx.name_key = outer_key;
    ctx.name_scope = outer;
    ctx.depth -= 1;
    ctx.wrap = wrap;
    ctx.env = env;
    out
}

pub fn struct_rect(
    wb: &crate::workbook::Workbook,
    table: &str,
    spec: &crate::workbook::tables::Spec,
    host: CellAddr,
) -> Result<Area, XlError> {
    let t = wb.table_by_name(table).ok_or(XlError::REF)?;
    let cols = match &spec.cols {
        Some((a, b)) => Some((
            t.column_index(a).ok_or(XlError::REF)?,
            t.column_index(b).ok_or(XlError::REF)?,
        )),
        None => None,
    };
    match t.resolve(spec.items, cols, host.row) {
        None => Err(XlError::REF),
        Some(Err(())) => Err(XlError::VALUE),
        Some(Ok(rect)) => Ok(Area {
            sheet: t.sheet,
            rect,
        }),
    }
}

pub fn struct_ref(ctx: &mut EvalCtx, table: &str, spec: &crate::workbook::tables::Spec) -> Operand {
    match struct_rect(ctx.wb, table, spec, ctx.host) {
        Ok(a) => Operand::Ref(Reference::Area(a)),
        Err(e) => Operand::err(e),
    }
}

pub fn implicit_intersection(ctx: &mut EvalCtx, v: Operand) -> Operand {
    match v {
        Operand::Ref(r) => {
            let Some(a) = r.single() else {
                if r.is_span() {
                    return Operand::err(XlError::REF);
                }
                return Operand::err(XlError::VALUE);
            };
            let rect = a.rect;
            if rect.is_cell() {
                return Operand::Ref(r);
            }
            let row = if rect.rows() == 1 {
                rect.r0
            } else if (rect.r0..=rect.r1).contains(&ctx.host.row) {
                ctx.host.row
            } else {
                return Operand::err(XlError::VALUE);
            };
            let col = if rect.cols() == 1 {
                rect.c0
            } else if (rect.c0..=rect.c1).contains(&ctx.host.col) {
                ctx.host.col
            } else {
                return Operand::err(XlError::VALUE);
            };
            Operand::Ref(Reference::Area(Area::cell(a.sheet, row, col)))
        }
        Operand::Val(Value::Array(a)) => Operand::Val(a.data[0].clone()),
        other => other,
    }
}

pub fn spill_ref(ctx: &mut EvalCtx, v: Operand) -> Operand {
    let Operand::Ref(r) = v else {
        return match v {
            Operand::Val(Value::Error(e)) => Operand::err(e),

            Operand::Val(Value::Array(a)) if a.data.len() > 1 => Operand::Val(
                crate::value::Array::new(
                    a.rows,
                    a.cols,
                    vec![Value::Error(XlError::VALUE); a.data.len()],
                )
                .into_value(),
            ),
            _ => Operand::err(XlError::VALUE),
        };
    };
    let Some(a) = r.single() else {
        return Operand::err(XlError::REF);
    };

    if a.rect.r0 != a.rect.r1 || a.rect.c0 != a.rect.c1 {
        return Operand::err(XlError::REF);
    }
    let (sheet, row, col) = (a.sheet, a.rect.r0, a.rect.c0);
    let wb = ctx.wb;
    let Some(sh) = wb.sheet(sheet) else {
        return Operand::err(XlError::REF);
    };
    let Some(Entry::Formula(fid)) = sh.cells.get(row, col) else {
        return Operand::err(XlError::REF);
    };
    let fid = *fid;

    let _ = ctx.formula_value(fid, row, col);
    if ctx.interrupted() {
        return Operand::err(XlError::CALC);
    }
    let f = wb.formula(fid);
    match f.spill {
        Some(rect) => Operand::Ref(Reference::Area(Area { sheet, rect })),

        None => Operand::Ref(Reference::Area(Area::cell(sheet, row, col))),
    }
}

pub fn ref_op(_ctx: &mut EvalCtx, op: BinOp, l: Operand, r: Operand) -> Operand {
    let (a, b) = match (l, r) {
        (Operand::Ref(a), Operand::Ref(b)) => (a, b),
        (Operand::Val(Value::Error(e)), _) | (_, Operand::Val(Value::Error(e))) => {
            return Operand::err(e)
        }
        _ => return Operand::err(XlError::VALUE),
    };
    let sheet = a.areas()[0].sheet;
    if a.areas().iter().chain(b.areas()).any(|x| x.sheet != sheet) {
        return Operand::err(XlError::VALUE);
    }
    match op {
        BinOp::Range => {
            let mut bound = a.areas()[0].rect;
            for x in a.areas().iter().chain(b.areas()) {
                bound = bound.union_bounds(&x.rect);
            }
            Operand::Ref(Reference::Area(Area { sheet, rect: bound }))
        }
        BinOp::Intersect => {
            let mut out = Vec::new();
            for x in a.areas() {
                for y in b.areas() {
                    if let Some(i) = x.rect.intersection(&y.rect) {
                        out.push(Area { sheet, rect: i });
                    }
                }
            }
            if out.is_empty() {
                Operand::err(XlError::NULL)
            } else {
                Operand::Ref(Reference::from_areas(out))
            }
        }
        BinOp::Union => {
            let out: Vec<Area> = a.areas().iter().chain(b.areas()).copied().collect();
            Operand::Ref(Reference::from_areas(out))
        }
        _ => Operand::err(XlError::VALUE),
    }
}
