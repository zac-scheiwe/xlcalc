use super::crit::{Criterion, Flavor};
use crate::address::{Rect, SheetId, MAX_COLS, MAX_ROWS};
use crate::eval::{Area, EvalCtx, Operand};
use crate::value::{Value, XlError};

type R<T> = Result<T, XlError>;

#[derive(Clone, Copy, PartialEq, Eq)]
pub enum Agg {
    Count,
    Sum,
    Average,
    Max,
    Min,
}

pub struct Window {
    pub clip: Option<Rect>,
    data: Vec<Value>,
}

impl Window {
    pub fn new(
        ctx: &mut EvalCtx,
        sheet: SheetId,
        r0: u32,
        c0: u32,
        rows: u32,
        cols: u32,
    ) -> R<Window> {
        Window::new_where(ctx, sheet, r0, c0, rows, cols, |_, _| true)
    }

    pub fn new_where(
        ctx: &mut EvalCtx,
        sheet: SheetId,
        r0: u32,
        c0: u32,
        rows: u32,
        cols: u32,
        keep: impl Fn(u32, u32) -> bool,
    ) -> R<Window> {
        let wb = ctx.wb;
        let Some(sh) = wb.sheet(sheet) else {
            return Err(XlError::REF);
        };
        let r1 = (r0 as u64 + rows as u64 - 1).min(MAX_ROWS as u64 - 1) as u32;
        let c1 = (c0 as u64 + cols as u64 - 1).min(MAX_COLS as u64 - 1) as u32;
        let Some(abs) = sh.bounded(&Rect { r0, c0, r1, c1 }) else {
            return Ok(Window {
                clip: None,
                data: Vec::new(),
            });
        };
        let w = abs.cols() as usize;
        let mut data = vec![Value::Blank; abs.rows() as usize * w];
        let (dr, dc) = (abs.r0 - r0, abs.c0 - c0);
        ctx.visit_area_where(
            &Area { sheet, rect: abs },
            |r, c| keep(r + dr, c + dc),
            |r, c, v| {
                data[r as usize * w + c as usize] = v.clone();
                true
            },
        );
        let clip = Rect {
            r0: abs.r0 - r0,
            c0: abs.c0 - c0,
            r1: abs.r1 - r0,
            c1: abs.c1 - c0,
        };
        Ok(Window {
            clip: Some(clip),
            data,
        })
    }

    pub fn of_area(ctx: &mut EvalCtx, a: &Area) -> R<Window> {
        Window::new(
            ctx,
            a.sheet,
            a.rect.r0,
            a.rect.c0,
            a.rect.rows(),
            a.rect.cols(),
        )
    }

    #[inline]
    pub fn get(&self, r: u32, c: u32) -> Option<&Value> {
        let k = self.clip?;
        if r < k.r0 || r > k.r1 || c < k.c0 || c > k.c1 {
            return None;
        }
        Some(&self.data[(r - k.r0) as usize * k.cols() as usize + (c - k.c0) as usize])
    }
}

pub fn area_of(op: &Operand) -> R<Area> {
    match op {
        Operand::Ref(r) => r.single().copied().ok_or(XlError::VALUE),
        Operand::Val(Value::Error(e)) => Err(*e),
        _ => Err(XlError::VALUE),
    }
}

pub fn crit_value(op: &Operand) -> Value {
    match op {
        Operand::Val(v) => v.clone(),
        _ => Value::Blank,
    }
}

fn dims(a: &Area) -> (u32, u32) {
    (a.rect.rows(), a.rect.cols())
}

fn evaluate(
    ctx: &mut EvalCtx,
    tests: &[(Window, Criterion)],
    values: Option<&Window>,
    rows: u32,
    cols: u32,
    agg: Agg,
) -> Value {
    if ctx.interrupted() {
        return Value::Error(XlError::CALC);
    }
    let blank = Value::Blank;
    let ctx: &EvalCtx = ctx;
    let all_match = |r: u32, c: u32| {
        tests
            .iter()
            .all(|(w, cr)| cr.matches(ctx, w.get(r, c).unwrap_or(&blank)))
    };
    let Some(vw) = values else {
        let all_blank = tests.iter().all(|(_, cr)| cr.matches(ctx, &blank));
        let total = rows as u64 * cols as u64;
        let bbox = tests
            .iter()
            .filter_map(|(w, _)| w.clip)
            .reduce(|a, b| a.union_bounds(&b));
        let mut n: u64 = 0;
        match bbox {
            Some(b) => {
                for r in b.r0..=b.r1 {
                    for c in b.c0..=b.c1 {
                        if all_match(r, c) {
                            n += 1;
                        }
                    }
                }
                if all_blank {
                    n += total - b.cell_count();
                }
            }
            None if all_blank => n = total,
            None => {}
        }
        return Value::Number(n as f64);
    };
    let mut sum = 0.0f64;
    let mut count = 0u64;
    let mut best: Option<f64> = None;
    if let Some(k) = vw.clip {
        for r in k.r0..=k.r1 {
            for c in k.c0..=k.c1 {
                let x = match vw.get(r, c) {
                    Some(Value::Number(x)) => *x,
                    Some(Value::Error(e)) => {
                        if all_match(r, c) {
                            return Value::Error(*e);
                        }
                        continue;
                    }
                    _ => continue,
                };
                if !all_match(r, c) {
                    continue;
                }
                match agg {
                    Agg::Sum | Agg::Average => {
                        sum = crate::x87::add(sum, x);
                        count += 1;
                    }
                    Agg::Max => best = Some(best.map_or(x, |m| if x > m { x } else { m })),
                    Agg::Min => best = Some(best.map_or(x, |m| if x < m { x } else { m })),
                    Agg::Count => count += 1,
                }
            }
        }
    }
    match agg {
        Agg::Sum => Value::num(sum),
        Agg::Average if count == 0 => Value::Error(XlError::DIV0),
        Agg::Average => Value::num(sum / count as f64),
        Agg::Max | Agg::Min => Value::num(best.unwrap_or(0.0)),
        Agg::Count => Value::Number(count as f64),
    }
}

fn lazy_window(
    ctx: &mut EvalCtx,
    sheet: SheetId,
    (r0, c0): (u32, u32),
    (rows, cols): (u32, u32),
    tests: &[(Window, Criterion)],
) -> R<Window> {
    let wb = ctx.wb;
    let Some(sh) = wb.sheet(sheet) else {
        return Err(XlError::REF);
    };
    let r1 = (r0 as u64 + rows as u64 - 1).min(MAX_ROWS as u64 - 1) as u32;
    let c1 = (c0 as u64 + cols as u64 - 1).min(MAX_COLS as u64 - 1) as u32;
    let abs = match sh.bounded(&Rect { r0, c0, r1, c1 }) {
        Some(a) if !tests.is_empty() => a,
        _ => return Window::new(ctx, sheet, r0, c0, rows, cols),
    };
    let (dr, dc, w) = (abs.r0 - r0, abs.c0 - c0, abs.cols());
    let blank = Value::Blank;
    let mask: Vec<bool> = {
        let ctx: &EvalCtx = ctx;
        (0..abs.rows())
            .flat_map(|r| (0..w).map(move |c| (r, c)))
            .map(|(r, c)| {
                tests
                    .iter()
                    .all(|(t, cr)| cr.matches(ctx, t.get(r + dr, c + dc).unwrap_or(&blank)))
            })
            .collect()
    };
    Window::new_where(ctx, sheet, r0, c0, rows, cols, move |r, c| {
        mask[((r - dr) * w + (c - dc)) as usize]
    })
}

fn run<T>(f: impl FnOnce() -> R<T>) -> R<T> {
    f()
}

pub fn countif(ctx: &mut EvalCtx, ops: &[Operand]) -> Operand {
    let r = run(|| {
        let area = area_of(&ops[0])?;
        let crit = Criterion::parse(ctx, &crit_value(&ops[1]), Flavor::Ifs)?;
        let (rows, cols) = dims(&area);
        let w = Window::of_area(ctx, &area)?;
        Ok(evaluate(ctx, &[(w, crit)], None, rows, cols, Agg::Count))
    });
    Operand::Val(r.unwrap_or_else(Value::Error))
}

pub fn sumif(ctx: &mut EvalCtx, ops: &[Operand], agg: Agg) -> Operand {
    let r = run(|| {
        if let Some(Operand::Val(Value::Error(e))) = ops.get(2) {
            return Err(*e);
        }
        let area = area_of(&ops[0])?;
        let crit = Criterion::parse(ctx, &crit_value(&ops[1]), Flavor::Ifs)?;
        let (rows, cols) = dims(&area);
        let target = match ops.get(2) {
            None | Some(Operand::Missing) => area,
            Some(op) => area_of(op)?,
        };
        let tests = [(Window::of_area(ctx, &area)?, crit)];
        let vw = lazy_window(
            ctx,
            target.sheet,
            (target.rect.r0, target.rect.c0),
            (rows, cols),
            &tests,
        )?;
        Ok(evaluate(ctx, &tests, Some(&vw), rows, cols, agg))
    });
    Operand::Val(r.unwrap_or_else(Value::Error))
}

pub fn ifs(ctx: &mut EvalCtx, ops: &[Operand], agg: Agg) -> Operand {
    let r = run(|| {
        let (values, pairs) = if agg == Agg::Count {
            (None, ops)
        } else {
            (Some(area_of(&ops[0])?), &ops[1..])
        };

        if pairs.is_empty() || pairs.len() % 2 != 0 {
            return Err(XlError::VALUE);
        }
        let mut areas = Vec::with_capacity(pairs.len() / 2);
        for p in pairs.chunks(2) {
            areas.push(area_of(&p[0])?);
        }
        let (rows, cols) = dims(values.as_ref().unwrap_or(&areas[0]));
        if areas.iter().any(|a| dims(a) != (rows, cols)) {
            return Err(XlError::VALUE);
        }
        let mut tests = Vec::with_capacity(areas.len());
        for (a, p) in areas.iter().zip(pairs.chunks(2)) {
            let crit = Criterion::parse(ctx, &crit_value(&p[1]), Flavor::Ifs)?;
            let w = lazy_window(ctx, a.sheet, (a.rect.r0, a.rect.c0), dims(a), &tests)?;
            tests.push((w, crit));
        }
        let vw = match &values {
            Some(a) => Some(lazy_window(
                ctx,
                a.sheet,
                (a.rect.r0, a.rect.c0),
                dims(a),
                &tests,
            )?),
            None => None,
        };
        Ok(evaluate(ctx, &tests, vw.as_ref(), rows, cols, agg))
    });
    Operand::Val(r.unwrap_or_else(Value::Error))
}
