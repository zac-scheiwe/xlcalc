use std::cmp::Ordering;

use super::{lift_values, type_rank, xl_cmp, xl_eq, Src};
use crate::address::{MAX_COLS, MAX_ROWS};
use crate::eval::{Area, EvalCtx, Operand};
use crate::value::{Value, XlError};

use super::super::args::{sfix, R};

struct Line {
    data: LineData,
    len: u32,
}

enum LineData {
    Cells(Vec<Value>),

    Array {
        a: std::sync::Arc<crate::value::Array>,
        vertical: bool,
        fixed: u32,
    },
}

impl Line {
    #[inline]
    fn get(&self, i: u32) -> Value {
        match &self.data {
            LineData::Cells(vals) => match vals.get(i as usize) {
                Some(v) => v.clone(),
                None => Value::Blank,
            },
            LineData::Array { a, vertical, fixed } => if *vertical {
                a.get(i, *fixed)
            } else {
                a.get(*fixed, i)
            }
            .clone(),
        }
    }

    fn materialized(&self) -> u32 {
        match &self.data {
            LineData::Cells(vals) => vals.len() as u32,
            LineData::Array { .. } => self.len,
        }
    }

    fn of(ctx: &mut EvalCtx, src: &Src, vertical: bool, fixed: u32) -> Line {
        match src {
            Src::Array(a) => {
                let len = if vertical { a.rows } else { a.cols };
                Line {
                    data: LineData::Array {
                        a: a.clone(),
                        vertical,
                        fixed,
                    },
                    len,
                }
            }
            Src::Area(area) => {
                let rect = if vertical {
                    crate::address::Rect::new(
                        area.rect.r0,
                        area.rect.c0 + fixed,
                        area.rect.r1,
                        area.rect.c0 + fixed,
                    )
                } else {
                    crate::address::Rect::new(
                        area.rect.r0 + fixed,
                        area.rect.c0,
                        area.rect.r0 + fixed,
                        area.rect.c1,
                    )
                };
                let len = if vertical { rect.rows() } else { rect.cols() };
                let used = ctx.wb.sheet(area.sheet).and_then(|s| s.bounded(&rect));
                let Some(used) = used else {
                    return Line {
                        data: LineData::Cells(Vec::new()),
                        len,
                    };
                };
                let n = if vertical {
                    used.r1 - rect.r0 + 1
                } else {
                    used.c1 - rect.c0 + 1
                };
                let mut vals = vec![Value::Blank; n as usize];
                let part = if vertical {
                    crate::address::Rect::new(rect.r0, rect.c0, used.r1, rect.c1)
                } else {
                    crate::address::Rect::new(rect.r0, rect.c0, rect.r1, used.c1)
                };
                ctx.visit_area(
                    &Area {
                        sheet: area.sheet,
                        rect: part,
                    },
                    |r, c, v| {
                        vals[(r + c) as usize] = v.clone();
                    },
                );
                Line {
                    data: LineData::Cells(vals),
                    len,
                }
            }
        }
    }
}

struct Pattern {
    toks: Vec<PTok>,
}

#[derive(Clone, Copy, PartialEq)]
enum PTok {
    Lit(u16),
    One,
    Any,
}

fn fold_unit(u: u16) -> u16 {
    match char::from_u32(u as u32) {
        Some(c) => {
            let mut l = c.to_lowercase();
            match (l.next(), l.next()) {
                (Some(x), None) if (x as u32) <= 0xFFFF => x as u32 as u16,
                _ => u,
            }
        }
        None => u,
    }
}

impl Pattern {
    fn new(p: &str) -> Pattern {
        let units: Vec<u16> = p.encode_utf16().collect();
        let mut toks = Vec::with_capacity(units.len());
        let mut i = 0;
        while i < units.len() {
            match units[i] {
                0x7E => {
                    if let Some(&n) = units.get(i + 1) {
                        toks.push(PTok::Lit(fold_unit(n)));
                    }
                    i += 2;
                    continue;
                }
                0x2A => {
                    if toks.last() != Some(&PTok::Any) {
                        toks.push(PTok::Any);
                    }
                }
                0x3F => toks.push(PTok::One),
                u => toks.push(PTok::Lit(fold_unit(u))),
            }
            i += 1;
        }
        Pattern { toks }
    }

    fn matches(&self, text: &str) -> bool {
        let t: Vec<u16> = text.encode_utf16().map(fold_unit).collect();
        let (mut ti, mut pi) = (0usize, 0usize);
        let mut star: Option<(usize, usize)> = None;
        loop {
            if pi == self.toks.len() && ti == t.len() {
                return true;
            }
            if pi < self.toks.len() {
                match self.toks[pi] {
                    PTok::Any => {
                        star = Some((pi, ti));
                        pi += 1;
                        continue;
                    }
                    PTok::One if ti < t.len() => {
                        ti += 1;
                        pi += 1;
                        continue;
                    }
                    PTok::Lit(c) if ti < t.len() && t[ti] == c => {
                        ti += 1;
                        pi += 1;
                        continue;
                    }
                    _ => {}
                }
            }
            match star {
                Some((sp, st)) if st < t.len() => {
                    star = Some((sp, st + 1));
                    pi = sp + 1;
                    ti = st + 1;
                }
                _ => return false,
            }
        }
    }
}

fn target_of(op: &Operand) -> R<Value> {
    match op {
        Operand::Val(Value::Error(e)) => Err(*e),
        Operand::Val(Value::Lambda(_)) => Err(XlError::VALUE),
        Operand::Val(v) => Ok(v.clone()),
        Operand::Missing => Ok(Value::Blank),
        Operand::Ref(_) => Err(XlError::VALUE),
    }
}

fn lookup_src(op: &Operand) -> R<Src> {
    match op {
        Operand::Missing => Err(XlError::VALUE),
        Operand::Ref(r) if r.is_span() => Err(XlError::VALUE),
        Operand::Ref(r) if r.single().is_none() => Err(XlError::NA),
        op => Src::from_operand(op),
    }
}

fn int_mode(ctx: &EvalCtx, op: Option<&Operand>, default: i32, allowed: &[i32]) -> R<i32> {
    let n = match super::opt_number(ctx, op)? {
        None => return Ok(default),
        Some(n) => sfix(n) as f64,
    };
    allowed
        .iter()
        .copied()
        .find(|&m| m as f64 == n)
        .ok_or(XlError::VALUE)
}

fn modes(ctx: &EvalCtx, mm: Option<&Operand>, sm: Option<&Operand>) -> R<(i32, i32)> {
    super::opt_number(ctx, mm)?;
    super::opt_number(ctx, sm)?;
    Ok((
        int_mode(ctx, mm, 0, &[0, -1, 1, 2])?,
        int_mode(ctx, sm, 1, &[1, -1, 2, -2])?,
    ))
}

fn xfind(line: &Line, t: &Value, mm: i32, sm: i32) -> Option<u32> {
    let len = line.len;
    if len == 0 {
        return None;
    }
    if sm.abs() == 2 {
        let (mut lo, mut hi) = (0i64, len as i64 - 1);
        let mut found = None;
        while lo <= hi {
            let mid = (lo + hi) / 2;
            let ord = xl_cmp(&line.get(mid as u32), t);
            match (ord, sm > 0) {
                (Ordering::Equal, true) => {
                    found = Some(mid);
                    hi = mid - 1;
                }
                (Ordering::Equal, false) => {
                    found = Some(mid);
                    lo = mid + 1;
                }
                (Ordering::Less, true) | (Ordering::Greater, false) => lo = mid + 1,
                (Ordering::Greater, true) | (Ordering::Less, false) => hi = mid - 1,
            }
        }
        if let Some(i) = found {
            return Some(i as u32);
        }

        let (smaller, larger) = if sm > 0 { (hi, lo) } else { (lo, hi) };
        let pick = match mm {
            -1 => smaller,
            1 => larger,
            _ => return None,
        };
        return (pick >= 0 && pick < len as i64).then_some(pick as u32);
    }

    let mat = line.materialized().min(len);
    let tail = mat < len;
    let order: Box<dyn Iterator<Item = u32>> = if sm > 0 {
        Box::new((0..mat).chain(tail.then_some(mat)))
    } else {
        Box::new(tail.then_some(len - 1).into_iter().chain((0..mat).rev()))
    };

    let pattern = match t {
        Value::Text(s) if mm == 2 => Some(Pattern::new(s)),
        _ => None,
    };
    if mm == 0 || mm == 2 {
        for i in order {
            let v = line.get(i);
            let hit = match &pattern {
                Some(p) => matches!(&v, Value::Text(s) if p.matches(s)),
                None => xl_eq(&v, t),
            };
            if hit {
                return Some(i);
            }
        }
        return None;
    }
    let mut best: Option<(u32, Value)> = None;
    for i in order {
        let v = line.get(i);
        let ord = xl_cmp(&v, t);
        if ord == Ordering::Equal {
            return Some(i);
        }
        let candidate = if mm < 0 {
            ord == Ordering::Less
        } else {
            ord == Ordering::Greater
        };
        if !candidate {
            continue;
        }
        let better = match &best {
            None => true,
            Some((_, b)) => {
                let o = xl_cmp(&v, b);
                if mm < 0 {
                    o == Ordering::Greater
                } else {
                    o == Ordering::Less
                }
            }
        };
        if better {
            best = Some((i, v));
        }
    }
    best.map(|(i, _)| i)
}

fn line_shape(src: &Src) -> R<(u32, Option<bool>)> {
    let (r, c) = (src.rows(), src.cols());
    if r > 1 && c > 1 {
        return Err(XlError::VALUE);
    }
    Ok(if r == 1 && c == 1 {
        (1, None)
    } else if c == 1 {
        (r, Some(true))
    } else {
        (c, Some(false))
    })
}

pub fn xmatch(ctx: &mut EvalCtx, ops: &[Operand]) -> Operand {
    let src = lookup_src(&ops[1]);
    let union_src = matches!(&ops[1], Operand::Ref(r) if r.single().is_none());
    let mut line: Option<Line> = None;
    lift_values(ctx, ops, &[0, 2, 3], |ctx, ops| {
        let r = (|| -> R<Value> {
            let target = target_of(&ops[0])?;

            if !union_src {
                src.as_ref().map_err(|e| *e)?;
            }
            let (mm, sm) = modes(ctx, ops.get(2), ops.get(3))?;
            let src = src.as_ref().map_err(|e| *e)?;
            let (_, orient) = line_shape(src)?;
            if mm == 2 && sm.abs() == 2 {
                return Err(XlError::VALUE);
            }
            let line = line.get_or_insert_with(|| Line::of(ctx, src, orient.unwrap_or(true), 0));
            match xfind(line, &target, mm, sm) {
                Some(i) => Ok(Value::Number(i as f64 + 1.0)),
                None => Err(XlError::NA),
            }
        })();
        match r {
            Ok(v) => Operand::Val(v),
            Err(e) => Operand::err(e),
        }
    })
}

pub fn xlookup(ctx: &mut EvalCtx, ops: &[Operand]) -> Operand {
    let src = lookup_src(&ops[1]);
    let ret = lookup_src(&ops[2]);
    let mut line: Option<Line> = None;

    let lifted = [0usize, 4, 5].iter().any(|&i| match ops.get(i) {
        Some(Operand::Val(Value::Array(_))) => true,
        Some(Operand::Ref(r)) => r.single().is_none_or(|a| !a.rect.is_cell()),
        _ => false,
    });

    let blank = matches!(ops.get(1), Some(Operand::Val(Value::Blank)))
        || matches!(ops.get(2), Some(Operand::Val(Value::Blank)));
    lift_values(ctx, ops, &[0, 4, 5], |ctx, ops| {
        let r = (|| -> R<Operand> {
            let target = target_of(&ops[0])?;
            if blank {
                return Err(XlError::VALUE);
            }
            let src = src.as_ref().map_err(|e| *e)?;
            let ret = ret.as_ref().map_err(|e| *e)?;
            let (mm, sm) = modes(ctx, ops.get(4), ops.get(5))?;
            let (len, orient) = line_shape(src)?;

            let vertical = match orient {
                Some(v) => v,
                None => {
                    if ret.rows() == 1 {
                        true
                    } else if ret.cols() == 1 {
                        false
                    } else {
                        return Err(XlError::VALUE);
                    }
                }
            };
            if (vertical && ret.rows() != len) || (!vertical && ret.cols() != len) {
                return Err(XlError::VALUE);
            }
            if mm == 2 && sm.abs() == 2 {
                return Err(XlError::VALUE);
            }
            let line = line.get_or_insert_with(|| Line::of(ctx, src, vertical, 0));
            match xfind(line, &target, mm, sm) {
                Some(i) => Ok(if vertical {
                    ret.slice(i, i, 0, ret.cols() - 1)
                } else {
                    ret.slice(0, ret.rows() - 1, i, i)
                }),
                None => match ops.get(3) {
                    None | Some(Operand::Missing) => Err(XlError::NA),
                    Some(Operand::Ref(r)) if r.single().is_none() => Err(XlError::VALUE),
                    Some(Operand::Ref(_)) if lifted => Err(XlError::VALUE),
                    Some(op) => Ok(op.clone()),
                },
            }
        })();
        r.unwrap_or_else(Operand::err)
    })
}

fn approx_cmp255(a: &Value, b: &Value) -> Ordering {
    match (a, b) {
        (Value::Text(x), Value::Text(y)) => xl_cmp(
            &Value::text(crate::text::cut255(x)),
            &Value::text(crate::text::cut255(y)),
        ),
        _ => xl_cmp(a, b),
    }
}

fn approx_find(line: &Line, t: &Value) -> Option<u32> {
    let len = line.len as i64;
    let mat = line.materialized() as i64;
    let kind = type_rank(t);
    let (mut lo, mut hi) = (0i64, len - 1);
    let mut best = None;
    while lo <= hi {
        let mid = (lo + hi) / 2;

        let mut i = mid;
        while i <= hi && i < mat && type_rank(&line.get(i as u32)) != kind {
            i += 1;
        }
        if i > hi || i >= mat {
            hi = mid - 1;
            continue;
        }
        match approx_cmp255(&line.get(i as u32), t) {
            Ordering::Equal => {
                let mut j = i;
                while j + 1 < mat && approx_cmp255(&line.get(j as u32 + 1), t) == Ordering::Equal {
                    j += 1;
                }
                return Some(j as u32);
            }
            Ordering::Less => {
                best = Some(i as u32);
                lo = i + 1;
            }
            Ordering::Greater => hi = i - 1,
        }
    }
    best
}

pub fn lookup(ctx: &mut EvalCtx, ops: &[Operand]) -> Operand {
    let src = lookup_src(&ops[1]);
    let mut line: Option<Line> = None;
    lift_values(ctx, ops, &[0], |ctx, ops| {
        match lookup_one(ctx, ops, &src, &mut line) {
            Ok(v) => Operand::Val(v),
            Err(e) => Operand::err(e),
        }
    })
}

fn lookup_one(
    ctx: &mut EvalCtx,
    ops: &[Operand],
    src: &R<Src>,
    line: &mut Option<Line>,
) -> R<Value> {
    {
        let target = match target_of(&ops[0])? {
            Value::Blank => Value::Number(0.0),
            v => v,
        };
        if let Operand::Val(Value::Error(e)) = &ops[1] {
            return Err(*e);
        }
        if matches!(ops.get(2), Some(Operand::Missing)) {
            return Err(XlError::VALUE);
        }
        if let Some(Operand::Val(Value::Error(e))) = ops.get(2) {
            return Err(*e);
        }
        let src = src.as_ref().map_err(|e| *e)?;
        let (rows, cols) = (src.rows(), src.cols());

        let vertical = !(cols > rows);
        let line = line.get_or_insert_with(|| Line::of(ctx, src, vertical, 0));
        let pos = approx_find(line, &target).ok_or(XlError::NA)?;
        let Some(result) = ops.get(2) else {
            return Ok(match src {
                Src::Array(a) => if vertical {
                    a.get(pos, a.cols - 1)
                } else {
                    a.get(a.rows - 1, pos)
                }
                .clone(),
                Src::Area(a) => {
                    if vertical {
                        ctx.cell_value(a.sheet, a.rect.r0 + pos, a.rect.c1)
                    } else {
                        ctx.cell_value(a.sheet, a.rect.r1, a.rect.c0 + pos)
                    }
                }
            });
        };
        match result {
            Operand::Val(Value::Error(e)) => Err(*e),
            Operand::Ref(r) => {
                let a = r.single().ok_or(XlError::NA)?;

                let (row, col) = if a.rect.rows() == 1 {
                    (a.rect.r0 as u64, a.rect.c0 as u64 + pos as u64)
                } else if a.rect.cols() == 1 {
                    (a.rect.r0 as u64 + pos as u64, a.rect.c0 as u64)
                } else {
                    return Err(XlError::NA);
                };

                if row >= MAX_ROWS as u64 || col >= MAX_COLS as u64 {
                    return Ok(Value::Blank);
                }
                Ok(ctx.cell_value(a.sheet, row as u32, col as u32))
            }
            Operand::Val(Value::Array(a)) => {
                if a.rows > 1 && a.cols > 1 || pos as usize >= a.len() {
                    Err(XlError::NA)
                } else {
                    Ok(a.data[pos as usize].clone())
                }
            }
            Operand::Val(v) => {
                if pos == 0 {
                    Ok(v.clone())
                } else {
                    Err(XlError::NA)
                }
            }
            Operand::Missing => Err(XlError::NA),
        }
    }
}
