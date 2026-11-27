use super::super::args::R;
use super::{array_value, check_result_size, Src};
use crate::eval::{EvalCtx, Operand};
use crate::value::{Value, XlError};
use crate::x87;

fn finish(r: R<Value>) -> Operand {
    match r {
        Ok(v) => Operand::Val(v),
        Err(e) => Operand::err(e),
    }
}

fn matrix_of(ctx: &mut EvalCtx, op: &Operand) -> R<(usize, usize, Vec<f64>)> {
    let a = Src::from_operand(op)?.to_array(ctx)?;
    matrix_nums(a)
}

fn matrix_nums(a: std::sync::Arc<crate::value::Array>) -> R<(usize, usize, Vec<f64>)> {
    let mut out = Vec::with_capacity(a.len());
    for v in &a.data {
        match v {
            Value::Number(n) => out.push(*n),
            Value::Error(e) => return Err(*e),
            _ => return Err(XlError::VALUE),
        }
    }
    Ok((a.rows as usize, a.cols as usize, out))
}

fn nz(x: f64) -> bool {
    x.to_bits() & 0xffff_0000_0000_0000 != 0
}

fn eliminate(flag: &mut x87::MathFlag, n: usize, w: usize, m: &mut [f64]) -> bool {
    let mut odd = false;
    for p in 0..n.saturating_sub(1) {
        let mut best = p;
        for i in p + 1..n {
            let d = x87::sub(m[i * w + p].abs(), m[best * w + p].abs());
            if nz(d) && d.is_sign_positive() {
                best = i;
            }
        }
        if best != p {
            odd = !odd;
            for k in p..w {
                m.swap(p * w + k, best * w + k);
            }
        }
        for i in p + 1..n {
            if !nz(m[i * w + p]) {
                continue;
            }
            let f = flag.div(m[i * w + p], m[p * w + p]);
            for k in p + 1..w {
                let mut t = flag.mul(m[p * w + k], f);
                if nz(t) {
                    t = -t;
                }
                m[i * w + k] = flag.add(t, m[i * w + k]);
            }
        }
    }
    odd
}

pub fn mdeterm(ctx: &mut EvalCtx, ops: &[Operand]) -> Operand {
    finish((|| {
        let (rows, cols, mut m) = matrix_of(ctx, &ops[0])?;
        if rows != cols {
            return Err(XlError::VALUE);
        }

        let n = rows;
        let mut flag = x87::MathFlag::default();
        let odd = eliminate(&mut flag, n, n, &mut m);
        let mut d = m[0];
        for i in 1..n {
            d = flag.mul(d, m[i * n + i]);
        }
        if odd && nz(d) {
            d = -d;
        }
        Ok(Value::num(flag.result(d).map_err(|_| XlError::NUM)?))
    })())
}

pub fn minverse(ctx: &mut EvalCtx, ops: &[Operand]) -> Operand {
    finish((|| {
        let (rows, cols, a) = matrix_of(ctx, &ops[0])?;
        if rows != cols {
            return Err(XlError::VALUE);
        }

        let n = rows;
        let w = 2 * n;
        let mut m = vec![0.0; n * w];
        for i in 0..n {
            m[i * w..i * w + n].copy_from_slice(&a[i * n..(i + 1) * n]);
            m[i * w + n + i] = 1.0;
        }
        let mut flag = x87::MathFlag::default();
        eliminate(&mut flag, n, w, &mut m);
        for col in n..w {
            m[(n - 1) * w + col] = flag.div(m[(n - 1) * w + col], m[(n - 1) * w + n - 1]);
            for i in (0..n - 1).rev() {
                let mut acc = m[i * w + col];
                for j in i + 1..n {
                    let mut t = flag.mul(m[i * w + j], m[j * w + col]);
                    if nz(t) {
                        t = -t;
                    }
                    acc = flag.add(t, acc);
                }
                m[i * w + col] = flag.div(acc, m[i * w + i]);
            }
        }
        flag.result(0.0).map_err(|_| XlError::NUM)?;
        let data = (0..n * n)
            .map(|k| Value::num(m[(k / n) * w + n + k % n]))
            .collect();
        Ok(array_value(n as u32, n as u32, data))
    })())
}

pub fn mmult(ctx: &mut EvalCtx, ops: &[Operand]) -> Operand {
    finish((|| {
        let ra = Src::from_operand(&ops[0]).and_then(|s| s.to_array(ctx));
        let rb = Src::from_operand(&ops[1]).and_then(|s| s.to_array(ctx));
        for x in [&ra, &rb] {
            match x {
                Err(e) => return Err(*e),
                Ok(x) => {
                    if let [Value::Error(e)] = x.data.as_slice() {
                        return Err(*e);
                    }
                }
            }
        }
        let (xa, xb) = (ra?, rb?);
        let (ar, ac, a) = matrix_nums(xa)?;
        let (br, bc, b) = matrix_nums(xb)?;
        if ac != br {
            return Err(XlError::VALUE);
        }
        check_result_size(ctx, ar as u64, bc as u64)?;

        let mut flag = x87::MathFlag::default();
        let mut data = Vec::with_capacity(ar * bc);
        for i in 0..ar {
            for j in 0..bc {
                let mut s = 0.0;
                for k in 0..ac {
                    let t = flag.mul(a[i * ac + k], b[k * bc + j]);
                    s = flag.add(t, s);
                }
                data.push(Value::num(s));
            }
        }
        flag.result(0.0).map_err(|_| XlError::NUM)?;
        Ok(array_value(ar as u32, bc as u32, data))
    })())
}

pub fn munit(ctx: &mut EvalCtx, ops: &[Operand]) -> Operand {
    finish((|| {
        let n = match &ops[0] {
            Operand::Val(v) => ctx.to_number(v)?.trunc(),
            _ => 0.0,
        };
        if n < 1.0 {
            return Err(XlError::VALUE);
        }

        let ni = n.min(1e18) as u64;
        if crate::workbook::ABANDON_CELLS / ni <= ni {
            ctx.observed.abandoned = true;
            return Err(XlError::NUM);
        }
        if n * n > crate::workbook::MAX_ARRAY_CELLS as f64 {
            return Err(XlError::NUM);
        }
        let n = n as u32;
        Ok(super::super::args::array_or_scalar(n, n, |r, c| {
            Value::Number(if r == c { 1.0 } else { 0.0 })
        }))
    })())
}
