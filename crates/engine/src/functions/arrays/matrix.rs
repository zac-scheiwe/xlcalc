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
            _ => return Err(XlError::Value),
        }
    }
    Ok((a.rows as usize, a.cols as usize, out))
}

pub fn div_x87(a: f64, b: f64) -> f64 {
    fn parts(x: f64) -> Option<(bool, u128, i32)> {
        let bits = x.to_bits();
        let e = ((bits >> 52) & 0x7ff) as i32;
        if e == 0 || e == 0x7ff {
            return None;
        }
        Some((
            bits >> 63 == 1,
            ((bits & ((1u64 << 52) - 1)) | (1u64 << 52)) as u128,
            e - 1075,
        ))
    }
    let (Some((sa, ma, ea)), Some((sb, mb, eb))) = (parts(a), parts(b)) else {
        return a / b;
    };
    let n = ma << 64;
    let mut q = n / mb;
    let r = n % mb;
    let mut e = ea - eb - 64;

    if q >> 64 != 0 {
        let dropped = q & 1;
        q >>= 1;
        e += 1;
        if dropped == 1 && (r != 0 || q & 1 == 1) {
            q += 1;
        }
    } else {
        let twice = r * 2;
        if twice > mb || (twice == mb && q & 1 == 1) {
            q += 1;
        }
    }
    if q >> 64 != 0 {
        q >>= 1;
        e += 1;
    }

    let rem = q & 0x7ff;
    let mut m = q >> 11;
    e += 11;
    if rem > 0x400 || (rem == 0x400 && m & 1 == 1) {
        m += 1;
        if m >> 53 != 0 {
            m >>= 1;
            e += 1;
        }
    }
    let biased = e + 52 + 1023;
    if !(1..=2046).contains(&biased) {
        return a / b;
    }
    let bits =
        (((sa != sb) as u64) << 63) | ((biased as u64) << 52) | ((m as u64) & ((1u64 << 52) - 1));
    f64::from_bits(bits)
}

fn lu(n: usize, m: &mut [f64]) -> Option<(Vec<usize>, usize)> {
    let mut perm: Vec<usize> = (0..n).collect();
    let mut swaps = 0;
    for p in 0..n {
        let mut best = p;
        for i in p + 1..n {
            if m[i * n + p].abs() > m[best * n + p].abs() {
                best = i;
            }
        }
        if m[best * n + p] == 0.0 {
            return None;
        }
        if best != p {
            for k in 0..n {
                m.swap(p * n + k, best * n + k);
            }
            perm.swap(p, best);
            swaps += 1;
        }
        let piv = m[p * n + p];
        for i in p + 1..n {
            let f = div_x87(m[i * n + p], piv);
            m[i * n + p] = f;
            for k in p + 1..n {
                m[i * n + k] = x87::sub(m[i * n + k], x87::mul(f, m[p * n + k]));
            }
        }
    }
    Some((perm, swaps))
}

pub fn mdeterm(ctx: &mut EvalCtx, ops: &[Operand]) -> Operand {
    finish((|| {
        let (rows, cols, mut m) = matrix_of(ctx, &ops[0])?;
        if rows != cols {
            return Err(XlError::Value);
        }
        let n = rows;
        let Some((_, swaps)) = lu(n, &mut m) else {
            return Ok(Value::Number(0.0));
        };
        let mut d = 1.0;
        for i in 0..n {
            d *= m[i * n + i];
        }
        if swaps % 2 == 1 {
            d = -d;
        }
        Ok(Value::num(d))
    })())
}

pub fn minverse(ctx: &mut EvalCtx, ops: &[Operand]) -> Operand {
    finish((|| {
        let (rows, cols, mut m) = matrix_of(ctx, &ops[0])?;
        if rows != cols {
            return Err(XlError::Value);
        }
        let n = rows;
        let (perm, _) = lu(n, &mut m).ok_or(XlError::Num)?;
        let mut out = vec![0.0; n * n];
        let mut y = vec![0.0; n];
        for col in 0..n {
            for i in 0..n {
                y[i] = if perm[i] == col { 1.0 } else { 0.0 };
            }
            for i in 0..n {
                for j in 0..i {
                    y[i] = x87::sub(y[i], x87::mul(m[i * n + j], y[j]));
                }
            }
            for i in (0..n).rev() {
                for j in i + 1..n {
                    y[i] = x87::sub(y[i], x87::mul(m[i * n + j], y[j]));
                }
                y[i] = div_x87(y[i], m[i * n + i]);
            }
            for i in 0..n {
                out[i * n + col] = y[i];
            }
        }
        if out.iter().any(|x| !x.is_finite()) {
            return Err(XlError::Num);
        }
        let data = out.into_iter().map(Value::num).collect();
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
            return Err(XlError::Value);
        }
        check_result_size(ctx, ar as u64, bc as u64)?;
        let mut data = Vec::with_capacity(ar * bc);
        for i in 0..ar {
            for j in 0..bc {
                let mut s = 0.0;
                for k in 0..ac {
                    s += a[i * ac + k] * b[k * bc + j];
                }
                data.push(Value::num(s));
            }
        }
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
            return Err(XlError::Value);
        }

        if n * n + n > crate::workbook::ABANDON_CELLS as f64 {
            ctx.abandoned = true;
            return Err(XlError::Num);
        }
        if n * n > crate::workbook::MAX_ARRAY_CELLS as f64 {
            return Err(XlError::Num);
        }
        let n = n as u32;
        Ok(super::super::args::array_or_scalar(n, n, |r, c| {
            Value::Number(if r == c { 1.0 } else { 0.0 })
        }))
    })())
}

#[cfg(test)]
mod tests {
    use super::div_x87;

    #[test]
    fn double_rounded_division() {
        let b = 1.0f64 / 3.0;
        let u = 2.0 - 4.0 * b;
        assert_eq!(div_x87(b, u), 0.49999999999999989);
        assert_eq!(b / u, 0.49999999999999994);
        assert_eq!(div_x87(1.0, 4.0), 0.25);
        assert_eq!(div_x87(-6.0, 3.0), -2.0);
    }
}
