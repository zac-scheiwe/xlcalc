use crate::number;
use crate::value::XlError;

type R<T> = Result<T, XlError>;

pub fn snapped_sum(v: &[f64]) -> f64 {
    let mut acc = 0.0f64;
    for &x in v {
        let s = acc + x;
        acc = number::snap_add(acc, x, s);
    }
    acc
}

pub fn max(v: &[f64]) -> f64 {
    v.iter()
        .copied()
        .reduce(|m, x| if x > m { x } else { m })
        .unwrap_or(0.0)
}

pub fn min(v: &[f64]) -> f64 {
    v.iter()
        .copied()
        .reduce(|m, x| if x < m { x } else { m })
        .unwrap_or(0.0)
}

pub fn var(v: &[f64], sample: bool) -> R<f64> {
    use crate::x87;
    let n = v.len();
    if n == 0 || (sample && n < 2) {
        return Err(XlError::Div0);
    }
    let hz = |x: f64| x.to_bits() & 0xFFFF_0000_0000_0000 == 0;
    let pos = |x: f64| (x.to_bits() & 0xFFFF_0000_0000_0000) as i64 > 0;
    let nf = n as f64;
    let (mut s, mut q) = (0.0f64, 0.0f64);
    for &x in v {
        s = x87::add(s, x);
        q = x87::add(x87::mul(x, x), q);
    }
    let t = x87::mul(s, s);
    let (d, c, div) = if sample {
        let c = x87::div(t, nf);
        (x87::sub(q, c), c, nf - 1.0)
    } else {
        (
            x87::sub(x87::mul(q, nf), t),
            x87::div(t, nf),
            x87::mul(nf, nf),
        )
    };
    let mean = x87::div(s, nf);
    if !d.is_finite() || !mean.is_finite() {
        return Err(XlError::Num);
    }
    if hz(mean) {
        return Ok(if hz(d) || pos(d) {
            x87::div(d, div)
        } else {
            0.0
        });
    }
    if hz(d) || pos(d) {
        let r = x87::div(d, div);
        let tight = x87::sub(x87::div(c, q), 1.0).abs() <= 0.01;
        if r.to_bits() >> 32 != 0 && !tight {
            let m = x87::sub(x87::div(mean, r).abs(), 1000.0);
            if !m.is_finite() {
                return Err(XlError::Num);
            }
            if !hz(m) && !pos(m) {
                return Ok(r);
            }
        }
    }
    let mut ss = 0.0f64;
    for &x in v {
        let e = x87::sub(x, mean);
        ss = x87::add(x87::mul(e, e), ss);
    }
    Ok(x87::div(ss, if sample { nf - 1.0 } else { nf }))
}

pub fn stdev(v: &[f64], sample: bool) -> R<f64> {
    var(v, sample).map(crate::x87::sqrt)
}

fn sorted(v: &[f64]) -> Vec<f64> {
    let mut s = v.to_vec();
    s.sort_by(|a, b| a.partial_cmp(b).unwrap());
    s
}

pub fn median(v: &[f64]) -> R<f64> {
    if v.is_empty() {
        return Err(XlError::Num);
    }
    let s = sorted(v);
    let n = s.len();
    Ok(if n % 2 == 1 {
        s[n / 2]
    } else {
        (s[n / 2 - 1] + s[n / 2]) / 2.0
    })
}

pub fn mode(v: &[f64]) -> R<f64> {
    let mut best: Option<(usize, f64)> = None;
    for (i, &x) in v.iter().enumerate() {
        if v[..i].contains(&x) {
            continue;
        }
        let n = v[i..].iter().filter(|&&y| y == x).count();
        if n >= 2 && best.is_none_or(|(m, _)| n > m) {
            best = Some((n, x));
        }
    }
    best.map(|(_, x)| x).ok_or(XlError::NA)
}

pub fn kth(v: &[f64], k: f64, largest: bool) -> R<f64> {
    if k < 1.0 || k > v.len() as f64 {
        return Err(XlError::Num);
    }
    let k = if largest { k.ceil() } else { k.floor() };
    let s = sorted(v);
    let i = k as usize - 1;
    Ok(if largest { s[s.len() - 1 - i] } else { s[i] })
}

fn interpolate(s: &[f64], rank0: f64) -> f64 {
    let f = rank0.floor();
    let i = f as usize;
    if i + 1 >= s.len() {
        return s[s.len() - 1];
    }
    let frac = rank0 - f;
    s[i] + frac * (s[i + 1] - s[i])
}

pub fn percentile_inc(v: &[f64], k: f64) -> R<f64> {
    if v.is_empty() || !(0.0..=1.0).contains(&k) {
        return Err(XlError::Num);
    }
    let s = sorted(v);

    Ok(interpolate(&s, (k * (s.len() - 1) as f64 + 1.0) - 1.0))
}

pub fn percentile_exc(v: &[f64], k: f64) -> R<f64> {
    let n = v.len() as f64;
    let rank = k * (n + 1.0);
    if v.is_empty() || k <= 0.0 || k >= 1.0 || rank < 1.0 || rank > n {
        return Err(XlError::Num);
    }
    let s = sorted(v);
    Ok(interpolate(&s, rank - 1.0))
}

pub fn quartile_inc(v: &[f64], q: f64) -> R<f64> {
    let q = q.trunc();
    if !(0.0..=4.0).contains(&q) {
        return Err(XlError::Num);
    }
    percentile_inc(v, q / 4.0)
}

pub fn quartile_exc(v: &[f64], q: f64) -> R<f64> {
    let q = q.trunc();
    if q <= 0.0 || q >= 4.0 {
        return Err(XlError::Num);
    }
    percentile_exc(v, q / 4.0)
}
