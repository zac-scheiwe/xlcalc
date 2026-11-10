use super::{number, output, Arg, AtpError, Out, Run};
use crate::Workbook;

struct Crt(u32);

impl Crt {
    fn rand(&mut self) -> i32 {
        self.0 = self.0.wrapping_mul(214_013).wrapping_add(2_531_011);
        ((self.0 >> 16) & 0x7fff) as i32
    }

    fn unit(&mut self) -> f64 {
        f64::from(self.rand()) / 32767.0
    }
}

fn pow(x: f64, y: f64) -> f64 {
    x.powf(std::hint::black_box(y))
}

const TWO_PI_XLL: f64 = f64::from_bits(0x401921FB54442D11);

const PI_XLL: f64 = f64::from_bits(0x400921FB54442D11);

fn gammln(x: f64) -> f64 {
    const COF: [f64; 6] = [
        76.18009173,
        -86.50532033,
        24.01409822,
        -1.231739516,
        0.00120858003,
        -0.00000536382,
    ];
    let mut x = x - 1.0;
    let tmp0 = x + 5.5;
    let tmp = tmp0.ln() * (x + 0.5) - tmp0;
    let mut ser = 1.0;
    for c in COF {
        x += 1.0;
        ser += c / x;
    }
    (ser * TWO_PI_XLL.sqrt()).ln() + tmp
}

fn normal_quantile(p: f64) -> Option<f64> {
    let norm = 1.0 / TWO_PI_XLL.sqrt();
    let p = if 3e-7 > 1.0 - p {
        f64::from_bits(0x3FEFFFFF5D53FF42)
    } else {
        p
    };
    let (mut hi, mut lo): (f64, f64) = (19.073486328125, -19.073486328125);
    for _ in 1..100 {
        let x = (hi - lo) * 0.5 + lo;
        let ax = x.abs();
        let t = 1.0 / (ax * 0.2316419 + 1.0);
        let mut s = t * 0.31938153;
        s -= pow(t, 2.0) * 0.356563782;
        s += pow(t, 3.0) * 1.781477937;
        s -= pow(t, 4.0) * 1.821255978;
        s += pow(t, 5.0) * 1.330274429;
        let pdf = (-(ax * ax * 0.5)).exp() * norm;
        let mut cdf = 1.0 - s * pdf;
        if 0.0 > x {
            cdf = 1.0 - cdf;
        }
        if 3e-7 > (cdf - p).abs() {
            return Some(x);
        }
        if p > cdf {
            lo = x;
        } else {
            hi = x;
        }
    }
    None
}

fn poisson(rng: &mut Crt, xm: f64) -> f64 {
    rng.rand();
    if xm < 12.0 {
        let g = if xm == -1.0 { 0.0 } else { (-xm).exp() };
        let mut em = -1.0;
        let mut t = 1.0;
        loop {
            em += 1.0;
            t *= rng.unit();
            if t <= g {
                return em;
            }
        }
    }
    let sq = (xm + xm).sqrt();
    let alxm = xm.ln();
    let g = alxm * xm - gammln(xm + 1.0);
    loop {
        let (mut y, mut em);
        loop {
            y = (rng.unit() * PI_XLL).tan();
            em = y * sq + xm;
            if em >= 0.0 {
                break;
            }
        }
        em = em.floor();
        let lg = gammln(em + 1.0);
        let u = rng.unit();
        let t = (alxm * em - lg - g).exp() * ((y * y + 1.0) * 0.9);
        if u <= t {
            return em;
        }
    }
}

fn discrete_table(wb: &Workbook, args: &[Arg]) -> Result<Vec<(f64, f64)>, AtpError> {
    let bad = || AtpError::Message("The table is not valid.".into());
    let Some(Arg::Range(sheet, rect)) = args.get(5) else {
        return Err(bad());
    };
    let src = super::Src {
        sheet: *sheet,
        rect: *rect,
    };
    if src.cols() != 2 {
        return Err(bad());
    }
    let mut t = Vec::new();
    let mut sum = 0.0;
    for r in 0..src.rows() {
        let (crate::value::Value::Number(v), crate::value::Value::Number(p)) =
            (wb.value(src.at(r, 0)), wb.value(src.at(r, 1)))
        else {
            return Err(bad());
        };
        sum += p;
        t.push((v, p));
    }
    if (sum - 1.0).abs() > 1e-10 {
        return Err(bad());
    }
    Ok(t)
}

fn unsigned(args: &[Arg], i: usize) -> Option<f64> {
    number(args, i)
}

pub(super) fn random(wb: &Workbook, args: &[Arg]) -> Result<Run, AtpError> {
    let Some(Arg::Range(sheet, rect)) = args.first() else {
        return Err(AtpError::Message("Output range is not valid.".into()));
    };
    let dist = unsigned(args, 3).unwrap_or(0.0).trunc() as i32;
    let given = |i: usize, dflt: u32| {
        let n = unsigned(args, i).unwrap_or(0.0).trunc() as i64;
        if n == 0 {
            dflt
        } else {
            n as u32
        }
    };
    let mut cols = given(1, rect.c1 - rect.c0 + 1);
    let mut rows = given(2, rect.r1 - rect.r0 + 1);
    let a = |i: usize| unsigned(args, 5 + i).unwrap_or(0.0);
    if dist == 6 {
        let n = ((a(1) - a(0)) / a(2)).ceil() + 1.0;
        rows = (n * (a(4) * a(3))) as i32 as u32;
    }
    let mut seed = unsigned(args, 4).unwrap_or(0.0);
    if seed == 0.0 {
        seed = 1.0;
    } else if !(1.0..=32767.0).contains(&seed) {
        return Err(AtpError::Message("Seed must be 1 to 32767.".into()));
    }
    let bad = match dist {
        2 => a(1) == 0.0,
        3 => !(0.0..=1.0).contains(&a(0)),
        4 => !(0.0..=1.0).contains(&a(0)) || a(1) <= 0.0,
        5 => a(0) <= 0.0 || a(0).is_nan(),
        6 => {
            a(2) == 0.0
                || (a(1) > a(0) && a(2) < 0.0)
                || (a(0) > a(1) && a(2) > 0.0)
                || a(3) < 1.0
                || a(4) < 1.0
        }
        _ => false,
    };
    if bad {
        return Err(AtpError::Message("Invalid distribution argument.".into()));
    }
    let table = if dist == 7 {
        Some(discrete_table(wb, args)?)
    } else {
        None
    };
    let mut rng = Crt(seed.floor() as i64 as u32);
    let mut o = Out::default();
    if dist == 6 {
        let (lo, hi, step) = (a(0), a(1), a(2));
        let each = a(3).trunc() as i32;
        let mut v = lo;
        let mut r = 1;
        let mut col_vals = Vec::new();
        while r <= rows as i32 {
            let mut k = 1;
            while r <= rows as i32 && k <= each {
                col_vals.push(v);
                r += 1;
                k += 1;
            }
            if v == hi {
                v = lo;
            } else {
                v += step;
                if hi <= v || v.is_nan() {
                    v = hi;
                }
            }
        }
        for c in 0..cols {
            for (r, x) in col_vals.iter().enumerate() {
                o.num(r as u32, c, *x);
            }
        }
        return output(args, 0, o);
    }
    if cols == 0 {
        cols = 1;
    }
    for r in 0..rows {
        for c in 0..cols {
            let x = match dist {
                1 => {
                    let (lo, hi) = (a(0).min(a(1)), a(0).max(a(1)));
                    (hi - lo) * rng.unit() + lo
                }
                2 => {
                    let Some(z) = normal_quantile(rng.unit()) else {
                        return Err(AtpError::Message("No convergence.".into()));
                    };
                    z * a(1) + a(0)
                }
                3 => f64::from(u8::from(a(0) >= rng.unit())),
                4 => {
                    let mut n = 0.0;
                    for _ in 0..(a(1) as i32).max(0) {
                        if a(0) >= rng.unit() {
                            n += 1.0;
                        }
                    }
                    n
                }
                5 => poisson(&mut rng, a(0)),
                7 => {
                    let u = rng.unit();
                    let mut cum = 0.0;
                    let mut pick = 0.0;
                    for (v, p) in table.iter().flatten() {
                        cum += p;
                        if cum > u {
                            pick = *v;
                            break;
                        }
                    }
                    pick
                }
                _ => return Err(AtpError::Message("Unknown distribution.".into())),
            };
            o.num(r, c, x);
        }
    }
    let _ = sheet;
    output(args, 0, o)
}
