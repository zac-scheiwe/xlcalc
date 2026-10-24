use super::atp::{self, R};
use super::xlmath;
use crate::eval::{EvalCtx, Operand};
use crate::functions::args::val;
use crate::value::{Value, XlError};
use crate::x87::{add, div, exp, ln, mul, sqrt, sub};

fn trig(x: f64) -> R<(f64, f64)> {
    if x.abs() >= xlmath::TRIG_LIMIT {
        return Err(XlError::Num);
    }
    Ok((xlmath::sin(x), xlmath::cos(x)))
}

fn horner(y: f64, c: &[f64]) -> f64 {
    c[1..].iter().fold(c[0], |p, &k| add(mul(p, y), k))
}

fn amp(x: f64) -> f64 {
    sqrt(div(0.636619772, x))
}

fn bessj0(x: f64) -> R<f64> {
    let ax = x.abs();
    if ax < 8.0 {
        let y = mul(x, x);
        let ans1 = horner(
            y,
            &[
                -184.9052456,
                77392.33017,
                -11214424.18,
                651619640.7,
                -13362590354.0,
                57568490574.0,
            ],
        );
        let ans2 = horner(
            y,
            &[
                1.0,
                267.8532712,
                59272.64853,
                9494680.718,
                1029532985.0,
                57568490411.0,
            ],
        );
        Ok(div(ans1, ans2))
    } else {
        let z = div(8.0, ax);
        let y = mul(z, z);
        let xx = sub(ax, 0.785398164);

        let ans1 = horner(
            y,
            &[
                0.2093887211e-6,
                -0.2073370639e-5,
                0.2734510407e-4,
                -0.1098628267e-2,
                1.0,
            ],
        );
        let ans2 = horner(
            y,
            &[
                -0.934935152e-7,
                0.7621095161e-6,
                -0.6911147651e-5,
                0.1430488765e-3,
                -0.1562499995e-1,
            ],
        );
        let (s, c) = trig(xx)?;
        Ok(mul(amp(ax), sub(mul(c, ans1), mul(mul(s, z), ans2))))
    }
}

fn bessj1(x: f64) -> R<f64> {
    let ax = x.abs();
    if ax < 8.0 {
        let y = mul(x, x);
        let ans1 = mul(
            x,
            horner(
                y,
                &[
                    -30.16036606,
                    15704.48260,
                    -2972611.439,
                    242396853.1,
                    -7895059235.0,
                    72362614232.0,
                ],
            ),
        );
        let ans2 = horner(
            y,
            &[
                1.0,
                376.9991397,
                99447.43394,
                18583304.74,
                2300535178.0,
                144725228442.0,
            ],
        );
        Ok(div(ans1, ans2))
    } else {
        let z = div(8.0, ax);
        let y = mul(z, z);
        let xx = sub(ax, 2.356194491);

        let ans1 = horner(
            y,
            &[
                -0.240337019e-6,
                0.2457520174e-5,
                -0.3516396496e-4,
                -0.3516396496e-4,
                0.183105e-2,
                1.0,
            ],
        );
        let ans2 = horner(
            y,
            &[
                0.105787412e-6,
                -0.88228987e-6,
                0.8449199096e-5,
                -0.2002690873e-3,
                0.04687499995,
            ],
        );
        let (s, c) = trig(xx)?;
        let ans = mul(amp(ax), sub(mul(c, ans1), mul(mul(s, ans2), z)));
        Ok(if x < 0.0 { -ans } else { ans })
    }
}

fn ftz(x: f64) -> f64 {
    crate::number::flush_subnormal(x)
}

const ACC_J: f64 = 40.0;
const ACC_I: f64 = 40.0;
const BIGNO: f64 = 1.0e10;
const BIGNI: f64 = 1.0e-10;

fn bessj(n: i32, x: f64) -> R<f64> {
    match n {
        0 => return bessj0(x),
        1 => return bessj1(x),
        _ => {}
    }
    let ax = x.abs();
    if ax == 0.0 {
        return Ok(0.0);
    }
    let ans = if ax > n as f64 {
        let tox = div(2.0, ax);
        let mut bjm = bessj0(ax)?;
        let mut bj = bessj1(ax)?;
        for j in 1..n {
            let bjp = sub(ftz(mul(mul(j as f64, tox), bj)), bjm);
            bjm = bj;
            bj = bjp;
        }
        bj
    } else {
        let tox = div(2.0, ax);
        let m = 2 * ((n + (ACC_J * n as f64).sqrt() as i32) / 2);
        let mut jsum = false;
        let (mut bjp, mut ans, mut sum) = (0.0f64, 0.0f64, 0.0f64);
        let mut bj = 1.0f64;
        let mut j = m;
        while j > 0 {
            let bjm = sub(ftz(mul(mul(j as f64, tox), bj)), bjp);
            bjp = bj;
            bj = bjm;
            if bj.abs() > BIGNO {
                bj *= BIGNI;
                bjp *= BIGNI;
                ans *= BIGNI;
                sum *= BIGNI;
            }
            if jsum {
                sum = add(sum, bj);
            }
            jsum = !jsum;
            if j == n {
                ans = bjp;
            }
            j -= 1;
        }
        sum = sub(mul(2.0, sum), bj);
        div(ans, sum)
    };
    Ok(if x < 0.0 && n & 1 == 1 { -ans } else { ans })
}

fn bessy0(x: f64) -> R<f64> {
    if x < 8.0 {
        let y = mul(x, x);
        let ans1 = horner(
            y,
            &[
                228.4622733,
                -86327.92757,
                10879881.29,
                -512359803.6,
                7062834065.0,
                -2957821389.0,
            ],
        );
        let ans2 = horner(
            y,
            &[
                1.0,
                226.1030244,
                47447.26470,
                7189466.438,
                745249964.8,
                40076544269.0,
            ],
        );
        Ok(add(
            div(ans1, ans2),
            mul(0.636619772, mul(bessj0(x)?, ln(x))),
        ))
    } else {
        let z = div(8.0, x);
        let y = mul(z, z);
        let xx = sub(x, 0.785398164);
        let ans1 = horner(
            y,
            &[
                0.2093887211e-6,
                -0.2073370639e-5,
                0.2734510407e-4,
                -0.1098628627e-2,
                1.0,
            ],
        );
        let ans2 = horner(
            y,
            &[
                -0.934945152e-7,
                0.7621095161e-6,
                -0.6911147651e-5,
                0.1430488765e-3,
                -0.1562499995e-1,
            ],
        );
        let (s, c) = trig(xx)?;
        Ok(mul(amp(x), add(mul(s, ans1), mul(mul(c, ans2), z))))
    }
}

fn bessy1(x: f64) -> R<f64> {
    if x < 8.0 {
        let y = mul(x, x);
        let ans1 = mul(
            x,
            horner(
                y,
                &[
                    0.8511937935e4,
                    -0.4237922726e7,
                    0.7349264551e9,
                    -0.5153438139e11,
                    0.1275274390e13,
                    -0.4900604943e13,
                ],
            ),
        );
        let ans2 = horner(
            y,
            &[
                1.0,
                0.3549632885e3,
                0.1020426050e6,
                0.2245904002e8,
                0.3733650367e10,
                0.4244419664e12,
                0.2499580570e14,
            ],
        );
        let inv = div(1.0, x);
        Ok(add(
            div(ans1, ans2),
            mul(0.636619772, sub(mul(bessj1(x)?, ln(x)), inv)),
        ))
    } else {
        let z = div(8.0, x);
        let y = mul(z, z);
        let xx = sub(x, 2.356194491);
        let ans1 = horner(
            y,
            &[
                -0.240337019e-6,
                0.2457520174e-5,
                -0.3516396496e-4,
                0.183105e-2,
                1.0,
            ],
        );
        let ans2 = horner(
            y,
            &[
                0.105787412e-6,
                -0.88228987e-6,
                0.8449199096e-5,
                -0.2002690873e-3,
                0.04687499995,
            ],
        );
        let (s, c) = trig(xx)?;
        Ok(mul(amp(x), add(mul(s, ans1), mul(mul(c, ans2), z))))
    }
}

fn bessy(n: i32, x: f64) -> R<f64> {
    match n {
        0 => return bessy0(x),
        1 => return bessy1(x),
        _ => {}
    }
    let tox = div(2.0, x);
    let mut by = bessy1(x)?;
    let mut bym = bessy0(x)?;
    for j in 1..n {
        let byp = sub(ftz(mul(mul(j as f64, tox), by)), bym);
        bym = by;
        by = byp;
    }
    Ok(by)
}

fn bessi0(x: f64) -> f64 {
    let ax = x.abs();
    if ax < 3.75 {
        let t = div(x, 3.75);
        let y = mul(t, t);
        horner(
            y,
            &[
                0.45813e-2,
                0.360768e-1,
                0.2659732,
                1.2067492,
                3.0899424,
                3.5156229,
                1.0,
            ],
        )
    } else {
        let y = div(3.75, ax);
        let p = horner(
            y,
            &[
                0.392377e-2,
                -0.1647633e-1,
                0.2635537e-1,
                -0.2057706e-1,
                0.916281e-2,
                -0.157565e-2,
                0.225319e-2,
                0.1328592e-1,
                0.39894228,
            ],
        );
        mul(div(exp(ax), sqrt(ax)), p)
    }
}

fn bessi1(x: f64) -> f64 {
    let ax = x.abs();
    let ans = if ax < 3.75 {
        let t = div(x, 3.75);
        let y = mul(t, t);
        mul(
            ax,
            horner(
                y,
                &[
                    0.32411e-3,
                    0.301532e-2,
                    0.2658733e-1,
                    0.15084934,
                    0.51498869,
                    0.87890594,
                    0.5,
                ],
            ),
        )
    } else {
        let y = div(3.75, ax);
        let p = horner(
            y,
            &[
                -0.420059e-2,
                0.1787654e-1,
                -0.2895312e-1,
                0.2282967e-1,
                -0.1031555e-1,
                0.163801e-2,
                -0.362018e-2,
                -0.3988024e-1,
                0.39894228,
            ],
        );
        mul(p, div(exp(ax), sqrt(ax)))
    };
    if x < 0.0 {
        -ans
    } else {
        ans
    }
}

fn bessi(n: i32, x: f64) -> f64 {
    match n {
        0 => return bessi0(x),
        1 => return bessi1(x),
        _ => {}
    }
    if x == 0.0 {
        return 0.0;
    }
    let tox = div(2.0, x.abs());
    let (mut bip, mut ans) = (0.0f64, 0.0f64);
    let mut bi = 1.0f64;
    let mut j = 2 * (n + (ACC_I * n as f64).sqrt() as i32);
    while j > 0 {
        let bim = add(bip, ftz(mul(mul(j as f64, tox), bi)));
        bip = bi;
        bi = bim;
        if bi.abs() > BIGNO {
            ans *= BIGNI;
            bi *= BIGNI;
            bip *= BIGNI;
        }
        if j == n {
            ans = bip;
        }
        j -= 1;
    }
    ans = mul(ans, div(bessi0(x), bi));
    if x < 0.0 && n & 1 == 1 {
        -ans
    } else {
        ans
    }
}

fn bessk0(x: f64) -> f64 {
    if x <= 2.0 {
        let y = div(mul(x, x), 4.0);
        let p = horner(
            y,
            &[
                0.74e-5,
                0.10750e-3,
                0.262698e-2,
                0.3488590e-1,
                0.23069756,
                0.42278420,
                -0.57721566,
            ],
        );
        add(mul(-ln(div(x, 2.0)), bessi0(x)), p)
    } else {
        let y = div(2.0, x);
        let p = horner(
            y,
            &[
                0.53208e-3,
                -0.251540e-2,
                0.587872e-2,
                -0.1062446e-1,
                0.2189568e-1,
                -0.7832358e-1,
                1.25331414,
            ],
        );
        mul(div(exp(-x), sqrt(x)), p)
    }
}

fn bessk1(x: f64) -> f64 {
    if x <= 2.0 {
        let y = div(mul(x, x), 4.0);
        let p = horner(
            y,
            &[
                -0.4686e-4,
                -0.110404e-2,
                -0.1919402e-1,
                -0.18156897,
                -0.67278579,
                0.15443144,
                1.0,
            ],
        );
        let lead = mul(div(1.0, x), p);
        add(lead, mul(ln(div(x, 2.0)), bessi1(x)))
    } else {
        let y = div(2.0, x);
        let p = horner(
            y,
            &[
                -0.68245e-3,
                0.325614e-2,
                -0.780353e-2,
                0.1504268e-1,
                -0.3655620e-1,
                0.23498619,
                1.25331414,
            ],
        );
        mul(div(exp(-x), sqrt(x)), p)
    }
}

fn bessk(n: i32, x: f64) -> f64 {
    match n {
        0 => return bessk0(x),
        1 => return bessk1(x),
        _ => {}
    }
    let tox = div(2.0, x);
    let mut bkm = bessk0(x);
    let mut bk = bessk1(x);
    for j in 1..n {
        let bkp = add(bkm, ftz(mul(mul(j as f64, tox), bk)));
        bkm = bk;
        bk = bkp;
    }
    bk
}

#[derive(Clone, Copy)]
pub enum Kind {
    J,
    Y,
    I,
    K,
}

pub fn bessel(ctx: &mut EvalCtx, ops: &[Operand], kind: Kind) -> Operand {
    atp::call(ctx, ops, "b", |ctx, a, _| {
        val((|| {
            let x = atp::req_num(ctx, a, 0)?;
            let n = atp::req_num(ctx, a, 1)?.trunc();
            if n < 0.0 {
                return Err(XlError::Num);
            }
            let n = n.min(i32::MAX as f64 / 4.0) as i32;
            let r = match kind {
                Kind::J => {
                    if x.abs() >= xlmath::TRIG_LIMIT {
                        return Err(XlError::Num);
                    }
                    bessj(n, x)?
                }
                Kind::Y => {
                    if x <= 0.0 {
                        return Err(XlError::Num);
                    }
                    bessy(n, x)?
                }
                Kind::I => bessi(n, x),
                Kind::K => {
                    if x <= 0.0 {
                        return Err(XlError::Num);
                    }
                    bessk(n, x)
                }
            };
            Ok(Value::num(r))
        })())
    })
}
