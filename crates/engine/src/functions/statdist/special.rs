use crate::x87;

pub const SQRT1_2: f64 = std::f64::consts::FRAC_1_SQRT_2;

pub const INV_SQRT_2PI: f64 = 0.3989422804014327;

const CODY_SQRTPI: f64 = 0.918_938_533_204_672_8;

const XL_D1: [f64; 9] = [
    1.9218729524556253,
    77.30614002374985,
    788.5812527964014,
    2931.34732789441,
    4494.513911447618,
    2904.352787117292,
    741.8371345206704,
    62.176154907703804,
    1.0,
];
const XL_N1: [f64; 10] = [
    -0.018779192100721907,
    -7.057239553279993,
    -152.11103889489053,
    -730.1038817811354,
    -345.9053270081915,
    2656.030289072731,
    3743.5379711174996,
    1557.3900560282782,
    201.17283904765537,
    5.4058524839061475,
];
const XL_D2: [f64; 8] = [
    -34.47940915109643,
    -821.3657152855959,
    -5224.492351463495,
    -11742.525322498115,
    -9749.36192183246,
    -2655.243289468887,
    -158.14556641099998,
    1.0,
];
const XL_N2: [f64; 8] = [
    0.005877167699924444,
    -0.707628417390826,
    -107.81457944634195,
    -1458.225984274407,
    -5535.020321599891,
    -7117.762803996429,
    -3006.040094071494,
    -312.90939347321034,
];
const XL_D3: [f64; 9] = [
    0.0004955148084238959,
    0.08276042744897276,
    2.6485411594408865,
    27.704029458859964,
    113.27059743840893,
    189.61325507101878,
    123.46789647375323,
    25.69657438870062,
    1.0,
];
const XL_N3: [f64; 9] = [
    0.0026730135451229087,
    0.26453758918914383,
    5.362058736627363,
    33.24858631079838,
    62.08239808377191,
    -2.063975725136575,
    -66.72197991305013,
    -29.907511646103828,
    -2.266786438166138,
];

const XL_STIRLING: [f64; 10] = [
    -1.3924322169059,
    0.17964437236883057,
    -0.029550653594771242,
    0.00641025641025641,
    -0.0019175269175269176,
    0.0008417508417508417,
    -0.0005952380952380953,
    0.0007936507936507937,
    -0.002777777777777778,
    0.08333333333333333,
];

pub(super) fn horner_x87(c: &[f64], x: f64) -> f64 {
    use crate::x87;
    let mut acc = c[0];
    for &k in &c[1..] {
        acc = x87::add(x87::mul(acc, x), k);
    }
    acc
}

pub fn lgamma_xl(x: f64) -> f64 {
    use crate::x87;
    if x <= 0.0 || x.is_nan() {
        return f64::NAN;
    }
    if x.is_infinite() {
        return f64::INFINITY;
    }
    if x <= 2.22e-16 {
        return -x87::ln(x);
    }
    if x >= 8.0 {
        let base = x87::add(
            x87::sub(x87::mul(x87::ln(x), x87::sub(x, 0.5)), x),
            CODY_SQRTPI,
        );
        if x > 2.25e76 {
            return base;
        }
        let q = x87::div(1.0, x);
        let s = x87::mul(horner_x87(&XL_STIRLING, x87::mul(q, q)), q);
        return x87::add(base, s);
    }
    if x > 2.0 {
        let (y, lnp) = if x > 3.0 {
            let n = (x - 2.0).floor();
            let y = x87::sub(x, n);
            let (mut p, mut t) = (1.0, y);
            while t < x {
                p = x87::mul(t, p);
                t = x87::add(t, 1.0);
            }
            (y, Some(x87::ln(p)))
        } else {
            (x, None)
        };
        let r = x87::div(horner_x87(&XL_N3, y), horner_x87(&XL_D3, y));
        let v = x87::mul(r, x87::sub(y, 2.0));
        return match lnp {
            Some(l) => x87::add(v, l),
            None => v,
        };
    }
    if x >= 1.5 {
        let r = x87::div(horner_x87(&XL_N2, x), horner_x87(&XL_D2, x));
        let v = x87::mul(x87::sub(x, 2.0), x87::mul(r, x));
        return x87::mul(x87::sub(x, 1.0), v);
    }
    let (y, corr) = if x < 0.7 {
        (x87::add(x, 1.0), -x87::ln(x))
    } else {
        (x, 0.0)
    };

    if y >= 1.5 {
        let r = x87::div(horner_x87(&XL_N2, y), horner_x87(&XL_D2, y));
        let v = x87::mul(x87::sub(y, 2.0), x87::mul(r, y));
        return x87::add(x87::mul(x87::sub(y, 1.0), v), corr);
    }
    let r = x87::div(horner_x87(&XL_N1, y), horner_x87(&XL_D1, y));
    x87::add(x87::mul(r, x87::sub(1.0, y)), corr)
}

const XL_GAMMA_NUM: [f64; 8] = [
    -41.32975575688121,
    -224.55734984993717,
    -1766.0581782060663,
    -11262.704073349521,
    -39779.10693069725,
    -183439.1689740763,
    -345373.8010894497,
    -896026.3354389172,
];
const XL_GAMMA_DEN: [f64; 8] = [
    -319.17974684033175,
    6642.776155506226,
    -49294.268969809906,
    118905.79578918342,
    204704.893587802,
    -862516.6292015722,
    -896037.449404572,
    1.0,
];

const XL_GAMMA_STIRLING: [f64; 16] = [
    -0.05905576189139824,
    0.001080329535785209,
    0.01280672566761614,
    -0.00032503252556783166,
    -0.003828876997130955,
    0.00014409790832040022,
    0.0016789974413441745,
    -0.00010343581816521184,
    -0.0011843328747073879,
    0.00013945627516731714,
    0.0015680784434401332,
    -0.00045894418724279834,
    -0.0053626543209876545,
    0.006944444444444444,
    0.16666666666666666,
    2.0,
];

fn powi_x87(x: f64, mut e: u64) -> f64 {
    use crate::x87;
    let (mut base, mut res) = (x, 1.0);
    while e > 0 {
        if e & 1 == 1 {
            res = x87::mul(res, base);
        }
        e >>= 1;
        if e > 0 {
            base = x87::mul(base, base);
        }
    }
    res
}

fn gamma_stirling_xl(x: f64) -> f64 {
    use crate::x87;
    let w = x87::div(1.0, x);
    let s = horner_x87(&XL_GAMMA_STIRLING, w);
    let t = x87::mul(x87::sqrt(w), x87::exp(-x));

    let h = x87::mul(x, 0.5);
    let n = h.trunc();
    let f = x87::sub(h, n);
    let p = x87::mul(powi_x87(x, n as u64), x87::exp(x87::mul(f, x87::ln(x))));
    let t = x87::mul(x87::mul(t, p), p);
    x87::mul(x87::mul(t, 1.2533141373155003), s)
}

pub fn gamma_xl(x: f64) -> f64 {
    use crate::x87;
    use std::f64::consts::PI;
    if x.is_nan() || x > 171.6243769563027 {
        return f64::NAN;
    }
    let a = x.abs();
    let n = a.trunc();
    let f = a - n;
    if f == 0.0 && x <= 0.0 {
        return f64::NAN;
    }
    if x < -171.03093456965283 {
        return 0.0;
    }
    if f == 0.0 {
        if x <= 88.0 {
            let mut z = 1.0;
            let mut k = x - 1.0;
            while k >= 2.0 {
                z = x87::mul(z, k);
                k -= 1.0;
            }
            return z;
        }
        return gamma_stirling_xl(x);
    }

    let odd = n as u64 % 2 == 1;
    if f == 0.5 {
        let mut prod = 1.0;
        let mut k = 1.0;
        for _ in 0..n as u64 {
            prod = x87::mul(prod, k / 2.0);
            k += 2.0;
        }
        let g = x87::mul(prod, 1.772453850905516);
        if x > 0.0 {
            return g;
        }
        let r = x87::div(x87::div(-PI, g), a);
        return if odd { -r } else { r };
    }
    if a >= 10.0 {
        let g = gamma_stirling_xl(a);
        if x > 0.0 {
            return g;
        }
        let r = x87::div(-PI, g);
        let f = if f > 0.5 { x87::sub(1.0, f) } else { f };
        let d = x87::mul(a, x87::sin(x87::mul(PI, f)));
        let r = x87::div(r, d);
        return if odd { -r } else { r };
    }
    let mut x = x;
    let mut z = 1.0;
    while x >= 2.0 {
        x = x87::sub(x, 1.0);
        z = x87::mul(z, x);
    }
    while x < 1.0 {
        z = x87::div(z, x);
        x = x87::add(x, 1.0);
    }
    let r = x87::div(horner_x87(&XL_GAMMA_NUM, x), horner_x87(&XL_GAMMA_DEN, x));
    x87::mul(z, r)
}

const TWO_OVER_SQRTPI: f64 = std::f64::consts::FRAC_2_SQRT_PI;

const ERF_SWITCH: f64 = 0.5;

const ERF_Q: [f64; 6] = [
    1.9975156139294637e-05,
    0.0006725141344930594,
    0.010463519793879432,
    0.09227786828023543,
    0.4546572664299418,
    0.9899450977749467,
];
const ERF_P: [f64; 5] = [
    0.00019223877368396887,
    0.0018052212007856519,
    0.04481917077177939,
    0.14068131264228023,
    1.1170334248975802,
];

const ERFC_D1: [f64; 11] = [
    5.058391992671494e-05,
    0.0010879439261419052,
    0.011278547023686922,
    0.07339642799281594,
    0.3295256097389463,
    1.0605339943111198,
    2.4670163186997796,
    4.081359865933331,
    4.582312665466074,
    3.1450982657907907,
    1.0,
];
const ERFC_N1: [f64; 10] = [
    2.853892058798777e-05,
    0.0006138066387309207,
    0.006348969040437286,
    0.04110260172693343,
    0.18275463061854574,
    0.5780986302521861,
    1.3036192048424995,
    2.0334493038775228,
    2.016719098695491,
    0.9999999999999892,
];

const ERFC_D2: [f64; 10] = [
    0.004513605399641127,
    0.03944627183270594,
    0.22517246755925643,
    0.8388160818923082,
    2.279306526289092,
    4.471532191673217,
    6.310184387183285,
    6.07899452238681,
    3.6109180444465694,
    1.0,
];
const ERFC_N2: [f64; 9] = [
    0.0025465291507224333,
    0.022255175677805633,
    0.12576669612334676,
    0.46212370807529407,
    1.224350916430729,
    2.302857616690908,
    3.0076650786628667,
    2.481518602470922,
    1.0001666513688992,
];

fn erf_kernel(x: f64) -> f64 {
    if x < f64::MIN_POSITIVE {
        return x * TWO_OVER_SQRTPI;
    }
    let y = x87::mul(x, x);
    let q = horner_x87(&ERF_Q, y);
    let p = horner_x87(&ERF_P, y);
    x87::div(x87::mul(p, x), q)
}

fn erfc_kernel(x: f64) -> f64 {
    let e = x87::exp(-x87::mul(x, x));
    if e == 0.0 {
        return 0.0;
    }
    let (d, n) = if x <= 4.0 {
        (horner_x87(&ERFC_D1, x), horner_x87(&ERFC_N1, x))
    } else {
        (horner_x87(&ERFC_D2, x), horner_x87(&ERFC_N2, x))
    };
    crate::number::flush_subnormal(x87::mul(x87::div(n, d), e))
}

pub fn erf(x: f64) -> f64 {
    if x == 0.0 {
        return 0.0;
    }
    let ax = x.abs();
    let v = if ax < ERF_SWITCH {
        erf_kernel(ax)
    } else {
        x87::sub(1.0, erfc_kernel(ax))
    };
    if x < 0.0 {
        -v
    } else {
        v
    }
}

pub fn erfc(x: f64) -> f64 {
    let ax = x.abs();
    let v = if ax < ERF_SWITCH {
        x87::sub(1.0, erf_kernel(ax))
    } else {
        erfc_kernel(ax)
    };
    if x < 0.0 {
        x87::sub(2.0, v)
    } else {
        v
    }
}

pub fn norm_cdf(z: f64) -> f64 {
    erfc(x87::mul(z, -SQRT1_2)) / 2.0
}
