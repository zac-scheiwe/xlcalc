use super::dd::{self, DD};
use crate::x87;

pub const MACHEP: f64 = 1.11022302462515654042e-16;
const BIG: f64 = 4.503599627370496e15;
const BIGINV: f64 = 2.22044604925031308085e-16;
pub const MAXLOG: f64 = 7.09782712893383996843e2;
pub const MINLOG: f64 = -7.08396418532264106224e2;
pub const SQRT1_2: f64 = std::f64::consts::FRAC_1_SQRT_2;

pub const INV_SQRT_2PI: f64 = 0.3989422804014327;

const STIRLING: [(f64, f64); 12] = [
    (1.0, 12.0),
    (-1.0, 360.0),
    (1.0, 1260.0),
    (-1.0, 1680.0),
    (1.0, 1188.0),
    (-691.0, 360360.0),
    (1.0, 156.0),
    (-3617.0, 122400.0),
    (43867.0, 244188.0),
    (-174611.0, 125400.0),
    (854513.0, 63756.0),
    (-236364091.0, 1506960.0),
];

fn stirling_dd(z: DD) -> DD {
    let lnz = z.ln();
    let mut s = z.add_f(-0.5).mul(lnz).sub(z).add(dd::LN_SQRT_2PI);
    let inv = z.recip();
    let inv2 = inv.mul(inv);
    let mut p = inv;
    for (n, d) in STIRLING {
        let term = p.mul(DD::new(n).div(DD::new(d)));
        s = s.add(term);
        if term.0.abs() < 1e-34 * s.0.abs() {
            break;
        }
        p = p.mul(inv2);
    }
    s
}

pub fn lgamma_dd(x: f64) -> DD {
    if x == 1.0 || x == 2.0 {
        return DD::new(0.0);
    }
    if x >= 20.0 {
        return stirling_dd(DD::new(x));
    }
    let mut z = DD::new(x);
    let mut prod = DD::new(1.0);
    while z.hi() < 20.0 {
        prod = prod.mul(z);
        z = z.add_f(1.0);
    }
    stirling_dd(z).sub(prod.ln())
}

const CODY_SQRTPI: f64 = 0.9189385332046727417803297;

pub fn lgamma(x: f64) -> f64 {
    if !(x > 0.0) {
        return f64::NAN;
    }
    if x.is_infinite() {
        return f64::INFINITY;
    }
    lgamma_dd(x).hi()
}

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
    if !(x > 0.0) {
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

fn sinpi(x: f64) -> f64 {
    let k = x.round();
    let r = x - k;
    let s = (std::f64::consts::PI * r).sin();
    if (k as i64) % 2 == 0 {
        s
    } else {
        -s
    }
}

pub fn gamma(x: f64) -> f64 {
    if x.is_nan() {
        return f64::NAN;
    }
    if x > 0.0 {
        if x > 171.7 {
            return f64::INFINITY;
        }
        if x > 88.0 {
            return lgamma_dd(x).exp().hi();
        }
        let mut x = x;
        let mut z = 1.0;
        while x >= 2.0 {
            x -= 1.0;
            z *= x;
        }
        while x < 1.0 {
            z /= x;
            x += 1.0;
        }
        return z * lgamma_dd(x).exp().hi();
    }
    if x == x.floor() {
        return f64::NAN;
    }

    let g = gamma(1.0 - x);
    if g.is_infinite() {
        return 0.0;
    }
    std::f64::consts::PI / (sinpi(x) * g)
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

fn igam_prefactor(a: f64, x: f64) -> f64 {
    let ax = a * x.ln() - x - lgamma(a);
    if ax < -MAXLOG {
        return 0.0;
    }
    ax.exp()
}

pub fn igam(a: f64, x: f64) -> f64 {
    if x <= 0.0 || a <= 0.0 {
        return 0.0;
    }
    if x > 1.0 && x > a {
        return 1.0 - igamc(a, x);
    }
    let ax = igam_prefactor(a, x);
    if ax == 0.0 {
        return 0.0;
    }
    let mut r = a;
    let mut c = 1.0;
    let mut ans = 1.0;
    loop {
        r += 1.0;
        c *= x / r;
        ans += c;
        if !(c / ans > MACHEP) {
            break;
        }
    }
    ans * ax / a
}

pub fn igamc(a: f64, x: f64) -> f64 {
    if x <= 0.0 || a <= 0.0 {
        return 1.0;
    }
    if x < 1.0 || x < a {
        return 1.0 - igam(a, x);
    }
    if x.is_infinite() {
        return 0.0;
    }
    let ax = igam_prefactor(a, x);
    if ax == 0.0 {
        return 0.0;
    }
    let mut y = 1.0 - a;
    let mut z = x + y + 1.0;
    let mut c = 0.0;
    let mut pkm2 = 1.0;
    let mut qkm2 = x;
    let mut pkm1 = x + 1.0;
    let mut qkm1 = z * x;
    let mut ans = pkm1 / qkm1;
    loop {
        c += 1.0;
        y += 1.0;
        z += 2.0;
        let yc = y * c;
        let pk = pkm1 * z - pkm2 * yc;
        let qk = qkm1 * z - qkm2 * yc;
        let t = if qk != 0.0 {
            let r = pk / qk;
            let t = ((ans - r) / r).abs();
            ans = r;
            t
        } else {
            1.0
        };
        pkm2 = pkm1;
        pkm1 = pk;
        qkm2 = qkm1;
        qkm1 = qk;
        if pk.abs() > BIG {
            pkm2 *= BIGINV;
            pkm1 *= BIGINV;
            qkm2 *= BIGINV;
            qkm1 *= BIGINV;
        }
        if !(t > MACHEP) || c > 10000.0 {
            break;
        }
    }
    ans * ax
}

const TWO_OVER_SQRTPI: f64 = 1.1283791670955126;

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

#[allow(dead_code)]

fn erf_dd(x: f64) -> DD {
    let xd = DD::new(x);
    let y = xd.mul(xd);
    let mut term = xd;
    let mut sum = xd;
    for n in 1..400 {
        term = term.mul(y).div(DD::new(n as f64)).neg();
        let t = term.div(DD::new((2 * n + 1) as f64));
        sum = sum.add(t);
        if t.0.abs() <= 1e-34 * sum.0.abs() {
            break;
        }
    }
    sum.mul(DD(TWO_OVER_SQRTPI, 1.533_545_961_316_588e-17))
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

#[allow(dead_code)]

pub fn erf_root_round(t0: f64, y: f64, upper: bool, bias: f64) -> f64 {
    if !(t0 > 0.0 && t0 < 2.5) {
        return t0;
    }
    let e = erf_dd(t0);
    let r = if upper {
        DD::new(y).sub(DD::new(1.0).sub(e))
    } else {
        e.sub(DD::new(y))
    };
    let ulp = f64::from_bits(t0.to_bits() + 1) - t0;
    t0 + (bias * ulp - r.hi() / (TWO_OVER_SQRTPI * (-(t0 * t0)).exp()))
}

pub fn norm_cdf(z: f64) -> f64 {
    erfc(-z * SQRT1_2) / 2.0
}

#[allow(dead_code)]

pub fn norm_sf(z: f64) -> f64 {
    erfc(z * SQRT1_2) / 2.0
}

pub fn norm_pdf(z: f64) -> f64 {
    INV_SQRT_2PI * (-0.5 * z * z).exp()
}

#[allow(dead_code)]

pub fn norm_inv(p: f64) -> f64 {
    let q = p - 0.5;
    if q.abs() <= 0.425 {
        let r = 0.180625 - q * q;
        return q
            * (((((((r * 2509.0809287301226727 + 33430.575583588128105) * r
                + 67265.770927008700853)
                * r
                + 45921.953931549871457)
                * r
                + 13731.693765509461125)
                * r
                + 1971.5909503065514427)
                * r
                + 133.14166789178437745)
                * r
                + 3.387132872796366608)
            / (((((((r * 5226.495278852545925 + 28729.085735721942674) * r
                + 39307.89580009271061)
                * r
                + 21213.794301586595867)
                * r
                + 5394.1960214247511077)
                * r
                + 687.1870074920579083)
                * r
                + 42.313330701600911252)
                * r
                + 1.0);
    }
    let mut r = if q < 0.0 { p } else { 1.0 - p };
    r = (-r.ln()).sqrt();
    let val = if r <= 5.0 {
        r -= 1.6;
        (((((((r * 7.7454501427834140764e-4 + 0.0227238449892691845833) * r
            + 0.24178072517745061177)
            * r
            + 1.27045825245236838258)
            * r
            + 3.64784832476320460504)
            * r
            + 5.7694972214606914055)
            * r
            + 4.6303378461565452959)
            * r
            + 1.42343711074968357734)
            / (((((((r * 1.05075007164441684324e-9 + 5.475938084995344946e-4) * r
                + 0.0151986665636164571966)
                * r
                + 0.14810397642748007459)
                * r
                + 0.68976733498510000455)
                * r
                + 1.6763848301838038494)
                * r
                + 2.05319162663775882187)
                * r
                + 1.0)
    } else {
        r -= 5.0;
        (((((((r * 2.01033439929228813265e-7 + 2.71155556874348757815e-5) * r
            + 0.0012426609473880784386)
            * r
            + 0.026532189526576123093)
            * r
            + 0.29656057182850489123)
            * r
            + 1.7848265399172913358)
            * r
            + 5.4637849111641143699)
            * r
            + 6.6579046435011037772)
            / (((((((r * 2.04426310338993978564e-15 + 1.4215117583164458887e-7) * r
                + 1.8463183175100546818e-5)
                * r
                + 7.868691311456132591e-4)
                * r
                + 0.0148753612908506148525)
                * r
                + 0.13692988092273580531)
                * r
                + 0.59983220655588793769)
                * r
                + 1.0)
    };
    if q < 0.0 {
        -val
    } else {
        val
    }
}

const MAXGAM: f64 = 171.624376956302725;

#[allow(dead_code)]

pub fn incbet(aa: f64, bb: f64, xx: f64) -> f64 {
    incbet2(aa, bb, xx, 1.0 - xx)
}

pub fn incbet2(aa: f64, bb: f64, xx: f64, xxc: f64) -> f64 {
    if aa <= 0.0 || bb <= 0.0 || !(0.0..=1.0).contains(&xx) {
        return f64::NAN;
    }
    if xx == 0.0 || xxc == 1.0 {
        return 0.0;
    }
    if xx == 1.0 || xxc == 0.0 {
        return 1.0;
    }
    if bb * xx <= 1.0 && xx <= 0.95 {
        return pseries(aa, bb, xx);
    }
    let mut w = xxc;
    let (a, b, x, xc, flag);
    if xx >= aa / (aa + bb) {
        flag = true;
        a = bb;
        b = aa;
        xc = xx;
        x = w;
    } else {
        flag = false;
        a = aa;
        b = bb;
        xc = w;
        x = xx;
    }
    let t;
    if flag && b * x <= 1.0 && x <= 0.95 {
        t = pseries(a, b, x);
    } else {
        let y = x * (a + b - 2.0) - (a - 1.0);
        w = if y < 0.0 {
            incbcf(a, b, x)
        } else {
            incbd(a, b, x, xc) / xc
        };

        let lnx = if xc < 0.5 { (-xc).ln_1p() } else { x.ln() };
        let lnxc = if x < 0.5 { (-x).ln_1p() } else { xc.ln() };
        let mut y = a * lnx;
        let tt = b * lnxc;
        if a + b < MAXGAM && y.abs() < MAXLOG && tt.abs() < MAXLOG {
            let mut t2 = xc.powf(b);
            t2 *= x.powf(a);
            t2 /= a;
            t2 *= w;
            t2 *= gamma(a + b) / (gamma(a) * gamma(b));
            t = t2;
        } else {
            y += tt + lgamma_ratio(a, b);
            y += (w / a).ln();
            t = if y < MINLOG { 0.0 } else { y.exp() };
        }
    }
    if flag {
        if t <= MACHEP {
            1.0 - MACHEP
        } else {
            1.0 - t
        }
    } else {
        t
    }
}

fn incbcf(a: f64, b: f64, x: f64) -> f64 {
    let (mut k1, mut k2, mut k3, mut k4) = (a, a + b, a, a + 1.0);
    let (mut k5, mut k6, mut k7, mut k8) = (1.0, b - 1.0, a + 1.0, a + 2.0);
    let (mut pkm2, mut qkm2, mut pkm1, mut qkm1) = (0.0, 1.0, 1.0, 1.0);
    let mut ans = 1.0;
    let mut r = 1.0;
    let thresh = 3.0 * MACHEP;
    for _ in 0..300 {
        let xk = -(x * k1 * k2) / (k3 * k4);
        let pk = pkm1 + pkm2 * xk;
        let qk = qkm1 + qkm2 * xk;
        pkm2 = pkm1;
        pkm1 = pk;
        qkm2 = qkm1;
        qkm1 = qk;
        let xk = (x * k5 * k6) / (k7 * k8);
        let pk = pkm1 + pkm2 * xk;
        let qk = qkm1 + qkm2 * xk;
        pkm2 = pkm1;
        pkm1 = pk;
        qkm2 = qkm1;
        qkm1 = qk;
        if qk != 0.0 {
            r = pk / qk;
        }
        let t = if r != 0.0 {
            let t = ((ans - r) / r).abs();
            ans = r;
            t
        } else {
            1.0
        };
        if t < thresh {
            break;
        }
        k1 += 1.0;
        k2 += 1.0;
        k3 += 2.0;
        k4 += 2.0;
        k5 += 1.0;
        k6 -= 1.0;
        k7 += 2.0;
        k8 += 2.0;
        if qk.abs() + pk.abs() > BIG {
            pkm2 *= BIGINV;
            pkm1 *= BIGINV;
            qkm2 *= BIGINV;
            qkm1 *= BIGINV;
        }
        if qk.abs() < BIGINV || pk.abs() < BIGINV {
            pkm2 *= BIG;
            pkm1 *= BIG;
            qkm2 *= BIG;
            qkm1 *= BIG;
        }
    }
    ans
}

fn incbd(a: f64, b: f64, x: f64, xc: f64) -> f64 {
    let (mut k1, mut k2, mut k3, mut k4) = (a, b - 1.0, a, a + 1.0);
    let (mut k5, mut k6, mut k7, mut k8) = (1.0, a + b, a + 1.0, a + 2.0);
    let (mut pkm2, mut qkm2, mut pkm1, mut qkm1) = (0.0, 1.0, 1.0, 1.0);
    let z = x / xc;
    let mut ans = 1.0;
    let mut r = 1.0;
    let thresh = 3.0 * MACHEP;
    for _ in 0..300 {
        let xk = -(z * k1 * k2) / (k3 * k4);
        let pk = pkm1 + pkm2 * xk;
        let qk = qkm1 + qkm2 * xk;
        pkm2 = pkm1;
        pkm1 = pk;
        qkm2 = qkm1;
        qkm1 = qk;
        let xk = (z * k5 * k6) / (k7 * k8);
        let pk = pkm1 + pkm2 * xk;
        let qk = qkm1 + qkm2 * xk;
        pkm2 = pkm1;
        pkm1 = pk;
        qkm2 = qkm1;
        qkm1 = qk;
        if qk != 0.0 {
            r = pk / qk;
        }
        let t = if r != 0.0 {
            let t = ((ans - r) / r).abs();
            ans = r;
            t
        } else {
            1.0
        };
        if t < thresh {
            break;
        }
        k1 += 1.0;
        k2 -= 1.0;
        k3 += 2.0;
        k4 += 2.0;
        k5 += 1.0;
        k6 += 1.0;
        k7 += 2.0;
        k8 += 2.0;
        if qk.abs() + pk.abs() > BIG {
            pkm2 *= BIGINV;
            pkm1 *= BIGINV;
            qkm2 *= BIGINV;
            qkm1 *= BIGINV;
        }
        if qk.abs() < BIGINV || pk.abs() < BIGINV {
            pkm2 *= BIG;
            pkm1 *= BIG;
            qkm2 *= BIG;
            qkm1 *= BIG;
        }
    }
    ans
}

fn pseries(a: f64, b: f64, x: f64) -> f64 {
    let ai = 1.0 / a;
    let mut u = (1.0 - b) * x;
    let mut v = u / (a + 1.0);
    let t1 = v;
    let mut t = u;
    let mut n = 2.0;
    let mut s = 0.0;
    let z = MACHEP * ai;
    while v.abs() > z && n < 1e6 {
        u = (n - b) * x / n;
        t *= u;
        v = t / (a + n);
        s += v;
        n += 1.0;
    }
    s += t1;
    s += ai;
    let u = a * x.ln();
    if a + b < MAXGAM && u.abs() < MAXLOG {
        let t = gamma(a + b) / (gamma(a) * gamma(b));
        s * t * x.powf(a)
    } else {
        let t = lgamma_ratio(a, b) + u + s.ln();
        if t < MINLOG {
            0.0
        } else {
            t.exp()
        }
    }
}

fn lgamma_ratio(a: f64, b: f64) -> f64 {
    lgamma_dd(a + b).sub(lgamma_dd(a)).sub(lgamma_dd(b)).hi()
}

#[allow(dead_code)]

pub fn lbeta(a: f64, b: f64) -> f64 {
    lgamma_dd(a).add(lgamma_dd(b)).sub(lgamma_dd(a + b)).hi()
}

pub fn solve_increasing(mut lo: f64, mut hi: f64, target: f64, f: impl Fn(f64) -> f64) -> f64 {
    debug_assert!(lo >= 0.0 && hi > lo);
    loop {
        let (bl, bh) = (lo.to_bits(), hi.to_bits());
        if bh - bl <= 1 {
            break;
        }
        let mid = f64::from_bits(bl + (bh - bl) / 2);
        if f(mid) < target {
            lo = mid;
        } else {
            hi = mid;
        }
    }
    let (fl, fh) = (f(lo), f(hi));
    if (target - fl).abs() <= (fh - target).abs() {
        lo
    } else {
        hi
    }
}
