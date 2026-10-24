use super::dist::{
    lbeta_xl, stirlerr, xl_beta_pdf, xl_expm1, xl_igamma, xl_log1p, xl_log1pmx, xl_pow, XlLogSum,
};
use super::special::erfc;
use crate::x87;

const EPS10: f64 = 2.220446049250313e-15;

fn flip(v: f64) -> f64 {
    x87::add(x87::sub(0.5, v), 0.5)
}

enum Tail {
    Lower(f64),
    Upper(f64),
}

pub(super) fn xl_ibeta(x: f64, y: f64, a: f64, b: f64) -> Option<(f64, f64)> {
    if !(x > 0.0) {
        return Some((0.0, 1.0));
    }
    if !(x < 1.0) {
        return Some((1.0, 0.0));
    }
    let pair = |w: f64| (w, flip(w));
    let apb = x87::add(a, b);
    if a == 0.5 && b == 0.5 {
        let s = x87::sqrt(x);
        let t = x87::sub(1.0, s);
        let v = x87::mul(x87::sub(2.0, t), t);
        let r = x87::atan(x87::div(s, x87::sqrt(v)));
        return Some(pair(x87::div(x87::mul(r, 2.0), std::f64::consts::PI)));
    }

    if b == 1.0 {
        if x > 0.9 {
            return Some(pair(flip(-xl_expm1(x87::mul(x87::ln(x), a)))));
        }
        return Some(pair(xl_pow(x, a)));
    }
    if a == 1.0 {
        if x < 0.1 {
            return Some(pair(-xl_expm1(x87::mul(xl_log1p(-x), b))));
        }

        let w1 = xl_pow(x87::sub(1.0, x), b);
        return Some((flip(w1), w1));
    }
    let (tail, swap) = if a.min(b) <= 1.0 {
        let swap = x > 0.5;
        let (a0, b0, x0, y0) = if swap { (b, a, y, x) } else { (a, b, x, y) };

        let t = if a0.max(b0) > 1.0 {
            if b0 <= 1.0 {
                Tail::Lower(bpser(a0, b0, x0)?)
            } else if x0 >= 0.3 {
                Tail::Upper(bpser(b0, a0, y0)?)
            } else if x0 < 0.1 && x87::mul(x87::ln(x87::mul(b0, x0)), a0) <= 0.7f64.ln() {
                Tail::Lower(bpser(a0, b0, x0)?)
            } else if b0 > 15.0 {
                Tail::Upper(bgrat(b0, a0, y0, x0, 0.0)?)
            } else {
                let w1 = bup(b0, a0, y0, x0, 20);
                Tail::Upper(bgrat(x87::add(b0, 20.0), a0, y0, x0, w1)?)
            }
        } else if a0 >= 0.2f64.min(b0) || x87::mul(x87::ln(x0), a0) <= 0.9f64.ln() {
            Tail::Lower(bpser(a0, b0, x0)?)
        } else if x0 >= 0.3 {
            Tail::Upper(bpser(b0, a0, y0)?)
        } else {
            let w1 = bup(b0, a0, y0, x0, 20);
            Tail::Upper(bgrat(x87::add(b0, 20.0), a0, y0, x0, w1)?)
        };
        (t, swap)
    } else {
        let lambda = if a >= b {
            x87::sub(x87::mul(apb, y), b)
        } else {
            x87::sub(a, x87::mul(apb, x))
        };
        let swap = lambda < 0.0;
        let (a0, b0, x0, y0) = if swap { (b, a, y, x) } else { (a, b, x, y) };
        let t = if b0 < 40.0 {
            if x87::mul(b0, x0) <= 0.7 {
                Tail::Lower(bpser(a0, b0, x0)?)
            } else {
                let mut n = b0.trunc();
                let mut b1 = x87::add(-n, b0);
                if b1 == 0.0 {
                    n -= 1.0;
                    b1 = 1.0;
                }
                let w = bup(b1, a0, y0, x0, n as usize);
                if x0 <= 0.7 {
                    Tail::Lower(x87::add(bpser(a0, b1, x0)?, w))
                } else if a0 <= 15.0 {
                    let w = x87::add(w, bup(a0, b1, x0, y0, 20));
                    Tail::Lower(bgrat(x87::add(a0, 20.0), b1, x0, y0, w)?)
                } else {
                    Tail::Lower(bgrat(a0, b1, x0, y0, w)?)
                }
            }
        } else {
            let m = a0.min(b0);
            if m > 10000.0 && lambda.abs() <= x87::mul(m, 0.03) {
                Tail::Lower(basym(a0, b0, lambda.abs()))
            } else {
                Tail::Lower(bfrac(a0, b0, x0, y0)?)
            }
        };
        (t, swap)
    };
    let (w, w1) = match tail {
        Tail::Lower(w) => (w, flip(w)),
        Tail::Upper(w1) => (flip(w1), w1),
    };
    Some(if swap { (w1, w) } else { (w, w1) })
}

fn bpser(a: f64, b: f64, x: f64) -> Option<f64> {
    let mut sum = x87::div(1.0, a);
    let (mut c, mut n) = (1.0, 1.0);
    for _ in 0..1_000_000 {
        let f = x87::add(x87::add(-x87::div(b, n), 0.5), 0.5);
        c = x87::mul(x87::mul(f, x), c);
        sum = x87::add(x87::div(c, x87::add(a, n)), sum);
        let tol = x87::mul(sum, EPS10);
        n = x87::add(n, 1.0);
        if !(c.abs() > tol) {
            let mut s = XlLogSum::new(-lbeta_xl(a, b));
            s.add(x87::mul(x87::ln(x), a));
            return Some(x87::mul(x87::exp(s.err), x87::mul(x87::exp(s.s), sum)));
        }
    }
    None
}

fn bup(a: f64, b: f64, x: f64, y: f64, n: usize) -> f64 {
    let w0 = x87::mul(x87::div(x87::mul(x, y), a), pdf_xy(x, y, a, b));
    if n <= 1 {
        return w0;
    }
    let nm1 = n - 1;
    let ap1 = x87::add(a, 1.0);
    let apb = x87::add(a, b);
    let mut p = Vec::with_capacity(nm1);
    p.push(x87::div(apb, ap1));
    for i in 1..nm1 {
        let j = (i + 1) as f64;
        let t1 = x87::sub(x87::add(ap1, j), 1.0);
        let t2 = x87::sub(x87::add(apb, j), 1.0);
        p.push(x87::mul(x87::div(t2, t1), p[i - 1]));
    }
    let mut deg = nm1 - 1;
    if b > 1.0 && y > 1e-4 {
        let r = x87::sub(x87::div(x87::mul(x87::sub(b, 1.0), x), y), a);
        if r < nm1 as f64 {
            deg = if r >= 1.0 { r as usize } else { 0 };
        }
    }
    let mut w = 1.0;
    if deg > 0 {
        let mut acc = p[deg - 1];
        for j in (1..deg).rev() {
            acc = x87::add(x87::mul(acc, x), p[j - 1]);
        }
        w = x87::add(x87::mul(acc, x), 1.0);
    }
    for i in deg + 1..=nm1 {
        let d = x87::mul(xl_pow(x, i as f64), p[i - 1]);
        w = x87::add(w, d);
        if d <= x87::mul(w, EPS10) {
            break;
        }
    }
    x87::mul(w0, w)
}

fn bgrat(a: f64, b: f64, x: f64, y: f64, w: f64) -> Option<f64> {
    let nu = x87::add(a, x87::mul(x87::sub(b, 1.0), 0.5));
    let ln_nu = x87::ln(nu);
    let c = bgrat_terms(b, x, y);
    let lnx = bgrat_lnx(x, y);
    let zn = x87::mul(lnx, nu);
    let mut s1 = XlLogSum::new(zn);
    s1.add(-ln_nu);
    let mut s2 = XlLogSum::new(s1.s);
    s2.add(-lbeta_xl(b, a));
    let u = x87::mul(x87::mul(x87::exp(s2.s), x87::exp(s1.err)), x87::exp(s2.err));
    let r = x87::div(1.0, nu);
    let mut sum = c[15];
    for k in (0..15).rev() {
        sum = x87::add(x87::mul(sum, r), c[k]);
    }
    let z = -zn;
    let q = if b == 0.5 {
        erfc(x87::sqrt(z))
    } else {
        xl_igamma(b, z)?.1
    };
    Some(x87::add(w, x87::add(x87::mul(u, sum), q)))
}

const BGRAT_E: [&[f64]; 9] = [
    &[
        1.0431236494104825e-18,
        -2.440909339620529e-17,
        2.6086436224457346e-16,
        -1.662822547052262e-15,
        6.92579893291736e-15,
        -1.941200467259431e-14,
        3.631578251286476e-14,
        -4.3296294615495955e-14,
        2.947803854668558e-14,
        -8.585996549822947e-15,
    ],
    &[
        2.253147082726642e-16,
        -4.326042398835153e-15,
        3.7312115689953195e-14,
        -1.8775024010944564e-13,
        5.986592369068807e-13,
        -1.2282459818942528e-12,
        1.5671058697878986e-12,
        -1.1219379578394035e-12,
        3.389576851489321e-13,
    ],
    &[
        4.326042398835153e-14,
        -6.662105294206136e-13,
        4.5077361795862295e-12,
        -1.7257448337433193e-11,
        4.003490430283252e-11,
        -5.568192566232827e-11,
        4.2400586543043315e-11,
        -1.3380902920268335e-11,
    ],
    &[
        7.267751230043057e-12,
        -8.721301476051669e-11,
        4.4582462545435557e-10,
        -1.232942848671533e-09,
        1.925051202821589e-09,
        -1.5861487128460714e-09,
        5.281609967721337e-10,
    ],
    &[
        1.0465561771262003e-09,
        -9.419005594135802e-09,
        3.450645224010386e-08,
        -6.369041877939447e-08,
        5.8392398028176384e-08,
        -2.0835982071876168e-08,
    ],
    &[
        1.2558674125514404e-07,
        -8.037551440329218e-07,
        1.9476709472369195e-06,
        -2.0897633744855966e-06,
        8.202608300264551e-07,
    ],
    &[
        1.2056327160493826e-05,
        -5.063657407407407e-05,
        7.061563051146384e-05,
        -3.2035383597883595e-05,
    ],
    &[
        0.0008680555555555555,
        -0.0020833333333333333,
        0.0012152777777777778,
    ],
    &[0.041666666666666664, -0.041666666666666664],
];

fn bgrat_lnx(x: f64, y: f64) -> f64 {
    if x >= 0.99 {
        xl_log1p(-y)
    } else {
        x87::ln(x)
    }
}

fn bgrat_terms(b: f64, x: f64, y: f64) -> [f64; 16] {
    let t = -bgrat_lnx(x, y);
    let l = x87::mul(t, t);
    let mut w = [0.0; 9];
    for (k, poly) in BGRAT_E.iter().enumerate() {
        let mut acc = poly[0];
        for &c in &poly[1..] {
            acc = x87::add(x87::mul(acc, b), c);
        }
        w[8 - k] = acc;
    }

    let mut bl = [0.0; 18];
    bl[1] = x87::add(b, 1.0);
    for m in 2..18 {
        bl[m] = x87::add(bl[m - 1], 1.0);
    }
    let pw1 = xl_pow(t, bl[1]);
    let pw0 = xl_pow(t, b);
    let horner = |w: &[f64]| {
        let mut acc = w[w.len() - 1];
        for j in (0..w.len() - 1).rev() {
            acc = x87::add(x87::mul(acc, l), w[j]);
        }
        acc
    };
    let mut c = [0.0; 16];
    c[0] = x87::mul(horner(&w), pw1);
    let mut lo = 0;
    for n in 1..16 {
        for j in lo..9 {
            let m = 2 * j + 2 - n;
            if m == 0 {
                lo += 1;
            } else {
                w[j] = x87::mul(w[j], bl[m]);
            }
        }
        c[n] = x87::mul(horner(&w[lo..]), if n % 2 == 1 { pw0 } else { pw1 });
    }
    c
}

fn basym(a: f64, b: f64, lambda: f64) -> f64 {
    const E0: f64 = 1.1283791670955126;
    const E1: f64 = 0.3535533905932738;
    let bma = x87::sub(b, a);
    let (h, r1, w0) = if a < b {
        let h = x87::div(a, b);
        (h, x87::div(bma, b), x87::mul(x87::add(h, 1.0), a))
    } else {
        let h = x87::div(b, a);
        (h, x87::div(bma, a), x87::mul(x87::add(h, 1.0), b))
    };
    let w0 = x87::div(1.0, x87::sqrt(w0));
    let r0 = x87::div(1.0, x87::add(1.0, h));
    let fa = x87::mul(xl_log1pmx(x87::div(lambda, -a)), -a);
    let fb = x87::mul(xl_log1pmx(x87::div(lambda, b)), -b);
    let f = x87::add(fa, fb);
    let t = x87::exp(x87::sub(0.0, f));
    if t == 0.0 {
        return 0.0;
    }
    let z0 = x87::sqrt(f);
    let z = x87::mul(z0, std::f64::consts::SQRT_2);
    let z2 = x87::add(f, f);

    let mut a0 = [0.0f64; 22];
    let mut b0 = [0.0f64; 22];
    let mut c = [0.0f64; 22];
    let mut d = [0.0f64; 22];
    a0[1] = x87::mul(r1, 0.6666666666666666);
    c[1] = x87::mul(a0[1], -0.5);
    d[1] = x87::sub(0.0, c[1]);

    let ex = if z0 > 26.641747557046326 {
        erfc_exp_x2(z0)
    } else {
        x87::mul(x87::exp(x87::mul(z0, z0)), erfc(z0))
    };
    let mut j0 = x87::mul(ex, 0.443113462726379);
    let mut j1 = E1;
    let mut sum = x87::add(x87::mul(x87::mul(E1, w0), d[1]), j0);
    let h2 = x87::mul(h, h);
    let (mut s, mut hn, mut w, mut znm1, mut zn) = (1.0, 1.0, w0, z, z2);
    let mut nd = 0.0;
    let mut n = 2usize;
    while n <= 20 {
        nd = x87::add(nd, 2.0);
        hn = x87::mul(hn, h2);
        let p = x87::mul(x87::mul(x87::add(x87::mul(hn, h), 1.0), 2.0), r0);
        a0[n] = x87::div(p, x87::add(nd, 2.0));
        s = x87::add(s, hn);
        a0[n + 1] = x87::mul(x87::mul(x87::div(s, x87::add(nd, 3.0)), 2.0), r1);
        for i in n..=n + 1 {
            let fi = i as f64;
            let r = x87::mul(x87::add(fi, 1.0), -0.5);
            b0[1] = x87::mul(r, a0[1]);
            for m in 2..=i {
                let mut bsum = 0.0;
                for j in 1..m {
                    let mut v = x87::sub(x87::mul(j as f64, r), (m - j) as f64);
                    v = x87::mul(x87::mul(v, a0[j]), b0[m - j]);
                    bsum = x87::add(v, bsum);
                }
                b0[m] = x87::add(x87::div(bsum, m as f64), x87::mul(r, a0[m]));
            }
            c[i] = x87::div(b0[i], x87::add(fi, 1.0));
            let mut dsum = 0.0;
            for j in 1..i {
                dsum = x87::add(x87::mul(d[i - j], c[j]), dsum);
            }
            d[i] = x87::sub(0.0, x87::add(dsum, c[i]));
        }
        j0 = x87::add(x87::mul(E1, znm1), x87::mul(x87::sub(nd, 1.0), j0));
        j1 = x87::add(x87::mul(E1, zn), x87::mul(j1, nd));
        znm1 = x87::mul(znm1, z2);
        zn = x87::mul(zn, z2);
        w = x87::mul(w, w0);
        let t0 = x87::mul(x87::mul(w, j0), d[n]);
        w = x87::mul(w, w0);
        let t1 = x87::mul(x87::mul(w, j1), d[n + 1]);
        sum = x87::add(x87::add(t0, t1), sum);
        let tol = x87::mul(sum, f64::EPSILON);
        if !(x87::add(t0.abs(), t1.abs()) > tol) {
            break;
        }
        n += 2;
    }
    let apb = x87::add(a, b);
    let (lo, hi) = if a > b { (b, a) } else { (a, b) };
    let bcorr = x87::add(x87::sub(stirlerr(hi), stirlerr(apb)), stirlerr(lo));
    let u = x87::exp(x87::sub(0.0, bcorr));
    x87::mul(x87::mul(x87::mul(u, sum), t), E0)
}

fn erfc_exp_x2(x: f64) -> f64 {
    let q = x87::mul(x87::mul(x, x), 4.0);
    let (mut term, mut sum, mut k, mut m) = (1.0, 1.0, 0.0, 0.0);
    for _ in 0..6 {
        term = x87::mul(term, -1.0);
        k = x87::add(k, 1.0);
        term = x87::mul(term, k);
        k = x87::add(k, 1.0);
        term = x87::mul(term, k);
        m = x87::add(m, 1.0);
        term = x87::div(term, m);
        term = x87::div(term, q);
        sum = x87::add(sum, term);
    }
    x87::mul(x87::div(sum, x), 0.5641895835477563)
}

fn pdf_xy(x: f64, y: f64, a: f64, b: f64) -> f64 {
    if x > 0.99 {
        xl_beta_pdf(y, b, a)
    } else {
        xl_beta_pdf(x, a, b)
    }
}

fn bfrac(a: f64, b: f64, x: f64, y: f64) -> Option<f64> {
    let apb = x87::add(a, b);

    let lambda = x87::sub(a, x87::mul(apb, x));
    let pdf = pdf_xy(x, y, a, b);
    let brc = x87::mul(pdf, x87::mul(x, y));
    let c = x87::add(lambda, 1.0);
    let yp1 = x87::add(y, 1.0);
    let (mut f, mut cc, mut d) = (1e-30, 1e-30, 0.0);
    let (mut alpha, mut beta) = (1.0, x87::mul(x87::div(a, x87::add(a, 1.0)), c));
    let mut n = 0.0;
    for _ in 0..100_000 {
        d = x87::add(x87::mul(alpha, d), beta);
        cc = x87::add(x87::div(alpha, cc), beta);
        d = x87::div(1.0, d);
        let delta = x87::mul(cc, d);
        n = x87::add(n, 1.0);
        f = x87::mul(f, delta);
        if x87::sub(delta, 1.0).abs() <= EPS10 {
            return Some(x87::mul(brc, f));
        }
        let an1 = x87::add(a, n);
        let a2n = x87::add(an1, n);
        let s = x87::sub(a2n, 1.0);
        let e1 = x87::div(x87::add(an1, -1.0), s);
        let mut t = x87::div(x87::add(x87::add(apb, n), -1.0), s);
        t = x87::mul(x87::mul(t, e1), n);
        let bmn = x87::sub(b, n);
        alpha = x87::mul(x87::mul(x87::mul(t, bmn), x), x);
        let w = x87::mul(x87::mul(n, x), bmn);
        let beta1 = x87::add(x87::div(w, s), n);
        let u = x87::add(x87::mul(yp1, n), c);
        let e2 = x87::div(an1, x87::add(a2n, 1.0));
        beta = x87::add(x87::mul(e2, u), beta1);
    }
    None
}
