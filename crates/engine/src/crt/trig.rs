use super::tables::PIBITS;
use super::{f, ABS, HEAD, INF, MANT, NAN, QUIET, SIGN};

const PIBY2: f64 = f(0x3ff9_21fb_5444_2d18);
const TWO_BY_PI: f64 = f(0x3fe4_5f30_6dc9_c883);

const PIBY2_1: f64 = f(0x3ff9_21fb_5440_0000);
const PIBY2_1TAIL: f64 = f(0x3dd0_b461_1a62_6331);
const PIBY2_2: f64 = f(0x3dd0_b461_1a60_0000);
const PIBY2_2TAIL: f64 = f(0x3ba3_198a_2e03_7073);

const BIG: u64 = 0x411e_8480_0000_0000;

const C: [f64; 6] = [
    f(0x3fa5_5555_5555_5555),
    f(0xbf56_c16c_16c1_6967),
    f(0x3efa_01a0_19f4_ec91),
    f(0xbe92_7e4f_a17f_667b),
    f(0x3e21_eeb6_9038_2eec),
    f(0xbda9_07db_4725_8aa7),
];

const S: [f64; 6] = [
    f(0xbfc5_5555_5555_5555),
    f(0x3f81_1111_1111_0bb3),
    f(0xbf2a_01a0_19e8_3e5c),
    f(0x3ec7_1de3_796c_de01),
    f(0xbe5a_e600_b42f_dfa7),
    f(0x3de5_e0b2_f9a4_3bb8),
];
const SIXTH: f64 = f(0x3fc5_5555_5555_5555);

fn sin_piby4(r: f64, rr: f64) -> f64 {
    let x2 = r * r;
    let a = ((S[5] * x2 + S[4]) * x2 + S[3]) * ((x2 * x2) * x2);
    let b = (S[2] * x2 + S[1]) * x2 + S[0];
    let x3 = r * x2;
    (rr + (x3 * (a + b) - (x2 * 0.5) * rr)) + r
}

fn cos_piby4(r: f64, rr: f64) -> f64 {
    let x2 = r * r;
    let xx = rr * r;
    let h = x2 * 0.5;
    let t = (((0.5 * x2 - 1.0) + 1.0) - h) - xx;
    let x6 = (x2 * x2) * x2;
    let a = (C[2] * x2 + C[1]) * x2 + C[0];
    let b = ((C[5] * x2 + C[4]) * x2 + C[3]) * x6;
    ((a + b) * (x2 * x2) + t) - (h - 1.0)
}

fn sin_small(x: f64, ax: u64) -> f64 {
    if ax < 0x3e40_0000_0000_0000 {
        return x;
    }
    let x2 = x * x;
    if ax < 0x3f20_0000_0000_0000 {
        return x - ((x2 * x) * SIXTH);
    }
    let a = ((S[5] * x2 + S[4]) * x2 + S[3]) * ((x2 * x2) * x2);
    let b = (S[2] * x2 + S[1]) * x2 + S[0];
    x + (x * x2) * (a + b)
}

fn cos_small(x: f64, ax: u64) -> f64 {
    if ax < 0x3e40_0000_0000_0000 {
        return 1.0;
    }
    let x2 = x * x;
    if ax < 0x3f20_0000_0000_0000 {
        return 1.0 - x2 * 0.5;
    }
    let x4 = x2 * x2;
    let x8 = x4 * x4;
    let a = (C[1] * x2 + C[0]) * x4;
    let b = (C[3] * x2 + C[2]) * x8;
    let c = (x4 * x8) * (C[5] * x2 + C[4]);
    let r = x2 * -0.5;
    let t = r + 1.0;
    (((1.0 - t) + r) + ((a + b) + c)) + t
}

enum Reduced {
    Full(f64, f64),

    Short(f64),

    Tiny(f64),
}

fn reduce(x: f64, ax: u64) -> (Reduced, i32) {
    if ax >= BIG {
        let (r, rr, region) = remainder_piby2(x);
        return (Reduced::Full(r, rr), region);
    }
    let npi2 = (x * TWO_BY_PI + 0.5) as i32;
    let dn = f64::from(npi2);
    let mut rhead = x - PIBY2_1 * dn;
    let mut rtail = PIBY2_1TAIL * dn;
    let mut r = rhead - rtail;
    let xexp = (ax >> 52) as i64;
    let rexp = ((r.to_bits() << 1) >> 53) as i64;
    if xexp - rexp > 15 {
        let t = rhead;
        rtail = PIBY2_2 * dn;
        rhead = t - rtail;
        rtail = PIBY2_2TAIL * dn - ((t - rhead) - rtail);
        r = rhead - rtail;
    }
    let rr = (rhead - r) - rtail;
    let red = if rexp >= 0x3f2 {
        Reduced::Full(r, rr)
    } else if rexp > 0x3de {
        Reduced::Short(r)
    } else {
        Reduced::Tiny(r)
    };
    (red, npi2)
}

fn special(ux: u64) -> f64 {
    if ux & MANT == 0 {
        f64::from_bits(NAN)
    } else {
        f64::from_bits(ux | QUIET)
    }
}

pub fn sin(x: f64) -> f64 {
    let ux = x.to_bits();
    if ux & INF == INF {
        return special(ux);
    }
    let ax = ux & ABS;
    if ax <= 0x3fe9_21fb_5444_2d18 {
        return sin_small(x, ax);
    }
    let neg = ux != ax;
    let x = if neg { 0.0 - x } else { x };
    let (red, region) = reduce(x, ax);
    let odd = region & 1 != 0;
    let v = match red {
        Reduced::Full(r, rr) if odd => cos_piby4(r, rr),
        Reduced::Full(r, rr) => sin_piby4(r, rr),
        Reduced::Short(r) if odd => 1.0 - (r * r) * 0.5,
        Reduced::Short(r) => r - (SIXTH * r) * (r * r),
        Reduced::Tiny(_) if odd => 1.0,
        Reduced::Tiny(r) => r,
    };
    if ((region >> 1) & 1 != 0) != neg {
        0.0 - v
    } else {
        v
    }
}

pub fn cos(x: f64) -> f64 {
    let ux = x.to_bits();
    if ux & INF == INF {
        return special(ux);
    }
    let ax = ux & ABS;
    if ax <= 0x3fe9_21fb_5444_2d18 {
        return cos_small(x, ax);
    }
    let x = if ux != ax { 0.0 - x } else { x };
    let (red, region) = reduce(x, ax);
    let odd = region & 1 != 0;
    let v = match red {
        Reduced::Full(r, rr) if odd => sin_piby4(r, rr),
        Reduced::Full(r, rr) => cos_piby4(r, rr),
        Reduced::Short(r) if odd => r - (SIXTH * r) * (r * r),
        Reduced::Short(r) => 1.0 - (r * r) * 0.5,
        Reduced::Tiny(r) if odd => r,
        Reduced::Tiny(_) => 1.0,
    };
    if region.wrapping_add(1) & 2 != 0 {
        0.0 - v
    } else {
        v
    }
}

fn remainder_piby2(x: f64) -> (f64, f64, i32) {
    const MASK: u64 = 0x3ff;
    const TOP: u64 = 1 << 53;
    let ux = x.to_bits();
    let mant = (ux & MANT) | (1 << 52);
    let xexp = ((ux >> 52) & 0x7ff) as i32 - 1023;
    let first = (xexp / 10) as usize;
    let resexp = xexp - first as i32 * 10;

    let mut res = [0u64; 20];
    let mut carry = 0;
    for i in (0..19).rev() {
        let t = PIBITS[first + i].wrapping_mul(mant).wrapping_add(carry);
        res[i] = t & MASK;
        carry = t >> 10;
    }
    let shift = (10 - resexp) as u32;
    let top3 = (((res[0] << 10) | res[1]) >> (shift - 1)) & 7;
    let neg = top3 & 1 != 0;
    let region;
    let mut i = 1;
    let mut hi = ((1u64 << shift) - 1) & if neg { !res[1] } else { res[1] };
    if neg {
        region = (((top3 >> 1) + 1) & 3) as i32;
        while hi < TOP {
            i += 1;
            hi = (hi << 10) | (!res[i] & MASK);
        }
    } else {
        region = (top3 >> 1) as i32;
        while hi < TOP {
            i += 1;
            hi = (hi << 10) | res[i];
        }
    }
    let mut lo = (if neg { !res[i + 1] } else { res[i + 1] }) << 54;
    let mut e = resexp - 10 * i as i32 + 52;
    while hi >= TOP {
        e += 1;
        lo = (hi << 63) | (lo >> 1);
        hi >>= 1;
    }
    let mut dbits = ((i64::from(e) + 0x3ff) as u64) << 52 | (hi & MANT);
    if neg {
        dbits |= SIGN;
    }
    let d = f64::from_bits(dbits);
    let base = ((i64::from(e) + 0x3ca) as u64) << 52;
    let tail = (f64::from_bits((lo >> 12) | base) - f64::from_bits(base))
        * if neg { -2.0 } else { 2.0 }
        * PIBY2;
    let dh = f64::from_bits(d.to_bits() & HEAD);
    let dt = d - dh;
    let a = d * f(0x3c91_a626_3314_5c06) + tail;
    let p = d * PIBY2;
    let (h, ht) = (f(0x3ff9_21fb_5000_0000), f(0x3e51_10b4_6000_0000));
    let s = ((((dh * h - p) + dt * h) + dh * ht) + dt * ht) + a;
    let r = s + p;
    (r, (p - r) + s, region)
}
