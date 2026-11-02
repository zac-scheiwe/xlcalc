mod tables;
mod trig;

#[cfg(all(test, windows))]
mod msvcr;

use tables::{
    EXP_FULL, EXP_LEAD, EXP_TAIL, LOG_INV, LOG_LEAD, LOG_TAIL, POW_INV_HEAD, POW_INV_TAIL,
};

pub use trig::{cos, sin};

const SIGN: u64 = 0x8000_0000_0000_0000;
const ABS: u64 = 0x7fff_ffff_ffff_ffff;
const INF: u64 = 0x7ff0_0000_0000_0000;
const NEG_INF: u64 = 0xfff0_0000_0000_0000;
const MANT: u64 = 0x000f_ffff_ffff_ffff;
const QUIET: u64 = 0x0008_0000_0000_0000;

const NAN: u64 = 0xfff8_0000_0000_0000;
const ONE: u64 = 0x3ff0_0000_0000_0000;
const HALF: u64 = 0x3fe0_0000_0000_0000;

const HEAD: u64 = 0xffff_ffff_f800_0000;

const fn f(bits: u64) -> f64 {
    f64::from_bits(bits)
}

const SIXTYFOUR_BY_LN2: f64 = f(0x4057_1547_652b_82fe);

const LN2_LEAD: f64 = f(0x3fe6_2e42_e000_0000);
const LN2_TAIL: f64 = f(0x3e6e_fa39_ef35_793c);

const EXP_UNDERFLOW: f64 = -68800.0;

const EXP_MIN_SUBNORMAL_BITS: i64 = 0xc087_4046_dfef_d9d0_u64 as i64;

fn exp_poly(r: f64) -> f64 {
    let r2 = r * r;
    let r3 = r2 * r;
    let r5 = r2 * r3;
    let a = (0.5 * r + 1.0) * r;
    let b = (f(0x3fa5_5555_5555_5555) * r + f(0x3fc5_5555_5555_5555)) * r3;
    let c = (f(0x3f56_c16c_16c1_6c17) * r + f(0x3f81_1111_1111_1111)) * r5;
    a + (b + c)
}

fn scale(f: f64, m: i32, z: f64) -> f64 {
    let bits = (u64::from(m as u32) + 0x3ff) << 52;
    if bits == INF {
        return f64::from_bits(f.to_bits() | 0x7fe0_0000_0000_0000);
    }
    if m > -1022 || (f >= 1.0 && m == -1022) {
        return f * f64::from_bits(bits);
    }
    if z.to_bits() as i64 > EXP_MIN_SUBNORMAL_BITS {
        return f64::from_bits(1);
    }
    let shift = (m + 1074).max(0) as u32 & 63;
    f * f64::from_bits(1u64 << shift)
}

pub fn exp(x: f64) -> f64 {
    let ux = x.to_bits();
    if ux & INF == INF {
        return match ux {
            INF => x,
            NEG_INF => 0.0,
            _ => f64::from_bits(ux | QUIET),
        };
    }
    if ux & ABS < 0x3c00_0000_0000_0000 {
        return 1.0;
    }
    let t = x * SIXTYFOUR_BY_LN2;
    if t > 65536.0 {
        return f64::INFINITY;
    }
    if t <= EXP_UNDERFLOW {
        return 0.0;
    }
    let n = t.round_ties_even() as i32;
    let dn = f64::from(n);
    let j = (n & 63) as usize;
    let m = (n - j as i32) >> 6;

    let r = (x - f(0x3f86_2e42_fefa_0000) * dn) + dn * f(0xbd1c_f79a_bc9e_3b39);
    let p = exp_poly(r);
    let v = (p * f(EXP_FULL[j]) + f(EXP_TAIL[j])) + f(EXP_LEAD[j]);
    scale(v, m, x)
}

fn log_split(x: f64) -> (f64, u64, usize) {
    let ux = x.to_bits();
    let mut xexp = f64::from(((ux >> 52) as i64 - 1023) as i32);
    let mut mant = ux & MANT;
    if xexp == -1023.0 {
        let g = f64::from_bits(mant | ONE) - 1.0;
        mant = g.to_bits() & MANT;
        xexp = f64::from(((g.to_bits() >> 52) as i64 - 0x7fd) as i32);
    }
    let idx = (mant & 0x000f_f000_0000_0000) + ((mant & 0x0000_0800_0000_0000) << 1);
    (xexp, mant, idx as usize)
}

pub fn log(x: f64) -> f64 {
    let ux = x.to_bits();
    if ux & INF == INF {
        return match ux {
            INF => x,
            NEG_INF => f64::from_bits(NAN),
            _ => f64::from_bits(ux | QUIET),
        };
    }
    let (xexp, mant, idx) = log_split(x);
    if (x - 1.0).abs() < 0.0625 {
        let r = x - 1.0;
        let u = r / (2.0 + r);
        let ru = r * u;
        let u = u + u;
        let v = u * u;
        let a = f(0x3f89_9999_99ba_c6d4) * v + f(0x3fb5_5555_5555_54e6);
        let b = f(0x3f3c_8034_c85d_fff0) * v + f(0x3f62_4923_07f1_519f);
        let u3 = v * u;
        let a = a * u3;
        let u7 = (u3 * u3) * u;
        let b = b * u7;
        return r + ((a + b) - ru);
    }
    if x <= 0.0 {
        return if x == 0.0 {
            f64::NEG_INFINITY
        } else {
            f64::from_bits(NAN)
        };
    }
    let j = (idx as u64 >> 44) as usize;
    let f2 = f64::from_bits(mant | HALF);
    let f1 = f64::from_bits(idx as u64 | HALF);
    let u = (f1 - f2) * f(LOG_INV[j]);
    let u2 = u * u;
    let u4 = u2 * u2;
    let a = (f(0x3fd5_5555_5555_5555) * u + 0.5) * u2 + u;
    let b = ((f(0x3fc5_5555_5555_5555) * u + f(0x3fc9_9999_9999_999a)) * u + 0.25) * u4;
    let t = LN2_TAIL * xexp - (a + b);
    (f(LOG_LEAD[j]) + LN2_LEAD * xexp) + (f(LOG_TAIL[j]) + t)
}

pub fn sqrt(x: f64) -> f64 {
    let ux = x.to_bits();
    if ux & INF == INF && ux & MANT != 0 {
        return f64::from_bits(ux | QUIET);
    }
    if ux & SIGN != 0 && ux & ABS != 0 {
        return f64::from_bits(NAN);
    }
    x.sqrt()
}

pub fn atan(x: f64) -> f64 {
    let ux = x.to_bits();
    let ax = ux & ABS;
    let neg = ux != ax;
    let v = f64::from_bits(ax);
    if ax > INF {
        return f64::from_bits(ux | QUIET);
    }

    let (t, c, clo) = if ax > 0x4003_8000_0000_0000 {
        if v > f(0x43d0_dc00_0000_0000) {
            let pi2 = f(0x3ff9_21fb_5444_2d18);
            return if neg { -pi2 } else { pi2 };
        }
        (-1.0 / v, f(0x3ff9_21fb_5444_2d18), f(0x3c91_a626_3314_5c06))
    } else if ax > 0x3ff3_0000_0000_0000 {
        let t = (v - 1.5) / (v * 1.5 + 1.0);
        (t, f(0x3fef_730b_d281_f69b), f(0x3c70_0788_7af0_cbbc))
    } else if ax > 0x3fe6_0000_0000_0000 {
        let t = (v - 1.0) / (v + 1.0);
        (t, f(0x3fe9_21fb_5444_2d18), f(0x3c81_a626_3314_5c06))
    } else if ax > 0x3fdc_0000_0000_0000 {
        let t = (v * 2.0 - 1.0) / (v + 2.0);
        (t, f(0x3fdd_ac67_0561_bb4f), f(0x3c7a_2b7f_222f_65e0))
    } else {
        (v, 0.0, 0.0)
    };
    let z = t * t;
    let t3 = z * t;
    let p = (((z * f(0x3f22_a75c_e41b_9f87) + f(0x3f9f_2d21_16f0_53f2)) * z
        + f(0x3fcc_3de4_3db4_25c0))
        * z
        + f(0x3fdc_a6be_4c99_3b3c))
        * z
        + f(0x3fd1_2bcb_0a91_69f3);
    let q = (((z * f(0x3fa3_f197_f1e8_5ed9) + f(0x3fdb_2cb0_5bf9_beff)) * z
        + f(0x3ff6_99c6_44c4_8d2e))
        * z
        + f(0x3ffd_372a_17cd_f5a0))
        * z
        + f(0x3fe9_c1b0_8fda_1eec);
    let r = c - (((p * t3) / q - clo) - t);
    if neg {
        -r
    } else {
        r
    }
}

pub fn tan(x: f64) -> f64 {
    sin(x) / cos(x)
}

pub fn pow(x: f64, y: f64) -> f64 {
    let (ux, uy) = (x.to_bits(), y.to_bits());
    if uy & ABS == 0 {
        return 1.0;
    }
    if uy == ONE {
        return if ux & INF == INF && ux & MANT != 0 {
            pow_nan_x(x, y)
        } else {
            x
        };
    }
    if ux & SIGN != 0 {
        return pow_neg_x(x, y);
    }
    if ux == ONE {
        return x;
    }
    if ux == 0 {
        return pow_zero_x(y, 0);
    }
    if ux & INF == INF {
        return pow_inf_nan_x(x, y, 0);
    }
    if uy & INF > 0x43e0_0000_0000_0000 {
        return pow_huge_y(x, y);
    }
    if uy & INF < 0x3c00_0000_0000_0000 {
        return 1.0 + y;
    }
    pow_main(x, y, 0)
}

fn pow_neg_x(x: f64, y: f64) -> f64 {
    let (ux, uy) = (x.to_bits(), y.to_bits());
    if uy & INF > 0x43e0_0000_0000_0000 {
        return pow_huge_y(x, y);
    }
    let ay = uy & ABS;
    let ye = (ay >> 52) as i64 - 1023;
    if ye < 0 {
        return pow_neg_x_fraction(x, y);
    }
    let mut sign = 0;
    if ye <= 53 {
        if (MANT >> ye) & ay != 0 {
            return pow_neg_x_fraction(x, y);
        }
        if (0x0010_0000_0000_0000 >> ye) & ay != 0 {
            sign = SIGN;
        }
    }
    if ux == SIGN {
        return pow_zero_x(y, sign);
    }
    if ux == 0xbff0_0000_0000_0000 {
        return pow_neg_one(y, sign);
    }
    if ux & INF == INF {
        return pow_inf_nan_x(x, y, sign);
    }
    pow_main(f64::from_bits(ux & ABS), y, sign)
}

fn pow_neg_x_fraction(x: f64, y: f64) -> f64 {
    let ux = x.to_bits();
    if ux & INF == INF {
        return pow_inf_nan_x(x, y, 0);
    }
    if ux == SIGN {
        return pow_zero_x(y, 0);
    }
    f64::from_bits(NAN)
}

fn pow_neg_one(y: f64, sign: u64) -> f64 {
    let uy = y.to_bits();
    if uy & INF == INF && uy & MANT != 0 {
        return y;
    }
    f64::from_bits(ONE | sign)
}

fn pow_huge_y(x: f64, y: f64) -> f64 {
    let (ux, uy) = (x.to_bits(), y.to_bits());
    if ux & INF == INF {
        return pow_inf_nan_x(x, y, 0);
    }
    if ux & ABS == 0 {
        return pow_zero_x(y, 0);
    }
    if ux == 0xbff0_0000_0000_0000 {
        return pow_neg_one(y, 0);
    }
    let y_neg = uy & SIGN != 0;
    let big = if ((ux & ABS) as i64) < ONE as i64 {
        y_neg
    } else {
        !y_neg
    };
    if uy & INF == INF && uy & MANT != 0 {
        return y;
    }
    if big {
        f64::INFINITY
    } else {
        0.0
    }
}

fn pow_zero_x(y: f64, sign: u64) -> f64 {
    let uy = y.to_bits();
    if uy & INF == INF {
        return match uy {
            NEG_INF => f64::from_bits(INF | sign),
            INF => 0.0,
            _ => y,
        };
    }
    if uy & SIGN != 0 {
        f64::from_bits(INF | sign)
    } else {
        f64::from_bits(sign)
    }
}

fn pow_inf_nan_x(x: f64, y: f64, sign: u64) -> f64 {
    let (ux, uy) = (x.to_bits(), y.to_bits());
    if ux & MANT != 0 {
        return pow_nan_x(x, y);
    }
    if uy & INF == INF && uy & MANT != 0 {
        return y;
    }
    let r = if uy & SIGN == 0 { INF } else { 0 };
    f64::from_bits(r | sign)
}

fn pow_nan_x(x: f64, y: f64) -> f64 {
    let (ux, uy) = (x.to_bits(), y.to_bits());
    if uy & INF != INF || uy & MANT == 0 {
        return x;
    }

    if ux == NAN {
        return y;
    }
    let mut other = if uy == NAN { ux } else { uy };
    if other & SIGN != 0 {
        other = ux;
    }
    f64::from_bits(if ux & SIGN != 0 { other } else { ux })
}

fn pow_main(x: f64, y: f64, sign: u64) -> f64 {
    let (xexp, mant, idx) = log_split(x);
    let j = (idx as u64 >> 44) as usize;
    let f2 = f64::from_bits(mant | HALF);
    let f1 = f64::from_bits(idx as u64 | HALF);
    let lead = f(LOG_LEAD[j]);
    let tail = f(LOG_TAIL[j]);

    let (a, b, c, u);
    if (x - 1.0).abs() < 0.125 {
        let d = f1 - f2;
        let inv = f(POW_INV_HEAD[j]) + f(POW_INV_TAIL[j]);
        let uh = f64::from_bits((d * inv).to_bits() & HEAD);
        let ut = (d - uh * f1) * inv;
        let w = ut + uh;
        let w2 = w * w;
        let w4 = w2 * w2;
        let p3 = (((f(0x3fc2_4924_9249_2494) * w + f(0x3fc5_5555_5555_5555)) * w
            + f(0x3fc9_9999_9999_999a))
            * w)
            * w4;
        let p1 = ((0.25 * w + f(0x3fd5_5555_5555_5555)) * w) * w2;
        let w5 = (uh * ut + (ut * ut) * 0.5) + ut;
        let q = (uh * uh) * 0.5;
        let s = uh + q;
        let w7 = (uh - s) + q;
        let t = (LN2_TAIL * xexp + tail) - ((p1 + (p3 + w7)) + w5);
        a = lead + LN2_LEAD * xexp;
        b = t - s;
        c = t;
        u = s;
    } else {
        let d = f1 - f2;
        let uh = d * f(POW_INV_HEAD[j]);
        let ut = d * f(POW_INV_TAIL[j]);
        let w = uh + ut;
        let w2 = w * w;
        let w4 = w2 * w2;
        let p1 = (f(0x3fd5_5555_5555_5555) * w + 0.5) * w2;
        let p2 = ((f(0x3fc5_5555_5555_5555) * w + f(0x3fc9_9999_9999_999a)) * w + 0.25) * w4;
        let poly = (p1 + p2) + (ut + (uh - w));
        let t = LN2_TAIL * xexp - poly;
        c = tail + t;
        b = c - w;
        a = lead + LN2_LEAD * xexp;
        u = w;
    }

    let s = a + b;
    let sh = f64::from_bits(s.to_bits() & HEAD);
    let lt = (((a - s) + b) + (c - (u + b))) + (s - sh);
    let yh = f64::from_bits(y.to_bits() & HEAD);
    let yt = y - yh;
    let z3 = (yt * lt + yt * sh) + lt * yh;
    let zh = sh * yh;
    let z = zh + z3;
    let ztail = (zh - z) + z3;

    let with_sign = |v: f64| f64::from_bits(v.to_bits() | sign);
    let t = SIXTYFOUR_BY_LN2 * z;
    if t > 65536.0 {
        return with_sign(f64::INFINITY);
    }
    if t < EXP_UNDERFLOW || t.is_nan() {
        return with_sign(0.0);
    }
    let n = t.round_ties_even() as i32;
    let dn = f64::from(n);
    let j = (n & 63) as usize;
    let m = (n - j as i32) >> 6;

    let r = ((z - f(0x3f86_2e42_f000_0000) * dn) + dn * f(0xbdfd_f473_de6a_f278)) + ztail;
    let p = exp_poly(r);
    let (el, et) = (f(EXP_LEAD[j]), f(EXP_TAIL[j]));
    let v = (el * p + (et * p + et)) + el;
    if m == 1024 && v >= 1.0 {
        return with_sign(f64::INFINITY);
    }
    with_sign(scale(v, m, z))
}
