mod tables;
#[cfg(all(test, windows))]
mod tests;

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum Errno {
    None,

    Domain,

    Range,
}

const SIGN: u64 = 0x8000_0000_0000_0000;
const ABS: u64 = 0x7fff_ffff_ffff_ffff;
const ONE: u64 = 0x3ff0_0000_0000_0000;
const QUIET: u64 = 0x0008_0000_0000_0000;

const INF2: u64 = 0xffe0_0000_0000_0000;

const INDEFINITE: u64 = 0xfff8_0000_0000_0000;

const SIGN_BIAS: u64 = 0x800 << 8;

const OFF: u64 = 0x3fe6_9555_0000_0000;

const fn f(bits: u64) -> f64 {
    f64::from_bits(bits)
}

const LN2_HI: f64 = f(0x3fe6_2e42_fefa_3800);
const LN2_LO: f64 = f(0x3d2e_f357_93c7_6730);

const A: [f64; 6] = [
    f(0xbfe5_5555_5555_5560),
    f(0x3fe0_0000_0000_0006),
    f(0x3fe9_9999_9959_554e),
    f(0xbfe5_5555_5529_a47a),
    f(0xbff2_495b_9b48_45e9),
    f(0x3ff0_002b_8b26_3fc3),
];

const INV_LN2_N: f64 = f(0x4077_1547_652b_82fe);
const SHIFT: f64 = f(0x4238_0000_0000_8000);

const LN2_HI_N: f64 = f(0x3f66_2e42_fefc_0000);
const LN2_LO_N: f64 = f(0x3d2c_610c_a86c_3899);

const C2: f64 = f(0x3fdf_ffff_ffff_fdbd);
const C3: f64 = f(0x3fc5_5555_5555_543c);
const C4: f64 = f(0x3fa5_5555_cf16_e1ed);
const C5: f64 = f(0x3f81_1111_67a4_b553);
const TWO_POW_52: f64 = f(0x4330_0000_0000_0000);
const TWO_POW_1009: f64 = f(0x7f00_0000_0000_0000);
const TWO_POW_M1022: f64 = f(0x0010_0000_0000_0000);

fn quiet(x: f64) -> f64 {
    f(x.to_bits() | QUIET)
}

fn nan_sum(x: f64, y: f64) -> f64 {
    if x.is_nan() {
        quiet(x)
    } else if y.is_nan() {
        quiet(y)
    } else {
        x + y
    }
}

fn signaling(x: f64) -> bool {
    (x.to_bits() ^ QUIET).wrapping_mul(2) > 0xfff0_0000_0000_0000
}

fn zero_inf_nan(bits: u64) -> bool {
    bits.wrapping_mul(2).wrapping_sub(1) >= INF2 - 1
}

fn integer_kind(iy: u64) -> u32 {
    let e = (iy >> 52 & 0x7ff) as u32;
    if e < 0x3ff {
        return 0;
    }
    if e > 0x3ff + 52 {
        return 2;
    }
    let unit = 1u64 << (0x3ff + 52 - e);
    if iy & (unit - 1) != 0 {
        0
    } else if iy & unit != 0 {
        1
    } else {
        2
    }
}

fn overflow(sign: u64) -> (f64, Errno) {
    (
        if sign == 0 {
            f64::INFINITY
        } else {
            f64::NEG_INFINITY
        },
        Errno::Range,
    )
}

fn underflow(sign: u64) -> (f64, Errno) {
    (if sign == 0 { 0.0 } else { -0.0 }, Errno::None)
}

pub fn pow(x: f64, y: f64) -> (f64, Errno) {
    let (mut ix, iy) = (x.to_bits(), y.to_bits());
    let mut top_x = (ix >> 52) as u32;
    let top_y = (iy >> 52) as u32;
    let mut sign_bias = 0;
    if top_x.wrapping_sub(1) >= 0x7fe || (top_y & 0x7ff).wrapping_sub(0x3be) >= 0x80 {
        if zero_inf_nan(iy) {
            let r = if iy << 1 == 0 {
                if signaling(x) {
                    nan_sum(x, y)
                } else {
                    1.0
                }
            } else if ix == ONE {
                if signaling(y) {
                    nan_sum(x, y)
                } else {
                    1.0
                }
            } else if ix << 1 > INF2 || iy << 1 > INF2 {
                nan_sum(x, y)
            } else if ix << 1 == ONE << 1 {
                1.0
            } else if (ix << 1 < ONE << 1) == (iy >> 63 == 0) {
                0.0
            } else {
                y * y
            };
            return (r, Errno::None);
        }
        if zero_inf_nan(ix) {
            let mut x2 = if x.is_nan() { quiet(x) } else { x * x };
            if ix >> 63 != 0 && integer_kind(iy) == 1 {
                x2 = -x2;
                sign_bias = 1;
            }
            if ix << 1 == 0 && iy >> 63 != 0 {
                return overflow(sign_bias);
            }
            let r = match iy >> 63 {
                0 => x2,
                _ if x2.is_nan() => x2,
                _ => 1.0 / x2,
            };
            return (r, Errno::None);
        }
        if ix >> 63 != 0 {
            match integer_kind(iy) {
                0 => return (f(INDEFINITE), Errno::Domain),
                1 => sign_bias = SIGN_BIAS,
                _ => {}
            }
            ix &= ABS;
            top_x &= 0x7ff;
        }
        if (top_y & 0x7ff).wrapping_sub(0x3be) >= 0x80 {
            if ix == ONE {
                return (1.0, Errno::None);
            }
            if top_y & 0x7ff < 0x3be {
                return (if ix > ONE { 1.0 + y } else { 1.0 - y }, Errno::None);
            }
            return if (ix > ONE) == (top_y < 0x800) {
                overflow(0)
            } else {
                underflow(0)
            };
        }
        if top_x == 0 {
            ix = ((x * TWO_POW_52).to_bits() & ABS).wrapping_sub(52 << 52);
        }
    }
    if iy == ONE {
        return (x, Errno::None);
    }
    if ix == ONE {
        return (if sign_bias == 0 { 1.0 } else { -1.0 }, Errno::None);
    }
    let (hi, lo) = log(ix);
    let ehi = y * hi;
    let elo = y.mul_add(hi, -ehi) + lo * y;
    exp(ehi, elo, sign_bias)
}

fn log(ix: u64) -> (f64, f64) {
    let tmp = ix.wrapping_sub(OFF);
    let i = (tmp >> 45 & 0x7f) as usize;
    let k = (tmp as i64 >> 52) as i32;
    let z = f(ix.wrapping_sub(tmp & 0xfff0_0000_0000_0000));
    let kd = f64::from(k);
    let [invc, logc, logctail] = tables::LOG[i].map(f);

    let r = z.mul_add(invc, -1.0);
    let t1 = kd * LN2_HI + logc;
    let t2 = t1 + r;
    let lo1 = kd * LN2_LO + logctail;
    let lo2 = t1 - t2 + r;
    let ar = -0.5 * r;
    let ar2 = r * ar;
    let ar3 = r * ar2;
    let hi = t2 + ar2;
    let lo3 = ar.mul_add(r, -ar2);
    let lo4 = t2 - hi + ar2;
    let p = ar3 * (A[0] + r * A[1] + ar2 * (A[2] + r * A[3] + ar2 * (A[4] + r * A[5])));
    let lo = lo1 + lo2 + lo3 + lo4 + p;
    let y = hi + lo;
    (y, hi - y + lo)
}

fn exp(x: f64, xtail: f64, sign_bias: u64) -> (f64, Errno) {
    let mut abstop = (x.to_bits() >> 52) as u32 & 0x7ff;
    if abstop.wrapping_sub(0x3c9) >= 0x408 - 0x3c9 {
        if abstop.wrapping_sub(0x3c9) >= 0x8000_0000 {
            let one = 1.0 + x;
            return (if sign_bias == 0 { one } else { -one }, Errno::None);
        }
        if abstop >= 0x409 {
            return if x.is_sign_negative() {
                underflow(sign_bias)
            } else {
                overflow(sign_bias)
            };
        }

        abstop = 0;
    }
    let kd = INV_LN2_N * x + SHIFT;
    let ki = kd.to_bits() >> 16;
    let kd = f64::from(ki as i32);
    let r = x - kd * LN2_HI_N + kd * LN2_LO_N + xtail;
    let idx = 2 * (ki & 0xff) as usize;
    let top = ki.wrapping_add(sign_bias) << 44;
    let tail = f(tables::EXP[idx]);
    let sbits = tables::EXP[idx + 1].wrapping_add(top);
    let r2 = r * r;
    let tmp = tail + r + r2 * (C2 + r * C3) + r2 * r2 * (C4 + r * C5);
    if abstop == 0 {
        return special(tmp, sbits, ki);
    }
    let scale = f(sbits);
    (scale + scale * tmp, Errno::None)
}

fn special(tmp: f64, sbits: u64, ki: u64) -> (f64, Errno) {
    if ki & 0x8000_0000 == 0 {
        let scale = f(sbits.wrapping_sub(1009 << 52));
        let y = (scale + scale * tmp) * TWO_POW_1009;
        return (
            y,
            if y.is_infinite() {
                Errno::Range
            } else {
                Errno::None
            },
        );
    }
    let sbits = sbits.wrapping_add(1022 << 52);
    let scale = f(sbits);
    let mut y = scale + scale * tmp;
    if y.abs() < 1.0 {
        let one = if y < 0.0 { -1.0 } else { 1.0 };
        let lo = scale - y + scale * tmp;
        let hi = one + y;
        let lo = one - hi + y + lo;
        y = (hi + lo) - one;
        if y == 0.0 {
            y = f(sbits & SIGN);
        }
    }
    let y = TWO_POW_M1022 * y;
    (y, if y == 0.0 { Errno::Range } else { Errno::None })
}
