use super::{DIV_ZERO, INVALID, OVERFLOW};
use crate::{Decimal, HResult};

#[derive(Clone, Copy, PartialEq, Eq, Debug)]
struct U256([u64; 4]);

impl U256 {
    fn from_u128(x: u128) -> U256 {
        U256([x as u64, (x >> 64) as u64, 0, 0])
    }

    fn mul_small(self, m: u64) -> U256 {
        let mut out = [0u64; 4];
        let mut carry = 0u128;
        for (o, l) in out.iter_mut().zip(self.0) {
            let t = u128::from(l) * u128::from(m) + carry;
            *o = t as u64;
            carry = t >> 64;
        }
        U256(out)
    }

    fn mul(a: u128, b: u128) -> U256 {
        let (a0, a1) = (a as u64, (a >> 64) as u64);
        let p = U256::from_u128(b);
        let lo = p.mul_small(a0);
        let hi = p.mul_small(a1);
        lo.add(hi.shl64())
    }

    fn shl64(self) -> U256 {
        U256([0, self.0[0], self.0[1], self.0[2]])
    }

    fn add(self, o: U256) -> U256 {
        let mut out = [0u64; 4];
        let mut carry = 0u64;
        for ((slot, a), b) in out.iter_mut().zip(self.0).zip(o.0) {
            let (s1, c1) = a.overflowing_add(b);
            let (s2, c2) = s1.overflowing_add(carry);
            *slot = s2;
            carry = u64::from(c1) + u64::from(c2);
        }
        U256(out)
    }

    fn sub(self, o: U256) -> U256 {
        let mut out = [0u64; 4];
        let mut borrow = 0u64;
        for ((slot, a), b) in out.iter_mut().zip(self.0).zip(o.0) {
            let (s1, b1) = a.overflowing_sub(b);
            let (s2, b2) = s1.overflowing_sub(borrow);
            *slot = s2;
            borrow = u64::from(b1) + u64::from(b2);
        }
        U256(out)
    }

    fn cmp(&self, o: &U256) -> std::cmp::Ordering {
        for i in (0..4).rev() {
            if self.0[i] != o.0[i] {
                return self.0[i].cmp(&o.0[i]);
            }
        }
        std::cmp::Ordering::Equal
    }

    fn divrem_small(self, d: u64) -> (U256, u64) {
        let mut out = [0u64; 4];
        let mut rem = 0u128;
        for i in (0..4).rev() {
            let cur = rem << 64 | u128::from(self.0[i]);
            out[i] = (cur / u128::from(d)) as u64;
            rem = cur % u128::from(d);
        }
        (U256(out), rem as u64)
    }

    fn bit(&self, i: usize) -> bool {
        self.0[i / 64] >> (i % 64) & 1 == 1
    }

    fn fits_96(&self) -> bool {
        self.0[3] == 0 && self.0[2] == 0 && self.0[1] >> 32 == 0
    }

    fn to_u128(self) -> u128 {
        u128::from(self.0[0]) | u128::from(self.0[1]) << 64
    }

    fn divrem(self, d: u128) -> (U256, u128) {
        let mut q = [0u64; 4];
        let mut r = 0u128;
        for i in (0..256).rev() {
            r = r << 1 | u128::from(self.bit(i));
            if r >= d {
                r -= d;
                q[i / 64] |= 1 << (i % 64);
            }
        }
        (U256(q), r)
    }
}

fn pow10(n: u32) -> U256 {
    let mut p = U256::from_u128(1);
    for _ in 0..n {
        p = p.mul_small(10);
    }
    p
}

fn check(d: &Decimal) -> Result<(), HResult> {
    if d.scale > 28 {
        Err(INVALID)
    } else {
        Ok(())
    }
}

fn shrink(m: U256, k: u32) -> U256 {
    if k == 0 {
        return m;
    }
    let (q, r) = m.divrem_small_pow(k);
    let half = pow10(k).divrem_small(2).0;
    match r.cmp(&half) {
        std::cmp::Ordering::Greater => q.add(U256::from_u128(1)),
        std::cmp::Ordering::Equal if q.0[0] & 1 == 1 => q.add(U256::from_u128(1)),
        _ => q,
    }
}

impl U256 {
    fn divrem_small_pow(self, k: u32) -> (U256, U256) {
        let mut q = self;
        let mut done = 0;
        while done < k {
            let step = (k - done).min(19);
            q = q.divrem_small(10u64.pow(step)).0;
            done += step;
        }
        let back = q.mul_pow(k);
        (q, self.sub(back))
    }

    fn mul_pow(self, k: u32) -> U256 {
        let mut p = self;
        let mut done = 0;
        while done < k {
            let step = (k - done).min(19);
            p = p.mul_small(10u64.pow(step));
            done += step;
        }
        p
    }
}

fn finish(negative: bool, m: U256, scale: u32) -> Result<Decimal, HResult> {
    let mut k = scale.saturating_sub(28);

    let mut t = shrink(m, k);
    while !t.fits_96() {
        k += 1;
        t = shrink(m, k);
    }
    if k > scale {
        return Err(OVERFLOW);
    }
    Ok(Decimal {
        negative,
        scale: (scale - k) as u8,
        mantissa: t.to_u128(),
    })
}

pub(crate) fn add(a: Decimal, b: Decimal) -> Result<Decimal, HResult> {
    check(&a)?;
    check(&b)?;
    let scale = u32::from(a.scale.max(b.scale));
    let ma = U256::from_u128(a.mantissa).mul_pow(scale - u32::from(a.scale));
    let mb = U256::from_u128(b.mantissa).mul_pow(scale - u32::from(b.scale));
    let (negative, m) = if a.negative == b.negative {
        (a.negative, ma.add(mb))
    } else if ma.cmp(&mb) == std::cmp::Ordering::Less {
        (b.negative, mb.sub(ma))
    } else {
        (a.negative, ma.sub(mb))
    };
    let zero = m == U256([0; 4]);

    let negative = if zero {
        if b.scale < a.scale {
            b.negative
        } else {
            a.negative
        }
    } else {
        negative
    };
    finish(negative, m, scale)
}

pub(crate) fn sub(a: Decimal, b: Decimal) -> Result<Decimal, HResult> {
    add(
        a,
        Decimal {
            negative: !b.negative,
            ..b
        },
    )
}

pub(crate) fn mul(a: Decimal, b: Decimal) -> Result<Decimal, HResult> {
    check(&a)?;
    check(&b)?;
    let m = U256::mul(a.mantissa, b.mantissa);
    let negative = a.negative != b.negative;
    if m == U256([0; 4]) {
        let wide = a.mantissa.max(b.mantissa) >> 32 != 0;
        return Ok(if wide {
            Decimal::default()
        } else {
            Decimal {
                negative,
                scale: (a.scale + b.scale).min(28),
                mantissa: 0,
            }
        });
    }
    let scale = u32::from(a.scale) + u32::from(b.scale);
    let d = finish(negative, m, scale)?;
    Ok(if d.mantissa == 0 {
        Decimal {
            negative,
            scale: if scale >= 48 && m.0[1..] == [0; 3] && m.0[0] >> 32 == 0 {
                0
            } else {
                28
            },
            mantissa: 0,
        }
    } else {
        d
    })
}

pub(crate) fn div(a: Decimal, b: Decimal) -> Result<Decimal, HResult> {
    check(&a)?;
    check(&b)?;
    if b.mantissa == 0 {
        return Err(DIV_ZERO);
    }
    let negative = a.negative != b.negative;
    if a.mantissa == 0 {
        return Ok(Decimal {
            negative,
            scale: 0,
            mantissa: 0,
        });
    }
    let mut scale = i32::from(a.scale) - i32::from(b.scale);
    let (mut q, mut rem) = U256::from_u128(a.mantissa).divrem(b.mantissa);

    let limit = U256::from_u128(7922816251426433759354395032);
    while (rem != 0 || scale < 0) && scale < 28 {
        if q.cmp(&limit) == std::cmp::Ordering::Greater {
            if scale < 0 {
                return Err(OVERFLOW);
            }
            break;
        }
        let (d, r) = U256::from_u128(rem).mul_small(10).divrem(b.mantissa);
        q = q.mul_small(10).add(d);
        rem = r;
        scale += 1;
    }
    let twice = rem * 2;
    if twice > b.mantissa || (twice == b.mantissa && q.0[0] & 1 == 1) {
        q = q.add(U256::from_u128(1));
    }
    if q == U256([0; 4]) {
        return Ok(Decimal {
            negative,
            ..Decimal::default()
        });
    }
    let mut scale = scale as u32;
    let mut m = q;
    while scale > 0 && m.divrem_small(10).1 == 0 {
        m = m.divrem_small(10).0;
        scale -= 1;
    }
    if !m.fits_96() {
        return Err(OVERFLOW);
    }
    Ok(Decimal {
        negative,
        scale: scale as u8,
        mantissa: m.to_u128(),
    })
}
