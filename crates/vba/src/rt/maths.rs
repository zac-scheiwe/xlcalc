use super::util::{double, is_null, null_error, opt, R};
use super::RtContext;
use crate::VbaError;
use xlcalc::crt;
use xlcalc_variant::Variant;

fn trig_limit() -> f64 {
    f64::from_bits(0x43df_ffff_fdda_d230)
}

fn finite(x: f64) -> R {
    if x.is_finite() {
        Ok(Variant::R8(x))
    } else {
        Err(VbaError::new(6))
    }
}

pub(super) fn exp(ctx: &RtContext, a: &[Variant]) -> R {
    finite(crt::exp(double(ctx, &a[0])?))
}

pub(super) fn log(ctx: &RtContext, a: &[Variant]) -> R {
    let x = double(ctx, &a[0])?;
    if x <= 0.0 || x.is_nan() {
        return Err(VbaError::new(5));
    }
    Ok(Variant::R8(crt::log(x)))
}

pub(super) fn sqr(ctx: &RtContext, a: &[Variant]) -> R {
    let x = double(ctx, &a[0])?;
    if x < 0.0 || x.is_nan() {
        return Err(VbaError::new(5));
    }
    Ok(Variant::R8(crt::sqrt(x)))
}

pub(super) fn atn(ctx: &RtContext, a: &[Variant]) -> R {
    Ok(Variant::R8(crt::atan(double(ctx, &a[0])?)))
}

fn trig(ctx: &RtContext, v: &Variant) -> Result<f64, VbaError> {
    let x = double(ctx, v)?;
    if x.abs() >= trig_limit() || x.is_nan() {
        return Err(VbaError::new(5));
    }
    Ok(x)
}

pub(super) fn sin(ctx: &RtContext, a: &[Variant]) -> R {
    Ok(Variant::R8(crt::sin(trig(ctx, &a[0])?)))
}

pub(super) fn cos(ctx: &RtContext, a: &[Variant]) -> R {
    Ok(Variant::R8(crt::cos(trig(ctx, &a[0])?)))
}

pub(super) fn tan(ctx: &RtContext, a: &[Variant]) -> R {
    let x = trig(ctx, &a[0])?;
    if crt::cos(x) == 0.0 {
        return Err(VbaError::new(5));
    }
    Ok(Variant::R8(crt::tan(x)))
}

thread_local! {

    static RND: std::cell::Cell<u32> = const { std::cell::Cell::new(0x0005_0000) };
}

pub(super) fn rnd(ctx: &RtContext, a: &[Variant]) -> R {
    let x = match opt(a, 0) {
        Some(v) => {
            if is_null(v) {
                return Err(null_error());
            }
            double(ctx, v)? as f32
        }
        None => 1.0,
    };
    let state = RND.with(|c| {
        let mut s = c.get();
        if x < 0.0 {
            let b = x.to_bits();
            s = ((b & 0x00FF_FFFF) + (b >> 24)) & 0x00FF_FFFF;
        }
        if x != 0.0 {
            s = s.wrapping_mul(0x43FD_43FD).wrapping_add(0x00C3_9EC3) & 0x00FF_FFFF;
        }
        c.set(s);
        s
    });
    Ok(Variant::R4(state as f32 / 16_777_216.0))
}

pub(super) fn randomize(ctx: &RtContext, a: &[Variant]) -> R {
    let seed = match opt(a, 0) {
        Some(v) => double(ctx, v)?,
        None => (ctx.clock)(),
    };
    RND.with(|c| {
        let hi = (seed.to_bits() >> 32) as u32;
        let upper = (hi >> 16) ^ (hi & 0xFFFF);
        c.set((upper << 8) | (c.get() & 0xFF));
    });
    Ok(Variant::Empty)
}
