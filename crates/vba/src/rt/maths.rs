use super::util::{double, R};
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
