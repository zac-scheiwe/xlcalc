use std::f64::consts::{FRAC_PI_2, PI};

pub fn atan(x: f64) -> f64 {
    x.atan2(1.0)
}

pub fn atan2(y: f64, x: f64) -> f64 {
    use crate::x87;

    if x > 0.0 {
        x87::add(x87::atan(x87::div(y, x)), 0.0)
    } else if x < 0.0 {
        x87::add(x87::atan(x87::div(y, x)), if y >= 0.0 { PI } else { -PI })
    } else if y > 0.0 {
        FRAC_PI_2
    } else {
        -FRAC_PI_2
    }
}

pub fn asin(x: f64) -> f64 {
    use crate::x87;
    if x.abs() == 1.0 {
        return x * FRAC_PI_2;
    }
    let t = x87::sub(1.0, x);
    let u = x87::sub(2.0, t);
    x87::atan(x87::div(x, x87::sqrt(x87::mul(u, t))))
}

pub fn acos(x: f64) -> f64 {
    if x == 1.0 {
        return 0.0;
    }
    if x == -1.0 {
        return PI;
    }
    crate::x87::sub(FRAC_PI_2, asin(x))
}

pub fn sinh(x: f64) -> f64 {
    use crate::x87;
    let d = if x.abs() < 1.0 {
        x87::sub(x87::expm1(x), x87::expm1(-x))
    } else {
        x87::sub(x87::exp(x), x87::exp(-x))
    };
    x87::div(d, 2.0)
}

pub fn cosh(x: f64) -> f64 {
    use crate::x87;
    x87::div(x87::add(x87::exp(-x), x87::exp(x)), 2.0)
}

pub fn tanh(x: f64) -> f64 {
    use crate::x87;
    if x.abs() > 354.0 {
        return x.signum();
    }
    if x.abs() >= 1.0 {
        let (p, m) = (x87::exp(x), x87::exp(-x));
        return x87::div(x87::sub(p, m), x87::add(p, m));
    }
    let (p, m) = (x87::expm1(x), x87::expm1(-x));
    x87::div(x87::sub(p, m), x87::add(2.0, x87::add(p, m)))
}

pub fn asinh(x: f64) -> f64 {
    use crate::x87;
    let a = x.abs();
    let r = x87::ln(x87::add(x87::sqrt(x87::add(x87::mul(a, a), 1.0)), a));
    if x < 0.0 {
        -r
    } else {
        r
    }
}

pub fn acosh(x: f64) -> f64 {
    use crate::x87;
    x87::ln(x87::add(x87::sqrt(x87::sub(x87::mul(x, x), 1.0)), x))
}

pub fn atanh(x: f64) -> f64 {
    use crate::x87;
    if x.abs().to_bits() < 0x3F1A_F82B_729C_1D83 {
        let t = x87::mul(x, 1.0 / 3.0);
        let t = x87::mul(x, t);
        let t = x87::mul(x, t);
        return x87::add(x, t);
    }
    let p = x87::sub(1.0, x);
    x87::mul(x87::ln(x87::div(x87::add(x, 1.0), p)), 0.5)
}

fn powi_squaring(x: f64, n: f64) -> f64 {
    let neg = n < 0.0;
    let mut n = n.abs();
    let mut r = 1.0f64;
    let mut p = x;
    while n >= 1.0 {
        if n % 2.0 == 1.0 {
            r = crate::x87::mul(r, p);
        }
        n = (n / 2.0).floor();
        if n >= 1.0 {
            p = crate::x87::mul(p, p);
        }
    }
    if neg {
        crate::x87::div(1.0, r)
    } else {
        r
    }
}

pub fn pow(x: f64, y: f64) -> f64 {
    if y.fract() == 0.0 && y.abs() < 9.007_199_254_740_992e15 {
        if x == 10.0 && y.abs() <= 400.0 {
            return format!("1e{}", y as i32).parse().unwrap_or(f64::INFINITY);
        }
        return powi_squaring(x, y);
    }

    use crate::x87;
    let p = if y.abs() == 0.5 {
        x87::sqrt(x)
    } else {
        x87::exp(x87::mul(x87::ln(x), y.abs()))
    };
    if y > 0.0 {
        p
    } else {
        x87::div(1.0, p)
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn matches_excel_samples() {
        assert_eq!(acos(0.5), 1.0471975511965976);
        assert_eq!(tanh(0.5), 0.4621171572600098);
        assert_eq!(asinh(1.0), 0.8813735870195429);
        assert_eq!(pow(8.0, 1.0 / 3.0), 1.9999999999999998);
        assert_eq!(pow(2.0, 10.0), 1024.0);
        assert_eq!(pow(10.0, -5.0), 1e-5);
        assert_eq!(pow(2.0, 0.5), std::f64::consts::SQRT_2);
        assert_eq!(pow(1.1, 10.0).to_bits(), 0x4004BFFC0C03023D);
        assert_eq!(pow(10.0, 308.0).to_bits(), 0x7FE1CCF385EBC8A0);
    }
}
