use super::atp::{self, R};
use super::xlmath;
use crate::coerce::parse_number_text;
use crate::eval::{EvalCtx, Operand};
use crate::functions::args::val;
use crate::functions::statdist::dd::{self, DD};
use crate::value::{Value, XlError};

#[derive(Clone, Copy, Debug, PartialEq)]
pub struct C {
    pub re: f64,
    pub im: f64,
}

impl C {
    pub const fn new(re: f64, im: f64) -> C {
        C { re, im }
    }

    fn is_zero(self) -> bool {
        self.re == 0.0 && self.im == 0.0
    }

    fn finite(self) -> bool {
        self.re.is_finite() && self.im.is_finite()
    }

    pub fn mul(self, o: C) -> C {
        C::new(
            self.re * o.re - self.im * o.im,
            self.re * o.im + self.im * o.re,
        )
    }

    pub fn div(self, o: C) -> C {
        let (a, b, c, d) = (self.re, self.im, o.re, o.im);
        if c.abs() >= d.abs() {
            let r = d / c;
            let den = c + d * r;
            C::new((a + b * r) / den, (b - a * r) / den)
        } else {
            let r = c / d;
            let den = c * r + d;
            C::new((a * r + b) / den, (b * r - a) / den)
        }
    }

    pub fn abs(self) -> f64 {
        let (a, b) = (self.re.abs(), self.im.abs());
        let (big, small) = if a >= b { (a, b) } else { (b, a) };
        if big == 0.0 {
            return 0.0;
        }
        let r = small / big;
        big * (1.0 + r * r).sqrt()
    }

    pub fn modulus(self) -> f64 {
        let sq = |v: f64| crate::number::flush_subnormal(v * v);
        crate::number::flush_subnormal(sq(self.re) + sq(self.im)).sqrt()
    }

    pub fn arg(self) -> f64 {
        xlmath::atan2(self.im, self.re)
    }
}

pub type Suffix = Option<char>;

fn digits15(x: f64) -> (String, i32) {
    let s = format!("{:.14e}", x.abs());
    let (mant, exp) = s.split_once('e').unwrap();
    let exp: i32 = exp.parse().unwrap();
    let mut digits: String = mant.chars().filter(|c| c.is_ascii_digit()).collect();
    while digits.len() > 1 && digits.ends_with('0') {
        digits.pop();
    }
    (digits, exp)
}

pub fn fmt_part(x: f64) -> String {
    if x == 0.0 {
        return "0".into();
    }
    let (d, exp) = digits15(x);
    let n = d.len() as i32;
    let fixed = if exp >= 0 {
        if exp + 1 >= n {
            format!("{}{}", d, "0".repeat((exp + 1 - n) as usize))
        } else {
            format!("{}.{}", &d[..(exp + 1) as usize], &d[(exp + 1) as usize..])
        }
    } else {
        format!("0.{}{}", "0".repeat((-exp - 1) as usize), d)
    };
    let body = if fixed.len() <= 21 {
        fixed
    } else {
        let mut s = d[..1].to_string();
        if d.len() > 1 {
            s.push('.');
            s.push_str(&d[1..]);
        }
        format!("{}E{}{:02}", s, if exp < 0 { '-' } else { '+' }, exp.abs())
    };
    if x < 0.0 {
        format!("-{body}")
    } else {
        body
    }
}

pub fn format(z: C, suffix: Suffix) -> R<String> {
    if !z.finite() {
        return Err(XlError::Num);
    }
    let sfx = suffix.unwrap_or('i');
    let re = crate::number::normalize(z.re);
    let im = crate::number::normalize(z.im);
    let imag = |v: f64| -> String {
        let a = if v.abs() == 1.0 {
            String::new()
        } else {
            fmt_part(v.abs())
        };
        format!("{}{}{}", if v < 0.0 { "-" } else { "" }, a, sfx)
    };
    Ok(if im == 0.0 {
        fmt_part(re)
    } else if re == 0.0 {
        imag(im)
    } else {
        let i = imag(im);
        if im > 0.0 {
            format!("{}+{}", fmt_part(re), i)
        } else {
            format!("{}{}", fmt_part(re), i)
        }
    })
}

fn plain(ctx: &EvalCtx, s: &str) -> Option<f64> {
    use crate::coerce::NumberKind;
    let p = parse_number_text(s, ctx.parse_env())?;
    if matches!(p.kind, NumberKind::Date | NumberKind::DateTime)
        && (s.contains('-') || (!s.contains('/') && !s.chars().any(|c| c.is_alphabetic())))
    {
        return None;
    }
    Some(p.value)
}

pub fn parse_text(ctx: &EvalCtx, s: &str) -> Option<(C, Suffix)> {
    if let Some(v) = plain(ctx, s) {
        return Some((C::new(v, 0.0), None));
    }
    let t = s.trim_matches(' ');
    let sfx = t.chars().last()?;
    if sfx != 'i' && sfx != 'j' {
        return None;
    }
    let body = &t[..t.len() - 1];
    let b = body.as_bytes();
    let split =
        (1..b.len()).find(|&i| (b[i] == b'+' || b[i] == b'-') && !matches!(b[i - 1], b'e' | b'E'));
    let coef = |s: &str| -> Option<f64> {
        let s = s.trim_matches(' ');
        match s {
            "" | "+" => Some(1.0),
            "-" => Some(-1.0),
            _ => plain(ctx, s),
        }
    };
    let z = match split {
        None => C::new(0.0, coef(body)?),
        Some(i) => {
            let re = plain(ctx, body[..i].trim_matches(' '))?;
            let rest = body[i + 1..].trim_matches(' ');
            let c = if rest.is_empty() {
                1.0
            } else {
                plain(ctx, rest)?
            };
            C::new(re, if b[i] == b'-' { -c } else { c })
        }
    };
    Some((z, Some(sfx)))
}

pub fn parse(ctx: &EvalCtx, v: &Value) -> R<(C, Suffix)> {
    match v {
        Value::Number(n) => Ok((C::new(*n, 0.0), None)),
        Value::Blank => Ok((C::new(0.0, 0.0), None)),
        Value::Text(s) => parse_text(ctx, s).ok_or(XlError::Num),
        Value::Error(e) => Err(*e),
        _ => Err(XlError::Value),
    }
}

fn req(ctx: &EvalCtx, a: &[Option<Value>], i: usize) -> R<(C, Suffix)> {
    match a.get(i) {
        Some(Some(v)) => parse(ctx, v),
        _ => Err(XlError::NA),
    }
}

fn join(a: Suffix, b: Suffix, mixed: XlError) -> R<Suffix> {
    match (a, b) {
        (Some(x), Some(y)) if x != y => Err(mixed),
        (Some(x), _) | (_, Some(x)) => Ok(Some(x)),
        _ => Ok(None),
    }
}

pub fn complex_fn(ctx: &mut EvalCtx, ops: &[Operand]) -> Operand {
    atp::call(ctx, ops, "a", |ctx, a, _| {
        val((|| {
            let re = atp::req_num(ctx, a, 0);
            let im = atp::req_num(ctx, a, 1);
            let sfx = (|| {
                Ok(match a.get(2) {
                    Some(Some(Value::Text(s))) => match &**s {
                        "" | "i" => 'i',
                        "j" => 'j',
                        _ => return Err(XlError::Value),
                    },
                    Some(Some(Value::Blank)) | Some(None) | None => 'i',
                    Some(Some(Value::Error(e))) => return Err(*e),
                    Some(Some(_)) => return Err(XlError::Value),
                })
            })();

            let sfx: char = sfx?;
            let (re, im) = (re?, im?);
            Ok(Value::from(format(C::new(re, im), Some(sfx))?))
        })())
    })
}

pub fn to_number(ctx: &mut EvalCtx, ops: &[Operand], f: fn(C) -> R<f64>) -> Operand {
    atp::call(ctx, ops, "a", |ctx, a, _| {
        val(req(ctx, a, 0).and_then(|(z, _)| f(z)).map(Value::num))
    })
}

pub fn unary(ctx: &mut EvalCtx, ops: &[Operand], f: fn(C) -> R<C>) -> Operand {
    atp::call(ctx, ops, "a", |ctx, a, _| {
        val((|| {
            let (z, sfx) = req(ctx, a, 0)?;
            Ok(Value::from(format(f(z)?, sfx)?))
        })())
    })
}

pub fn imargument(z: C) -> R<f64> {
    if z.is_zero() {
        return Err(XlError::Div0);
    }
    Ok(z.arg())
}

pub fn imsub(ctx: &mut EvalCtx, ops: &[Operand]) -> Operand {
    atp::call(ctx, ops, "a", |ctx, a, _| {
        val((|| {
            let (x, sx) = req(ctx, a, 0)?;
            let (y, sy) = req(ctx, a, 1)?;
            let s = join(sx, sy, XlError::Num)?;

            let fin = |v: f64| if v.is_finite() { v } else { 0.0 };
            Ok(Value::from(format(
                C::new(fin(x.re - y.re), fin(x.im - y.im)),
                s,
            )?))
        })())
    })
}

pub fn imdiv(ctx: &mut EvalCtx, ops: &[Operand]) -> Operand {
    atp::call(ctx, ops, "a", |ctx, a, _| {
        val((|| {
            let (x, sx) = req(ctx, a, 0)?;
            let (y, sy) = req(ctx, a, 1)?;
            let s = join(sx, sy, XlError::Num)?;
            if y.is_zero() {
                return Err(XlError::Num);
            }
            Ok(Value::from(format(x.div(y), s)?))
        })())
    })
}

pub fn impower(ctx: &mut EvalCtx, ops: &[Operand]) -> Operand {
    atp::call(ctx, ops, "a", |ctx, a, _| {
        val((|| {
            let (z, sfx) = req(ctx, a, 0)?;
            let n = atp::req_num(ctx, a, 1)?;
            Ok(Value::from(format(power(z, n)?, sfx)?))
        })())
    })
}

pub fn power(z: C, n: f64) -> R<C> {
    if z.is_zero() {
        return if n > 0.0 {
            Ok(C::new(0.0, 0.0))
        } else {
            Err(XlError::Num)
        };
    }
    let m = if z.im == 0.0 { z.re.abs() } else { z.modulus() };

    let r = if n < 0.0 && n.fract() != 0.0 {
        1.0 / xlmath::pow(m, -n)
    } else {
        xlmath::pow(m, n)
    };
    let t = n * z.arg();
    Ok(C::new(r * trig_cos(t)?, r * trig_sin(t)?))
}

pub fn fold(ctx: &mut EvalCtx, ops: &[Operand], product: bool) -> Operand {
    if ops.first().map_or(true, |o| o.is_missing()) {
        return Operand::err(XlError::NA);
    }
    let r: R<Value> = (|| {
        let mut acc: Option<C> = None;
        let mut sfx: Suffix = None;
        for op in ops {
            atp::visit_list(ctx, op, |ctx, v| {
                let (z, s) = parse(ctx, v)?;
                sfx = join(sfx, s, XlError::Value)?;
                acc = Some(match acc {
                    None => z,
                    Some(a) if product => a.mul(z),
                    Some(a) => C::new(a.re + z.re, a.im + z.im),
                });
                Ok(())
            })?;
        }
        let z = acc.unwrap_or(C::new(0.0, 0.0));
        if !z.finite() {
            return Err(XlError::Value);
        }
        Ok(Value::from(format(z, sfx)?))
    })();
    Operand::Val(val(r))
}

fn trig_sin(x: f64) -> R<f64> {
    if x.abs() >= xlmath::TRIG_LIMIT {
        return Err(XlError::Num);
    }
    Ok(xlmath::sin(x))
}

fn trig_cos(x: f64) -> R<f64> {
    if x.abs() >= xlmath::TRIG_LIMIT {
        return Err(XlError::Num);
    }
    Ok(xlmath::cos(x))
}

fn x87_sin(x: f64) -> R<f64> {
    trig_sin(x).map(|_| crate::x87::sin(x))
}

fn x87_cos(x: f64) -> R<f64> {
    trig_cos(x).map(|_| crate::x87::cos(x))
}

fn scale(m: f64, f: f64) -> f64 {
    if m == 0.0 {
        0.0
    } else {
        m * f
    }
}

fn dk_exp(x: f64, y: f64, eoff: i32) -> f64 {
    let g0 = x * 1.4426950408889634;
    let r = if g0 < 0.0 { g0 - 0.5 } else { g0 + 0.5 };
    let mut xe = (r as i32) as i16 as i32;
    let xd = xe as f64;
    let g = (x - xd * 0.693145751953125) - xd * 1.4286068203094172555e-06;
    if g > -5.5511151231257827e-17 && g < 5.5511151231257827e-17 {
        return scale2(y, xe + eoff);
    }
    xe -= 1;
    let z = g * g;
    let p = ((z + 420.30235984910632) * z + 15132.700946804747) * g;
    let q = (z * 30.015112906833171 + 3362.7215441655303) * z + 30265.401893609498;
    scale2(((p + q) / (q - p)) * 2.0 * y, xe + eoff)
}

fn scale2(v: f64, e: i32) -> f64 {
    let a = e / 2;
    v * 2f64.powi(a) * 2f64.powi(e - a)
}

fn dk_cosh(x: f64) -> f64 {
    let a = x.abs();
    if !(a < 710.0) {
        return x.cosh();
    }
    let w = dk_exp(a, 1.0, -1);
    w + 0.25 / w
}

pub fn imsqrt(z: C) -> R<C> {
    if z.is_zero() {
        return Ok(z);
    }
    let r = z.modulus().sqrt();
    let t = z.arg() / 2.0;
    Ok(C::new(r * trig_cos(t)?, r * trig_sin(t)?))
}

pub fn imexp(z: C) -> R<C> {
    let e = z.re.exp();
    Ok(C::new(scale(trig_cos(z.im)?, e), scale(trig_sin(z.im)?, e)))
}

pub fn imln(z: C) -> R<C> {
    if z.is_zero() {
        return Err(XlError::Num);
    }
    let m = if z.im == 0.0 { z.re.abs() } else { z.modulus() };
    Ok(C::new(m.ln(), z.arg()))
}

pub fn imlog10(z: C) -> R<C> {
    let l = imln(z)?;
    let k = std::f64::consts::LN_10;
    Ok(C::new(l.re / k, l.im / k))
}

pub fn imlog2(z: C) -> R<C> {
    let l = imln(z)?;

    let k = std::f64::consts::LN_2;
    Ok(C::new(l.re * k / (k * k), l.im * k / (k * k)))
}

fn sin_cos_e(z: C) -> R<(f64, f64, f64, f64)> {
    let (s, c) = (x87_sin(z.re)?, x87_cos(z.re)?);
    let e = crate::x87::exp(z.im);
    let ie = crate::x87::div(1.0, e);
    Ok((s, c, crate::x87::add(ie, e), crate::x87::sub(e, ie)))
}

fn half(t: f64, h: f64) -> f64 {
    if t == 0.0 {
        0.0
    } else {
        crate::x87::mul(crate::x87::mul(h, t), 0.5)
    }
}

pub fn imsin(z: C) -> R<C> {
    let (s, c, ch2, sh2) = sin_cos_e(z)?;
    Ok(C::new(half(s, ch2), half(c, sh2)))
}

pub fn imcos(z: C) -> R<C> {
    let (s, c, ch2, sh2) = sin_cos_e(z)?;
    Ok(C::new(half(c, ch2), -half(s, sh2)))
}

fn sinh_series(x: f64) -> f64 {
    let x3 = x * x * x;
    x + x3 / 6.0 + x3 * x * x / 120.0
}

fn sinh_n(x: f64) -> f64 {
    if x.abs() <= 0.01 {
        return sinh_series(x);
    }
    let e = (-x).exp();
    (1.0 / e - e) / 2.0
}

fn cosh_n(x: f64) -> f64 {
    let e = (-x).exp();
    (1.0 / e + e) / 2.0
}

pub fn imsinh(z: C) -> R<C> {
    let (s, c) = (trig_sin(z.im)?, trig_cos(z.im)?);
    Ok(C::new(scale(c, sinh_n(z.re)), scale(s, cosh_n(z.re))))
}

pub fn imcosh(z: C) -> R<C> {
    let (s, c) = (trig_sin(z.im)?, trig_cos(z.im)?);
    Ok(C::new(scale(c, cosh_n(z.re)), scale(s, sinh_n(z.re))))
}

fn sin_acc(z: C) -> R<C> {
    let (s, c) = (trig_sin(z.re)?, trig_cos(z.re)?);
    Ok(C::new(scale(s, dk_cosh(z.im)), scale(c, z.im.sinh())))
}

fn cos_acc(z: C) -> R<C> {
    let (s, c) = (trig_sin(z.re)?, trig_cos(z.re)?);
    Ok(C::new(scale(c, dk_cosh(z.im)), -scale(s, z.im.sinh())))
}

fn tan_cot(z: C, cot: bool) -> Option<R<C>> {
    if z.im == 0.0 {
        let t = xlmath::tan(z.re);
        return Some(Ok(C::new(if cot { 1.0 / t } else { t }, 0.0)));
    }

    use crate::x87::{add, div, mul, sub};
    let (s, c) = (x87_sin(z.re).ok()?, x87_cos(z.re).ok()?);
    let b = mul(z.im, 2.0);
    let e = crate::x87::exp(b);
    let ie = div(1.0, e);
    let ch2 = add(e, ie);
    let sh2 = if b.abs() <= 0.01 {
        2.0 * sinh_series(b)
    } else {
        sub(e, ie)
    };
    let (t, u) = if cot { (c, s) } else { (s, c) };
    let t4 = mul(4.0, t);
    let d = sub(add(ch2, 2.0), mul(t4, t));
    let im = div(sh2, d);
    let w = C::new(div(mul(t4, u), d), if cot { -im } else { im });
    w.finite().then_some(Ok(w))
}

pub fn imtan(z: C) -> R<C> {
    if z.re.abs() < xlmath::TRIG_LIMIT {
        if let Some(w) = tan_cot(z, false) {
            return w;
        }
    }
    let (a, b) = (2.0 * z.re, 2.0 * z.im);
    if b.abs() > 700.0 {
        return Ok(C::new(
            2.0 * trig_sin(a)? * (-b.abs()).exp(),
            1f64.copysign(b),
        ));
    }
    let d = trig_cos(a)? + b.cosh();
    Ok(C::new(trig_sin(a)? / d, b.sinh() / d))
}

fn recip(z: C) -> R<C> {
    if z.re.is_infinite() || z.im.is_infinite() {
        return Ok(C::new(0.0, 0.0));
    }
    if z.is_zero() {
        return Err(XlError::Num);
    }
    Ok(C::new(1.0, 0.0).div(z))
}

fn sinh_dd(x: f64) -> DD {
    let a = x.abs();
    let sh = if a < 1e-5 {
        let a2 = DD::new(a).mul_f(a);
        let a3 = a2.mul_f(a);
        DD::new(a)
            .add(a3.div(DD::new(6.0)))
            .add(a3.mul(a2).div(DD::new(120.0)))
    } else {
        let h = DD::new(a).sub(dd::LN2).exp();
        h.sub(DD::new(0.25).div(h))
    };
    if x < 0.0 {
        sh.neg()
    } else {
        sh
    }
}

fn scale_dd(v: DD, e: i32) -> DD {
    let (s1, s2) = (2f64.powi(e / 2), 2f64.powi(e - e / 2));
    DD(v.0 * s1 * s2, v.1 * s1 * s2)
}

fn recip_ext(t1: f64, h1: DD, t2: f64, h2: DD, fallback: C) -> R<C> {
    if !fallback.finite() || fallback.is_zero() {
        return recip(fallback);
    }
    let (c, d) = (h1.mul_f(t1), h2.mul_f(t2));
    let m = c.0.abs().max(d.0.abs());
    if m == 0.0 || !m.is_finite() {
        return recip(fallback);
    }
    let k = m.log2().floor() as i32;
    let (c, d) = (scale_dd(c, -k), scale_dd(d, -k));
    let den = c.mul(c).add(d.mul(d));
    let re = scale_dd(c.div(den), -k).hi();
    let im = scale_dd(d.div(den).neg(), -k).hi();
    let flush = crate::number::flush_subnormal;
    Ok(C::new(flush(re) + 0.0, flush(im) + 0.0))
}

fn sec_csc(z: C, csc: bool) -> Option<C> {
    use crate::x87::{add, div, mul, sub};
    let (s, c) = (x87_sin(z.re).ok()?, x87_cos(z.re).ok()?);
    let e = crate::x87::exp(z.im);
    let ie = div(1.0, e);
    let ch = add(e, ie);
    let sh = if z.im.abs() <= 0.01 {
        2.0 * sinh_series(z.im)
    } else {
        sub(e, ie)
    };
    let e2 = crate::x87::exp(mul(z.im, 2.0));
    let ch2 = add(e2, div(1.0, e2));
    let (s2, c2) = (mul(2.0, s), mul(2.0, c));
    let t2 = if csc { s2 } else { c2 };
    let d = add(mul(t2, t2), sub(ch2, 2.0));
    let w = if csc {
        C::new(mul(div(ch, d), s2), -mul(div(sh, d), c2))
    } else {
        C::new(mul(div(ch, d), c2), mul(div(sh, d), s2))
    };
    (w.finite() && d != 0.0).then_some(w)
}

pub fn imsec(z: C) -> R<C> {
    if let Some(w) = sec_csc(z, false) {
        return Ok(w);
    }
    let (s, c) = (trig_sin(z.re)?, trig_cos(z.re)?);
    let (ch, sh) = (DD::new(dk_cosh(z.im)), sinh_dd(z.im));
    recip_ext(c, ch, -s, sh, cos_acc(z)?)
}

pub fn imcsc(z: C) -> R<C> {
    if let Some(w) = sec_csc(z, true) {
        return Ok(w);
    }
    let (s, c) = (trig_sin(z.re)?, trig_cos(z.re)?);
    let (ch, sh) = (DD::new(dk_cosh(z.im)), sinh_dd(z.im));
    recip_ext(s, ch, c, sh, sin_acc(z)?)
}

pub fn imcot(z: C) -> R<C> {
    if z.re.abs() < xlmath::TRIG_LIMIT {
        match tan_cot(z, true) {
            Some(Ok(w)) if w.finite() => return Ok(w),
            _ => {}
        }
    }
    recip(imtan(z)?)
}

fn sech_csch(z: C, csch: bool) -> Option<C> {
    let (ch, sh) = (cosh_n(z.re), sinh_n(z.re));
    let w = if z.im == 0.0 {
        C::new(1.0 / if csch { sh } else { ch }, 0.0)
    } else {
        let (s, c) = (trig_sin(z.im).ok()?, trig_cos(z.im).ok()?);
        let (a1, a2, t) = if csch { (sh, ch, s) } else { (ch, sh, c) };
        let d = t * t + sh * sh;
        if d == 0.0 {
            return None;
        }
        C::new((a1 * c) / d, -((a2 * s) / d))
    };
    w.finite().then_some(w)
}

pub fn imsech(z: C) -> R<C> {
    if let Some(w) = sech_csch(z, false) {
        return Ok(w);
    }
    recip(imcosh(z)?)
}

pub fn imcsch(z: C) -> R<C> {
    if let Some(w) = sech_csch(z, true) {
        return Ok(w);
    }
    let (s, c) = (trig_sin(z.im)?, trig_cos(z.im)?);
    let (ch, sh) = (DD::new(dk_cosh(z.re)), sinh_dd(z.re));
    recip_ext(c, sh, s, ch, imsinh(z)?)
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn formatting() {
        assert_eq!(fmt_part(1e20), "100000000000000000000");
        assert_eq!(fmt_part(1e21), "1E+21");
        assert_eq!(fmt_part(1e-19), "0.0000000000000000001");
        assert_eq!(fmt_part(1.2e-19), "1.2E-19");
        assert_eq!(fmt_part(1.23456789012345e-6), "1.23456789012345E-06");
        assert_eq!(fmt_part(1.0 / 3.0), "0.333333333333333");
        assert_eq!(format(C::new(0.0, -1.0), None).unwrap(), "-i");
        assert_eq!(format(C::new(1.5, -2.25), Some('j')).unwrap(), "1.5-2.25j");
        assert_eq!(format(C::new(0.0, 0.0), None).unwrap(), "0");
    }
}
