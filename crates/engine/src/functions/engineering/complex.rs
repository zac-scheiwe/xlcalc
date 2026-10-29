use super::atp::{self, R};
use super::xlmath;
use crate::coerce::parse_number_text;
use crate::eval::{EvalCtx, Operand};
use crate::functions::args::val;
use crate::value::{Value, XlError};
use crate::x87::MpFlag;

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
        use crate::x87::{add, div, mul, sqrt};
        let (a, b) = (self.re.abs(), self.im.abs());
        let (big, small) = if a >= b { (a, b) } else { (b, a) };
        if big == 0.0 {
            return 0.0;
        }
        let r = div(small, big);
        mul(sqrt(add(mul(r, r), 1.0)), big)
    }

    pub fn modulus(self) -> f64 {
        let sq = |v: f64| crate::number::flush_subnormal(v * v);
        crate::number::flush_subnormal(sq(self.re) + sq(self.im)).sqrt()
    }

    pub fn arg(self) -> f64 {
        use crate::x87::{add, atan, div, sub};
        use std::f64::consts::{FRAC_PI_2, PI};
        let (y, x) = (self.im, self.re);
        if x == 0.0 {
            return if y < 0.0 { -FRAC_PI_2 } else { FRAC_PI_2 };
        }
        let t = atan(div(y, x));
        if x > 0.0 {
            t + 0.0
        } else if y < 0.0 {
            sub(t, PI)
        } else {
            add(t, PI)
        }
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
        return Err(XlError::NUM);
    }
    let sfx = suffix.unwrap_or('i');
    let re = crate::number::normalize(z.re);
    let im = crate::number::normalize(z.im);

    let too_big = |v: f64| {
        v != 0.0 && {
            let (d, e) = digits15(v.abs());
            e > 308 || e == 308 && format!("{d:0<15}").as_str() > "179769313486231"
        }
    };
    if too_big(re) || too_big(im) {
        return Err(XlError::NUM);
    }
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
        Value::Text(s) => parse_text(ctx, s).ok_or(XlError::NUM),
        Value::Error(e) => Err(*e),
        _ => Err(XlError::VALUE),
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
                        _ => return Err(XlError::VALUE),
                    },
                    Some(Some(Value::Blank)) | Some(None) | None => 'i',
                    Some(Some(Value::Error(e))) => return Err(*e),
                    Some(Some(_)) => return Err(XlError::VALUE),
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
        return Err(XlError::DIV0);
    }
    Ok(z.arg())
}

pub fn imsub(ctx: &mut EvalCtx, ops: &[Operand]) -> Operand {
    atp::call(ctx, ops, "a", |ctx, a, _| {
        val((|| {
            let (x, sx) = req(ctx, a, 0)?;
            let (y, sy) = req(ctx, a, 1)?;
            let s = join(sx, sy, XlError::NUM)?;

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
            let s = join(sx, sy, XlError::NUM)?;
            if y.is_zero() {
                return Err(XlError::NUM);
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
            Err(XlError::NUM)
        };
    }
    let m = if z.im == 0.0 { z.re.abs() } else { z.modulus() };

    let r = crate::xmath::pow(m, n);

    let t = crate::x87::mul(n, z.arg());
    let mul = crate::x87::mul;
    Ok(C::new(mul(x87_cos(t)?, r), mul(x87_sin(t)?, r)))
}

pub fn fold(ctx: &mut EvalCtx, ops: &[Operand], product: bool) -> Operand {
    if ops.first().is_none_or(|o| o.is_missing()) {
        return Operand::err(XlError::NA);
    }
    let r: R<Value> = (|| {
        let mut acc: Option<C> = None;
        let mut sfx: Suffix = None;
        for op in ops {
            atp::visit_list(ctx, op, |ctx, v| {
                let (z, s) = parse(ctx, v)?;
                sfx = join(sfx, s, XlError::VALUE)?;
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
            return Err(XlError::VALUE);
        }
        Ok(Value::from(format(z, sfx)?))
    })();
    Operand::Val(val(r))
}

fn trig_sin(x: f64) -> R<f64> {
    if x.abs() >= xlmath::TRIG_LIMIT {
        return Err(XlError::NUM);
    }
    Ok(xlmath::sin(x))
}

fn trig_cos(x: f64) -> R<f64> {
    if x.abs() >= xlmath::TRIG_LIMIT {
        return Err(XlError::NUM);
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

pub fn imsqrt(z: C) -> R<C> {
    if z.is_zero() {
        return Ok(z);
    }
    let r = z.modulus().sqrt();
    let t = z.arg() / 2.0;
    Ok(C::new(r * trig_cos(t)?, r * trig_sin(t)?))
}

fn exp_clamped(x: f64) -> f64 {
    if x > 709.7 {
        f64::MAX
    } else if x < -709.0 {
        0.0
    } else {
        let e = x.exp();
        if e.is_finite() {
            e
        } else {
            f64::MAX
        }
    }
}

pub fn imexp(z: C) -> R<C> {
    let e = exp_clamped(z.re);
    Ok(C::new(scale(trig_cos(z.im)?, e), scale(trig_sin(z.im)?, e)))
}

pub fn imln(z: C) -> R<C> {
    if z.is_zero() {
        return Err(XlError::NUM);
    }

    let m = if z.im == 0.0 {
        z.re.abs()
    } else if z.re == 0.0 {
        z.im.abs()
    } else {
        z.modulus()
    };
    Ok(C::new(m.ln(), z.arg()))
}

fn imlog_k(z: C, k: f64) -> R<C> {
    let l = imln(z)?;
    Ok(C::new(crate::x87::mul(l.re, k), crate::x87::mul(l.im, k)))
}

pub fn imlog10(z: C) -> R<C> {
    imlog_k(z, std::f64::consts::LOG10_E)
}

pub fn imlog2(z: C) -> R<C> {
    imlog_k(z, f64::from_bits(0x3FF71547652B82FF))
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

fn sinh_cosh(z: C, cosh: bool) -> R<C> {
    let (x, y) = (z.re, z.im);
    let mut f = MpFlag::default();
    let num = |_| XlError::NUM;
    if y == 0.0 {
        let v = if cosh {
            cosh2(&mut f, -x)
        } else {
            sinh2n(&mut f, -x)
        };
        let r = f.div(v, 2.0);
        return mp_done(&f, C::new(r, 0.0));
    }
    let (s, c) = (x87_sin(y).map_err(num)?, x87_cos(y).map_err(num)?);
    if x > 709.0 || x < -708.0 {
        let ax = x.abs();
        let mut big = |t: f64| -> R<f64> {
            let h = f.div(t, 2.0);
            if h <= 0.0 || h.is_nan() {
                return Err(XlError::NUM);
            }
            Ok(f.exp(crate::x87::add(crate::x87::ln(h), ax)))
        };
        let (mut re, mut im) = (big(c)?, big(s)?);
        if x < 0.0 {
            if cosh {
                im = -im;
            } else {
                re = -re;
            }
        }
        return mp_done(&f, C::new(re, im));
    }
    let sh = sinh2n(&mut f, -x);
    let ch = cosh2(&mut f, -x);
    let (a, b) = if cosh { (ch, sh) } else { (sh, ch) };
    let c2 = f.div(c, 2.0);
    let re = f.mul(c2, a);
    let s2 = f.div(s, 2.0);
    let im = f.mul(s2, b);
    mp_done(&f, C::new(re, im))
}

pub fn imsinh(z: C) -> R<C> {
    sinh_cosh(z, false)
}

pub fn imcosh(z: C) -> R<C> {
    sinh_cosh(z, true)
}

fn tan_cot(z: C, cot: bool) -> Option<R<C>> {
    if z.im == 0.0 {
        let t = xlmath::tan(z.re);
        return Some(Ok(C::new(if cot { 1.0 / t } else { t }, 0.0)));
    }

    use crate::x87::{add, div, mul, sub};
    let (s, c) = (x87_sin(z.re).ok()?, x87_cos(z.re).ok()?);
    let b = mul(z.im, 2.0);
    let mut f = MpFlag::default();
    let sh2 = -sinh2n(&mut f, b);
    let ch2 = cosh2(&mut f, b);
    let (t, u) = if cot { (c, s) } else { (s, c) };
    let t4 = mul(4.0, t);
    let d = sub(add(ch2, 2.0), mul(t4, t));
    let im = div(sh2, d);
    let w = C::new(div(mul(t4, u), d), if cot { -im } else { im });
    w.finite().then_some(Ok(w))
}

pub fn imtan(z: C) -> R<C> {
    if hyp_band(z.im) != 0 {
        return Ok(C::new(0.0, 1f64.copysign(z.im)));
    }
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
        return Err(XlError::NUM);
    }
    Ok(C::new(1.0, 0.0).div(z))
}

fn sinh2n(f: &mut MpFlag, t: f64) -> f64 {
    let a = t.abs();
    if a > 20.0 {
        let e = f.exp(a);
        return if t > 0.0 { -e } else { e };
    }
    if a > 0.01 {
        let e = f.exp(t);
        let ie = f.div(1.0, e);
        return f.sub(ie, e);
    }
    let u = -t;
    let u2 = f.mul(u, 2.0);
    let uu = f.mul(u, u);
    let a3 = f.mul(uu, u);
    let a3 = f.div(a3, 3.0);
    let s = f.add(a3, u2);
    let a5 = f.mul(a3, uu);
    let a5 = f.div(a5, 20.0);
    f.add(a5, s)
}

fn cosh2(f: &mut MpFlag, t: f64) -> f64 {
    let a = t.abs();
    if a > 20.0 {
        return f.exp(a);
    }
    if a > 1e-4 {
        let e = f.exp(t);
        let ie = f.div(1.0, e);
        return f.add(ie, e);
    }
    let tt = f.mul(t, t);
    let s = f.add(tt, 2.0);
    let a4 = f.mul(tt, tt);
    let a4 = f.div(a4, 12.0);
    f.add(a4, s)
}

fn hyp_band(v: f64) -> u8 {
    if v <= 354.5 && v > -354.0 {
        0
    } else if (-708.0..=709.0).contains(&v) {
        1
    } else {
        2
    }
}

fn mp_done(f: &MpFlag, w: C) -> R<C> {
    if f.0 != 0 {
        Err(XlError::NUM)
    } else {
        Ok(C::new(w.re + 0.0, w.im + 0.0))
    }
}

fn sec_csc(z: C, csc: bool) -> R<C> {
    let (x, y) = (z.re, z.im);
    let mut f = MpFlag::default();
    let num = |_| XlError::NUM;
    if y == 0.0 {
        let t = if csc { x87_sin(x) } else { x87_cos(x) }.map_err(num)?;
        let r = f.div(1.0, t);
        return mp_done(&f, C::new(r, 0.0));
    }
    let band = hyp_band(y);
    if band == 2 {
        return Ok(C::new(0.0, 0.0));
    }
    let (s, c) = (x87_sin(x).map_err(num)?, x87_cos(x).map_err(num)?);

    let (m1, m2) = if csc {
        (f.mul(s, 2.0), f.mul(c, 2.0))
    } else {
        (f.mul(c, 2.0), f.mul(s, -2.0))
    };
    let sh = sinh2n(&mut f, y);
    let ch = cosh2(&mut f, y);
    if band == 1 {
        let w = C::new(f.div(m1, ch), f.div(m2, sh));
        return mp_done(&f, w);
    }
    let y2 = f.mul(y, 2.0);
    let ch2 = cosh2(&mut f, y2);
    let ch2 = f.sub(ch2, 2.0);
    let sq = f.mul(m1, m1);
    let d = f.add(sq, ch2);
    let re = f.div(ch, d);
    let re = f.mul(re, m1);
    let im = f.div(sh, d);
    let im = f.mul(im, m2);
    mp_done(&f, C::new(re, im))
}

pub fn imsec(z: C) -> R<C> {
    sec_csc(z, false)
}

pub fn imcsc(z: C) -> R<C> {
    sec_csc(z, true)
}

pub fn imcot(z: C) -> R<C> {
    if hyp_band(z.im) != 0 {
        return Ok(C::new(0.0, -1f64.copysign(z.im)));
    }
    if z.re.abs() < xlmath::TRIG_LIMIT {
        match tan_cot(z, true) {
            Some(Ok(w)) if w.finite() => return Ok(w),
            _ => {}
        }
    }
    recip(imtan(z)?)
}

fn sech_csch(z: C, csch: bool) -> R<C> {
    let (x, y) = (z.re, z.im);
    let mut f = MpFlag::default();
    let num = |_| XlError::NUM;
    if y == 0.0 {
        let h = if csch {
            sinh2n(&mut f, -x)
        } else {
            cosh2(&mut f, -x)
        };
        if f.0 != 0 {
            return Ok(C::new(0.0, 0.0));
        }
        let r = f.div(2.0, h);
        return mp_done(&f, C::new(r, 0.0));
    }
    match hyp_band(x) {
        2 => Ok(C::new(0.0, 0.0)),
        1 => {
            let sh = sinh2n(&mut f, x);
            let ch = cosh2(&mut f, x);
            let c2 = f.mul(x87_cos(y).map_err(num)?, 2.0);
            let re = f.div(c2, if csch { sh } else { ch });
            let s2 = f.mul(x87_sin(y).map_err(num)?, 2.0);
            let im = f.div(s2, if csch { ch } else { sh });
            mp_done(&f, C::new(re, im))
        }
        _ => {
            let sh = sinh2n(&mut f, -x);
            let ch = cosh2(&mut f, -x);
            let c2 = f.mul(x87_cos(y).map_err(num)?, 2.0);
            let (re_n, im_n, d);
            if csch {
                re_n = f.mul(c2, sh);
                let s2 = f.mul(x87_sin(y).map_err(num)?, -2.0);
                im_n = f.mul(s2, ch);
                let ss = f.mul(s2, s2);
                let a = f.mul(sh, sh);
                d = f.add(a, ss);
            } else {
                re_n = f.mul(c2, ch);
                let cc = f.mul(c2, c2);
                let s2 = f.mul(x87_sin(y).map_err(num)?, -2.0);
                im_n = f.mul(s2, sh);
                let a = f.mul(sh, sh);
                d = f.add(a, cc);
            }
            let w = C::new(f.div(re_n, d), f.div(im_n, d));
            mp_done(&f, w)
        }
    }
}

pub fn imsech(z: C) -> R<C> {
    sech_csch(z, false)
}

pub fn imcsch(z: C) -> R<C> {
    sech_csch(z, true)
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
