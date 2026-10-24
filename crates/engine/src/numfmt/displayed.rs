use super::decimal::Dec;
use super::parse::Den;
use super::parse::{Body, DateTok, Item};
use super::render::approximate;
use super::NumberFormat;

fn to_f64(d: &Dec) -> f64 {
    if d.is_zero() {
        return 0.0;
    }
    let digits: String = d.digits.iter().map(|&x| (b'0' + x) as char).collect();
    let (head, tail) = digits.split_at(1);
    let tail = if tail.is_empty() { "0" } else { tail };
    format!("{head}.{tail}e{}", d.exp).parse().unwrap_or(0.0)
}

impl NumberFormat {
    pub fn has_calendar_date(&self) -> bool {
        match self.sections.first().map(|s| &s.body) {
            Some(Body::Date(l)) => l.items.iter().any(|it| {
                matches!(
                    it,
                    Item::Date(
                        DateTok::Year(_)
                            | DateTok::Month(_)
                            | DateTok::Day(_)
                            | DateTok::EYear(_)
                            | DateTok::Era(_)
                            | DateTok::Buddhist(_)
                    )
                )
            }),
            _ => false,
        }
    }

    pub fn displayed_value(&self, x: f64) -> f64 {
        if !x.is_finite() {
            return x;
        }
        let neg = x < 0.0;
        let a = x.abs();
        let signed = |v: f64| if neg && v != 0.0 { -v } else { v };
        let general = || signed(to_f64(&Dec::from_f64(a)));

        let n = self.numeric;
        if n == 0 {
            return general();
        }
        let idx = match n {
            1 => 0,
            2 => usize::from(x < 0.0),
            _ => {
                if x > 0.0 {
                    0
                } else if x < 0.0 {
                    1
                } else {
                    2
                }
            }
        };
        match &self.sections[idx].body {
            Body::Number(l) => {
                let d = Dec::from_f64(a)
                    .shifted(l.scale)
                    .round_to(l.n_frac as i32)
                    .shifted(-l.scale);
                signed(to_f64(&d))
            }
            Body::Sci(l) => {
                let d = Dec::from_f64(a);
                if d.is_zero() {
                    return 0.0;
                }
                let e = d.exp;
                let m = d.clone().shifted(-e).round_to(l.n_frac as i32);
                signed(to_f64(&m.shifted(e)))
            }
            Body::Frac(l) => {
                let v = a * 10f64.powi(l.scale);
                let (int, num, den) = match l.den {
                    Den::Fixed(d) => {
                        let d = d as f64;
                        let rr = (v * d + 0.5).floor();
                        if l.has_int {
                            let base = (rr / d).floor();
                            (base, rr - base * d, d)
                        } else {
                            (0.0, rr, d)
                        }
                    }
                    Den::Max(n) => {
                        let maxd = 10f64.powi(n as i32) - 1.0;
                        let (p, q) = approximate(v, maxd);
                        if l.has_int {
                            let base = (p / q).floor();
                            (base, p - base * q, q)
                        } else {
                            (0.0, p, q)
                        }
                    }
                };
                if !(den > 0.0) || !int.is_finite() || !num.is_finite() {
                    return general();
                }
                let shown = int + num / den;
                signed(if l.scale != 0 {
                    shown / 10f64.powi(l.scale)
                } else {
                    shown
                })
            }
            _ => general(),
        }
    }
}
