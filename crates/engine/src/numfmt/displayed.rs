use super::decimal::Dec;
use super::parse::{Body, DateTok, Item};
use super::render::{calc_fraction, frac_spec, mp_frac};
use super::NumberFormat;
use crate::x87;

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
                let int = a.trunc();
                let frac = mp_frac(a);
                match calc_fraction(frac, frac_spec(&l.den), l.has_int) {
                    Some((num, den)) => signed(x87::add(x87::div(num as f64, den as f64), int)),
                    None => frac,
                }
            }
            _ => general(),
        }
    }
}
