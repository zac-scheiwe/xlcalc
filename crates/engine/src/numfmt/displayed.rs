use super::decimal::Dec;
use super::parse::{Body, DateTok, Item};
use super::render::{calc_fraction, fractional_part};
use super::NumberFormat;
use crate::number::pack_decimal;
use crate::x87;

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub(crate) enum LimPrec {
    Store,

    Cell,
}

fn pack(d: &Dec) -> f64 {
    if d.is_zero() {
        return 0.0;
    }
    let digits = d.digits.iter().fold(0u64, |n, &x| n * 10 + u64::from(x));
    pack_decimal(digits, i64::from(d.exp) + 1 - d.digits.len() as i64)
}

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub(crate) enum EditClass {
    Plain,

    Percent,

    Date { calendar: bool },
}

impl NumberFormat {
    pub(crate) fn edit_class(&self) -> EditClass {
        match self.sections.first().map(|s| &s.body) {
            Some(Body::Date(_)) => EditClass::Date {
                calendar: self.has_calendar_date(),
            },
            Some(Body::Number(l)) if l.scale >= 2 => EditClass::Percent,
            Some(Body::Frac(l)) if l.scale >= 2 => EditClass::Percent,
            _ => EditClass::Plain,
        }
    }
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

    pub(crate) fn displayed_value(&self, x: f64, routine: LimPrec) -> f64 {
        if !x.is_finite() {
            return x;
        }

        let signed = |v: f64| if x < 0.0 && v != 0.0 { -v } else { v };
        let digits = Dec::from_f64(x);

        let idx = match self.numeric {
            0 => return signed(pack(&digits)),
            1 => 0,
            2 => usize::from(x < 0.0),
            _ if x > 0.0 => 0,
            _ if x < 0.0 => 1,
            _ => 2,
        };

        let byte3 = |n_frac: usize, scale: i32| i32::from((n_frac as i32 + scale) as i8);
        match &self.sections[idx].body {
            Body::Number(l) => signed(pack(&digits.round_to(byte3(l.n_frac, l.scale)))),

            Body::Sci(l) => {
                let places = byte3(l.n_frac, l.precision_scale) - digits.exp;
                signed(pack(&digits.round_to(places)))
            }
            Body::Frac(l) => {
                let a = x.abs();
                let frac = fractional_part(a);
                match calc_fraction(frac, l.den_field, l.has_int) {
                    Some((num, den)) => {
                        let q = x87::div(f64::from(num), f64::from(den));
                        signed(x87::add(q, a.trunc()))
                    }
                    None if routine == LimPrec::Store => frac,
                    None => x,
                }
            }
            _ => signed(pack(&digits)),
        }
    }
}

#[cfg(test)]
mod tests {
    use super::{LimPrec, NumberFormat};

    fn lim(code: &str, x: f64, routine: LimPrec) -> f64 {
        NumberFormat::parse(code)
            .unwrap()
            .displayed_value(x, routine)
    }

    #[test]
    fn header_byte_sets_the_digits_kept() {
        for (code, x, want) in [
            ("0.00", 1.005, 1.01),
            ("0,", 499.0, 0.0),
            ("0,", 500.0, 1000.0),
            ("0.00E+0", 123456.789, 123000.0),
            ("0.00E+0%", 1.23456789, 1.2346),
            ("0.00E+0,", 4999.0, 0.0),
            ("0.00E+0,", 5000.0, 10000.0),
            ("0.00,E+0", 5000.0, 10000.0),
        ] {
            assert_eq!(lim(code, x, LimPrec::Store), want, "{code} {x}");
            assert_eq!(lim(code, x, LimPrec::Cell), want, "{code} {x}");
        }
    }

    #[test]
    fn a_refused_fraction() {
        let code = "# ?/32768";
        assert_eq!(lim(code, -69.99999, LimPrec::Cell), -69.99999);
        let bare = lim(code, -69.99999, LimPrec::Store);
        assert!(bare > 0.9999 && bare < 1.0, "{bare}");
    }
}
