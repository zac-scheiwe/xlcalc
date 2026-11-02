use super::{conv, decimal, Opd, K, MISMATCH};
use crate::{HResult, Locale, VarType, Variant};
use std::cmp::Ordering;

pub const NORM_IGNORECASE: u32 = 1;

fn code(o: Ordering) -> u8 {
    match o {
        Ordering::Less => 0,
        Ordering::Equal => 1,
        Ordering::Greater => 2,
    }
}

pub fn compare(a: &Variant, b: &Variant, _locale: Locale, flags: u32) -> Result<u8, HResult> {
    if let (Variant::Error(x), Variant::Error(y)) = (a, b) {
        return Ok(code(x.cmp(y)));
    }
    let (l, r) = (Opd::of(a)?, Opd::of(b)?);
    if matches!(l, Opd::Nothing) || matches!(r, Opd::Nothing) {
        return Err(MISMATCH);
    }
    if matches!(l, Opd::Null) || matches!(r, Opd::Null) {
        return Ok(3);
    }
    match (&l, &r) {
        (Opd::Str(x), Opd::Str(y)) => Ok(code(text(x, y, flags))),
        (Opd::Str(x), Opd::Num(Variant::Empty)) => Ok(code(text(x, "", flags))),
        (Opd::Num(Variant::Empty), Opd::Str(y)) => Ok(code(text("", y, flags))),
        (Opd::Str(_), _) => Ok(2),
        (_, Opd::Str(_)) => Ok(0),
        (Opd::Num(x), Opd::Num(y)) => number(x, y).map(code),
        _ => Err(MISMATCH),
    }
}

const SYMBOLS: &str = " !\"#$%&()*,./:;?@[\\]^_`{|}~+<=>";

fn keys(c: char) -> (u32, u32, u32) {
    let lower = c.to_lowercase().next().unwrap_or(c);
    let case = u32::from(c != lower);
    let (base, accent) = match lower {
        '\u{e1}' => ('a', 1),
        '\u{e0}' => ('a', 2),
        '\u{e2}' => ('a', 3),
        '\u{e4}' => ('a', 4),
        '\u{e5}' => ('a', 5),
        '\u{e3}' => ('a', 6),
        '\u{e7}' => ('c', 7),
        '\u{e9}' => ('e', 1),
        '\u{e8}' => ('e', 2),
        '\u{ea}' => ('e', 3),
        '\u{eb}' => ('e', 4),
        '\u{ed}' => ('i', 1),
        '\u{ec}' => ('i', 2),
        '\u{ee}' => ('i', 3),
        '\u{ef}' => ('i', 4),
        '\u{f1}' => ('n', 6),
        '\u{f3}' => ('o', 1),
        '\u{f2}' => ('o', 2),
        '\u{f4}' => ('o', 3),
        '\u{f6}' => ('o', 4),
        '\u{f5}' => ('o', 6),
        '\u{fa}' => ('u', 1),
        '\u{f9}' => ('u', 2),
        '\u{fb}' => ('u', 3),
        '\u{fc}' => ('u', 4),
        '\u{fd}' => ('y', 1),
        '\u{ff}' => ('y', 4),
        l => (l, 0),
    };
    let primary = if let Some(i) = SYMBOLS.chars().position(|s| s == base) {
        i as u32
    } else if base.is_ascii_digit() || base.is_numeric() {
        0x100 + base as u32
    } else {
        0x1000 + base as u32
    };
    (primary, accent, case)
}

fn text(x: &str, y: &str, flags: u32) -> Ordering {
    let words = |t: &str| {
        t.chars()
            .filter(|c| !matches!(c, '-' | '\''))
            .map(keys)
            .collect::<Vec<_>>()
    };
    let (kx, ky) = (words(x), words(y));
    let level = |f: fn(&(u32, u32, u32)) -> u32| {
        let a: Vec<u32> = kx.iter().map(f).collect();
        let b: Vec<u32> = ky.iter().map(f).collect();
        a.cmp(&b)
    };
    level(|k| k.0)
        .then_with(|| level(|k| k.1))
        .then_with(|| {
            if flags & NORM_IGNORECASE != 0 {
                Ordering::Equal
            } else {
                level(|k| k.2)
            }
        })
        .then_with(|| {
            let marks = |t: &str| {
                let mut kept = 0;
                let mut out = Vec::new();
                for c in t.chars() {
                    if matches!(c, '-' | '\'') {
                        out.push((std::cmp::Reverse(kept), c == '-'));
                    } else {
                        kept += 1;
                    }
                }
                out
            };
            marks(x).cmp(&marks(y))
        })
}

fn number(x: &Variant, y: &Variant) -> Result<Ordering, HResult> {
    match exact(x, y) {
        Err(super::OVERFLOW)
            if matches!(x, Variant::R4(_) | Variant::R8(_) | Variant::Date(_))
                || matches!(y, Variant::R4(_) | Variant::R8(_) | Variant::Date(_)) =>
        {
            super::r8(x)?.partial_cmp(&super::r8(y)?).ok_or(MISMATCH)
        }
        r => r,
    }
}

fn exact(x: &Variant, y: &Variant) -> Result<Ordering, HResult> {
    let (kx, ky) = (K::of(x).ok_or(MISMATCH)?, K::of(y).ok_or(MISMATCH)?);
    if kx == K::Dec || ky == K::Dec {
        let (Variant::Decimal(p), Variant::Decimal(q)) =
            (conv(x, VarType::Decimal)?, conv(y, VarType::Decimal)?)
        else {
            return Err(MISMATCH);
        };

        let Ok(d) = decimal::sub(p, q) else {
            return Ok(if p.negative {
                Ordering::Less
            } else {
                Ordering::Greater
            });
        };
        return Ok(if d.mantissa == 0 {
            Ordering::Equal
        } else if d.negative {
            Ordering::Less
        } else {
            Ordering::Greater
        });
    }
    if kx == K::Cy || ky == K::Cy {
        let cy = |v: &Variant| match conv(v, VarType::Cy)? {
            Variant::Cy(c) => Ok(c),
            _ => Err(MISMATCH),
        };
        return Ok(cy(x)?.cmp(&cy(y)?));
    }
    if kx == K::R4 && ky == K::R8 || kx == K::R8 && ky == K::R4 {
        let f = |v: &Variant| super::r8(v).map(|x| f64::from(x as f32));
        return f(x)?.partial_cmp(&f(y)?).ok_or(MISMATCH);
    }
    let float = |k: K| matches!(k, K::R4 | K::R8 | K::Date | K::Cy);
    if !(float(kx) || float(ky)) {
        let i = |v: &Variant, other: K| match v {
            Variant::Bool(true) if other == K::Ui1 => 255,
            v => super::logic::bits(v),
        };
        return Ok(i(x, ky).cmp(&i(y, kx)));
    }
    let f = |v: &Variant| super::r8(v);
    f(x)?.partial_cmp(&f(y)?).ok_or(MISMATCH)
}
