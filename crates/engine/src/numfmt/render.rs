use super::calendars;
use super::decimal::{uint_digits, Dec};
use super::general::{format_general_width, TEXT_GENERAL_WIDTH};
use super::locales::names_for;
use super::parse::{
    Body, DateLayout, DateTok, Den, FracLayout, Item, NumLayout, Ph, SciLayout, Section,
};
use super::{Cmp, NumberFormat, Rendered};
use crate::datetime::{self, DateSystem};
use crate::locale::Locale;
use crate::value::XlError;
use crate::x87;

fn negative_only((op, b): (Cmp, f64)) -> bool {
    match op {
        Cmp::Lt => b <= 0.0,
        Cmp::Le | Cmp::Eq => b < 0.0,
        _ => false,
    }
}

fn else_drops_sign((op, b): (Cmp, f64)) -> bool {
    match op {
        Cmp::Lt | Cmp::Le | Cmp::Ne => true,
        Cmp::Eq => false,
        Cmp::Gt | Cmp::Ge => b <= 0.0,
    }
}

pub(super) fn choose(f: &NumberFormat, x: f64) -> Result<(Option<usize>, bool), XlError> {
    let n = f.numeric;
    let s = &f.sections;
    if n == 0 {
        return Ok((None, true));
    }
    let has_cond = s[..n.min(2)].iter().any(|s| s.cond.is_some());
    if !has_cond {
        return Ok(match n {
            1 => (Some(0), true),
            2 => {
                if x >= 0.0 {
                    (Some(0), true)
                } else {
                    (Some(1), false)
                }
            }
            _ => {
                if x > 0.0 {
                    (Some(0), true)
                } else if x < 0.0 {
                    (Some(1), false)
                } else {
                    (Some(2), true)
                }
            }
        });
    }
    let c1 = s[0].cond.unwrap_or((Cmp::Gt, 0.0));

    let plain_general = n == 1 && matches!(&s[0].body, Body::General(items) if items.len() == 1);
    if c1.0.test(x, c1.1) {
        return Ok((Some(0), plain_general || !negative_only(c1)));
    }
    if n == 1 {
        if plain_general || s.len() > 1 {
            return Err(XlError::VALUE);
        }
        return Ok((None, !else_drops_sign(c1)));
    }
    match s[1].cond {
        Some(c2) => {
            if c2.0.test(x, c2.1) {
                Ok((Some(1), !negative_only(c2)))
            } else if n >= 3 {
                Ok((Some(2), true))
            } else {
                Err(XlError::VALUE)
            }
        }

        None if n >= 3
            && matches!(
                (f.code_lens.first(), f.code_lens.get(2)),
                (Some(Some(a)), Some(Some(b))) if a % 256 == b % 256
            ) =>
        {
            Ok((Some(1), !else_drops_sign(c1)))
        }
        None if n >= 3 => {
            if x < 0.0 {
                Ok((Some(1), false))
            } else {
                Ok((Some(2), true))
            }
        }
        None => Ok((Some(1), !else_drops_sign(c1))),
    }
}

fn digit_system(nn: u8) -> Option<[char; 10]> {
    let (zero, has_zero) = match nn {
        2 => (0x0660, true),
        3 => (0x06F0, true),
        4 => (0x0966, true),
        5 => (0x09E6, true),
        6 => (0x0A66, true),
        7 => (0x0AE6, true),
        8 => (0x0B66, true),
        9 => (0x0BE6, false),
        0xA => (0x0C66, true),
        0xB => (0x0CE6, true),
        0xC => (0x0D66, true),
        0xD => (0x0E50, true),
        0xE => (0x0ED0, true),
        0xF => (0x0F20, true),
        0x10 => (0x1040, true),
        0x11 => (0x1368, false),
        0x12 => (0x17E0, true),
        0x13 => (0x1810, true),
        0x1B..=0x27 => {
            let t = match nn {
                0x1B => "〇一二三四五六七八九",
                0x1C => "〇壱弐参四伍六七八九",
                0x1E | 0x21 => "○一二三四五六七八九",
                0x1F => "零壹贰叁肆伍陆柒捌玖",
                0x22 => "零壹貳參肆伍陸柒捌玖",
                0x24 => "\u{ff10}一二三四五\u{f9d1}七八九",
                0x25 => "\u{f9b2}壹貳\u{f96b}四伍\u{f9d1}七八九",
                0x27 => "영일이삼사오육칠팔구",
                _ => "０１２３４５６７８９",
            };
            let mut d = ['0'; 10];
            for (c, o) in t.chars().zip(d.iter_mut()) {
                *o = c;
            }
            return Some(d);
        }
        _ => return None,
    };
    let mut d = ['0'; 10];
    for (i, c) in d.iter_mut().enumerate() {
        if i > 0 || has_zero {
            *c = char::from_u32(zero + i as u32).unwrap_or('0');
        }
    }
    Some(d)
}

pub(super) fn render_number(
    f: &NumberFormat,
    x: f64,
    loc: &Locale,
    ds: DateSystem,
) -> Result<Rendered, XlError> {
    let mut r = render_number_plain(f, x, loc, ds)?;

    let tag = f.sections.iter().find_map(|s| s.digit_sys);
    let cjk_date = tag.is_some_and(|n| n >= 0x1B) && f.is_date();
    if let (true, false, Some(d)) = (f.numeric > 0, cjk_date, tag.and_then(digit_system)) {
        r.text = r
            .text
            .chars()
            .map(|c| {
                if c.is_ascii_digit() {
                    d[c as usize - '0' as usize]
                } else {
                    c
                }
            })
            .collect();
    }
    r.text = unshield(&r.text);
    Ok(r)
}

fn render_number_plain(
    f: &NumberFormat,
    x: f64,
    loc: &Locale,
    ds: DateSystem,
) -> Result<Rendered, XlError> {
    let (idx, auto_minus) = choose(f, x)?;
    let Some(idx) = idx else {
        let g = format_general_width(x.abs(), TEXT_GENERAL_WIDTH);
        let text = if auto_minus && x < 0.0 {
            format!("-{g}")
        } else {
            g
        };
        return Ok(Rendered { text, color: None });
    };
    let sec: &Section = &f.sections[idx];
    let (text, zero) = render_body(&sec.body, x.abs(), x < 0.0, loc, ds)?;
    if auto_minus && x < 0.0 && zero {
        return render_number_plain(f, -x, loc, ds);
    }
    let text = if auto_minus && x < 0.0 && !zero {
        format!("-{text}")
    } else {
        text
    };
    Ok(Rendered {
        text,
        color: sec.color,
    })
}

pub(super) fn render_text(f: &NumberFormat, s: &str, _loc: &Locale) -> Result<Rendered, XlError> {
    let Some(i) = f.text_section else {
        return Ok(Rendered {
            text: s.to_string(),
            color: None,
        });
    };
    let sec = &f.sections[i];
    let items = match &sec.body {
        Body::Text(items) | Body::Literal(items) => items,
        _ => {
            return Ok(Rendered {
                text: s.to_string(),
                color: None,
            })
        }
    };
    let mut out = String::new();
    for it in items {
        match it {
            Item::Text => out.push_str(s),
            other => push_literal(&mut out, other),
        }
    }
    Ok(Rendered {
        text: unshield(&out),
        color: sec.color,
    })
}

const SHIELD: u32 = 0xE000;

fn shield(c: char) -> char {
    char::from_u32(SHIELD + c as u32 - '0' as u32).unwrap_or(c)
}

fn unshield(text: &str) -> String {
    text.chars()
        .map(|c| {
            if (SHIELD..=SHIELD + 9).contains(&(c as u32)) {
                char::from_u32(c as u32 - SHIELD + '0' as u32).unwrap_or(c)
            } else {
                c
            }
        })
        .collect()
}

fn push_literal(out: &mut String, it: &Item) {
    match it {
        Item::Lit(s) => out.extend(
            s.chars()
                .map(|c| if c.is_ascii_digit() { shield(c) } else { c }),
        ),
        Item::Space => out.push(' '),
        _ => {}
    }
}

fn render_body(
    body: &Body,
    a: f64,
    negative: bool,
    loc: &Locale,
    ds: DateSystem,
) -> Result<(String, bool), XlError> {
    Ok(match body {
        Body::Literal(items) | Body::Text(items) => {
            let mut out = String::new();
            for it in items {
                push_literal(&mut out, it);
            }
            (out, false)
        }
        Body::General(items) => {
            let mut out = String::new();
            for it in items {
                match it {
                    Item::General => out.push_str(&format_general_width(a, TEXT_GENERAL_WIDTH)),
                    other => push_literal(&mut out, other),
                }
            }
            (out, false)
        }
        Body::Number(l) => render_fixed(l, a, loc),
        Body::Sci(l) => (render_sci(l, a, loc), a == 0.0),
        Body::Frac(l) => render_frac(l, a, loc)?,
        Body::Date(l) => {
            if negative && !l.signed && ds == DateSystem::D1900 {
                return Err(XlError::VALUE);
            }
            render_date(l, a, ds, loc)?
        }
    })
}

#[inline]
fn digit(d: u8) -> char {
    (b'0' + d) as char
}

fn pad(k: Ph) -> Option<char> {
    match k {
        Ph::Zero => Some('0'),
        Ph::Q => Some(' '),
        Ph::Hash => None,
    }
}

struct IntLayout<'a> {
    digits: &'a [u8],
    n: usize,
    grouping: bool,
    sep: char,
    idx: usize,
}

impl IntLayout<'_> {
    fn group_after(&self, out: &mut String, pos: usize, real: bool, k: Ph) {
        if self.grouping && pos > 0 && pos.is_multiple_of(3) {
            if real || k == Ph::Zero {
                out.push(self.sep);
            } else if k == Ph::Q {
                out.push(' ');
            }
        }
    }

    fn excess(&self, out: &mut String) {
        let l = self.digits.len();
        if l > self.n {
            for j in 0..l - self.n {
                out.push(digit(self.digits[j]));
                self.group_after(out, l - 1 - j, true, Ph::Zero);
            }
        }
    }

    fn placeholder(&mut self, out: &mut String, k: Ph) {
        if self.idx == 0 {
            self.excess(out);
        }
        let l = self.digits.len();
        let pos = self.n - 1 - self.idx;
        let real = pos < l;
        if real {
            out.push(digit(self.digits[l - 1 - pos]));
        } else if let Some(c) = pad(k) {
            out.push(c);
        }
        self.group_after(out, pos, real, k);
        self.idx += 1;
    }
}

fn frac_mask(items: &[Item], frac: &[u8]) -> Vec<bool> {
    let kinds: Vec<Ph> = items
        .iter()
        .filter_map(|it| match it {
            Item::Frac(k) => Some(*k),
            _ => None,
        })
        .collect();

    let last = frac.iter().rposition(|&d| d != 0);
    (0..kinds.len())
        .map(|i| last.is_some_and(|l| i <= l) || kinds[i] == Ph::Zero)
        .collect()
}

fn render_fixed(l: &NumLayout, a: f64, loc: &Locale) -> (String, bool) {
    let d = Dec::from_f64(a).shifted(l.scale).round_to(l.n_frac as i32);
    let int = d.int_digits();
    let frac = d.frac_digits(l.n_frac);
    let show = frac_mask(&l.items, &frac);
    let mut out = String::new();
    let mut il = IntLayout {
        digits: &int,
        n: l.n_int,
        grouping: l.grouping,
        sep: loc.thousands_sep,
        idx: 0,
    };
    let mut fi = 0;
    for it in &l.items {
        match it {
            Item::Int(k) => il.placeholder(&mut out, *k),
            Item::Point => {
                if l.n_int == 0 {
                    il.excess(&mut out);
                }
                out.push(loc.decimal_sep);
            }
            Item::Frac(k) => {
                if show[fi] {
                    out.push(digit(frac[fi]));
                } else if *k == Ph::Q {
                    out.push(' ');
                }
                fi += 1;
            }
            other => push_literal(&mut out, other),
        }
    }
    (out, d.is_zero())
}

fn render_sci(l: &SciLayout, a: f64, loc: &Locale) -> String {
    let g = if l.grouping {
        l.n_int.max(4)
    } else {
        l.n_int.max(1)
    } as i32;
    let d = Dec::from_f64(a);
    let (mant, exp) = if d.is_zero() {
        (Dec::zero(), 0)
    } else {
        let mut e = d.exp.div_euclid(g) * g;
        let mut m = d.clone().shifted(-e).round_to(l.n_frac as i32);
        if m.exp >= g {
            e += g;
            m = d.clone().shifted(-e).round_to(l.n_frac as i32);
        }
        (m, e)
    };
    let int = mant.int_digits();
    let frac = mant.frac_digits(l.n_frac);
    let show = frac_mask(&l.items, &frac);
    let edigits = uint_digits(exp.unsigned_abs() as u64);

    let int = if mant.is_zero() {
        vec![0; l.n_int]
    } else {
        int
    };
    let mut out = String::new();
    let mut il = IntLayout {
        digits: &int,
        n: l.n_int,
        grouping: l.grouping,
        sep: loc.thousands_sep,
        idx: 0,
    };
    let mut el = IntLayout {
        digits: &edigits,
        n: l.n_exp,

        grouping: l.grouping,
        sep: loc.thousands_sep,
        idx: 0,
    };
    let mut fi = 0;
    for it in &l.items {
        match it {
            Item::Int(k) => il.placeholder(&mut out, *k),
            Item::Point => {
                if l.n_int == 0 {
                    il.excess(&mut out);
                }
                out.push(loc.decimal_sep);
            }
            Item::Frac(k) => {
                if show[fi] {
                    out.push(digit(frac[fi]));
                } else if *k == Ph::Q {
                    out.push(' ');
                }
                fi += 1;
            }
            Item::ExpMark(plus) => {
                if l.n_int == 0 && l.n_frac == 0 {
                    il.excess(&mut out);
                }
                out.push('E');
                if exp < 0 {
                    out.push('-');
                } else if *plus {
                    out.push('+');
                }
            }
            Item::ExpDigit(k) => el.placeholder(&mut out, *k),
            other => push_literal(&mut out, other),
        }
    }
    out
}

pub(super) fn frac_spec(den: &Den) -> i16 {
    match *den {
        Den::Max(n) => n.min(i16::MAX as u32) as i16,
        Den::Fixed(v) => (v as i32).wrapping_neg() as i16,
    }
}

fn mp_lfix(x: f64) -> i32 {
    let t = x.trunc();
    if t > -2_147_483_649.0 && t < 2_147_483_648.0 {
        t as i32
    } else {
        i32::MIN
    }
}

fn mp_fix(x: f64) -> u32 {
    let t = x.abs().trunc();
    if t < 4_294_967_296.0 {
        t as u32
    } else {
        u32::MAX
    }
}

fn muldiv1(a: i32, b: i32) -> i32 {
    let p = a as i64 * b as i64;
    if p > i32::MAX as i64 || p < i32::MIN as i64 {
        -1
    } else {
        p as i32
    }
}

pub(super) fn mp_frac(x: f64) -> f64 {
    let d = x87::sub(x.trunc(), x);
    if d.to_bits() & 0xFFFF_0000_0000_0000 != 0 {
        -d
    } else {
        d
    }
}

pub(super) fn calc_fraction(x: f64, spec: i16, mixed: bool) -> Option<(u32, u32)> {
    let mut x = if mixed { mp_frac(x) } else { x };
    if spec < 0 && x < 137_438_953_472.0 {
        x = x87::mul(x, f64::from_bits(0x3FF0_0000_0000_0001));
    }
    if mixed {
        x = mp_frac(x);
    }
    if spec < 0 {
        let d = -(spec as i32);
        let n2 = mp_lfix(x87::mul((2 * d) as f64, x));
        if n2 as u32 > 0xFFFE {
            return None;
        }
        return Some(((n2 as u32 + 1) >> 1, d as u32));
    }
    let limit: i32 = match spec {
        1 => 10,
        2 => 100,
        3 => 1_000,
        4 => 10_000,
        5 => 100_000,
        6 => 1_000_000,
        _ => 10_000_000,
    };
    let mut b = x;
    let mut bi = b.trunc();
    let mut a = mp_fix(b);
    if a > 0x7FFF_FFFE {
        return None;
    }

    let (mut n, mut d, mut pn, mut pd) = (1i32, 0i32, 0i32, 1i32);
    loop {
        let t1 = muldiv1(n, a as i32);
        let t2 = muldiv1(d, a as i32);
        let nd = t2.wrapping_add(pd);
        if t1 < 0 || t2 < 0 || nd >= limit {
            if d == 1 && a == 2 && x87::sub(bi, b).to_bits() & 0xFFFF_0000_0000_0000 == 0 {
                n = n.wrapping_add(1);
            }
            break;
        }
        let nn = t1.wrapping_add(pn);
        pn = n;
        pd = d;
        n = nn;
        d = nd;
        let diff = x87::sub(bi, b);
        if diff.to_bits() & 0xFFFF_0000_0000_0000 == 0 || !diff.is_finite() {
            break;
        }
        b = x87::div(1.0, -diff);
        bi = b.trunc();
        a = mp_fix(b);
        if (a as i32) < 0 {
            break;
        }
    }
    if d == 0 {
        return Some((1, 1));
    }
    Some((n as u32, d as u32))
}

fn den_layout(kinds: &[Ph], digits: &[u8]) -> Vec<String> {
    let mut cells = vec![String::new(); kinds.len()];
    let mut p = kinds.len() as i64;
    let mut d = digits.len() as i64;
    let mut buf = digits.iter().copied();
    let mut i = 0;
    while i < kinds.len() {
        let kind = kinds[i];
        let mut j = i;
        while j < kinds.len() && kinds[j] == kind {
            j += 1;
        }
        let run = (j - i) as i64;
        let mut pos = i;
        if kind == Ph::Q {
            let mut r = run;
            if d < 0 {
                let z = r.min(-d);
                for _ in 0..z {
                    cells[pos].push('0');
                    pos += 1;
                }
                d += z;
                r -= z;
            }
            for _ in 0..r {
                cells[pos].push(buf.next().map_or(' ', digit));
                pos += 1;
            }
            p -= r;
            d -= r;
        } else {
            let mut r = run;
            if p > d {
                let e = r.min(p - d);
                for _ in 0..e {
                    if kind == Ph::Zero {
                        cells[pos].push('0');
                    }
                    pos += 1;
                }
                p -= e;
                r -= e;
            } else {
                r += d - p;
                p = d;
            }

            let extra = r - (j - pos) as i64;
            for k in 0..r {
                cells[pos].push(buf.next().map_or('0', digit));
                if k >= extra {
                    pos += 1;
                }
                p -= 1;
                d -= 1;
            }
        }
        i = j;
    }
    cells
}

#[derive(PartialEq)]
enum Blank {
    Show,
    Spaces,
    Remove,
}

fn render_frac(l: &FracLayout, a: f64, loc: &Locale) -> Result<(String, bool), XlError> {
    let v = if l.scale != 0 {
        x87::mul(10f64.powi(l.scale), a)
    } else {
        a
    };
    let (num, den) = calc_fraction(v, frac_spec(&l.den), l.has_int).ok_or(XlError::VALUE)?;
    let (int_digits_raw, num) = if l.has_int {
        let dec = Dec::from_f64(v);
        if num == den || num == 0 {
            (dec.round_to(0).int_digits(), 0.0)
        } else {
            (dec.int_digits(), num as f64)
        }
    } else {
        (Vec::new(), num as f64)
    };
    let den = den as f64;
    let int = if int_digits_raw.is_empty() { 0.0 } else { 1.0 };

    let blank = if l.has_int && num == 0.0 {
        if l.num_has_zero {
            Blank::Show
        } else if l.int_has_q || l.num_has_q || l.den_left {
            Blank::Spaces
        } else {
            Blank::Remove
        }
    } else {
        Blank::Show
    };
    let all_zero = int == 0.0 && num == 0.0;
    let last_int = l.items.iter().rev().find_map(|it| match it {
        Item::Int(k) => Some(*k),
        _ => None,
    });
    let first_num = l.items.iter().find_map(|it| match it {
        Item::Num(k) => Some(*k),
        _ => None,
    });

    let int_digits: Vec<u8> = if int > 0.0 {
        int_digits_raw
    } else if all_zero
        && (blank != Blank::Show || last_int == Some(Ph::Q) || first_num != Some(Ph::Zero))
    {
        vec![0]
    } else {
        Vec::new()
    };

    let int_empty = int_digits.is_empty() && last_int != Some(Ph::Zero);

    if l.has_int && l.n_num == 0 && int_empty {
        return Err(XlError::VALUE);
    }

    let num_digits = if blank == Blank::Show {
        uint_digits(num as u64)
    } else {
        vec![0]
    };
    let den_digits = uint_digits(den as u64);
    let sep_as_spaces = num != 0.0 && (l.int_has_q || l.num_first_q);
    let mut out = String::new();
    let mut il = IntLayout {
        digits: &int_digits,
        n: l.n_int,
        grouping: l.grouping,
        sep: loc.thousands_sep,
        idx: 0,
    };
    let mut nl = IntLayout {
        digits: &num_digits,
        n: l.n_num,
        grouping: false,
        sep: ',',
        idx: 0,
    };
    let den_kinds: Vec<Ph> = l
        .items
        .iter()
        .filter_map(|it| match it {
            Item::Den(k) => Some(*k),
            _ => None,
        })
        .collect();
    let den_cells = den_layout(&den_kinds, &den_digits);
    let mut den_i = 0;
    let mut den_dec_i = 0;
    let mut den_q_seen = false;
    let mut tmp = String::new();
    for (i, it) in l.items.iter().enumerate() {
        tmp.clear();
        match it {
            Item::Int(k) => il.placeholder(&mut tmp, *k),
            Item::Num(k) => nl.placeholder(&mut tmp, *k),
            Item::NumAll => tmp.extend(num_digits.iter().map(|d| digit(*d))),
            Item::DenDec(k) => {
                match den_digits.get(den_dec_i) {
                    Some(d) => tmp.push(digit(*d)),
                    None => tmp.extend(pad(*k)),
                }
                den_dec_i += 1;
            }
            Item::Slash => tmp.push('/'),
            Item::Den(_) => {
                tmp.push_str(&den_cells[den_i]);
                den_i += 1;
            }
            Item::DenFixed(s) => {
                let lit: Vec<char> = s.chars().collect();
                for (j, d) in den_digits.iter().enumerate() {
                    match lit.get(j) {
                        Some(c) => tmp.push(*c),
                        None if !(l.num_has_q || l.int_has_q) => tmp.push(digit(*d)),
                        None => {}
                    }
                }
                if l.num_has_q || l.int_has_q {
                    let k = den_digits.len().min(lit.len());
                    tmp.extend(std::iter::repeat_n(' ', lit.len() - k));
                }
            }
            other => push_literal(&mut tmp, other),
        }
        let width = tmp.chars().count();
        let in_span = l.has_int && i >= l.sep_start && i < l.den_end;
        if in_span && blank != Blank::Show {
            if matches!(it, Item::Den(Ph::Q) | Item::DenDec(Ph::Q)) {
                den_q_seen = true;
            }
            let shown = match blank {
                Blank::Spaces => true,
                _ => {
                    matches!(
                        it,
                        Item::Num(Ph::Q) | Item::Den(Ph::Q) | Item::DenDec(Ph::Q)
                    ) || (den_q_seen && matches!(it, Item::Den(Ph::Zero) | Item::DenDec(Ph::Zero)))
                }
            };
            if shown {
                out.extend(std::iter::repeat_n(' ', width));
            }
            continue;
        }
        let in_sep = l.has_int && i >= l.sep_start && i < l.num_start;
        if in_sep && int_empty {
            if sep_as_spaces {
                out.extend(std::iter::repeat_n(' ', width));
            }
            continue;
        }
        out.push_str(&tmp);
    }
    let zero = if l.has_int { false } else { num == 0.0 };
    Ok((out, zero))
}

fn japanese_era(y: i64, m: u32, d: u32) -> (usize, i64) {
    const STARTS: [(i64, u32, u32); 5] = [
        (1868, 1, 1),
        (1912, 7, 30),
        (1926, 12, 25),
        (1989, 1, 8),
        (2019, 5, 1),
    ];
    let idx = STARTS.iter().rposition(|&s| (y, m, d) >= s).unwrap_or(0);
    (idx, y - STARTS[idx].0 + 1)
}

fn hu(x: f64) -> f64 {
    (x + 0.5).floor()
}

#[derive(Clone, Copy)]
struct Cascade {
    y: f64,
    day: i64,
    h: i64,
    m: i64,
    s: i64,
    sub: i64,

    stale: f64,
}

fn half_unit(k: u8) -> f64 {
    0.5 / 86_400.0 / 10f64.powi(k as i32)
}

fn set_num_cascade(x: f64, k: u8) -> Cascade {
    let y = x87::add(x, half_unit(k));
    let d = y.floor();
    let mut f = x87::sub(y, d);
    let mut parts = [0i64; 3];
    let mut v = 0.0;
    for (i, m) in [24.0, 60.0, 60.0].into_iter().enumerate() {
        v = x87::mul(f, m);
        let w = v.trunc();
        parts[i] = w as i64;
        f = x87::sub(v, w);
    }
    let r = crate::number::round15(x87::mul(f, 10f64.powi(k as i32)));
    Cascade {
        y,
        day: d as i64,
        h: parts[0],
        m: parts[1],
        s: parts[2],
        sub: r.trunc() as i64,
        stale: if k > 0 { r } else { v },
    }
}

fn elapsed_head(y: f64, unit: u8) -> f64 {
    let mut v = y;
    for _ in 1..unit {
        v = x87::mul(v, 60.0);
    }
    x87::mul(v, 24.0).floor()
}

fn head_text(v: f64, w: usize) -> String {
    if v.is_finite() && v.abs() < 1e15 {
        return format!("{:0w$}", v as i64);
    }
    let digits: String = if !v.is_finite() || v < 0.0 {
        "0".into()
    } else {
        Dec::from_f64(v)
            .round_to(0)
            .int_digits()
            .iter()
            .map(|&d| digit(d))
            .collect()
    };
    format!("{digits:0>w$}")
}

const HIJRI_MONTHS: [&str; 12] = [
    "محرم",
    "صفر",
    "ربيع\u{a0}الأول",
    "ربيع\u{a0}الثاني",
    "جمادى\u{a0}الأولى",
    "جمادى\u{a0}الثانية",
    "رجب",
    "شعبان",
    "رمضان",
    "شوال",
    "ذو\u{a0}القعدة",
    "ذو\u{a0}الحجة",
];
const HIJRI_DAYS: [&str; 7] = [
    "الأحد",
    "الإثنين",
    "الثلاثاء",
    "الأربعاء",
    "الخميس",
    "الجمعة",
    "السبت",
];

fn render_date(
    l: &DateLayout,
    x: f64,
    ds: DateSystem,
    loc: &Locale,
) -> Result<(String, bool), XlError> {
    let scale = 10i64.pow(l.subsec as u32);
    let per_day = 86_400 * scale;
    let (day, tod, total) = if l.elapsed {
        let ms = hu(x * 86_400_000.0);
        let t = hu(ms / (1000 / scale) as f64);
        if t > 9.0e18 {
            let heads_only = l.subsec == 0
                && x >= 0.0
                && l.items.iter().all(|it| {
                    !matches!(it, Item::Date(d) if !matches!(d, DateTok::ElapsedH(_) | DateTok::ElapsedM(_) | DateTok::ElapsedS(_)))
                });
            if !heads_only {
                return Err(XlError::VALUE);
            }
        }
        let t = t.min(9.0e18);

        if l.subsec > 0 && (t / per_day as f64).floor() > ds.max_serial() {
            return Err(XlError::VALUE);
        }
        let t = t as i64;
        (t / per_day, t % per_day, t)
    } else {
        let d0 = x.floor();
        if d0 > 1.0e9 {
            return Err(XlError::VALUE);
        }
        let mut d = d0 as i64;

        let mut t = hu((x - d0) * scale as f64 * 24.0 * 3600.0) as i64;
        if t >= per_day {
            t -= per_day;
            d += 1;
        }
        if l.subsec == 0 && x >= 0.0 {
            let (sd, hh, mm, ss, _) = datetime::split_serial(x);
            d = sd;
            t = (hh * 3600 + mm * 60 + ss) as i64;
        } else if x >= 0.0 {
            let c = set_num_cascade(x, l.subsec);
            d = c.day;
            t = ((c.h * 60 + c.m) * 60 + c.s) * scale + c.sub;
        }
        (d, t, d * per_day + t)
    };

    let traced = (l.elapsed && x >= 0.0).then(|| set_num_cascade(x, l.subsec));
    if l.needs_date && day as f64 > ds.max_serial() {
        return Err(XlError::VALUE);
    }

    let cal = if l.hijri {
        0
    } else {
        l.lcid.map_or(0, |id| ((id >> 16) & 0xFF) as u8)
    };
    let lang = l.lcid.map_or(0, |id| id & 0xFFFF);
    let cal_names = if cal != 0 {
        calendars::names(cal, lang)
    } else {
        None
    };
    let names = if cal_names.is_some() && (lang == 0 || lang == 0x0409) {
        &super::locales::EN_NZ
    } else {
        names_for(l.lcid)
    };
    let mut hebrew_leap = false;
    let (y, mo, dd) = if l.needs_date && calendars::converts(cal) {
        let serial = if ds == DateSystem::D1904 {
            day + 1462
        } else {
            day
        };
        let (y, m, d, leap) = calendars::convert(cal, serial)?;
        hebrew_leap = leap;
        (y, m, d)
    } else if l.needs_date && l.hijri {
        let serial = if ds == DateSystem::D1904 {
            day + 1462
        } else {
            day
        };
        datetime::hijri_from_serial(serial).ok_or(XlError::VALUE)?
    } else if l.needs_date {
        let (y, m, d) = datetime::ymd_from_serial(ds, day);
        (y + calendars::year_offset(cal), m, d)
    } else {
        (1900, 1, 0)
    };
    let secs_of_day = tod / scale;
    let sub = tod % scale;
    let total_secs = total / scale;
    let (h24, mut mi, mut s) = (
        secs_of_day / 3600,
        (secs_of_day / 60) % 60,
        secs_of_day % 60,
    );
    if l.elapsed && l.subsec == 0 && x >= 0.0 {
        let (_, _, mm, ss, _) = datetime::split_serial(x);
        mi = mm as i64;
        s = ss as i64;
    }
    let mut sub = sub;
    if let (Some(c), true) = (traced, l.subsec > 0) {
        (mi, s, sub) = (c.m, c.s, c.sub);
    }
    let head = |unit: u8, old: i64| traced.map_or(old as f64, |c| elapsed_head(c.y, unit));
    let hour12 = if h24 % 12 == 0 { 12 } else { h24 % 12 };
    let num = |v: i64, n: u8| {
        if n >= 2 {
            format!("{:02}", v)
        } else {
            v.to_string()
        }
    };

    let japanese = l.lcid.is_some_and(|id| id & 0xFFFF == 0x0411);

    let era_cal = cal == 0x03;
    let (era, era_year) = if (japanese || era_cal) && l.needs_date {
        japanese_era(y, mo, dd)
    } else {
        (0, y)
    };
    let era_years = era_cal
        || japanese
            && l.items
                .iter()
                .any(|it| matches!(it, Item::Date(DateTok::Era(_))));

    let stale = || set_num_cascade(x.abs(), l.subsec).stale;
    let left = |unit: u8| elapsed_head(x87::add(stale(), half_unit(l.subsec)), unit);
    let mut out = String::new();
    for it in &l.items {
        let Item::Date(t) = it else {
            push_literal(&mut out, it);
            continue;
        };
        match *t {
            DateTok::Year(_) if era_years => out.push_str(&format!("{:02}", era_year)),
            DateTok::Year(n) => {
                if n <= 2 {
                    out.push_str(&format!("{:02}", y.rem_euclid(100)));
                } else {
                    out.push_str(&y.to_string());
                }
            }
            DateTok::Month(n) => {
                let m = (mo as usize).min(12) - 1;
                match n {
                    1 | 2 => out.push_str(&num(mo as i64, n)),
                    _ if cal_names.is_some() => out.push_str(calendars::month_name(
                        cal_names.unwrap(),
                        hebrew_leap,
                        mo,
                        n.min(5),
                    )),
                    3 | 4 if l.hijri => out.push_str(HIJRI_MONTHS[m]),
                    5 if l.hijri => out.extend(HIJRI_MONTHS[m].chars().next()),
                    3 => out.push_str(names.months_abbr[m]),
                    5 => out.push_str(names.month_initial(m)),
                    _ => out.push_str(names.months[m]),
                }
            }
            DateTok::Day(n) => {
                let wd = datetime::weekday_from_serial(ds, day) as usize;
                match n {
                    1 | 2 => out.push_str(&num(dd as i64, n)),
                    3 if cal_names.is_some() => out.push_str(cal_names.unwrap().days_abbr[wd]),
                    _ if cal_names.is_some() => out.push_str(cal_names.unwrap().days[wd]),
                    _ if l.hijri => out.push_str(HIJRI_DAYS[wd]),
                    3 => out.push_str(names.days_abbr[wd]),
                    _ => out.push_str(names.days[wd]),
                }
            }
            DateTok::Hour(n) => out.push_str(&num(h24, n)),
            DateTok::Hour12(n) => out.push_str(&num(hour12, n)),
            DateTok::Minute(n) => out.push_str(&num(mi, n)),
            DateTok::Second(n) => out.push_str(&num(s, n)),
            DateTok::SubSec(n) => {
                let digits = format!("{:0w$}", sub, w = l.subsec as usize);
                out.push(loc.decimal_sep);
                out.push_str(&digits[..n as usize]);
            }
            DateTok::ElapsedH(n) => {
                out.push_str(&head_text(head(1, total_secs / 3600), n as usize))
            }
            DateTok::ElapsedM(n) => out.push_str(&head_text(head(2, total_secs / 60), n as usize)),
            DateTok::ElapsedS(n) => out.push_str(&head_text(head(3, total_secs), n as usize)),
            DateTok::LeftH(n) => out.push_str(&head_text(left(1), n as usize)),
            DateTok::LeftM(n) => out.push_str(&head_text(left(2), n as usize)),
            DateTok::LeftS(n) => out.push_str(&head_text(left(3), n as usize)),
            DateTok::General(serial) => {
                let v = if serial { x } else { stale() };
                out.push_str(&super::general::format_general_width(
                    v,
                    super::general::TEXT_GENERAL_WIDTH,
                ));
            }
            DateTok::AmPm => out.push_str(if h24 < 12 { names.am } else { names.pm }),
            DateTok::AmPmCjk => out.push_str(if h24 < 12 { "上午" } else { "下午" }),
            DateTok::EYear(n) => {
                if japanese {
                    out.push_str(&num(era_year, n));
                } else {
                    out.push_str(&y.to_string());
                }
            }
            DateTok::Era(n) => {
                if japanese {
                    out.push_str(match n {
                        1 => ["M", "T", "S", "H", "R"][era],
                        2 => ["明", "大", "昭", "平", "令"][era],
                        _ => ["明治", "大正", "昭和", "平成", "令和"][era],
                    });
                }
            }
            DateTok::Buddhist(n) => {
                let by = if l.lcid.is_some() { y } else { y + 543 };
                let s = by.to_string();
                if n <= 2 {
                    out.push_str(if s.len() >= 4 { &s[2..4] } else { &s });
                } else {
                    out.push_str(&s);
                }
            }
        }
    }
    Ok((out, l.signed && total == 0))
}
