use super::decimal::{digits_to_string, Dec};

pub const TEXT_GENERAL_WIDTH: usize = 11;

pub fn format_general_width(x: f64, width: usize) -> String {
    if x == 0.0 || !x.is_finite() {
        return "0".to_string();
    }
    let body = general_body(&Dec::from_f64(x), width.max(5) as i32);
    if x < 0.0 {
        format!("-{body}")
    } else {
        body
    }
}

pub fn format_general_cell(x: f64, chars: usize) -> Option<String> {
    format_general_cell_by(x, chars, None)
}

pub fn format_general_cell_by(
    x: f64,
    chars: usize,
    sci_fits: Option<&dyn Fn(&str) -> bool>,
) -> Option<String> {
    if x == 0.0 || !x.is_finite() {
        return (chars >= 1).then(|| "0".to_string());
    }
    let neg = x < 0.0;
    let w = chars
        .saturating_sub(usize::from(neg))
        .min(TEXT_GENERAL_WIDTH) as i32;
    if w == 0 {
        return None;
    }
    let d = Dec::from_f64(x.abs());
    let sci = (w >= 5)
        .then(|| scientific(&d, w))
        .filter(|s| match sci_fits {
            Some(f) => f(&if neg { format!("-{s}") } else { s.clone() }),
            None => s.len() as i32 <= w,
        });
    let body = match (fixed(&d, w), sci) {
        (Some((s, exact)), Some(sci)) if !exact && d.exp < 0 => {
            let exp_len = if d.exp.abs() >= 99 { 3 } else { 2 };
            if w - 1 + d.exp >= w - 3 - exp_len {
                s
            } else {
                sci
            }
        }
        (Some((s, _)), _) => s,
        (None, Some(sci)) => sci,
        (None, None) if d.exp < 0 && w < 5 => "0".to_string(),
        (None, None) => return None,
    };
    Some(if neg { format!("-{body}") } else { body })
}

fn fixed(d: &Dec, w: i32) -> Option<(String, bool)> {
    let (r, nd) = if d.exp >= 0 {
        let int_len = d.exp + 1;
        if int_len > w {
            return None;
        }
        let nd = (w - int_len - 1).max(0);
        (d.round_to(nd), nd)
    } else {
        let nd = (w - 2).max(0);
        (d.round_to(nd), nd)
    };
    if r.is_zero() {
        return None;
    }
    let int = r.int_digits();
    if int.len() as i32 > w {
        return None;
    }
    let exact = r == *d;
    let mut s = if int.is_empty() {
        "0".to_string()
    } else {
        digits_to_string(&int)
    };
    let frac = r.frac_digits(nd.max(0) as usize);
    let last = frac.iter().rposition(|&x| x != 0);
    if let Some(l) = last {
        s.push('.');
        s.push_str(&digits_to_string(&frac[..=l]));
    }
    Some((s, exact))
}

fn scientific(d: &Dec, w: i32) -> String {
    let mut exp = d.exp;
    let mant = Dec {
        digits: d.digits.clone(),
        exp: 0,
    };
    let exp_len = |e: i32| if e.abs() >= 99 { 3 } else { 2 };
    let mut nd = (w - 4 - exp_len(exp)).max(0);
    let mut m = mant.round_to(nd);
    if m.exp > 0 {
        exp += 1;
        nd = (w - 4 - exp_len(exp)).max(0);
        m = Dec {
            digits: m.digits.clone(),
            exp: 0,
        }
        .round_to(nd);
    }
    let mut s = String::new();
    s.push((b'0' + m.digit_at(0)) as char);
    let frac = m.frac_digits(nd as usize);
    if let Some(l) = frac.iter().rposition(|&x| x != 0) {
        s.push('.');
        s.push_str(&digits_to_string(&frac[..=l]));
    }
    s.push('E');
    s.push(if exp < 0 { '-' } else { '+' });
    s.push_str(&format!("{:02}", exp.abs()));
    s
}

fn general_body(d: &Dec, w: i32) -> String {
    let fx = fixed(d, w);
    if let Some((s, exact)) = &fx {
        if *exact || d.exp >= 0 {
            return s.clone();
        }

        let exp_len = if d.exp.abs() >= 99 { 3 } else { 2 };
        let fixed_cap = w - 1 + d.exp;
        let sci_cap = w - 3 - exp_len;
        if fixed_cap >= sci_cap {
            return s.clone();
        }
    }
    scientific(d, w)
}

pub const GEN_SCI_FLAG: u32 = 0x8000;

fn general_number_text(dig: &[u8], exp: i32, w: i32, flag: &mut u32) -> String {
    let mut buf = [0u8; 40];
    buf[..dig.len()].copy_from_slice(dig);
    let mut nsig = dig.len() as i32;
    let mut e = if nsig != 0 { exp } else { 1 };

    let (mut fixed, mut forced) = (false, false);
    if (-3..=4).contains(&e) && w < 5 {
        fixed = true;
        forced = true;
    } else if e <= w && ((w - 2 + e > 0 && (e > -4 || nsig <= w - 2 + e)) || w < 5) {
        fixed = true;
    }

    let mut keep = if fixed {
        if e == 0 && w == 1 {
            0
        } else {
            let point = if e >= w {
                0
            } else if e < 1 {
                2 - e
            } else {
                1
            };
            w - point
        }
    } else if w < 7 {
        1
    } else {
        w - if (-97..100).contains(&e) { 5 } else { 6 }
    };
    let mut fixed_out = true;
    if keep < nsig {
        if keep == 0 {
            if buf[0] < 5 {
                e = 1;
                nsig = 0;
            } else {
                buf[0] = 1;
                e += 1;
                nsig = 1;
            }
        } else if keep < 1 {
            e = 1;
            nsig = 0;
        } else {
            let mut j = keep as usize;
            let mut carried_out = false;
            if buf[j] >= 5 {
                loop {
                    buf[j - 1] += 1;
                    if buf[j - 1] != 10 {
                        break;
                    }
                    j -= 1;
                    if j == 0 {
                        carried_out = true;
                        break;
                    }
                }
            }
            if carried_out {
                buf[0] = 1;
                e += 1;
                nsig = 1;
            } else {
                keep = if buf[keep as usize] >= 5 {
                    j as i32
                } else {
                    keep
                };
                while keep > 1 && buf[keep as usize - 1] == 0 {
                    keep -= 1;
                }
                nsig = keep;
            }
        }
        fixed_out = e <= w || forced;
    }
    if fixed && fixed_out {
        if nsig <= e {
            return (0..e.max(1))
                .map(|i| if i < nsig { buf[i as usize] } else { 0 })
                .map(|d| (b'0' + d) as char)
                .collect();
        }
        *flag = (nsig - e) as u32;
        let digits = |r: std::ops::Range<i32>| -> String {
            r.map(|i| (b'0' + buf[i as usize]) as char).collect()
        };
        return if e > 0 {
            format!("{}.{}", digits(0..e), digits(e..nsig))
        } else {
            format!("0.{}{}", "0".repeat((-e) as usize), digits(0..nsig))
        };
    }

    *flag = GEN_SCI_FLAG;
    let mut out = String::new();
    out.push((b'0' + buf[0]) as char);
    if nsig > 1 {
        *flag = (nsig as u32 - 1) | GEN_SCI_FLAG;
        out.push('.');
        out.extend((1..nsig as usize).map(|i| (b'0' + buf[i]) as char));
    }
    let x = e - 1;
    out.push('E');
    out.push(if x < 0 { '-' } else { '+' });
    out.push_str(&format!("{:02}", x.abs()));
    out
}

pub fn general_best_fit(x: f64, start: i32, flag: &mut u32) -> String {
    general_fgen(x, start.clamp(1, 11), true, flag)
}

pub fn general_display_flag(x: f64, w: i32, flag: &mut u32) {
    if w >= 1 && x != 0.0 && x.is_finite() {
        general_fgen(x, w.min(11), false, flag);
    }
}

fn general_fgen(x: f64, start: i32, best_fit: bool, flag: &mut u32) -> String {
    let d = Dec::from_f64(x);
    let (dig, exp) = (&d.digits, d.exp + 1);
    let mut w = start;
    let mut text;
    loop {
        text = general_number_text(dig, exp, w, flag);
        if !best_fit || *flag & GEN_SCI_FLAG == 0 {
            break;
        }
        w += 1;
        if w >= 12 {
            break;
        }
    }
    if (w as usize) < text.len() {
        text = general_number_text(dig, exp, text.len() as i32, flag);
    }
    if x < 0.0 {
        text.insert(0, '-');
    }
    text
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn best_fit_text_and_flag() {
        let mut flag = 0;
        assert_eq!(general_best_fit(12345.6789, 1, &mut flag), "12345.7");
        assert_eq!(general_best_fit(123456.789, 1, &mut flag), "123456.8");
        assert_eq!(general_best_fit(99999.999, 1, &mut flag), "99999.999");
        assert_eq!(general_best_fit(5.0, 1, &mut flag), "5");
        assert_eq!(flag & GEN_SCI_FLAG, 0);
        let mut sci = GEN_SCI_FLAG;
        assert!(general_best_fit(-1234.5, 1, &mut sci).starts_with('-'));
    }

    #[test]
    fn best_fit_branch_edges() {
        let mut f = GEN_SCI_FLAG;
        assert_eq!(general_best_fit(-98.7654321, 3, &mut f), "-98.8");
        assert_eq!(f, 1);
        let mut f = 2;
        assert_eq!(general_best_fit(-98.7654321, 3, &mut f), "-99");
        assert_eq!(f, 2);

        let mut f = 2;
        assert_eq!(general_best_fit(0.000123456, 4, &mut f), "0");
        assert_eq!(f, 2);

        assert_eq!(general_best_fit(0.000123456, 5, &mut f), "0.0001");
        assert_eq!(f, 4);

        let mut f = 3;
        assert_eq!(general_best_fit(12345.6789, 4, &mut f), "12345.7");
        let mut f = 3;
        assert_eq!(general_best_fit(12345.6789, 5, &mut f), "12346");

        let mut f = 0;
        assert_eq!(general_best_fit(-1234.5, 3, &mut f), "-1235");

        let mut f = 0;
        assert_eq!(general_best_fit(1e-10, 4, &mut f), "0");
        assert_eq!(general_best_fit(1e-10, 5, &mut f), "1E-10");
        assert_eq!(f, GEN_SCI_FLAG);
    }

    #[test]
    fn display_flag() {
        let mut f = 0;
        general_display_flag(0.000123456, 5, &mut f);
        assert_eq!(f, GEN_SCI_FLAG);

        general_display_flag(123456789.0, 9, &mut f);
        assert_eq!(f, GEN_SCI_FLAG);
        general_display_flag(-98.7654321, 5, &mut f);
        assert_eq!(f, 2);

        general_display_flag(0.5, 0, &mut f);
        general_display_flag(0.0, 5, &mut f);
        assert_eq!(f, 2);
    }

    #[test]
    fn general_11() {
        let cases: &[(f64, &str)] = &[
            (1234567.891, "1234567.891"),
            (1234567890123.0, "1.23457E+12"),
            (12345678901.0, "12345678901"),
            (123456789012.0, "1.23457E+11"),
            (0.000012345, "0.000012345"),
            (1.0 / 3.0, "0.333333333"),
            (1e20, "1E+20"),
            (-1234567890123.0, "-1.23457E+12"),
            (99999999999.5, "1E+11"),
            (9999999999.96, "10000000000"),
            (0.000123456789, "0.000123457"),
            (0.0000123456789, "1.23457E-05"),
            (0.0000001234, "1.234E-07"),
            (0.000001234, "0.000001234"),
            (1e-10, "1E-10"),
            (0.00000001, "0.00000001"),
            (1.23456789e100, "1.2346E+100"),
            (9.99999e99, "1E+100"),
            (0.99999999999, "1"),
            (1.23456789012345, "1.23456789"),
        ];
        for (x, want) in cases {
            assert_eq!(format_general_width(*x, 11), *want, "for {x:e}");
        }
    }
}
