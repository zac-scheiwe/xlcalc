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

#[cfg(test)]
mod tests {
    use super::*;

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
