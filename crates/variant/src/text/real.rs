pub(super) const R8_DIGITS: usize = 15;

pub(super) const R4_DIGITS: usize = 7;

fn round_digits(x: f64, nd: usize) -> (u64, i32) {
    let (digits, exp) = scientific(x, nd - 1);
    let mut n: u64 = digits.parse().expect("digits");
    let mut decpt = exp + 1;
    if is_tie(x, nd as i32 - decpt) {
        let (exact, _) = scientific(x, nd);
        n = exact[..nd].parse::<u64>().expect("digits") + 1;
    }
    let ten = 10u64.pow(nd as u32 - 1);
    if n == ten && digits.starts_with('1') && below_power(x, exp) {
        n = ten * 10;
        decpt = exp;
    }
    (n, decpt)
}

fn scientific(x: f64, decimals: usize) -> (String, i32) {
    let s = format!("{x:.decimals$e}");
    let (m, e) = s.split_once('e').expect("exponent");
    (m.replace('.', ""), e.parse().expect("exponent"))
}

fn below_power(x: f64, exp: i32) -> bool {
    scientific(x, 40).1 < exp
}

fn is_tie(x: f64, s: i32) -> bool {
    let bits = x.to_bits();
    let (mut m, mut e) = if bits >> 52 == 0 {
        (bits, -1074)
    } else {
        (bits & ((1 << 52) - 1) | 1 << 52, (bits >> 52) as i32 - 1075)
    };
    let z = m.trailing_zeros();
    m >>= z;
    e += z as i32;
    e + 1 + s == 0 && (s >= 0 || -s <= 22 && m % 5u64.pow((-s) as u32) == 0)
}

fn special(x: f64) -> String {
    let sign = if x.is_sign_negative() { "-" } else { "" };
    let body = if x.is_infinite() {
        "inf"
    } else if x.to_bits() & 1 << 51 == 0 {
        "nan(snan)"
    } else if x.to_bits() == 0xFFF8_0000_0000_0000 {
        "nan(ind)"
    } else {
        "nan"
    };
    format!("{sign}{body}")
}

pub(super) fn real_text(x: f64, nd: usize) -> String {
    if !x.is_finite() {
        return special(x);
    }
    let sign = if x.is_sign_negative() { "-" } else { "" };
    if x == 0.0 {
        return format!("{sign}0");
    }
    let (n, decpt) = round_digits(x.abs(), nd);
    let digits = n.to_string();
    let exp = decpt - 1;
    let body = if exp < -1 || exp > nd as i32 - 1 {
        let exp = exp + (digits.len() > nd) as i32;
        let lead = &digits[..1];
        let frac = digits[1..nd].trim_end_matches('0');
        if exp < 0 && -exp as usize + frac.len() <= nd {
            format!("0.{}{lead}{frac}", "0".repeat(-exp as usize - 1))
        } else {
            let point = if frac.is_empty() { "" } else { "." };
            let esign = if exp < 0 { '-' } else { '+' };
            format!("{lead}{point}{frac}E{esign}{:02}", exp.unsigned_abs())
        }
    } else {
        let decimals = nd - decpt as usize;
        let padded = format!("{digits:0>width$}", width = decimals + 1);
        let (int, frac) = padded.split_at(padded.len() - decimals);
        let frac = frac.trim_end_matches('0');
        if frac.is_empty() {
            int.to_string()
        } else {
            format!("{int}.{frac}")
        }
    };
    format!("{sign}{body}")
}
