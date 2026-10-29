use std::cmp::Ordering;

#[inline]
pub fn normalize(n: f64) -> f64 {
    if n == 0.0 {
        0.0
    } else {
        n
    }
}

#[inline]
pub fn flush_subnormal(n: f64) -> f64 {
    if n != 0.0 && n.abs() < f64::MIN_POSITIVE {
        0.0
    } else {
        n
    }
}

fn binary_exponent(x: f64) -> i32 {
    ((x.to_bits() >> 52) & 0x7ff) as i32
}

pub fn snap_add(a: f64, b: f64, r: f64) -> f64 {
    if r == 0.0 || !r.is_finite() {
        return r;
    }
    let big = a.abs().max(b.abs());
    if big == 0.0 {
        return r;
    }
    if binary_exponent(big) - binary_exponent(r) >= 50 {
        0.0
    } else {
        r
    }
}

pub fn round15(x: f64) -> f64 {
    if x == 0.0 || !x.is_finite() {
        return x;
    }

    let long = format!("{:.15e}", x);
    let (mant, exp) = long.split_once('e').unwrap_or((&long, "0"));
    if mant.ends_with('5') {
        let exact = format!("{:.60e}", x);
        let (m60, _) = exact.split_once('e').unwrap_or((&exact, "0"));
        if m60[mant.len()..].bytes().all(|b| b == b'0') {
            let digits: String = mant.chars().filter(char::is_ascii_digit).collect();
            let head: u64 = digits[..15].parse().unwrap_or(0);
            let e: i32 = exp.parse().unwrap_or(0);
            let sign = if x < 0.0 { "-" } else { "" };
            return format!("{sign}{}e{}", head + 1, e - 14)
                .parse()
                .unwrap_or(x);
        }
    }
    format!("{:.14e}", x).parse().unwrap_or(x)
}

pub fn cmp_numbers(a: f64, b: f64) -> Ordering {
    if a == b {
        return Ordering::Equal;
    }
    let scale = a.abs().max(b.abs());
    if (a - b).abs() <= scale * 1e-13 && round15(a) == round15(b) {
        return Ordering::Equal;
    }
    a.partial_cmp(&b).unwrap_or(Ordering::Equal)
}

pub fn parse_literal(s: &str) -> Option<f64> {
    let b = s.as_bytes();
    let mut i = 0;
    let mut digits = String::with_capacity(20);
    let mut point_pos: Option<usize> = None;
    let mut any_digit = false;
    while i < b.len() {
        match b[i] {
            c @ b'0'..=b'9' => {
                any_digit = true;
                digits.push(c as char);
            }
            b'.' if point_pos.is_none() => point_pos = Some(digits.len()),
            _ => break,
        }
        i += 1;
    }
    if !any_digit {
        return None;
    }
    let mut exp: i64 = 0;
    if i < b.len() && (b[i] == b'e' || b[i] == b'E') {
        i += 1;
        let neg = match b.get(i) {
            Some(b'-') => {
                i += 1;
                true
            }
            Some(b'+') => {
                i += 1;
                false
            }
            _ => false,
        };
        let es = i;
        while i < b.len() && b[i].is_ascii_digit() {
            exp = (exp * 10 + (b[i] - b'0') as i64).min(100_000);
            i += 1;
        }
        if es == i {
            return None;
        }
        if neg {
            exp = -exp;
        }
    }
    if i != b.len() {
        return None;
    }
    let int_len = point_pos.unwrap_or(digits.len()) as i64;

    let first_sig = digits.bytes().position(|c| c != b'0');
    let Some(first_sig) = first_sig else {
        return (-308..=308).contains(&exp).then_some(0.0);
    };
    let sig: String = digits[first_sig..].chars().take(15).collect();

    let dec_exp = int_len - first_sig as i64 + exp - sig.len() as i64;

    let sci_exp = dec_exp + sig.len() as i64 - 1;
    if !(-309..=307).contains(&sci_exp) {
        return None;
    }
    let v = mp_pack(sig.parse().ok()?, dec_exp);
    if !v.is_finite() {
        return None;
    }
    Some(flush_subnormal(v))
}

fn mp_pack(d: u64, k: i64) -> f64 {
    if d == 0 {
        return 0.0;
    }
    let lz = d.leading_zeros();
    let (mut m, mut e) = (d << lz, -(lz as i64));
    let a = k.unsigned_abs();
    let (lo, mid) = if k > 0 {
        (&POS_LO, &POS_MID)
    } else {
        (&NEG_LO, &NEG_MID)
    };
    let mut pieces = Vec::with_capacity(3);
    if !a.is_multiple_of(16) {
        pieces.push(lo[(a % 16 - 1) as usize]);
    }
    if !(a / 16).is_multiple_of(16) {
        pieces.push(mid[((a / 16) % 16 - 1) as usize]);
    }
    if a >= 256 {
        if a >= 512 {
            return if k > 0 { f64::INFINITY } else { 0.0 };
        }
        pieces.push(mid[15]);
    }
    for (cm, ce) in pieces {
        let p = (m as u128) * (cm as u128);
        let sh = if p >> 127 != 0 { 64 } else { 63 };
        let mut hi = (p >> sh) as u64;
        let rem = p & ((1u128 << sh) - 1);
        let half = 1u128 << (sh - 1);
        if rem > half || (rem == half && hi & 1 == 1) {
            hi = hi.wrapping_add(1);
            if hi == 0 {
                hi = 1 << 63;
                e += 1;
            }
        }
        m = hi;
        e += ce as i64 + sh as i64;
    }
    let low = m & 0x7FF;
    let mut mm = m >> 11;
    if low > 0x400 || (low == 0x400 && mm & 1 == 1) {
        mm += 1;
        if mm == 1 << 53 {
            mm >>= 1;
            e += 1;
        }
    }

    let biased = e + 11 + 52 + 1023;
    if biased <= 0 {
        return 0.0;
    }
    if biased >= 2047 {
        return f64::INFINITY;
    }
    f64::from_bits(((biased as u64) << 52) | (mm & ((1 << 52) - 1)))
}

const POS_LO: [(u64, i32); 15] = [
    (0xA000000000000000, -60),
    (0xC800000000000000, -57),
    (0xFA00000000000000, -54),
    (0x9C40000000000000, -50),
    (0xC350000000000000, -47),
    (0xF424000000000000, -44),
    (0x9896800000000000, -40),
    (0xBEBC200000000000, -37),
    (0xEE6B280000000000, -34),
    (0x9502F90000000000, -30),
    (0xBA43B74000000000, -27),
    (0xE8D4A51000000000, -24),
    (0x9184E72A00000000, -20),
    (0xB5E620F480000000, -17),
    (0xE35FA931A0000000, -14),
];
const NEG_LO: [(u64, i32); 15] = [
    (0xCCCCCCCCCCCCCCCD, -67),
    (0xA3D70A3D70A3D70A, -70),
    (0x83126E978D4FDF3B, -73),
    (0xD1B71758E219652C, -77),
    (0xA7C5AC471B478423, -80),
    (0x8637BD05AF6C69B6, -83),
    (0xD6BF94D5E57A42BC, -87),
    (0xABCC77118461CEFD, -90),
    (0x89705F4136B4A597, -93),
    (0xDBE6FECEBDEDD5BF, -97),
    (0xAFEBFF0BCB24AAFF, -100),
    (0x8CBCCC096F5088CC, -103),
    (0xE12E13424BB40E13, -107),
    (0xB424DC35095CD80F, -110),
    (0x901D7CF73AB0ACD9, -113),
];

const POS_MID: [(u64, i32); 16] = [
    (0x8E1BC9BF04000000, -10),
    (0x9DC5ADA82B70B59E, 43),
    (0xAF298D050E4395D7, 96),
    (0xC2781F49FFCFA6D5, 149),
    (0xD7E77A8F87DAF7FC, 202),
    (0xEFB3AB16C59B14A3, 255),
    (0x850FADC09923329E, 309),
    (0x93BA47C980E98CE0, 362),
    (0xA402B9C5A8D3A6E7, 415),
    (0xB616A12B7FE617AA, 468),
    (0xCA28A291859BBF93, 521),
    (0xE070F78D3927556B, 574),
    (0xF92E0C3537826146, 627),
    (0x8A5296FFE33CC930, 681),
    (0x9991A6F3D6BF1766, 734),
    (0xAA7EEBFB9DF9DE8E, 787),
];
const NEG_MID: [(u64, i32); 16] = [
    (0xE69594BEC44DE15B, -117),
    (0xCFB11EAD453994BA, -170),
    (0xBB127C53B17EC159, -223),
    (0xA87FEA27A539E9A5, -276),
    (0x97C560BA6B0919A6, -329),
    (0x88B402F7FD75539B, -382),
    (0xF64335BCF065D37D, -436),
    (0xDDD0467C64BCE4A1, -489),
    (0xC7CABA6E7C5382C9, -542),
    (0xB3F4E093DB73A093, -595),
    (0xA21727DB38CB0030, -648),
    (0x91FF83775423CC06, -701),
    (0x8380DEA93DA4BC60, -754),
    (0xECE53CEC4A314EBE, -808),
    (0xD5605FCDCF32E1D7, -861),
    (0xC0314325637A193A, -914),
];

fn digits15(x: f64) -> (String, i32) {
    if !x.is_finite() {
        return ("0".into(), 0);
    }
    let s = format!("{:.14e}", x.abs());
    let (mant, exp) = s.split_once('e').unwrap();
    let exp: i32 = exp.parse().unwrap();
    let mut digits: String = mant.chars().filter(|c| c.is_ascii_digit()).collect();
    while digits.len() > 1 && digits.ends_with('0') {
        digits.pop();
    }
    (digits, exp)
}

fn round_digits(digits: &str, n: usize) -> (String, bool) {
    let b = digits.as_bytes();
    let mut v: Vec<u8> = b[..n].to_vec();
    let mut carry = false;
    if b[n] >= b'5' {
        let mut i = n;
        loop {
            if i == 0 {
                v.insert(0, b'1');
                v.pop();
                carry = true;
                break;
            }
            i -= 1;
            if v[i] == b'9' {
                v[i] = b'0';
            } else {
                v[i] += 1;
                break;
            }
        }
    }
    let mut s = String::from_utf8(v).unwrap();
    while s.len() > 1 && s.ends_with('0') {
        s.pop();
    }
    (s, carry)
}

fn fixed_from_digits(digits: &str, exp: i32) -> String {
    let n = digits.len() as i32;
    if exp >= 0 {
        if exp + 1 >= n {
            let mut s = digits.to_string();
            s.extend(std::iter::repeat_n('0', (exp + 1 - n) as usize));
            s
        } else {
            let split = (exp + 1) as usize;
            format!("{}.{}", &digits[..split], &digits[split..])
        }
    } else {
        let zeros = (-exp - 1) as usize;
        let mut s = String::with_capacity(2 + zeros + digits.len());
        s.push_str("0.");
        s.extend(std::iter::repeat_n('0', zeros));
        s.push_str(digits);
        s
    }
}

fn scientific_from_digits(digits: &str, exp: i32) -> String {
    let mut s = String::with_capacity(digits.len() + 6);
    s.push_str(&digits[..1]);
    if digits.len() > 1 {
        s.push('.');
        s.push_str(&digits[1..]);
    }
    s.push('E');
    s.push(if exp < 0 { '-' } else { '+' });
    s.push_str(&format!("{:02}", exp.abs()));
    s
}

pub fn format_literal(x: f64) -> String {
    if x == 0.0 {
        return "0".to_string();
    }
    let (digits, exp) = digits15(x);
    let fixed = fixed_from_digits(&digits, exp);
    let body = if fixed.len() <= 21 {
        fixed
    } else {
        scientific_from_digits(&digits, exp)
    };
    if x < 0.0 {
        format!("-{body}")
    } else {
        body
    }
}

pub fn format_general(x: f64) -> String {
    if x == 0.0 {
        return "0".to_string();
    }
    let (mut digits, mut exp) = digits15(x);
    if exp.abs() >= 99 && digits.len() == 15 {
        let (d, carry) = round_digits(&digits, 14);
        digits = d;
        exp += carry as i32;
    }
    let fixed = fixed_from_digits(&digits, exp);
    let body = if fixed.len() <= 20 {
        fixed
    } else {
        scientific_from_digits(&digits, exp)
    };
    if x < 0.0 {
        format!("-{body}")
    } else {
        body
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn literals_truncate_to_15_digits() {
        assert_eq!(parse_literal("1234567890123456"), Some(1234567890123450.0));
        assert_eq!(
            parse_literal("0.99999999999999999"),
            Some(0.999999999999999)
        );
        assert_eq!(parse_literal("1E-16"), Some(1e-16));
        assert_eq!(parse_literal(".5"), Some(0.5));
        assert_eq!(parse_literal("1E-320"), None);
        assert_eq!(parse_literal("1E-309"), Some(0.0));
        assert_eq!(parse_literal("1E-310"), None);
        assert_eq!(parse_literal("0E308"), Some(0.0));
        assert_eq!(parse_literal("0E309"), None);
        assert_eq!(parse_literal("1E+309"), None);
        assert_eq!(parse_literal("1E+308"), None);
        assert_eq!(
            parse_literal("9.99999999999999E+307"),
            Some(9.99999999999999e307)
        );
        assert_eq!(parse_literal("2.2250738585072E-308"), Some(0.0));
        assert_eq!(parse_literal("1e"), None);
    }

    #[test]
    fn general_matches_excel() {
        let cases: &[(f64, &str)] = &[
            (1.0 / 3.0, "0.333333333333333"),
            (1e15, "1000000000000000"),
            (1e19, "10000000000000000000"),
            (1e20, "1E+20"),
            (1.23456789012345e20, "1.23456789012345E+20"),
            (1e-18, "0.000000000000000001"),
            (1.5e-18, "1.5E-18"),
            (1.5e-17, "0.000000000000000015"),
            (1.23456789012345e-5, "1.23456789012345E-05"),
            (0.000123456789012345, "0.000123456789012345"),
            (2f64.sqrt(), "1.4142135623731"),
            (200.0 / 3.0, "66.6666666666667"),
            (-0.5, "-0.5"),
            (1e100, "1E+100"),
        ];
        for (x, want) in cases {
            assert_eq!(format_general(*x), *want, "for {x:e}");
        }
    }

    #[test]
    fn snapping_and_comparison() {
        let a = 0.1 + 0.2;
        assert_eq!(snap_add(a, -0.3, a - 0.3), 0.0);
        let b = 1.0 + 2f64.powi(-49);
        assert_ne!(snap_add(b, -1.0, b - 1.0), 0.0);
        assert_eq!(cmp_numbers(1.0, 1.0 + 2f64.powi(-48)), Ordering::Equal);
        assert_eq!(cmp_numbers(1.0, 1.0 + 2f64.powi(-46)), Ordering::Less);
        assert_eq!(cmp_numbers(0.0, 2f64.powi(-1000)), Ordering::Less);
    }
}
