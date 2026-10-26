#[derive(Clone, Debug, PartialEq, Eq)]
pub struct Dec {
    pub digits: Vec<u8>,
    pub exp: i32,
}

impl Dec {
    pub fn zero() -> Dec {
        Dec {
            digits: Vec::new(),
            exp: 0,
        }
    }

    pub fn is_zero(&self) -> bool {
        self.digits.is_empty()
    }

    pub fn from_f64(x: f64) -> Dec {
        let x = x.abs();
        if x == 0.0 || !x.is_finite() {
            return Dec::zero();
        }

        let long = format!("{:.40e}", x);
        let (lm, le) = long.split_once('e').unwrap();

        let s = if lm.as_bytes()[16] == b'5' && lm[17..].bytes().all(|b| b == b'0') {
            format!("{}e{le}", &lm[..16])
        } else {
            format!("{:.14e}", x)
        };
        let (m, e) = s.split_once('e').unwrap();
        let exp: i32 = e.parse().unwrap();
        let mut digits: Vec<u8> = m
            .bytes()
            .filter(u8::is_ascii_digit)
            .map(|b| b - b'0')
            .collect();
        while digits.last() == Some(&0) {
            digits.pop();
        }
        Dec { digits, exp }
    }

    pub fn digit_at(&self, p: i32) -> u8 {
        let i = self.exp - p;
        if i < 0 || i as usize >= self.digits.len() {
            0
        } else {
            self.digits[i as usize]
        }
    }

    pub fn shifted(mut self, k: i32) -> Dec {
        if !self.is_zero() {
            self.exp += k;
        }
        self
    }

    pub fn round_to(&self, nd: i32) -> Dec {
        if self.is_zero() {
            return self.clone();
        }
        let keep = self.exp + nd + 1;
        if keep >= self.digits.len() as i32 {
            return self.clone();
        }
        if keep < 0 {
            return Dec::zero();
        }
        let keep = keep as usize;
        let up = self.digits[keep] >= 5;
        let mut digits = self.digits[..keep].to_vec();
        let mut exp = self.exp;
        if up {
            let mut i = digits.len();
            loop {
                if i == 0 {
                    digits.insert(0, 1);
                    exp += 1;
                    break;
                }
                i -= 1;
                if digits[i] == 9 {
                    digits[i] = 0;
                } else {
                    digits[i] += 1;
                    break;
                }
            }
        }
        while digits.last() == Some(&0) {
            digits.pop();
        }
        if digits.is_empty() {
            return Dec::zero();
        }
        Dec { digits, exp }
    }

    pub fn int_digits(&self) -> Vec<u8> {
        if self.is_zero() || self.exp < 0 {
            return Vec::new();
        }
        (0..=self.exp).rev().map(|p| self.digit_at(p)).collect()
    }

    pub fn frac_digits(&self, n: usize) -> Vec<u8> {
        (1..=n as i32).map(|p| self.digit_at(-p)).collect()
    }
}

pub fn digits_to_string(d: &[u8]) -> String {
    d.iter().map(|&x| (b'0' + x) as char).collect()
}

pub fn uint_digits(mut n: u64) -> Vec<u8> {
    if n == 0 {
        return vec![0];
    }
    let mut v = Vec::new();
    while n > 0 {
        v.push((n % 10) as u8);
        n /= 10;
    }
    v.reverse();
    v
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn rounding_is_decimal() {
        let d = Dec::from_f64(1.005).round_to(2);
        assert_eq!((d.digits.clone(), d.exp), (vec![1, 0, 1], 0));
        let d = Dec::from_f64(999.995).round_to(2);
        assert_eq!((d.digits.clone(), d.exp), (vec![1], 3));
        let d = Dec::from_f64(0.4).round_to(0);
        assert!(d.is_zero());
        let d = Dec::from_f64(0.6).round_to(0);
        assert_eq!((d.digits.clone(), d.exp), (vec![1], 0));
        let d = Dec::from_f64(1234.5).round_to(-2);
        assert_eq!((d.digits.clone(), d.exp), (vec![1, 2], 3));
    }
}
