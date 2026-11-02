use crate::HResult;

pub(super) struct Parsed {
    pub digits: Vec<u8>,
    pub pwr10: i64,

    pub shift: u32,

    pub neg: bool,

    pub inexact: bool,
}

fn is_space(c: u16) -> bool {
    matches!(
        c,
        0x09..=0x0D
            | 0x20
            | 0x85
            | 0xA0
            | 0x1680
            | 0x180E
            | 0x2000..=0x200A
            | 0x2028
            | 0x2029
            | 0x202F
            | 0x205F
            | 0x3000
    )
}

fn is_currency(c: u16) -> bool {
    matches!(c, 0x24 | 0xFF04 | 0x2052)
}

fn digit(c: u16) -> Option<u8> {
    (0x30..=0x39).contains(&c).then(|| (c - 0x30) as u8)
}

struct Cursor {
    units: Vec<u16>,
    at: usize,
}

impl Cursor {
    fn next(&mut self) -> u16 {
        let c = self.units[self.at];
        self.at += 1;
        c
    }
}

pub(super) fn parse(s: &str, max_digits: usize) -> Result<Parsed, HResult> {
    let mismatch = Err(HResult::DISP_E_TYPEMISMATCH);
    let mut units: Vec<u16> = s.encode_utf16().take_while(|&c| c != 0).collect();
    units.push(0);
    let mut cur = Cursor { units, at: 0 };
    let mut c = cur.next();
    while is_space(c) {
        c = cur.next();
    }
    if c == u16::from(b'&') {
        return radix(&mut cur, max_digits);
    }
    let (mut sign_seen, mut neg, mut paren, mut currency) = (false, false, false, false);
    let mut decimal = false;

    let mut step = 0;
    loop {
        if digit(c).is_some() {
            break;
        }
        match c {
            _ if is_space(c) => {}
            0x2B | 0x2D | 0x28 => {
                if sign_seen {
                    return mismatch;
                }
                sign_seen = true;
                neg = c != 0x2B;
                paren = c == 0x28;
            }
            0x2E => {
                c = cur.next();
                if digit(c).is_none() {
                    return mismatch;
                }
                decimal = true;
                step = -1;
                break;
            }
            _ if !currency && is_currency(c) => currency = true,
            _ => return mismatch,
        }
        c = cur.next();
    }
    let mut digits = Vec::new();
    let mut pwr10: i64 = 0;
    let mut inexact = false;
    let mut leading = true;
    loop {
        if let Some(d) = digit(c) {
            if leading && d == 0 {
                pwr10 += step;
            } else if digits.len() >= max_digits {
                leading = false;
                inexact |= d != 0;
                pwr10 += 1 + step;
            } else {
                leading = false;
                digits.push(d);
                pwr10 += step;
            }
        } else if c == 0x2C {
        } else if c == 0x2E {
            if decimal {
                break;
            }
            decimal = true;
            step = -1;
            leading = digits.is_empty();
        } else if matches!(c, 0x44 | 0x45 | 0x64 | 0x65) {
            c = cur.next();
            let negative = c == 0x2D;
            if negative || c == 0x2B {
                c = cur.next();
            }
            let Some(d) = digit(c) else {
                return mismatch;
            };
            let mut e = i64::from(d);
            c = cur.next();
            while let Some(d) = digit(c) {
                if e >= 0x666_6666 {
                    return mismatch;
                }
                e = e * 10 + i64::from(d);
                c = cur.next();
            }
            pwr10 += if negative { -e } else { e };
            break;
        } else {
            break;
        }
        c = cur.next();
    }
    loop {
        match c {
            0 => break,
            _ if is_space(c) => {}
            0x29 if paren => paren = false,
            0x2B | 0x2D if !sign_seen => {
                sign_seen = true;
                neg = c == 0x2D;
            }
            _ if is_currency(c) => {}
            _ => return mismatch,
        }
        c = cur.next();
    }
    if paren {
        return mismatch;
    }
    while digits.len() > 1 && digits.last() == Some(&0) {
        digits.pop();
        pwr10 += 1;
    }
    if digits.is_empty() {
        digits.push(0);
    }
    Ok(Parsed {
        digits,
        pwr10,
        shift: 0,
        neg,
        inexact,
    })
}

fn radix(cur: &mut Cursor, max_digits: usize) -> Result<Parsed, HResult> {
    let mismatch = Err(HResult::DISP_E_TYPEMISMATCH);
    let mut c = cur.next();
    let (shift, max) = match c {
        0x48 | 0x68 => (4, 15),
        _ => (3, 7),
    };
    if matches!(c, 0x48 | 0x68 | 0x4F | 0x6F) {
        c = cur.next();
    }
    let first = c;
    while c == 0x30 {
        c = cur.next();
    }
    let mut digits = Vec::new();
    loop {
        let d = match c {
            0x30..=0x39 => c - 0x30,
            0x41..=0x46 => c - 0x37,
            0x61..=0x66 => c - 0x57,
            _ => break,
        };
        if d > max {
            break;
        }
        if digits.len() >= max_digits {
            return mismatch;
        }
        digits.push(d as u8);
        c = cur.next();
    }
    if digits.is_empty() {
        if first != 0x30 {
            return mismatch;
        }
        digits.push(0);
    }
    while is_space(c) {
        c = cur.next();
    }
    if c != 0 {
        return mismatch;
    }
    Ok(Parsed {
        digits,
        pwr10: 0,
        shift,
        neg: false,
        inexact: false,
    })
}
