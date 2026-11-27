use crate::legacy_ftab::{Ftab, FTAB};
use std::sync::OnceLock;

pub(crate) fn ftab(name: &str) -> Option<&'static Ftab> {
    static BY_NAME: OnceLock<std::collections::HashMap<&'static str, usize>> = OnceLock::new();
    let m = BY_NAME.get_or_init(|| {
        let mut m = std::collections::HashMap::new();
        for (i, f) in FTAB.iter().enumerate() {
            if !f.name.is_empty() && f.ifunc != 255 {
                m.entry(f.name).or_insert(i);
            }
        }
        m
    });
    m.get(name).map(|&i| &FTAB[i])
}

pub(crate) fn argc_ok(f: &Ftab, n: usize) -> bool {
    let min = (f.arity & 0xff) as usize;
    if n < min || n >= usize::from(f.argc_max) {
        return false;
    }
    let group = ((f.arity >> 8) & 0xf) as usize;
    let unbound = ((f.arity >> 12) & 0xf) as usize + min;
    let trailing = ((f.arity >> 18) & 0xf) as usize;
    group < 2 || n <= unbound || (n - unbound) % group <= trailing
}

pub(crate) fn can_return_multi(f: &Ftab) -> bool {
    f.ifunc != 0x261 && (f.flags & 0x60648 != 0 || matches!(f.ifunc, 0x24c | 0x24d))
}

pub(crate) fn int_flags(n: f64) -> u8 {
    if n.fract() == 0.0 && (0.0..=65535.0).contains(&n) && !(n == 0.0 && n.is_sign_negative()) {
        match n as u32 {
            0 => 0,
            1 => 0xc,
            _ => 4,
        }
    } else {
        0
    }
}

pub(crate) fn call_flags(f: &Ftab, a: &[u8], one_d: bool) -> u8 {
    let n = a.len();
    let any3 = || a.iter().any(|x| x & 3 != 0);
    let mut r = 0u8;
    match f.ifunc {
        0x167 | 0x1e0 | 0x24c | 0x24d | 0x24e => return u8::from(any3()),
        8 | 9 => return u8::from(n == 1 && any3()),
        0x261 | 0x264 => return 0,
        0x263 => return u8::from(any3()) | 2,

        0xff => return 3,
        0x65 | 0x66 => {
            return u8::from(a.iter().enumerate().any(|(k, x)| k != 1 && x & 3 != 0));
        }
        1 | 100 => {
            let mut multi = false;
            for k in (0..n).rev() {
                let x = a[k];
                if x & 3 == 0 || k != 0 {
                    r |= x & 3;
                    multi |= x & 2 != 0;
                } else {
                    r |= 1;
                    if multi {
                        r |= 0x11;
                    }
                }
            }
            return r;
        }
        0x1d => {
            let (mut row_open, mut col_open) = (true, true);
            for k in (0..n).rev() {
                let x = a[k];
                if k == 0 || x & 3 == 0 {
                    match k {
                        1 if x & 4 != 0 => row_open = false,
                        2 if x & 4 != 0 => col_open = false,
                        0 => {
                            let keep = row_open || if n < 3 { x & 1 != 0 } else { col_open };
                            if keep {
                                r |= x & 3;
                            }
                        }
                        _ => {}
                    }
                } else {
                    r |= 1;
                }
            }
            return r;
        }

        0x4e => {
            let (mut h1, mut w1) = (false, false);
            for k in (0..n).rev() {
                let x = a[k];
                if k == 0 || x & 3 == 0 {
                    match k {
                        3 => {
                            if x & 8 != 0 {
                                h1 = true;
                            } else {
                                r |= 2;
                            }
                        }
                        4 => {
                            if x & 8 != 0 {
                                w1 = true;
                            } else {
                                r |= 2;
                            }
                        }
                        0 => {
                            if x & 1 != 0 {
                                r |= 1;
                            }
                            if !h1 || !w1 {
                                r |= x & 3;
                            }
                        }
                        _ => {}
                    }
                } else {
                    r |= 1;
                }
            }
            return r;
        }
        0x275 => {
            for k in (0..n).rev() {
                let x = a[k];
                if (!one_d && k == 2) || k == 3 {
                    r |= x & 3;
                } else if x & 3 != 0 && (k == 0 || k >= 4) {
                    r |= 1;
                }
            }
            return r;
        }
        _ => {}
    }
    if f.flags & 0x60608 != 0 {
        r |= 2;
    }
    if f.flags & 0x40 != 0 {
        return r | 1;
    }
    if f.ifunc == 0x40 {
        r |= 4;
    }
    let last = f.masks.len().saturating_sub(1);
    for (k, x) in a.iter().enumerate() {
        let m = f.masks.get(k.min(last)).copied().unwrap_or(0);
        if x & 1 != 0 && m & 0x40 == 0 {
            r |= 1;
        }
        if x & 2 != 0 && m & 0x60648 == 0 {
            r |= 1;
        }
    }
    r
}

pub(crate) fn operator_flags(a: &[u8]) -> u8 {
    u8::from(a.iter().any(|x| x & 3 != 0))
}

pub(crate) fn range_flags(l: u8, r: u8, intersect: bool) -> u8 {
    let lift = (l | r) & 1;
    if intersect {
        lift | if l & 2 != 0 && r & 2 != 0 { 2 } else { 0 }
    } else {
        lift | 2
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    fn call(name: &str, a: &[u8]) -> u8 {
        call_flags(ftab(name).unwrap(), a, false)
    }

    #[test]
    fn argc_rule() {
        let ok = |n: &str, c: usize| argc_ok(ftab(n).unwrap(), c);
        assert!(ok("COUNTIFS", 4) && !ok("COUNTIFS", 3) && !ok("COUNTIFS", 1));
        assert!(ok("SUMIFS", 3) && !ok("SUMIFS", 4) && ok("SUMIFS", 5));
        assert!(ok("IFS", 2) && !ok("IFS", 3) && !ok("IFS", 255));
        assert!(ok("SWITCH", 4) && ok("SWITCH", 5) && !ok("SWITCH", 255));
        assert!(ok("SORTBY", 3) && !ok("SORTBY", 4) && ok("SORTBY", 5));
        assert!(ok("SUM", 255) && !ok("SUM", 0) && !ok("CONCAT", 255) && ok("CONCAT", 254));
    }

    #[test]
    fn defs_match_table() {
        for n in crate::functions::names() {
            let d = crate::functions::def(crate::functions::lookup(n).unwrap());
            let Some(f) = ftab(&n.to_uppercase()) else {
                continue;
            };
            if n.eq_ignore_ascii_case("IF") {
                continue;
            }
            let (min, max) = ((f.arity & 0xff) as u8, (f.argc_max - 1).min(255) as u8);
            assert_eq!((d.min_args, d.max_args), (min, max), "{n}");
        }
    }

    #[test]
    fn table_rows() {
        assert!(can_return_multi(ftab("INDEX").unwrap()));
        assert!(!can_return_multi(ftab("ABS").unwrap()));
        assert!(!can_return_multi(ftab("SINGLE").unwrap()));
        assert_eq!(int_flags(1.0), 0xc);
        assert_eq!(int_flags(7.0), 4);
        assert_eq!(int_flags(0.0), 0);
        assert_eq!(int_flags(1.5), 0);
    }

    #[test]
    fn index_keeps_bare_for_written_indexes() {
        assert_eq!(call("INDEX", &[2, 0xc, 0xc, 0]), 0);
        assert_eq!(call("INDEX", &[2, 4, 4]), 0);
        assert_eq!(call("INDEX", &[2, 0xc]), 0);

        assert_eq!(call("INDEX", &[2, 0]), 2);
        assert_eq!(call("INDEX", &[2, 0xc, 0]), 2);

        assert_eq!(call("INDEX", &[2, 0xc, 0xc, 2]) & 1, 1);
    }

    #[test]
    fn if_and_lifting() {
        assert_eq!(call("IF", &[0, 2, 0xc]) & 3, 2);
        assert_eq!(call("ABS", &[1]) & 3, 1);
        assert_eq!(call("SUM", &[2]), 0);
    }
}
