use crate::legacy_grbit::{Grbit, GRBIT};

fn entry(name: &str) -> Option<&'static Grbit> {
    GRBIT
        .binary_search_by(|e| e.name.cmp(name))
        .ok()
        .map(|i| &GRBIT[i])
}

pub(crate) fn grbit_at(name: &str, i: usize) -> Option<u32> {
    let e = entry(name)?;
    let m = e.head.len();
    let p = usize::from(e.period);
    Some(if i < m {
        e.head[i]
    } else {
        e.head[m - p + (i - m) % p]
    })
}

pub(crate) fn class_of_grbit(g: u32) -> u8 {
    if g & 0x17 == 0 {
        if g & 0x40 != 0 {
            b'A'
        } else if g & 8 != 0 {
            b'Q'
        } else {
            b'V'
        }
    } else if g & 8 != 0 {
        b'R'
    } else {
        b'V'
    }
}

pub(crate) fn class_of(name: &str, i: usize) -> Option<u8> {
    class_of_n(name, i, usize::MAX)
}

pub(crate) fn mask_n(name: &str, i: usize, argc: usize) -> Option<u32> {
    let g = grbit_at(name, i)?;
    Some(if name == "SWITCH" && i % 2 == 1 && i + 1 < argc {
        g & !8
    } else {
        g
    })
}

pub(crate) fn class_of_n(name: &str, i: usize, argc: usize) -> Option<u8> {
    mask_n(name, i, argc).map(class_of_grbit)
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn mask_branches() {
        assert_eq!(class_of_grbit(0x1), b'V');
        assert_eq!(class_of_grbit(0x9), b'R');
        assert_eq!(class_of_grbit(0x8), b'Q');
        assert_eq!(class_of_grbit(0x140), b'A');
        assert_eq!(class_of_grbit(0x48), b'A');
        assert_eq!(class_of_grbit(0x7f), b'R');
        assert_eq!(class_of_grbit(0x54), b'V');
    }

    #[test]
    fn per_function() {
        let s = |n: &str, k: usize| -> String {
            (0..k).map(|i| class_of(n, i).unwrap() as char).collect()
        };
        assert_eq!(s("IF", 3), "VRR");
        assert_eq!(s("SUMIF", 3), "QVQ");
        assert_eq!(s("VLOOKUP", 4), "VRRV");
        assert_eq!(s("SUMIFS", 7), "QQVQVQV");
        assert_eq!(s("COUNTIFS", 6), "QVQVQV");
        assert_eq!(s("SORTBY", 6), "AAVAVA");
        assert_eq!(s("AGGREGATE", 6), "VVARQQ");
        assert_eq!(s("SWITCH", 7), "VVRVRVR");

        assert_eq!(class_of_n("SWITCH", 3, 4), Some(b'R'));
        assert_eq!(class_of_n("SWITCH", 3, 5), Some(b'V'));
        assert_eq!(class_of("NOSUCHFN", 0), None);
    }
}
