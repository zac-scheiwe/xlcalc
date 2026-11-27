pub static TYPE_CLASS: [u8; 13] = [0, 1, 2, 4, 5, 3, 6, 7, 8, 9, 2, 0, 9];

pub static COMBINE_OTHER: [[u8; 10]; 10] = [
    [11, 1, 2, 11, 3, 4, 6, 7, 8, 9],
    [11, 1, 2, 1, 3, 4, 6, 7, 8, 9],
    [11, 11, 11, 2, 11, 11, 2, 2, 8, 9],
    [11, 11, 11, 5, 3, 4, 5, 7, 8, 9],
    [11, 11, 11, 11, 11, 11, 3, 3, 8, 9],
    [11, 11, 11, 11, 11, 11, 4, 4, 8, 9],
    [11, 11, 11, 11, 11, 11, 6, 7, 8, 9],
    [11, 11, 11, 11, 11, 11, 11, 7, 8, 9],
    [11, 11, 11, 11, 11, 11, 11, 11, 8, 9],
    [11, 11, 11, 11, 11, 11, 11, 11, 11, 9],
];

pub static COMBINE_ADDSUB: [[u8; 10]; 10] = [
    [11, 1, 2, 5, 3, 4, 6, 7, 8, 9],
    [11, 1, 2, 5, 3, 4, 1, 7, 8, 9],
    [11, 11, 2, 11, 11, 11, 2, 2, 8, 9],
    [11, 11, 11, 5, 11, 11, 5, 5, 8, 9],
    [11, 11, 11, 11, 11, 3, 11, 11, 8, 9],
    [11, 11, 11, 11, 11, 4, 11, 11, 8, 9],
    [11, 11, 11, 11, 11, 11, 6, 7, 8, 9],
    [11, 11, 11, 11, 11, 11, 11, 7, 8, 9],
    [11, 11, 11, 11, 11, 11, 11, 11, 8, 9],
    [11, 11, 11, 11, 11, 11, 11, 11, 11, 9],
];

pub fn combine(a: usize, b: usize, add_sub: bool) -> Option<u8> {
    let (ca, cb) = (*TYPE_CLASS.get(a)? as usize, *TYPE_CLASS.get(b)? as usize);
    let t = if add_sub {
        &COMBINE_ADDSUB
    } else {
        &COMBINE_OTHER
    };
    Some(t[ca.min(cb)][ca.max(cb)])
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn tables_read_from_exe() {
        assert_eq!(TYPE_CLASS[3], 4);
        assert_eq!(combine(1, 1, true), Some(1));
        assert_eq!(combine(0, 0, false), Some(11));
    }
}
