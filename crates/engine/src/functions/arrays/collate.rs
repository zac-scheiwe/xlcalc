use crate::text::{collate_compare, collate_compare_match_case, collate_key};
use std::cmp::Ordering;

pub fn compare(a: &str, b: &str) -> Ordering {
    if a == b {
        return Ordering::Equal;
    }
    collate_compare(a, b)
}

pub fn compare_match_case(a: &str, b: &str) -> Ordering {
    collate_compare_match_case(a, b)
}

pub fn equal(a: &str, b: &str) -> bool {
    compare(a, b) == Ordering::Equal
}

pub fn key(s: &str) -> Vec<u32> {
    collate_key(s).into_iter().map(u32::from).collect()
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn measured_orders() {
        let mut v = vec![
            "ab", "a-b", "a-c", "aa", "a b", "a'b", "_a", "-a", "1a", "Ab", "ä", "b", "é", "f", "e",
        ];
        v.sort_by(|a, b| compare(a, b));
        assert_eq!(
            v,
            [
                "_a", "1a", "-a", "ä", "a b", "aa", "ab", "Ab", "a'b", "a-b", "a-c", "b", "e", "é",
                "f"
            ]
        );
        assert!(equal("ß", "ss") && equal("Æ", "ae") && equal("a\u{AD}b", "ab"));
        assert!(!equal("é", "e") && !equal("a-b", "ab"));
        assert_eq!(compare("10", "9"), Ordering::Less);
        assert_eq!(compare("a b", "a\tb"), Ordering::Less);
    }
}
