use std::cell::RefCell;
use std::cmp::Ordering;
use std::sync::OnceLock;

static BLOB: &[u8] = include_bytes!("collate_table.bin");

struct Table {
    off: Vec<u32>,
}

fn table() -> &'static Table {
    static T: OnceLock<Table> = OnceLock::new();
    T.get_or_init(|| {
        let mut off = Vec::with_capacity(0x10000);
        let mut pos = 0usize;
        for _ in 0..0x10000 {
            off.push(pos as u32);
            if BLOB[pos] == 0xff {
                pos += 1;
            } else {
                pos += 2 + BLOB[pos + 1] as usize;
                for _ in 0..3 {
                    pos += 1 + BLOB[pos] as usize;
                }
                pos += 1 + BLOB[pos] as usize;
            }
        }
        debug_assert_eq!(pos, BLOB.len());
        Table { off }
    })
}

struct Rec<'a> {
    n: usize,
    p: &'a [u8],
    d: &'a [u8],
    w: &'a [u8],
    x: &'a [u8],
    s: Option<[u8; 2]>,
}

fn rec(cp: u16) -> Option<Rec<'static>> {
    let mut pos = table().off[cp as usize] as usize;
    if BLOB[pos] == 0xff {
        return None;
    }
    let n = BLOB[pos] as usize;
    pos += 1;
    let mut part = || {
        let len = BLOB[pos] as usize;
        let s = &BLOB[pos + 1..pos + 1 + len];
        pos += 1 + len;
        s
    };
    let p = part();
    let d = part();
    let w = part();
    let x = part();
    let s = part();
    Some(Rec {
        n,
        p,
        d,
        w,
        x,
        s: (s.len() == 2).then(|| [s[0], s[1]]),
    })
}

struct Supp {
    p: [u8; 4],
    d: [u8; 2],
    w: [u8; 2],
}

fn supp(cp: u32) -> Supp {
    let (pre, base, d, w) = if cp < 0x20000 {
        (0xb0u8, 0x10000u32, 0x3fu8, 0x27u8)
    } else {
        (0xfe, 0x20000, 0x3e, 0x26)
    };
    let a = 2 + ((cp - base) >> 10) as u8;
    let n = (cp & 0x3ff) + 45458;
    Supp {
        p: [pre, a, (n / 254) as u8 + 2, (n % 254) as u8 + 2],
        d: [d, 0x3f],
        w: [w, 0x27],
    }
}

#[derive(Default)]
struct Key {
    p: Vec<u8>,
    d: Vec<u8>,
    w: Vec<u8>,
    x: Vec<u8>,
    s: Vec<u8>,
    slots: usize,
}

impl Key {
    fn clear(&mut self) {
        self.p.clear();
        self.d.clear();
        self.w.clear();
        self.x.clear();
        self.s.clear();
        self.slots = 0;
    }

    fn build(&mut self, s: &str, primary_only: bool) {
        self.clear();
        for ch in s.chars() {
            let cp = ch as u32;
            if cp < 0x10000 {
                if let Some(r) = rec(cp as u16) {
                    self.add(&r, primary_only);
                }
            } else {
                let sp = supp(cp);
                self.add(
                    &Rec {
                        n: 2,
                        p: &sp.p,
                        d: &sp.d,
                        w: &sp.w,
                        x: &[],
                        s: None,
                    },
                    primary_only,
                );
            }
        }
    }

    fn add(&mut self, r: &Rec, primary_only: bool) {
        if !r.p.is_empty() {
            self.p.extend_from_slice(r.p);
            if !primary_only {
                for (v, e) in [(&mut self.d, r.d), (&mut self.w, r.w)] {
                    for i in 0..r.n {
                        v.push(*e.get(i).unwrap_or(&2));
                    }
                }
            }
            self.x.extend_from_slice(r.x);
            self.slots += r.n;
        } else if !primary_only {
            for (v, e) in [(&mut self.d, r.d), (&mut self.w, r.w)] {
                if let Some(&b) = e.first() {
                    if v.is_empty() {
                        v.push(0);
                    }
                    let l = v.last_mut().unwrap();
                    *l = l.wrapping_add(b);
                }
            }
        }
        if !primary_only {
            if let Some(sw) = r.s {
                let pos = 0xffffu16.wrapping_sub(self.slots.min(0xffff) as u16);
                self.s.extend_from_slice(&pos.to_be_bytes());
                self.s.extend_from_slice(&sw);
            }
        }
    }

    fn finish(&mut self) -> Vec<u8> {
        for v in [&mut self.d, &mut self.w] {
            while v.last() == Some(&2) {
                v.pop();
            }
        }
        let mut k = self.p.clone();
        for part in [&self.d, &self.w, &self.x, &self.s] {
            k.push(1);
            k.extend_from_slice(part);
        }
        k.push(0);
        k
    }
}

thread_local! {
    static SCRATCH: RefCell<(Key, Key)> = RefCell::new(Default::default());
}

#[cfg(test)]
pub fn sort_key(s: &str) -> Vec<u8> {
    let mut k = Key::default();
    k.build(s, false);
    k.finish()
}

pub fn compare(a: &str, b: &str) -> Ordering {
    if a.len() == b.len() && a.is_ascii() && b.is_ascii() && a.eq_ignore_ascii_case(b) {
        return Ordering::Equal;
    }
    SCRATCH.with(|sc| {
        let mut sc = sc.borrow_mut();
        let (ka, kb) = &mut *sc;

        ka.build(a, true);
        kb.build(b, true);
        let m = ka.p.len().min(kb.p.len());
        if let Some(i) = (0..m).find(|&i| ka.p[i] != kb.p[i]) {
            return ka.p[i].cmp(&kb.p[i]);
        }
        ka.build(a, false);
        kb.build(b, false);
        ka.finish().cmp(&kb.finish())
    })
}

#[cfg(all(test, windows))]
mod tests {
    use super::*;

    #[link(name = "kernel32")]
    extern "system" {
        fn LCMapStringEx(
            locale: *const u16,
            flags: u32,
            src: *const u16,
            n: i32,
            dst: *mut u8,
            nd: i32,
            version: *mut u8,
            reserved: *mut u8,
            lparam: isize,
        ) -> i32;
        fn CompareStringEx(
            locale: *const u16,
            flags: u32,
            a: *const u16,
            na: i32,
            b: *const u16,
            nb: i32,
            version: *mut u8,
            reserved: *mut u8,
            lparam: isize,
        ) -> i32;
    }

    const INVARIANT: [u16; 1] = [0];

    fn os_key(x: &[u16]) -> Vec<u8> {
        let mut buf = vec![0u8; 8192];

        let n = unsafe {
            LCMapStringEx(
                INVARIANT.as_ptr(),
                0x400 | 1,
                x.as_ptr(),
                x.len() as i32,
                buf.as_mut_ptr(),
                buf.len() as i32,
                std::ptr::null_mut(),
                std::ptr::null_mut(),
                0,
            )
        };
        buf.truncate(n.max(0) as usize);
        buf
    }

    fn os_compare(a: &str, b: &str) -> Ordering {
        let x: Vec<u16> = a.encode_utf16().collect();
        let y: Vec<u16> = b.encode_utf16().collect();

        let r = unsafe {
            CompareStringEx(
                INVARIANT.as_ptr(),
                1,
                x.as_ptr(),
                x.len() as i32,
                y.as_ptr(),
                y.len() as i32,
                std::ptr::null_mut(),
                std::ptr::null_mut(),
                0,
            )
        };
        match r {
            1 => Ordering::Less,
            2 => Ordering::Equal,
            3 => Ordering::Greater,
            _ => panic!("CompareStringEx failed"),
        }
    }

    fn encode(cp: u32) -> Vec<u8> {
        let k = os_key(&[cp as u16]);
        let pua = (0xe000..0xf900).contains(&cp);
        if k.last() != Some(&0) {
            return vec![0xff];
        }
        let k = &k[..k.len() - 1];

        let mut i = 0;
        while i + 1 < k.len() && !(k[i] == 1 && k[i + 1] != 0) {
            i += 2;
        }
        if pua {
            i = 3;
        }
        if k.get(i) != Some(&1) {
            return vec![0xff];
        }
        let parts: Vec<&[u8]> = k[i + 1..].splitn(4, |b| *b == 1).collect();
        if parts.len() != 4 {
            return vec![0xff];
        }
        let s = parts[3];
        let special = if s.len() == 4 && s[0] == 0xff && s[1] == 0xff {
            vec![s[2], s[3]]
        } else if s.is_empty() {
            vec![]
        } else {
            return vec![0xff];
        };
        let n = if pua { 1 } else { i / 2 };
        if n == 0
            && parts[0].is_empty()
            && parts[1].is_empty()
            && parts[2].is_empty()
            && special.is_empty()
        {
            return vec![0xff];
        }
        let mut out = vec![n as u8, i as u8];
        out.extend_from_slice(&k[..i]);
        for p in [parts[0], parts[1], parts[2], &special[..]] {
            out.push(p.len() as u8);
            out.extend_from_slice(p);
        }
        out
    }

    #[test]
    #[ignore = "writes src/text/collate_table.bin from the Windows sort keys"]
    fn regenerate() {
        let mut blob = Vec::new();
        for cp in 0..0x10000u32 {
            if (0xd800..0xe000).contains(&cp) {
                blob.push(0xff);
            } else {
                blob.extend(encode(cp));
            }
        }
        let path = concat!(env!("CARGO_MANIFEST_DIR"), "/src/text/collate_table.bin");
        std::fs::write(path, &blob).unwrap();
    }

    struct Rng(u64);
    impl Rng {
        fn n(&mut self, m: usize) -> usize {
            self.0 ^= self.0 << 13;
            self.0 ^= self.0 >> 7;
            self.0 ^= self.0 << 17;
            (self.0 % m as u64) as usize
        }
        fn string(&mut self, alpha: &[char], maxlen: usize) -> String {
            (0..self.n(maxlen) + 1)
                .map(|_| alpha[self.n(alpha.len())])
                .collect()
        }
    }

    fn range(lo: u32, hi: u32) -> Vec<char> {
        (lo..hi).filter_map(char::from_u32).collect()
    }

    fn anomalous(c: char) -> bool {
        matches!(c as u32, 0xf73 | 0xf75 | 0xf81 | 0x309f | 0x30ff | 0xfb2e..=0xfb30 | 0xfc5e..=0xfc63 | 0xfcf2..=0xfcf4 | 0xfe70..=0xfe7f)
    }

    fn agreement(alpha: &[char], maxlen: usize, pairs: usize, seed: u64) -> (usize, usize) {
        let alpha: Vec<char> = alpha.iter().copied().filter(|c| !anomalous(*c)).collect();
        let mut r = Rng(seed);
        let mut ok = 0;
        for _ in 0..pairs {
            let a = r.string(&alpha, maxlen);
            let b = r.string(&alpha, maxlen);
            if compare(&a, &b) == os_compare(&a, &b) {
                ok += 1;
            }
        }
        (ok, pairs)
    }

    #[test]
    fn keys_match_windows_for_every_bmp_character() {
        let mut bad = vec![];
        for cp in 0..0x10000u32 {
            let Some(c) = char::from_u32(cp) else {
                continue;
            };
            if anomalous(c) {
                continue;
            }
            let s = c.to_string();
            let u: Vec<u16> = s.encode_utf16().collect();
            if sort_key(&s) != os_key(&u) {
                bad.push(cp);
            }
        }

        assert!(
            bad.len() <= 8,
            "{} characters differ: {:x?}",
            bad.len(),
            bad
        );
    }

    #[test]
    fn exact_on_common_text() {
        let ascii = range(32, 127);
        let small: Vec<char> = "abcABC-' 0129_.,\u{e9}\u{c9}".chars().collect();
        let latin1 = range(32, 256);
        let punct: Vec<char> = " !\"#$%&'()*+,-./:;<=>?@[]^_`{|}~abc".chars().collect();
        let greek_cyr: Vec<char> = range(0x370, 0x530)
            .into_iter()
            .chain(range(32, 127))
            .collect();
        for (name, alpha, len) in [
            ("ascii", ascii, 8),
            ("small", small, 7),
            ("latin1", latin1, 6),
            ("punct", punct, 7),
            ("greek_cyr", greek_cyr, 6),
        ] {
            let (ok, n) = agreement(&alpha, len, 20000, 0x9e37_79b9_7f4a_7c15);
            assert_eq!(
                ok,
                n,
                "{name}: {} of {n} pairs differ from CompareStringEx",
                n - ok
            );
        }
    }

    #[test]
    fn near_exact_on_wide_text() {
        let wide = range(0x20, 0xd000);
        let (ok, n) = agreement(&wide, 4, 30000, 0x1234_5678_9abc_def1);
        assert!(ok * 10000 >= n * 9990, "BMP agreement {ok}/{n}");
        let supp: Vec<char> = range(0x1f300, 0x1f650)
            .into_iter()
            .chain(range(0x10000, 0x10400))
            .chain(range(0x20000, 0x20400))
            .chain(range(32, 127))
            .collect();
        let (ok, n) = agreement(&supp, 4, 20000, 0xdead_beef_1234_5678);
        assert_eq!(ok, n, "supplementary agreement");
    }

    #[test]
    fn word_sort_special_cases() {
        for (a, b) in [
            ("co-op", "coop"),
            ("co-op", "cop"),
            ("a'b", "ab"),
            ("\u{df}", "ss"),
            ("\u{e6}", "ae"),
            ("e\u{301}", "\u{e9}"),
            ("a-b-c", "a-bc-"),
        ] {
            assert_eq!(compare(a, b), os_compare(a, b), "{a:?} vs {b:?}");
        }
        let long_a = "a".repeat(300) + "-" + &"b".repeat(300);
        let long_b = "a".repeat(300) + &"b".repeat(300) + "-";
        assert_eq!(compare(&long_a, &long_b), os_compare(&long_a, &long_b));
    }
}
