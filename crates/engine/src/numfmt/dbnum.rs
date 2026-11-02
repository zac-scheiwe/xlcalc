#[derive(Clone, Copy, Debug, PartialEq, Eq)]
enum Fam {
    S,

    T,

    J,

    K,

    H,
}

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub(crate) struct Db {
    n: u8,
    fam: Fam,
}

const FULLWIDTH: &str = "０１２３４５６７８９";

impl Db {
    pub(crate) fn new(n: u8, lang: u32) -> Option<Db> {
        let fam = match lang {
            0x804 | 0x1004 => Fam::S,
            0x404 | 0xC04 | 0x1404 => Fam::T,
            0x411 => Fam::J,
            0x412 => Fam::K,
            _ => return None,
        };
        (1..=3).contains(&n).then_some(Db { n, fam })
    }

    pub(crate) fn from_digit_tag(nn: u8) -> Option<Db> {
        let (fam, n) = match nn {
            0x1B..=0x1D => (Fam::J, nn - 0x1A),
            0x1E..=0x20 => (Fam::S, nn - 0x1D),
            0x21..=0x23 => (Fam::T, nn - 0x20),
            0x24..=0x26 => (Fam::K, nn - 0x23),
            0x27 => (Fam::H, 1),
            _ => return None,
        };
        Some(Db { n, fam })
    }

    fn digits(self) -> [char; 10] {
        let t = match (self.n, self.fam) {
            (_, Fam::H) => "영일이삼사오육칠팔구",
            (1, Fam::J) => "〇一二三四五六七八九",

            (1, Fam::K) => "０一二三四五\u{f9d1}七八九",
            (1, _) => "○一二三四五六七八九",
            (2, Fam::S) => "零壹贰叁肆伍陆柒捌玖",
            (2, Fam::T) => "零壹貳參肆伍陸柒捌玖",
            (2, Fam::J) => "〇壱弐参四伍六七八九",
            (2, Fam::K) => "\u{f9b2}壹貳\u{f96b}四伍\u{f9d1}七八九",
            _ => FULLWIDTH,
        };
        let mut d = ['0'; 10];
        for (c, o) in t.chars().zip(d.iter_mut()) {
            *o = c;
        }
        d
    }

    pub(crate) fn positional(self, s: &str) -> String {
        let d = self.digits();
        s.chars()
            .map(|c| {
                if c.is_ascii_digit() {
                    d[c as usize - '0' as usize]
                } else {
                    c
                }
            })
            .collect()
    }

    pub(crate) fn field(self, t: &str) -> String {
        let b = t.as_bytes();
        if b.len() != 2 || b[0] == b'0' || self.n == 3 {
            return self.positional(t);
        }
        let d = self.digits();
        let digit = |c: u8| d[(c - b'0') as usize];
        let mut out = String::new();
        if b[0] != b'1' || self.n == 2 {
            out.push(digit(b[0]));
        }
        out.push(self.unit10(1));
        if b[1] != b'0' {
            out.push(digit(b[1]));
        }
        out
    }

    fn unit10(self, pos: usize) -> char {
        match (self.n, self.fam, pos) {
            (_, Fam::H, 1) => '십',
            (_, Fam::H, 2) => '백',
            (_, Fam::H, _) => '천',
            (2, Fam::S | Fam::T, 1) => '拾',
            (2, Fam::S | Fam::T, 2) => '佰',
            (2, Fam::S | Fam::T, _) => '仟',
            (2, Fam::K, 1) => '\u{f973}',
            (2, _, 1) => '拾',
            (2, _, 2) => '百',
            (2, _, _) => '阡',
            (_, _, 1) => '十',
            (_, _, 2) => '百',
            _ => '千',
        }
    }

    fn wan(self) -> char {
        match self.fam {
            Fam::H => '만',
            Fam::S => '万',
            Fam::T => '萬',
            _ if self.n == 2 => '萬',
            _ => '万',
        }
    }

    fn yi(self) -> char {
        if self.fam == Fam::H {
            '억'
        } else if self.fam == Fam::S {
            '亿'
        } else {
            '億'
        }
    }

    fn zhao(self) -> char {
        if self.fam == Fam::H {
            '조'
        } else {
            '兆'
        }
    }

    fn writes_zero(self) -> bool {
        matches!(self.fam, Fam::S | Fam::T)
    }

    fn omits_one(self) -> bool {
        (self.n == 1 && self.fam == Fam::J) || (self.n == 3 && matches!(self.fam, Fam::J | Fam::K))
    }

    pub(crate) fn words_int(self, digits: &str) -> String {
        let d = self.digits();
        let s = digits.trim_start_matches('0');
        if s.is_empty() {
            return d[0].to_string();
        }
        let mut out = String::new();

        let (top, rest) = if s.len() > 12 {
            s.split_at(s.len() - 12)
        } else {
            ("", s)
        };
        let mut groups: Vec<(String, Option<char>)> = Vec::new();
        if top.len() > 4 {
            out.push_str(&self.positional(top));
            out.push(self.zhao());
        } else if !top.is_empty() {
            groups.push((format!("{top:0>4}"), Some(self.zhao())));
        }
        let rest = format!("{rest:0>12}");
        groups.push((rest[0..4].to_string(), Some(self.yi())));
        groups.push((rest[4..8].to_string(), Some(self.wan())));
        groups.push((rest[8..12].to_string(), None));
        let (mut any, mut pending) = (!out.is_empty(), false);
        for (g, unit) in groups {
            let mut nonzero = false;
            for (i, c) in g.bytes().enumerate() {
                let pos = 3 - i;
                let v = (c - b'0') as usize;
                if v == 0 {
                    if any {
                        pending = true;
                    }
                    continue;
                }
                if pending && self.writes_zero() {
                    out.push(d[0]);
                }
                pending = false;
                if !(v == 1 && pos > 0 && self.omits_one()) {
                    out.push(d[v]);
                }
                if pos > 0 {
                    out.push(self.unit10(pos));
                }
                nonzero = true;
                any = true;
            }
            if nonzero {
                pending = false;
                out.extend(unit);
            }
        }
        out
    }

    pub(crate) fn words(self, v: f64) -> String {
        let e = format!("{:.14e}", v.abs());
        let (m, x) = e.split_once('e').unwrap_or((&e, "0"));
        let m = if m.contains('.') {
            m.trim_end_matches('0').trim_end_matches('.')
        } else {
            m
        };
        let t = expand_exponent(&format!("{m}E{x}"));
        let (int, frac) = t
            .split_once('.')
            .map_or((t.as_str(), None), |(a, b)| (a, Some(b)));
        let mut out = String::new();
        if v < 0.0 {
            out.push('-');
        }
        out.push_str(&self.words_int(int));
        if let Some(f) = frac {
            out.push('.');
            out.push_str(&self.positional(f));
        }
        out
    }
}

fn expand_exponent(text: &str) -> String {
    let Some((m, e)) = text.split_once('E') else {
        return text.to_string();
    };
    let Ok(e) = e.parse::<i32>() else {
        return text.to_string();
    };
    let (sign, m) = match m.strip_prefix('-') {
        Some(b) => ("-", b),
        None => ("", m),
    };
    let (ip, fp) = m.split_once('.').unwrap_or((m, ""));
    let digits = format!("{ip}{fp}");
    let point = ip.len() as i32 + e;
    let body = if point <= 0 {
        format!("0.{}{}", "0".repeat((-point) as usize), digits)
    } else if point as usize >= digits.len() {
        format!("{}{}", digits, "0".repeat(point as usize - digits.len()))
    } else {
        format!(
            "{}.{}",
            &digits[..point as usize],
            &digits[point as usize..]
        )
    };
    format!("{sign}{body}")
}
