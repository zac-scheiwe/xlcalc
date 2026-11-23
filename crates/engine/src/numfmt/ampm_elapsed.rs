use super::parse::{DateTok, Item};

fn code_len(b: &[u8]) -> usize {
    let get = |i: usize| b.get(i).copied().unwrap_or(0) as usize;
    match b[0] {
        0x03 => 2 * get(1) + 2,
        0x02 | 0x04 | 0x2d => 3,
        0x05 | 0x09 | 0x0b | 0x1e | 0x1f | 0x26 | 0x27 | 0x2e | 0x2f | 0x30 | 0x31 | 0x32
        | 0x3a => 2,
        0x08 | 0x1d => 4,
        0x29 => 6,
        0x2b => 2 * get(1) + 2,
        0x2c => 10,
        0x22 | 0x23 => 2 * (get(1) + get(2)) + 3,
        0x41 => 2 * (get(1) + (get(2) << 8)) + 15,
        0x5f => 2 * (get(1) + get(2)) + 3,
        _ => 1,
    }
}

fn push_bare(out: &mut String, c: char) {
    if c == ' '
        || c == '-'
        || "()+!^&'~{}<>=`|".contains(c)
        || (c.is_ascii_alphabetic() && !CODE_LETTERS.contains(c))
    {
        out.push(c);
    } else {
        out.push('\\');
        out.push(c);
    }
}

fn decode(b: &[u8], marked: bool) -> Option<String> {
    let mut out = String::new();
    let mut p = 0;
    while p < b.len() {
        let n = code_len(&b[p..]);
        let arg = |k: usize| b.get(p + k).copied();
        match b[p] {
            0x00 => {}
            0x01 => out.push_str("General"),
            c @ 0x0d..=0x10 => out.push_str(&"d".repeat(usize::from(c) - 0x0c)),
            c @ 0x11..=0x14 => out.push_str(&"m".repeat(usize::from(c) - 0x10)),
            0x15 | 0x20 => out.push('h'),
            0x16 | 0x21 => out.push_str("hh"),

            0x17 => out.push_str(if marked { "\u{e001}" } else { "m" }),
            0x18 => out.push_str(if marked { "\u{e002}" } else { "mm" }),
            0x19 => out.push('s'),
            0x1a => out.push_str("ss"),
            0x1b => out.push_str("yy"),
            0x1c => out.push_str("yyyy"),
            0x24 => out.push('/'),
            0x25 => out.push(':'),
            0x38 => out.push('e'),
            0x39 => out.push_str("ee"),
            0x3b => out.push_str("mmmmm"),
            0x2f => {}
            0x2e => {
                out.push('.');
                out.push_str(&"0".repeat(usize::from(arg(1)?)));
            }
            u @ 0x30..=0x32 => {
                let unit = ['h', 'm', 's'][usize::from(u) - 0x30];
                out.push('[');
                out.push_str(&unit.to_string().repeat(usize::from(arg(1)?)));
                out.push(']');
            }
            0x02 => {
                let v = u32::from(arg(1)?) | u32::from(arg(2)?) << 8;
                push_bare(&mut out, char::from_u32(v)?);
            }
            0x2d => {
                let v = u32::from(arg(1)?) | u32::from(arg(2)?) << 8;
                out.push('_');
                out.push(char::from_u32(v)?);
            }
            0x03 if p + n <= b.len() => {
                out.push('"');
                for k in 0..usize::from(arg(1)?) {
                    let v = u32::from(arg(2 + 2 * k)?) | u32::from(arg(3 + 2 * k)?) << 8;
                    out.push(char::from_u32(v)?);
                }
                out.push('"');
            }

            0x22 if p + n <= b.len() => out.push_str(if (arg(1), arg(2)) == (Some(1), Some(1)) {
                "a/p"
            } else {
                "am/pm"
            }),

            0x2c if b.len() - p == 3 => {
                out.push_str("[0]");
                break;
            }

            0x61..=0x7f => {}

            0x22 | 0x23 => break,
            _ => return None,
        }
        p += n;
    }
    Some(out)
}

struct Sun {
    code: u8,
    w2: u16,
    class: u8,
    off: usize,
    len: usize,
}

struct Suns {
    suns: Vec<Sun>,

    pool: Vec<u8>,

    elapsed: bool,
}

impl Suns {
    fn add(&mut self, code: u8, w2: u16, class: u8, bytes: &[u8]) {
        let off = self.pool.len() - 1;
        self.pool.pop();
        self.pool.extend_from_slice(bytes);
        self.pool.push(0);
        self.suns.push(Sun {
            code,
            w2,
            class,
            off,
            len: bytes.len(),
        });
    }

    fn literal(&mut self, c: char) -> Option<()> {
        let v = u16::try_from(u32::from(c)).ok()?;
        self.add(2, 0, 3, &[0x02, v as u8, (v >> 8) as u8]);
        Some(())
    }
}

const CODE_LETTERS: &str = "abdeghmnsycABDEGHMNSYC";

fn build(sec: &str) -> Option<Suns> {
    let cs: Vec<char> = sec.chars().collect();
    let mut s = Suns {
        suns: Vec::new(),
        pool: vec![0],
        elapsed: false,
    };
    let mut i = 0;
    while i < cs.len() {
        let c = cs[i];
        let run = cs[i..].iter().take_while(|&&d| d == c).count();
        if cs.len() >= i + 5
            && cs[i..i + 5]
                .iter()
                .collect::<String>()
                .eq_ignore_ascii_case("am/pm")
        {
            let mut b = vec![0x22, 2, 2];
            b.extend([b'a', 0, b'm', 0, b'p', 0, b'm', 0]);
            s.add(0x22, 0, 5, &b);
            i += 5;
            continue;
        }
        match c {
            '[' => {
                let end = i + cs[i..].iter().position(|&d| d == ']')?;
                let inner = &cs[i + 1..end];
                let unit = *inner.first()?;
                let kind = "hms".find(unit)? as u16;
                if inner.iter().any(|&d| d != unit) || inner.len() > 255 {
                    return None;
                }
                s.elapsed = true;
                let w2 = (kind << 10) | 0x300 | inner.len() as u16;
                s.add(0x30 + kind as u8, w2, 5, &[]);
                i = end;
            }
            ':' => s.add(0x25, 0, 5, &[0x02, b':', 0]),
            '/' => s.add(0x24, 0, 5, &[0x24]),
            '.' => {
                let z = cs[i + 1..].iter().take_while(|&&d| d == '0').count();
                if z == 0 {
                    s.add(2, 0, 5, &[0x02, b'.', 0]);
                } else if z <= 3 {
                    s.add(0x2e, z as u16, 5, &[0x2e, z as u8]);
                    i += z;
                } else {
                    return None;
                }
            }
            '\\' => {
                i += 1;
                s.literal(*cs.get(i)?)?;
            }
            '_' => {
                i += 1;
                let v = u16::try_from(u32::from(*cs.get(i)?)).ok()?;
                s.add(3, 0, 3, &[0x2d, v as u8, (v >> 8) as u8]);
            }
            '"' => {
                let end = i + 1 + cs[i + 1..].iter().position(|&d| d == '"')?;
                let mut b = vec![0x03, u8::try_from(end - i - 1).ok()?];
                for &ch in &cs[i + 1..end] {
                    let v = u16::try_from(u32::from(ch)).ok()?;
                    b.extend([v as u8, (v >> 8) as u8]);
                }
                s.add(3, 0, 3, &b);
                i = end;
            }
            'd' if run <= 4 => {
                s.add(0x0c + run as u8, 0, 5, &[0x0c + run as u8]);
                i += run - 1;
            }
            'm' if run <= 5 => {
                let code = if run == 5 { 0x3b } else { 0x10 + run as u8 };
                s.add(code, 0, 5, &[code]);
                i += run - 1;
            }
            'y' if run <= 4 => {
                let code = if run <= 2 { 0x1b } else { 0x1c };
                s.add(code, 0, 5, &[code]);
                i += run - 1;
            }
            'h' if run <= 2 => {
                let code = 0x14 + run as u8;
                s.add(code, 0x200 | run as u16, 5, &[code]);
                i += run - 1;
            }
            's' if run <= 2 => {
                let code = 0x18 + run as u8;
                s.add(code, 0xa00 | run as u16, 5, &[code]);
                i += run - 1;
            }
            'e' if run <= 2 => {
                let code = 0x37 + run as u8;
                s.add(code, 0, 5, &[code]);
                i += run - 1;
            }
            'G' => {
                if !cs[i..].starts_with(&['G', 'e', 'n', 'e', 'r', 'a', 'l']) {
                    return None;
                }
                s.add(4, 0, 2, &[0x01]);
                i += 6;
            }
            ' ' | '-' | ',' | '(' | ')' | '+' | '!' | '^' | '&' | '\'' | '~' | '{' | '}' | '<'
            | '>' | '=' => s.literal(c)?,
            c if c.is_ascii_alphabetic() && !CODE_LETTERS.contains(c) => s.literal(c)?,
            _ => return None,
        }
        i += 1;
    }
    Some(s)
}

#[derive(Default)]
struct Walk {
    elapsed_seen: bool,

    prev_hour: bool,
    ampm_seen: bool,
    clock: i32,
    dates: i32,

    month: Option<usize>,

    hour: Option<usize>,

    rank: Option<u16>,
}

impl Walk {
    fn note_rank(&mut self, w2: u16) {
        let r = (w2 >> 10) & 3;
        if self.rank.is_none_or(|g| r < g) {
            self.rank = Some(r);
        }
    }

    fn seconds(&mut self, s: &mut Suns) {
        match self.month {
            Some(m) if !self.elapsed_seen => {
                self.dates -= 1;
                self.clock += 2;
                let o = s.suns[m].off;
                s.pool[o] += 6;
                s.suns[m].w2 = (s.suns[m].w2 & 0xf7ff) | 0x600;
                let w2 = s.suns[m].w2;
                self.note_rank(w2);
                self.month = None;
            }
            _ => {
                self.clock += 1;
                if self.rank.is_none() {
                    self.prev_hour = true;
                }
            }
        }
    }
}

fn resolve(s: &mut Suns) -> Option<Option<u16>> {
    let mut w = Walk::default();
    for k in 0..s.suns.len() {
        if s.suns[k].class != 5 {
            continue;
        }
        let code = s.suns[k].code;
        match code {
            0x30..=0x32 => {
                if w.elapsed_seen
                    || matches!(
                        (code, w.rank),
                        (0x30, Some(0)) | (0x31, Some(0 | 1)) | (0x32, Some(_))
                    )
                {
                    return None;
                }
                w.elapsed_seen = true;
                match code {
                    0x30 => {
                        w.clock += 1;
                        w.prev_hour = true;
                        w.hour = Some(k);
                        w.month = None;
                    }
                    0x31 => {
                        w.clock += 1;
                        w.prev_hour = false;
                    }
                    _ => w.seconds(s),
                }
            }
            0x15 | 0x16 => {
                w.clock += 1;
                w.prev_hour = true;
                w.hour = Some(k);
                w.month = None;
            }
            0x17 | 0x18 => {
                w.clock += 1;
                w.prev_hour = false;
            }
            0x19 | 0x1a | 0x2e => w.seconds(s),
            0x11 | 0x12 => {
                if w.prev_hour {
                    w.prev_hour = false;
                    let o = s.suns[k].off;
                    s.pool[o] += 6;
                    s.suns[k].w2 = (s.suns[k].w2 & 0xf7ff) | 0x600;
                    w.clock += 1;
                } else {
                    w.dates += 1;
                    w.month = Some(k);
                }
            }
            0x0d..=0x10 | 0x13 | 0x14 | 0x1b | 0x1c | 0x3b => {
                w.dates += 1;
                w.month = None;
            }
            0x37..=0x39 => w.dates += 1,
            0x22 => {
                if w.elapsed_seen {
                    return None;
                }
                if let Some(h) = w.hour.take() {
                    let o = s.suns[h].off;
                    s.pool[o] += 0x0b;
                }
                w.ampm_seen = true;
            }
            0x25 if w.dates > 0 || w.clock > 0 => {
                let o = s.suns[k].off;
                s.pool[o] = 0x25;
                s.suns[k].len = 1;
            }
            _ => {}
        }
        if s.suns[k].w2 & 0x200 != 0 {
            let w2 = s.suns[k].w2;
            w.note_rank(w2);
        }
    }
    if let (true, Some(h)) = (w.ampm_seen, w.hour) {
        let o = s.suns[h].off;
        s.pool[o] += 0x0b;
    }
    Some(w.rank)
}

fn emit(s: &Suns, rank: Option<u16>) -> Vec<u8> {
    let mut out = Vec::new();
    for u in &s.suns {
        let kind = ((u.w2 >> 10) & 3) as u8;
        if s.elapsed && u.class == 5 && u.w2 & 0x200 != 0 {
            if Some(u16::from(kind)) == rank {
                out.extend([0x30 + kind, u.w2 as u8]);
                continue;
            }
            if u.w2 & 0x100 != 0 {
                out.push(0x15 + kind * 2 + u8::from(u.w2 as u8 > 1));
                continue;
            }
        }
        out.extend_from_slice(&s.pool[u.off..u.off + u.len]);
    }
    out
}

pub(super) fn garble_section(sec: &str, marked: bool) -> Option<String> {
    if !sec.contains("am/pm") || !sec.contains('[') {
        return None;
    }
    let mut s = build(sec)?;
    if !s.elapsed {
        return None;
    }
    let rank = resolve(&mut s)?;
    let text = decode(&emit(&s, rank), marked)?;
    Some(if marked {
        format!("\u{e000}{text}")
    } else {
        text
    })
}

pub(super) fn resolve_items(items: &mut [Item]) -> Option<()> {
    let mut s = Suns {
        suns: Vec::new(),
        pool: vec![0],
        elapsed: false,
    };
    let mut at = Vec::new();
    for (k, it) in items.iter().enumerate() {
        let Item::Date(d) = it else { continue };
        let w = |n: u8| u16::from(n.clamp(1, 2));
        let c = |n: u8| n.clamp(1, 2);
        let (code, w2) = match *d {
            DateTok::Year(n) => (if n <= 2 { 0x1b } else { 0x1c }, 0),
            DateTok::Month(n) if n >= 5 => (0x3b, 0),
            DateTok::Month(n) => (0x10 + n, 0),
            DateTok::Day(n) => (0x0c + n, 0),
            DateTok::Hour(n) | DateTok::Hour12(n) => (0x14 + c(n), 0x200 | w(n)),
            DateTok::Minute(n) => (0x16 + c(n), 0x600 | w(n)),
            DateTok::Second(n) => (0x18 + c(n), 0xa00 | w(n)),
            DateTok::SubSec(n) => (0x2e, w(n)),
            DateTok::ElapsedH(n) => (0x30, 0x300 | w(n)),
            DateTok::ElapsedM(n) => (0x31, 0x700 | w(n)),
            DateTok::ElapsedS(n) => (0x32, 0xb00 | w(n)),
            DateTok::AmPm | DateTok::AmPmCjk => (0x22, 0),
            DateTok::EYear(_) | DateTok::Era(_) => (0x38, 0),
            DateTok::Buddhist(_) => (0x1b, 0),
            _ => continue,
        };
        s.elapsed |= (0x30..=0x32).contains(&code);
        let off = s.pool.len() - 1;
        s.pool.pop();
        s.pool.extend([code, 0]);
        s.suns.push(Sun {
            code,
            w2,
            class: 5,
            off,
            len: 1,
        });
        at.push(k);
    }
    resolve(&mut s)?;
    for (sun, &k) in s.suns.iter().zip(&at) {
        if let Item::Date(DateTok::Month(n)) = items[k] {
            if sun.w2 & 0x200 != 0 {
                items[k] = Item::Date(DateTok::Minute(n));
            }
        }
    }
    Some(())
}

#[cfg(test)]
mod tests {
    use super::garble_section as g0;

    fn g(sec: &str) -> Option<String> {
        g0(sec, false)
    }

    #[test]
    fn follower_shifts() {
        assert_eq!(
            g("am/pm [h]:mm").unwrap(),
            "am/pm [h][hhhhhhhhhhhhhhhhhhhhhhhh]"
        );
        assert_eq!(g("am/pm [h]x").unwrap(), "am/pm [h]d");
        assert_eq!(g("am/pm [h] mm").unwrap(), "am/pm [h]dhmm");
        assert_eq!(g("am/pm [h]/mm").unwrap(), "am/pm [h]");
        assert_eq!(g("am/pm [h]-mm").unwrap(), "am/pm [h]d_\u{1800}");
    }

    #[test]
    fn no_shift_cases() {
        assert_eq!(g("am/pm [m]:ss").unwrap(), "am/pm [m]:ss");
        assert_eq!(g("am/pm [h]").unwrap(), "am/pm [h]");
        assert_eq!(g("[h] am/pm"), None);
        assert_eq!(g("am/pm [h]:[h]"), None);
        assert_eq!(g("h:mm am/pm"), None);
    }

    #[test]
    fn same_rank_clock_field_reads_as_elapsed() {
        assert_eq!(g("am/pm [h] h").unwrap(), "am/pm [h] [h]");
        assert_eq!(g("am/pm h [h]"), None);
    }
}
