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

fn push_literal(code: &mut Vec<u8>, c: char) -> Option<()> {
    let v = u16::try_from(u32::from(c)).ok()?;
    code.extend([0x02, v as u8, (v >> 8) as u8]);
    Some(())
}

fn encode_follower(s: &str) -> Option<Vec<u8>> {
    let cs: Vec<char> = s.chars().collect();
    let mut code = Vec::new();
    let mut minute_seen = false;
    let mut i = 0;
    while i < cs.len() {
        let c = cs[i];
        let run = cs[i..].iter().take_while(|&&d| d == c).count();
        match c {
            ':' => code.push(0x25),
            '/' => code.push(0x24),
            ' ' | '-' | ',' | '(' | ')' | '+' | '!' | '^' | '&' | '\'' | '~' | '{' | '}' | '<'
            | '>' | '=' => {
                push_literal(&mut code, c)?;
            }
            '.' => {
                let z = cs[i + 1..].iter().take_while(|&&d| d == '0').count();
                if z == 0 {
                    push_literal(&mut code, '.')?;
                } else {
                    code.extend([0x2e, u8::try_from(z).ok()?]);
                    i += z;
                }
            }
            '\\' => {
                i += 1;
                push_literal(&mut code, *cs.get(i)?)?;
            }
            '_' => {
                i += 1;
                let v = u16::try_from(u32::from(*cs.get(i)?)).ok()?;
                code.extend([0x2d, v as u8, (v >> 8) as u8]);
            }
            '"' => {
                let end = i + 1 + cs[i + 1..].iter().position(|&d| d == '"')?;
                let body = &cs[i + 1..end];
                code.push(0x03);
                code.push(u8::try_from(body.len()).ok()?);
                for &b in body {
                    let v = u16::try_from(u32::from(b)).ok()?;
                    code.extend([v as u8, (v >> 8) as u8]);
                }
                i = end;
            }
            'd' if run <= 4 => {
                code.push(0x0c + run as u8);
                i += run - 1;
            }
            'm' if run <= 2 => {
                if minute_seen {
                    code.push(0x10 + run as u8);
                } else {
                    code.push(0x16 + run as u8);
                    minute_seen = true;
                }
                i += run - 1;
            }
            'm' if run <= 5 => {
                code.push(if run == 5 { 0x3b } else { 0x10 + run as u8 });
                i += run - 1;
            }
            's' if run <= 2 => {
                code.push(0x18 + run as u8);
                i += run - 1;
            }
            'y' if run <= 4 => {
                code.push(if run <= 2 { 0x1b } else { 0x1c });
                i += run - 1;
            }
            'e' if run <= 2 => {
                code.push(0x37 + run as u8);
                i += run - 1;
            }
            'G' => {
                if !cs[i..].starts_with(&['G', 'e', 'n', 'e', 'r', 'a', 'l']) {
                    return None;
                }
                code.push(0x01);
                i += 6;
            }
            _ => return None,
        }
        i += 1;
    }
    Some(code)
}

fn push_bare(out: &mut String, c: char) {
    if c == ' ' || c == '-' || "()+!^&'~{}<>=`|".contains(c) {
        out.push(c);
    } else {
        out.push('\\');
        out.push(c);
    }
}

fn decode(b: &[u8]) -> Option<String> {
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
            0x16 => out.push_str("hh"),
            0x17 => out.push('m'),
            0x18 => out.push_str("mm"),
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

fn elapsed_hours(s: &str) -> Option<usize> {
    let inner = s.strip_prefix('[')?.split(']').next()?;
    (!inner.is_empty() && inner.chars().all(|c| c == 'h')).then_some(inner.len())
}

pub(super) fn garble_section(sec: &str) -> Option<String> {
    let at = sec.find("am/pm")?;

    let start = sec[at + 5..].find('[')? + at + 5;
    let width = elapsed_hours(&sec[start..])?;
    let before = &sec[..start];
    if sec[..at].contains('[') || before[at + 5..].chars().any(|c| !matches!(c, ' ' | 'd')) {
        return None;
    }
    let tail = &sec[start + width + 2..];
    if tail.is_empty() {
        return None;
    }
    let mut code = encode_follower(tail)?;

    if tail.starts_with('[') {
        return None;
    }
    code[0] += 11;
    let mut stream = vec![0x30, u8::try_from(width).ok()?];
    stream.extend(code);
    let decoded = decode(&stream)?;
    Some(format!("{before}{decoded}"))
}
