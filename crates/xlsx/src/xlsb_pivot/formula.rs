#[derive(Clone, Copy, PartialEq, Eq, Debug)]
pub(crate) enum Name {
    Field(usize),

    Item(usize, usize),
}

#[derive(Debug, Clone, PartialEq)]
enum Tok {
    Num(f64),
    Str(String),
    Name(String, Option<String>),
    Op(&'static str),
    Open,
    Close,
    Percent,
}

fn is_word(c: char) -> bool {
    c.is_alphanumeric() || c == '_' || c == '.'
}

fn quoted(ch: &[char], mut i: usize) -> Result<(String, usize), String> {
    let mut t = String::new();
    loop {
        match ch.get(i) {
            None => return Err("unterminated name".into()),
            Some('\'') if ch.get(i + 1) == Some(&'\'') => {
                t.push('\'');
                i += 2;
            }
            Some('\'') => return Ok((t, i + 1)),
            Some(&c) => {
                t.push(c);
                i += 1;
            }
        }
    }
}

fn lex(s: &str) -> Result<Vec<Tok>, String> {
    let ch: Vec<char> = s.chars().collect();
    let mut out = Vec::new();
    let mut i = 0;
    while i < ch.len() {
        let c = ch[i];
        if c.is_whitespace() {
            i += 1;
        } else if c.is_ascii_digit()
            || (c == '.' && ch.get(i + 1).is_some_and(char::is_ascii_digit))
        {
            let st = i;
            while i < ch.len() && (ch[i].is_ascii_digit() || ch[i] == '.') {
                i += 1;
            }
            if i < ch.len() && (ch[i] == 'E' || ch[i] == 'e') {
                let mut j = i + 1;
                if j < ch.len() && (ch[j] == '+' || ch[j] == '-') {
                    j += 1;
                }
                if j < ch.len() && ch[j].is_ascii_digit() {
                    while j < ch.len() && ch[j].is_ascii_digit() {
                        j += 1;
                    }
                    i = j;
                }
            }
            let t: String = ch[st..i].iter().collect();
            out.push(Tok::Num(t.parse().map_err(|_| format!("number {t}"))?));
        } else if c == '"' {
            i += 1;
            let mut t = String::new();
            loop {
                match ch.get(i) {
                    None => return Err("unterminated string".into()),
                    Some('"') if ch.get(i + 1) == Some(&'"') => {
                        t.push('"');
                        i += 2;
                    }
                    Some('"') => {
                        i += 1;
                        break;
                    }
                    Some(&c) => {
                        t.push(c);
                        i += 1;
                    }
                }
            }
            out.push(Tok::Str(t));
        } else if c == '\'' || c.is_alphabetic() || c == '_' {
            let name = if c == '\'' {
                let (t, j) = quoted(&ch, i + 1)?;
                i = j;
                t
            } else {
                let st = i;
                while i < ch.len() && is_word(ch[i]) {
                    i += 1;
                }
                ch[st..i].iter().collect()
            };
            if ch.get(i) == Some(&'(') {
                return Err(format!("function {name}"));
            }
            let mut item = None;
            if ch.get(i) == Some(&'[') {
                i += 1;
                if ch.get(i) == Some(&'\'') {
                    let (t, j) = quoted(&ch, i + 1)?;
                    if ch.get(j) != Some(&']') {
                        return Err("item bracket".into());
                    }
                    i = j + 1;
                    item = Some(t);
                } else {
                    let mut t = String::new();
                    while i < ch.len() && ch[i] != ']' {
                        t.push(ch[i]);
                        i += 1;
                    }
                    if i >= ch.len() {
                        return Err("unterminated item".into());
                    }
                    i += 1;
                    item = Some(t);
                }
            }
            out.push(Tok::Name(name, item));
        } else {
            let two: String = ch[i..(i + 2).min(ch.len())].iter().collect();
            let op = match two.as_str() {
                "<=" => Some("<="),
                ">=" => Some(">="),
                "<>" => Some("<>"),
                _ => None,
            };
            if let Some(op) = op {
                out.push(Tok::Op(op));
                i += 2;
                continue;
            }
            match c {
                '+' => out.push(Tok::Op("+")),
                '-' => out.push(Tok::Op("-")),
                '*' => out.push(Tok::Op("*")),
                '/' => out.push(Tok::Op("/")),
                '^' => out.push(Tok::Op("^")),
                '&' => out.push(Tok::Op("&")),
                '=' => out.push(Tok::Op("=")),
                '<' => out.push(Tok::Op("<")),
                '>' => out.push(Tok::Op(">")),
                '(' => out.push(Tok::Open),
                ')' => out.push(Tok::Close),
                '%' => out.push(Tok::Percent),
                _ => return Err(format!("character {c}")),
            }
            i += 1;
        }
    }
    Ok(out)
}

fn binary(op: &str) -> Option<(u8, u8)> {
    Some(match op {
        "+" => (0x03, 3),
        "-" => (0x04, 3),
        "*" => (0x05, 4),
        "/" => (0x06, 4),
        "^" => (0x07, 5),
        "&" => (0x08, 2),
        "<" => (0x09, 1),
        "<=" => (0x0A, 1),
        "=" => (0x0B, 1),
        ">=" => (0x0C, 1),
        ">" => (0x0D, 1),
        "<>" => (0x0E, 1),
        _ => return None,
    })
}

pub(crate) fn encode(
    text: &str,
    resolve: &dyn Fn(&str, Option<&str>) -> Option<Name>,
) -> Result<(Vec<u8>, Vec<Name>), String> {
    let toks = lex(text)?;
    let mut out: Vec<u8> = Vec::new();
    let mut names: Vec<Name> = Vec::new();

    let mut ops: Vec<(u8, u8)> = Vec::new();
    let mut operand_before = false;
    for t in toks {
        match t {
            Tok::Num(n) => {
                if n.fract() == 0.0 && (0.0..=65535.0).contains(&n) {
                    out.push(0x1E);
                    out.extend((n as u16).to_le_bytes());
                } else {
                    out.push(0x1F);
                    out.extend(n.to_le_bytes());
                }
                operand_before = true;
            }
            Tok::Str(s) => {
                let u: Vec<u16> = s.encode_utf16().collect();
                out.push(0x17);
                out.extend((u.len() as u16).to_le_bytes());
                for c in u {
                    out.extend(c.to_le_bytes());
                }
                operand_before = true;
            }
            Tok::Name(n, item) => {
                let nm = resolve(&n, item.as_deref()).ok_or_else(|| {
                    format!(
                        "unknown name {n}{}",
                        item.map_or(String::new(), |i| format!("[{i}]"))
                    )
                })?;
                let idx = names.iter().position(|x| *x == nm).unwrap_or_else(|| {
                    names.push(nm);
                    names.len() - 1
                });
                out.extend([0x18, 0x1D]);
                out.extend((idx as u32).to_le_bytes());
                operand_before = true;
            }
            Tok::Open => {
                ops.push((0, 0));
                operand_before = false;
            }
            Tok::Close => {
                loop {
                    match ops.pop() {
                        Some((0, 0)) => break,
                        Some((b, _)) => out.push(b),
                        None => return Err("unbalanced )".into()),
                    }
                }
                out.push(0x15);
                operand_before = true;
            }
            Tok::Percent => {
                if !operand_before {
                    return Err("% without operand".into());
                }
                out.push(0x14);
            }
            Tok::Op(op) => {
                if operand_before {
                    let (b, p) = binary(op).ok_or("operator")?;
                    while let Some(&(top, tp)) = ops.last() {
                        if top != 0 && tp >= p {
                            out.push(top);
                            ops.pop();
                        } else {
                            break;
                        }
                    }
                    ops.push((b, p));
                    operand_before = false;
                } else {
                    match op {
                        "-" => ops.push((0x13, 6)),
                        "+" => ops.push((0x12, 6)),
                        _ => return Err(format!("unexpected {op}")),
                    }
                }
            }
        }
    }
    while let Some((b, _)) = ops.pop() {
        if b == 0 {
            return Err("unbalanced (".into());
        }
        out.push(b);
    }
    if out.is_empty() {
        return Err("empty formula".into());
    }
    Ok((out, names))
}

pub(crate) fn decode(
    rgce: &[u8],
    name: &dyn Fn(usize) -> Option<String>,
) -> Result<String, String> {
    let mut st: Vec<String> = Vec::new();
    let mut i = 0;
    let need = |st: &mut Vec<String>| st.pop().ok_or_else(|| "formula underflow".to_string());
    let take = |i: usize, n: usize| {
        rgce.get(i..i + n)
            .ok_or_else(|| "formula short".to_string())
    };
    while i < rgce.len() {
        let p = rgce[i];
        i += 1;
        match p {
            0x03..=0x0E => {
                let op = [
                    "+", "-", "*", "/", "^", "&", "<", "<=", "=", ">=", ">", "<>",
                ][usize::from(p) - 3];
                let b = need(&mut st)?;
                let a = need(&mut st)?;
                st.push(format!("{a}{op}{b}"));
            }
            0x12 => {
                let a = need(&mut st)?;
                st.push(format!("+{a}"));
            }
            0x13 => {
                let a = need(&mut st)?;
                st.push(format!("-{a}"));
            }
            0x14 => {
                let a = need(&mut st)?;
                st.push(format!("{a}%"));
            }
            0x15 => {
                let a = need(&mut st)?;
                st.push(format!("({a})"));
            }
            0x17 => {
                let h = take(i, 2)?;
                let n = usize::from(u16::from_le_bytes([h[0], h[1]]));
                i += 2;
                let raw = take(i, 2 * n)?;
                i += 2 * n;
                let u: Vec<u16> = raw
                    .chunks(2)
                    .map(|c| u16::from_le_bytes([c[0], c[1]]))
                    .collect();
                st.push(format!(
                    "\"{}\"",
                    String::from_utf16_lossy(&u).replace('"', "\"\"")
                ));
            }
            0x1E => {
                let h = take(i, 2)?;
                i += 2;
                st.push(u16::from_le_bytes([h[0], h[1]]).to_string());
            }
            0x1F => {
                let raw = take(i, 8)?;
                i += 8;
                let mut a = [0u8; 8];
                a.copy_from_slice(raw);
                st.push(crate::write::number_text(f64::from_le_bytes(a)));
            }
            0x18 if rgce.get(i) == Some(&0x1D) => {
                let raw = take(i + 1, 4)?;
                i += 5;
                let idx = u32::from_le_bytes([raw[0], raw[1], raw[2], raw[3]]) as usize;
                st.push(name(idx).ok_or_else(|| format!("name {idx}"))?);
            }
            _ => return Err(format!("token {p:#x}")),
        }
    }
    if st.len() == 1 {
        Ok(st.remove(0))
    } else {
        Err("formula stack".into())
    }
}

pub(crate) fn field_text(name: &str) -> String {
    let plain = name
        .chars()
        .next()
        .is_some_and(|c| c.is_alphabetic() || c == '_')
        && name.chars().all(is_word);
    if plain {
        name.to_string()
    } else {
        format!("'{}'", name.replace('\'', "''"))
    }
}

pub(crate) fn item_text(field: &str, item: &str) -> String {
    let plain = item
        .chars()
        .next()
        .is_some_and(|c| c.is_alphabetic() || c == '_')
        && item.chars().all(|c| c.is_alphanumeric() || c == '_');
    if plain {
        format!("{}[{item}]", field_text(field))
    } else {
        format!("{}['{}']", field_text(field), item.replace('\'', "''"))
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn calculated_field_round_trip() {
        let res = |n: &str, i: Option<&str>| {
            assert!(i.is_none());
            match n {
                "Amount" => Some(Name::Field(2)),
                "Unit Price" => Some(Name::Field(1)),
                _ => None,
            }
        };
        let (b, names) = encode("'Unit Price'*2+Amount", &res).unwrap();
        assert_eq!(
            b,
            [0x18, 0x1D, 0, 0, 0, 0, 0x1E, 2, 0, 0x05, 0x18, 0x1D, 1, 0, 0, 0, 0x03]
        );
        assert_eq!(names, [Name::Field(1), Name::Field(2)]);
        let back = decode(&b, &|i| {
            Some(match names[i] {
                Name::Field(1) => "'Unit Price'".to_string(),
                _ => "Amount".to_string(),
            })
        })
        .unwrap();
        assert_eq!(back, "'Unit Price'*2+Amount");
    }

    #[test]
    fn items_and_parens() {
        let res = |n: &str, i: Option<&str>| {
            (n == "Region").then(|| Name::Item(0, i.map_or(9, |s| s.len())))
        };
        let (b, names) = encode("(Region[Mix]+1)*-2", &res).unwrap();
        assert_eq!(names, [Name::Item(0, 3)]);
        assert_eq!(*b.last().unwrap(), 0x05);
        assert!(encode("SUM(Region[a])", &res).is_err());
    }
}
