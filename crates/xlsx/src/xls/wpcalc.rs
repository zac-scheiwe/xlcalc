#[derive(Debug, Clone, PartialEq)]
enum Tok {
    Num(f64),
    Str(String),
    Name(String),
    Op(&'static str),
    Open,
    Close,
    Comma,
    Func(String),
}

fn lex(s: &str) -> Result<Vec<Tok>, String> {
    let c: Vec<char> = s.trim().trim_start_matches('=').chars().collect();
    let mut out = Vec::new();
    let mut i = 0;
    while i < c.len() {
        let ch = c[i];
        match ch {
            ' ' | '\t' => i += 1,
            '(' => {
                out.push(Tok::Open);
                i += 1;
            }
            ')' => {
                out.push(Tok::Close);
                i += 1;
            }
            ',' => {
                out.push(Tok::Comma);
                i += 1;
            }
            '+' | '-' | '*' | '/' | '^' | '&' | '%' => {
                out.push(Tok::Op(match ch {
                    '+' => "+",
                    '-' => "-",
                    '*' => "*",
                    '/' => "/",
                    '^' => "^",
                    '&' => "&",
                    _ => "%",
                }));
                i += 1;
            }
            '=' | '<' | '>' => {
                let two: String = c[i..(i + 2).min(c.len())].iter().collect();
                let (op, n) = match two.as_str() {
                    "<=" => ("<=", 2),
                    ">=" => (">=", 2),
                    "<>" => ("<>", 2),
                    _ => (
                        match ch {
                            '=' => "=",
                            '<' => "<",
                            _ => ">",
                        },
                        1,
                    ),
                };
                out.push(Tok::Op(op));
                i += n;
            }
            '"' => {
                let mut t = String::new();
                i += 1;
                loop {
                    match c.get(i) {
                        None => return Err("unterminated string".into()),
                        Some('"') if c.get(i + 1) == Some(&'"') => {
                            t.push('"');
                            i += 2;
                        }
                        Some('"') => {
                            i += 1;
                            break;
                        }
                        Some(x) => {
                            t.push(*x);
                            i += 1;
                        }
                    }
                }
                out.push(Tok::Str(t));
            }
            '\'' => {
                let mut t = String::new();
                i += 1;
                loop {
                    match c.get(i) {
                        None => return Err("unterminated name".into()),
                        Some('\'') if c.get(i + 1) == Some(&'\'') => {
                            t.push('\'');
                            i += 2;
                        }
                        Some('\'') => {
                            i += 1;
                            break;
                        }
                        Some(x) => {
                            t.push(*x);
                            i += 1;
                        }
                    }
                }
                out.push(Tok::Name(t));
            }
            _ if ch.is_ascii_digit() || ch == '.' => {
                let st = i;
                while i < c.len() && (c[i].is_ascii_digit() || c[i] == '.') {
                    i += 1;
                }
                if i < c.len() && (c[i] == 'e' || c[i] == 'E') {
                    let mut j = i + 1;
                    if j < c.len() && (c[j] == '+' || c[j] == '-') {
                        j += 1;
                    }
                    if j < c.len() && c[j].is_ascii_digit() {
                        while j < c.len() && c[j].is_ascii_digit() {
                            j += 1;
                        }
                        i = j;
                    }
                }
                let t: String = c[st..i].iter().collect();
                out.push(Tok::Num(t.parse().map_err(|_| format!("number {t}"))?));
            }
            _ => {
                let st = i;
                while i < c.len() && !" \t()+-*/^&%=<>,\"'".contains(c[i]) {
                    i += 1;
                }
                let t: String = c[st..i].iter().collect();
                if c.get(i) == Some(&'(') {
                    out.push(Tok::Func(t.to_uppercase()));
                } else {
                    out.push(Tok::Name(t));
                }
            }
        }
    }
    Ok(out)
}

struct P<'a> {
    t: Vec<Tok>,
    i: usize,
    fields: &'a [String],
    names: Vec<usize>,
    out: Vec<u8>,
}

const FIXED: [(&str, u16, u8); 5] = [
    ("SQRT", 20, 1),
    ("ABS", 24, 1),
    ("INT", 25, 1),
    ("ROUND", 27, 2),
    ("NOT", 38, 1),
];

const VAR: [(&str, u16); 7] = [
    ("IF", 1),
    ("SUM", 4),
    ("AVERAGE", 5),
    ("MIN", 6),
    ("MAX", 7),
    ("AND", 36),
    ("OR", 37),
];

fn binary(op: &str) -> Option<(u8, u8)> {
    Some(match op {
        "=" => (1, 0x0B),
        "<" => (1, 0x09),
        ">" => (1, 0x0D),
        "<=" => (1, 0x0A),
        ">=" => (1, 0x0C),
        "<>" => (1, 0x0E),
        "&" => (2, 0x08),
        "+" => (3, 0x03),
        "-" => (3, 0x04),
        "*" => (4, 0x05),
        "/" => (4, 0x06),
        "^" => (5, 0x07),
        _ => return None,
    })
}

impl P<'_> {
    fn peek(&self) -> Option<&Tok> {
        self.t.get(self.i)
    }

    fn expr(&mut self, min: u8) -> Result<(), String> {
        self.unary()?;
        while let Some(Tok::Op(op)) = self.peek().cloned() {
            let Some((prec, ptg)) = binary(op) else {
                break;
            };
            if prec < min {
                break;
            }
            self.i += 1;
            self.expr(prec + 1)?;
            self.out.push(ptg);
        }
        Ok(())
    }

    fn unary(&mut self) -> Result<(), String> {
        match self.peek().cloned() {
            Some(Tok::Op("-")) => {
                self.i += 1;
                if let Some(Tok::Num(x)) = self.peek().cloned() {
                    self.i += 1;
                    self.number(-x);
                } else {
                    self.unary()?;
                    self.out.push(0x13);
                }
                self.postfix();
                Ok(())
            }
            Some(Tok::Op("+")) => {
                self.i += 1;
                self.unary()?;
                self.out.push(0x12);
                Ok(())
            }
            _ => {
                self.atom()?;
                self.postfix();
                Ok(())
            }
        }
    }

    fn postfix(&mut self) {
        while self.peek() == Some(&Tok::Op("%")) {
            self.i += 1;
            self.out.push(0x14);
        }
    }

    fn number(&mut self, x: f64) {
        if x >= 0.0 && x.fract() == 0.0 && x <= 65535.0 {
            self.out.push(0x1E);
            self.out.extend((x as u16).to_le_bytes());
        } else {
            self.out.push(0x1F);
            self.out.extend(x.to_le_bytes());
        }
    }

    fn atom(&mut self) -> Result<(), String> {
        let Some(t) = self.peek().cloned() else {
            return Err("formula ends early".into());
        };
        self.i += 1;
        match t {
            Tok::Num(x) => self.number(x),
            Tok::Str(s) => {
                let n = s.encode_utf16().count();
                if n > 255 {
                    return Err("string too long".into());
                }
                self.out.extend([0x17, n as u8]);
                super::ptg_out::put_chars(&mut self.out, &s);
            }
            Tok::Name(n) => {
                let up = n.to_uppercase();
                let f = self
                    .fields
                    .iter()
                    .position(|f| *f == n)
                    .or_else(|| self.fields.iter().position(|f| f.to_uppercase() == up));
                match f {
                    Some(f) => {
                        let k = match self.names.iter().position(|x| *x == f) {
                            Some(k) => k,
                            None => {
                                self.names.push(f);
                                self.names.len() - 1
                            }
                        };
                        self.out.extend([0x18, 0x1D]);
                        self.out.extend((k as u32).to_le_bytes());
                    }
                    None if up == "TRUE" || up == "FALSE" => {
                        self.out.extend([0x1D, u8::from(up == "TRUE")]);
                    }
                    None => return Err(format!("unknown field {n} in a calculated field")),
                }
            }
            Tok::Open => {
                self.expr(1)?;
                if self.peek() != Some(&Tok::Close) {
                    return Err("missing )".into());
                }
                self.i += 1;
                self.out.push(0x15);
            }
            Tok::Func(name) => self.call(&name)?,
            _ => return Err("unexpected token".into()),
        }
        Ok(())
    }

    fn call(&mut self, name: &str) -> Result<(), String> {
        self.i += 1;
        let mut starts = Vec::new();
        let mut args = 0usize;
        if self.peek() == Some(&Tok::Close) {
            self.i += 1;
        } else {
            loop {
                starts.push(self.out.len());
                self.expr(1)?;
                args += 1;
                match self.peek() {
                    Some(Tok::Comma) => self.i += 1,
                    Some(Tok::Close) => {
                        self.i += 1;
                        break;
                    }
                    _ => return Err("bad argument list".into()),
                }
            }
        }
        if name == "IF" {
            if args != 3 {
                return Err("IF with two arguments is not written".into());
            }

            let body = self.out.split_off(starts[0]);
            let (cond_end, t_end) = (starts[1] - starts[0], starts[2] - starts[0]);
            let (cond, rest) = body.split_at(cond_end);
            let (tru, fal) = rest.split_at(t_end - cond_end);
            self.out.extend(cond);
            self.out.extend([0x19, 0x02]);
            self.out.extend((tru.len() as u16 + 4).to_le_bytes());
            self.out.extend(tru);
            self.out.extend([0x19, 0x08]);
            self.out
                .extend((fal.len() as u16 + 4 + 4 - 1).to_le_bytes());
            self.out.extend(fal);
            self.out.extend([0x19, 0x08, 3, 0]);
            self.out.extend([0x42, 3, 1, 0]);
            return Ok(());
        }
        if let Some((_, id, n)) = FIXED.iter().find(|f| f.0 == name) {
            if args != usize::from(*n) {
                return Err(format!("{name} takes {n} arguments"));
            }
            self.out.push(0x41);
            self.out.extend(id.to_le_bytes());
            return Ok(());
        }
        if let Some((_, id)) = VAR.iter().find(|f| f.0 == name) {
            self.out.extend([0x42, args as u8]);
            self.out.extend(id.to_le_bytes());
            return Ok(());
        }
        Err(format!(
            "function {name} in a calculated field is not written"
        ))
    }
}

pub(super) fn compile(formula: &str, fields: &[String]) -> Result<(Vec<u8>, Vec<usize>), String> {
    let mut p = P {
        t: lex(formula)?,
        i: 0,
        fields,
        names: Vec::new(),
        out: Vec::new(),
    };
    p.expr(1)?;
    if p.i != p.t.len() {
        return Err("trailing text in a calculated field".into());
    }
    Ok((p.out, p.names))
}

fn quote(n: &str) -> String {
    if n.chars()
        .all(|c| c.is_alphanumeric() || c == '_' || c == '.')
    {
        n.to_string()
    } else {
        format!("'{}'", n.replace('\'', "''"))
    }
}

pub(super) fn decompile(rgce: &[u8], names: &[String]) -> Option<String> {
    let mut st: Vec<String> = Vec::new();
    let mut i = 0;
    while i < rgce.len() {
        let p = rgce[i];
        i += 1;
        match p {
            0x18 => {
                if *rgce.get(i)? != 0x1D {
                    return None;
                }
                let k = u32::from_le_bytes(rgce.get(i + 1..i + 5)?.try_into().ok()?) as usize;
                st.push(quote(names.get(k)?));
                i += 5;
            }
            0x1E => {
                st.push(u16::from_le_bytes([*rgce.get(i)?, *rgce.get(i + 1)?]).to_string());
                i += 2;
            }
            0x1F => {
                let x = f64::from_le_bytes(rgce.get(i..i + 8)?.try_into().ok()?);
                st.push(format!("{x}"));
                i += 8;
            }
            0x1D => {
                st.push(if *rgce.get(i)? == 0 { "FALSE" } else { "TRUE" }.into());
                i += 1;
            }
            0x17 => {
                let n = usize::from(*rgce.get(i)?);
                if *rgce.get(i + 1)? != 0 {
                    return None;
                }
                let s = String::from_utf8_lossy(rgce.get(i + 2..i + 2 + n)?).into_owned();
                st.push(format!("\"{}\"", s.replace('"', "\"\"")));
                i += 2 + n;
            }
            0x19 => i += 3,
            0x03..=0x0E => {
                let b = st.pop()?;
                let a = st.pop()?;
                let op = [
                    "+", "-", "*", "/", "^", "&", "<", "<=", "=", ">=", ">", "<>",
                ][usize::from(p) - 3];
                st.push(format!("{a}{op}{b}"));
            }
            0x12 | 0x13 => {
                let a = st.pop()?;
                st.push(format!("{}{a}", if p == 0x12 { "+" } else { "-" }));
            }
            0x14 => {
                let a = st.pop()?;
                st.push(format!("{a}%"));
            }
            0x15 => {
                let a = st.pop()?;
                st.push(format!("({a})"));
            }
            0x41 | 0x42 => {
                let (n, id) = if p == 0x41 {
                    let id = u16::from_le_bytes([*rgce.get(i)?, *rgce.get(i + 1)?]);
                    i += 2;
                    (FIXED.iter().find(|f| f.1 == id)?.2 as usize, id)
                } else {
                    let n = usize::from(*rgce.get(i)?);
                    let id = u16::from_le_bytes([*rgce.get(i + 1)?, *rgce.get(i + 2)?]);
                    i += 3;
                    (n, id)
                };
                let name = FIXED
                    .iter()
                    .find(|f| f.1 == id)
                    .map(|f| f.0)
                    .or_else(|| VAR.iter().find(|f| f.1 == id).map(|f| f.0))?;
                if st.len() < n {
                    return None;
                }
                let args = st.split_off(st.len() - n);
                st.push(format!("{name}({})", args.join(",")));
            }
            _ => return None,
        }
    }
    (st.len() == 1).then(|| st.remove(0))
}
#[cfg(test)]
mod tests {
    use super::compile;

    fn f(n: &[&str]) -> Vec<String> {
        n.iter().map(|s| s.to_string()).collect()
    }

    #[test]
    fn arithmetic_and_names() {
        let (b, n) = compile("=Amount+Units*3", &f(&["Region", "Amount", "Units"])).unwrap();
        assert_eq!(n, [1, 2]);
        assert_eq!(
            b,
            [0x18, 0x1D, 0, 0, 0, 0, 0x18, 0x1D, 1, 0, 0, 0, 0x1E, 3, 0, 0x05, 0x03]
        );
    }

    #[test]
    fn decompile_round_trip() {
        let fs = f(&["Amount", "Unit Price"]);
        for s in [
            "IF(Amount>5,Amount,0)",
            "Amount+'Unit Price'*-2^2&\"x\"",
            "ROUND((Amount)%,2)",
        ] {
            let (b, n) = compile(s, &fs).unwrap();
            let names: Vec<String> = n.iter().map(|i| fs[*i].clone()).collect();
            assert_eq!(super::decompile(&b, &names).unwrap(), s);
        }
    }

    #[test]
    fn if_gets_attr_tokens() {
        let (b, _) = compile("=IF(Amount>5,Amount,0)", &f(&["Amount"])).unwrap();
        assert_eq!(
            b,
            [
                0x18, 0x1D, 0, 0, 0, 0, 0x1E, 5, 0, 0x0D, 0x19, 2, 10, 0, 0x18, 0x1D, 0, 0, 0, 0,
                0x19, 8, 10, 0, 0x1E, 0, 0, 0x19, 8, 3, 0, 0x42, 3, 1, 0
            ]
        );
    }
}
