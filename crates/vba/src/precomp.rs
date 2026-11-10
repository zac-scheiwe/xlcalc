use crate::CompileError;
use std::collections::HashMap;

#[derive(Clone, Debug, PartialEq)]
enum V {
    Int(i64),
    Dbl(f64),
    Str(String),
}

impl V {
    fn num(&self) -> Option<f64> {
        match self {
            V::Int(i) => Some(*i as f64),
            V::Dbl(d) => Some(*d),
            V::Str(s) => s.trim().parse().ok(),
        }
    }

    fn truth(&self) -> Option<bool> {
        self.num().map(|d| d != 0.0)
    }

    fn int(&self) -> Option<i64> {
        match self {
            V::Int(i) => Some(*i),
            _ => self.num().map(round_even),
        }
    }
}

fn round_even(d: f64) -> i64 {
    let r = d.round();
    if (d - d.trunc()).abs() == 0.5 && r % 2.0 != 0.0 {
        (r - d.signum()) as i64
    } else {
        r as i64
    }
}

fn boolean(b: bool) -> V {
    V::Int(if b { -1 } else { 0 })
}

struct Expr<'a> {
    toks: Vec<String>,
    pos: usize,
    consts: &'a HashMap<String, V>,
    text_compare: bool,
}

fn tokenize(s: &str) -> Option<Vec<String>> {
    let cs: Vec<char> = s.chars().collect();
    let mut out = Vec::new();
    let mut i = 0;
    while i < cs.len() {
        let c = cs[i];
        if c.is_whitespace() {
            i += 1;
        } else if c == '\'' {
            break;
        } else if c == '"' {
            let mut t = String::from("\"");
            i += 1;
            loop {
                match cs.get(i) {
                    None => return None,
                    Some('"') if cs.get(i + 1) == Some(&'"') => {
                        t.push('"');
                        i += 2;
                    }
                    Some('"') => {
                        i += 1;
                        break;
                    }
                    Some(&ch) => {
                        t.push(ch);
                        i += 1;
                    }
                }
            }
            out.push(t);
        } else if c.is_ascii_digit()
            || (c == '.' && cs.get(i + 1).is_some_and(|d| d.is_ascii_digit()))
        {
            let mut t = String::new();
            while i < cs.len()
                && (cs[i].is_ascii_alphanumeric()
                    || cs[i] == '.'
                    || ((cs[i] == '+' || cs[i] == '-')
                        && matches!(t.chars().last(), Some('e' | 'E'))
                        && !t.starts_with("&")))
            {
                t.push(cs[i]);
                i += 1;
            }
            out.push(t);
        } else if c == '&' && matches!(cs.get(i + 1), Some('H' | 'h' | 'O' | 'o')) {
            let mut t = String::from("&");
            i += 1;
            while i < cs.len() && (cs[i].is_ascii_alphanumeric()) {
                t.push(cs[i]);
                i += 1;
            }
            out.push(t);
        } else if c.is_alphabetic() || c == '_' {
            let mut t = String::new();
            while i < cs.len() && (cs[i].is_alphanumeric() || cs[i] == '_') {
                t.push(cs[i]);
                i += 1;
            }
            out.push(t);
        } else if (c == '<' || c == '>')
            && matches!(cs.get(i + 1), Some('=' | '>'))
            && !(c == '>' && cs[i + 1] == '>')
        {
            out.push(cs[i..i + 2].iter().collect());
            i += 2;
        } else if (c == '=') && matches!(cs.get(i + 1), Some('<' | '>')) {
            let t: String = cs[i..i + 2].iter().collect();
            out.push(if t == "=<" { "<=".into() } else { ">=".into() });
            i += 2;
        } else {
            out.push(c.to_string());
            i += 1;
        }
    }
    Some(out)
}

impl Expr<'_> {
    fn peek(&self) -> Option<&str> {
        self.toks.get(self.pos).map(String::as_str)
    }

    fn kw(&self, k: &str) -> bool {
        self.peek().is_some_and(|t| t.eq_ignore_ascii_case(k))
    }

    fn eat_kw(&mut self, k: &str) -> bool {
        if self.kw(k) {
            self.pos += 1;
            true
        } else {
            false
        }
    }

    fn eat(&mut self, k: &str) -> bool {
        if self.peek() == Some(k) {
            self.pos += 1;
            true
        } else {
            false
        }
    }

    fn imp(&mut self) -> Option<V> {
        let mut l = self.eqv()?;
        while self.eat_kw("Imp") {
            let r = self.eqv()?;
            l = V::Int(!l.int()? | r.int()?);
        }
        Some(l)
    }

    fn eqv(&mut self) -> Option<V> {
        let mut l = self.xor()?;
        while self.eat_kw("Eqv") {
            let r = self.xor()?;
            l = V::Int(!(l.int()? ^ r.int()?));
        }
        Some(l)
    }

    fn xor(&mut self) -> Option<V> {
        let mut l = self.or()?;
        while self.eat_kw("Xor") {
            let r = self.or()?;
            l = V::Int(l.int()? ^ r.int()?);
        }
        Some(l)
    }

    fn or(&mut self) -> Option<V> {
        let mut l = self.and()?;
        while self.eat_kw("Or") {
            let r = self.and()?;
            l = V::Int(l.int()? | r.int()?);
        }
        Some(l)
    }

    fn and(&mut self) -> Option<V> {
        let mut l = self.not()?;
        while self.eat_kw("And") {
            let r = self.not()?;
            l = V::Int(l.int()? & r.int()?);
        }
        Some(l)
    }

    fn not(&mut self) -> Option<V> {
        if self.eat_kw("Not") {
            let v = self.not()?;
            return Some(V::Int(!v.int()?));
        }
        self.cmp()
    }

    fn cmp(&mut self) -> Option<V> {
        let mut l = self.concat()?;
        while let Some(op) = self
            .peek()
            .filter(|t| ["=", "<>", "<", ">", "<=", ">="].contains(t))
        {
            let op = op.to_string();
            self.pos += 1;
            let r = self.concat()?;
            let ord = match (&l, &r) {
                (V::Str(a), V::Str(b)) => {
                    if self.text_compare {
                        a.to_lowercase().cmp(&b.to_lowercase())
                    } else {
                        a.cmp(b)
                    }
                }
                _ => l.num()?.partial_cmp(&r.num()?)?,
            };
            let b = match op.as_str() {
                "=" => ord.is_eq(),
                "<>" => ord.is_ne(),
                "<" => ord.is_lt(),
                ">" => ord.is_gt(),
                "<=" => ord.is_le(),
                _ => ord.is_ge(),
            };
            l = boolean(b);
        }
        Some(l)
    }

    fn concat(&mut self) -> Option<V> {
        let mut l = self.add()?;
        while self.eat("&") {
            let r = self.add()?;
            l = V::Str(format!("{}{}", show(&l), show(&r)));
        }
        Some(l)
    }

    fn add(&mut self) -> Option<V> {
        let mut l = self.modulo()?;
        loop {
            if self.eat("+") {
                let r = self.modulo()?;
                l = arith(&l, &r, |a, b| a + b, |a, b| a.checked_add(b))?;
            } else if self.eat("-") {
                let r = self.modulo()?;
                l = arith(&l, &r, |a, b| a - b, |a, b| a.checked_sub(b))?;
            } else {
                return Some(l);
            }
        }
    }

    fn modulo(&mut self) -> Option<V> {
        let mut l = self.intdiv()?;
        while self.eat_kw("Mod") {
            let r = self.intdiv()?;
            let (a, b) = (l.int()?, r.int()?);
            if b == 0 {
                return None;
            }
            l = V::Int(a % b);
        }
        Some(l)
    }

    fn intdiv(&mut self) -> Option<V> {
        let mut l = self.mul()?;
        while self.eat("\\") {
            let r = self.mul()?;
            let (a, b) = (l.int()?, r.int()?);
            if b == 0 {
                return None;
            }
            l = V::Int(a / b);
        }
        Some(l)
    }

    fn mul(&mut self) -> Option<V> {
        let mut l = self.neg()?;
        loop {
            if self.eat("*") {
                let r = self.neg()?;
                l = arith(&l, &r, |a, b| a * b, |a, b| a.checked_mul(b))?;
            } else if self.eat("/") {
                let r = self.neg()?;
                let d = r.num()?;
                if d == 0.0 {
                    return None;
                }
                let q = l.num()? / d;
                l = if q.fract() == 0.0 && q.abs() < 1e15 {
                    V::Int(q as i64)
                } else {
                    V::Dbl(q)
                };
            } else {
                return Some(l);
            }
        }
    }

    fn neg(&mut self) -> Option<V> {
        if self.eat("-") {
            return match self.neg()? {
                V::Int(i) => Some(V::Int(-i)),
                v => Some(V::Dbl(-v.num()?)),
            };
        }
        if self.eat("+") {
            return self.neg();
        }
        self.pow()
    }

    fn pow(&mut self) -> Option<V> {
        let mut l = self.atom()?;
        while self.eat("^") {
            let r = self.atom()?;
            let p = l.num()?.powf(r.num()?);
            l = if p.fract() == 0.0 && p.abs() < 1e15 {
                V::Int(p as i64)
            } else {
                V::Dbl(p)
            };
        }
        Some(l)
    }

    fn atom(&mut self) -> Option<V> {
        let t = self.peek()?.to_string();
        self.pos += 1;
        if t == "(" {
            let v = self.imp()?;
            return if self.eat(")") { Some(v) } else { None };
        }
        if let Some(s) = t.strip_prefix('"') {
            return Some(V::Str(s.to_string()));
        }
        let first = t.chars().next()?;
        if first.is_ascii_digit() || first == '.' {
            return if let Ok(i) = t.parse::<i64>() {
                Some(V::Int(i))
            } else {
                t.parse::<f64>().ok().map(V::Dbl)
            };
        }
        if let Some(h) = t.strip_prefix('&') {
            let (radix, digits) = match h.chars().next()? {
                'H' | 'h' => (16, &h[1..]),
                _ => (8, &h[1..]),
            };
            let digits = digits.trim_end_matches('&');
            let v = i64::from_str_radix(digits, radix).ok()?;

            let v = if !h.ends_with('&')
                && digits.len() <= if radix == 16 { 4 } else { 6 }
                && v > 32767
                && v <= 65535
            {
                v - 65536
            } else {
                v
            };
            return Some(V::Int(v));
        }
        let name = t.to_ascii_lowercase();
        match name.as_str() {
            "true" => return Some(boolean(true)),
            "false" => return Some(boolean(false)),
            "vba7" | "vba6" | "win32" | "win64" => return Some(boolean(true)),
            "win16" | "mac" => return Some(boolean(false)),
            _ => {}
        }
        Some(self.consts.get(&name).cloned().unwrap_or(V::Int(0)))
    }
}

fn show(v: &V) -> String {
    match v {
        V::Int(i) => i.to_string(),
        V::Dbl(d) => d.to_string(),
        V::Str(s) => s.clone(),
    }
}

fn arith(l: &V, r: &V, f: fn(f64, f64) -> f64, g: fn(i64, i64) -> Option<i64>) -> Option<V> {
    if let (V::Int(a), V::Int(b)) = (l, r) {
        if let Some(v) = g(*a, *b) {
            return Some(V::Int(v));
        }
    }
    Some(V::Dbl(f(l.num()?, r.num()?)))
}

fn eval(text: &str, consts: &HashMap<String, V>, text_compare: bool) -> Option<V> {
    let toks = tokenize(text)?;
    let mut e = Expr {
        toks,
        pos: 0,
        consts,
        text_compare,
    };
    let v = e.imp()?;
    if e.pos == e.toks.len() {
        Some(v)
    } else {
        None
    }
}

fn directive(line: &str) -> Option<(&'static str, &str)> {
    let t = line.trim_start();
    let rest = t.strip_prefix('#')?;
    let rest = rest.trim_start();
    let lower: String = rest
        .chars()
        .take(8)
        .collect::<String>()
        .to_ascii_lowercase();
    let cut = |n: usize| &rest[n..];
    let word_end = |n: usize| {
        rest.len() == n || !rest[n..].starts_with(|c: char| c.is_alphanumeric() || c == '_')
    };
    if lower.starts_with("elseif") && word_end(6) {
        Some(("elseif", cut(6)))
    } else if lower.starts_with("else") && word_end(4) {
        Some(("else", cut(4)))
    } else if lower.starts_with("if") && word_end(2) {
        Some(("if", cut(2)))
    } else if lower.starts_with("const") && word_end(5) {
        Some(("const", cut(5)))
    } else if lower.starts_with("end") {
        let after = rest[3..].trim_start();
        if after.len() >= 2 && after[..2].eq_ignore_ascii_case("if") {
            Some(("endif", &after[2..]))
        } else {
            None
        }
    } else {
        None
    }
}

fn strip_then(s: &str) -> &str {
    let s = s.trim();

    let lower = s.to_ascii_lowercase();
    match lower.rfind("then") {
        Some(i) if s[..i].matches('"').count().is_multiple_of(2) => &s[..i],
        _ => s,
    }
}

struct Frame {
    parent_active: bool,
    taken: bool,
    active: bool,
}

pub fn preprocess(module: &str, src: &str) -> Result<String, CompileError> {
    if !src.lines().any(|l| l.trim_start().starts_with('#')) {
        return Ok(src.to_string());
    }
    let text_compare = src
        .lines()
        .any(|l| l.trim().eq_ignore_ascii_case("option compare text"));
    let err = |line: usize, msg: &str| CompileError {
        module: module.to_string(),
        line: line as u32,
        col: 1,
        message: msg.to_string(),
    };
    let mut consts: HashMap<String, V> = HashMap::new();
    let mut stack: Vec<Frame> = Vec::new();
    let mut out = String::with_capacity(src.len());
    for (n, raw) in src.split('\n').enumerate() {
        let (line, cr) = match raw.strip_suffix('\r') {
            Some(l) => (l, "\r"),
            None => (raw, ""),
        };
        let active = stack.last().is_none_or(|f| f.active);
        let mut keep = active;
        if let Some((kw, rest)) = directive(line) {
            keep = false;
            match kw {
                "if" => {
                    let v = if active {
                        eval(strip_then(rest), &consts, text_compare)
                            .and_then(|v| v.truth())
                            .ok_or_else(|| {
                                err(n + 1, "invalid conditional compilation expression")
                            })?
                    } else {
                        false
                    };
                    stack.push(Frame {
                        parent_active: active,
                        taken: v,
                        active: active && v,
                    });
                }
                "elseif" => {
                    let f = stack
                        .last()
                        .ok_or_else(|| err(n + 1, "#ElseIf without #If"))?;
                    let (pa, taken) = (f.parent_active, f.taken);
                    let v = if pa && !taken {
                        eval(strip_then(rest), &consts, text_compare)
                            .and_then(|v| v.truth())
                            .ok_or_else(|| {
                                err(n + 1, "invalid conditional compilation expression")
                            })?
                    } else {
                        false
                    };
                    let f = stack
                        .last_mut()
                        .ok_or_else(|| err(n + 1, "#ElseIf without #If"))?;
                    f.active = pa && !taken && v;
                    f.taken |= v;
                }
                "else" => {
                    let f = stack
                        .last_mut()
                        .ok_or_else(|| err(n + 1, "#Else without #If"))?;
                    f.active = f.parent_active && !f.taken;
                    f.taken = true;
                }
                "endif" => {
                    stack
                        .pop()
                        .ok_or_else(|| err(n + 1, "#End If without #If"))?;
                }
                _ => {
                    if active {
                        let (name, expr) = rest
                            .split_once('=')
                            .ok_or_else(|| err(n + 1, "invalid #Const"))?;
                        let v = eval(expr, &consts, text_compare).ok_or_else(|| {
                            err(n + 1, "invalid conditional compilation expression")
                        })?;
                        consts.insert(name.trim().to_ascii_lowercase(), v);
                    }
                }
            }
        }
        if keep {
            out.push_str(line);
        }
        out.push_str(cr);
        if n + 1 != src.split('\n').count() {
            out.push('\n');
        }
    }
    if !stack.is_empty() {
        return Err(err(src.lines().count(), "#If without #End If"));
    }
    Ok(out)
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn branches() {
        let src = "#Const A = 3\n#If A > 2 Then\nx\n#ElseIf A = 2 Then\ny\n#Else\nz\n#End If\n";
        let out = preprocess("m", src).unwrap();
        assert_eq!(out, "\n\nx\n\n\n\n\n\n");
        let src = "#If Mac Then\nx\n#ElseIf VBA7 And Win64 Then\ny\n#End If\n";
        assert_eq!(preprocess("m", src).unwrap(), "\n\n\ny\n\n");
    }
}
