use super::{Cmp, Color, NumberFormat};

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub(crate) enum Ph {
    Zero,

    Hash,

    Q,
}

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub(crate) enum DateField {
    Year,
    Month,
    Day,
    Hour,
    Second,

    EYear,

    Era,

    Buddhist,
}

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub(crate) enum Unit {
    H,
    M,
    S,
}

#[derive(Clone, Debug, PartialEq)]
pub(crate) enum Tok {
    Lit(String),

    LitDigit(char),

    Space,

    Fill(char),
    Ph(Ph),
    Point,
    Comma,
    Percent,

    Exp(bool),
    Slash,

    Colon,
    At,
    General,
    AmPm,
    AmPmCjk,
    Date(DateField, u8),
    Elapsed(Unit, u8),
    Color(Color),
    Cond(Cmp, f64),
    Locale {
        currency: String,
        lcid: Option<u32>,
        tag: Option<String>,
    },
    Ignore,

    Calendar(u8),
}

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub(crate) enum DateTok {
    Year(u8),
    Month(u8),
    Minute(u8),
    Day(u8),
    Hour(u8),

    Hour12(u8),
    Second(u8),
    SubSec(u8),
    ElapsedH(u8),
    ElapsedM(u8),
    ElapsedS(u8),

    LeftH(u8),
    LeftM(u8),
    LeftS(u8),

    General(bool),
    AmPm,
    AmPmCjk,
    EYear(u8),
    Era(u8),
    Buddhist(u8),
}

#[derive(Clone, Debug, PartialEq)]
pub(crate) enum Item {
    Lit(String),
    Space,
    Fill(char),
    Int(Ph),
    Point,
    Frac(Ph),
    ExpMark(bool),
    ExpDigit(Ph),
    Num(Ph),
    Slash,
    Den(Ph),
    DenFixed(String),

    NumAll,

    DenDec(Ph),
    General,
    Text,
    Date(DateTok),
}

#[derive(Clone, Debug)]
pub(crate) struct NumLayout {
    pub items: Vec<Item>,
    pub n_int: usize,
    pub n_frac: usize,
    pub grouping: bool,

    pub scale: i32,
}

#[derive(Clone, Debug)]
pub(crate) struct SciLayout {
    pub items: Vec<Item>,
    pub n_int: usize,
    pub n_frac: usize,
    pub n_exp: usize,
    pub grouping: bool,
}

#[derive(Clone, Debug, PartialEq)]
pub(crate) enum Den {
    Max(u32),
    Fixed(u64),
}

#[derive(Clone, Debug)]
pub(crate) struct FracLayout {
    pub items: Vec<Item>,
    pub has_int: bool,
    pub n_int: usize,
    pub n_num: usize,
    pub den: Den,
    pub grouping: bool,
    pub scale: i32,

    pub sep_start: usize,
    pub num_start: usize,

    pub den_end: usize,
    pub num_has_q: bool,
    pub num_first_q: bool,
    pub num_has_zero: bool,
    pub int_has_q: bool,

    pub den_left: bool,
}

#[derive(Clone, Debug)]
pub(crate) struct DateLayout {
    pub items: Vec<Item>,

    pub subsec: u8,
    pub elapsed: bool,

    pub needs_date: bool,

    pub signed: bool,

    pub lcid: Option<u32>,

    pub hijri: bool,
}

#[derive(Clone, Debug)]
pub(crate) enum Body {
    Literal(Vec<Item>),
    General(Vec<Item>),
    Text(Vec<Item>),
    Number(NumLayout),
    Sci(SciLayout),
    Frac(FracLayout),
    Date(DateLayout),
}

#[derive(Clone, Debug)]
pub(crate) struct Section {
    pub cond: Option<(Cmp, f64)>,
    pub color: Option<Color>,
    pub body: Body,

    pub digit_sys: Option<u8>,
}

fn number_prefix(s: &str) -> f64 {
    let s = s.trim_start_matches(' ');
    let b = s.as_bytes();
    let mut i = 0;
    if i < b.len() && (b[i] == b'+' || b[i] == b'-') {
        i += 1;
    }
    let ds = i;
    while i < b.len() && b[i].is_ascii_digit() {
        i += 1;
    }
    if i < b.len() && b[i] == b'.' {
        i += 1;
        while i < b.len() && b[i].is_ascii_digit() {
            i += 1;
        }
    }
    if i == ds || (i == ds + 1 && b[ds] == b'.') {
        return 0.0;
    }
    let mut end = i;
    if i < b.len() && (b[i] == b'e' || b[i] == b'E') {
        let mut j = i + 1;
        if j < b.len() && (b[j] == b'+' || b[j] == b'-') {
            j += 1;
        }
        let es = j;
        while j < b.len() && b[j].is_ascii_digit() {
            j += 1;
        }
        if j > es {
            end = j;
        }
    }
    s[..end].parse().unwrap_or(0.0)
}

fn lcid_for_tag(t: &str) -> Option<u32> {
    const TAGS: [(&str, u32); 17] = [
        ("en-US", 0x0409),
        ("en-GB", 0x0809),
        ("en-AU", 0x0C09),
        ("en-CA", 0x1009),
        ("en-NZ", 0x1409),
        ("de-DE", 0x0407),
        ("de-CH", 0x0807),
        ("fr-FR", 0x040C),
        ("es-ES", 0x0C0A),
        ("es-MX", 0x080A),
        ("it-IT", 0x0410),
        ("ja-JP", 0x0411),
        ("zh-CN", 0x0804),
        ("pt-BR", 0x0416),
        ("pt-PT", 0x0816),
        ("nl-NL", 0x0413),
        ("ru-RU", 0x0419),
    ];
    TAGS.iter()
        .find(|(n, _)| n.eq_ignore_ascii_case(t))
        .map(|(_, id)| *id)
}

fn parse_bracket(s: &str) -> Option<Tok> {
    let first = s.chars().next()?;
    if let Some(rest) = s.strip_prefix('$') {
        let (currency, id) = match rest.find('-') {
            Some(i) => (&rest[..i], Some(&rest[i + 1..])),
            None => (rest, None),
        };
        let (lcid, tag) = match id {
            None => (None, None),
            Some(t)
                if t.len() >= 2 && t.is_char_boundary(2) && t[..2].eq_ignore_ascii_case("x-") =>
            {
                (None, Some(t.to_ascii_lowercase()))
            }
            Some(t) => (
                u32::from_str_radix(t, 16).ok().or_else(|| lcid_for_tag(t)),
                None,
            ),
        };
        return Some(Tok::Locale {
            currency: currency.to_string(),
            lcid,
            tag,
        });
    }
    if matches!(first, '<' | '>' | '=') {
        let (op, rest) = if let Some(r) = s.strip_prefix("<>") {
            (Cmp::Ne, r)
        } else if let Some(r) = s.strip_prefix("<=") {
            (Cmp::Le, r)
        } else if let Some(r) = s.strip_prefix(">=") {
            (Cmp::Ge, r)
        } else if let Some(r) = s.strip_prefix('<') {
            (Cmp::Lt, r)
        } else if let Some(r) = s.strip_prefix('>') {
            (Cmp::Gt, r)
        } else {
            (Cmp::Eq, &s[1..])
        };
        return Some(Tok::Cond(op, number_prefix(rest)));
    }
    let lower = s.to_ascii_lowercase();
    let color = match lower.as_str() {
        "black" => Some(Color::Black),
        "blue" => Some(Color::Blue),
        "cyan" => Some(Color::Cyan),
        "green" => Some(Color::Green),
        "magenta" => Some(Color::Magenta),
        "red" => Some(Color::Red),
        "white" => Some(Color::White),
        "yellow" => Some(Color::Yellow),
        _ => None,
    };
    if let Some(c) = color {
        return Some(Tok::Color(c));
    }
    if let Some(n) = lower.strip_prefix("color") {
        let n: u32 = n.parse().ok()?;
        return (1..=56)
            .contains(&n)
            .then_some(Tok::Color(Color::Indexed(n as u8)));
    }
    let c = lower.chars().next()?;
    if matches!(c, 'h' | 'm' | 's') && lower.chars().all(|x| x == c) {
        let unit = match c {
            'h' => Unit::H,
            'm' => Unit::M,
            _ => Unit::S,
        };
        return Some(Tok::Elapsed(unit, lower.len().min(255) as u8));
    }
    if let Some(n) = lower.strip_prefix("dbnum") {
        if matches!(n, "1" | "2" | "3" | "4") {
            return Some(Tok::Ignore);
        }
    }
    None
}

fn starts_with_ci(cs: &[char], i: usize, pat: &str) -> bool {
    let p: Vec<char> = pat.chars().collect();
    i + p.len() <= cs.len()
        && cs[i..i + p.len()]
            .iter()
            .zip(&p)
            .all(|(a, b)| a.to_lowercase().eq(b.to_lowercase()))
}

fn tokenize(code: &str) -> Option<Vec<Vec<Tok>>> {
    let cs: Vec<char> = code.chars().collect();
    let mut sections: Vec<Vec<Tok>> = vec![Vec::new()];
    let mut i = 0;
    while i < cs.len() {
        let c = cs[i];
        let cur = sections.last_mut().unwrap();
        let run = |letter: char| -> usize {
            cs[i..]
                .iter()
                .take_while(|x| x.to_ascii_lowercase() == letter)
                .count()
        };
        match c {
            '"' => {
                let j = (i + 1..cs.len()).find(|&j| cs[j] == '"')?;
                cur.push(Tok::Lit(cs[i + 1..j].iter().collect()));
                i = j + 1;
            }
            '\\' => {
                cur.push(Tok::Lit(cs.get(i + 1)?.to_string()));
                i += 2;
            }
            '_' => {
                cs.get(i + 1)?;
                cur.push(Tok::Space);
                i += 2;
            }
            '*' => {
                cur.push(Tok::Fill(*cs.get(i + 1)?));
                i += 2;
            }
            '[' => {
                let j = (i + 1..cs.len()).find(|&j| cs[j] == ']')?;
                let content: String = cs[i + 1..j].iter().collect();
                cur.push(parse_bracket(&content)?);
                i = j + 1;
            }
            ';' => {
                sections.push(Vec::new());
                i += 1;
            }
            '0' => {
                cur.push(Tok::Ph(Ph::Zero));
                i += 1;
            }
            '#' => {
                cur.push(Tok::Ph(Ph::Hash));
                i += 1;
            }
            '?' => {
                cur.push(Tok::Ph(Ph::Q));
                i += 1;
            }
            '.' => {
                cur.push(Tok::Point);
                i += 1;
            }
            ',' => {
                cur.push(Tok::Comma);
                i += 1;
            }
            '%' => {
                cur.push(Tok::Percent);
                i += 1;
            }
            '/' => {
                cur.push(Tok::Slash);
                i += 1;
            }
            '@' => {
                cur.push(Tok::At);
                i += 1;
            }
            ':' => {
                if cur.last() != Some(&Tok::Colon) {
                    cur.push(Tok::Colon);
                }
                i += 1;
            }
            '1'..='9' => {
                cur.push(Tok::LitDigit(c));
                i += 1;
            }
            'E' => match cs.get(i + 1) {
                Some('+') => {
                    cur.push(Tok::Exp(true));
                    i += 2;
                }
                Some('-') => {
                    cur.push(Tok::Exp(false));
                    i += 2;
                }
                _ => return None,
            },
            'G' | 'g' => {
                if starts_with_ci(&cs, i, "general") {
                    cur.push(Tok::General);
                    i += 7;
                } else {
                    let n = run('g');
                    cur.push(Tok::Date(DateField::Era, n.min(255) as u8));
                    i += n;
                }
            }
            'A' | 'a' if starts_with_ci(&cs, i, "am/pm") => {
                cur.push(Tok::AmPm);
                i += 5;
            }

            'A' | 'a' if run('a') >= 3 => {
                let n = run('a');
                cur.push(Tok::Date(DateField::Day, if n == 3 { 3 } else { 4 }));
                i += n;
            }
            '上' if starts_with_ci(&cs, i, "上午/下午") => {
                cur.push(Tok::AmPmCjk);
                i += 5;
            }
            'B' | 'b' if matches!(cs.get(i + 1), Some('1' | '2')) => {
                cur.push(Tok::Calendar(if cs[i + 1] == '1' { 1 } else { 2 }));
                i += 2;
            }
            'y' | 'Y' | 'm' | 'M' | 'd' | 'D' | 'h' | 'H' | 's' | 'S' | 'e' | 'b' => {
                let l = c.to_ascii_lowercase();
                let n = if c == 'e' || c == 'b' {
                    cs[i..].iter().take_while(|&&x| x == c).count()
                } else {
                    run(l)
                };
                let field = match l {
                    'y' => DateField::Year,
                    'm' => DateField::Month,
                    'd' => DateField::Day,
                    'h' => DateField::Hour,
                    's' => DateField::Second,
                    'e' => DateField::EYear,
                    _ => DateField::Buddhist,
                };
                cur.push(Tok::Date(field, n.min(255) as u8));
                i += n;
            }
            'B' | 'n' | 'N' => return None,
            _ => {
                cur.push(Tok::Lit(c.to_string()));
                i += 1;
            }
        }
    }
    if sections.len() > 4 {
        return None;
    }
    Some(sections)
}

fn is_date_tok(t: &Tok) -> bool {
    matches!(
        t,
        Tok::Date(..) | Tok::Elapsed(..) | Tok::AmPm | Tok::AmPmCjk
    )
}

fn literal_item(t: &Tok) -> Option<Item> {
    Some(match t {
        Tok::Lit(s) => Item::Lit(s.clone()),
        Tok::Colon => Item::Lit(":".into()),
        Tok::LitDigit(c) => Item::Lit(c.to_string()),
        Tok::Space => Item::Space,
        Tok::Fill(c) => Item::Fill(*c),
        _ => return None,
    })
}

fn compile_number(toks: &[Tok]) -> Option<Body> {
    let mut items = Vec::new();
    let (mut n_int, mut n_frac, mut grouping, mut scale, mut in_frac) =
        (0usize, 0usize, false, 0i32, false);
    for (i, t) in toks.iter().enumerate() {
        match t {
            Tok::Ph(p) => {
                if in_frac {
                    items.push(Item::Frac(*p));
                    n_frac += 1;
                } else {
                    items.push(Item::Int(*p));
                    n_int += 1;
                }
            }
            Tok::Point => {
                if in_frac {
                    items.push(Item::Lit(".".into()));
                } else {
                    in_frac = true;
                    items.push(Item::Point);
                }
            }
            Tok::Comma => {
                let prev = toks[..i].iter().rev().find(|t| !matches!(t, Tok::Comma));
                let next = toks[i + 1..].iter().find(|t| !matches!(t, Tok::Comma));
                if matches!(prev, Some(Tok::Ph(_) | Tok::Point)) {
                    let mut k = i + 1;
                    while k < toks.len() && matches!(toks[k], Tok::Comma) {
                        k += 1;
                    }
                    if k < toks.len() && matches!(toks[k], Tok::Point) {
                        k += 1;
                    }
                    while k < toks.len() && matches!(toks[k], Tok::Ph(_) | Tok::Comma) {
                        k += 1;
                    }
                    let later_digit = toks[k..]
                        .iter()
                        .any(|t| matches!(t, Tok::Ph(_) | Tok::Point));
                    if matches!(next, Some(Tok::Ph(_))) {
                        if !in_frac {
                            grouping = true;
                        }
                    } else if !later_digit {
                        scale -= 3;
                    }
                } else if i == 0 || !matches!(toks[i - 1], Tok::Comma) {
                    items.push(Item::Lit(",".into()));
                }
            }
            Tok::Percent => {
                scale += 2;
                items.push(Item::Lit("%".into()));
            }
            _ => items.push(literal_item(t)?),
        }
    }
    if n_frac > 127 {
        return None;
    }
    Some(Body::Number(NumLayout {
        items,
        n_int,
        n_frac,
        grouping,
        scale,
    }))
}

fn compile_sci(toks: &[Tok]) -> Option<Body> {
    let mut items = Vec::new();
    let (mut n_int, mut n_frac, mut n_exp, mut grouping) = (0usize, 0usize, 0usize, false);

    let mut phase = 0;
    for (i, t) in toks.iter().enumerate() {
        match t {
            Tok::Ph(p) => match phase {
                0 => {
                    items.push(Item::Int(*p));
                    n_int += 1;
                }
                1 => {
                    items.push(Item::Frac(*p));
                    n_frac += 1;
                }
                2 => {
                    items.push(Item::ExpDigit(*p));
                    n_exp += 1;
                }
                _ => items.push(Item::Lit("0".into())),
            },
            Tok::Point => match phase {
                0 => {
                    phase = 1;
                    items.push(Item::Point);
                }
                2 => {
                    phase = 3;
                    items.push(Item::Lit(".".into()));
                }
                _ => items.push(Item::Lit(".".into())),
            },
            Tok::Exp(plus) => {
                if phase >= 2
                    || !toks[i + 1..]
                        .iter()
                        .any(|x| matches!(x, Tok::Ph(_) | Tok::LitDigit(_)))
                {
                    return None;
                }
                phase = 2;
                items.push(Item::ExpMark(*plus));
            }
            Tok::Comma => {
                let prev = toks[..i].iter().rev().find(|t| !matches!(t, Tok::Comma));
                let next = toks[i + 1..].iter().find(|t| !matches!(t, Tok::Comma));
                if matches!(prev, Some(Tok::Ph(_) | Tok::Point)) {
                    if phase == 0
                        && matches!(prev, Some(Tok::Ph(_)))
                        && matches!(next, Some(Tok::Ph(_)))
                    {
                        grouping = true;
                    }
                } else if i == 0 || !matches!(toks[i - 1], Tok::Comma) {
                    items.push(Item::Lit(",".into()));
                }
            }
            Tok::Percent => items.push(Item::Lit("%".into())),
            _ => items.push(literal_item(t)?),
        }
    }
    if n_int + n_frac == 0 || n_frac > 127 {
        return None;
    }
    Some(Body::Sci(SciLayout {
        items,
        n_int,
        n_frac,
        n_exp,
        grouping,
    }))
}

fn compile_frac(toks: &[Tok]) -> Option<Body> {
    if toks.iter().any(|t| matches!(t, Tok::Exp(_))) {
        return None;
    }
    let slashes: Vec<usize> = toks
        .iter()
        .enumerate()
        .filter(|(_, t)| matches!(t, Tok::Slash))
        .map(|(i, _)| i)
        .collect();
    if slashes.len() != 1 {
        return None;
    }
    let s = slashes[0];

    let point = toks[..s].iter().position(|t| matches!(t, Tok::Point));
    if toks[s + 1..].iter().any(|t| matches!(t, Tok::Point)) {
        return None;
    }
    if let Some(p) = point {
        if toks[p..s].iter().any(|t| matches!(t, Tok::Ph(_))) {
            return None;
        }
    }
    let lit_between = |t: &Tok| {
        matches!(
            t,
            Tok::Lit(_) | Tok::Space | Tok::Fill(_) | Tok::LitDigit(_)
        )
    };
    let implicit = point.is_some_and(|p| {
        !matches!(
            toks[..p].iter().rev().find(|t| !matches!(t, Tok::Comma)),
            Some(Tok::Ph(_))
        )
    });

    let mut num_end = s;
    if implicit {
        num_end = point.unwrap();
    } else {
        while num_end > 0 && !matches!(toks[num_end - 1], Tok::Ph(_)) {
            let t = &toks[num_end - 1];
            let ok = lit_between(t)
                || (point.is_some_and(|p| num_end > p)
                    && matches!(t, Tok::Point | Tok::Comma | Tok::Percent))
                || matches!(t, Tok::Comma);
            if !ok {
                return None;
            }
            num_end -= 1;
        }
        if num_end == 0 {
            return None;
        }
    }
    let mut num_start = num_end;
    while !implicit && num_start > 0 && matches!(toks[num_start - 1], Tok::Ph(_)) {
        num_start -= 1;
    }

    if !implicit && num_start > 0 && matches!(toks[num_start - 1], Tok::Comma) {
        return None;
    }
    if implicit {
        let p = point.unwrap();
        if toks[p..s]
            .iter()
            .any(|t| !(lit_between(t) || matches!(t, Tok::Point | Tok::Comma | Tok::Percent)))
        {
            return None;
        }
    }

    let mut den_start = s + 1;
    while den_start < toks.len()
        && lit_between(&toks[den_start])
        && !matches!(toks[den_start], Tok::LitDigit(_))
    {
        den_start += 1;
    }
    let mut den_end = den_start;
    while den_end < toks.len() && matches!(toks[den_end], Tok::Ph(_) | Tok::LitDigit(_)) {
        den_end += 1;
    }
    if den_end == den_start {
        return None;
    }
    let den_toks = &toks[den_start..den_end];
    let has_lit_digit = den_toks.iter().any(|t| matches!(t, Tok::LitDigit(_)));
    let den = if has_lit_digit {
        let mut digits = String::new();
        for t in den_toks {
            match t {
                Tok::LitDigit(c) => digits.push(*c),
                Tok::Ph(Ph::Zero) => digits.push('0'),
                _ => return None,
            }
        }
        let v: u64 = digits.parse().ok()?;
        if digits.len() > 5 || v >= 100_000 {
            return None;
        }
        Den::Fixed(v)
    } else {
        Den::Max(den_toks.len().min(9) as u32)
    };
    let last_int = toks[..num_start]
        .iter()
        .rposition(|t| matches!(t, Tok::Ph(_)));
    let has_int = last_int.is_some();
    let first_int = toks[..num_start]
        .iter()
        .position(|t| matches!(t, Tok::Ph(_)));

    let mut items = Vec::new();
    let (mut n_int, mut n_num, mut grouping, mut scale) = (0usize, 0usize, false, 0i32);
    let (mut sep_start, mut num_item_start, mut den_item_end) = (0usize, 0usize, 0usize);
    let (mut num_has_q, mut num_has_zero, mut int_has_q) = (false, false, false);
    let mut num_first_q = false;
    let mut den_left = false;
    for (i, t) in toks.iter().enumerate() {
        if Some(i) == last_int.map(|l| l + 1) {
            sep_start = items.len();
        }
        if i == num_start {
            num_item_start = items.len();
            if implicit {
                items.push(Item::NumAll);
            }
        }
        match t {
            Tok::Ph(p) if i < num_start => {
                items.push(Item::Int(*p));
                n_int += 1;
                int_has_q |= *p == Ph::Q;
            }
            Tok::Ph(p) if i < num_end => {
                if i == num_start {
                    num_first_q = *p == Ph::Q;
                }
                items.push(Item::Num(*p));
                n_num += 1;
                num_has_q |= *p == Ph::Q;
                num_has_zero |= *p == Ph::Zero;
            }
            Tok::Slash => items.push(Item::Slash),
            _ if (den_start..den_end).contains(&i) => {
                if let Den::Max(_) = den {
                    let Tok::Ph(p) = t else { return None };
                    if i == den_start {
                        den_left = *p == Ph::Q;
                    }
                    items.push(if point.is_some() {
                        Item::DenDec(*p)
                    } else {
                        Item::Den(*p)
                    });
                } else if i == den_start {
                    let s: String = den_toks
                        .iter()
                        .map(|t| match t {
                            Tok::LitDigit(c) => *c,
                            _ => '0',
                        })
                        .collect();
                    items.push(Item::DenFixed(s));
                }
                if i + 1 == den_end {
                    den_item_end = items.len();
                }
            }
            Tok::Ph(_) => items.push(Item::Lit("0".into())),
            Tok::Point => items.push(Item::Lit(".".into())),
            Tok::Comma
                if (i >= num_end && i < s)
                    || (implicit && last_int.is_some_and(|l| i > l) && i < s) =>
            {
                let prev = toks[..i].iter().rev().find(|t| !matches!(t, Tok::Comma));
                if !matches!(prev, Some(Tok::Ph(_) | Tok::Point))
                    && !matches!(toks[i - 1], Tok::Comma)
                {
                    items.push(Item::Lit(",".into()));
                }
            }
            Tok::Comma if i >= den_end => {
                if toks[den_end..i].iter().all(|t| matches!(t, Tok::Comma)) {
                    scale -= 3;
                } else if !matches!(toks[i - 1], Tok::Comma) {
                    items.push(Item::Lit(",".into()));
                }
            }
            Tok::Comma => {
                let in_int = first_int.is_some_and(|f| i > f) && last_int.is_some_and(|l| i < l);
                if in_int {
                    grouping = true;
                }

                let leading = first_int.map_or(i < num_start, |f| i < f);
                if leading && (i == 0 || !matches!(toks[i - 1], Tok::Comma)) {
                    items.push(Item::Lit(",".into()));
                }
            }
            Tok::Percent => {
                scale += 2;
                items.push(Item::Lit("%".into()));
            }
            _ => items.push(literal_item(t)?),
        }
    }
    if !has_int {
        sep_start = num_item_start;
    }
    Some(Body::Frac(FracLayout {
        items,
        has_int,
        n_int,
        n_num,
        den,
        grouping,
        scale: scale.rem_euclid(256),
        sep_start,
        num_start: num_item_start,
        den_end: den_item_end,
        num_has_q,
        num_first_q,
        num_has_zero,
        int_has_q,
        den_left,
    }))
}

fn compile_date(toks: &[Tok], lcid: Option<u32>, hijri: bool) -> Option<Body> {
    let mut items = Vec::new();
    let (mut subsec, mut ampm, mut elapsed) = (0u8, false, 0);
    let mut i = 0;
    while i < toks.len() {
        let t = &toks[i];
        match t {
            Tok::Date(f, n) => items.push(Item::Date(match f {
                DateField::Year => DateTok::Year(*n),
                DateField::Month => DateTok::Month(*n),
                DateField::Day => DateTok::Day(*n),
                DateField::Hour => DateTok::Hour(*n),
                DateField::Second => DateTok::Second(*n),
                DateField::EYear => DateTok::EYear(*n),
                DateField::Era => DateTok::Era(*n),
                DateField::Buddhist => DateTok::Buddhist(*n),
            })),
            Tok::Elapsed(u, n) => {
                elapsed += 1;
                items.push(Item::Date(match u {
                    Unit::H => DateTok::ElapsedH(*n),
                    Unit::M => DateTok::ElapsedM(*n),
                    Unit::S => DateTok::ElapsedS(*n),
                }));
            }
            Tok::AmPm => {
                ampm = true;
                items.push(Item::Date(DateTok::AmPm));
            }
            Tok::AmPmCjk => {
                ampm = true;
                items.push(Item::Date(DateTok::AmPmCjk));
            }
            Tok::Point => {
                let k = toks[i + 1..]
                    .iter()
                    .take_while(|t| matches!(t, Tok::Ph(Ph::Zero)))
                    .count();
                if k == 0 {
                    items.push(Item::Lit(".".into()));
                } else {
                    if k > 3
                        || items.is_empty()
                        || matches!(items.last(), Some(Item::Date(DateTok::General(_))))
                    {
                        return None;
                    }
                    subsec = subsec.max(k as u8);
                    items.push(Item::Date(DateTok::SubSec(k as u8)));
                    i += k;
                }
            }
            Tok::General => items.push(Item::Date(DateTok::General(elapsed > 0))),

            Tok::Comma if i > 0 && matches!(toks[i - 1], Tok::Comma) => {}
            Tok::Comma => items.push(Item::Lit(",".into())),
            Tok::Slash => items.push(Item::Lit("/".into())),
            Tok::Lit(_) | Tok::Space | Tok::Fill(_) | Tok::Colon => items.push(literal_item(t)?),
            _ => return None,
        }
        i += 1;
    }
    if elapsed > 1 {
        return None;
    }

    if let Some(e) = items.iter().position(|it| {
        matches!(
            it,
            Item::Date(DateTok::ElapsedH(_) | DateTok::ElapsedM(_) | DateTok::ElapsedS(_))
        )
    }) {
        if items[e + 1..]
            .iter()
            .any(|it| matches!(it, Item::Date(DateTok::AmPm | DateTok::AmPmCjk)))
        {
            return None;
        }
    }

    if let Some(e) = items.iter().position(|it| {
        matches!(
            it,
            Item::Date(DateTok::ElapsedH(_) | DateTok::ElapsedM(_) | DateTok::ElapsedS(_))
        )
    }) {
        let rank = match items[e] {
            Item::Date(DateTok::ElapsedH(_)) => 3,
            Item::Date(DateTok::ElapsedM(_)) => 2,
            _ => 1,
        };
        let clash = items[..e].iter().any(|it| match it {
            Item::Date(DateTok::Hour(_)) => true,
            Item::Date(DateTok::Second(_)) => rank == 1,
            _ => false,
        });
        if clash {
            return None;
        }
    }

    let elapsed_hours = items
        .iter()
        .any(|it| matches!(it, Item::Date(DateTok::ElapsedH(_))));
    let (mut armed, mut minute_seen, mut elapsed_minutes) = (0u32, false, false);
    for k in 0..items.len() {
        match items[k] {
            Item::Date(DateTok::Hour(_)) | Item::Date(DateTok::ElapsedH(_)) => armed += 1,

            Item::Date(DateTok::ElapsedM(_)) => {
                minute_seen = true;
                elapsed_minutes = true;
            }
            Item::Date(DateTok::Second(_) | DateTok::SubSec(_) | DateTok::ElapsedS(_))
                if !minute_seen =>
            {
                armed = armed.max(1)
            }
            Item::Date(DateTok::Month(n)) if n <= 2 => {
                let next = items[k + 1..].iter().find_map(|it| match it {
                    Item::Date(
                        DateTok::AmPm | DateTok::AmPmCjk | DateTok::Era(_) | DateTok::EYear(_),
                    ) => None,
                    Item::Date(d) => Some(*d),
                    _ => None,
                });
                let before_seconds = !(minute_seen && (elapsed_hours || elapsed_minutes))
                    && matches!(next, Some(DateTok::Second(_) | DateTok::SubSec(_)));
                if armed > 0 || before_seconds {
                    items[k] = Item::Date(DateTok::Minute(n));
                    armed = armed.saturating_sub(1);
                    minute_seen = true;
                }
            }
            _ => {}
        }
    }
    if let Some(e) = items
        .iter()
        .position(|it| matches!(it, Item::Date(DateTok::ElapsedM(_) | DateTok::ElapsedS(_))))
    {
        if items[..e]
            .iter()
            .any(|it| matches!(it, Item::Date(DateTok::Minute(_))))
        {
            return None;
        }
    }

    if let Some(e) = items.iter().position(|it| {
        matches!(
            it,
            Item::Date(DateTok::ElapsedH(_) | DateTok::ElapsedM(_) | DateTok::ElapsedS(_))
        )
    }) {
        let Item::Date(unit) = items[e] else {
            unreachable!()
        };
        let (rank, width) = match unit {
            DateTok::ElapsedH(n) => (3, n),
            DateTok::ElapsedM(n) => (2, n),
            DateTok::ElapsedS(n) => (1, n),
            _ => unreachable!(),
        };
        let clock_rank = |d: &DateTok| match d {
            DateTok::Hour(_) => Some(3),
            DateTok::Minute(_) => Some(2),
            DateTok::Second(_) => Some(1),
            _ => None,
        };
        let left = |r: i32, n: u8| match r {
            3 => DateTok::LeftH(n),
            2 => DateTok::LeftM(n),
            _ => DateTok::LeftS(n),
        };
        let preceded = items[..e]
            .iter()
            .any(|it| matches!(it, Item::Date(d) if !matches!(d, DateTok::SubSec(_))));
        let larger_after = items[e + 1..]
            .iter()
            .any(|it| matches!(it, Item::Date(d) if clock_rank(d).is_some_and(|r| r > rank)));
        if preceded {
            items[e] = Item::Date(left(rank, width));
        } else if larger_after {
            items[e] = Item::Date(match rank {
                3 => DateTok::Hour(width),
                2 => DateTok::Minute(width),
                _ => DateTok::Second(width),
            });
        }

        let (mut sub_seen, mut larger_seen) = (false, false);
        let top = items[e + 1..]
            .iter()
            .filter_map(|it| match it {
                Item::Date(d) => clock_rank(d),
                _ => None,
            })
            .max()
            .unwrap_or(0);
        for it in items[e + 1..].iter_mut() {
            if let Item::Date(d) = it {
                let between =
                    !matches!(d, DateTok::AmPm | DateTok::AmPmCjk) && clock_rank(d) != Some(rank);
                if let Some(r) = clock_rank(d) {
                    let n = match *d {
                        DateTok::Hour(n) | DateTok::Minute(n) | DateTok::Second(n) => n,
                        _ => 0,
                    };
                    if r > rank && r == top && !larger_seen {
                        *d = left(r, n);
                        larger_seen = true;
                    } else if r > rank || (r == rank && larger_after && !preceded) {
                    } else if r == rank {
                        *d = if preceded || sub_seen {
                            left(r, n)
                        } else {
                            match r {
                                3 => DateTok::ElapsedH(n),
                                2 => DateTok::ElapsedM(n),
                                _ => DateTok::ElapsedS(n),
                            }
                        };
                    }
                }
                sub_seen |= between;
            }
        }
    }
    let needs_date = items.iter().any(|it| {
        matches!(it, Item::Date(d) if !matches!(d, DateTok::ElapsedH(_) | DateTok::ElapsedM(_) | DateTok::ElapsedS(_) | DateTok::SubSec(_) | DateTok::General(_) | DateTok::LeftH(_) | DateTok::LeftM(_) | DateTok::LeftS(_)))
    });

    if ampm {
        let hours: Vec<usize> = (0..items.len())
            .filter(|&k| matches!(items[k], Item::Date(DateTok::Hour(_))))
            .collect();
        let mut twelve: Vec<usize> = hours.last().copied().into_iter().collect();
        for (a, it) in items.iter().enumerate() {
            if matches!(it, Item::Date(DateTok::AmPm | DateTok::AmPmCjk)) {
                twelve.extend(hours.iter().copied().filter(|&h| h < a).max());
            }
        }
        for k in twelve {
            if let Item::Date(DateTok::Hour(n)) = items[k] {
                items[k] = Item::Date(DateTok::Hour12(n));
            }
        }
    }
    let signed = elapsed == 1 && !needs_date && subsec == 0;
    Some(Body::Date(DateLayout {
        items,
        subsec,
        elapsed: elapsed == 1,
        needs_date,
        signed,
        lcid,
        hijri,
    }))
}

fn compile_section(raw: Vec<Tok>) -> Option<Section> {
    let (mut color, mut cond, mut lcid, mut tag) = (None, None, None, None);
    let mut calendar: Option<u8> = None;
    let mut seen_locale = false;

    let implicit_general = !raw.is_empty()
        && raw
            .iter()
            .all(|t| matches!(t, Tok::Color(_) | Tok::Cond(..) | Tok::Ignore));
    let mut toks = Vec::with_capacity(raw.len());
    for t in raw {
        match t {
            Tok::Color(c) => {
                if color.is_some() {
                    return None;
                }
                color = Some(c);
            }
            Tok::Cond(op, v) => {
                if cond.is_some() {
                    return None;
                }
                cond = Some((op, v));
            }
            Tok::Locale {
                currency,
                lcid: l,
                tag: g,
            } => {
                if seen_locale {
                    return None;
                }
                seen_locale = true;
                if l.is_some() {
                    lcid = l;
                }
                if g.is_some() {
                    tag = g;
                }
                if !currency.is_empty() {
                    toks.push(Tok::Lit(currency));
                }
            }
            Tok::Ignore => {}

            Tok::Calendar(c) => {
                if toks.iter().any(|t| !matches!(t, Tok::Calendar(_))) {
                    return None;
                }
                if calendar.is_some_and(|k| k != c) {
                    return None;
                }
                calendar = Some(c);
            }
            t => toks.push(t),
        }
    }
    let hijri = calendar == Some(2);

    let digit_sys = lcid.map(|l| (l >> 24) as u8);
    let sys = match (lcid.map(|l| l & 0xFFFF), tag.as_deref()) {
        (Some(0xF800), _) | (_, Some("x-sysdate")) => Some(("dddd, d mmmm yyyy", None)),
        (Some(0xF400), _) | (_, Some("x-systime")) => Some(("h:mm:ss AM/PM", Some(0x0409))),
        _ => None,
    };
    if let Some((code, names)) = sys {
        toks = tokenize(code)?.pop()?;
        lcid = names;
    }
    let has = |f: &dyn Fn(&Tok) -> bool| toks.iter().any(f);
    let has_at = has(&|t| matches!(t, Tok::At));
    let has_date = has(&is_date_tok);
    let has_general = has(&|t| matches!(t, Tok::General));
    let has_digits = has(&|t| matches!(t, Tok::Ph(_)));
    let has_point = has(&|t| matches!(t, Tok::Point));

    let mut has_date = has_date;
    if !has_date && !has_at && !has_general {
        if let Some(c) = toks.iter().position(|t| matches!(t, Tok::Colon)) {
            if toks[c..].iter().any(|t| matches!(t, Tok::Ph(_))) {
                let ph_before = toks[..c].iter().any(|t| matches!(t, Tok::Ph(_)));
                let subsec_only = toks[c..].iter().enumerate().all(|(j, t)| match t {
                    Tok::Ph(Ph::Zero) => {
                        let k = c + j;
                        toks[..k]
                            .iter()
                            .rev()
                            .find(|t| !matches!(t, Tok::Ph(Ph::Zero)))
                            == Some(&Tok::Point)
                    }
                    Tok::Ph(_) => false,
                    _ => true,
                });
                if ph_before || !subsec_only {
                    return None;
                }
                has_date = true;
            }
        }
    }
    let body = if implicit_general {
        Body::General(vec![Item::General])
    } else if has_at {
        if has_date || has_digits || has_general || has(&|t| matches!(t, Tok::Exp(_) | Tok::Slash))
        {
            return None;
        }
        let mut items = Vec::new();
        for (i, t) in toks.iter().enumerate() {
            items.push(match t {
                Tok::At => Item::Text,
                Tok::Point => Item::Lit(".".into()),
                Tok::Comma if i > 0 && matches!(toks[i - 1], Tok::Comma) => continue,
                Tok::Comma => Item::Lit(",".into()),
                Tok::Percent => Item::Lit("%".into()),
                _ => literal_item(t)?,
            });
        }
        Body::Text(items)
    } else if has_date {
        compile_date(&toks, lcid, hijri)?
    } else if has_general {
        if has_digits || has(&|t| matches!(t, Tok::Exp(_) | Tok::Slash)) {
            return None;
        }
        let mut items = Vec::new();
        for (i, t) in toks.iter().enumerate() {
            items.push(match t {
                Tok::General => Item::General,
                Tok::Point => Item::Lit(".".into()),
                Tok::Comma if i > 0 && matches!(toks[i - 1], Tok::Comma) => continue,
                Tok::Comma => Item::Lit(",".into()),
                Tok::Percent => Item::Lit("%".into()),
                _ => literal_item(t)?,
            });
        }
        Body::General(items)
    } else if has_digits || has_point {
        if has(&|t| matches!(t, Tok::Slash)) {
            compile_frac(&toks)?
        } else if has(&|t| matches!(t, Tok::Exp(_))) {
            compile_sci(&toks)?
        } else {
            compile_number(&toks)?
        }
    } else {
        let mut items = Vec::new();
        for (i, t) in toks.iter().enumerate() {
            items.push(match t {
                Tok::Comma if i > 0 && matches!(toks[i - 1], Tok::Comma) => continue,
                Tok::Comma => Item::Lit(",".into()),
                Tok::Percent => Item::Lit("%".into()),

                Tok::Exp(plus)
                    if toks[..i].iter().any(|x| matches!(x, Tok::LitDigit(_)))
                        && toks[i + 1..].iter().any(|x| matches!(x, Tok::LitDigit(_))) =>
                {
                    Item::Lit(if *plus { "E+" } else { "E" }.into())
                }
                _ => literal_item(t)?,
            });
        }
        Body::Literal(items)
    };
    Some(Section {
        cond,
        color,
        body,
        digit_sys,
    })
}

pub(crate) fn parse_format(code: &str) -> Option<NumberFormat> {
    if code.encode_utf16().count() > 255 {
        return None;
    }
    let raw = tokenize(code)?;
    let mut sections: Vec<Section> = raw
        .into_iter()
        .map(compile_section)
        .collect::<Option<_>>()?;
    let n = sections.len();

    if n == 4 {
        if let Body::General(items) = &sections[3].body {
            let items = items
                .iter()
                .map(|it| {
                    if *it == Item::General {
                        Item::Text
                    } else {
                        it.clone()
                    }
                })
                .collect();
            sections[3].body = Body::Text(items);
        }
    }
    let text_at = sections
        .iter()
        .position(|s| matches!(s.body, Body::Text(_)));
    if let Some(t) = text_at {
        if t != n - 1 {
            return None;
        }
    }
    if n == 4 && !matches!(sections[3].body, Body::Text(_) | Body::Literal(_)) {
        return None;
    }
    let text_section = text_at.or(if n == 4 { Some(3) } else { None });
    let numeric = text_section.unwrap_or(n).min(3);

    if sections
        .iter()
        .enumerate()
        .any(|(i, s)| s.cond.is_some() && (i >= 2 || i >= numeric))
    {
        return None;
    }
    Some(NumberFormat {
        sections,
        text_section,
        numeric,
        code_lens: super::codelen::section_code_lens(code),
    })
}
