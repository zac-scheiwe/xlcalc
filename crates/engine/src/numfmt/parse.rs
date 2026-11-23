use super::dbnum::Db;
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

    Minute,
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

    Space(char),

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

    Compiled,
    Date(DateField, u8),
    Elapsed(Unit, u8),
    Color(Color),
    Cond(Cmp, f64),
    Locale {
        currency: String,
        lcid: Option<u32>,
        tag: Option<String>,
    },

    DbNum(u8),

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
    Space(char),
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

    DenPoint,
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

#[derive(Clone, Debug)]
pub(crate) struct FracLayout {
    pub items: Vec<Item>,
    pub has_int: bool,
    pub n_int: usize,
    pub n_num: usize,

    pub den_field: i16,
    pub grouping: bool,
    pub scale: i32,

    pub err: bool,
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

    pub db: Option<Db>,
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

pub(super) fn is_x_tag(t: &str) -> bool {
    t.chars().count() > 3 && t.get(..2).is_some_and(|p| p.eq_ignore_ascii_case("x-"))
}

pub(super) fn excel_hex(t: &str) -> Option<u32> {
    let mut acc: i32 = 0;
    for c in t.chars() {
        let d = match c.to_digit(36) {
            Some(d) => d as i32,
            None => c as i32 - 48,
        };
        acc = acc.wrapping_mul(16).wrapping_add(d);
    }

    if acc < 0 && acc > -0x400 {
        return None;
    }
    Some(acc as u32)
}

fn parse_bracket(s: &str) -> Option<Tok> {
    if s.is_empty() {
        return Some(Tok::Elapsed(Unit::M, 0));
    }
    let first = s.chars().next()?;
    if let Some(rest) = s.strip_prefix('$') {
        let (currency, id) = match rest.find('-') {
            Some(i) => (&rest[..i], Some(&rest[i + 1..])),
            None => (rest, None),
        };
        let (lcid, tag) = match id {
            None => (None, None),
            Some(t) if is_x_tag(t) => (None, Some(t.to_ascii_lowercase())),
            Some(t) => (
                match lcid_for_tag(t) {
                    Some(l) => Some(l),

                    None => Some(excel_hex(t)?),
                },
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

    const NAMES: [(&str, Color); 8] = [
        ("black", Color::Black),
        ("blue", Color::Blue),
        ("cyan", Color::Cyan),
        ("green", Color::Green),
        ("magenta", Color::Magenta),
        ("red", Color::Red),
        ("white", Color::White),
        ("yellow", Color::Yellow),
    ];
    if let Some((_, c)) = NAMES.iter().find(|(n, _)| lower.starts_with(n)) {
        return Some(Tok::Color(*c));
    }
    if let Some(n) = lower.strip_prefix("color") {
        let digits: String = n
            .trim_start_matches([' ', '\t'])
            .chars()
            .take_while(char::is_ascii_digit)
            .collect();
        let n: u32 = digits.trim_start_matches('0').parse().unwrap_or(0);
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
            return Some(Tok::DbNum(n.as_bytes()[0] - b'0'));
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
            '\u{e000}' => {
                cur.push(Tok::Compiled);
                i += 1;
            }
            '\u{e001}' | '\u{e002}' => {
                cur.push(Tok::Date(DateField::Minute, (c as u32 - 0xe000) as u8));
                i += 1;
            }
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
                cur.push(Tok::Space(*cs.get(i + 1)?));
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

            ';' | '\u{37e}' => {
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
                if cs.get(i + 2).is_some_and(char::is_ascii_digit) {
                    return None;
                }
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

            'B' if i + 1 < cs.len() => {
                let n = cs[i..]
                    .iter()
                    .take_while(|&&x| x == 'b' || x == 'B')
                    .count();
                cur.push(Tok::Date(DateField::Buddhist, n.min(255) as u8));
                i += n;
            }
            'B' | 'n' | 'N' => return None,
            c if code_lookalike(c) => return None,
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

fn code_lookalike(c: char) -> bool {
    matches!(u32::from(c),
        0xC8..=0xCB | 0xD0 | 0xD1 | 0xDD | 0xE8..=0xEB | 0xF0 | 0xF1 | 0xFD | 0xFF
        | 0x10E..=0x127 | 0x143..=0x149 | 0x15A..=0x161 | 0x176..=0x178
        | 0x180..=0x185 | 0x18A..=0x18F | 0x193 | 0x194 | 0x19C..=0x19E
        | 0x1E02..=0x1E07 | 0x1E0A..=0x1E1D | 0x1E20)
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
        Tok::Space(c) => Item::Space(*c),
        Tok::Fill(c) => Item::Fill(*c),
        Tok::AmPm => Item::Date(DateTok::AmPm),
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

                    while k < toks.len() && matches!(toks[k], Tok::Ph(_) | Tok::Comma | Tok::Point)
                    {
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

                _ => items.push(Item::Lit(
                    match p {
                        Ph::Zero => "0",
                        Ph::Hash => "",
                        Ph::Q => " ",
                    }
                    .into(),
                )),
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
    if let Some(p) = point {
        if toks[p..s].iter().any(|t| matches!(t, Tok::Ph(_))) {
            return None;
        }
    }
    let lit_between = |t: &Tok| {
        matches!(
            t,
            Tok::Lit(_) | Tok::Space(_) | Tok::Fill(_) | Tok::LitDigit(_)
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
                || (point.is_some_and(|p| num_end > p) && matches!(t, Tok::Point))
                || matches!(t, Tok::Comma | Tok::Percent);
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

    if matches!(toks.get(den_end), Some(Tok::Point)) {
        return None;
    }
    let den_toks = &toks[den_start..den_end];
    let has_lit_digit = den_toks.iter().any(|t| matches!(t, Tok::LitDigit(_)));
    if has_lit_digit
        && !den_toks
            .iter()
            .all(|t| matches!(t, Tok::LitDigit(_) | Tok::Ph(Ph::Zero)))
    {
        return None;
    }
    let den_field = den_field(den_toks, &toks[den_end..])?;
    let tail = toks[den_end..].iter().any(|t| matches!(t, Tok::Ph(_)));
    let garble = has_lit_digit
        && den_field == 0
        && toks[den_end..]
            .iter()
            .any(|t| matches!(t, Tok::Ph(_) | Tok::Point));

    let comma_err = matches!(toks.get(den_end), Some(Tok::Comma))
        && toks[den_end..]
            .iter()
            .find(|t| !matches!(t, Tok::Comma))
            .is_some_and(|t| matches!(t, Tok::Ph(_) | Tok::Point));
    let last_int = toks[..num_start]
        .iter()
        .rposition(|t| matches!(t, Tok::Ph(_)));
    let has_int = last_int.is_some();
    let first_int = toks[..num_start]
        .iter()
        .position(|t| matches!(t, Tok::Ph(_)));

    let mut items = Vec::new();
    let (mut n_int, mut n_num, mut grouping, mut scale) = (0usize, 0usize, false, 0i32);
    for (i, t) in toks.iter().enumerate() {
        match t {
            Tok::Ph(p) if i < num_start => {
                items.push(Item::Int(*p));
                n_int += 1;
            }
            Tok::Ph(p) if i < num_end => {
                items.push(Item::Num(*p));
                n_num += 1;
            }
            Tok::Slash => items.push(Item::Slash),
            _ if (den_start..den_end).contains(&i) => {
                if !has_lit_digit {
                    let Tok::Ph(p) = t else { return None };
                    items.push(Item::Den(*p));
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
            }
            Tok::Ph(p) => items.push(Item::Den(*p)),
            Tok::Point => items.push(Item::DenPoint),
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

            Tok::Comma
                if i > den_end
                    && matches!(toks[i - 1], Tok::Ph(_) | Tok::LitDigit(_) | Tok::Comma)
                    && toks[i..]
                        .iter()
                        .any(|t| matches!(t, Tok::Ph(_) | Tok::LitDigit(_))) => {}
            Tok::Comma
                if tail
                    && i > den_end
                    && !toks[i..].iter().any(|t| matches!(t, Tok::Ph(_)))
                    && toks[den_end..i].iter().any(|t| matches!(t, Tok::Ph(_))) =>
            {
                scale -= 3;
            }
            Tok::Comma if (garble || comma_err) && i == den_end => {}

            Tok::Comma
                if i == den_end && toks[den_end..].iter().any(|t| matches!(t, Tok::Ph(_))) => {}
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
    Some(Body::Frac(FracLayout {
        err: (garble && matches!(toks.get(den_end), Some(Tok::Comma))) || comma_err,
        items,
        has_int,
        n_int,
        n_num,
        den_field,
        grouping,
        scale: scale.rem_euclid(256),
    }))
}

fn den_field(den: &[Tok], tail: &[Tok]) -> Option<i16> {
    if !den.iter().any(|t| matches!(t, Tok::LitDigit(_))) {
        return Some(den.len().min(255) as i16);
    }
    let in_group = |t: &Tok| matches!(t, Tok::LitDigit(_) | Tok::Ph(_) | Tok::Point | Tok::Comma);
    let mut acc = group_value(den);
    let mut i = 0;
    while i < tail.len() {
        if !in_group(&tail[i]) || matches!(tail[i], Tok::Comma) {
            i += 1;
            continue;
        }
        let start = i;

        while i < tail.len()
            && (in_group(&tail[i])
                && (!matches!(tail[i], Tok::Comma)
                    || tail[i + 1..]
                        .first()
                        .is_some_and(|t| matches!(t, Tok::LitDigit(_) | Tok::Ph(_)))))
        {
            i += 1;
        }
        acc = group_value(&tail[start..i]);
    }

    let v = acc.0.trunc();
    if v >= 100_000.0 || acc.1 > 5 {
        return None;
    }
    Some((v as i32).wrapping_neg() as i16)
}

fn group_value(toks: &[Tok]) -> (f64, usize) {
    let (mut acc, mut point, mut places, mut digits) = (0.0f64, false, 0i32, 0usize);
    for t in toks {
        match t {
            Tok::Point => point = true,
            Tok::LitDigit(c) => {
                let d = c.to_digit(10).map_or(0.0, f64::from);
                if point {
                    acc += d / 10f64.powi(places);
                } else {
                    acc = acc * 10.0 + d;
                    digits += 1;
                }
            }
            Tok::Ph(Ph::Zero) if !point => {
                acc *= 10.0;
                digits += 1;
            }
            Tok::Ph(Ph::Hash | Ph::Q) if point => places += 1,
            _ => {}
        }
    }
    (acc, digits)
}

fn has_date_field(items: &[Item]) -> bool {
    items
        .iter()
        .any(|it| matches!(it, Item::Date(d) if !matches!(d, DateTok::General(_))))
}

fn compile_date(
    toks: &[Tok],
    lcid: Option<u32>,
    hijri: bool,
    cal_prefix: bool,
    compiled: bool,
) -> Option<Body> {
    let mut items = Vec::new();
    let (mut subsec, mut ampm, mut elapsed) = (0u8, false, 0);
    let mut i = 0;
    while i < toks.len() {
        let t = &toks[i];
        match t {
            Tok::Date(f, n) => items.push(Item::Date(match f {
                DateField::Year => DateTok::Year(*n),
                DateField::Month => DateTok::Month(*n),
                DateField::Minute => DateTok::Minute(*n),
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
                    Unit::M => DateTok::ElapsedM((*n).max(1)),
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
                    if !has_date_field(&items) {
                        return None;
                    }
                    items.push(Item::Lit(".".into()));
                } else {
                    if k > 3
                        || (items.is_empty() && !cal_prefix)
                        || matches!(items.last(), Some(Item::Date(DateTok::General(_))))
                    {
                        return None;
                    }
                    subsec = subsec.max(k as u8);
                    items.push(Item::Date(DateTok::SubSec(k as u8)));
                    i += k;
                }
            }
            Tok::General => items.push(Item::Date(DateTok::General(elapsed > 0 && !ampm))),

            Tok::Comma if i > 0 && matches!(toks[i - 1], Tok::Comma) => {}
            Tok::Comma => items.push(Item::Lit(",".into())),

            Tok::Slash => {
                if !has_date_field(&items) {
                    return None;
                }
                items.push(Item::Lit("/".into()));
            }
            Tok::Lit(_) | Tok::Space(_) | Tok::Fill(_) | Tok::Colon => items.push(literal_item(t)?),
            _ => return None,
        }
        i += 1;
    }

    if items
        .iter()
        .filter(|it| matches!(it, Item::Date(DateTok::General(_))))
        .count()
        > 1
    {
        return None;
    }

    if !compiled {
        super::ampm_elapsed::resolve_items(&mut items)?;
    }
    assign_elapsed_roles(&mut items, compiled);
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
        elapsed: elapsed >= 1,
        needs_date,
        signed,
        lcid,
        hijri,
    }))
}

fn assign_elapsed_roles(items: &mut [Item], compiled: bool) {
    fn unit(d: &DateTok) -> Option<(i32, u8, bool)> {
        match *d {
            DateTok::Hour(n) => Some((3, n, false)),
            DateTok::Minute(n) => Some((2, n, false)),
            DateTok::Second(n) => Some((1, n, false)),
            DateTok::ElapsedH(n) => Some((3, n, true)),
            DateTok::ElapsedM(n) => Some((2, n, true)),
            DateTok::ElapsedS(n) => Some((1, n, true)),
            _ => None,
        }
    }
    if !items.iter().any(|it| {
        matches!(
            it,
            Item::Date(DateTok::ElapsedH(_) | DateTok::ElapsedM(_) | DateTok::ElapsedS(_))
        )
    }) {
        return;
    }
    let top = items
        .iter()
        .filter_map(|it| match it {
            Item::Date(d) => unit(d).map(|u| u.0),
            _ => None,
        })
        .max()
        .unwrap_or(0);

    let setting = items.iter().any(|it| {
        matches!(it, Item::Date(d) if unit(d).is_none_or(|u| u.0 != top) && !matches!(d, DateTok::General(_)))
    });
    for it in items.iter_mut() {
        if let Item::Date(DateTok::General(serial)) = it {
            *serial = !setting;
        }
    }
    let mut set = false;
    for it in items.iter_mut() {
        let Item::Date(d) = it else { continue };
        match unit(d) {
            Some((rank, _, false)) if compiled && rank == top => set = true,
            Some((rank, n, bracket)) if rank == top => {
                let n = if bracket { n } else { n.min(2) };
                *d = match (rank, set) {
                    (3, false) => DateTok::ElapsedH(n),
                    (3, true) => DateTok::LeftH(n),
                    (2, false) => DateTok::ElapsedM(n),
                    (2, true) => DateTok::LeftM(n),
                    (_, false) => DateTok::ElapsedS(n),
                    (_, true) => DateTok::LeftS(n),
                };
            }
            Some((rank, n, bracket)) => {
                if bracket {
                    *d = match rank {
                        3 => DateTok::Hour(n),
                        2 => DateTok::Minute(n),
                        _ => DateTok::Second(n),
                    };
                }
                set = true;
            }
            None if matches!(d, DateTok::General(_)) => {}
            None => set = true,
        }
    }
}

fn compile_section(raw: Vec<Tok>) -> Option<Section> {
    let (mut color, mut cond, mut lcid, mut tag) = (None, None, None, None);
    let mut calendar: Option<u8> = None;
    let mut dbnum: Option<u8> = None;
    let mut seen_locale = false;
    let mut compiled = false;

    let implicit_general = !raw.is_empty()
        && raw
            .iter()
            .all(|t| matches!(t, Tok::Color(_) | Tok::Cond(..) | Tok::DbNum(_)));
    let mut toks = Vec::with_capacity(raw.len());
    for t in raw {
        match t {
            Tok::Compiled => compiled = true,
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
            Tok::DbNum(n) => {
                if dbnum.replace(n).is_some() {
                    return None;
                }
            }

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

    let hijri = calendar == Some(2) && lcid.is_none_or(|l| (l >> 16) & 0xFF == 0);

    let digit_sys = lcid.map(|l| (l >> 24) as u8);
    let db = match (dbnum, lcid) {
        (Some(n), Some(l)) if l >> 24 == 0 => Db::new(n, l & 0xFFFF),
        _ => None,
    };
    let sys = match (lcid.map(|l| l & 0xFFFF), tag.as_deref()) {
        (Some(0xF800), _) | (_, Some("x-sysdate")) => Some(("dddd, d mmmm yyyy", None)),
        (Some(0xF400), _) | (_, Some("x-systime")) => Some(("h:mm:ss AM/PM", Some(0x0409))),
        _ => None,
    };
    if let Some((code, names)) = sys {
        if let Some(s) = toks.iter().position(|t| matches!(t, Tok::Slash)) {
            if !toks[..s]
                .iter()
                .any(|t| is_date_tok(t) || matches!(t, Tok::Ph(_)))
            {
                return None;
            }
        }
        toks = tokenize(code)?.pop()?;
        lcid = names;
    }

    let ampm_subsec = toks
        .iter()
        .rposition(|t| matches!(t, Tok::Point))
        .is_some_and(|p| {
            let tail = &toks[p + 1..];
            (1..=3).contains(&tail.len())
                && tail.iter().all(|t| matches!(t, Tok::Ph(Ph::Zero)))
                && toks[..p].iter().any(|t| matches!(t, Tok::AmPm))
                && !toks[..p]
                    .iter()
                    .any(|t| matches!(t, Tok::Ph(_) | Tok::Percent))
        });
    if !ampm_subsec
        && !toks
            .iter()
            .any(|t| matches!(t, Tok::Date(..) | Tok::Elapsed(..) | Tok::AmPmCjk))
    {
        if let Some(first) = toks.iter().position(|t| matches!(t, Tok::AmPm)) {
            for t in &mut toks[first..] {
                match t {
                    Tok::Point => *t = Tok::Lit(".".into()),
                    Tok::Percent => *t = Tok::Lit("%".into()),
                    _ => {}
                }
            }
        }
    }
    let has = |f: &dyn Fn(&Tok) -> bool| toks.iter().any(f);
    let has_at = has(&|t| matches!(t, Tok::At));

    let ampm_number = !ampm_subsec
        && has(&|t| matches!(t, Tok::AmPm))
        && !has(&|t| matches!(t, Tok::Date(..) | Tok::Elapsed(..) | Tok::AmPmCjk))
        && has(&|t| matches!(t, Tok::Percent | Tok::Point | Tok::Ph(_)))
        && !has(&|t| matches!(t, Tok::At | Tok::General | Tok::Slash | Tok::Exp(_)));
    if ampm_number {
        let first = toks.iter().position(|t| matches!(t, Tok::AmPm))?;
        if toks[first..].iter().any(|t| matches!(t, Tok::Ph(_))) {
            return None;
        }
    }

    let cal_subsec = calendar.is_some()
        && toks.first().is_some_and(|t| matches!(t, Tok::Point))
        && (2..=4).contains(&toks.len())
        && toks[1..].iter().all(|t| matches!(t, Tok::Ph(Ph::Zero)));
    let has_date = (has(&is_date_tok) || cal_subsec) && !ampm_number;
    let has_general = has(&|t| matches!(t, Tok::General));
    let has_digits = has(&|t| matches!(t, Tok::Ph(_)));
    let has_point = has(&|t| matches!(t, Tok::Point));

    let mut has_date = has_date;
    if !has_date && !has_at && !has_general {
        if let Some(c) = toks.iter().position(|t| matches!(t, Tok::Colon)) {
            if toks[c..].iter().any(|t| matches!(t, Tok::LitDigit(_))) {
                return None;
            }
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

                Tok::Point | Tok::Percent => return None,
                Tok::Comma if i > 0 && matches!(toks[i - 1], Tok::Comma) => continue,
                Tok::Comma => Item::Lit(",".into()),
                _ => literal_item(t)?,
            });
        }
        Body::Text(items)
    } else if has_date {
        if calendar == Some(1) {
            for t in &mut toks {
                if let Tok::Date(DateField::Month, n) = t {
                    if *n == 5 {
                        *n = 4;
                    }
                }
            }
        }
        compile_date(&toks, lcid, hijri, calendar.is_some(), compiled)?
    } else if has_general {
        if toks.iter().filter(|t| matches!(t, Tok::General)).count() > 1 {
            return None;
        }
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
        db,
    })
}

fn inherit_first_locale(raw: &mut [Vec<Tok>]) {
    let first = raw.first().and_then(|s| {
        s.iter().find_map(|t| match t {
            Tok::Locale { lcid, .. } => Some(*lcid),
            _ => None,
        })
    });
    for sec in raw.iter_mut().skip(1) {
        let own = sec.iter_mut().find_map(|t| match t {
            Tok::Locale { lcid, .. } => Some(lcid),
            _ => None,
        });
        match (own, first) {
            (Some(l), Some(f)) => *l = f,
            (Some(l), None) => *l = None,
            (None, Some(Some(f))) if !matches!(f & 0xFFFF, 0xF400 | 0xF800) => {
                sec.insert(
                    0,
                    Tok::Locale {
                        currency: String::new(),
                        lcid: Some(f & 0xFFFF),
                        tag: None,
                    },
                );
            }
            _ => {}
        }
    }
}

pub(crate) fn parse_format(code: &str) -> Option<NumberFormat> {
    let plain = parse_code(code, code)?;

    let lower = code.to_ascii_lowercase();
    if lower.contains("am/pm") && lower.contains('[') {
        let compiled = super::readback::compiled_spelling(code);
        if compiled != code {
            if let Some(f) = parse_code(&compiled, code) {
                return Some(f);
            }
        }
    }
    Some(plain)
}

fn parse_code(code: &str, typed: &str) -> Option<NumberFormat> {
    if typed.encode_utf16().count() > 255 {
        return None;
    }
    let mut raw = tokenize(code)?;
    inherit_first_locale(&mut raw);
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

    let lens = super::codelen::section_code_lens(typed);
    if lens.iter().all(Option::is_some) {
        let mut total: u32 = lens.iter().flatten().sum();
        if n == 1 {
            total += match sections[0].body {
                Body::Literal(_) if typed.split('"').step_by(2).any(|s| !s.is_empty()) => 3,
                Body::Literal(_) => 11,
                Body::General(_) | Body::Text(_) | Body::Date(_) => 4,
                _ => 0,
            };
        }
        if total >= 253 {
            return None;
        }
    }
    Some(NumberFormat {
        sections,
        text_section,
        numeric,
        code_lens: lens,
    })
}
