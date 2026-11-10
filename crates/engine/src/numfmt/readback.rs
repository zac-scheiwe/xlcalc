enum Tok {
    Bracket(String),

    Raw(String),

    Esc(char),
    Ch(char),
}

fn tokens(fmt: &str) -> Vec<Tok> {
    let cs: Vec<char> = fmt.chars().collect();
    let mut out = Vec::new();
    let mut i = 0;
    while i < cs.len() {
        let c = cs[i];
        match c {
            '"' | '[' => {
                let close = if c == '"' { '"' } else { ']' };
                let Some(end) = cs[i + 1..].iter().position(|&d| d == close) else {
                    out.push(Tok::Raw(cs[i..].iter().collect()));
                    break;
                };
                let s: String = cs[i..=i + 1 + end].iter().collect();
                out.push(if c == '[' {
                    Tok::Bracket(s)
                } else {
                    Tok::Raw(s)
                });
                i += end + 2;
            }
            '\\' if i + 1 < cs.len() => {
                out.push(Tok::Esc(cs[i + 1]));
                i += 2;
            }
            '_' | '*' if i + 1 < cs.len() => {
                out.push(Tok::Raw(cs[i..i + 2].iter().collect()));
                i += 2;
            }
            _ => {
                out.push(Tok::Ch(c));
                i += 1;
            }
        }
    }
    out
}

fn sections_of(fmt: &str) -> Vec<Vec<Tok>> {
    let mut sections: Vec<Vec<Tok>> = vec![Vec::new()];
    for t in tokens(fmt) {
        if matches!(t, Tok::Ch(';')) {
            sections.push(Vec::new());
        } else {
            sections.last_mut().expect("one section").push(t);
        }
    }
    sections
}

fn push_tok(t: &Tok, out: &mut String) {
    match t {
        Tok::Bracket(s) | Tok::Raw(s) => out.push_str(s),
        Tok::Esc(c) => {
            out.push('\\');
            out.push(*c);
        }
        Tok::Ch(c) => out.push(*c),
    }
}

fn is_placeholder(t: &Tok) -> bool {
    matches!(t, Tok::Ch('0' | '#' | '?'))
}

fn has_condition(section: &str) -> bool {
    tokens(section)
        .iter()
        .any(|t| matches!(t, Tok::Bracket(b) if b[1..].starts_with(['<', '>', '='])))
}

fn currency_tag(b: &str) -> Option<(&str, Option<String>)> {
    let inner = b.strip_prefix("[$")?.strip_suffix(']')?;
    if let Some((sym, id)) = inner.split_once('-') {
        if id.is_empty() || super::parse::is_x_tag(id) {
            return Some((inner, None));
        }
        let Some(v) = super::parse::excel_hex(id) else {
            return Some((inner, None));
        };

        let (hi, lo) = (v & !0xFFFF, v & 0xFFFF);
        return Some(match plain_lcid(lo) {
            Some(m) if lo != 0 => (sym, Some(format!("{:X}", hi | m))),
            _ if hi != 0 && lo == 0 => (sym, Some(format!("{hi:X}"))),
            _ => (sym, None),
        });
    }
    Some((inner, None))
}

fn plain_lcid(v: u32) -> Option<u32> {
    match v {
        0x400 | 0xC00 => return Some(0x1409),
        0x800 => return Some(0x409),
        0xAF => return Some(0x36),
        0xBA => return Some(0x6D),
        0xBE => return Some(0x23),
        0xCA => return Some(3),
        0xDA => return Some(6),
        0xDE => return Some(7),
        0xFA => return Some(0x29),
        0xFF => return Some(0x67),
        _ => {}
    }
    let valid = matches!(v, 1..=0x7A | 0x7C | 0x7E..=0x88 | 0x8C | 0x91..=0x92 | 0x401..=0x43B | 0x43D..=0x441 | 0x1409);

    let kept = matches!(v, 0x442..=0x47A | 0x47C | 0x47E | 0x480..=0x488 | 0x48C | 0x491..=0x492 | 0x501 | 0x5FE | 0x801 | 0x803..=0x804 | 0x807 | 0x809..=0x80A | 0x80C | 0x810 | 0x813..=0x814 | 0x816 | 0x818..=0x81A | 0x81D | 0x820 | 0x82C | 0x82E | 0x832 | 0x83B..=0x83C | 0x83E | 0x843 | 0x845..=0x846 | 0x849 | 0x850..=0x851 | 0x859 | 0x85D | 0x85F..=0x861 | 0x867 | 0x86B | 0x873 | 0x9FF | 0xC01 | 0xC04 | 0xC07 | 0xC09..=0xC0A | 0xC0C | 0xC1A | 0xC3B | 0xC50..=0xC51 | 0xC6B | 0xCEB | 0x1001 | 0x1004 | 0x1007 | 0x1009..=0x100A | 0x100C | 0x101A | 0x103B | 0x105F | 0x1401 | 0x1404 | 0x1407 | 0x140A | 0x140C | 0x141A | 0x143B | 0x1801 | 0x1809..=0x180A | 0x180C | 0x181A | 0x183B | 0x1C01 | 0x1C09..=0x1C0A | 0x1C0C | 0x1C1A | 0x1C3B | 0x2001 | 0x2009..=0x200A | 0x200C | 0x201A | 0x203B | 0x2401 | 0x2409..=0x240A | 0x240C | 0x241A | 0x243B | 0x2801 | 0x2809..=0x280A | 0x280C | 0x281A | 0x2C01 | 0x2C09..=0x2C0A | 0x2C0C | 0x2C1A | 0x3001 | 0x3009..=0x300A | 0x300C | 0x301A | 0x3401 | 0x3409..=0x340A | 0x340C | 0x3801 | 0x3809..=0x380A | 0x380C | 0x3C01 | 0x3C09..=0x3C0A | 0x3C0C | 0x4001 | 0x4009..=0x400A | 0x4409..=0x440A | 0x4809..=0x480A | 0x4C09..=0x4C0A | 0x500A | 0x540A | 0x580A | 0x5C0A | 0x641A | 0x681A | 0x6C1A | 0x701A | 0x703B | 0x742C | 0x743B | 0x7804 | 0x7814 | 0x781A | 0x782C | 0x783B | 0x7843 | 0x7850 | 0x785D | 0x785F | 0x7C04 | 0x7C14 | 0x7C1A | 0x7C28 | 0x7C2E | 0x7C3B | 0x7C43 | 0x7C46 | 0x7C50 | 0x7C59 | 0x7C5C..=0x7C5D | 0x7C5F | 0x7C67..=0x7C68 | 0x7C86 | 0x7C92 | 0xAC6E | 0xF400..=0xFFFF);
    match v {
        0xFC00 => Some(0xFC09),
        0xFD00 | 0xFE00 | 0xFFFF => Some(0xFC00),
        _ => (valid || kept).then_some(v),
    }
}
fn is_text_section(toks: &[Tok]) -> bool {
    toks.iter().any(|t| matches!(t, Tok::Ch('@')))
}

fn format_lcid(first: &[Tok]) -> Option<String> {
    if is_text_section(first) {
        return None;
    }
    first.iter().find_map(|t| match t {
        Tok::Bracket(b) => currency_tag(b).and_then(|(_, l)| l),
        _ => None,
    })
}

pub(crate) fn readback_spelling(fmt: &str) -> String {
    spell(fmt, true)
}

pub(crate) fn spelled_for_parse(fmt: &str) -> String {
    spell(fmt, false)
}

fn spell(fmt: &str, for_display: bool) -> String {
    let sections = sections_of(fmt);
    let lcid = format_lcid(&sections[0]);
    let mut texts: Vec<String> = sections
        .into_iter()
        .enumerate()
        .map(|(i, s)| {
            let text = canon_section(s, i == 0, lcid.as_deref(), for_display);
            if for_display {
                super::ampm_elapsed::garble_section(&text).unwrap_or(text)
            } else {
                text
            }
        })
        .collect();

    if texts.len() == 4 && texts[3] == "General" {
        texts.pop();
    }

    let lone_general = texts[0]
        .strip_suffix("General")
        .is_some_and(|c| c.starts_with('[') && c.matches('[').count() == 1 && c.ends_with(']'));
    if texts.len() == 1 && has_condition(&texts[0]) && !lone_general {
        texts.push("General".into());
    }
    if texts[0].is_empty() && texts.len() >= 2 {
        if texts.len() <= 3 && texts.last().is_some_and(|l| l.contains('@')) {
            let text = texts.pop().expect("sections");
            while texts.len() < 3 {
                texts.push(String::new());
            }
            texts.push(text);
        } else if texts.len() == 2 {
            texts.push(String::new());
        }
    }
    texts.join(";")
}

pub(crate) fn format_key(spelled: &str) -> String {
    let mut parts: Vec<&str> = spelled.split(';').collect();
    if (parts.len() == 3 || (parts.len() == 4 && parts[3].contains('@')))
        && parts[2] == parts[0]
        && !parts[0].is_empty()
    {
        parts.remove(2);
    }
    parts.join(";")
}

fn tag_rank(b: &str) -> Option<u8> {
    let inner = &b[1..b.len() - 1];
    if colour_name(inner).is_some() {
        Some(0)
    } else if inner.starts_with(['<', '>', '=']) {
        Some(1)
    } else if inner.starts_with('$') {
        Some(2)
    } else {
        None
    }
}

fn colour_name(inner: &str) -> Option<String> {
    let lower = inner.to_ascii_lowercase();
    for n in [
        "red", "blue", "green", "black", "white", "cyan", "magenta", "yellow",
    ] {
        if lower.starts_with(n) {
            return Some(format!("[{}{}]", n[..1].to_ascii_uppercase(), &n[1..]));
        }
    }
    let r = lower.strip_prefix("color")?.trim_start_matches([' ', '\t']);
    let digits: String = r.chars().take_while(char::is_ascii_digit).collect();
    let n: u32 = digits.trim_start_matches('0').parse().unwrap_or(0);
    (1..=56).contains(&n).then(|| format!("[Color{n}]"))
}

fn cond_number(text: &str) -> Option<String> {
    let (neg, t) = match text.strip_prefix('-') {
        Some(r) => (true, r),
        None => (false, text.strip_prefix('+').unwrap_or(text)),
    };
    let (mant, exp) = match t.split_once(['e', 'E']) {
        Some((m, e)) => (m, e.parse::<i32>().ok()?),
        None => (t, 0),
    };
    let (int, frac) = mant.split_once('.').unwrap_or((mant, ""));
    if !int.chars().chain(frac.chars()).all(|c| c.is_ascii_digit())
        || mant == "."
        || mant.is_empty()
    {
        return None;
    }
    let all: String = format!("{int}{frac}");
    let lead = all.len() - all.trim_start_matches('0').len();
    let digits = all.trim_start_matches('0');
    if digits.is_empty() {
        return Some("0".into());
    }

    let point = int.len() as i32 - lead as i32 + exp;
    let digits = digits[..digits.len().min(15)].trim_end_matches('0');
    let x = point - 1;
    let sign = if neg { "-" } else { "" };
    let body = if !(-15..=20).contains(&x) {
        let rest = &digits[1..];
        let dot = if rest.is_empty() { "" } else { "." };
        format!(
            "{}{dot}{rest}E{}{}",
            &digits[..1],
            if x < 0 { '-' } else { '+' },
            x.abs()
        )
    } else if x >= 0 {
        let int_len = x as usize + 1;
        if digits.len() <= int_len {
            format!("{digits}{}", "0".repeat(int_len - digits.len()))
        } else {
            format!("{}.{}", &digits[..int_len], &digits[int_len..])
        }
    } else {
        format!("0.{}{digits}", "0".repeat((-x - 1) as usize))
    };
    Some(format!("{sign}{body}"))
}

fn canon_condition(b: &str) -> String {
    let inner = &b[1..b.len() - 1];
    let n = inner.chars().take_while(|c| "<>=".contains(*c)).count();
    let (op, num) = inner.split_at(n);

    match cond_number(if num.is_empty() { "0" } else { num }) {
        Some(s) => format!("[{op}{s}]"),
        None => b.to_string(),
    }
}

fn canon_bracket(b: &str, first: bool, text: bool, lcid: Option<&str>) -> String {
    let inner = &b[1..b.len() - 1];
    if let Some(c) = colour_name(inner) {
        return c;
    }
    if inner.starts_with(['<', '>', '=']) {
        return canon_condition(b);
    }
    if let Some((sym, own)) = currency_tag(b) {
        let l = if first && !text {
            own
        } else {
            lcid.map(String::from)
        };
        return match l {
            Some(l) => format!("[${sym}-{l}]"),
            None => format!("[${sym}]"),
        };
    }
    if !inner.is_empty() && inner.chars().all(|c| "hmsHMS".contains(c)) {
        return b.to_ascii_lowercase();
    }
    b.to_string()
}

fn matches_word(toks: &[Tok], i: usize, word: &str) -> bool {
    word.chars()
        .enumerate()
        .all(|(k, w)| matches!(toks.get(i + k), Some(Tok::Ch(c)) if c.to_ascii_lowercase() == w))
}

fn regroup_thousands(toks: Vec<Tok>) -> Vec<Tok> {
    let end = toks
        .iter()
        .position(|t| matches!(t, Tok::Ch('.' | 'E' | 'e')))
        .unwrap_or(toks.len());
    let point = matches!(toks.get(end), Some(Tok::Ch('.')));
    let run_char = |t: &Tok| matches!(t, Tok::Ch('0' | '#' | '?' | ','));
    let mut out: Vec<Tok> = Vec::with_capacity(toks.len());
    let mut i = 0;
    while i < toks.len() {
        if i < end && run_char(&toks[i]) {
            let mut j = i;
            while j < end && run_char(&toks[j]) {
                j += 1;
            }
            let run = &toks[i..j];
            let first = run.iter().position(is_placeholder);
            let last = run.iter().rposition(is_placeholder);
            let (f, l) = match (first, last) {
                (Some(f), Some(l)) if run[f..=l].iter().any(|t| matches!(t, Tok::Ch(','))) => {
                    (f, l)
                }
                _ => {
                    out.extend(run.iter().map(copy_tok));
                    i = j;
                    continue;
                }
            };
            let mut ph: Vec<char> = run[f..=l]
                .iter()
                .filter_map(|t| match t {
                    Tok::Ch(c) if *c != ',' => Some(*c),
                    _ => None,
                })
                .collect();
            while ph.len() < 4 {
                ph.insert(0, '#');
            }
            out.extend(run[..f].iter().map(copy_tok));
            for (k, c) in ph.iter().enumerate() {
                out.push(Tok::Ch(*c));
                if k + 1 < ph.len() && (ph.len() - k - 1).is_multiple_of(3) {
                    out.push(Tok::Ch(','));
                }
            }
            out.extend(run[l + 1..].iter().map(copy_tok));
            i = j;
            continue;
        }
        let drop_comma = point
            && i > end
            && matches!(toks[i], Tok::Ch(','))
            && matches!(toks.get(i - 1), Some(t) if is_placeholder(t))
            && matches!(toks.get(i + 1), Some(t) if is_placeholder(t));
        if !drop_comma {
            out.push(copy_tok(&toks[i]));
        }
        i += 1;
    }
    out
}

fn copy_tok(t: &Tok) -> Tok {
    match t {
        Tok::Bracket(s) => Tok::Bracket(s.clone()),
        Tok::Raw(s) => Tok::Raw(s.clone()),
        Tok::Esc(c) => Tok::Esc(*c),
        Tok::Ch(c) => Tok::Ch(*c),
    }
}

fn elapsed_unit(t: &Tok) -> Option<(char, usize)> {
    let Tok::Bracket(b) = t else { return None };
    let inner = b[1..b.len() - 1].to_ascii_lowercase();
    let c = inner.chars().next()?;
    (matches!(c, 'h' | 'm' | 's') && inner.chars().all(|d| d == c)).then_some((c, inner.len()))
}

fn unit_rank(c: char) -> u8 {
    match c {
        'h' => 3,
        'm' => 2,
        _ => 1,
    }
}

fn elapsed_fields(toks: Vec<Tok>) -> Vec<Tok> {
    let mut res: Vec<Tok> = Vec::with_capacity(toks.len());

    let mut prev: Option<(usize, char, usize)> = None;
    let mut i = 0;
    while i < toks.len() {
        let field = if let Some((c, n)) = elapsed_unit(&toks[i]) {
            Some((true, c, n, 1))
        } else if let Tok::Ch(c @ ('h' | 'm' | 's')) = toks[i] {
            let mut j = i;
            while matches!(toks.get(j), Some(Tok::Ch(d)) if *d == c) {
                j += 1;
            }
            (c != 'm' || j - i <= 2).then_some((false, c, j - i, j - i))
        } else {
            None
        };

        if field.is_none() && matches!(toks[i], Tok::Ch('m')) {
            let mut j = i;
            while matches!(toks.get(j), Some(Tok::Ch('m'))) {
                res.push(copy_tok(&toks[j]));
                j += 1;
            }
            i = j;
            continue;
        }
        let Some((elapsed, c, n, used)) = field else {
            if !matches!(toks[i], Tok::Ch(_) | Tok::Esc(_) | Tok::Raw(_)) {
                prev = None;
            }
            res.push(copy_tok(&toks[i]));
            i += 1;
            continue;
        };
        i += used;
        match prev {
            Some((idx, pc, pn)) if unit_rank(c) > unit_rank(pc) => {
                res.splice(idx..=idx, std::iter::repeat_with(|| Tok::Ch(pc)).take(pn));
                let inner: String = if c == 'm' {
                    String::new()
                } else {
                    c.to_string().repeat(n)
                };
                res.push(Tok::Bracket(format!("[{inner}]")));
                prev = Some((res.len() - 1, c, n));
            }
            Some((_, pc, _)) if unit_rank(c) == unit_rank(pc) && c != 'm' => {
                res.push(Tok::Bracket(format!("[{}]", c.to_string().repeat(n))));
                prev = Some((res.len() - 1, c, n));
            }
            _ => {
                if elapsed {
                    res.push(copy_tok(&toks[i - used]));
                    prev = Some((res.len() - 1, c, n));
                } else {
                    res.extend(std::iter::repeat_with(|| Tok::Ch(c)).take(n));
                    prev = None;
                }
            }
        }
    }
    res
}

fn canon_section(mut toks: Vec<Tok>, first: bool, lcid: Option<&str>, for_display: bool) -> String {
    let text = is_text_section(&toks);

    if let Some(p) = toks
        .iter()
        .position(|t| matches!(t, Tok::Bracket(b) if tag_rank(b) == Some(0)))
    {
        let t = toks.remove(p);
        toks.insert(0, t);
    }

    let n = toks
        .iter()
        .take_while(|t| matches!(t, Tok::Bracket(b) if tag_rank(b).is_some()))
        .count();
    let mut lead: Vec<Tok> = toks.drain(..n).collect();
    lead.sort_by_key(|t| match t {
        Tok::Bracket(b) => tag_rank(b).unwrap_or(3),
        _ => 3,
    });
    let only_colour = toks.is_empty()
        && !lead.is_empty()
        && lead
            .iter()
            .all(|t| matches!(t, Tok::Bracket(b) if tag_rank(b) == Some(0)));
    let has_time = toks
        .iter()
        .any(|t| matches!(t, Tok::Ch('h' | 'H' | 's' | 'S')));
    let placeholders = toks.iter().any(is_placeholder);
    let mut out: Vec<Tok> = Vec::new();
    let mut i = 0;
    while i < toks.len() {
        match &toks[i] {
            Tok::Ch('y' | 'Y') => {
                let mut j = i;
                while matches!(toks.get(j), Some(Tok::Ch('y' | 'Y'))) {
                    j += 1;
                }
                let k = match j - i {
                    1 => 2,
                    3.. => 4,
                    k => k,
                };
                out.extend(std::iter::repeat_with(|| Tok::Ch('y')).take(k));
                i = j;
            }

            Tok::Ch('m' | 'M') => {
                let mut j = i;
                while matches!(toks.get(j), Some(Tok::Ch('m' | 'M'))) {
                    j += 1;
                }
                let k = if j - i >= 6 { 4 } else { j - i };
                out.extend(std::iter::repeat_with(|| Tok::Ch('m')).take(k));
                i = j;
            }
            Tok::Ch(_) if matches_word(&toks, i, "am/pm") => {
                out.extend("am/pm".chars().map(Tok::Ch));
                i += 5;
            }
            Tok::Ch(_) if matches_word(&toks, i, "general") => {
                out.extend("General".chars().map(Tok::Ch));
                i += 7;
            }
            Tok::Ch(a @ ('a' | 'A'))
                if has_time
                    && matches!(toks.get(i + 1), Some(Tok::Ch('/')))
                    && matches!(toks.get(i + 2), Some(Tok::Ch('p' | 'P'))) =>
            {
                out.push(Tok::Esc(*a));
                i += 1;
            }

            Tok::Ch(a @ ('a' | 'A')) => {
                let mut j = i;
                while matches!(toks.get(j), Some(Tok::Ch(b)) if b.eq_ignore_ascii_case(a)) {
                    j += 1;
                }
                if j - i >= 3 {
                    out.extend(std::iter::repeat_with(|| Tok::Ch('a')).take(j - i));
                    i = j;
                } else {
                    out.push(Tok::Esc(*a));
                    i += 1;
                }
            }
            Tok::Ch(']') => {
                out.push(Tok::Esc(']'));
                i += 1;
            }
            Tok::Ch(c @ ('D' | 'H' | 'S')) => {
                out.push(Tok::Ch(c.to_ascii_lowercase()));
                i += 1;
            }
            Tok::Ch('%') if for_display && !placeholders => {
                out.push(Tok::Esc('%'));
                i += 1;
            }
            Tok::Esc(c)
                if *c == ' '
                    || *c == '-'
                    || (*c == '.' && !placeholders && (for_display || !is_text_section(&toks)))
                    || (*c == ',' && !toks[..i].iter().any(is_placeholder))
                    || "()+!^&'~{}<>=`|€£¥123456789".contains(*c)
                    || (c.is_ascii_alphabetic() && !"abdeghmnsyABDEGHMNSYcC".contains(*c)) =>
            {
                out.push(Tok::Ch(*c));
                i += 1;
            }
            t => {
                out.push(copy_tok(t));
                i += 1;
            }
        }
    }
    let mut all = lead;
    all.extend(regroup_thousands(if for_display {
        elapsed_fields(out)
    } else {
        out
    }));
    let mut s = String::new();
    for t in &mut all {
        if let Tok::Bracket(b) = t {
            *b = canon_bracket(b, first, text, lcid);
        }
        push_tok(t, &mut s);
    }
    if only_colour {
        s.push_str("General");
    }
    s
}

pub(crate) fn digits_with_date(fmt: &str) -> bool {
    sections_of(fmt).iter().any(|toks| {
        let mut date = false;
        let mut digits = false;
        let mut i = 0;
        while i < toks.len() {
            match &toks[i] {
                Tok::Ch(_) if matches_word(toks, i, "am/pm") => {
                    i += 5;
                    continue;
                }
                Tok::Ch(_) if matches_word(toks, i, "general") => {
                    i += 7;
                    continue;
                }
                Tok::Ch('d' | 'D' | 'm' | 'M' | 'y' | 'Y' | 'h' | 'H' | 's' | 'S') => {
                    date = true;
                }

                Tok::Ch('b' | 'B' | 'g' | 'G') => date = true,
                Tok::Ch('e' | 'E') if !matches!(toks.get(i + 1), Some(Tok::Ch('+' | '-'))) => {
                    date = true;
                }
                Tok::Bracket(b)
                    if b.len() > 2 && b[1..b.len() - 1].chars().all(|c| "hmsHMS".contains(c)) =>
                {
                    date = true;
                }
                Tok::Ch('.') if date => {
                    while matches!(toks.get(i + 1), Some(Tok::Ch('0'))) {
                        i += 1;
                    }
                }
                t if is_placeholder(t) => digits = true,
                _ => {}
            }
            i += 1;
        }
        date && digits
    })
}

pub(crate) fn quote_ampm(fmt: &str) -> String {
    let toks = tokens(fmt);
    let mut s = String::new();
    let mut i = 0;
    while i < toks.len() {
        if matches_word(&toks, i, "am/pm") {
            s.push_str("\"am/pm\"");
            i += 5;
        } else {
            push_tok(&toks[i], &mut s);
            i += 1;
        }
    }
    s
}

pub(crate) fn typed_elapsed_ok(code: &str) -> bool {
    sections_of(code).iter().all(|toks| {
        let fields: Vec<(usize, char)> = toks
            .iter()
            .enumerate()
            .filter_map(|(i, t)| elapsed_unit(t).map(|(c, _)| (i, c)))
            .collect();
        fields
            .windows(2)
            .all(|w| w[0].1 != w[1].1 || w[1].0 == w[0].0 + 1)
    })
}
#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn readback_pairs() {
        for (given, back) in [
            ("[Color 3]0", "[Color3]0"),
            ("[Red][>1E3]0", "[Red][>1000]0;General"),
            ("[>100][Yellow]0", "[Yellow][>100]0;General"),
            ("[<100]0", "[<100]0;General"),
            ("[Red]", "[Red]General"),
            ("mm-DD-YYYY", "mm-dd-yyyy"),
            ("yyy", "yyyy"),
            ("y", "yy"),
            ("0.0%", "0.0%"),
            ("%", r"\%"),
            (r"\,0", ",0"),
            ("#,#", "#,###"),
            ("0,0", "#,#00"),
            ("[s]h", "s[h]"),
            ("[ss]s", "[ss][s]"),
            ("0;-0;0;General", "0;-0;0"),
            (";@", ";;;@"),
        ] {
            assert_eq!(readback_spelling(given), back, "{given}");
        }
    }

    #[test]
    fn equivalent_keys() {
        assert_eq!(format_key("0;-0;0"), format_key("0;-0"));
        assert_ne!(format_key("0;-0;0.0"), format_key("0;-0"));
    }
}
