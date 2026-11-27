use super::parse::{parse_bracket, Tok as Bracketed};
use super::readback::{sections_of, spelled_stored, Tok};
use super::Cmp;

#[derive(Clone, Debug, PartialEq, Eq, Hash)]
enum Op {
    Code(char),
    Literal(char),
    Quoted(String),

    Space(char),

    Fill(char),
    General,

    AmPm(String),
    Condition(u8, u64),

    Elapsed(u8, u8),

    Locale(String, String),

    Bracket(String),
}

#[derive(Clone, Debug, PartialEq, Eq, Hash)]
pub(crate) struct Compiled {
    single: bool,

    text: bool,

    text_first: bool,
    sections: [Vec<Op>; 4],
}

impl Compiled {
    pub(crate) fn any_ampm_case(mut self) -> Compiled {
        for op in self.sections.iter_mut().flatten() {
            if let Op::AmPm(s) = op {
                s.make_ascii_lowercase();
            }
        }
        self
    }
}

fn bracket(b: &str) -> Op {
    let inner = b
        .strip_prefix('[')
        .and_then(|s| s.strip_suffix(']'))
        .unwrap_or(b);
    if let Some(rest) = inner.strip_prefix('$') {
        let (currency, id) = rest.split_once('-').unwrap_or((rest, ""));
        let id = id.trim_start_matches('0').to_ascii_uppercase();
        return Op::Locale(currency.to_string(), id);
    }
    match parse_bracket(inner) {
        Some(Bracketed::Cond(op, x)) => {
            let op = match op {
                Cmp::Lt => 0,
                Cmp::Le => 1,
                Cmp::Gt => 2,
                Cmp::Ge => 3,
                Cmp::Eq => 4,
                Cmp::Ne => 5,
            };
            Op::Condition(op, x.to_bits())
        }
        Some(Bracketed::Elapsed(unit, n)) => Op::Elapsed(unit as u8, n),
        _ => Op::Bracket(inner.to_ascii_lowercase()),
    }
}

fn stream(toks: &[Tok]) -> Vec<Op> {
    let ch = |i: usize| match toks.get(i) {
        Some(Tok::Ch(c)) => Some(*c),
        _ => None,
    };
    let word = |i: usize, w: &str| {
        w.chars()
            .enumerate()
            .all(|(k, c)| ch(i + k).is_some_and(|d| d.eq_ignore_ascii_case(&c)))
    };
    let dated = toks.iter().enumerate().any(|(i, t)| match t {
        Tok::Ch(c) => "ymdhsYMDHS".contains(*c) && !word(i, "general"),
        Tok::Bracket(b) => matches!(bracket(b), Op::Elapsed(..)),
        _ => false,
    });
    let mut out = Vec::new();
    let mut i = 0;
    while i < toks.len() {
        match &toks[i] {
            Tok::Bracket(b) => out.push(bracket(b)),
            Tok::Raw(s) => {
                let mut cs = s.chars();
                match cs.next() {
                    Some('_') => out.extend(cs.next().map(Op::Space)),
                    Some('*') => out.extend(cs.next().map(Op::Fill)),
                    _ => out.push(Op::Quoted(s.trim_matches('"').to_string())),
                }
            }
            Tok::Esc(c) => out.push(Op::Literal(*c)),
            Tok::Ch(_) if word(i, "general") => {
                out.push(Op::General);
                i += 7;
                continue;
            }
            Tok::Ch(_) if word(i, "am/pm") || word(i, "a/p") => {
                let n = if word(i, "a/p") { 3 } else { 5 };
                out.push(Op::AmPm((i..i + n).filter_map(&ch).collect()));
                i += n;
                continue;
            }
            Tok::Ch(c) => {
                let c = *c;
                let l = c.to_ascii_lowercase();

                let aaa = l == 'a'
                    && (word(i, "aaa")
                        || (i >= 1 && word(i - 1, "aaa"))
                        || (i >= 2 && word(i - 2, "aaa")));
                if "ymdhsegb".contains(l) || aaa {
                    out.push(Op::Code(l));
                } else if "0#?.,%/@$+".contains(c) && (c != '+' || i > 0 && word(i - 1, "e"))
                    || (c == ':' && dated)
                {
                    out.push(Op::Code(c));
                } else {
                    out.push(Op::Literal(c));
                }
            }
        }
        i += 1;
    }
    out
}

pub(crate) fn compile_for_compare(code: &str) -> Compiled {
    let spelled = spelled_stored(code);
    let given = sections_of(&spelled);

    let written = sections_of(code);
    let n = given.len().min(4);
    let general = || vec![Op::General];
    let mut sections: [Vec<Op>; 4] = [general(), general(), general(), general()];
    let (mut single, mut text, mut text_first) = (true, false, false);
    let mut numbers = 0;
    let mut last_conditional = false;
    for (k, toks) in given.iter().take(n).enumerate() {
        let mut ops = stream(toks);
        if let Some(raw) = written.get(k) {
            let mut ids = raw.iter().filter_map(|t| match t {
                Tok::Bracket(b) => Some(bracket(b)).filter(|o| matches!(o, Op::Locale(..))),
                _ => None,
            });
            for op in ops.iter_mut().filter(|o| matches!(o, Op::Locale(..))) {
                if let Some(id) = ids.next() {
                    *op = id;
                }
            }
        }
        if k == 3 || ops.contains(&Op::Code('@')) {
            text = ops != general();
            text_first = text && numbers == 0;
            sections[3] = ops;
            break;
        }
        last_conditional = ops.iter().any(|o| matches!(o, Op::Condition(..)));
        match k {
            0 => sections[..3].fill(ops),
            _ => {
                sections[k] = ops;
                single = false;
            }
        }
        numbers += 1;
    }
    if !text && last_conditional && numbers != 2 && numbers < 3 {
        sections[numbers] = general();
        single = false;
    }
    Compiled {
        single,
        text,
        text_first,
        sections,
    }
}

#[cfg(test)]
mod tests {
    use super::compile_for_compare as c;

    #[test]
    fn equal_formats() {
        for (a, b) in [
            ("yyyy", "YYYY"),
            ("0;-0;0", "0;-0"),
            ("0;-0;0;@", "0;-0;@"),
            ("0;0", "0;0;0"),
            ("0;-0;0;General", "0;-0"),
            (r"0\x", "0x"),
            (r"d\-mmm", "D-MMM"),
            ("h:mm AM/PM", "H:MM AM/PM"),
            ("general", "General"),
            ("[RED]0", "[Red]0"),
            ("[>1000]0", "[>1E3]0"),
            ("[>0]0", "[>0]0;General"),
            ("\"$\"#,##0", "$#,##0"),
            ("0.00E+00", "0.00E+00"),
        ] {
            assert_eq!(
                c(&crate::cellfmt::format_code_as_read(a)),
                c(&crate::cellfmt::format_code_as_read(b)),
                "{a} {b}"
            );
        }
    }

    #[test]
    fn different_formats() {
        for (a, b) in [
            (r#"0.0"x""#, r"0.0\x"),
            ("0x", "0X"),
            ("0_a", "0_A"),
            ("0*a", "0*A"),
            ("0", "0;0"),
            ("0;@", "0;0;@"),
            ("@", "General;General;General;@"),
            (r"$0", r"\$0"),
            (r"d/m", r"d\/m"),
            (r"h:mm", r"h\:mm"),
            ("[Red]0", "[Color3]0"),
            ("[$kr-41D]0", "[$KR-41D]0"),
            ("[<1]0", "[<=1]0"),
            ("0", "0.0"),
            ("h", "hh"),
            ("[hhh]", "[hhhh]"),
            ("[$-FC00]0", "[$-FC09]0"),
        ] {
            assert_ne!(c(a), c(b), "{a} {b}");
        }
    }
}
