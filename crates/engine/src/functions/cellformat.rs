use crate::numfmt::{NumberFormat, NumberHeader};

pub(super) struct CellFmtInfo {
    pub(super) code: String,
    pub(super) color: bool,
    pub(super) parens: bool,
}

const ELAPSED_MARK: char = '\u{4}';

fn strip_section(sec: &str) -> String {
    let mut clean = String::new();
    let mut chars = sec.chars();
    while let Some(c) = chars.next() {
        match c {
            '"' => {
                let mut q = String::new();
                for d in chars.by_ref() {
                    if d == '"' {
                        break;
                    }
                    q.push(d);
                }

                if q == "$" {
                    clean.push('$');
                }
            }
            '\\' | '_' | '*' => {
                chars.next();
            }
            '[' => {
                let mut b = String::new();
                for d in chars.by_ref() {
                    if d == ']' {
                        break;
                    }
                    b.push(d);
                }
                let l = b.to_ascii_lowercase();
                if !l.is_empty() && l.chars().all(|c| matches!(c, 'h' | 'm' | 's')) {
                    clean.push(ELAPSED_MARK);
                }
            }
            c => clean.push(c),
        }
    }
    clean.to_ascii_lowercase()
}

fn date_code(fmt: &str) -> &'static str {
    let classes = NumberFormat::parse_stored(&plain_system_tags(fmt))
        .ok()
        .and_then(|f| f.date_op_classes());
    let row = classes.and_then(|seq| DATE_ROWS.iter().find(|(_, row)| seq.starts_with(row)));
    row.map_or("G", |(code, _)| code)
}

const DATE_ROWS: [(&str, &[u8]); 9] = [
    ("D1", &[13, 17, 27]),
    ("D2", &[13, 17]),
    ("D3", &[17, 27]),
    ("D4", &[17, 13, 27]),
    ("D5", &[17, 13]),
    ("D6", &[21, 23, 25, 34]),
    ("D7", &[21, 23, 34]),
    ("D8", &[21, 23, 25]),
    ("D9", &[21, 23]),
];

fn has_color(s: &str) -> bool {
    let l = s.to_ascii_lowercase();
    [
        "[red]",
        "[blue]",
        "[green]",
        "[black]",
        "[white]",
        "[yellow]",
        "[cyan]",
        "[magenta]",
        "[color",
    ]
    .iter()
    .any(|c| l.contains(c))
}

fn has_paren(s: &str) -> bool {
    let mut it = s.chars();
    while let Some(c) = it.next() {
        match c {
            '"' => {
                for d in it.by_ref() {
                    if d == '"' {
                        break;
                    }
                }
            }
            '[' => {
                for d in it.by_ref() {
                    if d == ']' {
                        break;
                    }
                }
            }
            '_' | '*' => {
                it.next();
            }
            '\\' => {
                if it.next() == Some('(') {
                    return true;
                }
            }
            '(' => return true,
            _ => {}
        }
    }
    false
}

fn is_text_only(sec: &str) -> bool {
    let l = strip_section(sec);
    l.contains('@') && !l.contains(['0', '#', '?', 'd', 'm', 'y', 'h', 's', ELAPSED_MARK])
}

fn parse_flags(secs: &[&str], implicit_general: bool) -> (bool, bool) {
    let n = if secs.len() == 2 && secs[1] == "General" && implicit_general {
        1
    } else {
        secs.len()
    };
    let (mut color_acc, mut paren_acc) = (false, false);
    let (mut color, mut parens) = (false, false);
    for (s, sec) in secs.iter().take(n.min(4)).enumerate() {
        if s == 3 || is_text_only(sec) {
            break;
        }
        color_acc |= has_color(sec);
        paren_acc |= has_paren(sec);
        match s {
            0 => parens = paren_acc,
            1 => color = color_acc,
            _ => {}
        }
    }
    (color, parens)
}

fn is_hidden(secs: &[&str]) -> bool {
    let len = |s: &str| usize::from(!s.is_empty());
    let l0 = len(secs[0]);
    let l1 = secs.get(1).map_or(l0, |s| len(s));
    let l2 = secs.get(2).map_or(l0, |s| len(s));
    let text_short = secs.get(3).is_none_or(|s| s.is_empty() || *s == "@");
    secs.len() > 1 && l0 == 0 && l1 == 0 && l2 == 0 && text_short
}

fn plain_system_tags(fmt: &str) -> String {
    fmt.replace("-F800]", "-409]")
        .replace("-f800]", "-409]")
        .replace("-F400]", "-409]")
        .replace("-f400]", "-409]")
}

fn number_header(fmt: &str) -> Option<NumberHeader> {
    NumberFormat::parse(&plain_system_tags(fmt))
        .ok()?
        .number_header()
}

pub(super) fn cell_format_info(fmt: &str, implicit_general: bool) -> CellFmtInfo {
    let secs = split_sections(fmt);
    let pos = secs[0];
    let (color, parens) = parse_flags(&secs, implicit_general);
    let lower = strip_section(pos);
    let digits = lower.contains(['0', '#', '?']);
    let is_sci = (lower.contains("e+") || lower.contains("e-")) && digits;
    let has_dt = (lower.contains(['d', 'm', 'y', 'h', 's', ELAPSED_MARK])
        || lower.starts_with("b1")
        || lower.starts_with("b2"))
        && !lower.contains("general");
    let mut code = if fmt.contains(';') && is_hidden(&secs) {
        "H".to_string()
    } else if lower.trim().is_empty() || lower.contains("general") || lower.contains('@') {
        "G".to_string()
    } else if has_dt && !is_sci {
        date_code(fmt).to_string()
    } else if is_fraction(&lower) {
        match lower.split_once('/') {
            Some((_, den)) if lower.contains('$') => {
                let n = den
                    .chars()
                    .take_while(|c| matches!(c, '0'..='9' | '#' | '?'))
                    .count();
                format!("C{}", n.min(15))
            }
            _ => "G".to_string(),
        }
    } else if let Some(h) = number_header(fmt) {
        let s1 = h.frac_digits.min(127) as i32;
        let scaled = i32::from((s1 + h.scale) as i8);
        let kind = if lower.contains('$') {
            'C'
        } else if h.scientific {
            'S'
        } else if scaled >= s1 + 2 {
            'P'
        } else if h.grouping {
            ','
        } else {
            'F'
        };
        format!("{kind}{}", h.frac_digits.min(15))
    } else {
        "G".to_string()
    };
    if color {
        code.push('-');
    }
    if parens {
        code.push_str("()");
    }
    CellFmtInfo {
        code,
        color,
        parens,
    }
}

fn split_sections(fmt: &str) -> Vec<&str> {
    let mut out = Vec::new();
    let (mut start, mut quote, mut bracket, mut skip) = (0, false, false, false);
    for (i, c) in fmt.char_indices() {
        if skip {
            skip = false;
            continue;
        }
        match c {
            '"' if !bracket => quote = !quote,
            '[' if !quote => bracket = true,
            ']' if !quote => bracket = false,
            '\\' | '_' | '*' if !quote && !bracket => skip = true,
            ';' if !quote && !bracket => {
                out.push(&fmt[start..i]);
                start = i + 1;
            }
            _ => {}
        }
    }
    out.push(&fmt[start..]);
    out
}

fn is_fraction(lower: &str) -> bool {
    let b = lower.as_bytes();
    (1..b.len().saturating_sub(1)).any(|i| {
        b[i] == b'/'
            && matches!(b[i - 1], b'0' | b'#' | b'?')
            && matches!(b[i + 1], b'0'..=b'9' | b'#' | b'?')
    })
}

pub(super) fn cell_format_code(fmt: &str, implicit_general: bool) -> String {
    cell_format_info(fmt, implicit_general).code
}

#[cfg(test)]
mod tests {
    use super::*;

    fn code(f: &str) -> String {
        cell_format_code(f, false)
    }

    #[test]
    fn color_bit_needs_a_second_section() {
        assert_eq!(code("[Red]0"), "F0");
        assert_eq!(code("[Red]0;0"), "F0-");
        assert_eq!(code("0;[Red]0"), "F0-");

        assert_eq!(code("[Red]0;@"), "F0");
        assert_eq!(code("0;[Red]@"), "F0");

        assert_eq!(code("0;0;[Red]0"), "F0");
        assert_eq!(code("[Red]0;0;@"), "F0-");
    }

    #[test]
    fn implicit_general_is_not_a_section() {
        assert_eq!(cell_format_code("[Red][>1]0;General", true), "F0");
        assert_eq!(cell_format_code("[Red][>1]0;General", false), "F0-");
    }

    #[test]
    fn parentheses_bit_comes_from_section_one() {
        assert_eq!(code("(0)"), "F0()");
        assert_eq!(code("\\(0"), "F0()");
        assert_eq!(code("0;(0)"), "F0");
        assert_eq!(code("\"(\"0"), "F0");
        assert_eq!(code("_(0"), "F0");
        assert_eq!(code("[Red](0);0"), "F0-()");

        assert_eq!(code("(General)"), "G()");
    }

    #[test]
    fn hidden_needs_three_empty_sections() {
        assert_eq!(code(";;;"), "H");
        assert_eq!(code(";0"), "G");
        assert_eq!(code("0;;;"), "F0");
    }

    #[test]
    fn class_letter_follows_the_scaled_header() {
        assert_eq!(code("0%"), "P0");
        assert_eq!(code("0%%"), "P0");
        assert_eq!(code("0,%"), "F0");
        assert_eq!(code("0,,%"), "F0");
        assert_eq!(code("0.0,%"), "F1");
        assert_eq!(code("0%,"), "P0");
        assert_eq!(code("#,##0%"), "P0");

        assert_eq!(code("0;0%"), "F0");
        assert_eq!(code("0;#,##0"), "F0");
        assert_eq!(code("#,##0;0"), ",0");
        assert_eq!(code("0;$0"), "F0");

        assert_eq!(code("$0.0E+00"), "C1");
        assert_eq!(code("\\$0"), "F0");
        assert_eq!(code("[$$-409]0"), "F0");
        assert_eq!(code("[$-F800]0"), "F0");

        assert_eq!(code("0,. 0"), "F1");
        assert_eq!(code("0,.0.0"), "F2");
        assert_eq!(code("0.,0"), "F1");

        assert_eq!(code("\\0\\.\\0"), "G");
    }

    #[test]
    fn date_classes_are_a_prefix_match_of_op_classes() {
        assert_eq!(code("d/m/yy"), "D1");
        assert_eq!(code("dddd mmm yyyy"), "D1");
        assert_eq!(code("dddd d mmm yyyy"), "G");
        assert_eq!(code("d mmm"), "D2");
        assert_eq!(code("mmm-yy"), "D3");
        assert_eq!(code("m/d/yy"), "D4");
        assert_eq!(code("m/d"), "D5");
        assert_eq!(code("yyyy-mm-dd"), "G");
        assert_eq!(code("dd"), "G");
        assert_eq!(code("mmm"), "G");
        assert_eq!(code("h:mm:ss AM/PM"), "D6");
        assert_eq!(code("h:mm AM/PM"), "D7");
        assert_eq!(code("h:mm:ss"), "D8");
        assert_eq!(code("h:mm"), "D9");
        assert_eq!(code("h"), "G");
        assert_eq!(code("[$-409]m/d/yy"), "D2");
        assert_eq!(code("AM/PM h:mm"), "G");

        assert_eq!(code("d/m e"), "D2");
        assert_eq!(code("mmm e"), "G");

        assert_eq!(code("[h] dd-mmm-yy"), "D1");
        assert_eq!(code("d/m/yy [h]"), "D1");
        assert_eq!(code("[h]:mmm d"), "D5");
        assert_eq!(code("[h]:d/m/yy"), "G");
        assert_eq!(code("[h]:h:mm"), "G");

        assert_eq!(code("mmm[$-409]"), "D5");
        assert_eq!(code("B1m/d/yy"), "D2");
        assert_eq!(code("B2d/m/yy"), "D1");
        assert_eq!(code("[$-409][h]mmmm"), "D2");
    }

    #[test]
    fn digits_and_classes() {
        assert_eq!(code("0.00.00"), "F4");
        assert_eq!(code("0.0000000000000000"), "F15");
        assert_eq!(code("#,##0.00"), ",2");
        assert_eq!(code("0.0E+00"), "S1");
        assert_eq!(code("0%"), "P0");
        assert_eq!(code("$# ?/?"), "C1");
        assert_eq!(code("# ?/?"), "G");
    }
}
