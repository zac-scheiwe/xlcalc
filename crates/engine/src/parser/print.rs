use super::ast::*;
use crate::address::col_to_letters;
use crate::number::format_general;

pub fn print(e: &Expr) -> String {
    let mut s = String::new();
    write_expr(&mut s, e);
    s
}

pub fn bare_sheet_char(c: char) -> bool {
    if c.is_ascii() {
        c.is_ascii_alphanumeric() || matches!(c, '_' | '.')
    } else {
        super::namech::name_char(c, false)
    }
}

pub fn bare_sheet_name_ok(name: &str) -> bool {
    let start_ok =
        |c: char| c.is_ascii() || super::namech::name_char(c, true) || super::namech::is_digit(c);
    name.starts_with(start_ok)
        && name
            .chars()
            .enumerate()
            .all(|(i, c)| bare_sheet_char(c) || (i > 0 && c == '?'))
        && !name.eq_ignore_ascii_case("true")
        && !name.eq_ignore_ascii_case("false")
        && !super::starts_like_r1c1(name)
}

pub fn sheet_name_text(name: &str) -> String {
    let bare = |s: &str| super::prsnam::prs_nam(s, true) || super::prsnam::bracket_word(s);
    let book_form = || {
        let Some((book, sheet)) = name.strip_prefix('[').and_then(|r| r.split_once(']')) else {
            return false;
        };
        bare(book)
            && (sheet.is_empty()
                || bare(sheet)
                || sheet
                    .split_once(':')
                    .is_some_and(|(a, b)| bare(a) && bare(b)))
    };
    if name.is_empty() || bare(name) || book_form() {
        name.to_string()
    } else {
        format!("'{}'", name.replace('\'', "''"))
    }
}

pub(crate) fn index_prefix_text(body: &str) -> String {
    let sheets = body.split_once(']').map_or("", |(_, s)| s);
    if sheets.is_empty() || sheets.split(':').all(|s| super::prsnam::prs_nam(s, true)) {
        body.to_string()
    } else {
        sheet_name_text(body)
    }
}

fn need_ticks(sheet: &str) -> bool {
    sheet.char_indices().any(|(i, c)| match c {
        '.' | '\u{3002}' => !super::prsnam::easy_accepts(&sheet[..i]),
        '\u{2018}' | '\u{2019}' | '\u{201d}' | '\u{3000}' | '\u{3001}' => true,
        '\u{ff01}'..='\u{ff09}' | '\u{ff0b}'..='\u{ff0e}' | '\u{ff1b}'..='\u{ff1e}' => true,
        '\u{ff20}' | '\u{ff3e}' | '\u{ff40}' | '\u{ff5b}'..='\u{ff5e}' => true,
        '\u{ffe5}' | '\u{ffe6}' => true,
        _ => false,
    })
}

fn prefix_char(c: char) -> bool {
    !c.is_ascii() || c.is_ascii_alphanumeric() || matches!(c, '_' | '.' | '?' | '\\' | '$')
}

fn run_end(text: &str, from: usize) -> usize {
    text[from..]
        .find(|c| !prefix_char(c))
        .map_or(text.len(), |n| from + n)
}

fn cell_word(text: &str) -> (String, String) {
    let word: String = text[..run_end(text, 0)]
        .chars()
        .filter(|&c| c != '$')
        .collect();
    let letters = word.chars().take_while(char::is_ascii_alphabetic).count();
    (word[..letters].to_string(), word[letters..].to_string())
}

fn cell_like(text: &str) -> bool {
    let (letters, digits) = cell_word(text);
    (1..=3).contains(&letters.len())
        && !digits.is_empty()
        && digits.bytes().all(|b| b.is_ascii_digit())
}

fn reference_follows(rest: &str) -> bool {
    if rest.starts_with("#REF!") {
        return true;
    }
    let (letters, digits) = cell_word(rest);
    let numbered = !digits.is_empty() && digits.bytes().all(|b| b.is_ascii_digit());
    let second = rest[run_end(rest, 0)..].strip_prefix(':').map(cell_word);
    match letters.len() {
        0 => numbered && second.is_some(),
        1..=3 => {
            numbered
                || (digits.is_empty()
                    && second.is_some_and(|(l, d)| (1..=3).contains(&l.len()) && d.is_empty()))
        }
        _ => false,
    }
}

fn quoted_end(text: &str, from: usize, quote: u8) -> usize {
    let b = text.as_bytes();
    let mut i = from + 1;
    while i < b.len() {
        if b[i] == quote {
            if b.get(i + 1) != Some(&quote) {
                return i + 1;
            }
            i += 1;
        }
        i += 1;
    }
    b.len()
}

fn bracket_end(text: &str, from: usize) -> usize {
    let mut depth = 0usize;
    let mut chars = text[from..].char_indices();
    while let Some((i, c)) = chars.next() {
        match c {
            '\'' => {
                chars.next();
            }
            '[' => depth += 1,
            ']' => {
                depth = depth.saturating_sub(1);
                if depth == 0 {
                    return from + i + 1;
                }
            }
            _ => {}
        }
    }
    text.len()
}

fn prefix_end(text: &str, from: usize, after_bang: bool) -> usize {
    let end = run_end(text, from);
    if !after_bang && !cell_like(&text[from..end]) && text[end..].starts_with(':') {
        let last = run_end(text, end + 1);
        if text[last..].starts_with('!') {
            return last;
        }
    }
    end
}

pub fn stored_formula_text(text: &str) -> String {
    if !text.contains('!') {
        return text.to_string();
    }
    let mut out = String::with_capacity(text.len() + 8);
    let mut i = 0;
    while let Some(c) = text[i..].chars().next() {
        let after_bang = out.ends_with('!');
        let end = match c {
            '"' | '\'' => quoted_end(text, i, c as u8),
            '[' => {
                let close = bracket_end(text, i);
                let index = text[i + 1..close]
                    .strip_suffix(']')
                    .is_some_and(|n| !n.is_empty() && n.bytes().all(|b| b.is_ascii_digit()));
                if index && text[close..].starts_with(prefix_char) {
                    prefix_end(text, close, after_bang)
                } else {
                    close
                }
            }
            c if prefix_char(c) => prefix_end(text, i, after_bang),
            c => i + c.len_utf8(),
        };
        let piece = &text[i..end];
        let sheets = piece.split_once(']').map_or(piece, |(_, s)| s);
        let first = sheets.split_once(':').map_or(sheets, |(a, _)| a);
        let operand = out.trim_end().ends_with(':');
        let forced = !piece.starts_with(['"', '\''])
            && text[end..].starts_with('!')
            && reference_follows(&text[end + 1..])
            && (need_ticks(first) || (operand && !text[end + 1..].starts_with('#')));
        if forced {
            out.push('\'');
            out.push_str(&piece.replace('\'', "''"));
            out.push('\'');
        } else {
            out.push_str(piece);
        }
        i = end;
    }
    out
}

pub fn sheet_spec_text(spec: &SheetSpec) -> String {
    match spec {
        SheetSpec::Single(n) if n.is_empty() => "!".to_string(),
        SheetSpec::Single(n) => format!("{}!", sheet_name_text(n)),
        SheetSpec::Range(a, b) => {
            let combined = format!("{a}:{b}");
            if sheet_name_text(a) == *a && sheet_name_text(b) == *b {
                format!("{combined}!")
            } else {
                format!("'{}'!", combined.replace('\'', "''"))
            }
        }

        SheetSpec::External { book, sheet: None } if book.bytes().all(|b| b.is_ascii_digit()) => {
            format!("[{book}]")
        }
        SheetSpec::External { book, sheet } => {
            let s = sheet.as_deref().unwrap_or("");
            let (dir, name) = crate::workbook::ext::split_path(book);
            let body = format!("{dir}[{name}]{s}");
            if !dir.is_empty() {
                format!("'{}'!", body.replace('\x27', "''"))
            } else if name.bytes().all(|b| b.is_ascii_digit()) {
                format!("{}!", index_prefix_text(&body))
            } else {
                format!("{}!", sheet_name_text(&body))
            }
        }
    }
}

fn coord_col(c: Coord) -> String {
    format!(
        "{}{}",
        if c.abs { "$" } else { "" },
        col_to_letters(c.index)
    )
}

fn coord_row(r: Coord) -> String {
    format!("{}{}", if r.abs { "$" } else { "" }, r.index + 1)
}

pub fn ref_kind_text(kind: &RefKind) -> String {
    match *kind {
        RefKind::Cell { row, col } => format!("{}{}", coord_col(col), coord_row(row)),
        RefKind::Area { r0, c0, r1, c1 } => {
            format!(
                "{}{}:{}{}",
                coord_col(c0),
                coord_row(r0),
                coord_col(c1),
                coord_row(r1)
            )
        }
        RefKind::Cols { c0, c1 } => format!("{}:{}", coord_col(c0), coord_col(c1)),
        RefKind::Rows { r0, r1 } => format!("{}:{}", coord_row(r0), coord_row(r1)),
    }
}

pub fn text_literal(s: &str) -> String {
    format!("\"{}\"", s.replace('"', "\"\""))
}

fn write_items(s: &mut String, rows: &[Vec<ArrayItem>]) {
    s.push('{');
    for (ri, row) in rows.iter().enumerate() {
        if ri > 0 {
            s.push(';');
        }
        for (ci, it) in row.iter().enumerate() {
            if ci > 0 {
                s.push(',');
            }
            match it {
                ArrayItem::Number(n) => s.push_str(&format_general(*n)),
                ArrayItem::Text(t) => s.push_str(&text_literal(t)),
                ArrayItem::Bool(b) => s.push_str(if *b { "TRUE" } else { "FALSE" }),
                ArrayItem::Error(e) => s.push_str(e.as_str()),
            }
        }
    }
    s.push('}');
}

fn write_args(s: &mut String, args: &[Expr]) {
    s.push('(');
    for (i, a) in args.iter().enumerate() {
        if i > 0 {
            s.push(',');
        }
        write_expr(s, a);
    }
    s.push(')');
}

pub(super) fn write_expr(s: &mut String, e: &Expr) {
    crate::deep(|| write_expr_inner(s, e))
}

fn ends_with_ws(e: &Expr) -> bool {
    match e {
        Expr::Ws { after, .. } => !after.is_empty(),
        Expr::Unary { expr, .. } => ends_with_ws(expr),
        _ => false,
    }
}

fn write_expr_inner(s: &mut String, e: &Expr) {
    match e {
        Expr::Number(n) => s.push_str(&format_general(*n)),
        Expr::Text(t) => s.push_str(&text_literal(t)),
        Expr::Bool(b) => s.push_str(if *b { "TRUE" } else { "FALSE" }),
        Expr::Error(err) => s.push_str(err.as_str()),
        Expr::DeadRef(sp) => {
            s.push_str(&sheet_spec_text(sp));
            s.push_str("#REF!");
        }
        Expr::DeadTable { sheet, text } => {
            s.push_str(&sheet_spec_text(sheet));
            s.push_str(text);
        }
        Expr::Array(rows) => write_items(s, rows),
        Expr::Ref { sheet, kind } => {
            if let Some(sp) = sheet {
                s.push_str(&sheet_spec_text(sp));
            }
            s.push_str(&ref_kind_text(kind));
        }
        Expr::Name { sheet, name } => {
            if let Some(sp) = sheet {
                s.push_str(&sheet_spec_text(sp));
            }

            if crate::address::parse_a1_cell(name).is_some() {
                s.push('\u{1}');
            }
            s.push_str(name);
        }
        Expr::StructRef { table, spec } => {
            if let Some(t) = table {
                s.push_str(t);
            }
            s.push('[');
            s.push_str(spec);
            s.push(']');
        }
        Expr::Func { name, args } => {
            let name = match name.rsplit_once('!') {
                Some((book, f)) if super::is_addin_file(book) => f,
                _ => name.as_str(),
            };
            let bare = crate::formula::strip_prefixes(name);
            s.push_str(&name[..name.len() - bare.len()]);
            if let Some((book, func)) = bare.rsplit_once('!') {
                s.push_str(&sheet_name_text(book));
                s.push('!');
                s.push_str(func);
                write_args(s, args);
                return;
            }

            if crate::functions::lookup(&bare.to_uppercase()).is_some() {
                s.push_str(&bare.to_uppercase());
            } else {
                s.push_str(bare);
            }
            write_args(s, args);
        }
        Expr::Call { callee, args } => {
            write_expr(s, callee);
            write_args(s, args);
        }
        Expr::Unary { op, expr } => {
            s.push_str(match op {
                UnaryOp::Neg => "-",
                UnaryOp::Plus => "+",
                UnaryOp::Implicit => "@",
                UnaryOp::ArrayZero => "",
            });
            write_expr(s, expr);
        }
        Expr::Percent(x) => {
            write_expr(s, x);
            s.push('%');
        }
        Expr::Spill(x) => {
            write_expr(s, x);
            s.push('#');
        }
        Expr::Field { expr, field } => {
            write_expr(s, expr);
            s.push_str(field);
        }
        Expr::Binary { op, left, right } => {
            let mark = |s: &mut String, e: &Expr| {
                if *op == BinOp::Range
                    && !ends_with_ws(left)
                    && matches!(&**left, Expr::Name { .. })
                    && matches!(e, Expr::Name { sheet: None, name }
                        if crate::address::letters_to_col(name).is_some()
                            && crate::address::parse_a1_cell(name).is_none())
                {
                    s.push('\u{1}');
                }
            };
            write_expr(s, left);

            let typed = ends_with_ws(left);
            if !(*op == BinOp::Intersect && typed) {
                s.push_str(op.symbol());
            }
            mark(s, right);
            write_expr(s, right);
        }
        Expr::Paren(x) => {
            s.push('(');
            write_expr(s, x);
            s.push(')');
        }
        Expr::Missing | Expr::Unwritten => {}
        Expr::Ws {
            before,
            after,
            expr,
        } => {
            s.push_str(before);
            write_expr(s, expr);
            s.push_str(after);
        }
    }
}

#[cfg(test)]
mod stored_tests {
    use super::{need_ticks, stored_formula_text};

    #[test]
    fn dotted_sheet_names() {
        for forced in [
            "A1.x",
            "ab.cd.ef",
            "abc1.x",
            "R.x",
            "r.x",
            "Rx.y",
            "C.x",
            "TRUE.x",
            "x_1.y",
            "_a.b",
            "1.5",
            ".x",
            "ab.A1.",
            "Q1.2024",
            "a1234.b",
            "a\u{ff0b}b",
            "a\u{ff08}x",
            "a\u{ff0e}y",
            "a\u{3001}b",
            "a\u{2019}b",
            "a\u{3000}b",
            "a\u{ffe5}",
            "a\u{ff5e}",
            "a\u{ff01}",
            "a\u{ff05}",
        ] {
            assert!(need_ticks(forced), "{forced}");
        }
        for bare in [
            "Data",
            "A.1",
            "a.b",
            "ab.x",
            "data.2019",
            "\u{e9}t\u{e9}.x",
            "\u{4e2d}\u{6587}.x",
            "ab.",
            "a\u{3002}b",
            "ab\u{3002}c",
            "\u{ff41}\u{ff42}",
            "a\u{ff1f}b",
            "a\u{ff1a}",
            "a\u{ff0a}",
            "abcd.R1C1",
            "abcde1.x",
            "Sheet.1",
            "TRUE1.x",
            "ab1cd.e",
            "\u{ff32}.x",
        ] {
            assert!(!need_ticks(bare), "{bare}");
        }
    }

    #[test]
    fn stored_text() {
        for (text, stored) in [
            ("A1.x!A1", "'A1.x'!A1"),
            ("=A1.x!$A$1+ab.x!B2", "='A1.x'!$A$1+ab.x!B2"),
            ("[1]A1.x!A1", "'[1]A1.x'!A1"),
            ("[1]ab.x!A1", "[1]ab.x!A1"),
            ("SUM(A1.x:ab.x!A1)", "SUM('A1.x:ab.x'!A1)"),
            ("SUM(a.b:A1.x!A1)", "SUM(a.b:A1.x!A1)"),
            ("SUM([1]Data:A1.x!A1)", "SUM([1]Data:A1.x!A1)"),
            ("SUM([1]A1.x:ab.x!A1)", "SUM('[1]A1.x:ab.x'!A1)"),
            ("A1.x!A:A", "'A1.x'!A:A"),
            ("SUM(A1.x!$1:$1)", "SUM('A1.x'!$1:$1)"),
            ("SUM(A1.x!A1:B2 A1.x!B2)", "SUM('A1.x'!A1:B2 'A1.x'!B2)"),
            ("SUM(Main!B1:A1.x!B2)", "SUM(Main!B1:'A1.x'!B2)"),
            ("SUM(Main!B1:ab.x!B2)", "SUM(Main!B1:'ab.x'!B2)"),
            ("SUM(B1:Data!B2)", "SUM(B1:'Data'!B2)"),
            ("SUM($B$1:[1]Data!B2)", "SUM($B$1:'[1]Data'!B2)"),
            ("SUM(Main!B1:(ab.x!B2))", "SUM(Main!B1:(ab.x!B2))"),
            ("SUM(Main!B1:ab.x!loc)", "SUM(Main!B1:ab.x!loc)"),
            ("SUM(A1.x!B1:B1.y!B2)", "SUM('A1.x'!B1:'B1.y'!B2)"),
            ("A1.x!#REF!", "'A1.x'!#REF!"),
            (
                "A1.x!loc+[1]A1.x!loc+[1]!glob",
                "A1.x!loc+[1]A1.x!loc+[1]!glob",
            ),
            (
                "INDIRECT(\"A1.x!A1\")&'A1.x'!A1",
                "INDIRECT(\"A1.x!A1\")&'A1.x'!A1",
            ),
            (
                "'my A1.x'!A1+Table1[A1.x!A1]",
                "'my A1.x'!A1+Table1[A1.x!A1]",
            ),
            ("#REF!A1+#DIV/0!", "#REF!A1+#DIV/0!"),
        ] {
            assert_eq!(stored_formula_text(text), stored, "{text}");
            assert_eq!(stored_formula_text(stored), stored, "{stored}");
        }
    }
}
