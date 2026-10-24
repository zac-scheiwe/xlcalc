use super::ast::*;
use crate::address::col_to_letters;
use crate::number::format_general;

pub fn print(e: &Expr) -> String {
    let mut s = String::new();
    write_expr(&mut s, e);
    s
}

const QUOTED_INNER: &[(u32, u32)] = &[
    (0x80, 0xA0),
    (0xA2, 0xA3),
    (0xA5, 0xA6),
    (0xA9, 0xA9),
    (0xAB, 0xAC),
    (0xAE, 0xAE),
    (0xBB, 0xBB),
    (0x37E, 0x37E),
    (0x5BE, 0x5BE),
    (0x5C0, 0x5C0),
    (0x5C3, 0x5C3),
    (0x5C6, 0x5C6),
    (0x5EF, 0x5EF),
    (0x5F3, 0x5F4),
    (0x61C, 0x61C),
    (0x900, 0x900),
    (0x93A, 0x93B),
    (0x94E, 0x94F),
    (0x955, 0x957),
    (0x964, 0x965),
    (0x970, 0x970),
    (0x973, 0x97A),
    (0xE4F, 0xE4F),
    (0xE5A, 0xE5B),
    (0x115B, 0x115B),
    (0x11FC, 0x11FC),
    (0x2011, 0x2012),
    (0x2017, 0x2017),
    (0x201A, 0x201B),
    (0x201E, 0x201F),
    (0x2022, 0x2024),
    (0x2031, 0x2031),
    (0x2034, 0x2034),
    (0x2036, 0x203A),
    (0x203C, 0x2043),
    (0x2045, 0x2051),
    (0x2053, 0x205E),
    (0x2066, 0x2069),
    (0x207D, 0x207E),
    (0x208D, 0x208E),
    (0x2095, 0x209C),
    (0x20B6, 0x20C0),
    (0x2150, 0x2152),
    (0x2189, 0x218B),
    (0x232A, 0x232A),
    (0x23EE, 0x23EE),
    (0x23F5, 0x23F5),
    (0x23FC, 0x23FC),
    (0x26BD, 0x26BD),
    (0x26C4, 0x26C4),
    (0x26CB, 0x26CB),
    (0x26D2, 0x26D2),
    (0x26D9, 0x26D9),
    (0x26E0, 0x26E0),
    (0x26E7, 0x26E7),
    (0x26EE, 0x26EE),
    (0x26F5, 0x26F5),
    (0x26FC, 0x26FC),
    (0x2700, 0x2700),
    (0x2754, 0x2754),
    (0x2769, 0x2769),
    (0x2770, 0x2770),
    (0x3018, 0x301C),
    (0x3030, 0x3030),
    (0x303D, 0x303D),
    (0x30A0, 0x30A0),
    (0xFE32, 0xFE32),
    (0xFE45, 0xFE48),
];
const QUOTED_LEAD: &[(u32, u32)] = &[
    (0x80, 0xA0),
    (0xA2, 0xA3),
    (0xA5, 0xA6),
    (0xA9, 0xA9),
    (0xAB, 0xAC),
    (0xAE, 0xAE),
    (0xBB, 0xBB),
    (0x2B9, 0x2BA),
    (0x2C2, 0x2C6),
    (0x2C8, 0x2C8),
    (0x2CC, 0x2CC),
    (0x2CE, 0x2CF),
    (0x2D2, 0x2D7),
    (0x2DC, 0x2DC),
    (0x2DE, 0x2DF),
    (0x2E5, 0x2ED),
    (0x2EF, 0x2FF),
    (0x37E, 0x37E),
    (0x385, 0x385),
    (0x485, 0x485),
    (0x591, 0x5C7),
    (0x5EF, 0x5EF),
    (0x5F3, 0x5F4),
    (0x600, 0x600),
    (0x607, 0x607),
    (0x60E, 0x60E),
    (0x615, 0x615),
    (0x61C, 0x61C),
    (0x64D, 0x64D),
    (0x654, 0x654),
    (0x65B, 0x65B),
    (0x660, 0x669),
    (0x662, 0x662),
    (0x669, 0x669),
    (0x670, 0x670),
    (0x6D9, 0x6D9),
    (0x6E0, 0x6E0),
    (0x6E7, 0x6E7),
    (0x6F0, 0x6F9),
    (0x6F5, 0x6F5),
    (0x900, 0x903),
    (0x93A, 0x93C),
    (0x93E, 0x94F),
    (0x951, 0x957),
    (0x962, 0x970),
    (0x973, 0x97A),
    (0xE3F, 0xE3F),
    (0xE4F, 0xE5B),
    (0x115B, 0x115B),
    (0x11FC, 0x11FC),
    (0x2000, 0x200F),
    (0x2011, 0x2012),
    (0x2017, 0x2017),
    (0x201A, 0x201B),
    (0x201E, 0x201F),
    (0x2022, 0x2024),
    (0x2028, 0x202F),
    (0x2031, 0x2031),
    (0x2034, 0x2034),
    (0x2036, 0x203A),
    (0x203C, 0x2064),
    (0x2066, 0x2070),
    (0x2075, 0x207E),
    (0x2080, 0x2080),
    (0x2085, 0x208E),
    (0x2095, 0x209C),
    (0x20A0, 0x20C0),
    (0x2100, 0x2101),
    (0x2104, 0x2104),
    (0x2106, 0x2106),
    (0x2108, 0x2108),
    (0x2114, 0x2114),
    (0x2117, 0x2118),
    (0x211E, 0x2120),
    (0x2123, 0x2123),
    (0x2125, 0x2125),
    (0x2127, 0x2127),
    (0x2129, 0x2129),
    (0x212E, 0x212E),
    (0x213A, 0x213B),
    (0x2140, 0x2144),
    (0x214A, 0x214D),
    (0x214F, 0x2152),
    (0x2155, 0x215A),
    (0x215F, 0x215F),
    (0x2189, 0x218B),
    (0x219A, 0x21D1),
    (0x21D3, 0x21D3),
    (0x21D5, 0x21FF),
    (0x220E, 0x220E),
    (0x221C, 0x221C),
    (0x2231, 0x2231),
    (0x2238, 0x2238),
    (0x223F, 0x223F),
    (0x2246, 0x2246),
    (0x224D, 0x224D),
    (0x2254, 0x2254),
    (0x225B, 0x225B),
    (0x2262, 0x2262),
    (0x2269, 0x2269),
    (0x2270, 0x2270),
    (0x2277, 0x2277),
    (0x227E, 0x227E),
    (0x2285, 0x2285),
    (0x228C, 0x228C),
    (0x2293, 0x2293),
    (0x229A, 0x229A),
    (0x22A1, 0x22A1),
    (0x22A8, 0x22A8),
    (0x22AF, 0x22AF),
    (0x22B6, 0x22B6),
    (0x22BD, 0x22BD),
    (0x22C4, 0x22C4),
    (0x22CB, 0x22CB),
    (0x22D2, 0x22D2),
    (0x22D9, 0x22D9),
    (0x22E0, 0x22E0),
    (0x22E7, 0x22E7),
    (0x22EE, 0x22EE),
    (0x22F5, 0x22F5),
    (0x22FC, 0x22FC),
    (0x2300, 0x2300),
    (0x2307, 0x2307),
    (0x230E, 0x230E),
    (0x2315, 0x2315),
    (0x231C, 0x231C),
    (0x2323, 0x2323),
    (0x232A, 0x232A),
    (0x2331, 0x2331),
    (0x2338, 0x2338),
    (0x233F, 0x233F),
    (0x2346, 0x2346),
    (0x234D, 0x234D),
    (0x2354, 0x2354),
    (0x235B, 0x235B),
    (0x2362, 0x2362),
    (0x2369, 0x2369),
    (0x2370, 0x2370),
    (0x2377, 0x2377),
    (0x237E, 0x237E),
    (0x2385, 0x2385),
    (0x238C, 0x238C),
    (0x2393, 0x2393),
    (0x239A, 0x239A),
    (0x23A1, 0x23A1),
    (0x23A8, 0x23A8),
    (0x23AF, 0x23AF),
    (0x23B6, 0x23B6),
    (0x23BD, 0x23BD),
    (0x23C4, 0x23C4),
    (0x23CB, 0x23CB),
    (0x23D2, 0x23D2),
    (0x23D9, 0x23D9),
    (0x23E0, 0x23E0),
    (0x23E7, 0x23E7),
    (0x23EE, 0x23EE),
    (0x23F5, 0x23F5),
    (0x23FC, 0x23FC),
    (0x24BB, 0x24BB),
    (0x24C2, 0x24C2),
    (0x24C9, 0x24C9),
    (0x24EC, 0x24EC),
    (0x24F3, 0x24F3),
    (0x24FA, 0x24FA),
    (0x254D, 0x254D),
    (0x2577, 0x2577),
    (0x257E, 0x257E),
    (0x259A, 0x259A),
    (0x25AF, 0x25AF),
    (0x25C4, 0x25C4),
    (0x25D2, 0x25D2),
    (0x25D9, 0x25D9),
    (0x25E0, 0x25E0),
    (0x25E7, 0x25E7),
    (0x25EE, 0x25EE),
    (0x25F5, 0x25F5),
    (0x25FC, 0x25FC),
    (0x2600, 0x2600),
    (0x2607, 0x2607),
    (0x2615, 0x2615),
    (0x2623, 0x2623),
    (0x262A, 0x262A),
    (0x2631, 0x2631),
    (0x2638, 0x2638),
    (0x263F, 0x263F),
    (0x2646, 0x2646),
    (0x264D, 0x264D),
    (0x2654, 0x2654),
    (0x265B, 0x265B),
    (0x2662, 0x2662),
    (0x2670, 0x2670),
    (0x2677, 0x2677),
    (0x267E, 0x267E),
    (0x2685, 0x2685),
    (0x268C, 0x268C),
    (0x2693, 0x2693),
    (0x269A, 0x269A),
    (0x26A1, 0x26A1),
    (0x26A8, 0x26A8),
    (0x26AF, 0x26AF),
    (0x26B6, 0x26B6),
    (0x26BD, 0x26BD),
    (0x26C4, 0x26C4),
    (0x26CB, 0x26CB),
    (0x26D2, 0x26D2),
    (0x26D9, 0x26D9),
    (0x26E0, 0x26E0),
    (0x26E7, 0x26E7),
    (0x26EE, 0x26EE),
    (0x26F5, 0x26F5),
    (0x26FC, 0x26FC),
    (0x2700, 0x2700),
    (0x2707, 0x2707),
    (0x270E, 0x270E),
    (0x2715, 0x2715),
    (0x271C, 0x271C),
    (0x2723, 0x2723),
    (0x272A, 0x272A),
    (0x2731, 0x2731),
    (0x2738, 0x2738),
    (0x273F, 0x273F),
    (0x2746, 0x2746),
    (0x274D, 0x274D),
    (0x2754, 0x2754),
    (0x275B, 0x275B),
    (0x2762, 0x2762),
    (0x2769, 0x2769),
    (0x2770, 0x2770),
    (0x2777, 0x2777),
    (0x277E, 0x277E),
    (0x2785, 0x2785),
    (0x278C, 0x278C),
    (0x2793, 0x2793),
    (0x279A, 0x279A),
    (0x27A1, 0x27A1),
    (0x27A8, 0x27A8),
    (0x27AF, 0x27AF),
    (0x27B6, 0x27B6),
    (0x27BD, 0x27BD),
    (0x3004, 0x3004),
    (0x3018, 0x301C),
    (0x3020, 0x3020),
    (0x302A, 0x3030),
    (0x3036, 0x3037),
    (0x303D, 0x303F),
    (0x3099, 0x309A),
    (0x30A0, 0x30A0),
    (0xFE32, 0xFE32),
    (0xFE45, 0xFE48),
    (0xFFEE, 0xFFEE),
    (0xFFF9, 0xFFFD),
];

fn in_ranges(t: &[(u32, u32)], c: char) -> bool {
    let c = c as u32;
    t.iter().any(|&(a, b)| (a..=b).contains(&c))
}

fn sheet_char_plain(c: char, first: bool) -> bool {
    if c.is_ascii() {
        return c.is_ascii_alphabetic() || c == '_' || (!first && (c.is_ascii_digit() || c == '.'));
    }
    !in_ranges(if first { QUOTED_LEAD } else { QUOTED_INNER }, c)
}

pub fn sheet_name_text(name: &str) -> String {
    let plain = !name.is_empty()
        && name
            .chars()
            .enumerate()
            .all(|(i, c)| sheet_char_plain(c, i == 0))
        && crate::address::parse_a1_cell(name).is_none()
        && !looks_like_r1c1(name)
        && !name.eq_ignore_ascii_case("true")
        && !name.eq_ignore_ascii_case("false");
    if plain {
        name.to_string()
    } else {
        format!("'{}'", name.replace('\'', "''"))
    }
}

fn looks_like_r1c1(name: &str) -> bool {
    let up = name.to_ascii_uppercase();
    let rest = match up.strip_prefix('R') {
        Some(r) => r.trim_start_matches(|c: char| c.is_ascii_digit()),
        None => &up,
    };
    match rest.strip_prefix('C') {
        Some(r) => r.chars().all(|c| c.is_ascii_digit()),
        None => rest.is_empty() && up.starts_with('R'),
    }
}

pub fn sheet_spec_text(spec: &SheetSpec) -> String {
    match spec {
        SheetSpec::Single(n) => format!("{}!", sheet_name_text(n)),
        SheetSpec::Range(a, b) => {
            let combined = format!("{a}:{b}");
            if sheet_name_text(a) == *a && sheet_name_text(b) == *b {
                format!("{combined}!")
            } else {
                format!("'{}'!", combined.replace('\'', "''"))
            }
        }
        SheetSpec::External { book, sheet } => match sheet {
            Some(s) => format!("[{book}]{s}!"),
            None => format!("[{book}]!"),
        },
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

fn write_expr_inner(s: &mut String, e: &Expr) {
    match e {
        Expr::Number(n) => s.push_str(&format_general(*n)),
        Expr::Text(t) => s.push_str(&text_literal(t)),
        Expr::Bool(b) => s.push_str(if *b { "TRUE" } else { "FALSE" }),
        Expr::Error(err) => s.push_str(err.as_str()),
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
            let bare = crate::formula::strip_prefixes(name);
            s.push_str(&name[..name.len() - bare.len()]);
            s.push_str(&bare.to_uppercase());
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
                UnaryOp::ImplicitQuiet | UnaryOp::ArrayZero => "",
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
            write_expr(s, left);
            s.push_str(op.symbol());
            write_expr(s, right);
        }
        Expr::Paren(x) => {
            s.push('(');
            write_expr(s, x);
            s.push(')');
        }
        Expr::Missing => {}
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
