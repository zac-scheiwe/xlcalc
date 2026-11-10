use xlcalc::{col_to_letters, parse_range_ref, Rect, SheetId, Workbook};

pub(crate) const MAX_ROWS: u32 = 1_048_576;
pub(crate) const MAX_COLS: u32 = 16_384;

pub(crate) struct AddressStyle {
    pub row_abs: bool,
    pub col_abs: bool,
    pub r1c1: bool,

    pub relative_to: (u32, u32),
}

pub(crate) fn quote_sheet(name: &str) -> String {
    let plain = name
        .chars()
        .all(|c| c.is_alphanumeric() || c == '_' || c == '.')
        && !name.starts_with(|c: char| c.is_ascii_digit());
    if plain {
        name.to_string()
    } else {
        format!("'{}'", name.replace('\'', "''"))
    }
}

pub(crate) fn external_prefix(book: &str, sheet: &str) -> String {
    let plain = |s: &str| quote_sheet(s) == s;
    if plain(book) && plain(sheet) {
        format!("[{book}]{sheet}!")
    } else {
        format!("'[{book}]{}'!", sheet.replace('\'', "''"))
    }
}

fn a1_axis(n: u32, abs: bool, col: bool) -> String {
    let d = if abs { "$" } else { "" };
    if col {
        format!("{d}{}", col_to_letters(n))
    } else {
        format!("{d}{}", n + 1)
    }
}

fn r1c1_axis(n: u32, abs: bool, letter: char, host: u32) -> String {
    if abs {
        format!("{letter}{}", n + 1)
    } else if n == host {
        letter.to_string()
    } else {
        format!("{letter}[{}]", n as i64 - host as i64)
    }
}

pub(crate) fn range_text(rect: Rect, st: &AddressStyle) -> String {
    let (hr, hc) = st.relative_to;
    let all_cols = rect.c0 == 0 && rect.c1 == MAX_COLS - 1;
    let all_rows = rect.r0 == 0 && rect.r1 == MAX_ROWS - 1;
    let cell = |r: u32, c: u32| {
        if st.r1c1 {
            format!(
                "{}{}",
                r1c1_axis(r, st.row_abs, 'R', hr),
                r1c1_axis(c, st.col_abs, 'C', hc)
            )
        } else {
            format!(
                "{}{}",
                a1_axis(c, st.col_abs, true),
                a1_axis(r, st.row_abs, false)
            )
        }
    };
    let row = |r: u32| {
        if st.r1c1 {
            r1c1_axis(r, st.row_abs, 'R', hr)
        } else {
            a1_axis(r, st.row_abs, false)
        }
    };
    let col = |c: u32| {
        if st.r1c1 {
            r1c1_axis(c, st.col_abs, 'C', hc)
        } else {
            a1_axis(c, st.col_abs, true)
        }
    };
    if all_cols {
        format!("{}:{}", row(rect.r0), row(rect.r1))
    } else if all_rows {
        format!("{}:{}", col(rect.c0), col(rect.c1))
    } else if rect.is_cell() {
        cell(rect.r0, rect.c0)
    } else {
        format!("{}:{}", cell(rect.r0, rect.c0), cell(rect.r1, rect.c1))
    }
}

pub(crate) fn parse_range_text(
    wb: &Workbook,
    default_sheet: SheetId,
    text: &str,
) -> Option<(SheetId, Rect)> {
    let text = text.trim();

    if let Some(i) = text.rfind('!') {
        let name = &text[..i];
        if !name.starts_with('\'') && !name.starts_with('[') && quote_sheet(name) != name {
            return None;
        }
    }
    if let Some(found) = parse_range_ref(wb, text, default_sheet) {
        return Some(found);
    }
    let (sheet, body) = match text.rfind('!') {
        Some(i) => {
            let name = text[..i].trim_matches('\'').replace("''", "'");
            (wb.sheet_by_name(&name)?.id, &text[i + 1..])
        }
        None => (default_sheet, text),
    };
    if body.matches(':').count() >= 2 {
        let mut all: Option<Rect> = None;
        for part in body.split(':') {
            let (_, r) = parse_range_ref(wb, part, sheet)?;
            all = Some(all.map_or(r, |a| a.union_bounds(&r)));
        }
        return all.map(|r| (sheet, r));
    }
    if !body.contains(':') && !body.is_empty() {
        let def = wb.names().lookup(&body.to_uppercase(), sheet)?;
        let target = def.refers_to.strip_prefix('=')?;
        if target.eq_ignore_ascii_case(body) {
            return None;
        }
        return parse_range_text(wb, sheet, target);
    }
    None
}

fn split_top(text: &str, separator: char) -> Vec<&str> {
    let mut parts = Vec::new();
    let (mut start, mut quoted) = (0, false);
    for (i, c) in text.char_indices() {
        if c == '\'' {
            quoted = !quoted;
        } else if c == separator && !quoted {
            parts.push(&text[start..i]);
            start = i + c.len_utf8();
        }
    }
    parts.push(&text[start..]);
    parts
}

pub(crate) fn parse_areas_text(
    wb: &Workbook,
    default_sheet: SheetId,
    text: &str,
) -> Option<(SheetId, Vec<Rect>)> {
    let mut text = text.trim();
    if text.starts_with('(') && text.ends_with(')') {
        text = text[1..text.len() - 1].trim();
    }
    let mut sheet = None;
    let mut areas = Vec::new();
    for part in split_top(text, ',') {
        let mut found: Option<(SheetId, Rect)> = None;
        for piece in split_top(part.trim(), ' ')
            .into_iter()
            .filter(|p| !p.is_empty())
        {
            let (s, rect) = parse_range_text(wb, default_sheet, piece)?;
            found = match found {
                None => Some((s, rect)),
                Some((fs, fr)) if fs == s => Some((s, fr.intersection(&rect)?)),
                Some(_) => return None,
            };
        }
        let (s, rect) = found?;
        if *sheet.get_or_insert(s) != s {
            return None;
        }
        areas.push(rect);
    }
    Some((sheet?, areas))
}
