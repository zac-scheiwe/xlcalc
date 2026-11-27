use xlcalc::{col_to_letters, parse_range_ref, Rect, SheetId, Workbook};

pub(crate) const XLSX_ROWS: u32 = 1_048_576;
const XLSX_COLS: u32 = 16_384;
const XLS_ROWS: u32 = 65_536;
const XLS_COLS: u32 = 256;

thread_local! {

    static COMPAT: std::cell::Cell<bool> = const { std::cell::Cell::new(false) };
}

pub(crate) fn set_compat(on: bool) {
    COMPAT.with(|c| c.set(on));
}

pub(crate) fn max_rows() -> u32 {
    if COMPAT.with(|c| c.get()) {
        XLS_ROWS
    } else {
        XLSX_ROWS
    }
}

pub(crate) fn max_cols() -> u32 {
    if COMPAT.with(|c| c.get()) {
        XLS_COLS
    } else {
        XLSX_COLS
    }
}

pub(crate) fn fit_grid(rect: Rect) -> Option<Rect> {
    if !COMPAT.with(|c| c.get()) {
        return Some(rect);
    }
    let r1 = if rect.r1 == XLSX_ROWS - 1 {
        XLS_ROWS - 1
    } else {
        rect.r1
    };
    let c1 = if rect.c1 == XLSX_COLS - 1 {
        XLS_COLS - 1
    } else {
        rect.c1
    };
    (r1 < XLS_ROWS && c1 < XLS_COLS && rect.r0 <= r1 && rect.c0 <= c1)
        .then(|| Rect::new(rect.r0, rect.c0, r1, c1))
}

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
    format!(
        "{}!",
        xlcalc::testing::sheet_name_text(&format!("[{book}]{sheet}"))
    )
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
    let all_cols = rect.c0 == 0 && rect.c1 == max_cols() - 1;
    let all_rows = rect.r0 == 0 && rect.r1 == max_rows() - 1;
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

    if all_cols && st.r1c1 && rect.r0 == rect.r1 {
        row(rect.r0)
    } else if all_rows && st.r1c1 && rect.c0 == rect.c1 {
        col(rect.c0)
    } else if all_cols {
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

    if let Some(found) = wb.table_text_area(body, xlcalc::CellAddr::new(sheet, 0, 0)) {
        return found.ok().map(|a| (a.sheet, a.rect));
    }
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
        let target = def.refers_to.strip_prefix('=').unwrap_or(&def.refers_to);
        if target.eq_ignore_ascii_case(body) {
            return None;
        }
        return parse_range_text(wb, sheet, target);
    }
    None
}

fn split_top(text: &str, separator: char) -> Vec<&str> {
    let mut parts = Vec::new();
    let (mut start, mut quoted, mut depth) = (0, false, 0u32);
    for (i, c) in text.char_indices() {
        if c == '\'' && depth == 0 {
            quoted = !quoted;
        } else if c == '[' && !quoted {
            depth += 1;
        } else if c == ']' && !quoted {
            depth = depth.saturating_sub(1);
        } else if c == separator && !quoted && depth == 0 {
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

pub(crate) fn sz_to_ref(text: &str, append: bool) -> Option<Rect> {
    let text: String = text.chars().take(255).collect();
    let both = if !append || text.contains(':') {
        text
    } else {
        format!("{text}:{text}")
    };
    let both = both.trim_start_matches(' ');
    if !both
        .chars()
        .all(|c| c.is_ascii_alphanumeric() || c == '$' || c == ':')
    {
        return None;
    }
    let r = xlcalc::parse_a1_rect(both)?;
    Some(Rect::new(
        r.r0.min(r.r1),
        r.c0.min(r.c1),
        r.r0.max(r.r1),
        r.c0.max(r.c1),
    ))
}
