use xlcalc::{CellAddr, Rect, SheetId, Workbook};

fn split_sheet<'a>(wb: &Workbook, text: &'a str, default: SheetId) -> Option<(SheetId, &'a str)> {
    match text.rfind('!') {
        None => Some((default, text)),
        Some(i) => {
            let name = text[..i].trim();
            let name = match name.strip_prefix('\'') {
                Some(q) => q.strip_suffix('\'')?.replace("''", "'"),
                None => name.to_string(),
            };
            Some((wb.sheet_by_name(&name)?.id, &text[i + 1..]))
        }
    }
}

fn r1c1_cell(text: &str) -> Option<(u32, u32)> {
    let t = text.trim().to_ascii_uppercase();
    let rest = t.strip_prefix('R')?;
    let (r, c) = rest.split_once('C')?;
    if r.is_empty() || c.is_empty() || !r.bytes().all(|b| b.is_ascii_digit()) {
        return None;
    }
    if !c.bytes().all(|b| b.is_ascii_digit()) {
        return None;
    }
    let (r, c): (u32, u32) = (r.parse().ok()?, c.parse().ok()?);
    if r == 0 || c == 0 || r > 1_048_576 || c > 16_384 {
        return None;
    }
    Some((r - 1, c - 1))
}

pub fn cell(wb: &Workbook, text: &str, default: SheetId) -> Option<CellAddr> {
    let (sheet, body) = split_sheet(wb, text, default)?;
    if let Some((r, c)) = r1c1_cell(body) {
        return Some(CellAddr::new(sheet, r, c));
    }
    let a = xlcalc::parse_cell_ref(wb, body, sheet)?;
    Some(a)
}

pub fn range(wb: &Workbook, text: &str, default: SheetId) -> Option<(SheetId, Rect)> {
    let (sheet, body) = split_sheet(wb, text, default)?;
    if let Some((a, b)) = body.split_once(':') {
        if let (Some((r0, c0)), Some((r1, c1))) = (r1c1_cell(a), r1c1_cell(b)) {
            return Some((
                sheet,
                Rect::new(r0.min(r1), c0.min(c1), r0.max(r1), c0.max(c1)),
            ));
        }
    } else if let Some((r, c)) = r1c1_cell(body) {
        return Some((sheet, Rect::new(r, c, r, c)));
    }
    xlcalc::parse_range_ref(wb, body, sheet)
}

pub fn split_head(line: &str) -> (&str, &str) {
    let mut end = None;
    if line.starts_with('\'') {
        let b = line.as_bytes();
        let mut i = 1;
        while i < b.len() {
            if b[i] == b'\'' {
                if b.get(i + 1) == Some(&b'\'') {
                    i += 2;
                    continue;
                }
                end = Some(i + 1);
                break;
            }
            i += 1;
        }
    }
    let from = end.unwrap_or(0);
    match line[from..].find(char::is_whitespace) {
        Some(i) => (&line[..from + i], line[from + i..].trim_start()),
        None => (line, ""),
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn r1c1_and_a1() {
        let mut wb = Workbook::new();
        let s = wb.sheet_at(0).id;
        let s2 = wb.add_sheet("My Sheet").unwrap();
        assert_eq!(cell(&wb, "B3", s), Some(CellAddr::new(s, 2, 1)));
        assert_eq!(cell(&wb, "R3C2", s), Some(CellAddr::new(s, 2, 1)));
        assert_eq!(
            cell(&wb, "'My Sheet'!r3c2", s),
            Some(CellAddr::new(s2, 2, 1))
        );
        assert_eq!(cell(&wb, "R0C1", s), None);
        assert_eq!(cell(&wb, "R1048577C1", s), None);
        assert_eq!(range(&wb, "R1C1:R2C3", s), Some((s, Rect::new(0, 0, 1, 2))));
        assert_eq!(range(&wb, "A1:B2", s), Some((s, Rect::new(0, 0, 1, 1))));
        assert!(range(&wb, "nonsense", s).is_none());
        assert!(cell(&wb, "'unterminated!A1", s).is_none());
    }

    #[test]
    fn heads() {
        assert_eq!(split_head("A1 5"), ("A1", "5"));
        assert_eq!(split_head("'My Sheet'!A1 =1+1"), ("'My Sheet'!A1", "=1+1"));
        assert_eq!(split_head("'it''s'!A1  x y"), ("'it''s'!A1", "x y"));
        assert_eq!(split_head("'open A1"), ("'open", "A1"));
        assert_eq!(split_head("calc"), ("calc", ""));
    }
}
