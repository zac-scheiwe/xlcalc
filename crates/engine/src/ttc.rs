use crate::address::{CellAddr, Rect, SheetId};
use crate::calc::{self, RecalcReport};
use crate::locale::DateOrder;
use crate::value::Value;
use crate::workbook::Command;
use crate::workbook::Op;
use crate::workbook::{CalcMode, EditError, Entry, Workbook};

#[derive(Clone, Debug, Default)]
pub struct TtcSpec {
    pub fixed: bool,

    pub qualifier: Option<char>,

    pub consecutive: bool,
    pub tab: bool,
    pub semicolon: bool,
    pub comma: bool,
    pub space: bool,

    pub other: Option<char>,

    pub fieldinfo: Vec<(u32, u32)>,
    pub decimal: Option<char>,
    pub thousands: Option<char>,
    pub trailing_minus: bool,

    pub dest: Option<CellAddr>,

    pub refused: bool,
}

fn field_list(fieldinfo: &[(u32, u32)]) -> Vec<(u32, u32)> {
    let mut sorted = fieldinfo.to_vec();
    sorted.sort_by_key(|p| p.0);
    let mut kept: Vec<(u32, u32)> = Vec::new();
    for p in sorted {
        match kept.last_mut() {
            Some(l) if l.0 == p.0 => *l = p,
            _ => kept.push(p),
        }
    }
    kept
}

fn split_delimited(
    line: &str,
    delims: &[char],
    consecutive: bool,
    qual: Option<char>,
) -> Vec<String> {
    let ch: Vec<char> = line.chars().collect();
    let mut out = Vec::new();
    let mut cur = String::new();
    let mut i = 0;
    let mut start = true;
    while i < ch.len() {
        let c = ch[i];
        if start && Some(c) == qual {
            i += 1;
            while i < ch.len() {
                if Some(ch[i]) == qual {
                    if ch.get(i + 1).copied() == qual {
                        cur.push(ch[i]);
                        i += 2;
                        continue;
                    }
                    i += 1;
                    break;
                }
                cur.push(ch[i]);
                i += 1;
            }
            start = false;
            continue;
        }
        if delims.contains(&c) {
            out.push(std::mem::take(&mut cur));
            i += 1;
            if consecutive {
                while i < ch.len() && delims.contains(&ch[i]) {
                    i += 1;
                }
            }
            start = true;
            continue;
        }
        cur.push(c);
        start = false;
        i += 1;
    }
    out.push(cur);
    out
}

fn guess_breaks(lines: &[Vec<u16>]) -> Vec<u32> {
    let n = lines.len();
    let width = lines.iter().map(Vec::len).max().unwrap_or(0);
    let mut h = vec![0usize; width];
    for l in lines {
        for (i, &c) in l.iter().enumerate() {
            if c != 0x20 && c != 0x3000 {
                h[i] += 1;
            }
        }
    }
    let (eighth, half) = (n / 8, n / 2);

    let at = |i: usize| h.get(i).copied().unwrap_or(0);
    let mut out = vec![0u32];
    let mut i = 0;
    let mut prev = 0;
    while i < width {
        let c = h[i];
        if c > eighth || (prev != 0 && c == 0) {
            break;
        }
        prev = c;
        i += 1;
    }
    let (mut best_s, mut best_e) = (0usize, 0usize);
    while i < width {
        if h[i] <= eighth {
            let block = i;
            let mut low = usize::MAX;
            while i < width {
                let c = h[i];
                if c > eighth {
                    break;
                }
                let mut e = i;
                if c <= low {
                    while e < width && h[e] == c {
                        e += 1;
                    }
                    if e < width {
                        let next = h[e];
                        e -= 1;
                        if c <= next
                            && (c < low
                                || (at(block.wrapping_sub(1)) <= half
                                    && next != half
                                    && half <= next)
                                || best_e - best_s < e - i)
                        {
                            (best_s, best_e, low) = (i, e, c);
                        }
                    }
                }
                i = e + 1;
            }
            if i < width && low != usize::MAX {
                let mid = best_s + (best_e - best_s) / 2;
                let p = if half < at(best_s - 1) {
                    if half < at(best_e + 1) {
                        mid
                    } else {
                        best_s
                    }
                } else if at(best_e + 1) <= half {
                    mid
                } else {
                    best_e + 1
                };
                out.push(p as u32);
            }
        }
        i += 1;
    }
    out.sort_unstable();
    out.dedup();
    out
}

fn split_fixed(line: &str, breaks: &[u32]) -> Vec<String> {
    let ch: Vec<u16> = crate::text::wide::units(line).collect();
    let mut out = Vec::new();
    for (j, &s) in breaks.iter().enumerate() {
        let s = s as usize;
        if s >= ch.len() {
            break;
        }
        let e = breaks
            .get(j + 1)
            .map_or(ch.len(), |&e| (e as usize).min(ch.len()));
        out.push(
            crate::text::wide::from_units(&ch[s..e.max(s)])
                .trim_matches([' ', '\u{3000}'])
                .to_string(),
        );
    }
    out
}

fn ttc_slots(ty: u32) -> Option<[usize; 3]> {
    Some(match ty {
        3 => [1, 0, 2],
        4 => [0, 1, 2],
        5 => [2, 1, 0],
        6 => [2, 0, 1],
        7 => [0, 2, 1],
        8 => [1, 2, 0],
        _ => return None,
    })
}

fn ttc_date(t: &str, ty: u32, env: crate::coerce::ParseEnv) -> Option<String> {
    const DAY: usize = 0;
    const MONTH: usize = 1;
    const YEAR: usize = 2;
    let slots = ttc_slots(ty)?;
    let cs: Vec<char> = t.chars().collect();
    let len = cs.len();
    let blank = |c: char| c == ' ' || c == '\t';
    let alnum = |c: char| c.is_ascii_digit() || c.is_alphabetic();
    let mut pos = cs.iter().position(|&c| !blank(c)).unwrap_or(len);
    let run_end = cs[pos..]
        .iter()
        .position(|&c| !alnum(c))
        .map_or(len, |k| pos + k);
    let separated = cs[run_end..].iter().any(|&c| !blank(c));

    let width: [usize; 3] = if separated {
        [2, 9, 4]
    } else {
        match run_end - pos {
            4 | 6 => [2, 2, 2],
            5 => [2, 1, 2],
            7 => [2, 3, 2],
            8 => [2, 2, 4],
            9 => [2, 3, 4],
            _ => return None,
        }
    };
    let mut fields: [String; 3] = Default::default();
    for slot in 0..3 {
        let f = slots.iter().position(|&s| s == slot)?;
        let mut s = String::new();
        let mut n = 0;
        while n < width[f] && pos < len && alnum(cs[pos]) {
            s.push(cs[pos]);
            pos += 1;
            n += 1;
        }
        if n == 0 {
            if slot < 2 {
                return None;
            }
        } else if separated && (n > 4 && f != MONTH || n == 4 && f != YEAR || n == 3 && f != MONTH)
        {
            return None;
        }
        if separated {
            while pos < len && !alnum(cs[pos]) {
                pos += 1;
            }
        }
        fields[f] = s;
    }
    if cs[pos..].iter().any(|&c| !blank(c)) {
        return None;
    }
    let order = if fields[MONTH].starts_with(char::is_alphabetic) {
        [DAY, MONTH, YEAR]
    } else {
        match env.locale.date_order {
            DateOrder::MDY => [MONTH, DAY, YEAR],
            DateOrder::DMY => [DAY, MONTH, YEAR],
            DateOrder::YMD => [YEAR, MONTH, DAY],
        }
    };
    let sep = env.locale.date_sep.to_string();
    let text = order
        .iter()
        .map(|&f| fields[f].as_str())
        .filter(|s| !s.is_empty())
        .collect::<Vec<_>>()
        .join(&sep);
    crate::coerce::parse_datetime_text(&text, env).map(|_| text)
}

fn trailing_minus(t: &str) -> Option<String> {
    let s = t.trim();
    let body = s.strip_suffix('-')?.trim_end();
    (!body.is_empty()
        && body
            .chars()
            .all(|c| c.is_ascii_digit() || c == '.' || c == ','))
    .then(|| format!("-{body}"))
}

impl Workbook {
    pub fn text_to_columns(
        &mut self,
        sheet: SheetId,
        rect: Rect,
        spec: &TtcSpec,
    ) -> Result<RecalcReport, EditError> {
        self.command(Command::TextToColumns, |wb| {
            wb.text_to_columns_run(sheet, rect, spec)
        })
    }

    fn text_to_columns_run(
        &mut self,
        sheet: SheetId,
        rect: Rect,
        spec: &TtcSpec,
    ) -> Result<RecalcReport, EditError> {
        self.sheet(sheet).ok_or(EditError::NoSuchSheet)?;
        crate::structure::range_too_large(&rect)?;
        if rect.cols() != 1 {
            return Err(EditError::Compile(
                "Microsoft Excel can convert only one column at a time. The range can be many rows tall but no more than one column wide. Try again by selecting cells in one column only."
                    .into(),
            ));
        }
        let mut rect = rect;
        let Some(b) = self.sheet(sheet).unwrap().cells.bounds() else {
            return Ok(RecalcReport::default());
        };
        rect.r1 = rect.r1.min(b.r1.max(rect.r0));
        let dest = spec
            .dest
            .filter(|d| !(d.sheet == sheet && d.row == rect.r0 && d.col == rect.c0))
            .unwrap_or(CellAddr::new(sheet, rect.r0, rect.c0));

        let dest = CellAddr::new(sheet, dest.row, dest.col);
        if dest.sheet == sheet
            && rect.contains(dest.row, dest.col)
            && (dest.row, dest.col) != (rect.r0, rect.c0)
        {
            return Err(EditError::Compile(
                "Destination reference is not valid.".into(),
            ));
        }

        let mut delims: Vec<char> = Vec::new();
        for (on, c) in [
            (spec.tab, '\t'),
            (spec.semicolon, ';'),
            (spec.comma, ','),
            (spec.space, ' '),
        ] {
            if on {
                delims.push(c);
            }
        }
        if let Some(c) = spec.other {
            delims.push(c);
        }
        let mut breaks: Vec<(u32, u32)> = Vec::new();
        let mut fi = field_list(&spec.fieldinfo);
        if spec.fixed {
            if fi.is_empty() {
                let first = self
                    .cell_table_extent(sheet)
                    .map_or(rect.r0, |e| e.r0.max(rect.r0));
                let last = rect.r1.min(first.saturating_add(250));
                let texts: Vec<Vec<u16>> = (first..=last)
                    .map(|r| {
                        let addr = CellAddr::new(sheet, r, rect.c0);
                        let t = match self.sheet(sheet).and_then(|s| s.cells.get(r, rect.c0)) {
                            Some(Entry::Const(Value::Text(t))) => t.to_string(),
                            Some(Entry::Const(v)) => {
                                self.const_edit_text(addr, v).unwrap_or_default()
                            }
                            Some(Entry::Formula(_)) => {
                                self.legacy_formula_text(addr).unwrap_or_default()
                            }
                            None => String::new(),
                        };
                        crate::text::wide::units(&t).collect()
                    })
                    .collect();
                fi = guess_breaks(&texts).into_iter().map(|p| (p, 1)).collect();
            }
            breaks = std::mem::take(&mut fi);
        }
        if !(rect.r0..=rect.r1).any(|r| {
            self.sheet(sheet)
                .is_some_and(|s| s.cells.get(r, rect.c0).is_some())
        }) {
            return Err(EditError::Compile("No data was selected to parse.".into()));
        }
        if spec.refused {
            return Ok(RecalcReport::default());
        }
        let mut rows: Vec<Vec<(String, u32)>> = Vec::new();
        for r in rect.r0..=rect.r1 {
            let addr = CellAddr::new(sheet, r, rect.c0);
            let text = match self.sheet(sheet).and_then(|s| s.cells.get(r, rect.c0)) {
                Some(Entry::Const(Value::Text(t))) => t.to_string(),
                Some(Entry::Const(v)) => self.const_edit_text(addr, v).unwrap_or_default(),
                Some(Entry::Formula(_)) => self.legacy_formula_text(addr).unwrap_or_default(),
                None => String::new(),
            };
            let pieces: Vec<(String, u32)> = if spec.fixed {
                let starts: Vec<u32> = breaks.iter().map(|p| p.0).collect();
                split_fixed(&text, &starts)
                    .into_iter()
                    .enumerate()
                    .map(|(j, s)| (s, breaks[j].1))
                    .collect()
            } else {
                split_delimited(&text, &delims, spec.consecutive, spec.qualifier)
                    .into_iter()
                    .enumerate()
                    .map(|(j, s)| (s, fi.get(j).map_or(1, |p| p.1)))
                    .collect()
            };
            rows.push(pieces.into_iter().filter(|p| p.1 != 9).collect());
        }

        if rows.iter().all(|r| r.is_empty()) && dest.col != 0 {
            return Err(EditError::Compile(
                "Unable to get the TextToColumns property of the Range class".into(),
            ));
        }
        let width = rows
            .iter()
            .map(|r| r.len() as u32)
            .max()
            .unwrap_or(0)
            .max(1);
        if dest.col + width > crate::address::MAX_COLS
            || dest.row + rows.len() as u32 > crate::address::MAX_ROWS
        {
            return Err(EditError::Compile(
                "Application-defined or object-defined error".into(),
            ));
        }
        let area = Rect::new(
            dest.row,
            dest.col,
            dest.row + rows.len() as u32 - 1,
            dest.col + width - 1,
        );
        for (s, a) in [(sheet, rect), (dest.sheet, area)] {
            if !self.sheet(s).unwrap().spills.intersecting(&a).is_empty() {
                return Err(crate::cse::part_of_array());
            }
            for (_, blk) in self.array_blocks(s) {
                if blk.intersects(&a) {
                    return Err(crate::cse::part_of_array());
                }
            }
        }

        for (s, a) in [(sheet, rect), (dest.sheet, area)] {
            if self
                .data_tables
                .iter()
                .any(|t| t.sheet == s && t.interior().intersects(&a))
            {
                return Err(EditError::Compile(crate::datatable::PART_ERROR.into()));
            }
        }

        if self
            .merges(dest.sheet)
            .iter()
            .any(|m| m.intersects(&area) && !crate::cse::inside(m, &area))
        {
            return Err(EditError::Compile(
                "We can't do that to a merged cell.".into(),
            ));
        }

        let before_tables: Vec<(String, Rect)> = self
            .tables
            .iter()
            .filter(|t| t.sheet == dest.sheet)
            .map(|t| (t.name.clone(), t.rect))
            .collect();
        let mut report = RecalcReport::default();
        let mode = self.settings.mode;
        self.settings.mode = CalcMode::Manual;
        let normal = self.locale.clone();
        let mut res = Ok(());

        let held: Vec<Rect> = self
            .merges(dest.sheet)
            .into_iter()
            .filter(|m| m.intersects(&area))
            .collect();
        self.suspend_merges(dest.sheet, &held, true);
        let pieces: Vec<(u32, u32)> = rows
            .iter()
            .enumerate()
            .flat_map(|(i, row)| {
                row.iter()
                    .enumerate()
                    .filter(|(_, p)| !p.0.is_empty())
                    .map(move |(j, _)| (dest.row + i as u32, dest.col + j as u32))
            })
            .collect();
        self.ttc_block = Some((dest.sheet, area, pieces));
        self.with_op(Op::ReplaceEntry, true, |wb| {
            wb.with_op(Op::TextToColumns, true, |wb| {
                'rows: for (i, row) in rows.iter().enumerate() {
                    for j in 0..width as usize {
                        let addr =
                            CellAddr::new(dest.sheet, dest.row + i as u32, dest.col + j as u32);
                        let Some((text, ty)) = row.get(j) else {
                            wb.clear(addr);
                            report.changed.push(addr);
                            continue;
                        };
                        let mut loc = normal.clone();
                        if let Some(c) = spec.decimal {
                            loc.decimal_sep = c;
                        }
                        if let Some(c) = spec.thousands {
                            loc.thousands_sep = c;
                        }
                        let mut t = text.clone();
                        if spec.trailing_minus {
                            if let Some(n) = trailing_minus(&t) {
                                t = n;
                            }
                        }

                        let env = crate::coerce::ParseEnv {
                            locale: &normal,
                            date_system: wb.settings.date_system,
                            current_year: wb.calc.current_year,
                            iso_t: false,
                        };
                        if let Some(d) = ttc_date(&t, *ty, env) {
                            t = d;
                        }
                        if t.is_empty() {
                            wb.clear(addr);
                            report.changed.push(addr);
                            continue;
                        }
                        wb.locale = loc;
                        let r = if *ty == 2 {
                            let r = Ok(wb.set_value(addr, Value::from(t.as_str())));
                            wb.store_format(addr, "@");
                            r
                        } else if t.starts_with('\'') || has_r1c1_alias(&t) {
                            Ok(wb.set_value(addr, Value::from(t.as_str())))
                        } else {
                            match wb.set_input(addr, &t) {
                                Err(EditError::Parse(_)) | Err(EditError::Compile(_)) => {
                                    Ok(wb.set_value(addr, Value::from(t.as_str())))
                                }
                                r => r,
                            }
                        };
                        wb.locale = normal.clone();
                        match r {
                            Ok(rep) => {
                                report.changed.extend(rep.changed);
                                report.changed.push(addr);
                            }
                            Err(e) => {
                                res = Err(e);
                                break 'rows;
                            }
                        }
                    }
                }
            })
        });
        self.suspend_merges(dest.sheet, &held, false);
        self.locale = normal;
        self.settings.mode = mode;
        self.ttc_block = None;
        res?;

        let grown: Vec<(usize, u32)> = self
            .tables
            .iter()
            .enumerate()
            .filter(|(_, t)| t.sheet == dest.sheet)
            .filter_map(|(ti, t)| {
                let before = before_tables.iter().find(|(n, _)| *n == t.name)?.1;
                Some((ti, before.c1 + 1..=t.rect.c1))
            })
            .flat_map(|(ti, cols)| cols.map(move |c| (ti, c)))
            .collect();
        for (ti, c) in grown {
            let (d0, _) = self.tables[ti].data_rows();
            let a = CellAddr::new(dest.sheet, d0, c);
            if self.formula_at(a).is_some() {
                self.calc_fill_column(ti, a);
            }
        }

        while let Some(ti) = self
            .tables
            .iter()
            .position(|t| t.sheet == dest.sheet && t.columns.iter().any(|c| c.is_empty()))
        {
            self.drop_table_keep_cells(ti);
        }
        if mode == CalcMode::Automatic {
            let r = calc::recalc(self);
            report.evaluations += r.evaluations;
            report.circular = r.circular;
            report.changed.extend(r.changed);
        }
        Ok(calc::finish_report(report))
    }
}

fn has_r1c1_alias(t: &str) -> bool {
    fn part(b: &[u8], i: &mut usize, tag: u8) -> bool {
        if b.get(*i).map(u8::to_ascii_lowercase) != Some(tag) {
            return false;
        }
        *i += 1;
        if b.get(*i) == Some(&b'[') {
            let mut j = *i + 1;
            if b.get(j) == Some(&b'-') {
                j += 1;
            }
            let d = j;
            while b.get(j).is_some_and(u8::is_ascii_digit) {
                j += 1;
            }
            if j > d && b.get(j) == Some(&b']') {
                *i = j + 1;
            }
        } else {
            while b.get(*i).is_some_and(u8::is_ascii_digit) {
                *i += 1;
            }
        }
        true
    }
    let word = |x: u8| x.is_ascii_alphanumeric() || matches!(x, b'_' | b'.');
    let b = t.as_bytes();
    let mut s = 0;
    while s < b.len() {
        if !word(b[s]) {
            s += 1;
            continue;
        }
        let word_start = s == 0 || !(word(b[s - 1]) || b[s - 1] == b'$');
        let mut i = s;
        let r = part(b, &mut i, b'r');
        let c = part(b, &mut i, b'c');
        let ended = b.get(i).is_none_or(|x| !(word(*x) || *x == b'('));
        let a1 = {
            let w = &b[s..i];
            let letters = w.iter().take_while(|x| x.is_ascii_alphabetic()).count();
            (1..=3).contains(&letters)
                && letters < w.len()
                && w[letters..].iter().all(u8::is_ascii_digit)
        };
        if word_start && (r || c) && ended && !a1 {
            return true;
        }
        while s < b.len() && word(b[s]) {
            s += 1;
        }
    }
    false
}

#[cfg(test)]
mod tests {
    use super::*;

    fn u(s: &str) -> Vec<u16> {
        crate::text::wide::units(s).collect()
    }

    #[test]
    fn guess_breaks_cases() {
        assert_eq!(guess_breaks(&[u("abc def"), u("ghi jkl")]), [0, 3]);
        assert_eq!(guess_breaks(&[u("abcdef"), u("ghijkl")]), [0]);
        assert_eq!(guess_breaks(&[]), [0]);
        let mut l: Vec<Vec<u16>> = vec![u("ab  cd"); 16];
        l[0] = u("abXXcd");
        l[1] = u("abXXcd");
        assert_eq!(guess_breaks(&l), [0, 2]);
        l[2] = u("abXXcd");
        assert_eq!(guess_breaks(&l), [0]);
    }

    #[test]
    fn delimited() {
        let d = [','];
        assert_eq!(
            split_delimited("a,b,,c", &d, false, Some('"')),
            ["a", "b", "", "c"]
        );
        assert_eq!(split_delimited("a,,b", &d, true, Some('"')), ["a", "b"]);
        assert_eq!(
            split_delimited("\"a,b\",c", &d, false, Some('"')),
            ["a,b", "c"]
        );
        assert_eq!(
            split_delimited("\"x\"\"y\",z", &d, false, Some('"')),
            ["x\"y", "z"]
        );
        assert_eq!(split_delimited("a,\"b", &d, false, Some('"')), ["a", "b"]);
        assert_eq!(
            split_delimited("\"a,b\",c", &d, false, None),
            ["\"a", "b\"", "c"]
        );
    }

    #[test]
    fn fixed() {
        assert_eq!(split_fixed("abc  123 xy", &[0, 3, 8]), ["abc", "123", "xy"]);
        assert_eq!(split_fixed("abcdef", &[0, 10]), ["abcdef"]);
        assert_eq!(split_fixed("abcdefgh", &[0, 3, 5]), ["abc", "de", "fgh"]);
    }

    #[test]
    fn date_fields() {
        let nz = crate::locale::Locale::en_nz();
        let us = crate::locale::Locale::en_us();
        let env = |l| crate::coerce::ParseEnv {
            locale: l,
            date_system: crate::datetime::DateSystem::D1900,
            current_year: 2024,
            iso_t: false,
        };
        let d = |t: &str, ty: u32| ttc_date(t, ty, env(&nz));

        assert_eq!(d("1,234", 3).as_deref(), Some("23/1/4"));
        assert_eq!(d("12;30", 4).as_deref(), Some("12/30"));
        assert_eq!(d("2004.12.4", 5).as_deref(), Some("4/12/2004"));
        assert_eq!(d("12/4/04", 6).as_deref(), Some("04/12/4"));

        assert_eq!(d("Jan 2020", 3).as_deref(), Some("20/Jan/20"));
        assert_eq!(d("June 5", 3), None);
        assert_eq!(d("Jan;5", 5), None);

        assert_eq!(d("-12-4", 4), None);
        assert_eq!(d("1/2/123", 4), None);
        assert_eq!(d("12;4;2004 5:30", 4), None);

        assert_eq!(d("1230", 4).as_deref(), Some("12/30"));
        assert_eq!(d("00123", 5).as_deref(), Some("23/1/00"));
        assert_eq!(d("20201231", 5).as_deref(), Some("31/12/2020"));
        assert_eq!(d("123", 3), None);
        assert_eq!(ttc_date("1,234", 3, env(&us)).as_deref(), Some("1/23/4"));
        assert_eq!(d("5", 1), None);
    }

    #[test]
    fn minus() {
        assert_eq!(trailing_minus("5-").as_deref(), Some("-5"));
        assert_eq!(trailing_minus("x-"), None);
    }

    #[test]
    fn field_list_orders_and_replaces() {
        assert_eq!(field_list(&[(2, 2), (0, 9), (2, 5)]), vec![(0, 9), (2, 5)]);
    }
}
