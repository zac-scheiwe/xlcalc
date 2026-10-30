use crate::address::{CellAddr, Rect, SheetId};
use crate::calc::{self, RecalcReport};
use crate::locale::DateOrder;
use crate::value::Value;
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

fn split_fixed(line: &str, breaks: &[u32]) -> Vec<String> {
    let ch: Vec<char> = line.chars().collect();
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
            ch[s..e.max(s)]
                .iter()
                .collect::<String>()
                .trim_matches(' ')
                .to_string(),
        );
    }
    out
}

fn ttc_date(t: &str, order: DateOrder, ours: DateOrder, year_now: i64) -> Option<String> {
    let mut parts: Vec<&str> = Vec::new();
    let s = t.trim_matches(' ');
    let b = s.as_bytes();
    let mut i = 0;
    while i < b.len() {
        let st = i;
        if b[i].is_ascii_digit() {
            while i < b.len() && b[i].is_ascii_digit() {
                i += 1;
            }
        } else if b[i].is_ascii_alphabetic() {
            while i < b.len() && b[i].is_ascii_alphabetic() {
                i += 1;
            }
        } else {
            return None;
        }
        parts.push(&s[st..i]);
        let sep = i;
        while i < b.len() && (b[i] == b' ' || b[i].is_ascii_punctuation()) {
            i += 1;
        }
        if i == sep && i < b.len() {
            return None;
        }
    }
    let num = |p: &str, max_len: usize| -> Option<i64> {
        (p.len() <= max_len && p.bytes().all(|c| c.is_ascii_digit())).then(|| p.parse().ok())?
    };
    let month = |p: &str, names: bool| -> Option<i64> {
        match num(p, 2) {
            Some(m) => (1..=12).contains(&m).then_some(m),
            None if names && p.bytes().all(|c| c.is_ascii_alphabetic()) => {
                crate::locale::month_from_name(p)
                    .filter(|&m| {
                        p.len() == 3 || p.len() == crate::locale::MONTH_NAMES[m as usize - 1].len()
                    })
                    .map(i64::from)
            }
            None => None,
        }
    };
    let year = |p: &str| -> Option<i64> {
        let y = num(p, 4)?;
        match p.len() {
            1 | 2 => Some(if y < 30 { 2000 + y } else { 1900 + y }),
            4 if y >= 1900 => Some(y),
            _ => None,
        }
    };
    let day_ok = |y: i64, m: i64, d: i64| {
        d >= 1
            && (d <= crate::datetime::days_in_month(y, m as u32) as i64
                || (y == 1900 && m == 2 && d == 29))
    };
    const MON: [&str; 12] = [
        "Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep", "Oct", "Nov", "Dec",
    ];

    let mut split: Vec<&str> = Vec::new();
    if parts.len() == 1 && parts[0].bytes().all(|c| c.is_ascii_digit()) {
        let p = parts[0];
        match (p.len(), order) {
            (4, _) => split.extend([&p[..2], &p[2..]]),
            (6, _) => split.extend([&p[..2], &p[2..4], &p[4..]]),
            (5, DateOrder::MDY) => split.extend([&p[..1], &p[1..3], &p[3..]]),
            (5, _) => split.extend([&p[..2], &p[2..3], &p[3..]]),
            (8, DateOrder::YMD) => split.extend([&p[..4], &p[4..6], &p[6..]]),
            _ => return None,
        }
        parts = split;
    }
    let named = parts.iter().any(|p| p.as_bytes()[0].is_ascii_alphabetic());
    let fmt3 = |d: i64, m: i64, y: i64| -> String {
        if named {
            format!("{}-{}-{}", d, MON[m as usize - 1], y)
        } else {
            match ours {
                DateOrder::DMY => format!("{}/{}/{}", d, m, y),
                DateOrder::MDY => format!("{}/{}/{}", m, d, y),
                DateOrder::YMD => format!("{}/{}/{}", y, m, d),
            }
        }
    };
    match parts.len() {
        3 => {
            let (dp, mp, yp) = match order {
                DateOrder::DMY => (parts[0], parts[1], parts[2]),
                DateOrder::MDY => (parts[1], parts[0], parts[2]),
                DateOrder::YMD => (parts[2], parts[1], parts[0]),
            };
            let (m, y, d) = (month(mp, true)?, year(yp)?, num(dp, 2)?);
            day_ok(y, m, d).then(|| fmt3(d, m, y))
        }
        2 => {
            let md = |mp: &str, dp: &str, names: bool| -> Option<String> {
                let (m, d) = (month(mp, names)?, num(dp, 2)?);
                day_ok(year_now, m, d).then(|| format!("{}-{}", d, MON[m as usize - 1]))
            };

            let my = |mp: &str, yp: &str| -> Option<String> {
                let (m, y) = (month(mp, true)?, year(yp)?);
                (num(yp, 4)? != 0).then(|| format!("{}-{}", MON[m as usize - 1], y))
            };

            let name_first = parts[0].starts_with(|c: char| c.is_ascii_alphabetic());
            let plain = !s.contains(|c: char| c.is_ascii_punctuation() && c != '-' && c != '/');
            let strict = plain && s.contains([' ', '-', '/']);
            let ym = || my(parts[1], parts[0]);
            let m_y = || my(parts[0], parts[1]);
            match order {
                DateOrder::DMY if named => {
                    if name_first {
                        strict.then(m_y)?
                    } else {
                        md(parts[1], parts[0], true)
                    }
                }
                DateOrder::DMY => md(parts[1], parts[0], true).or_else(m_y),

                DateOrder::MDY if named && !name_first => {
                    plain.then(|| md(parts[1], parts[0], true))?
                }
                DateOrder::MDY => md(parts[0], parts[1], true)
                    .or_else(|| {
                        let p = parts[1];
                        if !(3..=4).contains(&p.len()) || !p.bytes().all(|c| c.is_ascii_digit()) {
                            return None;
                        }
                        let (m, d, y) = (month(parts[0], true)?, num(&p[..2], 2)?, year(&p[2..])?);
                        day_ok(y, m, d).then(|| fmt3(d, m, y))
                    })
                    .or_else(|| {
                        if named {
                            strict.then(m_y)?
                        } else {
                            (parts[0].len() <= 2)
                                .then(ym)
                                .flatten()
                                .or_else(|| strict.then(m_y)?)
                        }
                    }),

                DateOrder::YMD => md(parts[0], parts[1], false)
                    .or_else(ym)
                    .or_else(|| strict.then(m_y)?),
            }
        }
        _ => None,
    }
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
        if dest.sheet != sheet {
            return Err(EditError::OutOfScope("TextToColumns into another sheet"));
        }
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
        if spec.fixed {
            let mut fi = spec.fieldinfo.clone();
            fi.sort_by_key(|p| p.0);

            let mut kept: Vec<(u32, u32)> = Vec::new();
            for p in fi {
                match kept.last_mut() {
                    Some(l) if l.0 == p.0 => *l = p,
                    _ => kept.push(p),
                }
            }
            fi = kept;
            if fi.is_empty() {
                return Err(EditError::OutOfScope(
                    "TextToColumns fixed width without break positions",
                ));
            }
            breaks = fi;
        }
        if !(rect.r0..=rect.r1).any(|r| {
            self.sheet(sheet)
                .is_some_and(|s| s.cells.get(r, rect.c0).is_some())
        }) {
            return Err(EditError::Compile("No data was selected to parse.".into()));
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
                    .map(|(j, s)| {
                        let ty = spec
                            .fieldinfo
                            .iter()
                            .find(|p| p.0 == j as u32 + 1)
                            .map_or(1, |p| p.1);
                        (s, ty)
                    })
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
        if self.filter_mode(sheet) || self.filter_mode(dest.sheet) {
            return Err(EditError::OutOfScope("TextToColumns in filter mode"));
        }
        if self.merge_intersects(sheet, &rect) || self.merge_intersects(dest.sheet, &area) {
            return Err(EditError::OutOfScope("TextToColumns over merged cells"));
        }
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
                        let order = match ty {
                            3 => Some(DateOrder::MDY),
                            4 => Some(DateOrder::DMY),
                            5 => Some(DateOrder::YMD),
                            _ => None,
                        };

                        if let Some(d) = order
                            .and_then(|o| ttc_date(&t, o, normal.date_order, wb.calc.current_year))
                        {
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
                        } else if t.starts_with('\'') {
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
        self.locale = normal;
        self.settings.mode = mode;
        res?;

        if self.tables.iter().any(|t| {
            t.sheet == dest.sheet
                && before_tables
                    .iter()
                    .any(|(n, r)| *n == t.name && *r != t.rect)
        }) {
            return Err(EditError::OutOfScope(
                "TextToColumns extending an adjacent table",
            ));
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

#[cfg(test)]
mod tests {
    use super::*;

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
    fn minus() {
        assert_eq!(trailing_minus("5-").as_deref(), Some("-5"));
        assert_eq!(trailing_minus("x-"), None);
    }
}
