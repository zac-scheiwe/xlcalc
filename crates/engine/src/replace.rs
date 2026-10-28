use crate::address::{CellAddr, Rect, SheetId};
use crate::calc::{self, RecalcReport};
use crate::value::Value;
use crate::workbook::Op;
use crate::workbook::{EditError, Entry, Workbook};

#[derive(Clone, Debug, Default)]
pub struct ReplaceSpec {
    pub what: String,

    pub with: String,

    pub whole: bool,

    pub match_case: bool,

    pub by_columns: bool,
}

#[derive(Clone, Copy, PartialEq, Debug)]
enum Pat {
    Lit(char),
    One,
    Star,
}

fn compile(what: &str) -> Vec<Pat> {
    let cs: Vec<char> = what.chars().collect();
    let mut out = Vec::new();
    let mut i = 0;
    while i < cs.len() {
        match cs[i] {
            '~' => {
                if i + 1 < cs.len() {
                    out.push(Pat::Lit(cs[i + 1]));
                }
                i += 1;
            }
            '*' => out.push(Pat::Star),
            '?' => out.push(Pat::One),
            c => out.push(Pat::Lit(c)),
        }
        i += 1;
    }
    out
}

fn same(a: char, b: char, match_case: bool) -> bool {
    a == b || (!match_case && a.to_lowercase().eq(b.to_lowercase()))
}

fn match_at(p: &[Pat], t: &[char], ti: usize, mc: bool, need_end: bool) -> Option<usize> {
    let Some((first, rest)) = p.split_first() else {
        return (!need_end || ti == t.len()).then_some(ti);
    };
    match *first {
        Pat::Lit(c) => (ti < t.len() && same(c, t[ti], mc))
            .then(|| match_at(rest, t, ti + 1, mc, need_end))
            .flatten(),
        Pat::One => (ti < t.len())
            .then(|| match_at(rest, t, ti + 1, mc, need_end))
            .flatten(),
        Pat::Star if rest.is_empty() => Some(t.len()),
        Pat::Star => (ti..=t.len()).find_map(|k| match_at(rest, t, k, mc, need_end)),
    }
}

pub(crate) fn replace_text(text: &str, spec: &ReplaceSpec) -> Option<String> {
    let p = compile(&spec.what);
    if p.is_empty() {
        return None;
    }
    let t: Vec<char> = text.chars().collect();
    if spec.whole {
        return match_at(&p, &t, 0, spec.match_case, true).map(|_| spec.with.clone());
    }
    let (mut out, mut i, mut hit) = (String::new(), 0, false);
    while i < t.len() {
        match match_at(&p, &t, i, spec.match_case, false) {
            Some(e) if e > i => {
                out.push_str(&spec.with);
                i = e;
                hit = true;
            }
            _ => {
                out.push(t[i]);
                i += 1;
            }
        }
    }
    hit.then_some(out)
}

impl Workbook {
    pub fn replace_range(
        &mut self,
        sheet: SheetId,
        rect: Rect,
        spec: &ReplaceSpec,
    ) -> Result<RecalcReport, EditError> {
        let sh = self.sheet(sheet).ok_or(EditError::NoSuchSheet)?;
        crate::structure::range_too_large(&rect)?;
        let scope = if rect.is_cell() {
            sh.cells.bounds().unwrap_or(rect)
        } else {
            rect
        };
        let mut cells: Vec<(u32, u32)> =
            sh.cells.iter_rect(scope).map(|(r, c, _)| (r, c)).collect();
        if spec.by_columns {
            cells.sort_by_key(|&(r, c)| (c, r));
            let start = cells.partition_point(|&(r, c)| (c, r) <= (rect.c0, rect.r0));
            cells.rotate_left(start);
        }
        let mut report = RecalcReport::default();
        self.with_op(Op::ReplaceEntry, true, |wb| {
            for (r, c) in cells {
                let addr = CellAddr::new(sheet, r, c);
                match wb.replace_cell(addr, spec) {
                    Ok(Some(rep)) => {
                        report.evaluations += rep.evaluations;
                        report.changed.extend(rep.changed);
                        if rep.circular.is_some() {
                            report.circular = rep.circular;
                        }
                    }
                    Ok(None) => {}
                    Err(()) => break,
                }
            }
        });
        Ok(calc::finish_report(report))
    }

    pub(crate) fn set_legacy_entry(
        &mut self,
        addr: CellAddr,
        text: &str,
    ) -> Result<RecalcReport, EditError> {
        if !text.starts_with('=') && crate::testing::is_formula_entry(self, text) {
            return Ok(self.set_value(addr, Value::from(text)));
        }
        self.set_legacy_input(addr, text)
    }

    fn replace_cell(
        &mut self,
        addr: CellAddr,
        spec: &ReplaceSpec,
    ) -> Result<Option<RecalcReport>, ()> {
        let Some(entry) = self
            .sheet(addr.sheet)
            .and_then(|s| s.cells.get(addr.row, addr.col))
        else {
            return Ok(None);
        };
        let (text, formula, array, prefixed) = match entry {
            Entry::Const(v) => {
                let Some(t) = self.const_edit_text(addr, v) else {
                    return Ok(None);
                };

                let prefixed = matches!(v, Value::Text(s) if !s.is_empty()
                    && !self.is_text_format(addr)
                    && self.apostrophe.get(&addr).is_some_and(|a| a == s));
                (t, false, None, prefixed)
            }
            Entry::Formula(fid) => {
                let Some(t) = self.legacy_formula_text(addr) else {
                    return Ok(None);
                };
                (t, true, self.formula(*fid).array, false)
            }
        };
        let Some(new) = replace_text(&text, spec) else {
            return Ok(None);
        };
        let r = if new.is_empty() {
            match array {
                Some((rows, cols)) => self.clear_range(
                    addr.sheet,
                    Rect::new(addr.row, addr.col, addr.row + rows - 1, addr.col + cols - 1),
                ),
                None => Ok(self.clear(addr)),
            }
        } else if formula {
            match array {
                Some((rows, cols)) if !new.starts_with('=') => {
                    let block =
                        Rect::new(addr.row, addr.col, addr.row + rows - 1, addr.col + cols - 1);
                    self.clear_range(addr.sheet, block).and_then(|mut rep| {
                        for r in block.r0..=block.r1 {
                            for c in block.c0..=block.c1 {
                                let x =
                                    self.set_legacy_entry(CellAddr::new(addr.sheet, r, c), &new)?;
                                rep.changed.extend(x.changed);
                                rep.evaluations += x.evaluations;
                            }
                        }
                        Ok(rep)
                    })
                }
                Some((rows, cols)) => self.set_array_formula(
                    addr.sheet,
                    Rect::new(addr.row, addr.col, addr.row + rows - 1, addr.col + cols - 1),
                    &new,
                ),
                None => self.set_legacy_entry(addr, &new),
            }
        } else if prefixed {
            self.set_text_prefixed(addr, &new)
        } else {
            self.set_legacy_entry(addr, &new)
        };
        match r {
            Ok(rep) => Ok(Some(rep)),
            Err(EditError::Parse(_)) | Err(EditError::Compile(_)) => Err(()),
            Err(_) => Ok(None),
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    fn rt(text: &str, what: &str, with: &str, whole: bool, mc: bool) -> Option<String> {
        replace_text(
            text,
            &ReplaceSpec {
                what: what.into(),
                with: with.into(),
                whole,
                match_case: mc,
                by_columns: false,
            },
        )
    }

    #[test]
    fn literal_and_case() {
        assert_eq!(
            rt("Banana", "an", "_", false, false).as_deref(),
            Some("B__a")
        );
        assert_eq!(rt("Banana", "AN", "_", false, true), None);
        assert_eq!(rt("abc", "ABC", "x", true, false).as_deref(), Some("x"));
        assert_eq!(rt("abcd", "abc", "x", true, false), None);
        assert_eq!(rt("abc", "", "x", false, false), None);
    }

    #[test]
    fn wildcards() {
        assert_eq!(rt("xabcx", "a*", "M", false, false).as_deref(), Some("xM"));
        assert_eq!(
            rt("abcabc", "a*c", "M", false, false).as_deref(),
            Some("MM")
        );
        assert_eq!(rt("a?b", "~?", "!", false, false).as_deref(), Some("a!b"));
        assert_eq!(rt("a*b", "a~*b", "!", true, false).as_deref(), Some("!"));
        assert_eq!(rt("ab", "a?", "M", true, false).as_deref(), Some("M"));
        assert_eq!(rt("abc", "~", "M", false, false), None);
    }
}
