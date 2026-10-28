use std::collections::HashSet;
use std::sync::Arc;

use crate::address::{CellAddr, Rect, SheetId};
use crate::calc::{self, RecalcReport};
use crate::formula::{Formula, SheetRef};
use crate::formula_print::print_node;
use crate::sort::{move_ref, retarget};
use crate::structure::map_refs;
use crate::value::Value;
use crate::workbook::{CalcMode, EditError, Entry, Workbook};

#[derive(Clone, Debug, Default)]
pub struct DedupSpec {
    pub columns: Vec<u32>,

    pub header: bool,
}

#[derive(PartialEq, Eq, Hash)]
enum Key {
    Blank,
    Text(Vec<u32>),
    Num(u64, String),
    Bool(bool),
    Other(String),
}

const TOTAL_FUNCS: [&str; 8] = [
    "SUM",
    "AVERAGE",
    "COUNT",
    "COUNTA",
    "MAX",
    "MIN",
    "SUBTOTAL",
    "AGGREGATE",
];

fn has_total_call(text: &str) -> bool {
    let t = text.to_ascii_uppercase();
    let b: Vec<char> = t.chars().collect();
    let (mut i, mut quote) = (0, false);
    while i < b.len() {
        if b[i] == '"' {
            quote = !quote;
            i += 1;
            continue;
        }
        if !quote && (b[i].is_alphabetic() || b[i] == '_') {
            let s = i;
            while i < b.len() && (b[i].is_alphanumeric() || b[i] == '.' || b[i] == '_') {
                i += 1;
            }
            let name: String = b[s..i].iter().collect();
            let after_ref = s > 0 && matches!(b[s - 1], '!' | '[' | '$');
            if i < b.len() && b[i] == '(' && !after_ref && TOTAL_FUNCS.contains(&name.as_str()) {
                return true;
            }
            continue;
        }
        i += 1;
    }
    false
}

fn generic_error() -> EditError {
    EditError::Compile("Application-defined or object-defined error".into())
}

impl Workbook {
    fn dedup_key(&self, addr: CellAddr) -> Key {
        match self.value(addr) {
            Value::Blank => Key::Blank,
            Value::Text(s) => Key::Text(crate::functions::arrays::collate::key(&s)),
            Value::Number(n) => {
                let n = if n == 0.0 { 0.0 } else { n };
                Key::Num(n.to_bits(), self.display_text(addr))
            }
            Value::Bool(b) => Key::Bool(b),
            v => Key::Other(format!("{v:?}")),
        }
    }

    fn used_extent(&self, sheet: SheetId) -> Option<Rect> {
        let mut u = self.sheet(sheet)?.cells.sticky_bounds();
        for &(s, r, c) in self.cellfmt.map.keys() {
            if s == sheet {
                u = Some(match u {
                    Some(b) => Rect::new(b.r0.min(r), b.c0.min(c), b.r1.max(r), b.c1.max(c)),
                    None => Rect::new(r, c, r, c),
                });
            }
        }
        u
    }

    pub fn remove_duplicates(
        &mut self,
        sheet: SheetId,
        rect: Rect,
        spec: &DedupSpec,
    ) -> Result<RecalcReport, EditError> {
        self.sheet(sheet).ok_or(EditError::NoSuchSheet)?;
        crate::structure::range_too_large(&rect)?;
        if spec.columns.len() as u32 > rect.cols()
            || spec.columns.iter().any(|&c| c < 1 || c > rect.cols())
        {
            return Err(EditError::Compile(
                "Value does not fall within the expected range.".into(),
            ));
        }

        let mut table = None;
        let mut rect = rect;

        if rect.is_cell() {
            let active = Rect::new(0, 0, 0, 0);
            rect = match self
                .tables
                .iter()
                .find(|t| t.sheet == sheet && t.rect.intersects(&active))
            {
                Some(t) => t.rect,
                None => {
                    let reg = self.current_region(sheet, active);
                    let sh = self.sheet(sheet).unwrap();
                    let mut b: Option<Rect> = None;
                    for (r, c, _) in sh.cells.iter_rect(reg) {
                        let p = Rect::new(r, c, r, c);
                        b = Some(b.map_or(p, |b| b.union_bounds(&p)));
                    }
                    b.ok_or_else(generic_error)?
                }
            };
        }
        for (i, t) in self.tables.iter().enumerate() {
            if t.sheet == sheet && t.rect.intersects(&rect) {
                if !crate::cse::inside(&rect, &t.rect) {
                    return Err(generic_error());
                }
                table = Some(i);
                rect = t.rect;
                if t.totals {
                    rect.r1 -= 1;
                }
            }
        }
        let used = self.used_extent(sheet);
        let Some(eff) = used.and_then(|u| rect.intersection(&u)) else {
            return Err(generic_error());
        };
        let area = Rect::new(rect.r0, rect.c0, eff.r1, eff.c1.max(rect.c0));
        if self.filter_mode(sheet) {
            return Err(EditError::OutOfScope("RemoveDuplicates in filter mode"));
        }
        if self.merge_intersects(sheet, &area) {
            return Err(EditError::OutOfScope("RemoveDuplicates over merged cells"));
        }
        if !self
            .sheet(sheet)
            .unwrap()
            .spills
            .intersecting(&area)
            .is_empty()
        {
            return Err(crate::cse::part_of_array());
        }
        for (_, b) in self.array_blocks(sheet) {
            if b.intersects(&area) {
                return Err(crate::cse::part_of_array());
            }
        }
        if spec.columns.is_empty() {
            return Ok(RecalcReport::default());
        }
        let keys: Vec<u32> = spec.columns.iter().map(|&c| rect.c0 + c - 1).collect();
        if keys.iter().any(|&c| c > eff.c1) {
            return Err(generic_error());
        }
        let (c0, c1) = (rect.c0, eff.c1);
        let first = rect.r0 + u32::from(spec.header);
        let last = eff.r1;
        if first > last {
            return Ok(RecalcReport::default());
        }

        let sh = self.sheet(sheet).unwrap();
        let mut end = last;
        let total = (c0..=c1).any(|c| match sh.cells.get(last, c) {
            Some(Entry::Formula(fid)) => has_total_call(&self.formula(*fid).text),
            _ => false,
        });
        if total {
            end = last - 1;
        }
        if end < first {
            return Ok(RecalcReport::default());
        }

        if self
            .sheet(sheet)
            .unwrap()
            .cells
            .iter_rect(Rect::new(first, c0, end, c1))
            .next()
            .is_none()
        {
            return Ok(RecalcReport::default());
        }
        let rows: Vec<u32> = (first..=end).collect();
        let mut seen: HashSet<Vec<Key>> = HashSet::new();
        let mut kept: Vec<u32> = Vec::new();
        for &r in &rows {
            let k: Vec<Key> = keys
                .iter()
                .map(|&c| self.dedup_key(CellAddr::new(sheet, r, c)))
                .collect();
            if seen.insert(k) {
                kept.push(r);
            }
        }
        if kept.len() == rows.len() {
            return Ok(RecalcReport::default());
        }
        let removed = (rows.len() - kept.len()) as u32;

        let band = Rect::new(first, c0, end, c1);
        let entries: Vec<(u32, u32, Entry)> = self
            .sheet(sheet)
            .unwrap()
            .cells
            .iter_rect(band)
            .map(|(r, c, e)| (r, c, e.clone()))
            .collect();
        let by_pos: rustc_hash::FxHashMap<(u32, u32), usize> = entries
            .iter()
            .enumerate()
            .map(|(i, e)| ((e.0, e.1), i))
            .collect();
        #[allow(clippy::type_complexity)]
        let mut plan: Vec<(
            CellAddr,
            Result<Value, (Arc<Formula>, String, Value, Option<(u32, u32)>)>,
        )> = Vec::new();
        let mut fmt_pairs: Vec<(CellAddr, CellAddr)> = Vec::new();
        for (j, &src_row) in kept.iter().enumerate() {
            let dst_row = rows[j];
            if dst_row == src_row {
                continue;
            }
            for c in c0..=c1 {
                let to = CellAddr::new(sheet, dst_row, c);
                fmt_pairs.push((CellAddr::new(sheet, src_row, c), to));
                match by_pos.get(&(src_row, c)).map(|&i| &entries[i].2) {
                    None => plan.push((to, Ok(Value::Blank))),
                    Some(Entry::Const(v)) => plan.push((to, Ok(v.clone()))),
                    Some(Entry::Formula(fid)) => {
                        let f = self.formula(*fid);
                        let (dr, dc) = (
                            to.row as i64 - f.addr.row as i64,
                            to.col as i64 - f.addr.col as i64,
                        );
                        let root = map_refs(&f.formula.root, &mut |s, cref| {
                            if *s == SheetRef::Current {
                                Some(move_ref(cref, to))
                            } else {
                                Some(Some(retarget(cref, dr, dc)))
                            }
                        });
                        let text = print_node(self, &root, to);
                        plan.push((
                            to,
                            Err((
                                Arc::new(Formula {
                                    root,
                                    volatile: f.formula.volatile,
                                }),
                                text,
                                f.value_at(f.addr.row, f.addr.col),
                                f.array,
                            )),
                        ));
                    }
                }
            }
        }
        let vacated = &rows[kept.len()..];
        for &r in vacated {
            for c in c0..=c1 {
                plan.push((CellAddr::new(sheet, r, c), Ok(Value::Blank)));
            }
        }
        self.fmt_move_pairs(&fmt_pairs);
        for &r in vacated {
            for c in c0..=c1 {
                self.store_format(CellAddr::new(sheet, r, c), "General");
            }
        }

        let mut report = RecalcReport::default();
        let mode = self.settings.mode;
        self.settings.mode = CalcMode::Manual;
        for (to, what) in plan {
            match what {
                Ok(v) => {
                    self.set_value_raw(to, v);
                }
                Err((formula, text, shown, array)) => {
                    self.set_compiled_formula(to, formula, text);
                    if let Some(fid) = self.formula_at(to) {
                        self.formula_mut(fid).value = shown;
                        if array == Some((1, 1)) {
                            self.formula_mut(fid).array = array;
                        }
                    }
                }
            }
            report.changed.push(to);
        }

        let mut table_name = None;
        let mut refs_changed = false;
        if let Some(ti) = table {
            let name = self.tables[ti].name.clone();
            if self.tables[ti].totals {
                let t = &self.tables[ti];
                let shrunk = Rect::new(t.rect.r0, t.rect.c0, t.rect.r1 - 1 - removed, t.rect.c1);
                let _ = self.resize_table(&name, shrunk);
            } else {
                let old = self.tables[ti].rect;
                self.tables[ti].rect.r1 -= removed;
                refs_changed = self.shrink_exact_refs(ti, old, removed);
            }
            table_name = Some(name);
        }
        self.settings.mode = mode;
        if refs_changed {
            let r = self.relink_and_recalc();
            report.evaluations += r.evaluations;
            report.changed.extend(r.changed);
        }
        if let Some(name) = table_name {
            let r = self.table_changed(&name);
            report.evaluations += r.evaluations;
            report.changed.extend(r.changed);
        } else if mode == CalcMode::Automatic {
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
    fn total_calls() {
        assert!(has_total_call("=SUM(1)"));
        assert!(has_total_call("=1+sum(A1:A3)"));
        assert!(has_total_call("=IF(1,2,SUBTOTAL(9,A1))"));
        assert!(!has_total_call("=SUMIF(A1:A3,1)"));
        assert!(!has_total_call("=\"SUM(\"&A1"));
        assert!(!has_total_call("=MYSUM(1)"));
        assert!(!has_total_call("=ABS(A1)"));
    }
}
