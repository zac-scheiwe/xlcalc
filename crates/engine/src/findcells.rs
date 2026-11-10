use crate::address::{CellAddr, Rect, SheetId, MAX_COLS, MAX_ROWS};
use crate::replace::text_matches;
use crate::value::Value;
use crate::workbook::store::Entry;
use crate::workbook::Workbook;

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum EndDir {
    Up,
    Down,
    Left,
    Right,
}

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum LookIn {
    Formulas,

    Values,

    Comments,
}

#[derive(Clone, Debug)]
pub struct FindSpec {
    pub what: String,
    pub look_in: LookIn,
    pub whole: bool,
    pub by_columns: bool,
    pub backward: bool,
    pub match_case: bool,
}

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum SpecialKind {
    Constants(u32),

    Formulas(u32),
    Blanks,
    LastCell,
    Visible,

    Comments,

    Differences { by_col: bool },
}

impl Workbook {
    fn has_content(&self, sheet: SheetId, r: u32, c: u32) -> bool {
        !self.value(CellAddr::new(sheet, r, c)).is_blank()
    }

    pub fn range_end(&self, sheet: SheetId, row: u32, col: u32, dir: EndDir) -> (u32, u32) {
        let step = |r: u32, c: u32| -> Option<(u32, u32)> {
            match dir {
                EndDir::Up => r.checked_sub(1).map(|r| (r, c)),
                EndDir::Down => (r + 1 < MAX_ROWS).then_some((r + 1, c)),
                EndDir::Left => c.checked_sub(1).map(|c| (r, c)),
                EndDir::Right => (c + 1 < MAX_COLS).then_some((r, c + 1)),
            }
        };
        let Some(mut cur) = step(row, col) else {
            return (row, col);
        };
        if self.has_content(sheet, row, col) && self.has_content(sheet, cur.0, cur.1) {
            while let Some(next) = step(cur.0, cur.1) {
                if !self.has_content(sheet, next.0, next.1) {
                    break;
                }
                cur = next;
            }
            return cur;
        }
        loop {
            if self.has_content(sheet, cur.0, cur.1) {
                return cur;
            }
            match step(cur.0, cur.1) {
                Some(next) => cur = next,
                None => return cur,
            }
        }
    }

    fn find_text(&self, addr: CellAddr, look_in: LookIn) -> String {
        match look_in {
            LookIn::Comments => self
                .note_at(addr.sheet, addr.row, addr.col)
                .map(|n| n.text.clone())
                .unwrap_or_default(),
            LookIn::Values => self.display_text(addr),
            LookIn::Formulas => match self
                .sheet(addr.sheet)
                .and_then(|s| s.cells.get(addr.row, addr.col))
            {
                Some(Entry::Const(v)) => self.const_edit_text(addr, v).unwrap_or_default(),
                Some(Entry::Formula(_)) => self.legacy_formula_text(addr).unwrap_or_default(),
                None => self.display_text(addr),
            },
        }
    }

    pub fn find_cell(
        &self,
        sheet: SheetId,
        areas: &[Rect],
        after: (u32, u32),
        spec: &FindSpec,
    ) -> Option<(u32, u32)> {
        let notes = spec.look_in == LookIn::Comments;
        let used = if notes {
            Rect::new(0, 0, MAX_ROWS - 1, MAX_COLS - 1)
        } else {
            self.used_range(sheet)?
        };
        let scope: Vec<Rect> = match areas {
            [one] if one.is_cell() => vec![used],
            _ => areas.iter().filter_map(|a| a.intersection(&used)).collect(),
        };

        let mut cells: Vec<(usize, u32, u32)> = Vec::new();
        for (i, a) in scope.iter().enumerate() {
            if notes {
                let mut hits: Vec<(u32, u32)> = self
                    .notes(sheet)
                    .map(|(k, _)| k)
                    .filter(|&(r, c)| a.contains(r, c))
                    .collect();
                if spec.by_columns {
                    hits.sort_by_key(|&(r, c)| (c, r));
                }
                cells.extend(hits.into_iter().map(|(r, c)| (i, r, c)));
            } else if spec.by_columns {
                for c in a.c0..=a.c1 {
                    cells.extend((a.r0..=a.r1).map(|r| (i, r, c)));
                }
            } else {
                for r in a.r0..=a.r1 {
                    cells.extend((a.c0..=a.c1).map(|c| (i, r, c)));
                }
            }
        }
        let n = cells.len();
        if n == 0 {
            return None;
        }

        let key = |r: u32, c: u32| if spec.by_columns { (c, r) } else { (r, c) };
        let area_of_after = scope.iter().position(|a| a.contains(after.0, after.1));
        let pos = match area_of_after {
            Some(ai) if notes => {
                let le =
                    |&(i, r, c): &(usize, u32, u32)| (i, key(r, c)) <= (ai, key(after.0, after.1));
                let upto = cells.iter().rposition(le);
                let on = upto.is_some_and(|p| (cells[p].1, cells[p].2) == after);
                match (upto, spec.backward && !on) {
                    (Some(p), true) => p + 1,
                    (Some(p), false) => p,
                    (None, true) => 0,
                    (None, false) => n - 1,
                }
            }
            Some(ai) => cells
                .iter()
                .position(|&(i, r, c)| i == ai && (r, c) == after)
                .unwrap_or(0),
            None => cells.partition_point(|&(_, r, c)| key(r, c) < key(after.0, after.1)),
        };
        let filter_mode = self.filter_mode(sheet);
        for k in 1..=n {
            let idx = if spec.backward {
                (pos + n - (k % n)) % n
            } else {
                (pos + k) % n
            };
            let (_, r, c) = cells[idx];

            if (spec.look_in == LookIn::Values || filter_mode)
                && (self.row_hidden(sheet, r) || self.col_hidden(sheet, c))
            {
                continue;
            }
            let text = self.find_text(CellAddr::new(sheet, r, c), spec.look_in);

            let hit = if spec.what.is_empty() || text.is_empty() {
                spec.what.is_empty() && text.is_empty()
            } else {
                text_matches(&text, &spec.what, spec.whole, spec.match_case)
            };
            if hit {
                return Some((r, c));
            }
        }
        None
    }

    pub fn special_cells(&self, sheet: SheetId, areas: &[Rect], kind: SpecialKind) -> Vec<Rect> {
        let used = self.used_range(sheet);
        if kind == SpecialKind::LastCell {
            return self
                .ever_used(sheet)
                .or(used)
                .map(|u| vec![Rect::cell(u.r1, u.c1)])
                .unwrap_or_else(|| vec![Rect::cell(0, 0)]);
        }
        let scope: Vec<Rect> = match areas {
            [one] if one.is_cell() && kind == SpecialKind::Comments => {
                vec![Rect::new(0, 0, MAX_ROWS - 1, MAX_COLS - 1)]
            }
            [one] if one.is_cell() => used.into_iter().collect(),
            _ => areas.to_vec(),
        };
        if kind == SpecialKind::Visible {
            return self.visible_pieces(sheet, &scope);
        }
        if let SpecialKind::Differences { by_col } = kind {
            return self.differences(sheet, areas, by_col);
        }
        let mut cells: Vec<Rect> = Vec::new();
        for a in &scope {
            match kind {
                SpecialKind::Blanks => {
                    let Some(a) = used.and_then(|u| a.intersection(&u)) else {
                        continue;
                    };
                    for r in a.r0..=a.r1 {
                        for c in a.c0..=a.c1 {
                            if !self.has_content(sheet, r, c) {
                                cells.push(Rect::cell(r, c));
                            }
                        }
                    }
                }
                SpecialKind::Constants(mask) | SpecialKind::Formulas(mask) => {
                    let Some(sh) = self.sheet(sheet) else {
                        continue;
                    };
                    let formulas = matches!(kind, SpecialKind::Formulas(_));
                    for (r, c, e) in sh.cells.iter_rect(*a) {
                        let is_formula = matches!(e, Entry::Formula(_));
                        if is_formula != formulas {
                            continue;
                        }
                        let v = self.value(CellAddr::new(sheet, r, c));
                        let bit = match v {
                            Value::Blank => continue,
                            Value::Number(_) => 1,
                            Value::Text(_) => 2,
                            Value::Bool(_) => 4,
                            Value::Error(_) => 16,
                            _ => 1,
                        };
                        if mask & bit != 0 {
                            cells.push(Rect::cell(r, c));
                        }
                    }
                }
                SpecialKind::Comments => {
                    cells.extend(
                        self.notes(sheet)
                            .filter(|((r, c), _)| a.contains(*r, *c))
                            .map(|((r, c), _)| Rect::cell(r, c)),
                    );
                }
                SpecialKind::Differences { .. } | SpecialKind::LastCell | SpecialKind::Visible => {}
            }
        }
        crate::trace::coalesce(cells, true)
    }

    fn same_content(&self, sheet: SheetId, a: (u32, u32), b: (u32, u32)) -> bool {
        let entry = |p: (u32, u32)| {
            self.sheet(sheet)
                .and_then(|sh| sh.cells.iter_rect(Rect::cell(p.0, p.1)).next().map(|x| x.2))
        };
        match (entry(a), entry(b)) {
            (Some(Entry::Formula(x)), Some(Entry::Formula(y))) => {
                let (fx, fy) = (self.formula(*x), self.formula(*y));
                crate::formula_print::print_node_r1c1(self, &fx.formula.root, fx.addr)
                    == crate::formula_print::print_node_r1c1(self, &fy.formula.root, fy.addr)
            }
            (Some(Entry::Formula(_)), Some(_)) | (Some(_), Some(Entry::Formula(_))) => false,
            _ => {
                let va = self.value(CellAddr::new(sheet, a.0, a.1));
                let vb = self.value(CellAddr::new(sheet, b.0, b.1));
                match (&va, &vb) {
                    (Value::Text(x), Value::Text(y)) => x.to_lowercase() == y.to_lowercase(),
                    (Value::Number(x), Value::Number(y)) => x == y,
                    (Value::Bool(x), Value::Bool(y)) => x == y,
                    (Value::Error(x), Value::Error(y)) => x == y,
                    (Value::Blank, Value::Blank) => true,
                    _ => false,
                }
            }
        }
    }

    fn differences(&self, sheet: SheetId, areas: &[Rect], by_col: bool) -> Vec<Rect> {
        let Some(first) = areas.first() else {
            return Vec::new();
        };
        if areas.len() == 1 && first.is_cell() {
            return Vec::new();
        }
        let Some(used) = self.used_range(sheet) else {
            return Vec::new();
        };
        let mut cells = Vec::new();
        for a in areas {
            let Some(a) = a.intersection(&used) else {
                continue;
            };
            for r in a.r0..=a.r1 {
                for c in a.c0..=a.c1 {
                    let reference = if by_col { (first.r0, c) } else { (r, first.c0) };
                    if (r, c) != reference && !self.same_content(sheet, (r, c), reference) {
                        cells.push(Rect::cell(r, c));
                    }
                }
            }
        }
        crate::trace::coalesce(cells, true)
    }

    fn visible_pieces(&self, sheet: SheetId, areas: &[Rect]) -> Vec<Rect> {
        let mut out = Vec::new();
        for a in areas {
            let runs = |lo: u32, hi: u32, hidden: &dyn Fn(u32) -> bool| {
                let mut v: Vec<(u32, u32)> = Vec::new();
                for i in lo..=hi {
                    if hidden(i) {
                        continue;
                    }
                    match v.last_mut() {
                        Some(l) if l.1 + 1 == i => l.1 = i,
                        _ => v.push((i, i)),
                    }
                }
                v
            };
            let rows = runs(a.r0, a.r1, &|r| self.row_hidden(sheet, r));
            let cols = runs(a.c0, a.c1, &|c| self.col_hidden(sheet, c));
            for &(r0, r1) in &rows {
                for &(c0, c1) in &cols {
                    out.push(Rect::new(r0, c0, r1, c1));
                }
            }
        }
        out
    }
}
