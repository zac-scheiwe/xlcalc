use std::collections::BTreeMap;

use super::{FormulaId, Workbook};

#[derive(Default)]
pub(crate) struct Creation {
    keys: Vec<i64>,

    resolved: usize,

    order: Vec<FormulaId>,
}

const GAP: i64 = 1 << 24;

impl Workbook {
    fn chain_holds(&self, key: i64, fid: FormulaId) -> bool {
        self.formulas
            .get(fid as usize)
            .and_then(|f| f.as_ref())
            .is_some_and(|f| f.seq == key)
    }

    fn chain_next(&mut self, key: i64) -> Option<i64> {
        loop {
            let (&k, &g) = self.calc.chain.range(key + 1..).next()?;
            if self.chain_holds(k, g) {
                return Some(k);
            }
            self.calc.chain.remove(&k);
        }
    }

    pub(crate) fn chain_top(&mut self) -> i64 {
        let key = self.calc.next_new_seq;
        self.calc.next_new_seq -= GAP;
        if let Some(scope) = &mut self.calc.creation {
            scope.keys.push(key);
        }
        key
    }

    pub fn creating_formulas<R>(&mut self, f: impl FnOnce(&mut Workbook) -> R) -> R {
        if self.calc.creation.is_some() || self.passes.notifying {
            return f(self);
        }
        self.calc.creation = Some(Creation::default());
        let r = f(self);
        self.order_creation();
        self.calc.creation = None;
        r
    }

    pub(crate) fn outside_creation<R>(&mut self, f: impl FnOnce(&mut Workbook) -> R) -> R {
        if self.calc.creation.is_none() {
            return f(self);
        }
        self.order_creation();
        let scope = self.calc.creation.take();
        let r = f(self);
        self.calc.creation = scope;
        r
    }

    pub(crate) fn flush_creation(&mut self) {
        self.order_creation();
    }

    fn order_creation(&mut self) {
        let Some(mut scope) = self.calc.creation.take() else {
            return;
        };

        for &k in &scope.keys[scope.resolved..] {
            if let Some(&g) = self.calc.chain.get(&k) {
                if self.chain_holds(k, g) {
                    scope.order.retain(|&o| o != g);
                    scope.order.push(g);
                }
            }
        }
        scope.resolved = scope.keys.len();
        let pool: rustc_hash::FxHashSet<i64> = scope.keys.iter().copied().collect();
        let fids: Vec<FormulaId> = scope
            .order
            .iter()
            .copied()
            .filter(|&g| {
                self.formulas
                    .get(g as usize)
                    .and_then(|f| f.as_ref())
                    .is_some_and(|f| pool.contains(&f.seq) && self.chain_holds(f.seq, g))
            })
            .collect();
        scope.order.clone_from(&fids);
        self.calc.creation = Some(scope);
        if fids.len() < 2 {
            return;
        }
        let mut keys: Vec<i64> = fids.iter().map(|&g| self.formula(g).seq).collect();
        keys.sort_unstable();
        let mut moved = false;
        for (&k, &g) in keys.iter().zip(&fids) {
            if self.formula(g).seq != k {
                self.calc.chain.insert(k, g);
                self.formula_mut(g).seq = k;
                moved = true;
            }
        }
        if moved {
            self.calc.walk.rekey(&fids);
        }
    }

    pub(crate) fn chain_set_seq(&mut self, fid: FormulaId, seq: i64) {
        let cur = self.formula(fid).seq;
        if cur == seq {
            return;
        }
        self.calc.chain.remove(&cur);
        self.calc.chain.insert(seq, fid);
        self.formula_mut(fid).seq = seq;
        self.calc.chain_epoch += 1;
    }

    pub(crate) fn chain_to_top(&mut self, fid: FormulaId) {
        let old = self.formula(fid).seq;
        self.calc.chain.remove(&old);
        let key = self.chain_top();
        self.formula_mut(fid).seq = key;
        self.calc.chain.insert(key, fid);
    }

    pub(crate) fn chain_insert(&mut self, fid: FormulaId) {
        let key = self.formula(fid).seq;
        self.calc.chain.insert(key, fid);
    }

    pub(crate) fn chain_before(&mut self, prec: FormulaId, fid: FormulaId) {
        if fid == prec || !self.live_formula(prec) {
            return;
        }
        let own = self.formula(fid).seq;
        let old = self.formula(prec).seq;
        if old < own {
            return;
        }
        let prev = loop {
            match self
                .calc
                .chain
                .range(..own)
                .next_back()
                .map(|(&k, &g)| (k, g))
            {
                None => break None,
                Some((k, g)) if self.chain_holds(k, g) => break Some(k),
                Some((k, _)) => {
                    self.calc.chain.remove(&k);
                }
            }
        };
        let key = match prev {
            Some(p) if own - p >= 2 => p + (own - p) / 2,
            Some(p) => {
                self.chain_spread(p);
                let o = self.formula(fid).seq;
                p + (o - p) / 2
            }
            None => own - GAP,
        };
        self.calc.chain.remove(&old);
        self.formula_mut(prec).seq = key;
        self.calc.chain.insert(key, prec);
        if key <= self.calc.next_new_seq {
            self.calc.next_new_seq = key - GAP;
        }
    }

    pub(crate) fn chain_after(&mut self, fid: FormulaId, prec: FormulaId) {
        if fid == prec || !self.live_formula(prec) {
            return;
        }
        let p = self.formula(prec).seq;
        let own = self.formula(fid).seq;
        let next = self.chain_next(p);
        if next == Some(own) {
            return;
        }
        let key = match next {
            None => p + GAP,
            Some(n) if n - p >= 2 => p + (n - p) / 2,
            Some(_) => {
                self.chain_spread(p);
                let n = self.chain_next(p).unwrap_or(p + 2 * GAP);
                p + (n - p) / 2
            }
        };
        self.calc.chain.remove(&own);
        self.formula_mut(fid).seq = key;
        self.calc.chain.insert(key, fid);
    }

    fn chain_spread(&mut self, p: i64) {
        let mut n = 2usize;
        loop {
            let mut run: Vec<(i64, FormulaId)> = Vec::with_capacity(n);
            let mut stale = Vec::new();
            for (&k, &g) in self.calc.chain.range(p..) {
                if run.len() == n {
                    break;
                }
                if self.chain_holds(k, g) {
                    run.push((k, g));
                } else {
                    stale.push(k);
                }
            }
            for k in stale {
                self.calc.chain.remove(&k);
            }
            let last = run.last().map_or(p, |r| r.0);
            let end = match self.chain_next(last) {
                Some(e) => e,
                None => last + GAP * (run.len() as i64 + 1),
            };
            let len = run.len() as i64 + 1;
            if run.len() < n || end - p > len * len * 4 {
                let step = ((end - p) / len).max(1);
                for (k, _) in &run {
                    self.calc.chain.remove(k);
                }
                for (i, &(_, g)) in run.iter().enumerate() {
                    let key = p + i as i64 * step;
                    self.formula_mut(g).seq = key;
                    self.calc.chain.insert(key, g);
                }
                return;
            }
            n *= 2;
        }
    }

    pub(crate) fn chain_reset(&mut self, ids: &[FormulaId]) {
        self.calc.chain = BTreeMap::new();
        self.calc.chain_epoch += 1;
        for (rank, &fid) in ids.iter().enumerate() {
            let key = (rank as i64 + 1) * GAP;
            self.formula_mut(fid).seq = key;
            self.calc.chain.insert(key, fid);
        }
        self.calc.next_new_seq = -GAP;
    }

    fn live_formula(&self, fid: FormulaId) -> bool {
        self.formulas.get(fid as usize).is_some_and(|f| f.is_some())
    }
}

impl Workbook {
    pub fn debug_chain(&self) -> String {
        let mut out = Vec::new();
        for (&k, &g) in &self.calc.chain {
            if !self.chain_holds(k, g) {
                continue;
            }
            let f = self.formula(g);
            let a = f.addr;
            match f.array {
                Some((rows, cols)) => {
                    out.push(format!("{}{{}}", a.a1()));
                    if rows * cols > 1 {
                        for r in 0..rows {
                            for c in 0..cols {
                                out.push(crate::CellAddr::new(a.sheet, a.row + r, a.col + c).a1());
                            }
                        }
                    }
                }
                None if f.spill.is_some() || matches!(f.value, crate::Value::Array(_)) => {
                    out.push(format!("{}{{}}", a.a1()))
                }
                None => out.push(a.a1()),
            }
        }
        out.join(" ")
    }

    pub(crate) fn chain_reenter(&mut self, ids: &[FormulaId]) {
        for &fid in ids {
            let old = self.formula(fid).seq;
            self.calc.chain.remove(&old);
            let key = self.chain_top();
            self.formula_mut(fid).seq = key;
            self.calc.chain.insert(key, fid);
            self.calc.walk.raise(fid);
        }
    }
}

#[cfg(test)]
mod tests {
    use crate::address::{CellAddr, Rect};
    use crate::Workbook;

    #[test]
    fn a_creation_scope_enters_its_block_in_order() {
        let mut wb = Workbook::new();
        let s = wb.sheet_at(0).id;
        let a = |r, c| CellAddr::new(s, r, c);
        wb.set_input(a(0, 0), "=1").unwrap();
        wb.set_input(a(1, 0), "=2").unwrap();
        assert_eq!(wb.debug_chain(), "A2 A1");
        wb.creating_formulas(|wb| {
            wb.set_input(a(0, 1), "=3").unwrap();
            wb.creating_formulas(|wb| wb.set_input(a(1, 1), "=4").unwrap());
            wb.set_input(a(2, 1), "=5").unwrap();
        });
        assert_eq!(wb.debug_chain(), "B1 B2 B3 A2 A1");

        wb.creating_formulas(|wb| {
            wb.set_input(a(0, 0), "=6").unwrap();
            wb.set_input(a(0, 2), "=7").unwrap();
        });
        assert_eq!(wb.debug_chain(), "A1 C1 B1 B2 B3 A2");
    }

    #[test]
    fn a_fill_enters_its_copies_in_order() {
        let mut wb = Workbook::new();
        let s = wb.sheet_at(0).id;
        let a = |r, c| CellAddr::new(s, r, c);
        wb.set_input(a(0, 0), "=ROW()").unwrap();
        wb.fill(s, Rect::new(0, 0, 0, 0), Rect::new(1, 0, 3, 0))
            .unwrap();
        assert_eq!(wb.debug_chain(), "A2 A3 A4 A1");
    }
}
