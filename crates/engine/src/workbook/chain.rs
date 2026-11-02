use std::collections::BTreeMap;

use super::{FormulaId, Workbook};

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
        key
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
    pub(crate) fn chain_reenter(&mut self, ids: &[FormulaId]) {
        for &fid in ids {
            let old = self.formula(fid).seq;
            self.calc.chain.remove(&old);
            let key = self.chain_top();
            self.formula_mut(fid).seq = key;
            self.calc.chain.insert(key, fid);
        }
    }
}
