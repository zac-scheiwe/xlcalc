use crate::value::{Value, XlError};

pub(super) const NO_ITEM: u32 = 0x10_0100;

const NO_VALUE: u32 = 0x3a;

pub(super) const DATA: u16 = 0;
pub(super) const AUTO_SUB: u16 = 1;
pub(super) const GRAND: u16 = 0x10;

pub(super) const NON_VALUE: u16 = 0x11;

#[derive(Clone, Debug)]
pub(super) struct Sxli {
    pub items: Vec<u32>,

    pub kind: u16,

    pub data: usize,

    pub level: usize,
}

impl Sxli {
    fn depth(&self) -> usize {
        self.items.len()
    }

    fn item(&self, lv: usize) -> u32 {
        self.items.get(lv).copied().unwrap_or(NO_ITEM)
    }

    fn real(&self, lv: usize) -> bool {
        self.item(lv) < 0x10_0000
    }

    fn last_real(&self) -> isize {
        let mut i = self.depth() as isize - 1;
        while i > 0 && !self.real(i as usize) {
            i -= 1;
        }
        i
    }

    fn real_before(&self, from: isize) -> isize {
        let mut i = from;
        loop {
            i -= 1;
            if i < 1 || self.real(i as usize) {
                return i;
            }
        }
    }
}

fn direct_parent(p: &Sxli, c: &Sxli) -> bool {
    direct_up(p, c, false)
}

fn direct_ancestor(p: &Sxli, c: &Sxli) -> bool {
    direct_up(p, c, true)
}

fn direct_up(p: &Sxli, c: &Sxli, whole: bool) -> bool {
    if p.data != c.data {
        return false;
    }
    let up = c.real_before(c.last_real());
    if up < 0 {
        return p.kind == GRAND;
    }
    let up = up as usize;
    let mut pl = p.depth() as isize - 1;
    while pl > 0 && !(p.real(pl as usize) && p.real(up)) {
        pl -= 1;
    }
    if pl != up as isize {
        return false;
    }
    if whole {
        (0..=up).all(|i| p.item(i) == c.item(i))
    } else {
        p.item(up) == c.item(up)
    }
}

#[derive(Clone, Copy, Debug, PartialEq)]
pub(super) enum BaseItem {
    Prev,
    Next,
    Item(u32),
}

fn search_sxli(lines: &[Sxli], idx: usize, dim: usize, base: BaseItem) -> Option<usize> {
    let cur = &lines[idx];
    let fits = |cand: &Sxli, item: Option<u32>| {
        (cand.kind == cur.kind || cand.kind == AUTO_SUB || cur.kind == AUTO_SUB)
            && cand.depth() == cur.depth()
            && cand.kind != GRAND
            && (0..cur.depth()).all(|u| {
                if u == dim {
                    item.is_none_or(|b| cand.item(u) == b)
                } else {
                    cand.item(u) == cur.item(u)
                }
            })
    };
    match base {
        BaseItem::Prev => (0..idx)
            .rev()
            .find(|&i| fits(&lines[i], None))
            .or(Some(idx)),
        BaseItem::Next => (idx + 1..lines.len())
            .find(|&i| fits(&lines[i], None))
            .or(Some(idx)),
        BaseItem::Item(b) => (0..lines.len()).find(|&i| fits(&lines[i], Some(b))),
    }
}

#[derive(Clone, Copy, Debug, PartialEq)]
pub(super) struct XNum {
    bits: u64,
    ty: Ty,
}

#[derive(Clone, Copy, Debug, PartialEq)]
enum Ty {
    Num,
    Err,
    Blank,
}

const ERR_CODES: [(XlError, u32); 7] = [
    (XlError::NULL, 0),
    (XlError::DIV0, 7),
    (XlError::VALUE, 0xf),
    (XlError::REF, 0x17),
    (XlError::NAME, 0x1d),
    (XlError::NUM, 0x24),
    (XlError::NA, 0x2a),
];

impl XNum {
    const ZERO: XNum = XNum {
        bits: 0,
        ty: Ty::Num,
    };

    pub fn from_value(v: &Value) -> XNum {
        match v {
            Value::Number(n) => XNum::num(*n),
            Value::Error(e) => XNum::err(match ERR_CODES.iter().find(|x| x.0 == *e) {
                Some(x) => x.1,

                None => 0x100 + XlError::ALL.iter().position(|x| x == e).unwrap_or(0) as u32,
            }),
            _ => XNum::err(NO_VALUE),
        }
    }

    pub fn to_value(self) -> Value {
        match self.ty {
            Ty::Num => Value::Number(f64::from_bits(self.bits)),
            Ty::Blank => Value::Blank,
            Ty::Err => {
                let code = self.bits as u32;
                if code == NO_VALUE {
                    return Value::Blank;
                }
                if let Some(x) = ERR_CODES.iter().find(|x| x.1 == code) {
                    return Value::Error(x.0);
                }
                match code
                    .checked_sub(0x100)
                    .and_then(|i| XlError::ALL.get(i as usize))
                {
                    Some(e) => Value::Error(*e),

                    None => Value::Error(XlError::NA),
                }
            }
        }
    }

    fn num(x: f64) -> XNum {
        XNum {
            bits: x.to_bits(),
            ty: Ty::Num,
        }
    }

    fn err(code: u32) -> XNum {
        XNum {
            bits: code as u64,
            ty: Ty::Err,
        }
    }

    fn code(self) -> u32 {
        self.bits as u32
    }

    fn val(self) -> f64 {
        f64::from_bits(self.bits)
    }

    fn is_no_value(self) -> bool {
        self.ty == Ty::Err && self.code() == NO_VALUE
    }

    fn set_err(&mut self, code: u32) {
        self.bits = (self.bits & !0xffff_ffff) | code as u64;
        self.ty = Ty::Err;
    }
}

#[derive(Clone, Debug)]
pub(super) struct Spec {
    pub kind: i32,

    pub axis: u8,

    pub dim: usize,
    pub base: BaseItem,
}

pub(super) struct Grid {
    pub rows: Vec<Sxli>,
    pub cols: Vec<Sxli>,

    pub row_values: Vec<bool>,
    pub col_values: Vec<bool>,

    pub cells: Vec<Vec<XNum>>,

    pub nd: usize,

    pub data_axis: u8,
}

#[derive(Default)]
struct Reg {
    x: f64,
    flags: u32,
}

impl Reg {
    fn add(&mut self, b: XNum) {
        self.x += b.val();
        self.check_inf();
    }

    fn sub(&mut self, b: XNum) {
        self.x -= b.val();
        self.check_inf();
    }

    fn mul(&mut self, b: XNum) {
        self.x *= b.val();
        self.flush();
    }

    fn div(&mut self, b: XNum) {
        if b.bits & 0x7ff0_0000_0000_0000 == 0 {
            self.flags = 4;
            return;
        }
        self.x /= b.val();
        self.flush();
    }

    fn flush(&mut self) {
        match self.x.to_bits() & 0x7ff0_0000_0000_0000 {
            0 => self.x = 0.0,
            0x7ff0_0000_0000_0000 => self.flags = 1,
            _ => {}
        }
    }

    fn check_inf(&mut self) {
        if self.x.to_bits() & 0x7ff0_0000_0000_0000 == 0x7ff0_0000_0000_0000 {
            self.flags = 1;
        }
    }

    fn take_err(&mut self) -> Option<u32> {
        let f = std::mem::take(&mut self.flags);
        (f != 0).then_some(if f & 4 != 0 {
            7
        } else if f & 0x10 != 0 {
            0x2a
        } else if f & 0x20 != 0 {
            0xf
        } else {
            0x24
        })
    }
}

enum End {
    Keep,

    Write,

    WriteNoCheck,
    Blank,
    NA,
    Err(u32),
}

impl Grid {
    fn grand_row(&self, d: usize) -> usize {
        let a8 = if self.nd >= 2 && self.data_axis == 1 {
            self.nd
        } else {
            1
        };
        self.rows.len() - a8 + if self.data_axis == 1 { d } else { 0 }
    }

    fn grand_col(&self, d: usize) -> usize {
        let a4 = if self.nd >= 2 && self.data_axis == 2 {
            self.nd
        } else {
            1
        };
        self.cols.len() - a4 + if self.data_axis == 2 { d } else { 0 }
    }

    pub fn apply(&mut self, d: usize, spec: &Spec) {
        let k = spec.kind;
        if k <= 0 || k > 0xe || self.rows.is_empty() || self.cols.is_empty() {
            return;
        }
        if matches!(k, 0xd | 0xe) && spec.axis != 0 {
            self.ranked(d, spec);
            return;
        }
        let mut reg = Reg::default();
        let rev = spec.base == BaseItem::Prev && matches!(k, 1..=3);
        let passes = if matches!(k, 1..=3 | 0xc) { 2 } else { 1 };
        let (nr, nc) = (self.rows.len(), self.cols.len());
        let order = |n: usize| -> Vec<usize> {
            if rev {
                (0..n).rev().collect()
            } else {
                (0..n).collect()
            }
        };
        let multi_rows = self.nd >= 2 && self.data_axis == 1;
        let multi_cols = self.nd >= 2 && self.data_axis == 2;
        for pass in 0..passes {
            for r in order(nr) {
                if multi_rows && self.rows[r].data != d {
                    continue;
                }
                for c in order(nc) {
                    if multi_cols && self.cols[c].data != d {
                        continue;
                    }
                    let end = self.step(&mut reg, pass, r, c, d, spec);
                    let cell = &mut self.cells[r][c];
                    match end {
                        End::Keep => {}
                        End::Write => match reg.take_err() {
                            Some(code) => cell.set_err(code),
                            None => cell.bits = reg.x.to_bits(),
                        },
                        End::WriteNoCheck => cell.bits = reg.x.to_bits(),
                        End::Blank => cell.ty = Ty::Blank,
                        End::NA => cell.set_err(0x2a),
                        End::Err(code) => cell.set_err(code),
                    }
                }
            }
        }
    }

    fn step(&mut self, reg: &mut Reg, pass: usize, r: usize, c: usize, d: usize, s: &Spec) -> End {
        let k = s.kind;
        {
            let cell = &mut self.cells[r][c];
            if cell.ty == Ty::Num {
                reg.x = cell.val();
            } else if cell.is_no_value() {
                if !matches!(k, 2 | 3) {
                    cell.ty = Ty::Num;
                }
                reg.x = 0.0;
                cell.bits = 0;
            }
        }
        let (gr, gc) = (self.grand_row(d), self.grand_col(d));
        if matches!(k, 5..=8 | 0xc) && self.cells[r][c].ty != Ty::Num {
            return End::Keep;
        }
        match k {
            8 => {
                for (i, (rr, cc)) in [(gr, gc), (r, gc), (gr, c)].into_iter().enumerate() {
                    let mut b = self.cells[rr][cc];
                    if b.ty == Ty::Err {
                        if b.code() != NO_VALUE {
                            return End::Err(b.code());
                        }
                        b = XNum::ZERO;
                    }
                    if i == 0 {
                        reg.mul(b);
                    } else {
                        reg.div(b);
                    }
                }
                End::Write
            }
            5..=7 => {
                let mut b = match k {
                    5 => self.cells[r][gc],
                    6 => self.cells[gr][c],
                    _ => self.cells[gr][gc],
                };
                if b.ty == Ty::Err {
                    if b.code() != NO_VALUE {
                        return End::Err(b.code());
                    }
                    self.cells[r][c].ty = Ty::Blank;
                    b = XNum::ZERO;
                }
                reg.div(b);
                End::Write
            }
            1..=4 | 0xc => self.step_base(reg, pass, (r, c), d, s),
            9..=0xb => self.step_parent(reg, r, c, s),

            _ => End::NA,
        }
    }

    fn base_lines(&self, axis: u8) -> &[Sxli] {
        if axis == 2 {
            &self.cols
        } else {
            &self.rows
        }
    }

    fn at(&self, axis: u8, r: usize, c: usize, i: usize) -> XNum {
        if axis == 2 {
            self.cells[r][i]
        } else {
            self.cells[i][c]
        }
    }

    fn step_base(
        &mut self,
        reg: &mut Reg,
        pass: usize,
        (r, c): (usize, usize),
        d: usize,
        s: &Spec,
    ) -> End {
        let k = s.kind;
        if s.axis == 0 {
            return End::NA;
        }
        let idx = if s.axis == 2 { c } else { r };
        let lines = self.base_lines(s.axis);
        let line = &lines[idx];
        if line.kind != DATA {
            if line.kind == GRAND || s.dim >= line.depth() {
                return End::Blank;
            }
        } else if line.depth() <= s.dim {
            return End::NA;
        }
        let mine = line.item(s.dim);
        let base = if matches!(k, 4 | 0xc) {
            BaseItem::Prev
        } else {
            s.base
        };
        let Some(j) = search_sxli(lines, idx, s.dim, base) else {
            return End::NA;
        };
        let lb = lines[j].clone();
        let cur_ty = self.cells[r][c].ty;
        if pass == 0 {
            if mine == lb.item(s.dim) {
                if matches!(k, 2 | 3) && cur_ty == Ty::Err {
                    return End::Blank;
                }
                return End::Keep;
            }
        } else if k != 0xc {
            if mine == lb.item(s.dim) {
                if k == 2 {
                    if cur_ty != Ty::Num {
                        return End::Keep;
                    }
                    if self.cells[r][c].bits != 0 {
                        reg.x = 1.0;
                        self.cells[r][c].bits = 1f64.to_bits();
                        return End::Keep;
                    }
                    return End::Err(7);
                }
                return End::Blank;
            }
            return End::Keep;
        }
        let bcell = self.at(s.axis, r, c, j);
        if cur_ty == Ty::Err {
            return End::Keep;
        }
        let mut b = bcell;
        if matches!(b.ty, Ty::Blank | Ty::Err) {
            if matches!(k, 2 | 3) {
                return End::Blank;
            }
            if b.ty == Ty::Err {
                if b.code() != NO_VALUE {
                    return End::Err(b.code());
                }
                b = XNum::ZERO;
            }
        }
        if b.ty != Ty::Num {
            b = XNum::ZERO;
            self.cells[r][c].ty = Ty::Blank;
        }
        if k == 0xc {
            if pass == 0 {
                reg.add(b);
                return End::Write;
            }
            let p = self.running_divisor((r, c), d, s, &lb);
            if p.val() == 0.0 && self.cells[r][c].val() == 0.0 {
                return End::Blank;
            }
            reg.div(p);
            return End::Write;
        }
        if k == 4 {
            reg.add(b);
        } else if self.cells[r][c].ty != Ty::Blank {
            if k != 2 {
                reg.sub(b);
            }
            if k != 1 {
                reg.div(b);
            }
        }
        End::Write
    }

    fn running_divisor(&self, (r, c): (usize, usize), d: usize, s: &Spec, lb: &Sxli) -> XNum {
        let (gr, gc) = (self.grand_row(d), self.grand_col(d));
        let lines = self.base_lines(s.axis);
        let idx = if s.axis == 2 { c } else { r };
        let depth = lb.depth();
        if depth == s.dim + 1 {
            if s.dim != 0 {
                for (i, cand) in lines.iter().enumerate().skip(idx + 1) {
                    let cd = cand.depth();
                    if direct_ancestor(cand, lb)
                        && cd < depth
                        && cd > 0
                        && lb.item(cd - 1) == cand.item(cd - 1)
                        && cand.data == lb.data
                    {
                        return self.at(s.axis, r, c, i);
                    }
                }
            }
            if s.axis == 2 {
                self.cells[r][gc]
            } else {
                self.cells[gr][c]
            }
        } else {
            let mut p = self.cells[r][c];
            for (i, cand) in lines.iter().enumerate().skip(idx) {
                if cand.depth() == depth
                    && (s.dim == 0 || lb.item(s.dim - 1) == cand.item(s.dim - 1))
                    && cand.data == lb.data
                    && (0..depth).all(|u| u == s.dim || lb.item(u) == cand.item(u))
                {
                    p = self.at(s.axis, r, c, i);
                }
            }
            p
        }
    }

    fn step_parent(&mut self, reg: &mut Reg, r: usize, c: usize, s: &Spec) -> End {
        let k = s.kind;
        let (on_cols, dim) = match k {
            9 => (false, 0),
            10 => (true, 0),
            _ => {
                if s.axis == 0 {
                    return End::NA;
                }
                (s.axis == 2, s.dim)
            }
        };
        let lines = if on_cols { &self.cols } else { &self.rows };
        let idx = if on_cols { c } else { r };
        let cur = &lines[idx];
        if cur.depth() <= dim && (cur.kind == DATA || k != 0xb) {
            return End::Blank;
        }
        let found = if k != 0xb {
            lines.iter().enumerate().skip(idx).position(|(_, cand)| {
                cand.data == cur.data
                    && (cand.kind == GRAND
                        || (cand.kind == AUTO_SUB
                            && (direct_parent(cand, cur) || cand.level < cur.level)))
            })
        } else {
            if cur.depth() < dim + 1 || cur.kind == GRAND {
                return End::Blank;
            }
            let values = if on_cols {
                &self.col_values
            } else {
                &self.row_values
            };
            if dont_calc_per_par(values, cur, dim) {
                reg.x = 1.0;
                return End::WriteNoCheck;
            }
            lines.iter().enumerate().skip(idx).position(|(_, cand)| {
                cand.depth() == dim + 1
                    && cur.item(dim) == cand.item(dim)
                    && cand.data == cur.data
                    && cand.kind < 2
            })
        };
        let Some(i) = found.map(|p| p + idx) else {
            return End::Blank;
        };
        let div = if on_cols {
            self.cells[r][i]
        } else {
            self.cells[i][c]
        };
        reg.div(div);
        if reg.flags == 4 {
            reg.flags = 0;
            return End::Blank;
        }
        End::Write
    }

    fn ranked(&mut self, d: usize, s: &Spec) {
        let on_rows = s.axis == 1;
        let (nb, no) = if on_rows {
            (self.rows.len(), self.cols.len())
        } else {
            (self.cols.len(), self.rows.len())
        };
        let other_multi = self.nd >= 2 && self.data_axis == if on_rows { 2 } else { 1 };
        let asc = s.kind == 0xd;
        let dim = s.dim;
        for o in 0..no {
            let other = if on_rows {
                &self.cols[o]
            } else {
                &self.rows[o]
            };
            let other_data = other.data;
            let base = if on_rows { &self.rows } else { &self.cols };
            let cell = |g: &Grid, i: usize| {
                if on_rows {
                    g.cells[i][o]
                } else {
                    g.cells[o][i]
                }
            };
            let matches_data = |l: &Sxli| {
                if other_multi {
                    other_data == d
                } else {
                    l.data == d
                }
            };
            let mut sorted: Vec<usize> = (0..nb)
                .filter(|&i| matches_data(&base[i]) && cell(self, i).ty == Ty::Num)
                .collect();
            if sorted.is_empty() {
                continue;
            }
            sorted.sort_by(|&a, &b| cell(self, a).val().total_cmp(&cell(self, b).val()));
            let mut ranks: Vec<Option<usize>> = vec![None; nb];
            let mut blank: Vec<bool> = vec![false; nb];
            for i in 0..nb {
                let cur = &base[i];
                let x = cell(self, i);
                if cur.kind == GRAND && matches_data(cur) {
                    blank[i] = true;
                    continue;
                }
                if x.ty == Ty::Blank || !matches_data(cur) {
                    continue;
                }
                if cur.level < dim + 1 {
                    blank[i] = true;
                    continue;
                }
                let mut count = 0;
                let mut counted = false;
                let mut prev: Option<f64> = None;
                let mut p = if asc {
                    0isize
                } else {
                    sorted.len() as isize - 1
                };
                loop {
                    let j = sorted[p as usize];
                    let v = cell(self, j).val();
                    if v.total_cmp(&x.val()).is_eq() {
                        break;
                    }
                    p += if asc { 1 } else { -1 };
                    if p < 0 || p as usize >= sorted.len() {
                        break;
                    }
                    let cand = &base[j];
                    if prev.is_some_and(|q| !q.total_cmp(&v).is_eq()) {
                        counted = false;
                    }
                    let last = prev;
                    prev = Some(v);
                    if cand.level == cur.level
                        && cand.kind != GRAND
                        && (cur.level == dim + 1
                            || cur.item(cur.last_real().max(0) as usize)
                                == cand.item(cand.last_real().max(0) as usize))
                        && (dim < 1 || cur.item(dim - 1) == cand.item(dim - 1))
                    {
                        if counted && last.is_some_and(|q| q.total_cmp(&v).is_eq()) {
                            continue;
                        }
                        if csic(cand, cur) >= dim {
                            count += 1;
                            counted = true;
                        }
                    }
                }
                ranks[i] = Some(count + 1);
                if x.ty == Ty::Err {
                    blank[i] = true;
                }
            }
            for i in 0..nb {
                let c = if on_rows {
                    &mut self.cells[i][o]
                } else {
                    &mut self.cells[o][i]
                };
                if blank[i] {
                    c.ty = Ty::Blank;
                } else if let (Some(rk), Ty::Num) = (ranks[i], c.ty) {
                    *c = XNum::num(rk as f64);
                }
            }
        }
    }
}

fn csic(a: &Sxli, b: &Sxli) -> usize {
    if b.kind == GRAND {
        return 0;
    }
    let n = a.depth().min(b.depth());
    (0..n).take_while(|&i| a.item(i) == b.item(i)).count()
}

fn dont_calc_per_par(values: &[bool], line: &Sxli, dim: usize) -> bool {
    let mut i = dim + 1;
    while i < values.len() && line.item(i) == NO_ITEM {
        i += 1;
    }
    i < values.len() && values[i]
}

#[cfg(test)]
mod tests {
    use super::*;

    fn line(items: &[u32], kind: u16) -> Sxli {
        Sxli {
            items: items.to_vec(),
            kind,
            data: 0,
            level: items.len(),
        }
    }

    fn grid(vals: &[f64]) -> Grid {
        let rows = vec![
            line(&[0, 0], DATA),
            line(&[0, 1], DATA),
            line(&[0], AUTO_SUB),
            line(&[1, 0], DATA),
            line(&[1, 1], DATA),
            line(&[1], AUTO_SUB),
            line(&[NO_ITEM], GRAND),
        ];
        Grid {
            cells: vals.iter().map(|v| vec![XNum::num(*v)]).collect(),
            rows,
            cols: vec![line(&[], DATA)],
            row_values: vec![false, false],
            col_values: vec![],
            nd: 1,
            data_axis: 0,
        }
    }

    fn vals(g: &Grid) -> Vec<Option<f64>> {
        g.cells
            .iter()
            .map(|r| match r[0].to_value() {
                Value::Number(n) => Some(n),
                Value::Blank => None,
                v => panic!("{v:?}"),
            })
            .collect()
    }

    #[test]
    fn search_previous_and_item() {
        let g = grid(&[1.0, 2.0, 3.0, 4.0, 5.0, 9.0, 12.0]);
        assert_eq!(search_sxli(&g.rows, 3, 0, BaseItem::Prev), Some(0));
        assert_eq!(search_sxli(&g.rows, 0, 0, BaseItem::Prev), Some(0));
        assert_eq!(search_sxli(&g.rows, 4, 0, BaseItem::Item(0)), Some(1));
        assert_eq!(search_sxli(&g.rows, 5, 0, BaseItem::Item(0)), Some(2));
        assert_eq!(search_sxli(&g.rows, 4, 0, BaseItem::Item(7)), None);
        assert_eq!(search_sxli(&g.rows, 1, 1, BaseItem::Next), Some(1));
    }

    #[test]
    fn parent_lines() {
        let g = grid(&[0.0; 7]);
        assert!(direct_parent(&g.rows[2], &g.rows[0]));
        assert!(!direct_parent(&g.rows[5], &g.rows[0]));
        assert!(direct_parent(&g.rows[6], &g.rows[2]));
        assert!(direct_ancestor(&g.rows[6], &g.rows[5]));
    }

    #[test]
    fn percent_of_parent_row_and_running_total() {
        let mut g = grid(&[1.0, 3.0, 4.0, 2.0, 6.0, 8.0, 12.0]);
        let spec = Spec {
            kind: 9,
            axis: 0,
            dim: 0,
            base: BaseItem::Prev,
        };
        g.apply(0, &spec);
        assert_eq!(
            vals(&g),
            [0.25, 0.75, 4.0 / 12.0, 0.25, 0.75, 8.0 / 12.0]
                .iter()
                .map(|x| Some(*x))
                .chain([Some(1.0)])
                .collect::<Vec<_>>()
        );
        let mut g = grid(&[1.0, 3.0, 4.0, 2.0, 6.0, 8.0, 12.0]);
        let spec = Spec {
            kind: 4,
            axis: 1,
            dim: 0,
            base: BaseItem::Prev,
        };
        g.apply(0, &spec);
        let got = vals(&g);
        assert_eq!(got[3], Some(3.0));
        assert_eq!(got[5], Some(12.0));
        assert_eq!(got[6], None);
    }

    #[test]
    fn divide_by_zero_exponent() {
        let mut r = Reg { x: 1.0, flags: 0 };
        r.div(XNum::num(5e-324));
        assert_eq!(r.take_err(), Some(7));
        r.x = 1e300;
        r.div(XNum::num(1e-300));
        assert_eq!(r.take_err(), Some(0x24));
        r.x = 1e-300;
        r.div(XNum::num(1e300));
        assert_eq!((r.x, r.take_err()), (0.0, None));
    }

    #[test]
    fn dense_rank() {
        let mut g = grid(&[5.0, 5.0, 10.0, 7.0, 1.0, 8.0, 18.0]);
        let spec = Spec {
            kind: 0xe,
            axis: 1,
            dim: 1,
            base: BaseItem::Prev,
        };
        g.apply(0, &spec);
        let got = vals(&g);
        assert_eq!(got[0], Some(1.0));
        assert_eq!(got[1], Some(1.0));
        assert_eq!(got[3], Some(1.0));
        assert_eq!(got[4], Some(2.0));
        assert_eq!(got[2], None);
        assert_eq!(got[6], None);
    }
}
