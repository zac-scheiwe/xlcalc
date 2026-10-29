use std::collections::BTreeMap;

use crate::address::Rect;
use crate::value::Value;
use crate::workbook::FormulaId;

#[derive(Clone, Debug)]
pub enum Entry {
    Const(Value),
    Formula(FormulaId),
}

const BLOCK_BITS: u32 = 8;
const BLOCK: u32 = 1 << BLOCK_BITS;
const MASK: u32 = BLOCK - 1;

struct Block {
    cells: Box<[Option<Entry>]>,
    count: u32,
}

impl Block {
    fn new() -> Block {
        Block {
            cells: (0..BLOCK).map(|_| None).collect(),
            count: 0,
        }
    }
}

#[derive(Default)]
struct Column {
    blocks: BTreeMap<u32, Block>,
}

#[derive(Default)]
pub struct CellStore {
    cols: BTreeMap<u32, Column>,

    row_counts: BTreeMap<u32, u32>,
    col_counts: BTreeMap<u32, u32>,
    len: usize,

    sticky: Option<Rect>,
}

impl std::fmt::Debug for CellStore {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "CellStore({} cells)", self.len)
    }
}

fn bump(m: &mut BTreeMap<u32, u32>, k: u32, up: bool) {
    if up {
        *m.entry(k).or_default() += 1;
    } else if let Some(n) = m.get_mut(&k) {
        *n -= 1;
        if *n == 0 {
            m.remove(&k);
        }
    }
}

impl CellStore {
    #[inline]
    pub fn get(&self, row: u32, col: u32) -> Option<&Entry> {
        let b = self.cols.get(&col)?.blocks.get(&(row >> BLOCK_BITS))?;
        b.cells[(row & MASK) as usize].as_ref()
    }

    pub fn set(&mut self, row: u32, col: u32, e: Entry) -> Option<Entry> {
        let block = self
            .cols
            .entry(col)
            .or_default()
            .blocks
            .entry(row >> BLOCK_BITS)
            .or_insert_with(Block::new);
        let slot = &mut block.cells[(row & MASK) as usize];
        let prev = slot.replace(e);
        let cell = Rect::new(row, col, row, col);
        self.sticky = Some(self.sticky.map_or(cell, |b| b.union_bounds(&cell)));
        if prev.is_none() {
            block.count += 1;
            self.len += 1;
            bump(&mut self.row_counts, row, true);
            bump(&mut self.col_counts, col, true);
        }
        prev
    }

    pub fn remove(&mut self, row: u32, col: u32) -> Option<Entry> {
        let column = self.cols.get_mut(&col)?;
        let bi = row >> BLOCK_BITS;
        let block = column.blocks.get_mut(&bi)?;
        let prev = block.cells[(row & MASK) as usize].take()?;
        block.count -= 1;
        if block.count == 0 {
            column.blocks.remove(&bi);
            if column.blocks.is_empty() {
                self.cols.remove(&col);
            }
        }
        self.len -= 1;
        bump(&mut self.row_counts, row, false);
        bump(&mut self.col_counts, col, false);
        Some(prev)
    }

    pub fn len(&self) -> usize {
        self.len
    }

    pub fn sticky_bounds(&self) -> Option<Rect> {
        self.sticky
    }

    pub fn extend_sticky(&mut self, r: Rect) {
        self.sticky = Some(self.sticky.map_or(r, |b| b.union_bounds(&r)));
    }

    pub fn bounds(&self) -> Option<Rect> {
        let (&r0, _) = self.row_counts.first_key_value()?;
        let (&r1, _) = self.row_counts.last_key_value()?;
        let (&c0, _) = self.col_counts.first_key_value()?;
        let (&c1, _) = self.col_counts.last_key_value()?;
        Some(Rect::new(r0, c0, r1, c1))
    }

    pub fn for_each_in<'s>(&'s self, rect: Rect, mut f: impl FnMut(u32, u32, &'s Entry)) {
        let Some(b) = self.bounds().and_then(|b| b.intersection(&rect)) else {
            return;
        };
        let cols: Vec<(u32, &Column)> = self
            .cols
            .range(b.c0..=b.c1)
            .map(|(c, col)| (*c, col))
            .collect();
        if cols.len() == 1 {
            let (c, col) = cols[0];
            for (&bi, block) in col
                .blocks
                .range((b.r0 >> BLOCK_BITS)..=(b.r1 >> BLOCK_BITS))
            {
                let base = bi << BLOCK_BITS;
                let lo = b.r0.max(base) - base;
                let hi = b.r1.min(base + MASK) - base;
                for (i, slot) in block.cells[lo as usize..=hi as usize].iter().enumerate() {
                    if let Some(e) = slot {
                        f(base + lo + i as u32, c, e);
                    }
                }
            }
            return;
        }
        let mut current: Vec<(u32, &Block)> = Vec::with_capacity(cols.len());
        for bi in (b.r0 >> BLOCK_BITS)..=(b.r1 >> BLOCK_BITS) {
            current.clear();
            for (c, col) in &cols {
                if let Some(block) = col.blocks.get(&bi) {
                    current.push((*c, block));
                }
            }
            if current.is_empty() {
                continue;
            }
            let base = bi << BLOCK_BITS;
            let lo = b.r0.max(base);
            let hi = b.r1.min(base + MASK);
            for r in lo..=hi {
                let i = (r - base) as usize;
                for (c, block) in &current {
                    if let Some(e) = &block.cells[i] {
                        f(r, *c, e);
                    }
                }
            }
        }
    }

    pub fn iter_rect(&self, rect: Rect) -> impl Iterator<Item = (u32, u32, &Entry)> + '_ {
        let mut v = Vec::new();
        self.for_each_in(rect, |r, c, e| v.push((r, c, e)));
        v.into_iter()
    }

    pub fn iter_row(&self, row: u32, c0: u32, c1: u32) -> impl Iterator<Item = (u32, &Entry)> + '_ {
        self.cols.range(c0..=c1).filter_map(move |(&c, col)| {
            col.blocks
                .get(&(row >> BLOCK_BITS))
                .and_then(|b| b.cells[(row & MASK) as usize].as_ref())
                .map(|e| (c, e))
        })
    }

    pub fn iter(&self) -> impl Iterator<Item = (u32, u32, &Entry)> + '_ {
        let rect = self.bounds().unwrap_or(Rect::cell(0, 0));
        self.iter_rect(rect)
    }

    pub fn any_in(&self, rect: Rect, except: Option<(u32, u32)>) -> bool {
        let mut any = false;
        self.for_each_in(rect, |r, c, _| {
            if Some((r, c)) != except {
                any = true;
            }
        });
        any
    }
}
