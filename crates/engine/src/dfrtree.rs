const MAX: usize = 20;

const MIN: usize = 10;

const REINSERT: usize = 6;

pub const ANY_KEY: u64 = 0xffff_ffff_ffff;

const LAST_ROW: i32 = 0xf_ffff;
const LAST_COL: i32 = 0x3fff;

#[derive(Clone, Copy, Debug, Default, PartialEq, Eq)]
pub struct Bounds {
    pub row0: i32,
    pub row1: i32,
    pub col0: i32,
    pub col1: i32,
}

impl Bounds {
    pub const fn new(row0: i32, row1: i32, col0: i32, col1: i32) -> Bounds {
        Bounds {
            row0,
            row1,
            col0,
            col1,
        }
    }

    pub fn union(&mut self, o: &Bounds) {
        let valid = self.row1 >= self.row0
            && self.col1 >= self.col0
            && (0..=LAST_ROW).contains(&self.row0)
            && (0..=LAST_ROW).contains(&self.row1)
            && (0..=LAST_COL).contains(&self.col0)
            && (0..=LAST_COL).contains(&self.col1);
        if !valid {
            *self = *o;
            return;
        }
        self.row0 = self.row0.min(o.row0);
        self.row1 = self.row1.max(o.row1);
        self.col0 = self.col0.min(o.col0);
        self.col1 = self.col1.max(o.col1);
    }

    fn intersection(&self, o: &Bounds) -> Option<Bounds> {
        let r = Bounds {
            row0: self.row0.max(o.row0),
            row1: self.row1.min(o.row1),
            col0: self.col0.max(o.col0),
            col1: self.col1.min(o.col1),
        };
        (r.row0 <= r.row1 && r.col0 <= r.col1).then_some(r)
    }

    fn contains(&self, o: &Bounds) -> bool {
        self.row0 <= o.row0 && o.row1 <= self.row1 && self.col0 <= o.col0 && o.col1 <= self.col1
    }

    fn area(&self) -> i64 {
        i64::from(self.col1 - self.col0 + 1) * i64::from(self.row1 - self.row0 + 1)
    }

    fn side(&self, axis: usize) -> i32 {
        [self.row0, self.row1, self.col0, self.col1][axis]
    }

    fn centre(&self) -> (i32, i32) {
        ((self.row1 + self.row0) / 2, (self.col1 + self.col0) / 2)
    }
}

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum Search {
    Equal,

    Overlaps,

    Holds,

    Inside,

    OverlapsPartColumns,
}

#[derive(Clone, Copy, Debug, Default, PartialEq, Eq)]
struct Tags {
    key: u64,
    kind: u32,
}

#[derive(Clone, Copy, Debug)]
struct Entry<T> {
    bounds: Bounds,
    tags: Tags,
    value: T,
}

#[derive(Clone, Debug, Default)]
struct Node {
    bounds: Bounds,
    tags: Tags,
    parent: Option<usize>,
    index_in_parent: usize,

    level: usize,
    count: usize,
    child: [usize; MAX + 1],
}

#[derive(Clone, Debug)]
pub struct DfRtree<T> {
    nodes: Vec<Node>,
    free_nodes: Vec<usize>,
    entries: Vec<Entry<T>>,
    free_entries: Vec<usize>,
    root: usize,

    height: usize,
    len: usize,

    reinserted: u64,
}

impl<T: Copy + PartialEq> Default for DfRtree<T> {
    fn default() -> Self {
        DfRtree {
            nodes: vec![Node::default()],
            free_nodes: Vec::new(),
            entries: Vec::new(),
            free_entries: Vec::new(),
            root: 0,
            height: 0,
            len: 0,
            reinserted: 0,
        }
    }
}

pub(crate) fn xls_qsort<E>(v: &mut [E], cmp: impl Fn(&E, &E) -> std::cmp::Ordering) {
    use std::cmp::Ordering::{Equal, Greater};
    if v.len() < 2 {
        return;
    }
    let mut stack = vec![(0usize, v.len() - 1)];
    while let Some((lo, hi)) = stack.pop() {
        let size = hi - lo + 1;
        if size <= 8 {
            let mut end = hi;
            while end > lo {
                let mut max = lo;
                for p in lo + 1..=end {
                    if cmp(&v[p], &v[max]) == Greater {
                        max = p;
                    }
                }
                v.swap(max, end);
                end -= 1;
            }
            continue;
        }
        let mut mid = lo + size / 2;
        for (a, b) in [(lo, mid), (lo, hi), (mid, hi)] {
            if cmp(&v[a], &v[b]) == Greater {
                v.swap(a, b);
            }
        }
        let (mut low, mut high) = (lo, hi);
        loop {
            let mut stopped = false;
            if mid > low {
                loop {
                    low += 1;
                    if low >= mid {
                        break;
                    }
                    if cmp(&v[low], &v[mid]) == Greater {
                        stopped = true;
                        break;
                    }
                }
            }
            if !stopped && mid <= low {
                loop {
                    low += 1;
                    if low > hi || cmp(&v[low], &v[mid]) == Greater {
                        break;
                    }
                }
            }
            loop {
                high -= 1;
                if high <= mid || cmp(&v[high], &v[mid]) != Greater {
                    break;
                }
            }
            if high < low {
                break;
            }
            v.swap(low, high);
            if mid == high {
                mid = low;
            }
        }
        high += 1;
        let mut stopped = false;
        if mid < high {
            loop {
                high -= 1;
                if high <= mid {
                    break;
                }
                if cmp(&v[high], &v[mid]) != Equal {
                    stopped = true;
                    break;
                }
            }
        }
        if !stopped && mid >= high {
            loop {
                high -= 1;
                if high <= lo || cmp(&v[high], &v[mid]) != Equal {
                    break;
                }
            }
        }

        let left = (lo < high).then_some((lo, high));
        let right = (low < hi).then_some((low, hi));
        let left_is_smaller = (high as isize - lo as isize) < (hi as isize - low as isize);
        let (first, then) = if left_is_smaller {
            (right, left)
        } else {
            (left, right)
        };
        stack.extend(first);
        stack.extend(then);
    }
}

impl<T: Copy + PartialEq> DfRtree<T> {
    pub fn len(&self) -> usize {
        self.len
    }

    pub fn is_empty(&self) -> bool {
        self.len == 0
    }

    pub fn height(&self) -> usize {
        self.height
    }

    pub fn insert(&mut self, bounds: Bounds, kind: u32, key: u64, value: T) {
        let entry = Entry {
            bounds,
            tags: Tags { key, kind },
            value,
        };
        let id = match self.free_entries.pop() {
            Some(id) => {
                self.entries[id] = entry;
                id
            }
            None => {
                self.entries.push(entry);
                self.entries.len() - 1
            }
        };
        self.reinserted = 0;
        self.insert_at(id, 0);
        self.len += 1;
    }

    fn child_info(&self, level: usize, id: usize) -> (Bounds, Tags) {
        if level == 0 {
            (self.entries[id].bounds, self.entries[id].tags)
        } else {
            (self.nodes[id].bounds, self.nodes[id].tags)
        }
    }

    fn children(&self, node: usize) -> &[usize] {
        let n = &self.nodes[node];
        &n.child[..n.count]
    }

    fn choose_subtree(&self, bounds: &Bounds, level: usize) -> usize {
        let mut node = self.root;
        while self.nodes[node].level != level {
            let at = self.nodes[node].level;
            let kids = self.children(node);
            let mut best: Option<(usize, i64, i64)> = None;
            for (i, &c) in kids.iter().enumerate() {
                let own = self.nodes[c].bounds;
                let mut grown = *bounds;
                grown.union(&own);
                let (first, second) = if at == 1 {
                    let mut overlap = 0i64;
                    for (j, &o) in kids.iter().enumerate() {
                        if i == j {
                            continue;
                        }
                        let other = &self.nodes[o].bounds;
                        overlap += grown.intersection(other).map_or(0, |r| r.area());
                        overlap -= own.intersection(other).map_or(0, |r| r.area());
                    }
                    (overlap, grown.area() - own.area())
                } else {
                    (grown.area() - own.area(), own.area())
                };
                if best.is_none_or(|(_, a, b)| (first, second) < (a, b)) {
                    best = Some((c, first, second));
                }
            }
            match best {
                Some((c, ..)) => node = c,
                None => break,
            }
        }
        node
    }

    fn push_child(&mut self, node: usize, id: usize) {
        let count = self.nodes[node].count;
        if self.nodes[node].level != 0 {
            self.nodes[id].parent = Some(node);
            self.nodes[id].index_in_parent = count;
        }
        self.nodes[node].child[count] = id;
        self.nodes[node].count += 1;
    }

    fn insert_at(&mut self, id: usize, level: usize) {
        let bounds = self.child_info(level, id).0;
        let mut node = Some(self.choose_subtree(&bounds, level));
        let mut item = id;
        let mut sibling = None;
        while let Some(n) = node {
            sibling = None;
            self.push_child(n, item);
            if self.nodes[n].count == MAX + 1 {
                let at = self.nodes[n].level;
                let lone =
                    self.nodes[n].parent == Some(self.root) && self.nodes[self.root].count == 1;
                if self.reinserted & (1 << at) == 0 && at != self.height && !lone {
                    self.reinserted |= 1 << at;
                    self.reinsert(n);
                } else {
                    sibling = Some(self.split(n));
                }
            } else {
                self.adjust(n, false);
            }
            node = self.nodes[n].parent;
            match sibling {
                Some(s) if node.is_some() => item = s,
                _ => break,
            }
        }
        if let Some(n) = node {
            self.adjust(n, true);
        } else if let Some(s) = sibling {
            let old = self.root;
            let root = self.alloc_node();
            self.height += 1;
            self.nodes[root].level = self.height;
            self.push_child(root, old);
            self.push_child(root, s);
            self.root = root;
            self.adjust(root, true);
        }
    }

    fn alloc_node(&mut self) -> usize {
        match self.free_nodes.pop() {
            Some(n) => {
                self.nodes[n] = Node::default();
                n
            }
            None => {
                self.nodes.push(Node::default());
                self.nodes.len() - 1
            }
        }
    }

    fn adjust(&mut self, node: usize, up: bool) {
        let mut next = Some(node);
        while let Some(n) = next {
            let count = self.nodes[n].count;
            if count == 0 {
                return;
            }
            let level = self.nodes[n].level;
            let (mut bounds, mut tags) = self.child_info(level, self.nodes[n].child[0]);
            for &c in &self.children(n)[1..] {
                let (b, t) = self.child_info(level, c);
                bounds.union(&b);
                tags.key |= t.key;
                tags.kind |= t.kind;
            }
            let same = self.nodes[n].bounds == bounds && self.nodes[n].tags == tags;
            if up && same {
                return;
            }
            self.nodes[n].bounds = bounds;
            self.nodes[n].tags = tags;
            if !up {
                return;
            }
            next = self.nodes[n].parent;
        }
    }

    fn full_children(&self, node: usize) -> Vec<(usize, Bounds)> {
        let level = self.nodes[node].level;
        self.children(node)
            .iter()
            .map(|&c| (c, self.child_info(level, c).0))
            .collect()
    }

    fn store_children(&mut self, node: usize, order: &[(usize, Bounds)]) {
        self.nodes[node].count = 0;
        for &(c, _) in order {
            self.push_child(node, c);
        }
    }

    fn reinsert(&mut self, node: usize) {
        let (row, col) = self.nodes[node].bounds.centre();
        let distance = |b: &Bounds| {
            let (r, c) = b.centre();
            let (dr, dc) = (i64::from(r - row), i64::from(c - col));
            dr * dr + dc * dc
        };
        let mut order = self.full_children(node);
        xls_qsort(&mut order, |a, b| distance(&a.1).cmp(&distance(&b.1)));
        let keep = MAX - REINSERT + 1;
        self.store_children(node, &order[..keep]);
        self.adjust(node, true);
        let level = self.nodes[node].level;
        for &(c, _) in &order[keep..] {
            self.insert_at(c, level);
        }
    }

    fn split(&mut self, node: usize) -> usize {
        let cuts = MIN..MAX - MIN + 2;
        let halves = |v: &[(usize, Bounds)], k: usize| {
            let (mut a, mut b) = (v[0].1, v[k].1);
            for (i, (_, r)) in v.iter().enumerate().skip(1) {
                if k <= i {
                    b.union(r);
                } else {
                    a.union(r);
                }
            }
            (a, b)
        };
        let mut order = self.full_children(node);
        let mut best_axis = 0;
        let mut best_margin = i64::MAX;
        for axis in 0..4 {
            xls_qsort(&mut order, |a, b| a.1.side(axis).cmp(&b.1.side(axis)));
            let margin: i64 = cuts
                .clone()
                .map(|k| {
                    let (a, b) = halves(&order, k);
                    i64::from(
                        (a.col1 - a.col0)
                            + (a.row1 - a.row0)
                            + (b.col1 - b.col0)
                            + (b.row1 - b.row0)
                            + 4,
                    )
                })
                .sum();
            if margin < best_margin {
                best_margin = margin;
                best_axis = axis;
            }
        }
        xls_qsort(&mut order, |a, b| {
            a.1.side(best_axis).cmp(&b.1.side(best_axis))
        });
        let mut best = (0, i64::MAX, i64::MAX);
        for k in cuts {
            let (a, b) = halves(&order, k);
            let overlap = a.intersection(&b).map_or(0, |r| r.area());
            let area = a.area() + b.area();
            if (overlap, area) < (best.1, best.2) {
                best = (k, overlap, area);
            }
        }
        let sibling = self.alloc_node();
        self.nodes[sibling].level = self.nodes[node].level;
        self.store_children(node, &order[..best.0]);
        self.store_children(sibling, &order[best.0..]);
        self.adjust(node, false);
        self.adjust(sibling, false);
        sibling
    }

    fn find_leaf(&self, node: usize, bounds: &Bounds, value: T) -> Option<(usize, usize)> {
        let n = &self.nodes[node];
        if !n.bounds.contains(bounds) {
            return None;
        }
        self.children(node).iter().enumerate().find_map(|(i, &c)| {
            if n.level != 0 {
                return self.find_leaf(c, bounds, value);
            }
            let e = &self.entries[c];
            (e.value == value && e.bounds == *bounds).then_some((node, i))
        })
    }

    pub fn remove(&mut self, bounds: &Bounds, value: T) -> bool {
        self.reinserted = 0;
        let Some((leaf, i)) = self.find_leaf(self.root, bounds, value) else {
            return false;
        };
        self.free_entries.push(self.nodes[leaf].child[i]);
        let last = self.nodes[leaf].count - 1;
        self.nodes[leaf].child[i] = self.nodes[leaf].child[last];
        self.nodes[leaf].count = last;
        let mut cur = leaf;
        let mut taken_out = None;
        while cur != self.root && self.nodes[cur].count < MIN {
            let Some(parent) = self.nodes[cur].parent else {
                break;
            };
            let at = self.nodes[cur].index_in_parent;
            let last = self.nodes[parent].count - 1;
            let moved = self.nodes[parent].child[last];
            self.nodes[moved].index_in_parent = at;
            self.nodes[parent].child[at] = moved;
            self.nodes[parent].count = last;
            taken_out = Some(cur);
            cur = parent;
        }
        self.adjust(cur, true);
        if let Some(top) = taken_out {
            self.nodes[top].parent = None;
            let mut chain = Some(leaf);
            while let Some(n) = chain {
                let level = self.nodes[n].level;
                let mut i = 0;
                while i < self.nodes[n].count {
                    self.insert_at(self.nodes[n].child[i], level);
                    i += 1;
                }
                chain = self.nodes[n].parent;
                self.free_nodes.push(n);
            }
        }
        let root = self.root;
        if self.nodes[root].level != 0 && self.nodes[root].count == 1 {
            self.root = self.nodes[root].child[0];
            self.free_nodes.push(root);
            self.nodes[self.root].parent = None;
            self.nodes[self.root].index_in_parent = 0;
            self.height -= 1;
        }
        self.len -= 1;
        true
    }

    pub fn search(
        &self,
        query: Option<&Bounds>,
        option: Search,
        kind: u32,
        key: u64,
    ) -> Vec<(Bounds, T)> {
        let want = Want {
            query,
            option,
            kind,
            key,
        };

        let mut first = self.root;
        while self.nodes[first].level != 0 {
            first = self.nodes[first].child[0];
        }
        let mut out = Vec::new();
        self.scan_leaf(first, &want, &mut out);
        let mut node = first;
        while let Some(parent) = self.nodes[node].parent {
            let from = self.nodes[node].index_in_parent + 1;
            for &c in self.children(parent).iter().skip(from) {
                self.descend(c, &want, &mut out);
            }
            node = parent;
        }
        out
    }

    fn scan_leaf(&self, node: usize, want: &Want, out: &mut Vec<(Bounds, T)>) {
        for &c in self.children(node) {
            let e = &self.entries[c];
            if want.passes(&e.bounds, e.tags) {
                out.push((e.bounds, e.value));
            }
        }
    }

    fn descend(&self, node: usize, want: &Want, out: &mut Vec<(Bounds, T)>) {
        let n = &self.nodes[node];
        if !want.passes(&n.bounds, n.tags) {
            return;
        }
        if n.level == 0 {
            self.scan_leaf(node, want, out);
        } else {
            for &c in self.children(node) {
                self.descend(c, want, out);
            }
        }
    }

    pub fn outline(&self, name: &dyn Fn(T) -> String) -> String {
        let mut out = String::new();
        self.outline_node(self.root, name, &mut out);
        out
    }

    fn outline_node(&self, node: usize, name: &dyn Fn(T) -> String, out: &mut String) {
        let rect = |b: &Bounds| format!("({},{},{},{})", b.row0, b.row1, b.col0, b.col1);
        let n = &self.nodes[node];
        out.push_str(&rect(&n.bounds));
        out.push('[');
        for &c in self.children(node) {
            if n.level == 0 {
                let e = &self.entries[c];
                out.push_str(&rect(&e.bounds));
                out.push_str(&name(e.value));
                out.push(';');
            } else {
                self.outline_node(c, name, out);
            }
        }
        out.push(']');
    }
}

struct Want<'a> {
    query: Option<&'a Bounds>,
    option: Search,
    kind: u32,
    key: u64,
}

impl Want<'_> {
    fn passes(&self, b: &Bounds, tags: Tags) -> bool {
        if self.key & tags.key == 0 || (tags.kind != self.kind && self.kind & tags.kind == 0) {
            return false;
        }
        let whole = Bounds::new(0, LAST_ROW, 0, LAST_COL);
        let q = self.query.unwrap_or(&whole);
        match self.option {
            Search::Equal => b == q,
            Search::Overlaps => self.query.is_none() || b.intersection(q).is_some(),
            Search::Holds => b.contains(q),
            Search::Inside => q.contains(b),
            Search::OverlapsPartColumns => {
                b.intersection(q).is_some() && (b.row0 != 0 || b.row1 != LAST_ROW)
            }
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    const KIND: u32 = 8;

    fn put(t: &mut DfRtree<u32>, b: Bounds, v: u32) {
        t.insert(b, KIND, ANY_KEY, v);
    }

    fn find(t: &DfRtree<u32>, q: Option<&Bounds>, o: Search) -> Vec<u32> {
        let found = t.search(q, o, KIND, ANY_KEY);
        found.into_iter().map(|x| x.1).collect()
    }

    fn row(i: u32) -> Bounds {
        let r = i as i32;
        Bounds::new(r, r, 0, 0)
    }

    fn check(t: &DfRtree<u32>, node: usize) -> usize {
        let n = &t.nodes[node];
        assert!(n.count <= MAX);
        if node == t.root {
            assert_eq!(n.level, t.height);
        } else {
            assert!(n.count >= MIN, "{}", n.count);
        }
        let mut union: Option<Bounds> = None;
        let mut entries = 0;
        for (i, &c) in t.children(node).iter().enumerate() {
            let b = t.child_info(n.level, c).0;
            union.get_or_insert(b).union(&b);
            if n.level == 0 {
                entries += 1;
            } else {
                assert_eq!(t.nodes[c].parent, Some(node));
                assert_eq!(t.nodes[c].index_in_parent, i);
                assert_eq!(t.nodes[c].level + 1, n.level);
                entries += check(t, c);
            }
        }
        if let Some(u) = union {
            assert_eq!(n.bounds, u);
        }
        entries
    }

    #[test]
    fn sort_is_the_unstable_one_of_the_c_runtime() {
        let mut v = [(1, 'a'), (0, 'b'), (1, 'c')];
        xls_qsort(&mut v, |a, b| a.0.cmp(&b.0));
        assert_eq!(v, [(0, 'b'), (1, 'c'), (1, 'a')]);

        for n in 9..200u32 {
            let mut v: Vec<u32> = (0..n).map(|i| (i * 37 + 11) % 23).collect();
            let mut sorted = v.clone();
            sorted.sort_unstable();
            xls_qsort(&mut v, u32::cmp);
            assert_eq!(v, sorted, "{n}");
        }
    }

    #[test]
    fn the_twenty_first_entry_splits_a_root_leaf() {
        let mut t = DfRtree::default();
        for i in 0..20 {
            put(&mut t, row(i), i);
        }
        assert_eq!(t.height(), 0);
        put(&mut t, row(20), 20);
        assert_eq!((t.height(), t.len()), (1, 21));
        let counts: Vec<usize> = t
            .children(t.root)
            .iter()
            .map(|&c| t.nodes[c].count)
            .collect();
        assert_eq!(counts, [10, 11]);
        assert_eq!(check(&t, t.root), 21);
    }

    #[test]
    fn a_full_leaf_below_the_root_reinserts_before_it_splits() {
        let mut t = DfRtree::default();
        for i in 0..31 {
            put(&mut t, row(i), i);
        }

        assert_eq!(check(&t, t.root), 31);
        assert_eq!(t.height(), 1);
        let mut all = find(&t, None, Search::Overlaps);
        all.sort_unstable();
        assert_eq!(all, (0..31).collect::<Vec<_>>());
    }

    #[test]
    fn inside_search_misses_entries_below_a_node_that_sticks_out() {
        let mut t = DfRtree::default();
        for i in 0..200 {
            put(&mut t, row(i), i);
        }
        assert!(t.height() > 0);
        let mut missed = 0;
        for i in 0..200 {
            let q = row(i);
            assert_eq!(find(&t, Some(&q), Search::Overlaps), [i]);
            let inside = find(&t, Some(&q), Search::Inside);
            assert!(inside.is_empty() || inside == [i]);

            assert_eq!(find(&t, Some(&q), Search::Equal), inside);
            missed += usize::from(inside.is_empty());
        }

        let mut first = t.root;
        while t.nodes[first].level != 0 {
            first = t.nodes[first].child[0];
        }
        assert_eq!(missed, 200 - t.nodes[first].count);
    }

    #[test]
    fn options_compare_the_entry_with_the_query() {
        let mut t = DfRtree::default();
        let q = Bounds::new(2, 4, 2, 4);
        put(&mut t, Bounds::new(2, 4, 2, 4), 0);
        put(&mut t, Bounds::new(0, 9, 0, 9), 1);
        put(&mut t, Bounds::new(3, 3, 3, 3), 2);
        put(&mut t, Bounds::new(4, 8, 4, 8), 3);
        put(&mut t, Bounds::new(0, LAST_ROW, 3, 3), 4);
        put(&mut t, Bounds::new(20, 20, 20, 20), 5);
        assert_eq!(find(&t, Some(&q), Search::Equal), [0]);
        assert_eq!(find(&t, Some(&q), Search::Overlaps), [0, 1, 2, 3, 4]);
        assert_eq!(find(&t, Some(&q), Search::Holds), [0, 1]);
        assert_eq!(find(&t, Some(&q), Search::Inside), [0, 2]);
        assert_eq!(
            find(&t, Some(&q), Search::OverlapsPartColumns),
            [0, 1, 2, 3]
        );
        assert_eq!(find(&t, None, Search::Overlaps), [0, 1, 2, 3, 4, 5]);
        assert!(t.search(Some(&q), Search::Overlaps, 4, ANY_KEY).is_empty());
        assert!(t.search(Some(&q), Search::Overlaps, KIND, 0).is_empty());
    }

    #[test]
    fn removal_gives_the_leaf_place_to_the_last_child() {
        let mut t = DfRtree::default();
        for i in 0..5 {
            put(&mut t, row(i), i);
        }
        assert!(t.remove(&row(1), 1));
        assert_eq!(find(&t, None, Search::Overlaps), [0, 4, 2, 3]);
        assert!(!t.remove(&row(1), 1));
        assert!(!t.remove(&row(2), 3));
        assert_eq!(t.len(), 4);
    }

    #[test]
    fn inserts_and_removals_keep_the_tree_whole() {
        let rect = |i: u32| {
            let r = (i * 7919 % 1009) as i32;
            let c = (i * 104_729 % 53) as i32;
            Bounds::new(r, r + (i % 5) as i32, c, c + (i % 3) as i32)
        };
        let mut t = DfRtree::default();
        let mut live = Vec::new();
        for i in 0..1500u32 {
            put(&mut t, rect(i), i);
            live.push(i);
            if i % 3 == 0 {
                let gone = live.remove((i as usize * 31) % live.len());
                assert!(t.remove(&rect(gone), gone));
            }
            assert_eq!(check(&t, t.root), live.len());
        }
        assert!(t.height() >= 2);
        while let Some(gone) = live.pop() {
            assert!(t.remove(&rect(gone), gone));
            assert_eq!(check(&t, t.root), live.len());
            assert_eq!(find(&t, None, Search::Overlaps).len(), live.len());
        }
        assert!(t.is_empty());
        assert_eq!(t.height(), 0);
    }
}
