use crate::address::SheetId;
use crate::drawing::{pt_to_emu, Emu4, Shape, ShapeKind, SheetDrawing};
use crate::workbook::Workbook;

impl SheetDrawing {
    pub fn block(&self, id: u32) -> Vec<u32> {
        let mut out = Vec::new();
        self.collect_block(id, &mut out);
        out
    }

    fn collect_block(&self, id: u32, out: &mut Vec<u32>) {
        out.push(id);
        if let Some(Shape {
            kind: ShapeKind::Group(kids),
            ..
        }) = self.get(id)
        {
            for k in kids {
                self.collect_block(*k, out);
            }
        }
    }

    pub fn leaves(&self, id: u32) -> Vec<u32> {
        self.block(id)
            .into_iter()
            .filter(|i| {
                self.get(*i)
                    .is_some_and(|s| !matches!(s.kind, ShapeKind::Group(_)))
            })
            .collect()
    }

    pub fn listed_leaves(&self, id: u32) -> Vec<u32> {
        match self.listing.get(&id) {
            Some(l) => l
                .iter()
                .copied()
                .filter(|i| self.get(*i).is_some())
                .collect(),
            None => self.leaves(id),
        }
    }

    pub fn list_group(&mut self, id: u32) {
        if !self.listing.contains_key(&id) {
            let l = self.leaves(id);
            self.listing.insert(id, l);
        }
    }

    pub fn top_group(&self, id: u32) -> Option<u32> {
        let mut cur = self.get(id)?.parent?;
        while let Some(p) = self.get(cur).and_then(|s| s.parent) {
            cur = p;
        }
        Some(cur)
    }
}

impl Workbook {
    pub fn refresh_groups(&mut self, sheet: SheetId) {
        let Some(d) = self.drawing(sheet) else {
            return;
        };
        let mut updates = Vec::new();
        for g in d
            .shapes
            .iter()
            .filter(|s| matches!(s.kind, ShapeKind::Group(_)))
        {
            let mut bb: Option<Emu4> = None;
            for l in d.leaves(g.id) {
                let Some(s) = d.get(l) else { continue };
                let (x, y, w, h) = self.shape_rect_emu(sheet, &s.pos);
                bb = Some(match bb {
                    None => (x, y, w, h),
                    Some((bx, by, bw, bh)) => {
                        let (nx, ny) = (bx.min(x), by.min(y));
                        let (x2, y2) = ((bx + bw).max(x + w), (by + bh).max(y + h));
                        (nx, ny, x2 - nx, y2 - ny)
                    }
                });
            }
            if let Some(r) = bb {
                updates.push((g.id, self.pos_for(sheet, g.pos.placement(), r)));
            }
        }
        let d = self.drawing_mut(sheet);
        for (id, pos) in updates {
            if let Some(s) = d.get_mut(id) {
                s.pos = pos;
            }
        }
    }

    pub fn group_shapes(&mut self, sheet: SheetId, ids: &[u32]) -> Option<u32> {
        let d = self.drawing(sheet)?;
        let mut members: Vec<(usize, u32)> = Vec::new();
        for id in ids {
            let i = d.index_of(*id)?;
            if d.shapes[i].parent.is_some() {
                return None;
            }
            if !members.iter().any(|m| m.1 == *id) {
                members.push((i, *id));
            }
        }
        if members.len() < 2 {
            return None;
        }
        members.sort();
        let mut moved: Vec<u32> = Vec::new();
        for m in &members {
            moved.extend(d.block(m.1));
        }
        let top = members.last()?.0;
        let before = d.shapes[..top]
            .iter()
            .filter(|s| moved.contains(&s.id))
            .count();
        let gid = d.next_id;
        let d = self.drawing_mut(sheet);
        d.next_id += 1;
        let mut blocks: Vec<Shape> = Vec::new();
        let mut rest: Vec<Shape> = Vec::new();
        for s in std::mem::take(&mut d.shapes) {
            if moved.contains(&s.id) {
                blocks.push(s);
            } else {
                rest.push(s);
            }
        }
        let mut group = blocks[0].clone();
        group.id = gid;
        group.name = format!("Group {}", gid - 1);
        group.kind = ShapeKind::Group(members.iter().map(|m| m.1).collect());
        group.text = String::new();
        group.chart = None;
        group.flip_h = false;
        group.flip_v = false;
        group.rotation = 0.0;
        group.alt_text = String::new();
        group.visible = true;
        group.parent = None;
        for b in &mut blocks {
            if members.iter().any(|m| m.1 == b.id) {
                b.parent = Some(gid);
            }
        }
        let at = top - before;
        rest.splice(at..at, std::iter::once(group).chain(blocks));
        d.shapes = rest;
        self.set_shape_placement(sheet, gid, 1);
        self.refresh_groups(sheet);
        Some(gid)
    }

    pub fn ungroup_shape(&mut self, sheet: SheetId, id: u32) -> Option<Vec<u32>> {
        let d = self.drawing_mut(sheet);
        let i = d.index_of(id)?;
        let ShapeKind::Group(kids) = d.shapes[i].kind.clone() else {
            return None;
        };
        let outer = d.shapes[i].parent;
        d.listing.remove(&id);
        d.shapes.remove(i);
        for k in &kids {
            if let Some(s) = d.get_mut(*k) {
                s.parent = outer;
            }
        }
        if let Some(Shape {
            kind: ShapeKind::Group(sib),
            ..
        }) = outer.and_then(|o| d.get_mut(o))
        {
            let mut v = Vec::new();
            for x in sib.iter() {
                if *x == id {
                    v.extend(kids.iter().copied());
                } else {
                    v.push(*x);
                }
            }
            *sib = v;
        }
        Some(kids)
    }

    pub fn set_group_rect(&mut self, sheet: SheetId, id: u32, r: Emu4) {
        let Some(d) = self.drawing(sheet) else {
            return;
        };
        let Some(g) = d.get(id) else { return };
        let (ox, oy, ow, oh) = self.shape_rect_emu(sheet, &g.pos);
        let place = |v: i64, o: i64, n: i64, ol: i64, nl: i64| -> i64 {
            if ol == 0 {
                n + (v - o)
            } else {
                n + ((v - o) as f64 * nl as f64 / ol as f64).round() as i64
            }
        };
        let size = |v: i64, ol: i64, nl: i64| -> i64 {
            if ol == 0 {
                v
            } else {
                (v as f64 * nl as f64 / ol as f64).round() as i64
            }
        };
        let mut updates = Vec::new();
        for l in d.leaves(id) {
            let Some(s) = d.get(l) else { continue };
            let (x, y, w, h) = self.shape_rect_emu(sheet, &s.pos);
            let rect = (
                place(x, ox, r.0, ow, r.2),
                place(y, oy, r.1, oh, r.3),
                size(w, ow, r.2),
                size(h, oh, r.3),
            );
            updates.push((l, self.pos_for(sheet, s.pos.placement(), rect)));
        }
        let d = self.drawing_mut(sheet);
        for (l, pos) in updates {
            if let Some(s) = d.get_mut(l) {
                s.pos = pos;
            }
        }
        self.refresh_groups(sheet);
    }

    pub fn delete_shape(&mut self, sheet: SheetId, id: u32) {
        let d = self.drawing_mut(sheet);
        let Some(parent) = d.get(id).map(|s| s.parent) else {
            return;
        };
        let gone = d.block(id);
        d.shapes.retain(|s| !gone.contains(&s.id));
        if let Some(p) = parent {
            let empty = match d.get_mut(p) {
                Some(Shape {
                    kind: ShapeKind::Group(kids),
                    ..
                }) => {
                    kids.retain(|k| *k != id);
                    kids.is_empty()
                }
                _ => false,
            };
            if empty {
                self.delete_shape(sheet, p);
                return;
            }
        }
        self.refresh_groups(sheet);
    }

    pub fn duplicate_group(
        &mut self,
        sheet: SheetId,
        id: u32,
        base: fn(&ShapeKind) -> &'static str,
    ) -> Option<u32> {
        let d = self.drawing(sheet)?;
        let block = d.block(id);
        let off = pt_to_emu(12.0);
        let first = d.next_id;
        let map: Vec<(u32, u32)> = block
            .iter()
            .enumerate()
            .map(|(i, o)| (*o, first + i as u32))
            .collect();
        let new_id = |o: u32| map.iter().find(|m| m.0 == o).map(|m| m.1);
        let mut copies = Vec::new();
        for o in &block {
            let mut c = d.get(*o)?.clone();
            let n = new_id(*o)?;
            if c.name == format!("{} {}", base(&c.kind), c.id - 1) {
                c.name = format!("{} {}", base(&c.kind), n - 1);
            }
            c.id = n;
            c.parent = if *o == id {
                None
            } else {
                c.parent.and_then(new_id)
            };
            if let ShapeKind::Group(k) = &mut c.kind {
                *k = k.iter().filter_map(|x| new_id(*x)).collect();
            } else {
                let (x, y, w, h) = self.shape_rect_emu(sheet, &c.pos);
                c.pos = self.pos_for(sheet, c.pos.placement(), (x + off, y + off, w, h));
            }
            copies.push(c);
        }
        let d = self.drawing_mut(sheet);
        d.next_id = first + block.len() as u32;
        d.shapes.extend(copies);
        self.refresh_groups(sheet);
        Some(first)
    }

    pub fn z_order(&mut self, sheet: SheetId, id: u32, cmd: i32) {
        let d = self.drawing_mut(sheet);
        let Some(start) = d.index_of(id) else { return };
        let end = start + d.block(id).len();
        let parent = d.shapes[start].parent;
        let plen = parent.map_or(0, |p| d.block(p).len());
        let moved: Vec<Shape> = d.shapes.drain(start..end).collect();

        let pidx = parent.and_then(|p| d.index_of(p));
        let at = match cmd {
            0 if parent.is_none() => d.shapes.len(),
            1 if parent.is_none() => 0,
            0 if pidx.is_some() => pidx.map_or(start, |i| i + plen - moved.len()),
            1 if pidx.is_some() => pidx.map_or(start, |i| i + 1),
            2 if start < d.shapes.len() && d.shapes[start].parent == parent => {
                start + d.block(d.shapes[start].id).len()
            }
            3 => (0..start)
                .rev()
                .find(|i| d.shapes[*i].parent == parent)
                .unwrap_or(start),
            _ => start,
        };
        d.shapes.splice(at..at, moved);

        if let Some(p) = parent {
            let order: Vec<u32> = d.shapes.iter().map(|s| s.id).collect();
            if let Some(Shape {
                kind: ShapeKind::Group(kids),
                ..
            }) = d.shapes.iter_mut().find(|s| s.id == p)
            {
                kids.sort_by_key(|k| order.iter().position(|o| o == k));
            }
        }
    }
}
