use xlcalc::drawing::ShapeKind;
use xlcalc::{CellAddr, Rect, SheetId};
use xlcalc_variant::Variant;

use crate::addr::{max_cols, max_rows};
use crate::app::{Application, BookId};
use crate::error::OmError;
use crate::shapes::{base_name, Item};

const TILE_CELLS: u64 = 0xffff;

fn places(src: Rect, area: Rect, same_sheet: bool) -> Vec<(i64, i64)> {
    let first = (
        i64::from(area.r0) - i64::from(src.r0),
        i64::from(area.c0) - i64::from(src.c0),
    );
    let tiled = u64::from(area.rows()) * u64::from(area.cols()) < TILE_CELLS;
    let (down, across) = if tiled {
        (area.rows() / src.rows(), area.cols() / src.cols())
    } else {
        (1, 1)
    };
    (0..across)
        .flat_map(|c| (0..down).map(move |r| (r, c)))
        .map(|(r, c)| {
            (
                first.0 + i64::from(r * src.rows()),
                first.1 + i64::from(c * src.cols()),
            )
        })
        .filter(|&by| !(same_sheet && by == (0, 0)))
        .collect()
}

impl Application<'_> {
    pub(crate) fn carry_objects(
        &mut self,
        b: BookId,
        (ss, src): (SheetId, Rect),
        (ds, area): (SheetId, Rect),
        cut: bool,
    ) -> Result<(), OmError> {
        let off = matches!(
            self.opts.get("0:CopyObjectsWithCells"),
            Some(Variant::Bool(false) | Variant::I4(0))
        );
        if off
            || self.book(b)?.draw_objects == 3
            || area.rows() < src.rows()
            || area.cols() < src.cols()
            || self.sheet_prot(b, ds).objects
        {
            return Ok(());
        }
        let by = (
            i64::from(area.r0) - i64::from(src.r0),
            i64::from(area.c0) - i64::from(src.c0),
        );
        let picked = self.enclosed_objects(b, (ss, src), by)?;
        if picked.is_empty() {
            return Ok(());
        }
        if !cut {
            for place in places(src, area, ss == ds) {
                for &item in &picked {
                    self.duplicate_object(b, (ss, item), ds, place)?;
                }
            }
        } else if ss != ds {
            let husks = self.husks_left(b, ss, &picked)?;
            for &item in &picked {
                self.duplicate_object(b, (ss, item), ds, by)?;
            }
            for &item in &picked {
                self.drop_object(b, ss, item)?;
            }
            if husks > 0 {
                self.book_mut(b)?.controls.entry(ss).or_default().husks += husks;
            }
        } else if by != (0, 0) {
            for &item in &picked {
                self.shift_object(b, ss, item, by)?;
            }
        }
        Ok(())
    }

    pub(crate) fn carry_extract(
        &mut self,
        b: BookId,
        copied: &[(CellAddr, CellAddr)],
    ) -> Result<(), OmError> {
        let Some(s) = copied.first().map(|c| c.0.sheet) else {
            return Ok(());
        };
        let bk = self.book(b)?;
        let controls = bk.controls.get(&s).is_some_and(|c| !c.list.is_empty());
        if !controls && bk.wb.notes(s).next().is_none() {
            return Ok(());
        }

        let items = self.merged_items(b, s)?;
        let bk = self.book(b)?;
        let spans: Vec<_> = items
            .iter()
            .filter_map(|&(item, _)| match item {
                Item::Shape(id) => bk.wb.drawing(s).and_then(|d| d.get(id)).map(|sh| sh.pos),
                Item::Control(uid) => bk
                    .controls
                    .get(&s)
                    .and_then(|c| c.list.iter().find(|c| c.uid == uid))
                    .and_then(|c| bk.wb.control_anchor(s, c.spid)),
                Item::Note(_) => None,
            })
            .map(|pos| bk.wb.object_span(s, &pos))
            .collect();
        for &(from, to) in copied {
            let on = spans.iter().any(|&(r0, c0, r1, c1)| {
                (r0..=r1).contains(&from.row) && (c0..=c1).contains(&from.col)
            });
            if on {
                let cell = |a: CellAddr| (a.sheet, Rect::cell(a.row, a.col));
                self.carry_objects(b, cell(from), cell(to), false)?;
            }
        }
        Ok(())
    }

    fn enclosed_objects(
        &mut self,
        b: BookId,
        (s, src): (SheetId, Rect),
        by: (i64, i64),
    ) -> Result<Vec<Item>, OmError> {
        let items = self.merged_items(b, s)?;
        let protected = self.sheet_prot(b, s).objects;
        let bk = self.book(b)?;
        let on = |i: u32, d: i64, max: u32| (0..i64::from(max)).contains(&(i64::from(i) + d));
        let mut out = Vec::new();
        for (item, _) in items {
            let found = match item {
                Item::Shape(id) => bk.wb.drawing(s).and_then(|d| d.get(id)).and_then(|sh| {
                    let slicer = matches!(sh.kind, ShapeKind::Slicer | ShapeKind::Timeline);
                    let takes = !slicer && !(protected && sh.locked) && sh.pos.placement() != 3;
                    takes.then_some(sh.pos)
                }),
                Item::Control(uid) => bk
                    .controls
                    .get(&s)
                    .and_then(|c| c.list.iter().find(|c| c.uid == uid))
                    .filter(|c| c.c.placement != 3 && !(protected && c.c.locked))
                    .and_then(|c| bk.wb.control_anchor(s, c.spid)),
                Item::Note(_) => None,
            };
            let Some(pos) = found else {
                continue;
            };
            let (r0, c0, r1, c1) = bk.wb.object_span(s, &pos);
            let near = r0 + 1 >= src.r0 && c0 + 1 >= src.c0 && r1 <= src.r1 + 1 && c1 <= src.c1 + 1;
            let touches = r0 <= src.r1 && r1 >= src.r0 && c0 <= src.c1 && c1 >= src.c0;
            let stays = on(r0, by.0, max_rows())
                && on(r1, by.0, max_rows())
                && on(c0, by.1, max_cols())
                && on(c1, by.1, max_cols());
            if near && touches && stays {
                out.push(item);
            }
        }
        Ok(out)
    }

    fn husks_left(&self, b: BookId, s: SheetId, picked: &[Item]) -> Result<usize, OmError> {
        let bk = self.book(b)?;
        let mut hidden = 0;
        let mut shown = false;
        for &item in picked {
            match item {
                Item::Shape(id) => {
                    shown |= bk
                        .wb
                        .drawing(s)
                        .and_then(|d| d.get(id))
                        .is_some_and(|sh| sh.visible);
                }
                Item::Control(uid) => {
                    let visible = bk
                        .controls
                        .get(&s)
                        .and_then(|c| c.list.iter().find(|c| c.uid == uid))
                        .is_some_and(|c| c.visible);
                    shown |= visible;
                    hidden += usize::from(!visible);
                }
                Item::Note(_) => {}
            }
        }
        Ok(if shown { hidden } else { 0 })
    }

    fn duplicate_object(
        &mut self,
        b: BookId,
        (from, item): (SheetId, Item),
        to: SheetId,
        by: (i64, i64),
    ) -> Result<(), OmError> {
        match item {
            Item::Shape(id) => {
                self.draw_wb(b)?.carry_shape((from, id), to, by, base_name);
            }
            Item::Control(uid) => self.duplicate_control(b, (from, uid), to, by)?,
            Item::Note(_) => {}
        }
        Ok(())
    }

    fn drop_object(&mut self, b: BookId, s: SheetId, item: Item) -> Result<(), OmError> {
        match item {
            Item::Shape(id) => self.draw_wb(b)?.delete_shape(s, id),
            Item::Control(uid) => {
                if let Some(sc) = self.book_mut(b)?.controls.get_mut(&s) {
                    sc.list.retain(|c| c.uid != uid);
                }
            }
            Item::Note(_) => {}
        }
        Ok(())
    }

    fn shift_object(
        &mut self,
        b: BookId,
        s: SheetId,
        item: Item,
        by: (i64, i64),
    ) -> Result<(), OmError> {
        match item {
            Item::Shape(id) => self.draw_wb(b)?.shift_shape(s, id, by.0, by.1),
            Item::Control(uid) => {
                let bk = self.book_mut(b)?;
                let spid = bk
                    .controls
                    .get(&s)
                    .and_then(|c| c.list.iter().find(|c| c.uid == uid))
                    .map(|c| c.spid);
                if let Some(spid) = spid {
                    if let Some(pos) = bk.wb.control_anchor(s, spid) {
                        bk.wb.control_anchor_put(s, spid, pos.shifted(by.0, by.1));
                    }
                }
            }
            Item::Note(_) => {}
        }
        Ok(())
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn tiles_go_down_then_across_and_skip_the_source() {
        let src = Rect::new(0, 0, 1, 0);
        let area = Rect::new(0, 0, 5, 1);
        assert_eq!(
            places(src, area, true),
            [(2, 0), (4, 0), (0, 1), (2, 1), (4, 1)]
        );
        assert_eq!(places(src, area, false).len(), 6);

        assert_eq!(places(src, Rect::new(2, 0, 70_001, 0), true), [(2, 0)]);
    }
}
