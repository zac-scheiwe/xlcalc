use crate::address::{SheetId, MAX_COLS, MAX_ROWS};
use crate::drawing::{pt_to_emu, Anchor, Emu4, Pos, Shape, ShapeKind};
use crate::workbook::Workbook;

pub type CellSpan = (u32, u32, u32, u32);

fn shifted(a: Anchor, rows: i64, cols: i64) -> Anchor {
    let go = |i: u32, d: i64, max: u32| (i64::from(i) + d).clamp(0, i64::from(max) - 1) as u32;
    Anchor {
        row: go(a.row, rows, MAX_ROWS),
        col: go(a.col, cols, MAX_COLS),
        ..a
    }
}

impl Pos {
    #[must_use]
    pub fn shifted(self, rows: i64, cols: i64) -> Pos {
        match self {
            Pos::Both { from, to } => Pos::Both {
                from: shifted(from, rows, cols),
                to: shifted(to, rows, cols),
            },
            Pos::One { from, w, h } => Pos::One {
                from: shifted(from, rows, cols),
                w,
                h,
            },
            abs @ Pos::Abs { .. } => abs,
        }
    }
}

impl Workbook {
    pub fn object_span(&self, sheet: SheetId, pos: &Pos) -> CellSpan {
        let (x, y, w, h) = self.shape_rect_emu(sheet, pos);
        let (from, to) = match pos {
            Pos::Both { from, to } => (*from, *to),
            Pos::One { from, .. } => (*from, self.xy_anchor(sheet, x + w, y + h)),
            Pos::Abs { .. } => (
                self.xy_anchor(sheet, x, y),
                self.xy_anchor(sheet, x + w, y + h),
            ),
        };
        let last = |i: u32, off: i64, first: u32| {
            if off == 0 {
                i.saturating_sub(1).max(first)
            } else {
                i
            }
        };
        (
            from.row,
            from.col,
            last(to.row, to.row_off, from.row),
            last(to.col, to.col_off, from.col),
        )
    }

    pub fn control_anchor_set(
        &mut self,
        sheet: SheetId,
        spid: u32,
        placement: i32,
        rect: [f64; 4],
    ) {
        let pos = self.pos_for(sheet, placement, rect_emu(rect));
        self.control_anchors.insert((sheet, spid), pos);
    }

    pub fn shape_corners(&self, sheet: SheetId, pos: &Pos) -> [Anchor; 2] {
        let (x, y, w, h) = self.shape_rect_emu(sheet, pos);
        match pos {
            Pos::Both { from, to } => [*from, *to],
            Pos::One { from, .. } => [*from, self.xy_anchor(sheet, x + w, y + h)],
            Pos::Abs { .. } => [
                self.xy_anchor(sheet, x, y),
                self.xy_anchor(sheet, x + w, y + h),
            ],
        }
    }

    pub fn pos_between(&self, sheet: SheetId, placement: i32, [from, to]: [Anchor; 2]) -> Pos {
        let (x, y) = self.anchor_xy(sheet, &from);
        let (x2, y2) = self.anchor_xy(sheet, &to);
        let (w, h) = ((x2 - x).max(0), (y2 - y).max(0));
        match placement {
            1 => Pos::Both { from, to },
            2 => Pos::One { from, w, h },
            _ => Pos::Abs { x, y, w, h },
        }
    }

    pub fn control_anchor_put(&mut self, sheet: SheetId, spid: u32, pos: Pos) {
        self.control_anchors.insert((sheet, spid), pos);
    }

    pub fn control_anchor(&self, sheet: SheetId, spid: u32) -> Option<Pos> {
        self.control_anchors.get(&(sheet, spid)).copied()
    }

    pub fn control_rect(&self, sheet: SheetId, pos: &Pos, rect: [f64; 4]) -> [f64; 4] {
        let now = self.shape_rect_emu(sheet, pos);
        if now == rect_emu(rect) {
            return rect;
        }
        let pt = |v: i64| v as f64 / 12700.0;
        [pt(now.0), pt(now.1), pt(now.2), pt(now.3)]
    }

    pub fn shift_shape(&mut self, sheet: SheetId, id: u32, rows: i64, cols: i64) {
        let Some(d) = self.drawing(sheet) else {
            return;
        };
        let block = d.block(id);
        for sh in &mut self.drawing_mut(sheet).shapes {
            if block.contains(&sh.id) {
                sh.pos = sh.pos.shifted(rows, cols);
            }
        }
        self.refresh_groups(sheet);
    }

    pub fn carry_shape(
        &mut self,
        (from, id): (SheetId, u32),
        to: SheetId,
        (rows, cols): (i64, i64),
        base: impl Fn(&ShapeKind) -> &'static str,
    ) -> Option<u32> {
        let d = self.drawing(from)?;
        let block: Vec<Shape> = d
            .block(id)
            .into_iter()
            .filter_map(|i| d.get(i).cloned())
            .collect();
        let next = self.drawing_mut(to).next_id;
        let ids: Vec<(u32, u32)> = block.iter().map(|s| s.id).zip(next..).collect();
        let renamed = |old: u32| ids.iter().find(|i| i.0 == old).map(|i| i.1);
        let first = renamed(id)?;
        let copies: Vec<Shape> = block
            .into_iter()
            .filter_map(|mut s| {
                let nid = renamed(s.id)?;
                let kind = base(&s.kind);
                if s.name == format!("{kind} {}", s.id.wrapping_sub(1)) {
                    s.name = format!("{kind} {}", nid - 1);
                }
                s.id = nid;
                s.pos = s.pos.shifted(rows, cols);
                s.parent = s.parent.and_then(renamed);
                if let ShapeKind::Group(kids) = &mut s.kind {
                    *kids = kids.iter().filter_map(|k| renamed(*k)).collect();
                }
                Some(s)
            })
            .collect();
        let d = self.drawing_mut(to);
        d.next_id += copies.len() as u32;
        d.shapes.extend(copies);
        self.refresh_groups(to);
        Some(first)
    }
}

fn rect_emu(r: [f64; 4]) -> Emu4 {
    (
        pt_to_emu(r[0]),
        pt_to_emu(r[1]),
        pt_to_emu(r[2]),
        pt_to_emu(r[3]),
    )
}
