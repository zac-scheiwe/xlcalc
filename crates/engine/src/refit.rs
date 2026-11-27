use crate::address::{CellAddr, Rect};
use crate::style::{BorderLine, HAlign, Style, VAlign};
use crate::value::Value;
use crate::workbook::Workbook;

const SNAP_CELLS: u64 = 10_000;

fn breaks_lines(s: &Style) -> bool {
    let a = &s.alignment;
    a.wrap
        || a.rotation != 0
        || matches!(a.horizontal, HAlign::Justify | HAlign::Distributed)
        || matches!(a.vertical, VAlign::Justify | VAlign::Distributed)
}

pub fn xf_ifnt_changed(old: &Style, new: &Style) -> bool {
    old.font != new.font
        || old.alignment.vertical != new.alignment.vertical
        || breaks_lines(old)
        || breaks_lines(new)
}

fn border_dy(l: BorderLine) -> u8 {
    match l {
        BorderLine::None
        | BorderLine::Thin
        | BorderLine::Dashed
        | BorderLine::Dotted
        | BorderLine::DashDot
        | BorderLine::DashDotDot => 1,
        BorderLine::Hair => 0,
        BorderLine::Medium
        | BorderLine::MediumDashed
        | BorderLine::MediumDashDot
        | BorderLine::MediumDashDotDot
        | BorderLine::SlantDashDot => 2,
        BorderLine::Thick | BorderLine::Double => 3,
    }
}

pub fn border_gate(old: &Style, new: &Style) -> bool {
    let (ot, ob) = (old.borders.top.line, old.borders.bottom.line);
    let (nt, nb) = (new.borders.top.line, new.borders.bottom.line);
    (ot != nt || ob != nb) && (border_dy(ot) != border_dy(nt) || border_dy(ob) != border_dy(nb))
}

#[derive(Clone, Copy, PartialEq, Eq, Debug)]
pub enum RefitTest {
    Xf { font_put: bool },

    HorizontalBorder,
}

pub struct FormatSnap {
    rows: (u32, u32),

    cells: Vec<(CellAddr, Style)>,

    corner: (CellAddr, Style),
}

impl Workbook {
    pub fn format_snapshot(&self, sheet: crate::address::SheetId, rect: Rect) -> FormatSnap {
        let corner = CellAddr::new(sheet, rect.r0, rect.c0);
        let clip = self
            .used_range(sheet)
            .and_then(|u| rect.intersection(&u))
            .filter(|c| u64::from(c.r1 - c.r0 + 1) * u64::from(c.c1 - c.c0 + 1) <= SNAP_CELLS);
        let mut cells = Vec::new();
        if let Some(c) = clip {
            for r in c.r0..=c.r1 {
                for col in c.c0..=c.c1 {
                    let a = CellAddr::new(sheet, r, col);
                    cells.push((a, self.cell_style(a).clone()));
                }
            }
        }
        FormatSnap {
            rows: (rect.r0, rect.r1),
            cells,
            corner: (corner, self.cell_style(corner).clone()),
        }
    }

    pub fn refit_rows_after(&self, snap: &FormatSnap, test: RefitTest) -> Vec<u32> {
        let changed = |old: &Style, new: &Style| match test {
            RefitTest::Xf { .. } => xf_ifnt_changed(old, new),
            RefitTest::HorizontalBorder => border_gate(old, new),
        };
        let font_put = test == RefitTest::Xf { font_put: true };
        let mut rows: Vec<u32> = Vec::new();
        for (a, old) in &snap.cells {
            let text = font_put && matches!(self.value(*a), Value::Text(_));
            if (text || changed(old, self.cell_style(*a))) && !rows.contains(&a.row) {
                rows.push(a.row);
            }
        }
        let (c, old) = &snap.corner;
        if changed(old, self.cell_style(*c)) {
            let (r0, r1) = snap.rows;
            let cap = r1.min(r0.saturating_add(10_000));
            let fresh: Vec<u32> = (r0..=cap).filter(|r| !rows.contains(r)).collect();
            rows.extend(fresh);
        }
        rows
    }
}
