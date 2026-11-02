use rustc_hash::FxHashMap;

use crate::address::{Rect, SheetId};
use crate::moves::{Adj, Move};
use crate::structure::Shift;
use crate::workbook::Workbook;

#[derive(Clone, Copy, Debug, PartialEq, Eq, Hash)]
pub struct TrackId(u32);

#[derive(Default)]
pub(crate) struct Tracks {
    next: u32,
    live: FxHashMap<u32, (SheetId, Rect)>,
}

impl Tracks {
    fn adjust(&mut self, sheet: SheetId, f: impl Fn(&Rect) -> Option<Rect>) {
        self.live.retain(|_, (s, r)| {
            if *s != sheet {
                return true;
            }
            match f(r) {
                Some(n) => {
                    *r = n;
                    true
                }
                None => false,
            }
        });
    }
}

impl Workbook {
    pub fn track(&mut self, sheet: SheetId, rect: Rect) -> TrackId {
        let id = self.tracks.next;
        self.tracks.next += 1;
        self.tracks.live.insert(id, (sheet, rect));
        TrackId(id)
    }

    pub fn tracked(&self, id: TrackId) -> Option<(SheetId, Rect)> {
        self.tracks.live.get(&id.0).copied()
    }

    pub fn untrack(&mut self, id: TrackId) {
        self.tracks.live.remove(&id.0);
    }

    pub(crate) fn track_shift(&mut self, s: &Shift) {
        self.tracks.adjust(s.sheet, |r| s.adjust_rect(r));
    }

    pub(crate) fn track_move(&mut self, mv: &Move) {
        self.tracks.live.retain(|_, (s, r)| match mv.adjust(*s, r) {
            Adj::Keep => true,
            Adj::To(n) | Adj::Moved(n) => {
                *r = n;
                true
            }
            Adj::Dead => false,
        });
    }

    pub(crate) fn track_drop_sheet(&mut self, sheet: SheetId) {
        self.tracks.live.retain(|_, (s, _)| *s != sheet);
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::address::{parse_a1_rect, CellAddr};
    use crate::sort::{SortKey, SortSpec};
    use crate::value::Value;

    enum St {
        InsRows(u32, u32),
        DelRows(u32, u32),
        InsCols(u32, u32),
        DelCols(u32, u32),

        InsCells(&'static str, bool),

        DelCells(&'static str, bool),

        Cut(&'static str, bool, &'static str),
        Sort(&'static str),
        DelSheet,
        Rename,
        MoveSheet,
    }

    const HELD: [&str; 6] = ["B5:C7", "F2", "E10:F12", "A1:A3", "H3:J5", "B15:F16"];

    fn a1(s: &str) -> Rect {
        parse_a1_rect(s).unwrap()
    }

    #[test]
    fn held_ranges_follow_excel() {
        type Case = (&'static str, &'static [St], [Option<&'static str>; 6]);
        use St::*;
        let cases: &[Case] = &[
            (
                "probe01 ins_row_before",
                &[St::InsRows(0, 1)],
                [
                    Some("B6:C8"),
                    Some("F3"),
                    Some("E11:F13"),
                    Some("A2:A4"),
                    Some("H4:J6"),
                    Some("B16:F17"),
                ],
            ),
            (
                "probe02 ins_row_at_top",
                &[St::InsRows(4, 1)],
                [
                    Some("B6:C8"),
                    Some("F2"),
                    Some("E11:F13"),
                    Some("A1:A3"),
                    Some("H3:J6"),
                    Some("B16:F17"),
                ],
            ),
            (
                "probe03 ins_row_inside",
                &[St::InsRows(5, 1)],
                [
                    Some("B5:C8"),
                    Some("F2"),
                    Some("E11:F13"),
                    Some("A1:A3"),
                    Some("H3:J5"),
                    Some("B16:F17"),
                ],
            ),
            (
                "probe04 ins_row_inside_x2",
                &[St::InsRows(6, 2)],
                [
                    Some("B5:C9"),
                    Some("F2"),
                    Some("E12:F14"),
                    Some("A1:A3"),
                    Some("H3:J5"),
                    Some("B17:F18"),
                ],
            ),
            (
                "probe05 ins_row_after",
                &[St::InsRows(7, 1)],
                [
                    Some("B5:C7"),
                    Some("F2"),
                    Some("E11:F13"),
                    Some("A1:A3"),
                    Some("H3:J5"),
                    Some("B16:F17"),
                ],
            ),
            (
                "probe06 del_row_before",
                &[St::DelRows(0, 1)],
                [
                    Some("B4:C6"),
                    Some("F1"),
                    Some("E9:F11"),
                    Some("A1:A2"),
                    Some("H2:J4"),
                    Some("B14:F15"),
                ],
            ),
            (
                "probe07 del_row_inside",
                &[St::DelRows(5, 1)],
                [
                    Some("B5:C6"),
                    Some("F2"),
                    Some("E9:F11"),
                    Some("A1:A3"),
                    Some("H3:J5"),
                    Some("B14:F15"),
                ],
            ),
            (
                "probe08 del_row_top_overlap",
                &[St::DelRows(3, 2)],
                [
                    Some("B4:C5"),
                    Some("F2"),
                    Some("E8:F10"),
                    Some("A1:A3"),
                    Some("H3:J3"),
                    Some("B13:F14"),
                ],
            ),
            (
                "probe09 del_row_bottom_overlap",
                &[St::DelRows(6, 2)],
                [
                    Some("B5:C6"),
                    Some("F2"),
                    Some("E8:F10"),
                    Some("A1:A3"),
                    Some("H3:J5"),
                    Some("B13:F14"),
                ],
            ),
            (
                "probe10 del_row_cover",
                &[St::DelRows(4, 3)],
                [
                    None,
                    Some("F2"),
                    Some("E7:F9"),
                    Some("A1:A3"),
                    Some("H3:J4"),
                    Some("B12:F13"),
                ],
            ),
            (
                "probe11 del_row_superset",
                &[St::DelRows(3, 5)],
                [
                    None,
                    Some("F2"),
                    Some("E5:F7"),
                    Some("A1:A3"),
                    Some("H3:J3"),
                    Some("B10:F11"),
                ],
            ),
            (
                "probe12 del_row_after",
                &[St::DelRows(8, 1)],
                [
                    Some("B5:C7"),
                    Some("F2"),
                    Some("E9:F11"),
                    Some("A1:A3"),
                    Some("H3:J5"),
                    Some("B14:F15"),
                ],
            ),
            (
                "probe13 ins_col_before",
                &[St::InsCols(0, 1)],
                [
                    Some("C5:D7"),
                    Some("G2"),
                    Some("F10:G12"),
                    Some("B1:B3"),
                    Some("I3:K5"),
                    Some("C15:G16"),
                ],
            ),
            (
                "probe14 ins_col_at_left",
                &[St::InsCols(1, 1)],
                [
                    Some("C5:D7"),
                    Some("G2"),
                    Some("F10:G12"),
                    Some("A1:A3"),
                    Some("I3:K5"),
                    Some("C15:G16"),
                ],
            ),
            (
                "probe15 ins_col_inside",
                &[St::InsCols(3, 1)],
                [
                    Some("B5:C7"),
                    Some("G2"),
                    Some("F10:G12"),
                    Some("A1:A3"),
                    Some("I3:K5"),
                    Some("B15:G16"),
                ],
            ),
            (
                "probe16 ins_col_after",
                &[St::InsCols(4, 1)],
                [
                    Some("B5:C7"),
                    Some("G2"),
                    Some("F10:G12"),
                    Some("A1:A3"),
                    Some("I3:K5"),
                    Some("B15:G16"),
                ],
            ),
            (
                "probe17 del_col_before",
                &[St::DelCols(0, 1)],
                [
                    Some("A5:B7"),
                    Some("E2"),
                    Some("D10:E12"),
                    None,
                    Some("G3:I5"),
                    Some("A15:E16"),
                ],
            ),
            (
                "probe18 del_col_inside",
                &[St::DelCols(3, 1)],
                [
                    Some("B5:C7"),
                    Some("E2"),
                    Some("D10:E12"),
                    Some("A1:A3"),
                    Some("G3:I5"),
                    Some("B15:E16"),
                ],
            ),
            (
                "probe19 del_col_left_overlap",
                &[St::DelCols(0, 2)],
                [
                    Some("A5:A7"),
                    Some("D2"),
                    Some("C10:D12"),
                    None,
                    Some("F3:H5"),
                    Some("A15:D16"),
                ],
            ),
            (
                "probe20 del_col_right_overlap",
                &[St::DelCols(2, 2)],
                [
                    Some("B5:B7"),
                    Some("D2"),
                    Some("C10:D12"),
                    Some("A1:A3"),
                    Some("F3:H5"),
                    Some("B15:D16"),
                ],
            ),
            (
                "probe21 del_col_cover",
                &[St::DelCols(1, 2)],
                [
                    None,
                    Some("D2"),
                    Some("C10:D12"),
                    Some("A1:A3"),
                    Some("F3:H5"),
                    Some("B15:D16"),
                ],
            ),
            (
                "probe22 del_col_superset",
                &[St::DelCols(0, 4)],
                [
                    None,
                    Some("B2"),
                    Some("A10:B12"),
                    None,
                    Some("D3:F5"),
                    Some("A15:B16"),
                ],
            ),
            (
                "probe23 del_col_after",
                &[St::DelCols(4, 1)],
                [
                    Some("B5:C7"),
                    Some("E2"),
                    Some("E10:E12"),
                    Some("A1:A3"),
                    Some("G3:I5"),
                    Some("B15:E16"),
                ],
            ),
            (
                "probe24 inscell_down_above",
                &[St::InsCells("B2:B3", true)],
                [
                    Some("B5:C7"),
                    Some("F2"),
                    Some("E10:F12"),
                    Some("A1:A3"),
                    Some("H3:J5"),
                    Some("B15:F16"),
                ],
            ),
            (
                "probe25 inscell_down_partial_cols",
                &[St::InsCells("B5:B6", true)],
                [
                    Some("B5:C7"),
                    Some("F2"),
                    Some("E10:F12"),
                    Some("A1:A3"),
                    Some("H3:J5"),
                    Some("B15:F16"),
                ],
            ),
            (
                "probe26 inscell_down_exact",
                &[St::InsCells("B5:C7", true)],
                [
                    Some("B8:C10"),
                    Some("F2"),
                    Some("E10:F12"),
                    Some("A1:A3"),
                    Some("H3:J5"),
                    Some("B15:F16"),
                ],
            ),
            (
                "probe27 inscell_down_fullwidth_above",
                &[St::InsCells("B1:C2", true)],
                [
                    Some("B7:C9"),
                    Some("F2"),
                    Some("E10:F12"),
                    Some("A1:A3"),
                    Some("H3:J5"),
                    Some("B15:F16"),
                ],
            ),
            (
                "probe28 inscell_right_partial_rows",
                &[St::InsCells("A5:A6", false)],
                [
                    Some("B5:C7"),
                    Some("F2"),
                    Some("E10:F12"),
                    Some("A1:A3"),
                    Some("H3:J5"),
                    Some("B15:F16"),
                ],
            ),
            (
                "probe29 inscell_right_left_of",
                &[St::InsCells("A5:A7", false)],
                [
                    Some("C5:D7"),
                    Some("F2"),
                    Some("E10:F12"),
                    Some("A1:A3"),
                    Some("H3:J5"),
                    Some("B15:F16"),
                ],
            ),
            (
                "probe30 inscell_right_inside",
                &[St::InsCells("C5", false)],
                [
                    Some("B5:C7"),
                    Some("F2"),
                    Some("E10:F12"),
                    Some("A1:A3"),
                    Some("H3:J5"),
                    Some("B15:F16"),
                ],
            ),
            (
                "probe31 delcell_up_above",
                &[St::DelCells("B2:B3", true)],
                [
                    Some("B5:C7"),
                    Some("F2"),
                    Some("E10:F12"),
                    Some("A1:A3"),
                    Some("H3:J5"),
                    Some("B15:F16"),
                ],
            ),
            (
                "probe32 delcell_up_partial_cols",
                &[St::DelCells("B5:B6", true)],
                [
                    Some("B5:C7"),
                    Some("F2"),
                    Some("E10:F12"),
                    Some("A1:A3"),
                    Some("H3:J5"),
                    Some("B15:F16"),
                ],
            ),
            (
                "probe33 delcell_up_one_cell_inside",
                &[St::DelCells("C6", true)],
                [
                    Some("B5:C7"),
                    Some("F2"),
                    Some("E10:F12"),
                    Some("A1:A3"),
                    Some("H3:J5"),
                    Some("B15:F16"),
                ],
            ),
            (
                "probe34 delcell_up_exact",
                &[St::DelCells("B5:C7", true)],
                [
                    None,
                    Some("F2"),
                    Some("E10:F12"),
                    Some("A1:A3"),
                    Some("H3:J5"),
                    Some("B15:F16"),
                ],
            ),
            (
                "probe35 delcell_up_fullwidth_above",
                &[St::DelCells("B1:C2", true)],
                [
                    Some("B3:C5"),
                    Some("F2"),
                    Some("E10:F12"),
                    Some("A1:A3"),
                    Some("H3:J5"),
                    Some("B15:F16"),
                ],
            ),
            (
                "probe36 delcell_left_partial_rows",
                &[St::DelCells("A5:A6", false)],
                [
                    Some("B5:C7"),
                    Some("F2"),
                    Some("E10:F12"),
                    Some("A1:A3"),
                    Some("H3:J5"),
                    Some("B15:F16"),
                ],
            ),
            (
                "probe37 delcell_left_left_of",
                &[St::DelCells("A5:A7", false)],
                [
                    Some("A5:B7"),
                    Some("F2"),
                    Some("E10:F12"),
                    Some("A1:A3"),
                    Some("H3:J5"),
                    Some("B15:F16"),
                ],
            ),
            (
                "probe38 delcell_left_exact",
                &[St::DelCells("B5:C7", false)],
                [
                    None,
                    Some("F2"),
                    Some("E10:F12"),
                    Some("A1:A3"),
                    Some("H3:J5"),
                    Some("B15:F16"),
                ],
            ),
            (
                "probe39 delcell_left_single_inside",
                &[St::DelCells("C6", false)],
                [
                    Some("B5:C7"),
                    Some("F2"),
                    Some("E10:F12"),
                    Some("A1:A3"),
                    Some("H3:J5"),
                    Some("B15:F16"),
                ],
            ),
            (
                "probe40 cut_col_B_to_H1",
                &[St::Cut("B1:B20", false, "H1")],
                [
                    Some("B5:C7"),
                    Some("F2"),
                    Some("E10:F12"),
                    Some("A1:A3"),
                    Some("H3:J5"),
                    Some("B15:F16"),
                ],
            ),
            (
                "probe41 cut_whole_r1",
                &[St::Cut("B5:C7", false, "M1")],
                [
                    Some("M1:N3"),
                    Some("F2"),
                    Some("E10:F12"),
                    Some("A1:A3"),
                    Some("H3:J5"),
                    Some("B15:F16"),
                ],
            ),
            (
                "probe42 cut_r1_other_sheet",
                &[St::Cut("B5:C7", true, "A1")],
                [
                    Some("A1:B3"),
                    Some("F2"),
                    Some("E10:F12"),
                    Some("A1:A3"),
                    Some("H3:J5"),
                    Some("B15:F16"),
                ],
            ),
            (
                "probe43 cut_superset",
                &[St::Cut("A1:D10", false, "P1")],
                [
                    Some("Q5:R7"),
                    Some("F2"),
                    Some("E10:F12"),
                    Some("P1:P3"),
                    Some("H3:J5"),
                    Some("B15:F16"),
                ],
            ),
            (
                "probe44 cut_part_of_r1",
                &[St::Cut("B5:B6", false, "M1")],
                [
                    Some("B5:C7"),
                    Some("F2"),
                    Some("E10:F12"),
                    Some("A1:A3"),
                    Some("H3:J5"),
                    Some("B15:F16"),
                ],
            ),
            (
                "probe45 cut_onto_r1",
                &[St::Cut("M1:N3", false, "B5")],
                [
                    None,
                    Some("F2"),
                    Some("E10:F12"),
                    Some("A1:A3"),
                    Some("H3:J5"),
                    Some("B15:F16"),
                ],
            ),
            (
                "probe46 cut_row_5_to_row_30",
                &[St::Cut("A5:L5", false, "A30")],
                [
                    Some("B5:C7"),
                    Some("F2"),
                    Some("E10:F12"),
                    Some("A1:A3"),
                    Some("H3:J30"),
                    Some("B15:F16"),
                ],
            ),
            (
                "probe47 cut_into_before_shift",
                &[St::Cut("M1:M3", false, "B1")],
                [
                    Some("B5:C7"),
                    Some("F2"),
                    Some("E10:F12"),
                    Some("A1:A3"),
                    Some("H3:J5"),
                    Some("B15:F16"),
                ],
            ),
            (
                "probe48 cut_r5_shift_left_dest",
                &[St::Cut("H3:J5", false, "A40")],
                [
                    Some("B5:C7"),
                    Some("F2"),
                    Some("E10:F12"),
                    Some("A1:A3"),
                    Some("A40:C42"),
                    Some("B15:F16"),
                ],
            ),
            (
                "probe49 sort_covering",
                &[St::Sort("B4:C9")],
                [
                    Some("B5:C7"),
                    Some("F2"),
                    Some("E10:F12"),
                    Some("A1:A3"),
                    Some("H3:J5"),
                    Some("B15:F16"),
                ],
            ),
            (
                "probe50 sort_exact",
                &[St::Sort("B5:C7")],
                [
                    Some("B5:C7"),
                    Some("F2"),
                    Some("E10:F12"),
                    Some("A1:A3"),
                    Some("H3:J5"),
                    Some("B15:F16"),
                ],
            ),
            (
                "probe51 sort_partial_cols",
                &[St::Sort("B5:B7")],
                [
                    Some("B5:C7"),
                    Some("F2"),
                    Some("E10:F12"),
                    Some("A1:A3"),
                    Some("H3:J5"),
                    Some("B15:F16"),
                ],
            ),
            (
                "probe52 del_sheet",
                &[St::DelSheet],
                [None, None, None, None, None, None],
            ),
            (
                "probe53 rename_sheet",
                &[St::Rename],
                [
                    Some("B5:C7"),
                    Some("F2"),
                    Some("E10:F12"),
                    Some("A1:A3"),
                    Some("H3:J5"),
                    Some("B15:F16"),
                ],
            ),
            (
                "probe54 move_sheet",
                &[St::MoveSheet],
                [
                    Some("B5:C7"),
                    Some("F2"),
                    Some("E10:F12"),
                    Some("A1:A3"),
                    Some("H3:J5"),
                    Some("B15:F16"),
                ],
            ),
            (
                "probe55 ins_then_del_row",
                &[St::InsRows(5, 1), St::DelRows(5, 1)],
                [
                    Some("B5:C7"),
                    Some("F2"),
                    Some("E10:F12"),
                    Some("A1:A3"),
                    Some("H3:J5"),
                    Some("B15:F16"),
                ],
            ),
            (
                "probe56 ins_row_then_cut",
                &[St::InsRows(1, 1), St::Cut("B1:B20", false, "H1")],
                [
                    Some("B6:C8"),
                    Some("F3"),
                    Some("E11:F13"),
                    Some("A1:A4"),
                    Some("H4:J6"),
                    Some("B16:F17"),
                ],
            ),
            (
                "probe57 cut_start_cell_down",
                &[St::Cut("B5", false, "B6")],
                [
                    Some("B5:C7"),
                    Some("F2"),
                    Some("E10:F12"),
                    Some("A1:A3"),
                    Some("H3:J5"),
                    Some("B15:F16"),
                ],
            ),
            (
                "probe58 cut_end_cell_down",
                &[St::Cut("C7", false, "C9")],
                [
                    Some("B5:C7"),
                    Some("F2"),
                    Some("E10:F12"),
                    Some("A1:A3"),
                    Some("H3:J5"),
                    Some("B15:F16"),
                ],
            ),
        ];
        for (label, steps, expect) in cases {
            let mut wb = Workbook::new();
            let dest = wb.add_sheet("Dest").unwrap();
            let sheet = wb.add_sheet("probe").unwrap();
            for (i, v) in [
                4.0, 40.0, 9.0, 90.0, 2.0, 20.0, 7.0, 70.0, 1.0, 10.0, 5.0, 50.0,
            ]
            .into_iter()
            .enumerate()
            {
                let addr = CellAddr::new(sheet, 3 + i as u32 / 2, 1 + i as u32 % 2);
                wb.set_value(addr, Value::Number(v));
            }
            let ids = HELD.map(|h| wb.track(sheet, a1(h)));
            for st in *steps {
                match st {
                    InsRows(at, n) => drop(wb.insert_rows(sheet, *at, *n).unwrap()),
                    DelRows(at, n) => drop(wb.delete_rows(sheet, *at, *n).unwrap()),
                    InsCols(at, n) => drop(wb.insert_cols(sheet, *at, *n).unwrap()),
                    DelCols(at, n) => drop(wb.delete_cols(sheet, *at, *n).unwrap()),
                    InsCells(r, down) => drop(wb.insert_cells(sheet, a1(r), *down).unwrap()),
                    DelCells(r, up) => drop(wb.delete_cells(sheet, a1(r), *up).unwrap()),
                    Cut(src, to_dest, to) => {
                        let to = a1(to);
                        let target = if *to_dest { dest } else { sheet };
                        let at = CellAddr::new(target, to.r0, to.c0);
                        drop(wb.move_range(sheet, a1(src), at).unwrap());
                    }
                    Sort(r) => {
                        let spec = SortSpec {
                            keys: vec![SortKey {
                                index: 0,
                                descending: true,
                            }],
                            ..SortSpec::default()
                        };
                        drop(wb.sort_range(sheet, a1(r), &spec).unwrap());
                    }
                    DelSheet => drop(wb.delete_sheet(sheet).unwrap()),
                    Rename => drop(wb.rename_sheet(sheet, "probe_renamed").unwrap()),
                    MoveSheet => drop(wb.move_sheet(sheet, Some(dest)).unwrap()),
                }
            }
            for ((id, want), held) in ids.into_iter().zip(expect).zip(HELD) {
                let got = wb.tracked(id).map(|(s, r)| {
                    assert_eq!(s, sheet, "{label}: {held} sheet");
                    r.a1()
                });
                assert_eq!(got.as_deref(), *want, "{label}: {held}");
            }
        }
    }

    #[test]
    fn untrack_and_ids() {
        let mut wb = Workbook::new();
        let s = wb.add_sheet("T").unwrap();
        let a = wb.track(s, a1("A1:B2"));
        let b = wb.track(s, a1("A1:B2"));
        assert_ne!(a, b);
        assert_eq!(wb.tracked(a), Some((s, a1("A1:B2"))));
        wb.untrack(a);
        assert_eq!(wb.tracked(a), None);
        let c = wb.track(s, a1("C3"));
        assert_ne!(c, a);
        assert_ne!(c, b);
        assert_eq!(wb.tracked(b), Some((s, a1("A1:B2"))));
    }
}
