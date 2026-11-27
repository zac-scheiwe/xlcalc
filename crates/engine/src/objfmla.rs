use crate::address::{CellAddr, SheetId};
use crate::formula::{Node, SheetRef};
use crate::formula_print::print_node;
use crate::host::Evaluated;
use crate::moves::Move;
use crate::structure::{edit_refs, map_refs, shift_cref, targets_sheet, within_limit};
use crate::structure::{RefEdit, Shift};
use crate::workbook::sheetops::remap_sheet;
use crate::workbook::Workbook;

#[derive(Clone, Copy, PartialEq, Eq, Hash, Debug)]
pub enum ObjFmla {
    Link,

    Fill,

    Drawing,
}

type Obj = (SheetId, u32);

fn host(sheet: SheetId) -> CellAddr {
    CellAddr::new(sheet, 0, 0)
}

impl Workbook {
    pub fn object_formula_set(&mut self, (sheet, spid): Obj, which: ObjFmla, text: &str) {
        let key = (sheet, spid, which);
        match self.compile_formula(text, host(sheet)) {
            Ok(f) => self.object_formulas.insert(key, f.root),
            Err(_) => self.object_formulas.remove(&key),
        };
    }

    #[must_use]
    pub fn drawing_formula_put(&mut self, (sheet, spid): Obj, text: &str) -> bool {
        let key = (sheet, spid, ObjFmla::Drawing);
        if text.is_empty() {
            self.object_formulas.remove(&key);
            return true;
        }
        let body = text.strip_prefix('=').unwrap_or(text);
        let Some(root) = self.object_reference(body, host(sheet)) else {
            return false;
        };
        self.object_formulas.insert(key, root);
        true
    }

    fn object_reference(&self, body: &str, at: CellAddr) -> Option<Node> {
        if body.starts_with(' ') {
            return None;
        }

        let body = body.trim_end_matches(' ');
        let root = self.compile_formula(body, at).ok()?.root;
        let one_sheet = |s: &SheetRef| !matches!(s, SheetRef::Span(..) | SheetRef::Missing(_));
        let ok = match &root {
            Node::Ref(s, _) => one_sheet(s),
            Node::StructRef { .. } => true,
            Node::Name { .. } => matches!(
                self.evaluate_text(body, at),
                Evaluated::Ref(areas) if areas.len() == 1
            ),
            _ => false,
        };
        ok.then_some(root)
    }

    #[must_use]
    pub fn object_formula(&self, (sheet, spid): Obj, which: ObjFmla) -> Option<String> {
        let root = self.object_formulas.get(&(sheet, spid, which))?;
        Some(print_node(self, root, host(sheet)))
    }

    pub fn object_formula_copy(&mut self, from: Obj, to: Obj, sheet_copy: bool) {
        for which in [ObjFmla::Link, ObjFmla::Fill, ObjFmla::Drawing] {
            let Some(mut root) = self.object_formulas.get(&(from.0, from.1, which)).cloned() else {
                self.object_formulas.remove(&(to.0, to.1, which));
                continue;
            };
            if sheet_copy {
                root = crate::formula::map_node(&root, &mut |n| {
                    remap_sheet(n, &mut |s| match s {
                        SheetRef::Sheet(id) if *id == from.0 => Some(SheetRef::Sheet(to.0)),
                        _ => None,
                    })
                });
            }
            self.object_formulas.insert((to.0, to.1, which), root);
        }
    }

    pub(crate) fn object_formulas_shift(&mut self, s: &Shift) {
        for (&(sheet, ..), root) in &mut self.object_formulas {
            let at = host(sheet);
            *root = map_refs(root, &mut |sh, cref| {
                if !targets_sheet(sh, sheet, s.sheet) {
                    None
                } else if within_limit(cref, at, s) {
                    Some(shift_cref(cref, at, at, s).ok())
                } else {
                    s.straddle(cref, at, at).map(Some)
                }
            });
        }
    }

    pub(crate) fn object_formulas_move(&mut self, mv: &Move) {
        for (&(sheet, ..), root) in &mut self.object_formulas {
            let at = host(sheet);
            *root = edit_refs(root, &mut |sh, cref| match mv.edit(sh, cref, at, at) {
                RefEdit::Set(SheetRef::Sheet(to), _)
                    if matches!(sh, SheetRef::Current) && to != sheet =>
                {
                    RefEdit::Dead(SheetRef::Current)
                }
                e => e,
            });
        }
    }

    pub(crate) fn object_formulas_drop_sheet(
        &mut self,
        id: SheetId,
        give_up: &mut dyn FnMut(&SheetRef) -> Option<SheetRef>,
    ) {
        self.object_formulas.retain(|k, _| k.0 != id);
        for root in self.object_formulas.values_mut() {
            *root = crate::formula::map_node(root, &mut |n: &Node| remap_sheet(n, give_up));
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::address::Rect;
    use crate::structure::Axis;

    fn book() -> (Workbook, SheetId, SheetId) {
        let mut wb = Workbook::new();
        let a = wb.sheets().next().map(|s| s.id).unwrap();
        let b = wb.add_sheet("Data").unwrap();
        (wb, a, b)
    }

    fn set(wb: &mut Workbook, s: SheetId, spid: u32, text: &str) {
        wb.object_formula_set((s, spid), ObjFmla::Link, text);
    }

    fn get(wb: &Workbook, s: SheetId, spid: u32) -> String {
        wb.object_formula((s, spid), ObjFmla::Link)
            .unwrap_or_default()
    }

    #[test]
    fn text_is_printed_back_and_follows_inserts_and_deletes() {
        let (mut wb, a, b) = book();
        set(&mut wb, a, 1, "c5");
        set(&mut wb, a, 2, "data!$A$2:$A$6");
        set(&mut wb, a, 3, "Sheet1!C$7");
        assert_eq!(get(&wb, a, 1), "C5");
        assert_eq!(get(&wb, a, 2), "Data!$A$2:$A$6");
        wb.insert_rows(a, 1, 1).unwrap();
        wb.insert_rows(b, 2, 1).unwrap();
        assert_eq!(get(&wb, a, 1), "C6");
        assert_eq!(get(&wb, a, 2), "Data!$A$2:$A$7");
        assert_eq!(get(&wb, a, 3), "Sheet1!C$8");
        wb.delete_rows(a, 5, 3).unwrap();
        assert_eq!(get(&wb, a, 1), "#REF!");
        assert_eq!(get(&wb, a, 3), "Sheet1!#REF!");
        let shift = Shift {
            sheet: b,
            axis: Axis::Rows,
            at: 0,
            count: 1,
            insert: false,
            limit: None,
            cells: false,
        };
        wb.object_formulas_shift(&shift);
        assert_eq!(get(&wb, a, 2), "Data!$A$1:$A$6");
    }

    #[test]
    fn a_cut_to_another_sheet_kills_only_an_unqualified_reference() {
        let (mut wb, a, b) = book();
        set(&mut wb, a, 1, "C1");
        set(&mut wb, a, 2, "Sheet1!C1");
        wb.move_range(a, Rect::new(0, 2, 0, 2), CellAddr::new(b, 0, 3))
            .unwrap();
        assert_eq!(get(&wb, a, 1), "#REF!");
        assert_eq!(get(&wb, a, 2), "Data!D1");
        wb.delete_sheet(b).unwrap();
        assert_eq!(get(&wb, a, 2), "#REF!D1");
    }

    #[test]
    fn a_drawing_put_strips_the_equals_sign_refuses_bad_text_and_follows_rows() {
        let (mut wb, a, _b) = book();
        set(&mut wb, a, 4, "c5");
        assert!(wb.drawing_formula_put((a, 7), "=c5"));
        let get_drawing = |wb: &Workbook| wb.object_formula((a, 7), ObjFmla::Drawing);
        assert_eq!(get_drawing(&wb).as_deref(), Some("C5"));
        for refused in [
            "=(",
            "=",
            " A1",
            "=1+1",
            "=(A1)",
            "=INDEX(A1:A3,2)",
            "=A1,B2",
        ] {
            assert!(!wb.drawing_formula_put((a, 7), refused), "{refused}");
        }
        assert_eq!(get_drawing(&wb).as_deref(), Some("C5"));
        wb.insert_rows(a, 1, 1).unwrap();
        assert_eq!(get_drawing(&wb).as_deref(), Some("C6"));
        assert_eq!(get(&wb, a, 4), "C6");
        assert!(wb.drawing_formula_put((a, 7), ""));
        assert_eq!(get_drawing(&wb), None);
        assert_eq!(get(&wb, a, 4), "C6");
    }

    #[test]
    fn a_sheet_copy_retargets_the_own_name_only() {
        let (mut wb, a, b) = book();
        set(&mut wb, a, 1, "Sheet1!B2");
        wb.object_formula_copy((a, 1), (a, 2), false);
        wb.object_formula_copy((a, 1), (b, 3), true);
        assert_eq!(get(&wb, a, 2), "Sheet1!B2");
        assert_eq!(get(&wb, b, 3), "Data!B2");
    }
}
