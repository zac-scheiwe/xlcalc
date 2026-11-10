use std::collections::HashMap;

use xlcalc::{CellAddr, Rect, SheetId};

use crate::app::{unsupported, Application, BookId, Object};
use crate::conv::Args;
use crate::error::OmError;
use crate::typelib::{InvokeKind, Member};
use xlcalc_variant::Variant;

pub(crate) const LOCKED_CELL: &str = "The cell or chart you're trying to change is on a protected sheet. To make a change, unprotect the sheet. You might be requested to enter a password.";

pub(crate) const PROTECTED_COMMAND: &str = "You cannot use this command on a protected sheet. To use this command, you must first unprotect the sheet (Review tab, Protect group, Unprotect Sheet button). You may be prompted for a password.";
const CREATOR: i32 = 0x5843_454C;
const BAD_PASSWORD: &str = "The password you supplied is not correct. Verify that the CAPS LOCK key is off and be sure to use the correct capitalization.";

pub(crate) const ALLOW_NAMES: [&str; 11] = [
    "AllowFormattingCells",
    "AllowFormattingColumns",
    "AllowFormattingRows",
    "AllowInsertingColumns",
    "AllowInsertingRows",
    "AllowInsertingHyperlinks",
    "AllowDeletingColumns",
    "AllowDeletingRows",
    "AllowSorting",
    "AllowFiltering",
    "AllowUsingPivotTables",
];
const FMT_CELLS: usize = 0;
const FMT_COLUMNS: usize = 1;
const FMT_ROWS: usize = 2;
const INS_COLUMNS: usize = 3;
const INS_ROWS: usize = 4;
const DEL_COLUMNS: usize = 6;
const DEL_ROWS: usize = 7;
const SORTING: usize = 8;

#[derive(Clone, Debug, Default, PartialEq, Eq)]
pub(crate) struct SheetProt {
    pub contents: bool,
    pub objects: bool,
    pub scenarios: bool,
    pub password: Option<String>,
    pub ui_only: bool,
    pub allow: [bool; 11],
    pub selection: i32,
}

impl SheetProt {
    fn any(&self) -> bool {
        self.contents || self.objects || self.scenarios
    }
}

pub(crate) type BookSheetProt = HashMap<SheetId, SheetProt>;

impl Application<'_> {
    pub(crate) fn sheet_prot(&self, b: BookId, s: SheetId) -> SheetProt {
        self.book(b)
            .ok()
            .and_then(|b| b.sheet_prot.get(&s).cloned())
            .unwrap_or_default()
    }

    pub(crate) fn contents_protected(&self, b: BookId, s: SheetId) -> bool {
        self.book(b)
            .is_ok_and(|b| b.sheet_prot.get(&s).is_some_and(|p| p.contents))
    }

    pub(crate) fn merge_keeps_top_left_only(&self, b: BookId, s: SheetId) -> bool {
        self.book(b).is_ok_and(|b| {
            b.sheet_prot
                .get(&s)
                .is_some_and(|p| p.contents && p.allow[FMT_CELLS])
        })
    }

    fn set_sheet_prot(&mut self, b: BookId, s: SheetId, p: SheetProt) -> Result<(), OmError> {
        let book = self.book_mut(b)?;
        if p == SheetProt::default() {
            book.sheet_prot.remove(&s);
        } else {
            book.sheet_prot.insert(s, p);
        }
        Ok(())
    }

    pub(crate) fn protect_sheet(&mut self, b: BookId, s: SheetId, a: &Args) -> Result<(), OmError> {
        let password = a.string(0)?.filter(|p| !p.is_empty());

        if password.as_ref().is_some_and(|p| p.chars().count() > 255) {
            return Err(OmError::app_defined());
        }
        let old = self.sheet_prot(b, s);
        let mut allow = [false; 11];
        for (i, flag) in allow.iter_mut().enumerate() {
            *flag = a.bool(5 + i)?.unwrap_or(false);
        }
        let objects = a.bool(1)?.unwrap_or(true);
        let contents = a.bool(2)?.unwrap_or(true);
        let scenarios = a.bool(3)?.unwrap_or(true);
        let new = SheetProt {
            objects,
            contents,
            scenarios,
            password: if old.any() { old.password } else { password },
            ui_only: a.bool(4)?.unwrap_or(old.ui_only),
            allow: if contents || objects || scenarios {
                allow
            } else {
                old.allow
            },
            selection: old.selection,
        };
        self.set_sheet_prot(b, s, new)
    }

    pub(crate) fn set_enable_selection(
        &mut self,
        b: BookId,
        s: SheetId,
        v: i32,
    ) -> Result<(), OmError> {
        let mut p = self.sheet_prot(b, s);
        p.selection = v;
        self.set_sheet_prot(b, s, p)
    }

    pub(crate) fn protection_member(
        &mut self,
        b: BookId,
        s: SheetId,
        m: &'static Member,
    ) -> Result<Variant, OmError> {
        use InvokeKind::*;
        match (m.name, m.kind) {
            ("AllowEditRanges", Get) => Ok(self.alloc(Object::AllowEditRanges(b, s))),
            ("Application", Get) => Ok(Variant::Dispatch(Some(self.handle()))),
            ("Creator", Get) => Ok(Variant::I4(CREATOR)),
            ("Parent", Get) => Ok(self.alloc(Object::Worksheet(b, s))),
            (name, Get) => match ALLOW_NAMES.iter().position(|n| *n == name) {
                Some(i) => Ok(Variant::Bool(self.sheet_prot(b, s).allow[i])),
                None => Err(unsupported("Protection", m)),
            },
            _ => Err(unsupported("Protection", m)),
        }
    }

    pub(crate) fn allow_edit_ranges_member(
        &mut self,
        b: BookId,
        s: SheetId,
        m: &'static Member,
    ) -> Result<Variant, OmError> {
        use InvokeKind::*;
        match (m.name, m.kind) {
            ("Count", Get) => Ok(Variant::I4(0)),
            ("Application", Get) => Ok(Variant::Dispatch(Some(self.handle()))),
            ("Creator", Get) => Ok(Variant::I4(CREATOR)),
            ("Parent", Get) => Ok(self.alloc(Object::Protection(b, s))),
            ("Item" | "_Default", _) => Err(OmError::hresult(xlcalc_variant::HResult(
                0x8002_000B_u32 as i32,
            ))),
            _ => Err(unsupported("AllowEditRanges", m)),
        }
    }

    pub(crate) fn unprotect_sheet(
        &mut self,
        b: BookId,
        s: SheetId,
        a: &Args,
    ) -> Result<bool, OmError> {
        let old = self.sheet_prot(b, s);
        if !old.any() {
            return Ok(false);
        }
        if old.password.is_some() && a.string(0)? != old.password {
            return Err(OmError::app_defined_with(BAD_PASSWORD));
        }
        self.set_sheet_prot(
            b,
            s,
            SheetProt {
                ui_only: old.ui_only,
                allow: old.allow,
                selection: old.selection,
                ..SheetProt::default()
            },
        )?;
        Ok(true)
    }

    pub(crate) fn protect_document(
        &mut self,
        b: BookId,
        s: SheetId,
        flags: [Option<bool>; 3],
        password: Option<String>,
    ) -> Result<bool, OmError> {
        let old = self.sheet_prot(b, s);
        let new = SheetProt {
            contents: flags[0].unwrap_or(!old.contents),
            objects: flags[1].unwrap_or(!old.objects),
            scenarios: flags[2].unwrap_or(!old.scenarios),
            password: None,
            ..old.clone()
        };
        if old.any() && !new.any() && old.password.is_some() && old.password != password {
            return Ok(false);
        }
        let password = if old.any() { old.password } else { password };
        self.set_sheet_prot(b, s, SheetProt { password, ..new })?;
        Ok(true)
    }

    pub(crate) fn locked_snapshot(
        &self,
        b: BookId,
        s: SheetId,
        rect: Rect,
    ) -> Vec<(CellAddr, bool)> {
        let p = self.sheet_prot(b, s);
        let Ok(book) = self.book(b) else {
            return Vec::new();
        };
        if !p.contents || p.ui_only {
            return Vec::new();
        }
        (rect.r0..=rect.r1)
            .flat_map(|r| (rect.c0..=rect.c1).map(move |c| CellAddr::new(s, r, c)))
            .map(|x| (x, book.wb.cell_style(x).locked))
            .collect()
    }

    pub(crate) fn restore_locked(&mut self, b: BookId, flags: &[(CellAddr, bool)]) {
        let Ok(book) = self.book_mut(b) else {
            return;
        };
        for (x, locked) in flags {
            if book.wb.cell_style(*x).locked != *locked {
                let locked = *locked;
                book.wb.set_style_with(
                    x.sheet,
                    Rect::new(x.row, x.col, x.row, x.col),
                    &move |st: &xlcalc::style::Style| {
                        let mut st = st.clone();
                        st.locked = locked;
                        st
                    },
                );
            }
        }
    }

    pub(crate) fn clear_keeps_locked(&self, b: BookId, s: SheetId) -> bool {
        let p = self.sheet_prot(b, s);
        p.contents && !p.ui_only && !p.allow[FMT_CELLS]
    }

    fn any_locked(&self, b: BookId, s: SheetId, rect: Rect) -> bool {
        let Ok(book) = self.book(b) else {
            return false;
        };

        let merges = book.wb.merges(s);
        (rect.r0..=rect.r1)
            .flat_map(|r| (rect.c0..=rect.c1).map(move |c| (r, c)))
            .any(|(r, c)| {
                let (r, c) = merges
                    .iter()
                    .find(|m| m.r0 <= r && r <= m.r1 && m.c0 <= c && c <= m.c1)
                    .map_or((r, c), |m| (m.r0, m.c0));
                book.wb.cell_style(CellAddr::new(s, r, c)).locked
            })
    }

    pub(crate) fn protect_guard(
        &self,
        b: BookId,
        s: SheetId,
        rect: Rect,
        m: &Member,
    ) -> Result<(), OmError> {
        let p = self.sheet_prot(b, s);
        if !p.contents {
            return Ok(());
        }
        use InvokeKind::*;
        let put = matches!(m.kind, Put | PutRef);
        let locked = || OmError::app_defined_with(LOCKED_CELL);
        let name = m.name;
        let any_locked = || self.any_locked(b, s, rect);
        if p.ui_only {
            return match name {
                "ClearContents" if m.kind == Func && any_locked() => Err(locked()),
                _ => Ok(()),
            };
        }
        let entire_rows = rect.c0 == 0 && rect.c1 + 1 >= crate::addr::MAX_COLS;
        let entire_cols = rect.r0 == 0 && rect.r1 + 1 >= crate::addr::MAX_ROWS;
        let writes = matches!(
            name,
            "Value"
                | "Value2"
                | "_Default"
                | "Formula"
                | "FormulaLocal"
                | "Formula2"
                | "Formula2Local"
                | "FormulaR1C1"
                | "FormulaR1C1Local"
                | "Formula2R1C1"
                | "Formula2R1C1Local"
                | "FormulaArray"
        );
        if put && writes {
            return if any_locked() { Err(locked()) } else { Ok(()) };
        }
        let fail = |n: &str| OmError::unable_to_set(n, "Range");
        if put {
            let allowed = match name {
                "Locked" | "FormulaHidden" => false,
                "ColumnWidth" | "UseStandardWidth" => p.allow[FMT_COLUMNS],
                "RowHeight" | "UseStandardHeight" => p.allow[FMT_ROWS],
                "Hidden" if entire_cols => p.allow[FMT_COLUMNS],
                "Hidden" if entire_rows => p.allow[FMT_ROWS],
                "Hidden" => false,
                "NumberFormat"
                | "NumberFormatLocal"
                | "HorizontalAlignment"
                | "VerticalAlignment"
                | "WrapText"
                | "ShrinkToFit"
                | "IndentLevel"
                | "Orientation"
                | "AddIndent"
                | "ReadingOrder"
                | "MergeCells" => p.allow[FMT_CELLS],
                _ => return Ok(()),
            };
            return if allowed { Ok(()) } else { Err(fail(name)) };
        }
        if m.kind == Func {
            let failed =
                || OmError::app_defined_with(&format!("{name} method of Range class failed"));
            match name {
                "ClearContents" | "Clear" | "ClearFormats" if any_locked() => {
                    return Err(locked());
                }
                "ClearNotes" | "AddComment" => return Err(locked()),
                "Group" | "Ungroup" | "ClearOutline" | "AutoFilter" | "Justify" => {
                    return Err(OmError::app_defined_with(PROTECTED_COMMAND));
                }
                "Insert" if entire_cols && !entire_rows => {
                    if !p.allow[INS_COLUMNS] {
                        return Err(failed());
                    }
                }
                "Insert" if entire_rows => {
                    if !p.allow[INS_ROWS] {
                        return Err(failed());
                    }
                }
                "Delete" if entire_cols && !entire_rows => {
                    if !p.allow[DEL_COLUMNS] || any_locked() {
                        return Err(failed());
                    }
                }
                "Delete" if entire_rows => {
                    if !p.allow[DEL_ROWS] || any_locked() {
                        return Err(failed());
                    }
                }
                "Insert" | "Delete" => return Err(failed()),
                "Sort" => {
                    if !p.allow[SORTING] || any_locked() {
                        return Err(failed());
                    }

                    if rect.is_cell() {
                        let region = self
                            .book(b)
                            .map(|bk| bk.wb.current_region(s, rect))
                            .unwrap_or(rect);
                        if self.any_locked(b, s, region) {
                            return Err(locked());
                        }
                    }
                }
                "Merge" | "UnMerge" => return Err(failed()),
                "AutoFit" => {
                    let ok = if entire_cols {
                        p.allow[FMT_COLUMNS]
                    } else if entire_rows {
                        p.allow[FMT_ROWS]
                    } else {
                        false
                    };
                    if !ok {
                        return Err(failed());
                    }
                }

                "FillDown" if rect.r0 > 0 || rect.r1 > rect.r0 => return Err(locked()),
                "FillRight" if rect.c0 > 0 || rect.c1 > rect.c0 => return Err(locked()),
                _ => {}
            }
        }
        Ok(())
    }

    pub(crate) fn paste_guard(
        &self,
        b: BookId,
        (ss, src): (SheetId, Rect),
        (ds, pasted): (SheetId, Rect),
        cut: bool,
    ) -> Result<(), OmError> {
        let refuses = |s: SheetId, rect: Rect| {
            let p = self.sheet_prot(b, s);
            p.contents && !p.ui_only && self.any_locked(b, s, rect)
        };
        if refuses(ds, pasted) || (cut && refuses(ss, src)) {
            return Err(OmError::app_defined_with(LOCKED_CELL));
        }
        Ok(())
    }

    pub(crate) fn style_guard(
        &self,
        o: &crate::style::StyleOwner,
        class: &str,
        prop: &str,
    ) -> Result<(), OmError> {
        let crate::style::StyleOwner::Range(r) = o else {
            return Ok(());
        };
        let (sheet, _) = self.resolve(r)?;
        let p = self.sheet_prot(r.book, sheet);
        if p.contents && !p.ui_only && !p.allow[FMT_CELLS] {
            return Err(OmError::unable_to_set(prop, class));
        }
        Ok(())
    }
}
