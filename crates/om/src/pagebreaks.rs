use xlcalc::Rect;
use xlcalc_variant::Variant;

use crate::app::{unsupported, Application, BookId, Object};
use crate::conv::Args;
use crate::error::OmError;
use crate::typelib::{InvokeKind, Member};
use xlcalc::SheetId;

const MANUAL: i32 = -4135;
const FULL: i32 = 1;

const AUTOMATIC: i32 = -4105;

impl Application<'_> {
    fn breaks_of(
        &mut self,
        b: BookId,
        s: SheetId,
        rows: bool,
        count: bool,
    ) -> Result<Vec<u32>, OmError> {
        let plv = self.book(b)?.window.view_mode == 3;
        let parts = if plv {
            vec!["$A$1:$XFD$1048576".to_string()]
        } else {
            self.print_area_parts(b, s)
        };
        if parts.is_empty() {
            let wb = &self.book(b)?.wb;
            let used = wb.merges(s).into_iter().fold(wb.used_range(s), |u, m| {
                Some(u.map_or(m, |u| u.union_bounds(&m)))
            });
            let Some(used) = used else {
                return Ok(wb.page_breaks(s, rows));
            };
            let ps = self.page_setup(b, s);
            let win = self.break_window(b, s);
            let ub = crate::pagegeom::used_breaks(
                wb,
                s,
                &ps,
                (used.r1 + 1, used.c1 + 1),
                win,
                ps.slv_cached,
            );
            let list = match (count, rows) {
                (true, true) => ub.fresh.0,
                (true, false) => ub.fresh.1,
                (false, true) => ub.cached.0,
                (false, false) => ub.cached.1,
            };

            let grown = ps.zoom.is_some_and(|z| z < ps.list_zoom);
            if count && (!ps.slv_cached || grown) {
                self.book_mut(b)?
                    .page_setup
                    .entry(s)
                    .or_default()
                    .slv_cached = true;
                let win = self.break_window_idle(b, s);
                self.refresh_break_list_over(b, s, win);
            }
            return Ok(list.into_iter().map(|i| i - 1).collect());
        }
        let shown = matches!(
            self.opts.get(&format!("ws{}:DisplayPageBreaks", b.0)),
            Some(Variant::Bool(true))
        );
        let win = self.break_window(b, s);
        let mut ps = self.page_setup(b, s);
        if shown && ps.list_win != Some(win) {
            self.refresh_break_list(b, s);
            ps = self.page_setup(b, s);
        }
        let titles = crate::pagegeom::Titles::parse(
            &self.print_title_rows(b, s),
            &self.print_title_columns(b, s),
        );
        let (h, v) =
            crate::pagegeom::cached_breaks(&self.book(b)?.wb, s, &ps, &parts, &titles, win);
        let list = if rows { h } else { v };
        Ok(list.into_iter().map(|(i, _)| i - 1).collect())
    }

    pub(crate) fn page_breaks_member(
        &mut self,
        rows: bool,
        owner: Option<(BookId, SheetId)>,
        m: &'static Member,
        a: &Args,
    ) -> Result<Variant, OmError> {
        let Some((b, s)) = owner else {
            return match m.name {
                "Count" => Ok(Variant::I4(0)),
                "Item" | "_Default" => Err(OmError::bad_index()),
                _ => Err(unsupported("PageBreaks", m)),
            };
        };
        let list = self.breaks_of(b, s, rows, m.name == "Count")?;
        match (m.name, m.kind) {
            ("Count", InvokeKind::Get) => Ok(Variant::I4(list.len() as i32)),
            ("Item" | "_Default", InvokeKind::Get) => {
                let i = a.i32(0)?.ok_or_else(OmError::bad_index)?;
                let at = usize::try_from(i - 1)
                    .ok()
                    .and_then(|i| list.get(i))
                    .ok_or_else(OmError::bad_index)?;
                Ok(self.alloc(Object::PageBreak(rows, b, s, *at)))
            }
            ("_NewEnum", InvokeKind::Get) => {
                let items = list
                    .into_iter()
                    .map(|at| self.alloc(Object::PageBreak(rows, b, s, at)))
                    .collect();
                Ok(self.alloc_enumerator(items))
            }
            ("Add", InvokeKind::Func) => {
                let before = match a.get(0) {
                    Some(Variant::Dispatch(Some(id))) => match self.arena.get(*id) {
                        Some(Object::Range(r)) => self.resolve(&r.clone())?,
                        _ => return Err(OmError::app_defined()),
                    },
                    _ => return Err(OmError::app_defined()),
                };
                if before.0 != s {
                    return Err(OmError::app_defined());
                }
                let at = if rows { before.1.r0 } else { before.1.c0 };

                if at == 0 {
                    return Err(OmError::app_defined());
                }
                self.book_mut(b)?.wb.add_page_break(s, rows, at);
                Ok(self.alloc(Object::PageBreak(rows, b, s, at)))
            }
            ("Application", InvokeKind::Get) => Ok(Variant::Dispatch(Some(self.handle()))),
            ("Parent", InvokeKind::Get) => Ok(self.sheet_object(b, s)),
            ("Creator", InvokeKind::Get) => Ok(Variant::I4(0x5843_454C_u32 as i32)),
            _ => Err(unsupported(
                if rows { "HPageBreaks" } else { "VPageBreaks" },
                m,
            )),
        }
    }

    pub(crate) fn page_break_member(
        &mut self,
        (rows, b, s, at): (bool, BookId, SheetId, u32),
        m: &'static Member,
        _a: &Args,
    ) -> Result<Variant, OmError> {
        match (m.name, m.kind) {
            ("Type", InvokeKind::Get) => {
                let manual = self.book(b)?.wb.all_page_breaks(s, rows).contains(&at);
                Ok(Variant::I4(if manual { MANUAL } else { AUTOMATIC }))
            }
            ("Extent", InvokeKind::Get) => Ok(Variant::I4(FULL)),
            ("Location", InvokeKind::Get) => {
                let rect = if rows {
                    Rect::new(at, 0, at, 0)
                } else {
                    Rect::new(0, at, 0, at)
                };
                self.new_range(b, s, rect, crate::app::Mode::Cells)
            }
            ("Delete", InvokeKind::Func) => {
                self.book_mut(b)?.wb.delete_page_break(s, rows, at);
                Ok(Variant::Empty)
            }
            ("Application", InvokeKind::Get) => Ok(Variant::Dispatch(Some(self.handle()))),
            ("Parent", InvokeKind::Get) => Ok(self.sheet_object(b, s)),
            ("Creator", InvokeKind::Get) => Ok(Variant::I4(0x5843_454C_u32 as i32)),
            _ => Err(unsupported(
                if rows { "HPageBreak" } else { "VPageBreak" },
                m,
            )),
        }
    }
}
