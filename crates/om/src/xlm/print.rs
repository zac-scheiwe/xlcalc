use xlcalc::xlm::{Operand, XlmError};
use xlcalc::Rect;
use xlcalc::{Value, XlError};

use super::fmt::A;
use super::AppEnv;
use crate::addr::{max_cols, max_rows, range_text, AddressStyle};
use crate::pagesetup::{first_page_value, PageSetup};

const VALUE: Result<Value, XlmError> = Ok(Value::Error(XlError::VALUE));
const TRUE: Result<Value, XlmError> = Ok(Value::Bool(true));

fn is_whole(r: &Rect) -> bool {
    r.r0 == 0 && r.c0 == 0 && r.r1 + 1 >= max_rows() && r.c1 + 1 >= max_cols()
}

fn abs_text(r: Rect) -> String {
    range_text(
        r,
        &AddressStyle {
            row_abs: true,
            col_abs: true,
            r1c1: false,
            relative_to: (0, 0),
        },
    )
}

fn hf_parts(text: &str) -> ([String; 3], Vec<char>) {
    let chars: Vec<char> = text.chars().collect();
    let marker = |i: usize, k: char| {
        chars.get(i) == Some(&'&') && chars.get(i + 1).is_some_and(|c| c.eq_ignore_ascii_case(&k))
    };
    let at = |k: char| (0..chars.len()).find(|i| marker(*i, k));
    let any = |i: usize| ['L', 'C', 'R'].iter().any(|k| marker(i, *k));
    let mut parts = [String::new(), String::new(), String::new()];
    let found = [at('L'), at('C'), at('R')];
    for (slot, pos) in parts.iter_mut().zip(found) {
        if let Some(p) = pos {
            let start = p + 2;
            let end = (start + 1..chars.len())
                .find(|i| any(*i))
                .unwrap_or(chars.len());
            *slot = chars[start.min(chars.len())..end.max(start.min(chars.len()))]
                .iter()
                .collect();
        }
    }

    let mut at = [(0, 'L'), (0, 'C'), (0, 'R')];
    for (i, pos) in found.iter().enumerate() {
        at[i].0 = pos.map_or(usize::MAX, |p| p + 1);
    }
    if found[1].is_none() {
        let first = found.iter().flatten().min().copied().unwrap_or(chars.len());
        parts[1] = chars[..first].iter().collect();
        at[1].0 = 0;
    }
    at.sort();
    let order = at
        .iter()
        .filter(|(pos, k)| {
            *pos != usize::MAX && !parts[usize::from(*k != 'L') + usize::from(*k == 'R')].is_empty()
        })
        .map(|(_, k)| *k)
        .collect();
    (parts, order)
}

impl AppEnv<'_, '_> {
    fn hf_text(a: &A) -> Option<String> {
        match a {
            A::Text(t) => Some(t.clone()),
            A::Num(n) => Some(format!("{n}")),
            A::Bool(b) => Some(if *b { "TRUE" } else { "FALSE" }.to_string()),
            _ => None,
        }
    }

    fn put_hf(p: &mut PageSetup, footer: bool, text: &str) {
        let ([l, c, r], new_order) = hf_parts(text);
        let (order, raw, parts) = if footer {
            (
                &mut p.footer_order,
                &mut p.footer_raw,
                [
                    &mut p.left_footer,
                    &mut p.center_footer,
                    &mut p.right_footer,
                ],
            )
        } else {
            (
                &mut p.header_order,
                &mut p.header_raw,
                [
                    &mut p.left_header,
                    &mut p.center_header,
                    &mut p.right_header,
                ],
            )
        };
        *raw = Some(text.to_string());
        *order = new_order;
        for (slot, new) in parts.into_iter().zip([l, c, r]) {
            *slot = new;
        }
    }

    fn page_setup_cmd(&mut self, args: &[Operand]) -> Result<Value, XlmError> {
        let sheet = self.wbr().active_sheet();
        let mut p = self.app.page_setup(self.book, sheet);
        let a: Vec<A> = (0..21).map(|i| self.a(args, i)).collect();
        let mut ok = true;

        let num = |x: &A| match x {
            A::Num(n) => Some(*n),
            A::Bool(b) => Some(f64::from(u8::from(*b))),
            A::Text(t) => t.trim().parse::<f64>().ok(),
            _ => None,
        };
        let miss = |x: &A| matches!(x, A::Miss);
        for (i, footer) in [(0, false), (1, true)] {
            if !miss(&a[i]) {
                match Self::hf_text(&a[i]) {
                    Some(t) => Self::put_hf(&mut p, footer, &t),
                    None => ok = false,
                }
            }
        }
        for (i, slot) in [
            (2, &mut p.left),
            (3, &mut p.right),
            (4, &mut p.top),
            (5, &mut p.bottom),
            (17, &mut p.header_margin),
            (18, &mut p.footer_margin),
        ] {
            if !miss(&a[i]) {
                match num(&a[i]) {
                    Some(v) => *slot = v * 72.0,
                    None => ok = false,
                }
            }
        }
        for (i, slot) in [
            (6, &mut p.headings),
            (7, &mut p.gridlines),
            (8, &mut p.center_horizontally),
            (9, &mut p.center_vertically),
            (15, &mut p.black_and_white),
            (20, &mut p.draft),
        ] {
            if !miss(&a[i]) {
                match num(&a[i]) {
                    Some(v) => *slot = v != 0.0,
                    None => ok = false,
                }
            }
        }
        if !miss(&a[19]) {
            match num(&a[19]) {
                Some(v) => p.comments = if v != 0.0 { 1 } else { -4142 },
                None => ok = false,
            }
        }
        let int = |x: &A| num(x).map(|n| n.trunc() as i64);
        if !miss(&a[10]) {
            match int(&a[10]) {
                Some(n @ (1 | 2)) => p.orientation = n as f64,
                _ => ok = false,
            }
        }
        if !miss(&a[11]) {
            match int(&a[11]) {
                Some(n) if crate::pagesetup::paper_ok(n as i32) => p.paper_size = n as f64,
                _ => ok = false,
            }
        }
        match &a[12] {
            A::Miss => {}

            A::Bool(true) => {
                p.zoom = None;
                p.fit_wide = Some(1);
                p.fit_tall = Some(1);
            }
            x => match int(x) {
                Some(n @ 10..=400) if !matches!(x, A::Bool(_)) => p.zoom = Some(n as i32),
                _ => ok = false,
            },
        }
        if !miss(&a[13]) {
            match num(&a[13]) {
                Some(n) => p.first_page = first_page_value(n.trunc()),
                None => ok = false,
            }
        }
        if !miss(&a[14]) {
            match int(&a[14]) {
                Some(n @ (1 | 2)) => p.order = n as f64,
                _ => ok = false,
            }
        }

        if !miss(&a[16]) && int(&a[16]) != Some(600) {
            ok = false;
        }
        if !ok {
            return VALUE;
        }

        if (0..21).any(|i| i != 12 && !miss(&a[i])) {
            p.list_zoom = p.zoom.unwrap_or(0);
            p.list_win = Some(self.app.break_window(self.book, sheet));
        }
        if let Ok(b) = self.app.book_mut(self.book) {
            b.page_setup.insert(sheet, p);
        }
        TRUE
    }

    fn set_print_area(&mut self, args: &[Operand]) -> Result<Value, XlmError> {
        let (sheet, rects) = match args.first() {
            None | Some(Operand::Missing) => {
                let wb = self.wbr();
                let s = wb.active_sheet();
                (s, wb.selection_of(s).areas)
            }

            Some(Operand::Ref(r)) if !r.areas().is_empty() => (
                self.wbr().active_sheet(),
                r.areas().iter().map(|a| a.rect).collect(),
            ),

            Some(_) => {
                let A::Text(t) = self.a(args, 0) else {
                    return VALUE;
                };
                let sheet = self.wbr().active_sheet();
                return match self.app.put_print_area(self.book, sheet, &t) {
                    Ok(()) => TRUE,
                    Err(_) => VALUE,
                };
            }
        };
        let text = if rects.iter().any(is_whole) {
            String::new()
        } else {
            rects
                .iter()
                .map(|r| abs_text(*r))
                .collect::<Vec<_>>()
                .join(",")
        };
        match self.app.put_print_area(self.book, sheet, &text) {
            Ok(()) => TRUE,
            Err(_) => VALUE,
        }
    }

    fn set_print_titles(&mut self, args: &[Operand]) -> Result<Value, XlmError> {
        let sheet = self.wbr().active_sheet();
        let mut todo: Vec<(bool, String)> = Vec::new();
        for (i, rows) in [(0, true), (1, false)] {
            match args.get(i) {
                None | Some(Operand::Missing) => {}

                Some(Operand::Ref(r)) => match r.areas() {
                    [a] if a.sheet == sheet && !is_whole(&a.rect) => {
                        todo.push((rows, abs_text(a.rect)));
                    }
                    _ => return VALUE,
                },

                Some(_) => match self.a(args, i) {
                    A::Text(t) if t.is_empty() => todo.push((rows, String::new())),
                    A::Miss => {}
                    _ => return VALUE,
                },
            }
        }

        let saved = (
            self.app.print_title_rows(self.book, sheet),
            self.app.print_title_columns(self.book, sheet),
        );
        for (rows, text) in &todo {
            if self.app.put_titles(self.book, sheet, *rows, text).is_err() {
                let _ = self.app.put_titles(self.book, sheet, true, &saved.0);
                let _ = self.app.put_titles(self.book, sheet, false, &saved.1);
                return VALUE;
            }
        }
        TRUE
    }

    fn page_break_cmd(&mut self, set: bool) -> Result<Value, XlmError> {
        let (sheet, areas) = {
            let wb = self.wbr();
            let s = wb.active_sheet();
            (s, wb.selection_of(s).areas)
        };

        let [r] = areas.as_slice() else {
            return VALUE;
        };

        if r.r0 == 0 && r.c0 == 0 {
            return VALUE;
        }
        let mut found = false;
        let Ok(b) = self.app.book_mut(self.book) else {
            return VALUE;
        };
        for (rows, at) in [(true, r.r0), (false, r.c0)] {
            if at == 0 {
                continue;
            }
            if set {
                b.wb.add_page_break(sheet, rows, at);
                found = true;
            } else if b.wb.all_page_breaks(sheet, rows).contains(&at) {
                b.wb.delete_page_break(sheet, rows, at);
                found = true;
            }
        }
        Ok(Value::Bool(found))
    }

    pub(super) fn print_command(
        &mut self,
        name: &str,
        args: &[Operand],
    ) -> Result<Value, XlmError> {
        match name {
            "PAGE.SETUP" => self.page_setup_cmd(args),
            "SET.PRINT.AREA" => self.set_print_area(args),
            "SET.PRINT.TITLES" => self.set_print_titles(args),
            "SET.PAGE.BREAK" => self.page_break_cmd(true),
            "REMOVE.PAGE.BREAK" => self.page_break_cmd(false),
            _ => Err(XlmError::Unsupported(name.to_string())),
        }
    }
}
