use xlcalc::style::{Border, BorderLine, CellColor, Fill, Pattern};
use xlcalc::xlm::{Operand, XlmError};
use xlcalc::{Rect, SheetId};
use xlcalc::{Value, XlError};

use super::{first_element, first_value, AppEnv};
use crate::app::Call;
use xlcalc_variant::Variant;

#[derive(Clone, Debug)]
pub(super) enum A {
    Miss,
    Num(f64),
    Bool(bool),
    Text(String),
    Err,
}

impl A {
    fn int(&self) -> Option<i64> {
        match self {
            A::Num(n) => Some(n.trunc() as i64),
            A::Bool(b) => Some(i64::from(*b)),
            _ => None,
        }
    }

    pub(super) fn flag(&self) -> Option<bool> {
        match self {
            A::Num(n) => Some(*n != 0.0),
            A::Bool(b) => Some(*b),
            _ => None,
        }
    }
}

const VALUE: Result<Value, XlmError> = Ok(Value::Error(XlError::VALUE));
const TRUE: Result<Value, XlmError> = Ok(Value::Bool(true));

enum Opt<T> {
    Skip,
    Set(T),
    Bad,
}

fn flag_opt(a: &A) -> Opt<bool> {
    match a {
        A::Miss => Opt::Skip,
        a => a.flag().map_or(Opt::Bad, Opt::Set),
    }
}

fn color_opt(a: &A, font: bool) -> Opt<i32> {
    match a {
        A::Miss => Opt::Skip,
        A::Bool(_) => Opt::Skip,
        a => match a.int() {
            Some(n @ 0..=56) => Opt::Set(n as i32),
            Some(n) if n > 56 && font => Opt::Set(0),
            _ => Opt::Bad,
        },
    }
}

impl AppEnv<'_, '_> {
    pub(super) fn a(&self, args: &[Operand], i: usize) -> A {
        match args.get(i) {
            None | Some(Operand::Missing) => A::Miss,
            Some(op) => match first_element(first_value(self.wbr(), op)) {
                Value::Blank => A::Miss,
                Value::Number(n) => A::Num(n),
                Value::Bool(b) => A::Bool(b),
                Value::Text(t) => A::Text(t.to_string()),
                _ => A::Err,
            },
        }
    }

    fn sel_areas(&self) -> (SheetId, Vec<Rect>) {
        let wb = self.wbr();
        let s = wb.active_sheet();
        (s, wb.selection_of(s).areas)
    }

    fn font_obj(&mut self) -> Result<xlcalc_variant::ObjectId, XlmError> {
        let id = self.selection()?;
        Self::dispatch(self.call(id, "Font", Call::GET, &[])?)
    }

    pub(super) fn format_command(
        &mut self,
        name: &str,
        args: &[Operand],
    ) -> Result<Value, XlmError> {
        match name {
            "FORMAT.NUMBER" => self.format_number(args),
            "FORMAT.FONT" => self.format_font(args),
            "FONT.PROPERTIES" => self.font_properties(args),
            "ALIGNMENT" => self.alignment(args),
            "CELL.PROTECTION" => self.cell_protection(args),
            "PATTERNS" => self.patterns(args),
            "BORDER" => self.border(args),
            "APPLY.STYLE" => self.apply_style(args),
            "DELETE.STYLE" => self.delete_style(args),
            _ => Err(XlmError::Unsupported(name.to_string())),
        }
    }

    fn format_number(&mut self, args: &[Operand]) -> Result<Value, XlmError> {
        let fmt = match self.a(args, 0) {
            A::Miss => return TRUE,
            A::Text(t) if t.is_empty() => return TRUE,
            A::Text(t) => t,
            A::Num(n) => format!("{n}"),
            _ => return VALUE,
        };
        let id = self.selection()?;
        self.app.command_format_put = true;
        let put = self.call(id, "NumberFormat", Call::PUT, &[Variant::Bstr(fmt)]);
        self.app.command_format_put = false;
        put?;
        TRUE
    }

    fn put_font_color(&mut self, font: xlcalc_variant::ObjectId, n: i32) -> Result<(), XlmError> {
        let v = if n == 0 { -4105 } else { n };
        self.call(font, "ColorIndex", Call::PUT, &[Variant::I4(v)])?;
        Ok(())
    }

    fn format_font(&mut self, args: &[Operand]) -> Result<Value, XlmError> {
        let name = match self.a(args, 0) {
            A::Miss => None,
            A::Text(t) => Some(t),
            _ => return VALUE,
        };
        let size = match self.a(args, 1) {
            A::Miss | A::Bool(_) => None,
            A::Num(n) if (1.0..410.0).contains(&n) => Some(n),
            _ => return VALUE,
        };
        let mut flags = Vec::new();
        for (i, member) in [
            (2, "Bold"),
            (3, "Italic"),
            (4, "Underline"),
            (5, "Strikethrough"),
            (7, "OutlineFont"),
            (8, "Shadow"),
        ] {
            match flag_opt(&self.a(args, i)) {
                Opt::Skip => {}
                Opt::Set(b) => flags.push((member, b)),
                Opt::Bad => return VALUE,
            }
        }
        let color = match color_opt(&self.a(args, 6), true) {
            Opt::Skip => None,
            Opt::Set(n) => Some(n),
            Opt::Bad => return VALUE,
        };
        let font = self.font_obj()?;
        if let Some(n) = name {
            self.call(font, "Name", Call::PUT, &[Variant::Bstr(n)])?;
        }
        if let Some(s) = size {
            self.call(font, "Size", Call::PUT, &[Variant::R8(s)])?;
        }
        for (member, b) in flags {
            let v = if member == "Underline" {
                Variant::I4(if b { 2 } else { -4142 })
            } else {
                Variant::Bool(b)
            };
            self.call(font, member, Call::PUT, &[v])?;
        }
        if let Some(n) = color {
            self.put_font_color(font, n)?;
        }
        TRUE
    }

    fn font_properties(&mut self, args: &[Operand]) -> Result<Value, XlmError> {
        let name = match self.a(args, 0) {
            A::Miss => None,
            A::Text(t) => Some(t),
            _ => return VALUE,
        };
        let style = match self.a(args, 1) {
            A::Miss => None,
            A::Text(t) => match t.to_ascii_lowercase().as_str() {
                "regular" => Some((false, false)),
                "bold" => Some((true, false)),
                "italic" => Some((false, true)),
                "bold italic" => Some((true, true)),
                _ => None,
            },
            _ => return VALUE,
        };
        let size = match self.a(args, 2) {
            A::Miss | A::Bool(_) => None,
            A::Num(n) if (1.0..410.0).contains(&n) => Some(n),
            _ => return VALUE,
        };
        let mut flags = Vec::new();
        for (i, member) in [
            (3, "Strikethrough"),
            (4, "Superscript"),
            (5, "Subscript"),
            (6, "OutlineFont"),
            (7, "Shadow"),
        ] {
            match flag_opt(&self.a(args, i)) {
                Opt::Skip => {}
                Opt::Set(b) => flags.push((member, b)),
                Opt::Bad => return VALUE,
            }
        }
        let underline = match self.a(args, 8) {
            A::Miss => None,
            a => match a.int() {
                Some(1) => Some(-4142),
                Some(2) => Some(2),
                Some(3) => Some(-4119),
                Some(4) => Some(4),
                Some(5) => Some(5),
                _ => return VALUE,
            },
        };
        let color = match color_opt(&self.a(args, 9), true) {
            Opt::Skip => None,
            Opt::Set(n) => Some(n),
            Opt::Bad => return VALUE,
        };

        let chars = match (self.a(args, 12).int(), self.a(args, 13).int()) {
            (Some(start), Some(len)) => Some((start, len)),
            _ => None,
        };
        let font = match chars {
            None => self.font_obj()?,
            Some((start, len)) => {
                let id = self.selection()?;
                let args = [Variant::I4(start as i32), Variant::I4(len as i32)];
                let Ok(Variant::Dispatch(Some(ch))) = self.call(id, "Characters", Call::GET, &args)
                else {
                    return TRUE;
                };
                match self.call(ch, "Font", Call::GET, &[]) {
                    Ok(Variant::Dispatch(Some(f))) => f,
                    _ => return TRUE,
                }
            }
        };
        if let Some(n) = name {
            self.call(font, "Name", Call::PUT, &[Variant::Bstr(n)])?;
        }
        if let Some((b, i)) = style {
            self.call(font, "Bold", Call::PUT, &[Variant::Bool(b)])?;
            self.call(font, "Italic", Call::PUT, &[Variant::Bool(i)])?;
        }
        if let Some(s) = size {
            self.call(font, "Size", Call::PUT, &[Variant::R8(s)])?;
        }
        for (member, b) in flags {
            self.call(font, member, Call::PUT, &[Variant::Bool(b)])?;
        }
        if let Some(u) = underline {
            self.call(font, "Underline", Call::PUT, &[Variant::I4(u)])?;
        }
        if let Some(n) = color {
            self.put_font_color(font, n)?;
        }
        TRUE
    }

    fn alignment(&mut self, args: &[Operand]) -> Result<Value, XlmError> {
        const H: [i32; 8] = [1, -4131, -4108, -4152, 5, -4130, 7, -4117];
        const V: [i32; 5] = [-4160, -4108, -4107, -4130, -4117];
        let h = match self.a(args, 0) {
            A::Miss => None,
            a => match a.int() {
                Some(n @ 1..=8) => Some(H[n as usize - 1]),
                _ => return VALUE,
            },
        };
        let wrap = match flag_opt(&self.a(args, 1)) {
            Opt::Skip => None,
            Opt::Set(b) => Some(b),
            Opt::Bad => return VALUE,
        };
        let v = match self.a(args, 2) {
            A::Miss => None,
            a => match a.int() {
                Some(n @ 1..=5) => Some(V[n as usize - 1]),
                _ => return VALUE,
            },
        };
        let orient = match self.a(args, 3) {
            A::Miss => None,
            a => match a.int() {
                Some(0) => Some(-4128),
                Some(1) => Some(-4166),
                Some(2) => Some(-4171),
                Some(3) => Some(-4170),
                Some(4) => None,
                _ => return VALUE,
            },
        };
        let id = self.selection()?;

        let shrink = if wrap.is_some() {
            self.call(id, "ShrinkToFit", Call::GET, &[]).ok()
        } else {
            None
        };
        if let Some(n) = h {
            self.call(id, "HorizontalAlignment", Call::PUT, &[Variant::I4(n)])?;

            if n == 1 {
                self.call(id, "IndentLevel", Call::PUT, &[Variant::I4(0)])?;
            }
        }
        if let Some(n) = v {
            self.call(id, "VerticalAlignment", Call::PUT, &[Variant::I4(n)])?;
        }
        if let Some(b) = wrap {
            self.call(id, "WrapText", Call::PUT, &[Variant::Bool(b)])?;
            if let Some(s @ Variant::Bool(true)) = shrink {
                self.call(id, "ShrinkToFit", Call::PUT, &[s])?;
            }
        }
        if let Some(n) = orient {
            self.call(id, "Orientation", Call::PUT, &[Variant::I4(n)])?;
        }
        TRUE
    }

    fn cell_protection(&mut self, args: &[Operand]) -> Result<Value, XlmError> {
        let mut puts = Vec::new();
        for (i, member) in [(0, "Locked"), (1, "FormulaHidden")] {
            match flag_opt(&self.a(args, i)) {
                Opt::Skip => {}
                Opt::Set(b) => puts.push((member, b)),
                Opt::Bad => return VALUE,
            }
        }
        let id = self.selection()?;
        for (member, b) in puts {
            self.call(id, member, Call::PUT, &[Variant::Bool(b)])?;
        }
        TRUE
    }

    fn patterns(&mut self, args: &[Operand]) -> Result<Value, XlmError> {
        let pat = match self.a(args, 0) {
            A::Miss => None,
            a => match a.int() {
                Some(n @ 0..=18) => Some(n as usize),
                _ => return VALUE,
            },
        };
        let mut cols = [None, None];
        for (k, slot) in cols.iter_mut().enumerate() {
            *slot = match color_opt(&self.a(args, k + 1), false) {
                Opt::Skip => None,
                Opt::Set(n) => Some(n),
                Opt::Bad => return VALUE,
            };
        }
        let newui = matches!(self.a(args, 3).flag(), Some(true));
        let (sheet, areas) = self.sel_areas();
        let Ok(book) = self.app.book_mut(self.book) else {
            return TRUE;
        };
        for rect in areas {
            book.wb.set_style_with(sheet, rect, &|s| {
                let mut s = s.clone();
                let old = s.fill.clone();
                let p = pat.map_or(
                    if old.pattern == Pattern::None {
                        Pattern::Solid
                    } else {
                        old.pattern
                    },
                    |n| Pattern::ALL[n],
                );
                if p == Pattern::None {
                    s.fill = Fill {
                        cleared: true,
                        ..Fill::default()
                    };
                    return s;
                }

                let (fg, bg) = if old.pattern == Pattern::Solid {
                    (old.color, old.pattern_color)
                } else {
                    (old.pattern_color, old.color)
                };
                let swap = p == Pattern::Solid
                    && (newui || (pat.is_none() && old.pattern == Pattern::None));
                let put = |old: CellColor, n: Option<i32>| match (n, old) {
                    (None, _) => old,
                    (Some(0), CellColor::Rgb(_)) => CellColor::Rgb(57 << 16),
                    (Some(0), _) => CellColor::Auto,
                    (Some(n), CellColor::Rgb(_)) => CellColor::Rgb((n as u32) << 16),
                    (Some(n), _) => CellColor::Indexed(n as u8 + 7),
                };
                let (fg, bg) = if swap {
                    (put(fg, cols[1]), put(bg, cols[0]))
                } else {
                    (put(fg, cols[0]), put(bg, cols[1]))
                };
                (s.fill.color, s.fill.pattern_color) = if p == Pattern::Solid {
                    (fg, bg)
                } else {
                    (bg, fg)
                };
                s.fill.pattern = p;
                s.fill.cleared = false;
                s
            });
        }
        TRUE
    }

    fn border(&mut self, args: &[Operand]) -> Result<Value, XlmError> {
        let mut lines: [Option<BorderLine>; 5] = [None; 5];
        for (i, slot) in lines.iter_mut().enumerate() {
            *slot = match self.a(args, i) {
                A::Miss => None,
                a => match a.int() {
                    Some(n @ 0..=13) => Some(BorderLine::ALL[n as usize]),

                    Some(-1) => None,
                    _ => return VALUE,
                },
            };
        }
        let shade = self.a(args, 5).flag();
        let mut colors: [Option<CellColor>; 5] = [None; 5];
        for (i, slot) in colors.iter_mut().enumerate() {
            *slot = match self.a(args, 6 + i) {
                A::Miss => None,
                a => match a.int() {
                    Some(n @ 1..=56) => Some(CellColor::Indexed(n as u8 + 7)),
                    Some(_) => Some(CellColor::Auto),
                    None => return VALUE,
                },
            };
        }

        type Spec = (Option<BorderLine>, Option<CellColor>, bool);

        let outer = |k: usize| -> Spec {
            let outline = lines[0].filter(|l| *l != BorderLine::None);
            let color = if outline.is_some() {
                colors[0]
            } else {
                colors[0].or(colors[k])
            };
            (outline.or(lines[k]), color, outline.is_some())
        };
        let outline_given = lines[0].is_some_and(|l| l != BorderLine::None);

        let inner = |a: usize, b: usize| -> Spec {
            let color = if outline_given {
                colors[b].or(colors[a])
            } else {
                colors[a].filter(|_| lines[b].is_some()).or(colors[b])
            };
            (lines[a].or(lines[b]), color, false)
        };
        let set = |old: Border, (line, color, reset): Spec| -> Border {
            let given = line.is_some();
            let line = line.unwrap_or(old.line);
            Border {
                line: if color.is_some() && line == BorderLine::None {
                    BorderLine::Thin
                } else {
                    line
                },
                color: match (color, given) {
                    (Some(c), _) => c,
                    (None, g) if g && (reset || line == BorderLine::None) => CellColor::Auto,
                    (None, _) => old.color,
                },
            }
        };
        let (sheet, areas) = self.sel_areas();
        let Ok(book) = self.app.book_mut(self.book) else {
            return TRUE;
        };

        let split = |lo: u32, hi: u32| -> Vec<(u32, u32, bool, bool)> {
            if lo == hi {
                vec![(lo, lo, true, true)]
            } else if hi == lo + 1 {
                vec![(lo, lo, true, false), (hi, hi, false, true)]
            } else {
                vec![
                    (lo, lo, true, false),
                    (lo + 1, hi - 1, false, false),
                    (hi, hi, false, true),
                ]
            }
        };
        for rect in areas {
            for &(r0, r1, top, bottom) in &split(rect.r0, rect.r1) {
                for &(c0, c1, left, right) in &split(rect.c0, rect.c1) {
                    let l = if left { outer(1) } else { inner(1, 2) };
                    let r = if right { outer(2) } else { inner(1, 2) };
                    let t = if top { outer(3) } else { inner(3, 4) };
                    let b = if bottom { outer(4) } else { inner(3, 4) };
                    book.wb
                        .set_style_with(sheet, Rect::new(r0, c0, r1, c1), &|s| {
                            let mut s = s.clone();
                            s.borders.left = set(s.borders.left, l);
                            s.borders.right = set(s.borders.right, r);
                            s.borders.top = set(s.borders.top, t);
                            s.borders.bottom = set(s.borders.bottom, b);
                            if let Some(on) = shade {
                                s.fill = Fill::default();
                                if on {
                                    s.fill.pattern = Pattern::ALL[17];
                                }
                            }
                            s
                        });
                }
            }
        }
        TRUE
    }

    fn apply_style(&mut self, args: &[Operand]) -> Result<Value, XlmError> {
        let name = match self.a(args, 0) {
            A::Miss => return TRUE,
            A::Text(t) if !t.is_empty() => t,
            _ => return VALUE,
        };
        if self.style_obj(&name)?.is_none() {
            return VALUE;
        }
        let id = self.selection()?;
        self.call(id, "Style", Call::PUT, &[Variant::Bstr(name)])?;
        TRUE
    }

    fn delete_style(&mut self, args: &[Operand]) -> Result<Value, XlmError> {
        let A::Text(name) = self.a(args, 0) else {
            return VALUE;
        };
        if name.is_empty() || name.eq_ignore_ascii_case("Normal") {
            return VALUE;
        }
        let Some(st) = self.style_obj(&name)? else {
            return VALUE;
        };
        self.call(st, "Delete", Call::VB, &[])?;
        TRUE
    }

    fn style_obj(&mut self, name: &str) -> Result<Option<xlcalc_variant::ObjectId>, XlmError> {
        let wb = self.active_book_obj()?;
        let styles = Self::dispatch(self.call(wb, "Styles", Call::GET, &[])?)?;
        let item = self.app.invoke(
            styles,
            "Item",
            Call::GET,
            &[Variant::Bstr(name.to_string())],
            &[],
        );
        Ok(match item {
            Ok(Variant::Dispatch(Some(id))) => Some(id),
            _ => None,
        })
    }
}
