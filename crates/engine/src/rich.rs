use crate::structure::{Axis, Shift};
use crate::style::{CellColor, Font, Underline};
use crate::{CellAddr, Rect, SheetId, Value, Workbook};
use std::collections::BTreeMap;
use std::sync::Arc;

#[derive(Clone, PartialEq, Debug)]
pub enum RunValue {
    Bool(bool),
    Int(i32),
    Num(f64),
    Text(Arc<str>),
}

#[derive(Clone, Copy, Default, PartialEq, Eq, Debug)]
pub struct FontExtra {
    pub charset: Option<u8>,
    pub condense: bool,
    pub extend: bool,
}

#[derive(Clone, PartialEq, Debug)]
pub struct RunFont {
    pub font: Font,
    pub extra: FontExtra,
}

#[derive(Clone, PartialEq, Debug)]
pub struct PhoneticRun {
    pub text: Arc<str>,
    pub from: u32,
    pub to: u32,
}

#[derive(Clone, PartialEq, Debug)]
pub struct Phonetic {
    pub runs: Vec<PhoneticRun>,
    pub font: RunFont,

    pub font_id: u32,

    pub kind: u8,

    pub align: u8,
}

#[derive(Clone, Debug)]
pub struct Run {
    pub from: usize,
    pub to: usize,
    pub parts: Vec<(&'static str, RunValue)>,

    pub file: Option<RunFont>,
}

#[derive(Clone, Debug)]
pub struct RichText {
    pub text: Arc<str>,
    pub runs: Vec<Run>,
    pub phonetic: Option<Arc<Phonetic>>,
}

pub type RichMap = BTreeMap<(u32, u32), RichText>;

fn bgr(rgb: u32) -> u32 {
    ((rgb & 0xFF) << 16) | (rgb & 0xFF00) | ((rgb >> 16) & 0xFF)
}

fn underline_code(u: Underline) -> i32 {
    match u {
        Underline::None => -4142,
        Underline::Single => 2,
        Underline::Double => -4119,
        Underline::SingleAccounting => 4,
        Underline::DoubleAccounting => 5,
    }
}

fn underline_of(code: i32) -> Underline {
    match code {
        2 => Underline::Single,
        -4119 => Underline::Double,
        4 => Underline::SingleAccounting,
        5 => Underline::DoubleAccounting,
        _ => Underline::None,
    }
}

impl Workbook {
    pub fn rich_text(&self, at: CellAddr) -> Option<&RichText> {
        let rich = self.rich.get(&at.sheet)?.get(&(at.row, at.col))?;
        let live = self.formula_text(at).is_none()
            && matches!(self.value(at), Value::Text(t) if *t == *rich.text);
        live.then_some(rich)
    }

    pub fn rich_cells(&self, sheet: SheetId, r: Rect) -> Vec<(u32, u32)> {
        let Some(m) = self.rich.get(&sheet) else {
            return Vec::new();
        };
        m.range((r.r0, 0)..=(r.r1, u32::MAX))
            .map(|(k, _)| *k)
            .filter(|&(row, col)| {
                r.contains(row, col) && self.rich_text(CellAddr::new(sheet, row, col)).is_some()
            })
            .collect()
    }

    pub fn rich_set(&mut self, at: CellAddr, rich: RichText) {
        self.rich
            .entry(at.sheet)
            .or_default()
            .insert((at.row, at.col), rich);
    }

    pub fn rich_unset(&mut self, at: CellAddr, names: &[&str]) {
        let found = self
            .rich
            .get_mut(&at.sheet)
            .and_then(|m| m.get_mut(&(at.row, at.col)));
        if let Some(rich) = found {
            for run in &mut rich.runs {
                run.parts.retain(|(n, _)| !names.contains(n));
            }
        }
    }

    pub fn rich_load(
        &mut self,
        at: CellAddr,
        runs: &[(usize, usize, Option<RunFont>)],
        phonetic: Option<Phonetic>,
    ) {
        let Value::Text(text) = self.value(at) else {
            return;
        };
        let text: Arc<str> = Arc::from(text.to_string().as_str());
        let theme = self.style_table().theme.clone();
        let runs: Vec<Run> = runs
            .iter()
            .filter_map(|(from, to, font)| {
                let f = &font.as_ref()?.font;
                let flag = RunValue::Bool;
                let parts = vec![
                    ("Name", RunValue::Text(f.name.clone())),
                    ("Size", RunValue::Num(f64::from(f.size_twips) / 20.0)),
                    ("Bold", flag(f.bold)),
                    ("Italic", flag(f.italic)),
                    ("Underline", RunValue::Int(underline_code(f.underline))),
                    ("Strikethrough", flag(f.strikethrough)),
                    ("Superscript", flag(f.superscript)),
                    ("Subscript", flag(f.subscript)),
                    ("OutlineFont", flag(f.outline)),
                    ("Shadow", flag(f.shadow)),
                    (
                        "Color",
                        RunValue::Num(f64::from(bgr(theme.rgb_cv(f.color, 0)))),
                    ),
                ];
                Some(Run {
                    from: *from,
                    to: *to,
                    parts,
                    file: font.clone(),
                })
            })
            .collect();
        if !runs.is_empty() || phonetic.is_some() {
            let phonetic = phonetic.map(Arc::new);
            self.rich_set(
                at,
                RichText {
                    text,
                    runs,
                    phonetic,
                },
            );
        }
    }

    pub fn rich_segments(&self, at: CellAddr) -> Option<Vec<(String, Option<RunFont>)>> {
        let rich = self.rich_text(at)?;
        let cell = self.cell_style(at).font.clone();
        let theme = &self.style_table().theme;
        let units: Vec<u16> = crate::text::wide::units(&rich.text).collect();

        let mut cuts: Vec<usize> = rich
            .runs
            .iter()
            .flat_map(|r| [r.from, r.to])
            .chain([0, units.len()])
            .filter(|&i| i <= units.len())
            .collect();
        cuts.sort_unstable();
        cuts.dedup();
        let mut out: Vec<(String, RunFont)> = Vec::new();
        for w in cuts.windows(2) {
            let (from, to) = (w[0], w[1]);
            let covering = || rich.runs.iter().filter(|r| r.from <= from && to <= r.to);

            let mut font = cell.clone();
            let from_file = covering().filter_map(|r| r.file.as_ref()).next_back();
            let file = from_file.map(|f| &f.font);
            if let Some(f) = file {
                font.scheme = f.scheme;
                font.family = f.family;
            }
            for (name, v) in covering().flat_map(|r| &r.parts) {
                match (*name, v) {
                    ("Name", RunValue::Text(t)) => {
                        if !t.eq_ignore_ascii_case(&font.name) && file.is_none_or(|f| f.name != *t)
                        {
                            font.scheme = Default::default();
                            font.family = 0;
                        }
                        font.name = t.clone();
                    }
                    ("Size", RunValue::Num(n)) => font.size_twips = (n * 20.0).round() as u32,
                    ("Size", RunValue::Int(n)) => font.size_twips = n.unsigned_abs() * 20,
                    ("Bold", RunValue::Bool(b)) => font.bold = *b,
                    ("Italic", RunValue::Bool(b)) => font.italic = *b,
                    ("Underline", RunValue::Int(u)) => font.underline = underline_of(*u),
                    ("Underline", RunValue::Bool(b)) => {
                        font.underline = underline_of(if *b { 2 } else { -4142 });
                    }
                    ("Strikethrough", RunValue::Bool(b)) => font.strikethrough = *b,
                    ("Superscript", RunValue::Bool(b)) => {
                        font.superscript = *b;
                        font.subscript &= !*b;
                    }
                    ("Subscript", RunValue::Bool(b)) => {
                        font.subscript = *b;
                        font.superscript &= !*b;
                    }
                    ("OutlineFont", RunValue::Bool(b)) => font.outline = *b,
                    ("Shadow", RunValue::Bool(b)) => font.shadow = *b,
                    ("Color", RunValue::Num(c)) => {
                        let rgb = bgr(*c as u32);
                        font.color = match file {
                            Some(f) if theme.rgb_cv(f.color, 0) == rgb => f.color,
                            _ => CellColor::Rgb(rgb),
                        };
                    }
                    _ => {}
                }
            }

            let extra = match from_file {
                Some(f) if f.font.name == font.name => f.extra,
                _ => FontExtra::default(),
            };
            let font = RunFont { font, extra };
            let piece = crate::text::wide::from_units(&units[from..to]);
            match out.last_mut() {
                Some(last) if last.1 == font => {
                    let joined = std::mem::take(&mut last.0) + &piece;
                    last.0 = crate::text::wide::join_halves(joined);
                }
                _ => out.push((piece, font)),
            }
        }
        let cell = RunFont {
            font: cell,
            extra: FontExtra::default(),
        };
        if out.iter().all(|(_, f)| *f == cell) {
            return None;
        }
        Some(
            out.into_iter()
                .enumerate()
                .map(|(i, (t, f))| (t, (i > 0 || f != cell).then_some(f)))
                .collect(),
        )
    }

    pub fn rich_clear(&mut self, sheet: SheetId, r: &Rect) {
        if let Some(m) = self.rich.get_mut(&sheet) {
            m.retain(|&(row, col), _| !r.contains(row, col));
        }
    }

    pub(crate) fn rich_paste(
        &mut self,
        ss: SheetId,
        src: Rect,
        ds: SheetId,
        pasted: Rect,
        cut: bool,
    ) {
        let inside: Vec<((u32, u32), RichText)> = self
            .rich
            .get(&ss)
            .into_iter()
            .flatten()
            .filter(|((r, c), _)| src.contains(*r, *c))
            .map(|(k, v)| (*k, v.clone()))
            .collect();
        if cut {
            self.rich_clear(ss, &src);
        }
        self.rich_clear(ds, &pasted);
        if inside.is_empty() {
            return;
        }
        let (rows, cols) = (src.rows(), src.cols());
        let mut r = pasted.r0;
        while r + rows - 1 <= pasted.r1 {
            let mut c = pasted.c0;
            while c + cols - 1 <= pasted.c1 {
                for ((nr, nc), rich) in &inside {
                    let at = CellAddr::new(ds, nr - src.r0 + r, nc - src.c0 + c);
                    self.rich_set(at, rich.clone());
                }
                c += cols;
            }
            r += rows;
        }
    }

    pub(crate) fn rich_shift(&mut self, s: &Shift) {
        let rows = s.axis == Axis::Rows;
        let Some(m) = self.rich.get_mut(&s.sheet) else {
            return;
        };
        let old = std::mem::take(m);
        for ((r, c), rich) in old {
            let (along, across) = if rows { (r, c) } else { (c, r) };
            let outside = s.limit.is_some_and(|(p0, p1)| across < p0 || across > p1);
            let to = if outside {
                Some(along)
            } else {
                s.map_span(along, along).map(|(a, _)| a)
            };
            if let Some(a) = to {
                m.insert(if rows { (a, c) } else { (r, a) }, rich);
            }
        }
    }

    pub(crate) fn rich_sort_pairs(&mut self, pairs: &[(CellAddr, CellAddr)]) {
        let Some(first) = pairs.first() else {
            return;
        };
        let Some(m) = self.rich.get_mut(&first.0.sheet) else {
            return;
        };
        let moved: Vec<(CellAddr, Option<RichText>)> = pairs
            .iter()
            .map(|(from, to)| (*to, m.get(&(from.row, from.col)).cloned()))
            .collect();
        for (_, to) in pairs {
            m.remove(&(to.row, to.col));
        }
        for (to, rich) in moved {
            if let Some(rich) = rich {
                m.insert((to.row, to.col), rich);
            }
        }
    }

    pub(crate) fn rich_copy_sheet(&mut self, src: SheetId, new: SheetId) {
        if let Some(m) = self.rich.get(&src).cloned() {
            self.rich.insert(new, m);
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn runs_follow_their_cell_and_die_with_the_text() {
        let mut wb = Workbook::new();
        let sheet = wb.active_sheet();
        let at = CellAddr::new(sheet, 0, 0);
        wb.set_value(at, Value::Text("abcd".into()));
        let mut bold = wb.cell_style(at).font.clone();
        bold.bold = true;
        let bold = RunFont {
            font: bold,
            extra: FontExtra::default(),
        };
        wb.rich_load(at, &[(0, 2, None), (2, 4, Some(bold))], None);
        let pieces = wb.rich_segments(at).unwrap();
        assert_eq!(pieces.len(), 2);
        assert!(pieces[0].0 == "ab" && pieces[0].1.is_none());
        assert!(pieces[1].0 == "cd" && pieces[1].1.as_ref().unwrap().font.bold);
        wb.rich_unset(at, &["Bold"]);
        assert!(wb.rich_segments(at).is_none());
        wb.set_value(at, Value::Text("x".into()));
        assert!(wb.rich_text(at).is_none());
    }

    #[test]
    fn positions_are_utf16_units_and_extras_make_a_piece() {
        let mut wb = Workbook::new();
        let sheet = wb.active_sheet();
        let at = CellAddr::new(sheet, 0, 0);
        wb.set_value(at, Value::Text("ab\u{1F600}cdef".into()));
        let mut font = wb.cell_style(at).font.clone();
        font.bold = true;
        let bold = RunFont {
            font,
            extra: FontExtra::default(),
        };
        wb.rich_load(at, &[(0, 4, None), (4, 6, Some(bold.clone()))], None);
        let pieces = wb.rich_segments(at).unwrap();
        let texts: Vec<&str> = pieces.iter().map(|p| p.0.as_str()).collect();
        assert_eq!(texts, ["ab\u{1F600}", "cd", "ef"]);

        wb.rich_load(at, &[(0, 3, None), (3, 8, Some(bold.clone()))], None);
        let pieces = wb.rich_segments(at).unwrap();
        assert_eq!(pieces.len(), 2);
        let units = |i: usize| crate::text::wide::units(&pieces[i].0).collect::<Vec<u16>>();
        assert_eq!(units(0), [0x61, 0x62, 0xD83D]);
        assert_eq!(units(1)[0], 0xDE00);

        let charset = RunFont {
            font: wb.cell_style(at).font.clone(),
            extra: FontExtra {
                charset: Some(128),
                ..FontExtra::default()
            },
        };
        wb.rich_load(at, &[(0, 4, None), (4, 8, Some(charset))], None);
        let pieces = wb.rich_segments(at).unwrap();
        assert_eq!(pieces[1].1.as_ref().unwrap().extra.charset, Some(128));

        let mut rich = wb.rich_text(at).unwrap().clone();
        rich.runs = vec![Run {
            from: 0,
            to: 2,
            parts: vec![("Size", RunValue::Int(20))],
            file: None,
        }];
        wb.rich_set(at, rich);
        let pieces = wb.rich_segments(at).unwrap();
        assert_eq!(pieces[0].1.as_ref().unwrap().font.size_twips, 400);
    }
}
