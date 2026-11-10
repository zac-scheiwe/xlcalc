use xlcalc::sparkline::{SparkGroup, Sparkline, POINT_KINDS};
use xlcalc::{Rect, SheetId};
use xlcalc_variant::Variant;

use crate::app::{unsupported, Application, BookId, Mode, Object};
use crate::conv::Args;
use crate::error::OmError;
use crate::typelib::{InvokeKind, Member};

#[derive(Clone, Debug)]
pub(crate) enum SparkPart {
    Groups(Rect),
    Group(u32),

    Line(u32, usize),
    Points(u32),

    Color(u32, usize),

    Fmt(u32, usize),

    SeriesColor(u32),
    Axes(u32),
    VAxis(u32),
    HAxis(u32),

    HColor(u32),
    HFmt(u32),
}

impl SparkPart {
    pub(crate) fn interface_name(&self) -> &'static str {
        match self {
            SparkPart::Groups(_) => "SparklineGroups",
            SparkPart::Group(_) => "SparklineGroup",
            SparkPart::Line(..) => "Sparkline",
            SparkPart::Points(_) => "SparkPoints",
            SparkPart::Color(..) | SparkPart::HColor(_) => "SparkColor",
            SparkPart::Fmt(..) | SparkPart::SeriesColor(_) | SparkPart::HFmt(_) => "FormatColor",
            SparkPart::Axes(_) => "SparkAxes",
            SparkPart::VAxis(_) => "SparkVerticalAxis",
            SparkPart::HAxis(_) => "SparkHorizontalAxis",
        }
    }
}

fn put_i(a: &Args) -> Result<i32, OmError> {
    Ok(crate::controls::put_num(a)? as i32)
}

fn fail() -> OmError {
    OmError::app_defined()
}

fn cells_of(r: &Rect) -> Vec<(u32, u32)> {
    let mut v = Vec::new();
    for row in r.r0..=r.r1 {
        for col in r.c0..=r.c1 {
            v.push((row, col));
        }
    }
    v
}

fn a1(r: &Rect, abs: bool) -> String {
    let cell = |row: u32, col: u32| {
        if abs {
            format!("${}${}", xlcalc::col_to_letters(col), row + 1)
        } else {
            format!("{}{}", xlcalc::col_to_letters(col), row + 1)
        }
    };
    format!("{}:{}", cell(r.r0, r.c0), cell(r.r1, r.c1))
}

fn quote(name: &str) -> String {
    let plain = !name.is_empty()
        && name.chars().all(|c| c.is_alphanumeric() || c == '_')
        && !name.chars().next().is_some_and(|c| c.is_ascii_digit());
    if plain {
        name.to_string()
    } else {
        format!("'{}'", name.replace('\'', "''"))
    }
}

impl Application<'_> {
    fn group_ref(&self, b: BookId, s: SheetId, id: u32) -> Result<&SparkGroup, OmError> {
        self.book(b)?
            .wb
            .spark_groups(s)
            .iter()
            .find(|g| g.id == id)
            .ok_or_else(OmError::object_required)
    }

    fn group_mut(&mut self, b: BookId, s: SheetId, id: u32) -> Result<&mut SparkGroup, OmError> {
        self.book_mut(b)?
            .wb
            .spark_groups_mut(s)
            .iter_mut()
            .find(|g| g.id == id)
            .ok_or_else(OmError::object_required)
    }

    fn spark_src_text(&self, b: BookId, s: SheetId, g: &SparkGroup, l: &Sparkline) -> String {
        let wb = &self.book(b).map(|x| &x.wb);
        let sheet = match wb {
            Ok(w) if l.src_sheet != s => w
                .sheet(l.src_sheet)
                .map(|x| format!("{}!", quote(&x.name)))
                .unwrap_or_default(),
            _ => String::new(),
        };
        let single = l.src.r0 == l.src.r1 && l.src.c0 == l.src.c1 && g.lines.len() == 1;
        if single {
            let t = a1(&l.src, g.abs);
            let first = t.split(':').next().unwrap_or("").to_string();
            format!("{sheet}{first}")
        } else {
            format!("{sheet}{}", a1(&l.src, g.abs))
        }
    }

    fn group_source(&self, b: BookId, s: SheetId, g: &SparkGroup) -> String {
        let Some(first) = g.lines.first() else {
            return String::new();
        };
        if let Some((ss, r)) = g.src {
            let sheet = if ss == s {
                String::new()
            } else {
                self.book(b)
                    .ok()
                    .and_then(|x| x.wb.sheet(ss).map(|y| format!("{}!", quote(&y.name))))
                    .unwrap_or_default()
            };
            return format!("{sheet}{}", a1(&r, g.abs));
        }
        let same_sheet = g.lines.iter().all(|l| l.src_sheet == first.src_sheet);
        let bound = g
            .lines
            .iter()
            .fold(first.src, |b, l| b.union_bounds(&l.src));
        if same_sheet && g.sources_tile() {
            let sheet = if first.src_sheet == s {
                String::new()
            } else {
                self.book(b)
                    .ok()
                    .and_then(|x| {
                        x.wb.sheet(first.src_sheet)
                            .map(|y| format!("{}!", quote(&y.name)))
                    })
                    .unwrap_or_default()
            };
            format!("{sheet}{}", a1(&bound, g.abs))
        } else {
            g.lines
                .iter()
                .map(|l| {
                    let mut one = g.clone();
                    one.lines = vec![l.clone()];
                    let t = self.spark_src_text(b, s, &one, l);
                    if l.src.r0 == l.src.r1 && l.src.c0 == l.src.c1 {
                        format!("{}:{}", t, t.split(':').next_back().unwrap_or(""))
                    } else {
                        t
                    }
                })
                .collect::<Vec<_>>()
                .join(",")
        }
    }

    fn groups_in(&self, b: BookId, s: SheetId, r: &Rect) -> Result<Vec<u32>, OmError> {
        let mut v: Vec<(u32, u32)> = self
            .book(b)?
            .wb
            .spark_groups(s)
            .iter()
            .filter(|g| g.touches(r))
            .map(|g| (g.slot, g.id))
            .collect();
        v.sort_by_key(|x| std::cmp::Reverse(x.0));
        Ok(v.into_iter().map(|(_, id)| id).collect())
    }

    fn parse_spark_source(&self, b: BookId, text: &str) -> Result<(SheetId, Rect, bool), OmError> {
        let wb = &self.book(b)?.wb;
        let (sheet, cells) = match text.rsplit_once('!') {
            Some((name, cells)) => {
                let name = name
                    .strip_prefix('\'')
                    .and_then(|x| x.strip_suffix('\''))
                    .map_or_else(|| name.to_string(), |x| x.replace("''", "'"));
                let sheet = wb.sheet_by_name(&name).ok_or_else(fail)?.id;
                (sheet, cells)
            }
            None => (wb.active_sheet(), text),
        };
        let rect = xlcalc::parse_a1_rect(&cells.to_uppercase()).ok_or_else(fail)?;
        Ok((sheet, rect, cells.contains('$')))
    }

    fn spark_add(
        &mut self,
        b: BookId,
        s: SheetId,
        loc: Rect,
        a: &Args,
    ) -> Result<Variant, OmError> {
        let kind = a.i32(0).map_err(|_| fail())?.ok_or_else(fail)?;
        if !(1..=3).contains(&kind) {
            return Err(fail());
        }
        let text = a.string(1).map_err(|_| fail())?.ok_or_else(fail)?;
        let (src_sheet, src, abs) = self.parse_spark_source(b, &text)?;
        let cells = cells_of(&loc);
        let n = cells.len() as u32;
        let (rows, cols) = (src.rows(), src.cols());
        let by_rows = rows == n;
        if !by_rows && cols != n {
            return Err(fail());
        }
        let mut lines = Vec::new();
        for (i, (row, col)) in cells.iter().enumerate() {
            let i = i as u32;
            let part = if by_rows {
                Rect::new(src.r0 + i, src.c0, src.r0 + i, src.c1)
            } else {
                Rect::new(src.r0, src.c0 + i, src.r1, src.c0 + i)
            };
            lines.push(Sparkline {
                row: *row,
                col: *col,
                src_sheet,
                src: part,
            });
        }
        let wb = &mut self.book_mut(b)?.wb;
        wb.spark_clear(s, &loc);
        let id = wb.next_spark_id();
        let mut g = SparkGroup::new(id, kind, abs);
        g.lines = lines;
        g.src = Some((src_sheet, src));
        g.plot_by = i32::from(by_rows && cols == n && n > 1);
        g.slot = wb.spark_take_slot(s, id);
        wb.spark_groups_mut(s).push(g);
        Ok(self.alloc(Object::Spark(b, s, SparkPart::Group(id))))
    }

    pub(crate) fn spark_member(
        &mut self,
        b: BookId,
        s: SheetId,
        part: &SparkPart,
        m: &'static Member,
        a: &Args,
    ) -> Result<Variant, OmError> {
        use InvokeKind::*;
        let put = matches!(m.kind, Put | PutRef);
        match m.name {
            "Application" if !put => return Ok(Variant::Dispatch(Some(self.handle()))),
            "Creator" if !put => return Ok(Variant::I4(0x5845_4C20)),
            _ => {}
        }
        match part {
            SparkPart::Groups(r) => {
                let ids = self.groups_in(b, s, r)?;
                match m.name {
                    "Count" => Ok(Variant::I4(ids.len() as i32)),
                    "Item" | "_Default" => {
                        let i = a.i32(0).map_err(|_| fail())?.ok_or_else(fail)?;
                        let id = usize::try_from(i - 1)
                            .ok()
                            .and_then(|i| ids.get(i))
                            .ok_or_else(fail)?;
                        Ok(self.alloc(Object::Spark(b, s, SparkPart::Group(*id))))
                    }
                    "_NewEnum" => {
                        let items = ids
                            .iter()
                            .map(|id| self.alloc(Object::Spark(b, s, SparkPart::Group(*id))))
                            .collect();
                        Ok(self.alloc_enumerator(items))
                    }
                    "Add" => self.spark_add(b, s, *r, a),
                    "Clear" => {
                        self.book_mut(b)?.wb.spark_clear(s, r);
                        Ok(Variant::Empty)
                    }
                    "ClearGroups" => {
                        let r = *r;
                        self.book_mut(b)?.wb.spark_remove(s, |g| g.touches(&r));
                        Ok(Variant::Empty)
                    }
                    "Parent" => Ok(self.alloc(Object::Worksheet(b, s))),
                    _ => Err(unsupported("SparklineGroups", m)),
                }
            }
            SparkPart::Group(id) => self.spark_group_member(b, s, *id, m, a),
            SparkPart::Line(id, i) => {
                let g = self.group_ref(b, s, *id)?;
                let l = g
                    .lines
                    .get(*i)
                    .ok_or_else(OmError::object_required)?
                    .clone();
                match m.name {
                    "Location" if !put => {
                        self.new_range(b, s, Rect::cell(l.row, l.col), Mode::Cells)
                    }
                    "SourceData" if !put => {
                        let g = g.clone();
                        Ok(Variant::Bstr(self.spark_src_text(b, s, &g, &l)))
                    }
                    "Parent" => Ok(self.alloc(Object::Spark(b, s, SparkPart::Group(*id)))),
                    _ => Err(unsupported("Sparkline", m)),
                }
            }
            SparkPart::Points(id) => {
                self.group_ref(b, s, *id)?;
                let k = POINT_KINDS
                    .iter()
                    .position(|n| n.eq_ignore_ascii_case(m.name))
                    .ok_or_else(|| unsupported("SparkPoints", m));
                match (m.name, k) {
                    ("Parent", _) => Ok(self.alloc(Object::Spark(b, s, SparkPart::Group(*id)))),
                    (_, Ok(k)) => Ok(self.alloc(Object::Spark(b, s, SparkPart::Color(*id, k)))),
                    (_, Err(e)) => Err(e),
                }
            }
            SparkPart::Color(id, k) => match m.name {
                "Visible" if !put => {
                    Ok(Variant::Bool(self.group_ref(b, s, *id)?.points[*k].visible))
                }
                "Visible" => {
                    let v = crate::chartparts::put_bool(a)?;
                    self.group_mut(b, s, *id)?.points[*k].visible = v;
                    Ok(Variant::Empty)
                }
                "Color" if !put => Ok(self.alloc(Object::Spark(b, s, SparkPart::Fmt(*id, *k)))),
                "Parent" => Ok(self.alloc(Object::Spark(b, s, SparkPart::Points(*id)))),
                _ => Err(unsupported("SparkColor", m)),
            },
            SparkPart::Fmt(id, k) => self.spark_color_member(b, s, *id, Some(*k), m, a),
            SparkPart::SeriesColor(id) => self.spark_color_member(b, s, *id, None, m, a),
            SparkPart::Axes(id) => {
                self.group_ref(b, s, *id)?;
                match m.name {
                    "Vertical" => Ok(self.alloc(Object::Spark(b, s, SparkPart::VAxis(*id)))),
                    "Horizontal" => Ok(self.alloc(Object::Spark(b, s, SparkPart::HAxis(*id)))),
                    "Parent" => Ok(self.alloc(Object::Spark(b, s, SparkPart::Group(*id)))),
                    _ => Err(unsupported("SparkAxes", m)),
                }
            }
            SparkPart::VAxis(id) => {
                let scale = |v: Option<i32>| match v {
                    Some(n @ 1..=3) => Ok(n),
                    _ => Err(fail()),
                };
                match (m.name, put) {
                    ("MinScaleType", false) => Ok(Variant::I4(self.group_ref(b, s, *id)?.min_type)),
                    ("MaxScaleType", false) => Ok(Variant::I4(self.group_ref(b, s, *id)?.max_type)),
                    ("CustomMinScaleValue", false) => {
                        Ok(Variant::R8(self.group_ref(b, s, *id)?.min_value))
                    }
                    ("CustomMaxScaleValue", false) => {
                        Ok(Variant::R8(self.group_ref(b, s, *id)?.max_value))
                    }
                    ("MinScaleType", true) => {
                        let v = scale(Some(put_i(a)?))?;
                        self.group_mut(b, s, *id)?.min_type = v;
                        Ok(Variant::Empty)
                    }
                    ("MaxScaleType", true) => {
                        let v = scale(Some(put_i(a)?))?;
                        self.group_mut(b, s, *id)?.max_type = v;
                        Ok(Variant::Empty)
                    }
                    ("CustomMinScaleValue" | "CustomMaxScaleValue", true) => {
                        let v = crate::controls::put_num(a)?;
                        let g = self.group_mut(b, s, *id)?;
                        if m.name == "CustomMinScaleValue" {
                            g.min_value = v;
                            g.min_type = 3;
                        } else {
                            g.max_value = v;
                            g.max_type = 3;
                        }
                        Ok(Variant::Empty)
                    }
                    ("Parent", _) => Ok(self.alloc(Object::Spark(b, s, SparkPart::Axes(*id)))),
                    _ => Err(unsupported("SparkVerticalAxis", m)),
                }
            }
            SparkPart::HAxis(id) => match (m.name, put) {
                ("IsDateAxis", false) => {
                    self.group_ref(b, s, *id)?;
                    Ok(Variant::Bool(false))
                }
                ("RightToLeftPlotOrder", false) => {
                    Ok(Variant::Bool(self.group_ref(b, s, *id)?.right_to_left))
                }
                ("RightToLeftPlotOrder", true) => {
                    let v = crate::chartparts::put_bool(a)?;
                    self.group_mut(b, s, *id)?.right_to_left = v;
                    Ok(Variant::Empty)
                }
                ("Axis", false) => Ok(self.alloc(Object::Spark(b, s, SparkPart::HColor(*id)))),
                ("Parent", _) => Ok(self.alloc(Object::Spark(b, s, SparkPart::Axes(*id)))),
                _ => Err(unsupported("SparkHorizontalAxis", m)),
            },
            SparkPart::HColor(id) => match (m.name, put) {
                ("Visible", false) => Ok(Variant::Bool(self.group_ref(b, s, *id)?.axis_visible)),
                ("Visible", true) => {
                    let v = crate::chartparts::put_bool(a)?;
                    self.group_mut(b, s, *id)?.axis_visible = v;
                    Ok(Variant::Empty)
                }
                ("Color", false) => Ok(self.alloc(Object::Spark(b, s, SparkPart::HFmt(*id)))),
                _ => Err(unsupported("SparkColor", m)),
            },
            SparkPart::HFmt(id) => match (m.name, put) {
                ("Color", false) => Ok(Variant::I4(self.group_ref(b, s, *id)?.axis_color)),
                ("Color", true) => {
                    let v = put_i(a)?;
                    self.group_mut(b, s, *id)?.axis_color = v;
                    Ok(Variant::Empty)
                }
                _ => Err(unsupported("FormatColor", m)),
            },
        }
    }

    fn spark_color_member(
        &mut self,
        b: BookId,
        s: SheetId,
        id: u32,
        kind: Option<usize>,
        m: &'static Member,
        a: &Args,
    ) -> Result<Variant, OmError> {
        let put = matches!(m.kind, InvokeKind::Put | InvokeKind::PutRef);
        match (m.name, put) {
            ("Color", false) => {
                let g = self.group_ref(b, s, id)?;
                Ok(Variant::I4(
                    kind.map_or(g.series_color, |k| g.points[k].color),
                ))
            }
            ("Color", true) => {
                let v = put_i(a)?;
                let g = self.group_mut(b, s, id)?;
                match kind {
                    Some(k) => g.points[k].color = v,
                    None => g.series_color = v,
                }
                Ok(Variant::Empty)
            }
            ("ThemeColor", false) => {
                self.group_ref(b, s, id)?;
                Ok(Variant::I4(0))
            }
            _ => Err(unsupported("FormatColor", m)),
        }
    }

    fn spark_group_member(
        &mut self,
        b: BookId,
        s: SheetId,
        id: u32,
        m: &'static Member,
        a: &Args,
    ) -> Result<Variant, OmError> {
        use InvokeKind::*;
        let put = matches!(m.kind, Put | PutRef);
        let g = self.group_ref(b, s, id)?.clone();
        match (m.name, put) {
            ("Count", false) => Ok(Variant::I4(g.lines.len() as i32)),
            ("Item" | "_Default", false) => {
                let i = a.i32(0).map_err(|_| fail())?.ok_or_else(fail)?;
                let i = usize::try_from(i - 1)
                    .ok()
                    .filter(|i| *i < g.lines.len())
                    .ok_or_else(fail)?;
                Ok(self.alloc(Object::Spark(b, s, SparkPart::Line(id, i))))
            }
            ("_NewEnum", false) => {
                let items = (0..g.lines.len())
                    .map(|i| self.alloc(Object::Spark(b, s, SparkPart::Line(id, i))))
                    .collect();
                Ok(self.alloc_enumerator(items))
            }
            ("Type", false) => Ok(Variant::I4(g.kind)),
            ("Type", true) => {
                let v = put_i(a)?;
                if !(1..=3).contains(&v) {
                    return Err(fail());
                }
                self.group_mut(b, s, id)?.kind = v;
                Ok(Variant::Empty)
            }
            ("SourceData", false) => Ok(Variant::Bstr(self.group_source(b, s, &g))),
            ("LineWeight", false) => Ok(Variant::R8(g.line_weight)),
            ("LineWeight", true) => {
                let v = crate::controls::put_num(a)?;
                if !(0.0..=1584.0).contains(&v) {
                    return Err(fail());
                }
                self.group_mut(b, s, id)?.line_weight = v;
                Ok(Variant::Empty)
            }
            ("DisplayBlanksAs", false) => Ok(Variant::I4(g.blanks)),
            ("DisplayBlanksAs", true) => {
                let v = put_i(a)?;
                if !(1..=3).contains(&v) {
                    return Err(fail());
                }
                self.group_mut(b, s, id)?.blanks = v;
                Ok(Variant::Empty)
            }
            ("DisplayHidden", false) => Ok(Variant::Bool(g.hidden)),
            ("DisplayHidden", true) => {
                let v = crate::chartparts::put_bool(a)?;
                self.group_mut(b, s, id)?.hidden = v;
                Ok(Variant::Empty)
            }
            ("PlotBy", false) => Ok(Variant::I4(g.plot_by)),
            ("PlotBy", true) => {
                let v = put_i(a)?;
                if !(0..=2).contains(&v) {
                    return Err(fail());
                }
                self.group_mut(b, s, id)?.plot_by = v;
                Ok(Variant::Empty)
            }
            ("DateRange", false) => Err(fail()),
            ("Location", false) => {
                let r = g.location().ok_or_else(OmError::object_required)?;
                if g.cells_fill() && g.sources_tile() {
                    return self.new_range(b, s, r, Mode::Cells);
                }
                let cells: Vec<Rect> = g.lines.iter().map(|l| Rect::cell(l.row, l.col)).collect();
                self.new_multi(b, s, &cells, Mode::Cells)
            }
            ("Points", false) => Ok(self.alloc(Object::Spark(b, s, SparkPart::Points(id)))),
            ("SeriesColor", false) => {
                Ok(self.alloc(Object::Spark(b, s, SparkPart::SeriesColor(id))))
            }
            ("Axes", false) => Ok(self.alloc(Object::Spark(b, s, SparkPart::Axes(id)))),
            ("Delete", false) => {
                self.book_mut(b)?.wb.spark_remove(s, |x| x.id == id);
                Ok(Variant::Empty)
            }
            ("Parent", false) => Ok(self.alloc(Object::Worksheet(b, s))),
            _ => Err(unsupported("SparklineGroup", m)),
        }
    }
}
