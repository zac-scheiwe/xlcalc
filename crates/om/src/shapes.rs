use xlcalc::drawing::{pt_to_emu, Chart, Shape, ShapeKind, ShapeProps, EMU_PT};
use xlcalc::{Rect, SheetId, Workbook};
use xlcalc_variant::Variant;

mod groups;

use crate::app::{unsupported, Application, BookId, Mode, Object};
use crate::controls::Coll;
use crate::conv::Args;
use crate::error::OmError;
use crate::typelib::{InvokeKind, Member};

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub(crate) enum DrawKind {
    Shapes,
    Shape,
    ShapeRange,
    TextFrame,
    TextFrame2,
    TextRange2,
    Fill,
    LineFmt,
    Chars,
    ChartObjects,
    ChartObject,
    Chart,
    SeriesColl,
    Series(usize),
    Sub(crate::chartparts::Sub),

    Stub(&'static str),

    Multi(usize),
}

#[derive(Clone, Copy, Debug)]
pub(crate) struct DrawRef {
    pub book: BookId,
    pub sheet: SheetId,
    pub id: u32,
    pub kind: DrawKind,
}

impl DrawKind {
    pub(crate) fn interface_name(self) -> &'static str {
        match self {
            DrawKind::Shapes => "Shapes",
            DrawKind::Shape => "Shape",
            DrawKind::ShapeRange | DrawKind::Multi(_) => "ShapeRange",
            DrawKind::TextFrame => "TextFrame",
            DrawKind::TextFrame2 => "TextFrame2",
            DrawKind::TextRange2 => "TextRange2",
            DrawKind::Fill => "FillFormat",
            DrawKind::LineFmt => "LineFormat",
            DrawKind::Chars => "Characters",
            DrawKind::ChartObjects => "ChartObjects",
            DrawKind::ChartObject => "ChartObject",
            DrawKind::Chart => "_Chart",
            DrawKind::SeriesColl => "SeriesCollection",
            DrawKind::Series(_) => "Series",
            DrawKind::Sub(k) => k.interface_name(),
            DrawKind::Stub(n) => n,
        }
    }
}

fn shape_type(k: &ShapeKind) -> i32 {
    match k {
        ShapeKind::Auto(_) => 1,
        ShapeKind::TextBox => 17,
        ShapeKind::Line => 9,
        ShapeKind::Picture => 13,
        ShapeKind::Chart => 3,
        ShapeKind::Slicer => 25,
        ShapeKind::Timeline => -2,
        ShapeKind::Group(_) => 6,
    }
}

fn auto_info(t: i32) -> Option<(i32, &'static str)> {
    crate::shapes_names::AUTO_SHAPES
        .iter()
        .find(|e| e.0 == t)
        .map(|e| (e.1, e.2))
}

fn base_name(k: &ShapeKind) -> &'static str {
    match k {
        ShapeKind::Auto(t) => crate::shapes_names::AUTO_SHAPES
            .iter()
            .find(|e| e.1 == *t)
            .map_or("AutoShape", |e| e.2),
        ShapeKind::TextBox => "TextBox",
        ShapeKind::Line => "Straight Connector",
        ShapeKind::Picture => "Picture",
        ShapeKind::Chart => "Chart",
        ShapeKind::Slicer => "Slicer",
        ShapeKind::Timeline => "Timeline",
        ShapeKind::Group(_) => "Group",
    }
}

pub(crate) const NO_NAME: &str = "The item with the specified name wasn't found.";

pub(crate) fn out_of_bounds() -> OmError {
    OmError::exception(
        0x8007_0057_u32 as i32,
        Some("The index into the specified collection is out of bounds."),
    )
}

fn resize_about_centre(
    (l, t, w0, h0): (i64, i64, i64, i64),
    w: i64,
    h: i64,
    chart_obj: bool,
    rotation: f64,
) -> (i64, i64, i64, i64) {
    if chart_obj {
        return (l, t, w, h);
    }
    if rotation != 0.0 {
        let (sin, cos) = rotation.to_radians().sin_cos();
        let turn = |x: f64, y: f64| (x * cos - y * sin, x * sin + y * cos);
        let (cx, cy) = (l as f64 + w0 as f64 / 2.0, t as f64 + h0 as f64 / 2.0);
        let (dx, dy) = turn(-(w0 as f64) / 2.0, -(h0 as f64) / 2.0);
        let (ex, ey) = turn(w as f64 / 2.0, h as f64 / 2.0);
        let nx = cx + dx + ex - w as f64 / 2.0;
        let ny = cy + dy + ey - h as f64 / 2.0;
        return (nx.round() as i64, ny.round() as i64, w, h);
    }
    let axis = |p: i64, old: i64, new: i64| p + old / 2 + (new - old) / 2 - new / 2;
    (axis(l, w0, w), axis(t, h0, h), w, h)
}

fn clamp_on_sheet(rc: (i64, i64, i64, i64), rotation: f64) -> (i64, i64, i64, i64) {
    let (x, y, w, h) = rc;
    let turned = (45.0..135.0).contains(&(rotation % 180.0));
    let (min_x, min_y) = if turned {
        (((h - w) / 2).max(0), ((w - h) / 2).max(0))
    } else {
        (0, 0)
    };
    (x.max(min_x), y.max(min_y), w, h)
}

const CONNECTION_SITES: [i32; 183] = [
    4, 6, 4, 4, 4, 8, 6, 6, 8, 6, 4, 6, 5, 6, 8, 4, 8, 8, 8, 3, 2, 7, 4, 4, 3, 4, 4, 4, 3, 3, 3, 3,
    4, 4, 4, 4, 8, 8, 4, 4, 4, 5, 8, 6, 5, 5, 5, 5, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 6,
    4, 4, 4, 4, 4, 4, 4, 4, 8, 4, 4, 4, 8, 8, 3, 4, 4, 4, 4, 4, 4, 5, 5, 4, 4, 4, 4, 5, 8, 16, 4,
    4, 4, 4, 4, 4, 4, 4, 4, 4, 5, 5, 9, 5, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4,
    4, 4, 4, 4, 4, 4, 4, 4, 4, 5, 0, 4, 6, 4, 4, 4, 10, 7, 12, 6, 7, 10, 12, 4, 4, 4, 4, 4, 4, 4,
    4, 4, 8, 3, 4, 4, 4, 4, 4, 6, 6, 12, 16, 12, 6, 9, 0, 2, 4, 6, 4, 4, 0, 0, 0, 2,
];

fn emu_pt(v: i64) -> Variant {
    Variant::R4((v as f64 / EMU_PT) as f32)
}

fn tri(b: bool) -> Variant {
    Variant::I4(if b { -1 } else { 0 })
}

fn put_f64(a: &Args) -> Result<f64, OmError> {
    crate::controls::put_num(a)
}

fn put_i32(a: &Args) -> Result<i32, OmError> {
    Ok(put_f64(a)? as i32)
}

fn out_of_range() -> OmError {
    OmError::exception(
        0x8007_0057_u32 as i32,
        Some("The specified value is out of range."),
    )
}

fn invalid_value() -> OmError {
    OmError::exception(
        0x8007_0057_u32 as i32,
        Some("The specified parameter has an invalid value."),
    )
}

fn is_auto_like(k: &ShapeKind) -> bool {
    matches!(k, ShapeKind::Auto(_) | ShapeKind::TextBox)
}

fn num(a: &Args, i: usize) -> Result<f64, OmError> {
    crate::controls::arg_num(a, i)
}

enum Item {
    Control(u32),
    Shape(u32),
}

impl Item {
    fn shape_id(&self) -> Option<u32> {
        match self {
            Item::Shape(id) => Some(*id),
            Item::Control(_) => None,
        }
    }
}

impl Application<'_> {
    fn draw_wb(&mut self, b: BookId) -> Result<&mut Workbook, OmError> {
        Ok(&mut self.book_mut(b)?.wb)
    }

    fn draw_obj(&mut self, b: BookId, s: SheetId, id: u32, kind: DrawKind) -> Variant {
        self.alloc(Object::Draw(DrawRef {
            book: b,
            sheet: s,
            id,
            kind,
        }))
    }

    pub(crate) fn chart_sheet_to_new_book(
        &mut self,
        b: BookId,
        s: SheetId,
    ) -> Result<Variant, OmError> {
        let book = self.book(b)?;
        let src_name = book.name.clone();
        let sheet_name = book
            .wb
            .sheet(s)
            .map(|x| x.name.to_string())
            .ok_or_else(OmError::object_required)?;
        let mut drawing = book.wb.drawing(s).cloned().unwrap_or_default();
        for sh in &mut drawing.shapes {
            if let Some(c) = &mut sh.chart {
                for p in c.parts_mut() {
                    if let Some(t) = crate::charts::external_text(&book.wb, p, &src_name) {
                        *p = xlcalc::drawing::Part::External(t);
                    }
                }
            }
        }
        let name = format!("Book{}", self.next_book_number());
        let mut wb = Workbook::empty();
        let ns = wb
            .add_chart_sheet_before(&sheet_name, None)
            .map_err(|_| OmError::app_defined())?;
        *wb.drawing_mut(ns) = drawing;
        let id = self.open_new(wb, name, String::new());
        self.book_mut(id)?.saved = true;
        Ok(Variant::Bool(true))
    }

    pub(crate) fn chart_sheet_ref(&mut self, b: BookId, s: SheetId) -> Result<DrawRef, OmError> {
        let wb = self.draw_wb(b)?;
        let found = wb
            .drawing(s)
            .and_then(|d| d.shapes.iter().find(|x| x.chart.is_some()).map(|x| x.id));
        let id = match found {
            Some(id) => id,
            None => {
                let id = wb.add_shape(s, ShapeKind::Chart, "Chart", [0.0, 0.0, 480.0, 288.0]);
                if let Some(sh) = wb.drawing_mut(s).get_mut(id) {
                    let mut c = Chart::new();
                    c.plot_by = 1;
                    sh.chart = Some(c);
                }
                id
            }
        };
        Ok(DrawRef {
            book: b,
            sheet: s,
            id,
            kind: DrawKind::Chart,
        })
    }

    pub(crate) fn chart_location(
        &mut self,
        r: &DrawRef,
        place: i32,
        name: Option<String>,
    ) -> Result<Variant, OmError> {
        let b = r.book;
        let on_sheet = self
            .book(b)?
            .wb
            .sheet(r.sheet)
            .is_some_and(|x| x.kind == xlcalc::SheetKind::Chart);
        let chart = self
            .shape_clone(r)?
            .chart
            .ok_or_else(OmError::object_required)?;
        match place {
            3 => Ok(self.chart_handle(r, on_sheet)),
            1 => {
                if on_sheet {
                    if let Some(n) = name {
                        self.book_mut(b)?
                            .wb
                            .rename_sheet(r.sheet, &n)
                            .map_err(|_| OmError::app_defined())?;
                    }
                    return Ok(self.chart_handle(r, true));
                }
                let book = self.book_mut(b)?;
                let name = match name {
                    Some(n) => n,
                    None => loop {
                        let n = format!("Chart{}", book.next_chart);
                        book.next_chart += 1;
                        if book.wb.sheet_by_name(&n).is_none() {
                            break n;
                        }
                    },
                };
                let new = book
                    .wb
                    .add_chart_sheet_before(&name, Some(r.sheet))
                    .map_err(|_| OmError::app_defined())?;
                book.wb.drawing_mut(r.sheet).shapes.retain(|x| x.id != r.id);
                let nr = self.chart_sheet_ref(b, new)?;
                if let Some(sh) = self.draw_wb(b)?.drawing_mut(new).get_mut(nr.id) {
                    sh.chart = Some(chart);
                }
                Ok(self.alloc(Object::Chart(b, new)))
            }
            2 => {
                let name = name.ok_or_else(OmError::app_defined)?;
                let wb = self.draw_wb(b)?;
                let host = wb
                    .sheet_by_name(&name)
                    .map(|x| x.id)
                    .ok_or_else(OmError::app_defined)?;
                let id = wb.add_shape(host, ShapeKind::Chart, "Chart", [0.0, 0.0, 360.0, 216.0]);
                if let Some(sh) = wb.drawing_mut(host).get_mut(id) {
                    sh.chart = Some(chart);
                }
                if on_sheet {
                    wb.delete_sheet(r.sheet)
                        .map_err(|_| OmError::app_defined())?;
                } else {
                    wb.drawing_mut(r.sheet).shapes.retain(|x| x.id != r.id);
                }
                Ok(self.draw_obj(b, host, id, DrawKind::Chart))
            }
            _ => Err(OmError::app_defined()),
        }
    }

    fn chart_handle(&mut self, r: &DrawRef, on_sheet: bool) -> Variant {
        if on_sheet {
            self.alloc(Object::Chart(r.book, r.sheet))
        } else {
            self.draw_obj(r.book, r.sheet, r.id, DrawKind::Chart)
        }
    }

    pub(crate) fn shape_clone(&self, r: &DrawRef) -> Result<Shape, OmError> {
        let bk = self.book(r.book)?;
        bk.wb
            .drawing(r.sheet)
            .and_then(|d| d.get(r.id))
            .cloned()
            .ok_or_else(OmError::object_required)
    }

    fn top_level(&self, b: BookId, s: SheetId) -> Result<Vec<Shape>, OmError> {
        let bk = self.book(b)?;
        Ok(bk
            .wb
            .drawing(s)
            .map(|d| {
                d.shapes
                    .iter()
                    .filter(|x| x.parent.is_none())
                    .cloned()
                    .collect()
            })
            .unwrap_or_default())
    }

    pub(crate) fn new_shape_range(&mut self, b: BookId, s: SheetId, ids: Vec<u32>) -> Variant {
        self.shape_ranges.push(ids);
        let i = self.shape_ranges.len() - 1;
        self.draw_obj(b, s, 0, DrawKind::Multi(i))
    }

    fn merged_items(&mut self, b: BookId, s: SheetId) -> Result<Vec<(Item, String)>, OmError> {
        let mut v: Vec<(Item, String)> = self
            .collection_items(b, s, Coll::Shapes)?
            .into_iter()
            .map(|(uid, _, name)| (Item::Control(uid), name))
            .collect();
        for sh in self.top_level(b, s)? {
            v.push((Item::Shape(sh.id), sh.name));
        }
        Ok(v)
    }

    fn item_handle(
        &mut self,
        b: BookId,
        s: SheetId,
        item: &Item,
        shape_iface: bool,
    ) -> Result<Variant, OmError> {
        match item {
            Item::Control(uid) => {
                let kind = self
                    .collection_items(b, s, Coll::Shapes)?
                    .into_iter()
                    .find(|c| c.0 == *uid)
                    .map(|c| c.1)
                    .ok_or_else(out_of_bounds)?;
                Ok(self.alloc(Object::Control(crate::controls::CtlRef {
                    book: b,
                    sheet: s,
                    uid: *uid,
                    kind,
                    shape: true,
                })))
            }
            Item::Shape(id) => {
                let k = if shape_iface {
                    DrawKind::Shape
                } else {
                    DrawKind::ChartObject
                };
                Ok(self.draw_obj(b, s, *id, k))
            }
        }
    }

    pub(crate) fn sheet_draw_member(
        &mut self,
        b: BookId,
        s: SheetId,
        m: &'static Member,
        a: &Args,
    ) -> Result<Variant, OmError> {
        let shapes = m.name == "Shapes";
        let kind = if shapes {
            DrawKind::Shapes
        } else {
            DrawKind::ChartObjects
        };
        match a.get(0) {
            Some(i) if m.kind == InvokeKind::Func => {
                let c = self.draw_obj(b, s, 0, kind);
                let Variant::Dispatch(Some(id)) = c else {
                    return Err(OmError::app_defined());
                };
                let Some(Object::Draw(r)) = self.arena.get(id).cloned() else {
                    return Err(OmError::app_defined());
                };
                self.collection_item(&r, i, a.lcid)
            }
            _ => Ok(self.draw_obj(b, s, 0, kind)),
        }
    }

    fn collection_item(
        &mut self,
        r: &DrawRef,
        index: &Variant,
        lcid: u32,
    ) -> Result<Variant, OmError> {
        let shapes = r.kind == DrawKind::Shapes;
        let items: Vec<(Item, String)> = if shapes {
            self.merged_items(r.book, r.sheet)?
        } else {
            self.top_level(r.book, r.sheet)?
                .into_iter()
                .filter(|x| x.kind == ShapeKind::Chart)
                .map(|x| (Item::Shape(x.id), x.name))
                .collect()
        };
        let found = match index {
            Variant::Bstr(name) => {
                let flat = self.book(r.book)?.wb.drawing(r.sheet).and_then(|d| {
                    d.shapes
                        .iter()
                        .find(|x| x.name.eq_ignore_ascii_case(name))
                        .map(|x| x.id)
                });
                let hit = match flat {
                    Some(id) if shapes => {
                        return self.item_handle(r.book, r.sheet, &Item::Shape(id), shapes)
                    }
                    _ => items.iter().find(|i| i.1.eq_ignore_ascii_case(name)),
                };
                if hit.is_none() {
                    return match flat {
                        Some(id) => self.item_handle(r.book, r.sheet, &Item::Shape(id), shapes),
                        None => Err(OmError::exception(0x8007_0057_u32 as i32, Some(NO_NAME))),
                    };
                }
                hit
            }
            v => {
                let n = crate::controls::num_arg(v, lcid)?.trunc();
                if n >= 1.0 {
                    items.get(n as usize - 1)
                } else {
                    None
                }
            }
        };
        let item = &found.ok_or_else(out_of_bounds)?.0;
        self.item_handle(r.book, r.sheet, item, shapes)
    }

    pub(crate) fn draw_collection_member(
        &mut self,
        r: &DrawRef,
        m: &'static Member,
        a: &Args,
    ) -> Result<Variant, OmError> {
        use InvokeKind::*;
        let (b, s) = (r.book, r.sheet);
        let shapes = r.kind == DrawKind::Shapes;
        if self.book(b)?.wb.sheet(s).is_none() {
            return Err(OmError::object_required());
        }
        match (m.name, m.kind) {
            ("Count", Get) => {
                let n = if shapes {
                    self.merged_items(b, s)?.len()
                } else {
                    self.top_level(b, s)?
                        .iter()
                        .filter(|x| x.kind == ShapeKind::Chart)
                        .count()
                };
                Ok(Variant::I4(n as i32))
            }
            ("Item" | "_Default", _) => match a.get(0) {
                Some(i) => self.collection_item(r, i, a.lcid),
                None => Err(OmError::app_defined()),
            },
            ("_NewEnum", _) => {
                let n = if shapes {
                    self.merged_items(b, s)?.len()
                } else {
                    self.top_level(b, s)?
                        .iter()
                        .filter(|x| x.kind == ShapeKind::Chart)
                        .count()
                };
                let mut handles = Vec::new();
                for i in 1..=n {
                    handles.push(self.collection_item(r, &Variant::I4(i as i32), a.lcid)?);
                }
                Ok(self.alloc_enumerator(handles))
            }
            ("SelectAll", _) if shapes => Ok(Variant::Empty),
            ("Range", _) if shapes => self.shapes_range(r, a),
            ("AddShape", _) if shapes => {
                let t = a.i32(0)?.ok_or_else(OmError::app_defined)?;
                let rect = [num(a, 1)?, num(a, 2)?, num(a, 3)?, num(a, 4)?];
                let (actual, name) = auto_info(t).ok_or_else(OmError::app_defined)?;
                if rect[2] < 0.0 || rect[3] < 0.0 {
                    return Err(OmError::app_defined());
                }
                let id = self
                    .draw_wb(b)?
                    .add_shape(s, ShapeKind::Auto(actual), name, rect);
                Ok(self.draw_obj(b, s, id, DrawKind::Shape))
            }
            ("AddTextbox", _) if shapes => {
                let rect = [num(a, 1)?, num(a, 2)?, num(a, 3)?, num(a, 4)?];
                let id = self
                    .draw_wb(b)?
                    .add_shape(s, ShapeKind::TextBox, "TextBox", rect);
                Ok(self.draw_obj(b, s, id, DrawKind::Shape))
            }
            ("AddLine", _) if shapes => {
                let (bx, by, ex, ey) = (num(a, 0)?, num(a, 1)?, num(a, 2)?, num(a, 3)?);
                let rect = [bx.min(ex), by.min(ey), (ex - bx).abs(), (ey - by).abs()];
                let wb = self.draw_wb(b)?;
                let id = wb.add_shape(s, ShapeKind::Line, "Straight Connector", rect);
                if let Some(sh) = wb.drawing_mut(s).get_mut(id) {
                    sh.flip_h = ex < bx;
                    sh.flip_v = ey < by;
                }
                Ok(self.draw_obj(b, s, id, DrawKind::Shape))
            }
            ("AddPicture", _) if shapes => {
                let Some(Variant::Bstr(file)) = a.get(0) else {
                    return Err(OmError::type_mismatch());
                };
                let truthy =
                    |i: usize| !matches!(a.get(i), Some(Variant::I4(0) | Variant::Bool(false)));
                let (link, save) = (truthy(1), truthy(2));
                let bytes = std::fs::read(file).map_err(|_| {
                    OmError::exception(
                        0x800A_03EC_u32 as i32,
                        Some("The specified file wasn't found."),
                    )
                })?;
                if !link && !save {
                    return Err(OmError::exception(
                        0x800A_03EC_u32 as i32,
                        Some("The specified value is out of range."),
                    ));
                }
                let (ext, nw, nh) = crate::picfile::probe(&bytes).ok_or_else(|| {
                    OmError::exception(0x800A_03EC_u32 as i32, Some("Unable to read the picture."))
                })?;
                let size = |i: usize, native: i64| -> Result<i64, OmError> {
                    let v = num(a, i)?;
                    Ok(if v < 0.0 {
                        native
                    } else {
                        pt_to_emu(v).max(1588)
                    })
                };
                let (w, h) = (size(5, nw)?, size(6, nh)?);
                let native = num(a, 5)? < 0.0 && num(a, 6)? < 0.0;
                let (x, y) = (pt_to_emu(num(a, 3)?), pt_to_emu(num(a, 4)?));
                let wb = self.draw_wb(b)?;

                wb.drawing_mut(s).next_id += 1;
                let id = wb.add_shape(s, ShapeKind::Picture, "Picture", [0.0; 4]);
                wb.set_shape_rect(s, id, (x, y, w, h));
                wb.set_shape_placement(s, id, 2);
                if let Some(sh) = wb.drawing_mut(s).get_mut(id) {
                    sh.image = Some(xlcalc::drawing::Image {
                        ext: ext.to_string(),
                        bytes,
                        linked: link,
                        source: file.to_string(),
                        embedded: save,
                    });
                    sh.lock_aspect = native;
                }
                Ok(self.draw_obj(b, s, id, DrawKind::Shape))
            }
            ("Add", _) if !shapes => {
                let rect = [num(a, 0)?, num(a, 1)?, num(a, 2)?, num(a, 3)?];
                let wb = self.draw_wb(b)?;
                let id = wb.add_shape(s, ShapeKind::Chart, "Chart", rect);
                if let Some(sh) = wb.drawing_mut(s).get_mut(id) {
                    sh.chart = Some(Chart::new());
                }
                Ok(self.draw_obj(b, s, id, DrawKind::ChartObject))
            }
            ("Delete", _) if !shapes => {
                let wb = self.draw_wb(b)?;
                wb.drawing_mut(s)
                    .shapes
                    .retain(|x| x.kind != ShapeKind::Chart);
                Ok(Variant::Empty)
            }
            _ if shapes => self.controls_member(b, s, Coll::Shapes, m, a),
            _ => Err(unsupported(r.kind.interface_name(), m)),
        }
    }

    fn shape_cell(&mut self, r: &DrawRef, br: bool) -> Result<Variant, OmError> {
        let sh = self.shape_clone(r)?;
        let wb = &self.book(r.book)?.wb;
        let (tl, bright) = wb.shape_cells(r.sheet, &sh.pos);
        let (row, col) = if br { bright } else { tl };
        self.new_range(r.book, r.sheet, Rect::cell(row, col), Mode::Cells)
    }

    pub(crate) fn shape_member(
        &mut self,
        r: &DrawRef,
        m: &'static Member,
        a: &Args,
    ) -> Result<Variant, OmError> {
        use InvokeKind::*;
        let (b, s, id) = (r.book, r.sheet, r.id);
        let chart_obj = r.kind == DrawKind::ChartObject;
        let sh = self.shape_clone(r)?;
        let rect = self.book(b)?.wb.shape_rect_emu(s, &sh.pos);
        let put = m.kind == Put || m.kind == PutRef;
        let is_group = matches!(sh.kind, ShapeKind::Group(_));
        let set_rect = |app: &mut Self, rc: (i64, i64, i64, i64)| -> Result<Variant, OmError> {
            let rc = clamp_on_sheet(rc, sh.rotation);
            let wb = app.draw_wb(b)?;
            if is_group {
                wb.set_group_rect(s, id, rc);
            } else {
                wb.set_shape_rect(s, id, rc);
                wb.refresh_groups(s);
            }
            Ok(Variant::Empty)
        };

        let set_pos = |app: &mut Self, rc: (i64, i64, i64, i64)| -> Result<Variant, OmError> {
            let held = clamp_on_sheet(rc, sh.rotation);
            if held.2 == 0 && held.3 == 0 && held.0 == 0 && held.1 == 0 {
                return Ok(Variant::Empty);
            }
            set_rect(app, rc)
        };
        match (m.name, put) {
            ("Name", false) => Ok(Variant::Bstr(sh.name)),
            ("Name", true) => {
                let name = match a.put.as_ref() {
                    Some(Variant::Bstr(n)) => n.clone(),
                    _ => return Err(OmError::type_mismatch()),
                };
                if let Some(x) = self.draw_wb(b)?.drawing_mut(s).get_mut(id) {
                    x.name = name;
                }
                Ok(Variant::Empty)
            }
            ("ID" | "Id", false) => Ok(Variant::I4(sh.id as i32)),
            ("Type", false) if !chart_obj => Ok(Variant::I4(match &sh.image {
                Some(i) if i.linked => 11,
                _ => shape_type(&sh.kind),
            })),
            ("Left" | "Top" | "Width" | "Height", false) if chart_obj => {
                let v = match m.name {
                    "Left" => rect.0,
                    "Top" => rect.1,
                    "Width" => rect.2,
                    _ => rect.3,
                };
                Ok(Variant::R8(v as f64 / EMU_PT))
            }
            ("Left", false) => Ok(emu_pt(rect.0)),
            ("Top", false) => Ok(emu_pt(rect.1)),
            ("Width", false) => Ok(emu_pt(rect.2)),
            ("Height", false) => Ok(emu_pt(rect.3)),
            ("Left", true) => set_pos(self, (pt_to_emu(put_f64(a)?), rect.1, rect.2, rect.3)),
            ("IncrementLeft", _) => {
                let to = rect.0 as f64 / EMU_PT + num(a, 0)?;
                set_pos(self, (pt_to_emu(to), rect.1, rect.2, rect.3))
            }
            ("IncrementTop", _) => {
                let to = rect.1 as f64 / EMU_PT + num(a, 0)?;
                set_pos(self, (rect.0, pt_to_emu(to), rect.2, rect.3))
            }
            ("Top", true) => set_pos(self, (rect.0, pt_to_emu(put_f64(a)?), rect.2, rect.3)),
            ("Width", true) => {
                let w = pt_to_emu(put_f64(a)?).max(0);
                let h = if !sh.lock_aspect {
                    rect.3
                } else if rect.2 > 0 {
                    (rect.3 as f64 * w as f64 / rect.2 as f64).round() as i64
                } else if rect.3 == 0 {
                    w
                } else {
                    rect.3
                };
                set_rect(
                    self,
                    resize_about_centre(rect, w, h, chart_obj, sh.rotation),
                )
            }
            ("Height", true) => {
                let h = pt_to_emu(put_f64(a)?).max(0);
                let w = if !sh.lock_aspect {
                    rect.2
                } else if rect.3 > 0 {
                    (rect.2 as f64 * h as f64 / rect.3 as f64).round() as i64
                } else if rect.2 == 0 {
                    h
                } else {
                    rect.2
                };
                set_rect(
                    self,
                    resize_about_centre(rect, w, h, chart_obj, sh.rotation),
                )
            }
            ("LockAspectRatio", false) => Ok(Variant::I4(if sh.lock_aspect { -1 } else { 0 })),
            ("LockAspectRatio", true) => {
                let v = !matches!(a.put.as_ref(), Some(Variant::I4(0) | Variant::Bool(false)));
                if let Some(x) = self.draw_wb(b)?.drawing_mut(s).get_mut(id) {
                    x.lock_aspect = v;
                }
                Ok(Variant::Empty)
            }
            ("Placement", false) => Ok(Variant::I4(sh.pos.placement())),
            ("Placement", true) => {
                let p = put_f64(a)? as i32;
                if !(1..=3).contains(&p) {
                    return Err(OmError::unable_to_set("Placement", r.kind.interface_name()));
                }
                let wb = self.draw_wb(b)?;
                let block = wb.drawing_mut(s).block(id);
                for i in block {
                    wb.set_shape_placement(s, i, p);
                }
                wb.refresh_groups(s);
                Ok(Variant::Empty)
            }
            ("ZOrderPosition", false) | ("ZOrder", false)
                if chart_obj || m.name == "ZOrderPosition" =>
            {
                let nctl = self.collection_items(b, s, Coll::Shapes)?.len();
                let pos = self
                    .book(b)?
                    .wb
                    .drawing(s)
                    .and_then(|d| d.index_of(id))
                    .unwrap_or(0);
                Ok(Variant::I4((nctl + pos + 1) as i32))
            }
            ("Visible", false) if chart_obj => Ok(Variant::Bool(sh.visible)),
            ("Visible", false) => Ok(tri(sh.visible)),
            ("Visible", true) => {
                let v = match a.put.as_ref() {
                    Some(Variant::Bool(x)) => *x,
                    Some(v) => crate::controls::num_arg(v, a.lcid)? != 0.0,
                    None => return Err(OmError::type_mismatch()),
                };
                let d = self.draw_wb(b)?.drawing_mut(s);
                for i in d.block(id) {
                    if let Some(x) = d.get_mut(i) {
                        x.visible = v;
                    }
                }
                Ok(Variant::Empty)
            }
            ("TopLeftCell", false) => self.shape_cell(r, false),
            ("BottomRightCell", false) => self.shape_cell(r, true),
            ("AutoShapeType", false) => Ok(Variant::I4(match sh.kind {
                ShapeKind::Auto(t) => t,
                ShapeKind::TextBox => sh.props.text_auto,
                _ => -2,
            })),
            ("AutoShapeType", true) => {
                let v = put_i32(a)?;
                if !(1..=183).contains(&v) || v == 138 || !is_auto_like(&sh.kind) {
                    return Err(out_of_range());
                }
                let old = base_name(&sh.kind);
                let new_kind = ShapeKind::Auto(v);
                let new_base = base_name(&new_kind);
                if let Some(x) = self.draw_wb(b)?.drawing_mut(s).get_mut(id) {
                    if matches!(sh.kind, ShapeKind::TextBox) {
                        x.props.text_auto = v;
                    } else {
                        if let Some(n) = x.name.strip_prefix(old) {
                            if n.starts_with(' ') && n[1..].bytes().all(|c| c.is_ascii_digit()) {
                                x.name = format!("{new_base}{n}");
                            }
                        }
                        x.kind = new_kind;
                    }
                }
                Ok(Variant::Empty)
            }
            ("BlackWhiteMode", false) => Ok(Variant::I4(sh.props.black_white)),
            ("BlackWhiteMode", true) => {
                let v = put_i32(a)?;
                if !(1..=10).contains(&v) {
                    return Err(out_of_range());
                }
                self.set_props(b, s, id, |p| p.black_white = v)
            }
            ("ShapeStyle", false) => {
                Ok(Variant::I4(sh.props.shape_style.unwrap_or(match sh.kind {
                    ShapeKind::Auto(_) => 9,
                    ShapeKind::Line => 10009,
                    _ => 0,
                })))
            }
            ("ShapeStyle", true) => {
                let v = put_i32(a)?;
                if !(1..=77).contains(&v) && !(10001..=10042).contains(&v) {
                    return Err(invalid_value());
                }
                self.set_props(b, s, id, |p| {
                    p.shape_style = Some(v);
                    p.background_style = 0;
                })
            }
            ("BackgroundStyle", false) => Ok(Variant::I4(sh.props.background_style)),
            ("BackgroundStyle", true) => {
                let v = put_i32(a)?;
                if !(1..=12).contains(&v) {
                    return Err(invalid_value());
                }
                if !is_auto_like(&sh.kind) {
                    return Err(out_of_range());
                }

                let styled = sh.props.background_style != 0
                    || sh.props.shape_style.unwrap_or(match sh.kind {
                        ShapeKind::Auto(_) => 9,
                        _ => 0,
                    }) != 0;
                if !styled {
                    return Ok(Variant::Empty);
                }
                self.set_props(b, s, id, |p| {
                    p.background_style = v;
                    p.shape_style = Some(0);
                })
            }
            ("GraphicStyle", false) => Ok(Variant::I4(0)),
            ("GraphicStyle", true) => {
                let v = put_i32(a)?;
                Err(if (1..=28).contains(&v) {
                    out_of_range()
                } else {
                    invalid_value()
                })
            }
            ("Decorative", false) => Ok(Variant::I4(if sh.props.decorative { -1 } else { 0 })),
            ("Decorative", true) => {
                let v = put_i32(a)? != 0;
                if v {
                    if let Some(x) = self.draw_wb(b)?.drawing_mut(s).get_mut(id) {
                        x.alt_text.clear();
                    }
                }
                self.set_props(b, s, id, |p| {
                    p.decorative = v;
                    if v {
                        p.title.clear();
                    }
                })
            }
            ("OnAction", false) => Ok(Variant::Bstr(sh.props.on_action)),
            ("OnAction", true) => {
                let t = crate::controls::put_text(a)?;
                if t.encode_utf16().count() > 255 {
                    return Err(OmError::exception(0x800A_03EC_u32 as i32, None));
                }
                self.set_props(b, s, id, |p| p.on_action = t)
            }
            ("Title", false) => Ok(Variant::Bstr(sh.props.title)),
            ("Title", true) => {
                let t = crate::controls::put_text(a)?;
                self.set_props(b, s, id, |p| p.title = t)
            }
            ("Connector", false) => Ok(tri(sh.kind == ShapeKind::Line)),
            ("HorizontalFlip", false) => Ok(tri(sh.flip_h)),
            ("VerticalFlip", false) => Ok(tri(sh.flip_v)),
            ("HasChart", false) => Ok(tri(sh.chart.is_some())),
            ("Chart", false) if sh.chart.is_some() => Ok(self.draw_obj(b, s, id, DrawKind::Chart)),
            ("TextFrame", false) => Ok(self.draw_obj(b, s, id, DrawKind::TextFrame)),
            ("TextFrame2", false) => Ok(self.draw_obj(b, s, id, DrawKind::TextFrame2)),
            ("Fill", false) => Ok(self.draw_obj(b, s, id, DrawKind::Fill)),
            ("Line", false) => Ok(self.draw_obj(b, s, id, DrawKind::LineFmt)),
            ("Parent", false) => Ok(self.alloc(Object::Worksheet(b, s))),
            (
                "PictureFormat" | "Shadow" | "ThreeD" | "SoftEdge" | "Glow" | "Reflection"
                | "Model3D" | "TextEffect" | "Callout" | "ConnectorFormat" | "Nodes"
                | "ControlFormat" | "LinkFormat" | "OLEFormat" | "Adjustments",
                false,
            ) if !chart_obj => {
                let iface = match m.name {
                    "Shadow" => "ShadowFormat",
                    "ThreeD" => "ThreeDFormat",
                    "SoftEdge" => "SoftEdgeFormat",
                    "Glow" => "GlowFormat",
                    "Reflection" => "ReflectionFormat",
                    "Model3D" => "Model3DFormat",
                    "TextEffect" => "TextEffectFormat",
                    "Callout" => "CalloutFormat",
                    "Nodes" => "ShapeNodes",
                    other => other,
                };
                Ok(self.draw_obj(b, s, id, DrawKind::Stub(iface)))
            }
            ("Child" | "HasSmartArt", false) => Ok(tri(false)),
            ("SmartArt", false) => Err(OmError::exception(
                0x8007_0057_u32 as i32,
                Some("The shape is not a SmartArt object."),
            )),
            ("Hyperlink" | "FormControlType", false) => {
                Err(OmError::exception(0x800A_03EC_u32 as i32, None))
            }
            ("Vertices", false) => match sh.kind {
                ShapeKind::Line => Err(OmError::exception(0x8000_4005_u32 as i32, None)),
                _ => Ok(Variant::Empty),
            },
            ("ConnectionSiteCount", false) => Ok(Variant::I4(match sh.kind {
                ShapeKind::Auto(t) => CONNECTION_SITES.get(t as usize - 1).map_or(4, |n| *n),
                ShapeKind::Line => 2,
                ShapeKind::TextBox => 4,
                _ => 0,
            })),
            ("Chart", false) => Err(OmError::exception(
                0x8007_0057_u32 as i32,
                Some("This member can only be accessed for a Chart object."),
            )),
            ("Delete", _) => {
                self.draw_wb(b)?.delete_shape(s, id);
                Ok(Variant::Empty)
            }
            ("Rotation", false) => Ok(Variant::R4(sh.rotation as f32)),
            ("Rotation", true) => {
                let v = put_f64(a)?;
                if let Some(x) = self.draw_wb(b)?.drawing_mut(s).get_mut(id) {
                    x.rotation = v.rem_euclid(360.0);
                }
                Ok(Variant::Empty)
            }
            ("Locked", false) => Ok(Variant::Bool(sh.locked)),
            ("Locked", true) => {
                let v = matches!(a.put.as_ref(), Some(Variant::Bool(true)));
                if let Some(x) = self.draw_wb(b)?.drawing_mut(s).get_mut(id) {
                    x.locked = v;
                }
                Ok(Variant::Empty)
            }
            ("AlternativeText", false) => Ok(Variant::Bstr(sh.alt_text)),
            ("AlternativeText", true) => {
                let t = match a.put.as_ref() {
                    Some(Variant::Bstr(t)) => t.clone(),
                    _ => return Err(OmError::type_mismatch()),
                };
                if let Some(x) = self.draw_wb(b)?.drawing_mut(s).get_mut(id) {
                    x.alt_text = t;
                }
                Ok(Variant::Empty)
            }
            ("Duplicate", _) => {
                let wb = self.draw_wb(b)?;
                let nid = if is_group {
                    wb.duplicate_group(s, id, base_name)
                } else {
                    wb.duplicate_shape(s, id, base_name(&sh.kind))
                }
                .ok_or_else(OmError::object_required)?;
                Ok(self.draw_obj(b, s, nid, r.kind))
            }
            ("ZOrder", _) => {
                let cmd = a.i32(0)?.ok_or_else(OmError::app_defined)?;
                if !(0..=3).contains(&cmd) {
                    return Err(OmError::exception(
                        0x8007_0057_u32 as i32,
                        Some("The specified value is out of range."),
                    ));
                }
                self.draw_wb(b)?.z_order(s, id, cmd);
                Ok(Variant::Empty)
            }
            ("GroupItems", false) => {
                if !is_group {
                    return Err(OmError::exception(
                        0x800A_03EC_u32 as i32,
                        Some("This member can only be accessed for a group."),
                    ));
                }
                let wb = &mut self.book_mut(b)?.wb;
                wb.drawing_mut(s).list_group(id);
                let leaves = wb
                    .drawing(s)
                    .map(|d| d.listed_leaves(id))
                    .unwrap_or_default();
                let got = self.new_shape_range(b, s, leaves);
                if let Variant::Dispatch(Some(rid)) = &got {
                    self.pins.insert(*rid, (b, s, id));
                    self.group_ranges.insert(self.shape_ranges.len() - 1, id);
                }
                Ok(got)
            }
            ("ParentGroup", false) => {
                let top = self.book(b)?.wb.drawing(s).and_then(|d| d.top_group(id));
                match top {
                    Some(g) => Ok(self.draw_obj(b, s, g, DrawKind::Shape)),
                    None => Err(OmError::exception(
                        0x800A_03EC_u32 as i32,
                        Some("The specified value is out of range."),
                    )),
                }
            }
            ("Ungroup", _) => {
                if !is_group {
                    return Err(OmError::exception(
                        0x8007_0005_u32 as i32,
                        Some("This member can only be accessed for a group."),
                    ));
                }
                if sh.parent.is_some() {
                    return Err(OmError::exception(
                        0x8007_0005_u32 as i32,
                        Some("This member cannot be accessed on an object in a group."),
                    ));
                }
                let kids = self
                    .draw_wb(b)?
                    .ungroup_shape(s, id)
                    .ok_or_else(OmError::object_required)?;
                Ok(self.new_shape_range(b, s, kids))
            }
            ("Select", _) => Ok(Variant::Empty),
            ("Activate", _) => Ok(Variant::Bool(true)),
            _ => Err(unsupported(r.kind.interface_name(), m)),
        }
    }

    fn set_props(
        &mut self,
        b: BookId,
        s: SheetId,
        id: u32,
        f: impl FnOnce(&mut ShapeProps),
    ) -> Result<Variant, OmError> {
        if let Some(x) = self.draw_wb(b)?.drawing_mut(s).get_mut(id) {
            f(&mut x.props);
        }
        Ok(Variant::Empty)
    }

    pub(crate) fn text_member(
        &mut self,
        r: &DrawRef,
        m: &'static Member,
        a: &Args,
    ) -> Result<Variant, OmError> {
        use InvokeKind::*;
        let put = m.kind == Put || m.kind == PutRef;
        match (r.kind, m.name, put) {
            (DrawKind::TextFrame, "Characters", _) => {
                Ok(self.draw_obj(r.book, r.sheet, r.id, DrawKind::Chars))
            }
            (DrawKind::TextFrame2, "TextRange", false) => {
                Ok(self.draw_obj(r.book, r.sheet, r.id, DrawKind::TextRange2))
            }
            (DrawKind::TextRange2, "Characters", false) => {
                Ok(self.draw_obj(r.book, r.sheet, r.id, DrawKind::TextRange2))
            }
            (DrawKind::TextRange2, "Count" | "Length", false) => {
                let n = self.shape_clone(r)?.text.encode_utf16().count();
                Ok(Variant::I4(i32::try_from(n).unwrap_or(i32::MAX)))
            }
            (DrawKind::Fill | DrawKind::LineFmt, "Visible", false) => Ok(Variant::I4(-1)),
            (DrawKind::Chars | DrawKind::TextRange2, "Text" | "_Default", false) => {
                Ok(Variant::Bstr(self.shape_clone(r)?.text))
            }
            (DrawKind::Chars | DrawKind::TextRange2, "Text", true) => {
                let t = match a.put.as_ref() {
                    Some(Variant::Bstr(t)) => t.clone(),
                    Some(v) => {
                        match xlcalc_variant::change_type(
                            v,
                            xlcalc_variant::VarType::Bstr,
                            crate::conv::variant_locale(a.lcid),
                            0,
                        ) {
                            Ok(Variant::Bstr(t)) => t,
                            _ => return Err(OmError::type_mismatch()),
                        }
                    }
                    None => return Err(OmError::type_mismatch()),
                };
                self.shape_clone(r)?;
                if let Some(x) = self.draw_wb(r.book)?.drawing_mut(r.sheet).get_mut(r.id) {
                    x.text = t;
                }
                Ok(Variant::Empty)
            }
            _ => Err(unsupported(r.kind.interface_name(), m)),
        }
    }

    pub(crate) fn draw_member(
        &mut self,
        r: &DrawRef,
        m: &'static Member,
        a: &Args,
    ) -> Result<Variant, OmError> {
        match r.kind {
            DrawKind::Shapes | DrawKind::ChartObjects => self.draw_collection_member(r, m, a),
            DrawKind::Shape | DrawKind::ChartObject | DrawKind::ShapeRange => {
                if r.kind == DrawKind::ChartObject && m.name == "ShapeRange" {
                    return Ok(self.draw_obj(r.book, r.sheet, r.id, DrawKind::ShapeRange));
                }
                self.shape_member(r, m, a)
            }
            DrawKind::Multi(i) => {
                let got = self.shape_range_member(r, i, m, a)?;
                if let (Some(g), Variant::Dispatch(Some(id))) = (self.group_ranges.get(&i), &got) {
                    self.pins.insert(*id, (r.book, r.sheet, *g));
                }
                Ok(got)
            }
            DrawKind::TextFrame
            | DrawKind::Chars
            | DrawKind::TextFrame2
            | DrawKind::TextRange2
            | DrawKind::Fill
            | DrawKind::LineFmt => self.text_member(r, m, a),
            DrawKind::Sub(k) => self.sub_member(r, k, m, a),
            DrawKind::Stub(_) => Err(unsupported(r.kind.interface_name(), m)),
            DrawKind::Chart | DrawKind::SeriesColl | DrawKind::Series(_) => {
                self.chart_draw_member(r, m, a)
            }
        }
    }
}
