use xlcalc::slicer::SlicerError;
use xlcalc::SheetId;
use xlcalc_variant::Variant;

use crate::app::{unsupported, Application, BookId, Object};
use crate::conv::Args;
use crate::error::OmError;
use crate::objects::edit_error;
use crate::shapes::{DrawKind, DrawRef};
use crate::typelib::{InvokeKind, Member};

#[derive(Clone, Debug)]
pub(crate) enum SlicerPart {
    Caches,
    Cache(usize),

    Items(usize, bool),
    Item(usize, String),
    Slicers(usize),
    Slicer(usize, usize),

    Style(usize, usize),

    Timeline(usize),

    TimelineView(usize, usize),

    PivotTables(usize),
}

impl SlicerPart {
    pub(crate) fn interface_name(&self) -> &'static str {
        match self {
            SlicerPart::Caches => "SlicerCaches",
            SlicerPart::Cache(_) => "SlicerCache",
            SlicerPart::Items(..) => "SlicerItems",
            SlicerPart::Item(..) => "SlicerItem",
            SlicerPart::Slicers(_) => "Slicers",
            SlicerPart::Slicer(..) => "Slicer",
            SlicerPart::Style(..) => "TableStyle",
            SlicerPart::Timeline(_) => "TimelineState",
            SlicerPart::TimelineView(..) => "TimelineViewState",
            SlicerPart::PivotTables(_) => "SlicerPivotTables",
        }
    }
}

fn invalid() -> OmError {
    OmError::exception(0x8007_0057_u32 as i32, None)
}

fn from_err(e: SlicerError) -> OmError {
    match e {
        SlicerError::Invalid => invalid(),
        SlicerError::BadIndex => OmError::bad_index(),
        SlicerError::AppDefined => OmError::app_defined(),
        SlicerError::NotFound => OmError::exception(0x8002_802B_u32 as i32, None),
    }
}

const DEFAULT_RECT: [f64; 4] = [642.5, 240.0, 144.0, 206.249_763_779_527_57];

const TIMELINE_RECT: [f64; 4] = [583.5, 290.0, 262.5, 108.0];

fn not_impl() -> OmError {
    OmError::exception(0x8000_4001_u32 as i32, None)
}

impl Application<'_> {
    fn slicer_pick(
        &self,
        v: Option<&Variant>,
        lcid: u32,
        names: &[String],
    ) -> Result<usize, OmError> {
        match v {
            Some(Variant::Bstr(n)) => names
                .iter()
                .position(|x| x.to_lowercase() == n.to_lowercase())
                .ok_or_else(invalid),
            Some(v) => {
                let a = Args {
                    values: vec![Some(v.clone())],
                    put: None,
                    lcid,
                };
                let n = a.i32(0)?.unwrap_or(0);
                usize::try_from(n - 1)
                    .ok()
                    .filter(|&i| i < names.len())
                    .ok_or_else(OmError::bad_index)
            }
            None => Err(OmError::bad_index()),
        }
    }

    fn slc_of(&self, b: BookId, i: usize) -> Result<xlcalc::slicer::SlicerCache, OmError> {
        self.book(b)?
            .wb
            .slicer_caches()
            .get(i)
            .cloned()
            .ok_or_else(OmError::object_required)
    }

    fn slicer_geometry(&self, b: BookId, i: usize, j: usize) -> Result<[f64; 4], OmError> {
        let wb = &self.book(b)?.wb;
        let s = wb
            .slicer_caches()
            .get(i)
            .and_then(|c| c.slicers.get(j))
            .ok_or_else(OmError::object_required)?;
        let sh = wb
            .drawing(s.sheet)
            .and_then(|d| d.get(s.shape))
            .ok_or_else(OmError::object_required)?;
        let r = wb.shape_rect_emu(s.sheet, &sh.pos);
        let pt = |v: i64| v as f64 / 12700.0;
        Ok([pt(r.0), pt(r.1), pt(r.2), pt(r.3)])
    }

    fn slicer_set_geometry(
        &mut self,
        b: BookId,
        i: usize,
        j: usize,
        k: usize,
        v: f64,
    ) -> Result<(), OmError> {
        let mut g = self.slicer_geometry(b, i, j)?;
        g[k] = v;
        let wb = &mut self.book_mut(b)?.wb;
        let s = wb.slicer_caches()[i].slicers[j].clone();
        let e = |x: f64| xlcalc::drawing::pt_to_emu(x);
        wb.set_shape_rect(s.sheet, s.shape, (e(g[0]), e(g[1]), e(g[2]), e(g[3])));
        Ok(())
    }

    pub(crate) fn slicer_member(
        &mut self,
        b: BookId,
        part: &SlicerPart,
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
            SlicerPart::Caches => self.slc_caches_member(b, m, a),
            SlicerPart::Cache(i) => self.slc_cache_member(b, *i, m, a),
            SlicerPart::Items(i, _) if self.slc_of(b, *i)?.timeline.is_some() => match m.name {
                "Item" | "_Default" => Err(OmError::hresult(crate::error::DISP_E_MEMBERNOTFOUND)),
                "Parent" => Ok(self.alloc(Object::Slicer(b, SlicerPart::Cache(*i)))),
                _ => Err(not_impl()),
            },
            SlicerPart::Items(i, visible) => {
                self.slc_of(b, *i)?;
                let off = self.book(b)?.wb.slicer_off(*i);
                let mut names = self.book(b)?.wb.slicer_items(*i);
                if *visible {
                    names.retain(|n| !off.iter().any(|o| o.to_lowercase() == n.to_lowercase()));
                }
                match m.name {
                    "Count" => Ok(Variant::I4(names.len() as i32)),
                    "Item" | "_Default" => {
                        let k = self.slicer_pick(a.get(0), a.lcid, &names)?;
                        Ok(self.alloc(Object::Slicer(b, SlicerPart::Item(*i, names[k].clone()))))
                    }
                    "_NewEnum" => {
                        let items = names
                            .iter()
                            .map(|n| self.alloc(Object::Slicer(b, SlicerPart::Item(*i, n.clone()))))
                            .collect();
                        Ok(self.alloc_enumerator(items))
                    }
                    "Parent" => Ok(self.alloc(Object::Slicer(b, SlicerPart::Cache(*i)))),
                    _ => Err(unsupported("SlicerItems", m)),
                }
            }
            SlicerPart::Item(i, name) => {
                self.slc_of(b, *i)?;
                let off = self.book(b)?.wb.slicer_off(*i);
                let on = !off.iter().any(|o| o.to_lowercase() == name.to_lowercase());
                match (m.name, put) {
                    ("Name" | "Caption" | "Value" | "SourceNameStandard", false) => {
                        Ok(Variant::Bstr(name.clone()))
                    }
                    ("SourceName", false) => Ok(Variant::Bstr(name.clone())),
                    ("HasData", false) => Ok(Variant::Bool(true)),
                    ("Selected", false) => Ok(Variant::Bool(on)),
                    ("Selected", true) => {
                        let v = crate::chartparts::put_bool(a)?;
                        self.book_mut(b)?
                            .wb
                            .slicer_select(*i, name, v)
                            .map_err(edit_error)?;
                        Ok(Variant::Empty)
                    }
                    ("Parent", false) => Ok(self.alloc(Object::Slicer(b, SlicerPart::Cache(*i)))),
                    _ => Err(unsupported("SlicerItem", m)),
                }
            }
            SlicerPart::Slicers(i) => {
                let c = self.slc_of(b, *i)?;
                match m.name {
                    "Count" => Ok(Variant::I4(c.slicers.len() as i32)),
                    "Item" | "_Default" => {
                        let names: Vec<String> = c.slicers.iter().map(|s| s.name.clone()).collect();
                        let k = self.slicer_pick(a.get(0), a.lcid, &names)?;
                        Ok(self.alloc(Object::Slicer(b, SlicerPart::Slicer(*i, k))))
                    }
                    "_NewEnum" => {
                        let items = (0..c.slicers.len())
                            .map(|k| self.alloc(Object::Slicer(b, SlicerPart::Slicer(*i, k))))
                            .collect();
                        Ok(self.alloc_enumerator(items))
                    }
                    "Add" => self.slicers_add(b, *i, a),
                    "Parent" => Ok(self.alloc(Object::Slicer(b, SlicerPart::Cache(*i)))),
                    _ => Err(unsupported("Slicers", m)),
                }
            }
            SlicerPart::Slicer(i, j) => self.one_slicer_member(b, *i, *j, m, a),
            SlicerPart::Timeline(i) => self.timeline_member(b, *i, m, a),
            SlicerPart::TimelineView(i, j) => self.timeline_view_member(b, *i, *j, m, a),
            SlicerPart::PivotTables(i) => self.slicer_pivots_member(b, *i, m, a),
            SlicerPart::Style(i, j) => {
                let c = self.slc_of(b, *i)?;
                let s = c.slicers.get(*j).ok_or_else(OmError::object_required)?;
                match (m.name, put) {
                    ("Name" | "NameLocal", false) => Ok(Variant::Bstr(s.style.clone())),
                    _ => Err(unsupported("TableStyle", m)),
                }
            }
        }
    }

    fn slc_caches_member(
        &mut self,
        b: BookId,
        m: &'static Member,
        a: &Args,
    ) -> Result<Variant, OmError> {
        let names: Vec<String> = self
            .book(b)?
            .wb
            .slicer_caches()
            .iter()
            .map(|c| c.name.clone())
            .collect();
        match m.name {
            "Count" => Ok(Variant::I4(names.len() as i32)),
            "Item" | "_Default" => {
                let k = self.slicer_pick(a.get(0), a.lcid, &names)?;
                Ok(self.alloc(Object::Slicer(b, SlicerPart::Cache(k))))
            }
            "_NewEnum" => {
                let items = (0..names.len())
                    .map(|k| self.alloc(Object::Slicer(b, SlicerPart::Cache(k))))
                    .collect();
                Ok(self.alloc_enumerator(items))
            }
            "Add" | "Add2" => {
                let (table, pivot) = match a.get(0) {
                    Some(Variant::Dispatch(Some(id))) => match self.arena.get(*id) {
                        Some(Object::Table(_, name)) => (name.clone(), false),
                        Some(Object::Pivot(crate::pivot::PivotRef::Table(_, name))) => {
                            (name.clone(), true)
                        }
                        _ => return Err(invalid()),
                    },
                    _ => return Err(invalid()),
                };
                let field = match a.get(1) {
                    Some(Variant::Bstr(f)) => f.clone(),
                    _ => return Err(invalid()),
                };
                let timeline = match a.get(3) {
                    Some(v) => crate::controls::num_arg(v, a.lcid)? as i32 == 2,
                    None => false,
                };
                let wb = &mut self.book_mut(b)?.wb;
                let i = if timeline && pivot {
                    wb.timeline_cache_add(&table, &field).map_err(|e| match e {
                        SlicerError::AppDefined => {
                            OmError::app_defined_with("This slicer cache already exists")
                        }
                        e => from_err(e),
                    })?
                } else {
                    wb.slicer_cache_add(&table, &field, pivot)
                        .map_err(from_err)?
                };
                Ok(self.alloc(Object::Slicer(b, SlicerPart::Cache(i))))
            }
            "Parent" => Ok(self.alloc(Object::Workbook(b))),
            _ => Err(unsupported("SlicerCaches", m)),
        }
    }

    fn slc_cache_member(
        &mut self,
        b: BookId,
        i: usize,
        m: &'static Member,
        a: &Args,
    ) -> Result<Variant, OmError> {
        use InvokeKind::*;
        let put = matches!(m.kind, Put | PutRef);
        let c = self.slc_of(b, i)?;
        match (m.name, put) {
            ("Name", false) => Ok(Variant::Bstr(c.name)),
            ("Name", true) => {
                let n = match a.put.as_ref() {
                    Some(Variant::Bstr(n)) => n.clone(),
                    _ => return Err(OmError::type_mismatch()),
                };
                let ok = !n.is_empty()
                    && !n.starts_with(|ch: char| ch.is_ascii_digit())
                    && n.chars()
                        .all(|ch| ch.is_alphanumeric() || ch == '_' || ch == '.');
                let taken = self
                    .book(b)?
                    .wb
                    .slicer_caches()
                    .iter()
                    .enumerate()
                    .any(|(k, x)| k != i && x.name.to_lowercase() == n.to_lowercase());
                if !ok || taken {
                    return Err(invalid());
                }
                if let Some(x) = self.book_mut(b)?.wb.slicer_cache_mut(i) {
                    x.name = n;
                }
                Ok(Variant::Empty)
            }
            ("SourceName", false) => Ok(Variant::Bstr(c.field)),
            ("Index", false) => Ok(Variant::I4(i as i32 + 1)),
            ("List", false) => Ok(Variant::Bool(!c.pivot)),
            ("OLAP", false) => Ok(Variant::Bool(false)),
            ("SourceType", false) => Ok(Variant::I4(1)),
            ("SlicerCacheType", false) if c.timeline.is_some() => Ok(Variant::I4(2)),
            ("SlicerCacheType", false) => Ok(Variant::I4(i32::from(c.pivot))),
            ("ShowAllItems" | "SortItems", _) if c.timeline.is_some() => Err(not_impl()),
            ("ShowAllItems", false) => Ok(Variant::Bool(true)),
            ("FilterCleared", false) if c.timeline.is_some() => Ok(Variant::Bool(
                c.timeline.as_ref().is_some_and(|t| t.range.is_none()),
            )),
            ("FilterCleared", false) => {
                Ok(Variant::Bool(self.book(b)?.wb.slicer_off(i).is_empty()))
            }
            ("TimelineState", false) if c.timeline.is_some() => {
                Ok(self.alloc(Object::Slicer(b, SlicerPart::Timeline(i))))
            }
            ("TimelineState", false) => Ok(Variant::Dispatch(None)),
            ("PivotTables", false) if c.pivot => {
                Ok(self.alloc(Object::Slicer(b, SlicerPart::PivotTables(i))))
            }
            ("ClearDateFilter", _) if c.timeline.is_some() => {
                self.book_mut(b)?.wb.timeline_clear(i);
                Ok(Variant::Empty)
            }
            ("ClearManualFilter", _) if c.timeline.is_some() => Ok(Variant::Empty),
            ("ClearAllFilters", _) if c.timeline.is_some() => {
                self.book_mut(b)?.wb.timeline_clear(i);
                Ok(Variant::Empty)
            }
            ("SortUsingCustomLists", false) => Ok(Variant::Bool(c.custom_lists)),
            ("SortItems", false) => Ok(Variant::I4(c.sort)),
            ("SortItems", true) => {
                let v = crate::controls::put_num(a)? as i32;
                if !(2..=3).contains(&v) {
                    return Err(invalid());
                }
                if let Some(x) = self.book_mut(b)?.wb.slicer_cache_mut(i) {
                    x.sort = v;
                }
                Ok(Variant::Empty)
            }
            ("CrossFilterType", false) => Ok(Variant::I4(c.cross)),
            ("CrossFilterType", true) => {
                let v = crate::controls::put_num(a)? as i32;
                if !(1..=3).contains(&v) {
                    return Err(invalid());
                }
                if let Some(x) = self.book_mut(b)?.wb.slicer_cache_mut(i) {
                    x.cross = v;
                }
                Ok(Variant::Empty)
            }
            ("RequireManualUpdate", false) => Ok(Variant::Bool(c.manual)),
            ("RequireManualUpdate", true) => {
                let v = crate::chartparts::put_bool(a)?;
                if let Some(x) = self.book_mut(b)?.wb.slicer_cache_mut(i) {
                    x.manual = v;
                }
                Ok(Variant::Empty)
            }
            ("ListObject", false) if c.pivot => Err(OmError::app_defined()),
            ("ListObject", false) => Ok(self.alloc(Object::Table(b, c.table))),
            ("SlicerItems", false) => {
                Ok(self.alloc(Object::Slicer(b, SlicerPart::Items(i, false))))
            }
            ("VisibleSlicerItems", false) => {
                Ok(self.alloc(Object::Slicer(b, SlicerPart::Items(i, true))))
            }
            ("Slicers", false) => Ok(self.alloc(Object::Slicer(b, SlicerPart::Slicers(i)))),
            ("ClearManualFilter" | "ClearAllFilters", _) => {
                self.book_mut(b)?.wb.slicer_clear(i).map_err(edit_error)?;
                Ok(Variant::Empty)
            }
            ("Delete", _) => {
                self.book_mut(b)?.wb.slicer_cache_delete(i);
                Ok(Variant::Empty)
            }
            ("Parent", false) => Ok(self.alloc(Object::Workbook(b))),
            _ => Err(unsupported("SlicerCache", m)),
        }
    }

    fn slicers_add(&mut self, b: BookId, i: usize, a: &Args) -> Result<Variant, OmError> {
        let sheet: SheetId = match a.get(0) {
            Some(Variant::Dispatch(Some(id))) => match self.arena.get(*id) {
                Some(Object::Worksheet(_, s)) => *s,
                _ => return Err(invalid()),
            },
            _ => return Err(invalid()),
        };
        let text = |k: usize| -> Result<Option<String>, OmError> {
            match a.get(k) {
                None => Ok(None),
                Some(Variant::Bstr(s)) => Ok(Some(s.clone())),
                Some(_) => Err(invalid()),
            }
        };
        let name = text(2)?;
        let caption = text(3)?;
        let mut rect = if self.slc_of(b, i)?.timeline.is_some() {
            TIMELINE_RECT
        } else {
            DEFAULT_RECT
        };

        for (arg, slot) in [(4, 1), (5, 0), (6, 2), (7, 3)] {
            if let Some(v) = a.get(arg) {
                let x = crate::controls::num_arg(v, a.lcid)?;
                rect[slot] = x;
            }
        }
        let j = self
            .book_mut(b)?
            .wb
            .slicer_add(i, sheet, name.as_deref(), caption.as_deref(), rect)
            .map_err(from_err)?;
        Ok(self.alloc(Object::Slicer(b, SlicerPart::Slicer(i, j))))
    }

    fn one_slicer_member(
        &mut self,
        b: BookId,
        i: usize,
        j: usize,
        m: &'static Member,
        a: &Args,
    ) -> Result<Variant, OmError> {
        use InvokeKind::*;
        let put = matches!(m.kind, Put | PutRef);
        let c = self.slc_of(b, i)?;
        let s = c
            .slicers
            .get(j)
            .cloned()
            .ok_or_else(OmError::object_required)?;
        let geo = |k: usize| -> Result<Variant, OmError> {
            Ok(Variant::R8(self.slicer_geometry(b, i, j)?[k]))
        };
        match (m.name, put) {
            ("Name", false) => Ok(Variant::Bstr(s.name)),
            ("Name", true) => {
                let n = match a.put.as_ref() {
                    Some(Variant::Bstr(n)) => n.clone(),
                    _ => return Err(OmError::type_mismatch()),
                };
                self.book_mut(b)?
                    .wb
                    .slicer_rename(i, j, &n)
                    .map_err(from_err)?;
                Ok(Variant::Empty)
            }
            ("Caption", false) => Ok(Variant::Bstr(s.caption)),
            ("Caption", true) => {
                let n = match a.put.as_ref() {
                    Some(Variant::Bstr(n)) => n.clone(),
                    _ => return Err(OmError::type_mismatch()),
                };
                self.edit_slicer(b, i, j, |x| x.caption = n);
                Ok(Variant::Empty)
            }
            ("NumberOfColumns" | "DisplayHeader", false) if c.timeline.is_some() => {
                Err(OmError::app_defined())
            }
            ("NumberOfColumns", true) if c.timeline.is_some() => Err(invalid()),
            ("TimelineViewState", false) if c.timeline.is_some() => {
                Ok(self.alloc(Object::Slicer(b, SlicerPart::TimelineView(i, j))))
            }
            ("TimelineViewState", false) => Ok(Variant::Dispatch(None)),
            ("Style", true)
                if c.timeline.is_some()
                    && !matches!(a.put.as_ref(), Some(Variant::Bstr(n)) if n.starts_with("TimeSlicerStyle")) =>
            {
                Err(OmError::hresult(crate::error::DISP_E_BADPARAMCOUNT))
            }
            ("NumberOfColumns", false) => Ok(Variant::I4(s.columns)),
            ("NumberOfColumns", true) => {
                let v = crate::controls::put_num(a)? as i32;
                if !(1..=20_000).contains(&v) {
                    return Err(invalid());
                }
                self.edit_slicer(b, i, j, |x| x.columns = v);
                Ok(Variant::Empty)
            }
            ("DisplayHeader", false) => Ok(Variant::Bool(s.header)),
            ("DisplayHeader", true) => {
                let v = crate::chartparts::put_bool(a)?;
                self.edit_slicer(b, i, j, |x| x.header = v);
                Ok(Variant::Empty)
            }
            ("Locked", false) => Ok(Variant::Bool(s.locked)),
            ("Locked", true) => {
                let v = crate::chartparts::put_bool(a)?;
                self.edit_slicer(b, i, j, |x| x.locked = v);
                Ok(Variant::Empty)
            }
            ("ColumnWidth", false) => Ok(Variant::R8(s.col_w)),
            ("ColumnWidth", true) => {
                let v = crate::controls::put_num(a)?;
                self.edit_slicer(b, i, j, |x| x.col_w = v);
                Ok(Variant::Empty)
            }
            ("RowHeight", false) => Ok(Variant::R8(s.row_h)),
            ("RowHeight", true) => {
                let v = crate::controls::put_num(a)?;
                self.edit_slicer(b, i, j, |x| x.row_h = v);
                Ok(Variant::Empty)
            }
            ("Left", false) => geo(0),
            ("Top", false) => geo(1),
            ("Width", false) => geo(2),
            ("Height", false) => geo(3),
            ("Left" | "Top" | "Width" | "Height", true) => {
                let k = ["Left", "Top", "Width", "Height"]
                    .iter()
                    .position(|n| *n == m.name)
                    .unwrap_or(0);
                let v = crate::controls::put_num(a)?;
                self.slicer_set_geometry(b, i, j, k, v)?;
                Ok(Variant::Empty)
            }
            ("Style", false) => Ok(self.alloc(Object::Slicer(b, SlicerPart::Style(i, j)))),
            ("Style", true) => {
                let n = match a.put.as_ref() {
                    Some(Variant::Bstr(n)) => n.clone(),
                    _ => return Err(OmError::type_mismatch()),
                };
                self.edit_slicer(b, i, j, |x| x.style = n);
                Ok(Variant::Empty)
            }
            ("SlicerCacheType", false) if c.timeline.is_some() => Ok(Variant::I4(2)),
            ("SlicerCacheType", false) => Ok(Variant::I4(0)),
            ("SlicerCache", false) => Ok(self.alloc(Object::Slicer(b, SlicerPart::Cache(i)))),
            ("Shape", false) => Ok(self.alloc(Object::Draw(DrawRef {
                book: b,
                sheet: s.sheet,
                id: s.shape,
                kind: DrawKind::Shape,
            }))),
            ("Delete", _) => {
                self.book_mut(b)?.wb.slicer_delete(i, j);
                Ok(Variant::Empty)
            }
            ("Parent", false) => Ok(self.sheet_object(b, s.sheet)),
            _ => Err(unsupported("Slicer", m)),
        }
    }

    fn timeline_member(
        &mut self,
        b: BookId,
        i: usize,
        m: &'static Member,
        a: &Args,
    ) -> Result<Variant, OmError> {
        let range = self.slc_of(b, i)?.timeline.and_then(|t| t.range);
        let need = |v: Option<f64>| v.map(Variant::Date).ok_or_else(OmError::app_defined);
        match m.name {
            "StartDate" | "FilterValue1" => need(range.map(|r| r.0)),
            "EndDate" | "FilterValue2" => need(range.map(|r| r.1)),
            "FilterType" => match range {
                Some((x, y)) if x == y => Ok(Variant::I4(29)),
                Some(_) => Ok(Variant::I4(35)),
                None => Err(OmError::app_defined()),
            },
            "SingleRangeFilterState" => Ok(Variant::Bool(true)),
            "SetFilterDateRange" => {
                if a.get(1).is_none() {
                    return Err(OmError::hresult(crate::error::DISP_E_BADPARAMCOUNT));
                }
                let num = |k: usize| match a.get(k) {
                    Some(Variant::R8(x) | Variant::Date(x)) => Some(*x),
                    Some(Variant::I4(x)) => Some(f64::from(*x)),
                    _ => None,
                };
                let r = self.book_mut(b)?.wb.timeline_set(i, num(0), num(1));
                Ok(Variant::I4(r))
            }
            "Parent" => Ok(self.alloc(Object::Slicer(b, SlicerPart::Cache(i)))),
            _ => Err(unsupported("TimelineState", m)),
        }
    }

    fn timeline_view_member(
        &mut self,
        b: BookId,
        i: usize,
        j: usize,
        m: &'static Member,
        a: &Args,
    ) -> Result<Variant, OmError> {
        use InvokeKind::*;
        let put = matches!(m.kind, Put | PutRef);
        let s = self
            .slc_of(b, i)?
            .slicers
            .get(j)
            .cloned()
            .ok_or_else(OmError::object_required)?;
        let flag = |this: &mut Self,
                    get: bool,
                    set: fn(&mut xlcalc::slicer::TimelineView, bool)|
         -> Result<Variant, OmError> {
            if put {
                let v = crate::chartparts::put_bool(a)?;
                let mut tv = s.tv.clone();
                set(&mut tv, v);

                const H: [f64; 16] = [
                    52.5, 73.0, 67.5, 88.0, 73.0, 93.0, 88.0, 108.0, 72.0, 92.5, 87.0, 107.5, 73.0,
                    93.0, 88.0, 108.0,
                ];
                let k = usize::from(tv.header)
                    + 2 * usize::from(tv.hscroll)
                    + 4 * usize::from(tv.label)
                    + 8 * usize::from(tv.time_level);
                this.slicer_set_geometry(b, i, j, 3, H[k])?;
                this.edit_slicer(b, i, j, |x| x.tv = tv);
                Ok(Variant::Empty)
            } else {
                Ok(Variant::Bool(get))
            }
        };
        match m.name {
            "Level" if !put => Ok(Variant::I4(s.tv.level)),
            "Level" => {
                let v = crate::controls::put_num(a)? as i32;
                if !(0..=3).contains(&v) {
                    return Err(invalid());
                }
                self.edit_slicer(b, i, j, |x| x.tv.level = v);
                Ok(Variant::Empty)
            }
            "ShowHeader" => flag(self, s.tv.header, |t, v| t.header = v),
            "ShowHorizontalScrollbar" => flag(self, s.tv.hscroll, |t, v| t.hscroll = v),
            "ShowSelectionLabel" => flag(self, s.tv.label, |t, v| t.label = v),
            "ShowTimeLevel" => flag(self, s.tv.time_level, |t, v| t.time_level = v),
            "Parent" => Ok(self.alloc(Object::Slicer(b, SlicerPart::Slicer(i, j)))),
            _ => Err(unsupported("TimelineViewState", m)),
        }
    }

    fn slicer_pivots_member(
        &mut self,
        b: BookId,
        i: usize,
        m: &'static Member,
        a: &Args,
    ) -> Result<Variant, OmError> {
        let c = self.slc_of(b, i)?;
        let pivot_arg = |this: &Self| -> Result<String, OmError> {
            match a.get(0) {
                Some(Variant::Dispatch(Some(id))) => match this.arena.get(*id) {
                    Some(Object::Pivot(crate::pivot::PivotRef::Table(_, name))) => Ok(name.clone()),
                    _ => Err(invalid()),
                },
                _ => Err(invalid()),
            }
        };
        match m.name {
            "Count" => Ok(Variant::I4(c.pivots.len() as i32)),
            "Item" | "_Default" => {
                let k = self.slicer_pick(a.get(0), a.lcid, &c.pivots)?;
                let name = c.pivots[k].clone();
                Ok(self.alloc(Object::Pivot(crate::pivot::PivotRef::Table(b, name))))
            }
            "_NewEnum" => {
                let items = c
                    .pivots
                    .iter()
                    .map(|n| self.alloc(Object::Pivot(crate::pivot::PivotRef::Table(b, n.clone()))))
                    .collect();
                Ok(self.alloc_enumerator(items))
            }
            "AddPivotTable" => {
                let name = pivot_arg(self)?;
                self.book_mut(b)?
                    .wb
                    .slicer_pivot_add(i, &name)
                    .map_err(from_err)?;
                Ok(Variant::Empty)
            }
            "RemovePivotTable" => {
                let name = pivot_arg(self)?;
                self.book_mut(b)?.wb.slicer_pivot_remove(i, &name);
                Ok(Variant::Empty)
            }
            "Parent" => Ok(self.alloc(Object::Slicer(b, SlicerPart::Cache(i)))),
            _ => Err(unsupported("SlicerPivotTables", m)),
        }
    }

    fn edit_slicer(
        &mut self,
        b: BookId,
        i: usize,
        j: usize,
        f: impl FnOnce(&mut xlcalc::slicer::Slicer),
    ) {
        if let Some(s) = self
            .book_mut(b)
            .ok()
            .and_then(|x| x.wb.slicer_cache_mut(i))
            .and_then(|c| c.slicers.get_mut(j))
        {
            f(s);
        }
    }
}
