use xlcalc_variant::Variant;

use super::{emu_pt, out_of_bounds, DrawKind, DrawRef, Item, RangeItem, NO_NAME};
use crate::app::{Application, BookId};
use crate::controls::Coll;
use crate::conv::Args;
use crate::error::OmError;
use crate::typelib::{InvokeKind, Member};
use xlcalc::drawing::ShapeKind;
use xlcalc::SheetId;

fn range_error(text: Option<&str>) -> OmError {
    OmError::exception(0x800A_03EC_u32 as i32, text)
}

fn single_only() -> OmError {
    OmError::exception(
        0x8007_0005_u32 as i32,
        Some("This member can only be accessed for a single shape."),
    )
}

impl Application<'_> {
    pub(super) fn shapes_range(&mut self, r: &DrawRef, a: &Args) -> Result<Variant, OmError> {
        let (b, s) = (r.book, r.sheet);
        let items = self.merged_items(b, s)?;
        let wanted: Vec<Variant> = match a.get(0) {
            None => {
                let ids = items.iter().filter_map(|i| i.0.shape_id()).collect();
                return Ok(self.new_shape_range(b, s, ids));
            }
            Some(Variant::Array(sa)) => sa.data.clone(),
            Some(v) => vec![v.clone()],
        };

        let legacy = self.legacy_items(b, s)?;
        let notes = legacy
            .iter()
            .filter(|i| matches!(i.0, Item::Note(_)))
            .count();
        let (mut first, mut second) = (Vec::new(), Vec::new());
        for w in &wanted {
            match w {
                Variant::Bstr(name) => {
                    let d = self.book(b)?.wb.drawing(s);
                    let hit =
                        d.and_then(|d| d.shapes.iter().find(|x| x.name.eq_ignore_ascii_case(name)));
                    if let Some(hit) = hit {
                        if hit.parent.is_none() {
                            first.push(RangeItem::new(Item::Shape(hit.id), false));
                        }
                        continue;
                    }
                    let named = |i: &&(Item, String)| i.1.eq_ignore_ascii_case(name);
                    if let Some(i) = items.iter().find(named) {
                        first.push(RangeItem::new(i.0, false));
                    } else if let Some(i) = legacy.iter().find(named) {
                        second.push(RangeItem::new(i.0, true));
                    } else {
                        return Err(range_error(Some(NO_NAME)));
                    }
                }
                v => {
                    let n = crate::controls::num_arg(v, a.lcid)?.trunc();
                    if n < 1.0 {
                        return Err(range_error(None));
                    }
                    let n = n as usize;
                    if let Some(i) = items.get(n - 1) {
                        first.push(RangeItem::new(i.0, false));
                    } else if n <= items.len() + notes {
                        let i = legacy.get(n - items.len() - 1);
                        let i = i.ok_or_else(|| range_error(None))?;
                        second.push(RangeItem::new(i.0, true));
                    } else {
                        return Err(range_error(None));
                    }
                }
            }
        }
        first.append(&mut second);
        let ids: Option<Vec<u32>> = first.iter().map(|i| i.item.shape_id()).collect();
        match ids {
            Some(ids) => Ok(self.new_shape_range(b, s, ids)),
            None => Ok(self.mixed_range(b, s, first)),
        }
    }

    fn legacy_items(&mut self, b: BookId, s: SheetId) -> Result<Vec<(Item, String)>, OmError> {
        let mut all = self.shapes_items(b, s)?;
        let bk = self.book(b)?;
        let spid = |i: &Item| match i {
            Item::Control(uid) => bk
                .controls
                .get(&s)
                .and_then(|c| c.list.iter().find(|c| c.uid == *uid))
                .map(|c| c.spid),
            Item::Note(id) => bk.wb.note_by_id(s, *id).map(|(_, n)| n.spid),
            Item::Shape(_) => None,
        };
        all.retain(|i| spid(&i.0).is_some());
        all.sort_by_key(|i| spid(&i.0));
        Ok(all)
    }

    fn range_shape(&mut self, r: &DrawRef, item: RangeItem) -> Result<Variant, OmError> {
        self.item_handle(r.book, r.sheet, &item.item, true)
    }

    pub(crate) fn control_shape_range(&mut self, r: &crate::controls::CtlRef) -> Variant {
        let item = RangeItem::new(Item::Control(r.uid), false);
        self.mixed_range(r.book, r.sheet, vec![item])
    }

    fn mixed_range(&mut self, b: BookId, s: SheetId, items: Vec<RangeItem>) -> Variant {
        self.mixed_ranges.push(items);
        let i = self.mixed_ranges.len() - 1;
        self.draw_obj(b, s, 0, DrawKind::Mixed(i))
    }

    pub(super) fn mixed_range_member(
        &mut self,
        r: &DrawRef,
        index: usize,
        m: &'static Member,
        a: &Args,
    ) -> Result<Variant, OmError> {
        let items = self.mixed_ranges.get(index).cloned().unwrap_or_default();
        match m.name {
            "Count" => return Ok(Variant::I4(items.len() as i32)),
            "Item" | "_Default" => {
                let v = a.get(0).ok_or_else(OmError::app_defined)?;
                let n = crate::controls::num_arg(v, a.lcid)?.trunc();
                let item = (n >= 1.0).then(|| items.get(n as usize - 1)).flatten();
                let item = *item.ok_or_else(out_of_bounds)?;
                return self.range_shape(r, item);
            }
            "_NewEnum" => {
                let handles = items
                    .iter()
                    .map(|i| self.range_shape(r, *i))
                    .collect::<Result<_, _>>()?;
                return Ok(self.alloc_enumerator(handles));
            }
            _ => {}
        }
        let [one] = items[..] else {
            return Err(range_error(None));
        };
        match one.item {
            Item::Control(uid) if one.legacy && m.name == "ZOrderPosition" => {
                let spid = self
                    .book(r.book)?
                    .controls
                    .get(&r.sheet)
                    .and_then(|c| c.list.iter().find(|c| c.uid == uid))
                    .map(|c| c.spid)
                    .ok_or_else(OmError::object_required)?;
                self.note_z(r.book, r.sheet, spid)
            }
            Item::Shape(id) => {
                let sub = DrawRef {
                    id,
                    kind: DrawKind::Shape,
                    ..*r
                };
                self.shape_member(&sub, m, a)
            }
            Item::Note(id) => self.comment_shape_member(r.book, r.sheet, id, m, a),
            Item::Control(uid) => {
                let kind = self
                    .collection_items(r.book, r.sheet, Coll::Shapes)?
                    .into_iter()
                    .find(|c| c.0 == uid)
                    .map(|c| c.1)
                    .ok_or_else(OmError::object_required)?;
                let control = crate::controls::CtlRef {
                    book: r.book,
                    sheet: r.sheet,
                    uid,
                    kind,
                    shape: true,
                    ranged: true,
                };
                self.control_member(&control, m, a)
            }
        }
    }

    pub(super) fn shape_range_member(
        &mut self,
        r: &DrawRef,
        index: usize,
        m: &'static Member,
        a: &Args,
    ) -> Result<Variant, OmError> {
        let (b, s) = (r.book, r.sheet);
        let ids = self.shape_ranges.get(index).cloned().unwrap_or_default();
        let one = |app: &mut Self, id: u32| app.draw_obj(b, s, id, DrawKind::Shape);
        if ids.len() == 1 && !matches!(m.name, "Count" | "Item" | "_Default" | "_NewEnum" | "Group")
        {
            let sub = DrawRef {
                id: ids[0],
                kind: DrawKind::Shape,
                ..*r
            };
            return self.shape_member(&sub, m, a);
        }
        match (m.name, m.kind) {
            ("Count", InvokeKind::Get) => Ok(Variant::I4(ids.len() as i32)),
            ("Item" | "_Default", _) => {
                let id = match a.get(0) {
                    Some(Variant::Bstr(name)) => self
                        .book(b)?
                        .wb
                        .drawing(s)
                        .and_then(|d| {
                            ids.iter().copied().find(|i| {
                                d.get(*i).is_some_and(|x| x.name.eq_ignore_ascii_case(name))
                            })
                        })
                        .ok_or_else(|| OmError::exception(0x8007_0057_u32 as i32, Some(NO_NAME)))?,
                    Some(v) => {
                        let n = crate::controls::num_arg(v, a.lcid)?.trunc();
                        if n < 1.0 {
                            return Err(out_of_bounds());
                        }
                        *ids.get(n as usize - 1).ok_or_else(out_of_bounds)?
                    }
                    None => return Err(OmError::app_defined()),
                };
                Ok(one(self, id))
            }
            ("_NewEnum", _) => {
                let handles = ids.iter().map(|i| one(self, *i)).collect();
                Ok(self.alloc_enumerator(handles))
            }
            ("Group", _) => {
                if ids.len() < 2 {
                    return Err(range_error(Some(
                        "The ShapeRange object must contain at least two items.",
                    )));
                }

                let mut seen = ids.clone();
                seen.sort_unstable();
                seen.dedup();
                if seen.len() != ids.len() {
                    return Err(OmError::app_defined());
                }
                let g = self
                    .draw_wb(b)?
                    .group_shapes(s, &ids)
                    .ok_or_else(OmError::app_defined)?;
                Ok(one(self, g))
            }
            ("Ungroup", _) => {
                let any_group = {
                    let d = self.book(b)?.wb.drawing(s);
                    ids.iter().any(|i| {
                        d.and_then(|d| d.get(*i))
                            .is_some_and(|x| matches!(x.kind, ShapeKind::Group(_)))
                    })
                };
                if !any_group {
                    return Err(range_error(Some(
                        "This member can only be accessed for a group.",
                    )));
                }
                let mut out = Vec::new();
                for id in &ids {
                    if let Some(k) = self.draw_wb(b)?.ungroup_shape(s, *id) {
                        out.extend(k);
                    }
                }
                Ok(self.new_shape_range(b, s, out))
            }
            ("Delete", _) => {
                for id in &ids {
                    self.draw_wb(b)?.delete_shape(s, *id);
                }
                Ok(Variant::Empty)
            }
            ("Left" | "Top" | "Width" | "Height", InvokeKind::Get) => {
                let wb = &self.book(b)?.wb;
                let vals: Vec<i64> = ids
                    .iter()
                    .filter_map(|i| wb.drawing(s)?.get(*i))
                    .map(|x| {
                        let rc = wb.shape_rect_emu(s, &x.pos);
                        match m.name {
                            "Left" => rc.0,
                            "Top" => rc.1,
                            "Width" => rc.2,
                            _ => rc.3,
                        }
                    })
                    .collect();
                match vals.first() {
                    Some(v) if vals.iter().all(|x| x == v) => Ok(emu_pt(*v)),
                    _ => Ok(Variant::R4(i32::MIN as f32)),
                }
            }
            _ => Err(single_only()),
        }
    }
}
