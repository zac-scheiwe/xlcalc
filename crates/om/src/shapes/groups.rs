use xlcalc_variant::Variant;

use super::{emu_pt, out_of_bounds, DrawKind, DrawRef, NO_NAME};
use crate::app::Application;
use crate::conv::Args;
use crate::error::OmError;
use crate::typelib::{InvokeKind, Member};

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
        let mut ids = Vec::new();
        for w in &wanted {
            let id = match w {
                Variant::Bstr(name) => self
                    .book(b)?
                    .wb
                    .drawing(s)
                    .and_then(|d| {
                        d.shapes
                            .iter()
                            .find(|x| x.name.eq_ignore_ascii_case(name))
                            .map(|x| x.id)
                    })
                    .ok_or_else(|| range_error(Some(NO_NAME)))?,
                v => {
                    let n = crate::controls::num_arg(v, a.lcid)?.trunc();
                    if n < 1.0 {
                        return Err(range_error(None));
                    }
                    items
                        .get(n as usize - 1)
                        .and_then(|i| i.0.shape_id())
                        .ok_or_else(|| range_error(None))?
                }
            };
            ids.push(id);
        }
        Ok(self.new_shape_range(b, s, ids))
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
                let g = self
                    .draw_wb(b)?
                    .group_shapes(s, &ids)
                    .ok_or_else(OmError::app_defined)?;
                Ok(one(self, g))
            }
            ("Ungroup", _) => {
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
