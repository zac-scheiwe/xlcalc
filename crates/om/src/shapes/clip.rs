use xlcalc::drawing::{pt_to_emu, Shape, ShapeKind};
use xlcalc::{Rect, SheetId};
use xlcalc_variant::Variant;

use super::{base_name, DrawRef};
use crate::app::{unsupported, Application, BookId};
use crate::error::OmError;
use crate::typelib::Member;

#[derive(Clone, Debug)]
pub(crate) struct ShapeClip {
    pub src: (BookId, SheetId, u32),
    pub shape: Shape,

    pub size: (i64, i64),
    pub base: &'static str,
}

impl Application<'_> {
    pub(super) fn shape_to_clipboard(
        &mut self,
        r: &DrawRef,
        sh: Shape,
        size: (i64, i64),
        cut: bool,
        m: &'static Member,
    ) -> Result<Variant, OmError> {
        if matches!(sh.kind, ShapeKind::Group(_)) || sh.parent.is_some() {
            return Err(unsupported("Shape", m));
        }
        let base = base_name(&sh.kind);
        self.shape_clip = Some(ShapeClip {
            src: (r.book, r.sheet, r.id),
            shape: sh,
            size,
            base,
        });

        self.clip = None;
        self.clip_more.0.clear();
        if cut {
            self.draw_wb(r.book)?.delete_shape(r.sheet, r.id);
        }
        Ok(Variant::Empty)
    }

    pub(crate) fn paste_shape_clip(
        &mut self,
        b: BookId,
        s: SheetId,
        dest: Option<(SheetId, Rect)>,
    ) -> Result<Variant, OmError> {
        let Some(clip) = self.shape_clip.clone() else {
            return Err(OmError::app_defined_with(
                "Paste method of Worksheet class failed",
            ));
        };
        let (ds, corner) = match dest {
            Some((ds, r)) => (ds, (r.r0, r.c0)),
            None => {
                let sel = self.book(b)?.wb.selection_of(s);
                let first = sel.areas.first().map_or((0, 0), |r| (r.r0, r.c0));
                (s, first)
            }
        };
        let (bx, by) = self.book(b)?.wb.cell_corner_emu(ds, corner.0, corner.1);
        let key = (b, ds, bx, by);
        let same = self.paste_source == Some(clip.src);
        let step = pt_to_emu(12.0);
        let (x, y) = match self.paste_chain.get(&key) {
            Some(&(px, py)) if same => (px + step, py + step),
            Some(&at) => at,
            None => (bx, by),
        };
        self.draw_wb(b)?
            .paste_shape(ds, &clip.shape, clip.base, (x, y, clip.size.0, clip.size.1));
        self.paste_chain.insert(key, (x, y));
        self.paste_source = Some(clip.src);
        Ok(Variant::Bool(true))
    }
}
