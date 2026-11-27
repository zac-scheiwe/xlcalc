use xlcalc::{Rect, SheetId};
use xlcalc_variant::Variant;

use crate::app::{Application, BookId, Mode, Object};
use crate::conv::Args;
use crate::error::{OmError, DISP_E_BADPARAMCOUNT};
use crate::shapes::DrawKind;
use crate::typelib::{InvokeKind, Member};

const EMU_PT: f64 = 12_700.0;

const BOX_W: f64 = 104.0;
const BOX_H: f64 = 61.5;

fn denied(text: &str) -> OmError {
    OmError::exception(0x8007_0005_u32 as i32, Some(text))
}

#[derive(Clone)]
pub(crate) struct CShape {
    alt: Option<String>,
    black_white: i32,
    lock_aspect: i32,
    locked: bool,
    placement: i32,
    auto_shape: i32,
    auto_margins: bool,
    auto_size: bool,
    h_align: i32,
    v_align: i32,

    margins: [f32; 4],
    orientation: i32,
}

impl Default for CShape {
    fn default() -> Self {
        CShape {
            alt: None,
            black_white: 1,
            lock_aspect: 0,
            locked: true,
            placement: 3,
            auto_shape: 1,
            auto_margins: true,
            auto_size: false,
            h_align: -4131,
            v_align: -4160,
            margins: [7.2, 3.6, 7.2, 3.6],
            orientation: 1,
        }
    }
}

fn invalid() -> OmError {
    OmError::exception(0x8007_0057_u32 as i32, None)
}

fn not_implemented() -> OmError {
    OmError::exception(0x8000_4001_u32 as i32, None)
}

impl Application<'_> {
    fn note_box(&self, b: BookId, s: SheetId, row: u32, col: u32) -> Result<(f64, f64), OmError> {
        let wb = &self.book(b)?.wb;
        let pt = |emu: i64| emu as f64 / EMU_PT;
        let total_w = pt(wb.col_left_emu(s, crate::addr::max_cols()));
        let total_h = pt(wb.row_top_emu(s, crate::addr::max_rows()));
        let mut left = pt(wb.col_left_emu(s, col + 1)) + 7.5;
        if left + BOX_W > total_w {
            left = total_w - BOX_W - 55.5;
        }
        let top = (pt(wb.row_top_emu(s, row)) - 5.0)
            .max(1.0)
            .min(total_h - BOX_H - 10.0);
        Ok((left, top))
    }

    pub(crate) fn comment_shape_member(
        &mut self,
        b: BookId,
        s: SheetId,
        id: u32,
        m: &'static Member,
        a: &Args,
    ) -> Result<Variant, OmError> {
        use InvokeKind::*;
        let ((row, col), note) = self
            .book(b)?
            .wb
            .note_by_id(s, id)
            .map(|(k, n)| (k, n.clone()))
            .ok_or_else(OmError::object_required)?;

        if self.fc_poisoned
            && matches!(
                m.name,
                "Locked" | "TopLeftCell" | "BottomRightCell" | "OnAction" | "Placement"
            )
        {
            return Err(OmError::app_defined());
        }
        let key = (b, s, id);
        let st = self.comment_shapes.get(&key).cloned().unwrap_or_default();
        if m.name == "Delete" {
            self.book_mut(b)?.wb.note_remove(s, id);
            self.comment_shapes.remove(&key);
            return Ok(Variant::Empty);
        }
        if m.kind != Get {
            return self.comment_shape_put(key, st, m, a);
        }
        let (left, top) = self.note_box(b, s, row, col)?;
        let stub = |this: &mut Self, name: &'static str| {
            Ok(this.alloc(Object::Draw(crate::shapes::DrawRef {
                book: b,
                sheet: s,
                id: 0,
                kind: DrawKind::Stub(name),
            })))
        };
        let cell_at = |this: &mut Self, x: f64, y: f64| -> Result<Variant, OmError> {
            let an = this
                .book(b)?
                .wb
                .xy_anchor(s, (x * EMU_PT) as i64, (y * EMU_PT) as i64);
            this.new_range(b, s, Rect::cell(an.row, an.col), Mode::Cells)
        };
        let shape_id = i32::try_from(note.spid).unwrap_or(0);
        match m.name {
            "Name" => {
                let n = self.book(b)?.wb.legacy_shape_number(s, note.spid);
                Ok(Variant::Bstr(format!("Comment {n}")))
            }
            "Left" => Ok(Variant::R4(left as f32)),
            "Top" => Ok(Variant::R4(top as f32)),
            "Width" => Ok(Variant::R4(BOX_W as f32)),
            "Height" => Ok(Variant::R4(BOX_H as f32)),
            "Rotation" => Ok(Variant::R4(0.0)),
            "TopLeftCell" => cell_at(self, left, top),
            "BottomRightCell" => cell_at(self, left + BOX_W, top + BOX_H),
            "AutoShapeType" => Ok(Variant::I4(st.auto_shape)),
            "BlackWhiteMode" => Ok(Variant::I4(st.black_white)),
            "LockAspectRatio" => Ok(Variant::I4(st.lock_aspect)),
            "ZOrderPosition" => self.note_z(b, s, note.spid),
            "Script" => Ok(Variant::Dispatch(None)),

            "DrawingObject" => stub(self, "TextBox"),
            "HasDiagram" | "HasDiagramNode" => Ok(Variant::I4(0)),
            "Diagram" | "DiagramNode" => Err(not_implemented()),
            "CanvasItems" => Err(OmError::app_defined()),
            "ConnectionSiteCount"
            | "Connector"
            | "HorizontalFlip"
            | "VerticalFlip"
            | "Child"
            | "HasChart" => Ok(Variant::I4(0)),
            "Type" => Ok(Variant::I4(4)),
            "Visible" => Ok(Variant::I4(if note.visible { -1 } else { 0 })),
            "Placement" => Ok(Variant::I4(st.placement)),
            "ID" => Ok(Variant::I4(shape_id)),
            "Locked" => Ok(Variant::Bool(st.locked)),
            "OnAction" => Ok(Variant::Bstr(String::new())),
            "AlternativeText" => Ok(Variant::Bstr(
                st.alt
                    .clone()
                    .unwrap_or_else(|| format!("Text Box: {}", note.text)),
            )),
            "Parent" => self.new_range(b, s, Rect::cell(row, col), Mode::Cells),
            "Application" => Ok(Variant::Dispatch(Some(self.handle()))),
            "Adjustments" => stub(self, "Adjustments"),
            "TextFrame" => Ok(self.alloc(Object::CommentTextFrame(b, s, id))),
            "Fill" => stub(self, "FillFormat"),
            "Line" => stub(self, "LineFormat"),
            "Shadow" => stub(self, "ShadowFormat"),
            "ThreeD" => stub(self, "ThreeDFormat"),
            "Model3D" => stub(self, "Model3DFormat"),
            "ControlFormat" => stub(self, "ControlFormat"),
            "LinkFormat" => stub(self, "LinkFormat"),
            "OLEFormat" => stub(self, "OLEFormat"),
            "Callout" => Err(denied(
                "This member can only be accessed for a line callout.",
            )),
            "Nodes" | "Vertices" => Err(denied(
                "This member can only be accessed for a freeform object.",
            )),
            "PictureFormat" => Err(denied(
                "This member can only be accessed for a picture or an OLE object.",
            )),
            "ConnectorFormat" => Err(OmError::app_defined_with(
                "This member can only be accessed for a connector.",
            )),
            "GroupItems" => Err(OmError::app_defined_with(
                "This member can only be accessed for a group.",
            )),
            "TextEffect" => Err(OmError::exception(
                0x8007_0057_u32 as i32,
                Some("This member can only be accessed for a WordArt object."),
            )),
            "ParentGroup" => Err(OmError::exception(
                0x8007_0057_u32 as i32,
                Some("The specified value is out of range."),
            )),
            "Hyperlink" | "FormControlType" => Err(OmError::app_defined()),
            "Chart" | "TextFrame2" | "ShapeStyle" | "BackgroundStyle" | "SoftEdge" | "Glow"
            | "Reflection" | "HasSmartArt" | "SmartArt" | "Title" | "GraphicStyle"
            | "Decorative" => Err(not_implemented()),
            "Flip" | "IncrementLeft" | "IncrementRotation" | "IncrementTop" | "ScaleHeight"
            | "ScaleWidth" | "ZOrder" => Err(OmError::hresult(DISP_E_BADPARAMCOUNT)),
            _ => Err(crate::app::unsupported("Shape", m)),
        }
    }

    fn comment_shape_put(
        &mut self,
        key: (BookId, SheetId, u32),
        mut st: CShape,
        m: &'static Member,
        a: &Args,
    ) -> Result<Variant, OmError> {
        match m.name {
            "AlternativeText" => st.alt = Some(self.put_string(a)?),
            "AutoShapeType" => {
                let n = self.put_i32(a)?;
                if !(1..=182).contains(&n) {
                    return Err(invalid());
                }
                st.auto_shape = n;
            }
            "BlackWhiteMode" => {
                let n = self.put_i32(a)?;
                if !(1..=10).contains(&n) {
                    return Err(invalid());
                }
                st.black_white = n;
            }
            "LockAspectRatio" => {
                let n = self.put_i32(a)?;
                if !matches!(n, 0 | -1) {
                    return Err(invalid());
                }
                st.lock_aspect = n;
            }
            "Locked" => st.locked = self.put_bool(a)?,
            "Placement" => {
                let n = self.put_i32(a)?;
                if !(1..=3).contains(&n) {
                    return Err(OmError::app_defined());
                }
                st.placement = n;
            }
            "OnAction" => return Err(OmError::app_defined()),
            "ShapeStyle" | "BackgroundStyle" | "GraphicStyle" | "Title" | "Decorative" => {
                return Err(not_implemented())
            }
            _ => return Err(crate::app::unsupported("Shape", m)),
        }
        self.comment_shapes.insert(key, st);
        Ok(Variant::Empty)
    }

    pub(crate) fn comment_textframe_member(
        &mut self,
        b: BookId,
        s: SheetId,
        id: u32,
        m: &'static Member,
        a: &Args,
    ) -> Result<Variant, OmError> {
        use InvokeKind::*;
        let key = (b, s, id);
        let mut st = self.comment_shapes.get(&key).cloned().unwrap_or_default();
        if matches!(m.name, "HorizontalOverflow" | "VerticalOverflow") {
            return Err(OmError::exception(0x8000_4005_u32 as i32, None));
        }
        if self.fc_poisoned
            && matches!(
                m.name,
                "AutoSize" | "HorizontalAlignment" | "VerticalAlignment"
            )
        {
            return Err(OmError::app_defined());
        }
        if m.kind == Get {
            return Ok(match m.name {
                "AutoMargins" => Variant::Bool(st.auto_margins),
                "AutoSize" => Variant::Bool(st.auto_size),
                "HorizontalAlignment" => Variant::I4(st.h_align),
                "VerticalAlignment" => Variant::I4(st.v_align),
                "MarginLeft" => Variant::R4(st.margins[0]),
                "MarginTop" => Variant::R4(st.margins[1]),
                "MarginRight" => Variant::R4(st.margins[2]),
                "MarginBottom" => Variant::R4(st.margins[3]),
                "Orientation" => Variant::I4(st.orientation),
                "ReadingOrder" => Variant::I4(27),
                "Application" => Variant::Dispatch(Some(self.handle())),
                _ => return Err(crate::app::unsupported("TextFrame", m)),
            });
        }
        match m.name {
            "AutoMargins" => st.auto_margins = self.put_bool(a)?,
            "AutoSize" => st.auto_size = self.put_bool(a)?,
            "HorizontalAlignment" | "VerticalAlignment" => {
                let n = self.put_i32(a)?;
                let horizontal = m.name == "HorizontalAlignment";
                let v = match (n, horizontal) {
                    (1 | -4131, true) => -4131,
                    (2 | -4108, _) => -4108,
                    (3 | -4152, true) => -4152,
                    (1 | -4160, false) => -4160,
                    (3 | -4107, false) => -4107,
                    _ => return Err(OmError::app_defined()),
                };
                if horizontal {
                    st.h_align = v;
                } else {
                    st.v_align = v;
                }
            }
            "MarginLeft" | "MarginTop" | "MarginRight" | "MarginBottom" => {
                let x = self.put_f64(a)?;
                if !(0.0..=100_000.0).contains(&x) {
                    return Err(invalid());
                }
                let i = match m.name {
                    "MarginLeft" => 0,
                    "MarginTop" => 1,
                    "MarginRight" => 2,
                    _ => 3,
                };
                st.margins[i] = x as f32;
            }
            "Orientation" => {
                let n = self.put_i32(a)?;
                if !(1..=6).contains(&n) {
                    return Err(invalid());
                }
                st.orientation = n;
            }
            "ReadingOrder" => {}
            _ => return Err(crate::app::unsupported("TextFrame", m)),
        }
        self.comment_shapes.insert(key, st);
        Ok(Variant::Empty)
    }
}
