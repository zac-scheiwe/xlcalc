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

fn not_implemented() -> OmError {
    OmError::exception(0x8000_4001_u32 as i32, None)
}

impl Application<'_> {
    fn note_box(&self, b: BookId, s: SheetId, row: u32, col: u32) -> Result<(f64, f64), OmError> {
        let wb = &self.book(b)?.wb;
        let pt = |emu: i64| emu as f64 / EMU_PT;
        let total_w = pt(wb.col_left_emu(s, crate::addr::MAX_COLS));
        let total_h = pt(wb.row_top_emu(s, crate::addr::MAX_ROWS));
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
        _a: &Args,
    ) -> Result<Variant, OmError> {
        use InvokeKind::*;
        let ((row, col), note) = self
            .book(b)?
            .wb
            .note_by_id(s, id)
            .map(|(k, n)| (k, n.clone()))
            .ok_or_else(OmError::object_required)?;
        if m.kind != Get {
            return Err(crate::app::unsupported("Shape", m));
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
        let shape_id = 1024 + i32::try_from(note.id).unwrap_or(0);
        match m.name {
            "Name" => Ok(Variant::Bstr(format!("Comment {}", shape_id - 1024))),
            "Left" => Ok(Variant::R4(left as f32)),
            "Top" => Ok(Variant::R4(top as f32)),
            "Width" => Ok(Variant::R4(BOX_W as f32)),
            "Height" => Ok(Variant::R4(BOX_H as f32)),
            "Rotation" => Ok(Variant::R4(0.0)),
            "TopLeftCell" => cell_at(self, left, top),
            "BottomRightCell" => cell_at(self, left + BOX_W, top + BOX_H),
            "AutoShapeType" | "BlackWhiteMode" | "ZOrderPosition" => Ok(Variant::I4(1)),
            "ConnectionSiteCount"
            | "Connector"
            | "HorizontalFlip"
            | "VerticalFlip"
            | "LockAspectRatio"
            | "Child"
            | "HasChart" => Ok(Variant::I4(0)),
            "Type" => Ok(Variant::I4(4)),
            "Visible" => Ok(Variant::I4(-1)),
            "Placement" => Ok(Variant::I4(3)),
            "ID" => Ok(Variant::I4(shape_id)),
            "Locked" => Ok(Variant::Bool(true)),
            "OnAction" => Ok(Variant::Bstr(String::new())),
            "AlternativeText" => Ok(Variant::Bstr(format!("Text Box: {}", note.text))),
            "Parent" => self.new_range(b, s, Rect::cell(row, col), Mode::Cells),
            "Application" => Ok(Variant::Dispatch(Some(self.handle()))),
            "Adjustments" => stub(self, "Adjustments"),
            "TextFrame" => stub(self, "TextFrame"),
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
}
